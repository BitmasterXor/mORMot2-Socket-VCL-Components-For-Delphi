unit MorMotClientComponent;

interface

uses
  System.Classes, System.SysUtils, Vcl.Forms,
  mormot.core.base, mormot.core.log, mormot.core.rtti,
  mormot.net.sock, mormot.net.async,
  uSimpleProtocol, winapi.Windows;

type
  // Forward declarations
  TMorMotClient = class;

  // Event types - EXACTLY like NetCom7
  TMorMotClientHandleCommandEvent = function(Sender: TObject; aCmd: Integer; aData: TBytes; aRequiresResult: Boolean): TBytes of object;
  TMorMotClientConnectEvent = procedure(Sender: TObject) of object;
  TMorMotClientDisconnectEvent = procedure(Sender: TObject) of object;
  TMorMotClientReconnectEvent = procedure(Sender: TObject) of object;

  // Internal connection class
  TMorMotClientConnection = class(TAsyncConnection)
  private
    fOwnerComponent: TMorMotClient;
    fCleanupDone: Boolean;
    fPendingResponse: TBytes;
    fWaitingForResponse: Boolean;
    procedure ProcessProtocolCommand(const Data: RawByteString);
    procedure SendProtocolMessage(const Message: RawByteString);
  protected
    function OnRead: TPollAsyncSocketOnReadWrite; override;
    procedure AfterCreate; override;
    procedure OnClose; override;
  public
    constructor Create(aOwner: TAsyncConnections; const aRemoteIP: TNetAddr); override;
    function WaitForResponse(TimeoutMs: Integer = 5000): TBytes;
  end;

  // Main client component - NetCom7 style interface
  TMorMotClient = class(TComponent)
  private
    fClient: TAsyncClient;
    fConnection: TMorMotClientConnection;
    fLogFamily: TSynLogFamily;
    fActive: Boolean;

    // NetCom7-style connection properties
    fHost: string;
    fPort: Integer;
    fReconnect: Boolean;

    // NetCom7-style events
    fOnHandleCommand: TMorMotClientHandleCommandEvent;
    fOnConnect: TMorMotClientConnectEvent;
    fOnDisconnect: TMorMotClientDisconnectEvent;
    fOnReconnect: TMorMotClientReconnectEvent;

    function GetPortStr: string;
    procedure SetPortStr(const Value: string);
    procedure SetActive(const Value: Boolean);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // NetCom7-style connection methods
    procedure Connect;
    procedure Disconnect;

    // NetCom7-style ExecCommand - THE MAIN METHOD
    function ExecCommand(aCmd: Integer; aData: TBytes; aRequiresResult: Boolean = True): TBytes;

    // Properties (read-only at runtime except Active)
    property Connection: TMorMotClientConnection read fConnection;

  published
    // NetCom7-style properties
    property Active: Boolean read fActive write SetActive;
    property Host: string read fHost write fHost;
    property Port: Integer read fPort write fPort default 3434;
    property PortStr: string read GetPortStr write SetPortStr stored False; // For compatibility
    property Reconnect: Boolean read fReconnect write fReconnect default True;

    // NetCom7-style events
    property OnHandleCommand: TMorMotClientHandleCommandEvent read fOnHandleCommand write fOnHandleCommand;
    property OnConnect: TMorMotClientConnectEvent read fOnConnect write fOnConnect;
    property OnDisconnect: TMorMotClientDisconnectEvent read fOnDisconnect write fOnDisconnect;
    property OnReconnect: TMorMotClientReconnectEvent read fOnReconnect write fOnReconnect;
  end;

// Global component reference
var
  GlobalClientComponent: TMorMotClient = nil;

implementation

{ TMorMotClientConnection }

constructor TMorMotClientConnection.Create(aOwner: TAsyncConnections; const aRemoteIP: TNetAddr);
begin
  inherited Create(aOwner, aRemoteIP);
  fOwnerComponent := GlobalClientComponent;
  fCleanupDone := False;
  fWaitingForResponse := False;
  SetLength(fPendingResponse, 0);
end;

procedure TMorMotClientConnection.AfterCreate;
begin
  inherited AfterCreate;

  if Assigned(fOwnerComponent) then
  begin
    fOwnerComponent.fConnection := Self;
    fOwnerComponent.fActive := True;

    // Fire NetCom7-style OnConnect event
    if Assigned(fOwnerComponent.fOnConnect) then
      fOwnerComponent.fOnConnect(fOwnerComponent);
  end;
end;

procedure TMorMotClientConnection.OnClose;
var
  TempOwner: TMorMotClient;
begin
  if fCleanupDone then
  begin
    inherited OnClose;
    Exit;
  end;

  fCleanupDone := True;
  TempOwner := fOwnerComponent;

  if Assigned(TempOwner) then
  begin
    TempOwner.fConnection := nil;
    TempOwner.fActive := False;

    // Fire NetCom7-style OnDisconnect event
    if Assigned(TempOwner.fOnDisconnect) then
    begin
      try
        TempOwner.fOnDisconnect(TempOwner);
      except
        // Ignore exceptions in event handlers
      end;
    end;

    // Auto-reconnect like NetCom7
    if TempOwner.fReconnect and not (csDestroying in TempOwner.ComponentState) then
    begin
      if Assigned(TempOwner.fOnReconnect) then
      begin
        try
          TempOwner.fOnReconnect(TempOwner);
        except
          // Ignore exceptions in event handlers
        end;
      end;

      // Attempt reconnection (simplified)
      try
        TempOwner.Connect;
      except
        // Ignore reconnection failures
      end;
    end;
  end;

  fOwnerComponent := nil;
  inherited OnClose;
end;

function TMorMotClientConnection.OnRead: TPollAsyncSocketOnReadWrite;
var
  bufferData: RawByteString;
  data: RawByteString;
  messageSize: UInt32;
begin
  Result := soContinue;

  try
    while fRd.Len > 0 do
    begin
      SetString(bufferData, PAnsiChar(fRd.Buffer), fRd.Len);

      if TSimpleProtocol.HasMagicMarker(bufferData) then
      begin
        if TSimpleProtocol.TryParseMessage(bufferData, data) then
        begin
          messageSize := SizeOf(TSimpleMessage) + Length(data);
          fRd.Remove(messageSize);

          ProcessProtocolCommand(data);
        end
        else
          break;
      end
      else
      begin
        Result := soClose;
        break;
      end;
    end;

  except
    on E: Exception do
    begin
      Result := soClose;
    end;
  end;
end;

procedure TMorMotClientConnection.ProcessProtocolCommand(const Data: RawByteString);
var
  dataBytes: TBytes;
  cmd: Integer;
  cmdData: TBytes;
  requiresResult: Boolean;
  resultData: TBytes;
begin
  if Assigned(fOwnerComponent) then
  begin
    // Check if this is a response to our request
    if TSimpleProtocol.TryUnpackResult(Data, resultData) then
    begin
      fPendingResponse := resultData;
      fWaitingForResponse := False;
      Exit;
    end;

    // Try to unpack NetCom7-style command
    if TSimpleProtocol.TryUnpackCommand(Data, cmd, cmdData, requiresResult) then
    begin
      // Call NetCom7-style OnHandleCommand event
      if Assigned(fOwnerComponent.fOnHandleCommand) then
      begin
        try
          resultData := fOwnerComponent.fOnHandleCommand(fOwnerComponent, cmd, cmdData, requiresResult);

          if requiresResult then
          begin
            // Send result back to server
            SendProtocolMessage(TSimpleProtocol.PackResult(resultData));
          end;
        except
          on E: Exception do
          begin
            if requiresResult then
            begin
              // Send exception back to server (NetCom7 feature)
              SendProtocolMessage(TSimpleProtocol.PackException(E.Message));
            end;
          end;
        end;
      end;
    end
    else
    begin
      // Fallback to raw data handling
      dataBytes := TSimpleProtocol.RawByteStringToBytes(Data);
      if Assigned(fOwnerComponent.fOnHandleCommand) then
      begin
        try
          fOwnerComponent.fOnHandleCommand(fOwnerComponent, 0, dataBytes, False);
        except
          // Ignore exceptions for raw data
        end;
      end;
    end;
  end;
end;

procedure TMorMotClientConnection.SendProtocolMessage(const Message: RawByteString);
begin
  fOwner.WriteString(Self, Message);
end;

function TMorMotClientConnection.WaitForResponse(TimeoutMs: Integer): TBytes;
var
  StartTime: Cardinal;
begin
  SetLength(Result, 0);
  fWaitingForResponse := True;
  SetLength(fPendingResponse, 0);

  StartTime := GetTickCount;

  while fWaitingForResponse and ((GetTickCount - StartTime) < Cardinal(TimeoutMs)) do
  begin
    Sleep(10);
    Application.ProcessMessages;
  end;

  if not fWaitingForResponse then
    Result := fPendingResponse
  else
    fWaitingForResponse := False; // Timeout
end;

{ TMorMotClient }

constructor TMorMotClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fActive := False;
  fHost := 'localhost';
  fPort := 3434;
  fReconnect := True;

  // Configure logging
  fLogFamily := TSynLog.Family;
  fLogFamily.Level := LOG_VERBOSE;
  fLogFamily.PerThreadLog := ptIdentifiedInOnFile;
end;

destructor TMorMotClient.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

function TMorMotClient.GetPortStr: string;
begin
  Result := IntToStr(fPort);
end;

procedure TMorMotClient.SetPortStr(const Value: string);
begin
  fPort := StrToIntDef(Value, 3434);
end;

procedure TMorMotClient.SetActive(const Value: Boolean);
begin
  if fActive = Value then Exit;

  if Value then
    Connect
  else
    Disconnect;
end;

procedure TMorMotClient.Connect;
begin
  if fActive then
    Exit;

  if Assigned(fClient) then
  begin
    fClient.Free;
    fClient := nil;
  end;

  try
    GlobalClientComponent := Self;

    fClient := TAsyncClient.Create(
      fHost,
      IntToStr(fPort),
      1,
      10, // Connection timeout
      nil, nil,
      TMorMotClientConnection,
      'MorMotClient',
      fLogFamily.SynLogClass,
      [],
      1 // Thread pool size
    );

    // Wait a bit for connection to establish
    Sleep(100);

  except
    on E: Exception do
    begin
      GlobalClientComponent := nil;
      fActive := False;
      raise;
    end;
  end;
end;

procedure TMorMotClient.Disconnect;
begin
  fActive := False;

  // Clean up the client - this will trigger OnClose
  if Assigned(fClient) then
  begin
    try
      fClient.Free;
    except
      // Ignore errors during disconnection
    end;
    fClient := nil;
  end;

  // Ensure state is clean
  fConnection := nil;

  if GlobalClientComponent = Self then
    GlobalClientComponent := nil;
end;

// NetCom7-style ExecCommand - THE MAIN METHOD
function TMorMotClient.ExecCommand(aCmd: Integer; aData: TBytes; aRequiresResult: Boolean): TBytes;
var
  CommandData: RawByteString;
  RetryCount: Integer;
begin
  SetLength(Result, 0);

  // Auto-connect like NetCom7 if not active
  if not fActive then
  begin
    for RetryCount := 1 to 3 do
    begin
      try
        Connect;
        if fActive then Break;
        Sleep(500);
      except
        if RetryCount = 3 then
          raise;
        Sleep(1000);
      end;
    end;
  end;

  if fActive and Assigned(fConnection) then
  begin
    // Pack command data exactly like NetCom7
    CommandData := TSimpleProtocol.PackCommand(aCmd, aData, aRequiresResult);
    fConnection.SendProtocolMessage(CommandData);

    if aRequiresResult then
    begin
      // Wait for response with timeout
      Result := fConnection.WaitForResponse(10000); // 10 second timeout
    end;
  end
  else
  begin
    raise Exception.Create('Client not connected and auto-connect failed');
  end;
end;

end.
