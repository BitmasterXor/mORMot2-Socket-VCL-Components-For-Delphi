unit MorMotServerComponent;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, SyncObjs,
  mormot.core.base, mormot.core.log, mormot.core.rtti,
  mormot.net.sock, mormot.net.async,
  uSimpleProtocol;

type
  // Forward declarations
  TMorMotServer = class;
  TMorMotServerClient = class;

  // Event types - EXACTLY like NetCom7
  TMorMotServerHandleCommandEvent = function(Sender: TObject; aCmd: Integer; aData: TBytes; aRequiresResult: Boolean): TBytes of object;
  TMorMotServerConnectEvent = procedure(Sender: TObject; aLine: TMorMotServerClient) of object;
  TMorMotServerDisconnectEvent = procedure(Sender: TObject; aLine: TMorMotServerClient) of object;
  TMorMotServerReconnectEvent = procedure(Sender: TObject; aLine: TMorMotServerClient) of object;
  TMorMotServerLogEvent = procedure(Sender: TObject; const LogMsg: string) of object;

  // Client information class - NetCom7 calls this "Line"
  TMorMotServerClient = class
  private
    fConnection: TAsyncConnection;
    fRemoteIP: string;
    fHandle: TPollAsyncConnectionHandle;
    fConnectedAt: TDateTime;
    fServer: TMorMotServer;
    fPeerIP: string;
    fPeerPort: Integer;
    procedure SendProtocolMessage(const Message: RawByteString);
  public
    constructor Create(AConnection: TAsyncConnection; AServer: TMorMotServer);

    // NetCom7-style properties
    property Connection: TAsyncConnection read fConnection;
    property PeerIP: string read fPeerIP;
    property PeerPort: Integer read fPeerPort;
    property Handle: TPollAsyncConnectionHandle read fHandle;
    property ConnectedAt: TDateTime read fConnectedAt;

    // NetCom7-style methods - ONLY TBytes
    function ExecCommand(aCmd: Integer; aData: TBytes; aRequiresResult: Boolean = True): TBytes;
    procedure Disconnect;
  end;

  // Internal connection class
  TMorMotServerConnection = class(TAsyncConnection)
  private
    fOwnerComponent: TMorMotServer;
    fClientObject: TMorMotServerClient;
    fCleanupDone: Boolean;
  protected
    function OnRead: TPollAsyncSocketOnReadWrite; override;
    procedure AfterCreate; override;
    procedure OnClose; override;
  public
    constructor Create(aOwner: TAsyncConnections; const aRemoteIP: TNetAddr); override;
    procedure WriteMessage(const Message: RawByteString);
    function GetRemoteIPString: string;
    property ClientObject: TMorMotServerClient read fClientObject;
  end;

  // Main server component - NetCom7 style interface
  TMorMotServer = class(TComponent)
  private
    fServer: TAsyncServer;
    fLogFamily: TSynLogFamily;
    fClients: TObjectDictionary<TPollAsyncConnectionHandle, TMorMotServerClient>;
    fCriticalSection: TCriticalSection;
    fActive: Boolean;

    // NetCom7-style properties
    fPort: Integer;
    fReconnect: Boolean;
    fEnableLogging: Boolean;

    // NetCom7-style events
    fOnHandleCommand: TMorMotServerHandleCommandEvent;
    fOnConnect: TMorMotServerConnectEvent;
    fOnDisconnect: TMorMotServerDisconnectEvent;
    fOnReconnect: TMorMotServerReconnectEvent;
    fOnLog: TMorMotServerLogEvent;

    procedure DoLog(const Msg: string);
    procedure ProcessProtocolCommand(Client: TMorMotServerClient; const Data: RawByteString);
    function GetClientCount: Integer;
    function GetClients: TArray<TMorMotServerClient>;
    function GetPortStr: string;
    procedure SetPortStr(const Value: string);
    procedure SetActive(const Value: Boolean);

  protected
    procedure Loaded; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // NetCom7-style methods
    procedure Start;
    procedure Stop;

    // Client management - NetCom7 calls clients "Lines"
    function FindClient(Handle: TPollAsyncConnectionHandle): TMorMotServerClient;
    procedure DisconnectClient(Client: TMorMotServerClient);
    procedure DisconnectAll;

    // NetCom7-style ExecCommand to specific client
    function ExecCommand(aLine: TMorMotServerClient; aCmd: Integer; aData: TBytes; aRequiresResult: Boolean = True): TBytes;

    // Properties (read-only at runtime)
    property ClientCount: Integer read GetClientCount;
    property Clients: TArray<TMorMotServerClient> read GetClients;

  published
    // NetCom7-style properties
    property Active: Boolean read fActive write SetActive;
    property Port: Integer read fPort write fPort default 3434;
    property PortStr: string read GetPortStr write SetPortStr stored False; // For compatibility
    property Reconnect: Boolean read fReconnect write fReconnect default True;
    property EnableLogging: Boolean read fEnableLogging write fEnableLogging default True;

    // NetCom7-style events
    property OnHandleCommand: TMorMotServerHandleCommandEvent read fOnHandleCommand write fOnHandleCommand;
    property OnConnect: TMorMotServerConnectEvent read fOnConnect write fOnConnect;
    property OnDisconnect: TMorMotServerDisconnectEvent read fOnDisconnect write fOnDisconnect;
    property OnReconnect: TMorMotServerReconnectEvent read fOnReconnect write fOnReconnect;
    property OnLog: TMorMotServerLogEvent read fOnLog write fOnLog;
  end;

// Global server reference for factory method
var
  GlobalServerComponent: TMorMotServer = nil;

implementation

{ TMorMotServerClient }

constructor TMorMotServerClient.Create(AConnection: TAsyncConnection; AServer: TMorMotServer);
begin
  inherited Create;
  fConnection := AConnection;
  fServer := AServer;
  fHandle := AConnection.Handle;
  fConnectedAt := Now;
  fPeerIP := '127.0.0.1'; // Default for localhost
  fPeerPort := 0;
end;

function TMorMotServerClient.ExecCommand(aCmd: Integer; aData: TBytes; aRequiresResult: Boolean): TBytes;
var
  CommandData: RawByteString;
begin
  // NetCom7-style command execution from server to client
  SetLength(Result, 0);

  if Assigned(fConnection) and Assigned(fServer) then
  begin
    try
      // Pack command data exactly like NetCom7
      CommandData := TSimpleProtocol.PackCommand(aCmd, aData, aRequiresResult);
      SendProtocolMessage(CommandData);

      if fServer.EnableLogging then
        fServer.DoLog('Sent command ' + IntToStr(aCmd) + ' to client ' + fPeerIP + ': ' + IntToStr(Length(aData)) + ' bytes');

      if aRequiresResult then
      begin
        // Wait for response (simplified - in real NetCom7 this is more complex)
        // For now, just return empty result
        SetLength(Result, 0);
      end;
    except
      on E: Exception do
      begin
        if Assigned(fServer) and fServer.EnableLogging then
          fServer.DoLog('Failed to send command to client: ' + E.Message);
        raise;
      end;
    end;
  end
  else
  begin
    raise Exception.Create('Client connection not available for ExecCommand');
  end;
end;

procedure TMorMotServerClient.SendProtocolMessage(const Message: RawByteString);
begin
  if Assigned(fConnection) then
  begin
    TMorMotServerConnection(fConnection).WriteMessage(Message);
  end;
end;

procedure TMorMotServerClient.Disconnect;
begin
  if Assigned(fConnection) and Assigned(fServer) then
  begin
    fServer.DisconnectClient(Self);
  end;
end;

{ TMorMotServerConnection }

constructor TMorMotServerConnection.Create(aOwner: TAsyncConnections; const aRemoteIP: TNetAddr);
begin
  inherited Create(aOwner, aRemoteIP);
  fOwnerComponent := GlobalServerComponent;
  fCleanupDone := False;
end;

procedure TMorMotServerConnection.WriteMessage(const Message: RawByteString);
begin
  fOwner.WriteString(Self, Message);
end;

function TMorMotServerConnection.GetRemoteIPString: string;
begin
  if fRemoteIP = '' then
    Result := '127.0.0.1'
  else
    Result := string(fRemoteIP);
end;

procedure TMorMotServerConnection.AfterCreate;
begin
  inherited AfterCreate;

  if Assigned(fOwnerComponent) then
  begin
    fOwnerComponent.fCriticalSection.Enter;
    try
      fClientObject := TMorMotServerClient.Create(Self, fOwnerComponent);
      fClientObject.fPeerIP := GetRemoteIPString;
      fOwnerComponent.fClients.AddOrSetValue(Handle, fClientObject);

      // Fire NetCom7-style OnConnect event
      if Assigned(fOwnerComponent.fOnConnect) then
        fOwnerComponent.fOnConnect(fOwnerComponent, fClientObject);

    finally
      fOwnerComponent.fCriticalSection.Leave;
    end;
  end;
end;

procedure TMorMotServerConnection.OnClose;
var
  ClientToRemove: TMorMotServerClient;
begin
  if fCleanupDone then
  begin
    inherited OnClose;
    Exit;
  end;

  fCleanupDone := True;
  ClientToRemove := nil;

  if Assigned(fOwnerComponent) and Assigned(fClientObject) then
  begin
    fOwnerComponent.fCriticalSection.Enter;
    try
      if fOwnerComponent.fClients.ContainsKey(Handle) then
      begin
        ClientToRemove := fOwnerComponent.fClients[Handle];
        fOwnerComponent.fClients.Remove(Handle);
      end;
    finally
      fOwnerComponent.fCriticalSection.Leave;
    end;

    // Fire NetCom7-style OnDisconnect event
    if Assigned(ClientToRemove) and Assigned(fOwnerComponent.fOnDisconnect) then
    begin
      try
        fOwnerComponent.fOnDisconnect(fOwnerComponent, ClientToRemove);
      except
        // Ignore exceptions in event handlers
      end;
    end;
  end;

  fClientObject := nil;
  inherited OnClose;
end;

function TMorMotServerConnection.OnRead: TPollAsyncSocketOnReadWrite;
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

          if Assigned(fOwnerComponent) and Assigned(fClientObject) then
            fOwnerComponent.ProcessProtocolCommand(fClientObject, data);
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

{ TMorMotServer }

constructor TMorMotServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fClients := TObjectDictionary<TPollAsyncConnectionHandle, TMorMotServerClient>.Create([doOwnsValues]);
  fCriticalSection := TCriticalSection.Create;
  fActive := False;

  // NetCom7-style defaults
  fPort := 3434;
  fReconnect := True;
  fEnableLogging := True;

  // Configure logging
  fLogFamily := TSynLog.Family;
  fLogFamily.Level := LOG_VERBOSE;
  fLogFamily.PerThreadLog := ptIdentifiedInOnFile;
end;

destructor TMorMotServer.Destroy;
begin
  Stop;
  fClients.Free;
  fCriticalSection.Free;
  inherited Destroy;
end;

procedure TMorMotServer.Loaded;
begin
  inherited Loaded;
end;

procedure TMorMotServer.DoLog(const Msg: string);
begin
  if fEnableLogging and Assigned(fOnLog) then
    fOnLog(Self, Msg);
end;

procedure TMorMotServer.ProcessProtocolCommand(Client: TMorMotServerClient; const Data: RawByteString);
var
  dataBytes: TBytes;
  cmd: Integer;
  cmdData: TBytes;
  requiresResult: Boolean;
  resultData: TBytes;
begin
  // Try to unpack NetCom7-style command
  if TSimpleProtocol.TryUnpackCommand(Data, cmd, cmdData, requiresResult) then
  begin
    // Call NetCom7-style OnHandleCommand event
    if Assigned(fOnHandleCommand) then
    begin
      try
        resultData := fOnHandleCommand(Self, cmd, cmdData, requiresResult);

        if requiresResult then
        begin
          // Send result back to client
          Client.SendProtocolMessage(TSimpleProtocol.PackResult(resultData));
        end;
      except
        on E: Exception do
        begin
          if requiresResult then
          begin
            // Send exception back to client (NetCom7 feature)
            Client.SendProtocolMessage(TSimpleProtocol.PackException(E.Message));
          end;
        end;
      end;
    end
    else if requiresResult then
    begin
      // No handler, send empty result
      Client.SendProtocolMessage(TSimpleProtocol.PackResult([]));
    end;
  end
  else
  begin
    // Fallback to raw data handling
    dataBytes := TSimpleProtocol.RawByteStringToBytes(Data);
    if Assigned(fOnHandleCommand) then
    begin
      try
        fOnHandleCommand(Self, 0, dataBytes, False);
      except
        // Ignore exceptions for raw data
      end;
    end;
  end;
end;

function TMorMotServer.GetClientCount: Integer;
begin
  fCriticalSection.Enter;
  try
    Result := fClients.Count;
  finally
    fCriticalSection.Leave;
  end;
end;

function TMorMotServer.GetClients: TArray<TMorMotServerClient>;
var
  ClientList: TArray<TMorMotServerClient>;
  Client: TMorMotServerClient;
  i: Integer;
begin
  fCriticalSection.Enter;
  try
    SetLength(ClientList, fClients.Count);
    i := 0;
    for Client in fClients.Values do
    begin
      ClientList[i] := Client;
      Inc(i);
    end;
    Result := ClientList;
  finally
    fCriticalSection.Leave;
  end;
end;

function TMorMotServer.GetPortStr: string;
begin
  Result := IntToStr(fPort);
end;

procedure TMorMotServer.SetPortStr(const Value: string);
begin
  fPort := StrToIntDef(Value, 3434);
end;

procedure TMorMotServer.SetActive(const Value: Boolean);
begin
  if fActive = Value then Exit;

  if Value then
    Start
  else
    Stop;
end;

procedure TMorMotServer.Start;
begin
  if fActive then
    Exit;

  try
    GlobalServerComponent := Self;

    fServer := TAsyncServer.Create(
      IntToStr(fPort),
      nil, nil,
      TMorMotServerConnection,
      'MorMotServer',
      fLogFamily.SynLogClass,
      [],
      4 // Default thread pool size
    );

    fServer.WaitStarted(10);
    fActive := True;

  except
    on E: Exception do
    begin
      GlobalServerComponent := nil;
      raise;
    end;
  end;
end;

procedure TMorMotServer.Stop;
begin
  if not fActive then
    Exit;

  fActive := False;

  // Disconnect all clients first
  try
    DisconnectAll;
  except
    // Ignore errors during client disconnection
  end;

  // Stop the server
  if Assigned(fServer) then
  begin
    try
      fServer.Free;
    except
      // Ignore errors during server shutdown
    end;
    fServer := nil;
  end;

  if GlobalServerComponent = Self then
    GlobalServerComponent := nil;
end;

function TMorMotServer.FindClient(Handle: TPollAsyncConnectionHandle): TMorMotServerClient;
begin
  fCriticalSection.Enter;
  try
    if not fClients.TryGetValue(Handle, Result) then
      Result := nil;
  finally
    fCriticalSection.Leave;
  end;
end;

procedure TMorMotServer.DisconnectClient(Client: TMorMotServerClient);
begin
  if Assigned(Client) and Assigned(Client.Connection) then
  begin
    // Connection will be closed by the async framework
  end;
end;

procedure TMorMotServer.DisconnectAll;
var
  ClientsArray: TArray<TMorMotServerClient>;
  Client: TMorMotServerClient;
begin
  ClientsArray := GetClients;
  for Client in ClientsArray do
    DisconnectClient(Client);
end;

// NetCom7-style ExecCommand to specific client
function TMorMotServer.ExecCommand(aLine: TMorMotServerClient; aCmd: Integer; aData: TBytes; aRequiresResult: Boolean): TBytes;
var
  CommandData: RawByteString;
begin
  SetLength(Result, 0);

  if not Assigned(aLine) then
  begin
    raise Exception.Create('Client line not assigned');
  end;

  if not Assigned(aLine.Connection) then
  begin
    raise Exception.Create('Client connection not available');
  end;

  try
    // Pack command data exactly like NetCom7
    CommandData := TSimpleProtocol.PackCommand(aCmd, aData, aRequiresResult);
    aLine.SendProtocolMessage(CommandData);

    if aRequiresResult then
    begin
      // Wait for response (simplified - in real NetCom7 this is more complex)
      // For now, just return empty result
      SetLength(Result, 0);
    end;
  except
    on E: Exception do
    begin
      raise Exception.Create('Failed to execute command on client: ' + E.Message);
    end;
  end;
end;

end.
