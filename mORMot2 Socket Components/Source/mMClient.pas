unit mMClient;

interface

uses
  System.Classes, System.SysUtils, System.SyncObjs, System.DateUtils,
  {$IFDEF MSWINDOWS}
  Vcl.ExtCtrls, Math,  // For VCL TTimer and Math requirements!
  {$ENDIF}
  {$IFDEF ANDROID OR DEFINED(IOS)}
  FMX.Types,     // For FireMonkey TTimer
  {$ENDIF}
  mormot.core.base, mormot.core.log, mormot.core.rtti,
  mormot.net.sock, mormot.net.async,
  mormot.crypt.secure,  // for Pbkdf2HmacSha256
  mMProtocol;

type
  // Forward declarations
  TmMClient = class;

  // Logging level enumeration
  TmMClientLogLevel = (
    llNone,         // No logging
    llErrors,       // Only errors, exceptions, and critical failures
    llDebug,        // Debug information including enter/leave
    llClientServer, // Client/server communication logs
    llVerbose       // All logging
  );

  // Connection state enumeration
  TmMClientState = (
    csDisconnected,
    csConnecting,
    csConnected,
    csDisconnecting
  );

  // Reconnection strategy
  TmMReconnectStrategy = (
    rsNone,           // No automatic reconnection
    rsImmediate,      // Reconnect immediately
    rsBackoff,        // Exponential backoff
    rsFixedInterval   // Fixed interval reconnection
  );

  // Event types
  TmMClientHandleCommandEvent = function(Sender: TObject; aData: TBytes): TBytes of object;
  TmMClientConnectEvent = procedure(Sender: TObject) of object;
  TmMClientDisconnectEvent = procedure(Sender: TObject) of object;
  TmMClientErrorEvent = procedure(Sender: TObject; const Error: string; ErrorCode: Integer) of object;
  TmMClientStateChangeEvent = procedure(Sender: TObject; OldState, NewState: TmMClientState) of object;
  TmMClientDataSentEvent = procedure(Sender: TObject; BytesSent: Integer) of object;
  TmMClientDataReceivedEvent = procedure(Sender: TObject; BytesReceived: Integer) of object;

  // Custom timer class for thread-safe operation
  TmMReconnectTimer = class
  private
    FTimer: TTimer;
    FOwner: TmMClient;
    FEnabled: Boolean;
    FInterval: Integer;
    procedure OnTimerEvent(Sender: TObject);
  public
    constructor Create(AOwner: TmMClient);
    destructor Destroy; override;
    procedure Start(AInterval: Integer);
    procedure Stop;
    property Enabled: Boolean read FEnabled;
    property Interval: Integer read FInterval;
  end;

  // Internal connection class
  TmMClientConnection = class(TAsyncConnection)
  private
    FOwnerComponent: TmMClient;
    FBytesReceived: Int64;
    FBytesSent: Int64;
    FRemoteIP: string;  // Store the remote server IP
    procedure ProcessCommand(const Data: TBytes);
    procedure SendProtocolMessage(const Message: RawByteString);
  protected
    function OnRead: TPollAsyncSocketOnReadWrite; override;
    procedure AfterCreate; override;
    procedure OnClose; override;
  public
    constructor Create(aOwner: TAsyncConnections; const aRemoteIP: TNetAddr); override;
    property BytesReceived: Int64 read FBytesReceived;
    property BytesSent: Int64 read FBytesSent;
    property RemoteIP: string read FRemoteIP;  // Expose remote server IP
  end;

  // Main client component
  TmMClient = class(TComponent)
  private
    FInitActive: Boolean;
    FClient: TAsyncClient;
    FConnection: TmMClientConnection;
    FLogFamily: TSynLogFamily;
    FLogLevel: TmMClientLogLevel;
    FActive: Boolean;
    FHost: string;
    FPort: Integer;
    FConnectionTimeout: Integer;
    FThreadPoolSize: Integer;
    FState: TmMClientState;
    FLastConnectedServerIP: string;  // FIXED: Store server IP for disconnect event

    // Encryption support - Only key and mode (no enabled flag)
    FEncryptionKey: string;
    FEncryptionMode: TAESMode;
    FEncryptionKeySize: TAESKeySize;
    FEncryptionContext: TEncryptionContext;

    // Reconnection properties
    FAutoReconnect: Boolean;
    FReconnectStrategy: TmMReconnectStrategy;
    FReconnectInterval: Integer;
    FReconnectMaxAttempts: Integer;
    FReconnectAttempts: Integer;
    FReconnectBackoffMultiplier: Double;
    FReconnectTimer: TmMReconnectTimer;

    // Buffer and protocol settings
    FReceiveBufferSize: Integer;
    FSendBufferSize: Integer;
    FKeepAlive: Boolean;
    FNoDelay: Boolean;

    // Statistics
    FConnectTime: TDateTime;
    FLastActivityTime: TDateTime;
    FTotalBytesReceived: Int64;
    FTotalBytesSent: Int64;
    FMessagesReceived: Int64;
    FMessagesSent: Int64;

    // Component info
    FVersion: string;
    FDescription: string;
    FEventsUseMainThread: Boolean;

    // Events
    FOnHandleCommand: TmMClientHandleCommandEvent;
    FOnConnect: TmMClientConnectEvent;
    FOnDisconnect: TmMClientDisconnectEvent;
    FOnError: TmMClientErrorEvent;
    FOnStateChange: TmMClientStateChangeEvent;
    FOnDataSent: TmMClientDataSentEvent;
    FOnDataReceived: TmMClientDataReceivedEvent;

    // Property methods
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    function GetHost: string;
    procedure SetHost(const Value: string);
    function GetPort: Integer;
    procedure SetPort(const Value: Integer);
    function GetConnectionTimeout: Integer;
    procedure SetConnectionTimeout(const Value: Integer);
    function GetThreadPoolSize: Integer;
    procedure SetThreadPoolSize(const Value: Integer);
    function GetEventsUseMainThread: Boolean;
    procedure SetEventsUseMainThread(const Value: Boolean);
    function GetLogLevel: TmMClientLogLevel;
    procedure SetLogLevel(const Value: TmMClientLogLevel);
    function GetState: TmMClientState;
    procedure SetState(const Value: TmMClientState);

    // FIXED: Remote IP property method
    function GetRemoteIP: string;

    // Encryption property methods - Only key and mode (no enabled flag)
    function GetEncryptionKey: string;
    procedure SetEncryptionKey(const Value: string);
    function GetEncryptionMode: TAESMode;
    procedure SetEncryptionMode(const Value: TAESMode);
    function GetEncryptionKeySize: TAESKeySize;
    procedure SetEncryptionKeySize(const Value: TAESKeySize);
    function GetEncryptionInfo: string;

    // Reconnection property methods
    function GetAutoReconnect: Boolean;
    procedure SetAutoReconnect(const Value: Boolean);
    function GetReconnectStrategy: TmMReconnectStrategy;
    procedure SetReconnectStrategy(const Value: TmMReconnectStrategy);
    function GetReconnectInterval: Integer;
    procedure SetReconnectInterval(const Value: Integer);
    function GetReconnectMaxAttempts: Integer;
    procedure SetReconnectMaxAttempts(const Value: Integer);

    // Buffer property methods
    function GetReceiveBufferSize: Integer;
    procedure SetReceiveBufferSize(const Value: Integer);
    function GetSendBufferSize: Integer;
    procedure SetSendBufferSize(const Value: Integer);
    function GetKeepAlive: Boolean;
    procedure SetKeepAlive(const Value: Boolean);
    function GetNoDelay: Boolean;
    procedure SetNoDelay(const Value: Boolean);

    // Statistics property methods
    function GetConnectedTime: Integer;
    function GetLastActivityTime: TDateTime;
    function GetTotalBytesReceived: Int64;
    function GetTotalBytesSent: Int64;
    function GetMessagesReceived: Int64;
    function GetMessagesSent: Int64;

    // Internal methods
    procedure DoStateChange(NewState: TmMClientState);
    procedure DoError(const ErrorMsg: string; ErrorCode: Integer = 0);
    procedure DoDataSent(BytesSent: Integer);
    procedure DoDataReceived(BytesReceived: Integer);
    procedure UpdateEncryptionContext;
    procedure OnReconnectTimer;
    procedure StartReconnectTimer;
    procedure StopReconnectTimer;
    procedure ResetStatistics;
    procedure DoActivate(aActivate: Boolean);
    procedure InternalCleanup; // NEW: Centralized cleanup

  protected
    PropertyLock: TCriticalSection;
    procedure Loaded; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Connection methods
    procedure Connect;
    procedure Disconnect;
    procedure Reconnect;

    // ONLY ONE DATA TRANSMISSION METHOD
    procedure SendData(const Data: TBytes; Encrypted: Boolean = False);

    // Utility methods
    procedure ClearStatistics;
    function IsConnected: Boolean;
    function GetConnectionInfo: string;
    procedure ResetReconnectionState; // NEW: Manual reset

    // Properties (read-only at runtime except Active)
    property Connection: TmMClientConnection read FConnection;
    property State: TmMClientState read GetState;
    property ConnectedTime: Integer read GetConnectedTime;
    property LastActivityTime: TDateTime read GetLastActivityTime;
    property ReconnectAttempts: Integer read FReconnectAttempts;
    property EncryptionInfo: string read GetEncryptionInfo;
    property RemoteIP: string read GetRemoteIP;  // FIXED: Access remote server IP

  published
    // Properties with proper getters/setters
    property Active: Boolean read GetActive write SetActive default False;
    property Host: string read GetHost write SetHost;
    property Port: Integer read GetPort write SetPort default 3434;
    property ConnectionTimeout: Integer read GetConnectionTimeout write SetConnectionTimeout default 10;
    property ThreadPoolSize: Integer read GetThreadPoolSize write SetThreadPoolSize default 1;
    property EventsUseMainThread: Boolean read GetEventsUseMainThread write SetEventsUseMainThread default True;

    // Encryption properties - Only key and mode (no enabled flag)
    property EncryptionKey: string read GetEncryptionKey write SetEncryptionKey;
    property EncryptionMode: TAESMode read GetEncryptionMode write SetEncryptionMode default amCBC;
    property EncryptionKeySize: TAESKeySize read GetEncryptionKeySize write SetEncryptionKeySize default aks256;

    // Reconnection properties
    property AutoReconnect: Boolean read GetAutoReconnect write SetAutoReconnect default False;
    property ReconnectStrategy: TmMReconnectStrategy read GetReconnectStrategy write SetReconnectStrategy default rsBackoff;
    property ReconnectInterval: Integer read GetReconnectInterval write SetReconnectInterval default 5000;
    property ReconnectMaxAttempts: Integer read GetReconnectMaxAttempts write SetReconnectMaxAttempts default 10;

    // Buffer and socket properties
    property ReceiveBufferSize: Integer read GetReceiveBufferSize write SetReceiveBufferSize default 8192;
    property SendBufferSize: Integer read GetSendBufferSize write SetSendBufferSize default 8192;
    property KeepAlive: Boolean read GetKeepAlive write SetKeepAlive default True;
    property NoDelay: Boolean read GetNoDelay write SetNoDelay default True;

    // Statistics (read-only)
    property TotalBytesReceived: Int64 read GetTotalBytesReceived;
    property TotalBytesSent: Int64 read GetTotalBytesSent;
    property MessagesReceived: Int64 read GetMessagesReceived;
    property MessagesSent: Int64 read GetMessagesSent;

    // Logging control
    property LogLevel: TmMClientLogLevel read GetLogLevel write SetLogLevel default llErrors;

    // Component info
    property Version: string read FVersion;
    property Description: string read FDescription;

    // Events
    property OnHandleCommand: TmMClientHandleCommandEvent read FOnHandleCommand write FOnHandleCommand;
    property OnConnect: TmMClientConnectEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TmMClientDisconnectEvent read FOnDisconnect write FOnDisconnect;
    property OnError: TmMClientErrorEvent read FOnError write FOnError;
    property OnStateChange: TmMClientStateChangeEvent read FOnStateChange write FOnStateChange;
    property OnDataSent: TmMClientDataSentEvent read FOnDataSent write FOnDataSent;
    property OnDataReceived: TmMClientDataReceivedEvent read FOnDataReceived write FOnDataReceived;
  end;

// Global component reference for connection callback
var
  GlobalClientComponent: TmMClient = nil;

implementation

{ TmMReconnectTimer }

constructor TmMReconnectTimer.Create(AOwner: TmMClient);
begin
  inherited Create;
  FOwner := AOwner;
  FEnabled := False;
  FInterval := 0;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.OnTimer := OnTimerEvent;
end;

destructor TmMReconnectTimer.Destroy;
begin
  if Assigned(FTimer) then
  begin
    FTimer.Enabled := False;
    FTimer.Free;
  end;
  inherited Destroy;
end;

procedure TmMReconnectTimer.OnTimerEvent(Sender: TObject);
begin
  Stop;
  if Assigned(FOwner) then
    FOwner.OnReconnectTimer;
end;

procedure TmMReconnectTimer.Start(AInterval: Integer);
begin
  if not Assigned(FTimer) then
    Exit;
  FInterval := AInterval;

  // CRITICAL FIX: Minimum interval for immediate reconnection
  if FInterval < 100 then
    FInterval := 100;

  FTimer.Interval := FInterval;
  FTimer.Enabled := True;
  FEnabled := True;
end;

procedure TmMReconnectTimer.Stop;
begin
  if Assigned(FTimer) then
  begin
    FTimer.Enabled := False;
    FEnabled := False;
  end;
end;

{ TmMClientConnection }

constructor TmMClientConnection.Create(aOwner: TAsyncConnections; const aRemoteIP: TNetAddr);
var
  ServerIP: RawUtf8;
begin
  inherited Create(aOwner, aRemoteIP);
  FOwnerComponent := GlobalClientComponent;
  FBytesReceived := 0;
  FBytesSent := 0;

  // FIXED: Extract server IP correctly from TNetAddr
  aRemoteIP.IP(ServerIP, false);
  FRemoteIP := string(ServerIP);

  // Fallback to component host if extraction fails
  if (FRemoteIP = '') and Assigned(FOwnerComponent) then
    FRemoteIP := FOwnerComponent.FHost;
end;

procedure TmMClientConnection.AfterCreate;
begin
  inherited AfterCreate;
  if Assigned(FOwnerComponent) then
  begin
    // FIXED: Store the server IP for use in disconnect event
    FOwnerComponent.FLastConnectedServerIP := FRemoteIP;

    FOwnerComponent.FConnection := Self;
    FOwnerComponent.DoStateChange(csConnected);
    FOwnerComponent.FConnectTime := Now;
    FOwnerComponent.FLastActivityTime := Now;

    // MEGA FIX: Reset reconnect attempts on successful connection
    FOwnerComponent.FReconnectAttempts := 0;
    FOwnerComponent.StopReconnectTimer;

    if Assigned(FOwnerComponent.FOnConnect) then
    begin
      if FOwnerComponent.FEventsUseMainThread then
      begin
        TThread.Queue(nil,
          procedure
          begin
            if Assigned(FOwnerComponent) and Assigned(FOwnerComponent.FOnConnect) then
            try
              FOwnerComponent.FOnConnect(FOwnerComponent);
            except
            end;
          end);
      end
      else
      begin
        try
          FOwnerComponent.FOnConnect(FOwnerComponent);
        except
        end;
      end;
    end;
  end;
end;

procedure TmMClientConnection.OnClose;
var
  OwnerRef: TmMClient;
begin
  // CRITICAL FIX: Store reference before clearing to prevent race condition
  OwnerRef := FOwnerComponent;

  if Assigned(OwnerRef) then
  begin
    // MEGA FIX: Clear the connection reference FIRST to avoid race conditions
    OwnerRef.FConnection := nil;
    OwnerRef.DoStateChange(csDisconnected);

    if Assigned(OwnerRef.FOnDisconnect) then
    begin
      if OwnerRef.FEventsUseMainThread then
      begin
        TThread.Queue(nil,
          procedure
          begin
            if Assigned(OwnerRef) and Assigned(OwnerRef.FOnDisconnect) then
            try
              OwnerRef.FOnDisconnect(OwnerRef);
            except
            end;
          end);
      end
      else
      begin
        try
          OwnerRef.FOnDisconnect(OwnerRef);
        except
        end;
      end;
    end;

    // CRITICAL FIX: IMMEDIATE reconnection check with proper reference
    if OwnerRef.FAutoReconnect and
       (OwnerRef.FReconnectAttempts < OwnerRef.FReconnectMaxAttempts) then
    begin
      // IMMEDIATE timer start - no delay needed
      TThread.Queue(nil,
        procedure
        begin
          if Assigned(OwnerRef) and
             OwnerRef.FAutoReconnect and
             (OwnerRef.FState = csDisconnected) and
             (OwnerRef.FReconnectAttempts < OwnerRef.FReconnectMaxAttempts) then
          begin
            OwnerRef.StartReconnectTimer;
          end;
        end);
    end;
  end;
  inherited OnClose;
end;

function TmMClientConnection.OnRead: TPollAsyncSocketOnReadWrite;
var
  bufferData: RawByteString;
  data: TBytes;
  messageSize: UInt32;
  Header: TSimpleMessage;
begin
  result := soContinue;
  try
    while fRd.Len > 0 do
    begin
      SetString(bufferData, PAnsiChar(fRd.Buffer), fRd.Len);
      if TSimpleProtocol.HasMagicMarker(bufferData) then
      begin
        if TSimpleProtocol.TryParseMessage(bufferData, data, FOwnerComponent.FEncryptionContext) then
        begin
          Move(bufferData[1], Header, SizeOf(Header));
          messageSize := SizeOf(Header) + Header.DataSize;
          fRd.Remove(messageSize);

          Inc(FBytesReceived, messageSize);
          if Assigned(FOwnerComponent) then
          begin
            Inc(FOwnerComponent.FTotalBytesReceived, messageSize);
            Inc(FOwnerComponent.FMessagesReceived);
            FOwnerComponent.FLastActivityTime := Now;
            FOwnerComponent.DoDataReceived(messageSize);
          end;
          ProcessCommand(data);
        end
        else
          break;
      end
      else
      begin
        if Assigned(FOwnerComponent) then
          FOwnerComponent.DoError('Invalid protocol data received', -1);
        result := soClose;
        break;
      end;
    end;
  except
    on E: Exception do
    begin
      if Assigned(FOwnerComponent) then
        FOwnerComponent.DoError('Error processing received data: ' + E.Message, -1);
      result := soClose;
    end;
  end;
end;

procedure TmMClientConnection.ProcessCommand(const Data: TBytes);
var
  response: TBytes;
begin
  if not Assigned(FOwnerComponent) or not Assigned(FOwnerComponent.FOnHandleCommand) then
    Exit;
  response := FOwnerComponent.FOnHandleCommand(FOwnerComponent, Data);
  // Let user decide encryption per message - no automatic encryption
  if Length(response) > 0 then
    TSimpleProtocol.SendMessage(SendProtocolMessage, response, False, FOwnerComponent.FEncryptionContext);
end;

procedure TmMClientConnection.SendProtocolMessage(const Message: RawByteString);
begin
  fOwner.WriteString(self, Message);
  Inc(FBytesSent, Length(Message));
  if Assigned(FOwnerComponent) then
  begin
    Inc(FOwnerComponent.FTotalBytesSent, Length(Message));
    Inc(FOwnerComponent.FMessagesSent);
    FOwnerComponent.FLastActivityTime := Now;
    FOwnerComponent.DoDataSent(Length(Message));
  end;
end;

{ TmMClient }

constructor TmMClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  PropertyLock := TCriticalSection.Create;
  FInitActive := False;
  FActive := False;
  FHost := 'localhost';
  FPort := 3434;
  FConnectionTimeout := 10;
  FThreadPoolSize := 1;
  FConnection := nil;
  FClient := nil;
  FState := csDisconnected;
  FLastConnectedServerIP := '';  // FIXED: Initialize last connected server IP

  // Encryption defaults - Only key and mode (no enabled flag)
  FEncryptionKey := '';
  FEncryptionMode := amCBC;
  FEncryptionKeySize := aks256;
  FEncryptionContext.Clear;

  FAutoReconnect := False;
  FReconnectStrategy := rsBackoff;
  FReconnectInterval := 5000;
  FReconnectMaxAttempts := 10;
  FReconnectAttempts := 0;
  FReconnectBackoffMultiplier := 2.0;
  FReconnectTimer := nil;

  FReceiveBufferSize := 8192;
  FSendBufferSize := 8192;
  FKeepAlive := True;
  FNoDelay := True;

  ResetStatistics;

  FVersion := '1.0.3';  // Updated version
  FDescription := 'mORMot2 Async Client Component with BULLETPROOF Reconnection After Force Disconnect';
  FEventsUseMainThread := True;

  FLogFamily := TSynLog.Family;
  FLogFamily.PerThreadLog := ptIdentifiedInOnFile;
  FLogLevel := llErrors;
  SetLogLevel(FLogLevel);
end;

destructor TmMClient.Destroy;
begin
  InternalCleanup;
  if Assigned(FReconnectTimer) then
    FReconnectTimer.Free;
  PropertyLock.Free;
  inherited Destroy;
end;

procedure TmMClient.Loaded;
begin
  inherited Loaded;
  if FInitActive then
    DoActivate(True);
end;

// MEGA FIX: Centralized cleanup procedure
procedure TmMClient.InternalCleanup;
begin
  StopReconnectTimer;

  // Clear connection first
  FConnection := nil;

  // Cleanup client
  if Assigned(FClient) then
  begin
    try
      FClient.Free;
    except
      // Ignore cleanup errors
    end;
    FClient := nil;
  end;
end;

procedure TmMClient.DoStateChange(NewState: TmMClientState);
var
  OldState: TmMClientState;
begin
  PropertyLock.Acquire;
  try
    OldState := FState;
    FState := NewState;
    FActive := (NewState = csConnected);
  finally
    PropertyLock.Release;
  end;

  if Assigned(FOnStateChange) then
  begin
    if FEventsUseMainThread then
    begin
      TThread.Queue(nil,
        procedure
        begin
          if Assigned(FOnStateChange) then
          try
            FOnStateChange(Self, OldState, NewState);
          except
          end;
        end);
    end
    else
    begin
      try
        FOnStateChange(Self, OldState, NewState);
      except
      end;
    end;
  end;
end;

procedure TmMClient.DoError(const ErrorMsg: string; ErrorCode: Integer = 0);
begin
  if Assigned(FOnError) then
  begin
    if FEventsUseMainThread then
    begin
      TThread.Queue(nil,
        procedure
        begin
          if Assigned(FOnError) then
          try
            FOnError(Self, ErrorMsg, ErrorCode);
          except
          end;
        end);
    end
    else
    begin
      try
        FOnError(Self, ErrorMsg, ErrorCode);
      except
      end;
    end;
  end;
end;

procedure TmMClient.DoDataSent(BytesSent: Integer);
begin
  if Assigned(FOnDataSent) then
  begin
    if FEventsUseMainThread then
    begin
      TThread.Queue(nil,
        procedure
        begin
          if Assigned(FOnDataSent) then
          try
            FOnDataSent(Self, BytesSent);
          except
          end;
        end);
    end
    else
    begin
      try
        FOnDataSent(Self, BytesSent);
      except
      end;
    end;
  end;
end;

procedure TmMClient.DoDataReceived(BytesReceived: Integer);
begin
  if Assigned(FOnDataReceived) then
  begin
    if FEventsUseMainThread then
    begin
      TThread.Queue(nil,
        procedure
        begin
          if Assigned(FOnDataReceived) then
          try
            FOnDataReceived(Self, BytesReceived);
          except
          end;
        end);
    end
    else
    begin
      try
        FOnDataReceived(Self, BytesReceived);
      except
      end;
    end;
  end;
end;

// UpdateEncryptionContext - Only when key is provided
procedure TmMClient.UpdateEncryptionContext;
begin
  PropertyLock.Acquire;
  try
    if FEncryptionKey <> '' then
      FEncryptionContext.SetKey(FEncryptionKey, FEncryptionMode, FEncryptionKeySize)
    else
      FEncryptionContext.Clear;
  finally
    PropertyLock.Release;
  end;
end;

// MEGA FIX: More aggressive reconnection timer handler
procedure TmMClient.OnReconnectTimer;
begin
  StopReconnectTimer;

  // CRITICAL FIX: More permissive reconnection conditions
  if FAutoReconnect and (FReconnectAttempts < FReconnectMaxAttempts) then
  begin
    try
      // FORCE state to disconnected if needed
      if FState <> csDisconnected then
      begin
        InternalCleanup;
        DoStateChange(csDisconnected);
      end;

      Connect;
      // Success: attempts reset in AfterCreate
    except
      on E: Exception do
      begin
        Inc(FReconnectAttempts); // Increment on failure
        DoError('Reconnection attempt ' + IntToStr(FReconnectAttempts) + ' failed: ' + E.Message, FReconnectAttempts);

        if FReconnectAttempts < FReconnectMaxAttempts then
        begin
          // IMMEDIATE retry with shorter interval for force disconnects
          StartReconnectTimer;
        end
        else
          DoError('Max reconnection attempts reached. Auto-reconnect disabled.', -99);
      end;
    end;
  end;
end;

procedure TmMClient.StartReconnectTimer;
var
  Interval: Integer;
begin
  if not Assigned(FReconnectTimer) then
    FReconnectTimer := TmMReconnectTimer.Create(Self);

  case FReconnectStrategy of
    rsImmediate: Interval := 100; // Very fast for force disconnects
    rsBackoff:
      begin
        if FReconnectAttempts = 0 then
          Interval := 500  // First attempt after kick - quick
        else
          Interval := Round(FReconnectInterval * Power(FReconnectBackoffMultiplier, FReconnectAttempts - 1));
      end;
    rsFixedInterval: Interval := FReconnectInterval;
  else
    Exit;
  end;

  // Cap maximum interval at 30 seconds for better responsiveness
  if Interval > 30000 then
    Interval := 30000;

  FReconnectTimer.Start(Interval);
end;

procedure TmMClient.StopReconnectTimer;
begin
  if Assigned(FReconnectTimer) then
    FReconnectTimer.Stop;
end;

procedure TmMClient.ResetStatistics;
begin
  PropertyLock.Acquire;
  try
    FConnectTime := 0;
    FLastActivityTime := 0;
    FTotalBytesReceived := 0;
    FTotalBytesSent := 0;
    FMessagesReceived := 0;
    FMessagesSent := 0;
  finally
    PropertyLock.Release;
  end;
end;

// NEW: Manual reset of reconnection state
procedure TmMClient.ResetReconnectionState;
begin
  PropertyLock.Acquire;
  try
    StopReconnectTimer;
    FReconnectAttempts := 0;
  finally
    PropertyLock.Release;
  end;
end;

// Property methods
function TmMClient.GetActive: Boolean;
begin
  PropertyLock.Acquire;
  try
    Result := FActive;
  finally
    PropertyLock.Release;
  end;
end;

procedure TmMClient.SetActive(const Value: Boolean);
begin
  PropertyLock.Acquire;
  try
    if not(csLoading in ComponentState) then
      DoActivate(Value);
    FInitActive := Value;
  finally
    PropertyLock.Release;
  end;
end;

function TmMClient.GetHost: string;
begin
  PropertyLock.Acquire;
  try
    Result := FHost;
  finally
    PropertyLock.Release;
  end;
end;

procedure TmMClient.SetHost(const Value: string);
begin
  if not(csLoading in ComponentState) then
    if GetActive then
      raise Exception.Create('Cannot change Host while connected');
  PropertyLock.Acquire;
  try
    FHost := Value;
  finally
    PropertyLock.Release;
  end;
end;

function TmMClient.GetPort: Integer;
begin
  PropertyLock.Acquire;
  try
    Result := FPort;
  finally
    PropertyLock.Release;
  end;
end;

procedure TmMClient.SetPort(const Value: Integer);
begin
  if not(csLoading in ComponentState) then
    if GetActive then
      raise Exception.Create('Cannot change Port while connected');
  PropertyLock.Acquire;
  try
    FPort := Value;
  finally
    PropertyLock.Release;
  end;
end;

function TmMClient.GetConnectionTimeout: Integer;
begin
  PropertyLock.Acquire;
  try
    Result := FConnectionTimeout;
  finally
    PropertyLock.Release;
  end;
end;

procedure TmMClient.SetConnectionTimeout(const Value: Integer);
begin
  if not(csLoading in ComponentState) then
    if GetActive then
      raise Exception.Create('Cannot change ConnectionTimeout while connected');
  PropertyLock.Acquire;
  try
    if Value < 1 then
      FConnectionTimeout := 1
    else
      FConnectionTimeout := Value;
  finally
    PropertyLock.Release;
  end;
end;

function TmMClient.GetThreadPoolSize: Integer;
begin
  PropertyLock.Acquire;
  try
    Result := FThreadPoolSize;
  finally
    PropertyLock.Release;
  end;
end;

procedure TmMClient.SetThreadPoolSize(const Value: Integer);
begin
  if not(csLoading in ComponentState) then
    if GetActive then
      raise Exception.Create('Cannot change ThreadPoolSize while connected');
  PropertyLock.Acquire;
  try
    if Value < 1 then
      FThreadPoolSize := 1
    else
      FThreadPoolSize := Value;
  finally
    PropertyLock.Release;
  end;
end;

function TmMClient.GetEventsUseMainThread: Boolean;
begin
  PropertyLock.Acquire;
  try
    Result := FEventsUseMainThread;
  finally
    PropertyLock.Release;
  end;
end;

procedure TmMClient.SetEventsUseMainThread(const Value: Boolean);
begin
  PropertyLock.Acquire;
  try
    FEventsUseMainThread := Value;
  finally
    PropertyLock.Release;
  end;
end;

function TmMClient.GetLogLevel: TmMClientLogLevel;
begin
  PropertyLock.Acquire;
  try
    Result := FLogLevel;
  finally
    PropertyLock.Release;
  end;
end;

procedure TmMClient.SetLogLevel(const Value: TmMClientLogLevel);
begin
  PropertyLock.Acquire;
  try
    FLogLevel := Value;
    case FLogLevel of
      llNone: FLogFamily.Level := LOG_FILTER[lfNone];
      llErrors: FLogFamily.Level := LOG_FILTER[lfErrors];
      llDebug: FLogFamily.Level := LOG_FILTER[lfDebug];
      llClientServer: FLogFamily.Level := LOG_FILTER[lfClientServer];
      llVerbose: FLogFamily.Level := LOG_VERBOSE;
    end;
  finally
    PropertyLock.Release;
  end;
end;

function TmMClient.GetState: TmMClientState;
begin
  PropertyLock.Acquire;
  try
    Result := FState;
  finally
    PropertyLock.Release;
  end;
end;

procedure TmMClient.SetState(const Value: TmMClientState);
begin
  DoStateChange(Value);
end;

// FIXED: Remote IP property method - returns stored IP when disconnected
function TmMClient.GetRemoteIP: string;
begin
  PropertyLock.Acquire;
  try
    if Assigned(FConnection) then
      Result := FConnection.RemoteIP
    else
      Result := FLastConnectedServerIP;  // FIXED: Use stored IP when disconnected
  finally
    PropertyLock.Release;
  end;
end;

// Encryption property methods - Only key and mode (no enabled flag)
function TmMClient.GetEncryptionKey: string;
begin
  PropertyLock.Acquire;
  try
    Result := FEncryptionKey;
  finally
    PropertyLock.Release;
  end;
end;

procedure TmMClient.SetEncryptionKey(const Value: string);
begin
  if not(csLoading in ComponentState) then
    if GetActive then
      raise Exception.Create('Cannot change encryption key while connected');
  PropertyLock.Acquire;
  try
    FEncryptionKey := Value;
    UpdateEncryptionContext;
  finally
    PropertyLock.Release;
  end;
end;

function TmMClient.GetEncryptionMode: TAESMode;
begin
  PropertyLock.Acquire;
  try
    Result := FEncryptionMode;
  finally
    PropertyLock.Release;
  end;
end;

procedure TmMClient.SetEncryptionMode(const Value: TAESMode);
begin
  if not(csLoading in ComponentState) then
    if GetActive then
      raise Exception.Create('Cannot change encryption mode while connected');
  PropertyLock.Acquire;
  try
    FEncryptionMode := Value;
    UpdateEncryptionContext;
  finally
    PropertyLock.Release;
  end;
end;

function TmMClient.GetEncryptionKeySize: TAESKeySize;
begin
  PropertyLock.Acquire;
  try
    Result := FEncryptionKeySize;
  finally
    PropertyLock.Release;
  end;
end;

procedure TmMClient.SetEncryptionKeySize(const Value: TAESKeySize);
begin
  if not(csLoading in ComponentState) then
    if GetActive then
      raise Exception.Create('Cannot change encryption key size while connected');
  PropertyLock.Acquire;
  try
    FEncryptionKeySize := Value;
    UpdateEncryptionContext;
  finally
    PropertyLock.Release;
  end;
end;

function TmMClient.GetEncryptionInfo: string;
begin
  PropertyLock.Acquire;
  try
    if FEncryptionKey <> '' then
      Result := FEncryptionContext.GetModeString
    else
      Result := 'NO KEY SET';
  finally
    PropertyLock.Release;
  end;
end;

// Reconnection property methods
function TmMClient.GetAutoReconnect: Boolean;
begin
  PropertyLock.Acquire;
  try
    Result := FAutoReconnect;
  finally
    PropertyLock.Release;
  end;
end;

procedure TmMClient.SetAutoReconnect(const Value: Boolean);
begin
  PropertyLock.Acquire;
  try
    FAutoReconnect := Value;
    if not Value then
      StopReconnectTimer;
  finally
    PropertyLock.Release;
  end;
end;

function TmMClient.GetReconnectStrategy: TmMReconnectStrategy;
begin
  PropertyLock.Acquire;
  try
    Result := FReconnectStrategy;
  finally
    PropertyLock.Release;
  end;
end;

procedure TmMClient.SetReconnectStrategy(const Value: TmMReconnectStrategy);
begin
  PropertyLock.Acquire;
  try
    FReconnectStrategy := Value;
  finally
    PropertyLock.Release;
  end;
end;

function TmMClient.GetReconnectInterval: Integer;
begin
  PropertyLock.Acquire;
  try
    Result := FReconnectInterval;
  finally
    PropertyLock.Release;
  end;
end;

procedure TmMClient.SetReconnectInterval(const Value: Integer);
begin
  PropertyLock.Acquire;
  try
    if Value < 100 then
      FReconnectInterval := 100
    else
      FReconnectInterval := Value;
  finally
    PropertyLock.Release;
  end;
end;

function TmMClient.GetReconnectMaxAttempts: Integer;
begin
  PropertyLock.Acquire;
  try
    Result := FReconnectMaxAttempts;
  finally
    PropertyLock.Release;
  end;
end;

procedure TmMClient.SetReconnectMaxAttempts(const Value: Integer);
begin
  PropertyLock.Acquire;
  try
    if Value < 0 then
      FReconnectMaxAttempts := 0
    else
      FReconnectMaxAttempts := Value;
  finally
    PropertyLock.Release;
  end;
end;

// Buffer property methods
function TmMClient.GetReceiveBufferSize: Integer;
begin
  PropertyLock.Acquire;
  try
    Result := FReceiveBufferSize;
  finally
    PropertyLock.Release;
  end;
end;

procedure TmMClient.SetReceiveBufferSize(const Value: Integer);
begin
  PropertyLock.Acquire;
  try
    if Value < 1024 then
      FReceiveBufferSize := 1024
    else
      FReceiveBufferSize := Value;
  finally
    PropertyLock.Release;
  end;
end;

function TmMClient.GetSendBufferSize: Integer;
begin
  PropertyLock.Acquire;
  try
    Result := FSendBufferSize;
  finally
    PropertyLock.Release;
  end;
end;

procedure TmMClient.SetSendBufferSize(const Value: Integer);
begin
  PropertyLock.Acquire;
  try
    if Value < 1024 then
      FSendBufferSize := 1024
    else
      FSendBufferSize := Value;
  finally
    PropertyLock.Release;
  end;
end;

function TmMClient.GetKeepAlive: Boolean;
begin
  PropertyLock.Acquire;
  try
    Result := FKeepAlive;
  finally
    PropertyLock.Release;
  end;
end;

procedure TmMClient.SetKeepAlive(const Value: Boolean);
begin
  PropertyLock.Acquire;
  try
    FKeepAlive := Value;
  finally
    PropertyLock.Release;
  end;
end;

function TmMClient.GetNoDelay: Boolean;
begin
  PropertyLock.Acquire;
  try
    Result := FNoDelay;
  finally
    PropertyLock.Release;
  end;
end;

procedure TmMClient.SetNoDelay(const Value: Boolean);
begin
  PropertyLock.Acquire;
  try
    FNoDelay := Value;
  finally
    PropertyLock.Release;
  end;
end;

// Statistics property methods
function TmMClient.GetConnectedTime: Integer;
begin
  PropertyLock.Acquire;
  try
    if (FState = csConnected) and (FConnectTime > 0) then
      Result := SecondsBetween(Now, FConnectTime)
    else
      Result := 0;
  finally
    PropertyLock.Release;
  end;
end;

function TmMClient.GetLastActivityTime: TDateTime;
begin
  PropertyLock.Acquire;
  try
    Result := FLastActivityTime;
  finally
    PropertyLock.Release;
  end;
end;

function TmMClient.GetTotalBytesReceived: Int64;
begin
  PropertyLock.Acquire;
  try
    Result := FTotalBytesReceived;
  finally
    PropertyLock.Release;
  end;
end;

function TmMClient.GetTotalBytesSent: Int64;
begin
  PropertyLock.Acquire;
  try
    Result := FTotalBytesSent;
  finally
    PropertyLock.Release;
  end;
end;

function TmMClient.GetMessagesReceived: Int64;
begin
  PropertyLock.Acquire;
  try
    Result := FMessagesReceived;
  finally
    PropertyLock.Release;
  end;
end;

function TmMClient.GetMessagesSent: Int64;
begin
  PropertyLock.Acquire;
  try
    Result := FMessagesSent;
  finally
    PropertyLock.Release;
  end;
end;

// DoActivate method
procedure TmMClient.DoActivate(aActivate: Boolean);
begin
  if aActivate = GetActive then
    Exit;
  if aActivate then
    Connect
  else
    Disconnect;
end;

// MEGA FIX: Bulletproof Connect method
procedure TmMClient.Connect;
begin
  if FState in [csConnected, csConnecting] then
    Exit;

  DoStateChange(csConnecting);

  // MEGA FIX: Complete cleanup before new connection
  InternalCleanup;

  try
    GlobalClientComponent := Self;
    UpdateEncryptionContext;

    // MEGA FIX: Small delay for reconnections to ensure complete cleanup
    if FReconnectAttempts > 0 then
      Sleep(200);

    FClient := TAsyncClient.Create(
      FHost,
      IntToStr(FPort),
      1,
      FConnectionTimeout,
      nil, nil,
      TmMClientConnection,
      'mMClient',
      FLogFamily.SynLogClass,
      [],
      FThreadPoolSize
    );

  except
    on E: Exception do
    begin
      InternalCleanup; // MEGA FIX: Use centralized cleanup
      DoStateChange(csDisconnected);
      DoError('Connection failed: ' + E.Message, -1);

      // MEGA FIX: Reconnection handled in OnReconnectTimer now
      raise;
    end;
  end;
end;

// MEGA FIX: Improved Disconnect method
procedure TmMClient.Disconnect;
begin
  if FState = csDisconnected then
    Exit;

  DoStateChange(csDisconnecting);
  InternalCleanup; // MEGA FIX: Use centralized cleanup
  DoStateChange(csDisconnected);
end;

procedure TmMClient.Reconnect;
begin
  Disconnect;
  ResetReconnectionState; // MEGA FIX: Reset attempts on manual reconnect
  Connect;
end;

// ONLY ONE DATA TRANSMISSION METHOD
procedure TmMClient.SendData(const Data: TBytes; Encrypted: Boolean = False);
begin
  if not IsConnected or not Assigned(FConnection) then
    raise Exception.Create('Client not connected');

  TSimpleProtocol.SendMessage(FConnection.SendProtocolMessage, Data, Encrypted, FEncryptionContext);
end;

// Utility methods
procedure TmMClient.ClearStatistics;
begin
  ResetStatistics;
end;

function TmMClient.IsConnected: Boolean;
begin
  Result := (FState = csConnected) and Assigned(FConnection);
end;

function TmMClient.GetConnectionInfo: string;
const
  StateNames: array[TmMClientState] of string = ('Disconnected', 'Connecting', 'Connected', 'Disconnecting');
begin
  PropertyLock.Acquire;
  try
    Result := Format('Host: %s, Port: %d, State: %s, Connected: %s, Remote IP: %s, Encryption: %s',
      [FHost, FPort,
       StateNames[FState],
       BoolToStr(IsConnected, True),
       GetRemoteIP,
       GetEncryptionInfo]);
  finally
    PropertyLock.Release;
  end;
end;

end.
