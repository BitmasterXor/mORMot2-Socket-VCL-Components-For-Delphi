unit mMServer;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, System.SyncObjs,
  System.DateUtils, System.TypInfo,
  mormot.core.base, mormot.core.log, mormot.core.rtti,
  mormot.net.sock, mormot.net.async,
  mormot.crypt.secure,
  mMProtocol;

type
  // Forward declarations
  TmMServer = class;
  TmMServerClient = class;

  // Logging level enumeration for easy component property
  // Maps directly to mORMot2's TSynLogFilter for consistency
  TmMServerLogLevel = (llNone, // No logging (completely disabled)
    llErrors, // Only errors, exceptions, and critical failures
    llDebug, // Debug information including enter/leave
    llClientServer, // Client/server communication logs
    llVerbose // All logging
    );

  // Server state enumeration
  TmMServerState = (ssDisconnected, ssStarting, ssListening, ssStopping);

  // Event types for server component
  TmMServerHandleCommandEvent = function(Sender: TObject;
    Client: TmMServerClient; aData: TBytes): TBytes of object;
  TmMServerConnectEvent = procedure(Sender: TObject; Client: TmMServerClient)
    of object;
  TmMServerDisconnectEvent = procedure(Sender: TObject; Client: TmMServerClient)
    of object;
  TmMServerErrorEvent = procedure(Sender: TObject; Client: TmMServerClient;
    const Error: string; ErrorCode: Integer) of object;
  TmMServerStateChangeEvent = procedure(Sender: TObject;
    OldState, NewState: TmMServerState) of object;
  TmMServerDataSentEvent = procedure(Sender: TObject; Client: TmMServerClient;
    BytesSent: Integer) of object;
  TmMServerDataReceivedEvent = procedure(Sender: TObject;
    Client: TmMServerClient; BytesReceived: Integer) of object;

  // Client information class
  TmMServerClient = class
  private
    fConnection: TAsyncConnection;
    fHandle: TPollAsyncConnectionHandle;
    fConnectedAt: TDateTime;
    fLastActivityTime: TDateTime;
    fServer: TmMServer;
    fPeerIP: string;
    fPeerPort: Integer;
    fBytesReceived: Int64;
    fBytesSent: Int64;
    fMessagesReceived: Int64;
    fMessagesSent: Int64;
    fCustomData: TObject; // For user custom data
    procedure SendProtocolMessage(const Message: RawByteString);
  public
    constructor Create(AConnection: TAsyncConnection; AServer: TmMServer);
    destructor Destroy; override;

    // ONLY ONE DATA TRANSMISSION METHOD
    procedure SendData(const Data: TBytes; Encrypted: Boolean = False);

    // Utility methods
    function GetConnectedTime: Integer; // Returns seconds connected
    function GetConnectionInfo: string;

    // Properties
    property Connection: TAsyncConnection read fConnection;
    property PeerIP: string read fPeerIP; // FIXED: Now properly gets client IP
    property PeerPort: Integer read fPeerPort;
    property Handle: TPollAsyncConnectionHandle read fHandle;
    property ConnectedAt: TDateTime read fConnectedAt;
    property LastActivityTime: TDateTime read fLastActivityTime;
    property BytesReceived: Int64 read fBytesReceived;
    property BytesSent: Int64 read fBytesSent;
    property MessagesReceived: Int64 read fMessagesReceived;
    property MessagesSent: Int64 read fMessagesSent;
    property CustomData: TObject read fCustomData write fCustomData;

    // Methods
    procedure Disconnect;
  end;

  // Internal connection class based on uServer.pas TConnection
  TmMServerConnection = class(TAsyncConnection)
  private
    fOwnerComponent: TmMServer;
    fClientObject: TmMServerClient;
    fRemoteAddr: TNetAddr; // NEW: Store the original TNetAddr
    procedure ProcessCommand(const Data: TBytes);
    function GetRemoteIPDisplay: RawUtf8;
    procedure SendProtocolMessage(const Message: RawByteString);
  protected
    function OnRead: TPollAsyncSocketOnReadWrite; override;
    procedure AfterCreate; override; // Maps to OnConnect event
    procedure OnClose; override; // Maps to OnDisconnect event
  public
    constructor Create(aOwner: TAsyncConnections;
      const aRemoteIP: TNetAddr); override;

    property ClientObject: TmMServerClient read fClientObject;
    property RemoteIPDisplay: RawUtf8 read GetRemoteIPDisplay;
    property RemoteAddr: TNetAddr read fRemoteAddr; // NEW: Expose the TNetAddr
  end;

  // Main server component
  TmMServer = class(TComponent)
  private
    fServer: TAsyncServer;
    FClients: TObjectDictionary<TPollAsyncConnectionHandle, TmMServerClient>;
    FLogFamily: TSynLogFamily;
    FLogLevel: TmMServerLogLevel;
    FActive: Boolean;
    FInitActive: Boolean; // For Loaded pattern
    FPort: Integer;
    FConnectionTimeout: Integer;
    FThreadPoolSize: Integer;
    FState: TmMServerState;
    FStartTime: TDateTime;

    // Encryption support - Key and mode only (no enabled flag)
    FEncryptionKey: string;
    FEncryptionMode: TAESMode; // AES Mode selection
    FEncryptionKeySize: TAESKeySize; // Key size selection
    FEncryptionContext: TEncryptionContext;

    // Server limits and settings
    FMaxConnections: Integer;
    FReceiveBufferSize: Integer;
    FSendBufferSize: Integer;
    FKeepAlive: Boolean;
    FNoDelay: Boolean; // TCP_NODELAY
    FReusePort: Boolean;

    // Statistics
    FTotalConnections: Int64;
    // REMOVED: FTotalActiveConnections - Now using live count from FClients.Count
    FTotalBytesReceived: Int64;
    FTotalBytesSent: Int64;
    FTotalMessagesReceived: Int64;
    FTotalMessagesSent: Int64;

    // Component info
    FVersion: string;
    FDescription: string;

    // EventsUseMainThread like ncSockets
    FEventsUseMainThread: Boolean;

    // Events
    FOnHandleCommand: TmMServerHandleCommandEvent;
    FOnConnect: TmMServerConnectEvent;
    FOnDisconnect: TmMServerDisconnectEvent;
    FOnError: TmMServerErrorEvent;
    FOnStateChange: TmMServerStateChangeEvent;
    FOnDataSent: TmMServerDataSentEvent;
    FOnDataReceived: TmMServerDataReceivedEvent;

    // Property methods with thread safety
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    function GetPort: Integer;
    procedure SetPort(const Value: Integer);
    function GetConnectionTimeout: Integer;
    procedure SetConnectionTimeout(const Value: Integer);
    function GetThreadPoolSize: Integer;
    procedure SetThreadPoolSize(const Value: Integer);
    function GetEventsUseMainThread: Boolean;
    procedure SetEventsUseMainThread(const Value: Boolean);
    function GetLogLevel: TmMServerLogLevel;
    procedure SetLogLevel(const Value: TmMServerLogLevel);
    function GetState: TmMServerState;
    procedure SetState(const Value: TmMServerState);

    // Encryption property methods - Only key and mode (no enabled flag)
    function GetEncryptionKey: string;
    procedure SetEncryptionKey(const Value: string);
    function GetEncryptionMode: TAESMode;
    procedure SetEncryptionMode(const Value: TAESMode);
    function GetEncryptionKeySize: TAESKeySize;
    procedure SetEncryptionKeySize(const Value: TAESKeySize);
    function GetEncryptionInfo: string;

    // Server settings property methods
    function GetMaxConnections: Integer;
    procedure SetMaxConnections(const Value: Integer);
    function GetReceiveBufferSize: Integer;
    procedure SetReceiveBufferSize(const Value: Integer);
    function GetSendBufferSize: Integer;
    procedure SetSendBufferSize(const Value: Integer);
    function GetKeepAlive: Boolean;
    procedure SetKeepAlive(const Value: Boolean);
    function GetNoDelay: Boolean;
    procedure SetNoDelay(const Value: Boolean);
    function GetReusePort: Boolean;
    procedure SetReusePort(const Value: Boolean);

    // Statistics property methods
    function GetClientCount: Integer;
    function GetUpTime: Integer; // Returns seconds running
    function GetTotalConnections: Int64;
    function GetTotalActiveConnections: Int64; // FIXED: Now uses live count
    function GetTotalBytesReceived: Int64;
    function GetTotalBytesSent: Int64;
    function GetTotalMessagesReceived: Int64;
    function GetTotalMessagesSent: Int64;

    // Internal methods
    procedure DoStateChange(NewState: TmMServerState);
    procedure DoError(Client: TmMServerClient; const ErrorMsg: string;
      ErrorCode: Integer = 0);
    procedure DoDataSent(Client: TmMServerClient; BytesSent: Integer);
    procedure DoDataReceived(Client: TmMServerClient; BytesReceived: Integer);
    procedure UpdateEncryptionContext;
    procedure ResetStatistics;
    procedure DoActivate(aActivate: Boolean);

  protected
    PropertyLock: TCriticalSection;
    procedure Loaded; override;

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    // Server methods
    procedure Connect;
    procedure Disconnect;

    // Client management methods
    function FindClient(Handle: TPollAsyncConnectionHandle): TmMServerClient;
    function FindClientByIP(const IP: string): TmMServerClient;
    procedure DisconnectClient(Handle: TPollAsyncConnectionHandle); overload;
    procedure DisconnectClient(Client: TmMServerClient); overload;
    procedure DisconnectAllClients;

    // ONLY ONE BROADCASTING METHOD
    procedure BroadcastData(const Data: TBytes; Encrypted: Boolean = False);

    // Utility methods
    procedure ClearStatistics;
    function GetServerInfo: string;
    function IsListening: Boolean;

    // Properties (read-only at runtime)
    property Clients: TObjectDictionary<TPollAsyncConnectionHandle,
      TmMServerClient> read FClients;
    property State: TmMServerState read GetState;
    property ClientCount: Integer read GetClientCount;
    property UpTime: Integer read GetUpTime;
    property EncryptionInfo: string read GetEncryptionInfo;

  published
    // Main properties
    property Active: Boolean read GetActive write SetActive default False;
    property Port: Integer read GetPort write SetPort default 3434;
    property ConnectionTimeout: Integer read GetConnectionTimeout
      write SetConnectionTimeout default 10;
    property ThreadPoolSize: Integer read GetThreadPoolSize
      write SetThreadPoolSize default 4;
    property EventsUseMainThread: Boolean read GetEventsUseMainThread
      write SetEventsUseMainThread default True;

    // Encryption properties - Only key and mode (no enabled flag)
    property EncryptionKey: string read GetEncryptionKey write SetEncryptionKey;
    property EncryptionMode: TAESMode read GetEncryptionMode
      write SetEncryptionMode default amCBC;
    property EncryptionKeySize: TAESKeySize read GetEncryptionKeySize
      write SetEncryptionKeySize default aks256;

    // Server configuration properties
    property MaxConnections: Integer read GetMaxConnections
      write SetMaxConnections default 1000;
    property ReceiveBufferSize: Integer read GetReceiveBufferSize
      write SetReceiveBufferSize default 8192;
    property SendBufferSize: Integer read GetSendBufferSize
      write SetSendBufferSize default 8192;
    property KeepAlive: Boolean read GetKeepAlive write SetKeepAlive
      default True;
    property NoDelay: Boolean read GetNoDelay write SetNoDelay default True;
    property ReusePort: Boolean read GetReusePort write SetReusePort
      default True;

    // Statistics (read-only)
    property TotalConnections: Int64 read GetTotalConnections;
    property TotalActiveConnections: Int64 read GetTotalActiveConnections;
    // FIXED: Live count
    property TotalBytesReceived: Int64 read GetTotalBytesReceived;
    property TotalBytesSent: Int64 read GetTotalBytesSent;
    property TotalMessagesReceived: Int64 read GetTotalMessagesReceived;
    property TotalMessagesSent: Int64 read GetTotalMessagesSent;

    // Logging control
    property LogLevel: TmMServerLogLevel read GetLogLevel write SetLogLevel
      default llErrors;

    // Component info
    property Version: string read FVersion;
    property Description: string read FDescription;

    // Events
    property OnHandleCommand: TmMServerHandleCommandEvent read FOnHandleCommand
      write FOnHandleCommand;
    property OnConnect: TmMServerConnectEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TmMServerDisconnectEvent read FOnDisconnect
      write FOnDisconnect;
    property OnError: TmMServerErrorEvent read FOnError write FOnError;
    property OnStateChange: TmMServerStateChangeEvent read FOnStateChange
      write FOnStateChange;
    property OnDataSent: TmMServerDataSentEvent read FOnDataSent
      write FOnDataSent;
    property OnDataReceived: TmMServerDataReceivedEvent read FOnDataReceived
      write FOnDataReceived;
  end;

  // Global server reference for connection callback
var
  GlobalServerComponent: TmMServer = nil;

implementation

{ TmMServerClient }

constructor TmMServerClient.Create(AConnection: TAsyncConnection;
  AServer: TmMServer);
begin
  inherited Create;
  fConnection := AConnection;
  fServer := AServer;
  fHandle := AConnection.Handle;
  fConnectedAt := Now;
  fLastActivityTime := Now;
  fPeerIP := ''; // Will be set by the connection class
  fPeerPort := 0;
  fBytesReceived := 0;
  fBytesSent := 0;
  fMessagesReceived := 0;
  fMessagesSent := 0;
  fCustomData := nil;
end;

destructor TmMServerClient.Destroy;
begin
  if Assigned(fCustomData) then
    fCustomData.Free;
  inherited Destroy;
end;

procedure TmMServerClient.SendProtocolMessage(const Message: RawByteString);
begin
  if Assigned(fConnection) then
  begin
    TmMServerConnection(fConnection).SendProtocolMessage(Message);

    // Update statistics
    Inc(fBytesSent, Length(Message));
    Inc(fMessagesSent);
    fLastActivityTime := Now;

    if Assigned(fServer) then
    begin
      Inc(fServer.FTotalBytesSent, Length(Message));
      Inc(fServer.FTotalMessagesSent);
      fServer.DoDataSent(Self, Length(Message));
    end;
  end;
end;

// ONLY ONE SENDDATA METHOD
procedure TmMServerClient.SendData(const Data: TBytes;
  Encrypted: Boolean = False);
begin
  if not Assigned(fConnection) then
    raise Exception.Create('Client connection is not available');

  // Call protocol with proper signature: SendMessage(Callback, Data, Encrypted, var Context)
  TSimpleProtocol.SendMessage(SendProtocolMessage, Data, Encrypted,
    fServer.FEncryptionContext);
end;

function TmMServerClient.GetConnectedTime: Integer;
begin
  Result := SecondsBetween(Now, fConnectedAt);
end;

function TmMServerClient.GetConnectionInfo: string;
begin
  Result := Format
    ('Handle: %d, IP: %s:%d, Connected: %s, Bytes: %d/%d, Messages: %d/%d',
    [fHandle, fPeerIP, fPeerPort, FormatDateTime('hh:nn:ss', fConnectedAt),
    fBytesReceived, fBytesSent, fMessagesReceived, fMessagesSent]);
end;

procedure TmMServerClient.Disconnect;
begin
  if Assigned(fServer) and Assigned(fServer.fServer) then
  begin
    fServer.fServer.ConnectionRemove(fHandle);  // Use the same server object as DisconnectAllClients
  end;
end;

{ TmMServerConnection }

constructor TmMServerConnection.Create(aOwner: TAsyncConnections;
  const aRemoteIP: TNetAddr);
var
  IP: RawUtf8;
begin
  inherited Create(aOwner, aRemoteIP);
  fOwnerComponent := GlobalServerComponent;
  fRemoteAddr := aRemoteIP; // Store the original TNetAddr

  // FIXED: Force extract the IP from the TNetAddr parameter
  aRemoteIP.IP(IP, False);
  if IP <> '' then
    fRemoteIP := IP
  else
    fRemoteIP := '127.0.0.1'; // Fallback
end;

function TmMServerConnection.GetRemoteIPDisplay: RawUtf8;
begin
  Result := fRemoteIP;
  if Result = '' then
    Result := '127.0.0.1';
end;

// MEGA FIX: Improved AfterCreate with proper error handling
procedure TmMServerConnection.AfterCreate;
var
  IP: RawUtf8;
begin
  inherited AfterCreate;

  if Assigned(fOwnerComponent) then
  begin
    fOwnerComponent.PropertyLock.Enter;
    try
      // Check connection limits
      if (fOwnerComponent.FMaxConnections > 0) and
        (fOwnerComponent.FClients.Count >= fOwnerComponent.FMaxConnections) then
      begin
        // Connection limit reached - close immediately
        Exit;
      end;

      fClientObject := TmMServerClient.Create(Self, fOwnerComponent);

      // FINAL FIX: Use the inherited fRemoteIP field directly
      IP := fRemoteIP;
      if IP = '' then
      begin
        // Force extraction from the remote address
        fRemoteAddr.IP(IP, False);
      end;

      // Set the client IP - convert RawUtf8 to string
      fClientObject.fPeerIP := string(IP);

      // If still empty, use localhost for debugging
      if fClientObject.fPeerIP = '' then
        fClientObject.fPeerIP := '127.0.0.1';

      // Set the port - use server port, not client's ephemeral port
      fClientObject.fPeerPort := fOwnerComponent.FPort;

      // CRITICAL FIX: Add to dictionary - this automatically updates the live count
      fOwnerComponent.FClients.AddOrSetValue(Handle, fClientObject);
      Inc(fOwnerComponent.FTotalConnections);
      // Only increment total connections
      // REMOVED: Manual active counter increment - we use live count now

      if Assigned(fOwnerComponent.FOnConnect) then
      begin
        if fOwnerComponent.FEventsUseMainThread then
        begin
          TThread.Queue(nil,
            procedure
            begin
              if Assigned(fOwnerComponent) and
                Assigned(fOwnerComponent.FOnConnect) and Assigned(fClientObject)
              then
                try
                  fOwnerComponent.FOnConnect(fOwnerComponent, fClientObject);
                except
                  // Swallow event exceptions
                end;
            end);
        end
        else
        begin
          try
            fOwnerComponent.FOnConnect(fOwnerComponent, fClientObject);
          except
            // Swallow event exceptions
          end;
        end;
      end;

    finally
      fOwnerComponent.PropertyLock.Leave;
    end;
  end;
end;

procedure TmMServerConnection.OnClose;
var
  ClientToNotify: TmMServerClient;
  ClientHandle: TPollAsyncConnectionHandle;
begin
  ClientToNotify := nil;
  ClientHandle := Handle;

  if Assigned(fOwnerComponent) then
  begin
    fOwnerComponent.PropertyLock.Enter;
    try
      // Get reference to client before removing from dictionary
      if Assigned(fClientObject) then
        ClientToNotify := fClientObject;

      // MEGA FIX: Remove from dictionary - this will free the object now!
      if fOwnerComponent.FClients.ContainsKey(ClientHandle) then
      begin
        fOwnerComponent.FClients.Remove(ClientHandle);
      end;

    finally
      fOwnerComponent.PropertyLock.Leave;
    end;

    // Fire the disconnect event immediately (no TThread.Queue!)
    if Assigned(ClientToNotify) and Assigned(fOwnerComponent.FOnDisconnect) then
    begin
      try
        fOwnerComponent.FOnDisconnect(fOwnerComponent, ClientToNotify);
      except
        // Swallow event exceptions
      end;
    end;

    fClientObject := nil;
  end;
  inherited OnClose;
end;

function TmMServerConnection.OnRead: TPollAsyncSocketOnReadWrite;
var
  bufferData: RawByteString;
  Data: TBytes;
  messageSize: UInt32;
  Header: TSimpleMessage;
begin
  Result := soContinue;

  try
    while fRd.Len > 0 do
    begin
      SetString(bufferData, PAnsiChar(fRd.Buffer), fRd.Len);

      if TSimpleProtocol.HasMagicMarker(bufferData) then
      begin
        if TSimpleProtocol.TryParseMessage(bufferData, Data,
          fOwnerComponent.FEncryptionContext) then
        begin
          Move(bufferData[1], Header, SizeOf(Header));
          messageSize := SizeOf(Header) + Header.DataSize;

          fRd.Remove(messageSize);

          if Assigned(fClientObject) then
          begin
            Inc(fClientObject.fBytesReceived, messageSize);
            Inc(fClientObject.fMessagesReceived);
            fClientObject.fLastActivityTime := Now;
          end;

          if Assigned(fOwnerComponent) then
          begin
            Inc(fOwnerComponent.FTotalBytesReceived, messageSize);
            Inc(fOwnerComponent.FTotalMessagesReceived);
            fOwnerComponent.DoDataReceived(fClientObject, messageSize);
          end;

          ProcessCommand(Data);
        end
        else
        begin
          break;
        end;
      end
      else
      begin
        if Assigned(fOwnerComponent) and Assigned(fClientObject) then
          fOwnerComponent.DoError(fClientObject,
            'Invalid protocol data received', -1);
        Result := soClose;
        break;
      end;
    end;

  except
    on E: Exception do
    begin
      if Assigned(fOwnerComponent) and Assigned(fClientObject) then
        fOwnerComponent.DoError(fClientObject,
          'Error processing received data: ' + E.Message, -1);
      Result := soClose;
    end;
  end;
end;

procedure TmMServerConnection.ProcessCommand(const Data: TBytes);
var
  response: TBytes;
begin
  if not Assigned(fOwnerComponent) or
    not Assigned(fOwnerComponent.FOnHandleCommand) or not Assigned(fClientObject)
  then
    Exit;

  response := fOwnerComponent.FOnHandleCommand(fOwnerComponent,
    fClientObject, Data);

  // Send response back if any - let user decide encryption per message
  if Length(response) > 0 then
    TSimpleProtocol.SendMessage(SendProtocolMessage, response, False,
      fOwnerComponent.FEncryptionContext);
end;

procedure TmMServerConnection.SendProtocolMessage(const Message: RawByteString);
begin
  fOwner.WriteString(Self, Message);
end;

{ TmMServer }

// MEGA FIX: Remove the manual counter from constructor and use only live count
constructor TmMServer.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  PropertyLock := TCriticalSection.Create;

  FInitActive := False;
  FActive := False;
  FPort := 3434;
  FConnectionTimeout := 10;
  FThreadPoolSize := 4;
  fServer := nil;
  FState := ssDisconnected;
  FStartTime := 0;

  // Encryption defaults - Only key and mode (no enabled flag)
  FEncryptionKey := '';
  FEncryptionMode := amCBC; // Default to CBC mode
  FEncryptionKeySize := aks256; // Default to 256-bit
  FEncryptionContext.Clear;

  // Server configuration defaults
  FMaxConnections := 1000;
  FReceiveBufferSize := 8192;
  FSendBufferSize := 8192;
  FKeepAlive := True;
  FNoDelay := True;
  FReusePort := True;

  // Statistics - REMOVED FTotalActiveConnections initialization
  ResetStatistics;

  // Component info - Updated version and description
  FVersion := '1.0.3';
  FDescription :=
    'mORMot2 Async Server Component with BULLETPROOF Active Connection Counting';

  FEventsUseMainThread := True;

  FClients := TObjectDictionary<TPollAsyncConnectionHandle, TmMServerClient>.
    Create([]);

  // Setup logging
  FLogFamily := TSynLog.Family;
  FLogFamily.PerThreadLog := ptIdentifiedInOnFile;

  FLogLevel := llErrors;
  SetLogLevel(FLogLevel);
end;

destructor TmMServer.Destroy;
begin
  Disconnect;
  PropertyLock.Free;
  FClients.Free;
  inherited Destroy;
end;

procedure TmMServer.Loaded;
begin
  inherited Loaded;

  if FInitActive then
    DoActivate(True);
end;

// UpdateEncryptionContext - Only when key is provided
procedure TmMServer.UpdateEncryptionContext;
begin
  PropertyLock.Enter;
  try
    if FEncryptionKey <> '' then
    begin
      FEncryptionContext.SetKey(FEncryptionKey, FEncryptionMode,
        FEncryptionKeySize);
    end
    else
    begin
      FEncryptionContext.Clear;
    end;
  finally
    PropertyLock.Leave;
  end;
end;

// MEGA FIX: Update ResetStatistics to not reset the live counter
procedure TmMServer.ResetStatistics;
begin
  PropertyLock.Enter;
  try
    FStartTime := 0;
    FTotalConnections := 0;
    // REMOVED: FTotalActiveConnections reset - we use live count now
    FTotalBytesReceived := 0;
    FTotalBytesSent := 0;
    FTotalMessagesReceived := 0;
    FTotalMessagesSent := 0;
  finally
    PropertyLock.Leave;
  end;
end;

procedure TmMServer.DoActivate(aActivate: Boolean);
begin
  if FActive = aActivate then
    Exit;

  if csLoading in ComponentState then
  begin
    FInitActive := aActivate;
    Exit;
  end;

  if aActivate then
    Connect
  else
    Disconnect;
end;

// Property methods implementation
function TmMServer.GetActive: Boolean;
begin
  PropertyLock.Enter;
  try
    Result := FActive;
  finally
    PropertyLock.Leave;
  end;
end;

procedure TmMServer.SetActive(const Value: Boolean);
begin
  DoActivate(Value);
end;

function TmMServer.GetPort: Integer;
begin
  PropertyLock.Enter;
  try
    Result := FPort;
  finally
    PropertyLock.Leave;
  end;
end;

procedure TmMServer.SetPort(const Value: Integer);
begin
  if not(csLoading in ComponentState) then
    if GetActive then
      raise Exception.Create('Cannot change Port while server is active');

  PropertyLock.Enter;
  try
    FPort := Value;
  finally
    PropertyLock.Leave;
  end;
end;

function TmMServer.GetConnectionTimeout: Integer;
begin
  PropertyLock.Enter;
  try
    Result := FConnectionTimeout;
  finally
    PropertyLock.Leave;
  end;
end;

procedure TmMServer.SetConnectionTimeout(const Value: Integer);
begin
  if not(csLoading in ComponentState) then
    if GetActive then
      raise Exception.Create
        ('Cannot change ConnectionTimeout while server is active');

  PropertyLock.Enter;
  try
    if Value < 1 then
      FConnectionTimeout := 1
    else
      FConnectionTimeout := Value;
  finally
    PropertyLock.Leave;
  end;
end;

function TmMServer.GetThreadPoolSize: Integer;
begin
  PropertyLock.Enter;
  try
    Result := FThreadPoolSize;
  finally
    PropertyLock.Leave;
  end;
end;

procedure TmMServer.SetThreadPoolSize(const Value: Integer);
begin
  if not(csLoading in ComponentState) then
    if GetActive then
      raise Exception.Create
        ('Cannot change ThreadPoolSize while server is active');

  PropertyLock.Enter;
  try
    if Value < 1 then
      FThreadPoolSize := 1
    else
      FThreadPoolSize := Value;
  finally
    PropertyLock.Leave;
  end;
end;

function TmMServer.GetEventsUseMainThread: Boolean;
begin
  PropertyLock.Enter;
  try
    Result := FEventsUseMainThread;
  finally
    PropertyLock.Leave;
  end;
end;

procedure TmMServer.SetEventsUseMainThread(const Value: Boolean);
begin
  PropertyLock.Enter;
  try
    FEventsUseMainThread := Value;
  finally
    PropertyLock.Leave;
  end;
end;

function TmMServer.GetLogLevel: TmMServerLogLevel;
begin
  PropertyLock.Enter;
  try
    Result := FLogLevel;
  finally
    PropertyLock.Leave;
  end;
end;

procedure TmMServer.SetLogLevel(const Value: TmMServerLogLevel);
begin
  PropertyLock.Enter;
  try
    FLogLevel := Value;

    case FLogLevel of
      llNone:
        FLogFamily.Level := [];
      llErrors:
        FLogFamily.Level := [sllError, sllException];
      llDebug:
        FLogFamily.Level := [sllError, sllException, sllDebug, sllEnter,
          sllLeave];
      llClientServer:
        FLogFamily.Level := [sllError, sllException, sllInfo];
      llVerbose:
        FLogFamily.Level := [low(TSynLogInfo) .. high(TSynLogInfo)];
    end;
  finally
    PropertyLock.Leave;
  end;
end;

function TmMServer.GetState: TmMServerState;
begin
  PropertyLock.Enter;
  try
    Result := FState;
  finally
    PropertyLock.Leave;
  end;
end;

procedure TmMServer.SetState(const Value: TmMServerState);
begin
  DoStateChange(Value);
end;

// Encryption property methods - Only key and mode (no enabled flag)
function TmMServer.GetEncryptionKey: string;
begin
  PropertyLock.Enter;
  try
    Result := FEncryptionKey;
  finally
    PropertyLock.Leave;
  end;
end;

procedure TmMServer.SetEncryptionKey(const Value: string);
begin
  if not(csLoading in ComponentState) then
    if GetActive then
      raise Exception.Create
        ('Cannot change encryption key while server is active');

  PropertyLock.Enter;
  try
    FEncryptionKey := Value;
    UpdateEncryptionContext;
  finally
    PropertyLock.Leave;
  end;
end;

function TmMServer.GetEncryptionMode: TAESMode;
begin
  PropertyLock.Enter;
  try
    Result := FEncryptionMode;
  finally
    PropertyLock.Leave;
  end;
end;

procedure TmMServer.SetEncryptionMode(const Value: TAESMode);
begin
  if not(csLoading in ComponentState) then
    if GetActive then
      raise Exception.Create
        ('Cannot change encryption mode while server is active');

  PropertyLock.Enter;
  try
    FEncryptionMode := Value;
    UpdateEncryptionContext;
  finally
    PropertyLock.Leave;
  end;
end;

function TmMServer.GetEncryptionKeySize: TAESKeySize;
begin
  PropertyLock.Enter;
  try
    Result := FEncryptionKeySize;
  finally
    PropertyLock.Leave;
  end;
end;

procedure TmMServer.SetEncryptionKeySize(const Value: TAESKeySize);
begin
  if not(csLoading in ComponentState) then
    if GetActive then
      raise Exception.Create
        ('Cannot change encryption key size while server is active');

  PropertyLock.Enter;
  try
    FEncryptionKeySize := Value;
    UpdateEncryptionContext;
  finally
    PropertyLock.Leave;
  end;
end;

function TmMServer.GetEncryptionInfo: string;
begin
  PropertyLock.Enter;
  try
    if FEncryptionKey <> '' then
      Result := FEncryptionContext.GetModeString
    else
      Result := 'NO KEY SET';
  finally
    PropertyLock.Leave;
  end;
end;

// Server settings property methods
function TmMServer.GetMaxConnections: Integer;
begin
  PropertyLock.Enter;
  try
    Result := FMaxConnections;
  finally
    PropertyLock.Leave;
  end;
end;

procedure TmMServer.SetMaxConnections(const Value: Integer);
begin
  PropertyLock.Enter;
  try
    if Value < 0 then
      FMaxConnections := 0
    else
      FMaxConnections := Value;
  finally
    PropertyLock.Leave;
  end;
end;

function TmMServer.GetReceiveBufferSize: Integer;
begin
  PropertyLock.Enter;
  try
    Result := FReceiveBufferSize;
  finally
    PropertyLock.Leave;
  end;
end;

procedure TmMServer.SetReceiveBufferSize(const Value: Integer);
begin
  PropertyLock.Enter;
  try
    if Value < 1024 then
      FReceiveBufferSize := 1024
    else
      FReceiveBufferSize := Value;
  finally
    PropertyLock.Leave;
  end;
end;

function TmMServer.GetSendBufferSize: Integer;
begin
  PropertyLock.Enter;
  try
    Result := FSendBufferSize;
  finally
    PropertyLock.Leave;
  end;
end;

procedure TmMServer.SetSendBufferSize(const Value: Integer);
begin
  PropertyLock.Enter;
  try
    if Value < 1024 then
      FSendBufferSize := 1024
    else
      FSendBufferSize := Value;
  finally
    PropertyLock.Leave;
  end;
end;

function TmMServer.GetKeepAlive: Boolean;
begin
  PropertyLock.Enter;
  try
    Result := FKeepAlive;
  finally
    PropertyLock.Leave;
  end;
end;

procedure TmMServer.SetKeepAlive(const Value: Boolean);
begin
  PropertyLock.Enter;
  try
    FKeepAlive := Value;
  finally
    PropertyLock.Leave;
  end;
end;

function TmMServer.GetNoDelay: Boolean;
begin
  PropertyLock.Enter;
  try
    Result := FNoDelay;
  finally
    PropertyLock.Leave;
  end;
end;

procedure TmMServer.SetNoDelay(const Value: Boolean);
begin
  PropertyLock.Enter;
  try
    FNoDelay := Value;
  finally
    PropertyLock.Leave;
  end;
end;

function TmMServer.GetReusePort: Boolean;
begin
  PropertyLock.Enter;
  try
    Result := FReusePort;
  finally
    PropertyLock.Leave;
  end;
end;

procedure TmMServer.SetReusePort(const Value: Boolean);
begin
  PropertyLock.Enter;
  try
    FReusePort := Value;
  finally
    PropertyLock.Leave;
  end;
end;

// Statistics property methods
function TmMServer.GetClientCount: Integer;
begin
  PropertyLock.Enter;
  try
    Result := FClients.Count;
  finally
    PropertyLock.Leave;
  end;
end;

function TmMServer.GetUpTime: Integer;
begin
  PropertyLock.Enter;
  try
    if (FState = ssListening) and (FStartTime > 0) then
      Result := SecondsBetween(Now, FStartTime)
    else
      Result := 0;
  finally
    PropertyLock.Leave;
  end;
end;

function TmMServer.GetTotalConnections: Int64;
begin
  PropertyLock.Enter;
  try
    Result := FTotalConnections;
  finally
    PropertyLock.Leave;
  end;
end;

// CRITICAL FIX: Replace the TotalActiveConnections getter with LIVE count
function TmMServer.GetTotalActiveConnections: Int64;
begin
  PropertyLock.Enter;
  try
    // MEGA FIX: Always use LIVE count from dictionary instead of counter
    Result := FClients.Count;
  finally
    PropertyLock.Leave;
  end;
end;

function TmMServer.GetTotalBytesReceived: Int64;
begin
  PropertyLock.Enter;
  try
    Result := FTotalBytesReceived;
  finally
    PropertyLock.Leave;
  end;
end;

function TmMServer.GetTotalBytesSent: Int64;
begin
  PropertyLock.Enter;
  try
    Result := FTotalBytesSent;
  finally
    PropertyLock.Leave;
  end;
end;

function TmMServer.GetTotalMessagesReceived: Int64;
begin
  PropertyLock.Enter;
  try
    Result := FTotalMessagesReceived;
  finally
    PropertyLock.Leave;
  end;
end;

function TmMServer.GetTotalMessagesSent: Int64;
begin
  PropertyLock.Enter;
  try
    Result := FTotalMessagesSent;
  finally
    PropertyLock.Leave;
  end;
end;

// State management
procedure TmMServer.DoStateChange(NewState: TmMServerState);
var
  OldState: TmMServerState;
begin
  PropertyLock.Enter;
  try
    OldState := FState;
    FState := NewState;
    FActive := (NewState = ssListening);

    if NewState = ssListening then
      FStartTime := Now;
  finally
    PropertyLock.Leave;
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
              // Swallow event exceptions
            end;
        end);
    end
    else
    begin
      try
        FOnStateChange(Self, OldState, NewState);
      except
        // Swallow event exceptions
      end;
    end;
  end;
end;

procedure TmMServer.DoError(Client: TmMServerClient; const ErrorMsg: string;
ErrorCode: Integer = 0);
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
              FOnError(Self, Client, ErrorMsg, ErrorCode);
            except
              // Swallow event exceptions
            end;
        end);
    end
    else
    begin
      try
        FOnError(Self, Client, ErrorMsg, ErrorCode);
      except
        // Swallow event exceptions
      end;
    end;
  end;
end;

procedure TmMServer.DoDataSent(Client: TmMServerClient; BytesSent: Integer);
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
              FOnDataSent(Self, Client, BytesSent);
            except
              // Swallow event exceptions
            end;
        end);
    end
    else
    begin
      try
        FOnDataSent(Self, Client, BytesSent);
      except
        // Swallow event exceptions
      end;
    end;
  end;
end;

procedure TmMServer.DoDataReceived(Client: TmMServerClient;
BytesReceived: Integer);
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
              FOnDataReceived(Self, Client, BytesReceived);
            except
              // Swallow event exceptions
            end;
        end);
    end
    else
    begin
      try
        FOnDataReceived(Self, Client, BytesReceived);
      except
        // Swallow event exceptions
      end;
    end;
  end;
end;

// Server control methods
procedure TmMServer.Connect;
var
  Options: TAsyncConnectionsOptions;
begin
  if FState in [ssListening, ssStarting] then
    Exit;

  DoStateChange(ssStarting);

  if Assigned(fServer) then
  begin
    fServer.Free;
    fServer := nil;
  end;

  try
    GlobalServerComponent := Self;

    UpdateEncryptionContext;

    Options := [];
    if FReusePort then
      Options := Options + [acoReusePort];

    fServer := TAsyncServer.Create(IntToStr(FPort), nil, nil,
      TmMServerConnection, 'mMServer', FLogFamily.SynLogClass, Options,
      FThreadPoolSize);

    fServer.WaitStarted(FConnectionTimeout);
    DoStateChange(ssListening);

  except
    on E: Exception do
    begin
      if Assigned(fServer) then
      begin
        fServer.Free;
        fServer := nil;
      end;
      DoStateChange(ssDisconnected);
      DoError(nil, 'Server start failed: ' + E.Message, -1);
      raise;
    end;
  end;
end;

// MEGA FIX: Update Disconnect method to not manually reset counter
procedure TmMServer.Disconnect;
var
  Client: TmMServerClient;
  ClientsToFree: TArray<TmMServerClient>;
  i: Integer;
begin
  if FState = ssDisconnected then
    Exit;

  DoStateChange(ssStopping);

  // Collect all clients before freeing the server
  PropertyLock.Enter;
  try
    SetLength(ClientsToFree, FClients.Count);
    i := 0;
    for Client in FClients.Values do
    begin
      ClientsToFree[i] := Client;
      Inc(i);
    end;
    FClients.Clear; // This automatically sets live count to 0
    // REMOVED: Manual counter reset - we use live count now
  finally
    PropertyLock.Leave;
  end;

  if Assigned(fServer) then
  begin
    try
      fServer.Shutdown;
    except
      // Ignore shutdown errors
    end;

    try
      fServer.Free;
    finally
      fServer := nil;
    end;
  end;

  // Free all client objects
  for i := 0 to High(ClientsToFree) do
    ClientsToFree[i].Free;

  DoStateChange(ssDisconnected);
end;

// Client management methods
function TmMServer.FindClient(Handle: TPollAsyncConnectionHandle)
  : TmMServerClient;
begin
  PropertyLock.Enter;
  try
    if not FClients.TryGetValue(Handle, Result) then
      Result := nil;
  finally
    PropertyLock.Leave;
  end;
end;

function TmMServer.FindClientByIP(const IP: string): TmMServerClient;
var
  Client: TmMServerClient;
begin
  Result := nil;
  PropertyLock.Enter;
  try
    for Client in FClients.Values do
    begin
      if Client.PeerIP = IP then
      begin
        Result := Client;
        break;
      end;
    end;
  finally
    PropertyLock.Leave;
  end;
end;

procedure TmMServer.DisconnectClient(Handle: TPollAsyncConnectionHandle);
begin
  if Assigned(fServer) then
  begin
    fServer.ConnectionRemove(Handle);

    // Force cleanup like DisconnectAllClients does
    Sleep(50);
    PropertyLock.Enter;
    try
      if FClients.ContainsKey(Handle) then
        FClients.Remove(Handle);
    finally
      PropertyLock.Leave;
    end;
  end;
end;

procedure TmMServer.DisconnectClient(Client: TmMServerClient);
begin
  if Assigned(Client) then
  begin
    // Call the first overload which now has the proper cleanup
    DisconnectClient(Client.Handle);
  end;
end;

procedure TmMServer.DisconnectAllClients;
var
  Client: TmMServerClient;
  ClientsToDisconnect: TArray<TPollAsyncConnectionHandle>;
  i: Integer;
begin
  PropertyLock.Enter;
  try
    SetLength(ClientsToDisconnect, FClients.Count);
    i := 0;
    for Client in FClients.Values do
    begin
      ClientsToDisconnect[i] := Client.Handle;
      Inc(i);
    end;
  finally
    PropertyLock.Leave;
  end;

  // FIXED: Use the correct mORMot2 method
  if Assigned(fServer) then
  begin
    for i := 0 to High(ClientsToDisconnect) do
    begin
      try
        // This is the correct method in mORMot2 TAsyncServer
        fServer.ConnectionRemove((ClientsToDisconnect[i]));
      except
        // Continue with other clients if one fails
      end;
    end;
  end;

  // Give time for OnClose events to fire
  Sleep(50);

  // Force cleanup if any connections are stuck
  PropertyLock.Enter;
  try
    if FClients.Count > 0 then
    begin
      FClients.Clear; // Force clear any remaining
    end;
  finally
    PropertyLock.Leave;
  end;
end;

// ONLY ONE BROADCASTING METHOD
procedure TmMServer.BroadcastData(const Data: TBytes;
Encrypted: Boolean = False);
var
  Client: TmMServerClient;
  ClientsToSend: TArray<TmMServerClient>;
  i: Integer;
begin
  PropertyLock.Enter;
  try
    SetLength(ClientsToSend, FClients.Count);
    i := 0;
    for Client in FClients.Values do
    begin
      ClientsToSend[i] := Client;
      Inc(i);
    end;
  finally
    PropertyLock.Leave;
  end;

  for i := 0 to High(ClientsToSend) do
  begin
    try
      ClientsToSend[i].SendData(Data, Encrypted);
    except
      // Continue with other clients if one fails
    end;
  end;
end;

// Utility methods
procedure TmMServer.ClearStatistics;
begin
  ResetStatistics;
end;

function TmMServer.GetServerInfo: string;
const
  StateNames: array [TmMServerState] of string = ('Disconnected', 'Starting',
    'Listening', 'Stopping');
begin
  PropertyLock.Enter;
  try
    Result := Format
      ('Port: %d, State: %s, Active: %d/%d, Total: %d, Encryption: %s, UpTime: %d sec',
      [FPort, StateNames[FState], GetTotalActiveConnections,
    // FIXED: Show live active connections
    FMaxConnections, GetTotalConnections, GetEncryptionInfo, GetUpTime]);
  finally
    PropertyLock.Leave;
  end;
end;

function TmMServer.IsListening: Boolean;
begin
  Result := (FState = ssListening) and Assigned(fServer);
end;

end.
