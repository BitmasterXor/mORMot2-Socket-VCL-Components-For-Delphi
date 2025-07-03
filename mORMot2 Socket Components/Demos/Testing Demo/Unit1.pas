unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Grids, Vcl.Buttons,
  System.DateUtils, System.Generics.Collections, System.SyncObjs,
  mMClient, mMServer, mMProtocol;

type
  TForm1 = class(TForm)
    // Main control panels
    Panel1: TPanel;
    Panel3: TPanel;

    // Server controls
    GroupBox1: TGroupBox;
    Button1: TButton;         // Start Server
    Button2: TButton;         // Stop Server
    btnDisconnectAll: TButton;// Disconnect All Clients
    btnBroadcastUnenc: TButton;// Broadcast Unencrypted
    btnBroadcastEnc: TButton; // Broadcast Encrypted
    btnKickClient: TButton;   // Kick Selected Client

    // Client controls
    GroupBox2: TGroupBox;
    Button3: TButton;         // Connect Client
    Button4: TButton;         // Disconnect Client
    Button5: TButton;         // Send Unencrypted
    Button6: TButton;         // Send Encrypted
    btnReconnect: TButton;    // Manual Reconnect
    btnClearStats: TButton;   // Clear Statistics

    // Advanced controls
    GroupBox3: TGroupBox;
    btnStressTest: TButton;   // Stress Test
    btnLargeData: TButton;    // Send Large Data
    btnEncryptionTest: TButton;// Encryption Test
    btnAutoReconnect: TButton; // Toggle Auto Reconnect
    btnChangeKey: TButton;    // Change Encryption Key

    // Configuration controls
    GroupBox4: TGroupBox;
    EditHost: TEdit;
    EditPort: TEdit;
    EditMaxConn: TEdit;
    EditBufferSize: TEdit;
    ComboLogLevel: TComboBox;
    CheckKeepAlive: TCheckBox;
    CheckNoDelay: TCheckBox;

    // Main display
    Memo1: TMemo;

    // Status and statistics
    StatusBar1: TStatusBar;

    // Client list
    GroupBox5: TGroupBox;
    StringGrid1: TStringGrid;

    // Live statistics panel
    GroupBox6: TGroupBox;
    Label1: TLabel;  // Server Stats
    Label2: TLabel;  // Client Stats
    Label3: TLabel;  // Connection Info
    Label4: TLabel;  // Throughput

    // Timer for live updates
    Timer1: TTimer;

    // Components
    mMServer1: TmMServer;
    mMClient1: TmMClient;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);

    // Button events
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure btnDisconnectAllClick(Sender: TObject);
    procedure btnBroadcastUnencClick(Sender: TObject);
    procedure btnBroadcastEncClick(Sender: TObject);
    procedure btnKickClientClick(Sender: TObject);
    procedure btnReconnectClick(Sender: TObject);
    procedure btnClearStatsClick(Sender: TObject);
    procedure btnStressTestClick(Sender: TObject);
    procedure btnLargeDataClick(Sender: TObject);
    procedure btnEncryptionTestClick(Sender: TObject);
    procedure btnAutoReconnectClick(Sender: TObject);
    procedure btnChangeKeyClick(Sender: TObject);

    // Configuration events
    procedure EditHostChange(Sender: TObject);
    procedure EditPortChange(Sender: TObject);
    procedure EditMaxConnChange(Sender: TObject);
    procedure EditBufferSizeChange(Sender: TObject);
    procedure ComboLogLevelChange(Sender: TObject);
    procedure CheckKeepAliveClick(Sender: TObject);
    procedure CheckNoDelayClick(Sender: TObject);
    procedure StringGrid1Click(Sender: TObject);

    // Server Events - ALL OF THEM
    procedure mMServer1Connect(Sender: TObject; Client: TmMServerClient);
    procedure mMServer1Disconnect(Sender: TObject; Client: TmMServerClient);
    procedure mMServer1StateChange(Sender: TObject; OldState, NewState: TmMServerState);
    procedure mMServer1DataReceived(Sender: TObject; Client: TmMServerClient; BytesReceived: Integer);
    procedure mMServer1DataSent(Sender: TObject; Client: TmMServerClient; BytesSent: Integer);
    procedure mMServer1Error(Sender: TObject; Client: TmMServerClient; const Error: string; ErrorCode: Integer);
    function mMServer1HandleCommand(Sender: TObject; Client: TmMServerClient; aData: TBytes): TBytes;

    // Client Events - ALL OF THEM
    procedure mMClient1Connect(Sender: TObject);
    procedure mMClient1Disconnect(Sender: TObject);
    procedure mMClient1StateChange(Sender: TObject; OldState, NewState: TmMClientState);
    procedure mMClient1DataReceived(Sender: TObject; BytesReceived: Integer);
    procedure mMClient1DataSent(Sender: TObject; BytesSent: Integer);
    procedure mMClient1Error(Sender: TObject; const Error: string; ErrorCode: Integer);
    function mMClient1HandleCommand(Sender: TObject; aData: TBytes): TBytes;

  private
    FLastServerBytes: Int64;
    FLastClientBytes: Int64;
    FLastUpdate: TDateTime;
    FStressTestRunning: Boolean;
    FMessageCounter: Integer;

    // UI freeze prevention
    FStressTestMessages: Int64;
    FLastStressLogTime: TDateTime;
    FLogLock: TCriticalSection;

    procedure LogMessage(const Msg: string; const Category: string = 'INFO');
    procedure LogMessageSafe(const Msg: string; const Category: string = 'INFO'); // NEW: Thread-safe logging
    procedure UpdateServerStatus;
    procedure UpdateClientList(ForceUpdate: Boolean = False);
    procedure UpdateStatistics;
    procedure UpdateConfiguration;
    procedure ApplyConfiguration;
    procedure StartStressTest;
    procedure StopStressTest;
    procedure ShowThroughput;
    procedure SetupUI;
    function FormatByteSize(Bytes: Int64): string;
    function GetServerStateName(State: TmMServerState): string;
    function GetClientStateName(State: TmMClientState): string;
    function BoolToText(Value: Boolean; TrueText, FalseText: string): string;

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetupUI;

  // MEGA FIX: Initialize UI freeze prevention
  FLogLock := TCriticalSection.Create;
  FStressTestMessages := 0;
  FLastStressLogTime := Now;

  // Setup default encryption keys
  mMServer1.EncryptionKey := 'MySecretKey123456789012345678901234567890';
  mMClient1.EncryptionKey := 'MySecretKey123456789012345678901234567890';

  // Configure default settings
  EditHost.Text := '127.0.0.1';
  EditPort.Text := '3434';
  EditMaxConn.Text := '1000';
  EditBufferSize.Text := '8192';
  ComboLogLevel.ItemIndex := 1; // Errors
  CheckKeepAlive.Checked := True;
  CheckNoDelay.Checked := True;

  // Initialize stats
  FLastServerBytes := 0;
  FLastClientBytes := 0;
  FLastUpdate := Now;
  FStressTestRunning := False;
  FMessageCounter := 0;

  // Start live updates
  Timer1.Interval := 1000; // 1 second
  Timer1.Enabled := True;

  LogMessage('🚀 ULTIMATE mORMot2 Socket Demo Started!', 'SYSTEM');
  LogMessage('💪 ALL FEATURES ENABLED - Server & Client Ready!', 'SYSTEM');
  LogMessage('🔐 AES-256-CBC Encryption Configured', 'CRYPTO');

  UpdateConfiguration;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Stop stress test first
  if FStressTestRunning then
    StopStressTest;

  // Stop timer
  if Assigned(Timer1) then
    Timer1.Enabled := False;

  // MEGA FIX: Cleanup thread safety
  if Assigned(FLogLock) then
  begin
    FLogLock.Free;
    FLogLock := nil;
  end;

  // Properly disconnect client first
  if Assigned(mMClient1) then
  begin
    try
      if mMClient1.Active then
      begin
        mMClient1.AutoReconnect := False; // Disable auto-reconnect
        mMClient1.Active := False;
      end;
    except
      // Ignore disconnect errors during shutdown
    end;
  end;

  // Small delay to allow clean disconnect (probably really not even needed but just in case BOYS!)
  Sleep(100);

  // Properly stop server
  if Assigned(mMServer1) then
  begin
    try
      if mMServer1.Active then
        mMServer1.Active := False;
    except
      // Ignore shutdown errors
    end;
  end;

  // Another small delay for cleanup
  Sleep(100);
end;

procedure TForm1.SetupUI;
begin
  // Setup status bar with multiple panels
  StatusBar1.Panels.Clear;
  StatusBar1.Panels.Add; // Server status
  StatusBar1.Panels.Add; // Client status
  StatusBar1.Panels.Add; // Connection count
  StatusBar1.Panels.Add; // Throughput
  StatusBar1.Panels.Add; // Encryption status

  StatusBar1.Panels[0].Text := '🖥️ Server: Stopped';
  StatusBar1.Panels[0].Width := 150;
  StatusBar1.Panels[1].Text := '💻 Client: Disconnected';
  StatusBar1.Panels[1].Width := 180;
  StatusBar1.Panels[2].Text := '👥 Connections: 0';
  StatusBar1.Panels[2].Width := 120;
  StatusBar1.Panels[3].Text := '📊 Throughput: 0 KB/s';
  StatusBar1.Panels[3].Width := 150;
  StatusBar1.Panels[4].Text := '🔐 Encryption: Ready';
  StatusBar1.Panels[4].Width := 150;

  // Setup client list grid
  StringGrid1.ColCount := 5;
  StringGrid1.RowCount := 1;
  StringGrid1.Cells[0, 0] := 'Handle';
  StringGrid1.Cells[1, 0] := 'IP Address';
  StringGrid1.Cells[2, 0] := 'Port';
  StringGrid1.Cells[3, 0] := 'Connected';
  StringGrid1.Cells[4, 0] := 'Data (RX/TX)';
  StringGrid1.ColWidths[0] := 60;
  StringGrid1.ColWidths[1] := 100;
  StringGrid1.ColWidths[2] := 60;
  StringGrid1.ColWidths[3] := 80;
  StringGrid1.ColWidths[4] := 120;

  // Setup combo box items
  ComboLogLevel.Items.Clear;
  ComboLogLevel.Items.Add('None');
  ComboLogLevel.Items.Add('Errors');
  ComboLogLevel.Items.Add('Debug');
  ComboLogLevel.Items.Add('Client/Server');
  ComboLogLevel.Items.Add('Verbose');

  // Set form properties
  Caption := 'ULTIMATE mORMot2 Socket Demo - ALL FEATURES ENABLED!';

  // Configure memo for better display
  Memo1.Font.Name := 'Consolas';
  Memo1.Font.Size := 9;
  Memo1.ScrollBars := ssVertical;
  Memo1.WordWrap := False;
end;

// MEGA FIX: Thread-safe logging that prevents UI freeze
procedure TForm1.LogMessageSafe(const Msg: string; const Category: string = 'INFO');
begin
  // During stress test, only log every 1000 messages or every 5 seconds
  if FStressTestRunning then
  begin
    FLogLock.Enter;
    try
      Inc(FStressTestMessages);

      // Only log stress test updates periodically
      if (FStressTestMessages mod 1000 = 0) or
         ((Now - FLastStressLogTime) * 24 * 60 * 60 > 5) then
      begin
        TThread.Queue(nil,
          procedure
          begin
            LogMessage(Format('🔥 Stress Test: %d messages processed', [FStressTestMessages]), 'STATS');
          end);
        FLastStressLogTime := Now;
      end;
    finally
      FLogLock.Leave;
    end;
  end
  else
  begin
    // Normal logging for non-stress test messages
    TThread.Queue(nil,
      procedure
      begin
        LogMessage(Msg, Category);
      end);
  end;
end;

procedure TForm1.LogMessage(const Msg: string; const Category: string = 'INFO');
var
  TimeStamp: string;
  CategoryIcon: string;
begin
  TimeStamp := FormatDateTime('hh:nn:ss.zzz', Now);

  // Use if-else instead of case for strings
  if Category = 'SYSTEM' then
    CategoryIcon := '🚀'
  else if Category = 'SERVER' then
    CategoryIcon := '🖥️'
  else if Category = 'CLIENT' then
    CategoryIcon := '💻'
  else if Category = 'CRYPTO' then
    CategoryIcon := '🔐'
  else if Category = 'DATA' then
    CategoryIcon := '📦'
  else if Category = 'ERROR' then
    CategoryIcon := '❌'
  else if Category = 'SUCCESS' then
    CategoryIcon := '✅'
  else if Category = 'WARNING' then
    CategoryIcon := '⚠️'
  else if Category = 'STATS' then
    CategoryIcon := '📊'
  else
    CategoryIcon := 'ℹ️';

  Memo1.Lines.Add(Format('%s %s [%s] %s', [TimeStamp, CategoryIcon, Category, Msg]));

  // MEGA FIX: Limit memo lines during stress test to prevent UI freeze
  if FStressTestRunning and (Memo1.Lines.Count > 1000) then
  begin
    // Remove first 500 lines to keep it manageable
    while Memo1.Lines.Count > 500 do
      Memo1.Lines.Delete(0);
  end;

  // Auto scroll to bottom (only if not stress testing to avoid UI flood)
  if not FStressTestRunning then
  begin
    SendMessage(Memo1.Handle, WM_VSCROLL, SB_BOTTOM, 0);
    Application.ProcessMessages;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  UpdateServerStatus;
  UpdateClientList(False);  // <-- Normal update (respects stress test flag)
  UpdateStatistics;
  ShowThroughput;
end;

procedure TForm1.UpdateServerStatus;
const
  ServerStateNames: array[TmMServerState] of string = ('Stopped', 'Starting', 'Running', 'Stopping');
  ClientStateNames: array[TmMClientState] of string = ('Disconnected', 'Connecting', 'Connected', 'Disconnecting');
begin
  // Update status bar
  StatusBar1.Panels[0].Text := Format('Server: %s', [ServerStateNames[mMServer1.State]]);
  StatusBar1.Panels[1].Text := Format('Client: %s', [ClientStateNames[mMClient1.State]]);
  StatusBar1.Panels[2].Text := Format('Active: %d | Total: %d', [mMServer1.TotalActiveConnections, mMServer1.TotalConnections]);

  // Update encryption status
  if (mMServer1.EncryptionKey <> '') and (mMClient1.EncryptionKey <> '') then
    StatusBar1.Panels[4].Text := 'Encryption: Active'
  else
    StatusBar1.Panels[4].Text := 'Encryption: Disabled';

  // Update button states
  Button1.Enabled := not mMServer1.Active;
  Button2.Enabled := mMServer1.Active;
  btnDisconnectAll.Enabled := mMServer1.Active and (mMServer1.ClientCount > 0);
  btnBroadcastUnenc.Enabled := mMServer1.Active and (mMServer1.ClientCount > 0);
  btnBroadcastEnc.Enabled := mMServer1.Active and (mMServer1.ClientCount > 0);
  btnKickClient.Enabled := mMServer1.Active and (StringGrid1.Row > 0);

  Button3.Enabled := not mMClient1.Active;
  Button4.Enabled := mMClient1.Active;
  Button5.Enabled := mMClient1.IsConnected;
  Button6.Enabled := mMClient1.IsConnected;
  btnReconnect.Enabled := not mMClient1.Active;

  // Update auto-reconnect button - FIXED to show actual state
  if mMClient1.AutoReconnect then
    btnAutoReconnect.Caption := 'Auto-Reconnect: ON'
  else
    btnAutoReconnect.Caption := 'Auto-Reconnect: OFF';
end;

procedure TForm1.UpdateClientList(ForceUpdate: Boolean = False);
var
  Client: TmMServerClient;
  Row: Integer;
begin
  // FIXED: Allow forced updates even during stress test
  if FStressTestRunning and not ForceUpdate then
    Exit;

  // Clear existing rows (keep header)
  StringGrid1.RowCount := 1;
  StringGrid1.Row := 0; // Reset selection to header

  if mMServer1.ClientCount > 0 then
  begin
    Row := 1;
    for Client in mMServer1.Clients.Values do
    begin
      StringGrid1.RowCount := Row + 1;
      StringGrid1.Cells[0, Row] := IntToStr(Client.Handle);
      StringGrid1.Cells[1, Row] := Client.PeerIP;
      StringGrid1.Cells[2, Row] := IntToStr(Client.PeerPort);
      StringGrid1.Cells[3, Row] := FormatDateTime('hh:nn:ss', Client.ConnectedAt);
      StringGrid1.Cells[4, Row] := Format('%d/%d', [Client.BytesReceived, Client.BytesSent]);
      Inc(Row);
    end;
  end;

  // Update kick button state
  btnKickClient.Enabled := mMServer1.Active and (StringGrid1.Row > 0) and (mMServer1.ClientCount > 0);
end;

procedure TForm1.UpdateStatistics;
begin
  // Server statistics
  Label1.Caption := Format(
    'SERVER STATS:'#13#10 +
    '📊 State: %s'#13#10 +
    '👥 Active Connections: %d'#13#10 +
    '📈 Total Connections: %d'#13#10 +
    '📥 Bytes Received: %s'#13#10 +
    '📤 Bytes Sent: %s'#13#10 +
    '📬 Messages RX: %d'#13#10 +
    '📮 Messages TX: %d'#13#10 +
    '⏱️ Uptime: %d seconds',
    [GetServerStateName(mMServer1.State),
     mMServer1.TotalActiveConnections,
     mMServer1.TotalConnections,
     FormatByteSize(mMServer1.TotalBytesReceived),
     FormatByteSize(mMServer1.TotalBytesSent),
     mMServer1.TotalMessagesReceived,
     mMServer1.TotalMessagesSent,
     mMServer1.UpTime]);

  // Client statistics
  Label2.Caption := Format(
    'CLIENT STATS:'#13#10 +
    '📊 State: %s'#13#10 +
    '🌐 Connected to: %s'#13#10 +
    '📥 Bytes Received: %s'#13#10 +
    '📤 Bytes Sent: %s'#13#10 +
    '📬 Messages RX: %d'#13#10 +
    '📮 Messages TX: %d'#13#10 +
    '⏱️ Connected: %d seconds'#13#10 +
    '🔄 Reconnect Attempts: %d',
    [GetClientStateName(mMClient1.State),
     mMClient1.RemoteIP,
     FormatByteSize(mMClient1.TotalBytesReceived),
     FormatByteSize(mMClient1.TotalBytesSent),
     mMClient1.MessagesReceived,
     mMClient1.MessagesSent,
     mMClient1.ConnectedTime,
     mMClient1.ReconnectAttempts]);

  // Connection info
  Label3.Caption := Format(
    'CONNECTION INFO:'#13#10 +
    '🖥️ Server: %s'#13#10 +
    '🔐 Encryption: %s'#13#10 +
    '📦 Buffer Size: %d bytes'#13#10 +
    '🔄 Keep Alive: %s'#13#10 +
    '⚡ No Delay: %s'#13#10 +
    '📊 Log Level: %s',
    [mMServer1.GetServerInfo,
     mMServer1.EncryptionInfo,
     mMServer1.ReceiveBufferSize,
     BoolToStr(mMServer1.KeepAlive, True),
     BoolToStr(mMServer1.NoDelay, True),
     ComboLogLevel.Text]);
end;

procedure TForm1.ShowThroughput;
var
  CurrentTime: TDateTime;
  TimeDiff: Double;
  ServerBytesDiff, ClientBytesDiff: Int64;
  ServerThroughput, ClientThroughput: Double;
begin
  CurrentTime := Now;
  TimeDiff := (CurrentTime - FLastUpdate) * 24 * 60 * 60; // Convert to seconds

  if TimeDiff >= 1.0 then // Update every second
  begin
    ServerBytesDiff := (mMServer1.TotalBytesReceived + mMServer1.TotalBytesSent) - FLastServerBytes;
    ClientBytesDiff := (mMClient1.TotalBytesReceived + mMClient1.TotalBytesSent) - FLastClientBytes;

    ServerThroughput := ServerBytesDiff / TimeDiff / 1024; // KB/s
    ClientThroughput := ClientBytesDiff / TimeDiff / 1024; // KB/s

    StatusBar1.Panels[3].Text := Format('📊 S:%.1f C:%.1f KB/s', [ServerThroughput, ClientThroughput]);

    Label4.Caption := Format(
      'THROUGHPUT:'#13#10 +
      '📊 Server: %.2f KB/s'#13#10 +
      '📊 Client: %.2f KB/s'#13#10 +
      '📈 Combined: %.2f KB/s'#13#10 +
      '📦 Messages/sec: %d'#13#10 +
      '⚡ Stress Test: %s'#13#10 +
      '🔥 Stress Messages: %d',
      [ServerThroughput,
       ClientThroughput,
       ServerThroughput + ClientThroughput,
       Round(FMessageCounter / TimeDiff),
       BoolToText(FStressTestRunning, 'RUNNING', 'STOPPED'),
       FStressTestMessages]);

    FLastServerBytes := mMServer1.TotalBytesReceived + mMServer1.TotalBytesSent;
    FLastClientBytes := mMClient1.TotalBytesReceived + mMClient1.TotalBytesSent;
    FLastUpdate := CurrentTime;
    FMessageCounter := 0;
  end;
end;

function TForm1.FormatByteSize(Bytes: Int64): string;
begin
  if Bytes < 1024 then
    Result := Format('%d B', [Bytes])
  else if Bytes < 1024 * 1024 then
    Result := Format('%.1f KB', [Bytes / 1024])
  else if Bytes < 1024 * 1024 * 1024 then
    Result := Format('%.1f MB', [Bytes / (1024 * 1024)])
  else
    Result := Format('%.1f GB', [Bytes / (1024 * 1024 * 1024)]);
end;

function TForm1.GetServerStateName(State: TmMServerState): string;
begin
  case State of
    ssDisconnected: Result := 'Disconnected';
    ssStarting: Result := 'Starting';
    ssListening: Result := 'Listening';
    ssStopping: Result := 'Stopping';
  else
    Result := 'Unknown';
  end;
end;

function TForm1.GetClientStateName(State: TmMClientState): string;
begin
  case State of
    csDisconnected: Result := 'Disconnected';
    csConnecting: Result := 'Connecting';
    csConnected: Result := 'Connected';
    csDisconnecting: Result := 'Disconnecting';
  else
    Result := 'Unknown';
  end;
end;

function TForm1.BoolToText(Value: Boolean; TrueText, FalseText: string): string;
begin
  if Value then
    Result := TrueText
  else
    Result := FalseText;
end;

procedure TForm1.UpdateConfiguration;
begin
  EditHost.Text := mMClient1.Host;
  EditPort.Text := IntToStr(mMServer1.Port);
  EditMaxConn.Text := IntToStr(mMServer1.MaxConnections);
  EditBufferSize.Text := IntToStr(mMServer1.ReceiveBufferSize);
  ComboLogLevel.ItemIndex := Ord(mMServer1.LogLevel);
  CheckKeepAlive.Checked := mMServer1.KeepAlive;
  CheckNoDelay.Checked := mMServer1.NoDelay;
end;

procedure TForm1.ApplyConfiguration;
begin
  if not mMServer1.Active then
  begin
    mMServer1.Port := StrToIntDef(EditPort.Text, 3434);
    mMServer1.MaxConnections := StrToIntDef(EditMaxConn.Text, 1000);
    mMServer1.ReceiveBufferSize := StrToIntDef(EditBufferSize.Text, 8192);
    mMServer1.SendBufferSize := mMServer1.ReceiveBufferSize;
    mMServer1.LogLevel := TmMServerLogLevel(ComboLogLevel.ItemIndex);
    mMServer1.KeepAlive := CheckKeepAlive.Checked;
    mMServer1.NoDelay := CheckNoDelay.Checked;
  end;

  if not mMClient1.Active then
  begin
    mMClient1.Host := EditHost.Text;
    mMClient1.Port := StrToIntDef(EditPort.Text, 3434);
    mMClient1.ReceiveBufferSize := StrToIntDef(EditBufferSize.Text, 8192);
    mMClient1.SendBufferSize := mMClient1.ReceiveBufferSize;
    mMClient1.LogLevel := TmMClientLogLevel(ComboLogLevel.ItemIndex);
    mMClient1.KeepAlive := CheckKeepAlive.Checked;
    mMClient1.NoDelay := CheckNoDelay.Checked;
  end;
end;

// =============================================================================
// BUTTON CLICK HANDLERS - ALL FEATURES
// =============================================================================

procedure TForm1.Button1Click(Sender: TObject);
begin
  ApplyConfiguration;
  mMServer1.Active := True;
  LogMessage('🖥️ Server starting on port ' + IntToStr(mMServer1.Port) + '...', 'SERVER');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  // MEGA FIX: Disconnect all clients FIRST before stopping server
  if mMServer1.Active and (mMServer1.ClientCount > 0) then
  begin
    LogMessage('🖥️ Disconnecting all clients before stopping server...', 'SERVER');
    mMServer1.DisconnectAllClients;

    // Give time for clients to disconnect cleanly
    Sleep(500);
    Application.ProcessMessages;
  end;

  mMServer1.Active := False;
  LogMessage('🖥️ Server stopped', 'SERVER');
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  ApplyConfiguration;
  mMClient1.Active := True;
  LogMessage('💻 Client connecting to ' + mMClient1.Host + ':' + IntToStr(mMClient1.Port) + '...', 'CLIENT');
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  mMClient1.Active := False;
  LogMessage('💻 Client disconnected', 'CLIENT');
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  TestData: TBytes;
begin
  TestData := bytesof('🚀 UNENCRYPTED test message from client! Time: ' + DateTimeToStr(Now));
  mMClient1.SendData(TestData, False);
  LogMessage('📤 Sent UNENCRYPTED data (' + IntToStr(Length(TestData)) + ' bytes)', 'CLIENT');
  Inc(FMessageCounter);
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  TestData: TBytes;
begin
  TestData := bytesof('🔐 ENCRYPTED test message from client! Time: ' + DateTimeToStr(Now));
  mMClient1.SendData(TestData, True);
  LogMessage('📤 Sent ENCRYPTED data (' + IntToStr(Length(TestData)) + ' bytes)', 'CLIENT');
  Inc(FMessageCounter);
end;

procedure TForm1.btnEncryptionTestClick(Sender: TObject);
var
  TestData: TBytes;
  i: Integer;
begin
  LogMessage('🔐 ENCRYPTION TEST - Sending both encrypted and unencrypted data...', 'CRYPTO');

  for i := 1 to 5 do
  begin
    // Send unencrypted
    TestData := bytesof(Format('🔓 Unencrypted test #%d - %s', [i, DateTimeToStr(Now)]));
    if mMClient1.IsConnected then
    begin
      mMClient1.SendData(TestData, False);
      Inc(FMessageCounter);
    end;

    Sleep(100);

    // Send encrypted
    TestData := bytesof(Format('🔐 Encrypted test #%d - %s', [i, DateTimeToStr(Now)]));
    if mMClient1.IsConnected then
    begin
      mMClient1.SendData(TestData, True);
      Inc(FMessageCounter);
    end;

    Sleep(100);
  end;

  LogMessage('🔐 ENCRYPTION TEST completed - 10 messages sent (5 encrypted, 5 unencrypted)', 'CRYPTO');
end;

procedure TForm1.btnAutoReconnectClick(Sender: TObject);
begin
  if NOT mMClient1.AutoReconnect then
  begin
    mMClient1.ReconnectStrategy := rsBackoff;
    mMClient1.ReconnectMaxAttempts := 10;
    mMClient1.ReconnectInterval := 2000;
    mMClient1.AutoReconnect := True;
    LogMessage('🔄 Auto-reconnect ENABLED (Backoff strategy, max 10 attempts)', 'CLIENT');
    LogMessage('⚠️ WARNING: Client will auto-reconnect if kicked from server!', 'WARNING');
  end
  else
  begin
    mMClient1.AutoReconnect := False;
    LogMessage('🔄 Auto-reconnect DISABLED', 'CLIENT');
    LogMessage('✅ Client will stay disconnected when kicked from server', 'SUCCESS');
  end;
end;

procedure TForm1.btnChangeKeyClick(Sender: TObject);
var
  NewKey: string;
begin
  NewKey := InputBox('Change Encryption Key', 'Enter new encryption key (min 32 chars):', '');
  if Length(NewKey) >= 32 then
  begin
    if not mMServer1.Active and not mMClient1.Active then
    begin
      mMServer1.EncryptionKey := NewKey;
      mMClient1.EncryptionKey := NewKey;
      LogMessage('🔐 Encryption key changed successfully', 'CRYPTO');
    end
    else
    begin
      LogMessage('⚠️ Cannot change key while connections are active!', 'WARNING');
    end;
  end
  else
  begin
    LogMessage('❌ Key must be at least 32 characters long!', 'ERROR');
  end;
end;

procedure TForm1.btnDisconnectAllClick(Sender: TObject);
begin
  LogMessage(Format('🔍 DEBUG: Before disconnect - Client count: %d, Grid rows: %d', [mMServer1.ClientCount, StringGrid1.RowCount]), 'DEBUG');

  // Force clear grid first
  StringGrid1.RowCount := 1;
  StringGrid1.Row := 0;
  LogMessage('🔍 DEBUG: Force cleared grid', 'DEBUG');

  // Disconnect all
  mMServer1.DisconnectAllClients;
  LogMessage('🖥️ Disconnected all clients', 'SERVER');

  LogMessage(Format('🔍 DEBUG: After disconnect - Client count: %d', [mMServer1.ClientCount]), 'DEBUG');

  // Force update
  UpdateClientList;
  UpdateServerStatus;
  Application.ProcessMessages;

  LogMessage(Format('🔍 DEBUG: After update - Client count: %d, Grid rows: %d', [mMServer1.ClientCount, StringGrid1.RowCount]), 'DEBUG');

  // Check what's in the grid
  if StringGrid1.RowCount > 1 then
  begin
    LogMessage(Format('🔍 DEBUG: Grid still has data - Row 1: Handle=%s, IP=%s', [StringGrid1.Cells[0,1], StringGrid1.Cells[1,1]]), 'DEBUG');
  end;
end;

procedure TForm1.btnBroadcastUnencClick(Sender: TObject);
var
  BroadcastData: TBytes;
begin
  BroadcastData := bytesof('📢 BROADCAST UNENCRYPTED from server! Time: ' + DateTimeToStr(Now));
  mMServer1.BroadcastData(BroadcastData, False);
  LogMessage('📢 Broadcast UNENCRYPTED to all clients (' + IntToStr(Length(BroadcastData)) + ' bytes)', 'SERVER');
  Inc(FMessageCounter, mMServer1.ClientCount);
end;

procedure TForm1.btnBroadcastEncClick(Sender: TObject);
var
  BroadcastData: TBytes;
begin
  BroadcastData := bytesof('🔐 BROADCAST ENCRYPTED from server! Time: ' + DateTimeToStr(Now));
  mMServer1.BroadcastData(BroadcastData, True);
  LogMessage('📢 Broadcast ENCRYPTED to all clients (' + IntToStr(Length(BroadcastData)) + ' bytes)', 'SERVER');
  Inc(FMessageCounter, mMServer1.ClientCount);
end;

procedure TForm1.btnKickClientClick(Sender: TObject);
var
  Handle: Integer;
  ClientIP: string;
  Client: TmMServerClient;  // Add this
begin
  if StringGrid1.Row > 0 then
  begin
    Handle := StrToIntDef(StringGrid1.Cells[0, StringGrid1.Row], 0);
    ClientIP := StringGrid1.Cells[1, StringGrid1.Row];

    if Handle > 0 then
    begin
      Client := mMServer1.FindClient(Handle);  // Find the client object
      if Assigned(Client) then
      begin
        mMServer1.DisconnectClient(Client);  // Use the CLIENT overload instead of HANDLE
        // ... rest of your code
      end;
    end;
  end;
end;

procedure TForm1.btnReconnectClick(Sender: TObject);
begin
  mMClient1.Reconnect;
  LogMessage('🔄 Client manual reconnect initiated', 'CLIENT');
end;

procedure TForm1.btnClearStatsClick(Sender: TObject);
begin
  mMServer1.ClearStatistics;
  mMClient1.ClearStatistics;
  FLastServerBytes := 0;
  FLastClientBytes := 0;
  FStressTestMessages := 0; // MEGA FIX: Reset stress test counter
  LogMessage('📊 Statistics cleared', 'STATS');
end;

procedure TForm1.btnStressTestClick(Sender: TObject);
begin
  if FStressTestRunning then
    StopStressTest
  else
    StartStressTest;
end;

// MEGA FIX: Non-blocking stress test that won't freeze UI
procedure TForm1.StartStressTest;
begin
  FStressTestRunning := True;
  FStressTestMessages := 0;
  FLastStressLogTime := Now;
  btnStressTest.Caption := 'Stop Stress Test';
  LogMessage('⚡ STRESS TEST STARTED - High frequency message sending!', 'SYSTEM');
  LogMessage('💡 UI logging throttled to prevent freeze', 'SYSTEM');

  // MEGA FIX: Use background thread that doesn't block UI
  TThread.CreateAnonymousThread(
    procedure
    var
      i: Integer;
      TestData: TBytes;
    begin
      i := 0;
      while FStressTestRunning do
      begin
        if mMClient1.IsConnected then
        begin
          TestData := bytesof(Format('🔥 Stress #%d', [i])); // Shorter message
          try
            mMClient1.SendData(TestData, i mod 2 = 0); // Alternate encryption

            // MEGA FIX: Use thread-safe counter increment
            FLogLock.Enter;
            try
              Inc(FStressTestMessages);
            finally
              FLogLock.Leave;
            end;

          except
            // Ignore errors during stress test
          end;
        end;
        Inc(i);
        Sleep(5); // MEGA FIX: Faster sending - 200 messages per second
      end;
    end).Start;
end;

procedure TForm1.StopStressTest;
begin
  if FStressTestRunning then
  begin
    FStressTestRunning := False;
    btnStressTest.Caption := 'Start Stress Test';
    LogMessage(Format('⚡ STRESS TEST STOPPED - %d messages sent', [FStressTestMessages]), 'SYSTEM');
    LogMessage('📊 UI logging restored to normal', 'SYSTEM');

    // Give the thread time to finish
    Sleep(100);
  end;
end;

procedure TForm1.btnLargeDataClick(Sender: TObject);
var
  LargeData: TBytes;
  i: Integer;
  DataStr: string;
begin
  // Create 64KB of test data
  DataStr := '';
  for i := 1 to 1024 do
    DataStr := DataStr + Format('Line %d - Large data test with lots of content! ', [i]);

  LargeData := bytesof(DataStr);

  if mMClient1.IsConnected then
  begin
    mMClient1.SendData(LargeData, True);
    LogMessage('📦 Sent LARGE DATA: ' + FormatByteSize(Length(LargeData)), 'DATA');
    Inc(FMessageCounter);
  end;
end;

// =============================================================================
// CONFIGURATION EVENT HANDLERS
// =============================================================================

procedure TForm1.EditHostChange(Sender: TObject);
begin
  if not mMClient1.Active then
    mMClient1.Host := EditHost.Text;
end;

procedure TForm1.EditPortChange(Sender: TObject);
var
  Port: Integer;
begin
  Port := StrToIntDef(EditPort.Text, 3434);
  if not mMServer1.Active then
    mMServer1.Port := Port;
  if not mMClient1.Active then
    mMClient1.Port := Port;
end;

procedure TForm1.EditMaxConnChange(Sender: TObject);
begin
  if not mMServer1.Active then
    mMServer1.MaxConnections := StrToIntDef(EditMaxConn.Text, 1000);
end;

procedure TForm1.EditBufferSizeChange(Sender: TObject);
var
  BufferSize: Integer;
begin
  BufferSize := StrToIntDef(EditBufferSize.Text, 8192);
  if not mMServer1.Active then
  begin
    mMServer1.ReceiveBufferSize := BufferSize;
    mMServer1.SendBufferSize := BufferSize;
  end;
  if not mMClient1.Active then
  begin
    mMClient1.ReceiveBufferSize := BufferSize;
    mMClient1.SendBufferSize := BufferSize;
  end;
end;

procedure TForm1.ComboLogLevelChange(Sender: TObject);
begin
  mMServer1.LogLevel := TmMServerLogLevel(ComboLogLevel.ItemIndex);
  mMClient1.LogLevel := TmMClientLogLevel(ComboLogLevel.ItemIndex);
  LogMessage('📝 Log level changed to: ' + ComboLogLevel.Text, 'SYSTEM');
end;

procedure TForm1.CheckKeepAliveClick(Sender: TObject);
begin
  mMServer1.KeepAlive := CheckKeepAlive.Checked;
  mMClient1.KeepAlive := CheckKeepAlive.Checked;
  LogMessage('🔄 Keep Alive: ' + BoolToStr(CheckKeepAlive.Checked, True), 'SYSTEM');
end;

procedure TForm1.CheckNoDelayClick(Sender: TObject);
begin
  mMServer1.NoDelay := CheckNoDelay.Checked;
  mMClient1.NoDelay := CheckNoDelay.Checked;
  LogMessage('⚡ No Delay (Nagle): ' + BoolToStr(CheckNoDelay.Checked, True), 'SYSTEM');
end;

procedure TForm1.StringGrid1Click(Sender: TObject);
begin
  // Update kick button availability
  btnKickClient.Enabled := (StringGrid1.Row > 0) and mMServer1.Active;
end;

// =============================================================================
// SERVER EVENT HANDLERS - THROTTLED FOR STRESS TESTING
// =============================================================================

procedure TForm1.mMServer1Connect(Sender: TObject; Client: TmMServerClient);
begin
  if not FStressTestRunning then
  begin
    LogMessage('🔗 CLIENT CONNECTED!', 'SERVER');
    LogMessage(Format('👤 IP: %s | Port: %d | Handle: %d', [Client.PeerIP, Client.PeerPort, Client.Handle]), 'SERVER');
    LogMessage(Format('🕒 Connected at: %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss', Client.ConnectedAt)]), 'SERVER');
    LogMessage(Format('🔐 Server Encryption: %s', [mMServer1.EncryptionInfo]), 'CRYPTO');
    LogMessage(Format('📊 Total Active: %d | Lifetime Total: %d', [mMServer1.TotalActiveConnections, mMServer1.TotalConnections]), 'STATS');
    LogMessage('═══════════════════════════════════════', 'SERVER');
  end;
end;

procedure TForm1.mMServer1Disconnect(Sender: TObject; Client: TmMServerClient);
begin
  if not FStressTestRunning then
  begin
    LogMessage('📤 CLIENT DISCONNECTED!', 'SERVER');
    LogMessage(Format('👤 IP: %s disconnected after %d seconds', [Client.PeerIP, Client.GetConnectedTime]), 'SERVER');
    LogMessage(Format('📊 Final Stats - RX: %s | TX: %s | Messages RX/TX: %d/%d',
      [FormatByteSize(Client.BytesReceived), FormatByteSize(Client.BytesSent), Client.MessagesReceived, Client.MessagesSent]), 'STATS');
    LogMessage(Format('📊 Remaining Active: %d', [mMServer1.TotalActiveConnections]), 'STATS');
    LogMessage('═══════════════════════════════════════', 'SERVER');
  end;
end;

procedure TForm1.mMServer1StateChange(Sender: TObject; OldState, NewState: TmMServerState);
const
  StateNames: array[TmMServerState] of string = ('Disconnected', 'Starting', 'Listening', 'Stopping');
  StateIcons: array[TmMServerState] of string = ('🔴', '🟡', '🟢', '🟠');
begin
  LogMessage(Format('%s SERVER STATE: %s → %s', [StateIcons[NewState], StateNames[OldState], StateNames[NewState]]), 'SERVER');

  case NewState of
    ssListening:
      begin
        LogMessage(Format('🎯 Server listening on port %d', [mMServer1.Port]), 'SERVER');
        LogMessage(Format('👥 Max connections: %d', [mMServer1.MaxConnections]), 'SERVER');
        LogMessage(Format('📦 Buffer size: %s', [FormatByteSize(mMServer1.ReceiveBufferSize)]), 'SERVER');
      end;
    ssDisconnected:
      begin
        LogMessage('🛑 Server stopped', 'SERVER');
      end;
  end;
end;

procedure TForm1.mMServer1DataReceived(Sender: TObject; Client: TmMServerClient; BytesReceived: Integer);
begin
  // MEGA FIX: Skip logging during stress test to prevent UI freeze
  if not FStressTestRunning then
  begin
    LogMessage(Format('📥 RX %s from %s (Total: %s)',
      [FormatByteSize(BytesReceived), Client.PeerIP, FormatByteSize(Client.BytesReceived)]), 'DATA');
  end;
end;

procedure TForm1.mMServer1DataSent(Sender: TObject; Client: TmMServerClient; BytesSent: Integer);
begin
  // MEGA FIX: Skip logging during stress test to prevent UI freeze
  if not FStressTestRunning then
  begin
    LogMessage(Format('📤 TX %s to %s (Total: %s)',
      [FormatByteSize(BytesSent), Client.PeerIP, FormatByteSize(Client.BytesSent)]), 'DATA');
  end;
end;

procedure TForm1.mMServer1Error(Sender: TObject; Client: TmMServerClient; const Error: string; ErrorCode: Integer);
begin
  if Assigned(Client) then
    LogMessage(Format('❌ SERVER ERROR from %s: %s (Code: %d)', [Client.PeerIP, Error, ErrorCode]), 'ERROR')
  else
    LogMessage(Format('❌ SERVER ERROR: %s (Code: %d)', [Error, ErrorCode]), 'ERROR');
end;

function TForm1.mMServer1HandleCommand(Sender: TObject; Client: TmMServerClient; aData: TBytes): TBytes;
var
  Msg: string;
  Response: string;
begin
  Msg := stringof(aData);

  // MEGA FIX: Skip verbose logging during stress test
  if not FStressTestRunning then
  begin
    LogMessage(Format('📨 [FROM %s]: %s', [Client.PeerIP, Msg]), 'SERVER');
  end;

  // Auto-respond with acknowledgment
  Response := Format('✅ Server ACK: Received %d bytes from %s at %s',
    [Length(aData), Client.PeerIP, FormatDateTime('hh:nn:ss.zzz', Now)]);
  Result := bytesof(Response);

  Inc(FMessageCounter);
end;

// =============================================================================
// CLIENT EVENT HANDLERS - THROTTLED FOR STRESS TESTING
// =============================================================================

procedure TForm1.mMClient1Connect(Sender: TObject);
begin
  LogMessage('🚀 CLIENT CONNECTED TO SERVER!', 'CLIENT');
  LogMessage(Format('🌐 Connected to: %s:%d', [mMClient1.RemoteIP, mMClient1.Port]), 'CLIENT');
  LogMessage(Format('🔐 Client Encryption: %s', [mMClient1.EncryptionInfo]), 'CRYPTO');
  LogMessage(Format('📊 Connection timeout: %d seconds', [mMClient1.ConnectionTimeout]), 'CLIENT');
  LogMessage(Format('🧵 Thread pool size: %d', [mMClient1.ThreadPoolSize]), 'CLIENT');
  LogMessage(Format('📦 Buffer sizes: RX=%s TX=%s', [FormatByteSize(mMClient1.ReceiveBufferSize), FormatByteSize(mMClient1.SendBufferSize)]), 'CLIENT');
  LogMessage('═══════════════════════════════════════', 'CLIENT');
end;

procedure TForm1.mMClient1Disconnect(Sender: TObject);
begin
  LogMessage('📤 CLIENT DISCONNECTED FROM SERVER!', 'CLIENT');
  LogMessage(Format('🌐 Was connected to: %s', [mMClient1.RemoteIP]), 'CLIENT');
  LogMessage(Format('📊 Final Stats - RX: %s | TX: %s | Messages RX/TX: %d/%d',
    [FormatByteSize(mMClient1.TotalBytesReceived), FormatByteSize(mMClient1.TotalBytesSent),
     mMClient1.MessagesReceived, mMClient1.MessagesSent]), 'STATS');
  LogMessage(Format('⏱️ Was connected for: %d seconds', [mMClient1.ConnectedTime]), 'CLIENT');
  if mMClient1.AutoReconnect then
    LogMessage(Format('🔄 Auto-reconnect enabled (Attempts: %d/%d)', [mMClient1.ReconnectAttempts, mMClient1.ReconnectMaxAttempts]), 'CLIENT');
  LogMessage('═══════════════════════════════════════', 'CLIENT');
end;

procedure TForm1.mMClient1StateChange(Sender: TObject; OldState, NewState: TmMClientState);
const
  StateNames: array[TmMClientState] of string = ('Disconnected', 'Connecting', 'Connected', 'Disconnecting');
  StateIcons: array[TmMClientState] of string = ('🔴', '🟡', '🟢', '🟠');
begin
  LogMessage(Format('%s CLIENT STATE: %s → %s', [StateIcons[NewState], StateNames[OldState], StateNames[NewState]]), 'CLIENT');
end;

procedure TForm1.mMClient1DataReceived(Sender: TObject; BytesReceived: Integer);
begin
  // MEGA FIX: Skip logging during stress test to prevent UI freeze
  if not FStressTestRunning then
  begin
    LogMessage(Format('📥 RX %s from server (Total: %s)',
      [FormatByteSize(BytesReceived), FormatByteSize(mMClient1.TotalBytesReceived)]), 'DATA');
  end;
end;

procedure TForm1.mMClient1DataSent(Sender: TObject; BytesSent: Integer);
begin
  // MEGA FIX: Skip logging during stress test to prevent UI freeze
  if not FStressTestRunning then
  begin
    LogMessage(Format('📤 TX %s to server (Total: %s)',
      [FormatByteSize(BytesSent), FormatByteSize(mMClient1.TotalBytesSent)]), 'DATA');
  end;
end;

procedure TForm1.mMClient1Error(Sender: TObject; const Error: string; ErrorCode: Integer);
begin
  LogMessage(Format('❌ CLIENT ERROR: %s (Code: %d)', [Error, ErrorCode]), 'ERROR');
end;

function TForm1.mMClient1HandleCommand(Sender: TObject; aData: TBytes): TBytes;
var
  Msg: string;
begin
  Msg := stringof(aData);

  // MEGA FIX: Skip verbose logging during stress test
  if not FStressTestRunning then
  begin
    LogMessage(Format('📨 [FROM SERVER]: %s', [Msg]), 'CLIENT');
  end;

  // Return empty response (no auto-reply from client)
  SetLength(Result, 0);
  Inc(FMessageCounter);
end;

end.
