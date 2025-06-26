unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, DateUtils,
  MorMotClientComponent;

type
  TForm1 = class(TForm)
    MorMotClient1: TMorMotClient;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Edit1: TEdit;
    Memo1: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MorMotClient1Connect(Sender: TObject);
    procedure MorMotClient1Disconnect(Sender: TObject);
    function MorMotClient1HandleCommand(Sender: TObject; aCmd: Integer; aData: TBytes; aRequiresResult: Boolean): TBytes;
    procedure MorMotClient1Reconnect(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    fShouldStayConnected: Boolean;
    fIsConnecting: Boolean;
    fLastConnectionAttempt: TDateTime;
    procedure LogMessage(const Msg: string);
    procedure UpdateStatus;
    procedure TryReconnect;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption := 'MorMot Client Demo';

  // Initialize connection state
  fShouldStayConnected := False;
  fIsConnecting := False;
  fLastConnectionAttempt := 0;

  // Setup timer for connection monitoring
  Timer1.Interval := 6000; // 6 seconds
  Timer1.Enabled := False;

  // Setup buttons
  Button1.Caption := 'Connect';
  Button2.Caption := 'Disconnect';
  Button3.Caption := 'Send Echo';
  Button4.Caption := 'Get Server Time';
  Button5.Caption := 'Get Client Count';

  Button2.Enabled := False;
  Button3.Enabled := False;
  Button4.Enabled := False;
  Button5.Enabled := False;

  // Setup controls
  Edit1.Text := 'Hello from client!';
  Label1.Caption := 'Message to send:';
  Label2.Caption := 'Client Log:';

  Memo1.ScrollBars := ssVertical;
  Memo1.ReadOnly := True;

  UpdateStatus;
  LogMessage('Client ready to connect to ' + MorMotClient1.Host + ':' + IntToStr(MorMotClient1.Port));
  LogMessage('Auto-reconnect will check every 6 seconds when enabled');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  fShouldStayConnected := False;
  Timer1.Enabled := False;
  if MorMotClient1.Active then
    MorMotClient1.Disconnect;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  // Enable auto-reconnect mode
  fShouldStayConnected := True;
  Timer1.Enabled := True;

  LogMessage('Auto-reconnect enabled - attempting first connection...');
  TryReconnect;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  // Disable auto-reconnect mode
  fShouldStayConnected := False;
  Timer1.Enabled := False;
  fIsConnecting := False;

  try
    MorMotClient1.Disconnect;
    LogMessage('Auto-reconnect disabled - disconnecting from server');
  except
    on E: Exception do
      LogMessage('Error during disconnect: ' + E.Message);
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if fShouldStayConnected and not MorMotClient1.Active then
  begin
    LogMessage('Timer: Attempting reconnection...');
    fIsConnecting := False; // Reset flag before trying
    TryReconnect;
  end;
end;

procedure TForm1.TryReconnect;
begin
  if MorMotClient1.Active then
    Exit;

  fIsConnecting := True;

  try
    LogMessage('Attempting to connect to server...');
    MorMotClient1.Connect;
  except
    on E: Exception do
    begin
      LogMessage('Connection failed: ' + E.Message);
      fIsConnecting := False;
    end;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  Response: TBytes;
  ResponseStr: string;
begin
  // Send echo command
  try
    LogMessage('Sending echo command: ' + Edit1.Text);
    Response := MorMotClient1.ExecCommand(1, TEncoding.UTF8.GetBytes(Edit1.Text), True);
    ResponseStr := TEncoding.UTF8.GetString(Response);
    LogMessage('Echo response: ' + ResponseStr);
  except
    on E: Exception do
      LogMessage('Failed to send echo command: ' + E.Message);
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  Response: TBytes;
  ResponseStr: string;
begin
  // Get server time
  try
    LogMessage('Requesting server time...');
    Response := MorMotClient1.ExecCommand(2, [], True);
    ResponseStr := TEncoding.UTF8.GetString(Response);
    LogMessage('Server time response: ' + ResponseStr);
  except
    on E: Exception do
      LogMessage('Failed to get server time: ' + E.Message);
  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  Response: TBytes;
  ResponseStr: string;
begin
  // Get client count
  try
    LogMessage('Requesting client count...');
    Response := MorMotClient1.ExecCommand(3, [], True);
    ResponseStr := TEncoding.UTF8.GetString(Response);
    LogMessage('Client count response: ' + ResponseStr);
  except
    on E: Exception do
      LogMessage('Failed to get client count: ' + E.Message);
  end;
end;

procedure TForm1.MorMotClient1Connect(Sender: TObject);
begin
  LogMessage('Connected to server!');
  fIsConnecting := False;

  Button1.Enabled := False;
  Button2.Enabled := True;
  Button3.Enabled := True;
  Button4.Enabled := True;
  Button5.Enabled := True;

  UpdateStatus;
end;

procedure TForm1.MorMotClient1Disconnect(Sender: TObject);
begin
  LogMessage('Disconnected from server');
  fIsConnecting := False;

  Button1.Enabled := True;
  Button2.Enabled := False;
  Button3.Enabled := False;
  Button4.Enabled := False;
  Button5.Enabled := False;

  UpdateStatus;

  // If we should stay connected, the timer will handle reconnection
  if fShouldStayConnected then
    LogMessage('Will attempt to reconnect in 6 seconds...');
end;

function TForm1.MorMotClient1HandleCommand(Sender: TObject; aCmd: Integer; aData: TBytes; aRequiresResult: Boolean): TBytes;
var
  DataStr: string;
  ResponseStr: string;
begin
  SetLength(Result, 0);

  try
    // Convert received data to string
    DataStr := TEncoding.UTF8.GetString(aData);

    LogMessage(Format('Received command %d: %s (RequiresResult: %s)',
      [aCmd, DataStr, BoolToStr(aRequiresResult, True)]));

    case aCmd of
      100: // Welcome message
      begin
        LogMessage('Server says: ' + DataStr);
      end;

      200: // Broadcast message
      begin
        LogMessage('Broadcast: ' + DataStr);
      end;

      201: // Private message
      begin
        LogMessage('Private message: ' + DataStr);
      end;

      999: // Ping command - respond with pong
      begin
        ResponseStr := 'Pong from client at ' + DateTimeToStr(Now);
        if aRequiresResult then
          Result := TEncoding.UTF8.GetBytes(ResponseStr);
        LogMessage('Ping received, sent pong response');
      end;

      else
      begin
        LogMessage('Unknown command received: ' + IntToStr(aCmd));
        if aRequiresResult then
          Result := TEncoding.UTF8.GetBytes('Unknown command');
      end;
    end;

  except
    on E: Exception do
    begin
      LogMessage('Error handling command: ' + E.Message);
      if aRequiresResult then
        Result := TEncoding.UTF8.GetBytes('Error: ' + E.Message);
    end;
  end;
end;

procedure TForm1.MorMotClient1Reconnect(Sender: TObject);
begin
  LogMessage('Reconnected to server!');
  UpdateStatus;
end;

procedure TForm1.LogMessage(const Msg: string);
begin
  Memo1.Lines.Add(Format('[%s] %s', [TimeToStr(Now), Msg]));

  // Keep memo from getting too long
  if Memo1.Lines.Count > 1000 then
  begin
    while Memo1.Lines.Count > 800 do
      Memo1.Lines.Delete(0);
  end;

  // Scroll to bottom
  SendMessage(Memo1.Handle, WM_VSCROLL, SB_BOTTOM, 0);
end;

procedure TForm1.UpdateStatus;
begin
  if MorMotClient1.Active then
    StatusBar1.SimpleText := 'Connected to ' + MorMotClient1.Host + ':' + IntToStr(MorMotClient1.Port)
  else if fShouldStayConnected then
    StatusBar1.SimpleText := 'Auto-reconnect enabled - Disconnected'
  else
    StatusBar1.SimpleText := 'Disconnected';
end;

end.
