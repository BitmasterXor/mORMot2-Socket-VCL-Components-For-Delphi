unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.Menus, Vcl.StdCtrls,
  MorMotServerComponent;

type
  TForm1 = class(TForm)
    ListView1: TListView;
    PopupMenu1: TPopupMenu;
    S1: TMenuItem;
    S2: TMenuItem;
    MorMotServer1: TMorMotServer;
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure S1Click(Sender: TObject);
    procedure S2Click(Sender: TObject);
    procedure MorMotServer1Connect(Sender: TObject; aLine: TMorMotServerClient);
    procedure MorMotServer1Disconnect(Sender: TObject; aLine: TMorMotServerClient);
    procedure MorMotServer1Reconnect(Sender: TObject; aLine: TMorMotServerClient);
    function MorMotServer1HandleCommand(Sender: TObject; aCmd: Integer; aData: TBytes; aRequiresResult: Boolean): TBytes;
    procedure MorMotServer1Log(Sender: TObject; const LogMsg: string);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    procedure UpdateClientList;
    procedure LogMessage(const Msg: string);
    function GetSelectedClient: TMorMotServerClient;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption := 'MorMot Server Demo';

  // Setup ListView columns
  ListView1.Columns[0].Caption := 'IP Address';
  ListView1.Columns[1].Caption := 'Client ID';
  ListView1.Columns[2].Caption := 'PC Name';
  ListView1.Columns[3].Caption := 'UserName';
  ListView1.PopupMenu := PopupMenu1;

  // Setup buttons
  Button1.Caption := 'Start Server';
  Button2.Caption := 'Stop Server';
  Button2.Enabled := False;

  // Setup memo for logging
  Memo1.ScrollBars := ssVertical;
  Memo1.ReadOnly := True;
  Label1.Caption := 'Server Log:';

  LogMessage('Server ready to start on port ' + IntToStr(MorMotServer1.Port));
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if MorMotServer1.Active then
    MorMotServer1.Stop;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  try
    MorMotServer1.Start;
    Button1.Enabled := False;
    Button2.Enabled := True;
    LogMessage('Server started successfully on port ' + IntToStr(MorMotServer1.Port));
  except
    on E: Exception do
      LogMessage('Failed to start server: ' + E.Message);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  try
    MorMotServer1.Stop;
    Button1.Enabled := True;
    Button2.Enabled := False;
    ListView1.Clear;
    LogMessage('Server stopped');
  except
    on E: Exception do
      LogMessage('Error stopping server: ' + E.Message);
  end;
end;

procedure TForm1.MorMotServer1Connect(Sender: TObject; aLine: TMorMotServerClient);
begin
  LogMessage('Client connected from ' + aLine.PeerIP);
  UpdateClientList;

  // Send welcome message to new client
  try
    MorMotServer1.ExecCommand(aLine, 100, TEncoding.UTF8.GetBytes('Welcome to MorMot Server!'), False);
  except
    on E: Exception do
      LogMessage('Failed to send welcome message: ' + E.Message);
  end;
end;

procedure TForm1.MorMotServer1Disconnect(Sender: TObject; aLine: TMorMotServerClient);
begin
  LogMessage('Client disconnected from ' + aLine.PeerIP);
  UpdateClientList;
end;

procedure TForm1.MorMotServer1Reconnect(Sender: TObject; aLine: TMorMotServerClient);
begin
  LogMessage('Client reconnected from ' + aLine.PeerIP);
  UpdateClientList;
end;

function TForm1.MorMotServer1HandleCommand(Sender: TObject; aCmd: Integer; aData: TBytes; aRequiresResult: Boolean): TBytes;
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
      1: // Echo command
      begin
        ResponseStr := 'Echo: ' + DataStr;
        if aRequiresResult then
          Result := TEncoding.UTF8.GetBytes(ResponseStr);
      end;

      2: // Get server time
      begin
        ResponseStr := 'Server time: ' + DateTimeToStr(Now);
        if aRequiresResult then
          Result := TEncoding.UTF8.GetBytes(ResponseStr);
      end;

      3: // Get client count
      begin
        ResponseStr := Format('Connected clients: %d', [MorMotServer1.ClientCount]);
        if aRequiresResult then
          Result := TEncoding.UTF8.GetBytes(ResponseStr);
      end;

      else
      begin
        ResponseStr := 'Unknown command: ' + IntToStr(aCmd);
        if aRequiresResult then
          Result := TEncoding.UTF8.GetBytes(ResponseStr);
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

procedure TForm1.MorMotServer1Log(Sender: TObject; const LogMsg: string);
begin
  LogMessage('MorMot: ' + LogMsg);
end;

procedure TForm1.S1Click(Sender: TObject);
var
  Clients: TArray<TMorMotServerClient>;
  Client: TMorMotServerClient;
  Message: string;
begin
  // Send to ALL clients
  Message := 'Broadcast message from server at ' + DateTimeToStr(Now);
  Clients := MorMotServer1.Clients;

  LogMessage(Format('Broadcasting to %d clients: %s', [Length(Clients), Message]));

  for Client in Clients do
  begin
    try
      MorMotServer1.ExecCommand(Client, 200, TEncoding.UTF8.GetBytes(Message), False);
    except
      on E: Exception do
        LogMessage('Failed to send broadcast to client: ' + E.Message);
    end;
  end;
end;

procedure TForm1.S2Click(Sender: TObject);
var
  SelectedClient: TMorMotServerClient;
  Message: string;
begin
  // Send to selected client
  SelectedClient := GetSelectedClient;
  if not Assigned(SelectedClient) then
  begin
    LogMessage('No client selected');
    Exit;
  end;

  Message := 'Private message from server at ' + DateTimeToStr(Now);
  LogMessage(Format('Sending private message to %s: %s', [SelectedClient.PeerIP, Message]));

  try
    MorMotServer1.ExecCommand(SelectedClient, 201, TEncoding.UTF8.GetBytes(Message), False);
  except
    on E: Exception do
      LogMessage('Failed to send private message: ' + E.Message);
  end;
end;

procedure TForm1.UpdateClientList;
var
  Clients: TArray<TMorMotServerClient>;
  Client: TMorMotServerClient;
  Item: TListItem;
begin
  ListView1.Clear;

  Clients := MorMotServer1.Clients;
  for Client in Clients do
  begin
    Item := ListView1.Items.Add;
    Item.Caption := Client.PeerIP;
    Item.SubItems.Add(IntToStr(Integer(Client.Handle)));
    Item.SubItems.Add('Unknown'); // PC Name - would need client to send this
    Item.SubItems.Add('Unknown'); // Username - would need client to send this
    Item.Data := Client;
  end;

  Caption := Format('MorMot Server Demo - %d clients connected', [Length(Clients)]);
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

function TForm1.GetSelectedClient: TMorMotServerClient;
begin
  Result := nil;
  if (ListView1.Selected <> nil) and Assigned(ListView1.Selected.Data) then
    Result := TMorMotServerClient(ListView1.Selected.Data);
end;

end.
