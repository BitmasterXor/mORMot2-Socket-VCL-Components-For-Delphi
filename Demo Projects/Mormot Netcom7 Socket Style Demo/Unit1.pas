unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, MorMotServerComponent,
  MorMotClientComponent, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Imaging.jpeg,
  Vcl.Imaging.pngimage, System.IOUtils, Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    MorMotClient1: TMorMotClient;
    MorMotServer1: TMorMotServer;
    Panel1: TPanel;
    Panel2: TPanel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    Edit1: TEdit;
    Button3: TButton;
    Label1: TLabel;
    Button4: TButton;
    Button5: TButton;
    OpenDialog1: TOpenDialog;
    Image1: TImage;
    ScrollBox1: TScrollBox;
    Splitter1: TSplitter;
    Button6: TButton;
    Label2: TLabel;
    Button7: TButton;
    Button8: TButton;
    Label3: TLabel;
    ListView1: TListView;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button9Click(Sender: TObject);
  private
    // NetCom7-style event handlers
    function ServerHandleCommand(Sender: TObject; aCmd: Integer; aData: TBytes; aRequiresResult: Boolean): TBytes;
    procedure ServerConnect(Sender: TObject; aLine: TMorMotServerClient);
    procedure ServerDisconnect(Sender: TObject; aLine: TMorMotServerClient);

    function ClientHandleCommand(Sender: TObject; aCmd: Integer; aData: TBytes; aRequiresResult: Boolean): TBytes;
    procedure ClientConnect(Sender: TObject);
    procedure ClientDisconnect(Sender: TObject);

    // Helper methods
    function CaptureDesktop: TBytes;
    function CaptureDesktopSimple: TBytes;
    procedure LoadImageFromBytes(const Data: TBytes);
    function LoadFileToBytes(const FileName: string): TBytes;
    procedure SaveBytesToFile(const Data: TBytes; const FileName: string);
    procedure Log(const Msg: string);
    procedure UpdateConnectionsList;
    procedure AddConnection(const IP: string; Handle: Integer);
    procedure RemoveConnection(Handle: Integer);
  public
  end;


var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Setup ListView for connections
  ListView1.ViewStyle := vsReport;
  ListView1.Columns.Clear;
  ListView1.Columns.Add.Caption := 'Client IP';
  ListView1.Columns.Add.Caption := 'Handle';
  ListView1.Columns.Add.Caption := 'Connected At';
  ListView1.Columns[0].Width := 120;
  ListView1.Columns[1].Width := 80;
  ListView1.Columns[2].Width := 120;

  // Setup server - NetCom7 style
  MorMotServer1.Port := 8080;
  MorMotServer1.OnHandleCommand := ServerHandleCommand;
  MorMotServer1.OnConnect := ServerConnect;
  MorMotServer1.OnDisconnect := ServerDisconnect;

  // Setup client - NetCom7 style
  MorMotClient1.Host := 'localhost';
  MorMotClient1.Port := 8080;
  MorMotClient1.Reconnect := False; // DISABLE auto-reconnect to prevent double connections
  MorMotClient1.OnHandleCommand := ClientHandleCommand;
  MorMotClient1.OnConnect := ClientConnect;
  MorMotClient1.OnDisconnect := ClientDisconnect;

  Log('NetCom7-style demo ready. Start server first, then connect client.');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  MorMotClient1.Active := False;
  MorMotServer1.Active := False;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  // Start Server
  try
    MorMotServer1.Active := True;
    Button1.Enabled := False;
    Log('Server started on port ' + IntToStr(MorMotServer1.Port));
  except
    on E: Exception do
      Log('Failed to start server: ' + E.Message);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  RetryCount: Integer;
begin
  // Connect Client
  try
    Log('Attempting to connect to server...');

    for RetryCount := 1 to 5 do
    begin
      try
        MorMotClient1.Active := True;

        // Wait and check if connection was established
        Sleep(200);
        Application.ProcessMessages;

        if MorMotClient1.Active then
        begin
          Button2.Enabled := False;
          Log('Connection attempt #' + IntToStr(RetryCount) + ' - waiting for OnConnect event...');
          Exit;
        end;
      except
        on E: Exception do
          Log('Connection attempt #' + IntToStr(RetryCount) + ' failed: ' + E.Message);
      end;

      Sleep(1000);
      Application.ProcessMessages;
    end;

    Log('Failed to connect after 5 attempts. Check if server is running.');
  except
    on E: Exception do
      Log('Connection error: ' + E.Message);
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  ChatData: TBytes;
begin
  // Send Chat Message - simple one-way
  if not MorMotClient1.Active then
  begin
    Log('Client not connected! Use "Connect Client" first.');
    Exit;
  end;

  if Trim(Edit1.Text) = '' then
  begin
    Log('Please enter a message first.');
    Exit;
  end;

  try
    ChatData := bytesof('CHAT:' + Edit1.Text);
    MorMotClient1.ExecCommand(0, ChatData, False); // No response needed
    Log('You: ' + Edit1.Text);
    Edit1.Clear;
  except
    on E: Exception do
      Log('Failed to send message: ' + E.Message);
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  RequestData: TBytes;
begin
  // Request Screenshot - simple one-way request
  if not MorMotClient1.Active then
  begin
    Log('Client not connected! Use "Connect Client" first.');
    Exit;
  end;

  try
    Log('Requesting screenshot from server...');
    RequestData := bytesof('GET_SCREENSHOT');
    MorMotClient1.ExecCommand(0, RequestData, False); // No response expected
  except
    on E: Exception do
      Log('Failed to request screenshot: ' + E.Message);
  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  FileData: TBytes;
  FileName: string;
  FinalData: TBytes;
begin
  // Send File - simple one-way
  if not MorMotClient1.Active then
  begin
    Log('Client not connected! Use "Connect Client" first.');
    Exit;
  end;

  if OpenDialog1.Execute then
  begin
    try
      FileName := ExtractFileName(OpenDialog1.FileName);
      FileData := LoadFileToBytes(OpenDialog1.FileName);

      // Prepend identifier and filename to data
      FinalData := bytesof('FILE:' + FileName + '|') + FileData;

      Log('Sending file: ' + FileName + ' (' + IntToStr(Length(FinalData)) + ' bytes)');
      MorMotClient1.ExecCommand(0, FinalData, False); // No response needed
      Log('File sent successfully.');
    except
      on E: Exception do
        Log('Failed to send file: ' + E.Message);
    end;
  end;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  // Disconnect All - NON-BLOCKING
  try
    Log('Disconnecting all connections...');

    // Set reconnect to false BEFORE disconnecting
    MorMotClient1.Reconnect := False;

    // Disconnect in separate thread to prevent UI freeze
    TThread.CreateAnonymousThread(
      procedure
      begin
        try
          MorMotClient1.Active := False;
          Sleep(50);
          MorMotServer1.Active := False;
          Sleep(50);
        except
          // Ignore disconnect errors
        end;

        // Update UI in main thread
        TThread.Synchronize(nil,
          procedure
          begin
            Button1.Enabled := True;
            Button2.Enabled := True;
            ListView1.Clear;
            Log('All connections disconnected.');
          end);
      end).Start;

  except
    on E: Exception do
    begin
      Log('Disconnect error: ' + E.Message);
      // Force enable buttons anyway
      Button1.Enabled := True;
      Button2.Enabled := True;
    end;
  end;
end;

procedure TForm1.Button7Click(Sender: TObject);
var
  ClientsArray: TArray<TMorMotServerClient>;
  Client: TMorMotServerClient;
  RequestData: TBytes;
begin
  // Server Request Screenshot from Clients - simple one-way request
  if not MorMotServer1.Active then
  begin
    Log('Server not started! Use "Start Server" first.');
    Exit;
  end;

  ClientsArray := MorMotServer1.Clients;
  if Length(ClientsArray) = 0 then
  begin
    Log('No clients connected to request screenshot from.');
    Exit;
  end;

  try
    Log('Server requesting screenshot from ' + IntToStr(Length(ClientsArray)) + ' client(s)...');
    for Client in ClientsArray do
    begin
      Log('Sending screenshot request to client: ' + Client.PeerIP);

      try
        RequestData := bytesof('GET_SCREENSHOT');
        MorMotServer1.ExecCommand(Client, 0, RequestData, False); // No response expected
        Log('Screenshot request sent to ' + Client.PeerIP);
      except
        on E: Exception do
          Log('Error requesting screenshot from ' + Client.PeerIP + ': ' + E.Message);
      end;
    end;
  except
    on E: Exception do
      Log('Failed to request screenshot: ' + E.Message);
  end;
end;

procedure TForm1.Button8Click(Sender: TObject);
var
  ClientsArray: TArray<TMorMotServerClient>;
  Client: TMorMotServerClient;
  RequestData: TBytes;
begin
  // Server Request File from Clients - simple one-way request
  if not MorMotServer1.Active then
  begin
    Log('Server not started! Use "Start Server" first.');
    Exit;
  end;

  ClientsArray := MorMotServer1.Clients;
  if Length(ClientsArray) = 0 then
  begin
    Log('No clients connected to request file from.');
    Exit;
  end;

  try
    Log('Server requesting file from ' + IntToStr(Length(ClientsArray)) + ' client(s)...');
    for Client in ClientsArray do
    begin
      RequestData := bytesof('GET_FILE:test.txt');
      MorMotServer1.ExecCommand(Client, 0, RequestData, False); // No response expected
      Log('File request sent to ' + Client.PeerIP);
    end;
  except
    on E: Exception do
      Log('Failed to request file: ' + E.Message);
  end;
end;

procedure TForm1.Button9Click(Sender: TObject);
begin

end;

// Server Event Handlers - Simplified
function TForm1.ServerHandleCommand(Sender: TObject; aCmd: Integer; aData: TBytes; aRequiresResult: Boolean): TBytes;
var
  DataStr: string;
  SeparatorPos: Integer;
  FileName: string;
  FileData: TBytes;
  Screenshot: TBytes;
begin
  SetLength(Result, 0); // Never send responses back

  try
    DataStr := stringof(aData);

    if DataStr.StartsWith('CHAT:') then
    begin
      // Handle chat message
      Log('Client: ' + Copy(DataStr, 6, Length(DataStr)));
    end
    else if DataStr = 'GET_SCREENSHOT' then
    begin
      // Client wants server's screenshot - send it back
      Log('Client requesting server screenshot...');
      Screenshot := CaptureDesktop;
      if Length(Screenshot) = 0 then
        Screenshot := CaptureDesktopSimple;

      // Send screenshot back to client as IMAGE: prefix
      if Length(Screenshot) > 0 then
      begin
        // Find the client that requested it and send back
        var ClientsArray := MorMotServer1.Clients;
        for var Client in ClientsArray do
        begin
          var ImageData := bytesof('IMAGE:') + Screenshot;
          MorMotServer1.ExecCommand(Client, 0, ImageData, False);
          Log('Sent screenshot to client ' + Client.PeerIP + ' (' + IntToStr(Length(Screenshot)) + ' bytes)');
        end;
      end;
    end
    else if DataStr.StartsWith('FILE:') then
    begin
      // Handle file transfer
      SeparatorPos := Pos('|', DataStr);
      if SeparatorPos > 6 then
      begin
        FileName := Copy(DataStr, 6, SeparatorPos - 6);

        // Extract file data (everything after the separator)
        SetLength(FileData, Length(aData) - Length(bytesof(Copy(DataStr, 1, SeparatorPos))));
        Move(aData[Length(bytesof(Copy(DataStr, 1, SeparatorPos)))], FileData[0], Length(FileData));

        FileName := TPath.Combine(TPath.GetDocumentsPath, 'Received_' + FileName);
        SaveBytesToFile(FileData, FileName);
        Log('File received and saved: ' + ExtractFileName(FileName) + ' (' + IntToStr(Length(FileData)) + ' bytes)');
      end;
    end
    else if DataStr.StartsWith('IMAGE:') then
    begin
      // Handle incoming image - display it
      Log('Received image from client...');
      SetLength(FileData, Length(aData) - 6); // Skip "IMAGE:" prefix
      Move(aData[6], FileData[0], Length(FileData));

      Log('Loading image: ' + IntToStr(Length(FileData)) + ' bytes');
      LoadImageFromBytes(FileData);
      Log('Image displayed successfully');
    end;

  except
    on E: Exception do
      Log('Error processing command: ' + E.Message);
  end;
end;

procedure TForm1.ServerConnect(Sender: TObject; aLine: TMorMotServerClient);
begin
  Log('Client connected from: ' + aLine.PeerIP);
  AddConnection(aLine.PeerIP, aLine.Handle);
end;

procedure TForm1.ServerDisconnect(Sender: TObject; aLine: TMorMotServerClient);
begin
  Log('Client disconnected from: ' + aLine.PeerIP);
  RemoveConnection(aLine.Handle);
end;

// Client Event Handlers - Simplified
function TForm1.ClientHandleCommand(Sender: TObject; aCmd: Integer; aData: TBytes; aRequiresResult: Boolean): TBytes;
var
  DataStr: string;
  Screenshot: TBytes;
  FileData: TBytes;
  RequestedFileName: string;
begin
  SetLength(Result, 0); // Never send responses back

  try
    DataStr := stringof(aData);
    Log('Client received: ' + Copy(DataStr, 1, 50) + '...');

    if DataStr = 'GET_SCREENSHOT' then
    begin
      // Server wants our screenshot - send it back
      Log('Server requesting our screenshot...');
      Screenshot := CaptureDesktop;
      if Length(Screenshot) = 0 then
        Screenshot := CaptureDesktopSimple;

      if Length(Screenshot) > 0 then
      begin
        // Send screenshot back to server with IMAGE: prefix
        var ImageData := bytesof('IMAGE:') + Screenshot;
        MorMotClient1.ExecCommand(0, ImageData, False);
        Log('Sent screenshot to server (' + IntToStr(Length(Screenshot)) + ' bytes)');
      end;
    end
    else if DataStr.StartsWith('GET_FILE:') then
    begin
      // Server wants a file
      RequestedFileName := Copy(DataStr, 10, Length(DataStr));
      Log('Server requesting file: ' + RequestedFileName);

      try
        // Create a demo file in temp folder
        RequestedFileName := TPath.Combine(TPath.GetTempPath, RequestedFileName);
        if not FileExists(RequestedFileName) then
        begin
          FileData := bytesof('Demo file created by client at ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
          SaveBytesToFile(FileData, RequestedFileName);
        end;

        FileData := LoadFileToBytes(RequestedFileName);
        var FileResponse := bytesof('FILE:' + ExtractFileName(RequestedFileName) + '|') + FileData;
        MorMotClient1.ExecCommand(0, FileResponse, False);
        Log('Sent file to server: ' + ExtractFileName(RequestedFileName) + ' (' + IntToStr(Length(FileData)) + ' bytes)');
      except
        on E: Exception do
          Log('Failed to send requested file: ' + E.Message);
      end;
    end
    else if DataStr.StartsWith('IMAGE:') then
    begin
      // Handle incoming image from server - display it
      Log('Received image from server...');
      SetLength(FileData, Length(aData) - 6); // Skip "IMAGE:" prefix
      Move(aData[6], FileData[0], Length(FileData));

      Log('Loading image: ' + IntToStr(Length(FileData)) + ' bytes');
      LoadImageFromBytes(FileData);
      Log('Image displayed successfully');
    end
    else if DataStr.StartsWith('CHAT:') then
    begin
      Log('Server: ' + Copy(DataStr, 6, Length(DataStr)));
    end;

  except
    on E: Exception do
      Log('Error processing command: ' + E.Message);
  end;
end;

procedure TForm1.ClientConnect(Sender: TObject);
begin
  Log('✓ Connected to server successfully!');
  Button2.Enabled := False;
end;

procedure TForm1.ClientDisconnect(Sender: TObject);
begin
  Log('✗ Disconnected from server.');
  Button2.Enabled := True;
end;

// Helper Methods
function TForm1.CaptureDesktop: TBytes;
var
  DesktopDC: HDC;
  ScreenWidth, ScreenHeight: Integer;
  VCLBitmap: TBitmap;
  JpegImage: TJPEGImage;
  Stream: TMemoryStream;
begin
  SetLength(Result, 0);

  try
    // Get screen dimensions
    ScreenWidth := GetSystemMetrics(SM_CXSCREEN);
    ScreenHeight := GetSystemMetrics(SM_CYSCREEN);

    if (ScreenWidth <= 0) or (ScreenHeight <= 0) then
    begin
      Log('Invalid screen dimensions');
      Exit;
    end;

    // Create VCL bitmap
    VCLBitmap := TBitmap.Create;
    JpegImage := TJPEGImage.Create;
    Stream := TMemoryStream.Create;
    try
      // Set bitmap properties
      VCLBitmap.PixelFormat := pf24bit;
      VCLBitmap.Width := ScreenWidth;
      VCLBitmap.Height := ScreenHeight;

      // Get desktop DC
      DesktopDC := GetDC(0);
      if DesktopDC = 0 then
      begin
        Log('Failed to get desktop DC');
        Exit;
      end;

      try
        // Copy screen to bitmap
        if not BitBlt(VCLBitmap.Canvas.Handle, 0, 0, ScreenWidth, ScreenHeight,
                     DesktopDC, 0, 0, SRCCOPY) then
        begin
          Log('BitBlt failed');
          Exit;
        end;

        // Convert to JPEG
        JpegImage.Assign(VCLBitmap);
        JpegImage.CompressionQuality := 60; // Lower quality for smaller size
        JpegImage.SaveToStream(Stream);

        // Convert to bytes
        if Stream.Size > 0 then
        begin
          SetLength(Result, Stream.Size);
          Move(Stream.Memory^, Result[0], Stream.Size);
          Log('Desktop captured: ' + IntToStr(Length(Result)) + ' bytes');
        end
        else
        begin
          Log('No data in JPEG stream');
        end;

      finally
        ReleaseDC(0, DesktopDC);
      end;

    finally
      Stream.Free;
      JpegImage.Free;
      VCLBitmap.Free;
    end;
  except
    on E: Exception do
    begin
      Log('Failed to capture desktop: ' + E.Message);
      SetLength(Result, 0);
    end;
  end;
end;

function TForm1.CaptureDesktopSimple: TBytes;
var
  Bitmap: TBitmap;
  JpegImage: TJPEGImage;
  Stream: TMemoryStream;
begin
  SetLength(Result, 0);

  try
    // Create a simple test image instead of screen capture
    Bitmap := TBitmap.Create;
    JpegImage := TJPEGImage.Create;
    Stream := TMemoryStream.Create;
    try
      // Create a 800x600 test image
      Bitmap.Width := 800;
      Bitmap.Height := 600;
      Bitmap.PixelFormat := pf24bit;

      // Draw something simple
      Bitmap.Canvas.Brush.Color := clBlue;
      Bitmap.Canvas.FillRect(Rect(0, 0, 800, 600));

      Bitmap.Canvas.Font.Size := 48;
      Bitmap.Canvas.Font.Color := clWhite;
      Bitmap.Canvas.TextOut(50, 250, 'NetCom7 Screenshot Test');

      Bitmap.Canvas.Font.Size := 24;
      Bitmap.Canvas.TextOut(50, 350, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));

      // Convert to JPEG
      JpegImage.Assign(Bitmap);
      JpegImage.CompressionQuality := 70;
      JpegImage.SaveToStream(Stream);

      // Convert to bytes
      SetLength(Result, Stream.Size);
      Move(Stream.Memory^, Result[0], Stream.Size);

      Log('Test screenshot created: ' + IntToStr(Length(Result)) + ' bytes');

    finally
      Stream.Free;
      JpegImage.Free;
      Bitmap.Free;
    end;
  except
    on E: Exception do
    begin
      Log('Failed to create test screenshot: ' + E.Message);
      SetLength(Result, 0);
    end;
  end;
end;

procedure TForm1.LoadImageFromBytes(const Data: TBytes);
var
  Stream: TMemoryStream;
  JpegImage: TJPEGImage;
  Bitmap: TBitmap;
begin
  if Length(Data) = 0 then
  begin
    Log('LoadImageFromBytes: No data to load');
    Exit;
  end;

  try
    Stream := TMemoryStream.Create;
    try
      Stream.WriteBuffer(Data[0], Length(Data));
      Stream.Position := 0;

      Log('LoadImageFromBytes: Stream size = ' + IntToStr(Stream.Size));

      // Try JPEG first
      try
        JpegImage := TJPEGImage.Create;
        try
          JpegImage.LoadFromStream(Stream);
          Image1.Picture.Assign(JpegImage);
          Log('Image loaded successfully as JPEG');
          Exit;
        finally
          JpegImage.Free;
        end;
      except
        on E: Exception do
          Log('Failed to load as JPEG: ' + E.Message);
      end;

      // Try Bitmap
      try
        Stream.Position := 0;
        Bitmap := TBitmap.Create;
        try
          Bitmap.LoadFromStream(Stream);
          Image1.Picture.Assign(Bitmap);
          Log('Image loaded successfully as Bitmap');
        finally
          Bitmap.Free;
        end;
      except
        on E: Exception do
          Log('Failed to load as Bitmap: ' + E.Message);
      end;

    finally
      Stream.Free;
    end;
  except
    on E: Exception do
      Log('Failed to load image: ' + E.Message);
  end;
end;

function TForm1.LoadFileToBytes(const FileName: string): TBytes;
var
  FileStream: TFileStream;
begin
  SetLength(Result, 0);

  if not FileExists(FileName) then
    raise Exception.Create('File not found: ' + FileName);

  try
    FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      SetLength(Result, FileStream.Size);
      FileStream.ReadBuffer(Result[0], FileStream.Size);
    finally
      FileStream.Free;
    end;
  except
    on E: Exception do
      raise Exception.Create('Failed to read file: ' + E.Message);
  end;
end;

procedure TForm1.SaveBytesToFile(const Data: TBytes; const FileName: string);
var
  FileStream: TFileStream;
begin
  if Length(Data) = 0 then
    raise Exception.Create('No data to save');

  try
    FileStream := TFileStream.Create(FileName, fmCreate);
    try
      FileStream.WriteBuffer(Data[0], Length(Data));
    finally
      FileStream.Free;
    end;
  except
    on E: Exception do
      raise Exception.Create('Failed to save file: ' + E.Message);
  end;
end;

procedure TForm1.Log(const Msg: string);
begin
  Memo1.Lines.Add(FormatDateTime('hh:nn:ss', Now) + ' - ' + Msg);
  SendMessage(Memo1.Handle, WM_VSCROLL, SB_BOTTOM, 0);
end;

// ListView Management Methods
procedure TForm1.UpdateConnectionsList;
var
  ClientsArray: TArray<TMorMotServerClient>;
  Client: TMorMotServerClient;
  Item: TListItem;
begin
  ListView1.Clear;

  if MorMotServer1.Active then
  begin
    ClientsArray := MorMotServer1.Clients;
    for Client in ClientsArray do
    begin
      Item := ListView1.Items.Add;
      Item.Caption := Client.PeerIP;
      Item.SubItems.Add(IntToStr(Client.Handle));
      Item.SubItems.Add(FormatDateTime('hh:nn:ss', Client.ConnectedAt));
      Item.Data := Pointer(Client.Handle);
    end;
  end;
end;

procedure TForm1.AddConnection(const IP: string; Handle: Integer);
var
  Item: TListItem;
begin
  Item := ListView1.Items.Add;
  Item.Caption := IP;
  Item.SubItems.Add(IntToStr(Handle));
  Item.SubItems.Add(FormatDateTime('hh:nn:ss', Now));
  Item.Data := Pointer(Handle);

  Log('Added to connections list: ' + IP + ' (Handle: ' + IntToStr(Handle) + ')');
end;

procedure TForm1.RemoveConnection(Handle: Integer);
var
  i: Integer;
begin
  for i := ListView1.Items.Count - 1 downto 0 do
  begin
    if Integer(ListView1.Items[i].Data) = Handle then
    begin
      Log('Removed from connections list: ' + ListView1.Items[i].Caption + ' (Handle: ' + IntToStr(Handle) + ')');
      ListView1.Items.Delete(i);
      Break;
    end;
  end;
end;

end.
