object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'MorMot Client Demo'
  ClientHeight = 400
  ClientWidth = 600
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object Label1: TLabel
    Left = 8
    Top = 88
    Width = 91
    Height = 15
    Caption = 'Message to send:'
  end
  object Label2: TLabel
    Left = 8
    Top = 168
    Width = 57
    Height = 15
    Caption = 'Client Log:'
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 100
    Height = 32
    Caption = 'Connect'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 114
    Top = 8
    Width = 100
    Height = 32
    Caption = 'Disconnect'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 136
    Width = 100
    Height = 25
    Caption = 'Send Echo'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 114
    Top = 136
    Width = 100
    Height = 25
    Caption = 'Get Server Time'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 220
    Top = 136
    Width = 100
    Height = 25
    Caption = 'Get Client Count'
    TabOrder = 4
    OnClick = Button5Click
  end
  object Edit1: TEdit
    Left = 8
    Top = 109
    Width = 584
    Height = 23
    TabOrder = 5
    Text = 'Hello from client!'
  end
  object Memo1: TMemo
    Left = 8
    Top = 189
    Width = 584
    Height = 180
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 6
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 381
    Width = 600
    Height = 19
    Panels = <>
    SimplePanel = True
    SimpleText = 'Disconnected'
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 6000
    OnTimer = Timer1Timer
    Left = 424
    Top = 16
  end
  object MorMotClient1: TMorMotClient
    Active = False
    Host = 'localhost'
    Reconnect = False
    OnHandleCommand = MorMotClient1HandleCommand
    OnConnect = MorMotClient1Connect
    OnDisconnect = MorMotClient1Disconnect
    OnReconnect = MorMotClient1Reconnect
    Left = 328
    Top = 16
  end
end
