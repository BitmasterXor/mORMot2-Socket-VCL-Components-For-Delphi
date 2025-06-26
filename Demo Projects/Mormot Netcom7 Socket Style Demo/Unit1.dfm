object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 
    'NetCom7-Style MorMot Demo - Bidirectional Chat, Screenshot, File' +
    ' Transfer'
  ClientHeight = 700
  ClientWidth = 1000
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 450
    Top = 0
    Height = 700
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 450
    Height = 700
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object GroupBox1: TGroupBox
      Left = 8
      Top = 8
      Width = 434
      Height = 105
      Caption = ' Server Control '
      TabOrder = 0
      object Label2: TLabel
        Left = 16
        Top = 60
        Width = 386
        Height = 26
        Caption = 
          'Server can now REQUEST screenshots and files FROM clients! Start' +
          ' server, connect client, then use the request buttons to pull da' +
          'ta from connected clients.'
        WordWrap = True
      end
      object Button1: TButton
        Left = 16
        Top = 24
        Width = 100
        Height = 30
        Caption = 'Start Server'
        TabOrder = 0
        OnClick = Button1Click
      end
      object Button7: TButton
        Left = 130
        Top = 24
        Width = 120
        Height = 30
        Caption = 'Request Client Screenshot'
        TabOrder = 1
        OnClick = Button7Click
      end
      object Button8: TButton
        Left = 260
        Top = 24
        Width = 120
        Height = 30
        Caption = 'Request Client File'
        TabOrder = 2
        OnClick = Button8Click
      end
    end
    object GroupBox2: TGroupBox
      Left = 8
      Top = 119
      Width = 434
      Height = 165
      Caption = ' Client Control '
      TabOrder = 1
      object Label1: TLabel
        Left = 16
        Top = 105
        Width = 68
        Height = 13
        Caption = 'Chat Message'
      end
      object Label3: TLabel
        Left = 16
        Top = 60
        Width = 357
        Height = 13
        Caption = 
          'Client automatically responds to server requests for screenshots' +
          ' and files!'
        WordWrap = True
      end
      object Button2: TButton
        Left = 16
        Top = 24
        Width = 100
        Height = 30
        Caption = 'Connect Client'
        TabOrder = 0
        OnClick = Button2Click
      end
      object Edit1: TEdit
        Left = 16
        Top = 124
        Width = 300
        Height = 21
        TabOrder = 1
      end
      object Button3: TButton
        Left = 322
        Top = 122
        Width = 100
        Height = 25
        Caption = 'Send Chat'
        TabOrder = 2
        OnClick = Button3Click
      end
      object Button4: TButton
        Left = 130
        Top = 24
        Width = 120
        Height = 30
        Caption = 'Get Server Screenshot'
        TabOrder = 3
        OnClick = Button4Click
      end
      object Button5: TButton
        Left = 260
        Top = 24
        Width = 120
        Height = 30
        Caption = 'Send File to Server'
        TabOrder = 4
        OnClick = Button5Click
      end
      object Button6: TButton
        Left = 16
        Top = 88
        Width = 100
        Height = 25
        Caption = 'Disconnect All'
        TabOrder = 5
        OnClick = Button6Click
      end
    end
    object GroupBox3: TGroupBox
      Left = 8
      Top = 290
      Width = 434
      Height = 140
      Caption = ' Active Connections '
      TabOrder = 2
      object Label4: TLabel
        Left = 16
        Top = 20
        Width = 147
        Height = 13
        Caption = 'Connected Clients (Real-time):'
      end
      object ListView1: TListView
        Left = 16
        Top = 39
        Width = 402
        Height = 95
        Columns = <
          item
            Caption = 'Client IP'
            Width = 120
          end
          item
            Caption = 'Handle'
            Width = 80
          end
          item
            Caption = 'Connected At'
            Width = 120
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object Memo1: TMemo
      Left = 8
      Top = 436
      Width = 434
      Height = 256
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Consolas'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 3
      WordWrap = False
    end
  end
  object Panel2: TPanel
    Left = 453
    Top = 0
    Width = 547
    Height = 700
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Screenshot Display Area - Shows screenshots from either side'
    TabOrder = 1
    object ScrollBox1: TScrollBox
      Left = 0
      Top = 0
      Width = 547
      Height = 700
      Align = alClient
      TabOrder = 0
      object Image1: TImage
        Left = 0
        Top = 0
        Width = 543
        Height = 696
        Align = alClient
        Center = True
        Proportional = True
        Stretch = True
        ExplicitLeft = 224
        ExplicitTop = 304
        ExplicitWidth = 105
        ExplicitHeight = 105
      end
    end
  end
  object MorMotClient1: TMorMotClient
    Active = False
    Host = 'localhost'
    Port = 8080
    Left = 184
    Top = 320
  end
  object MorMotServer1: TMorMotServer
    Active = False
    Port = 8080
    Left = 88
    Top = 320
  end
  object OpenDialog1: TOpenDialog
    Filter = 'All Files (*.*)|*.*'
    Title = 'Select File to Send'
    Left = 288
    Top = 320
  end
end
