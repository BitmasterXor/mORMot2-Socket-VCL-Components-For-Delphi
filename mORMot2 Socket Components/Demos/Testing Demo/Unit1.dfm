object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'ULTIMATE mORMot2 Socket Demo - ALL FEATURES ENABLED!'
  ClientHeight = 800
  ClientWidth = 1400
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 280
    Height = 781
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object GroupBox1: TGroupBox
      Left = 8
      Top = 8
      Width = 264
      Height = 160
      Caption = 'SERVER CONTROLS'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      object Button1: TButton
        Left = 8
        Top = 20
        Width = 120
        Height = 30
        Caption = 'Start Server'
        TabOrder = 0
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 136
        Top = 20
        Width = 120
        Height = 30
        Caption = 'Stop Server'
        TabOrder = 1
        OnClick = Button2Click
      end
      object btnDisconnectAll: TButton
        Left = 8
        Top = 56
        Width = 120
        Height = 30
        Caption = 'Disconnect All'
        TabOrder = 2
        OnClick = btnDisconnectAllClick
      end
      object btnBroadcastUnenc: TButton
        Left = 136
        Top = 56
        Width = 120
        Height = 30
        Caption = 'Broadcast'
        TabOrder = 3
        OnClick = btnBroadcastUnencClick
      end
      object btnBroadcastEnc: TButton
        Left = 8
        Top = 92
        Width = 120
        Height = 30
        Caption = 'Broadcast Enc'
        TabOrder = 4
        OnClick = btnBroadcastEncClick
      end
      object btnKickClient: TButton
        Left = 136
        Top = 92
        Width = 120
        Height = 30
        Caption = 'Kick Client'
        TabOrder = 5
        OnClick = btnKickClientClick
      end
    end
    object GroupBox2: TGroupBox
      Left = 8
      Top = 174
      Width = 264
      Height = 160
      Caption = 'CLIENT CONTROLS'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      object Button3: TButton
        Left = 8
        Top = 20
        Width = 120
        Height = 30
        Caption = 'Connect'
        TabOrder = 0
        OnClick = Button3Click
      end
      object Button4: TButton
        Left = 136
        Top = 20
        Width = 120
        Height = 30
        Caption = 'Disconnect'
        TabOrder = 1
        OnClick = Button4Click
      end
      object Button5: TButton
        Left = 8
        Top = 56
        Width = 120
        Height = 30
        Caption = 'Send Text'
        TabOrder = 2
        OnClick = Button5Click
      end
      object Button6: TButton
        Left = 136
        Top = 56
        Width = 120
        Height = 30
        Caption = 'Send Encrypted'
        TabOrder = 3
        OnClick = Button6Click
      end
      object btnReconnect: TButton
        Left = 8
        Top = 92
        Width = 120
        Height = 30
        Caption = 'Reconnect'
        TabOrder = 4
        OnClick = btnReconnectClick
      end
      object btnClearStats: TButton
        Left = 136
        Top = 92
        Width = 120
        Height = 30
        Caption = 'Clear Stats'
        TabOrder = 5
        OnClick = btnClearStatsClick
      end
    end
    object GroupBox3: TGroupBox
      Left = 8
      Top = 340
      Width = 264
      Height = 160
      Caption = 'ADVANCED FEATURES'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      object btnStressTest: TButton
        Left = 8
        Top = 20
        Width = 120
        Height = 30
        Caption = 'Start Stress Test'
        TabOrder = 0
        OnClick = btnStressTestClick
      end
      object btnLargeData: TButton
        Left = 136
        Top = 20
        Width = 120
        Height = 30
        Caption = 'Large Data'
        TabOrder = 1
        OnClick = btnLargeDataClick
      end
      object btnEncryptionTest: TButton
        Left = 8
        Top = 56
        Width = 120
        Height = 30
        Caption = 'Crypto Test'
        TabOrder = 2
        OnClick = btnEncryptionTestClick
      end
      object btnAutoReconnect: TButton
        Left = 136
        Top = 56
        Width = 120
        Height = 30
        Caption = 'Auto-Reconnect'
        TabOrder = 3
        OnClick = btnAutoReconnectClick
      end
      object btnChangeKey: TButton
        Left = 8
        Top = 92
        Width = 248
        Height = 30
        Caption = 'Change Encryption Key'
        TabOrder = 4
        OnClick = btnChangeKeyClick
      end
    end
    object GroupBox4: TGroupBox
      Left = 8
      Top = 506
      Width = 264
      Height = 220
      Caption = 'CONFIGURATION'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 3
      object EditHost: TEdit
        Left = 8
        Top = 20
        Width = 100
        Height = 21
        Hint = 'Server Host/IP'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        Text = '127.0.0.1'
        OnChange = EditHostChange
      end
      object EditPort: TEdit
        Left = 116
        Top = 20
        Width = 60
        Height = 21
        Hint = 'Port Number'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        Text = '3434'
        OnChange = EditPortChange
      end
      object EditMaxConn: TEdit
        Left = 184
        Top = 20
        Width = 72
        Height = 21
        Hint = 'Max Connections'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        Text = '1000'
        OnChange = EditMaxConnChange
      end
      object EditBufferSize: TEdit
        Left = 8
        Top = 50
        Width = 100
        Height = 21
        Hint = 'Buffer Size (bytes)'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
        Text = '8192'
        OnChange = EditBufferSizeChange
      end
      object ComboLogLevel: TComboBox
        Left = 116
        Top = 50
        Width = 140
        Height = 21
        Style = csDropDownList
        TabOrder = 4
        OnChange = ComboLogLevelChange
        Items.Strings = (
          'None'
          'Errors'
          'Debug'
          'Client/Server'
          'Verbose')
      end
      object CheckKeepAlive: TCheckBox
        Left = 8
        Top = 80
        Width = 97
        Height = 17
        Caption = 'Keep Alive'
        Checked = True
        State = cbChecked
        TabOrder = 5
        OnClick = CheckKeepAliveClick
      end
      object CheckNoDelay: TCheckBox
        Left = 116
        Top = 80
        Width = 97
        Height = 17
        Caption = 'No Delay'
        Checked = True
        State = cbChecked
        TabOrder = 6
        OnClick = CheckNoDelayClick
      end
    end
  end
  object Panel3: TPanel
    Left = 280
    Top = 0
    Width = 1120
    Height = 781
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object GroupBox5: TGroupBox
      Left = 8
      Top = 8
      Width = 1104
      Height = 180
      Caption = 'CONNECTED CLIENTS'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      object StringGrid1: TStringGrid
        Left = 8
        Top = 20
        Width = 1088
        Height = 152
        DefaultRowHeight = 18
        FixedCols = 0
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect]
        ParentFont = False
        TabOrder = 0
        OnClick = StringGrid1Click
      end
    end
    object GroupBox6: TGroupBox
      Left = 8
      Top = 194
      Width = 1104
      Height = 200
      Caption = 'LIVE STATISTICS'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      object Label1: TLabel
        Left = 8
        Top = 20
        Width = 270
        Height = 120
        AutoSize = False
        Caption = 'SERVER STATS:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object Label2: TLabel
        Left = 284
        Top = 20
        Width = 270
        Height = 120
        AutoSize = False
        Caption = 'CLIENT STATS:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object Label3: TLabel
        Left = 560
        Top = 20
        Width = 270
        Height = 120
        AutoSize = False
        Caption = 'CONNECTION INFO:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object Label4: TLabel
        Left = 836
        Top = 20
        Width = 260
        Height = 120
        AutoSize = False
        Caption = 'THROUGHPUT:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
    end
    object Memo1: TMemo
      Left = 8
      Top = 400
      Width = 1104
      Height = 373
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Consolas'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 2
      WordWrap = False
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 781
    Width = 1400
    Height = 19
    Panels = <
      item
        Width = 150
      end
      item
        Width = 180
      end
      item
        Width = 120
      end
      item
        Width = 150
      end
      item
        Width = 150
      end>
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 240
    Top = 740
  end
  object mMServer1: TmMServer
    OnHandleCommand = mMServer1HandleCommand
    OnConnect = mMServer1Connect
    OnDisconnect = mMServer1Disconnect
    OnError = mMServer1Error
    OnStateChange = mMServer1StateChange
    OnDataSent = mMServer1DataSent
    OnDataReceived = mMServer1DataReceived
    Left = 136
    Top = 740
  end
  object mMClient1: TmMClient
    Host = 'localhost'
    OnHandleCommand = mMClient1HandleCommand
    OnConnect = mMClient1Connect
    OnDisconnect = mMClient1Disconnect
    OnError = mMClient1Error
    OnStateChange = mMClient1StateChange
    OnDataSent = mMClient1DataSent
    OnDataReceived = mMClient1DataReceived
    Left = 32
    Top = 740
  end
end
