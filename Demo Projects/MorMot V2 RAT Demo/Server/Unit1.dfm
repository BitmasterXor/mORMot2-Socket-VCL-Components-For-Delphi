object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'MorMot Server Demo'
  ClientHeight = 450
  ClientWidth = 800
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
    Top = 290
    Width = 58
    Height = 15
    Caption = 'Server Log:'
  end
  object ListView1: TListView
    Left = 8
    Top = 48
    Width = 784
    Height = 233
    Columns = <
      item
        Caption = 'IP Address'
        Width = 120
      end
      item
        Caption = 'Client ID'
        Width = 100
      end
      item
        Caption = 'PC Name'
        Width = 150
      end
      item
        Caption = 'UserName'
        Width = 150
      end>
    ReadOnly = True
    RowSelect = True
    PopupMenu = PopupMenu1
    TabOrder = 0
    ViewStyle = vsReport
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 100
    Height = 32
    Caption = 'Start Server'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 114
    Top = 8
    Width = 100
    Height = 32
    Caption = 'Stop Server'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 311
    Width = 784
    Height = 131
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object PopupMenu1: TPopupMenu
    Left = 240
    Top = 16
    object S1: TMenuItem
      Caption = 'Send To ALL'
      OnClick = S1Click
    end
    object S2: TMenuItem
      Caption = 'Send To Selected Client'
      OnClick = S2Click
    end
  end
  object MorMotServer1: TMorMotServer
    Active = False
    OnHandleCommand = MorMotServer1HandleCommand
    OnConnect = MorMotServer1Connect
    OnDisconnect = MorMotServer1Disconnect
    OnReconnect = MorMotServer1Reconnect
    OnLog = MorMotServer1Log
    Left = 344
    Top = 16
  end
end
