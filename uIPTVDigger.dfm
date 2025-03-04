object Form1: TForm1
  Left = 0
  Top = 0
  ActiveControl = Edit1
  BorderIcons = [biSystemMenu, biMinimize]
  ClientHeight = 233
  ClientWidth = 290
  Color = clBurlywood
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  TextHeight = 15
  object StatusBar: TStatusBar
    Left = 0
    Top = 214
    Width = 290
    Height = 19
    Panels = <
      item
        Alignment = taCenter
        Text = 'Status'
        Width = 220
      end
      item
        Alignment = taRightJustify
        Width = 100
      end>
    PopupMenu = MPStatusBar
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 290
    Height = 214
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Search'
      DesignSize = (
        282
        184)
      object Edit1: TEdit
        Left = 3
        Top = 3
        Width = 276
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        ParentShowHint = False
        PopupMenu = PMEdit
        ShowHint = False
        TabOrder = 0
        OnDblClick = Edit1DblClick
        OnKeyPress = Edit1KeyPress
      end
      object LB_Canals: TListBox
        Left = 4
        Top = 29
        Width = 275
        Height = 153
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = 4
        DoubleBuffered = True
        ExtendedSelect = False
        ItemHeight = 15
        ParentDoubleBuffered = False
        ParentShowHint = False
        PopupMenu = PMEdit
        ScrollWidth = 1
        ShowHint = False
        Sorted = True
        TabOrder = 1
        OnClick = LB_CanalsClick
        OnDblClick = LB_CanalsDblClick
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Favorites'
      ImageIndex = 1
      object LB_Fav: TListBox
        Left = 0
        Top = 0
        Width = 282
        Height = 184
        Align = alClient
        ItemHeight = 15
        PopupMenu = PMRemove
        Sorted = True
        TabOrder = 0
        OnDblClick = LB_FavDblClick
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Settings'
      ImageIndex = 2
      object Label5: TLabel
        Left = 10
        Top = 16
        Width = 38
        Height = 15
        Caption = 'Player :'
      end
      object Label1: TLabel
        Left = 96
        Top = 129
        Width = 88
        Height = 15
        Caption = 'IPTV@lmeyer.fr'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        OnClick = Label1Click
      end
      object Label3: TLabel
        Left = 32
        Top = 166
        Width = 226
        Height = 15
        Caption = 'https://github.com/bnzbnz/IPTVRemote'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold, fsItalic]
        ParentColor = False
        ParentFont = False
        OnClick = Label3Click
        OnMouseEnter = Label3MouseEnter
        OnMouseLeave = Label3MouseLeave
      end
      object Label2: TLabel
        Left = 104
        Top = 148
        Width = 66
        Height = 15
        Caption = 'MIT License'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Button1: TButton
        Left = 70
        Top = 60
        Width = 139
        Height = 23
        Caption = 'Load IPTV M3U File'
        TabOrder = 0
        OnClick = Button1Click
      end
      object CB_Players: TComboBox
        Left = 54
        Top = 13
        Width = 197
        Height = 23
        AutoCompleteDelay = 250
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 1
        Text = 'Media Player (Default)'
        Items.Strings = (
          'Media Player (Default)'
          'VLC'
          'PotPlayer')
      end
      object Button2: TButton
        Left = 70
        Top = 89
        Width = 139
        Height = 25
        Caption = 'Download M3U Playlist'
        TabOrder = 2
        OnClick = Button2Click
      end
    end
  end
  object OpenDialog: TFileOpenDialog
    DefaultExtension = 'm3u'
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'M3U File'
        FileMask = '*.m3u'
      end
      item
        DisplayName = 'All Files'
        FileMask = '*.*'
      end>
    Options = [fdoStrictFileTypes]
    Title = 'M3U File Selection'
    Left = 17
    Top = 66
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 43
    Top = 83
  end
  object PMEdit: TPopupMenu
    Left = 51
    Top = 131
    object Play1: TMenuItem
      Caption = 'Play'
      OnClick = Play
    end
    object PopupMenu11: TMenuItem
      Caption = 'Query to Favorites'
      OnClick = PopupMenu11Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object CopyTitle1: TMenuItem
      Caption = 'Copy Title'
      OnClick = CopyTitle1Click
    end
    object CopyURL1: TMenuItem
      Caption = 'Copy URL'
      OnClick = CopyURL1Click
    end
    object Raw1: TMenuItem
      Caption = 'Show Raw Data'
      OnClick = Raw1Click
    end
  end
  object HTTP: TNetHTTPClient
    Asynchronous = True
    ConnectionTimeout = 15000
    SendTimeout = 15000
    ResponseTimeout = 1200000
    ProtocolVersion = HTTP_1_1
    UserAgent = 'VLC/3.0.4 LibVLC/3.0.4'
    OnRequestCompleted = HTTPRequestCompleted
    OnRequestException = HTTPRequestException
    OnReceiveDataEx = HTTPReceiveDataEx
    Left = 204
    Top = 106
  end
  object PMRemove: TPopupMenu
    Left = 132
    Top = 66
    object Remove1: TMenuItem
      Caption = 'Remove'
      OnClick = Remove1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Import1: TMenuItem
      Caption = 'Import'
      Enabled = False
    end
    object Export1: TMenuItem
      Caption = 'Export'
      Enabled = False
    end
  end
  object Timer2: TTimer
    Interval = 36000000
    OnTimer = Timer2Timer
    Left = 116
    Top = 130
  end
  object MPStatusBar: TPopupMenu
    Left = 188
    Top = 34
    object RefreshM3UData1: TMenuItem
      Caption = 'Refresh M3U Data'
      OnClick = RefreshM3UData1Click
    end
  end
end
