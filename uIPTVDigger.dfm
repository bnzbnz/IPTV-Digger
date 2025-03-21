object IPTVForm: TIPTVForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'IPTVForm'
  ClientHeight = 383
  ClientWidth = 292
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object PC: TPageControl
    Left = 0
    Top = 0
    Width = 292
    Height = 346
    ActivePage = TabSheet3
    Align = alClient
    TabOrder = 0
    object TabSheet2: TTabSheet
      Caption = 'Query'
      ImageIndex = 1
      object EQuery: TEdit
        Left = 0
        Top = 0
        Width = 284
        Height = 23
        Align = alTop
        PopupMenu = PPQuery
        TabOrder = 0
        OnChange = EQueryChange
        OnKeyPress = EQueryKeyPress
      end
      object LB_Canals: TListBox
        Left = 0
        Top = 23
        Width = 284
        Height = 293
        Align = alClient
        ItemHeight = 15
        ParentShowHint = False
        PopupMenu = PPQuery
        ShowHint = False
        TabOrder = 1
        OnDblClick = LB_CanalsDblClick
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Favorites'
      ImageIndex = 2
      object LB_Fav: TListBox
        Left = 0
        Top = 0
        Width = 284
        Height = 316
        Align = alClient
        ItemHeight = 15
        PopupMenu = PPFav
        Sorted = True
        TabOrder = 0
        OnDblClick = LB_FavDblClick
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'History'
      ImageIndex = 3
      object LB_History: TListBox
        Left = 0
        Top = 0
        Width = 284
        Height = 316
        Align = alClient
        ItemHeight = 15
        MultiSelect = True
        PopupMenu = PPHisto
        TabOrder = 0
        OnDblClick = LB_HistoryDblClick
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Settings'
      DesignSize = (
        284
        316)
      object Label1: TLabel
        Left = 3
        Top = 3
        Width = 170
        Height = 15
        Caption = 'M3U URL : (Hit Enter to validate)'
      end
      object Label2: TLabel
        Left = 3
        Top = 116
        Width = 68
        Height = 15
        Caption = 'Video Player:'
      end
      object Label3: TLabel
        Left = 3
        Top = 86
        Width = 71
        Height = 15
        Caption = 'Max Results  :'
      end
      object Label4: TLabel
        Left = 3
        Top = 56
        Width = 104
        Height = 15
        Caption = 'M3U Refresh (mns.)'
      end
      object EM3U: TEdit
        Left = 3
        Top = 24
        Width = 278
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnKeyPress = EM3U2KeyPressed
      end
      object CBPlayers: TComboBox
        Left = 77
        Top = 113
        Width = 204
        Height = 23
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemIndex = 0
        TabOrder = 1
        Text = 'Media Player'
        Items.Strings = (
          'Media Player'
          'VLC'
          'PotPlayer')
      end
      object SPMaxVal: TSpinEdit
        Left = 77
        Top = 83
        Width = 204
        Height = 24
        Anchors = [akLeft, akTop, akRight]
        MaxValue = 20000
        MinValue = 100
        TabOrder = 2
        Value = 500
      end
      object SERefresh: TSpinEdit
        Left = 121
        Top = 53
        Width = 160
        Height = 24
        Anchors = [akLeft, akTop, akRight]
        MaxValue = 9999
        MinValue = 15
        TabOrder = 3
        Value = 60
      end
      object CBOnTop: TCheckBox
        Left = 76
        Top = 150
        Width = 97
        Height = 17
        Caption = 'Always On Top'
        Checked = True
        State = cbChecked
        TabOrder = 4
        OnClick = CBOnTopClick
      end
    end
  end
  object ProgressBar: TProgressBar
    Left = 0
    Top = 346
    Width = 292
    Height = 18
    Align = alBottom
    Smooth = True
    Style = pbstMarquee
    MarqueeInterval = 1
    TabOrder = 1
    Visible = False
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 364
    Width = 292
    Height = 19
    Panels = <
      item
        Alignment = taCenter
        Width = 50
      end>
    PopupMenu = PPStatusBar
  end
  object PPQuery: TPopupMenu
    Left = 180
    Top = 178
    object AddQuerytoFavorites1: TMenuItem
      Caption = 'Add Query to Favorites'
      OnClick = AddQuerytoFavorites1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object CopyURL1: TMenuItem
      Caption = 'Copy URL'
      OnClick = CopyURL1Click
    end
  end
  object PPFav: TPopupMenu
    Left = 180
    Top = 58
    object Open1: TMenuItem
      Caption = 'Open'
      OnClick = Open1Click
    end
    object Remove1: TMenuItem
      Caption = 'Remove'
      OnClick = Remove1Click
    end
  end
  object PPStatusBar: TPopupMenu
    Left = 116
    Top = 210
    object RefreshM3UData1: TMenuItem
      Caption = 'Refresh M3U Data'
      OnClick = RefreshM3UData1Click
    end
  end
  object PPHisto: TPopupMenu
    Left = 628
    Top = 458
    object Open2: TMenuItem
      Caption = 'Open'
      OnClick = LB_HistoryDblClick
    end
    object Remove2: TMenuItem
      Caption = 'Remove'
      OnClick = Remove2Click
    end
    object RemoveAll1: TMenuItem
      Caption = 'Remove All'
      OnClick = RemoveAll1Click
    end
  end
end
