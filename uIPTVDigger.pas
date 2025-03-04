(*****************************************************************************
The MIT License (MIT)

Copyright (c) 2020-2025 Laurent Meyer JsonX4@lmeyer.fr

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
******************************************************************************)
unit uIPTVDigger;

interface

uses
  Winapi.Windows, Winapi.Messages,System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, System.Net.HttpClient,
  System.Net.HttpClientComponent, Vcl.OleCtrls, Vcl.Menus, Vcl.Clipbrd
  , SysUtils, System.Net.URLClient
  , RTTI, uJX4Object, uJX4List
  , System.Generics.Collections, Vcl.AppEvnts
  ;

const
  APPTitle = 'IPTV Digger v1.0';
  APPFile  = 'IPTVDigger';
type
  TIPTVPrefs = class(TJX4Object)
    Ver: TValue;
    Edt: TValue;
    Ply: TValue;
    Url: TValue;
    Fav: TJX4ListOfValues;
  end;

  TIPTVChannel =  class
    Raw: string;
    Hash: string;
    Id: string;
    Name: string;
    Lang: string;
    Group: string;
    Cat: string;
    LogoURL: string;
    Origin: string;
    Title: string;
    IsMovie: Boolean;
    URL: string;
    UID: string;
  end;

  TIPTV = class
   Channels: TObjectList<TIPTVChannel>;
   Categories: TObjectDictionary<string, TObjectList<TIPTVChannel>>;
   Languages:  TObjectDictionary<string, TObjectList<TIPTVChannel>>;
   constructor  Create; overload;
   destructor   Destroy; override;
  end;

  TForm1 = class(TForm)
    StatusBar: TStatusBar;
    OpenDialog: TFileOpenDialog;
    Timer1: TTimer;
    PMEdit: TPopupMenu;
    PopupMenu11: TMenuItem;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Button1: TButton;
    CB_Players: TComboBox;
    Label5: TLabel;
    Edit1: TEdit;
    LB_Canals: TListBox;
    LB_Fav: TListBox;
    HTTP: TNetHTTPClient;
    PMRemove: TPopupMenu;
    Remove1: TMenuItem;
    Button2: TButton;
    Label1: TLabel;
    Label3: TLabel;
    Label2: TLabel;
    N1: TMenuItem;
    Import1: TMenuItem;
    Export1: TMenuItem;
    CopyTitle1: TMenuItem;
    N2: TMenuItem;
    CopyURL1: TMenuItem;
    Timer2: TTimer;
    Raw1: TMenuItem;
    Play1: TMenuItem;
    MPStatusBar: TPopupMenu;
    RefreshM3UData1: TMenuItem;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LB_CanalsDblClick(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure HTTPRequestCompleted(const Sender: TObject;
      const AResponse: IHTTPResponse);
    procedure Edit1DblClick(Sender: TObject);
    procedure PopupMenu11Click(Sender: TObject);
    procedure Remove1Click(Sender: TObject);
    procedure LB_FavDblClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure HTTPRequestException(const Sender: TObject;
      const AError: Exception);
    procedure Label3Click(Sender: TObject);
    procedure Label3MouseEnter(Sender: TObject);
    procedure Label3MouseLeave(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure CopyTitle1Click(Sender: TObject);
    procedure CopyURL1Click(Sender: TObject);
    procedure ApplicationEvents1Minimize(Sender: TObject);
    procedure TrayIcon1Click(Sender: TObject);
    procedure ApplicationEvents1Restore(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure Raw1Click(Sender: TObject);
    procedure Play(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure HTTPReceiveDataEx(const Sender: TObject; AContentLength,
      AReadCount: Int64; AChunk: Pointer; AChunkLength: Cardinal;
      var AAbort: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure LB_CanalsClick(Sender: TObject);
    procedure RefreshM3UData1Click(Sender: TObject);
  private
    { Private declarations }
    procedure Log(AStr: string; Panel: Integer = 0);
  public
    { Public declarations }
    TV: TIPTV;
    HTTPStream: TStringStream;
    CurURL: string;
    function  BuildM3UFile(Filename: string): Boolean;
    function  ShowChannels: Boolean;
  end;

var
  Form1: TForm1;

implementation
uses
  StrUtils
  , System.Character
  , ShellAPI
  , Registry
  , System.IOUtils
  , System.Hash
  , IdGlobal
  , IdURI
  , System.Types
  ;

{$R *.dfm}

procedure GetQuotedFields(AList: TStringList; const AStr: string; ASeparator: Char = ',');
var
  i, StartPos: Integer;
  InQuotes: Boolean;
  Field: string;
begin
  AList.Clear;
  i := 1;
  StartPos := 1;
  InQuotes := False;
  while i <= Length(AStr) do
  begin
     if AStr[i] = '"' then
      InQuotes := not InQuotes
    else if AStr[i] = ASeparator then
    begin
      if not InQuotes then
      begin
        Field := Trim(Copy(AStr, StartPos, i - StartPos));
        if (Length(Field) > 1) and (Field[1] = '"') and (Field[Length(Field)] = '"') then Field := Field.DeQuotedString('"');;
        AList.Add(Field);
        StartPos := i + 1;
      end;
    end;
    Inc(i);
  end;
  Field := Trim(Copy(AStr, StartPos, Length(AStr) - StartPos + 1));
  if (Length(Field) > 1) and (Field[1] = '"') and (Field[Length(Field)] = '"') then Field := Field.DeQuotedString('"');;
  if Length(Field) > 0 then AList.Add(Field);
end;

function XOREncrypt(Key: Byte; Str: string): string;
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create(Str);
  try
    for var i := 1 to Length(Str) do
    begin
      var a := Key xor ord(Str[i]);
      Key := a;
      Stream.WriteData(Byte(a));
    end;
    Result := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

function XORDecrypt(Key: Byte; Str: string): string;
var
  c: Byte;
  Stream: TStringStream;
begin
  Result := '';
  Stream := TStringStream.Create(Str);
  try
  while(Stream.Position < Stream.Size) do
  begin
    Stream.read(c, 1);
    var a := Key xor c;
    Key := c;
    Result := Result + chr(a);
  end;
  finally
    Stream.Free;
  end;
end;

procedure TForm1.ApplicationEvents1Minimize(Sender: TObject);
begin
  // Hide;
end;

procedure TForm1.ApplicationEvents1Restore(Sender: TObject);
begin
  Show;
  BringToFront;
end;

function TForm1.BuildM3UFile(Filename: string): Boolean;
var

  LLines: TStringList;
  LIdx : Integer;
  LSs: TStringStream;
  LCanal: TIPTVChannel;
  LCurCat: string;
  LParts : TArray<string>;
  LSubParts : TStringList;
  LTitleParts : TArray<string>;
  LFileName: string;
  LUri: TIdUri;
  LLang: TObjectList<TIPTVChannel>;
  LCat: TObjectList<TIPTVChannel>;
  LPair: TPair<string, TObjectList<TIPTVChannel>>;

begin
  Log ('Building Playlist');
  Result := False;
  LSs := Nil;
  LLines := Nil;
  LSubParts:= Nil;
  LIdx := 0;
  LCurCat := '';

  try
  try
    if not FileExists(Filename) then Exit;
    TV.Channels.Clear;
    TV.Categories.Clear;
    TV.Languages.Clear;

    StatusBar.Panels[0].Text := 'Building Playlist';
    LSs := TStringStream.Create('', TEncoding.UTF8);
    LSs.LoadFromFile(Filename);
    if LSs.size = 0 then Exit;
    LSubParts := TStringList.Create;
    LLines := TStringList.Create;
    LLines.StrictDelimiter := True;
    LLines.Text := LSs.DataString;
    if LLines[LIdx].ToUpper.Trim <> '#EXTM3U' then Exit;
    if Pos('#EXTINF:-1', LLines[LIdx + 1].ToUpper.Trim) <> 1 then Exit;
    LIdx := 1;
    while LIdx + 2 < LLines.count do
    begin
      if not LLines[LIdx].ToUpper.StartsWith('#EXTINF:-1') then
      begin
        Inc(LIdx, 2);
        Break
      end;

      LCanal := TIPTVChannel.Create;

      LCanal.URL := LLines[LIdx + 1];

      LParts    :=  LLines[LIdx].Split([','], '"', '"',  99);
      GetQuotedFields(LSubParts, LParts[0], ' ');

      LCanal.Raw := LLines[LIdx];
      LCanal.Id := LSubParts.Values['tvg-id'].DeQuotedString('"').Trim;
      LCanal.Name := LSubParts.Values['tvg-name'].DeQuotedString('"').Trim;
      LCanal.LogoUrl := LSubParts.Values['tvg-logo'].DeQuotedString('"').Trim;
      LCanal.Group := LSubParts.Values['group-title'].DeQuotedString('"').Trim;
      LCanal.IsMovie := not ExtractFileExt(LCanal.URL).Trim.IsEmpty;


      // Title

      If Length(LParts) = 1 then LCanal.Title := LCanal.Name else
      begin
        LCanal.Title := LParts[1];
        for var i := 2 to Length(LParts) - 1 do LCanal.Title :=  LCanal.Title + ' ' + LParts[i];
      end;
      LCanal.Title := LCanal.Title.Trim;
      LTitleParts := LCanal.Title.Trim.Split(['[', ']', '- ', '|', ':', '_'], 99);
      if Length(LTitleParts) <= 1 then LCanal.Title := LTitleParts[0].Trim else
      begin
        LCanal.Title := LTitleParts[1];
        for var i := 2 to Length(LTitleParts) - 1 do LCanal.Title := LCanal.Title + ', ' + LTitleParts[i];
      end;

      // Language

      var LangArray: TArray<string> :=  LCanal.Name.Split(['[', ']', ' - ', '|', ':', '_'], '"', '"', 999999);
      if (Length(LangArray) > 1) then // and (Length(LangArray[0].Trim) <= 12) then
      begin
        LCanal.Lang := '';  LangArray[0] := LangArray[0].Trim;
        for var i := 1  to Length(LangArray[0]) do if IsLetterOrDigit(LangArray[0][i]) then
          LCanal.Lang :=  LCanal.Lang + LangArray[0][i];
        if LCanal.lang.Trim.IsEmpty then LCanal.lang := '???';
        if TV.Languages.TryGetValue(LCanal.Lang, LLang)  then
        begin
          LLang.Add(LCanal);
        end else begin
          LLang := TObjectList<TIPTVChannel>.Create(False);
          LLang.Add(LCanal);
          TV.Languages.TryAdd(LCanal.Lang, LLang);
        end;
      end;

      // Categories & Save

      if LCanal.Name.Trim.StartsWith('#') then
      begin
        LCurCat := LCanal.name.trim.Replace('#','',[rfReplaceAll]).Replace('[','',[]).Replace(']','',[]).Replace('|','',[rfReplaceAll]).Trim;
        if not TV.Categories.TryGetValue(LCurCat, LCat)  then
        begin
          LCat := TObjectList<TIPTVChannel>.Create(False);
          LCat.Add(LCanal);
          TV.Categories.TryAdd(LCurCat, LCat);
        end else begin
          LCat.Add(LCanal);
        end;
        LCanal.Free;
      end else begin
        if not LCanal.Lang.IsEmpty then LCanal.Title := LCanal.Title.Join(': ', [LCanal.Lang, LCanal.Title]).Trim;
        LCanal.Cat := LCurCat;
        TV.Channels.Add(LCanal);
      end;

      Inc(LIdx, 2);
    end;

    Result := TV.Channels.Count > 0;
    if Result then
    begin
      CreateDir(TPath.GetHomePath + '\'+APPFile);
      LSs.SaveToFile(TPath.GetHomePath + '\' + APPFile +'\Playlist.m3u');
    end;

  finally
    LSubParts.Free;
    LSs.Free;
    LLInes.Free;
    if Result then
    Log ('Ready')
    else
      Log ('Failed, Please Get a Valid M3U');
  end;
  except
    Result := False;
  end;

end;

procedure TForm1.Log(AStr: string; Panel: Integer);
begin
  if csDestroying in Self.ComponentState then Exit;
  StatusBar.Panels[Panel].Text := AStr;
  StatusBar.Refresh; statusbar.Repaint;
end;

procedure TForm1.Play(Sender: TObject);
begin
  if LB_Canals.ItemIndex <> -1  then
    LB_CanalsDblClick(Self);
end;

procedure TForm1.PopupMenu11Click(Sender: TObject);
begin
  if LB_Fav.Items.IndexOf(Edit1.Text) = -1  then
   LB_Fav.AddItem(Edit1.Text, Nil);
end;

procedure TForm1.Raw1Click(Sender: TObject);
begin
 if LB_Canals.ItemIndex = - 1 then Exit;
    InputBox('Raw Data :', '', TIPTVChannel(LB_Canals.Items.Objects[LB_Canals.ItemIndex]).Raw);
end;

procedure TForm1.RefreshM3UData1Click(Sender: TObject);
begin
  Timer2Timer(Self);
end;

procedure TForm1.Remove1Click(Sender: TObject);
begin
  LB_Fav.Items.Delete(LB_Fav.ItemIndex);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  LFilename : string;
begin
  Timer1.Enabled := False;
  LFilename := TPath.GetHomePath + '\' + APPFile + '\Playlist.m3u';
  if FileExists(LFilename) then
  begin
    if BuildM3UFile(LFilename) then
    begin
      Log('Ready');
      ShowChannels;
    end else Log('Failed');
  end else
    PageControl1.ActivePage := TabSheet3;
end;


procedure TForm1.Timer2Timer(Sender: TObject);
begin
   if Timer2.Enabled and  not CurURL.Trim.IsEmpty then
   begin
     PageControl1.Enabled := FileExists(TPath.GetHomePath + '\' + APPFile +'\Playlist.m3u');
     Timer2.Enabled := False;
     HTTPStream.Clear;
     Log('Refreshing M3U Data');
     HTTP.Get(CurURL, HTTPStream);
   end;
end;

procedure TForm1.TrayIcon1Click(Sender: TObject);
begin
  Self.WindowState := wsNormal;
  Application.RestoreTopMosts;
end;

procedure TForm1.Label1Click(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar('mailto:IPTV@lmeyer.fr'), nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.Label3Click(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar('https://github.com/bnzbnz/' + APPFile), nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.Label3MouseEnter(Sender: TObject);
begin
  Screen.Cursor := crHandPoint;
end;

procedure TForm1.Label3MouseLeave(Sender: TObject);
begin
  Screen.Cursor := crDefault;
end;

procedure TForm1.LB_CanalsClick(Sender: TObject);
var
  Item : TIPTVChannel;
begin
  if LB_Canals.ItemIndex = - 1 then Exit;
  Item :=  TIPTVChannel(LB_Canals.Items.Objects[LB_Canals.ItemIndex]);
  if Assigned(Item) then
    LB_Canals.Hint := Item.Title + sLineBreak + Item.Group;
end;

procedure TForm1.LB_CanalsDblClick(Sender: TObject);

  function GetEnvVarValue(const VarName: string): string;
  var
    BufSize: Integer;
  begin
    BufSize := GetEnvironmentVariable(
      PChar(VarName), nil, 0);
    if BufSize > 0 then
    begin
     SetLength(Result, BufSize - 1);
     GetEnvironmentVariable(PChar(VarName),
       PChar(Result), BufSize);
    end
    else
     Result := '';
  end;

var
  Reg: TRegistry;
  LParam: string;
  LApp: string;
begin
  Reg := TRegistry.Create(KEY_READ);
  Reg.RootKey := HKEY_LOCAL_MACHINE;
  try
    if not Assigned(LB_Canals.iTems.Objects[LB_Canals.ItemIndex]) then Exit;
    case CB_Players.ItemIndex of
      0:  begin
            if not Reg.OpenKey('SOFTWARE\WOW6432Node\Microsoft\Windows\CurrentVersion\App Paths\wmplayer.exe', False) then Exit;
            LApp := Reg.ReadString('Path').Replace('%ProgramFiles(x86)%', GetEnvVarValue('ProgramFiles(x86)')) + '\wmplayer.exe';
            LParam := ' /play "' + TIPTVChannel(LB_Canals.iTems.Objects[LB_Canals.ItemIndex]).URL + '"';
            ShellExecute(0, '', PChar(LApp), PChar(LParam), nil , SW_SHOWNORMAL);
          end;
      1:  begin
            if not Reg.OpenKey('SOFTWARE\WOW6432Node\Microsoft\Windows\CurrentVersion\App Paths\vlc.exe', False) then Exit;
            LApp := Reg.ReadString('Path') + '\vlc.exe';
            LParam := ' --one-instance ' + TIPTVChannel(LB_Canals.iTems.Objects[LB_Canals.ItemIndex]).URL;
            ShellExecute(0, '', PChar(LApp), PChar(LParam), nil , SW_SHOWNORMAL);
          end;
      2:  begin
            if not Reg.OpenKey('SOFTWARE\WOW6432Node\Microsoft\Windows\CurrentVersion\App Paths\PotPlayerMini64.exe', False) then Exit;
            LApp := Reg.ReadString('Path').Trim.DeQuotedString('"');
            LParam := TIPTVChannel(LB_Canals.iTems.Objects[LB_Canals.ItemIndex]).URL + ' /current /play';
            ShellExecute(0, '', PChar(LApp), PChar(LParam), nil , SW_SHOWNORMAL);
          end;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TForm1.LB_FavDblClick(Sender: TObject);
begin
   Edit1.Text := LB_Fav.Items[LB_Fav.ItemIndex];
  PageControl1.ActivePage := TabSheet1;
  Self.ShowChannels;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  LSs: TStringStream;
begin
  LSs := Nil;
  try
    FormStyle := fsNormal;
    if OpenDialog.Execute then
    begin
      Screen.Cursor := crHourGlass;
      BuildM3UFile(OpenDialog.FileName);
      PageControl1.ActivePage := TabSheet1;
      if ShowChannels then CurURL := '';
      Screen.Cursor := crHourGlass;
   end;
  finally
    Screen.Cursor := crDefault;
    LSs.Free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  LURL: string;
  LUri: TidURI;
  TSl : TStringList;
begin
  try
    LURL := InputBox('Enter an MP3 Playlist URL :', 'URL : ', CurURL);
    try
      if not LURL.Trim.IsEmpty then
      begin
        LUri := TIdURI.Create(LURL);
        LURL := LURI.Protocol + '://' + LUri.Host + '/' + LUri.Document + '?username=';
        TSl := TStringList.Create(#0, '&', [soStrictDelimiter]);
        TSl.DelimitedText := LUri.Params;
        LUrl := LUrl + TSL.Values['username'] + '&password=' + TSL.Values['password'] + '&type=m3u_plus&output=ts';
        LUri.Free;
        LUri := TIdURI.Create(LUrl);
        Log('Downloading M3U (It may take a while...)');
        CurURL := LUrl;
        HTTP.Get(CurURL, HTTPStream);
        end;
    finally
      TSl.Free;
      LUri.Free
    end
  except
    Log('Invalid URL...');
  end
end;

procedure TForm1.CopyTitle1Click(Sender: TObject);
begin
  if LB_Canals.ItemIndex = - 1 then Exit;
  Clipboard.AsText := TIPTVChannel(LB_Canals.Items.Objects[LB_Canals.ItemIndex]).Title;
end;

procedure TForm1.CopyURL1Click(Sender: TObject);
begin
  if LB_Canals.ItemIndex = - 1 then Exit;
    Clipboard.AsText := TIPTVChannel(LB_Canals.Items.Objects[LB_Canals.ItemIndex]).Title;
end;

procedure TForm1.Edit1DblClick(Sender: TObject);
begin
  LB_Fav.AddItem(Edit1.Text, Nil);
end;

function Tform1.ShowChannels: Boolean;
var
  LWords: TStringList;
  Name: string;
begin
  Result := True;
  Log('Filtering');
  LWords := Nil;
  try
  try
  Self.PageControl1.ActivePage := Self.TabSheet1;
  Screen.Cursor := crHourGlass;
  LB_Canals.Clear;
  LB_Canals.Sorted := True;
  if (Trim(Edit1.Text) = '') then
  begin
    LB_Canals.Sorted := False;
    LB_Canals.AddItem('Please define a Query and Press "Enter"...',  Nil);
    LB_Canals.AddItem('Default : is Inclusive: DEMO IS',  Nil);
    LB_Canals.AddItem('- : is exclusive: DEMO SWE -AFR',  Nil);
    LB_Canals.AddItem('"..." : for sentences: "DEMO NL" -AFR',  Nil);
    LB_Canals.AddItem('">" : Title start with: "DEMO DE" >AFR',  Nil);
    LB_Canals.AddItem('"+tv" : only TV channels',  Nil);
    LB_Canals.AddItem('"+mov" : only Movies channels',  Nil);
    LB_Canals.AddItem('Ex: ''AMAZON PRIME FR:'' French Amazon Prime',Nil);
    LB_Canals.AddItem('Ex: ''CANAL AFR -sud -DAZN'' African Canal+', Nil);
    LB_Canals.AddItem('Ex: ''DAZN BE:'' = Belgium DAZN', Nil);
    LB_Canals.AddItem('Ex: ''"BEIN SPORT" CA:'' = Canadian BEIN SPORTS', Nil);
    LB_Canals.AddItem('...', Nil);
    Exit;
  end;
  LB_Canals.Items.BeginUpdate;
  LWords := TStringList.Create;
  GetQuotedFields(LWords, Edit1.Text, ' ');
  for var c in TV.Channels do
  begin
    If LB_Canals.Items.Count >= 40000 then Break;
    var Match : Boolean := True;
    for var w in LWords do
    begin
      var s := w.Trim.ToLower;
      if not s.IsEmpty then
      begin
        if not Match then Break;
        if s[1] = '>' then
        begin
          Match := Match and (Pos(Copy(s, 2), c.Title.ToLower) = 1);
        end else
        if s[1] = '-' then
        begin
          Match := Match and not (Pos( Copy(s, 2), c.Group.ToLower + ' ' + c.Title.ToLower) > 0);
          if Match then Continue;
        end;

        if Pos('+mov', s) > 0 then
        begin
          Match := Match and c.IsMovie;
          if Match then Continue;
        end
        else
       if Pos('+tv', s) > 0 then
          begin
            Match := Match and not c.IsMovie;
            if Match then Continue;
          end
        else
          Match := Match and (Pos(s, c.Group.ToLower + ' ' +  c.Title.ToLower) > 0);

        if not Match then Break;
      end;
    end;
      if Match and not c.Title.IsEmpty then
        if c.IsMovie then Self.LB_Canals.AddItem(c.Title + ' *', c) else LB_Canals.AddItem(c.Title, c);
  end;
  finally
    LB_Canals.Items.EndUpdate;
    LWords.Free;
    Screen.Cursor := crDefault;
  end;
  Log(Format('Ready %d Channels Filtered',[LB_Canals.Count]));
  except
    Result := False;
    Log('Filtering Failed');
  end;
end;

procedure TForm1.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  if ord(Key) = VK_RETURN then
  begin
    Key := #0;
    ShowChannels;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  PageControl1.ActivePage := TabSheet3;
  Log('Loading Default M3U File');
  Timer1.Enabled := True;
  Timer2Timer(Self);
end;

procedure TForm1.HTTPReceiveDataEx(const Sender: TObject; AContentLength,
  AReadCount: Int64; AChunk: Pointer; AChunkLength: Cardinal;
  var AAbort: Boolean);
begin
  AAbort := (Self.ComponentState = [csDestroying]);
  Log(Trunc((AReadCount / AContentLength) * 100).ToString + '%', 1);
end;

procedure TForm1.HTTPRequestCompleted(const Sender: TObject;
  const AResponse: IHTTPResponse);
begin
  Log('', 1);
  if AResponse.StatusCode <> 200 then
  begin
    Log('Failed');
    HTTPStream.Clear;
  end;
  CreateDir(TPath.GetHomePath + '\' + APPFile );
  HTTPStream.SaveToFile(TPath.GetHomePath + '\' + APPFile + '\Playlist.m3u');
  HTTPStream.Clear;
   Log('Building Playlist');
  Screen.Cursor := crHourGlass;
  if BuildM3UFile(TPath.GetHomePath + '\' + APPFile + '\Playlist.m3u') then
  begin
    ShowChannels;
    Log('Ready: ' + TV.Channels.Count.ToString + ' Total Channels');
  end else
    Log('Failed');
  Screen.Cursor := crDefault;
  PageControl1.Enabled := True;
  Timer2.Enabled := True;
end;

procedure TForm1.HTTPRequestException(const Sender: TObject;
  const AError: Exception);
begin
  if csFreeNotification in Form1.ComponentState then Exit;
  Log('Failed'); Log('', 1);
  Timer2.Enabled := True;
  PageControl1.Enabled := True;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  Timer2.Enabled := False;
  Try HTTP.Get(''); except end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  P: TIPTVPrefs;
  F: string;
  S: TValue;
begin
  P := Nil;
  Caption := APPTitle;
  TV := TIPTV.Create;
  HTTPStream := TStringStream.Create('', TEncoding.UTF8, True);
  F := TPath.GetHomePath + '\' + APPFile + '\Settings.json';
  if FileExists(F) then
  begin
    P := TJX4Object.LoadFromJSONFile<TIPTVPrefs>(F);
    for S in P.Fav do
      LB_Fav.AddItem(S.AsString, Nil);
    Edit1.Text := P.Edt.AsString;
    CB_PLayers.ItemIndex := P.Ply.AsInteger;
    CurURL := XORDecrypt($BE, P.Url.AsString);
  end else
    PageControl1.ActivePage := TabSheet3;
  P.Free;

  var LScreenWidth := GetSystemMetrics(SM_CXSCREEN);
  var LScreenHeight := GetSystemMetrics(SM_CYSCREEN);
  var LAppBarData: TAppBarData;
  LAppBarData.cbSize := SizeOf(TAppBarData);
  SHAppBarMessage(ABM_GETTASKBARPOS, LAppBarData);
  Form1.Left := LAppBarData.rc.Right - Form1.Width;
  Form1.Top := LAppBarData.rc.Top - Form1.Height;
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  P: TIPTVPrefs;
  F: string;
  S: string;
begin
  CreateDir(TPath.GetHomePath + '\' + APPFile );
  F := TPath.GetHomePath + '\' + APPFile + '\Settings.json';
  P := TIPTVPrefs.Create;
  P.Ver := 1;
  for S in LB_Fav.Items do P.Fav.Add(S);
  P.Edt := Trim(Edit1.Text);
  P.Ply := CB_PLayers.ItemIndex;
  P.Url := XOREncrypt($BE, CurURL);
  P.SaveToJSONFile(F, TEncoding.UTF8);
  P.Free;
  HTTPStream.Free;
  TV.Free;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  LB_Canals.Columns := Trunc(LB_Canals.Width / 256) ;
end;

{ TIPTV }

constructor TIPTV.Create;
begin
 Channels   := TObjectList<TIPTVChannel>.Create(True);
 Categories := TObjectDictionary<string, TObjectList<TIPTVChannel>>.Create([doOwnsValues]);
 Languages  := TObjectDictionary<string, TObjectList<TIPTVChannel>>.Create([doOwnsValues]);
end;

destructor TIPTV.Destroy;
begin
  Languages.Free;
  Categories.Free;
  Channels.Free;
  inherited;
end;

end.
