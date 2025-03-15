(*****************************************************************************
The MIT License (MIT)

Copyright (c) 2020-2025 Laurent Meyer JsonX3@ea4d.com

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
*******************************************************************************)
unit uIPTVDigger;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,System.Win.TaskbarCore, Vcl.Taskbar,
  Vcl.Menus, Vcl.Samples.Spin
  , uThreadedHTTPGet, uDigThread
  , RTTI, uJX4Object, uJX4List
  ;


const
  APPTitle = 'IPTV Digger v1.0 Beta 4';
  APPFile  = 'IPTVDigger';

type

  TSettings = class(TJX4Object)
    [JX4Defaault('')]
    M3U: TValue;
    [JX4Defaault('')]
    Query: TValue;
    [JX4Defaault(0)]
    Player: TValue;
    [JX4Defaault(400)]
    MaxValues: TValue;
    [JX4Defaault(60)]
    Refresh: TValue;
    [JX4Defaault(True)]
    OnTop: TValue;
    Favorites: TJX4ListOfValues;
  end;

  TIPTVForm = class(TForm)
    PC: TPageControl;
    TabSheet1: TTabSheet;
    ProgressBar: TProgressBar;
    TabSheet2: TTabSheet;
    EQuery: TEdit;
    LB_Canals: TListBox;
    TabSheet3: TTabSheet;
    LB_Fav: TListBox;
    StatusBar: TStatusBar;
    EM3U: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    CBPlayers: TComboBox;
    Label3: TLabel;
    SPMaxVal: TSpinEdit;
    PPQuery: TPopupMenu;
    AddQuerytoFavorites1: TMenuItem;
    PPFav: TPopupMenu;
    Open1: TMenuItem;
    Remove1: TMenuItem;
    Label4: TLabel;
    SERefresh: TSpinEdit;
    CBOnTop: TCheckBox;
    PPStatusBar: TPopupMenu;
    RefreshM3UData1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LB_FavDblClick(Sender: TObject);
    procedure LB_CanalsDblClick(Sender: TObject);
    procedure EM3U2KeyPressed(Sender: TObject; var Key: Char);
    procedure AddQuerytoFavorites1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Remove1Click(Sender: TObject);
    procedure CBOnTopClick(Sender: TObject);
    procedure EQueryKeyPress(Sender: TObject; var Key: Char);
    procedure RefreshM3UData1Click(Sender: TObject);
  private
    { Private declarations }
    procedure Log(AMsg: string; APanel: integer = 0);
    procedure Play(URL: string; Player: Integer);
    procedure OnFoundChannel(ADigger: TDigThread; ATitle: string; AIndex: integer);
  public
    CList: TStringList;
    HTTPUrl: string;
    HTTPStream: TStringStream;
    HTTPRaw: THTTPGetThread;
    Digger: TDigThread;

    procedure HTTPProgress(ASender: THTTPGetThread; Progress: Integer);
    procedure HTTPCompleted(ASender: THTTPGetThread; IsTerminated: Boolean; IsAsync: Boolean);
    procedure DownloadM3U(AUrl: string);
    procedure ShowChannels;
  end;

var
  IPTVForm: TIPTVForm;

implementation
uses
    ShellAPI
  , System.IOUtils
  , System.Character
  , IdUri
  , Registry
  ;
{$R *.dfm}

// Simple obfucation
function XOREncrypt(AKey: Byte; AStr: string): string;
var
  Stream: TStringStream;
  LIdx: Integer;
  LVal: Byte;
begin
  Stream := TStringStream.Create(AStr);
  try
    for LIdx := 1 to Length(AStr) do
    begin
      LVal := AKey xor ord(AStr[LIdx]);
      AKey := LVal;
      Stream.WriteData(Byte(Lval));
    end;
    Result := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

function XORDecrypt(AKey: Byte; AStr: string): string;
var
  Stream: TStringStream;
  LIdx: Integer;
  LVal, LVal2: Byte;
begin
  Result := '';
  Stream := TStringStream.Create(AStr);
  try
  while(Stream.Position < Stream.Size) do
  begin
    Stream.read(LVal, 1);
    LVal2 := AKey xor LVal;
    AKey := LVal;
    Result := Result + Chr(LVal2);
  end;
  finally
    Stream.Free;
  end;
end;

procedure TIPTVForm.Log(AMsg: string; APanel: integer);
begin
  StatusBar.Panels[APanel].Text:= AMsg;
end;

procedure TIPTVForm.Open1Click(Sender: TObject);
begin
  LB_FavDblClick(Sender);
end;

procedure TIPTVForm.AddQuerytoFavorites1Click(Sender: TObject);
begin
  LB_Fav.AddItem(Trim(EQuery.text), Nil);
end;

procedure TIPTVForm.CBOnTopClick(Sender: TObject);
begin
  if CBOnTop.Checked then
    Self.FormStyle := fsStayOnTop
  else
    Self.FormStyle := fsNormal;
end;

procedure TIPTVForm.DownloadM3U(AUrl: string);
begin
  if HTTPUrl.IsEmpty then Exit;
  HTTPStream.Clear;
  HTTPRaw.Free;
  ProgressBar.Visible := True;
  ProgressBar.Style := pbstMarquee;
  HTTPRaw := THTTPGetThread.Create(AUrl, HTTPStream, Nil, 'IPTVSmartersPro', HTTPCompleted, HTTPProgress);
end;

procedure TIPTVForm.EM3U2KeyPressed(Sender: TObject; var Key: Char);
var
  LURL: string;
  LUri: TidURI;
  LSl : TStringList;
  LPort: string;
begin
  LSl := Nil;
  LUri := Nil;
  try
    if ord(Key) = VK_RETURN  then
    begin
      Key := #0;
      if Trim(EM3U.Text).IsEmpty then Exit;
      LUri := TIdURI.Create(Trim(EM3U.Text));
      if LUri.Port.IsEmpty then
        LURL := LURI.Protocol + '://' + LUri.Host + '/' + LUri.Document + '?username='
      else
        LURL := LURI.Protocol + '://' + LUri.Host + ':' + LUri.Port + '/' + LUri.Document + '?username=';
      LSl := TStringList.Create(#0, '&', [soStrictDelimiter]);
      LSl.DelimitedText := LUri.Params;
      HTTPUrl := LUrl + LSl.Values['username'] + '&password=' + LSl.Values['password'] + '&type=m3u_plus&output=ts';
      DownloadM3U(HTTPUrl);
    end;
  finally
    LSl.Free;
    LUri.Free
  end;
end;

procedure TIPTVForm.EQueryKeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key) = VK_RETURN then Key := #0;
  ShowChannels;
end;

procedure TIPTVForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  LSettings: TSettings;
  LStr: string;
begin
  LSettings := TSettings.Create;
  LSettings.M3U := XOREncrypt($B9, HTTPUrl);
  LSettings.Query := Trim(EQuery.Text);
  LSettings.Player := CBPlayers.ItemIndex;
  LSettings.MaxValues := SPMaxVal.Value;
  LSettings.Refresh := SERefresh.Value;
  LSettings.OnTop := CBOnTop.Checked;
  for LStr in LB_Fav.Items do LSettings.Favorites.Add(LStr);
  LSettings.SaveToJSONFile(TPath.GetHomePath + '\' + APPFile + '\Settings.json', TEncoding.UTF8);
  LSettings.Free;
end;

procedure TIPTVForm.FormCreate(Sender: TObject);
var
  LSettings: TSettings;
  LTVal: TValue;
begin
  Log('Starting Up...');
  Caption := APPTitle;
  CList := TStringList.Create;
  HTTPStream := TStringStream.Create;
  HTTPRaw    := Nil;
  Digger := TDigThread.Create(OnFoundChannel);
  CreateDir(TPath.GetHomePath + '\' + APPFile );

  if FileExists(TPath.GetHomePath + '\' + APPFile + '\Settings.json') then
  begin
    LSettings := TJX4Object.LoadFromJSONFile<TSettings>(TPath.GetHomePath + '\' + APPFile + '\Settings.json');
    try
      try
        HTTPUrl := XORDecrypt($B9, LSettings.M3U.AsString);
        EM3U.Text := HTTPUrl;
        EQuery.Text := LSettings.Query.AsString;
        CBPlayers.ItemIndex := LSettings.Player.AsInteger;
        SPMaxVal.Value := LSettings.MaxValues.AsInteger;
        CBOnTop.Checked := LSettings.OnTop.AsBoolean;
        CBOnTopClick(Self);
        if not LSettings.Refresh.IsEmpty then SERefresh.Value := LSettings.Refresh.AsInteger;
        for LTVal in LSettings.Favorites do LB_Fav.AddItem(LTVal.AsString, Nil);
      except end;
    finally
      LSettings.Free;
    end;

    if FileExists(TPath.GetHomePath + '\' + APPFile + '\Playlist.m3x') then
      CList.LoadFromFile(TPath.GetHomePath + '\' + APPFile + '\Playlist.m3x', TEncoding.UTF8);

    PC.ActivePage := TabSheet2;
    Log('Connecting to M3U Server, it may take a while');
    DownloadM3U(HTTPUrl);
    ShowChannels;

  end else begin
    ShowMessage('IPTV Digger does not offer any IPTV channel services. You have to provide your own IPTV M3U URL');
    Self.PC.ActivePage := TabSheet1;
  end;

  var LAppBarData: TAppBarData;
  LAppBarData.cbSize := SizeOf(TAppBarData);
  SHAppBarMessage(ABM_GETTASKBARPOS, LAppBarData);
  Left := LAppBarData.rc.Right - Width;
  Top := LAppBarData.rc.Top - Height;
end;

procedure TIPTVForm.FormDestroy(Sender: TObject);
begin
  Digger.Free;
  HTTPRaw.Free;
  HTTPStream.Free;
  CList.Free;
end;

procedure TIPTVForm.HTTPProgress(ASender: THTTPGetThread; Progress: Integer);
begin
  ProgressBar.Visible := True;
  ProgressBar.Style := pbstNormal;
  ProgressBar.Position := Progress;
  Log(Format('Loading M3U :  %d %%', [Progress]));
end;

procedure TIPTVForm.LB_CanalsDblClick(Sender: TObject);
var
  LIdx: integer;
  LURL: string;
begin
  LIdx := LB_Canals.ItemIndex;
  if LIdx = -1 then Exit;
  LIdx := Integer(LB_Canals.ItEms.Objects[LIdx]);
  if LIdx = -1 then Exit;
  LURL := XORDecrypt($AF, CList[LIdx + 2]);
  Play(LURL, CBPlayers.ItemIndex);
end;

procedure TIPTVForm.LB_FavDblClick(Sender: TObject);
var
  LIdx: integer;
begin
  LIdx := LB_Fav.ItemIndex;
  if LIdx = -1 then Exit;
  EQuery.Text := LB_Fav.Items[LIdx];
  Self.PC.ActivePage := TabSheet2;
  ShowChannels;
end;

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

function Parse(ALine: string; var  ATitle: string; var ALang: string): boolean;
var
  LParts : TArray<string>;
  LTitleParts : TArray<string>;
  LSubParts: TStringList;
  LLangParts: TArray<string>;
  LName: string;
begin
  Result := False;
  LSubParts := Nil;
  try
  try
   LParts    :=  ALine.Split([','], '"', '"',  99);
   LSubParts := TStringList.Create;
   GetQuotedFields(LSubParts, LParts[0], ' ');
   LName := LSubParts.Values['tvg-name'].DeQuotedString('"').Trim;
   if LName.Trim.StartsWith('#') then Exit;

   If Length(LParts) = 1 then  ATitle := LName else
    begin
      ATitle := LParts[1];
      for var i := 2 to Length(LParts) - 1 do ATitle :=  ATitle + ' ' + LParts[i];
    end;
    ATitle := ATitle.Trim;
    LTitleParts := ATitle.Trim.Split(['[', ']', '- ', '|', ':', '_'], 99);
    if Length(LTitleParts) <= 1 then ATitle := LTitleParts[0].Trim else
    begin
      ATitle := LTitleParts[1];
      for var i := 2 to Length(LTitleParts) - 1 do ATitle := ATitle + ', ' + LTitleParts[i];
    end;

    LLangParts :=  LName.Split(['[', ']', ' - ', '|', ':', '_'], '"', '"', 99);
    if (Length(LLangParts) > 1) then // and (Length(LangArray[0].Trim) <= 12) then
    begin
      ALang := '';  LLangParts[0] := LLangParts[0].Trim;
      for var i := 1  to Length(LLangParts[0]) do if IsLetterOrDigit(LLangParts[0][i]) then
        ALang :=  ALang + LLangParts[0][i];
      if ALang.Trim.IsEmpty then
      begin
        if (Length(LLangParts) > 1) and LLangParts[0].Trim.IsEmpty then
          ALang := LLangParts[1]
        else
          ALang := '???';
      end;

      Result := True;
    end;
    ATitle := ATitle.Trim;
    ALang := ALang.Trim;
  finally
    LSubParts.Free;
  end;
  except
    Result := False;
  end;
end;

procedure TIPTVForm.HTTPCompleted(ASender: THTTPGetThread; IsTerminated: Boolean; IsAsync: Boolean);
var
  LIdx: integer;
  LList: TStringList;
  LTempList: TStringList;
  LTitle, LLang: string;
begin
  if IsAsync then
  begin
    if not Assigned(ASender.Result) then Exit;
    if ASender.Result.StatusCode <> 200 then Exit;
    try
      LList := Nil; LTempList := Nil;
      if not (TStringStream(THTTPGetThread(ASender).Stream).ReadString(8) = '#EXTM3U'+#$A) then Exit;
      LTempList:= TStringList.Create;
      LTempList.StrictDelimiter := False;
      try
      try
        LTempList.Text := UTF8Decode(TStringStream(THTTPGetThread(ASender).Stream).ReadString( THTTPGetThread(ASender).Stream.Size - 8 ));
        TStringStream(THTTPGetThread(ASender).Stream).Clear;

        LList :=  TStringList.Create;;
        THTTPGetThread(ASender).UserObj := LList;
        LIdx := 0;
        while LIdx < LTempList.count - 2  do
        begin
          if LTempList[LIdx].ToUpper.StartsWith('#EXTINF:-1') then
          begin
            if Parse(LTempList[LIdx], LTitle, LLang) then
            begin
              LList.Add(LLang + ': ' + LTitle);
              LList.Add(LTempList[LIdx]);
              LList.Add(XOREncrypt($AF, LTempList[LIdx + 1]));
            end;
          end;
          Inc(LIdx, 2);
        end;
        LList.SaveToFile(TPath.GetHomePath + '\' + APPFile + '\Playlist.m3x', TEncoding.UTF8);
      except
        FreeAndNil(THTTPGetThread(ASender).UserObj);
      end;
      finally
        TStringStream(THTTPGetThread(ASender).Stream).Clear;
        LTempList.Free;
      end;
    except
    end;
  end else begin // IsAsync
    if Assigned(ASender.Result) and (ASender.Result.StatusCode = 200) then
    begin
       HTTPUrl := ASender.URL;
       LList := TStringList(THTTPGetThread(ASender).UserObj);
       CList.Free; CList := LList;
       ShowChannels;
       Log('');
    end else
      FreeAndNil(THTTPGetThread(ASender).UserObj);
    ProgressBar.Visible := False;
  end;
end;

procedure TIPTVForm.OnFoundChannel(ADigger: TDigThread; ATitle: string; AIndex: integer);
begin
  LB_Canals.AddItem(ATitle, Tobject(AIndex));
  StatusBar.Panels[0].Text := LB_Canals.Count.ToString + ' Channels Found';
end;

procedure TIPTVForm.ShowChannels;
var
  LIdx: integer;
  LWords: TStringList;
begin
  LB_Canals.Clear;
  LB_Canals.Sorted := True;
  if CList.Count = 0 then Exit;
  if (Trim(EQuery.Text).IsEmpty) then
  begin
    LB_Canals.Sorted := False;
    LB_Canals.AddItem('Please define a Query and Press "Enter"...',  Nil);
    LB_Canals.AddItem('Default : is Inclusive: DEMO EN:',  Nil);
    LB_Canals.AddItem('- : is exclusive: DEMO SWE -AFR',  Nil);
    LB_Canals.AddItem('"" : for sentences: "DEMO NL" -AFR',  Nil);
    LB_Canals.AddItem('Ex: ''AMAZON PRIME FR:'' French Amazon Prime',Nil);
    LB_Canals.AddItem('Ex: ''CANAL AFR'' African Canal+', Nil);
    LB_Canals.AddItem('Ex: ''DAZN BE:'' = Belgium DAZN', Nil);
    LB_Canals.AddItem('Ex: ''"BEIN SPORT" UK:'' =  UK BEIN SPORTS', Nil);
    LB_Canals.AddItem('Ex: ''"SKY SPORT" UK: F1'' =  UK SKY SPORTS, F1 channels', Nil);
    LB_Canals.AddItem('...', Nil);
    Exit;
  end;
  Digger.Dig(CList, Trim(EQuery.Text).ToLower, SPMaxVal.Value);
end;

procedure TIPTVForm.Play(URL: string; Player: Integer);

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
    if URL.Trim.IsEmpty then Exit;
    case Player of
      0:  begin
            if not Reg.OpenKey('SOFTWARE\WOW6432Node\Microsoft\Windows\CurrentVersion\App Paths\wmplayer.exe', False) then Exit;
            LApp := Reg.ReadString('Path').Replace('%ProgramFiles(x86)%', GetEnvVarValue('ProgramFiles(x86)')) + '\wmplayer.exe';
            LParam := ' /play "' + URL + '"';
            ShellExecute(0, '', PChar(LApp), PChar(LParam), nil , SW_SHOWNORMAL);
          end;
      1:  begin
            if not Reg.OpenKey('SOFTWARE\WOW6432Node\Microsoft\Windows\CurrentVersion\App Paths\vlc.exe', False) then Exit;
            LApp := Reg.ReadString('Path') + '\vlc.exe';
            LParam := ' --one-instance ' + URL;
            if (GetKeyState(VK_CONTROL)<0) then LParam := URL;
            ShellExecute(0, '', PChar(LApp), PChar(LParam), nil , SW_SHOWNORMAL);
          end;
      2:  begin
            if not Reg.OpenKey('SOFTWARE\WOW6432Node\Microsoft\Windows\CurrentVersion\App Paths\PotPlayerMini64.exe', False) then Exit;
            LApp := Reg.ReadString('Path').Trim.DeQuotedString('"');
            LParam := URL + ' /current /play';
            if (GetKeyState(VK_CONTROL)<0) then LParam := URL + ' /play';
            ShellExecute(0, '', PChar(LApp), PChar(LParam), nil , SW_SHOWNORMAL);
          end;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TIPTVForm.RefreshM3UData1Click(Sender: TObject);
begin
  DownloadM3U(HTTPUrl);
end;

procedure TIPTVForm.Remove1Click(Sender: TObject);
begin
  if LB_Fav.ItemIndex > -1 then
    LB_Fav.Items.Delete(LB_Fav.ItemIndex);
end;

end.
