unit uIPTVCommon;

interface
uses
    Classes
  ;


const
  M3xOffset =  3;
  M3XBlokk  =  5;
  M3XTitle  =  0;
  M3XId     =  1;
  M3XType   =  2;
  M3XRaw    =  3;
  M3XUrl    =  4;

  function Parse(AList: TStringList; AIdx: integer; var AId: integer; var  ATitle: string; var ALang: string; var AType: string): boolean;
  procedure GetQuotedFields(AList: TStringList; const AStr: string; ASeparator: Char = ',');

implementation
uses
    System.Character
  , System.IOUtils
  , Sysutils
  ;

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

function Parse(AList: TStringList; AIdx: integer; var AId: integer; var  ATitle: string; var ALang: string; var AType: string): boolean;
var
  LParts : TArray<string>;
  LTitleParts : TArray<string>;
  LSubParts: TStringList;
  LLangParts: TArray<string>;
  LName, LUrl, LStr: string;
begin
  Result := False;
  LSubParts := Nil;
  try
  try
   LStr      := UTF8Decode(AList[AIdx]);
   LParts    :=  LStr.Split([','], '"', '"',  99);
   LSubParts := TStringList.Create;
   GetQuotedFields(LSubParts, LParts[0], ' ');
   LName := LSubParts.Values['tvg-name'].DeQuotedString('"').Trim;
   if LName.Trim.StartsWith('#') then Exit;

    if Length(LParts) = 1 then  ATitle := LName else
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
    ATitle := ATitle.Trim;

    LLangParts :=  LName.Split(['[', ']', ' - ', '|', ':', '_'], '"', '"', 99);
    if (Length(LLangParts) > 1) then
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
    end;
    ALang := ALang.Trim;

    LUrl := AList[AIdx + 1].ToLower;
    try
      if not TryStrToInt(TPath.GetFileNameWithoutExtension(AList[AIdx +1]), AId) then AId := 0;
    except
      Aid := 0;
    end;
    if LUrl.Contains('/movie/') then AType := 'MOVIE'
    else if LUrl.Contains('/series/') then AType := 'SERIE'
    else AType := 'TV';

    Result := True;

  finally
    LSubParts.Free;
  end;
  except
    Result := False;
  end;
end;

end.
