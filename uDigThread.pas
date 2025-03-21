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
unit uDigThread;

interface

uses
    System.Classes
  , SysUtils
  , SyncObjs
  , uIPTVCommon
  ;

type
  TDigThread = class(TThread)
  private
    { Private declarations }
    FList: TStringList;
    FStart: integer;
    FCount: integer;
    FQuery: string;
    FMax: integer;
  protected
    procedure Execute; override;
  public
    OnStart: TProc<TDigThread>;
    OnFound: TProc<TDigThread, string, integer>;
    OnDone : TProc<TDigThread>;
    constructor Create(
      AList: TStringList;
      AQuery: string;
      AMax: Integer;
      AOnStart: TProc<TDigThread>;
      AOnFound: TProc<TDigThread, string, integer>;
      AOnDone:  TProc<TDigThread>
      ); overload;
    destructor  Destroy; override;
    procedure   Dig(AList: TStringList; AQuery: string; AMax: integer);
  end;

implementation

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

function Match(AList: TStringList; AIdx: Integer; AWords: TStringList): Boolean;
var
  LWord: string;
  LMatch: Boolean;
  LLine: string;
begin
  Result := False;
  LLine := AList[AIdx].ToLower;
  Result := True;
  for LWord in AWords do
  begin
    if  LWord.IsEmpty then Continue;

    if LWord[1] = '+' then
    begin
      if Copy(LWord, 2, 3) = 'tv' then Result := Result and (AList[AIdx + 2] = 'TV')
      else if Copy(LWord, 2, 3) = 'mov' then Result := Result and (AList[AIdx + 2] = 'MOVIE')
      else if Copy(LWord, 2, 3) = 'ser' then Result := Result and (AList[AIdx + 2] = 'SERIE')
      else Result := False;
    end else
    if LWord[1] = '-' then
      Result := Result and not (Pos( Copy(LWord, 2),  LLine) > 0)
    else
      Result := Result and ( Pos(LWord, LLine) > 0 );
    if not Result then Break;
  end;
end;

constructor TDigThread.Create(
              AList: TStringList;
              AQuery: string;
              AMax: Integer;
              AOnStart: TProc<TDigThread>;
              AOnFound: TProc<TDigThread, string, integer>;
              AOnDone:  TProc<TDigThread>
            );
begin
  inherited Create(False);
  FQuery := AQuery;
  FMax := AMax;
  FList := AList;
  OnStart := AOnStart;
  OnFound := AOnFound;
  OnDone  := AOnDone;
end;

destructor TDigThread.Destroy;
begin
  Terminate;
  WaitFor;
  inherited;
end;

procedure Abort;
begin
end;

procedure TDigThread.Dig(AList: TStringList; AQuery: string; AMax: integer);
begin

end;

procedure TDigThread.Execute;
var
  LineIdx: Integer;
  LWords: TStringList;
  LMax: Integer;
begin
  try
    LWords := TStringList.Create;

      if Assigned(OnStart) then Synchronize( procedure begin OnStart(Self); end );
      LIneIdx := 2;
      LMax := FMax;
      LWords.Clear;
      GetQuotedFields(LWords, FQuery.ToLower, ' ');
      while (LineIdx < FList.Count) do
        begin
          if Terminated or (LMax = 0) then Break;
          if Match(FList, LIneIdx, LWords) and Assigned(OnFound) then
          begin
          Dec(LMax);
          Synchronize(
             procedure
             begin
               OnFound(Self, FList[LIneIdx], LineIdx);
             end
           );
          end;
          Inc(LIneIdx, 6);
      end;
      if Assigned(OnDone) then Synchronize( procedure begin OnDone(Self); end );
    LWords.Free;
  Terminate;
  except
    on Ex: Exception do
      var a := 1;
  end;
end;

end.
