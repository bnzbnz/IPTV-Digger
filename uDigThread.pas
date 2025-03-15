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
  ;

type
  TDigThread = class(TThread)
  private
    { Private declarations }
    PAbort: PBoolean;
    List: TStringList;
    Start: integer;
    Count: integer;
    Query: string;
    RunEvent: TEvent;
    FMax: integer;
  protected
    procedure Execute; override;
  public
    OnFound: TProc<TDigThread, string, integer>;
    constructor Create(AOnFound: TProc<TDigThread, string, integer>); overload;
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

function Match(ALine: string; AWords: TStringList): Boolean;
var
  LWord: string;
  LMatch: Boolean;
begin
  Result := False;
  ALine := ALine.ToLower;
  Result := True;
  for LWord in AWords do
  begin
    if  LWord.IsEmpty then Continue;

    if LWord[1] = '-' then
      Result := Result and not (Pos( Copy(LWord, 2),  ALine) > 0)
    else
      Result := Result and ( Pos(LWord, ALine) > 0 );
    if not Result then Break;
  end;
end;

constructor TDigThread.Create(AOnFound: TProc<TDigThread, string, integer>);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  OnFound := AOnFound;
  RunEvent := TEvent.Create(nil,True, False, '');
end;

destructor TDigThread.Destroy;
begin
  RunEvent.ResetEvent;
  Terminate;
  WaitFor;
  RunEvent.Free;
  inherited;
end;

procedure TDigThread.Dig(AList: TStringList; AQuery: string; AMax: integer);
begin
  RunEvent.ResetEvent;
  while (RunEvent.WaitFor(10) = wrSignaled) do;
  Query := AQuery;
  FMax := AMax;
  List := AList;
  RunEvent.SetEvent;
end;

procedure TDigThread.Execute;
var
  LIneIdx: integer;
  LWords: TStringList;
  LMax: Integer;
begin
  try
  LWords := TStringList.Create;
  while(not Terminated) do
  begin

    while( RunEvent.WaitFor(500) <> wrSignaled) do if Terminated then Break;
    if not Terminated then
    begin
      LIneIdx := 0;
      LMax := FMAx;
      LWords.Clear;
      GetQuotedFields(LWords, Query.ToLower, ' ');
      while (LIneIdx < List.Count) do
       begin
         if Terminated or (RunEvent.WaitFor(0) = wrTimeout) or (LMax=0) then Break;
         if Match(List[LIneIdx], LWords) and Assigned(OnFound) then
         begin
          Dec(LMax);
          Synchronize(
             procedure
             begin
               OnFound(Self, List[LIneIdx], LineIdx);
             end
           );
          end;
        Inc(LIneIdx, 3);
      end;
    end;
    RunEvent.ResetEvent;

  end;
  LWords.Free;
  except
    on Ex: Exception do
      var a := 1;
  end;
end;

end.
