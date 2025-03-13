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
unit uThreadedHTTPGet;

interface

uses
  System.Classes
  , SysUtils
  , System.Types
  , System.Net.URLClient
  , System.Net.HttpClient
  , System.Net.HttpClientComponent
  ;

type
  THTTPGetThread = class(TThread)
  private
    { Private declarations }
  protected
    AsyncResult: IAsyncResult;
    TempProgress: Integer;
    procedure Execute; override;
    procedure  ReceiveDataExCallback(const Sender: TObject; AContentLength, AReadCount: Int64; AChunk: Pointer; AChunkLength: Cardinal; var AAbort: Boolean);
  public
    //ASync: Boolean;
    URL: string;
    HTTP: THTTPClient;
    Result: IHTTPResponse;
    Stream: TStream;
    UserObj: TObject;
    OnCompleted: TProc<THTTPGetThread, Boolean, Boolean>;
    OnProgress: TProc<THTTPGetThread, Integer>;
    constructor Create(AURL: string; AStream: TStream; AUserObj: TObject; AOnCompleted: TProc<THTTPGetThread, Boolean, Boolean>; AOnProgress: TProc<THTTPGetThread, Integer>); overload;
    destructor Destroy; override;
  end;

implementation

constructor THTTPGetThread.Create(AURL: string; AStream: TStream;
  AUserObj: TObject; AOnCompleted: TProc<THTTPGetThread, Boolean, Boolean>;
  AOnProgress: TProc<THTTPGetThread, Integer>);
begin
  inherited Create(False);
  FreeOnTerminate := False;
  TempProgress := -1;
  URL := AURL;
  Stream := AStream;
  UserObj := AUserObj;
  OnCompleted := AOnCompleted;
  OnProgress := AOnProgress;
end;

destructor THTTPGetThread.Destroy;
begin
  if Assigned(AsyncResult) then
  begin
    AsyncResult.Cancel;
    HTTP.EndAsyncHTTP(AsyncResult);
    WaitFor;
  end;
  inherited;
end;

procedure THTTPGetThread.ReceiveDataExCallback(const Sender: TObject; AContentLength, AReadCount: Int64; AChunk: Pointer; AChunkLength: Cardinal; var AAbort: Boolean);
var
  LPRogress: Integer;
begin
  LProgress :=  Trunc((AReadCount / AContentLength) * 100);
  if Assigned(OnProgress) and (TempProgress < LProgress) then
    Synchronize(procedure begin OnProgress(Self, LProgress); end);
  TempProgress := LProgress;
end;

procedure THTTPGetThread.Execute;
begin
  try
    HTTP := THTTPClient.Create;
    HTTP.HandleRedirects := True;
    HTTP.MaxRedirects := 5;
    HTTP.ConnectionTimeout := 2*60*1000;
    HTTP.SendTimeout:= 15*1000;
    HTTP.ResponseTimeout:= 5*60*1000;
    HTTP.ReceiveDataExCallback := ReceiveDataExCallback;
    try
      AsyncResult := HTTP.BeginGet(URL, Stream);
      while (not AsyncResult.IsCancelled) and (not AsyncResult.IsCompleted) do
      begin
        if Terminated then
        begin
          AsyncResult.Cancel;
          HTTP.EndAsyncHTTP(AsyncResult);
          Break;
        end;
        Sleep(100);
      end;

      Result := HTTP.EndAsyncHTTP(AsyncResult);
      if Assigned(OnCompleted) then
      begin
        OnCompleted(Self, Terminated or AsyncResult.IsCancelled, True);
        Synchronize(procedure begin OnCompleted(Self, Terminated or AsyncResult.IsCancelled, False); end);
      end;
    except
      OnCompleted(Self, True, True);
      Synchronize(procedure begin OnCompleted(Self, True, False); end);
    end;
  finally
    FreeAndNil(HTTP);
    Result := Nil;
    AsyncResult := Nil;
  end;
end;

end.
