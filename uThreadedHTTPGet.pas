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
    ASync: Boolean;
    URL: string;
    HTTP: THTTPClient;
    Result: IHTTPResponse;
    Stream: TStream;
    OnCompleted: TProc<THTTPGetThread, Boolean, Boolean>;
    OnProgress: TProc<THTTPGetThread, Integer>;
    constructor Create(AAsync: Boolean; AURL: string; AStream: TStream; AOnCompleted: TProc<THTTPGetThread, Boolean, Boolean>; AOnProgress: TProc<THTTPGetThread, Integer>); overload;
    destructor Destroy; override;
  end;

implementation

constructor THTTPGetThread.Create(AAsync: Boolean; AURL: string;
  AStream: TStream; AOnCompleted: TProc<THTTPGetThread, Boolean, Boolean>;
  AOnProgress: TProc<THTTPGetThread, Integer>);
begin
  inherited Create(False);
  FreeOnTerminate := False;
  TempProgress := -1;
  ASync := AAsync;
  URL := AURL;
  Stream := AStream;
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

  finally
    FreeAndNil(HTTP);
    Result := Nil;
    AsyncResult := Nil;
  end;
end;

end.
