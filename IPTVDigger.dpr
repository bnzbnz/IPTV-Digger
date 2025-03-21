program IPTVDigger;

uses
  Vcl.Forms,
  uIPTVDigger in 'uIPTVDigger.pas' {IPTVForm},
  Vcl.Themes,
  Vcl.Styles,
  uThreadedHTTPGet in 'API\uThreadedHTTPGet.pas',
  uJX4Dict in 'API\uJsonX4\uJX4Dict.pas',
  uJX4List in 'API\uJsonX4\uJX4List.pas',
  uJX4Object in 'API\uJsonX4\uJX4Object.pas',
  uJX4Rtti in 'API\uJsonX4\uJX4Rtti.pas',
  uJX4Value in 'API\uJsonX4\uJX4Value.pas',
  uJX4YAML in 'API\uJsonX4\uJX4YAML.pas',
  uDigThread in 'uDigThread.pas',
  uIPTVCommon in 'uIPTVCommon.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := TRUE;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Iceberg Classico');
  Application.CreateForm(TIPTVForm, IPTVForm);
  Application.Run;
end.
