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
program IPTVDigger;

uses
  Vcl.Forms,
  uIPTVDigger in 'uIPTVDigger.pas' {Form1},
  Vcl.Themes,
  Vcl.Styles,
  uJX4Dict in 'API\uJsonX4\uJX4Dict.pas',
  uJX4List in 'API\uJsonX4\uJX4List.pas',
  uJX4Object in 'API\uJsonX4\uJX4Object.pas',
  uJX4Rtti in 'API\uJsonX4\uJX4Rtti.pas',
  uJX4Value in 'API\uJsonX4\uJX4Value.pas',
  uJX4YAML in 'API\uJsonX4\uJX4YAML.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := TRUE;
  Application.Initialize;
  Application.MainFormOnTaskbar := False;
  TStyleManager.TrySetStyle('Iceberg Classico');
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
