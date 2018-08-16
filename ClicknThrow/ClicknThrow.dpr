program ClicknThrow;

uses
  Forms,
  ClicknThrow.Main in 'ClicknThrow.Main.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
