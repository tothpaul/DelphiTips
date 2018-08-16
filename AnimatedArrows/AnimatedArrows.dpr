program AnimatedArrows;

uses
  Forms,
  AnimatedArrows.Main in 'AnimatedArrows.Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

