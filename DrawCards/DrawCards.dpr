program DrawCards;





uses
  Forms,
  DrawCards.Main in 'DrawCards.Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
