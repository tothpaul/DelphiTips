unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  Vcl.ComCtrls, Vcl.StdCtrls;

type
  TFrame2 = class(TFrame)
    Label1: TLabel;
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

implementation

{$R *.dfm}

uses Unit1, Unit3;

procedure TFrame2.Timer1Timer(Sender: TObject);
begin
  if ProgressBar1.Position = ProgressBar1.Max then
  begin
    Form1.SetFrame(TFrame3, False);
  end else begin
    ProgressBar1.StepIt;
  end;
end;

end.
