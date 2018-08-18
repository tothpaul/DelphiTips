unit AngrBirds.Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls;

type
  TVector = record
    x, y : Single;
  end;

  TForm1 = class(TForm)
    Shape1: TShape;
    Support: TShape;
    Bird: TShape;
    Ground: TShape;
    PaintBox1: TPaintBox;
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
    Start   : TPoint;
    Action  : (acNone, acPull, acFly);
    Vector  : TVector;
    Position: TVector;
    Freq    : Int64;
    Timer   : Int64;
    procedure Idle(Sender: TObject; var Done: Boolean);
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Start.X := Support.Left + Support.Width div 2;
  Start.Y := Support.Top - Bird.Height div 2;
  DoubleBuffered := True;
  QueryPerformanceFrequency(Freq);
  Application.OnIdle := Idle;
end;


procedure TForm1.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if Action = acPull then
  begin
    Bird.Left := x - Bird.Width div 2;
    Bird.Top  := y - Bird.Height div 2;
    Invalidate;
  end;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  if Action = acPull then
  begin
    with PaintBox1.Canvas do
    begin
      Pen.Color := clRed;
      Pen.Width := 4;
      MoveTo(Start.X, Start.Y);
      LineTo(Bird.Left + Bird.Width div 2, Bird.Top + Bird.Height div 2);
    end;
  end;
end;

procedure TForm1.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Action = acNone then
  begin
    if (x > Bird.Left) and (x < Bird.Left + Bird.Width)
    and(y > Bird.Top) and (y < Bird.Top + Bird.Height) then
      Action := acPull;
  end;
  if Action = acFly then
  begin
    Bird.Left := Start.X - Bird.Width div 2;
    Bird.Top := Start.Y - Bird.Height div 2;
    Action := acNone;
  end;
end;

procedure TForm1.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Action = acPull then
  begin
    Action   := acFly;
    Vector.X := Start.X - (Bird.Left + Bird.Width div 2);
    Vector.Y := Start.Y - (Bird.Top + Bird.Height div 2);
    Position.X := Bird.Left;
    Position.Y := Bird.Top;
    QueryPerformanceCounter(Timer);
    Invalidate;
  end;
end;

procedure TForm1.Idle(Sender: TObject; var Done: Boolean);
const
  Speed = 20;
  g     = 9.81;
  K     = 0.05;
var
  Time : Int64;
  Delta: Single;
  x, y : Single;
begin
  Done := Action <> acFly;
  if not Done then
  begin
    QueryPerformanceCounter(Time);
    Delta := Speed * (Time - Timer) / Freq;
    Timer := Time;

    // perte de vitesse
    Vector.X := Vector.X - K * Vector.X * Delta;

    // perte de vitesse et gravitation
    Vector.Y := Vector.Y + G/2 * Delta - K * Vector.Y * Delta;

   // déplacement horizontal
    x := Position.x + Vector.X * Delta;

   // rebondir sur les bords d'écran
    if ((Vector.X < 0) and (x < 0)) or ((Vector.x > 0) and(x > ClientWidth - Bird.Width)) then
    begin
      x := Position.x;
      Vector.X := - Vector.X * 0.85;
    end;

   // déplacement vertical
    y := Position.y + Vector.Y * Delta;

   // rebondir sur le sol
    if ((Vector.Y > 0) and (y >= Ground.Top - Bird.Height)) then
    begin
      if Vector.Y < 5 then // évite les tous petits rebonds
      begin
        y := Ground.top - Bird.Height;
        Vector.Y := 0;
        Vector.X := Vector.X * 0.90;
      end else begin
        y := Position.y;
        Vector.Y := - Vector.Y * 0.85;
      end;
    end;

    // Nouvelle position définitive
    Position.x := x;
    Position.y := y;

    // Déplacer le Shape en conséquence
    Bird.Left := Round(x);
    Bird.Top  := Round(y);

    Invalidate;
  end;
end;

end.

