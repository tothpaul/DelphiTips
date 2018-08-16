unit AnimatedArrows.Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
  private
    { Déclarations privées }
    Points : array[0..3] of TPoint;
    Current: Integer;
    Down   : TPoint;
    Timer  : Integer;
    procedure ArrowLine(const p1, p2: TPoint);
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  DoubleBuffered := True;
  Points[0].x := 100;
  Points[0].y := 100;
  Points[1].x := 200;
  Points[1].y := 100;
  Points[2].x := 200;
  Points[2].y := 200;
  Points[3].x := 300;
  Points[3].y := 200;
  Current := -1;
end;

procedure TForm1.FormPaint(Sender: TObject);
var
  Index: Integer;
begin
  with Canvas do
  begin
    Pen.Color := clBlue;
    Pen.Style := psSolid;

    MoveTo(Points[0].x, Points[0].y);
    LineTo(Points[1].x, Points[1].y);
    LineTo(Points[2].x, Points[2].y);
    LineTo(Points[3].x, Points[3].y);

    Pen.Color := clBlack;
    Brush.Color := clLime;

    ArrowLine(Points[0], Points[1]);
    ArrowLine(Points[1], Points[2]);
    ArrowLine(Points[2], Points[3]);

    for Index := 0 to 3 do
    begin
      if Index = Current then
        Brush.Color := clYellow
      else
        Brush.Color := clRed;
      with Points[Index] do
      begin
        Rectangle(x-5, y-5, x+5, y+5);
      end;
    end;
  end;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  Focus: Integer;
  Index: Integer;

  function NearPoint(const p:TPoint): Boolean;
  begin
    Result := (x > p.x - 5) and (x < p.x + 5) and (y > p.y - 5) and (y < p.y + 5);
  end;

begin
  if  (ssLeft in Shift) then
  begin

    if Current >= 0 then
    begin
      Points[Current].x := Down.x + x;
      Points[Current].y := Down.y + y;
      Invalidate;
    end;

  end else begin
    Focus := -1;
    for Index := 0 to 3 do
    begin
      if NearPoint(Points[Index]) then
      begin
        Focus := index;
        Break;
      end;
    end;
    if Current <> Focus then
    begin
      Current := Focus;
      Invalidate;
    end;
  end;
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Current >= 0 then
  begin
    Down.x := Points[Current].x - x;
    Down.y := Points[Current].y - y;
  end;
end;

procedure TForm1.ArrowLine(const p1, p2: TPoint);
var
  Len   : Single;
  Arrow : array[0..2] of TPoint;
  cs, sn: Single;
  x     : Integer;
begin
  Len := Sqrt(Sqr(p2.x - p1.x) + Sqr(p2.y - p1.y));
  if Len < 10 then
    Exit;

  cs := (p2.x - p1.x) / Len;
  sn := (p2.y - p1.y) / Len;

  x := Timer mod 40;

  while x + 10 < Len do
  begin

    Arrow[0].x := p1.x + Round(  x * cs -  5 * sn);
    Arrow[0].y := p1.y + Round(+ 5 * cs +  x * sn);

    Arrow[1].x := p1.x + Round(  x * cs +  5 * sn);
    Arrow[1].y := p1.y + Round(- 5 * cs +  x * sn);

    Inc(x, 10); // length of the arrow

    Arrow[2].x := p1.x + Round(  x * cs -  0 * sn);
    Arrow[2].y := p1.y + Round(  0 * cs +  x * sn);

    Canvas.Polygon(Arrow);

    Inc(x, 30); // space between 2 arrows

  end;

end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Inc(Timer);
  Invalidate;
end;

end.
 
