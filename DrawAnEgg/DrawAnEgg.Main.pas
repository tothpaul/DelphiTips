unit DrawAnEgg.Main;

{ Draw an egg with Delphi Tokyo (c)2018 Execute SARL }

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TForm1 = class(TForm)
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

type
  TArc = record
    x1, y1: Integer;
    x2, y2: Integer;
    x3, y3: Integer;
    x4, y4: Integer;
  end;

  TEgg = record
    Arcs: array[0..3] of TArc;
  end;

procedure TForm1.FormPaint(Sender: TObject);
var
  ox, oy: Integer;
  Egg   : TEgg;
  e     : Integer;
  r     : Integer;
  x1, y1: Integer;
  x2, y2: Integer;
  x3, y3: Integer;
  x4, y4: Integer;
  r2    : Integer;
begin
 // egg center
  ox := ClientWidth div 2;
  oy := ClientHeight div 2;

// egg radius
  r  := ox div 3;

  // bottom of the egg
  with Egg.Arcs[0] do
  begin
    x1 := ox - r;
    y1 := oy + r;
    x2 := ox + r;
    y2 := oy - r;
    x3 := x1;
    y3 := oy;
    x4 := x2;
    y4 := oy;
  end;

  // left part of the egg
  with Egg.Arcs[1] do
  begin
    x1 := ox - r;
    y1 := oy + 2 * r;
    x2 := ox + 3 * r;
    y2 := oy - 2 * r;
    x3 := x1;
    y3 := y2;
    x4 := x1;
    y4 := oy;
  end;

  // right part of the egg
  with Egg.Arcs[2] do
  begin
    x1 := ox - 3 * r;
    y1 := oy + 2 * r;
    x2 := ox + r;
    y2 := oy - 2 * r;
    x3 := x2;
    y3 := oy;
    x4 := x2;
    y4 := y2;
  end;

  // top of the egg
  //  CB² + AC² = AB²
  //  2 x CB² = (2*r)²
  // CB² = 2r²
  // CB = sqrt(2)*r
  r2 := Round((2 - sqrt(2)) * r);
  with Egg.Arcs[3] do
  begin
    x1 := ox - r2;
    y1 := oy - r + r2;
    x2 := ox + r2;
    y2 := oy - r - r2;

    x3 := x2;
    y3 := y2;
    x4 := x1;
    y4 := y2;
  end;

  with Canvas do
  begin
    Brush.Style := bsClear;

    Pen.Width := 1;
    Pen.Color := $d0d0d0;
    for e := 0 to 3 do
      with Egg.Arcs[e] do
      begin
//        Rectangle(x1, y1, x2, y2);
        Arc(x1, y1, x2, y2, x1, y1, x1, y1);
      end;

    Pen.Width := 4;
    Pen.Color := clRed;
    with Egg.Arcs[0] do
      Arc(x1, y1, x2, y2, x3 ,y3, x4, y4);

    Pen.Color := clGreen;
    with Egg.Arcs[1] do
      Arc(x1, y1, x2, y2, x3 ,y3, x4, y4);

    Pen.Color := clBlue;
    with Egg.Arcs[2] do
      Arc(x1, y1, x2, y2, x3 ,y3, x4, y4);

    Pen.Color := clFuchsia;
    with Egg.Arcs[3] do
      Arc(x1, y1, x2, y2, x3 ,y3, x4, y4);
  end;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  Invalidate;
end;

end.
