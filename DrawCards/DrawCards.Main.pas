unit DrawCards.Main;

// (c)2014-2018 Execute SARL

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    CheckBox1: TCheckBox;
    Image1: TImage;
    Image2: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure CheckBox1Click(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Déclarations privées }
    FCurrent: Integer;
    FWidth  : Cardinal;
    FHeight : Cardinal;
   {$POINTERMATH ON}
    FBitmap : PCardinal;
    FStack  : array[1..19] of Integer;
    FInfo   : TBitmapInfo;
    procedure RotateBitmap(ABitmap: TBitmap; x, y, Angle, rx, ry: Integer; AndMask, OrMask: Cardinal);
    procedure RotateBitmap2(ABitmap: TBitmap; x, y, Angle, rx, ry: Integer; AndMask, OrMask: Cardinal);
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Math;

{
  Dessine un bitmap en x, y avec rotation de centre rx, ry
  AndMask et OrMask permettent de modifier la couleur de chaque pixel
}
procedure TForm1.RotateBitmap(ABitmap: TBitmap; x, y, Angle, rx, ry: Integer; AndMask, OrMask: Cardinal);
var
  transColor : Cardinal;
  sn, cs     : Extended;
  iCos, iSin : Integer;
  Row, Col   : Integer;
  pixels     : PCardinal;
  Color      : Cardinal;
  yCos, ySin : Integer;
  dx, dy     : Integer;
  px, py     : Integer;
begin
  TransColor := PCardinal(ABitmap.ScanLine[0])^;  // Couleur de transparence

  SinCos(Angle * PI/180, sn, cs);

  iCos := Trunc(cs * (1 shl 16)); // sin et cos précalulé "shl 16" pour augmenter la précision
  iSin := Trunc(sn * (1 shl 16));

  for Row := 0 to ABitmap.Height - 1 do // parcourir le bitmap source
  begin
    for dy := 0 to 0 do
    begin
      pixels := ABitmap.ScanLine[Row]; // sélectionner la ligne
      yCos := (2 * (Row + ry) + dy) * iCos; // précalcul de la rotation de la ligne
      ySin := (2 * (Row + ry) + dy) * iSin;
      for Col := 0 to ABitmap.Width - 1 do // parcourir en largeur
      begin
        Color := pixels^; // couleur du pixel
        if Color <> TransColor then // s'il n'est pas tranparent
        begin
          for dx := 0 to 0 do // le dessiner deux fois (cf boucle dy)
          begin
            px := x + SmallInt(((2 * (Col + rx) + dx) * iCos - ySin) shr 16) div 3; // point final à l'écran
            py := y + SmallInt(((2 * (Col + rx) + dx) * iSin + yCos) shr 16) div 3;
            if (Cardinal(px) < FWidth) and (Cardinal(py) < FHeight) then // s'il est visible à l'écran
            begin
              FBitmap[px + py * FWidth] := (Color and AndMask) or OrMask; // renseigner sa couleur en appliquant les masques
            end;
          end;
        end;
        Inc(pixels); // pixel suivant dans la ligne
      end;
    end;
  end;

end;

// Même méthode que ci-dessus en calculant le rectangle destination
procedure TForm1.RotateBitmap2(ABitmap: TBitmap; x, y, Angle, rx, ry: Integer; AndMask, OrMask: Cardinal);
type
  TCardinals = array[Word] of Cardinal;
  PCardinals = ^TCardinals;
var
  transColor : Cardinal;
  sn, cs     : Extended;
  iCos, iSin : Integer;
  x1, y1     : Integer;
  x2, y2     : Integer;
  Row, Col   : Integer;
  pixels     : PCardinal;
  Color      : Cardinal;
  yCos, ySin : Integer;
  dx, dy     : Integer;
  px, py     : Integer;
  source     : PCardinals;

  procedure AddPoint(dx, dy: Integer);
  var
    tx, ty: Integer;
  begin
    tx := x + SmallInt(((rx + dx) * iCos - (ry + dy) * iSin) shr 16);
    ty := y + SmallInt(((rx + dx) * iSin + (ry + dy) * iCos) shr 16);
    if tx < x1 then x1 := tx;
    if ty < y1 then y1 := ty;
    if tx > x2 then x2 := tx;
    if ty > y2 then y2 := ty;
  end;

begin
  TransColor := PCardinal(ABitmap.ScanLine[0])^;  // Couleur de transparence

  SinCos(Angle * PI/180, sn, cs);

  iCos := Trunc(cs * (1 shl 16)); // sin et cos précalulé "shl 16" pour augmenter la précision
  iSin := Trunc(sn * (1 shl 16));

  x1 := x + SmallInt((rx * iCos - ry * iSin) shr 16); // rotation du point 0,0
  y1 := y + SmallInt((rx * iSin + ry * iCos) shr 16);
  x2 := x1;
  y2 := y1;
  // rotation des 4 points du bitmap pour avoir le rectangle qui inclus le bitmap après rotation
  AddPoint(0, ABitmap.Height);
  AddPoint(ABitmap.Width, 0);
  AddPoint(ABitmap.Width, ABitmap.Height);

  // s'il est hors écran on passe
  if (x1 >= FWidth) or (y1 >= FHeight) or (x2 < 0) or (y2 < 0) then
    Exit;

  // limiter à l'écran
  x1 := Max(0, x1);
  y1 := Max(0, y1);
  x2 := Min(x2, FWidth - 1);
  y2 := Min(y2, FHeight - 1);

  // c'est trop lent d'accéder aux ScanLine à chaque pixel
  // Sur un Bitmap 32bit, on peut utiliser un espace unique
  Source := ABitmap.ScanLine[ABitmap.Height - 1];

  for Row := y1 to y2 do // pour chaque ligne du rectangle englobant
  begin
    yCos := (Row - y) * iCos; // précalcul de la rotation de la ligne
    ySin := (Row - y) * iSin;
    for Col := x1 to x2 do // parcourir en largeur
    begin
      px := SmallInt(((Col - x) * iCos + ySin) shr 15) - rx; // point d'origine dans le bitmap (formule inverse que dans RotateBitmap)
      py := SmallInt(((x - Col) * iSin + yCos) shr 15) - ry;
      if (Cardinal(px) < ABitmap.Width) and (Cardinal(py) < ABitmap.Height) then // si on retombe à l'intérieur du bitmap
      begin
        //pixels := ABitmap.ScanLine[py]; <-- trop lent pour le faire sur chaque pixel (dans RotateBitmap on travaille par ligne c'est plus rapide)
        //Inc(pixels, px);
        //Color := pixels^; // couleur du pixel
        Color := Source[px + ABitmap.Width * (ABitmap.Height - py - 1)]; // retrouver le pixel directement dans le bitmap 32bit
        if Color <> TransColor then // s'il n'est pas tranparent
          FBitmap[Col + Row * FWidth] := (Color and AndMask) or OrMask;
      end;
    end;
  end;

end;


procedure TForm1.FormCreate(Sender: TObject);
var
  Index: Integer;
begin
  FCurrent := 0;
  for Index := Low(FStack) to High(FStack) do
    FStack[Index] := Index - 10;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  FWidth := ClientWidth;
  FHeight := ClientHeight;
  if FBitmap <> nil then
    VirtualFree(FBitmap, 0, MEM_RELEASE);
  FBitmap := VirtualAlloc(0, Width * Height * SizeOf(Cardinal), MEM_COMMIT or MEM_RESERVE, PAGE_READWRITE);
  with FInfo do
  begin
    with bmiHeader do
    begin
      biSize     := 40;
      biWidth    := FWidth;
      biHeight   := - FHeight;
      biPlanes   := 1;
      biBitCount := 32;
    end;
  end;
  InvalidateRect(Handle, nil, false);
end;

procedure TForm1.FormPaint(Sender: TObject);
type
  TCardinals = array[Word] of Cardinal;
  PCardinals = ^TCardinals;
var
  Bitmap : TBitmap;
  x, y   : Integer;
  Pixels : PCardinals;
  iStack : Integer;
  Index  : Integer;
  AndMask: Cardinal;
  OrMask : Cardinal;
begin

// Image de fond en 32Bits
  Bitmap := Image2.Picture.Bitmap;
  Bitmap.PixelFormat := pf32Bit;

// Remplir FBitmap avec l'image de fond
  Index := 0;
  for y := 0 to FHeight - 1 do
  begin
    Pixels := Bitmap.ScanLine[y mod Bitmap.Height];
    for x := 0 to FWidth - 1 do
    begin
      FBitmap[Index] := Pixels[x mod Bitmap.Width] and $FFFFFF;  // canal Alpha à 0
      Inc(Index);
    end;
  end;

// Image de la carte en 32 bits
  Bitmap := Image1.Picture.Bitmap;
  Bitmap.PixelFormat := pf32Bit;

// Dessiner les cartes
  for iStack := Low(FStack) to High(FStack) do
  begin
    OrMask := iStack shl 24;
    Index := FStack[iStack];
    if CheckBox1.Checked then
    begin
      AndMask := 0; // le bitmap source est ignoré (sauf pour la transparence)
      OrMask := OrMask + (255 div 20) * (Index + 10);
      if iStack = FCurrent then
        OrMask := OrMask or $00FF0000;
    end else begin
      if iStack = FCurrent then
      begin
        AndMask := $80f0ff; // réduit les couleurs rouge et vert
      end else begin
        AndMask := $ffffff;
      end;
    end;
    RotateBitmap2(Bitmap, ClientWidth div 2, 2 * ClientHeight div 3, 10 * Index, - Bitmap.Width div 2, - Bitmap.Height, AndMask, OrMask);
  end;

// Afficher directement FBitmap sur le canvas
  SetDIBitsToDevice(Canvas.Handle, 0, 0, FWidth, FHeight, 0, 0, 0, FHeight, @FBitmap[0], FInfo, 0);
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
  Stack: Integer;
begin
  if FCurrent = 0 then
    Exit;
  Index := FStack[FCurrent];
  for Stack := FCurrent to High(FStack) - 1 do
  begin
    FStack[Stack] := FStack[Stack + 1];
  end;
  FStack[High(FStack)] := Index;
  InvalidateRect(Handle, nil, false);;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  Pixel: Integer;
begin
  Pixel := FBitmap[x + FWidth * y] shr 24; // récupère directement le canal alpha du bitmap unique
  if Pixel <> FCurrent then
  begin
    FCurrent := Pixel;
    InvalidateRect(Handle, nil, False);
    if FCurrent = 0 then
      Cursor := crDefault
    else
      Cursor := crHandPoint;
  end;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  InvalidateRect(Handle, nil, false);
end;

end.
