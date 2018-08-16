unit ClicknThrow.Main;

{
  Effet de scrolling à la iPhone (c)2009, Paul TOTH <tothpaul@free.fr>
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Math;

type
  TMainForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormResize(Sender: TObject);
  private
    { Déclarations privées }
    FDesktop  : TBitmap;
    FBitmap   : TBitmap;
    FTop      : Integer;
    FLeft     : Integer;
    FPos      : TPoint;
    FDownTime : Cardinal;
    FDownPos  : TPoint;
    FDownState: TPoint;
    FMoveTime : Cardinal;
    FVector   : TPoint;
    FSpeed    : Cardinal;
    FTicks    : Cardinal;
    FBounds   : TRect;
    procedure InitPosition;
    procedure WMEraseBkGnd(var Msg: TMessage); message WM_ERASEBKGND;
    procedure DrawScrollBars;
  public
    { Déclarations publiques }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TransformColor(var aColor: Cardinal);
begin
// assombrir la couleur
  aColor := (aColor and $FEFEFEFE) shr 1;
end;

procedure TransformBitmap(aBitmap : TBitmap);
var
  x, y : Integer;
  Pixel: PCardinal;
begin
// effectuer une transformation sur chaque pixel de l'image
  aBitmap.PixelFormat := pf32Bit;
  for y := 0 to Pred(aBitmap.Height) do
  begin
    Pixel := aBitmap.ScanLine[y];
    for x := 0 to Pred(aBitmap.Width) do
    begin
      TransformColor(Pixel^);
      Inc(Pixel);
    end;
  end;
end;

function GetDesktopBitmap: HBitmap;
var
  cScreenDC : HDC;
  cOldBitmap: HBitmap;
  cBitmapDC : HDC;
begin
// récupérer le DC de l'écran
  cScreenDC := GetDC(0);
// créer un bitmap compatible
  Result := CreateCompatibleBitmap(cScreenDC, Screen.Width, Screen.Height);
// obtenir un second DC compatible
  cBitmapDC := CreateCompatibleDC(cScreenDC);
// y placer le bitmap
  cOldBitmap := SelectObject(cBitmapDC, Result);
// copier le DC de l'écran dans le DC contenant le bitmap
  BitBlt(cBitmapDC, 0, 0, Screen.Width, Screen.Height, cScreenDC, 0, 0, SRCCOPY);
// restaurer l'ancien bitmap
  SelectObject(cBitmapDC, cOldBitmap);
// supprimer les DC
  DeleteDC(cBitmapDC);
  ReleaseDC(0, cScreenDC);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
// on récupère l'image du bureau
  FDesktop := TBitmap.Create;
  FDesktop.Handle := GetDesktopBitmap;
// image pour dessiner offline
  FBitmap := TBitmap.Create;
  FBitmap.PixelFormat := pf32Bit;
// on lui applique une petite transformation
  TransformBitmap(FDesktop);
// l'image a besoin d'être repositionnée
  Tag := 1;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
 // positions limites de l'image
  FBounds := Rect(ClientWidth - FDesktop.Width, ClientHeight - FDesktop.Height, 0, 0);
  FBitmap.Width := ClientWidth;
  FBitmap.Height := ClientHeight;
end;

procedure TMainForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
// quand on clic, l'image ne doit plus bouger, mais nous devons connaitre la position et le temps de référence
  FSpeed := 0;
  FVector.X := 0;
  FVector.Y := 0;
  FDownTime := GetTickCount;
  FDownPos.X := x;
  FDownPos.Y := y;
  FDownState := FDownPos;
end;

procedure TMainForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
// l'image se déplace avec la souris quand un bouton est enfoncé
  if [ssLeft, ssRight] * Shift <> [] then
  begin
    Inc(FLeft, X - FDownState.X);
    Inc(FTop, Y - FDownState.Y);
    FDownState.X := X;
    FDownState.Y := Y;
    FMoveTime := GetTickCount;
   // on vérifie que la souris n'est pas restée sur place trop longtemps
    if FMoveTime - FDownTime > 150 then
    begin
      FDownPos := FDownState;
      FDownTime := FMoveTime;
    end;
    Invalidate;
  end;
end;

procedure TMainForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
// quand on relache la souris, on détermine le déplacement de l'image en conséquence
  if FDownTime > 0 then
  begin
   // vecteur de déplacement
    FVector.X := X - FDownPos.X;
    FVector.Y := Y - FDownPos.Y;
   // vitesse de déplacement
    FTicks := GetTickCount;
    FSpeed := FTicks - FDownTime;
    FDownTime := 0;
    Invalidate;
  end;
end;

procedure TMainForm.InitPosition;
var
  cPoint    : TPoint;
begin
// calcul de la position de l'image pour que la fiche semble transparente
  cPoint.X := 0;
  cPoint.Y := 0;
  cPoint := ClientToScreen(cPoint);
  FLeft := - cPoint.X;
  FTop := - cPoint.Y;
  FPos.X := Left;
  FPos.Y := Top;
end;

procedure TMainForm.WMEraseBkGnd(var Msg: TMessage);
begin
// pas de dessin du background (évite le clignotemment)
  Msg.Result := 1;
end;

procedure TMainForm.FormPaint(Sender: TObject);
var
  cDelta  : Cardinal;
  cRedraw : Boolean;
  cDx, cDy: Integer;
begin
// si nécessaire, calcul la position initiale de l'image
  if Tag = 1 then
  begin
    Tag := 0;
    InitPosition;
  end;
// on va boucler sur FormPaint aussi longtemps que nécessaire
  cRedraw := False;
// a-t-on une vitesse de déplacement ?
  if FSpeed > 0 then
  begin
   // dans ce cas on va boucler
    cRedraw := True;
   // et en fonction du temps, on déplace l'image
    cDelta := GetTickCount - FTicks;
    if cDelta > 0 then
    begin
    // nouvelle position en fonction du temps écouler et de la vitesse
      cDx := Round(FVector.X * cDelta / FSpeed);
      cDy := Round(FVector.Y * cDelta / FSpeed);
      if (cDx = 0) and (cDy = 0) then
      begin
        FVector.X := 0;
        FVector.Y := 0;
        FSpeed := 0;
      end
      else
      begin
        FLeft := Round(FLeft + cDx);
        FTop := Round(FTop + cDy);
      // ajutement de la vitesse
        FSpeed := Round(FSpeed + FSpeed * cDelta / 1000);
      // nouveau temps de référence
        Inc(FTicks, cDelta);
      // si le vecteur de déplacement est nul, la vitesse l'est également
        if (FVector.X = 0) and (FVector.Y = 0) then
          FSpeed := 0
      end;
    end;
  end;
// gestion des cas ou l'image ne couvre pas toute la fenêtre
  if FDownTime = 0 then
  begin
    if FLeft > FBounds.Right then
    begin
      FLeft := FBounds.Right;
      FVector.X := 0;
      cRedraw := True;
    end;
    if FLeft < FBounds.Left then
    begin
      FLeft := FBounds.Left;
      FVector.X := 0;
      cRedraw := True;
    end;
    if FTop > FBounds.Bottom then
    begin
      FTop := FBounds.Bottom;
      FVector.Y := 0;
      cRedraw := True;
    end;
    if FTop < FBounds.Top then
    begin
      FTop := FBounds.Top;
      FVector.Y := 0;
      cRedraw := True;
    end;
  end;
  with FBitmap.Canvas do
  begin
  // dessiner l'image
    Draw(FLeft, FTop, FDesktop);
  // dessiner au besoin des zones grises autour de l'image
    Brush.Color := $606060;
    if (FLeft > FBounds.Right) then
      FillRect(Rect(0, 0, Pred(FLeft), FBitmap.Height));
    if FLeft < FBounds.Left then
      FillRect(Rect(FLeft + FDesktop.Width + 1, 0, FBitmap.Width, FBitmap.Height));
    if FTop > FBounds.Bottom then
      FillRect(Rect(0, 0, FBitmap.Width, Pred(FTop)));
    if FTop < FBounds.Top then
      FillRect(Rect(0, FTop + FDesktop.Height + 1, FBitmap.Width, FBitmap.Height));
    if FDownTime > 0 then
      DrawScrollBars;
  end;
  Canvas.Draw(0, 0, FBitmap);
  if cRedraw then
    Invalidate;
end;

procedure TMainForm.DrawScrollBars;
var
  x, y : Integer;
  Pixel: PCardinal;
  start: Integer;
  len  : Integer;
begin
  if (FBitmap.Height > 10) then
  begin
    len := (FBitmap.Width - 17) * FBitmap.Width div FDesktop.Width;
    Start := 5 - (FBitmap.Width - 17) * FLeft div FDesktop.Width;
    if Start < 5 then
    begin
      inc(len, start - 5);
      start := 5;
    end;
    if start + len > FBitmap.Width - 17 then
      len := FBitmap.Width - 17 - start;
    for y := FBitmap.Height - 10 to FBitmap.Height - 5 do
    begin
      Pixel := FBitmap.ScanLine[y];
      Inc(Pixel, start);
      for x := start to start + len do
      begin
        TransformColor(Pixel^);
        Inc(Pixel);
      end;
    end;
  end;
  if FBitmap.Width > 10 then
  begin
    len := (FBitmap.Height - 17) * FBitmap.Height div FDesktop.Height;
    start := 5 - (FBitmap.Height - 17) * Ftop div FDesktop.Height;
    if start < 5 then
    begin
      inc(len, start - 5);
      start := 5;
    end;
    if start + len > FBitmap.Height - 17 then
      len := FBitmap.Height - 17 - start;
    for y := start to start + len do
    begin
      Pixel := FBitmap.ScanLine[y];
      Inc(Pixel, FBitmap.Width - 10);
      for x := FBitmap.Width - 10 to FBitmap.Width - 5 do
      begin
        TransformColor(Pixel^);
        Inc(Pixel);
      end;
    end;
  end;
end;

end.
