unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TFrameClass = class of TFrame;

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
    FFrame: TFrame;
  public
    { Déclarations publiques }
    procedure SetFrame(FrameClass: TFrameClass; AutoSize: Boolean);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses Unit2, Unit3;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetFrame(TFrame2, True);
end;

procedure TForm1.SetFrame(FrameClass: TFrameClass; AutoSize: Boolean);
var
  x, y: Integer;
begin
  FFrame.Free;
  FFrame := FrameClass.Create(Self);
  FFrame.Parent := Self;
  if AutoSize then
  begin
    ClientWidth := FFrame.Width;
    ClientHeight := FFrame.Height;
    Left := (Screen.WorkAreaWidth - Width) div 2;
    Top := (Screen.WorkAreaHeight - Height) div 2;
  end else begin
    FFrame.Align := alClient;
    WindowState := wsMaximized;
  end;
end;

end.
