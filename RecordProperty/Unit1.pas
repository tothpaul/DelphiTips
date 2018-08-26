unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Execute.RecordProperty;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
    procedure OnChange(Sender: TObject);
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  M: TRecordPropertyObject;
begin
  M := TRecordPropertyObject.Create(Self);
  // Values is a Record, let's see if we are notified of any change
  // and if the values are updated !
  Memo1.Clear;
  M.OnChange := OnChange;
  OnChange(M);             // display default values
  M.Values.Str := 'Hello'; // should raise an OnChange event
  M.Values.Int := 5;       // should raise an OnChange event
  M.Free;
  if Memo1.Text = ' : 0'#13#10'Hello : 0'#13#10'Hello : 5'#13#10 then
    ShowMessage('It works !')
  else
    ShowMessage('Something is wrong :(');
end;

procedure TForm1.OnChange(Sender: TObject);
begin
  Memo1.Lines.Add(TRecordPropertyObject(Sender).Values.Str + ' : ' + IntToStr(TRecordPropertyObject(Sender).Values.Int));
end;

end.
