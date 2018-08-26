unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Execute.RecordProperty, Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    RecordPropertyObject1: TRecordPropertyObject;
    Memo2: TMemo;
    procedure RecordPropertyObject1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
begin
  Memo1.Clear;
// Display initial values
  RecordPropertyObject1Change(Self);
// Change Int value
  RecordPropertyObject1.Values.Int := 123;
// Change Str value
  RecordPropertyObject1.Values.Str := 'Hello';
end;

procedure TForm2.RecordPropertyObject1Change(Sender: TObject);
begin
// shows current values of the component
  Memo1.Lines.Add(RecordPropertyObject1.Values.Str + ' : ' + IntToStr(RecordPropertyObject1.Values.Int));
end;

end.
