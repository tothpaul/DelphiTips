object Form1: TForm1
  Left = 351
  Top = 212
  BorderStyle = bsDialog
  Caption = 'Angry Birds'
  ClientHeight = 400
  ClientWidth = 543
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Ground: TShape
    Left = 0
    Top = 353
    Width = 801
    Height = 48
    Brush.Color = clGreen
  end
  object Shape1: TShape
    Left = 112
    Top = 264
    Width = 17
    Height = 89
    Brush.Color = clMaroon
  end
  object Support: TShape
    Left = 96
    Top = 256
    Width = 49
    Height = 9
    Brush.Color = clMaroon
  end
  object Bird: TShape
    Left = 104
    Top = 224
    Width = 33
    Height = 33
    Brush.Color = clYellow
    Shape = stCircle
  end
  object PaintBox1: TPaintBox
    Left = 0
    Top = 0
    Width = 543
    Height = 400
    Align = alClient
    OnMouseDown = PaintBox1MouseDown
    OnMouseMove = PaintBox1MouseMove
    OnMouseUp = PaintBox1MouseUp
    OnPaint = PaintBox1Paint
    ExplicitWidth = 800
  end
end
