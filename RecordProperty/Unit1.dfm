object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 24
    Top = 15
    Width = 75
    Height = 25
    Caption = 'let'#39's try !'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 24
    Top = 56
    Width = 545
    Height = 225
    Lines.Strings = (
      
        'Project1 uses TRecordPropertyObject, a sample Delphi Component w' +
        'ith a published Record property'
      ''
      
        'you can change any value of the record and be notified by the On' +
        'Change event.'
      ''
      
        'This code do not need the installation of Execute.RecordProperti' +
        'es.Package'
      ''
      
        '(Demo2 is the same demo with a registered packaged and a DesignT' +
        'ime instance of the component)')
    TabOrder = 1
  end
end
