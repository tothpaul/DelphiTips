object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
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
    Top = 16
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
      
        'Demonstration of a sample component with a Record published prop' +
        'erty'
      ''
      
        'You can change values of the Record and be notified of the chang' +
        'es by a OnChange event !'
      ''
      
        'If you have an error about TRecordPropertyObject while opening t' +
        'his form'
      ''
      '- close the project  "WITHOUT SAVING CHANGES"'
      ''
      '- Install "Execute.RecrodProperties.Packaged" package'
      ''
      '- reopen this project'
      ''
      '(see Demo1 for a demo without the package)')
    TabOrder = 1
  end
  object Memo2: TMemo
    Left = 408
    Top = 136
    Width = 219
    Height = 83
    Color = clInfoBk
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      '// the structured property'
      '  TRecordProperty = record'
      '    Str: string;'
      '    Int: Integer;'
      '  end;')
    ParentFont = False
    TabOrder = 2
  end
  object RecordPropertyObject1: TRecordPropertyObject
    OnChange = RecordPropertyObject1Change
    Left = 352
    Top = 224
    FValues.Str = 'DesignTimeValue'
    FValues.Int = 456
  end
end
