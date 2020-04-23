object Frame2: TFrame2
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  DesignSize = (
    320
    240)
  object Label1: TLabel
    Left = 120
    Top = 113
    Width = 75
    Height = 13
    Anchors = []
    Caption = 'Chargement....'
  end
  object ProgressBar1: TProgressBar
    Left = 16
    Top = 208
    Width = 281
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Max = 50
    Step = 1
    TabOrder = 0
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 152
    Top = 32
  end
end
