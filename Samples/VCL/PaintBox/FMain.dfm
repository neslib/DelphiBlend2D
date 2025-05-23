object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'TBlend2DPaintBox for VCL'
  ClientHeight = 445
  ClientWidth = 435
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object PanelSettings: TPanel
    Left = 0
    Top = 0
    Width = 435
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 425
    object CheckBoxLowRes: TCheckBox
      AlignWithMargins = True
      Left = 40
      Top = 16
      Width = 395
      Height = 17
      Margins.Left = 40
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alBottom
      Caption = 'Low Resolution (on high-DPI displays)'
      TabOrder = 0
      OnClick = CheckBoxLowResClick
      ExplicitWidth = 385
    end
  end
  object GridPanel: TGridPanel
    Left = 0
    Top = 33
    Width = 435
    Height = 412
    Align = alClient
    BevelOuter = bvNone
    ColumnCollection = <
      item
        SizeStyle = ssAbsolute
        Value = 40.000000000000000000
      end
      item
        Value = 100.000000000000000000
      end
      item
        SizeStyle = ssAbsolute
        Value = 40.000000000000000000
      end>
    ControlCollection = <
      item
        Column = 1
        Control = TrackBarX
        Row = 0
      end
      item
        Column = 0
        Control = TrackBarY
        Row = 1
      end
      item
        Column = 2
        Control = TrackBarZoom
        Row = 1
      end
      item
        Column = 1
        Control = TrackBarRotate
        Row = 2
      end
      item
        Column = 1
        Control = Blend2DPaintBox
        Row = 1
      end>
    RowCollection = <
      item
        SizeStyle = ssAbsolute
        Value = 40.000000000000000000
      end
      item
        Value = 100.000000000000000000
      end
      item
        SizeStyle = ssAbsolute
        Value = 40.000000000000000000
      end>
    TabOrder = 1
    ExplicitWidth = 425
    ExplicitHeight = 380
    object TrackBarX: TTrackBar
      Left = 40
      Top = 15
      Width = 355
      Height = 25
      Align = alBottom
      Max = 240
      ShowSelRange = False
      TabOrder = 0
      TickStyle = tsNone
      OnChange = TrackBarChange
      ExplicitWidth = 345
    end
    object TrackBarY: TTrackBar
      Left = 15
      Top = 40
      Width = 25
      Height = 332
      Align = alRight
      Max = 240
      Orientation = trVertical
      ShowSelRange = False
      TabOrder = 1
      TickStyle = tsNone
      OnChange = TrackBarChange
      ExplicitHeight = 300
    end
    object TrackBarZoom: TTrackBar
      Left = 395
      Top = 40
      Width = 25
      Height = 332
      Align = alLeft
      Max = 500
      Min = 1
      Orientation = trVertical
      Position = 100
      ShowSelRange = False
      TabOrder = 2
      TickMarks = tmTopLeft
      TickStyle = tsNone
      OnChange = TrackBarChange
      ExplicitLeft = 385
      ExplicitHeight = 300
    end
    object TrackBarRotate: TTrackBar
      Left = 40
      Top = 372
      Width = 355
      Height = 25
      Align = alTop
      Max = 360
      Min = -360
      ShowSelRange = False
      TabOrder = 3
      TickMarks = tmTopLeft
      TickStyle = tsNone
      OnChange = TrackBarChange
      ExplicitTop = 340
      ExplicitWidth = 345
    end
    object Blend2DPaintBox: TBlend2DPaintBox
      Left = 40
      Top = 40
      Width = 355
      Height = 332
      Align = alClient
      OnPaint = Blend2DPaintBoxPaint
      ExplicitLeft = 35
      ExplicitTop = 47
      ExplicitWidth = 353
      ExplicitHeight = 330
    end
  end
end
