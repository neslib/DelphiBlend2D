unit FMain;

{$INCLUDE 'blFmxConfig.inc'}

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Diagnostics,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Objects,
  FMX.ListBox,
  FMX.Layouts,
  FMX.Edit,
  {$IFDEF USE_SKIA}
  System.Skia,
  {$ENDIF}
  Blend2D,
  FBase;

type
  TFormMain = class(TFormBase)
    ToolBar2: TToolBar;
    LayoutText: TLayout;
    EditText: TEdit;
    ToolBar1: TToolBar;
    LabelStyle: TLabel;
    Layout1: TLayout;
    ComboBoxStyle: TComboBox;
    LabelFontSizeHeader: TLabel;
    TrackBarFontSize: TTrackBar;
    LabelFontSize: TLabel;
    CheckBoxDebug: TCheckBox;
    LabelText: TLabel;
    LabelMS: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure TrackBarFontSizeChange(Sender: TObject);
  private
    FBlend2DFontFace: TBLFontFace;
    FBlend2DFont: TBLFont;
    {$IFDEF USE_SKIA}
    FSkiaTypeface: ISkTypeface;
    FSkiaFont: ISkFont;
    {$ENDIF}
  private
    class procedure DebugGlyphBufferSink(const AMessage: PUTF8Char;
      ASize: Size_T; AUserData: Pointer); cdecl; static;
  protected
    procedure BeforeRender; override;
    procedure RenderFireMonkey(const ACanvas: TCanvas); override;
    procedure RenderBlend2D(const AContext: TBLContext); override;
    {$IFDEF USE_SKIA}
    procedure RenderSkia(const ACanvas: ISkCanvas); override;
    {$ENDIF}
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}
{$R assets.res}

uses
  System.Math;

{ TFormMain }

procedure TFormMain.BeforeRender;
begin
  inherited;
end;

class procedure TFormMain.DebugGlyphBufferSink(const AMessage: PUTF8Char;
  ASize: Size_T; AUserData: Pointer);
type
  PBLString = ^TBLString;
var
  Buffer: PBLString absolute AUserData;
begin
  Buffer.Append(AMessage, ASize);
  Buffer.Append(#10);
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  inherited;
  var FontData: TBytes;
  var Stream := TResourceStream.Create(HInstance, 'BASKERVVILLE_REGULAR', RT_RCDATA);
  try
    SetLength(FontData, Stream.Size);
    Stream.ReadBuffer(FontData, Length(FontData));

    {$IFDEF USE_SKIA}
    Stream.Position := 0;
    FSkiaTypeface := TSkTypeface.MakeFromStream(Stream);
    FSkiaFont := TSkFont.Create(FSkiaTypeface, TrackBarFontSize.Value);
    {$ENDIF}
  finally
    Stream.Free;
  end;

  var Blend2DFontData: TBLFontData;
  Blend2DFontData.MakeFromData(FontData);

  FBlend2DFontFace.MakeFromData(Blend2DFontData, 0);
  FBlend2DFont.MakeFromFace(FBlend2DFontFace, TrackBarFontSize.Value);
end;

procedure TFormMain.RenderBlend2D(const AContext: TBLContext);
begin
  AContext.FillAll(TAlphaColors.Black);

  var Style: TBLVar;
  var W, H: Double;
  var Gradient: TBLGradient;

  case ComboBoxStyle.ItemIndex of
    0: Style := BLRgba32(TAlphaColors.White);

    1: begin
         W := PaintBox.Width;
         H := PaintBox.Height;

         Gradient.Make(BLLinearGradientValues(0, 0, W, H));
         Gradient.AddStop(0.0, $FFFF0000);
         Gradient.AddStop(0.5, $FFAF00AF);
         Gradient.AddStop(1.0, $FF0000FF);

         Style := Gradient;
       end;

    2: begin
         W := PaintBox.Width;
         H := PaintBox.Height;
         var R: Double := Min(W, H);

         Gradient.Make(BLRadialGradientValues(W * 0.5, H * 0.5, W * 0.5, H * 0.5, R * 0.5));
         Gradient.AddStop(0.0, $FFFF0000);
         Gradient.AddStop(0.5, $FFAF00AF);
         Gradient.AddStop(1.0, $FF0000FF);

         Style := Gradient;
       end;

    3: begin
         W := PaintBox.Width;
         H := PaintBox.Height;

         Gradient.Make(BLConicGradientValues(W * 0.5, H * 0.5, 0.0));
         Gradient.AddStop(0.00, $FFFF0000);
         Gradient.AddStop(0.33, $FFAF00AF);
         Gradient.AddStop(0.66, $FF0000FF);
         Gradient.AddStop(1.00, $FFFF0000);

         Style := Gradient;
       end;
  end;

  var Stopwatch := TStopwatch.StartNew;
  AContext.FillText(BLPoint(10, 10 + FBlend2DFont.Size), FBlend2DFont,
    EditText.Text, Style);
  var MS := Stopwatch.Elapsed.TotalMilliseconds;
  LabelMS.Text := Format('%.3f ms', [MS]);

  if (CheckBoxDebug.IsChecked) then
  begin
    var GlyphBuffer: TBLGlyphBuffer;
    var Output: TBLString;
    GlyphBuffer.SetDebugSink(DebugGlyphBufferSink, @Output);
    GlyphBuffer.SetText(EditText.Text);
    FBlend2DFont.Shape(GlyphBuffer);

    var SmallFont: TBLFont;
    SmallFont.MakeFromFace(FBlend2DFontFace, 22);
    var Metrics := SmallFont.Metrics;

    var I := 0;
    var Pos := BLPoint(10, 10 + (FBlend2DFont.Size * 1.2) + SmallFont.Size);
    while (I < Output.Size) do
    begin
      var Stop := Min(Output.IndexOf(#10, I), Output.Size);

      var Color := TAlphaColors.White;
      if ((Stop - I) > 0) and (Output.Data[I] = '[') then
        Color := TAlphaColors.Yellow;

      var View := TBLStringView.Create(Pointer(Output.Data + I), Stop - I);
      AContext.FillUtf8Text(Pos, SmallFont, View, Color);

      Pos.Y := Pos.Y + Metrics.Ascent + Metrics.Descent;
      I := Stop + 1;
    end;
  end;
end;

procedure TFormMain.RenderFireMonkey(const ACanvas: TCanvas);
begin
  ACanvas.Clear(TAlphaColors.Black);
  if (ComboBoxStyle.ItemIndex <> 0) then
  begin
    RenderNotSupported;
    Exit;
  end;

  { Note: custom fonts are not (easily) supported with FireMonkey.
    So we assume that the Noto Sans font is installed on the system. }
  ACanvas.Font.Family := 'Noto Sans';
  ACanvas.Font.Size := TrackBarFontSize.Value;
  ACanvas.Fill.Kind := TBrushKind.Solid;
  ACanvas.Fill.Color := TAlphaColors.White;
  ACanvas.FillText(RectF(10, 6, PaintBox.Width, PaintBox.Height), EditText.Text,
    False, 1, [], TTextAlign.Leading, TTextAlign.Leading);
end;

{$IFDEF USE_SKIA}
procedure TFormMain.RenderSkia(const ACanvas: ISkCanvas);
begin
  ACanvas.Clear(TAlphaColors.Black);

  var Shader: ISkShader := nil;
  var W: Single := PaintBox.Width;
  var H: Single := PaintBox.Height;
  case ComboBoxStyle.ItemIndex of
    0: FSkiaFill.Color := TAlphaColors.White;

    1: Shader := TSkShader.MakeGradientLinear(PointF(0, 0), PointF(W, H),
        [$FFFF0000, $FFAF00AF, $FF0000FF], [0.0, 0.5, 1.0]);

    2: Shader := TSkShader.MakeGradientRadial(PointF(W * 0.5, H * 0.5),
        Min(W, H) * 0.5, [$FFFF0000, $FFAF00AF, $FF0000FF], [0.0, 0.5, 1.0]);

    3: Shader := TSkShader.MakeGradientSweep(
        PointF(W * 0, H * 0.5), [$FFFF0000, $FFAF00AF, $FF0000FF, $FFFF0000],
        [0.00, 0.33, 0.66, 1.00]);
  end;

  FSkiaFill.Shader := Shader;
  ACanvas.DrawSimpleText(EditText.Text, 10, 10 + FSkiaFont.Size, FSkiaFont, FSkiaFill);
end;
{$ENDIF}

procedure TFormMain.TrackBarFontSizeChange(Sender: TObject);
begin
  inherited;
  var Size := Trunc(TrackBarFontSize.Value);
  LabelFontSize.Text := Size.ToString;

  FBlend2DFont.Reset;
  FBlend2DFont.MakeFromFace(FBlend2DFontFace, TrackBarFontSize.Value);

  {$IFDEF USE_SKIA}
  FSkiaFont := TSkFont.Create(FSkiaTypeface, TrackBarFontSize.Value);
  {$ENDIF}
end;

end.
