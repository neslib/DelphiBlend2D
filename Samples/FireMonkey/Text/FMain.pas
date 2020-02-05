unit FMain;

{$INCLUDE 'blFmxConfig.inc'}

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
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
  Neslib.Skia,
  {$ENDIF}
  Blend4D,
  FBase;

type
  TFormMain = class(TFormBase)
    ToolBar: TToolBar;
    TrackBarFontSize: TTrackBar;
    LayoutText: TLayout;
    EditText: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure TrackBarFontSizeChange(Sender: TObject);
  private
    FBlend2DFontFace: IBLFontFace;
    FBlend2DFont: IBLFont;
    {$IFDEF USE_SKIA}
    FSkiaTypeface: ISKTypeface;
    {$ENDIF}
  protected
    procedure BeforeRender; override;
    procedure RenderFireMonkey(const ACanvas: TCanvas); override;
    procedure RenderBlend2D(const AContext: IBLContext); override;
    {$IFDEF USE_SKIA}
    procedure RenderSkia(const ACanvas: ISKCanvas); override;
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

procedure TFormMain.FormCreate(Sender: TObject);
var
  Stream: TResourceStream;
  FontData: TBytes;
  Blend2DFontData: IBLFontData;
  {$IFDEF USE_SKIA}
  SkiaFontData: ISKData;
  {$ENDIF}
begin
  inherited;
  Stream := TResourceStream.Create(HInstance, 'NOTO_SANS_REGULAR', RT_RCDATA);
  try
    SetLength(FontData, Stream.Size);
    Stream.ReadBuffer(FontData, Length(FontData));
  finally
    Stream.Free;
  end;

  Blend2DFontData := TBLFontData.Create;
  Blend2DFontData.InitializeFromData(FontData);

  FBlend2DFontFace := TBLFontFace.Create;
  FBlend2DFontFace.InitializeFromData(Blend2DFontData, 0);

  FBlend2DFont := TBLFont.Create;
  FBlend2DFont.InitializeFromFace(FBlend2DFontFace, TrackBarFontSize.Value);

  {$IFDEF USE_SKIA}
  SkiaFontData := TSKData.CreateCopy(FontData);
  FSkiaTypeface := TSKTypeface.FromData(SkiaFontData);
  FSkiaFill.Typeface := FSkiaTypeface;
  {$ENDIF}
end;

procedure TFormMain.RenderBlend2D(const AContext: IBLContext);
begin
  AContext.FillColor := TAlphaColors.Black;
  AContext.FillAll;

  AContext.FillColor := TAlphaColors.White;
  AContext.FillText(BLPoint(10, 10 + FBlend2DFont.Size), FBlend2DFont, EditText.Text);
end;

procedure TFormMain.RenderFireMonkey(const ACanvas: TCanvas);
begin
  { Note: custom fonts are not (easily) supported with FireMonkey.
    So we assume that the Noto Sans font is installed on the system. }
  ACanvas.Clear(TAlphaColors.Black);
  ACanvas.Font.Family := 'Noto Sans';
  ACanvas.Font.Size := TrackBarFontSize.Value;
  ACanvas.Fill.Color := TAlphaColors.White;
  ACanvas.FillText(RectF(10, 6, PaintBox.Width, PaintBox.Height), EditText.Text,
    False, 1, [], TTextAlign.Leading, TTextAlign.Leading);
end;

{$IFDEF USE_SKIA}
procedure TFormMain.RenderSkia(const ACanvas: ISKCanvas);
var
  TextSize: Single;
begin
  ACanvas.Clear(TAlphaColors.Black);
  TextSize := TrackBarFontSize.Value;
  FSkiaFill.TextSize := TextSize;
  FSkiaFill.Color := TAlphaColors.White;
  ACanvas.DrawText(EditText.Text, 10, 10 + TextSize, FSkiaFill);
end;
{$ENDIF}

procedure TFormMain.TrackBarFontSizeChange(Sender: TObject);
begin
  inherited;
  if Assigned(FBlend2DFont) then
  begin
    FBlend2DFont.Reset;
    FBlend2DFont.InitializeFromFace(FBlend2DFontFace, TrackBarFontSize.Value);
  end;
end;

end.
