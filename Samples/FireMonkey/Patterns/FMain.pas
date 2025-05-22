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
  {$IFDEF USE_SKIA}
  System.Skia,
  {$ENDIF}
  Blend2D,
  FBase;

type
  TFormMain = class(TFormBase)
    ToolBar2: TToolBar;
    LabelFracX: TLabel;
    TrackBarFracX: TTrackBar;
    LayoutFracX: TLayout;
    LayoutFracY: TLayout;
    LabelFracY: TLabel;
    TrackBarFracY: TTrackBar;
    ToolBar3: TToolBar;
    LayoutAngle: TLayout;
    LabelAngle: TLabel;
    TrackBarAngle: TTrackBar;
    LayoutScale: TLayout;
    LabelScale: TLabel;
    TrackBarScale: TTrackBar;
    CheckBoxBilinear: TCheckBox;
    CheckBoxFillPath: TCheckBox;
    LabelExtendMode: TLabel;
    LayoutCompOp: TLayout;
    ComboBoxExtendMode: TComboBox;
    procedure ToolBar2Resize(Sender: TObject);
    procedure ToolBar3Resize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FSprite: TBLImage;
    function TX: Double; inline;
    function TY: Double; inline;
    function AngleRad: Double; inline;
    function Scale: Double; inline;
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

uses
  System.Math,
  Images;

{ TFormMain }

function TFormMain.AngleRad: Double;
begin
  Result := (TrackBarAngle.Value / (3600 / 2)) * Pi;
end;

procedure TFormMain.BeforeRender;
begin
  inherited;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  inherited;
  FSprite.ReadFromData(@RESOURCE_BABELFISH_PNG, SizeOf(RESOURCE_BABELFISH_PNG));
end;

procedure TFormMain.RenderBlend2D(const AContext: TBLContext);
begin
  var RX: Double := PaintBox.Width * 0.5;
  var RY: Double := PaintBox.Height * 0.5;

  var Pattern := TBLPattern.Create(FSprite, TBLExtendMode(ComboBoxExtendMode.ItemIndex));
  Pattern.Rotate(AngleRad, RX, RY);
  Pattern.Translate(TX, TY);
  Pattern.Scale(Scale);

  if (CheckBoxBilinear.IsChecked) then
    AContext.PatternQuality := TBLPatternQuality.Bilinear
  else
    AContext.PatternQuality := TBLPatternQuality.Nearest;

  AContext.CompOp := TBLCompOp.SrcCopy;

  if (CheckBoxFillPath.IsChecked) then
  begin
    AContext.ClearAll;
    AContext.FillCircle(RX, RY, Min(RX, RY), Pattern);
  end
  else
    AContext.FillAll(Pattern);
end;

procedure TFormMain.RenderFireMonkey(const ACanvas: TCanvas);
begin
  ACanvas.Clear(TAlphaColors.Black);
  RenderNotSupported;
end;

function TFormMain.Scale: Double;
begin
  Result := (TrackBarScale.Value + 100) / 100;
end;

{$IFDEF USE_SKIA}
procedure TFormMain.RenderSkia(const ACanvas: ISkCanvas);
begin
  ACanvas.Clear(TAlphaColors.Black);
  RenderNotSupported;
end;
{$ENDIF}

procedure TFormMain.ToolBar2Resize(Sender: TObject);
begin
  inherited;
  LayoutFracX.Width := Trunc(ToolBar2.Width) div 2;
end;

procedure TFormMain.ToolBar3Resize(Sender: TObject);
begin
  inherited;
  LayoutAngle.Width := Trunc(ToolBar3.Width) div 2;
end;

function TFormMain.TX: Double;
begin
  Result := 256 + (TrackBarFracX.Value / 256);
end;

function TFormMain.TY: Double;
begin
  Result := 256 + (TrackBarFracY.Value / 256);
end;

end.
