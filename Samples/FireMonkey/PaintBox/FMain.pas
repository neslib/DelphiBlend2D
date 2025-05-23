unit FMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Math,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Layouts,
  Blend2D,
  Blend2D.Fmx.PaintBox;

type
  TFormMain = class(TForm)
    Blend2DPaintBox: TBlend2DPaintBox;
    CheckBoxLowRes: TCheckBox;
    TrackBarX: TTrackBar;
    TrackBarY: TTrackBar;
    TrackBarZoom: TTrackBar;
    TrackBarRotate: TTrackBar;
    LayoutSettings: TLayout;
    GridPanelLayout: TGridPanelLayout;
    procedure Blend2DPaintBoxPaint(const ASender: TObject;
      const AContext: TBLContext);
    procedure FormCreate(Sender: TObject);
    procedure CheckBoxLowResChange(Sender: TObject);
    procedure TrackBarChange(Sender: TObject);
  private
    { Private declarations }
    FRadial: TBLGradient;
    FLinear: TBLGradient;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

procedure TFormMain.Blend2DPaintBoxPaint(const ASender: TObject;
  const AContext: TBLContext);
begin
  { Apply trackbar settings }
  AContext.Rotate(DegToRad(TrackBarRotate.Value), 120, 120);
  AContext.Scale(TrackBarZoom.Value);
  AContext.Translate(-TrackBarX.Value, -TrackBarY.Value);

  { Fill background with black }
  AContext.CompOp := TBLCompOp.SrcCopy;
  AContext.FillAll(TAlphaColors.Black);

  { Draw circle with radial gradient }
  AContext.CompOp := TBLCompOp.SrcOver;
  AContext.FillCircle(90, 90, 80, FRadial);

  { Draw rounded rectangle with linear gradient.
    Use Difference composition where circle and rectangle overlap. }
  AContext.CompOp := TBLCompOp.Difference;
  AContext.FillRoundRect(97.5, 97.5, 135, 135, 12.5, FLinear);
end;

procedure TFormMain.CheckBoxLowResChange(Sender: TObject);
begin
  Blend2DPaintBox.LowResolution := CheckBoxLowRes.IsChecked;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FRadial := TBLGradient.Create(BLRadialGradientValues(90, 90, 90, 90, 90));
  FRadial.AddStop(0.0, BLRgba32($FFFFFFFF));
  FRadial.AddStop(1.0, BLRgba32($FFFF6F3F));

  FLinear := TBLGradient.Create(BLLinearGradientValues(97.5, 97.5, 235, 235));
  FLinear.AddStop(0.0, BLRgba32($FFFFFFFF));
  FLinear.AddStop(1.0, BLRgba32($FF3F9FFF));
end;

procedure TFormMain.TrackBarChange(Sender: TObject);
begin
  Blend2DPaintBox.Repaint;
end;

end.
