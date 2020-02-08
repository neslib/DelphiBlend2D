unit FMain;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.UITypes,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Blend2D,
  Blend2D.Vcl.PaintBox;

type
  TFormMain = class(TForm)
    PanelSettings: TPanel;
    CheckBoxLowRes: TCheckBox;
    GridPanel: TGridPanel;
    TrackBarX: TTrackBar;
    TrackBarY: TTrackBar;
    TrackBarZoom: TTrackBar;
    TrackBarRotate: TTrackBar;
    Blend2DPaintBox: TBlend2DPaintBox;
    procedure TrackBarChange(Sender: TObject);
    procedure CheckBoxLowResClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Blend2DPaintBoxPaint(const ASender: TObject;
      const AContext: IBLContext);
  private
    { Private declarations }
    FRadial: IBLGradient;
    FLinear: IBLGradient;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.Blend2DPaintBoxPaint(const ASender: TObject;
  const AContext: IBLContext);
begin
  { Apply trackbar settings }
  AContext.Rotate(DegToRad(TrackBarRotate.Position), 120, 120);
  AContext.Scale(TrackBarZoom.Position / 100);
  AContext.Translate(-TrackBarX.Position, -TrackBarY.Position);

  { Fill background with black }
  AContext.CompOp := TBLCompOp.SrcCopy;
  AContext.FillColor := TAlphaColors.Black;
  AContext.FillAll;

  { Draw circle with radial gradient }
  AContext.CompOp := TBLCompOp.SrcOver;
  AContext.FillGradient := FRadial;
  AContext.FillCircle(90, 90, 80);

  { Draw rounded rectangle with linear gradient.
    Use Difference composition where circle and rectangle overlap. }
  AContext.CompOp := TBLCompOp.Difference;
  AContext.FillGradient := FLinear;
  AContext.FillRoundRect(97.5, 97.5, 135, 135, 12.5);
end;

procedure TFormMain.CheckBoxLowResClick(Sender: TObject);
begin
  Blend2DPaintBox.LowResolution := CheckBoxLowRes.Checked;
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
