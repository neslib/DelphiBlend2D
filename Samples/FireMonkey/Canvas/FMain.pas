unit FMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Math,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  Blend2D.Fmx.PaintBox, Blend2D;

type
  TFormMain = class(TForm)
    Rectangle1: TRectangle;
    Blend2DPaintBox1: TBlend2DPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure Blend2DPaintBox1Paint(const ASender: TObject;
      const AContext: TBLContext);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

procedure TFormMain.Blend2DPaintBox1Paint(const ASender: TObject;
  const AContext: TBLContext);
begin
//  AContext.ClearAll;
  var G: TBLGradient;

  var W: Double := Blend2DPaintBox1.Width;
  var H: Double := Blend2DPaintBox1.Height;

//  G.Make(BLLinearGradientValues(0.919549822807312000, 1, 0.080450169742107390, 0.000000004811006704));
  G.Make(BLLinearGradientValues(W * 0.919549822807312000, H * 1, W * 0.080450169742107390, H * 0.000000004811006704));
  G.AddStop(0, TAlphaColors.Red);
  G.AddStop(0.322981357574462900, TAlphaColors.Yellow);
  G.AddStop(0.658385097980499300, $00FFFF00);
  G.AddStop(1, TAlphaColors.Lime); {}

{  G.Make(BLRadialGradientValues(0.5, 0.5, 0.5, 0.5, 0.5));//Min(W/2, H/2), Min(W/2, H/2)));
  G.AddStop(1-0, TAlphaColors.Red);
  G.AddStop(1-0.322981357574462900, TAlphaColors.Yellow);
  G.AddStop(1-0.658385097980499300, $00FFFF00);
  G.AddStop(1-1, TAlphaColors.Lime);}

//  G.Scale(W, H);

  AContext.FillRect(0, 0, W, H, G);
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
end;

end.
