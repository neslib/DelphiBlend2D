unit FMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Math,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  Blend2D.Fmx.PaintBox, Blend2D, FMX.Controls.Presentation, FMX.Edit,
  FMX.EditBox, FMX.SpinBox, FMX.StdCtrls;

type
  TFormMain = class(TForm)
    Rectangle1: TRectangle;
    Blend2DPaintBox1: TBlend2DPaintBox;
    Image1: TImage;
    Rectangle2: TRectangle;
    Rectangle3: TRectangle;
    Rectangle4: TRectangle;
    Rectangle5: TRectangle;
    Rectangle6: TRectangle;
    Rectangle7: TRectangle;
    Line1: TLine;
    Rectangle8: TRectangle;
    Rectangle9: TRectangle;
    Line2: TLine;
    Rectangle10: TRectangle;
    Rectangle11: TRectangle;
    Rectangle12: TRectangle;
    Rectangle13: TRectangle;
    Rectangle14: TRectangle;
    Rectangle15: TRectangle;
    Ellipse1: TEllipse;
    RoundRect1: TRoundRect;
    Path1: TPath;
    TimerUpate: TTimer;
    PaintBox1: TPaintBox;
    Label1: TLabel;
    Rectangle16: TRectangle;
    procedure FormCreate(Sender: TObject);
    procedure Blend2DPaintBox1Paint(const ASender: TObject;
      const AContext: TBLContext);
    procedure TimerUpateTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
  private
    { Private declarations }
    FBitmap: TBitmap;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses
  System.IOUtils;

{$R *.fmx}

procedure TFormMain.Blend2DPaintBox1Paint(const ASender: TObject;
  const AContext: TBLContext);
begin
//  AContext.ClearAll;
  var G: TBLGradient;

  var W: Double := Blend2DPaintBox1.Width;
  var H: Double := Blend2DPaintBox1.Height;

{//  G.Make(BLLinearGradientValues(0.919549822807312000, 1, 0.080450169742107390, 0.000000004811006704));
  G.Make(BLLinearGradientValues(W * 0.919549822807312000, H * 1, W * 0.080450169742107390, H * 0.000000004811006704));
  G.AddStop(0, TAlphaColors.Red);
  G.AddStop(0.322981357574462900, TAlphaColors.Yellow);
  G.AddStop(0.658385097980499300, $00FFFF00);
  G.AddStop(1, TAlphaColors.Lime); {}

  G.Make(BLRadialGradientValues(0.3, 0.7, 0.3, 0.7, 0.5));//Min(W/2, H/2), Min(W/2, H/2)));
//  G.Make(BLRadialGradientValues(0.5*W, 0.5*H, 0.5*W, 0.5*H, 0.5*W, 0));//Min(W/2, H/2), Min(W/2, H/2)));
  G.AddStop(1-0, TAlphaColors.Red);
  G.AddStop(1-0.322981357574462900, TAlphaColors.Yellow);
  G.AddStop(1-0.658385097980499300, $00FFFF00);
  G.AddStop(1-1, TAlphaColors.Lime);

  G.Scale(W, H);

  AContext.FillRect(0, 0, W, H, G);
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;

  FBitmap := TBitmap.Create(50, 50);
  FBitmap.Canvas.BeginScene;
  FBitmap.Canvas.Clear(0);
  FBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
  FBitmap.Canvas.Fill.Color := TAlphaColors.Yellow;
  FBitmap.Canvas.FillEllipse(RectF(0, 0, 50, 50), 0.8);
  var State := FBitmap.Canvas.SaveState;
  FBitmap.Canvas.RestoreState(State);

  var TempBitmap := TBitmap.Create(30, 30);
  TempBitmap.Canvas.BeginScene;
  TempBitmap.Canvas.Clear(0);
  TempBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
  TempBitmap.Canvas.Fill.Color := TAlphaColors.Red;
  TempBitmap.Canvas.FillEllipse(RectF(0, 0, 30, 30), 0.8);
  FBitmap.Canvas.DrawBitmap(TempBitmap, RectF(0, 0, 30, 30),
    RectF(10, 10, 40, 40), 1);
  TempBitmap.Free;

  FBitmap.Canvas.EndScene;

  Rectangle3.Fill.Gradient.RadialTransform.RotationCenter.X := 0.3;
  Rectangle3.Fill.Gradient.RadialTransform.RotationCenter.Y := 0.7;

  var Stream := TFileStream.Create('Resources\ABeeZee-Regular.ttf', fmOpenRead or fmShareDenyWrite);
  try
    Canvas.LoadFontFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FBitmap.Free;
end;

procedure TFormMain.PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
begin
  Canvas.DrawBitmap(FBitmap, RectF(0, 0, FBitmap.Width, FBitmap.Height),
    RectF(0, 0, PaintBox1.Width, PaintBox1.Height), 0.8);
end;

procedure TFormMain.TimerUpateTimer(Sender: TObject);
begin
  var ControlObj: TObject := nil;
  var Control := Hovered;
  if (Control <> nil) then
    ControlObj := Control.GetObject;

  if (ControlObj = nil) then
  begin
    Caption := 'Hovered: None';
    Exit;
  end;

  var Name := '';
  if (ControlObj is TFmxObject) then
    Name := TFmxObject(ControlObj).Name;

  if (Name = '') then
    Name := ControlObj.ClassName;

  Caption := 'Hovered: ' + Name;
end;

end.
