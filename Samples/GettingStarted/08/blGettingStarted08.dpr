program blGettingStarted08;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.UITypes,
  Blend2D in '..\..\..\Blend2D.pas';

procedure Run;
const
  TEXTS: array [0..2] of String = (
    'Hello Blend2D!',
    'I''m a simple multiline text example',
    'that uses GlyphBuffer and GlyphRun!');
begin
  ReportMemoryLeaksOnShutdown := True;
  var Image := TBLImage.Create(480, 480, TBLFormat.Prgb32);
  var Context := TBLContext.Create(Image);

  var Face: TBLFontFace;
  Face.MakeFromFile('Resources/ABeeZee-Regular.ttf');

  var Font: TBLFont;
  Font.MakeFromFace(Face, 20);

  var GlyphBuffer: TBLGlyphBuffer;
  var TextMetrics: TBLTextMetrics;
  var FontMetrics := Font.Metrics;
  var Y: Double := 190 + FontMetrics.Ascent;

  Context.ClearAll;

  for var I := 0 to Length(TEXTS) - 1 do
  begin
    GlyphBuffer.SetText(TEXTS[I]);
    Font.Shape(GlyphBuffer);
    Font.GetTextMetrics(GlyphBuffer, TextMetrics);

    var X: Double := (TextMetrics.BoundingBox.X1 - TextMetrics.BoundingBox.X0);
    Context.FillGlyphRun(BLPoint((480 - X) / 2, Y), Font,
      GlyphBuffer.GlyphRun, $FF000000);

    Y := Y + FontMetrics.Ascent + FontMetrics.Descent + FontMetrics.LineGap;
  end;

  Context.Finish;

  Image.WriteToFile('blGettingStarted08.png');
end;

begin
  try
    Run;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
