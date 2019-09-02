{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{ Copyright(c) 2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit FMX.FontGlyphs.Android;

interface

uses
  System.Types, System.Classes, System.SysUtils, System.UITypes, System.UIConsts, System.Generics.Collections,
  FMX.Types, FMX.Surfaces, FMX.FontGlyphs, FMX.PixelFormats,
  Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNIBridge;

{$SCOPEDENUMS ON}

type

  TAndroidFontGlyphManager = class(TFontGlyphManager)
  private
    FPaint: JPaint;
    //Current metrics
    FSpacing: Single;
    FTop: Single;
    FTopInt: Integer;
    FAscent: Single;
    FDescent: Single;
    FBottom: Single;
    FBottomInt: Integer;
    FLeading: Single;
    FLeadingInt: Integer;
  protected
    procedure LoadResource; override;
    procedure FreeResource; override;
    function DoGetGlyph(const Char: UCS4Char; const Settings: TFontGlyphSettings): TFontGlyph; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  System.Math, System.Character,
  Androidapi.Bitmap,
  FMX.Graphics;

{ TAndroidFontGlyphManager }

constructor TAndroidFontGlyphManager.Create;
begin
  inherited Create;
  FPaint := TJPaint.Create;
end;

destructor TAndroidFontGlyphManager.Destroy;
begin
  FPaint := nil;
  inherited;
end;

procedure TAndroidFontGlyphManager.LoadResource;
const
  BoldAndItalic = [TFontStyle.fsBold, TFontStyle.fsItalic];
var
  TypefaceFlag: Integer;
  Typeface: JTypeface;
  FamilyName: JString;
  Metrics: JPaint_FontMetrics;
  MetricsInt: JPaint_FontMetricsInt;
begin
  FPaint.setAntiAlias(True);
  FPaint.setTextSize(CurrentSettings.Size * CurrentSettings.Scale);
  FPaint.setARGB(255, 255, 255, 255);
  FPaint.setUnderlineText(TFontStyle.fsUnderline in CurrentSettings.Style);
  FPaint.setStrikeThruText(TFontStyle.fsStrikeOut in CurrentSettings.Style);
  if TOSVersion.Check(4, 0) then
    FPaint.setHinting(TJPaint.JavaClass.HINTING_ON);
  //Font
  try
    FamilyName := StringToJString(CurrentSettings.Family);
    if (BoldAndItalic * CurrentSettings.Style) = BoldAndItalic then
      TypefaceFlag := TJTypeface.JavaClass.BOLD_ITALIC
    else
      if TFontStyle.fsBold in CurrentSettings.Style then
        TypefaceFlag := TJTypeface.JavaClass.BOLD
      else
        if TFontStyle.fsItalic in CurrentSettings.Style then
          TypefaceFlag := TJTypeface.JavaClass.ITALIC
        else
          TypefaceFlag := TJTypeface.JavaClass.NORMAL;
    Typeface := TJTypeface.JavaClass.create(FamilyName, TypefaceFlag);
    FPaint.setTypeface(Typeface);
    try
      Metrics := FPaint.getFontMetrics;
      MetricsInt := FPaint.getFontMetricsInt;
      //
      FSpacing := FPaint.getFontMetrics(Metrics);
      FTop := Metrics.top;
      FTopInt := MetricsInt.top;
      FAscent := Metrics.ascent;
      FDescent := Metrics.descent;
      FBottom := Metrics.bottom;
      FBottomInt := MetricsInt.bottom;
      FLeading := Metrics.leading;
      FLeadingInt := MetricsInt.leading;

//      SysDebug(FloatToStr(CurrentSettings.Size) + ':' + FloatToStr(CurrentSettings.Scale));
//      Log.d(Format('Top=(%d %f) Bottom=(%d %f) Leading=(%d %f) FAscent=(%d %f)', [FTopInt, FTop, FBottomInt, FBottom, FLeadingInt, FLeading, MetricsInt.ascent, FAscent]));
    finally
      Metrics := nil;
      MetricsInt := nil;
    end;
  finally
    FamilyName := nil;
    Typeface := nil;
  end;
end;

procedure TAndroidFontGlyphManager.FreeResource;
begin
  if Assigned(FPaint) then
    FPaint.reset;
end;

function TAndroidFontGlyphManager.DoGetGlyph(const Char: UCS4Char; const Settings: TFontGlyphSettings): TFontGlyph;
var
  Text: JString;
  Bitmap: JBitmap;
  Canvas: JCanvas;
  GlyphRect: TRect;
  C, I, J, Width, Height: Integer;
  Advance: Single;
  Bounds: JRect;
  GlyphStyle: TFontGlyphStyles;
  PixelBuffer: Pointer;
  Data: PIntegerArray;
  Path: JPath;
  PathMeasure: JPathMeasure;
  PathLength: Single;
  Coords: TJavaArray<Single>;
  StartPoint, LastPoint, Point: TPointF;
  NewContour, HasStartPoint: Boolean;
begin
  try
    Text := StringToJString(System.Char.ConvertFromUtf32(Char));
    Advance := FPaint.measureText(Text);
//    SysDebug(Format('%s %f', [System.Char.ConvertFromUtf32(Char), Advance]));
    Height := Abs(FTopInt) + Abs(FBottomInt) + 2;
    Width := Ceil(Abs(Advance)) + 2;
    try
      Bitmap := TJBitmap.JavaClass.createBitmap(Width, Height, TJBitmap_Config.JavaClass.ARGB_8888);
      try
        Bounds := TJRect.Create;
        FPaint.getTextBounds(Text, 0, Text.length, Bounds);
//        Log.d(Format('Bounds=(%d %d %d %d) %d %d ', [Bounds.left, Bounds.top, Bounds.right, Bounds.bottom, Bounds.width, Bounds.height]));
        try
          Canvas := TJCanvas.JavaClass.init(Bitmap);
          Canvas.drawText(Text, 0, -Trunc(FAscent), FPaint);
        finally
          Canvas := nil;
        end;

        GlyphStyle := [];
        if ((FAscent = 0) and (FDescent = 0)) or not HasGlyph(Char) then
          GlyphStyle := [TFontGlyphStyle.NoGlyph];
        if TFontGlyphSetting.gsPath in Settings then
          GlyphStyle := GlyphStyle + [TFontGlyphStyle.HasPath];


        Result := TFontGlyph.Create(TPoint.Create(Bounds.left, Abs(FTopInt -  Bounds.top)),
          Advance, Abs(FTopInt) + Abs(FBottomInt) + Abs(FLeadingInt), GlyphStyle);

        if (TFontGlyphSetting.gsBitmap in Settings) and
           (HasGlyph(Char) or ((FAscent <> 0) or (FDescent <> 0))) and
           (AndroidBitmap_lockPixels(TJNIResolver.GetJNIEnv, (Bitmap as ILocalObject).GetObjectID, @PixelBuffer) = 0) then
        begin
          Data := PIntegerArray(PixelBuffer);
          GlyphRect.Left := Bounds.left;
          GlyphRect.Right := Bounds.Right;
          GlyphRect.Top := Abs(Trunc(FAscent) - Bounds.top);
          GlyphRect.Bottom := Abs(Trunc(FAscent) - Bounds.bottom);
//          Log.d(Format('GlyphRect=(%d %d %d %d) %d %d', [GlyphRect.Left, GlyphRect.Top, GlyphRect.Right, GlyphRect.Bottom, GlyphRect.Width, GlyphRect.Height]));

          if (GlyphRect.Width > 0) or (GlyphRect.Height > 0) then
          begin
            Result.Bitmap.SetSize(GlyphRect.Width + 1, GlyphRect.Height + 1, TPixelFormat.pfA8R8G8B8);
            if TFontGlyphSetting.gsPremultipliedAlpha in Settings then
            begin
              for I := GlyphRect.Top to GlyphRect.Bottom do
                Move(Data[I * Width + Max(GlyphRect.Left, 0)],
                  Result.Bitmap.GetPixelAddr(0, I - GlyphRect.Top)^, Result.Bitmap.Pitch);
            end
            else
              for I := GlyphRect.Top to GlyphRect.Bottom - 1 do
                for J := GlyphRect.Left to GlyphRect.Right - 1 do
                begin
                  C := Data[I * Width + J];
                  if C <> 0 then
                  begin
                    C := ((C shr 16) and $FF + (C shr 8) and $FF + (C and $FF)) div 3;
                    Result.Bitmap.Pixels[J - GlyphRect.Left, I - GlyphRect.Top] := MakeColor($FF, $FF, $FF, C);
                  end
                end;
          end;
          AndroidBitmap_unlockPixels(TJNIResolver.GetJNIEnv, (Bitmap as ILocalObject).GetObjectID);
        end;
        //Path
        if TFontGlyphSetting.gsPath in Settings then
        try
          Path := TJPath.Create;
          FPaint.getTextPath(Text, 0, Text.length, Result.Origin.X, Result.Origin.Y, Path);
          PathMeasure := TJPathMeasure.Create;
          PathMeasure.setPath(Path, False);
          Coords := TJavaArray<Single>.Create(2);
          if PathMeasure.getLength > 0 then
          repeat
            PathLength := PathMeasure.getLength;
            NewContour := True;
            HasStartPoint := False;
            I := 0;
            while I < PathLength do
            begin
              if PathMeasure.getPosTan(I, Coords, nil) then
              begin
                Point := PointF(Coords[0], Coords[1]);
                if NewContour then
                begin
                  Result.Path.MoveTo(Point);
                  NewContour := False;
                  HasStartPoint := False;
                end
                else
                  if Point <> LastPoint then
                  begin
                    if HasStartPoint and (LastPoint <> StartPoint) then
                      if not SameValue(((Point.Y - StartPoint.Y) / (Point.X - StartPoint.X)), ((Point.Y - LastPoint.Y) / (Point.X - LastPoint.X)), Epsilon) then
                      begin
                        Result.Path.LineTo(Point);
                        HasStartPoint := False;
                      end
                      else
                    else
                      Result.Path.LineTo(Point);
                  end;
                LastPoint := Point;
                if not HasStartPoint then
                begin
                  StartPoint := Point;
                  HasStartPoint := True;
                end;
              end;
              Inc(I);
            end;
            if Result.Path.Count > 0 then
              Result.Path.ClosePath;
          until not PathMeasure.nextContour;
          Point := Result.Path.GetBounds.TopLeft;
          Result.Path.Translate(-Point.X + Result.Origin.X, -Point.Y + Result.Origin.Y);
        finally
          FreeAndNil(Coords);
          Path := nil;
          PathMeasure := nil;
        end;
      finally
        Bounds := nil;
      end;
    finally
      Bitmap.recycle;
      Bitmap := nil;
    end;
  finally
    Text := nil;
  end;
end;

end.
