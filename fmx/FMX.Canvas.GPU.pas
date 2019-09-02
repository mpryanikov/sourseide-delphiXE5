{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit FMX.Canvas.GPU;

{$SCOPEDENUMS ON}

interface

uses
  System.Types, System.UITypes, System.UIConsts, System.Classes, System.Generics.Collections, FMX.Types, FMX.Types3D,
  FMX.Messages, FMX.Graphics, FMX.Canvas.GPU.Helpers;

type
  TCustomCanvasGpu = class;

  TBitmapCtx = class
  private
    [Weak] FBitmap: TBitmap;
    [Weak] FCanvas: TCustomCanvasGpu;
    FTexture: TTexture;
    FRenderTarget: TTexture;
    FAccess: TMapAccess;
    FData: Pointer;
    FContextLostId: Integer;
    procedure ContextLostHandler(const Sender: TObject; const Msg: TMessage);
    function GetTexture: TTexture;
    function GetRenderTarget: TTexture;
    function GetPaintingTexture: TTexture;
    procedure CreateBuffer;
    procedure FreeBuffer;
  public
    constructor Create(const ABitmap: TBitmap);
    destructor Destroy; override;
    property Access: TMapAccess read FAccess write FAccess;
    property Canvas: TCustomCanvasGpu read FCanvas write FCanvas;
    property Texture: TTexture read GetTexture;
    property RenderTarget: TTexture read GetRenderTarget;
    property PaintingTexture: TTexture read GetPaintingTexture;
  end;

  TCustomCanvasGpu = class(TCanvas)
  private
  class var
    FModulateColor: TAlphaColor;
    FAlignToPixels: Boolean;
    FFlushCountPerFrame: Integer;
    FPrimitiveCountPerFrame: Integer;
  private
    FContext: TContext3D;
  public
    procedure DrawTexture(const ARect, ATexRect: TRectF; const AColor: TAlphaColor;
      const ATexture: TTexture); virtual; abstract;
    procedure Flush; virtual; abstract;
    class property ModulateColor: TAlphaColor read FModulateColor write FModulateColor;
    class property AlignToPixels: Boolean read FAlignToPixels write FAlignToPixels;
    class property FlushCountPerFrame: Integer read FFlushCountPerFrame;
    class property PrimitiveCountPerFrame: Integer read FPrimitiveCountPerFrame;
    property Context: TContext3D read FContext;
  end;

procedure RegisterCanvasClasses;
procedure UnregisterCanvasClasses;

implementation

uses
  System.Math, FMX.Consts, FMX.Platform, FMX.Forms, FMX.TextLayout, FMX.TextLayout.GPU, FMX.StrokeBuilder;

type
  TCanvasSaveStateCtx = class(TCanvasSaveState)
  private
    FClippingEnabled: Boolean;
    FSavedClipRect: TRect;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure Assign(Source: TPersistent); override;
    property SavedClipRect: TRect read FSavedClipRect;
  end;

  TCanvasGpu = class(TCustomCanvasGpu)
  private class var
    FCurrentCanvas: TCanvasGpu;
    FGlobalBeginScene: Integer;
    FCanvasHelper: TCanvasHelper;
    FStrokeBuilder: TStrokeBuilder;
  private
    FSaveCanvas: TCanvasGpu;
    FClippingEnabled: Boolean;
    FSavedScissorRect: TRect;
    class procedure CreateResources;
    class procedure FreeResources;
  private
    procedure AlignToPixel(var Point: TPointF); inline;
    function TransformBounds(const Bounds: TCornersF): TCornersF; inline;
    procedure TransformCallback(var Position: TPointF);
  protected
    function TransformPoint(const P: TPointF): TPointF; reintroduce; inline;
    function TransformRect(const Rect: TRectF): TRectF; reintroduce; inline;

    function CreateSaveState: TCanvasSaveState; override;
    { begin and }
    function DoBeginScene(const AClipRects: PClipRects = nil; AContextHandle: THandle = 0): Boolean; override;
    procedure DoEndScene; override;
    function GetCanvasScale: Single; override;
    { creation }
    constructor CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer;
      const AQuality: TCanvasQuality = TCanvasQuality.ccSystemDefault); override;
    constructor CreateFromBitmap(const ABitmap: TBitmap; const AQuality: TCanvasQuality = TCanvasQuality.ccSystemDefault); override;
    constructor CreateFromPrinter(const APrinter: TAbstractPrinter); override;
    { Bitmaps }
    class procedure DoInitializeBitmap(const Bitmap: TBitmap); override;
    class procedure DoFinalizeBitmap(const Bitmap: TBitmap); override;
    class function DoMapBitmap(const Bitmap: TBitmap; const Access: TMapAccess; var Data: TBitmapData): Boolean; override;
    class procedure DoUnmapBitmap(const Bitmap: TBitmap; var Data: TBitmapData); override;
    { states }
    procedure DoBlendingChanged; override;
    { drawing }
    procedure DoFillRect(const ARect: TRectF; const AOpacity: Single; const ABrush: TBrush); override;
    procedure DoFillPath(const APath: TPathData; const AOpacity: Single; const ABrush: TBrush); override;
    procedure DoFillEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TBrush); override;
    procedure DoDrawBitmap(const ABitmap: TBitmap; const SrcRect, DstRect: TRectF; const AOpacity: Single;
      const HighSpeed: Boolean = False); override;
    procedure DoDrawLine(const APt1, APt2: TPointF; const AOpacity: Single; const ABrush: TStrokeBrush); override;
    procedure DoDrawRect(const ARect: TRectF; const AOpacity: Single; const ABrush: TStrokeBrush); override;
    procedure DoDrawPath(const APath: TPathData; const AOpacity: Single; const ABrush: TStrokeBrush); override;
    procedure DoDrawEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TStrokeBrush); override;
  public
    destructor Destroy; override;
    class function GetCanvasStyle(): TCanvasStyles; override;
    procedure SetSize(const AWidth, AHeight: Integer); override;
    { buffer }
    procedure Clear(const Color: TAlphaColor); override;
    procedure ClearRect(const ARect: TRectF; const AColor: TAlphaColor = 0); override;
    { cliping }
    procedure IntersectClipRect(const ARect: TRectF); override;
    procedure ExcludeClipRect(const ARect: TRectF); override;
    { drawing }
    procedure MeasureLines(const ALines: TLineMetricInfo; const ARect: TRectF; const AText: string; const WordWrap: Boolean;
      const Flags: TFillTextFlags; const ATextAlign: TTextAlign; const AVTextAlign: TTextAlign = TTextAlign.taCenter); override;
    function PtInPath(const APoint: TPointF; const APath: TPathData): Boolean; override;
    { external }
    procedure Flush; override;
    procedure DrawTexture(const DstRect, SrcRect: TRectF; const AColor: TAlphaColor; const ATexture: TTexture); override;
  end;

{ TCanvasSaveStateCtx }

procedure TCanvasSaveStateCtx.Assign(Source: TPersistent);
var
  LSource: TCanvasGpu;
begin
  inherited Assign(Source);
  if Source is TCanvasGpu then
  begin
    LSource := TCanvasGpu(Source);
    FClippingEnabled := LSource.FClippingEnabled;
    FSavedClipRect := LSource.FCanvasHelper.ScissorRect;
  end;
end;

procedure TCanvasSaveStateCtx.AssignTo(Dest: TPersistent);
var
  LDest: TCanvasGpu;
begin
  if Dest is TCanvasGpu then
  begin
    LDest := TCanvasGpu(Dest);
    LDest.FClippingEnabled := FClippingEnabled;
    if FClippingEnabled then
      LDest.FCanvasHelper.ScissorRect := FSavedClipRect
    else begin
      LDest.FCanvasHelper.ResetScissorRect;
      LDest.FCanvasHelper.SetScissorRectWithoutUpdate(FSavedClipRect);
    end;
  end;
  inherited AssignTo(Dest);
end;

{ TBitmapCtx }

constructor TBitmapCtx.Create(const ABitmap: TBitmap);
begin
  inherited Create;
  FBitmap := ABitmap;
  FAccess := TMapAccess.maRead;
  FContextLostId := TMessageManager.DefaultManager.SubscribeToMessage(TContextLostMessage, ContextLostHandler);
end;

destructor TBitmapCtx.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TContextLostMessage, FContextLostId);
  if Assigned(TCanvasGpu.FCanvasHelper) and (TCanvasGpu.FGlobalBeginScene > 0) then
    TCanvasGpu.FCanvasHelper.Flush;
  if Assigned(FRenderTarget) then
    FRenderTarget.DisposeOf;
  if Assigned(FTexture) then
    FTexture.DisposeOf;
  FreeBuffer;
  inherited;
end;

procedure TBitmapCtx.CreateBuffer;
begin
  if not Assigned(FData) then
    GetMem(FData, FBitmap.Height * FBitmap.BytesPerLine);
end;

procedure TBitmapCtx.FreeBuffer;
begin
  if Assigned(FData) then
  begin
    FreeMem(FData);
    FData := nil;
  end;
end;

procedure TBitmapCtx.ContextLostHandler(const Sender: TObject; const Msg: TMessage);
begin
  if Assigned(FTexture) then
  begin
    FTexture.DisposeOf;
    FTexture := nil;
  end;
end;

function TBitmapCtx.GetPaintingTexture: TTexture;
begin
  if Assigned(FRenderTarget) then
    Result := FRenderTarget
  else
    Result := Texture;
end;

function TBitmapCtx.GetRenderTarget: TTexture;
begin
  if not Assigned(FRenderTarget) then
  begin
    FRenderTarget := TTexture.Create;
    FRenderTarget.SetSize(FBitmap.Width, FBitmap.Height);
    FRenderTarget.Style := [TTextureStyle.tsRenderTarget];
    ITextureAccess(FRenderTarget).TextureScale := FBitmap.BitmapScale;
    if Assigned(FData) then
    begin
      FRenderTarget.Initialize;
      FRenderTarget.UpdateTexture(FData, FBitmap.BytesPerLine);
    end;
    if Assigned(FTexture) then
    begin
      FTexture.DisposeOf;
      FTexture := nil;
    end;
  end;
  Result := FRenderTarget;
end;

function TBitmapCtx.GetTexture: TTexture;
begin
  if not Assigned(FTexture) then
  begin
    FTexture := TTexture.Create;
    FTexture.SetSize(FBitmap.Width, FBitmap.Height);
    FTexture.Style := [TTextureStyle.tsDynamic, TTextureStyle.tsVolatile];
    if Assigned(FData) then
      FTexture.UpdateTexture(FData, FBitmap.BytesPerLine);
  end;
  Result := FTexture;
end;

{ TCanvasGpu }

function PrepareColor(const SrcColor: TAlphaColor; const Opacity: Single): TAlphaColor;
begin
  if Opacity < 1 then
  begin
    TAlphaColorRec(Result).R := Round(TAlphaColorRec(SrcColor).R * Opacity);
    TAlphaColorRec(Result).G := Round(TAlphaColorRec(SrcColor).G * Opacity);
    TAlphaColorRec(Result).B := Round(TAlphaColorRec(SrcColor).B * Opacity);
    TAlphaColorRec(Result).A := Round(TAlphaColorRec(SrcColor).A * Opacity);
  end
  else if (TAlphaColorRec(SrcColor).A < $FF) then
    Result := PremultiplyAlpha(SrcColor)
  else
    Result := SrcColor;
end;

constructor TCanvasGpu.CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer;
  const AQuality: TCanvasQuality = TCanvasQuality.ccSystemDefault);
var
  Multisample: TMultisample;
begin
  inherited CreateFromWindow(AParent, AWidth, AHeight, AQuality);
  case Quality of
    TCanvasQuality.ccHighPerformance: Multisample := TMultisample.msNone;
{$IFDEF CPUARM}
    TCanvasQuality.ccHighQuality: Multisample := TMultisample.ms4Samples;
{$ELSE}
    TCanvasQuality.ccHighQuality: Multisample := TMultisample.msNone;
{$ENDIF}
  else
    Multisample := TMultisample.msNone;
  end;
  FContext := TContextManager.CreateFromWindow(AParent, AWidth, AHeight, Multisample, True);
end;

constructor TCanvasGpu.CreateFromBitmap(const ABitmap: TBitmap;
  const AQuality: TCanvasQuality = TCanvasQuality.ccSystemDefault);
var
  Multisample: TMultisample;
  H: TBitmapCtx;
begin
  inherited CreateFromBitmap(ABitmap, AQuality);
  if not Bitmap.HandleAllocated then
    raise ECanvasException.Create('Handle not allocated');
  case Quality of
    TCanvasQuality.ccHighPerformance: Multisample := TMultisample.msNone;
    TCanvasQuality.ccHighQuality: Multisample := TMultisample.ms4Samples;
  else
    Multisample := TMultisample.msNone;
  end;
  H := TBitmapCtx(Bitmap.Handle);
  FContext := TContextManager.CreateFromTexture(H.RenderTarget, Multisample, True);
  H.Canvas := Self;
end;

constructor TCanvasGpu.CreateFromPrinter(const APrinter: TAbstractPrinter);
begin
  // Just a stub implementation - not used.
end;

destructor TCanvasGpu.Destroy;
begin
  if Assigned(Bitmap) then
    TBitmapCtx(Bitmap.Handle).Canvas := nil;
  FContext.DisposeOf;
  inherited Destroy;
end;

function TCanvasGpu.GetCanvasScale: Single;
var
  WindowSvc: IFMXWindowService;
begin
  if Assigned(Parent) and TPlatformServices.Current.SupportsPlatformService(IFMXWindowService, IInterface(WindowSvc)) then
    Result := WindowSvc.GetWindowScale(WindowSvc.FindForm(Parent))
  else if Assigned(Bitmap) then
    Result := Bitmap.BitmapScale
  else
    Result := inherited GetCanvasScale;
end;

class function TCanvasGpu.GetCanvasStyle: TCanvasStyles;
begin
  Result := [TCanvasStyle.NeedGPUSurface];
end;

function TCanvasGpu.CreateSaveState: TCanvasSaveState;
begin
  Result := TCanvasSaveStateCtx.Create;
end;

procedure TCanvasGpu.Clear(const Color: TAlphaColor);
begin
  if Assigned(FContext) then
  begin
    FCanvasHelper.Flush;
    FContext.Clear(Color);
  end;
end;

procedure TCanvasGpu.ClearRect(const ARect: TRectF; const AColor: TAlphaColor);
begin
  FCanvasHelper.Flush;
  try
    FContext.SetContextState(TContextState.csAlphaBlendOff);
    FCanvasHelper.FillRect(TransformBounds(CornersF(ARect)), AColor);
    FCanvasHelper.Flush;
  finally
    FContext.SetContextState(TContextState.csAlphaBlendOn);
  end;
end;

procedure TCanvasGpu.SetSize(const AWidth, AHeight: Integer);
begin
  inherited;
  if Assigned(FContext) then
    FContext.SetSize(Width, Height);
end;

procedure TCanvasGpu.AlignToPixel(var Point: TPointF);

  procedure AlignValue(var Value: Single; const Scale: Single); inline;
  begin
    Value := Trunc(Value * Scale) / Scale;
  end;

begin
  AlignValue(Point.X, Scale);
  AlignValue(Point.Y, Scale);
end;

function TCanvasGpu.TransformBounds(const Bounds: TCornersF): TCornersF;
begin
  case MatrixMeaning of
    TMatrixMeaning.mmUnknown:
      begin
        Result[0] := Bounds[0] * Matrix;
        Result[1] := Bounds[1] * Matrix;
        Result[2] := Bounds[2] * Matrix;
        Result[3] := Bounds[3] * Matrix;
      end;

    TMatrixMeaning.mmIdentity:
      Result := Bounds;

    TMatrixMeaning.mmTranslate:
      begin
        Result[0] := Bounds[0] + MatrixTranslate;
        Result[1] := Bounds[1] + MatrixTranslate;
        Result[2] := Bounds[2] + MatrixTranslate;
        Result[3] := Bounds[3] + MatrixTranslate;
      end;
  end;

  if FAlignToPixels then
  begin
    AlignToPixel(Result[0]);
    AlignToPixel(Result[1]);
    AlignToPixel(Result[2]);
    AlignToPixel(Result[3]);
  end;
end;

function TCanvasGpu.TransformPoint(const P: TPointF): TPointF;
begin
  Result := inherited;

  if FAlignToPixels then
    AlignToPixel(Result);
end;

function TCanvasGpu.TransformRect(const Rect: TRectF): TRectF;
begin
  Result := inherited;

  if FAlignToPixels then
  begin
    AlignToPixel(Result.TopLeft);
    AlignToPixel(Result.BottomRight);
  end;
end;

function TCanvasGpu.DoBeginScene(const AClipRects: PClipRects; AContextHandle: THandle): Boolean;
begin
  if FGlobalBeginScene = 0 then
  begin
    FCanvasHelper.SetContext(Context);
    FCanvasHelper.BeginRender;
    TTextLayoutNG.BeginRender;
  end
  else
  begin
    FCanvasHelper.Flush;
    FCanvasHelper.SetContext(Context);

    FContext.SetMatrix(TMatrix3D.Identity);
    FContext.SetContextState(TContextState.cs2DScene);
    FContext.SetContextState(TContextState.csAllFace);
    FContext.SetContextState(TContextState.csZWriteOff);
    FContext.SetContextState(TContextState.csZTestOff);
  end;
  Inc(FGlobalBeginScene);
  FSaveCanvas := FCurrentCanvas;
  if Assigned(FSaveCanvas) and FSaveCanvas.FClippingEnabled then
    FSavedScissorRect := FCanvasHelper.ScissorRect;

  FCurrentCanvas := Self;

  Result := inherited DoBeginScene(AClipRects) and Assigned(FContext) and FContext.BeginScene;
  if Result then
  begin
    FClippingEnabled := False;
    FCanvasHelper.ResetScissorRect;
    FCanvasHelper.UpdateDrawingMode;
  end;
end;

procedure TCanvasGpu.DoEndScene;
begin
  if Assigned(FContext) then
  begin
    FCanvasHelper.Flush;
    FContext.EndScene;
    if Assigned(FSaveCanvas) then
    begin
      FCanvasHelper.SetContext(FSaveCanvas.FContext);
      if FSaveCanvas.FClippingEnabled then
        FCanvasHelper.ScissorRect := FSavedScissorRect;
    end;
    FCurrentCanvas := FSaveCanvas;
    Dec(FGlobalBeginScene);
    if FGlobalBeginScene = 0 then
    begin
      FCanvasHelper.EndRender;
      TTextLayoutNG.EndRender;
      FFlushCountPerFrame := FCanvasHelper.FlushCountPerFrame;
      FPrimitiveCountPerFrame := FCanvasHelper.PrimitiveCountPerFrame;
    end;
  end;
  inherited;
end;

procedure TCanvasGpu.DoBlendingChanged;
begin
  inherited;
  if Assigned(FContext) then
  begin
    FCanvasHelper.Flush;
    if Blending then
      FContext.SetContextState(TContextState.csAlphaBlendOn)
    else
      FContext.SetContextState(TContextState.csAlphaBlendOff);
  end;
end;

procedure TCanvasGpu.IntersectClipRect(const ARect: TRectF);
var
  Intersection: TRect;
  TopLeft, BottomRight: TVector;
begin
  FClippingEnabled := True;
  TopLeft := PointF(ARect.Left, ARect.Top) * FMatrix;
  BottomRight := PointF(ARect.Right, ARect.Bottom) * FMatrix;
  if not FCanvasHelper.ScissorRect.IsEmpty then
    Intersection := TRect.Intersect(FCanvasHelper.ScissorRect,
      Rect(Trunc(TopLeft.X), Trunc(TopLeft.Y), Round(BottomRight.X), Round(BottomRight.Y)))
  else
    Intersection := Rect(Trunc(TopLeft.X), Trunc(TopLeft.Y), Round(BottomRight.X), Round(BottomRight.Y));
  FCanvasHelper.ScissorRect := Intersection;
end;

procedure TCanvasGpu.ExcludeClipRect(const ARect: TRectF);
begin
end;

class procedure TCanvasGpu.CreateResources;
begin
  FCanvasHelper := TCanvasHelper.Create;
  FStrokeBuilder := TStrokeBuilder.Create;
end;

procedure TCanvasGpu.Flush;
begin
  if Assigned(FCanvasHelper) then
    FCanvasHelper.Flush;
end;

class procedure TCanvasGpu.FreeResources;
begin
  FStrokeBuilder.Free;
  FCanvasHelper.Free;
end;

procedure TCanvasGpu.DoDrawLine(const APt1, APt2: TPointF; const AOpacity: Single; const ABrush: TStrokeBrush);
begin
  FStrokeBuilder.Matrix := Matrix;
  FStrokeBuilder.Brush := ABrush;

  FStrokeBuilder.BuildLine(APt1, APt2, AOpacity);

  if Length(FStrokeBuilder.Indices) > 2 then
  begin
    FCanvasHelper.FillTriangles(FStrokeBuilder.Vertices, FStrokeBuilder.Colors, FStrokeBuilder.Indices,
      Length(FStrokeBuilder.Vertices), Length(FStrokeBuilder.Indices) div 3);
    Inc(FPrimitiveCountPerFrame);
  end;
end;

procedure TCanvasGpu.DoDrawRect(const ARect: TRectF; const AOpacity: Single; const ABrush: TStrokeBrush);
begin
  FStrokeBuilder.Matrix := Matrix;
  FStrokeBuilder.Brush := ABrush;

  FStrokeBuilder.BuildLine(ARect.TopLeft, ARect.TopLeft + PointF(ARect.Width, 0.0), AOpacity);

  if Length(FStrokeBuilder.Indices) > 2 then
  begin
    FCanvasHelper.FillTriangles(FStrokeBuilder.Vertices, FStrokeBuilder.Colors, FStrokeBuilder.Indices,
      Length(FStrokeBuilder.Vertices), Length(FStrokeBuilder.Indices) div 3);
    Inc(FPrimitiveCountPerFrame);
  end;

  FStrokeBuilder.BuildLine(ARect.TopLeft + PointF(ARect.Width, 0.0), ARect.BottomRight, AOpacity);

  if Length(FStrokeBuilder.Indices) > 2 then
  begin
    FCanvasHelper.FillTriangles(FStrokeBuilder.Vertices, FStrokeBuilder.Colors, FStrokeBuilder.Indices,
      Length(FStrokeBuilder.Vertices), Length(FStrokeBuilder.Indices) div 3);
    Inc(FPrimitiveCountPerFrame);
  end;

  FStrokeBuilder.BuildLine(ARect.BottomRight, ARect.TopLeft + PointF(0.0, ARect.Height), AOpacity);

  if Length(FStrokeBuilder.Indices) > 2 then
  begin
    FCanvasHelper.FillTriangles(FStrokeBuilder.Vertices, FStrokeBuilder.Colors, FStrokeBuilder.Indices,
      Length(FStrokeBuilder.Vertices), Length(FStrokeBuilder.Indices) div 3);
    Inc(FPrimitiveCountPerFrame);
  end;

  FStrokeBuilder.BuildLine(ARect.TopLeft + PointF(0.0, ARect.Height), ARect.TopLeft, AOpacity);

  if Length(FStrokeBuilder.Indices) > 2 then
  begin
    FCanvasHelper.FillTriangles(FStrokeBuilder.Vertices, FStrokeBuilder.Colors, FStrokeBuilder.Indices,
      Length(FStrokeBuilder.Vertices), Length(FStrokeBuilder.Indices) div 3);
    Inc(FPrimitiveCountPerFrame);
  end;
end;

procedure TCanvasGpu.DoDrawEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TStrokeBrush);
var
  Center, Radius: TPointF;
begin
  FStrokeBuilder.Matrix := Matrix;
  FStrokeBuilder.Brush := ABrush;

  Center.X := (ARect.Left + ARect.Right) / 2;
  Center.Y := (ARect.Top + ARect.Bottom) / 2;

  Radius.X := ARect.Width / 2;
  Radius.Y := ARect.Height / 2;

  if (ABrush.Dash <> TStrokeDash.sdSolid) and (ABrush.Dash <> TStrokeDash.sdCustom) then
    FStrokeBuilder.BuildIntermEllipse(Center, Radius, AOpacity)
  else
    FStrokeBuilder.BuildSolidEllipse(Center, Radius, AOpacity);

  if Length(FStrokeBuilder.Indices) > 2 then
  begin
    FCanvasHelper.FillTriangles(FStrokeBuilder.Vertices, FStrokeBuilder.Colors, FStrokeBuilder.Indices,
      Length(FStrokeBuilder.Vertices), Length(FStrokeBuilder.Indices) div 3);
    Inc(FPrimitiveCountPerFrame);
  end;
end;

procedure TCanvasGpu.DoDrawPath(const APath: TPathData; const AOpacity: Single; const ABrush: TStrokeBrush);
begin
  FStrokeBuilder.Matrix := Matrix;
  FStrokeBuilder.Brush := ABrush;

  if (ABrush.Dash <> TStrokeDash.sdSolid) and (ABrush.Dash <> TStrokeDash.sdCustom) then
    FStrokeBuilder.BuildIntermPath(APath, AOpacity)
  else
    FStrokeBuilder.BuildSolidPath(APath, AOpacity);

  if Length(FStrokeBuilder.Indices) > 2 then
  begin
    FCanvasHelper.FillTriangles(FStrokeBuilder.Vertices, FStrokeBuilder.Colors, FStrokeBuilder.Indices,
      Length(FStrokeBuilder.Vertices), Length(FStrokeBuilder.Indices) div 3);
    Inc(FPrimitiveCountPerFrame);
  end;
end;

procedure TCanvasGpu.DoFillRect(const ARect: TRectF; const AOpacity: Single; const ABrush: TBrush);
var
  B: TBitmapCtx;
  Bmp: TBitmap;
  DstRect: TRectF;
  Gradient: TGradient;
begin
  if Assigned(FContext) then
  begin
    if ABrush.Kind = TBrushKind.bkGradient then
    begin
      if AOpacity < 1 then
      begin
        Gradient := TGradient.Create;
        Gradient.Assign(ABrush.Gradient);
        Gradient.ApplyOpacity(AOpacity);
        FCanvasHelper.GradientRect(TransformBounds(CornersF(ARect)), Gradient);
        Gradient.Free;
      end else
        FCanvasHelper.GradientRect(TransformBounds(CornersF(ARect)), ABrush.Gradient);
    end else if ABrush.Kind = TBrushKind.bkBitmap then
    begin
      Bmp := ABrush.Bitmap.Bitmap;
      if Assigned(Bmp.ResourceBitmap) then
        Bmp := ABrush.Bitmap.Bitmap.ResourceBitmap;
      if Bmp.HandleAllocated then
      begin
        B := TBitmapCtx(Bmp.Handle);
        case ABrush.Bitmap.WrapMode of
          TWrapMode.wmTile:
            FCanvasHelper.TexQuad(TransformPoint(PointF(ARect.Left, ARect.Top)), TransformPoint(PointF(ARect.Right, ARect.Top)),
              TransformPoint(PointF(ARect.Right, ARect.Bottom)), TransformPoint(PointF(ARect.Left, ARect.Bottom)), PointF(0, 0),
              PointF(ARect.Width / Bmp.Width, 0), PointF(ARect.Width / Bmp.Width, ARect.Height / Bmp.Height),
              PointF(0, ARect.Height / Bmp.Height), PrepareColor(FModulateColor, AOpacity), B.PaintingTexture);
          TWrapMode.wmTileOriginal:
            begin
              DstRect := RectF(0, 0, Bmp.Width, Bmp.Height);
              IntersectRect(DstRect, DstRect, ARect);
              FCanvasHelper.TexRect(TransformBounds(CornersF(DstRect)), CornersF(DstRect), B.PaintingTexture,
                PrepareColor(FModulateColor, AOpacity));
            end;
          TWrapMode.wmTileStretch:
            FCanvasHelper.TexRect(TransformBounds(CornersF(ARect)), B.PaintingTexture, PrepareColor(FModulateColor, AOpacity));
        end;
      end;
    end
    else
      FCanvasHelper.FillRect(TransformBounds(CornersF(ARect)), PrepareColor(ABrush.Color, AOpacity));
  end;
end;

procedure TCanvasGpu.TransformCallback(var Position: TPointF);
begin
  Position := TransformPoint(Position);
end;

procedure TCanvasGpu.DoFillEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TBrush);
var
  B: TBitmapCtx;
  Bmp: TBitmap;
  DstRect: TRectF;
  Gradient: TGradient;
begin
  if Assigned(FContext) then
  begin
    if ABrush.Kind = TBrushKind.bkGradient then
    begin
      if AOpacity < 1 then
      begin
        Gradient := TGradient.Create;
        Gradient.Assign(ABrush.Gradient);
        Gradient.ApplyOpacity(AOpacity);
        FCanvasHelper.GradientEllipse(ARect, Gradient, TransformCallback);
        Gradient.Free;
      end else
        FCanvasHelper.GradientEllipse(ARect, ABrush.Gradient, TransformCallback);
    end
    else if ABrush.Kind = TBrushKind.bkBitmap then
    begin
      Bmp := ABrush.Bitmap.Bitmap;
      if Assigned(Bmp.ResourceBitmap) then
        Bmp := ABrush.Bitmap.Bitmap.ResourceBitmap;
      if Bmp.HandleAllocated then
      begin
        B := TBitmapCtx(Bmp.Handle);
        case ABrush.Bitmap.WrapMode of
          TWrapMode.wmTile:
            FCanvasHelper.TexEllipse(ARect, RectF(0, 0, ARect.Width / Bmp.Width, ARect.Height / Bmp.Height),
              PrepareColor(FModulateColor, AOpacity), B.PaintingTexture, TransformCallback);
          TWrapMode.wmTileOriginal:
            begin
              DstRect := RectF(0, 0, Bmp.Width, Bmp.Height);
              IntersectRect(DstRect, DstRect, ARect);
              FCanvasHelper.TexEllipse(ARect, RectF(0, 0, ARect.Width / DstRect.Width, ARect.Height / DstRect.Height),
                PrepareColor(FModulateColor, AOpacity), B.PaintingTexture, TransformCallback);
            end;
          TWrapMode.wmTileStretch:
            FCanvasHelper.TexEllipse(ARect, PrepareColor(FModulateColor, AOpacity), B.PaintingTexture,
              TransformCallback);
        end;
      end;
    end
    else
      FCanvasHelper.FillEllipse(ARect, PrepareColor(ABrush.Color, AOpacity), TransformCallback);
  end;
end;

procedure TCanvasGpu.MeasureLines(const ALines: TLineMetricInfo; const ARect: TRectF; const AText: string;
  const WordWrap: Boolean; const Flags: TFillTextFlags; const ATextAlign: TTextAlign; const AVTextAlign: TTextAlign);
begin
end;

{ Bitmaps }

class procedure TCanvasGpu.DoInitializeBitmap(const Bitmap: TBitmap);
var
  H: TBitmapCtx;
begin
  H := TBitmapCtx.Create(Bitmap);
{$IFDEF AUTOREFCOUNT}
  H.__ObjAddRef;
{$ENDIF}
  (Bitmap as IBitmapAccess).Handle := THandle(H);
  (Bitmap as IBitmapAccess).PixelFormat := TContextManager.DefaultContextClass.PixelFormat;
end;

class procedure TCanvasGpu.DoFinalizeBitmap(const Bitmap: TBitmap);
var
  H: TBitmapCtx;
begin
  H := TBitmapCtx(Bitmap.Handle);
{$IFDEF AUTOREFCOUNT}
  H.__ObjRelease;
{$ENDIF}
  H.DisposeOf;
end;

class function TCanvasGpu.DoMapBitmap(const Bitmap: TBitmap; const Access: TMapAccess; var Data: TBitmapData): Boolean;
var
  H: TBitmapCtx;
begin
  H := TBitmapCtx(Bitmap.Handle);
  H.FAccess := Access;
  H.CreateBuffer;

  Data.Data := H.FData;
  Data.Pitch := Bitmap.BytesPerLine;
  if Assigned(H.FRenderTarget) and (Access <> TMapAccess.maWrite) then
    TCanvasGpu(Bitmap.Canvas).Context.CopyToBits(Data.Data, Data.Pitch, Rect(0, 0, Bitmap.Width, Bitmap.Height));

  if Access <> TMapAccess.maRead then
  begin
    if Assigned(H.FTexture) then
      H.FTexture.DisposeOf;
    H.FTexture := nil;
  end;
  Result := True;
end;

class procedure TCanvasGpu.DoUnmapBitmap(const Bitmap: TBitmap; var Data: TBitmapData);
var
  H: TBitmapCtx;
begin
  H := TBitmapCtx(Bitmap.Handle);
  if (H.Access <> TMapAccess.maRead) and Assigned(H.FRenderTarget) then
    H.FRenderTarget.UpdateTexture(H.FData, Bitmap.BytesPerLine);
  if Assigned(H.FRenderTarget) then
    H.FreeBuffer;
end;

procedure TCanvasGpu.DrawTexture(const DstRect, SrcRect: TRectF; const AColor: TAlphaColor; const ATexture: TTexture);
begin
  FCanvasHelper.TexRect(TransformBounds(CornersF(DstRect)), CornersF(SrcRect.Left, SrcRect.Top, SrcRect.Width,
    SrcRect.Height), ATexture, $FFFFFFFF);
end;

procedure TCanvasGpu.DoDrawBitmap(const ABitmap: TBitmap; const SrcRect, DstRect: TRectF; const AOpacity: Single;
  const HighSpeed: Boolean);
var
  B: TBitmapCtx;
  Bmp: TBitmap;
begin
  if Assigned(FContext) then
  begin
    Bmp := ABitmap;
    if Assigned(Bmp.ResourceBitmap) then
      Bmp := Bmp.ResourceBitmap;
    if Bmp.HandleAllocated then
    begin
      B := TBitmapCtx(Bmp.Handle);
      FCanvasHelper.TexRect(TransformBounds(CornersF(DstRect)), CornersF(SrcRect.Left, SrcRect.Top, SrcRect.Width,
        SrcRect.Height), B.PaintingTexture, PrepareColor(FModulateColor, AOpacity));
    end;
  end;
end;

{ Path }

function TCanvasGpu.PtInPath(const APoint: TPointF; const APath: TPathData): Boolean;
var
  Polygon: TPolygon;
  I, PointCount: Integer;
  Temp1, Temp2: TPointF;
begin
  Result := False;
  if not Assigned(APath) then
    Exit;

  APath.FlattenToPolygon(Polygon, 1);

  PointCount := Length(Polygon);
  if PointCount < 3 then
    Exit;

  Temp1 := Polygon[0];

  for I := 0 to PointCount - 1 do
  begin
    Temp2 := Polygon[(I + 1) mod PointCount];

    if APoint.Y > Min(Temp1.Y, Temp2.Y) then
      if APoint.Y <= Max(Temp1.Y, Temp2.Y) then
        if APoint.X <= Max(Temp1.X, Temp2.X) then
          if not SameValue(Temp1.Y, Temp2.Y, TEpsilon.Vector) then
          begin
            if SameValue(Temp1.X, Temp2.X, TEpsilon.Vector) or (APoint.X <= (APoint.Y - Temp1.Y) * (Temp2.X -
              Temp1.X) / (Temp2.Y - Temp1.Y) + Temp1.X) then
              Result := not Result;
          end;

    Temp1 := Temp2;
  end;
end;

procedure TCanvasGpu.DoFillPath(const APath: TPathData; const AOpacity: Single; const ABrush: TBrush);
var
  DstRect, CBounds: TRectF;
  Polygon: TPolygon;
  Vertices: TCanvasHelper.TVertexArray;
  Colors: TCanvasHelper.TAlphaColorArray;
  FillColors: TCanvasHelper.TAlphaColorArray;
  TexCoords: TCanvasHelper.TVertexArray;
  Indices: TCanvasHelper.TIndexArray;
  Point: TPointF;
  InitIndex, CurIndex, PrevIndex, PlaceAt: Integer;
  PrevMode: TDrawingMode;
  B: TBitmapCtx;
  Bmp: TBitmap;
  Gradient: TGradient;
begin
  CBounds := APath.GetBounds;
  APath.FlattenToPolygon(Polygon, 1);

  InitIndex := -1;
  PrevIndex := -1;
  CurIndex := 0;

  SetLength(Vertices, 0);
  SetLength(Colors, 0);
  SetLength(Indices, 0);
  SetLength(TexCoords, 0);

  Bmp := nil;
  if ABrush.Kind = TBrushKind.bkBitmap then
  begin
    Bmp := ABrush.Bitmap.Bitmap;
    if Assigned(Bmp.ResourceBitmap) then
      Bmp := ABrush.Bitmap.Bitmap.ResourceBitmap;
  end;
  for Point in Polygon do
  begin
    if (Point.X < $FFFF) and (Point.Y < $FFFF) then
    begin
      PlaceAt := Length(Vertices);
      SetLength(Vertices, PlaceAt + 1);
      Vertices[PlaceAt] := TransformPoint(Point);

      PlaceAt := Length(Colors);
      SetLength(Colors, PlaceAt + 1);
      Colors[PlaceAt] := PrepareColor($FFFFFFFF, AOpacity);
      SetLength(FillColors, PlaceAt + 1);
      FillColors[PlaceAt] := PrepareColor(ABrush.Color, AOpacity);

      SetLength(TexCoords, PlaceAt + 1);
      if (ABrush.Kind = TBrushKind.bkBitmap) and (ABrush.Bitmap.WrapMode = TWrapMode.wmTile) and Assigned(Bmp) and
        not Bmp.IsEmpty then
        TexCoords[PlaceAt] := TPointF.Create((Point.X - CBounds.Left) / Bmp.Width,
          (Point.Y - CBounds.Top) / Bmp.Height)
      else
        TexCoords[PlaceAt] := TPointF.Create((Point.X - CBounds.Left) / CBounds.Width,
          (Point.Y - CBounds.Top) / CBounds.Height);

      if InitIndex = -1 then
        InitIndex := CurIndex;

      if (InitIndex <> -1) and (PrevIndex <> -1) and (InitIndex <> PrevIndex) then
      begin
        PlaceAt := Length(Indices);
        SetLength(Indices, PlaceAt + 3);

        Indices[PlaceAt] := InitIndex;
        Indices[PlaceAt + 1] := PrevIndex;
        Indices[PlaceAt + 2] := CurIndex;
      end;

      PrevIndex := CurIndex;
      Inc(CurIndex);
    end
    else
    begin
      InitIndex := -1;
      PrevIndex := -1;
    end;
  end;

  PrevMode := FCanvasHelper.DrawingMode;

  if Assigned(Bitmap) then
  begin
    // Clear working stencil surface before drawing shape.
    FCanvasHelper.DrawingMode := TDrawingMode.dmClearStencil;
    FCanvasHelper.FillTriangles(Vertices, Colors, Indices, Length(Vertices), Length(Indices) div 3);
  end;

  FCanvasHelper.DrawingMode := TDrawingMode.dmWriteStencilInvert;
  FCanvasHelper.FillTriangles(Vertices, Colors, Indices, Length(Vertices), Length(Indices) div 3);

  FCanvasHelper.DrawingMode := TDrawingMode.dmReadStencil;
  if ABrush.Kind = TBrushKind.bkGradient then
  begin
    if AOpacity < 1 then
    begin
      Gradient := TGradient.Create;
      Gradient.Assign(ABrush.Gradient);
      Gradient.ApplyOpacity(AOpacity);
      FCanvasHelper.GradientTriangles(Gradient, Vertices, TexCoords, Indices, Length(Vertices), Length(Indices) div 3);
      Gradient.Free;
    end
    else
      FCanvasHelper.GradientTriangles(ABrush.Gradient, Vertices, TexCoords, Indices, Length(Vertices),
        Length(Indices) div 3)
  end
  else if (ABrush.Kind = TBrushKind.bkBitmap) and Assigned(Bmp) and Bmp.HandleAllocated then
  begin
    B := TBitmapCtx(Bmp.Handle);
    case ABrush.Bitmap.WrapMode of
      TWrapMode.wmTile:
        FCanvasHelper.TexTriangles(B.PaintingTexture, Vertices, TexCoords, Colors, Indices, Length(Vertices),
          Length(Indices) div 3);
      TWrapMode.wmTileOriginal:
        begin
          DstRect := RectF(0, 0, Bmp.Width, Bmp.Height);
          IntersectRect(DstRect, DstRect, CBounds);
          FCanvasHelper.TexRect(TransformBounds(CornersF(DstRect)), CornersF(DstRect), B.PaintingTexture,
            PrepareColor(FModulateColor, AOpacity));
        end;
      TWrapMode.wmTileStretch:
        FCanvasHelper.TexTriangles(B.PaintingTexture, Vertices, TexCoords, Colors, Indices, Length(Vertices),
          Length(Indices) div 3);
    end;
  end
  else
    FCanvasHelper.FillTriangles(Vertices, FillColors, Indices, Length(Vertices), Length(Indices) div 3);

  FCanvasHelper.DrawingMode := TDrawingMode.dmClearStencil;
  FCanvasHelper.FillTriangles(Vertices, Colors, Indices, Length(Vertices), Length(Indices) div 3);

  FCanvasHelper.DrawingMode := PrevMode;
end;

procedure RegisterCanvasClasses;
begin
  TCanvasManager.RegisterCanvas(TCanvasGpu, True, False);
  TTextLayoutManager.RegisterTextLayout(TTextLayoutNG, TCanvasGpu);
  TCanvasGpu.CreateResources;
end;

procedure UnregisterCanvasClasses;
begin
  TCanvasGpu.FreeResources;
end;

initialization
  TCustomCanvasGpu.ModulateColor := $FFFFFFFF;
  TCustomCanvasGpu.AlignToPixels := False;

end.