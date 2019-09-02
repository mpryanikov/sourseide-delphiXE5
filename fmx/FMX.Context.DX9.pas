{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit FMX.Context.DX9;

{.$DEFINE DXDEBUG}

interface

uses
  Winapi.Direct3D9, FMX.Types3D, System.Types, System.UITypes, System.SysUtils,
  System.Classes, System.Math, System.Generics.Collections;

type
  TCustomDX9Context = class(TContext3D)
  private class var
    FSharedDevice: IDirect3DDevice9;
    FDirect3D9Obj: IDirect3D9;
    FAllowSample, FAllowSample4: Boolean;
    FSharedDevicePresentParams: TD3DPresentParameters;
    FContextList: TList<TCustomDX9Context>;
    FCaps: TD3DCaps9;
    FSharedBeginSceneCount: Integer;
    FVB: IDirect3DVertexBuffer9;
    FIB16: IDirect3DIndexBuffer9;
    FIB32: IDirect3DIndexBuffer9;
    FVBLockPos, FIB16LockPos, FIB32LockPos: Integer;
    FVertexDeclaration: IDirect3DVertexDeclaration9;
    class function GetSharedDevice: IDirect3DDevice9; static;
    class function GetDirect3D9Obj: IDirect3D9; static;
    class procedure CreateDirect3D9Obj; static;
    class procedure CreateSharedDevice; static;
  public
    class procedure DestroySharedDevice; static;
    class function HardwareSupported: Boolean;
    class property SharedDevice: IDirect3DDevice9 read GetSharedDevice;
    class property Direct3D9Obj: IDirect3D9 read GetDirect3D9Obj;
  end;

procedure RegisterContextClasses;
procedure UnregisterContextClasses;

implementation

uses
  Winapi.Windows, Winapi.DXTypes, System.Win.ComObj, FMX.Messages,
  FMX.Types, FMX.Forms, FMX.PixelFormats, FMX.Platform.Win, FMX.Graphics, FMX.Canvas.GPU;

var
  HR: HResult;
  VBSize: Integer = $FFFF * 56;
  IB16Size: Integer = $FFFF * 2 * 2;
  IB32Size: Integer = $FFFF * 2 * 2;

type

{ TDX9Context }

  TDX9Context = class(TCustomDX9Context)
  private class var
    FResources: IInterfaceList;
    class procedure ResetSharedDevice;
  private
    class function AddResource(const Resource: IInterface): THandle;
    class procedure RemoveResource(Resource: THandle);
    class function ResourceToVertexShader(Resource: THandle): IDirect3DVertexShader9;
    class function ResourceToPixelShader(Resource: THandle): IDirect3DPixelShader9;
    class function ResourceToTexture(Resource: THandle): IDirect3DTexture9;
  private
    FPresentParams: TD3DPresentParameters;
    { states }
    FSavedRT, FSavedDepth: IDirect3DSurface9;
    FSaveViewport: TD3DViewport9;
    { swapchain }
    FSwapChain: IDirect3DSwapChain9;
    FSwapChainSurf: IDirect3DSurface9;
    FSwapChainDepth: IDirect3DSurface9;
    { texture or bitmap }
    FTextureSurf: IDirect3DSurface9;
    FSysMemBuf, FColorBuf2, FDepthBuf: IDirect3DSurface9;
    FColorBufTex: IDirect3DTexture9;
    { shaders }
    function GetPresentParameters(FmxHandle: TWindowHandle): TD3DPresentParameters; overload; inline;
    function GetPresentParameters(Wnd: HWnd): TD3DPresentParameters; overload;
  protected
    { assign }
    function GetValid: Boolean; override;
    { buffer }
    procedure DoCreateBuffer; override;
    procedure DoResize; override;
    procedure DoFreeBuffer; override;
    procedure DoCopyToBitmap(const Dest: TBitmap; const ARect: TRect); override;
    procedure DoCopyToBits(const Bits: Pointer; const Pitch: Integer; const ARect: TRect); override;
    { scene }
    function DoBeginScene: Boolean; override;
    procedure DoEndScene; override;
    { states }
    procedure DoClear(const ATarget: TClearTargets; const AColor: TAlphaColor; const ADepth: single; const AStencil: Cardinal); override;
    procedure DoSetContextState(AState: TContextState); override;
    procedure DoSetStencilOp(const Fail, ZFail, ZPass: TStencilOp); override;
    procedure DoSetStencilFunc(const Func: TStencilfunc; Ref, Mask: Cardinal); override;
    procedure DoSetScissorRect(const ScissorRect: TRect); override;
    { drawing }
    procedure DoDrawPrimitives(const AKind: TPrimitivesKind; const Vertices, Indices: Pointer; const VertexDeclaration: TVertexDeclaration;
      const VertexSize, VertexCount, IndexSize, IndexCount: Integer); override;
    { texture }
    class procedure DoInitializeTexture(const Texture: TTexture); override;
    class procedure DoFinalizeTexture(const Texture: TTexture); override;
    class procedure DoUpdateTexture(const Texture: TTexture; const Bits: Pointer; const Pitch: Integer); override;
    { bitmap }
    class function DoBitmapToTexture(const Bitmap: TBitmap): TTexture; override;
    { shaders }
    class procedure DoInitializeShader(const Shader: TContextShader); override;
    class procedure DoFinalizeShader(const Shader: TContextShader); override;
    procedure DoSetShaders(const VertexShader, PixelShader: TContextShader); override;
    procedure DoSetShaderVariable(const Name: string; const Data: array of TVector3D); override;
    procedure DoSetShaderVariable(const Name: string; const Texture: TTexture); override;
    procedure DoSetShaderVariable(const Name: string; const Matrix: TMatrix3D); override;
    { constructors }
    constructor CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer;
      const AMultisample: TMultisample; const ADepthStencil: Boolean); override;
    constructor CreateFromTexture(const ATexture: TTexture; const AMultisample: TMultisample;
      const ADepthStencil: Boolean); override;
  public
    destructor Destroy; override;
  end;

function ColorToD3DColor(const AColor: TAlphaColor): TD3DColorValue;
begin
  Result.r := TAlphaColorRec(AColor).r / $FF;
  Result.g := TAlphaColorRec(AColor).g / $FF;
  Result.b := TAlphaColorRec(AColor).b / $FF;
  Result.a := TAlphaColorRec(AColor).a / $FF;
end;

function D3DMatrix(M: TMatrix3D): TD3DMatrix;
begin
  Result := TD3DMatrix(M);
end;

function TexturePixelFormatToDX(PF: TPixelFormat): TD3DFormat;
begin
  case PF of
    pfA16B16G16R16:
      Result := D3DFMT_A16B16G16R16;
    pfA2R10G10B10:
      Result := D3DFMT_A2R10G10B10;
    pfA2B10G10R10:
      Result := D3DFMT_A2B10G10R10;
    pfA8R8G8B8:
      Result := D3DFMT_A8R8G8B8;
    pfX8R8G8B8:
      Result := D3DFMT_X8R8G8B8;
    pfA8B8G8R8:
      Result := D3DFMT_A8B8G8R8;
    pfX8B8G8R8:
      Result := D3DFMT_X8B8G8R8;
    pfR5G6B5:
      Result := D3DFMT_R5G6B5;
    pfA4R4G4B4:
      Result := D3DFMT_A4R4G4B4;
    pfA1R5G5B5:
      Result := D3DFMT_A1R5G5B5;
    pfX1R5G5B5:
      Result := D3DFMT_X1R5G5B5;
    pfG16R16:
      Result := D3DFMT_G16R16;
    pfA8L8:
      Result := D3DFMT_A8L8;
    pfA4L4:
      Result := D3DFMT_A4L4;
    pfL16:
      Result := D3DFMT_L16;
    pfL8:
      Result := D3DFMT_L8;
    pfR16F:
      Result := D3DFMT_R16F;
    pfG16R16F:
      Result := D3DFMT_G16R16F;
    pfA16B16G16R16F:
      Result := D3DFMT_A16B16G16R16F;
    pfR32F:
      Result := D3DFMT_R32F;
    pfG32R32F:
      Result := D3DFMT_G32R32F;
    pfA32B32G32R32F:
      Result := D3DFMT_A32B32G32R32F;
    pfA8:
      Result := D3DFMT_A8;
    pfV8U8:
      Result := D3DFMT_V8U8;
    pfL6V5U5:
      Result := D3DFMT_L6V5U5;
    pfX8L8V8U8:
      Result := D3DFMT_X8L8V8U8;
    pfQ8W8V8U8:
      Result := D3DFMT_Q8W8V8U8;
    pfV16U16:
      Result := D3DFMT_V16U16;
    pfA2W10V10U10:
      Result := D3DFMT_A2W10V10U10;
    pfR8G8_B8G8:
      Result := D3DFMT_R8G8_B8G8;
    pfG8R8_G8B8:
      Result := D3DFMT_G8R8_G8B8;
    pfQ16W16V16U16:
      Result := D3DFMT_Q16W16V16U16;
    pfCxV8U8:
      Result := D3DFMT_CxV8U8;
    pfDXT1:
      Result := D3DFMT_DXT1;
    pfDXT2:
      Result := D3DFMT_DXT2;
    pfDXT3:
      Result := D3DFMT_DXT3;
    pfDXT4:
      Result := D3DFMT_DXT4;
    pfDXT5:
      Result := D3DFMT_DXT5;

  else
    Result := D3DFMT_UNKNOWN;
  end;
end;

{$R-}

{ TCustomDX9Context }

class procedure TCustomDX9Context.CreateDirect3D9Obj;
begin
  if not Assigned(FDirect3D9Obj) then
    FDirect3D9Obj := IDirect3D9(_Direct3DCreate9(D3D_SDK_VERSION));
end;

class procedure TCustomDX9Context.CreateSharedDevice;
var
  VP: Cardinal;
  DM: TD3DDisplayMode;
begin
  if not Assigned(FSharedDevice) then
  begin
    if Failed(Direct3D9Obj.GetAdapterDisplayMode(D3DADAPTER_DEFAULT, DM)) then
      Exit;

    FillChar(FSharedDevicePresentParams, SizeOf(FSharedDevicePresentParams), 0);
    FSharedDevicePresentParams.hDeviceWindow := GetDesktopWindow;
    FSharedDevicePresentParams.Windowed := True;
    FSharedDevicePresentParams.BackBufferWidth := 2;
    FSharedDevicePresentParams.BackBufferHeight := 2;
    FSharedDevicePresentParams.BackBufferFormat := DM.Format;
    FSharedDevicePresentParams.BackBufferCount := 1;
    FSharedDevicePresentParams.EnableAutoDepthStencil := False;
    FSharedDevicePresentParams.SwapEffect := D3DSWAPEFFECT_DISCARD;
    FSharedDevicePresentParams.PresentationInterval := D3DPRESENT_INTERVAL_IMMEDIATE;

    Direct3D9Obj.GetDeviceCaps(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, FCaps);
    if (FCaps.DevCaps and D3DDEVCAPS_HWTRANSFORMANDLIGHT = D3DDEVCAPS_HWTRANSFORMANDLIGHT) then
      VP := D3DCREATE_HARDWARE_VERTEXPROCESSING
    else
      VP := D3DCREATE_SOFTWARE_VERTEXPROCESSING;
    VP := VP or D3DCREATE_MULTITHREADED or D3DCREATE_FPU_PRESERVE;

    FAllowSample := Succeeded(Direct3D9Obj.CheckDeviceMultiSampleType(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, FSharedDevicePresentParams.BackBufferFormat, True, D3DMULTISAMPLE_2_SAMPLES, nil));
    FAllowSample4 := Succeeded(Direct3D9Obj.CheckDeviceMultiSampleType(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, FSharedDevicePresentParams.BackBufferFormat, True, D3DMULTISAMPLE_4_SAMPLES, nil));

    FDirect3D9Obj.CreateDevice(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, 0, VP, @FSharedDevicePresentParams, FSharedDevice);
    if Assigned(FSharedDevice) then
    begin
      FSharedDevice.CreateVertexBuffer(VBSize, D3DUSAGE_WRITEONLY or D3DUSAGE_DYNAMIC, D3DFVF_XYZ, D3DPOOL_DEFAULT, FVB, nil);
      FSharedDevice.CreateIndexBuffer(IB16Size, D3DUSAGE_WRITEONLY or D3DUSAGE_DYNAMIC, D3DFMT_INDEX16, D3DPOOL_DEFAULT, FIB16, nil);
      FSharedDevice.CreateIndexBuffer(IB32Size, D3DUSAGE_WRITEONLY or D3DUSAGE_DYNAMIC, D3DFMT_INDEX32, D3DPOOL_DEFAULT, FIB32, nil);
      FVBLockPos := 0;
      FIB16LockPos := 0;
      FIB32LockPos := 0;
    end;
    FContextList := TList<TCustomDX9Context>.Create;

    TCustomDX9Context.FPixelFormat := TPixelFormat.pfA8R8G8B8;
    TCustomDX9Context.FTextureUnitCount := Min(FCaps.MaxSimultaneousTextures, FCaps.MaxTextureBlendStages);
    TCustomDX9Context.FMaxTextureSize := FCaps.MaxTextureWidth;
    TCustomDX9Context.FPixelToPixelPolygonOffset := PointF(-0.5, -0.5);
  end;
end;

class function TCustomDX9Context.GetDirect3D9Obj: IDirect3D9;
begin
  CreateDirect3D9Obj;
  Result := FDirect3D9Obj;
end;

class function TCustomDX9Context.GetSharedDevice: IDirect3DDevice9;
begin
  CreateSharedDevice;
  Result := FSharedDevice;
end;

class function TCustomDX9Context.HardwareSupported: Boolean;
var
  Direct3D9Obj: IDirect3D9;
  DM: TD3DDisplayMode;
begin
  Result := False;
  Direct3D9Obj := Direct3DCreate9(D3D_SDK_VERSION);
  if Assigned(Direct3D9Obj) then
  begin
    if Failed(Direct3D9Obj.GetAdapterDisplayMode(D3DADAPTER_DEFAULT, DM)) then Exit;
    if Succeeded(Direct3D9Obj.CheckDeviceType(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, DM.Format, DM.Format, True)) then
      Result := True;
  end;
end;

class procedure TCustomDX9Context.DestroySharedDevice;
begin
  if FSharedDevice <> nil then
  begin
    FreeAndNil(FContextList);
  end;
end;

{ TDX9Context }

class procedure TDX9Context.ResetSharedDevice;
var
  I: Integer;
begin
  FVB := nil;
  FIB16 := nil;
  FIB32 := nil;

  TMessageManager.DefaultManager.SendMessage(nil, TContextLostMessage.Create, True);

  for I := 0 to FContextList.Count - 1 do
    FContextList[I].FreeBuffer;

  HR := SharedDevice.Reset(FSharedDevicePresentParams);

  TMessageManager.DefaultManager.SendMessage(nil, TContextResetMessage.Create, True);

  for I := 0 to FContextList.Count - 1 do
    FContextList[I].CreateBuffer;

  FSharedDevice.CreateVertexBuffer(VBSize, D3DUSAGE_WRITEONLY or D3DUSAGE_DYNAMIC, D3DFVF_XYZ, D3DPOOL_DEFAULT, FVB, nil);
  FSharedDevice.CreateIndexBuffer(IB16Size, D3DUSAGE_WRITEONLY or D3DUSAGE_DYNAMIC, D3DFMT_INDEX16, D3DPOOL_DEFAULT, FIB16, nil);
  FSharedDevice.CreateIndexBuffer(IB32Size, D3DUSAGE_WRITEONLY or D3DUSAGE_DYNAMIC, D3DFMT_INDEX32, D3DPOOL_DEFAULT, FIB32, nil);
  FVBLockPos := 0;
  FIB16LockPos := 0;
  FIB32LockPos := 0;
end;

constructor TDX9Context.CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer;
  const AMultisample: TMultisample; const ADepthStencil: Boolean);
begin
  inherited;
  CreateSharedDevice;
  if Assigned(FSharedDevice) then
  begin
    FContextList.Add(Self);
    FPresentParams := GetPresentParameters(WindowHandleToPlatform(AParent).Wnd);
    CreateBuffer;
  end;
end;

constructor TDX9Context.CreateFromTexture(const ATexture: TTexture; const AMultisample: TMultisample; const ADepthStencil: Boolean);
begin
  inherited;
  CreateSharedDevice;
  if Assigned(FSharedDevice) then
  begin
    FContextList.Add(Self);
    FPresentParams := GetPresentParameters(GetDesktopWindow);
    CreateBuffer;
  end;
end;

destructor TDX9Context.Destroy;
begin
  if Assigned(FSharedDevice) then
    FContextList.Remove(Self);
  inherited;
end;

function TDX9Context.GetPresentParameters(FmxHandle: TWindowHandle): TD3DPresentParameters;
begin
  Result := GetPresentParameters(WindowHandleToPlatform(FmxHandle).Wnd);
end;

function TDX9Context.GetPresentParameters(Wnd: HWnd): TD3DPresentParameters;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.hDeviceWindow := Wnd;
  Result.Windowed := True;
  Result.BackBufferWidth := Width;
  Result.BackBufferHeight := Height;
  Result.BackBufferFormat := D3DFMT_A8R8G8B8;
  if Width = 0 then
    Result.BackBufferWidth := 2;
  if Height = 0 then
    Result.BackBufferHeight := 2;

  Result.BackBufferCount := 1;

  Result.EnableAutoDepthStencil := DepthStencil;
  Result.AutoDepthStencilFormat := D3DFMT_D24S8; // D3DFMT_D16;

  Result.SwapEffect := D3DSWAPEFFECT_DISCARD;

  Result.PresentationInterval := D3DPRESENT_INTERVAL_IMMEDIATE;

  if (Multisample = TMultisample.ms4Samples) and FAllowSample4 then
    Result.MultiSampleType := D3DMULTISAMPLE_4_SAMPLES
  else if (Multisample in [TMultisample.ms2Samples, TMultisample.ms4Samples]) and FAllowSample then
    Result.MultiSampleType := D3DMULTISAMPLE_2_SAMPLES;
end;

function TDX9Context.GetValid: Boolean;
begin
  Result := SharedDevice <> nil;
end;

procedure TDX9Context.DoCreateBuffer;
var
  Tex: IDirect3DTexture9;
begin
  if Valid then
  begin
    { Create RenderTarget }
    if (Texture <> nil) then
    begin
      Tex := ResourceToTexture(Texture.Handle);
      if Assigned(Tex) then
      begin
        Tex.GetSurfaceLevel(0, FTextureSurf);
        if DepthStencil then
          HR := SharedDevice.CreateDepthStencilSurface(Texture.Width, Texture.Height, D3DFMT_D24S8, FPresentParams.MultiSampleType, 0, True, FDepthBuf, nil);
      end;
    end
    else 
    begin
      FPresentParams := GetPresentParameters(WindowHandleToPlatform(Parent).Wnd);
      HR := SharedDevice.CreateAdditionalSwapChain(FPresentParams, FSwapChain);
      if Succeeded(HR) then
      begin
        HR := FSwapChain.GetBackBuffer(0, D3DBACKBUFFER_TYPE_MONO, FSwapChainSurf);
        if DepthStencil then
          HR := SharedDevice.CreateDepthStencilSurface(Width, Height, D3DFMT_D24S8, FPresentParams.MultiSampleType, 0, false, FSwapChainDepth, nil);
      end;
    end;
  end;
end;

procedure TDX9Context.DoResize;
begin
end;

procedure TDX9Context.DoFreeBuffer;
begin
  if Valid then
  begin
    FSavedRT := nil;
    FSavedDepth := nil;
    FTextureSurf := nil;
    FSwapChainSurf := nil;
    FSwapChainDepth := nil;
    FSwapChain := nil;
    FColorBufTex := nil;
    FSysMemBuf := nil;
    FColorBuf2 := nil;
    FDepthBuf := nil;
  end;
end;

procedure TDX9Context.DoClear(const ATarget: TClearTargets; const AColor: TAlphaColor; const ADepth: single; const AStencil: Cardinal);
var
  Flags: Integer;
begin
  if Valid then
  begin
    Flags := 0;
    if DepthStencil and (TClearTarget.ctDepth in ATarget) then
      Flags := Flags or D3DCLEAR_ZBUFFER;
    if DepthStencil and (TClearTarget.ctStencil in ATarget) then
      Flags := Flags or D3DCLEAR_STENCIL;
    if (TClearTarget.ctColor in ATarget) then
      Flags := Flags or D3DCLEAR_TARGET;
    SharedDevice.Clear(0, nil, Flags, AColor, ADepth, AStencil)
  end;
end;

procedure TDX9Context.DoCopyToBitmap(const Dest: TBitmap; const ARect: TRect);
begin
  if Valid then
  begin
    if (TCanvasStyle.NeedGPUSurface in Dest.CanvasClass.GetCanvasStyle) and Assigned(Texture) then
    begin
      if TCustomCanvasGpu(Dest.Canvas).BeginScene then
      try
        TCustomCanvasGpu(Dest.Canvas).Clear(0);
        TCustomCanvasGpu(Dest.Canvas).SetMatrix(TMatrix.Identity);
        TCustomCanvasGpu(Dest.Canvas).DrawTexture(RectF(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom),
          RectF(0, 0, ARect.Width, ARect.Height), $FFFFFFFF, Texture);
      finally
        TCustomCanvasGpu(Dest.Canvas).EndScene;
      end;
    end else begin
      inherited DoCopyToBitmap(Dest, ARect);
    end;
  end;
end;

procedure TDX9Context.DoCopyToBits(const Bits: Pointer; const Pitch: Integer; const ARect: TRect);
var
  RT: IDirect3DSurface9;
  Surface: TD3DLockedRect;
  M: TBitmapData;
  I: Integer;
begin
  ARect.Intersect(ARect, Rect(0, 0, Width, Height));
  if FSysMemBuf = nil then
  begin
    if FAILED(SharedDevice.CreateOffscreenPlainSurface(Width, Height, D3DFMT_A8R8G8B8, D3DPOOL_SYSTEMMEM, FSysMemBuf, nil)) then Exit;
    if (FPresentParams.MultiSampleType <> D3DMULTISAMPLE_NONE) then
      SharedDevice.CreateRenderTarget(Width, Height, D3DFMT_A8R8G8B8, D3DMULTISAMPLE_NONE, 0, False, FColorBuf2, nil);
  end;
  if Texture <> nil then
    RT := FTextureSurf
  else
    RT := FSwapChainSurf;
  if FColorBuf2 <> nil then
  begin
    SharedDevice.StretchRect(RT, nil, FColorBuf2, nil, D3DTEXF_LINEAR);
    if (RT <> nil) and not FAILED(SharedDevice.GetRenderTargetData(FColorBuf2, FSysMemBuf)) then
    begin
      if not FAILED(FSysMemBuf.LockRect(Surface, nil, 0)) then
      begin
        for I := ARect.Top to ARect.Bottom - 1 do
          Move(PAlphaColorArray(Surface.pBits)[ARect.Left + (I * (Surface.Pitch div 4))], PAlphaColorArray(Bits)[I * (Pitch div 4) + ARect.Left], ARect.Width * 4);
        FSysMemBuf.UnlockRect;
      end
    end;
  end
  else
  begin
    if (RT <> nil) and not FAILED(SharedDevice.GetRenderTargetData(RT, FSysMemBuf)) then
    begin
      if not FAILED(FSysMemBuf.LockRect(Surface, nil, 0)) then
      begin
        if (ARect.Left = 0) and (ARect.Top = 0) and (ARect.Width = Width) and (ARect.Height = Height) and
          (Surface.Pitch = M.Pitch) and (M.Pitch = Width * 4)
        then
          Move(Surface.pBits^, Bits^, Pitch * Height)
        else
        begin
          for I := ARect.Top to ARect.Bottom - 1 do
            Move(PAlphaColorArray(Surface.pBits)[ARect.Left + (I * (Surface.Pitch div 4))], PAlphaColorArray(Bits)[I * (Pitch div 4) + ARect.Left], ARect.Width * 4);
        end;
        FSysMemBuf.UnlockRect;
      end
    end;
  end;
end;

function TDX9Context.DoBeginScene: Boolean;
var
  Viewport: TD3DViewport9;
begin
  Result := False;
  if Valid then
  begin
    { Check device }
    HR := SharedDevice.TestCooperativeLevel;
    if FAILED(HR) then
    begin
      if (D3DERR_DEVICELOST = HR) then
        Exit;
      if (D3DERR_DEVICENOTRESET = HR) then
        ResetSharedDevice;
    end;
    { Render }
    SharedDevice.GetRenderTarget(0, FSavedRT);
    SharedDevice.GetDepthStencilSurface(FSavedDepth);
    SharedDevice.GetViewport(FSaveViewport);
    if Texture <> nil then
    begin
      HR := SharedDevice.SetRenderTarget(0, FTextureSurf);
      HR := SharedDevice.SetDepthStencilSurface(FDepthBuf);
    end
    else
    begin
      HR := SharedDevice.SetRenderTarget(0, FSwapChainSurf);
      HR := SharedDevice.SetDepthStencilSurface(FSwapChainDepth);
    end;
    Viewport.X := 0;
    Viewport.Y := 0;
    Viewport.Width := Width;
    Viewport.Height := Height;
    Viewport.MinZ := 0;
    Viewport.MaxZ := 1;
    HR := SharedDevice.SetViewport(Viewport);
    if FSharedBeginSceneCount = 0 then
    begin
      HR := SharedDevice.BeginScene;
      if FAILED(HR) then Exit;
    end;
    FSharedBeginSceneCount := FSharedBeginSceneCount + 1;

    try
      Result := inherited DoBeginScene;
      if not Result then
      begin
        SharedDevice.EndScene;
        Exit;
      end;
    except
      SharedDevice.EndScene;
      raise;
    end;
    if FSharedBeginSceneCount = 1 then
    begin
      SharedDevice.SetRenderState(D3DRS_SPECULARENABLE, iFALSE);

      SharedDevice.SetRenderState(D3DRS_SRCBLEND, D3DBLEND_ONE);
      SharedDevice.SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA);

      SharedDevice.SetRenderState(D3DRS_CLIPPING, iTRUE);
      SharedDevice.SetRenderState(D3DRS_CLIPPLANEENABLE, iFALSE);

      SharedDevice.SetRenderState(D3DRS_NORMALIZENORMALS, iTRUE);
      // disable default T&L
      SharedDevice.SetRenderState(D3DRS_FOGENABLE, iFALSE);
      SharedDevice.SetRenderState(D3DRS_LIGHTING, iFALSE);
      SharedDevice.SetRenderState(D3DRS_COLORVERTEX, iFALSE);
    end;
  end;
end;

procedure TDX9Context.DoEndScene;
begin
  if Valid then
  begin
    FSharedBeginSceneCount := FSharedBeginSceneCount - 1;
    if FSharedBeginSceneCount = 0 then
    begin
      if not FAILED(SharedDevice.TestCooperativeLevel) then
      begin
        HR := SharedDevice.EndScene;
        if Assigned(FSwapChain) then
          HR := FSwapChain.Present(nil, nil, 0, nil, 0);
      end;
    end;
    HR := SharedDevice.SetRenderTarget(0, FSavedRT);
    HR := SharedDevice.SetDepthStencilSurface(FSavedDepth);
    HR := SharedDevice.SetViewport(FSaveViewport);
  end;
  inherited ;
end;

class function TDX9Context.DoBitmapToTexture(const Bitmap: TBitmap): TTexture;
begin
  if Bitmap.CanvasClass.InheritsFrom(TCustomCanvasGpu) then
    Result := TBitmapCtx(Bitmap.Handle).PaintingTexture
  else
    Result := inherited DoBitmapToTexture(Bitmap);
end;

procedure TDX9Context.DoSetScissorRect(const ScissorRect: TRect);
begin
  if Assigned(SharedDevice) then
    SharedDevice.SetScissorRect(@ScissorRect);
end;

procedure TDX9Context.DoSetContextState(AState: TContextState);
begin
  if Assigned(SharedDevice) then
  begin
    case AState of
      TContextState.csZTestOn:
        begin
          SharedDevice.SetRenderState(D3DRS_ZENABLE, iTRUE);
          SharedDevice.SetRenderState(D3DRS_ZFUNC, D3DCMP_LESSEQUAL);
        end;
      TContextState.csZTestOff:
        SharedDevice.SetRenderState(D3DRS_ZENABLE, iFALSE);
      TContextState.csZWriteOn:
        SharedDevice.SetRenderState(D3DRS_ZWRITEENABLE, iTRUE);
      TContextState.csZWriteOff:
        SharedDevice.SetRenderState(D3DRS_ZWRITEENABLE, iFALSE);
      TContextState.csAlphaBlendOn:
        begin
          SharedDevice.SetRenderState(D3DRS_ALPHATESTENABLE, iTRUE);
          SharedDevice.SetRenderState(D3DRS_ALPHABLENDENABLE, iTRUE);
        end;
      TContextState.csAlphaBlendOff:
        begin
          SharedDevice.SetRenderState(D3DRS_ALPHATESTENABLE, iFALSE);
          SharedDevice.SetRenderState(D3DRS_ALPHABLENDENABLE, iFALSE);
        end;
      TContextState.csStencilOn:
        SharedDevice.SetRenderState(D3DRS_STENCILENABLE, iTRUE);
      TContextState.csStencilOff:
        SharedDevice.SetRenderState(D3DRS_STENCILENABLE, iFALSE);
      TContextState.csColorWriteOn:
        SharedDevice.SetRenderState(D3DRS_COLORWRITEENABLE, $FFFFFFFF);
      TContextState.csColorWriteOff:
        SharedDevice.SetRenderState(D3DRS_COLORWRITEENABLE, 0);
      TContextState.csScissorOn:
        SharedDevice.SetRenderState(D3DRS_SCISSORTESTENABLE, iTRUE);
      TContextState.csScissorOff:
        SharedDevice.SetRenderState(D3DRS_SCISSORTESTENABLE, iFALSE);
      TContextState.csFrontFace:
        SharedDevice.SetRenderState(D3DRS_CULLMODE, D3DCULL_CCW);
      TContextState.csBackFace:
        SharedDevice.SetRenderState(D3DRS_CULLMODE, D3DCULL_CW);
      TContextState.csAllFace:
        SharedDevice.SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);
    end;
  end;
end;

procedure TDX9Context.DoSetStencilOp(const Fail, ZFail, ZPass: TStencilOp);
begin
  case Fail of
    TStencilOp.soKeep: SharedDevice.SetRenderState(D3DRS_STENCILFAIL, D3DSTENCILOP_KEEP);
    TStencilOp.soZero: SharedDevice.SetRenderState(D3DRS_STENCILFAIL, D3DSTENCILOP_ZERO);
    TStencilOp.soReplace: SharedDevice.SetRenderState(D3DRS_STENCILFAIL, D3DSTENCILOP_REPLACE);
    TStencilOp.soIncrease: SharedDevice.SetRenderState(D3DRS_STENCILFAIL, D3DSTENCILOP_INCRSAT);
    TStencilOp.soDecrease: SharedDevice.SetRenderState(D3DRS_STENCILFAIL, D3DSTENCILOP_DECRSAT);
    TStencilOp.soInvert: SharedDevice.SetRenderState(D3DRS_STENCILFAIL, D3DSTENCILOP_INVERT);
  end;
  case ZFail of
    TStencilOp.soKeep: SharedDevice.SetRenderState(D3DRS_STENCILZFAIL, D3DSTENCILOP_KEEP);
    TStencilOp.soZero: SharedDevice.SetRenderState(D3DRS_STENCILZFAIL, D3DSTENCILOP_ZERO);
    TStencilOp.soReplace: SharedDevice.SetRenderState(D3DRS_STENCILZFAIL, D3DSTENCILOP_REPLACE);
    TStencilOp.soIncrease: SharedDevice.SetRenderState(D3DRS_STENCILZFAIL, D3DSTENCILOP_INCRSAT);
    TStencilOp.soDecrease: SharedDevice.SetRenderState(D3DRS_STENCILZFAIL, D3DSTENCILOP_DECRSAT);
    TStencilOp.soInvert: SharedDevice.SetRenderState(D3DRS_STENCILZFAIL, D3DSTENCILOP_INVERT);
  end;
  case ZPass of
    TStencilOp.soKeep: SharedDevice.SetRenderState(D3DRS_STENCILPASS, D3DSTENCILOP_KEEP);
    TStencilOp.soZero: SharedDevice.SetRenderState(D3DRS_STENCILPASS, D3DSTENCILOP_ZERO);
    TStencilOp.soReplace: SharedDevice.SetRenderState(D3DRS_STENCILPASS, D3DSTENCILOP_REPLACE);
    TStencilOp.soIncrease: SharedDevice.SetRenderState(D3DRS_STENCILPASS, D3DSTENCILOP_INCRSAT);
    TStencilOp.soDecrease: SharedDevice.SetRenderState(D3DRS_STENCILPASS, D3DSTENCILOP_DECRSAT);
    TStencilOp.soInvert: SharedDevice.SetRenderState(D3DRS_STENCILPASS, D3DSTENCILOP_INVERT);
  end;
end;

procedure TDX9Context.DoSetStencilFunc(const Func: TStencilfunc; Ref, Mask: Cardinal);
begin
  case Func of
    TStencilFunc.sfNever: SharedDevice.SetRenderState(D3DRS_STENCILFUNC, D3DCMP_NEVER);
    TStencilFunc.sfLess: SharedDevice.SetRenderState(D3DRS_STENCILFUNC, D3DCMP_LESS);
    TStencilFunc.sfLequal: SharedDevice.SetRenderState(D3DRS_STENCILFUNC, D3DCMP_LESSEQUAL);
    TStencilFunc.sfGreater: SharedDevice.SetRenderState(D3DRS_STENCILFUNC, D3DCMP_GREATER);
    TStencilFunc.sfGequal: SharedDevice.SetRenderState(D3DRS_STENCILFUNC, D3DCMP_GREATEREQUAL);
    TStencilFunc.sfEqual: SharedDevice.SetRenderState(D3DRS_STENCILFUNC, D3DCMP_EQUAL);
    TStencilFunc.sfNotEqual: SharedDevice.SetRenderState(D3DRS_STENCILFUNC, D3DCMP_NOTEQUAL);
    TStencilFunc.sfAlways: SharedDevice.SetRenderState(D3DRS_STENCILFUNC, D3DCMP_ALWAYS);
  end;
  SharedDevice.SetRenderState(D3DRS_STENCILREF, Ref);
  SharedDevice.SetRenderState(D3DRS_STENCILMASK, Mask);
end;

procedure TDX9Context.DoDrawPrimitives(const AKind: TPrimitivesKind; const Vertices, Indices: Pointer;
  const VertexDeclaration: TVertexDeclaration; const VertexSize, VertexCount, IndexSize, IndexCount: Integer);
var
  Ver: Pointer;
  Idx: ^Word;
  Flags: Cardinal;
  VertexDecl: array of TD3DVertexElement9;
  OldVertexDeclaration: IDirect3DVertexDeclaration9;
  Element: TVertexElement;
begin
  if Valid and Assigned(FVB) and (Assigned(FIB16) or Assigned(FIB32)) then
  begin
    if VertexCount * VertexSize > VBSize then
    begin
      FVB := nil;
      HR := SharedDevice.CreateVertexBuffer(VertexCount * VertexSize, D3DUSAGE_WRITEONLY or D3DUSAGE_DYNAMIC, D3DFVF_XYZ, D3DPOOL_DEFAULT, FVB, nil);
      if FAILED(HR) then
        SharedDevice.CreateVertexBuffer(VBSize, D3DUSAGE_WRITEONLY or D3DUSAGE_DYNAMIC, D3DFVF_XYZ, D3DPOOL_DEFAULT, FVB, nil)
      else
        VBSize := VertexCount * VertexSize;
    end;
    if (IndexSize = 2) and (IndexCount * IndexSize > IB16Size) then
    begin
      FIB16 := nil;
      HR := SharedDevice.CreateIndexBuffer(IndexCount * 2, D3DUSAGE_WRITEONLY or D3DUSAGE_DYNAMIC, D3DFMT_INDEX16, D3DPOOL_DEFAULT, FIB16, nil);
      if FAILED(HR) then
        SharedDevice.CreateIndexBuffer(IB16Size, D3DUSAGE_WRITEONLY or D3DUSAGE_DYNAMIC, D3DFMT_INDEX16, D3DPOOL_DEFAULT, FIB16, nil)
      else
        IB16Size := IndexCount * 2;
    end;
    if (IndexSize = 4) and (IndexCount * IndexSize > IB32Size) then
    begin
      FIB32 := nil;
      HR := SharedDevice.CreateIndexBuffer(IndexCount * 4, D3DUSAGE_WRITEONLY or D3DUSAGE_DYNAMIC, D3DFMT_INDEX32, D3DPOOL_DEFAULT, FIB32, nil);
      if FAILED(HR) then
        SharedDevice.CreateIndexBuffer(IB32Size, D3DUSAGE_WRITEONLY or D3DUSAGE_DYNAMIC, D3DFMT_INDEX32, D3DPOOL_DEFAULT, FIB32, nil)
      else
        IB32Size := IndexCount * 4;
    end;

    if FVBLockPos + VertexCount * VertexSize > VBSize then
    begin
      FVBLockPos := 0;
      Flags := D3DLOCK_DISCARD;
    end
    else
      Flags := D3DLOCK_NOOVERWRITE;

    if Succeeded(FVB.Lock(FVBLockPos, VertexCount * VertexSize, Pointer(Ver), Flags)) then
    begin
      try
        Move(Vertices^, Ver^, VertexCount * VertexSize);
        { indexs }
        if IndexSize = 4 then
        begin
          if FIB32LockPos + IndexCount * IndexSize > IB32Size then
          begin
            FIB32LockPos := 0;
            Flags := D3DLOCK_DISCARD;
          end else
            Flags := D3DLOCK_NOOVERWRITE;
          if Succeeded(FIB32.Lock(FIB32LockPos, IndexCount * IndexSize, Pointer(Idx), Flags)) then
          try
            Move(Indices^, Idx^, IndexCount * IndexSize);
          finally
            FIB32.Unlock;
          end;
        end else begin
          if FIB16LockPos + IndexCount * IndexSize > IB16Size then
          begin
            FIB16LockPos := 0;
            Flags := D3DLOCK_DISCARD;
          end else
            Flags := D3DLOCK_NOOVERWRITE;
          if Succeeded(FIB16.Lock(FIB16LockPos, IndexCount * IndexSize, Pointer(Idx), Flags)) then
          try
            Move(Indices^, Idx^, IndexCount * IndexSize);
          finally
            FIB16.Unlock;
          end;
        end;
      finally
        { unlock }
        FVB.Unlock;
      end;

      SetLength(VertexDecl, 0);
      for Element in VertexDeclaration do
      begin
        case Element.Format of
          TVertexFormat.vfVertex:
            begin
              SetLength(VertexDecl, Length(VertexDecl) + 1);
              VertexDecl[High(VertexDecl)].Stream := 0;
              VertexDecl[High(VertexDecl)].Offset := Element.Offset;
              VertexDecl[High(VertexDecl)]._Type := D3DDECLTYPE_FLOAT3;
              VertexDecl[High(VertexDecl)].Method := D3DDECLMETHOD_DEFAULT;
              VertexDecl[High(VertexDecl)].Usage := D3DDECLUSAGE_POSITION;
              VertexDecl[High(VertexDecl)].UsageIndex := 0;
            end;
          TVertexFormat.vfNormal:
            begin
              SetLength(VertexDecl, Length(VertexDecl) + 1);
              VertexDecl[High(VertexDecl)].Stream := 0;
              VertexDecl[High(VertexDecl)].Offset := Element.Offset;
              VertexDecl[High(VertexDecl)]._Type := D3DDECLTYPE_FLOAT3;
              VertexDecl[High(VertexDecl)].Method := D3DDECLMETHOD_DEFAULT;
              VertexDecl[High(VertexDecl)].Usage := D3DDECLUSAGE_NORMAL;
              VertexDecl[High(VertexDecl)].UsageIndex := 0;
            end;
          TVertexFormat.vfColor0:
            begin
              SetLength(VertexDecl, Length(VertexDecl) + 1);
              VertexDecl[High(VertexDecl)].Stream := 0;
              VertexDecl[High(VertexDecl)].Offset := Element.Offset;
              VertexDecl[High(VertexDecl)]._Type := D3DDECLTYPE_D3DCOLOR;
              VertexDecl[High(VertexDecl)].Method := D3DDECLMETHOD_DEFAULT;
              VertexDecl[High(VertexDecl)].Usage := D3DDECLUSAGE_COLOR;
              VertexDecl[High(VertexDecl)].UsageIndex := 0;
            end;
          TVertexFormat.vfColor1:
            begin
              SetLength(VertexDecl, Length(VertexDecl) + 1);
              VertexDecl[High(VertexDecl)].Stream := 0;
              VertexDecl[High(VertexDecl)].Offset := Element.Offset;
              VertexDecl[High(VertexDecl)]._Type := D3DDECLTYPE_D3DCOLOR;
              VertexDecl[High(VertexDecl)].Method := D3DDECLMETHOD_DEFAULT;
              VertexDecl[High(VertexDecl)].Usage := D3DDECLUSAGE_COLOR;
              VertexDecl[High(VertexDecl)].UsageIndex := 1;
            end;
          TVertexFormat.vfColor2:
            begin
              SetLength(VertexDecl, Length(VertexDecl) + 1);
              VertexDecl[High(VertexDecl)].Stream := 0;
              VertexDecl[High(VertexDecl)].Offset := Element.Offset;
              VertexDecl[High(VertexDecl)]._Type := D3DDECLTYPE_D3DCOLOR;
              VertexDecl[High(VertexDecl)].Method := D3DDECLMETHOD_DEFAULT;
              VertexDecl[High(VertexDecl)].Usage := D3DDECLUSAGE_COLOR;
              VertexDecl[High(VertexDecl)].UsageIndex := 2;
            end;
          TVertexFormat.vfColor3:
            begin
              SetLength(VertexDecl, Length(VertexDecl) + 1);
              VertexDecl[High(VertexDecl)].Stream := 0;
              VertexDecl[High(VertexDecl)].Offset := Element.Offset;
              VertexDecl[High(VertexDecl)]._Type := D3DDECLTYPE_D3DCOLOR;
              VertexDecl[High(VertexDecl)].Method := D3DDECLMETHOD_DEFAULT;
              VertexDecl[High(VertexDecl)].Usage := D3DDECLUSAGE_COLOR;
              VertexDecl[High(VertexDecl)].UsageIndex := 3;
            end;
          TVertexFormat.vfTexCoord0:
            begin
              SetLength(VertexDecl, Length(VertexDecl) + 1);
              VertexDecl[High(VertexDecl)].Stream := 0;
              VertexDecl[High(VertexDecl)].Offset := Element.Offset;
              VertexDecl[High(VertexDecl)]._Type := D3DDECLTYPE_FLOAT2;
              VertexDecl[High(VertexDecl)].Method := D3DDECLMETHOD_DEFAULT;
              VertexDecl[High(VertexDecl)].Usage := D3DDECLUSAGE_TEXCOORD;
              VertexDecl[High(VertexDecl)].UsageIndex := 0;
            end;
          TVertexFormat.vfTexCoord1:
            begin
              SetLength(VertexDecl, Length(VertexDecl) + 1);
              VertexDecl[High(VertexDecl)].Stream := 0;
              VertexDecl[High(VertexDecl)].Offset := Element.Offset;
              VertexDecl[High(VertexDecl)]._Type := D3DDECLTYPE_FLOAT2;
              VertexDecl[High(VertexDecl)].Method := D3DDECLMETHOD_DEFAULT;
              VertexDecl[High(VertexDecl)].Usage := D3DDECLUSAGE_TEXCOORD;
              VertexDecl[High(VertexDecl)].UsageIndex := 1;
            end;
          TVertexFormat.vfTexCoord2:
            begin
              SetLength(VertexDecl, Length(VertexDecl) + 1);
              VertexDecl[High(VertexDecl)].Stream := 0;
              VertexDecl[High(VertexDecl)].Offset := Element.Offset;
              VertexDecl[High(VertexDecl)]._Type := D3DDECLTYPE_FLOAT2;
              VertexDecl[High(VertexDecl)].Method := D3DDECLMETHOD_DEFAULT;
              VertexDecl[High(VertexDecl)].Usage := D3DDECLUSAGE_TEXCOORD;
              VertexDecl[High(VertexDecl)].UsageIndex := 2;
            end;
          TVertexFormat.vfTexCoord3:
            begin
              SetLength(VertexDecl, Length(VertexDecl) + 1);
              VertexDecl[High(VertexDecl)].Stream := 0;
              VertexDecl[High(VertexDecl)].Offset := Element.Offset;
              VertexDecl[High(VertexDecl)]._Type := D3DDECLTYPE_FLOAT2;
              VertexDecl[High(VertexDecl)].Method := D3DDECLMETHOD_DEFAULT;
              VertexDecl[High(VertexDecl)].Usage := D3DDECLUSAGE_TEXCOORD;
              VertexDecl[High(VertexDecl)].UsageIndex := 3;
            end;
          TVertexFormat.vfBiNormal:
            begin
              SetLength(VertexDecl, Length(VertexDecl) + 1);
              VertexDecl[High(VertexDecl)].Stream := 0;
              VertexDecl[High(VertexDecl)].Offset := Element.Offset;
              VertexDecl[High(VertexDecl)]._Type := D3DDECLTYPE_FLOAT3;
              VertexDecl[High(VertexDecl)].Method := D3DDECLMETHOD_DEFAULT;
              VertexDecl[High(VertexDecl)].Usage := D3DDECLUSAGE_BINORMAL;
              VertexDecl[High(VertexDecl)].UsageIndex := 0;
            end;
          TVertexFormat.vfTangent:
            begin
              SetLength(VertexDecl, Length(VertexDecl) + 1);
              VertexDecl[High(VertexDecl)].Stream := 0;
              VertexDecl[High(VertexDecl)].Offset := Element.Offset;
              VertexDecl[High(VertexDecl)]._Type := D3DDECLTYPE_FLOAT3;
              VertexDecl[High(VertexDecl)].Method := D3DDECLMETHOD_DEFAULT;
              VertexDecl[High(VertexDecl)].Usage := D3DDECLUSAGE_TANGENT;
              VertexDecl[High(VertexDecl)].UsageIndex := 0;
            end;
          TVertexFormat.vfColorF0:
            begin
              SetLength(VertexDecl, Length(VertexDecl) + 1);
              VertexDecl[High(VertexDecl)].Stream := 0;
              VertexDecl[High(VertexDecl)].Offset := Element.Offset;
              VertexDecl[High(VertexDecl)]._Type := D3DDECLTYPE_FLOAT4;
              VertexDecl[High(VertexDecl)].Method := D3DDECLMETHOD_DEFAULT;
              VertexDecl[High(VertexDecl)].Usage := D3DDECLUSAGE_COLOR;
              VertexDecl[High(VertexDecl)].UsageIndex := 0;
            end;
          TVertexFormat.vfColorF1:
            begin
              SetLength(VertexDecl, Length(VertexDecl) + 1);
              VertexDecl[High(VertexDecl)].Stream := 0;
              VertexDecl[High(VertexDecl)].Offset := Element.Offset;
              VertexDecl[High(VertexDecl)]._Type := D3DDECLTYPE_FLOAT4;
              VertexDecl[High(VertexDecl)].Method := D3DDECLMETHOD_DEFAULT;
              VertexDecl[High(VertexDecl)].Usage := D3DDECLUSAGE_COLOR;
              VertexDecl[High(VertexDecl)].UsageIndex := 0;
            end;
          TVertexFormat.vfColorF2:
            begin
              SetLength(VertexDecl, Length(VertexDecl) + 1);
              VertexDecl[High(VertexDecl)].Stream := 0;
              VertexDecl[High(VertexDecl)].Offset := Element.Offset;
              VertexDecl[High(VertexDecl)]._Type := D3DDECLTYPE_FLOAT4;
              VertexDecl[High(VertexDecl)].Method := D3DDECLMETHOD_DEFAULT;
              VertexDecl[High(VertexDecl)].Usage := D3DDECLUSAGE_COLOR;
              VertexDecl[High(VertexDecl)].UsageIndex := 0;
            end;
          TVertexFormat.vfColorF3:
            begin
              SetLength(VertexDecl, Length(VertexDecl) + 1);
              VertexDecl[High(VertexDecl)].Stream := 0;
              VertexDecl[High(VertexDecl)].Offset := Element.Offset;
              VertexDecl[High(VertexDecl)]._Type := D3DDECLTYPE_FLOAT4;
              VertexDecl[High(VertexDecl)].Method := D3DDECLMETHOD_DEFAULT;
              VertexDecl[High(VertexDecl)].Usage := D3DDECLUSAGE_COLOR;
              VertexDecl[High(VertexDecl)].UsageIndex := 0;
            end;
        end;
      end;

      SetLength(VertexDecl, Length(VertexDecl) + 1);
      VertexDecl[High(VertexDecl)] := D3DDECL_END;

      OldVertexDeclaration := FVertexDeclaration;
      FVertexDeclaration := nil;
      SharedDevice.CreateVertexDeclaration(@VertexDecl[0], FVertexDeclaration);
      OldVertexDeclaration := nil;

      SharedDevice.SetVertexDeclaration(FVertexDeclaration);
      SharedDevice.SetStreamSource(0, FVB, FVBLockPos, VertexSize);

      if IndexSize = 4 then
      begin
        SharedDevice.SetIndices(FIB32);
        case AKind of
          TPrimitivesKind.pkPoints:
            FAILED(SharedDevice.DrawIndexedPrimitive(D3DPT_POINTLIST, 0, 0, VertexCount, FIB32LockPos div 4, IndexCount));
          TPrimitivesKind.pkLines:
            FAILED(SharedDevice.DrawIndexedPrimitive(D3DPT_LINELIST, 0, 0, VertexCount, FIB32LockPos div 4, IndexCount div 2));
        else
          FAILED(SharedDevice.DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, VertexCount, FIB32LockPos div 4, IndexCount div 3));
        end;
        FIB32LockPos := FIB32LockPos + IndexCount * IndexSize;
      end else begin
        SharedDevice.SetIndices(FIB16);
        case AKind of
          TPrimitivesKind.pkPoints:
            FAILED(SharedDevice.DrawIndexedPrimitive(D3DPT_POINTLIST, 0, 0, VertexCount, FIB16LockPos div 2, IndexCount));
          TPrimitivesKind.pkLines:
            FAILED(SharedDevice.DrawIndexedPrimitive(D3DPT_LINELIST, 0, 0, VertexCount, FIB16LockPos div 2, IndexCount div 2));
        else
          FAILED(SharedDevice.DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, VertexCount, FIB16LockPos div 2, IndexCount div 3));
        end;
        FIB16LockPos := FIB16LockPos + IndexCount * IndexSize;
      end;

      FVBLockPos := FVBLockPos + VertexCount * VertexSize;
    end;
  end;
end;

{ Resources }

{$WARNINGS OFF} 

class function TDX9Context.AddResource(const Resource: IInterface): THandle;
begin
  if FResources = nil then
  begin
    FResources := TInterfaceList.Create;
    // Fill in the first slot with a dummy entry. This will make it so that a TContextShader value of 0 is invalid.
    FResources.Add(TInterfacedObject.Create);
  end;
  Result := 0;
  while (Result < FResources.Count) and (FResources[Result] <> nil) do
    Inc(Result);
  if Result < FResources.Count then
    FResources[Result] := Resource
  else
    Result := FResources.Add(Resource);
end;

class procedure TDX9Context.RemoveResource(Resource: THandle);
begin
  if (FResources <> nil) and (Resource <> 0) then
    FResources[Resource] := nil;
end;

class function TDX9Context.ResourceToTexture(Resource: THandle): IDirect3DTexture9;
begin
  if (FResources <> nil) and (Resource > 0) and (Resource < FResources.Count) then
    Result := FResources[Resource] as IDirect3DTexture9
  else
    Result := nil;
end;

class function TDX9Context.ResourceToPixelShader(Resource: THandle): IDirect3DPixelShader9;
begin
  if (FResources <> nil) and (Resource > 0) and (Resource < FResources.Count) then
    Result := FResources[Resource] as IDirect3DPixelShader9
  else
    Result := nil;
end;

class function TDX9Context.ResourceToVertexShader(Resource: THandle): IDirect3DVertexShader9;
begin
  if (FResources <> nil) and (Resource > 0) and (Resource < FResources.Count) then
    Result := FResources[Resource] as IDirect3DVertexShader9
  else
    Result := nil;
end;

{ Textures }

class procedure TDX9Context.DoInitializeTexture(const Texture: TTexture);
var
  Tex: IDirect3DTexture9;
  Flags: Cardinal;
  Level: Integer;
begin
  CreateSharedDevice;
  if Assigned(FSharedDevice) then
  begin
    Flags := 0;
    if Texture.PixelFormat = TPixelFormat.pfUnknown then
      Texture.PixelFormat := TPixelFormat.pfA8R8G8B8;
    if TTextureStyle.tsDynamic in Texture.Style then
      Flags := Flags or D3DUSAGE_DYNAMIC;
    if TTextureStyle.tsRenderTarget in Texture.Style then
      Flags := Flags or D3DUSAGE_RENDERTARGET;
    Level := 1;
    if TTextureStyle.tsMipMaps in Texture.Style then
    begin
      if FCaps.Caps2 and D3DCAPS2_CANAUTOGENMIPMAP = D3DCAPS2_CANAUTOGENMIPMAP then
      begin
        HR := Direct3D9Obj.CheckDeviceFormat(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, FSharedDevicePresentParams.BackBufferFormat,
          Flags or D3DUSAGE_AUTOGENMIPMAP, D3DRTYPE_TEXTURE, TexturePixelFormatToDX(Texture.PixelFormat));
        if Succeeded(HR) then
        begin
          Flags := Flags or D3DUSAGE_AUTOGENMIPMAP;
          Level := 0;
        end;
      end;
    end;
    HR := SharedDevice.CreateTexture(Texture.Width, Texture.Height, Level, Flags, TexturePixelFormatToDX(Texture.PixelFormat), D3DPOOL_DEFAULT, Tex, nil);

    if Assigned(Tex) then
      ITextureAccess(Texture).Handle := AddResource(Tex);
  end;
end;

class procedure TDX9Context.DoFinalizeTexture(const Texture: TTexture);
begin
  if Assigned(FSharedDevice) then
    RemoveResource(Texture.Handle);
  ITextureAccess(Texture).Handle := 0;
end;

class procedure TDX9Context.DoUpdateTexture(const Texture: TTexture; const Bits: Pointer; const Pitch: Integer);
var
  I, BytesToCopy: Integer;
  Tex: IDirect3DTexture9;
  Surface: TD3DLockedRect;
begin
  if Texture.Handle <> 0 then
  begin
    Tex := ResourceToTexture(Texture.Handle);
    if Succeeded(Tex.LockRect(0, Surface, nil, D3DLOCK_DISCARD)) then
    try
      if Pitch = Surface.Pitch then
        Move(Bits^, Surface.pBits^, Texture.Height * Pitch)
      else
      begin
        BytesToCopy := Min(Pitch, Surface.Pitch);
        for I := 0 to Texture.Height - 1 do
          Move(PByteArray(Bits)[Pitch * I], PByteArray(Surface.pBits)[Surface.Pitch * I], BytesToCopy)
      end;
    finally
      Tex.UnlockRect(0);
    end;
  end;
end;

{ Shader }

class procedure TDX9Context.DoInitializeShader(const Shader: TContextShader);
var
  VSShader: IDirect3DVertexShader9;
  PSShader: IDirect3DPixelShader9;
  Source: TContextShaderSource;
begin
  CreateSharedDevice;
  if Assigned(FSharedDevice) then
  begin
    Source := Shader.GetSourceByArch(TContextShaderArch.saDX9);
    if Source.IsDefined then
    begin
      if Shader.Kind = TContextShaderKind.skVertexShader then
      begin
        SharedDevice.CreateVertexShader(PDWord(Source.Code), VSShader);
        if Assigned(VSShader) then
          Shader.Handle := AddResource(VSShader);
      end
      else
      begin
        SharedDevice.CreatePixelShader(PDWord(Source.Code), PSShader);
        if Assigned(PSShader) then
          Shader.Handle := AddResource(PSShader);
      end;
    end;
  end;
end;

class procedure TDX9Context.DoFinalizeShader(const Shader: TContextShader);
begin
  if Assigned(FSharedDevice) then
    RemoveResource(Shader.Handle);
  Shader.Handle := 0;
end;

procedure TDX9Context.DoSetShaders(const VertexShader, PixelShader: TContextShader);
begin
  if Assigned(SharedDevice) then
  begin
    HR := SharedDevice.SetVertexShader(ResourceToVertexShader(VertexShader.Handle));
    HR := SharedDevice.SetPixelShader(ResourceToPixelShader(PixelShader.Handle));
  end;
end;

procedure TDX9Context.DoSetShaderVariable(const Name: string; const Data: array of TVector3D);
var
  I: Integer;
  Source: TContextShaderSource;
begin
  if Assigned(SharedDevice) then
  begin
    if CurrentVertexShader <> nil then
    begin
      Source := CurrentVertexShader.GetSourceByArch(TContextShaderArch.saDX9);
      for I := 0 to High(Source.Variables) do
        if SameText(Source.Variables[I].Name, Name) then
        begin
          SharedDevice.SetVertexShaderConstantF(Source.Variables[I].Index, PSingle(@Data), Min(Length(Data), Source.Variables[I].Size));
          Exit;
        end;
    end;
    if CurrentPixelShader <> nil then
    begin
      Source := CurrentPixelShader.GetSourceByArch(TContextShaderArch.saDX9);
      for I := 0 to High(Source.Variables) do
        if SameText(Source.Variables[I].Name, Name) then
        begin
          SharedDevice.SetPixelShaderConstantF(Source.Variables[I].Index, PSingle(@Data), Min(Length(Data), Source.Variables[I].Size));
          Exit;
        end;
    end;
  end;
end;

procedure TDX9Context.DoSetShaderVariable(const Name: string; const Texture: TTexture);
var
  I: Integer;
  Source: TContextShaderSource;
begin
  if Assigned(SharedDevice) and (CurrentPixelShader <> nil) then
  begin
    Source := CurrentPixelShader.GetSourceByArch(TContextShaderArch.saDX9);
    for I := 0 to High(Source.Variables) do
      if SameText(Source.Variables[I].Name, Name) then
      begin
        if Assigned(Texture) then
          SharedDevice.SetTexture(Source.Variables[I].Index, ResourceToTexture(Texture.Handle))
        else
          SharedDevice.SetTexture(Source.Variables[I].Index, nil);
        Exit;
      end;
  end;
end;

procedure TDX9Context.DoSetShaderVariable(const Name: string; const Matrix: TMatrix3D);
begin
  SetShaderVariable(Name, Matrix.M);
end;

procedure RegisterContextClasses;
var
  Direct3D9Obj: IDirect3D9;
  DM: TD3DDisplayMode;
begin
  Direct3D9Obj := Direct3DCreate9(D3D_SDK_VERSION);
  if Assigned(Direct3D9Obj) then
  begin
    if Failed(Direct3D9Obj.GetAdapterDisplayMode(D3DADAPTER_DEFAULT, DM)) then Exit;
    if Succeeded(Direct3D9Obj.CheckDeviceType(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, DM.Format, DM.Format, True)) then
      TContextManager.RegisterContext(TDX9Context, False);
  end;
end;

procedure UnregisterContextClasses;
begin
  TDX9Context.DestroySharedDevice;
end;

initialization
end.


