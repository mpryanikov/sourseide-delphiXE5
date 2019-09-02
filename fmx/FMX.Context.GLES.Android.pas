{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit FMX.Context.GLES.Android;

interface

uses
  System.Types, System.Classes, Androidapi.Egl, FMX.Types3D, FMX.Context.GLES;

type
  TCustomAndroidContext = class(TCustomContextOpenGL)
  protected class var
    FSharedDisplay: EGLDisplay;
    FSharedSurface: EGLSurface;
    FSharedContext: EGLContext;
    FFrozen: Boolean;
    class function GetSharedContext: EGLContext; static;
  protected
    class procedure CreateSharedContext; override;
    class procedure DestroySharedContext; override;
  public
    class function CreateContextFromActivity(const AWidth, AHeight: Integer; const AMultisample: TMultisample;
      const ADepthStencil: Boolean): TCustomAndroidContext;
    class procedure FreezeSharedContext;
    class procedure UnfreezeSharedContext;
    class property SharedDisplay: EGLDisplay read FSharedDisplay;
    class property SharedSurface: EGLSurface read FSharedSurface;
    class property SharedContext: EGLContext read GetSharedContext;
    class function IsContextAvailable: Boolean; override;
  end;

procedure RegisterContextClasses;
procedure UnregisterContextClasses;

implementation

uses
  Androidapi.Gles2, Androidapi.Gles2ext, {Androidapi.Eglext,} Androidapi.NativeWindow, FMX.Consts, FMX.Types,
  FMX.PixelFormats, FMX.Materials, FMX.Messages, FMX.Platform.Android;

const
  OES_packed_depth_stencil = 'GL_OES_packed_depth_stencil';
  NV_depth_nonlinear = 'GL_NV_depth_nonlinear';

  // Temporary workaround for RS-41928, where including "Androidapi.Eglext" crashes applications on Android device.
  EGL_DEPTH_ENCODING_NV = $30E2;
  EGL_DEPTH_ENCODING_NONLINEAR_NV = $30E3;

{ TCustomAndroidContext }

class procedure TCustomAndroidContext.CreateSharedContext;

const
  ContextAttributes: array[0..2] of EGLint = (EGL_CONTEXT_CLIENT_VERSION, 2, EGL_NONE);

  function CreateDummyContext: Boolean;
  const
    DummyConfig: array[0..4] of EGLint = (EGL_BUFFER_SIZE, 32, EGL_RENDERABLE_TYPE, EGL_OPENGL_ES2_BIT, EGL_NONE);
  var
    Config: EGLConfig;
    NumConfigs: EGLint;
    Format: EGLint;
  begin
    if eglChooseConfig(FSharedDisplay, @DummyConfig[0], @Config, 1, @NumConfigs) = 0 then
      Exit(False);

    eglGetConfigAttrib(FSharedDisplay, Config, EGL_NATIVE_VISUAL_ID, @Format);
    ANativeWindow_setBuffersGeometry(GetAndroidApp^.window, 0, 0, Format);

    FSharedSurface := eglCreateWindowSurface(FSharedDisplay, Config, GetAndroidApp^.window, nil);
    FSharedContext := eglCreateContext(FSharedDisplay, Config, EGL_NO_CONTEXT, @ContextAttributes[0]);

    if eglMakeCurrent(FSharedDisplay, FSharedSurface, FSharedSurface, FSharedContext) = 0 then
      Exit(False);

    Result := True;
  end;

  procedure DestroyDummyContext;
  begin
    eglMakeCurrent(FSharedDisplay, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);
    eglDestroySurface(FSharedDisplay, FSharedSurface);
    eglDestroyContext(FSharedDisplay, FSharedContext);
  end;

const
  DefaultConfig: array[0..8] of EGLint = (EGL_BUFFER_SIZE, 32, EGL_RENDERABLE_TYPE, EGL_OPENGL_ES2_BIT,
    EGL_DEPTH_SIZE, 24, EGL_STENCIL_SIZE, 8, EGL_NONE);
var
  Config: EGLConfig;
  ConfigAttribs: array of EGLint;
  NumConfigs: EGLint;
  Format: EGLint;
begin
  if not Assigned(FSharedContext) and not FFrozen and (GetAndroidApp <> nil) and Assigned(GetAndroidApp^.window) then
  begin
    FSharedDisplay := eglGetDisplay(EGL_DEFAULT_DISPLAY);

    if eglInitialize(FSharedDisplay, nil, nil) = 0 then
      RaiseContextExceptionFmt(@SCannotCreateOpenGLContext, ['eglInitialize']);

    // Create dummy OpenGL ES context from which extensions can be obtained.
    if not CreateDummyContext then
      RaiseContextExceptionFmt(@SCannotCreateOpenGLContext, ['CreateDummyContext']);
    try
      GetExtensions;
    finally
      DestroyDummyContext;
    end;

    // Now that extensions are available, create final configuration for OpenGL ES context.
    SetLength(ConfigAttribs, High(DefaultConfig) + 1);
    Move(DefaultConfig[0], ConfigAttribs[0], (High(DefaultConfig) + 1) * SizeOf(EGLint));

    // Tegra 3 GPU does not support 24-bit depth buffer so fall back to 16-bit.
    if not Extensions[OES_packed_depth_stencil] then
      ConfigAttribs[5] := 16;

    { Tegra 3 GPU although supports only 16-bit depth buffer, has non-linear depth buffer encoding extension, which
      allows to reduce depth fighting in typical 3D applications. }
    if Extensions[NV_depth_nonlinear] then
    begin
      SetLength(ConfigAttribs, Length(ConfigAttribs) + 2);

      ConfigAttribs[Length(ConfigAttribs) - 3] := EGL_DEPTH_ENCODING_NV;
      ConfigAttribs[Length(ConfigAttribs) - 2] := EGL_DEPTH_ENCODING_NONLINEAR_NV;
      ConfigAttribs[Length(ConfigAttribs) - 1] := EGL_NONE;
    end;

    if eglChooseConfig(FSharedDisplay, @ConfigAttribs[0], @Config, 1, @NumConfigs) = 0 then
      RaiseContextExceptionFmt(@SCannotCreateOpenGLContext, ['eglChooseConfig']);

    eglGetConfigAttrib(FSharedDisplay, Config, EGL_NATIVE_VISUAL_ID, @Format);
    ANativeWindow_setBuffersGeometry(GetAndroidApp^.window, 0, 0, Format);

    FSharedSurface := eglCreateWindowSurface(FSharedDisplay, Config, GetAndroidApp^.window, nil);
    FSharedContext := eglCreateContext(FSharedDisplay, Config, EGL_NO_CONTEXT, @ContextAttributes[0]);

    if eglMakeCurrent(FSharedDisplay, FSharedSurface, FSharedSurface, FSharedContext) = 0 then
      RaiseContextExceptionFmt(@SCannotCreateOpenGLContext, ['eglMakeCurrent']);

    TCustomAndroidContext.FStyle := [TContextStyle.RenderTargetFlipped, TContextStyle.Fragile];
    TCustomAndroidContext.FPixelFormat := TPixelFormat.pfA8B8G8R8;
    TCustomAndroidContext.FPixelToPixelPolygonOffset :=  TPointF.Create(0, 0);
    TCustomAndroidContext.FMaxLightCount := 1;

    FillMetrics;
  end;
end;

class procedure TCustomAndroidContext.DestroySharedContext;
begin
  if Assigned(FSharedContext) then
  begin
    DestroyPrograms;
    eglDestroySurface(eglGetCurrentDisplay, FSharedSurface);
    eglDestroyContext(eglGetCurrentDisplay, FSharedContext);
    FSharedContext := nil;
  end;
end;

class procedure TCustomAndroidContext.FreezeSharedContext;
begin
  TMessageManager.DefaultManager.SendMessage(nil, TContextLostMessage.Create, False);
  DestroySharedContext;
  eglMakeCurrent(FSharedDisplay, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);
  FFrozen := True;
end;

class procedure TCustomAndroidContext.UnfreezeSharedContext;
begin
  FFrozen := False;
  CreateSharedContext;
  ResetStates;
  TMessageManager.DefaultManager.SendMessage(nil, TContextResetMessage.Create, True);
end;

class function TCustomAndroidContext.IsContextAvailable: Boolean;
begin
  Result := Assigned(SharedContext) and not FFrozen;
end;

class function TCustomAndroidContext.GetSharedContext: EGLContext;
begin
  CreateSharedContext;
  Result := FSharedContext;
end;

{ TContextAndroid }

type
  TContextAndroid = class(TCustomAndroidContext)
  private
    HandlesLostResetMessages: Boolean;
    FActivity: Boolean;
    FContextLostId: Integer;
    FContextResetId: Integer;
    FLostBits: Pointer;
    function SupportBuffers: Boolean;
    procedure ContextLostHandler(const Sender: TObject; const Msg: TMessage);
    procedure ContextResetHandler(const Sender: TObject; const Msg: TMessage);
    function CreateFrameBuffer(const BufferWidth, BufferHeight: GLint; const TextureHandle: GLuint;
      const DepthStencil: Boolean; out FrameBuf, DepthBuf, StencilBuf: GLuint): Boolean;
  protected
    function GetValid: Boolean; override;
    class function GetShaderArch: TContextShaderArch; override;
    procedure DoSetScissorRect(const ScissorRect: TRect); override;
    { buffer }
    procedure DoCreateBuffer; override;
    procedure DoFreeBuffer; override;
    { constructors }
    constructor CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer;
      const AMultisample: TMultisample; const ADepthStencil: Boolean); override;
    constructor CreateFromTexture(const ATexture: TTexture; const AMultisample: TMultisample;
      const ADepthStencil: Boolean); override;
    constructor CreateFromActivity(const AWidth, AHeight: Integer; const AMultisample: TMultisample;
      const ADepthStencil: Boolean);
  public
    destructor Destroy; override;
  end;

{ TContextAndroid }

constructor TContextAndroid.CreateFromActivity(const AWidth, AHeight: Integer; const AMultisample: TMultisample;
  const ADepthStencil: Boolean);
begin
  FActivity := True;
  inherited CreateFromWindow(nil, AWidth, AHeight, AMultisample, ADepthStencil);
  CreateSharedContext;
  HandlesLostResetMessages := False;
end;

constructor TContextAndroid.CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer;
  const AMultisample: TMultisample; const ADepthStencil: Boolean);
begin
  FSupportMS := False;
  inherited;
  CreateSharedContext;

  if SupportBuffers then
  begin
    CreateBuffer;
    FContextLostId := TMessageManager.DefaultManager.SubscribeToMessage(TContextLostMessage, ContextLostHandler);
    FContextResetId := TMessageManager.DefaultManager.SubscribeToMessage(TContextResetMessage, ContextResetHandler);
    HandlesLostResetMessages := True;
  end;
end;

constructor TContextAndroid.CreateFromTexture(const ATexture: TTexture; const AMultisample: TMultisample;
  const ADepthStencil: Boolean);
begin
  FSupportMS := False;
  inherited;
  FContextLostId := TMessageManager.DefaultManager.SubscribeToMessage(TContextLostMessage, ContextLostHandler);
  FContextResetId := TMessageManager.DefaultManager.SubscribeToMessage(TContextResetMessage, ContextResetHandler);
  HandlesLostResetMessages := True;
end;

destructor TContextAndroid.Destroy;
begin
  if HandlesLostResetMessages then
  begin
    TMessageManager.DefaultManager.Unsubscribe(TContextLostMessage, FContextLostId);
    TMessageManager.DefaultManager.Unsubscribe(TContextResetMessage, FContextResetId);
    HandlesLostResetMessages := False;
  end;

  inherited;
end;

function TContextAndroid.SupportBuffers: Boolean;
begin
  Result := Assigned(Parent) and TAndroidWindowHandle(Parent).IsPopup;
end;

procedure TContextAndroid.ContextLostHandler(const Sender: TObject; const Msg: TMessage);
var
  Pitch: Integer;
  OldFBO: GLuint;
begin
  if not Assigned(Parent) and Assigned(Texture) then
  begin
    Pitch := Width * Texture.BytesPerPixel;
    GetMem(FLostBits, Height * Pitch);

    glGetIntegerv(GL_FRAMEBUFFER_BINDING, @OldFBO);
    glBindFramebuffer(GL_FRAMEBUFFER, FFrameBuf);
    glReadPixels(0, 0, Width, Height, GL_RGBA, GL_UNSIGNED_BYTE, FLostBits);
    glBindFramebuffer(GL_FRAMEBUFFER, OldFBO);
  end;

  FreeBuffer;
end;

procedure TContextAndroid.ContextResetHandler(const Sender: TObject; const Msg: TMessage);
var
  OldTexture: GLuint;
begin
  if not Assigned(Parent) and Assigned(Texture) and Assigned(FLostBits) then
  begin
    if Texture.Handle = 0 then
      Texture.Initialize;

    glGetIntegerv(GL_TEXTURE_BINDING_2D, @OldTexture);
    glBindTexture(GL_TEXTURE_2D, Texture.Handle);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, Texture.Width, Texture.Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, FLostBits);
    glBindTexture(GL_TEXTURE_2D, OldTexture);

    FreeMem(FLostBits);
    FLostBits := nil;
  end;
  CreateBuffer;
end;

function TContextAndroid.CreateFrameBuffer(const BufferWidth, BufferHeight: GLint; const TextureHandle: GLuint;
  const DepthStencil: Boolean; out FrameBuf, DepthBuf, StencilBuf: GLuint): Boolean;
var
  Status: GLint;
begin
  glGenFramebuffers(1, @FrameBuf);
  glBindFramebuffer(GL_FRAMEBUFFER, FrameBuf);

  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, TextureHandle, 0);

  if DepthStencil then
  begin
    if Extensions[OES_packed_depth_stencil] then
    begin // using OES_packed_depth_stencil extension
      glGenRenderbuffers(1, @DepthBuf);
      glBindRenderbuffer(GL_RENDERBUFFER, DepthBuf);
      glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH24_STENCIL8_OES, BufferWidth, BufferHeight);
      glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, DepthBuf);
      glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_STENCIL_ATTACHMENT, GL_RENDERBUFFER, DepthBuf);
      glBindRenderbuffer(GL_RENDERBUFFER, 0);
      StencilBuf := 0;
    end
    else
    begin // attempting more conservative approach
      glGenRenderbuffers(1, @DepthBuf);
      glBindRenderbuffer(GL_RENDERBUFFER, DepthBuf);
      glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT16, BufferWidth, BufferHeight);
      glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, DepthBuf);

      glGenRenderbuffers(1, @StencilBuf);
      glBindRenderbuffer(GL_RENDERBUFFER, StencilBuf);
      glRenderbufferStorage(GL_RENDERBUFFER, GL_STENCIL_INDEX8, BufferWidth, BufferHeight);
      glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_STENCIL_ATTACHMENT, GL_RENDERBUFFER, StencilBuf);
      glBindRenderbuffer(GL_RENDERBUFFER, 0);
    end;
  end;

  Status := glCheckFramebufferStatus(GL_FRAMEBUFFER);
  if (Status <> GL_FRAMEBUFFER_COMPLETE) or GLHasAnyErrors then
  begin
    if StencilBuf <> 0 then
    begin
      glDeleteRenderbuffers(1, @StencilBuf);
      StencilBuf := 0;
    end;

    if DepthBuf <> 0 then
    begin
      glDeleteRenderbuffers(1, @DepthBuf);
      DepthBuf := 0;
    end;

    if FrameBuf <> 0 then
    begin
      glDeleteFramebuffers(1, @FrameBuf);
      FrameBuf := 0;
    end;

    Result := False;
  end
  else
    Result := True;
end;

procedure TContextAndroid.DoCreateBuffer;
var
  OldFBO: GLuint;
  WindowTexture: TTexture;
begin
  if IsContextAvailable and SupportBuffers and (Width > 0) and (Height > 0) then
  begin
    WindowHandleToPlatform(Parent).CreateTexture;
    WindowTexture := WindowHandleToPlatform(Parent).Texture;

    glGetIntegerv(GL_FRAMEBUFFER_BINDING, @OldFBO);
    try
      if (not CreateFrameBuffer(WindowTexture.Width, WindowTexture.Height, WindowTexture.Handle,
        DepthStencil, FFrameBuf, FDepthBuf, FStencilBuf)) and DepthStencil then
      begin
        if not CreateFrameBuffer(WindowTexture.Width, WindowTexture.Height, WindowTexture.Handle,
          False, FFrameBuf, FDepthBuf, FStencilBuf) then
          RaiseContextExceptionFmt(@SCannotCreateRenderBuffers, [ClassName]);
      end;
    finally
      glBindFramebuffer(GL_FRAMEBUFFER, OldFBO);
    end;
  end;

  if IsContextAvailable and Assigned(Texture) then
  begin
    if Texture.Handle = 0 then
      Texture.Initialize;

    glGetIntegerv(GL_FRAMEBUFFER_BINDING, @OldFBO);
    try
      if (not CreateFrameBuffer(Width, Height, Texture.Handle, DepthStencil, FFrameBuf, FDepthBuf, FStencilBuf)) and
        DepthStencil then
      begin
        if not CreateFrameBuffer(Width, Height, Texture.Handle, False, FFrameBuf, FDepthBuf, FStencilBuf) then
          RaiseContextExceptionFmt(@SCannotCreateRenderBuffers, [ClassName]);
      end;
    finally
      glBindFramebuffer(GL_FRAMEBUFFER, OldFBO);
    end;
  end;
end;

procedure TContextAndroid.DoFreeBuffer;
begin
  if IsContextAvailable and SupportBuffers and Assigned(Parent) then
    WindowHandleToPlatform(Parent).DestroyTexture;
  inherited;
end;

class function TContextAndroid.GetShaderArch: TContextShaderArch;
begin
  Result := TContextShaderArch.saAndroid;
end;

function TContextAndroid.GetValid: Boolean;
begin
  Result := IsContextAvailable;
  if Result then
  begin
    eglMakeCurrent(eglGetCurrentDisplay, FSharedSurface, FSharedSurface, FSharedContext);
    if GLHasAnyErrors then
      RaiseContextExceptionFmt(@SErrorInContextMethod, ['GetValid']);
  end;
end;

procedure TContextAndroid.DoSetScissorRect(const ScissorRect: TRect);
var
  R: TRect;
begin
  R := Rect(Round(ScissorRect.Left * FScale), Round(ScissorRect.Top * FScale),
    Round(ScissorRect.Right * FScale), Round(ScissorRect.Bottom * FScale));

  if Assigned(Texture) then
    glScissor(R.Left, Height - R.Bottom, R.Width, R.Height)
  else
    glScissor(R.Left, Round(Height * FScale) - R.Bottom, R.Width, R.Height);

  if (GLHasAnyErrors()) then
    RaiseContextExceptionFmt(@SErrorInContextMethod, ['DoSetScissorRect']);
end;

class function TCustomAndroidContext.CreateContextFromActivity(const AWidth, AHeight: Integer;
  const AMultisample: TMultisample; const ADepthStencil: Boolean): TCustomAndroidContext;
begin
  Result := TContextAndroid.CreateFromActivity(AWidth, AHeight, AMultisample, ADepthStencil);
end;

procedure RegisterContextClasses;
begin
  TContextManager.RegisterContext(TContextAndroid, True);
end;

procedure UnregisterContextClasses;
begin
  TContextAndroid.DestroySharedContext;
end;

end.
