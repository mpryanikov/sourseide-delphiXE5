{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit FMX.Context.GLES.iOS;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.UITypes, System.UIConsts, System.Generics.Collections, System.Math,
  Macapi.CoreFoundation, iOSapi.CocoaTypes, iOSapi.CoreGraphics, iOSapi.Foundation, iOSapi.UIKit, iOSapi.OpenGLES, iOSapi.GLKit,
  FMX.Types, FMX.Types3D, FMX.Platform, FMX.Filter, FMX.Graphics, FMX.Context.GLES;

type

  TCustomContextIOS = class(TCustomContextOpenGL)
  protected class var
    FSharedContext: EAGLContext;
    class function GetSharedContext: EAGLContext; static;
  protected
    class procedure CreateSharedContext; override;
    class procedure DestroySharedContext; override;
  public
    class property SharedContext: EAGLContext read GetSharedContext;
    class function IsContextAvailable: Boolean; override;
  end;

procedure RegisterContextClasses;
procedure UnregisterContextClasses;

implementation {===============================================================}

uses
  FMX.Platform.iOS, FMX.Forms, FMX.Consts, FMX.Canvas.GPU, FMX.Materials, FMX.PixelFormats;

{ TCustomContextIOS }

class procedure TCustomContextIOS.CreateSharedContext;
begin
  if FSharedContext = nil then
  begin
    FSharedContext := TEAGLContext.Wrap(TEAGLContext.Create.initWithAPI(kEAGLRenderingAPIOpenGLES2));
    TEAGLContext.OCClass.setCurrentContext(FSharedContext);

    TCustomContextIOS.FStyle := [TContextStyle.RenderTargetFlipped];
    TCustomContextIOS.FPixelFormat := TPixelFormat.pfA8B8G8R8;
    TCustomContextIOS.FPixelToPixelPolygonOffset := PointF(0, 0);
    TCustomContextIOS.FMaxLightCount := 4;
    FillMetrics;
  end;
end;

class procedure TCustomContextIOS.DestroySharedContext;
begin
  if FSharedContext <> nil then
  begin
    DestroyPrograms;
    FSharedContext := nil;
  end;
end;

class function TCustomContextIOS.IsContextAvailable: Boolean;
begin
  Result := Assigned(SharedContext);
end;

class function TCustomContextIOS.GetSharedContext: EAGLContext;
begin
  CreateSharedContext;
  Result := FSharedContext;
end;

{ TContextOpenGL }

type

  TContextIOS = class(TCustomContextIOS)
  private
  protected
    function GetValid: Boolean; override;
    class function GetShaderArch: TContextShaderArch; override;
    procedure DoSetScissorRect(const ScissorRect: TRect); override;
    { buffer }
    procedure DoCreateBuffer; override;
    procedure DoEndScene; override;
    { constructors }
    constructor CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer;
      const AMultisample: TMultisample; const ADepthStencil: Boolean); override;
    constructor CreateFromTexture(const ATexture: TTexture; const AMultisample: TMultisample;
      const ADepthStencil: Boolean); override;
  public
  end;

{ TContextIOS }

constructor TContextIOS.CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer;
      const AMultisample: TMultisample; const ADepthStencil: Boolean);
var
  View: GLKView;
begin
  inherited;
  View := WindowHandleToPlatform(AParent).GLView;
  if Assigned(View) and (Multisample <> TMultisample.msNone) then
    View.setDrawableMultisample(GLKViewDrawableMultisample4X);
  CreateSharedContext;
  CreateBuffer;
end;

constructor TContextIOS.CreateFromTexture(const ATexture: TTexture; const AMultisample: TMultisample;
      const ADepthStencil: Boolean);
begin
  {$WARNINGS OFF}
  FSupportMS := Pos('gl_apple_framebuffer_multisample', LowerCase(MarshaledAString(glGetString(GL_EXTENSIONS)))) > 0;
  {$WARNINGS ON}
  inherited;
end;

procedure TContextIOS.DoCreateBuffer;
var
  Status: Integer;
  OldFBO: GLuint;
begin
  if IsContextAvailable and Assigned(Texture) then
  begin
    { create buffers }
    if (Multisample <> TMultisample.msNone) and FSupportMS then
    begin
      glGetIntegerv(GL_FRAMEBUFFER_BINDING, @OldFBO);
      glGenFramebuffers(1, @FFrameBuf);
      glBindFramebuffer(GL_FRAMEBUFFER, FFrameBuf);

      glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, Texture.Handle, 0);

      if DepthStencil then
      begin
        glGenRenderbuffers(1, @FDepthBuf);
        glBindRenderbuffer(GL_RENDERBUFFER, FDepthBuf);
        glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH24_STENCIL8_OES, Width, Height);
        glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, FDepthBuf);
        glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_STENCIL_ATTACHMENT, GL_RENDERBUFFER, FDepthBuf);
        glBindRenderbuffer(GL_RENDERBUFFER, 0);
      end;

      Status := glCheckFramebufferStatus(GL_FRAMEBUFFER);
      if Status <> GL_FRAMEBUFFER_COMPLETE then
        RaiseContextExceptionFmt(@SCannotCreateRenderBuffers, [ClassName]);

      { MS }
      glGenFramebuffers(1, @FFrameBufMS);
      glBindFramebuffer(GL_FRAMEBUFFER, FFrameBufMS);
      glGenRenderbuffers(1, @FRenderBufMS);
      glBindRenderbuffer(GL_RENDERBUFFER, FRenderBufMS);
      glRenderbufferStorageMultisampleAPPLE(GL_RENDERBUFFER, FMSValue, GL_RGBA8_OES, Width, Height);
      glFrameBufferRenderBuffer(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_RENDERBUFFER, FRenderBufMS);
      if DepthStencil then
      begin
        glGenRenderbuffers(1, @FDepthBufMS);
        glBindRenderbuffer(GL_RENDERBUFFER, FDepthBufMS);
        glRenderbufferStorageMultisampleAPPLE(GL_RENDERBUFFER, FMSValue, GL_DEPTH24_STENCIL8_OES, Width, Height);
        glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, FDepthBufMS);
        glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_STENCIL_ATTACHMENT, GL_RENDERBUFFER, FDepthBufMS);
        glBindRenderbuffer(GL_RENDERBUFFER, 0);
      end;
      Status := glCheckFramebufferStatus(GL_FRAMEBUFFER);
      if Status <> GL_FRAMEBUFFER_COMPLETE then
        RaiseContextExceptionFmt(@SCannotCreateRenderBuffers, [ClassName]);

      glBindFramebuffer(GL_FRAMEBUFFER, OldFBO);
    end
    else
    begin
      glGetIntegerv(GL_FRAMEBUFFER_BINDING, @OldFBO);
      glGenFramebuffers(1, @FFrameBuf);
      glBindFramebuffer(GL_FRAMEBUFFER, FFrameBuf);

      glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, Texture.Handle, 0);

      if DepthStencil then
      begin
        glGenRenderbuffers(1, @FDepthBuf);
        glBindRenderbuffer(GL_RENDERBUFFER, FDepthBuf);
        glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH24_STENCIL8_OES, Width, Height);
        glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, FDepthBuf);
        glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_STENCIL_ATTACHMENT, GL_RENDERBUFFER, FDepthBuf);
        glBindRenderbuffer(GL_RENDERBUFFER, 0);
      end;

      Status := glCheckFramebufferStatus(GL_FRAMEBUFFER);
      if Status <> GL_FRAMEBUFFER_COMPLETE then
        RaiseContextExceptionFmt(@SCannotCreateRenderBuffers, [ClassName]);

      glBindFramebuffer(GL_FRAMEBUFFER, OldFBO);
    end;
    if (GLHasAnyErrors()) then
      RaiseContextExceptionFmt(@SCannotCreateRenderBuffers, [ClassName]);
  end;
end;

procedure TContextIOS.DoEndScene;
begin
  if Valid then
  begin
    if FFrameBufMS <> 0 then
    begin
      glBindFramebuffer(GL_READ_FRAMEBUFFER_APPLE, FFrameBufMS);
      glBindFramebuffer(GL_DRAW_FRAMEBUFFER_APPLE, FFrameBuf);
      glResolveMultisampleFramebufferAPPLE;
      glBindFramebuffer(GL_READ_FRAMEBUFFER_APPLE, 0);
      glBindFramebuffer(GL_DRAW_FRAMEBUFFER_APPLE, 0);
    end;
    inherited;
  end;
end;

procedure TContextIOS.DoSetScissorRect(const ScissorRect: TRect);
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

class function TContextIOS.GetShaderArch: TContextShaderArch;
begin
  Result := TContextShaderArch.saIOS;
end;

function TContextIOS.GetValid: Boolean;
begin
  Result := IsContextAvailable;
  if Result then
    TEAGLContext.OCClass.setCurrentContext(FSharedContext);
end;

procedure RegisterContextClasses;
begin
  TContextManager.RegisterContext(TContextIOS, True);
end;

procedure UnregisterContextClasses;
begin
  TContextIOS.DestroySharedContext;
end;


end.
