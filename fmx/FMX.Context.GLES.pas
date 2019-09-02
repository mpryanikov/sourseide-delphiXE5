{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit FMX.Context.GLES;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.UITypes, System.UIConsts, System.Generics.Collections, System.Math,
  {$IFDEF IOS}
  iOSapi.OpenGLES,
  {$ENDIF}
  {$IFDEF ANDROID}
  Androidapi.Egl, Androidapi.Gles2, Androidapi.Gles2ext,
  {$ENDIF}
  FMX.Types, FMX.Types3D, FMX.Platform, FMX.Filter, FMX.Forms, FMX.Graphics;

{$SCOPEDENUMS ON}

const
  ATTRIB_VERTEX     = 0;
  ATTRIB_NORMAL     = 1;
  ATTRIB_COLOR0     = 2;
  ATTRIB_COLOR1     = 3;
  ATTRIB_COLOR2     = 4;
  ATTRIB_COLOR3     = 5;
  ATTRIB_TEXCOORD0  = 6;
  ATTRIB_TEXCOORD1  = 7;
  ATTRIB_TEXCOORD2  = 8;
  ATTRIB_TEXCOORD3  = 9;

type
  TShaderProgram = class
  private
    VS, PS: THandle;
    Prog: THandle;
    Variables: TDictionary<string, TContextShaderVariable>;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TExtensions = class
  private
    Extensions: TStringList;

    procedure FillExtensions;
    function GetExtension(const Name: string): Boolean;
  public
    property Extension[const Name: string]: Boolean read GetExtension; default;

    constructor Create;
    destructor Destroy; override;
  end;

  TCustomContextOpenGL = class(TContext3D)
  public type
    TIndexBufferSupport = (Unknown, Int16, Int32);
  private
    class var FExtensions: TExtensions;
    class var FIndexBufferSupport: TIndexBufferSupport;
    class function GetIndexBufferSupport: TIndexBufferSupport; static;
  protected class var
    FPrograms: TList<TShaderProgram>;
    FCurrentProgram: TShaderProgram;
  protected
    class function GetExtensions: TExtensions; static;
  protected
    { buffers }
    FScale: Single;
    FRenderBuf: GLuint;
    FFrameBuf: GLuint;
    FDepthBuf: GLuint;
    FStencilBuf: GLuint;
    { MS }
    FSupportMS: Boolean;
    FMSValue: Integer;
    FRenderBufMS: GLuint;
    FFrameBufMS: GLuint;
    FDepthBufMS: GLuint;
    { States }
    FOldFBO: GLuint;
    FOldViewport: array [0..3] of GLint;
    { Activity }
    FActivity: Boolean;
    class function BuildShader(AType: Integer; const ACode: TContextShaderCode): Integer;
    class function FindProgram(const VS, PS: TContextShader): TShaderProgram;
    class procedure UseProgram(const VS, PS: TContextShader);
    class function GetShaderArch: TContextShaderArch; virtual; abstract;
  protected
    class procedure FillMetrics;
    class procedure DestroyPrograms;
    class procedure CreateSharedContext; virtual; abstract;
    class procedure DestroySharedContext; virtual; abstract;
    { buffer }
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
    class function IsContextAvailable: Boolean; virtual; abstract;
    class function GLHasAnyErrors: Boolean;
    class property Extensions: TExtensions read GetExtensions;
    class property IndexBufferSupport: TIndexBufferSupport read GetIndexBufferSupport;
  end;

procedure RaiseContextExceptionFmt(ResStringRec: PResStringRec; const Args: array of const);

implementation {===============================================================}

uses
  FMX.Consts, FMX.Canvas.GPU, FMX.Materials, FMX.PixelFormats;

function GLGetErrorFlags: Integer;
const
  MaxStopLock = 16;
var
  Flag, StopLock: Integer;
begin
  Result := 0;

  StopLock := MaxStopLock;

  repeat
    Flag := glGetError;
    if Flag <> GL_NO_ERROR then
      Result := Result or Flag;

    Dec(StopLock);
  until (Flag = GL_NO_ERROR) or (StopLock <= 0);
end;

procedure RaiseContextExceptionFmt(ResStringRec: PResStringRec; const Args: array of const);
begin
  {$IFDEF ANDROID}
  Log.d('[Context Exception]: ' + Format(LoadResString(ResStringRec), Args));
  {$ENDIF}
  EContext3DException.CreateResFmt(ResStringRec, Args);
end;

{ TExtensions }

constructor TExtensions.Create;
begin
  inherited;

  Extensions := TStringList.Create;
  Extensions.CaseSensitive := False;

  FillExtensions;
end;

destructor TExtensions.Destroy;
begin
  Extensions.Free;

  inherited;
end;

procedure TExtensions.FillExtensions;
const
  ExtensionSeparator: Char = ' ';

  procedure ParseString(ExtList: string);
  var
    ExtName: string;
    SepAt: Integer;
  begin
    repeat
      SepAt := ExtList.IndexOf(ExtensionSeparator);

      if SepAt <> -1 then
      begin
        ExtName := ExtList.Substring(0, SepAt);

        Extensions.Add(ExtName);

        ExtList := ExtList.Remove(0, 1 + SepAt);
      end
      else if ExtList.Length > 0 then
        Extensions.Add(ExtList);
    until (SepAt = -1) or (ExtList.Length < 1);
  end;

var
  ExtList: string;
begin
  try
    // Get normal OpenGL ES extensions.
    ExtList := string(MarshaledAString(glGetString(GL_EXTENSIONS)));
    if GLGetErrorFlags <> GL_NO_ERROR then
      Exit;

    ParseString(ExtList);

{$IFDEF ANDROID}
    // Get special EGL extensions.
    ExtList := string(eglQueryString(eglGetCurrentDisplay, EGL_EXTENSIONS));
    if GLGetErrorFlags <> GL_NO_ERROR then
      Exit;
{$ENDIF}

    ParseString(ExtList);
  finally
    Extensions.Sorted := True;
  end;
end;

function TExtensions.GetExtension(const Name: string): Boolean;
begin
  Result := Extensions.IndexOf(Name) <> -1;
end;

{ TShaderProgram }

constructor TShaderProgram.Create;
begin
  inherited ;
  Variables := TDictionary<string, TContextShaderVariable>.Create;
end;

destructor TShaderProgram.Destroy;
begin
  FreeAndNil(Variables);
  inherited;
end;

{ TCustomContextOpenGL }

class procedure TCustomContextOpenGL.DestroyPrograms;
var
  Rec: TShaderProgram;
begin
  if Assigned(FPrograms) then
  begin
    for Rec in FPrograms do
      glDeleteProgram(Rec.Prog);
    FPrograms.Clear;
    FreeAndNil(FPrograms);
  end;
end;

class procedure TCustomContextOpenGL.FillMetrics;
begin
  glGetIntegerv(GL_MAX_TEXTURE_SIZE, @TCustomContextOpenGL.FMaxTextureSize);
  glGetIntegerv(GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS, @TCustomContextOpenGL.FTextureUnitCount);
end;

class function TCustomContextOpenGL.GLHasAnyErrors: Boolean;
begin
  Result := GLGetErrorFlags <> 0;
end;

class function TCustomContextOpenGL.GetExtensions: TExtensions;
begin
  if not Assigned(FExtensions) then
    FExtensions := TExtensions.Create;

  Result := FExtensions;
end;

class function TCustomContextOpenGL.GetIndexBufferSupport: TIndexBufferSupport;
const
  OES_element_index_uint = 'GL_OES_element_index_uint';
begin
  if FIndexBufferSupport = TIndexBufferSupport.Unknown then
  begin
{$IFDEF ANDROID}
    // AVD crash workaround: although 32-bit index buffer appears to be supported, using it leads to emulator crash.
    FIndexBufferSupport := TIndexBufferSupport.Int16;
{$ELSE}
    if Extensions[OES_element_index_uint] then
      FIndexBufferSupport := TIndexBufferSupport.Int32
    else
      FIndexBufferSupport := TIndexBufferSupport.Int16;
{$ENDIF}
  end;

  Result := FIndexBufferSupport;
end;

constructor TCustomContextOpenGL.CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer;
      const AMultisample: TMultisample; const ADepthStencil: Boolean);
var
  WinSvc: IFMXWindowService;
  Form: TCommonCustomForm;
begin
  inherited;
  FScale := 1.0;
  if TPlatformServices.Current.SupportsPlatformService(IFMXWindowService, IInterface(WinSvc)) then
  begin
    Form := WinSvc.FindForm(AParent);
    if Assigned(Form) then
      FScale := WinSvc.GetWindowScale(Form);
  end;
end;

constructor TCustomContextOpenGL.CreateFromTexture(const ATexture: TTexture; const AMultisample: TMultisample;
      const ADepthStencil: Boolean);
begin
  inherited;
  FScale := ATexture.TextureScale;
  CreateSharedContext;
  if Multisample <> TMultisample.ms2Samples then
    FMSValue := 2
  else
    FMSValue := 4;
  CreateBuffer;
end;

procedure TCustomContextOpenGL.DoResize;
begin
end;

procedure TCustomContextOpenGL.DoFreeBuffer;
begin
  if FFrameBuf <> 0 then
  begin
    if FDepthBufMS <> 0 then
      glDeleteRenderbuffers(1, @FDepthBufMS);
    if FRenderBufMS <> 0 then
       glDeleteRenderbuffers(1, @FRenderBufMS);
    if FFrameBufMS <> 0 then
       glDeleteFramebuffers(1, @FFrameBufMS);
    FDepthBufMS := 0;
    FFrameBufMS := 0;
    FRenderBufMS := 0;
    if FStencilBuf <> 0 then
      glDeleteRenderbuffers(1, @FStencilBuf);
    if FDepthBuf <> 0 then
      glDeleteRenderbuffers(1, @FDepthBuf);
    if FRenderBuf <> 0 then
      glDeleteRenderbuffers(1, @FRenderBuf);
    if FFrameBuf <> 0 then
      glDeleteFramebuffers(1, @FFrameBuf);
    FFrameBuf := 0;
  end;
end;

function TCustomContextOpenGL.DoBeginScene: Boolean;
begin
  Result := False;
  if Valid then
  begin
    glGetIntegerv(GL_VIEWPORT, @FOldViewport[0]);
    if FFrameBuf <> 0 then
    begin
      glGetIntegerv(GL_FRAMEBUFFER_BINDING, @FOldFBO);
      if FFrameBufMS <> 0 then
        glBindFramebuffer(GL_FRAMEBUFFER, FFrameBufMS)
      else
        glBindFramebuffer(GL_FRAMEBUFFER, FFrameBuf);
    end;

    if Assigned(Texture) then
      glViewport(0, Height - Round(Height * FScale), Round(Width * FScale), Round(Height * FScale))
    else
      glViewport(0, 0, Round(Width * FScale), Round(Height * FScale));

    Result := inherited DoBeginScene;

    if (GLHasAnyErrors()) then
      RaiseContextExceptionFmt(@SCannotBeginRenderingScene, [ClassName]);
  end;
end;

procedure TCustomContextOpenGL.DoEndScene;
begin
  if Valid then
  begin
    if FFrameBuf <> 0 then
      glBindFramebuffer(GL_FRAMEBUFFER, FOldFBO);
    glViewport(FOldViewport[0], FOldViewport[1], FOldViewport[2], FOldViewport[3]);
    inherited ;
  end;
end;

class function TCustomContextOpenGL.DoBitmapToTexture(const Bitmap: TBitmap): TTexture;
begin
  if Bitmap.CanvasClass.InheritsFrom(TCustomCanvasGpu) then
    Result := TBitmapCtx(Bitmap.Handle).PaintingTexture
  else
    Result := inherited DoBitmapToTexture(Bitmap);
end;

procedure TCustomContextOpenGL.DoSetContextState(AState: TContextState);
begin
  if IsContextAvailable then
  begin
    case AState of
      TContextState.csZTestOn:
        begin
          glEnable(GL_DEPTH_TEST);
          glDepthFunc(GL_LEQUAL);
        end;
      TContextState.csZTestOff: glDisable(GL_DEPTH_TEST);
      TContextState.csZWriteOn: glDepthMask(1);
      TContextState.csZWriteOff: glDepthMask(0);
      TContextState.csAlphaBlendOn:
        begin
          glEnable(GL_BLEND);
          glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_ALPHA);
        end;
      TContextState.csAlphaBlendOff: glDisable(GL_BLEND);
      TContextState.csStencilOn: glEnable(GL_STENCIL_TEST);
      TContextState.csStencilOff: glDisable(GL_STENCIL_TEST);
      TContextState.csColorWriteOn: glColorMask(1, 1, 1, 1);
      TContextState.csColorWriteOff: glColorMask(0, 0, 0, 0);
      TContextState.csScissorOn:
        glEnable(GL_SCISSOR_TEST);
      TContextState.csScissorOff:
        glDisable(GL_SCISSOR_TEST);
      TContextState.csFrontFace:
        begin
          glFrontFace(GL_CW);
          glCullFace(GL_BACK);
          glEnable(GL_CULL_FACE);
        end;
      TContextState.csBackFace:
        begin
          glFrontFace(GL_CW);
          glCullFace(GL_FRONT);
          glEnable(GL_CULL_FACE);
        end;
      TContextState.csAllFace: glDisable(GL_CULL_FACE);
    end;
    if (GLHasAnyErrors()) then
      RaiseContextExceptionFmt(@SErrorInContextMethod, ['DoSetContextState']);
  end;
end;

procedure TCustomContextOpenGL.DoClear(const ATarget: TClearTargets; const AColor: TAlphaColor; const ADepth: single; const AStencil: Cardinal);
var
  Flags: Integer;
  SaveMask: GLint;
begin
  if Valid then
  begin
    Flags := 0;
    if DepthStencil and (TClearTarget.ctDepth in ATarget) then
    begin
      Flags := Flags or GL_DEPTH_BUFFER_BIT;
      glGetIntegerv(GL_DEPTH_WRITEMASK, @SaveMask);
      glDepthMask(1);
      glClearDepthf(ADepth);
    end;
    if DepthStencil and (TClearTarget.ctStencil in ATarget) then
    begin
      Flags := Flags or GL_STENCIL_BUFFER_BIT;
      glClearStencil(AStencil);
    end;
    if (TClearTarget.ctColor in ATarget) then
    begin
      Flags := Flags or GL_COLOR_BUFFER_BIT;
      glClearColor(TAlphaColorRec(AColor).R / $FF, TAlphaColorRec(AColor).G / $FF, TAlphaColorRec(AColor).B / $FF, TAlphaColorRec(AColor).A / $FF);
    end;

    glClear(Flags);

    if DepthStencil and (TClearTarget.ctDepth in ATarget) then
      glDepthMask(SaveMask);

    if (GLHasAnyErrors()) then
      RaiseContextExceptionFmt(@SErrorInContextMethod, ['DoClear']);
  end;
end;

procedure TCustomContextOpenGL.DoCopyToBitmap(const Dest: TBitmap; const ARect: TRect);
var
  OldTexture, OldFBO: GLuint;
  DestContext: TCustomContextOpenGL;
begin
  if Valid then
  begin
    if (TCanvasStyle.NeedGPUSurface in Dest.CanvasClass.GetCanvasStyle) and Assigned(Texture) then
    begin
      DestContext := TCustomContextOpenGL(TCustomCanvasGpu(Dest.Canvas).Context);
      glGetIntegerv(GL_FRAMEBUFFER_BINDING, @OldFBO);
      glGetIntegerv(GL_TEXTURE_BINDING_2D, @OldTexture);

      glBindTexture(GL_TEXTURE_2D, DestContext.Texture.Handle);
      glBindFramebuffer(GL_FRAMEBUFFER, FFrameBuf);

      if (TTextureStyle.tsRenderTarget in Texture.Style) and (TContextStyle.RenderTargetFlipped in Style) then
        glCopyTexSubImage2D(GL_TEXTURE_2D, 0, ARect.Left, ARect.Top, ARect.Left, Height - ARect.Bottom, ARect.Width, ARect.Height)
      else
        glCopyTexSubImage2D(GL_TEXTURE_2D, 0, ARect.Left, ARect.Top, ARect.Left, ARect.Top, ARect.Width, ARect.Height);

      glBindFramebuffer(GL_FRAMEBUFFER, OldFBO);
      glBindTexture(GL_TEXTURE_2D, OldTexture);

      if (GLHasAnyErrors()) then
        RaiseContextExceptionFmt(@SErrorInContextMethod, ['DoCopyToBitmap']);
    end else begin
      inherited DoCopyToBitmap(Dest, ARect);
    end;
  end;
end;

procedure TCustomContextOpenGL.DoCopyToBits(const Bits: Pointer; const Pitch: Integer; const ARect: TRect);
var
  I: Integer;
  OldFBO: GLuint;
  BitmapBuffer: PAlphaColorArray;
  BitmapBufferLen: Integer;
begin
  if Valid then
  begin
    BitmapBufferLen := Width * Height * 4;
    GetMem(BitmapBuffer, BitmapBufferLen);
    try
      if FFrameBuf <> 0 then
      begin
        glGetIntegerv(GL_FRAMEBUFFER_BINDING, @OldFBO);
        glBindFramebuffer(GL_FRAMEBUFFER, FFrameBuf);
      end;

      glReadPixels(0, 0, Width, Height, GL_RGBA, GL_UNSIGNED_BYTE, BitmapBuffer);
      // Flip
      for I := 0 to Height - 1 do
        Move(PAlphaColorArray(BitmapBuffer)^[I * Width], PLongByteArray(Bits)^[(Height - I - 1) * Pitch], Width * 4);

      if FFrameBuf <> 0 then
        glBindFramebuffer(GL_FRAMEBUFFER, OldFBO);
    finally
      FreeMem(BitmapBuffer);
    end;

    if (GLHasAnyErrors()) then
      RaiseContextExceptionFmt(@SErrorInContextMethod, ['DoCopyBits']);
  end;
end;

procedure TCustomContextOpenGL.DoDrawPrimitives(const AKind: TPrimitivesKind; const Vertices, Indices: Pointer;
  const VertexDeclaration: TVertexDeclaration; const VertexSize, VertexCount, IndexSize, IndexCount: Integer);
var
  Element: TVertexElement;
  Mode: GLEnum;
  TempIndices: array of Word;
  I: Integer;
begin
  if Valid then
  begin
    for Element in VertexDeclaration do
    begin
      case Element.Format of
        TVertexFormat.vfVertex:
          begin
            glVertexAttribPointer(ATTRIB_VERTEX, 3, GL_FLOAT, GL_FALSE, VertexSize, Pointer(NativeInt(Vertices) + Element.Offset));
            glEnableVertexAttribArray(ATTRIB_VERTEX);
          end;
        TVertexFormat.vfNormal:
          begin
            glVertexAttribPointer(ATTRIB_NORMAL, 3, GL_FLOAT, GL_FALSE, VertexSize, Pointer(NativeInt(Vertices) + Element.Offset));
            glEnableVertexAttribArray(ATTRIB_NORMAL);
          end;
        TVertexFormat.vfTexCoord0:
          begin
            glVertexAttribPointer(ATTRIB_TEXCOORD0, 2, GL_FLOAT, GL_FALSE, VertexSize, Pointer(NativeInt(Vertices) + Element.Offset));
            glEnableVertexAttribArray(ATTRIB_TEXCOORD0);
          end;
        TVertexFormat.vfTexCoord1:
          begin
            glVertexAttribPointer(ATTRIB_TEXCOORD1, 2, GL_FLOAT, GL_FALSE, VertexSize, Pointer(NativeInt(Vertices) + Element.Offset));
            glEnableVertexAttribArray(ATTRIB_TEXCOORD1);
          end;
        TVertexFormat.vfTexCoord2:
          begin
            glVertexAttribPointer(ATTRIB_TEXCOORD2, 2, GL_FLOAT, GL_FALSE, VertexSize, Pointer(NativeInt(Vertices) + Element.Offset));
            glEnableVertexAttribArray(ATTRIB_TEXCOORD2);
          end;
        TVertexFormat.vfTexCoord3:
          begin
            glVertexAttribPointer(ATTRIB_TEXCOORD3, 2, GL_FLOAT, GL_FALSE, VertexSize, Pointer(NativeInt(Vertices) + Element.Offset));
            glEnableVertexAttribArray(ATTRIB_TEXCOORD3);
          end;
        TVertexFormat.vfColor0:
          begin
            glVertexAttribPointer(ATTRIB_COLOR0, 4, GL_UNSIGNED_BYTE, GL_TRUE, VertexSize, Pointer(NativeInt(Vertices) + Element.Offset));
            glEnableVertexAttribArray(ATTRIB_COLOR0);
          end;
        TVertexFormat.vfColor1:
          begin
            glVertexAttribPointer(ATTRIB_COLOR0, 4, GL_UNSIGNED_BYTE, GL_TRUE, VertexSize, Pointer(NativeInt(Vertices) + Element.Offset));
            glEnableVertexAttribArray(ATTRIB_COLOR0);
          end;
        TVertexFormat.vfColor2:
          begin
            glVertexAttribPointer(ATTRIB_COLOR0, 4, GL_UNSIGNED_BYTE, GL_TRUE, VertexSize, Pointer(NativeInt(Vertices) + Element.Offset));
            glEnableVertexAttribArray(ATTRIB_COLOR0);
          end;
        TVertexFormat.vfColor3:
          begin
            glVertexAttribPointer(ATTRIB_COLOR0, 4, GL_UNSIGNED_BYTE, GL_TRUE, VertexSize, Pointer(NativeInt(Vertices) + Element.Offset));
            glEnableVertexAttribArray(ATTRIB_COLOR0);
          end;
        TVertexFormat.vfColorF0:
          begin
            glVertexAttribPointer(ATTRIB_COLOR0, 4, GL_FLOAT, GL_FALSE, VertexSize, Pointer(NativeInt(Vertices) + Element.Offset));
            glEnableVertexAttribArray(ATTRIB_COLOR0);
          end;
        TVertexFormat.vfColorF1:
          begin
            glVertexAttribPointer(ATTRIB_COLOR0, 4, GL_FLOAT, GL_FALSE, VertexSize, Pointer(NativeInt(Vertices) + Element.Offset));
            glEnableVertexAttribArray(ATTRIB_COLOR0);
          end;
        TVertexFormat.vfColorF2:
          begin
            glVertexAttribPointer(ATTRIB_COLOR0, 4, GL_FLOAT, GL_FALSE, VertexSize, Pointer(NativeInt(Vertices) + Element.Offset));
            glEnableVertexAttribArray(ATTRIB_COLOR0);
          end;
        TVertexFormat.vfColorF3:
          begin
            glVertexAttribPointer(ATTRIB_COLOR0, 4, GL_FLOAT, GL_FALSE, VertexSize, Pointer(NativeInt(Vertices) + Element.Offset));
            glEnableVertexAttribArray(ATTRIB_COLOR0);
          end;
      end;
    end;

    case AKind of
      TPrimitivesKind.pkPoints: Mode := GL_POINTS;
      TPrimitivesKind.pkLines: Mode := GL_LINES;
    else
      Mode := GL_TRIANGLES;
    end;

    if IndexSize = 4 then
    begin
      if IndexBufferSupport <> TIndexBufferSupport.Int32 then
      begin
        // Workaround for 16-bit index buffer limitation.
        if (VertexCount > High(Word)) or (IndexCount > High(Word)) then
          RaiseContextExceptionFmt(@SErrorInContextMethod, ['DoDrawPrimitive']);

        SetLength(TempIndices, IndexCount);

        for I := 0 to IndexCount - 1 do
          TempIndices[I] := PIntegerArray(Indices)[I];

        glDrawElements(Mode, IndexCount, GL_UNSIGNED_SHORT, @TempIndices[0]);
      end
      else
        glDrawElements(Mode, IndexCount, GL_UNSIGNED_INT, Indices)
    end
    else
      glDrawElements(Mode, IndexCount, GL_UNSIGNED_SHORT, Indices);

    glUseProgram(0);
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, 0);

    glDisableVertexAttribArray(ATTRIB_VERTEX);
    glDisableVertexAttribArray(ATTRIB_NORMAL);
    glDisableVertexAttribArray(ATTRIB_COLOR0);
    glDisableVertexAttribArray(ATTRIB_COLOR1);
    glDisableVertexAttribArray(ATTRIB_COLOR2);
    glDisableVertexAttribArray(ATTRIB_COLOR3);
    glDisableVertexAttribArray(ATTRIB_TEXCOORD0);
    glDisableVertexAttribArray(ATTRIB_TEXCOORD1);
    glDisableVertexAttribArray(ATTRIB_TEXCOORD2);
    glDisableVertexAttribArray(ATTRIB_TEXCOORD3);
    if GLHasAnyErrors then
      RaiseContextExceptionFmt(@SErrorInContextMethod, ['DoDrawPrimitive']);
  end;
end;

procedure TCustomContextOpenGL.DoSetStencilOp(const Fail, ZFail, ZPass: TStencilOp);
var
  gFail, gZFail, gZPass: GLenum;
begin
  if Valid then
  begin
    case Fail of
      TStencilOp.soKeep: gFail := GL_KEEP;
      TStencilOp.soZero: gFail := GL_ZERO;
      TStencilOp.soReplace: gFail := GL_REPLACE;
      TStencilOp.soIncrease: gFail := GL_INCR;
      TStencilOp.soDecrease: gFail := GL_DECR;
      TStencilOp.soInvert: gFail := GL_INVERT;
    else
      gFail := GL_INVERT;
    end;
    case ZFail of
      TStencilOp.soKeep: gZFail := GL_KEEP;
      TStencilOp.soZero: gZFail := GL_ZERO;
      TStencilOp.soReplace: gZFail := GL_REPLACE;
      TStencilOp.soIncrease: gZFail := GL_INCR;
      TStencilOp.soDecrease: gZFail := GL_DECR;
      TStencilOp.soInvert: gZFail := GL_INVERT;
    else
      gZFail := GL_INVERT;
    end;
    case ZPass of
      TStencilOp.soKeep: gZPass := GL_KEEP;
      TStencilOp.soZero: gZPass := GL_ZERO;
      TStencilOp.soReplace: gZPass := GL_REPLACE;
      TStencilOp.soIncrease: gZPass := GL_INCR;
      TStencilOp.soDecrease: gZPass := GL_DECR;
      TStencilOp.soInvert: gZPass := GL_INVERT;
    else
      gZPass := GL_INVERT;
    end;
    glStencilOp(gFail, gZFail, gZPass);
    if (GLHasAnyErrors()) then
      RaiseContextExceptionFmt(@SErrorInContextMethod, ['DoSetStencilOp']);
  end;
end;

procedure TCustomContextOpenGL.DoSetStencilFunc(const Func: TStencilfunc; Ref, Mask: cardinal);
var
  gFunc: GLenum;
begin
  if Valid then
  begin
    case Func of
      TStencilFunc.sfNever: gFunc := GL_NEVER;
      TStencilFunc.sfLess: gFunc := GL_LESS;
      TStencilFunc.sfLequal: gFunc := GL_LEQUAL;
      TStencilFunc.sfGreater: gFunc := GL_GREATER;
      TStencilFunc.sfGequal: gFunc := GL_GEQUAL;
      TStencilFunc.sfEqual: gFunc := GL_EQUAL;
      TStencilFunc.sfNotEqual: gFunc := GL_NOTEQUAL;
      TStencilFunc.sfAlways: gFunc := GL_ALWAYS;
    else
      gFunc := GL_ALWAYS;
    end;
    glStencilFunc(gFunc, Ref, Mask);
    if (GLHasAnyErrors()) then
      RaiseContextExceptionFmt(@SErrorInContextMethod, ['DoSetStencilFunc']);
  end;
end;

{ Textures }

class procedure TCustomContextOpenGL.DoInitializeTexture(const Texture: TTexture);
var
  Tex: THandle;
begin
  CreateSharedContext;
  if IsContextAvailable then
  begin
    glActiveTexture(GL_TEXTURE0);
    glGenTextures(1, @Tex);
    glBindTexture(GL_TEXTURE_2D, Tex);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    case Texture.MagFilter of
      TTextureFilter.tfNearest: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
      TTextureFilter.tfLinear: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    end;
    if TTextureStyle.tsMipMaps in Texture.Style then
    begin
      case Texture.MinFilter of
        TTextureFilter.tfNearest: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST_MIPMAP_NEAREST);
        TTextureFilter.tfLinear: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
      end;
    end
    else
    begin
      case Texture.MinFilter of
        TTextureFilter.tfNearest: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        TTextureFilter.tfLinear: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      end;
    end;
    if TTextureStyle.tsRenderTarget in Texture.Style then
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, Texture.Width, Texture.Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
    glBindTexture(GL_TEXTURE_2D, 0);
    ITextureAccess(Texture).Handle := Tex;
    if (GLHasAnyErrors()) then
      RaiseContextExceptionFmt(@SCannotCreateTexture, [ClassName]);
  end;
end;

class procedure TCustomContextOpenGL.DoFinalizeTexture(const Texture: TTexture);
begin
  if IsContextAvailable then
  begin
    glDeleteTextures(1, @Texture.Handle);
    if GLHasAnyErrors then
      RaiseContextExceptionFmt(@SErrorInContextMethod, ['DoFinalizeTexture']);
  end;
  ITextureAccess(Texture).Handle := 0;
end;

class procedure TCustomContextOpenGL.DoUpdateTexture(const Texture: TTexture; const Bits: Pointer; const Pitch: Integer);
var
  I: Integer;
begin
  if IsContextAvailable then
  begin
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, Texture.Handle);
    if TTextureStyle.tsRenderTarget in Texture.Style then
    begin
      for I := 0 to Texture.Height - 1 do
        glTexSubImage2D(GL_TEXTURE_2D, 0, 0, Texture.Height - 1 - I, Texture.Width, 1, GL_RGBA, GL_UNSIGNED_BYTE, @PLongByteArray(Bits)[I * Pitch]);
    end else
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, Texture.Width, Texture.Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, Bits);
    if TTextureStyle.tsMipMaps in Texture.Style then
      glGenerateMipmap(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, 0);
    if (GLHasAnyErrors()) then
      RaiseContextExceptionFmt(@SCannotUploadTexture, [ClassName]);
  end;
end;

{ shaders }

const
  GLESHeaderHigh: array [0..24] of byte =
    (Byte('p'), Byte('r'), Byte('e'), Byte('c'), Byte('i'), Byte('s'), Byte('i'), Byte('o'), Byte('n'), Byte(' '),
     Byte('h'), Byte('i'), Byte('g'), Byte('h'), Byte('p'), Byte(' '), Byte(' '), Byte(' '), Byte('f'), Byte('l'),
     Byte('o'), Byte('a'), Byte('t'), Byte(';'), Byte(#13));

class function TCustomContextOpenGL.BuildShader(AType: Integer; const ACode: TContextShaderCode): Integer;
var
  log: array of Byte;
  len, compiled: Integer;
  code: array of Byte;
  I: Integer;
begin
  Result := 0;
  CreateSharedContext;
  if IsContextAvailable then
  begin
    if AType = GL_FRAGMENT_SHADER then
    begin
      SetLength(code, Length(GLESHeaderHigh) + Length(ACode));
      for I := 0 to High(GLESHeaderHigh) do
        code[I] := GLESHeaderHigh[I];
      for I := 0 to High(ACode) do
        code[Length(GLESHeaderHigh) + I] := ACode[I];
    end else begin
      SetLength(code, Length(ACode));
      for I := 0 to High(ACode) do
        code[I] := ACode[I];
    end;
    Result := glCreateShader(AType);
    len := Length(code);
    glShaderSource(Result, 1, @code, @len);
    glCompileShader(Result);
    glGetShaderiv(Result, GL_COMPILE_STATUS, @compiled);
    if compiled = 0 then
    begin
      glGetShaderiv(Result, GL_INFO_LOG_LENGTH, @compiled);
      if (compiled > 0) then
      begin
        {$WARNINGS OFF}
        SetLength(log, compiled);
        glGetShaderInfoLog(Result, compiled, @compiled, MarshaledAString(log));
        FMX.Types.Log.d(MarshaledAString(log));
        if AType = GL_VERTEX_SHADER then
          RaiseContextExceptionFmt(@SCannotCreateVertexShader, [ClassName])
        else
          RaiseContextExceptionFmt(@SCannotCreatePixelShader, [ClassName]);
        {$WARNINGS ON}
      end;
    end;
    if (GLHasAnyErrors()) then
      RaiseContextExceptionFmt(@SCannotCreateShader, [ClassName]);
  end
end;

function GetUniformSize(const ShaderProgram: TShaderProgram; const Name: string): Integer;
var
  I: Integer;
  UniformCount: Integer;
  UniformSize, UniformType: Integer;
  UniformName: array of Byte;
begin
  glGetProgramiv(ShaderProgram.Prog, GL_ACTIVE_UNIFORMS, @UniformCount);

  SetLength(UniformName, 100);

  for I := 0 to UniformCount - 1 do
  begin
    glGetActiveUniform(ShaderProgram.Prog, I, 100, nil, @UniformSize, @UniformType, @UniformName[0]);
    if SameText(UTF8ToString(MarshaledAString(UniformName)), Name) then
    begin
      Result := UniformSize;
      Exit;
    end;
  end;
  Result := 0;
end;

class function TCustomContextOpenGL.FindProgram(const VS, PS: TContextShader): TShaderProgram;
var
  ShaderProgram: TShaderProgram;
  compiled: Integer;
  S: TStringBuilder;
  UniformSize, I, J: Integer;
  Source: TContextShaderSource;
  TexUnit: Integer;
  Variable, Variable2: TContextShaderVariable;
begin
  Result := nil;
  if IsContextAvailable and Assigned(VS) and Assigned(PS) and (VS.Handle <> 0) and (PS.Handle <> 0) then
  begin
    if FPrograms <> nil then
      for ShaderProgram in FPrograms do
        if (ShaderProgram.VS = VS.Handle) and (ShaderProgram.PS = PS.Handle) then
          Exit(ShaderProgram);

    ShaderProgram := TShaderProgram.Create;
    ShaderProgram.Prog := glCreateProgram();

    glAttachShader(ShaderProgram.Prog, VS.Handle);
    glAttachShader(ShaderProgram.Prog, PS.Handle);

    glBindAttribLocation(ShaderProgram.Prog, ATTRIB_VERTEX, 'a_Position');
    glBindAttribLocation(ShaderProgram.Prog, ATTRIB_NORMAL, 'a_Normal');
    glBindAttribLocation(ShaderProgram.Prog, ATTRIB_TEXCOORD0, 'a_TexCoord0');
    glBindAttribLocation(ShaderProgram.Prog, ATTRIB_TEXCOORD1, 'a_TexCoord1');
    glBindAttribLocation(ShaderProgram.Prog, ATTRIB_TEXCOORD2, 'a_TexCoord2');
    glBindAttribLocation(ShaderProgram.Prog, ATTRIB_TEXCOORD3, 'a_TexCoord3');
    glBindAttribLocation(ShaderProgram.Prog, ATTRIB_COLOR0, 'a_Color');
    glBindAttribLocation(ShaderProgram.Prog, ATTRIB_COLOR1, 'a_Color1');
    glBindAttribLocation(ShaderProgram.Prog, ATTRIB_COLOR2, 'a_Color2');
    glBindAttribLocation(ShaderProgram.Prog, ATTRIB_COLOR3, 'a_Color3');

    glLinkProgram(ShaderProgram.Prog);
    glGetProgramiv(ShaderProgram.Prog, GL_LINK_STATUS, @compiled);
    if compiled = 0 then
    begin
      glDeleteProgram(ShaderProgram.Prog);
      RaiseContextExceptionFmt(@SCannotCreateShader, [ClassName]);
    end;

    S := TStringBuilder.Create;
    try
      Source := VS.GetSourceByArch(GetShaderArch);
      if Source.Arch = TContextShaderArch.saUndefined then
        Source := VS.GetSourceByArch(TContextShaderArch.saGLSL);
      if Source.IsDefined then
      begin
        // set vs
        for I := 0 to High(Source.Variables) do
        begin
          S.Clear;
          S.Append('_').Append(Source.Variables[I].Name);
          if Pos('.', S.ToString) > 0 then
            S.Insert(Pos('.', S.ToString), '_');
          Variable := Source.Variables[I];
          Variable.ShaderKind := TContextShaderKind.skVertexShader;
          Variable.Index := glGetUniformLocation(ShaderProgram.Prog, MarshaledAString(TMarshal.AsAnsi(S.ToString)));
          if Variable.Kind = TContextShaderVariableKind.vkMatrix then
          begin
            S.Append('[0]');
            UniformSize := GetUniformSize(ShaderProgram, S.ToString);
            if (UniformSize > 0) and (Variable.Size <> UniformSize) then
              Variable.Size := UniformSize;
          end;
          ShaderProgram.Variables.Add(Source.Variables[I].Name, Variable);
        end;
        // check locations - correct size if compiler decrease it
        if ShaderProgram.Variables.Count > 2 then
          for I := 0 to ShaderProgram.Variables.Count - 1 do
            for J := 0 to ShaderProgram.Variables.Count - 1 do
              if (I <> J) then
              begin
                Variable := ShaderProgram.Variables.ToArray[I].Value;
                Variable2 := ShaderProgram.Variables.ToArray[J].Value;
                if (Variable.ShaderKind = Variable2.ShaderKind) and
                   (Variable2.Index > Variable.Index) and (Variable.Index + Variable.Size > Variable2.Index) then
                begin
                  Variable.Size := Variable2.Index - Variable.Index;
                  ShaderProgram.Variables.AddOrSetValue(Variable.Name, Variable);
                end;
              end;
      end;
      // set ps
      Source := PS.GetSourceByArch(GetShaderArch);
      if Source.Arch = TContextShaderArch.saUndefined then
        Source := PS.GetSourceByArch(TContextShaderArch.saGLSL);
      if Source.IsDefined then
      begin
        TexUnit := 0;
        for I := 0 to High(Source.Variables) do
        begin
          S.Clear;
          S.Append('_').Append(Source.Variables[I].Name);
          if Pos('.', S.ToString) > 0 then
            S.Insert(Pos('.', S.ToString), '_');

          Variable := Source.Variables[I];
          Variable.ShaderKind := TContextShaderKind.skPixelShader;
          Variable.Index := glGetUniformLocation(ShaderProgram.Prog, MarshaledAString(TMarshal.AsAnsi(S.ToString)));
          if (Variable.Index >= 0) and (Variable.Kind = TContextShaderVariableKind.vkTexture) then
          begin
            Variable.TextureUnit := TexUnit;
            TexUnit := TexUnit + 1;
          end;
          ShaderProgram.Variables.Add(Source.Variables[I].Name, Variable);
        end;
        // check locations - correct size if compiler decrease it
        if ShaderProgram.Variables.Count > 2 then
          for I := 0 to ShaderProgram.Variables.Count - 1 do
            for J := 0 to ShaderProgram.Variables.Count - 1 do
              if (I <> J) then
              begin
                Variable := ShaderProgram.Variables.ToArray[I].Value;
                Variable2 := ShaderProgram.Variables.ToArray[J].Value;
                if (Variable.ShaderKind = Variable2.ShaderKind) and
                   (Variable2.Index > Variable.Index) and (Variable.Index + Variable.Size > Variable2.Index) then
                begin
                  Variable.Size := Variable2.Index - Variable.Index;
                  ShaderProgram.Variables.AddOrSetValue(Variable.Name, Variable);
                end;
              end;
      end;
    finally
      S.DisposeOf;
    end;

    if FPrograms = nil then
      FPrograms := TList<TShaderProgram>.Create;

    ShaderProgram.VS := VS.Handle;
    ShaderProgram.PS := PS.Handle;
    FPrograms.Add(ShaderProgram);
    if (GLHasAnyErrors()) then
      RaiseContextExceptionFmt(@SCannotActivateShaderProgram, [ClassName]);

    Result := ShaderProgram;
  end;
end;

class procedure TCustomContextOpenGL.UseProgram(const VS, PS: TContextShader);
var
  Prog: TShaderProgram;
begin
  if IsContextAvailable and (VS <> nil) and (PS <> nil) and (VS.Handle <> 0) and (PS.Handle <> 0) then
  begin
    Prog := FindProgram(VS, PS);
    FCurrentProgram := Prog;
    glUseProgram(FCurrentProgram.Prog);
    if (GLHasAnyErrors()) then
      RaiseContextExceptionFmt(@SCannotActivateShaderProgram, [ClassName]);
  end;
end;

class procedure TCustomContextOpenGL.DoInitializeShader(const Shader: TContextShader);
var
  Source: TContextShaderSource;
begin
  CreateSharedContext;
  if IsContextAvailable then
  begin
    if Shader.Kind = TContextShaderKind.skVertexShader then
    begin
      Source := Shader.GetSourceByArch(GetShaderArch);
      if Source.Arch = TContextShaderArch.saUndefined then
        Source := Shader.GetSourceByArch(TContextShaderArch.saGLSL);
      if Source.IsDefined then
        Shader.Handle := BuildShader(GL_VERTEX_SHADER, Source.Code)
    end else begin
      Source := Shader.GetSourceByArch(GetShaderArch);
      if Source.Arch = TContextShaderArch.saUndefined then
        Source := Shader.GetSourceByArch(TContextShaderArch.saGLSL);
      if Source.IsDefined then
        Shader.Handle := BuildShader(GL_FRAGMENT_SHADER, Source.Code)
    end;
  end;
end;

class procedure TCustomContextOpenGL.DoFinalizeShader(const Shader: TContextShader);
begin
  if IsContextAvailable then
  begin
    glDeleteShader(Shader.Handle);
    if GLHasAnyErrors then
      RaiseContextExceptionFmt(@SErrorInContextMethod, ['DoFinalizeShader']);
  end;
  Shader.Handle := 0;
end;

procedure TCustomContextOpenGL.DoSetShaders(const VertexShader, PixelShader: TContextShader);
begin
  if IsContextAvailable then
    UseProgram(VertexShader, PixelShader);
end;

procedure TCustomContextOpenGL.DoSetShaderVariable(const Name: string; const Data: array of TVector3D);
var
  J: Integer;
  Variable: TContextShaderVariable;
begin
  if IsContextAvailable then
  begin
    if Assigned(FCurrentProgram) then
    begin
      if FCurrentProgram.Variables.TryGetValue(Name, Variable) then
      begin
        case Variable.Kind of
          TContextShaderVariableKind.vkFloat:
             glUniform1f(Variable.Index, Data[0].X);
          TContextShaderVariableKind.vkFloat2:
             glUniform2f(Variable.Index, Data[0].X, Data[0].Y);
          TContextShaderVariableKind.vkFloat3:
             glUniform3f(Variable.Index, Data[0].X, Data[0].Y, Data[0].Z);
        else
          for J := 0 to Min(High(Data), Variable.Size - 1) do
            glUniform4f(Variable.Index + J, Data[J].X, Data[J].Y, Data[J].Z, Data[J].W);
        end;
      end;
    end;
    if GLHasAnyErrors then
      RaiseContextExceptionFmt(@SCannotFindShaderVariable, [Name]);
  end;
end;

procedure TCustomContextOpenGL.DoSetShaderVariable(const Name: string; const Texture: TTexture);
var
  Variable: TContextShaderVariable;
begin
  if IsContextAvailable then
  begin
    if Assigned(FCurrentProgram) then
    begin
      if FCurrentProgram.Variables.TryGetValue(Name, Variable) then
      begin
        glActiveTexture(GL_TEXTURE0 + Variable.TextureUnit);
        if Texture = nil then
          glBindTexture(GL_TEXTURE_2D, 0)
        else
          glBindTexture(GL_TEXTURE_2D, Texture.Handle);
        glUniform1i(Variable.Index, Variable.TextureUnit);
        glActiveTexture(GL_TEXTURE0);
      end;
    end;
    if GLHasAnyErrors then
      RaiseContextExceptionFmt(@SCannotFindShaderVariable, [Name]);
  end;
end;

procedure TCustomContextOpenGL.DoSetShaderVariable(const Name: string; const Matrix: TMatrix3D);
var
  Temp: TMatrix3D;
begin
  Temp := Matrix.Transpose;
  DoSetShaderVariable(Name, Temp.M);
end;

initialization

finalization
  if Assigned(TCustomContextOpenGL.FExtensions) then
    FreeAndNil(TCustomContextOpenGL.FExtensions);

end.