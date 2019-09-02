{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit Androidapi.JNI.OpenGL;

interface

uses 
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Util,
  Androidapi.JNI.GraphicsContentViewText;

type

  {Class forward declarations}
  JETC1 = interface;//android.opengl.ETC1
  JGLSurfaceView = interface;//android.opengl.GLSurfaceView
  JGLES10 = interface;//android.opengl.GLES10
  JEGL14 = interface;//android.opengl.EGL14
  JVisibility = interface;//android.opengl.Visibility
  Jopengl_Matrix = interface;//android.opengl.Matrix
  JEGLObjectHandle = interface;//android.opengl.EGLObjectHandle
  Jopengl_EGLSurface = interface;//android.opengl.EGLSurface
  Jopengl_EGLContext = interface;//android.opengl.EGLContext
  Jopengl_EGLDisplay = interface;//android.opengl.EGLDisplay
  JGLDebugHelper = interface;//android.opengl.GLDebugHelper
  Jopengl_EGLConfig = interface;//android.opengl.EGLConfig
  JGLSurfaceView_EGLContextFactory = interface;//android.opengl.GLSurfaceView$EGLContextFactory
  JGLSurfaceView_Renderer = interface;//android.opengl.GLSurfaceView$Renderer
  JGLES11Ext = interface;//android.opengl.GLES11Ext
  JGLSurfaceView_EGLConfigChooser = interface;//android.opengl.GLSurfaceView$EGLConfigChooser
  JETC1Util_ETC1Texture = interface;//android.opengl.ETC1Util$ETC1Texture
  JGLException = interface;//android.opengl.GLException
  JGLES20 = interface;//android.opengl.GLES20
  JGLUtils = interface;//android.opengl.GLUtils
  JGLES10Ext = interface;//android.opengl.GLES10Ext
  JGLSurfaceView_EGLWindowSurfaceFactory = interface;//android.opengl.GLSurfaceView$EGLWindowSurfaceFactory
  JGLSurfaceView_GLWrapper = interface;//android.opengl.GLSurfaceView$GLWrapper
  JGLES11 = interface;//android.opengl.GLES11
  JETC1Util = interface;//android.opengl.ETC1Util
  JGLU = interface;//android.opengl.GLU

JETC1Class = interface(JObjectClass)
['{54BA31A1-A2A8-448C-96F9-9D95D460D0E9}']
  {Property Methods}
  function _GetDECODED_BLOCK_SIZE: Integer;
  function _GetENCODED_BLOCK_SIZE: Integer;
  function _GetETC1_RGB8_OES: Integer;
  function _GetETC_PKM_HEADER_SIZE: Integer;
  {Methods}
  function init: JETC1; cdecl;
  function getEncodedDataSize(width: Integer; height: Integer): Integer; cdecl;
  {Properties}
  property DECODED_BLOCK_SIZE: Integer read _GetDECODED_BLOCK_SIZE;
  property ENCODED_BLOCK_SIZE: Integer read _GetENCODED_BLOCK_SIZE;
  property ETC1_RGB8_OES: Integer read _GetETC1_RGB8_OES;
  property ETC_PKM_HEADER_SIZE: Integer read _GetETC_PKM_HEADER_SIZE;
end;

[JavaSignature('android/opengl/ETC1')]
JETC1 = interface(JObject)
['{412471CE-BF49-40AF-B106-E3DBB54F5811}']
end;
TJETC1 = class(TJavaGenericImport<JETC1Class, JETC1>) end;

JGLSurfaceViewClass = interface(JSurfaceViewClass)
['{63463E9C-6F3C-4267-AB08-75F38C2165EC}']
  {Property Methods}
  function _GetDEBUG_CHECK_GL_ERROR: Integer;
  function _GetDEBUG_LOG_GL_CALLS: Integer;
  function _GetRENDERMODE_CONTINUOUSLY: Integer;
  function _GetRENDERMODE_WHEN_DIRTY: Integer;
  {Methods}
  function init(context: JContext): JGLSurfaceView; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet): JGLSurfaceView; cdecl; overload;
  {Properties}
  property DEBUG_CHECK_GL_ERROR: Integer read _GetDEBUG_CHECK_GL_ERROR;
  property DEBUG_LOG_GL_CALLS: Integer read _GetDEBUG_LOG_GL_CALLS;
  property RENDERMODE_CONTINUOUSLY: Integer read _GetRENDERMODE_CONTINUOUSLY;
  property RENDERMODE_WHEN_DIRTY: Integer read _GetRENDERMODE_WHEN_DIRTY;
end;

[JavaSignature('android/opengl/GLSurfaceView')]
JGLSurfaceView = interface(JSurfaceView)
['{89634C91-C25A-47F1-9F63-1B86349BD676}']
  {Methods}
  function getDebugFlags: Integer; cdecl;
  function getPreserveEGLContextOnPause: Boolean; cdecl;
  function getRenderMode: Integer; cdecl;
  procedure onPause; cdecl;
  procedure onResume; cdecl;
  procedure queueEvent(r: JRunnable); cdecl;
  procedure requestRender; cdecl;
  procedure setDebugFlags(debugFlags: Integer); cdecl;
  procedure setEGLConfigChooser(configChooser: JGLSurfaceView_EGLConfigChooser); cdecl; overload;
  procedure setEGLConfigChooser(needDepth: Boolean); cdecl; overload;
  procedure setEGLConfigChooser(redSize: Integer; greenSize: Integer; blueSize: Integer; alphaSize: Integer; depthSize: Integer; stencilSize: Integer); cdecl; overload;
  procedure setEGLContextClientVersion(version: Integer); cdecl;
  procedure setEGLContextFactory(factory: JGLSurfaceView_EGLContextFactory); cdecl;
  procedure setEGLWindowSurfaceFactory(factory: JGLSurfaceView_EGLWindowSurfaceFactory); cdecl;
  procedure setGLWrapper(glWrapper: JGLSurfaceView_GLWrapper); cdecl;
  procedure setPreserveEGLContextOnPause(preserveOnPause: Boolean); cdecl;
  procedure setRenderMode(renderMode: Integer); cdecl;
  procedure setRenderer(renderer: JGLSurfaceView_Renderer); cdecl;
  procedure surfaceChanged(holder: JSurfaceHolder; format: Integer; w: Integer; h: Integer); cdecl;
  procedure surfaceCreated(holder: JSurfaceHolder); cdecl;
  procedure surfaceDestroyed(holder: JSurfaceHolder); cdecl;
end;
TJGLSurfaceView = class(TJavaGenericImport<JGLSurfaceViewClass, JGLSurfaceView>) end;

JGLES10Class = interface(JObjectClass)
['{175246E8-7414-4AA7-932F-878CC6D920EE}']
  {Property Methods}
  function _GetGL_ADD: Integer;
  function _GetGL_ALIASED_LINE_WIDTH_RANGE: Integer;
  function _GetGL_ALIASED_POINT_SIZE_RANGE: Integer;
  function _GetGL_ALPHA: Integer;
  function _GetGL_ALPHA_BITS: Integer;
  function _GetGL_ALPHA_TEST: Integer;
  function _GetGL_ALWAYS: Integer;
  function _GetGL_AMBIENT: Integer;
  function _GetGL_AMBIENT_AND_DIFFUSE: Integer;
  function _GetGL_AND: Integer;
  function _GetGL_AND_INVERTED: Integer;
  function _GetGL_AND_REVERSE: Integer;
  function _GetGL_BACK: Integer;
  function _GetGL_BLEND: Integer;
  function _GetGL_BLUE_BITS: Integer;
  function _GetGL_BYTE: Integer;
  function _GetGL_CCW: Integer;
  function _GetGL_CLAMP_TO_EDGE: Integer;
  function _GetGL_CLEAR: Integer;
  function _GetGL_COLOR_ARRAY: Integer;
  function _GetGL_COLOR_BUFFER_BIT: Integer;
  function _GetGL_COLOR_LOGIC_OP: Integer;
  function _GetGL_COLOR_MATERIAL: Integer;
  function _GetGL_COMPRESSED_TEXTURE_FORMATS: Integer;
  function _GetGL_CONSTANT_ATTENUATION: Integer;
  function _GetGL_COPY: Integer;
  function _GetGL_COPY_INVERTED: Integer;
  function _GetGL_CULL_FACE: Integer;
  function _GetGL_CW: Integer;
  function _GetGL_DECAL: Integer;
  function _GetGL_DECR: Integer;
  function _GetGL_DEPTH_BITS: Integer;
  function _GetGL_DEPTH_BUFFER_BIT: Integer;
  function _GetGL_DEPTH_TEST: Integer;
  function _GetGL_DIFFUSE: Integer;
  function _GetGL_DITHER: Integer;
  function _GetGL_DONT_CARE: Integer;
  function _GetGL_DST_ALPHA: Integer;
  function _GetGL_DST_COLOR: Integer;
  function _GetGL_EMISSION: Integer;
  function _GetGL_EQUAL: Integer;
  function _GetGL_EQUIV: Integer;
  function _GetGL_EXP: Integer;
  function _GetGL_EXP2: Integer;
  function _GetGL_EXTENSIONS: Integer;
  function _GetGL_FALSE: Integer;
  function _GetGL_FASTEST: Integer;
  function _GetGL_FIXED: Integer;
  function _GetGL_FLAT: Integer;
  function _GetGL_FLOAT: Integer;
  function _GetGL_FOG: Integer;
  function _GetGL_FOG_COLOR: Integer;
  function _GetGL_FOG_DENSITY: Integer;
  function _GetGL_FOG_END: Integer;
  function _GetGL_FOG_HINT: Integer;
  function _GetGL_FOG_MODE: Integer;
  function _GetGL_FOG_START: Integer;
  function _GetGL_FRONT: Integer;
  function _GetGL_FRONT_AND_BACK: Integer;
  function _GetGL_GEQUAL: Integer;
  function _GetGL_GREATER: Integer;
  function _GetGL_GREEN_BITS: Integer;
  function _GetGL_IMPLEMENTATION_COLOR_READ_FORMAT_OES: Integer;
  function _GetGL_IMPLEMENTATION_COLOR_READ_TYPE_OES: Integer;
  function _GetGL_INCR: Integer;
  function _GetGL_INVALID_ENUM: Integer;
  function _GetGL_INVALID_OPERATION: Integer;
  function _GetGL_INVALID_VALUE: Integer;
  function _GetGL_INVERT: Integer;
  function _GetGL_KEEP: Integer;
  function _GetGL_LEQUAL: Integer;
  function _GetGL_LESS: Integer;
  function _GetGL_LIGHT0: Integer;
  function _GetGL_LIGHT1: Integer;
  function _GetGL_LIGHT2: Integer;
  function _GetGL_LIGHT3: Integer;
  function _GetGL_LIGHT4: Integer;
  function _GetGL_LIGHT5: Integer;
  function _GetGL_LIGHT6: Integer;
  function _GetGL_LIGHT7: Integer;
  function _GetGL_LIGHTING: Integer;
  function _GetGL_LIGHT_MODEL_AMBIENT: Integer;
  function _GetGL_LIGHT_MODEL_TWO_SIDE: Integer;
  function _GetGL_LINEAR: Integer;
  function _GetGL_LINEAR_ATTENUATION: Integer;
  function _GetGL_LINEAR_MIPMAP_LINEAR: Integer;
  function _GetGL_LINEAR_MIPMAP_NEAREST: Integer;
  function _GetGL_LINES: Integer;
  function _GetGL_LINE_LOOP: Integer;
  function _GetGL_LINE_SMOOTH: Integer;
  function _GetGL_LINE_SMOOTH_HINT: Integer;
  function _GetGL_LINE_STRIP: Integer;
  function _GetGL_LUMINANCE: Integer;
  function _GetGL_LUMINANCE_ALPHA: Integer;
  function _GetGL_MAX_ELEMENTS_INDICES: Integer;
  function _GetGL_MAX_ELEMENTS_VERTICES: Integer;
  function _GetGL_MAX_LIGHTS: Integer;
  function _GetGL_MAX_MODELVIEW_STACK_DEPTH: Integer;
  function _GetGL_MAX_PROJECTION_STACK_DEPTH: Integer;
  function _GetGL_MAX_TEXTURE_SIZE: Integer;
  function _GetGL_MAX_TEXTURE_STACK_DEPTH: Integer;
  function _GetGL_MAX_TEXTURE_UNITS: Integer;
  function _GetGL_MAX_VIEWPORT_DIMS: Integer;
  function _GetGL_MODELVIEW: Integer;
  function _GetGL_MODULATE: Integer;
  function _GetGL_MULTISAMPLE: Integer;
  function _GetGL_NAND: Integer;
  function _GetGL_NEAREST: Integer;
  function _GetGL_NEAREST_MIPMAP_LINEAR: Integer;
  function _GetGL_NEAREST_MIPMAP_NEAREST: Integer;
  function _GetGL_NEVER: Integer;
  function _GetGL_NICEST: Integer;
  function _GetGL_NOOP: Integer;
  function _GetGL_NOR: Integer;
  function _GetGL_NORMALIZE: Integer;
  function _GetGL_NORMAL_ARRAY: Integer;
  function _GetGL_NOTEQUAL: Integer;
  function _GetGL_NO_ERROR: Integer;
  function _GetGL_NUM_COMPRESSED_TEXTURE_FORMATS: Integer;
  function _GetGL_ONE: Integer;
  function _GetGL_ONE_MINUS_DST_ALPHA: Integer;
  function _GetGL_ONE_MINUS_DST_COLOR: Integer;
  function _GetGL_ONE_MINUS_SRC_ALPHA: Integer;
  function _GetGL_ONE_MINUS_SRC_COLOR: Integer;
  function _GetGL_OR: Integer;
  function _GetGL_OR_INVERTED: Integer;
  function _GetGL_OR_REVERSE: Integer;
  function _GetGL_OUT_OF_MEMORY: Integer;
  function _GetGL_PACK_ALIGNMENT: Integer;
  function _GetGL_PALETTE4_R5_G6_B5_OES: Integer;
  function _GetGL_PALETTE4_RGB5_A1_OES: Integer;
  function _GetGL_PALETTE4_RGB8_OES: Integer;
  function _GetGL_PALETTE4_RGBA4_OES: Integer;
  function _GetGL_PALETTE4_RGBA8_OES: Integer;
  function _GetGL_PALETTE8_R5_G6_B5_OES: Integer;
  function _GetGL_PALETTE8_RGB5_A1_OES: Integer;
  function _GetGL_PALETTE8_RGB8_OES: Integer;
  function _GetGL_PALETTE8_RGBA4_OES: Integer;
  function _GetGL_PALETTE8_RGBA8_OES: Integer;
  function _GetGL_PERSPECTIVE_CORRECTION_HINT: Integer;
  function _GetGL_POINTS: Integer;
  function _GetGL_POINT_FADE_THRESHOLD_SIZE: Integer;
  function _GetGL_POINT_SIZE: Integer;
  function _GetGL_POINT_SMOOTH: Integer;
  function _GetGL_POINT_SMOOTH_HINT: Integer;
  function _GetGL_POLYGON_OFFSET_FILL: Integer;
  function _GetGL_POLYGON_SMOOTH_HINT: Integer;
  function _GetGL_POSITION: Integer;
  function _GetGL_PROJECTION: Integer;
  function _GetGL_QUADRATIC_ATTENUATION: Integer;
  function _GetGL_RED_BITS: Integer;
  function _GetGL_RENDERER: Integer;
  function _GetGL_REPEAT: Integer;
  function _GetGL_REPLACE: Integer;
  function _GetGL_RESCALE_NORMAL: Integer;
  function _GetGL_RGB: Integer;
  function _GetGL_RGBA: Integer;
  function _GetGL_SAMPLE_ALPHA_TO_COVERAGE: Integer;
  function _GetGL_SAMPLE_ALPHA_TO_ONE: Integer;
  function _GetGL_SAMPLE_COVERAGE: Integer;
  function _GetGL_SCISSOR_TEST: Integer;
  function _GetGL_SET: Integer;
  function _GetGL_SHININESS: Integer;
  function _GetGL_SHORT: Integer;
  function _GetGL_SMOOTH: Integer;
  function _GetGL_SMOOTH_LINE_WIDTH_RANGE: Integer;
  function _GetGL_SMOOTH_POINT_SIZE_RANGE: Integer;
  function _GetGL_SPECULAR: Integer;
  function _GetGL_SPOT_CUTOFF: Integer;
  function _GetGL_SPOT_DIRECTION: Integer;
  function _GetGL_SPOT_EXPONENT: Integer;
  function _GetGL_SRC_ALPHA: Integer;
  function _GetGL_SRC_ALPHA_SATURATE: Integer;
  function _GetGL_SRC_COLOR: Integer;
  function _GetGL_STACK_OVERFLOW: Integer;
  function _GetGL_STACK_UNDERFLOW: Integer;
  function _GetGL_STENCIL_BITS: Integer;
  function _GetGL_STENCIL_BUFFER_BIT: Integer;
  function _GetGL_STENCIL_TEST: Integer;
  function _GetGL_SUBPIXEL_BITS: Integer;
  function _GetGL_TEXTURE: Integer;
  function _GetGL_TEXTURE0: Integer;
  function _GetGL_TEXTURE1: Integer;
  function _GetGL_TEXTURE10: Integer;
  function _GetGL_TEXTURE11: Integer;
  function _GetGL_TEXTURE12: Integer;
  function _GetGL_TEXTURE13: Integer;
  function _GetGL_TEXTURE14: Integer;
  function _GetGL_TEXTURE15: Integer;
  function _GetGL_TEXTURE16: Integer;
  function _GetGL_TEXTURE17: Integer;
  function _GetGL_TEXTURE18: Integer;
  function _GetGL_TEXTURE19: Integer;
  function _GetGL_TEXTURE2: Integer;
  function _GetGL_TEXTURE20: Integer;
  function _GetGL_TEXTURE21: Integer;
  function _GetGL_TEXTURE22: Integer;
  function _GetGL_TEXTURE23: Integer;
  function _GetGL_TEXTURE24: Integer;
  function _GetGL_TEXTURE25: Integer;
  function _GetGL_TEXTURE26: Integer;
  function _GetGL_TEXTURE27: Integer;
  function _GetGL_TEXTURE28: Integer;
  function _GetGL_TEXTURE29: Integer;
  function _GetGL_TEXTURE3: Integer;
  function _GetGL_TEXTURE30: Integer;
  function _GetGL_TEXTURE31: Integer;
  function _GetGL_TEXTURE4: Integer;
  function _GetGL_TEXTURE5: Integer;
  function _GetGL_TEXTURE6: Integer;
  function _GetGL_TEXTURE7: Integer;
  function _GetGL_TEXTURE8: Integer;
  function _GetGL_TEXTURE9: Integer;
  function _GetGL_TEXTURE_2D: Integer;
  function _GetGL_TEXTURE_COORD_ARRAY: Integer;
  function _GetGL_TEXTURE_ENV: Integer;
  function _GetGL_TEXTURE_ENV_COLOR: Integer;
  function _GetGL_TEXTURE_ENV_MODE: Integer;
  function _GetGL_TEXTURE_MAG_FILTER: Integer;
  function _GetGL_TEXTURE_MIN_FILTER: Integer;
  function _GetGL_TEXTURE_WRAP_S: Integer;
  function _GetGL_TEXTURE_WRAP_T: Integer;
  function _GetGL_TRIANGLES: Integer;
  function _GetGL_TRIANGLE_FAN: Integer;
  function _GetGL_TRIANGLE_STRIP: Integer;
  function _GetGL_TRUE: Integer;
  function _GetGL_UNPACK_ALIGNMENT: Integer;
  function _GetGL_UNSIGNED_BYTE: Integer;
  function _GetGL_UNSIGNED_SHORT: Integer;
  function _GetGL_UNSIGNED_SHORT_4_4_4_4: Integer;
  function _GetGL_UNSIGNED_SHORT_5_5_5_1: Integer;
  function _GetGL_UNSIGNED_SHORT_5_6_5: Integer;
  function _GetGL_VENDOR: Integer;
  function _GetGL_VERSION: Integer;
  function _GetGL_VERTEX_ARRAY: Integer;
  function _GetGL_XOR: Integer;
  function _GetGL_ZERO: Integer;
  {Methods}
  function init: JGLES10; cdecl;
  procedure glActiveTexture(texture: Integer); cdecl;
  procedure glAlphaFunc(func: Integer; ref: Single); cdecl;
  procedure glAlphaFuncx(func: Integer; ref: Integer); cdecl;
  procedure glBindTexture(target: Integer; texture: Integer); cdecl;
  procedure glBlendFunc(sfactor: Integer; dfactor: Integer); cdecl;
  procedure glClear(mask: Integer); cdecl;
  procedure glClearColor(red: Single; green: Single; blue: Single; alpha: Single); cdecl;
  procedure glClearColorx(red: Integer; green: Integer; blue: Integer; alpha: Integer); cdecl;
  procedure glClearDepthf(depth: Single); cdecl;
  procedure glClearDepthx(depth: Integer); cdecl;
  procedure glClearStencil(s: Integer); cdecl;
  procedure glClientActiveTexture(texture: Integer); cdecl;
  procedure glColor4f(red: Single; green: Single; blue: Single; alpha: Single); cdecl;
  procedure glColor4x(red: Integer; green: Integer; blue: Integer; alpha: Integer); cdecl;
  procedure glColorMask(red: Boolean; green: Boolean; blue: Boolean; alpha: Boolean); cdecl;
  procedure glCopyTexImage2D(target: Integer; level: Integer; internalformat: Integer; x: Integer; y: Integer; width: Integer; height: Integer; border: Integer); cdecl;
  procedure glCopyTexSubImage2D(target: Integer; level: Integer; xoffset: Integer; yoffset: Integer; x: Integer; y: Integer; width: Integer; height: Integer); cdecl;
  procedure glCullFace(mode: Integer); cdecl;
  procedure glDeleteTextures(n: Integer; textures: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glDepthFunc(func: Integer); cdecl;
  procedure glDepthMask(flag: Boolean); cdecl;
  procedure glDepthRangef(zNear: Single; zFar: Single); cdecl;
  procedure glDepthRangex(zNear: Integer; zFar: Integer); cdecl;
  procedure glDisable(cap: Integer); cdecl;
  procedure glDisableClientState(array_: Integer); cdecl;
  procedure glDrawArrays(mode: Integer; first: Integer; count: Integer); cdecl;
  procedure glEnable(cap: Integer); cdecl;
  procedure glEnableClientState(array_: Integer); cdecl;
  procedure glFinish; cdecl;
  procedure glFlush; cdecl;
  procedure glFogf(pname: Integer; param: Single); cdecl;
  procedure glFogfv(pname: Integer; params: TJavaArray<Single>; offset: Integer); cdecl;
  procedure glFogx(pname: Integer; param: Integer); cdecl;
  procedure glFogxv(pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glFrontFace(mode: Integer); cdecl;
  procedure glFrustumf(left: Single; right: Single; bottom: Single; top: Single; zNear: Single; zFar: Single); cdecl;
  procedure glFrustumx(left: Integer; right: Integer; bottom: Integer; top: Integer; zNear: Integer; zFar: Integer); cdecl;
  procedure glGenTextures(n: Integer; textures: TJavaArray<Integer>; offset: Integer); cdecl;
  function glGetError: Integer; cdecl;
  procedure glGetIntegerv(pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  function glGetString(name: Integer): JString; cdecl;
  procedure glHint(target: Integer; mode: Integer); cdecl;
  procedure glLightModelf(pname: Integer; param: Single); cdecl;
  procedure glLightModelfv(pname: Integer; params: TJavaArray<Single>; offset: Integer); cdecl;
  procedure glLightModelx(pname: Integer; param: Integer); cdecl;
  procedure glLightModelxv(pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glLightf(light: Integer; pname: Integer; param: Single); cdecl;
  procedure glLightfv(light: Integer; pname: Integer; params: TJavaArray<Single>; offset: Integer); cdecl;
  procedure glLightx(light: Integer; pname: Integer; param: Integer); cdecl;
  procedure glLightxv(light: Integer; pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glLineWidth(width: Single); cdecl;
  procedure glLineWidthx(width: Integer); cdecl;
  procedure glLoadIdentity; cdecl;
  procedure glLoadMatrixf(m: TJavaArray<Single>; offset: Integer); cdecl;
  procedure glLoadMatrixx(m: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glLogicOp(opcode: Integer); cdecl;
  procedure glMaterialf(face: Integer; pname: Integer; param: Single); cdecl;
  procedure glMaterialfv(face: Integer; pname: Integer; params: TJavaArray<Single>; offset: Integer); cdecl;
  procedure glMaterialx(face: Integer; pname: Integer; param: Integer); cdecl;
  procedure glMaterialxv(face: Integer; pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glMatrixMode(mode: Integer); cdecl;
  procedure glMultMatrixf(m: TJavaArray<Single>; offset: Integer); cdecl;
  procedure glMultMatrixx(m: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glMultiTexCoord4f(target: Integer; s: Single; t: Single; r: Single; q: Single); cdecl;
  procedure glMultiTexCoord4x(target: Integer; s: Integer; t: Integer; r: Integer; q: Integer); cdecl;
  procedure glNormal3f(nx: Single; ny: Single; nz: Single); cdecl;
  procedure glNormal3x(nx: Integer; ny: Integer; nz: Integer); cdecl;
  procedure glOrthof(left: Single; right: Single; bottom: Single; top: Single; zNear: Single; zFar: Single); cdecl;
  procedure glOrthox(left: Integer; right: Integer; bottom: Integer; top: Integer; zNear: Integer; zFar: Integer); cdecl;
  procedure glPixelStorei(pname: Integer; param: Integer); cdecl;
  procedure glPointSize(size: Single); cdecl;
  procedure glPointSizex(size: Integer); cdecl;
  procedure glPolygonOffset(factor: Single; units: Single); cdecl;
  procedure glPolygonOffsetx(factor: Integer; units: Integer); cdecl;
  procedure glPopMatrix; cdecl;
  procedure glPushMatrix; cdecl;
  procedure glRotatef(angle: Single; x: Single; y: Single; z: Single); cdecl;
  procedure glRotatex(angle: Integer; x: Integer; y: Integer; z: Integer); cdecl;
  procedure glSampleCoverage(value: Single; invert: Boolean); cdecl;
  procedure glSampleCoveragex(value: Integer; invert: Boolean); cdecl;
  procedure glScalef(x: Single; y: Single; z: Single); cdecl;
  procedure glScalex(x: Integer; y: Integer; z: Integer); cdecl;
  procedure glScissor(x: Integer; y: Integer; width: Integer; height: Integer); cdecl;
  procedure glShadeModel(mode: Integer); cdecl;
  procedure glStencilFunc(func: Integer; ref: Integer; mask: Integer); cdecl;
  procedure glStencilMask(mask: Integer); cdecl;
  procedure glStencilOp(fail: Integer; zfail: Integer; zpass: Integer); cdecl;
  procedure glTexEnvf(target: Integer; pname: Integer; param: Single); cdecl;
  procedure glTexEnvfv(target: Integer; pname: Integer; params: TJavaArray<Single>; offset: Integer); cdecl;
  procedure glTexEnvx(target: Integer; pname: Integer; param: Integer); cdecl;
  procedure glTexEnvxv(target: Integer; pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glTexParameterf(target: Integer; pname: Integer; param: Single); cdecl;
  procedure glTexParameterx(target: Integer; pname: Integer; param: Integer); cdecl;
  procedure glTranslatef(x: Single; y: Single; z: Single); cdecl;
  procedure glTranslatex(x: Integer; y: Integer; z: Integer); cdecl;
  procedure glViewport(x: Integer; y: Integer; width: Integer; height: Integer); cdecl;
  {Properties}
  property GL_ADD: Integer read _GetGL_ADD;
  property GL_ALIASED_LINE_WIDTH_RANGE: Integer read _GetGL_ALIASED_LINE_WIDTH_RANGE;
  property GL_ALIASED_POINT_SIZE_RANGE: Integer read _GetGL_ALIASED_POINT_SIZE_RANGE;
  property GL_ALPHA: Integer read _GetGL_ALPHA;
  property GL_ALPHA_BITS: Integer read _GetGL_ALPHA_BITS;
  property GL_ALPHA_TEST: Integer read _GetGL_ALPHA_TEST;
  property GL_ALWAYS: Integer read _GetGL_ALWAYS;
  property GL_AMBIENT: Integer read _GetGL_AMBIENT;
  property GL_AMBIENT_AND_DIFFUSE: Integer read _GetGL_AMBIENT_AND_DIFFUSE;
  property GL_AND: Integer read _GetGL_AND;
  property GL_AND_INVERTED: Integer read _GetGL_AND_INVERTED;
  property GL_AND_REVERSE: Integer read _GetGL_AND_REVERSE;
  property GL_BACK: Integer read _GetGL_BACK;
  property GL_BLEND: Integer read _GetGL_BLEND;
  property GL_BLUE_BITS: Integer read _GetGL_BLUE_BITS;
  property GL_BYTE: Integer read _GetGL_BYTE;
  property GL_CCW: Integer read _GetGL_CCW;
  property GL_CLAMP_TO_EDGE: Integer read _GetGL_CLAMP_TO_EDGE;
  property GL_CLEAR: Integer read _GetGL_CLEAR;
  property GL_COLOR_ARRAY: Integer read _GetGL_COLOR_ARRAY;
  property GL_COLOR_BUFFER_BIT: Integer read _GetGL_COLOR_BUFFER_BIT;
  property GL_COLOR_LOGIC_OP: Integer read _GetGL_COLOR_LOGIC_OP;
  property GL_COLOR_MATERIAL: Integer read _GetGL_COLOR_MATERIAL;
  property GL_COMPRESSED_TEXTURE_FORMATS: Integer read _GetGL_COMPRESSED_TEXTURE_FORMATS;
  property GL_CONSTANT_ATTENUATION: Integer read _GetGL_CONSTANT_ATTENUATION;
  property GL_COPY: Integer read _GetGL_COPY;
  property GL_COPY_INVERTED: Integer read _GetGL_COPY_INVERTED;
  property GL_CULL_FACE: Integer read _GetGL_CULL_FACE;
  property GL_CW: Integer read _GetGL_CW;
  property GL_DECAL: Integer read _GetGL_DECAL;
  property GL_DECR: Integer read _GetGL_DECR;
  property GL_DEPTH_BITS: Integer read _GetGL_DEPTH_BITS;
  property GL_DEPTH_BUFFER_BIT: Integer read _GetGL_DEPTH_BUFFER_BIT;
  property GL_DEPTH_TEST: Integer read _GetGL_DEPTH_TEST;
  property GL_DIFFUSE: Integer read _GetGL_DIFFUSE;
  property GL_DITHER: Integer read _GetGL_DITHER;
  property GL_DONT_CARE: Integer read _GetGL_DONT_CARE;
  property GL_DST_ALPHA: Integer read _GetGL_DST_ALPHA;
  property GL_DST_COLOR: Integer read _GetGL_DST_COLOR;
  property GL_EMISSION: Integer read _GetGL_EMISSION;
  property GL_EQUAL: Integer read _GetGL_EQUAL;
  property GL_EQUIV: Integer read _GetGL_EQUIV;
  property GL_EXP: Integer read _GetGL_EXP;
  property GL_EXP2: Integer read _GetGL_EXP2;
  property GL_EXTENSIONS: Integer read _GetGL_EXTENSIONS;
  property GL_FALSE: Integer read _GetGL_FALSE;
  property GL_FASTEST: Integer read _GetGL_FASTEST;
  property GL_FIXED: Integer read _GetGL_FIXED;
  property GL_FLAT: Integer read _GetGL_FLAT;
  property GL_FLOAT: Integer read _GetGL_FLOAT;
  property GL_FOG: Integer read _GetGL_FOG;
  property GL_FOG_COLOR: Integer read _GetGL_FOG_COLOR;
  property GL_FOG_DENSITY: Integer read _GetGL_FOG_DENSITY;
  property GL_FOG_END: Integer read _GetGL_FOG_END;
  property GL_FOG_HINT: Integer read _GetGL_FOG_HINT;
  property GL_FOG_MODE: Integer read _GetGL_FOG_MODE;
  property GL_FOG_START: Integer read _GetGL_FOG_START;
  property GL_FRONT: Integer read _GetGL_FRONT;
  property GL_FRONT_AND_BACK: Integer read _GetGL_FRONT_AND_BACK;
  property GL_GEQUAL: Integer read _GetGL_GEQUAL;
  property GL_GREATER: Integer read _GetGL_GREATER;
  property GL_GREEN_BITS: Integer read _GetGL_GREEN_BITS;
  property GL_IMPLEMENTATION_COLOR_READ_FORMAT_OES: Integer read _GetGL_IMPLEMENTATION_COLOR_READ_FORMAT_OES;
  property GL_IMPLEMENTATION_COLOR_READ_TYPE_OES: Integer read _GetGL_IMPLEMENTATION_COLOR_READ_TYPE_OES;
  property GL_INCR: Integer read _GetGL_INCR;
  property GL_INVALID_ENUM: Integer read _GetGL_INVALID_ENUM;
  property GL_INVALID_OPERATION: Integer read _GetGL_INVALID_OPERATION;
  property GL_INVALID_VALUE: Integer read _GetGL_INVALID_VALUE;
  property GL_INVERT: Integer read _GetGL_INVERT;
  property GL_KEEP: Integer read _GetGL_KEEP;
  property GL_LEQUAL: Integer read _GetGL_LEQUAL;
  property GL_LESS: Integer read _GetGL_LESS;
  property GL_LIGHT0: Integer read _GetGL_LIGHT0;
  property GL_LIGHT1: Integer read _GetGL_LIGHT1;
  property GL_LIGHT2: Integer read _GetGL_LIGHT2;
  property GL_LIGHT3: Integer read _GetGL_LIGHT3;
  property GL_LIGHT4: Integer read _GetGL_LIGHT4;
  property GL_LIGHT5: Integer read _GetGL_LIGHT5;
  property GL_LIGHT6: Integer read _GetGL_LIGHT6;
  property GL_LIGHT7: Integer read _GetGL_LIGHT7;
  property GL_LIGHTING: Integer read _GetGL_LIGHTING;
  property GL_LIGHT_MODEL_AMBIENT: Integer read _GetGL_LIGHT_MODEL_AMBIENT;
  property GL_LIGHT_MODEL_TWO_SIDE: Integer read _GetGL_LIGHT_MODEL_TWO_SIDE;
  property GL_LINEAR: Integer read _GetGL_LINEAR;
  property GL_LINEAR_ATTENUATION: Integer read _GetGL_LINEAR_ATTENUATION;
  property GL_LINEAR_MIPMAP_LINEAR: Integer read _GetGL_LINEAR_MIPMAP_LINEAR;
  property GL_LINEAR_MIPMAP_NEAREST: Integer read _GetGL_LINEAR_MIPMAP_NEAREST;
  property GL_LINES: Integer read _GetGL_LINES;
  property GL_LINE_LOOP: Integer read _GetGL_LINE_LOOP;
  property GL_LINE_SMOOTH: Integer read _GetGL_LINE_SMOOTH;
  property GL_LINE_SMOOTH_HINT: Integer read _GetGL_LINE_SMOOTH_HINT;
  property GL_LINE_STRIP: Integer read _GetGL_LINE_STRIP;
  property GL_LUMINANCE: Integer read _GetGL_LUMINANCE;
  property GL_LUMINANCE_ALPHA: Integer read _GetGL_LUMINANCE_ALPHA;
  property GL_MAX_ELEMENTS_INDICES: Integer read _GetGL_MAX_ELEMENTS_INDICES;
  property GL_MAX_ELEMENTS_VERTICES: Integer read _GetGL_MAX_ELEMENTS_VERTICES;
  property GL_MAX_LIGHTS: Integer read _GetGL_MAX_LIGHTS;
  property GL_MAX_MODELVIEW_STACK_DEPTH: Integer read _GetGL_MAX_MODELVIEW_STACK_DEPTH;
  property GL_MAX_PROJECTION_STACK_DEPTH: Integer read _GetGL_MAX_PROJECTION_STACK_DEPTH;
  property GL_MAX_TEXTURE_SIZE: Integer read _GetGL_MAX_TEXTURE_SIZE;
  property GL_MAX_TEXTURE_STACK_DEPTH: Integer read _GetGL_MAX_TEXTURE_STACK_DEPTH;
  property GL_MAX_TEXTURE_UNITS: Integer read _GetGL_MAX_TEXTURE_UNITS;
  property GL_MAX_VIEWPORT_DIMS: Integer read _GetGL_MAX_VIEWPORT_DIMS;
  property GL_MODELVIEW: Integer read _GetGL_MODELVIEW;
  property GL_MODULATE: Integer read _GetGL_MODULATE;
  property GL_MULTISAMPLE: Integer read _GetGL_MULTISAMPLE;
  property GL_NAND: Integer read _GetGL_NAND;
  property GL_NEAREST: Integer read _GetGL_NEAREST;
  property GL_NEAREST_MIPMAP_LINEAR: Integer read _GetGL_NEAREST_MIPMAP_LINEAR;
  property GL_NEAREST_MIPMAP_NEAREST: Integer read _GetGL_NEAREST_MIPMAP_NEAREST;
  property GL_NEVER: Integer read _GetGL_NEVER;
  property GL_NICEST: Integer read _GetGL_NICEST;
  property GL_NOOP: Integer read _GetGL_NOOP;
  property GL_NOR: Integer read _GetGL_NOR;
  property GL_NORMALIZE: Integer read _GetGL_NORMALIZE;
  property GL_NORMAL_ARRAY: Integer read _GetGL_NORMAL_ARRAY;
  property GL_NOTEQUAL: Integer read _GetGL_NOTEQUAL;
  property GL_NO_ERROR: Integer read _GetGL_NO_ERROR;
  property GL_NUM_COMPRESSED_TEXTURE_FORMATS: Integer read _GetGL_NUM_COMPRESSED_TEXTURE_FORMATS;
  property GL_ONE: Integer read _GetGL_ONE;
  property GL_ONE_MINUS_DST_ALPHA: Integer read _GetGL_ONE_MINUS_DST_ALPHA;
  property GL_ONE_MINUS_DST_COLOR: Integer read _GetGL_ONE_MINUS_DST_COLOR;
  property GL_ONE_MINUS_SRC_ALPHA: Integer read _GetGL_ONE_MINUS_SRC_ALPHA;
  property GL_ONE_MINUS_SRC_COLOR: Integer read _GetGL_ONE_MINUS_SRC_COLOR;
  property GL_OR: Integer read _GetGL_OR;
  property GL_OR_INVERTED: Integer read _GetGL_OR_INVERTED;
  property GL_OR_REVERSE: Integer read _GetGL_OR_REVERSE;
  property GL_OUT_OF_MEMORY: Integer read _GetGL_OUT_OF_MEMORY;
  property GL_PACK_ALIGNMENT: Integer read _GetGL_PACK_ALIGNMENT;
  property GL_PALETTE4_R5_G6_B5_OES: Integer read _GetGL_PALETTE4_R5_G6_B5_OES;
  property GL_PALETTE4_RGB5_A1_OES: Integer read _GetGL_PALETTE4_RGB5_A1_OES;
  property GL_PALETTE4_RGB8_OES: Integer read _GetGL_PALETTE4_RGB8_OES;
  property GL_PALETTE4_RGBA4_OES: Integer read _GetGL_PALETTE4_RGBA4_OES;
  property GL_PALETTE4_RGBA8_OES: Integer read _GetGL_PALETTE4_RGBA8_OES;
  property GL_PALETTE8_R5_G6_B5_OES: Integer read _GetGL_PALETTE8_R5_G6_B5_OES;
  property GL_PALETTE8_RGB5_A1_OES: Integer read _GetGL_PALETTE8_RGB5_A1_OES;
  property GL_PALETTE8_RGB8_OES: Integer read _GetGL_PALETTE8_RGB8_OES;
  property GL_PALETTE8_RGBA4_OES: Integer read _GetGL_PALETTE8_RGBA4_OES;
  property GL_PALETTE8_RGBA8_OES: Integer read _GetGL_PALETTE8_RGBA8_OES;
  property GL_PERSPECTIVE_CORRECTION_HINT: Integer read _GetGL_PERSPECTIVE_CORRECTION_HINT;
  property GL_POINTS: Integer read _GetGL_POINTS;
  property GL_POINT_FADE_THRESHOLD_SIZE: Integer read _GetGL_POINT_FADE_THRESHOLD_SIZE;
  property GL_POINT_SIZE: Integer read _GetGL_POINT_SIZE;
  property GL_POINT_SMOOTH: Integer read _GetGL_POINT_SMOOTH;
  property GL_POINT_SMOOTH_HINT: Integer read _GetGL_POINT_SMOOTH_HINT;
  property GL_POLYGON_OFFSET_FILL: Integer read _GetGL_POLYGON_OFFSET_FILL;
  property GL_POLYGON_SMOOTH_HINT: Integer read _GetGL_POLYGON_SMOOTH_HINT;
  property GL_POSITION: Integer read _GetGL_POSITION;
  property GL_PROJECTION: Integer read _GetGL_PROJECTION;
  property GL_QUADRATIC_ATTENUATION: Integer read _GetGL_QUADRATIC_ATTENUATION;
  property GL_RED_BITS: Integer read _GetGL_RED_BITS;
  property GL_RENDERER: Integer read _GetGL_RENDERER;
  property GL_REPEAT: Integer read _GetGL_REPEAT;
  property GL_REPLACE: Integer read _GetGL_REPLACE;
  property GL_RESCALE_NORMAL: Integer read _GetGL_RESCALE_NORMAL;
  property GL_RGB: Integer read _GetGL_RGB;
  property GL_RGBA: Integer read _GetGL_RGBA;
  property GL_SAMPLE_ALPHA_TO_COVERAGE: Integer read _GetGL_SAMPLE_ALPHA_TO_COVERAGE;
  property GL_SAMPLE_ALPHA_TO_ONE: Integer read _GetGL_SAMPLE_ALPHA_TO_ONE;
  property GL_SAMPLE_COVERAGE: Integer read _GetGL_SAMPLE_COVERAGE;
  property GL_SCISSOR_TEST: Integer read _GetGL_SCISSOR_TEST;
  property GL_SET: Integer read _GetGL_SET;
  property GL_SHININESS: Integer read _GetGL_SHININESS;
  property GL_SHORT: Integer read _GetGL_SHORT;
  property GL_SMOOTH: Integer read _GetGL_SMOOTH;
  property GL_SMOOTH_LINE_WIDTH_RANGE: Integer read _GetGL_SMOOTH_LINE_WIDTH_RANGE;
  property GL_SMOOTH_POINT_SIZE_RANGE: Integer read _GetGL_SMOOTH_POINT_SIZE_RANGE;
  property GL_SPECULAR: Integer read _GetGL_SPECULAR;
  property GL_SPOT_CUTOFF: Integer read _GetGL_SPOT_CUTOFF;
  property GL_SPOT_DIRECTION: Integer read _GetGL_SPOT_DIRECTION;
  property GL_SPOT_EXPONENT: Integer read _GetGL_SPOT_EXPONENT;
  property GL_SRC_ALPHA: Integer read _GetGL_SRC_ALPHA;
  property GL_SRC_ALPHA_SATURATE: Integer read _GetGL_SRC_ALPHA_SATURATE;
  property GL_SRC_COLOR: Integer read _GetGL_SRC_COLOR;
  property GL_STACK_OVERFLOW: Integer read _GetGL_STACK_OVERFLOW;
  property GL_STACK_UNDERFLOW: Integer read _GetGL_STACK_UNDERFLOW;
  property GL_STENCIL_BITS: Integer read _GetGL_STENCIL_BITS;
  property GL_STENCIL_BUFFER_BIT: Integer read _GetGL_STENCIL_BUFFER_BIT;
  property GL_STENCIL_TEST: Integer read _GetGL_STENCIL_TEST;
  property GL_SUBPIXEL_BITS: Integer read _GetGL_SUBPIXEL_BITS;
  property GL_TEXTURE: Integer read _GetGL_TEXTURE;
  property GL_TEXTURE0: Integer read _GetGL_TEXTURE0;
  property GL_TEXTURE1: Integer read _GetGL_TEXTURE1;
  property GL_TEXTURE10: Integer read _GetGL_TEXTURE10;
  property GL_TEXTURE11: Integer read _GetGL_TEXTURE11;
  property GL_TEXTURE12: Integer read _GetGL_TEXTURE12;
  property GL_TEXTURE13: Integer read _GetGL_TEXTURE13;
  property GL_TEXTURE14: Integer read _GetGL_TEXTURE14;
  property GL_TEXTURE15: Integer read _GetGL_TEXTURE15;
  property GL_TEXTURE16: Integer read _GetGL_TEXTURE16;
  property GL_TEXTURE17: Integer read _GetGL_TEXTURE17;
  property GL_TEXTURE18: Integer read _GetGL_TEXTURE18;
  property GL_TEXTURE19: Integer read _GetGL_TEXTURE19;
  property GL_TEXTURE2: Integer read _GetGL_TEXTURE2;
  property GL_TEXTURE20: Integer read _GetGL_TEXTURE20;
  property GL_TEXTURE21: Integer read _GetGL_TEXTURE21;
  property GL_TEXTURE22: Integer read _GetGL_TEXTURE22;
  property GL_TEXTURE23: Integer read _GetGL_TEXTURE23;
  property GL_TEXTURE24: Integer read _GetGL_TEXTURE24;
  property GL_TEXTURE25: Integer read _GetGL_TEXTURE25;
  property GL_TEXTURE26: Integer read _GetGL_TEXTURE26;
  property GL_TEXTURE27: Integer read _GetGL_TEXTURE27;
  property GL_TEXTURE28: Integer read _GetGL_TEXTURE28;
  property GL_TEXTURE29: Integer read _GetGL_TEXTURE29;
  property GL_TEXTURE3: Integer read _GetGL_TEXTURE3;
  property GL_TEXTURE30: Integer read _GetGL_TEXTURE30;
  property GL_TEXTURE31: Integer read _GetGL_TEXTURE31;
  property GL_TEXTURE4: Integer read _GetGL_TEXTURE4;
  property GL_TEXTURE5: Integer read _GetGL_TEXTURE5;
  property GL_TEXTURE6: Integer read _GetGL_TEXTURE6;
  property GL_TEXTURE7: Integer read _GetGL_TEXTURE7;
  property GL_TEXTURE8: Integer read _GetGL_TEXTURE8;
  property GL_TEXTURE9: Integer read _GetGL_TEXTURE9;
  property GL_TEXTURE_2D: Integer read _GetGL_TEXTURE_2D;
  property GL_TEXTURE_COORD_ARRAY: Integer read _GetGL_TEXTURE_COORD_ARRAY;
  property GL_TEXTURE_ENV: Integer read _GetGL_TEXTURE_ENV;
  property GL_TEXTURE_ENV_COLOR: Integer read _GetGL_TEXTURE_ENV_COLOR;
  property GL_TEXTURE_ENV_MODE: Integer read _GetGL_TEXTURE_ENV_MODE;
  property GL_TEXTURE_MAG_FILTER: Integer read _GetGL_TEXTURE_MAG_FILTER;
  property GL_TEXTURE_MIN_FILTER: Integer read _GetGL_TEXTURE_MIN_FILTER;
  property GL_TEXTURE_WRAP_S: Integer read _GetGL_TEXTURE_WRAP_S;
  property GL_TEXTURE_WRAP_T: Integer read _GetGL_TEXTURE_WRAP_T;
  property GL_TRIANGLES: Integer read _GetGL_TRIANGLES;
  property GL_TRIANGLE_FAN: Integer read _GetGL_TRIANGLE_FAN;
  property GL_TRIANGLE_STRIP: Integer read _GetGL_TRIANGLE_STRIP;
  property GL_TRUE: Integer read _GetGL_TRUE;
  property GL_UNPACK_ALIGNMENT: Integer read _GetGL_UNPACK_ALIGNMENT;
  property GL_UNSIGNED_BYTE: Integer read _GetGL_UNSIGNED_BYTE;
  property GL_UNSIGNED_SHORT: Integer read _GetGL_UNSIGNED_SHORT;
  property GL_UNSIGNED_SHORT_4_4_4_4: Integer read _GetGL_UNSIGNED_SHORT_4_4_4_4;
  property GL_UNSIGNED_SHORT_5_5_5_1: Integer read _GetGL_UNSIGNED_SHORT_5_5_5_1;
  property GL_UNSIGNED_SHORT_5_6_5: Integer read _GetGL_UNSIGNED_SHORT_5_6_5;
  property GL_VENDOR: Integer read _GetGL_VENDOR;
  property GL_VERSION: Integer read _GetGL_VERSION;
  property GL_VERTEX_ARRAY: Integer read _GetGL_VERTEX_ARRAY;
  property GL_XOR: Integer read _GetGL_XOR;
  property GL_ZERO: Integer read _GetGL_ZERO;
end;

[JavaSignature('android/opengl/GLES10')]
JGLES10 = interface(JObject)
['{C4CE6341-D0A4-4865-87A2-5251AE952149}']
end;
TJGLES10 = class(TJavaGenericImport<JGLES10Class, JGLES10>) end;

JEGL14Class = interface(JObjectClass)
['{9310A9F9-6946-402D-8221-860ED16B719D}']
  {Property Methods}
  function _GetEGL_ALPHA_MASK_SIZE: Integer;
  function _GetEGL_ALPHA_SIZE: Integer;
  function _GetEGL_BACK_BUFFER: Integer;
  function _GetEGL_BAD_ACCESS: Integer;
  function _GetEGL_BAD_ALLOC: Integer;
  function _GetEGL_BAD_ATTRIBUTE: Integer;
  function _GetEGL_BAD_CONFIG: Integer;
  function _GetEGL_BAD_CONTEXT: Integer;
  function _GetEGL_BAD_CURRENT_SURFACE: Integer;
  function _GetEGL_BAD_DISPLAY: Integer;
  function _GetEGL_BAD_MATCH: Integer;
  function _GetEGL_BAD_NATIVE_PIXMAP: Integer;
  function _GetEGL_BAD_NATIVE_WINDOW: Integer;
  function _GetEGL_BAD_PARAMETER: Integer;
  function _GetEGL_BAD_SURFACE: Integer;
  function _GetEGL_BIND_TO_TEXTURE_RGB: Integer;
  function _GetEGL_BIND_TO_TEXTURE_RGBA: Integer;
  function _GetEGL_BLUE_SIZE: Integer;
  function _GetEGL_BUFFER_DESTROYED: Integer;
  function _GetEGL_BUFFER_PRESERVED: Integer;
  function _GetEGL_BUFFER_SIZE: Integer;
  function _GetEGL_CLIENT_APIS: Integer;
  function _GetEGL_COLOR_BUFFER_TYPE: Integer;
  function _GetEGL_CONFIG_CAVEAT: Integer;
  function _GetEGL_CONFIG_ID: Integer;
  function _GetEGL_CONFORMANT: Integer;
  function _GetEGL_CONTEXT_CLIENT_TYPE: Integer;
  function _GetEGL_CONTEXT_CLIENT_VERSION: Integer;
  function _GetEGL_CONTEXT_LOST: Integer;
  function _GetEGL_CORE_NATIVE_ENGINE: Integer;
  function _GetEGL_DEFAULT_DISPLAY: Integer;
  function _GetEGL_DEPTH_SIZE: Integer;
  function _GetEGL_DISPLAY_SCALING: Integer;
  function _GetEGL_DRAW: Integer;
  function _GetEGL_EXTENSIONS: Integer;
  function _GetEGL_FALSE: Integer;
  function _GetEGL_GREEN_SIZE: Integer;
  function _GetEGL_HEIGHT: Integer;
  function _GetEGL_HORIZONTAL_RESOLUTION: Integer;
  function _GetEGL_LARGEST_PBUFFER: Integer;
  function _GetEGL_LEVEL: Integer;
  function _GetEGL_LUMINANCE_BUFFER: Integer;
  function _GetEGL_LUMINANCE_SIZE: Integer;
  function _GetEGL_MATCH_NATIVE_PIXMAP: Integer;
  function _GetEGL_MAX_PBUFFER_HEIGHT: Integer;
  function _GetEGL_MAX_PBUFFER_PIXELS: Integer;
  function _GetEGL_MAX_PBUFFER_WIDTH: Integer;
  function _GetEGL_MAX_SWAP_INTERVAL: Integer;
  function _GetEGL_MIN_SWAP_INTERVAL: Integer;
  function _GetEGL_MIPMAP_LEVEL: Integer;
  function _GetEGL_MIPMAP_TEXTURE: Integer;
  function _GetEGL_MULTISAMPLE_RESOLVE: Integer;
  function _GetEGL_MULTISAMPLE_RESOLVE_BOX: Integer;
  function _GetEGL_MULTISAMPLE_RESOLVE_BOX_BIT: Integer;
  function _GetEGL_MULTISAMPLE_RESOLVE_DEFAULT: Integer;
  function _GetEGL_NATIVE_RENDERABLE: Integer;
  function _GetEGL_NATIVE_VISUAL_ID: Integer;
  function _GetEGL_NATIVE_VISUAL_TYPE: Integer;
  function _GetEGL_NONE: Integer;
  function _GetEGL_NON_CONFORMANT_CONFIG: Integer;
  function _GetEGL_NOT_INITIALIZED: Integer;
  function _GetEGL_NO_CONTEXT: Jopengl_EGLContext;
  procedure _SetEGL_NO_CONTEXT(Value: Jopengl_EGLContext);
  function _GetEGL_NO_DISPLAY: Jopengl_EGLDisplay;
  procedure _SetEGL_NO_DISPLAY(Value: Jopengl_EGLDisplay);
  function _GetEGL_NO_SURFACE: Jopengl_EGLSurface;
  procedure _SetEGL_NO_SURFACE(Value: Jopengl_EGLSurface);
  function _GetEGL_NO_TEXTURE: Integer;
  function _GetEGL_OPENGL_API: Integer;
  function _GetEGL_OPENGL_BIT: Integer;
  function _GetEGL_OPENGL_ES2_BIT: Integer;
  function _GetEGL_OPENGL_ES_API: Integer;
  function _GetEGL_OPENGL_ES_BIT: Integer;
  function _GetEGL_OPENVG_API: Integer;
  function _GetEGL_OPENVG_BIT: Integer;
  function _GetEGL_OPENVG_IMAGE: Integer;
  function _GetEGL_PBUFFER_BIT: Integer;
  function _GetEGL_PIXEL_ASPECT_RATIO: Integer;
  function _GetEGL_PIXMAP_BIT: Integer;
  function _GetEGL_READ: Integer;
  function _GetEGL_RED_SIZE: Integer;
  function _GetEGL_RENDERABLE_TYPE: Integer;
  function _GetEGL_RENDER_BUFFER: Integer;
  function _GetEGL_RGB_BUFFER: Integer;
  function _GetEGL_SAMPLES: Integer;
  function _GetEGL_SAMPLE_BUFFERS: Integer;
  function _GetEGL_SINGLE_BUFFER: Integer;
  function _GetEGL_SLOW_CONFIG: Integer;
  function _GetEGL_STENCIL_SIZE: Integer;
  function _GetEGL_SUCCESS: Integer;
  function _GetEGL_SURFACE_TYPE: Integer;
  function _GetEGL_SWAP_BEHAVIOR: Integer;
  function _GetEGL_SWAP_BEHAVIOR_PRESERVED_BIT: Integer;
  function _GetEGL_TEXTURE_2D: Integer;
  function _GetEGL_TEXTURE_FORMAT: Integer;
  function _GetEGL_TEXTURE_RGB: Integer;
  function _GetEGL_TEXTURE_RGBA: Integer;
  function _GetEGL_TEXTURE_TARGET: Integer;
  function _GetEGL_TRANSPARENT_BLUE_VALUE: Integer;
  function _GetEGL_TRANSPARENT_GREEN_VALUE: Integer;
  function _GetEGL_TRANSPARENT_RED_VALUE: Integer;
  function _GetEGL_TRANSPARENT_RGB: Integer;
  function _GetEGL_TRANSPARENT_TYPE: Integer;
  function _GetEGL_TRUE: Integer;
  function _GetEGL_VENDOR: Integer;
  function _GetEGL_VERSION: Integer;
  function _GetEGL_VERTICAL_RESOLUTION: Integer;
  function _GetEGL_VG_ALPHA_FORMAT: Integer;
  function _GetEGL_VG_ALPHA_FORMAT_NONPRE: Integer;
  function _GetEGL_VG_ALPHA_FORMAT_PRE: Integer;
  function _GetEGL_VG_ALPHA_FORMAT_PRE_BIT: Integer;
  function _GetEGL_VG_COLORSPACE: Integer;
  function _GetEGL_VG_COLORSPACE_LINEAR: Integer;
  function _GetEGL_VG_COLORSPACE_LINEAR_BIT: Integer;
  function _GetEGL_VG_COLORSPACE_sRGB: Integer;
  function _GetEGL_WIDTH: Integer;
  function _GetEGL_WINDOW_BIT: Integer;
  {Methods}
  function init: JEGL14; cdecl;
  function eglBindAPI(api: Integer): Boolean; cdecl;
  function eglBindTexImage(dpy: Jopengl_EGLDisplay; surface: Jopengl_EGLSurface; buffer: Integer): Boolean; cdecl;
  function eglChooseConfig(dpy: Jopengl_EGLDisplay; attrib_list: TJavaArray<Integer>; attrib_listOffset: Integer; configs: TJavaObjectArray<Jopengl_EGLConfig>; configsOffset: Integer; config_size: Integer; num_config: TJavaArray<Integer>; num_configOffset: Integer): Boolean; cdecl;
  function eglCopyBuffers(dpy: Jopengl_EGLDisplay; surface: Jopengl_EGLSurface; target: Integer): Boolean; cdecl;
  function eglCreateContext(dpy: Jopengl_EGLDisplay; config: Jopengl_EGLConfig; share_context: Jopengl_EGLContext; attrib_list: TJavaArray<Integer>; offset: Integer): Jopengl_EGLContext; cdecl;
  function eglCreatePbufferFromClientBuffer(dpy: Jopengl_EGLDisplay; buftype: Integer; buffer: Integer; config: Jopengl_EGLConfig; attrib_list: TJavaArray<Integer>; offset: Integer): Jopengl_EGLSurface; cdecl;
  function eglCreatePbufferSurface(dpy: Jopengl_EGLDisplay; config: Jopengl_EGLConfig; attrib_list: TJavaArray<Integer>; offset: Integer): Jopengl_EGLSurface; cdecl;
  function eglCreatePixmapSurface(dpy: Jopengl_EGLDisplay; config: Jopengl_EGLConfig; pixmap: Integer; attrib_list: TJavaArray<Integer>; offset: Integer): Jopengl_EGLSurface; cdecl;
  function eglCreateWindowSurface(dpy: Jopengl_EGLDisplay; config: Jopengl_EGLConfig; win: JObject; attrib_list: TJavaArray<Integer>; offset: Integer): Jopengl_EGLSurface; cdecl;
  function eglDestroyContext(dpy: Jopengl_EGLDisplay; ctx: Jopengl_EGLContext): Boolean; cdecl;
  function eglDestroySurface(dpy: Jopengl_EGLDisplay; surface: Jopengl_EGLSurface): Boolean; cdecl;
  function eglGetConfigAttrib(dpy: Jopengl_EGLDisplay; config: Jopengl_EGLConfig; attribute: Integer; value: TJavaArray<Integer>; offset: Integer): Boolean; cdecl;
  function eglGetConfigs(dpy: Jopengl_EGLDisplay; configs: TJavaObjectArray<Jopengl_EGLConfig>; configsOffset: Integer; config_size: Integer; num_config: TJavaArray<Integer>; num_configOffset: Integer): Boolean; cdecl;
  function eglGetCurrentContext: Jopengl_EGLContext; cdecl;
  function eglGetCurrentDisplay: Jopengl_EGLDisplay; cdecl;
  function eglGetCurrentSurface(readdraw: Integer): Jopengl_EGLSurface; cdecl;
  function eglGetDisplay(display_id: Integer): Jopengl_EGLDisplay; cdecl;
  function eglGetError: Integer; cdecl;
  function eglInitialize(dpy: Jopengl_EGLDisplay; major: TJavaArray<Integer>; majorOffset: Integer; minor: TJavaArray<Integer>; minorOffset: Integer): Boolean; cdecl;
  function eglMakeCurrent(dpy: Jopengl_EGLDisplay; draw: Jopengl_EGLSurface; read: Jopengl_EGLSurface; ctx: Jopengl_EGLContext): Boolean; cdecl;
  function eglQueryAPI: Integer; cdecl;
  function eglQueryContext(dpy: Jopengl_EGLDisplay; ctx: Jopengl_EGLContext; attribute: Integer; value: TJavaArray<Integer>; offset: Integer): Boolean; cdecl;
  function eglQueryString(dpy: Jopengl_EGLDisplay; name: Integer): JString; cdecl;
  function eglQuerySurface(dpy: Jopengl_EGLDisplay; surface: Jopengl_EGLSurface; attribute: Integer; value: TJavaArray<Integer>; offset: Integer): Boolean; cdecl;
  function eglReleaseTexImage(dpy: Jopengl_EGLDisplay; surface: Jopengl_EGLSurface; buffer: Integer): Boolean; cdecl;
  function eglReleaseThread: Boolean; cdecl;
  function eglSurfaceAttrib(dpy: Jopengl_EGLDisplay; surface: Jopengl_EGLSurface; attribute: Integer; value: Integer): Boolean; cdecl;
  function eglSwapBuffers(dpy: Jopengl_EGLDisplay; surface: Jopengl_EGLSurface): Boolean; cdecl;
  function eglSwapInterval(dpy: Jopengl_EGLDisplay; interval: Integer): Boolean; cdecl;
  function eglTerminate(dpy: Jopengl_EGLDisplay): Boolean; cdecl;
  function eglWaitClient: Boolean; cdecl;
  function eglWaitGL: Boolean; cdecl;
  function eglWaitNative(engine: Integer): Boolean; cdecl;
  {Properties}
  property EGL_ALPHA_MASK_SIZE: Integer read _GetEGL_ALPHA_MASK_SIZE;
  property EGL_ALPHA_SIZE: Integer read _GetEGL_ALPHA_SIZE;
  property EGL_BACK_BUFFER: Integer read _GetEGL_BACK_BUFFER;
  property EGL_BAD_ACCESS: Integer read _GetEGL_BAD_ACCESS;
  property EGL_BAD_ALLOC: Integer read _GetEGL_BAD_ALLOC;
  property EGL_BAD_ATTRIBUTE: Integer read _GetEGL_BAD_ATTRIBUTE;
  property EGL_BAD_CONFIG: Integer read _GetEGL_BAD_CONFIG;
  property EGL_BAD_CONTEXT: Integer read _GetEGL_BAD_CONTEXT;
  property EGL_BAD_CURRENT_SURFACE: Integer read _GetEGL_BAD_CURRENT_SURFACE;
  property EGL_BAD_DISPLAY: Integer read _GetEGL_BAD_DISPLAY;
  property EGL_BAD_MATCH: Integer read _GetEGL_BAD_MATCH;
  property EGL_BAD_NATIVE_PIXMAP: Integer read _GetEGL_BAD_NATIVE_PIXMAP;
  property EGL_BAD_NATIVE_WINDOW: Integer read _GetEGL_BAD_NATIVE_WINDOW;
  property EGL_BAD_PARAMETER: Integer read _GetEGL_BAD_PARAMETER;
  property EGL_BAD_SURFACE: Integer read _GetEGL_BAD_SURFACE;
  property EGL_BIND_TO_TEXTURE_RGB: Integer read _GetEGL_BIND_TO_TEXTURE_RGB;
  property EGL_BIND_TO_TEXTURE_RGBA: Integer read _GetEGL_BIND_TO_TEXTURE_RGBA;
  property EGL_BLUE_SIZE: Integer read _GetEGL_BLUE_SIZE;
  property EGL_BUFFER_DESTROYED: Integer read _GetEGL_BUFFER_DESTROYED;
  property EGL_BUFFER_PRESERVED: Integer read _GetEGL_BUFFER_PRESERVED;
  property EGL_BUFFER_SIZE: Integer read _GetEGL_BUFFER_SIZE;
  property EGL_CLIENT_APIS: Integer read _GetEGL_CLIENT_APIS;
  property EGL_COLOR_BUFFER_TYPE: Integer read _GetEGL_COLOR_BUFFER_TYPE;
  property EGL_CONFIG_CAVEAT: Integer read _GetEGL_CONFIG_CAVEAT;
  property EGL_CONFIG_ID: Integer read _GetEGL_CONFIG_ID;
  property EGL_CONFORMANT: Integer read _GetEGL_CONFORMANT;
  property EGL_CONTEXT_CLIENT_TYPE: Integer read _GetEGL_CONTEXT_CLIENT_TYPE;
  property EGL_CONTEXT_CLIENT_VERSION: Integer read _GetEGL_CONTEXT_CLIENT_VERSION;
  property EGL_CONTEXT_LOST: Integer read _GetEGL_CONTEXT_LOST;
  property EGL_CORE_NATIVE_ENGINE: Integer read _GetEGL_CORE_NATIVE_ENGINE;
  property EGL_DEFAULT_DISPLAY: Integer read _GetEGL_DEFAULT_DISPLAY;
  property EGL_DEPTH_SIZE: Integer read _GetEGL_DEPTH_SIZE;
  property EGL_DISPLAY_SCALING: Integer read _GetEGL_DISPLAY_SCALING;
  property EGL_DRAW: Integer read _GetEGL_DRAW;
  property EGL_EXTENSIONS: Integer read _GetEGL_EXTENSIONS;
  property EGL_FALSE: Integer read _GetEGL_FALSE;
  property EGL_GREEN_SIZE: Integer read _GetEGL_GREEN_SIZE;
  property EGL_HEIGHT: Integer read _GetEGL_HEIGHT;
  property EGL_HORIZONTAL_RESOLUTION: Integer read _GetEGL_HORIZONTAL_RESOLUTION;
  property EGL_LARGEST_PBUFFER: Integer read _GetEGL_LARGEST_PBUFFER;
  property EGL_LEVEL: Integer read _GetEGL_LEVEL;
  property EGL_LUMINANCE_BUFFER: Integer read _GetEGL_LUMINANCE_BUFFER;
  property EGL_LUMINANCE_SIZE: Integer read _GetEGL_LUMINANCE_SIZE;
  property EGL_MATCH_NATIVE_PIXMAP: Integer read _GetEGL_MATCH_NATIVE_PIXMAP;
  property EGL_MAX_PBUFFER_HEIGHT: Integer read _GetEGL_MAX_PBUFFER_HEIGHT;
  property EGL_MAX_PBUFFER_PIXELS: Integer read _GetEGL_MAX_PBUFFER_PIXELS;
  property EGL_MAX_PBUFFER_WIDTH: Integer read _GetEGL_MAX_PBUFFER_WIDTH;
  property EGL_MAX_SWAP_INTERVAL: Integer read _GetEGL_MAX_SWAP_INTERVAL;
  property EGL_MIN_SWAP_INTERVAL: Integer read _GetEGL_MIN_SWAP_INTERVAL;
  property EGL_MIPMAP_LEVEL: Integer read _GetEGL_MIPMAP_LEVEL;
  property EGL_MIPMAP_TEXTURE: Integer read _GetEGL_MIPMAP_TEXTURE;
  property EGL_MULTISAMPLE_RESOLVE: Integer read _GetEGL_MULTISAMPLE_RESOLVE;
  property EGL_MULTISAMPLE_RESOLVE_BOX: Integer read _GetEGL_MULTISAMPLE_RESOLVE_BOX;
  property EGL_MULTISAMPLE_RESOLVE_BOX_BIT: Integer read _GetEGL_MULTISAMPLE_RESOLVE_BOX_BIT;
  property EGL_MULTISAMPLE_RESOLVE_DEFAULT: Integer read _GetEGL_MULTISAMPLE_RESOLVE_DEFAULT;
  property EGL_NATIVE_RENDERABLE: Integer read _GetEGL_NATIVE_RENDERABLE;
  property EGL_NATIVE_VISUAL_ID: Integer read _GetEGL_NATIVE_VISUAL_ID;
  property EGL_NATIVE_VISUAL_TYPE: Integer read _GetEGL_NATIVE_VISUAL_TYPE;
  property EGL_NONE: Integer read _GetEGL_NONE;
  property EGL_NON_CONFORMANT_CONFIG: Integer read _GetEGL_NON_CONFORMANT_CONFIG;
  property EGL_NOT_INITIALIZED: Integer read _GetEGL_NOT_INITIALIZED;
  property EGL_NO_CONTEXT: Jopengl_EGLContext read _GetEGL_NO_CONTEXT write _SetEGL_NO_CONTEXT;
  property EGL_NO_DISPLAY: Jopengl_EGLDisplay read _GetEGL_NO_DISPLAY write _SetEGL_NO_DISPLAY;
  property EGL_NO_SURFACE: Jopengl_EGLSurface read _GetEGL_NO_SURFACE write _SetEGL_NO_SURFACE;
  property EGL_NO_TEXTURE: Integer read _GetEGL_NO_TEXTURE;
  property EGL_OPENGL_API: Integer read _GetEGL_OPENGL_API;
  property EGL_OPENGL_BIT: Integer read _GetEGL_OPENGL_BIT;
  property EGL_OPENGL_ES2_BIT: Integer read _GetEGL_OPENGL_ES2_BIT;
  property EGL_OPENGL_ES_API: Integer read _GetEGL_OPENGL_ES_API;
  property EGL_OPENGL_ES_BIT: Integer read _GetEGL_OPENGL_ES_BIT;
  property EGL_OPENVG_API: Integer read _GetEGL_OPENVG_API;
  property EGL_OPENVG_BIT: Integer read _GetEGL_OPENVG_BIT;
  property EGL_OPENVG_IMAGE: Integer read _GetEGL_OPENVG_IMAGE;
  property EGL_PBUFFER_BIT: Integer read _GetEGL_PBUFFER_BIT;
  property EGL_PIXEL_ASPECT_RATIO: Integer read _GetEGL_PIXEL_ASPECT_RATIO;
  property EGL_PIXMAP_BIT: Integer read _GetEGL_PIXMAP_BIT;
  property EGL_READ: Integer read _GetEGL_READ;
  property EGL_RED_SIZE: Integer read _GetEGL_RED_SIZE;
  property EGL_RENDERABLE_TYPE: Integer read _GetEGL_RENDERABLE_TYPE;
  property EGL_RENDER_BUFFER: Integer read _GetEGL_RENDER_BUFFER;
  property EGL_RGB_BUFFER: Integer read _GetEGL_RGB_BUFFER;
  property EGL_SAMPLES: Integer read _GetEGL_SAMPLES;
  property EGL_SAMPLE_BUFFERS: Integer read _GetEGL_SAMPLE_BUFFERS;
  property EGL_SINGLE_BUFFER: Integer read _GetEGL_SINGLE_BUFFER;
  property EGL_SLOW_CONFIG: Integer read _GetEGL_SLOW_CONFIG;
  property EGL_STENCIL_SIZE: Integer read _GetEGL_STENCIL_SIZE;
  property EGL_SUCCESS: Integer read _GetEGL_SUCCESS;
  property EGL_SURFACE_TYPE: Integer read _GetEGL_SURFACE_TYPE;
  property EGL_SWAP_BEHAVIOR: Integer read _GetEGL_SWAP_BEHAVIOR;
  property EGL_SWAP_BEHAVIOR_PRESERVED_BIT: Integer read _GetEGL_SWAP_BEHAVIOR_PRESERVED_BIT;
  property EGL_TEXTURE_2D: Integer read _GetEGL_TEXTURE_2D;
  property EGL_TEXTURE_FORMAT: Integer read _GetEGL_TEXTURE_FORMAT;
  property EGL_TEXTURE_RGB: Integer read _GetEGL_TEXTURE_RGB;
  property EGL_TEXTURE_RGBA: Integer read _GetEGL_TEXTURE_RGBA;
  property EGL_TEXTURE_TARGET: Integer read _GetEGL_TEXTURE_TARGET;
  property EGL_TRANSPARENT_BLUE_VALUE: Integer read _GetEGL_TRANSPARENT_BLUE_VALUE;
  property EGL_TRANSPARENT_GREEN_VALUE: Integer read _GetEGL_TRANSPARENT_GREEN_VALUE;
  property EGL_TRANSPARENT_RED_VALUE: Integer read _GetEGL_TRANSPARENT_RED_VALUE;
  property EGL_TRANSPARENT_RGB: Integer read _GetEGL_TRANSPARENT_RGB;
  property EGL_TRANSPARENT_TYPE: Integer read _GetEGL_TRANSPARENT_TYPE;
  property EGL_TRUE: Integer read _GetEGL_TRUE;
  property EGL_VENDOR: Integer read _GetEGL_VENDOR;
  property EGL_VERSION: Integer read _GetEGL_VERSION;
  property EGL_VERTICAL_RESOLUTION: Integer read _GetEGL_VERTICAL_RESOLUTION;
  property EGL_VG_ALPHA_FORMAT: Integer read _GetEGL_VG_ALPHA_FORMAT;
  property EGL_VG_ALPHA_FORMAT_NONPRE: Integer read _GetEGL_VG_ALPHA_FORMAT_NONPRE;
  property EGL_VG_ALPHA_FORMAT_PRE: Integer read _GetEGL_VG_ALPHA_FORMAT_PRE;
  property EGL_VG_ALPHA_FORMAT_PRE_BIT: Integer read _GetEGL_VG_ALPHA_FORMAT_PRE_BIT;
  property EGL_VG_COLORSPACE: Integer read _GetEGL_VG_COLORSPACE;
  property EGL_VG_COLORSPACE_LINEAR: Integer read _GetEGL_VG_COLORSPACE_LINEAR;
  property EGL_VG_COLORSPACE_LINEAR_BIT: Integer read _GetEGL_VG_COLORSPACE_LINEAR_BIT;
  property EGL_VG_COLORSPACE_sRGB: Integer read _GetEGL_VG_COLORSPACE_sRGB;
  property EGL_WIDTH: Integer read _GetEGL_WIDTH;
  property EGL_WINDOW_BIT: Integer read _GetEGL_WINDOW_BIT;
end;

[JavaSignature('android/opengl/EGL14')]
JEGL14 = interface(JObject)
['{3F6CFADC-F857-4996-886D-079C676135C7}']
end;
TJEGL14 = class(TJavaGenericImport<JEGL14Class, JEGL14>) end;

JVisibilityClass = interface(JObjectClass)
['{6B76D3D5-276B-4DCE-8BE7-E8E4569C7FC2}']
  {Methods}
  function init: JVisibility; cdecl;
  procedure computeBoundingSphere(positions: TJavaArray<Single>; positionsOffset: Integer; positionsCount: Integer; sphere: TJavaArray<Single>; sphereOffset: Integer); cdecl;
  function frustumCullSpheres(mvp: TJavaArray<Single>; mvpOffset: Integer; spheres: TJavaArray<Single>; spheresOffset: Integer; spheresCount: Integer; results: TJavaArray<Integer>; resultsOffset: Integer; resultsCapacity: Integer): Integer; cdecl;
  function visibilityTest(ws: TJavaArray<Single>; wsOffset: Integer; positions: TJavaArray<Single>; positionsOffset: Integer; indices: TJavaArray<Char>; indicesOffset: Integer; indexCount: Integer): Integer; cdecl;
end;

[JavaSignature('android/opengl/Visibility')]
JVisibility = interface(JObject)
['{C7FA08CF-B60D-4208-BE44-C1A46B01B75D}']
end;
TJVisibility = class(TJavaGenericImport<JVisibilityClass, JVisibility>) end;

Jopengl_MatrixClass = interface(JObjectClass)
['{4EB67487-D94B-4FC1-95FA-26589B23C44C}']
  {Methods}
  function init: Jopengl_Matrix; cdecl;
  procedure frustumM(m: TJavaArray<Single>; offset: Integer; left: Single; right: Single; bottom: Single; top: Single; near: Single; far: Single); cdecl;
  function invertM(mInv: TJavaArray<Single>; mInvOffset: Integer; m: TJavaArray<Single>; mOffset: Integer): Boolean; cdecl;
  function length(x: Single; y: Single; z: Single): Single; cdecl;
  procedure multiplyMM(result: TJavaArray<Single>; resultOffset: Integer; lhs: TJavaArray<Single>; lhsOffset: Integer; rhs: TJavaArray<Single>; rhsOffset: Integer); cdecl;
  procedure multiplyMV(resultVec: TJavaArray<Single>; resultVecOffset: Integer; lhsMat: TJavaArray<Single>; lhsMatOffset: Integer; rhsVec: TJavaArray<Single>; rhsVecOffset: Integer); cdecl;
  procedure orthoM(m: TJavaArray<Single>; mOffset: Integer; left: Single; right: Single; bottom: Single; top: Single; near: Single; far: Single); cdecl;
  procedure perspectiveM(m: TJavaArray<Single>; offset: Integer; fovy: Single; aspect: Single; zNear: Single; zFar: Single); cdecl;
  procedure rotateM(rm: TJavaArray<Single>; rmOffset: Integer; m: TJavaArray<Single>; mOffset: Integer; a: Single; x: Single; y: Single; z: Single); cdecl; overload;
  procedure rotateM(m: TJavaArray<Single>; mOffset: Integer; a: Single; x: Single; y: Single; z: Single); cdecl; overload;
  procedure scaleM(sm: TJavaArray<Single>; smOffset: Integer; m: TJavaArray<Single>; mOffset: Integer; x: Single; y: Single; z: Single); cdecl; overload;
  procedure scaleM(m: TJavaArray<Single>; mOffset: Integer; x: Single; y: Single; z: Single); cdecl; overload;
  procedure setIdentityM(sm: TJavaArray<Single>; smOffset: Integer); cdecl;
  procedure setLookAtM(rm: TJavaArray<Single>; rmOffset: Integer; eyeX: Single; eyeY: Single; eyeZ: Single; centerX: Single; centerY: Single; centerZ: Single; upX: Single; upY: Single; upZ: Single); cdecl;
  procedure setRotateEulerM(rm: TJavaArray<Single>; rmOffset: Integer; x: Single; y: Single; z: Single); cdecl;
  procedure setRotateM(rm: TJavaArray<Single>; rmOffset: Integer; a: Single; x: Single; y: Single; z: Single); cdecl;
  procedure translateM(tm: TJavaArray<Single>; tmOffset: Integer; m: TJavaArray<Single>; mOffset: Integer; x: Single; y: Single; z: Single); cdecl; overload;
  procedure translateM(m: TJavaArray<Single>; mOffset: Integer; x: Single; y: Single; z: Single); cdecl; overload;
  procedure transposeM(mTrans: TJavaArray<Single>; mTransOffset: Integer; m: TJavaArray<Single>; mOffset: Integer); cdecl;
end;

[JavaSignature('android/opengl/Matrix')]
Jopengl_Matrix = interface(JObject)
['{CFB1A8B7-3B83-4397-8FFA-5E7E733D6107}']
end;
TJopengl_Matrix = class(TJavaGenericImport<Jopengl_MatrixClass, Jopengl_Matrix>) end;

JEGLObjectHandleClass = interface(JObjectClass)
['{FF7E34A3-9353-4DF9-9BD6-21DA955FB685}']
end;

[JavaSignature('android/opengl/EGLObjectHandle')]
JEGLObjectHandle = interface(JObject)
['{DDBE4C0D-FAFA-4488-9AF0-A0DA2288D4EE}']
  {Methods}
  function getHandle: Integer; cdecl;
  function hashCode: Integer; cdecl;
end;
TJEGLObjectHandle = class(TJavaGenericImport<JEGLObjectHandleClass, JEGLObjectHandle>) end;

Jopengl_EGLSurfaceClass = interface(JEGLObjectHandleClass)
['{16333FF4-C63D-4F38-B92E-C0C7BAD97E25}']
end;

[JavaSignature('android/opengl/EGLSurface')]
Jopengl_EGLSurface = interface(JEGLObjectHandle)
['{14FEC125-04A4-4B09-8C09-6B352317AB30}']
  {Methods}
  function equals(o: JObject): Boolean; cdecl;
end;
TJopengl_EGLSurface = class(TJavaGenericImport<Jopengl_EGLSurfaceClass, Jopengl_EGLSurface>) end;

Jopengl_EGLContextClass = interface(JEGLObjectHandleClass)
['{E3975386-B545-40C7-A1E4-2C27B7E78418}']
end;

[JavaSignature('android/opengl/EGLContext')]
Jopengl_EGLContext = interface(JEGLObjectHandle)
['{7878A9EA-E332-4F81-A941-38213B756F83}']
  {Methods}
  function equals(o: JObject): Boolean; cdecl;
end;
TJopengl_EGLContext = class(TJavaGenericImport<Jopengl_EGLContextClass, Jopengl_EGLContext>) end;

Jopengl_EGLDisplayClass = interface(JEGLObjectHandleClass)
['{88F3BA54-B00D-48F7-AF12-A0CB468AC554}']
end;

[JavaSignature('android/opengl/EGLDisplay')]
Jopengl_EGLDisplay = interface(JEGLObjectHandle)
['{04C41700-CE9C-472E-AD2B-F9BD85ED2F2F}']
  {Methods}
  function equals(o: JObject): Boolean; cdecl;
end;
TJopengl_EGLDisplay = class(TJavaGenericImport<Jopengl_EGLDisplayClass, Jopengl_EGLDisplay>) end;

JGLDebugHelperClass = interface(JObjectClass)
['{7E6A0223-471A-40A4-819C-F743E8C79B03}']
  {Property Methods}
  function _GetCONFIG_CHECK_GL_ERROR: Integer;
  function _GetCONFIG_CHECK_THREAD: Integer;
  function _GetCONFIG_LOG_ARGUMENT_NAMES: Integer;
  function _GetERROR_WRONG_THREAD: Integer;
  {Methods}
  function init: JGLDebugHelper; cdecl;
  {Properties}
  property CONFIG_CHECK_GL_ERROR: Integer read _GetCONFIG_CHECK_GL_ERROR;
  property CONFIG_CHECK_THREAD: Integer read _GetCONFIG_CHECK_THREAD;
  property CONFIG_LOG_ARGUMENT_NAMES: Integer read _GetCONFIG_LOG_ARGUMENT_NAMES;
  property ERROR_WRONG_THREAD: Integer read _GetERROR_WRONG_THREAD;
end;

[JavaSignature('android/opengl/GLDebugHelper')]
JGLDebugHelper = interface(JObject)
['{A639D925-310B-4DA9-AA69-984007F9B7CD}']
end;
TJGLDebugHelper = class(TJavaGenericImport<JGLDebugHelperClass, JGLDebugHelper>) end;

Jopengl_EGLConfigClass = interface(JEGLObjectHandleClass)
['{3822999D-D9EE-4F0F-BBB3-C0B5081A54C3}']
end;

[JavaSignature('android/opengl/EGLConfig')]
Jopengl_EGLConfig = interface(JEGLObjectHandle)
['{B821AC66-FC87-4B82-9F10-A1021B85AE65}']
  {Methods}
  function equals(o: JObject): Boolean; cdecl;
end;
TJopengl_EGLConfig = class(TJavaGenericImport<Jopengl_EGLConfigClass, Jopengl_EGLConfig>) end;

JGLSurfaceView_EGLContextFactoryClass = interface(IJavaClass)
['{A2A6C144-BFC6-42FB-A2F9-9ADB73B3FDA4}']
end;

[JavaSignature('android/opengl/GLSurfaceView$EGLContextFactory')]
JGLSurfaceView_EGLContextFactory = interface(IJavaInstance)
['{84229F89-FEA5-4E44-B2E0-F0D728C4A307}']
end;
TJGLSurfaceView_EGLContextFactory = class(TJavaGenericImport<JGLSurfaceView_EGLContextFactoryClass, JGLSurfaceView_EGLContextFactory>) end;

JGLSurfaceView_RendererClass = interface(IJavaClass)
['{217726BB-8C84-479F-B023-74178C68EC53}']
end;

[JavaSignature('android/opengl/GLSurfaceView$Renderer')]
JGLSurfaceView_Renderer = interface(IJavaInstance)
['{B2554C00-E9D5-49B0-9DFC-ED774863B255}']
end;
TJGLSurfaceView_Renderer = class(TJavaGenericImport<JGLSurfaceView_RendererClass, JGLSurfaceView_Renderer>) end;

JGLES11ExtClass = interface(JObjectClass)
['{67F44FDE-35A7-4E7D-A292-13B5A0A63992}']
  {Property Methods}
  function _GetGL_3DC_XY_AMD: Integer;
  function _GetGL_3DC_X_AMD: Integer;
  function _GetGL_ATC_RGBA_EXPLICIT_ALPHA_AMD: Integer;
  function _GetGL_ATC_RGBA_INTERPOLATED_ALPHA_AMD: Integer;
  function _GetGL_ATC_RGB_AMD: Integer;
  function _GetGL_BGRA: Integer;
  function _GetGL_BLEND_DST_ALPHA_OES: Integer;
  function _GetGL_BLEND_DST_RGB_OES: Integer;
  function _GetGL_BLEND_EQUATION_ALPHA_OES: Integer;
  function _GetGL_BLEND_EQUATION_OES: Integer;
  function _GetGL_BLEND_EQUATION_RGB_OES: Integer;
  function _GetGL_BLEND_SRC_ALPHA_OES: Integer;
  function _GetGL_BLEND_SRC_RGB_OES: Integer;
  function _GetGL_BUFFER_ACCESS_OES: Integer;
  function _GetGL_BUFFER_MAPPED_OES: Integer;
  function _GetGL_BUFFER_MAP_POINTER_OES: Integer;
  function _GetGL_COLOR_ATTACHMENT0_OES: Integer;
  function _GetGL_CURRENT_PALETTE_MATRIX_OES: Integer;
  function _GetGL_DECR_WRAP_OES: Integer;
  function _GetGL_DEPTH24_STENCIL8_OES: Integer;
  function _GetGL_DEPTH_ATTACHMENT_OES: Integer;
  function _GetGL_DEPTH_COMPONENT16_OES: Integer;
  function _GetGL_DEPTH_COMPONENT24_OES: Integer;
  function _GetGL_DEPTH_COMPONENT32_OES: Integer;
  function _GetGL_DEPTH_STENCIL_OES: Integer;
  function _GetGL_ETC1_RGB8_OES: Integer;
  function _GetGL_FIXED_OES: Integer;
  function _GetGL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME_OES: Integer;
  function _GetGL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE_OES: Integer;
  function _GetGL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE_OES: Integer;
  function _GetGL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL_OES: Integer;
  function _GetGL_FRAMEBUFFER_BINDING_OES: Integer;
  function _GetGL_FRAMEBUFFER_COMPLETE_OES: Integer;
  function _GetGL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_OES: Integer;
  function _GetGL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_OES: Integer;
  function _GetGL_FRAMEBUFFER_INCOMPLETE_FORMATS_OES: Integer;
  function _GetGL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_OES: Integer;
  function _GetGL_FRAMEBUFFER_OES: Integer;
  function _GetGL_FRAMEBUFFER_UNSUPPORTED_OES: Integer;
  function _GetGL_FUNC_ADD_OES: Integer;
  function _GetGL_FUNC_REVERSE_SUBTRACT_OES: Integer;
  function _GetGL_FUNC_SUBTRACT_OES: Integer;
  function _GetGL_INCR_WRAP_OES: Integer;
  function _GetGL_INVALID_FRAMEBUFFER_OPERATION_OES: Integer;
  function _GetGL_MATRIX_INDEX_ARRAY_BUFFER_BINDING_OES: Integer;
  function _GetGL_MATRIX_INDEX_ARRAY_OES: Integer;
  function _GetGL_MATRIX_INDEX_ARRAY_POINTER_OES: Integer;
  function _GetGL_MATRIX_INDEX_ARRAY_SIZE_OES: Integer;
  function _GetGL_MATRIX_INDEX_ARRAY_STRIDE_OES: Integer;
  function _GetGL_MATRIX_INDEX_ARRAY_TYPE_OES: Integer;
  function _GetGL_MATRIX_PALETTE_OES: Integer;
  function _GetGL_MAX_CUBE_MAP_TEXTURE_SIZE_OES: Integer;
  function _GetGL_MAX_PALETTE_MATRICES_OES: Integer;
  function _GetGL_MAX_RENDERBUFFER_SIZE_OES: Integer;
  function _GetGL_MAX_TEXTURE_MAX_ANISOTROPY_EXT: Integer;
  function _GetGL_MAX_VERTEX_UNITS_OES: Integer;
  function _GetGL_MIRRORED_REPEAT_OES: Integer;
  function _GetGL_MODELVIEW_MATRIX_FLOAT_AS_INT_BITS_OES: Integer;
  function _GetGL_NONE_OES: Integer;
  function _GetGL_NORMAL_MAP_OES: Integer;
  function _GetGL_PROJECTION_MATRIX_FLOAT_AS_INT_BITS_OES: Integer;
  function _GetGL_REFLECTION_MAP_OES: Integer;
  function _GetGL_RENDERBUFFER_ALPHA_SIZE_OES: Integer;
  function _GetGL_RENDERBUFFER_BINDING_OES: Integer;
  function _GetGL_RENDERBUFFER_BLUE_SIZE_OES: Integer;
  function _GetGL_RENDERBUFFER_DEPTH_SIZE_OES: Integer;
  function _GetGL_RENDERBUFFER_GREEN_SIZE_OES: Integer;
  function _GetGL_RENDERBUFFER_HEIGHT_OES: Integer;
  function _GetGL_RENDERBUFFER_INTERNAL_FORMAT_OES: Integer;
  function _GetGL_RENDERBUFFER_OES: Integer;
  function _GetGL_RENDERBUFFER_RED_SIZE_OES: Integer;
  function _GetGL_RENDERBUFFER_STENCIL_SIZE_OES: Integer;
  function _GetGL_RENDERBUFFER_WIDTH_OES: Integer;
  function _GetGL_REQUIRED_TEXTURE_IMAGE_UNITS_OES: Integer;
  function _GetGL_RGB565_OES: Integer;
  function _GetGL_RGB5_A1_OES: Integer;
  function _GetGL_RGB8_OES: Integer;
  function _GetGL_RGBA4_OES: Integer;
  function _GetGL_RGBA8_OES: Integer;
  function _GetGL_SAMPLER_EXTERNAL_OES: Integer;
  function _GetGL_STENCIL_ATTACHMENT_OES: Integer;
  function _GetGL_STENCIL_INDEX1_OES: Integer;
  function _GetGL_STENCIL_INDEX4_OES: Integer;
  function _GetGL_STENCIL_INDEX8_OES: Integer;
  function _GetGL_TEXTURE_BINDING_CUBE_MAP_OES: Integer;
  function _GetGL_TEXTURE_BINDING_EXTERNAL_OES: Integer;
  function _GetGL_TEXTURE_CROP_RECT_OES: Integer;
  function _GetGL_TEXTURE_CUBE_MAP_NEGATIVE_X_OES: Integer;
  function _GetGL_TEXTURE_CUBE_MAP_NEGATIVE_Y_OES: Integer;
  function _GetGL_TEXTURE_CUBE_MAP_NEGATIVE_Z_OES: Integer;
  function _GetGL_TEXTURE_CUBE_MAP_OES: Integer;
  function _GetGL_TEXTURE_CUBE_MAP_POSITIVE_X_OES: Integer;
  function _GetGL_TEXTURE_CUBE_MAP_POSITIVE_Y_OES: Integer;
  function _GetGL_TEXTURE_CUBE_MAP_POSITIVE_Z_OES: Integer;
  function _GetGL_TEXTURE_EXTERNAL_OES: Integer;
  function _GetGL_TEXTURE_GEN_MODE_OES: Integer;
  function _GetGL_TEXTURE_GEN_STR_OES: Integer;
  function _GetGL_TEXTURE_MATRIX_FLOAT_AS_INT_BITS_OES: Integer;
  function _GetGL_TEXTURE_MAX_ANISOTROPY_EXT: Integer;
  function _GetGL_UNSIGNED_INT_24_8_OES: Integer;
  function _GetGL_WEIGHT_ARRAY_BUFFER_BINDING_OES: Integer;
  function _GetGL_WEIGHT_ARRAY_OES: Integer;
  function _GetGL_WEIGHT_ARRAY_POINTER_OES: Integer;
  function _GetGL_WEIGHT_ARRAY_SIZE_OES: Integer;
  function _GetGL_WEIGHT_ARRAY_STRIDE_OES: Integer;
  function _GetGL_WEIGHT_ARRAY_TYPE_OES: Integer;
  function _GetGL_WRITE_ONLY_OES: Integer;
  {Methods}
  function init: JGLES11Ext; cdecl;
  procedure glAlphaFuncxOES(func: Integer; ref: Integer); cdecl;
  procedure glBindFramebufferOES(target: Integer; framebuffer: Integer); cdecl;
  procedure glBindRenderbufferOES(target: Integer; renderbuffer: Integer); cdecl;
  procedure glBlendEquationOES(mode: Integer); cdecl;
  procedure glBlendEquationSeparateOES(modeRGB: Integer; modeAlpha: Integer); cdecl;
  procedure glBlendFuncSeparateOES(srcRGB: Integer; dstRGB: Integer; srcAlpha: Integer; dstAlpha: Integer); cdecl;
  function glCheckFramebufferStatusOES(target: Integer): Integer; cdecl;
  procedure glClearColorxOES(red: Integer; green: Integer; blue: Integer; alpha: Integer); cdecl;
  procedure glClearDepthfOES(depth: Single); cdecl;
  procedure glClearDepthxOES(depth: Integer); cdecl;
  procedure glClipPlanefOES(plane: Integer; equation: TJavaArray<Single>; offset: Integer); cdecl;
  procedure glClipPlanexOES(plane: Integer; equation: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glColor4xOES(red: Integer; green: Integer; blue: Integer; alpha: Integer); cdecl;
  procedure glCurrentPaletteMatrixOES(matrixpaletteindex: Integer); cdecl;
  procedure glDeleteFramebuffersOES(n: Integer; framebuffers: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glDeleteRenderbuffersOES(n: Integer; renderbuffers: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glDepthRangefOES(zNear: Single; zFar: Single); cdecl;
  procedure glDepthRangexOES(zNear: Integer; zFar: Integer); cdecl;
  procedure glDrawTexfOES(x: Single; y: Single; z: Single; width: Single; height: Single); cdecl;
  procedure glDrawTexfvOES(coords: TJavaArray<Single>; offset: Integer); cdecl;
  procedure glDrawTexiOES(x: Integer; y: Integer; z: Integer; width: Integer; height: Integer); cdecl;
  procedure glDrawTexivOES(coords: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glDrawTexsOES(x: SmallInt; y: SmallInt; z: SmallInt; width: SmallInt; height: SmallInt); cdecl;
  procedure glDrawTexsvOES(coords: TJavaArray<SmallInt>; offset: Integer); cdecl;
  procedure glDrawTexxOES(x: Integer; y: Integer; z: Integer; width: Integer; height: Integer); cdecl;
  procedure glDrawTexxvOES(coords: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glFogxOES(pname: Integer; param: Integer); cdecl;
  procedure glFogxvOES(pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glFramebufferRenderbufferOES(target: Integer; attachment: Integer; renderbuffertarget: Integer; renderbuffer: Integer); cdecl;
  procedure glFramebufferTexture2DOES(target: Integer; attachment: Integer; textarget: Integer; texture: Integer; level: Integer); cdecl;
  procedure glFrustumfOES(left: Single; right: Single; bottom: Single; top: Single; zNear: Single; zFar: Single); cdecl;
  procedure glFrustumxOES(left: Integer; right: Integer; bottom: Integer; top: Integer; zNear: Integer; zFar: Integer); cdecl;
  procedure glGenFramebuffersOES(n: Integer; framebuffers: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glGenRenderbuffersOES(n: Integer; renderbuffers: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glGenerateMipmapOES(target: Integer); cdecl;
  procedure glGetClipPlanefOES(pname: Integer; eqn: TJavaArray<Single>; offset: Integer); cdecl;
  procedure glGetClipPlanexOES(pname: Integer; eqn: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glGetFixedvOES(pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glGetFramebufferAttachmentParameterivOES(target: Integer; attachment: Integer; pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glGetLightxvOES(light: Integer; pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glGetMaterialxvOES(face: Integer; pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glGetRenderbufferParameterivOES(target: Integer; pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glGetTexEnvxvOES(env: Integer; pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glGetTexGenfvOES(coord: Integer; pname: Integer; params: TJavaArray<Single>; offset: Integer); cdecl;
  procedure glGetTexGenivOES(coord: Integer; pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glGetTexGenxvOES(coord: Integer; pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glGetTexParameterxvOES(target: Integer; pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  function glIsFramebufferOES(framebuffer: Integer): Boolean; cdecl;
  function glIsRenderbufferOES(renderbuffer: Integer): Boolean; cdecl;
  procedure glLightModelxOES(pname: Integer; param: Integer); cdecl;
  procedure glLightModelxvOES(pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glLightxOES(light: Integer; pname: Integer; param: Integer); cdecl;
  procedure glLightxvOES(light: Integer; pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glLineWidthxOES(width: Integer); cdecl;
  procedure glLoadMatrixxOES(m: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glLoadPaletteFromModelViewMatrixOES; cdecl;
  procedure glMaterialxOES(face: Integer; pname: Integer; param: Integer); cdecl;
  procedure glMaterialxvOES(face: Integer; pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glMultMatrixxOES(m: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glMultiTexCoord4xOES(target: Integer; s: Integer; t: Integer; r: Integer; q: Integer); cdecl;
  procedure glNormal3xOES(nx: Integer; ny: Integer; nz: Integer); cdecl;
  procedure glOrthofOES(left: Single; right: Single; bottom: Single; top: Single; zNear: Single; zFar: Single); cdecl;
  procedure glOrthoxOES(left: Integer; right: Integer; bottom: Integer; top: Integer; zNear: Integer; zFar: Integer); cdecl;
  procedure glPointParameterxOES(pname: Integer; param: Integer); cdecl;
  procedure glPointParameterxvOES(pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glPointSizexOES(size: Integer); cdecl;
  procedure glPolygonOffsetxOES(factor: Integer; units: Integer); cdecl;
  procedure glRenderbufferStorageOES(target: Integer; internalformat: Integer; width: Integer; height: Integer); cdecl;
  procedure glRotatexOES(angle: Integer; x: Integer; y: Integer; z: Integer); cdecl;
  procedure glSampleCoveragexOES(value: Integer; invert: Boolean); cdecl;
  procedure glScalexOES(x: Integer; y: Integer; z: Integer); cdecl;
  procedure glTexEnvxOES(target: Integer; pname: Integer; param: Integer); cdecl;
  procedure glTexEnvxvOES(target: Integer; pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glTexGenfOES(coord: Integer; pname: Integer; param: Single); cdecl;
  procedure glTexGenfvOES(coord: Integer; pname: Integer; params: TJavaArray<Single>; offset: Integer); cdecl;
  procedure glTexGeniOES(coord: Integer; pname: Integer; param: Integer); cdecl;
  procedure glTexGenivOES(coord: Integer; pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glTexGenxOES(coord: Integer; pname: Integer; param: Integer); cdecl;
  procedure glTexGenxvOES(coord: Integer; pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glTexParameterxOES(target: Integer; pname: Integer; param: Integer); cdecl;
  procedure glTexParameterxvOES(target: Integer; pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glTranslatexOES(x: Integer; y: Integer; z: Integer); cdecl;
  {Properties}
  property GL_3DC_XY_AMD: Integer read _GetGL_3DC_XY_AMD;
  property GL_3DC_X_AMD: Integer read _GetGL_3DC_X_AMD;
  property GL_ATC_RGBA_EXPLICIT_ALPHA_AMD: Integer read _GetGL_ATC_RGBA_EXPLICIT_ALPHA_AMD;
  property GL_ATC_RGBA_INTERPOLATED_ALPHA_AMD: Integer read _GetGL_ATC_RGBA_INTERPOLATED_ALPHA_AMD;
  property GL_ATC_RGB_AMD: Integer read _GetGL_ATC_RGB_AMD;
  property GL_BGRA: Integer read _GetGL_BGRA;
  property GL_BLEND_DST_ALPHA_OES: Integer read _GetGL_BLEND_DST_ALPHA_OES;
  property GL_BLEND_DST_RGB_OES: Integer read _GetGL_BLEND_DST_RGB_OES;
  property GL_BLEND_EQUATION_ALPHA_OES: Integer read _GetGL_BLEND_EQUATION_ALPHA_OES;
  property GL_BLEND_EQUATION_OES: Integer read _GetGL_BLEND_EQUATION_OES;
  property GL_BLEND_EQUATION_RGB_OES: Integer read _GetGL_BLEND_EQUATION_RGB_OES;
  property GL_BLEND_SRC_ALPHA_OES: Integer read _GetGL_BLEND_SRC_ALPHA_OES;
  property GL_BLEND_SRC_RGB_OES: Integer read _GetGL_BLEND_SRC_RGB_OES;
  property GL_BUFFER_ACCESS_OES: Integer read _GetGL_BUFFER_ACCESS_OES;
  property GL_BUFFER_MAPPED_OES: Integer read _GetGL_BUFFER_MAPPED_OES;
  property GL_BUFFER_MAP_POINTER_OES: Integer read _GetGL_BUFFER_MAP_POINTER_OES;
  property GL_COLOR_ATTACHMENT0_OES: Integer read _GetGL_COLOR_ATTACHMENT0_OES;
  property GL_CURRENT_PALETTE_MATRIX_OES: Integer read _GetGL_CURRENT_PALETTE_MATRIX_OES;
  property GL_DECR_WRAP_OES: Integer read _GetGL_DECR_WRAP_OES;
  property GL_DEPTH24_STENCIL8_OES: Integer read _GetGL_DEPTH24_STENCIL8_OES;
  property GL_DEPTH_ATTACHMENT_OES: Integer read _GetGL_DEPTH_ATTACHMENT_OES;
  property GL_DEPTH_COMPONENT16_OES: Integer read _GetGL_DEPTH_COMPONENT16_OES;
  property GL_DEPTH_COMPONENT24_OES: Integer read _GetGL_DEPTH_COMPONENT24_OES;
  property GL_DEPTH_COMPONENT32_OES: Integer read _GetGL_DEPTH_COMPONENT32_OES;
  property GL_DEPTH_STENCIL_OES: Integer read _GetGL_DEPTH_STENCIL_OES;
  property GL_ETC1_RGB8_OES: Integer read _GetGL_ETC1_RGB8_OES;
  property GL_FIXED_OES: Integer read _GetGL_FIXED_OES;
  property GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME_OES: Integer read _GetGL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME_OES;
  property GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE_OES: Integer read _GetGL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE_OES;
  property GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE_OES: Integer read _GetGL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE_OES;
  property GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL_OES: Integer read _GetGL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL_OES;
  property GL_FRAMEBUFFER_BINDING_OES: Integer read _GetGL_FRAMEBUFFER_BINDING_OES;
  property GL_FRAMEBUFFER_COMPLETE_OES: Integer read _GetGL_FRAMEBUFFER_COMPLETE_OES;
  property GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_OES: Integer read _GetGL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_OES;
  property GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_OES: Integer read _GetGL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_OES;
  property GL_FRAMEBUFFER_INCOMPLETE_FORMATS_OES: Integer read _GetGL_FRAMEBUFFER_INCOMPLETE_FORMATS_OES;
  property GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_OES: Integer read _GetGL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_OES;
  property GL_FRAMEBUFFER_OES: Integer read _GetGL_FRAMEBUFFER_OES;
  property GL_FRAMEBUFFER_UNSUPPORTED_OES: Integer read _GetGL_FRAMEBUFFER_UNSUPPORTED_OES;
  property GL_FUNC_ADD_OES: Integer read _GetGL_FUNC_ADD_OES;
  property GL_FUNC_REVERSE_SUBTRACT_OES: Integer read _GetGL_FUNC_REVERSE_SUBTRACT_OES;
  property GL_FUNC_SUBTRACT_OES: Integer read _GetGL_FUNC_SUBTRACT_OES;
  property GL_INCR_WRAP_OES: Integer read _GetGL_INCR_WRAP_OES;
  property GL_INVALID_FRAMEBUFFER_OPERATION_OES: Integer read _GetGL_INVALID_FRAMEBUFFER_OPERATION_OES;
  property GL_MATRIX_INDEX_ARRAY_BUFFER_BINDING_OES: Integer read _GetGL_MATRIX_INDEX_ARRAY_BUFFER_BINDING_OES;
  property GL_MATRIX_INDEX_ARRAY_OES: Integer read _GetGL_MATRIX_INDEX_ARRAY_OES;
  property GL_MATRIX_INDEX_ARRAY_POINTER_OES: Integer read _GetGL_MATRIX_INDEX_ARRAY_POINTER_OES;
  property GL_MATRIX_INDEX_ARRAY_SIZE_OES: Integer read _GetGL_MATRIX_INDEX_ARRAY_SIZE_OES;
  property GL_MATRIX_INDEX_ARRAY_STRIDE_OES: Integer read _GetGL_MATRIX_INDEX_ARRAY_STRIDE_OES;
  property GL_MATRIX_INDEX_ARRAY_TYPE_OES: Integer read _GetGL_MATRIX_INDEX_ARRAY_TYPE_OES;
  property GL_MATRIX_PALETTE_OES: Integer read _GetGL_MATRIX_PALETTE_OES;
  property GL_MAX_CUBE_MAP_TEXTURE_SIZE_OES: Integer read _GetGL_MAX_CUBE_MAP_TEXTURE_SIZE_OES;
  property GL_MAX_PALETTE_MATRICES_OES: Integer read _GetGL_MAX_PALETTE_MATRICES_OES;
  property GL_MAX_RENDERBUFFER_SIZE_OES: Integer read _GetGL_MAX_RENDERBUFFER_SIZE_OES;
  property GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT: Integer read _GetGL_MAX_TEXTURE_MAX_ANISOTROPY_EXT;
  property GL_MAX_VERTEX_UNITS_OES: Integer read _GetGL_MAX_VERTEX_UNITS_OES;
  property GL_MIRRORED_REPEAT_OES: Integer read _GetGL_MIRRORED_REPEAT_OES;
  property GL_MODELVIEW_MATRIX_FLOAT_AS_INT_BITS_OES: Integer read _GetGL_MODELVIEW_MATRIX_FLOAT_AS_INT_BITS_OES;
  property GL_NONE_OES: Integer read _GetGL_NONE_OES;
  property GL_NORMAL_MAP_OES: Integer read _GetGL_NORMAL_MAP_OES;
  property GL_PROJECTION_MATRIX_FLOAT_AS_INT_BITS_OES: Integer read _GetGL_PROJECTION_MATRIX_FLOAT_AS_INT_BITS_OES;
  property GL_REFLECTION_MAP_OES: Integer read _GetGL_REFLECTION_MAP_OES;
  property GL_RENDERBUFFER_ALPHA_SIZE_OES: Integer read _GetGL_RENDERBUFFER_ALPHA_SIZE_OES;
  property GL_RENDERBUFFER_BINDING_OES: Integer read _GetGL_RENDERBUFFER_BINDING_OES;
  property GL_RENDERBUFFER_BLUE_SIZE_OES: Integer read _GetGL_RENDERBUFFER_BLUE_SIZE_OES;
  property GL_RENDERBUFFER_DEPTH_SIZE_OES: Integer read _GetGL_RENDERBUFFER_DEPTH_SIZE_OES;
  property GL_RENDERBUFFER_GREEN_SIZE_OES: Integer read _GetGL_RENDERBUFFER_GREEN_SIZE_OES;
  property GL_RENDERBUFFER_HEIGHT_OES: Integer read _GetGL_RENDERBUFFER_HEIGHT_OES;
  property GL_RENDERBUFFER_INTERNAL_FORMAT_OES: Integer read _GetGL_RENDERBUFFER_INTERNAL_FORMAT_OES;
  property GL_RENDERBUFFER_OES: Integer read _GetGL_RENDERBUFFER_OES;
  property GL_RENDERBUFFER_RED_SIZE_OES: Integer read _GetGL_RENDERBUFFER_RED_SIZE_OES;
  property GL_RENDERBUFFER_STENCIL_SIZE_OES: Integer read _GetGL_RENDERBUFFER_STENCIL_SIZE_OES;
  property GL_RENDERBUFFER_WIDTH_OES: Integer read _GetGL_RENDERBUFFER_WIDTH_OES;
  property GL_REQUIRED_TEXTURE_IMAGE_UNITS_OES: Integer read _GetGL_REQUIRED_TEXTURE_IMAGE_UNITS_OES;
  property GL_RGB565_OES: Integer read _GetGL_RGB565_OES;
  property GL_RGB5_A1_OES: Integer read _GetGL_RGB5_A1_OES;
  property GL_RGB8_OES: Integer read _GetGL_RGB8_OES;
  property GL_RGBA4_OES: Integer read _GetGL_RGBA4_OES;
  property GL_RGBA8_OES: Integer read _GetGL_RGBA8_OES;
  property GL_SAMPLER_EXTERNAL_OES: Integer read _GetGL_SAMPLER_EXTERNAL_OES;
  property GL_STENCIL_ATTACHMENT_OES: Integer read _GetGL_STENCIL_ATTACHMENT_OES;
  property GL_STENCIL_INDEX1_OES: Integer read _GetGL_STENCIL_INDEX1_OES;
  property GL_STENCIL_INDEX4_OES: Integer read _GetGL_STENCIL_INDEX4_OES;
  property GL_STENCIL_INDEX8_OES: Integer read _GetGL_STENCIL_INDEX8_OES;
  property GL_TEXTURE_BINDING_CUBE_MAP_OES: Integer read _GetGL_TEXTURE_BINDING_CUBE_MAP_OES;
  property GL_TEXTURE_BINDING_EXTERNAL_OES: Integer read _GetGL_TEXTURE_BINDING_EXTERNAL_OES;
  property GL_TEXTURE_CROP_RECT_OES: Integer read _GetGL_TEXTURE_CROP_RECT_OES;
  property GL_TEXTURE_CUBE_MAP_NEGATIVE_X_OES: Integer read _GetGL_TEXTURE_CUBE_MAP_NEGATIVE_X_OES;
  property GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_OES: Integer read _GetGL_TEXTURE_CUBE_MAP_NEGATIVE_Y_OES;
  property GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_OES: Integer read _GetGL_TEXTURE_CUBE_MAP_NEGATIVE_Z_OES;
  property GL_TEXTURE_CUBE_MAP_OES: Integer read _GetGL_TEXTURE_CUBE_MAP_OES;
  property GL_TEXTURE_CUBE_MAP_POSITIVE_X_OES: Integer read _GetGL_TEXTURE_CUBE_MAP_POSITIVE_X_OES;
  property GL_TEXTURE_CUBE_MAP_POSITIVE_Y_OES: Integer read _GetGL_TEXTURE_CUBE_MAP_POSITIVE_Y_OES;
  property GL_TEXTURE_CUBE_MAP_POSITIVE_Z_OES: Integer read _GetGL_TEXTURE_CUBE_MAP_POSITIVE_Z_OES;
  property GL_TEXTURE_EXTERNAL_OES: Integer read _GetGL_TEXTURE_EXTERNAL_OES;
  property GL_TEXTURE_GEN_MODE_OES: Integer read _GetGL_TEXTURE_GEN_MODE_OES;
  property GL_TEXTURE_GEN_STR_OES: Integer read _GetGL_TEXTURE_GEN_STR_OES;
  property GL_TEXTURE_MATRIX_FLOAT_AS_INT_BITS_OES: Integer read _GetGL_TEXTURE_MATRIX_FLOAT_AS_INT_BITS_OES;
  property GL_TEXTURE_MAX_ANISOTROPY_EXT: Integer read _GetGL_TEXTURE_MAX_ANISOTROPY_EXT;
  property GL_UNSIGNED_INT_24_8_OES: Integer read _GetGL_UNSIGNED_INT_24_8_OES;
  property GL_WEIGHT_ARRAY_BUFFER_BINDING_OES: Integer read _GetGL_WEIGHT_ARRAY_BUFFER_BINDING_OES;
  property GL_WEIGHT_ARRAY_OES: Integer read _GetGL_WEIGHT_ARRAY_OES;
  property GL_WEIGHT_ARRAY_POINTER_OES: Integer read _GetGL_WEIGHT_ARRAY_POINTER_OES;
  property GL_WEIGHT_ARRAY_SIZE_OES: Integer read _GetGL_WEIGHT_ARRAY_SIZE_OES;
  property GL_WEIGHT_ARRAY_STRIDE_OES: Integer read _GetGL_WEIGHT_ARRAY_STRIDE_OES;
  property GL_WEIGHT_ARRAY_TYPE_OES: Integer read _GetGL_WEIGHT_ARRAY_TYPE_OES;
  property GL_WRITE_ONLY_OES: Integer read _GetGL_WRITE_ONLY_OES;
end;

[JavaSignature('android/opengl/GLES11Ext')]
JGLES11Ext = interface(JObject)
['{82AC1A8D-82F5-4A70-93C3-A55C1CFA7E95}']
end;
TJGLES11Ext = class(TJavaGenericImport<JGLES11ExtClass, JGLES11Ext>) end;

JGLSurfaceView_EGLConfigChooserClass = interface(IJavaClass)
['{A50E2BAD-797D-4F11-AF7A-2F9B1E9D975B}']
end;

[JavaSignature('android/opengl/GLSurfaceView$EGLConfigChooser')]
JGLSurfaceView_EGLConfigChooser = interface(IJavaInstance)
['{34C89178-6D28-40CE-A5E4-20C433F5D871}']
end;
TJGLSurfaceView_EGLConfigChooser = class(TJavaGenericImport<JGLSurfaceView_EGLConfigChooserClass, JGLSurfaceView_EGLConfigChooser>) end;

JETC1Util_ETC1TextureClass = interface(JObjectClass)
['{4A43895C-EFD2-444A-8048-4F8DFF323CBD}']
end;

[JavaSignature('android/opengl/ETC1Util$ETC1Texture')]
JETC1Util_ETC1Texture = interface(JObject)
['{C53E11F2-9E67-4CC3-8D31-14F55D2E3A6F}']
  {Methods}
  function getHeight: Integer; cdecl;
  function getWidth: Integer; cdecl;
end;
TJETC1Util_ETC1Texture = class(TJavaGenericImport<JETC1Util_ETC1TextureClass, JETC1Util_ETC1Texture>) end;

JGLExceptionClass = interface(JRuntimeExceptionClass)
['{3125A88E-3ADF-4D9C-B145-B8AEB89658CD}']
  {Methods}
  function init(error: Integer): JGLException; cdecl; overload;
  function init(error: Integer; string_: JString): JGLException; cdecl; overload;
end;

[JavaSignature('android/opengl/GLException')]
JGLException = interface(JRuntimeException)
['{1779AF1E-76A0-4401-AA06-879864915F19}']
end;
TJGLException = class(TJavaGenericImport<JGLExceptionClass, JGLException>) end;

JGLES20Class = interface(JObjectClass)
['{C0AFF4E3-5F31-4377-9F58-C376F76D1CF4}']
  {Property Methods}
  function _GetGL_ACTIVE_ATTRIBUTES: Integer;
  function _GetGL_ACTIVE_ATTRIBUTE_MAX_LENGTH: Integer;
  function _GetGL_ACTIVE_TEXTURE: Integer;
  function _GetGL_ACTIVE_UNIFORMS: Integer;
  function _GetGL_ACTIVE_UNIFORM_MAX_LENGTH: Integer;
  function _GetGL_ALIASED_LINE_WIDTH_RANGE: Integer;
  function _GetGL_ALIASED_POINT_SIZE_RANGE: Integer;
  function _GetGL_ALPHA: Integer;
  function _GetGL_ALPHA_BITS: Integer;
  function _GetGL_ALWAYS: Integer;
  function _GetGL_ARRAY_BUFFER: Integer;
  function _GetGL_ARRAY_BUFFER_BINDING: Integer;
  function _GetGL_ATTACHED_SHADERS: Integer;
  function _GetGL_BACK: Integer;
  function _GetGL_BLEND: Integer;
  function _GetGL_BLEND_COLOR: Integer;
  function _GetGL_BLEND_DST_ALPHA: Integer;
  function _GetGL_BLEND_DST_RGB: Integer;
  function _GetGL_BLEND_EQUATION: Integer;
  function _GetGL_BLEND_EQUATION_ALPHA: Integer;
  function _GetGL_BLEND_EQUATION_RGB: Integer;
  function _GetGL_BLEND_SRC_ALPHA: Integer;
  function _GetGL_BLEND_SRC_RGB: Integer;
  function _GetGL_BLUE_BITS: Integer;
  function _GetGL_BOOL: Integer;
  function _GetGL_BOOL_VEC2: Integer;
  function _GetGL_BOOL_VEC3: Integer;
  function _GetGL_BOOL_VEC4: Integer;
  function _GetGL_BUFFER_SIZE: Integer;
  function _GetGL_BUFFER_USAGE: Integer;
  function _GetGL_BYTE: Integer;
  function _GetGL_CCW: Integer;
  function _GetGL_CLAMP_TO_EDGE: Integer;
  function _GetGL_COLOR_ATTACHMENT0: Integer;
  function _GetGL_COLOR_BUFFER_BIT: Integer;
  function _GetGL_COLOR_CLEAR_VALUE: Integer;
  function _GetGL_COLOR_WRITEMASK: Integer;
  function _GetGL_COMPILE_STATUS: Integer;
  function _GetGL_COMPRESSED_TEXTURE_FORMATS: Integer;
  function _GetGL_CONSTANT_ALPHA: Integer;
  function _GetGL_CONSTANT_COLOR: Integer;
  function _GetGL_CULL_FACE: Integer;
  function _GetGL_CULL_FACE_MODE: Integer;
  function _GetGL_CURRENT_PROGRAM: Integer;
  function _GetGL_CURRENT_VERTEX_ATTRIB: Integer;
  function _GetGL_CW: Integer;
  function _GetGL_DECR: Integer;
  function _GetGL_DECR_WRAP: Integer;
  function _GetGL_DELETE_STATUS: Integer;
  function _GetGL_DEPTH_ATTACHMENT: Integer;
  function _GetGL_DEPTH_BITS: Integer;
  function _GetGL_DEPTH_BUFFER_BIT: Integer;
  function _GetGL_DEPTH_CLEAR_VALUE: Integer;
  function _GetGL_DEPTH_COMPONENT: Integer;
  function _GetGL_DEPTH_COMPONENT16: Integer;
  function _GetGL_DEPTH_FUNC: Integer;
  function _GetGL_DEPTH_RANGE: Integer;
  function _GetGL_DEPTH_TEST: Integer;
  function _GetGL_DEPTH_WRITEMASK: Integer;
  function _GetGL_DITHER: Integer;
  function _GetGL_DONT_CARE: Integer;
  function _GetGL_DST_ALPHA: Integer;
  function _GetGL_DST_COLOR: Integer;
  function _GetGL_DYNAMIC_DRAW: Integer;
  function _GetGL_ELEMENT_ARRAY_BUFFER: Integer;
  function _GetGL_ELEMENT_ARRAY_BUFFER_BINDING: Integer;
  function _GetGL_EQUAL: Integer;
  function _GetGL_EXTENSIONS: Integer;
  function _GetGL_FALSE: Integer;
  function _GetGL_FASTEST: Integer;
  function _GetGL_FIXED: Integer;
  function _GetGL_FLOAT: Integer;
  function _GetGL_FLOAT_MAT2: Integer;
  function _GetGL_FLOAT_MAT3: Integer;
  function _GetGL_FLOAT_MAT4: Integer;
  function _GetGL_FLOAT_VEC2: Integer;
  function _GetGL_FLOAT_VEC3: Integer;
  function _GetGL_FLOAT_VEC4: Integer;
  function _GetGL_FRAGMENT_SHADER: Integer;
  function _GetGL_FRAMEBUFFER: Integer;
  function _GetGL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME: Integer;
  function _GetGL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE: Integer;
  function _GetGL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE: Integer;
  function _GetGL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL: Integer;
  function _GetGL_FRAMEBUFFER_BINDING: Integer;
  function _GetGL_FRAMEBUFFER_COMPLETE: Integer;
  function _GetGL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT: Integer;
  function _GetGL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS: Integer;
  function _GetGL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT: Integer;
  function _GetGL_FRAMEBUFFER_UNSUPPORTED: Integer;
  function _GetGL_FRONT: Integer;
  function _GetGL_FRONT_AND_BACK: Integer;
  function _GetGL_FRONT_FACE: Integer;
  function _GetGL_FUNC_ADD: Integer;
  function _GetGL_FUNC_REVERSE_SUBTRACT: Integer;
  function _GetGL_FUNC_SUBTRACT: Integer;
  function _GetGL_GENERATE_MIPMAP_HINT: Integer;
  function _GetGL_GEQUAL: Integer;
  function _GetGL_GREATER: Integer;
  function _GetGL_GREEN_BITS: Integer;
  function _GetGL_HIGH_FLOAT: Integer;
  function _GetGL_HIGH_INT: Integer;
  function _GetGL_IMPLEMENTATION_COLOR_READ_FORMAT: Integer;
  function _GetGL_IMPLEMENTATION_COLOR_READ_TYPE: Integer;
  function _GetGL_INCR: Integer;
  function _GetGL_INCR_WRAP: Integer;
  function _GetGL_INFO_LOG_LENGTH: Integer;
  function _GetGL_INT: Integer;
  function _GetGL_INT_VEC2: Integer;
  function _GetGL_INT_VEC3: Integer;
  function _GetGL_INT_VEC4: Integer;
  function _GetGL_INVALID_ENUM: Integer;
  function _GetGL_INVALID_FRAMEBUFFER_OPERATION: Integer;
  function _GetGL_INVALID_OPERATION: Integer;
  function _GetGL_INVALID_VALUE: Integer;
  function _GetGL_INVERT: Integer;
  function _GetGL_KEEP: Integer;
  function _GetGL_LEQUAL: Integer;
  function _GetGL_LESS: Integer;
  function _GetGL_LINEAR: Integer;
  function _GetGL_LINEAR_MIPMAP_LINEAR: Integer;
  function _GetGL_LINEAR_MIPMAP_NEAREST: Integer;
  function _GetGL_LINES: Integer;
  function _GetGL_LINE_LOOP: Integer;
  function _GetGL_LINE_STRIP: Integer;
  function _GetGL_LINE_WIDTH: Integer;
  function _GetGL_LINK_STATUS: Integer;
  function _GetGL_LOW_FLOAT: Integer;
  function _GetGL_LOW_INT: Integer;
  function _GetGL_LUMINANCE: Integer;
  function _GetGL_LUMINANCE_ALPHA: Integer;
  function _GetGL_MAX_COMBINED_TEXTURE_IMAGE_UNITS: Integer;
  function _GetGL_MAX_CUBE_MAP_TEXTURE_SIZE: Integer;
  function _GetGL_MAX_FRAGMENT_UNIFORM_VECTORS: Integer;
  function _GetGL_MAX_RENDERBUFFER_SIZE: Integer;
  function _GetGL_MAX_TEXTURE_IMAGE_UNITS: Integer;
  function _GetGL_MAX_TEXTURE_SIZE: Integer;
  function _GetGL_MAX_VARYING_VECTORS: Integer;
  function _GetGL_MAX_VERTEX_ATTRIBS: Integer;
  function _GetGL_MAX_VERTEX_TEXTURE_IMAGE_UNITS: Integer;
  function _GetGL_MAX_VERTEX_UNIFORM_VECTORS: Integer;
  function _GetGL_MAX_VIEWPORT_DIMS: Integer;
  function _GetGL_MEDIUM_FLOAT: Integer;
  function _GetGL_MEDIUM_INT: Integer;
  function _GetGL_MIRRORED_REPEAT: Integer;
  function _GetGL_NEAREST: Integer;
  function _GetGL_NEAREST_MIPMAP_LINEAR: Integer;
  function _GetGL_NEAREST_MIPMAP_NEAREST: Integer;
  function _GetGL_NEVER: Integer;
  function _GetGL_NICEST: Integer;
  function _GetGL_NONE: Integer;
  function _GetGL_NOTEQUAL: Integer;
  function _GetGL_NO_ERROR: Integer;
  function _GetGL_NUM_COMPRESSED_TEXTURE_FORMATS: Integer;
  function _GetGL_NUM_SHADER_BINARY_FORMATS: Integer;
  function _GetGL_ONE: Integer;
  function _GetGL_ONE_MINUS_CONSTANT_ALPHA: Integer;
  function _GetGL_ONE_MINUS_CONSTANT_COLOR: Integer;
  function _GetGL_ONE_MINUS_DST_ALPHA: Integer;
  function _GetGL_ONE_MINUS_DST_COLOR: Integer;
  function _GetGL_ONE_MINUS_SRC_ALPHA: Integer;
  function _GetGL_ONE_MINUS_SRC_COLOR: Integer;
  function _GetGL_OUT_OF_MEMORY: Integer;
  function _GetGL_PACK_ALIGNMENT: Integer;
  function _GetGL_POINTS: Integer;
  function _GetGL_POLYGON_OFFSET_FACTOR: Integer;
  function _GetGL_POLYGON_OFFSET_FILL: Integer;
  function _GetGL_POLYGON_OFFSET_UNITS: Integer;
  function _GetGL_RED_BITS: Integer;
  function _GetGL_RENDERBUFFER: Integer;
  function _GetGL_RENDERBUFFER_ALPHA_SIZE: Integer;
  function _GetGL_RENDERBUFFER_BINDING: Integer;
  function _GetGL_RENDERBUFFER_BLUE_SIZE: Integer;
  function _GetGL_RENDERBUFFER_DEPTH_SIZE: Integer;
  function _GetGL_RENDERBUFFER_GREEN_SIZE: Integer;
  function _GetGL_RENDERBUFFER_HEIGHT: Integer;
  function _GetGL_RENDERBUFFER_INTERNAL_FORMAT: Integer;
  function _GetGL_RENDERBUFFER_RED_SIZE: Integer;
  function _GetGL_RENDERBUFFER_STENCIL_SIZE: Integer;
  function _GetGL_RENDERBUFFER_WIDTH: Integer;
  function _GetGL_RENDERER: Integer;
  function _GetGL_REPEAT: Integer;
  function _GetGL_REPLACE: Integer;
  function _GetGL_RGB: Integer;
  function _GetGL_RGB565: Integer;
  function _GetGL_RGB5_A1: Integer;
  function _GetGL_RGBA: Integer;
  function _GetGL_RGBA4: Integer;
  function _GetGL_SAMPLER_2D: Integer;
  function _GetGL_SAMPLER_CUBE: Integer;
  function _GetGL_SAMPLES: Integer;
  function _GetGL_SAMPLE_ALPHA_TO_COVERAGE: Integer;
  function _GetGL_SAMPLE_BUFFERS: Integer;
  function _GetGL_SAMPLE_COVERAGE: Integer;
  function _GetGL_SAMPLE_COVERAGE_INVERT: Integer;
  function _GetGL_SAMPLE_COVERAGE_VALUE: Integer;
  function _GetGL_SCISSOR_BOX: Integer;
  function _GetGL_SCISSOR_TEST: Integer;
  function _GetGL_SHADER_BINARY_FORMATS: Integer;
  function _GetGL_SHADER_COMPILER: Integer;
  function _GetGL_SHADER_SOURCE_LENGTH: Integer;
  function _GetGL_SHADER_TYPE: Integer;
  function _GetGL_SHADING_LANGUAGE_VERSION: Integer;
  function _GetGL_SHORT: Integer;
  function _GetGL_SRC_ALPHA: Integer;
  function _GetGL_SRC_ALPHA_SATURATE: Integer;
  function _GetGL_SRC_COLOR: Integer;
  function _GetGL_STATIC_DRAW: Integer;
  function _GetGL_STENCIL_ATTACHMENT: Integer;
  function _GetGL_STENCIL_BACK_FAIL: Integer;
  function _GetGL_STENCIL_BACK_FUNC: Integer;
  function _GetGL_STENCIL_BACK_PASS_DEPTH_FAIL: Integer;
  function _GetGL_STENCIL_BACK_PASS_DEPTH_PASS: Integer;
  function _GetGL_STENCIL_BACK_REF: Integer;
  function _GetGL_STENCIL_BACK_VALUE_MASK: Integer;
  function _GetGL_STENCIL_BACK_WRITEMASK: Integer;
  function _GetGL_STENCIL_BITS: Integer;
  function _GetGL_STENCIL_BUFFER_BIT: Integer;
  function _GetGL_STENCIL_CLEAR_VALUE: Integer;
  function _GetGL_STENCIL_FAIL: Integer;
  function _GetGL_STENCIL_FUNC: Integer;
  function _GetGL_STENCIL_INDEX: Integer;
  function _GetGL_STENCIL_INDEX8: Integer;
  function _GetGL_STENCIL_PASS_DEPTH_FAIL: Integer;
  function _GetGL_STENCIL_PASS_DEPTH_PASS: Integer;
  function _GetGL_STENCIL_REF: Integer;
  function _GetGL_STENCIL_TEST: Integer;
  function _GetGL_STENCIL_VALUE_MASK: Integer;
  function _GetGL_STENCIL_WRITEMASK: Integer;
  function _GetGL_STREAM_DRAW: Integer;
  function _GetGL_SUBPIXEL_BITS: Integer;
  function _GetGL_TEXTURE: Integer;
  function _GetGL_TEXTURE0: Integer;
  function _GetGL_TEXTURE1: Integer;
  function _GetGL_TEXTURE10: Integer;
  function _GetGL_TEXTURE11: Integer;
  function _GetGL_TEXTURE12: Integer;
  function _GetGL_TEXTURE13: Integer;
  function _GetGL_TEXTURE14: Integer;
  function _GetGL_TEXTURE15: Integer;
  function _GetGL_TEXTURE16: Integer;
  function _GetGL_TEXTURE17: Integer;
  function _GetGL_TEXTURE18: Integer;
  function _GetGL_TEXTURE19: Integer;
  function _GetGL_TEXTURE2: Integer;
  function _GetGL_TEXTURE20: Integer;
  function _GetGL_TEXTURE21: Integer;
  function _GetGL_TEXTURE22: Integer;
  function _GetGL_TEXTURE23: Integer;
  function _GetGL_TEXTURE24: Integer;
  function _GetGL_TEXTURE25: Integer;
  function _GetGL_TEXTURE26: Integer;
  function _GetGL_TEXTURE27: Integer;
  function _GetGL_TEXTURE28: Integer;
  function _GetGL_TEXTURE29: Integer;
  function _GetGL_TEXTURE3: Integer;
  function _GetGL_TEXTURE30: Integer;
  function _GetGL_TEXTURE31: Integer;
  function _GetGL_TEXTURE4: Integer;
  function _GetGL_TEXTURE5: Integer;
  function _GetGL_TEXTURE6: Integer;
  function _GetGL_TEXTURE7: Integer;
  function _GetGL_TEXTURE8: Integer;
  function _GetGL_TEXTURE9: Integer;
  function _GetGL_TEXTURE_2D: Integer;
  function _GetGL_TEXTURE_BINDING_2D: Integer;
  function _GetGL_TEXTURE_BINDING_CUBE_MAP: Integer;
  function _GetGL_TEXTURE_CUBE_MAP: Integer;
  function _GetGL_TEXTURE_CUBE_MAP_NEGATIVE_X: Integer;
  function _GetGL_TEXTURE_CUBE_MAP_NEGATIVE_Y: Integer;
  function _GetGL_TEXTURE_CUBE_MAP_NEGATIVE_Z: Integer;
  function _GetGL_TEXTURE_CUBE_MAP_POSITIVE_X: Integer;
  function _GetGL_TEXTURE_CUBE_MAP_POSITIVE_Y: Integer;
  function _GetGL_TEXTURE_CUBE_MAP_POSITIVE_Z: Integer;
  function _GetGL_TEXTURE_MAG_FILTER: Integer;
  function _GetGL_TEXTURE_MIN_FILTER: Integer;
  function _GetGL_TEXTURE_WRAP_S: Integer;
  function _GetGL_TEXTURE_WRAP_T: Integer;
  function _GetGL_TRIANGLES: Integer;
  function _GetGL_TRIANGLE_FAN: Integer;
  function _GetGL_TRIANGLE_STRIP: Integer;
  function _GetGL_TRUE: Integer;
  function _GetGL_UNPACK_ALIGNMENT: Integer;
  function _GetGL_UNSIGNED_BYTE: Integer;
  function _GetGL_UNSIGNED_INT: Integer;
  function _GetGL_UNSIGNED_SHORT: Integer;
  function _GetGL_UNSIGNED_SHORT_4_4_4_4: Integer;
  function _GetGL_UNSIGNED_SHORT_5_5_5_1: Integer;
  function _GetGL_UNSIGNED_SHORT_5_6_5: Integer;
  function _GetGL_VALIDATE_STATUS: Integer;
  function _GetGL_VENDOR: Integer;
  function _GetGL_VERSION: Integer;
  function _GetGL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING: Integer;
  function _GetGL_VERTEX_ATTRIB_ARRAY_ENABLED: Integer;
  function _GetGL_VERTEX_ATTRIB_ARRAY_NORMALIZED: Integer;
  function _GetGL_VERTEX_ATTRIB_ARRAY_POINTER: Integer;
  function _GetGL_VERTEX_ATTRIB_ARRAY_SIZE: Integer;
  function _GetGL_VERTEX_ATTRIB_ARRAY_STRIDE: Integer;
  function _GetGL_VERTEX_ATTRIB_ARRAY_TYPE: Integer;
  function _GetGL_VERTEX_SHADER: Integer;
  function _GetGL_VIEWPORT: Integer;
  function _GetGL_ZERO: Integer;
  {Methods}
  function init: JGLES20; cdecl;
  procedure glActiveTexture(texture: Integer); cdecl;
  procedure glAttachShader(program_: Integer; shader: Integer); cdecl;
  procedure glBindAttribLocation(program_: Integer; index: Integer; name: JString); cdecl;
  procedure glBindBuffer(target: Integer; buffer: Integer); cdecl;
  procedure glBindFramebuffer(target: Integer; framebuffer: Integer); cdecl;
  procedure glBindRenderbuffer(target: Integer; renderbuffer: Integer); cdecl;
  procedure glBindTexture(target: Integer; texture: Integer); cdecl;
  procedure glBlendColor(red: Single; green: Single; blue: Single; alpha: Single); cdecl;
  procedure glBlendEquation(mode: Integer); cdecl;
  procedure glBlendEquationSeparate(modeRGB: Integer; modeAlpha: Integer); cdecl;
  procedure glBlendFunc(sfactor: Integer; dfactor: Integer); cdecl;
  procedure glBlendFuncSeparate(srcRGB: Integer; dstRGB: Integer; srcAlpha: Integer; dstAlpha: Integer); cdecl;
  function glCheckFramebufferStatus(target: Integer): Integer; cdecl;
  procedure glClear(mask: Integer); cdecl;
  procedure glClearColor(red: Single; green: Single; blue: Single; alpha: Single); cdecl;
  procedure glClearDepthf(depth: Single); cdecl;
  procedure glClearStencil(s: Integer); cdecl;
  procedure glColorMask(red: Boolean; green: Boolean; blue: Boolean; alpha: Boolean); cdecl;
  procedure glCompileShader(shader: Integer); cdecl;
  procedure glCopyTexImage2D(target: Integer; level: Integer; internalformat: Integer; x: Integer; y: Integer; width: Integer; height: Integer; border: Integer); cdecl;
  procedure glCopyTexSubImage2D(target: Integer; level: Integer; xoffset: Integer; yoffset: Integer; x: Integer; y: Integer; width: Integer; height: Integer); cdecl;
  function glCreateProgram: Integer; cdecl;
  function glCreateShader(type_: Integer): Integer; cdecl;
  procedure glCullFace(mode: Integer); cdecl;
  procedure glDeleteBuffers(n: Integer; buffers: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glDeleteFramebuffers(n: Integer; framebuffers: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glDeleteProgram(program_: Integer); cdecl;
  procedure glDeleteRenderbuffers(n: Integer; renderbuffers: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glDeleteShader(shader: Integer); cdecl;
  procedure glDeleteTextures(n: Integer; textures: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glDepthFunc(func: Integer); cdecl;
  procedure glDepthMask(flag: Boolean); cdecl;
  procedure glDepthRangef(zNear: Single; zFar: Single); cdecl;
  procedure glDetachShader(program_: Integer; shader: Integer); cdecl;
  procedure glDisable(cap: Integer); cdecl;
  procedure glDisableVertexAttribArray(index: Integer); cdecl;
  procedure glDrawArrays(mode: Integer; first: Integer; count: Integer); cdecl;
  procedure glDrawElements(mode: Integer; count: Integer; type_: Integer; offset: Integer); cdecl;
  procedure glEnable(cap: Integer); cdecl;
  procedure glEnableVertexAttribArray(index: Integer); cdecl;
  procedure glFinish; cdecl;
  procedure glFlush; cdecl;
  procedure glFramebufferRenderbuffer(target: Integer; attachment: Integer; renderbuffertarget: Integer; renderbuffer: Integer); cdecl;
  procedure glFramebufferTexture2D(target: Integer; attachment: Integer; textarget: Integer; texture: Integer; level: Integer); cdecl;
  procedure glFrontFace(mode: Integer); cdecl;
  procedure glGenBuffers(n: Integer; buffers: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glGenFramebuffers(n: Integer; framebuffers: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glGenRenderbuffers(n: Integer; renderbuffers: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glGenTextures(n: Integer; textures: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glGenerateMipmap(target: Integer); cdecl;
  procedure glGetActiveAttrib(program_: Integer; index: Integer; bufsize: Integer; length: TJavaArray<Integer>; lengthOffset: Integer; size: TJavaArray<Integer>; sizeOffset: Integer; type_: TJavaArray<Integer>; typeOffset: Integer; name: TJavaArray<Byte>; nameOffset: Integer); cdecl; overload;
  function glGetActiveAttrib(program_: Integer; index: Integer; size: TJavaArray<Integer>; sizeOffset: Integer; type_: TJavaArray<Integer>; typeOffset: Integer): JString; cdecl; overload;
  procedure glGetActiveUniform(program_: Integer; index: Integer; bufsize: Integer; length: TJavaArray<Integer>; lengthOffset: Integer; size: TJavaArray<Integer>; sizeOffset: Integer; type_: TJavaArray<Integer>; typeOffset: Integer; name: TJavaArray<Byte>; nameOffset: Integer); cdecl; overload;
  function glGetActiveUniform(program_: Integer; index: Integer; size: TJavaArray<Integer>; sizeOffset: Integer; type_: TJavaArray<Integer>; typeOffset: Integer): JString; cdecl; overload;
  procedure glGetAttachedShaders(program_: Integer; maxcount: Integer; count: TJavaArray<Integer>; countOffset: Integer; shaders: TJavaArray<Integer>; shadersOffset: Integer); cdecl;
  function glGetAttribLocation(program_: Integer; name: JString): Integer; cdecl;
  procedure glGetBooleanv(pname: Integer; params: TJavaArray<Boolean>; offset: Integer); cdecl;
  procedure glGetBufferParameteriv(target: Integer; pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  function glGetError: Integer; cdecl;
  procedure glGetFloatv(pname: Integer; params: TJavaArray<Single>; offset: Integer); cdecl;
  procedure glGetFramebufferAttachmentParameteriv(target: Integer; attachment: Integer; pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glGetIntegerv(pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  function glGetProgramInfoLog(program_: Integer): JString; cdecl;
  procedure glGetProgramiv(program_: Integer; pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glGetRenderbufferParameteriv(target: Integer; pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  function glGetShaderInfoLog(shader: Integer): JString; cdecl;
  procedure glGetShaderPrecisionFormat(shadertype: Integer; precisiontype: Integer; range: TJavaArray<Integer>; rangeOffset: Integer; precision: TJavaArray<Integer>; precisionOffset: Integer); cdecl;
  procedure glGetShaderSource(shader: Integer; bufsize: Integer; length: TJavaArray<Integer>; lengthOffset: Integer; source: TJavaArray<Byte>; sourceOffset: Integer); cdecl; overload;
  function glGetShaderSource(shader: Integer): JString; cdecl; overload;
  procedure glGetShaderiv(shader: Integer; pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  function glGetString(name: Integer): JString; cdecl;
  procedure glGetTexParameterfv(target: Integer; pname: Integer; params: TJavaArray<Single>; offset: Integer); cdecl;
  procedure glGetTexParameteriv(target: Integer; pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  function glGetUniformLocation(program_: Integer; name: JString): Integer; cdecl;
  procedure glGetUniformfv(program_: Integer; location: Integer; params: TJavaArray<Single>; offset: Integer); cdecl;
  procedure glGetUniformiv(program_: Integer; location: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glGetVertexAttribfv(index: Integer; pname: Integer; params: TJavaArray<Single>; offset: Integer); cdecl;
  procedure glGetVertexAttribiv(index: Integer; pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glHint(target: Integer; mode: Integer); cdecl;
  function glIsBuffer(buffer: Integer): Boolean; cdecl;
  function glIsEnabled(cap: Integer): Boolean; cdecl;
  function glIsFramebuffer(framebuffer: Integer): Boolean; cdecl;
  function glIsProgram(program_: Integer): Boolean; cdecl;
  function glIsRenderbuffer(renderbuffer: Integer): Boolean; cdecl;
  function glIsShader(shader: Integer): Boolean; cdecl;
  function glIsTexture(texture: Integer): Boolean; cdecl;
  procedure glLineWidth(width: Single); cdecl;
  procedure glLinkProgram(program_: Integer); cdecl;
  procedure glPixelStorei(pname: Integer; param: Integer); cdecl;
  procedure glPolygonOffset(factor: Single; units: Single); cdecl;
  procedure glReleaseShaderCompiler; cdecl;
  procedure glRenderbufferStorage(target: Integer; internalformat: Integer; width: Integer; height: Integer); cdecl;
  procedure glSampleCoverage(value: Single; invert: Boolean); cdecl;
  procedure glScissor(x: Integer; y: Integer; width: Integer; height: Integer); cdecl;
  procedure glShaderSource(shader: Integer; string_: JString); cdecl;
  procedure glStencilFunc(func: Integer; ref: Integer; mask: Integer); cdecl;
  procedure glStencilFuncSeparate(face: Integer; func: Integer; ref: Integer; mask: Integer); cdecl;
  procedure glStencilMask(mask: Integer); cdecl;
  procedure glStencilMaskSeparate(face: Integer; mask: Integer); cdecl;
  procedure glStencilOp(fail: Integer; zfail: Integer; zpass: Integer); cdecl;
  procedure glStencilOpSeparate(face: Integer; fail: Integer; zfail: Integer; zpass: Integer); cdecl;
  procedure glTexParameterf(target: Integer; pname: Integer; param: Single); cdecl;
  procedure glTexParameterfv(target: Integer; pname: Integer; params: TJavaArray<Single>; offset: Integer); cdecl;
  procedure glTexParameteri(target: Integer; pname: Integer; param: Integer); cdecl;
  procedure glTexParameteriv(target: Integer; pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glUniform1f(location: Integer; x: Single); cdecl;
  procedure glUniform1fv(location: Integer; count: Integer; v: TJavaArray<Single>; offset: Integer); cdecl;
  procedure glUniform1i(location: Integer; x: Integer); cdecl;
  procedure glUniform1iv(location: Integer; count: Integer; v: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glUniform2f(location: Integer; x: Single; y: Single); cdecl;
  procedure glUniform2fv(location: Integer; count: Integer; v: TJavaArray<Single>; offset: Integer); cdecl;
  procedure glUniform2i(location: Integer; x: Integer; y: Integer); cdecl;
  procedure glUniform2iv(location: Integer; count: Integer; v: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glUniform3f(location: Integer; x: Single; y: Single; z: Single); cdecl;
  procedure glUniform3fv(location: Integer; count: Integer; v: TJavaArray<Single>; offset: Integer); cdecl;
  procedure glUniform3i(location: Integer; x: Integer; y: Integer; z: Integer); cdecl;
  procedure glUniform3iv(location: Integer; count: Integer; v: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glUniform4f(location: Integer; x: Single; y: Single; z: Single; w: Single); cdecl;
  procedure glUniform4fv(location: Integer; count: Integer; v: TJavaArray<Single>; offset: Integer); cdecl;
  procedure glUniform4i(location: Integer; x: Integer; y: Integer; z: Integer; w: Integer); cdecl;
  procedure glUniform4iv(location: Integer; count: Integer; v: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glUniformMatrix2fv(location: Integer; count: Integer; transpose: Boolean; value: TJavaArray<Single>; offset: Integer); cdecl;
  procedure glUniformMatrix3fv(location: Integer; count: Integer; transpose: Boolean; value: TJavaArray<Single>; offset: Integer); cdecl;
  procedure glUniformMatrix4fv(location: Integer; count: Integer; transpose: Boolean; value: TJavaArray<Single>; offset: Integer); cdecl;
  procedure glUseProgram(program_: Integer); cdecl;
  procedure glValidateProgram(program_: Integer); cdecl;
  procedure glVertexAttrib1f(indx: Integer; x: Single); cdecl;
  procedure glVertexAttrib1fv(indx: Integer; values: TJavaArray<Single>; offset: Integer); cdecl;
  procedure glVertexAttrib2f(indx: Integer; x: Single; y: Single); cdecl;
  procedure glVertexAttrib2fv(indx: Integer; values: TJavaArray<Single>; offset: Integer); cdecl;
  procedure glVertexAttrib3f(indx: Integer; x: Single; y: Single; z: Single); cdecl;
  procedure glVertexAttrib3fv(indx: Integer; values: TJavaArray<Single>; offset: Integer); cdecl;
  procedure glVertexAttrib4f(indx: Integer; x: Single; y: Single; z: Single; w: Single); cdecl;
  procedure glVertexAttrib4fv(indx: Integer; values: TJavaArray<Single>; offset: Integer); cdecl;
  procedure glVertexAttribPointer(indx: Integer; size: Integer; type_: Integer; normalized: Boolean; stride: Integer; offset: Integer); cdecl;
  procedure glViewport(x: Integer; y: Integer; width: Integer; height: Integer); cdecl;
  {Properties}
  property GL_ACTIVE_ATTRIBUTES: Integer read _GetGL_ACTIVE_ATTRIBUTES;
  property GL_ACTIVE_ATTRIBUTE_MAX_LENGTH: Integer read _GetGL_ACTIVE_ATTRIBUTE_MAX_LENGTH;
  property GL_ACTIVE_TEXTURE: Integer read _GetGL_ACTIVE_TEXTURE;
  property GL_ACTIVE_UNIFORMS: Integer read _GetGL_ACTIVE_UNIFORMS;
  property GL_ACTIVE_UNIFORM_MAX_LENGTH: Integer read _GetGL_ACTIVE_UNIFORM_MAX_LENGTH;
  property GL_ALIASED_LINE_WIDTH_RANGE: Integer read _GetGL_ALIASED_LINE_WIDTH_RANGE;
  property GL_ALIASED_POINT_SIZE_RANGE: Integer read _GetGL_ALIASED_POINT_SIZE_RANGE;
  property GL_ALPHA: Integer read _GetGL_ALPHA;
  property GL_ALPHA_BITS: Integer read _GetGL_ALPHA_BITS;
  property GL_ALWAYS: Integer read _GetGL_ALWAYS;
  property GL_ARRAY_BUFFER: Integer read _GetGL_ARRAY_BUFFER;
  property GL_ARRAY_BUFFER_BINDING: Integer read _GetGL_ARRAY_BUFFER_BINDING;
  property GL_ATTACHED_SHADERS: Integer read _GetGL_ATTACHED_SHADERS;
  property GL_BACK: Integer read _GetGL_BACK;
  property GL_BLEND: Integer read _GetGL_BLEND;
  property GL_BLEND_COLOR: Integer read _GetGL_BLEND_COLOR;
  property GL_BLEND_DST_ALPHA: Integer read _GetGL_BLEND_DST_ALPHA;
  property GL_BLEND_DST_RGB: Integer read _GetGL_BLEND_DST_RGB;
  property GL_BLEND_EQUATION: Integer read _GetGL_BLEND_EQUATION;
  property GL_BLEND_EQUATION_ALPHA: Integer read _GetGL_BLEND_EQUATION_ALPHA;
  property GL_BLEND_EQUATION_RGB: Integer read _GetGL_BLEND_EQUATION_RGB;
  property GL_BLEND_SRC_ALPHA: Integer read _GetGL_BLEND_SRC_ALPHA;
  property GL_BLEND_SRC_RGB: Integer read _GetGL_BLEND_SRC_RGB;
  property GL_BLUE_BITS: Integer read _GetGL_BLUE_BITS;
  property GL_BOOL: Integer read _GetGL_BOOL;
  property GL_BOOL_VEC2: Integer read _GetGL_BOOL_VEC2;
  property GL_BOOL_VEC3: Integer read _GetGL_BOOL_VEC3;
  property GL_BOOL_VEC4: Integer read _GetGL_BOOL_VEC4;
  property GL_BUFFER_SIZE: Integer read _GetGL_BUFFER_SIZE;
  property GL_BUFFER_USAGE: Integer read _GetGL_BUFFER_USAGE;
  property GL_BYTE: Integer read _GetGL_BYTE;
  property GL_CCW: Integer read _GetGL_CCW;
  property GL_CLAMP_TO_EDGE: Integer read _GetGL_CLAMP_TO_EDGE;
  property GL_COLOR_ATTACHMENT0: Integer read _GetGL_COLOR_ATTACHMENT0;
  property GL_COLOR_BUFFER_BIT: Integer read _GetGL_COLOR_BUFFER_BIT;
  property GL_COLOR_CLEAR_VALUE: Integer read _GetGL_COLOR_CLEAR_VALUE;
  property GL_COLOR_WRITEMASK: Integer read _GetGL_COLOR_WRITEMASK;
  property GL_COMPILE_STATUS: Integer read _GetGL_COMPILE_STATUS;
  property GL_COMPRESSED_TEXTURE_FORMATS: Integer read _GetGL_COMPRESSED_TEXTURE_FORMATS;
  property GL_CONSTANT_ALPHA: Integer read _GetGL_CONSTANT_ALPHA;
  property GL_CONSTANT_COLOR: Integer read _GetGL_CONSTANT_COLOR;
  property GL_CULL_FACE: Integer read _GetGL_CULL_FACE;
  property GL_CULL_FACE_MODE: Integer read _GetGL_CULL_FACE_MODE;
  property GL_CURRENT_PROGRAM: Integer read _GetGL_CURRENT_PROGRAM;
  property GL_CURRENT_VERTEX_ATTRIB: Integer read _GetGL_CURRENT_VERTEX_ATTRIB;
  property GL_CW: Integer read _GetGL_CW;
  property GL_DECR: Integer read _GetGL_DECR;
  property GL_DECR_WRAP: Integer read _GetGL_DECR_WRAP;
  property GL_DELETE_STATUS: Integer read _GetGL_DELETE_STATUS;
  property GL_DEPTH_ATTACHMENT: Integer read _GetGL_DEPTH_ATTACHMENT;
  property GL_DEPTH_BITS: Integer read _GetGL_DEPTH_BITS;
  property GL_DEPTH_BUFFER_BIT: Integer read _GetGL_DEPTH_BUFFER_BIT;
  property GL_DEPTH_CLEAR_VALUE: Integer read _GetGL_DEPTH_CLEAR_VALUE;
  property GL_DEPTH_COMPONENT: Integer read _GetGL_DEPTH_COMPONENT;
  property GL_DEPTH_COMPONENT16: Integer read _GetGL_DEPTH_COMPONENT16;
  property GL_DEPTH_FUNC: Integer read _GetGL_DEPTH_FUNC;
  property GL_DEPTH_RANGE: Integer read _GetGL_DEPTH_RANGE;
  property GL_DEPTH_TEST: Integer read _GetGL_DEPTH_TEST;
  property GL_DEPTH_WRITEMASK: Integer read _GetGL_DEPTH_WRITEMASK;
  property GL_DITHER: Integer read _GetGL_DITHER;
  property GL_DONT_CARE: Integer read _GetGL_DONT_CARE;
  property GL_DST_ALPHA: Integer read _GetGL_DST_ALPHA;
  property GL_DST_COLOR: Integer read _GetGL_DST_COLOR;
  property GL_DYNAMIC_DRAW: Integer read _GetGL_DYNAMIC_DRAW;
  property GL_ELEMENT_ARRAY_BUFFER: Integer read _GetGL_ELEMENT_ARRAY_BUFFER;
  property GL_ELEMENT_ARRAY_BUFFER_BINDING: Integer read _GetGL_ELEMENT_ARRAY_BUFFER_BINDING;
  property GL_EQUAL: Integer read _GetGL_EQUAL;
  property GL_EXTENSIONS: Integer read _GetGL_EXTENSIONS;
  property GL_FALSE: Integer read _GetGL_FALSE;
  property GL_FASTEST: Integer read _GetGL_FASTEST;
  property GL_FIXED: Integer read _GetGL_FIXED;
  property GL_FLOAT: Integer read _GetGL_FLOAT;
  property GL_FLOAT_MAT2: Integer read _GetGL_FLOAT_MAT2;
  property GL_FLOAT_MAT3: Integer read _GetGL_FLOAT_MAT3;
  property GL_FLOAT_MAT4: Integer read _GetGL_FLOAT_MAT4;
  property GL_FLOAT_VEC2: Integer read _GetGL_FLOAT_VEC2;
  property GL_FLOAT_VEC3: Integer read _GetGL_FLOAT_VEC3;
  property GL_FLOAT_VEC4: Integer read _GetGL_FLOAT_VEC4;
  property GL_FRAGMENT_SHADER: Integer read _GetGL_FRAGMENT_SHADER;
  property GL_FRAMEBUFFER: Integer read _GetGL_FRAMEBUFFER;
  property GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME: Integer read _GetGL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME;
  property GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE: Integer read _GetGL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE;
  property GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE: Integer read _GetGL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE;
  property GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL: Integer read _GetGL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL;
  property GL_FRAMEBUFFER_BINDING: Integer read _GetGL_FRAMEBUFFER_BINDING;
  property GL_FRAMEBUFFER_COMPLETE: Integer read _GetGL_FRAMEBUFFER_COMPLETE;
  property GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT: Integer read _GetGL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT;
  property GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS: Integer read _GetGL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS;
  property GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT: Integer read _GetGL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT;
  property GL_FRAMEBUFFER_UNSUPPORTED: Integer read _GetGL_FRAMEBUFFER_UNSUPPORTED;
  property GL_FRONT: Integer read _GetGL_FRONT;
  property GL_FRONT_AND_BACK: Integer read _GetGL_FRONT_AND_BACK;
  property GL_FRONT_FACE: Integer read _GetGL_FRONT_FACE;
  property GL_FUNC_ADD: Integer read _GetGL_FUNC_ADD;
  property GL_FUNC_REVERSE_SUBTRACT: Integer read _GetGL_FUNC_REVERSE_SUBTRACT;
  property GL_FUNC_SUBTRACT: Integer read _GetGL_FUNC_SUBTRACT;
  property GL_GENERATE_MIPMAP_HINT: Integer read _GetGL_GENERATE_MIPMAP_HINT;
  property GL_GEQUAL: Integer read _GetGL_GEQUAL;
  property GL_GREATER: Integer read _GetGL_GREATER;
  property GL_GREEN_BITS: Integer read _GetGL_GREEN_BITS;
  property GL_HIGH_FLOAT: Integer read _GetGL_HIGH_FLOAT;
  property GL_HIGH_INT: Integer read _GetGL_HIGH_INT;
  property GL_IMPLEMENTATION_COLOR_READ_FORMAT: Integer read _GetGL_IMPLEMENTATION_COLOR_READ_FORMAT;
  property GL_IMPLEMENTATION_COLOR_READ_TYPE: Integer read _GetGL_IMPLEMENTATION_COLOR_READ_TYPE;
  property GL_INCR: Integer read _GetGL_INCR;
  property GL_INCR_WRAP: Integer read _GetGL_INCR_WRAP;
  property GL_INFO_LOG_LENGTH: Integer read _GetGL_INFO_LOG_LENGTH;
  property GL_INT: Integer read _GetGL_INT;
  property GL_INT_VEC2: Integer read _GetGL_INT_VEC2;
  property GL_INT_VEC3: Integer read _GetGL_INT_VEC3;
  property GL_INT_VEC4: Integer read _GetGL_INT_VEC4;
  property GL_INVALID_ENUM: Integer read _GetGL_INVALID_ENUM;
  property GL_INVALID_FRAMEBUFFER_OPERATION: Integer read _GetGL_INVALID_FRAMEBUFFER_OPERATION;
  property GL_INVALID_OPERATION: Integer read _GetGL_INVALID_OPERATION;
  property GL_INVALID_VALUE: Integer read _GetGL_INVALID_VALUE;
  property GL_INVERT: Integer read _GetGL_INVERT;
  property GL_KEEP: Integer read _GetGL_KEEP;
  property GL_LEQUAL: Integer read _GetGL_LEQUAL;
  property GL_LESS: Integer read _GetGL_LESS;
  property GL_LINEAR: Integer read _GetGL_LINEAR;
  property GL_LINEAR_MIPMAP_LINEAR: Integer read _GetGL_LINEAR_MIPMAP_LINEAR;
  property GL_LINEAR_MIPMAP_NEAREST: Integer read _GetGL_LINEAR_MIPMAP_NEAREST;
  property GL_LINES: Integer read _GetGL_LINES;
  property GL_LINE_LOOP: Integer read _GetGL_LINE_LOOP;
  property GL_LINE_STRIP: Integer read _GetGL_LINE_STRIP;
  property GL_LINE_WIDTH: Integer read _GetGL_LINE_WIDTH;
  property GL_LINK_STATUS: Integer read _GetGL_LINK_STATUS;
  property GL_LOW_FLOAT: Integer read _GetGL_LOW_FLOAT;
  property GL_LOW_INT: Integer read _GetGL_LOW_INT;
  property GL_LUMINANCE: Integer read _GetGL_LUMINANCE;
  property GL_LUMINANCE_ALPHA: Integer read _GetGL_LUMINANCE_ALPHA;
  property GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS: Integer read _GetGL_MAX_COMBINED_TEXTURE_IMAGE_UNITS;
  property GL_MAX_CUBE_MAP_TEXTURE_SIZE: Integer read _GetGL_MAX_CUBE_MAP_TEXTURE_SIZE;
  property GL_MAX_FRAGMENT_UNIFORM_VECTORS: Integer read _GetGL_MAX_FRAGMENT_UNIFORM_VECTORS;
  property GL_MAX_RENDERBUFFER_SIZE: Integer read _GetGL_MAX_RENDERBUFFER_SIZE;
  property GL_MAX_TEXTURE_IMAGE_UNITS: Integer read _GetGL_MAX_TEXTURE_IMAGE_UNITS;
  property GL_MAX_TEXTURE_SIZE: Integer read _GetGL_MAX_TEXTURE_SIZE;
  property GL_MAX_VARYING_VECTORS: Integer read _GetGL_MAX_VARYING_VECTORS;
  property GL_MAX_VERTEX_ATTRIBS: Integer read _GetGL_MAX_VERTEX_ATTRIBS;
  property GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS: Integer read _GetGL_MAX_VERTEX_TEXTURE_IMAGE_UNITS;
  property GL_MAX_VERTEX_UNIFORM_VECTORS: Integer read _GetGL_MAX_VERTEX_UNIFORM_VECTORS;
  property GL_MAX_VIEWPORT_DIMS: Integer read _GetGL_MAX_VIEWPORT_DIMS;
  property GL_MEDIUM_FLOAT: Integer read _GetGL_MEDIUM_FLOAT;
  property GL_MEDIUM_INT: Integer read _GetGL_MEDIUM_INT;
  property GL_MIRRORED_REPEAT: Integer read _GetGL_MIRRORED_REPEAT;
  property GL_NEAREST: Integer read _GetGL_NEAREST;
  property GL_NEAREST_MIPMAP_LINEAR: Integer read _GetGL_NEAREST_MIPMAP_LINEAR;
  property GL_NEAREST_MIPMAP_NEAREST: Integer read _GetGL_NEAREST_MIPMAP_NEAREST;
  property GL_NEVER: Integer read _GetGL_NEVER;
  property GL_NICEST: Integer read _GetGL_NICEST;
  property GL_NONE: Integer read _GetGL_NONE;
  property GL_NOTEQUAL: Integer read _GetGL_NOTEQUAL;
  property GL_NO_ERROR: Integer read _GetGL_NO_ERROR;
  property GL_NUM_COMPRESSED_TEXTURE_FORMATS: Integer read _GetGL_NUM_COMPRESSED_TEXTURE_FORMATS;
  property GL_NUM_SHADER_BINARY_FORMATS: Integer read _GetGL_NUM_SHADER_BINARY_FORMATS;
  property GL_ONE: Integer read _GetGL_ONE;
  property GL_ONE_MINUS_CONSTANT_ALPHA: Integer read _GetGL_ONE_MINUS_CONSTANT_ALPHA;
  property GL_ONE_MINUS_CONSTANT_COLOR: Integer read _GetGL_ONE_MINUS_CONSTANT_COLOR;
  property GL_ONE_MINUS_DST_ALPHA: Integer read _GetGL_ONE_MINUS_DST_ALPHA;
  property GL_ONE_MINUS_DST_COLOR: Integer read _GetGL_ONE_MINUS_DST_COLOR;
  property GL_ONE_MINUS_SRC_ALPHA: Integer read _GetGL_ONE_MINUS_SRC_ALPHA;
  property GL_ONE_MINUS_SRC_COLOR: Integer read _GetGL_ONE_MINUS_SRC_COLOR;
  property GL_OUT_OF_MEMORY: Integer read _GetGL_OUT_OF_MEMORY;
  property GL_PACK_ALIGNMENT: Integer read _GetGL_PACK_ALIGNMENT;
  property GL_POINTS: Integer read _GetGL_POINTS;
  property GL_POLYGON_OFFSET_FACTOR: Integer read _GetGL_POLYGON_OFFSET_FACTOR;
  property GL_POLYGON_OFFSET_FILL: Integer read _GetGL_POLYGON_OFFSET_FILL;
  property GL_POLYGON_OFFSET_UNITS: Integer read _GetGL_POLYGON_OFFSET_UNITS;
  property GL_RED_BITS: Integer read _GetGL_RED_BITS;
  property GL_RENDERBUFFER: Integer read _GetGL_RENDERBUFFER;
  property GL_RENDERBUFFER_ALPHA_SIZE: Integer read _GetGL_RENDERBUFFER_ALPHA_SIZE;
  property GL_RENDERBUFFER_BINDING: Integer read _GetGL_RENDERBUFFER_BINDING;
  property GL_RENDERBUFFER_BLUE_SIZE: Integer read _GetGL_RENDERBUFFER_BLUE_SIZE;
  property GL_RENDERBUFFER_DEPTH_SIZE: Integer read _GetGL_RENDERBUFFER_DEPTH_SIZE;
  property GL_RENDERBUFFER_GREEN_SIZE: Integer read _GetGL_RENDERBUFFER_GREEN_SIZE;
  property GL_RENDERBUFFER_HEIGHT: Integer read _GetGL_RENDERBUFFER_HEIGHT;
  property GL_RENDERBUFFER_INTERNAL_FORMAT: Integer read _GetGL_RENDERBUFFER_INTERNAL_FORMAT;
  property GL_RENDERBUFFER_RED_SIZE: Integer read _GetGL_RENDERBUFFER_RED_SIZE;
  property GL_RENDERBUFFER_STENCIL_SIZE: Integer read _GetGL_RENDERBUFFER_STENCIL_SIZE;
  property GL_RENDERBUFFER_WIDTH: Integer read _GetGL_RENDERBUFFER_WIDTH;
  property GL_RENDERER: Integer read _GetGL_RENDERER;
  property GL_REPEAT: Integer read _GetGL_REPEAT;
  property GL_REPLACE: Integer read _GetGL_REPLACE;
  property GL_RGB: Integer read _GetGL_RGB;
  property GL_RGB565: Integer read _GetGL_RGB565;
  property GL_RGB5_A1: Integer read _GetGL_RGB5_A1;
  property GL_RGBA: Integer read _GetGL_RGBA;
  property GL_RGBA4: Integer read _GetGL_RGBA4;
  property GL_SAMPLER_2D: Integer read _GetGL_SAMPLER_2D;
  property GL_SAMPLER_CUBE: Integer read _GetGL_SAMPLER_CUBE;
  property GL_SAMPLES: Integer read _GetGL_SAMPLES;
  property GL_SAMPLE_ALPHA_TO_COVERAGE: Integer read _GetGL_SAMPLE_ALPHA_TO_COVERAGE;
  property GL_SAMPLE_BUFFERS: Integer read _GetGL_SAMPLE_BUFFERS;
  property GL_SAMPLE_COVERAGE: Integer read _GetGL_SAMPLE_COVERAGE;
  property GL_SAMPLE_COVERAGE_INVERT: Integer read _GetGL_SAMPLE_COVERAGE_INVERT;
  property GL_SAMPLE_COVERAGE_VALUE: Integer read _GetGL_SAMPLE_COVERAGE_VALUE;
  property GL_SCISSOR_BOX: Integer read _GetGL_SCISSOR_BOX;
  property GL_SCISSOR_TEST: Integer read _GetGL_SCISSOR_TEST;
  property GL_SHADER_BINARY_FORMATS: Integer read _GetGL_SHADER_BINARY_FORMATS;
  property GL_SHADER_COMPILER: Integer read _GetGL_SHADER_COMPILER;
  property GL_SHADER_SOURCE_LENGTH: Integer read _GetGL_SHADER_SOURCE_LENGTH;
  property GL_SHADER_TYPE: Integer read _GetGL_SHADER_TYPE;
  property GL_SHADING_LANGUAGE_VERSION: Integer read _GetGL_SHADING_LANGUAGE_VERSION;
  property GL_SHORT: Integer read _GetGL_SHORT;
  property GL_SRC_ALPHA: Integer read _GetGL_SRC_ALPHA;
  property GL_SRC_ALPHA_SATURATE: Integer read _GetGL_SRC_ALPHA_SATURATE;
  property GL_SRC_COLOR: Integer read _GetGL_SRC_COLOR;
  property GL_STATIC_DRAW: Integer read _GetGL_STATIC_DRAW;
  property GL_STENCIL_ATTACHMENT: Integer read _GetGL_STENCIL_ATTACHMENT;
  property GL_STENCIL_BACK_FAIL: Integer read _GetGL_STENCIL_BACK_FAIL;
  property GL_STENCIL_BACK_FUNC: Integer read _GetGL_STENCIL_BACK_FUNC;
  property GL_STENCIL_BACK_PASS_DEPTH_FAIL: Integer read _GetGL_STENCIL_BACK_PASS_DEPTH_FAIL;
  property GL_STENCIL_BACK_PASS_DEPTH_PASS: Integer read _GetGL_STENCIL_BACK_PASS_DEPTH_PASS;
  property GL_STENCIL_BACK_REF: Integer read _GetGL_STENCIL_BACK_REF;
  property GL_STENCIL_BACK_VALUE_MASK: Integer read _GetGL_STENCIL_BACK_VALUE_MASK;
  property GL_STENCIL_BACK_WRITEMASK: Integer read _GetGL_STENCIL_BACK_WRITEMASK;
  property GL_STENCIL_BITS: Integer read _GetGL_STENCIL_BITS;
  property GL_STENCIL_BUFFER_BIT: Integer read _GetGL_STENCIL_BUFFER_BIT;
  property GL_STENCIL_CLEAR_VALUE: Integer read _GetGL_STENCIL_CLEAR_VALUE;
  property GL_STENCIL_FAIL: Integer read _GetGL_STENCIL_FAIL;
  property GL_STENCIL_FUNC: Integer read _GetGL_STENCIL_FUNC;
  property GL_STENCIL_INDEX: Integer read _GetGL_STENCIL_INDEX;
  property GL_STENCIL_INDEX8: Integer read _GetGL_STENCIL_INDEX8;
  property GL_STENCIL_PASS_DEPTH_FAIL: Integer read _GetGL_STENCIL_PASS_DEPTH_FAIL;
  property GL_STENCIL_PASS_DEPTH_PASS: Integer read _GetGL_STENCIL_PASS_DEPTH_PASS;
  property GL_STENCIL_REF: Integer read _GetGL_STENCIL_REF;
  property GL_STENCIL_TEST: Integer read _GetGL_STENCIL_TEST;
  property GL_STENCIL_VALUE_MASK: Integer read _GetGL_STENCIL_VALUE_MASK;
  property GL_STENCIL_WRITEMASK: Integer read _GetGL_STENCIL_WRITEMASK;
  property GL_STREAM_DRAW: Integer read _GetGL_STREAM_DRAW;
  property GL_SUBPIXEL_BITS: Integer read _GetGL_SUBPIXEL_BITS;
  property GL_TEXTURE: Integer read _GetGL_TEXTURE;
  property GL_TEXTURE0: Integer read _GetGL_TEXTURE0;
  property GL_TEXTURE1: Integer read _GetGL_TEXTURE1;
  property GL_TEXTURE10: Integer read _GetGL_TEXTURE10;
  property GL_TEXTURE11: Integer read _GetGL_TEXTURE11;
  property GL_TEXTURE12: Integer read _GetGL_TEXTURE12;
  property GL_TEXTURE13: Integer read _GetGL_TEXTURE13;
  property GL_TEXTURE14: Integer read _GetGL_TEXTURE14;
  property GL_TEXTURE15: Integer read _GetGL_TEXTURE15;
  property GL_TEXTURE16: Integer read _GetGL_TEXTURE16;
  property GL_TEXTURE17: Integer read _GetGL_TEXTURE17;
  property GL_TEXTURE18: Integer read _GetGL_TEXTURE18;
  property GL_TEXTURE19: Integer read _GetGL_TEXTURE19;
  property GL_TEXTURE2: Integer read _GetGL_TEXTURE2;
  property GL_TEXTURE20: Integer read _GetGL_TEXTURE20;
  property GL_TEXTURE21: Integer read _GetGL_TEXTURE21;
  property GL_TEXTURE22: Integer read _GetGL_TEXTURE22;
  property GL_TEXTURE23: Integer read _GetGL_TEXTURE23;
  property GL_TEXTURE24: Integer read _GetGL_TEXTURE24;
  property GL_TEXTURE25: Integer read _GetGL_TEXTURE25;
  property GL_TEXTURE26: Integer read _GetGL_TEXTURE26;
  property GL_TEXTURE27: Integer read _GetGL_TEXTURE27;
  property GL_TEXTURE28: Integer read _GetGL_TEXTURE28;
  property GL_TEXTURE29: Integer read _GetGL_TEXTURE29;
  property GL_TEXTURE3: Integer read _GetGL_TEXTURE3;
  property GL_TEXTURE30: Integer read _GetGL_TEXTURE30;
  property GL_TEXTURE31: Integer read _GetGL_TEXTURE31;
  property GL_TEXTURE4: Integer read _GetGL_TEXTURE4;
  property GL_TEXTURE5: Integer read _GetGL_TEXTURE5;
  property GL_TEXTURE6: Integer read _GetGL_TEXTURE6;
  property GL_TEXTURE7: Integer read _GetGL_TEXTURE7;
  property GL_TEXTURE8: Integer read _GetGL_TEXTURE8;
  property GL_TEXTURE9: Integer read _GetGL_TEXTURE9;
  property GL_TEXTURE_2D: Integer read _GetGL_TEXTURE_2D;
  property GL_TEXTURE_BINDING_2D: Integer read _GetGL_TEXTURE_BINDING_2D;
  property GL_TEXTURE_BINDING_CUBE_MAP: Integer read _GetGL_TEXTURE_BINDING_CUBE_MAP;
  property GL_TEXTURE_CUBE_MAP: Integer read _GetGL_TEXTURE_CUBE_MAP;
  property GL_TEXTURE_CUBE_MAP_NEGATIVE_X: Integer read _GetGL_TEXTURE_CUBE_MAP_NEGATIVE_X;
  property GL_TEXTURE_CUBE_MAP_NEGATIVE_Y: Integer read _GetGL_TEXTURE_CUBE_MAP_NEGATIVE_Y;
  property GL_TEXTURE_CUBE_MAP_NEGATIVE_Z: Integer read _GetGL_TEXTURE_CUBE_MAP_NEGATIVE_Z;
  property GL_TEXTURE_CUBE_MAP_POSITIVE_X: Integer read _GetGL_TEXTURE_CUBE_MAP_POSITIVE_X;
  property GL_TEXTURE_CUBE_MAP_POSITIVE_Y: Integer read _GetGL_TEXTURE_CUBE_MAP_POSITIVE_Y;
  property GL_TEXTURE_CUBE_MAP_POSITIVE_Z: Integer read _GetGL_TEXTURE_CUBE_MAP_POSITIVE_Z;
  property GL_TEXTURE_MAG_FILTER: Integer read _GetGL_TEXTURE_MAG_FILTER;
  property GL_TEXTURE_MIN_FILTER: Integer read _GetGL_TEXTURE_MIN_FILTER;
  property GL_TEXTURE_WRAP_S: Integer read _GetGL_TEXTURE_WRAP_S;
  property GL_TEXTURE_WRAP_T: Integer read _GetGL_TEXTURE_WRAP_T;
  property GL_TRIANGLES: Integer read _GetGL_TRIANGLES;
  property GL_TRIANGLE_FAN: Integer read _GetGL_TRIANGLE_FAN;
  property GL_TRIANGLE_STRIP: Integer read _GetGL_TRIANGLE_STRIP;
  property GL_TRUE: Integer read _GetGL_TRUE;
  property GL_UNPACK_ALIGNMENT: Integer read _GetGL_UNPACK_ALIGNMENT;
  property GL_UNSIGNED_BYTE: Integer read _GetGL_UNSIGNED_BYTE;
  property GL_UNSIGNED_INT: Integer read _GetGL_UNSIGNED_INT;
  property GL_UNSIGNED_SHORT: Integer read _GetGL_UNSIGNED_SHORT;
  property GL_UNSIGNED_SHORT_4_4_4_4: Integer read _GetGL_UNSIGNED_SHORT_4_4_4_4;
  property GL_UNSIGNED_SHORT_5_5_5_1: Integer read _GetGL_UNSIGNED_SHORT_5_5_5_1;
  property GL_UNSIGNED_SHORT_5_6_5: Integer read _GetGL_UNSIGNED_SHORT_5_6_5;
  property GL_VALIDATE_STATUS: Integer read _GetGL_VALIDATE_STATUS;
  property GL_VENDOR: Integer read _GetGL_VENDOR;
  property GL_VERSION: Integer read _GetGL_VERSION;
  property GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING: Integer read _GetGL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING;
  property GL_VERTEX_ATTRIB_ARRAY_ENABLED: Integer read _GetGL_VERTEX_ATTRIB_ARRAY_ENABLED;
  property GL_VERTEX_ATTRIB_ARRAY_NORMALIZED: Integer read _GetGL_VERTEX_ATTRIB_ARRAY_NORMALIZED;
  property GL_VERTEX_ATTRIB_ARRAY_POINTER: Integer read _GetGL_VERTEX_ATTRIB_ARRAY_POINTER;
  property GL_VERTEX_ATTRIB_ARRAY_SIZE: Integer read _GetGL_VERTEX_ATTRIB_ARRAY_SIZE;
  property GL_VERTEX_ATTRIB_ARRAY_STRIDE: Integer read _GetGL_VERTEX_ATTRIB_ARRAY_STRIDE;
  property GL_VERTEX_ATTRIB_ARRAY_TYPE: Integer read _GetGL_VERTEX_ATTRIB_ARRAY_TYPE;
  property GL_VERTEX_SHADER: Integer read _GetGL_VERTEX_SHADER;
  property GL_VIEWPORT: Integer read _GetGL_VIEWPORT;
  property GL_ZERO: Integer read _GetGL_ZERO;
end;

[JavaSignature('android/opengl/GLES20')]
JGLES20 = interface(JObject)
['{C9117E42-C2BB-477E-9824-8499EB52648C}']
end;
TJGLES20 = class(TJavaGenericImport<JGLES20Class, JGLES20>) end;

JGLUtilsClass = interface(JObjectClass)
['{81E5073C-57E6-43E4-A9D3-5408E7BD4BED}']
  {Methods}
  function getEGLErrorString(error: Integer): JString; cdecl;
  function getInternalFormat(bitmap: JBitmap): Integer; cdecl;
  function getType(bitmap: JBitmap): Integer; cdecl;
  procedure texImage2D(target: Integer; level: Integer; internalformat: Integer; bitmap: JBitmap; border: Integer); cdecl; overload;
  procedure texImage2D(target: Integer; level: Integer; internalformat: Integer; bitmap: JBitmap; type_: Integer; border: Integer); cdecl; overload;
  procedure texImage2D(target: Integer; level: Integer; bitmap: JBitmap; border: Integer); cdecl; overload;
  procedure texSubImage2D(target: Integer; level: Integer; xoffset: Integer; yoffset: Integer; bitmap: JBitmap); cdecl; overload;
  procedure texSubImage2D(target: Integer; level: Integer; xoffset: Integer; yoffset: Integer; bitmap: JBitmap; format: Integer; type_: Integer); cdecl; overload;
end;

[JavaSignature('android/opengl/GLUtils')]
JGLUtils = interface(JObject)
['{74547182-0A71-406F-916A-12CA7D3B380F}']
end;
TJGLUtils = class(TJavaGenericImport<JGLUtilsClass, JGLUtils>) end;

JGLES10ExtClass = interface(JObjectClass)
['{15FC6B5A-A329-4CA4-8C7B-96DD6FCA4775}']
  {Methods}
  function init: JGLES10Ext; cdecl;
  function glQueryMatrixxOES(mantissa: TJavaArray<Integer>; mantissaOffset: Integer; exponent: TJavaArray<Integer>; exponentOffset: Integer): Integer; cdecl;
end;

[JavaSignature('android/opengl/GLES10Ext')]
JGLES10Ext = interface(JObject)
['{2AC60F2B-5ED3-4A6C-ACEC-AFE55D9545E7}']
end;
TJGLES10Ext = class(TJavaGenericImport<JGLES10ExtClass, JGLES10Ext>) end;

JGLSurfaceView_EGLWindowSurfaceFactoryClass = interface(IJavaClass)
['{C942EC43-1E26-4355-89C2-2AF11A106E67}']
end;

[JavaSignature('android/opengl/GLSurfaceView$EGLWindowSurfaceFactory')]
JGLSurfaceView_EGLWindowSurfaceFactory = interface(IJavaInstance)
['{7DD4EB8B-7244-477A-B1CA-92ACB9679FBE}']
end;
TJGLSurfaceView_EGLWindowSurfaceFactory = class(TJavaGenericImport<JGLSurfaceView_EGLWindowSurfaceFactoryClass, JGLSurfaceView_EGLWindowSurfaceFactory>) end;

JGLSurfaceView_GLWrapperClass = interface(IJavaClass)
['{C178D1DB-2917-48D8-9F9E-EF01C799E8EB}']
end;

[JavaSignature('android/opengl/GLSurfaceView$GLWrapper')]
JGLSurfaceView_GLWrapper = interface(IJavaInstance)
['{14B3F93A-06F9-44C0-AEA1-8B49C917B0A7}']
end;
TJGLSurfaceView_GLWrapper = class(TJavaGenericImport<JGLSurfaceView_GLWrapperClass, JGLSurfaceView_GLWrapper>) end;

JGLES11Class = interface(JGLES10Class)
['{7E756B17-DA84-4FA1-95BD-A2C395836589}']
  {Property Methods}
  function _GetGL_ACTIVE_TEXTURE: Integer;
  function _GetGL_ADD_SIGNED: Integer;
  function _GetGL_ALPHA_SCALE: Integer;
  function _GetGL_ALPHA_TEST_FUNC: Integer;
  function _GetGL_ALPHA_TEST_REF: Integer;
  function _GetGL_ARRAY_BUFFER: Integer;
  function _GetGL_ARRAY_BUFFER_BINDING: Integer;
  function _GetGL_BLEND_DST: Integer;
  function _GetGL_BLEND_SRC: Integer;
  function _GetGL_BUFFER_ACCESS: Integer;
  function _GetGL_BUFFER_SIZE: Integer;
  function _GetGL_BUFFER_USAGE: Integer;
  function _GetGL_CLIENT_ACTIVE_TEXTURE: Integer;
  function _GetGL_CLIP_PLANE0: Integer;
  function _GetGL_CLIP_PLANE1: Integer;
  function _GetGL_CLIP_PLANE2: Integer;
  function _GetGL_CLIP_PLANE3: Integer;
  function _GetGL_CLIP_PLANE4: Integer;
  function _GetGL_CLIP_PLANE5: Integer;
  function _GetGL_COLOR_ARRAY_BUFFER_BINDING: Integer;
  function _GetGL_COLOR_ARRAY_POINTER: Integer;
  function _GetGL_COLOR_ARRAY_SIZE: Integer;
  function _GetGL_COLOR_ARRAY_STRIDE: Integer;
  function _GetGL_COLOR_ARRAY_TYPE: Integer;
  function _GetGL_COLOR_CLEAR_VALUE: Integer;
  function _GetGL_COLOR_WRITEMASK: Integer;
  function _GetGL_COMBINE: Integer;
  function _GetGL_COMBINE_ALPHA: Integer;
  function _GetGL_COMBINE_RGB: Integer;
  function _GetGL_CONSTANT: Integer;
  function _GetGL_COORD_REPLACE_OES: Integer;
  function _GetGL_CULL_FACE_MODE: Integer;
  function _GetGL_CURRENT_COLOR: Integer;
  function _GetGL_CURRENT_NORMAL: Integer;
  function _GetGL_CURRENT_TEXTURE_COORDS: Integer;
  function _GetGL_DEPTH_CLEAR_VALUE: Integer;
  function _GetGL_DEPTH_FUNC: Integer;
  function _GetGL_DEPTH_RANGE: Integer;
  function _GetGL_DEPTH_WRITEMASK: Integer;
  function _GetGL_DOT3_RGB: Integer;
  function _GetGL_DOT3_RGBA: Integer;
  function _GetGL_DYNAMIC_DRAW: Integer;
  function _GetGL_ELEMENT_ARRAY_BUFFER: Integer;
  function _GetGL_ELEMENT_ARRAY_BUFFER_BINDING: Integer;
  function _GetGL_FRONT_FACE: Integer;
  function _GetGL_GENERATE_MIPMAP: Integer;
  function _GetGL_GENERATE_MIPMAP_HINT: Integer;
  function _GetGL_INTERPOLATE: Integer;
  function _GetGL_LINE_WIDTH: Integer;
  function _GetGL_LOGIC_OP_MODE: Integer;
  function _GetGL_MATRIX_MODE: Integer;
  function _GetGL_MAX_CLIP_PLANES: Integer;
  function _GetGL_MODELVIEW_MATRIX: Integer;
  function _GetGL_MODELVIEW_MATRIX_FLOAT_AS_INT_BITS_OES: Integer;
  function _GetGL_MODELVIEW_STACK_DEPTH: Integer;
  function _GetGL_NORMAL_ARRAY_BUFFER_BINDING: Integer;
  function _GetGL_NORMAL_ARRAY_POINTER: Integer;
  function _GetGL_NORMAL_ARRAY_STRIDE: Integer;
  function _GetGL_NORMAL_ARRAY_TYPE: Integer;
  function _GetGL_OPERAND0_ALPHA: Integer;
  function _GetGL_OPERAND0_RGB: Integer;
  function _GetGL_OPERAND1_ALPHA: Integer;
  function _GetGL_OPERAND1_RGB: Integer;
  function _GetGL_OPERAND2_ALPHA: Integer;
  function _GetGL_OPERAND2_RGB: Integer;
  function _GetGL_POINT_DISTANCE_ATTENUATION: Integer;
  function _GetGL_POINT_FADE_THRESHOLD_SIZE: Integer;
  function _GetGL_POINT_SIZE: Integer;
  function _GetGL_POINT_SIZE_ARRAY_BUFFER_BINDING_OES: Integer;
  function _GetGL_POINT_SIZE_ARRAY_OES: Integer;
  function _GetGL_POINT_SIZE_ARRAY_POINTER_OES: Integer;
  function _GetGL_POINT_SIZE_ARRAY_STRIDE_OES: Integer;
  function _GetGL_POINT_SIZE_ARRAY_TYPE_OES: Integer;
  function _GetGL_POINT_SIZE_MAX: Integer;
  function _GetGL_POINT_SIZE_MIN: Integer;
  function _GetGL_POINT_SPRITE_OES: Integer;
  function _GetGL_POLYGON_OFFSET_FACTOR: Integer;
  function _GetGL_POLYGON_OFFSET_UNITS: Integer;
  function _GetGL_PREVIOUS: Integer;
  function _GetGL_PRIMARY_COLOR: Integer;
  function _GetGL_PROJECTION_MATRIX: Integer;
  function _GetGL_PROJECTION_MATRIX_FLOAT_AS_INT_BITS_OES: Integer;
  function _GetGL_PROJECTION_STACK_DEPTH: Integer;
  function _GetGL_RGB_SCALE: Integer;
  function _GetGL_SAMPLES: Integer;
  function _GetGL_SAMPLE_BUFFERS: Integer;
  function _GetGL_SAMPLE_COVERAGE_INVERT: Integer;
  function _GetGL_SAMPLE_COVERAGE_VALUE: Integer;
  function _GetGL_SCISSOR_BOX: Integer;
  function _GetGL_SHADE_MODEL: Integer;
  function _GetGL_SRC0_ALPHA: Integer;
  function _GetGL_SRC0_RGB: Integer;
  function _GetGL_SRC1_ALPHA: Integer;
  function _GetGL_SRC1_RGB: Integer;
  function _GetGL_SRC2_ALPHA: Integer;
  function _GetGL_SRC2_RGB: Integer;
  function _GetGL_STATIC_DRAW: Integer;
  function _GetGL_STENCIL_CLEAR_VALUE: Integer;
  function _GetGL_STENCIL_FAIL: Integer;
  function _GetGL_STENCIL_FUNC: Integer;
  function _GetGL_STENCIL_PASS_DEPTH_FAIL: Integer;
  function _GetGL_STENCIL_PASS_DEPTH_PASS: Integer;
  function _GetGL_STENCIL_REF: Integer;
  function _GetGL_STENCIL_VALUE_MASK: Integer;
  function _GetGL_STENCIL_WRITEMASK: Integer;
  function _GetGL_SUBTRACT: Integer;
  function _GetGL_TEXTURE_BINDING_2D: Integer;
  function _GetGL_TEXTURE_COORD_ARRAY_BUFFER_BINDING: Integer;
  function _GetGL_TEXTURE_COORD_ARRAY_POINTER: Integer;
  function _GetGL_TEXTURE_COORD_ARRAY_SIZE: Integer;
  function _GetGL_TEXTURE_COORD_ARRAY_STRIDE: Integer;
  function _GetGL_TEXTURE_COORD_ARRAY_TYPE: Integer;
  function _GetGL_TEXTURE_MATRIX: Integer;
  function _GetGL_TEXTURE_MATRIX_FLOAT_AS_INT_BITS_OES: Integer;
  function _GetGL_TEXTURE_STACK_DEPTH: Integer;
  function _GetGL_VERTEX_ARRAY_BUFFER_BINDING: Integer;
  function _GetGL_VERTEX_ARRAY_POINTER: Integer;
  function _GetGL_VERTEX_ARRAY_SIZE: Integer;
  function _GetGL_VERTEX_ARRAY_STRIDE: Integer;
  function _GetGL_VERTEX_ARRAY_TYPE: Integer;
  function _GetGL_VIEWPORT: Integer;
  function _GetGL_WRITE_ONLY: Integer;
  {Methods}
  function init: JGLES11; cdecl;
  procedure glBindBuffer(target: Integer; buffer: Integer); cdecl;
  procedure glClipPlanef(plane: Integer; equation: TJavaArray<Single>; offset: Integer); cdecl;
  procedure glClipPlanex(plane: Integer; equation: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glColor4ub(red: Byte; green: Byte; blue: Byte; alpha: Byte); cdecl;
  procedure glColorPointer(size: Integer; type_: Integer; stride: Integer; offset: Integer); cdecl;
  procedure glDeleteBuffers(n: Integer; buffers: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glDrawElements(mode: Integer; count: Integer; type_: Integer; offset: Integer); cdecl;
  procedure glGenBuffers(n: Integer; buffers: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glGetBooleanv(pname: Integer; params: TJavaArray<Boolean>; offset: Integer); cdecl;
  procedure glGetBufferParameteriv(target: Integer; pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glGetClipPlanef(pname: Integer; eqn: TJavaArray<Single>; offset: Integer); cdecl;
  procedure glGetClipPlanex(pname: Integer; eqn: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glGetFixedv(pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glGetFloatv(pname: Integer; params: TJavaArray<Single>; offset: Integer); cdecl;
  procedure glGetLightfv(light: Integer; pname: Integer; params: TJavaArray<Single>; offset: Integer); cdecl;
  procedure glGetLightxv(light: Integer; pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glGetMaterialfv(face: Integer; pname: Integer; params: TJavaArray<Single>; offset: Integer); cdecl;
  procedure glGetMaterialxv(face: Integer; pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glGetTexEnvfv(env: Integer; pname: Integer; params: TJavaArray<Single>; offset: Integer); cdecl;
  procedure glGetTexEnviv(env: Integer; pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glGetTexEnvxv(env: Integer; pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glGetTexParameterfv(target: Integer; pname: Integer; params: TJavaArray<Single>; offset: Integer); cdecl;
  procedure glGetTexParameteriv(target: Integer; pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glGetTexParameterxv(target: Integer; pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  function glIsBuffer(buffer: Integer): Boolean; cdecl;
  function glIsEnabled(cap: Integer): Boolean; cdecl;
  function glIsTexture(texture: Integer): Boolean; cdecl;
  procedure glNormalPointer(type_: Integer; stride: Integer; offset: Integer); cdecl;
  procedure glPointParameterf(pname: Integer; param: Single); cdecl;
  procedure glPointParameterfv(pname: Integer; params: TJavaArray<Single>; offset: Integer); cdecl;
  procedure glPointParameterx(pname: Integer; param: Integer); cdecl;
  procedure glPointParameterxv(pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glTexCoordPointer(size: Integer; type_: Integer; stride: Integer; offset: Integer); cdecl;
  procedure glTexEnvi(target: Integer; pname: Integer; param: Integer); cdecl;
  procedure glTexEnviv(target: Integer; pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glTexParameterfv(target: Integer; pname: Integer; params: TJavaArray<Single>; offset: Integer); cdecl;
  procedure glTexParameteri(target: Integer; pname: Integer; param: Integer); cdecl;
  procedure glTexParameteriv(target: Integer; pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glTexParameterxv(target: Integer; pname: Integer; params: TJavaArray<Integer>; offset: Integer); cdecl;
  procedure glVertexPointer(size: Integer; type_: Integer; stride: Integer; offset: Integer); cdecl;
  {Properties}
  property GL_ACTIVE_TEXTURE: Integer read _GetGL_ACTIVE_TEXTURE;
  property GL_ADD_SIGNED: Integer read _GetGL_ADD_SIGNED;
  property GL_ALPHA_SCALE: Integer read _GetGL_ALPHA_SCALE;
  property GL_ALPHA_TEST_FUNC: Integer read _GetGL_ALPHA_TEST_FUNC;
  property GL_ALPHA_TEST_REF: Integer read _GetGL_ALPHA_TEST_REF;
  property GL_ARRAY_BUFFER: Integer read _GetGL_ARRAY_BUFFER;
  property GL_ARRAY_BUFFER_BINDING: Integer read _GetGL_ARRAY_BUFFER_BINDING;
  property GL_BLEND_DST: Integer read _GetGL_BLEND_DST;
  property GL_BLEND_SRC: Integer read _GetGL_BLEND_SRC;
  property GL_BUFFER_ACCESS: Integer read _GetGL_BUFFER_ACCESS;
  property GL_BUFFER_SIZE: Integer read _GetGL_BUFFER_SIZE;
  property GL_BUFFER_USAGE: Integer read _GetGL_BUFFER_USAGE;
  property GL_CLIENT_ACTIVE_TEXTURE: Integer read _GetGL_CLIENT_ACTIVE_TEXTURE;
  property GL_CLIP_PLANE0: Integer read _GetGL_CLIP_PLANE0;
  property GL_CLIP_PLANE1: Integer read _GetGL_CLIP_PLANE1;
  property GL_CLIP_PLANE2: Integer read _GetGL_CLIP_PLANE2;
  property GL_CLIP_PLANE3: Integer read _GetGL_CLIP_PLANE3;
  property GL_CLIP_PLANE4: Integer read _GetGL_CLIP_PLANE4;
  property GL_CLIP_PLANE5: Integer read _GetGL_CLIP_PLANE5;
  property GL_COLOR_ARRAY_BUFFER_BINDING: Integer read _GetGL_COLOR_ARRAY_BUFFER_BINDING;
  property GL_COLOR_ARRAY_POINTER: Integer read _GetGL_COLOR_ARRAY_POINTER;
  property GL_COLOR_ARRAY_SIZE: Integer read _GetGL_COLOR_ARRAY_SIZE;
  property GL_COLOR_ARRAY_STRIDE: Integer read _GetGL_COLOR_ARRAY_STRIDE;
  property GL_COLOR_ARRAY_TYPE: Integer read _GetGL_COLOR_ARRAY_TYPE;
  property GL_COLOR_CLEAR_VALUE: Integer read _GetGL_COLOR_CLEAR_VALUE;
  property GL_COLOR_WRITEMASK: Integer read _GetGL_COLOR_WRITEMASK;
  property GL_COMBINE: Integer read _GetGL_COMBINE;
  property GL_COMBINE_ALPHA: Integer read _GetGL_COMBINE_ALPHA;
  property GL_COMBINE_RGB: Integer read _GetGL_COMBINE_RGB;
  property GL_CONSTANT: Integer read _GetGL_CONSTANT;
  property GL_COORD_REPLACE_OES: Integer read _GetGL_COORD_REPLACE_OES;
  property GL_CULL_FACE_MODE: Integer read _GetGL_CULL_FACE_MODE;
  property GL_CURRENT_COLOR: Integer read _GetGL_CURRENT_COLOR;
  property GL_CURRENT_NORMAL: Integer read _GetGL_CURRENT_NORMAL;
  property GL_CURRENT_TEXTURE_COORDS: Integer read _GetGL_CURRENT_TEXTURE_COORDS;
  property GL_DEPTH_CLEAR_VALUE: Integer read _GetGL_DEPTH_CLEAR_VALUE;
  property GL_DEPTH_FUNC: Integer read _GetGL_DEPTH_FUNC;
  property GL_DEPTH_RANGE: Integer read _GetGL_DEPTH_RANGE;
  property GL_DEPTH_WRITEMASK: Integer read _GetGL_DEPTH_WRITEMASK;
  property GL_DOT3_RGB: Integer read _GetGL_DOT3_RGB;
  property GL_DOT3_RGBA: Integer read _GetGL_DOT3_RGBA;
  property GL_DYNAMIC_DRAW: Integer read _GetGL_DYNAMIC_DRAW;
  property GL_ELEMENT_ARRAY_BUFFER: Integer read _GetGL_ELEMENT_ARRAY_BUFFER;
  property GL_ELEMENT_ARRAY_BUFFER_BINDING: Integer read _GetGL_ELEMENT_ARRAY_BUFFER_BINDING;
  property GL_FRONT_FACE: Integer read _GetGL_FRONT_FACE;
  property GL_GENERATE_MIPMAP: Integer read _GetGL_GENERATE_MIPMAP;
  property GL_GENERATE_MIPMAP_HINT: Integer read _GetGL_GENERATE_MIPMAP_HINT;
  property GL_INTERPOLATE: Integer read _GetGL_INTERPOLATE;
  property GL_LINE_WIDTH: Integer read _GetGL_LINE_WIDTH;
  property GL_LOGIC_OP_MODE: Integer read _GetGL_LOGIC_OP_MODE;
  property GL_MATRIX_MODE: Integer read _GetGL_MATRIX_MODE;
  property GL_MAX_CLIP_PLANES: Integer read _GetGL_MAX_CLIP_PLANES;
  property GL_MODELVIEW_MATRIX: Integer read _GetGL_MODELVIEW_MATRIX;
  property GL_MODELVIEW_MATRIX_FLOAT_AS_INT_BITS_OES: Integer read _GetGL_MODELVIEW_MATRIX_FLOAT_AS_INT_BITS_OES;
  property GL_MODELVIEW_STACK_DEPTH: Integer read _GetGL_MODELVIEW_STACK_DEPTH;
  property GL_NORMAL_ARRAY_BUFFER_BINDING: Integer read _GetGL_NORMAL_ARRAY_BUFFER_BINDING;
  property GL_NORMAL_ARRAY_POINTER: Integer read _GetGL_NORMAL_ARRAY_POINTER;
  property GL_NORMAL_ARRAY_STRIDE: Integer read _GetGL_NORMAL_ARRAY_STRIDE;
  property GL_NORMAL_ARRAY_TYPE: Integer read _GetGL_NORMAL_ARRAY_TYPE;
  property GL_OPERAND0_ALPHA: Integer read _GetGL_OPERAND0_ALPHA;
  property GL_OPERAND0_RGB: Integer read _GetGL_OPERAND0_RGB;
  property GL_OPERAND1_ALPHA: Integer read _GetGL_OPERAND1_ALPHA;
  property GL_OPERAND1_RGB: Integer read _GetGL_OPERAND1_RGB;
  property GL_OPERAND2_ALPHA: Integer read _GetGL_OPERAND2_ALPHA;
  property GL_OPERAND2_RGB: Integer read _GetGL_OPERAND2_RGB;
  property GL_POINT_DISTANCE_ATTENUATION: Integer read _GetGL_POINT_DISTANCE_ATTENUATION;
{GL_POINT_FADE_THRESHOLD_SIZE is defined in parent interface}
{GL_POINT_SIZE is defined in parent interface}
  property GL_POINT_SIZE_ARRAY_BUFFER_BINDING_OES: Integer read _GetGL_POINT_SIZE_ARRAY_BUFFER_BINDING_OES;
  property GL_POINT_SIZE_ARRAY_OES: Integer read _GetGL_POINT_SIZE_ARRAY_OES;
  property GL_POINT_SIZE_ARRAY_POINTER_OES: Integer read _GetGL_POINT_SIZE_ARRAY_POINTER_OES;
  property GL_POINT_SIZE_ARRAY_STRIDE_OES: Integer read _GetGL_POINT_SIZE_ARRAY_STRIDE_OES;
  property GL_POINT_SIZE_ARRAY_TYPE_OES: Integer read _GetGL_POINT_SIZE_ARRAY_TYPE_OES;
  property GL_POINT_SIZE_MAX: Integer read _GetGL_POINT_SIZE_MAX;
  property GL_POINT_SIZE_MIN: Integer read _GetGL_POINT_SIZE_MIN;
  property GL_POINT_SPRITE_OES: Integer read _GetGL_POINT_SPRITE_OES;
  property GL_POLYGON_OFFSET_FACTOR: Integer read _GetGL_POLYGON_OFFSET_FACTOR;
  property GL_POLYGON_OFFSET_UNITS: Integer read _GetGL_POLYGON_OFFSET_UNITS;
  property GL_PREVIOUS: Integer read _GetGL_PREVIOUS;
  property GL_PRIMARY_COLOR: Integer read _GetGL_PRIMARY_COLOR;
  property GL_PROJECTION_MATRIX: Integer read _GetGL_PROJECTION_MATRIX;
  property GL_PROJECTION_MATRIX_FLOAT_AS_INT_BITS_OES: Integer read _GetGL_PROJECTION_MATRIX_FLOAT_AS_INT_BITS_OES;
  property GL_PROJECTION_STACK_DEPTH: Integer read _GetGL_PROJECTION_STACK_DEPTH;
  property GL_RGB_SCALE: Integer read _GetGL_RGB_SCALE;
  property GL_SAMPLES: Integer read _GetGL_SAMPLES;
  property GL_SAMPLE_BUFFERS: Integer read _GetGL_SAMPLE_BUFFERS;
  property GL_SAMPLE_COVERAGE_INVERT: Integer read _GetGL_SAMPLE_COVERAGE_INVERT;
  property GL_SAMPLE_COVERAGE_VALUE: Integer read _GetGL_SAMPLE_COVERAGE_VALUE;
  property GL_SCISSOR_BOX: Integer read _GetGL_SCISSOR_BOX;
  property GL_SHADE_MODEL: Integer read _GetGL_SHADE_MODEL;
  property GL_SRC0_ALPHA: Integer read _GetGL_SRC0_ALPHA;
  property GL_SRC0_RGB: Integer read _GetGL_SRC0_RGB;
  property GL_SRC1_ALPHA: Integer read _GetGL_SRC1_ALPHA;
  property GL_SRC1_RGB: Integer read _GetGL_SRC1_RGB;
  property GL_SRC2_ALPHA: Integer read _GetGL_SRC2_ALPHA;
  property GL_SRC2_RGB: Integer read _GetGL_SRC2_RGB;
  property GL_STATIC_DRAW: Integer read _GetGL_STATIC_DRAW;
  property GL_STENCIL_CLEAR_VALUE: Integer read _GetGL_STENCIL_CLEAR_VALUE;
  property GL_STENCIL_FAIL: Integer read _GetGL_STENCIL_FAIL;
  property GL_STENCIL_FUNC: Integer read _GetGL_STENCIL_FUNC;
  property GL_STENCIL_PASS_DEPTH_FAIL: Integer read _GetGL_STENCIL_PASS_DEPTH_FAIL;
  property GL_STENCIL_PASS_DEPTH_PASS: Integer read _GetGL_STENCIL_PASS_DEPTH_PASS;
  property GL_STENCIL_REF: Integer read _GetGL_STENCIL_REF;
  property GL_STENCIL_VALUE_MASK: Integer read _GetGL_STENCIL_VALUE_MASK;
  property GL_STENCIL_WRITEMASK: Integer read _GetGL_STENCIL_WRITEMASK;
  property GL_SUBTRACT: Integer read _GetGL_SUBTRACT;
  property GL_TEXTURE_BINDING_2D: Integer read _GetGL_TEXTURE_BINDING_2D;
  property GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING: Integer read _GetGL_TEXTURE_COORD_ARRAY_BUFFER_BINDING;
  property GL_TEXTURE_COORD_ARRAY_POINTER: Integer read _GetGL_TEXTURE_COORD_ARRAY_POINTER;
  property GL_TEXTURE_COORD_ARRAY_SIZE: Integer read _GetGL_TEXTURE_COORD_ARRAY_SIZE;
  property GL_TEXTURE_COORD_ARRAY_STRIDE: Integer read _GetGL_TEXTURE_COORD_ARRAY_STRIDE;
  property GL_TEXTURE_COORD_ARRAY_TYPE: Integer read _GetGL_TEXTURE_COORD_ARRAY_TYPE;
  property GL_TEXTURE_MATRIX: Integer read _GetGL_TEXTURE_MATRIX;
  property GL_TEXTURE_MATRIX_FLOAT_AS_INT_BITS_OES: Integer read _GetGL_TEXTURE_MATRIX_FLOAT_AS_INT_BITS_OES;
  property GL_TEXTURE_STACK_DEPTH: Integer read _GetGL_TEXTURE_STACK_DEPTH;
  property GL_VERTEX_ARRAY_BUFFER_BINDING: Integer read _GetGL_VERTEX_ARRAY_BUFFER_BINDING;
  property GL_VERTEX_ARRAY_POINTER: Integer read _GetGL_VERTEX_ARRAY_POINTER;
  property GL_VERTEX_ARRAY_SIZE: Integer read _GetGL_VERTEX_ARRAY_SIZE;
  property GL_VERTEX_ARRAY_STRIDE: Integer read _GetGL_VERTEX_ARRAY_STRIDE;
  property GL_VERTEX_ARRAY_TYPE: Integer read _GetGL_VERTEX_ARRAY_TYPE;
  property GL_VIEWPORT: Integer read _GetGL_VIEWPORT;
  property GL_WRITE_ONLY: Integer read _GetGL_WRITE_ONLY;
end;

[JavaSignature('android/opengl/GLES11')]
JGLES11 = interface(JGLES10)
['{07CD2AD1-3814-49AC-AA6E-09475BED866C}']
end;
TJGLES11 = class(TJavaGenericImport<JGLES11Class, JGLES11>) end;

JETC1UtilClass = interface(JObjectClass)
['{0D4A24DC-5C27-4C15-BD0E-1F58E876AFDE}']
  {Methods}
  function init: JETC1Util; cdecl;
  function createTexture(input: JInputStream): JETC1Util_ETC1Texture; cdecl;
  function isETC1Supported: Boolean; cdecl;
  procedure loadTexture(target: Integer; level: Integer; border: Integer; fallbackFormat: Integer; fallbackType: Integer; input: JInputStream); cdecl; overload;
  procedure loadTexture(target: Integer; level: Integer; border: Integer; fallbackFormat: Integer; fallbackType: Integer; texture: JETC1Util_ETC1Texture); cdecl; overload;
  procedure writeTexture(texture: JETC1Util_ETC1Texture; output: JOutputStream); cdecl;
end;

[JavaSignature('android/opengl/ETC1Util')]
JETC1Util = interface(JObject)
['{EFB4D5C0-69BE-4ACE-949F-32EF40434DCE}']
end;
TJETC1Util = class(TJavaGenericImport<JETC1UtilClass, JETC1Util>) end;

JGLUClass = interface(JObjectClass)
['{38DF26FD-6854-4A0B-A390-1247A628F094}']
  {Methods}
  function init: JGLU; cdecl;
  function gluErrorString(error: Integer): JString; cdecl;
  function gluProject(objX: Single; objY: Single; objZ: Single; model: TJavaArray<Single>; modelOffset: Integer; project: TJavaArray<Single>; projectOffset: Integer; view: TJavaArray<Integer>; viewOffset: Integer; win: TJavaArray<Single>; winOffset: Integer): Integer; cdecl;
  function gluUnProject(winX: Single; winY: Single; winZ: Single; model: TJavaArray<Single>; modelOffset: Integer; project: TJavaArray<Single>; projectOffset: Integer; view: TJavaArray<Integer>; viewOffset: Integer; obj: TJavaArray<Single>; objOffset: Integer): Integer; cdecl;
end;

[JavaSignature('android/opengl/GLU')]
JGLU = interface(JObject)
['{0E20BDE3-3902-49F7-9AD1-D665C1B13FBA}']
end;
TJGLU = class(TJavaGenericImport<JGLUClass, JGLU>) end;




implementation

begin

end.


