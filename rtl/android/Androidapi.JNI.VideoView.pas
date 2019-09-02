{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit Androidapi.JNI.VideoView;

interface

uses 
  Androidapi.JNIBridge,
  Androidapi.JNI.Media,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Net,
  Androidapi.JNI.Widget,
  Androidapi.JNI.Util,
  Androidapi.JNI.GraphicsContentViewText;

type

  {Class forward declarations}
  JMediaController_MediaPlayerControl = interface;//android.widget.MediaController$MediaPlayerControl
  JMediaController = interface;//android.widget.MediaController
  JVideoView = interface;//android.widget.VideoView

JMediaController_MediaPlayerControlClass = interface(IJavaClass)
['{31BC739A-5B96-48E4-B4D9-7486683FA117}']
end;

[JavaSignature('android/widget/MediaController$MediaPlayerControl')]
JMediaController_MediaPlayerControl = interface(IJavaInstance)
['{57C4DECE-8BAC-4153-ACEB-919DF5FE6844}']
  {Methods}
  function canPause: Boolean; cdecl;
  function canSeekBackward: Boolean; cdecl;
  function canSeekForward: Boolean; cdecl;
  function getBufferPercentage: Integer; cdecl;
  function getCurrentPosition: Integer; cdecl;
  function getDuration: Integer; cdecl;
  function isPlaying: Boolean; cdecl;
  procedure pause; cdecl;
  procedure seekTo(pos: Integer); cdecl;
  procedure start; cdecl;
end;
TJMediaController_MediaPlayerControl = class(TJavaGenericImport<JMediaController_MediaPlayerControlClass, JMediaController_MediaPlayerControl>) end;

JMediaControllerClass = interface(JFrameLayoutClass)
['{DF006611-A831-4E6D-92EF-9D03B98C99E0}']
  {Methods}
  function init(context: JContext; attrs: JAttributeSet): JMediaController; cdecl; overload;
  function init(context: JContext; useFastForward: Boolean): JMediaController; cdecl; overload;
  function init(context: JContext): JMediaController; cdecl; overload;
end;

[JavaSignature('android/widget/MediaController')]
JMediaController = interface(JFrameLayout)
['{18253CF1-90C2-43A0-A928-490468F4DD1A}']
  {Methods}
  function dispatchKeyEvent(event: JKeyEvent): Boolean; cdecl;
  procedure hide; cdecl;
  function isShowing: Boolean; cdecl;
  procedure onFinishInflate; cdecl;
  function onTouchEvent(event: JMotionEvent): Boolean; cdecl;
  function onTrackballEvent(ev: JMotionEvent): Boolean; cdecl;
  procedure setAnchorView(view: JView); cdecl;
  procedure setEnabled(enabled: Boolean); cdecl;
  procedure setMediaPlayer(player: JMediaController_MediaPlayerControl); cdecl;
  procedure setPrevNextListeners(next: JView_OnClickListener; prev: JView_OnClickListener); cdecl;
  procedure show; cdecl; overload;
  procedure show(timeout: Integer); cdecl; overload;
end;
TJMediaController = class(TJavaGenericImport<JMediaControllerClass, JMediaController>) end;

JVideoViewClass = interface(JSurfaceViewClass)
['{E393C1DD-943D-4190-B984-25FF211E020F}']
  {Methods}
  function init(context: JContext): JVideoView; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet): JVideoView; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet; defStyle: Integer): JVideoView; cdecl; overload;
end;

[JavaSignature('android/widget/VideoView')]
JVideoView = interface(JSurfaceView)
['{03A73FEC-6252-431B-904E-67581CD109DF}']
  {Methods}
  function canPause: Boolean; cdecl;
  function canSeekBackward: Boolean; cdecl;
  function canSeekForward: Boolean; cdecl;
  function getBufferPercentage: Integer; cdecl;
  function getCurrentPosition: Integer; cdecl;
  function getDuration: Integer; cdecl;
  function isPlaying: Boolean; cdecl;
  function onKeyDown(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  function onTouchEvent(ev: JMotionEvent): Boolean; cdecl;
  function onTrackballEvent(ev: JMotionEvent): Boolean; cdecl;
  procedure pause; cdecl;
  function resolveAdjustedSize(desiredSize: Integer; measureSpec: Integer): Integer; cdecl;
  procedure resume; cdecl;
  procedure seekTo(msec: Integer); cdecl;
  procedure setMediaController(controller: JMediaController); cdecl;
  procedure setOnCompletionListener(l: JMediaPlayer_OnCompletionListener); cdecl;
  procedure setOnErrorListener(l: JMediaPlayer_OnErrorListener); cdecl;
  procedure setOnInfoListener(l: JMediaPlayer_OnInfoListener); cdecl;
  procedure setOnPreparedListener(l: JMediaPlayer_OnPreparedListener); cdecl;
  procedure setVideoPath(path: JString); cdecl;
  procedure setVideoURI(uri: Jnet_Uri); cdecl;
  procedure start; cdecl;
  procedure stopPlayback; cdecl;
  procedure suspend; cdecl;
end;
TJVideoView = class(TJavaGenericImport<JVideoViewClass, JVideoView>) end;




implementation

begin

end.


