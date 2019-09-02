{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit Androidapi.JNI.Media;

interface

uses 
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
  Androidapi.JNI.App,
  Androidapi.JNI.Net,
  Androidapi.JNI.Hardware,
  Androidapi.JNI.GraphicsContentViewText;

type

  {Class forward declarations}
  JFaceDetector = interface;//android.media.FaceDetector
  JMediaRouter_RouteInfo = interface;//android.media.MediaRouter$RouteInfo
  JAsyncPlayer = interface;//android.media.AsyncPlayer
  JMediaRecorder_OnErrorListener = interface;//android.media.MediaRecorder$OnErrorListener
  JMediaRecorder_AudioSource = interface;//android.media.MediaRecorder$AudioSource
  JEffectFactory = interface;//android.media.effect.EffectFactory
  JFaceDetector_Face = interface;//android.media.FaceDetector$Face
  JAudioManager = interface;//android.media.AudioManager
  JAudioEffect_OnControlStatusChangeListener = interface;//android.media.audiofx.AudioEffect$OnControlStatusChangeListener
  JSoundPool_OnLoadCompleteListener = interface;//android.media.SoundPool$OnLoadCompleteListener
  JAudioTrack_OnPlaybackPositionUpdateListener = interface;//android.media.AudioTrack$OnPlaybackPositionUpdateListener
  JJetPlayer_OnJetEventListener = interface;//android.media.JetPlayer$OnJetEventListener
  JAudioEffect = interface;//android.media.audiofx.AudioEffect
  JNoiseSuppressor = interface;//android.media.audiofx.NoiseSuppressor
  JVirtualizer_Settings = interface;//android.media.audiofx.Virtualizer$Settings
  JMediaSyncEvent = interface;//android.media.MediaSyncEvent
  JEffectContext = interface;//android.media.effect.EffectContext
  JVirtualizer_OnParameterChangeListener = interface;//android.media.audiofx.Virtualizer$OnParameterChangeListener
  JMediaScannerConnection = interface;//android.media.MediaScannerConnection
  JThumbnailUtils = interface;//android.media.ThumbnailUtils
  JRemoteControlClient_MetadataEditor = interface;//android.media.RemoteControlClient$MetadataEditor
  JRingtoneManager = interface;//android.media.RingtoneManager
  JCamcorderProfile = interface;//android.media.CamcorderProfile
  JVisualizer_OnDataCaptureListener = interface;//android.media.audiofx.Visualizer$OnDataCaptureListener
  JMediaPlayer_OnVideoSizeChangedListener = interface;//android.media.MediaPlayer$OnVideoSizeChangedListener
  JAudioTrack = interface;//android.media.AudioTrack
  JVisualizer = interface;//android.media.audiofx.Visualizer
  JMediaRouter_Callback = interface;//android.media.MediaRouter$Callback
  JMediaRouter_SimpleCallback = interface;//android.media.MediaRouter$SimpleCallback
  JEnvironmentalReverb_Settings = interface;//android.media.audiofx.EnvironmentalReverb$Settings
  JAudioRecord_OnRecordPositionUpdateListener = interface;//android.media.AudioRecord$OnRecordPositionUpdateListener
  JMediaCodecInfo_CodecProfileLevel = interface;//android.media.MediaCodecInfo$CodecProfileLevel
  JEffectUpdateListener = interface;//android.media.effect.EffectUpdateListener
  JRingtone = interface;//android.media.Ringtone
  JEnvironmentalReverb = interface;//android.media.audiofx.EnvironmentalReverb
  JPresetReverb_Settings = interface;//android.media.audiofx.PresetReverb$Settings
  JVirtualizer = interface;//android.media.audiofx.Virtualizer
  JMediaCodec_BufferInfo = interface;//android.media.MediaCodec$BufferInfo
  JMediaRecorder_AudioEncoder = interface;//android.media.MediaRecorder$AudioEncoder
  JMediaFormat = interface;//android.media.MediaFormat
  JExifInterface = interface;//android.media.ExifInterface
  JEqualizer_OnParameterChangeListener = interface;//android.media.audiofx.Equalizer$OnParameterChangeListener
  JAudioEffect_Descriptor = interface;//android.media.audiofx.AudioEffect$Descriptor
  JMediaCodecList = interface;//android.media.MediaCodecList
  JMediaPlayer_TrackInfo = interface;//android.media.MediaPlayer$TrackInfo
  JMediaCodec = interface;//android.media.MediaCodec
  JToneGenerator = interface;//android.media.ToneGenerator
  JEnvironmentalReverb_OnParameterChangeListener = interface;//android.media.audiofx.EnvironmentalReverb$OnParameterChangeListener
  JMediaPlayer_OnSeekCompleteListener = interface;//android.media.MediaPlayer$OnSeekCompleteListener
  JMediaRecorder_VideoSource = interface;//android.media.MediaRecorder$VideoSource
  JBassBoost_Settings = interface;//android.media.audiofx.BassBoost$Settings
  JMediaCrypto = interface;//android.media.MediaCrypto
  JMediaCodec_CryptoInfo = interface;//android.media.MediaCodec$CryptoInfo
  JAudioEffect_OnEnableStatusChangeListener = interface;//android.media.audiofx.AudioEffect$OnEnableStatusChangeListener
  JMediaScannerConnection_OnScanCompletedListener = interface;//android.media.MediaScannerConnection$OnScanCompletedListener
  JMediaScannerConnection_MediaScannerConnectionClient = interface;//android.media.MediaScannerConnection$MediaScannerConnectionClient
  JMediaCryptoException = interface;//android.media.MediaCryptoException
  JMediaRecorder_VideoEncoder = interface;//android.media.MediaRecorder$VideoEncoder
  JMediaRouter_VolumeCallback = interface;//android.media.MediaRouter$VolumeCallback
  JEqualizer = interface;//android.media.audiofx.Equalizer
  JAudioFormat = interface;//android.media.AudioFormat
  JMediaExtractor = interface;//android.media.MediaExtractor
  JMediaMetadataRetriever = interface;//android.media.MediaMetadataRetriever
  JMediaCodecInfo_CodecCapabilities = interface;//android.media.MediaCodecInfo$CodecCapabilities
  JRemoteControlClient = interface;//android.media.RemoteControlClient
  JMediaActionSound = interface;//android.media.MediaActionSound
  JMediaRecorder_OnInfoListener = interface;//android.media.MediaRecorder$OnInfoListener
  JAutomaticGainControl = interface;//android.media.audiofx.AutomaticGainControl
  JBassBoost = interface;//android.media.audiofx.BassBoost
  JPresetReverb_OnParameterChangeListener = interface;//android.media.audiofx.PresetReverb$OnParameterChangeListener
  JMediaPlayer_OnPreparedListener = interface;//android.media.MediaPlayer$OnPreparedListener
  JSoundPool = interface;//android.media.SoundPool
  JMediaRouter_RouteGroup = interface;//android.media.MediaRouter$RouteGroup
  JMediaRouter = interface;//android.media.MediaRouter
  JAudioRecord = interface;//android.media.AudioRecord
  JMediaRecorder = interface;//android.media.MediaRecorder
  JMediaPlayer_OnErrorListener = interface;//android.media.MediaPlayer$OnErrorListener
  JJetPlayer = interface;//android.media.JetPlayer
  JAudioManager_OnAudioFocusChangeListener = interface;//android.media.AudioManager$OnAudioFocusChangeListener
  JMediaPlayer_OnBufferingUpdateListener = interface;//android.media.MediaPlayer$OnBufferingUpdateListener
  JTimedText = interface;//android.media.TimedText
  JEqualizer_Settings = interface;//android.media.audiofx.Equalizer$Settings
  JMediaRouter_UserRouteInfo = interface;//android.media.MediaRouter$UserRouteInfo
  JPresetReverb = interface;//android.media.audiofx.PresetReverb
  JBassBoost_OnParameterChangeListener = interface;//android.media.audiofx.BassBoost$OnParameterChangeListener
  JMediaPlayer_OnInfoListener = interface;//android.media.MediaPlayer$OnInfoListener
  JMediaCodecInfo = interface;//android.media.MediaCodecInfo
  JMediaPlayer_OnTimedTextListener = interface;//android.media.MediaPlayer$OnTimedTextListener
  JMediaRouter_RouteCategory = interface;//android.media.MediaRouter$RouteCategory
  JMediaRecorder_OutputFormat = interface;//android.media.MediaRecorder$OutputFormat
  JMediaPlayer = interface;//android.media.MediaPlayer
  JAcousticEchoCanceler = interface;//android.media.audiofx.AcousticEchoCanceler
  JCameraProfile = interface;//android.media.CameraProfile
  JMediaCodec_CryptoException = interface;//android.media.MediaCodec$CryptoException
  JEffect = interface;//android.media.effect.Effect
  JMediaPlayer_OnCompletionListener = interface;//android.media.MediaPlayer$OnCompletionListener

JFaceDetectorClass = interface(JObjectClass)
['{1EFF1585-F9D1-473B-8157-809AF8522E6C}']
  {Methods}
  function init(width: Integer; height: Integer; maxFaces: Integer): JFaceDetector; cdecl;
end;

[JavaSignature('android/media/FaceDetector')]
JFaceDetector = interface(JObject)
['{51F75D8F-DFA7-4368-B73F-DE24C7C060E4}']
  {Methods}
  function findFaces(bitmap: JBitmap; faces: TJavaObjectArray<JFaceDetector_Face>): Integer; cdecl;
end;
TJFaceDetector = class(TJavaGenericImport<JFaceDetectorClass, JFaceDetector>) end;

JMediaRouter_RouteInfoClass = interface(JObjectClass)
['{AEC0CEF3-E779-45E4-A633-3453115EDCE5}']
  {Property Methods}
  function _GetPLAYBACK_TYPE_LOCAL: Integer;
  function _GetPLAYBACK_TYPE_REMOTE: Integer;
  function _GetPLAYBACK_VOLUME_FIXED: Integer;
  function _GetPLAYBACK_VOLUME_VARIABLE: Integer;
  {Properties}
  property PLAYBACK_TYPE_LOCAL: Integer read _GetPLAYBACK_TYPE_LOCAL;
  property PLAYBACK_TYPE_REMOTE: Integer read _GetPLAYBACK_TYPE_REMOTE;
  property PLAYBACK_VOLUME_FIXED: Integer read _GetPLAYBACK_VOLUME_FIXED;
  property PLAYBACK_VOLUME_VARIABLE: Integer read _GetPLAYBACK_VOLUME_VARIABLE;
end;

[JavaSignature('android/media/MediaRouter$RouteInfo')]
JMediaRouter_RouteInfo = interface(JObject)
['{D009A403-61AD-4006-AB7A-292CDB8562E2}']
  {Methods}
  function getCategory: JMediaRouter_RouteCategory; cdecl;
  function getGroup: JMediaRouter_RouteGroup; cdecl;
  function getIconDrawable: JDrawable; cdecl;
  function getName: JCharSequence; cdecl; overload;
  function getName(context: JContext): JCharSequence; cdecl; overload;
  function getPlaybackStream: Integer; cdecl;
  function getPlaybackType: Integer; cdecl;
  function getPresentationDisplay: JDisplay; cdecl;
  function getStatus: JCharSequence; cdecl;
  function getSupportedTypes: Integer; cdecl;
  function getTag: JObject; cdecl;
  function getVolume: Integer; cdecl;
  function getVolumeHandling: Integer; cdecl;
  function getVolumeMax: Integer; cdecl;
  function isEnabled: Boolean; cdecl;
  procedure requestSetVolume(volume: Integer); cdecl;
  procedure requestUpdateVolume(direction: Integer); cdecl;
  procedure setTag(tag: JObject); cdecl;
  function toString: JString; cdecl;
end;
TJMediaRouter_RouteInfo = class(TJavaGenericImport<JMediaRouter_RouteInfoClass, JMediaRouter_RouteInfo>) end;

JAsyncPlayerClass = interface(JObjectClass)
['{BF7F601F-6678-4903-B30E-7A73E4EE059C}']
  {Methods}
  function init(tag: JString): JAsyncPlayer; cdecl;
end;

[JavaSignature('android/media/AsyncPlayer')]
JAsyncPlayer = interface(JObject)
['{8415DF11-11ED-4FA6-8864-F4420519D2C3}']
  {Methods}
  procedure play(context: JContext; uri: Jnet_Uri; looping: Boolean; stream: Integer); cdecl;
  procedure stop; cdecl;
end;
TJAsyncPlayer = class(TJavaGenericImport<JAsyncPlayerClass, JAsyncPlayer>) end;

JMediaRecorder_OnErrorListenerClass = interface(IJavaClass)
['{61C1AC3C-6F4E-4513-B98A-7CFDD07FE71B}']
end;

[JavaSignature('android/media/MediaRecorder$OnErrorListener')]
JMediaRecorder_OnErrorListener = interface(IJavaInstance)
['{E36B1187-DCC1-48EF-994A-4AB4C6FBE5AF}']
  {Methods}
  procedure onError(mr: JMediaRecorder; what: Integer; extra: Integer); cdecl;
end;
TJMediaRecorder_OnErrorListener = class(TJavaGenericImport<JMediaRecorder_OnErrorListenerClass, JMediaRecorder_OnErrorListener>) end;

JMediaRecorder_AudioSourceClass = interface(JObjectClass)
['{A2ACA9E4-86BA-483F-B173-9AC32C457B5B}']
  {Property Methods}
  function _GetCAMCORDER: Integer;
  function _GetDEFAULT: Integer;
  function _GetMIC: Integer;
  function _GetVOICE_CALL: Integer;
  function _GetVOICE_COMMUNICATION: Integer;
  function _GetVOICE_DOWNLINK: Integer;
  function _GetVOICE_RECOGNITION: Integer;
  function _GetVOICE_UPLINK: Integer;
  {Properties}
  property CAMCORDER: Integer read _GetCAMCORDER;
  property DEFAULT: Integer read _GetDEFAULT;
  property MIC: Integer read _GetMIC;
  property VOICE_CALL: Integer read _GetVOICE_CALL;
  property VOICE_COMMUNICATION: Integer read _GetVOICE_COMMUNICATION;
  property VOICE_DOWNLINK: Integer read _GetVOICE_DOWNLINK;
  property VOICE_RECOGNITION: Integer read _GetVOICE_RECOGNITION;
  property VOICE_UPLINK: Integer read _GetVOICE_UPLINK;
end;

[JavaSignature('android/media/MediaRecorder$AudioSource')]
JMediaRecorder_AudioSource = interface(JObject)
['{32A3494D-269C-4635-83D3-8C8E42C03557}']
end;
TJMediaRecorder_AudioSource = class(TJavaGenericImport<JMediaRecorder_AudioSourceClass, JMediaRecorder_AudioSource>) end;

JEffectFactoryClass = interface(JObjectClass)
['{14E2D166-0AC1-4491-BF2B-962F24BAB38A}']
  {Property Methods}
  function _GetEFFECT_AUTOFIX: JString;
  function _GetEFFECT_BACKDROPPER: JString;
  function _GetEFFECT_BITMAPOVERLAY: JString;
  function _GetEFFECT_BLACKWHITE: JString;
  function _GetEFFECT_BRIGHTNESS: JString;
  function _GetEFFECT_CONTRAST: JString;
  function _GetEFFECT_CROP: JString;
  function _GetEFFECT_CROSSPROCESS: JString;
  function _GetEFFECT_DOCUMENTARY: JString;
  function _GetEFFECT_DUOTONE: JString;
  function _GetEFFECT_FILLLIGHT: JString;
  function _GetEFFECT_FISHEYE: JString;
  function _GetEFFECT_FLIP: JString;
  function _GetEFFECT_GRAIN: JString;
  function _GetEFFECT_GRAYSCALE: JString;
  function _GetEFFECT_LOMOISH: JString;
  function _GetEFFECT_NEGATIVE: JString;
  function _GetEFFECT_POSTERIZE: JString;
  function _GetEFFECT_REDEYE: JString;
  function _GetEFFECT_ROTATE: JString;
  function _GetEFFECT_SATURATE: JString;
  function _GetEFFECT_SEPIA: JString;
  function _GetEFFECT_SHARPEN: JString;
  function _GetEFFECT_STRAIGHTEN: JString;
  function _GetEFFECT_TEMPERATURE: JString;
  function _GetEFFECT_TINT: JString;
  function _GetEFFECT_VIGNETTE: JString;
  {Methods}
  function isEffectSupported(effectName: JString): Boolean; cdecl;
  {Properties}
  property EFFECT_AUTOFIX: JString read _GetEFFECT_AUTOFIX;
  property EFFECT_BACKDROPPER: JString read _GetEFFECT_BACKDROPPER;
  property EFFECT_BITMAPOVERLAY: JString read _GetEFFECT_BITMAPOVERLAY;
  property EFFECT_BLACKWHITE: JString read _GetEFFECT_BLACKWHITE;
  property EFFECT_BRIGHTNESS: JString read _GetEFFECT_BRIGHTNESS;
  property EFFECT_CONTRAST: JString read _GetEFFECT_CONTRAST;
  property EFFECT_CROP: JString read _GetEFFECT_CROP;
  property EFFECT_CROSSPROCESS: JString read _GetEFFECT_CROSSPROCESS;
  property EFFECT_DOCUMENTARY: JString read _GetEFFECT_DOCUMENTARY;
  property EFFECT_DUOTONE: JString read _GetEFFECT_DUOTONE;
  property EFFECT_FILLLIGHT: JString read _GetEFFECT_FILLLIGHT;
  property EFFECT_FISHEYE: JString read _GetEFFECT_FISHEYE;
  property EFFECT_FLIP: JString read _GetEFFECT_FLIP;
  property EFFECT_GRAIN: JString read _GetEFFECT_GRAIN;
  property EFFECT_GRAYSCALE: JString read _GetEFFECT_GRAYSCALE;
  property EFFECT_LOMOISH: JString read _GetEFFECT_LOMOISH;
  property EFFECT_NEGATIVE: JString read _GetEFFECT_NEGATIVE;
  property EFFECT_POSTERIZE: JString read _GetEFFECT_POSTERIZE;
  property EFFECT_REDEYE: JString read _GetEFFECT_REDEYE;
  property EFFECT_ROTATE: JString read _GetEFFECT_ROTATE;
  property EFFECT_SATURATE: JString read _GetEFFECT_SATURATE;
  property EFFECT_SEPIA: JString read _GetEFFECT_SEPIA;
  property EFFECT_SHARPEN: JString read _GetEFFECT_SHARPEN;
  property EFFECT_STRAIGHTEN: JString read _GetEFFECT_STRAIGHTEN;
  property EFFECT_TEMPERATURE: JString read _GetEFFECT_TEMPERATURE;
  property EFFECT_TINT: JString read _GetEFFECT_TINT;
  property EFFECT_VIGNETTE: JString read _GetEFFECT_VIGNETTE;
end;

[JavaSignature('android/media/effect/EffectFactory')]
JEffectFactory = interface(JObject)
['{BABFFFB3-AB4C-4626-86FE-BAD146F6C251}']
  {Methods}
  function createEffect(effectName: JString): JEffect; cdecl;
end;
TJEffectFactory = class(TJavaGenericImport<JEffectFactoryClass, JEffectFactory>) end;

JFaceDetector_FaceClass = interface(JObjectClass)
['{17134C3D-57CA-4AF8-8AF5-042B9891843E}']
  {Property Methods}
  function _GetCONFIDENCE_THRESHOLD: Single;
  function _GetEULER_X: Integer;
  function _GetEULER_Y: Integer;
  function _GetEULER_Z: Integer;
  {Properties}
  property CONFIDENCE_THRESHOLD: Single read _GetCONFIDENCE_THRESHOLD;
  property EULER_X: Integer read _GetEULER_X;
  property EULER_Y: Integer read _GetEULER_Y;
  property EULER_Z: Integer read _GetEULER_Z;
end;

[JavaSignature('android/media/FaceDetector$Face')]
JFaceDetector_Face = interface(JObject)
['{45D33BFD-93F8-4603-BA45-E24ED10ABE1E}']
  {Methods}
  function confidence: Single; cdecl;
  function eyesDistance: Single; cdecl;
  procedure getMidPoint(point: JPointF); cdecl;
  function pose(euler: Integer): Single; cdecl;
end;
TJFaceDetector_Face = class(TJavaGenericImport<JFaceDetector_FaceClass, JFaceDetector_Face>) end;

JAudioManagerClass = interface(JObjectClass)
['{9CE5F205-F003-485F-AF71-8F0DAB8346B8}']
  {Property Methods}
  function _GetACTION_AUDIO_BECOMING_NOISY: JString;
  function _GetACTION_SCO_AUDIO_STATE_CHANGED: JString;
  function _GetACTION_SCO_AUDIO_STATE_UPDATED: JString;
  function _GetADJUST_LOWER: Integer;
  function _GetADJUST_RAISE: Integer;
  function _GetADJUST_SAME: Integer;
  function _GetAUDIOFOCUS_GAIN: Integer;
  function _GetAUDIOFOCUS_GAIN_TRANSIENT: Integer;
  function _GetAUDIOFOCUS_GAIN_TRANSIENT_MAY_DUCK: Integer;
  function _GetAUDIOFOCUS_LOSS: Integer;
  function _GetAUDIOFOCUS_LOSS_TRANSIENT: Integer;
  function _GetAUDIOFOCUS_LOSS_TRANSIENT_CAN_DUCK: Integer;
  function _GetAUDIOFOCUS_REQUEST_FAILED: Integer;
  function _GetAUDIOFOCUS_REQUEST_GRANTED: Integer;
  function _GetEXTRA_RINGER_MODE: JString;
  function _GetEXTRA_SCO_AUDIO_PREVIOUS_STATE: JString;
  function _GetEXTRA_SCO_AUDIO_STATE: JString;
  function _GetEXTRA_VIBRATE_SETTING: JString;
  function _GetEXTRA_VIBRATE_TYPE: JString;
  function _GetFLAG_ALLOW_RINGER_MODES: Integer;
  function _GetFLAG_PLAY_SOUND: Integer;
  function _GetFLAG_REMOVE_SOUND_AND_VIBRATE: Integer;
  function _GetFLAG_SHOW_UI: Integer;
  function _GetFLAG_VIBRATE: Integer;
  function _GetFX_FOCUS_NAVIGATION_DOWN: Integer;
  function _GetFX_FOCUS_NAVIGATION_LEFT: Integer;
  function _GetFX_FOCUS_NAVIGATION_RIGHT: Integer;
  function _GetFX_FOCUS_NAVIGATION_UP: Integer;
  function _GetFX_KEYPRESS_DELETE: Integer;
  function _GetFX_KEYPRESS_RETURN: Integer;
  function _GetFX_KEYPRESS_SPACEBAR: Integer;
  function _GetFX_KEYPRESS_STANDARD: Integer;
  function _GetFX_KEY_CLICK: Integer;
  function _GetMODE_CURRENT: Integer;
  function _GetMODE_INVALID: Integer;
  function _GetMODE_IN_CALL: Integer;
  function _GetMODE_IN_COMMUNICATION: Integer;
  function _GetMODE_NORMAL: Integer;
  function _GetMODE_RINGTONE: Integer;
  function _GetNUM_STREAMS: Integer;
  function _GetPROPERTY_OUTPUT_FRAMES_PER_BUFFER: JString;
  function _GetPROPERTY_OUTPUT_SAMPLE_RATE: JString;
  function _GetRINGER_MODE_CHANGED_ACTION: JString;
  function _GetRINGER_MODE_NORMAL: Integer;
  function _GetRINGER_MODE_SILENT: Integer;
  function _GetRINGER_MODE_VIBRATE: Integer;
  function _GetROUTE_ALL: Integer;
  function _GetROUTE_BLUETOOTH: Integer;
  function _GetROUTE_BLUETOOTH_A2DP: Integer;
  function _GetROUTE_BLUETOOTH_SCO: Integer;
  function _GetROUTE_EARPIECE: Integer;
  function _GetROUTE_HEADSET: Integer;
  function _GetROUTE_SPEAKER: Integer;
  function _GetSCO_AUDIO_STATE_CONNECTED: Integer;
  function _GetSCO_AUDIO_STATE_CONNECTING: Integer;
  function _GetSCO_AUDIO_STATE_DISCONNECTED: Integer;
  function _GetSCO_AUDIO_STATE_ERROR: Integer;
  function _GetSTREAM_ALARM: Integer;
  function _GetSTREAM_DTMF: Integer;
  function _GetSTREAM_MUSIC: Integer;
  function _GetSTREAM_NOTIFICATION: Integer;
  function _GetSTREAM_RING: Integer;
  function _GetSTREAM_SYSTEM: Integer;
  function _GetSTREAM_VOICE_CALL: Integer;
  function _GetUSE_DEFAULT_STREAM_TYPE: Integer;
  function _GetVIBRATE_SETTING_CHANGED_ACTION: JString;
  function _GetVIBRATE_SETTING_OFF: Integer;
  function _GetVIBRATE_SETTING_ON: Integer;
  function _GetVIBRATE_SETTING_ONLY_SILENT: Integer;
  function _GetVIBRATE_TYPE_NOTIFICATION: Integer;
  function _GetVIBRATE_TYPE_RINGER: Integer;
  {Properties}
  property ACTION_AUDIO_BECOMING_NOISY: JString read _GetACTION_AUDIO_BECOMING_NOISY;
  property ACTION_SCO_AUDIO_STATE_CHANGED: JString read _GetACTION_SCO_AUDIO_STATE_CHANGED;
  property ACTION_SCO_AUDIO_STATE_UPDATED: JString read _GetACTION_SCO_AUDIO_STATE_UPDATED;
  property ADJUST_LOWER: Integer read _GetADJUST_LOWER;
  property ADJUST_RAISE: Integer read _GetADJUST_RAISE;
  property ADJUST_SAME: Integer read _GetADJUST_SAME;
  property AUDIOFOCUS_GAIN: Integer read _GetAUDIOFOCUS_GAIN;
  property AUDIOFOCUS_GAIN_TRANSIENT: Integer read _GetAUDIOFOCUS_GAIN_TRANSIENT;
  property AUDIOFOCUS_GAIN_TRANSIENT_MAY_DUCK: Integer read _GetAUDIOFOCUS_GAIN_TRANSIENT_MAY_DUCK;
  property AUDIOFOCUS_LOSS: Integer read _GetAUDIOFOCUS_LOSS;
  property AUDIOFOCUS_LOSS_TRANSIENT: Integer read _GetAUDIOFOCUS_LOSS_TRANSIENT;
  property AUDIOFOCUS_LOSS_TRANSIENT_CAN_DUCK: Integer read _GetAUDIOFOCUS_LOSS_TRANSIENT_CAN_DUCK;
  property AUDIOFOCUS_REQUEST_FAILED: Integer read _GetAUDIOFOCUS_REQUEST_FAILED;
  property AUDIOFOCUS_REQUEST_GRANTED: Integer read _GetAUDIOFOCUS_REQUEST_GRANTED;
  property EXTRA_RINGER_MODE: JString read _GetEXTRA_RINGER_MODE;
  property EXTRA_SCO_AUDIO_PREVIOUS_STATE: JString read _GetEXTRA_SCO_AUDIO_PREVIOUS_STATE;
  property EXTRA_SCO_AUDIO_STATE: JString read _GetEXTRA_SCO_AUDIO_STATE;
  property EXTRA_VIBRATE_SETTING: JString read _GetEXTRA_VIBRATE_SETTING;
  property EXTRA_VIBRATE_TYPE: JString read _GetEXTRA_VIBRATE_TYPE;
  property FLAG_ALLOW_RINGER_MODES: Integer read _GetFLAG_ALLOW_RINGER_MODES;
  property FLAG_PLAY_SOUND: Integer read _GetFLAG_PLAY_SOUND;
  property FLAG_REMOVE_SOUND_AND_VIBRATE: Integer read _GetFLAG_REMOVE_SOUND_AND_VIBRATE;
  property FLAG_SHOW_UI: Integer read _GetFLAG_SHOW_UI;
  property FLAG_VIBRATE: Integer read _GetFLAG_VIBRATE;
  property FX_FOCUS_NAVIGATION_DOWN: Integer read _GetFX_FOCUS_NAVIGATION_DOWN;
  property FX_FOCUS_NAVIGATION_LEFT: Integer read _GetFX_FOCUS_NAVIGATION_LEFT;
  property FX_FOCUS_NAVIGATION_RIGHT: Integer read _GetFX_FOCUS_NAVIGATION_RIGHT;
  property FX_FOCUS_NAVIGATION_UP: Integer read _GetFX_FOCUS_NAVIGATION_UP;
  property FX_KEYPRESS_DELETE: Integer read _GetFX_KEYPRESS_DELETE;
  property FX_KEYPRESS_RETURN: Integer read _GetFX_KEYPRESS_RETURN;
  property FX_KEYPRESS_SPACEBAR: Integer read _GetFX_KEYPRESS_SPACEBAR;
  property FX_KEYPRESS_STANDARD: Integer read _GetFX_KEYPRESS_STANDARD;
  property FX_KEY_CLICK: Integer read _GetFX_KEY_CLICK;
  property MODE_CURRENT: Integer read _GetMODE_CURRENT;
  property MODE_INVALID: Integer read _GetMODE_INVALID;
  property MODE_IN_CALL: Integer read _GetMODE_IN_CALL;
  property MODE_IN_COMMUNICATION: Integer read _GetMODE_IN_COMMUNICATION;
  property MODE_NORMAL: Integer read _GetMODE_NORMAL;
  property MODE_RINGTONE: Integer read _GetMODE_RINGTONE;
  property NUM_STREAMS: Integer read _GetNUM_STREAMS;
  property PROPERTY_OUTPUT_FRAMES_PER_BUFFER: JString read _GetPROPERTY_OUTPUT_FRAMES_PER_BUFFER;
  property PROPERTY_OUTPUT_SAMPLE_RATE: JString read _GetPROPERTY_OUTPUT_SAMPLE_RATE;
  property RINGER_MODE_CHANGED_ACTION: JString read _GetRINGER_MODE_CHANGED_ACTION;
  property RINGER_MODE_NORMAL: Integer read _GetRINGER_MODE_NORMAL;
  property RINGER_MODE_SILENT: Integer read _GetRINGER_MODE_SILENT;
  property RINGER_MODE_VIBRATE: Integer read _GetRINGER_MODE_VIBRATE;
  property ROUTE_ALL: Integer read _GetROUTE_ALL;
  property ROUTE_BLUETOOTH: Integer read _GetROUTE_BLUETOOTH;
  property ROUTE_BLUETOOTH_A2DP: Integer read _GetROUTE_BLUETOOTH_A2DP;
  property ROUTE_BLUETOOTH_SCO: Integer read _GetROUTE_BLUETOOTH_SCO;
  property ROUTE_EARPIECE: Integer read _GetROUTE_EARPIECE;
  property ROUTE_HEADSET: Integer read _GetROUTE_HEADSET;
  property ROUTE_SPEAKER: Integer read _GetROUTE_SPEAKER;
  property SCO_AUDIO_STATE_CONNECTED: Integer read _GetSCO_AUDIO_STATE_CONNECTED;
  property SCO_AUDIO_STATE_CONNECTING: Integer read _GetSCO_AUDIO_STATE_CONNECTING;
  property SCO_AUDIO_STATE_DISCONNECTED: Integer read _GetSCO_AUDIO_STATE_DISCONNECTED;
  property SCO_AUDIO_STATE_ERROR: Integer read _GetSCO_AUDIO_STATE_ERROR;
  property STREAM_ALARM: Integer read _GetSTREAM_ALARM;
  property STREAM_DTMF: Integer read _GetSTREAM_DTMF;
  property STREAM_MUSIC: Integer read _GetSTREAM_MUSIC;
  property STREAM_NOTIFICATION: Integer read _GetSTREAM_NOTIFICATION;
  property STREAM_RING: Integer read _GetSTREAM_RING;
  property STREAM_SYSTEM: Integer read _GetSTREAM_SYSTEM;
  property STREAM_VOICE_CALL: Integer read _GetSTREAM_VOICE_CALL;
  property USE_DEFAULT_STREAM_TYPE: Integer read _GetUSE_DEFAULT_STREAM_TYPE;
  property VIBRATE_SETTING_CHANGED_ACTION: JString read _GetVIBRATE_SETTING_CHANGED_ACTION;
  property VIBRATE_SETTING_OFF: Integer read _GetVIBRATE_SETTING_OFF;
  property VIBRATE_SETTING_ON: Integer read _GetVIBRATE_SETTING_ON;
  property VIBRATE_SETTING_ONLY_SILENT: Integer read _GetVIBRATE_SETTING_ONLY_SILENT;
  property VIBRATE_TYPE_NOTIFICATION: Integer read _GetVIBRATE_TYPE_NOTIFICATION;
  property VIBRATE_TYPE_RINGER: Integer read _GetVIBRATE_TYPE_RINGER;
end;

[JavaSignature('android/media/AudioManager')]
JAudioManager = interface(JObject)
['{C02E6757-33D6-4A65-99F4-F658813EF426}']
  {Methods}
  function abandonAudioFocus(l: JAudioManager_OnAudioFocusChangeListener): Integer; cdecl;
  procedure adjustStreamVolume(streamType: Integer; direction: Integer; flags: Integer); cdecl;
  procedure adjustSuggestedStreamVolume(direction: Integer; suggestedStreamType: Integer; flags: Integer); cdecl;
  procedure adjustVolume(direction: Integer; flags: Integer); cdecl;
  function getMode: Integer; cdecl;
  function getParameters(keys: JString): JString; cdecl;
  function getProperty(key: JString): JString; cdecl;
  function getRingerMode: Integer; cdecl;
  function getRouting(mode: Integer): Integer; cdecl;//Deprecated
  function getStreamMaxVolume(streamType: Integer): Integer; cdecl;
  function getStreamVolume(streamType: Integer): Integer; cdecl;
  function getVibrateSetting(vibrateType: Integer): Integer; cdecl;//Deprecated
  function isBluetoothA2dpOn: Boolean; cdecl;
  function isBluetoothScoAvailableOffCall: Boolean; cdecl;
  function isBluetoothScoOn: Boolean; cdecl;
  function isMicrophoneMute: Boolean; cdecl;
  function isMusicActive: Boolean; cdecl;
  function isSpeakerphoneOn: Boolean; cdecl;
  function isWiredHeadsetOn: Boolean; cdecl;//Deprecated
  procedure loadSoundEffects; cdecl;
  procedure playSoundEffect(effectType: Integer); cdecl; overload;
  procedure playSoundEffect(effectType: Integer; volume: Single); cdecl; overload;
  procedure registerMediaButtonEventReceiver(eventReceiver: JComponentName); cdecl;
  procedure registerRemoteControlClient(rcClient: JRemoteControlClient); cdecl;
  function requestAudioFocus(l: JAudioManager_OnAudioFocusChangeListener; streamType: Integer; durationHint: Integer): Integer; cdecl;
  procedure setBluetoothA2dpOn(on: Boolean); cdecl;//Deprecated
  procedure setBluetoothScoOn(on: Boolean); cdecl;
  procedure setMicrophoneMute(on: Boolean); cdecl;
  procedure setMode(mode: Integer); cdecl;
  procedure setParameters(keyValuePairs: JString); cdecl;
  procedure setRingerMode(ringerMode: Integer); cdecl;
  procedure setRouting(mode: Integer; routes: Integer; mask: Integer); cdecl;//Deprecated
  procedure setSpeakerphoneOn(on: Boolean); cdecl;
  procedure setStreamMute(streamType: Integer; state: Boolean); cdecl;
  procedure setStreamSolo(streamType: Integer; state: Boolean); cdecl;
  procedure setStreamVolume(streamType: Integer; index: Integer; flags: Integer); cdecl;
  procedure setVibrateSetting(vibrateType: Integer; vibrateSetting: Integer); cdecl;//Deprecated
  procedure setWiredHeadsetOn(on: Boolean); cdecl;//Deprecated
  function shouldVibrate(vibrateType: Integer): Boolean; cdecl;//Deprecated
  procedure startBluetoothSco; cdecl;
  procedure stopBluetoothSco; cdecl;
  procedure unloadSoundEffects; cdecl;
  procedure unregisterMediaButtonEventReceiver(eventReceiver: JComponentName); cdecl;
  procedure unregisterRemoteControlClient(rcClient: JRemoteControlClient); cdecl;
end;
TJAudioManager = class(TJavaGenericImport<JAudioManagerClass, JAudioManager>) end;

JAudioEffect_OnControlStatusChangeListenerClass = interface(IJavaClass)
['{77DA1E79-25E0-48F3-B55B-9D716AE5D500}']
end;

[JavaSignature('android/media/audiofx/AudioEffect$OnControlStatusChangeListener')]
JAudioEffect_OnControlStatusChangeListener = interface(IJavaInstance)
['{C3C4A416-A600-46BB-8710-0FCFEFA42076}']
  {Methods}
  procedure onControlStatusChange(effect: JAudioEffect; controlGranted: Boolean); cdecl;
end;
TJAudioEffect_OnControlStatusChangeListener = class(TJavaGenericImport<JAudioEffect_OnControlStatusChangeListenerClass, JAudioEffect_OnControlStatusChangeListener>) end;

JSoundPool_OnLoadCompleteListenerClass = interface(IJavaClass)
['{3BB55DBE-8614-4445-BA79-3EB818241E8C}']
end;

[JavaSignature('android/media/SoundPool$OnLoadCompleteListener')]
JSoundPool_OnLoadCompleteListener = interface(IJavaInstance)
['{E0F6A95C-27EF-4865-9C1C-DDD2664D58BD}']
  {Methods}
  procedure onLoadComplete(soundPool: JSoundPool; sampleId: Integer; status: Integer); cdecl;
end;
TJSoundPool_OnLoadCompleteListener = class(TJavaGenericImport<JSoundPool_OnLoadCompleteListenerClass, JSoundPool_OnLoadCompleteListener>) end;

JAudioTrack_OnPlaybackPositionUpdateListenerClass = interface(IJavaClass)
['{8D53A876-1D82-45AE-87A9-26C1E7CF5736}']
end;

[JavaSignature('android/media/AudioTrack$OnPlaybackPositionUpdateListener')]
JAudioTrack_OnPlaybackPositionUpdateListener = interface(IJavaInstance)
['{71399D16-1559-485C-900C-D25CFD06E25B}']
  {Methods}
  procedure onMarkerReached(track: JAudioTrack); cdecl;
  procedure onPeriodicNotification(track: JAudioTrack); cdecl;
end;
TJAudioTrack_OnPlaybackPositionUpdateListener = class(TJavaGenericImport<JAudioTrack_OnPlaybackPositionUpdateListenerClass, JAudioTrack_OnPlaybackPositionUpdateListener>) end;

JJetPlayer_OnJetEventListenerClass = interface(IJavaClass)
['{1B13E4B7-FC2B-4396-9E99-04D8FDE142F7}']
end;

[JavaSignature('android/media/JetPlayer$OnJetEventListener')]
JJetPlayer_OnJetEventListener = interface(IJavaInstance)
['{3F6F1880-0533-4008-91A0-EDA0C4C2EC62}']
  {Methods}
  procedure onJetEvent(player: JJetPlayer; segment: SmallInt; track: Byte; channel: Byte; controller: Byte; value: Byte); cdecl;
  procedure onJetNumQueuedSegmentUpdate(player: JJetPlayer; nbSegments: Integer); cdecl;
  procedure onJetPauseUpdate(player: JJetPlayer; paused: Integer); cdecl;
  procedure onJetUserIdUpdate(player: JJetPlayer; userId: Integer; repeatCount: Integer); cdecl;
end;
TJJetPlayer_OnJetEventListener = class(TJavaGenericImport<JJetPlayer_OnJetEventListenerClass, JJetPlayer_OnJetEventListener>) end;

JAudioEffectClass = interface(JObjectClass)
['{13CBADC9-1D73-46D0-9D39-50F50F8E5EE5}']
  {Property Methods}
  function _GetACTION_CLOSE_AUDIO_EFFECT_CONTROL_SESSION: JString;
  function _GetACTION_DISPLAY_AUDIO_EFFECT_CONTROL_PANEL: JString;
  function _GetACTION_OPEN_AUDIO_EFFECT_CONTROL_SESSION: JString;
  function _GetALREADY_EXISTS: Integer;
  function _GetCONTENT_TYPE_GAME: Integer;
  function _GetCONTENT_TYPE_MOVIE: Integer;
  function _GetCONTENT_TYPE_MUSIC: Integer;
  function _GetCONTENT_TYPE_VOICE: Integer;
  function _GetEFFECT_AUXILIARY: JString;
  function _GetEFFECT_INSERT: JString;
  function _GetERROR: Integer;
  function _GetERROR_BAD_VALUE: Integer;
  function _GetERROR_DEAD_OBJECT: Integer;
  function _GetERROR_INVALID_OPERATION: Integer;
  function _GetERROR_NO_INIT: Integer;
  function _GetERROR_NO_MEMORY: Integer;
  function _GetEXTRA_AUDIO_SESSION: JString;
  function _GetEXTRA_CONTENT_TYPE: JString;
  function _GetEXTRA_PACKAGE_NAME: JString;
  function _GetSUCCESS: Integer;
  {Methods}
  function queryEffects: TJavaObjectArray<JAudioEffect_Descriptor>; cdecl;
  {Properties}
  property ACTION_CLOSE_AUDIO_EFFECT_CONTROL_SESSION: JString read _GetACTION_CLOSE_AUDIO_EFFECT_CONTROL_SESSION;
  property ACTION_DISPLAY_AUDIO_EFFECT_CONTROL_PANEL: JString read _GetACTION_DISPLAY_AUDIO_EFFECT_CONTROL_PANEL;
  property ACTION_OPEN_AUDIO_EFFECT_CONTROL_SESSION: JString read _GetACTION_OPEN_AUDIO_EFFECT_CONTROL_SESSION;
  property ALREADY_EXISTS: Integer read _GetALREADY_EXISTS;
  property CONTENT_TYPE_GAME: Integer read _GetCONTENT_TYPE_GAME;
  property CONTENT_TYPE_MOVIE: Integer read _GetCONTENT_TYPE_MOVIE;
  property CONTENT_TYPE_MUSIC: Integer read _GetCONTENT_TYPE_MUSIC;
  property CONTENT_TYPE_VOICE: Integer read _GetCONTENT_TYPE_VOICE;
  property EFFECT_AUXILIARY: JString read _GetEFFECT_AUXILIARY;
  property EFFECT_INSERT: JString read _GetEFFECT_INSERT;
  property ERROR: Integer read _GetERROR;
  property ERROR_BAD_VALUE: Integer read _GetERROR_BAD_VALUE;
  property ERROR_DEAD_OBJECT: Integer read _GetERROR_DEAD_OBJECT;
  property ERROR_INVALID_OPERATION: Integer read _GetERROR_INVALID_OPERATION;
  property ERROR_NO_INIT: Integer read _GetERROR_NO_INIT;
  property ERROR_NO_MEMORY: Integer read _GetERROR_NO_MEMORY;
  property EXTRA_AUDIO_SESSION: JString read _GetEXTRA_AUDIO_SESSION;
  property EXTRA_CONTENT_TYPE: JString read _GetEXTRA_CONTENT_TYPE;
  property EXTRA_PACKAGE_NAME: JString read _GetEXTRA_PACKAGE_NAME;
  property SUCCESS: Integer read _GetSUCCESS;
end;

[JavaSignature('android/media/audiofx/AudioEffect')]
JAudioEffect = interface(JObject)
['{B494B3B3-1965-4831-9006-E22DC339A6C9}']
  {Methods}
  function getDescriptor: JAudioEffect_Descriptor; cdecl;
  function getEnabled: Boolean; cdecl;
  function getId: Integer; cdecl;
  function hasControl: Boolean; cdecl;
  procedure release; cdecl;
  procedure setControlStatusListener(listener: JAudioEffect_OnControlStatusChangeListener); cdecl;
  procedure setEnableStatusListener(listener: JAudioEffect_OnEnableStatusChangeListener); cdecl;
  function setEnabled(enabled: Boolean): Integer; cdecl;
end;
TJAudioEffect = class(TJavaGenericImport<JAudioEffectClass, JAudioEffect>) end;

JNoiseSuppressorClass = interface(JAudioEffectClass)
['{F7824EA5-778F-46DC-8D0C-FB914FE0B3AA}']
  {Methods}
  function create(audioSession: Integer): JNoiseSuppressor; cdecl;
  function isAvailable: Boolean; cdecl;
end;

[JavaSignature('android/media/audiofx/NoiseSuppressor')]
JNoiseSuppressor = interface(JAudioEffect)
['{2B24295A-1361-400E-9ECF-75B5A696A150}']
end;
TJNoiseSuppressor = class(TJavaGenericImport<JNoiseSuppressorClass, JNoiseSuppressor>) end;

JVirtualizer_SettingsClass = interface(JObjectClass)
['{B9BC83BF-D598-46EB-985D-C2155AF80A53}']
  {Methods}
  function init: JVirtualizer_Settings; cdecl; overload;
  function init(settings: JString): JVirtualizer_Settings; cdecl; overload;
end;

[JavaSignature('android/media/audiofx/Virtualizer$Settings')]
JVirtualizer_Settings = interface(JObject)
['{664E4688-59A7-42AC-B5D0-A63571A56A5A}']
  {Property Methods}
  function _Getstrength: SmallInt;
  procedure _Setstrength(Value: SmallInt);
  {Methods}
  function toString: JString; cdecl;
  {Properties}
  property strength: SmallInt read _Getstrength write _Setstrength;
end;
TJVirtualizer_Settings = class(TJavaGenericImport<JVirtualizer_SettingsClass, JVirtualizer_Settings>) end;

JMediaSyncEventClass = interface(JObjectClass)
['{79525AAA-24FE-4FE9-BBA4-E887C710A091}']
  {Property Methods}
  function _GetSYNC_EVENT_NONE: Integer;
  function _GetSYNC_EVENT_PRESENTATION_COMPLETE: Integer;
  {Methods}
  function createEvent(eventType: Integer): JMediaSyncEvent; cdecl;
  {Properties}
  property SYNC_EVENT_NONE: Integer read _GetSYNC_EVENT_NONE;
  property SYNC_EVENT_PRESENTATION_COMPLETE: Integer read _GetSYNC_EVENT_PRESENTATION_COMPLETE;
end;

[JavaSignature('android/media/MediaSyncEvent')]
JMediaSyncEvent = interface(JObject)
['{13BEE748-33EE-423F-9C9B-9C570F9C3A9A}']
  {Methods}
  function getAudioSessionId: Integer; cdecl;
  function getType: Integer; cdecl;
  function setAudioSessionId(audioSessionId: Integer): JMediaSyncEvent; cdecl;
end;
TJMediaSyncEvent = class(TJavaGenericImport<JMediaSyncEventClass, JMediaSyncEvent>) end;

JEffectContextClass = interface(JObjectClass)
['{D74230AA-9EF6-439C-A03C-B6D0B03D9821}']
  {Methods}
  function createWithCurrentGlContext: JEffectContext; cdecl;
end;

[JavaSignature('android/media/effect/EffectContext')]
JEffectContext = interface(JObject)
['{C963ECA3-2F09-44E5-948B-A564A49BBB5E}']
  {Methods}
  function getFactory: JEffectFactory; cdecl;
  procedure release; cdecl;
end;
TJEffectContext = class(TJavaGenericImport<JEffectContextClass, JEffectContext>) end;

JVirtualizer_OnParameterChangeListenerClass = interface(IJavaClass)
['{0B91AE60-FB61-4425-BC9E-E025BB1AB698}']
end;

[JavaSignature('android/media/audiofx/Virtualizer$OnParameterChangeListener')]
JVirtualizer_OnParameterChangeListener = interface(IJavaInstance)
['{B1BC6069-30E3-40E7-937C-AE8A51C12405}']
  {Methods}
  procedure onParameterChange(effect: JVirtualizer; status: Integer; param: Integer; value: SmallInt); cdecl;
end;
TJVirtualizer_OnParameterChangeListener = class(TJavaGenericImport<JVirtualizer_OnParameterChangeListenerClass, JVirtualizer_OnParameterChangeListener>) end;

JMediaScannerConnectionClass = interface(JObjectClass)
['{B3D9C29E-B436-469D-B694-181060E571DC}']
  {Methods}
  function init(context: JContext; client: JMediaScannerConnection_MediaScannerConnectionClient): JMediaScannerConnection; cdecl;
  procedure scanFile(context: JContext; paths: TJavaObjectArray<JString>; mimeTypes: TJavaObjectArray<JString>; callback: JMediaScannerConnection_OnScanCompletedListener); cdecl; overload;
end;

[JavaSignature('android/media/MediaScannerConnection')]
JMediaScannerConnection = interface(JObject)
['{CB9CBD70-C6AD-4FB5-8359-07F04747FB02}']
  {Methods}
  procedure connect; cdecl;
  procedure disconnect; cdecl;
  function isConnected: Boolean; cdecl;
  procedure onServiceConnected(className: JComponentName; service: JIBinder); cdecl;
  procedure onServiceDisconnected(className: JComponentName); cdecl;
  procedure scanFile(path: JString; mimeType: JString); cdecl; overload;
end;
TJMediaScannerConnection = class(TJavaGenericImport<JMediaScannerConnectionClass, JMediaScannerConnection>) end;

JThumbnailUtilsClass = interface(JObjectClass)
['{5D772E54-5912-4CF0-A97D-0A4B171E7EF7}']
  {Property Methods}
  function _GetOPTIONS_RECYCLE_INPUT: Integer;
  {Methods}
  function init: JThumbnailUtils; cdecl;
  function createVideoThumbnail(filePath: JString; kind: Integer): JBitmap; cdecl;
  function extractThumbnail(source: JBitmap; width: Integer; height: Integer): JBitmap; cdecl; overload;
  function extractThumbnail(source: JBitmap; width: Integer; height: Integer; options: Integer): JBitmap; cdecl; overload;
  {Properties}
  property OPTIONS_RECYCLE_INPUT: Integer read _GetOPTIONS_RECYCLE_INPUT;
end;

[JavaSignature('android/media/ThumbnailUtils')]
JThumbnailUtils = interface(JObject)
['{EF230179-DF54-4876-A9BC-5D982DF95E21}']
end;
TJThumbnailUtils = class(TJavaGenericImport<JThumbnailUtilsClass, JThumbnailUtils>) end;

JRemoteControlClient_MetadataEditorClass = interface(JObjectClass)
['{C27FF739-A30D-4355-82E4-E29C5068899F}']
  {Property Methods}
  function _GetBITMAP_KEY_ARTWORK: Integer;
  {Properties}
  property BITMAP_KEY_ARTWORK: Integer read _GetBITMAP_KEY_ARTWORK;
end;

[JavaSignature('android/media/RemoteControlClient$MetadataEditor')]
JRemoteControlClient_MetadataEditor = interface(JObject)
['{C73C0E46-3442-440C-8A3D-E583A926B37B}']
  {Methods}
  procedure apply; cdecl;
  procedure clear; cdecl;
  function putBitmap(key: Integer; bitmap: JBitmap): JRemoteControlClient_MetadataEditor; cdecl;
  function putLong(key: Integer; value: Int64): JRemoteControlClient_MetadataEditor; cdecl;
  function putString(key: Integer; value: JString): JRemoteControlClient_MetadataEditor; cdecl;
end;
TJRemoteControlClient_MetadataEditor = class(TJavaGenericImport<JRemoteControlClient_MetadataEditorClass, JRemoteControlClient_MetadataEditor>) end;

JRingtoneManagerClass = interface(JObjectClass)
['{8AFA6E70-C245-4694-8B7E-72C5D436FD94}']
  {Property Methods}
  function _GetACTION_RINGTONE_PICKER: JString;
  function _GetEXTRA_RINGTONE_DEFAULT_URI: JString;
  function _GetEXTRA_RINGTONE_EXISTING_URI: JString;
  function _GetEXTRA_RINGTONE_INCLUDE_DRM: JString;
  function _GetEXTRA_RINGTONE_PICKED_URI: JString;
  function _GetEXTRA_RINGTONE_SHOW_DEFAULT: JString;
  function _GetEXTRA_RINGTONE_SHOW_SILENT: JString;
  function _GetEXTRA_RINGTONE_TITLE: JString;
  function _GetEXTRA_RINGTONE_TYPE: JString;
  function _GetID_COLUMN_INDEX: Integer;
  function _GetTITLE_COLUMN_INDEX: Integer;
  function _GetTYPE_ALARM: Integer;
  function _GetTYPE_ALL: Integer;
  function _GetTYPE_NOTIFICATION: Integer;
  function _GetTYPE_RINGTONE: Integer;
  function _GetURI_COLUMN_INDEX: Integer;
  {Methods}
  function init(activity: JActivity): JRingtoneManager; cdecl; overload;
  function init(context: JContext): JRingtoneManager; cdecl; overload;
  function getActualDefaultRingtoneUri(context: JContext; type_: Integer): Jnet_Uri; cdecl;
  function getDefaultType(defaultRingtoneUri: Jnet_Uri): Integer; cdecl;
  function getDefaultUri(type_: Integer): Jnet_Uri; cdecl;
  function getRingtone(context: JContext; ringtoneUri: Jnet_Uri): JRingtone; cdecl; overload;
  function getValidRingtoneUri(context: JContext): Jnet_Uri; cdecl;
  function isDefault(ringtoneUri: Jnet_Uri): Boolean; cdecl;
  procedure setActualDefaultRingtoneUri(context: JContext; type_: Integer; ringtoneUri: Jnet_Uri); cdecl;
  {Properties}
  property ACTION_RINGTONE_PICKER: JString read _GetACTION_RINGTONE_PICKER;
  property EXTRA_RINGTONE_DEFAULT_URI: JString read _GetEXTRA_RINGTONE_DEFAULT_URI;
  property EXTRA_RINGTONE_EXISTING_URI: JString read _GetEXTRA_RINGTONE_EXISTING_URI;
  property EXTRA_RINGTONE_INCLUDE_DRM: JString read _GetEXTRA_RINGTONE_INCLUDE_DRM;
  property EXTRA_RINGTONE_PICKED_URI: JString read _GetEXTRA_RINGTONE_PICKED_URI;
  property EXTRA_RINGTONE_SHOW_DEFAULT: JString read _GetEXTRA_RINGTONE_SHOW_DEFAULT;
  property EXTRA_RINGTONE_SHOW_SILENT: JString read _GetEXTRA_RINGTONE_SHOW_SILENT;
  property EXTRA_RINGTONE_TITLE: JString read _GetEXTRA_RINGTONE_TITLE;
  property EXTRA_RINGTONE_TYPE: JString read _GetEXTRA_RINGTONE_TYPE;
  property ID_COLUMN_INDEX: Integer read _GetID_COLUMN_INDEX;
  property TITLE_COLUMN_INDEX: Integer read _GetTITLE_COLUMN_INDEX;
  property TYPE_ALARM: Integer read _GetTYPE_ALARM;
  property TYPE_ALL: Integer read _GetTYPE_ALL;
  property TYPE_NOTIFICATION: Integer read _GetTYPE_NOTIFICATION;
  property TYPE_RINGTONE: Integer read _GetTYPE_RINGTONE;
  property URI_COLUMN_INDEX: Integer read _GetURI_COLUMN_INDEX;
end;

[JavaSignature('android/media/RingtoneManager')]
JRingtoneManager = interface(JObject)
['{1BD04E09-7368-4024-9231-7C23D839447C}']
  {Methods}
  function getCursor: JCursor; cdecl;
  function getIncludeDrm: Boolean; cdecl;
  function getRingtone(position: Integer): JRingtone; cdecl; overload;
  function getRingtonePosition(ringtoneUri: Jnet_Uri): Integer; cdecl;
  function getRingtoneUri(position: Integer): Jnet_Uri; cdecl;
  function getStopPreviousRingtone: Boolean; cdecl;
  function inferStreamType: Integer; cdecl;
  procedure setIncludeDrm(includeDrm: Boolean); cdecl;
  procedure setStopPreviousRingtone(stopPreviousRingtone: Boolean); cdecl;
  procedure setType(type_: Integer); cdecl;
  procedure stopPreviousRingtone; cdecl;
end;
TJRingtoneManager = class(TJavaGenericImport<JRingtoneManagerClass, JRingtoneManager>) end;

JCamcorderProfileClass = interface(JObjectClass)
['{74CB0E77-4E93-4724-A3AA-EEE1C29C90AD}']
  {Property Methods}
  function _GetQUALITY_1080P: Integer;
  function _GetQUALITY_480P: Integer;
  function _GetQUALITY_720P: Integer;
  function _GetQUALITY_CIF: Integer;
  function _GetQUALITY_HIGH: Integer;
  function _GetQUALITY_LOW: Integer;
  function _GetQUALITY_QCIF: Integer;
  function _GetQUALITY_QVGA: Integer;
  function _GetQUALITY_TIME_LAPSE_1080P: Integer;
  function _GetQUALITY_TIME_LAPSE_480P: Integer;
  function _GetQUALITY_TIME_LAPSE_720P: Integer;
  function _GetQUALITY_TIME_LAPSE_CIF: Integer;
  function _GetQUALITY_TIME_LAPSE_HIGH: Integer;
  function _GetQUALITY_TIME_LAPSE_LOW: Integer;
  function _GetQUALITY_TIME_LAPSE_QCIF: Integer;
  function _GetQUALITY_TIME_LAPSE_QVGA: Integer;
  {Methods}
  function &get(quality: Integer): JCamcorderProfile; cdecl; overload;
  function &get(cameraId: Integer; quality: Integer): JCamcorderProfile; cdecl; overload;
  function hasProfile(quality: Integer): Boolean; cdecl; overload;
  function hasProfile(cameraId: Integer; quality: Integer): Boolean; cdecl; overload;
  {Properties}
  property QUALITY_1080P: Integer read _GetQUALITY_1080P;
  property QUALITY_480P: Integer read _GetQUALITY_480P;
  property QUALITY_720P: Integer read _GetQUALITY_720P;
  property QUALITY_CIF: Integer read _GetQUALITY_CIF;
  property QUALITY_HIGH: Integer read _GetQUALITY_HIGH;
  property QUALITY_LOW: Integer read _GetQUALITY_LOW;
  property QUALITY_QCIF: Integer read _GetQUALITY_QCIF;
  property QUALITY_QVGA: Integer read _GetQUALITY_QVGA;
  property QUALITY_TIME_LAPSE_1080P: Integer read _GetQUALITY_TIME_LAPSE_1080P;
  property QUALITY_TIME_LAPSE_480P: Integer read _GetQUALITY_TIME_LAPSE_480P;
  property QUALITY_TIME_LAPSE_720P: Integer read _GetQUALITY_TIME_LAPSE_720P;
  property QUALITY_TIME_LAPSE_CIF: Integer read _GetQUALITY_TIME_LAPSE_CIF;
  property QUALITY_TIME_LAPSE_HIGH: Integer read _GetQUALITY_TIME_LAPSE_HIGH;
  property QUALITY_TIME_LAPSE_LOW: Integer read _GetQUALITY_TIME_LAPSE_LOW;
  property QUALITY_TIME_LAPSE_QCIF: Integer read _GetQUALITY_TIME_LAPSE_QCIF;
  property QUALITY_TIME_LAPSE_QVGA: Integer read _GetQUALITY_TIME_LAPSE_QVGA;
end;

[JavaSignature('android/media/CamcorderProfile')]
JCamcorderProfile = interface(JObject)
['{2957E3CA-A488-4829-84DE-F15DEA6F7FFB}']
  {Property Methods}
  function _GetaudioBitRate: Integer;
  procedure _SetaudioBitRate(Value: Integer);
  function _GetaudioChannels: Integer;
  procedure _SetaudioChannels(Value: Integer);
  function _GetaudioCodec: Integer;
  procedure _SetaudioCodec(Value: Integer);
  function _GetaudioSampleRate: Integer;
  procedure _SetaudioSampleRate(Value: Integer);
  function _Getduration: Integer;
  procedure _Setduration(Value: Integer);
  function _GetfileFormat: Integer;
  procedure _SetfileFormat(Value: Integer);
  function _Getquality: Integer;
  procedure _Setquality(Value: Integer);
  function _GetvideoBitRate: Integer;
  procedure _SetvideoBitRate(Value: Integer);
  function _GetvideoCodec: Integer;
  procedure _SetvideoCodec(Value: Integer);
  function _GetvideoFrameHeight: Integer;
  procedure _SetvideoFrameHeight(Value: Integer);
  function _GetvideoFrameRate: Integer;
  procedure _SetvideoFrameRate(Value: Integer);
  function _GetvideoFrameWidth: Integer;
  procedure _SetvideoFrameWidth(Value: Integer);
  {Properties}
  property audioBitRate: Integer read _GetaudioBitRate write _SetaudioBitRate;
  property audioChannels: Integer read _GetaudioChannels write _SetaudioChannels;
  property audioCodec: Integer read _GetaudioCodec write _SetaudioCodec;
  property audioSampleRate: Integer read _GetaudioSampleRate write _SetaudioSampleRate;
  property duration: Integer read _Getduration write _Setduration;
  property fileFormat: Integer read _GetfileFormat write _SetfileFormat;
  property quality: Integer read _Getquality write _Setquality;
  property videoBitRate: Integer read _GetvideoBitRate write _SetvideoBitRate;
  property videoCodec: Integer read _GetvideoCodec write _SetvideoCodec;
  property videoFrameHeight: Integer read _GetvideoFrameHeight write _SetvideoFrameHeight;
  property videoFrameRate: Integer read _GetvideoFrameRate write _SetvideoFrameRate;
  property videoFrameWidth: Integer read _GetvideoFrameWidth write _SetvideoFrameWidth;
end;
TJCamcorderProfile = class(TJavaGenericImport<JCamcorderProfileClass, JCamcorderProfile>) end;

JVisualizer_OnDataCaptureListenerClass = interface(IJavaClass)
['{3CCB3AC2-5737-486B-987E-88C328277426}']
end;

[JavaSignature('android/media/audiofx/Visualizer$OnDataCaptureListener')]
JVisualizer_OnDataCaptureListener = interface(IJavaInstance)
['{092C4DEF-238A-4053-A59B-04136A5C3820}']
  {Methods}
  procedure onFftDataCapture(visualizer: JVisualizer; fft: TJavaArray<Byte>; samplingRate: Integer); cdecl;
  procedure onWaveFormDataCapture(visualizer: JVisualizer; waveform: TJavaArray<Byte>; samplingRate: Integer); cdecl;
end;
TJVisualizer_OnDataCaptureListener = class(TJavaGenericImport<JVisualizer_OnDataCaptureListenerClass, JVisualizer_OnDataCaptureListener>) end;

JMediaPlayer_OnVideoSizeChangedListenerClass = interface(IJavaClass)
['{3693771D-E38F-4C43-9FC2-AAB16CBF4DA3}']
end;

[JavaSignature('android/media/MediaPlayer$OnVideoSizeChangedListener')]
JMediaPlayer_OnVideoSizeChangedListener = interface(IJavaInstance)
['{37F96835-D256-4DAF-B0F0-17B653F8ECB9}']
  {Methods}
  procedure onVideoSizeChanged(mp: JMediaPlayer; width: Integer; height: Integer); cdecl;
end;
TJMediaPlayer_OnVideoSizeChangedListener = class(TJavaGenericImport<JMediaPlayer_OnVideoSizeChangedListenerClass, JMediaPlayer_OnVideoSizeChangedListener>) end;

JAudioTrackClass = interface(JObjectClass)
['{F1B69999-E4BF-4586-B05F-708A2172FC51}']
  {Property Methods}
  function _GetERROR: Integer;
  function _GetERROR_BAD_VALUE: Integer;
  function _GetERROR_INVALID_OPERATION: Integer;
  function _GetMODE_STATIC: Integer;
  function _GetMODE_STREAM: Integer;
  function _GetPLAYSTATE_PAUSED: Integer;
  function _GetPLAYSTATE_PLAYING: Integer;
  function _GetPLAYSTATE_STOPPED: Integer;
  function _GetSTATE_INITIALIZED: Integer;
  function _GetSTATE_NO_STATIC_DATA: Integer;
  function _GetSTATE_UNINITIALIZED: Integer;
  function _GetSUCCESS: Integer;
  {Methods}
  function init(streamType: Integer; sampleRateInHz: Integer; channelConfig: Integer; audioFormat: Integer; bufferSizeInBytes: Integer; mode: Integer): JAudioTrack; cdecl; overload;
  function init(streamType: Integer; sampleRateInHz: Integer; channelConfig: Integer; audioFormat: Integer; bufferSizeInBytes: Integer; mode: Integer; sessionId: Integer): JAudioTrack; cdecl; overload;
  function getMaxVolume: Single; cdecl;
  function getMinBufferSize(sampleRateInHz: Integer; channelConfig: Integer; audioFormat: Integer): Integer; cdecl;
  function getMinVolume: Single; cdecl;
  function getNativeOutputSampleRate(streamType: Integer): Integer; cdecl;
  {Properties}
  property ERROR: Integer read _GetERROR;
  property ERROR_BAD_VALUE: Integer read _GetERROR_BAD_VALUE;
  property ERROR_INVALID_OPERATION: Integer read _GetERROR_INVALID_OPERATION;
  property MODE_STATIC: Integer read _GetMODE_STATIC;
  property MODE_STREAM: Integer read _GetMODE_STREAM;
  property PLAYSTATE_PAUSED: Integer read _GetPLAYSTATE_PAUSED;
  property PLAYSTATE_PLAYING: Integer read _GetPLAYSTATE_PLAYING;
  property PLAYSTATE_STOPPED: Integer read _GetPLAYSTATE_STOPPED;
  property STATE_INITIALIZED: Integer read _GetSTATE_INITIALIZED;
  property STATE_NO_STATIC_DATA: Integer read _GetSTATE_NO_STATIC_DATA;
  property STATE_UNINITIALIZED: Integer read _GetSTATE_UNINITIALIZED;
  property SUCCESS: Integer read _GetSUCCESS;
end;

[JavaSignature('android/media/AudioTrack')]
JAudioTrack = interface(JObject)
['{84D3E8CB-9DDC-4DCB-902C-43C9AECE0830}']
  {Methods}
  function attachAuxEffect(effectId: Integer): Integer; cdecl;
  procedure flush; cdecl;
  function getAudioFormat: Integer; cdecl;
  function getAudioSessionId: Integer; cdecl;
  function getChannelConfiguration: Integer; cdecl;
  function getChannelCount: Integer; cdecl;
  function getNotificationMarkerPosition: Integer; cdecl;
  function getPlayState: Integer; cdecl;
  function getPlaybackHeadPosition: Integer; cdecl;
  function getPlaybackRate: Integer; cdecl;
  function getPositionNotificationPeriod: Integer; cdecl;
  function getSampleRate: Integer; cdecl;
  function getState: Integer; cdecl;
  function getStreamType: Integer; cdecl;
  procedure pause; cdecl;
  procedure play; cdecl;
  procedure release; cdecl;
  function reloadStaticData: Integer; cdecl;
  function setAuxEffectSendLevel(level: Single): Integer; cdecl;
  function setLoopPoints(startInFrames: Integer; endInFrames: Integer; loopCount: Integer): Integer; cdecl;
  function setNotificationMarkerPosition(markerInFrames: Integer): Integer; cdecl;
  function setPlaybackHeadPosition(positionInFrames: Integer): Integer; cdecl;
  procedure setPlaybackPositionUpdateListener(listener: JAudioTrack_OnPlaybackPositionUpdateListener); cdecl; overload;
  procedure setPlaybackPositionUpdateListener(listener: JAudioTrack_OnPlaybackPositionUpdateListener; handler: JHandler); cdecl; overload;
  function setPlaybackRate(sampleRateInHz: Integer): Integer; cdecl;
  function setPositionNotificationPeriod(periodInFrames: Integer): Integer; cdecl;
  function setStereoVolume(leftVolume: Single; rightVolume: Single): Integer; cdecl;
  procedure stop; cdecl;
  function write(audioData: TJavaArray<Byte>; offsetInBytes: Integer; sizeInBytes: Integer): Integer; cdecl; overload;
  function write(audioData: TJavaArray<SmallInt>; offsetInShorts: Integer; sizeInShorts: Integer): Integer; cdecl; overload;
end;
TJAudioTrack = class(TJavaGenericImport<JAudioTrackClass, JAudioTrack>) end;

JVisualizerClass = interface(JObjectClass)
['{8DBB66F3-CE56-4034-8FCA-1D4D88599192}']
  {Property Methods}
  function _GetALREADY_EXISTS: Integer;
  function _GetERROR: Integer;
  function _GetERROR_BAD_VALUE: Integer;
  function _GetERROR_DEAD_OBJECT: Integer;
  function _GetERROR_INVALID_OPERATION: Integer;
  function _GetERROR_NO_INIT: Integer;
  function _GetERROR_NO_MEMORY: Integer;
  function _GetSCALING_MODE_AS_PLAYED: Integer;
  function _GetSCALING_MODE_NORMALIZED: Integer;
  function _GetSTATE_ENABLED: Integer;
  function _GetSTATE_INITIALIZED: Integer;
  function _GetSTATE_UNINITIALIZED: Integer;
  function _GetSUCCESS: Integer;
  {Methods}
  function init(audioSession: Integer): JVisualizer; cdecl;
  function getCaptureSizeRange: TJavaArray<Integer>; cdecl;
  function getMaxCaptureRate: Integer; cdecl;
  {Properties}
  property ALREADY_EXISTS: Integer read _GetALREADY_EXISTS;
  property ERROR: Integer read _GetERROR;
  property ERROR_BAD_VALUE: Integer read _GetERROR_BAD_VALUE;
  property ERROR_DEAD_OBJECT: Integer read _GetERROR_DEAD_OBJECT;
  property ERROR_INVALID_OPERATION: Integer read _GetERROR_INVALID_OPERATION;
  property ERROR_NO_INIT: Integer read _GetERROR_NO_INIT;
  property ERROR_NO_MEMORY: Integer read _GetERROR_NO_MEMORY;
  property SCALING_MODE_AS_PLAYED: Integer read _GetSCALING_MODE_AS_PLAYED;
  property SCALING_MODE_NORMALIZED: Integer read _GetSCALING_MODE_NORMALIZED;
  property STATE_ENABLED: Integer read _GetSTATE_ENABLED;
  property STATE_INITIALIZED: Integer read _GetSTATE_INITIALIZED;
  property STATE_UNINITIALIZED: Integer read _GetSTATE_UNINITIALIZED;
  property SUCCESS: Integer read _GetSUCCESS;
end;

[JavaSignature('android/media/audiofx/Visualizer')]
JVisualizer = interface(JObject)
['{6796AEB3-D2B1-4840-9182-92C09375E8E6}']
  {Methods}
  function getCaptureSize: Integer; cdecl;
  function getEnabled: Boolean; cdecl;
  function getFft(fft: TJavaArray<Byte>): Integer; cdecl;
  function getSamplingRate: Integer; cdecl;
  function getScalingMode: Integer; cdecl;
  function getWaveForm(waveform: TJavaArray<Byte>): Integer; cdecl;
  procedure release; cdecl;
  function setCaptureSize(size: Integer): Integer; cdecl;
  function setDataCaptureListener(listener: JVisualizer_OnDataCaptureListener; rate: Integer; waveform: Boolean; fft: Boolean): Integer; cdecl;
  function setEnabled(enabled: Boolean): Integer; cdecl;
  function setScalingMode(mode: Integer): Integer; cdecl;
end;
TJVisualizer = class(TJavaGenericImport<JVisualizerClass, JVisualizer>) end;

JMediaRouter_CallbackClass = interface(JObjectClass)
['{CE16DA8F-E295-40D7-BDB9-90B613FEF489}']
  {Methods}
  function init: JMediaRouter_Callback; cdecl;
end;

[JavaSignature('android/media/MediaRouter$Callback')]
JMediaRouter_Callback = interface(JObject)
['{4B9C5746-9823-437E-8109-AE475A504BA9}']
  {Methods}
  procedure onRouteAdded(router: JMediaRouter; info: JMediaRouter_RouteInfo); cdecl;
  procedure onRouteChanged(router: JMediaRouter; info: JMediaRouter_RouteInfo); cdecl;
  procedure onRouteGrouped(router: JMediaRouter; info: JMediaRouter_RouteInfo; group: JMediaRouter_RouteGroup; index: Integer); cdecl;
  procedure onRoutePresentationDisplayChanged(router: JMediaRouter; info: JMediaRouter_RouteInfo); cdecl;
  procedure onRouteRemoved(router: JMediaRouter; info: JMediaRouter_RouteInfo); cdecl;
  procedure onRouteSelected(router: JMediaRouter; type_: Integer; info: JMediaRouter_RouteInfo); cdecl;
  procedure onRouteUngrouped(router: JMediaRouter; info: JMediaRouter_RouteInfo; group: JMediaRouter_RouteGroup); cdecl;
  procedure onRouteUnselected(router: JMediaRouter; type_: Integer; info: JMediaRouter_RouteInfo); cdecl;
  procedure onRouteVolumeChanged(router: JMediaRouter; info: JMediaRouter_RouteInfo); cdecl;
end;
TJMediaRouter_Callback = class(TJavaGenericImport<JMediaRouter_CallbackClass, JMediaRouter_Callback>) end;

JMediaRouter_SimpleCallbackClass = interface(JMediaRouter_CallbackClass)
['{FE51271A-3E43-46CC-B7C7-1642F6906114}']
  {Methods}
  function init: JMediaRouter_SimpleCallback; cdecl;
end;

[JavaSignature('android/media/MediaRouter$SimpleCallback')]
JMediaRouter_SimpleCallback = interface(JMediaRouter_Callback)
['{7CA629D1-D9D7-4F6A-85BA-4C3F81C87BD8}']
  {Methods}
  procedure onRouteAdded(router: JMediaRouter; info: JMediaRouter_RouteInfo); cdecl;
  procedure onRouteChanged(router: JMediaRouter; info: JMediaRouter_RouteInfo); cdecl;
  procedure onRouteGrouped(router: JMediaRouter; info: JMediaRouter_RouteInfo; group: JMediaRouter_RouteGroup; index: Integer); cdecl;
  procedure onRouteRemoved(router: JMediaRouter; info: JMediaRouter_RouteInfo); cdecl;
  procedure onRouteSelected(router: JMediaRouter; type_: Integer; info: JMediaRouter_RouteInfo); cdecl;
  procedure onRouteUngrouped(router: JMediaRouter; info: JMediaRouter_RouteInfo; group: JMediaRouter_RouteGroup); cdecl;
  procedure onRouteUnselected(router: JMediaRouter; type_: Integer; info: JMediaRouter_RouteInfo); cdecl;
  procedure onRouteVolumeChanged(router: JMediaRouter; info: JMediaRouter_RouteInfo); cdecl;
end;
TJMediaRouter_SimpleCallback = class(TJavaGenericImport<JMediaRouter_SimpleCallbackClass, JMediaRouter_SimpleCallback>) end;

JEnvironmentalReverb_SettingsClass = interface(JObjectClass)
['{552A5CB8-6E2A-4F05-ABCB-0C4947BADB69}']
  {Methods}
  function init: JEnvironmentalReverb_Settings; cdecl; overload;
  function init(settings: JString): JEnvironmentalReverb_Settings; cdecl; overload;
end;

[JavaSignature('android/media/audiofx/EnvironmentalReverb$Settings')]
JEnvironmentalReverb_Settings = interface(JObject)
['{8456E091-3040-4D39-A819-68433D6B5C08}']
  {Property Methods}
  function _GetdecayHFRatio: SmallInt;
  procedure _SetdecayHFRatio(Value: SmallInt);
  function _GetdecayTime: Integer;
  procedure _SetdecayTime(Value: Integer);
  function _Getdensity: SmallInt;
  procedure _Setdensity(Value: SmallInt);
  function _Getdiffusion: SmallInt;
  procedure _Setdiffusion(Value: SmallInt);
  function _GetreflectionsDelay: Integer;
  procedure _SetreflectionsDelay(Value: Integer);
  function _GetreflectionsLevel: SmallInt;
  procedure _SetreflectionsLevel(Value: SmallInt);
  function _GetreverbDelay: Integer;
  procedure _SetreverbDelay(Value: Integer);
  function _GetreverbLevel: SmallInt;
  procedure _SetreverbLevel(Value: SmallInt);
  function _GetroomHFLevel: SmallInt;
  procedure _SetroomHFLevel(Value: SmallInt);
  function _GetroomLevel: SmallInt;
  procedure _SetroomLevel(Value: SmallInt);
  {Methods}
  function toString: JString; cdecl;
  {Properties}
  property decayHFRatio: SmallInt read _GetdecayHFRatio write _SetdecayHFRatio;
  property decayTime: Integer read _GetdecayTime write _SetdecayTime;
  property density: SmallInt read _Getdensity write _Setdensity;
  property diffusion: SmallInt read _Getdiffusion write _Setdiffusion;
  property reflectionsDelay: Integer read _GetreflectionsDelay write _SetreflectionsDelay;
  property reflectionsLevel: SmallInt read _GetreflectionsLevel write _SetreflectionsLevel;
  property reverbDelay: Integer read _GetreverbDelay write _SetreverbDelay;
  property reverbLevel: SmallInt read _GetreverbLevel write _SetreverbLevel;
  property roomHFLevel: SmallInt read _GetroomHFLevel write _SetroomHFLevel;
  property roomLevel: SmallInt read _GetroomLevel write _SetroomLevel;
end;
TJEnvironmentalReverb_Settings = class(TJavaGenericImport<JEnvironmentalReverb_SettingsClass, JEnvironmentalReverb_Settings>) end;

JAudioRecord_OnRecordPositionUpdateListenerClass = interface(IJavaClass)
['{61883778-37ED-4EE0-9039-ED97AA45C8FD}']
end;

[JavaSignature('android/media/AudioRecord$OnRecordPositionUpdateListener')]
JAudioRecord_OnRecordPositionUpdateListener = interface(IJavaInstance)
['{C465EEDB-9E94-4687-B81D-9A0194874655}']
  {Methods}
  procedure onMarkerReached(recorder: JAudioRecord); cdecl;
  procedure onPeriodicNotification(recorder: JAudioRecord); cdecl;
end;
TJAudioRecord_OnRecordPositionUpdateListener = class(TJavaGenericImport<JAudioRecord_OnRecordPositionUpdateListenerClass, JAudioRecord_OnRecordPositionUpdateListener>) end;

JMediaCodecInfo_CodecProfileLevelClass = interface(JObjectClass)
['{E162DC0D-F8D3-4572-8029-474D4691B5E0}']
  {Property Methods}
  function _GetAACObjectELD: Integer;
  function _GetAACObjectERLC: Integer;
  function _GetAACObjectHE: Integer;
  function _GetAACObjectHE_PS: Integer;
  function _GetAACObjectLC: Integer;
  function _GetAACObjectLD: Integer;
  function _GetAACObjectLTP: Integer;
  function _GetAACObjectMain: Integer;
  function _GetAACObjectSSR: Integer;
  function _GetAACObjectScalable: Integer;
  function _GetAVCLevel1: Integer;
  function _GetAVCLevel11: Integer;
  function _GetAVCLevel12: Integer;
  function _GetAVCLevel13: Integer;
  function _GetAVCLevel1b: Integer;
  function _GetAVCLevel2: Integer;
  function _GetAVCLevel21: Integer;
  function _GetAVCLevel22: Integer;
  function _GetAVCLevel3: Integer;
  function _GetAVCLevel31: Integer;
  function _GetAVCLevel32: Integer;
  function _GetAVCLevel4: Integer;
  function _GetAVCLevel41: Integer;
  function _GetAVCLevel42: Integer;
  function _GetAVCLevel5: Integer;
  function _GetAVCLevel51: Integer;
  function _GetAVCProfileBaseline: Integer;
  function _GetAVCProfileExtended: Integer;
  function _GetAVCProfileHigh: Integer;
  function _GetAVCProfileHigh10: Integer;
  function _GetAVCProfileHigh422: Integer;
  function _GetAVCProfileHigh444: Integer;
  function _GetAVCProfileMain: Integer;
  function _GetH263Level10: Integer;
  function _GetH263Level20: Integer;
  function _GetH263Level30: Integer;
  function _GetH263Level40: Integer;
  function _GetH263Level45: Integer;
  function _GetH263Level50: Integer;
  function _GetH263Level60: Integer;
  function _GetH263Level70: Integer;
  function _GetH263ProfileBackwardCompatible: Integer;
  function _GetH263ProfileBaseline: Integer;
  function _GetH263ProfileH320Coding: Integer;
  function _GetH263ProfileHighCompression: Integer;
  function _GetH263ProfileHighLatency: Integer;
  function _GetH263ProfileISWV2: Integer;
  function _GetH263ProfileISWV3: Integer;
  function _GetH263ProfileInterlace: Integer;
  function _GetH263ProfileInternet: Integer;
  function _GetMPEG4Level0: Integer;
  function _GetMPEG4Level0b: Integer;
  function _GetMPEG4Level1: Integer;
  function _GetMPEG4Level2: Integer;
  function _GetMPEG4Level3: Integer;
  function _GetMPEG4Level4: Integer;
  function _GetMPEG4Level4a: Integer;
  function _GetMPEG4Level5: Integer;
  function _GetMPEG4ProfileAdvancedCoding: Integer;
  function _GetMPEG4ProfileAdvancedCore: Integer;
  function _GetMPEG4ProfileAdvancedRealTime: Integer;
  function _GetMPEG4ProfileAdvancedScalable: Integer;
  function _GetMPEG4ProfileAdvancedSimple: Integer;
  function _GetMPEG4ProfileBasicAnimated: Integer;
  function _GetMPEG4ProfileCore: Integer;
  function _GetMPEG4ProfileCoreScalable: Integer;
  function _GetMPEG4ProfileHybrid: Integer;
  function _GetMPEG4ProfileMain: Integer;
  function _GetMPEG4ProfileNbit: Integer;
  function _GetMPEG4ProfileScalableTexture: Integer;
  function _GetMPEG4ProfileSimple: Integer;
  function _GetMPEG4ProfileSimpleFBA: Integer;
  function _GetMPEG4ProfileSimpleFace: Integer;
  function _GetMPEG4ProfileSimpleScalable: Integer;
  {Methods}
  function init: JMediaCodecInfo_CodecProfileLevel; cdecl;
  {Properties}
  property AACObjectELD: Integer read _GetAACObjectELD;
  property AACObjectERLC: Integer read _GetAACObjectERLC;
  property AACObjectHE: Integer read _GetAACObjectHE;
  property AACObjectHE_PS: Integer read _GetAACObjectHE_PS;
  property AACObjectLC: Integer read _GetAACObjectLC;
  property AACObjectLD: Integer read _GetAACObjectLD;
  property AACObjectLTP: Integer read _GetAACObjectLTP;
  property AACObjectMain: Integer read _GetAACObjectMain;
  property AACObjectSSR: Integer read _GetAACObjectSSR;
  property AACObjectScalable: Integer read _GetAACObjectScalable;
  property AVCLevel1: Integer read _GetAVCLevel1;
  property AVCLevel11: Integer read _GetAVCLevel11;
  property AVCLevel12: Integer read _GetAVCLevel12;
  property AVCLevel13: Integer read _GetAVCLevel13;
  property AVCLevel1b: Integer read _GetAVCLevel1b;
  property AVCLevel2: Integer read _GetAVCLevel2;
  property AVCLevel21: Integer read _GetAVCLevel21;
  property AVCLevel22: Integer read _GetAVCLevel22;
  property AVCLevel3: Integer read _GetAVCLevel3;
  property AVCLevel31: Integer read _GetAVCLevel31;
  property AVCLevel32: Integer read _GetAVCLevel32;
  property AVCLevel4: Integer read _GetAVCLevel4;
  property AVCLevel41: Integer read _GetAVCLevel41;
  property AVCLevel42: Integer read _GetAVCLevel42;
  property AVCLevel5: Integer read _GetAVCLevel5;
  property AVCLevel51: Integer read _GetAVCLevel51;
  property AVCProfileBaseline: Integer read _GetAVCProfileBaseline;
  property AVCProfileExtended: Integer read _GetAVCProfileExtended;
  property AVCProfileHigh: Integer read _GetAVCProfileHigh;
  property AVCProfileHigh10: Integer read _GetAVCProfileHigh10;
  property AVCProfileHigh422: Integer read _GetAVCProfileHigh422;
  property AVCProfileHigh444: Integer read _GetAVCProfileHigh444;
  property AVCProfileMain: Integer read _GetAVCProfileMain;
  property H263Level10: Integer read _GetH263Level10;
  property H263Level20: Integer read _GetH263Level20;
  property H263Level30: Integer read _GetH263Level30;
  property H263Level40: Integer read _GetH263Level40;
  property H263Level45: Integer read _GetH263Level45;
  property H263Level50: Integer read _GetH263Level50;
  property H263Level60: Integer read _GetH263Level60;
  property H263Level70: Integer read _GetH263Level70;
  property H263ProfileBackwardCompatible: Integer read _GetH263ProfileBackwardCompatible;
  property H263ProfileBaseline: Integer read _GetH263ProfileBaseline;
  property H263ProfileH320Coding: Integer read _GetH263ProfileH320Coding;
  property H263ProfileHighCompression: Integer read _GetH263ProfileHighCompression;
  property H263ProfileHighLatency: Integer read _GetH263ProfileHighLatency;
  property H263ProfileISWV2: Integer read _GetH263ProfileISWV2;
  property H263ProfileISWV3: Integer read _GetH263ProfileISWV3;
  property H263ProfileInterlace: Integer read _GetH263ProfileInterlace;
  property H263ProfileInternet: Integer read _GetH263ProfileInternet;
  property MPEG4Level0: Integer read _GetMPEG4Level0;
  property MPEG4Level0b: Integer read _GetMPEG4Level0b;
  property MPEG4Level1: Integer read _GetMPEG4Level1;
  property MPEG4Level2: Integer read _GetMPEG4Level2;
  property MPEG4Level3: Integer read _GetMPEG4Level3;
  property MPEG4Level4: Integer read _GetMPEG4Level4;
  property MPEG4Level4a: Integer read _GetMPEG4Level4a;
  property MPEG4Level5: Integer read _GetMPEG4Level5;
  property MPEG4ProfileAdvancedCoding: Integer read _GetMPEG4ProfileAdvancedCoding;
  property MPEG4ProfileAdvancedCore: Integer read _GetMPEG4ProfileAdvancedCore;
  property MPEG4ProfileAdvancedRealTime: Integer read _GetMPEG4ProfileAdvancedRealTime;
  property MPEG4ProfileAdvancedScalable: Integer read _GetMPEG4ProfileAdvancedScalable;
  property MPEG4ProfileAdvancedSimple: Integer read _GetMPEG4ProfileAdvancedSimple;
  property MPEG4ProfileBasicAnimated: Integer read _GetMPEG4ProfileBasicAnimated;
  property MPEG4ProfileCore: Integer read _GetMPEG4ProfileCore;
  property MPEG4ProfileCoreScalable: Integer read _GetMPEG4ProfileCoreScalable;
  property MPEG4ProfileHybrid: Integer read _GetMPEG4ProfileHybrid;
  property MPEG4ProfileMain: Integer read _GetMPEG4ProfileMain;
  property MPEG4ProfileNbit: Integer read _GetMPEG4ProfileNbit;
  property MPEG4ProfileScalableTexture: Integer read _GetMPEG4ProfileScalableTexture;
  property MPEG4ProfileSimple: Integer read _GetMPEG4ProfileSimple;
  property MPEG4ProfileSimpleFBA: Integer read _GetMPEG4ProfileSimpleFBA;
  property MPEG4ProfileSimpleFace: Integer read _GetMPEG4ProfileSimpleFace;
  property MPEG4ProfileSimpleScalable: Integer read _GetMPEG4ProfileSimpleScalable;
end;

[JavaSignature('android/media/MediaCodecInfo$CodecProfileLevel')]
JMediaCodecInfo_CodecProfileLevel = interface(JObject)
['{73647127-6989-4ABF-B521-186C8899A5AC}']
  {Property Methods}
  function _Getlevel: Integer;
  procedure _Setlevel(Value: Integer);
  function _Getprofile: Integer;
  procedure _Setprofile(Value: Integer);
  {Properties}
  property level: Integer read _Getlevel write _Setlevel;
  property profile: Integer read _Getprofile write _Setprofile;
end;
TJMediaCodecInfo_CodecProfileLevel = class(TJavaGenericImport<JMediaCodecInfo_CodecProfileLevelClass, JMediaCodecInfo_CodecProfileLevel>) end;

JEffectUpdateListenerClass = interface(IJavaClass)
['{B38D8A57-D3AB-4F58-AE3A-A9A7D8EE612B}']
end;

[JavaSignature('android/media/effect/EffectUpdateListener')]
JEffectUpdateListener = interface(IJavaInstance)
['{FF287CF5-5208-4CF5-A0EF-8F76D144AF71}']
  {Methods}
  procedure onEffectUpdated(effect: JEffect; info: JObject); cdecl;
end;
TJEffectUpdateListener = class(TJavaGenericImport<JEffectUpdateListenerClass, JEffectUpdateListener>) end;

JRingtoneClass = interface(JObjectClass)
['{C62E030E-5189-47DF-963A-BCA873D1865B}']
end;

[JavaSignature('android/media/Ringtone')]
JRingtone = interface(JObject)
['{D08C2112-5F37-4665-BA84-4DC9848539CC}']
  {Methods}
  function getStreamType: Integer; cdecl;
  function getTitle(context: JContext): JString; cdecl;
  function isPlaying: Boolean; cdecl;
  procedure play; cdecl;
  procedure setStreamType(streamType: Integer); cdecl;
  procedure stop; cdecl;
end;
TJRingtone = class(TJavaGenericImport<JRingtoneClass, JRingtone>) end;

JEnvironmentalReverbClass = interface(JAudioEffectClass)
['{6BDA0E80-3384-4B98-8145-F15D5728C3A6}']
  {Property Methods}
  function _GetPARAM_DECAY_HF_RATIO: Integer;
  function _GetPARAM_DECAY_TIME: Integer;
  function _GetPARAM_DENSITY: Integer;
  function _GetPARAM_DIFFUSION: Integer;
  function _GetPARAM_REFLECTIONS_DELAY: Integer;
  function _GetPARAM_REFLECTIONS_LEVEL: Integer;
  function _GetPARAM_REVERB_DELAY: Integer;
  function _GetPARAM_REVERB_LEVEL: Integer;
  function _GetPARAM_ROOM_HF_LEVEL: Integer;
  function _GetPARAM_ROOM_LEVEL: Integer;
  {Methods}
  function init(priority: Integer; audioSession: Integer): JEnvironmentalReverb; cdecl;
  {Properties}
  property PARAM_DECAY_HF_RATIO: Integer read _GetPARAM_DECAY_HF_RATIO;
  property PARAM_DECAY_TIME: Integer read _GetPARAM_DECAY_TIME;
  property PARAM_DENSITY: Integer read _GetPARAM_DENSITY;
  property PARAM_DIFFUSION: Integer read _GetPARAM_DIFFUSION;
  property PARAM_REFLECTIONS_DELAY: Integer read _GetPARAM_REFLECTIONS_DELAY;
  property PARAM_REFLECTIONS_LEVEL: Integer read _GetPARAM_REFLECTIONS_LEVEL;
  property PARAM_REVERB_DELAY: Integer read _GetPARAM_REVERB_DELAY;
  property PARAM_REVERB_LEVEL: Integer read _GetPARAM_REVERB_LEVEL;
  property PARAM_ROOM_HF_LEVEL: Integer read _GetPARAM_ROOM_HF_LEVEL;
  property PARAM_ROOM_LEVEL: Integer read _GetPARAM_ROOM_LEVEL;
end;

[JavaSignature('android/media/audiofx/EnvironmentalReverb')]
JEnvironmentalReverb = interface(JAudioEffect)
['{94A98733-66B8-4C17-B49C-5AD18808F3DB}']
  {Methods}
  function getDecayHFRatio: SmallInt; cdecl;
  function getDecayTime: Integer; cdecl;
  function getDensity: SmallInt; cdecl;
  function getDiffusion: SmallInt; cdecl;
  function getProperties: JEnvironmentalReverb_Settings; cdecl;
  function getReflectionsDelay: Integer; cdecl;
  function getReflectionsLevel: SmallInt; cdecl;
  function getReverbDelay: Integer; cdecl;
  function getReverbLevel: SmallInt; cdecl;
  function getRoomHFLevel: SmallInt; cdecl;
  function getRoomLevel: SmallInt; cdecl;
  procedure setDecayHFRatio(decayHFRatio: SmallInt); cdecl;
  procedure setDecayTime(decayTime: Integer); cdecl;
  procedure setDensity(density: SmallInt); cdecl;
  procedure setDiffusion(diffusion: SmallInt); cdecl;
  procedure setParameterListener(listener: JEnvironmentalReverb_OnParameterChangeListener); cdecl;
  procedure setProperties(settings: JEnvironmentalReverb_Settings); cdecl;
  procedure setReflectionsDelay(reflectionsDelay: Integer); cdecl;
  procedure setReflectionsLevel(reflectionsLevel: SmallInt); cdecl;
  procedure setReverbDelay(reverbDelay: Integer); cdecl;
  procedure setReverbLevel(reverbLevel: SmallInt); cdecl;
  procedure setRoomHFLevel(roomHF: SmallInt); cdecl;
  procedure setRoomLevel(room: SmallInt); cdecl;
end;
TJEnvironmentalReverb = class(TJavaGenericImport<JEnvironmentalReverbClass, JEnvironmentalReverb>) end;

JPresetReverb_SettingsClass = interface(JObjectClass)
['{DF5566DC-55CD-4C26-9AB8-520F698491CC}']
  {Methods}
  function init: JPresetReverb_Settings; cdecl; overload;
  function init(settings: JString): JPresetReverb_Settings; cdecl; overload;
end;

[JavaSignature('android/media/audiofx/PresetReverb$Settings')]
JPresetReverb_Settings = interface(JObject)
['{F57B0B89-4CDE-451E-BF10-43518B99BB08}']
  {Property Methods}
  function _Getpreset: SmallInt;
  procedure _Setpreset(Value: SmallInt);
  {Methods}
  function toString: JString; cdecl;
  {Properties}
  property preset: SmallInt read _Getpreset write _Setpreset;
end;
TJPresetReverb_Settings = class(TJavaGenericImport<JPresetReverb_SettingsClass, JPresetReverb_Settings>) end;

JVirtualizerClass = interface(JAudioEffectClass)
['{FD870DEA-E35B-48C4-B8D9-FFFEC73F5AEC}']
  {Property Methods}
  function _GetPARAM_STRENGTH: Integer;
  function _GetPARAM_STRENGTH_SUPPORTED: Integer;
  {Methods}
  function init(priority: Integer; audioSession: Integer): JVirtualizer; cdecl;
  {Properties}
  property PARAM_STRENGTH: Integer read _GetPARAM_STRENGTH;
  property PARAM_STRENGTH_SUPPORTED: Integer read _GetPARAM_STRENGTH_SUPPORTED;
end;

[JavaSignature('android/media/audiofx/Virtualizer')]
JVirtualizer = interface(JAudioEffect)
['{29728474-34B6-43F2-8D30-D42C4D3C1B9D}']
  {Methods}
  function getProperties: JVirtualizer_Settings; cdecl;
  function getRoundedStrength: SmallInt; cdecl;
  function getStrengthSupported: Boolean; cdecl;
  procedure setParameterListener(listener: JVirtualizer_OnParameterChangeListener); cdecl;
  procedure setProperties(settings: JVirtualizer_Settings); cdecl;
  procedure setStrength(strength: SmallInt); cdecl;
end;
TJVirtualizer = class(TJavaGenericImport<JVirtualizerClass, JVirtualizer>) end;

JMediaCodec_BufferInfoClass = interface(JObjectClass)
['{F2CE3B97-9D62-442D-A689-ED511D19C5F2}']
  {Methods}
  function init: JMediaCodec_BufferInfo; cdecl;
end;

[JavaSignature('android/media/MediaCodec$BufferInfo')]
JMediaCodec_BufferInfo = interface(JObject)
['{EB7BFC1B-B1F0-4602-9173-69B036DE28E1}']
  {Property Methods}
  function _Getflags: Integer;
  procedure _Setflags(Value: Integer);
  function _Getoffset: Integer;
  procedure _Setoffset(Value: Integer);
  function _GetpresentationTimeUs: Int64;
  procedure _SetpresentationTimeUs(Value: Int64);
  function _Getsize: Integer;
  procedure _Setsize(Value: Integer);
  {Methods}
  procedure &set(newOffset: Integer; newSize: Integer; newTimeUs: Int64; newFlags: Integer); cdecl;
  {Properties}
  property flags: Integer read _Getflags write _Setflags;
  property offset: Integer read _Getoffset write _Setoffset;
  property presentationTimeUs: Int64 read _GetpresentationTimeUs write _SetpresentationTimeUs;
  property size: Integer read _Getsize write _Setsize;
end;
TJMediaCodec_BufferInfo = class(TJavaGenericImport<JMediaCodec_BufferInfoClass, JMediaCodec_BufferInfo>) end;

JMediaRecorder_AudioEncoderClass = interface(JObjectClass)
['{60BE0488-31F9-4C18-8DF8-ED6D05525906}']
  {Property Methods}
  function _GetAAC: Integer;
  function _GetAAC_ELD: Integer;
  function _GetAMR_NB: Integer;
  function _GetAMR_WB: Integer;
  function _GetDEFAULT: Integer;
  function _GetHE_AAC: Integer;
  {Properties}
  property AAC: Integer read _GetAAC;
  property AAC_ELD: Integer read _GetAAC_ELD;
  property AMR_NB: Integer read _GetAMR_NB;
  property AMR_WB: Integer read _GetAMR_WB;
  property DEFAULT: Integer read _GetDEFAULT;
  property HE_AAC: Integer read _GetHE_AAC;
end;

[JavaSignature('android/media/MediaRecorder$AudioEncoder')]
JMediaRecorder_AudioEncoder = interface(JObject)
['{A827A239-F905-460F-850D-633F3C370A44}']
end;
TJMediaRecorder_AudioEncoder = class(TJavaGenericImport<JMediaRecorder_AudioEncoderClass, JMediaRecorder_AudioEncoder>) end;

JMediaFormatClass = interface(JObjectClass)
['{60105A0D-2764-4533-8EF5-DCE2759F23CC}']
  {Property Methods}
  function _GetKEY_AAC_PROFILE: JString;
  function _GetKEY_BIT_RATE: JString;
  function _GetKEY_CHANNEL_COUNT: JString;
  function _GetKEY_CHANNEL_MASK: JString;
  function _GetKEY_COLOR_FORMAT: JString;
  function _GetKEY_DURATION: JString;
  function _GetKEY_FLAC_COMPRESSION_LEVEL: JString;
  function _GetKEY_FRAME_RATE: JString;
  function _GetKEY_HEIGHT: JString;
  function _GetKEY_IS_ADTS: JString;
  function _GetKEY_I_FRAME_INTERVAL: JString;
  function _GetKEY_MAX_INPUT_SIZE: JString;
  function _GetKEY_MIME: JString;
  function _GetKEY_SAMPLE_RATE: JString;
  function _GetKEY_WIDTH: JString;
  {Methods}
  function init: JMediaFormat; cdecl;
  function createAudioFormat(mime: JString; sampleRate: Integer; channelCount: Integer): JMediaFormat; cdecl;
  function createVideoFormat(mime: JString; width: Integer; height: Integer): JMediaFormat; cdecl;
  {Properties}
  property KEY_AAC_PROFILE: JString read _GetKEY_AAC_PROFILE;
  property KEY_BIT_RATE: JString read _GetKEY_BIT_RATE;
  property KEY_CHANNEL_COUNT: JString read _GetKEY_CHANNEL_COUNT;
  property KEY_CHANNEL_MASK: JString read _GetKEY_CHANNEL_MASK;
  property KEY_COLOR_FORMAT: JString read _GetKEY_COLOR_FORMAT;
  property KEY_DURATION: JString read _GetKEY_DURATION;
  property KEY_FLAC_COMPRESSION_LEVEL: JString read _GetKEY_FLAC_COMPRESSION_LEVEL;
  property KEY_FRAME_RATE: JString read _GetKEY_FRAME_RATE;
  property KEY_HEIGHT: JString read _GetKEY_HEIGHT;
  property KEY_IS_ADTS: JString read _GetKEY_IS_ADTS;
  property KEY_I_FRAME_INTERVAL: JString read _GetKEY_I_FRAME_INTERVAL;
  property KEY_MAX_INPUT_SIZE: JString read _GetKEY_MAX_INPUT_SIZE;
  property KEY_MIME: JString read _GetKEY_MIME;
  property KEY_SAMPLE_RATE: JString read _GetKEY_SAMPLE_RATE;
  property KEY_WIDTH: JString read _GetKEY_WIDTH;
end;

[JavaSignature('android/media/MediaFormat')]
JMediaFormat = interface(JObject)
['{93B4C18E-4FE8-4AA6-9E32-469B5CC4E6DC}']
  {Methods}
  function containsKey(name: JString): Boolean; cdecl;
  function getFloat(name: JString): Single; cdecl;
  function getInteger(name: JString): Integer; cdecl;
  function getLong(name: JString): Int64; cdecl;
  function getString(name: JString): JString; cdecl;
  procedure setFloat(name: JString; value: Single); cdecl;
  procedure setInteger(name: JString; value: Integer); cdecl;
  procedure setLong(name: JString; value: Int64); cdecl;
  procedure setString(name: JString; value: JString); cdecl;
  function toString: JString; cdecl;
end;
TJMediaFormat = class(TJavaGenericImport<JMediaFormatClass, JMediaFormat>) end;

JExifInterfaceClass = interface(JObjectClass)
['{023B6C11-488A-4106-9562-9DE6A3B3305C}']
  {Property Methods}
  function _GetORIENTATION_FLIP_HORIZONTAL: Integer;
  function _GetORIENTATION_FLIP_VERTICAL: Integer;
  function _GetORIENTATION_NORMAL: Integer;
  function _GetORIENTATION_ROTATE_180: Integer;
  function _GetORIENTATION_ROTATE_270: Integer;
  function _GetORIENTATION_ROTATE_90: Integer;
  function _GetORIENTATION_TRANSPOSE: Integer;
  function _GetORIENTATION_TRANSVERSE: Integer;
  function _GetORIENTATION_UNDEFINED: Integer;
  function _GetTAG_APERTURE: JString;
  function _GetTAG_DATETIME: JString;
  function _GetTAG_EXPOSURE_TIME: JString;
  function _GetTAG_FLASH: JString;
  function _GetTAG_FOCAL_LENGTH: JString;
  function _GetTAG_GPS_ALTITUDE: JString;
  function _GetTAG_GPS_ALTITUDE_REF: JString;
  function _GetTAG_GPS_DATESTAMP: JString;
  function _GetTAG_GPS_LATITUDE: JString;
  function _GetTAG_GPS_LATITUDE_REF: JString;
  function _GetTAG_GPS_LONGITUDE: JString;
  function _GetTAG_GPS_LONGITUDE_REF: JString;
  function _GetTAG_GPS_PROCESSING_METHOD: JString;
  function _GetTAG_GPS_TIMESTAMP: JString;
  function _GetTAG_IMAGE_LENGTH: JString;
  function _GetTAG_IMAGE_WIDTH: JString;
  function _GetTAG_ISO: JString;
  function _GetTAG_MAKE: JString;
  function _GetTAG_MODEL: JString;
  function _GetTAG_ORIENTATION: JString;
  function _GetTAG_WHITE_BALANCE: JString;
  function _GetWHITEBALANCE_AUTO: Integer;
  function _GetWHITEBALANCE_MANUAL: Integer;
  {Methods}
  function init(filename: JString): JExifInterface; cdecl;
  {Properties}
  property ORIENTATION_FLIP_HORIZONTAL: Integer read _GetORIENTATION_FLIP_HORIZONTAL;
  property ORIENTATION_FLIP_VERTICAL: Integer read _GetORIENTATION_FLIP_VERTICAL;
  property ORIENTATION_NORMAL: Integer read _GetORIENTATION_NORMAL;
  property ORIENTATION_ROTATE_180: Integer read _GetORIENTATION_ROTATE_180;
  property ORIENTATION_ROTATE_270: Integer read _GetORIENTATION_ROTATE_270;
  property ORIENTATION_ROTATE_90: Integer read _GetORIENTATION_ROTATE_90;
  property ORIENTATION_TRANSPOSE: Integer read _GetORIENTATION_TRANSPOSE;
  property ORIENTATION_TRANSVERSE: Integer read _GetORIENTATION_TRANSVERSE;
  property ORIENTATION_UNDEFINED: Integer read _GetORIENTATION_UNDEFINED;
  property TAG_APERTURE: JString read _GetTAG_APERTURE;
  property TAG_DATETIME: JString read _GetTAG_DATETIME;
  property TAG_EXPOSURE_TIME: JString read _GetTAG_EXPOSURE_TIME;
  property TAG_FLASH: JString read _GetTAG_FLASH;
  property TAG_FOCAL_LENGTH: JString read _GetTAG_FOCAL_LENGTH;
  property TAG_GPS_ALTITUDE: JString read _GetTAG_GPS_ALTITUDE;
  property TAG_GPS_ALTITUDE_REF: JString read _GetTAG_GPS_ALTITUDE_REF;
  property TAG_GPS_DATESTAMP: JString read _GetTAG_GPS_DATESTAMP;
  property TAG_GPS_LATITUDE: JString read _GetTAG_GPS_LATITUDE;
  property TAG_GPS_LATITUDE_REF: JString read _GetTAG_GPS_LATITUDE_REF;
  property TAG_GPS_LONGITUDE: JString read _GetTAG_GPS_LONGITUDE;
  property TAG_GPS_LONGITUDE_REF: JString read _GetTAG_GPS_LONGITUDE_REF;
  property TAG_GPS_PROCESSING_METHOD: JString read _GetTAG_GPS_PROCESSING_METHOD;
  property TAG_GPS_TIMESTAMP: JString read _GetTAG_GPS_TIMESTAMP;
  property TAG_IMAGE_LENGTH: JString read _GetTAG_IMAGE_LENGTH;
  property TAG_IMAGE_WIDTH: JString read _GetTAG_IMAGE_WIDTH;
  property TAG_ISO: JString read _GetTAG_ISO;
  property TAG_MAKE: JString read _GetTAG_MAKE;
  property TAG_MODEL: JString read _GetTAG_MODEL;
  property TAG_ORIENTATION: JString read _GetTAG_ORIENTATION;
  property TAG_WHITE_BALANCE: JString read _GetTAG_WHITE_BALANCE;
  property WHITEBALANCE_AUTO: Integer read _GetWHITEBALANCE_AUTO;
  property WHITEBALANCE_MANUAL: Integer read _GetWHITEBALANCE_MANUAL;
end;

[JavaSignature('android/media/ExifInterface')]
JExifInterface = interface(JObject)
['{A1FEA8A4-6E88-46E7-BB89-BC24ACA69331}']
  {Methods}
  function getAltitude(defaultValue: Double): Double; cdecl;
  function getAttribute(tag: JString): JString; cdecl;
  function getAttributeDouble(tag: JString; defaultValue: Double): Double; cdecl;
  function getAttributeInt(tag: JString; defaultValue: Integer): Integer; cdecl;
  function getLatLong(output: TJavaArray<Single>): Boolean; cdecl;
  function getThumbnail: TJavaArray<Byte>; cdecl;
  function hasThumbnail: Boolean; cdecl;
  procedure saveAttributes; cdecl;
  procedure setAttribute(tag: JString; value: JString); cdecl;
end;
TJExifInterface = class(TJavaGenericImport<JExifInterfaceClass, JExifInterface>) end;

JEqualizer_OnParameterChangeListenerClass = interface(IJavaClass)
['{E9CD9590-C15D-4BF5-8EC8-0459C90DF626}']
end;

[JavaSignature('android/media/audiofx/Equalizer$OnParameterChangeListener')]
JEqualizer_OnParameterChangeListener = interface(IJavaInstance)
['{9C53A56E-58B3-477F-8B7A-40972ADDBC15}']
  {Methods}
  procedure onParameterChange(effect: JEqualizer; status: Integer; param1: Integer; param2: Integer; value: Integer); cdecl;
end;
TJEqualizer_OnParameterChangeListener = class(TJavaGenericImport<JEqualizer_OnParameterChangeListenerClass, JEqualizer_OnParameterChangeListener>) end;

JAudioEffect_DescriptorClass = interface(JObjectClass)
['{9A3069D0-C184-480C-B71E-20295F8F478D}']
  {Methods}
  function init: JAudioEffect_Descriptor; cdecl; overload;
  function init(type_: JString; uuid: JString; connectMode: JString; name: JString; implementor: JString): JAudioEffect_Descriptor; cdecl; overload;
end;

[JavaSignature('android/media/audiofx/AudioEffect$Descriptor')]
JAudioEffect_Descriptor = interface(JObject)
['{51401F65-35F1-4271-88E5-9991F790D845}']
  {Property Methods}
  function _GetconnectMode: JString;
  procedure _SetconnectMode(Value: JString);
  function _Getimplementor: JString;
  procedure _Setimplementor(Value: JString);
  function _Getname: JString;
  procedure _Setname(Value: JString);
  function _Gettype: JUUID;
  procedure _Settype(Value: JUUID);
  function _Getuuid: JUUID;
  procedure _Setuuid(Value: JUUID);
  {Properties}
  property connectMode: JString read _GetconnectMode write _SetconnectMode;
  property implementor: JString read _Getimplementor write _Setimplementor;
  property name: JString read _Getname write _Setname;
  property &type: JUUID read _Gettype write _Settype;
  property uuid: JUUID read _Getuuid write _Setuuid;
end;
TJAudioEffect_Descriptor = class(TJavaGenericImport<JAudioEffect_DescriptorClass, JAudioEffect_Descriptor>) end;

JMediaCodecListClass = interface(JObjectClass)
['{F1400E68-3BDF-4B8B-9B45-F679CD2B2BF3}']
  {Methods}
  function getCodecCount: Integer; cdecl;
  function getCodecInfoAt(index: Integer): JMediaCodecInfo; cdecl;
end;

[JavaSignature('android/media/MediaCodecList')]
JMediaCodecList = interface(JObject)
['{26F449D6-A979-4BE9-BCE4-C67841504DA3}']
end;
TJMediaCodecList = class(TJavaGenericImport<JMediaCodecListClass, JMediaCodecList>) end;

JMediaPlayer_TrackInfoClass = interface(JObjectClass)
['{D7B4E7F5-E306-471E-8F77-ADBDB9B94282}']
  {Property Methods}
  function _GetMEDIA_TRACK_TYPE_AUDIO: Integer;
  function _GetMEDIA_TRACK_TYPE_TIMEDTEXT: Integer;
  function _GetMEDIA_TRACK_TYPE_UNKNOWN: Integer;
  function _GetMEDIA_TRACK_TYPE_VIDEO: Integer;
  {Properties}
  property MEDIA_TRACK_TYPE_AUDIO: Integer read _GetMEDIA_TRACK_TYPE_AUDIO;
  property MEDIA_TRACK_TYPE_TIMEDTEXT: Integer read _GetMEDIA_TRACK_TYPE_TIMEDTEXT;
  property MEDIA_TRACK_TYPE_UNKNOWN: Integer read _GetMEDIA_TRACK_TYPE_UNKNOWN;
  property MEDIA_TRACK_TYPE_VIDEO: Integer read _GetMEDIA_TRACK_TYPE_VIDEO;
end;

[JavaSignature('android/media/MediaPlayer$TrackInfo')]
JMediaPlayer_TrackInfo = interface(JObject)
['{488EB87A-19D3-4231-BEE0-01414518C163}']
  {Methods}
  function describeContents: Integer; cdecl;
  function getLanguage: JString; cdecl;
  function getTrackType: Integer; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
end;
TJMediaPlayer_TrackInfo = class(TJavaGenericImport<JMediaPlayer_TrackInfoClass, JMediaPlayer_TrackInfo>) end;

JMediaCodecClass = interface(JObjectClass)
['{CE3DED9B-E9AD-4A29-9755-ECFFFC1831BA}']
  {Property Methods}
  function _GetBUFFER_FLAG_CODEC_CONFIG: Integer;
  function _GetBUFFER_FLAG_END_OF_STREAM: Integer;
  function _GetBUFFER_FLAG_SYNC_FRAME: Integer;
  function _GetCONFIGURE_FLAG_ENCODE: Integer;
  function _GetCRYPTO_MODE_AES_CTR: Integer;
  function _GetCRYPTO_MODE_UNENCRYPTED: Integer;
  function _GetINFO_OUTPUT_BUFFERS_CHANGED: Integer;
  function _GetINFO_OUTPUT_FORMAT_CHANGED: Integer;
  function _GetINFO_TRY_AGAIN_LATER: Integer;
  function _GetVIDEO_SCALING_MODE_SCALE_TO_FIT: Integer;
  function _GetVIDEO_SCALING_MODE_SCALE_TO_FIT_WITH_CROPPING: Integer;
  {Methods}
  function createByCodecName(name: JString): JMediaCodec; cdecl;
  function createDecoderByType(type_: JString): JMediaCodec; cdecl;
  function createEncoderByType(type_: JString): JMediaCodec; cdecl;
  {Properties}
  property BUFFER_FLAG_CODEC_CONFIG: Integer read _GetBUFFER_FLAG_CODEC_CONFIG;
  property BUFFER_FLAG_END_OF_STREAM: Integer read _GetBUFFER_FLAG_END_OF_STREAM;
  property BUFFER_FLAG_SYNC_FRAME: Integer read _GetBUFFER_FLAG_SYNC_FRAME;
  property CONFIGURE_FLAG_ENCODE: Integer read _GetCONFIGURE_FLAG_ENCODE;
  property CRYPTO_MODE_AES_CTR: Integer read _GetCRYPTO_MODE_AES_CTR;
  property CRYPTO_MODE_UNENCRYPTED: Integer read _GetCRYPTO_MODE_UNENCRYPTED;
  property INFO_OUTPUT_BUFFERS_CHANGED: Integer read _GetINFO_OUTPUT_BUFFERS_CHANGED;
  property INFO_OUTPUT_FORMAT_CHANGED: Integer read _GetINFO_OUTPUT_FORMAT_CHANGED;
  property INFO_TRY_AGAIN_LATER: Integer read _GetINFO_TRY_AGAIN_LATER;
  property VIDEO_SCALING_MODE_SCALE_TO_FIT: Integer read _GetVIDEO_SCALING_MODE_SCALE_TO_FIT;
  property VIDEO_SCALING_MODE_SCALE_TO_FIT_WITH_CROPPING: Integer read _GetVIDEO_SCALING_MODE_SCALE_TO_FIT_WITH_CROPPING;
end;

[JavaSignature('android/media/MediaCodec')]
JMediaCodec = interface(JObject)
['{A535089E-5F3D-464D-9BB5-2A3553A461F1}']
  {Methods}
  procedure configure(format: JMediaFormat; surface: JSurface; crypto: JMediaCrypto; flags: Integer); cdecl;
  function dequeueInputBuffer(timeoutUs: Int64): Integer; cdecl;
  function dequeueOutputBuffer(info: JMediaCodec_BufferInfo; timeoutUs: Int64): Integer; cdecl;
  procedure flush; cdecl;
  function getOutputFormat: JMediaFormat; cdecl;
  procedure queueInputBuffer(index: Integer; offset: Integer; size: Integer; presentationTimeUs: Int64; flags: Integer); cdecl;
  procedure queueSecureInputBuffer(index: Integer; offset: Integer; info: JMediaCodec_CryptoInfo; presentationTimeUs: Int64; flags: Integer); cdecl;
  procedure release; cdecl;
  procedure releaseOutputBuffer(index: Integer; render: Boolean); cdecl;
  procedure setVideoScalingMode(mode: Integer); cdecl;
  procedure start; cdecl;
  procedure stop; cdecl;
end;
TJMediaCodec = class(TJavaGenericImport<JMediaCodecClass, JMediaCodec>) end;

JToneGeneratorClass = interface(JObjectClass)
['{2E43AD5A-F8CC-439A-B70E-78EAC7415740}']
  {Property Methods}
  function _GetMAX_VOLUME: Integer;
  function _GetMIN_VOLUME: Integer;
  function _GetTONE_CDMA_ABBR_ALERT: Integer;
  function _GetTONE_CDMA_ABBR_INTERCEPT: Integer;
  function _GetTONE_CDMA_ABBR_REORDER: Integer;
  function _GetTONE_CDMA_ALERT_AUTOREDIAL_LITE: Integer;
  function _GetTONE_CDMA_ALERT_CALL_GUARD: Integer;
  function _GetTONE_CDMA_ALERT_INCALL_LITE: Integer;
  function _GetTONE_CDMA_ALERT_NETWORK_LITE: Integer;
  function _GetTONE_CDMA_ANSWER: Integer;
  function _GetTONE_CDMA_CALLDROP_LITE: Integer;
  function _GetTONE_CDMA_CALL_SIGNAL_ISDN_INTERGROUP: Integer;
  function _GetTONE_CDMA_CALL_SIGNAL_ISDN_NORMAL: Integer;
  function _GetTONE_CDMA_CALL_SIGNAL_ISDN_PAT3: Integer;
  function _GetTONE_CDMA_CALL_SIGNAL_ISDN_PAT5: Integer;
  function _GetTONE_CDMA_CALL_SIGNAL_ISDN_PAT6: Integer;
  function _GetTONE_CDMA_CALL_SIGNAL_ISDN_PAT7: Integer;
  function _GetTONE_CDMA_CALL_SIGNAL_ISDN_PING_RING: Integer;
  function _GetTONE_CDMA_CALL_SIGNAL_ISDN_SP_PRI: Integer;
  function _GetTONE_CDMA_CONFIRM: Integer;
  function _GetTONE_CDMA_DIAL_TONE_LITE: Integer;
  function _GetTONE_CDMA_EMERGENCY_RINGBACK: Integer;
  function _GetTONE_CDMA_HIGH_L: Integer;
  function _GetTONE_CDMA_HIGH_PBX_L: Integer;
  function _GetTONE_CDMA_HIGH_PBX_SLS: Integer;
  function _GetTONE_CDMA_HIGH_PBX_SS: Integer;
  function _GetTONE_CDMA_HIGH_PBX_SSL: Integer;
  function _GetTONE_CDMA_HIGH_PBX_S_X4: Integer;
  function _GetTONE_CDMA_HIGH_SLS: Integer;
  function _GetTONE_CDMA_HIGH_SS: Integer;
  function _GetTONE_CDMA_HIGH_SSL: Integer;
  function _GetTONE_CDMA_HIGH_SS_2: Integer;
  function _GetTONE_CDMA_HIGH_S_X4: Integer;
  function _GetTONE_CDMA_INTERCEPT: Integer;
  function _GetTONE_CDMA_KEYPAD_VOLUME_KEY_LITE: Integer;
  function _GetTONE_CDMA_LOW_L: Integer;
  function _GetTONE_CDMA_LOW_PBX_L: Integer;
  function _GetTONE_CDMA_LOW_PBX_SLS: Integer;
  function _GetTONE_CDMA_LOW_PBX_SS: Integer;
  function _GetTONE_CDMA_LOW_PBX_SSL: Integer;
  function _GetTONE_CDMA_LOW_PBX_S_X4: Integer;
  function _GetTONE_CDMA_LOW_SLS: Integer;
  function _GetTONE_CDMA_LOW_SS: Integer;
  function _GetTONE_CDMA_LOW_SSL: Integer;
  function _GetTONE_CDMA_LOW_SS_2: Integer;
  function _GetTONE_CDMA_LOW_S_X4: Integer;
  function _GetTONE_CDMA_MED_L: Integer;
  function _GetTONE_CDMA_MED_PBX_L: Integer;
  function _GetTONE_CDMA_MED_PBX_SLS: Integer;
  function _GetTONE_CDMA_MED_PBX_SS: Integer;
  function _GetTONE_CDMA_MED_PBX_SSL: Integer;
  function _GetTONE_CDMA_MED_PBX_S_X4: Integer;
  function _GetTONE_CDMA_MED_SLS: Integer;
  function _GetTONE_CDMA_MED_SS: Integer;
  function _GetTONE_CDMA_MED_SSL: Integer;
  function _GetTONE_CDMA_MED_SS_2: Integer;
  function _GetTONE_CDMA_MED_S_X4: Integer;
  function _GetTONE_CDMA_NETWORK_BUSY: Integer;
  function _GetTONE_CDMA_NETWORK_BUSY_ONE_SHOT: Integer;
  function _GetTONE_CDMA_NETWORK_CALLWAITING: Integer;
  function _GetTONE_CDMA_NETWORK_USA_RINGBACK: Integer;
  function _GetTONE_CDMA_ONE_MIN_BEEP: Integer;
  function _GetTONE_CDMA_PIP: Integer;
  function _GetTONE_CDMA_PRESSHOLDKEY_LITE: Integer;
  function _GetTONE_CDMA_REORDER: Integer;
  function _GetTONE_CDMA_SIGNAL_OFF: Integer;
  function _GetTONE_CDMA_SOFT_ERROR_LITE: Integer;
  function _GetTONE_DTMF_0: Integer;
  function _GetTONE_DTMF_1: Integer;
  function _GetTONE_DTMF_2: Integer;
  function _GetTONE_DTMF_3: Integer;
  function _GetTONE_DTMF_4: Integer;
  function _GetTONE_DTMF_5: Integer;
  function _GetTONE_DTMF_6: Integer;
  function _GetTONE_DTMF_7: Integer;
  function _GetTONE_DTMF_8: Integer;
  function _GetTONE_DTMF_9: Integer;
  function _GetTONE_DTMF_A: Integer;
  function _GetTONE_DTMF_B: Integer;
  function _GetTONE_DTMF_C: Integer;
  function _GetTONE_DTMF_D: Integer;
  function _GetTONE_DTMF_P: Integer;
  function _GetTONE_DTMF_S: Integer;
  function _GetTONE_PROP_ACK: Integer;
  function _GetTONE_PROP_BEEP: Integer;
  function _GetTONE_PROP_BEEP2: Integer;
  function _GetTONE_PROP_NACK: Integer;
  function _GetTONE_PROP_PROMPT: Integer;
  function _GetTONE_SUP_BUSY: Integer;
  function _GetTONE_SUP_CALL_WAITING: Integer;
  function _GetTONE_SUP_CONFIRM: Integer;
  function _GetTONE_SUP_CONGESTION: Integer;
  function _GetTONE_SUP_CONGESTION_ABBREV: Integer;
  function _GetTONE_SUP_DIAL: Integer;
  function _GetTONE_SUP_ERROR: Integer;
  function _GetTONE_SUP_INTERCEPT: Integer;
  function _GetTONE_SUP_INTERCEPT_ABBREV: Integer;
  function _GetTONE_SUP_PIP: Integer;
  function _GetTONE_SUP_RADIO_ACK: Integer;
  function _GetTONE_SUP_RADIO_NOTAVAIL: Integer;
  function _GetTONE_SUP_RINGTONE: Integer;
  {Methods}
  function init(streamType: Integer; volume: Integer): JToneGenerator; cdecl;
  {Properties}
  property MAX_VOLUME: Integer read _GetMAX_VOLUME;
  property MIN_VOLUME: Integer read _GetMIN_VOLUME;
  property TONE_CDMA_ABBR_ALERT: Integer read _GetTONE_CDMA_ABBR_ALERT;
  property TONE_CDMA_ABBR_INTERCEPT: Integer read _GetTONE_CDMA_ABBR_INTERCEPT;
  property TONE_CDMA_ABBR_REORDER: Integer read _GetTONE_CDMA_ABBR_REORDER;
  property TONE_CDMA_ALERT_AUTOREDIAL_LITE: Integer read _GetTONE_CDMA_ALERT_AUTOREDIAL_LITE;
  property TONE_CDMA_ALERT_CALL_GUARD: Integer read _GetTONE_CDMA_ALERT_CALL_GUARD;
  property TONE_CDMA_ALERT_INCALL_LITE: Integer read _GetTONE_CDMA_ALERT_INCALL_LITE;
  property TONE_CDMA_ALERT_NETWORK_LITE: Integer read _GetTONE_CDMA_ALERT_NETWORK_LITE;
  property TONE_CDMA_ANSWER: Integer read _GetTONE_CDMA_ANSWER;
  property TONE_CDMA_CALLDROP_LITE: Integer read _GetTONE_CDMA_CALLDROP_LITE;
  property TONE_CDMA_CALL_SIGNAL_ISDN_INTERGROUP: Integer read _GetTONE_CDMA_CALL_SIGNAL_ISDN_INTERGROUP;
  property TONE_CDMA_CALL_SIGNAL_ISDN_NORMAL: Integer read _GetTONE_CDMA_CALL_SIGNAL_ISDN_NORMAL;
  property TONE_CDMA_CALL_SIGNAL_ISDN_PAT3: Integer read _GetTONE_CDMA_CALL_SIGNAL_ISDN_PAT3;
  property TONE_CDMA_CALL_SIGNAL_ISDN_PAT5: Integer read _GetTONE_CDMA_CALL_SIGNAL_ISDN_PAT5;
  property TONE_CDMA_CALL_SIGNAL_ISDN_PAT6: Integer read _GetTONE_CDMA_CALL_SIGNAL_ISDN_PAT6;
  property TONE_CDMA_CALL_SIGNAL_ISDN_PAT7: Integer read _GetTONE_CDMA_CALL_SIGNAL_ISDN_PAT7;
  property TONE_CDMA_CALL_SIGNAL_ISDN_PING_RING: Integer read _GetTONE_CDMA_CALL_SIGNAL_ISDN_PING_RING;
  property TONE_CDMA_CALL_SIGNAL_ISDN_SP_PRI: Integer read _GetTONE_CDMA_CALL_SIGNAL_ISDN_SP_PRI;
  property TONE_CDMA_CONFIRM: Integer read _GetTONE_CDMA_CONFIRM;
  property TONE_CDMA_DIAL_TONE_LITE: Integer read _GetTONE_CDMA_DIAL_TONE_LITE;
  property TONE_CDMA_EMERGENCY_RINGBACK: Integer read _GetTONE_CDMA_EMERGENCY_RINGBACK;
  property TONE_CDMA_HIGH_L: Integer read _GetTONE_CDMA_HIGH_L;
  property TONE_CDMA_HIGH_PBX_L: Integer read _GetTONE_CDMA_HIGH_PBX_L;
  property TONE_CDMA_HIGH_PBX_SLS: Integer read _GetTONE_CDMA_HIGH_PBX_SLS;
  property TONE_CDMA_HIGH_PBX_SS: Integer read _GetTONE_CDMA_HIGH_PBX_SS;
  property TONE_CDMA_HIGH_PBX_SSL: Integer read _GetTONE_CDMA_HIGH_PBX_SSL;
  property TONE_CDMA_HIGH_PBX_S_X4: Integer read _GetTONE_CDMA_HIGH_PBX_S_X4;
  property TONE_CDMA_HIGH_SLS: Integer read _GetTONE_CDMA_HIGH_SLS;
  property TONE_CDMA_HIGH_SS: Integer read _GetTONE_CDMA_HIGH_SS;
  property TONE_CDMA_HIGH_SSL: Integer read _GetTONE_CDMA_HIGH_SSL;
  property TONE_CDMA_HIGH_SS_2: Integer read _GetTONE_CDMA_HIGH_SS_2;
  property TONE_CDMA_HIGH_S_X4: Integer read _GetTONE_CDMA_HIGH_S_X4;
  property TONE_CDMA_INTERCEPT: Integer read _GetTONE_CDMA_INTERCEPT;
  property TONE_CDMA_KEYPAD_VOLUME_KEY_LITE: Integer read _GetTONE_CDMA_KEYPAD_VOLUME_KEY_LITE;
  property TONE_CDMA_LOW_L: Integer read _GetTONE_CDMA_LOW_L;
  property TONE_CDMA_LOW_PBX_L: Integer read _GetTONE_CDMA_LOW_PBX_L;
  property TONE_CDMA_LOW_PBX_SLS: Integer read _GetTONE_CDMA_LOW_PBX_SLS;
  property TONE_CDMA_LOW_PBX_SS: Integer read _GetTONE_CDMA_LOW_PBX_SS;
  property TONE_CDMA_LOW_PBX_SSL: Integer read _GetTONE_CDMA_LOW_PBX_SSL;
  property TONE_CDMA_LOW_PBX_S_X4: Integer read _GetTONE_CDMA_LOW_PBX_S_X4;
  property TONE_CDMA_LOW_SLS: Integer read _GetTONE_CDMA_LOW_SLS;
  property TONE_CDMA_LOW_SS: Integer read _GetTONE_CDMA_LOW_SS;
  property TONE_CDMA_LOW_SSL: Integer read _GetTONE_CDMA_LOW_SSL;
  property TONE_CDMA_LOW_SS_2: Integer read _GetTONE_CDMA_LOW_SS_2;
  property TONE_CDMA_LOW_S_X4: Integer read _GetTONE_CDMA_LOW_S_X4;
  property TONE_CDMA_MED_L: Integer read _GetTONE_CDMA_MED_L;
  property TONE_CDMA_MED_PBX_L: Integer read _GetTONE_CDMA_MED_PBX_L;
  property TONE_CDMA_MED_PBX_SLS: Integer read _GetTONE_CDMA_MED_PBX_SLS;
  property TONE_CDMA_MED_PBX_SS: Integer read _GetTONE_CDMA_MED_PBX_SS;
  property TONE_CDMA_MED_PBX_SSL: Integer read _GetTONE_CDMA_MED_PBX_SSL;
  property TONE_CDMA_MED_PBX_S_X4: Integer read _GetTONE_CDMA_MED_PBX_S_X4;
  property TONE_CDMA_MED_SLS: Integer read _GetTONE_CDMA_MED_SLS;
  property TONE_CDMA_MED_SS: Integer read _GetTONE_CDMA_MED_SS;
  property TONE_CDMA_MED_SSL: Integer read _GetTONE_CDMA_MED_SSL;
  property TONE_CDMA_MED_SS_2: Integer read _GetTONE_CDMA_MED_SS_2;
  property TONE_CDMA_MED_S_X4: Integer read _GetTONE_CDMA_MED_S_X4;
  property TONE_CDMA_NETWORK_BUSY: Integer read _GetTONE_CDMA_NETWORK_BUSY;
  property TONE_CDMA_NETWORK_BUSY_ONE_SHOT: Integer read _GetTONE_CDMA_NETWORK_BUSY_ONE_SHOT;
  property TONE_CDMA_NETWORK_CALLWAITING: Integer read _GetTONE_CDMA_NETWORK_CALLWAITING;
  property TONE_CDMA_NETWORK_USA_RINGBACK: Integer read _GetTONE_CDMA_NETWORK_USA_RINGBACK;
  property TONE_CDMA_ONE_MIN_BEEP: Integer read _GetTONE_CDMA_ONE_MIN_BEEP;
  property TONE_CDMA_PIP: Integer read _GetTONE_CDMA_PIP;
  property TONE_CDMA_PRESSHOLDKEY_LITE: Integer read _GetTONE_CDMA_PRESSHOLDKEY_LITE;
  property TONE_CDMA_REORDER: Integer read _GetTONE_CDMA_REORDER;
  property TONE_CDMA_SIGNAL_OFF: Integer read _GetTONE_CDMA_SIGNAL_OFF;
  property TONE_CDMA_SOFT_ERROR_LITE: Integer read _GetTONE_CDMA_SOFT_ERROR_LITE;
  property TONE_DTMF_0: Integer read _GetTONE_DTMF_0;
  property TONE_DTMF_1: Integer read _GetTONE_DTMF_1;
  property TONE_DTMF_2: Integer read _GetTONE_DTMF_2;
  property TONE_DTMF_3: Integer read _GetTONE_DTMF_3;
  property TONE_DTMF_4: Integer read _GetTONE_DTMF_4;
  property TONE_DTMF_5: Integer read _GetTONE_DTMF_5;
  property TONE_DTMF_6: Integer read _GetTONE_DTMF_6;
  property TONE_DTMF_7: Integer read _GetTONE_DTMF_7;
  property TONE_DTMF_8: Integer read _GetTONE_DTMF_8;
  property TONE_DTMF_9: Integer read _GetTONE_DTMF_9;
  property TONE_DTMF_A: Integer read _GetTONE_DTMF_A;
  property TONE_DTMF_B: Integer read _GetTONE_DTMF_B;
  property TONE_DTMF_C: Integer read _GetTONE_DTMF_C;
  property TONE_DTMF_D: Integer read _GetTONE_DTMF_D;
  property TONE_DTMF_P: Integer read _GetTONE_DTMF_P;
  property TONE_DTMF_S: Integer read _GetTONE_DTMF_S;
  property TONE_PROP_ACK: Integer read _GetTONE_PROP_ACK;
  property TONE_PROP_BEEP: Integer read _GetTONE_PROP_BEEP;
  property TONE_PROP_BEEP2: Integer read _GetTONE_PROP_BEEP2;
  property TONE_PROP_NACK: Integer read _GetTONE_PROP_NACK;
  property TONE_PROP_PROMPT: Integer read _GetTONE_PROP_PROMPT;
  property TONE_SUP_BUSY: Integer read _GetTONE_SUP_BUSY;
  property TONE_SUP_CALL_WAITING: Integer read _GetTONE_SUP_CALL_WAITING;
  property TONE_SUP_CONFIRM: Integer read _GetTONE_SUP_CONFIRM;
  property TONE_SUP_CONGESTION: Integer read _GetTONE_SUP_CONGESTION;
  property TONE_SUP_CONGESTION_ABBREV: Integer read _GetTONE_SUP_CONGESTION_ABBREV;
  property TONE_SUP_DIAL: Integer read _GetTONE_SUP_DIAL;
  property TONE_SUP_ERROR: Integer read _GetTONE_SUP_ERROR;
  property TONE_SUP_INTERCEPT: Integer read _GetTONE_SUP_INTERCEPT;
  property TONE_SUP_INTERCEPT_ABBREV: Integer read _GetTONE_SUP_INTERCEPT_ABBREV;
  property TONE_SUP_PIP: Integer read _GetTONE_SUP_PIP;
  property TONE_SUP_RADIO_ACK: Integer read _GetTONE_SUP_RADIO_ACK;
  property TONE_SUP_RADIO_NOTAVAIL: Integer read _GetTONE_SUP_RADIO_NOTAVAIL;
  property TONE_SUP_RINGTONE: Integer read _GetTONE_SUP_RINGTONE;
end;

[JavaSignature('android/media/ToneGenerator')]
JToneGenerator = interface(JObject)
['{F31E2BEF-7899-4B50-8ED2-13547C7AA405}']
  {Methods}
  function getAudioSessionId: Integer; cdecl;
  procedure release; cdecl;
  function startTone(toneType: Integer): Boolean; cdecl; overload;
  function startTone(toneType: Integer; durationMs: Integer): Boolean; cdecl; overload;
  procedure stopTone; cdecl;
end;
TJToneGenerator = class(TJavaGenericImport<JToneGeneratorClass, JToneGenerator>) end;

JEnvironmentalReverb_OnParameterChangeListenerClass = interface(IJavaClass)
['{51B7A404-EFBC-402C-AEA7-59B0E9ADABEE}']
end;

[JavaSignature('android/media/audiofx/EnvironmentalReverb$OnParameterChangeListener')]
JEnvironmentalReverb_OnParameterChangeListener = interface(IJavaInstance)
['{2D4E8354-BD92-4AC9-8907-0B63369AA1CF}']
  {Methods}
  procedure onParameterChange(effect: JEnvironmentalReverb; status: Integer; param: Integer; value: Integer); cdecl;
end;
TJEnvironmentalReverb_OnParameterChangeListener = class(TJavaGenericImport<JEnvironmentalReverb_OnParameterChangeListenerClass, JEnvironmentalReverb_OnParameterChangeListener>) end;

JMediaPlayer_OnSeekCompleteListenerClass = interface(IJavaClass)
['{ED9A90D0-F45E-4F82-A31F-A8C9735E2CBE}']
end;

[JavaSignature('android/media/MediaPlayer$OnSeekCompleteListener')]
JMediaPlayer_OnSeekCompleteListener = interface(IJavaInstance)
['{2F3ABA30-AB96-4030-9621-3C3A6FEC13AD}']
  {Methods}
  procedure onSeekComplete(mp: JMediaPlayer); cdecl;
end;
TJMediaPlayer_OnSeekCompleteListener = class(TJavaGenericImport<JMediaPlayer_OnSeekCompleteListenerClass, JMediaPlayer_OnSeekCompleteListener>) end;

JMediaRecorder_VideoSourceClass = interface(JObjectClass)
['{007242EA-B501-4179-BE6F-3D7641C9A412}']
  {Property Methods}
  function _GetCAMERA: Integer;
  function _GetDEFAULT: Integer;
  {Properties}
  property CAMERA: Integer read _GetCAMERA;
  property DEFAULT: Integer read _GetDEFAULT;
end;

[JavaSignature('android/media/MediaRecorder$VideoSource')]
JMediaRecorder_VideoSource = interface(JObject)
['{E14FEC4B-01A8-4E23-B9F2-63464382FE92}']
end;
TJMediaRecorder_VideoSource = class(TJavaGenericImport<JMediaRecorder_VideoSourceClass, JMediaRecorder_VideoSource>) end;

JBassBoost_SettingsClass = interface(JObjectClass)
['{16961C98-3B24-451A-A990-83E334394621}']
  {Methods}
  function init: JBassBoost_Settings; cdecl; overload;
  function init(settings: JString): JBassBoost_Settings; cdecl; overload;
end;

[JavaSignature('android/media/audiofx/BassBoost$Settings')]
JBassBoost_Settings = interface(JObject)
['{126672AC-6E3E-4118-85C0-52A19083384F}']
  {Property Methods}
  function _Getstrength: SmallInt;
  procedure _Setstrength(Value: SmallInt);
  {Methods}
  function toString: JString; cdecl;
  {Properties}
  property strength: SmallInt read _Getstrength write _Setstrength;
end;
TJBassBoost_Settings = class(TJavaGenericImport<JBassBoost_SettingsClass, JBassBoost_Settings>) end;

JMediaCryptoClass = interface(JObjectClass)
['{31B7D145-1DBA-4D5C-860B-338AA3F6780B}']
  {Methods}
  function init(uuid: JUUID; initData: TJavaArray<Byte>): JMediaCrypto; cdecl;
  function isCryptoSchemeSupported(uuid: JUUID): Boolean; cdecl;
end;

[JavaSignature('android/media/MediaCrypto')]
JMediaCrypto = interface(JObject)
['{B1E505C9-9F43-4C70-AB68-C0DEC0999882}']
  {Methods}
  procedure release; cdecl;
  function requiresSecureDecoderComponent(mime: JString): Boolean; cdecl;
end;
TJMediaCrypto = class(TJavaGenericImport<JMediaCryptoClass, JMediaCrypto>) end;

JMediaCodec_CryptoInfoClass = interface(JObjectClass)
['{DDA3A4C6-56FD-4C37-8812-5269A274E6DD}']
  {Methods}
  function init: JMediaCodec_CryptoInfo; cdecl;
end;

[JavaSignature('android/media/MediaCodec$CryptoInfo')]
JMediaCodec_CryptoInfo = interface(JObject)
['{5281B1AC-4C52-4BF3-A45F-75B3778E3774}']
  {Property Methods}
  function _Getiv: TJavaArray<Byte>;
  procedure _Setiv(Value: TJavaArray<Byte>);
  function _Getkey: TJavaArray<Byte>;
  procedure _Setkey(Value: TJavaArray<Byte>);
  function _Getmode: Integer;
  procedure _Setmode(Value: Integer);
  function _GetnumBytesOfClearData: TJavaArray<Integer>;
  procedure _SetnumBytesOfClearData(Value: TJavaArray<Integer>);
  function _GetnumBytesOfEncryptedData: TJavaArray<Integer>;
  procedure _SetnumBytesOfEncryptedData(Value: TJavaArray<Integer>);
  function _GetnumSubSamples: Integer;
  procedure _SetnumSubSamples(Value: Integer);
  {Methods}
  procedure &set(newNumSubSamples: Integer; newNumBytesOfClearData: TJavaArray<Integer>; newNumBytesOfEncryptedData: TJavaArray<Integer>; newKey: TJavaArray<Byte>; newIV: TJavaArray<Byte>; newMode: Integer); cdecl;
  {Properties}
  property iv: TJavaArray<Byte> read _Getiv write _Setiv;
  property key: TJavaArray<Byte> read _Getkey write _Setkey;
  property mode: Integer read _Getmode write _Setmode;
  property numBytesOfClearData: TJavaArray<Integer> read _GetnumBytesOfClearData write _SetnumBytesOfClearData;
  property numBytesOfEncryptedData: TJavaArray<Integer> read _GetnumBytesOfEncryptedData write _SetnumBytesOfEncryptedData;
  property numSubSamples: Integer read _GetnumSubSamples write _SetnumSubSamples;
end;
TJMediaCodec_CryptoInfo = class(TJavaGenericImport<JMediaCodec_CryptoInfoClass, JMediaCodec_CryptoInfo>) end;

JAudioEffect_OnEnableStatusChangeListenerClass = interface(IJavaClass)
['{98D458F7-D2B6-412A-BE43-6825ACA42EA8}']
end;

[JavaSignature('android/media/audiofx/AudioEffect$OnEnableStatusChangeListener')]
JAudioEffect_OnEnableStatusChangeListener = interface(IJavaInstance)
['{F2A3F549-058B-4354-A8B2-508B65EC88AF}']
  {Methods}
  procedure onEnableStatusChange(effect: JAudioEffect; enabled: Boolean); cdecl;
end;
TJAudioEffect_OnEnableStatusChangeListener = class(TJavaGenericImport<JAudioEffect_OnEnableStatusChangeListenerClass, JAudioEffect_OnEnableStatusChangeListener>) end;

JMediaScannerConnection_OnScanCompletedListenerClass = interface(IJavaClass)
['{743ED55A-4735-41E1-AE1A-E0D4023F8AA5}']
end;

[JavaSignature('android/media/MediaScannerConnection$OnScanCompletedListener')]
JMediaScannerConnection_OnScanCompletedListener = interface(IJavaInstance)
['{67909CB7-D7EA-4870-9461-7394752675F3}']
  {Methods}
  procedure onScanCompleted(path: JString; uri: Jnet_Uri); cdecl;
end;
TJMediaScannerConnection_OnScanCompletedListener = class(TJavaGenericImport<JMediaScannerConnection_OnScanCompletedListenerClass, JMediaScannerConnection_OnScanCompletedListener>) end;

JMediaScannerConnection_MediaScannerConnectionClientClass = interface(JMediaScannerConnection_OnScanCompletedListenerClass)
['{BA6A6FBB-C272-4BFF-986D-6DD9ABB7EE36}']
end;

[JavaSignature('android/media/MediaScannerConnection$MediaScannerConnectionClient')]
JMediaScannerConnection_MediaScannerConnectionClient = interface(JMediaScannerConnection_OnScanCompletedListener)
['{02CB4035-E2C0-4C2D-9250-5805C45E789D}']
  {Methods}
  procedure onMediaScannerConnected; cdecl;
  procedure onScanCompleted(path: JString; uri: Jnet_Uri); cdecl;
end;
TJMediaScannerConnection_MediaScannerConnectionClient = class(TJavaGenericImport<JMediaScannerConnection_MediaScannerConnectionClientClass, JMediaScannerConnection_MediaScannerConnectionClient>) end;

JMediaCryptoExceptionClass = interface(JExceptionClass)
['{36A5DB4A-6D5E-49B1-B7A0-5C25892D9C7E}']
  {Methods}
  function init(detailMessage: JString): JMediaCryptoException; cdecl;
end;

[JavaSignature('android/media/MediaCryptoException')]
JMediaCryptoException = interface(JException)
['{1039BDE3-1DD7-440A-A6B8-B55FE0B3658E}']
end;
TJMediaCryptoException = class(TJavaGenericImport<JMediaCryptoExceptionClass, JMediaCryptoException>) end;

JMediaRecorder_VideoEncoderClass = interface(JObjectClass)
['{CAC0CF12-4686-461C-AB83-FF52102278BF}']
  {Property Methods}
  function _GetDEFAULT: Integer;
  function _GetH263: Integer;
  function _GetH264: Integer;
  function _GetMPEG_4_SP: Integer;
  {Properties}
  property DEFAULT: Integer read _GetDEFAULT;
  property H263: Integer read _GetH263;
  property H264: Integer read _GetH264;
  property MPEG_4_SP: Integer read _GetMPEG_4_SP;
end;

[JavaSignature('android/media/MediaRecorder$VideoEncoder')]
JMediaRecorder_VideoEncoder = interface(JObject)
['{7790955C-286A-4B61-B266-A95E1774953A}']
end;
TJMediaRecorder_VideoEncoder = class(TJavaGenericImport<JMediaRecorder_VideoEncoderClass, JMediaRecorder_VideoEncoder>) end;

JMediaRouter_VolumeCallbackClass = interface(JObjectClass)
['{F2ABA613-70F4-47B1-B5F4-D1CD8AB94202}']
  {Methods}
  function init: JMediaRouter_VolumeCallback; cdecl;
end;

[JavaSignature('android/media/MediaRouter$VolumeCallback')]
JMediaRouter_VolumeCallback = interface(JObject)
['{9D096802-5425-4970-BCBE-B112A7E4C19E}']
  {Methods}
  procedure onVolumeSetRequest(info: JMediaRouter_RouteInfo; volume: Integer); cdecl;
  procedure onVolumeUpdateRequest(info: JMediaRouter_RouteInfo; direction: Integer); cdecl;
end;
TJMediaRouter_VolumeCallback = class(TJavaGenericImport<JMediaRouter_VolumeCallbackClass, JMediaRouter_VolumeCallback>) end;

JEqualizerClass = interface(JAudioEffectClass)
['{C4A093C1-0B55-4362-850E-FD34701A91F6}']
  {Property Methods}
  function _GetPARAM_BAND_FREQ_RANGE: Integer;
  function _GetPARAM_BAND_LEVEL: Integer;
  function _GetPARAM_CENTER_FREQ: Integer;
  function _GetPARAM_CURRENT_PRESET: Integer;
  function _GetPARAM_GET_BAND: Integer;
  function _GetPARAM_GET_NUM_OF_PRESETS: Integer;
  function _GetPARAM_GET_PRESET_NAME: Integer;
  function _GetPARAM_LEVEL_RANGE: Integer;
  function _GetPARAM_NUM_BANDS: Integer;
  function _GetPARAM_STRING_SIZE_MAX: Integer;
  {Methods}
  function init(priority: Integer; audioSession: Integer): JEqualizer; cdecl;
  {Properties}
  property PARAM_BAND_FREQ_RANGE: Integer read _GetPARAM_BAND_FREQ_RANGE;
  property PARAM_BAND_LEVEL: Integer read _GetPARAM_BAND_LEVEL;
  property PARAM_CENTER_FREQ: Integer read _GetPARAM_CENTER_FREQ;
  property PARAM_CURRENT_PRESET: Integer read _GetPARAM_CURRENT_PRESET;
  property PARAM_GET_BAND: Integer read _GetPARAM_GET_BAND;
  property PARAM_GET_NUM_OF_PRESETS: Integer read _GetPARAM_GET_NUM_OF_PRESETS;
  property PARAM_GET_PRESET_NAME: Integer read _GetPARAM_GET_PRESET_NAME;
  property PARAM_LEVEL_RANGE: Integer read _GetPARAM_LEVEL_RANGE;
  property PARAM_NUM_BANDS: Integer read _GetPARAM_NUM_BANDS;
  property PARAM_STRING_SIZE_MAX: Integer read _GetPARAM_STRING_SIZE_MAX;
end;

[JavaSignature('android/media/audiofx/Equalizer')]
JEqualizer = interface(JAudioEffect)
['{AFA7DCA7-C3EA-4D50-811D-1788E9648023}']
  {Methods}
  function getBand(frequency: Integer): SmallInt; cdecl;
  function getBandFreqRange(band: SmallInt): TJavaArray<Integer>; cdecl;
  function getBandLevel(band: SmallInt): SmallInt; cdecl;
  function getBandLevelRange: TJavaArray<SmallInt>; cdecl;
  function getCenterFreq(band: SmallInt): Integer; cdecl;
  function getCurrentPreset: SmallInt; cdecl;
  function getNumberOfBands: SmallInt; cdecl;
  function getNumberOfPresets: SmallInt; cdecl;
  function getPresetName(preset: SmallInt): JString; cdecl;
  function getProperties: JEqualizer_Settings; cdecl;
  procedure setBandLevel(band: SmallInt; level: SmallInt); cdecl;
  procedure setParameterListener(listener: JEqualizer_OnParameterChangeListener); cdecl;
  procedure setProperties(settings: JEqualizer_Settings); cdecl;
  procedure usePreset(preset: SmallInt); cdecl;
end;
TJEqualizer = class(TJavaGenericImport<JEqualizerClass, JEqualizer>) end;

JAudioFormatClass = interface(JObjectClass)
['{C173E007-9835-486C-AA1A-700127383914}']
  {Property Methods}
  function _GetCHANNEL_CONFIGURATION_DEFAULT: Integer;
  function _GetCHANNEL_CONFIGURATION_INVALID: Integer;
  function _GetCHANNEL_CONFIGURATION_MONO: Integer;
  function _GetCHANNEL_CONFIGURATION_STEREO: Integer;
  function _GetCHANNEL_INVALID: Integer;
  function _GetCHANNEL_IN_BACK: Integer;
  function _GetCHANNEL_IN_BACK_PROCESSED: Integer;
  function _GetCHANNEL_IN_DEFAULT: Integer;
  function _GetCHANNEL_IN_FRONT: Integer;
  function _GetCHANNEL_IN_FRONT_PROCESSED: Integer;
  function _GetCHANNEL_IN_LEFT: Integer;
  function _GetCHANNEL_IN_LEFT_PROCESSED: Integer;
  function _GetCHANNEL_IN_MONO: Integer;
  function _GetCHANNEL_IN_PRESSURE: Integer;
  function _GetCHANNEL_IN_RIGHT: Integer;
  function _GetCHANNEL_IN_RIGHT_PROCESSED: Integer;
  function _GetCHANNEL_IN_STEREO: Integer;
  function _GetCHANNEL_IN_VOICE_DNLINK: Integer;
  function _GetCHANNEL_IN_VOICE_UPLINK: Integer;
  function _GetCHANNEL_IN_X_AXIS: Integer;
  function _GetCHANNEL_IN_Y_AXIS: Integer;
  function _GetCHANNEL_IN_Z_AXIS: Integer;
  function _GetCHANNEL_OUT_5POINT1: Integer;
  function _GetCHANNEL_OUT_7POINT1: Integer;
  function _GetCHANNEL_OUT_BACK_CENTER: Integer;
  function _GetCHANNEL_OUT_BACK_LEFT: Integer;
  function _GetCHANNEL_OUT_BACK_RIGHT: Integer;
  function _GetCHANNEL_OUT_DEFAULT: Integer;
  function _GetCHANNEL_OUT_FRONT_CENTER: Integer;
  function _GetCHANNEL_OUT_FRONT_LEFT: Integer;
  function _GetCHANNEL_OUT_FRONT_LEFT_OF_CENTER: Integer;
  function _GetCHANNEL_OUT_FRONT_RIGHT: Integer;
  function _GetCHANNEL_OUT_FRONT_RIGHT_OF_CENTER: Integer;
  function _GetCHANNEL_OUT_LOW_FREQUENCY: Integer;
  function _GetCHANNEL_OUT_MONO: Integer;
  function _GetCHANNEL_OUT_QUAD: Integer;
  function _GetCHANNEL_OUT_STEREO: Integer;
  function _GetCHANNEL_OUT_SURROUND: Integer;
  function _GetENCODING_DEFAULT: Integer;
  function _GetENCODING_INVALID: Integer;
  function _GetENCODING_PCM_16BIT: Integer;
  function _GetENCODING_PCM_8BIT: Integer;
  {Methods}
  function init: JAudioFormat; cdecl;
  {Properties}
  property CHANNEL_CONFIGURATION_DEFAULT: Integer read _GetCHANNEL_CONFIGURATION_DEFAULT;
  property CHANNEL_CONFIGURATION_INVALID: Integer read _GetCHANNEL_CONFIGURATION_INVALID;
  property CHANNEL_CONFIGURATION_MONO: Integer read _GetCHANNEL_CONFIGURATION_MONO;
  property CHANNEL_CONFIGURATION_STEREO: Integer read _GetCHANNEL_CONFIGURATION_STEREO;
  property CHANNEL_INVALID: Integer read _GetCHANNEL_INVALID;
  property CHANNEL_IN_BACK: Integer read _GetCHANNEL_IN_BACK;
  property CHANNEL_IN_BACK_PROCESSED: Integer read _GetCHANNEL_IN_BACK_PROCESSED;
  property CHANNEL_IN_DEFAULT: Integer read _GetCHANNEL_IN_DEFAULT;
  property CHANNEL_IN_FRONT: Integer read _GetCHANNEL_IN_FRONT;
  property CHANNEL_IN_FRONT_PROCESSED: Integer read _GetCHANNEL_IN_FRONT_PROCESSED;
  property CHANNEL_IN_LEFT: Integer read _GetCHANNEL_IN_LEFT;
  property CHANNEL_IN_LEFT_PROCESSED: Integer read _GetCHANNEL_IN_LEFT_PROCESSED;
  property CHANNEL_IN_MONO: Integer read _GetCHANNEL_IN_MONO;
  property CHANNEL_IN_PRESSURE: Integer read _GetCHANNEL_IN_PRESSURE;
  property CHANNEL_IN_RIGHT: Integer read _GetCHANNEL_IN_RIGHT;
  property CHANNEL_IN_RIGHT_PROCESSED: Integer read _GetCHANNEL_IN_RIGHT_PROCESSED;
  property CHANNEL_IN_STEREO: Integer read _GetCHANNEL_IN_STEREO;
  property CHANNEL_IN_VOICE_DNLINK: Integer read _GetCHANNEL_IN_VOICE_DNLINK;
  property CHANNEL_IN_VOICE_UPLINK: Integer read _GetCHANNEL_IN_VOICE_UPLINK;
  property CHANNEL_IN_X_AXIS: Integer read _GetCHANNEL_IN_X_AXIS;
  property CHANNEL_IN_Y_AXIS: Integer read _GetCHANNEL_IN_Y_AXIS;
  property CHANNEL_IN_Z_AXIS: Integer read _GetCHANNEL_IN_Z_AXIS;
  property CHANNEL_OUT_5POINT1: Integer read _GetCHANNEL_OUT_5POINT1;
  property CHANNEL_OUT_7POINT1: Integer read _GetCHANNEL_OUT_7POINT1;
  property CHANNEL_OUT_BACK_CENTER: Integer read _GetCHANNEL_OUT_BACK_CENTER;
  property CHANNEL_OUT_BACK_LEFT: Integer read _GetCHANNEL_OUT_BACK_LEFT;
  property CHANNEL_OUT_BACK_RIGHT: Integer read _GetCHANNEL_OUT_BACK_RIGHT;
  property CHANNEL_OUT_DEFAULT: Integer read _GetCHANNEL_OUT_DEFAULT;
  property CHANNEL_OUT_FRONT_CENTER: Integer read _GetCHANNEL_OUT_FRONT_CENTER;
  property CHANNEL_OUT_FRONT_LEFT: Integer read _GetCHANNEL_OUT_FRONT_LEFT;
  property CHANNEL_OUT_FRONT_LEFT_OF_CENTER: Integer read _GetCHANNEL_OUT_FRONT_LEFT_OF_CENTER;
  property CHANNEL_OUT_FRONT_RIGHT: Integer read _GetCHANNEL_OUT_FRONT_RIGHT;
  property CHANNEL_OUT_FRONT_RIGHT_OF_CENTER: Integer read _GetCHANNEL_OUT_FRONT_RIGHT_OF_CENTER;
  property CHANNEL_OUT_LOW_FREQUENCY: Integer read _GetCHANNEL_OUT_LOW_FREQUENCY;
  property CHANNEL_OUT_MONO: Integer read _GetCHANNEL_OUT_MONO;
  property CHANNEL_OUT_QUAD: Integer read _GetCHANNEL_OUT_QUAD;
  property CHANNEL_OUT_STEREO: Integer read _GetCHANNEL_OUT_STEREO;
  property CHANNEL_OUT_SURROUND: Integer read _GetCHANNEL_OUT_SURROUND;
  property ENCODING_DEFAULT: Integer read _GetENCODING_DEFAULT;
  property ENCODING_INVALID: Integer read _GetENCODING_INVALID;
  property ENCODING_PCM_16BIT: Integer read _GetENCODING_PCM_16BIT;
  property ENCODING_PCM_8BIT: Integer read _GetENCODING_PCM_8BIT;
end;

[JavaSignature('android/media/AudioFormat')]
JAudioFormat = interface(JObject)
['{54729F91-CB43-4106-8A44-08A8FFF26397}']
end;
TJAudioFormat = class(TJavaGenericImport<JAudioFormatClass, JAudioFormat>) end;

JMediaExtractorClass = interface(JObjectClass)
['{CAAD040F-C796-427A-91E6-29BD622C8972}']
  {Property Methods}
  function _GetSAMPLE_FLAG_ENCRYPTED: Integer;
  function _GetSAMPLE_FLAG_SYNC: Integer;
  function _GetSEEK_TO_CLOSEST_SYNC: Integer;
  function _GetSEEK_TO_NEXT_SYNC: Integer;
  function _GetSEEK_TO_PREVIOUS_SYNC: Integer;
  {Methods}
  function init: JMediaExtractor; cdecl;
  {Properties}
  property SAMPLE_FLAG_ENCRYPTED: Integer read _GetSAMPLE_FLAG_ENCRYPTED;
  property SAMPLE_FLAG_SYNC: Integer read _GetSAMPLE_FLAG_SYNC;
  property SEEK_TO_CLOSEST_SYNC: Integer read _GetSEEK_TO_CLOSEST_SYNC;
  property SEEK_TO_NEXT_SYNC: Integer read _GetSEEK_TO_NEXT_SYNC;
  property SEEK_TO_PREVIOUS_SYNC: Integer read _GetSEEK_TO_PREVIOUS_SYNC;
end;

[JavaSignature('android/media/MediaExtractor')]
JMediaExtractor = interface(JObject)
['{0B544D30-92E2-4592-86C8-80CB330796C9}']
  {Methods}
  function advance: Boolean; cdecl;
  function getCachedDuration: Int64; cdecl;
  function getSampleCryptoInfo(info: JMediaCodec_CryptoInfo): Boolean; cdecl;
  function getSampleFlags: Integer; cdecl;
  function getSampleTime: Int64; cdecl;
  function getSampleTrackIndex: Integer; cdecl;
  function getTrackCount: Integer; cdecl;
  function getTrackFormat(index: Integer): JMediaFormat; cdecl;
  function hasCacheReachedEndOfStream: Boolean; cdecl;
  procedure release; cdecl;
  procedure seekTo(timeUs: Int64; mode: Integer); cdecl;
  procedure selectTrack(index: Integer); cdecl;
  procedure setDataSource(context: JContext; uri: Jnet_Uri; headers: JMap); cdecl; overload;
  procedure setDataSource(path: JString; headers: JMap); cdecl; overload;
  procedure setDataSource(path: JString); cdecl; overload;
  procedure setDataSource(fd: JFileDescriptor); cdecl; overload;
  procedure setDataSource(fd: JFileDescriptor; offset: Int64; length: Int64); cdecl; overload;
  procedure unselectTrack(index: Integer); cdecl;
end;
TJMediaExtractor = class(TJavaGenericImport<JMediaExtractorClass, JMediaExtractor>) end;

JMediaMetadataRetrieverClass = interface(JObjectClass)
['{A5C39E03-F4F3-4A10-B43C-0A01C64CB8A9}']
  {Property Methods}
  function _GetMETADATA_KEY_ALBUM: Integer;
  function _GetMETADATA_KEY_ALBUMARTIST: Integer;
  function _GetMETADATA_KEY_ARTIST: Integer;
  function _GetMETADATA_KEY_AUTHOR: Integer;
  function _GetMETADATA_KEY_BITRATE: Integer;
  function _GetMETADATA_KEY_CD_TRACK_NUMBER: Integer;
  function _GetMETADATA_KEY_COMPILATION: Integer;
  function _GetMETADATA_KEY_COMPOSER: Integer;
  function _GetMETADATA_KEY_DATE: Integer;
  function _GetMETADATA_KEY_DISC_NUMBER: Integer;
  function _GetMETADATA_KEY_DURATION: Integer;
  function _GetMETADATA_KEY_GENRE: Integer;
  function _GetMETADATA_KEY_HAS_AUDIO: Integer;
  function _GetMETADATA_KEY_HAS_VIDEO: Integer;
  function _GetMETADATA_KEY_LOCATION: Integer;
  function _GetMETADATA_KEY_MIMETYPE: Integer;
  function _GetMETADATA_KEY_NUM_TRACKS: Integer;
  function _GetMETADATA_KEY_TITLE: Integer;
  function _GetMETADATA_KEY_VIDEO_HEIGHT: Integer;
  function _GetMETADATA_KEY_VIDEO_ROTATION: Integer;
  function _GetMETADATA_KEY_VIDEO_WIDTH: Integer;
  function _GetMETADATA_KEY_WRITER: Integer;
  function _GetMETADATA_KEY_YEAR: Integer;
  function _GetOPTION_CLOSEST: Integer;
  function _GetOPTION_CLOSEST_SYNC: Integer;
  function _GetOPTION_NEXT_SYNC: Integer;
  function _GetOPTION_PREVIOUS_SYNC: Integer;
  {Methods}
  function init: JMediaMetadataRetriever; cdecl;
  {Properties}
  property METADATA_KEY_ALBUM: Integer read _GetMETADATA_KEY_ALBUM;
  property METADATA_KEY_ALBUMARTIST: Integer read _GetMETADATA_KEY_ALBUMARTIST;
  property METADATA_KEY_ARTIST: Integer read _GetMETADATA_KEY_ARTIST;
  property METADATA_KEY_AUTHOR: Integer read _GetMETADATA_KEY_AUTHOR;
  property METADATA_KEY_BITRATE: Integer read _GetMETADATA_KEY_BITRATE;
  property METADATA_KEY_CD_TRACK_NUMBER: Integer read _GetMETADATA_KEY_CD_TRACK_NUMBER;
  property METADATA_KEY_COMPILATION: Integer read _GetMETADATA_KEY_COMPILATION;
  property METADATA_KEY_COMPOSER: Integer read _GetMETADATA_KEY_COMPOSER;
  property METADATA_KEY_DATE: Integer read _GetMETADATA_KEY_DATE;
  property METADATA_KEY_DISC_NUMBER: Integer read _GetMETADATA_KEY_DISC_NUMBER;
  property METADATA_KEY_DURATION: Integer read _GetMETADATA_KEY_DURATION;
  property METADATA_KEY_GENRE: Integer read _GetMETADATA_KEY_GENRE;
  property METADATA_KEY_HAS_AUDIO: Integer read _GetMETADATA_KEY_HAS_AUDIO;
  property METADATA_KEY_HAS_VIDEO: Integer read _GetMETADATA_KEY_HAS_VIDEO;
  property METADATA_KEY_LOCATION: Integer read _GetMETADATA_KEY_LOCATION;
  property METADATA_KEY_MIMETYPE: Integer read _GetMETADATA_KEY_MIMETYPE;
  property METADATA_KEY_NUM_TRACKS: Integer read _GetMETADATA_KEY_NUM_TRACKS;
  property METADATA_KEY_TITLE: Integer read _GetMETADATA_KEY_TITLE;
  property METADATA_KEY_VIDEO_HEIGHT: Integer read _GetMETADATA_KEY_VIDEO_HEIGHT;
  property METADATA_KEY_VIDEO_ROTATION: Integer read _GetMETADATA_KEY_VIDEO_ROTATION;
  property METADATA_KEY_VIDEO_WIDTH: Integer read _GetMETADATA_KEY_VIDEO_WIDTH;
  property METADATA_KEY_WRITER: Integer read _GetMETADATA_KEY_WRITER;
  property METADATA_KEY_YEAR: Integer read _GetMETADATA_KEY_YEAR;
  property OPTION_CLOSEST: Integer read _GetOPTION_CLOSEST;
  property OPTION_CLOSEST_SYNC: Integer read _GetOPTION_CLOSEST_SYNC;
  property OPTION_NEXT_SYNC: Integer read _GetOPTION_NEXT_SYNC;
  property OPTION_PREVIOUS_SYNC: Integer read _GetOPTION_PREVIOUS_SYNC;
end;

[JavaSignature('android/media/MediaMetadataRetriever')]
JMediaMetadataRetriever = interface(JObject)
['{FE494ED1-BDE3-4DC1-BB24-35F061DE55DC}']
  {Methods}
  function extractMetadata(keyCode: Integer): JString; cdecl;
  function getEmbeddedPicture: TJavaArray<Byte>; cdecl;
  function getFrameAtTime(timeUs: Int64; option: Integer): JBitmap; cdecl; overload;
  function getFrameAtTime(timeUs: Int64): JBitmap; cdecl; overload;
  function getFrameAtTime: JBitmap; cdecl; overload;
  procedure release; cdecl;
  procedure setDataSource(path: JString); cdecl; overload;
  procedure setDataSource(uri: JString; headers: JMap); cdecl; overload;
  procedure setDataSource(fd: JFileDescriptor; offset: Int64; length: Int64); cdecl; overload;
  procedure setDataSource(fd: JFileDescriptor); cdecl; overload;
  procedure setDataSource(context: JContext; uri: Jnet_Uri); cdecl; overload;
end;
TJMediaMetadataRetriever = class(TJavaGenericImport<JMediaMetadataRetrieverClass, JMediaMetadataRetriever>) end;

JMediaCodecInfo_CodecCapabilitiesClass = interface(JObjectClass)
['{007827D3-EC90-41C8-BA89-794265D12834}']
  {Property Methods}
  function _GetCOLOR_Format12bitRGB444: Integer;
  function _GetCOLOR_Format16bitARGB1555: Integer;
  function _GetCOLOR_Format16bitARGB4444: Integer;
  function _GetCOLOR_Format16bitBGR565: Integer;
  function _GetCOLOR_Format16bitRGB565: Integer;
  function _GetCOLOR_Format18BitBGR666: Integer;
  function _GetCOLOR_Format18bitARGB1665: Integer;
  function _GetCOLOR_Format18bitRGB666: Integer;
  function _GetCOLOR_Format19bitARGB1666: Integer;
  function _GetCOLOR_Format24BitABGR6666: Integer;
  function _GetCOLOR_Format24BitARGB6666: Integer;
  function _GetCOLOR_Format24bitARGB1887: Integer;
  function _GetCOLOR_Format24bitBGR888: Integer;
  function _GetCOLOR_Format24bitRGB888: Integer;
  function _GetCOLOR_Format25bitARGB1888: Integer;
  function _GetCOLOR_Format32bitARGB8888: Integer;
  function _GetCOLOR_Format32bitBGRA8888: Integer;
  function _GetCOLOR_Format8bitRGB332: Integer;
  function _GetCOLOR_FormatCbYCrY: Integer;
  function _GetCOLOR_FormatCrYCbY: Integer;
  function _GetCOLOR_FormatL16: Integer;
  function _GetCOLOR_FormatL2: Integer;
  function _GetCOLOR_FormatL24: Integer;
  function _GetCOLOR_FormatL32: Integer;
  function _GetCOLOR_FormatL4: Integer;
  function _GetCOLOR_FormatL8: Integer;
  function _GetCOLOR_FormatMonochrome: Integer;
  function _GetCOLOR_FormatRawBayer10bit: Integer;
  function _GetCOLOR_FormatRawBayer8bit: Integer;
  function _GetCOLOR_FormatRawBayer8bitcompressed: Integer;
  function _GetCOLOR_FormatYCbYCr: Integer;
  function _GetCOLOR_FormatYCrYCb: Integer;
  function _GetCOLOR_FormatYUV411PackedPlanar: Integer;
  function _GetCOLOR_FormatYUV411Planar: Integer;
  function _GetCOLOR_FormatYUV420PackedPlanar: Integer;
  function _GetCOLOR_FormatYUV420PackedSemiPlanar: Integer;
  function _GetCOLOR_FormatYUV420Planar: Integer;
  function _GetCOLOR_FormatYUV420SemiPlanar: Integer;
  function _GetCOLOR_FormatYUV422PackedPlanar: Integer;
  function _GetCOLOR_FormatYUV422PackedSemiPlanar: Integer;
  function _GetCOLOR_FormatYUV422Planar: Integer;
  function _GetCOLOR_FormatYUV422SemiPlanar: Integer;
  function _GetCOLOR_FormatYUV444Interleaved: Integer;
  function _GetCOLOR_QCOM_FormatYUV420SemiPlanar: Integer;
  function _GetCOLOR_TI_FormatYUV420PackedSemiPlanar: Integer;
  {Methods}
  function init: JMediaCodecInfo_CodecCapabilities; cdecl;
  {Properties}
  property COLOR_Format12bitRGB444: Integer read _GetCOLOR_Format12bitRGB444;
  property COLOR_Format16bitARGB1555: Integer read _GetCOLOR_Format16bitARGB1555;
  property COLOR_Format16bitARGB4444: Integer read _GetCOLOR_Format16bitARGB4444;
  property COLOR_Format16bitBGR565: Integer read _GetCOLOR_Format16bitBGR565;
  property COLOR_Format16bitRGB565: Integer read _GetCOLOR_Format16bitRGB565;
  property COLOR_Format18BitBGR666: Integer read _GetCOLOR_Format18BitBGR666;
  property COLOR_Format18bitARGB1665: Integer read _GetCOLOR_Format18bitARGB1665;
  property COLOR_Format18bitRGB666: Integer read _GetCOLOR_Format18bitRGB666;
  property COLOR_Format19bitARGB1666: Integer read _GetCOLOR_Format19bitARGB1666;
  property COLOR_Format24BitABGR6666: Integer read _GetCOLOR_Format24BitABGR6666;
  property COLOR_Format24BitARGB6666: Integer read _GetCOLOR_Format24BitARGB6666;
  property COLOR_Format24bitARGB1887: Integer read _GetCOLOR_Format24bitARGB1887;
  property COLOR_Format24bitBGR888: Integer read _GetCOLOR_Format24bitBGR888;
  property COLOR_Format24bitRGB888: Integer read _GetCOLOR_Format24bitRGB888;
  property COLOR_Format25bitARGB1888: Integer read _GetCOLOR_Format25bitARGB1888;
  property COLOR_Format32bitARGB8888: Integer read _GetCOLOR_Format32bitARGB8888;
  property COLOR_Format32bitBGRA8888: Integer read _GetCOLOR_Format32bitBGRA8888;
  property COLOR_Format8bitRGB332: Integer read _GetCOLOR_Format8bitRGB332;
  property COLOR_FormatCbYCrY: Integer read _GetCOLOR_FormatCbYCrY;
  property COLOR_FormatCrYCbY: Integer read _GetCOLOR_FormatCrYCbY;
  property COLOR_FormatL16: Integer read _GetCOLOR_FormatL16;
  property COLOR_FormatL2: Integer read _GetCOLOR_FormatL2;
  property COLOR_FormatL24: Integer read _GetCOLOR_FormatL24;
  property COLOR_FormatL32: Integer read _GetCOLOR_FormatL32;
  property COLOR_FormatL4: Integer read _GetCOLOR_FormatL4;
  property COLOR_FormatL8: Integer read _GetCOLOR_FormatL8;
  property COLOR_FormatMonochrome: Integer read _GetCOLOR_FormatMonochrome;
  property COLOR_FormatRawBayer10bit: Integer read _GetCOLOR_FormatRawBayer10bit;
  property COLOR_FormatRawBayer8bit: Integer read _GetCOLOR_FormatRawBayer8bit;
  property COLOR_FormatRawBayer8bitcompressed: Integer read _GetCOLOR_FormatRawBayer8bitcompressed;
  property COLOR_FormatYCbYCr: Integer read _GetCOLOR_FormatYCbYCr;
  property COLOR_FormatYCrYCb: Integer read _GetCOLOR_FormatYCrYCb;
  property COLOR_FormatYUV411PackedPlanar: Integer read _GetCOLOR_FormatYUV411PackedPlanar;
  property COLOR_FormatYUV411Planar: Integer read _GetCOLOR_FormatYUV411Planar;
  property COLOR_FormatYUV420PackedPlanar: Integer read _GetCOLOR_FormatYUV420PackedPlanar;
  property COLOR_FormatYUV420PackedSemiPlanar: Integer read _GetCOLOR_FormatYUV420PackedSemiPlanar;
  property COLOR_FormatYUV420Planar: Integer read _GetCOLOR_FormatYUV420Planar;
  property COLOR_FormatYUV420SemiPlanar: Integer read _GetCOLOR_FormatYUV420SemiPlanar;
  property COLOR_FormatYUV422PackedPlanar: Integer read _GetCOLOR_FormatYUV422PackedPlanar;
  property COLOR_FormatYUV422PackedSemiPlanar: Integer read _GetCOLOR_FormatYUV422PackedSemiPlanar;
  property COLOR_FormatYUV422Planar: Integer read _GetCOLOR_FormatYUV422Planar;
  property COLOR_FormatYUV422SemiPlanar: Integer read _GetCOLOR_FormatYUV422SemiPlanar;
  property COLOR_FormatYUV444Interleaved: Integer read _GetCOLOR_FormatYUV444Interleaved;
  property COLOR_QCOM_FormatYUV420SemiPlanar: Integer read _GetCOLOR_QCOM_FormatYUV420SemiPlanar;
  property COLOR_TI_FormatYUV420PackedSemiPlanar: Integer read _GetCOLOR_TI_FormatYUV420PackedSemiPlanar;
end;

[JavaSignature('android/media/MediaCodecInfo$CodecCapabilities')]
JMediaCodecInfo_CodecCapabilities = interface(JObject)
['{A04BF209-41B0-4DA2-B376-447B7BBC6237}']
  {Property Methods}
  function _GetcolorFormats: TJavaArray<Integer>;
  procedure _SetcolorFormats(Value: TJavaArray<Integer>);
  function _GetprofileLevels: TJavaObjectArray<JMediaCodecInfo_CodecProfileLevel>;
  procedure _SetprofileLevels(Value: TJavaObjectArray<JMediaCodecInfo_CodecProfileLevel>);
  {Properties}
  property colorFormats: TJavaArray<Integer> read _GetcolorFormats write _SetcolorFormats;
  property profileLevels: TJavaObjectArray<JMediaCodecInfo_CodecProfileLevel> read _GetprofileLevels write _SetprofileLevels;
end;
TJMediaCodecInfo_CodecCapabilities = class(TJavaGenericImport<JMediaCodecInfo_CodecCapabilitiesClass, JMediaCodecInfo_CodecCapabilities>) end;

JRemoteControlClientClass = interface(JObjectClass)
['{3BCB8D6B-7F9E-4559-97A5-FD7A0B386EBA}']
  {Property Methods}
  function _GetFLAG_KEY_MEDIA_FAST_FORWARD: Integer;
  function _GetFLAG_KEY_MEDIA_NEXT: Integer;
  function _GetFLAG_KEY_MEDIA_PAUSE: Integer;
  function _GetFLAG_KEY_MEDIA_PLAY: Integer;
  function _GetFLAG_KEY_MEDIA_PLAY_PAUSE: Integer;
  function _GetFLAG_KEY_MEDIA_PREVIOUS: Integer;
  function _GetFLAG_KEY_MEDIA_REWIND: Integer;
  function _GetFLAG_KEY_MEDIA_STOP: Integer;
  function _GetPLAYSTATE_BUFFERING: Integer;
  function _GetPLAYSTATE_ERROR: Integer;
  function _GetPLAYSTATE_FAST_FORWARDING: Integer;
  function _GetPLAYSTATE_PAUSED: Integer;
  function _GetPLAYSTATE_PLAYING: Integer;
  function _GetPLAYSTATE_REWINDING: Integer;
  function _GetPLAYSTATE_SKIPPING_BACKWARDS: Integer;
  function _GetPLAYSTATE_SKIPPING_FORWARDS: Integer;
  function _GetPLAYSTATE_STOPPED: Integer;
  {Methods}
  function init(mediaButtonIntent: JPendingIntent): JRemoteControlClient; cdecl; overload;
  function init(mediaButtonIntent: JPendingIntent; looper: JLooper): JRemoteControlClient; cdecl; overload;
  {Properties}
  property FLAG_KEY_MEDIA_FAST_FORWARD: Integer read _GetFLAG_KEY_MEDIA_FAST_FORWARD;
  property FLAG_KEY_MEDIA_NEXT: Integer read _GetFLAG_KEY_MEDIA_NEXT;
  property FLAG_KEY_MEDIA_PAUSE: Integer read _GetFLAG_KEY_MEDIA_PAUSE;
  property FLAG_KEY_MEDIA_PLAY: Integer read _GetFLAG_KEY_MEDIA_PLAY;
  property FLAG_KEY_MEDIA_PLAY_PAUSE: Integer read _GetFLAG_KEY_MEDIA_PLAY_PAUSE;
  property FLAG_KEY_MEDIA_PREVIOUS: Integer read _GetFLAG_KEY_MEDIA_PREVIOUS;
  property FLAG_KEY_MEDIA_REWIND: Integer read _GetFLAG_KEY_MEDIA_REWIND;
  property FLAG_KEY_MEDIA_STOP: Integer read _GetFLAG_KEY_MEDIA_STOP;
  property PLAYSTATE_BUFFERING: Integer read _GetPLAYSTATE_BUFFERING;
  property PLAYSTATE_ERROR: Integer read _GetPLAYSTATE_ERROR;
  property PLAYSTATE_FAST_FORWARDING: Integer read _GetPLAYSTATE_FAST_FORWARDING;
  property PLAYSTATE_PAUSED: Integer read _GetPLAYSTATE_PAUSED;
  property PLAYSTATE_PLAYING: Integer read _GetPLAYSTATE_PLAYING;
  property PLAYSTATE_REWINDING: Integer read _GetPLAYSTATE_REWINDING;
  property PLAYSTATE_SKIPPING_BACKWARDS: Integer read _GetPLAYSTATE_SKIPPING_BACKWARDS;
  property PLAYSTATE_SKIPPING_FORWARDS: Integer read _GetPLAYSTATE_SKIPPING_FORWARDS;
  property PLAYSTATE_STOPPED: Integer read _GetPLAYSTATE_STOPPED;
end;

[JavaSignature('android/media/RemoteControlClient')]
JRemoteControlClient = interface(JObject)
['{63280B1B-0F55-4FE3-B931-E5E8DE0591E3}']
  {Methods}
  function editMetadata(startEmpty: Boolean): JRemoteControlClient_MetadataEditor; cdecl;
  procedure setPlaybackState(state: Integer); cdecl;
  procedure setTransportControlFlags(transportControlFlags: Integer); cdecl;
end;
TJRemoteControlClient = class(TJavaGenericImport<JRemoteControlClientClass, JRemoteControlClient>) end;

JMediaActionSoundClass = interface(JObjectClass)
['{2B4CEE61-D673-4EDE-9154-6C7D30F15988}']
  {Property Methods}
  function _GetFOCUS_COMPLETE: Integer;
  function _GetSHUTTER_CLICK: Integer;
  function _GetSTART_VIDEO_RECORDING: Integer;
  function _GetSTOP_VIDEO_RECORDING: Integer;
  {Methods}
  function init: JMediaActionSound; cdecl;
  {Properties}
  property FOCUS_COMPLETE: Integer read _GetFOCUS_COMPLETE;
  property SHUTTER_CLICK: Integer read _GetSHUTTER_CLICK;
  property START_VIDEO_RECORDING: Integer read _GetSTART_VIDEO_RECORDING;
  property STOP_VIDEO_RECORDING: Integer read _GetSTOP_VIDEO_RECORDING;
end;

[JavaSignature('android/media/MediaActionSound')]
JMediaActionSound = interface(JObject)
['{F9314010-8F95-4079-A513-EA752C916635}']
  {Methods}
  procedure load(soundName: Integer); cdecl;
  procedure play(soundName: Integer); cdecl;
  procedure release; cdecl;
end;
TJMediaActionSound = class(TJavaGenericImport<JMediaActionSoundClass, JMediaActionSound>) end;

JMediaRecorder_OnInfoListenerClass = interface(IJavaClass)
['{2243D66B-0BD1-49F1-8031-1432CC3EBBD6}']
end;

[JavaSignature('android/media/MediaRecorder$OnInfoListener')]
JMediaRecorder_OnInfoListener = interface(IJavaInstance)
['{49627774-9F01-4122-B7F3-62ED95C7DD6B}']
  {Methods}
  procedure onInfo(mr: JMediaRecorder; what: Integer; extra: Integer); cdecl;
end;
TJMediaRecorder_OnInfoListener = class(TJavaGenericImport<JMediaRecorder_OnInfoListenerClass, JMediaRecorder_OnInfoListener>) end;

JAutomaticGainControlClass = interface(JAudioEffectClass)
['{D3C90786-5E89-4FF8-8D7E-9E79A05CBB6A}']
  {Methods}
  function create(audioSession: Integer): JAutomaticGainControl; cdecl;
  function isAvailable: Boolean; cdecl;
end;

[JavaSignature('android/media/audiofx/AutomaticGainControl')]
JAutomaticGainControl = interface(JAudioEffect)
['{36F93C00-2A45-4AD0-B880-F79B56FE4096}']
end;
TJAutomaticGainControl = class(TJavaGenericImport<JAutomaticGainControlClass, JAutomaticGainControl>) end;

JBassBoostClass = interface(JAudioEffectClass)
['{A209500F-399B-4AD4-B8BB-7D8E574B745E}']
  {Property Methods}
  function _GetPARAM_STRENGTH: Integer;
  function _GetPARAM_STRENGTH_SUPPORTED: Integer;
  {Methods}
  function init(priority: Integer; audioSession: Integer): JBassBoost; cdecl;
  {Properties}
  property PARAM_STRENGTH: Integer read _GetPARAM_STRENGTH;
  property PARAM_STRENGTH_SUPPORTED: Integer read _GetPARAM_STRENGTH_SUPPORTED;
end;

[JavaSignature('android/media/audiofx/BassBoost')]
JBassBoost = interface(JAudioEffect)
['{38228576-3D9D-42B1-82AD-30EA32654FDD}']
  {Methods}
  function getProperties: JBassBoost_Settings; cdecl;
  function getRoundedStrength: SmallInt; cdecl;
  function getStrengthSupported: Boolean; cdecl;
  procedure setParameterListener(listener: JBassBoost_OnParameterChangeListener); cdecl;
  procedure setProperties(settings: JBassBoost_Settings); cdecl;
  procedure setStrength(strength: SmallInt); cdecl;
end;
TJBassBoost = class(TJavaGenericImport<JBassBoostClass, JBassBoost>) end;

JPresetReverb_OnParameterChangeListenerClass = interface(IJavaClass)
['{CF4131D1-77D9-444D-B8B5-FD871FF14C33}']
end;

[JavaSignature('android/media/audiofx/PresetReverb$OnParameterChangeListener')]
JPresetReverb_OnParameterChangeListener = interface(IJavaInstance)
['{737B2E06-FD75-46B4-938E-E8DD6530E263}']
  {Methods}
  procedure onParameterChange(effect: JPresetReverb; status: Integer; param: Integer; value: SmallInt); cdecl;
end;
TJPresetReverb_OnParameterChangeListener = class(TJavaGenericImport<JPresetReverb_OnParameterChangeListenerClass, JPresetReverb_OnParameterChangeListener>) end;

JMediaPlayer_OnPreparedListenerClass = interface(IJavaClass)
['{FF67A260-7174-40EF-B53B-DA2BE4CDD72B}']
end;

[JavaSignature('android/media/MediaPlayer$OnPreparedListener')]
JMediaPlayer_OnPreparedListener = interface(IJavaInstance)
['{4082D9D4-82AE-4A31-9C58-D8CA78DC1A2B}']
  {Methods}
  procedure onPrepared(mp: JMediaPlayer); cdecl;
end;
TJMediaPlayer_OnPreparedListener = class(TJavaGenericImport<JMediaPlayer_OnPreparedListenerClass, JMediaPlayer_OnPreparedListener>) end;

JSoundPoolClass = interface(JObjectClass)
['{DFFA6151-B321-4111-A703-864E406FDA28}']
  {Methods}
  function init(maxStreams: Integer; streamType: Integer; srcQuality: Integer): JSoundPool; cdecl;
end;

[JavaSignature('android/media/SoundPool')]
JSoundPool = interface(JObject)
['{923ACDC6-1C0D-437A-9A44-D43B0BA82522}']
  {Methods}
  procedure autoPause; cdecl;
  procedure autoResume; cdecl;
  function load(path: JString; priority: Integer): Integer; cdecl; overload;
  function load(context: JContext; resId: Integer; priority: Integer): Integer; cdecl; overload;
  function load(afd: JAssetFileDescriptor; priority: Integer): Integer; cdecl; overload;
  function load(fd: JFileDescriptor; offset: Int64; length: Int64; priority: Integer): Integer; cdecl; overload;
  procedure pause(streamID: Integer); cdecl;
  function play(soundID: Integer; leftVolume: Single; rightVolume: Single; priority: Integer; loop: Integer; rate: Single): Integer; cdecl;
  procedure release; cdecl;
  procedure resume(streamID: Integer); cdecl;
  procedure setLoop(streamID: Integer; loop: Integer); cdecl;
  procedure setOnLoadCompleteListener(listener: JSoundPool_OnLoadCompleteListener); cdecl;
  procedure setPriority(streamID: Integer; priority: Integer); cdecl;
  procedure setRate(streamID: Integer; rate: Single); cdecl;
  procedure setVolume(streamID: Integer; leftVolume: Single; rightVolume: Single); cdecl;
  procedure stop(streamID: Integer); cdecl;
  function unload(soundID: Integer): Boolean; cdecl;
end;
TJSoundPool = class(TJavaGenericImport<JSoundPoolClass, JSoundPool>) end;

JMediaRouter_RouteGroupClass = interface(JMediaRouter_RouteInfoClass)
['{6B4C631F-9D54-4FC0-9B20-A52A75CB5ACC}']
end;

[JavaSignature('android/media/MediaRouter$RouteGroup')]
JMediaRouter_RouteGroup = interface(JMediaRouter_RouteInfo)
['{D55DC4A2-A487-467A-A3C0-873B2BF10409}']
  {Methods}
  procedure addRoute(route: JMediaRouter_RouteInfo); cdecl; overload;
  procedure addRoute(route: JMediaRouter_RouteInfo; insertAt: Integer); cdecl; overload;
  function getRouteAt(index: Integer): JMediaRouter_RouteInfo; cdecl;
  function getRouteCount: Integer; cdecl;
  procedure removeRoute(route: JMediaRouter_RouteInfo); cdecl; overload;
  procedure removeRoute(index: Integer); cdecl; overload;
  procedure requestSetVolume(volume: Integer); cdecl;
  procedure requestUpdateVolume(direction: Integer); cdecl;
  procedure setIconDrawable(icon: JDrawable); cdecl;
  procedure setIconResource(resId: Integer); cdecl;
  function toString: JString; cdecl;
end;
TJMediaRouter_RouteGroup = class(TJavaGenericImport<JMediaRouter_RouteGroupClass, JMediaRouter_RouteGroup>) end;

JMediaRouterClass = interface(JObjectClass)
['{BE499891-CFC7-47BB-946C-E633641DEA6F}']
  {Property Methods}
  function _GetROUTE_TYPE_LIVE_AUDIO: Integer;
  function _GetROUTE_TYPE_LIVE_VIDEO: Integer;
  function _GetROUTE_TYPE_USER: Integer;
  {Properties}
  property ROUTE_TYPE_LIVE_AUDIO: Integer read _GetROUTE_TYPE_LIVE_AUDIO;
  property ROUTE_TYPE_LIVE_VIDEO: Integer read _GetROUTE_TYPE_LIVE_VIDEO;
  property ROUTE_TYPE_USER: Integer read _GetROUTE_TYPE_USER;
end;

[JavaSignature('android/media/MediaRouter')]
JMediaRouter = interface(JObject)
['{461EECED-4158-4A87-87AD-568D08569170}']
  {Methods}
  procedure addCallback(types: Integer; cb: JMediaRouter_Callback); cdecl;
  procedure addUserRoute(info: JMediaRouter_UserRouteInfo); cdecl;
  procedure clearUserRoutes; cdecl;
  function createRouteCategory(name: JCharSequence; isGroupable: Boolean): JMediaRouter_RouteCategory; cdecl; overload;
  function createRouteCategory(nameResId: Integer; isGroupable: Boolean): JMediaRouter_RouteCategory; cdecl; overload;
  function createUserRoute(category: JMediaRouter_RouteCategory): JMediaRouter_UserRouteInfo; cdecl;
  function getCategoryAt(index: Integer): JMediaRouter_RouteCategory; cdecl;
  function getCategoryCount: Integer; cdecl;
  function getRouteAt(index: Integer): JMediaRouter_RouteInfo; cdecl;
  function getRouteCount: Integer; cdecl;
  function getSelectedRoute(type_: Integer): JMediaRouter_RouteInfo; cdecl;
  procedure removeCallback(cb: JMediaRouter_Callback); cdecl;
  procedure removeUserRoute(info: JMediaRouter_UserRouteInfo); cdecl;
  procedure selectRoute(types: Integer; route: JMediaRouter_RouteInfo); cdecl;
end;
TJMediaRouter = class(TJavaGenericImport<JMediaRouterClass, JMediaRouter>) end;

JAudioRecordClass = interface(JObjectClass)
['{52854509-E981-4598-90BD-591E322B9E11}']
  {Property Methods}
  function _GetERROR: Integer;
  function _GetERROR_BAD_VALUE: Integer;
  function _GetERROR_INVALID_OPERATION: Integer;
  function _GetRECORDSTATE_RECORDING: Integer;
  function _GetRECORDSTATE_STOPPED: Integer;
  function _GetSTATE_INITIALIZED: Integer;
  function _GetSTATE_UNINITIALIZED: Integer;
  function _GetSUCCESS: Integer;
  {Methods}
  function init(audioSource: Integer; sampleRateInHz: Integer; channelConfig: Integer; audioFormat: Integer; bufferSizeInBytes: Integer): JAudioRecord; cdecl;
  function getMinBufferSize(sampleRateInHz: Integer; channelConfig: Integer; audioFormat: Integer): Integer; cdecl;
  {Properties}
  property ERROR: Integer read _GetERROR;
  property ERROR_BAD_VALUE: Integer read _GetERROR_BAD_VALUE;
  property ERROR_INVALID_OPERATION: Integer read _GetERROR_INVALID_OPERATION;
  property RECORDSTATE_RECORDING: Integer read _GetRECORDSTATE_RECORDING;
  property RECORDSTATE_STOPPED: Integer read _GetRECORDSTATE_STOPPED;
  property STATE_INITIALIZED: Integer read _GetSTATE_INITIALIZED;
  property STATE_UNINITIALIZED: Integer read _GetSTATE_UNINITIALIZED;
  property SUCCESS: Integer read _GetSUCCESS;
end;

[JavaSignature('android/media/AudioRecord')]
JAudioRecord = interface(JObject)
['{512F5ADB-2452-441B-8FA6-C45F9A4449C0}']
  {Methods}
  function getAudioFormat: Integer; cdecl;
  function getAudioSessionId: Integer; cdecl;
  function getAudioSource: Integer; cdecl;
  function getChannelConfiguration: Integer; cdecl;
  function getChannelCount: Integer; cdecl;
  function getNotificationMarkerPosition: Integer; cdecl;
  function getPositionNotificationPeriod: Integer; cdecl;
  function getRecordingState: Integer; cdecl;
  function getSampleRate: Integer; cdecl;
  function getState: Integer; cdecl;
  function read(audioData: TJavaArray<Byte>; offsetInBytes: Integer; sizeInBytes: Integer): Integer; cdecl; overload;
  function read(audioData: TJavaArray<SmallInt>; offsetInShorts: Integer; sizeInShorts: Integer): Integer; cdecl; overload;
  procedure release; cdecl;
  function setNotificationMarkerPosition(markerInFrames: Integer): Integer; cdecl;
  function setPositionNotificationPeriod(periodInFrames: Integer): Integer; cdecl;
  procedure setRecordPositionUpdateListener(listener: JAudioRecord_OnRecordPositionUpdateListener); cdecl; overload;
  procedure setRecordPositionUpdateListener(listener: JAudioRecord_OnRecordPositionUpdateListener; handler: JHandler); cdecl; overload;
  procedure startRecording; cdecl; overload;
  procedure startRecording(syncEvent: JMediaSyncEvent); cdecl; overload;
  procedure stop; cdecl;
end;
TJAudioRecord = class(TJavaGenericImport<JAudioRecordClass, JAudioRecord>) end;

JMediaRecorderClass = interface(JObjectClass)
['{C70673D1-3BC5-4951-8C84-3023833CD3CE}']
  {Property Methods}
  function _GetMEDIA_ERROR_SERVER_DIED: Integer;
  function _GetMEDIA_RECORDER_ERROR_UNKNOWN: Integer;
  function _GetMEDIA_RECORDER_INFO_MAX_DURATION_REACHED: Integer;
  function _GetMEDIA_RECORDER_INFO_MAX_FILESIZE_REACHED: Integer;
  function _GetMEDIA_RECORDER_INFO_UNKNOWN: Integer;
  {Methods}
  function init: JMediaRecorder; cdecl;
  function getAudioSourceMax: Integer; cdecl;
  {Properties}
  property MEDIA_ERROR_SERVER_DIED: Integer read _GetMEDIA_ERROR_SERVER_DIED;
  property MEDIA_RECORDER_ERROR_UNKNOWN: Integer read _GetMEDIA_RECORDER_ERROR_UNKNOWN;
  property MEDIA_RECORDER_INFO_MAX_DURATION_REACHED: Integer read _GetMEDIA_RECORDER_INFO_MAX_DURATION_REACHED;
  property MEDIA_RECORDER_INFO_MAX_FILESIZE_REACHED: Integer read _GetMEDIA_RECORDER_INFO_MAX_FILESIZE_REACHED;
  property MEDIA_RECORDER_INFO_UNKNOWN: Integer read _GetMEDIA_RECORDER_INFO_UNKNOWN;
end;

[JavaSignature('android/media/MediaRecorder')]
JMediaRecorder = interface(JObject)
['{39D29E60-05D8-4B2C-A0F5-EF03FD1E0E20}']
  {Methods}
  function getMaxAmplitude: Integer; cdecl;
  procedure prepare; cdecl;
  procedure release; cdecl;
  procedure reset; cdecl;
  procedure setAudioChannels(numChannels: Integer); cdecl;
  procedure setAudioEncoder(audio_encoder: Integer); cdecl;
  procedure setAudioEncodingBitRate(bitRate: Integer); cdecl;
  procedure setAudioSamplingRate(samplingRate: Integer); cdecl;
  procedure setAudioSource(audio_source: Integer); cdecl;
  procedure setCamera(c: JCamera); cdecl;
  procedure setCaptureRate(fps: Double); cdecl;
  procedure setLocation(latitude: Single; longitude: Single); cdecl;
  procedure setMaxDuration(max_duration_ms: Integer); cdecl;
  procedure setMaxFileSize(max_filesize_bytes: Int64); cdecl;
  procedure setOnErrorListener(l: JMediaRecorder_OnErrorListener); cdecl;
  procedure setOnInfoListener(listener: JMediaRecorder_OnInfoListener); cdecl;
  procedure setOrientationHint(degrees: Integer); cdecl;
  procedure setOutputFile(fd: JFileDescriptor); cdecl; overload;
  procedure setOutputFile(path: JString); cdecl; overload;
  procedure setOutputFormat(output_format: Integer); cdecl;
  procedure setPreviewDisplay(sv: JSurface); cdecl;
  procedure setProfile(profile: JCamcorderProfile); cdecl;
  procedure setVideoEncoder(video_encoder: Integer); cdecl;
  procedure setVideoEncodingBitRate(bitRate: Integer); cdecl;
  procedure setVideoFrameRate(rate: Integer); cdecl;
  procedure setVideoSize(width: Integer; height: Integer); cdecl;
  procedure setVideoSource(video_source: Integer); cdecl;
  procedure start; cdecl;
  procedure stop; cdecl;
end;
TJMediaRecorder = class(TJavaGenericImport<JMediaRecorderClass, JMediaRecorder>) end;

JMediaPlayer_OnErrorListenerClass = interface(IJavaClass)
['{35FC0354-8C2B-4B4B-ABEC-DEBD6B40E6F7}']
end;

[JavaSignature('android/media/MediaPlayer$OnErrorListener')]
JMediaPlayer_OnErrorListener = interface(IJavaInstance)
['{164290D2-CB7D-41A2-94DC-20306173FB5E}']
  {Methods}
  function onError(mp: JMediaPlayer; what: Integer; extra: Integer): Boolean; cdecl;
end;
TJMediaPlayer_OnErrorListener = class(TJavaGenericImport<JMediaPlayer_OnErrorListenerClass, JMediaPlayer_OnErrorListener>) end;

JJetPlayerClass = interface(JObjectClass)
['{2CA09255-297D-4CAE-A504-4B4D00375EC3}']
  {Methods}
  function getJetPlayer: JJetPlayer; cdecl;
  function getMaxTracks: Integer; cdecl;
end;

[JavaSignature('android/media/JetPlayer')]
JJetPlayer = interface(JObject)
['{3E8DCD57-E0F8-4526-BFA1-62121FB1B4C2}']
  {Methods}
  function clearQueue: Boolean; cdecl;
  function clone: JObject; cdecl;
  function closeJetFile: Boolean; cdecl;
  function loadJetFile(path: JString): Boolean; cdecl; overload;
  function loadJetFile(afd: JAssetFileDescriptor): Boolean; cdecl; overload;
  function pause: Boolean; cdecl;
  function play: Boolean; cdecl;
  function queueJetSegment(segmentNum: Integer; libNum: Integer; repeatCount: Integer; transpose: Integer; muteFlags: Integer; userID: Byte): Boolean; cdecl;
  function queueJetSegmentMuteArray(segmentNum: Integer; libNum: Integer; repeatCount: Integer; transpose: Integer; muteArray: TJavaArray<Boolean>; userID: Byte): Boolean; cdecl;
  procedure release; cdecl;
  procedure setEventListener(listener: JJetPlayer_OnJetEventListener); cdecl; overload;
  procedure setEventListener(listener: JJetPlayer_OnJetEventListener; handler: JHandler); cdecl; overload;
  function setMuteArray(muteArray: TJavaArray<Boolean>; sync: Boolean): Boolean; cdecl;
  function setMuteFlag(trackId: Integer; muteFlag: Boolean; sync: Boolean): Boolean; cdecl;
  function setMuteFlags(muteFlags: Integer; sync: Boolean): Boolean; cdecl;
  function triggerClip(clipId: Integer): Boolean; cdecl;
end;
TJJetPlayer = class(TJavaGenericImport<JJetPlayerClass, JJetPlayer>) end;

JAudioManager_OnAudioFocusChangeListenerClass = interface(IJavaClass)
['{D79C0846-0031-48D5-9EB0-A995A3D034A2}']
end;

[JavaSignature('android/media/AudioManager$OnAudioFocusChangeListener')]
JAudioManager_OnAudioFocusChangeListener = interface(IJavaInstance)
['{F6FE80F4-5596-4E41-B718-BFEEEDBFAE47}']
  {Methods}
  procedure onAudioFocusChange(focusChange: Integer); cdecl;
end;
TJAudioManager_OnAudioFocusChangeListener = class(TJavaGenericImport<JAudioManager_OnAudioFocusChangeListenerClass, JAudioManager_OnAudioFocusChangeListener>) end;

JMediaPlayer_OnBufferingUpdateListenerClass = interface(IJavaClass)
['{9690972A-5827-477B-A01D-4285196FB577}']
end;

[JavaSignature('android/media/MediaPlayer$OnBufferingUpdateListener')]
JMediaPlayer_OnBufferingUpdateListener = interface(IJavaInstance)
['{BA8BF9D7-325E-4A93-B58F-81B180199D14}']
  {Methods}
  procedure onBufferingUpdate(mp: JMediaPlayer; percent: Integer); cdecl;
end;
TJMediaPlayer_OnBufferingUpdateListener = class(TJavaGenericImport<JMediaPlayer_OnBufferingUpdateListenerClass, JMediaPlayer_OnBufferingUpdateListener>) end;

JTimedTextClass = interface(JObjectClass)
['{1C368248-EC42-4D7E-9CBF-4C9910FD9C67}']
end;

[JavaSignature('android/media/TimedText')]
JTimedText = interface(JObject)
['{9240B2F5-1767-4D67-9C9D-780021A81BAE}']
  {Methods}
  function getBounds: JRect; cdecl;
  function getText: JString; cdecl;
end;
TJTimedText = class(TJavaGenericImport<JTimedTextClass, JTimedText>) end;

JEqualizer_SettingsClass = interface(JObjectClass)
['{23304420-D186-451A-B6DC-5846218AA62D}']
  {Methods}
  function init: JEqualizer_Settings; cdecl; overload;
  function init(settings: JString): JEqualizer_Settings; cdecl; overload;
end;

[JavaSignature('android/media/audiofx/Equalizer$Settings')]
JEqualizer_Settings = interface(JObject)
['{F4F67A0D-3F81-4AE7-80BE-21154E40D5A1}']
  {Property Methods}
  function _GetbandLevels: TJavaArray<SmallInt>;
  procedure _SetbandLevels(Value: TJavaArray<SmallInt>);
  function _GetcurPreset: SmallInt;
  procedure _SetcurPreset(Value: SmallInt);
  function _GetnumBands: SmallInt;
  procedure _SetnumBands(Value: SmallInt);
  {Methods}
  function toString: JString; cdecl;
  {Properties}
  property bandLevels: TJavaArray<SmallInt> read _GetbandLevels write _SetbandLevels;
  property curPreset: SmallInt read _GetcurPreset write _SetcurPreset;
  property numBands: SmallInt read _GetnumBands write _SetnumBands;
end;
TJEqualizer_Settings = class(TJavaGenericImport<JEqualizer_SettingsClass, JEqualizer_Settings>) end;

JMediaRouter_UserRouteInfoClass = interface(JMediaRouter_RouteInfoClass)
['{E9430B96-BDA5-4EEA-BC9B-AB818A32EF7A}']
end;

[JavaSignature('android/media/MediaRouter$UserRouteInfo')]
JMediaRouter_UserRouteInfo = interface(JMediaRouter_RouteInfo)
['{A7AD5F99-15D8-439D-9A45-2E6E013F6FA0}']
  {Methods}
  function getRemoteControlClient: JRemoteControlClient; cdecl;
  procedure requestSetVolume(volume: Integer); cdecl;
  procedure requestUpdateVolume(direction: Integer); cdecl;
  procedure setIconDrawable(icon: JDrawable); cdecl;
  procedure setIconResource(resId: Integer); cdecl;
  procedure setName(name: JCharSequence); cdecl; overload;
  procedure setName(resId: Integer); cdecl; overload;
  procedure setPlaybackStream(stream: Integer); cdecl;
  procedure setPlaybackType(type_: Integer); cdecl;
  procedure setRemoteControlClient(rcc: JRemoteControlClient); cdecl;
  procedure setStatus(status: JCharSequence); cdecl;
  procedure setVolume(volume: Integer); cdecl;
  procedure setVolumeCallback(vcb: JMediaRouter_VolumeCallback); cdecl;
  procedure setVolumeHandling(volumeHandling: Integer); cdecl;
  procedure setVolumeMax(volumeMax: Integer); cdecl;
end;
TJMediaRouter_UserRouteInfo = class(TJavaGenericImport<JMediaRouter_UserRouteInfoClass, JMediaRouter_UserRouteInfo>) end;

JPresetReverbClass = interface(JAudioEffectClass)
['{E6BDD7E1-087C-4BEF-BBCC-001619236297}']
  {Property Methods}
  function _GetPARAM_PRESET: Integer;
  function _GetPRESET_LARGEHALL: SmallInt;
  function _GetPRESET_LARGEROOM: SmallInt;
  function _GetPRESET_MEDIUMHALL: SmallInt;
  function _GetPRESET_MEDIUMROOM: SmallInt;
  function _GetPRESET_NONE: SmallInt;
  function _GetPRESET_PLATE: SmallInt;
  function _GetPRESET_SMALLROOM: SmallInt;
  {Methods}
  function init(priority: Integer; audioSession: Integer): JPresetReverb; cdecl;
  {Properties}
  property PARAM_PRESET: Integer read _GetPARAM_PRESET;
  property PRESET_LARGEHALL: SmallInt read _GetPRESET_LARGEHALL;
  property PRESET_LARGEROOM: SmallInt read _GetPRESET_LARGEROOM;
  property PRESET_MEDIUMHALL: SmallInt read _GetPRESET_MEDIUMHALL;
  property PRESET_MEDIUMROOM: SmallInt read _GetPRESET_MEDIUMROOM;
  property PRESET_NONE: SmallInt read _GetPRESET_NONE;
  property PRESET_PLATE: SmallInt read _GetPRESET_PLATE;
  property PRESET_SMALLROOM: SmallInt read _GetPRESET_SMALLROOM;
end;

[JavaSignature('android/media/audiofx/PresetReverb')]
JPresetReverb = interface(JAudioEffect)
['{86BA2002-A4F4-4B7F-8EB0-9DE5F08D9893}']
  {Methods}
  function getPreset: SmallInt; cdecl;
  function getProperties: JPresetReverb_Settings; cdecl;
  procedure setParameterListener(listener: JPresetReverb_OnParameterChangeListener); cdecl;
  procedure setPreset(preset: SmallInt); cdecl;
  procedure setProperties(settings: JPresetReverb_Settings); cdecl;
end;
TJPresetReverb = class(TJavaGenericImport<JPresetReverbClass, JPresetReverb>) end;

JBassBoost_OnParameterChangeListenerClass = interface(IJavaClass)
['{BFF4F1F8-5F21-47A9-BBB5-3EEE6066B689}']
end;

[JavaSignature('android/media/audiofx/BassBoost$OnParameterChangeListener')]
JBassBoost_OnParameterChangeListener = interface(IJavaInstance)
['{CF34C515-408C-4ED9-8D9A-C1F73EA0CDAB}']
  {Methods}
  procedure onParameterChange(effect: JBassBoost; status: Integer; param: Integer; value: SmallInt); cdecl;
end;
TJBassBoost_OnParameterChangeListener = class(TJavaGenericImport<JBassBoost_OnParameterChangeListenerClass, JBassBoost_OnParameterChangeListener>) end;

JMediaPlayer_OnInfoListenerClass = interface(IJavaClass)
['{7141FE3D-8201-49F6-919F-FF71788DFFED}']
end;

[JavaSignature('android/media/MediaPlayer$OnInfoListener')]
JMediaPlayer_OnInfoListener = interface(IJavaInstance)
['{84CB6D98-5AC9-4787-8411-E5F7A7CAFA84}']
  {Methods}
  function onInfo(mp: JMediaPlayer; what: Integer; extra: Integer): Boolean; cdecl;
end;
TJMediaPlayer_OnInfoListener = class(TJavaGenericImport<JMediaPlayer_OnInfoListenerClass, JMediaPlayer_OnInfoListener>) end;

JMediaCodecInfoClass = interface(JObjectClass)
['{65843676-C663-48B7-B957-36E0FAA8C6DE}']
end;

[JavaSignature('android/media/MediaCodecInfo')]
JMediaCodecInfo = interface(JObject)
['{DEB2A66A-5E90-4568-8DD2-5D17A9008896}']
  {Methods}
  function getCapabilitiesForType(type_: JString): JMediaCodecInfo_CodecCapabilities; cdecl;
  function getName: JString; cdecl;
  function getSupportedTypes: TJavaObjectArray<JString>; cdecl;
  function isEncoder: Boolean; cdecl;
end;
TJMediaCodecInfo = class(TJavaGenericImport<JMediaCodecInfoClass, JMediaCodecInfo>) end;

JMediaPlayer_OnTimedTextListenerClass = interface(IJavaClass)
['{B1D8CCB6-43D0-4238-85F7-E9A91DA43744}']
end;

[JavaSignature('android/media/MediaPlayer$OnTimedTextListener')]
JMediaPlayer_OnTimedTextListener = interface(IJavaInstance)
['{0884B998-F061-44F4-BAB4-A31DAF22F162}']
  {Methods}
  procedure onTimedText(mp: JMediaPlayer; text: JTimedText); cdecl;
end;
TJMediaPlayer_OnTimedTextListener = class(TJavaGenericImport<JMediaPlayer_OnTimedTextListenerClass, JMediaPlayer_OnTimedTextListener>) end;

JMediaRouter_RouteCategoryClass = interface(JObjectClass)
['{CB0B2500-99F0-4F30-AC54-0BED238E7560}']
end;

[JavaSignature('android/media/MediaRouter$RouteCategory')]
JMediaRouter_RouteCategory = interface(JObject)
['{C2544513-A0C0-4452-8046-95530994926B}']
  {Methods}
  function getName: JCharSequence; cdecl; overload;
  function getName(context: JContext): JCharSequence; cdecl; overload;
  function getRoutes(out_: JList): JList; cdecl;
  function getSupportedTypes: Integer; cdecl;
  function isGroupable: Boolean; cdecl;
  function toString: JString; cdecl;
end;
TJMediaRouter_RouteCategory = class(TJavaGenericImport<JMediaRouter_RouteCategoryClass, JMediaRouter_RouteCategory>) end;

JMediaRecorder_OutputFormatClass = interface(JObjectClass)
['{E5FB95FA-4387-4454-9464-8F93A2FB550A}']
  {Property Methods}
  function _GetAAC_ADTS: Integer;
  function _GetAMR_NB: Integer;
  function _GetAMR_WB: Integer;
  function _GetDEFAULT: Integer;
  function _GetMPEG_4: Integer;
  function _GetRAW_AMR: Integer;
  function _GetTHREE_GPP: Integer;
  {Properties}
  property AAC_ADTS: Integer read _GetAAC_ADTS;
  property AMR_NB: Integer read _GetAMR_NB;
  property AMR_WB: Integer read _GetAMR_WB;
  property DEFAULT: Integer read _GetDEFAULT;
  property MPEG_4: Integer read _GetMPEG_4;
  property RAW_AMR: Integer read _GetRAW_AMR;
  property THREE_GPP: Integer read _GetTHREE_GPP;
end;

[JavaSignature('android/media/MediaRecorder$OutputFormat')]
JMediaRecorder_OutputFormat = interface(JObject)
['{3988D375-2062-46A4-8DA6-145374E051D9}']
end;
TJMediaRecorder_OutputFormat = class(TJavaGenericImport<JMediaRecorder_OutputFormatClass, JMediaRecorder_OutputFormat>) end;

JMediaPlayerClass = interface(JObjectClass)
['{5C9CABE2-E9F5-4990-8E18-6D0BB6C469FB}']
  {Property Methods}
  function _GetMEDIA_ERROR_IO: Integer;
  function _GetMEDIA_ERROR_MALFORMED: Integer;
  function _GetMEDIA_ERROR_NOT_VALID_FOR_PROGRESSIVE_PLAYBACK: Integer;
  function _GetMEDIA_ERROR_SERVER_DIED: Integer;
  function _GetMEDIA_ERROR_TIMED_OUT: Integer;
  function _GetMEDIA_ERROR_UNKNOWN: Integer;
  function _GetMEDIA_ERROR_UNSUPPORTED: Integer;
  function _GetMEDIA_INFO_BAD_INTERLEAVING: Integer;
  function _GetMEDIA_INFO_BUFFERING_END: Integer;
  function _GetMEDIA_INFO_BUFFERING_START: Integer;
  function _GetMEDIA_INFO_METADATA_UPDATE: Integer;
  function _GetMEDIA_INFO_NOT_SEEKABLE: Integer;
  function _GetMEDIA_INFO_UNKNOWN: Integer;
  function _GetMEDIA_INFO_VIDEO_RENDERING_START: Integer;
  function _GetMEDIA_INFO_VIDEO_TRACK_LAGGING: Integer;
  function _GetMEDIA_MIMETYPE_TEXT_SUBRIP: JString;
  function _GetVIDEO_SCALING_MODE_SCALE_TO_FIT: Integer;
  function _GetVIDEO_SCALING_MODE_SCALE_TO_FIT_WITH_CROPPING: Integer;
  {Methods}
  function init: JMediaPlayer; cdecl;
  function create(context: JContext; uri: Jnet_Uri): JMediaPlayer; cdecl; overload;
  function create(context: JContext; uri: Jnet_Uri; holder: JSurfaceHolder): JMediaPlayer; cdecl; overload;
  function create(context: JContext; resid: Integer): JMediaPlayer; cdecl; overload;
  {Properties}
  property MEDIA_ERROR_IO: Integer read _GetMEDIA_ERROR_IO;
  property MEDIA_ERROR_MALFORMED: Integer read _GetMEDIA_ERROR_MALFORMED;
  property MEDIA_ERROR_NOT_VALID_FOR_PROGRESSIVE_PLAYBACK: Integer read _GetMEDIA_ERROR_NOT_VALID_FOR_PROGRESSIVE_PLAYBACK;
  property MEDIA_ERROR_SERVER_DIED: Integer read _GetMEDIA_ERROR_SERVER_DIED;
  property MEDIA_ERROR_TIMED_OUT: Integer read _GetMEDIA_ERROR_TIMED_OUT;
  property MEDIA_ERROR_UNKNOWN: Integer read _GetMEDIA_ERROR_UNKNOWN;
  property MEDIA_ERROR_UNSUPPORTED: Integer read _GetMEDIA_ERROR_UNSUPPORTED;
  property MEDIA_INFO_BAD_INTERLEAVING: Integer read _GetMEDIA_INFO_BAD_INTERLEAVING;
  property MEDIA_INFO_BUFFERING_END: Integer read _GetMEDIA_INFO_BUFFERING_END;
  property MEDIA_INFO_BUFFERING_START: Integer read _GetMEDIA_INFO_BUFFERING_START;
  property MEDIA_INFO_METADATA_UPDATE: Integer read _GetMEDIA_INFO_METADATA_UPDATE;
  property MEDIA_INFO_NOT_SEEKABLE: Integer read _GetMEDIA_INFO_NOT_SEEKABLE;
  property MEDIA_INFO_UNKNOWN: Integer read _GetMEDIA_INFO_UNKNOWN;
  property MEDIA_INFO_VIDEO_RENDERING_START: Integer read _GetMEDIA_INFO_VIDEO_RENDERING_START;
  property MEDIA_INFO_VIDEO_TRACK_LAGGING: Integer read _GetMEDIA_INFO_VIDEO_TRACK_LAGGING;
  property MEDIA_MIMETYPE_TEXT_SUBRIP: JString read _GetMEDIA_MIMETYPE_TEXT_SUBRIP;
  property VIDEO_SCALING_MODE_SCALE_TO_FIT: Integer read _GetVIDEO_SCALING_MODE_SCALE_TO_FIT;
  property VIDEO_SCALING_MODE_SCALE_TO_FIT_WITH_CROPPING: Integer read _GetVIDEO_SCALING_MODE_SCALE_TO_FIT_WITH_CROPPING;
end;

[JavaSignature('android/media/MediaPlayer')]
JMediaPlayer = interface(JObject)
['{BC844B6F-92C1-4D1D-93C5-E9B18351F502}']
  {Methods}
  procedure addTimedTextSource(path: JString; mimeType: JString); cdecl; overload;
  procedure addTimedTextSource(context: JContext; uri: Jnet_Uri; mimeType: JString); cdecl; overload;
  procedure addTimedTextSource(fd: JFileDescriptor; mimeType: JString); cdecl; overload;
  procedure addTimedTextSource(fd: JFileDescriptor; offset: Int64; length: Int64; mimeType: JString); cdecl; overload;
  procedure attachAuxEffect(effectId: Integer); cdecl;
  procedure deselectTrack(index: Integer); cdecl;
  function getAudioSessionId: Integer; cdecl;
  function getCurrentPosition: Integer; cdecl;
  function getDuration: Integer; cdecl;
  function getTrackInfo: TJavaObjectArray<JMediaPlayer_TrackInfo>; cdecl;
  function getVideoHeight: Integer; cdecl;
  function getVideoWidth: Integer; cdecl;
  function isLooping: Boolean; cdecl;
  function isPlaying: Boolean; cdecl;
  procedure pause; cdecl;
  procedure prepare; cdecl;
  procedure prepareAsync; cdecl;
  procedure release; cdecl;
  procedure reset; cdecl;
  procedure seekTo(msec: Integer); cdecl;
  procedure selectTrack(index: Integer); cdecl;
  procedure setAudioSessionId(sessionId: Integer); cdecl;
  procedure setAudioStreamType(streamtype: Integer); cdecl;
  procedure setAuxEffectSendLevel(level: Single); cdecl;
  procedure setDataSource(context: JContext; uri: Jnet_Uri); cdecl; overload;
  procedure setDataSource(context: JContext; uri: Jnet_Uri; headers: JMap); cdecl; overload;
  procedure setDataSource(path: JString); cdecl; overload;
  procedure setDataSource(fd: JFileDescriptor); cdecl; overload;
  procedure setDataSource(fd: JFileDescriptor; offset: Int64; length: Int64); cdecl; overload;
  procedure setDisplay(sh: JSurfaceHolder); cdecl;
  procedure setLooping(looping: Boolean); cdecl;
  procedure setNextMediaPlayer(next: JMediaPlayer); cdecl;
  procedure setOnBufferingUpdateListener(listener: JMediaPlayer_OnBufferingUpdateListener); cdecl;
  procedure setOnCompletionListener(listener: JMediaPlayer_OnCompletionListener); cdecl;
  procedure setOnErrorListener(listener: JMediaPlayer_OnErrorListener); cdecl;
  procedure setOnInfoListener(listener: JMediaPlayer_OnInfoListener); cdecl;
  procedure setOnPreparedListener(listener: JMediaPlayer_OnPreparedListener); cdecl;
  procedure setOnSeekCompleteListener(listener: JMediaPlayer_OnSeekCompleteListener); cdecl;
  procedure setOnTimedTextListener(listener: JMediaPlayer_OnTimedTextListener); cdecl;
  procedure setOnVideoSizeChangedListener(listener: JMediaPlayer_OnVideoSizeChangedListener); cdecl;
  procedure setScreenOnWhilePlaying(screenOn: Boolean); cdecl;
  procedure setSurface(surface: JSurface); cdecl;
  procedure setVideoScalingMode(mode: Integer); cdecl;
  procedure setVolume(leftVolume: Single; rightVolume: Single); cdecl;
  procedure setWakeMode(context: JContext; mode: Integer); cdecl;
  procedure start; cdecl;
  procedure stop; cdecl;
end;
TJMediaPlayer = class(TJavaGenericImport<JMediaPlayerClass, JMediaPlayer>) end;

JAcousticEchoCancelerClass = interface(JAudioEffectClass)
['{460A7A72-477C-497E-8D78-1BC0E9A71276}']
  {Methods}
  function create(audioSession: Integer): JAcousticEchoCanceler; cdecl;
  function isAvailable: Boolean; cdecl;
end;

[JavaSignature('android/media/audiofx/AcousticEchoCanceler')]
JAcousticEchoCanceler = interface(JAudioEffect)
['{DD17BEFF-C337-4979-A5DA-7A5B51C8BED5}']
end;
TJAcousticEchoCanceler = class(TJavaGenericImport<JAcousticEchoCancelerClass, JAcousticEchoCanceler>) end;

JCameraProfileClass = interface(JObjectClass)
['{60E4B807-EC20-448E-86E5-DC8FE661FCEF}']
  {Property Methods}
  function _GetQUALITY_HIGH: Integer;
  function _GetQUALITY_LOW: Integer;
  function _GetQUALITY_MEDIUM: Integer;
  {Methods}
  function init: JCameraProfile; cdecl;
  function getJpegEncodingQualityParameter(quality: Integer): Integer; cdecl; overload;
  function getJpegEncodingQualityParameter(cameraId: Integer; quality: Integer): Integer; cdecl; overload;
  {Properties}
  property QUALITY_HIGH: Integer read _GetQUALITY_HIGH;
  property QUALITY_LOW: Integer read _GetQUALITY_LOW;
  property QUALITY_MEDIUM: Integer read _GetQUALITY_MEDIUM;
end;

[JavaSignature('android/media/CameraProfile')]
JCameraProfile = interface(JObject)
['{D149134A-7FEF-4216-B46B-F7E2E2F3C919}']
end;
TJCameraProfile = class(TJavaGenericImport<JCameraProfileClass, JCameraProfile>) end;

JMediaCodec_CryptoExceptionClass = interface(JRuntimeExceptionClass)
['{CE90A025-BE54-4F7A-95ED-4FEA792EA200}']
  {Methods}
  function init(errorCode: Integer; detailMessage: JString): JMediaCodec_CryptoException; cdecl;
end;

[JavaSignature('android/media/MediaCodec$CryptoException')]
JMediaCodec_CryptoException = interface(JRuntimeException)
['{D0A8F499-E687-4578-9E2D-A86E14881DB5}']
  {Methods}
  function getErrorCode: Integer; cdecl;
end;
TJMediaCodec_CryptoException = class(TJavaGenericImport<JMediaCodec_CryptoExceptionClass, JMediaCodec_CryptoException>) end;

JEffectClass = interface(JObjectClass)
['{DC62EAE5-6D58-4DEC-B60B-CE5890CC3CDF}']
  {Methods}
  function init: JEffect; cdecl;
end;

[JavaSignature('android/media/effect/Effect')]
JEffect = interface(JObject)
['{B0029DD4-ED95-41AD-9F14-3E81375B7DC6}']
  {Methods}
  procedure apply(inputTexId: Integer; width: Integer; height: Integer; outputTexId: Integer); cdecl;
  function getName: JString; cdecl;
  procedure release; cdecl;
  procedure setParameter(parameterKey: JString; value: JObject); cdecl;
  procedure setUpdateListener(listener: JEffectUpdateListener); cdecl;
end;
TJEffect = class(TJavaGenericImport<JEffectClass, JEffect>) end;

JMediaPlayer_OnCompletionListenerClass = interface(IJavaClass)
['{B52C8D4D-90D6-4A31-A123-590582DCA314}']
end;

[JavaSignature('android/media/MediaPlayer$OnCompletionListener')]
JMediaPlayer_OnCompletionListener = interface(IJavaInstance)
['{855040E1-8E41-40EE-B36F-06C212B8AC81}']
  {Methods}
  procedure onCompletion(mp: JMediaPlayer); cdecl;
end;
TJMediaPlayer_OnCompletionListener = class(TJavaGenericImport<JMediaPlayer_OnCompletionListenerClass, JMediaPlayer_OnCompletionListener>) end;




implementation

begin

end.


