{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit Androidapi.JNI.Hardware;

interface

uses 
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.GraphicsContentViewText;

type

  {Class forward declarations}
  JCamera_Face = interface;//android.hardware.Camera$Face
  JCamera_ErrorCallback = interface;//android.hardware.Camera$ErrorCallback
  JCamera_CameraInfo = interface;//android.hardware.Camera$CameraInfo
  JCamera_Size = interface;//android.hardware.Camera$Size
  JCamera_FaceDetectionListener = interface;//android.hardware.Camera$FaceDetectionListener
  JCamera_ShutterCallback = interface;//android.hardware.Camera$ShutterCallback
  JCamera_PreviewCallback = interface;//android.hardware.Camera$PreviewCallback
  JCamera = interface;//android.hardware.Camera
  JCamera_Parameters = interface;//android.hardware.Camera$Parameters
  JCamera_AutoFocusMoveCallback = interface;//android.hardware.Camera$AutoFocusMoveCallback
  JCamera_AutoFocusCallback = interface;//android.hardware.Camera$AutoFocusCallback
  JCamera_PictureCallback = interface;//android.hardware.Camera$PictureCallback
  JCamera_OnZoomChangeListener = interface;//android.hardware.Camera$OnZoomChangeListener

JCamera_FaceClass = interface(JObjectClass)
['{3926BFB1-9866-403B-A24E-7FCB9251F1A5}']
  {Methods}
  function init: JCamera_Face; cdecl;
end;

[JavaSignature('android/hardware/Camera$Face')]
JCamera_Face = interface(JObject)
['{4FBCBB16-3A6B-493C-952F-55AC60C75DE9}']
  {Property Methods}
  function _Getid: Integer;
  procedure _Setid(Value: Integer);
  function _GetleftEye: JPoint;
  procedure _SetleftEye(Value: JPoint);
  function _Getmouth: JPoint;
  procedure _Setmouth(Value: JPoint);
  function _Getrect: JRect;
  procedure _Setrect(Value: JRect);
  function _GetrightEye: JPoint;
  procedure _SetrightEye(Value: JPoint);
  function _Getscore: Integer;
  procedure _Setscore(Value: Integer);
  {Properties}
  property id: Integer read _Getid write _Setid;
  property leftEye: JPoint read _GetleftEye write _SetleftEye;
  property mouth: JPoint read _Getmouth write _Setmouth;
  property rect: JRect read _Getrect write _Setrect;
  property rightEye: JPoint read _GetrightEye write _SetrightEye;
  property score: Integer read _Getscore write _Setscore;
end;
TJCamera_Face = class(TJavaGenericImport<JCamera_FaceClass, JCamera_Face>) end;

JCamera_ErrorCallbackClass = interface(IJavaClass)
['{5AAEF0D6-153A-481B-8A54-527D810352D2}']
end;

[JavaSignature('android/hardware/Camera$ErrorCallback')]
JCamera_ErrorCallback = interface(IJavaInstance)
['{45CC94C4-4AEC-43B6-861D-A7AC2392CB2D}']
  {Methods}
  procedure onError(error: Integer; camera: JCamera); cdecl;
end;
TJCamera_ErrorCallback = class(TJavaGenericImport<JCamera_ErrorCallbackClass, JCamera_ErrorCallback>) end;

JCamera_CameraInfoClass = interface(JObjectClass)
['{4D9A2405-77B3-4828-B7F5-AF756B98D6B5}']
  {Property Methods}
  function _GetCAMERA_FACING_BACK: Integer;
  function _GetCAMERA_FACING_FRONT: Integer;
  {Methods}
  function init: JCamera_CameraInfo; cdecl;
  {Properties}
  property CAMERA_FACING_BACK: Integer read _GetCAMERA_FACING_BACK;
  property CAMERA_FACING_FRONT: Integer read _GetCAMERA_FACING_FRONT;
end;

[JavaSignature('android/hardware/Camera$CameraInfo')]
JCamera_CameraInfo = interface(JObject)
['{D7A9C455-C629-40F6-BCAE-E20195C6069B}']
  {Property Methods}
  function _GetcanDisableShutterSound: Boolean;
  procedure _SetcanDisableShutterSound(Value: Boolean);
  function _Getfacing: Integer;
  procedure _Setfacing(Value: Integer);
  function _Getorientation: Integer;
  procedure _Setorientation(Value: Integer);
  {Properties}
  property canDisableShutterSound: Boolean read _GetcanDisableShutterSound write _SetcanDisableShutterSound;
  property facing: Integer read _Getfacing write _Setfacing;
  property orientation: Integer read _Getorientation write _Setorientation;
end;
TJCamera_CameraInfo = class(TJavaGenericImport<JCamera_CameraInfoClass, JCamera_CameraInfo>) end;

JCamera_SizeClass = interface(JObjectClass)
['{23FA6E4F-E2F1-4FDF-9892-A56C71EEA6D4}']
  {Methods}
  function init(w: Integer; h: Integer): JCamera_Size; cdecl;
end;

[JavaSignature('android/hardware/Camera$Size')]
JCamera_Size = interface(JObject)
['{2D2A15A6-C3ED-4B61-8276-214F497A766A}']
  {Property Methods}
  function _Getheight: Integer;
  procedure _Setheight(Value: Integer);
  function _Getwidth: Integer;
  procedure _Setwidth(Value: Integer);
  {Methods}
  function equals(obj: JObject): Boolean; cdecl;
  function hashCode: Integer; cdecl;
  {Properties}
  property height: Integer read _Getheight write _Setheight;
  property width: Integer read _Getwidth write _Setwidth;
end;
TJCamera_Size = class(TJavaGenericImport<JCamera_SizeClass, JCamera_Size>) end;

JCamera_FaceDetectionListenerClass = interface(IJavaClass)
['{2C26C033-6093-440C-8B6F-E0E8632CE495}']
end;

[JavaSignature('android/hardware/Camera$FaceDetectionListener')]
JCamera_FaceDetectionListener = interface(IJavaInstance)
['{F5A3CD35-3B25-41A8-9B69-6EC00C39A0BF}']
  {Methods}
  procedure onFaceDetection(faces: TJavaObjectArray<JCamera_Face>; camera: JCamera); cdecl;
end;
TJCamera_FaceDetectionListener = class(TJavaGenericImport<JCamera_FaceDetectionListenerClass, JCamera_FaceDetectionListener>) end;

JCamera_ShutterCallbackClass = interface(IJavaClass)
['{D658141F-9627-4E2E-8B23-CDF1814F02FB}']
end;

[JavaSignature('android/hardware/Camera$ShutterCallback')]
JCamera_ShutterCallback = interface(IJavaInstance)
['{50F23354-86CD-4B59-9CC8-E647BDF98EC2}']
  {Methods}
  procedure onShutter; cdecl;
end;
TJCamera_ShutterCallback = class(TJavaGenericImport<JCamera_ShutterCallbackClass, JCamera_ShutterCallback>) end;

JCamera_PreviewCallbackClass = interface(IJavaClass)
['{C6836D36-1914-4DB8-8458-D6AEC71A7257}']
end;

[JavaSignature('android/hardware/Camera$PreviewCallback')]
JCamera_PreviewCallback = interface(IJavaInstance)
['{6F2F0374-DCFF-43EC-B8BF-DB2F72574EBB}']
  {Methods}
  procedure onPreviewFrame(data: TJavaArray<Byte>; camera: JCamera); cdecl;
end;
TJCamera_PreviewCallback = class(TJavaGenericImport<JCamera_PreviewCallbackClass, JCamera_PreviewCallback>) end;

JCameraClass = interface(JObjectClass)
['{EC7FA230-96BA-4ED6-9328-CAC5F459C235}']
  {Property Methods}
  function _GetACTION_NEW_PICTURE: JString;
  function _GetACTION_NEW_VIDEO: JString;
  function _GetCAMERA_ERROR_SERVER_DIED: Integer;
  function _GetCAMERA_ERROR_UNKNOWN: Integer;
  {Methods}
  procedure getCameraInfo(cameraId: Integer; cameraInfo: JCamera_CameraInfo); cdecl;
  function getNumberOfCameras: Integer; cdecl;
  function open(cameraId: Integer): JCamera; cdecl; overload;
  function open: JCamera; cdecl; overload;
  {Properties}
  property ACTION_NEW_PICTURE: JString read _GetACTION_NEW_PICTURE;
  property ACTION_NEW_VIDEO: JString read _GetACTION_NEW_VIDEO;
  property CAMERA_ERROR_SERVER_DIED: Integer read _GetCAMERA_ERROR_SERVER_DIED;
  property CAMERA_ERROR_UNKNOWN: Integer read _GetCAMERA_ERROR_UNKNOWN;
end;

[JavaSignature('android/hardware/Camera')]
JCamera = interface(JObject)
['{40A86A47-3393-4E33-8884-C33107CD903B}']
  {Methods}
  procedure addCallbackBuffer(callbackBuffer: TJavaArray<Byte>); cdecl;
  procedure autoFocus(cb: JCamera_AutoFocusCallback); cdecl;
  procedure cancelAutoFocus; cdecl;
  function enableShutterSound(enabled: Boolean): Boolean; cdecl;
  function getParameters: JCamera_Parameters; cdecl;
  procedure lock; cdecl;
  procedure reconnect; cdecl;
  procedure release; cdecl;
  procedure setAutoFocusMoveCallback(cb: JCamera_AutoFocusMoveCallback); cdecl;
  procedure setDisplayOrientation(degrees: Integer); cdecl;
  procedure setErrorCallback(cb: JCamera_ErrorCallback); cdecl;
  procedure setFaceDetectionListener(listener: JCamera_FaceDetectionListener); cdecl;
  procedure setOneShotPreviewCallback(cb: JCamera_PreviewCallback); cdecl;
  procedure setParameters(params: JCamera_Parameters); cdecl;
  procedure setPreviewCallback(cb: JCamera_PreviewCallback); cdecl;
  procedure setPreviewCallbackWithBuffer(cb: JCamera_PreviewCallback); cdecl;
  procedure setPreviewDisplay(holder: JSurfaceHolder); cdecl;
  procedure setPreviewTexture(surfaceTexture: JSurfaceTexture); cdecl;
  procedure setZoomChangeListener(listener: JCamera_OnZoomChangeListener); cdecl;
  procedure startFaceDetection; cdecl;
  procedure startPreview; cdecl;
  procedure startSmoothZoom(value: Integer); cdecl;
  procedure stopFaceDetection; cdecl;
  procedure stopPreview; cdecl;
  procedure stopSmoothZoom; cdecl;
  procedure takePicture(shutter: JCamera_ShutterCallback; raw: JCamera_PictureCallback; jpeg: JCamera_PictureCallback); cdecl; overload;
  procedure takePicture(shutter: JCamera_ShutterCallback; raw: JCamera_PictureCallback; postview: JCamera_PictureCallback; jpeg: JCamera_PictureCallback); cdecl; overload;
  procedure unlock; cdecl;
end;
TJCamera = class(TJavaGenericImport<JCameraClass, JCamera>) end;

JCamera_ParametersClass = interface(JObjectClass)
['{519157BE-F3CB-41ED-90A0-239A67F07E7C}']
  {Property Methods}
  function _GetANTIBANDING_50HZ: JString;
  function _GetANTIBANDING_60HZ: JString;
  function _GetANTIBANDING_AUTO: JString;
  function _GetANTIBANDING_OFF: JString;
  function _GetEFFECT_AQUA: JString;
  function _GetEFFECT_BLACKBOARD: JString;
  function _GetEFFECT_MONO: JString;
  function _GetEFFECT_NEGATIVE: JString;
  function _GetEFFECT_NONE: JString;
  function _GetEFFECT_POSTERIZE: JString;
  function _GetEFFECT_SEPIA: JString;
  function _GetEFFECT_SOLARIZE: JString;
  function _GetEFFECT_WHITEBOARD: JString;
  function _GetFLASH_MODE_AUTO: JString;
  function _GetFLASH_MODE_OFF: JString;
  function _GetFLASH_MODE_ON: JString;
  function _GetFLASH_MODE_RED_EYE: JString;
  function _GetFLASH_MODE_TORCH: JString;
  function _GetFOCUS_DISTANCE_FAR_INDEX: Integer;
  function _GetFOCUS_DISTANCE_NEAR_INDEX: Integer;
  function _GetFOCUS_DISTANCE_OPTIMAL_INDEX: Integer;
  function _GetFOCUS_MODE_AUTO: JString;
  function _GetFOCUS_MODE_CONTINUOUS_PICTURE: JString;
  function _GetFOCUS_MODE_CONTINUOUS_VIDEO: JString;
  function _GetFOCUS_MODE_EDOF: JString;
  function _GetFOCUS_MODE_FIXED: JString;
  function _GetFOCUS_MODE_INFINITY: JString;
  function _GetFOCUS_MODE_MACRO: JString;
  function _GetPREVIEW_FPS_MAX_INDEX: Integer;
  function _GetPREVIEW_FPS_MIN_INDEX: Integer;
  function _GetSCENE_MODE_ACTION: JString;
  function _GetSCENE_MODE_AUTO: JString;
  function _GetSCENE_MODE_BARCODE: JString;
  function _GetSCENE_MODE_BEACH: JString;
  function _GetSCENE_MODE_CANDLELIGHT: JString;
  function _GetSCENE_MODE_FIREWORKS: JString;
  function _GetSCENE_MODE_HDR: JString;
  function _GetSCENE_MODE_LANDSCAPE: JString;
  function _GetSCENE_MODE_NIGHT: JString;
  function _GetSCENE_MODE_NIGHT_PORTRAIT: JString;
  function _GetSCENE_MODE_PARTY: JString;
  function _GetSCENE_MODE_PORTRAIT: JString;
  function _GetSCENE_MODE_SNOW: JString;
  function _GetSCENE_MODE_SPORTS: JString;
  function _GetSCENE_MODE_STEADYPHOTO: JString;
  function _GetSCENE_MODE_SUNSET: JString;
  function _GetSCENE_MODE_THEATRE: JString;
  function _GetWHITE_BALANCE_AUTO: JString;
  function _GetWHITE_BALANCE_CLOUDY_DAYLIGHT: JString;
  function _GetWHITE_BALANCE_DAYLIGHT: JString;
  function _GetWHITE_BALANCE_FLUORESCENT: JString;
  function _GetWHITE_BALANCE_INCANDESCENT: JString;
  function _GetWHITE_BALANCE_SHADE: JString;
  function _GetWHITE_BALANCE_TWILIGHT: JString;
  function _GetWHITE_BALANCE_WARM_FLUORESCENT: JString;
  {Properties}
  property ANTIBANDING_50HZ: JString read _GetANTIBANDING_50HZ;
  property ANTIBANDING_60HZ: JString read _GetANTIBANDING_60HZ;
  property ANTIBANDING_AUTO: JString read _GetANTIBANDING_AUTO;
  property ANTIBANDING_OFF: JString read _GetANTIBANDING_OFF;
  property EFFECT_AQUA: JString read _GetEFFECT_AQUA;
  property EFFECT_BLACKBOARD: JString read _GetEFFECT_BLACKBOARD;
  property EFFECT_MONO: JString read _GetEFFECT_MONO;
  property EFFECT_NEGATIVE: JString read _GetEFFECT_NEGATIVE;
  property EFFECT_NONE: JString read _GetEFFECT_NONE;
  property EFFECT_POSTERIZE: JString read _GetEFFECT_POSTERIZE;
  property EFFECT_SEPIA: JString read _GetEFFECT_SEPIA;
  property EFFECT_SOLARIZE: JString read _GetEFFECT_SOLARIZE;
  property EFFECT_WHITEBOARD: JString read _GetEFFECT_WHITEBOARD;
  property FLASH_MODE_AUTO: JString read _GetFLASH_MODE_AUTO;
  property FLASH_MODE_OFF: JString read _GetFLASH_MODE_OFF;
  property FLASH_MODE_ON: JString read _GetFLASH_MODE_ON;
  property FLASH_MODE_RED_EYE: JString read _GetFLASH_MODE_RED_EYE;
  property FLASH_MODE_TORCH: JString read _GetFLASH_MODE_TORCH;
  property FOCUS_DISTANCE_FAR_INDEX: Integer read _GetFOCUS_DISTANCE_FAR_INDEX;
  property FOCUS_DISTANCE_NEAR_INDEX: Integer read _GetFOCUS_DISTANCE_NEAR_INDEX;
  property FOCUS_DISTANCE_OPTIMAL_INDEX: Integer read _GetFOCUS_DISTANCE_OPTIMAL_INDEX;
  property FOCUS_MODE_AUTO: JString read _GetFOCUS_MODE_AUTO;
  property FOCUS_MODE_CONTINUOUS_PICTURE: JString read _GetFOCUS_MODE_CONTINUOUS_PICTURE;
  property FOCUS_MODE_CONTINUOUS_VIDEO: JString read _GetFOCUS_MODE_CONTINUOUS_VIDEO;
  property FOCUS_MODE_EDOF: JString read _GetFOCUS_MODE_EDOF;
  property FOCUS_MODE_FIXED: JString read _GetFOCUS_MODE_FIXED;
  property FOCUS_MODE_INFINITY: JString read _GetFOCUS_MODE_INFINITY;
  property FOCUS_MODE_MACRO: JString read _GetFOCUS_MODE_MACRO;
  property PREVIEW_FPS_MAX_INDEX: Integer read _GetPREVIEW_FPS_MAX_INDEX;
  property PREVIEW_FPS_MIN_INDEX: Integer read _GetPREVIEW_FPS_MIN_INDEX;
  property SCENE_MODE_ACTION: JString read _GetSCENE_MODE_ACTION;
  property SCENE_MODE_AUTO: JString read _GetSCENE_MODE_AUTO;
  property SCENE_MODE_BARCODE: JString read _GetSCENE_MODE_BARCODE;
  property SCENE_MODE_BEACH: JString read _GetSCENE_MODE_BEACH;
  property SCENE_MODE_CANDLELIGHT: JString read _GetSCENE_MODE_CANDLELIGHT;
  property SCENE_MODE_FIREWORKS: JString read _GetSCENE_MODE_FIREWORKS;
  property SCENE_MODE_HDR: JString read _GetSCENE_MODE_HDR;
  property SCENE_MODE_LANDSCAPE: JString read _GetSCENE_MODE_LANDSCAPE;
  property SCENE_MODE_NIGHT: JString read _GetSCENE_MODE_NIGHT;
  property SCENE_MODE_NIGHT_PORTRAIT: JString read _GetSCENE_MODE_NIGHT_PORTRAIT;
  property SCENE_MODE_PARTY: JString read _GetSCENE_MODE_PARTY;
  property SCENE_MODE_PORTRAIT: JString read _GetSCENE_MODE_PORTRAIT;
  property SCENE_MODE_SNOW: JString read _GetSCENE_MODE_SNOW;
  property SCENE_MODE_SPORTS: JString read _GetSCENE_MODE_SPORTS;
  property SCENE_MODE_STEADYPHOTO: JString read _GetSCENE_MODE_STEADYPHOTO;
  property SCENE_MODE_SUNSET: JString read _GetSCENE_MODE_SUNSET;
  property SCENE_MODE_THEATRE: JString read _GetSCENE_MODE_THEATRE;
  property WHITE_BALANCE_AUTO: JString read _GetWHITE_BALANCE_AUTO;
  property WHITE_BALANCE_CLOUDY_DAYLIGHT: JString read _GetWHITE_BALANCE_CLOUDY_DAYLIGHT;
  property WHITE_BALANCE_DAYLIGHT: JString read _GetWHITE_BALANCE_DAYLIGHT;
  property WHITE_BALANCE_FLUORESCENT: JString read _GetWHITE_BALANCE_FLUORESCENT;
  property WHITE_BALANCE_INCANDESCENT: JString read _GetWHITE_BALANCE_INCANDESCENT;
  property WHITE_BALANCE_SHADE: JString read _GetWHITE_BALANCE_SHADE;
  property WHITE_BALANCE_TWILIGHT: JString read _GetWHITE_BALANCE_TWILIGHT;
  property WHITE_BALANCE_WARM_FLUORESCENT: JString read _GetWHITE_BALANCE_WARM_FLUORESCENT;
end;

[JavaSignature('android/hardware/Camera$Parameters')]
JCamera_Parameters = interface(JObject)
['{EFDE0CD6-C9EB-4DE2-B903-A61589549842}']
  {Methods}
  function flatten: JString; cdecl;
  function &get(key: JString): JString; cdecl;
  function getAntibanding: JString; cdecl;
  function getAutoExposureLock: Boolean; cdecl;
  function getAutoWhiteBalanceLock: Boolean; cdecl;
  function getColorEffect: JString; cdecl;
  function getExposureCompensation: Integer; cdecl;
  function getExposureCompensationStep: Single; cdecl;
  function getFlashMode: JString; cdecl;
  function getFocalLength: Single; cdecl;
  function getFocusAreas: JList; cdecl;
  procedure getFocusDistances(output: TJavaArray<Single>); cdecl;
  function getFocusMode: JString; cdecl;
  function getHorizontalViewAngle: Single; cdecl;
  function getInt(key: JString): Integer; cdecl;
  function getJpegQuality: Integer; cdecl;
  function getJpegThumbnailQuality: Integer; cdecl;
  function getJpegThumbnailSize: JCamera_Size; cdecl;
  function getMaxExposureCompensation: Integer; cdecl;
  function getMaxNumDetectedFaces: Integer; cdecl;
  function getMaxNumFocusAreas: Integer; cdecl;
  function getMaxNumMeteringAreas: Integer; cdecl;
  function getMaxZoom: Integer; cdecl;
  function getMeteringAreas: JList; cdecl;
  function getMinExposureCompensation: Integer; cdecl;
  function getPictureFormat: Integer; cdecl;
  function getPictureSize: JCamera_Size; cdecl;
  function getPreferredPreviewSizeForVideo: JCamera_Size; cdecl;
  function getPreviewFormat: Integer; cdecl;
  procedure getPreviewFpsRange(range: TJavaArray<Integer>); cdecl;
  function getPreviewFrameRate: Integer; cdecl;//Deprecated
  function getPreviewSize: JCamera_Size; cdecl;
  function getSceneMode: JString; cdecl;
  function getSupportedAntibanding: JList; cdecl;
  function getSupportedColorEffects: JList; cdecl;
  function getSupportedFlashModes: JList; cdecl;
  function getSupportedFocusModes: JList; cdecl;
  function getSupportedJpegThumbnailSizes: JList; cdecl;
  function getSupportedPictureFormats: JList; cdecl;
  function getSupportedPictureSizes: JList; cdecl;
  function getSupportedPreviewFormats: JList; cdecl;
  function getSupportedPreviewFpsRange: TJavaObjectArray<JList>; cdecl;
  function getSupportedPreviewFrameRates: JList; cdecl;//Deprecated
  function getSupportedPreviewSizes: JList; cdecl;
  function getSupportedSceneModes: JList; cdecl;
  function getSupportedVideoSizes: JList; cdecl;
  function getSupportedWhiteBalance: JList; cdecl;
  function getVerticalViewAngle: Single; cdecl;
  function getVideoStabilization: Boolean; cdecl;
  function getWhiteBalance: JString; cdecl;
  function getZoom: Integer; cdecl;
  function getZoomRatios: JList; cdecl;
  function isAutoExposureLockSupported: Boolean; cdecl;
  function isAutoWhiteBalanceLockSupported: Boolean; cdecl;
  function isSmoothZoomSupported: Boolean; cdecl;
  function isVideoSnapshotSupported: Boolean; cdecl;
  function isVideoStabilizationSupported: Boolean; cdecl;
  function isZoomSupported: Boolean; cdecl;
  procedure remove(key: JString); cdecl;
  procedure removeGpsData; cdecl;
  procedure &set(key: JString; value: JString); cdecl; overload;
  procedure &set(key: JString; value: Integer); cdecl; overload;
  procedure setAntibanding(antibanding: JString); cdecl;
  procedure setAutoExposureLock(toggle: Boolean); cdecl;
  procedure setAutoWhiteBalanceLock(toggle: Boolean); cdecl;
  procedure setColorEffect(value: JString); cdecl;
  procedure setExposureCompensation(value: Integer); cdecl;
  procedure setFlashMode(value: JString); cdecl;
  procedure setFocusAreas(focusAreas: JList); cdecl;
  procedure setFocusMode(value: JString); cdecl;
  procedure setGpsAltitude(altitude: Double); cdecl;
  procedure setGpsLatitude(latitude: Double); cdecl;
  procedure setGpsLongitude(longitude: Double); cdecl;
  procedure setGpsProcessingMethod(processing_method: JString); cdecl;
  procedure setGpsTimestamp(timestamp: Int64); cdecl;
  procedure setJpegQuality(quality: Integer); cdecl;
  procedure setJpegThumbnailQuality(quality: Integer); cdecl;
  procedure setJpegThumbnailSize(width: Integer; height: Integer); cdecl;
  procedure setMeteringAreas(meteringAreas: JList); cdecl;
  procedure setPictureFormat(pixel_format: Integer); cdecl;
  procedure setPictureSize(width: Integer; height: Integer); cdecl;
  procedure setPreviewFormat(pixel_format: Integer); cdecl;
  procedure setPreviewFpsRange(min: Integer; max: Integer); cdecl;
  procedure setPreviewFrameRate(fps: Integer); cdecl;//Deprecated
  procedure setPreviewSize(width: Integer; height: Integer); cdecl;
  procedure setRecordingHint(hint: Boolean); cdecl;
  procedure setRotation(rotation: Integer); cdecl;
  procedure setSceneMode(value: JString); cdecl;
  procedure setVideoStabilization(toggle: Boolean); cdecl;
  procedure setWhiteBalance(value: JString); cdecl;
  procedure setZoom(value: Integer); cdecl;
  procedure unflatten(flattened: JString); cdecl;
end;
TJCamera_Parameters = class(TJavaGenericImport<JCamera_ParametersClass, JCamera_Parameters>) end;

JCamera_AutoFocusMoveCallbackClass = interface(IJavaClass)
['{901C04AC-438E-4233-A469-A3A53EA3A5E3}']
end;

[JavaSignature('android/hardware/Camera$AutoFocusMoveCallback')]
JCamera_AutoFocusMoveCallback = interface(IJavaInstance)
['{3F071E3E-4BE6-4DFF-A5C9-458E91DD4204}']
  {Methods}
  procedure onAutoFocusMoving(start: Boolean; camera: JCamera); cdecl;
end;
TJCamera_AutoFocusMoveCallback = class(TJavaGenericImport<JCamera_AutoFocusMoveCallbackClass, JCamera_AutoFocusMoveCallback>) end;

JCamera_AutoFocusCallbackClass = interface(IJavaClass)
['{624541C8-C3C3-4A09-8367-4C3237E658D0}']
end;

[JavaSignature('android/hardware/Camera$AutoFocusCallback')]
JCamera_AutoFocusCallback = interface(IJavaInstance)
['{2E9C9152-C3B7-43EE-98BD-04189FBE43A7}']
  {Methods}
  procedure onAutoFocus(success: Boolean; camera: JCamera); cdecl;
end;
TJCamera_AutoFocusCallback = class(TJavaGenericImport<JCamera_AutoFocusCallbackClass, JCamera_AutoFocusCallback>) end;

JCamera_PictureCallbackClass = interface(IJavaClass)
['{45CCC52F-A446-40A5-BA95-A16D17A11415}']
end;

[JavaSignature('android/hardware/Camera$PictureCallback')]
JCamera_PictureCallback = interface(IJavaInstance)
['{307615DE-4EFD-4290-A113-CCE958C0C8C2}']
  {Methods}
  procedure onPictureTaken(data: TJavaArray<Byte>; camera: JCamera); cdecl;
end;
TJCamera_PictureCallback = class(TJavaGenericImport<JCamera_PictureCallbackClass, JCamera_PictureCallback>) end;

JCamera_OnZoomChangeListenerClass = interface(IJavaClass)
['{7D8BC2A6-9164-48A0-B14F-D65D3642D2BA}']
end;

[JavaSignature('android/hardware/Camera$OnZoomChangeListener')]
JCamera_OnZoomChangeListener = interface(IJavaInstance)
['{8083D248-A911-4752-8C17-5F3C9F26CB33}']
  {Methods}
  procedure onZoomChange(zoomValue: Integer; stopped: Boolean; camera: JCamera); cdecl;
end;
TJCamera_OnZoomChangeListener = class(TJavaGenericImport<JCamera_OnZoomChangeListenerClass, JCamera_OnZoomChangeListener>) end;




implementation

begin

end.


