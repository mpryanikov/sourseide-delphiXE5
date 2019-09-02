{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit Androidapi.JNI.Embarcadero;

interface

uses 
  Androidapi.JNIBridge,
  Androidapi.JNI.Os,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.App,
  Androidapi.JNI.Net,
  Androidapi.JNI.Location,
  Androidapi.JNI.Webkit,
  Androidapi.JNI.Hardware,
  Androidapi.JNI.Util,
  Androidapi.JNI.GraphicsContentViewText;

type

  {Class forward declarations}
  JFMXMediaLibrary = interface;//com.embarcadero.firemonkey.medialibrary.FMXMediaLibrary
  JOnWebViewListener = interface;//com.embarcadero.firemonkey.webbrowser.OnWebViewListener
  JBasePicker = interface;//com.embarcadero.firemonkey.pickers.BasePicker
  JBaseDateTimePicker = interface;//com.embarcadero.firemonkey.pickers.BaseDateTimePicker
  JWebBrowser = interface;//com.embarcadero.firemonkey.webbrowser.WebBrowser
  JFMXTextEditorProxy = interface;//com.embarcadero.firemonkey.text.FMXTextEditorProxy
  JOnDateTimeChangedListener = interface;//com.embarcadero.firemonkey.pickers.OnDateTimeChangedListener
  JWebClient = interface;//com.embarcadero.firemonkey.webbrowser.WebClient
  JNativeLayout = interface;//com.embarcadero.firemonkey.nativelayout.NativeLayout
  JVKStateChangeListener = interface;//com.embarcadero.firemonkey.text.VKStateChangeListener
  JFMXTextEditorProxy_OnEnterActionListener = interface;//com.embarcadero.firemonkey.text.FMXTextEditorProxy$OnEnterActionListener
  JJavaGeocoder = interface;//com.embarcadero.firemonkey.geocoder.JavaGeocoder
  JFMXNotificationAlarm = interface;//com.embarcadero.firemonkey.notifications.FMXNotificationAlarm
  JOnItemChangedListener = interface;//com.embarcadero.firemonkey.pickers.OnItemChangedListener
  JOnActivityListener = interface;//com.embarcadero.firemonkey.OnActivityListener
  JCamPreview = interface;//com.embarcadero.firemonkey.camerapreview.CamPreview
  JFMXUtils = interface;//com.embarcadero.firemonkey.medialibrary.FMXUtils
  JBasePickersFactory = interface;//com.embarcadero.firemonkey.pickers.BasePickersFactory
  JFMXNativeActivity = interface;//com.embarcadero.firemonkey.FMXNativeActivity
  JFMXTextListener = interface;//com.embarcadero.firemonkey.text.FMXTextListener
  JBaseListPicker = interface;//com.embarcadero.firemonkey.pickers.BaseListPicker

JFMXMediaLibraryClass = interface(JObjectClass)
['{66D17071-77BC-4881-9546-0F803D781E41}']
  {Property Methods}
  function _GetACTION_TAKE_IMAGE_FROM_CAMERA: Integer;
  function _GetACTION_TAKE_IMAGE_FROM_LIBRARY: Integer;
  {Methods}
  function init(activity: JActivity): JFMXMediaLibrary; cdecl;
  {Properties}
  property ACTION_TAKE_IMAGE_FROM_CAMERA: Integer read _GetACTION_TAKE_IMAGE_FROM_CAMERA;
  property ACTION_TAKE_IMAGE_FROM_LIBRARY: Integer read _GetACTION_TAKE_IMAGE_FROM_LIBRARY;
end;

[JavaSignature('com/embarcadero/firemonkey/medialibrary/FMXMediaLibrary')]
JFMXMediaLibrary = interface(JObject)
['{FA55B1EF-8E7E-4D22-B835-14F0B0B4C375}']
  {Methods}
  function handleCameraPhoto(data: JIntent): JBitmap; cdecl;
  function handleLibraryPhoto(data: JIntent): JBitmap; cdecl;
  procedure onRestoreInstanceState(savedInstanceState: JBundle); cdecl;
  procedure onSaveInstanceState(outState: JBundle); cdecl;
  procedure takeImageFromCamera(maxWidth: Integer; maxHeight: Integer); cdecl;
  procedure takeImageFromLibrary(maxWidth: Integer; maxHeight: Integer); cdecl;
end;
TJFMXMediaLibrary = class(TJavaGenericImport<JFMXMediaLibraryClass, JFMXMediaLibrary>) end;

JOnWebViewListenerClass = interface(IJavaClass)
['{01D11CF8-BF46-45EF-8090-628E2BA23A2E}']
end;

[JavaSignature('com/embarcadero/firemonkey/webbrowser/OnWebViewListener')]
JOnWebViewListener = interface(IJavaInstance)
['{EF36619D-8759-4381-B638-C99B1C0A0EF8}']
  {Methods}
  procedure doUpdateVisitedHistory(view: JWebView; url: JString; isReload: Boolean); cdecl;
  procedure onFormResubmission(view: JWebView; dontResend: JMessage; resend: JMessage); cdecl;
  procedure onLoadResource(view: JWebView; url: JString); cdecl;
  procedure onPageFinished(view: JWebView; url: JString); cdecl;
  procedure onPageStarted(view: JWebView; url: JString; favicon: JBitmap); cdecl;
  procedure onReceivedError(view: JWebView; errorCode: Integer; description: JString; failingUrl: JString); cdecl;
  procedure onReceivedHttpAuthRequest(view: JWebView; handler: JHttpAuthHandler; host: JString; realm: JString); cdecl;
  procedure onReceivedSslError(view: JWebView; handler: JSslErrorHandler; error: JSslError); cdecl;
  procedure onScaleChanged(view: JWebView; oldScale: Single; newScale: Single); cdecl;
  procedure onUnhandledKeyEvent(view: JWebView; event: JKeyEvent); cdecl;
  function shouldOverrideKeyEvent(view: JWebView; event: JKeyEvent): Boolean; cdecl;
  function shouldOverrideUrlLoading(view: JWebView; url: JString): Boolean; cdecl;
end;
TJOnWebViewListener = class(TJavaGenericImport<JOnWebViewListenerClass, JOnWebViewListener>) end;

JBasePickerClass = interface(JObjectClass)
['{9F35AC6E-96AD-4E73-BFC2-23B3ACB3F01E}']
  {Methods}
  function init: JBasePicker; cdecl;
end;

[JavaSignature('com/embarcadero/firemonkey/pickers/BasePicker')]
JBasePicker = interface(JObject)
['{FB1961D2-65AB-4224-B8D9-2D48AA436652}']
  {Methods}
  procedure hide; cdecl;
  function isShown: Boolean; cdecl;
  procedure setTheme(theme: Integer); cdecl;
  procedure show; cdecl;
end;
TJBasePicker = class(TJavaGenericImport<JBasePickerClass, JBasePicker>) end;

JBaseDateTimePickerClass = interface(JBasePickerClass)
['{AEF4DB42-B726-4D97-923F-EBACCD26AE68}']
  {Methods}
  function init: JBaseDateTimePicker; cdecl;
end;

[JavaSignature('com/embarcadero/firemonkey/pickers/BaseDateTimePicker')]
JBaseDateTimePicker = interface(JBasePicker)
['{93D09E24-E245-43DE-B992-77C7BB27C672}']
  {Methods}
  function hasListener: Boolean; cdecl;
  procedure setDate(timeInMillis: Int64); cdecl;
  procedure setListener(listener: JOnDateTimeChangedListener); cdecl;
end;
TJBaseDateTimePicker = class(TJavaGenericImport<JBaseDateTimePickerClass, JBaseDateTimePicker>) end;

JWebBrowserClass = interface(JWebViewClass)
['{C9D39057-C2B7-4264-8E58-52DE4CA5AE56}']
  {Methods}
  function init(context: JContext): JWebBrowser; cdecl;
end;

[JavaSignature('com/embarcadero/firemonkey/webbrowser/WebBrowser')]
JWebBrowser = interface(JWebView)
['{21876269-EEA5-4130-BE15-F23BEB8ECA69}']
  {Methods}
  procedure SetWebViewListener(listener: JOnWebViewListener); cdecl;
end;
TJWebBrowser = class(TJavaGenericImport<JWebBrowserClass, JWebBrowser>) end;

JFMXTextEditorProxyClass = interface(JViewClass)
['{A014F8A2-06B4-4AF7-B077-52980F566B5E}']
  {Property Methods}
  function _GetACTION_DONE: Integer;
  function _GetACTION_ENTER: Integer;
  function _GetACTION_GO: Integer;
  function _GetACTION_NEXT: Integer;
  function _GetACTION_SEARCH: Integer;
  function _GetACTION_SEND: Integer;
  function _GetINPUT_ALPHABET: Integer;
  function _GetINPUT_EMAIL_ADDRESS: Integer;
  function _GetINPUT_NAME_PHONE_PAD: Integer;
  function _GetINPUT_NUMBER: Integer;
  function _GetINPUT_NUMBER_AND_PUNCTUATION: Integer;
  function _GetINPUT_PHONE: Integer;
  function _GetINPUT_TEXT: Integer;
  function _GetINPUT_URL: Integer;
  {Methods}
  function init(context: JContext): JFMXTextEditorProxy; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet): JFMXTextEditorProxy; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet; defStyle: Integer): JFMXTextEditorProxy; cdecl; overload;
  {Properties}
  property ACTION_DONE: Integer read _GetACTION_DONE;
  property ACTION_ENTER: Integer read _GetACTION_ENTER;
  property ACTION_GO: Integer read _GetACTION_GO;
  property ACTION_NEXT: Integer read _GetACTION_NEXT;
  property ACTION_SEARCH: Integer read _GetACTION_SEARCH;
  property ACTION_SEND: Integer read _GetACTION_SEND;
  property INPUT_ALPHABET: Integer read _GetINPUT_ALPHABET;
  property INPUT_EMAIL_ADDRESS: Integer read _GetINPUT_EMAIL_ADDRESS;
  property INPUT_NAME_PHONE_PAD: Integer read _GetINPUT_NAME_PHONE_PAD;
  property INPUT_NUMBER: Integer read _GetINPUT_NUMBER;
  property INPUT_NUMBER_AND_PUNCTUATION: Integer read _GetINPUT_NUMBER_AND_PUNCTUATION;
  property INPUT_PHONE: Integer read _GetINPUT_PHONE;
  property INPUT_TEXT: Integer read _GetINPUT_TEXT;
  property INPUT_URL: Integer read _GetINPUT_URL;
end;

[JavaSignature('com/embarcadero/firemonkey/text/FMXTextEditorProxy')]
JFMXTextEditorProxy = interface(JView)
['{667444A4-E8C6-4FFD-ABC5-69B426278BFA}']
  {Methods}
  procedure addTextListener(textListener: JFMXTextListener); cdecl;
  function getCursorPosition: Integer; cdecl;
  function getEditable: JEditable; cdecl;
  function getSelectionEnd: Integer; cdecl;
  function getSelectionStart: Integer; cdecl;
  function getText: JCharSequence; cdecl;
  function onCheckIsTextEditor: Boolean; cdecl;
  function onCreateInputConnection(outAttrs: JEditorInfo): JInputConnection; cdecl;
  procedure onDraw(canvas: JCanvas); cdecl;
  function onKeyDown(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  procedure onRestoreInstanceState(state: JParcelable); cdecl;
  function onSaveInstanceState: JParcelable; cdecl;
  function onTouchEvent(event: JMotionEvent): Boolean; cdecl;
  procedure removeTextListener(textListener: JFMXTextListener); cdecl;
  procedure resetSingleLineText; cdecl;
  procedure sendDeleteSurroundingText(beforeLength: Integer; afterLength: Integer); cdecl;
  procedure sendEndIMEInput; cdecl;
  procedure sendInputText(text: JCharSequence); cdecl;
  procedure sendSetMarkedText(text: JCharSequence); cdecl;
  procedure sendSkipKeyEvent(event: JKeyEvent); cdecl;
  procedure sendStartIMEInput; cdecl;
  procedure setEnterAction(enterAction: Integer); cdecl;
  procedure setInputType(inputType: Integer); cdecl;
  procedure setOnEditorActionListener(actionListener: JFMXTextEditorProxy_OnEnterActionListener); cdecl;
  procedure setOnVKStateChangeListener(vkListener: JVKStateChangeListener); cdecl;
  procedure setSelection(start: Integer; end_: Integer); cdecl;
  procedure setCursorPosition(position: Integer); cdecl;
  procedure setText(text: JCharSequence); cdecl;
  procedure showSoftInput(show: Boolean); cdecl;
  procedure copySelectedText; cdecl;
  procedure cutSelectedText; cdecl;
  procedure pasteText; cdecl;
end;
TJFMXTextEditorProxy = class(TJavaGenericImport<JFMXTextEditorProxyClass, JFMXTextEditorProxy>) end;

JOnDateTimeChangedListenerClass = interface(IJavaClass)
['{6253D5BE-9035-4894-B479-2B91B1496180}']
end;

[JavaSignature('com/embarcadero/firemonkey/pickers/OnDateTimeChangedListener')]
JOnDateTimeChangedListener = interface(IJavaInstance)
['{3BE7DC7B-92F9-4B5D-BBC4-B49FAE5CE96E}']
  {Methods}
  procedure onDateChanged(date: JDate); cdecl;
  procedure onHide; cdecl;
  procedure onShow; cdecl;
end;
TJOnDateTimeChangedListener = class(TJavaGenericImport<JOnDateTimeChangedListenerClass, JOnDateTimeChangedListener>) end;

JWebClientClass = interface(JWebViewClientClass)
['{424BCB34-24B0-4A4B-830B-D396C3667344}']
  {Methods}
  function init: JWebClient; cdecl;
end;

[JavaSignature('com/embarcadero/firemonkey/webbrowser/WebClient')]
JWebClient = interface(JWebViewClient)
['{6D32A60F-6D97-4756-95C6-AFBFE15C465F}']
  {Methods}
  procedure SetWebViewListener(listener: JOnWebViewListener); cdecl;
  procedure doUpdateVisitedHistory(view: JWebView; url: JString; isReload: Boolean); cdecl;
  procedure onFormResubmission(view: JWebView; dontResend: JMessage; resend: JMessage); cdecl;
  procedure onLoadResource(view: JWebView; url: JString); cdecl;
  procedure onPageFinished(view: JWebView; url: JString); cdecl;
  procedure onPageStarted(view: JWebView; url: JString; favicon: JBitmap); cdecl;
  procedure onReceivedError(view: JWebView; errorCode: Integer; description: JString; failingUrl: JString); cdecl;
  procedure onReceivedHttpAuthRequest(view: JWebView; handler: JHttpAuthHandler; host: JString; realm: JString); cdecl;
  procedure onReceivedSslError(view: JWebView; handler: JSslErrorHandler; error: JSslError); cdecl;
  procedure onScaleChanged(view: JWebView; oldScale: Single; newScale: Single); cdecl;
  procedure onUnhandledKeyEvent(view: JWebView; event: JKeyEvent); cdecl;
  function shouldOverrideKeyEvent(view: JWebView; event: JKeyEvent): Boolean; cdecl;
  function shouldOverrideUrlLoading(view: JWebView; url: JString): Boolean; cdecl;
end;
TJWebClient = class(TJavaGenericImport<JWebClientClass, JWebClient>) end;

JNativeLayoutClass = interface(JWindowManager_LayoutParamsClass)
['{199D9A89-2415-4FFA-8BF5-92EABBD883CE}']
  {Methods}
  function init(Con: JContext; Token: JIBinder): JNativeLayout; cdecl;
end;

[JavaSignature('com/embarcadero/firemonkey/nativelayout/NativeLayout')]
JNativeLayout = interface(JWindowManager_LayoutParams)
['{3D369CB4-4A09-4C30-8D92-9BA198957802}']
  {Methods}
  procedure SetControl(view: JView); cdecl;
  procedure SetFocus(newFocusState: Boolean); cdecl;
  procedure SetPosition(absoluteX: Integer; absoluteY: Integer); cdecl;
  procedure SetSize(absoluteWidth: Integer; absoluteHeight: Integer); cdecl;
end;
TJNativeLayout = class(TJavaGenericImport<JNativeLayoutClass, JNativeLayout>) end;

JVKStateChangeListenerClass = interface(IJavaClass)
['{3FB50750-6D9D-436D-829F-BE6EA8085453}']
end;

[JavaSignature('com/embarcadero/firemonkey/text/VKStateChangeListener')]
JVKStateChangeListener = interface(IJavaInstance)
['{00B21846-0476-4BF6-8F08-7DC3774FA25F}']
  {Methods}
  procedure onVirtualKeyboardHidden; cdecl;
  procedure onVirtualKeyboardShown; cdecl;
end;
TJVKStateChangeListener = class(TJavaGenericImport<JVKStateChangeListenerClass, JVKStateChangeListener>) end;

JFMXTextEditorProxy_OnEnterActionListenerClass = interface(IJavaClass)
['{E309A61C-597B-4A10-AA18-11761591D1CF}']
end;

[JavaSignature('com/embarcadero/firemonkey/text/FMXTextEditorProxy$OnEnterActionListener')]
JFMXTextEditorProxy_OnEnterActionListener = interface(IJavaInstance)
['{5C64CAD2-21B3-4158-92C6-7E3C24901CA5}']
  {Methods}
  function onAction(v: JFMXTextEditorProxy; actionId: Integer; event: JKeyEvent): Boolean; cdecl;
end;
TJFMXTextEditorProxy_OnEnterActionListener = class(TJavaGenericImport<JFMXTextEditorProxy_OnEnterActionListenerClass, JFMXTextEditorProxy_OnEnterActionListener>) end;

JJavaGeocoderClass = interface(JObjectClass)
['{DC6EC615-5DEF-4BFC-88A7-FA31017D974B}']
  {Methods}
  function init(Con: JContext): JJavaGeocoder; cdecl;
end;

[JavaSignature('com/embarcadero/firemonkey/geocoder/JavaGeocoder')]
JJavaGeocoder = interface(JObject)
['{A6AF9F65-04F2-4AE6-A75A-ABE841B23DD2}']
  {Property Methods}
  function _GetInstanceOfGeocoder: JGeocoder;
  procedure _SetInstanceOfGeocoder(Value: JGeocoder);
  {Properties}
  property InstanceOfGeocoder: JGeocoder read _GetInstanceOfGeocoder write _SetInstanceOfGeocoder;
end;
TJJavaGeocoder = class(TJavaGenericImport<JJavaGeocoderClass, JJavaGeocoder>) end;

JFMXNotificationAlarmClass = interface(JBroadcastReceiverClass)
['{D8C33135-DB42-46AB-B9A0-EB049BE7EA31}']
  {Property Methods}
  function _GetACTION_FMX_NOTIFICATION: JString;
  function _GetEXTRA_ACTIVITY_CLASS_NAME: JString;
  function _GetEXTRA_ALERT_ACTION: JString;
  function _GetEXTRA_ALERT_BODY: JString;
  function _GetEXTRA_ENABLE_SOUND: JString;
  function _GetEXTRA_FIRE_DATE: JString;
  function _GetEXTRA_FIRE_GMT_DATE: JString;
  function _GetEXTRA_HAS_ACTION: JString;
  function _GetEXTRA_NAME: JString;
  function _GetEXTRA_NUMBER: JString;
  function _GetEXTRA_TITLE: JString;
  function _GetEXTRA_UNIQUE_ID: JString;
  function _GetFMX_NOTIFICATION_CENTER: JString;
  {Methods}
  function init: JFMXNotificationAlarm; cdecl;
  {Properties}
  property ACTION_FMX_NOTIFICATION: JString read _GetACTION_FMX_NOTIFICATION;
  property EXTRA_ACTIVITY_CLASS_NAME: JString read _GetEXTRA_ACTIVITY_CLASS_NAME;
  property EXTRA_ALERT_ACTION: JString read _GetEXTRA_ALERT_ACTION;
  property EXTRA_ALERT_BODY: JString read _GetEXTRA_ALERT_BODY;
  property EXTRA_ENABLE_SOUND: JString read _GetEXTRA_ENABLE_SOUND;
  property EXTRA_FIRE_DATE: JString read _GetEXTRA_FIRE_DATE;
  property EXTRA_FIRE_GMT_DATE: JString read _GetEXTRA_FIRE_GMT_DATE;
  property EXTRA_HAS_ACTION: JString read _GetEXTRA_HAS_ACTION;
  property EXTRA_NAME: JString read _GetEXTRA_NAME;
  property EXTRA_NUMBER: JString read _GetEXTRA_NUMBER;
  property EXTRA_TITLE: JString read _GetEXTRA_TITLE;
  property EXTRA_UNIQUE_ID: JString read _GetEXTRA_UNIQUE_ID;
  property FMX_NOTIFICATION_CENTER: JString read _GetFMX_NOTIFICATION_CENTER;
end;

[JavaSignature('com/embarcadero/firemonkey/notifications/FMXNotificationAlarm')]
JFMXNotificationAlarm = interface(JBroadcastReceiver)
['{3C593D47-0A41-41F4-A49F-3163A2D19F90}']
  {Methods}
  procedure onReceive(context: JContext; intent: JIntent); cdecl;
end;
TJFMXNotificationAlarm = class(TJavaGenericImport<JFMXNotificationAlarmClass, JFMXNotificationAlarm>) end;

JOnItemChangedListenerClass = interface(IJavaClass)
['{1A8B5BC7-25C3-43B9-9E0E-B144BDC49CC5}']
end;

[JavaSignature('com/embarcadero/firemonkey/pickers/OnItemChangedListener')]
JOnItemChangedListener = interface(IJavaInstance)
['{BC0BDDA3-53FD-4BA8-9E3D-B17F647345C6}']
  {Methods}
  procedure onHide; cdecl;
  procedure onItemChanged(itemIndex: Integer); cdecl;
  procedure onShow; cdecl;
end;
TJOnItemChangedListener = class(TJavaGenericImport<JOnItemChangedListenerClass, JOnItemChangedListener>) end;

JOnActivityListenerClass = interface(IJavaClass)
['{168F8C7B-7FE9-4A08-87AD-51CCC3C56E43}']
end;

[JavaSignature('com/embarcadero/firemonkey/OnActivityListener')]
JOnActivityListener = interface(IJavaInstance)
['{D0E0FCFB-0400-4522-B51E-220FC79F92BB}']
  {Methods}
  procedure onCancelReceiveImage(requestCode: Integer); cdecl;
  procedure onReceiveImage(requestCode: Integer; bitmap: JBitmap); cdecl;
  procedure onReceiveNotification(intent: JIntent); cdecl;
end;
TJOnActivityListener = class(TJavaGenericImport<JOnActivityListenerClass, JOnActivityListener>) end;

JCamPreviewClass = interface(JSurfaceViewClass)
['{4A2F8A98-B8E3-4616-8E02-DA083EC4E2BA}']
  {Methods}
  function init(context: JContext): JCamPreview; cdecl;
end;

[JavaSignature('com/embarcadero/firemonkey/camerapreview/CamPreview')]
JCamPreview = interface(JSurfaceView)
['{09E012FD-099E-45F9-AC84-DDB431920646}']
  {Property Methods}
  function _GetmCamera: JCamera;
  procedure _SetmCamera(Value: JCamera);
  {Methods}
  procedure draw(canvas: JCanvas); cdecl;
  procedure surfaceChanged(holder: JSurfaceHolder; format: Integer; w: Integer; h: Integer); cdecl;
  procedure surfaceCreated(holder: JSurfaceHolder); cdecl;
  procedure surfaceDestroyed(holder: JSurfaceHolder); cdecl;
  {Properties}
  property mCamera: JCamera read _GetmCamera write _SetmCamera;
end;
TJCamPreview = class(TJavaGenericImport<JCamPreviewClass, JCamPreview>) end;

JFMXUtilsClass = interface(JObjectClass)
['{40DDB67A-0058-44C3-8C36-17E3EEC17BCF}']
  {Methods}
  function init: JFMXUtils; cdecl;
  function copyStream(input: JInputStream; output: JOutputStream): Integer; cdecl;
end;

[JavaSignature('com/embarcadero/firemonkey/medialibrary/FMXUtils')]
JFMXUtils = interface(JObject)
['{6A581D8D-E53E-4052-BFB4-BDD220EC866E}']
end;
TJFMXUtils = class(TJavaGenericImport<JFMXUtilsClass, JFMXUtils>) end;

JBasePickersFactoryClass = interface(JObjectClass)
['{235431A6-84D7-48A1-9194-9A44E118294E}']
  {Methods}
  function init: JBasePickersFactory; cdecl;
  function getFactory: JBasePickersFactory; cdecl;
end;

[JavaSignature('com/embarcadero/firemonkey/pickers/BasePickersFactory')]
JBasePickersFactory = interface(JObject)
['{F192F199-A07C-41CC-A4A4-8DCC857B26EA}']
  {Methods}
  function createDatePicker(activity: JFMXNativeActivity): JBaseDateTimePicker; cdecl;
  function createListPicker(activity: JFMXNativeActivity): JBaseListPicker; cdecl;
  function createTimePicker(activity: JFMXNativeActivity): JBaseDateTimePicker; cdecl;
end;
TJBasePickersFactory = class(TJavaGenericImport<JBasePickersFactoryClass, JBasePickersFactory>) end;

JFMXNativeActivityClass = interface(JNativeActivityClass)
['{829C77FB-08F1-4D19-9782-3C58EEC12599}']
  {Methods}
  function init: JFMXNativeActivity; cdecl;
end;

[JavaSignature('com/embarcadero/firemonkey/FMXNativeActivity')]
JFMXNativeActivity = interface(JNativeActivity)
['{2FA559EC-D1D7-46AA-9C52-FEFC6B3E2DE3}']
  {Methods}
  function dispatchKeyEvent(event: JKeyEvent): Boolean; cdecl;
  procedure embSetOrientation(orientationMask: Integer); cdecl;
  function getFMXMediaLibrary: JFMXMediaLibrary; cdecl;
  function getLastEvent: JKeyEvent; cdecl;
  function getNextPickerDialogId: Integer; cdecl;
  function getTextEditorProxy: JFMXTextEditorProxy; cdecl;
  procedure hideStatusBar; cdecl;
  procedure setListener(listener: JOnActivityListener); cdecl;
  procedure showDialog(id: Integer; dialog: JDialog); cdecl;
  procedure showStatusBar; cdecl;
end;
TJFMXNativeActivity = class(TJavaGenericImport<JFMXNativeActivityClass, JFMXNativeActivity>) end;

JFMXTextListenerClass = interface(IJavaClass)
['{A8C204B6-E91D-43C9-AAE6-A8F638AC8B0B}']
end;

[JavaSignature('com/embarcadero/firemonkey/text/FMXTextListener')]
JFMXTextListener = interface(IJavaInstance)
['{343503D5-CA05-4DAE-AABF-C112202B9CFA}']
  {Methods}
  procedure onTextUpdated(text: JCharSequence; position: Integer); cdecl;
  procedure onComposingText(beginPosition: Integer; endPosition: Integer); cdecl;
  procedure onSkipKeyEvent(event: JKeyEvent); cdecl;
end;
TJFMXTextListener = class(TJavaGenericImport<JFMXTextListenerClass, JFMXTextListener>) end;

JBaseListPickerClass = interface(JBasePickerClass)
['{960A0BD3-E971-4206-B089-ED555BA8F736}']
  {Methods}
  function init: JBaseListPicker; cdecl;
end;

[JavaSignature('com/embarcadero/firemonkey/pickers/BaseListPicker')]
JBaseListPicker = interface(JBasePicker)
['{E7D99458-E717-4FDB-8CEB-8288A81AC9B4}']
  {Methods}
  function hasListener: Boolean; cdecl;
  procedure setItemIndex(itemIndex: Integer); cdecl;
  procedure setItems(items: TJavaObjectArray<JCharSequence>); cdecl;
  procedure setListener(listener: JOnItemChangedListener); cdecl;
end;
TJBaseListPicker = class(TJavaGenericImport<JBaseListPickerClass, JBaseListPicker>) end;




implementation

begin

end.


