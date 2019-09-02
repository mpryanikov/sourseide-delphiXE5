{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit Androidapi.JNI.InputMethodService;

interface

uses 
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
  Androidapi.JNI.Widget,
  Androidapi.JNI.App,
  Androidapi.JNI.Util,
  Androidapi.JNI.GraphicsContentViewText;

type

  {Class forward declarations}
  JExtractEditText = interface;//android.inputmethodservice.ExtractEditText
  JAbstractInputMethodService_AbstractInputMethodImpl = interface;//android.inputmethodservice.AbstractInputMethodService$AbstractInputMethodImpl
  JAbstractInputMethodService = interface;//android.inputmethodservice.AbstractInputMethodService
  JAbstractInputMethodService_AbstractInputMethodSessionImpl = interface;//android.inputmethodservice.AbstractInputMethodService$AbstractInputMethodSessionImpl
  JKeyboard_Key = interface;//android.inputmethodservice.Keyboard$Key
  JInputMethodService_InputMethodSessionImpl = interface;//android.inputmethodservice.InputMethodService$InputMethodSessionImpl
  JKeyboardView = interface;//android.inputmethodservice.KeyboardView
  JInputMethodService_InputMethodImpl = interface;//android.inputmethodservice.InputMethodService$InputMethodImpl
  JInputMethodService_Insets = interface;//android.inputmethodservice.InputMethodService$Insets
  JKeyboardView_OnKeyboardActionListener = interface;//android.inputmethodservice.KeyboardView$OnKeyboardActionListener
  JKeyboard = interface;//android.inputmethodservice.Keyboard
  JKeyboard_Row = interface;//android.inputmethodservice.Keyboard$Row
  JInputMethodService = interface;//android.inputmethodservice.InputMethodService

JExtractEditTextClass = interface(JEditTextClass)
['{B369E49F-E699-4912-A4E6-BCBF578FFE74}']
  {Methods}
  function init(context: JContext): JExtractEditText; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet): JExtractEditText; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet; defStyle: Integer): JExtractEditText; cdecl; overload;
end;

[JavaSignature('android/inputmethodservice/ExtractEditText')]
JExtractEditText = interface(JEditText)
['{09E89DEE-6973-47FE-8FC5-A68EB74B34F1}']
  {Methods}
  procedure finishInternalChanges; cdecl;
  function hasFocus: Boolean; cdecl;
  function hasVerticalScrollBar: Boolean; cdecl;
  function hasWindowFocus: Boolean; cdecl;
  function isFocused: Boolean; cdecl;
  function isInputMethodTarget: Boolean; cdecl;
  function onTextContextMenuItem(id: Integer): Boolean; cdecl;
  function performClick: Boolean; cdecl;
  procedure setExtractedText(text: JExtractedText); cdecl;
  procedure startInternalChanges; cdecl;
end;
TJExtractEditText = class(TJavaGenericImport<JExtractEditTextClass, JExtractEditText>) end;

JAbstractInputMethodService_AbstractInputMethodImplClass = interface(JObjectClass)
['{59CB1E26-657A-4437-B964-FE56B62FFC5B}']
  {Methods}
  function init: JAbstractInputMethodService_AbstractInputMethodImpl; cdecl;
end;

[JavaSignature('android/inputmethodservice/AbstractInputMethodService$AbstractInputMethodImpl')]
JAbstractInputMethodService_AbstractInputMethodImpl = interface(JObject)
['{4BA1739F-1C71-4182-B6C8-B4884FBA7F42}']
  {Methods}
  procedure createSession(callback: JInputMethod_SessionCallback); cdecl;
  procedure revokeSession(session: JInputMethodSession); cdecl;
  procedure setSessionEnabled(session: JInputMethodSession; enabled: Boolean); cdecl;
end;
TJAbstractInputMethodService_AbstractInputMethodImpl = class(TJavaGenericImport<JAbstractInputMethodService_AbstractInputMethodImplClass, JAbstractInputMethodService_AbstractInputMethodImpl>) end;

JAbstractInputMethodServiceClass = interface(JServiceClass)
['{65936083-8001-4263-B08D-AF9EB3818754}']
  {Methods}
  function init: JAbstractInputMethodService; cdecl;
end;

[JavaSignature('android/inputmethodservice/AbstractInputMethodService')]
JAbstractInputMethodService = interface(JService)
['{76A44416-D321-44FE-8DC9-CA4905BA14AF}']
  {Methods}
  function getKeyDispatcherState: JKeyEvent_DispatcherState; cdecl;
  function onBind(intent: JIntent): JIBinder; cdecl;
  function onCreateInputMethodInterface: JAbstractInputMethodService_AbstractInputMethodImpl; cdecl;
  function onCreateInputMethodSessionInterface: JAbstractInputMethodService_AbstractInputMethodSessionImpl; cdecl;
  function onGenericMotionEvent(event: JMotionEvent): Boolean; cdecl;
  function onTrackballEvent(event: JMotionEvent): Boolean; cdecl;
end;
TJAbstractInputMethodService = class(TJavaGenericImport<JAbstractInputMethodServiceClass, JAbstractInputMethodService>) end;

JAbstractInputMethodService_AbstractInputMethodSessionImplClass = interface(JObjectClass)
['{7E04E9F4-F218-4249-81A1-2A85C94750F9}']
  {Methods}
  function init: JAbstractInputMethodService_AbstractInputMethodSessionImpl; cdecl;
end;

[JavaSignature('android/inputmethodservice/AbstractInputMethodService$AbstractInputMethodSessionImpl')]
JAbstractInputMethodService_AbstractInputMethodSessionImpl = interface(JObject)
['{32455BA6-922B-4AD4-BFBC-6E1305890C3B}']
  {Methods}
  procedure dispatchGenericMotionEvent(seq: Integer; event: JMotionEvent; callback: JInputMethodSession_EventCallback); cdecl;
  procedure dispatchKeyEvent(seq: Integer; event: JKeyEvent; callback: JInputMethodSession_EventCallback); cdecl;
  procedure dispatchTrackballEvent(seq: Integer; event: JMotionEvent; callback: JInputMethodSession_EventCallback); cdecl;
  function isEnabled: Boolean; cdecl;
  function isRevoked: Boolean; cdecl;
  procedure revokeSelf; cdecl;
  procedure setEnabled(enabled: Boolean); cdecl;
end;
TJAbstractInputMethodService_AbstractInputMethodSessionImpl = class(TJavaGenericImport<JAbstractInputMethodService_AbstractInputMethodSessionImplClass, JAbstractInputMethodService_AbstractInputMethodSessionImpl>) end;

JKeyboard_KeyClass = interface(JObjectClass)
['{97E00498-38CE-40C5-BF50-AE916672F4EA}']
  {Methods}
  function init(parent: JKeyboard_Row): JKeyboard_Key; cdecl; overload;
  function init(res: JResources; parent: JKeyboard_Row; x: Integer; y: Integer; parser: JXmlResourceParser): JKeyboard_Key; cdecl; overload;
end;

[JavaSignature('android/inputmethodservice/Keyboard$Key')]
JKeyboard_Key = interface(JObject)
['{244EF65E-2ECB-404B-BAB5-F2354EFFCCEB}']
  {Property Methods}
  function _Getcodes: TJavaArray<Integer>;
  procedure _Setcodes(Value: TJavaArray<Integer>);
  function _GetedgeFlags: Integer;
  procedure _SetedgeFlags(Value: Integer);
  function _Getgap: Integer;
  procedure _Setgap(Value: Integer);
  function _Getheight: Integer;
  procedure _Setheight(Value: Integer);
  function _Geticon: JDrawable;
  procedure _Seticon(Value: JDrawable);
  function _GeticonPreview: JDrawable;
  procedure _SeticonPreview(Value: JDrawable);
  function _Getlabel: JCharSequence;
  procedure _Setlabel(Value: JCharSequence);
  function _Getmodifier: Boolean;
  procedure _Setmodifier(Value: Boolean);
  function _Geton: Boolean;
  procedure _Seton(Value: Boolean);
  function _GetpopupCharacters: JCharSequence;
  procedure _SetpopupCharacters(Value: JCharSequence);
  function _GetpopupResId: Integer;
  procedure _SetpopupResId(Value: Integer);
  function _Getpressed: Boolean;
  procedure _Setpressed(Value: Boolean);
  function _Getrepeatable: Boolean;
  procedure _Setrepeatable(Value: Boolean);
  function _Getsticky: Boolean;
  procedure _Setsticky(Value: Boolean);
  function _Gettext: JCharSequence;
  procedure _Settext(Value: JCharSequence);
  function _Getwidth: Integer;
  procedure _Setwidth(Value: Integer);
  function _Getx: Integer;
  procedure _Setx(Value: Integer);
  function _Gety: Integer;
  procedure _Sety(Value: Integer);
  {Methods}
  function getCurrentDrawableState: TJavaArray<Integer>; cdecl;
  function isInside(x: Integer; y: Integer): Boolean; cdecl;
  procedure onPressed; cdecl;
  procedure onReleased(inside: Boolean); cdecl;
  function squaredDistanceFrom(x: Integer; y: Integer): Integer; cdecl;
  {Properties}
  property codes: TJavaArray<Integer> read _Getcodes write _Setcodes;
  property edgeFlags: Integer read _GetedgeFlags write _SetedgeFlags;
  property gap: Integer read _Getgap write _Setgap;
  property height: Integer read _Getheight write _Setheight;
  property icon: JDrawable read _Geticon write _Seticon;
  property iconPreview: JDrawable read _GeticonPreview write _SeticonPreview;
  property &label: JCharSequence read _Getlabel write _Setlabel;
  property modifier: Boolean read _Getmodifier write _Setmodifier;
  property on: Boolean read _Geton write _Seton;
  property popupCharacters: JCharSequence read _GetpopupCharacters write _SetpopupCharacters;
  property popupResId: Integer read _GetpopupResId write _SetpopupResId;
  property pressed: Boolean read _Getpressed write _Setpressed;
  property repeatable: Boolean read _Getrepeatable write _Setrepeatable;
  property sticky: Boolean read _Getsticky write _Setsticky;
  property text: JCharSequence read _Gettext write _Settext;
  property width: Integer read _Getwidth write _Setwidth;
  property x: Integer read _Getx write _Setx;
  property y: Integer read _Gety write _Sety;
end;
TJKeyboard_Key = class(TJavaGenericImport<JKeyboard_KeyClass, JKeyboard_Key>) end;

JInputMethodService_InputMethodSessionImplClass = interface(JAbstractInputMethodService_AbstractInputMethodSessionImplClass)
['{54A7E06B-0E98-4B57-ADFF-353732851EBF}']
  {Methods}
  function init: JInputMethodService_InputMethodSessionImpl; cdecl;
end;

[JavaSignature('android/inputmethodservice/InputMethodService$InputMethodSessionImpl')]
JInputMethodService_InputMethodSessionImpl = interface(JAbstractInputMethodService_AbstractInputMethodSessionImpl)
['{50561A9E-9EDD-44EC-9690-3CD9EA7E11EA}']
  {Methods}
  procedure appPrivateCommand(action: JString; data: JBundle); cdecl;
  procedure displayCompletions(completions: TJavaObjectArray<JCompletionInfo>); cdecl;
  procedure finishInput; cdecl;
  procedure toggleSoftInput(showFlags: Integer; hideFlags: Integer); cdecl;
  procedure updateCursor(newCursor: JRect); cdecl;
  procedure updateExtractedText(token: Integer; text: JExtractedText); cdecl;
  procedure updateSelection(oldSelStart: Integer; oldSelEnd: Integer; newSelStart: Integer; newSelEnd: Integer; candidatesStart: Integer; candidatesEnd: Integer); cdecl;
  procedure viewClicked(focusChanged: Boolean); cdecl;
end;
TJInputMethodService_InputMethodSessionImpl = class(TJavaGenericImport<JInputMethodService_InputMethodSessionImplClass, JInputMethodService_InputMethodSessionImpl>) end;

JKeyboardViewClass = interface(JViewClass)
['{42940038-508B-45EF-A693-06F5A4FD13A7}']
  {Methods}
  function init(context: JContext; attrs: JAttributeSet): JKeyboardView; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet; defStyle: Integer): JKeyboardView; cdecl; overload;
end;

[JavaSignature('android/inputmethodservice/KeyboardView')]
JKeyboardView = interface(JView)
['{E1D3273B-0921-4ADA-BF8E-6CDAB5D52F52}']
  {Methods}
  procedure closing; cdecl;
  function getKeyboard: JKeyboard; cdecl;
  function handleBack: Boolean; cdecl;
  procedure invalidateAllKeys; cdecl;
  procedure invalidateKey(keyIndex: Integer); cdecl;
  function isPreviewEnabled: Boolean; cdecl;
  function isProximityCorrectionEnabled: Boolean; cdecl;
  function isShifted: Boolean; cdecl;
  procedure onDetachedFromWindow; cdecl;
  procedure onDraw(canvas: JCanvas); cdecl;
  function onHoverEvent(event: JMotionEvent): Boolean; cdecl;
  procedure onMeasure(widthMeasureSpec: Integer; heightMeasureSpec: Integer); cdecl;
  procedure onSizeChanged(w: Integer; h: Integer; oldw: Integer; oldh: Integer); cdecl;
  function onTouchEvent(me: JMotionEvent): Boolean; cdecl;
  procedure setKeyboard(keyboard: JKeyboard); cdecl;
  procedure setOnKeyboardActionListener(listener: JKeyboardView_OnKeyboardActionListener); cdecl;
  procedure setPopupOffset(x: Integer; y: Integer); cdecl;
  procedure setPopupParent(v: JView); cdecl;
  procedure setPreviewEnabled(previewEnabled: Boolean); cdecl;
  procedure setProximityCorrectionEnabled(enabled: Boolean); cdecl;
  function setShifted(shifted: Boolean): Boolean; cdecl;
  procedure setVerticalCorrection(verticalOffset: Integer); cdecl;
end;
TJKeyboardView = class(TJavaGenericImport<JKeyboardViewClass, JKeyboardView>) end;

JInputMethodService_InputMethodImplClass = interface(JAbstractInputMethodService_AbstractInputMethodImplClass)
['{BEDEA638-BFD1-4AB1-8875-09804265A5FE}']
  {Methods}
  function init: JInputMethodService_InputMethodImpl; cdecl;
end;

[JavaSignature('android/inputmethodservice/InputMethodService$InputMethodImpl')]
JInputMethodService_InputMethodImpl = interface(JAbstractInputMethodService_AbstractInputMethodImpl)
['{84058E99-B0CE-4CE8-8B41-2723E697C4A9}']
  {Methods}
  procedure attachToken(token: JIBinder); cdecl;
  procedure bindInput(binding: JInputBinding); cdecl;
  procedure changeInputMethodSubtype(subtype: JInputMethodSubtype); cdecl;
  procedure hideSoftInput(flags: Integer; resultReceiver: JResultReceiver); cdecl;
  procedure restartInput(ic: JInputConnection; attribute: JEditorInfo); cdecl;
  procedure showSoftInput(flags: Integer; resultReceiver: JResultReceiver); cdecl;
  procedure startInput(ic: JInputConnection; attribute: JEditorInfo); cdecl;
  procedure unbindInput; cdecl;
end;
TJInputMethodService_InputMethodImpl = class(TJavaGenericImport<JInputMethodService_InputMethodImplClass, JInputMethodService_InputMethodImpl>) end;

JInputMethodService_InsetsClass = interface(JObjectClass)
['{F0ED3F1A-3152-4BCF-926B-57CBC40FCD5D}']
  {Property Methods}
  function _GetTOUCHABLE_INSETS_CONTENT: Integer;
  function _GetTOUCHABLE_INSETS_FRAME: Integer;
  function _GetTOUCHABLE_INSETS_REGION: Integer;
  function _GetTOUCHABLE_INSETS_VISIBLE: Integer;
  {Methods}
  function init: JInputMethodService_Insets; cdecl;
  {Properties}
  property TOUCHABLE_INSETS_CONTENT: Integer read _GetTOUCHABLE_INSETS_CONTENT;
  property TOUCHABLE_INSETS_FRAME: Integer read _GetTOUCHABLE_INSETS_FRAME;
  property TOUCHABLE_INSETS_REGION: Integer read _GetTOUCHABLE_INSETS_REGION;
  property TOUCHABLE_INSETS_VISIBLE: Integer read _GetTOUCHABLE_INSETS_VISIBLE;
end;

[JavaSignature('android/inputmethodservice/InputMethodService$Insets')]
JInputMethodService_Insets = interface(JObject)
['{60F3EDB8-4C1B-411B-8466-7C849D07C475}']
  {Property Methods}
  function _GetcontentTopInsets: Integer;
  procedure _SetcontentTopInsets(Value: Integer);
  function _GettouchableInsets: Integer;
  procedure _SettouchableInsets(Value: Integer);
  function _GettouchableRegion: JRegion;
  function _GetvisibleTopInsets: Integer;
  procedure _SetvisibleTopInsets(Value: Integer);
  {Properties}
  property contentTopInsets: Integer read _GetcontentTopInsets write _SetcontentTopInsets;
  property touchableInsets: Integer read _GettouchableInsets write _SettouchableInsets;
  property touchableRegion: JRegion read _GettouchableRegion;
  property visibleTopInsets: Integer read _GetvisibleTopInsets write _SetvisibleTopInsets;
end;
TJInputMethodService_Insets = class(TJavaGenericImport<JInputMethodService_InsetsClass, JInputMethodService_Insets>) end;

JKeyboardView_OnKeyboardActionListenerClass = interface(IJavaClass)
['{6236A842-C447-47FA-8A41-5EADA3BDCF4B}']
end;

[JavaSignature('android/inputmethodservice/KeyboardView$OnKeyboardActionListener')]
JKeyboardView_OnKeyboardActionListener = interface(IJavaInstance)
['{5EA363D7-B77D-49A3-B54B-5DA8FAE2CD75}']
  {Methods}
  procedure onKey(primaryCode: Integer; keyCodes: TJavaArray<Integer>); cdecl;
  procedure onPress(primaryCode: Integer); cdecl;
  procedure onRelease(primaryCode: Integer); cdecl;
  procedure onText(text: JCharSequence); cdecl;
  procedure swipeDown; cdecl;
  procedure swipeLeft; cdecl;
  procedure swipeRight; cdecl;
  procedure swipeUp; cdecl;
end;
TJKeyboardView_OnKeyboardActionListener = class(TJavaGenericImport<JKeyboardView_OnKeyboardActionListenerClass, JKeyboardView_OnKeyboardActionListener>) end;

JKeyboardClass = interface(JObjectClass)
['{91B42F2E-9703-4C17-AAEB-27B8D1E33E7C}']
  {Property Methods}
  function _GetEDGE_BOTTOM: Integer;
  function _GetEDGE_LEFT: Integer;
  function _GetEDGE_RIGHT: Integer;
  function _GetEDGE_TOP: Integer;
  function _GetKEYCODE_ALT: Integer;
  function _GetKEYCODE_CANCEL: Integer;
  function _GetKEYCODE_DELETE: Integer;
  function _GetKEYCODE_DONE: Integer;
  function _GetKEYCODE_MODE_CHANGE: Integer;
  function _GetKEYCODE_SHIFT: Integer;
  {Methods}
  function init(context: JContext; xmlLayoutResId: Integer): JKeyboard; cdecl; overload;
  function init(context: JContext; xmlLayoutResId: Integer; modeId: Integer; width: Integer; height: Integer): JKeyboard; cdecl; overload;
  function init(context: JContext; xmlLayoutResId: Integer; modeId: Integer): JKeyboard; cdecl; overload;
  function init(context: JContext; layoutTemplateResId: Integer; characters: JCharSequence; columns: Integer; horizontalPadding: Integer): JKeyboard; cdecl; overload;
  {Properties}
  property EDGE_BOTTOM: Integer read _GetEDGE_BOTTOM;
  property EDGE_LEFT: Integer read _GetEDGE_LEFT;
  property EDGE_RIGHT: Integer read _GetEDGE_RIGHT;
  property EDGE_TOP: Integer read _GetEDGE_TOP;
  property KEYCODE_ALT: Integer read _GetKEYCODE_ALT;
  property KEYCODE_CANCEL: Integer read _GetKEYCODE_CANCEL;
  property KEYCODE_DELETE: Integer read _GetKEYCODE_DELETE;
  property KEYCODE_DONE: Integer read _GetKEYCODE_DONE;
  property KEYCODE_MODE_CHANGE: Integer read _GetKEYCODE_MODE_CHANGE;
  property KEYCODE_SHIFT: Integer read _GetKEYCODE_SHIFT;
end;

[JavaSignature('android/inputmethodservice/Keyboard')]
JKeyboard = interface(JObject)
['{1E09EB21-68A8-4014-B9B5-3533B484F606}']
  {Methods}
  function getHeight: Integer; cdecl;
  function getKeys: JList; cdecl;
  function getMinWidth: Integer; cdecl;
  function getModifierKeys: JList; cdecl;
  function getNearestKeys(x: Integer; y: Integer): TJavaArray<Integer>; cdecl;
  function getShiftKeyIndex: Integer; cdecl;
  function isShifted: Boolean; cdecl;
  function setShifted(shiftState: Boolean): Boolean; cdecl;
end;
TJKeyboard = class(TJavaGenericImport<JKeyboardClass, JKeyboard>) end;

JKeyboard_RowClass = interface(JObjectClass)
['{70EFB31D-C849-4E18-B877-4FA6FF21C9C0}']
  {Methods}
  function init(parent: JKeyboard): JKeyboard_Row; cdecl; overload;
  function init(res: JResources; parent: JKeyboard; parser: JXmlResourceParser): JKeyboard_Row; cdecl; overload;
end;

[JavaSignature('android/inputmethodservice/Keyboard$Row')]
JKeyboard_Row = interface(JObject)
['{1D90FE86-1727-42D8-8C2A-1AF5306F4208}']
  {Property Methods}
  function _GetdefaultHeight: Integer;
  procedure _SetdefaultHeight(Value: Integer);
  function _GetdefaultHorizontalGap: Integer;
  procedure _SetdefaultHorizontalGap(Value: Integer);
  function _GetdefaultWidth: Integer;
  procedure _SetdefaultWidth(Value: Integer);
  function _Getmode: Integer;
  procedure _Setmode(Value: Integer);
  function _GetrowEdgeFlags: Integer;
  procedure _SetrowEdgeFlags(Value: Integer);
  function _GetverticalGap: Integer;
  procedure _SetverticalGap(Value: Integer);
  {Properties}
  property defaultHeight: Integer read _GetdefaultHeight write _SetdefaultHeight;
  property defaultHorizontalGap: Integer read _GetdefaultHorizontalGap write _SetdefaultHorizontalGap;
  property defaultWidth: Integer read _GetdefaultWidth write _SetdefaultWidth;
  property mode: Integer read _Getmode write _Setmode;
  property rowEdgeFlags: Integer read _GetrowEdgeFlags write _SetrowEdgeFlags;
  property verticalGap: Integer read _GetverticalGap write _SetverticalGap;
end;
TJKeyboard_Row = class(TJavaGenericImport<JKeyboard_RowClass, JKeyboard_Row>) end;

JInputMethodServiceClass = interface(JAbstractInputMethodServiceClass)
['{7DE4CFF2-4B59-44A3-A3CA-1870175FF807}']
  {Property Methods}
  function _GetBACK_DISPOSITION_DEFAULT: Integer;
  function _GetBACK_DISPOSITION_WILL_DISMISS: Integer;
  function _GetBACK_DISPOSITION_WILL_NOT_DISMISS: Integer;
  {Methods}
  function init: JInputMethodService; cdecl;
  {Properties}
  property BACK_DISPOSITION_DEFAULT: Integer read _GetBACK_DISPOSITION_DEFAULT;
  property BACK_DISPOSITION_WILL_DISMISS: Integer read _GetBACK_DISPOSITION_WILL_DISMISS;
  property BACK_DISPOSITION_WILL_NOT_DISMISS: Integer read _GetBACK_DISPOSITION_WILL_NOT_DISMISS;
end;

[JavaSignature('android/inputmethodservice/InputMethodService')]
JInputMethodService = interface(JAbstractInputMethodService)
['{A0954F9A-CF37-4E9B-943F-AAA9A6828992}']
  {Methods}
  function enableHardwareAcceleration: Boolean; cdecl;
  function getBackDisposition: Integer; cdecl;
  function getCandidatesHiddenVisibility: Integer; cdecl;
  function getCurrentInputBinding: JInputBinding; cdecl;
  function getCurrentInputConnection: JInputConnection; cdecl;
  function getCurrentInputEditorInfo: JEditorInfo; cdecl;
  function getCurrentInputStarted: Boolean; cdecl;
  function getLayoutInflater: JLayoutInflater; cdecl;
  function getMaxWidth: Integer; cdecl;
  function getTextForImeAction(imeOptions: Integer): JCharSequence; cdecl;
  function getWindow: JDialog; cdecl;
  procedure hideStatusIcon; cdecl;
  procedure hideWindow; cdecl;
  function isExtractViewShown: Boolean; cdecl;
  function isFullscreenMode: Boolean; cdecl;
  function isInputViewShown: Boolean; cdecl;
  function isShowInputRequested: Boolean; cdecl;
  procedure onAppPrivateCommand(action: JString; data: JBundle); cdecl;
  procedure onBindInput; cdecl;
  procedure onComputeInsets(outInsets: JInputMethodService_Insets); cdecl;
  procedure onConfigurationChanged(newConfig: JConfiguration); cdecl;
  procedure onConfigureWindow(win: JWindow; isFullscreen: Boolean; isCandidatesOnly: Boolean); cdecl;
  procedure onCreate; cdecl;
  function onCreateCandidatesView: JView; cdecl;
  function onCreateExtractTextView: JView; cdecl;
  function onCreateInputMethodInterface: JAbstractInputMethodService_AbstractInputMethodImpl; cdecl;
  function onCreateInputMethodSessionInterface: JAbstractInputMethodService_AbstractInputMethodSessionImpl; cdecl;
  function onCreateInputView: JView; cdecl;
  procedure onDestroy; cdecl;
  procedure onDisplayCompletions(completions: TJavaObjectArray<JCompletionInfo>); cdecl;
  function onEvaluateFullscreenMode: Boolean; cdecl;
  function onEvaluateInputViewShown: Boolean; cdecl;
  function onExtractTextContextMenuItem(id: Integer): Boolean; cdecl;
  procedure onExtractedCursorMovement(dx: Integer; dy: Integer); cdecl;
  procedure onExtractedSelectionChanged(start: Integer; end_: Integer); cdecl;
  procedure onExtractedTextClicked; cdecl;
  procedure onExtractingInputChanged(ei: JEditorInfo); cdecl;
  procedure onFinishCandidatesView(finishingInput: Boolean); cdecl;
  procedure onFinishInput; cdecl;
  procedure onFinishInputView(finishingInput: Boolean); cdecl;
  function onGenericMotionEvent(event: JMotionEvent): Boolean; cdecl;
  procedure onInitializeInterface; cdecl;
  function onKeyDown(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  function onKeyLongPress(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  function onKeyMultiple(keyCode: Integer; count: Integer; event: JKeyEvent): Boolean; cdecl;
  function onKeyUp(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  function onShowInputRequested(flags: Integer; configChange: Boolean): Boolean; cdecl;
  procedure onStartCandidatesView(info: JEditorInfo; restarting: Boolean); cdecl;
  procedure onStartInput(attribute: JEditorInfo; restarting: Boolean); cdecl;
  procedure onStartInputView(info: JEditorInfo; restarting: Boolean); cdecl;
  function onTrackballEvent(event: JMotionEvent): Boolean; cdecl;
  procedure onUnbindInput; cdecl;
  procedure onUpdateCursor(newCursor: JRect); cdecl;
  procedure onUpdateExtractedText(token: Integer; text: JExtractedText); cdecl;
  procedure onUpdateExtractingViews(ei: JEditorInfo); cdecl;
  procedure onUpdateExtractingVisibility(ei: JEditorInfo); cdecl;
  procedure onUpdateSelection(oldSelStart: Integer; oldSelEnd: Integer; newSelStart: Integer; newSelEnd: Integer; candidatesStart: Integer; candidatesEnd: Integer); cdecl;
  procedure onViewClicked(focusChanged: Boolean); cdecl;
  procedure onWindowHidden; cdecl;
  procedure onWindowShown; cdecl;
  procedure requestHideSelf(flags: Integer); cdecl;
  function sendDefaultEditorAction(fromEnterKey: Boolean): Boolean; cdecl;
  procedure sendDownUpKeyEvents(keyEventCode: Integer); cdecl;
  procedure sendKeyChar(charCode: Char); cdecl;
  procedure setBackDisposition(disposition: Integer); cdecl;
  procedure setCandidatesView(view: JView); cdecl;
  procedure setCandidatesViewShown(shown: Boolean); cdecl;
  procedure setExtractView(view: JView); cdecl;
  procedure setExtractViewShown(shown: Boolean); cdecl;
  procedure setInputView(view: JView); cdecl;
  procedure setTheme(theme: Integer); cdecl;
  procedure showStatusIcon(iconResId: Integer); cdecl;
  procedure showWindow(showInput: Boolean); cdecl;
  procedure switchInputMethod(id: JString); cdecl;
  procedure updateFullscreenMode; cdecl;
  procedure updateInputViewShown; cdecl;
end;
TJInputMethodService = class(TJavaGenericImport<JInputMethodServiceClass, JInputMethodService>) end;




implementation

begin

end.


