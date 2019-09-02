{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit Androidapi.JNI.Widget;

interface

uses 
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
  Androidapi.JNI.Net,
  Androidapi.JNI.Util,
  Androidapi.JNI.GraphicsContentViewText;

type

  {Class forward declarations}
  JTextView = interface;//android.widget.TextView
  JEditText = interface;//android.widget.EditText
  JAbsListView_OnScrollListener = interface;//android.widget.AbsListView$OnScrollListener
  JScroller = interface;//android.widget.Scroller
  JButton = interface;//android.widget.Button
  JAdapterView = interface;//android.widget.AdapterView
  JAbsListView = interface;//android.widget.AbsListView
  JListView = interface;//android.widget.ListView
  JFrameLayout = interface;//android.widget.FrameLayout
  JScrollView = interface;//android.widget.ScrollView
  JTextView_BufferType = interface;//android.widget.TextView$BufferType
  JAdapter = interface;//android.widget.Adapter
  JListAdapter = interface;//android.widget.ListAdapter
  JLinearLayout_LayoutParams = interface;//android.widget.LinearLayout$LayoutParams
  JImageView_ScaleType = interface;//android.widget.ImageView$ScaleType
  JRemoteViews = interface;//android.widget.RemoteViews
  JAbsListView_LayoutParams = interface;//android.widget.AbsListView$LayoutParams
  JAbsListView_RecyclerListener = interface;//android.widget.AbsListView$RecyclerListener
  JFrameLayout_LayoutParams = interface;//android.widget.FrameLayout$LayoutParams
  JSpinnerAdapter = interface;//android.widget.SpinnerAdapter
  JAdapterView_OnItemSelectedListener = interface;//android.widget.AdapterView$OnItemSelectedListener
  JAdapterView_OnItemLongClickListener = interface;//android.widget.AdapterView$OnItemLongClickListener
  JAbsoluteLayout = interface;//android.widget.AbsoluteLayout
  JAdapterView_OnItemClickListener = interface;//android.widget.AdapterView$OnItemClickListener
  JAbsListView_MultiChoiceModeListener = interface;//android.widget.AbsListView$MultiChoiceModeListener
  JPopupWindow = interface;//android.widget.PopupWindow
  JPopupWindow_OnDismissListener = interface;//android.widget.PopupWindow$OnDismissListener
  JTextView_OnEditorActionListener = interface;//android.widget.TextView$OnEditorActionListener
  JImageView = interface;//android.widget.ImageView
  JLinearLayout = interface;//android.widget.LinearLayout

JTextViewClass = interface(JViewClass)
['{FF2B8D3E-17E8-4F9A-AEC2-B2E39AB20CDE}']
  {Methods}
  function init(context: JContext): JTextView; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet): JTextView; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet; defStyle: Integer): JTextView; cdecl; overload;
  function getTextColor(context: JContext; attrs: JTypedArray; def: Integer): Integer; cdecl;
  function getTextColors(context: JContext; attrs: JTypedArray): JColorStateList; cdecl; overload;
end;

[JavaSignature('android/widget/TextView')]
JTextView = interface(JView)
['{45436351-10E4-41C0-ADB2-099CB38BA534}']
  {Methods}
  procedure addTextChangedListener(watcher: JTextWatcher); cdecl;
  procedure append(text: JCharSequence); cdecl; overload;
  procedure append(text: JCharSequence; start: Integer; end_: Integer); cdecl; overload;
  procedure beginBatchEdit; cdecl;
  function bringPointIntoView(offset: Integer): Boolean; cdecl;
  procedure cancelLongPress; cdecl;
  procedure clearComposingText; cdecl;
  procedure computeScroll; cdecl;
  procedure debug(depth: Integer); cdecl;
  function didTouchFocusSelect: Boolean; cdecl;
  procedure endBatchEdit; cdecl;
  function extractText(request: JExtractedTextRequest; outText: JExtractedText): Boolean; cdecl;
  procedure findViewsWithText(outViews: JArrayList; searched: JCharSequence; flags: Integer); cdecl;
  function getAutoLinkMask: Integer; cdecl;
  function getBaseline: Integer; cdecl;
  function getCompoundDrawablePadding: Integer; cdecl;
  function getCompoundDrawables: TJavaObjectArray<JDrawable>; cdecl;
  function getCompoundDrawablesRelative: TJavaObjectArray<JDrawable>; cdecl;
  function getCompoundPaddingBottom: Integer; cdecl;
  function getCompoundPaddingEnd: Integer; cdecl;
  function getCompoundPaddingLeft: Integer; cdecl;
  function getCompoundPaddingRight: Integer; cdecl;
  function getCompoundPaddingStart: Integer; cdecl;
  function getCompoundPaddingTop: Integer; cdecl;
  function getCurrentHintTextColor: Integer; cdecl;
  function getCurrentTextColor: Integer; cdecl;
  function getCustomSelectionActionModeCallback: JActionMode_Callback; cdecl;
  function getEditableText: JEditable; cdecl;
  function getEllipsize: JTextUtils_TruncateAt; cdecl;
  function getError: JCharSequence; cdecl;
  function getExtendedPaddingBottom: Integer; cdecl;
  function getExtendedPaddingTop: Integer; cdecl;
  function getFilters: TJavaObjectArray<JInputFilter>; cdecl;
  procedure getFocusedRect(r: JRect); cdecl;
  function getFreezesText: Boolean; cdecl;
  function getGravity: Integer; cdecl;
  function getHighlightColor: Integer; cdecl;
  function getHint: JCharSequence; cdecl;
  function getHintTextColors: JColorStateList; cdecl;
  function getImeActionId: Integer; cdecl;
  function getImeActionLabel: JCharSequence; cdecl;
  function getImeOptions: Integer; cdecl;
  function getIncludeFontPadding: Boolean; cdecl;
  function getInputExtras(create: Boolean): JBundle; cdecl;
  function getInputType: Integer; cdecl;
  function getKeyListener: JKeyListener; cdecl;
  function getLayout: JLayout; cdecl;
  function getLineBounds(line: Integer; bounds: JRect): Integer; cdecl;
  function getLineCount: Integer; cdecl;
  function getLineHeight: Integer; cdecl;
  function getLineSpacingExtra: Single; cdecl;
  function getLineSpacingMultiplier: Single; cdecl;
  function getLinkTextColors: JColorStateList; cdecl;
  function getLinksClickable: Boolean; cdecl;
  function getMarqueeRepeatLimit: Integer; cdecl;
  function getMaxEms: Integer; cdecl;
  function getMaxHeight: Integer; cdecl;
  function getMaxLines: Integer; cdecl;
  function getMaxWidth: Integer; cdecl;
  function getMinEms: Integer; cdecl;
  function getMinHeight: Integer; cdecl;
  function getMinLines: Integer; cdecl;
  function getMinWidth: Integer; cdecl;
  function getOffsetForPosition(x: Single; y: Single): Integer; cdecl;
  function getPaint: JTextPaint; cdecl;
  function getPaintFlags: Integer; cdecl;
  function getPrivateImeOptions: JString; cdecl;
  function getSelectionEnd: Integer; cdecl;
  function getSelectionStart: Integer; cdecl;
  function getShadowColor: Integer; cdecl;
  function getShadowDx: Single; cdecl;
  function getShadowDy: Single; cdecl;
  function getShadowRadius: Single; cdecl;
  function getText: JCharSequence; cdecl;
  function getTextColors: JColorStateList; cdecl; overload;
  function getTextLocale: JLocale; cdecl;
  function getTextScaleX: Single; cdecl;
  function getTextSize: Single; cdecl;
  function getTotalPaddingBottom: Integer; cdecl;
  function getTotalPaddingEnd: Integer; cdecl;
  function getTotalPaddingLeft: Integer; cdecl;
  function getTotalPaddingRight: Integer; cdecl;
  function getTotalPaddingStart: Integer; cdecl;
  function getTotalPaddingTop: Integer; cdecl;
  function getTransformationMethod: JTransformationMethod; cdecl;
  function getTypeface: JTypeface; cdecl;
  function getUrls: TJavaObjectArray<JURLSpan>; cdecl;
  function hasOverlappingRendering: Boolean; cdecl;
  function hasSelection: Boolean; cdecl;
  procedure invalidateDrawable(drawable: JDrawable); cdecl;
  function isCursorVisible: Boolean; cdecl;
  function isInputMethodTarget: Boolean; cdecl;
  function isSuggestionsEnabled: Boolean; cdecl;
  function isTextSelectable: Boolean; cdecl;
  procedure jumpDrawablesToCurrentState; cdecl;
  function length: Integer; cdecl;
  function moveCursorToVisibleOffset: Boolean; cdecl;
  procedure onBeginBatchEdit; cdecl;
  function onCheckIsTextEditor: Boolean; cdecl;
  procedure onCommitCompletion(text: JCompletionInfo); cdecl;
  procedure onCommitCorrection(info: JCorrectionInfo); cdecl;
  function onCreateInputConnection(outAttrs: JEditorInfo): JInputConnection; cdecl;
  function onDragEvent(event: JDragEvent): Boolean; cdecl;
  procedure onEditorAction(actionCode: Integer); cdecl;
  procedure onEndBatchEdit; cdecl;
  procedure onFinishTemporaryDetach; cdecl;
  function onGenericMotionEvent(event: JMotionEvent): Boolean; cdecl;
  function onKeyDown(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  function onKeyMultiple(keyCode: Integer; repeatCount: Integer; event: JKeyEvent): Boolean; cdecl;
  function onKeyPreIme(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  function onKeyShortcut(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  function onKeyUp(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  function onPreDraw: Boolean; cdecl;
  function onPrivateIMECommand(action: JString; data: JBundle): Boolean; cdecl;
  procedure onRestoreInstanceState(state: JParcelable); cdecl;
  procedure onRtlPropertiesChanged(layoutDirection: Integer); cdecl;
  function onSaveInstanceState: JParcelable; cdecl;
  procedure onScreenStateChanged(screenState: Integer); cdecl;
  procedure onStartTemporaryDetach; cdecl;
  function onTextContextMenuItem(id: Integer): Boolean; cdecl;
  function onTouchEvent(event: JMotionEvent): Boolean; cdecl;
  function onTrackballEvent(event: JMotionEvent): Boolean; cdecl;
  procedure onWindowFocusChanged(hasWindowFocus: Boolean); cdecl;
  function performLongClick: Boolean; cdecl;
  procedure removeTextChangedListener(watcher: JTextWatcher); cdecl;
  procedure sendAccessibilityEvent(eventType: Integer); cdecl;
  procedure setAllCaps(allCaps: Boolean); cdecl;
  procedure setAutoLinkMask(mask: Integer); cdecl;
  procedure setCompoundDrawablePadding(pad: Integer); cdecl;
  procedure setCompoundDrawables(left: JDrawable; top: JDrawable; right: JDrawable; bottom: JDrawable); cdecl;
  procedure setCompoundDrawablesRelative(start: JDrawable; top: JDrawable; end_: JDrawable; bottom: JDrawable); cdecl;
  procedure setCompoundDrawablesRelativeWithIntrinsicBounds(start: Integer; top: Integer; end_: Integer; bottom: Integer); cdecl; overload;
  procedure setCompoundDrawablesRelativeWithIntrinsicBounds(start: JDrawable; top: JDrawable; end_: JDrawable; bottom: JDrawable); cdecl; overload;
  procedure setCompoundDrawablesWithIntrinsicBounds(left: Integer; top: Integer; right: Integer; bottom: Integer); cdecl; overload;
  procedure setCompoundDrawablesWithIntrinsicBounds(left: JDrawable; top: JDrawable; right: JDrawable; bottom: JDrawable); cdecl; overload;
  procedure setCursorVisible(visible: Boolean); cdecl;
  procedure setCustomSelectionActionModeCallback(actionModeCallback: JActionMode_Callback); cdecl;
  procedure setEditableFactory(factory: JEditable_Factory); cdecl;
  procedure setEllipsize(where: JTextUtils_TruncateAt); cdecl;
  procedure setEms(ems: Integer); cdecl;
  procedure setEnabled(enabled: Boolean); cdecl;
  procedure setError(error: JCharSequence); cdecl; overload;
  procedure setError(error: JCharSequence; icon: JDrawable); cdecl; overload;
  procedure setExtractedText(text: JExtractedText); cdecl;
  procedure setFilters(filters: TJavaObjectArray<JInputFilter>); cdecl;
  procedure setFreezesText(freezesText: Boolean); cdecl;
  procedure setGravity(gravity: Integer); cdecl;
  procedure setHeight(pixels: Integer); cdecl;
  procedure setHighlightColor(color: Integer); cdecl;
  procedure setHint(hint: JCharSequence); cdecl; overload;
  procedure setHint(resid: Integer); cdecl; overload;
  procedure setHintTextColor(color: Integer); cdecl; overload;
  procedure setHintTextColor(colors: JColorStateList); cdecl; overload;
  procedure setHorizontallyScrolling(whether: Boolean); cdecl;
  procedure setImeActionLabel(label_: JCharSequence; actionId: Integer); cdecl;
  procedure setImeOptions(imeOptions: Integer); cdecl;
  procedure setIncludeFontPadding(includepad: Boolean); cdecl;
  procedure setInputExtras(xmlResId: Integer); cdecl;
  procedure setInputType(type_: Integer); cdecl;
  procedure setKeyListener(input: JKeyListener); cdecl;
  procedure setLineSpacing(add: Single; mult: Single); cdecl;
  procedure setLines(lines: Integer); cdecl;
  procedure setLinkTextColor(color: Integer); cdecl; overload;
  procedure setLinkTextColor(colors: JColorStateList); cdecl; overload;
  procedure setLinksClickable(whether: Boolean); cdecl;
  procedure setMarqueeRepeatLimit(marqueeLimit: Integer); cdecl;
  procedure setMaxEms(maxems: Integer); cdecl;
  procedure setMaxHeight(maxHeight: Integer); cdecl;
  procedure setMaxLines(maxlines: Integer); cdecl;
  procedure setMaxWidth(maxpixels: Integer); cdecl;
  procedure setMinEms(minems: Integer); cdecl;
  procedure setMinHeight(minHeight: Integer); cdecl;
  procedure setMinLines(minlines: Integer); cdecl;
  procedure setMinWidth(minpixels: Integer); cdecl;
  procedure setOnEditorActionListener(l: JTextView_OnEditorActionListener); cdecl;
  procedure setPadding(left: Integer; top: Integer; right: Integer; bottom: Integer); cdecl;
  procedure setPaddingRelative(start: Integer; top: Integer; end_: Integer; bottom: Integer); cdecl;
  procedure setPaintFlags(flags: Integer); cdecl;
  procedure setPrivateImeOptions(type_: JString); cdecl;
  procedure setRawInputType(type_: Integer); cdecl;
  procedure setScroller(s: JScroller); cdecl;
  procedure setSelectAllOnFocus(selectAllOnFocus: Boolean); cdecl;
  procedure setSelected(selected: Boolean); cdecl;
  procedure setShadowLayer(radius: Single; dx: Single; dy: Single; color: Integer); cdecl;
  procedure setSingleLine; cdecl; overload;
  procedure setSingleLine(singleLine: Boolean); cdecl; overload;
  procedure setSpannableFactory(factory: JSpannable_Factory); cdecl;
  procedure setText(text: JCharSequence); cdecl; overload;
  procedure setText(text: JCharSequence; type_: JTextView_BufferType); cdecl; overload;
  procedure setText(text: TJavaArray<Char>; start: Integer; len: Integer); cdecl; overload;
  procedure setText(resid: Integer); cdecl; overload;
  procedure setText(resid: Integer; type_: JTextView_BufferType); cdecl; overload;
  procedure setTextAppearance(context: JContext; resid: Integer); cdecl;
  procedure setTextColor(color: Integer); cdecl; overload;
  procedure setTextColor(colors: JColorStateList); cdecl; overload;
  procedure setTextIsSelectable(selectable: Boolean); cdecl;
  procedure setTextKeepState(text: JCharSequence); cdecl; overload;
  procedure setTextKeepState(text: JCharSequence; type_: JTextView_BufferType); cdecl; overload;
  procedure setTextLocale(locale: JLocale); cdecl;
  procedure setTextScaleX(size: Single); cdecl;
  procedure setTextSize(size: Single); cdecl; overload;
  procedure setTextSize(unit_: Integer; size: Single); cdecl; overload;
  procedure setTransformationMethod(method: JTransformationMethod); cdecl;
  procedure setTypeface(tf: JTypeface; style: Integer); cdecl; overload;
  procedure setTypeface(tf: JTypeface); cdecl; overload;
  procedure setWidth(pixels: Integer); cdecl;
end;
TJTextView = class(TJavaGenericImport<JTextViewClass, JTextView>) end;

JEditTextClass = interface(JTextViewClass)
['{A1E1255C-8FC9-449A-A886-57ABA315C3E3}']
  {Methods}
  function init(context: JContext): JEditText; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet): JEditText; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet; defStyle: Integer): JEditText; cdecl; overload;
end;

[JavaSignature('android/widget/EditText')]
JEditText = interface(JTextView)
['{53E7BC41-9ED8-4863-B9A3-B8D23DA1794F}']
  {Methods}
  procedure extendSelection(index: Integer); cdecl;
  function getText: JEditable; cdecl;
  procedure selectAll; cdecl;
  procedure setEllipsize(ellipsis: JTextUtils_TruncateAt); cdecl;
  procedure setSelection(start: Integer; stop: Integer); cdecl; overload;
  procedure setSelection(index: Integer); cdecl; overload;
  procedure setText(text: JCharSequence; type_: JTextView_BufferType); cdecl;
end;
TJEditText = class(TJavaGenericImport<JEditTextClass, JEditText>) end;

JAbsListView_OnScrollListenerClass = interface(IJavaClass)
['{479243DC-B7C6-4219-A4EA-4DB71A796E0D}']
  {Property Methods}
  function _GetSCROLL_STATE_FLING: Integer;
  function _GetSCROLL_STATE_IDLE: Integer;
  function _GetSCROLL_STATE_TOUCH_SCROLL: Integer;
  {Properties}
  property SCROLL_STATE_FLING: Integer read _GetSCROLL_STATE_FLING;
  property SCROLL_STATE_IDLE: Integer read _GetSCROLL_STATE_IDLE;
  property SCROLL_STATE_TOUCH_SCROLL: Integer read _GetSCROLL_STATE_TOUCH_SCROLL;
end;

[JavaSignature('android/widget/AbsListView$OnScrollListener')]
JAbsListView_OnScrollListener = interface(IJavaInstance)
['{9B3AA66F-2D52-4E66-BF3B-4664B89919F7}']
  {Methods}
  procedure onScroll(view: JAbsListView; firstVisibleItem: Integer; visibleItemCount: Integer; totalItemCount: Integer); cdecl;
  procedure onScrollStateChanged(view: JAbsListView; scrollState: Integer); cdecl;
end;
TJAbsListView_OnScrollListener = class(TJavaGenericImport<JAbsListView_OnScrollListenerClass, JAbsListView_OnScrollListener>) end;

JScrollerClass = interface(JObjectClass)
['{B6534774-81A9-4C48-BB64-97570118163B}']
  {Methods}
  function init(context: JContext): JScroller; cdecl;
end;

[JavaSignature('android/widget/Scroller')]
JScroller = interface(JObject)
['{35B1FBEE-2281-4808-B971-93DBCCA5E7F2}']
  {Methods}
  procedure abortAnimation; cdecl;
  function computeScrollOffset: Boolean; cdecl;
  procedure extendDuration(extend: Integer); cdecl;
  procedure fling(startX: Integer; startY: Integer; velocityX: Integer; velocityY: Integer; minX: Integer; maxX: Integer; minY: Integer; maxY: Integer); cdecl;
  procedure forceFinished(finished: Boolean); cdecl;
  function getCurrVelocity: Single; cdecl;
  function getCurrX: Integer; cdecl;
  function getCurrY: Integer; cdecl;
  function getDuration: Integer; cdecl;
  function getFinalX: Integer; cdecl;
  function getFinalY: Integer; cdecl;
  function getStartX: Integer; cdecl;
  function getStartY: Integer; cdecl;
  function isFinished: Boolean; cdecl;
  procedure setFinalX(newX: Integer); cdecl;
  procedure setFinalY(newY: Integer); cdecl;
  procedure setFriction(friction: Single); cdecl;
  procedure startScroll(startX: Integer; startY: Integer; dx: Integer; dy: Integer); cdecl; overload;
  procedure startScroll(startX: Integer; startY: Integer; dx: Integer; dy: Integer; duration: Integer); cdecl; overload;
  function timePassed: Integer; cdecl;
end;
TJScroller = class(TJavaGenericImport<JScrollerClass, JScroller>) end;

JButtonClass = interface(JTextViewClass)
['{84EB5039-0F0A-420F-8AAA-1454DE4BC213}']
  {Methods}
  function init(context: JContext): JButton; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet): JButton; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet; defStyle: Integer): JButton; cdecl; overload;
end;

[JavaSignature('android/widget/Button')]
JButton = interface(JTextView)
['{72BF0D65-7D16-4B9D-B6E7-5CAF786278CC}']
end;
TJButton = class(TJavaGenericImport<JButtonClass, JButton>) end;

JAdapterViewClass = interface(JViewGroupClass)
['{6490B102-5823-45D6-A2C4-E7211652D552}']
  {Property Methods}
  function _GetINVALID_POSITION: Integer;
  function _GetINVALID_ROW_ID: Int64;
  function _GetITEM_VIEW_TYPE_HEADER_OR_FOOTER: Integer;
  function _GetITEM_VIEW_TYPE_IGNORE: Integer;
  {Methods}
  function init(context: JContext): JAdapterView; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet): JAdapterView; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet; defStyle: Integer): JAdapterView; cdecl; overload;
  {Properties}
  property INVALID_POSITION: Integer read _GetINVALID_POSITION;
  property INVALID_ROW_ID: Int64 read _GetINVALID_ROW_ID;
  property ITEM_VIEW_TYPE_HEADER_OR_FOOTER: Integer read _GetITEM_VIEW_TYPE_HEADER_OR_FOOTER;
  property ITEM_VIEW_TYPE_IGNORE: Integer read _GetITEM_VIEW_TYPE_IGNORE;
end;

[JavaSignature('android/widget/AdapterView')]
JAdapterView = interface(JViewGroup)
['{7A567A58-E436-4C18-94B6-DCE4ECB35FDB}']
  {Methods}
  procedure addView(child: JView); cdecl; overload;
  procedure addView(child: JView; index: Integer); cdecl; overload;
  procedure addView(child: JView; params: JViewGroup_LayoutParams); cdecl; overload;
  procedure addView(child: JView; index: Integer; params: JViewGroup_LayoutParams); cdecl; overload;
  function getAdapter: JAdapter; cdecl;
  function getCount: Integer; cdecl;
  function getEmptyView: JView; cdecl;
  function getFirstVisiblePosition: Integer; cdecl;
  function getItemAtPosition(position: Integer): JObject; cdecl;
  function getItemIdAtPosition(position: Integer): Int64; cdecl;
  function getLastVisiblePosition: Integer; cdecl;
  function getOnItemClickListener: JAdapterView_OnItemClickListener; cdecl;
  function getOnItemLongClickListener: JAdapterView_OnItemLongClickListener; cdecl;
  function getOnItemSelectedListener: JAdapterView_OnItemSelectedListener; cdecl;
  function getPositionForView(view: JView): Integer; cdecl;
  function getSelectedItem: JObject; cdecl;
  function getSelectedItemId: Int64; cdecl;
  function getSelectedItemPosition: Integer; cdecl;
  function getSelectedView: JView; cdecl;
  function performItemClick(view: JView; position: Integer; id: Int64): Boolean; cdecl;
  procedure removeAllViews; cdecl;
  procedure removeView(child: JView); cdecl;
  procedure removeViewAt(index: Integer); cdecl;
  procedure setAdapter(adapter: JAdapter); cdecl;
  procedure setEmptyView(emptyView: JView); cdecl;
  procedure setFocusable(focusable: Boolean); cdecl;
  procedure setFocusableInTouchMode(focusable: Boolean); cdecl;
  procedure setOnClickListener(l: JView_OnClickListener); cdecl;
  procedure setOnItemClickListener(listener: JAdapterView_OnItemClickListener); cdecl;
  procedure setOnItemLongClickListener(listener: JAdapterView_OnItemLongClickListener); cdecl;
  procedure setOnItemSelectedListener(listener: JAdapterView_OnItemSelectedListener); cdecl;
  procedure setSelection(position: Integer); cdecl;
end;
TJAdapterView = class(TJavaGenericImport<JAdapterViewClass, JAdapterView>) end;

JAbsListViewClass = interface(JAdapterViewClass)
['{BAC07826-4DE7-4811-BCF8-A4E35AF7F784}']
  {Property Methods}
  function _GetCHOICE_MODE_MULTIPLE: Integer;
  function _GetCHOICE_MODE_MULTIPLE_MODAL: Integer;
  function _GetCHOICE_MODE_NONE: Integer;
  function _GetCHOICE_MODE_SINGLE: Integer;
  function _GetTRANSCRIPT_MODE_ALWAYS_SCROLL: Integer;
  function _GetTRANSCRIPT_MODE_DISABLED: Integer;
  function _GetTRANSCRIPT_MODE_NORMAL: Integer;
  {Methods}
  function init(context: JContext): JAbsListView; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet): JAbsListView; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet; defStyle: Integer): JAbsListView; cdecl; overload;
  {Properties}
  property CHOICE_MODE_MULTIPLE: Integer read _GetCHOICE_MODE_MULTIPLE;
  property CHOICE_MODE_MULTIPLE_MODAL: Integer read _GetCHOICE_MODE_MULTIPLE_MODAL;
  property CHOICE_MODE_NONE: Integer read _GetCHOICE_MODE_NONE;
  property CHOICE_MODE_SINGLE: Integer read _GetCHOICE_MODE_SINGLE;
  property TRANSCRIPT_MODE_ALWAYS_SCROLL: Integer read _GetTRANSCRIPT_MODE_ALWAYS_SCROLL;
  property TRANSCRIPT_MODE_DISABLED: Integer read _GetTRANSCRIPT_MODE_DISABLED;
  property TRANSCRIPT_MODE_NORMAL: Integer read _GetTRANSCRIPT_MODE_NORMAL;
end;

[JavaSignature('android/widget/AbsListView')]
JAbsListView = interface(JAdapterView)
['{F366DE71-7C40-4135-8C83-F7354D97A276}']
  {Methods}
  procedure addTouchables(views: JArrayList); cdecl;
  procedure afterTextChanged(s: JEditable); cdecl;
  procedure beforeTextChanged(s: JCharSequence; start: Integer; count: Integer; after: Integer); cdecl;
  function checkInputConnectionProxy(view: JView): Boolean; cdecl;
  procedure clearChoices; cdecl;
  procedure clearTextFilter; cdecl;
  procedure deferNotifyDataSetChanged; cdecl;
  procedure draw(canvas: JCanvas); cdecl;
  function generateLayoutParams(attrs: JAttributeSet): JAbsListView_LayoutParams; cdecl;
  function getCacheColorHint: Integer; cdecl;
  function getCheckedItemCount: Integer; cdecl;
  function getCheckedItemIds: TJavaArray<Int64>; cdecl;
  function getCheckedItemPosition: Integer; cdecl;
  function getCheckedItemPositions: JSparseBooleanArray; cdecl;
  function getChoiceMode: Integer; cdecl;
  procedure getFocusedRect(r: JRect); cdecl;
  function getListPaddingBottom: Integer; cdecl;
  function getListPaddingLeft: Integer; cdecl;
  function getListPaddingRight: Integer; cdecl;
  function getListPaddingTop: Integer; cdecl;
  function getSelectedView: JView; cdecl;
  function getSelector: JDrawable; cdecl;
  function getSolidColor: Integer; cdecl;
  function getTextFilter: JCharSequence; cdecl;
  function getTranscriptMode: Integer; cdecl;
  function getVerticalScrollbarWidth: Integer; cdecl;
  function hasTextFilter: Boolean; cdecl;
  procedure invalidateViews; cdecl;
  function isFastScrollAlwaysVisible: Boolean; cdecl;
  function isFastScrollEnabled: Boolean; cdecl;
  function isItemChecked(position: Integer): Boolean; cdecl;
  function isScrollingCacheEnabled: Boolean; cdecl;
  function isSmoothScrollbarEnabled: Boolean; cdecl;
  function isStackFromBottom: Boolean; cdecl;
  function isTextFilterEnabled: Boolean; cdecl;
  procedure jumpDrawablesToCurrentState; cdecl;
  function onCreateInputConnection(outAttrs: JEditorInfo): JInputConnection; cdecl;
  procedure onFilterComplete(count: Integer); cdecl;
  function onGenericMotionEvent(event: JMotionEvent): Boolean; cdecl;
  procedure onGlobalLayout; cdecl;
  function onInterceptTouchEvent(ev: JMotionEvent): Boolean; cdecl;
  function onKeyDown(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  function onKeyUp(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  function onRemoteAdapterConnected: Boolean; cdecl;
  procedure onRemoteAdapterDisconnected; cdecl;
  procedure onRestoreInstanceState(state: JParcelable); cdecl;
  function onSaveInstanceState: JParcelable; cdecl;
  procedure onTextChanged(s: JCharSequence; start: Integer; before: Integer; count: Integer); cdecl;
  function onTouchEvent(ev: JMotionEvent): Boolean; cdecl;
  procedure onTouchModeChanged(isInTouchMode: Boolean); cdecl;
  procedure onWindowFocusChanged(hasWindowFocus: Boolean); cdecl;
  function performAccessibilityAction(action: Integer; arguments: JBundle): Boolean; cdecl;
  function performItemClick(view: JView; position: Integer; id: Int64): Boolean; cdecl;
  function pointToPosition(x: Integer; y: Integer): Integer; cdecl;
  function pointToRowId(x: Integer; y: Integer): Int64; cdecl;
  procedure reclaimViews(views: JList); cdecl;
  procedure requestDisallowInterceptTouchEvent(disallowIntercept: Boolean); cdecl;
  procedure requestLayout; cdecl;
  procedure sendAccessibilityEvent(eventType: Integer); cdecl;
  procedure setAdapter(adapter: JListAdapter); cdecl;
  procedure setCacheColorHint(color: Integer); cdecl;
  procedure setChoiceMode(choiceMode: Integer); cdecl;
  procedure setDrawSelectorOnTop(onTop: Boolean); cdecl;
  procedure setFastScrollAlwaysVisible(alwaysShow: Boolean); cdecl;
  procedure setFastScrollEnabled(enabled: Boolean); cdecl;
  procedure setFilterText(filterText: JString); cdecl;
  procedure setFriction(friction: Single); cdecl;
  procedure setItemChecked(position: Integer; value: Boolean); cdecl;
  procedure setMultiChoiceModeListener(listener: JAbsListView_MultiChoiceModeListener); cdecl;
  procedure setOnScrollListener(l: JAbsListView_OnScrollListener); cdecl;
  procedure setOverScrollMode(mode: Integer); cdecl;
  procedure setRecyclerListener(listener: JAbsListView_RecyclerListener); cdecl;
  procedure setRemoteViewsAdapter(intent: JIntent); cdecl;
  procedure setScrollIndicators(up: JView; down: JView); cdecl;
  procedure setScrollingCacheEnabled(enabled: Boolean); cdecl;
  procedure setSelector(resID: Integer); cdecl; overload;
  procedure setSelector(sel: JDrawable); cdecl; overload;
  procedure setSmoothScrollbarEnabled(enabled: Boolean); cdecl;
  procedure setStackFromBottom(stackFromBottom: Boolean); cdecl;
  procedure setTextFilterEnabled(textFilterEnabled: Boolean); cdecl;
  procedure setTranscriptMode(mode: Integer); cdecl;
  procedure setVelocityScale(scale: Single); cdecl;
  procedure setVerticalScrollbarPosition(position: Integer); cdecl;
  function showContextMenuForChild(originalView: JView): Boolean; cdecl;
  procedure smoothScrollBy(distance: Integer; duration: Integer); cdecl;
  procedure smoothScrollToPosition(position: Integer); cdecl; overload;
  procedure smoothScrollToPosition(position: Integer; boundPosition: Integer); cdecl; overload;
  procedure smoothScrollToPositionFromTop(position: Integer; offset: Integer; duration: Integer); cdecl; overload;
  procedure smoothScrollToPositionFromTop(position: Integer; offset: Integer); cdecl; overload;
  function verifyDrawable(dr: JDrawable): Boolean; cdecl;
end;
TJAbsListView = class(TJavaGenericImport<JAbsListViewClass, JAbsListView>) end;

JListViewClass = interface(JAbsListViewClass)
['{64E52E74-CA76-451C-83EA-2A6C996C3E42}']
  {Methods}
  function init(context: JContext): JListView; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet): JListView; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet; defStyle: Integer): JListView; cdecl; overload;
end;

[JavaSignature('android/widget/ListView')]
JListView = interface(JAbsListView)
['{2F533089-9B07-456B-9CCD-E37633BC3E9B}']
  {Methods}
  procedure addFooterView(v: JView; data: JObject; isSelectable: Boolean); cdecl; overload;
  procedure addFooterView(v: JView); cdecl; overload;
  procedure addHeaderView(v: JView; data: JObject; isSelectable: Boolean); cdecl; overload;
  procedure addHeaderView(v: JView); cdecl; overload;
  function dispatchKeyEvent(event: JKeyEvent): Boolean; cdecl;
  function getAdapter: JListAdapter; cdecl;
  function getCheckItemIds: TJavaArray<Int64>; cdecl;//Deprecated
  function getDivider: JDrawable; cdecl;
  function getDividerHeight: Integer; cdecl;
  function getFooterViewsCount: Integer; cdecl;
  function getHeaderViewsCount: Integer; cdecl;
  function getItemsCanFocus: Boolean; cdecl;
  function getMaxScrollAmount: Integer; cdecl;
  function getOverscrollFooter: JDrawable; cdecl;
  function getOverscrollHeader: JDrawable; cdecl;
  function isOpaque: Boolean; cdecl;
  function onKeyDown(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  function onKeyMultiple(keyCode: Integer; repeatCount: Integer; event: JKeyEvent): Boolean; cdecl;
  function onKeyUp(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  function removeFooterView(v: JView): Boolean; cdecl;
  function removeHeaderView(v: JView): Boolean; cdecl;
  function requestChildRectangleOnScreen(child: JView; rect: JRect; immediate: Boolean): Boolean; cdecl;
  procedure setAdapter(adapter: JListAdapter); cdecl;
  procedure setCacheColorHint(color: Integer); cdecl;
  procedure setDivider(divider: JDrawable); cdecl;
  procedure setDividerHeight(height: Integer); cdecl;
  procedure setFooterDividersEnabled(footerDividersEnabled: Boolean); cdecl;
  procedure setHeaderDividersEnabled(headerDividersEnabled: Boolean); cdecl;
  procedure setItemsCanFocus(itemsCanFocus: Boolean); cdecl;
  procedure setOverscrollFooter(footer: JDrawable); cdecl;
  procedure setOverscrollHeader(header: JDrawable); cdecl;
  procedure setRemoteViewsAdapter(intent: JIntent); cdecl;
  procedure setSelection(position: Integer); cdecl;
  procedure setSelectionAfterHeaderView; cdecl;
  procedure setSelectionFromTop(position: Integer; y: Integer); cdecl;
  procedure smoothScrollByOffset(offset: Integer); cdecl;
  procedure smoothScrollToPosition(position: Integer); cdecl;
end;
TJListView = class(TJavaGenericImport<JListViewClass, JListView>) end;

JFrameLayoutClass = interface(JViewGroupClass)
['{6E65B2D2-EEA5-40FA-9F02-D104EC2EABCC}']
  {Methods}
  function init(context: JContext): JFrameLayout; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet): JFrameLayout; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet; defStyle: Integer): JFrameLayout; cdecl; overload;
end;

[JavaSignature('android/widget/FrameLayout')]
JFrameLayout = interface(JViewGroup)
['{94E79213-4DDA-45B3-B896-22E5AA59C3D3}']
  {Methods}
  procedure draw(canvas: JCanvas); cdecl;
  function gatherTransparentRegion(region: JRegion): Boolean; cdecl;
  function generateLayoutParams(attrs: JAttributeSet): JFrameLayout_LayoutParams; cdecl;
  function getConsiderGoneChildrenWhenMeasuring: Boolean; cdecl;//Deprecated
  function getForeground: JDrawable; cdecl;
  function getForegroundGravity: Integer; cdecl;
  function getMeasureAllChildren: Boolean; cdecl;
  procedure jumpDrawablesToCurrentState; cdecl;
  procedure setForeground(drawable: JDrawable); cdecl;
  procedure setForegroundGravity(foregroundGravity: Integer); cdecl;
  procedure setMeasureAllChildren(measureAll: Boolean); cdecl;
  function shouldDelayChildPressedState: Boolean; cdecl;
end;
TJFrameLayout = class(TJavaGenericImport<JFrameLayoutClass, JFrameLayout>) end;

JScrollViewClass = interface(JFrameLayoutClass)
['{ECF090FA-E2FD-4EBD-8703-621FCE93BBF3}']
  {Methods}
  function init(context: JContext): JScrollView; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet): JScrollView; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet; defStyle: Integer): JScrollView; cdecl; overload;
end;

[JavaSignature('android/widget/ScrollView')]
JScrollView = interface(JFrameLayout)
['{583976EE-6287-4A78-8B75-27D2799348D0}']
  {Methods}
  procedure addView(child: JView); cdecl; overload;
  procedure addView(child: JView; index: Integer); cdecl; overload;
  procedure addView(child: JView; params: JViewGroup_LayoutParams); cdecl; overload;
  procedure addView(child: JView; index: Integer; params: JViewGroup_LayoutParams); cdecl; overload;
  function arrowScroll(direction: Integer): Boolean; cdecl;
  procedure computeScroll; cdecl;
  function dispatchKeyEvent(event: JKeyEvent): Boolean; cdecl;
  procedure draw(canvas: JCanvas); cdecl;
  function executeKeyEvent(event: JKeyEvent): Boolean; cdecl;
  procedure fling(velocityY: Integer); cdecl;
  function fullScroll(direction: Integer): Boolean; cdecl;
  function getMaxScrollAmount: Integer; cdecl;
  function isFillViewport: Boolean; cdecl;
  function isSmoothScrollingEnabled: Boolean; cdecl;
  function onGenericMotionEvent(event: JMotionEvent): Boolean; cdecl;
  function onInterceptTouchEvent(ev: JMotionEvent): Boolean; cdecl;
  function onTouchEvent(ev: JMotionEvent): Boolean; cdecl;
  function pageScroll(direction: Integer): Boolean; cdecl;
  function performAccessibilityAction(action: Integer; arguments: JBundle): Boolean; cdecl;
  procedure requestChildFocus(child: JView; focused: JView); cdecl;
  function requestChildRectangleOnScreen(child: JView; rectangle: JRect; immediate: Boolean): Boolean; cdecl;
  procedure requestDisallowInterceptTouchEvent(disallowIntercept: Boolean); cdecl;
  procedure requestLayout; cdecl;
  procedure scrollTo(x: Integer; y: Integer); cdecl;
  procedure setFillViewport(fillViewport: Boolean); cdecl;
  procedure setOverScrollMode(mode: Integer); cdecl;
  procedure setSmoothScrollingEnabled(smoothScrollingEnabled: Boolean); cdecl;
  function shouldDelayChildPressedState: Boolean; cdecl;
  procedure smoothScrollBy(dx: Integer; dy: Integer); cdecl;
  procedure smoothScrollTo(x: Integer; y: Integer); cdecl;
end;
TJScrollView = class(TJavaGenericImport<JScrollViewClass, JScrollView>) end;

JTextView_BufferTypeClass = interface(JEnumClass)
['{7275F65A-D22F-4DB7-8B68-C7607AFCB6DC}']
  {Property Methods}
  function _GetEDITABLE: JTextView_BufferType;
  function _GetNORMAL: JTextView_BufferType;
  function _GetSPANNABLE: JTextView_BufferType;
  {Methods}
  function valueOf(name: JString): JTextView_BufferType; cdecl;
  function values: TJavaObjectArray<JTextView_BufferType>; cdecl;
  {Properties}
  property EDITABLE: JTextView_BufferType read _GetEDITABLE;
  property NORMAL: JTextView_BufferType read _GetNORMAL;
  property SPANNABLE: JTextView_BufferType read _GetSPANNABLE;
end;

[JavaSignature('android/widget/TextView$BufferType')]
JTextView_BufferType = interface(JEnum)
['{059F9BC3-E477-4961-B36D-8BEB7170D0F5}']
end;
TJTextView_BufferType = class(TJavaGenericImport<JTextView_BufferTypeClass, JTextView_BufferType>) end;

JAdapterClass = interface(IJavaClass)
['{3958FE3D-7A07-4294-B0AD-318CFF53495F}']
  {Property Methods}
  function _GetIGNORE_ITEM_VIEW_TYPE: Integer;
  function _GetNO_SELECTION: Integer;
  {Properties}
  property IGNORE_ITEM_VIEW_TYPE: Integer read _GetIGNORE_ITEM_VIEW_TYPE;
  property NO_SELECTION: Integer read _GetNO_SELECTION;
end;

[JavaSignature('android/widget/Adapter')]
JAdapter = interface(IJavaInstance)
['{0D24EAC8-AFC6-4494-B3D7-B7A70C11B7A2}']
  {Methods}
  function getCount: Integer; cdecl;
  function getItem(position: Integer): JObject; cdecl;
  function getItemId(position: Integer): Int64; cdecl;
  function getItemViewType(position: Integer): Integer; cdecl;
  function getView(position: Integer; convertView: JView; parent: JViewGroup): JView; cdecl;
  function getViewTypeCount: Integer; cdecl;
  function hasStableIds: Boolean; cdecl;
  function isEmpty: Boolean; cdecl;
  procedure registerDataSetObserver(observer: JDataSetObserver); cdecl;
  procedure unregisterDataSetObserver(observer: JDataSetObserver); cdecl;
end;
TJAdapter = class(TJavaGenericImport<JAdapterClass, JAdapter>) end;

JListAdapterClass = interface(JAdapterClass)
['{C631AA3D-E166-4F53-8B50-A801AD94FB9C}']
end;

[JavaSignature('android/widget/ListAdapter')]
JListAdapter = interface(JAdapter)
['{F7DEDFB9-6976-4AFF-9264-02AEC6317A7B}']
  {Methods}
  function areAllItemsEnabled: Boolean; cdecl;
  function isEnabled(position: Integer): Boolean; cdecl;
end;
TJListAdapter = class(TJavaGenericImport<JListAdapterClass, JListAdapter>) end;

JLinearLayout_LayoutParamsClass = interface(JViewGroup_MarginLayoutParamsClass)
['{F97E1053-9B59-4F9E-AB20-FFAE01213E75}']
  {Methods}
  function init(c: JContext; attrs: JAttributeSet): JLinearLayout_LayoutParams; cdecl; overload;
  function init(width: Integer; height: Integer): JLinearLayout_LayoutParams; cdecl; overload;
  function init(width: Integer; height: Integer; weight: Single): JLinearLayout_LayoutParams; cdecl; overload;
  function init(p: JViewGroup_LayoutParams): JLinearLayout_LayoutParams; cdecl; overload;
  function init(source: JViewGroup_MarginLayoutParams): JLinearLayout_LayoutParams; cdecl; overload;
end;

[JavaSignature('android/widget/LinearLayout$LayoutParams')]
JLinearLayout_LayoutParams = interface(JViewGroup_MarginLayoutParams)
['{D53D211C-BCB8-4B84-9F82-BBCAC65549F7}']
  {Property Methods}
  function _Getgravity: Integer;
  procedure _Setgravity(Value: Integer);
  function _Getweight: Single;
  procedure _Setweight(Value: Single);
  {Methods}
  function debug(output: JString): JString; cdecl;
  {Properties}
  property gravity: Integer read _Getgravity write _Setgravity;
  property weight: Single read _Getweight write _Setweight;
end;
TJLinearLayout_LayoutParams = class(TJavaGenericImport<JLinearLayout_LayoutParamsClass, JLinearLayout_LayoutParams>) end;

JImageView_ScaleTypeClass = interface(JEnumClass)
['{37CB0769-42AF-482A-BC66-8B9D10398D26}']
  {Property Methods}
  function _GetCENTER: JImageView_ScaleType;
  function _GetCENTER_CROP: JImageView_ScaleType;
  function _GetCENTER_INSIDE: JImageView_ScaleType;
  function _GetFIT_CENTER: JImageView_ScaleType;
  function _GetFIT_END: JImageView_ScaleType;
  function _GetFIT_START: JImageView_ScaleType;
  function _GetFIT_XY: JImageView_ScaleType;
  function _GetMATRIX: JImageView_ScaleType;
  {Methods}
  function valueOf(name: JString): JImageView_ScaleType; cdecl;
  function values: TJavaObjectArray<JImageView_ScaleType>; cdecl;
  {Properties}
  property CENTER: JImageView_ScaleType read _GetCENTER;
  property CENTER_CROP: JImageView_ScaleType read _GetCENTER_CROP;
  property CENTER_INSIDE: JImageView_ScaleType read _GetCENTER_INSIDE;
  property FIT_CENTER: JImageView_ScaleType read _GetFIT_CENTER;
  property FIT_END: JImageView_ScaleType read _GetFIT_END;
  property FIT_START: JImageView_ScaleType read _GetFIT_START;
  property FIT_XY: JImageView_ScaleType read _GetFIT_XY;
  property MATRIX: JImageView_ScaleType read _GetMATRIX;
end;

[JavaSignature('android/widget/ImageView$ScaleType')]
JImageView_ScaleType = interface(JEnum)
['{A1D4F79C-D43B-4158-B200-826C032F62A1}']
end;
TJImageView_ScaleType = class(TJavaGenericImport<JImageView_ScaleTypeClass, JImageView_ScaleType>) end;

JRemoteViewsClass = interface(JObjectClass)
['{5C019581-AE7F-440F-A66D-AAA4F5DBE9C2}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Methods}
  function init(packageName: JString; layoutId: Integer): JRemoteViews; cdecl; overload;
  function init(landscape: JRemoteViews; portrait: JRemoteViews): JRemoteViews; cdecl; overload;
  function init(parcel: JParcel): JRemoteViews; cdecl; overload;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/widget/RemoteViews')]
JRemoteViews = interface(JObject)
['{C4C8FF03-5E94-4821-9401-A3DCA8A1119A}']
  {Methods}
  procedure addView(viewId: Integer; nestedView: JRemoteViews); cdecl;
  function apply(context: JContext; parent: JViewGroup): JView; cdecl;
  function clone: JRemoteViews; cdecl;
  function describeContents: Integer; cdecl;
  function getLayoutId: Integer; cdecl;
  function getPackage: JString; cdecl;
  function onLoadClass(clazz: Jlang_Class): Boolean; cdecl;
  procedure reapply(context: JContext; v: JView); cdecl;
  procedure removeAllViews(viewId: Integer); cdecl;
  procedure setBitmap(viewId: Integer; methodName: JString; value: JBitmap); cdecl;
  procedure setBoolean(viewId: Integer; methodName: JString; value: Boolean); cdecl;
  procedure setBundle(viewId: Integer; methodName: JString; value: JBundle); cdecl;
  procedure setByte(viewId: Integer; methodName: JString; value: Byte); cdecl;
  procedure setChar(viewId: Integer; methodName: JString; value: Char); cdecl;
  procedure setCharSequence(viewId: Integer; methodName: JString; value: JCharSequence); cdecl;
  procedure setChronometer(viewId: Integer; base: Int64; format: JString; started: Boolean); cdecl;
  procedure setContentDescription(viewId: Integer; contentDescription: JCharSequence); cdecl;
  procedure setDisplayedChild(viewId: Integer; childIndex: Integer); cdecl;
  procedure setDouble(viewId: Integer; methodName: JString; value: Double); cdecl;
  procedure setEmptyView(viewId: Integer; emptyViewId: Integer); cdecl;
  procedure setFloat(viewId: Integer; methodName: JString; value: Single); cdecl;
  procedure setImageViewBitmap(viewId: Integer; bitmap: JBitmap); cdecl;
  procedure setImageViewResource(viewId: Integer; srcId: Integer); cdecl;
  procedure setImageViewUri(viewId: Integer; uri: Jnet_Uri); cdecl;
  procedure setInt(viewId: Integer; methodName: JString; value: Integer); cdecl;
  procedure setIntent(viewId: Integer; methodName: JString; value: JIntent); cdecl;
  procedure setLabelFor(viewId: Integer; labeledId: Integer); cdecl;
  procedure setLong(viewId: Integer; methodName: JString; value: Int64); cdecl;
  procedure setOnClickFillInIntent(viewId: Integer; fillInIntent: JIntent); cdecl;
  procedure setProgressBar(viewId: Integer; max: Integer; progress: Integer; indeterminate: Boolean); cdecl;
  procedure setRelativeScrollPosition(viewId: Integer; offset: Integer); cdecl;
  procedure setRemoteAdapter(appWidgetId: Integer; viewId: Integer; intent: JIntent); cdecl; overload;//Deprecated
  procedure setRemoteAdapter(viewId: Integer; intent: JIntent); cdecl; overload;
  procedure setScrollPosition(viewId: Integer; position: Integer); cdecl;
  procedure setShort(viewId: Integer; methodName: JString; value: SmallInt); cdecl;
  procedure setString(viewId: Integer; methodName: JString; value: JString); cdecl;
  procedure setTextColor(viewId: Integer; color: Integer); cdecl;
  procedure setTextViewCompoundDrawables(viewId: Integer; left: Integer; top: Integer; right: Integer; bottom: Integer); cdecl;
  procedure setTextViewCompoundDrawablesRelative(viewId: Integer; start: Integer; top: Integer; end_: Integer; bottom: Integer); cdecl;
  procedure setTextViewText(viewId: Integer; text: JCharSequence); cdecl;
  procedure setTextViewTextSize(viewId: Integer; units: Integer; size: Single); cdecl;
  procedure setUri(viewId: Integer; methodName: JString; value: Jnet_Uri); cdecl;
  procedure setViewPadding(viewId: Integer; left: Integer; top: Integer; right: Integer; bottom: Integer); cdecl;
  procedure setViewVisibility(viewId: Integer; visibility: Integer); cdecl;
  procedure showNext(viewId: Integer); cdecl;
  procedure showPrevious(viewId: Integer); cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
end;
TJRemoteViews = class(TJavaGenericImport<JRemoteViewsClass, JRemoteViews>) end;

JAbsListView_LayoutParamsClass = interface(JViewGroup_LayoutParamsClass)
['{BA119D83-F4CC-4B61-B09E-949DDD5F3E2C}']
  {Methods}
  function init(c: JContext; attrs: JAttributeSet): JAbsListView_LayoutParams; cdecl; overload;
  function init(w: Integer; h: Integer): JAbsListView_LayoutParams; cdecl; overload;
  function init(w: Integer; h: Integer; viewType: Integer): JAbsListView_LayoutParams; cdecl; overload;
  function init(source: JViewGroup_LayoutParams): JAbsListView_LayoutParams; cdecl; overload;
end;

[JavaSignature('android/widget/AbsListView$LayoutParams')]
JAbsListView_LayoutParams = interface(JViewGroup_LayoutParams)
['{0257889C-E50D-40DF-8A0E-1CABF3504F80}']
end;
TJAbsListView_LayoutParams = class(TJavaGenericImport<JAbsListView_LayoutParamsClass, JAbsListView_LayoutParams>) end;

JAbsListView_RecyclerListenerClass = interface(IJavaClass)
['{06DBD286-C7E2-43B3-B493-7DA33E6C15BC}']
end;

[JavaSignature('android/widget/AbsListView$RecyclerListener')]
JAbsListView_RecyclerListener = interface(IJavaInstance)
['{FD43448A-4F16-42C9-BA44-62CB7DC68E6C}']
  {Methods}
  procedure onMovedToScrapHeap(view: JView); cdecl;
end;
TJAbsListView_RecyclerListener = class(TJavaGenericImport<JAbsListView_RecyclerListenerClass, JAbsListView_RecyclerListener>) end;

JFrameLayout_LayoutParamsClass = interface(JViewGroup_MarginLayoutParamsClass)
['{F5F1E63E-8329-4F5C-8096-D369E2650637}']
  {Methods}
  function init(c: JContext; attrs: JAttributeSet): JFrameLayout_LayoutParams; cdecl; overload;
  function init(width: Integer; height: Integer): JFrameLayout_LayoutParams; cdecl; overload;
  function init(width: Integer; height: Integer; gravity: Integer): JFrameLayout_LayoutParams; cdecl; overload;
  function init(source: JViewGroup_LayoutParams): JFrameLayout_LayoutParams; cdecl; overload;
  function init(source: JViewGroup_MarginLayoutParams): JFrameLayout_LayoutParams; cdecl; overload;
end;

[JavaSignature('android/widget/FrameLayout$LayoutParams')]
JFrameLayout_LayoutParams = interface(JViewGroup_MarginLayoutParams)
['{01468B94-1F5E-4E57-990C-909B89476072}']
  {Property Methods}
  function _Getgravity: Integer;
  procedure _Setgravity(Value: Integer);
  {Properties}
  property gravity: Integer read _Getgravity write _Setgravity;
end;
TJFrameLayout_LayoutParams = class(TJavaGenericImport<JFrameLayout_LayoutParamsClass, JFrameLayout_LayoutParams>) end;

JSpinnerAdapterClass = interface(JAdapterClass)
['{691169DF-C0EF-4E6C-8E72-E7128AE1841B}']
end;

[JavaSignature('android/widget/SpinnerAdapter')]
JSpinnerAdapter = interface(JAdapter)
['{68A77466-E8F0-443D-88C6-FB41DB5AB915}']
  {Methods}
  function getDropDownView(position: Integer; convertView: JView; parent: JViewGroup): JView; cdecl;
end;
TJSpinnerAdapter = class(TJavaGenericImport<JSpinnerAdapterClass, JSpinnerAdapter>) end;

JAdapterView_OnItemSelectedListenerClass = interface(IJavaClass)
['{8CC3E8E8-40EF-49D6-990B-985B599D11E1}']
end;

[JavaSignature('android/widget/AdapterView$OnItemSelectedListener')]
JAdapterView_OnItemSelectedListener = interface(IJavaInstance)
['{81BE7775-99A9-4BF0-B1D4-920851584468}']
  {Methods}
  procedure onItemSelected(parent: JAdapterView; view: JView; position: Integer; id: Int64); cdecl;
  procedure onNothingSelected(parent: JAdapterView); cdecl;
end;
TJAdapterView_OnItemSelectedListener = class(TJavaGenericImport<JAdapterView_OnItemSelectedListenerClass, JAdapterView_OnItemSelectedListener>) end;

JAdapterView_OnItemLongClickListenerClass = interface(IJavaClass)
['{56FC07F2-9B26-4321-BBA7-87B90FE02F20}']
end;

[JavaSignature('android/widget/AdapterView$OnItemLongClickListener')]
JAdapterView_OnItemLongClickListener = interface(IJavaInstance)
['{64F90302-CAC6-499D-85CB-B8159D130BE7}']
  {Methods}
  function onItemLongClick(parent: JAdapterView; view: JView; position: Integer; id: Int64): Boolean; cdecl;
end;
TJAdapterView_OnItemLongClickListener = class(TJavaGenericImport<JAdapterView_OnItemLongClickListenerClass, JAdapterView_OnItemLongClickListener>) end;

JAbsoluteLayoutClass = interface(JViewGroupClass)
['{FD88C087-3EAF-4ABA-A323-5C0788D7B04F}']
  {Methods}
  function init(context: JContext): JAbsoluteLayout; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet): JAbsoluteLayout; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet; defStyle: Integer): JAbsoluteLayout; cdecl; overload;
end;

[JavaSignature('android/widget/AbsoluteLayout')]
JAbsoluteLayout = interface(JViewGroup)
['{0880FBEB-72E8-4BFB-922D-A773808F1999}']
  {Methods}
  function generateLayoutParams(attrs: JAttributeSet): JViewGroup_LayoutParams; cdecl;
  function shouldDelayChildPressedState: Boolean; cdecl;
end;
TJAbsoluteLayout = class(TJavaGenericImport<JAbsoluteLayoutClass, JAbsoluteLayout>) end;

JAdapterView_OnItemClickListenerClass = interface(IJavaClass)
['{927C4193-A697-4855-B9DB-F8E7238E505B}']
end;

[JavaSignature('android/widget/AdapterView$OnItemClickListener')]
JAdapterView_OnItemClickListener = interface(IJavaInstance)
['{C961007F-9268-49A1-A7D2-FF18F99CB04F}']
  {Methods}
  procedure onItemClick(parent: JAdapterView; view: JView; position: Integer; id: Int64); cdecl;
end;
TJAdapterView_OnItemClickListener = class(TJavaGenericImport<JAdapterView_OnItemClickListenerClass, JAdapterView_OnItemClickListener>) end;

JAbsListView_MultiChoiceModeListenerClass = interface(JActionMode_CallbackClass)
['{8BB552DD-2198-4EE3-954D-7AED2EA26652}']
end;

[JavaSignature('android/widget/AbsListView$MultiChoiceModeListener')]
JAbsListView_MultiChoiceModeListener = interface(JActionMode_Callback)
['{33997ECE-97BD-47EF-BCAD-E9175C1E4DED}']
  {Methods}
  procedure onItemCheckedStateChanged(mode: JActionMode; position: Integer; id: Int64; checked: Boolean); cdecl;
end;
TJAbsListView_MultiChoiceModeListener = class(TJavaGenericImport<JAbsListView_MultiChoiceModeListenerClass, JAbsListView_MultiChoiceModeListener>) end;

JPopupWindowClass = interface(JObjectClass)
['{D703BEFF-3652-475F-A780-B2B2C9F43D66}']
  {Property Methods}
  function _GetINPUT_METHOD_FROM_FOCUSABLE: Integer;
  function _GetINPUT_METHOD_NEEDED: Integer;
  function _GetINPUT_METHOD_NOT_NEEDED: Integer;
  {Methods}
  function init(context: JContext): JPopupWindow; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet): JPopupWindow; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet; defStyle: Integer): JPopupWindow; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet; defStyleAttr: Integer; defStyleRes: Integer): JPopupWindow; cdecl; overload;
  function init: JPopupWindow; cdecl; overload;
  function init(contentView: JView): JPopupWindow; cdecl; overload;
  function init(width: Integer; height: Integer): JPopupWindow; cdecl; overload;
  function init(contentView: JView; width: Integer; height: Integer): JPopupWindow; cdecl; overload;
  function init(contentView: JView; width: Integer; height: Integer; focusable: Boolean): JPopupWindow; cdecl; overload;
  {Properties}
  property INPUT_METHOD_FROM_FOCUSABLE: Integer read _GetINPUT_METHOD_FROM_FOCUSABLE;
  property INPUT_METHOD_NEEDED: Integer read _GetINPUT_METHOD_NEEDED;
  property INPUT_METHOD_NOT_NEEDED: Integer read _GetINPUT_METHOD_NOT_NEEDED;
end;

[JavaSignature('android/widget/PopupWindow')]
JPopupWindow = interface(JObject)
['{5D44B452-2E94-47F4-9610-F25833488A46}']
  {Methods}
  procedure dismiss; cdecl;
  function getAnimationStyle: Integer; cdecl;
  function getBackground: JDrawable; cdecl;
  function getContentView: JView; cdecl;
  function getHeight: Integer; cdecl;
  function getInputMethodMode: Integer; cdecl;
  function getMaxAvailableHeight(anchor: JView): Integer; cdecl; overload;
  function getMaxAvailableHeight(anchor: JView; yOffset: Integer): Integer; cdecl; overload;
  function getSoftInputMode: Integer; cdecl;
  function getWidth: Integer; cdecl;
  function isAboveAnchor: Boolean; cdecl;
  function isClippingEnabled: Boolean; cdecl;
  function isFocusable: Boolean; cdecl;
  function isOutsideTouchable: Boolean; cdecl;
  function isShowing: Boolean; cdecl;
  function isSplitTouchEnabled: Boolean; cdecl;
  function isTouchable: Boolean; cdecl;
  procedure setAnimationStyle(animationStyle: Integer); cdecl;
  procedure setBackgroundDrawable(background: JDrawable); cdecl;
  procedure setClippingEnabled(enabled: Boolean); cdecl;
  procedure setContentView(contentView: JView); cdecl;
  procedure setFocusable(focusable: Boolean); cdecl;
  procedure setHeight(height: Integer); cdecl;
  procedure setIgnoreCheekPress; cdecl;
  procedure setInputMethodMode(mode: Integer); cdecl;
  procedure setOnDismissListener(onDismissListener: JPopupWindow_OnDismissListener); cdecl;
  procedure setOutsideTouchable(touchable: Boolean); cdecl;
  procedure setSoftInputMode(mode: Integer); cdecl;
  procedure setSplitTouchEnabled(enabled: Boolean); cdecl;
  procedure setTouchInterceptor(l: JView_OnTouchListener); cdecl;
  procedure setTouchable(touchable: Boolean); cdecl;
  procedure setWidth(width: Integer); cdecl;
  procedure setWindowLayoutMode(widthSpec: Integer; heightSpec: Integer); cdecl;
  procedure showAsDropDown(anchor: JView); cdecl; overload;
  procedure showAsDropDown(anchor: JView; xoff: Integer; yoff: Integer); cdecl; overload;
  procedure showAtLocation(parent: JView; gravity: Integer; x: Integer; y: Integer); cdecl;
  procedure update; cdecl; overload;
  procedure update(width: Integer; height: Integer); cdecl; overload;
  procedure update(x: Integer; y: Integer; width: Integer; height: Integer); cdecl; overload;
  procedure update(x: Integer; y: Integer; width: Integer; height: Integer; force: Boolean); cdecl; overload;
  procedure update(anchor: JView; width: Integer; height: Integer); cdecl; overload;
  procedure update(anchor: JView; xoff: Integer; yoff: Integer; width: Integer; height: Integer); cdecl; overload;
end;
TJPopupWindow = class(TJavaGenericImport<JPopupWindowClass, JPopupWindow>) end;

JPopupWindow_OnDismissListenerClass = interface(IJavaClass)
['{0D8EC9E0-1E9D-4218-B811-4D795CBDEF2D}']
end;

[JavaSignature('android/widget/PopupWindow$OnDismissListener')]
JPopupWindow_OnDismissListener = interface(IJavaInstance)
['{C4613C55-80EE-41CB-A235-6C002B229AED}']
  {Methods}
  procedure onDismiss; cdecl;
end;
TJPopupWindow_OnDismissListener = class(TJavaGenericImport<JPopupWindow_OnDismissListenerClass, JPopupWindow_OnDismissListener>) end;

JTextView_OnEditorActionListenerClass = interface(IJavaClass)
['{F1975C99-6495-441B-AE06-1BF3187DCCC8}']
end;

[JavaSignature('android/widget/TextView$OnEditorActionListener')]
JTextView_OnEditorActionListener = interface(IJavaInstance)
['{6E2FAEE9-92A5-4592-B040-72632B17DA08}']
  {Methods}
  function onEditorAction(v: JTextView; actionId: Integer; event: JKeyEvent): Boolean; cdecl;
end;
TJTextView_OnEditorActionListener = class(TJavaGenericImport<JTextView_OnEditorActionListenerClass, JTextView_OnEditorActionListener>) end;

JImageViewClass = interface(JViewClass)
['{0BBBCB19-E4B1-4147-99A8-9DA0AEEEA7AD}']
  {Methods}
  function init(context: JContext): JImageView; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet): JImageView; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet; defStyle: Integer): JImageView; cdecl; overload;
end;

[JavaSignature('android/widget/ImageView')]
JImageView = interface(JView)
['{21CA0CF7-F857-4E53-9E2B-A12903F4216E}']
  {Methods}
  procedure clearColorFilter; cdecl;
  function getAdjustViewBounds: Boolean; cdecl;
  function getBaseline: Integer; cdecl;
  function getBaselineAlignBottom: Boolean; cdecl;
  function getColorFilter: JColorFilter; cdecl;
  function getCropToPadding: Boolean; cdecl;
  function getDrawable: JDrawable; cdecl;
  function getImageAlpha: Integer; cdecl;
  function getImageMatrix: JMatrix; cdecl;
  function getMaxHeight: Integer; cdecl;
  function getMaxWidth: Integer; cdecl;
  function getScaleType: JImageView_ScaleType; cdecl;
  function hasOverlappingRendering: Boolean; cdecl;
  procedure invalidateDrawable(dr: JDrawable); cdecl;
  procedure jumpDrawablesToCurrentState; cdecl;
  function onCreateDrawableState(extraSpace: Integer): TJavaArray<Integer>; cdecl;
  procedure setAdjustViewBounds(adjustViewBounds: Boolean); cdecl;
  procedure setAlpha(alpha: Integer); cdecl;//Deprecated
  procedure setBaseline(baseline: Integer); cdecl;
  procedure setBaselineAlignBottom(aligned: Boolean); cdecl;
  procedure setColorFilter(color: Integer; mode: JPorterDuff_Mode); cdecl; overload;
  procedure setColorFilter(color: Integer); cdecl; overload;
  procedure setColorFilter(cf: JColorFilter); cdecl; overload;
  procedure setCropToPadding(cropToPadding: Boolean); cdecl;
  procedure setImageAlpha(alpha: Integer); cdecl;
  procedure setImageBitmap(bm: JBitmap); cdecl;
  procedure setImageDrawable(drawable: JDrawable); cdecl;
  procedure setImageLevel(level: Integer); cdecl;
  procedure setImageMatrix(matrix: JMatrix); cdecl;
  procedure setImageResource(resId: Integer); cdecl;
  procedure setImageState(state: TJavaArray<Integer>; merge: Boolean); cdecl;
  procedure setImageURI(uri: Jnet_Uri); cdecl;
  procedure setMaxHeight(maxHeight: Integer); cdecl;
  procedure setMaxWidth(maxWidth: Integer); cdecl;
  procedure setScaleType(scaleType: JImageView_ScaleType); cdecl;
  procedure setSelected(selected: Boolean); cdecl;
  procedure setVisibility(visibility: Integer); cdecl;
end;
TJImageView = class(TJavaGenericImport<JImageViewClass, JImageView>) end;

JLinearLayoutClass = interface(JViewGroupClass)
['{EAEAF7E9-98CA-403B-A2E4-2AA56B0A3E1F}']
  {Property Methods}
  function _GetHORIZONTAL: Integer;
  function _GetSHOW_DIVIDER_BEGINNING: Integer;
  function _GetSHOW_DIVIDER_END: Integer;
  function _GetSHOW_DIVIDER_MIDDLE: Integer;
  function _GetSHOW_DIVIDER_NONE: Integer;
  function _GetVERTICAL: Integer;
  {Methods}
  function init(context: JContext): JLinearLayout; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet): JLinearLayout; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet; defStyle: Integer): JLinearLayout; cdecl; overload;
  {Properties}
  property HORIZONTAL: Integer read _GetHORIZONTAL;
  property SHOW_DIVIDER_BEGINNING: Integer read _GetSHOW_DIVIDER_BEGINNING;
  property SHOW_DIVIDER_END: Integer read _GetSHOW_DIVIDER_END;
  property SHOW_DIVIDER_MIDDLE: Integer read _GetSHOW_DIVIDER_MIDDLE;
  property SHOW_DIVIDER_NONE: Integer read _GetSHOW_DIVIDER_NONE;
  property VERTICAL: Integer read _GetVERTICAL;
end;

[JavaSignature('android/widget/LinearLayout')]
JLinearLayout = interface(JViewGroup)
['{903D0B13-CCC1-4012-B4A8-C24B6013F07F}']
  {Methods}
  function generateLayoutParams(attrs: JAttributeSet): JLinearLayout_LayoutParams; cdecl;
  function getBaseline: Integer; cdecl;
  function getBaselineAlignedChildIndex: Integer; cdecl;
  function getDividerDrawable: JDrawable; cdecl;
  function getDividerPadding: Integer; cdecl;
  function getOrientation: Integer; cdecl;
  function getShowDividers: Integer; cdecl;
  function getWeightSum: Single; cdecl;
  function isBaselineAligned: Boolean; cdecl;
  function isMeasureWithLargestChildEnabled: Boolean; cdecl;
  procedure setBaselineAligned(baselineAligned: Boolean); cdecl;
  procedure setBaselineAlignedChildIndex(i: Integer); cdecl;
  procedure setDividerDrawable(divider: JDrawable); cdecl;
  procedure setDividerPadding(padding: Integer); cdecl;
  procedure setGravity(gravity: Integer); cdecl;
  procedure setHorizontalGravity(horizontalGravity: Integer); cdecl;
  procedure setMeasureWithLargestChildEnabled(enabled: Boolean); cdecl;
  procedure setOrientation(orientation: Integer); cdecl;
  procedure setShowDividers(showDividers: Integer); cdecl;
  procedure setVerticalGravity(verticalGravity: Integer); cdecl;
  procedure setWeightSum(weightSum: Single); cdecl;
  function shouldDelayChildPressedState: Boolean; cdecl;
end;
TJLinearLayout = class(TJavaGenericImport<JLinearLayoutClass, JLinearLayout>) end;




implementation

begin

end.


