{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit Androidapi.JNI.GraphicsContentViewText;

interface

uses 
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
  Androidapi.JNI.Net,
  Androidapi.JNI.Util;

type

  {Class forward declarations}
  JDialogInterface_OnKeyListener = interface;//android.content.DialogInterface$OnKeyListener
  JSurfaceHolder_Callback = interface;//android.view.SurfaceHolder$Callback
  JInputEvent = interface;//android.view.InputEvent
  JPathEffect = interface;//android.graphics.PathEffect
  JPathDashPathEffect = interface;//android.graphics.PathDashPathEffect
  JSumPathEffect = interface;//android.graphics.SumPathEffect
  JView_OnClickListener = interface;//android.view.View$OnClickListener
  JDialogInterface_OnShowListener = interface;//android.content.DialogInterface$OnShowListener
  JSQLiteClosable = interface;//android.database.sqlite.SQLiteClosable
  JSQLiteDatabase = interface;//android.database.sqlite.SQLiteDatabase
  JMotionEvent = interface;//android.view.MotionEvent
  JInputMethod = interface;//android.view.inputmethod.InputMethod
  JSyncInfo = interface;//android.content.SyncInfo
  JLayout_Directions = interface;//android.text.Layout$Directions
  JMenu = interface;//android.view.Menu
  JContextMenu = interface;//android.view.ContextMenu
  JPath = interface;//android.graphics.Path
  JInputQueue_Callback = interface;//android.view.InputQueue$Callback
  Jgraphics_Interpolator = interface;//android.graphics.Interpolator
  JLayoutInflater = interface;//android.view.LayoutInflater
  JImageFormat = interface;//android.graphics.ImageFormat
  JInputFilter = interface;//android.text.InputFilter
  JPixelFormat = interface;//android.graphics.PixelFormat
  JLoader_OnLoadCanceledListener = interface;//android.content.Loader$OnLoadCanceledListener
  JNinePatch = interface;//android.graphics.NinePatch
  JSyncAdapterType = interface;//android.content.SyncAdapterType
  JContentProvider_PipeDataWriter = interface;//android.content.ContentProvider$PipeDataWriter
  JLoader_OnLoadCompleteListener = interface;//android.content.Loader$OnLoadCompleteListener
  JInputConnection = interface;//android.view.inputmethod.InputConnection
  JNoCopySpan = interface;//android.text.NoCopySpan
  JTextWatcher = interface;//android.text.TextWatcher
  JViewTreeObserver = interface;//android.view.ViewTreeObserver
  JShader = interface;//android.graphics.Shader
  JComposeShader = interface;//android.graphics.ComposeShader
  JPackageInfo = interface;//android.content.pm.PackageInfo
  JConfigurationInfo = interface;//android.content.pm.ConfigurationInfo
  JIntent = interface;//android.content.Intent
  JLabeledIntent = interface;//android.content.pm.LabeledIntent
  JIntentSender_OnFinished = interface;//android.content.IntentSender$OnFinished
  JSQLiteProgram = interface;//android.database.sqlite.SQLiteProgram
  JMovie = interface;//android.graphics.Movie
  JPathPermission = interface;//android.content.pm.PathPermission
  JActionMode = interface;//android.view.ActionMode
  JPackageItemInfo_DisplayNameComparator = interface;//android.content.pm.PackageItemInfo$DisplayNameComparator
  JLoader = interface;//android.content.Loader
  JAsyncTaskLoader = interface;//android.content.AsyncTaskLoader
  JEditorInfo = interface;//android.view.inputmethod.EditorInfo
  JResolveInfo_DisplayNameComparator = interface;//android.content.pm.ResolveInfo$DisplayNameComparator
  JBroadcastReceiver_PendingResult = interface;//android.content.BroadcastReceiver$PendingResult
  JGestureDetector_SimpleOnGestureListener = interface;//android.view.GestureDetector$SimpleOnGestureListener
  JInputQueue = interface;//android.view.InputQueue
  JSQLiteQuery = interface;//android.database.sqlite.SQLiteQuery
  JDrawable = interface;//android.graphics.drawable.Drawable
  JClipDrawable = interface;//android.graphics.drawable.ClipDrawable
  JSurfaceTexture_OutOfResourcesException = interface;//android.graphics.SurfaceTexture$OutOfResourcesException
  JPackageItemInfo = interface;//android.content.pm.PackageItemInfo
  JComponentInfo = interface;//android.content.pm.ComponentInfo
  JProviderInfo = interface;//android.content.pm.ProviderInfo
  JView = interface;//android.view.View
  JSurfaceView = interface;//android.view.SurfaceView
  JPackageStats = interface;//android.content.pm.PackageStats
  JView_OnAttachStateChangeListener = interface;//android.view.View$OnAttachStateChangeListener
  JXfermode = interface;//android.graphics.Xfermode
  JPixelXorXfermode = interface;//android.graphics.PixelXorXfermode
  JSyncStats = interface;//android.content.SyncStats
  JOperationApplicationException = interface;//android.content.OperationApplicationException
  JDialogInterface_OnDismissListener = interface;//android.content.DialogInterface$OnDismissListener
  JPaint_Join = interface;//android.graphics.Paint$Join
  JPaint_Cap = interface;//android.graphics.Paint$Cap
  JViewTreeObserver_OnGlobalFocusChangeListener = interface;//android.view.ViewTreeObserver$OnGlobalFocusChangeListener
  Jdatabase_CharArrayBuffer = interface;//android.database.CharArrayBuffer
  JBlurMaskFilter_Blur = interface;//android.graphics.BlurMaskFilter$Blur
  JPermissionGroupInfo = interface;//android.content.pm.PermissionGroupInfo
  JContextMenu_ContextMenuInfo = interface;//android.view.ContextMenu$ContextMenuInfo
  JPaint = interface;//android.graphics.Paint
  JPaint_FontMetricsInt = interface;//android.graphics.Paint$FontMetricsInt
  JCompletionInfo = interface;//android.view.inputmethod.CompletionInfo
  JViewGroup = interface;//android.view.ViewGroup
  JLinearGradient = interface;//android.graphics.LinearGradient
  JCorrectionInfo = interface;//android.view.inputmethod.CorrectionInfo
  JDisplay = interface;//android.view.Display
  JView_OnGenericMotionListener = interface;//android.view.View$OnGenericMotionListener
  JTextUtils_TruncateAt = interface;//android.text.TextUtils$TruncateAt
  JContext = interface;//android.content.Context
  JContextWrapper = interface;//android.content.ContextWrapper
  JMutableContextWrapper = interface;//android.content.MutableContextWrapper
  JPicture = interface;//android.graphics.Picture
  JViewManager = interface;//android.view.ViewManager
  JWindowManager = interface;//android.view.WindowManager
  JPackageManager = interface;//android.content.pm.PackageManager
  JEntityIterator = interface;//android.content.EntityIterator
  JInputBinding = interface;//android.view.inputmethod.InputBinding
  JDrawable_ConstantState = interface;//android.graphics.drawable.Drawable$ConstantState
  JPath_Direction = interface;//android.graphics.Path$Direction
  JBitmapDrawable = interface;//android.graphics.drawable.BitmapDrawable
  JScaleDrawable = interface;//android.graphics.drawable.ScaleDrawable
  JContextThemeWrapper = interface;//android.view.ContextThemeWrapper
  JView_OnKeyListener = interface;//android.view.View$OnKeyListener
  JMenuItem = interface;//android.view.MenuItem
  JView_OnCreateContextMenuListener = interface;//android.view.View$OnCreateContextMenuListener
  JIntentFilter_AuthorityEntry = interface;//android.content.IntentFilter$AuthorityEntry
  JGestureDetector = interface;//android.view.GestureDetector
  JShapeDrawable_ShaderFactory = interface;//android.graphics.drawable.ShapeDrawable$ShaderFactory
  JMenuInflater = interface;//android.view.MenuInflater
  JView_OnTouchListener = interface;//android.view.View$OnTouchListener
  JInsetDrawable = interface;//android.graphics.drawable.InsetDrawable
  JDatabaseErrorHandler = interface;//android.database.DatabaseErrorHandler
  JDialogInterface_OnMultiChoiceClickListener = interface;//android.content.DialogInterface$OnMultiChoiceClickListener
  JInputMethodSession = interface;//android.view.inputmethod.InputMethodSession
  JMatrix = interface;//android.graphics.Matrix
  JDrawableContainer = interface;//android.graphics.drawable.DrawableContainer
  JAnimationDrawable = interface;//android.graphics.drawable.AnimationDrawable
  JPoint = interface;//android.graphics.Point
  JShapeDrawable = interface;//android.graphics.drawable.ShapeDrawable
  JExtractedTextRequest = interface;//android.view.inputmethod.ExtractedTextRequest
  JColorFilter = interface;//android.graphics.ColorFilter
  JColorMatrixColorFilter = interface;//android.graphics.ColorMatrixColorFilter
  JSweepGradient = interface;//android.graphics.SweepGradient
  JContentProviderOperation_Builder = interface;//android.content.ContentProviderOperation$Builder
  JObbScanner = interface;//android.content.res.ObbScanner
  JMatrix_ScaleToFit = interface;//android.graphics.Matrix$ScaleToFit
  JResources_NotFoundException = interface;//android.content.res.Resources$NotFoundException
  JGradientDrawable = interface;//android.graphics.drawable.GradientDrawable
  JViewTreeObserver_OnScrollChangedListener = interface;//android.view.ViewTreeObserver$OnScrollChangedListener
  JBaseInputConnection = interface;//android.view.inputmethod.BaseInputConnection
  JPorterDuffColorFilter = interface;//android.graphics.PorterDuffColorFilter
  JCharacterStyle = interface;//android.text.style.CharacterStyle
  JClickableSpan = interface;//android.text.style.ClickableSpan
  JURLSpan = interface;//android.text.style.URLSpan
  JCanvas_EdgeType = interface;//android.graphics.Canvas$EdgeType
  JShape = interface;//android.graphics.drawable.shapes.Shape
  JPathShape = interface;//android.graphics.drawable.shapes.PathShape
  JComponentCallbacks = interface;//android.content.ComponentCallbacks
  JComponentCallbacks2 = interface;//android.content.ComponentCallbacks2
  JSignature = interface;//android.content.pm.Signature
  JInputDevice = interface;//android.view.InputDevice
  Jcontent_Entity = interface;//android.content.Entity
  JContentValues = interface;//android.content.ContentValues
  JLayoutInflater_Filter = interface;//android.view.LayoutInflater$Filter
  JLayoutInflater_Factory = interface;//android.view.LayoutInflater$Factory
  JBitmapFactory_Options = interface;//android.graphics.BitmapFactory$Options
  JColorMatrix = interface;//android.graphics.ColorMatrix
  JIntentSender_SendIntentException = interface;//android.content.IntentSender$SendIntentException
  JAssetManager = interface;//android.content.res.AssetManager
  JSurface = interface;//android.view.Surface
  JBitmapFactory = interface;//android.graphics.BitmapFactory
  JBitmapShader = interface;//android.graphics.BitmapShader
  JClipDescription = interface;//android.content.ClipDescription
  JContentProviderOperation = interface;//android.content.ContentProviderOperation
  JActivityInfo = interface;//android.content.pm.ActivityInfo
  JRectShape = interface;//android.graphics.drawable.shapes.RectShape
  JArcShape = interface;//android.graphics.drawable.shapes.ArcShape
  Jgraphics_Camera = interface;//android.graphics.Camera
  JPaint_Style = interface;//android.graphics.Paint$Style
  JActionProvider_VisibilityListener = interface;//android.view.ActionProvider$VisibilityListener
  JLayout_Alignment = interface;//android.text.Layout$Alignment
  JActionProvider = interface;//android.view.ActionProvider
  JSpanned = interface;//android.text.Spanned
  JSQLiteCursorDriver = interface;//android.database.sqlite.SQLiteCursorDriver
  JDiscretePathEffect = interface;//android.graphics.DiscretePathEffect
  JDialogInterface_OnClickListener = interface;//android.content.DialogInterface$OnClickListener
  JMenuItem_OnMenuItemClickListener = interface;//android.view.MenuItem$OnMenuItemClickListener
  JViewTreeObserver_OnPreDrawListener = interface;//android.view.ViewTreeObserver$OnPreDrawListener
  JIntentFilter = interface;//android.content.IntentFilter
  JInputMethodManager = interface;//android.view.inputmethod.InputMethodManager
  JScaleGestureDetector = interface;//android.view.ScaleGestureDetector
  JSurfaceHolder_Callback2 = interface;//android.view.SurfaceHolder$Callback2
  JXmlResourceParser = interface;//android.content.res.XmlResourceParser
  JPaint_Align = interface;//android.graphics.Paint$Align
  JSpannable = interface;//android.text.Spannable
  JContentResolver = interface;//android.content.ContentResolver
  JResources = interface;//android.content.res.Resources
  JShader_TileMode = interface;//android.graphics.Shader$TileMode
  JRasterizer = interface;//android.graphics.Rasterizer
  JLayerRasterizer = interface;//android.graphics.LayerRasterizer
  JOvalShape = interface;//android.graphics.drawable.shapes.OvalShape
  JScaleGestureDetector_OnScaleGestureListener = interface;//android.view.ScaleGestureDetector$OnScaleGestureListener
  JDragEvent = interface;//android.view.DragEvent
  JInputMethodSubtype = interface;//android.view.inputmethod.InputMethodSubtype
  JAssetManager_AssetInputStream = interface;//android.content.res.AssetManager$AssetInputStream
  JPath_FillType = interface;//android.graphics.Path$FillType
  JLayerDrawable = interface;//android.graphics.drawable.LayerDrawable
  JPermissionInfo = interface;//android.content.pm.PermissionInfo
  JResolveInfo = interface;//android.content.pm.ResolveInfo
  JSharedPreferences = interface;//android.content.SharedPreferences
  JSurfaceTexture = interface;//android.graphics.SurfaceTexture
  JPathDashPathEffect_Style = interface;//android.graphics.PathDashPathEffect$Style
  JClipData_Item = interface;//android.content.ClipData$Item
  JResources_Theme = interface;//android.content.res.Resources$Theme
  JViewTreeObserver_OnTouchModeChangeListener = interface;//android.view.ViewTreeObserver$OnTouchModeChangeListener
  JReceiverCallNotAllowedException = interface;//android.content.ReceiverCallNotAllowedException
  JGestureDetector_OnGestureListener = interface;//android.view.GestureDetector$OnGestureListener
  JBitmap = interface;//android.graphics.Bitmap
  JInputDevice_MotionRange = interface;//android.view.InputDevice$MotionRange
  JAbstractThreadedSyncAdapter = interface;//android.content.AbstractThreadedSyncAdapter
  JInputType = interface;//android.text.InputType
  JTextPaint = interface;//android.text.TextPaint
  JEditable_Factory = interface;//android.text.Editable$Factory
  JContentObserver = interface;//android.database.ContentObserver
  JLoader_ForceLoadContentObserver = interface;//android.content.Loader$ForceLoadContentObserver
  JDataSetObserver = interface;//android.database.DataSetObserver
  JTransitionDrawable = interface;//android.graphics.drawable.TransitionDrawable
  JCanvas_VertexMode = interface;//android.graphics.Canvas$VertexMode
  JComponentName = interface;//android.content.ComponentName
  JServiceConnection = interface;//android.content.ServiceConnection
  JTransformationMethod = interface;//android.text.method.TransformationMethod
  JPeriodicSync = interface;//android.content.PeriodicSync
  JContentQueryMap = interface;//android.content.ContentQueryMap
  JMotionEvent_PointerCoords = interface;//android.view.MotionEvent$PointerCoords
  JSurfaceHolder = interface;//android.view.SurfaceHolder
  JRadialGradient = interface;//android.graphics.RadialGradient
  JViewGroup_LayoutParams = interface;//android.view.ViewGroup$LayoutParams
  JViewGroup_MarginLayoutParams = interface;//android.view.ViewGroup$MarginLayoutParams
  JSQLiteStatement = interface;//android.database.sqlite.SQLiteStatement
  JViewTreeObserver_OnGlobalLayoutListener = interface;//android.view.ViewTreeObserver$OnGlobalLayoutListener
  JView_OnLongClickListener = interface;//android.view.View$OnLongClickListener
  JKeyEvent_DispatcherState = interface;//android.view.KeyEvent$DispatcherState
  JRegionIterator = interface;//android.graphics.RegionIterator
  JRect = interface;//android.graphics.Rect
  JContentProviderResult = interface;//android.content.ContentProviderResult
  JAsyncQueryHandler = interface;//android.content.AsyncQueryHandler
  JAssetFileDescriptor_AutoCloseOutputStream = interface;//android.content.res.AssetFileDescriptor$AutoCloseOutputStream
  JAssetFileDescriptor = interface;//android.content.res.AssetFileDescriptor
  JWindow = interface;//android.view.Window
  JInputConnectionWrapper = interface;//android.view.inputmethod.InputConnectionWrapper
  JWindow_Callback = interface;//android.view.Window$Callback
  JSpannable_Factory = interface;//android.text.Spannable$Factory
  JRegion = interface;//android.graphics.Region
  JClipData = interface;//android.content.ClipData
  JSharedPreferences_Editor = interface;//android.content.SharedPreferences$Editor
  JBroadcastReceiver = interface;//android.content.BroadcastReceiver
  JSyncStatusObserver = interface;//android.content.SyncStatusObserver
  JView_DragShadowBuilder = interface;//android.view.View$DragShadowBuilder
  JViewTreeObserver_OnDrawListener = interface;//android.view.ViewTreeObserver$OnDrawListener
  JDashPathEffect = interface;//android.graphics.DashPathEffect
  JClipboardManager = interface;//android.text.ClipboardManager
  JPaint_FontMetrics = interface;//android.graphics.Paint$FontMetrics
  JLayoutInflater_Factory2 = interface;//android.view.LayoutInflater$Factory2
  JLightingColorFilter = interface;//android.graphics.LightingColorFilter
  JRotateDrawable = interface;//android.graphics.drawable.RotateDrawable
  JContentProviderClient = interface;//android.content.ContentProviderClient
  JContentUris = interface;//android.content.ContentUris
  JPaintDrawable = interface;//android.graphics.drawable.PaintDrawable
  JPackageManager_NameNotFoundException = interface;//android.content.pm.PackageManager$NameNotFoundException
  JSurfaceTexture_OnFrameAvailableListener = interface;//android.graphics.SurfaceTexture$OnFrameAvailableListener
  JInterpolator_Result = interface;//android.graphics.Interpolator$Result
  JYuvImage = interface;//android.graphics.YuvImage
  JDrawFilter = interface;//android.graphics.DrawFilter
  JPaintFlagsDrawFilter = interface;//android.graphics.PaintFlagsDrawFilter
  JBitmapRegionDecoder = interface;//android.graphics.BitmapRegionDecoder
  JRegion_Op = interface;//android.graphics.Region$Op
  JColor = interface;//android.graphics.Color
  JCanvas = interface;//android.graphics.Canvas
  JMaskFilter = interface;//android.graphics.MaskFilter
  JEmbossMaskFilter = interface;//android.graphics.EmbossMaskFilter
  JCornerPathEffect = interface;//android.graphics.CornerPathEffect
  JGradientDrawable_Orientation = interface;//android.graphics.drawable.GradientDrawable$Orientation
  JPorterDuff_Mode = interface;//android.graphics.PorterDuff$Mode
  JConfiguration = interface;//android.content.res.Configuration
  JIntentSender = interface;//android.content.IntentSender
  JIntent_FilterComparison = interface;//android.content.Intent$FilterComparison
  JBitmap_CompressFormat = interface;//android.graphics.Bitmap$CompressFormat
  JPathMeasure = interface;//android.graphics.PathMeasure
  JTypedArray = interface;//android.content.res.TypedArray
  JMotionEvent_PointerProperties = interface;//android.view.MotionEvent$PointerProperties
  JInputMethodInfo = interface;//android.view.inputmethod.InputMethodInfo
  JKeyCharacterMap = interface;//android.view.KeyCharacterMap
  JViewGroup_OnHierarchyChangeListener = interface;//android.view.ViewGroup$OnHierarchyChangeListener
  JMenuItem_OnActionExpandListener = interface;//android.view.MenuItem$OnActionExpandListener
  JColorDrawable = interface;//android.graphics.drawable.ColorDrawable
  JEditable = interface;//android.text.Editable
  JBlurMaskFilter = interface;//android.graphics.BlurMaskFilter
  JComposePathEffect = interface;//android.graphics.ComposePathEffect
  JPictureDrawable = interface;//android.graphics.drawable.PictureDrawable
  JContentProvider = interface;//android.content.ContentProvider
  JSearchRecentSuggestionsProvider = interface;//android.content.SearchRecentSuggestionsProvider
  JSubMenu = interface;//android.view.SubMenu
  JLevelListDrawable = interface;//android.graphics.drawable.LevelListDrawable
  JAvoidXfermode_Mode = interface;//android.graphics.AvoidXfermode$Mode
  JExtractedText = interface;//android.view.inputmethod.ExtractedText
  JNinePatchDrawable = interface;//android.graphics.drawable.NinePatchDrawable
  JSyncContext = interface;//android.content.SyncContext
  JApplicationInfo = interface;//android.content.pm.ApplicationInfo
  JView_OnLayoutChangeListener = interface;//android.view.View$OnLayoutChangeListener
  JInstrumentationInfo = interface;//android.content.pm.InstrumentationInfo
  JDialogInterface = interface;//android.content.DialogInterface
  JSQLiteDatabase_CursorFactory = interface;//android.database.sqlite.SQLiteDatabase$CursorFactory
  JIntentFilter_MalformedMimeTypeException = interface;//android.content.IntentFilter$MalformedMimeTypeException
  JViewParent = interface;//android.view.ViewParent
  JPorterDuff = interface;//android.graphics.PorterDuff
  JRectF = interface;//android.graphics.RectF
  JIntent_ShortcutIconResource = interface;//android.content.Intent$ShortcutIconResource
  JKeyEvent_Callback = interface;//android.view.KeyEvent$Callback
  JScaleGestureDetector_SimpleOnScaleGestureListener = interface;//android.view.ScaleGestureDetector$SimpleOnScaleGestureListener
  JStateListDrawable = interface;//android.graphics.drawable.StateListDrawable
  Jcontent_ClipboardManager = interface;//android.content.ClipboardManager
  JPointF = interface;//android.graphics.PointF
  JKeyEvent = interface;//android.view.KeyEvent
  JCursor = interface;//android.database.Cursor
  JCursorLoader = interface;//android.content.CursorLoader
  JClipboardManager_OnPrimaryClipChangedListener = interface;//android.content.ClipboardManager$OnPrimaryClipChangedListener
  JServiceInfo = interface;//android.content.pm.ServiceInfo
  JAnimatable = interface;//android.graphics.drawable.Animatable
  JAvoidXfermode = interface;//android.graphics.AvoidXfermode
  JSharedPreferences_OnSharedPreferenceChangeListener = interface;//android.content.SharedPreferences$OnSharedPreferenceChangeListener
  JDialogInterface_OnCancelListener = interface;//android.content.DialogInterface$OnCancelListener
  JObbInfo = interface;//android.content.res.ObbInfo
  JKeyCharacterMap_KeyData = interface;//android.view.KeyCharacterMap$KeyData
  JEntity_NamedContentValues = interface;//android.content.Entity$NamedContentValues
  JUriMatcher = interface;//android.content.UriMatcher
  JBitmap_Config = interface;//android.graphics.Bitmap$Config
  JColorStateList = interface;//android.content.res.ColorStateList
  JApplicationInfo_DisplayNameComparator = interface;//android.content.pm.ApplicationInfo$DisplayNameComparator
  JInputMethod_SessionCallback = interface;//android.view.inputmethod.InputMethod$SessionCallback
  JViewPropertyAnimator = interface;//android.view.ViewPropertyAnimator
  JRoundRectShape = interface;//android.graphics.drawable.shapes.RoundRectShape
  JDrawableContainer_DrawableContainerState = interface;//android.graphics.drawable.DrawableContainer$DrawableContainerState
  JDrawable_Callback = interface;//android.graphics.drawable.Drawable$Callback
  JSyncResult = interface;//android.content.SyncResult
  JView_AccessibilityDelegate = interface;//android.view.View$AccessibilityDelegate
  JTypeface = interface;//android.graphics.Typeface
  JView_OnSystemUiVisibilityChangeListener = interface;//android.view.View$OnSystemUiVisibilityChangeListener
  JView_OnFocusChangeListener = interface;//android.view.View$OnFocusChangeListener
  JActivityNotFoundException = interface;//android.content.ActivityNotFoundException
  JSQLiteTransactionListener = interface;//android.database.sqlite.SQLiteTransactionListener
  JInputMethodSession_EventCallback = interface;//android.view.inputmethod.InputMethodSession$EventCallback
  JFeatureInfo = interface;//android.content.pm.FeatureInfo
  JGestureDetector_OnDoubleTapListener = interface;//android.view.GestureDetector$OnDoubleTapListener
  JAssetFileDescriptor_AutoCloseInputStream = interface;//android.content.res.AssetFileDescriptor$AutoCloseInputStream
  JWindowManager_LayoutParams = interface;//android.view.WindowManager$LayoutParams
  JActionMode_Callback = interface;//android.view.ActionMode$Callback
  JKeyListener = interface;//android.text.method.KeyListener
  JView_OnHoverListener = interface;//android.view.View$OnHoverListener
  JView_OnDragListener = interface;//android.view.View$OnDragListener
  JPorterDuffXfermode = interface;//android.graphics.PorterDuffXfermode
  JTouchDelegate = interface;//android.view.TouchDelegate
  JLayout = interface;//android.text.Layout

JDialogInterface_OnKeyListenerClass = interface(IJavaClass)
['{F95ED60A-27E5-4A1C-8081-E4646C4AF61D}']
end;

[JavaSignature('android/content/DialogInterface$OnKeyListener')]
JDialogInterface_OnKeyListener = interface(IJavaInstance)
['{597D3989-B3F3-4AE2-8521-1D1141FBE3E3}']
  {Methods}
  function onKey(dialog: JDialogInterface; keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
end;
TJDialogInterface_OnKeyListener = class(TJavaGenericImport<JDialogInterface_OnKeyListenerClass, JDialogInterface_OnKeyListener>) end;

JSurfaceHolder_CallbackClass = interface(IJavaClass)
['{97AD9204-F7F7-4094-9AF1-0C9426C9FFC4}']
end;

[JavaSignature('android/view/SurfaceHolder$Callback')]
JSurfaceHolder_Callback = interface(IJavaInstance)
['{A126B8BF-D9CA-4D5B-95A5-5ADC0EAAC38B}']
  {Methods}
  procedure surfaceChanged(holder: JSurfaceHolder; format: Integer; width: Integer; height: Integer); cdecl;
  procedure surfaceCreated(holder: JSurfaceHolder); cdecl;
  procedure surfaceDestroyed(holder: JSurfaceHolder); cdecl;
end;
TJSurfaceHolder_Callback = class(TJavaGenericImport<JSurfaceHolder_CallbackClass, JSurfaceHolder_Callback>) end;

JInputEventClass = interface(JObjectClass)
['{2144AA86-B47F-4D38-844A-F4E2B7EA242E}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/view/InputEvent')]
JInputEvent = interface(JObject)
['{8E25F546-E8FF-4C54-BCAA-733E5AF3523C}']
  {Methods}
  function describeContents: Integer; cdecl;
  function getDevice: JInputDevice; cdecl;
  function getDeviceId: Integer; cdecl;
  function getEventTime: Int64; cdecl;
  function getSource: Integer; cdecl;
end;
TJInputEvent = class(TJavaGenericImport<JInputEventClass, JInputEvent>) end;

JPathEffectClass = interface(JObjectClass)
['{914257B7-DE57-4297-93FD-4051CC297EE5}']
  {Methods}
  function init: JPathEffect; cdecl;
end;

[JavaSignature('android/graphics/PathEffect')]
JPathEffect = interface(JObject)
['{69C98CE4-6442-47CC-990F-EEF2AD4863DF}']
end;
TJPathEffect = class(TJavaGenericImport<JPathEffectClass, JPathEffect>) end;

JPathDashPathEffectClass = interface(JPathEffectClass)
['{4AD7DE02-F54C-4AEE-A162-AFDF4C6E220C}']
  {Methods}
  function init(shape: JPath; advance: Single; phase: Single; style: JPathDashPathEffect_Style): JPathDashPathEffect; cdecl;
end;

[JavaSignature('android/graphics/PathDashPathEffect')]
JPathDashPathEffect = interface(JPathEffect)
['{411DA0F4-339D-41FF-95AB-8CF6B41115F5}']
end;
TJPathDashPathEffect = class(TJavaGenericImport<JPathDashPathEffectClass, JPathDashPathEffect>) end;

JSumPathEffectClass = interface(JPathEffectClass)
['{71F21540-5C34-4BF7-BD0A-3751401D0E7A}']
  {Methods}
  function init(first: JPathEffect; second: JPathEffect): JSumPathEffect; cdecl;
end;

[JavaSignature('android/graphics/SumPathEffect')]
JSumPathEffect = interface(JPathEffect)
['{5E43867D-8903-47C9-A653-CE79CD40E9B9}']
end;
TJSumPathEffect = class(TJavaGenericImport<JSumPathEffectClass, JSumPathEffect>) end;

JView_OnClickListenerClass = interface(IJavaClass)
['{D4FE7E96-D2E9-4720-890E-0B03CBBFCB1B}']
end;

[JavaSignature('android/view/View$OnClickListener')]
JView_OnClickListener = interface(IJavaInstance)
['{7603153E-EEF8-4F60-9882-FF5490A5AF28}']
  {Methods}
  procedure onClick(v: JView); cdecl;
end;
TJView_OnClickListener = class(TJavaGenericImport<JView_OnClickListenerClass, JView_OnClickListener>) end;

JDialogInterface_OnShowListenerClass = interface(IJavaClass)
['{88EC37A6-8599-4E19-8F35-E10A8EEA97E1}']
end;

[JavaSignature('android/content/DialogInterface$OnShowListener')]
JDialogInterface_OnShowListener = interface(IJavaInstance)
['{09556630-5FED-493C-A1C0-C42612C7CEDB}']
  {Methods}
  procedure onShow(dialog: JDialogInterface); cdecl;
end;
TJDialogInterface_OnShowListener = class(TJavaGenericImport<JDialogInterface_OnShowListenerClass, JDialogInterface_OnShowListener>) end;

JSQLiteClosableClass = interface(JObjectClass)
['{CA8FF629-F608-4D9F-9C8C-24FEBC885043}']
  {Methods}
  function init: JSQLiteClosable; cdecl;
end;

[JavaSignature('android/database/sqlite/SQLiteClosable')]
JSQLiteClosable = interface(JObject)
['{96B1666D-82B6-4E92-AB43-CF56279857DC}']
  {Methods}
  procedure acquireReference; cdecl;
  procedure close; cdecl;
  procedure releaseReference; cdecl;
  procedure releaseReferenceFromContainer; cdecl;//Deprecated
end;
TJSQLiteClosable = class(TJavaGenericImport<JSQLiteClosableClass, JSQLiteClosable>) end;

JSQLiteDatabaseClass = interface(JSQLiteClosableClass)
['{1AF1FB6C-87F2-4815-B50E-BB3838935F27}']
  {Property Methods}
  function _GetCONFLICT_ABORT: Integer;
  function _GetCONFLICT_FAIL: Integer;
  function _GetCONFLICT_IGNORE: Integer;
  function _GetCONFLICT_NONE: Integer;
  function _GetCONFLICT_REPLACE: Integer;
  function _GetCONFLICT_ROLLBACK: Integer;
  function _GetCREATE_IF_NECESSARY: Integer;
  function _GetENABLE_WRITE_AHEAD_LOGGING: Integer;
  function _GetMAX_SQL_CACHE_SIZE: Integer;
  function _GetNO_LOCALIZED_COLLATORS: Integer;
  function _GetOPEN_READONLY: Integer;
  function _GetOPEN_READWRITE: Integer;
  function _GetSQLITE_MAX_LIKE_PATTERN_LENGTH: Integer;
  {Methods}
  function create(factory: JSQLiteDatabase_CursorFactory): JSQLiteDatabase; cdecl;
  function deleteDatabase(file_: JFile): Boolean; cdecl;
  function findEditTable(tables: JString): JString; cdecl;
  function openDatabase(path: JString; factory: JSQLiteDatabase_CursorFactory; flags: Integer): JSQLiteDatabase; cdecl; overload;
  function openDatabase(path: JString; factory: JSQLiteDatabase_CursorFactory; flags: Integer; errorHandler: JDatabaseErrorHandler): JSQLiteDatabase; cdecl; overload;
  function openOrCreateDatabase(file_: JFile; factory: JSQLiteDatabase_CursorFactory): JSQLiteDatabase; cdecl; overload;
  function openOrCreateDatabase(path: JString; factory: JSQLiteDatabase_CursorFactory): JSQLiteDatabase; cdecl; overload;
  function openOrCreateDatabase(path: JString; factory: JSQLiteDatabase_CursorFactory; errorHandler: JDatabaseErrorHandler): JSQLiteDatabase; cdecl; overload;
  function releaseMemory: Integer; cdecl;
  {Properties}
  property CONFLICT_ABORT: Integer read _GetCONFLICT_ABORT;
  property CONFLICT_FAIL: Integer read _GetCONFLICT_FAIL;
  property CONFLICT_IGNORE: Integer read _GetCONFLICT_IGNORE;
  property CONFLICT_NONE: Integer read _GetCONFLICT_NONE;
  property CONFLICT_REPLACE: Integer read _GetCONFLICT_REPLACE;
  property CONFLICT_ROLLBACK: Integer read _GetCONFLICT_ROLLBACK;
  property CREATE_IF_NECESSARY: Integer read _GetCREATE_IF_NECESSARY;
  property ENABLE_WRITE_AHEAD_LOGGING: Integer read _GetENABLE_WRITE_AHEAD_LOGGING;
  property MAX_SQL_CACHE_SIZE: Integer read _GetMAX_SQL_CACHE_SIZE;
  property NO_LOCALIZED_COLLATORS: Integer read _GetNO_LOCALIZED_COLLATORS;
  property OPEN_READONLY: Integer read _GetOPEN_READONLY;
  property OPEN_READWRITE: Integer read _GetOPEN_READWRITE;
  property SQLITE_MAX_LIKE_PATTERN_LENGTH: Integer read _GetSQLITE_MAX_LIKE_PATTERN_LENGTH;
end;

[JavaSignature('android/database/sqlite/SQLiteDatabase')]
JSQLiteDatabase = interface(JSQLiteClosable)
['{4721F6D4-E806-4FBB-B2F1-90375EDE6787}']
  {Methods}
  procedure beginTransaction; cdecl;
  procedure beginTransactionNonExclusive; cdecl;
  procedure beginTransactionWithListener(transactionListener: JSQLiteTransactionListener); cdecl;
  procedure beginTransactionWithListenerNonExclusive(transactionListener: JSQLiteTransactionListener); cdecl;
  function compileStatement(sql: JString): JSQLiteStatement; cdecl;
  function delete(table: JString; whereClause: JString; whereArgs: TJavaObjectArray<JString>): Integer; cdecl;
  procedure disableWriteAheadLogging; cdecl;
  function enableWriteAheadLogging: Boolean; cdecl;
  procedure endTransaction; cdecl;
  procedure execSQL(sql: JString); cdecl; overload;
  procedure execSQL(sql: JString; bindArgs: TJavaObjectArray<JObject>); cdecl; overload;
  function getAttachedDbs: JList; cdecl;
  function getMaximumSize: Int64; cdecl;
  function getPageSize: Int64; cdecl;
  function getPath: JString; cdecl;
  function getSyncedTables: JMap; cdecl;//Deprecated
  function getVersion: Integer; cdecl;
  function inTransaction: Boolean; cdecl;
  function insert(table: JString; nullColumnHack: JString; values: JContentValues): Int64; cdecl;
  function insertOrThrow(table: JString; nullColumnHack: JString; values: JContentValues): Int64; cdecl;
  function insertWithOnConflict(table: JString; nullColumnHack: JString; initialValues: JContentValues; conflictAlgorithm: Integer): Int64; cdecl;
  function isDatabaseIntegrityOk: Boolean; cdecl;
  function isDbLockedByCurrentThread: Boolean; cdecl;
  function isDbLockedByOtherThreads: Boolean; cdecl;//Deprecated
  function isOpen: Boolean; cdecl;
  function isReadOnly: Boolean; cdecl;
  function isWriteAheadLoggingEnabled: Boolean; cdecl;
  procedure markTableSyncable(table: JString; deletedTable: JString); cdecl; overload;//Deprecated
  procedure markTableSyncable(table: JString; foreignKey: JString; updateTable: JString); cdecl; overload;//Deprecated
  function needUpgrade(newVersion: Integer): Boolean; cdecl;
  function query(distinct: Boolean; table: JString; columns: TJavaObjectArray<JString>; selection: JString; selectionArgs: TJavaObjectArray<JString>; groupBy: JString; having: JString; orderBy: JString; limit: JString): JCursor; cdecl; overload;
  function query(distinct: Boolean; table: JString; columns: TJavaObjectArray<JString>; selection: JString; selectionArgs: TJavaObjectArray<JString>; groupBy: JString; having: JString; orderBy: JString; limit: JString; cancellationSignal: JCancellationSignal): JCursor; cdecl; overload;
  function query(table: JString; columns: TJavaObjectArray<JString>; selection: JString; selectionArgs: TJavaObjectArray<JString>; groupBy: JString; having: JString; orderBy: JString): JCursor; cdecl; overload;
  function query(table: JString; columns: TJavaObjectArray<JString>; selection: JString; selectionArgs: TJavaObjectArray<JString>; groupBy: JString; having: JString; orderBy: JString; limit: JString): JCursor; cdecl; overload;
  function queryWithFactory(cursorFactory: JSQLiteDatabase_CursorFactory; distinct: Boolean; table: JString; columns: TJavaObjectArray<JString>; selection: JString; selectionArgs: TJavaObjectArray<JString>; groupBy: JString; having: JString; orderBy: JString; limit: JString): JCursor; cdecl; overload;
  function queryWithFactory(cursorFactory: JSQLiteDatabase_CursorFactory; distinct: Boolean; table: JString; columns: TJavaObjectArray<JString>; selection: JString; selectionArgs: TJavaObjectArray<JString>; groupBy: JString; having: JString; orderBy: JString; limit: JString; cancellationSignal: JCancellationSignal): JCursor; cdecl; overload;
  function rawQuery(sql: JString; selectionArgs: TJavaObjectArray<JString>): JCursor; cdecl; overload;
  function rawQuery(sql: JString; selectionArgs: TJavaObjectArray<JString>; cancellationSignal: JCancellationSignal): JCursor; cdecl; overload;
  function rawQueryWithFactory(cursorFactory: JSQLiteDatabase_CursorFactory; sql: JString; selectionArgs: TJavaObjectArray<JString>; editTable: JString): JCursor; cdecl; overload;
  function rawQueryWithFactory(cursorFactory: JSQLiteDatabase_CursorFactory; sql: JString; selectionArgs: TJavaObjectArray<JString>; editTable: JString; cancellationSignal: JCancellationSignal): JCursor; cdecl; overload;
  function replace(table: JString; nullColumnHack: JString; initialValues: JContentValues): Int64; cdecl;
  function replaceOrThrow(table: JString; nullColumnHack: JString; initialValues: JContentValues): Int64; cdecl;
  procedure setForeignKeyConstraintsEnabled(enable: Boolean); cdecl;
  procedure setLocale(locale: JLocale); cdecl;
  procedure setLockingEnabled(lockingEnabled: Boolean); cdecl;//Deprecated
  procedure setMaxSqlCacheSize(cacheSize: Integer); cdecl;
  function setMaximumSize(numBytes: Int64): Int64; cdecl;
  procedure setPageSize(numBytes: Int64); cdecl;
  procedure setTransactionSuccessful; cdecl;
  procedure setVersion(version: Integer); cdecl;
  function toString: JString; cdecl;
  function update(table: JString; values: JContentValues; whereClause: JString; whereArgs: TJavaObjectArray<JString>): Integer; cdecl;
  function updateWithOnConflict(table: JString; values: JContentValues; whereClause: JString; whereArgs: TJavaObjectArray<JString>; conflictAlgorithm: Integer): Integer; cdecl;
  function yieldIfContended: Boolean; cdecl;//Deprecated
  function yieldIfContendedSafely: Boolean; cdecl; overload;
  function yieldIfContendedSafely(sleepAfterYieldDelay: Int64): Boolean; cdecl; overload;
end;
TJSQLiteDatabase = class(TJavaGenericImport<JSQLiteDatabaseClass, JSQLiteDatabase>) end;

JMotionEventClass = interface(JInputEventClass)
['{1923D7DE-9206-4376-829C-20D14334553B}']
  {Property Methods}
  function _GetACTION_CANCEL: Integer;
  function _GetACTION_DOWN: Integer;
  function _GetACTION_HOVER_ENTER: Integer;
  function _GetACTION_HOVER_EXIT: Integer;
  function _GetACTION_HOVER_MOVE: Integer;
  function _GetACTION_MASK: Integer;
  function _GetACTION_MOVE: Integer;
  function _GetACTION_OUTSIDE: Integer;
  function _GetACTION_POINTER_1_DOWN: Integer;
  function _GetACTION_POINTER_1_UP: Integer;
  function _GetACTION_POINTER_2_DOWN: Integer;
  function _GetACTION_POINTER_2_UP: Integer;
  function _GetACTION_POINTER_3_DOWN: Integer;
  function _GetACTION_POINTER_3_UP: Integer;
  function _GetACTION_POINTER_DOWN: Integer;
  function _GetACTION_POINTER_ID_MASK: Integer;
  function _GetACTION_POINTER_ID_SHIFT: Integer;
  function _GetACTION_POINTER_INDEX_MASK: Integer;
  function _GetACTION_POINTER_INDEX_SHIFT: Integer;
  function _GetACTION_POINTER_UP: Integer;
  function _GetACTION_SCROLL: Integer;
  function _GetACTION_UP: Integer;
  function _GetAXIS_BRAKE: Integer;
  function _GetAXIS_DISTANCE: Integer;
  function _GetAXIS_GAS: Integer;
  function _GetAXIS_GENERIC_1: Integer;
  function _GetAXIS_GENERIC_10: Integer;
  function _GetAXIS_GENERIC_11: Integer;
  function _GetAXIS_GENERIC_12: Integer;
  function _GetAXIS_GENERIC_13: Integer;
  function _GetAXIS_GENERIC_14: Integer;
  function _GetAXIS_GENERIC_15: Integer;
  function _GetAXIS_GENERIC_16: Integer;
  function _GetAXIS_GENERIC_2: Integer;
  function _GetAXIS_GENERIC_3: Integer;
  function _GetAXIS_GENERIC_4: Integer;
  function _GetAXIS_GENERIC_5: Integer;
  function _GetAXIS_GENERIC_6: Integer;
  function _GetAXIS_GENERIC_7: Integer;
  function _GetAXIS_GENERIC_8: Integer;
  function _GetAXIS_GENERIC_9: Integer;
  function _GetAXIS_HAT_X: Integer;
  function _GetAXIS_HAT_Y: Integer;
  function _GetAXIS_HSCROLL: Integer;
  function _GetAXIS_LTRIGGER: Integer;
  function _GetAXIS_ORIENTATION: Integer;
  function _GetAXIS_PRESSURE: Integer;
  function _GetAXIS_RTRIGGER: Integer;
  function _GetAXIS_RUDDER: Integer;
  function _GetAXIS_RX: Integer;
  function _GetAXIS_RY: Integer;
  function _GetAXIS_RZ: Integer;
  function _GetAXIS_SIZE: Integer;
  function _GetAXIS_THROTTLE: Integer;
  function _GetAXIS_TILT: Integer;
  function _GetAXIS_TOOL_MAJOR: Integer;
  function _GetAXIS_TOOL_MINOR: Integer;
  function _GetAXIS_TOUCH_MAJOR: Integer;
  function _GetAXIS_TOUCH_MINOR: Integer;
  function _GetAXIS_VSCROLL: Integer;
  function _GetAXIS_WHEEL: Integer;
  function _GetAXIS_X: Integer;
  function _GetAXIS_Y: Integer;
  function _GetAXIS_Z: Integer;
  function _GetBUTTON_BACK: Integer;
  function _GetBUTTON_FORWARD: Integer;
  function _GetBUTTON_PRIMARY: Integer;
  function _GetBUTTON_SECONDARY: Integer;
  function _GetBUTTON_TERTIARY: Integer;
  function _GetCREATOR: JParcelable_Creator;
  function _GetEDGE_BOTTOM: Integer;
  function _GetEDGE_LEFT: Integer;
  function _GetEDGE_RIGHT: Integer;
  function _GetEDGE_TOP: Integer;
  function _GetFLAG_WINDOW_IS_OBSCURED: Integer;
  function _GetINVALID_POINTER_ID: Integer;
  function _GetTOOL_TYPE_ERASER: Integer;
  function _GetTOOL_TYPE_FINGER: Integer;
  function _GetTOOL_TYPE_MOUSE: Integer;
  function _GetTOOL_TYPE_STYLUS: Integer;
  function _GetTOOL_TYPE_UNKNOWN: Integer;
  {Methods}
  function axisFromString(symbolicName: JString): Integer; cdecl;
  function axisToString(axis: Integer): JString; cdecl;
  function obtain(downTime: Int64; eventTime: Int64; action: Integer; pointerCount: Integer; pointerProperties: TJavaObjectArray<JMotionEvent_PointerProperties>; pointerCoords: TJavaObjectArray<JMotionEvent_PointerCoords>; metaState: Integer; buttonState: Integer; xPrecision: Single; yPrecision: Single; deviceId: Integer; edgeFlags: Integer; source: Integer; flags: Integer): JMotionEvent; cdecl; overload;
  function obtain(downTime: Int64; eventTime: Int64; action: Integer; pointerCount: Integer; pointerIds: TJavaArray<Integer>; pointerCoords: TJavaObjectArray<JMotionEvent_PointerCoords>; metaState: Integer; xPrecision: Single; yPrecision: Single; deviceId: Integer; edgeFlags: Integer; source: Integer; flags: Integer): JMotionEvent; cdecl; overload;//Deprecated
  function obtain(downTime: Int64; eventTime: Int64; action: Integer; x: Single; y: Single; pressure: Single; size: Single; metaState: Integer; xPrecision: Single; yPrecision: Single; deviceId: Integer; edgeFlags: Integer): JMotionEvent; cdecl; overload;
  function obtain(downTime: Int64; eventTime: Int64; action: Integer; pointerCount: Integer; x: Single; y: Single; pressure: Single; size: Single; metaState: Integer; xPrecision: Single; yPrecision: Single; deviceId: Integer; edgeFlags: Integer): JMotionEvent; cdecl; overload;//Deprecated
  function obtain(downTime: Int64; eventTime: Int64; action: Integer; x: Single; y: Single; metaState: Integer): JMotionEvent; cdecl; overload;
  function obtain(other: JMotionEvent): JMotionEvent; cdecl; overload;
  function obtainNoHistory(other: JMotionEvent): JMotionEvent; cdecl;
  {Properties}
  property ACTION_CANCEL: Integer read _GetACTION_CANCEL;
  property ACTION_DOWN: Integer read _GetACTION_DOWN;
  property ACTION_HOVER_ENTER: Integer read _GetACTION_HOVER_ENTER;
  property ACTION_HOVER_EXIT: Integer read _GetACTION_HOVER_EXIT;
  property ACTION_HOVER_MOVE: Integer read _GetACTION_HOVER_MOVE;
  property ACTION_MASK: Integer read _GetACTION_MASK;
  property ACTION_MOVE: Integer read _GetACTION_MOVE;
  property ACTION_OUTSIDE: Integer read _GetACTION_OUTSIDE;
  property ACTION_POINTER_1_DOWN: Integer read _GetACTION_POINTER_1_DOWN;
  property ACTION_POINTER_1_UP: Integer read _GetACTION_POINTER_1_UP;
  property ACTION_POINTER_2_DOWN: Integer read _GetACTION_POINTER_2_DOWN;
  property ACTION_POINTER_2_UP: Integer read _GetACTION_POINTER_2_UP;
  property ACTION_POINTER_3_DOWN: Integer read _GetACTION_POINTER_3_DOWN;
  property ACTION_POINTER_3_UP: Integer read _GetACTION_POINTER_3_UP;
  property ACTION_POINTER_DOWN: Integer read _GetACTION_POINTER_DOWN;
  property ACTION_POINTER_ID_MASK: Integer read _GetACTION_POINTER_ID_MASK;
  property ACTION_POINTER_ID_SHIFT: Integer read _GetACTION_POINTER_ID_SHIFT;
  property ACTION_POINTER_INDEX_MASK: Integer read _GetACTION_POINTER_INDEX_MASK;
  property ACTION_POINTER_INDEX_SHIFT: Integer read _GetACTION_POINTER_INDEX_SHIFT;
  property ACTION_POINTER_UP: Integer read _GetACTION_POINTER_UP;
  property ACTION_SCROLL: Integer read _GetACTION_SCROLL;
  property ACTION_UP: Integer read _GetACTION_UP;
  property AXIS_BRAKE: Integer read _GetAXIS_BRAKE;
  property AXIS_DISTANCE: Integer read _GetAXIS_DISTANCE;
  property AXIS_GAS: Integer read _GetAXIS_GAS;
  property AXIS_GENERIC_1: Integer read _GetAXIS_GENERIC_1;
  property AXIS_GENERIC_10: Integer read _GetAXIS_GENERIC_10;
  property AXIS_GENERIC_11: Integer read _GetAXIS_GENERIC_11;
  property AXIS_GENERIC_12: Integer read _GetAXIS_GENERIC_12;
  property AXIS_GENERIC_13: Integer read _GetAXIS_GENERIC_13;
  property AXIS_GENERIC_14: Integer read _GetAXIS_GENERIC_14;
  property AXIS_GENERIC_15: Integer read _GetAXIS_GENERIC_15;
  property AXIS_GENERIC_16: Integer read _GetAXIS_GENERIC_16;
  property AXIS_GENERIC_2: Integer read _GetAXIS_GENERIC_2;
  property AXIS_GENERIC_3: Integer read _GetAXIS_GENERIC_3;
  property AXIS_GENERIC_4: Integer read _GetAXIS_GENERIC_4;
  property AXIS_GENERIC_5: Integer read _GetAXIS_GENERIC_5;
  property AXIS_GENERIC_6: Integer read _GetAXIS_GENERIC_6;
  property AXIS_GENERIC_7: Integer read _GetAXIS_GENERIC_7;
  property AXIS_GENERIC_8: Integer read _GetAXIS_GENERIC_8;
  property AXIS_GENERIC_9: Integer read _GetAXIS_GENERIC_9;
  property AXIS_HAT_X: Integer read _GetAXIS_HAT_X;
  property AXIS_HAT_Y: Integer read _GetAXIS_HAT_Y;
  property AXIS_HSCROLL: Integer read _GetAXIS_HSCROLL;
  property AXIS_LTRIGGER: Integer read _GetAXIS_LTRIGGER;
  property AXIS_ORIENTATION: Integer read _GetAXIS_ORIENTATION;
  property AXIS_PRESSURE: Integer read _GetAXIS_PRESSURE;
  property AXIS_RTRIGGER: Integer read _GetAXIS_RTRIGGER;
  property AXIS_RUDDER: Integer read _GetAXIS_RUDDER;
  property AXIS_RX: Integer read _GetAXIS_RX;
  property AXIS_RY: Integer read _GetAXIS_RY;
  property AXIS_RZ: Integer read _GetAXIS_RZ;
  property AXIS_SIZE: Integer read _GetAXIS_SIZE;
  property AXIS_THROTTLE: Integer read _GetAXIS_THROTTLE;
  property AXIS_TILT: Integer read _GetAXIS_TILT;
  property AXIS_TOOL_MAJOR: Integer read _GetAXIS_TOOL_MAJOR;
  property AXIS_TOOL_MINOR: Integer read _GetAXIS_TOOL_MINOR;
  property AXIS_TOUCH_MAJOR: Integer read _GetAXIS_TOUCH_MAJOR;
  property AXIS_TOUCH_MINOR: Integer read _GetAXIS_TOUCH_MINOR;
  property AXIS_VSCROLL: Integer read _GetAXIS_VSCROLL;
  property AXIS_WHEEL: Integer read _GetAXIS_WHEEL;
  property AXIS_X: Integer read _GetAXIS_X;
  property AXIS_Y: Integer read _GetAXIS_Y;
  property AXIS_Z: Integer read _GetAXIS_Z;
  property BUTTON_BACK: Integer read _GetBUTTON_BACK;
  property BUTTON_FORWARD: Integer read _GetBUTTON_FORWARD;
  property BUTTON_PRIMARY: Integer read _GetBUTTON_PRIMARY;
  property BUTTON_SECONDARY: Integer read _GetBUTTON_SECONDARY;
  property BUTTON_TERTIARY: Integer read _GetBUTTON_TERTIARY;
{CREATOR is defined in parent interface}
  property EDGE_BOTTOM: Integer read _GetEDGE_BOTTOM;
  property EDGE_LEFT: Integer read _GetEDGE_LEFT;
  property EDGE_RIGHT: Integer read _GetEDGE_RIGHT;
  property EDGE_TOP: Integer read _GetEDGE_TOP;
  property FLAG_WINDOW_IS_OBSCURED: Integer read _GetFLAG_WINDOW_IS_OBSCURED;
  property INVALID_POINTER_ID: Integer read _GetINVALID_POINTER_ID;
  property TOOL_TYPE_ERASER: Integer read _GetTOOL_TYPE_ERASER;
  property TOOL_TYPE_FINGER: Integer read _GetTOOL_TYPE_FINGER;
  property TOOL_TYPE_MOUSE: Integer read _GetTOOL_TYPE_MOUSE;
  property TOOL_TYPE_STYLUS: Integer read _GetTOOL_TYPE_STYLUS;
  property TOOL_TYPE_UNKNOWN: Integer read _GetTOOL_TYPE_UNKNOWN;
end;

[JavaSignature('android/view/MotionEvent')]
JMotionEvent = interface(JInputEvent)
['{0EC8B7D2-BB97-467E-B225-EF2DBA06BB34}']
  {Methods}
  procedure addBatch(eventTime: Int64; x: Single; y: Single; pressure: Single; size: Single; metaState: Integer); cdecl; overload;
  procedure addBatch(eventTime: Int64; pointerCoords: TJavaObjectArray<JMotionEvent_PointerCoords>; metaState: Integer); cdecl; overload;
  function findPointerIndex(pointerId: Integer): Integer; cdecl;
  function getAction: Integer; cdecl;
  function getActionIndex: Integer; cdecl;
  function getActionMasked: Integer; cdecl;
  function getAxisValue(axis: Integer): Single; cdecl; overload;
  function getAxisValue(axis: Integer; pointerIndex: Integer): Single; cdecl; overload;
  function getButtonState: Integer; cdecl;
  function getDeviceId: Integer; cdecl;
  function getDownTime: Int64; cdecl;
  function getEdgeFlags: Integer; cdecl;
  function getEventTime: Int64; cdecl;
  function getFlags: Integer; cdecl;
  function getHistoricalAxisValue(axis: Integer; pos: Integer): Single; cdecl; overload;
  function getHistoricalAxisValue(axis: Integer; pointerIndex: Integer; pos: Integer): Single; cdecl; overload;
  function getHistoricalEventTime(pos: Integer): Int64; cdecl;
  function getHistoricalOrientation(pos: Integer): Single; cdecl; overload;
  function getHistoricalOrientation(pointerIndex: Integer; pos: Integer): Single; cdecl; overload;
  procedure getHistoricalPointerCoords(pointerIndex: Integer; pos: Integer; outPointerCoords: JMotionEvent_PointerCoords); cdecl;
  function getHistoricalPressure(pos: Integer): Single; cdecl; overload;
  function getHistoricalPressure(pointerIndex: Integer; pos: Integer): Single; cdecl; overload;
  function getHistoricalSize(pos: Integer): Single; cdecl; overload;
  function getHistoricalSize(pointerIndex: Integer; pos: Integer): Single; cdecl; overload;
  function getHistoricalToolMajor(pos: Integer): Single; cdecl; overload;
  function getHistoricalToolMajor(pointerIndex: Integer; pos: Integer): Single; cdecl; overload;
  function getHistoricalToolMinor(pos: Integer): Single; cdecl; overload;
  function getHistoricalToolMinor(pointerIndex: Integer; pos: Integer): Single; cdecl; overload;
  function getHistoricalTouchMajor(pos: Integer): Single; cdecl; overload;
  function getHistoricalTouchMajor(pointerIndex: Integer; pos: Integer): Single; cdecl; overload;
  function getHistoricalTouchMinor(pos: Integer): Single; cdecl; overload;
  function getHistoricalTouchMinor(pointerIndex: Integer; pos: Integer): Single; cdecl; overload;
  function getHistoricalX(pos: Integer): Single; cdecl; overload;
  function getHistoricalX(pointerIndex: Integer; pos: Integer): Single; cdecl; overload;
  function getHistoricalY(pos: Integer): Single; cdecl; overload;
  function getHistoricalY(pointerIndex: Integer; pos: Integer): Single; cdecl; overload;
  function getHistorySize: Integer; cdecl;
  function getMetaState: Integer; cdecl;
  function getOrientation: Single; cdecl; overload;
  function getOrientation(pointerIndex: Integer): Single; cdecl; overload;
  procedure getPointerCoords(pointerIndex: Integer; outPointerCoords: JMotionEvent_PointerCoords); cdecl;
  function getPointerCount: Integer; cdecl;
  function getPointerId(pointerIndex: Integer): Integer; cdecl;
  procedure getPointerProperties(pointerIndex: Integer; outPointerProperties: JMotionEvent_PointerProperties); cdecl;
  function getPressure: Single; cdecl; overload;
  function getPressure(pointerIndex: Integer): Single; cdecl; overload;
  function getRawX: Single; cdecl;
  function getRawY: Single; cdecl;
  function getSize: Single; cdecl; overload;
  function getSize(pointerIndex: Integer): Single; cdecl; overload;
  function getSource: Integer; cdecl;
  function getToolMajor: Single; cdecl; overload;
  function getToolMajor(pointerIndex: Integer): Single; cdecl; overload;
  function getToolMinor: Single; cdecl; overload;
  function getToolMinor(pointerIndex: Integer): Single; cdecl; overload;
  function getToolType(pointerIndex: Integer): Integer; cdecl;
  function getTouchMajor: Single; cdecl; overload;
  function getTouchMajor(pointerIndex: Integer): Single; cdecl; overload;
  function getTouchMinor: Single; cdecl; overload;
  function getTouchMinor(pointerIndex: Integer): Single; cdecl; overload;
  function getX: Single; cdecl; overload;
  function getX(pointerIndex: Integer): Single; cdecl; overload;
  function getXPrecision: Single; cdecl;
  function getY: Single; cdecl; overload;
  function getY(pointerIndex: Integer): Single; cdecl; overload;
  function getYPrecision: Single; cdecl;
  procedure offsetLocation(deltaX: Single; deltaY: Single); cdecl;
  procedure recycle; cdecl;
  procedure setAction(action: Integer); cdecl;
  procedure setEdgeFlags(flags: Integer); cdecl;
  procedure setLocation(x: Single; y: Single); cdecl;
  procedure setSource(source: Integer); cdecl;
  function toString: JString; cdecl;
  procedure transform(matrix: JMatrix); cdecl;
  procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
end;
TJMotionEvent = class(TJavaGenericImport<JMotionEventClass, JMotionEvent>) end;

JInputMethodClass = interface(IJavaClass)
['{A413E3D4-C4F1-487F-90E7-C066E4C38802}']
  {Property Methods}
  function _GetSERVICE_INTERFACE: JString;
  function _GetSERVICE_META_DATA: JString;
  function _GetSHOW_EXPLICIT: Integer;
  function _GetSHOW_FORCED: Integer;
  {Properties}
  property SERVICE_INTERFACE: JString read _GetSERVICE_INTERFACE;
  property SERVICE_META_DATA: JString read _GetSERVICE_META_DATA;
  property SHOW_EXPLICIT: Integer read _GetSHOW_EXPLICIT;
  property SHOW_FORCED: Integer read _GetSHOW_FORCED;
end;

[JavaSignature('android/view/inputmethod/InputMethod')]
JInputMethod = interface(IJavaInstance)
['{072935DE-7CB2-43CA-A3F5-024BA6FC5F04}']
  {Methods}
  procedure attachToken(token: JIBinder); cdecl;
  procedure bindInput(binding: JInputBinding); cdecl;
  procedure changeInputMethodSubtype(subtype: JInputMethodSubtype); cdecl;
  procedure createSession(callback: JInputMethod_SessionCallback); cdecl;
  procedure hideSoftInput(flags: Integer; resultReceiver: JResultReceiver); cdecl;
  procedure restartInput(inputConnection: JInputConnection; attribute: JEditorInfo); cdecl;
  procedure revokeSession(session: JInputMethodSession); cdecl;
  procedure setSessionEnabled(session: JInputMethodSession; enabled: Boolean); cdecl;
  procedure showSoftInput(flags: Integer; resultReceiver: JResultReceiver); cdecl;
  procedure startInput(inputConnection: JInputConnection; info: JEditorInfo); cdecl;
  procedure unbindInput; cdecl;
end;
TJInputMethod = class(TJavaGenericImport<JInputMethodClass, JInputMethod>) end;

JSyncInfoClass = interface(JObjectClass)
['{71459AA9-8F8B-461C-B76E-F236B51008DA}']
end;

[JavaSignature('android/content/SyncInfo')]
JSyncInfo = interface(JObject)
['{0212F6E9-4CE7-4BE6-828A-B7275E3BCD33}']
  {Property Methods}
  function _Getauthority: JString;
  function _GetstartTime: Int64;
  {Properties}
  property authority: JString read _Getauthority;
  property startTime: Int64 read _GetstartTime;
end;
TJSyncInfo = class(TJavaGenericImport<JSyncInfoClass, JSyncInfo>) end;

JLayout_DirectionsClass = interface(JObjectClass)
['{2956EB3F-0228-4359-93CD-12038DA23E7E}']
end;

[JavaSignature('android/text/Layout$Directions')]
JLayout_Directions = interface(JObject)
['{C170D193-15D6-4B26-AAB2-979CC7FBA049}']
end;
TJLayout_Directions = class(TJavaGenericImport<JLayout_DirectionsClass, JLayout_Directions>) end;

JMenuClass = interface(IJavaClass)
['{DB3A723B-6B18-407D-942F-D801B1260750}']
  {Property Methods}
  function _GetCATEGORY_ALTERNATIVE: Integer;
  function _GetCATEGORY_CONTAINER: Integer;
  function _GetCATEGORY_SECONDARY: Integer;
  function _GetCATEGORY_SYSTEM: Integer;
  function _GetFIRST: Integer;
  function _GetFLAG_ALWAYS_PERFORM_CLOSE: Integer;
  function _GetFLAG_APPEND_TO_GROUP: Integer;
  function _GetFLAG_PERFORM_NO_CLOSE: Integer;
  function _GetNONE: Integer;
  {Properties}
  property CATEGORY_ALTERNATIVE: Integer read _GetCATEGORY_ALTERNATIVE;
  property CATEGORY_CONTAINER: Integer read _GetCATEGORY_CONTAINER;
  property CATEGORY_SECONDARY: Integer read _GetCATEGORY_SECONDARY;
  property CATEGORY_SYSTEM: Integer read _GetCATEGORY_SYSTEM;
  property FIRST: Integer read _GetFIRST;
  property FLAG_ALWAYS_PERFORM_CLOSE: Integer read _GetFLAG_ALWAYS_PERFORM_CLOSE;
  property FLAG_APPEND_TO_GROUP: Integer read _GetFLAG_APPEND_TO_GROUP;
  property FLAG_PERFORM_NO_CLOSE: Integer read _GetFLAG_PERFORM_NO_CLOSE;
  property NONE: Integer read _GetNONE;
end;

[JavaSignature('android/view/Menu')]
JMenu = interface(IJavaInstance)
['{DECE608E-FA87-43AF-A7CE-2223FCD5BD3E}']
  {Methods}
  function add(title: JCharSequence): JMenuItem; cdecl; overload;
  function add(titleRes: Integer): JMenuItem; cdecl; overload;
  function add(groupId: Integer; itemId: Integer; order: Integer; title: JCharSequence): JMenuItem; cdecl; overload;
  function add(groupId: Integer; itemId: Integer; order: Integer; titleRes: Integer): JMenuItem; cdecl; overload;
  function addIntentOptions(groupId: Integer; itemId: Integer; order: Integer; caller: JComponentName; specifics: TJavaObjectArray<JIntent>; intent: JIntent; flags: Integer; outSpecificItems: TJavaObjectArray<JMenuItem>): Integer; cdecl;
  function addSubMenu(title: JCharSequence): JSubMenu; cdecl; overload;
  function addSubMenu(titleRes: Integer): JSubMenu; cdecl; overload;
  function addSubMenu(groupId: Integer; itemId: Integer; order: Integer; title: JCharSequence): JSubMenu; cdecl; overload;
  function addSubMenu(groupId: Integer; itemId: Integer; order: Integer; titleRes: Integer): JSubMenu; cdecl; overload;
  procedure clear; cdecl;
  procedure close; cdecl;
  function findItem(id: Integer): JMenuItem; cdecl;
  function getItem(index: Integer): JMenuItem; cdecl;
  function hasVisibleItems: Boolean; cdecl;
  function isShortcutKey(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  function performIdentifierAction(id: Integer; flags: Integer): Boolean; cdecl;
  function performShortcut(keyCode: Integer; event: JKeyEvent; flags: Integer): Boolean; cdecl;
  procedure removeGroup(groupId: Integer); cdecl;
  procedure removeItem(id: Integer); cdecl;
  procedure setGroupCheckable(group: Integer; checkable: Boolean; exclusive: Boolean); cdecl;
  procedure setGroupEnabled(group: Integer; enabled: Boolean); cdecl;
  procedure setGroupVisible(group: Integer; visible: Boolean); cdecl;
  procedure setQwertyMode(isQwerty: Boolean); cdecl;
  function size: Integer; cdecl;
end;
TJMenu = class(TJavaGenericImport<JMenuClass, JMenu>) end;

JContextMenuClass = interface(JMenuClass)
['{620B70D9-BD29-4FFF-B9D6-22DEF53FF3CB}']
end;

[JavaSignature('android/view/ContextMenu')]
JContextMenu = interface(JMenu)
['{31A13877-FC0E-46C5-BF67-FC2A7624CC60}']
  {Methods}
  procedure clearHeader; cdecl;
  function setHeaderIcon(iconRes: Integer): JContextMenu; cdecl; overload;
  function setHeaderIcon(icon: JDrawable): JContextMenu; cdecl; overload;
  function setHeaderTitle(titleRes: Integer): JContextMenu; cdecl; overload;
  function setHeaderTitle(title: JCharSequence): JContextMenu; cdecl; overload;
  function setHeaderView(view: JView): JContextMenu; cdecl;
end;
TJContextMenu = class(TJavaGenericImport<JContextMenuClass, JContextMenu>) end;

JPathClass = interface(JObjectClass)
['{15AA69AD-517A-4AD4-BB4E-129E32102F0C}']
  {Methods}
  function init: JPath; cdecl; overload;
  function init(src: JPath): JPath; cdecl; overload;
end;

[JavaSignature('android/graphics/Path')]
JPath = interface(JObject)
['{0E150B56-1B83-464A-A292-8B9C24E1E14F}']
  {Methods}
  procedure addArc(oval: JRectF; startAngle: Single; sweepAngle: Single); cdecl;
  procedure addCircle(x: Single; y: Single; radius: Single; dir: JPath_Direction); cdecl;
  procedure addOval(oval: JRectF; dir: JPath_Direction); cdecl;
  procedure addPath(src: JPath; dx: Single; dy: Single); cdecl; overload;
  procedure addPath(src: JPath); cdecl; overload;
  procedure addPath(src: JPath; matrix: JMatrix); cdecl; overload;
  procedure addRect(rect: JRectF; dir: JPath_Direction); cdecl; overload;
  procedure addRect(left: Single; top: Single; right: Single; bottom: Single; dir: JPath_Direction); cdecl; overload;
  procedure addRoundRect(rect: JRectF; rx: Single; ry: Single; dir: JPath_Direction); cdecl; overload;
  procedure addRoundRect(rect: JRectF; radii: TJavaArray<Single>; dir: JPath_Direction); cdecl; overload;
  procedure arcTo(oval: JRectF; startAngle: Single; sweepAngle: Single; forceMoveTo: Boolean); cdecl; overload;
  procedure arcTo(oval: JRectF; startAngle: Single; sweepAngle: Single); cdecl; overload;
  procedure close; cdecl;
  procedure computeBounds(bounds: JRectF; exact: Boolean); cdecl;
  procedure cubicTo(x1: Single; y1: Single; x2: Single; y2: Single; x3: Single; y3: Single); cdecl;
  function getFillType: JPath_FillType; cdecl;
  procedure incReserve(extraPtCount: Integer); cdecl;
  function isEmpty: Boolean; cdecl;
  function isInverseFillType: Boolean; cdecl;
  function isRect(rect: JRectF): Boolean; cdecl;
  procedure lineTo(x: Single; y: Single); cdecl;
  procedure moveTo(x: Single; y: Single); cdecl;
  procedure offset(dx: Single; dy: Single; dst: JPath); cdecl; overload;
  procedure offset(dx: Single; dy: Single); cdecl; overload;
  procedure quadTo(x1: Single; y1: Single; x2: Single; y2: Single); cdecl;
  procedure rCubicTo(x1: Single; y1: Single; x2: Single; y2: Single; x3: Single; y3: Single); cdecl;
  procedure rLineTo(dx: Single; dy: Single); cdecl;
  procedure rMoveTo(dx: Single; dy: Single); cdecl;
  procedure rQuadTo(dx1: Single; dy1: Single; dx2: Single; dy2: Single); cdecl;
  procedure reset; cdecl;
  procedure rewind; cdecl;
  procedure &set(src: JPath); cdecl;
  procedure setFillType(ft: JPath_FillType); cdecl;
  procedure setLastPoint(dx: Single; dy: Single); cdecl;
  procedure toggleInverseFillType; cdecl;
  procedure transform(matrix: JMatrix; dst: JPath); cdecl; overload;
  procedure transform(matrix: JMatrix); cdecl; overload;
end;
TJPath = class(TJavaGenericImport<JPathClass, JPath>) end;

JInputQueue_CallbackClass = interface(IJavaClass)
['{3C12E67D-2387-44D7-B467-359188FB2BC4}']
end;

[JavaSignature('android/view/InputQueue$Callback')]
JInputQueue_Callback = interface(IJavaInstance)
['{EE832599-49D1-406A-9A6C-7293ED164F84}']
  {Methods}
  procedure onInputQueueCreated(queue: JInputQueue); cdecl;
  procedure onInputQueueDestroyed(queue: JInputQueue); cdecl;
end;
TJInputQueue_Callback = class(TJavaGenericImport<JInputQueue_CallbackClass, JInputQueue_Callback>) end;

Jgraphics_InterpolatorClass = interface(JObjectClass)
['{4305ED09-E153-4638-BA91-7EC654E86893}']
  {Methods}
  function init(valueCount: Integer): Jgraphics_Interpolator; cdecl; overload;
  function init(valueCount: Integer; frameCount: Integer): Jgraphics_Interpolator; cdecl; overload;
end;

[JavaSignature('android/graphics/Interpolator')]
Jgraphics_Interpolator = interface(JObject)
['{25B2099C-5F7F-4F93-98A8-D7563EF5DB2B}']
  {Methods}
  function getKeyFrameCount: Integer; cdecl;
  function getValueCount: Integer; cdecl;
  procedure reset(valueCount: Integer); cdecl; overload;
  procedure reset(valueCount: Integer; frameCount: Integer); cdecl; overload;
  procedure setKeyFrame(index: Integer; msec: Integer; values: TJavaArray<Single>); cdecl; overload;
  procedure setKeyFrame(index: Integer; msec: Integer; values: TJavaArray<Single>; blend: TJavaArray<Single>); cdecl; overload;
  procedure setRepeatMirror(repeatCount: Single; mirror: Boolean); cdecl;
  function timeToValues(values: TJavaArray<Single>): JInterpolator_Result; cdecl; overload;
  function timeToValues(msec: Integer; values: TJavaArray<Single>): JInterpolator_Result; cdecl; overload;
end;
TJgraphics_Interpolator = class(TJavaGenericImport<Jgraphics_InterpolatorClass, Jgraphics_Interpolator>) end;

JLayoutInflaterClass = interface(JObjectClass)
['{FD7AD6DE-7D47-48E0-9E0E-EF34B5CC6720}']
  {Methods}
  function from(context: JContext): JLayoutInflater; cdecl;
end;

[JavaSignature('android/view/LayoutInflater')]
JLayoutInflater = interface(JObject)
['{F65EBB1F-25CF-4A00-92E4-B16D40BF4299}']
  {Methods}
  function cloneInContext(newContext: JContext): JLayoutInflater; cdecl;
  function createView(name: JString; prefix: JString; attrs: JAttributeSet): JView; cdecl;
  function getContext: JContext; cdecl;
  function getFactory: JLayoutInflater_Factory; cdecl;
  function getFactory2: JLayoutInflater_Factory2; cdecl;
  function getFilter: JLayoutInflater_Filter; cdecl;
  function inflate(resource: Integer; root: JViewGroup): JView; cdecl; overload;
  function inflate(resource: Integer; root: JViewGroup; attachToRoot: Boolean): JView; cdecl; overload;
  procedure setFactory(factory: JLayoutInflater_Factory); cdecl;
  procedure setFactory2(factory: JLayoutInflater_Factory2); cdecl;
  procedure setFilter(filter: JLayoutInflater_Filter); cdecl;
end;
TJLayoutInflater = class(TJavaGenericImport<JLayoutInflaterClass, JLayoutInflater>) end;

JImageFormatClass = interface(JObjectClass)
['{FA5ABF62-CD4C-4308-9E12-4DC09865BA95}']
  {Property Methods}
  function _GetJPEG: Integer;
  function _GetNV16: Integer;
  function _GetNV21: Integer;
  function _GetRGB_565: Integer;
  function _GetUNKNOWN: Integer;
  function _GetYUY2: Integer;
  function _GetYV12: Integer;
  {Methods}
  function init: JImageFormat; cdecl;
  function getBitsPerPixel(format: Integer): Integer; cdecl;
  {Properties}
  property JPEG: Integer read _GetJPEG;
  property NV16: Integer read _GetNV16;
  property NV21: Integer read _GetNV21;
  property RGB_565: Integer read _GetRGB_565;
  property UNKNOWN: Integer read _GetUNKNOWN;
  property YUY2: Integer read _GetYUY2;
  property YV12: Integer read _GetYV12;
end;

[JavaSignature('android/graphics/ImageFormat')]
JImageFormat = interface(JObject)
['{0AE078B6-B798-468F-8936-64218ACA689C}']
end;
TJImageFormat = class(TJavaGenericImport<JImageFormatClass, JImageFormat>) end;

JInputFilterClass = interface(IJavaClass)
['{A71F6CCA-A2D4-4557-A1AB-8DBC3A519936}']
end;

[JavaSignature('android/text/InputFilter')]
JInputFilter = interface(IJavaInstance)
['{B6E265A5-32DE-4AA1-BD61-93217E5B53DB}']
  {Methods}
  function filter(source: JCharSequence; start: Integer; end_: Integer; dest: JSpanned; dstart: Integer; dend: Integer): JCharSequence; cdecl;
end;
TJInputFilter = class(TJavaGenericImport<JInputFilterClass, JInputFilter>) end;

JPixelFormatClass = interface(JObjectClass)
['{3A069AE1-5A8C-4294-A6A9-AAB87656677C}']
  {Property Methods}
  function _GetA_8: Integer;
  function _GetJPEG: Integer;
  function _GetLA_88: Integer;
  function _GetL_8: Integer;
  function _GetOPAQUE: Integer;
  function _GetRGBA_4444: Integer;
  function _GetRGBA_5551: Integer;
  function _GetRGBA_8888: Integer;
  function _GetRGBX_8888: Integer;
  function _GetRGB_332: Integer;
  function _GetRGB_565: Integer;
  function _GetRGB_888: Integer;
  function _GetTRANSLUCENT: Integer;
  function _GetTRANSPARENT: Integer;
  function _GetUNKNOWN: Integer;
  function _GetYCbCr_420_SP: Integer;
  function _GetYCbCr_422_I: Integer;
  function _GetYCbCr_422_SP: Integer;
  {Methods}
  function init: JPixelFormat; cdecl;
  function formatHasAlpha(format: Integer): Boolean; cdecl;
  procedure getPixelFormatInfo(format: Integer; info: JPixelFormat); cdecl;
  {Properties}
  property A_8: Integer read _GetA_8;
  property JPEG: Integer read _GetJPEG;
  property LA_88: Integer read _GetLA_88;
  property L_8: Integer read _GetL_8;
  property OPAQUE: Integer read _GetOPAQUE;
  property RGBA_4444: Integer read _GetRGBA_4444;
  property RGBA_5551: Integer read _GetRGBA_5551;
  property RGBA_8888: Integer read _GetRGBA_8888;
  property RGBX_8888: Integer read _GetRGBX_8888;
  property RGB_332: Integer read _GetRGB_332;
  property RGB_565: Integer read _GetRGB_565;
  property RGB_888: Integer read _GetRGB_888;
  property TRANSLUCENT: Integer read _GetTRANSLUCENT;
  property TRANSPARENT: Integer read _GetTRANSPARENT;
  property UNKNOWN: Integer read _GetUNKNOWN;
  property YCbCr_420_SP: Integer read _GetYCbCr_420_SP;
  property YCbCr_422_I: Integer read _GetYCbCr_422_I;
  property YCbCr_422_SP: Integer read _GetYCbCr_422_SP;
end;

[JavaSignature('android/graphics/PixelFormat')]
JPixelFormat = interface(JObject)
['{09E8682B-A994-438B-BB64-F3C0F54C84AC}']
  {Property Methods}
  function _GetbitsPerPixel: Integer;
  procedure _SetbitsPerPixel(Value: Integer);
  function _GetbytesPerPixel: Integer;
  procedure _SetbytesPerPixel(Value: Integer);
  {Properties}
  property bitsPerPixel: Integer read _GetbitsPerPixel write _SetbitsPerPixel;
  property bytesPerPixel: Integer read _GetbytesPerPixel write _SetbytesPerPixel;
end;
TJPixelFormat = class(TJavaGenericImport<JPixelFormatClass, JPixelFormat>) end;

JLoader_OnLoadCanceledListenerClass = interface(IJavaClass)
['{769CEFA7-F086-4CDE-8348-66CB478E6CBE}']
end;

[JavaSignature('android/content/Loader$OnLoadCanceledListener')]
JLoader_OnLoadCanceledListener = interface(IJavaInstance)
['{4C6C08EE-6DB0-4612-9F99-F36104D7BFCF}']
  {Methods}
  procedure onLoadCanceled(loader: JLoader); cdecl;
end;
TJLoader_OnLoadCanceledListener = class(TJavaGenericImport<JLoader_OnLoadCanceledListenerClass, JLoader_OnLoadCanceledListener>) end;

JNinePatchClass = interface(JObjectClass)
['{5F4F53E2-09A1-4B53-9C0B-54FA541CF30E}']
  {Methods}
  function init(bitmap: JBitmap; chunk: TJavaArray<Byte>; srcName: JString): JNinePatch; cdecl;
  function isNinePatchChunk(chunk: TJavaArray<Byte>): Boolean; cdecl;
end;

[JavaSignature('android/graphics/NinePatch')]
JNinePatch = interface(JObject)
['{02C8AAEB-2EEC-4ED0-B53D-11E3BF121264}']
  {Methods}
  procedure draw(canvas: JCanvas; location: JRectF); cdecl; overload;
  procedure draw(canvas: JCanvas; location: JRect); cdecl; overload;
  procedure draw(canvas: JCanvas; location: JRect; paint: JPaint); cdecl; overload;
  function getDensity: Integer; cdecl;
  function getHeight: Integer; cdecl;
  function getTransparentRegion(location: JRect): JRegion; cdecl;
  function getWidth: Integer; cdecl;
  function hasAlpha: Boolean; cdecl;
  procedure setPaint(p: JPaint); cdecl;
end;
TJNinePatch = class(TJavaGenericImport<JNinePatchClass, JNinePatch>) end;

JSyncAdapterTypeClass = interface(JObjectClass)
['{701008C3-9875-4994-908E-0CAD192C047E}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Methods}
  function init(authority: JString; accountType: JString; userVisible: Boolean; supportsUploading: Boolean): JSyncAdapterType; cdecl; overload;
  function init(source: JParcel): JSyncAdapterType; cdecl; overload;
  function newKey(authority: JString; accountType: JString): JSyncAdapterType; cdecl;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/content/SyncAdapterType')]
JSyncAdapterType = interface(JObject)
['{42EF06A0-D1AE-412C-B244-BAE8DB567003}']
  {Property Methods}
  function _GetaccountType: JString;
  function _Getauthority: JString;
  function _GetisKey: Boolean;
  {Methods}
  function allowParallelSyncs: Boolean; cdecl;
  function describeContents: Integer; cdecl;
  function equals(o: JObject): Boolean; cdecl;
  function getSettingsActivity: JString; cdecl;
  function hashCode: Integer; cdecl;
  function isAlwaysSyncable: Boolean; cdecl;
  function isUserVisible: Boolean; cdecl;
  function supportsUploading: Boolean; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  {Properties}
  property accountType: JString read _GetaccountType;
  property authority: JString read _Getauthority;
  property isKey: Boolean read _GetisKey;
end;
TJSyncAdapterType = class(TJavaGenericImport<JSyncAdapterTypeClass, JSyncAdapterType>) end;

JContentProvider_PipeDataWriterClass = interface(IJavaClass)
['{885A1239-1EE5-40A1-A053-EA283661D1BC}']
end;

[JavaSignature('android/content/ContentProvider$PipeDataWriter')]
JContentProvider_PipeDataWriter = interface(IJavaInstance)
['{ABFB532E-1496-4099-85DB-E9099B6BAAEA}']
  {Methods}
  procedure writeDataToPipe(output: JParcelFileDescriptor; uri: Jnet_Uri; mimeType: JString; opts: JBundle; args: JObject); cdecl;
end;
TJContentProvider_PipeDataWriter = class(TJavaGenericImport<JContentProvider_PipeDataWriterClass, JContentProvider_PipeDataWriter>) end;

JLoader_OnLoadCompleteListenerClass = interface(IJavaClass)
['{B637B683-4104-4A5C-8BB2-961070E40E05}']
end;

[JavaSignature('android/content/Loader$OnLoadCompleteListener')]
JLoader_OnLoadCompleteListener = interface(IJavaInstance)
['{16E33371-5A38-4FD1-8B83-58FFE169A785}']
  {Methods}
  procedure onLoadComplete(loader: JLoader; data: JObject); cdecl;
end;
TJLoader_OnLoadCompleteListener = class(TJavaGenericImport<JLoader_OnLoadCompleteListenerClass, JLoader_OnLoadCompleteListener>) end;

JInputConnectionClass = interface(IJavaClass)
['{0A80826B-3A89-47EE-9F02-2F2D8793B30E}']
  {Property Methods}
  function _GetGET_EXTRACTED_TEXT_MONITOR: Integer;
  function _GetGET_TEXT_WITH_STYLES: Integer;
  {Properties}
  property GET_EXTRACTED_TEXT_MONITOR: Integer read _GetGET_EXTRACTED_TEXT_MONITOR;
  property GET_TEXT_WITH_STYLES: Integer read _GetGET_TEXT_WITH_STYLES;
end;

[JavaSignature('android/view/inputmethod/InputConnection')]
JInputConnection = interface(IJavaInstance)
['{4C729CD3-F58A-4053-94E1-7F280F3B42EF}']
  {Methods}
  function beginBatchEdit: Boolean; cdecl;
  function clearMetaKeyStates(states: Integer): Boolean; cdecl;
  function commitCompletion(text: JCompletionInfo): Boolean; cdecl;
  function commitCorrection(correctionInfo: JCorrectionInfo): Boolean; cdecl;
  function commitText(text: JCharSequence; newCursorPosition: Integer): Boolean; cdecl;
  function deleteSurroundingText(beforeLength: Integer; afterLength: Integer): Boolean; cdecl;
  function endBatchEdit: Boolean; cdecl;
  function finishComposingText: Boolean; cdecl;
  function getCursorCapsMode(reqModes: Integer): Integer; cdecl;
  function getExtractedText(request: JExtractedTextRequest; flags: Integer): JExtractedText; cdecl;
  function getSelectedText(flags: Integer): JCharSequence; cdecl;
  function getTextAfterCursor(n: Integer; flags: Integer): JCharSequence; cdecl;
  function getTextBeforeCursor(n: Integer; flags: Integer): JCharSequence; cdecl;
  function performContextMenuAction(id: Integer): Boolean; cdecl;
  function performEditorAction(editorAction: Integer): Boolean; cdecl;
  function performPrivateCommand(action: JString; data: JBundle): Boolean; cdecl;
  function reportFullscreenMode(enabled: Boolean): Boolean; cdecl;
  function sendKeyEvent(event: JKeyEvent): Boolean; cdecl;
  function setComposingRegion(start: Integer; end_: Integer): Boolean; cdecl;
  function setComposingText(text: JCharSequence; newCursorPosition: Integer): Boolean; cdecl;
  function setSelection(start: Integer; end_: Integer): Boolean; cdecl;
end;
TJInputConnection = class(TJavaGenericImport<JInputConnectionClass, JInputConnection>) end;

JNoCopySpanClass = interface(IJavaClass)
['{2C00BEC5-5ECB-44BC-ABC4-1C5EB4986AA8}']
end;

[JavaSignature('android/text/NoCopySpan')]
JNoCopySpan = interface(IJavaInstance)
['{E9CC4E75-A421-457C-89E1-7B76557934D7}']
end;
TJNoCopySpan = class(TJavaGenericImport<JNoCopySpanClass, JNoCopySpan>) end;

JTextWatcherClass = interface(JNoCopySpanClass)
['{68D3C86E-F1BC-44A3-BBD3-7AB0FD9439D5}']
end;

[JavaSignature('android/text/TextWatcher')]
JTextWatcher = interface(JNoCopySpan)
['{63D741B9-2232-4298-97E0-098F283B7BB6}']
  {Methods}
  procedure afterTextChanged(s: JEditable); cdecl;
  procedure beforeTextChanged(s: JCharSequence; start: Integer; count: Integer; after: Integer); cdecl;
  procedure onTextChanged(s: JCharSequence; start: Integer; before: Integer; count: Integer); cdecl;
end;
TJTextWatcher = class(TJavaGenericImport<JTextWatcherClass, JTextWatcher>) end;

JViewTreeObserverClass = interface(JObjectClass)
['{8221B758-DDD2-423B-8A5F-4E4C641AA1C2}']
end;

[JavaSignature('android/view/ViewTreeObserver')]
JViewTreeObserver = interface(JObject)
['{970260A4-CEA0-498B-93FD-426F280764AA}']
  {Methods}
  procedure addOnDrawListener(listener: JViewTreeObserver_OnDrawListener); cdecl;
  procedure addOnGlobalFocusChangeListener(listener: JViewTreeObserver_OnGlobalFocusChangeListener); cdecl;
  procedure addOnGlobalLayoutListener(listener: JViewTreeObserver_OnGlobalLayoutListener); cdecl;
  procedure addOnPreDrawListener(listener: JViewTreeObserver_OnPreDrawListener); cdecl;
  procedure addOnScrollChangedListener(listener: JViewTreeObserver_OnScrollChangedListener); cdecl;
  procedure addOnTouchModeChangeListener(listener: JViewTreeObserver_OnTouchModeChangeListener); cdecl;
  procedure dispatchOnDraw; cdecl;
  procedure dispatchOnGlobalLayout; cdecl;
  function dispatchOnPreDraw: Boolean; cdecl;
  function isAlive: Boolean; cdecl;
  procedure removeGlobalOnLayoutListener(victim: JViewTreeObserver_OnGlobalLayoutListener); cdecl;//Deprecated
  procedure removeOnDrawListener(victim: JViewTreeObserver_OnDrawListener); cdecl;
  procedure removeOnGlobalFocusChangeListener(victim: JViewTreeObserver_OnGlobalFocusChangeListener); cdecl;
  procedure removeOnGlobalLayoutListener(victim: JViewTreeObserver_OnGlobalLayoutListener); cdecl;
  procedure removeOnPreDrawListener(victim: JViewTreeObserver_OnPreDrawListener); cdecl;
  procedure removeOnScrollChangedListener(victim: JViewTreeObserver_OnScrollChangedListener); cdecl;
  procedure removeOnTouchModeChangeListener(victim: JViewTreeObserver_OnTouchModeChangeListener); cdecl;
end;
TJViewTreeObserver = class(TJavaGenericImport<JViewTreeObserverClass, JViewTreeObserver>) end;

JShaderClass = interface(JObjectClass)
['{E09DAEDF-1831-4180-AD13-6B01F559396E}']
  {Methods}
  function init: JShader; cdecl;
end;

[JavaSignature('android/graphics/Shader')]
JShader = interface(JObject)
['{300E27ED-093C-4CB5-B18D-C0CEFB899BA5}']
  {Methods}
  function getLocalMatrix(localM: JMatrix): Boolean; cdecl;
  procedure setLocalMatrix(localM: JMatrix); cdecl;
end;
TJShader = class(TJavaGenericImport<JShaderClass, JShader>) end;

JComposeShaderClass = interface(JShaderClass)
['{7A61B787-FD33-45CE-87B4-91A1334FCA7B}']
  {Methods}
  function init(shaderA: JShader; shaderB: JShader; mode: JXfermode): JComposeShader; cdecl; overload;
  function init(shaderA: JShader; shaderB: JShader; mode: JPorterDuff_Mode): JComposeShader; cdecl; overload;
end;

[JavaSignature('android/graphics/ComposeShader')]
JComposeShader = interface(JShader)
['{A42E0489-62B7-4864-BABA-80E4443EC46E}']
end;
TJComposeShader = class(TJavaGenericImport<JComposeShaderClass, JComposeShader>) end;

JPackageInfoClass = interface(JObjectClass)
['{113B61F1-7B39-4C59-A2CC-87E301EE94B8}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  function _GetREQUESTED_PERMISSION_GRANTED: Integer;
  function _GetREQUESTED_PERMISSION_REQUIRED: Integer;
  {Methods}
  function init: JPackageInfo; cdecl;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
  property REQUESTED_PERMISSION_GRANTED: Integer read _GetREQUESTED_PERMISSION_GRANTED;
  property REQUESTED_PERMISSION_REQUIRED: Integer read _GetREQUESTED_PERMISSION_REQUIRED;
end;

[JavaSignature('android/content/pm/PackageInfo')]
JPackageInfo = interface(JObject)
['{430E0F0C-5A02-46D7-A50F-2B192576D25E}']
  {Property Methods}
  function _Getactivities: TJavaObjectArray<JActivityInfo>;
  procedure _Setactivities(Value: TJavaObjectArray<JActivityInfo>);
  function _GetapplicationInfo: JApplicationInfo;
  procedure _SetapplicationInfo(Value: JApplicationInfo);
  function _GetconfigPreferences: TJavaObjectArray<JConfigurationInfo>;
  procedure _SetconfigPreferences(Value: TJavaObjectArray<JConfigurationInfo>);
  function _GetfirstInstallTime: Int64;
  procedure _SetfirstInstallTime(Value: Int64);
  function _Getgids: TJavaArray<Integer>;
  procedure _Setgids(Value: TJavaArray<Integer>);
  function _Getinstrumentation: TJavaObjectArray<JInstrumentationInfo>;
  procedure _Setinstrumentation(Value: TJavaObjectArray<JInstrumentationInfo>);
  function _GetlastUpdateTime: Int64;
  procedure _SetlastUpdateTime(Value: Int64);
  function _GetpackageName: JString;
  procedure _SetpackageName(Value: JString);
  function _Getpermissions: TJavaObjectArray<JPermissionInfo>;
  procedure _Setpermissions(Value: TJavaObjectArray<JPermissionInfo>);
  function _Getproviders: TJavaObjectArray<JProviderInfo>;
  procedure _Setproviders(Value: TJavaObjectArray<JProviderInfo>);
  function _Getreceivers: TJavaObjectArray<JActivityInfo>;
  procedure _Setreceivers(Value: TJavaObjectArray<JActivityInfo>);
  function _GetreqFeatures: TJavaObjectArray<JFeatureInfo>;
  procedure _SetreqFeatures(Value: TJavaObjectArray<JFeatureInfo>);
  function _GetrequestedPermissions: TJavaObjectArray<JString>;
  procedure _SetrequestedPermissions(Value: TJavaObjectArray<JString>);
  function _GetrequestedPermissionsFlags: TJavaArray<Integer>;
  procedure _SetrequestedPermissionsFlags(Value: TJavaArray<Integer>);
  function _Getservices: TJavaObjectArray<JServiceInfo>;
  procedure _Setservices(Value: TJavaObjectArray<JServiceInfo>);
  function _GetsharedUserId: JString;
  procedure _SetsharedUserId(Value: JString);
  function _GetsharedUserLabel: Integer;
  procedure _SetsharedUserLabel(Value: Integer);
  function _Getsignatures: TJavaObjectArray<JSignature>;
  procedure _Setsignatures(Value: TJavaObjectArray<JSignature>);
  function _GetversionCode: Integer;
  procedure _SetversionCode(Value: Integer);
  function _GetversionName: JString;
  procedure _SetversionName(Value: JString);
  {Methods}
  function describeContents: Integer; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; parcelableFlags: Integer); cdecl;
  {Properties}
  property activities: TJavaObjectArray<JActivityInfo> read _Getactivities write _Setactivities;
  property applicationInfo: JApplicationInfo read _GetapplicationInfo write _SetapplicationInfo;
  property configPreferences: TJavaObjectArray<JConfigurationInfo> read _GetconfigPreferences write _SetconfigPreferences;
  property firstInstallTime: Int64 read _GetfirstInstallTime write _SetfirstInstallTime;
  property gids: TJavaArray<Integer> read _Getgids write _Setgids;
  property instrumentation: TJavaObjectArray<JInstrumentationInfo> read _Getinstrumentation write _Setinstrumentation;
  property lastUpdateTime: Int64 read _GetlastUpdateTime write _SetlastUpdateTime;
  property packageName: JString read _GetpackageName write _SetpackageName;
  property permissions: TJavaObjectArray<JPermissionInfo> read _Getpermissions write _Setpermissions;
  property providers: TJavaObjectArray<JProviderInfo> read _Getproviders write _Setproviders;
  property receivers: TJavaObjectArray<JActivityInfo> read _Getreceivers write _Setreceivers;
  property reqFeatures: TJavaObjectArray<JFeatureInfo> read _GetreqFeatures write _SetreqFeatures;
  property requestedPermissions: TJavaObjectArray<JString> read _GetrequestedPermissions write _SetrequestedPermissions;
  property requestedPermissionsFlags: TJavaArray<Integer> read _GetrequestedPermissionsFlags write _SetrequestedPermissionsFlags;
  property services: TJavaObjectArray<JServiceInfo> read _Getservices write _Setservices;
  property sharedUserId: JString read _GetsharedUserId write _SetsharedUserId;
  property sharedUserLabel: Integer read _GetsharedUserLabel write _SetsharedUserLabel;
  property signatures: TJavaObjectArray<JSignature> read _Getsignatures write _Setsignatures;
  property versionCode: Integer read _GetversionCode write _SetversionCode;
  property versionName: JString read _GetversionName write _SetversionName;
end;
TJPackageInfo = class(TJavaGenericImport<JPackageInfoClass, JPackageInfo>) end;

JConfigurationInfoClass = interface(JObjectClass)
['{45B946FD-0196-413D-A521-2DCEE09FBDF3}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  function _GetGL_ES_VERSION_UNDEFINED: Integer;
  function _GetINPUT_FEATURE_FIVE_WAY_NAV: Integer;
  function _GetINPUT_FEATURE_HARD_KEYBOARD: Integer;
  {Methods}
  function init: JConfigurationInfo; cdecl; overload;
  function init(orig: JConfigurationInfo): JConfigurationInfo; cdecl; overload;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
  property GL_ES_VERSION_UNDEFINED: Integer read _GetGL_ES_VERSION_UNDEFINED;
  property INPUT_FEATURE_FIVE_WAY_NAV: Integer read _GetINPUT_FEATURE_FIVE_WAY_NAV;
  property INPUT_FEATURE_HARD_KEYBOARD: Integer read _GetINPUT_FEATURE_HARD_KEYBOARD;
end;

[JavaSignature('android/content/pm/ConfigurationInfo')]
JConfigurationInfo = interface(JObject)
['{7C7C1C86-3575-4631-A168-544AC96685AA}']
  {Property Methods}
  function _GetreqGlEsVersion: Integer;
  procedure _SetreqGlEsVersion(Value: Integer);
  function _GetreqInputFeatures: Integer;
  procedure _SetreqInputFeatures(Value: Integer);
  function _GetreqKeyboardType: Integer;
  procedure _SetreqKeyboardType(Value: Integer);
  function _GetreqNavigation: Integer;
  procedure _SetreqNavigation(Value: Integer);
  function _GetreqTouchScreen: Integer;
  procedure _SetreqTouchScreen(Value: Integer);
  {Methods}
  function describeContents: Integer; cdecl;
  function getGlEsVersion: JString; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; parcelableFlags: Integer); cdecl;
  {Properties}
  property reqGlEsVersion: Integer read _GetreqGlEsVersion write _SetreqGlEsVersion;
  property reqInputFeatures: Integer read _GetreqInputFeatures write _SetreqInputFeatures;
  property reqKeyboardType: Integer read _GetreqKeyboardType write _SetreqKeyboardType;
  property reqNavigation: Integer read _GetreqNavigation write _SetreqNavigation;
  property reqTouchScreen: Integer read _GetreqTouchScreen write _SetreqTouchScreen;
end;
TJConfigurationInfo = class(TJavaGenericImport<JConfigurationInfoClass, JConfigurationInfo>) end;

JIntentClass = interface(JObjectClass)
['{3FCF8E6F-7E34-46C1-9007-50B8C6479CE9}']
  {Property Methods}
  function _GetACTION_AIRPLANE_MODE_CHANGED: JString;
  function _GetACTION_ALL_APPS: JString;
  function _GetACTION_ANSWER: JString;
  function _GetACTION_APP_ERROR: JString;
  function _GetACTION_ASSIST: JString;
  function _GetACTION_ATTACH_DATA: JString;
  function _GetACTION_BATTERY_CHANGED: JString;
  function _GetACTION_BATTERY_LOW: JString;
  function _GetACTION_BATTERY_OKAY: JString;
  function _GetACTION_BOOT_COMPLETED: JString;
  function _GetACTION_BUG_REPORT: JString;
  function _GetACTION_CALL: JString;
  function _GetACTION_CALL_BUTTON: JString;
  function _GetACTION_CAMERA_BUTTON: JString;
  function _GetACTION_CHOOSER: JString;
  function _GetACTION_CLOSE_SYSTEM_DIALOGS: JString;
  function _GetACTION_CONFIGURATION_CHANGED: JString;
  function _GetACTION_CREATE_SHORTCUT: JString;
  function _GetACTION_DATE_CHANGED: JString;
  function _GetACTION_DEFAULT: JString;
  function _GetACTION_DELETE: JString;
  function _GetACTION_DEVICE_STORAGE_LOW: JString;
  function _GetACTION_DEVICE_STORAGE_OK: JString;
  function _GetACTION_DIAL: JString;
  function _GetACTION_DOCK_EVENT: JString;
  function _GetACTION_DREAMING_STARTED: JString;
  function _GetACTION_DREAMING_STOPPED: JString;
  function _GetACTION_EDIT: JString;
  function _GetACTION_EXTERNAL_APPLICATIONS_AVAILABLE: JString;
  function _GetACTION_EXTERNAL_APPLICATIONS_UNAVAILABLE: JString;
  function _GetACTION_FACTORY_TEST: JString;
  function _GetACTION_GET_CONTENT: JString;
  function _GetACTION_GTALK_SERVICE_CONNECTED: JString;
  function _GetACTION_GTALK_SERVICE_DISCONNECTED: JString;
  function _GetACTION_HEADSET_PLUG: JString;
  function _GetACTION_INPUT_METHOD_CHANGED: JString;
  function _GetACTION_INSERT: JString;
  function _GetACTION_INSERT_OR_EDIT: JString;
  function _GetACTION_INSTALL_PACKAGE: JString;
  function _GetACTION_LOCALE_CHANGED: JString;
  function _GetACTION_MAIN: JString;
  function _GetACTION_MANAGE_NETWORK_USAGE: JString;
  function _GetACTION_MANAGE_PACKAGE_STORAGE: JString;
  function _GetACTION_MEDIA_BAD_REMOVAL: JString;
  function _GetACTION_MEDIA_BUTTON: JString;
  function _GetACTION_MEDIA_CHECKING: JString;
  function _GetACTION_MEDIA_EJECT: JString;
  function _GetACTION_MEDIA_MOUNTED: JString;
  function _GetACTION_MEDIA_NOFS: JString;
  function _GetACTION_MEDIA_REMOVED: JString;
  function _GetACTION_MEDIA_SCANNER_FINISHED: JString;
  function _GetACTION_MEDIA_SCANNER_SCAN_FILE: JString;
  function _GetACTION_MEDIA_SCANNER_STARTED: JString;
  function _GetACTION_MEDIA_SHARED: JString;
  function _GetACTION_MEDIA_UNMOUNTABLE: JString;
  function _GetACTION_MEDIA_UNMOUNTED: JString;
  function _GetACTION_MY_PACKAGE_REPLACED: JString;
  function _GetACTION_NEW_OUTGOING_CALL: JString;
  function _GetACTION_PACKAGE_ADDED: JString;
  function _GetACTION_PACKAGE_CHANGED: JString;
  function _GetACTION_PACKAGE_DATA_CLEARED: JString;
  function _GetACTION_PACKAGE_FIRST_LAUNCH: JString;
  function _GetACTION_PACKAGE_FULLY_REMOVED: JString;
  function _GetACTION_PACKAGE_INSTALL: JString;
  function _GetACTION_PACKAGE_NEEDS_VERIFICATION: JString;
  function _GetACTION_PACKAGE_REMOVED: JString;
  function _GetACTION_PACKAGE_REPLACED: JString;
  function _GetACTION_PACKAGE_RESTARTED: JString;
  function _GetACTION_PACKAGE_VERIFIED: JString;
  function _GetACTION_PASTE: JString;
  function _GetACTION_PICK: JString;
  function _GetACTION_PICK_ACTIVITY: JString;
  function _GetACTION_POWER_CONNECTED: JString;
  function _GetACTION_POWER_DISCONNECTED: JString;
  function _GetACTION_POWER_USAGE_SUMMARY: JString;
  function _GetACTION_PROVIDER_CHANGED: JString;
  function _GetACTION_QUICK_CLOCK: JString;
  function _GetACTION_REBOOT: JString;
  function _GetACTION_RUN: JString;
  function _GetACTION_SCREEN_OFF: JString;
  function _GetACTION_SCREEN_ON: JString;
  function _GetACTION_SEARCH: JString;
  function _GetACTION_SEARCH_LONG_PRESS: JString;
  function _GetACTION_SEND: JString;
  function _GetACTION_SENDTO: JString;
  function _GetACTION_SEND_MULTIPLE: JString;
  function _GetACTION_SET_WALLPAPER: JString;
  function _GetACTION_SHUTDOWN: JString;
  function _GetACTION_SYNC: JString;
  function _GetACTION_SYSTEM_TUTORIAL: JString;
  function _GetACTION_TIMEZONE_CHANGED: JString;
  function _GetACTION_TIME_CHANGED: JString;
  function _GetACTION_TIME_TICK: JString;
  function _GetACTION_UID_REMOVED: JString;
  function _GetACTION_UMS_CONNECTED: JString;
  function _GetACTION_UMS_DISCONNECTED: JString;
  function _GetACTION_UNINSTALL_PACKAGE: JString;
  function _GetACTION_USER_BACKGROUND: JString;
  function _GetACTION_USER_FOREGROUND: JString;
  function _GetACTION_USER_INITIALIZE: JString;
  function _GetACTION_USER_PRESENT: JString;
  function _GetACTION_VIEW: JString;
  function _GetACTION_VOICE_COMMAND: JString;
  function _GetACTION_WALLPAPER_CHANGED: JString;
  function _GetACTION_WEB_SEARCH: JString;
  function _GetCATEGORY_ALTERNATIVE: JString;
  function _GetCATEGORY_APP_BROWSER: JString;
  function _GetCATEGORY_APP_CALCULATOR: JString;
  function _GetCATEGORY_APP_CALENDAR: JString;
  function _GetCATEGORY_APP_CONTACTS: JString;
  function _GetCATEGORY_APP_EMAIL: JString;
  function _GetCATEGORY_APP_GALLERY: JString;
  function _GetCATEGORY_APP_MAPS: JString;
  function _GetCATEGORY_APP_MARKET: JString;
  function _GetCATEGORY_APP_MESSAGING: JString;
  function _GetCATEGORY_APP_MUSIC: JString;
  function _GetCATEGORY_BROWSABLE: JString;
  function _GetCATEGORY_CAR_DOCK: JString;
  function _GetCATEGORY_CAR_MODE: JString;
  function _GetCATEGORY_DEFAULT: JString;
  function _GetCATEGORY_DESK_DOCK: JString;
  function _GetCATEGORY_DEVELOPMENT_PREFERENCE: JString;
  function _GetCATEGORY_EMBED: JString;
  function _GetCATEGORY_FRAMEWORK_INSTRUMENTATION_TEST: JString;
  function _GetCATEGORY_HE_DESK_DOCK: JString;
  function _GetCATEGORY_HOME: JString;
  function _GetCATEGORY_INFO: JString;
  function _GetCATEGORY_LAUNCHER: JString;
  function _GetCATEGORY_LE_DESK_DOCK: JString;
  function _GetCATEGORY_MONKEY: JString;
  function _GetCATEGORY_OPENABLE: JString;
  function _GetCATEGORY_PREFERENCE: JString;
  function _GetCATEGORY_SAMPLE_CODE: JString;
  function _GetCATEGORY_SELECTED_ALTERNATIVE: JString;
  function _GetCATEGORY_TAB: JString;
  function _GetCATEGORY_TEST: JString;
  function _GetCATEGORY_UNIT_TEST: JString;
  function _GetCREATOR: JParcelable_Creator;
  function _GetEXTRA_ALARM_COUNT: JString;
  function _GetEXTRA_ALLOW_REPLACE: JString;
  function _GetEXTRA_BCC: JString;
  function _GetEXTRA_BUG_REPORT: JString;
  function _GetEXTRA_CC: JString;
  function _GetEXTRA_CHANGED_COMPONENT_NAME: JString;
  function _GetEXTRA_CHANGED_COMPONENT_NAME_LIST: JString;
  function _GetEXTRA_CHANGED_PACKAGE_LIST: JString;
  function _GetEXTRA_CHANGED_UID_LIST: JString;
  function _GetEXTRA_DATA_REMOVED: JString;
  function _GetEXTRA_DOCK_STATE: JString;
  function _GetEXTRA_DOCK_STATE_CAR: Integer;
  function _GetEXTRA_DOCK_STATE_DESK: Integer;
  function _GetEXTRA_DOCK_STATE_HE_DESK: Integer;
  function _GetEXTRA_DOCK_STATE_LE_DESK: Integer;
  function _GetEXTRA_DOCK_STATE_UNDOCKED: Integer;
  function _GetEXTRA_DONT_KILL_APP: JString;
  function _GetEXTRA_EMAIL: JString;
  function _GetEXTRA_HTML_TEXT: JString;
  function _GetEXTRA_INITIAL_INTENTS: JString;
  function _GetEXTRA_INSTALLER_PACKAGE_NAME: JString;
  function _GetEXTRA_INTENT: JString;
  function _GetEXTRA_KEY_EVENT: JString;
  function _GetEXTRA_LOCAL_ONLY: JString;
  function _GetEXTRA_NOT_UNKNOWN_SOURCE: JString;
  function _GetEXTRA_ORIGINATING_URI: JString;
  function _GetEXTRA_PHONE_NUMBER: JString;
  function _GetEXTRA_REFERRER: JString;
  function _GetEXTRA_REMOTE_INTENT_TOKEN: JString;
  function _GetEXTRA_REPLACING: JString;
  function _GetEXTRA_RETURN_RESULT: JString;
  function _GetEXTRA_SHORTCUT_ICON: JString;
  function _GetEXTRA_SHORTCUT_ICON_RESOURCE: JString;
  function _GetEXTRA_SHORTCUT_INTENT: JString;
  function _GetEXTRA_SHORTCUT_NAME: JString;
  function _GetEXTRA_STREAM: JString;
  function _GetEXTRA_SUBJECT: JString;
  function _GetEXTRA_TEMPLATE: JString;
  function _GetEXTRA_TEXT: JString;
  function _GetEXTRA_TITLE: JString;
  function _GetEXTRA_UID: JString;
  function _GetFILL_IN_ACTION: Integer;
  function _GetFILL_IN_CATEGORIES: Integer;
  function _GetFILL_IN_CLIP_DATA: Integer;
  function _GetFILL_IN_COMPONENT: Integer;
  function _GetFILL_IN_DATA: Integer;
  function _GetFILL_IN_PACKAGE: Integer;
  function _GetFILL_IN_SELECTOR: Integer;
  function _GetFILL_IN_SOURCE_BOUNDS: Integer;
  function _GetFLAG_ACTIVITY_BROUGHT_TO_FRONT: Integer;
  function _GetFLAG_ACTIVITY_CLEAR_TASK: Integer;
  function _GetFLAG_ACTIVITY_CLEAR_TOP: Integer;
  function _GetFLAG_ACTIVITY_CLEAR_WHEN_TASK_RESET: Integer;
  function _GetFLAG_ACTIVITY_EXCLUDE_FROM_RECENTS: Integer;
  function _GetFLAG_ACTIVITY_FORWARD_RESULT: Integer;
  function _GetFLAG_ACTIVITY_LAUNCHED_FROM_HISTORY: Integer;
  function _GetFLAG_ACTIVITY_MULTIPLE_TASK: Integer;
  function _GetFLAG_ACTIVITY_NEW_TASK: Integer;
  function _GetFLAG_ACTIVITY_NO_ANIMATION: Integer;
  function _GetFLAG_ACTIVITY_NO_HISTORY: Integer;
  function _GetFLAG_ACTIVITY_NO_USER_ACTION: Integer;
  function _GetFLAG_ACTIVITY_PREVIOUS_IS_TOP: Integer;
  function _GetFLAG_ACTIVITY_REORDER_TO_FRONT: Integer;
  function _GetFLAG_ACTIVITY_RESET_TASK_IF_NEEDED: Integer;
  function _GetFLAG_ACTIVITY_SINGLE_TOP: Integer;
  function _GetFLAG_ACTIVITY_TASK_ON_HOME: Integer;
  function _GetFLAG_DEBUG_LOG_RESOLUTION: Integer;
  function _GetFLAG_EXCLUDE_STOPPED_PACKAGES: Integer;
  function _GetFLAG_FROM_BACKGROUND: Integer;
  function _GetFLAG_GRANT_READ_URI_PERMISSION: Integer;
  function _GetFLAG_GRANT_WRITE_URI_PERMISSION: Integer;
  function _GetFLAG_INCLUDE_STOPPED_PACKAGES: Integer;
  function _GetFLAG_RECEIVER_FOREGROUND: Integer;
  function _GetFLAG_RECEIVER_REGISTERED_ONLY: Integer;
  function _GetFLAG_RECEIVER_REPLACE_PENDING: Integer;
  function _GetMETADATA_DOCK_HOME: JString;
  function _GetURI_INTENT_SCHEME: Integer;
  {Methods}
  function init: JIntent; cdecl; overload;
  function init(o: JIntent): JIntent; cdecl; overload;
  function init(action: JString): JIntent; cdecl; overload;
  function init(action: JString; uri: Jnet_Uri): JIntent; cdecl; overload;
  function init(packageContext: JContext; cls: Jlang_Class): JIntent; cdecl; overload;
  function init(action: JString; uri: Jnet_Uri; packageContext: JContext; cls: Jlang_Class): JIntent; cdecl; overload;
  function createChooser(target: JIntent; title: JCharSequence): JIntent; cdecl;
  function getIntent(uri: JString): JIntent; cdecl;//Deprecated
  function getIntentOld(uri: JString): JIntent; cdecl;
  function makeMainActivity(mainActivity: JComponentName): JIntent; cdecl;
  function makeMainSelectorActivity(selectorAction: JString; selectorCategory: JString): JIntent; cdecl;
  function makeRestartActivityTask(mainActivity: JComponentName): JIntent; cdecl;
  function normalizeMimeType(type_: JString): JString; cdecl;
  function parseUri(uri: JString; flags: Integer): JIntent; cdecl;
  {Properties}
  property ACTION_AIRPLANE_MODE_CHANGED: JString read _GetACTION_AIRPLANE_MODE_CHANGED;
  property ACTION_ALL_APPS: JString read _GetACTION_ALL_APPS;
  property ACTION_ANSWER: JString read _GetACTION_ANSWER;
  property ACTION_APP_ERROR: JString read _GetACTION_APP_ERROR;
  property ACTION_ASSIST: JString read _GetACTION_ASSIST;
  property ACTION_ATTACH_DATA: JString read _GetACTION_ATTACH_DATA;
  property ACTION_BATTERY_CHANGED: JString read _GetACTION_BATTERY_CHANGED;
  property ACTION_BATTERY_LOW: JString read _GetACTION_BATTERY_LOW;
  property ACTION_BATTERY_OKAY: JString read _GetACTION_BATTERY_OKAY;
  property ACTION_BOOT_COMPLETED: JString read _GetACTION_BOOT_COMPLETED;
  property ACTION_BUG_REPORT: JString read _GetACTION_BUG_REPORT;
  property ACTION_CALL: JString read _GetACTION_CALL;
  property ACTION_CALL_BUTTON: JString read _GetACTION_CALL_BUTTON;
  property ACTION_CAMERA_BUTTON: JString read _GetACTION_CAMERA_BUTTON;
  property ACTION_CHOOSER: JString read _GetACTION_CHOOSER;
  property ACTION_CLOSE_SYSTEM_DIALOGS: JString read _GetACTION_CLOSE_SYSTEM_DIALOGS;
  property ACTION_CONFIGURATION_CHANGED: JString read _GetACTION_CONFIGURATION_CHANGED;
  property ACTION_CREATE_SHORTCUT: JString read _GetACTION_CREATE_SHORTCUT;
  property ACTION_DATE_CHANGED: JString read _GetACTION_DATE_CHANGED;
  property ACTION_DEFAULT: JString read _GetACTION_DEFAULT;
  property ACTION_DELETE: JString read _GetACTION_DELETE;
  property ACTION_DEVICE_STORAGE_LOW: JString read _GetACTION_DEVICE_STORAGE_LOW;
  property ACTION_DEVICE_STORAGE_OK: JString read _GetACTION_DEVICE_STORAGE_OK;
  property ACTION_DIAL: JString read _GetACTION_DIAL;
  property ACTION_DOCK_EVENT: JString read _GetACTION_DOCK_EVENT;
  property ACTION_DREAMING_STARTED: JString read _GetACTION_DREAMING_STARTED;
  property ACTION_DREAMING_STOPPED: JString read _GetACTION_DREAMING_STOPPED;
  property ACTION_EDIT: JString read _GetACTION_EDIT;
  property ACTION_EXTERNAL_APPLICATIONS_AVAILABLE: JString read _GetACTION_EXTERNAL_APPLICATIONS_AVAILABLE;
  property ACTION_EXTERNAL_APPLICATIONS_UNAVAILABLE: JString read _GetACTION_EXTERNAL_APPLICATIONS_UNAVAILABLE;
  property ACTION_FACTORY_TEST: JString read _GetACTION_FACTORY_TEST;
  property ACTION_GET_CONTENT: JString read _GetACTION_GET_CONTENT;
  property ACTION_GTALK_SERVICE_CONNECTED: JString read _GetACTION_GTALK_SERVICE_CONNECTED;
  property ACTION_GTALK_SERVICE_DISCONNECTED: JString read _GetACTION_GTALK_SERVICE_DISCONNECTED;
  property ACTION_HEADSET_PLUG: JString read _GetACTION_HEADSET_PLUG;
  property ACTION_INPUT_METHOD_CHANGED: JString read _GetACTION_INPUT_METHOD_CHANGED;
  property ACTION_INSERT: JString read _GetACTION_INSERT;
  property ACTION_INSERT_OR_EDIT: JString read _GetACTION_INSERT_OR_EDIT;
  property ACTION_INSTALL_PACKAGE: JString read _GetACTION_INSTALL_PACKAGE;
  property ACTION_LOCALE_CHANGED: JString read _GetACTION_LOCALE_CHANGED;
  property ACTION_MAIN: JString read _GetACTION_MAIN;
  property ACTION_MANAGE_NETWORK_USAGE: JString read _GetACTION_MANAGE_NETWORK_USAGE;
  property ACTION_MANAGE_PACKAGE_STORAGE: JString read _GetACTION_MANAGE_PACKAGE_STORAGE;
  property ACTION_MEDIA_BAD_REMOVAL: JString read _GetACTION_MEDIA_BAD_REMOVAL;
  property ACTION_MEDIA_BUTTON: JString read _GetACTION_MEDIA_BUTTON;
  property ACTION_MEDIA_CHECKING: JString read _GetACTION_MEDIA_CHECKING;
  property ACTION_MEDIA_EJECT: JString read _GetACTION_MEDIA_EJECT;
  property ACTION_MEDIA_MOUNTED: JString read _GetACTION_MEDIA_MOUNTED;
  property ACTION_MEDIA_NOFS: JString read _GetACTION_MEDIA_NOFS;
  property ACTION_MEDIA_REMOVED: JString read _GetACTION_MEDIA_REMOVED;
  property ACTION_MEDIA_SCANNER_FINISHED: JString read _GetACTION_MEDIA_SCANNER_FINISHED;
  property ACTION_MEDIA_SCANNER_SCAN_FILE: JString read _GetACTION_MEDIA_SCANNER_SCAN_FILE;
  property ACTION_MEDIA_SCANNER_STARTED: JString read _GetACTION_MEDIA_SCANNER_STARTED;
  property ACTION_MEDIA_SHARED: JString read _GetACTION_MEDIA_SHARED;
  property ACTION_MEDIA_UNMOUNTABLE: JString read _GetACTION_MEDIA_UNMOUNTABLE;
  property ACTION_MEDIA_UNMOUNTED: JString read _GetACTION_MEDIA_UNMOUNTED;
  property ACTION_MY_PACKAGE_REPLACED: JString read _GetACTION_MY_PACKAGE_REPLACED;
  property ACTION_NEW_OUTGOING_CALL: JString read _GetACTION_NEW_OUTGOING_CALL;
  property ACTION_PACKAGE_ADDED: JString read _GetACTION_PACKAGE_ADDED;
  property ACTION_PACKAGE_CHANGED: JString read _GetACTION_PACKAGE_CHANGED;
  property ACTION_PACKAGE_DATA_CLEARED: JString read _GetACTION_PACKAGE_DATA_CLEARED;
  property ACTION_PACKAGE_FIRST_LAUNCH: JString read _GetACTION_PACKAGE_FIRST_LAUNCH;
  property ACTION_PACKAGE_FULLY_REMOVED: JString read _GetACTION_PACKAGE_FULLY_REMOVED;
  property ACTION_PACKAGE_INSTALL: JString read _GetACTION_PACKAGE_INSTALL;
  property ACTION_PACKAGE_NEEDS_VERIFICATION: JString read _GetACTION_PACKAGE_NEEDS_VERIFICATION;
  property ACTION_PACKAGE_REMOVED: JString read _GetACTION_PACKAGE_REMOVED;
  property ACTION_PACKAGE_REPLACED: JString read _GetACTION_PACKAGE_REPLACED;
  property ACTION_PACKAGE_RESTARTED: JString read _GetACTION_PACKAGE_RESTARTED;
  property ACTION_PACKAGE_VERIFIED: JString read _GetACTION_PACKAGE_VERIFIED;
  property ACTION_PASTE: JString read _GetACTION_PASTE;
  property ACTION_PICK: JString read _GetACTION_PICK;
  property ACTION_PICK_ACTIVITY: JString read _GetACTION_PICK_ACTIVITY;
  property ACTION_POWER_CONNECTED: JString read _GetACTION_POWER_CONNECTED;
  property ACTION_POWER_DISCONNECTED: JString read _GetACTION_POWER_DISCONNECTED;
  property ACTION_POWER_USAGE_SUMMARY: JString read _GetACTION_POWER_USAGE_SUMMARY;
  property ACTION_PROVIDER_CHANGED: JString read _GetACTION_PROVIDER_CHANGED;
  property ACTION_QUICK_CLOCK: JString read _GetACTION_QUICK_CLOCK;
  property ACTION_REBOOT: JString read _GetACTION_REBOOT;
  property ACTION_RUN: JString read _GetACTION_RUN;
  property ACTION_SCREEN_OFF: JString read _GetACTION_SCREEN_OFF;
  property ACTION_SCREEN_ON: JString read _GetACTION_SCREEN_ON;
  property ACTION_SEARCH: JString read _GetACTION_SEARCH;
  property ACTION_SEARCH_LONG_PRESS: JString read _GetACTION_SEARCH_LONG_PRESS;
  property ACTION_SEND: JString read _GetACTION_SEND;
  property ACTION_SENDTO: JString read _GetACTION_SENDTO;
  property ACTION_SEND_MULTIPLE: JString read _GetACTION_SEND_MULTIPLE;
  property ACTION_SET_WALLPAPER: JString read _GetACTION_SET_WALLPAPER;
  property ACTION_SHUTDOWN: JString read _GetACTION_SHUTDOWN;
  property ACTION_SYNC: JString read _GetACTION_SYNC;
  property ACTION_SYSTEM_TUTORIAL: JString read _GetACTION_SYSTEM_TUTORIAL;
  property ACTION_TIMEZONE_CHANGED: JString read _GetACTION_TIMEZONE_CHANGED;
  property ACTION_TIME_CHANGED: JString read _GetACTION_TIME_CHANGED;
  property ACTION_TIME_TICK: JString read _GetACTION_TIME_TICK;
  property ACTION_UID_REMOVED: JString read _GetACTION_UID_REMOVED;
  property ACTION_UMS_CONNECTED: JString read _GetACTION_UMS_CONNECTED;
  property ACTION_UMS_DISCONNECTED: JString read _GetACTION_UMS_DISCONNECTED;
  property ACTION_UNINSTALL_PACKAGE: JString read _GetACTION_UNINSTALL_PACKAGE;
  property ACTION_USER_BACKGROUND: JString read _GetACTION_USER_BACKGROUND;
  property ACTION_USER_FOREGROUND: JString read _GetACTION_USER_FOREGROUND;
  property ACTION_USER_INITIALIZE: JString read _GetACTION_USER_INITIALIZE;
  property ACTION_USER_PRESENT: JString read _GetACTION_USER_PRESENT;
  property ACTION_VIEW: JString read _GetACTION_VIEW;
  property ACTION_VOICE_COMMAND: JString read _GetACTION_VOICE_COMMAND;
  property ACTION_WALLPAPER_CHANGED: JString read _GetACTION_WALLPAPER_CHANGED;
  property ACTION_WEB_SEARCH: JString read _GetACTION_WEB_SEARCH;
  property CATEGORY_ALTERNATIVE: JString read _GetCATEGORY_ALTERNATIVE;
  property CATEGORY_APP_BROWSER: JString read _GetCATEGORY_APP_BROWSER;
  property CATEGORY_APP_CALCULATOR: JString read _GetCATEGORY_APP_CALCULATOR;
  property CATEGORY_APP_CALENDAR: JString read _GetCATEGORY_APP_CALENDAR;
  property CATEGORY_APP_CONTACTS: JString read _GetCATEGORY_APP_CONTACTS;
  property CATEGORY_APP_EMAIL: JString read _GetCATEGORY_APP_EMAIL;
  property CATEGORY_APP_GALLERY: JString read _GetCATEGORY_APP_GALLERY;
  property CATEGORY_APP_MAPS: JString read _GetCATEGORY_APP_MAPS;
  property CATEGORY_APP_MARKET: JString read _GetCATEGORY_APP_MARKET;
  property CATEGORY_APP_MESSAGING: JString read _GetCATEGORY_APP_MESSAGING;
  property CATEGORY_APP_MUSIC: JString read _GetCATEGORY_APP_MUSIC;
  property CATEGORY_BROWSABLE: JString read _GetCATEGORY_BROWSABLE;
  property CATEGORY_CAR_DOCK: JString read _GetCATEGORY_CAR_DOCK;
  property CATEGORY_CAR_MODE: JString read _GetCATEGORY_CAR_MODE;
  property CATEGORY_DEFAULT: JString read _GetCATEGORY_DEFAULT;
  property CATEGORY_DESK_DOCK: JString read _GetCATEGORY_DESK_DOCK;
  property CATEGORY_DEVELOPMENT_PREFERENCE: JString read _GetCATEGORY_DEVELOPMENT_PREFERENCE;
  property CATEGORY_EMBED: JString read _GetCATEGORY_EMBED;
  property CATEGORY_FRAMEWORK_INSTRUMENTATION_TEST: JString read _GetCATEGORY_FRAMEWORK_INSTRUMENTATION_TEST;
  property CATEGORY_HE_DESK_DOCK: JString read _GetCATEGORY_HE_DESK_DOCK;
  property CATEGORY_HOME: JString read _GetCATEGORY_HOME;
  property CATEGORY_INFO: JString read _GetCATEGORY_INFO;
  property CATEGORY_LAUNCHER: JString read _GetCATEGORY_LAUNCHER;
  property CATEGORY_LE_DESK_DOCK: JString read _GetCATEGORY_LE_DESK_DOCK;
  property CATEGORY_MONKEY: JString read _GetCATEGORY_MONKEY;
  property CATEGORY_OPENABLE: JString read _GetCATEGORY_OPENABLE;
  property CATEGORY_PREFERENCE: JString read _GetCATEGORY_PREFERENCE;
  property CATEGORY_SAMPLE_CODE: JString read _GetCATEGORY_SAMPLE_CODE;
  property CATEGORY_SELECTED_ALTERNATIVE: JString read _GetCATEGORY_SELECTED_ALTERNATIVE;
  property CATEGORY_TAB: JString read _GetCATEGORY_TAB;
  property CATEGORY_TEST: JString read _GetCATEGORY_TEST;
  property CATEGORY_UNIT_TEST: JString read _GetCATEGORY_UNIT_TEST;
  property CREATOR: JParcelable_Creator read _GetCREATOR;
  property EXTRA_ALARM_COUNT: JString read _GetEXTRA_ALARM_COUNT;
  property EXTRA_ALLOW_REPLACE: JString read _GetEXTRA_ALLOW_REPLACE;
  property EXTRA_BCC: JString read _GetEXTRA_BCC;
  property EXTRA_BUG_REPORT: JString read _GetEXTRA_BUG_REPORT;
  property EXTRA_CC: JString read _GetEXTRA_CC;
  property EXTRA_CHANGED_COMPONENT_NAME: JString read _GetEXTRA_CHANGED_COMPONENT_NAME;
  property EXTRA_CHANGED_COMPONENT_NAME_LIST: JString read _GetEXTRA_CHANGED_COMPONENT_NAME_LIST;
  property EXTRA_CHANGED_PACKAGE_LIST: JString read _GetEXTRA_CHANGED_PACKAGE_LIST;
  property EXTRA_CHANGED_UID_LIST: JString read _GetEXTRA_CHANGED_UID_LIST;
  property EXTRA_DATA_REMOVED: JString read _GetEXTRA_DATA_REMOVED;
  property EXTRA_DOCK_STATE: JString read _GetEXTRA_DOCK_STATE;
  property EXTRA_DOCK_STATE_CAR: Integer read _GetEXTRA_DOCK_STATE_CAR;
  property EXTRA_DOCK_STATE_DESK: Integer read _GetEXTRA_DOCK_STATE_DESK;
  property EXTRA_DOCK_STATE_HE_DESK: Integer read _GetEXTRA_DOCK_STATE_HE_DESK;
  property EXTRA_DOCK_STATE_LE_DESK: Integer read _GetEXTRA_DOCK_STATE_LE_DESK;
  property EXTRA_DOCK_STATE_UNDOCKED: Integer read _GetEXTRA_DOCK_STATE_UNDOCKED;
  property EXTRA_DONT_KILL_APP: JString read _GetEXTRA_DONT_KILL_APP;
  property EXTRA_EMAIL: JString read _GetEXTRA_EMAIL;
  property EXTRA_HTML_TEXT: JString read _GetEXTRA_HTML_TEXT;
  property EXTRA_INITIAL_INTENTS: JString read _GetEXTRA_INITIAL_INTENTS;
  property EXTRA_INSTALLER_PACKAGE_NAME: JString read _GetEXTRA_INSTALLER_PACKAGE_NAME;
  property EXTRA_INTENT: JString read _GetEXTRA_INTENT;
  property EXTRA_KEY_EVENT: JString read _GetEXTRA_KEY_EVENT;
  property EXTRA_LOCAL_ONLY: JString read _GetEXTRA_LOCAL_ONLY;
  property EXTRA_NOT_UNKNOWN_SOURCE: JString read _GetEXTRA_NOT_UNKNOWN_SOURCE;
  property EXTRA_ORIGINATING_URI: JString read _GetEXTRA_ORIGINATING_URI;
  property EXTRA_PHONE_NUMBER: JString read _GetEXTRA_PHONE_NUMBER;
  property EXTRA_REFERRER: JString read _GetEXTRA_REFERRER;
  property EXTRA_REMOTE_INTENT_TOKEN: JString read _GetEXTRA_REMOTE_INTENT_TOKEN;
  property EXTRA_REPLACING: JString read _GetEXTRA_REPLACING;
  property EXTRA_RETURN_RESULT: JString read _GetEXTRA_RETURN_RESULT;
  property EXTRA_SHORTCUT_ICON: JString read _GetEXTRA_SHORTCUT_ICON;
  property EXTRA_SHORTCUT_ICON_RESOURCE: JString read _GetEXTRA_SHORTCUT_ICON_RESOURCE;
  property EXTRA_SHORTCUT_INTENT: JString read _GetEXTRA_SHORTCUT_INTENT;
  property EXTRA_SHORTCUT_NAME: JString read _GetEXTRA_SHORTCUT_NAME;
  property EXTRA_STREAM: JString read _GetEXTRA_STREAM;
  property EXTRA_SUBJECT: JString read _GetEXTRA_SUBJECT;
  property EXTRA_TEMPLATE: JString read _GetEXTRA_TEMPLATE;
  property EXTRA_TEXT: JString read _GetEXTRA_TEXT;
  property EXTRA_TITLE: JString read _GetEXTRA_TITLE;
  property EXTRA_UID: JString read _GetEXTRA_UID;
  property FILL_IN_ACTION: Integer read _GetFILL_IN_ACTION;
  property FILL_IN_CATEGORIES: Integer read _GetFILL_IN_CATEGORIES;
  property FILL_IN_CLIP_DATA: Integer read _GetFILL_IN_CLIP_DATA;
  property FILL_IN_COMPONENT: Integer read _GetFILL_IN_COMPONENT;
  property FILL_IN_DATA: Integer read _GetFILL_IN_DATA;
  property FILL_IN_PACKAGE: Integer read _GetFILL_IN_PACKAGE;
  property FILL_IN_SELECTOR: Integer read _GetFILL_IN_SELECTOR;
  property FILL_IN_SOURCE_BOUNDS: Integer read _GetFILL_IN_SOURCE_BOUNDS;
  property FLAG_ACTIVITY_BROUGHT_TO_FRONT: Integer read _GetFLAG_ACTIVITY_BROUGHT_TO_FRONT;
  property FLAG_ACTIVITY_CLEAR_TASK: Integer read _GetFLAG_ACTIVITY_CLEAR_TASK;
  property FLAG_ACTIVITY_CLEAR_TOP: Integer read _GetFLAG_ACTIVITY_CLEAR_TOP;
  property FLAG_ACTIVITY_CLEAR_WHEN_TASK_RESET: Integer read _GetFLAG_ACTIVITY_CLEAR_WHEN_TASK_RESET;
  property FLAG_ACTIVITY_EXCLUDE_FROM_RECENTS: Integer read _GetFLAG_ACTIVITY_EXCLUDE_FROM_RECENTS;
  property FLAG_ACTIVITY_FORWARD_RESULT: Integer read _GetFLAG_ACTIVITY_FORWARD_RESULT;
  property FLAG_ACTIVITY_LAUNCHED_FROM_HISTORY: Integer read _GetFLAG_ACTIVITY_LAUNCHED_FROM_HISTORY;
  property FLAG_ACTIVITY_MULTIPLE_TASK: Integer read _GetFLAG_ACTIVITY_MULTIPLE_TASK;
  property FLAG_ACTIVITY_NEW_TASK: Integer read _GetFLAG_ACTIVITY_NEW_TASK;
  property FLAG_ACTIVITY_NO_ANIMATION: Integer read _GetFLAG_ACTIVITY_NO_ANIMATION;
  property FLAG_ACTIVITY_NO_HISTORY: Integer read _GetFLAG_ACTIVITY_NO_HISTORY;
  property FLAG_ACTIVITY_NO_USER_ACTION: Integer read _GetFLAG_ACTIVITY_NO_USER_ACTION;
  property FLAG_ACTIVITY_PREVIOUS_IS_TOP: Integer read _GetFLAG_ACTIVITY_PREVIOUS_IS_TOP;
  property FLAG_ACTIVITY_REORDER_TO_FRONT: Integer read _GetFLAG_ACTIVITY_REORDER_TO_FRONT;
  property FLAG_ACTIVITY_RESET_TASK_IF_NEEDED: Integer read _GetFLAG_ACTIVITY_RESET_TASK_IF_NEEDED;
  property FLAG_ACTIVITY_SINGLE_TOP: Integer read _GetFLAG_ACTIVITY_SINGLE_TOP;
  property FLAG_ACTIVITY_TASK_ON_HOME: Integer read _GetFLAG_ACTIVITY_TASK_ON_HOME;
  property FLAG_DEBUG_LOG_RESOLUTION: Integer read _GetFLAG_DEBUG_LOG_RESOLUTION;
  property FLAG_EXCLUDE_STOPPED_PACKAGES: Integer read _GetFLAG_EXCLUDE_STOPPED_PACKAGES;
  property FLAG_FROM_BACKGROUND: Integer read _GetFLAG_FROM_BACKGROUND;
  property FLAG_GRANT_READ_URI_PERMISSION: Integer read _GetFLAG_GRANT_READ_URI_PERMISSION;
  property FLAG_GRANT_WRITE_URI_PERMISSION: Integer read _GetFLAG_GRANT_WRITE_URI_PERMISSION;
  property FLAG_INCLUDE_STOPPED_PACKAGES: Integer read _GetFLAG_INCLUDE_STOPPED_PACKAGES;
  property FLAG_RECEIVER_FOREGROUND: Integer read _GetFLAG_RECEIVER_FOREGROUND;
  property FLAG_RECEIVER_REGISTERED_ONLY: Integer read _GetFLAG_RECEIVER_REGISTERED_ONLY;
  property FLAG_RECEIVER_REPLACE_PENDING: Integer read _GetFLAG_RECEIVER_REPLACE_PENDING;
  property METADATA_DOCK_HOME: JString read _GetMETADATA_DOCK_HOME;
  property URI_INTENT_SCHEME: Integer read _GetURI_INTENT_SCHEME;
end;

[JavaSignature('android/content/Intent')]
JIntent = interface(JObject)
['{022B0525-86CA-41C8-90D4-3559F098275A}']
  {Methods}
  function addCategory(category: JString): JIntent; cdecl;
  function addFlags(flags: Integer): JIntent; cdecl;
  function clone: JObject; cdecl;
  function cloneFilter: JIntent; cdecl;
  function describeContents: Integer; cdecl;
  function fillIn(other: JIntent; flags: Integer): Integer; cdecl;
  function filterEquals(other: JIntent): Boolean; cdecl;
  function filterHashCode: Integer; cdecl;
  function getAction: JString; cdecl;
  function getBooleanArrayExtra(name: JString): TJavaArray<Boolean>; cdecl;
  function getBooleanExtra(name: JString; defaultValue: Boolean): Boolean; cdecl;
  function getBundleExtra(name: JString): JBundle; cdecl;
  function getByteArrayExtra(name: JString): TJavaArray<Byte>; cdecl;
  function getByteExtra(name: JString; defaultValue: Byte): Byte; cdecl;
  function getCategories: JSet; cdecl;
  function getCharArrayExtra(name: JString): TJavaArray<Char>; cdecl;
  function getCharExtra(name: JString; defaultValue: Char): Char; cdecl;
  function getCharSequenceArrayExtra(name: JString): TJavaObjectArray<JCharSequence>; cdecl;
  function getCharSequenceArrayListExtra(name: JString): JArrayList; cdecl;
  function getCharSequenceExtra(name: JString): JCharSequence; cdecl;
  function getClipData: JClipData; cdecl;
  function getComponent: JComponentName; cdecl;
  function getData: Jnet_Uri; cdecl;
  function getDataString: JString; cdecl;
  function getDoubleArrayExtra(name: JString): TJavaArray<Double>; cdecl;
  function getDoubleExtra(name: JString; defaultValue: Double): Double; cdecl;
  function getExtras: JBundle; cdecl;
  function getFlags: Integer; cdecl;
  function getFloatArrayExtra(name: JString): TJavaArray<Single>; cdecl;
  function getFloatExtra(name: JString; defaultValue: Single): Single; cdecl;
  function getIntArrayExtra(name: JString): TJavaArray<Integer>; cdecl;
  function getIntExtra(name: JString; defaultValue: Integer): Integer; cdecl;
  function getIntegerArrayListExtra(name: JString): JArrayList; cdecl;
  function getLongArrayExtra(name: JString): TJavaArray<Int64>; cdecl;
  function getLongExtra(name: JString; defaultValue: Int64): Int64; cdecl;
  function getPackage: JString; cdecl;
  function getParcelableArrayExtra(name: JString): TJavaObjectArray<JParcelable>; cdecl;
  function getParcelableArrayListExtra(name: JString): JArrayList; cdecl;
  function getParcelableExtra(name: JString): JParcelable; cdecl;
  function getScheme: JString; cdecl;
  function getSelector: JIntent; cdecl;
  function getSerializableExtra(name: JString): JSerializable; cdecl;
  function getShortArrayExtra(name: JString): TJavaArray<SmallInt>; cdecl;
  function getShortExtra(name: JString; defaultValue: SmallInt): SmallInt; cdecl;
  function getSourceBounds: JRect; cdecl;
  function getStringArrayExtra(name: JString): TJavaObjectArray<JString>; cdecl;
  function getStringArrayListExtra(name: JString): JArrayList; cdecl;
  function getStringExtra(name: JString): JString; cdecl;
  function getType: JString; cdecl;
  function hasCategory(category: JString): Boolean; cdecl;
  function hasExtra(name: JString): Boolean; cdecl;
  function hasFileDescriptors: Boolean; cdecl;
  function putCharSequenceArrayListExtra(name: JString; value: JArrayList): JIntent; cdecl;
  function putExtra(name: JString; value: Boolean): JIntent; cdecl; overload;
  function putExtra(name: JString; value: Byte): JIntent; cdecl; overload;
  function putExtra(name: JString; value: Char): JIntent; cdecl; overload;
  function putExtra(name: JString; value: SmallInt): JIntent; cdecl; overload;
  function putExtra(name: JString; value: Integer): JIntent; cdecl; overload;
  function putExtra(name: JString; value: Int64): JIntent; cdecl; overload;
  function putExtra(name: JString; value: Single): JIntent; cdecl; overload;
  function putExtra(name: JString; value: Double): JIntent; cdecl; overload;
  function putExtra(name: JString; value: JString): JIntent; cdecl; overload;
  function putExtra(name: JString; value: JCharSequence): JIntent; cdecl; overload;
  function putExtra(name: JString; value: JParcelable): JIntent; cdecl; overload;
  function putExtra(name: JString; value: TJavaObjectArray<JParcelable>): JIntent; cdecl; overload;
  function putExtra(name: JString; value: JSerializable): JIntent; cdecl; overload;
  function putExtra(name: JString; value: TJavaArray<Boolean>): JIntent; cdecl; overload;
  function putExtra(name: JString; value: TJavaArray<Byte>): JIntent; cdecl; overload;
  function putExtra(name: JString; value: TJavaArray<SmallInt>): JIntent; cdecl; overload;
  function putExtra(name: JString; value: TJavaArray<Char>): JIntent; cdecl; overload;
  function putExtra(name: JString; value: TJavaArray<Integer>): JIntent; cdecl; overload;
  function putExtra(name: JString; value: TJavaArray<Int64>): JIntent; cdecl; overload;
  function putExtra(name: JString; value: TJavaArray<Single>): JIntent; cdecl; overload;
  function putExtra(name: JString; value: TJavaArray<Double>): JIntent; cdecl; overload;
  function putExtra(name: JString; value: TJavaObjectArray<JString>): JIntent; cdecl; overload;
  function putExtra(name: JString; value: TJavaObjectArray<JCharSequence>): JIntent; cdecl; overload;
  function putExtra(name: JString; value: JBundle): JIntent; cdecl; overload;
  function putExtras(src: JIntent): JIntent; cdecl; overload;
  function putExtras(extras: JBundle): JIntent; cdecl; overload;
  function putIntegerArrayListExtra(name: JString; value: JArrayList): JIntent; cdecl;
  function putParcelableArrayListExtra(name: JString; value: JArrayList): JIntent; cdecl;
  function putStringArrayListExtra(name: JString; value: JArrayList): JIntent; cdecl;
  procedure readFromParcel(in_: JParcel); cdecl;
  procedure removeCategory(category: JString); cdecl;
  procedure removeExtra(name: JString); cdecl;
  function replaceExtras(src: JIntent): JIntent; cdecl; overload;
  function replaceExtras(extras: JBundle): JIntent; cdecl; overload;
  function resolveActivity(pm: JPackageManager): JComponentName; cdecl;
  function resolveActivityInfo(pm: JPackageManager; flags: Integer): JActivityInfo; cdecl;
  function resolveType(context: JContext): JString; cdecl; overload;
  function resolveType(resolver: JContentResolver): JString; cdecl; overload;
  function resolveTypeIfNeeded(resolver: JContentResolver): JString; cdecl;
  function setAction(action: JString): JIntent; cdecl;
  function setClass(packageContext: JContext; cls: Jlang_Class): JIntent; cdecl;
  function setClassName(packageContext: JContext; className: JString): JIntent; cdecl; overload;
  function setClassName(packageName: JString; className: JString): JIntent; cdecl; overload;
  procedure setClipData(clip: JClipData); cdecl;
  function setComponent(component: JComponentName): JIntent; cdecl;
  function setData(data: Jnet_Uri): JIntent; cdecl;
  function setDataAndNormalize(data: Jnet_Uri): JIntent; cdecl;
  function setDataAndType(data: Jnet_Uri; type_: JString): JIntent; cdecl;
  function setDataAndTypeAndNormalize(data: Jnet_Uri; type_: JString): JIntent; cdecl;
  procedure setExtrasClassLoader(loader: JClassLoader); cdecl;
  function setFlags(flags: Integer): JIntent; cdecl;
  function setPackage(packageName: JString): JIntent; cdecl;
  procedure setSelector(selector: JIntent); cdecl;
  procedure setSourceBounds(r: JRect); cdecl;
  function setType(type_: JString): JIntent; cdecl;
  function setTypeAndNormalize(type_: JString): JIntent; cdecl;
  function toString: JString; cdecl;
  function toURI: JString; cdecl; overload;//Deprecated
  function toUri(flags: Integer): JString; cdecl; overload;
  procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
end;
TJIntent = class(TJavaGenericImport<JIntentClass, JIntent>) end;

JLabeledIntentClass = interface(JIntentClass)
['{1A42C952-80B3-4EC9-8C95-4FDE9E0D653F}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Methods}
  function init(origIntent: JIntent; sourcePackage: JString; labelRes: Integer; icon: Integer): JLabeledIntent; cdecl; overload;
  function init(origIntent: JIntent; sourcePackage: JString; nonLocalizedLabel: JCharSequence; icon: Integer): JLabeledIntent; cdecl; overload;
  function init(sourcePackage: JString; labelRes: Integer; icon: Integer): JLabeledIntent; cdecl; overload;
  function init(sourcePackage: JString; nonLocalizedLabel: JCharSequence; icon: Integer): JLabeledIntent; cdecl; overload;
  {Properties}
{CREATOR is defined in parent interface}
end;

[JavaSignature('android/content/pm/LabeledIntent')]
JLabeledIntent = interface(JIntent)
['{0127A87D-88DB-493E-AC69-4E00ADC4F08F}']
  {Methods}
  function getIconResource: Integer; cdecl;
  function getLabelResource: Integer; cdecl;
  function getNonLocalizedLabel: JCharSequence; cdecl;
  function getSourcePackage: JString; cdecl;
  function loadIcon(pm: JPackageManager): JDrawable; cdecl;
  function loadLabel(pm: JPackageManager): JCharSequence; cdecl;
  procedure readFromParcel(in_: JParcel); cdecl;
  procedure writeToParcel(dest: JParcel; parcelableFlags: Integer); cdecl;
end;
TJLabeledIntent = class(TJavaGenericImport<JLabeledIntentClass, JLabeledIntent>) end;

JIntentSender_OnFinishedClass = interface(IJavaClass)
['{3812401C-6A82-4684-9C66-70945067ABFD}']
end;

[JavaSignature('android/content/IntentSender$OnFinished')]
JIntentSender_OnFinished = interface(IJavaInstance)
['{80F2FB82-F57E-4604-A556-0012FAC55805}']
  {Methods}
  procedure onSendFinished(IntentSender: JIntentSender; intent: JIntent; resultCode: Integer; resultData: JString; resultExtras: JBundle); cdecl;
end;
TJIntentSender_OnFinished = class(TJavaGenericImport<JIntentSender_OnFinishedClass, JIntentSender_OnFinished>) end;

JSQLiteProgramClass = interface(JSQLiteClosableClass)
['{1CAB352F-57F3-487F-9089-5E038EE18DE8}']
end;

[JavaSignature('android/database/sqlite/SQLiteProgram')]
JSQLiteProgram = interface(JSQLiteClosable)
['{0C26AE0C-8368-437A-A879-44DF499D9AD6}']
  {Methods}
  procedure bindAllArgsAsStrings(bindArgs: TJavaObjectArray<JString>); cdecl;
  procedure bindBlob(index: Integer; value: TJavaArray<Byte>); cdecl;
  procedure bindDouble(index: Integer; value: Double); cdecl;
  procedure bindLong(index: Integer; value: Int64); cdecl;
  procedure bindNull(index: Integer); cdecl;
  procedure bindString(index: Integer; value: JString); cdecl;
  procedure clearBindings; cdecl;
  function getUniqueId: Integer; cdecl;//Deprecated
end;
TJSQLiteProgram = class(TJavaGenericImport<JSQLiteProgramClass, JSQLiteProgram>) end;

JMovieClass = interface(JObjectClass)
['{A8B58AED-7BF1-4A1D-872A-B066AF81A108}']
  {Methods}
  function decodeByteArray(data: TJavaArray<Byte>; offset: Integer; length: Integer): JMovie; cdecl;
  function decodeFile(pathName: JString): JMovie; cdecl;
  function decodeStream(is_: JInputStream): JMovie; cdecl;
end;

[JavaSignature('android/graphics/Movie')]
JMovie = interface(JObject)
['{05A99234-057F-4B23-A52D-07C6C1175265}']
  {Methods}
  procedure draw(canvas: JCanvas; x: Single; y: Single; paint: JPaint); cdecl; overload;
  procedure draw(canvas: JCanvas; x: Single; y: Single); cdecl; overload;
  function duration: Integer; cdecl;
  function height: Integer; cdecl;
  function isOpaque: Boolean; cdecl;
  function setTime(relativeMilliseconds: Integer): Boolean; cdecl;
  function width: Integer; cdecl;
end;
TJMovie = class(TJavaGenericImport<JMovieClass, JMovie>) end;

JPathPermissionClass = interface(JPatternMatcherClass)
['{DBBBF98A-864E-4D1C-B4EB-CC18420332D6}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Methods}
  function init(pattern: JString; type_: Integer; readPermission: JString; writePermission: JString): JPathPermission; cdecl; overload;
  function init(src: JParcel): JPathPermission; cdecl; overload;
  {Properties}
{CREATOR is defined in parent interface}
end;

[JavaSignature('android/content/pm/PathPermission')]
JPathPermission = interface(JPatternMatcher)
['{E7931286-8F93-4F0D-A794-700D7CE7E59D}']
  {Methods}
  function getReadPermission: JString; cdecl;
  function getWritePermission: JString; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
end;
TJPathPermission = class(TJavaGenericImport<JPathPermissionClass, JPathPermission>) end;

JActionModeClass = interface(JObjectClass)
['{71825176-284A-429E-8391-FB39774F4500}']
  {Methods}
  function init: JActionMode; cdecl;
end;

[JavaSignature('android/view/ActionMode')]
JActionMode = interface(JObject)
['{2D0091FA-DF79-4640-BA1D-FACF25666C4B}']
  {Methods}
  procedure finish; cdecl;
  function getCustomView: JView; cdecl;
  function getMenu: JMenu; cdecl;
  function getMenuInflater: JMenuInflater; cdecl;
  function getSubtitle: JCharSequence; cdecl;
  function getTag: JObject; cdecl;
  function getTitle: JCharSequence; cdecl;
  function getTitleOptionalHint: Boolean; cdecl;
  procedure invalidate; cdecl;
  function isTitleOptional: Boolean; cdecl;
  procedure setCustomView(view: JView); cdecl;
  procedure setSubtitle(subtitle: JCharSequence); cdecl; overload;
  procedure setSubtitle(resId: Integer); cdecl; overload;
  procedure setTag(tag: JObject); cdecl;
  procedure setTitle(title: JCharSequence); cdecl; overload;
  procedure setTitle(resId: Integer); cdecl; overload;
  procedure setTitleOptionalHint(titleOptional: Boolean); cdecl;
end;
TJActionMode = class(TJavaGenericImport<JActionModeClass, JActionMode>) end;

JPackageItemInfo_DisplayNameComparatorClass = interface(JObjectClass)
['{D65CE03C-43AB-4F7A-8B76-F9631BC95D98}']
  {Methods}
  function init(pm: JPackageManager): JPackageItemInfo_DisplayNameComparator; cdecl;
end;

[JavaSignature('android/content/pm/PackageItemInfo$DisplayNameComparator')]
JPackageItemInfo_DisplayNameComparator = interface(JObject)
['{9B9CE437-9EBC-46BB-9334-F138DE5DCD5C}']
  {Methods}
  function compare(aa: JPackageItemInfo; ab: JPackageItemInfo): Integer; cdecl;
end;
TJPackageItemInfo_DisplayNameComparator = class(TJavaGenericImport<JPackageItemInfo_DisplayNameComparatorClass, JPackageItemInfo_DisplayNameComparator>) end;

JLoaderClass = interface(JObjectClass)
['{3579AEB2-2950-4D22-B62F-8FE4EAEDDD5D}']
  {Methods}
  function init(context: JContext): JLoader; cdecl;
end;

[JavaSignature('android/content/Loader')]
JLoader = interface(JObject)
['{57E129AA-1B42-4642-A3AF-08E4EB94EE0D}']
  {Methods}
  procedure abandon; cdecl;
  function cancelLoad: Boolean; cdecl;
  function dataToString(data: JObject): JString; cdecl;
  procedure deliverCancellation; cdecl;
  procedure deliverResult(data: JObject); cdecl;
  procedure dump(prefix: JString; fd: JFileDescriptor; writer: JPrintWriter; args: TJavaObjectArray<JString>); cdecl;
  procedure forceLoad; cdecl;
  function getContext: JContext; cdecl;
  function getId: Integer; cdecl;
  function isAbandoned: Boolean; cdecl;
  function isReset: Boolean; cdecl;
  function isStarted: Boolean; cdecl;
  procedure onContentChanged; cdecl;
  procedure registerListener(id: Integer; listener: JLoader_OnLoadCompleteListener); cdecl;
  procedure registerOnLoadCanceledListener(listener: JLoader_OnLoadCanceledListener); cdecl;
  procedure reset; cdecl;
  procedure startLoading; cdecl;
  procedure stopLoading; cdecl;
  function takeContentChanged: Boolean; cdecl;
  function toString: JString; cdecl;
  procedure unregisterListener(listener: JLoader_OnLoadCompleteListener); cdecl;
  procedure unregisterOnLoadCanceledListener(listener: JLoader_OnLoadCanceledListener); cdecl;
end;
TJLoader = class(TJavaGenericImport<JLoaderClass, JLoader>) end;

JAsyncTaskLoaderClass = interface(JLoaderClass)
['{A062D485-BEC3-4495-9879-3A7C567EE29F}']
  {Methods}
  function init(context: JContext): JAsyncTaskLoader; cdecl;
end;

[JavaSignature('android/content/AsyncTaskLoader')]
JAsyncTaskLoader = interface(JLoader)
['{B9A48AE1-A477-4851-A889-06A23B999FF2}']
  {Methods}
  procedure cancelLoadInBackground; cdecl;
  procedure dump(prefix: JString; fd: JFileDescriptor; writer: JPrintWriter; args: TJavaObjectArray<JString>); cdecl;
  function isLoadInBackgroundCanceled: Boolean; cdecl;
  function loadInBackground: JObject; cdecl;
  procedure onCanceled(data: JObject); cdecl;
  procedure setUpdateThrottle(delayMS: Int64); cdecl;
end;
TJAsyncTaskLoader = class(TJavaGenericImport<JAsyncTaskLoaderClass, JAsyncTaskLoader>) end;

JEditorInfoClass = interface(JObjectClass)
['{86A3770F-C28B-4553-8DD5-3BE7E4DBD2BF}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  function _GetIME_ACTION_DONE: Integer;
  function _GetIME_ACTION_GO: Integer;
  function _GetIME_ACTION_NEXT: Integer;
  function _GetIME_ACTION_NONE: Integer;
  function _GetIME_ACTION_PREVIOUS: Integer;
  function _GetIME_ACTION_SEARCH: Integer;
  function _GetIME_ACTION_SEND: Integer;
  function _GetIME_ACTION_UNSPECIFIED: Integer;
  function _GetIME_FLAG_FORCE_ASCII: Integer;
  function _GetIME_FLAG_NAVIGATE_NEXT: Integer;
  function _GetIME_FLAG_NAVIGATE_PREVIOUS: Integer;
  function _GetIME_FLAG_NO_ACCESSORY_ACTION: Integer;
  function _GetIME_FLAG_NO_ENTER_ACTION: Integer;
  function _GetIME_FLAG_NO_EXTRACT_UI: Integer;
  function _GetIME_FLAG_NO_FULLSCREEN: Integer;
  function _GetIME_MASK_ACTION: Integer;
  function _GetIME_NULL: Integer;
  {Methods}
  function init: JEditorInfo; cdecl;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
  property IME_ACTION_DONE: Integer read _GetIME_ACTION_DONE;
  property IME_ACTION_GO: Integer read _GetIME_ACTION_GO;
  property IME_ACTION_NEXT: Integer read _GetIME_ACTION_NEXT;
  property IME_ACTION_NONE: Integer read _GetIME_ACTION_NONE;
  property IME_ACTION_PREVIOUS: Integer read _GetIME_ACTION_PREVIOUS;
  property IME_ACTION_SEARCH: Integer read _GetIME_ACTION_SEARCH;
  property IME_ACTION_SEND: Integer read _GetIME_ACTION_SEND;
  property IME_ACTION_UNSPECIFIED: Integer read _GetIME_ACTION_UNSPECIFIED;
  property IME_FLAG_FORCE_ASCII: Integer read _GetIME_FLAG_FORCE_ASCII;
  property IME_FLAG_NAVIGATE_NEXT: Integer read _GetIME_FLAG_NAVIGATE_NEXT;
  property IME_FLAG_NAVIGATE_PREVIOUS: Integer read _GetIME_FLAG_NAVIGATE_PREVIOUS;
  property IME_FLAG_NO_ACCESSORY_ACTION: Integer read _GetIME_FLAG_NO_ACCESSORY_ACTION;
  property IME_FLAG_NO_ENTER_ACTION: Integer read _GetIME_FLAG_NO_ENTER_ACTION;
  property IME_FLAG_NO_EXTRACT_UI: Integer read _GetIME_FLAG_NO_EXTRACT_UI;
  property IME_FLAG_NO_FULLSCREEN: Integer read _GetIME_FLAG_NO_FULLSCREEN;
  property IME_MASK_ACTION: Integer read _GetIME_MASK_ACTION;
  property IME_NULL: Integer read _GetIME_NULL;
end;

[JavaSignature('android/view/inputmethod/EditorInfo')]
JEditorInfo = interface(JObject)
['{9D8B3117-5784-44CF-85A8-1845595D4A2C}']
  {Property Methods}
  function _GetactionId: Integer;
  procedure _SetactionId(Value: Integer);
  function _GetactionLabel: JCharSequence;
  procedure _SetactionLabel(Value: JCharSequence);
  function _Getextras: JBundle;
  procedure _Setextras(Value: JBundle);
  function _GetfieldId: Integer;
  procedure _SetfieldId(Value: Integer);
  function _GetfieldName: JString;
  procedure _SetfieldName(Value: JString);
  function _GethintText: JCharSequence;
  procedure _SethintText(Value: JCharSequence);
  function _GetimeOptions: Integer;
  procedure _SetimeOptions(Value: Integer);
  function _GetinitialCapsMode: Integer;
  procedure _SetinitialCapsMode(Value: Integer);
  function _GetinitialSelEnd: Integer;
  procedure _SetinitialSelEnd(Value: Integer);
  function _GetinitialSelStart: Integer;
  procedure _SetinitialSelStart(Value: Integer);
  function _GetinputType: Integer;
  procedure _SetinputType(Value: Integer);
  function _Getlabel: JCharSequence;
  procedure _Setlabel(Value: JCharSequence);
  function _GetpackageName: JString;
  procedure _SetpackageName(Value: JString);
  function _GetprivateImeOptions: JString;
  procedure _SetprivateImeOptions(Value: JString);
  {Methods}
  function describeContents: Integer; cdecl;
  procedure makeCompatible(targetSdkVersion: Integer); cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  {Properties}
  property actionId: Integer read _GetactionId write _SetactionId;
  property actionLabel: JCharSequence read _GetactionLabel write _SetactionLabel;
  property extras: JBundle read _Getextras write _Setextras;
  property fieldId: Integer read _GetfieldId write _SetfieldId;
  property fieldName: JString read _GetfieldName write _SetfieldName;
  property hintText: JCharSequence read _GethintText write _SethintText;
  property imeOptions: Integer read _GetimeOptions write _SetimeOptions;
  property initialCapsMode: Integer read _GetinitialCapsMode write _SetinitialCapsMode;
  property initialSelEnd: Integer read _GetinitialSelEnd write _SetinitialSelEnd;
  property initialSelStart: Integer read _GetinitialSelStart write _SetinitialSelStart;
  property inputType: Integer read _GetinputType write _SetinputType;
  property &label: JCharSequence read _Getlabel write _Setlabel;
  property packageName: JString read _GetpackageName write _SetpackageName;
  property privateImeOptions: JString read _GetprivateImeOptions write _SetprivateImeOptions;
end;
TJEditorInfo = class(TJavaGenericImport<JEditorInfoClass, JEditorInfo>) end;

JResolveInfo_DisplayNameComparatorClass = interface(JObjectClass)
['{1A75CFB1-B2D2-4C6F-9424-71DBF8536869}']
  {Methods}
  function init(pm: JPackageManager): JResolveInfo_DisplayNameComparator; cdecl;
end;

[JavaSignature('android/content/pm/ResolveInfo$DisplayNameComparator')]
JResolveInfo_DisplayNameComparator = interface(JObject)
['{6D7217A8-3402-4DF8-8DD8-4DB3429194D0}']
  {Methods}
  function compare(a: JResolveInfo; b: JResolveInfo): Integer; cdecl;
end;
TJResolveInfo_DisplayNameComparator = class(TJavaGenericImport<JResolveInfo_DisplayNameComparatorClass, JResolveInfo_DisplayNameComparator>) end;

JBroadcastReceiver_PendingResultClass = interface(JObjectClass)
['{41EB4E26-FA55-4428-819D-0C56DC11596B}']
end;

[JavaSignature('android/content/BroadcastReceiver$PendingResult')]
JBroadcastReceiver_PendingResult = interface(JObject)
['{8F52DCF6-7EEC-4EBA-8A4D-A1DE293A1A41}']
  {Methods}
  procedure abortBroadcast; cdecl;
  procedure clearAbortBroadcast; cdecl;
  procedure finish; cdecl;
  function getAbortBroadcast: Boolean; cdecl;
  function getResultCode: Integer; cdecl;
  function getResultData: JString; cdecl;
  function getResultExtras(makeMap: Boolean): JBundle; cdecl;
  procedure setResult(code: Integer; data: JString; extras: JBundle); cdecl;
  procedure setResultCode(code: Integer); cdecl;
  procedure setResultData(data: JString); cdecl;
  procedure setResultExtras(extras: JBundle); cdecl;
end;
TJBroadcastReceiver_PendingResult = class(TJavaGenericImport<JBroadcastReceiver_PendingResultClass, JBroadcastReceiver_PendingResult>) end;

JGestureDetector_SimpleOnGestureListenerClass = interface(JObjectClass)
['{1E226D50-6D5B-42A1-8178-20CF72CDE156}']
  {Methods}
  function init: JGestureDetector_SimpleOnGestureListener; cdecl;
end;

[JavaSignature('android/view/GestureDetector$SimpleOnGestureListener')]
JGestureDetector_SimpleOnGestureListener = interface(JObject)
['{0C4A6AE9-8E3A-4325-9393-2A71A6AFEC8D}']
  {Methods}
  function onDoubleTap(e: JMotionEvent): Boolean; cdecl;
  function onDoubleTapEvent(e: JMotionEvent): Boolean; cdecl;
  function onDown(e: JMotionEvent): Boolean; cdecl;
  function onFling(e1: JMotionEvent; e2: JMotionEvent; velocityX: Single; velocityY: Single): Boolean; cdecl;
  procedure onLongPress(e: JMotionEvent); cdecl;
  function onScroll(e1: JMotionEvent; e2: JMotionEvent; distanceX: Single; distanceY: Single): Boolean; cdecl;
  procedure onShowPress(e: JMotionEvent); cdecl;
  function onSingleTapConfirmed(e: JMotionEvent): Boolean; cdecl;
  function onSingleTapUp(e: JMotionEvent): Boolean; cdecl;
end;
TJGestureDetector_SimpleOnGestureListener = class(TJavaGenericImport<JGestureDetector_SimpleOnGestureListenerClass, JGestureDetector_SimpleOnGestureListener>) end;

JInputQueueClass = interface(JObjectClass)
['{D864DFE2-C91F-469A-954F-01F887676348}']
end;

[JavaSignature('android/view/InputQueue')]
JInputQueue = interface(JObject)
['{514AF3EE-764E-4F6F-8774-5D0DB3F9F761}']
end;
TJInputQueue = class(TJavaGenericImport<JInputQueueClass, JInputQueue>) end;

JSQLiteQueryClass = interface(JSQLiteProgramClass)
['{54EA3C25-1BC0-4806-8627-66E2B2FACDBE}']
end;

[JavaSignature('android/database/sqlite/SQLiteQuery')]
JSQLiteQuery = interface(JSQLiteProgram)
['{0ACE7FC4-DD34-43FE-AF15-498D050DD215}']
  {Methods}
  function toString: JString; cdecl;
end;
TJSQLiteQuery = class(TJavaGenericImport<JSQLiteQueryClass, JSQLiteQuery>) end;

JDrawableClass = interface(JObjectClass)
['{65629D6B-EE84-4012-8A99-80CA18343BE7}']
  {Methods}
  function init: JDrawable; cdecl;
  function createFromPath(pathName: JString): JDrawable; cdecl;
  function createFromResourceStream(res: JResources; value: JTypedValue; is_: JInputStream; srcName: JString): JDrawable; cdecl; overload;
  function createFromResourceStream(res: JResources; value: JTypedValue; is_: JInputStream; srcName: JString; opts: JBitmapFactory_Options): JDrawable; cdecl; overload;
  function createFromStream(is_: JInputStream; srcName: JString): JDrawable; cdecl;
  function resolveOpacity(op1: Integer; op2: Integer): Integer; cdecl;
end;

[JavaSignature('android/graphics/drawable/Drawable')]
JDrawable = interface(JObject)
['{063DF683-9F17-4BA2-96C8-B98246BDB114}']
  {Methods}
  procedure clearColorFilter; cdecl;
  procedure copyBounds(bounds: JRect); cdecl; overload;
  function copyBounds: JRect; cdecl; overload;
  procedure draw(canvas: JCanvas); cdecl;
  function getBounds: JRect; cdecl;
  function getCallback: JDrawable_Callback; cdecl;
  function getChangingConfigurations: Integer; cdecl;
  function getConstantState: JDrawable_ConstantState; cdecl;
  function getCurrent: JDrawable; cdecl;
  function getIntrinsicHeight: Integer; cdecl;
  function getIntrinsicWidth: Integer; cdecl;
  function getLevel: Integer; cdecl;
  function getMinimumHeight: Integer; cdecl;
  function getMinimumWidth: Integer; cdecl;
  function getOpacity: Integer; cdecl;
  function getPadding(padding: JRect): Boolean; cdecl;
  function getState: TJavaArray<Integer>; cdecl;
  function getTransparentRegion: JRegion; cdecl;
  procedure invalidateSelf; cdecl;
  function isStateful: Boolean; cdecl;
  function isVisible: Boolean; cdecl;
  procedure jumpToCurrentState; cdecl;
  function mutate: JDrawable; cdecl;
  procedure scheduleSelf(what: JRunnable; when: Int64); cdecl;
  procedure setAlpha(alpha: Integer); cdecl;
  procedure setBounds(left: Integer; top: Integer; right: Integer; bottom: Integer); cdecl; overload;
  procedure setBounds(bounds: JRect); cdecl; overload;
  procedure setCallback(cb: JDrawable_Callback); cdecl;
  procedure setChangingConfigurations(configs: Integer); cdecl;
  procedure setColorFilter(cf: JColorFilter); cdecl; overload;
  procedure setColorFilter(color: Integer; mode: JPorterDuff_Mode); cdecl; overload;
  procedure setDither(dither: Boolean); cdecl;
  procedure setFilterBitmap(filter: Boolean); cdecl;
  function setLevel(level: Integer): Boolean; cdecl;
  function setState(stateSet: TJavaArray<Integer>): Boolean; cdecl;
  function setVisible(visible: Boolean; restart: Boolean): Boolean; cdecl;
  procedure unscheduleSelf(what: JRunnable); cdecl;
end;
TJDrawable = class(TJavaGenericImport<JDrawableClass, JDrawable>) end;

JClipDrawableClass = interface(JDrawableClass)
['{D574B655-BF43-42DC-BFB2-F0B97B32484A}']
  {Property Methods}
  function _GetHORIZONTAL: Integer;
  function _GetVERTICAL: Integer;
  {Methods}
  function init(drawable: JDrawable; gravity: Integer; orientation: Integer): JClipDrawable; cdecl;
  {Properties}
  property HORIZONTAL: Integer read _GetHORIZONTAL;
  property VERTICAL: Integer read _GetVERTICAL;
end;

[JavaSignature('android/graphics/drawable/ClipDrawable')]
JClipDrawable = interface(JDrawable)
['{2BDDA0C4-28C8-42DE-8B80-842AD3DEB5A8}']
  {Methods}
  procedure draw(canvas: JCanvas); cdecl;
  function getChangingConfigurations: Integer; cdecl;
  function getConstantState: JDrawable_ConstantState; cdecl;
  function getIntrinsicHeight: Integer; cdecl;
  function getIntrinsicWidth: Integer; cdecl;
  function getOpacity: Integer; cdecl;
  function getPadding(padding: JRect): Boolean; cdecl;
  procedure invalidateDrawable(who: JDrawable); cdecl;
  function isStateful: Boolean; cdecl;
  procedure scheduleDrawable(who: JDrawable; what: JRunnable; when: Int64); cdecl;
  procedure setAlpha(alpha: Integer); cdecl;
  procedure setColorFilter(cf: JColorFilter); cdecl;
  function setVisible(visible: Boolean; restart: Boolean): Boolean; cdecl;
  procedure unscheduleDrawable(who: JDrawable; what: JRunnable); cdecl;
end;
TJClipDrawable = class(TJavaGenericImport<JClipDrawableClass, JClipDrawable>) end;

JSurfaceTexture_OutOfResourcesExceptionClass = interface(JExceptionClass)
['{115BC5D2-1F30-4911-A0AD-F2F62EF96551}']
  {Methods}
  function init: JSurfaceTexture_OutOfResourcesException; cdecl; overload;
  function init(name: JString): JSurfaceTexture_OutOfResourcesException; cdecl; overload;
end;

[JavaSignature('android/graphics/SurfaceTexture$OutOfResourcesException')]
JSurfaceTexture_OutOfResourcesException = interface(JException)
['{76C8E3F6-A92E-435E-A016-27C43EE8D73F}']
end;
TJSurfaceTexture_OutOfResourcesException = class(TJavaGenericImport<JSurfaceTexture_OutOfResourcesExceptionClass, JSurfaceTexture_OutOfResourcesException>) end;

JPackageItemInfoClass = interface(JObjectClass)
['{8E60F2CA-9F18-4D13-83A5-736AF9200D73}']
  {Methods}
  function init: JPackageItemInfo; cdecl; overload;
  function init(orig: JPackageItemInfo): JPackageItemInfo; cdecl; overload;
end;

[JavaSignature('android/content/pm/PackageItemInfo')]
JPackageItemInfo = interface(JObject)
['{FB22C7C1-E6FE-49AE-97E9-6CBDC1424263}']
  {Property Methods}
  function _Geticon: Integer;
  procedure _Seticon(Value: Integer);
  function _GetlabelRes: Integer;
  procedure _SetlabelRes(Value: Integer);
  function _Getlogo: Integer;
  procedure _Setlogo(Value: Integer);
  function _GetmetaData: JBundle;
  procedure _SetmetaData(Value: JBundle);
  function _Getname: JString;
  procedure _Setname(Value: JString);
  function _GetnonLocalizedLabel: JCharSequence;
  procedure _SetnonLocalizedLabel(Value: JCharSequence);
  function _GetpackageName: JString;
  procedure _SetpackageName(Value: JString);
  {Methods}
  function loadIcon(pm: JPackageManager): JDrawable; cdecl;
  function loadLabel(pm: JPackageManager): JCharSequence; cdecl;
  function loadLogo(pm: JPackageManager): JDrawable; cdecl;
  function loadXmlMetaData(pm: JPackageManager; name: JString): JXmlResourceParser; cdecl;
  procedure writeToParcel(dest: JParcel; parcelableFlags: Integer); cdecl;
  {Properties}
  property icon: Integer read _Geticon write _Seticon;
  property labelRes: Integer read _GetlabelRes write _SetlabelRes;
  property logo: Integer read _Getlogo write _Setlogo;
  property metaData: JBundle read _GetmetaData write _SetmetaData;
  property name: JString read _Getname write _Setname;
  property nonLocalizedLabel: JCharSequence read _GetnonLocalizedLabel write _SetnonLocalizedLabel;
  property packageName: JString read _GetpackageName write _SetpackageName;
end;
TJPackageItemInfo = class(TJavaGenericImport<JPackageItemInfoClass, JPackageItemInfo>) end;

JComponentInfoClass = interface(JPackageItemInfoClass)
['{637D9B9F-9FEA-410E-B18F-01C9FFB10252}']
  {Methods}
  function init: JComponentInfo; cdecl; overload;
  function init(orig: JComponentInfo): JComponentInfo; cdecl; overload;
end;

[JavaSignature('android/content/pm/ComponentInfo')]
JComponentInfo = interface(JPackageItemInfo)
['{464FA916-A0D4-4C20-BD6F-DB7D20F36E18}']
  {Property Methods}
  function _GetapplicationInfo: JApplicationInfo;
  procedure _SetapplicationInfo(Value: JApplicationInfo);
  function _GetdescriptionRes: Integer;
  procedure _SetdescriptionRes(Value: Integer);
  function _Getenabled: Boolean;
  procedure _Setenabled(Value: Boolean);
  function _Getexported: Boolean;
  procedure _Setexported(Value: Boolean);
  function _GetprocessName: JString;
  procedure _SetprocessName(Value: JString);
  {Methods}
  function getIconResource: Integer; cdecl;
  function isEnabled: Boolean; cdecl;
  function loadLabel(pm: JPackageManager): JCharSequence; cdecl;
  procedure writeToParcel(dest: JParcel; parcelableFlags: Integer); cdecl;
  {Properties}
  property applicationInfo: JApplicationInfo read _GetapplicationInfo write _SetapplicationInfo;
  property descriptionRes: Integer read _GetdescriptionRes write _SetdescriptionRes;
  property enabled: Boolean read _Getenabled write _Setenabled;
  property exported: Boolean read _Getexported write _Setexported;
  property processName: JString read _GetprocessName write _SetprocessName;
end;
TJComponentInfo = class(TJavaGenericImport<JComponentInfoClass, JComponentInfo>) end;

JProviderInfoClass = interface(JComponentInfoClass)
['{65211587-41C0-4B0A-AA07-1BF3120B490A}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  function _GetFLAG_SINGLE_USER: Integer;
  {Methods}
  function init: JProviderInfo; cdecl; overload;
  function init(orig: JProviderInfo): JProviderInfo; cdecl; overload;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
  property FLAG_SINGLE_USER: Integer read _GetFLAG_SINGLE_USER;
end;

[JavaSignature('android/content/pm/ProviderInfo')]
JProviderInfo = interface(JComponentInfo)
['{D001C0E5-CD94-4529-ABEF-DE6A6D4CD232}']
  {Property Methods}
  function _Getauthority: JString;
  procedure _Setauthority(Value: JString);
  function _Getflags: Integer;
  procedure _Setflags(Value: Integer);
  function _GetgrantUriPermissions: Boolean;
  procedure _SetgrantUriPermissions(Value: Boolean);
  function _GetinitOrder: Integer;
  procedure _SetinitOrder(Value: Integer);
  function _GetisSyncable: Boolean;
  procedure _SetisSyncable(Value: Boolean);
  function _Getmultiprocess: Boolean;
  procedure _Setmultiprocess(Value: Boolean);
  function _GetpathPermissions: TJavaObjectArray<JPathPermission>;
  procedure _SetpathPermissions(Value: TJavaObjectArray<JPathPermission>);
  function _GetreadPermission: JString;
  procedure _SetreadPermission(Value: JString);
  function _GeturiPermissionPatterns: TJavaObjectArray<JPatternMatcher>;
  procedure _SeturiPermissionPatterns(Value: TJavaObjectArray<JPatternMatcher>);
  function _GetwritePermission: JString;
  procedure _SetwritePermission(Value: JString);
  {Methods}
  function describeContents: Integer; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(out_: JParcel; parcelableFlags: Integer); cdecl;
  {Properties}
  property authority: JString read _Getauthority write _Setauthority;
  property flags: Integer read _Getflags write _Setflags;
  property grantUriPermissions: Boolean read _GetgrantUriPermissions write _SetgrantUriPermissions;
  property initOrder: Integer read _GetinitOrder write _SetinitOrder;
  property isSyncable: Boolean read _GetisSyncable write _SetisSyncable;
  property multiprocess: Boolean read _Getmultiprocess write _Setmultiprocess;
  property pathPermissions: TJavaObjectArray<JPathPermission> read _GetpathPermissions write _SetpathPermissions;
  property readPermission: JString read _GetreadPermission write _SetreadPermission;
  property uriPermissionPatterns: TJavaObjectArray<JPatternMatcher> read _GeturiPermissionPatterns write _SeturiPermissionPatterns;
  property writePermission: JString read _GetwritePermission write _SetwritePermission;
end;
TJProviderInfo = class(TJavaGenericImport<JProviderInfoClass, JProviderInfo>) end;

JViewClass = interface(JObjectClass)
['{FB6C1483-BDE7-423F-97F9-76A8D785EF36}']
  {Property Methods}
  function _GetALPHA: JProperty;
  function _GetDRAWING_CACHE_QUALITY_AUTO: Integer;
  function _GetDRAWING_CACHE_QUALITY_HIGH: Integer;
  function _GetDRAWING_CACHE_QUALITY_LOW: Integer;
  function _GetFIND_VIEWS_WITH_CONTENT_DESCRIPTION: Integer;
  function _GetFIND_VIEWS_WITH_TEXT: Integer;
  function _GetFOCUSABLES_ALL: Integer;
  function _GetFOCUSABLES_TOUCH_MODE: Integer;
  function _GetFOCUS_BACKWARD: Integer;
  function _GetFOCUS_DOWN: Integer;
  function _GetFOCUS_FORWARD: Integer;
  function _GetFOCUS_LEFT: Integer;
  function _GetFOCUS_RIGHT: Integer;
  function _GetFOCUS_UP: Integer;
  function _GetGONE: Integer;
  function _GetHAPTIC_FEEDBACK_ENABLED: Integer;
  function _GetIMPORTANT_FOR_ACCESSIBILITY_AUTO: Integer;
  function _GetIMPORTANT_FOR_ACCESSIBILITY_NO: Integer;
  function _GetIMPORTANT_FOR_ACCESSIBILITY_YES: Integer;
  function _GetINVISIBLE: Integer;
  function _GetKEEP_SCREEN_ON: Integer;
  function _GetLAYER_TYPE_HARDWARE: Integer;
  function _GetLAYER_TYPE_NONE: Integer;
  function _GetLAYER_TYPE_SOFTWARE: Integer;
  function _GetLAYOUT_DIRECTION_INHERIT: Integer;
  function _GetLAYOUT_DIRECTION_LOCALE: Integer;
  function _GetLAYOUT_DIRECTION_LTR: Integer;
  function _GetLAYOUT_DIRECTION_RTL: Integer;
  function _GetMEASURED_HEIGHT_STATE_SHIFT: Integer;
  function _GetMEASURED_SIZE_MASK: Integer;
  function _GetMEASURED_STATE_MASK: Integer;
  function _GetMEASURED_STATE_TOO_SMALL: Integer;
  function _GetNO_ID: Integer;
  function _GetOVER_SCROLL_ALWAYS: Integer;
  function _GetOVER_SCROLL_IF_CONTENT_SCROLLS: Integer;
  function _GetOVER_SCROLL_NEVER: Integer;
  function _GetROTATION: JProperty;
  function _GetROTATION_X: JProperty;
  function _GetROTATION_Y: JProperty;
  function _GetSCALE_X: JProperty;
  function _GetSCALE_Y: JProperty;
  function _GetSCREEN_STATE_OFF: Integer;
  function _GetSCREEN_STATE_ON: Integer;
  function _GetSCROLLBARS_INSIDE_INSET: Integer;
  function _GetSCROLLBARS_INSIDE_OVERLAY: Integer;
  function _GetSCROLLBARS_OUTSIDE_INSET: Integer;
  function _GetSCROLLBARS_OUTSIDE_OVERLAY: Integer;
  function _GetSCROLLBAR_POSITION_DEFAULT: Integer;
  function _GetSCROLLBAR_POSITION_LEFT: Integer;
  function _GetSCROLLBAR_POSITION_RIGHT: Integer;
  function _GetSOUND_EFFECTS_ENABLED: Integer;
  function _GetSTATUS_BAR_HIDDEN: Integer;
  function _GetSTATUS_BAR_VISIBLE: Integer;
  function _GetSYSTEM_UI_FLAG_FULLSCREEN: Integer;
  function _GetSYSTEM_UI_FLAG_HIDE_NAVIGATION: Integer;
  function _GetSYSTEM_UI_FLAG_LAYOUT_FULLSCREEN: Integer;
  function _GetSYSTEM_UI_FLAG_LAYOUT_HIDE_NAVIGATION: Integer;
  function _GetSYSTEM_UI_FLAG_LAYOUT_STABLE: Integer;
  function _GetSYSTEM_UI_FLAG_LOW_PROFILE: Integer;
  function _GetSYSTEM_UI_FLAG_VISIBLE: Integer;
  function _GetSYSTEM_UI_LAYOUT_FLAGS: Integer;
  function _GetTEXT_ALIGNMENT_CENTER: Integer;
  function _GetTEXT_ALIGNMENT_GRAVITY: Integer;
  function _GetTEXT_ALIGNMENT_INHERIT: Integer;
  function _GetTEXT_ALIGNMENT_TEXT_END: Integer;
  function _GetTEXT_ALIGNMENT_TEXT_START: Integer;
  function _GetTEXT_ALIGNMENT_VIEW_END: Integer;
  function _GetTEXT_ALIGNMENT_VIEW_START: Integer;
  function _GetTEXT_DIRECTION_ANY_RTL: Integer;
  function _GetTEXT_DIRECTION_FIRST_STRONG: Integer;
  function _GetTEXT_DIRECTION_INHERIT: Integer;
  function _GetTEXT_DIRECTION_LOCALE: Integer;
  function _GetTEXT_DIRECTION_LTR: Integer;
  function _GetTEXT_DIRECTION_RTL: Integer;
  function _GetTRANSLATION_X: JProperty;
  function _GetTRANSLATION_Y: JProperty;
  function _GetVISIBLE: Integer;
  function _GetX: JProperty;
  function _GetY: JProperty;
  {Methods}
  function init(context: JContext): JView; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet): JView; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet; defStyle: Integer): JView; cdecl; overload;
  function combineMeasuredStates(curState: Integer; newState: Integer): Integer; cdecl;
  function generateViewId: Integer; cdecl;
  function getDefaultSize(size: Integer; measureSpec: Integer): Integer; cdecl;
  function inflate(context: JContext; resource: Integer; root: JViewGroup): JView; cdecl;
  function resolveSize(size: Integer; measureSpec: Integer): Integer; cdecl;
  function resolveSizeAndState(size: Integer; measureSpec: Integer; childMeasuredState: Integer): Integer; cdecl;
  {Properties}
  property ALPHA: JProperty read _GetALPHA;
  property DRAWING_CACHE_QUALITY_AUTO: Integer read _GetDRAWING_CACHE_QUALITY_AUTO;
  property DRAWING_CACHE_QUALITY_HIGH: Integer read _GetDRAWING_CACHE_QUALITY_HIGH;
  property DRAWING_CACHE_QUALITY_LOW: Integer read _GetDRAWING_CACHE_QUALITY_LOW;
  property FIND_VIEWS_WITH_CONTENT_DESCRIPTION: Integer read _GetFIND_VIEWS_WITH_CONTENT_DESCRIPTION;
  property FIND_VIEWS_WITH_TEXT: Integer read _GetFIND_VIEWS_WITH_TEXT;
  property FOCUSABLES_ALL: Integer read _GetFOCUSABLES_ALL;
  property FOCUSABLES_TOUCH_MODE: Integer read _GetFOCUSABLES_TOUCH_MODE;
  property FOCUS_BACKWARD: Integer read _GetFOCUS_BACKWARD;
  property FOCUS_DOWN: Integer read _GetFOCUS_DOWN;
  property FOCUS_FORWARD: Integer read _GetFOCUS_FORWARD;
  property FOCUS_LEFT: Integer read _GetFOCUS_LEFT;
  property FOCUS_RIGHT: Integer read _GetFOCUS_RIGHT;
  property FOCUS_UP: Integer read _GetFOCUS_UP;
  property GONE: Integer read _GetGONE;
  property HAPTIC_FEEDBACK_ENABLED: Integer read _GetHAPTIC_FEEDBACK_ENABLED;
  property IMPORTANT_FOR_ACCESSIBILITY_AUTO: Integer read _GetIMPORTANT_FOR_ACCESSIBILITY_AUTO;
  property IMPORTANT_FOR_ACCESSIBILITY_NO: Integer read _GetIMPORTANT_FOR_ACCESSIBILITY_NO;
  property IMPORTANT_FOR_ACCESSIBILITY_YES: Integer read _GetIMPORTANT_FOR_ACCESSIBILITY_YES;
  property INVISIBLE: Integer read _GetINVISIBLE;
  property KEEP_SCREEN_ON: Integer read _GetKEEP_SCREEN_ON;
  property LAYER_TYPE_HARDWARE: Integer read _GetLAYER_TYPE_HARDWARE;
  property LAYER_TYPE_NONE: Integer read _GetLAYER_TYPE_NONE;
  property LAYER_TYPE_SOFTWARE: Integer read _GetLAYER_TYPE_SOFTWARE;
  property LAYOUT_DIRECTION_INHERIT: Integer read _GetLAYOUT_DIRECTION_INHERIT;
  property LAYOUT_DIRECTION_LOCALE: Integer read _GetLAYOUT_DIRECTION_LOCALE;
  property LAYOUT_DIRECTION_LTR: Integer read _GetLAYOUT_DIRECTION_LTR;
  property LAYOUT_DIRECTION_RTL: Integer read _GetLAYOUT_DIRECTION_RTL;
  property MEASURED_HEIGHT_STATE_SHIFT: Integer read _GetMEASURED_HEIGHT_STATE_SHIFT;
  property MEASURED_SIZE_MASK: Integer read _GetMEASURED_SIZE_MASK;
  property MEASURED_STATE_MASK: Integer read _GetMEASURED_STATE_MASK;
  property MEASURED_STATE_TOO_SMALL: Integer read _GetMEASURED_STATE_TOO_SMALL;
  property NO_ID: Integer read _GetNO_ID;
  property OVER_SCROLL_ALWAYS: Integer read _GetOVER_SCROLL_ALWAYS;
  property OVER_SCROLL_IF_CONTENT_SCROLLS: Integer read _GetOVER_SCROLL_IF_CONTENT_SCROLLS;
  property OVER_SCROLL_NEVER: Integer read _GetOVER_SCROLL_NEVER;
  property ROTATION: JProperty read _GetROTATION;
  property ROTATION_X: JProperty read _GetROTATION_X;
  property ROTATION_Y: JProperty read _GetROTATION_Y;
  property SCALE_X: JProperty read _GetSCALE_X;
  property SCALE_Y: JProperty read _GetSCALE_Y;
  property SCREEN_STATE_OFF: Integer read _GetSCREEN_STATE_OFF;
  property SCREEN_STATE_ON: Integer read _GetSCREEN_STATE_ON;
  property SCROLLBARS_INSIDE_INSET: Integer read _GetSCROLLBARS_INSIDE_INSET;
  property SCROLLBARS_INSIDE_OVERLAY: Integer read _GetSCROLLBARS_INSIDE_OVERLAY;
  property SCROLLBARS_OUTSIDE_INSET: Integer read _GetSCROLLBARS_OUTSIDE_INSET;
  property SCROLLBARS_OUTSIDE_OVERLAY: Integer read _GetSCROLLBARS_OUTSIDE_OVERLAY;
  property SCROLLBAR_POSITION_DEFAULT: Integer read _GetSCROLLBAR_POSITION_DEFAULT;
  property SCROLLBAR_POSITION_LEFT: Integer read _GetSCROLLBAR_POSITION_LEFT;
  property SCROLLBAR_POSITION_RIGHT: Integer read _GetSCROLLBAR_POSITION_RIGHT;
  property SOUND_EFFECTS_ENABLED: Integer read _GetSOUND_EFFECTS_ENABLED;
  property STATUS_BAR_HIDDEN: Integer read _GetSTATUS_BAR_HIDDEN;
  property STATUS_BAR_VISIBLE: Integer read _GetSTATUS_BAR_VISIBLE;
  property SYSTEM_UI_FLAG_FULLSCREEN: Integer read _GetSYSTEM_UI_FLAG_FULLSCREEN;
  property SYSTEM_UI_FLAG_HIDE_NAVIGATION: Integer read _GetSYSTEM_UI_FLAG_HIDE_NAVIGATION;
  property SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN: Integer read _GetSYSTEM_UI_FLAG_LAYOUT_FULLSCREEN;
  property SYSTEM_UI_FLAG_LAYOUT_HIDE_NAVIGATION: Integer read _GetSYSTEM_UI_FLAG_LAYOUT_HIDE_NAVIGATION;
  property SYSTEM_UI_FLAG_LAYOUT_STABLE: Integer read _GetSYSTEM_UI_FLAG_LAYOUT_STABLE;
  property SYSTEM_UI_FLAG_LOW_PROFILE: Integer read _GetSYSTEM_UI_FLAG_LOW_PROFILE;
  property SYSTEM_UI_FLAG_VISIBLE: Integer read _GetSYSTEM_UI_FLAG_VISIBLE;
  property SYSTEM_UI_LAYOUT_FLAGS: Integer read _GetSYSTEM_UI_LAYOUT_FLAGS;
  property TEXT_ALIGNMENT_CENTER: Integer read _GetTEXT_ALIGNMENT_CENTER;
  property TEXT_ALIGNMENT_GRAVITY: Integer read _GetTEXT_ALIGNMENT_GRAVITY;
  property TEXT_ALIGNMENT_INHERIT: Integer read _GetTEXT_ALIGNMENT_INHERIT;
  property TEXT_ALIGNMENT_TEXT_END: Integer read _GetTEXT_ALIGNMENT_TEXT_END;
  property TEXT_ALIGNMENT_TEXT_START: Integer read _GetTEXT_ALIGNMENT_TEXT_START;
  property TEXT_ALIGNMENT_VIEW_END: Integer read _GetTEXT_ALIGNMENT_VIEW_END;
  property TEXT_ALIGNMENT_VIEW_START: Integer read _GetTEXT_ALIGNMENT_VIEW_START;
  property TEXT_DIRECTION_ANY_RTL: Integer read _GetTEXT_DIRECTION_ANY_RTL;
  property TEXT_DIRECTION_FIRST_STRONG: Integer read _GetTEXT_DIRECTION_FIRST_STRONG;
  property TEXT_DIRECTION_INHERIT: Integer read _GetTEXT_DIRECTION_INHERIT;
  property TEXT_DIRECTION_LOCALE: Integer read _GetTEXT_DIRECTION_LOCALE;
  property TEXT_DIRECTION_LTR: Integer read _GetTEXT_DIRECTION_LTR;
  property TEXT_DIRECTION_RTL: Integer read _GetTEXT_DIRECTION_RTL;
  property TRANSLATION_X: JProperty read _GetTRANSLATION_X;
  property TRANSLATION_Y: JProperty read _GetTRANSLATION_Y;
  property VISIBLE: Integer read _GetVISIBLE;
  property X: JProperty read _GetX;
  property Y: JProperty read _GetY;
end;

[JavaSignature('android/view/View')]
JView = interface(JObject)
['{DC3C5B70-7559-4AFB-9FAC-0DBAE311F8BF}']
  {Methods}
  procedure addChildrenForAccessibility(children: JArrayList); cdecl;
  procedure addFocusables(views: JArrayList; direction: Integer); cdecl; overload;
  procedure addFocusables(views: JArrayList; direction: Integer; focusableMode: Integer); cdecl; overload;
  procedure addOnAttachStateChangeListener(listener: JView_OnAttachStateChangeListener); cdecl;
  procedure addOnLayoutChangeListener(listener: JView_OnLayoutChangeListener); cdecl;
  procedure addTouchables(views: JArrayList); cdecl;
  function animate: JViewPropertyAnimator; cdecl;
  procedure announceForAccessibility(text: JCharSequence); cdecl;
  procedure bringToFront; cdecl;
  procedure buildDrawingCache; cdecl; overload;
  procedure buildDrawingCache(autoScale: Boolean); cdecl; overload;
  procedure buildLayer; cdecl;
  function callOnClick: Boolean; cdecl;
  function canScrollHorizontally(direction: Integer): Boolean; cdecl;
  function canScrollVertically(direction: Integer): Boolean; cdecl;
  procedure cancelLongPress; cdecl;
  function checkInputConnectionProxy(view: JView): Boolean; cdecl;
  procedure clearAnimation; cdecl;
  procedure clearFocus; cdecl;
  procedure computeScroll; cdecl;
  procedure createContextMenu(menu: JContextMenu); cdecl;
  procedure destroyDrawingCache; cdecl;
  procedure dispatchConfigurationChanged(newConfig: JConfiguration); cdecl;
  procedure dispatchDisplayHint(hint: Integer); cdecl;
  function dispatchDragEvent(event: JDragEvent): Boolean; cdecl;
  function dispatchGenericMotionEvent(event: JMotionEvent): Boolean; cdecl;
  function dispatchKeyEvent(event: JKeyEvent): Boolean; cdecl;
  function dispatchKeyEventPreIme(event: JKeyEvent): Boolean; cdecl;
  function dispatchKeyShortcutEvent(event: JKeyEvent): Boolean; cdecl;
  procedure dispatchSystemUiVisibilityChanged(visibility: Integer); cdecl;
  function dispatchTouchEvent(event: JMotionEvent): Boolean; cdecl;
  function dispatchTrackballEvent(event: JMotionEvent): Boolean; cdecl;
  function dispatchUnhandledMove(focused: JView; direction: Integer): Boolean; cdecl;
  procedure dispatchWindowFocusChanged(hasFocus: Boolean); cdecl;
  procedure dispatchWindowSystemUiVisiblityChanged(visible: Integer); cdecl;
  procedure dispatchWindowVisibilityChanged(visibility: Integer); cdecl;
  procedure draw(canvas: JCanvas); cdecl;
  function findFocus: JView; cdecl;
  function findViewById(id: Integer): JView; cdecl;
  function findViewWithTag(tag: JObject): JView; cdecl;
  procedure findViewsWithText(outViews: JArrayList; searched: JCharSequence; flags: Integer); cdecl;
  function focusSearch(direction: Integer): JView; cdecl;
  procedure forceLayout; cdecl;
  function getAlpha: Single; cdecl;
  function getApplicationWindowToken: JIBinder; cdecl;
  function getBackground: JDrawable; cdecl;
  function getBaseline: Integer; cdecl;
  function getBottom: Integer; cdecl;
  function getCameraDistance: Single; cdecl;
  function getContentDescription: JCharSequence; cdecl;
  function getContext: JContext; cdecl;
  function getDisplay: JDisplay; cdecl;
  function getDrawableState: TJavaArray<Integer>; cdecl;
  function getDrawingCache: JBitmap; cdecl; overload;
  function getDrawingCache(autoScale: Boolean): JBitmap; cdecl; overload;
  function getDrawingCacheBackgroundColor: Integer; cdecl;
  function getDrawingCacheQuality: Integer; cdecl;
  procedure getDrawingRect(outRect: JRect); cdecl;
  function getDrawingTime: Int64; cdecl;
  function getFilterTouchesWhenObscured: Boolean; cdecl;
  function getFitsSystemWindows: Boolean; cdecl;
  function getFocusables(direction: Integer): JArrayList; cdecl;
  procedure getFocusedRect(r: JRect); cdecl;
  function getGlobalVisibleRect(r: JRect; globalOffset: JPoint): Boolean; cdecl; overload;
  function getGlobalVisibleRect(r: JRect): Boolean; cdecl; overload;
  function getHandler: JHandler; cdecl;
  function getHeight: Integer; cdecl;
  procedure getHitRect(outRect: JRect); cdecl;
  function getHorizontalFadingEdgeLength: Integer; cdecl;
  function getId: Integer; cdecl;
  function getImportantForAccessibility: Integer; cdecl;
  function getKeepScreenOn: Boolean; cdecl;
  function getKeyDispatcherState: JKeyEvent_DispatcherState; cdecl;
  function getLabelFor: Integer; cdecl;
  function getLayerType: Integer; cdecl;
  function getLayoutDirection: Integer; cdecl;
  function getLayoutParams: JViewGroup_LayoutParams; cdecl;
  function getLeft: Integer; cdecl;
  function getLocalVisibleRect(r: JRect): Boolean; cdecl;
  procedure getLocationInWindow(location: TJavaArray<Integer>); cdecl;
  procedure getLocationOnScreen(location: TJavaArray<Integer>); cdecl;
  function getMatrix: JMatrix; cdecl;
  function getMeasuredHeight: Integer; cdecl;
  function getMeasuredHeightAndState: Integer; cdecl;
  function getMeasuredState: Integer; cdecl;
  function getMeasuredWidth: Integer; cdecl;
  function getMeasuredWidthAndState: Integer; cdecl;
  function getMinimumHeight: Integer; cdecl;
  function getMinimumWidth: Integer; cdecl;
  function getNextFocusDownId: Integer; cdecl;
  function getNextFocusForwardId: Integer; cdecl;
  function getNextFocusLeftId: Integer; cdecl;
  function getNextFocusRightId: Integer; cdecl;
  function getNextFocusUpId: Integer; cdecl;
  function getOnFocusChangeListener: JView_OnFocusChangeListener; cdecl;
  function getOverScrollMode: Integer; cdecl;
  function getPaddingBottom: Integer; cdecl;
  function getPaddingEnd: Integer; cdecl;
  function getPaddingLeft: Integer; cdecl;
  function getPaddingRight: Integer; cdecl;
  function getPaddingStart: Integer; cdecl;
  function getPaddingTop: Integer; cdecl;
  function getParent: JViewParent; cdecl;
  function getParentForAccessibility: JViewParent; cdecl;
  function getPivotX: Single; cdecl;
  function getPivotY: Single; cdecl;
  function getResources: JResources; cdecl;
  function getRight: Integer; cdecl;
  function getRootView: JView; cdecl;
  function getRotation: Single; cdecl;
  function getRotationX: Single; cdecl;
  function getRotationY: Single; cdecl;
  function getScaleX: Single; cdecl;
  function getScaleY: Single; cdecl;
  function getScrollBarDefaultDelayBeforeFade: Integer; cdecl;
  function getScrollBarFadeDuration: Integer; cdecl;
  function getScrollBarSize: Integer; cdecl;
  function getScrollBarStyle: Integer; cdecl;
  function getScrollX: Integer; cdecl;
  function getScrollY: Integer; cdecl;
  function getSolidColor: Integer; cdecl;
  function getSystemUiVisibility: Integer; cdecl;
  function getTag: JObject; cdecl; overload;
  function getTag(key: Integer): JObject; cdecl; overload;
  function getTextAlignment: Integer; cdecl;
  function getTextDirection: Integer; cdecl;
  function getTop: Integer; cdecl;
  function getTouchDelegate: JTouchDelegate; cdecl;
  function getTouchables: JArrayList; cdecl;
  function getTranslationX: Single; cdecl;
  function getTranslationY: Single; cdecl;
  function getVerticalFadingEdgeLength: Integer; cdecl;
  function getVerticalScrollbarPosition: Integer; cdecl;
  function getVerticalScrollbarWidth: Integer; cdecl;
  function getViewTreeObserver: JViewTreeObserver; cdecl;
  function getVisibility: Integer; cdecl;
  function getWidth: Integer; cdecl;
  function getWindowSystemUiVisibility: Integer; cdecl;
  function getWindowToken: JIBinder; cdecl;
  function getWindowVisibility: Integer; cdecl;
  procedure getWindowVisibleDisplayFrame(outRect: JRect); cdecl;
  function getX: Single; cdecl;
  function getY: Single; cdecl;
  function hasFocus: Boolean; cdecl;
  function hasFocusable: Boolean; cdecl;
  function hasOnClickListeners: Boolean; cdecl;
  function hasOverlappingRendering: Boolean; cdecl;
  function hasTransientState: Boolean; cdecl;
  function hasWindowFocus: Boolean; cdecl;
  procedure invalidate(dirty: JRect); cdecl; overload;
  procedure invalidate(l: Integer; t: Integer; r: Integer; b: Integer); cdecl; overload;
  procedure invalidate; cdecl; overload;
  procedure invalidateDrawable(drawable: JDrawable); cdecl;
  function isActivated: Boolean; cdecl;
  function isClickable: Boolean; cdecl;
  function isDirty: Boolean; cdecl;
  function isDrawingCacheEnabled: Boolean; cdecl;
  function isDuplicateParentStateEnabled: Boolean; cdecl;
  function isEnabled: Boolean; cdecl;
  function isFocusable: Boolean; cdecl;
  function isFocusableInTouchMode: Boolean; cdecl;
  function isFocused: Boolean; cdecl;
  function isHapticFeedbackEnabled: Boolean; cdecl;
  function isHardwareAccelerated: Boolean; cdecl;
  function isHorizontalFadingEdgeEnabled: Boolean; cdecl;
  function isHorizontalScrollBarEnabled: Boolean; cdecl;
  function isHovered: Boolean; cdecl;
  function isInEditMode: Boolean; cdecl;
  function isInTouchMode: Boolean; cdecl;
  function isLayoutRequested: Boolean; cdecl;
  function isLongClickable: Boolean; cdecl;
  function isOpaque: Boolean; cdecl;
  function isPaddingRelative: Boolean; cdecl;
  function isPressed: Boolean; cdecl;
  function isSaveEnabled: Boolean; cdecl;
  function isSaveFromParentEnabled: Boolean; cdecl;
  function isScrollContainer: Boolean; cdecl;
  function isScrollbarFadingEnabled: Boolean; cdecl;
  function isSelected: Boolean; cdecl;
  function isShown: Boolean; cdecl;
  function isSoundEffectsEnabled: Boolean; cdecl;
  function isVerticalFadingEdgeEnabled: Boolean; cdecl;
  function isVerticalScrollBarEnabled: Boolean; cdecl;
  procedure jumpDrawablesToCurrentState; cdecl;
  procedure layout(l: Integer; t: Integer; r: Integer; b: Integer); cdecl;
  procedure measure(widthMeasureSpec: Integer; heightMeasureSpec: Integer); cdecl;
  procedure offsetLeftAndRight(offset: Integer); cdecl;
  procedure offsetTopAndBottom(offset: Integer); cdecl;
  function onCheckIsTextEditor: Boolean; cdecl;
  function onCreateInputConnection(outAttrs: JEditorInfo): JInputConnection; cdecl;
  function onDragEvent(event: JDragEvent): Boolean; cdecl;
  function onFilterTouchEventForSecurity(event: JMotionEvent): Boolean; cdecl;
  procedure onFinishTemporaryDetach; cdecl;
  function onGenericMotionEvent(event: JMotionEvent): Boolean; cdecl;
  procedure onHoverChanged(hovered: Boolean); cdecl;
  function onHoverEvent(event: JMotionEvent): Boolean; cdecl;
  function onKeyDown(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  function onKeyLongPress(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  function onKeyMultiple(keyCode: Integer; repeatCount: Integer; event: JKeyEvent): Boolean; cdecl;
  function onKeyPreIme(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  function onKeyShortcut(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  function onKeyUp(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  procedure onRtlPropertiesChanged(layoutDirection: Integer); cdecl;
  procedure onScreenStateChanged(screenState: Integer); cdecl;
  procedure onStartTemporaryDetach; cdecl;
  function onTouchEvent(event: JMotionEvent): Boolean; cdecl;
  function onTrackballEvent(event: JMotionEvent): Boolean; cdecl;
  procedure onWindowFocusChanged(hasWindowFocus: Boolean); cdecl;
  procedure onWindowSystemUiVisibilityChanged(visible: Integer); cdecl;
  function performAccessibilityAction(action: Integer; arguments: JBundle): Boolean; cdecl;
  function performClick: Boolean; cdecl;
  function performHapticFeedback(feedbackConstant: Integer): Boolean; cdecl; overload;
  function performHapticFeedback(feedbackConstant: Integer; flags: Integer): Boolean; cdecl; overload;
  function performLongClick: Boolean; cdecl;
  procedure playSoundEffect(soundConstant: Integer); cdecl;
  function post(action: JRunnable): Boolean; cdecl;
  function postDelayed(action: JRunnable; delayMillis: Int64): Boolean; cdecl;
  procedure postInvalidate; cdecl; overload;
  procedure postInvalidate(left: Integer; top: Integer; right: Integer; bottom: Integer); cdecl; overload;
  procedure postInvalidateDelayed(delayMilliseconds: Int64); cdecl; overload;
  procedure postInvalidateDelayed(delayMilliseconds: Int64; left: Integer; top: Integer; right: Integer; bottom: Integer); cdecl; overload;
  procedure postInvalidateOnAnimation; cdecl; overload;
  procedure postInvalidateOnAnimation(left: Integer; top: Integer; right: Integer; bottom: Integer); cdecl; overload;
  procedure postOnAnimation(action: JRunnable); cdecl;
  procedure postOnAnimationDelayed(action: JRunnable; delayMillis: Int64); cdecl;
  procedure refreshDrawableState; cdecl;
  function removeCallbacks(action: JRunnable): Boolean; cdecl;
  procedure removeOnAttachStateChangeListener(listener: JView_OnAttachStateChangeListener); cdecl;
  procedure removeOnLayoutChangeListener(listener: JView_OnLayoutChangeListener); cdecl;
  procedure requestFitSystemWindows; cdecl;
  function requestFocus: Boolean; cdecl; overload;
  function requestFocus(direction: Integer): Boolean; cdecl; overload;
  function requestFocus(direction: Integer; previouslyFocusedRect: JRect): Boolean; cdecl; overload;
  function requestFocusFromTouch: Boolean; cdecl;
  procedure requestLayout; cdecl;
  function requestRectangleOnScreen(rectangle: JRect): Boolean; cdecl; overload;
  function requestRectangleOnScreen(rectangle: JRect; immediate: Boolean): Boolean; cdecl; overload;
  procedure restoreHierarchyState(container: JSparseArray); cdecl;
  procedure saveHierarchyState(container: JSparseArray); cdecl;
  procedure scheduleDrawable(who: JDrawable; what: JRunnable; when: Int64); cdecl;
  procedure scrollBy(x: Integer; y: Integer); cdecl;
  procedure scrollTo(x: Integer; y: Integer); cdecl;
  procedure sendAccessibilityEvent(eventType: Integer); cdecl;
  procedure setAccessibilityDelegate(delegate: JView_AccessibilityDelegate); cdecl;
  procedure setActivated(activated: Boolean); cdecl;
  procedure setAlpha(alpha: Single); cdecl;
  procedure setBackground(background: JDrawable); cdecl;
  procedure setBackgroundColor(color: Integer); cdecl;
  procedure setBackgroundDrawable(background: JDrawable); cdecl;//Deprecated
  procedure setBackgroundResource(resid: Integer); cdecl;
  procedure setBottom(bottom: Integer); cdecl;
  procedure setCameraDistance(distance: Single); cdecl;
  procedure setClickable(clickable: Boolean); cdecl;
  procedure setContentDescription(contentDescription: JCharSequence); cdecl;
  procedure setDrawingCacheBackgroundColor(color: Integer); cdecl;
  procedure setDrawingCacheEnabled(enabled: Boolean); cdecl;
  procedure setDrawingCacheQuality(quality: Integer); cdecl;
  procedure setDuplicateParentStateEnabled(enabled: Boolean); cdecl;
  procedure setEnabled(enabled: Boolean); cdecl;
  procedure setFadingEdgeLength(length: Integer); cdecl;
  procedure setFilterTouchesWhenObscured(enabled: Boolean); cdecl;
  procedure setFitsSystemWindows(fitSystemWindows: Boolean); cdecl;
  procedure setFocusable(focusable: Boolean); cdecl;
  procedure setFocusableInTouchMode(focusableInTouchMode: Boolean); cdecl;
  procedure setHapticFeedbackEnabled(hapticFeedbackEnabled: Boolean); cdecl;
  procedure setHasTransientState(hasTransientState: Boolean); cdecl;
  procedure setHorizontalFadingEdgeEnabled(horizontalFadingEdgeEnabled: Boolean); cdecl;
  procedure setHorizontalScrollBarEnabled(horizontalScrollBarEnabled: Boolean); cdecl;
  procedure setHovered(hovered: Boolean); cdecl;
  procedure setId(id: Integer); cdecl;
  procedure setImportantForAccessibility(mode: Integer); cdecl;
  procedure setKeepScreenOn(keepScreenOn: Boolean); cdecl;
  procedure setLabelFor(id: Integer); cdecl;
  procedure setLayerPaint(paint: JPaint); cdecl;
  procedure setLayerType(layerType: Integer; paint: JPaint); cdecl;
  procedure setLayoutDirection(layoutDirection: Integer); cdecl;
  procedure setLayoutParams(params: JViewGroup_LayoutParams); cdecl;
  procedure setLeft(left: Integer); cdecl;
  procedure setLongClickable(longClickable: Boolean); cdecl;
  procedure setMinimumHeight(minHeight: Integer); cdecl;
  procedure setMinimumWidth(minWidth: Integer); cdecl;
  procedure setNextFocusDownId(nextFocusDownId: Integer); cdecl;
  procedure setNextFocusForwardId(nextFocusForwardId: Integer); cdecl;
  procedure setNextFocusLeftId(nextFocusLeftId: Integer); cdecl;
  procedure setNextFocusRightId(nextFocusRightId: Integer); cdecl;
  procedure setNextFocusUpId(nextFocusUpId: Integer); cdecl;
  procedure setOnClickListener(l: JView_OnClickListener); cdecl;
  procedure setOnCreateContextMenuListener(l: JView_OnCreateContextMenuListener); cdecl;
  procedure setOnDragListener(l: JView_OnDragListener); cdecl;
  procedure setOnFocusChangeListener(l: JView_OnFocusChangeListener); cdecl;
  procedure setOnGenericMotionListener(l: JView_OnGenericMotionListener); cdecl;
  procedure setOnHoverListener(l: JView_OnHoverListener); cdecl;
  procedure setOnKeyListener(l: JView_OnKeyListener); cdecl;
  procedure setOnLongClickListener(l: JView_OnLongClickListener); cdecl;
  procedure setOnSystemUiVisibilityChangeListener(l: JView_OnSystemUiVisibilityChangeListener); cdecl;
  procedure setOnTouchListener(l: JView_OnTouchListener); cdecl;
  procedure setOverScrollMode(overScrollMode: Integer); cdecl;
  procedure setPadding(left: Integer; top: Integer; right: Integer; bottom: Integer); cdecl;
  procedure setPaddingRelative(start: Integer; top: Integer; end_: Integer; bottom: Integer); cdecl;
  procedure setPivotX(pivotX: Single); cdecl;
  procedure setPivotY(pivotY: Single); cdecl;
  procedure setPressed(pressed: Boolean); cdecl;
  procedure setRight(right: Integer); cdecl;
  procedure setRotation(rotation: Single); cdecl;
  procedure setRotationX(rotationX: Single); cdecl;
  procedure setRotationY(rotationY: Single); cdecl;
  procedure setSaveEnabled(enabled: Boolean); cdecl;
  procedure setSaveFromParentEnabled(enabled: Boolean); cdecl;
  procedure setScaleX(scaleX: Single); cdecl;
  procedure setScaleY(scaleY: Single); cdecl;
  procedure setScrollBarDefaultDelayBeforeFade(scrollBarDefaultDelayBeforeFade: Integer); cdecl;
  procedure setScrollBarFadeDuration(scrollBarFadeDuration: Integer); cdecl;
  procedure setScrollBarSize(scrollBarSize: Integer); cdecl;
  procedure setScrollBarStyle(style: Integer); cdecl;
  procedure setScrollContainer(isScrollContainer: Boolean); cdecl;
  procedure setScrollX(value: Integer); cdecl;
  procedure setScrollY(value: Integer); cdecl;
  procedure setScrollbarFadingEnabled(fadeScrollbars: Boolean); cdecl;
  procedure setSelected(selected: Boolean); cdecl;
  procedure setSoundEffectsEnabled(soundEffectsEnabled: Boolean); cdecl;
  procedure setSystemUiVisibility(visibility: Integer); cdecl;
  procedure setTag(tag: JObject); cdecl; overload;
  procedure setTag(key: Integer; tag: JObject); cdecl; overload;
  procedure setTextAlignment(textAlignment: Integer); cdecl;
  procedure setTextDirection(textDirection: Integer); cdecl;
  procedure setTop(top: Integer); cdecl;
  procedure setTouchDelegate(delegate: JTouchDelegate); cdecl;
  procedure setTranslationX(translationX: Single); cdecl;
  procedure setTranslationY(translationY: Single); cdecl;
  procedure setVerticalFadingEdgeEnabled(verticalFadingEdgeEnabled: Boolean); cdecl;
  procedure setVerticalScrollBarEnabled(verticalScrollBarEnabled: Boolean); cdecl;
  procedure setVerticalScrollbarPosition(position: Integer); cdecl;
  procedure setVisibility(visibility: Integer); cdecl;
  procedure setWillNotCacheDrawing(willNotCacheDrawing: Boolean); cdecl;
  procedure setWillNotDraw(willNotDraw: Boolean); cdecl;
  procedure setX(x: Single); cdecl;
  procedure setY(y: Single); cdecl;
  function showContextMenu: Boolean; cdecl;
  function startActionMode(callback: JActionMode_Callback): JActionMode; cdecl;
  function startDrag(data: JClipData; shadowBuilder: JView_DragShadowBuilder; myLocalState: JObject; flags: Integer): Boolean; cdecl;
  function toString: JString; cdecl;
  procedure unscheduleDrawable(who: JDrawable; what: JRunnable); cdecl; overload;
  procedure unscheduleDrawable(who: JDrawable); cdecl; overload;
  function willNotCacheDrawing: Boolean; cdecl;
  function willNotDraw: Boolean; cdecl;
end;
TJView = class(TJavaGenericImport<JViewClass, JView>) end;

JSurfaceViewClass = interface(JViewClass)
['{C67B60CB-6C82-4920-BA7F-C504D331FBA5}']
  {Methods}
  function init(context: JContext): JSurfaceView; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet): JSurfaceView; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet; defStyle: Integer): JSurfaceView; cdecl; overload;
end;

[JavaSignature('android/view/SurfaceView')]
JSurfaceView = interface(JView)
['{1A9C1F1D-C888-47F7-AB78-C3D1AB5533D6}']
  {Methods}
  procedure draw(canvas: JCanvas); cdecl;
  function gatherTransparentRegion(region: JRegion): Boolean; cdecl;
  function getHolder: JSurfaceHolder; cdecl;
  procedure setSecure(isSecure: Boolean); cdecl;
  procedure setVisibility(visibility: Integer); cdecl;
  procedure setZOrderMediaOverlay(isMediaOverlay: Boolean); cdecl;
  procedure setZOrderOnTop(onTop: Boolean); cdecl;
end;
TJSurfaceView = class(TJavaGenericImport<JSurfaceViewClass, JSurfaceView>) end;

JPackageStatsClass = interface(JObjectClass)
['{784B2B42-5752-4326-B07D-233B196D3550}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Methods}
  function init(pkgName: JString): JPackageStats; cdecl; overload;
  function init(source: JParcel): JPackageStats; cdecl; overload;
  function init(pStats: JPackageStats): JPackageStats; cdecl; overload;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/content/pm/PackageStats')]
JPackageStats = interface(JObject)
['{E3C49E36-2C4A-4AFE-A351-36B1439832E8}']
  {Property Methods}
  function _GetcacheSize: Int64;
  procedure _SetcacheSize(Value: Int64);
  function _GetcodeSize: Int64;
  procedure _SetcodeSize(Value: Int64);
  function _GetdataSize: Int64;
  procedure _SetdataSize(Value: Int64);
  function _GetexternalCacheSize: Int64;
  procedure _SetexternalCacheSize(Value: Int64);
  function _GetexternalCodeSize: Int64;
  procedure _SetexternalCodeSize(Value: Int64);
  function _GetexternalDataSize: Int64;
  procedure _SetexternalDataSize(Value: Int64);
  function _GetexternalMediaSize: Int64;
  procedure _SetexternalMediaSize(Value: Int64);
  function _GetexternalObbSize: Int64;
  procedure _SetexternalObbSize(Value: Int64);
  function _GetpackageName: JString;
  procedure _SetpackageName(Value: JString);
  {Methods}
  function describeContents: Integer; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; parcelableFlags: Integer); cdecl;
  {Properties}
  property cacheSize: Int64 read _GetcacheSize write _SetcacheSize;
  property codeSize: Int64 read _GetcodeSize write _SetcodeSize;
  property dataSize: Int64 read _GetdataSize write _SetdataSize;
  property externalCacheSize: Int64 read _GetexternalCacheSize write _SetexternalCacheSize;
  property externalCodeSize: Int64 read _GetexternalCodeSize write _SetexternalCodeSize;
  property externalDataSize: Int64 read _GetexternalDataSize write _SetexternalDataSize;
  property externalMediaSize: Int64 read _GetexternalMediaSize write _SetexternalMediaSize;
  property externalObbSize: Int64 read _GetexternalObbSize write _SetexternalObbSize;
  property packageName: JString read _GetpackageName write _SetpackageName;
end;
TJPackageStats = class(TJavaGenericImport<JPackageStatsClass, JPackageStats>) end;

JView_OnAttachStateChangeListenerClass = interface(IJavaClass)
['{47D8E055-F47A-4A64-BB5B-EF33D445C77F}']
end;

[JavaSignature('android/view/View$OnAttachStateChangeListener')]
JView_OnAttachStateChangeListener = interface(IJavaInstance)
['{EC79895D-7F69-4EAE-9213-CF41D73EFF91}']
  {Methods}
  procedure onViewAttachedToWindow(v: JView); cdecl;
  procedure onViewDetachedFromWindow(v: JView); cdecl;
end;
TJView_OnAttachStateChangeListener = class(TJavaGenericImport<JView_OnAttachStateChangeListenerClass, JView_OnAttachStateChangeListener>) end;

JXfermodeClass = interface(JObjectClass)
['{A02A0774-5EEB-461D-9A5A-DE7A410045C1}']
  {Methods}
  function init: JXfermode; cdecl;
end;

[JavaSignature('android/graphics/Xfermode')]
JXfermode = interface(JObject)
['{2A25D3BB-02D9-4E64-A984-8C3A238DE91B}']
end;
TJXfermode = class(TJavaGenericImport<JXfermodeClass, JXfermode>) end;

JPixelXorXfermodeClass = interface(JXfermodeClass)
['{72F979E0-EC9B-4387-95A1-BA245CDE4310}']
  {Methods}
  function init(opColor: Integer): JPixelXorXfermode; cdecl;
end;

[JavaSignature('android/graphics/PixelXorXfermode')]
JPixelXorXfermode = interface(JXfermode)
['{8BA08395-74DA-486A-9B9B-F76894D1EBDE}']
end;
TJPixelXorXfermode = class(TJavaGenericImport<JPixelXorXfermodeClass, JPixelXorXfermode>) end;

JSyncStatsClass = interface(JObjectClass)
['{7BA32D0C-16F1-47A6-B318-1229042DC565}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Methods}
  function init: JSyncStats; cdecl; overload;
  function init(in_: JParcel): JSyncStats; cdecl; overload;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/content/SyncStats')]
JSyncStats = interface(JObject)
['{9AC3B6F7-3C69-4E3C-9DB6-DAD7E40F2B10}']
  {Property Methods}
  function _GetnumAuthExceptions: Int64;
  procedure _SetnumAuthExceptions(Value: Int64);
  function _GetnumConflictDetectedExceptions: Int64;
  procedure _SetnumConflictDetectedExceptions(Value: Int64);
  function _GetnumDeletes: Int64;
  procedure _SetnumDeletes(Value: Int64);
  function _GetnumEntries: Int64;
  procedure _SetnumEntries(Value: Int64);
  function _GetnumInserts: Int64;
  procedure _SetnumInserts(Value: Int64);
  function _GetnumIoExceptions: Int64;
  procedure _SetnumIoExceptions(Value: Int64);
  function _GetnumParseExceptions: Int64;
  procedure _SetnumParseExceptions(Value: Int64);
  function _GetnumSkippedEntries: Int64;
  procedure _SetnumSkippedEntries(Value: Int64);
  function _GetnumUpdates: Int64;
  procedure _SetnumUpdates(Value: Int64);
  {Methods}
  procedure clear; cdecl;
  function describeContents: Integer; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  {Properties}
  property numAuthExceptions: Int64 read _GetnumAuthExceptions write _SetnumAuthExceptions;
  property numConflictDetectedExceptions: Int64 read _GetnumConflictDetectedExceptions write _SetnumConflictDetectedExceptions;
  property numDeletes: Int64 read _GetnumDeletes write _SetnumDeletes;
  property numEntries: Int64 read _GetnumEntries write _SetnumEntries;
  property numInserts: Int64 read _GetnumInserts write _SetnumInserts;
  property numIoExceptions: Int64 read _GetnumIoExceptions write _SetnumIoExceptions;
  property numParseExceptions: Int64 read _GetnumParseExceptions write _SetnumParseExceptions;
  property numSkippedEntries: Int64 read _GetnumSkippedEntries write _SetnumSkippedEntries;
  property numUpdates: Int64 read _GetnumUpdates write _SetnumUpdates;
end;
TJSyncStats = class(TJavaGenericImport<JSyncStatsClass, JSyncStats>) end;

JOperationApplicationExceptionClass = interface(JExceptionClass)
['{F286AB2B-DD1B-46C2-B241-E70EF02088E6}']
  {Methods}
  function init: JOperationApplicationException; cdecl; overload;
  function init(message: JString): JOperationApplicationException; cdecl; overload;
  function init(message: JString; cause: JThrowable): JOperationApplicationException; cdecl; overload;
  function init(cause: JThrowable): JOperationApplicationException; cdecl; overload;
  function init(numSuccessfulYieldPoints: Integer): JOperationApplicationException; cdecl; overload;
  function init(message: JString; numSuccessfulYieldPoints: Integer): JOperationApplicationException; cdecl; overload;
end;

[JavaSignature('android/content/OperationApplicationException')]
JOperationApplicationException = interface(JException)
['{929E2751-19CD-4E05-8763-815EAC48443C}']
  {Methods}
  function getNumSuccessfulYieldPoints: Integer; cdecl;
end;
TJOperationApplicationException = class(TJavaGenericImport<JOperationApplicationExceptionClass, JOperationApplicationException>) end;

JDialogInterface_OnDismissListenerClass = interface(IJavaClass)
['{0B4A6A73-99E6-459A-95A7-C7664815AE0A}']
end;

[JavaSignature('android/content/DialogInterface$OnDismissListener')]
JDialogInterface_OnDismissListener = interface(IJavaInstance)
['{C7D62CC4-0CA6-4071-AA11-54CB0F74EB1F}']
  {Methods}
  procedure onDismiss(dialog: JDialogInterface); cdecl;
end;
TJDialogInterface_OnDismissListener = class(TJavaGenericImport<JDialogInterface_OnDismissListenerClass, JDialogInterface_OnDismissListener>) end;

JPaint_JoinClass = interface(JEnumClass)
['{DC90FC45-0EFB-419D-8784-75B944293687}']
  {Property Methods}
  function _GetBEVEL: JPaint_Join;
  function _GetMITER: JPaint_Join;
  function _GetROUND: JPaint_Join;
  {Methods}
  function valueOf(name: JString): JPaint_Join; cdecl;
  function values: TJavaObjectArray<JPaint_Join>; cdecl;
  {Properties}
  property BEVEL: JPaint_Join read _GetBEVEL;
  property MITER: JPaint_Join read _GetMITER;
  property ROUND: JPaint_Join read _GetROUND;
end;

[JavaSignature('android/graphics/Paint$Join')]
JPaint_Join = interface(JEnum)
['{FA5E10E3-0794-4DBF-8197-262F4A8AE178}']
end;
TJPaint_Join = class(TJavaGenericImport<JPaint_JoinClass, JPaint_Join>) end;

JPaint_CapClass = interface(JEnumClass)
['{1BBB271F-5B0D-408F-95FD-0D4864FCEF3D}']
  {Property Methods}
  function _GetBUTT: JPaint_Cap;
  function _GetROUND: JPaint_Cap;
  function _GetSQUARE: JPaint_Cap;
  {Methods}
  function valueOf(name: JString): JPaint_Cap; cdecl;
  function values: TJavaObjectArray<JPaint_Cap>; cdecl;
  {Properties}
  property BUTT: JPaint_Cap read _GetBUTT;
  property ROUND: JPaint_Cap read _GetROUND;
  property SQUARE: JPaint_Cap read _GetSQUARE;
end;

[JavaSignature('android/graphics/Paint$Cap')]
JPaint_Cap = interface(JEnum)
['{070E3F78-E07E-4030-B63C-CC827D673EF6}']
end;
TJPaint_Cap = class(TJavaGenericImport<JPaint_CapClass, JPaint_Cap>) end;

JViewTreeObserver_OnGlobalFocusChangeListenerClass = interface(IJavaClass)
['{0A2FBF59-F932-4639-844E-1B27BA4C7DE3}']
end;

[JavaSignature('android/view/ViewTreeObserver$OnGlobalFocusChangeListener')]
JViewTreeObserver_OnGlobalFocusChangeListener = interface(IJavaInstance)
['{003C4EF4-A75A-4B41-8D70-053F31AD13E3}']
  {Methods}
  procedure onGlobalFocusChanged(oldFocus: JView; newFocus: JView); cdecl;
end;
TJViewTreeObserver_OnGlobalFocusChangeListener = class(TJavaGenericImport<JViewTreeObserver_OnGlobalFocusChangeListenerClass, JViewTreeObserver_OnGlobalFocusChangeListener>) end;

Jdatabase_CharArrayBufferClass = interface(JObjectClass)
['{9803A361-150D-4474-8FDC-CB9CC38CFD8E}']
  {Methods}
  function init(size: Integer): Jdatabase_CharArrayBuffer; cdecl; overload;
  function init(buf: TJavaArray<Char>): Jdatabase_CharArrayBuffer; cdecl; overload;
end;

[JavaSignature('android/database/CharArrayBuffer')]
Jdatabase_CharArrayBuffer = interface(JObject)
['{C8B96698-AB4C-40DD-8075-99AF94766B5D}']
  {Property Methods}
  function _Getdata: TJavaArray<Char>;
  procedure _Setdata(Value: TJavaArray<Char>);
  function _GetsizeCopied: Integer;
  procedure _SetsizeCopied(Value: Integer);
  {Properties}
  property data: TJavaArray<Char> read _Getdata write _Setdata;
  property sizeCopied: Integer read _GetsizeCopied write _SetsizeCopied;
end;
TJdatabase_CharArrayBuffer = class(TJavaGenericImport<Jdatabase_CharArrayBufferClass, Jdatabase_CharArrayBuffer>) end;

JBlurMaskFilter_BlurClass = interface(JEnumClass)
['{827ABCE6-A489-4242-A315-4FE5AC73C6B4}']
  {Property Methods}
  function _GetINNER: JBlurMaskFilter_Blur;
  function _GetNORMAL: JBlurMaskFilter_Blur;
  function _GetOUTER: JBlurMaskFilter_Blur;
  function _GetSOLID: JBlurMaskFilter_Blur;
  {Methods}
  function valueOf(name: JString): JBlurMaskFilter_Blur; cdecl;
  function values: TJavaObjectArray<JBlurMaskFilter_Blur>; cdecl;
  {Properties}
  property INNER: JBlurMaskFilter_Blur read _GetINNER;
  property NORMAL: JBlurMaskFilter_Blur read _GetNORMAL;
  property OUTER: JBlurMaskFilter_Blur read _GetOUTER;
  property SOLID: JBlurMaskFilter_Blur read _GetSOLID;
end;

[JavaSignature('android/graphics/BlurMaskFilter$Blur')]
JBlurMaskFilter_Blur = interface(JEnum)
['{4A2F89C6-8703-4ECA-B6C3-298F40355435}']
end;
TJBlurMaskFilter_Blur = class(TJavaGenericImport<JBlurMaskFilter_BlurClass, JBlurMaskFilter_Blur>) end;

JPermissionGroupInfoClass = interface(JPackageItemInfoClass)
['{964923E8-1297-49FE-B34F-0133BBFD0467}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  function _GetFLAG_PERSONAL_INFO: Integer;
  {Methods}
  function init: JPermissionGroupInfo; cdecl; overload;
  function init(orig: JPermissionGroupInfo): JPermissionGroupInfo; cdecl; overload;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
  property FLAG_PERSONAL_INFO: Integer read _GetFLAG_PERSONAL_INFO;
end;

[JavaSignature('android/content/pm/PermissionGroupInfo')]
JPermissionGroupInfo = interface(JPackageItemInfo)
['{1D8E1B26-089D-4458-AAEA-5C4E6179A631}']
  {Property Methods}
  function _GetdescriptionRes: Integer;
  procedure _SetdescriptionRes(Value: Integer);
  function _Getflags: Integer;
  procedure _Setflags(Value: Integer);
  function _GetnonLocalizedDescription: JCharSequence;
  procedure _SetnonLocalizedDescription(Value: JCharSequence);
  function _Getpriority: Integer;
  procedure _Setpriority(Value: Integer);
  {Methods}
  function describeContents: Integer; cdecl;
  function loadDescription(pm: JPackageManager): JCharSequence; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; parcelableFlags: Integer); cdecl;
  {Properties}
  property descriptionRes: Integer read _GetdescriptionRes write _SetdescriptionRes;
  property flags: Integer read _Getflags write _Setflags;
  property nonLocalizedDescription: JCharSequence read _GetnonLocalizedDescription write _SetnonLocalizedDescription;
  property priority: Integer read _Getpriority write _Setpriority;
end;
TJPermissionGroupInfo = class(TJavaGenericImport<JPermissionGroupInfoClass, JPermissionGroupInfo>) end;

JContextMenu_ContextMenuInfoClass = interface(IJavaClass)
['{6F3CD3B3-B391-4929-AD4E-366A60F4385C}']
end;

[JavaSignature('android/view/ContextMenu$ContextMenuInfo')]
JContextMenu_ContextMenuInfo = interface(IJavaInstance)
['{2DC4CB20-2CDD-41BD-89B3-E8FE8AEEA852}']
end;
TJContextMenu_ContextMenuInfo = class(TJavaGenericImport<JContextMenu_ContextMenuInfoClass, JContextMenu_ContextMenuInfo>) end;

JPaintClass = interface(JObjectClass)
['{51637BF7-79B0-4E30-9A4D-22514020490B}']
  {Property Methods}
  function _GetANTI_ALIAS_FLAG: Integer;
  function _GetDEV_KERN_TEXT_FLAG: Integer;
  function _GetDITHER_FLAG: Integer;
  function _GetFAKE_BOLD_TEXT_FLAG: Integer;
  function _GetFILTER_BITMAP_FLAG: Integer;
  function _GetHINTING_OFF: Integer;
  function _GetHINTING_ON: Integer;
  function _GetLINEAR_TEXT_FLAG: Integer;
  function _GetSTRIKE_THRU_TEXT_FLAG: Integer;
  function _GetSUBPIXEL_TEXT_FLAG: Integer;
  function _GetUNDERLINE_TEXT_FLAG: Integer;
  {Methods}
  function init: JPaint; cdecl; overload;
  function init(flags: Integer): JPaint; cdecl; overload;
  function init(paint: JPaint): JPaint; cdecl; overload;
  {Properties}
  property ANTI_ALIAS_FLAG: Integer read _GetANTI_ALIAS_FLAG;
  property DEV_KERN_TEXT_FLAG: Integer read _GetDEV_KERN_TEXT_FLAG;
  property DITHER_FLAG: Integer read _GetDITHER_FLAG;
  property FAKE_BOLD_TEXT_FLAG: Integer read _GetFAKE_BOLD_TEXT_FLAG;
  property FILTER_BITMAP_FLAG: Integer read _GetFILTER_BITMAP_FLAG;
  property HINTING_OFF: Integer read _GetHINTING_OFF;
  property HINTING_ON: Integer read _GetHINTING_ON;
  property LINEAR_TEXT_FLAG: Integer read _GetLINEAR_TEXT_FLAG;
  property STRIKE_THRU_TEXT_FLAG: Integer read _GetSTRIKE_THRU_TEXT_FLAG;
  property SUBPIXEL_TEXT_FLAG: Integer read _GetSUBPIXEL_TEXT_FLAG;
  property UNDERLINE_TEXT_FLAG: Integer read _GetUNDERLINE_TEXT_FLAG;
end;

[JavaSignature('android/graphics/Paint')]
JPaint = interface(JObject)
['{56C76B78-7D6A-47D8-985F-401A64D81FFB}']
  {Methods}
  function ascent: Single; cdecl;
  function breakText(text: TJavaArray<Char>; index: Integer; count: Integer; maxWidth: Single; measuredWidth: TJavaArray<Single>): Integer; cdecl; overload;
  function breakText(text: JCharSequence; start: Integer; end_: Integer; measureForwards: Boolean; maxWidth: Single; measuredWidth: TJavaArray<Single>): Integer; cdecl; overload;
  function breakText(text: JString; measureForwards: Boolean; maxWidth: Single; measuredWidth: TJavaArray<Single>): Integer; cdecl; overload;
  procedure clearShadowLayer; cdecl;
  function descent: Single; cdecl;
  function getAlpha: Integer; cdecl;
  function getColor: Integer; cdecl;
  function getColorFilter: JColorFilter; cdecl;
  function getFillPath(src: JPath; dst: JPath): Boolean; cdecl;
  function getFlags: Integer; cdecl;
  function getFontMetrics(metrics: JPaint_FontMetrics): Single; cdecl; overload;
  function getFontMetrics: JPaint_FontMetrics; cdecl; overload;
  function getFontMetricsInt(fmi: JPaint_FontMetricsInt): Integer; cdecl; overload;
  function getFontMetricsInt: JPaint_FontMetricsInt; cdecl; overload;
  function getFontSpacing: Single; cdecl;
  function getHinting: Integer; cdecl;
  function getMaskFilter: JMaskFilter; cdecl;
  function getPathEffect: JPathEffect; cdecl;
  function getRasterizer: JRasterizer; cdecl;
  function getShader: JShader; cdecl;
  function getStrokeCap: JPaint_Cap; cdecl;
  function getStrokeJoin: JPaint_Join; cdecl;
  function getStrokeMiter: Single; cdecl;
  function getStrokeWidth: Single; cdecl;
  function getStyle: JPaint_Style; cdecl;
  function getTextAlign: JPaint_Align; cdecl;
  procedure getTextBounds(text: JString; start: Integer; end_: Integer; bounds: JRect); cdecl; overload;
  procedure getTextBounds(text: TJavaArray<Char>; index: Integer; count: Integer; bounds: JRect); cdecl; overload;
  function getTextLocale: JLocale; cdecl;
  procedure getTextPath(text: TJavaArray<Char>; index: Integer; count: Integer; x: Single; y: Single; path: JPath); cdecl; overload;
  procedure getTextPath(text: JString; start: Integer; end_: Integer; x: Single; y: Single; path: JPath); cdecl; overload;
  function getTextScaleX: Single; cdecl;
  function getTextSize: Single; cdecl;
  function getTextSkewX: Single; cdecl;
  function getTextWidths(text: TJavaArray<Char>; index: Integer; count: Integer; widths: TJavaArray<Single>): Integer; cdecl; overload;
  function getTextWidths(text: JCharSequence; start: Integer; end_: Integer; widths: TJavaArray<Single>): Integer; cdecl; overload;
  function getTextWidths(text: JString; start: Integer; end_: Integer; widths: TJavaArray<Single>): Integer; cdecl; overload;
  function getTextWidths(text: JString; widths: TJavaArray<Single>): Integer; cdecl; overload;
  function getTypeface: JTypeface; cdecl;
  function getXfermode: JXfermode; cdecl;
  function isAntiAlias: Boolean; cdecl;
  function isDither: Boolean; cdecl;
  function isFakeBoldText: Boolean; cdecl;
  function isFilterBitmap: Boolean; cdecl;
  function isLinearText: Boolean; cdecl;//Deprecated
  function isStrikeThruText: Boolean; cdecl;
  function isSubpixelText: Boolean; cdecl;
  function isUnderlineText: Boolean; cdecl;
  function measureText(text: TJavaArray<Char>; index: Integer; count: Integer): Single; cdecl; overload;
  function measureText(text: JString; start: Integer; end_: Integer): Single; cdecl; overload;
  function measureText(text: JString): Single; cdecl; overload;
  function measureText(text: JCharSequence; start: Integer; end_: Integer): Single; cdecl; overload;
  procedure reset; cdecl;
  procedure &set(src: JPaint); cdecl;
  procedure setARGB(a: Integer; r: Integer; g: Integer; b: Integer); cdecl;
  procedure setAlpha(a: Integer); cdecl;
  procedure setAntiAlias(aa: Boolean); cdecl;
  procedure setColor(color: Integer); cdecl;
  function setColorFilter(filter: JColorFilter): JColorFilter; cdecl;
  procedure setDither(dither: Boolean); cdecl;
  procedure setFakeBoldText(fakeBoldText: Boolean); cdecl;
  procedure setFilterBitmap(filter: Boolean); cdecl;
  procedure setFlags(flags: Integer); cdecl;
  procedure setHinting(mode: Integer); cdecl;
  procedure setLinearText(linearText: Boolean); cdecl;//Deprecated
  function setMaskFilter(maskfilter: JMaskFilter): JMaskFilter; cdecl;
  function setPathEffect(effect: JPathEffect): JPathEffect; cdecl;
  function setRasterizer(rasterizer: JRasterizer): JRasterizer; cdecl;
  function setShader(shader: JShader): JShader; cdecl;
  procedure setShadowLayer(radius: Single; dx: Single; dy: Single; color: Integer); cdecl;
  procedure setStrikeThruText(strikeThruText: Boolean); cdecl;
  procedure setStrokeCap(cap: JPaint_Cap); cdecl;
  procedure setStrokeJoin(join: JPaint_Join); cdecl;
  procedure setStrokeMiter(miter: Single); cdecl;
  procedure setStrokeWidth(width: Single); cdecl;
  procedure setStyle(style: JPaint_Style); cdecl;
  procedure setSubpixelText(subpixelText: Boolean); cdecl;
  procedure setTextAlign(align: JPaint_Align); cdecl;
  procedure setTextLocale(locale: JLocale); cdecl;
  procedure setTextScaleX(scaleX: Single); cdecl;
  procedure setTextSize(textSize: Single); cdecl;
  procedure setTextSkewX(skewX: Single); cdecl;
  function setTypeface(typeface: JTypeface): JTypeface; cdecl;
  procedure setUnderlineText(underlineText: Boolean); cdecl;
  function setXfermode(xfermode: JXfermode): JXfermode; cdecl;
end;
TJPaint = class(TJavaGenericImport<JPaintClass, JPaint>) end;

JPaint_FontMetricsIntClass = interface(JObjectClass)
['{7E299A8C-AB57-48E4-862D-979BC932C3EA}']
  {Methods}
  function init: JPaint_FontMetricsInt; cdecl;
end;

[JavaSignature('android/graphics/Paint$FontMetricsInt')]
JPaint_FontMetricsInt = interface(JObject)
['{D21F9199-6129-4DF5-8090-9F4E2563AF6C}']
  {Property Methods}
  function _Getascent: Integer;
  procedure _Setascent(Value: Integer);
  function _Getbottom: Integer;
  procedure _Setbottom(Value: Integer);
  function _Getdescent: Integer;
  procedure _Setdescent(Value: Integer);
  function _Getleading: Integer;
  procedure _Setleading(Value: Integer);
  function _Gettop: Integer;
  procedure _Settop(Value: Integer);
  {Methods}
  function toString: JString; cdecl;
  {Properties}
  property ascent: Integer read _Getascent write _Setascent;
  property bottom: Integer read _Getbottom write _Setbottom;
  property descent: Integer read _Getdescent write _Setdescent;
  property leading: Integer read _Getleading write _Setleading;
  property top: Integer read _Gettop write _Settop;
end;
TJPaint_FontMetricsInt = class(TJavaGenericImport<JPaint_FontMetricsIntClass, JPaint_FontMetricsInt>) end;

JCompletionInfoClass = interface(JObjectClass)
['{9720ED4C-1222-4EBD-A4B3-7806D9E5BA79}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Methods}
  function init(id: Int64; index: Integer; text: JCharSequence): JCompletionInfo; cdecl; overload;
  function init(id: Int64; index: Integer; text: JCharSequence; label_: JCharSequence): JCompletionInfo; cdecl; overload;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/view/inputmethod/CompletionInfo')]
JCompletionInfo = interface(JObject)
['{3B9797AB-1BDC-4C21-A88E-741E865398A4}']
  {Methods}
  function describeContents: Integer; cdecl;
  function getId: Int64; cdecl;
  function getLabel: JCharSequence; cdecl;
  function getPosition: Integer; cdecl;
  function getText: JCharSequence; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
end;
TJCompletionInfo = class(TJavaGenericImport<JCompletionInfoClass, JCompletionInfo>) end;

JViewGroupClass = interface(JViewClass)
['{249ABD79-A894-4B68-A005-F798FC9D670F}']
  {Property Methods}
  function _GetFOCUS_AFTER_DESCENDANTS: Integer;
  function _GetFOCUS_BEFORE_DESCENDANTS: Integer;
  function _GetFOCUS_BLOCK_DESCENDANTS: Integer;
  function _GetPERSISTENT_ALL_CACHES: Integer;
  function _GetPERSISTENT_ANIMATION_CACHE: Integer;
  function _GetPERSISTENT_NO_CACHE: Integer;
  function _GetPERSISTENT_SCROLLING_CACHE: Integer;
  {Methods}
  function init(context: JContext): JViewGroup; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet): JViewGroup; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet; defStyle: Integer): JViewGroup; cdecl; overload;
  function getChildMeasureSpec(spec: Integer; padding: Integer; childDimension: Integer): Integer; cdecl;
  {Properties}
  property FOCUS_AFTER_DESCENDANTS: Integer read _GetFOCUS_AFTER_DESCENDANTS;
  property FOCUS_BEFORE_DESCENDANTS: Integer read _GetFOCUS_BEFORE_DESCENDANTS;
  property FOCUS_BLOCK_DESCENDANTS: Integer read _GetFOCUS_BLOCK_DESCENDANTS;
  property PERSISTENT_ALL_CACHES: Integer read _GetPERSISTENT_ALL_CACHES;
  property PERSISTENT_ANIMATION_CACHE: Integer read _GetPERSISTENT_ANIMATION_CACHE;
  property PERSISTENT_NO_CACHE: Integer read _GetPERSISTENT_NO_CACHE;
  property PERSISTENT_SCROLLING_CACHE: Integer read _GetPERSISTENT_SCROLLING_CACHE;
end;

[JavaSignature('android/view/ViewGroup')]
JViewGroup = interface(JView)
['{DA84626B-CF12-4552-9CD8-EA843FE6C878}']
  {Methods}
  procedure addChildrenForAccessibility(childrenForAccessibility: JArrayList); cdecl;
  procedure addFocusables(views: JArrayList; direction: Integer; focusableMode: Integer); cdecl;
  function addStatesFromChildren: Boolean; cdecl;
  procedure addTouchables(views: JArrayList); cdecl;
  procedure addView(child: JView); cdecl; overload;
  procedure addView(child: JView; index: Integer); cdecl; overload;
  procedure addView(child: JView; width: Integer; height: Integer); cdecl; overload;
  procedure addView(child: JView; params: JViewGroup_LayoutParams); cdecl; overload;
  procedure addView(child: JView; index: Integer; params: JViewGroup_LayoutParams); cdecl; overload;
  procedure bringChildToFront(child: JView); cdecl;
  procedure childDrawableStateChanged(child: JView); cdecl;
  procedure clearChildFocus(child: JView); cdecl;
  procedure clearDisappearingChildren; cdecl;
  procedure clearFocus; cdecl;
  procedure dispatchConfigurationChanged(newConfig: JConfiguration); cdecl;
  procedure dispatchDisplayHint(hint: Integer); cdecl;
  function dispatchDragEvent(event: JDragEvent): Boolean; cdecl;
  function dispatchKeyEvent(event: JKeyEvent): Boolean; cdecl;
  function dispatchKeyEventPreIme(event: JKeyEvent): Boolean; cdecl;
  function dispatchKeyShortcutEvent(event: JKeyEvent): Boolean; cdecl;
  procedure dispatchSetActivated(activated: Boolean); cdecl;
  procedure dispatchSetSelected(selected: Boolean); cdecl;
  procedure dispatchSystemUiVisibilityChanged(visible: Integer); cdecl;
  function dispatchTouchEvent(ev: JMotionEvent): Boolean; cdecl;
  function dispatchTrackballEvent(event: JMotionEvent): Boolean; cdecl;
  function dispatchUnhandledMove(focused: JView; direction: Integer): Boolean; cdecl;
  procedure dispatchWindowFocusChanged(hasFocus: Boolean); cdecl;
  procedure dispatchWindowSystemUiVisiblityChanged(visible: Integer); cdecl;
  procedure dispatchWindowVisibilityChanged(visibility: Integer); cdecl;
  procedure endViewTransition(view: JView); cdecl;
  function findFocus: JView; cdecl;
  procedure findViewsWithText(outViews: JArrayList; text: JCharSequence; flags: Integer); cdecl;
  function focusSearch(focused: JView; direction: Integer): JView; cdecl;
  procedure focusableViewAvailable(v: JView); cdecl;
  function gatherTransparentRegion(region: JRegion): Boolean; cdecl;
  function generateLayoutParams(attrs: JAttributeSet): JViewGroup_LayoutParams; cdecl;
  function getChildAt(index: Integer): JView; cdecl;
  function getChildCount: Integer; cdecl;
  function getChildVisibleRect(child: JView; r: JRect; offset: JPoint): Boolean; cdecl;
  function getDescendantFocusability: Integer; cdecl;
  function getFocusedChild: JView; cdecl;
  function getPersistentDrawingCache: Integer; cdecl;
  function hasFocus: Boolean; cdecl;
  function hasFocusable: Boolean; cdecl;
  function indexOfChild(child: JView): Integer; cdecl;
  procedure invalidateChild(child: JView; dirty: JRect); cdecl;
  function invalidateChildInParent(location: TJavaArray<Integer>; dirty: JRect): JViewParent; cdecl;
  function isAlwaysDrawnWithCacheEnabled: Boolean; cdecl;
  function isAnimationCacheEnabled: Boolean; cdecl;
  function isMotionEventSplittingEnabled: Boolean; cdecl;
  procedure jumpDrawablesToCurrentState; cdecl;
  procedure layout(l: Integer; t: Integer; r: Integer; b: Integer); cdecl;
  procedure offsetDescendantRectToMyCoords(descendant: JView; rect: JRect); cdecl;
  procedure offsetRectIntoDescendantCoords(descendant: JView; rect: JRect); cdecl;
  function onInterceptHoverEvent(event: JMotionEvent): Boolean; cdecl;
  function onInterceptTouchEvent(ev: JMotionEvent): Boolean; cdecl;
  procedure recomputeViewAttributes(child: JView); cdecl;
  procedure removeAllViews; cdecl;
  procedure removeAllViewsInLayout; cdecl;
  procedure removeView(view: JView); cdecl;
  procedure removeViewAt(index: Integer); cdecl;
  procedure removeViewInLayout(view: JView); cdecl;
  procedure removeViews(start: Integer; count: Integer); cdecl;
  procedure removeViewsInLayout(start: Integer; count: Integer); cdecl;
  procedure requestChildFocus(child: JView; focused: JView); cdecl;
  function requestChildRectangleOnScreen(child: JView; rectangle: JRect; immediate: Boolean): Boolean; cdecl;
  procedure requestDisallowInterceptTouchEvent(disallowIntercept: Boolean); cdecl;
  function requestFocus(direction: Integer; previouslyFocusedRect: JRect): Boolean; cdecl;
  procedure requestTransparentRegion(child: JView); cdecl;
  procedure scheduleLayoutAnimation; cdecl;
  procedure setAddStatesFromChildren(addsStates: Boolean); cdecl;
  procedure setAlwaysDrawnWithCacheEnabled(always: Boolean); cdecl;
  procedure setAnimationCacheEnabled(enabled: Boolean); cdecl;
  procedure setClipChildren(clipChildren: Boolean); cdecl;
  procedure setClipToPadding(clipToPadding: Boolean); cdecl;
  procedure setDescendantFocusability(focusability: Integer); cdecl;
  procedure setMotionEventSplittingEnabled(split: Boolean); cdecl;
  procedure setOnHierarchyChangeListener(listener: JViewGroup_OnHierarchyChangeListener); cdecl;
  procedure setPersistentDrawingCache(drawingCacheToKeep: Integer); cdecl;
  function shouldDelayChildPressedState: Boolean; cdecl;
  function showContextMenuForChild(originalView: JView): Boolean; cdecl;
  function startActionModeForChild(originalView: JView; callback: JActionMode_Callback): JActionMode; cdecl;
  procedure startLayoutAnimation; cdecl;
  procedure startViewTransition(view: JView); cdecl;
  procedure updateViewLayout(view: JView; params: JViewGroup_LayoutParams); cdecl;
end;
TJViewGroup = class(TJavaGenericImport<JViewGroupClass, JViewGroup>) end;

JLinearGradientClass = interface(JShaderClass)
['{7C2075CF-A281-458B-90E5-3899AF747163}']
  {Methods}
  function init(x0: Single; y0: Single; x1: Single; y1: Single; colors: TJavaArray<Integer>; positions: TJavaArray<Single>; tile: JShader_TileMode): JLinearGradient; cdecl; overload;
  function init(x0: Single; y0: Single; x1: Single; y1: Single; color0: Integer; color1: Integer; tile: JShader_TileMode): JLinearGradient; cdecl; overload;
end;

[JavaSignature('android/graphics/LinearGradient')]
JLinearGradient = interface(JShader)
['{EEFB1AC5-72CB-4579-B12F-35ACF6A3743D}']
end;
TJLinearGradient = class(TJavaGenericImport<JLinearGradientClass, JLinearGradient>) end;

JCorrectionInfoClass = interface(JObjectClass)
['{34F793B9-F218-4224-89AC-20EEA48F830C}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Methods}
  function init(offset: Integer; oldText: JCharSequence; newText: JCharSequence): JCorrectionInfo; cdecl;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/view/inputmethod/CorrectionInfo')]
JCorrectionInfo = interface(JObject)
['{8C4FD364-59EB-486A-AD7F-0C10204A5115}']
  {Methods}
  function describeContents: Integer; cdecl;
  function getNewText: JCharSequence; cdecl;
  function getOffset: Integer; cdecl;
  function getOldText: JCharSequence; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
end;
TJCorrectionInfo = class(TJavaGenericImport<JCorrectionInfoClass, JCorrectionInfo>) end;

JDisplayClass = interface(JObjectClass)
['{E130072D-1414-44D4-85B6-A9041E73BA9B}']
  {Property Methods}
  function _GetDEFAULT_DISPLAY: Integer;
  function _GetFLAG_SECURE: Integer;
  function _GetFLAG_SUPPORTS_PROTECTED_BUFFERS: Integer;
  {Properties}
  property DEFAULT_DISPLAY: Integer read _GetDEFAULT_DISPLAY;
  property FLAG_SECURE: Integer read _GetFLAG_SECURE;
  property FLAG_SUPPORTS_PROTECTED_BUFFERS: Integer read _GetFLAG_SUPPORTS_PROTECTED_BUFFERS;
end;

[JavaSignature('android/view/Display')]
JDisplay = interface(JObject)
['{53A1E800-9C93-4F4D-B5B4-2DDE6DF3859E}']
  {Methods}
  procedure getCurrentSizeRange(outSmallestSize: JPoint; outLargestSize: JPoint); cdecl;
  function getDisplayId: Integer; cdecl;
  function getFlags: Integer; cdecl;
  function getHeight: Integer; cdecl;//Deprecated
  procedure getMetrics(outMetrics: JDisplayMetrics); cdecl;
  function getName: JString; cdecl;
  function getOrientation: Integer; cdecl;//Deprecated
  function getPixelFormat: Integer; cdecl;//Deprecated
  procedure getRealMetrics(outMetrics: JDisplayMetrics); cdecl;
  procedure getRealSize(outSize: JPoint); cdecl;
  procedure getRectSize(outSize: JRect); cdecl;
  function getRefreshRate: Single; cdecl;
  function getRotation: Integer; cdecl;
  procedure getSize(outSize: JPoint); cdecl;
  function getWidth: Integer; cdecl;//Deprecated
  function isValid: Boolean; cdecl;
  function toString: JString; cdecl;
end;
TJDisplay = class(TJavaGenericImport<JDisplayClass, JDisplay>) end;

JView_OnGenericMotionListenerClass = interface(IJavaClass)
['{CCA433E3-85AD-48F7-BF53-67E534A399A8}']
end;

[JavaSignature('android/view/View$OnGenericMotionListener')]
JView_OnGenericMotionListener = interface(IJavaInstance)
['{EFD680B0-26A6-46B9-819C-AC7BAFE0B661}']
  {Methods}
  function onGenericMotion(v: JView; event: JMotionEvent): Boolean; cdecl;
end;
TJView_OnGenericMotionListener = class(TJavaGenericImport<JView_OnGenericMotionListenerClass, JView_OnGenericMotionListener>) end;

JTextUtils_TruncateAtClass = interface(JEnumClass)
['{9339BF23-272E-4F60-B4A3-99F66622C905}']
  {Property Methods}
  function _GetEND: JTextUtils_TruncateAt;
  function _GetMARQUEE: JTextUtils_TruncateAt;
  function _GetMIDDLE: JTextUtils_TruncateAt;
  function _GetSTART: JTextUtils_TruncateAt;
  {Methods}
  function valueOf(name: JString): JTextUtils_TruncateAt; cdecl;
  function values: TJavaObjectArray<JTextUtils_TruncateAt>; cdecl;
  {Properties}
  property &END: JTextUtils_TruncateAt read _GetEND;
  property MARQUEE: JTextUtils_TruncateAt read _GetMARQUEE;
  property MIDDLE: JTextUtils_TruncateAt read _GetMIDDLE;
  property START: JTextUtils_TruncateAt read _GetSTART;
end;

[JavaSignature('android/text/TextUtils$TruncateAt')]
JTextUtils_TruncateAt = interface(JEnum)
['{96820C13-A2B2-48F2-B37E-E86918C45DA8}']
end;
TJTextUtils_TruncateAt = class(TJavaGenericImport<JTextUtils_TruncateAtClass, JTextUtils_TruncateAt>) end;

JContextClass = interface(JObjectClass)
['{DFC8B995-8B7D-4BBF-8100-C0782A8B9086}']
  {Property Methods}
  function _GetACCESSIBILITY_SERVICE: JString;
  function _GetACCOUNT_SERVICE: JString;
  function _GetACTIVITY_SERVICE: JString;
  function _GetALARM_SERVICE: JString;
  function _GetAUDIO_SERVICE: JString;
  function _GetBIND_ABOVE_CLIENT: Integer;
  function _GetBIND_ADJUST_WITH_ACTIVITY: Integer;
  function _GetBIND_ALLOW_OOM_MANAGEMENT: Integer;
  function _GetBIND_AUTO_CREATE: Integer;
  function _GetBIND_DEBUG_UNBIND: Integer;
  function _GetBIND_IMPORTANT: Integer;
  function _GetBIND_NOT_FOREGROUND: Integer;
  function _GetBIND_WAIVE_PRIORITY: Integer;
  function _GetCLIPBOARD_SERVICE: JString;
  function _GetCONNECTIVITY_SERVICE: JString;
  function _GetCONTEXT_IGNORE_SECURITY: Integer;
  function _GetCONTEXT_INCLUDE_CODE: Integer;
  function _GetCONTEXT_RESTRICTED: Integer;
  function _GetDEVICE_POLICY_SERVICE: JString;
  function _GetDISPLAY_SERVICE: JString;
  function _GetDOWNLOAD_SERVICE: JString;
  function _GetDROPBOX_SERVICE: JString;
  function _GetINPUT_METHOD_SERVICE: JString;
  function _GetINPUT_SERVICE: JString;
  function _GetKEYGUARD_SERVICE: JString;
  function _GetLAYOUT_INFLATER_SERVICE: JString;
  function _GetLOCATION_SERVICE: JString;
  function _GetMEDIA_ROUTER_SERVICE: JString;
  function _GetMODE_APPEND: Integer;
  function _GetMODE_ENABLE_WRITE_AHEAD_LOGGING: Integer;
  function _GetMODE_MULTI_PROCESS: Integer;
  function _GetMODE_PRIVATE: Integer;
  function _GetMODE_WORLD_READABLE: Integer;
  function _GetMODE_WORLD_WRITEABLE: Integer;
  function _GetNFC_SERVICE: JString;
  function _GetNOTIFICATION_SERVICE: JString;
  function _GetNSD_SERVICE: JString;
  function _GetPOWER_SERVICE: JString;
  function _GetSEARCH_SERVICE: JString;
  function _GetSENSOR_SERVICE: JString;
  function _GetSTORAGE_SERVICE: JString;
  function _GetTELEPHONY_SERVICE: JString;
  function _GetTEXT_SERVICES_MANAGER_SERVICE: JString;
  function _GetUI_MODE_SERVICE: JString;
  function _GetUSB_SERVICE: JString;
  function _GetUSER_SERVICE: JString;
  function _GetVIBRATOR_SERVICE: JString;
  function _GetWALLPAPER_SERVICE: JString;
  function _GetWIFI_P2P_SERVICE: JString;
  function _GetWIFI_SERVICE: JString;
  function _GetWINDOW_SERVICE: JString;
  {Methods}
  function init: JContext; cdecl;
  {Properties}
  property ACCESSIBILITY_SERVICE: JString read _GetACCESSIBILITY_SERVICE;
  property ACCOUNT_SERVICE: JString read _GetACCOUNT_SERVICE;
  property ACTIVITY_SERVICE: JString read _GetACTIVITY_SERVICE;
  property ALARM_SERVICE: JString read _GetALARM_SERVICE;
  property AUDIO_SERVICE: JString read _GetAUDIO_SERVICE;
  property BIND_ABOVE_CLIENT: Integer read _GetBIND_ABOVE_CLIENT;
  property BIND_ADJUST_WITH_ACTIVITY: Integer read _GetBIND_ADJUST_WITH_ACTIVITY;
  property BIND_ALLOW_OOM_MANAGEMENT: Integer read _GetBIND_ALLOW_OOM_MANAGEMENT;
  property BIND_AUTO_CREATE: Integer read _GetBIND_AUTO_CREATE;
  property BIND_DEBUG_UNBIND: Integer read _GetBIND_DEBUG_UNBIND;
  property BIND_IMPORTANT: Integer read _GetBIND_IMPORTANT;
  property BIND_NOT_FOREGROUND: Integer read _GetBIND_NOT_FOREGROUND;
  property BIND_WAIVE_PRIORITY: Integer read _GetBIND_WAIVE_PRIORITY;
  property CLIPBOARD_SERVICE: JString read _GetCLIPBOARD_SERVICE;
  property CONNECTIVITY_SERVICE: JString read _GetCONNECTIVITY_SERVICE;
  property CONTEXT_IGNORE_SECURITY: Integer read _GetCONTEXT_IGNORE_SECURITY;
  property CONTEXT_INCLUDE_CODE: Integer read _GetCONTEXT_INCLUDE_CODE;
  property CONTEXT_RESTRICTED: Integer read _GetCONTEXT_RESTRICTED;
  property DEVICE_POLICY_SERVICE: JString read _GetDEVICE_POLICY_SERVICE;
  property DISPLAY_SERVICE: JString read _GetDISPLAY_SERVICE;
  property DOWNLOAD_SERVICE: JString read _GetDOWNLOAD_SERVICE;
  property DROPBOX_SERVICE: JString read _GetDROPBOX_SERVICE;
  property INPUT_METHOD_SERVICE: JString read _GetINPUT_METHOD_SERVICE;
  property INPUT_SERVICE: JString read _GetINPUT_SERVICE;
  property KEYGUARD_SERVICE: JString read _GetKEYGUARD_SERVICE;
  property LAYOUT_INFLATER_SERVICE: JString read _GetLAYOUT_INFLATER_SERVICE;
  property LOCATION_SERVICE: JString read _GetLOCATION_SERVICE;
  property MEDIA_ROUTER_SERVICE: JString read _GetMEDIA_ROUTER_SERVICE;
  property MODE_APPEND: Integer read _GetMODE_APPEND;
  property MODE_ENABLE_WRITE_AHEAD_LOGGING: Integer read _GetMODE_ENABLE_WRITE_AHEAD_LOGGING;
  property MODE_MULTI_PROCESS: Integer read _GetMODE_MULTI_PROCESS;
  property MODE_PRIVATE: Integer read _GetMODE_PRIVATE;
  property MODE_WORLD_READABLE: Integer read _GetMODE_WORLD_READABLE;
  property MODE_WORLD_WRITEABLE: Integer read _GetMODE_WORLD_WRITEABLE;
  property NFC_SERVICE: JString read _GetNFC_SERVICE;
  property NOTIFICATION_SERVICE: JString read _GetNOTIFICATION_SERVICE;
  property NSD_SERVICE: JString read _GetNSD_SERVICE;
  property POWER_SERVICE: JString read _GetPOWER_SERVICE;
  property SEARCH_SERVICE: JString read _GetSEARCH_SERVICE;
  property SENSOR_SERVICE: JString read _GetSENSOR_SERVICE;
  property STORAGE_SERVICE: JString read _GetSTORAGE_SERVICE;
  property TELEPHONY_SERVICE: JString read _GetTELEPHONY_SERVICE;
  property TEXT_SERVICES_MANAGER_SERVICE: JString read _GetTEXT_SERVICES_MANAGER_SERVICE;
  property UI_MODE_SERVICE: JString read _GetUI_MODE_SERVICE;
  property USB_SERVICE: JString read _GetUSB_SERVICE;
  property USER_SERVICE: JString read _GetUSER_SERVICE;
  property VIBRATOR_SERVICE: JString read _GetVIBRATOR_SERVICE;
  property WALLPAPER_SERVICE: JString read _GetWALLPAPER_SERVICE;
  property WIFI_P2P_SERVICE: JString read _GetWIFI_P2P_SERVICE;
  property WIFI_SERVICE: JString read _GetWIFI_SERVICE;
  property WINDOW_SERVICE: JString read _GetWINDOW_SERVICE;
end;

[JavaSignature('android/content/Context')]
JContext = interface(JObject)
['{72BE5195-DD04-4E6E-8A27-AA4DF0D8F912}']
  {Methods}
  function bindService(service: JIntent; conn: JServiceConnection; flags: Integer): Boolean; cdecl;
  function checkCallingOrSelfPermission(permission: JString): Integer; cdecl;
  function checkCallingOrSelfUriPermission(uri: Jnet_Uri; modeFlags: Integer): Integer; cdecl;
  function checkCallingPermission(permission: JString): Integer; cdecl;
  function checkCallingUriPermission(uri: Jnet_Uri; modeFlags: Integer): Integer; cdecl;
  function checkPermission(permission: JString; pid: Integer; uid: Integer): Integer; cdecl;
  function checkUriPermission(uri: Jnet_Uri; pid: Integer; uid: Integer; modeFlags: Integer): Integer; cdecl; overload;
  function checkUriPermission(uri: Jnet_Uri; readPermission: JString; writePermission: JString; pid: Integer; uid: Integer; modeFlags: Integer): Integer; cdecl; overload;
  procedure clearWallpaper; cdecl;//Deprecated
  function createConfigurationContext(overrideConfiguration: JConfiguration): JContext; cdecl;
  function createDisplayContext(display: JDisplay): JContext; cdecl;
  function createPackageContext(packageName: JString; flags: Integer): JContext; cdecl;
  function databaseList: TJavaObjectArray<JString>; cdecl;
  function deleteDatabase(name: JString): Boolean; cdecl;
  function deleteFile(name: JString): Boolean; cdecl;
  procedure enforceCallingOrSelfPermission(permission: JString; message: JString); cdecl;
  procedure enforceCallingOrSelfUriPermission(uri: Jnet_Uri; modeFlags: Integer; message: JString); cdecl;
  procedure enforceCallingPermission(permission: JString; message: JString); cdecl;
  procedure enforceCallingUriPermission(uri: Jnet_Uri; modeFlags: Integer; message: JString); cdecl;
  procedure enforcePermission(permission: JString; pid: Integer; uid: Integer; message: JString); cdecl;
  procedure enforceUriPermission(uri: Jnet_Uri; pid: Integer; uid: Integer; modeFlags: Integer; message: JString); cdecl; overload;
  procedure enforceUriPermission(uri: Jnet_Uri; readPermission: JString; writePermission: JString; pid: Integer; uid: Integer; modeFlags: Integer; message: JString); cdecl; overload;
  function fileList: TJavaObjectArray<JString>; cdecl;
  function getApplicationContext: JContext; cdecl;
  function getApplicationInfo: JApplicationInfo; cdecl;
  function getAssets: JAssetManager; cdecl;
  function getCacheDir: JFile; cdecl;
  function getClassLoader: JClassLoader; cdecl;
  function getContentResolver: JContentResolver; cdecl;
  function getDatabasePath(name: JString): JFile; cdecl;
  function getDir(name: JString; mode: Integer): JFile; cdecl;
  function getExternalCacheDir: JFile; cdecl;
  function getExternalFilesDir(type_: JString): JFile; cdecl;
  function getFileStreamPath(name: JString): JFile; cdecl;
  function getFilesDir: JFile; cdecl;
  function getMainLooper: JLooper; cdecl;
  function getObbDir: JFile; cdecl;
  function getPackageCodePath: JString; cdecl;
  function getPackageManager: JPackageManager; cdecl;
  function getPackageName: JString; cdecl;
  function getPackageResourcePath: JString; cdecl;
  function getResources: JResources; cdecl;
  function getSharedPreferences(name: JString; mode: Integer): JSharedPreferences; cdecl;
  function getString(resId: Integer): JString; cdecl; overload;
  function getSystemService(name: JString): JObject; cdecl;
  function getText(resId: Integer): JCharSequence; cdecl;
  function getTheme: JResources_Theme; cdecl;
  function getWallpaper: JDrawable; cdecl;//Deprecated
  function getWallpaperDesiredMinimumHeight: Integer; cdecl;//Deprecated
  function getWallpaperDesiredMinimumWidth: Integer; cdecl;//Deprecated
  procedure grantUriPermission(toPackage: JString; uri: Jnet_Uri; modeFlags: Integer); cdecl;
  function isRestricted: Boolean; cdecl;
  function obtainStyledAttributes(attrs: TJavaArray<Integer>): JTypedArray; cdecl; overload;
  function obtainStyledAttributes(resid: Integer; attrs: TJavaArray<Integer>): JTypedArray; cdecl; overload;
  function obtainStyledAttributes(set_: JAttributeSet; attrs: TJavaArray<Integer>): JTypedArray; cdecl; overload;
  function obtainStyledAttributes(set_: JAttributeSet; attrs: TJavaArray<Integer>; defStyleAttr: Integer; defStyleRes: Integer): JTypedArray; cdecl; overload;
  function openFileInput(name: JString): JFileInputStream; cdecl;
  function openFileOutput(name: JString; mode: Integer): JFileOutputStream; cdecl;
  function openOrCreateDatabase(name: JString; mode: Integer; factory: JSQLiteDatabase_CursorFactory): JSQLiteDatabase; cdecl; overload;
  function openOrCreateDatabase(name: JString; mode: Integer; factory: JSQLiteDatabase_CursorFactory; errorHandler: JDatabaseErrorHandler): JSQLiteDatabase; cdecl; overload;
  function peekWallpaper: JDrawable; cdecl;//Deprecated
  procedure registerComponentCallbacks(callback: JComponentCallbacks); cdecl;
  function registerReceiver(receiver: JBroadcastReceiver; filter: JIntentFilter): JIntent; cdecl; overload;
  function registerReceiver(receiver: JBroadcastReceiver; filter: JIntentFilter; broadcastPermission: JString; scheduler: JHandler): JIntent; cdecl; overload;
  procedure removeStickyBroadcast(intent: JIntent); cdecl;
  procedure removeStickyBroadcastAsUser(intent: JIntent; user: JUserHandle); cdecl;
  procedure revokeUriPermission(uri: Jnet_Uri; modeFlags: Integer); cdecl;
  procedure sendBroadcast(intent: JIntent); cdecl; overload;
  procedure sendBroadcast(intent: JIntent; receiverPermission: JString); cdecl; overload;
  procedure sendBroadcastAsUser(intent: JIntent; user: JUserHandle); cdecl; overload;
  procedure sendBroadcastAsUser(intent: JIntent; user: JUserHandle; receiverPermission: JString); cdecl; overload;
  procedure sendOrderedBroadcast(intent: JIntent; receiverPermission: JString); cdecl; overload;
  procedure sendOrderedBroadcast(intent: JIntent; receiverPermission: JString; resultReceiver: JBroadcastReceiver; scheduler: JHandler; initialCode: Integer; initialData: JString; initialExtras: JBundle); cdecl; overload;
  procedure sendOrderedBroadcastAsUser(intent: JIntent; user: JUserHandle; receiverPermission: JString; resultReceiver: JBroadcastReceiver; scheduler: JHandler; initialCode: Integer; initialData: JString; initialExtras: JBundle); cdecl;
  procedure sendStickyBroadcast(intent: JIntent); cdecl;
  procedure sendStickyBroadcastAsUser(intent: JIntent; user: JUserHandle); cdecl;
  procedure sendStickyOrderedBroadcast(intent: JIntent; resultReceiver: JBroadcastReceiver; scheduler: JHandler; initialCode: Integer; initialData: JString; initialExtras: JBundle); cdecl;
  procedure sendStickyOrderedBroadcastAsUser(intent: JIntent; user: JUserHandle; resultReceiver: JBroadcastReceiver; scheduler: JHandler; initialCode: Integer; initialData: JString; initialExtras: JBundle); cdecl;
  procedure setTheme(resid: Integer); cdecl;
  procedure setWallpaper(bitmap: JBitmap); cdecl; overload;//Deprecated
  procedure setWallpaper(data: JInputStream); cdecl; overload;//Deprecated
  procedure startActivities(intents: TJavaObjectArray<JIntent>); cdecl; overload;
  procedure startActivities(intents: TJavaObjectArray<JIntent>; options: JBundle); cdecl; overload;
  procedure startActivity(intent: JIntent); cdecl; overload;
  procedure startActivity(intent: JIntent; options: JBundle); cdecl; overload;
  function startInstrumentation(className: JComponentName; profileFile: JString; arguments: JBundle): Boolean; cdecl;
  procedure startIntentSender(intent: JIntentSender; fillInIntent: JIntent; flagsMask: Integer; flagsValues: Integer; extraFlags: Integer); cdecl; overload;
  procedure startIntentSender(intent: JIntentSender; fillInIntent: JIntent; flagsMask: Integer; flagsValues: Integer; extraFlags: Integer; options: JBundle); cdecl; overload;
  function startService(service: JIntent): JComponentName; cdecl;
  function stopService(service: JIntent): Boolean; cdecl;
  procedure unbindService(conn: JServiceConnection); cdecl;
  procedure unregisterComponentCallbacks(callback: JComponentCallbacks); cdecl;
  procedure unregisterReceiver(receiver: JBroadcastReceiver); cdecl;
end;
TJContext = class(TJavaGenericImport<JContextClass, JContext>) end;

JContextWrapperClass = interface(JContextClass)
['{EA8706C6-B2D2-41C0-935D-838BB8704209}']
  {Methods}
  function init(base: JContext): JContextWrapper; cdecl;
end;

[JavaSignature('android/content/ContextWrapper')]
JContextWrapper = interface(JContext)
['{D742A401-5631-42C5-9B65-6F1F46811A40}']
  {Methods}
  function bindService(service: JIntent; conn: JServiceConnection; flags: Integer): Boolean; cdecl;
  function checkCallingOrSelfPermission(permission: JString): Integer; cdecl;
  function checkCallingOrSelfUriPermission(uri: Jnet_Uri; modeFlags: Integer): Integer; cdecl;
  function checkCallingPermission(permission: JString): Integer; cdecl;
  function checkCallingUriPermission(uri: Jnet_Uri; modeFlags: Integer): Integer; cdecl;
  function checkPermission(permission: JString; pid: Integer; uid: Integer): Integer; cdecl;
  function checkUriPermission(uri: Jnet_Uri; pid: Integer; uid: Integer; modeFlags: Integer): Integer; cdecl; overload;
  function checkUriPermission(uri: Jnet_Uri; readPermission: JString; writePermission: JString; pid: Integer; uid: Integer; modeFlags: Integer): Integer; cdecl; overload;
  procedure clearWallpaper; cdecl;
  function createConfigurationContext(overrideConfiguration: JConfiguration): JContext; cdecl;
  function createDisplayContext(display: JDisplay): JContext; cdecl;
  function createPackageContext(packageName: JString; flags: Integer): JContext; cdecl;
  function databaseList: TJavaObjectArray<JString>; cdecl;
  function deleteDatabase(name: JString): Boolean; cdecl;
  function deleteFile(name: JString): Boolean; cdecl;
  procedure enforceCallingOrSelfPermission(permission: JString; message: JString); cdecl;
  procedure enforceCallingOrSelfUriPermission(uri: Jnet_Uri; modeFlags: Integer; message: JString); cdecl;
  procedure enforceCallingPermission(permission: JString; message: JString); cdecl;
  procedure enforceCallingUriPermission(uri: Jnet_Uri; modeFlags: Integer; message: JString); cdecl;
  procedure enforcePermission(permission: JString; pid: Integer; uid: Integer; message: JString); cdecl;
  procedure enforceUriPermission(uri: Jnet_Uri; pid: Integer; uid: Integer; modeFlags: Integer; message: JString); cdecl; overload;
  procedure enforceUriPermission(uri: Jnet_Uri; readPermission: JString; writePermission: JString; pid: Integer; uid: Integer; modeFlags: Integer; message: JString); cdecl; overload;
  function fileList: TJavaObjectArray<JString>; cdecl;
  function getApplicationContext: JContext; cdecl;
  function getApplicationInfo: JApplicationInfo; cdecl;
  function getAssets: JAssetManager; cdecl;
  function getBaseContext: JContext; cdecl;
  function getCacheDir: JFile; cdecl;
  function getClassLoader: JClassLoader; cdecl;
  function getContentResolver: JContentResolver; cdecl;
  function getDatabasePath(name: JString): JFile; cdecl;
  function getDir(name: JString; mode: Integer): JFile; cdecl;
  function getExternalCacheDir: JFile; cdecl;
  function getExternalFilesDir(type_: JString): JFile; cdecl;
  function getFileStreamPath(name: JString): JFile; cdecl;
  function getFilesDir: JFile; cdecl;
  function getMainLooper: JLooper; cdecl;
  function getObbDir: JFile; cdecl;
  function getPackageCodePath: JString; cdecl;
  function getPackageManager: JPackageManager; cdecl;
  function getPackageName: JString; cdecl;
  function getPackageResourcePath: JString; cdecl;
  function getResources: JResources; cdecl;
  function getSharedPreferences(name: JString; mode: Integer): JSharedPreferences; cdecl;
  function getSystemService(name: JString): JObject; cdecl;
  function getTheme: JResources_Theme; cdecl;
  function getWallpaper: JDrawable; cdecl;
  function getWallpaperDesiredMinimumHeight: Integer; cdecl;
  function getWallpaperDesiredMinimumWidth: Integer; cdecl;
  procedure grantUriPermission(toPackage: JString; uri: Jnet_Uri; modeFlags: Integer); cdecl;
  function isRestricted: Boolean; cdecl;
  function openFileInput(name: JString): JFileInputStream; cdecl;
  function openFileOutput(name: JString; mode: Integer): JFileOutputStream; cdecl;
  function openOrCreateDatabase(name: JString; mode: Integer; factory: JSQLiteDatabase_CursorFactory): JSQLiteDatabase; cdecl; overload;
  function openOrCreateDatabase(name: JString; mode: Integer; factory: JSQLiteDatabase_CursorFactory; errorHandler: JDatabaseErrorHandler): JSQLiteDatabase; cdecl; overload;
  function peekWallpaper: JDrawable; cdecl;
  function registerReceiver(receiver: JBroadcastReceiver; filter: JIntentFilter): JIntent; cdecl; overload;
  function registerReceiver(receiver: JBroadcastReceiver; filter: JIntentFilter; broadcastPermission: JString; scheduler: JHandler): JIntent; cdecl; overload;
  procedure removeStickyBroadcast(intent: JIntent); cdecl;
  procedure removeStickyBroadcastAsUser(intent: JIntent; user: JUserHandle); cdecl;
  procedure revokeUriPermission(uri: Jnet_Uri; modeFlags: Integer); cdecl;
  procedure sendBroadcast(intent: JIntent); cdecl; overload;
  procedure sendBroadcast(intent: JIntent; receiverPermission: JString); cdecl; overload;
  procedure sendBroadcastAsUser(intent: JIntent; user: JUserHandle); cdecl; overload;
  procedure sendBroadcastAsUser(intent: JIntent; user: JUserHandle; receiverPermission: JString); cdecl; overload;
  procedure sendOrderedBroadcast(intent: JIntent; receiverPermission: JString); cdecl; overload;
  procedure sendOrderedBroadcast(intent: JIntent; receiverPermission: JString; resultReceiver: JBroadcastReceiver; scheduler: JHandler; initialCode: Integer; initialData: JString; initialExtras: JBundle); cdecl; overload;
  procedure sendOrderedBroadcastAsUser(intent: JIntent; user: JUserHandle; receiverPermission: JString; resultReceiver: JBroadcastReceiver; scheduler: JHandler; initialCode: Integer; initialData: JString; initialExtras: JBundle); cdecl;
  procedure sendStickyBroadcast(intent: JIntent); cdecl;
  procedure sendStickyBroadcastAsUser(intent: JIntent; user: JUserHandle); cdecl;
  procedure sendStickyOrderedBroadcast(intent: JIntent; resultReceiver: JBroadcastReceiver; scheduler: JHandler; initialCode: Integer; initialData: JString; initialExtras: JBundle); cdecl;
  procedure sendStickyOrderedBroadcastAsUser(intent: JIntent; user: JUserHandle; resultReceiver: JBroadcastReceiver; scheduler: JHandler; initialCode: Integer; initialData: JString; initialExtras: JBundle); cdecl;
  procedure setTheme(resid: Integer); cdecl;
  procedure setWallpaper(bitmap: JBitmap); cdecl; overload;
  procedure setWallpaper(data: JInputStream); cdecl; overload;
  procedure startActivities(intents: TJavaObjectArray<JIntent>); cdecl; overload;
  procedure startActivities(intents: TJavaObjectArray<JIntent>; options: JBundle); cdecl; overload;
  procedure startActivity(intent: JIntent); cdecl; overload;
  procedure startActivity(intent: JIntent; options: JBundle); cdecl; overload;
  function startInstrumentation(className: JComponentName; profileFile: JString; arguments: JBundle): Boolean; cdecl;
  procedure startIntentSender(intent: JIntentSender; fillInIntent: JIntent; flagsMask: Integer; flagsValues: Integer; extraFlags: Integer); cdecl; overload;
  procedure startIntentSender(intent: JIntentSender; fillInIntent: JIntent; flagsMask: Integer; flagsValues: Integer; extraFlags: Integer; options: JBundle); cdecl; overload;
  function startService(service: JIntent): JComponentName; cdecl;
  function stopService(name: JIntent): Boolean; cdecl;
  procedure unbindService(conn: JServiceConnection); cdecl;
  procedure unregisterReceiver(receiver: JBroadcastReceiver); cdecl;
end;
TJContextWrapper = class(TJavaGenericImport<JContextWrapperClass, JContextWrapper>) end;

JMutableContextWrapperClass = interface(JContextWrapperClass)
['{06AAE2A0-2502-46EE-9E37-1C7E3E0522A4}']
  {Methods}
  function init(base: JContext): JMutableContextWrapper; cdecl;
end;

[JavaSignature('android/content/MutableContextWrapper')]
JMutableContextWrapper = interface(JContextWrapper)
['{A350046D-0F6F-41C3-8375-35988DFB1B05}']
  {Methods}
  procedure setBaseContext(base: JContext); cdecl;
end;
TJMutableContextWrapper = class(TJavaGenericImport<JMutableContextWrapperClass, JMutableContextWrapper>) end;

JPictureClass = interface(JObjectClass)
['{61779B05-9E01-4AD7-A7B4-2B5EC8AF1FB5}']
  {Methods}
  function init: JPicture; cdecl; overload;
  function init(src: JPicture): JPicture; cdecl; overload;
  function createFromStream(stream: JInputStream): JPicture; cdecl;
end;

[JavaSignature('android/graphics/Picture')]
JPicture = interface(JObject)
['{BE8841B4-DE7A-4399-ACBE-184B6C2F75AC}']
  {Methods}
  function beginRecording(width: Integer; height: Integer): JCanvas; cdecl;
  procedure draw(canvas: JCanvas); cdecl;
  procedure endRecording; cdecl;
  function getHeight: Integer; cdecl;
  function getWidth: Integer; cdecl;
  procedure writeToStream(stream: JOutputStream); cdecl;
end;
TJPicture = class(TJavaGenericImport<JPictureClass, JPicture>) end;

JViewManagerClass = interface(IJavaClass)
['{1E5DFC6A-2F4C-4D78-8556-F1D28EE0F0A4}']
end;

[JavaSignature('android/view/ViewManager')]
JViewManager = interface(IJavaInstance)
['{8A913606-3A14-4346-A93B-4F6FF646A0F8}']
  {Methods}
  procedure addView(view: JView; params: JViewGroup_LayoutParams); cdecl;
  procedure removeView(view: JView); cdecl;
  procedure updateViewLayout(view: JView; params: JViewGroup_LayoutParams); cdecl;
end;
TJViewManager = class(TJavaGenericImport<JViewManagerClass, JViewManager>) end;

JWindowManagerClass = interface(JViewManagerClass)
['{13B7F7E9-A84B-4290-8C2E-3DB6C817A9C9}']
end;

[JavaSignature('android/view/WindowManager')]
JWindowManager = interface(JViewManager)
['{CE046C1F-687D-4438-B364-573A7FF59B26}']
  {Methods}
  function getDefaultDisplay: JDisplay; cdecl;
  procedure removeViewImmediate(view: JView); cdecl;
end;
TJWindowManager = class(TJavaGenericImport<JWindowManagerClass, JWindowManager>) end;

JPackageManagerClass = interface(JObjectClass)
['{E911B416-BD2A-4EE5-8F4F-519E086224CC}']
  {Property Methods}
  function _GetCOMPONENT_ENABLED_STATE_DEFAULT: Integer;
  function _GetCOMPONENT_ENABLED_STATE_DISABLED: Integer;
  function _GetCOMPONENT_ENABLED_STATE_DISABLED_USER: Integer;
  function _GetCOMPONENT_ENABLED_STATE_ENABLED: Integer;
  function _GetDONT_KILL_APP: Integer;
  function _GetEXTRA_VERIFICATION_ID: JString;
  function _GetEXTRA_VERIFICATION_RESULT: JString;
  function _GetFEATURE_AUDIO_LOW_LATENCY: JString;
  function _GetFEATURE_BLUETOOTH: JString;
  function _GetFEATURE_CAMERA: JString;
  function _GetFEATURE_CAMERA_ANY: JString;
  function _GetFEATURE_CAMERA_AUTOFOCUS: JString;
  function _GetFEATURE_CAMERA_FLASH: JString;
  function _GetFEATURE_CAMERA_FRONT: JString;
  function _GetFEATURE_FAKETOUCH: JString;
  function _GetFEATURE_FAKETOUCH_MULTITOUCH_DISTINCT: JString;
  function _GetFEATURE_FAKETOUCH_MULTITOUCH_JAZZHAND: JString;
  function _GetFEATURE_LIVE_WALLPAPER: JString;
  function _GetFEATURE_LOCATION: JString;
  function _GetFEATURE_LOCATION_GPS: JString;
  function _GetFEATURE_LOCATION_NETWORK: JString;
  function _GetFEATURE_MICROPHONE: JString;
  function _GetFEATURE_NFC: JString;
  function _GetFEATURE_SCREEN_LANDSCAPE: JString;
  function _GetFEATURE_SCREEN_PORTRAIT: JString;
  function _GetFEATURE_SENSOR_ACCELEROMETER: JString;
  function _GetFEATURE_SENSOR_BAROMETER: JString;
  function _GetFEATURE_SENSOR_COMPASS: JString;
  function _GetFEATURE_SENSOR_GYROSCOPE: JString;
  function _GetFEATURE_SENSOR_LIGHT: JString;
  function _GetFEATURE_SENSOR_PROXIMITY: JString;
  function _GetFEATURE_SIP: JString;
  function _GetFEATURE_SIP_VOIP: JString;
  function _GetFEATURE_TELEPHONY: JString;
  function _GetFEATURE_TELEPHONY_CDMA: JString;
  function _GetFEATURE_TELEPHONY_GSM: JString;
  function _GetFEATURE_TELEVISION: JString;
  function _GetFEATURE_TOUCHSCREEN: JString;
  function _GetFEATURE_TOUCHSCREEN_MULTITOUCH: JString;
  function _GetFEATURE_TOUCHSCREEN_MULTITOUCH_DISTINCT: JString;
  function _GetFEATURE_TOUCHSCREEN_MULTITOUCH_JAZZHAND: JString;
  function _GetFEATURE_USB_ACCESSORY: JString;
  function _GetFEATURE_USB_HOST: JString;
  function _GetFEATURE_WIFI: JString;
  function _GetFEATURE_WIFI_DIRECT: JString;
  function _GetGET_ACTIVITIES: Integer;
  function _GetGET_CONFIGURATIONS: Integer;
  function _GetGET_DISABLED_COMPONENTS: Integer;
  function _GetGET_GIDS: Integer;
  function _GetGET_INSTRUMENTATION: Integer;
  function _GetGET_INTENT_FILTERS: Integer;
  function _GetGET_META_DATA: Integer;
  function _GetGET_PERMISSIONS: Integer;
  function _GetGET_PROVIDERS: Integer;
  function _GetGET_RECEIVERS: Integer;
  function _GetGET_RESOLVED_FILTER: Integer;
  function _GetGET_SERVICES: Integer;
  function _GetGET_SHARED_LIBRARY_FILES: Integer;
  function _GetGET_SIGNATURES: Integer;
  function _GetGET_UNINSTALLED_PACKAGES: Integer;
  function _GetGET_URI_PERMISSION_PATTERNS: Integer;
  function _GetMATCH_DEFAULT_ONLY: Integer;
  function _GetMAXIMUM_VERIFICATION_TIMEOUT: Int64;
  function _GetPERMISSION_DENIED: Integer;
  function _GetPERMISSION_GRANTED: Integer;
  function _GetSIGNATURE_FIRST_NOT_SIGNED: Integer;
  function _GetSIGNATURE_MATCH: Integer;
  function _GetSIGNATURE_NEITHER_SIGNED: Integer;
  function _GetSIGNATURE_NO_MATCH: Integer;
  function _GetSIGNATURE_SECOND_NOT_SIGNED: Integer;
  function _GetSIGNATURE_UNKNOWN_PACKAGE: Integer;
  function _GetVERIFICATION_ALLOW: Integer;
  function _GetVERIFICATION_REJECT: Integer;
  {Methods}
  function init: JPackageManager; cdecl;
  {Properties}
  property COMPONENT_ENABLED_STATE_DEFAULT: Integer read _GetCOMPONENT_ENABLED_STATE_DEFAULT;
  property COMPONENT_ENABLED_STATE_DISABLED: Integer read _GetCOMPONENT_ENABLED_STATE_DISABLED;
  property COMPONENT_ENABLED_STATE_DISABLED_USER: Integer read _GetCOMPONENT_ENABLED_STATE_DISABLED_USER;
  property COMPONENT_ENABLED_STATE_ENABLED: Integer read _GetCOMPONENT_ENABLED_STATE_ENABLED;
  property DONT_KILL_APP: Integer read _GetDONT_KILL_APP;
  property EXTRA_VERIFICATION_ID: JString read _GetEXTRA_VERIFICATION_ID;
  property EXTRA_VERIFICATION_RESULT: JString read _GetEXTRA_VERIFICATION_RESULT;
  property FEATURE_AUDIO_LOW_LATENCY: JString read _GetFEATURE_AUDIO_LOW_LATENCY;
  property FEATURE_BLUETOOTH: JString read _GetFEATURE_BLUETOOTH;
  property FEATURE_CAMERA: JString read _GetFEATURE_CAMERA;
  property FEATURE_CAMERA_ANY: JString read _GetFEATURE_CAMERA_ANY;
  property FEATURE_CAMERA_AUTOFOCUS: JString read _GetFEATURE_CAMERA_AUTOFOCUS;
  property FEATURE_CAMERA_FLASH: JString read _GetFEATURE_CAMERA_FLASH;
  property FEATURE_CAMERA_FRONT: JString read _GetFEATURE_CAMERA_FRONT;
  property FEATURE_FAKETOUCH: JString read _GetFEATURE_FAKETOUCH;
  property FEATURE_FAKETOUCH_MULTITOUCH_DISTINCT: JString read _GetFEATURE_FAKETOUCH_MULTITOUCH_DISTINCT;
  property FEATURE_FAKETOUCH_MULTITOUCH_JAZZHAND: JString read _GetFEATURE_FAKETOUCH_MULTITOUCH_JAZZHAND;
  property FEATURE_LIVE_WALLPAPER: JString read _GetFEATURE_LIVE_WALLPAPER;
  property FEATURE_LOCATION: JString read _GetFEATURE_LOCATION;
  property FEATURE_LOCATION_GPS: JString read _GetFEATURE_LOCATION_GPS;
  property FEATURE_LOCATION_NETWORK: JString read _GetFEATURE_LOCATION_NETWORK;
  property FEATURE_MICROPHONE: JString read _GetFEATURE_MICROPHONE;
  property FEATURE_NFC: JString read _GetFEATURE_NFC;
  property FEATURE_SCREEN_LANDSCAPE: JString read _GetFEATURE_SCREEN_LANDSCAPE;
  property FEATURE_SCREEN_PORTRAIT: JString read _GetFEATURE_SCREEN_PORTRAIT;
  property FEATURE_SENSOR_ACCELEROMETER: JString read _GetFEATURE_SENSOR_ACCELEROMETER;
  property FEATURE_SENSOR_BAROMETER: JString read _GetFEATURE_SENSOR_BAROMETER;
  property FEATURE_SENSOR_COMPASS: JString read _GetFEATURE_SENSOR_COMPASS;
  property FEATURE_SENSOR_GYROSCOPE: JString read _GetFEATURE_SENSOR_GYROSCOPE;
  property FEATURE_SENSOR_LIGHT: JString read _GetFEATURE_SENSOR_LIGHT;
  property FEATURE_SENSOR_PROXIMITY: JString read _GetFEATURE_SENSOR_PROXIMITY;
  property FEATURE_SIP: JString read _GetFEATURE_SIP;
  property FEATURE_SIP_VOIP: JString read _GetFEATURE_SIP_VOIP;
  property FEATURE_TELEPHONY: JString read _GetFEATURE_TELEPHONY;
  property FEATURE_TELEPHONY_CDMA: JString read _GetFEATURE_TELEPHONY_CDMA;
  property FEATURE_TELEPHONY_GSM: JString read _GetFEATURE_TELEPHONY_GSM;
  property FEATURE_TELEVISION: JString read _GetFEATURE_TELEVISION;
  property FEATURE_TOUCHSCREEN: JString read _GetFEATURE_TOUCHSCREEN;
  property FEATURE_TOUCHSCREEN_MULTITOUCH: JString read _GetFEATURE_TOUCHSCREEN_MULTITOUCH;
  property FEATURE_TOUCHSCREEN_MULTITOUCH_DISTINCT: JString read _GetFEATURE_TOUCHSCREEN_MULTITOUCH_DISTINCT;
  property FEATURE_TOUCHSCREEN_MULTITOUCH_JAZZHAND: JString read _GetFEATURE_TOUCHSCREEN_MULTITOUCH_JAZZHAND;
  property FEATURE_USB_ACCESSORY: JString read _GetFEATURE_USB_ACCESSORY;
  property FEATURE_USB_HOST: JString read _GetFEATURE_USB_HOST;
  property FEATURE_WIFI: JString read _GetFEATURE_WIFI;
  property FEATURE_WIFI_DIRECT: JString read _GetFEATURE_WIFI_DIRECT;
  property GET_ACTIVITIES: Integer read _GetGET_ACTIVITIES;
  property GET_CONFIGURATIONS: Integer read _GetGET_CONFIGURATIONS;
  property GET_DISABLED_COMPONENTS: Integer read _GetGET_DISABLED_COMPONENTS;
  property GET_GIDS: Integer read _GetGET_GIDS;
  property GET_INSTRUMENTATION: Integer read _GetGET_INSTRUMENTATION;
  property GET_INTENT_FILTERS: Integer read _GetGET_INTENT_FILTERS;
  property GET_META_DATA: Integer read _GetGET_META_DATA;
  property GET_PERMISSIONS: Integer read _GetGET_PERMISSIONS;
  property GET_PROVIDERS: Integer read _GetGET_PROVIDERS;
  property GET_RECEIVERS: Integer read _GetGET_RECEIVERS;
  property GET_RESOLVED_FILTER: Integer read _GetGET_RESOLVED_FILTER;
  property GET_SERVICES: Integer read _GetGET_SERVICES;
  property GET_SHARED_LIBRARY_FILES: Integer read _GetGET_SHARED_LIBRARY_FILES;
  property GET_SIGNATURES: Integer read _GetGET_SIGNATURES;
  property GET_UNINSTALLED_PACKAGES: Integer read _GetGET_UNINSTALLED_PACKAGES;
  property GET_URI_PERMISSION_PATTERNS: Integer read _GetGET_URI_PERMISSION_PATTERNS;
  property MATCH_DEFAULT_ONLY: Integer read _GetMATCH_DEFAULT_ONLY;
  property MAXIMUM_VERIFICATION_TIMEOUT: Int64 read _GetMAXIMUM_VERIFICATION_TIMEOUT;
  property PERMISSION_DENIED: Integer read _GetPERMISSION_DENIED;
  property PERMISSION_GRANTED: Integer read _GetPERMISSION_GRANTED;
  property SIGNATURE_FIRST_NOT_SIGNED: Integer read _GetSIGNATURE_FIRST_NOT_SIGNED;
  property SIGNATURE_MATCH: Integer read _GetSIGNATURE_MATCH;
  property SIGNATURE_NEITHER_SIGNED: Integer read _GetSIGNATURE_NEITHER_SIGNED;
  property SIGNATURE_NO_MATCH: Integer read _GetSIGNATURE_NO_MATCH;
  property SIGNATURE_SECOND_NOT_SIGNED: Integer read _GetSIGNATURE_SECOND_NOT_SIGNED;
  property SIGNATURE_UNKNOWN_PACKAGE: Integer read _GetSIGNATURE_UNKNOWN_PACKAGE;
  property VERIFICATION_ALLOW: Integer read _GetVERIFICATION_ALLOW;
  property VERIFICATION_REJECT: Integer read _GetVERIFICATION_REJECT;
end;

[JavaSignature('android/content/pm/PackageManager')]
JPackageManager = interface(JObject)
['{9B00A726-E4BF-49E6-B802-AB17E4A5295F}']
  {Methods}
  procedure addPackageToPreferred(packageName: JString); cdecl;//Deprecated
  function addPermission(info: JPermissionInfo): Boolean; cdecl;
  function addPermissionAsync(info: JPermissionInfo): Boolean; cdecl;
  procedure addPreferredActivity(filter: JIntentFilter; match: Integer; set_: TJavaObjectArray<JComponentName>; activity: JComponentName); cdecl;//Deprecated
  function canonicalToCurrentPackageNames(names: TJavaObjectArray<JString>): TJavaObjectArray<JString>; cdecl;
  function checkPermission(permName: JString; pkgName: JString): Integer; cdecl;
  function checkSignatures(pkg1: JString; pkg2: JString): Integer; cdecl; overload;
  function checkSignatures(uid1: Integer; uid2: Integer): Integer; cdecl; overload;
  procedure clearPackagePreferredActivities(packageName: JString); cdecl;
  function currentToCanonicalPackageNames(names: TJavaObjectArray<JString>): TJavaObjectArray<JString>; cdecl;
  procedure extendVerificationTimeout(id: Integer; verificationCodeAtTimeout: Integer; millisecondsToDelay: Int64); cdecl;
  function getActivityIcon(activityName: JComponentName): JDrawable; cdecl; overload;
  function getActivityIcon(intent: JIntent): JDrawable; cdecl; overload;
  function getActivityInfo(component: JComponentName; flags: Integer): JActivityInfo; cdecl;
  function getActivityLogo(activityName: JComponentName): JDrawable; cdecl; overload;
  function getActivityLogo(intent: JIntent): JDrawable; cdecl; overload;
  function getAllPermissionGroups(flags: Integer): JList; cdecl;
  function getApplicationEnabledSetting(packageName: JString): Integer; cdecl;
  function getApplicationIcon(info: JApplicationInfo): JDrawable; cdecl; overload;
  function getApplicationIcon(packageName: JString): JDrawable; cdecl; overload;
  function getApplicationInfo(packageName: JString; flags: Integer): JApplicationInfo; cdecl;
  function getApplicationLabel(info: JApplicationInfo): JCharSequence; cdecl;
  function getApplicationLogo(info: JApplicationInfo): JDrawable; cdecl; overload;
  function getApplicationLogo(packageName: JString): JDrawable; cdecl; overload;
  function getComponentEnabledSetting(componentName: JComponentName): Integer; cdecl;
  function getDefaultActivityIcon: JDrawable; cdecl;
  function getDrawable(packageName: JString; resid: Integer; appInfo: JApplicationInfo): JDrawable; cdecl;
  function getInstalledApplications(flags: Integer): JList; cdecl;
  function getInstalledPackages(flags: Integer): JList; cdecl;
  function getInstallerPackageName(packageName: JString): JString; cdecl;
  function getInstrumentationInfo(className: JComponentName; flags: Integer): JInstrumentationInfo; cdecl;
  function getLaunchIntentForPackage(packageName: JString): JIntent; cdecl;
  function getNameForUid(uid: Integer): JString; cdecl;
  function getPackageArchiveInfo(archiveFilePath: JString; flags: Integer): JPackageInfo; cdecl;
  function getPackageGids(packageName: JString): TJavaArray<Integer>; cdecl;
  function getPackageInfo(packageName: JString; flags: Integer): JPackageInfo; cdecl;
  function getPackagesForUid(uid: Integer): TJavaObjectArray<JString>; cdecl;
  function getPermissionGroupInfo(name: JString; flags: Integer): JPermissionGroupInfo; cdecl;
  function getPermissionInfo(name: JString; flags: Integer): JPermissionInfo; cdecl;
  function getPreferredActivities(outFilters: JList; outActivities: JList; packageName: JString): Integer; cdecl;
  function getPreferredPackages(flags: Integer): JList; cdecl;
  function getProviderInfo(component: JComponentName; flags: Integer): JProviderInfo; cdecl;
  function getReceiverInfo(component: JComponentName; flags: Integer): JActivityInfo; cdecl;
  function getResourcesForActivity(activityName: JComponentName): JResources; cdecl;
  function getResourcesForApplication(app: JApplicationInfo): JResources; cdecl; overload;
  function getResourcesForApplication(appPackageName: JString): JResources; cdecl; overload;
  function getServiceInfo(component: JComponentName; flags: Integer): JServiceInfo; cdecl;
  function getSystemAvailableFeatures: TJavaObjectArray<JFeatureInfo>; cdecl;
  function getSystemSharedLibraryNames: TJavaObjectArray<JString>; cdecl;
  function getText(packageName: JString; resid: Integer; appInfo: JApplicationInfo): JCharSequence; cdecl;
  function getXml(packageName: JString; resid: Integer; appInfo: JApplicationInfo): JXmlResourceParser; cdecl;
  function hasSystemFeature(name: JString): Boolean; cdecl;
  function isSafeMode: Boolean; cdecl;
  function queryBroadcastReceivers(intent: JIntent; flags: Integer): JList; cdecl;
  function queryContentProviders(processName: JString; uid: Integer; flags: Integer): JList; cdecl;
  function queryInstrumentation(targetPackage: JString; flags: Integer): JList; cdecl;
  function queryIntentActivities(intent: JIntent; flags: Integer): JList; cdecl;
  function queryIntentActivityOptions(caller: JComponentName; specifics: TJavaObjectArray<JIntent>; intent: JIntent; flags: Integer): JList; cdecl;
  function queryIntentServices(intent: JIntent; flags: Integer): JList; cdecl;
  function queryPermissionsByGroup(group: JString; flags: Integer): JList; cdecl;
  procedure removePackageFromPreferred(packageName: JString); cdecl;//Deprecated
  procedure removePermission(name: JString); cdecl;
  function resolveActivity(intent: JIntent; flags: Integer): JResolveInfo; cdecl;
  function resolveContentProvider(name: JString; flags: Integer): JProviderInfo; cdecl;
  function resolveService(intent: JIntent; flags: Integer): JResolveInfo; cdecl;
  procedure setApplicationEnabledSetting(packageName: JString; newState: Integer; flags: Integer); cdecl;
  procedure setComponentEnabledSetting(componentName: JComponentName; newState: Integer; flags: Integer); cdecl;
  procedure setInstallerPackageName(targetPackage: JString; installerPackageName: JString); cdecl;
  procedure verifyPendingInstall(id: Integer; verificationCode: Integer); cdecl;
end;
TJPackageManager = class(TJavaGenericImport<JPackageManagerClass, JPackageManager>) end;

JEntityIteratorClass = interface(JIteratorClass)
['{FDF5F378-C146-4798-B825-C95E2259B445}']
end;

[JavaSignature('android/content/EntityIterator')]
JEntityIterator = interface(JIterator)
['{55F48181-11CC-4113-9930-C732F26A4814}']
  {Methods}
  procedure close; cdecl;
  procedure reset; cdecl;
end;
TJEntityIterator = class(TJavaGenericImport<JEntityIteratorClass, JEntityIterator>) end;

JInputBindingClass = interface(JObjectClass)
['{743151AD-B779-42A1-99B0-24965834D4D9}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Methods}
  function init(conn: JInputConnection; connToken: JIBinder; uid: Integer; pid: Integer): JInputBinding; cdecl; overload;
  function init(conn: JInputConnection; binding: JInputBinding): JInputBinding; cdecl; overload;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/view/inputmethod/InputBinding')]
JInputBinding = interface(JObject)
['{53198C86-E9C0-4F14-A6BC-B1C54246355F}']
  {Methods}
  function describeContents: Integer; cdecl;
  function getConnection: JInputConnection; cdecl;
  function getConnectionToken: JIBinder; cdecl;
  function getPid: Integer; cdecl;
  function getUid: Integer; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
end;
TJInputBinding = class(TJavaGenericImport<JInputBindingClass, JInputBinding>) end;

JDrawable_ConstantStateClass = interface(JObjectClass)
['{897FEAC3-7E54-4684-98E0-E4EA096D3FD0}']
  {Methods}
  function init: JDrawable_ConstantState; cdecl;
end;

[JavaSignature('android/graphics/drawable/Drawable$ConstantState')]
JDrawable_ConstantState = interface(JObject)
['{A56FEC85-8E9B-4879-AE79-FA588A0702C0}']
  {Methods}
  function getChangingConfigurations: Integer; cdecl;
  function newDrawable: JDrawable; cdecl; overload;
  function newDrawable(res: JResources): JDrawable; cdecl; overload;
end;
TJDrawable_ConstantState = class(TJavaGenericImport<JDrawable_ConstantStateClass, JDrawable_ConstantState>) end;

JPath_DirectionClass = interface(JEnumClass)
['{378CF43B-67A5-4193-946C-27B290D70734}']
  {Property Methods}
  function _GetCCW: JPath_Direction;
  function _GetCW: JPath_Direction;
  {Methods}
  function valueOf(name: JString): JPath_Direction; cdecl;
  function values: TJavaObjectArray<JPath_Direction>; cdecl;
  {Properties}
  property CCW: JPath_Direction read _GetCCW;
  property CW: JPath_Direction read _GetCW;
end;

[JavaSignature('android/graphics/Path$Direction')]
JPath_Direction = interface(JEnum)
['{74028FFA-4405-47D1-BB35-89BE6503E395}']
end;
TJPath_Direction = class(TJavaGenericImport<JPath_DirectionClass, JPath_Direction>) end;

JBitmapDrawableClass = interface(JDrawableClass)
['{8DD36F5B-B87C-45AA-A897-6740ED35CFFC}']
  {Methods}
  function init: JBitmapDrawable; cdecl; overload;//Deprecated
  function init(res: JResources): JBitmapDrawable; cdecl; overload;
  function init(bitmap: JBitmap): JBitmapDrawable; cdecl; overload;//Deprecated
  function init(res: JResources; bitmap: JBitmap): JBitmapDrawable; cdecl; overload;
  function init(filepath: JString): JBitmapDrawable; cdecl; overload;//Deprecated
  function init(res: JResources; filepath: JString): JBitmapDrawable; cdecl; overload;
  function init(is_: JInputStream): JBitmapDrawable; cdecl; overload;//Deprecated
  function init(res: JResources; is_: JInputStream): JBitmapDrawable; cdecl; overload;
end;

[JavaSignature('android/graphics/drawable/BitmapDrawable')]
JBitmapDrawable = interface(JDrawable)
['{D04741C7-39A6-454D-B77D-4E90CCB79D47}']
  {Methods}
  procedure draw(canvas: JCanvas); cdecl;
  function getBitmap: JBitmap; cdecl;
  function getChangingConfigurations: Integer; cdecl;
  function getConstantState: JDrawable_ConstantState; cdecl;
  function getGravity: Integer; cdecl;
  function getIntrinsicHeight: Integer; cdecl;
  function getIntrinsicWidth: Integer; cdecl;
  function getOpacity: Integer; cdecl;
  function getPaint: JPaint; cdecl;
  function getTileModeX: JShader_TileMode; cdecl;
  function getTileModeY: JShader_TileMode; cdecl;
  function mutate: JDrawable; cdecl;
  procedure setAlpha(alpha: Integer); cdecl;
  procedure setAntiAlias(aa: Boolean); cdecl;
  procedure setColorFilter(cf: JColorFilter); cdecl;
  procedure setDither(dither: Boolean); cdecl;
  procedure setFilterBitmap(filter: Boolean); cdecl;
  procedure setGravity(gravity: Integer); cdecl;
  procedure setTargetDensity(canvas: JCanvas); cdecl; overload;
  procedure setTargetDensity(metrics: JDisplayMetrics); cdecl; overload;
  procedure setTargetDensity(density: Integer); cdecl; overload;
  procedure setTileModeX(mode: JShader_TileMode); cdecl;
  procedure setTileModeXY(xmode: JShader_TileMode; ymode: JShader_TileMode); cdecl;
  procedure setTileModeY(mode: JShader_TileMode); cdecl;
end;
TJBitmapDrawable = class(TJavaGenericImport<JBitmapDrawableClass, JBitmapDrawable>) end;

JScaleDrawableClass = interface(JDrawableClass)
['{C722EF4C-58E6-4510-A19C-E43032F02344}']
  {Methods}
  function init(drawable: JDrawable; gravity: Integer; scaleWidth: Single; scaleHeight: Single): JScaleDrawable; cdecl;
end;

[JavaSignature('android/graphics/drawable/ScaleDrawable')]
JScaleDrawable = interface(JDrawable)
['{2915F20F-256D-4428-8424-34412F11AAF2}']
  {Methods}
  procedure draw(canvas: JCanvas); cdecl;
  function getChangingConfigurations: Integer; cdecl;
  function getConstantState: JDrawable_ConstantState; cdecl;
  function getDrawable: JDrawable; cdecl;
  function getIntrinsicHeight: Integer; cdecl;
  function getIntrinsicWidth: Integer; cdecl;
  function getOpacity: Integer; cdecl;
  function getPadding(padding: JRect): Boolean; cdecl;
  procedure invalidateDrawable(who: JDrawable); cdecl;
  function isStateful: Boolean; cdecl;
  function mutate: JDrawable; cdecl;
  procedure scheduleDrawable(who: JDrawable; what: JRunnable; when: Int64); cdecl;
  procedure setAlpha(alpha: Integer); cdecl;
  procedure setColorFilter(cf: JColorFilter); cdecl;
  function setVisible(visible: Boolean; restart: Boolean): Boolean; cdecl;
  procedure unscheduleDrawable(who: JDrawable; what: JRunnable); cdecl;
end;
TJScaleDrawable = class(TJavaGenericImport<JScaleDrawableClass, JScaleDrawable>) end;

JContextThemeWrapperClass = interface(JContextWrapperClass)
['{E4B006F8-E355-4D54-B94B-D17758D93102}']
  {Methods}
  function init: JContextThemeWrapper; cdecl; overload;
  function init(base: JContext; themeres: Integer): JContextThemeWrapper; cdecl; overload;
end;

[JavaSignature('android/view/ContextThemeWrapper')]
JContextThemeWrapper = interface(JContextWrapper)
['{68840867-EC00-40D8-9130-022454F26948}']
  {Methods}
  procedure applyOverrideConfiguration(overrideConfiguration: JConfiguration); cdecl;
  function getResources: JResources; cdecl;
  function getSystemService(name: JString): JObject; cdecl;
  function getTheme: JResources_Theme; cdecl;
  procedure setTheme(resid: Integer); cdecl;
end;
TJContextThemeWrapper = class(TJavaGenericImport<JContextThemeWrapperClass, JContextThemeWrapper>) end;

JView_OnKeyListenerClass = interface(IJavaClass)
['{53AEAB4F-2C34-494D-9C5D-E98026DBFF9A}']
end;

[JavaSignature('android/view/View$OnKeyListener')]
JView_OnKeyListener = interface(IJavaInstance)
['{4E271738-0766-458B-883B-721CFD9F1DE6}']
  {Methods}
  function onKey(v: JView; keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
end;
TJView_OnKeyListener = class(TJavaGenericImport<JView_OnKeyListenerClass, JView_OnKeyListener>) end;

JMenuItemClass = interface(IJavaClass)
['{211E0ADC-2FE6-4521-8B8E-1BA046C7795D}']
  {Property Methods}
  function _GetSHOW_AS_ACTION_ALWAYS: Integer;
  function _GetSHOW_AS_ACTION_COLLAPSE_ACTION_VIEW: Integer;
  function _GetSHOW_AS_ACTION_IF_ROOM: Integer;
  function _GetSHOW_AS_ACTION_NEVER: Integer;
  function _GetSHOW_AS_ACTION_WITH_TEXT: Integer;
  {Properties}
  property SHOW_AS_ACTION_ALWAYS: Integer read _GetSHOW_AS_ACTION_ALWAYS;
  property SHOW_AS_ACTION_COLLAPSE_ACTION_VIEW: Integer read _GetSHOW_AS_ACTION_COLLAPSE_ACTION_VIEW;
  property SHOW_AS_ACTION_IF_ROOM: Integer read _GetSHOW_AS_ACTION_IF_ROOM;
  property SHOW_AS_ACTION_NEVER: Integer read _GetSHOW_AS_ACTION_NEVER;
  property SHOW_AS_ACTION_WITH_TEXT: Integer read _GetSHOW_AS_ACTION_WITH_TEXT;
end;

[JavaSignature('android/view/MenuItem')]
JMenuItem = interface(IJavaInstance)
['{C8611D60-549B-49E7-A560-1D25F0294587}']
  {Methods}
  function collapseActionView: Boolean; cdecl;
  function expandActionView: Boolean; cdecl;
  function getActionProvider: JActionProvider; cdecl;
  function getActionView: JView; cdecl;
  function getAlphabeticShortcut: Char; cdecl;
  function getGroupId: Integer; cdecl;
  function getIcon: JDrawable; cdecl;
  function getIntent: JIntent; cdecl;
  function getItemId: Integer; cdecl;
  function getMenuInfo: JContextMenu_ContextMenuInfo; cdecl;
  function getNumericShortcut: Char; cdecl;
  function getOrder: Integer; cdecl;
  function getSubMenu: JSubMenu; cdecl;
  function getTitle: JCharSequence; cdecl;
  function getTitleCondensed: JCharSequence; cdecl;
  function hasSubMenu: Boolean; cdecl;
  function isActionViewExpanded: Boolean; cdecl;
  function isCheckable: Boolean; cdecl;
  function isChecked: Boolean; cdecl;
  function isEnabled: Boolean; cdecl;
  function isVisible: Boolean; cdecl;
  function setActionProvider(actionProvider: JActionProvider): JMenuItem; cdecl;
  function setActionView(view: JView): JMenuItem; cdecl; overload;
  function setActionView(resId: Integer): JMenuItem; cdecl; overload;
  function setAlphabeticShortcut(alphaChar: Char): JMenuItem; cdecl;
  function setCheckable(checkable: Boolean): JMenuItem; cdecl;
  function setChecked(checked: Boolean): JMenuItem; cdecl;
  function setEnabled(enabled: Boolean): JMenuItem; cdecl;
  function setIcon(icon: JDrawable): JMenuItem; cdecl; overload;
  function setIcon(iconRes: Integer): JMenuItem; cdecl; overload;
  function setIntent(intent: JIntent): JMenuItem; cdecl;
  function setNumericShortcut(numericChar: Char): JMenuItem; cdecl;
  function setOnActionExpandListener(listener: JMenuItem_OnActionExpandListener): JMenuItem; cdecl;
  function setOnMenuItemClickListener(menuItemClickListener: JMenuItem_OnMenuItemClickListener): JMenuItem; cdecl;
  function setShortcut(numericChar: Char; alphaChar: Char): JMenuItem; cdecl;
  procedure setShowAsAction(actionEnum: Integer); cdecl;
  function setShowAsActionFlags(actionEnum: Integer): JMenuItem; cdecl;
  function setTitle(title: JCharSequence): JMenuItem; cdecl; overload;
  function setTitle(title: Integer): JMenuItem; cdecl; overload;
  function setTitleCondensed(title: JCharSequence): JMenuItem; cdecl;
  function setVisible(visible: Boolean): JMenuItem; cdecl;
end;
TJMenuItem = class(TJavaGenericImport<JMenuItemClass, JMenuItem>) end;

JView_OnCreateContextMenuListenerClass = interface(IJavaClass)
['{3AA65EA5-116C-4AB1-9793-A1280304879E}']
end;

[JavaSignature('android/view/View$OnCreateContextMenuListener')]
JView_OnCreateContextMenuListener = interface(IJavaInstance)
['{03A19F11-8D70-4543-87C7-BD792FF23925}']
  {Methods}
  procedure onCreateContextMenu(menu: JContextMenu; v: JView; menuInfo: JContextMenu_ContextMenuInfo); cdecl;
end;
TJView_OnCreateContextMenuListener = class(TJavaGenericImport<JView_OnCreateContextMenuListenerClass, JView_OnCreateContextMenuListener>) end;

JIntentFilter_AuthorityEntryClass = interface(JObjectClass)
['{0B3684C3-E5D3-4DB6-9FE5-E66579E5C4B4}']
  {Methods}
  function init(host: JString; port: JString): JIntentFilter_AuthorityEntry; cdecl;
end;

[JavaSignature('android/content/IntentFilter$AuthorityEntry')]
JIntentFilter_AuthorityEntry = interface(JObject)
['{25FAFB4D-95BB-4D76-99CC-E995B61B1ED0}']
  {Methods}
  function getHost: JString; cdecl;
  function getPort: Integer; cdecl;
  function match(data: Jnet_Uri): Integer; cdecl;
end;
TJIntentFilter_AuthorityEntry = class(TJavaGenericImport<JIntentFilter_AuthorityEntryClass, JIntentFilter_AuthorityEntry>) end;

JGestureDetectorClass = interface(JObjectClass)
['{4CF183C0-64F2-4604-A7B5-0AA713A2F86E}']
  {Methods}
  function init(listener: JGestureDetector_OnGestureListener; handler: JHandler): JGestureDetector; cdecl; overload;//Deprecated
  function init(listener: JGestureDetector_OnGestureListener): JGestureDetector; cdecl; overload;//Deprecated
  function init(context: JContext; listener: JGestureDetector_OnGestureListener): JGestureDetector; cdecl; overload;
  function init(context: JContext; listener: JGestureDetector_OnGestureListener; handler: JHandler): JGestureDetector; cdecl; overload;
  function init(context: JContext; listener: JGestureDetector_OnGestureListener; handler: JHandler; unused: Boolean): JGestureDetector; cdecl; overload;
end;

[JavaSignature('android/view/GestureDetector')]
JGestureDetector = interface(JObject)
['{4897CF85-CC0C-4A98-BB19-E55262ECE992}']
  {Methods}
  function isLongpressEnabled: Boolean; cdecl;
  function onTouchEvent(ev: JMotionEvent): Boolean; cdecl;
  procedure setIsLongpressEnabled(isLongpressEnabled: Boolean); cdecl;
  procedure setOnDoubleTapListener(onDoubleTapListener: JGestureDetector_OnDoubleTapListener); cdecl;
end;
TJGestureDetector = class(TJavaGenericImport<JGestureDetectorClass, JGestureDetector>) end;

JShapeDrawable_ShaderFactoryClass = interface(JObjectClass)
['{1169F195-175A-4AAC-B8B5-F300F63591A2}']
  {Methods}
  function init: JShapeDrawable_ShaderFactory; cdecl;
end;

[JavaSignature('android/graphics/drawable/ShapeDrawable$ShaderFactory')]
JShapeDrawable_ShaderFactory = interface(JObject)
['{F4735750-5E22-4BC1-8F4E-235457EFD9EB}']
  {Methods}
  function resize(width: Integer; height: Integer): JShader; cdecl;
end;
TJShapeDrawable_ShaderFactory = class(TJavaGenericImport<JShapeDrawable_ShaderFactoryClass, JShapeDrawable_ShaderFactory>) end;

JMenuInflaterClass = interface(JObjectClass)
['{0B821491-17EC-4FA7-9BD8-A52F15EBA066}']
  {Methods}
  function init(context: JContext): JMenuInflater; cdecl;
end;

[JavaSignature('android/view/MenuInflater')]
JMenuInflater = interface(JObject)
['{D8483B67-5688-41FE-98D1-4FE7765FCE12}']
  {Methods}
  procedure inflate(menuRes: Integer; menu: JMenu); cdecl;
end;
TJMenuInflater = class(TJavaGenericImport<JMenuInflaterClass, JMenuInflater>) end;

JView_OnTouchListenerClass = interface(IJavaClass)
['{C7C4B1E0-BBE6-464C-BF96-54DF9750D103}']
end;

[JavaSignature('android/view/View$OnTouchListener')]
JView_OnTouchListener = interface(IJavaInstance)
['{CE4A1789-BAF5-4C37-9A87-54BF5842504B}']
  {Methods}
  function onTouch(v: JView; event: JMotionEvent): Boolean; cdecl;
end;
TJView_OnTouchListener = class(TJavaGenericImport<JView_OnTouchListenerClass, JView_OnTouchListener>) end;

JInsetDrawableClass = interface(JDrawableClass)
['{F5DBFF6F-2A58-4EAA-B193-3886578E6BCA}']
  {Methods}
  function init(drawable: JDrawable; inset: Integer): JInsetDrawable; cdecl; overload;
  function init(drawable: JDrawable; insetLeft: Integer; insetTop: Integer; insetRight: Integer; insetBottom: Integer): JInsetDrawable; cdecl; overload;
end;

[JavaSignature('android/graphics/drawable/InsetDrawable')]
JInsetDrawable = interface(JDrawable)
['{AD1B3CB1-5EFC-46B4-A58E-FC28A37A77DE}']
  {Methods}
  procedure draw(canvas: JCanvas); cdecl;
  function getChangingConfigurations: Integer; cdecl;
  function getConstantState: JDrawable_ConstantState; cdecl;
  function getIntrinsicHeight: Integer; cdecl;
  function getIntrinsicWidth: Integer; cdecl;
  function getOpacity: Integer; cdecl;
  function getPadding(padding: JRect): Boolean; cdecl;
  procedure invalidateDrawable(who: JDrawable); cdecl;
  function isStateful: Boolean; cdecl;
  function mutate: JDrawable; cdecl;
  procedure scheduleDrawable(who: JDrawable; what: JRunnable; when: Int64); cdecl;
  procedure setAlpha(alpha: Integer); cdecl;
  procedure setColorFilter(cf: JColorFilter); cdecl;
  function setVisible(visible: Boolean; restart: Boolean): Boolean; cdecl;
  procedure unscheduleDrawable(who: JDrawable; what: JRunnable); cdecl;
end;
TJInsetDrawable = class(TJavaGenericImport<JInsetDrawableClass, JInsetDrawable>) end;

JDatabaseErrorHandlerClass = interface(IJavaClass)
['{D41A8571-A0DA-4DEA-A665-9F1F7404A17B}']
end;

[JavaSignature('android/database/DatabaseErrorHandler')]
JDatabaseErrorHandler = interface(IJavaInstance)
['{A31F3725-966F-4228-B86A-C8B8A876E3C8}']
  {Methods}
  procedure onCorruption(dbObj: JSQLiteDatabase); cdecl;
end;
TJDatabaseErrorHandler = class(TJavaGenericImport<JDatabaseErrorHandlerClass, JDatabaseErrorHandler>) end;

JDialogInterface_OnMultiChoiceClickListenerClass = interface(IJavaClass)
['{0B3FADA0-DBF9-43B2-BDEC-0B121B4AA408}']
end;

[JavaSignature('android/content/DialogInterface$OnMultiChoiceClickListener')]
JDialogInterface_OnMultiChoiceClickListener = interface(IJavaInstance)
['{1BA26A6B-931A-4FC3-AB72-898AD26FC476}']
  {Methods}
  procedure onClick(dialog: JDialogInterface; which: Integer; isChecked: Boolean); cdecl;
end;
TJDialogInterface_OnMultiChoiceClickListener = class(TJavaGenericImport<JDialogInterface_OnMultiChoiceClickListenerClass, JDialogInterface_OnMultiChoiceClickListener>) end;

JInputMethodSessionClass = interface(IJavaClass)
['{AB35CFBC-A6CC-4235-84A0-848A44DC9A9E}']
end;

[JavaSignature('android/view/inputmethod/InputMethodSession')]
JInputMethodSession = interface(IJavaInstance)
['{7F038217-AC6A-4F44-B0DC-4C9D564BBE06}']
  {Methods}
  procedure appPrivateCommand(action: JString; data: JBundle); cdecl;
  procedure dispatchGenericMotionEvent(seq: Integer; event: JMotionEvent; callback: JInputMethodSession_EventCallback); cdecl;
  procedure dispatchKeyEvent(seq: Integer; event: JKeyEvent; callback: JInputMethodSession_EventCallback); cdecl;
  procedure dispatchTrackballEvent(seq: Integer; event: JMotionEvent; callback: JInputMethodSession_EventCallback); cdecl;
  procedure displayCompletions(completions: TJavaObjectArray<JCompletionInfo>); cdecl;
  procedure finishInput; cdecl;
  procedure toggleSoftInput(showFlags: Integer; hideFlags: Integer); cdecl;
  procedure updateCursor(newCursor: JRect); cdecl;
  procedure updateExtractedText(token: Integer; text: JExtractedText); cdecl;
  procedure updateSelection(oldSelStart: Integer; oldSelEnd: Integer; newSelStart: Integer; newSelEnd: Integer; candidatesStart: Integer; candidatesEnd: Integer); cdecl;
  procedure viewClicked(focusChanged: Boolean); cdecl;
end;
TJInputMethodSession = class(TJavaGenericImport<JInputMethodSessionClass, JInputMethodSession>) end;

JMatrixClass = interface(JObjectClass)
['{91EA9B3A-8F52-4413-84F4-8B75BDD6EC25}']
  {Property Methods}
  function _GetMPERSP_0: Integer;
  function _GetMPERSP_1: Integer;
  function _GetMPERSP_2: Integer;
  function _GetMSCALE_X: Integer;
  function _GetMSCALE_Y: Integer;
  function _GetMSKEW_X: Integer;
  function _GetMSKEW_Y: Integer;
  function _GetMTRANS_X: Integer;
  function _GetMTRANS_Y: Integer;
  {Methods}
  function init: JMatrix; cdecl; overload;
  function init(src: JMatrix): JMatrix; cdecl; overload;
  {Properties}
  property MPERSP_0: Integer read _GetMPERSP_0;
  property MPERSP_1: Integer read _GetMPERSP_1;
  property MPERSP_2: Integer read _GetMPERSP_2;
  property MSCALE_X: Integer read _GetMSCALE_X;
  property MSCALE_Y: Integer read _GetMSCALE_Y;
  property MSKEW_X: Integer read _GetMSKEW_X;
  property MSKEW_Y: Integer read _GetMSKEW_Y;
  property MTRANS_X: Integer read _GetMTRANS_X;
  property MTRANS_Y: Integer read _GetMTRANS_Y;
end;

[JavaSignature('android/graphics/Matrix')]
JMatrix = interface(JObject)
['{FF273EAF-C0C2-46D0-9D40-356C370EAF43}']
  {Methods}
  function equals(obj: JObject): Boolean; cdecl;
  procedure getValues(values: TJavaArray<Single>); cdecl;
  function invert(inverse: JMatrix): Boolean; cdecl;
  function isIdentity: Boolean; cdecl;
  procedure mapPoints(dst: TJavaArray<Single>; dstIndex: Integer; src: TJavaArray<Single>; srcIndex: Integer; pointCount: Integer); cdecl; overload;
  procedure mapPoints(dst: TJavaArray<Single>; src: TJavaArray<Single>); cdecl; overload;
  procedure mapPoints(pts: TJavaArray<Single>); cdecl; overload;
  function mapRadius(radius: Single): Single; cdecl;
  function mapRect(dst: JRectF; src: JRectF): Boolean; cdecl; overload;
  function mapRect(rect: JRectF): Boolean; cdecl; overload;
  procedure mapVectors(dst: TJavaArray<Single>; dstIndex: Integer; src: TJavaArray<Single>; srcIndex: Integer; vectorCount: Integer); cdecl; overload;
  procedure mapVectors(dst: TJavaArray<Single>; src: TJavaArray<Single>); cdecl; overload;
  procedure mapVectors(vecs: TJavaArray<Single>); cdecl; overload;
  function postConcat(other: JMatrix): Boolean; cdecl;
  function postRotate(degrees: Single; px: Single; py: Single): Boolean; cdecl; overload;
  function postRotate(degrees: Single): Boolean; cdecl; overload;
  function postScale(sx: Single; sy: Single; px: Single; py: Single): Boolean; cdecl; overload;
  function postScale(sx: Single; sy: Single): Boolean; cdecl; overload;
  function postSkew(kx: Single; ky: Single; px: Single; py: Single): Boolean; cdecl; overload;
  function postSkew(kx: Single; ky: Single): Boolean; cdecl; overload;
  function postTranslate(dx: Single; dy: Single): Boolean; cdecl;
  function preConcat(other: JMatrix): Boolean; cdecl;
  function preRotate(degrees: Single; px: Single; py: Single): Boolean; cdecl; overload;
  function preRotate(degrees: Single): Boolean; cdecl; overload;
  function preScale(sx: Single; sy: Single; px: Single; py: Single): Boolean; cdecl; overload;
  function preScale(sx: Single; sy: Single): Boolean; cdecl; overload;
  function preSkew(kx: Single; ky: Single; px: Single; py: Single): Boolean; cdecl; overload;
  function preSkew(kx: Single; ky: Single): Boolean; cdecl; overload;
  function preTranslate(dx: Single; dy: Single): Boolean; cdecl;
  function rectStaysRect: Boolean; cdecl;
  procedure reset; cdecl;
  procedure &set(src: JMatrix); cdecl;
  function setConcat(a: JMatrix; b: JMatrix): Boolean; cdecl;
  function setPolyToPoly(src: TJavaArray<Single>; srcIndex: Integer; dst: TJavaArray<Single>; dstIndex: Integer; pointCount: Integer): Boolean; cdecl;
  function setRectToRect(src: JRectF; dst: JRectF; stf: JMatrix_ScaleToFit): Boolean; cdecl;
  procedure setRotate(degrees: Single; px: Single; py: Single); cdecl; overload;
  procedure setRotate(degrees: Single); cdecl; overload;
  procedure setScale(sx: Single; sy: Single; px: Single; py: Single); cdecl; overload;
  procedure setScale(sx: Single; sy: Single); cdecl; overload;
  procedure setSinCos(sinValue: Single; cosValue: Single; px: Single; py: Single); cdecl; overload;
  procedure setSinCos(sinValue: Single; cosValue: Single); cdecl; overload;
  procedure setSkew(kx: Single; ky: Single; px: Single; py: Single); cdecl; overload;
  procedure setSkew(kx: Single; ky: Single); cdecl; overload;
  procedure setTranslate(dx: Single; dy: Single); cdecl;
  procedure setValues(values: TJavaArray<Single>); cdecl;
  function toShortString: JString; cdecl;
  function toString: JString; cdecl;
end;
TJMatrix = class(TJavaGenericImport<JMatrixClass, JMatrix>) end;

JDrawableContainerClass = interface(JDrawableClass)
['{DA9B07EB-62A2-42BC-A734-B8353978095A}']
  {Methods}
  function init: JDrawableContainer; cdecl;
end;

[JavaSignature('android/graphics/drawable/DrawableContainer')]
JDrawableContainer = interface(JDrawable)
['{69501A34-A3A1-4738-BF7E-756C55E465EA}']
  {Methods}
  procedure draw(canvas: JCanvas); cdecl;
  function getChangingConfigurations: Integer; cdecl;
  function getConstantState: JDrawable_ConstantState; cdecl;
  function getCurrent: JDrawable; cdecl;
  function getIntrinsicHeight: Integer; cdecl;
  function getIntrinsicWidth: Integer; cdecl;
  function getMinimumHeight: Integer; cdecl;
  function getMinimumWidth: Integer; cdecl;
  function getOpacity: Integer; cdecl;
  function getPadding(padding: JRect): Boolean; cdecl;
  procedure invalidateDrawable(who: JDrawable); cdecl;
  function isStateful: Boolean; cdecl;
  procedure jumpToCurrentState; cdecl;
  function mutate: JDrawable; cdecl;
  procedure scheduleDrawable(who: JDrawable; what: JRunnable; when: Int64); cdecl;
  function selectDrawable(idx: Integer): Boolean; cdecl;
  procedure setAlpha(alpha: Integer); cdecl;
  procedure setColorFilter(cf: JColorFilter); cdecl;
  procedure setDither(dither: Boolean); cdecl;
  procedure setEnterFadeDuration(ms: Integer); cdecl;
  procedure setExitFadeDuration(ms: Integer); cdecl;
  function setVisible(visible: Boolean; restart: Boolean): Boolean; cdecl;
  procedure unscheduleDrawable(who: JDrawable; what: JRunnable); cdecl;
end;
TJDrawableContainer = class(TJavaGenericImport<JDrawableContainerClass, JDrawableContainer>) end;

JAnimationDrawableClass = interface(JDrawableContainerClass)
['{5ADA9FF7-EF36-40DF-B080-295545E7A42F}']
  {Methods}
  function init: JAnimationDrawable; cdecl;
end;

[JavaSignature('android/graphics/drawable/AnimationDrawable')]
JAnimationDrawable = interface(JDrawableContainer)
['{89D46091-A136-4579-886D-0F5F99695146}']
  {Methods}
  procedure addFrame(frame: JDrawable; duration: Integer); cdecl;
  function getDuration(i: Integer): Integer; cdecl;
  function getFrame(index: Integer): JDrawable; cdecl;
  function getNumberOfFrames: Integer; cdecl;
  function isOneShot: Boolean; cdecl;
  function isRunning: Boolean; cdecl;
  function mutate: JDrawable; cdecl;
  procedure run; cdecl;
  procedure setOneShot(oneShot: Boolean); cdecl;
  function setVisible(visible: Boolean; restart: Boolean): Boolean; cdecl;
  procedure start; cdecl;
  procedure stop; cdecl;
  procedure unscheduleSelf(what: JRunnable); cdecl;
end;
TJAnimationDrawable = class(TJavaGenericImport<JAnimationDrawableClass, JAnimationDrawable>) end;

JPointClass = interface(JObjectClass)
['{61361EC3-509A-4566-BB60-B66834DB7612}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Methods}
  function init: JPoint; cdecl; overload;
  function init(x: Integer; y: Integer): JPoint; cdecl; overload;
  function init(src: JPoint): JPoint; cdecl; overload;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/graphics/Point')]
JPoint = interface(JObject)
['{E85805BB-5295-4924-A804-07D93814DD1E}']
  {Property Methods}
  function _Getx: Integer;
  procedure _Setx(Value: Integer);
  function _Gety: Integer;
  procedure _Sety(Value: Integer);
  {Methods}
  function describeContents: Integer; cdecl;
  function equals(x: Integer; y: Integer): Boolean; cdecl; overload;
  function equals(o: JObject): Boolean; cdecl; overload;
  function hashCode: Integer; cdecl;
  procedure negate; cdecl;
  procedure offset(dx: Integer; dy: Integer); cdecl;
  procedure readFromParcel(in_: JParcel); cdecl;
  procedure &set(x: Integer; y: Integer); cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
  {Properties}
  property x: Integer read _Getx write _Setx;
  property y: Integer read _Gety write _Sety;
end;
TJPoint = class(TJavaGenericImport<JPointClass, JPoint>) end;

JShapeDrawableClass = interface(JDrawableClass)
['{127DBE9F-4758-471F-9874-CA5C349BBCFD}']
  {Methods}
  function init: JShapeDrawable; cdecl; overload;
  function init(s: JShape): JShapeDrawable; cdecl; overload;
end;

[JavaSignature('android/graphics/drawable/ShapeDrawable')]
JShapeDrawable = interface(JDrawable)
['{4FEC3B0A-E214-4AF5-8143-93CCC01803EF}']
  {Methods}
  procedure draw(canvas: JCanvas); cdecl;
  function getChangingConfigurations: Integer; cdecl;
  function getConstantState: JDrawable_ConstantState; cdecl;
  function getIntrinsicHeight: Integer; cdecl;
  function getIntrinsicWidth: Integer; cdecl;
  function getOpacity: Integer; cdecl;
  function getPadding(padding: JRect): Boolean; cdecl;
  function getPaint: JPaint; cdecl;
  function getShaderFactory: JShapeDrawable_ShaderFactory; cdecl;
  function getShape: JShape; cdecl;
  function mutate: JDrawable; cdecl;
  procedure setAlpha(alpha: Integer); cdecl;
  procedure setColorFilter(cf: JColorFilter); cdecl;
  procedure setDither(dither: Boolean); cdecl;
  procedure setIntrinsicHeight(height: Integer); cdecl;
  procedure setIntrinsicWidth(width: Integer); cdecl;
  procedure setPadding(left: Integer; top: Integer; right: Integer; bottom: Integer); cdecl; overload;
  procedure setPadding(padding: JRect); cdecl; overload;
  procedure setShaderFactory(fact: JShapeDrawable_ShaderFactory); cdecl;
  procedure setShape(s: JShape); cdecl;
end;
TJShapeDrawable = class(TJavaGenericImport<JShapeDrawableClass, JShapeDrawable>) end;

JExtractedTextRequestClass = interface(JObjectClass)
['{440AAAA9-C797-4705-B105-0782D243E191}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Methods}
  function init: JExtractedTextRequest; cdecl;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/view/inputmethod/ExtractedTextRequest')]
JExtractedTextRequest = interface(JObject)
['{F26402AD-9FFC-428F-9FAB-2445588990AB}']
  {Property Methods}
  function _Getflags: Integer;
  procedure _Setflags(Value: Integer);
  function _GethintMaxChars: Integer;
  procedure _SethintMaxChars(Value: Integer);
  function _GethintMaxLines: Integer;
  procedure _SethintMaxLines(Value: Integer);
  function _Gettoken: Integer;
  procedure _Settoken(Value: Integer);
  {Methods}
  function describeContents: Integer; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  {Properties}
  property flags: Integer read _Getflags write _Setflags;
  property hintMaxChars: Integer read _GethintMaxChars write _SethintMaxChars;
  property hintMaxLines: Integer read _GethintMaxLines write _SethintMaxLines;
  property token: Integer read _Gettoken write _Settoken;
end;
TJExtractedTextRequest = class(TJavaGenericImport<JExtractedTextRequestClass, JExtractedTextRequest>) end;

JColorFilterClass = interface(JObjectClass)
['{CD0422E3-73FA-411C-9DA1-BE5349A34965}']
  {Methods}
  function init: JColorFilter; cdecl;
end;

[JavaSignature('android/graphics/ColorFilter')]
JColorFilter = interface(JObject)
['{03501DF4-6680-4152-AA2F-231A97E7EB8F}']
end;
TJColorFilter = class(TJavaGenericImport<JColorFilterClass, JColorFilter>) end;

JColorMatrixColorFilterClass = interface(JColorFilterClass)
['{4385CAF4-3E7F-4798-854D-B2F8F5E1023F}']
  {Methods}
  function init(matrix: JColorMatrix): JColorMatrixColorFilter; cdecl; overload;
  function init(array_: TJavaArray<Single>): JColorMatrixColorFilter; cdecl; overload;
end;

[JavaSignature('android/graphics/ColorMatrixColorFilter')]
JColorMatrixColorFilter = interface(JColorFilter)
['{73ECD817-E5B6-4BF5-A73A-B1404C746181}']
end;
TJColorMatrixColorFilter = class(TJavaGenericImport<JColorMatrixColorFilterClass, JColorMatrixColorFilter>) end;

JSweepGradientClass = interface(JShaderClass)
['{13878C2C-C9DC-4A09-AD53-0383CA0A482A}']
  {Methods}
  function init(cx: Single; cy: Single; colors: TJavaArray<Integer>; positions: TJavaArray<Single>): JSweepGradient; cdecl; overload;
  function init(cx: Single; cy: Single; color0: Integer; color1: Integer): JSweepGradient; cdecl; overload;
end;

[JavaSignature('android/graphics/SweepGradient')]
JSweepGradient = interface(JShader)
['{177E973D-2CF6-4583-A666-84297F17604D}']
end;
TJSweepGradient = class(TJavaGenericImport<JSweepGradientClass, JSweepGradient>) end;

JContentProviderOperation_BuilderClass = interface(JObjectClass)
['{ECA2D315-FD5C-4271-856B-768CAE25888A}']
end;

[JavaSignature('android/content/ContentProviderOperation$Builder')]
JContentProviderOperation_Builder = interface(JObject)
['{29CFAD81-EBF2-4FAD-91B9-21A28868A58E}']
  {Methods}
  function build: JContentProviderOperation; cdecl;
  function withExpectedCount(count: Integer): JContentProviderOperation_Builder; cdecl;
  function withSelection(selection: JString; selectionArgs: TJavaObjectArray<JString>): JContentProviderOperation_Builder; cdecl;
  function withSelectionBackReference(selectionArgIndex: Integer; previousResult: Integer): JContentProviderOperation_Builder; cdecl;
  function withValue(key: JString; value: JObject): JContentProviderOperation_Builder; cdecl;
  function withValueBackReference(key: JString; previousResult: Integer): JContentProviderOperation_Builder; cdecl;
  function withValueBackReferences(backReferences: JContentValues): JContentProviderOperation_Builder; cdecl;
  function withValues(values: JContentValues): JContentProviderOperation_Builder; cdecl;
  function withYieldAllowed(yieldAllowed: Boolean): JContentProviderOperation_Builder; cdecl;
end;
TJContentProviderOperation_Builder = class(TJavaGenericImport<JContentProviderOperation_BuilderClass, JContentProviderOperation_Builder>) end;

JObbScannerClass = interface(JObjectClass)
['{A9E61D74-9F04-4163-9FED-F379826B414D}']
  {Methods}
  function getObbInfo(filePath: JString): JObbInfo; cdecl;
end;

[JavaSignature('android/content/res/ObbScanner')]
JObbScanner = interface(JObject)
['{7941EFD3-144F-4281-A18A-6327DEAF2EC3}']
end;
TJObbScanner = class(TJavaGenericImport<JObbScannerClass, JObbScanner>) end;

JMatrix_ScaleToFitClass = interface(JEnumClass)
['{8FA5BE46-BB7B-4575-B3B5-E34370C2E93F}']
  {Property Methods}
  function _GetCENTER: JMatrix_ScaleToFit;
  function _GetEND: JMatrix_ScaleToFit;
  function _GetFILL: JMatrix_ScaleToFit;
  function _GetSTART: JMatrix_ScaleToFit;
  {Methods}
  function valueOf(name: JString): JMatrix_ScaleToFit; cdecl;
  function values: TJavaObjectArray<JMatrix_ScaleToFit>; cdecl;
  {Properties}
  property CENTER: JMatrix_ScaleToFit read _GetCENTER;
  property &END: JMatrix_ScaleToFit read _GetEND;
  property FILL: JMatrix_ScaleToFit read _GetFILL;
  property START: JMatrix_ScaleToFit read _GetSTART;
end;

[JavaSignature('android/graphics/Matrix$ScaleToFit')]
JMatrix_ScaleToFit = interface(JEnum)
['{792A8576-200E-49FE-9A47-C55746C0FD72}']
end;
TJMatrix_ScaleToFit = class(TJavaGenericImport<JMatrix_ScaleToFitClass, JMatrix_ScaleToFit>) end;

JResources_NotFoundExceptionClass = interface(JRuntimeExceptionClass)
['{67D1B823-0BF2-4DED-8AD0-E383DC53B37F}']
  {Methods}
  function init: JResources_NotFoundException; cdecl; overload;
  function init(name: JString): JResources_NotFoundException; cdecl; overload;
end;

[JavaSignature('android/content/res/Resources$NotFoundException')]
JResources_NotFoundException = interface(JRuntimeException)
['{A25CA5F9-9E49-4A67-A74A-0650828F8B84}']
end;
TJResources_NotFoundException = class(TJavaGenericImport<JResources_NotFoundExceptionClass, JResources_NotFoundException>) end;

JGradientDrawableClass = interface(JDrawableClass)
['{C04CD531-461A-464B-B3D0-25F633D2E1B4}']
  {Property Methods}
  function _GetLINE: Integer;
  function _GetLINEAR_GRADIENT: Integer;
  function _GetOVAL: Integer;
  function _GetRADIAL_GRADIENT: Integer;
  function _GetRECTANGLE: Integer;
  function _GetRING: Integer;
  function _GetSWEEP_GRADIENT: Integer;
  {Methods}
  function init: JGradientDrawable; cdecl; overload;
  function init(orientation: JGradientDrawable_Orientation; colors: TJavaArray<Integer>): JGradientDrawable; cdecl; overload;
  {Properties}
  property LINE: Integer read _GetLINE;
  property LINEAR_GRADIENT: Integer read _GetLINEAR_GRADIENT;
  property OVAL: Integer read _GetOVAL;
  property RADIAL_GRADIENT: Integer read _GetRADIAL_GRADIENT;
  property RECTANGLE: Integer read _GetRECTANGLE;
  property RING: Integer read _GetRING;
  property SWEEP_GRADIENT: Integer read _GetSWEEP_GRADIENT;
end;

[JavaSignature('android/graphics/drawable/GradientDrawable')]
JGradientDrawable = interface(JDrawable)
['{8573D42B-D225-4021-98B4-92385AF6777B}']
  {Methods}
  procedure draw(canvas: JCanvas); cdecl;
  function getChangingConfigurations: Integer; cdecl;
  function getConstantState: JDrawable_ConstantState; cdecl;
  function getIntrinsicHeight: Integer; cdecl;
  function getIntrinsicWidth: Integer; cdecl;
  function getOpacity: Integer; cdecl;
  function getOrientation: JGradientDrawable_Orientation; cdecl;
  function getPadding(padding: JRect): Boolean; cdecl;
  function mutate: JDrawable; cdecl;
  procedure setAlpha(alpha: Integer); cdecl;
  procedure setColor(argb: Integer); cdecl;
  procedure setColorFilter(cf: JColorFilter); cdecl;
  procedure setColors(colors: TJavaArray<Integer>); cdecl;
  procedure setCornerRadii(radii: TJavaArray<Single>); cdecl;
  procedure setCornerRadius(radius: Single); cdecl;
  procedure setDither(dither: Boolean); cdecl;
  procedure setGradientCenter(x: Single; y: Single); cdecl;
  procedure setGradientRadius(gradientRadius: Single); cdecl;
  procedure setGradientType(gradient: Integer); cdecl;
  procedure setOrientation(orientation: JGradientDrawable_Orientation); cdecl;
  procedure setShape(shape: Integer); cdecl;
  procedure setSize(width: Integer; height: Integer); cdecl;
  procedure setStroke(width: Integer; color: Integer); cdecl; overload;
  procedure setStroke(width: Integer; color: Integer; dashWidth: Single; dashGap: Single); cdecl; overload;
  procedure setUseLevel(useLevel: Boolean); cdecl;
end;
TJGradientDrawable = class(TJavaGenericImport<JGradientDrawableClass, JGradientDrawable>) end;

JViewTreeObserver_OnScrollChangedListenerClass = interface(IJavaClass)
['{47F7F111-5E81-4E3C-9DEF-5EB15E1338D7}']
end;

[JavaSignature('android/view/ViewTreeObserver$OnScrollChangedListener')]
JViewTreeObserver_OnScrollChangedListener = interface(IJavaInstance)
['{61DC96EE-2F56-4621-BA41-48FE4C367744}']
  {Methods}
  procedure onScrollChanged; cdecl;
end;
TJViewTreeObserver_OnScrollChangedListener = class(TJavaGenericImport<JViewTreeObserver_OnScrollChangedListenerClass, JViewTreeObserver_OnScrollChangedListener>) end;

JBaseInputConnectionClass = interface(JObjectClass)
['{2DA1840F-A45E-49FE-A74B-A2F601B6A32D}']
  {Methods}
  function init(targetView: JView; fullEditor: Boolean): JBaseInputConnection; cdecl;
  function getComposingSpanEnd(text: JSpannable): Integer; cdecl;
  function getComposingSpanStart(text: JSpannable): Integer; cdecl;
  procedure removeComposingSpans(text: JSpannable); cdecl;
  procedure setComposingSpans(text: JSpannable); cdecl;
end;

[JavaSignature('android/view/inputmethod/BaseInputConnection')]
JBaseInputConnection = interface(JObject)
['{7EDEBBAC-690B-4400-B285-190FA17DC215}']
  {Methods}
  function beginBatchEdit: Boolean; cdecl;
  function clearMetaKeyStates(states: Integer): Boolean; cdecl;
  function commitCompletion(text: JCompletionInfo): Boolean; cdecl;
  function commitCorrection(correctionInfo: JCorrectionInfo): Boolean; cdecl;
  function commitText(text: JCharSequence; newCursorPosition: Integer): Boolean; cdecl;
  function deleteSurroundingText(beforeLength: Integer; afterLength: Integer): Boolean; cdecl;
  function endBatchEdit: Boolean; cdecl;
  function finishComposingText: Boolean; cdecl;
  function getCursorCapsMode(reqModes: Integer): Integer; cdecl;
  function getEditable: JEditable; cdecl;
  function getExtractedText(request: JExtractedTextRequest; flags: Integer): JExtractedText; cdecl;
  function getSelectedText(flags: Integer): JCharSequence; cdecl;
  function getTextAfterCursor(length: Integer; flags: Integer): JCharSequence; cdecl;
  function getTextBeforeCursor(length: Integer; flags: Integer): JCharSequence; cdecl;
  function performContextMenuAction(id: Integer): Boolean; cdecl;
  function performEditorAction(actionCode: Integer): Boolean; cdecl;
  function performPrivateCommand(action: JString; data: JBundle): Boolean; cdecl;
  function reportFullscreenMode(enabled: Boolean): Boolean; cdecl;
  function sendKeyEvent(event: JKeyEvent): Boolean; cdecl;
  function setComposingRegion(start: Integer; end_: Integer): Boolean; cdecl;
  function setComposingText(text: JCharSequence; newCursorPosition: Integer): Boolean; cdecl;
  function setSelection(start: Integer; end_: Integer): Boolean; cdecl;
end;
TJBaseInputConnection = class(TJavaGenericImport<JBaseInputConnectionClass, JBaseInputConnection>) end;

JPorterDuffColorFilterClass = interface(JColorFilterClass)
['{60E5736C-B182-400C-942B-D86A7231FB74}']
  {Methods}
  function init(srcColor: Integer; mode: JPorterDuff_Mode): JPorterDuffColorFilter; cdecl;
end;

[JavaSignature('android/graphics/PorterDuffColorFilter')]
JPorterDuffColorFilter = interface(JColorFilter)
['{EDDC13E8-0D20-4AEA-9CC9-2E4CED3384CD}']
end;
TJPorterDuffColorFilter = class(TJavaGenericImport<JPorterDuffColorFilterClass, JPorterDuffColorFilter>) end;

JCharacterStyleClass = interface(JObjectClass)
['{CDC00A8A-001E-4012-9B1A-E2B8A5EA13EB}']
  {Methods}
  function init: JCharacterStyle; cdecl;
  function wrap(cs: JCharacterStyle): JCharacterStyle; cdecl;
end;

[JavaSignature('android/text/style/CharacterStyle')]
JCharacterStyle = interface(JObject)
['{9785229B-10B2-435D-AF2C-2A02622EEAA6}']
  {Methods}
  function getUnderlying: JCharacterStyle; cdecl;
  procedure updateDrawState(tp: JTextPaint); cdecl;
end;
TJCharacterStyle = class(TJavaGenericImport<JCharacterStyleClass, JCharacterStyle>) end;

JClickableSpanClass = interface(JCharacterStyleClass)
['{A2958C1E-9CF3-48BC-84C1-404992C708E7}']
  {Methods}
  function init: JClickableSpan; cdecl;
end;

[JavaSignature('android/text/style/ClickableSpan')]
JClickableSpan = interface(JCharacterStyle)
['{7749B988-4ADD-4E17-9F64-BA50060D2BCA}']
  {Methods}
  procedure onClick(widget: JView); cdecl;
  procedure updateDrawState(ds: JTextPaint); cdecl;
end;
TJClickableSpan = class(TJavaGenericImport<JClickableSpanClass, JClickableSpan>) end;

JURLSpanClass = interface(JClickableSpanClass)
['{F6F51E82-0AB4-4832-B8E7-24612C1154BA}']
  {Methods}
  function init(url: JString): JURLSpan; cdecl; overload;
  function init(src: JParcel): JURLSpan; cdecl; overload;
end;

[JavaSignature('android/text/style/URLSpan')]
JURLSpan = interface(JClickableSpan)
['{A18D08F1-4B56-4594-8680-1A0ABE57807C}']
  {Methods}
  function describeContents: Integer; cdecl;
  function getSpanTypeId: Integer; cdecl;
  function getURL: JString; cdecl;
  procedure onClick(widget: JView); cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
end;
TJURLSpan = class(TJavaGenericImport<JURLSpanClass, JURLSpan>) end;

JCanvas_EdgeTypeClass = interface(JEnumClass)
['{8D1EEE35-A96F-4AF3-9E0A-4DE303F76689}']
  {Property Methods}
  function _GetAA: JCanvas_EdgeType;
  function _GetBW: JCanvas_EdgeType;
  {Methods}
  function valueOf(name: JString): JCanvas_EdgeType; cdecl;
  function values: TJavaObjectArray<JCanvas_EdgeType>; cdecl;
  {Properties}
  property AA: JCanvas_EdgeType read _GetAA;
  property BW: JCanvas_EdgeType read _GetBW;
end;

[JavaSignature('android/graphics/Canvas$EdgeType')]
JCanvas_EdgeType = interface(JEnum)
['{8D22D842-F814-49A2-ADA9-4F8F8AD97D80}']
end;
TJCanvas_EdgeType = class(TJavaGenericImport<JCanvas_EdgeTypeClass, JCanvas_EdgeType>) end;

JShapeClass = interface(JObjectClass)
['{7C5B6760-DDFD-4D27-BF6A-13320A247039}']
  {Methods}
  function init: JShape; cdecl;
end;

[JavaSignature('android/graphics/drawable/shapes/Shape')]
JShape = interface(JObject)
['{2FE9999A-9388-45DF-BFB9-0ED2F659EB9B}']
  {Methods}
  function clone: JShape; cdecl;
  procedure draw(canvas: JCanvas; paint: JPaint); cdecl;
  function getHeight: Single; cdecl;
  function getWidth: Single; cdecl;
  function hasAlpha: Boolean; cdecl;
  procedure resize(width: Single; height: Single); cdecl;
end;
TJShape = class(TJavaGenericImport<JShapeClass, JShape>) end;

JPathShapeClass = interface(JShapeClass)
['{AC049327-80D1-4F2E-BD60-BFED7EF9D2C5}']
  {Methods}
  function init(path: JPath; stdWidth: Single; stdHeight: Single): JPathShape; cdecl;
end;

[JavaSignature('android/graphics/drawable/shapes/PathShape')]
JPathShape = interface(JShape)
['{DEDE18ED-0466-4F1B-8EC8-465B59FF0D09}']
  {Methods}
  function clone: JPathShape; cdecl;
  procedure draw(canvas: JCanvas; paint: JPaint); cdecl;
end;
TJPathShape = class(TJavaGenericImport<JPathShapeClass, JPathShape>) end;

JComponentCallbacksClass = interface(IJavaClass)
['{916F97DB-E2D9-44BA-A88B-A9636A3381B5}']
end;

[JavaSignature('android/content/ComponentCallbacks')]
JComponentCallbacks = interface(IJavaInstance)
['{A878B465-4CD0-4210-A990-5B05CE38A99F}']
  {Methods}
  procedure onConfigurationChanged(newConfig: JConfiguration); cdecl;
  procedure onLowMemory; cdecl;
end;
TJComponentCallbacks = class(TJavaGenericImport<JComponentCallbacksClass, JComponentCallbacks>) end;

JComponentCallbacks2Class = interface(JComponentCallbacksClass)
['{A16878C7-9C07-4346-BB24-8ADC7D943B48}']
  {Property Methods}
  function _GetTRIM_MEMORY_BACKGROUND: Integer;
  function _GetTRIM_MEMORY_COMPLETE: Integer;
  function _GetTRIM_MEMORY_MODERATE: Integer;
  function _GetTRIM_MEMORY_RUNNING_CRITICAL: Integer;
  function _GetTRIM_MEMORY_RUNNING_LOW: Integer;
  function _GetTRIM_MEMORY_RUNNING_MODERATE: Integer;
  function _GetTRIM_MEMORY_UI_HIDDEN: Integer;
  {Properties}
  property TRIM_MEMORY_BACKGROUND: Integer read _GetTRIM_MEMORY_BACKGROUND;
  property TRIM_MEMORY_COMPLETE: Integer read _GetTRIM_MEMORY_COMPLETE;
  property TRIM_MEMORY_MODERATE: Integer read _GetTRIM_MEMORY_MODERATE;
  property TRIM_MEMORY_RUNNING_CRITICAL: Integer read _GetTRIM_MEMORY_RUNNING_CRITICAL;
  property TRIM_MEMORY_RUNNING_LOW: Integer read _GetTRIM_MEMORY_RUNNING_LOW;
  property TRIM_MEMORY_RUNNING_MODERATE: Integer read _GetTRIM_MEMORY_RUNNING_MODERATE;
  property TRIM_MEMORY_UI_HIDDEN: Integer read _GetTRIM_MEMORY_UI_HIDDEN;
end;

[JavaSignature('android/content/ComponentCallbacks2')]
JComponentCallbacks2 = interface(JComponentCallbacks)
['{2D389328-9C3F-44D8-8D5A-B124D8E5C5C5}']
  {Methods}
  procedure onTrimMemory(level: Integer); cdecl;
end;
TJComponentCallbacks2 = class(TJavaGenericImport<JComponentCallbacks2Class, JComponentCallbacks2>) end;

JSignatureClass = interface(JObjectClass)
['{09302121-4635-41A0-A997-7A6D89360D58}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Methods}
  function init(signature: TJavaArray<Byte>): JSignature; cdecl; overload;
  function init(text: JString): JSignature; cdecl; overload;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/content/pm/Signature')]
JSignature = interface(JObject)
['{31FAFA1C-0DB9-4CCD-96BC-332908BF2D01}']
  {Methods}
  function describeContents: Integer; cdecl;
  function equals(obj: JObject): Boolean; cdecl;
  function hashCode: Integer; cdecl;
  function toByteArray: TJavaArray<Byte>; cdecl;
  function toChars: TJavaArray<Char>; cdecl; overload;
  function toChars(existingArray: TJavaArray<Char>; outLen: TJavaArray<Integer>): TJavaArray<Char>; cdecl; overload;
  function toCharsString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; parcelableFlags: Integer); cdecl;
end;
TJSignature = class(TJavaGenericImport<JSignatureClass, JSignature>) end;

JInputDeviceClass = interface(JObjectClass)
['{E0E16597-3285-468E-8687-2CE588147240}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  function _GetKEYBOARD_TYPE_ALPHABETIC: Integer;
  function _GetKEYBOARD_TYPE_NONE: Integer;
  function _GetKEYBOARD_TYPE_NON_ALPHABETIC: Integer;
  function _GetMOTION_RANGE_ORIENTATION: Integer;
  function _GetMOTION_RANGE_PRESSURE: Integer;
  function _GetMOTION_RANGE_SIZE: Integer;
  function _GetMOTION_RANGE_TOOL_MAJOR: Integer;
  function _GetMOTION_RANGE_TOOL_MINOR: Integer;
  function _GetMOTION_RANGE_TOUCH_MAJOR: Integer;
  function _GetMOTION_RANGE_TOUCH_MINOR: Integer;
  function _GetMOTION_RANGE_X: Integer;
  function _GetMOTION_RANGE_Y: Integer;
  function _GetSOURCE_ANY: Integer;
  function _GetSOURCE_CLASS_BUTTON: Integer;
  function _GetSOURCE_CLASS_JOYSTICK: Integer;
  function _GetSOURCE_CLASS_MASK: Integer;
  function _GetSOURCE_CLASS_POINTER: Integer;
  function _GetSOURCE_CLASS_POSITION: Integer;
  function _GetSOURCE_CLASS_TRACKBALL: Integer;
  function _GetSOURCE_DPAD: Integer;
  function _GetSOURCE_GAMEPAD: Integer;
  function _GetSOURCE_JOYSTICK: Integer;
  function _GetSOURCE_KEYBOARD: Integer;
  function _GetSOURCE_MOUSE: Integer;
  function _GetSOURCE_STYLUS: Integer;
  function _GetSOURCE_TOUCHPAD: Integer;
  function _GetSOURCE_TOUCHSCREEN: Integer;
  function _GetSOURCE_TRACKBALL: Integer;
  function _GetSOURCE_UNKNOWN: Integer;
  {Methods}
  function getDevice(id: Integer): JInputDevice; cdecl;
  function getDeviceIds: TJavaArray<Integer>; cdecl;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
  property KEYBOARD_TYPE_ALPHABETIC: Integer read _GetKEYBOARD_TYPE_ALPHABETIC;
  property KEYBOARD_TYPE_NONE: Integer read _GetKEYBOARD_TYPE_NONE;
  property KEYBOARD_TYPE_NON_ALPHABETIC: Integer read _GetKEYBOARD_TYPE_NON_ALPHABETIC;
  property MOTION_RANGE_ORIENTATION: Integer read _GetMOTION_RANGE_ORIENTATION;
  property MOTION_RANGE_PRESSURE: Integer read _GetMOTION_RANGE_PRESSURE;
  property MOTION_RANGE_SIZE: Integer read _GetMOTION_RANGE_SIZE;
  property MOTION_RANGE_TOOL_MAJOR: Integer read _GetMOTION_RANGE_TOOL_MAJOR;
  property MOTION_RANGE_TOOL_MINOR: Integer read _GetMOTION_RANGE_TOOL_MINOR;
  property MOTION_RANGE_TOUCH_MAJOR: Integer read _GetMOTION_RANGE_TOUCH_MAJOR;
  property MOTION_RANGE_TOUCH_MINOR: Integer read _GetMOTION_RANGE_TOUCH_MINOR;
  property MOTION_RANGE_X: Integer read _GetMOTION_RANGE_X;
  property MOTION_RANGE_Y: Integer read _GetMOTION_RANGE_Y;
  property SOURCE_ANY: Integer read _GetSOURCE_ANY;
  property SOURCE_CLASS_BUTTON: Integer read _GetSOURCE_CLASS_BUTTON;
  property SOURCE_CLASS_JOYSTICK: Integer read _GetSOURCE_CLASS_JOYSTICK;
  property SOURCE_CLASS_MASK: Integer read _GetSOURCE_CLASS_MASK;
  property SOURCE_CLASS_POINTER: Integer read _GetSOURCE_CLASS_POINTER;
  property SOURCE_CLASS_POSITION: Integer read _GetSOURCE_CLASS_POSITION;
  property SOURCE_CLASS_TRACKBALL: Integer read _GetSOURCE_CLASS_TRACKBALL;
  property SOURCE_DPAD: Integer read _GetSOURCE_DPAD;
  property SOURCE_GAMEPAD: Integer read _GetSOURCE_GAMEPAD;
  property SOURCE_JOYSTICK: Integer read _GetSOURCE_JOYSTICK;
  property SOURCE_KEYBOARD: Integer read _GetSOURCE_KEYBOARD;
  property SOURCE_MOUSE: Integer read _GetSOURCE_MOUSE;
  property SOURCE_STYLUS: Integer read _GetSOURCE_STYLUS;
  property SOURCE_TOUCHPAD: Integer read _GetSOURCE_TOUCHPAD;
  property SOURCE_TOUCHSCREEN: Integer read _GetSOURCE_TOUCHSCREEN;
  property SOURCE_TRACKBALL: Integer read _GetSOURCE_TRACKBALL;
  property SOURCE_UNKNOWN: Integer read _GetSOURCE_UNKNOWN;
end;

[JavaSignature('android/view/InputDevice')]
JInputDevice = interface(JObject)
['{6E91CB04-A6A7-4C2B-B90E-4D6B321E18C0}']
  {Methods}
  function describeContents: Integer; cdecl;
  function getDescriptor: JString; cdecl;
  function getId: Integer; cdecl;
  function getKeyCharacterMap: JKeyCharacterMap; cdecl;
  function getKeyboardType: Integer; cdecl;
  function getMotionRange(axis: Integer): JInputDevice_MotionRange; cdecl; overload;
  function getMotionRange(axis: Integer; source: Integer): JInputDevice_MotionRange; cdecl; overload;
  function getMotionRanges: JList; cdecl;
  function getName: JString; cdecl;
  function getSources: Integer; cdecl;
  function getVibrator: JVibrator; cdecl;
  function isVirtual: Boolean; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
end;
TJInputDevice = class(TJavaGenericImport<JInputDeviceClass, JInputDevice>) end;

Jcontent_EntityClass = interface(JObjectClass)
['{9D8D5E64-FE08-490B-857C-AFE5884F7935}']
  {Methods}
  function init(values: JContentValues): Jcontent_Entity; cdecl;
end;

[JavaSignature('android/content/Entity')]
Jcontent_Entity = interface(JObject)
['{8AE3B991-8A16-4BF0-8828-B2D86DD6D97F}']
  {Methods}
  procedure addSubValue(uri: Jnet_Uri; values: JContentValues); cdecl;
  function getEntityValues: JContentValues; cdecl;
  function getSubValues: JArrayList; cdecl;
  function toString: JString; cdecl;
end;
TJcontent_Entity = class(TJavaGenericImport<Jcontent_EntityClass, Jcontent_Entity>) end;

JContentValuesClass = interface(JObjectClass)
['{292EE66E-E970-4812-89F5-8E22A32EDA29}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  function _GetTAG: JString;
  {Methods}
  function init: JContentValues; cdecl; overload;
  function init(size: Integer): JContentValues; cdecl; overload;
  function init(from: JContentValues): JContentValues; cdecl; overload;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
  property TAG: JString read _GetTAG;
end;

[JavaSignature('android/content/ContentValues')]
JContentValues = interface(JObject)
['{1A22E83D-2DC3-4E93-9FB4-9476C7402066}']
  {Methods}
  procedure clear; cdecl;
  function containsKey(key: JString): Boolean; cdecl;
  function describeContents: Integer; cdecl;
  function equals(object_: JObject): Boolean; cdecl;
  function &get(key: JString): JObject; cdecl;
  function getAsBoolean(key: JString): JBoolean; cdecl;
  function getAsByte(key: JString): JByte; cdecl;
  function getAsByteArray(key: JString): TJavaArray<Byte>; cdecl;
  function getAsDouble(key: JString): JDouble; cdecl;
  function getAsFloat(key: JString): JFloat; cdecl;
  function getAsInteger(key: JString): JInteger; cdecl;
  function getAsLong(key: JString): JLong; cdecl;
  function getAsShort(key: JString): JShort; cdecl;
  function getAsString(key: JString): JString; cdecl;
  function hashCode: Integer; cdecl;
  function keySet: JSet; cdecl;
  procedure put(key: JString; value: JString); cdecl; overload;
  procedure put(key: JString; value: JByte); cdecl; overload;
  procedure put(key: JString; value: JShort); cdecl; overload;
  procedure put(key: JString; value: JInteger); cdecl; overload;
  procedure put(key: JString; value: JLong); cdecl; overload;
  procedure put(key: JString; value: JFloat); cdecl; overload;
  procedure put(key: JString; value: JDouble); cdecl; overload;
  procedure put(key: JString; value: JBoolean); cdecl; overload;
  procedure put(key: JString; value: TJavaArray<Byte>); cdecl; overload;
  procedure putAll(other: JContentValues); cdecl;
  procedure putNull(key: JString); cdecl;
  procedure remove(key: JString); cdecl;
  function size: Integer; cdecl;
  function toString: JString; cdecl;
  function valueSet: JSet; cdecl;
  procedure writeToParcel(parcel: JParcel; flags: Integer); cdecl;
end;
TJContentValues = class(TJavaGenericImport<JContentValuesClass, JContentValues>) end;

JLayoutInflater_FilterClass = interface(IJavaClass)
['{504748F4-5E9F-480B-8779-1622CA371CA7}']
end;

[JavaSignature('android/view/LayoutInflater$Filter')]
JLayoutInflater_Filter = interface(IJavaInstance)
['{89E43521-AEEE-4746-95F9-732DA680FA75}']
  {Methods}
  function onLoadClass(clazz: Jlang_Class): Boolean; cdecl;
end;
TJLayoutInflater_Filter = class(TJavaGenericImport<JLayoutInflater_FilterClass, JLayoutInflater_Filter>) end;

JLayoutInflater_FactoryClass = interface(IJavaClass)
['{B778AAF7-E1CD-4A08-9330-31E71A823B3E}']
end;

[JavaSignature('android/view/LayoutInflater$Factory')]
JLayoutInflater_Factory = interface(IJavaInstance)
['{4F826237-FA2E-4392-A26B-5F7C6ACB48E3}']
  {Methods}
  function onCreateView(name: JString; context: JContext; attrs: JAttributeSet): JView; cdecl;
end;
TJLayoutInflater_Factory = class(TJavaGenericImport<JLayoutInflater_FactoryClass, JLayoutInflater_Factory>) end;

JBitmapFactory_OptionsClass = interface(JObjectClass)
['{15B209CE-4C2A-4351-8426-8E962B81C428}']
  {Methods}
  function init: JBitmapFactory_Options; cdecl;
end;

[JavaSignature('android/graphics/BitmapFactory$Options')]
JBitmapFactory_Options = interface(JObject)
['{F255C336-DE61-4A4A-91E6-206BEBE1F9DB}']
  {Property Methods}
  function _GetinBitmap: JBitmap;
  procedure _SetinBitmap(Value: JBitmap);
  function _GetinDensity: Integer;
  procedure _SetinDensity(Value: Integer);
  function _GetinDither: Boolean;
  procedure _SetinDither(Value: Boolean);
  function _GetinInputShareable: Boolean;
  procedure _SetinInputShareable(Value: Boolean);
  function _GetinJustDecodeBounds: Boolean;
  procedure _SetinJustDecodeBounds(Value: Boolean);
  function _GetinMutable: Boolean;
  procedure _SetinMutable(Value: Boolean);
  function _GetinPreferQualityOverSpeed: Boolean;
  procedure _SetinPreferQualityOverSpeed(Value: Boolean);
  function _GetinPreferredConfig: JBitmap_Config;
  procedure _SetinPreferredConfig(Value: JBitmap_Config);
  function _GetinPurgeable: Boolean;
  procedure _SetinPurgeable(Value: Boolean);
  function _GetinSampleSize: Integer;
  procedure _SetinSampleSize(Value: Integer);
  function _GetinScaled: Boolean;
  procedure _SetinScaled(Value: Boolean);
  function _GetinScreenDensity: Integer;
  procedure _SetinScreenDensity(Value: Integer);
  function _GetinTargetDensity: Integer;
  procedure _SetinTargetDensity(Value: Integer);
  function _GetinTempStorage: TJavaArray<Byte>;
  procedure _SetinTempStorage(Value: TJavaArray<Byte>);
  function _GetmCancel: Boolean;
  procedure _SetmCancel(Value: Boolean);
  function _GetoutHeight: Integer;
  procedure _SetoutHeight(Value: Integer);
  function _GetoutMimeType: JString;
  procedure _SetoutMimeType(Value: JString);
  function _GetoutWidth: Integer;
  procedure _SetoutWidth(Value: Integer);
  {Methods}
  procedure requestCancelDecode; cdecl;
  {Properties}
  property inBitmap: JBitmap read _GetinBitmap write _SetinBitmap;
  property inDensity: Integer read _GetinDensity write _SetinDensity;
  property inDither: Boolean read _GetinDither write _SetinDither;
  property inInputShareable: Boolean read _GetinInputShareable write _SetinInputShareable;
  property inJustDecodeBounds: Boolean read _GetinJustDecodeBounds write _SetinJustDecodeBounds;
  property inMutable: Boolean read _GetinMutable write _SetinMutable;
  property inPreferQualityOverSpeed: Boolean read _GetinPreferQualityOverSpeed write _SetinPreferQualityOverSpeed;
  property inPreferredConfig: JBitmap_Config read _GetinPreferredConfig write _SetinPreferredConfig;
  property inPurgeable: Boolean read _GetinPurgeable write _SetinPurgeable;
  property inSampleSize: Integer read _GetinSampleSize write _SetinSampleSize;
  property inScaled: Boolean read _GetinScaled write _SetinScaled;
  property inScreenDensity: Integer read _GetinScreenDensity write _SetinScreenDensity;
  property inTargetDensity: Integer read _GetinTargetDensity write _SetinTargetDensity;
  property inTempStorage: TJavaArray<Byte> read _GetinTempStorage write _SetinTempStorage;
  property mCancel: Boolean read _GetmCancel write _SetmCancel;
  property outHeight: Integer read _GetoutHeight write _SetoutHeight;
  property outMimeType: JString read _GetoutMimeType write _SetoutMimeType;
  property outWidth: Integer read _GetoutWidth write _SetoutWidth;
end;
TJBitmapFactory_Options = class(TJavaGenericImport<JBitmapFactory_OptionsClass, JBitmapFactory_Options>) end;

JColorMatrixClass = interface(JObjectClass)
['{0DBEAB0A-B522-47CA-9EB9-3D921E7BB22E}']
  {Methods}
  function init: JColorMatrix; cdecl; overload;
  function init(src: TJavaArray<Single>): JColorMatrix; cdecl; overload;
  function init(src: JColorMatrix): JColorMatrix; cdecl; overload;
end;

[JavaSignature('android/graphics/ColorMatrix')]
JColorMatrix = interface(JObject)
['{F60844E8-C106-410F-9D38-7C1176FF3D59}']
  {Methods}
  function getArray: TJavaArray<Single>; cdecl;
  procedure postConcat(postmatrix: JColorMatrix); cdecl;
  procedure preConcat(prematrix: JColorMatrix); cdecl;
  procedure reset; cdecl;
  procedure &set(src: JColorMatrix); cdecl; overload;
  procedure &set(src: TJavaArray<Single>); cdecl; overload;
  procedure setConcat(matA: JColorMatrix; matB: JColorMatrix); cdecl;
  procedure setRGB2YUV; cdecl;
  procedure setRotate(axis: Integer; degrees: Single); cdecl;
  procedure setSaturation(sat: Single); cdecl;
  procedure setScale(rScale: Single; gScale: Single; bScale: Single; aScale: Single); cdecl;
  procedure setYUV2RGB; cdecl;
end;
TJColorMatrix = class(TJavaGenericImport<JColorMatrixClass, JColorMatrix>) end;

JIntentSender_SendIntentExceptionClass = interface(JAndroidExceptionClass)
['{B8FD197B-1FA1-4689-9AF0-9CD8F6929CFD}']
  {Methods}
  function init: JIntentSender_SendIntentException; cdecl; overload;
  function init(name: JString): JIntentSender_SendIntentException; cdecl; overload;
  function init(cause: JException): JIntentSender_SendIntentException; cdecl; overload;
end;

[JavaSignature('android/content/IntentSender$SendIntentException')]
JIntentSender_SendIntentException = interface(JAndroidException)
['{56AA4D1D-E7E5-418B-8507-286B3F66D5A4}']
end;
TJIntentSender_SendIntentException = class(TJavaGenericImport<JIntentSender_SendIntentExceptionClass, JIntentSender_SendIntentException>) end;

JAssetManagerClass = interface(JObjectClass)
['{4DA55E97-38E2-41CC-92DA-2764845B4E5A}']
  {Property Methods}
  function _GetACCESS_BUFFER: Integer;
  function _GetACCESS_RANDOM: Integer;
  function _GetACCESS_STREAMING: Integer;
  function _GetACCESS_UNKNOWN: Integer;
  {Properties}
  property ACCESS_BUFFER: Integer read _GetACCESS_BUFFER;
  property ACCESS_RANDOM: Integer read _GetACCESS_RANDOM;
  property ACCESS_STREAMING: Integer read _GetACCESS_STREAMING;
  property ACCESS_UNKNOWN: Integer read _GetACCESS_UNKNOWN;
end;

[JavaSignature('android/content/res/AssetManager')]
JAssetManager = interface(JObject)
['{319A7832-408F-487B-8EF0-6BA96F232C84}']
  {Methods}
  procedure close; cdecl;
  function getLocales: TJavaObjectArray<JString>; cdecl;
  function list(path: JString): TJavaObjectArray<JString>; cdecl;
  function open(fileName: JString): JInputStream; cdecl; overload;
  function open(fileName: JString; accessMode: Integer): JInputStream; cdecl; overload;
  function openFd(fileName: JString): JAssetFileDescriptor; cdecl;
  function openNonAssetFd(fileName: JString): JAssetFileDescriptor; cdecl; overload;
  function openNonAssetFd(cookie: Integer; fileName: JString): JAssetFileDescriptor; cdecl; overload;
  function openXmlResourceParser(fileName: JString): JXmlResourceParser; cdecl; overload;
  function openXmlResourceParser(cookie: Integer; fileName: JString): JXmlResourceParser; cdecl; overload;
end;
TJAssetManager = class(TJavaGenericImport<JAssetManagerClass, JAssetManager>) end;

JSurfaceClass = interface(JObjectClass)
['{2C926B05-29F6-4EDC-92D6-222B6D030BB4}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  function _GetROTATION_0: Integer;
  function _GetROTATION_180: Integer;
  function _GetROTATION_270: Integer;
  function _GetROTATION_90: Integer;
  {Methods}
  function init(surfaceTexture: JSurfaceTexture): JSurface; cdecl;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
  property ROTATION_0: Integer read _GetROTATION_0;
  property ROTATION_180: Integer read _GetROTATION_180;
  property ROTATION_270: Integer read _GetROTATION_270;
  property ROTATION_90: Integer read _GetROTATION_90;
end;

[JavaSignature('android/view/Surface')]
JSurface = interface(JObject)
['{87E788EF-EE5D-4CC8-BA6A-528B8B0A7EA5}']
  {Methods}
  function describeContents: Integer; cdecl;
  function isValid: Boolean; cdecl;
  function lockCanvas(dirty: JRect): JCanvas; cdecl;
  procedure readFromParcel(source: JParcel); cdecl;
  procedure release; cdecl;
  function toString: JString; cdecl;
  procedure unlockCanvas(canvas: JCanvas); cdecl;//Deprecated
  procedure unlockCanvasAndPost(canvas: JCanvas); cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
end;
TJSurface = class(TJavaGenericImport<JSurfaceClass, JSurface>) end;

JBitmapFactoryClass = interface(JObjectClass)
['{B036E322-D1D6-43F7-A4D6-2C8C0BEFCDA6}']
  {Methods}
  function init: JBitmapFactory; cdecl;
  function decodeByteArray(data: TJavaArray<Byte>; offset: Integer; length: Integer; opts: JBitmapFactory_Options): JBitmap; cdecl; overload;
  function decodeByteArray(data: TJavaArray<Byte>; offset: Integer; length: Integer): JBitmap; cdecl; overload;
  function decodeFile(pathName: JString; opts: JBitmapFactory_Options): JBitmap; cdecl; overload;
  function decodeFile(pathName: JString): JBitmap; cdecl; overload;
  function decodeFileDescriptor(fd: JFileDescriptor; outPadding: JRect; opts: JBitmapFactory_Options): JBitmap; cdecl; overload;
  function decodeFileDescriptor(fd: JFileDescriptor): JBitmap; cdecl; overload;
  function decodeResource(res: JResources; id: Integer; opts: JBitmapFactory_Options): JBitmap; cdecl; overload;
  function decodeResource(res: JResources; id: Integer): JBitmap; cdecl; overload;
  function decodeResourceStream(res: JResources; value: JTypedValue; is_: JInputStream; pad: JRect; opts: JBitmapFactory_Options): JBitmap; cdecl;
  function decodeStream(is_: JInputStream; outPadding: JRect; opts: JBitmapFactory_Options): JBitmap; cdecl; overload;
  function decodeStream(is_: JInputStream): JBitmap; cdecl; overload;
end;

[JavaSignature('android/graphics/BitmapFactory')]
JBitmapFactory = interface(JObject)
['{7D95177F-7BBF-4FBE-BDAF-2DAC213B6591}']
end;
TJBitmapFactory = class(TJavaGenericImport<JBitmapFactoryClass, JBitmapFactory>) end;

JBitmapShaderClass = interface(JShaderClass)
['{6465A729-9A1A-4295-8CB5-745656392F46}']
  {Methods}
  function init(bitmap: JBitmap; tileX: JShader_TileMode; tileY: JShader_TileMode): JBitmapShader; cdecl;
end;

[JavaSignature('android/graphics/BitmapShader')]
JBitmapShader = interface(JShader)
['{E4461B90-31BB-4C74-A418-AB0D73850721}']
end;
TJBitmapShader = class(TJavaGenericImport<JBitmapShaderClass, JBitmapShader>) end;

JClipDescriptionClass = interface(JObjectClass)
['{6C6FB2F7-7FDB-4625-8C43-3663387476D5}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  function _GetMIMETYPE_TEXT_HTML: JString;
  function _GetMIMETYPE_TEXT_INTENT: JString;
  function _GetMIMETYPE_TEXT_PLAIN: JString;
  function _GetMIMETYPE_TEXT_URILIST: JString;
  {Methods}
  function init(label_: JCharSequence; mimeTypes: TJavaObjectArray<JString>): JClipDescription; cdecl; overload;
  function init(o: JClipDescription): JClipDescription; cdecl; overload;
  function compareMimeTypes(concreteType: JString; desiredType: JString): Boolean; cdecl;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
  property MIMETYPE_TEXT_HTML: JString read _GetMIMETYPE_TEXT_HTML;
  property MIMETYPE_TEXT_INTENT: JString read _GetMIMETYPE_TEXT_INTENT;
  property MIMETYPE_TEXT_PLAIN: JString read _GetMIMETYPE_TEXT_PLAIN;
  property MIMETYPE_TEXT_URILIST: JString read _GetMIMETYPE_TEXT_URILIST;
end;

[JavaSignature('android/content/ClipDescription')]
JClipDescription = interface(JObject)
['{BA87A0BC-6174-460C-BA29-521BC4824920}']
  {Methods}
  function describeContents: Integer; cdecl;
  function filterMimeTypes(mimeType: JString): TJavaObjectArray<JString>; cdecl;
  function getLabel: JCharSequence; cdecl;
  function getMimeType(index: Integer): JString; cdecl;
  function getMimeTypeCount: Integer; cdecl;
  function hasMimeType(mimeType: JString): Boolean; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
end;
TJClipDescription = class(TJavaGenericImport<JClipDescriptionClass, JClipDescription>) end;

JContentProviderOperationClass = interface(JObjectClass)
['{07D5CC7B-D9E7-4BE4-ACDC-0452935B5D93}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Methods}
  function newAssertQuery(uri: Jnet_Uri): JContentProviderOperation_Builder; cdecl;
  function newDelete(uri: Jnet_Uri): JContentProviderOperation_Builder; cdecl;
  function newInsert(uri: Jnet_Uri): JContentProviderOperation_Builder; cdecl;
  function newUpdate(uri: Jnet_Uri): JContentProviderOperation_Builder; cdecl;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/content/ContentProviderOperation')]
JContentProviderOperation = interface(JObject)
['{48EB1D9B-DB88-447D-A009-3FA8B16CB6F8}']
  {Methods}
  function apply(provider: JContentProvider; backRefs: TJavaObjectArray<JContentProviderResult>; numBackRefs: Integer): JContentProviderResult; cdecl;
  function describeContents: Integer; cdecl;
  function getUri: Jnet_Uri; cdecl;
  function isReadOperation: Boolean; cdecl;
  function isWriteOperation: Boolean; cdecl;
  function isYieldAllowed: Boolean; cdecl;
  function resolveSelectionArgsBackReferences(backRefs: TJavaObjectArray<JContentProviderResult>; numBackRefs: Integer): TJavaObjectArray<JString>; cdecl;
  function resolveValueBackReferences(backRefs: TJavaObjectArray<JContentProviderResult>; numBackRefs: Integer): JContentValues; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
end;
TJContentProviderOperation = class(TJavaGenericImport<JContentProviderOperationClass, JContentProviderOperation>) end;

JActivityInfoClass = interface(JComponentInfoClass)
['{668E9F65-FCB9-44FB-B049-B332B42C8201}']
  {Property Methods}
  function _GetCONFIG_DENSITY: Integer;
  function _GetCONFIG_FONT_SCALE: Integer;
  function _GetCONFIG_KEYBOARD: Integer;
  function _GetCONFIG_KEYBOARD_HIDDEN: Integer;
  function _GetCONFIG_LAYOUT_DIRECTION: Integer;
  function _GetCONFIG_LOCALE: Integer;
  function _GetCONFIG_MCC: Integer;
  function _GetCONFIG_MNC: Integer;
  function _GetCONFIG_NAVIGATION: Integer;
  function _GetCONFIG_ORIENTATION: Integer;
  function _GetCONFIG_SCREEN_LAYOUT: Integer;
  function _GetCONFIG_SCREEN_SIZE: Integer;
  function _GetCONFIG_SMALLEST_SCREEN_SIZE: Integer;
  function _GetCONFIG_TOUCHSCREEN: Integer;
  function _GetCONFIG_UI_MODE: Integer;
  function _GetCREATOR: JParcelable_Creator;
  function _GetFLAG_ALLOW_TASK_REPARENTING: Integer;
  function _GetFLAG_ALWAYS_RETAIN_TASK_STATE: Integer;
  function _GetFLAG_CLEAR_TASK_ON_LAUNCH: Integer;
  function _GetFLAG_EXCLUDE_FROM_RECENTS: Integer;
  function _GetFLAG_FINISH_ON_CLOSE_SYSTEM_DIALOGS: Integer;
  function _GetFLAG_FINISH_ON_TASK_LAUNCH: Integer;
  function _GetFLAG_HARDWARE_ACCELERATED: Integer;
  function _GetFLAG_MULTIPROCESS: Integer;
  function _GetFLAG_NO_HISTORY: Integer;
  function _GetFLAG_SINGLE_USER: Integer;
  function _GetFLAG_STATE_NOT_NEEDED: Integer;
  function _GetLAUNCH_MULTIPLE: Integer;
  function _GetLAUNCH_SINGLE_INSTANCE: Integer;
  function _GetLAUNCH_SINGLE_TASK: Integer;
  function _GetLAUNCH_SINGLE_TOP: Integer;
  function _GetSCREEN_ORIENTATION_BEHIND: Integer;
  function _GetSCREEN_ORIENTATION_FULL_SENSOR: Integer;
  function _GetSCREEN_ORIENTATION_LANDSCAPE: Integer;
  function _GetSCREEN_ORIENTATION_NOSENSOR: Integer;
  function _GetSCREEN_ORIENTATION_PORTRAIT: Integer;
  function _GetSCREEN_ORIENTATION_REVERSE_LANDSCAPE: Integer;
  function _GetSCREEN_ORIENTATION_REVERSE_PORTRAIT: Integer;
  function _GetSCREEN_ORIENTATION_SENSOR: Integer;
  function _GetSCREEN_ORIENTATION_SENSOR_LANDSCAPE: Integer;
  function _GetSCREEN_ORIENTATION_SENSOR_PORTRAIT: Integer;
  function _GetSCREEN_ORIENTATION_UNSPECIFIED: Integer;
  function _GetSCREEN_ORIENTATION_USER: Integer;
  function _GetUIOPTION_SPLIT_ACTION_BAR_WHEN_NARROW: Integer;
  {Methods}
  function init: JActivityInfo; cdecl; overload;
  function init(orig: JActivityInfo): JActivityInfo; cdecl; overload;
  {Properties}
  property CONFIG_DENSITY: Integer read _GetCONFIG_DENSITY;
  property CONFIG_FONT_SCALE: Integer read _GetCONFIG_FONT_SCALE;
  property CONFIG_KEYBOARD: Integer read _GetCONFIG_KEYBOARD;
  property CONFIG_KEYBOARD_HIDDEN: Integer read _GetCONFIG_KEYBOARD_HIDDEN;
  property CONFIG_LAYOUT_DIRECTION: Integer read _GetCONFIG_LAYOUT_DIRECTION;
  property CONFIG_LOCALE: Integer read _GetCONFIG_LOCALE;
  property CONFIG_MCC: Integer read _GetCONFIG_MCC;
  property CONFIG_MNC: Integer read _GetCONFIG_MNC;
  property CONFIG_NAVIGATION: Integer read _GetCONFIG_NAVIGATION;
  property CONFIG_ORIENTATION: Integer read _GetCONFIG_ORIENTATION;
  property CONFIG_SCREEN_LAYOUT: Integer read _GetCONFIG_SCREEN_LAYOUT;
  property CONFIG_SCREEN_SIZE: Integer read _GetCONFIG_SCREEN_SIZE;
  property CONFIG_SMALLEST_SCREEN_SIZE: Integer read _GetCONFIG_SMALLEST_SCREEN_SIZE;
  property CONFIG_TOUCHSCREEN: Integer read _GetCONFIG_TOUCHSCREEN;
  property CONFIG_UI_MODE: Integer read _GetCONFIG_UI_MODE;
  property CREATOR: JParcelable_Creator read _GetCREATOR;
  property FLAG_ALLOW_TASK_REPARENTING: Integer read _GetFLAG_ALLOW_TASK_REPARENTING;
  property FLAG_ALWAYS_RETAIN_TASK_STATE: Integer read _GetFLAG_ALWAYS_RETAIN_TASK_STATE;
  property FLAG_CLEAR_TASK_ON_LAUNCH: Integer read _GetFLAG_CLEAR_TASK_ON_LAUNCH;
  property FLAG_EXCLUDE_FROM_RECENTS: Integer read _GetFLAG_EXCLUDE_FROM_RECENTS;
  property FLAG_FINISH_ON_CLOSE_SYSTEM_DIALOGS: Integer read _GetFLAG_FINISH_ON_CLOSE_SYSTEM_DIALOGS;
  property FLAG_FINISH_ON_TASK_LAUNCH: Integer read _GetFLAG_FINISH_ON_TASK_LAUNCH;
  property FLAG_HARDWARE_ACCELERATED: Integer read _GetFLAG_HARDWARE_ACCELERATED;
  property FLAG_MULTIPROCESS: Integer read _GetFLAG_MULTIPROCESS;
  property FLAG_NO_HISTORY: Integer read _GetFLAG_NO_HISTORY;
  property FLAG_SINGLE_USER: Integer read _GetFLAG_SINGLE_USER;
  property FLAG_STATE_NOT_NEEDED: Integer read _GetFLAG_STATE_NOT_NEEDED;
  property LAUNCH_MULTIPLE: Integer read _GetLAUNCH_MULTIPLE;
  property LAUNCH_SINGLE_INSTANCE: Integer read _GetLAUNCH_SINGLE_INSTANCE;
  property LAUNCH_SINGLE_TASK: Integer read _GetLAUNCH_SINGLE_TASK;
  property LAUNCH_SINGLE_TOP: Integer read _GetLAUNCH_SINGLE_TOP;
  property SCREEN_ORIENTATION_BEHIND: Integer read _GetSCREEN_ORIENTATION_BEHIND;
  property SCREEN_ORIENTATION_FULL_SENSOR: Integer read _GetSCREEN_ORIENTATION_FULL_SENSOR;
  property SCREEN_ORIENTATION_LANDSCAPE: Integer read _GetSCREEN_ORIENTATION_LANDSCAPE;
  property SCREEN_ORIENTATION_NOSENSOR: Integer read _GetSCREEN_ORIENTATION_NOSENSOR;
  property SCREEN_ORIENTATION_PORTRAIT: Integer read _GetSCREEN_ORIENTATION_PORTRAIT;
  property SCREEN_ORIENTATION_REVERSE_LANDSCAPE: Integer read _GetSCREEN_ORIENTATION_REVERSE_LANDSCAPE;
  property SCREEN_ORIENTATION_REVERSE_PORTRAIT: Integer read _GetSCREEN_ORIENTATION_REVERSE_PORTRAIT;
  property SCREEN_ORIENTATION_SENSOR: Integer read _GetSCREEN_ORIENTATION_SENSOR;
  property SCREEN_ORIENTATION_SENSOR_LANDSCAPE: Integer read _GetSCREEN_ORIENTATION_SENSOR_LANDSCAPE;
  property SCREEN_ORIENTATION_SENSOR_PORTRAIT: Integer read _GetSCREEN_ORIENTATION_SENSOR_PORTRAIT;
  property SCREEN_ORIENTATION_UNSPECIFIED: Integer read _GetSCREEN_ORIENTATION_UNSPECIFIED;
  property SCREEN_ORIENTATION_USER: Integer read _GetSCREEN_ORIENTATION_USER;
  property UIOPTION_SPLIT_ACTION_BAR_WHEN_NARROW: Integer read _GetUIOPTION_SPLIT_ACTION_BAR_WHEN_NARROW;
end;

[JavaSignature('android/content/pm/ActivityInfo')]
JActivityInfo = interface(JComponentInfo)
['{9C1A2F33-54F9-49B7-8AE1-B3B10008B309}']
  {Property Methods}
  function _GetconfigChanges: Integer;
  procedure _SetconfigChanges(Value: Integer);
  function _Getflags: Integer;
  procedure _Setflags(Value: Integer);
  function _GetlaunchMode: Integer;
  procedure _SetlaunchMode(Value: Integer);
  function _GetparentActivityName: JString;
  procedure _SetparentActivityName(Value: JString);
  function _Getpermission: JString;
  procedure _Setpermission(Value: JString);
  function _GetscreenOrientation: Integer;
  procedure _SetscreenOrientation(Value: Integer);
  function _GetsoftInputMode: Integer;
  procedure _SetsoftInputMode(Value: Integer);
  function _GettargetActivity: JString;
  procedure _SettargetActivity(Value: JString);
  function _GettaskAffinity: JString;
  procedure _SettaskAffinity(Value: JString);
  function _Gettheme: Integer;
  procedure _Settheme(Value: Integer);
  function _GetuiOptions: Integer;
  procedure _SetuiOptions(Value: Integer);
  {Methods}
  function describeContents: Integer; cdecl;
  function getThemeResource: Integer; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; parcelableFlags: Integer); cdecl;
  {Properties}
  property configChanges: Integer read _GetconfigChanges write _SetconfigChanges;
  property flags: Integer read _Getflags write _Setflags;
  property launchMode: Integer read _GetlaunchMode write _SetlaunchMode;
  property parentActivityName: JString read _GetparentActivityName write _SetparentActivityName;
  property permission: JString read _Getpermission write _Setpermission;
  property screenOrientation: Integer read _GetscreenOrientation write _SetscreenOrientation;
  property softInputMode: Integer read _GetsoftInputMode write _SetsoftInputMode;
  property targetActivity: JString read _GettargetActivity write _SettargetActivity;
  property taskAffinity: JString read _GettaskAffinity write _SettaskAffinity;
  property theme: Integer read _Gettheme write _Settheme;
  property uiOptions: Integer read _GetuiOptions write _SetuiOptions;
end;
TJActivityInfo = class(TJavaGenericImport<JActivityInfoClass, JActivityInfo>) end;

JRectShapeClass = interface(JShapeClass)
['{78A92C31-C06A-4152-A353-B7D617A18387}']
  {Methods}
  function init: JRectShape; cdecl;
end;

[JavaSignature('android/graphics/drawable/shapes/RectShape')]
JRectShape = interface(JShape)
['{41677A83-E028-401E-BB0D-F1D563D58F57}']
  {Methods}
  function clone: JRectShape; cdecl;
  procedure draw(canvas: JCanvas; paint: JPaint); cdecl;
end;
TJRectShape = class(TJavaGenericImport<JRectShapeClass, JRectShape>) end;

JArcShapeClass = interface(JRectShapeClass)
['{29DCA278-564D-48F8-962C-142CCBA009CD}']
  {Methods}
  function init(startAngle: Single; sweepAngle: Single): JArcShape; cdecl;
end;

[JavaSignature('android/graphics/drawable/shapes/ArcShape')]
JArcShape = interface(JRectShape)
['{498B495C-A330-42D8-B5A8-09706EB738E0}']
  {Methods}
  procedure draw(canvas: JCanvas; paint: JPaint); cdecl;
end;
TJArcShape = class(TJavaGenericImport<JArcShapeClass, JArcShape>) end;

Jgraphics_CameraClass = interface(JObjectClass)
['{D3090283-790D-410F-8BE9-C3AD79AED7FD}']
  {Methods}
  function init: Jgraphics_Camera; cdecl;
end;

[JavaSignature('android/graphics/Camera')]
Jgraphics_Camera = interface(JObject)
['{C1A83D60-8064-4A14-9854-B70174E37610}']
  {Methods}
  procedure applyToCanvas(canvas: JCanvas); cdecl;
  function dotWithNormal(dx: Single; dy: Single; dz: Single): Single; cdecl;
  function getLocationX: Single; cdecl;
  function getLocationY: Single; cdecl;
  function getLocationZ: Single; cdecl;
  procedure getMatrix(matrix: JMatrix); cdecl;
  procedure restore; cdecl;
  procedure rotate(x: Single; y: Single; z: Single); cdecl;
  procedure rotateX(deg: Single); cdecl;
  procedure rotateY(deg: Single); cdecl;
  procedure rotateZ(deg: Single); cdecl;
  procedure save; cdecl;
  procedure setLocation(x: Single; y: Single; z: Single); cdecl;
  procedure translate(x: Single; y: Single; z: Single); cdecl;
end;
TJgraphics_Camera = class(TJavaGenericImport<Jgraphics_CameraClass, Jgraphics_Camera>) end;

JPaint_StyleClass = interface(JEnumClass)
['{9868584E-BF88-4BEF-96EA-67323FD4DB5E}']
  {Property Methods}
  function _GetFILL: JPaint_Style;
  function _GetFILL_AND_STROKE: JPaint_Style;
  function _GetSTROKE: JPaint_Style;
  {Methods}
  function valueOf(name: JString): JPaint_Style; cdecl;
  function values: TJavaObjectArray<JPaint_Style>; cdecl;
  {Properties}
  property FILL: JPaint_Style read _GetFILL;
  property FILL_AND_STROKE: JPaint_Style read _GetFILL_AND_STROKE;
  property STROKE: JPaint_Style read _GetSTROKE;
end;

[JavaSignature('android/graphics/Paint$Style')]
JPaint_Style = interface(JEnum)
['{1BB91885-3BDE-4C4F-A7AC-C9C676DEC571}']
end;
TJPaint_Style = class(TJavaGenericImport<JPaint_StyleClass, JPaint_Style>) end;

JActionProvider_VisibilityListenerClass = interface(IJavaClass)
['{DA44FA03-BC67-4E62-9F6F-6F6065A24844}']
end;

[JavaSignature('android/view/ActionProvider$VisibilityListener')]
JActionProvider_VisibilityListener = interface(IJavaInstance)
['{C7394E05-61E3-4463-B618-DA0802949CFD}']
  {Methods}
  procedure onActionProviderVisibilityChanged(isVisible: Boolean); cdecl;
end;
TJActionProvider_VisibilityListener = class(TJavaGenericImport<JActionProvider_VisibilityListenerClass, JActionProvider_VisibilityListener>) end;

JLayout_AlignmentClass = interface(JEnumClass)
['{9C9DD4F5-EF2D-4920-8C97-F5BA540F393D}']
  {Property Methods}
  function _GetALIGN_CENTER: JLayout_Alignment;
  function _GetALIGN_NORMAL: JLayout_Alignment;
  function _GetALIGN_OPPOSITE: JLayout_Alignment;
  {Methods}
  function valueOf(name: JString): JLayout_Alignment; cdecl;
  function values: TJavaObjectArray<JLayout_Alignment>; cdecl;
  {Properties}
  property ALIGN_CENTER: JLayout_Alignment read _GetALIGN_CENTER;
  property ALIGN_NORMAL: JLayout_Alignment read _GetALIGN_NORMAL;
  property ALIGN_OPPOSITE: JLayout_Alignment read _GetALIGN_OPPOSITE;
end;

[JavaSignature('android/text/Layout$Alignment')]
JLayout_Alignment = interface(JEnum)
['{93E74BAF-E0DC-4F08-8E4D-C58CA4EAB0AE}']
end;
TJLayout_Alignment = class(TJavaGenericImport<JLayout_AlignmentClass, JLayout_Alignment>) end;

JActionProviderClass = interface(JObjectClass)
['{9D317490-3FE1-4DFA-88A0-4C16C5CBF881}']
  {Methods}
  function init(context: JContext): JActionProvider; cdecl;
end;

[JavaSignature('android/view/ActionProvider')]
JActionProvider = interface(JObject)
['{ABB5DE02-6FEB-49A1-881C-90A35AA8E477}']
  {Methods}
  function hasSubMenu: Boolean; cdecl;
  function isVisible: Boolean; cdecl;
  function onCreateActionView: JView; cdecl; overload;//Deprecated
  function onCreateActionView(forItem: JMenuItem): JView; cdecl; overload;
  function onPerformDefaultAction: Boolean; cdecl;
  procedure onPrepareSubMenu(subMenu: JSubMenu); cdecl;
  function overridesItemVisibility: Boolean; cdecl;
  procedure refreshVisibility; cdecl;
  procedure setVisibilityListener(listener: JActionProvider_VisibilityListener); cdecl;
end;
TJActionProvider = class(TJavaGenericImport<JActionProviderClass, JActionProvider>) end;

JSpannedClass = interface(JCharSequenceClass)
['{9F00483B-3D41-4307-A016-4CCC278AAB57}']
  {Property Methods}
  function _GetSPAN_COMPOSING: Integer;
  function _GetSPAN_EXCLUSIVE_EXCLUSIVE: Integer;
  function _GetSPAN_EXCLUSIVE_INCLUSIVE: Integer;
  function _GetSPAN_INCLUSIVE_EXCLUSIVE: Integer;
  function _GetSPAN_INCLUSIVE_INCLUSIVE: Integer;
  function _GetSPAN_INTERMEDIATE: Integer;
  function _GetSPAN_MARK_MARK: Integer;
  function _GetSPAN_MARK_POINT: Integer;
  function _GetSPAN_PARAGRAPH: Integer;
  function _GetSPAN_POINT_MARK: Integer;
  function _GetSPAN_POINT_MARK_MASK: Integer;
  function _GetSPAN_POINT_POINT: Integer;
  function _GetSPAN_PRIORITY: Integer;
  function _GetSPAN_PRIORITY_SHIFT: Integer;
  function _GetSPAN_USER: Integer;
  function _GetSPAN_USER_SHIFT: Integer;
  {Properties}
  property SPAN_COMPOSING: Integer read _GetSPAN_COMPOSING;
  property SPAN_EXCLUSIVE_EXCLUSIVE: Integer read _GetSPAN_EXCLUSIVE_EXCLUSIVE;
  property SPAN_EXCLUSIVE_INCLUSIVE: Integer read _GetSPAN_EXCLUSIVE_INCLUSIVE;
  property SPAN_INCLUSIVE_EXCLUSIVE: Integer read _GetSPAN_INCLUSIVE_EXCLUSIVE;
  property SPAN_INCLUSIVE_INCLUSIVE: Integer read _GetSPAN_INCLUSIVE_INCLUSIVE;
  property SPAN_INTERMEDIATE: Integer read _GetSPAN_INTERMEDIATE;
  property SPAN_MARK_MARK: Integer read _GetSPAN_MARK_MARK;
  property SPAN_MARK_POINT: Integer read _GetSPAN_MARK_POINT;
  property SPAN_PARAGRAPH: Integer read _GetSPAN_PARAGRAPH;
  property SPAN_POINT_MARK: Integer read _GetSPAN_POINT_MARK;
  property SPAN_POINT_MARK_MASK: Integer read _GetSPAN_POINT_MARK_MASK;
  property SPAN_POINT_POINT: Integer read _GetSPAN_POINT_POINT;
  property SPAN_PRIORITY: Integer read _GetSPAN_PRIORITY;
  property SPAN_PRIORITY_SHIFT: Integer read _GetSPAN_PRIORITY_SHIFT;
  property SPAN_USER: Integer read _GetSPAN_USER;
  property SPAN_USER_SHIFT: Integer read _GetSPAN_USER_SHIFT;
end;

[JavaSignature('android/text/Spanned')]
JSpanned = interface(JCharSequence)
['{534DAF98-ED56-41E4-8CCC-74687680B667}']
  {Methods}
  function getSpanEnd(tag: JObject): Integer; cdecl;
  function getSpanFlags(tag: JObject): Integer; cdecl;
  function getSpanStart(tag: JObject): Integer; cdecl;
  function getSpans(start: Integer; end_: Integer; type_: Jlang_Class): TJavaObjectArray<JObject>; cdecl;
  function nextSpanTransition(start: Integer; limit: Integer; type_: Jlang_Class): Integer; cdecl;
end;
TJSpanned = class(TJavaGenericImport<JSpannedClass, JSpanned>) end;

JSQLiteCursorDriverClass = interface(IJavaClass)
['{2D28FDEF-35BB-4B78-87FB-8AAFF86DC43B}']
end;

[JavaSignature('android/database/sqlite/SQLiteCursorDriver')]
JSQLiteCursorDriver = interface(IJavaInstance)
['{6714A968-83B7-41C7-9657-56ADF0D05F07}']
  {Methods}
  procedure cursorClosed; cdecl;
  procedure cursorDeactivated; cdecl;
  procedure cursorRequeried(cursor: JCursor); cdecl;
  function query(factory: JSQLiteDatabase_CursorFactory; bindArgs: TJavaObjectArray<JString>): JCursor; cdecl;
  procedure setBindArguments(bindArgs: TJavaObjectArray<JString>); cdecl;
end;
TJSQLiteCursorDriver = class(TJavaGenericImport<JSQLiteCursorDriverClass, JSQLiteCursorDriver>) end;

JDiscretePathEffectClass = interface(JPathEffectClass)
['{94123CD0-45CB-4A02-A07F-DC8429292802}']
  {Methods}
  function init(segmentLength: Single; deviation: Single): JDiscretePathEffect; cdecl;
end;

[JavaSignature('android/graphics/DiscretePathEffect')]
JDiscretePathEffect = interface(JPathEffect)
['{D1C123BD-6C32-49F4-AB15-6E20B4BC0B56}']
end;
TJDiscretePathEffect = class(TJavaGenericImport<JDiscretePathEffectClass, JDiscretePathEffect>) end;

JDialogInterface_OnClickListenerClass = interface(IJavaClass)
['{4D55282C-7A9D-43DB-879D-2C1EFC59953E}']
end;

[JavaSignature('android/content/DialogInterface$OnClickListener')]
JDialogInterface_OnClickListener = interface(IJavaInstance)
['{060B7391-D53F-4BAD-A491-5D50CED3CF9A}']
  {Methods}
  procedure onClick(dialog: JDialogInterface; which: Integer); cdecl;
end;
TJDialogInterface_OnClickListener = class(TJavaGenericImport<JDialogInterface_OnClickListenerClass, JDialogInterface_OnClickListener>) end;

JMenuItem_OnMenuItemClickListenerClass = interface(IJavaClass)
['{5D80AB8B-B86B-4B92-91CE-B0928D17937F}']
end;

[JavaSignature('android/view/MenuItem$OnMenuItemClickListener')]
JMenuItem_OnMenuItemClickListener = interface(IJavaInstance)
['{D9AEABD0-0126-4F9E-B7F0-EB4C0F6477BC}']
  {Methods}
  function onMenuItemClick(item: JMenuItem): Boolean; cdecl;
end;
TJMenuItem_OnMenuItemClickListener = class(TJavaGenericImport<JMenuItem_OnMenuItemClickListenerClass, JMenuItem_OnMenuItemClickListener>) end;

JViewTreeObserver_OnPreDrawListenerClass = interface(IJavaClass)
['{7C6C84FC-8B94-4BAF-8A45-719D67BF6156}']
end;

[JavaSignature('android/view/ViewTreeObserver$OnPreDrawListener')]
JViewTreeObserver_OnPreDrawListener = interface(IJavaInstance)
['{D9958504-8A29-450B-AF99-9711FE18D731}']
  {Methods}
  function onPreDraw: Boolean; cdecl;
end;
TJViewTreeObserver_OnPreDrawListener = class(TJavaGenericImport<JViewTreeObserver_OnPreDrawListenerClass, JViewTreeObserver_OnPreDrawListener>) end;

JIntentFilterClass = interface(JObjectClass)
['{D8E327D8-DE10-4013-980F-63D2B7E5437B}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  function _GetMATCH_ADJUSTMENT_MASK: Integer;
  function _GetMATCH_ADJUSTMENT_NORMAL: Integer;
  function _GetMATCH_CATEGORY_EMPTY: Integer;
  function _GetMATCH_CATEGORY_HOST: Integer;
  function _GetMATCH_CATEGORY_MASK: Integer;
  function _GetMATCH_CATEGORY_PATH: Integer;
  function _GetMATCH_CATEGORY_PORT: Integer;
  function _GetMATCH_CATEGORY_SCHEME: Integer;
  function _GetMATCH_CATEGORY_TYPE: Integer;
  function _GetNO_MATCH_ACTION: Integer;
  function _GetNO_MATCH_CATEGORY: Integer;
  function _GetNO_MATCH_DATA: Integer;
  function _GetNO_MATCH_TYPE: Integer;
  function _GetSYSTEM_HIGH_PRIORITY: Integer;
  function _GetSYSTEM_LOW_PRIORITY: Integer;
  {Methods}
  function init: JIntentFilter; cdecl; overload;
  function init(action: JString): JIntentFilter; cdecl; overload;
  function init(action: JString; dataType: JString): JIntentFilter; cdecl; overload;
  function init(o: JIntentFilter): JIntentFilter; cdecl; overload;
  function create(action: JString; dataType: JString): JIntentFilter; cdecl;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
  property MATCH_ADJUSTMENT_MASK: Integer read _GetMATCH_ADJUSTMENT_MASK;
  property MATCH_ADJUSTMENT_NORMAL: Integer read _GetMATCH_ADJUSTMENT_NORMAL;
  property MATCH_CATEGORY_EMPTY: Integer read _GetMATCH_CATEGORY_EMPTY;
  property MATCH_CATEGORY_HOST: Integer read _GetMATCH_CATEGORY_HOST;
  property MATCH_CATEGORY_MASK: Integer read _GetMATCH_CATEGORY_MASK;
  property MATCH_CATEGORY_PATH: Integer read _GetMATCH_CATEGORY_PATH;
  property MATCH_CATEGORY_PORT: Integer read _GetMATCH_CATEGORY_PORT;
  property MATCH_CATEGORY_SCHEME: Integer read _GetMATCH_CATEGORY_SCHEME;
  property MATCH_CATEGORY_TYPE: Integer read _GetMATCH_CATEGORY_TYPE;
  property NO_MATCH_ACTION: Integer read _GetNO_MATCH_ACTION;
  property NO_MATCH_CATEGORY: Integer read _GetNO_MATCH_CATEGORY;
  property NO_MATCH_DATA: Integer read _GetNO_MATCH_DATA;
  property NO_MATCH_TYPE: Integer read _GetNO_MATCH_TYPE;
  property SYSTEM_HIGH_PRIORITY: Integer read _GetSYSTEM_HIGH_PRIORITY;
  property SYSTEM_LOW_PRIORITY: Integer read _GetSYSTEM_LOW_PRIORITY;
end;

[JavaSignature('android/content/IntentFilter')]
JIntentFilter = interface(JObject)
['{A40FA09B-35DE-4B8E-8E74-89383527F0DF}']
  {Methods}
  function actionsIterator: JIterator; cdecl;
  procedure addAction(action: JString); cdecl;
  procedure addCategory(category: JString); cdecl;
  procedure addDataAuthority(host: JString; port: JString); cdecl;
  procedure addDataPath(path: JString; type_: Integer); cdecl;
  procedure addDataScheme(scheme: JString); cdecl;
  procedure addDataType(type_: JString); cdecl;
  function authoritiesIterator: JIterator; cdecl;
  function categoriesIterator: JIterator; cdecl;
  function countActions: Integer; cdecl;
  function countCategories: Integer; cdecl;
  function countDataAuthorities: Integer; cdecl;
  function countDataPaths: Integer; cdecl;
  function countDataSchemes: Integer; cdecl;
  function countDataTypes: Integer; cdecl;
  function describeContents: Integer; cdecl;
  function getAction(index: Integer): JString; cdecl;
  function getCategory(index: Integer): JString; cdecl;
  function getDataAuthority(index: Integer): JIntentFilter_AuthorityEntry; cdecl;
  function getDataPath(index: Integer): JPatternMatcher; cdecl;
  function getDataScheme(index: Integer): JString; cdecl;
  function getDataType(index: Integer): JString; cdecl;
  function getPriority: Integer; cdecl;
  function hasAction(action: JString): Boolean; cdecl;
  function hasCategory(category: JString): Boolean; cdecl;
  function hasDataAuthority(data: Jnet_Uri): Boolean; cdecl;
  function hasDataPath(data: JString): Boolean; cdecl;
  function hasDataScheme(scheme: JString): Boolean; cdecl;
  function hasDataType(type_: JString): Boolean; cdecl;
  function match(resolver: JContentResolver; intent: JIntent; resolve: Boolean; logTag: JString): Integer; cdecl; overload;
  function match(action: JString; type_: JString; scheme: JString; data: Jnet_Uri; categories: JSet; logTag: JString): Integer; cdecl; overload;
  function matchAction(action: JString): Boolean; cdecl;
  function matchCategories(categories: JSet): JString; cdecl;
  function matchData(type_: JString; scheme: JString; data: Jnet_Uri): Integer; cdecl;
  function matchDataAuthority(data: Jnet_Uri): Integer; cdecl;
  function pathsIterator: JIterator; cdecl;
  function schemesIterator: JIterator; cdecl;
  procedure setPriority(priority: Integer); cdecl;
  function typesIterator: JIterator; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
end;
TJIntentFilter = class(TJavaGenericImport<JIntentFilterClass, JIntentFilter>) end;

JInputMethodManagerClass = interface(JObjectClass)
['{EBEE74C0-AA6B-4588-92FC-18172558D880}']
  {Property Methods}
  function _GetHIDE_IMPLICIT_ONLY: Integer;
  function _GetHIDE_NOT_ALWAYS: Integer;
  function _GetRESULT_HIDDEN: Integer;
  function _GetRESULT_SHOWN: Integer;
  function _GetRESULT_UNCHANGED_HIDDEN: Integer;
  function _GetRESULT_UNCHANGED_SHOWN: Integer;
  function _GetSHOW_FORCED: Integer;
  function _GetSHOW_IMPLICIT: Integer;
  {Properties}
  property HIDE_IMPLICIT_ONLY: Integer read _GetHIDE_IMPLICIT_ONLY;
  property HIDE_NOT_ALWAYS: Integer read _GetHIDE_NOT_ALWAYS;
  property RESULT_HIDDEN: Integer read _GetRESULT_HIDDEN;
  property RESULT_SHOWN: Integer read _GetRESULT_SHOWN;
  property RESULT_UNCHANGED_HIDDEN: Integer read _GetRESULT_UNCHANGED_HIDDEN;
  property RESULT_UNCHANGED_SHOWN: Integer read _GetRESULT_UNCHANGED_SHOWN;
  property SHOW_FORCED: Integer read _GetSHOW_FORCED;
  property SHOW_IMPLICIT: Integer read _GetSHOW_IMPLICIT;
end;

[JavaSignature('android/view/inputmethod/InputMethodManager')]
JInputMethodManager = interface(JObject)
['{D2C04F98-0614-4251-B7A7-5479A0EFDFE2}']
  {Methods}
  procedure displayCompletions(view: JView; completions: TJavaObjectArray<JCompletionInfo>); cdecl;
  function getCurrentInputMethodSubtype: JInputMethodSubtype; cdecl;
  function getEnabledInputMethodList: JList; cdecl;
  function getEnabledInputMethodSubtypeList(imi: JInputMethodInfo; allowsImplicitlySelectedSubtypes: Boolean): JList; cdecl;
  function getInputMethodList: JList; cdecl;
  function getLastInputMethodSubtype: JInputMethodSubtype; cdecl;
  function getShortcutInputMethodsAndSubtypes: JMap; cdecl;
  procedure hideSoftInputFromInputMethod(token: JIBinder; flags: Integer); cdecl;
  function hideSoftInputFromWindow(windowToken: JIBinder; flags: Integer): Boolean; cdecl; overload;
  function hideSoftInputFromWindow(windowToken: JIBinder; flags: Integer; resultReceiver: JResultReceiver): Boolean; cdecl; overload;
  procedure hideStatusIcon(imeToken: JIBinder); cdecl;
  function isAcceptingText: Boolean; cdecl;
  function isActive(view: JView): Boolean; cdecl; overload;
  function isActive: Boolean; cdecl; overload;
  function isFullscreenMode: Boolean; cdecl;
  function isWatchingCursor(view: JView): Boolean; cdecl;
  procedure restartInput(view: JView); cdecl;
  procedure sendAppPrivateCommand(view: JView; action: JString; data: JBundle); cdecl;
  procedure setAdditionalInputMethodSubtypes(imiId: JString; subtypes: TJavaObjectArray<JInputMethodSubtype>); cdecl;
  function setCurrentInputMethodSubtype(subtype: JInputMethodSubtype): Boolean; cdecl;
  procedure setInputMethod(token: JIBinder; id: JString); cdecl;
  procedure setInputMethodAndSubtype(token: JIBinder; id: JString; subtype: JInputMethodSubtype); cdecl;
  procedure showInputMethodAndSubtypeEnabler(imiId: JString); cdecl;
  procedure showInputMethodPicker; cdecl;
  function showSoftInput(view: JView; flags: Integer): Boolean; cdecl; overload;
  function showSoftInput(view: JView; flags: Integer; resultReceiver: JResultReceiver): Boolean; cdecl; overload;
  procedure showSoftInputFromInputMethod(token: JIBinder; flags: Integer); cdecl;
  procedure showStatusIcon(imeToken: JIBinder; packageName: JString; iconId: Integer); cdecl;
  function switchToLastInputMethod(imeToken: JIBinder): Boolean; cdecl;
  function switchToNextInputMethod(imeToken: JIBinder; onlyCurrentIme: Boolean): Boolean; cdecl;
  procedure toggleSoftInput(showFlags: Integer; hideFlags: Integer); cdecl;
  procedure toggleSoftInputFromWindow(windowToken: JIBinder; showFlags: Integer; hideFlags: Integer); cdecl;
  procedure updateCursor(view: JView; left: Integer; top: Integer; right: Integer; bottom: Integer); cdecl;
  procedure updateExtractedText(view: JView; token: Integer; text: JExtractedText); cdecl;
  procedure updateSelection(view: JView; selStart: Integer; selEnd: Integer; candidatesStart: Integer; candidatesEnd: Integer); cdecl;
  procedure viewClicked(view: JView); cdecl;
end;
TJInputMethodManager = class(TJavaGenericImport<JInputMethodManagerClass, JInputMethodManager>) end;

JScaleGestureDetectorClass = interface(JObjectClass)
['{E1C3BFB3-676A-4844-A4C5-35BB02B14F37}']
  {Methods}
  function init(context: JContext; listener: JScaleGestureDetector_OnScaleGestureListener): JScaleGestureDetector; cdecl;
end;

[JavaSignature('android/view/ScaleGestureDetector')]
JScaleGestureDetector = interface(JObject)
['{A074A59C-6357-46C5-93AB-EB9978E6F817}']
  {Methods}
  function getCurrentSpan: Single; cdecl;
  function getCurrentSpanX: Single; cdecl;
  function getCurrentSpanY: Single; cdecl;
  function getEventTime: Int64; cdecl;
  function getFocusX: Single; cdecl;
  function getFocusY: Single; cdecl;
  function getPreviousSpan: Single; cdecl;
  function getPreviousSpanX: Single; cdecl;
  function getPreviousSpanY: Single; cdecl;
  function getScaleFactor: Single; cdecl;
  function getTimeDelta: Int64; cdecl;
  function isInProgress: Boolean; cdecl;
  function onTouchEvent(event: JMotionEvent): Boolean; cdecl;
end;
TJScaleGestureDetector = class(TJavaGenericImport<JScaleGestureDetectorClass, JScaleGestureDetector>) end;

JSurfaceHolder_Callback2Class = interface(JSurfaceHolder_CallbackClass)
['{5396FDAA-D490-4AE6-B8CA-1D9B332A93D9}']
end;

[JavaSignature('android/view/SurfaceHolder$Callback2')]
JSurfaceHolder_Callback2 = interface(JSurfaceHolder_Callback)
['{6D51F2B2-F52C-47D0-ABDC-1469F1C3EB42}']
  {Methods}
  procedure surfaceRedrawNeeded(holder: JSurfaceHolder); cdecl;
end;
TJSurfaceHolder_Callback2 = class(TJavaGenericImport<JSurfaceHolder_Callback2Class, JSurfaceHolder_Callback2>) end;

JXmlResourceParserClass = interface(JAttributeSetClass)
['{E007E94A-8D69-4803-8C66-29C9AC6E1235}']
end;

[JavaSignature('android/content/res/XmlResourceParser')]
JXmlResourceParser = interface(JAttributeSet)
['{D808A282-BBE2-4A1D-B909-82ACDFE2A84E}']
  {Methods}
  procedure close; cdecl;
end;
TJXmlResourceParser = class(TJavaGenericImport<JXmlResourceParserClass, JXmlResourceParser>) end;

JPaint_AlignClass = interface(JEnumClass)
['{CA524B48-AD2A-4679-8554-9BA634A758A8}']
  {Property Methods}
  function _GetCENTER: JPaint_Align;
  function _GetLEFT: JPaint_Align;
  function _GetRIGHT: JPaint_Align;
  {Methods}
  function valueOf(name: JString): JPaint_Align; cdecl;
  function values: TJavaObjectArray<JPaint_Align>; cdecl;
  {Properties}
  property CENTER: JPaint_Align read _GetCENTER;
  property LEFT: JPaint_Align read _GetLEFT;
  property RIGHT: JPaint_Align read _GetRIGHT;
end;

[JavaSignature('android/graphics/Paint$Align')]
JPaint_Align = interface(JEnum)
['{E5958A36-025D-497A-87AD-4AEE6297BEED}']
end;
TJPaint_Align = class(TJavaGenericImport<JPaint_AlignClass, JPaint_Align>) end;

JSpannableClass = interface(JSpannedClass)
['{5FAF8DF3-F895-499D-9C35-1F4F5575A22C}']
end;

[JavaSignature('android/text/Spannable')]
JSpannable = interface(JSpanned)
['{D9F023CA-ADC4-4C2F-A7F7-8B44324183B2}']
  {Methods}
  procedure removeSpan(what: JObject); cdecl;
  procedure setSpan(what: JObject; start: Integer; end_: Integer; flags: Integer); cdecl;
end;
TJSpannable = class(TJavaGenericImport<JSpannableClass, JSpannable>) end;

JContentResolverClass = interface(JObjectClass)
['{29F2ED97-64A0-435B-A79C-7B8F80E6659A}']
  {Property Methods}
  function _GetCURSOR_DIR_BASE_TYPE: JString;
  function _GetCURSOR_ITEM_BASE_TYPE: JString;
  function _GetSCHEME_ANDROID_RESOURCE: JString;
  function _GetSCHEME_CONTENT: JString;
  function _GetSCHEME_FILE: JString;
  function _GetSYNC_EXTRAS_ACCOUNT: JString;
  function _GetSYNC_EXTRAS_DISCARD_LOCAL_DELETIONS: JString;
  function _GetSYNC_EXTRAS_DO_NOT_RETRY: JString;
  function _GetSYNC_EXTRAS_EXPEDITED: JString;
  function _GetSYNC_EXTRAS_FORCE: JString;
  function _GetSYNC_EXTRAS_IGNORE_BACKOFF: JString;
  function _GetSYNC_EXTRAS_IGNORE_SETTINGS: JString;
  function _GetSYNC_EXTRAS_INITIALIZE: JString;
  function _GetSYNC_EXTRAS_MANUAL: JString;
  function _GetSYNC_EXTRAS_OVERRIDE_TOO_MANY_DELETIONS: JString;
  function _GetSYNC_EXTRAS_UPLOAD: JString;
  function _GetSYNC_OBSERVER_TYPE_ACTIVE: Integer;
  function _GetSYNC_OBSERVER_TYPE_PENDING: Integer;
  function _GetSYNC_OBSERVER_TYPE_SETTINGS: Integer;
  {Methods}
  function init(context: JContext): JContentResolver; cdecl;
  function addStatusChangeListener(mask: Integer; callback: JSyncStatusObserver): JObject; cdecl;
  function getCurrentSync: JSyncInfo; cdecl;//Deprecated
  function getCurrentSyncs: JList; cdecl;
  function getMasterSyncAutomatically: Boolean; cdecl;
  function getSyncAdapterTypes: TJavaObjectArray<JSyncAdapterType>; cdecl;
  procedure removeStatusChangeListener(handle: JObject); cdecl;
  procedure setMasterSyncAutomatically(sync: Boolean); cdecl;
  procedure validateSyncExtrasBundle(extras: JBundle); cdecl;
  {Properties}
  property CURSOR_DIR_BASE_TYPE: JString read _GetCURSOR_DIR_BASE_TYPE;
  property CURSOR_ITEM_BASE_TYPE: JString read _GetCURSOR_ITEM_BASE_TYPE;
  property SCHEME_ANDROID_RESOURCE: JString read _GetSCHEME_ANDROID_RESOURCE;
  property SCHEME_CONTENT: JString read _GetSCHEME_CONTENT;
  property SCHEME_FILE: JString read _GetSCHEME_FILE;
  property SYNC_EXTRAS_ACCOUNT: JString read _GetSYNC_EXTRAS_ACCOUNT;
  property SYNC_EXTRAS_DISCARD_LOCAL_DELETIONS: JString read _GetSYNC_EXTRAS_DISCARD_LOCAL_DELETIONS;
  property SYNC_EXTRAS_DO_NOT_RETRY: JString read _GetSYNC_EXTRAS_DO_NOT_RETRY;
  property SYNC_EXTRAS_EXPEDITED: JString read _GetSYNC_EXTRAS_EXPEDITED;
  property SYNC_EXTRAS_FORCE: JString read _GetSYNC_EXTRAS_FORCE;
  property SYNC_EXTRAS_IGNORE_BACKOFF: JString read _GetSYNC_EXTRAS_IGNORE_BACKOFF;
  property SYNC_EXTRAS_IGNORE_SETTINGS: JString read _GetSYNC_EXTRAS_IGNORE_SETTINGS;
  property SYNC_EXTRAS_INITIALIZE: JString read _GetSYNC_EXTRAS_INITIALIZE;
  property SYNC_EXTRAS_MANUAL: JString read _GetSYNC_EXTRAS_MANUAL;
  property SYNC_EXTRAS_OVERRIDE_TOO_MANY_DELETIONS: JString read _GetSYNC_EXTRAS_OVERRIDE_TOO_MANY_DELETIONS;
  property SYNC_EXTRAS_UPLOAD: JString read _GetSYNC_EXTRAS_UPLOAD;
  property SYNC_OBSERVER_TYPE_ACTIVE: Integer read _GetSYNC_OBSERVER_TYPE_ACTIVE;
  property SYNC_OBSERVER_TYPE_PENDING: Integer read _GetSYNC_OBSERVER_TYPE_PENDING;
  property SYNC_OBSERVER_TYPE_SETTINGS: Integer read _GetSYNC_OBSERVER_TYPE_SETTINGS;
end;

[JavaSignature('android/content/ContentResolver')]
JContentResolver = interface(JObject)
['{774C50C1-66DC-489E-9CAC-5434A5DE7CE0}']
  {Methods}
  function acquireContentProviderClient(uri: Jnet_Uri): JContentProviderClient; cdecl; overload;
  function acquireContentProviderClient(name: JString): JContentProviderClient; cdecl; overload;
  function acquireUnstableContentProviderClient(uri: Jnet_Uri): JContentProviderClient; cdecl; overload;
  function acquireUnstableContentProviderClient(name: JString): JContentProviderClient; cdecl; overload;
  function applyBatch(authority: JString; operations: JArrayList): TJavaObjectArray<JContentProviderResult>; cdecl;
  function bulkInsert(url: Jnet_Uri; values: TJavaObjectArray<JContentValues>): Integer; cdecl;
  function call(uri: Jnet_Uri; method: JString; arg: JString; extras: JBundle): JBundle; cdecl;
  procedure cancelSync(uri: Jnet_Uri); cdecl;//Deprecated
  function delete(url: Jnet_Uri; where: JString; selectionArgs: TJavaObjectArray<JString>): Integer; cdecl;
  function getStreamTypes(url: Jnet_Uri; mimeTypeFilter: JString): TJavaObjectArray<JString>; cdecl;
  function getType(url: Jnet_Uri): JString; cdecl;
  function insert(url: Jnet_Uri; values: JContentValues): Jnet_Uri; cdecl;
  procedure notifyChange(uri: Jnet_Uri; observer: JContentObserver); cdecl; overload;
  procedure notifyChange(uri: Jnet_Uri; observer: JContentObserver; syncToNetwork: Boolean); cdecl; overload;
  function openAssetFileDescriptor(uri: Jnet_Uri; mode: JString): JAssetFileDescriptor; cdecl;
  function openFileDescriptor(uri: Jnet_Uri; mode: JString): JParcelFileDescriptor; cdecl;
  function openInputStream(uri: Jnet_Uri): JInputStream; cdecl;
  function openOutputStream(uri: Jnet_Uri): JOutputStream; cdecl; overload;
  function openOutputStream(uri: Jnet_Uri; mode: JString): JOutputStream; cdecl; overload;
  function openTypedAssetFileDescriptor(uri: Jnet_Uri; mimeType: JString; opts: JBundle): JAssetFileDescriptor; cdecl;
  function query(uri: Jnet_Uri; projection: TJavaObjectArray<JString>; selection: JString; selectionArgs: TJavaObjectArray<JString>; sortOrder: JString): JCursor; cdecl; overload;
  function query(uri: Jnet_Uri; projection: TJavaObjectArray<JString>; selection: JString; selectionArgs: TJavaObjectArray<JString>; sortOrder: JString; cancellationSignal: JCancellationSignal): JCursor; cdecl; overload;
  procedure registerContentObserver(uri: Jnet_Uri; notifyForDescendents: Boolean; observer: JContentObserver); cdecl;
  procedure startSync(uri: Jnet_Uri; extras: JBundle); cdecl;//Deprecated
  procedure unregisterContentObserver(observer: JContentObserver); cdecl;
  function update(uri: Jnet_Uri; values: JContentValues; where: JString; selectionArgs: TJavaObjectArray<JString>): Integer; cdecl;
end;
TJContentResolver = class(TJavaGenericImport<JContentResolverClass, JContentResolver>) end;

JResourcesClass = interface(JObjectClass)
['{FB4A6A0F-8AF6-4EDC-81E8-13B3253D9857}']
  {Methods}
  function init(assets: JAssetManager; metrics: JDisplayMetrics; config: JConfiguration): JResources; cdecl;
  function getSystem: JResources; cdecl;
end;

[JavaSignature('android/content/res/Resources')]
JResources = interface(JObject)
['{ADD30FF7-D701-41CF-A710-1149B206FCE0}']
  {Methods}
  procedure finishPreloading; cdecl;
  procedure flushLayoutCache; cdecl;
  function getAnimation(id: Integer): JXmlResourceParser; cdecl;
  function getAssets: JAssetManager; cdecl;
  function getBoolean(id: Integer): Boolean; cdecl;
  function getColor(id: Integer): Integer; cdecl;
  function getColorStateList(id: Integer): JColorStateList; cdecl;
  function getConfiguration: JConfiguration; cdecl;
  function getDimension(id: Integer): Single; cdecl;
  function getDimensionPixelOffset(id: Integer): Integer; cdecl;
  function getDimensionPixelSize(id: Integer): Integer; cdecl;
  function getDisplayMetrics: JDisplayMetrics; cdecl;
  function getDrawable(id: Integer): JDrawable; cdecl;
  function getDrawableForDensity(id: Integer; density: Integer): JDrawable; cdecl;
  function getFraction(id: Integer; base: Integer; pbase: Integer): Single; cdecl;
  function getIdentifier(name: JString; defType: JString; defPackage: JString): Integer; cdecl;
  function getIntArray(id: Integer): TJavaArray<Integer>; cdecl;
  function getInteger(id: Integer): Integer; cdecl;
  function getLayout(id: Integer): JXmlResourceParser; cdecl;
  function getMovie(id: Integer): JMovie; cdecl;
  function getQuantityString(id: Integer; quantity: Integer): JString; cdecl; overload;
  function getQuantityText(id: Integer; quantity: Integer): JCharSequence; cdecl;
  function getResourceEntryName(resid: Integer): JString; cdecl;
  function getResourceName(resid: Integer): JString; cdecl;
  function getResourcePackageName(resid: Integer): JString; cdecl;
  function getResourceTypeName(resid: Integer): JString; cdecl;
  function getString(id: Integer): JString; cdecl; overload;
  function getStringArray(id: Integer): TJavaObjectArray<JString>; cdecl;
  function getText(id: Integer): JCharSequence; cdecl; overload;
  function getText(id: Integer; def: JCharSequence): JCharSequence; cdecl; overload;
  function getTextArray(id: Integer): TJavaObjectArray<JCharSequence>; cdecl;
  procedure getValue(id: Integer; outValue: JTypedValue; resolveRefs: Boolean); cdecl; overload;
  procedure getValue(name: JString; outValue: JTypedValue; resolveRefs: Boolean); cdecl; overload;
  procedure getValueForDensity(id: Integer; density: Integer; outValue: JTypedValue; resolveRefs: Boolean); cdecl;
  function getXml(id: Integer): JXmlResourceParser; cdecl;
  function newTheme: JResources_Theme; cdecl;
  function obtainAttributes(set_: JAttributeSet; attrs: TJavaArray<Integer>): JTypedArray; cdecl;
  function obtainTypedArray(id: Integer): JTypedArray; cdecl;
  function openRawResource(id: Integer): JInputStream; cdecl; overload;
  function openRawResource(id: Integer; value: JTypedValue): JInputStream; cdecl; overload;
  function openRawResourceFd(id: Integer): JAssetFileDescriptor; cdecl;
  procedure parseBundleExtra(tagName: JString; attrs: JAttributeSet; outBundle: JBundle); cdecl;
  procedure parseBundleExtras(parser: JXmlResourceParser; outBundle: JBundle); cdecl;
  procedure updateConfiguration(config: JConfiguration; metrics: JDisplayMetrics); cdecl;
end;
TJResources = class(TJavaGenericImport<JResourcesClass, JResources>) end;

JShader_TileModeClass = interface(JEnumClass)
['{79478EFD-F336-4317-9E61-102DBBA54973}']
  {Property Methods}
  function _GetCLAMP: JShader_TileMode;
  function _GetMIRROR: JShader_TileMode;
  function _GetREPEAT: JShader_TileMode;
  {Methods}
  function valueOf(name: JString): JShader_TileMode; cdecl;
  function values: TJavaObjectArray<JShader_TileMode>; cdecl;
  {Properties}
  property CLAMP: JShader_TileMode read _GetCLAMP;
  property MIRROR: JShader_TileMode read _GetMIRROR;
  property &REPEAT: JShader_TileMode read _GetREPEAT;
end;

[JavaSignature('android/graphics/Shader$TileMode')]
JShader_TileMode = interface(JEnum)
['{A98BD682-AE2D-4FFB-8971-C1239E5779BC}']
end;
TJShader_TileMode = class(TJavaGenericImport<JShader_TileModeClass, JShader_TileMode>) end;

JRasterizerClass = interface(JObjectClass)
['{5F2AA4C2-9E97-42BF-AE2B-72D004DA4D36}']
  {Methods}
  function init: JRasterizer; cdecl;
end;

[JavaSignature('android/graphics/Rasterizer')]
JRasterizer = interface(JObject)
['{C1E10C30-32E4-422D-AFB1-6F0A6BFCB38E}']
end;
TJRasterizer = class(TJavaGenericImport<JRasterizerClass, JRasterizer>) end;

JLayerRasterizerClass = interface(JRasterizerClass)
['{474E4C95-BB7B-49B1-AE43-35AB2796E8A4}']
  {Methods}
  function init: JLayerRasterizer; cdecl;
end;

[JavaSignature('android/graphics/LayerRasterizer')]
JLayerRasterizer = interface(JRasterizer)
['{9C8F2CCB-17E7-4588-BFCE-3A3F78B7AC87}']
  {Methods}
  procedure addLayer(paint: JPaint; dx: Single; dy: Single); cdecl; overload;
  procedure addLayer(paint: JPaint); cdecl; overload;
end;
TJLayerRasterizer = class(TJavaGenericImport<JLayerRasterizerClass, JLayerRasterizer>) end;

JOvalShapeClass = interface(JRectShapeClass)
['{4260F618-8D5F-4E41-B386-E8FA3F22C4EB}']
  {Methods}
  function init: JOvalShape; cdecl;
end;

[JavaSignature('android/graphics/drawable/shapes/OvalShape')]
JOvalShape = interface(JRectShape)
['{B80FDC7B-88DF-4C06-90A6-9E8A5F1EB4B3}']
  {Methods}
  procedure draw(canvas: JCanvas; paint: JPaint); cdecl;
end;
TJOvalShape = class(TJavaGenericImport<JOvalShapeClass, JOvalShape>) end;

JScaleGestureDetector_OnScaleGestureListenerClass = interface(IJavaClass)
['{6FE82996-5BDB-4CF3-AB1E-E42DA3FDFD2F}']
end;

[JavaSignature('android/view/ScaleGestureDetector$OnScaleGestureListener')]
JScaleGestureDetector_OnScaleGestureListener = interface(IJavaInstance)
['{17BA86A8-F7BA-45D9-BEFE-67F92E6F034A}']
  {Methods}
  function onScale(detector: JScaleGestureDetector): Boolean; cdecl;
  function onScaleBegin(detector: JScaleGestureDetector): Boolean; cdecl;
  procedure onScaleEnd(detector: JScaleGestureDetector); cdecl;
end;
TJScaleGestureDetector_OnScaleGestureListener = class(TJavaGenericImport<JScaleGestureDetector_OnScaleGestureListenerClass, JScaleGestureDetector_OnScaleGestureListener>) end;

JDragEventClass = interface(JObjectClass)
['{942CCCAD-2F23-4A86-A7DB-C63D200CEB0F}']
  {Property Methods}
  function _GetACTION_DRAG_ENDED: Integer;
  function _GetACTION_DRAG_ENTERED: Integer;
  function _GetACTION_DRAG_EXITED: Integer;
  function _GetACTION_DRAG_LOCATION: Integer;
  function _GetACTION_DRAG_STARTED: Integer;
  function _GetACTION_DROP: Integer;
  function _GetCREATOR: JParcelable_Creator;
  {Properties}
  property ACTION_DRAG_ENDED: Integer read _GetACTION_DRAG_ENDED;
  property ACTION_DRAG_ENTERED: Integer read _GetACTION_DRAG_ENTERED;
  property ACTION_DRAG_EXITED: Integer read _GetACTION_DRAG_EXITED;
  property ACTION_DRAG_LOCATION: Integer read _GetACTION_DRAG_LOCATION;
  property ACTION_DRAG_STARTED: Integer read _GetACTION_DRAG_STARTED;
  property ACTION_DROP: Integer read _GetACTION_DROP;
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/view/DragEvent')]
JDragEvent = interface(JObject)
['{058D88CB-DB10-477F-BE28-36295CA6507A}']
  {Methods}
  function describeContents: Integer; cdecl;
  function getAction: Integer; cdecl;
  function getClipData: JClipData; cdecl;
  function getClipDescription: JClipDescription; cdecl;
  function getLocalState: JObject; cdecl;
  function getResult: Boolean; cdecl;
  function getX: Single; cdecl;
  function getY: Single; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
end;
TJDragEvent = class(TJavaGenericImport<JDragEventClass, JDragEvent>) end;

JInputMethodSubtypeClass = interface(JObjectClass)
['{A4198834-6CE2-445E-9B56-9A46DA9B8FD1}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Methods}
  function init(nameId: Integer; iconId: Integer; locale: JString; mode: JString; extraValue: JString; isAuxiliary: Boolean; overridesImplicitlyEnabledSubtype: Boolean): JInputMethodSubtype; cdecl; overload;
  function init(nameId: Integer; iconId: Integer; locale: JString; mode: JString; extraValue: JString; isAuxiliary: Boolean; overridesImplicitlyEnabledSubtype: Boolean; id: Integer): JInputMethodSubtype; cdecl; overload;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/view/inputmethod/InputMethodSubtype')]
JInputMethodSubtype = interface(JObject)
['{0BB46363-838A-4B33-81A3-9E7A33F90382}']
  {Methods}
  function containsExtraValueKey(key: JString): Boolean; cdecl;
  function describeContents: Integer; cdecl;
  function equals(o: JObject): Boolean; cdecl;
  function getDisplayName(context: JContext; packageName: JString; appInfo: JApplicationInfo): JCharSequence; cdecl;
  function getExtraValue: JString; cdecl;
  function getExtraValueOf(key: JString): JString; cdecl;
  function getIconResId: Integer; cdecl;
  function getLocale: JString; cdecl;
  function getMode: JString; cdecl;
  function getNameResId: Integer; cdecl;
  function hashCode: Integer; cdecl;
  function isAuxiliary: Boolean; cdecl;
  function overridesImplicitlyEnabledSubtype: Boolean; cdecl;
  procedure writeToParcel(dest: JParcel; parcelableFlags: Integer); cdecl;
end;
TJInputMethodSubtype = class(TJavaGenericImport<JInputMethodSubtypeClass, JInputMethodSubtype>) end;

JAssetManager_AssetInputStreamClass = interface(JInputStreamClass)
['{55715557-E5A5-4411-BECA-FAA45DFDDDA3}']
end;

[JavaSignature('android/content/res/AssetManager$AssetInputStream')]
JAssetManager_AssetInputStream = interface(JInputStream)
['{6AE63092-7E80-4139-B59B-F4EBE163F1A6}']
  {Methods}
  function available: Integer; cdecl;
  procedure close; cdecl;
  function getAssetInt: Integer; cdecl;
  procedure mark(readlimit: Integer); cdecl;
  function markSupported: Boolean; cdecl;
  function read: Integer; cdecl; overload;
  function read(b: TJavaArray<Byte>): Integer; cdecl; overload;
  function read(b: TJavaArray<Byte>; off: Integer; len: Integer): Integer; cdecl; overload;
  procedure reset; cdecl;
  function skip(n: Int64): Int64; cdecl;
end;
TJAssetManager_AssetInputStream = class(TJavaGenericImport<JAssetManager_AssetInputStreamClass, JAssetManager_AssetInputStream>) end;

JPath_FillTypeClass = interface(JEnumClass)
['{73B4E1B8-E73B-4DBC-ADBF-BBF72F7FA7B3}']
  {Property Methods}
  function _GetEVEN_ODD: JPath_FillType;
  function _GetINVERSE_EVEN_ODD: JPath_FillType;
  function _GetINVERSE_WINDING: JPath_FillType;
  function _GetWINDING: JPath_FillType;
  {Methods}
  function valueOf(name: JString): JPath_FillType; cdecl;
  function values: TJavaObjectArray<JPath_FillType>; cdecl;
  {Properties}
  property EVEN_ODD: JPath_FillType read _GetEVEN_ODD;
  property INVERSE_EVEN_ODD: JPath_FillType read _GetINVERSE_EVEN_ODD;
  property INVERSE_WINDING: JPath_FillType read _GetINVERSE_WINDING;
  property WINDING: JPath_FillType read _GetWINDING;
end;

[JavaSignature('android/graphics/Path$FillType')]
JPath_FillType = interface(JEnum)
['{ECD8A84E-F5C5-4E25-83C2-A5A1BB932D65}']
end;
TJPath_FillType = class(TJavaGenericImport<JPath_FillTypeClass, JPath_FillType>) end;

JLayerDrawableClass = interface(JDrawableClass)
['{83EA2827-F626-4F02-94BA-A44DDB32A062}']
  {Methods}
  function init(layers: TJavaObjectArray<JDrawable>): JLayerDrawable; cdecl;
end;

[JavaSignature('android/graphics/drawable/LayerDrawable')]
JLayerDrawable = interface(JDrawable)
['{9CF5C6FE-7799-4439-853C-E39B0A9CE04A}']
  {Methods}
  procedure draw(canvas: JCanvas); cdecl;
  function findDrawableByLayerId(id: Integer): JDrawable; cdecl;
  function getChangingConfigurations: Integer; cdecl;
  function getConstantState: JDrawable_ConstantState; cdecl;
  function getDrawable(index: Integer): JDrawable; cdecl;
  function getId(index: Integer): Integer; cdecl;
  function getIntrinsicHeight: Integer; cdecl;
  function getIntrinsicWidth: Integer; cdecl;
  function getNumberOfLayers: Integer; cdecl;
  function getOpacity: Integer; cdecl;
  function getPadding(padding: JRect): Boolean; cdecl;
  procedure invalidateDrawable(who: JDrawable); cdecl;
  function isStateful: Boolean; cdecl;
  function mutate: JDrawable; cdecl;
  procedure scheduleDrawable(who: JDrawable; what: JRunnable; when: Int64); cdecl;
  procedure setAlpha(alpha: Integer); cdecl;
  procedure setColorFilter(cf: JColorFilter); cdecl;
  procedure setDither(dither: Boolean); cdecl;
  function setDrawableByLayerId(id: Integer; drawable: JDrawable): Boolean; cdecl;
  procedure setId(index: Integer; id: Integer); cdecl;
  procedure setLayerInset(index: Integer; l: Integer; t: Integer; r: Integer; b: Integer); cdecl;
  procedure setOpacity(opacity: Integer); cdecl;
  function setVisible(visible: Boolean; restart: Boolean): Boolean; cdecl;
  procedure unscheduleDrawable(who: JDrawable; what: JRunnable); cdecl;
end;
TJLayerDrawable = class(TJavaGenericImport<JLayerDrawableClass, JLayerDrawable>) end;

JPermissionInfoClass = interface(JPackageItemInfoClass)
['{E67E88B6-D393-402E-937D-D9EA9313CDCE}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  function _GetFLAG_COSTS_MONEY: Integer;
  function _GetPROTECTION_DANGEROUS: Integer;
  function _GetPROTECTION_FLAG_DEVELOPMENT: Integer;
  function _GetPROTECTION_FLAG_SYSTEM: Integer;
  function _GetPROTECTION_MASK_BASE: Integer;
  function _GetPROTECTION_MASK_FLAGS: Integer;
  function _GetPROTECTION_NORMAL: Integer;
  function _GetPROTECTION_SIGNATURE: Integer;
  function _GetPROTECTION_SIGNATURE_OR_SYSTEM: Integer;
  {Methods}
  function init: JPermissionInfo; cdecl; overload;
  function init(orig: JPermissionInfo): JPermissionInfo; cdecl; overload;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
  property FLAG_COSTS_MONEY: Integer read _GetFLAG_COSTS_MONEY;
  property PROTECTION_DANGEROUS: Integer read _GetPROTECTION_DANGEROUS;
  property PROTECTION_FLAG_DEVELOPMENT: Integer read _GetPROTECTION_FLAG_DEVELOPMENT;
  property PROTECTION_FLAG_SYSTEM: Integer read _GetPROTECTION_FLAG_SYSTEM;
  property PROTECTION_MASK_BASE: Integer read _GetPROTECTION_MASK_BASE;
  property PROTECTION_MASK_FLAGS: Integer read _GetPROTECTION_MASK_FLAGS;
  property PROTECTION_NORMAL: Integer read _GetPROTECTION_NORMAL;
  property PROTECTION_SIGNATURE: Integer read _GetPROTECTION_SIGNATURE;
  property PROTECTION_SIGNATURE_OR_SYSTEM: Integer read _GetPROTECTION_SIGNATURE_OR_SYSTEM;
end;

[JavaSignature('android/content/pm/PermissionInfo')]
JPermissionInfo = interface(JPackageItemInfo)
['{6023977F-D9A2-4B92-B205-93C747E1D13B}']
  {Property Methods}
  function _GetdescriptionRes: Integer;
  procedure _SetdescriptionRes(Value: Integer);
  function _Getflags: Integer;
  procedure _Setflags(Value: Integer);
  function _Getgroup: JString;
  procedure _Setgroup(Value: JString);
  function _GetnonLocalizedDescription: JCharSequence;
  procedure _SetnonLocalizedDescription(Value: JCharSequence);
  function _GetprotectionLevel: Integer;
  procedure _SetprotectionLevel(Value: Integer);
  {Methods}
  function describeContents: Integer; cdecl;
  function loadDescription(pm: JPackageManager): JCharSequence; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; parcelableFlags: Integer); cdecl;
  {Properties}
  property descriptionRes: Integer read _GetdescriptionRes write _SetdescriptionRes;
  property flags: Integer read _Getflags write _Setflags;
  property group: JString read _Getgroup write _Setgroup;
  property nonLocalizedDescription: JCharSequence read _GetnonLocalizedDescription write _SetnonLocalizedDescription;
  property protectionLevel: Integer read _GetprotectionLevel write _SetprotectionLevel;
end;
TJPermissionInfo = class(TJavaGenericImport<JPermissionInfoClass, JPermissionInfo>) end;

JResolveInfoClass = interface(JObjectClass)
['{4C04EF6B-B5BD-486D-90C1-F7F8AD2902DC}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Methods}
  function init: JResolveInfo; cdecl; overload;
  function init(orig: JResolveInfo): JResolveInfo; cdecl; overload;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/content/pm/ResolveInfo')]
JResolveInfo = interface(JObject)
['{467E8989-4CE6-43F0-AB23-FE4FC787F872}']
  {Property Methods}
  function _GetactivityInfo: JActivityInfo;
  procedure _SetactivityInfo(Value: JActivityInfo);
  function _Getfilter: JIntentFilter;
  procedure _Setfilter(Value: JIntentFilter);
  function _Geticon: Integer;
  procedure _Seticon(Value: Integer);
  function _GetisDefault: Boolean;
  procedure _SetisDefault(Value: Boolean);
  function _GetlabelRes: Integer;
  procedure _SetlabelRes(Value: Integer);
  function _Getmatch: Integer;
  procedure _Setmatch(Value: Integer);
  function _GetnonLocalizedLabel: JCharSequence;
  procedure _SetnonLocalizedLabel(Value: JCharSequence);
  function _GetpreferredOrder: Integer;
  procedure _SetpreferredOrder(Value: Integer);
  function _Getpriority: Integer;
  procedure _Setpriority(Value: Integer);
  function _GetresolvePackageName: JString;
  procedure _SetresolvePackageName(Value: JString);
  function _GetserviceInfo: JServiceInfo;
  procedure _SetserviceInfo(Value: JServiceInfo);
  function _GetspecificIndex: Integer;
  procedure _SetspecificIndex(Value: Integer);
  {Methods}
  function describeContents: Integer; cdecl;
  function getIconResource: Integer; cdecl;
  function loadIcon(pm: JPackageManager): JDrawable; cdecl;
  function loadLabel(pm: JPackageManager): JCharSequence; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; parcelableFlags: Integer); cdecl;
  {Properties}
  property activityInfo: JActivityInfo read _GetactivityInfo write _SetactivityInfo;
  property filter: JIntentFilter read _Getfilter write _Setfilter;
  property icon: Integer read _Geticon write _Seticon;
  property isDefault: Boolean read _GetisDefault write _SetisDefault;
  property labelRes: Integer read _GetlabelRes write _SetlabelRes;
  property match: Integer read _Getmatch write _Setmatch;
  property nonLocalizedLabel: JCharSequence read _GetnonLocalizedLabel write _SetnonLocalizedLabel;
  property preferredOrder: Integer read _GetpreferredOrder write _SetpreferredOrder;
  property priority: Integer read _Getpriority write _Setpriority;
  property resolvePackageName: JString read _GetresolvePackageName write _SetresolvePackageName;
  property serviceInfo: JServiceInfo read _GetserviceInfo write _SetserviceInfo;
  property specificIndex: Integer read _GetspecificIndex write _SetspecificIndex;
end;
TJResolveInfo = class(TJavaGenericImport<JResolveInfoClass, JResolveInfo>) end;

JSharedPreferencesClass = interface(IJavaClass)
['{C6937724-11B9-468B-9898-5D0C17D64535}']
end;

[JavaSignature('android/content/SharedPreferences')]
JSharedPreferences = interface(IJavaInstance)
['{E44179D1-B961-4316-A8B0-45B52A482FA7}']
  {Methods}
  function &contains(key: JString): Boolean; cdecl;
  function edit: JSharedPreferences_Editor; cdecl;
  function getAll: JMap; cdecl;
  function getBoolean(key: JString; defValue: Boolean): Boolean; cdecl;
  function getFloat(key: JString; defValue: Single): Single; cdecl;
  function getInt(key: JString; defValue: Integer): Integer; cdecl;
  function getLong(key: JString; defValue: Int64): Int64; cdecl;
  function getString(key: JString; defValue: JString): JString; cdecl;
  function getStringSet(key: JString; defValues: JSet): JSet; cdecl;
  procedure registerOnSharedPreferenceChangeListener(listener: JSharedPreferences_OnSharedPreferenceChangeListener); cdecl;
  procedure unregisterOnSharedPreferenceChangeListener(listener: JSharedPreferences_OnSharedPreferenceChangeListener); cdecl;
end;
TJSharedPreferences = class(TJavaGenericImport<JSharedPreferencesClass, JSharedPreferences>) end;

JSurfaceTextureClass = interface(JObjectClass)
['{E65AB583-F23C-4A17-8246-3B30FE16F300}']
  {Methods}
  function init(texName: Integer): JSurfaceTexture; cdecl;
end;

[JavaSignature('android/graphics/SurfaceTexture')]
JSurfaceTexture = interface(JObject)
['{270948EE-C26E-49F4-835E-BB7A23613999}']
  {Methods}
  procedure attachToGLContext(texName: Integer); cdecl;
  procedure detachFromGLContext; cdecl;
  function getTimestamp: Int64; cdecl;
  procedure getTransformMatrix(mtx: TJavaArray<Single>); cdecl;
  procedure release; cdecl;
  procedure setDefaultBufferSize(width: Integer; height: Integer); cdecl;
  procedure setOnFrameAvailableListener(l: JSurfaceTexture_OnFrameAvailableListener); cdecl;
  procedure updateTexImage; cdecl;
end;
TJSurfaceTexture = class(TJavaGenericImport<JSurfaceTextureClass, JSurfaceTexture>) end;

JPathDashPathEffect_StyleClass = interface(JEnumClass)
['{F8000283-837E-4D79-AC7B-84BF67F48E7F}']
  {Property Methods}
  function _GetMORPH: JPathDashPathEffect_Style;
  function _GetROTATE: JPathDashPathEffect_Style;
  function _GetTRANSLATE: JPathDashPathEffect_Style;
  {Methods}
  function valueOf(name: JString): JPathDashPathEffect_Style; cdecl;
  function values: TJavaObjectArray<JPathDashPathEffect_Style>; cdecl;
  {Properties}
  property MORPH: JPathDashPathEffect_Style read _GetMORPH;
  property ROTATE: JPathDashPathEffect_Style read _GetROTATE;
  property TRANSLATE: JPathDashPathEffect_Style read _GetTRANSLATE;
end;

[JavaSignature('android/graphics/PathDashPathEffect$Style')]
JPathDashPathEffect_Style = interface(JEnum)
['{60D9C29F-F11E-4325-9A20-F2403693B759}']
end;
TJPathDashPathEffect_Style = class(TJavaGenericImport<JPathDashPathEffect_StyleClass, JPathDashPathEffect_Style>) end;

JClipData_ItemClass = interface(JObjectClass)
['{FC99163D-9B5D-4F25-BBE0-7C05AE05D5E2}']
  {Methods}
  function init(text: JCharSequence): JClipData_Item; cdecl; overload;
  function init(text: JCharSequence; htmlText: JString): JClipData_Item; cdecl; overload;
  function init(intent: JIntent): JClipData_Item; cdecl; overload;
  function init(uri: Jnet_Uri): JClipData_Item; cdecl; overload;
  function init(text: JCharSequence; intent: JIntent; uri: Jnet_Uri): JClipData_Item; cdecl; overload;
  function init(text: JCharSequence; htmlText: JString; intent: JIntent; uri: Jnet_Uri): JClipData_Item; cdecl; overload;
end;

[JavaSignature('android/content/ClipData$Item')]
JClipData_Item = interface(JObject)
['{CC198ECA-644C-456A-8065-E00F54D3EDE1}']
  {Methods}
  function coerceToHtmlText(context: JContext): JString; cdecl;
  function coerceToStyledText(context: JContext): JCharSequence; cdecl;
  function coerceToText(context: JContext): JCharSequence; cdecl;
  function getHtmlText: JString; cdecl;
  function getIntent: JIntent; cdecl;
  function getText: JCharSequence; cdecl;
  function getUri: Jnet_Uri; cdecl;
  function toString: JString; cdecl;
end;
TJClipData_Item = class(TJavaGenericImport<JClipData_ItemClass, JClipData_Item>) end;

JResources_ThemeClass = interface(JObjectClass)
['{967E4A62-EA10-4238-BD59-44F1E821179C}']
end;

[JavaSignature('android/content/res/Resources$Theme')]
JResources_Theme = interface(JObject)
['{74665859-1F41-447D-A92E-523CAAEA741D}']
  {Methods}
  procedure applyStyle(resid: Integer; force: Boolean); cdecl;
  procedure dump(priority: Integer; tag: JString; prefix: JString); cdecl;
  function obtainStyledAttributes(attrs: TJavaArray<Integer>): JTypedArray; cdecl; overload;
  function obtainStyledAttributes(resid: Integer; attrs: TJavaArray<Integer>): JTypedArray; cdecl; overload;
  function obtainStyledAttributes(set_: JAttributeSet; attrs: TJavaArray<Integer>; defStyleAttr: Integer; defStyleRes: Integer): JTypedArray; cdecl; overload;
  function resolveAttribute(resid: Integer; outValue: JTypedValue; resolveRefs: Boolean): Boolean; cdecl;
  procedure setTo(other: JResources_Theme); cdecl;
end;
TJResources_Theme = class(TJavaGenericImport<JResources_ThemeClass, JResources_Theme>) end;

JViewTreeObserver_OnTouchModeChangeListenerClass = interface(IJavaClass)
['{343F033E-028A-4613-AF32-92B5F78C7D0A}']
end;

[JavaSignature('android/view/ViewTreeObserver$OnTouchModeChangeListener')]
JViewTreeObserver_OnTouchModeChangeListener = interface(IJavaInstance)
['{6C910F27-8837-49F4-86F4-7AFC7AA5BED1}']
  {Methods}
  procedure onTouchModeChanged(isInTouchMode: Boolean); cdecl;
end;
TJViewTreeObserver_OnTouchModeChangeListener = class(TJavaGenericImport<JViewTreeObserver_OnTouchModeChangeListenerClass, JViewTreeObserver_OnTouchModeChangeListener>) end;

JReceiverCallNotAllowedExceptionClass = interface(JAndroidRuntimeExceptionClass)
['{87130490-4CBA-4118-94BB-8357391F1B65}']
  {Methods}
  function init(msg: JString): JReceiverCallNotAllowedException; cdecl;
end;

[JavaSignature('android/content/ReceiverCallNotAllowedException')]
JReceiverCallNotAllowedException = interface(JAndroidRuntimeException)
['{28E59F90-1B49-4ADA-86A7-5B189CB4410B}']
end;
TJReceiverCallNotAllowedException = class(TJavaGenericImport<JReceiverCallNotAllowedExceptionClass, JReceiverCallNotAllowedException>) end;

JGestureDetector_OnGestureListenerClass = interface(IJavaClass)
['{F2B6E4EF-66AB-4548-AB15-4253FDC78CD1}']
end;

[JavaSignature('android/view/GestureDetector$OnGestureListener')]
JGestureDetector_OnGestureListener = interface(IJavaInstance)
['{A72C80A3-7E4C-4FDD-98FC-99B328D6BFDD}']
  {Methods}
  function onDown(e: JMotionEvent): Boolean; cdecl;
  function onFling(e1: JMotionEvent; e2: JMotionEvent; velocityX: Single; velocityY: Single): Boolean; cdecl;
  procedure onLongPress(e: JMotionEvent); cdecl;
  function onScroll(e1: JMotionEvent; e2: JMotionEvent; distanceX: Single; distanceY: Single): Boolean; cdecl;
  procedure onShowPress(e: JMotionEvent); cdecl;
  function onSingleTapUp(e: JMotionEvent): Boolean; cdecl;
end;
TJGestureDetector_OnGestureListener = class(TJavaGenericImport<JGestureDetector_OnGestureListenerClass, JGestureDetector_OnGestureListener>) end;

JBitmapClass = interface(JObjectClass)
['{297AF779-3A6F-4030-A8FB-1E91DEF2C89F}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  function _GetDENSITY_NONE: Integer;
  {Methods}
  function createBitmap(src: JBitmap): JBitmap; cdecl; overload;
  function createBitmap(source: JBitmap; x: Integer; y: Integer; width: Integer; height: Integer): JBitmap; cdecl; overload;
  function createBitmap(source: JBitmap; x: Integer; y: Integer; width: Integer; height: Integer; m: JMatrix; filter: Boolean): JBitmap; cdecl; overload;
  function createBitmap(width: Integer; height: Integer; config: JBitmap_Config): JBitmap; cdecl; overload;
  function createBitmap(display: JDisplayMetrics; width: Integer; height: Integer; config: JBitmap_Config): JBitmap; cdecl; overload;
  function createBitmap(colors: TJavaArray<Integer>; offset: Integer; stride: Integer; width: Integer; height: Integer; config: JBitmap_Config): JBitmap; cdecl; overload;
  function createBitmap(display: JDisplayMetrics; colors: TJavaArray<Integer>; offset: Integer; stride: Integer; width: Integer; height: Integer; config: JBitmap_Config): JBitmap; cdecl; overload;
  function createBitmap(colors: TJavaArray<Integer>; width: Integer; height: Integer; config: JBitmap_Config): JBitmap; cdecl; overload;
  function createBitmap(display: JDisplayMetrics; colors: TJavaArray<Integer>; width: Integer; height: Integer; config: JBitmap_Config): JBitmap; cdecl; overload;
  function createScaledBitmap(src: JBitmap; dstWidth: Integer; dstHeight: Integer; filter: Boolean): JBitmap; cdecl;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
  property DENSITY_NONE: Integer read _GetDENSITY_NONE;
end;

[JavaSignature('android/graphics/Bitmap')]
JBitmap = interface(JObject)
['{D248EC72-F7F9-4F71-BBDC-0940D8BAD29E}']
  {Methods}
  function compress(format: JBitmap_CompressFormat; quality: Integer; stream: JOutputStream): Boolean; cdecl;
  function copy(config: JBitmap_Config; isMutable: Boolean): JBitmap; cdecl;
  function describeContents: Integer; cdecl;
  procedure eraseColor(c: Integer); cdecl;
  function extractAlpha: JBitmap; cdecl; overload;
  function extractAlpha(paint: JPaint; offsetXY: TJavaArray<Integer>): JBitmap; cdecl; overload;
  function getByteCount: Integer; cdecl;
  function getConfig: JBitmap_Config; cdecl;
  function getDensity: Integer; cdecl;
  function getGenerationId: Integer; cdecl;
  function getHeight: Integer; cdecl;
  function getNinePatchChunk: TJavaArray<Byte>; cdecl;
  function getPixel(x: Integer; y: Integer): Integer; cdecl;
  procedure getPixels(pixels: TJavaArray<Integer>; offset: Integer; stride: Integer; x: Integer; y: Integer; width: Integer; height: Integer); cdecl;
  function getRowBytes: Integer; cdecl;
  function getScaledHeight(canvas: JCanvas): Integer; cdecl; overload;
  function getScaledHeight(metrics: JDisplayMetrics): Integer; cdecl; overload;
  function getScaledHeight(targetDensity: Integer): Integer; cdecl; overload;
  function getScaledWidth(canvas: JCanvas): Integer; cdecl; overload;
  function getScaledWidth(metrics: JDisplayMetrics): Integer; cdecl; overload;
  function getScaledWidth(targetDensity: Integer): Integer; cdecl; overload;
  function getWidth: Integer; cdecl;
  function hasAlpha: Boolean; cdecl;
  function hasMipMap: Boolean; cdecl;
  function isMutable: Boolean; cdecl;
  function isPremultiplied: Boolean; cdecl;
  function isRecycled: Boolean; cdecl;
  procedure prepareToDraw; cdecl;
  procedure recycle; cdecl;
  function sameAs(other: JBitmap): Boolean; cdecl;
  procedure setDensity(density: Integer); cdecl;
  procedure setHasAlpha(hasAlpha: Boolean); cdecl;
  procedure setHasMipMap(hasMipMap: Boolean); cdecl;
  procedure setPixel(x: Integer; y: Integer; color: Integer); cdecl;
  procedure setPixels(pixels: TJavaArray<Integer>; offset: Integer; stride: Integer; x: Integer; y: Integer; width: Integer; height: Integer); cdecl;
  procedure writeToParcel(p: JParcel; flags: Integer); cdecl;
end;
TJBitmap = class(TJavaGenericImport<JBitmapClass, JBitmap>) end;

JInputDevice_MotionRangeClass = interface(JObjectClass)
['{8BDD8CA8-236A-4693-9E0A-B4F81831B6EB}']
end;

[JavaSignature('android/view/InputDevice$MotionRange')]
JInputDevice_MotionRange = interface(JObject)
['{9F1D1FF9-EAF0-4877-BB6E-9D91EB287CF9}']
  {Methods}
  function getAxis: Integer; cdecl;
  function getFlat: Single; cdecl;
  function getFuzz: Single; cdecl;
  function getMax: Single; cdecl;
  function getMin: Single; cdecl;
  function getRange: Single; cdecl;
  function getSource: Integer; cdecl;
end;
TJInputDevice_MotionRange = class(TJavaGenericImport<JInputDevice_MotionRangeClass, JInputDevice_MotionRange>) end;

JAbstractThreadedSyncAdapterClass = interface(JObjectClass)
['{84F624CB-6DD0-43C4-B565-646C373BA09D}']
  {Property Methods}
  function _GetLOG_SYNC_DETAILS: Integer;
  {Methods}
  function init(context: JContext; autoInitialize: Boolean): JAbstractThreadedSyncAdapter; cdecl; overload;
  function init(context: JContext; autoInitialize: Boolean; allowParallelSyncs: Boolean): JAbstractThreadedSyncAdapter; cdecl; overload;
  {Properties}
  property LOG_SYNC_DETAILS: Integer read _GetLOG_SYNC_DETAILS;
end;

[JavaSignature('android/content/AbstractThreadedSyncAdapter')]
JAbstractThreadedSyncAdapter = interface(JObject)
['{15B35E75-7646-4F1A-B0FD-8F5CCD9D52CE}']
  {Methods}
  function getContext: JContext; cdecl;
  function getSyncAdapterBinder: JIBinder; cdecl;
  procedure onSyncCanceled; cdecl; overload;
  procedure onSyncCanceled(thread: JThread); cdecl; overload;
end;
TJAbstractThreadedSyncAdapter = class(TJavaGenericImport<JAbstractThreadedSyncAdapterClass, JAbstractThreadedSyncAdapter>) end;

JInputTypeClass = interface(IJavaClass)
['{2704D6AD-D90D-4CA7-838F-AD766525083E}']
  {Property Methods}
  function _GetTYPE_CLASS_DATETIME: Integer;
  function _GetTYPE_CLASS_NUMBER: Integer;
  function _GetTYPE_CLASS_PHONE: Integer;
  function _GetTYPE_CLASS_TEXT: Integer;
  function _GetTYPE_DATETIME_VARIATION_DATE: Integer;
  function _GetTYPE_DATETIME_VARIATION_NORMAL: Integer;
  function _GetTYPE_DATETIME_VARIATION_TIME: Integer;
  function _GetTYPE_MASK_CLASS: Integer;
  function _GetTYPE_MASK_FLAGS: Integer;
  function _GetTYPE_MASK_VARIATION: Integer;
  function _GetTYPE_NULL: Integer;
  function _GetTYPE_NUMBER_FLAG_DECIMAL: Integer;
  function _GetTYPE_NUMBER_FLAG_SIGNED: Integer;
  function _GetTYPE_NUMBER_VARIATION_NORMAL: Integer;
  function _GetTYPE_NUMBER_VARIATION_PASSWORD: Integer;
  function _GetTYPE_TEXT_FLAG_AUTO_COMPLETE: Integer;
  function _GetTYPE_TEXT_FLAG_AUTO_CORRECT: Integer;
  function _GetTYPE_TEXT_FLAG_CAP_CHARACTERS: Integer;
  function _GetTYPE_TEXT_FLAG_CAP_SENTENCES: Integer;
  function _GetTYPE_TEXT_FLAG_CAP_WORDS: Integer;
  function _GetTYPE_TEXT_FLAG_IME_MULTI_LINE: Integer;
  function _GetTYPE_TEXT_FLAG_MULTI_LINE: Integer;
  function _GetTYPE_TEXT_FLAG_NO_SUGGESTIONS: Integer;
  function _GetTYPE_TEXT_VARIATION_EMAIL_ADDRESS: Integer;
  function _GetTYPE_TEXT_VARIATION_EMAIL_SUBJECT: Integer;
  function _GetTYPE_TEXT_VARIATION_FILTER: Integer;
  function _GetTYPE_TEXT_VARIATION_LONG_MESSAGE: Integer;
  function _GetTYPE_TEXT_VARIATION_NORMAL: Integer;
  function _GetTYPE_TEXT_VARIATION_PASSWORD: Integer;
  function _GetTYPE_TEXT_VARIATION_PERSON_NAME: Integer;
  function _GetTYPE_TEXT_VARIATION_PHONETIC: Integer;
  function _GetTYPE_TEXT_VARIATION_POSTAL_ADDRESS: Integer;
  function _GetTYPE_TEXT_VARIATION_SHORT_MESSAGE: Integer;
  function _GetTYPE_TEXT_VARIATION_URI: Integer;
  function _GetTYPE_TEXT_VARIATION_VISIBLE_PASSWORD: Integer;
  function _GetTYPE_TEXT_VARIATION_WEB_EDIT_TEXT: Integer;
  function _GetTYPE_TEXT_VARIATION_WEB_EMAIL_ADDRESS: Integer;
  function _GetTYPE_TEXT_VARIATION_WEB_PASSWORD: Integer;
  {Properties}
  property TYPE_CLASS_DATETIME: Integer read _GetTYPE_CLASS_DATETIME;
  property TYPE_CLASS_NUMBER: Integer read _GetTYPE_CLASS_NUMBER;
  property TYPE_CLASS_PHONE: Integer read _GetTYPE_CLASS_PHONE;
  property TYPE_CLASS_TEXT: Integer read _GetTYPE_CLASS_TEXT;
  property TYPE_DATETIME_VARIATION_DATE: Integer read _GetTYPE_DATETIME_VARIATION_DATE;
  property TYPE_DATETIME_VARIATION_NORMAL: Integer read _GetTYPE_DATETIME_VARIATION_NORMAL;
  property TYPE_DATETIME_VARIATION_TIME: Integer read _GetTYPE_DATETIME_VARIATION_TIME;
  property TYPE_MASK_CLASS: Integer read _GetTYPE_MASK_CLASS;
  property TYPE_MASK_FLAGS: Integer read _GetTYPE_MASK_FLAGS;
  property TYPE_MASK_VARIATION: Integer read _GetTYPE_MASK_VARIATION;
  property TYPE_NULL: Integer read _GetTYPE_NULL;
  property TYPE_NUMBER_FLAG_DECIMAL: Integer read _GetTYPE_NUMBER_FLAG_DECIMAL;
  property TYPE_NUMBER_FLAG_SIGNED: Integer read _GetTYPE_NUMBER_FLAG_SIGNED;
  property TYPE_NUMBER_VARIATION_NORMAL: Integer read _GetTYPE_NUMBER_VARIATION_NORMAL;
  property TYPE_NUMBER_VARIATION_PASSWORD: Integer read _GetTYPE_NUMBER_VARIATION_PASSWORD;
  property TYPE_TEXT_FLAG_AUTO_COMPLETE: Integer read _GetTYPE_TEXT_FLAG_AUTO_COMPLETE;
  property TYPE_TEXT_FLAG_AUTO_CORRECT: Integer read _GetTYPE_TEXT_FLAG_AUTO_CORRECT;
  property TYPE_TEXT_FLAG_CAP_CHARACTERS: Integer read _GetTYPE_TEXT_FLAG_CAP_CHARACTERS;
  property TYPE_TEXT_FLAG_CAP_SENTENCES: Integer read _GetTYPE_TEXT_FLAG_CAP_SENTENCES;
  property TYPE_TEXT_FLAG_CAP_WORDS: Integer read _GetTYPE_TEXT_FLAG_CAP_WORDS;
  property TYPE_TEXT_FLAG_IME_MULTI_LINE: Integer read _GetTYPE_TEXT_FLAG_IME_MULTI_LINE;
  property TYPE_TEXT_FLAG_MULTI_LINE: Integer read _GetTYPE_TEXT_FLAG_MULTI_LINE;
  property TYPE_TEXT_FLAG_NO_SUGGESTIONS: Integer read _GetTYPE_TEXT_FLAG_NO_SUGGESTIONS;
  property TYPE_TEXT_VARIATION_EMAIL_ADDRESS: Integer read _GetTYPE_TEXT_VARIATION_EMAIL_ADDRESS;
  property TYPE_TEXT_VARIATION_EMAIL_SUBJECT: Integer read _GetTYPE_TEXT_VARIATION_EMAIL_SUBJECT;
  property TYPE_TEXT_VARIATION_FILTER: Integer read _GetTYPE_TEXT_VARIATION_FILTER;
  property TYPE_TEXT_VARIATION_LONG_MESSAGE: Integer read _GetTYPE_TEXT_VARIATION_LONG_MESSAGE;
  property TYPE_TEXT_VARIATION_NORMAL: Integer read _GetTYPE_TEXT_VARIATION_NORMAL;
  property TYPE_TEXT_VARIATION_PASSWORD: Integer read _GetTYPE_TEXT_VARIATION_PASSWORD;
  property TYPE_TEXT_VARIATION_PERSON_NAME: Integer read _GetTYPE_TEXT_VARIATION_PERSON_NAME;
  property TYPE_TEXT_VARIATION_PHONETIC: Integer read _GetTYPE_TEXT_VARIATION_PHONETIC;
  property TYPE_TEXT_VARIATION_POSTAL_ADDRESS: Integer read _GetTYPE_TEXT_VARIATION_POSTAL_ADDRESS;
  property TYPE_TEXT_VARIATION_SHORT_MESSAGE: Integer read _GetTYPE_TEXT_VARIATION_SHORT_MESSAGE;
  property TYPE_TEXT_VARIATION_URI: Integer read _GetTYPE_TEXT_VARIATION_URI;
  property TYPE_TEXT_VARIATION_VISIBLE_PASSWORD: Integer read _GetTYPE_TEXT_VARIATION_VISIBLE_PASSWORD;
  property TYPE_TEXT_VARIATION_WEB_EDIT_TEXT: Integer read _GetTYPE_TEXT_VARIATION_WEB_EDIT_TEXT;
  property TYPE_TEXT_VARIATION_WEB_EMAIL_ADDRESS: Integer read _GetTYPE_TEXT_VARIATION_WEB_EMAIL_ADDRESS;
  property TYPE_TEXT_VARIATION_WEB_PASSWORD: Integer read _GetTYPE_TEXT_VARIATION_WEB_PASSWORD;
end;

[JavaSignature('android/text/InputType')]
JInputType = interface(IJavaInstance)
['{6FC86E96-FCF0-4BDB-9815-162E3696871D}']
end;
TJInputType = class(TJavaGenericImport<JInputTypeClass, JInputType>) end;

JTextPaintClass = interface(JPaintClass)
['{A0A7AA51-6729-4C37-8256-B45690967C11}']
  {Methods}
  function init: JTextPaint; cdecl; overload;
  function init(flags: Integer): JTextPaint; cdecl; overload;
  function init(p: JPaint): JTextPaint; cdecl; overload;
end;

[JavaSignature('android/text/TextPaint')]
JTextPaint = interface(JPaint)
['{790B2C3F-B6A0-43D9-9CD7-6312133D19F4}']
  {Property Methods}
  function _GetbaselineShift: Integer;
  procedure _SetbaselineShift(Value: Integer);
  function _GetbgColor: Integer;
  procedure _SetbgColor(Value: Integer);
  function _Getdensity: Single;
  procedure _Setdensity(Value: Single);
  function _GetdrawableState: TJavaArray<Integer>;
  procedure _SetdrawableState(Value: TJavaArray<Integer>);
  function _GetlinkColor: Integer;
  procedure _SetlinkColor(Value: Integer);
  {Methods}
  procedure &set(tp: JTextPaint); cdecl;
  {Properties}
  property baselineShift: Integer read _GetbaselineShift write _SetbaselineShift;
  property bgColor: Integer read _GetbgColor write _SetbgColor;
  property density: Single read _Getdensity write _Setdensity;
  property drawableState: TJavaArray<Integer> read _GetdrawableState write _SetdrawableState;
  property linkColor: Integer read _GetlinkColor write _SetlinkColor;
end;
TJTextPaint = class(TJavaGenericImport<JTextPaintClass, JTextPaint>) end;

JEditable_FactoryClass = interface(JObjectClass)
['{638B498B-C0BF-4778-BA84-BA1C20F8C197}']
  {Methods}
  function init: JEditable_Factory; cdecl;
  function getInstance: JEditable_Factory; cdecl;
end;

[JavaSignature('android/text/Editable$Factory')]
JEditable_Factory = interface(JObject)
['{C5E77FBA-6429-4997-A8BB-56D758B07E3A}']
  {Methods}
  function newEditable(source: JCharSequence): JEditable; cdecl;
end;
TJEditable_Factory = class(TJavaGenericImport<JEditable_FactoryClass, JEditable_Factory>) end;

JContentObserverClass = interface(JObjectClass)
['{B34CFEA3-9391-42D9-9F24-7D8293038099}']
  {Methods}
  function init(handler: JHandler): JContentObserver; cdecl;
end;

[JavaSignature('android/database/ContentObserver')]
JContentObserver = interface(JObject)
['{7360FA96-2F67-476D-801E-6B99CEA3C281}']
  {Methods}
  function deliverSelfNotifications: Boolean; cdecl;
  procedure dispatchChange(selfChange: Boolean); cdecl; overload;//Deprecated
  procedure dispatchChange(selfChange: Boolean; uri: Jnet_Uri); cdecl; overload;
  procedure onChange(selfChange: Boolean); cdecl; overload;
  procedure onChange(selfChange: Boolean; uri: Jnet_Uri); cdecl; overload;
end;
TJContentObserver = class(TJavaGenericImport<JContentObserverClass, JContentObserver>) end;

JLoader_ForceLoadContentObserverClass = interface(JContentObserverClass)
['{81D7EA1D-6953-4C18-9426-81E3D1C49210}']
  {Methods}
  function init: JLoader_ForceLoadContentObserver; cdecl;
end;

[JavaSignature('android/content/Loader$ForceLoadContentObserver')]
JLoader_ForceLoadContentObserver = interface(JContentObserver)
['{5408F796-56EB-481C-97CB-06CBDF53F040}']
  {Methods}
  function deliverSelfNotifications: Boolean; cdecl;
  procedure onChange(selfChange: Boolean); cdecl;
end;
TJLoader_ForceLoadContentObserver = class(TJavaGenericImport<JLoader_ForceLoadContentObserverClass, JLoader_ForceLoadContentObserver>) end;

JDataSetObserverClass = interface(JObjectClass)
['{C7C6316A-CC0A-4A2A-A099-107A93295C97}']
  {Methods}
  function init: JDataSetObserver; cdecl;
end;

[JavaSignature('android/database/DataSetObserver')]
JDataSetObserver = interface(JObject)
['{B204ED4E-691F-440A-9484-9AFCE74EEF86}']
  {Methods}
  procedure onChanged; cdecl;
  procedure onInvalidated; cdecl;
end;
TJDataSetObserver = class(TJavaGenericImport<JDataSetObserverClass, JDataSetObserver>) end;

JTransitionDrawableClass = interface(JLayerDrawableClass)
['{72B55F6D-A42D-4ABE-9654-9A515758F752}']
  {Methods}
  function init(layers: TJavaObjectArray<JDrawable>): JTransitionDrawable; cdecl;
end;

[JavaSignature('android/graphics/drawable/TransitionDrawable')]
JTransitionDrawable = interface(JLayerDrawable)
['{CFEC20FD-AF1F-40CC-AC70-C2543FB5AF24}']
  {Methods}
  procedure draw(canvas: JCanvas); cdecl;
  function isCrossFadeEnabled: Boolean; cdecl;
  procedure resetTransition; cdecl;
  procedure reverseTransition(duration: Integer); cdecl;
  procedure setCrossFadeEnabled(enabled: Boolean); cdecl;
  procedure startTransition(durationMillis: Integer); cdecl;
end;
TJTransitionDrawable = class(TJavaGenericImport<JTransitionDrawableClass, JTransitionDrawable>) end;

JCanvas_VertexModeClass = interface(JEnumClass)
['{15C117F4-B2A0-4040-8817-4D58CC105900}']
  {Property Methods}
  function _GetTRIANGLES: JCanvas_VertexMode;
  function _GetTRIANGLE_FAN: JCanvas_VertexMode;
  function _GetTRIANGLE_STRIP: JCanvas_VertexMode;
  {Methods}
  function valueOf(name: JString): JCanvas_VertexMode; cdecl;
  function values: TJavaObjectArray<JCanvas_VertexMode>; cdecl;
  {Properties}
  property TRIANGLES: JCanvas_VertexMode read _GetTRIANGLES;
  property TRIANGLE_FAN: JCanvas_VertexMode read _GetTRIANGLE_FAN;
  property TRIANGLE_STRIP: JCanvas_VertexMode read _GetTRIANGLE_STRIP;
end;

[JavaSignature('android/graphics/Canvas$VertexMode')]
JCanvas_VertexMode = interface(JEnum)
['{F7B3C1F3-EBC6-4078-AD26-E0BF71251EC4}']
end;
TJCanvas_VertexMode = class(TJavaGenericImport<JCanvas_VertexModeClass, JCanvas_VertexMode>) end;

JComponentNameClass = interface(JObjectClass)
['{299BEF02-1270-416B-8F7C-657B0F7F8873}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Methods}
  function init(pkg: JString; cls: JString): JComponentName; cdecl; overload;
  function init(pkg: JContext; cls: JString): JComponentName; cdecl; overload;
  function init(pkg: JContext; cls: Jlang_Class): JComponentName; cdecl; overload;
  function init(in_: JParcel): JComponentName; cdecl; overload;
  function readFromParcel(in_: JParcel): JComponentName; cdecl;
  function unflattenFromString(str: JString): JComponentName; cdecl;
  procedure writeToParcel(c: JComponentName; out_: JParcel); cdecl; overload;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/content/ComponentName')]
JComponentName = interface(JObject)
['{D5059D73-4735-465B-958D-E552E5534EA3}']
  {Methods}
  function clone: JComponentName; cdecl;
  function compareTo(that: JComponentName): Integer; cdecl;
  function describeContents: Integer; cdecl;
  function equals(obj: JObject): Boolean; cdecl;
  function flattenToShortString: JString; cdecl;
  function flattenToString: JString; cdecl;
  function getClassName: JString; cdecl;
  function getPackageName: JString; cdecl;
  function getShortClassName: JString; cdecl;
  function hashCode: Integer; cdecl;
  function toShortString: JString; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(out_: JParcel; flags: Integer); cdecl; overload;
end;
TJComponentName = class(TJavaGenericImport<JComponentNameClass, JComponentName>) end;

JServiceConnectionClass = interface(IJavaClass)
['{04453C2D-CBBA-4879-A697-6C1C930DA1E4}']
end;

[JavaSignature('android/content/ServiceConnection')]
JServiceConnection = interface(IJavaInstance)
['{B1F97308-817B-4AC2-91CA-3C5FF72D4707}']
  {Methods}
  procedure onServiceConnected(name: JComponentName; service: JIBinder); cdecl;
  procedure onServiceDisconnected(name: JComponentName); cdecl;
end;
TJServiceConnection = class(TJavaGenericImport<JServiceConnectionClass, JServiceConnection>) end;

JTransformationMethodClass = interface(IJavaClass)
['{D5A74199-7E85-4AFB-B4AF-6DEC7C41766D}']
end;

[JavaSignature('android/text/method/TransformationMethod')]
JTransformationMethod = interface(IJavaInstance)
['{C442A623-FB0B-43A9-A8CD-AD423AEEE451}']
  {Methods}
  function getTransformation(source: JCharSequence; view: JView): JCharSequence; cdecl;
  procedure onFocusChanged(view: JView; sourceText: JCharSequence; focused: Boolean; direction: Integer; previouslyFocusedRect: JRect); cdecl;
end;
TJTransformationMethod = class(TJavaGenericImport<JTransformationMethodClass, JTransformationMethod>) end;

JPeriodicSyncClass = interface(JObjectClass)
['{712C0D8C-A539-4529-BCDD-57539C37EE37}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/content/PeriodicSync')]
JPeriodicSync = interface(JObject)
['{AF79411F-188F-4B28-A1EA-DD369D8C921C}']
  {Property Methods}
  function _Getauthority: JString;
  function _Getextras: JBundle;
  function _Getperiod: Int64;
  {Methods}
  function describeContents: Integer; cdecl;
  function equals(o: JObject): Boolean; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  {Properties}
  property authority: JString read _Getauthority;
  property extras: JBundle read _Getextras;
  property period: Int64 read _Getperiod;
end;
TJPeriodicSync = class(TJavaGenericImport<JPeriodicSyncClass, JPeriodicSync>) end;

JContentQueryMapClass = interface(Jutil_ObservableClass)
['{61DB2504-CA86-4425-B750-B1D54AF83693}']
  {Methods}
  function init(cursor: JCursor; columnNameOfKey: JString; keepUpdated: Boolean; handlerForUpdateNotifications: JHandler): JContentQueryMap; cdecl;
end;

[JavaSignature('android/content/ContentQueryMap')]
JContentQueryMap = interface(Jutil_Observable)
['{4DE3BCB2-9904-4CD9-B9E0-31D7B312AA4E}']
  {Methods}
  procedure close; cdecl;
  function getRows: JMap; cdecl;
  function getValues(rowName: JString): JContentValues; cdecl;
  procedure requery; cdecl;
  procedure setKeepUpdated(keepUpdated: Boolean); cdecl;
end;
TJContentQueryMap = class(TJavaGenericImport<JContentQueryMapClass, JContentQueryMap>) end;

JMotionEvent_PointerCoordsClass = interface(JObjectClass)
['{96AEAF72-A3A1-4A43-8580-AA88344C5B90}']
  {Methods}
  function init: JMotionEvent_PointerCoords; cdecl; overload;
  function init(other: JMotionEvent_PointerCoords): JMotionEvent_PointerCoords; cdecl; overload;
end;

[JavaSignature('android/view/MotionEvent$PointerCoords')]
JMotionEvent_PointerCoords = interface(JObject)
['{D776B0D6-547F-4086-B381-B1059548BCAC}']
  {Property Methods}
  function _Getorientation: Single;
  procedure _Setorientation(Value: Single);
  function _Getpressure: Single;
  procedure _Setpressure(Value: Single);
  function _Getsize: Single;
  procedure _Setsize(Value: Single);
  function _GettoolMajor: Single;
  procedure _SettoolMajor(Value: Single);
  function _GettoolMinor: Single;
  procedure _SettoolMinor(Value: Single);
  function _GettouchMajor: Single;
  procedure _SettouchMajor(Value: Single);
  function _GettouchMinor: Single;
  procedure _SettouchMinor(Value: Single);
  function _Getx: Single;
  procedure _Setx(Value: Single);
  function _Gety: Single;
  procedure _Sety(Value: Single);
  {Methods}
  procedure clear; cdecl;
  procedure copyFrom(other: JMotionEvent_PointerCoords); cdecl;
  function getAxisValue(axis: Integer): Single; cdecl;
  procedure setAxisValue(axis: Integer; value: Single); cdecl;
  {Properties}
  property orientation: Single read _Getorientation write _Setorientation;
  property pressure: Single read _Getpressure write _Setpressure;
  property size: Single read _Getsize write _Setsize;
  property toolMajor: Single read _GettoolMajor write _SettoolMajor;
  property toolMinor: Single read _GettoolMinor write _SettoolMinor;
  property touchMajor: Single read _GettouchMajor write _SettouchMajor;
  property touchMinor: Single read _GettouchMinor write _SettouchMinor;
  property x: Single read _Getx write _Setx;
  property y: Single read _Gety write _Sety;
end;
TJMotionEvent_PointerCoords = class(TJavaGenericImport<JMotionEvent_PointerCoordsClass, JMotionEvent_PointerCoords>) end;

JSurfaceHolderClass = interface(IJavaClass)
['{0FBCFD81-0142-402E-A67D-66F8BF2B1DF5}']
  {Property Methods}
  function _GetSURFACE_TYPE_GPU: Integer;
  function _GetSURFACE_TYPE_HARDWARE: Integer;
  function _GetSURFACE_TYPE_NORMAL: Integer;
  function _GetSURFACE_TYPE_PUSH_BUFFERS: Integer;
  {Properties}
  property SURFACE_TYPE_GPU: Integer read _GetSURFACE_TYPE_GPU;
  property SURFACE_TYPE_HARDWARE: Integer read _GetSURFACE_TYPE_HARDWARE;
  property SURFACE_TYPE_NORMAL: Integer read _GetSURFACE_TYPE_NORMAL;
  property SURFACE_TYPE_PUSH_BUFFERS: Integer read _GetSURFACE_TYPE_PUSH_BUFFERS;
end;

[JavaSignature('android/view/SurfaceHolder')]
JSurfaceHolder = interface(IJavaInstance)
['{EAA969D0-D517-49C8-8A09-17DB28B2EA91}']
  {Methods}
  procedure addCallback(callback: JSurfaceHolder_Callback); cdecl;
  function getSurface: JSurface; cdecl;
  function getSurfaceFrame: JRect; cdecl;
  function isCreating: Boolean; cdecl;
  function lockCanvas: JCanvas; cdecl; overload;
  function lockCanvas(dirty: JRect): JCanvas; cdecl; overload;
  procedure removeCallback(callback: JSurfaceHolder_Callback); cdecl;
  procedure setFixedSize(width: Integer; height: Integer); cdecl;
  procedure setFormat(format: Integer); cdecl;
  procedure setKeepScreenOn(screenOn: Boolean); cdecl;
  procedure setSizeFromLayout; cdecl;
  procedure setType(type_: Integer); cdecl;//Deprecated
  procedure unlockCanvasAndPost(canvas: JCanvas); cdecl;
end;
TJSurfaceHolder = class(TJavaGenericImport<JSurfaceHolderClass, JSurfaceHolder>) end;

JRadialGradientClass = interface(JShaderClass)
['{CB0B2E63-9E6F-41E5-88F9-1816ECAC7A71}']
  {Methods}
  function init(x: Single; y: Single; radius: Single; colors: TJavaArray<Integer>; positions: TJavaArray<Single>; tile: JShader_TileMode): JRadialGradient; cdecl; overload;
  function init(x: Single; y: Single; radius: Single; color0: Integer; color1: Integer; tile: JShader_TileMode): JRadialGradient; cdecl; overload;
end;

[JavaSignature('android/graphics/RadialGradient')]
JRadialGradient = interface(JShader)
['{17F8C6B7-F5BA-420C-8966-E92CD280D66A}']
end;
TJRadialGradient = class(TJavaGenericImport<JRadialGradientClass, JRadialGradient>) end;

JViewGroup_LayoutParamsClass = interface(JObjectClass)
['{D6F81CA0-279D-4335-8440-AFCDBF07CFC5}']
  {Property Methods}
  function _GetFILL_PARENT: Integer;
  function _GetMATCH_PARENT: Integer;
  function _GetWRAP_CONTENT: Integer;
  {Methods}
  function init(c: JContext; attrs: JAttributeSet): JViewGroup_LayoutParams; cdecl; overload;
  function init(width: Integer; height: Integer): JViewGroup_LayoutParams; cdecl; overload;
  function init(source: JViewGroup_LayoutParams): JViewGroup_LayoutParams; cdecl; overload;
  {Properties}
  property FILL_PARENT: Integer read _GetFILL_PARENT;
  property MATCH_PARENT: Integer read _GetMATCH_PARENT;
  property WRAP_CONTENT: Integer read _GetWRAP_CONTENT;
end;

[JavaSignature('android/view/ViewGroup$LayoutParams')]
JViewGroup_LayoutParams = interface(JObject)
['{81AC5483-A2B0-497B-9441-7866C9ACCBAC}']
  {Property Methods}
  function _Getheight: Integer;
  procedure _Setheight(Value: Integer);
  function _Getwidth: Integer;
  procedure _Setwidth(Value: Integer);
  {Methods}
  procedure resolveLayoutDirection(layoutDirection: Integer); cdecl;
  {Properties}
  property height: Integer read _Getheight write _Setheight;
  property width: Integer read _Getwidth write _Setwidth;
end;
TJViewGroup_LayoutParams = class(TJavaGenericImport<JViewGroup_LayoutParamsClass, JViewGroup_LayoutParams>) end;

JViewGroup_MarginLayoutParamsClass = interface(JViewGroup_LayoutParamsClass)
['{5BE0EE58-1162-4C87-BE61-BAB657F8557D}']
  {Methods}
  function init(c: JContext; attrs: JAttributeSet): JViewGroup_MarginLayoutParams; cdecl; overload;
  function init(width: Integer; height: Integer): JViewGroup_MarginLayoutParams; cdecl; overload;
  function init(source: JViewGroup_MarginLayoutParams): JViewGroup_MarginLayoutParams; cdecl; overload;
  function init(source: JViewGroup_LayoutParams): JViewGroup_MarginLayoutParams; cdecl; overload;
end;

[JavaSignature('android/view/ViewGroup$MarginLayoutParams')]
JViewGroup_MarginLayoutParams = interface(JViewGroup_LayoutParams)
['{ABE3DE12-48CD-4B1D-996E-6DE619E74BE4}']
  {Property Methods}
  function _GetbottomMargin: Integer;
  procedure _SetbottomMargin(Value: Integer);
  function _GetleftMargin: Integer;
  procedure _SetleftMargin(Value: Integer);
  function _GetrightMargin: Integer;
  procedure _SetrightMargin(Value: Integer);
  function _GettopMargin: Integer;
  procedure _SettopMargin(Value: Integer);
  {Methods}
  function getLayoutDirection: Integer; cdecl;
  function getMarginEnd: Integer; cdecl;
  function getMarginStart: Integer; cdecl;
  function isMarginRelative: Boolean; cdecl;
  procedure resolveLayoutDirection(layoutDirection: Integer); cdecl;
  procedure setLayoutDirection(layoutDirection: Integer); cdecl;
  procedure setMarginEnd(end_: Integer); cdecl;
  procedure setMarginStart(start: Integer); cdecl;
  procedure setMargins(left: Integer; top: Integer; right: Integer; bottom: Integer); cdecl;
  {Properties}
  property bottomMargin: Integer read _GetbottomMargin write _SetbottomMargin;
  property leftMargin: Integer read _GetleftMargin write _SetleftMargin;
  property rightMargin: Integer read _GetrightMargin write _SetrightMargin;
  property topMargin: Integer read _GettopMargin write _SettopMargin;
end;
TJViewGroup_MarginLayoutParams = class(TJavaGenericImport<JViewGroup_MarginLayoutParamsClass, JViewGroup_MarginLayoutParams>) end;

JSQLiteStatementClass = interface(JSQLiteProgramClass)
['{D290B8D2-21A6-4B7F-A552-A8816B323BAD}']
end;

[JavaSignature('android/database/sqlite/SQLiteStatement')]
JSQLiteStatement = interface(JSQLiteProgram)
['{AB3828A0-8DEB-40AD-AE35-EEEFAA60A96B}']
  {Methods}
  procedure execute; cdecl;
  function executeInsert: Int64; cdecl;
  function executeUpdateDelete: Integer; cdecl;
  function simpleQueryForBlobFileDescriptor: JParcelFileDescriptor; cdecl;
  function simpleQueryForLong: Int64; cdecl;
  function simpleQueryForString: JString; cdecl;
  function toString: JString; cdecl;
end;
TJSQLiteStatement = class(TJavaGenericImport<JSQLiteStatementClass, JSQLiteStatement>) end;

JViewTreeObserver_OnGlobalLayoutListenerClass = interface(IJavaClass)
['{FCAAD8E4-B4C8-4FEE-BEED-3F3668AE33E7}']
end;

[JavaSignature('android/view/ViewTreeObserver$OnGlobalLayoutListener')]
JViewTreeObserver_OnGlobalLayoutListener = interface(IJavaInstance)
['{BF64D09F-8760-4651-AB66-675EEF7D73C4}']
  {Methods}
  procedure onGlobalLayout; cdecl;
end;
TJViewTreeObserver_OnGlobalLayoutListener = class(TJavaGenericImport<JViewTreeObserver_OnGlobalLayoutListenerClass, JViewTreeObserver_OnGlobalLayoutListener>) end;

JView_OnLongClickListenerClass = interface(IJavaClass)
['{15319AED-FAAF-43BF-841B-757694000752}']
end;

[JavaSignature('android/view/View$OnLongClickListener')]
JView_OnLongClickListener = interface(IJavaInstance)
['{0895EAD0-05FE-4EFA-ACDF-199A50D2E2BC}']
  {Methods}
  function onLongClick(v: JView): Boolean; cdecl;
end;
TJView_OnLongClickListener = class(TJavaGenericImport<JView_OnLongClickListenerClass, JView_OnLongClickListener>) end;

JKeyEvent_DispatcherStateClass = interface(JObjectClass)
['{177CF315-60C0-49F2-B775-6402E70AFA6C}']
  {Methods}
  function init: JKeyEvent_DispatcherState; cdecl;
end;

[JavaSignature('android/view/KeyEvent$DispatcherState')]
JKeyEvent_DispatcherState = interface(JObject)
['{A8C80695-5C72-4B34-91F0-F4FADF05FEBA}']
  {Methods}
  procedure handleUpEvent(event: JKeyEvent); cdecl;
  function isTracking(event: JKeyEvent): Boolean; cdecl;
  procedure performedLongPress(event: JKeyEvent); cdecl;
  procedure reset; cdecl; overload;
  procedure reset(target: JObject); cdecl; overload;
  procedure startTracking(event: JKeyEvent; target: JObject); cdecl;
end;
TJKeyEvent_DispatcherState = class(TJavaGenericImport<JKeyEvent_DispatcherStateClass, JKeyEvent_DispatcherState>) end;

JRegionIteratorClass = interface(JObjectClass)
['{A98F661A-961F-43E5-9B0D-B2D9ADD717AA}']
  {Methods}
  function init(region: JRegion): JRegionIterator; cdecl;
end;

[JavaSignature('android/graphics/RegionIterator')]
JRegionIterator = interface(JObject)
['{D48A2745-3ECF-4FC9-B6E8-EF93534C98BB}']
  {Methods}
  function next(r: JRect): Boolean; cdecl;
end;
TJRegionIterator = class(TJavaGenericImport<JRegionIteratorClass, JRegionIterator>) end;

JRectClass = interface(JObjectClass)
['{8D5B11EB-2267-4991-A011-298C12EE0B34}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Methods}
  function init: JRect; cdecl; overload;
  function init(left: Integer; top: Integer; right: Integer; bottom: Integer): JRect; cdecl; overload;
  function init(r: JRect): JRect; cdecl; overload;
  function intersects(a: JRect; b: JRect): Boolean; cdecl; overload;
  function unflattenFromString(str: JString): JRect; cdecl;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/graphics/Rect')]
JRect = interface(JObject)
['{E5D89D45-DCA4-4805-BF55-B6BE7CE968C0}']
  {Property Methods}
  function _Getbottom: Integer;
  procedure _Setbottom(Value: Integer);
  function _Getleft: Integer;
  procedure _Setleft(Value: Integer);
  function _Getright: Integer;
  procedure _Setright(Value: Integer);
  function _Gettop: Integer;
  procedure _Settop(Value: Integer);
  {Methods}
  function centerX: Integer; cdecl;
  function centerY: Integer; cdecl;
  function &contains(x: Integer; y: Integer): Boolean; cdecl; overload;
  function &contains(left: Integer; top: Integer; right: Integer; bottom: Integer): Boolean; cdecl; overload;
  function &contains(r: JRect): Boolean; cdecl; overload;
  function describeContents: Integer; cdecl;
  function equals(o: JObject): Boolean; cdecl;
  function exactCenterX: Single; cdecl;
  function exactCenterY: Single; cdecl;
  function flattenToString: JString; cdecl;
  function hashCode: Integer; cdecl;
  function height: Integer; cdecl;
  procedure inset(dx: Integer; dy: Integer); cdecl;
  function intersect(left: Integer; top: Integer; right: Integer; bottom: Integer): Boolean; cdecl; overload;
  function intersect(r: JRect): Boolean; cdecl; overload;
  function intersects(left: Integer; top: Integer; right: Integer; bottom: Integer): Boolean; cdecl; overload;
  function isEmpty: Boolean; cdecl;
  procedure offset(dx: Integer; dy: Integer); cdecl;
  procedure offsetTo(newLeft: Integer; newTop: Integer); cdecl;
  procedure readFromParcel(in_: JParcel); cdecl;
  procedure &set(left: Integer; top: Integer; right: Integer; bottom: Integer); cdecl; overload;
  procedure &set(src: JRect); cdecl; overload;
  procedure setEmpty; cdecl;
  function setIntersect(a: JRect; b: JRect): Boolean; cdecl;
  procedure sort; cdecl;
  function toShortString: JString; cdecl;
  function toString: JString; cdecl;
  procedure union(left: Integer; top: Integer; right: Integer; bottom: Integer); cdecl; overload;
  procedure union(r: JRect); cdecl; overload;
  procedure union(x: Integer; y: Integer); cdecl; overload;
  function width: Integer; cdecl;
  procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
  {Properties}
  property bottom: Integer read _Getbottom write _Setbottom;
  property left: Integer read _Getleft write _Setleft;
  property right: Integer read _Getright write _Setright;
  property top: Integer read _Gettop write _Settop;
end;
TJRect = class(TJavaGenericImport<JRectClass, JRect>) end;

JContentProviderResultClass = interface(JObjectClass)
['{0D9801F6-6AC7-41F4-B54B-1B3AFFDFCCEB}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Methods}
  function init(uri: Jnet_Uri): JContentProviderResult; cdecl; overload;
  function init(count: Integer): JContentProviderResult; cdecl; overload;
  function init(source: JParcel): JContentProviderResult; cdecl; overload;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/content/ContentProviderResult')]
JContentProviderResult = interface(JObject)
['{675498D2-9DDF-489E-9B8B-C41CEACF829E}']
  {Property Methods}
  function _Getcount: JInteger;
  function _Geturi: Jnet_Uri;
  {Methods}
  function describeContents: Integer; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  {Properties}
  property count: JInteger read _Getcount;
  property uri: Jnet_Uri read _Geturi;
end;
TJContentProviderResult = class(TJavaGenericImport<JContentProviderResultClass, JContentProviderResult>) end;

JAsyncQueryHandlerClass = interface(JHandlerClass)
['{DCE5CC9D-9176-4C35-9776-4AAD65F610DE}']
  {Methods}
  function init(cr: JContentResolver): JAsyncQueryHandler; cdecl;
end;

[JavaSignature('android/content/AsyncQueryHandler')]
JAsyncQueryHandler = interface(JHandler)
['{82729BB8-F6CC-4636-A1F9-736EA8141F63}']
  {Methods}
  procedure cancelOperation(token: Integer); cdecl;
  procedure handleMessage(msg: JMessage); cdecl;
  procedure startDelete(token: Integer; cookie: JObject; uri: Jnet_Uri; selection: JString; selectionArgs: TJavaObjectArray<JString>); cdecl;
  procedure startInsert(token: Integer; cookie: JObject; uri: Jnet_Uri; initialValues: JContentValues); cdecl;
  procedure startQuery(token: Integer; cookie: JObject; uri: Jnet_Uri; projection: TJavaObjectArray<JString>; selection: JString; selectionArgs: TJavaObjectArray<JString>; orderBy: JString); cdecl;
  procedure startUpdate(token: Integer; cookie: JObject; uri: Jnet_Uri; values: JContentValues; selection: JString; selectionArgs: TJavaObjectArray<JString>); cdecl;
end;
TJAsyncQueryHandler = class(TJavaGenericImport<JAsyncQueryHandlerClass, JAsyncQueryHandler>) end;

JAssetFileDescriptor_AutoCloseOutputStreamClass = interface(JParcelFileDescriptor_AutoCloseOutputStreamClass)
['{7AEED44A-DF3A-492F-8BCF-11A2A39E1A9F}']
  {Methods}
  function init(fd: JAssetFileDescriptor): JAssetFileDescriptor_AutoCloseOutputStream; cdecl;
end;

[JavaSignature('android/content/res/AssetFileDescriptor$AutoCloseOutputStream')]
JAssetFileDescriptor_AutoCloseOutputStream = interface(JParcelFileDescriptor_AutoCloseOutputStream)
['{F0D74178-1D4A-4023-B6C6-EA9E7643CA31}']
  {Methods}
  procedure write(buffer: TJavaArray<Byte>; offset: Integer; count: Integer); cdecl; overload;
  procedure write(buffer: TJavaArray<Byte>); cdecl; overload;
  procedure write(oneByte: Integer); cdecl; overload;
end;
TJAssetFileDescriptor_AutoCloseOutputStream = class(TJavaGenericImport<JAssetFileDescriptor_AutoCloseOutputStreamClass, JAssetFileDescriptor_AutoCloseOutputStream>) end;

JAssetFileDescriptorClass = interface(JObjectClass)
['{018B7332-9F89-4C15-9916-0A1070375BD4}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  function _GetUNKNOWN_LENGTH: Int64;
  {Methods}
  function init(fd: JParcelFileDescriptor; startOffset: Int64; length: Int64): JAssetFileDescriptor; cdecl;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
  property UNKNOWN_LENGTH: Int64 read _GetUNKNOWN_LENGTH;
end;

[JavaSignature('android/content/res/AssetFileDescriptor')]
JAssetFileDescriptor = interface(JObject)
['{DB464100-96DF-443A-A210-BF6B9937911B}']
  {Methods}
  procedure close; cdecl;
  function createInputStream: JFileInputStream; cdecl;
  function createOutputStream: JFileOutputStream; cdecl;
  function describeContents: Integer; cdecl;
  function getDeclaredLength: Int64; cdecl;
  function getFileDescriptor: JFileDescriptor; cdecl;
  function getLength: Int64; cdecl;
  function getParcelFileDescriptor: JParcelFileDescriptor; cdecl;
  function getStartOffset: Int64; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
end;
TJAssetFileDescriptor = class(TJavaGenericImport<JAssetFileDescriptorClass, JAssetFileDescriptor>) end;

JWindowClass = interface(JObjectClass)
['{C2183A64-1ACD-4D95-8E51-7063E03D9E71}']
  {Property Methods}
  function _GetFEATURE_ACTION_BAR: Integer;
  function _GetFEATURE_ACTION_BAR_OVERLAY: Integer;
  function _GetFEATURE_ACTION_MODE_OVERLAY: Integer;
  function _GetFEATURE_CONTEXT_MENU: Integer;
  function _GetFEATURE_CUSTOM_TITLE: Integer;
  function _GetFEATURE_INDETERMINATE_PROGRESS: Integer;
  function _GetFEATURE_LEFT_ICON: Integer;
  function _GetFEATURE_NO_TITLE: Integer;
  function _GetFEATURE_OPTIONS_PANEL: Integer;
  function _GetFEATURE_PROGRESS: Integer;
  function _GetFEATURE_RIGHT_ICON: Integer;
  function _GetID_ANDROID_CONTENT: Integer;
  function _GetPROGRESS_END: Integer;
  function _GetPROGRESS_INDETERMINATE_OFF: Integer;
  function _GetPROGRESS_INDETERMINATE_ON: Integer;
  function _GetPROGRESS_SECONDARY_END: Integer;
  function _GetPROGRESS_SECONDARY_START: Integer;
  function _GetPROGRESS_START: Integer;
  function _GetPROGRESS_VISIBILITY_OFF: Integer;
  function _GetPROGRESS_VISIBILITY_ON: Integer;
  {Methods}
  function init(context: JContext): JWindow; cdecl;
  {Properties}
  property FEATURE_ACTION_BAR: Integer read _GetFEATURE_ACTION_BAR;
  property FEATURE_ACTION_BAR_OVERLAY: Integer read _GetFEATURE_ACTION_BAR_OVERLAY;
  property FEATURE_ACTION_MODE_OVERLAY: Integer read _GetFEATURE_ACTION_MODE_OVERLAY;
  property FEATURE_CONTEXT_MENU: Integer read _GetFEATURE_CONTEXT_MENU;
  property FEATURE_CUSTOM_TITLE: Integer read _GetFEATURE_CUSTOM_TITLE;
  property FEATURE_INDETERMINATE_PROGRESS: Integer read _GetFEATURE_INDETERMINATE_PROGRESS;
  property FEATURE_LEFT_ICON: Integer read _GetFEATURE_LEFT_ICON;
  property FEATURE_NO_TITLE: Integer read _GetFEATURE_NO_TITLE;
  property FEATURE_OPTIONS_PANEL: Integer read _GetFEATURE_OPTIONS_PANEL;
  property FEATURE_PROGRESS: Integer read _GetFEATURE_PROGRESS;
  property FEATURE_RIGHT_ICON: Integer read _GetFEATURE_RIGHT_ICON;
  property ID_ANDROID_CONTENT: Integer read _GetID_ANDROID_CONTENT;
  property PROGRESS_END: Integer read _GetPROGRESS_END;
  property PROGRESS_INDETERMINATE_OFF: Integer read _GetPROGRESS_INDETERMINATE_OFF;
  property PROGRESS_INDETERMINATE_ON: Integer read _GetPROGRESS_INDETERMINATE_ON;
  property PROGRESS_SECONDARY_END: Integer read _GetPROGRESS_SECONDARY_END;
  property PROGRESS_SECONDARY_START: Integer read _GetPROGRESS_SECONDARY_START;
  property PROGRESS_START: Integer read _GetPROGRESS_START;
  property PROGRESS_VISIBILITY_OFF: Integer read _GetPROGRESS_VISIBILITY_OFF;
  property PROGRESS_VISIBILITY_ON: Integer read _GetPROGRESS_VISIBILITY_ON;
end;

[JavaSignature('android/view/Window')]
JWindow = interface(JObject)
['{A9AC44DA-219C-41C4-AD29-88B01254819E}']
  {Methods}
  procedure addContentView(view: JView; params: JViewGroup_LayoutParams); cdecl;
  procedure addFlags(flags: Integer); cdecl;
  procedure clearFlags(flags: Integer); cdecl;
  procedure closeAllPanels; cdecl;
  procedure closePanel(featureId: Integer); cdecl;
  function findViewById(id: Integer): JView; cdecl;
  function getAttributes: JWindowManager_LayoutParams; cdecl;
  function getCallback: JWindow_Callback; cdecl;
  function getContainer: JWindow; cdecl;
  function getContext: JContext; cdecl;
  function getCurrentFocus: JView; cdecl;
  function getDecorView: JView; cdecl;
  function getLayoutInflater: JLayoutInflater; cdecl;
  function getVolumeControlStream: Integer; cdecl;
  function getWindowManager: JWindowManager; cdecl;
  function getWindowStyle: JTypedArray; cdecl;
  function hasChildren: Boolean; cdecl;
  function hasFeature(feature: Integer): Boolean; cdecl;
  procedure invalidatePanelMenu(featureId: Integer); cdecl;
  function isActive: Boolean; cdecl;
  function isFloating: Boolean; cdecl;
  function isShortcutKey(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  procedure makeActive; cdecl;
  procedure onConfigurationChanged(newConfig: JConfiguration); cdecl;
  procedure openPanel(featureId: Integer; event: JKeyEvent); cdecl;
  function peekDecorView: JView; cdecl;
  function performContextMenuIdentifierAction(id: Integer; flags: Integer): Boolean; cdecl;
  function performPanelIdentifierAction(featureId: Integer; id: Integer; flags: Integer): Boolean; cdecl;
  function performPanelShortcut(featureId: Integer; keyCode: Integer; event: JKeyEvent; flags: Integer): Boolean; cdecl;
  function requestFeature(featureId: Integer): Boolean; cdecl;
  procedure restoreHierarchyState(savedInstanceState: JBundle); cdecl;
  function saveHierarchyState: JBundle; cdecl;
  procedure setAttributes(a: JWindowManager_LayoutParams); cdecl;
  procedure setBackgroundDrawable(drawable: JDrawable); cdecl;
  procedure setBackgroundDrawableResource(resid: Integer); cdecl;
  procedure setCallback(callback: JWindow_Callback); cdecl;
  procedure setChildDrawable(featureId: Integer; drawable: JDrawable); cdecl;
  procedure setChildInt(featureId: Integer; value: Integer); cdecl;
  procedure setContainer(container: JWindow); cdecl;
  procedure setContentView(layoutResID: Integer); cdecl; overload;
  procedure setContentView(view: JView); cdecl; overload;
  procedure setContentView(view: JView; params: JViewGroup_LayoutParams); cdecl; overload;
  procedure setDimAmount(amount: Single); cdecl;
  procedure setFeatureDrawable(featureId: Integer; drawable: JDrawable); cdecl;
  procedure setFeatureDrawableAlpha(featureId: Integer; alpha: Integer); cdecl;
  procedure setFeatureDrawableResource(featureId: Integer; resId: Integer); cdecl;
  procedure setFeatureDrawableUri(featureId: Integer; uri: Jnet_Uri); cdecl;
  procedure setFeatureInt(featureId: Integer; value: Integer); cdecl;
  procedure setFlags(flags: Integer; mask: Integer); cdecl;
  procedure setFormat(format: Integer); cdecl;
  procedure setGravity(gravity: Integer); cdecl;
  procedure setLayout(width: Integer; height: Integer); cdecl;
  procedure setSoftInputMode(mode: Integer); cdecl;
  procedure setTitle(title: JCharSequence); cdecl;
  procedure setTitleColor(textColor: Integer); cdecl;
  procedure setType(type_: Integer); cdecl;
  procedure setUiOptions(uiOptions: Integer); cdecl; overload;
  procedure setUiOptions(uiOptions: Integer; mask: Integer); cdecl; overload;
  procedure setVolumeControlStream(streamType: Integer); cdecl;
  procedure setWindowAnimations(resId: Integer); cdecl;
  procedure setWindowManager(wm: JWindowManager; appToken: JIBinder; appName: JString); cdecl; overload;
  procedure setWindowManager(wm: JWindowManager; appToken: JIBinder; appName: JString; hardwareAccelerated: Boolean); cdecl; overload;
  function superDispatchGenericMotionEvent(event: JMotionEvent): Boolean; cdecl;
  function superDispatchKeyEvent(event: JKeyEvent): Boolean; cdecl;
  function superDispatchKeyShortcutEvent(event: JKeyEvent): Boolean; cdecl;
  function superDispatchTouchEvent(event: JMotionEvent): Boolean; cdecl;
  function superDispatchTrackballEvent(event: JMotionEvent): Boolean; cdecl;
  procedure takeInputQueue(callback: JInputQueue_Callback); cdecl;
  procedure takeKeyEvents(get_: Boolean); cdecl;
  procedure takeSurface(callback: JSurfaceHolder_Callback2); cdecl;
  procedure togglePanel(featureId: Integer; event: JKeyEvent); cdecl;
end;
TJWindow = class(TJavaGenericImport<JWindowClass, JWindow>) end;

JInputConnectionWrapperClass = interface(JObjectClass)
['{3BA5DB87-DFF3-481E-80E4-C77B379A7302}']
  {Methods}
  function init(target: JInputConnection; mutable: Boolean): JInputConnectionWrapper; cdecl;
end;

[JavaSignature('android/view/inputmethod/InputConnectionWrapper')]
JInputConnectionWrapper = interface(JObject)
['{E5C3C7C9-8428-4ED4-B92C-058B1017500B}']
  {Methods}
  function beginBatchEdit: Boolean; cdecl;
  function clearMetaKeyStates(states: Integer): Boolean; cdecl;
  function commitCompletion(text: JCompletionInfo): Boolean; cdecl;
  function commitCorrection(correctionInfo: JCorrectionInfo): Boolean; cdecl;
  function commitText(text: JCharSequence; newCursorPosition: Integer): Boolean; cdecl;
  function deleteSurroundingText(beforeLength: Integer; afterLength: Integer): Boolean; cdecl;
  function endBatchEdit: Boolean; cdecl;
  function finishComposingText: Boolean; cdecl;
  function getCursorCapsMode(reqModes: Integer): Integer; cdecl;
  function getExtractedText(request: JExtractedTextRequest; flags: Integer): JExtractedText; cdecl;
  function getSelectedText(flags: Integer): JCharSequence; cdecl;
  function getTextAfterCursor(n: Integer; flags: Integer): JCharSequence; cdecl;
  function getTextBeforeCursor(n: Integer; flags: Integer): JCharSequence; cdecl;
  function performContextMenuAction(id: Integer): Boolean; cdecl;
  function performEditorAction(editorAction: Integer): Boolean; cdecl;
  function performPrivateCommand(action: JString; data: JBundle): Boolean; cdecl;
  function reportFullscreenMode(enabled: Boolean): Boolean; cdecl;
  function sendKeyEvent(event: JKeyEvent): Boolean; cdecl;
  function setComposingRegion(start: Integer; end_: Integer): Boolean; cdecl;
  function setComposingText(text: JCharSequence; newCursorPosition: Integer): Boolean; cdecl;
  function setSelection(start: Integer; end_: Integer): Boolean; cdecl;
  procedure setTarget(target: JInputConnection); cdecl;
end;
TJInputConnectionWrapper = class(TJavaGenericImport<JInputConnectionWrapperClass, JInputConnectionWrapper>) end;

JWindow_CallbackClass = interface(IJavaClass)
['{772DBA67-C77A-4EBE-9D06-7E6E737CDC41}']
end;

[JavaSignature('android/view/Window$Callback')]
JWindow_Callback = interface(IJavaInstance)
['{0ABD51F1-DC48-4EE0-9459-7EC39345F8BA}']
  {Methods}
  function dispatchGenericMotionEvent(event: JMotionEvent): Boolean; cdecl;
  function dispatchKeyEvent(event: JKeyEvent): Boolean; cdecl;
  function dispatchKeyShortcutEvent(event: JKeyEvent): Boolean; cdecl;
  function dispatchTouchEvent(event: JMotionEvent): Boolean; cdecl;
  function dispatchTrackballEvent(event: JMotionEvent): Boolean; cdecl;
  procedure onActionModeFinished(mode: JActionMode); cdecl;
  procedure onActionModeStarted(mode: JActionMode); cdecl;
  procedure onAttachedToWindow; cdecl;
  procedure onContentChanged; cdecl;
  function onCreatePanelMenu(featureId: Integer; menu: JMenu): Boolean; cdecl;
  function onCreatePanelView(featureId: Integer): JView; cdecl;
  procedure onDetachedFromWindow; cdecl;
  function onMenuItemSelected(featureId: Integer; item: JMenuItem): Boolean; cdecl;
  function onMenuOpened(featureId: Integer; menu: JMenu): Boolean; cdecl;
  procedure onPanelClosed(featureId: Integer; menu: JMenu); cdecl;
  function onPreparePanel(featureId: Integer; view: JView; menu: JMenu): Boolean; cdecl;
  function onSearchRequested: Boolean; cdecl;
  procedure onWindowAttributesChanged(attrs: JWindowManager_LayoutParams); cdecl;
  procedure onWindowFocusChanged(hasFocus: Boolean); cdecl;
  function onWindowStartingActionMode(callback: JActionMode_Callback): JActionMode; cdecl;
end;
TJWindow_Callback = class(TJavaGenericImport<JWindow_CallbackClass, JWindow_Callback>) end;

JSpannable_FactoryClass = interface(JObjectClass)
['{D715DAF0-9891-4F6A-8625-FEB82A2EB8F5}']
  {Methods}
  function init: JSpannable_Factory; cdecl;
  function getInstance: JSpannable_Factory; cdecl;
end;

[JavaSignature('android/text/Spannable$Factory')]
JSpannable_Factory = interface(JObject)
['{C7DCC285-BA5B-4B63-B003-5DC3FB9791FC}']
  {Methods}
  function newSpannable(source: JCharSequence): JSpannable; cdecl;
end;
TJSpannable_Factory = class(TJavaGenericImport<JSpannable_FactoryClass, JSpannable_Factory>) end;

JRegionClass = interface(JObjectClass)
['{53503A08-DB57-45F2-8DEB-C728DC2749AD}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Methods}
  function init: JRegion; cdecl; overload;
  function init(region: JRegion): JRegion; cdecl; overload;
  function init(r: JRect): JRegion; cdecl; overload;
  function init(left: Integer; top: Integer; right: Integer; bottom: Integer): JRegion; cdecl; overload;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/graphics/Region')]
JRegion = interface(JObject)
['{7CEA0E0F-4559-48D7-8C5F-9528722D5E7D}']
  {Methods}
  function &contains(x: Integer; y: Integer): Boolean; cdecl;
  function describeContents: Integer; cdecl;
  function equals(obj: JObject): Boolean; cdecl;
  function getBoundaryPath: JPath; cdecl; overload;
  function getBoundaryPath(path: JPath): Boolean; cdecl; overload;
  function getBounds: JRect; cdecl; overload;
  function getBounds(r: JRect): Boolean; cdecl; overload;
  function isComplex: Boolean; cdecl;
  function isEmpty: Boolean; cdecl;
  function isRect: Boolean; cdecl;
  function op(r: JRect; op: JRegion_Op): Boolean; cdecl; overload;
  function op(left: Integer; top: Integer; right: Integer; bottom: Integer; op: JRegion_Op): Boolean; cdecl; overload;
  function op(region: JRegion; op: JRegion_Op): Boolean; cdecl; overload;
  function op(rect: JRect; region: JRegion; op: JRegion_Op): Boolean; cdecl; overload;
  function op(region1: JRegion; region2: JRegion; op: JRegion_Op): Boolean; cdecl; overload;
  function quickContains(r: JRect): Boolean; cdecl; overload;
  function quickContains(left: Integer; top: Integer; right: Integer; bottom: Integer): Boolean; cdecl; overload;
  function quickReject(r: JRect): Boolean; cdecl; overload;
  function quickReject(left: Integer; top: Integer; right: Integer; bottom: Integer): Boolean; cdecl; overload;
  function quickReject(rgn: JRegion): Boolean; cdecl; overload;
  function &set(region: JRegion): Boolean; cdecl; overload;
  function &set(r: JRect): Boolean; cdecl; overload;
  function &set(left: Integer; top: Integer; right: Integer; bottom: Integer): Boolean; cdecl; overload;
  procedure setEmpty; cdecl;
  function setPath(path: JPath; clip: JRegion): Boolean; cdecl;
  function toString: JString; cdecl;
  procedure translate(dx: Integer; dy: Integer); cdecl; overload;
  procedure translate(dx: Integer; dy: Integer; dst: JRegion); cdecl; overload;
  function union(r: JRect): Boolean; cdecl;
  procedure writeToParcel(p: JParcel; flags: Integer); cdecl;
end;
TJRegion = class(TJavaGenericImport<JRegionClass, JRegion>) end;

JClipDataClass = interface(JObjectClass)
['{350C6650-0BF9-4608-A3E8-A083C5ED4543}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Methods}
  function init(label_: JCharSequence; mimeTypes: TJavaObjectArray<JString>; item: JClipData_Item): JClipData; cdecl; overload;
  function init(description: JClipDescription; item: JClipData_Item): JClipData; cdecl; overload;
  function init(other: JClipData): JClipData; cdecl; overload;
  function newHtmlText(label_: JCharSequence; text: JCharSequence; htmlText: JString): JClipData; cdecl;
  function newIntent(label_: JCharSequence; intent: JIntent): JClipData; cdecl;
  function newPlainText(label_: JCharSequence; text: JCharSequence): JClipData; cdecl;
  function newRawUri(label_: JCharSequence; uri: Jnet_Uri): JClipData; cdecl;
  function newUri(resolver: JContentResolver; label_: JCharSequence; uri: Jnet_Uri): JClipData; cdecl;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/content/ClipData')]
JClipData = interface(JObject)
['{C2884715-49ED-4BFE-BCE3-204FD4DAA417}']
  {Methods}
  procedure addItem(item: JClipData_Item); cdecl;
  function describeContents: Integer; cdecl;
  function getDescription: JClipDescription; cdecl;
  function getItemAt(index: Integer): JClipData_Item; cdecl;
  function getItemCount: Integer; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
end;
TJClipData = class(TJavaGenericImport<JClipDataClass, JClipData>) end;

JSharedPreferences_EditorClass = interface(IJavaClass)
['{EF362B32-D6AD-4EC6-BACA-094E25708649}']
end;

[JavaSignature('android/content/SharedPreferences$Editor')]
JSharedPreferences_Editor = interface(IJavaInstance)
['{A162AACF-DD6D-466E-838B-363E6B092CA4}']
  {Methods}
  procedure apply; cdecl;
  function clear: JSharedPreferences_Editor; cdecl;
  function commit: Boolean; cdecl;
  function putBoolean(key: JString; value: Boolean): JSharedPreferences_Editor; cdecl;
  function putFloat(key: JString; value: Single): JSharedPreferences_Editor; cdecl;
  function putInt(key: JString; value: Integer): JSharedPreferences_Editor; cdecl;
  function putLong(key: JString; value: Int64): JSharedPreferences_Editor; cdecl;
  function putString(key: JString; value: JString): JSharedPreferences_Editor; cdecl;
  function putStringSet(key: JString; values: JSet): JSharedPreferences_Editor; cdecl;
  function remove(key: JString): JSharedPreferences_Editor; cdecl;
end;
TJSharedPreferences_Editor = class(TJavaGenericImport<JSharedPreferences_EditorClass, JSharedPreferences_Editor>) end;

JBroadcastReceiverClass = interface(JObjectClass)
['{633E207B-4483-4EB5-B2B1-C7830A2237A9}']
  {Methods}
  function init: JBroadcastReceiver; cdecl;
end;

[JavaSignature('android/content/BroadcastReceiver')]
JBroadcastReceiver = interface(JObject)
['{32FE5004-DE3B-4733-98E2-A37B87A6F663}']
  {Methods}
  procedure abortBroadcast; cdecl;
  procedure clearAbortBroadcast; cdecl;
  function getAbortBroadcast: Boolean; cdecl;
  function getDebugUnregister: Boolean; cdecl;
  function getResultCode: Integer; cdecl;
  function getResultData: JString; cdecl;
  function getResultExtras(makeMap: Boolean): JBundle; cdecl;
  function goAsync: JBroadcastReceiver_PendingResult; cdecl;
  function isInitialStickyBroadcast: Boolean; cdecl;
  function isOrderedBroadcast: Boolean; cdecl;
  procedure onReceive(context: JContext; intent: JIntent); cdecl;
  function peekService(myContext: JContext; service: JIntent): JIBinder; cdecl;
  procedure setDebugUnregister(debug: Boolean); cdecl;
  procedure setOrderedHint(isOrdered: Boolean); cdecl;
  procedure setResult(code: Integer; data: JString; extras: JBundle); cdecl;
  procedure setResultCode(code: Integer); cdecl;
  procedure setResultData(data: JString); cdecl;
  procedure setResultExtras(extras: JBundle); cdecl;
end;
TJBroadcastReceiver = class(TJavaGenericImport<JBroadcastReceiverClass, JBroadcastReceiver>) end;

JSyncStatusObserverClass = interface(IJavaClass)
['{BD8BC3F1-68C5-4649-B659-1E51F3934376}']
end;

[JavaSignature('android/content/SyncStatusObserver')]
JSyncStatusObserver = interface(IJavaInstance)
['{1DFEEAD6-94AE-48B6-AF87-1200CEADAECC}']
  {Methods}
  procedure onStatusChanged(which: Integer); cdecl;
end;
TJSyncStatusObserver = class(TJavaGenericImport<JSyncStatusObserverClass, JSyncStatusObserver>) end;

JView_DragShadowBuilderClass = interface(JObjectClass)
['{1E9C9A18-F23F-4938-A011-EA78BF5F2AC0}']
  {Methods}
  function init(view: JView): JView_DragShadowBuilder; cdecl; overload;
  function init: JView_DragShadowBuilder; cdecl; overload;
end;

[JavaSignature('android/view/View$DragShadowBuilder')]
JView_DragShadowBuilder = interface(JObject)
['{383BA36F-4F18-4AB3-9E63-AD32EF5DFEA5}']
  {Methods}
  function getView: JView; cdecl;
  procedure onDrawShadow(canvas: JCanvas); cdecl;
  procedure onProvideShadowMetrics(shadowSize: JPoint; shadowTouchPoint: JPoint); cdecl;
end;
TJView_DragShadowBuilder = class(TJavaGenericImport<JView_DragShadowBuilderClass, JView_DragShadowBuilder>) end;

JViewTreeObserver_OnDrawListenerClass = interface(IJavaClass)
['{77519968-B2FC-485D-A508-0BA6B6EB04EB}']
end;

[JavaSignature('android/view/ViewTreeObserver$OnDrawListener')]
JViewTreeObserver_OnDrawListener = interface(IJavaInstance)
['{D7DD6757-C930-4347-896B-49771A88B68C}']
  {Methods}
  procedure onDraw; cdecl;
end;
TJViewTreeObserver_OnDrawListener = class(TJavaGenericImport<JViewTreeObserver_OnDrawListenerClass, JViewTreeObserver_OnDrawListener>) end;

JDashPathEffectClass = interface(JPathEffectClass)
['{59B6B7FB-43F1-4789-836E-2E77BD324294}']
  {Methods}
  function init(intervals: TJavaArray<Single>; phase: Single): JDashPathEffect; cdecl;
end;

[JavaSignature('android/graphics/DashPathEffect')]
JDashPathEffect = interface(JPathEffect)
['{62E19601-80A3-479E-8E3F-E6EDB5561BFB}']
end;
TJDashPathEffect = class(TJavaGenericImport<JDashPathEffectClass, JDashPathEffect>) end;

JClipboardManagerClass = interface(JObjectClass)
['{CA54A5DE-6509-4DC9-89C6-CF1D6009466E}']
  {Methods}
  function init: JClipboardManager; cdecl;
end;

[JavaSignature('android/text/ClipboardManager')]
JClipboardManager = interface(JObject)
['{0053FEAD-CD66-4FA9-934F-02AD63669797}']
  {Methods}
  function getText: JCharSequence; cdecl;
  function hasText: Boolean; cdecl;
  procedure setText(text: JCharSequence); cdecl;
end;
TJClipboardManager = class(TJavaGenericImport<JClipboardManagerClass, JClipboardManager>) end;

JPaint_FontMetricsClass = interface(JObjectClass)
['{15A2A7FE-09DB-487F-A3A6-4A8B1670412E}']
  {Methods}
  function init: JPaint_FontMetrics; cdecl;
end;

[JavaSignature('android/graphics/Paint$FontMetrics')]
JPaint_FontMetrics = interface(JObject)
['{91263E0D-1BA1-4DAD-BD30-905C0E7DC043}']
  {Property Methods}
  function _Getascent: Single;
  procedure _Setascent(Value: Single);
  function _Getbottom: Single;
  procedure _Setbottom(Value: Single);
  function _Getdescent: Single;
  procedure _Setdescent(Value: Single);
  function _Getleading: Single;
  procedure _Setleading(Value: Single);
  function _Gettop: Single;
  procedure _Settop(Value: Single);
  {Properties}
  property ascent: Single read _Getascent write _Setascent;
  property bottom: Single read _Getbottom write _Setbottom;
  property descent: Single read _Getdescent write _Setdescent;
  property leading: Single read _Getleading write _Setleading;
  property top: Single read _Gettop write _Settop;
end;
TJPaint_FontMetrics = class(TJavaGenericImport<JPaint_FontMetricsClass, JPaint_FontMetrics>) end;

JLayoutInflater_Factory2Class = interface(JLayoutInflater_FactoryClass)
['{E157E921-555A-4D25-82AF-60CB4AED982C}']
end;

[JavaSignature('android/view/LayoutInflater$Factory2')]
JLayoutInflater_Factory2 = interface(JLayoutInflater_Factory)
['{FF951177-1917-41A4-99C0-8B707393FACF}']
  {Methods}
  function onCreateView(parent: JView; name: JString; context: JContext; attrs: JAttributeSet): JView; cdecl;
end;
TJLayoutInflater_Factory2 = class(TJavaGenericImport<JLayoutInflater_Factory2Class, JLayoutInflater_Factory2>) end;

JLightingColorFilterClass = interface(JColorFilterClass)
['{2EAA9BD4-23CE-4C6E-BB82-C584956BB5F9}']
  {Methods}
  function init(mul: Integer; add: Integer): JLightingColorFilter; cdecl;
end;

[JavaSignature('android/graphics/LightingColorFilter')]
JLightingColorFilter = interface(JColorFilter)
['{DE516E75-92A9-4E9C-BAE2-1D4911D5A8A2}']
end;
TJLightingColorFilter = class(TJavaGenericImport<JLightingColorFilterClass, JLightingColorFilter>) end;

JRotateDrawableClass = interface(JDrawableClass)
['{58ABB7B5-8AA0-4972-A206-1E70423AEA6A}']
  {Methods}
  function init: JRotateDrawable; cdecl;
end;

[JavaSignature('android/graphics/drawable/RotateDrawable')]
JRotateDrawable = interface(JDrawable)
['{B64FF7BE-FFDC-47C5-9160-85503034A331}']
  {Methods}
  procedure draw(canvas: JCanvas); cdecl;
  function getChangingConfigurations: Integer; cdecl;
  function getConstantState: JDrawable_ConstantState; cdecl;
  function getDrawable: JDrawable; cdecl;
  function getIntrinsicHeight: Integer; cdecl;
  function getIntrinsicWidth: Integer; cdecl;
  function getOpacity: Integer; cdecl;
  function getPadding(padding: JRect): Boolean; cdecl;
  procedure invalidateDrawable(who: JDrawable); cdecl;
  function isStateful: Boolean; cdecl;
  function mutate: JDrawable; cdecl;
  procedure scheduleDrawable(who: JDrawable; what: JRunnable; when: Int64); cdecl;
  procedure setAlpha(alpha: Integer); cdecl;
  procedure setColorFilter(cf: JColorFilter); cdecl;
  function setVisible(visible: Boolean; restart: Boolean): Boolean; cdecl;
  procedure unscheduleDrawable(who: JDrawable; what: JRunnable); cdecl;
end;
TJRotateDrawable = class(TJavaGenericImport<JRotateDrawableClass, JRotateDrawable>) end;

JContentProviderClientClass = interface(JObjectClass)
['{E35B7920-67B1-4ED4-A635-9E3E96A27413}']
end;

[JavaSignature('android/content/ContentProviderClient')]
JContentProviderClient = interface(JObject)
['{3D01ECDB-0FC3-4949-A3C1-53A6A100582F}']
  {Methods}
  function applyBatch(operations: JArrayList): TJavaObjectArray<JContentProviderResult>; cdecl;
  function bulkInsert(url: Jnet_Uri; initialValues: TJavaObjectArray<JContentValues>): Integer; cdecl;
  function call(method: JString; arg: JString; extras: JBundle): JBundle; cdecl;
  function delete(url: Jnet_Uri; selection: JString; selectionArgs: TJavaObjectArray<JString>): Integer; cdecl;
  function getLocalContentProvider: JContentProvider; cdecl;
  function getStreamTypes(url: Jnet_Uri; mimeTypeFilter: JString): TJavaObjectArray<JString>; cdecl;
  function getType(url: Jnet_Uri): JString; cdecl;
  function insert(url: Jnet_Uri; initialValues: JContentValues): Jnet_Uri; cdecl;
  function openAssetFile(url: Jnet_Uri; mode: JString): JAssetFileDescriptor; cdecl;
  function openFile(url: Jnet_Uri; mode: JString): JParcelFileDescriptor; cdecl;
  function openTypedAssetFileDescriptor(uri: Jnet_Uri; mimeType: JString; opts: JBundle): JAssetFileDescriptor; cdecl;
  function query(url: Jnet_Uri; projection: TJavaObjectArray<JString>; selection: JString; selectionArgs: TJavaObjectArray<JString>; sortOrder: JString): JCursor; cdecl; overload;
  function query(url: Jnet_Uri; projection: TJavaObjectArray<JString>; selection: JString; selectionArgs: TJavaObjectArray<JString>; sortOrder: JString; cancellationSignal: JCancellationSignal): JCursor; cdecl; overload;
  function release: Boolean; cdecl;
  function update(url: Jnet_Uri; values: JContentValues; selection: JString; selectionArgs: TJavaObjectArray<JString>): Integer; cdecl;
end;
TJContentProviderClient = class(TJavaGenericImport<JContentProviderClientClass, JContentProviderClient>) end;

JContentUrisClass = interface(JObjectClass)
['{5D5A5751-372E-4023-A753-53D97377774E}']
  {Methods}
  function init: JContentUris; cdecl;
  function appendId(builder: JUri_Builder; id: Int64): JUri_Builder; cdecl;
  function parseId(contentUri: Jnet_Uri): Int64; cdecl;
  function withAppendedId(contentUri: Jnet_Uri; id: Int64): Jnet_Uri; cdecl;
end;

[JavaSignature('android/content/ContentUris')]
JContentUris = interface(JObject)
['{1E64E1A3-4CD2-46E6-9A36-FCD92EEDE341}']
end;
TJContentUris = class(TJavaGenericImport<JContentUrisClass, JContentUris>) end;

JPaintDrawableClass = interface(JShapeDrawableClass)
['{B3EA2085-D3A1-4D2B-88ED-1143599C8DD5}']
  {Methods}
  function init: JPaintDrawable; cdecl; overload;
  function init(color: Integer): JPaintDrawable; cdecl; overload;
end;

[JavaSignature('android/graphics/drawable/PaintDrawable')]
JPaintDrawable = interface(JShapeDrawable)
['{1C57A0BD-E491-49A1-A0A5-46DE37267AC5}']
  {Methods}
  procedure setCornerRadii(radii: TJavaArray<Single>); cdecl;
  procedure setCornerRadius(radius: Single); cdecl;
end;
TJPaintDrawable = class(TJavaGenericImport<JPaintDrawableClass, JPaintDrawable>) end;

JPackageManager_NameNotFoundExceptionClass = interface(JAndroidExceptionClass)
['{88014691-9D44-4E2D-A6E3-EBF4013AB32A}']
  {Methods}
  function init: JPackageManager_NameNotFoundException; cdecl; overload;
  function init(name: JString): JPackageManager_NameNotFoundException; cdecl; overload;
end;

[JavaSignature('android/content/pm/PackageManager$NameNotFoundException')]
JPackageManager_NameNotFoundException = interface(JAndroidException)
['{1ABD6B0B-50BF-48D1-9597-6B4FAC875BDD}']
end;
TJPackageManager_NameNotFoundException = class(TJavaGenericImport<JPackageManager_NameNotFoundExceptionClass, JPackageManager_NameNotFoundException>) end;

JSurfaceTexture_OnFrameAvailableListenerClass = interface(IJavaClass)
['{DC4394C7-850D-48C7-8C48-3A69DADB3F4E}']
end;

[JavaSignature('android/graphics/SurfaceTexture$OnFrameAvailableListener')]
JSurfaceTexture_OnFrameAvailableListener = interface(IJavaInstance)
['{7C82FB69-874D-4DA8-A789-E78F0AB4F4DA}']
  {Methods}
  procedure onFrameAvailable(surfaceTexture: JSurfaceTexture); cdecl;
end;
TJSurfaceTexture_OnFrameAvailableListener = class(TJavaGenericImport<JSurfaceTexture_OnFrameAvailableListenerClass, JSurfaceTexture_OnFrameAvailableListener>) end;

JInterpolator_ResultClass = interface(JEnumClass)
['{A131B06A-1B74-44CD-ADFF-B324ABDE3530}']
  {Property Methods}
  function _GetFREEZE_END: JInterpolator_Result;
  function _GetFREEZE_START: JInterpolator_Result;
  function _GetNORMAL: JInterpolator_Result;
  {Methods}
  function valueOf(name: JString): JInterpolator_Result; cdecl;
  function values: TJavaObjectArray<JInterpolator_Result>; cdecl;
  {Properties}
  property FREEZE_END: JInterpolator_Result read _GetFREEZE_END;
  property FREEZE_START: JInterpolator_Result read _GetFREEZE_START;
  property NORMAL: JInterpolator_Result read _GetNORMAL;
end;

[JavaSignature('android/graphics/Interpolator$Result')]
JInterpolator_Result = interface(JEnum)
['{88FFBFD3-0A6B-4CA3-B941-38D7CBC198BB}']
end;
TJInterpolator_Result = class(TJavaGenericImport<JInterpolator_ResultClass, JInterpolator_Result>) end;

JYuvImageClass = interface(JObjectClass)
['{55F960E0-1DB5-40BA-B289-389C8EBC820D}']
  {Methods}
  function init(yuv: TJavaArray<Byte>; format: Integer; width: Integer; height: Integer; strides: TJavaArray<Integer>): JYuvImage; cdecl;
end;

[JavaSignature('android/graphics/YuvImage')]
JYuvImage = interface(JObject)
['{5A3F0577-3F67-4ABF-94EF-3438EEAE8720}']
  {Methods}
  function compressToJpeg(rectangle: JRect; quality: Integer; stream: JOutputStream): Boolean; cdecl;
  function getHeight: Integer; cdecl;
  function getStrides: TJavaArray<Integer>; cdecl;
  function getWidth: Integer; cdecl;
  function getYuvData: TJavaArray<Byte>; cdecl;
  function getYuvFormat: Integer; cdecl;
end;
TJYuvImage = class(TJavaGenericImport<JYuvImageClass, JYuvImage>) end;

JDrawFilterClass = interface(JObjectClass)
['{4828BAA5-CA85-4263-86AC-4589594C8468}']
  {Methods}
  function init: JDrawFilter; cdecl;
end;

[JavaSignature('android/graphics/DrawFilter')]
JDrawFilter = interface(JObject)
['{70F6DBC6-C410-4E64-B6AB-0862C2D94BF1}']
end;
TJDrawFilter = class(TJavaGenericImport<JDrawFilterClass, JDrawFilter>) end;

JPaintFlagsDrawFilterClass = interface(JDrawFilterClass)
['{F67C7E88-47B8-4728-8E4E-C8625B550A1F}']
  {Methods}
  function init(clearBits: Integer; setBits: Integer): JPaintFlagsDrawFilter; cdecl;
end;

[JavaSignature('android/graphics/PaintFlagsDrawFilter')]
JPaintFlagsDrawFilter = interface(JDrawFilter)
['{F5AEA86E-39D9-4735-A157-4223AD7B26E8}']
end;
TJPaintFlagsDrawFilter = class(TJavaGenericImport<JPaintFlagsDrawFilterClass, JPaintFlagsDrawFilter>) end;

JBitmapRegionDecoderClass = interface(JObjectClass)
['{793D0C30-0517-43A8-9749-A93D4ABD7D68}']
  {Methods}
  function newInstance(data: TJavaArray<Byte>; offset: Integer; length: Integer; isShareable: Boolean): JBitmapRegionDecoder; cdecl; overload;
  function newInstance(fd: JFileDescriptor; isShareable: Boolean): JBitmapRegionDecoder; cdecl; overload;
  function newInstance(is_: JInputStream; isShareable: Boolean): JBitmapRegionDecoder; cdecl; overload;
  function newInstance(pathName: JString; isShareable: Boolean): JBitmapRegionDecoder; cdecl; overload;
end;

[JavaSignature('android/graphics/BitmapRegionDecoder')]
JBitmapRegionDecoder = interface(JObject)
['{C087A6D8-CD30-4D90-9917-5D1D3BE0F9CB}']
  {Methods}
  function decodeRegion(rect: JRect; options: JBitmapFactory_Options): JBitmap; cdecl;
  function getHeight: Integer; cdecl;
  function getWidth: Integer; cdecl;
  function isRecycled: Boolean; cdecl;
  procedure recycle; cdecl;
end;
TJBitmapRegionDecoder = class(TJavaGenericImport<JBitmapRegionDecoderClass, JBitmapRegionDecoder>) end;

JRegion_OpClass = interface(JEnumClass)
['{D3BC078B-CC50-4546-8ADD-DD1DA8DF5B7B}']
  {Property Methods}
  function _GetDIFFERENCE: JRegion_Op;
  function _GetINTERSECT: JRegion_Op;
  function _GetREPLACE: JRegion_Op;
  function _GetREVERSE_DIFFERENCE: JRegion_Op;
  function _GetUNION: JRegion_Op;
  function _GetXOR: JRegion_Op;
  {Methods}
  function valueOf(name: JString): JRegion_Op; cdecl;
  function values: TJavaObjectArray<JRegion_Op>; cdecl;
  {Properties}
  property DIFFERENCE: JRegion_Op read _GetDIFFERENCE;
  property INTERSECT: JRegion_Op read _GetINTERSECT;
  property REPLACE: JRegion_Op read _GetREPLACE;
  property REVERSE_DIFFERENCE: JRegion_Op read _GetREVERSE_DIFFERENCE;
  property UNION: JRegion_Op read _GetUNION;
  property &XOR: JRegion_Op read _GetXOR;
end;

[JavaSignature('android/graphics/Region$Op')]
JRegion_Op = interface(JEnum)
['{47159EE4-0384-467B-9F11-04DF089D9431}']
end;
TJRegion_Op = class(TJavaGenericImport<JRegion_OpClass, JRegion_Op>) end;

JColorClass = interface(JObjectClass)
['{4A2B6D4E-E3C4-405A-A41F-40F86D1B72E1}']
  {Property Methods}
  function _GetBLACK: Integer;
  function _GetBLUE: Integer;
  function _GetCYAN: Integer;
  function _GetDKGRAY: Integer;
  function _GetGRAY: Integer;
  function _GetGREEN: Integer;
  function _GetLTGRAY: Integer;
  function _GetMAGENTA: Integer;
  function _GetRED: Integer;
  function _GetTRANSPARENT: Integer;
  function _GetWHITE: Integer;
  function _GetYELLOW: Integer;
  {Methods}
  function init: JColor; cdecl;
  function HSVToColor(hsv: TJavaArray<Single>): Integer; cdecl; overload;
  function HSVToColor(alpha: Integer; hsv: TJavaArray<Single>): Integer; cdecl; overload;
  procedure RGBToHSV(red: Integer; green: Integer; blue: Integer; hsv: TJavaArray<Single>); cdecl;
  function alpha(color: Integer): Integer; cdecl;
  function argb(alpha: Integer; red: Integer; green: Integer; blue: Integer): Integer; cdecl;
  function blue(color: Integer): Integer; cdecl;
  procedure colorToHSV(color: Integer; hsv: TJavaArray<Single>); cdecl;
  function green(color: Integer): Integer; cdecl;
  function parseColor(colorString: JString): Integer; cdecl;
  function red(color: Integer): Integer; cdecl;
  function rgb(red: Integer; green: Integer; blue: Integer): Integer; cdecl;
  {Properties}
  property BLACK: Integer read _GetBLACK;
  property BLUE: Integer read _GetBLUE;
  property CYAN: Integer read _GetCYAN;
  property DKGRAY: Integer read _GetDKGRAY;
  property GRAY: Integer read _GetGRAY;
  property GREEN: Integer read _GetGREEN;
  property LTGRAY: Integer read _GetLTGRAY;
  property MAGENTA: Integer read _GetMAGENTA;
  property RED: Integer read _GetRED;
  property TRANSPARENT: Integer read _GetTRANSPARENT;
  property WHITE: Integer read _GetWHITE;
  property YELLOW: Integer read _GetYELLOW;
end;

[JavaSignature('android/graphics/Color')]
JColor = interface(JObject)
['{3CEB6F50-C9D3-458F-81EF-44FB35AB4DA6}']
end;
TJColor = class(TJavaGenericImport<JColorClass, JColor>) end;

JCanvasClass = interface(JObjectClass)
['{5C77F1D0-F7A1-4806-8697-EA2A86F66BC1}']
  {Property Methods}
  function _GetALL_SAVE_FLAG: Integer;
  function _GetCLIP_SAVE_FLAG: Integer;
  function _GetCLIP_TO_LAYER_SAVE_FLAG: Integer;
  function _GetFULL_COLOR_LAYER_SAVE_FLAG: Integer;
  function _GetHAS_ALPHA_LAYER_SAVE_FLAG: Integer;
  function _GetMATRIX_SAVE_FLAG: Integer;
  {Methods}
  function init: JCanvas; cdecl; overload;
  function init(bitmap: JBitmap): JCanvas; cdecl; overload;
  {Properties}
  property ALL_SAVE_FLAG: Integer read _GetALL_SAVE_FLAG;
  property CLIP_SAVE_FLAG: Integer read _GetCLIP_SAVE_FLAG;
  property CLIP_TO_LAYER_SAVE_FLAG: Integer read _GetCLIP_TO_LAYER_SAVE_FLAG;
  property FULL_COLOR_LAYER_SAVE_FLAG: Integer read _GetFULL_COLOR_LAYER_SAVE_FLAG;
  property HAS_ALPHA_LAYER_SAVE_FLAG: Integer read _GetHAS_ALPHA_LAYER_SAVE_FLAG;
  property MATRIX_SAVE_FLAG: Integer read _GetMATRIX_SAVE_FLAG;
end;

[JavaSignature('android/graphics/Canvas')]
JCanvas = interface(JObject)
['{00C36DBF-A6AE-4B19-9DC9-0FD6D71823BB}']
  {Methods}
  function clipPath(path: JPath; op: JRegion_Op): Boolean; cdecl; overload;
  function clipPath(path: JPath): Boolean; cdecl; overload;
  function clipRect(rect: JRectF; op: JRegion_Op): Boolean; cdecl; overload;
  function clipRect(rect: JRect; op: JRegion_Op): Boolean; cdecl; overload;
  function clipRect(rect: JRectF): Boolean; cdecl; overload;
  function clipRect(rect: JRect): Boolean; cdecl; overload;
  function clipRect(left: Single; top: Single; right: Single; bottom: Single; op: JRegion_Op): Boolean; cdecl; overload;
  function clipRect(left: Single; top: Single; right: Single; bottom: Single): Boolean; cdecl; overload;
  function clipRect(left: Integer; top: Integer; right: Integer; bottom: Integer): Boolean; cdecl; overload;
  function clipRegion(region: JRegion; op: JRegion_Op): Boolean; cdecl; overload;
  function clipRegion(region: JRegion): Boolean; cdecl; overload;
  procedure concat(matrix: JMatrix); cdecl;
  procedure drawARGB(a: Integer; r: Integer; g: Integer; b: Integer); cdecl;
  procedure drawArc(oval: JRectF; startAngle: Single; sweepAngle: Single; useCenter: Boolean; paint: JPaint); cdecl;
  procedure drawBitmap(bitmap: JBitmap; left: Single; top: Single; paint: JPaint); cdecl; overload;
  procedure drawBitmap(bitmap: JBitmap; src: JRect; dst: JRectF; paint: JPaint); cdecl; overload;
  procedure drawBitmap(bitmap: JBitmap; src: JRect; dst: JRect; paint: JPaint); cdecl; overload;
  procedure drawBitmap(colors: TJavaArray<Integer>; offset: Integer; stride: Integer; x: Single; y: Single; width: Integer; height: Integer; hasAlpha: Boolean; paint: JPaint); cdecl; overload;
  procedure drawBitmap(colors: TJavaArray<Integer>; offset: Integer; stride: Integer; x: Integer; y: Integer; width: Integer; height: Integer; hasAlpha: Boolean; paint: JPaint); cdecl; overload;
  procedure drawBitmap(bitmap: JBitmap; matrix: JMatrix; paint: JPaint); cdecl; overload;
  procedure drawBitmapMesh(bitmap: JBitmap; meshWidth: Integer; meshHeight: Integer; verts: TJavaArray<Single>; vertOffset: Integer; colors: TJavaArray<Integer>; colorOffset: Integer; paint: JPaint); cdecl;
  procedure drawCircle(cx: Single; cy: Single; radius: Single; paint: JPaint); cdecl;
  procedure drawColor(color: Integer); cdecl; overload;
  procedure drawColor(color: Integer; mode: JPorterDuff_Mode); cdecl; overload;
  procedure drawLine(startX: Single; startY: Single; stopX: Single; stopY: Single; paint: JPaint); cdecl;
  procedure drawLines(pts: TJavaArray<Single>; offset: Integer; count: Integer; paint: JPaint); cdecl; overload;
  procedure drawLines(pts: TJavaArray<Single>; paint: JPaint); cdecl; overload;
  procedure drawOval(oval: JRectF; paint: JPaint); cdecl;
  procedure drawPaint(paint: JPaint); cdecl;
  procedure drawPath(path: JPath; paint: JPaint); cdecl;
  procedure drawPicture(picture: JPicture); cdecl; overload;
  procedure drawPicture(picture: JPicture; dst: JRectF); cdecl; overload;
  procedure drawPicture(picture: JPicture; dst: JRect); cdecl; overload;
  procedure drawPoint(x: Single; y: Single; paint: JPaint); cdecl;
  procedure drawPoints(pts: TJavaArray<Single>; offset: Integer; count: Integer; paint: JPaint); cdecl; overload;
  procedure drawPoints(pts: TJavaArray<Single>; paint: JPaint); cdecl; overload;
  procedure drawPosText(text: TJavaArray<Char>; index: Integer; count: Integer; pos: TJavaArray<Single>; paint: JPaint); cdecl; overload;//Deprecated
  procedure drawPosText(text: JString; pos: TJavaArray<Single>; paint: JPaint); cdecl; overload;//Deprecated
  procedure drawRGB(r: Integer; g: Integer; b: Integer); cdecl;
  procedure drawRect(rect: JRectF; paint: JPaint); cdecl; overload;
  procedure drawRect(r: JRect; paint: JPaint); cdecl; overload;
  procedure drawRect(left: Single; top: Single; right: Single; bottom: Single; paint: JPaint); cdecl; overload;
  procedure drawRoundRect(rect: JRectF; rx: Single; ry: Single; paint: JPaint); cdecl;
  procedure drawText(text: TJavaArray<Char>; index: Integer; count: Integer; x: Single; y: Single; paint: JPaint); cdecl; overload;
  procedure drawText(text: JString; x: Single; y: Single; paint: JPaint); cdecl; overload;
  procedure drawText(text: JString; start: Integer; end_: Integer; x: Single; y: Single; paint: JPaint); cdecl; overload;
  procedure drawText(text: JCharSequence; start: Integer; end_: Integer; x: Single; y: Single; paint: JPaint); cdecl; overload;
  procedure drawTextOnPath(text: TJavaArray<Char>; index: Integer; count: Integer; path: JPath; hOffset: Single; vOffset: Single; paint: JPaint); cdecl; overload;
  procedure drawTextOnPath(text: JString; path: JPath; hOffset: Single; vOffset: Single; paint: JPaint); cdecl; overload;
  procedure drawVertices(mode: JCanvas_VertexMode; vertexCount: Integer; verts: TJavaArray<Single>; vertOffset: Integer; texs: TJavaArray<Single>; texOffset: Integer; colors: TJavaArray<Integer>; colorOffset: Integer; indices: TJavaArray<SmallInt>; indexOffset: Integer; indexCount: Integer; paint: JPaint); cdecl;
  function getClipBounds(bounds: JRect): Boolean; cdecl; overload;
  function getClipBounds: JRect; cdecl; overload;
  function getDensity: Integer; cdecl;
  function getDrawFilter: JDrawFilter; cdecl;
  function getHeight: Integer; cdecl;
  procedure getMatrix(ctm: JMatrix); cdecl; overload;//Deprecated
  function getMatrix: JMatrix; cdecl; overload;//Deprecated
  function getMaximumBitmapHeight: Integer; cdecl;
  function getMaximumBitmapWidth: Integer; cdecl;
  function getSaveCount: Integer; cdecl;
  function getWidth: Integer; cdecl;
  function isHardwareAccelerated: Boolean; cdecl;
  function isOpaque: Boolean; cdecl;
  function quickReject(rect: JRectF; type_: JCanvas_EdgeType): Boolean; cdecl; overload;
  function quickReject(path: JPath; type_: JCanvas_EdgeType): Boolean; cdecl; overload;
  function quickReject(left: Single; top: Single; right: Single; bottom: Single; type_: JCanvas_EdgeType): Boolean; cdecl; overload;
  procedure restore; cdecl;
  procedure restoreToCount(saveCount: Integer); cdecl;
  procedure rotate(degrees: Single); cdecl; overload;
  procedure rotate(degrees: Single; px: Single; py: Single); cdecl; overload;
  function save: Integer; cdecl; overload;
  function save(saveFlags: Integer): Integer; cdecl; overload;
  function saveLayer(bounds: JRectF; paint: JPaint; saveFlags: Integer): Integer; cdecl; overload;
  function saveLayer(left: Single; top: Single; right: Single; bottom: Single; paint: JPaint; saveFlags: Integer): Integer; cdecl; overload;
  function saveLayerAlpha(bounds: JRectF; alpha: Integer; saveFlags: Integer): Integer; cdecl; overload;
  function saveLayerAlpha(left: Single; top: Single; right: Single; bottom: Single; alpha: Integer; saveFlags: Integer): Integer; cdecl; overload;
  procedure scale(sx: Single; sy: Single); cdecl; overload;
  procedure scale(sx: Single; sy: Single; px: Single; py: Single); cdecl; overload;
  procedure setBitmap(bitmap: JBitmap); cdecl;
  procedure setDensity(density: Integer); cdecl;
  procedure setDrawFilter(filter: JDrawFilter); cdecl;
  procedure setMatrix(matrix: JMatrix); cdecl;
  procedure skew(sx: Single; sy: Single); cdecl;
  procedure translate(dx: Single; dy: Single); cdecl;
end;
TJCanvas = class(TJavaGenericImport<JCanvasClass, JCanvas>) end;

JMaskFilterClass = interface(JObjectClass)
['{47DA8E8A-21AC-4DAC-86A8-8908EC5560F9}']
  {Methods}
  function init: JMaskFilter; cdecl;
end;

[JavaSignature('android/graphics/MaskFilter')]
JMaskFilter = interface(JObject)
['{6DA8DB66-410C-461F-A0D8-BE2D76E78873}']
end;
TJMaskFilter = class(TJavaGenericImport<JMaskFilterClass, JMaskFilter>) end;

JEmbossMaskFilterClass = interface(JMaskFilterClass)
['{1C7816EE-0FC7-496C-9FE4-4DEB9C813FE1}']
  {Methods}
  function init(direction: TJavaArray<Single>; ambient: Single; specular: Single; blurRadius: Single): JEmbossMaskFilter; cdecl;
end;

[JavaSignature('android/graphics/EmbossMaskFilter')]
JEmbossMaskFilter = interface(JMaskFilter)
['{B9273A69-52FC-46FF-89F3-6A1C0FEEE5DA}']
end;
TJEmbossMaskFilter = class(TJavaGenericImport<JEmbossMaskFilterClass, JEmbossMaskFilter>) end;

JCornerPathEffectClass = interface(JPathEffectClass)
['{1B66DCB0-AD2E-46F3-B181-50AF35B7E86A}']
  {Methods}
  function init(radius: Single): JCornerPathEffect; cdecl;
end;

[JavaSignature('android/graphics/CornerPathEffect')]
JCornerPathEffect = interface(JPathEffect)
['{E513FB33-ADA1-48CF-BB5B-58D047DDAA79}']
end;
TJCornerPathEffect = class(TJavaGenericImport<JCornerPathEffectClass, JCornerPathEffect>) end;

JGradientDrawable_OrientationClass = interface(JEnumClass)
['{F2C32A94-FD6D-4247-861B-4D895928E1AE}']
  {Property Methods}
  function _GetBL_TR: JGradientDrawable_Orientation;
  function _GetBOTTOM_TOP: JGradientDrawable_Orientation;
  function _GetBR_TL: JGradientDrawable_Orientation;
  function _GetLEFT_RIGHT: JGradientDrawable_Orientation;
  function _GetRIGHT_LEFT: JGradientDrawable_Orientation;
  function _GetTL_BR: JGradientDrawable_Orientation;
  function _GetTOP_BOTTOM: JGradientDrawable_Orientation;
  function _GetTR_BL: JGradientDrawable_Orientation;
  {Methods}
  function valueOf(name: JString): JGradientDrawable_Orientation; cdecl;
  function values: TJavaObjectArray<JGradientDrawable_Orientation>; cdecl;
  {Properties}
  property BL_TR: JGradientDrawable_Orientation read _GetBL_TR;
  property BOTTOM_TOP: JGradientDrawable_Orientation read _GetBOTTOM_TOP;
  property BR_TL: JGradientDrawable_Orientation read _GetBR_TL;
  property LEFT_RIGHT: JGradientDrawable_Orientation read _GetLEFT_RIGHT;
  property RIGHT_LEFT: JGradientDrawable_Orientation read _GetRIGHT_LEFT;
  property TL_BR: JGradientDrawable_Orientation read _GetTL_BR;
  property TOP_BOTTOM: JGradientDrawable_Orientation read _GetTOP_BOTTOM;
  property TR_BL: JGradientDrawable_Orientation read _GetTR_BL;
end;

[JavaSignature('android/graphics/drawable/GradientDrawable$Orientation')]
JGradientDrawable_Orientation = interface(JEnum)
['{7DC952FB-E023-4DDB-BB16-88E784289B12}']
end;
TJGradientDrawable_Orientation = class(TJavaGenericImport<JGradientDrawable_OrientationClass, JGradientDrawable_Orientation>) end;

JPorterDuff_ModeClass = interface(JEnumClass)
['{AF28263D-7D2E-4F7D-8E23-7DBACB480EB5}']
  {Property Methods}
  function _GetADD: JPorterDuff_Mode;
  function _GetCLEAR: JPorterDuff_Mode;
  function _GetDARKEN: JPorterDuff_Mode;
  function _GetDST: JPorterDuff_Mode;
  function _GetDST_ATOP: JPorterDuff_Mode;
  function _GetDST_IN: JPorterDuff_Mode;
  function _GetDST_OUT: JPorterDuff_Mode;
  function _GetDST_OVER: JPorterDuff_Mode;
  function _GetLIGHTEN: JPorterDuff_Mode;
  function _GetMULTIPLY: JPorterDuff_Mode;
  function _GetOVERLAY: JPorterDuff_Mode;
  function _GetSCREEN: JPorterDuff_Mode;
  function _GetSRC: JPorterDuff_Mode;
  function _GetSRC_ATOP: JPorterDuff_Mode;
  function _GetSRC_IN: JPorterDuff_Mode;
  function _GetSRC_OUT: JPorterDuff_Mode;
  function _GetSRC_OVER: JPorterDuff_Mode;
  function _GetXOR: JPorterDuff_Mode;
  {Methods}
  function valueOf(name: JString): JPorterDuff_Mode; cdecl;
  function values: TJavaObjectArray<JPorterDuff_Mode>; cdecl;
  {Properties}
  property ADD: JPorterDuff_Mode read _GetADD;
  property CLEAR: JPorterDuff_Mode read _GetCLEAR;
  property DARKEN: JPorterDuff_Mode read _GetDARKEN;
  property DST: JPorterDuff_Mode read _GetDST;
  property DST_ATOP: JPorterDuff_Mode read _GetDST_ATOP;
  property DST_IN: JPorterDuff_Mode read _GetDST_IN;
  property DST_OUT: JPorterDuff_Mode read _GetDST_OUT;
  property DST_OVER: JPorterDuff_Mode read _GetDST_OVER;
  property LIGHTEN: JPorterDuff_Mode read _GetLIGHTEN;
  property MULTIPLY: JPorterDuff_Mode read _GetMULTIPLY;
  property OVERLAY: JPorterDuff_Mode read _GetOVERLAY;
  property SCREEN: JPorterDuff_Mode read _GetSCREEN;
  property SRC: JPorterDuff_Mode read _GetSRC;
  property SRC_ATOP: JPorterDuff_Mode read _GetSRC_ATOP;
  property SRC_IN: JPorterDuff_Mode read _GetSRC_IN;
  property SRC_OUT: JPorterDuff_Mode read _GetSRC_OUT;
  property SRC_OVER: JPorterDuff_Mode read _GetSRC_OVER;
  property &XOR: JPorterDuff_Mode read _GetXOR;
end;

[JavaSignature('android/graphics/PorterDuff$Mode')]
JPorterDuff_Mode = interface(JEnum)
['{BB12CE91-1F0C-4B3B-9910-ACF2BDB39B4A}']
end;
TJPorterDuff_Mode = class(TJavaGenericImport<JPorterDuff_ModeClass, JPorterDuff_Mode>) end;

JConfigurationClass = interface(JObjectClass)
['{ED192161-6D1B-44E2-9CD0-4688E2EE8B4B}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  function _GetDENSITY_DPI_UNDEFINED: Integer;
  function _GetHARDKEYBOARDHIDDEN_NO: Integer;
  function _GetHARDKEYBOARDHIDDEN_UNDEFINED: Integer;
  function _GetHARDKEYBOARDHIDDEN_YES: Integer;
  function _GetKEYBOARDHIDDEN_NO: Integer;
  function _GetKEYBOARDHIDDEN_UNDEFINED: Integer;
  function _GetKEYBOARDHIDDEN_YES: Integer;
  function _GetKEYBOARD_12KEY: Integer;
  function _GetKEYBOARD_NOKEYS: Integer;
  function _GetKEYBOARD_QWERTY: Integer;
  function _GetKEYBOARD_UNDEFINED: Integer;
  function _GetNAVIGATIONHIDDEN_NO: Integer;
  function _GetNAVIGATIONHIDDEN_UNDEFINED: Integer;
  function _GetNAVIGATIONHIDDEN_YES: Integer;
  function _GetNAVIGATION_DPAD: Integer;
  function _GetNAVIGATION_NONAV: Integer;
  function _GetNAVIGATION_TRACKBALL: Integer;
  function _GetNAVIGATION_UNDEFINED: Integer;
  function _GetNAVIGATION_WHEEL: Integer;
  function _GetORIENTATION_LANDSCAPE: Integer;
  function _GetORIENTATION_PORTRAIT: Integer;
  function _GetORIENTATION_SQUARE: Integer;
  function _GetORIENTATION_UNDEFINED: Integer;
  function _GetSCREENLAYOUT_LAYOUTDIR_LTR: Integer;
  function _GetSCREENLAYOUT_LAYOUTDIR_MASK: Integer;
  function _GetSCREENLAYOUT_LAYOUTDIR_RTL: Integer;
  function _GetSCREENLAYOUT_LAYOUTDIR_SHIFT: Integer;
  function _GetSCREENLAYOUT_LAYOUTDIR_UNDEFINED: Integer;
  function _GetSCREENLAYOUT_LONG_MASK: Integer;
  function _GetSCREENLAYOUT_LONG_NO: Integer;
  function _GetSCREENLAYOUT_LONG_UNDEFINED: Integer;
  function _GetSCREENLAYOUT_LONG_YES: Integer;
  function _GetSCREENLAYOUT_SIZE_LARGE: Integer;
  function _GetSCREENLAYOUT_SIZE_MASK: Integer;
  function _GetSCREENLAYOUT_SIZE_NORMAL: Integer;
  function _GetSCREENLAYOUT_SIZE_SMALL: Integer;
  function _GetSCREENLAYOUT_SIZE_UNDEFINED: Integer;
  function _GetSCREENLAYOUT_SIZE_XLARGE: Integer;
  function _GetSCREENLAYOUT_UNDEFINED: Integer;
  function _GetSCREEN_HEIGHT_DP_UNDEFINED: Integer;
  function _GetSCREEN_WIDTH_DP_UNDEFINED: Integer;
  function _GetSMALLEST_SCREEN_WIDTH_DP_UNDEFINED: Integer;
  function _GetTOUCHSCREEN_FINGER: Integer;
  function _GetTOUCHSCREEN_NOTOUCH: Integer;
  function _GetTOUCHSCREEN_STYLUS: Integer;
  function _GetTOUCHSCREEN_UNDEFINED: Integer;
  function _GetUI_MODE_NIGHT_MASK: Integer;
  function _GetUI_MODE_NIGHT_NO: Integer;
  function _GetUI_MODE_NIGHT_UNDEFINED: Integer;
  function _GetUI_MODE_NIGHT_YES: Integer;
  function _GetUI_MODE_TYPE_APPLIANCE: Integer;
  function _GetUI_MODE_TYPE_CAR: Integer;
  function _GetUI_MODE_TYPE_DESK: Integer;
  function _GetUI_MODE_TYPE_MASK: Integer;
  function _GetUI_MODE_TYPE_NORMAL: Integer;
  function _GetUI_MODE_TYPE_TELEVISION: Integer;
  function _GetUI_MODE_TYPE_UNDEFINED: Integer;
  {Methods}
  function init: JConfiguration; cdecl; overload;
  function init(o: JConfiguration): JConfiguration; cdecl; overload;
  function needNewResources(configChanges: Integer; interestingChanges: Integer): Boolean; cdecl;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
  property DENSITY_DPI_UNDEFINED: Integer read _GetDENSITY_DPI_UNDEFINED;
  property HARDKEYBOARDHIDDEN_NO: Integer read _GetHARDKEYBOARDHIDDEN_NO;
  property HARDKEYBOARDHIDDEN_UNDEFINED: Integer read _GetHARDKEYBOARDHIDDEN_UNDEFINED;
  property HARDKEYBOARDHIDDEN_YES: Integer read _GetHARDKEYBOARDHIDDEN_YES;
  property KEYBOARDHIDDEN_NO: Integer read _GetKEYBOARDHIDDEN_NO;
  property KEYBOARDHIDDEN_UNDEFINED: Integer read _GetKEYBOARDHIDDEN_UNDEFINED;
  property KEYBOARDHIDDEN_YES: Integer read _GetKEYBOARDHIDDEN_YES;
  property KEYBOARD_12KEY: Integer read _GetKEYBOARD_12KEY;
  property KEYBOARD_NOKEYS: Integer read _GetKEYBOARD_NOKEYS;
  property KEYBOARD_QWERTY: Integer read _GetKEYBOARD_QWERTY;
  property KEYBOARD_UNDEFINED: Integer read _GetKEYBOARD_UNDEFINED;
  property NAVIGATIONHIDDEN_NO: Integer read _GetNAVIGATIONHIDDEN_NO;
  property NAVIGATIONHIDDEN_UNDEFINED: Integer read _GetNAVIGATIONHIDDEN_UNDEFINED;
  property NAVIGATIONHIDDEN_YES: Integer read _GetNAVIGATIONHIDDEN_YES;
  property NAVIGATION_DPAD: Integer read _GetNAVIGATION_DPAD;
  property NAVIGATION_NONAV: Integer read _GetNAVIGATION_NONAV;
  property NAVIGATION_TRACKBALL: Integer read _GetNAVIGATION_TRACKBALL;
  property NAVIGATION_UNDEFINED: Integer read _GetNAVIGATION_UNDEFINED;
  property NAVIGATION_WHEEL: Integer read _GetNAVIGATION_WHEEL;
  property ORIENTATION_LANDSCAPE: Integer read _GetORIENTATION_LANDSCAPE;
  property ORIENTATION_PORTRAIT: Integer read _GetORIENTATION_PORTRAIT;
  property ORIENTATION_SQUARE: Integer read _GetORIENTATION_SQUARE;
  property ORIENTATION_UNDEFINED: Integer read _GetORIENTATION_UNDEFINED;
  property SCREENLAYOUT_LAYOUTDIR_LTR: Integer read _GetSCREENLAYOUT_LAYOUTDIR_LTR;
  property SCREENLAYOUT_LAYOUTDIR_MASK: Integer read _GetSCREENLAYOUT_LAYOUTDIR_MASK;
  property SCREENLAYOUT_LAYOUTDIR_RTL: Integer read _GetSCREENLAYOUT_LAYOUTDIR_RTL;
  property SCREENLAYOUT_LAYOUTDIR_SHIFT: Integer read _GetSCREENLAYOUT_LAYOUTDIR_SHIFT;
  property SCREENLAYOUT_LAYOUTDIR_UNDEFINED: Integer read _GetSCREENLAYOUT_LAYOUTDIR_UNDEFINED;
  property SCREENLAYOUT_LONG_MASK: Integer read _GetSCREENLAYOUT_LONG_MASK;
  property SCREENLAYOUT_LONG_NO: Integer read _GetSCREENLAYOUT_LONG_NO;
  property SCREENLAYOUT_LONG_UNDEFINED: Integer read _GetSCREENLAYOUT_LONG_UNDEFINED;
  property SCREENLAYOUT_LONG_YES: Integer read _GetSCREENLAYOUT_LONG_YES;
  property SCREENLAYOUT_SIZE_LARGE: Integer read _GetSCREENLAYOUT_SIZE_LARGE;
  property SCREENLAYOUT_SIZE_MASK: Integer read _GetSCREENLAYOUT_SIZE_MASK;
  property SCREENLAYOUT_SIZE_NORMAL: Integer read _GetSCREENLAYOUT_SIZE_NORMAL;
  property SCREENLAYOUT_SIZE_SMALL: Integer read _GetSCREENLAYOUT_SIZE_SMALL;
  property SCREENLAYOUT_SIZE_UNDEFINED: Integer read _GetSCREENLAYOUT_SIZE_UNDEFINED;
  property SCREENLAYOUT_SIZE_XLARGE: Integer read _GetSCREENLAYOUT_SIZE_XLARGE;
  property SCREENLAYOUT_UNDEFINED: Integer read _GetSCREENLAYOUT_UNDEFINED;
  property SCREEN_HEIGHT_DP_UNDEFINED: Integer read _GetSCREEN_HEIGHT_DP_UNDEFINED;
  property SCREEN_WIDTH_DP_UNDEFINED: Integer read _GetSCREEN_WIDTH_DP_UNDEFINED;
  property SMALLEST_SCREEN_WIDTH_DP_UNDEFINED: Integer read _GetSMALLEST_SCREEN_WIDTH_DP_UNDEFINED;
  property TOUCHSCREEN_FINGER: Integer read _GetTOUCHSCREEN_FINGER;
  property TOUCHSCREEN_NOTOUCH: Integer read _GetTOUCHSCREEN_NOTOUCH;
  property TOUCHSCREEN_STYLUS: Integer read _GetTOUCHSCREEN_STYLUS;
  property TOUCHSCREEN_UNDEFINED: Integer read _GetTOUCHSCREEN_UNDEFINED;
  property UI_MODE_NIGHT_MASK: Integer read _GetUI_MODE_NIGHT_MASK;
  property UI_MODE_NIGHT_NO: Integer read _GetUI_MODE_NIGHT_NO;
  property UI_MODE_NIGHT_UNDEFINED: Integer read _GetUI_MODE_NIGHT_UNDEFINED;
  property UI_MODE_NIGHT_YES: Integer read _GetUI_MODE_NIGHT_YES;
  property UI_MODE_TYPE_APPLIANCE: Integer read _GetUI_MODE_TYPE_APPLIANCE;
  property UI_MODE_TYPE_CAR: Integer read _GetUI_MODE_TYPE_CAR;
  property UI_MODE_TYPE_DESK: Integer read _GetUI_MODE_TYPE_DESK;
  property UI_MODE_TYPE_MASK: Integer read _GetUI_MODE_TYPE_MASK;
  property UI_MODE_TYPE_NORMAL: Integer read _GetUI_MODE_TYPE_NORMAL;
  property UI_MODE_TYPE_TELEVISION: Integer read _GetUI_MODE_TYPE_TELEVISION;
  property UI_MODE_TYPE_UNDEFINED: Integer read _GetUI_MODE_TYPE_UNDEFINED;
end;

[JavaSignature('android/content/res/Configuration')]
JConfiguration = interface(JObject)
['{1D50A729-D1DD-4424-819B-A2106CC761DB}']
  {Property Methods}
  function _GetdensityDpi: Integer;
  procedure _SetdensityDpi(Value: Integer);
  function _GetfontScale: Single;
  procedure _SetfontScale(Value: Single);
  function _GethardKeyboardHidden: Integer;
  procedure _SethardKeyboardHidden(Value: Integer);
  function _Getkeyboard: Integer;
  procedure _Setkeyboard(Value: Integer);
  function _GetkeyboardHidden: Integer;
  procedure _SetkeyboardHidden(Value: Integer);
  function _Getlocale: JLocale;
  procedure _Setlocale(Value: JLocale);
  function _Getmcc: Integer;
  procedure _Setmcc(Value: Integer);
  function _Getmnc: Integer;
  procedure _Setmnc(Value: Integer);
  function _Getnavigation: Integer;
  procedure _Setnavigation(Value: Integer);
  function _GetnavigationHidden: Integer;
  procedure _SetnavigationHidden(Value: Integer);
  function _Getorientation: Integer;
  procedure _Setorientation(Value: Integer);
  function _GetscreenHeightDp: Integer;
  procedure _SetscreenHeightDp(Value: Integer);
  function _GetscreenLayout: Integer;
  procedure _SetscreenLayout(Value: Integer);
  function _GetscreenWidthDp: Integer;
  procedure _SetscreenWidthDp(Value: Integer);
  function _GetsmallestScreenWidthDp: Integer;
  procedure _SetsmallestScreenWidthDp(Value: Integer);
  function _Gettouchscreen: Integer;
  procedure _Settouchscreen(Value: Integer);
  function _GetuiMode: Integer;
  procedure _SetuiMode(Value: Integer);
  {Methods}
  function compareTo(that: JConfiguration): Integer; cdecl;
  function describeContents: Integer; cdecl;
  function diff(delta: JConfiguration): Integer; cdecl;
  function equals(that: JConfiguration): Boolean; cdecl; overload;
  function equals(that: JObject): Boolean; cdecl; overload;
  function getLayoutDirection: Integer; cdecl;
  function hashCode: Integer; cdecl;
  function isLayoutSizeAtLeast(size: Integer): Boolean; cdecl;
  procedure readFromParcel(source: JParcel); cdecl;
  procedure setLayoutDirection(locale: JLocale); cdecl;
  procedure setLocale(loc: JLocale); cdecl;
  procedure setTo(o: JConfiguration); cdecl;
  procedure setToDefaults; cdecl;
  function toString: JString; cdecl;
  function updateFrom(delta: JConfiguration): Integer; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  {Properties}
  property densityDpi: Integer read _GetdensityDpi write _SetdensityDpi;
  property fontScale: Single read _GetfontScale write _SetfontScale;
  property hardKeyboardHidden: Integer read _GethardKeyboardHidden write _SethardKeyboardHidden;
  property keyboard: Integer read _Getkeyboard write _Setkeyboard;
  property keyboardHidden: Integer read _GetkeyboardHidden write _SetkeyboardHidden;
  property locale: JLocale read _Getlocale write _Setlocale;
  property mcc: Integer read _Getmcc write _Setmcc;
  property mnc: Integer read _Getmnc write _Setmnc;
  property navigation: Integer read _Getnavigation write _Setnavigation;
  property navigationHidden: Integer read _GetnavigationHidden write _SetnavigationHidden;
  property orientation: Integer read _Getorientation write _Setorientation;
  property screenHeightDp: Integer read _GetscreenHeightDp write _SetscreenHeightDp;
  property screenLayout: Integer read _GetscreenLayout write _SetscreenLayout;
  property screenWidthDp: Integer read _GetscreenWidthDp write _SetscreenWidthDp;
  property smallestScreenWidthDp: Integer read _GetsmallestScreenWidthDp write _SetsmallestScreenWidthDp;
  property touchscreen: Integer read _Gettouchscreen write _Settouchscreen;
  property uiMode: Integer read _GetuiMode write _SetuiMode;
end;
TJConfiguration = class(TJavaGenericImport<JConfigurationClass, JConfiguration>) end;

JIntentSenderClass = interface(JObjectClass)
['{E26CE668-2908-47E1-A8B2-996052142D18}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Methods}
  function readIntentSenderOrNullFromParcel(in_: JParcel): JIntentSender; cdecl;
  procedure writeIntentSenderOrNullToParcel(sender: JIntentSender; out_: JParcel); cdecl;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/content/IntentSender')]
JIntentSender = interface(JObject)
['{3CE39732-EC8A-45C4-BABD-15C936378193}']
  {Methods}
  function describeContents: Integer; cdecl;
  function equals(otherObj: JObject): Boolean; cdecl;
  function getCreatorPackage: JString; cdecl;
  function getCreatorUid: Integer; cdecl;
  function getCreatorUserHandle: JUserHandle; cdecl;
  function getTargetPackage: JString; cdecl;//Deprecated
  function hashCode: Integer; cdecl;
  procedure sendIntent(context: JContext; code: Integer; intent: JIntent; onFinished: JIntentSender_OnFinished; handler: JHandler); cdecl; overload;
  procedure sendIntent(context: JContext; code: Integer; intent: JIntent; onFinished: JIntentSender_OnFinished; handler: JHandler; requiredPermission: JString); cdecl; overload;
  function toString: JString; cdecl;
  procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
end;
TJIntentSender = class(TJavaGenericImport<JIntentSenderClass, JIntentSender>) end;

JIntent_FilterComparisonClass = interface(JObjectClass)
['{F237F4D9-ADB3-4DF5-A8C6-7B6564FF6AE6}']
  {Methods}
  function init(intent: JIntent): JIntent_FilterComparison; cdecl;
end;

[JavaSignature('android/content/Intent$FilterComparison')]
JIntent_FilterComparison = interface(JObject)
['{C4464353-23E9-45E9-969E-2235202DE321}']
  {Methods}
  function equals(obj: JObject): Boolean; cdecl;
  function getIntent: JIntent; cdecl;
  function hashCode: Integer; cdecl;
end;
TJIntent_FilterComparison = class(TJavaGenericImport<JIntent_FilterComparisonClass, JIntent_FilterComparison>) end;

JBitmap_CompressFormatClass = interface(JEnumClass)
['{6F46CFB7-31A8-480F-9D88-958A1E1B9FF3}']
  {Property Methods}
  function _GetJPEG: JBitmap_CompressFormat;
  function _GetPNG: JBitmap_CompressFormat;
  function _GetWEBP: JBitmap_CompressFormat;
  {Methods}
  function valueOf(name: JString): JBitmap_CompressFormat; cdecl;
  function values: TJavaObjectArray<JBitmap_CompressFormat>; cdecl;
  {Properties}
  property JPEG: JBitmap_CompressFormat read _GetJPEG;
  property PNG: JBitmap_CompressFormat read _GetPNG;
  property WEBP: JBitmap_CompressFormat read _GetWEBP;
end;

[JavaSignature('android/graphics/Bitmap$CompressFormat')]
JBitmap_CompressFormat = interface(JEnum)
['{80D19EC8-AA8B-47D4-BF4A-05E5DC4C5E09}']
end;
TJBitmap_CompressFormat = class(TJavaGenericImport<JBitmap_CompressFormatClass, JBitmap_CompressFormat>) end;

JPathMeasureClass = interface(JObjectClass)
['{405F5AC9-D234-4E67-A4FB-70917551A76C}']
  {Property Methods}
  function _GetPOSITION_MATRIX_FLAG: Integer;
  function _GetTANGENT_MATRIX_FLAG: Integer;
  {Methods}
  function init: JPathMeasure; cdecl; overload;
  function init(path: JPath; forceClosed: Boolean): JPathMeasure; cdecl; overload;
  {Properties}
  property POSITION_MATRIX_FLAG: Integer read _GetPOSITION_MATRIX_FLAG;
  property TANGENT_MATRIX_FLAG: Integer read _GetTANGENT_MATRIX_FLAG;
end;

[JavaSignature('android/graphics/PathMeasure')]
JPathMeasure = interface(JObject)
['{87A613BB-686A-44EB-BB91-D4D92576384A}']
  {Methods}
  function getLength: Single; cdecl;
  function getMatrix(distance: Single; matrix: JMatrix; flags: Integer): Boolean; cdecl;
  function getPosTan(distance: Single; pos: TJavaArray<Single>; tan: TJavaArray<Single>): Boolean; cdecl;
  function getSegment(startD: Single; stopD: Single; dst: JPath; startWithMoveTo: Boolean): Boolean; cdecl;
  function isClosed: Boolean; cdecl;
  function nextContour: Boolean; cdecl;
  procedure setPath(path: JPath; forceClosed: Boolean); cdecl;
end;
TJPathMeasure = class(TJavaGenericImport<JPathMeasureClass, JPathMeasure>) end;

JTypedArrayClass = interface(JObjectClass)
['{9AA420A7-6C38-40EA-876D-DA87109DA825}']
end;

[JavaSignature('android/content/res/TypedArray')]
JTypedArray = interface(JObject)
['{CD776189-F421-46B2-BAE5-A66A72F034F7}']
  {Methods}
  function getBoolean(index: Integer; defValue: Boolean): Boolean; cdecl;
  function getColor(index: Integer; defValue: Integer): Integer; cdecl;
  function getColorStateList(index: Integer): JColorStateList; cdecl;
  function getDimension(index: Integer; defValue: Single): Single; cdecl;
  function getDimensionPixelOffset(index: Integer; defValue: Integer): Integer; cdecl;
  function getDimensionPixelSize(index: Integer; defValue: Integer): Integer; cdecl;
  function getDrawable(index: Integer): JDrawable; cdecl;
  function getFloat(index: Integer; defValue: Single): Single; cdecl;
  function getFraction(index: Integer; base: Integer; pbase: Integer; defValue: Single): Single; cdecl;
  function getIndex(at: Integer): Integer; cdecl;
  function getIndexCount: Integer; cdecl;
  function getInt(index: Integer; defValue: Integer): Integer; cdecl;
  function getInteger(index: Integer; defValue: Integer): Integer; cdecl;
  function getLayoutDimension(index: Integer; name: JString): Integer; cdecl; overload;
  function getLayoutDimension(index: Integer; defValue: Integer): Integer; cdecl; overload;
  function getNonResourceString(index: Integer): JString; cdecl;
  function getPositionDescription: JString; cdecl;
  function getResourceId(index: Integer; defValue: Integer): Integer; cdecl;
  function getResources: JResources; cdecl;
  function getString(index: Integer): JString; cdecl;
  function getText(index: Integer): JCharSequence; cdecl;
  function getTextArray(index: Integer): TJavaObjectArray<JCharSequence>; cdecl;
  function getValue(index: Integer; outValue: JTypedValue): Boolean; cdecl;
  function hasValue(index: Integer): Boolean; cdecl;
  function length: Integer; cdecl;
  function peekValue(index: Integer): JTypedValue; cdecl;
  procedure recycle; cdecl;
  function toString: JString; cdecl;
end;
TJTypedArray = class(TJavaGenericImport<JTypedArrayClass, JTypedArray>) end;

JMotionEvent_PointerPropertiesClass = interface(JObjectClass)
['{3885DB03-50CF-4125-B949-90B9AC8D261E}']
  {Methods}
  function init: JMotionEvent_PointerProperties; cdecl; overload;
  function init(other: JMotionEvent_PointerProperties): JMotionEvent_PointerProperties; cdecl; overload;
end;

[JavaSignature('android/view/MotionEvent$PointerProperties')]
JMotionEvent_PointerProperties = interface(JObject)
['{BB01C136-9C2A-4E2E-B223-979E167FAA79}']
  {Property Methods}
  function _Getid: Integer;
  procedure _Setid(Value: Integer);
  function _GettoolType: Integer;
  procedure _SettoolType(Value: Integer);
  {Methods}
  procedure clear; cdecl;
  procedure copyFrom(other: JMotionEvent_PointerProperties); cdecl;
  function equals(other: JObject): Boolean; cdecl;
  function hashCode: Integer; cdecl;
  {Properties}
  property id: Integer read _Getid write _Setid;
  property toolType: Integer read _GettoolType write _SettoolType;
end;
TJMotionEvent_PointerProperties = class(TJavaGenericImport<JMotionEvent_PointerPropertiesClass, JMotionEvent_PointerProperties>) end;

JInputMethodInfoClass = interface(JObjectClass)
['{FBCAFC39-0F74-488C-9502-0C83FB8860CE}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Methods}
  function init(context: JContext; service: JResolveInfo): JInputMethodInfo; cdecl; overload;
  function init(packageName: JString; className: JString; label_: JCharSequence; settingsActivity: JString): JInputMethodInfo; cdecl; overload;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/view/inputmethod/InputMethodInfo')]
JInputMethodInfo = interface(JObject)
['{F8FE6EFB-8F51-4BB7-B76F-E0F8BFF93CE6}']
  {Methods}
  function describeContents: Integer; cdecl;
  function equals(o: JObject): Boolean; cdecl;
  function getComponent: JComponentName; cdecl;
  function getId: JString; cdecl;
  function getIsDefaultResourceId: Integer; cdecl;
  function getPackageName: JString; cdecl;
  function getServiceInfo: JServiceInfo; cdecl;
  function getServiceName: JString; cdecl;
  function getSettingsActivity: JString; cdecl;
  function getSubtypeAt(index: Integer): JInputMethodSubtype; cdecl;
  function getSubtypeCount: Integer; cdecl;
  function hashCode: Integer; cdecl;
  function loadIcon(pm: JPackageManager): JDrawable; cdecl;
  function loadLabel(pm: JPackageManager): JCharSequence; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
end;
TJInputMethodInfo = class(TJavaGenericImport<JInputMethodInfoClass, JInputMethodInfo>) end;

JKeyCharacterMapClass = interface(JObjectClass)
['{76935FF1-DC3D-4EB8-84A2-45875FA8F3C3}']
  {Property Methods}
  function _GetALPHA: Integer;
  function _GetBUILT_IN_KEYBOARD: Integer;
  function _GetCOMBINING_ACCENT: Integer;
  function _GetCOMBINING_ACCENT_MASK: Integer;
  function _GetCREATOR: JParcelable_Creator;
  function _GetFULL: Integer;
  function _GetHEX_INPUT: Char;
  function _GetMODIFIER_BEHAVIOR_CHORDED: Integer;
  function _GetMODIFIER_BEHAVIOR_CHORDED_OR_TOGGLED: Integer;
  function _GetNUMERIC: Integer;
  function _GetPICKER_DIALOG_INPUT: Char;
  function _GetPREDICTIVE: Integer;
  function _GetSPECIAL_FUNCTION: Integer;
  function _GetVIRTUAL_KEYBOARD: Integer;
  {Methods}
  function deviceHasKey(keyCode: Integer): Boolean; cdecl;
  function deviceHasKeys(keyCodes: TJavaArray<Integer>): TJavaArray<Boolean>; cdecl;
  function getDeadChar(accent: Integer; c: Integer): Integer; cdecl;
  function load(deviceId: Integer): JKeyCharacterMap; cdecl;
  {Properties}
  property ALPHA: Integer read _GetALPHA;
  property BUILT_IN_KEYBOARD: Integer read _GetBUILT_IN_KEYBOARD;
  property COMBINING_ACCENT: Integer read _GetCOMBINING_ACCENT;
  property COMBINING_ACCENT_MASK: Integer read _GetCOMBINING_ACCENT_MASK;
  property CREATOR: JParcelable_Creator read _GetCREATOR;
  property FULL: Integer read _GetFULL;
  property HEX_INPUT: Char read _GetHEX_INPUT;
  property MODIFIER_BEHAVIOR_CHORDED: Integer read _GetMODIFIER_BEHAVIOR_CHORDED;
  property MODIFIER_BEHAVIOR_CHORDED_OR_TOGGLED: Integer read _GetMODIFIER_BEHAVIOR_CHORDED_OR_TOGGLED;
  property NUMERIC: Integer read _GetNUMERIC;
  property PICKER_DIALOG_INPUT: Char read _GetPICKER_DIALOG_INPUT;
  property PREDICTIVE: Integer read _GetPREDICTIVE;
  property SPECIAL_FUNCTION: Integer read _GetSPECIAL_FUNCTION;
  property VIRTUAL_KEYBOARD: Integer read _GetVIRTUAL_KEYBOARD;
end;

[JavaSignature('android/view/KeyCharacterMap')]
JKeyCharacterMap = interface(JObject)
['{9C009DB9-CC5F-400E-8930-72BB0811F110}']
  {Methods}
  function describeContents: Integer; cdecl;
  function &get(keyCode: Integer; metaState: Integer): Integer; cdecl;
  function getDisplayLabel(keyCode: Integer): Char; cdecl;
  function getEvents(chars: TJavaArray<Char>): TJavaObjectArray<JKeyEvent>; cdecl;
  function getKeyData(keyCode: Integer; results: JKeyCharacterMap_KeyData): Boolean; cdecl;//Deprecated
  function getKeyboardType: Integer; cdecl;
  function getMatch(keyCode: Integer; chars: TJavaArray<Char>): Char; cdecl; overload;
  function getMatch(keyCode: Integer; chars: TJavaArray<Char>; metaState: Integer): Char; cdecl; overload;
  function getModifierBehavior: Integer; cdecl;
  function getNumber(keyCode: Integer): Char; cdecl;
  function isPrintingKey(keyCode: Integer): Boolean; cdecl;
  procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
end;
TJKeyCharacterMap = class(TJavaGenericImport<JKeyCharacterMapClass, JKeyCharacterMap>) end;

JViewGroup_OnHierarchyChangeListenerClass = interface(IJavaClass)
['{D41768BD-020D-4CDE-B0EC-D21B49128F1E}']
end;

[JavaSignature('android/view/ViewGroup$OnHierarchyChangeListener')]
JViewGroup_OnHierarchyChangeListener = interface(IJavaInstance)
['{0844FBF9-D751-4DB6-994B-FF5A30264A07}']
  {Methods}
  procedure onChildViewAdded(parent: JView; child: JView); cdecl;
  procedure onChildViewRemoved(parent: JView; child: JView); cdecl;
end;
TJViewGroup_OnHierarchyChangeListener = class(TJavaGenericImport<JViewGroup_OnHierarchyChangeListenerClass, JViewGroup_OnHierarchyChangeListener>) end;

JMenuItem_OnActionExpandListenerClass = interface(IJavaClass)
['{5F3450A9-E2F3-4DD3-8734-A2698A227231}']
end;

[JavaSignature('android/view/MenuItem$OnActionExpandListener')]
JMenuItem_OnActionExpandListener = interface(IJavaInstance)
['{D0287E3E-BC17-476D-8936-96A6BF0E34AB}']
  {Methods}
  function onMenuItemActionCollapse(item: JMenuItem): Boolean; cdecl;
  function onMenuItemActionExpand(item: JMenuItem): Boolean; cdecl;
end;
TJMenuItem_OnActionExpandListener = class(TJavaGenericImport<JMenuItem_OnActionExpandListenerClass, JMenuItem_OnActionExpandListener>) end;

JColorDrawableClass = interface(JDrawableClass)
['{C48FA944-8DB5-4626-BF46-E264FDFA6137}']
  {Methods}
  function init: JColorDrawable; cdecl; overload;
  function init(color: Integer): JColorDrawable; cdecl; overload;
end;

[JavaSignature('android/graphics/drawable/ColorDrawable')]
JColorDrawable = interface(JDrawable)
['{BF6A29E7-9693-4FFB-AD3E-92F3B7BDC9CF}']
  {Methods}
  procedure draw(canvas: JCanvas); cdecl;
  function getAlpha: Integer; cdecl;
  function getChangingConfigurations: Integer; cdecl;
  function getColor: Integer; cdecl;
  function getConstantState: JDrawable_ConstantState; cdecl;
  function getOpacity: Integer; cdecl;
  function mutate: JDrawable; cdecl;
  procedure setAlpha(alpha: Integer); cdecl;
  procedure setColor(color: Integer); cdecl;
  procedure setColorFilter(colorFilter: JColorFilter); cdecl;
end;
TJColorDrawable = class(TJavaGenericImport<JColorDrawableClass, JColorDrawable>) end;

JEditableClass = interface(JCharSequenceClass)
['{4DF3A8C1-D180-4B93-80D6-3B57236FB3B9}']
end;

[JavaSignature('android/text/Editable')]
JEditable = interface(JCharSequence)
['{A8BBE246-26EE-44F1-A467-779D080957A0}']
  {Methods}
  function append(text: JCharSequence): JEditable; cdecl; overload;
  function append(text: JCharSequence; start: Integer; end_: Integer): JEditable; cdecl; overload;
  function append(text: Char): JEditable; cdecl; overload;
  procedure clear; cdecl;
  procedure clearSpans; cdecl;
  function delete(st: Integer; en: Integer): JEditable; cdecl;
  function getFilters: TJavaObjectArray<JInputFilter>; cdecl;
  function insert(where: Integer; text: JCharSequence; start: Integer; end_: Integer): JEditable; cdecl; overload;
  function insert(where: Integer; text: JCharSequence): JEditable; cdecl; overload;
  function replace(st: Integer; en: Integer; source: JCharSequence; start: Integer; end_: Integer): JEditable; cdecl; overload;
  function replace(st: Integer; en: Integer; text: JCharSequence): JEditable; cdecl; overload;
  procedure setFilters(filters: TJavaObjectArray<JInputFilter>); cdecl;
end;
TJEditable = class(TJavaGenericImport<JEditableClass, JEditable>) end;

JBlurMaskFilterClass = interface(JMaskFilterClass)
['{59DC22CE-33D8-4E04-BC54-57C1593511B6}']
  {Methods}
  function init(radius: Single; style: JBlurMaskFilter_Blur): JBlurMaskFilter; cdecl;
end;

[JavaSignature('android/graphics/BlurMaskFilter')]
JBlurMaskFilter = interface(JMaskFilter)
['{7023099B-9AA8-44C0-879A-28591CC9DF27}']
end;
TJBlurMaskFilter = class(TJavaGenericImport<JBlurMaskFilterClass, JBlurMaskFilter>) end;

JComposePathEffectClass = interface(JPathEffectClass)
['{46513C14-21C2-4EDF-88D6-6C0A4A3F8CBE}']
  {Methods}
  function init(outerpe: JPathEffect; innerpe: JPathEffect): JComposePathEffect; cdecl;
end;

[JavaSignature('android/graphics/ComposePathEffect')]
JComposePathEffect = interface(JPathEffect)
['{6E324B1F-E418-40F2-8392-F918AA09D391}']
end;
TJComposePathEffect = class(TJavaGenericImport<JComposePathEffectClass, JComposePathEffect>) end;

JPictureDrawableClass = interface(JDrawableClass)
['{34008001-5539-415F-89C7-138E7B48FFE1}']
  {Methods}
  function init(picture: JPicture): JPictureDrawable; cdecl;
end;

[JavaSignature('android/graphics/drawable/PictureDrawable')]
JPictureDrawable = interface(JDrawable)
['{71D3B28F-EB0A-4446-ABDF-8CFFB82240D6}']
  {Methods}
  procedure draw(canvas: JCanvas); cdecl;
  function getIntrinsicHeight: Integer; cdecl;
  function getIntrinsicWidth: Integer; cdecl;
  function getOpacity: Integer; cdecl;
  function getPicture: JPicture; cdecl;
  procedure setAlpha(alpha: Integer); cdecl;
  procedure setColorFilter(colorFilter: JColorFilter); cdecl;
  procedure setDither(dither: Boolean); cdecl;
  procedure setFilterBitmap(filter: Boolean); cdecl;
  procedure setPicture(picture: JPicture); cdecl;
end;
TJPictureDrawable = class(TJavaGenericImport<JPictureDrawableClass, JPictureDrawable>) end;

JContentProviderClass = interface(JObjectClass)
['{8EE3C773-05FF-43F9-9258-8DEF6555223E}']
  {Methods}
  function init: JContentProvider; cdecl;
end;

[JavaSignature('android/content/ContentProvider')]
JContentProvider = interface(JObject)
['{F0B401FB-B195-4776-B2F6-0723D8BF8A06}']
  {Methods}
  function applyBatch(operations: JArrayList): TJavaObjectArray<JContentProviderResult>; cdecl;
  procedure attachInfo(context: JContext; info: JProviderInfo); cdecl;
  function bulkInsert(uri: Jnet_Uri; values: TJavaObjectArray<JContentValues>): Integer; cdecl;
  function call(method: JString; arg: JString; extras: JBundle): JBundle; cdecl;
  function delete(uri: Jnet_Uri; selection: JString; selectionArgs: TJavaObjectArray<JString>): Integer; cdecl;
  function getContext: JContext; cdecl;
  function getPathPermissions: TJavaObjectArray<JPathPermission>; cdecl;
  function getReadPermission: JString; cdecl;
  function getStreamTypes(uri: Jnet_Uri; mimeTypeFilter: JString): TJavaObjectArray<JString>; cdecl;
  function getType(uri: Jnet_Uri): JString; cdecl;
  function getWritePermission: JString; cdecl;
  function insert(uri: Jnet_Uri; values: JContentValues): Jnet_Uri; cdecl;
  procedure onConfigurationChanged(newConfig: JConfiguration); cdecl;
  function onCreate: Boolean; cdecl;
  procedure onLowMemory; cdecl;
  procedure onTrimMemory(level: Integer); cdecl;
  function openAssetFile(uri: Jnet_Uri; mode: JString): JAssetFileDescriptor; cdecl;
  function openFile(uri: Jnet_Uri; mode: JString): JParcelFileDescriptor; cdecl;
  function openPipeHelper(uri: Jnet_Uri; mimeType: JString; opts: JBundle; args: JObject; func: JContentProvider_PipeDataWriter): JParcelFileDescriptor; cdecl;
  function openTypedAssetFile(uri: Jnet_Uri; mimeTypeFilter: JString; opts: JBundle): JAssetFileDescriptor; cdecl;
  function query(uri: Jnet_Uri; projection: TJavaObjectArray<JString>; selection: JString; selectionArgs: TJavaObjectArray<JString>; sortOrder: JString): JCursor; cdecl; overload;
  function query(uri: Jnet_Uri; projection: TJavaObjectArray<JString>; selection: JString; selectionArgs: TJavaObjectArray<JString>; sortOrder: JString; cancellationSignal: JCancellationSignal): JCursor; cdecl; overload;
  procedure shutdown; cdecl;
  function update(uri: Jnet_Uri; values: JContentValues; selection: JString; selectionArgs: TJavaObjectArray<JString>): Integer; cdecl;
end;
TJContentProvider = class(TJavaGenericImport<JContentProviderClass, JContentProvider>) end;

JSearchRecentSuggestionsProviderClass = interface(JContentProviderClass)
['{230B5BBC-D00A-4478-9511-F1F99773FAE8}']
  {Property Methods}
  function _GetDATABASE_MODE_2LINES: Integer;
  function _GetDATABASE_MODE_QUERIES: Integer;
  {Methods}
  function init: JSearchRecentSuggestionsProvider; cdecl;
  {Properties}
  property DATABASE_MODE_2LINES: Integer read _GetDATABASE_MODE_2LINES;
  property DATABASE_MODE_QUERIES: Integer read _GetDATABASE_MODE_QUERIES;
end;

[JavaSignature('android/content/SearchRecentSuggestionsProvider')]
JSearchRecentSuggestionsProvider = interface(JContentProvider)
['{C8FE17D4-9142-4AEC-A486-81E0349B62AC}']
  {Methods}
  function delete(uri: Jnet_Uri; selection: JString; selectionArgs: TJavaObjectArray<JString>): Integer; cdecl;
  function getType(uri: Jnet_Uri): JString; cdecl;
  function insert(uri: Jnet_Uri; values: JContentValues): Jnet_Uri; cdecl;
  function onCreate: Boolean; cdecl;
  function query(uri: Jnet_Uri; projection: TJavaObjectArray<JString>; selection: JString; selectionArgs: TJavaObjectArray<JString>; sortOrder: JString): JCursor; cdecl;
  function update(uri: Jnet_Uri; values: JContentValues; selection: JString; selectionArgs: TJavaObjectArray<JString>): Integer; cdecl;
end;
TJSearchRecentSuggestionsProvider = class(TJavaGenericImport<JSearchRecentSuggestionsProviderClass, JSearchRecentSuggestionsProvider>) end;

JSubMenuClass = interface(JMenuClass)
['{F9A030D2-EE0B-4CA1-BE38-82DD815A6573}']
end;

[JavaSignature('android/view/SubMenu')]
JSubMenu = interface(JMenu)
['{F9C533A8-5B94-4DD4-BFFE-9313E4A362B4}']
  {Methods}
  procedure clearHeader; cdecl;
  function getItem: JMenuItem; cdecl;
  function setHeaderIcon(iconRes: Integer): JSubMenu; cdecl; overload;
  function setHeaderIcon(icon: JDrawable): JSubMenu; cdecl; overload;
  function setHeaderTitle(titleRes: Integer): JSubMenu; cdecl; overload;
  function setHeaderTitle(title: JCharSequence): JSubMenu; cdecl; overload;
  function setHeaderView(view: JView): JSubMenu; cdecl;
  function setIcon(iconRes: Integer): JSubMenu; cdecl; overload;
  function setIcon(icon: JDrawable): JSubMenu; cdecl; overload;
end;
TJSubMenu = class(TJavaGenericImport<JSubMenuClass, JSubMenu>) end;

JLevelListDrawableClass = interface(JDrawableContainerClass)
['{AD8A4554-441D-4FC1-A800-1194C2724EB5}']
  {Methods}
  function init: JLevelListDrawable; cdecl;
end;

[JavaSignature('android/graphics/drawable/LevelListDrawable')]
JLevelListDrawable = interface(JDrawableContainer)
['{7D90BA12-2370-49F8-8CC9-6ED6D7773F0D}']
  {Methods}
  procedure addLevel(low: Integer; high: Integer; drawable: JDrawable); cdecl;
  function mutate: JDrawable; cdecl;
end;
TJLevelListDrawable = class(TJavaGenericImport<JLevelListDrawableClass, JLevelListDrawable>) end;

JAvoidXfermode_ModeClass = interface(JEnumClass)
['{D2330AA6-4011-4DC7-9DEC-9FF8E661EF67}']
  {Property Methods}
  function _GetAVOID: JAvoidXfermode_Mode;
  function _GetTARGET: JAvoidXfermode_Mode;
  {Methods}
  function valueOf(name: JString): JAvoidXfermode_Mode; cdecl;
  function values: TJavaObjectArray<JAvoidXfermode_Mode>; cdecl;
  {Properties}
  property AVOID: JAvoidXfermode_Mode read _GetAVOID;
  property TARGET: JAvoidXfermode_Mode read _GetTARGET;
end;

[JavaSignature('android/graphics/AvoidXfermode$Mode')]
JAvoidXfermode_Mode = interface(JEnum)
['{555505F5-1421-4ECE-9AAB-BBB62E629B38}']
end;
TJAvoidXfermode_Mode = class(TJavaGenericImport<JAvoidXfermode_ModeClass, JAvoidXfermode_Mode>) end;

JExtractedTextClass = interface(JObjectClass)
['{D8A18C24-B8D8-499D-B2D7-1A4B41EBF674}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  function _GetFLAG_SELECTING: Integer;
  function _GetFLAG_SINGLE_LINE: Integer;
  {Methods}
  function init: JExtractedText; cdecl;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
  property FLAG_SELECTING: Integer read _GetFLAG_SELECTING;
  property FLAG_SINGLE_LINE: Integer read _GetFLAG_SINGLE_LINE;
end;

[JavaSignature('android/view/inputmethod/ExtractedText')]
JExtractedText = interface(JObject)
['{53D94752-ABC7-45F2-85F8-D4517C7452C5}']
  {Property Methods}
  function _Getflags: Integer;
  procedure _Setflags(Value: Integer);
  function _GetpartialEndOffset: Integer;
  procedure _SetpartialEndOffset(Value: Integer);
  function _GetpartialStartOffset: Integer;
  procedure _SetpartialStartOffset(Value: Integer);
  function _GetselectionEnd: Integer;
  procedure _SetselectionEnd(Value: Integer);
  function _GetselectionStart: Integer;
  procedure _SetselectionStart(Value: Integer);
  function _GetstartOffset: Integer;
  procedure _SetstartOffset(Value: Integer);
  function _Gettext: JCharSequence;
  procedure _Settext(Value: JCharSequence);
  {Methods}
  function describeContents: Integer; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  {Properties}
  property flags: Integer read _Getflags write _Setflags;
  property partialEndOffset: Integer read _GetpartialEndOffset write _SetpartialEndOffset;
  property partialStartOffset: Integer read _GetpartialStartOffset write _SetpartialStartOffset;
  property selectionEnd: Integer read _GetselectionEnd write _SetselectionEnd;
  property selectionStart: Integer read _GetselectionStart write _SetselectionStart;
  property startOffset: Integer read _GetstartOffset write _SetstartOffset;
  property text: JCharSequence read _Gettext write _Settext;
end;
TJExtractedText = class(TJavaGenericImport<JExtractedTextClass, JExtractedText>) end;

JNinePatchDrawableClass = interface(JDrawableClass)
['{73FA6785-B351-441A-A494-E362EA82B6AE}']
  {Methods}
  function init(bitmap: JBitmap; chunk: TJavaArray<Byte>; padding: JRect; srcName: JString): JNinePatchDrawable; cdecl; overload;//Deprecated
  function init(res: JResources; bitmap: JBitmap; chunk: TJavaArray<Byte>; padding: JRect; srcName: JString): JNinePatchDrawable; cdecl; overload;
  function init(patch: JNinePatch): JNinePatchDrawable; cdecl; overload;//Deprecated
  function init(res: JResources; patch: JNinePatch): JNinePatchDrawable; cdecl; overload;
end;

[JavaSignature('android/graphics/drawable/NinePatchDrawable')]
JNinePatchDrawable = interface(JDrawable)
['{42C8A806-41A1-4B9F-BDE2-0FB9FAF6FB6F}']
  {Methods}
  procedure draw(canvas: JCanvas); cdecl;
  function getChangingConfigurations: Integer; cdecl;
  function getConstantState: JDrawable_ConstantState; cdecl;
  function getIntrinsicHeight: Integer; cdecl;
  function getIntrinsicWidth: Integer; cdecl;
  function getMinimumHeight: Integer; cdecl;
  function getMinimumWidth: Integer; cdecl;
  function getOpacity: Integer; cdecl;
  function getPadding(padding: JRect): Boolean; cdecl;
  function getPaint: JPaint; cdecl;
  function getTransparentRegion: JRegion; cdecl;
  function mutate: JDrawable; cdecl;
  procedure setAlpha(alpha: Integer); cdecl;
  procedure setColorFilter(cf: JColorFilter); cdecl;
  procedure setDither(dither: Boolean); cdecl;
  procedure setFilterBitmap(filter: Boolean); cdecl;
  procedure setTargetDensity(canvas: JCanvas); cdecl; overload;
  procedure setTargetDensity(metrics: JDisplayMetrics); cdecl; overload;
  procedure setTargetDensity(density: Integer); cdecl; overload;
end;
TJNinePatchDrawable = class(TJavaGenericImport<JNinePatchDrawableClass, JNinePatchDrawable>) end;

JSyncContextClass = interface(JObjectClass)
['{658C6138-474B-427E-8444-E60B1D8BD42C}']
end;

[JavaSignature('android/content/SyncContext')]
JSyncContext = interface(JObject)
['{E5C7D653-6BE5-4A63-94E1-1639F17F4FBD}']
  {Methods}
  function getSyncContextBinder: JIBinder; cdecl;
  procedure onFinished(result: JSyncResult); cdecl;
end;
TJSyncContext = class(TJavaGenericImport<JSyncContextClass, JSyncContext>) end;

JApplicationInfoClass = interface(JPackageItemInfoClass)
['{6C1CC199-522E-457E-B714-632058B818A5}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  function _GetFLAG_ALLOW_BACKUP: Integer;
  function _GetFLAG_ALLOW_CLEAR_USER_DATA: Integer;
  function _GetFLAG_ALLOW_TASK_REPARENTING: Integer;
  function _GetFLAG_DEBUGGABLE: Integer;
  function _GetFLAG_EXTERNAL_STORAGE: Integer;
  function _GetFLAG_FACTORY_TEST: Integer;
  function _GetFLAG_HAS_CODE: Integer;
  function _GetFLAG_INSTALLED: Integer;
  function _GetFLAG_IS_DATA_ONLY: Integer;
  function _GetFLAG_KILL_AFTER_RESTORE: Integer;
  function _GetFLAG_LARGE_HEAP: Integer;
  function _GetFLAG_PERSISTENT: Integer;
  function _GetFLAG_RESIZEABLE_FOR_SCREENS: Integer;
  function _GetFLAG_RESTORE_ANY_VERSION: Integer;
  function _GetFLAG_STOPPED: Integer;
  function _GetFLAG_SUPPORTS_LARGE_SCREENS: Integer;
  function _GetFLAG_SUPPORTS_NORMAL_SCREENS: Integer;
  function _GetFLAG_SUPPORTS_RTL: Integer;
  function _GetFLAG_SUPPORTS_SCREEN_DENSITIES: Integer;
  function _GetFLAG_SUPPORTS_SMALL_SCREENS: Integer;
  function _GetFLAG_SUPPORTS_XLARGE_SCREENS: Integer;
  function _GetFLAG_SYSTEM: Integer;
  function _GetFLAG_TEST_ONLY: Integer;
  function _GetFLAG_UPDATED_SYSTEM_APP: Integer;
  function _GetFLAG_VM_SAFE_MODE: Integer;
  {Methods}
  function init: JApplicationInfo; cdecl; overload;
  function init(orig: JApplicationInfo): JApplicationInfo; cdecl; overload;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
  property FLAG_ALLOW_BACKUP: Integer read _GetFLAG_ALLOW_BACKUP;
  property FLAG_ALLOW_CLEAR_USER_DATA: Integer read _GetFLAG_ALLOW_CLEAR_USER_DATA;
  property FLAG_ALLOW_TASK_REPARENTING: Integer read _GetFLAG_ALLOW_TASK_REPARENTING;
  property FLAG_DEBUGGABLE: Integer read _GetFLAG_DEBUGGABLE;
  property FLAG_EXTERNAL_STORAGE: Integer read _GetFLAG_EXTERNAL_STORAGE;
  property FLAG_FACTORY_TEST: Integer read _GetFLAG_FACTORY_TEST;
  property FLAG_HAS_CODE: Integer read _GetFLAG_HAS_CODE;
  property FLAG_INSTALLED: Integer read _GetFLAG_INSTALLED;
  property FLAG_IS_DATA_ONLY: Integer read _GetFLAG_IS_DATA_ONLY;
  property FLAG_KILL_AFTER_RESTORE: Integer read _GetFLAG_KILL_AFTER_RESTORE;
  property FLAG_LARGE_HEAP: Integer read _GetFLAG_LARGE_HEAP;
  property FLAG_PERSISTENT: Integer read _GetFLAG_PERSISTENT;
  property FLAG_RESIZEABLE_FOR_SCREENS: Integer read _GetFLAG_RESIZEABLE_FOR_SCREENS;
  property FLAG_RESTORE_ANY_VERSION: Integer read _GetFLAG_RESTORE_ANY_VERSION;
  property FLAG_STOPPED: Integer read _GetFLAG_STOPPED;
  property FLAG_SUPPORTS_LARGE_SCREENS: Integer read _GetFLAG_SUPPORTS_LARGE_SCREENS;
  property FLAG_SUPPORTS_NORMAL_SCREENS: Integer read _GetFLAG_SUPPORTS_NORMAL_SCREENS;
  property FLAG_SUPPORTS_RTL: Integer read _GetFLAG_SUPPORTS_RTL;
  property FLAG_SUPPORTS_SCREEN_DENSITIES: Integer read _GetFLAG_SUPPORTS_SCREEN_DENSITIES;
  property FLAG_SUPPORTS_SMALL_SCREENS: Integer read _GetFLAG_SUPPORTS_SMALL_SCREENS;
  property FLAG_SUPPORTS_XLARGE_SCREENS: Integer read _GetFLAG_SUPPORTS_XLARGE_SCREENS;
  property FLAG_SYSTEM: Integer read _GetFLAG_SYSTEM;
  property FLAG_TEST_ONLY: Integer read _GetFLAG_TEST_ONLY;
  property FLAG_UPDATED_SYSTEM_APP: Integer read _GetFLAG_UPDATED_SYSTEM_APP;
  property FLAG_VM_SAFE_MODE: Integer read _GetFLAG_VM_SAFE_MODE;
end;

[JavaSignature('android/content/pm/ApplicationInfo')]
JApplicationInfo = interface(JPackageItemInfo)
['{821C839E-7C7F-401F-AECD-8C3317518BE9}']
  {Property Methods}
  function _GetbackupAgentName: JString;
  procedure _SetbackupAgentName(Value: JString);
  function _GetclassName: JString;
  procedure _SetclassName(Value: JString);
  function _GetcompatibleWidthLimitDp: Integer;
  procedure _SetcompatibleWidthLimitDp(Value: Integer);
  function _GetdataDir: JString;
  procedure _SetdataDir(Value: JString);
  function _GetdescriptionRes: Integer;
  procedure _SetdescriptionRes(Value: Integer);
  function _Getenabled: Boolean;
  procedure _Setenabled(Value: Boolean);
  function _Getflags: Integer;
  procedure _Setflags(Value: Integer);
  function _GetlargestWidthLimitDp: Integer;
  procedure _SetlargestWidthLimitDp(Value: Integer);
  function _GetmanageSpaceActivityName: JString;
  procedure _SetmanageSpaceActivityName(Value: JString);
  function _GetnativeLibraryDir: JString;
  procedure _SetnativeLibraryDir(Value: JString);
  function _Getpermission: JString;
  procedure _Setpermission(Value: JString);
  function _GetprocessName: JString;
  procedure _SetprocessName(Value: JString);
  function _GetpublicSourceDir: JString;
  procedure _SetpublicSourceDir(Value: JString);
  function _GetrequiresSmallestWidthDp: Integer;
  procedure _SetrequiresSmallestWidthDp(Value: Integer);
  function _GetsharedLibraryFiles: TJavaObjectArray<JString>;
  procedure _SetsharedLibraryFiles(Value: TJavaObjectArray<JString>);
  function _GetsourceDir: JString;
  procedure _SetsourceDir(Value: JString);
  function _GettargetSdkVersion: Integer;
  procedure _SettargetSdkVersion(Value: Integer);
  function _GettaskAffinity: JString;
  procedure _SettaskAffinity(Value: JString);
  function _Gettheme: Integer;
  procedure _Settheme(Value: Integer);
  function _GetuiOptions: Integer;
  procedure _SetuiOptions(Value: Integer);
  function _Getuid: Integer;
  procedure _Setuid(Value: Integer);
  {Methods}
  function describeContents: Integer; cdecl;
  function loadDescription(pm: JPackageManager): JCharSequence; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; parcelableFlags: Integer); cdecl;
  {Properties}
  property backupAgentName: JString read _GetbackupAgentName write _SetbackupAgentName;
  property className: JString read _GetclassName write _SetclassName;
  property compatibleWidthLimitDp: Integer read _GetcompatibleWidthLimitDp write _SetcompatibleWidthLimitDp;
  property dataDir: JString read _GetdataDir write _SetdataDir;
  property descriptionRes: Integer read _GetdescriptionRes write _SetdescriptionRes;
  property enabled: Boolean read _Getenabled write _Setenabled;
  property flags: Integer read _Getflags write _Setflags;
  property largestWidthLimitDp: Integer read _GetlargestWidthLimitDp write _SetlargestWidthLimitDp;
  property manageSpaceActivityName: JString read _GetmanageSpaceActivityName write _SetmanageSpaceActivityName;
  property nativeLibraryDir: JString read _GetnativeLibraryDir write _SetnativeLibraryDir;
  property permission: JString read _Getpermission write _Setpermission;
  property processName: JString read _GetprocessName write _SetprocessName;
  property publicSourceDir: JString read _GetpublicSourceDir write _SetpublicSourceDir;
  property requiresSmallestWidthDp: Integer read _GetrequiresSmallestWidthDp write _SetrequiresSmallestWidthDp;
  property sharedLibraryFiles: TJavaObjectArray<JString> read _GetsharedLibraryFiles write _SetsharedLibraryFiles;
  property sourceDir: JString read _GetsourceDir write _SetsourceDir;
  property targetSdkVersion: Integer read _GettargetSdkVersion write _SettargetSdkVersion;
  property taskAffinity: JString read _GettaskAffinity write _SettaskAffinity;
  property theme: Integer read _Gettheme write _Settheme;
  property uiOptions: Integer read _GetuiOptions write _SetuiOptions;
  property uid: Integer read _Getuid write _Setuid;
end;
TJApplicationInfo = class(TJavaGenericImport<JApplicationInfoClass, JApplicationInfo>) end;

JView_OnLayoutChangeListenerClass = interface(IJavaClass)
['{BCD1B297-4D8D-4762-8F4B-3DACFF2BFFC0}']
end;

[JavaSignature('android/view/View$OnLayoutChangeListener')]
JView_OnLayoutChangeListener = interface(IJavaInstance)
['{2F6FD289-4AD3-4E28-B71F-306D24A3DC58}']
  {Methods}
  procedure onLayoutChange(v: JView; left: Integer; top: Integer; right: Integer; bottom: Integer; oldLeft: Integer; oldTop: Integer; oldRight: Integer; oldBottom: Integer); cdecl;
end;
TJView_OnLayoutChangeListener = class(TJavaGenericImport<JView_OnLayoutChangeListenerClass, JView_OnLayoutChangeListener>) end;

JInstrumentationInfoClass = interface(JPackageItemInfoClass)
['{5335DAE0-A1E4-45D5-B9EE-EE7F60B1E86D}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Methods}
  function init: JInstrumentationInfo; cdecl; overload;
  function init(orig: JInstrumentationInfo): JInstrumentationInfo; cdecl; overload;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/content/pm/InstrumentationInfo')]
JInstrumentationInfo = interface(JPackageItemInfo)
['{8F71514D-3E27-4245-832C-AE9959BECD84}']
  {Property Methods}
  function _GetdataDir: JString;
  procedure _SetdataDir(Value: JString);
  function _GetfunctionalTest: Boolean;
  procedure _SetfunctionalTest(Value: Boolean);
  function _GethandleProfiling: Boolean;
  procedure _SethandleProfiling(Value: Boolean);
  function _GetpublicSourceDir: JString;
  procedure _SetpublicSourceDir(Value: JString);
  function _GetsourceDir: JString;
  procedure _SetsourceDir(Value: JString);
  function _GettargetPackage: JString;
  procedure _SettargetPackage(Value: JString);
  {Methods}
  function describeContents: Integer; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; parcelableFlags: Integer); cdecl;
  {Properties}
  property dataDir: JString read _GetdataDir write _SetdataDir;
  property functionalTest: Boolean read _GetfunctionalTest write _SetfunctionalTest;
  property handleProfiling: Boolean read _GethandleProfiling write _SethandleProfiling;
  property publicSourceDir: JString read _GetpublicSourceDir write _SetpublicSourceDir;
  property sourceDir: JString read _GetsourceDir write _SetsourceDir;
  property targetPackage: JString read _GettargetPackage write _SettargetPackage;
end;
TJInstrumentationInfo = class(TJavaGenericImport<JInstrumentationInfoClass, JInstrumentationInfo>) end;

JDialogInterfaceClass = interface(IJavaClass)
['{674BCC17-38EE-4151-90EE-AC3AB6DCB0F1}']
  {Property Methods}
  function _GetBUTTON1: Integer;
  function _GetBUTTON2: Integer;
  function _GetBUTTON3: Integer;
  function _GetBUTTON_NEGATIVE: Integer;
  function _GetBUTTON_NEUTRAL: Integer;
  function _GetBUTTON_POSITIVE: Integer;
  {Properties}
  property BUTTON1: Integer read _GetBUTTON1;
  property BUTTON2: Integer read _GetBUTTON2;
  property BUTTON3: Integer read _GetBUTTON3;
  property BUTTON_NEGATIVE: Integer read _GetBUTTON_NEGATIVE;
  property BUTTON_NEUTRAL: Integer read _GetBUTTON_NEUTRAL;
  property BUTTON_POSITIVE: Integer read _GetBUTTON_POSITIVE;
end;

[JavaSignature('android/content/DialogInterface')]
JDialogInterface = interface(IJavaInstance)
['{91B070BB-6395-468C-8316-A32E4A2AD21A}']
  {Methods}
  procedure cancel; cdecl;
  procedure dismiss; cdecl;
end;
TJDialogInterface = class(TJavaGenericImport<JDialogInterfaceClass, JDialogInterface>) end;

JSQLiteDatabase_CursorFactoryClass = interface(IJavaClass)
['{A427AD75-87E1-4F79-8EC8-C024197EE16F}']
end;

[JavaSignature('android/database/sqlite/SQLiteDatabase$CursorFactory')]
JSQLiteDatabase_CursorFactory = interface(IJavaInstance)
['{22266733-082B-4989-8DFF-A4AD8FDA17E2}']
  {Methods}
  function newCursor(db: JSQLiteDatabase; masterQuery: JSQLiteCursorDriver; editTable: JString; query: JSQLiteQuery): JCursor; cdecl;
end;
TJSQLiteDatabase_CursorFactory = class(TJavaGenericImport<JSQLiteDatabase_CursorFactoryClass, JSQLiteDatabase_CursorFactory>) end;

JIntentFilter_MalformedMimeTypeExceptionClass = interface(JAndroidExceptionClass)
['{85A7FAC4-6FD1-4555-8A9F-E38B48725D28}']
  {Methods}
  function init: JIntentFilter_MalformedMimeTypeException; cdecl; overload;
  function init(name: JString): JIntentFilter_MalformedMimeTypeException; cdecl; overload;
end;

[JavaSignature('android/content/IntentFilter$MalformedMimeTypeException')]
JIntentFilter_MalformedMimeTypeException = interface(JAndroidException)
['{7056624D-AAC5-499A-A953-1230433E9030}']
end;
TJIntentFilter_MalformedMimeTypeException = class(TJavaGenericImport<JIntentFilter_MalformedMimeTypeExceptionClass, JIntentFilter_MalformedMimeTypeException>) end;

JViewParentClass = interface(IJavaClass)
['{A91CFAFD-3CE9-4096-9EE7-00EC7828023D}']
end;

[JavaSignature('android/view/ViewParent')]
JViewParent = interface(IJavaInstance)
['{910F6769-C189-4CF1-90B6-6FD45516718D}']
  {Methods}
  procedure bringChildToFront(child: JView); cdecl;
  procedure childDrawableStateChanged(child: JView); cdecl;
  procedure clearChildFocus(child: JView); cdecl;
  procedure createContextMenu(menu: JContextMenu); cdecl;
  function focusSearch(v: JView; direction: Integer): JView; cdecl;
  procedure focusableViewAvailable(v: JView); cdecl;
  function getChildVisibleRect(child: JView; r: JRect; offset: JPoint): Boolean; cdecl;
  function getParent: JViewParent; cdecl;
  function getParentForAccessibility: JViewParent; cdecl;
  procedure invalidateChild(child: JView; r: JRect); cdecl;
  function invalidateChildInParent(location: TJavaArray<Integer>; r: JRect): JViewParent; cdecl;
  function isLayoutRequested: Boolean; cdecl;
  procedure recomputeViewAttributes(child: JView); cdecl;
  procedure requestChildFocus(child: JView; focused: JView); cdecl;
  function requestChildRectangleOnScreen(child: JView; rectangle: JRect; immediate: Boolean): Boolean; cdecl;
  procedure requestDisallowInterceptTouchEvent(disallowIntercept: Boolean); cdecl;
  procedure requestFitSystemWindows; cdecl;
  procedure requestLayout; cdecl;
  procedure requestTransparentRegion(child: JView); cdecl;
  function showContextMenuForChild(originalView: JView): Boolean; cdecl;
  function startActionModeForChild(originalView: JView; callback: JActionMode_Callback): JActionMode; cdecl;
end;
TJViewParent = class(TJavaGenericImport<JViewParentClass, JViewParent>) end;

JPorterDuffClass = interface(JObjectClass)
['{D017DF20-908A-468F-86FB-FCEC628BE8CB}']
  {Methods}
  function init: JPorterDuff; cdecl;
end;

[JavaSignature('android/graphics/PorterDuff')]
JPorterDuff = interface(JObject)
['{1E3AFE84-D0AA-4365-A719-D53CE6E9D8EC}']
end;
TJPorterDuff = class(TJavaGenericImport<JPorterDuffClass, JPorterDuff>) end;

JRectFClass = interface(JObjectClass)
['{70E49C38-BAB4-4BDA-9B00-03F5C269F3A5}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Methods}
  function init: JRectF; cdecl; overload;
  function init(left: Single; top: Single; right: Single; bottom: Single): JRectF; cdecl; overload;
  function init(r: JRectF): JRectF; cdecl; overload;
  function init(r: JRect): JRectF; cdecl; overload;
  function intersects(a: JRectF; b: JRectF): Boolean; cdecl; overload;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/graphics/RectF')]
JRectF = interface(JObject)
['{44C699AF-A015-4109-B6F0-30F2F7A54410}']
  {Property Methods}
  function _Getbottom: Single;
  procedure _Setbottom(Value: Single);
  function _Getleft: Single;
  procedure _Setleft(Value: Single);
  function _Getright: Single;
  procedure _Setright(Value: Single);
  function _Gettop: Single;
  procedure _Settop(Value: Single);
  {Methods}
  function centerX: Single; cdecl;
  function centerY: Single; cdecl;
  function &contains(x: Single; y: Single): Boolean; cdecl; overload;
  function &contains(left: Single; top: Single; right: Single; bottom: Single): Boolean; cdecl; overload;
  function &contains(r: JRectF): Boolean; cdecl; overload;
  function describeContents: Integer; cdecl;
  function equals(o: JObject): Boolean; cdecl;
  function hashCode: Integer; cdecl;
  function height: Single; cdecl;
  procedure inset(dx: Single; dy: Single); cdecl;
  function intersect(left: Single; top: Single; right: Single; bottom: Single): Boolean; cdecl; overload;
  function intersect(r: JRectF): Boolean; cdecl; overload;
  function intersects(left: Single; top: Single; right: Single; bottom: Single): Boolean; cdecl; overload;
  function isEmpty: Boolean; cdecl;
  procedure offset(dx: Single; dy: Single); cdecl;
  procedure offsetTo(newLeft: Single; newTop: Single); cdecl;
  procedure readFromParcel(in_: JParcel); cdecl;
  procedure round(dst: JRect); cdecl;
  procedure roundOut(dst: JRect); cdecl;
  procedure &set(left: Single; top: Single; right: Single; bottom: Single); cdecl; overload;
  procedure &set(src: JRectF); cdecl; overload;
  procedure &set(src: JRect); cdecl; overload;
  procedure setEmpty; cdecl;
  function setIntersect(a: JRectF; b: JRectF): Boolean; cdecl;
  procedure sort; cdecl;
  function toShortString: JString; cdecl;
  function toString: JString; cdecl;
  procedure union(left: Single; top: Single; right: Single; bottom: Single); cdecl; overload;
  procedure union(r: JRectF); cdecl; overload;
  procedure union(x: Single; y: Single); cdecl; overload;
  function width: Single; cdecl;
  procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
  {Properties}
  property bottom: Single read _Getbottom write _Setbottom;
  property left: Single read _Getleft write _Setleft;
  property right: Single read _Getright write _Setright;
  property top: Single read _Gettop write _Settop;
end;
TJRectF = class(TJavaGenericImport<JRectFClass, JRectF>) end;

JIntent_ShortcutIconResourceClass = interface(JObjectClass)
['{52C2038E-04AB-4165-B2F3-8076C1C7D2B5}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Methods}
  function init: JIntent_ShortcutIconResource; cdecl;
  function fromContext(context: JContext; resourceId: Integer): JIntent_ShortcutIconResource; cdecl;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/content/Intent$ShortcutIconResource')]
JIntent_ShortcutIconResource = interface(JObject)
['{6269D767-3CD4-4C55-9174-6D90D217F529}']
  {Property Methods}
  function _GetpackageName: JString;
  procedure _SetpackageName(Value: JString);
  function _GetresourceName: JString;
  procedure _SetresourceName(Value: JString);
  {Methods}
  function describeContents: Integer; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  {Properties}
  property packageName: JString read _GetpackageName write _SetpackageName;
  property resourceName: JString read _GetresourceName write _SetresourceName;
end;
TJIntent_ShortcutIconResource = class(TJavaGenericImport<JIntent_ShortcutIconResourceClass, JIntent_ShortcutIconResource>) end;

JKeyEvent_CallbackClass = interface(IJavaClass)
['{681049BD-1FD0-428D-8B0B-B5A7A4A25103}']
end;

[JavaSignature('android/view/KeyEvent$Callback')]
JKeyEvent_Callback = interface(IJavaInstance)
['{DE393309-B906-4BEB-80A7-9285BDDD9BA9}']
  {Methods}
  function onKeyDown(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  function onKeyLongPress(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  function onKeyMultiple(keyCode: Integer; count: Integer; event: JKeyEvent): Boolean; cdecl;
  function onKeyUp(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
end;
TJKeyEvent_Callback = class(TJavaGenericImport<JKeyEvent_CallbackClass, JKeyEvent_Callback>) end;

JScaleGestureDetector_SimpleOnScaleGestureListenerClass = interface(JObjectClass)
['{A4E5B6F6-A03A-4582-9F07-4613FA7F0EC4}']
  {Methods}
  function init: JScaleGestureDetector_SimpleOnScaleGestureListener; cdecl;
end;

[JavaSignature('android/view/ScaleGestureDetector$SimpleOnScaleGestureListener')]
JScaleGestureDetector_SimpleOnScaleGestureListener = interface(JObject)
['{11B14F37-7112-4CC6-AE48-D54F81CC19E0}']
  {Methods}
  function onScale(detector: JScaleGestureDetector): Boolean; cdecl;
  function onScaleBegin(detector: JScaleGestureDetector): Boolean; cdecl;
  procedure onScaleEnd(detector: JScaleGestureDetector); cdecl;
end;
TJScaleGestureDetector_SimpleOnScaleGestureListener = class(TJavaGenericImport<JScaleGestureDetector_SimpleOnScaleGestureListenerClass, JScaleGestureDetector_SimpleOnScaleGestureListener>) end;

JStateListDrawableClass = interface(JDrawableContainerClass)
['{800AF89B-2C59-45AE-A25F-85A4EDFCAC3F}']
  {Methods}
  function init: JStateListDrawable; cdecl;
end;

[JavaSignature('android/graphics/drawable/StateListDrawable')]
JStateListDrawable = interface(JDrawableContainer)
['{70C240CA-58E2-4A9A-8706-05754EF39D22}']
  {Methods}
  procedure addState(stateSet: TJavaArray<Integer>; drawable: JDrawable); cdecl;
  function isStateful: Boolean; cdecl;
  function mutate: JDrawable; cdecl;
end;
TJStateListDrawable = class(TJavaGenericImport<JStateListDrawableClass, JStateListDrawable>) end;

Jcontent_ClipboardManagerClass = interface(JClipboardManagerClass)
['{45DFEA1B-074F-4A1A-8CFF-8E8E5F13A36F}']
end;

[JavaSignature('android/content/ClipboardManager')]
Jcontent_ClipboardManager = interface(JClipboardManager)
['{4A648B9F-6FF6-4F14-BEC0-03F29E9BD84B}']
  {Methods}
  procedure addPrimaryClipChangedListener(what: JClipboardManager_OnPrimaryClipChangedListener); cdecl;
  function getPrimaryClip: JClipData; cdecl;
  function getPrimaryClipDescription: JClipDescription; cdecl;
  function getText: JCharSequence; cdecl;//Deprecated
  function hasPrimaryClip: Boolean; cdecl;
  function hasText: Boolean; cdecl;//Deprecated
  procedure removePrimaryClipChangedListener(what: JClipboardManager_OnPrimaryClipChangedListener); cdecl;
  procedure setPrimaryClip(clip: JClipData); cdecl;
  procedure setText(text: JCharSequence); cdecl;//Deprecated
end;
TJcontent_ClipboardManager = class(TJavaGenericImport<Jcontent_ClipboardManagerClass, Jcontent_ClipboardManager>) end;

JPointFClass = interface(JObjectClass)
['{24275084-F838-4B34-9608-C8B50B17EE8C}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Methods}
  function init: JPointF; cdecl; overload;
  function init(x: Single; y: Single): JPointF; cdecl; overload;
  function init(p: JPoint): JPointF; cdecl; overload;
  function length(x: Single; y: Single): Single; cdecl; overload;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/graphics/PointF')]
JPointF = interface(JObject)
['{73BB8B8D-2F0D-45A3-8D1F-2513B931C282}']
  {Property Methods}
  function _Getx: Single;
  procedure _Setx(Value: Single);
  function _Gety: Single;
  procedure _Sety(Value: Single);
  {Methods}
  function describeContents: Integer; cdecl;
  function equals(x: Single; y: Single): Boolean; cdecl; overload;
  function equals(o: JObject): Boolean; cdecl; overload;
  function hashCode: Integer; cdecl;
  function length: Single; cdecl; overload;
  procedure negate; cdecl;
  procedure offset(dx: Single; dy: Single); cdecl;
  procedure readFromParcel(in_: JParcel); cdecl;
  procedure &set(x: Single; y: Single); cdecl; overload;
  procedure &set(p: JPointF); cdecl; overload;
  function toString: JString; cdecl;
  procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
  {Properties}
  property x: Single read _Getx write _Setx;
  property y: Single read _Gety write _Sety;
end;
TJPointF = class(TJavaGenericImport<JPointFClass, JPointF>) end;

JKeyEventClass = interface(JInputEventClass)
['{BF382776-AD45-47D2-BDDE-CEFE64BBE44A}']
  {Property Methods}
  function _GetACTION_DOWN: Integer;
  function _GetACTION_MULTIPLE: Integer;
  function _GetACTION_UP: Integer;
  function _GetCREATOR: JParcelable_Creator;
  function _GetFLAG_CANCELED: Integer;
  function _GetFLAG_CANCELED_LONG_PRESS: Integer;
  function _GetFLAG_EDITOR_ACTION: Integer;
  function _GetFLAG_FALLBACK: Integer;
  function _GetFLAG_FROM_SYSTEM: Integer;
  function _GetFLAG_KEEP_TOUCH_MODE: Integer;
  function _GetFLAG_LONG_PRESS: Integer;
  function _GetFLAG_SOFT_KEYBOARD: Integer;
  function _GetFLAG_TRACKING: Integer;
  function _GetFLAG_VIRTUAL_HARD_KEY: Integer;
  function _GetFLAG_WOKE_HERE: Integer;
  function _GetKEYCODE_0: Integer;
  function _GetKEYCODE_1: Integer;
  function _GetKEYCODE_2: Integer;
  function _GetKEYCODE_3: Integer;
  function _GetKEYCODE_3D_MODE: Integer;
  function _GetKEYCODE_4: Integer;
  function _GetKEYCODE_5: Integer;
  function _GetKEYCODE_6: Integer;
  function _GetKEYCODE_7: Integer;
  function _GetKEYCODE_8: Integer;
  function _GetKEYCODE_9: Integer;
  function _GetKEYCODE_A: Integer;
  function _GetKEYCODE_ALT_LEFT: Integer;
  function _GetKEYCODE_ALT_RIGHT: Integer;
  function _GetKEYCODE_APOSTROPHE: Integer;
  function _GetKEYCODE_APP_SWITCH: Integer;
  function _GetKEYCODE_ASSIST: Integer;
  function _GetKEYCODE_AT: Integer;
  function _GetKEYCODE_AVR_INPUT: Integer;
  function _GetKEYCODE_AVR_POWER: Integer;
  function _GetKEYCODE_B: Integer;
  function _GetKEYCODE_BACK: Integer;
  function _GetKEYCODE_BACKSLASH: Integer;
  function _GetKEYCODE_BOOKMARK: Integer;
  function _GetKEYCODE_BREAK: Integer;
  function _GetKEYCODE_BUTTON_1: Integer;
  function _GetKEYCODE_BUTTON_10: Integer;
  function _GetKEYCODE_BUTTON_11: Integer;
  function _GetKEYCODE_BUTTON_12: Integer;
  function _GetKEYCODE_BUTTON_13: Integer;
  function _GetKEYCODE_BUTTON_14: Integer;
  function _GetKEYCODE_BUTTON_15: Integer;
  function _GetKEYCODE_BUTTON_16: Integer;
  function _GetKEYCODE_BUTTON_2: Integer;
  function _GetKEYCODE_BUTTON_3: Integer;
  function _GetKEYCODE_BUTTON_4: Integer;
  function _GetKEYCODE_BUTTON_5: Integer;
  function _GetKEYCODE_BUTTON_6: Integer;
  function _GetKEYCODE_BUTTON_7: Integer;
  function _GetKEYCODE_BUTTON_8: Integer;
  function _GetKEYCODE_BUTTON_9: Integer;
  function _GetKEYCODE_BUTTON_A: Integer;
  function _GetKEYCODE_BUTTON_B: Integer;
  function _GetKEYCODE_BUTTON_C: Integer;
  function _GetKEYCODE_BUTTON_L1: Integer;
  function _GetKEYCODE_BUTTON_L2: Integer;
  function _GetKEYCODE_BUTTON_MODE: Integer;
  function _GetKEYCODE_BUTTON_R1: Integer;
  function _GetKEYCODE_BUTTON_R2: Integer;
  function _GetKEYCODE_BUTTON_SELECT: Integer;
  function _GetKEYCODE_BUTTON_START: Integer;
  function _GetKEYCODE_BUTTON_THUMBL: Integer;
  function _GetKEYCODE_BUTTON_THUMBR: Integer;
  function _GetKEYCODE_BUTTON_X: Integer;
  function _GetKEYCODE_BUTTON_Y: Integer;
  function _GetKEYCODE_BUTTON_Z: Integer;
  function _GetKEYCODE_C: Integer;
  function _GetKEYCODE_CALCULATOR: Integer;
  function _GetKEYCODE_CALENDAR: Integer;
  function _GetKEYCODE_CALL: Integer;
  function _GetKEYCODE_CAMERA: Integer;
  function _GetKEYCODE_CAPS_LOCK: Integer;
  function _GetKEYCODE_CAPTIONS: Integer;
  function _GetKEYCODE_CHANNEL_DOWN: Integer;
  function _GetKEYCODE_CHANNEL_UP: Integer;
  function _GetKEYCODE_CLEAR: Integer;
  function _GetKEYCODE_COMMA: Integer;
  function _GetKEYCODE_CONTACTS: Integer;
  function _GetKEYCODE_CTRL_LEFT: Integer;
  function _GetKEYCODE_CTRL_RIGHT: Integer;
  function _GetKEYCODE_D: Integer;
  function _GetKEYCODE_DEL: Integer;
  function _GetKEYCODE_DPAD_CENTER: Integer;
  function _GetKEYCODE_DPAD_DOWN: Integer;
  function _GetKEYCODE_DPAD_LEFT: Integer;
  function _GetKEYCODE_DPAD_RIGHT: Integer;
  function _GetKEYCODE_DPAD_UP: Integer;
  function _GetKEYCODE_DVR: Integer;
  function _GetKEYCODE_E: Integer;
  function _GetKEYCODE_EISU: Integer;
  function _GetKEYCODE_ENDCALL: Integer;
  function _GetKEYCODE_ENTER: Integer;
  function _GetKEYCODE_ENVELOPE: Integer;
  function _GetKEYCODE_EQUALS: Integer;
  function _GetKEYCODE_ESCAPE: Integer;
  function _GetKEYCODE_EXPLORER: Integer;
  function _GetKEYCODE_F: Integer;
  function _GetKEYCODE_F1: Integer;
  function _GetKEYCODE_F10: Integer;
  function _GetKEYCODE_F11: Integer;
  function _GetKEYCODE_F12: Integer;
  function _GetKEYCODE_F2: Integer;
  function _GetKEYCODE_F3: Integer;
  function _GetKEYCODE_F4: Integer;
  function _GetKEYCODE_F5: Integer;
  function _GetKEYCODE_F6: Integer;
  function _GetKEYCODE_F7: Integer;
  function _GetKEYCODE_F8: Integer;
  function _GetKEYCODE_F9: Integer;
  function _GetKEYCODE_FOCUS: Integer;
  function _GetKEYCODE_FORWARD: Integer;
  function _GetKEYCODE_FORWARD_DEL: Integer;
  function _GetKEYCODE_FUNCTION: Integer;
  function _GetKEYCODE_G: Integer;
  function _GetKEYCODE_GRAVE: Integer;
  function _GetKEYCODE_GUIDE: Integer;
  function _GetKEYCODE_H: Integer;
  function _GetKEYCODE_HEADSETHOOK: Integer;
  function _GetKEYCODE_HENKAN: Integer;
  function _GetKEYCODE_HOME: Integer;
  function _GetKEYCODE_I: Integer;
  function _GetKEYCODE_INFO: Integer;
  function _GetKEYCODE_INSERT: Integer;
  function _GetKEYCODE_J: Integer;
  function _GetKEYCODE_K: Integer;
  function _GetKEYCODE_KANA: Integer;
  function _GetKEYCODE_KATAKANA_HIRAGANA: Integer;
  function _GetKEYCODE_L: Integer;
  function _GetKEYCODE_LANGUAGE_SWITCH: Integer;
  function _GetKEYCODE_LEFT_BRACKET: Integer;
  function _GetKEYCODE_M: Integer;
  function _GetKEYCODE_MANNER_MODE: Integer;
  function _GetKEYCODE_MEDIA_CLOSE: Integer;
  function _GetKEYCODE_MEDIA_EJECT: Integer;
  function _GetKEYCODE_MEDIA_FAST_FORWARD: Integer;
  function _GetKEYCODE_MEDIA_NEXT: Integer;
  function _GetKEYCODE_MEDIA_PAUSE: Integer;
  function _GetKEYCODE_MEDIA_PLAY: Integer;
  function _GetKEYCODE_MEDIA_PLAY_PAUSE: Integer;
  function _GetKEYCODE_MEDIA_PREVIOUS: Integer;
  function _GetKEYCODE_MEDIA_RECORD: Integer;
  function _GetKEYCODE_MEDIA_REWIND: Integer;
  function _GetKEYCODE_MEDIA_STOP: Integer;
  function _GetKEYCODE_MENU: Integer;
  function _GetKEYCODE_META_LEFT: Integer;
  function _GetKEYCODE_META_RIGHT: Integer;
  function _GetKEYCODE_MINUS: Integer;
  function _GetKEYCODE_MOVE_END: Integer;
  function _GetKEYCODE_MOVE_HOME: Integer;
  function _GetKEYCODE_MUHENKAN: Integer;
  function _GetKEYCODE_MUSIC: Integer;
  function _GetKEYCODE_MUTE: Integer;
  function _GetKEYCODE_N: Integer;
  function _GetKEYCODE_NOTIFICATION: Integer;
  function _GetKEYCODE_NUM: Integer;
  function _GetKEYCODE_NUMPAD_0: Integer;
  function _GetKEYCODE_NUMPAD_1: Integer;
  function _GetKEYCODE_NUMPAD_2: Integer;
  function _GetKEYCODE_NUMPAD_3: Integer;
  function _GetKEYCODE_NUMPAD_4: Integer;
  function _GetKEYCODE_NUMPAD_5: Integer;
  function _GetKEYCODE_NUMPAD_6: Integer;
  function _GetKEYCODE_NUMPAD_7: Integer;
  function _GetKEYCODE_NUMPAD_8: Integer;
  function _GetKEYCODE_NUMPAD_9: Integer;
  function _GetKEYCODE_NUMPAD_ADD: Integer;
  function _GetKEYCODE_NUMPAD_COMMA: Integer;
  function _GetKEYCODE_NUMPAD_DIVIDE: Integer;
  function _GetKEYCODE_NUMPAD_DOT: Integer;
  function _GetKEYCODE_NUMPAD_ENTER: Integer;
  function _GetKEYCODE_NUMPAD_EQUALS: Integer;
  function _GetKEYCODE_NUMPAD_LEFT_PAREN: Integer;
  function _GetKEYCODE_NUMPAD_MULTIPLY: Integer;
  function _GetKEYCODE_NUMPAD_RIGHT_PAREN: Integer;
  function _GetKEYCODE_NUMPAD_SUBTRACT: Integer;
  function _GetKEYCODE_NUM_LOCK: Integer;
  function _GetKEYCODE_O: Integer;
  function _GetKEYCODE_P: Integer;
  function _GetKEYCODE_PAGE_DOWN: Integer;
  function _GetKEYCODE_PAGE_UP: Integer;
  function _GetKEYCODE_PERIOD: Integer;
  function _GetKEYCODE_PICTSYMBOLS: Integer;
  function _GetKEYCODE_PLUS: Integer;
  function _GetKEYCODE_POUND: Integer;
  function _GetKEYCODE_POWER: Integer;
  function _GetKEYCODE_PROG_BLUE: Integer;
  function _GetKEYCODE_PROG_GREEN: Integer;
  function _GetKEYCODE_PROG_RED: Integer;
  function _GetKEYCODE_PROG_YELLOW: Integer;
  function _GetKEYCODE_Q: Integer;
  function _GetKEYCODE_R: Integer;
  function _GetKEYCODE_RIGHT_BRACKET: Integer;
  function _GetKEYCODE_RO: Integer;
  function _GetKEYCODE_S: Integer;
  function _GetKEYCODE_SCROLL_LOCK: Integer;
  function _GetKEYCODE_SEARCH: Integer;
  function _GetKEYCODE_SEMICOLON: Integer;
  function _GetKEYCODE_SETTINGS: Integer;
  function _GetKEYCODE_SHIFT_LEFT: Integer;
  function _GetKEYCODE_SHIFT_RIGHT: Integer;
  function _GetKEYCODE_SLASH: Integer;
  function _GetKEYCODE_SOFT_LEFT: Integer;
  function _GetKEYCODE_SOFT_RIGHT: Integer;
  function _GetKEYCODE_SPACE: Integer;
  function _GetKEYCODE_STAR: Integer;
  function _GetKEYCODE_STB_INPUT: Integer;
  function _GetKEYCODE_STB_POWER: Integer;
  function _GetKEYCODE_SWITCH_CHARSET: Integer;
  function _GetKEYCODE_SYM: Integer;
  function _GetKEYCODE_SYSRQ: Integer;
  function _GetKEYCODE_T: Integer;
  function _GetKEYCODE_TAB: Integer;
  function _GetKEYCODE_TV: Integer;
  function _GetKEYCODE_TV_INPUT: Integer;
  function _GetKEYCODE_TV_POWER: Integer;
  function _GetKEYCODE_U: Integer;
  function _GetKEYCODE_UNKNOWN: Integer;
  function _GetKEYCODE_V: Integer;
  function _GetKEYCODE_VOLUME_DOWN: Integer;
  function _GetKEYCODE_VOLUME_MUTE: Integer;
  function _GetKEYCODE_VOLUME_UP: Integer;
  function _GetKEYCODE_W: Integer;
  function _GetKEYCODE_WINDOW: Integer;
  function _GetKEYCODE_X: Integer;
  function _GetKEYCODE_Y: Integer;
  function _GetKEYCODE_YEN: Integer;
  function _GetKEYCODE_Z: Integer;
  function _GetKEYCODE_ZENKAKU_HANKAKU: Integer;
  function _GetKEYCODE_ZOOM_IN: Integer;
  function _GetKEYCODE_ZOOM_OUT: Integer;
  function _GetMAX_KEYCODE: Integer;
  function _GetMETA_ALT_LEFT_ON: Integer;
  function _GetMETA_ALT_MASK: Integer;
  function _GetMETA_ALT_ON: Integer;
  function _GetMETA_ALT_RIGHT_ON: Integer;
  function _GetMETA_CAPS_LOCK_ON: Integer;
  function _GetMETA_CTRL_LEFT_ON: Integer;
  function _GetMETA_CTRL_MASK: Integer;
  function _GetMETA_CTRL_ON: Integer;
  function _GetMETA_CTRL_RIGHT_ON: Integer;
  function _GetMETA_FUNCTION_ON: Integer;
  function _GetMETA_META_LEFT_ON: Integer;
  function _GetMETA_META_MASK: Integer;
  function _GetMETA_META_ON: Integer;
  function _GetMETA_META_RIGHT_ON: Integer;
  function _GetMETA_NUM_LOCK_ON: Integer;
  function _GetMETA_SCROLL_LOCK_ON: Integer;
  function _GetMETA_SHIFT_LEFT_ON: Integer;
  function _GetMETA_SHIFT_MASK: Integer;
  function _GetMETA_SHIFT_ON: Integer;
  function _GetMETA_SHIFT_RIGHT_ON: Integer;
  function _GetMETA_SYM_ON: Integer;
  {Methods}
  function init(action: Integer; code: Integer): JKeyEvent; cdecl; overload;
  function init(downTime: Int64; eventTime: Int64; action: Integer; code: Integer; repeat_: Integer): JKeyEvent; cdecl; overload;
  function init(downTime: Int64; eventTime: Int64; action: Integer; code: Integer; repeat_: Integer; metaState: Integer): JKeyEvent; cdecl; overload;
  function init(downTime: Int64; eventTime: Int64; action: Integer; code: Integer; repeat_: Integer; metaState: Integer; deviceId: Integer; scancode: Integer): JKeyEvent; cdecl; overload;
  function init(downTime: Int64; eventTime: Int64; action: Integer; code: Integer; repeat_: Integer; metaState: Integer; deviceId: Integer; scancode: Integer; flags: Integer): JKeyEvent; cdecl; overload;
  function init(downTime: Int64; eventTime: Int64; action: Integer; code: Integer; repeat_: Integer; metaState: Integer; deviceId: Integer; scancode: Integer; flags: Integer; source: Integer): JKeyEvent; cdecl; overload;
  function init(time: Int64; characters: JString; deviceId: Integer; flags: Integer): JKeyEvent; cdecl; overload;
  function init(origEvent: JKeyEvent): JKeyEvent; cdecl; overload;
  function init(origEvent: JKeyEvent; eventTime: Int64; newRepeat: Integer): JKeyEvent; cdecl; overload;//Deprecated
  function changeAction(event: JKeyEvent; action: Integer): JKeyEvent; cdecl;
  function changeFlags(event: JKeyEvent; flags: Integer): JKeyEvent; cdecl;
  function changeTimeRepeat(event: JKeyEvent; eventTime: Int64; newRepeat: Integer): JKeyEvent; cdecl; overload;
  function changeTimeRepeat(event: JKeyEvent; eventTime: Int64; newRepeat: Integer; newFlags: Integer): JKeyEvent; cdecl; overload;
  function getDeadChar(accent: Integer; c: Integer): Integer; cdecl;
  function getMaxKeyCode: Integer; cdecl;
  function getModifierMetaStateMask: Integer; cdecl;
  function isGamepadButton(keyCode: Integer): Boolean; cdecl;
  function isModifierKey(keyCode: Integer): Boolean; cdecl;
  function keyCodeFromString(symbolicName: JString): Integer; cdecl;
  function keyCodeToString(keyCode: Integer): JString; cdecl;
  function metaStateHasModifiers(metaState: Integer; modifiers: Integer): Boolean; cdecl;
  function metaStateHasNoModifiers(metaState: Integer): Boolean; cdecl;
  function normalizeMetaState(metaState: Integer): Integer; cdecl;
  {Properties}
  property ACTION_DOWN: Integer read _GetACTION_DOWN;
  property ACTION_MULTIPLE: Integer read _GetACTION_MULTIPLE;
  property ACTION_UP: Integer read _GetACTION_UP;
{CREATOR is defined in parent interface}
  property FLAG_CANCELED: Integer read _GetFLAG_CANCELED;
  property FLAG_CANCELED_LONG_PRESS: Integer read _GetFLAG_CANCELED_LONG_PRESS;
  property FLAG_EDITOR_ACTION: Integer read _GetFLAG_EDITOR_ACTION;
  property FLAG_FALLBACK: Integer read _GetFLAG_FALLBACK;
  property FLAG_FROM_SYSTEM: Integer read _GetFLAG_FROM_SYSTEM;
  property FLAG_KEEP_TOUCH_MODE: Integer read _GetFLAG_KEEP_TOUCH_MODE;
  property FLAG_LONG_PRESS: Integer read _GetFLAG_LONG_PRESS;
  property FLAG_SOFT_KEYBOARD: Integer read _GetFLAG_SOFT_KEYBOARD;
  property FLAG_TRACKING: Integer read _GetFLAG_TRACKING;
  property FLAG_VIRTUAL_HARD_KEY: Integer read _GetFLAG_VIRTUAL_HARD_KEY;
  property FLAG_WOKE_HERE: Integer read _GetFLAG_WOKE_HERE;
  property KEYCODE_0: Integer read _GetKEYCODE_0;
  property KEYCODE_1: Integer read _GetKEYCODE_1;
  property KEYCODE_2: Integer read _GetKEYCODE_2;
  property KEYCODE_3: Integer read _GetKEYCODE_3;
  property KEYCODE_3D_MODE: Integer read _GetKEYCODE_3D_MODE;
  property KEYCODE_4: Integer read _GetKEYCODE_4;
  property KEYCODE_5: Integer read _GetKEYCODE_5;
  property KEYCODE_6: Integer read _GetKEYCODE_6;
  property KEYCODE_7: Integer read _GetKEYCODE_7;
  property KEYCODE_8: Integer read _GetKEYCODE_8;
  property KEYCODE_9: Integer read _GetKEYCODE_9;
  property KEYCODE_A: Integer read _GetKEYCODE_A;
  property KEYCODE_ALT_LEFT: Integer read _GetKEYCODE_ALT_LEFT;
  property KEYCODE_ALT_RIGHT: Integer read _GetKEYCODE_ALT_RIGHT;
  property KEYCODE_APOSTROPHE: Integer read _GetKEYCODE_APOSTROPHE;
  property KEYCODE_APP_SWITCH: Integer read _GetKEYCODE_APP_SWITCH;
  property KEYCODE_ASSIST: Integer read _GetKEYCODE_ASSIST;
  property KEYCODE_AT: Integer read _GetKEYCODE_AT;
  property KEYCODE_AVR_INPUT: Integer read _GetKEYCODE_AVR_INPUT;
  property KEYCODE_AVR_POWER: Integer read _GetKEYCODE_AVR_POWER;
  property KEYCODE_B: Integer read _GetKEYCODE_B;
  property KEYCODE_BACK: Integer read _GetKEYCODE_BACK;
  property KEYCODE_BACKSLASH: Integer read _GetKEYCODE_BACKSLASH;
  property KEYCODE_BOOKMARK: Integer read _GetKEYCODE_BOOKMARK;
  property KEYCODE_BREAK: Integer read _GetKEYCODE_BREAK;
  property KEYCODE_BUTTON_1: Integer read _GetKEYCODE_BUTTON_1;
  property KEYCODE_BUTTON_10: Integer read _GetKEYCODE_BUTTON_10;
  property KEYCODE_BUTTON_11: Integer read _GetKEYCODE_BUTTON_11;
  property KEYCODE_BUTTON_12: Integer read _GetKEYCODE_BUTTON_12;
  property KEYCODE_BUTTON_13: Integer read _GetKEYCODE_BUTTON_13;
  property KEYCODE_BUTTON_14: Integer read _GetKEYCODE_BUTTON_14;
  property KEYCODE_BUTTON_15: Integer read _GetKEYCODE_BUTTON_15;
  property KEYCODE_BUTTON_16: Integer read _GetKEYCODE_BUTTON_16;
  property KEYCODE_BUTTON_2: Integer read _GetKEYCODE_BUTTON_2;
  property KEYCODE_BUTTON_3: Integer read _GetKEYCODE_BUTTON_3;
  property KEYCODE_BUTTON_4: Integer read _GetKEYCODE_BUTTON_4;
  property KEYCODE_BUTTON_5: Integer read _GetKEYCODE_BUTTON_5;
  property KEYCODE_BUTTON_6: Integer read _GetKEYCODE_BUTTON_6;
  property KEYCODE_BUTTON_7: Integer read _GetKEYCODE_BUTTON_7;
  property KEYCODE_BUTTON_8: Integer read _GetKEYCODE_BUTTON_8;
  property KEYCODE_BUTTON_9: Integer read _GetKEYCODE_BUTTON_9;
  property KEYCODE_BUTTON_A: Integer read _GetKEYCODE_BUTTON_A;
  property KEYCODE_BUTTON_B: Integer read _GetKEYCODE_BUTTON_B;
  property KEYCODE_BUTTON_C: Integer read _GetKEYCODE_BUTTON_C;
  property KEYCODE_BUTTON_L1: Integer read _GetKEYCODE_BUTTON_L1;
  property KEYCODE_BUTTON_L2: Integer read _GetKEYCODE_BUTTON_L2;
  property KEYCODE_BUTTON_MODE: Integer read _GetKEYCODE_BUTTON_MODE;
  property KEYCODE_BUTTON_R1: Integer read _GetKEYCODE_BUTTON_R1;
  property KEYCODE_BUTTON_R2: Integer read _GetKEYCODE_BUTTON_R2;
  property KEYCODE_BUTTON_SELECT: Integer read _GetKEYCODE_BUTTON_SELECT;
  property KEYCODE_BUTTON_START: Integer read _GetKEYCODE_BUTTON_START;
  property KEYCODE_BUTTON_THUMBL: Integer read _GetKEYCODE_BUTTON_THUMBL;
  property KEYCODE_BUTTON_THUMBR: Integer read _GetKEYCODE_BUTTON_THUMBR;
  property KEYCODE_BUTTON_X: Integer read _GetKEYCODE_BUTTON_X;
  property KEYCODE_BUTTON_Y: Integer read _GetKEYCODE_BUTTON_Y;
  property KEYCODE_BUTTON_Z: Integer read _GetKEYCODE_BUTTON_Z;
  property KEYCODE_C: Integer read _GetKEYCODE_C;
  property KEYCODE_CALCULATOR: Integer read _GetKEYCODE_CALCULATOR;
  property KEYCODE_CALENDAR: Integer read _GetKEYCODE_CALENDAR;
  property KEYCODE_CALL: Integer read _GetKEYCODE_CALL;
  property KEYCODE_CAMERA: Integer read _GetKEYCODE_CAMERA;
  property KEYCODE_CAPS_LOCK: Integer read _GetKEYCODE_CAPS_LOCK;
  property KEYCODE_CAPTIONS: Integer read _GetKEYCODE_CAPTIONS;
  property KEYCODE_CHANNEL_DOWN: Integer read _GetKEYCODE_CHANNEL_DOWN;
  property KEYCODE_CHANNEL_UP: Integer read _GetKEYCODE_CHANNEL_UP;
  property KEYCODE_CLEAR: Integer read _GetKEYCODE_CLEAR;
  property KEYCODE_COMMA: Integer read _GetKEYCODE_COMMA;
  property KEYCODE_CONTACTS: Integer read _GetKEYCODE_CONTACTS;
  property KEYCODE_CTRL_LEFT: Integer read _GetKEYCODE_CTRL_LEFT;
  property KEYCODE_CTRL_RIGHT: Integer read _GetKEYCODE_CTRL_RIGHT;
  property KEYCODE_D: Integer read _GetKEYCODE_D;
  property KEYCODE_DEL: Integer read _GetKEYCODE_DEL;
  property KEYCODE_DPAD_CENTER: Integer read _GetKEYCODE_DPAD_CENTER;
  property KEYCODE_DPAD_DOWN: Integer read _GetKEYCODE_DPAD_DOWN;
  property KEYCODE_DPAD_LEFT: Integer read _GetKEYCODE_DPAD_LEFT;
  property KEYCODE_DPAD_RIGHT: Integer read _GetKEYCODE_DPAD_RIGHT;
  property KEYCODE_DPAD_UP: Integer read _GetKEYCODE_DPAD_UP;
  property KEYCODE_DVR: Integer read _GetKEYCODE_DVR;
  property KEYCODE_E: Integer read _GetKEYCODE_E;
  property KEYCODE_EISU: Integer read _GetKEYCODE_EISU;
  property KEYCODE_ENDCALL: Integer read _GetKEYCODE_ENDCALL;
  property KEYCODE_ENTER: Integer read _GetKEYCODE_ENTER;
  property KEYCODE_ENVELOPE: Integer read _GetKEYCODE_ENVELOPE;
  property KEYCODE_EQUALS: Integer read _GetKEYCODE_EQUALS;
  property KEYCODE_ESCAPE: Integer read _GetKEYCODE_ESCAPE;
  property KEYCODE_EXPLORER: Integer read _GetKEYCODE_EXPLORER;
  property KEYCODE_F: Integer read _GetKEYCODE_F;
  property KEYCODE_F1: Integer read _GetKEYCODE_F1;
  property KEYCODE_F10: Integer read _GetKEYCODE_F10;
  property KEYCODE_F11: Integer read _GetKEYCODE_F11;
  property KEYCODE_F12: Integer read _GetKEYCODE_F12;
  property KEYCODE_F2: Integer read _GetKEYCODE_F2;
  property KEYCODE_F3: Integer read _GetKEYCODE_F3;
  property KEYCODE_F4: Integer read _GetKEYCODE_F4;
  property KEYCODE_F5: Integer read _GetKEYCODE_F5;
  property KEYCODE_F6: Integer read _GetKEYCODE_F6;
  property KEYCODE_F7: Integer read _GetKEYCODE_F7;
  property KEYCODE_F8: Integer read _GetKEYCODE_F8;
  property KEYCODE_F9: Integer read _GetKEYCODE_F9;
  property KEYCODE_FOCUS: Integer read _GetKEYCODE_FOCUS;
  property KEYCODE_FORWARD: Integer read _GetKEYCODE_FORWARD;
  property KEYCODE_FORWARD_DEL: Integer read _GetKEYCODE_FORWARD_DEL;
  property KEYCODE_FUNCTION: Integer read _GetKEYCODE_FUNCTION;
  property KEYCODE_G: Integer read _GetKEYCODE_G;
  property KEYCODE_GRAVE: Integer read _GetKEYCODE_GRAVE;
  property KEYCODE_GUIDE: Integer read _GetKEYCODE_GUIDE;
  property KEYCODE_H: Integer read _GetKEYCODE_H;
  property KEYCODE_HEADSETHOOK: Integer read _GetKEYCODE_HEADSETHOOK;
  property KEYCODE_HENKAN: Integer read _GetKEYCODE_HENKAN;
  property KEYCODE_HOME: Integer read _GetKEYCODE_HOME;
  property KEYCODE_I: Integer read _GetKEYCODE_I;
  property KEYCODE_INFO: Integer read _GetKEYCODE_INFO;
  property KEYCODE_INSERT: Integer read _GetKEYCODE_INSERT;
  property KEYCODE_J: Integer read _GetKEYCODE_J;
  property KEYCODE_K: Integer read _GetKEYCODE_K;
  property KEYCODE_KANA: Integer read _GetKEYCODE_KANA;
  property KEYCODE_KATAKANA_HIRAGANA: Integer read _GetKEYCODE_KATAKANA_HIRAGANA;
  property KEYCODE_L: Integer read _GetKEYCODE_L;
  property KEYCODE_LANGUAGE_SWITCH: Integer read _GetKEYCODE_LANGUAGE_SWITCH;
  property KEYCODE_LEFT_BRACKET: Integer read _GetKEYCODE_LEFT_BRACKET;
  property KEYCODE_M: Integer read _GetKEYCODE_M;
  property KEYCODE_MANNER_MODE: Integer read _GetKEYCODE_MANNER_MODE;
  property KEYCODE_MEDIA_CLOSE: Integer read _GetKEYCODE_MEDIA_CLOSE;
  property KEYCODE_MEDIA_EJECT: Integer read _GetKEYCODE_MEDIA_EJECT;
  property KEYCODE_MEDIA_FAST_FORWARD: Integer read _GetKEYCODE_MEDIA_FAST_FORWARD;
  property KEYCODE_MEDIA_NEXT: Integer read _GetKEYCODE_MEDIA_NEXT;
  property KEYCODE_MEDIA_PAUSE: Integer read _GetKEYCODE_MEDIA_PAUSE;
  property KEYCODE_MEDIA_PLAY: Integer read _GetKEYCODE_MEDIA_PLAY;
  property KEYCODE_MEDIA_PLAY_PAUSE: Integer read _GetKEYCODE_MEDIA_PLAY_PAUSE;
  property KEYCODE_MEDIA_PREVIOUS: Integer read _GetKEYCODE_MEDIA_PREVIOUS;
  property KEYCODE_MEDIA_RECORD: Integer read _GetKEYCODE_MEDIA_RECORD;
  property KEYCODE_MEDIA_REWIND: Integer read _GetKEYCODE_MEDIA_REWIND;
  property KEYCODE_MEDIA_STOP: Integer read _GetKEYCODE_MEDIA_STOP;
  property KEYCODE_MENU: Integer read _GetKEYCODE_MENU;
  property KEYCODE_META_LEFT: Integer read _GetKEYCODE_META_LEFT;
  property KEYCODE_META_RIGHT: Integer read _GetKEYCODE_META_RIGHT;
  property KEYCODE_MINUS: Integer read _GetKEYCODE_MINUS;
  property KEYCODE_MOVE_END: Integer read _GetKEYCODE_MOVE_END;
  property KEYCODE_MOVE_HOME: Integer read _GetKEYCODE_MOVE_HOME;
  property KEYCODE_MUHENKAN: Integer read _GetKEYCODE_MUHENKAN;
  property KEYCODE_MUSIC: Integer read _GetKEYCODE_MUSIC;
  property KEYCODE_MUTE: Integer read _GetKEYCODE_MUTE;
  property KEYCODE_N: Integer read _GetKEYCODE_N;
  property KEYCODE_NOTIFICATION: Integer read _GetKEYCODE_NOTIFICATION;
  property KEYCODE_NUM: Integer read _GetKEYCODE_NUM;
  property KEYCODE_NUMPAD_0: Integer read _GetKEYCODE_NUMPAD_0;
  property KEYCODE_NUMPAD_1: Integer read _GetKEYCODE_NUMPAD_1;
  property KEYCODE_NUMPAD_2: Integer read _GetKEYCODE_NUMPAD_2;
  property KEYCODE_NUMPAD_3: Integer read _GetKEYCODE_NUMPAD_3;
  property KEYCODE_NUMPAD_4: Integer read _GetKEYCODE_NUMPAD_4;
  property KEYCODE_NUMPAD_5: Integer read _GetKEYCODE_NUMPAD_5;
  property KEYCODE_NUMPAD_6: Integer read _GetKEYCODE_NUMPAD_6;
  property KEYCODE_NUMPAD_7: Integer read _GetKEYCODE_NUMPAD_7;
  property KEYCODE_NUMPAD_8: Integer read _GetKEYCODE_NUMPAD_8;
  property KEYCODE_NUMPAD_9: Integer read _GetKEYCODE_NUMPAD_9;
  property KEYCODE_NUMPAD_ADD: Integer read _GetKEYCODE_NUMPAD_ADD;
  property KEYCODE_NUMPAD_COMMA: Integer read _GetKEYCODE_NUMPAD_COMMA;
  property KEYCODE_NUMPAD_DIVIDE: Integer read _GetKEYCODE_NUMPAD_DIVIDE;
  property KEYCODE_NUMPAD_DOT: Integer read _GetKEYCODE_NUMPAD_DOT;
  property KEYCODE_NUMPAD_ENTER: Integer read _GetKEYCODE_NUMPAD_ENTER;
  property KEYCODE_NUMPAD_EQUALS: Integer read _GetKEYCODE_NUMPAD_EQUALS;
  property KEYCODE_NUMPAD_LEFT_PAREN: Integer read _GetKEYCODE_NUMPAD_LEFT_PAREN;
  property KEYCODE_NUMPAD_MULTIPLY: Integer read _GetKEYCODE_NUMPAD_MULTIPLY;
  property KEYCODE_NUMPAD_RIGHT_PAREN: Integer read _GetKEYCODE_NUMPAD_RIGHT_PAREN;
  property KEYCODE_NUMPAD_SUBTRACT: Integer read _GetKEYCODE_NUMPAD_SUBTRACT;
  property KEYCODE_NUM_LOCK: Integer read _GetKEYCODE_NUM_LOCK;
  property KEYCODE_O: Integer read _GetKEYCODE_O;
  property KEYCODE_P: Integer read _GetKEYCODE_P;
  property KEYCODE_PAGE_DOWN: Integer read _GetKEYCODE_PAGE_DOWN;
  property KEYCODE_PAGE_UP: Integer read _GetKEYCODE_PAGE_UP;
  property KEYCODE_PERIOD: Integer read _GetKEYCODE_PERIOD;
  property KEYCODE_PICTSYMBOLS: Integer read _GetKEYCODE_PICTSYMBOLS;
  property KEYCODE_PLUS: Integer read _GetKEYCODE_PLUS;
  property KEYCODE_POUND: Integer read _GetKEYCODE_POUND;
  property KEYCODE_POWER: Integer read _GetKEYCODE_POWER;
  property KEYCODE_PROG_BLUE: Integer read _GetKEYCODE_PROG_BLUE;
  property KEYCODE_PROG_GREEN: Integer read _GetKEYCODE_PROG_GREEN;
  property KEYCODE_PROG_RED: Integer read _GetKEYCODE_PROG_RED;
  property KEYCODE_PROG_YELLOW: Integer read _GetKEYCODE_PROG_YELLOW;
  property KEYCODE_Q: Integer read _GetKEYCODE_Q;
  property KEYCODE_R: Integer read _GetKEYCODE_R;
  property KEYCODE_RIGHT_BRACKET: Integer read _GetKEYCODE_RIGHT_BRACKET;
  property KEYCODE_RO: Integer read _GetKEYCODE_RO;
  property KEYCODE_S: Integer read _GetKEYCODE_S;
  property KEYCODE_SCROLL_LOCK: Integer read _GetKEYCODE_SCROLL_LOCK;
  property KEYCODE_SEARCH: Integer read _GetKEYCODE_SEARCH;
  property KEYCODE_SEMICOLON: Integer read _GetKEYCODE_SEMICOLON;
  property KEYCODE_SETTINGS: Integer read _GetKEYCODE_SETTINGS;
  property KEYCODE_SHIFT_LEFT: Integer read _GetKEYCODE_SHIFT_LEFT;
  property KEYCODE_SHIFT_RIGHT: Integer read _GetKEYCODE_SHIFT_RIGHT;
  property KEYCODE_SLASH: Integer read _GetKEYCODE_SLASH;
  property KEYCODE_SOFT_LEFT: Integer read _GetKEYCODE_SOFT_LEFT;
  property KEYCODE_SOFT_RIGHT: Integer read _GetKEYCODE_SOFT_RIGHT;
  property KEYCODE_SPACE: Integer read _GetKEYCODE_SPACE;
  property KEYCODE_STAR: Integer read _GetKEYCODE_STAR;
  property KEYCODE_STB_INPUT: Integer read _GetKEYCODE_STB_INPUT;
  property KEYCODE_STB_POWER: Integer read _GetKEYCODE_STB_POWER;
  property KEYCODE_SWITCH_CHARSET: Integer read _GetKEYCODE_SWITCH_CHARSET;
  property KEYCODE_SYM: Integer read _GetKEYCODE_SYM;
  property KEYCODE_SYSRQ: Integer read _GetKEYCODE_SYSRQ;
  property KEYCODE_T: Integer read _GetKEYCODE_T;
  property KEYCODE_TAB: Integer read _GetKEYCODE_TAB;
  property KEYCODE_TV: Integer read _GetKEYCODE_TV;
  property KEYCODE_TV_INPUT: Integer read _GetKEYCODE_TV_INPUT;
  property KEYCODE_TV_POWER: Integer read _GetKEYCODE_TV_POWER;
  property KEYCODE_U: Integer read _GetKEYCODE_U;
  property KEYCODE_UNKNOWN: Integer read _GetKEYCODE_UNKNOWN;
  property KEYCODE_V: Integer read _GetKEYCODE_V;
  property KEYCODE_VOLUME_DOWN: Integer read _GetKEYCODE_VOLUME_DOWN;
  property KEYCODE_VOLUME_MUTE: Integer read _GetKEYCODE_VOLUME_MUTE;
  property KEYCODE_VOLUME_UP: Integer read _GetKEYCODE_VOLUME_UP;
  property KEYCODE_W: Integer read _GetKEYCODE_W;
  property KEYCODE_WINDOW: Integer read _GetKEYCODE_WINDOW;
  property KEYCODE_X: Integer read _GetKEYCODE_X;
  property KEYCODE_Y: Integer read _GetKEYCODE_Y;
  property KEYCODE_YEN: Integer read _GetKEYCODE_YEN;
  property KEYCODE_Z: Integer read _GetKEYCODE_Z;
  property KEYCODE_ZENKAKU_HANKAKU: Integer read _GetKEYCODE_ZENKAKU_HANKAKU;
  property KEYCODE_ZOOM_IN: Integer read _GetKEYCODE_ZOOM_IN;
  property KEYCODE_ZOOM_OUT: Integer read _GetKEYCODE_ZOOM_OUT;
  property MAX_KEYCODE: Integer read _GetMAX_KEYCODE;
  property META_ALT_LEFT_ON: Integer read _GetMETA_ALT_LEFT_ON;
  property META_ALT_MASK: Integer read _GetMETA_ALT_MASK;
  property META_ALT_ON: Integer read _GetMETA_ALT_ON;
  property META_ALT_RIGHT_ON: Integer read _GetMETA_ALT_RIGHT_ON;
  property META_CAPS_LOCK_ON: Integer read _GetMETA_CAPS_LOCK_ON;
  property META_CTRL_LEFT_ON: Integer read _GetMETA_CTRL_LEFT_ON;
  property META_CTRL_MASK: Integer read _GetMETA_CTRL_MASK;
  property META_CTRL_ON: Integer read _GetMETA_CTRL_ON;
  property META_CTRL_RIGHT_ON: Integer read _GetMETA_CTRL_RIGHT_ON;
  property META_FUNCTION_ON: Integer read _GetMETA_FUNCTION_ON;
  property META_META_LEFT_ON: Integer read _GetMETA_META_LEFT_ON;
  property META_META_MASK: Integer read _GetMETA_META_MASK;
  property META_META_ON: Integer read _GetMETA_META_ON;
  property META_META_RIGHT_ON: Integer read _GetMETA_META_RIGHT_ON;
  property META_NUM_LOCK_ON: Integer read _GetMETA_NUM_LOCK_ON;
  property META_SCROLL_LOCK_ON: Integer read _GetMETA_SCROLL_LOCK_ON;
  property META_SHIFT_LEFT_ON: Integer read _GetMETA_SHIFT_LEFT_ON;
  property META_SHIFT_MASK: Integer read _GetMETA_SHIFT_MASK;
  property META_SHIFT_ON: Integer read _GetMETA_SHIFT_ON;
  property META_SHIFT_RIGHT_ON: Integer read _GetMETA_SHIFT_RIGHT_ON;
  property META_SYM_ON: Integer read _GetMETA_SYM_ON;
end;

[JavaSignature('android/view/KeyEvent')]
JKeyEvent = interface(JInputEvent)
['{EB2AEB24-3B12-4FA3-8D94-636EE7A15435}']
  {Methods}
  function dispatch(receiver: JKeyEvent_Callback): Boolean; cdecl; overload;//Deprecated
  function dispatch(receiver: JKeyEvent_Callback; state: JKeyEvent_DispatcherState; target: JObject): Boolean; cdecl; overload;
  function getAction: Integer; cdecl;
  function getCharacters: JString; cdecl;
  function getDeviceId: Integer; cdecl;
  function getDisplayLabel: Char; cdecl;
  function getDownTime: Int64; cdecl;
  function getEventTime: Int64; cdecl;
  function getFlags: Integer; cdecl;
  function getKeyCharacterMap: JKeyCharacterMap; cdecl;
  function getKeyCode: Integer; cdecl;
  function getKeyData(results: JKeyCharacterMap_KeyData): Boolean; cdecl;//Deprecated
  function getMatch(chars: TJavaArray<Char>): Char; cdecl; overload;
  function getMatch(chars: TJavaArray<Char>; metaState: Integer): Char; cdecl; overload;
  function getMetaState: Integer; cdecl;
  function getModifiers: Integer; cdecl;
  function getNumber: Char; cdecl;
  function getRepeatCount: Integer; cdecl;
  function getScanCode: Integer; cdecl;
  function getSource: Integer; cdecl;
  function getUnicodeChar: Integer; cdecl; overload;
  function getUnicodeChar(metaState: Integer): Integer; cdecl; overload;
  function hasModifiers(modifiers: Integer): Boolean; cdecl;
  function hasNoModifiers: Boolean; cdecl;
  function isAltPressed: Boolean; cdecl;
  function isCanceled: Boolean; cdecl;
  function isCapsLockOn: Boolean; cdecl;
  function isCtrlPressed: Boolean; cdecl;
  function isFunctionPressed: Boolean; cdecl;
  function isLongPress: Boolean; cdecl;
  function isMetaPressed: Boolean; cdecl;
  function isNumLockOn: Boolean; cdecl;
  function isPrintingKey: Boolean; cdecl;
  function isScrollLockOn: Boolean; cdecl;
  function isShiftPressed: Boolean; cdecl;
  function isSymPressed: Boolean; cdecl;
  function isSystem: Boolean; cdecl;
  function isTracking: Boolean; cdecl;
  procedure setSource(source: Integer); cdecl;
  procedure startTracking; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
end;
TJKeyEvent = class(TJavaGenericImport<JKeyEventClass, JKeyEvent>) end;

JCursorClass = interface(JCloseableClass)
['{BBCDC870-9E56-4704-94E9-B06FA2B6E945}']
  {Property Methods}
  function _GetFIELD_TYPE_BLOB: Integer;
  function _GetFIELD_TYPE_FLOAT: Integer;
  function _GetFIELD_TYPE_INTEGER: Integer;
  function _GetFIELD_TYPE_NULL: Integer;
  function _GetFIELD_TYPE_STRING: Integer;
  {Properties}
  property FIELD_TYPE_BLOB: Integer read _GetFIELD_TYPE_BLOB;
  property FIELD_TYPE_FLOAT: Integer read _GetFIELD_TYPE_FLOAT;
  property FIELD_TYPE_INTEGER: Integer read _GetFIELD_TYPE_INTEGER;
  property FIELD_TYPE_NULL: Integer read _GetFIELD_TYPE_NULL;
  property FIELD_TYPE_STRING: Integer read _GetFIELD_TYPE_STRING;
end;

[JavaSignature('android/database/Cursor')]
JCursor = interface(JCloseable)
['{5AAF735F-9251-4E0B-950B-A67F2E27F44C}']
  {Methods}
  procedure close; cdecl;
  procedure copyStringToBuffer(columnIndex: Integer; buffer: Jdatabase_CharArrayBuffer); cdecl;
  procedure deactivate; cdecl;//Deprecated
  function getBlob(columnIndex: Integer): TJavaArray<Byte>; cdecl;
  function getColumnCount: Integer; cdecl;
  function getColumnIndex(columnName: JString): Integer; cdecl;
  function getColumnIndexOrThrow(columnName: JString): Integer; cdecl;
  function getColumnName(columnIndex: Integer): JString; cdecl;
  function getColumnNames: TJavaObjectArray<JString>; cdecl;
  function getCount: Integer; cdecl;
  function getDouble(columnIndex: Integer): Double; cdecl;
  function getExtras: JBundle; cdecl;
  function getFloat(columnIndex: Integer): Single; cdecl;
  function getInt(columnIndex: Integer): Integer; cdecl;
  function getLong(columnIndex: Integer): Int64; cdecl;
  function getPosition: Integer; cdecl;
  function getShort(columnIndex: Integer): SmallInt; cdecl;
  function getString(columnIndex: Integer): JString; cdecl;
  function getType(columnIndex: Integer): Integer; cdecl;
  function getWantsAllOnMoveCalls: Boolean; cdecl;
  function isAfterLast: Boolean; cdecl;
  function isBeforeFirst: Boolean; cdecl;
  function isClosed: Boolean; cdecl;
  function isFirst: Boolean; cdecl;
  function isLast: Boolean; cdecl;
  function isNull(columnIndex: Integer): Boolean; cdecl;
  function move(offset: Integer): Boolean; cdecl;
  function moveToFirst: Boolean; cdecl;
  function moveToLast: Boolean; cdecl;
  function moveToNext: Boolean; cdecl;
  function moveToPosition(position: Integer): Boolean; cdecl;
  function moveToPrevious: Boolean; cdecl;
  procedure registerContentObserver(observer: JContentObserver); cdecl;
  procedure registerDataSetObserver(observer: JDataSetObserver); cdecl;
  function requery: Boolean; cdecl;//Deprecated
  function respond(extras: JBundle): JBundle; cdecl;
  procedure setNotificationUri(cr: JContentResolver; uri: Jnet_Uri); cdecl;
  procedure unregisterContentObserver(observer: JContentObserver); cdecl;
  procedure unregisterDataSetObserver(observer: JDataSetObserver); cdecl;
end;
TJCursor = class(TJavaGenericImport<JCursorClass, JCursor>) end;

JCursorLoaderClass = interface(JAsyncTaskLoaderClass)
['{C1736040-F484-4E56-9453-35EC4565E16F}']
  {Methods}
  function init(context: JContext): JCursorLoader; cdecl; overload;
  function init(context: JContext; uri: Jnet_Uri; projection: TJavaObjectArray<JString>; selection: JString; selectionArgs: TJavaObjectArray<JString>; sortOrder: JString): JCursorLoader; cdecl; overload;
end;

[JavaSignature('android/content/CursorLoader')]
JCursorLoader = interface(JAsyncTaskLoader)
['{BCEB8495-484F-44BA-9048-1D337963C928}']
  {Methods}
  procedure cancelLoadInBackground; cdecl;
  procedure deliverResult(cursor: JCursor); cdecl;
  procedure dump(prefix: JString; fd: JFileDescriptor; writer: JPrintWriter; args: TJavaObjectArray<JString>); cdecl;
  function getProjection: TJavaObjectArray<JString>; cdecl;
  function getSelection: JString; cdecl;
  function getSelectionArgs: TJavaObjectArray<JString>; cdecl;
  function getSortOrder: JString; cdecl;
  function getUri: Jnet_Uri; cdecl;
  function loadInBackground: JCursor; cdecl;
  procedure onCanceled(cursor: JCursor); cdecl;
  procedure setProjection(projection: TJavaObjectArray<JString>); cdecl;
  procedure setSelection(selection: JString); cdecl;
  procedure setSelectionArgs(selectionArgs: TJavaObjectArray<JString>); cdecl;
  procedure setSortOrder(sortOrder: JString); cdecl;
  procedure setUri(uri: Jnet_Uri); cdecl;
end;
TJCursorLoader = class(TJavaGenericImport<JCursorLoaderClass, JCursorLoader>) end;

JClipboardManager_OnPrimaryClipChangedListenerClass = interface(IJavaClass)
['{3EC09394-C7F1-4DB3-A7C1-F9D274F8DEEC}']
end;

[JavaSignature('android/content/ClipboardManager$OnPrimaryClipChangedListener')]
JClipboardManager_OnPrimaryClipChangedListener = interface(IJavaInstance)
['{A4F0CC5D-0887-4114-A38E-160C75FD198F}']
  {Methods}
  procedure onPrimaryClipChanged; cdecl;
end;
TJClipboardManager_OnPrimaryClipChangedListener = class(TJavaGenericImport<JClipboardManager_OnPrimaryClipChangedListenerClass, JClipboardManager_OnPrimaryClipChangedListener>) end;

JServiceInfoClass = interface(JComponentInfoClass)
['{AC714975-FAB2-45A1-A076-1652016BBEED}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  function _GetFLAG_ISOLATED_PROCESS: Integer;
  function _GetFLAG_SINGLE_USER: Integer;
  function _GetFLAG_STOP_WITH_TASK: Integer;
  {Methods}
  function init: JServiceInfo; cdecl; overload;
  function init(orig: JServiceInfo): JServiceInfo; cdecl; overload;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
  property FLAG_ISOLATED_PROCESS: Integer read _GetFLAG_ISOLATED_PROCESS;
  property FLAG_SINGLE_USER: Integer read _GetFLAG_SINGLE_USER;
  property FLAG_STOP_WITH_TASK: Integer read _GetFLAG_STOP_WITH_TASK;
end;

[JavaSignature('android/content/pm/ServiceInfo')]
JServiceInfo = interface(JComponentInfo)
['{50AE8F16-96AF-4441-AB34-FEF5038CD7D6}']
  {Property Methods}
  function _Getflags: Integer;
  procedure _Setflags(Value: Integer);
  function _Getpermission: JString;
  procedure _Setpermission(Value: JString);
  {Methods}
  function describeContents: Integer; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; parcelableFlags: Integer); cdecl;
  {Properties}
  property flags: Integer read _Getflags write _Setflags;
  property permission: JString read _Getpermission write _Setpermission;
end;
TJServiceInfo = class(TJavaGenericImport<JServiceInfoClass, JServiceInfo>) end;

JAnimatableClass = interface(IJavaClass)
['{BC82CC78-6570-4A20-A902-78659365B695}']
end;

[JavaSignature('android/graphics/drawable/Animatable')]
JAnimatable = interface(IJavaInstance)
['{FA61E2CC-0AC9-44D8-A303-8646A8305230}']
  {Methods}
  function isRunning: Boolean; cdecl;
  procedure start; cdecl;
  procedure stop; cdecl;
end;
TJAnimatable = class(TJavaGenericImport<JAnimatableClass, JAnimatable>) end;

JAvoidXfermodeClass = interface(JXfermodeClass)
['{59AC62D5-C559-4E2C-A26F-A2AA917DF0AD}']
  {Methods}
  function init(opColor: Integer; tolerance: Integer; mode: JAvoidXfermode_Mode): JAvoidXfermode; cdecl;
end;

[JavaSignature('android/graphics/AvoidXfermode')]
JAvoidXfermode = interface(JXfermode)
['{A7830284-D6BD-4070-8249-B01D1E29F34D}']
end;
TJAvoidXfermode = class(TJavaGenericImport<JAvoidXfermodeClass, JAvoidXfermode>) end;

JSharedPreferences_OnSharedPreferenceChangeListenerClass = interface(IJavaClass)
['{1780D887-EB33-4DD2-AC0D-E0EC298D520C}']
end;

[JavaSignature('android/content/SharedPreferences$OnSharedPreferenceChangeListener')]
JSharedPreferences_OnSharedPreferenceChangeListener = interface(IJavaInstance)
['{9DE5999B-3F66-4CFB-B2BE-7AC79B1730E7}']
  {Methods}
  procedure onSharedPreferenceChanged(sharedPreferences: JSharedPreferences; key: JString); cdecl;
end;
TJSharedPreferences_OnSharedPreferenceChangeListener = class(TJavaGenericImport<JSharedPreferences_OnSharedPreferenceChangeListenerClass, JSharedPreferences_OnSharedPreferenceChangeListener>) end;

JDialogInterface_OnCancelListenerClass = interface(IJavaClass)
['{8ADC670B-1399-439A-A691-7CAC7D2550BD}']
end;

[JavaSignature('android/content/DialogInterface$OnCancelListener')]
JDialogInterface_OnCancelListener = interface(IJavaInstance)
['{7FB8830E-FACE-4BF7-9D67-B2FBF5D1AF48}']
  {Methods}
  procedure onCancel(dialog: JDialogInterface); cdecl;
end;
TJDialogInterface_OnCancelListener = class(TJavaGenericImport<JDialogInterface_OnCancelListenerClass, JDialogInterface_OnCancelListener>) end;

JObbInfoClass = interface(JObjectClass)
['{8D534E48-EA84-4A80-8939-E86DAA95E0F5}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  function _GetOBB_OVERLAY: Integer;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
  property OBB_OVERLAY: Integer read _GetOBB_OVERLAY;
end;

[JavaSignature('android/content/res/ObbInfo')]
JObbInfo = interface(JObject)
['{9CF489E5-747A-454C-AB5E-3755133EF147}']
  {Property Methods}
  function _Getfilename: JString;
  procedure _Setfilename(Value: JString);
  function _Getflags: Integer;
  procedure _Setflags(Value: Integer);
  function _GetpackageName: JString;
  procedure _SetpackageName(Value: JString);
  function _Getversion: Integer;
  procedure _Setversion(Value: Integer);
  {Methods}
  function describeContents: Integer; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; parcelableFlags: Integer); cdecl;
  {Properties}
  property filename: JString read _Getfilename write _Setfilename;
  property flags: Integer read _Getflags write _Setflags;
  property packageName: JString read _GetpackageName write _SetpackageName;
  property version: Integer read _Getversion write _Setversion;
end;
TJObbInfo = class(TJavaGenericImport<JObbInfoClass, JObbInfo>) end;

JKeyCharacterMap_KeyDataClass = interface(JObjectClass)
['{6C0CB530-CBCE-408E-B83B-214BC758AE6A}']
  {Property Methods}
  function _GetMETA_LENGTH: Integer;
  {Methods}
  function init: JKeyCharacterMap_KeyData; cdecl;
  {Properties}
  property META_LENGTH: Integer read _GetMETA_LENGTH;
end;

[JavaSignature('android/view/KeyCharacterMap$KeyData')]
JKeyCharacterMap_KeyData = interface(JObject)
['{A4AE82F8-F493-4B07-A979-E148E550ACC4}']
  {Property Methods}
  function _GetdisplayLabel: Char;
  procedure _SetdisplayLabel(Value: Char);
  function _Getmeta: TJavaArray<Char>;
  procedure _Setmeta(Value: TJavaArray<Char>);
  function _Getnumber: Char;
  procedure _Setnumber(Value: Char);
  {Properties}
  property displayLabel: Char read _GetdisplayLabel write _SetdisplayLabel;
  property meta: TJavaArray<Char> read _Getmeta write _Setmeta;
  property number: Char read _Getnumber write _Setnumber;
end;
TJKeyCharacterMap_KeyData = class(TJavaGenericImport<JKeyCharacterMap_KeyDataClass, JKeyCharacterMap_KeyData>) end;

JEntity_NamedContentValuesClass = interface(JObjectClass)
['{609C9106-0DD5-4C76-8888-BBBE59539300}']
  {Methods}
  function init(uri: Jnet_Uri; values: JContentValues): JEntity_NamedContentValues; cdecl;
end;

[JavaSignature('android/content/Entity$NamedContentValues')]
JEntity_NamedContentValues = interface(JObject)
['{F0B1D32C-F188-467C-A379-AD7A356D3C8A}']
  {Property Methods}
  function _Geturi: Jnet_Uri;
  function _Getvalues: JContentValues;
  {Properties}
  property uri: Jnet_Uri read _Geturi;
  property values: JContentValues read _Getvalues;
end;
TJEntity_NamedContentValues = class(TJavaGenericImport<JEntity_NamedContentValuesClass, JEntity_NamedContentValues>) end;

JUriMatcherClass = interface(JObjectClass)
['{7E025473-95A2-4F4E-9E78-155BF04C2F74}']
  {Property Methods}
  function _GetNO_MATCH: Integer;
  {Methods}
  function init(code: Integer): JUriMatcher; cdecl;
  {Properties}
  property NO_MATCH: Integer read _GetNO_MATCH;
end;

[JavaSignature('android/content/UriMatcher')]
JUriMatcher = interface(JObject)
['{792FE785-2ED6-491E-831F-51E80A0DBEE5}']
  {Methods}
  procedure addURI(authority: JString; path: JString; code: Integer); cdecl;
  function match(uri: Jnet_Uri): Integer; cdecl;
end;
TJUriMatcher = class(TJavaGenericImport<JUriMatcherClass, JUriMatcher>) end;

JBitmap_ConfigClass = interface(JEnumClass)
['{B0485749-2E7B-4025-AC8A-536956C4AB63}']
  {Property Methods}
  function _GetALPHA_8: JBitmap_Config;
  function _GetARGB_4444: JBitmap_Config;
  function _GetARGB_8888: JBitmap_Config;
  function _GetRGB_565: JBitmap_Config;
  {Methods}
  function valueOf(name: JString): JBitmap_Config; cdecl;
  function values: TJavaObjectArray<JBitmap_Config>; cdecl;
  {Properties}
  property ALPHA_8: JBitmap_Config read _GetALPHA_8;
  property ARGB_4444: JBitmap_Config read _GetARGB_4444;
  property ARGB_8888: JBitmap_Config read _GetARGB_8888;
  property RGB_565: JBitmap_Config read _GetRGB_565;
end;

[JavaSignature('android/graphics/Bitmap$Config')]
JBitmap_Config = interface(JEnum)
['{85490287-B946-4DB1-A23C-FA190B2DBDDD}']
end;
TJBitmap_Config = class(TJavaGenericImport<JBitmap_ConfigClass, JBitmap_Config>) end;

JColorStateListClass = interface(JObjectClass)
['{FE6E6803-0FA7-4277-8D1E-2386FAF50392}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Methods}
  function init(states: TJavaBiArray<Integer>; colors: TJavaArray<Integer>): JColorStateList; cdecl;
  function valueOf(color: Integer): JColorStateList; cdecl;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/content/res/ColorStateList')]
JColorStateList = interface(JObject)
['{63B96C0B-D508-4B27-B84C-48ECF4BA162B}']
  {Methods}
  function describeContents: Integer; cdecl;
  function getColorForState(stateSet: TJavaArray<Integer>; defaultColor: Integer): Integer; cdecl;
  function getDefaultColor: Integer; cdecl;
  function isStateful: Boolean; cdecl;
  function toString: JString; cdecl;
  function withAlpha(alpha: Integer): JColorStateList; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
end;
TJColorStateList = class(TJavaGenericImport<JColorStateListClass, JColorStateList>) end;

JApplicationInfo_DisplayNameComparatorClass = interface(JObjectClass)
['{A2EC6479-49C1-4015-97E5-543F78A4D0B7}']
  {Methods}
  function init(pm: JPackageManager): JApplicationInfo_DisplayNameComparator; cdecl;
end;

[JavaSignature('android/content/pm/ApplicationInfo$DisplayNameComparator')]
JApplicationInfo_DisplayNameComparator = interface(JObject)
['{D0078998-D2BF-4CD8-866F-8313C5B28069}']
  {Methods}
  function compare(aa: JApplicationInfo; ab: JApplicationInfo): Integer; cdecl;
end;
TJApplicationInfo_DisplayNameComparator = class(TJavaGenericImport<JApplicationInfo_DisplayNameComparatorClass, JApplicationInfo_DisplayNameComparator>) end;

JInputMethod_SessionCallbackClass = interface(IJavaClass)
['{493EE3FA-B9E7-4212-840C-BB207BFDEA2C}']
end;

[JavaSignature('android/view/inputmethod/InputMethod$SessionCallback')]
JInputMethod_SessionCallback = interface(IJavaInstance)
['{FA9324BD-9C4F-4C26-9765-1604AF615B5F}']
  {Methods}
  procedure sessionCreated(session: JInputMethodSession); cdecl;
end;
TJInputMethod_SessionCallback = class(TJavaGenericImport<JInputMethod_SessionCallbackClass, JInputMethod_SessionCallback>) end;

JViewPropertyAnimatorClass = interface(JObjectClass)
['{66D7DA64-ED33-4831-9C30-02F8C0D2ABA7}']
end;

[JavaSignature('android/view/ViewPropertyAnimator')]
JViewPropertyAnimator = interface(JObject)
['{5766E8F5-FDF6-490F-9544-8238274F7EAA}']
  {Methods}
  function alpha(value: Single): JViewPropertyAnimator; cdecl;
  function alphaBy(value: Single): JViewPropertyAnimator; cdecl;
  procedure cancel; cdecl;
  function getDuration: Int64; cdecl;
  function getStartDelay: Int64; cdecl;
  function rotation(value: Single): JViewPropertyAnimator; cdecl;
  function rotationBy(value: Single): JViewPropertyAnimator; cdecl;
  function rotationX(value: Single): JViewPropertyAnimator; cdecl;
  function rotationXBy(value: Single): JViewPropertyAnimator; cdecl;
  function rotationY(value: Single): JViewPropertyAnimator; cdecl;
  function rotationYBy(value: Single): JViewPropertyAnimator; cdecl;
  function scaleX(value: Single): JViewPropertyAnimator; cdecl;
  function scaleXBy(value: Single): JViewPropertyAnimator; cdecl;
  function scaleY(value: Single): JViewPropertyAnimator; cdecl;
  function scaleYBy(value: Single): JViewPropertyAnimator; cdecl;
  function setDuration(duration: Int64): JViewPropertyAnimator; cdecl;
  function setStartDelay(startDelay: Int64): JViewPropertyAnimator; cdecl;
  procedure start; cdecl;
  function translationX(value: Single): JViewPropertyAnimator; cdecl;
  function translationXBy(value: Single): JViewPropertyAnimator; cdecl;
  function translationY(value: Single): JViewPropertyAnimator; cdecl;
  function translationYBy(value: Single): JViewPropertyAnimator; cdecl;
  function withEndAction(runnable: JRunnable): JViewPropertyAnimator; cdecl;
  function withLayer: JViewPropertyAnimator; cdecl;
  function withStartAction(runnable: JRunnable): JViewPropertyAnimator; cdecl;
  function x(value: Single): JViewPropertyAnimator; cdecl;
  function xBy(value: Single): JViewPropertyAnimator; cdecl;
  function y(value: Single): JViewPropertyAnimator; cdecl;
  function yBy(value: Single): JViewPropertyAnimator; cdecl;
end;
TJViewPropertyAnimator = class(TJavaGenericImport<JViewPropertyAnimatorClass, JViewPropertyAnimator>) end;

JRoundRectShapeClass = interface(JRectShapeClass)
['{F5B46D75-F12C-484A-A89C-86B47F84F3EA}']
  {Methods}
  function init(outerRadii: TJavaArray<Single>; inset: JRectF; innerRadii: TJavaArray<Single>): JRoundRectShape; cdecl;
end;

[JavaSignature('android/graphics/drawable/shapes/RoundRectShape')]
JRoundRectShape = interface(JRectShape)
['{8642D3BE-B527-4A7C-96F5-A20B5AC67A8D}']
  {Methods}
  function clone: JRoundRectShape; cdecl;
  procedure draw(canvas: JCanvas; paint: JPaint); cdecl;
end;
TJRoundRectShape = class(TJavaGenericImport<JRoundRectShapeClass, JRoundRectShape>) end;

JDrawableContainer_DrawableContainerStateClass = interface(JDrawable_ConstantStateClass)
['{4ADDD2D1-BDBC-4DBC-87D6-85DE34C2EBCF}']
end;

[JavaSignature('android/graphics/drawable/DrawableContainer$DrawableContainerState')]
JDrawableContainer_DrawableContainerState = interface(JDrawable_ConstantState)
['{10EB3DB2-2579-43CD-9330-E9B6C36B1AB8}']
  {Methods}
  function addChild(dr: JDrawable): Integer; cdecl;
  function canConstantState: Boolean; cdecl;
  function getChangingConfigurations: Integer; cdecl;
  function getChildCount: Integer; cdecl;
  function getChildren: TJavaObjectArray<JDrawable>; cdecl;
  function getConstantHeight: Integer; cdecl;
  function getConstantMinimumHeight: Integer; cdecl;
  function getConstantMinimumWidth: Integer; cdecl;
  function getConstantPadding: JRect; cdecl;
  function getConstantWidth: Integer; cdecl;
  function getEnterFadeDuration: Integer; cdecl;
  function getExitFadeDuration: Integer; cdecl;
  function getOpacity: Integer; cdecl;
  procedure growArray(oldSize: Integer; newSize: Integer); cdecl;
  function isConstantSize: Boolean; cdecl;
  function isStateful: Boolean; cdecl;
  procedure setConstantSize(constant: Boolean); cdecl;
  procedure setEnterFadeDuration(duration: Integer); cdecl;
  procedure setExitFadeDuration(duration: Integer); cdecl;
  procedure setVariablePadding(variable: Boolean); cdecl;
end;
TJDrawableContainer_DrawableContainerState = class(TJavaGenericImport<JDrawableContainer_DrawableContainerStateClass, JDrawableContainer_DrawableContainerState>) end;

JDrawable_CallbackClass = interface(IJavaClass)
['{9CC689FA-7A85-45BF-9177-B3AD29D31FE9}']
end;

[JavaSignature('android/graphics/drawable/Drawable$Callback')]
JDrawable_Callback = interface(IJavaInstance)
['{B41BB3DD-4E4B-4771-9D5A-FADE128DBFDA}']
  {Methods}
  procedure invalidateDrawable(who: JDrawable); cdecl;
  procedure scheduleDrawable(who: JDrawable; what: JRunnable; when: Int64); cdecl;
  procedure unscheduleDrawable(who: JDrawable; what: JRunnable); cdecl;
end;
TJDrawable_Callback = class(TJavaGenericImport<JDrawable_CallbackClass, JDrawable_Callback>) end;

JSyncResultClass = interface(JObjectClass)
['{4AB5B7A4-F685-4D10-9E00-DEE5C5FE93C6}']
  {Property Methods}
  function _GetALREADY_IN_PROGRESS: JSyncResult;
  function _GetCREATOR: JParcelable_Creator;
  {Methods}
  function init: JSyncResult; cdecl;
  {Properties}
  property ALREADY_IN_PROGRESS: JSyncResult read _GetALREADY_IN_PROGRESS;
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/content/SyncResult')]
JSyncResult = interface(JObject)
['{5A5A4C47-BC5C-409B-8BD3-C8D600D485D9}']
  {Property Methods}
  function _GetdatabaseError: Boolean;
  procedure _SetdatabaseError(Value: Boolean);
  function _GetdelayUntil: Int64;
  procedure _SetdelayUntil(Value: Int64);
  function _GetfullSyncRequested: Boolean;
  procedure _SetfullSyncRequested(Value: Boolean);
  function _GetmoreRecordsToGet: Boolean;
  procedure _SetmoreRecordsToGet(Value: Boolean);
  function _GetpartialSyncUnavailable: Boolean;
  procedure _SetpartialSyncUnavailable(Value: Boolean);
  function _Getstats: JSyncStats;
  function _GetsyncAlreadyInProgress: Boolean;
  function _GettooManyDeletions: Boolean;
  procedure _SettooManyDeletions(Value: Boolean);
  function _GettooManyRetries: Boolean;
  procedure _SettooManyRetries(Value: Boolean);
  {Methods}
  procedure clear; cdecl;
  function describeContents: Integer; cdecl;
  function hasError: Boolean; cdecl;
  function hasHardError: Boolean; cdecl;
  function hasSoftError: Boolean; cdecl;
  function madeSomeProgress: Boolean; cdecl;
  function toDebugString: JString; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(parcel: JParcel; flags: Integer); cdecl;
  {Properties}
  property databaseError: Boolean read _GetdatabaseError write _SetdatabaseError;
  property delayUntil: Int64 read _GetdelayUntil write _SetdelayUntil;
  property fullSyncRequested: Boolean read _GetfullSyncRequested write _SetfullSyncRequested;
  property moreRecordsToGet: Boolean read _GetmoreRecordsToGet write _SetmoreRecordsToGet;
  property partialSyncUnavailable: Boolean read _GetpartialSyncUnavailable write _SetpartialSyncUnavailable;
  property stats: JSyncStats read _Getstats;
  property syncAlreadyInProgress: Boolean read _GetsyncAlreadyInProgress;
  property tooManyDeletions: Boolean read _GettooManyDeletions write _SettooManyDeletions;
  property tooManyRetries: Boolean read _GettooManyRetries write _SettooManyRetries;
end;
TJSyncResult = class(TJavaGenericImport<JSyncResultClass, JSyncResult>) end;

JView_AccessibilityDelegateClass = interface(JObjectClass)
['{DC7776BF-CA33-4BBF-964E-77C7037A82CD}']
  {Methods}
  function init: JView_AccessibilityDelegate; cdecl;
end;

[JavaSignature('android/view/View$AccessibilityDelegate')]
JView_AccessibilityDelegate = interface(JObject)
['{88C63CE9-94D4-4DE8-B74D-1EC6F8DCF8E5}']
  {Methods}
  function performAccessibilityAction(host: JView; action: Integer; args: JBundle): Boolean; cdecl;
  procedure sendAccessibilityEvent(host: JView; eventType: Integer); cdecl;
end;
TJView_AccessibilityDelegate = class(TJavaGenericImport<JView_AccessibilityDelegateClass, JView_AccessibilityDelegate>) end;

JTypefaceClass = interface(JObjectClass)
['{4F012009-BA52-43D2-9B95-18833CF3F090}']
  {Property Methods}
  function _GetBOLD: Integer;
  function _GetBOLD_ITALIC: Integer;
  function _GetDEFAULT: JTypeface;
  function _GetDEFAULT_BOLD: JTypeface;
  function _GetITALIC: Integer;
  function _GetMONOSPACE: JTypeface;
  function _GetNORMAL: Integer;
  function _GetSANS_SERIF: JTypeface;
  function _GetSERIF: JTypeface;
  {Methods}
  function create(familyName: JString; style: Integer): JTypeface; cdecl; overload;
  function create(family: JTypeface; style: Integer): JTypeface; cdecl; overload;
  function createFromAsset(mgr: JAssetManager; path: JString): JTypeface; cdecl;
  function createFromFile(path: JFile): JTypeface; cdecl; overload;
  function createFromFile(path: JString): JTypeface; cdecl; overload;
  function defaultFromStyle(style: Integer): JTypeface; cdecl;
  {Properties}
  property BOLD: Integer read _GetBOLD;
  property BOLD_ITALIC: Integer read _GetBOLD_ITALIC;
  property DEFAULT: JTypeface read _GetDEFAULT;
  property DEFAULT_BOLD: JTypeface read _GetDEFAULT_BOLD;
  property ITALIC: Integer read _GetITALIC;
  property MONOSPACE: JTypeface read _GetMONOSPACE;
  property NORMAL: Integer read _GetNORMAL;
  property SANS_SERIF: JTypeface read _GetSANS_SERIF;
  property SERIF: JTypeface read _GetSERIF;
end;

[JavaSignature('android/graphics/Typeface')]
JTypeface = interface(JObject)
['{7D5C8025-19B6-46B1-A86D-3539C26F7D11}']
  {Methods}
  function equals(o: JObject): Boolean; cdecl;
  function getStyle: Integer; cdecl;
  function hashCode: Integer; cdecl;
  function isBold: Boolean; cdecl;
  function isItalic: Boolean; cdecl;
end;
TJTypeface = class(TJavaGenericImport<JTypefaceClass, JTypeface>) end;

JView_OnSystemUiVisibilityChangeListenerClass = interface(IJavaClass)
['{2C9D51FB-FF3B-4FCE-A737-88A2111FC420}']
end;

[JavaSignature('android/view/View$OnSystemUiVisibilityChangeListener')]
JView_OnSystemUiVisibilityChangeListener = interface(IJavaInstance)
['{852870F3-99B1-4EF9-B1F8-6F2ED7749D45}']
  {Methods}
  procedure onSystemUiVisibilityChange(visibility: Integer); cdecl;
end;
TJView_OnSystemUiVisibilityChangeListener = class(TJavaGenericImport<JView_OnSystemUiVisibilityChangeListenerClass, JView_OnSystemUiVisibilityChangeListener>) end;

JView_OnFocusChangeListenerClass = interface(IJavaClass)
['{4530BDF1-6C95-4EFC-B728-AAC8A1231FA3}']
end;

[JavaSignature('android/view/View$OnFocusChangeListener')]
JView_OnFocusChangeListener = interface(IJavaInstance)
['{5FFFED8A-78CC-4E8C-BD41-8F7DBC6DF4AC}']
  {Methods}
  procedure onFocusChange(v: JView; hasFocus: Boolean); cdecl;
end;
TJView_OnFocusChangeListener = class(TJavaGenericImport<JView_OnFocusChangeListenerClass, JView_OnFocusChangeListener>) end;

JActivityNotFoundExceptionClass = interface(JRuntimeExceptionClass)
['{073EE9FA-3FED-43AD-8BE6-5DBD8A50D3DF}']
  {Methods}
  function init: JActivityNotFoundException; cdecl; overload;
  function init(name: JString): JActivityNotFoundException; cdecl; overload;
end;

[JavaSignature('android/content/ActivityNotFoundException')]
JActivityNotFoundException = interface(JRuntimeException)
['{87851F6B-1234-4A5C-8C94-3B6E101B90A9}']
end;
TJActivityNotFoundException = class(TJavaGenericImport<JActivityNotFoundExceptionClass, JActivityNotFoundException>) end;

JSQLiteTransactionListenerClass = interface(IJavaClass)
['{7B2DA9F0-CA67-454A-92FE-2424DC354414}']
end;

[JavaSignature('android/database/sqlite/SQLiteTransactionListener')]
JSQLiteTransactionListener = interface(IJavaInstance)
['{C05E63B7-B5F5-4A6D-8EC1-09973B9CF1A2}']
  {Methods}
  procedure onBegin; cdecl;
  procedure onCommit; cdecl;
  procedure onRollback; cdecl;
end;
TJSQLiteTransactionListener = class(TJavaGenericImport<JSQLiteTransactionListenerClass, JSQLiteTransactionListener>) end;

JInputMethodSession_EventCallbackClass = interface(IJavaClass)
['{DC00D9CC-2852-44F7-9D2B-B1FD068D2FD0}']
end;

[JavaSignature('android/view/inputmethod/InputMethodSession$EventCallback')]
JInputMethodSession_EventCallback = interface(IJavaInstance)
['{EA9CD00C-A636-46C5-A4B4-1FEC7600B74E}']
  {Methods}
  procedure finishedEvent(seq: Integer; handled: Boolean); cdecl;
end;
TJInputMethodSession_EventCallback = class(TJavaGenericImport<JInputMethodSession_EventCallbackClass, JInputMethodSession_EventCallback>) end;

JFeatureInfoClass = interface(JObjectClass)
['{4F2C5DED-CBE6-4F78-A84C-BEF08B859AC2}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  function _GetFLAG_REQUIRED: Integer;
  function _GetGL_ES_VERSION_UNDEFINED: Integer;
  {Methods}
  function init: JFeatureInfo; cdecl; overload;
  function init(orig: JFeatureInfo): JFeatureInfo; cdecl; overload;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
  property FLAG_REQUIRED: Integer read _GetFLAG_REQUIRED;
  property GL_ES_VERSION_UNDEFINED: Integer read _GetGL_ES_VERSION_UNDEFINED;
end;

[JavaSignature('android/content/pm/FeatureInfo')]
JFeatureInfo = interface(JObject)
['{A62531D1-D497-45F3-9845-4E21175295FC}']
  {Property Methods}
  function _Getflags: Integer;
  procedure _Setflags(Value: Integer);
  function _Getname: JString;
  procedure _Setname(Value: JString);
  function _GetreqGlEsVersion: Integer;
  procedure _SetreqGlEsVersion(Value: Integer);
  {Methods}
  function describeContents: Integer; cdecl;
  function getGlEsVersion: JString; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; parcelableFlags: Integer); cdecl;
  {Properties}
  property flags: Integer read _Getflags write _Setflags;
  property name: JString read _Getname write _Setname;
  property reqGlEsVersion: Integer read _GetreqGlEsVersion write _SetreqGlEsVersion;
end;
TJFeatureInfo = class(TJavaGenericImport<JFeatureInfoClass, JFeatureInfo>) end;

JGestureDetector_OnDoubleTapListenerClass = interface(IJavaClass)
['{219C5FEA-C7A3-42D6-9634-7F7EA908EB99}']
end;

[JavaSignature('android/view/GestureDetector$OnDoubleTapListener')]
JGestureDetector_OnDoubleTapListener = interface(IJavaInstance)
['{759D27FB-6DD3-4C4C-BA8D-3814CB8278A2}']
  {Methods}
  function onDoubleTap(e: JMotionEvent): Boolean; cdecl;
  function onDoubleTapEvent(e: JMotionEvent): Boolean; cdecl;
  function onSingleTapConfirmed(e: JMotionEvent): Boolean; cdecl;
end;
TJGestureDetector_OnDoubleTapListener = class(TJavaGenericImport<JGestureDetector_OnDoubleTapListenerClass, JGestureDetector_OnDoubleTapListener>) end;

JAssetFileDescriptor_AutoCloseInputStreamClass = interface(JParcelFileDescriptor_AutoCloseInputStreamClass)
['{2B308E42-8E36-4B97-BE28-0D5282CFFFD5}']
  {Methods}
  function init(fd: JAssetFileDescriptor): JAssetFileDescriptor_AutoCloseInputStream; cdecl;
end;

[JavaSignature('android/content/res/AssetFileDescriptor$AutoCloseInputStream')]
JAssetFileDescriptor_AutoCloseInputStream = interface(JParcelFileDescriptor_AutoCloseInputStream)
['{281E5E4C-402B-4851-A5FC-06F1D53E8600}']
  {Methods}
  function available: Integer; cdecl;
  procedure mark(readlimit: Integer); cdecl;
  function markSupported: Boolean; cdecl;
  function read: Integer; cdecl; overload;
  function read(buffer: TJavaArray<Byte>; offset: Integer; count: Integer): Integer; cdecl; overload;
  function read(buffer: TJavaArray<Byte>): Integer; cdecl; overload;
  procedure reset; cdecl;
  function skip(count: Int64): Int64; cdecl;
end;
TJAssetFileDescriptor_AutoCloseInputStream = class(TJavaGenericImport<JAssetFileDescriptor_AutoCloseInputStreamClass, JAssetFileDescriptor_AutoCloseInputStream>) end;

JWindowManager_LayoutParamsClass = interface(JViewGroup_LayoutParamsClass)
['{677013A8-C0C6-4ED2-83EA-188BF4D1331A}']
  {Property Methods}
  function _GetALPHA_CHANGED: Integer;
  function _GetANIMATION_CHANGED: Integer;
  function _GetBRIGHTNESS_OVERRIDE_FULL: Single;
  function _GetBRIGHTNESS_OVERRIDE_NONE: Single;
  function _GetBRIGHTNESS_OVERRIDE_OFF: Single;
  function _GetCREATOR: JParcelable_Creator;
  function _GetDIM_AMOUNT_CHANGED: Integer;
  function _GetFIRST_APPLICATION_WINDOW: Integer;
  function _GetFIRST_SUB_WINDOW: Integer;
  function _GetFIRST_SYSTEM_WINDOW: Integer;
  function _GetFLAGS_CHANGED: Integer;
  function _GetFLAG_ALLOW_LOCK_WHILE_SCREEN_ON: Integer;
  function _GetFLAG_ALT_FOCUSABLE_IM: Integer;
  function _GetFLAG_BLUR_BEHIND: Integer;
  function _GetFLAG_DIM_BEHIND: Integer;
  function _GetFLAG_DISMISS_KEYGUARD: Integer;
  function _GetFLAG_DITHER: Integer;
  function _GetFLAG_FORCE_NOT_FULLSCREEN: Integer;
  function _GetFLAG_FULLSCREEN: Integer;
  function _GetFLAG_HARDWARE_ACCELERATED: Integer;
  function _GetFLAG_IGNORE_CHEEK_PRESSES: Integer;
  function _GetFLAG_KEEP_SCREEN_ON: Integer;
  function _GetFLAG_LAYOUT_INSET_DECOR: Integer;
  function _GetFLAG_LAYOUT_IN_SCREEN: Integer;
  function _GetFLAG_LAYOUT_NO_LIMITS: Integer;
  function _GetFLAG_NOT_FOCUSABLE: Integer;
  function _GetFLAG_NOT_TOUCHABLE: Integer;
  function _GetFLAG_NOT_TOUCH_MODAL: Integer;
  function _GetFLAG_SCALED: Integer;
  function _GetFLAG_SECURE: Integer;
  function _GetFLAG_SHOW_WALLPAPER: Integer;
  function _GetFLAG_SHOW_WHEN_LOCKED: Integer;
  function _GetFLAG_SPLIT_TOUCH: Integer;
  function _GetFLAG_TOUCHABLE_WHEN_WAKING: Integer;
  function _GetFLAG_TURN_SCREEN_ON: Integer;
  function _GetFLAG_WATCH_OUTSIDE_TOUCH: Integer;
  function _GetFORMAT_CHANGED: Integer;
  function _GetLAST_APPLICATION_WINDOW: Integer;
  function _GetLAST_SUB_WINDOW: Integer;
  function _GetLAST_SYSTEM_WINDOW: Integer;
  function _GetLAYOUT_CHANGED: Integer;
  function _GetMEMORY_TYPE_CHANGED: Integer;
  function _GetMEMORY_TYPE_GPU: Integer;
  function _GetMEMORY_TYPE_HARDWARE: Integer;
  function _GetMEMORY_TYPE_NORMAL: Integer;
  function _GetMEMORY_TYPE_PUSH_BUFFERS: Integer;
  function _GetSCREEN_BRIGHTNESS_CHANGED: Integer;
  function _GetSCREEN_ORIENTATION_CHANGED: Integer;
  function _GetSOFT_INPUT_ADJUST_NOTHING: Integer;
  function _GetSOFT_INPUT_ADJUST_PAN: Integer;
  function _GetSOFT_INPUT_ADJUST_RESIZE: Integer;
  function _GetSOFT_INPUT_ADJUST_UNSPECIFIED: Integer;
  function _GetSOFT_INPUT_IS_FORWARD_NAVIGATION: Integer;
  function _GetSOFT_INPUT_MASK_ADJUST: Integer;
  function _GetSOFT_INPUT_MASK_STATE: Integer;
  function _GetSOFT_INPUT_MODE_CHANGED: Integer;
  function _GetSOFT_INPUT_STATE_ALWAYS_HIDDEN: Integer;
  function _GetSOFT_INPUT_STATE_ALWAYS_VISIBLE: Integer;
  function _GetSOFT_INPUT_STATE_HIDDEN: Integer;
  function _GetSOFT_INPUT_STATE_UNCHANGED: Integer;
  function _GetSOFT_INPUT_STATE_UNSPECIFIED: Integer;
  function _GetSOFT_INPUT_STATE_VISIBLE: Integer;
  function _GetTITLE_CHANGED: Integer;
  function _GetTYPE_APPLICATION: Integer;
  function _GetTYPE_APPLICATION_ATTACHED_DIALOG: Integer;
  function _GetTYPE_APPLICATION_MEDIA: Integer;
  function _GetTYPE_APPLICATION_PANEL: Integer;
  function _GetTYPE_APPLICATION_STARTING: Integer;
  function _GetTYPE_APPLICATION_SUB_PANEL: Integer;
  function _GetTYPE_BASE_APPLICATION: Integer;
  function _GetTYPE_CHANGED: Integer;
  function _GetTYPE_INPUT_METHOD: Integer;
  function _GetTYPE_INPUT_METHOD_DIALOG: Integer;
  function _GetTYPE_KEYGUARD: Integer;
  function _GetTYPE_KEYGUARD_DIALOG: Integer;
  function _GetTYPE_PHONE: Integer;
  function _GetTYPE_PRIORITY_PHONE: Integer;
  function _GetTYPE_SEARCH_BAR: Integer;
  function _GetTYPE_STATUS_BAR: Integer;
  function _GetTYPE_STATUS_BAR_PANEL: Integer;
  function _GetTYPE_SYSTEM_ALERT: Integer;
  function _GetTYPE_SYSTEM_DIALOG: Integer;
  function _GetTYPE_SYSTEM_ERROR: Integer;
  function _GetTYPE_SYSTEM_OVERLAY: Integer;
  function _GetTYPE_TOAST: Integer;
  function _GetTYPE_WALLPAPER: Integer;
  {Methods}
  function init: JWindowManager_LayoutParams; cdecl; overload;
  function init(_type: Integer): JWindowManager_LayoutParams; cdecl; overload;
  function init(_type: Integer; _flags: Integer): JWindowManager_LayoutParams; cdecl; overload;
  function init(_type: Integer; _flags: Integer; _format: Integer): JWindowManager_LayoutParams; cdecl; overload;
  function init(w: Integer; h: Integer; _type: Integer; _flags: Integer; _format: Integer): JWindowManager_LayoutParams; cdecl; overload;
  function init(w: Integer; h: Integer; xpos: Integer; ypos: Integer; _type: Integer; _flags: Integer; _format: Integer): JWindowManager_LayoutParams; cdecl; overload;
  function init(in_: JParcel): JWindowManager_LayoutParams; cdecl; overload;
  function mayUseInputMethod(flags: Integer): Boolean; cdecl;
  {Properties}
  property ALPHA_CHANGED: Integer read _GetALPHA_CHANGED;
  property ANIMATION_CHANGED: Integer read _GetANIMATION_CHANGED;
  property BRIGHTNESS_OVERRIDE_FULL: Single read _GetBRIGHTNESS_OVERRIDE_FULL;
  property BRIGHTNESS_OVERRIDE_NONE: Single read _GetBRIGHTNESS_OVERRIDE_NONE;
  property BRIGHTNESS_OVERRIDE_OFF: Single read _GetBRIGHTNESS_OVERRIDE_OFF;
  property CREATOR: JParcelable_Creator read _GetCREATOR;
  property DIM_AMOUNT_CHANGED: Integer read _GetDIM_AMOUNT_CHANGED;
  property FIRST_APPLICATION_WINDOW: Integer read _GetFIRST_APPLICATION_WINDOW;
  property FIRST_SUB_WINDOW: Integer read _GetFIRST_SUB_WINDOW;
  property FIRST_SYSTEM_WINDOW: Integer read _GetFIRST_SYSTEM_WINDOW;
  property FLAGS_CHANGED: Integer read _GetFLAGS_CHANGED;
  property FLAG_ALLOW_LOCK_WHILE_SCREEN_ON: Integer read _GetFLAG_ALLOW_LOCK_WHILE_SCREEN_ON;
  property FLAG_ALT_FOCUSABLE_IM: Integer read _GetFLAG_ALT_FOCUSABLE_IM;
  property FLAG_BLUR_BEHIND: Integer read _GetFLAG_BLUR_BEHIND;
  property FLAG_DIM_BEHIND: Integer read _GetFLAG_DIM_BEHIND;
  property FLAG_DISMISS_KEYGUARD: Integer read _GetFLAG_DISMISS_KEYGUARD;
  property FLAG_DITHER: Integer read _GetFLAG_DITHER;
  property FLAG_FORCE_NOT_FULLSCREEN: Integer read _GetFLAG_FORCE_NOT_FULLSCREEN;
  property FLAG_FULLSCREEN: Integer read _GetFLAG_FULLSCREEN;
  property FLAG_HARDWARE_ACCELERATED: Integer read _GetFLAG_HARDWARE_ACCELERATED;
  property FLAG_IGNORE_CHEEK_PRESSES: Integer read _GetFLAG_IGNORE_CHEEK_PRESSES;
  property FLAG_KEEP_SCREEN_ON: Integer read _GetFLAG_KEEP_SCREEN_ON;
  property FLAG_LAYOUT_INSET_DECOR: Integer read _GetFLAG_LAYOUT_INSET_DECOR;
  property FLAG_LAYOUT_IN_SCREEN: Integer read _GetFLAG_LAYOUT_IN_SCREEN;
  property FLAG_LAYOUT_NO_LIMITS: Integer read _GetFLAG_LAYOUT_NO_LIMITS;
  property FLAG_NOT_FOCUSABLE: Integer read _GetFLAG_NOT_FOCUSABLE;
  property FLAG_NOT_TOUCHABLE: Integer read _GetFLAG_NOT_TOUCHABLE;
  property FLAG_NOT_TOUCH_MODAL: Integer read _GetFLAG_NOT_TOUCH_MODAL;
  property FLAG_SCALED: Integer read _GetFLAG_SCALED;
  property FLAG_SECURE: Integer read _GetFLAG_SECURE;
  property FLAG_SHOW_WALLPAPER: Integer read _GetFLAG_SHOW_WALLPAPER;
  property FLAG_SHOW_WHEN_LOCKED: Integer read _GetFLAG_SHOW_WHEN_LOCKED;
  property FLAG_SPLIT_TOUCH: Integer read _GetFLAG_SPLIT_TOUCH;
  property FLAG_TOUCHABLE_WHEN_WAKING: Integer read _GetFLAG_TOUCHABLE_WHEN_WAKING;
  property FLAG_TURN_SCREEN_ON: Integer read _GetFLAG_TURN_SCREEN_ON;
  property FLAG_WATCH_OUTSIDE_TOUCH: Integer read _GetFLAG_WATCH_OUTSIDE_TOUCH;
  property FORMAT_CHANGED: Integer read _GetFORMAT_CHANGED;
  property LAST_APPLICATION_WINDOW: Integer read _GetLAST_APPLICATION_WINDOW;
  property LAST_SUB_WINDOW: Integer read _GetLAST_SUB_WINDOW;
  property LAST_SYSTEM_WINDOW: Integer read _GetLAST_SYSTEM_WINDOW;
  property LAYOUT_CHANGED: Integer read _GetLAYOUT_CHANGED;
  property MEMORY_TYPE_CHANGED: Integer read _GetMEMORY_TYPE_CHANGED;
  property MEMORY_TYPE_GPU: Integer read _GetMEMORY_TYPE_GPU;
  property MEMORY_TYPE_HARDWARE: Integer read _GetMEMORY_TYPE_HARDWARE;
  property MEMORY_TYPE_NORMAL: Integer read _GetMEMORY_TYPE_NORMAL;
  property MEMORY_TYPE_PUSH_BUFFERS: Integer read _GetMEMORY_TYPE_PUSH_BUFFERS;
  property SCREEN_BRIGHTNESS_CHANGED: Integer read _GetSCREEN_BRIGHTNESS_CHANGED;
  property SCREEN_ORIENTATION_CHANGED: Integer read _GetSCREEN_ORIENTATION_CHANGED;
  property SOFT_INPUT_ADJUST_NOTHING: Integer read _GetSOFT_INPUT_ADJUST_NOTHING;
  property SOFT_INPUT_ADJUST_PAN: Integer read _GetSOFT_INPUT_ADJUST_PAN;
  property SOFT_INPUT_ADJUST_RESIZE: Integer read _GetSOFT_INPUT_ADJUST_RESIZE;
  property SOFT_INPUT_ADJUST_UNSPECIFIED: Integer read _GetSOFT_INPUT_ADJUST_UNSPECIFIED;
  property SOFT_INPUT_IS_FORWARD_NAVIGATION: Integer read _GetSOFT_INPUT_IS_FORWARD_NAVIGATION;
  property SOFT_INPUT_MASK_ADJUST: Integer read _GetSOFT_INPUT_MASK_ADJUST;
  property SOFT_INPUT_MASK_STATE: Integer read _GetSOFT_INPUT_MASK_STATE;
  property SOFT_INPUT_MODE_CHANGED: Integer read _GetSOFT_INPUT_MODE_CHANGED;
  property SOFT_INPUT_STATE_ALWAYS_HIDDEN: Integer read _GetSOFT_INPUT_STATE_ALWAYS_HIDDEN;
  property SOFT_INPUT_STATE_ALWAYS_VISIBLE: Integer read _GetSOFT_INPUT_STATE_ALWAYS_VISIBLE;
  property SOFT_INPUT_STATE_HIDDEN: Integer read _GetSOFT_INPUT_STATE_HIDDEN;
  property SOFT_INPUT_STATE_UNCHANGED: Integer read _GetSOFT_INPUT_STATE_UNCHANGED;
  property SOFT_INPUT_STATE_UNSPECIFIED: Integer read _GetSOFT_INPUT_STATE_UNSPECIFIED;
  property SOFT_INPUT_STATE_VISIBLE: Integer read _GetSOFT_INPUT_STATE_VISIBLE;
  property TITLE_CHANGED: Integer read _GetTITLE_CHANGED;
  property TYPE_APPLICATION: Integer read _GetTYPE_APPLICATION;
  property TYPE_APPLICATION_ATTACHED_DIALOG: Integer read _GetTYPE_APPLICATION_ATTACHED_DIALOG;
  property TYPE_APPLICATION_MEDIA: Integer read _GetTYPE_APPLICATION_MEDIA;
  property TYPE_APPLICATION_PANEL: Integer read _GetTYPE_APPLICATION_PANEL;
  property TYPE_APPLICATION_STARTING: Integer read _GetTYPE_APPLICATION_STARTING;
  property TYPE_APPLICATION_SUB_PANEL: Integer read _GetTYPE_APPLICATION_SUB_PANEL;
  property TYPE_BASE_APPLICATION: Integer read _GetTYPE_BASE_APPLICATION;
  property TYPE_CHANGED: Integer read _GetTYPE_CHANGED;
  property TYPE_INPUT_METHOD: Integer read _GetTYPE_INPUT_METHOD;
  property TYPE_INPUT_METHOD_DIALOG: Integer read _GetTYPE_INPUT_METHOD_DIALOG;
  property TYPE_KEYGUARD: Integer read _GetTYPE_KEYGUARD;
  property TYPE_KEYGUARD_DIALOG: Integer read _GetTYPE_KEYGUARD_DIALOG;
  property TYPE_PHONE: Integer read _GetTYPE_PHONE;
  property TYPE_PRIORITY_PHONE: Integer read _GetTYPE_PRIORITY_PHONE;
  property TYPE_SEARCH_BAR: Integer read _GetTYPE_SEARCH_BAR;
  property TYPE_STATUS_BAR: Integer read _GetTYPE_STATUS_BAR;
  property TYPE_STATUS_BAR_PANEL: Integer read _GetTYPE_STATUS_BAR_PANEL;
  property TYPE_SYSTEM_ALERT: Integer read _GetTYPE_SYSTEM_ALERT;
  property TYPE_SYSTEM_DIALOG: Integer read _GetTYPE_SYSTEM_DIALOG;
  property TYPE_SYSTEM_ERROR: Integer read _GetTYPE_SYSTEM_ERROR;
  property TYPE_SYSTEM_OVERLAY: Integer read _GetTYPE_SYSTEM_OVERLAY;
  property TYPE_TOAST: Integer read _GetTYPE_TOAST;
  property TYPE_WALLPAPER: Integer read _GetTYPE_WALLPAPER;
end;

[JavaSignature('android/view/WindowManager$LayoutParams')]
JWindowManager_LayoutParams = interface(JViewGroup_LayoutParams)
['{12E0B6C8-78BE-4565-B79D-BBD5D7BB8086}']
  {Property Methods}
  function _Getalpha: Single;
  procedure _Setalpha(Value: Single);
  function _GetbuttonBrightness: Single;
  procedure _SetbuttonBrightness(Value: Single);
  function _GetdimAmount: Single;
  procedure _SetdimAmount(Value: Single);
  function _Getflags: Integer;
  procedure _Setflags(Value: Integer);
  function _Getformat: Integer;
  procedure _Setformat(Value: Integer);
  function _Getgravity: Integer;
  procedure _Setgravity(Value: Integer);
  function _GethorizontalMargin: Single;
  procedure _SethorizontalMargin(Value: Single);
  function _GethorizontalWeight: Single;
  procedure _SethorizontalWeight(Value: Single);
  function _GetmemoryType: Integer;
  procedure _SetmemoryType(Value: Integer);
  function _GetpackageName: JString;
  procedure _SetpackageName(Value: JString);
  function _GetscreenBrightness: Single;
  procedure _SetscreenBrightness(Value: Single);
  function _GetscreenOrientation: Integer;
  procedure _SetscreenOrientation(Value: Integer);
  function _GetsoftInputMode: Integer;
  procedure _SetsoftInputMode(Value: Integer);
  function _GetsystemUiVisibility: Integer;
  procedure _SetsystemUiVisibility(Value: Integer);
  function _Gettoken: JIBinder;
  procedure _Settoken(Value: JIBinder);
  function _Gettype: Integer;
  procedure _Settype(Value: Integer);
  function _GetverticalMargin: Single;
  procedure _SetverticalMargin(Value: Single);
  function _GetverticalWeight: Single;
  procedure _SetverticalWeight(Value: Single);
  function _GetwindowAnimations: Integer;
  procedure _SetwindowAnimations(Value: Integer);
  function _Getx: Integer;
  procedure _Setx(Value: Integer);
  function _Gety: Integer;
  procedure _Sety(Value: Integer);
  {Methods}
  function copyFrom(o: JWindowManager_LayoutParams): Integer; cdecl;
  function debug(output: JString): JString; cdecl;
  function describeContents: Integer; cdecl;
  function getTitle: JCharSequence; cdecl;
  procedure setTitle(title: JCharSequence); cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(out_: JParcel; parcelableFlags: Integer); cdecl;
  {Properties}
  property alpha: Single read _Getalpha write _Setalpha;
  property buttonBrightness: Single read _GetbuttonBrightness write _SetbuttonBrightness;
  property dimAmount: Single read _GetdimAmount write _SetdimAmount;
  property flags: Integer read _Getflags write _Setflags;
  property format: Integer read _Getformat write _Setformat;
  property gravity: Integer read _Getgravity write _Setgravity;
  property horizontalMargin: Single read _GethorizontalMargin write _SethorizontalMargin;
  property horizontalWeight: Single read _GethorizontalWeight write _SethorizontalWeight;
  property memoryType: Integer read _GetmemoryType write _SetmemoryType;
  property packageName: JString read _GetpackageName write _SetpackageName;
  property screenBrightness: Single read _GetscreenBrightness write _SetscreenBrightness;
  property screenOrientation: Integer read _GetscreenOrientation write _SetscreenOrientation;
  property softInputMode: Integer read _GetsoftInputMode write _SetsoftInputMode;
  property systemUiVisibility: Integer read _GetsystemUiVisibility write _SetsystemUiVisibility;
  property token: JIBinder read _Gettoken write _Settoken;
  property &type: Integer read _Gettype write _Settype;
  property verticalMargin: Single read _GetverticalMargin write _SetverticalMargin;
  property verticalWeight: Single read _GetverticalWeight write _SetverticalWeight;
  property windowAnimations: Integer read _GetwindowAnimations write _SetwindowAnimations;
  property x: Integer read _Getx write _Setx;
  property y: Integer read _Gety write _Sety;
end;
TJWindowManager_LayoutParams = class(TJavaGenericImport<JWindowManager_LayoutParamsClass, JWindowManager_LayoutParams>) end;

JActionMode_CallbackClass = interface(IJavaClass)
['{A3D54DA6-C258-4990-AD71-B6BF8FBD5608}']
end;

[JavaSignature('android/view/ActionMode$Callback')]
JActionMode_Callback = interface(IJavaInstance)
['{792BF4FC-1F34-4F60-8FA4-B86B8F14EBFF}']
  {Methods}
  function onActionItemClicked(mode: JActionMode; item: JMenuItem): Boolean; cdecl;
  function onCreateActionMode(mode: JActionMode; menu: JMenu): Boolean; cdecl;
  procedure onDestroyActionMode(mode: JActionMode); cdecl;
  function onPrepareActionMode(mode: JActionMode; menu: JMenu): Boolean; cdecl;
end;
TJActionMode_Callback = class(TJavaGenericImport<JActionMode_CallbackClass, JActionMode_Callback>) end;

JKeyListenerClass = interface(IJavaClass)
['{1CD6D223-B165-472F-B0EF-654B16926EF0}']
end;

[JavaSignature('android/text/method/KeyListener')]
JKeyListener = interface(IJavaInstance)
['{C007FF58-EB56-423A-831C-AD7EC2052127}']
  {Methods}
  procedure clearMetaKeyState(view: JView; content: JEditable; states: Integer); cdecl;
  function getInputType: Integer; cdecl;
  function onKeyDown(view: JView; text: JEditable; keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  function onKeyOther(view: JView; text: JEditable; event: JKeyEvent): Boolean; cdecl;
  function onKeyUp(view: JView; text: JEditable; keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
end;
TJKeyListener = class(TJavaGenericImport<JKeyListenerClass, JKeyListener>) end;

JView_OnHoverListenerClass = interface(IJavaClass)
['{D535C463-652A-4EEC-9BE9-AA6BC7EF4D5F}']
end;

[JavaSignature('android/view/View$OnHoverListener')]
JView_OnHoverListener = interface(IJavaInstance)
['{D74EF222-6CD6-4D9F-BF49-9E0AF7B33EB7}']
  {Methods}
  function onHover(v: JView; event: JMotionEvent): Boolean; cdecl;
end;
TJView_OnHoverListener = class(TJavaGenericImport<JView_OnHoverListenerClass, JView_OnHoverListener>) end;

JView_OnDragListenerClass = interface(IJavaClass)
['{C23EC70C-39DC-4E2B-A0E2-84587D8A47B1}']
end;

[JavaSignature('android/view/View$OnDragListener')]
JView_OnDragListener = interface(IJavaInstance)
['{78BD5F37-B6DF-4ABA-9F5B-CCA3401D91F1}']
  {Methods}
  function onDrag(v: JView; event: JDragEvent): Boolean; cdecl;
end;
TJView_OnDragListener = class(TJavaGenericImport<JView_OnDragListenerClass, JView_OnDragListener>) end;

JPorterDuffXfermodeClass = interface(JXfermodeClass)
['{7D0DA4F2-299C-4A9A-96D5-D8739FE0CC20}']
  {Methods}
  function init(mode: JPorterDuff_Mode): JPorterDuffXfermode; cdecl;
end;

[JavaSignature('android/graphics/PorterDuffXfermode')]
JPorterDuffXfermode = interface(JXfermode)
['{3BC0F433-1C7A-4E85-9246-89F54356FABD}']
end;
TJPorterDuffXfermode = class(TJavaGenericImport<JPorterDuffXfermodeClass, JPorterDuffXfermode>) end;

JTouchDelegateClass = interface(JObjectClass)
['{923E77FA-AC43-4168-B866-7645C259C17F}']
  {Property Methods}
  function _GetABOVE: Integer;
  function _GetBELOW: Integer;
  function _GetTO_LEFT: Integer;
  function _GetTO_RIGHT: Integer;
  {Methods}
  function init(bounds: JRect; delegateView: JView): JTouchDelegate; cdecl;
  {Properties}
  property ABOVE: Integer read _GetABOVE;
  property BELOW: Integer read _GetBELOW;
  property TO_LEFT: Integer read _GetTO_LEFT;
  property TO_RIGHT: Integer read _GetTO_RIGHT;
end;

[JavaSignature('android/view/TouchDelegate')]
JTouchDelegate = interface(JObject)
['{1575B097-6A88-40F9-B95A-5ACDDA6F26BE}']
  {Methods}
  function onTouchEvent(event: JMotionEvent): Boolean; cdecl;
end;
TJTouchDelegate = class(TJavaGenericImport<JTouchDelegateClass, JTouchDelegate>) end;

JLayoutClass = interface(JObjectClass)
['{56DBEDE9-766B-47BD-97F5-9DB7136FFAA3}']
  {Property Methods}
  function _GetDIR_LEFT_TO_RIGHT: Integer;
  function _GetDIR_RIGHT_TO_LEFT: Integer;
  {Methods}
  function getDesiredWidth(source: JCharSequence; paint: JTextPaint): Single; cdecl; overload;
  function getDesiredWidth(source: JCharSequence; start: Integer; end_: Integer; paint: JTextPaint): Single; cdecl; overload;
  {Properties}
  property DIR_LEFT_TO_RIGHT: Integer read _GetDIR_LEFT_TO_RIGHT;
  property DIR_RIGHT_TO_LEFT: Integer read _GetDIR_RIGHT_TO_LEFT;
end;

[JavaSignature('android/text/Layout')]
JLayout = interface(JObject)
['{606D9AE7-D751-4FC9-A68F-B30C347B5CD4}']
  {Methods}
  procedure draw(c: JCanvas); cdecl; overload;
  procedure draw(canvas: JCanvas; highlight: JPath; highlightPaint: JPaint; cursorOffsetVertical: Integer); cdecl; overload;
  function getAlignment: JLayout_Alignment; cdecl;
  function getBottomPadding: Integer; cdecl;
  procedure getCursorPath(point: Integer; dest: JPath; editingBuffer: JCharSequence); cdecl;
  function getEllipsisCount(line: Integer): Integer; cdecl;
  function getEllipsisStart(line: Integer): Integer; cdecl;
  function getEllipsizedWidth: Integer; cdecl;
  function getHeight: Integer; cdecl;
  function getLineAscent(line: Integer): Integer; cdecl;
  function getLineBaseline(line: Integer): Integer; cdecl;
  function getLineBottom(line: Integer): Integer; cdecl;
  function getLineBounds(line: Integer; bounds: JRect): Integer; cdecl;
  function getLineContainsTab(line: Integer): Boolean; cdecl;
  function getLineCount: Integer; cdecl;
  function getLineDescent(line: Integer): Integer; cdecl;
  function getLineDirections(line: Integer): JLayout_Directions; cdecl;
  function getLineEnd(line: Integer): Integer; cdecl;
  function getLineForOffset(offset: Integer): Integer; cdecl;
  function getLineForVertical(vertical: Integer): Integer; cdecl;
  function getLineLeft(line: Integer): Single; cdecl;
  function getLineMax(line: Integer): Single; cdecl;
  function getLineRight(line: Integer): Single; cdecl;
  function getLineStart(line: Integer): Integer; cdecl;
  function getLineTop(line: Integer): Integer; cdecl;
  function getLineVisibleEnd(line: Integer): Integer; cdecl;
  function getLineWidth(line: Integer): Single; cdecl;
  function getOffsetForHorizontal(line: Integer; horiz: Single): Integer; cdecl;
  function getOffsetToLeftOf(offset: Integer): Integer; cdecl;
  function getOffsetToRightOf(offset: Integer): Integer; cdecl;
  function getPaint: JTextPaint; cdecl;
  function getParagraphAlignment(line: Integer): JLayout_Alignment; cdecl;
  function getParagraphDirection(line: Integer): Integer; cdecl;
  function getParagraphLeft(line: Integer): Integer; cdecl;
  function getParagraphRight(line: Integer): Integer; cdecl;
  function getPrimaryHorizontal(offset: Integer): Single; cdecl;
  function getSecondaryHorizontal(offset: Integer): Single; cdecl;
  procedure getSelectionPath(start: Integer; end_: Integer; dest: JPath); cdecl;
  function getSpacingAdd: Single; cdecl;
  function getSpacingMultiplier: Single; cdecl;
  function getText: JCharSequence; cdecl;
  function getTopPadding: Integer; cdecl;
  function getWidth: Integer; cdecl;
  procedure increaseWidthTo(wid: Integer); cdecl;
  function isRtlCharAt(offset: Integer): Boolean; cdecl;
end;
TJLayout = class(TJavaGenericImport<JLayoutClass, JLayout>) end;




implementation

begin

end.


