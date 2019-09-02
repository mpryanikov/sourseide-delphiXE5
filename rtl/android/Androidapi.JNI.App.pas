{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit Androidapi.JNI.App;

interface

uses 
  Androidapi.JNIBridge,
  Androidapi.JNI.Os,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Net,
  Androidapi.JNI.Widget,
  Androidapi.JNI.Util,
  Androidapi.JNI.GraphicsContentViewText;

type

  {Class forward declarations}
  JFragmentManager = interface;//android.app.FragmentManager
  JNotification = interface;//android.app.Notification
  JApplication_ActivityLifecycleCallbacks = interface;//android.app.Application$ActivityLifecycleCallbacks
  JFragmentManager_OnBackStackChangedListener = interface;//android.app.FragmentManager$OnBackStackChangedListener
  JService = interface;//android.app.Service
  JApplication = interface;//android.app.Application
  JTaskStackBuilder = interface;//android.app.TaskStackBuilder
  JLoaderManager = interface;//android.app.LoaderManager
  JFragment = interface;//android.app.Fragment
  JAlarmManager = interface;//android.app.AlarmManager
  JActionBar_LayoutParams = interface;//android.app.ActionBar$LayoutParams
  JDialog = interface;//android.app.Dialog
  JAlertDialog = interface;//android.app.AlertDialog
  JActionBar_OnNavigationListener = interface;//android.app.ActionBar$OnNavigationListener
  JActionBar_TabListener = interface;//android.app.ActionBar$TabListener
  JPendingIntent = interface;//android.app.PendingIntent
  JFragment_SavedState = interface;//android.app.Fragment$SavedState
  JLoaderManager_LoaderCallbacks = interface;//android.app.LoaderManager$LoaderCallbacks
  JAlertDialog_Builder = interface;//android.app.AlertDialog$Builder
  JActionBar_Tab = interface;//android.app.ActionBar$Tab
  JActionBar = interface;//android.app.ActionBar
  JPendingIntent_OnFinished = interface;//android.app.PendingIntent$OnFinished
  JNotificationManager = interface;//android.app.NotificationManager
  JIntentService = interface;//android.app.IntentService
  JFragmentTransaction = interface;//android.app.FragmentTransaction
  JActivity = interface;//android.app.Activity
  JActionBar_OnMenuVisibilityListener = interface;//android.app.ActionBar$OnMenuVisibilityListener
  JDialogFragment = interface;//android.app.DialogFragment
  JFragmentManager_BackStackEntry = interface;//android.app.FragmentManager$BackStackEntry
  JNativeActivity = interface;//android.app.NativeActivity

JFragmentManagerClass = interface(JObjectClass)
['{AF70B1C7-BC2A-4F71-83F4-BCB987DC7F04}']
  {Property Methods}
  function _GetPOP_BACK_STACK_INCLUSIVE: Integer;
  {Methods}
  function init: JFragmentManager; cdecl;
  procedure enableDebugLogging(enabled: Boolean); cdecl;
  {Properties}
  property POP_BACK_STACK_INCLUSIVE: Integer read _GetPOP_BACK_STACK_INCLUSIVE;
end;

[JavaSignature('android/app/FragmentManager')]
JFragmentManager = interface(JObject)
['{4B06A33C-C4AF-4BE8-82E2-82E3B8BCBFCF}']
  {Methods}
  procedure addOnBackStackChangedListener(listener: JFragmentManager_OnBackStackChangedListener); cdecl;
  function beginTransaction: JFragmentTransaction; cdecl;
  procedure dump(prefix: JString; fd: JFileDescriptor; writer: JPrintWriter; args: TJavaObjectArray<JString>); cdecl;
  function executePendingTransactions: Boolean; cdecl;
  function findFragmentById(id: Integer): JFragment; cdecl;
  function findFragmentByTag(tag: JString): JFragment; cdecl;
  function getBackStackEntryAt(index: Integer): JFragmentManager_BackStackEntry; cdecl;
  function getBackStackEntryCount: Integer; cdecl;
  function getFragment(bundle: JBundle; key: JString): JFragment; cdecl;
  procedure invalidateOptionsMenu; cdecl;
  function isDestroyed: Boolean; cdecl;
  procedure popBackStack; cdecl; overload;
  procedure popBackStack(name: JString; flags: Integer); cdecl; overload;
  procedure popBackStack(id: Integer; flags: Integer); cdecl; overload;
  function popBackStackImmediate: Boolean; cdecl; overload;
  function popBackStackImmediate(name: JString; flags: Integer): Boolean; cdecl; overload;
  function popBackStackImmediate(id: Integer; flags: Integer): Boolean; cdecl; overload;
  procedure putFragment(bundle: JBundle; key: JString; fragment: JFragment); cdecl;
  procedure removeOnBackStackChangedListener(listener: JFragmentManager_OnBackStackChangedListener); cdecl;
  function saveFragmentInstanceState(f: JFragment): JFragment_SavedState; cdecl;
end;
TJFragmentManager = class(TJavaGenericImport<JFragmentManagerClass, JFragmentManager>) end;

JNotificationClass = interface(JObjectClass)
['{4584515F-7E22-49A9-A55B-A6DE3BFD7E1C}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  function _GetDEFAULT_ALL: Integer;
  function _GetDEFAULT_LIGHTS: Integer;
  function _GetDEFAULT_SOUND: Integer;
  function _GetDEFAULT_VIBRATE: Integer;
  function _GetFLAG_AUTO_CANCEL: Integer;
  function _GetFLAG_FOREGROUND_SERVICE: Integer;
  function _GetFLAG_HIGH_PRIORITY: Integer;
  function _GetFLAG_INSISTENT: Integer;
  function _GetFLAG_NO_CLEAR: Integer;
  function _GetFLAG_ONGOING_EVENT: Integer;
  function _GetFLAG_ONLY_ALERT_ONCE: Integer;
  function _GetFLAG_SHOW_LIGHTS: Integer;
  function _GetPRIORITY_DEFAULT: Integer;
  function _GetPRIORITY_HIGH: Integer;
  function _GetPRIORITY_LOW: Integer;
  function _GetPRIORITY_MAX: Integer;
  function _GetPRIORITY_MIN: Integer;
  function _GetSTREAM_DEFAULT: Integer;
  {Methods}
  function init: JNotification; cdecl; overload;
  function init(icon: Integer; tickerText: JCharSequence; when: Int64): JNotification; cdecl; overload;//Deprecated
  function init(parcel: JParcel): JNotification; cdecl; overload;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
  property DEFAULT_ALL: Integer read _GetDEFAULT_ALL;
  property DEFAULT_LIGHTS: Integer read _GetDEFAULT_LIGHTS;
  property DEFAULT_SOUND: Integer read _GetDEFAULT_SOUND;
  property DEFAULT_VIBRATE: Integer read _GetDEFAULT_VIBRATE;
  property FLAG_AUTO_CANCEL: Integer read _GetFLAG_AUTO_CANCEL;
  property FLAG_FOREGROUND_SERVICE: Integer read _GetFLAG_FOREGROUND_SERVICE;
  property FLAG_HIGH_PRIORITY: Integer read _GetFLAG_HIGH_PRIORITY;
  property FLAG_INSISTENT: Integer read _GetFLAG_INSISTENT;
  property FLAG_NO_CLEAR: Integer read _GetFLAG_NO_CLEAR;
  property FLAG_ONGOING_EVENT: Integer read _GetFLAG_ONGOING_EVENT;
  property FLAG_ONLY_ALERT_ONCE: Integer read _GetFLAG_ONLY_ALERT_ONCE;
  property FLAG_SHOW_LIGHTS: Integer read _GetFLAG_SHOW_LIGHTS;
  property PRIORITY_DEFAULT: Integer read _GetPRIORITY_DEFAULT;
  property PRIORITY_HIGH: Integer read _GetPRIORITY_HIGH;
  property PRIORITY_LOW: Integer read _GetPRIORITY_LOW;
  property PRIORITY_MAX: Integer read _GetPRIORITY_MAX;
  property PRIORITY_MIN: Integer read _GetPRIORITY_MIN;
  property STREAM_DEFAULT: Integer read _GetSTREAM_DEFAULT;
end;

[JavaSignature('android/app/Notification')]
JNotification = interface(JObject)
['{4BCE0ADE-6C94-464C-806C-115E4F4DAFF3}']
  {Property Methods}
  function _GetaudioStreamType: Integer;
  procedure _SetaudioStreamType(Value: Integer);
  function _GetbigContentView: JRemoteViews;
  procedure _SetbigContentView(Value: JRemoteViews);
  function _GetcontentIntent: JPendingIntent;
  procedure _SetcontentIntent(Value: JPendingIntent);
  function _GetcontentView: JRemoteViews;
  procedure _SetcontentView(Value: JRemoteViews);
  function _Getdefaults: Integer;
  procedure _Setdefaults(Value: Integer);
  function _GetdeleteIntent: JPendingIntent;
  procedure _SetdeleteIntent(Value: JPendingIntent);
  function _Getflags: Integer;
  procedure _Setflags(Value: Integer);
  function _GetfullScreenIntent: JPendingIntent;
  procedure _SetfullScreenIntent(Value: JPendingIntent);
  function _Geticon: Integer;
  procedure _Seticon(Value: Integer);
  function _GeticonLevel: Integer;
  procedure _SeticonLevel(Value: Integer);
  function _GetlargeIcon: JBitmap;
  procedure _SetlargeIcon(Value: JBitmap);
  function _GetledARGB: Integer;
  procedure _SetledARGB(Value: Integer);
  function _GetledOffMS: Integer;
  procedure _SetledOffMS(Value: Integer);
  function _GetledOnMS: Integer;
  procedure _SetledOnMS(Value: Integer);
  function _Getnumber: Integer;
  procedure _Setnumber(Value: Integer);
  function _Getpriority: Integer;
  procedure _Setpriority(Value: Integer);
  function _Getsound: Jnet_Uri;
  procedure _Setsound(Value: Jnet_Uri);
  function _GettickerText: JCharSequence;
  procedure _SettickerText(Value: JCharSequence);
  function _GettickerView: JRemoteViews;
  procedure _SettickerView(Value: JRemoteViews);
  function _Getvibrate: TJavaArray<Int64>;
  procedure _Setvibrate(Value: TJavaArray<Int64>);
  function _Getwhen: Int64;
  procedure _Setwhen(Value: Int64);
  {Methods}
  function clone: JNotification; cdecl;
  function describeContents: Integer; cdecl;
  procedure setLatestEventInfo(context: JContext; contentTitle: JCharSequence; contentText: JCharSequence; contentIntent: JPendingIntent); cdecl;//Deprecated
  function toString: JString; cdecl;
  procedure writeToParcel(parcel: JParcel; flags: Integer); cdecl;
  {Properties}
  property audioStreamType: Integer read _GetaudioStreamType write _SetaudioStreamType;
  property bigContentView: JRemoteViews read _GetbigContentView write _SetbigContentView;
  property contentIntent: JPendingIntent read _GetcontentIntent write _SetcontentIntent;
  property contentView: JRemoteViews read _GetcontentView write _SetcontentView;
  property defaults: Integer read _Getdefaults write _Setdefaults;
  property deleteIntent: JPendingIntent read _GetdeleteIntent write _SetdeleteIntent;
  property flags: Integer read _Getflags write _Setflags;
  property fullScreenIntent: JPendingIntent read _GetfullScreenIntent write _SetfullScreenIntent;
  property icon: Integer read _Geticon write _Seticon;
  property iconLevel: Integer read _GeticonLevel write _SeticonLevel;
  property largeIcon: JBitmap read _GetlargeIcon write _SetlargeIcon;
  property ledARGB: Integer read _GetledARGB write _SetledARGB;
  property ledOffMS: Integer read _GetledOffMS write _SetledOffMS;
  property ledOnMS: Integer read _GetledOnMS write _SetledOnMS;
  property number: Integer read _Getnumber write _Setnumber;
  property priority: Integer read _Getpriority write _Setpriority;
  property sound: Jnet_Uri read _Getsound write _Setsound;
  property tickerText: JCharSequence read _GettickerText write _SettickerText;
  property tickerView: JRemoteViews read _GettickerView write _SettickerView;
  property vibrate: TJavaArray<Int64> read _Getvibrate write _Setvibrate;
  property when: Int64 read _Getwhen write _Setwhen;
end;
TJNotification = class(TJavaGenericImport<JNotificationClass, JNotification>) end;

JApplication_ActivityLifecycleCallbacksClass = interface(IJavaClass)
['{82C4F2D1-0E03-4C87-940A-E2EE997EAC0C}']
end;

[JavaSignature('android/app/Application$ActivityLifecycleCallbacks')]
JApplication_ActivityLifecycleCallbacks = interface(IJavaInstance)
['{45381B4E-F44D-4BB9-883F-CB7DC2EC4AE3}']
  {Methods}
  procedure onActivityCreated(activity: JActivity; savedInstanceState: JBundle); cdecl;
  procedure onActivityDestroyed(activity: JActivity); cdecl;
  procedure onActivityPaused(activity: JActivity); cdecl;
  procedure onActivityResumed(activity: JActivity); cdecl;
  procedure onActivitySaveInstanceState(activity: JActivity; outState: JBundle); cdecl;
  procedure onActivityStarted(activity: JActivity); cdecl;
  procedure onActivityStopped(activity: JActivity); cdecl;
end;
TJApplication_ActivityLifecycleCallbacks = class(TJavaGenericImport<JApplication_ActivityLifecycleCallbacksClass, JApplication_ActivityLifecycleCallbacks>) end;

JFragmentManager_OnBackStackChangedListenerClass = interface(IJavaClass)
['{7255BA0C-3B6C-4C55-B7EB-50A193BC8186}']
end;

[JavaSignature('android/app/FragmentManager$OnBackStackChangedListener')]
JFragmentManager_OnBackStackChangedListener = interface(IJavaInstance)
['{B3418C11-9DEC-46E8-B17D-262F80649BC8}']
  {Methods}
  procedure onBackStackChanged; cdecl;
end;
TJFragmentManager_OnBackStackChangedListener = class(TJavaGenericImport<JFragmentManager_OnBackStackChangedListenerClass, JFragmentManager_OnBackStackChangedListener>) end;

JServiceClass = interface(JContextWrapperClass)
['{50459866-E20A-4564-B22B-9A8AB2716406}']
  {Property Methods}
  function _GetSTART_CONTINUATION_MASK: Integer;
  function _GetSTART_FLAG_REDELIVERY: Integer;
  function _GetSTART_FLAG_RETRY: Integer;
  function _GetSTART_NOT_STICKY: Integer;
  function _GetSTART_REDELIVER_INTENT: Integer;
  function _GetSTART_STICKY: Integer;
  function _GetSTART_STICKY_COMPATIBILITY: Integer;
  {Methods}
  function init: JService; cdecl;
  {Properties}
  property START_CONTINUATION_MASK: Integer read _GetSTART_CONTINUATION_MASK;
  property START_FLAG_REDELIVERY: Integer read _GetSTART_FLAG_REDELIVERY;
  property START_FLAG_RETRY: Integer read _GetSTART_FLAG_RETRY;
  property START_NOT_STICKY: Integer read _GetSTART_NOT_STICKY;
  property START_REDELIVER_INTENT: Integer read _GetSTART_REDELIVER_INTENT;
  property START_STICKY: Integer read _GetSTART_STICKY;
  property START_STICKY_COMPATIBILITY: Integer read _GetSTART_STICKY_COMPATIBILITY;
end;

[JavaSignature('android/app/Service')]
JService = interface(JContextWrapper)
['{C1C969BF-8725-4920-9691-4981E4892C1F}']
  {Methods}
  function getApplication: JApplication; cdecl;
  function onBind(intent: JIntent): JIBinder; cdecl;
  procedure onConfigurationChanged(newConfig: JConfiguration); cdecl;
  procedure onCreate; cdecl;
  procedure onDestroy; cdecl;
  procedure onLowMemory; cdecl;
  procedure onRebind(intent: JIntent); cdecl;
  procedure onStart(intent: JIntent; startId: Integer); cdecl;//Deprecated
  function onStartCommand(intent: JIntent; flags: Integer; startId: Integer): Integer; cdecl;
  procedure onTaskRemoved(rootIntent: JIntent); cdecl;
  procedure onTrimMemory(level: Integer); cdecl;
  function onUnbind(intent: JIntent): Boolean; cdecl;
  procedure startForeground(id: Integer; notification: JNotification); cdecl;
  procedure stopForeground(removeNotification: Boolean); cdecl;
  procedure stopSelf; cdecl; overload;
  procedure stopSelf(startId: Integer); cdecl; overload;
  function stopSelfResult(startId: Integer): Boolean; cdecl;
end;
TJService = class(TJavaGenericImport<JServiceClass, JService>) end;

JApplicationClass = interface(JContextWrapperClass)
['{B2E72B31-70E4-4B3A-A067-F11C002F8275}']
  {Methods}
  function init: JApplication; cdecl;
end;

[JavaSignature('android/app/Application')]
JApplication = interface(JContextWrapper)
['{28FC188F-36BA-4C06-843B-22F715AC831D}']
  {Methods}
  procedure onConfigurationChanged(newConfig: JConfiguration); cdecl;
  procedure onCreate; cdecl;
  procedure onLowMemory; cdecl;
  procedure onTerminate; cdecl;
  procedure onTrimMemory(level: Integer); cdecl;
  procedure registerActivityLifecycleCallbacks(callback: JApplication_ActivityLifecycleCallbacks); cdecl;
  procedure registerComponentCallbacks(callback: JComponentCallbacks); cdecl;
  procedure unregisterActivityLifecycleCallbacks(callback: JApplication_ActivityLifecycleCallbacks); cdecl;
  procedure unregisterComponentCallbacks(callback: JComponentCallbacks); cdecl;
end;
TJApplication = class(TJavaGenericImport<JApplicationClass, JApplication>) end;

JTaskStackBuilderClass = interface(JObjectClass)
['{CBF458C4-738C-4F67-BCF1-C94E8A1DA3FE}']
  {Methods}
  function create(context: JContext): JTaskStackBuilder; cdecl;
end;

[JavaSignature('android/app/TaskStackBuilder')]
JTaskStackBuilder = interface(JObject)
['{53D84177-E954-404C-A9C9-ED6578A29492}']
  {Methods}
  function addNextIntent(nextIntent: JIntent): JTaskStackBuilder; cdecl;
  function addNextIntentWithParentStack(nextIntent: JIntent): JTaskStackBuilder; cdecl;
  function addParentStack(sourceActivity: JActivity): JTaskStackBuilder; cdecl; overload;
  function addParentStack(sourceActivityClass: Jlang_Class): JTaskStackBuilder; cdecl; overload;
  function addParentStack(sourceActivityName: JComponentName): JTaskStackBuilder; cdecl; overload;
  function editIntentAt(index: Integer): JIntent; cdecl;
  function getIntentCount: Integer; cdecl;
  function getIntents: TJavaObjectArray<JIntent>; cdecl;
  function getPendingIntent(requestCode: Integer; flags: Integer): JPendingIntent; cdecl; overload;
  function getPendingIntent(requestCode: Integer; flags: Integer; options: JBundle): JPendingIntent; cdecl; overload;
  procedure startActivities; cdecl; overload;
  procedure startActivities(options: JBundle); cdecl; overload;
end;
TJTaskStackBuilder = class(TJavaGenericImport<JTaskStackBuilderClass, JTaskStackBuilder>) end;

JLoaderManagerClass = interface(JObjectClass)
['{50FB82E8-7ED8-409E-BC2D-0A385DFE7B94}']
  {Methods}
  function init: JLoaderManager; cdecl;
  procedure enableDebugLogging(enabled: Boolean); cdecl;
end;

[JavaSignature('android/app/LoaderManager')]
JLoaderManager = interface(JObject)
['{D45E200D-E969-42B8-9537-22C62850C7C5}']
  {Methods}
  procedure destroyLoader(id: Integer); cdecl;
  procedure dump(prefix: JString; fd: JFileDescriptor; writer: JPrintWriter; args: TJavaObjectArray<JString>); cdecl;
  function getLoader(id: Integer): JLoader; cdecl;
  function initLoader(id: Integer; args: JBundle; callback: JLoaderManager_LoaderCallbacks): JLoader; cdecl;
  function restartLoader(id: Integer; args: JBundle; callback: JLoaderManager_LoaderCallbacks): JLoader; cdecl;
end;
TJLoaderManager = class(TJavaGenericImport<JLoaderManagerClass, JLoaderManager>) end;

JFragmentClass = interface(JObjectClass)
['{3C54B094-6ECA-4E7F-9A49-E8B77FE2B156}']
  {Methods}
  function init: JFragment; cdecl;
  function instantiate(context: JContext; fname: JString): JFragment; cdecl; overload;
  function instantiate(context: JContext; fname: JString; args: JBundle): JFragment; cdecl; overload;
end;

[JavaSignature('android/app/Fragment')]
JFragment = interface(JObject)
['{A17DB3FE-8968-475D-988E-0CB75BE207CF}']
  {Methods}
  procedure dump(prefix: JString; fd: JFileDescriptor; writer: JPrintWriter; args: TJavaObjectArray<JString>); cdecl;
  function equals(o: JObject): Boolean; cdecl;
  function getActivity: JActivity; cdecl;
  function getArguments: JBundle; cdecl;
  function getChildFragmentManager: JFragmentManager; cdecl;
  function getFragmentManager: JFragmentManager; cdecl;
  function getId: Integer; cdecl;
  function getLoaderManager: JLoaderManager; cdecl;
  function getParentFragment: JFragment; cdecl;
  function getResources: JResources; cdecl;
  function getRetainInstance: Boolean; cdecl;
  function getString(resId: Integer): JString; cdecl; overload;
  function getTag: JString; cdecl;
  function getTargetFragment: JFragment; cdecl;
  function getTargetRequestCode: Integer; cdecl;
  function getText(resId: Integer): JCharSequence; cdecl;
  function getUserVisibleHint: Boolean; cdecl;
  function getView: JView; cdecl;
  function hashCode: Integer; cdecl;
  function isAdded: Boolean; cdecl;
  function isDetached: Boolean; cdecl;
  function isHidden: Boolean; cdecl;
  function isInLayout: Boolean; cdecl;
  function isRemoving: Boolean; cdecl;
  function isResumed: Boolean; cdecl;
  function isVisible: Boolean; cdecl;
  procedure onActivityCreated(savedInstanceState: JBundle); cdecl;
  procedure onActivityResult(requestCode: Integer; resultCode: Integer; data: JIntent); cdecl;
  procedure onAttach(activity: JActivity); cdecl;
  procedure onConfigurationChanged(newConfig: JConfiguration); cdecl;
  function onContextItemSelected(item: JMenuItem): Boolean; cdecl;
  procedure onCreate(savedInstanceState: JBundle); cdecl;
  procedure onCreateContextMenu(menu: JContextMenu; v: JView; menuInfo: JContextMenu_ContextMenuInfo); cdecl;
  procedure onCreateOptionsMenu(menu: JMenu; inflater: JMenuInflater); cdecl;
  function onCreateView(inflater: JLayoutInflater; container: JViewGroup; savedInstanceState: JBundle): JView; cdecl;
  procedure onDestroy; cdecl;
  procedure onDestroyOptionsMenu; cdecl;
  procedure onDestroyView; cdecl;
  procedure onDetach; cdecl;
  procedure onHiddenChanged(hidden: Boolean); cdecl;
  procedure onInflate(attrs: JAttributeSet; savedInstanceState: JBundle); cdecl; overload;//Deprecated
  procedure onInflate(activity: JActivity; attrs: JAttributeSet; savedInstanceState: JBundle); cdecl; overload;
  procedure onLowMemory; cdecl;
  function onOptionsItemSelected(item: JMenuItem): Boolean; cdecl;
  procedure onOptionsMenuClosed(menu: JMenu); cdecl;
  procedure onPause; cdecl;
  procedure onPrepareOptionsMenu(menu: JMenu); cdecl;
  procedure onResume; cdecl;
  procedure onSaveInstanceState(outState: JBundle); cdecl;
  procedure onStart; cdecl;
  procedure onStop; cdecl;
  procedure onTrimMemory(level: Integer); cdecl;
  procedure onViewCreated(view: JView; savedInstanceState: JBundle); cdecl;
  procedure onViewStateRestored(savedInstanceState: JBundle); cdecl;
  procedure registerForContextMenu(view: JView); cdecl;
  procedure setArguments(args: JBundle); cdecl;
  procedure setHasOptionsMenu(hasMenu: Boolean); cdecl;
  procedure setInitialSavedState(state: JFragment_SavedState); cdecl;
  procedure setMenuVisibility(menuVisible: Boolean); cdecl;
  procedure setRetainInstance(retain: Boolean); cdecl;
  procedure setTargetFragment(fragment: JFragment; requestCode: Integer); cdecl;
  procedure setUserVisibleHint(isVisibleToUser: Boolean); cdecl;
  procedure startActivity(intent: JIntent); cdecl; overload;
  procedure startActivity(intent: JIntent; options: JBundle); cdecl; overload;
  procedure startActivityForResult(intent: JIntent; requestCode: Integer); cdecl; overload;
  procedure startActivityForResult(intent: JIntent; requestCode: Integer; options: JBundle); cdecl; overload;
  function toString: JString; cdecl;
  procedure unregisterForContextMenu(view: JView); cdecl;
end;
TJFragment = class(TJavaGenericImport<JFragmentClass, JFragment>) end;

JAlarmManagerClass = interface(JObjectClass)
['{B26537D0-A769-4DD1-A5C3-2E2D9344193B}']
  {Property Methods}
  function _GetELAPSED_REALTIME: Integer;
  function _GetELAPSED_REALTIME_WAKEUP: Integer;
  function _GetINTERVAL_DAY: Int64;
  function _GetINTERVAL_FIFTEEN_MINUTES: Int64;
  function _GetINTERVAL_HALF_DAY: Int64;
  function _GetINTERVAL_HALF_HOUR: Int64;
  function _GetINTERVAL_HOUR: Int64;
  function _GetRTC: Integer;
  function _GetRTC_WAKEUP: Integer;
  {Properties}
  property ELAPSED_REALTIME: Integer read _GetELAPSED_REALTIME;
  property ELAPSED_REALTIME_WAKEUP: Integer read _GetELAPSED_REALTIME_WAKEUP;
  property INTERVAL_DAY: Int64 read _GetINTERVAL_DAY;
  property INTERVAL_FIFTEEN_MINUTES: Int64 read _GetINTERVAL_FIFTEEN_MINUTES;
  property INTERVAL_HALF_DAY: Int64 read _GetINTERVAL_HALF_DAY;
  property INTERVAL_HALF_HOUR: Int64 read _GetINTERVAL_HALF_HOUR;
  property INTERVAL_HOUR: Int64 read _GetINTERVAL_HOUR;
  property RTC: Integer read _GetRTC;
  property RTC_WAKEUP: Integer read _GetRTC_WAKEUP;
end;

[JavaSignature('android/app/AlarmManager')]
JAlarmManager = interface(JObject)
['{D4B2A2E3-48AD-491C-823B-DC301F6DA456}']
  {Methods}
  procedure cancel(operation: JPendingIntent); cdecl;
  procedure &set(type_: Integer; triggerAtMillis: Int64; operation: JPendingIntent); cdecl;
  procedure setInexactRepeating(type_: Integer; triggerAtMillis: Int64; intervalMillis: Int64; operation: JPendingIntent); cdecl;
  procedure setRepeating(type_: Integer; triggerAtMillis: Int64; intervalMillis: Int64; operation: JPendingIntent); cdecl;
  procedure setTime(millis: Int64); cdecl;
  procedure setTimeZone(timeZone: JString); cdecl;
end;
TJAlarmManager = class(TJavaGenericImport<JAlarmManagerClass, JAlarmManager>) end;

JActionBar_LayoutParamsClass = interface(JViewGroup_MarginLayoutParamsClass)
['{EE4F0254-72CB-4FAC-8827-660F24B0EE45}']
  {Methods}
  function init(c: JContext; attrs: JAttributeSet): JActionBar_LayoutParams; cdecl; overload;
  function init(width: Integer; height: Integer): JActionBar_LayoutParams; cdecl; overload;
  function init(width: Integer; height: Integer; gravity: Integer): JActionBar_LayoutParams; cdecl; overload;
  function init(gravity: Integer): JActionBar_LayoutParams; cdecl; overload;
  function init(source: JActionBar_LayoutParams): JActionBar_LayoutParams; cdecl; overload;
  function init(source: JViewGroup_LayoutParams): JActionBar_LayoutParams; cdecl; overload;
end;

[JavaSignature('android/app/ActionBar$LayoutParams')]
JActionBar_LayoutParams = interface(JViewGroup_MarginLayoutParams)
['{A6E6BC68-48E8-4FCC-8011-66834CDEC359}']
  {Property Methods}
  function _Getgravity: Integer;
  procedure _Setgravity(Value: Integer);
  {Properties}
  property gravity: Integer read _Getgravity write _Setgravity;
end;
TJActionBar_LayoutParams = class(TJavaGenericImport<JActionBar_LayoutParamsClass, JActionBar_LayoutParams>) end;

JDialogClass = interface(JObjectClass)
['{92218268-7592-4242-B041-BE3C84FFC844}']
  {Methods}
  function init(context: JContext): JDialog; cdecl; overload;
  function init(context: JContext; theme: Integer): JDialog; cdecl; overload;
end;

[JavaSignature('android/app/Dialog')]
JDialog = interface(JObject)
['{71959C70-0CB3-42B3-9654-30005BAEB3F5}']
  {Methods}
  procedure addContentView(view: JView; params: JViewGroup_LayoutParams); cdecl;
  procedure cancel; cdecl;
  procedure closeOptionsMenu; cdecl;
  procedure dismiss; cdecl;
  function dispatchGenericMotionEvent(ev: JMotionEvent): Boolean; cdecl;
  function dispatchKeyEvent(event: JKeyEvent): Boolean; cdecl;
  function dispatchKeyShortcutEvent(event: JKeyEvent): Boolean; cdecl;
  function dispatchTouchEvent(ev: JMotionEvent): Boolean; cdecl;
  function dispatchTrackballEvent(ev: JMotionEvent): Boolean; cdecl;
  function findViewById(id: Integer): JView; cdecl;
  function getActionBar: JActionBar; cdecl;
  function getContext: JContext; cdecl;
  function getCurrentFocus: JView; cdecl;
  function getLayoutInflater: JLayoutInflater; cdecl;
  function getOwnerActivity: JActivity; cdecl;
  function getVolumeControlStream: Integer; cdecl;
  function getWindow: JWindow; cdecl;
  procedure hide; cdecl;
  procedure invalidateOptionsMenu; cdecl;
  function isShowing: Boolean; cdecl;
  procedure onActionModeFinished(mode: JActionMode); cdecl;
  procedure onActionModeStarted(mode: JActionMode); cdecl;
  procedure onAttachedToWindow; cdecl;
  procedure onBackPressed; cdecl;
  procedure onContentChanged; cdecl;
  function onContextItemSelected(item: JMenuItem): Boolean; cdecl;
  procedure onContextMenuClosed(menu: JMenu); cdecl;
  procedure onCreateContextMenu(menu: JContextMenu; v: JView; menuInfo: JContextMenu_ContextMenuInfo); cdecl;
  function onCreateOptionsMenu(menu: JMenu): Boolean; cdecl;
  function onCreatePanelMenu(featureId: Integer; menu: JMenu): Boolean; cdecl;
  function onCreatePanelView(featureId: Integer): JView; cdecl;
  procedure onDetachedFromWindow; cdecl;
  function onGenericMotionEvent(event: JMotionEvent): Boolean; cdecl;
  function onKeyDown(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  function onKeyLongPress(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  function onKeyMultiple(keyCode: Integer; repeatCount: Integer; event: JKeyEvent): Boolean; cdecl;
  function onKeyShortcut(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  function onKeyUp(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  function onMenuItemSelected(featureId: Integer; item: JMenuItem): Boolean; cdecl;
  function onMenuOpened(featureId: Integer; menu: JMenu): Boolean; cdecl;
  function onOptionsItemSelected(item: JMenuItem): Boolean; cdecl;
  procedure onOptionsMenuClosed(menu: JMenu); cdecl;
  procedure onPanelClosed(featureId: Integer; menu: JMenu); cdecl;
  function onPrepareOptionsMenu(menu: JMenu): Boolean; cdecl;
  function onPreparePanel(featureId: Integer; view: JView; menu: JMenu): Boolean; cdecl;
  procedure onRestoreInstanceState(savedInstanceState: JBundle); cdecl;
  function onSaveInstanceState: JBundle; cdecl;
  function onSearchRequested: Boolean; cdecl;
  function onTouchEvent(event: JMotionEvent): Boolean; cdecl;
  function onTrackballEvent(event: JMotionEvent): Boolean; cdecl;
  procedure onWindowAttributesChanged(params: JWindowManager_LayoutParams); cdecl;
  procedure onWindowFocusChanged(hasFocus: Boolean); cdecl;
  function onWindowStartingActionMode(callback: JActionMode_Callback): JActionMode; cdecl;
  procedure openContextMenu(view: JView); cdecl;
  procedure openOptionsMenu; cdecl;
  procedure registerForContextMenu(view: JView); cdecl;
  function requestWindowFeature(featureId: Integer): Boolean; cdecl;
  procedure setCancelMessage(msg: JMessage); cdecl;
  procedure setCancelable(flag: Boolean); cdecl;
  procedure setCanceledOnTouchOutside(cancel: Boolean); cdecl;
  procedure setContentView(layoutResID: Integer); cdecl; overload;
  procedure setContentView(view: JView); cdecl; overload;
  procedure setContentView(view: JView; params: JViewGroup_LayoutParams); cdecl; overload;
  procedure setDismissMessage(msg: JMessage); cdecl;
  procedure setFeatureDrawable(featureId: Integer; drawable: JDrawable); cdecl;
  procedure setFeatureDrawableAlpha(featureId: Integer; alpha: Integer); cdecl;
  procedure setFeatureDrawableResource(featureId: Integer; resId: Integer); cdecl;
  procedure setFeatureDrawableUri(featureId: Integer; uri: Jnet_Uri); cdecl;
  procedure setOnCancelListener(listener: JDialogInterface_OnCancelListener); cdecl;
  procedure setOnDismissListener(listener: JDialogInterface_OnDismissListener); cdecl;
  procedure setOnKeyListener(onKeyListener: JDialogInterface_OnKeyListener); cdecl;
  procedure setOnShowListener(listener: JDialogInterface_OnShowListener); cdecl;
  procedure setOwnerActivity(activity: JActivity); cdecl;
  procedure setTitle(title: JCharSequence); cdecl; overload;
  procedure setTitle(titleId: Integer); cdecl; overload;
  procedure setVolumeControlStream(streamType: Integer); cdecl;
  procedure show; cdecl;
  procedure takeKeyEvents(get_: Boolean); cdecl;
  procedure unregisterForContextMenu(view: JView); cdecl;
end;
TJDialog = class(TJavaGenericImport<JDialogClass, JDialog>) end;

JAlertDialogClass = interface(JDialogClass)
['{36BE871D-2AD4-4CA9-8524-C14BEB73AC56}']
  {Property Methods}
  function _GetTHEME_DEVICE_DEFAULT_DARK: Integer;
  function _GetTHEME_DEVICE_DEFAULT_LIGHT: Integer;
  function _GetTHEME_HOLO_DARK: Integer;
  function _GetTHEME_HOLO_LIGHT: Integer;
  function _GetTHEME_TRADITIONAL: Integer;
  {Properties}
  property THEME_DEVICE_DEFAULT_DARK: Integer read _GetTHEME_DEVICE_DEFAULT_DARK;
  property THEME_DEVICE_DEFAULT_LIGHT: Integer read _GetTHEME_DEVICE_DEFAULT_LIGHT;
  property THEME_HOLO_DARK: Integer read _GetTHEME_HOLO_DARK;
  property THEME_HOLO_LIGHT: Integer read _GetTHEME_HOLO_LIGHT;
  property THEME_TRADITIONAL: Integer read _GetTHEME_TRADITIONAL;
end;

[JavaSignature('android/app/AlertDialog')]
JAlertDialog = interface(JDialog)
['{53DA68C7-828A-4527-8A92-1BC09462F4E2}']
  {Methods}
  function getButton(whichButton: Integer): JButton; cdecl;
  function getListView: JListView; cdecl;
  function onKeyDown(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  function onKeyUp(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  procedure setButton(whichButton: Integer; text: JCharSequence; msg: JMessage); cdecl; overload;
  procedure setButton(whichButton: Integer; text: JCharSequence; listener: JDialogInterface_OnClickListener); cdecl; overload;
  procedure setButton(text: JCharSequence; msg: JMessage); cdecl; overload;//Deprecated
  procedure setButton(text: JCharSequence; listener: JDialogInterface_OnClickListener); cdecl; overload;//Deprecated
  procedure setButton2(text: JCharSequence; msg: JMessage); cdecl; overload;//Deprecated
  procedure setButton2(text: JCharSequence; listener: JDialogInterface_OnClickListener); cdecl; overload;//Deprecated
  procedure setButton3(text: JCharSequence; msg: JMessage); cdecl; overload;//Deprecated
  procedure setButton3(text: JCharSequence; listener: JDialogInterface_OnClickListener); cdecl; overload;//Deprecated
  procedure setCustomTitle(customTitleView: JView); cdecl;
  procedure setIcon(resId: Integer); cdecl; overload;
  procedure setIcon(icon: JDrawable); cdecl; overload;
  procedure setIconAttribute(attrId: Integer); cdecl;
  procedure setInverseBackgroundForced(forceInverseBackground: Boolean); cdecl;
  procedure setMessage(message: JCharSequence); cdecl;
  procedure setTitle(title: JCharSequence); cdecl;
  procedure setView(view: JView); cdecl; overload;
  procedure setView(view: JView; viewSpacingLeft: Integer; viewSpacingTop: Integer; viewSpacingRight: Integer; viewSpacingBottom: Integer); cdecl; overload;
end;
TJAlertDialog = class(TJavaGenericImport<JAlertDialogClass, JAlertDialog>) end;

JActionBar_OnNavigationListenerClass = interface(IJavaClass)
['{AE55F295-D4A1-4ED2-989C-83DCA6409D2A}']
end;

[JavaSignature('android/app/ActionBar$OnNavigationListener')]
JActionBar_OnNavigationListener = interface(IJavaInstance)
['{E16E1F51-1909-4712-B76A-0CB4F3BC178E}']
  {Methods}
  function onNavigationItemSelected(itemPosition: Integer; itemId: Int64): Boolean; cdecl;
end;
TJActionBar_OnNavigationListener = class(TJavaGenericImport<JActionBar_OnNavigationListenerClass, JActionBar_OnNavigationListener>) end;

JActionBar_TabListenerClass = interface(IJavaClass)
['{3F89E13F-E7E0-45A7-9A8F-D64422BA7B0E}']
end;

[JavaSignature('android/app/ActionBar$TabListener')]
JActionBar_TabListener = interface(IJavaInstance)
['{C7B5769A-4307-4E57-8F73-31E022332836}']
  {Methods}
  procedure onTabReselected(tab: JActionBar_Tab; ft: JFragmentTransaction); cdecl;
  procedure onTabSelected(tab: JActionBar_Tab; ft: JFragmentTransaction); cdecl;
  procedure onTabUnselected(tab: JActionBar_Tab; ft: JFragmentTransaction); cdecl;
end;
TJActionBar_TabListener = class(TJavaGenericImport<JActionBar_TabListenerClass, JActionBar_TabListener>) end;

JPendingIntentClass = interface(JObjectClass)
['{3C96E145-86EE-487F-9068-C48BA32D0E89}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  function _GetFLAG_CANCEL_CURRENT: Integer;
  function _GetFLAG_NO_CREATE: Integer;
  function _GetFLAG_ONE_SHOT: Integer;
  function _GetFLAG_UPDATE_CURRENT: Integer;
  {Methods}
  function getActivities(context: JContext; requestCode: Integer; intents: TJavaObjectArray<JIntent>; flags: Integer): JPendingIntent; cdecl; overload;
  function getActivities(context: JContext; requestCode: Integer; intents: TJavaObjectArray<JIntent>; flags: Integer; options: JBundle): JPendingIntent; cdecl; overload;
  function getActivity(context: JContext; requestCode: Integer; intent: JIntent; flags: Integer): JPendingIntent; cdecl; overload;
  function getActivity(context: JContext; requestCode: Integer; intent: JIntent; flags: Integer; options: JBundle): JPendingIntent; cdecl; overload;
  function getBroadcast(context: JContext; requestCode: Integer; intent: JIntent; flags: Integer): JPendingIntent; cdecl;
  function getService(context: JContext; requestCode: Integer; intent: JIntent; flags: Integer): JPendingIntent; cdecl;
  function readPendingIntentOrNullFromParcel(in_: JParcel): JPendingIntent; cdecl;
  procedure writePendingIntentOrNullToParcel(sender: JPendingIntent; out_: JParcel); cdecl;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
  property FLAG_CANCEL_CURRENT: Integer read _GetFLAG_CANCEL_CURRENT;
  property FLAG_NO_CREATE: Integer read _GetFLAG_NO_CREATE;
  property FLAG_ONE_SHOT: Integer read _GetFLAG_ONE_SHOT;
  property FLAG_UPDATE_CURRENT: Integer read _GetFLAG_UPDATE_CURRENT;
end;

[JavaSignature('android/app/PendingIntent')]
JPendingIntent = interface(JObject)
['{6475B1E1-B04F-4269-B4EE-33EED669FDDD}']
  {Methods}
  procedure cancel; cdecl;
  function describeContents: Integer; cdecl;
  function equals(otherObj: JObject): Boolean; cdecl;
  function getCreatorPackage: JString; cdecl;
  function getCreatorUid: Integer; cdecl;
  function getCreatorUserHandle: JUserHandle; cdecl;
  function getIntentSender: JIntentSender; cdecl;
  function getTargetPackage: JString; cdecl;//Deprecated
  function hashCode: Integer; cdecl;
  procedure send; cdecl; overload;
  procedure send(code: Integer); cdecl; overload;
  procedure send(context: JContext; code: Integer; intent: JIntent); cdecl; overload;
  procedure send(code: Integer; onFinished: JPendingIntent_OnFinished; handler: JHandler); cdecl; overload;
  procedure send(context: JContext; code: Integer; intent: JIntent; onFinished: JPendingIntent_OnFinished; handler: JHandler); cdecl; overload;
  procedure send(context: JContext; code: Integer; intent: JIntent; onFinished: JPendingIntent_OnFinished; handler: JHandler; requiredPermission: JString); cdecl; overload;
  function toString: JString; cdecl;
  procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
end;
TJPendingIntent = class(TJavaGenericImport<JPendingIntentClass, JPendingIntent>) end;

JFragment_SavedStateClass = interface(JObjectClass)
['{5BA60ED2-A67D-4A23-8B92-E4F26A4799D9}']
  {Property Methods}
  function _GetCREATOR: JParcelable_ClassLoaderCreator;
  {Properties}
  property CREATOR: JParcelable_ClassLoaderCreator read _GetCREATOR;
end;

[JavaSignature('android/app/Fragment$SavedState')]
JFragment_SavedState = interface(JObject)
['{76BE11A9-82B9-4AC9-8C54-80BAFBD3A578}']
  {Methods}
  function describeContents: Integer; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
end;
TJFragment_SavedState = class(TJavaGenericImport<JFragment_SavedStateClass, JFragment_SavedState>) end;

JLoaderManager_LoaderCallbacksClass = interface(IJavaClass)
['{5143414F-8245-45AA-A70D-DB8CA12DC90C}']
end;

[JavaSignature('android/app/LoaderManager$LoaderCallbacks')]
JLoaderManager_LoaderCallbacks = interface(IJavaInstance)
['{F28327C2-3A34-4296-997C-F69AEDE35ECF}']
  {Methods}
  function onCreateLoader(id: Integer; args: JBundle): JLoader; cdecl;
  procedure onLoadFinished(loader: JLoader; data: JObject); cdecl;
  procedure onLoaderReset(loader: JLoader); cdecl;
end;
TJLoaderManager_LoaderCallbacks = class(TJavaGenericImport<JLoaderManager_LoaderCallbacksClass, JLoaderManager_LoaderCallbacks>) end;

JAlertDialog_BuilderClass = interface(JObjectClass)
['{69C77054-20E9-4492-BCBB-FB1F9F062BFD}']
  {Methods}
  function init(context: JContext): JAlertDialog_Builder; cdecl; overload;
  function init(context: JContext; theme: Integer): JAlertDialog_Builder; cdecl; overload;
end;

[JavaSignature('android/app/AlertDialog$Builder')]
JAlertDialog_Builder = interface(JObject)
['{25BC41E2-CA0A-4EA8-A854-993A0E86087C}']
  {Methods}
  function create: JAlertDialog; cdecl;
  function getContext: JContext; cdecl;
  function setAdapter(adapter: JListAdapter; listener: JDialogInterface_OnClickListener): JAlertDialog_Builder; cdecl;
  function setCancelable(cancelable: Boolean): JAlertDialog_Builder; cdecl;
  function setCursor(cursor: JCursor; listener: JDialogInterface_OnClickListener; labelColumn: JString): JAlertDialog_Builder; cdecl;
  function setCustomTitle(customTitleView: JView): JAlertDialog_Builder; cdecl;
  function setIcon(iconId: Integer): JAlertDialog_Builder; cdecl; overload;
  function setIcon(icon: JDrawable): JAlertDialog_Builder; cdecl; overload;
  function setIconAttribute(attrId: Integer): JAlertDialog_Builder; cdecl;
  function setInverseBackgroundForced(useInverseBackground: Boolean): JAlertDialog_Builder; cdecl;
  function setItems(itemsId: Integer; listener: JDialogInterface_OnClickListener): JAlertDialog_Builder; cdecl; overload;
  function setItems(items: TJavaObjectArray<JCharSequence>; listener: JDialogInterface_OnClickListener): JAlertDialog_Builder; cdecl; overload;
  function setMessage(messageId: Integer): JAlertDialog_Builder; cdecl; overload;
  function setMessage(message: JCharSequence): JAlertDialog_Builder; cdecl; overload;
  function setMultiChoiceItems(itemsId: Integer; checkedItems: TJavaArray<Boolean>; listener: JDialogInterface_OnMultiChoiceClickListener): JAlertDialog_Builder; cdecl; overload;
  function setMultiChoiceItems(items: TJavaObjectArray<JCharSequence>; checkedItems: TJavaArray<Boolean>; listener: JDialogInterface_OnMultiChoiceClickListener): JAlertDialog_Builder; cdecl; overload;
  function setMultiChoiceItems(cursor: JCursor; isCheckedColumn: JString; labelColumn: JString; listener: JDialogInterface_OnMultiChoiceClickListener): JAlertDialog_Builder; cdecl; overload;
  function setNegativeButton(textId: Integer; listener: JDialogInterface_OnClickListener): JAlertDialog_Builder; cdecl; overload;
  function setNegativeButton(text: JCharSequence; listener: JDialogInterface_OnClickListener): JAlertDialog_Builder; cdecl; overload;
  function setNeutralButton(textId: Integer; listener: JDialogInterface_OnClickListener): JAlertDialog_Builder; cdecl; overload;
  function setNeutralButton(text: JCharSequence; listener: JDialogInterface_OnClickListener): JAlertDialog_Builder; cdecl; overload;
  function setOnCancelListener(onCancelListener: JDialogInterface_OnCancelListener): JAlertDialog_Builder; cdecl;
  function setOnDismissListener(onDismissListener: JDialogInterface_OnDismissListener): JAlertDialog_Builder; cdecl;
  function setOnItemSelectedListener(listener: JAdapterView_OnItemSelectedListener): JAlertDialog_Builder; cdecl;
  function setOnKeyListener(onKeyListener: JDialogInterface_OnKeyListener): JAlertDialog_Builder; cdecl;
  function setPositiveButton(textId: Integer; listener: JDialogInterface_OnClickListener): JAlertDialog_Builder; cdecl; overload;
  function setPositiveButton(text: JCharSequence; listener: JDialogInterface_OnClickListener): JAlertDialog_Builder; cdecl; overload;
  function setSingleChoiceItems(itemsId: Integer; checkedItem: Integer; listener: JDialogInterface_OnClickListener): JAlertDialog_Builder; cdecl; overload;
  function setSingleChoiceItems(cursor: JCursor; checkedItem: Integer; labelColumn: JString; listener: JDialogInterface_OnClickListener): JAlertDialog_Builder; cdecl; overload;
  function setSingleChoiceItems(items: TJavaObjectArray<JCharSequence>; checkedItem: Integer; listener: JDialogInterface_OnClickListener): JAlertDialog_Builder; cdecl; overload;
  function setSingleChoiceItems(adapter: JListAdapter; checkedItem: Integer; listener: JDialogInterface_OnClickListener): JAlertDialog_Builder; cdecl; overload;
  function setTitle(titleId: Integer): JAlertDialog_Builder; cdecl; overload;
  function setTitle(title: JCharSequence): JAlertDialog_Builder; cdecl; overload;
  function setView(view: JView): JAlertDialog_Builder; cdecl;
  function show: JAlertDialog; cdecl;
end;
TJAlertDialog_Builder = class(TJavaGenericImport<JAlertDialog_BuilderClass, JAlertDialog_Builder>) end;

JActionBar_TabClass = interface(JObjectClass)
['{142282F8-609C-42B5-8953-9BD4E01A9412}']
  {Property Methods}
  function _GetINVALID_POSITION: Integer;
  {Methods}
  function init: JActionBar_Tab; cdecl;
  {Properties}
  property INVALID_POSITION: Integer read _GetINVALID_POSITION;
end;

[JavaSignature('android/app/ActionBar$Tab')]
JActionBar_Tab = interface(JObject)
['{75064B3A-7834-47BD-9C0E-5FCBE4668F27}']
  {Methods}
  function getContentDescription: JCharSequence; cdecl;
  function getCustomView: JView; cdecl;
  function getIcon: JDrawable; cdecl;
  function getPosition: Integer; cdecl;
  function getTag: JObject; cdecl;
  function getText: JCharSequence; cdecl;
  procedure select; cdecl;
  function setContentDescription(resId: Integer): JActionBar_Tab; cdecl; overload;
  function setContentDescription(contentDesc: JCharSequence): JActionBar_Tab; cdecl; overload;
  function setCustomView(view: JView): JActionBar_Tab; cdecl; overload;
  function setCustomView(layoutResId: Integer): JActionBar_Tab; cdecl; overload;
  function setIcon(icon: JDrawable): JActionBar_Tab; cdecl; overload;
  function setIcon(resId: Integer): JActionBar_Tab; cdecl; overload;
  function setTabListener(listener: JActionBar_TabListener): JActionBar_Tab; cdecl;
  function setTag(obj: JObject): JActionBar_Tab; cdecl;
  function setText(text: JCharSequence): JActionBar_Tab; cdecl; overload;
  function setText(resId: Integer): JActionBar_Tab; cdecl; overload;
end;
TJActionBar_Tab = class(TJavaGenericImport<JActionBar_TabClass, JActionBar_Tab>) end;

JActionBarClass = interface(JObjectClass)
['{C1A41981-8687-4794-91A5-AA384F60A546}']
  {Property Methods}
  function _GetDISPLAY_HOME_AS_UP: Integer;
  function _GetDISPLAY_SHOW_CUSTOM: Integer;
  function _GetDISPLAY_SHOW_HOME: Integer;
  function _GetDISPLAY_SHOW_TITLE: Integer;
  function _GetDISPLAY_USE_LOGO: Integer;
  function _GetNAVIGATION_MODE_LIST: Integer;
  function _GetNAVIGATION_MODE_STANDARD: Integer;
  function _GetNAVIGATION_MODE_TABS: Integer;
  {Methods}
  function init: JActionBar; cdecl;
  {Properties}
  property DISPLAY_HOME_AS_UP: Integer read _GetDISPLAY_HOME_AS_UP;
  property DISPLAY_SHOW_CUSTOM: Integer read _GetDISPLAY_SHOW_CUSTOM;
  property DISPLAY_SHOW_HOME: Integer read _GetDISPLAY_SHOW_HOME;
  property DISPLAY_SHOW_TITLE: Integer read _GetDISPLAY_SHOW_TITLE;
  property DISPLAY_USE_LOGO: Integer read _GetDISPLAY_USE_LOGO;
  property NAVIGATION_MODE_LIST: Integer read _GetNAVIGATION_MODE_LIST;
  property NAVIGATION_MODE_STANDARD: Integer read _GetNAVIGATION_MODE_STANDARD;
  property NAVIGATION_MODE_TABS: Integer read _GetNAVIGATION_MODE_TABS;
end;

[JavaSignature('android/app/ActionBar')]
JActionBar = interface(JObject)
['{607E590E-509A-40CB-A31C-448056752FAB}']
  {Methods}
  procedure addOnMenuVisibilityListener(listener: JActionBar_OnMenuVisibilityListener); cdecl;
  procedure addTab(tab: JActionBar_Tab); cdecl; overload;
  procedure addTab(tab: JActionBar_Tab; setSelected: Boolean); cdecl; overload;
  procedure addTab(tab: JActionBar_Tab; position: Integer); cdecl; overload;
  procedure addTab(tab: JActionBar_Tab; position: Integer; setSelected: Boolean); cdecl; overload;
  function getCustomView: JView; cdecl;
  function getDisplayOptions: Integer; cdecl;
  function getHeight: Integer; cdecl;
  function getNavigationItemCount: Integer; cdecl;
  function getNavigationMode: Integer; cdecl;
  function getSelectedNavigationIndex: Integer; cdecl;
  function getSelectedTab: JActionBar_Tab; cdecl;
  function getSubtitle: JCharSequence; cdecl;
  function getTabAt(index: Integer): JActionBar_Tab; cdecl;
  function getTabCount: Integer; cdecl;
  function getThemedContext: JContext; cdecl;
  function getTitle: JCharSequence; cdecl;
  procedure hide; cdecl;
  function isShowing: Boolean; cdecl;
  function newTab: JActionBar_Tab; cdecl;
  procedure removeAllTabs; cdecl;
  procedure removeOnMenuVisibilityListener(listener: JActionBar_OnMenuVisibilityListener); cdecl;
  procedure removeTab(tab: JActionBar_Tab); cdecl;
  procedure removeTabAt(position: Integer); cdecl;
  procedure selectTab(tab: JActionBar_Tab); cdecl;
  procedure setBackgroundDrawable(d: JDrawable); cdecl;
  procedure setCustomView(view: JView); cdecl; overload;
  procedure setCustomView(view: JView; layoutParams: JActionBar_LayoutParams); cdecl; overload;
  procedure setCustomView(resId: Integer); cdecl; overload;
  procedure setDisplayHomeAsUpEnabled(showHomeAsUp: Boolean); cdecl;
  procedure setDisplayOptions(options: Integer); cdecl; overload;
  procedure setDisplayOptions(options: Integer; mask: Integer); cdecl; overload;
  procedure setDisplayShowCustomEnabled(showCustom: Boolean); cdecl;
  procedure setDisplayShowHomeEnabled(showHome: Boolean); cdecl;
  procedure setDisplayShowTitleEnabled(showTitle: Boolean); cdecl;
  procedure setDisplayUseLogoEnabled(useLogo: Boolean); cdecl;
  procedure setHomeButtonEnabled(enabled: Boolean); cdecl;
  procedure setIcon(resId: Integer); cdecl; overload;
  procedure setIcon(icon: JDrawable); cdecl; overload;
  procedure setListNavigationCallbacks(adapter: JSpinnerAdapter; callback: JActionBar_OnNavigationListener); cdecl;
  procedure setLogo(resId: Integer); cdecl; overload;
  procedure setLogo(logo: JDrawable); cdecl; overload;
  procedure setNavigationMode(mode: Integer); cdecl;
  procedure setSelectedNavigationItem(position: Integer); cdecl;
  procedure setSplitBackgroundDrawable(d: JDrawable); cdecl;
  procedure setStackedBackgroundDrawable(d: JDrawable); cdecl;
  procedure setSubtitle(subtitle: JCharSequence); cdecl; overload;
  procedure setSubtitle(resId: Integer); cdecl; overload;
  procedure setTitle(title: JCharSequence); cdecl; overload;
  procedure setTitle(resId: Integer); cdecl; overload;
  procedure show; cdecl;
end;
TJActionBar = class(TJavaGenericImport<JActionBarClass, JActionBar>) end;

JPendingIntent_OnFinishedClass = interface(IJavaClass)
['{CA268228-C808-4F93-AF5A-F9B15D8A480C}']
end;

[JavaSignature('android/app/PendingIntent$OnFinished')]
JPendingIntent_OnFinished = interface(IJavaInstance)
['{7420C1F1-8701-4496-B2EB-20C427D3FC50}']
  {Methods}
  procedure onSendFinished(pendingIntent: JPendingIntent; intent: JIntent; resultCode: Integer; resultData: JString; resultExtras: JBundle); cdecl;
end;
TJPendingIntent_OnFinished = class(TJavaGenericImport<JPendingIntent_OnFinishedClass, JPendingIntent_OnFinished>) end;

JNotificationManagerClass = interface(JObjectClass)
['{66101C50-DAE9-4C81-8186-81A0A43A73BD}']
end;

[JavaSignature('android/app/NotificationManager')]
JNotificationManager = interface(JObject)
['{C3E111F8-A16B-4E14-AEDA-C4E38AF03C49}']
  {Methods}
  procedure cancel(id: Integer); cdecl; overload;
  procedure cancel(tag: JString; id: Integer); cdecl; overload;
  procedure cancelAll; cdecl;
  procedure notify(id: Integer; notification: JNotification); cdecl; overload;
  procedure notify(tag: JString; id: Integer; notification: JNotification); cdecl; overload;
end;
TJNotificationManager = class(TJavaGenericImport<JNotificationManagerClass, JNotificationManager>) end;

JIntentServiceClass = interface(JServiceClass)
['{46298ACB-D39D-4C26-B9BB-90888D68848F}']
  {Methods}
  function init(name: JString): JIntentService; cdecl;
end;

[JavaSignature('android/app/IntentService')]
JIntentService = interface(JService)
['{E5F9EDF4-5A96-49F9-A1FF-F69868B5946D}']
  {Methods}
  function onBind(intent: JIntent): JIBinder; cdecl;
  procedure onCreate; cdecl;
  procedure onDestroy; cdecl;
  procedure onStart(intent: JIntent; startId: Integer); cdecl;
  function onStartCommand(intent: JIntent; flags: Integer; startId: Integer): Integer; cdecl;
  procedure setIntentRedelivery(enabled: Boolean); cdecl;
end;
TJIntentService = class(TJavaGenericImport<JIntentServiceClass, JIntentService>) end;

JFragmentTransactionClass = interface(JObjectClass)
['{F09CC327-C48D-46CD-AE7A-4E7036F3C90E}']
  {Property Methods}
  function _GetTRANSIT_ENTER_MASK: Integer;
  function _GetTRANSIT_EXIT_MASK: Integer;
  function _GetTRANSIT_FRAGMENT_CLOSE: Integer;
  function _GetTRANSIT_FRAGMENT_FADE: Integer;
  function _GetTRANSIT_FRAGMENT_OPEN: Integer;
  function _GetTRANSIT_NONE: Integer;
  function _GetTRANSIT_UNSET: Integer;
  {Methods}
  function init: JFragmentTransaction; cdecl;
  {Properties}
  property TRANSIT_ENTER_MASK: Integer read _GetTRANSIT_ENTER_MASK;
  property TRANSIT_EXIT_MASK: Integer read _GetTRANSIT_EXIT_MASK;
  property TRANSIT_FRAGMENT_CLOSE: Integer read _GetTRANSIT_FRAGMENT_CLOSE;
  property TRANSIT_FRAGMENT_FADE: Integer read _GetTRANSIT_FRAGMENT_FADE;
  property TRANSIT_FRAGMENT_OPEN: Integer read _GetTRANSIT_FRAGMENT_OPEN;
  property TRANSIT_NONE: Integer read _GetTRANSIT_NONE;
  property TRANSIT_UNSET: Integer read _GetTRANSIT_UNSET;
end;

[JavaSignature('android/app/FragmentTransaction')]
JFragmentTransaction = interface(JObject)
['{A55DB157-F3A6-4F55-AD4C-6BC7A21C6FD8}']
  {Methods}
  function add(fragment: JFragment; tag: JString): JFragmentTransaction; cdecl; overload;
  function add(containerViewId: Integer; fragment: JFragment): JFragmentTransaction; cdecl; overload;
  function add(containerViewId: Integer; fragment: JFragment; tag: JString): JFragmentTransaction; cdecl; overload;
  function addToBackStack(name: JString): JFragmentTransaction; cdecl;
  function attach(fragment: JFragment): JFragmentTransaction; cdecl;
  function commit: Integer; cdecl;
  function commitAllowingStateLoss: Integer; cdecl;
  function detach(fragment: JFragment): JFragmentTransaction; cdecl;
  function disallowAddToBackStack: JFragmentTransaction; cdecl;
  function hide(fragment: JFragment): JFragmentTransaction; cdecl;
  function isAddToBackStackAllowed: Boolean; cdecl;
  function isEmpty: Boolean; cdecl;
  function remove(fragment: JFragment): JFragmentTransaction; cdecl;
  function replace(containerViewId: Integer; fragment: JFragment): JFragmentTransaction; cdecl; overload;
  function replace(containerViewId: Integer; fragment: JFragment; tag: JString): JFragmentTransaction; cdecl; overload;
  function setBreadCrumbShortTitle(res: Integer): JFragmentTransaction; cdecl; overload;
  function setBreadCrumbShortTitle(text: JCharSequence): JFragmentTransaction; cdecl; overload;
  function setBreadCrumbTitle(res: Integer): JFragmentTransaction; cdecl; overload;
  function setBreadCrumbTitle(text: JCharSequence): JFragmentTransaction; cdecl; overload;
  function setCustomAnimations(enter: Integer; exit: Integer): JFragmentTransaction; cdecl; overload;
  function setCustomAnimations(enter: Integer; exit: Integer; popEnter: Integer; popExit: Integer): JFragmentTransaction; cdecl; overload;
  function setTransition(transit: Integer): JFragmentTransaction; cdecl;
  function setTransitionStyle(styleRes: Integer): JFragmentTransaction; cdecl;
  function show(fragment: JFragment): JFragmentTransaction; cdecl;
end;
TJFragmentTransaction = class(TJavaGenericImport<JFragmentTransactionClass, JFragmentTransaction>) end;

JActivityClass = interface(JContextThemeWrapperClass)
['{5269A525-12C7-4E15-AE63-4BD764EB357D}']
  {Property Methods}
  function _GetDEFAULT_KEYS_DIALER: Integer;
  function _GetDEFAULT_KEYS_DISABLE: Integer;
  function _GetDEFAULT_KEYS_SEARCH_GLOBAL: Integer;
  function _GetDEFAULT_KEYS_SEARCH_LOCAL: Integer;
  function _GetDEFAULT_KEYS_SHORTCUT: Integer;
  function _GetRESULT_CANCELED: Integer;
  function _GetRESULT_FIRST_USER: Integer;
  function _GetRESULT_OK: Integer;
  {Methods}
  function init: JActivity; cdecl;
  {Properties}
  property DEFAULT_KEYS_DIALER: Integer read _GetDEFAULT_KEYS_DIALER;
  property DEFAULT_KEYS_DISABLE: Integer read _GetDEFAULT_KEYS_DISABLE;
  property DEFAULT_KEYS_SEARCH_GLOBAL: Integer read _GetDEFAULT_KEYS_SEARCH_GLOBAL;
  property DEFAULT_KEYS_SEARCH_LOCAL: Integer read _GetDEFAULT_KEYS_SEARCH_LOCAL;
  property DEFAULT_KEYS_SHORTCUT: Integer read _GetDEFAULT_KEYS_SHORTCUT;
  property RESULT_CANCELED: Integer read _GetRESULT_CANCELED;
  property RESULT_FIRST_USER: Integer read _GetRESULT_FIRST_USER;
  property RESULT_OK: Integer read _GetRESULT_OK;
end;

[JavaSignature('android/app/Activity')]
JActivity = interface(JContextThemeWrapper)
['{A91E133E-CBAB-48EB-99A8-AA047E90D3B8}']
  {Methods}
  procedure addContentView(view: JView; params: JViewGroup_LayoutParams); cdecl;
  procedure closeContextMenu; cdecl;
  procedure closeOptionsMenu; cdecl;
  function createPendingResult(requestCode: Integer; data: JIntent; flags: Integer): JPendingIntent; cdecl;
  procedure dismissDialog(id: Integer); cdecl;//Deprecated
  function dispatchGenericMotionEvent(ev: JMotionEvent): Boolean; cdecl;
  function dispatchKeyEvent(event: JKeyEvent): Boolean; cdecl;
  function dispatchKeyShortcutEvent(event: JKeyEvent): Boolean; cdecl;
  function dispatchTouchEvent(ev: JMotionEvent): Boolean; cdecl;
  function dispatchTrackballEvent(ev: JMotionEvent): Boolean; cdecl;
  procedure dump(prefix: JString; fd: JFileDescriptor; writer: JPrintWriter; args: TJavaObjectArray<JString>); cdecl;
  function findViewById(id: Integer): JView; cdecl;
  procedure finish; cdecl;
  procedure finishActivity(requestCode: Integer); cdecl;
  procedure finishActivityFromChild(child: JActivity; requestCode: Integer); cdecl;
  procedure finishAffinity; cdecl;
  procedure finishFromChild(child: JActivity); cdecl;
  function getActionBar: JActionBar; cdecl;
  function getApplication: JApplication; cdecl;
  function getCallingActivity: JComponentName; cdecl;
  function getCallingPackage: JString; cdecl;
  function getChangingConfigurations: Integer; cdecl;
  function getComponentName: JComponentName; cdecl;
  function getCurrentFocus: JView; cdecl;
  function getFragmentManager: JFragmentManager; cdecl;
  function getIntent: JIntent; cdecl;
  function getLastNonConfigurationInstance: JObject; cdecl;//Deprecated
  function getLayoutInflater: JLayoutInflater; cdecl;
  function getLoaderManager: JLoaderManager; cdecl;
  function getLocalClassName: JString; cdecl;
  function getMenuInflater: JMenuInflater; cdecl;
  function getParent: JActivity; cdecl;
  function getParentActivityIntent: JIntent; cdecl;
  function getPreferences(mode: Integer): JSharedPreferences; cdecl;
  function getRequestedOrientation: Integer; cdecl;
  function getSystemService(name: JString): JObject; cdecl;
  function getTaskId: Integer; cdecl;
  function getTitle: JCharSequence; cdecl;
  function getTitleColor: Integer; cdecl;
  function getVolumeControlStream: Integer; cdecl;
  function getWindow: JWindow; cdecl;
  function getWindowManager: JWindowManager; cdecl;
  function hasWindowFocus: Boolean; cdecl;
  procedure invalidateOptionsMenu; cdecl;
  function isChangingConfigurations: Boolean; cdecl;
  function isChild: Boolean; cdecl;
  function isDestroyed: Boolean; cdecl;
  function isFinishing: Boolean; cdecl;
  function isTaskRoot: Boolean; cdecl;
  function managedQuery(uri: Jnet_Uri; projection: TJavaObjectArray<JString>; selection: JString; selectionArgs: TJavaObjectArray<JString>; sortOrder: JString): JCursor; cdecl;//Deprecated
  function moveTaskToBack(nonRoot: Boolean): Boolean; cdecl;
  function navigateUpTo(upIntent: JIntent): Boolean; cdecl;
  function navigateUpToFromChild(child: JActivity; upIntent: JIntent): Boolean; cdecl;
  procedure onActionModeFinished(mode: JActionMode); cdecl;
  procedure onActionModeStarted(mode: JActionMode); cdecl;
  procedure onAttachFragment(fragment: JFragment); cdecl;
  procedure onAttachedToWindow; cdecl;
  procedure onBackPressed; cdecl;
  procedure onConfigurationChanged(newConfig: JConfiguration); cdecl;
  procedure onContentChanged; cdecl;
  function onContextItemSelected(item: JMenuItem): Boolean; cdecl;
  procedure onContextMenuClosed(menu: JMenu); cdecl;
  procedure onCreateContextMenu(menu: JContextMenu; v: JView; menuInfo: JContextMenu_ContextMenuInfo); cdecl;
  function onCreateDescription: JCharSequence; cdecl;
  procedure onCreateNavigateUpTaskStack(builder: JTaskStackBuilder); cdecl;
  function onCreateOptionsMenu(menu: JMenu): Boolean; cdecl;
  function onCreatePanelMenu(featureId: Integer; menu: JMenu): Boolean; cdecl;
  function onCreatePanelView(featureId: Integer): JView; cdecl;
  function onCreateThumbnail(outBitmap: JBitmap; canvas: JCanvas): Boolean; cdecl;
  function onCreateView(name: JString; context: JContext; attrs: JAttributeSet): JView; cdecl; overload;
  function onCreateView(parent: JView; name: JString; context: JContext; attrs: JAttributeSet): JView; cdecl; overload;
  procedure onDetachedFromWindow; cdecl;
  function onGenericMotionEvent(event: JMotionEvent): Boolean; cdecl;
  function onKeyDown(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  function onKeyLongPress(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  function onKeyMultiple(keyCode: Integer; repeatCount: Integer; event: JKeyEvent): Boolean; cdecl;
  function onKeyShortcut(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  function onKeyUp(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  procedure onLowMemory; cdecl;
  function onMenuItemSelected(featureId: Integer; item: JMenuItem): Boolean; cdecl;
  function onMenuOpened(featureId: Integer; menu: JMenu): Boolean; cdecl;
  function onNavigateUp: Boolean; cdecl;
  function onNavigateUpFromChild(child: JActivity): Boolean; cdecl;
  function onOptionsItemSelected(item: JMenuItem): Boolean; cdecl;
  procedure onOptionsMenuClosed(menu: JMenu); cdecl;
  procedure onPanelClosed(featureId: Integer; menu: JMenu); cdecl;
  procedure onPrepareNavigateUpTaskStack(builder: JTaskStackBuilder); cdecl;
  function onPrepareOptionsMenu(menu: JMenu): Boolean; cdecl;
  function onPreparePanel(featureId: Integer; view: JView; menu: JMenu): Boolean; cdecl;
  function onRetainNonConfigurationInstance: JObject; cdecl;//Deprecated
  function onSearchRequested: Boolean; cdecl;
  function onTouchEvent(event: JMotionEvent): Boolean; cdecl;
  function onTrackballEvent(event: JMotionEvent): Boolean; cdecl;
  procedure onTrimMemory(level: Integer); cdecl;
  procedure onUserInteraction; cdecl;
  procedure onWindowAttributesChanged(params: JWindowManager_LayoutParams); cdecl;
  procedure onWindowFocusChanged(hasFocus: Boolean); cdecl;
  function onWindowStartingActionMode(callback: JActionMode_Callback): JActionMode; cdecl;
  procedure openContextMenu(view: JView); cdecl;
  procedure openOptionsMenu; cdecl;
  procedure overridePendingTransition(enterAnim: Integer; exitAnim: Integer); cdecl;
  procedure recreate; cdecl;
  procedure registerForContextMenu(view: JView); cdecl;
  procedure removeDialog(id: Integer); cdecl;//Deprecated
  function requestWindowFeature(featureId: Integer): Boolean; cdecl;
  procedure runOnUiThread(action: JRunnable); cdecl;
  procedure setContentView(layoutResID: Integer); cdecl; overload;
  procedure setContentView(view: JView); cdecl; overload;
  procedure setContentView(view: JView; params: JViewGroup_LayoutParams); cdecl; overload;
  procedure setDefaultKeyMode(mode: Integer); cdecl;
  procedure setFeatureDrawable(featureId: Integer; drawable: JDrawable); cdecl;
  procedure setFeatureDrawableAlpha(featureId: Integer; alpha: Integer); cdecl;
  procedure setFeatureDrawableResource(featureId: Integer; resId: Integer); cdecl;
  procedure setFeatureDrawableUri(featureId: Integer; uri: Jnet_Uri); cdecl;
  procedure setFinishOnTouchOutside(finish: Boolean); cdecl;
  procedure setIntent(newIntent: JIntent); cdecl;
  procedure setProgress(progress: Integer); cdecl;
  procedure setProgressBarIndeterminate(indeterminate: Boolean); cdecl;
  procedure setProgressBarIndeterminateVisibility(visible: Boolean); cdecl;
  procedure setProgressBarVisibility(visible: Boolean); cdecl;
  procedure setRequestedOrientation(requestedOrientation: Integer); cdecl;
  procedure setResult(resultCode: Integer); cdecl; overload;
  procedure setResult(resultCode: Integer; data: JIntent); cdecl; overload;
  procedure setSecondaryProgress(secondaryProgress: Integer); cdecl;
  procedure setTitle(title: JCharSequence); cdecl; overload;
  procedure setTitle(titleId: Integer); cdecl; overload;
  procedure setTitleColor(textColor: Integer); cdecl;
  procedure setVisible(visible: Boolean); cdecl;
  procedure setVolumeControlStream(streamType: Integer); cdecl;
  function shouldUpRecreateTask(targetIntent: JIntent): Boolean; cdecl;
  procedure showDialog(id: Integer); cdecl; overload;//Deprecated
  function showDialog(id: Integer; args: JBundle): Boolean; cdecl; overload;//Deprecated
  function startActionMode(callback: JActionMode_Callback): JActionMode; cdecl;
  procedure startActivities(intents: TJavaObjectArray<JIntent>); cdecl; overload;
  procedure startActivities(intents: TJavaObjectArray<JIntent>; options: JBundle); cdecl; overload;
  procedure startActivity(intent: JIntent); cdecl; overload;
  procedure startActivity(intent: JIntent; options: JBundle); cdecl; overload;
  procedure startActivityForResult(intent: JIntent; requestCode: Integer); cdecl; overload;
  procedure startActivityForResult(intent: JIntent; requestCode: Integer; options: JBundle); cdecl; overload;
  procedure startActivityFromChild(child: JActivity; intent: JIntent; requestCode: Integer); cdecl; overload;
  procedure startActivityFromChild(child: JActivity; intent: JIntent; requestCode: Integer; options: JBundle); cdecl; overload;
  procedure startActivityFromFragment(fragment: JFragment; intent: JIntent; requestCode: Integer); cdecl; overload;
  procedure startActivityFromFragment(fragment: JFragment; intent: JIntent; requestCode: Integer; options: JBundle); cdecl; overload;
  function startActivityIfNeeded(intent: JIntent; requestCode: Integer): Boolean; cdecl; overload;
  function startActivityIfNeeded(intent: JIntent; requestCode: Integer; options: JBundle): Boolean; cdecl; overload;
  procedure startIntentSender(intent: JIntentSender; fillInIntent: JIntent; flagsMask: Integer; flagsValues: Integer; extraFlags: Integer); cdecl; overload;
  procedure startIntentSender(intent: JIntentSender; fillInIntent: JIntent; flagsMask: Integer; flagsValues: Integer; extraFlags: Integer; options: JBundle); cdecl; overload;
  procedure startIntentSenderForResult(intent: JIntentSender; requestCode: Integer; fillInIntent: JIntent; flagsMask: Integer; flagsValues: Integer; extraFlags: Integer); cdecl; overload;
  procedure startIntentSenderForResult(intent: JIntentSender; requestCode: Integer; fillInIntent: JIntent; flagsMask: Integer; flagsValues: Integer; extraFlags: Integer; options: JBundle); cdecl; overload;
  procedure startIntentSenderFromChild(child: JActivity; intent: JIntentSender; requestCode: Integer; fillInIntent: JIntent; flagsMask: Integer; flagsValues: Integer; extraFlags: Integer); cdecl; overload;
  procedure startIntentSenderFromChild(child: JActivity; intent: JIntentSender; requestCode: Integer; fillInIntent: JIntent; flagsMask: Integer; flagsValues: Integer; extraFlags: Integer; options: JBundle); cdecl; overload;
  procedure startManagingCursor(c: JCursor); cdecl;//Deprecated
  function startNextMatchingActivity(intent: JIntent): Boolean; cdecl; overload;
  function startNextMatchingActivity(intent: JIntent; options: JBundle): Boolean; cdecl; overload;
  procedure startSearch(initialQuery: JString; selectInitialQuery: Boolean; appSearchData: JBundle; globalSearch: Boolean); cdecl;
  procedure stopManagingCursor(c: JCursor); cdecl;//Deprecated
  procedure takeKeyEvents(get_: Boolean); cdecl;
  procedure triggerSearch(query: JString; appSearchData: JBundle); cdecl;
  procedure unregisterForContextMenu(view: JView); cdecl;
end;
TJActivity = class(TJavaGenericImport<JActivityClass, JActivity>) end;

JActionBar_OnMenuVisibilityListenerClass = interface(IJavaClass)
['{1E888E42-9D09-452B-9B44-2ABBB5BF0C1E}']
end;

[JavaSignature('android/app/ActionBar$OnMenuVisibilityListener')]
JActionBar_OnMenuVisibilityListener = interface(IJavaInstance)
['{3BBAA971-1B13-4420-937F-D4DFEA9100FF}']
  {Methods}
  procedure onMenuVisibilityChanged(isVisible: Boolean); cdecl;
end;
TJActionBar_OnMenuVisibilityListener = class(TJavaGenericImport<JActionBar_OnMenuVisibilityListenerClass, JActionBar_OnMenuVisibilityListener>) end;

JDialogFragmentClass = interface(JFragmentClass)
['{0BA4C933-9BC4-4D8B-9757-C02740EBC511}']
  {Property Methods}
  function _GetSTYLE_NORMAL: Integer;
  function _GetSTYLE_NO_FRAME: Integer;
  function _GetSTYLE_NO_INPUT: Integer;
  function _GetSTYLE_NO_TITLE: Integer;
  {Methods}
  function init: JDialogFragment; cdecl;
  {Properties}
  property STYLE_NORMAL: Integer read _GetSTYLE_NORMAL;
  property STYLE_NO_FRAME: Integer read _GetSTYLE_NO_FRAME;
  property STYLE_NO_INPUT: Integer read _GetSTYLE_NO_INPUT;
  property STYLE_NO_TITLE: Integer read _GetSTYLE_NO_TITLE;
end;

[JavaSignature('android/app/DialogFragment')]
JDialogFragment = interface(JFragment)
['{D5B8C228-5BCF-43DF-B26A-18A3EB4C893E}']
  {Methods}
  procedure dismiss; cdecl;
  procedure dismissAllowingStateLoss; cdecl;
  procedure dump(prefix: JString; fd: JFileDescriptor; writer: JPrintWriter; args: TJavaObjectArray<JString>); cdecl;
  function getDialog: JDialog; cdecl;
  function getShowsDialog: Boolean; cdecl;
  function getTheme: Integer; cdecl;
  function isCancelable: Boolean; cdecl;
  procedure onActivityCreated(savedInstanceState: JBundle); cdecl;
  procedure onAttach(activity: JActivity); cdecl;
  procedure onCancel(dialog: JDialogInterface); cdecl;
  procedure onCreate(savedInstanceState: JBundle); cdecl;
  function onCreateDialog(savedInstanceState: JBundle): JDialog; cdecl;
  procedure onDestroyView; cdecl;
  procedure onDetach; cdecl;
  procedure onDismiss(dialog: JDialogInterface); cdecl;
  procedure onSaveInstanceState(outState: JBundle); cdecl;
  procedure onStart; cdecl;
  procedure onStop; cdecl;
  procedure setCancelable(cancelable: Boolean); cdecl;
  procedure setShowsDialog(showsDialog: Boolean); cdecl;
  procedure setStyle(style: Integer; theme: Integer); cdecl;
  procedure show(manager: JFragmentManager; tag: JString); cdecl; overload;
  function show(transaction: JFragmentTransaction; tag: JString): Integer; cdecl; overload;
end;
TJDialogFragment = class(TJavaGenericImport<JDialogFragmentClass, JDialogFragment>) end;

JFragmentManager_BackStackEntryClass = interface(IJavaClass)
['{8B736CA5-FB98-4A84-8BDE-7A43FEE35224}']
end;

[JavaSignature('android/app/FragmentManager$BackStackEntry')]
JFragmentManager_BackStackEntry = interface(IJavaInstance)
['{F5CD98DC-3473-4D0E-B189-93124ABAFDCD}']
  {Methods}
  function getBreadCrumbShortTitle: JCharSequence; cdecl;
  function getBreadCrumbShortTitleRes: Integer; cdecl;
  function getBreadCrumbTitle: JCharSequence; cdecl;
  function getBreadCrumbTitleRes: Integer; cdecl;
  function getId: Integer; cdecl;
  function getName: JString; cdecl;
end;
TJFragmentManager_BackStackEntry = class(TJavaGenericImport<JFragmentManager_BackStackEntryClass, JFragmentManager_BackStackEntry>) end;

JNativeActivityClass = interface(JActivityClass)
['{F54B4BFD-1C6B-4CD6-88E7-68ACDD301C01}']
  {Property Methods}
  function _GetMETA_DATA_FUNC_NAME: JString;
  function _GetMETA_DATA_LIB_NAME: JString;
  {Methods}
  function init: JNativeActivity; cdecl;
  {Properties}
  property META_DATA_FUNC_NAME: JString read _GetMETA_DATA_FUNC_NAME;
  property META_DATA_LIB_NAME: JString read _GetMETA_DATA_LIB_NAME;
end;

[JavaSignature('android/app/NativeActivity')]
JNativeActivity = interface(JActivity)
['{DFE875A8-A174-41E7-91C0-D959852DE56D}']
  {Methods}
  function dispatchKeyEvent(event: JKeyEvent): Boolean; cdecl;
  procedure onConfigurationChanged(newConfig: JConfiguration); cdecl;
  procedure onGlobalLayout; cdecl;
  procedure onInputQueueCreated(queue: JInputQueue); cdecl;
  procedure onInputQueueDestroyed(queue: JInputQueue); cdecl;
  procedure onLowMemory; cdecl;
  procedure onWindowFocusChanged(hasFocus: Boolean); cdecl;
  procedure surfaceChanged(holder: JSurfaceHolder; format: Integer; width: Integer; height: Integer); cdecl;
  procedure surfaceCreated(holder: JSurfaceHolder); cdecl;
  procedure surfaceDestroyed(holder: JSurfaceHolder); cdecl;
  procedure surfaceRedrawNeeded(holder: JSurfaceHolder); cdecl;
end;
TJNativeActivity = class(TJavaGenericImport<JNativeActivityClass, JNativeActivity>) end;




implementation

begin

end.


