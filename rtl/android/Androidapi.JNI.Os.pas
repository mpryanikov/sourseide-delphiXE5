{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit Androidapi.JNI.Os;

interface

uses 
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Util;

type

  {Class forward declarations}
  JMessage = interface;//android.os.Message
  JBinder = interface;//android.os.Binder
  JCancellationSignal = interface;//android.os.CancellationSignal
  JMessenger = interface;//android.os.Messenger
  JBuild = interface;//android.os.Build
  JRemoteException = interface;//android.os.RemoteException
  JParcelFileDescriptor_AutoCloseOutputStream = interface;//android.os.ParcelFileDescriptor$AutoCloseOutputStream
  JParcelable = interface;//android.os.Parcelable
  JIInterface = interface;//android.os.IInterface
  JIBinder_DeathRecipient = interface;//android.os.IBinder$DeathRecipient
  JBuild_VERSION = interface;//android.os.Build$VERSION
  JUserHandle = interface;//android.os.UserHandle
  JParcelFileDescriptor = interface;//android.os.ParcelFileDescriptor
  JPatternMatcher = interface;//android.os.PatternMatcher
  JParcelFileDescriptor_AutoCloseInputStream = interface;//android.os.ParcelFileDescriptor$AutoCloseInputStream
  JBuild_VERSION_CODES = interface;//android.os.Build$VERSION_CODES
  JLooper = interface;//android.os.Looper
  JBundle = interface;//android.os.Bundle
  JCancellationSignal_OnCancelListener = interface;//android.os.CancellationSignal$OnCancelListener
  JHandler = interface;//android.os.Handler
  JParcelable_Creator = interface;//android.os.Parcelable$Creator
  JParcelable_ClassLoaderCreator = interface;//android.os.Parcelable$ClassLoaderCreator
  JHandler_Callback = interface;//android.os.Handler$Callback
  JVibrator = interface;//android.os.Vibrator
  JMessageQueue = interface;//android.os.MessageQueue
  JMessageQueue_IdleHandler = interface;//android.os.MessageQueue$IdleHandler
  JIBinder = interface;//android.os.IBinder
  JResultReceiver = interface;//android.os.ResultReceiver
  JParcel = interface;//android.os.Parcel

JMessageClass = interface(JObjectClass)
['{1B2538A8-B0AD-4E06-B2E5-80EF21543420}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Methods}
  function init: JMessage; cdecl;
  function obtain: JMessage; cdecl; overload;
  function obtain(orig: JMessage): JMessage; cdecl; overload;
  function obtain(h: JHandler): JMessage; cdecl; overload;
  function obtain(h: JHandler; callback: JRunnable): JMessage; cdecl; overload;
  function obtain(h: JHandler; what: Integer): JMessage; cdecl; overload;
  function obtain(h: JHandler; what: Integer; obj: JObject): JMessage; cdecl; overload;
  function obtain(h: JHandler; what: Integer; arg1: Integer; arg2: Integer): JMessage; cdecl; overload;
  function obtain(h: JHandler; what: Integer; arg1: Integer; arg2: Integer; obj: JObject): JMessage; cdecl; overload;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/os/Message')]
JMessage = interface(JObject)
['{6BD63150-99D3-4A77-A12D-41F00022009A}']
  {Property Methods}
  function _Getarg1: Integer;
  procedure _Setarg1(Value: Integer);
  function _Getarg2: Integer;
  procedure _Setarg2(Value: Integer);
  function _Getobj: JObject;
  procedure _Setobj(Value: JObject);
  function _GetreplyTo: JMessenger;
  procedure _SetreplyTo(Value: JMessenger);
  function _Getwhat: Integer;
  procedure _Setwhat(Value: Integer);
  {Methods}
  procedure copyFrom(o: JMessage); cdecl;
  function describeContents: Integer; cdecl;
  function getCallback: JRunnable; cdecl;
  function getData: JBundle; cdecl;
  function getTarget: JHandler; cdecl;
  function getWhen: Int64; cdecl;
  function peekData: JBundle; cdecl;
  procedure recycle; cdecl;
  procedure sendToTarget; cdecl;
  procedure setData(data: JBundle); cdecl;
  procedure setTarget(target: JHandler); cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  {Properties}
  property arg1: Integer read _Getarg1 write _Setarg1;
  property arg2: Integer read _Getarg2 write _Setarg2;
  property obj: JObject read _Getobj write _Setobj;
  property replyTo: JMessenger read _GetreplyTo write _SetreplyTo;
  property what: Integer read _Getwhat write _Setwhat;
end;
TJMessage = class(TJavaGenericImport<JMessageClass, JMessage>) end;

JBinderClass = interface(JObjectClass)
['{28B153C2-C5E2-4A29-8263-9F290048E72A}']
  {Methods}
  function init: JBinder; cdecl;
  function clearCallingIdentity: Int64; cdecl;
  procedure flushPendingCommands; cdecl;
  function getCallingPid: Integer; cdecl;
  function getCallingUid: Integer; cdecl;
  function getCallingUserHandle: JUserHandle; cdecl;
  procedure joinThreadPool; cdecl;
  procedure restoreCallingIdentity(token: Int64); cdecl;
end;

[JavaSignature('android/os/Binder')]
JBinder = interface(JObject)
['{0A24225C-72F2-43B3-904F-77E148717C48}']
  {Methods}
  procedure attachInterface(owner: JIInterface; descriptor: JString); cdecl;
  procedure dump(fd: JFileDescriptor; args: TJavaObjectArray<JString>); cdecl;
  procedure dumpAsync(fd: JFileDescriptor; args: TJavaObjectArray<JString>); cdecl;
  function getInterfaceDescriptor: JString; cdecl;
  function isBinderAlive: Boolean; cdecl;
  procedure linkToDeath(recipient: JIBinder_DeathRecipient; flags: Integer); cdecl;
  function pingBinder: Boolean; cdecl;
  function queryLocalInterface(descriptor: JString): JIInterface; cdecl;
  function transact(code: Integer; data: JParcel; reply: JParcel; flags: Integer): Boolean; cdecl;
  function unlinkToDeath(recipient: JIBinder_DeathRecipient; flags: Integer): Boolean; cdecl;
end;
TJBinder = class(TJavaGenericImport<JBinderClass, JBinder>) end;

JCancellationSignalClass = interface(JObjectClass)
['{15B2FE5D-1470-43DD-8693-45A3DC38300A}']
  {Methods}
  function init: JCancellationSignal; cdecl;
end;

[JavaSignature('android/os/CancellationSignal')]
JCancellationSignal = interface(JObject)
['{68D52DA5-BAFC-40CA-9CC8-083A1DC6EF2A}']
  {Methods}
  procedure cancel; cdecl;
  function isCanceled: Boolean; cdecl;
  procedure setOnCancelListener(listener: JCancellationSignal_OnCancelListener); cdecl;
  procedure throwIfCanceled; cdecl;
end;
TJCancellationSignal = class(TJavaGenericImport<JCancellationSignalClass, JCancellationSignal>) end;

JMessengerClass = interface(JObjectClass)
['{F56C4382-C9CD-44C5-8FD9-ED57F2EB7020}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Methods}
  function init(target: JHandler): JMessenger; cdecl; overload;
  function init(target: JIBinder): JMessenger; cdecl; overload;
  function readMessengerOrNullFromParcel(in_: JParcel): JMessenger; cdecl;
  procedure writeMessengerOrNullToParcel(messenger: JMessenger; out_: JParcel); cdecl;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/os/Messenger')]
JMessenger = interface(JObject)
['{5DB42D38-8B04-4E46-BB40-71E75131EE34}']
  {Methods}
  function describeContents: Integer; cdecl;
  function equals(otherObj: JObject): Boolean; cdecl;
  function getBinder: JIBinder; cdecl;
  function hashCode: Integer; cdecl;
  procedure send(message: JMessage); cdecl;
  procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
end;
TJMessenger = class(TJavaGenericImport<JMessengerClass, JMessenger>) end;

JBuildClass = interface(JObjectClass)
['{D90671CF-F0CF-4B07-9660-00D373289D50}']
  {Property Methods}
  function _GetBOARD: JString;
  function _GetBOOTLOADER: JString;
  function _GetBRAND: JString;
  function _GetCPU_ABI: JString;
  function _GetCPU_ABI2: JString;
  function _GetDEVICE: JString;
  function _GetDISPLAY: JString;
  function _GetFINGERPRINT: JString;
  function _GetHARDWARE: JString;
  function _GetHOST: JString;
  function _GetID: JString;
  function _GetMANUFACTURER: JString;
  function _GetMODEL: JString;
  function _GetPRODUCT: JString;
  function _GetRADIO: JString;
  function _GetSERIAL: JString;
  function _GetTAGS: JString;
  function _GetTIME: Int64;
  function _GetTYPE: JString;
  function _GetUNKNOWN: JString;
  function _GetUSER: JString;
  {Methods}
  function init: JBuild; cdecl;
  function getRadioVersion: JString; cdecl;
  {Properties}
  property BOARD: JString read _GetBOARD;
  property BOOTLOADER: JString read _GetBOOTLOADER;
  property BRAND: JString read _GetBRAND;
  property CPU_ABI: JString read _GetCPU_ABI;
  property CPU_ABI2: JString read _GetCPU_ABI2;
  property DEVICE: JString read _GetDEVICE;
  property DISPLAY: JString read _GetDISPLAY;
  property FINGERPRINT: JString read _GetFINGERPRINT;
  property HARDWARE: JString read _GetHARDWARE;
  property HOST: JString read _GetHOST;
  property ID: JString read _GetID;
  property MANUFACTURER: JString read _GetMANUFACTURER;
  property MODEL: JString read _GetMODEL;
  property PRODUCT: JString read _GetPRODUCT;
  property RADIO: JString read _GetRADIO;
  property SERIAL: JString read _GetSERIAL;
  property TAGS: JString read _GetTAGS;
  property TIME: Int64 read _GetTIME;
  property &TYPE: JString read _GetTYPE;
  property UNKNOWN: JString read _GetUNKNOWN;
  property USER: JString read _GetUSER;
end;

[JavaSignature('android/os/Build')]
JBuild = interface(JObject)
['{68FCB5C7-BF21-4462-AEA3-D43FBD2D2B24}']
end;
TJBuild = class(TJavaGenericImport<JBuildClass, JBuild>) end;

JRemoteExceptionClass = interface(JAndroidExceptionClass)
['{51B4919C-703A-41F5-B1E4-9BA4BAA4139A}']
  {Methods}
  function init: JRemoteException; cdecl; overload;
  function init(message: JString): JRemoteException; cdecl; overload;
end;

[JavaSignature('android/os/RemoteException')]
JRemoteException = interface(JAndroidException)
['{D75584EB-7EF7-444D-B989-C9487675E1ED}']
end;
TJRemoteException = class(TJavaGenericImport<JRemoteExceptionClass, JRemoteException>) end;

JParcelFileDescriptor_AutoCloseOutputStreamClass = interface(JFileOutputStreamClass)
['{3DA3CB8C-5AF5-4ACD-915D-6BB844D37AC5}']
  {Methods}
  function init(fd: JParcelFileDescriptor): JParcelFileDescriptor_AutoCloseOutputStream; cdecl;
end;

[JavaSignature('android/os/ParcelFileDescriptor$AutoCloseOutputStream')]
JParcelFileDescriptor_AutoCloseOutputStream = interface(JFileOutputStream)
['{AC268DEA-CB4A-49D1-B14B-78950AD93E4F}']
  {Methods}
  procedure close; cdecl;
end;
TJParcelFileDescriptor_AutoCloseOutputStream = class(TJavaGenericImport<JParcelFileDescriptor_AutoCloseOutputStreamClass, JParcelFileDescriptor_AutoCloseOutputStream>) end;

JParcelableClass = interface(IJavaClass)
['{C4028E29-2B61-4CBE-86B0-E7AEB7DB6073}']
  {Property Methods}
  function _GetCONTENTS_FILE_DESCRIPTOR: Integer;
  function _GetPARCELABLE_WRITE_RETURN_VALUE: Integer;
  {Properties}
  property CONTENTS_FILE_DESCRIPTOR: Integer read _GetCONTENTS_FILE_DESCRIPTOR;
  property PARCELABLE_WRITE_RETURN_VALUE: Integer read _GetPARCELABLE_WRITE_RETURN_VALUE;
end;

[JavaSignature('android/os/Parcelable')]
JParcelable = interface(IJavaInstance)
['{60865491-0B26-486B-A5EA-F137A0408CFD}']
  {Methods}
  function describeContents: Integer; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
end;
TJParcelable = class(TJavaGenericImport<JParcelableClass, JParcelable>) end;

JIInterfaceClass = interface(IJavaClass)
['{5C536661-DE48-4AEF-B371-51FFBFB39875}']
end;

[JavaSignature('android/os/IInterface')]
JIInterface = interface(IJavaInstance)
['{CCF0440C-DEDC-468C-952A-8EB7637E0E57}']
  {Methods}
  function asBinder: JIBinder; cdecl;
end;
TJIInterface = class(TJavaGenericImport<JIInterfaceClass, JIInterface>) end;

JIBinder_DeathRecipientClass = interface(IJavaClass)
['{0BB1DF5F-6D10-4BDE-B1FD-00479125622E}']
end;

[JavaSignature('android/os/IBinder$DeathRecipient')]
JIBinder_DeathRecipient = interface(IJavaInstance)
['{C23CFA91-43FE-4E30-86C2-5F125D7F01E0}']
  {Methods}
  procedure binderDied; cdecl;
end;
TJIBinder_DeathRecipient = class(TJavaGenericImport<JIBinder_DeathRecipientClass, JIBinder_DeathRecipient>) end;

JBuild_VERSIONClass = interface(JObjectClass)
['{1CEE3318-C68B-4A45-A09A-0584BFB9FA31}']
  {Property Methods}
  function _GetCODENAME: JString;
  function _GetINCREMENTAL: JString;
  function _GetRELEASE: JString;
  function _GetSDK: JString;
  function _GetSDK_INT: Integer;
  {Methods}
  function init: JBuild_VERSION; cdecl;
  {Properties}
  property CODENAME: JString read _GetCODENAME;
  property INCREMENTAL: JString read _GetINCREMENTAL;
  property RELEASE: JString read _GetRELEASE;
  property SDK: JString read _GetSDK;
  property SDK_INT: Integer read _GetSDK_INT;
end;

[JavaSignature('android/os/Build$VERSION')]
JBuild_VERSION = interface(JObject)
['{364B79B2-B660-4622-BCD5-85A530549F8E}']
end;
TJBuild_VERSION = class(TJavaGenericImport<JBuild_VERSIONClass, JBuild_VERSION>) end;

JUserHandleClass = interface(JObjectClass)
['{FF26D134-0260-4F44-A103-45FECCA03842}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Methods}
  function init(in_: JParcel): JUserHandle; cdecl;
  function readFromParcel(in_: JParcel): JUserHandle; cdecl;
  procedure writeToParcel(h: JUserHandle; out_: JParcel); cdecl; overload;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/os/UserHandle')]
JUserHandle = interface(JObject)
['{77D557B7-B984-434F-93A1-19407712B375}']
  {Methods}
  function describeContents: Integer; cdecl;
  function equals(obj: JObject): Boolean; cdecl;
  function hashCode: Integer; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(out_: JParcel; flags: Integer); cdecl; overload;
end;
TJUserHandle = class(TJavaGenericImport<JUserHandleClass, JUserHandle>) end;

JParcelFileDescriptorClass = interface(JObjectClass)
['{FFBFF8DF-F6F4-4EB2-A73D-0B2E753BD03D}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  function _GetMODE_APPEND: Integer;
  function _GetMODE_CREATE: Integer;
  function _GetMODE_READ_ONLY: Integer;
  function _GetMODE_READ_WRITE: Integer;
  function _GetMODE_TRUNCATE: Integer;
  function _GetMODE_WORLD_READABLE: Integer;
  function _GetMODE_WORLD_WRITEABLE: Integer;
  function _GetMODE_WRITE_ONLY: Integer;
  {Methods}
  function init(descriptor: JParcelFileDescriptor): JParcelFileDescriptor; cdecl;
  function adoptFd(fd: Integer): JParcelFileDescriptor; cdecl;
  function createPipe: TJavaObjectArray<JParcelFileDescriptor>; cdecl;
  function dup(orig: JFileDescriptor): JParcelFileDescriptor; cdecl; overload;
  function fromFd(fd: Integer): JParcelFileDescriptor; cdecl;
  function open(file_: JFile; mode: Integer): JParcelFileDescriptor; cdecl;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
  property MODE_APPEND: Integer read _GetMODE_APPEND;
  property MODE_CREATE: Integer read _GetMODE_CREATE;
  property MODE_READ_ONLY: Integer read _GetMODE_READ_ONLY;
  property MODE_READ_WRITE: Integer read _GetMODE_READ_WRITE;
  property MODE_TRUNCATE: Integer read _GetMODE_TRUNCATE;
  property MODE_WORLD_READABLE: Integer read _GetMODE_WORLD_READABLE;
  property MODE_WORLD_WRITEABLE: Integer read _GetMODE_WORLD_WRITEABLE;
  property MODE_WRITE_ONLY: Integer read _GetMODE_WRITE_ONLY;
end;

[JavaSignature('android/os/ParcelFileDescriptor')]
JParcelFileDescriptor = interface(JObject)
['{C1A682AA-6579-416E-868E-C951ED8FB338}']
  {Methods}
  procedure close; cdecl;
  function describeContents: Integer; cdecl;
  function detachFd: Integer; cdecl;
  function dup: JParcelFileDescriptor; cdecl; overload;
  function getFd: Integer; cdecl;
  function getFileDescriptor: JFileDescriptor; cdecl;
  function getStatSize: Int64; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
end;
TJParcelFileDescriptor = class(TJavaGenericImport<JParcelFileDescriptorClass, JParcelFileDescriptor>) end;

JPatternMatcherClass = interface(JObjectClass)
['{5531BAAE-118E-44DB-8296-A24A9DCC4B84}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  function _GetPATTERN_LITERAL: Integer;
  function _GetPATTERN_PREFIX: Integer;
  function _GetPATTERN_SIMPLE_GLOB: Integer;
  {Methods}
  function init(pattern: JString; type_: Integer): JPatternMatcher; cdecl; overload;
  function init(src: JParcel): JPatternMatcher; cdecl; overload;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
  property PATTERN_LITERAL: Integer read _GetPATTERN_LITERAL;
  property PATTERN_PREFIX: Integer read _GetPATTERN_PREFIX;
  property PATTERN_SIMPLE_GLOB: Integer read _GetPATTERN_SIMPLE_GLOB;
end;

[JavaSignature('android/os/PatternMatcher')]
JPatternMatcher = interface(JObject)
['{FDB6AB90-AA53-4B07-A28D-9BD3FD65AF85}']
  {Methods}
  function describeContents: Integer; cdecl;
  function getPath: JString; cdecl;
  function getType: Integer; cdecl;
  function match(str: JString): Boolean; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
end;
TJPatternMatcher = class(TJavaGenericImport<JPatternMatcherClass, JPatternMatcher>) end;

JParcelFileDescriptor_AutoCloseInputStreamClass = interface(JFileInputStreamClass)
['{5C32589D-9078-47EE-9BCC-B7E2C587ACEF}']
  {Methods}
  function init(fd: JParcelFileDescriptor): JParcelFileDescriptor_AutoCloseInputStream; cdecl;
end;

[JavaSignature('android/os/ParcelFileDescriptor$AutoCloseInputStream')]
JParcelFileDescriptor_AutoCloseInputStream = interface(JFileInputStream)
['{28B7EEA4-CA6D-4565-AF0B-9AA87233C96B}']
  {Methods}
  procedure close; cdecl;
end;
TJParcelFileDescriptor_AutoCloseInputStream = class(TJavaGenericImport<JParcelFileDescriptor_AutoCloseInputStreamClass, JParcelFileDescriptor_AutoCloseInputStream>) end;

JBuild_VERSION_CODESClass = interface(JObjectClass)
['{B29B57B4-E754-4491-8E21-18F8AC40121D}']
  {Property Methods}
  function _GetBASE: Integer;
  function _GetBASE_1_1: Integer;
  function _GetCUPCAKE: Integer;
  function _GetCUR_DEVELOPMENT: Integer;
  function _GetDONUT: Integer;
  function _GetECLAIR: Integer;
  function _GetECLAIR_0_1: Integer;
  function _GetECLAIR_MR1: Integer;
  function _GetFROYO: Integer;
  function _GetGINGERBREAD: Integer;
  function _GetGINGERBREAD_MR1: Integer;
  function _GetHONEYCOMB: Integer;
  function _GetHONEYCOMB_MR1: Integer;
  function _GetHONEYCOMB_MR2: Integer;
  function _GetICE_CREAM_SANDWICH: Integer;
  function _GetICE_CREAM_SANDWICH_MR1: Integer;
  function _GetJELLY_BEAN: Integer;
  function _GetJELLY_BEAN_MR1: Integer;
  {Methods}
  function init: JBuild_VERSION_CODES; cdecl;
  {Properties}
  property BASE: Integer read _GetBASE;
  property BASE_1_1: Integer read _GetBASE_1_1;
  property CUPCAKE: Integer read _GetCUPCAKE;
  property CUR_DEVELOPMENT: Integer read _GetCUR_DEVELOPMENT;
  property DONUT: Integer read _GetDONUT;
  property ECLAIR: Integer read _GetECLAIR;
  property ECLAIR_0_1: Integer read _GetECLAIR_0_1;
  property ECLAIR_MR1: Integer read _GetECLAIR_MR1;
  property FROYO: Integer read _GetFROYO;
  property GINGERBREAD: Integer read _GetGINGERBREAD;
  property GINGERBREAD_MR1: Integer read _GetGINGERBREAD_MR1;
  property HONEYCOMB: Integer read _GetHONEYCOMB;
  property HONEYCOMB_MR1: Integer read _GetHONEYCOMB_MR1;
  property HONEYCOMB_MR2: Integer read _GetHONEYCOMB_MR2;
  property ICE_CREAM_SANDWICH: Integer read _GetICE_CREAM_SANDWICH;
  property ICE_CREAM_SANDWICH_MR1: Integer read _GetICE_CREAM_SANDWICH_MR1;
  property JELLY_BEAN: Integer read _GetJELLY_BEAN;
  property JELLY_BEAN_MR1: Integer read _GetJELLY_BEAN_MR1;
end;

[JavaSignature('android/os/Build$VERSION_CODES')]
JBuild_VERSION_CODES = interface(JObject)
['{02FEBAA4-6C7B-4AA8-8415-ED410352FF64}']
end;
TJBuild_VERSION_CODES = class(TJavaGenericImport<JBuild_VERSION_CODESClass, JBuild_VERSION_CODES>) end;

JLooperClass = interface(JObjectClass)
['{EDF411AD-10E9-46B9-851F-0BF490BEBDEA}']
  {Methods}
  function getMainLooper: JLooper; cdecl;
  procedure loop; cdecl;
  function myLooper: JLooper; cdecl;
  function myQueue: JMessageQueue; cdecl;
  procedure prepare; cdecl;
  procedure prepareMainLooper; cdecl;
end;

[JavaSignature('android/os/Looper')]
JLooper = interface(JObject)
['{C7E0949B-044A-41EB-A38F-87D18DB4750E}']
  {Methods}
  function getThread: JThread; cdecl;
  procedure quit; cdecl;
  function toString: JString; cdecl;
end;
TJLooper = class(TJavaGenericImport<JLooperClass, JLooper>) end;

JBundleClass = interface(JObjectClass)
['{35D462B0-8866-4937-B9CD-5DC0F7E9DE87}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  function _GetEMPTY: JBundle;
  {Methods}
  function init: JBundle; cdecl; overload;
  function init(loader: JClassLoader): JBundle; cdecl; overload;
  function init(capacity: Integer): JBundle; cdecl; overload;
  function init(b: JBundle): JBundle; cdecl; overload;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
  property EMPTY: JBundle read _GetEMPTY;
end;

[JavaSignature('android/os/Bundle')]
JBundle = interface(JObject)
['{7D0B7FBA-46ED-4D53-9E0C-F48362182E4F}']
  {Methods}
  procedure clear; cdecl;
  function clone: JObject; cdecl;
  function containsKey(key: JString): Boolean; cdecl;
  function describeContents: Integer; cdecl;
  function &get(key: JString): JObject; cdecl;
  function getBoolean(key: JString): Boolean; cdecl; overload;
  function getBoolean(key: JString; defaultValue: Boolean): Boolean; cdecl; overload;
  function getBooleanArray(key: JString): TJavaArray<Boolean>; cdecl;
  function getBundle(key: JString): JBundle; cdecl;
  function getByte(key: JString): Byte; cdecl; overload;
  function getByte(key: JString; defaultValue: Byte): JByte; cdecl; overload;
  function getByteArray(key: JString): TJavaArray<Byte>; cdecl;
  function getChar(key: JString): Char; cdecl; overload;
  function getChar(key: JString; defaultValue: Char): Char; cdecl; overload;
  function getCharArray(key: JString): TJavaArray<Char>; cdecl;
  function getCharSequence(key: JString): JCharSequence; cdecl; overload;
  function getCharSequence(key: JString; defaultValue: JCharSequence): JCharSequence; cdecl; overload;
  function getCharSequenceArray(key: JString): TJavaObjectArray<JCharSequence>; cdecl;
  function getCharSequenceArrayList(key: JString): JArrayList; cdecl;
  function getClassLoader: JClassLoader; cdecl;
  function getDouble(key: JString): Double; cdecl; overload;
  function getDouble(key: JString; defaultValue: Double): Double; cdecl; overload;
  function getDoubleArray(key: JString): TJavaArray<Double>; cdecl;
  function getFloat(key: JString): Single; cdecl; overload;
  function getFloat(key: JString; defaultValue: Single): Single; cdecl; overload;
  function getFloatArray(key: JString): TJavaArray<Single>; cdecl;
  function getInt(key: JString): Integer; cdecl; overload;
  function getInt(key: JString; defaultValue: Integer): Integer; cdecl; overload;
  function getIntArray(key: JString): TJavaArray<Integer>; cdecl;
  function getIntegerArrayList(key: JString): JArrayList; cdecl;
  function getLong(key: JString): Int64; cdecl; overload;
  function getLong(key: JString; defaultValue: Int64): Int64; cdecl; overload;
  function getLongArray(key: JString): TJavaArray<Int64>; cdecl;
  function getParcelable(key: JString): JParcelable; cdecl;
  function getParcelableArray(key: JString): TJavaObjectArray<JParcelable>; cdecl;
  function getParcelableArrayList(key: JString): JArrayList; cdecl;
  function getSerializable(key: JString): JSerializable; cdecl;
  function getShort(key: JString): SmallInt; cdecl; overload;
  function getShort(key: JString; defaultValue: SmallInt): SmallInt; cdecl; overload;
  function getShortArray(key: JString): TJavaArray<SmallInt>; cdecl;
  function getSparseParcelableArray(key: JString): JSparseArray; cdecl;
  function getString(key: JString): JString; cdecl; overload;
  function getString(key: JString; defaultValue: JString): JString; cdecl; overload;
  function getStringArray(key: JString): TJavaObjectArray<JString>; cdecl;
  function getStringArrayList(key: JString): JArrayList; cdecl;
  function hasFileDescriptors: Boolean; cdecl;
  function isEmpty: Boolean; cdecl;
  function keySet: JSet; cdecl;
  procedure putAll(map: JBundle); cdecl;
  procedure putBoolean(key: JString; value: Boolean); cdecl;
  procedure putBooleanArray(key: JString; value: TJavaArray<Boolean>); cdecl;
  procedure putBundle(key: JString; value: JBundle); cdecl;
  procedure putByte(key: JString; value: Byte); cdecl;
  procedure putByteArray(key: JString; value: TJavaArray<Byte>); cdecl;
  procedure putChar(key: JString; value: Char); cdecl;
  procedure putCharArray(key: JString; value: TJavaArray<Char>); cdecl;
  procedure putCharSequence(key: JString; value: JCharSequence); cdecl;
  procedure putCharSequenceArray(key: JString; value: TJavaObjectArray<JCharSequence>); cdecl;
  procedure putCharSequenceArrayList(key: JString; value: JArrayList); cdecl;
  procedure putDouble(key: JString; value: Double); cdecl;
  procedure putDoubleArray(key: JString; value: TJavaArray<Double>); cdecl;
  procedure putFloat(key: JString; value: Single); cdecl;
  procedure putFloatArray(key: JString; value: TJavaArray<Single>); cdecl;
  procedure putInt(key: JString; value: Integer); cdecl;
  procedure putIntArray(key: JString; value: TJavaArray<Integer>); cdecl;
  procedure putIntegerArrayList(key: JString; value: JArrayList); cdecl;
  procedure putLong(key: JString; value: Int64); cdecl;
  procedure putLongArray(key: JString; value: TJavaArray<Int64>); cdecl;
  procedure putParcelable(key: JString; value: JParcelable); cdecl;
  procedure putParcelableArray(key: JString; value: TJavaObjectArray<JParcelable>); cdecl;
  procedure putParcelableArrayList(key: JString; value: JArrayList); cdecl;
  procedure putSerializable(key: JString; value: JSerializable); cdecl;
  procedure putShort(key: JString; value: SmallInt); cdecl;
  procedure putShortArray(key: JString; value: TJavaArray<SmallInt>); cdecl;
  procedure putSparseParcelableArray(key: JString; value: JSparseArray); cdecl;
  procedure putString(key: JString; value: JString); cdecl;
  procedure putStringArray(key: JString; value: TJavaObjectArray<JString>); cdecl;
  procedure putStringArrayList(key: JString; value: JArrayList); cdecl;
  procedure readFromParcel(parcel: JParcel); cdecl;
  procedure remove(key: JString); cdecl;
  procedure setClassLoader(loader: JClassLoader); cdecl;
  function size: Integer; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(parcel: JParcel; flags: Integer); cdecl;
end;
TJBundle = class(TJavaGenericImport<JBundleClass, JBundle>) end;

JCancellationSignal_OnCancelListenerClass = interface(IJavaClass)
['{C5DA883A-D40B-4567-93D7-B7D9EB1399EA}']
end;

[JavaSignature('android/os/CancellationSignal$OnCancelListener')]
JCancellationSignal_OnCancelListener = interface(IJavaInstance)
['{6B0E6041-7C1B-4911-8FC3-167328179B8C}']
  {Methods}
  procedure onCancel; cdecl;
end;
TJCancellationSignal_OnCancelListener = class(TJavaGenericImport<JCancellationSignal_OnCancelListenerClass, JCancellationSignal_OnCancelListener>) end;

JHandlerClass = interface(JObjectClass)
['{6DFA74DD-1C38-4654-A690-215B98106A9E}']
  {Methods}
  function init: JHandler; cdecl; overload;
  function init(callback: JHandler_Callback): JHandler; cdecl; overload;
  function init(looper: JLooper): JHandler; cdecl; overload;
  function init(looper: JLooper; callback: JHandler_Callback): JHandler; cdecl; overload;
end;

[JavaSignature('android/os/Handler')]
JHandler = interface(JObject)
['{9D5EF727-535C-41E1-8459-570107053097}']
  {Methods}
  procedure dispatchMessage(msg: JMessage); cdecl;
  function getLooper: JLooper; cdecl;
  function getMessageName(message: JMessage): JString; cdecl;
  procedure handleMessage(msg: JMessage); cdecl;
  function hasMessages(what: Integer): Boolean; cdecl; overload;
  function hasMessages(what: Integer; object_: JObject): Boolean; cdecl; overload;
  function obtainMessage: JMessage; cdecl; overload;
  function obtainMessage(what: Integer): JMessage; cdecl; overload;
  function obtainMessage(what: Integer; obj: JObject): JMessage; cdecl; overload;
  function obtainMessage(what: Integer; arg1: Integer; arg2: Integer): JMessage; cdecl; overload;
  function obtainMessage(what: Integer; arg1: Integer; arg2: Integer; obj: JObject): JMessage; cdecl; overload;
  function post(r: JRunnable): Boolean; cdecl;
  function postAtFrontOfQueue(r: JRunnable): Boolean; cdecl;
  function postAtTime(r: JRunnable; uptimeMillis: Int64): Boolean; cdecl; overload;
  function postAtTime(r: JRunnable; token: JObject; uptimeMillis: Int64): Boolean; cdecl; overload;
  function postDelayed(r: JRunnable; delayMillis: Int64): Boolean; cdecl;
  procedure removeCallbacks(r: JRunnable); cdecl; overload;
  procedure removeCallbacks(r: JRunnable; token: JObject); cdecl; overload;
  procedure removeCallbacksAndMessages(token: JObject); cdecl;
  procedure removeMessages(what: Integer); cdecl; overload;
  procedure removeMessages(what: Integer; object_: JObject); cdecl; overload;
  function sendEmptyMessage(what: Integer): Boolean; cdecl;
  function sendEmptyMessageAtTime(what: Integer; uptimeMillis: Int64): Boolean; cdecl;
  function sendEmptyMessageDelayed(what: Integer; delayMillis: Int64): Boolean; cdecl;
  function sendMessage(msg: JMessage): Boolean; cdecl;
  function sendMessageAtFrontOfQueue(msg: JMessage): Boolean; cdecl;
  function sendMessageAtTime(msg: JMessage; uptimeMillis: Int64): Boolean; cdecl;
  function sendMessageDelayed(msg: JMessage; delayMillis: Int64): Boolean; cdecl;
  function toString: JString; cdecl;
end;
TJHandler = class(TJavaGenericImport<JHandlerClass, JHandler>) end;

JParcelable_CreatorClass = interface(IJavaClass)
['{BDBDE50C-1639-42B7-8880-F0DE45C12CE1}']
end;

[JavaSignature('android/os/Parcelable$Creator')]
JParcelable_Creator = interface(IJavaInstance)
['{A2263E47-9FD8-4553-B31B-9EBB64A04843}']
  {Methods}
  function createFromParcel(source: JParcel): JObject; cdecl;
  function newArray(size: Integer): TJavaObjectArray<JObject>; cdecl;
end;
TJParcelable_Creator = class(TJavaGenericImport<JParcelable_CreatorClass, JParcelable_Creator>) end;

JParcelable_ClassLoaderCreatorClass = interface(JParcelable_CreatorClass)
['{AD975D2E-9776-4FED-8213-81FA6C89A7BA}']
end;

[JavaSignature('android/os/Parcelable$ClassLoaderCreator')]
JParcelable_ClassLoaderCreator = interface(JParcelable_Creator)
['{FCAFEC5F-1CCE-4991-B75D-9376254D7E15}']
  {Methods}
  function createFromParcel(source: JParcel; loader: JClassLoader): JObject; cdecl;
end;
TJParcelable_ClassLoaderCreator = class(TJavaGenericImport<JParcelable_ClassLoaderCreatorClass, JParcelable_ClassLoaderCreator>) end;

JHandler_CallbackClass = interface(IJavaClass)
['{56610FAC-0DB7-4E6C-9DE7-34BC98A7CF57}']
end;

[JavaSignature('android/os/Handler$Callback')]
JHandler_Callback = interface(IJavaInstance)
['{347B7489-1A0D-4B7E-BA98-26BF948DA15B}']
  {Methods}
  function handleMessage(msg: JMessage): Boolean; cdecl;
end;
TJHandler_Callback = class(TJavaGenericImport<JHandler_CallbackClass, JHandler_Callback>) end;

JVibratorClass = interface(JObjectClass)
['{00EEB92F-8145-441D-81C3-218E7B271F1B}']
end;

[JavaSignature('android/os/Vibrator')]
JVibrator = interface(JObject)
['{82BDC8BC-22A3-4EAD-99AF-5DA70739B086}']
  {Methods}
  procedure cancel; cdecl;
  function hasVibrator: Boolean; cdecl;
  procedure vibrate(milliseconds: Int64); cdecl; overload;
  procedure vibrate(pattern: TJavaArray<Int64>; repeat_: Integer); cdecl; overload;
end;
TJVibrator = class(TJavaGenericImport<JVibratorClass, JVibrator>) end;

JMessageQueueClass = interface(JObjectClass)
['{C3690F20-6E73-4103-B86F-02B72CC58D9E}']
end;

[JavaSignature('android/os/MessageQueue')]
JMessageQueue = interface(JObject)
['{73ED03E5-98F2-49F7-970D-B96D3B54F339}']
  {Methods}
  procedure addIdleHandler(handler: JMessageQueue_IdleHandler); cdecl;
  procedure removeIdleHandler(handler: JMessageQueue_IdleHandler); cdecl;
end;
TJMessageQueue = class(TJavaGenericImport<JMessageQueueClass, JMessageQueue>) end;

JMessageQueue_IdleHandlerClass = interface(IJavaClass)
['{5AA2B208-7429-47C2-824A-B9A94B306B83}']
end;

[JavaSignature('android/os/MessageQueue$IdleHandler')]
JMessageQueue_IdleHandler = interface(IJavaInstance)
['{36B80D1E-6D2E-421C-A29D-E7435F3023DE}']
  {Methods}
  function queueIdle: Boolean; cdecl;
end;
TJMessageQueue_IdleHandler = class(TJavaGenericImport<JMessageQueue_IdleHandlerClass, JMessageQueue_IdleHandler>) end;

JIBinderClass = interface(IJavaClass)
['{39FDECDD-962B-44DB-80B1-BA608C7F6F22}']
  {Property Methods}
  function _GetDUMP_TRANSACTION: Integer;
  function _GetFIRST_CALL_TRANSACTION: Integer;
  function _GetFLAG_ONEWAY: Integer;
  function _GetINTERFACE_TRANSACTION: Integer;
  function _GetLAST_CALL_TRANSACTION: Integer;
  function _GetLIKE_TRANSACTION: Integer;
  function _GetPING_TRANSACTION: Integer;
  function _GetTWEET_TRANSACTION: Integer;
  {Properties}
  property DUMP_TRANSACTION: Integer read _GetDUMP_TRANSACTION;
  property FIRST_CALL_TRANSACTION: Integer read _GetFIRST_CALL_TRANSACTION;
  property FLAG_ONEWAY: Integer read _GetFLAG_ONEWAY;
  property INTERFACE_TRANSACTION: Integer read _GetINTERFACE_TRANSACTION;
  property LAST_CALL_TRANSACTION: Integer read _GetLAST_CALL_TRANSACTION;
  property LIKE_TRANSACTION: Integer read _GetLIKE_TRANSACTION;
  property PING_TRANSACTION: Integer read _GetPING_TRANSACTION;
  property TWEET_TRANSACTION: Integer read _GetTWEET_TRANSACTION;
end;

[JavaSignature('android/os/IBinder')]
JIBinder = interface(IJavaInstance)
['{9DCFD46A-EA83-48EB-87BC-B18A3D9284E0}']
  {Methods}
  procedure dump(fd: JFileDescriptor; args: TJavaObjectArray<JString>); cdecl;
  procedure dumpAsync(fd: JFileDescriptor; args: TJavaObjectArray<JString>); cdecl;
  function getInterfaceDescriptor: JString; cdecl;
  function isBinderAlive: Boolean; cdecl;
  procedure linkToDeath(recipient: JIBinder_DeathRecipient; flags: Integer); cdecl;
  function pingBinder: Boolean; cdecl;
  function queryLocalInterface(descriptor: JString): JIInterface; cdecl;
  function transact(code: Integer; data: JParcel; reply: JParcel; flags: Integer): Boolean; cdecl;
  function unlinkToDeath(recipient: JIBinder_DeathRecipient; flags: Integer): Boolean; cdecl;
end;
TJIBinder = class(TJavaGenericImport<JIBinderClass, JIBinder>) end;

JResultReceiverClass = interface(JObjectClass)
['{590CF4A2-8625-4919-A054-77EC93C5981E}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Methods}
  function init(handler: JHandler): JResultReceiver; cdecl;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/os/ResultReceiver')]
JResultReceiver = interface(JObject)
['{BBFBBD02-25B2-49B9-B441-CFB8DF3AA9C5}']
  {Methods}
  function describeContents: Integer; cdecl;
  procedure send(resultCode: Integer; resultData: JBundle); cdecl;
  procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
end;
TJResultReceiver = class(TJavaGenericImport<JResultReceiverClass, JResultReceiver>) end;

JParcelClass = interface(JObjectClass)
['{4221C753-227D-4D4A-8F17-7897C2BD8870}']
  {Property Methods}
  function _GetSTRING_CREATOR: JParcelable_Creator;
  {Methods}
  function obtain: JParcel; cdecl;
  {Properties}
  property STRING_CREATOR: JParcelable_Creator read _GetSTRING_CREATOR;
end;

[JavaSignature('android/os/Parcel')]
JParcel = interface(JObject)
['{4E777D3D-D7CB-4157-9ED0-F956FB00B18E}']
  {Methods}
  procedure appendFrom(parcel: JParcel; offset: Integer; length: Integer); cdecl;
  function createBinderArray: TJavaObjectArray<JIBinder>; cdecl;
  function createBinderArrayList: JArrayList; cdecl;
  function createBooleanArray: TJavaArray<Boolean>; cdecl;
  function createByteArray: TJavaArray<Byte>; cdecl;
  function createCharArray: TJavaArray<Char>; cdecl;
  function createDoubleArray: TJavaArray<Double>; cdecl;
  function createFloatArray: TJavaArray<Single>; cdecl;
  function createIntArray: TJavaArray<Integer>; cdecl;
  function createLongArray: TJavaArray<Int64>; cdecl;
  function createStringArray: TJavaObjectArray<JString>; cdecl;
  function createStringArrayList: JArrayList; cdecl;
  function createTypedArray(c: JParcelable_Creator): TJavaObjectArray<JObject>; cdecl;
  function createTypedArrayList(c: JParcelable_Creator): JArrayList; cdecl;
  function dataAvail: Integer; cdecl;
  function dataCapacity: Integer; cdecl;
  function dataPosition: Integer; cdecl;
  function dataSize: Integer; cdecl;
  procedure enforceInterface(interfaceName: JString); cdecl;
  function hasFileDescriptors: Boolean; cdecl;
  function marshall: TJavaArray<Byte>; cdecl;
  function readArray(loader: JClassLoader): TJavaObjectArray<JObject>; cdecl;
  function readArrayList(loader: JClassLoader): JArrayList; cdecl;
  procedure readBinderArray(val: TJavaObjectArray<JIBinder>); cdecl;
  procedure readBinderList(list: JList); cdecl;
  procedure readBooleanArray(val: TJavaArray<Boolean>); cdecl;
  function readBundle: JBundle; cdecl; overload;
  function readBundle(loader: JClassLoader): JBundle; cdecl; overload;
  function readByte: Byte; cdecl;
  procedure readByteArray(val: TJavaArray<Byte>); cdecl;
  procedure readCharArray(val: TJavaArray<Char>); cdecl;
  function readDouble: Double; cdecl;
  procedure readDoubleArray(val: TJavaArray<Double>); cdecl;
  procedure readException; cdecl; overload;
  procedure readException(code: Integer; msg: JString); cdecl; overload;
  function readFileDescriptor: JParcelFileDescriptor; cdecl;
  function readFloat: Single; cdecl;
  procedure readFloatArray(val: TJavaArray<Single>); cdecl;
  function readHashMap(loader: JClassLoader): JHashMap; cdecl;
  function readInt: Integer; cdecl;
  procedure readIntArray(val: TJavaArray<Integer>); cdecl;
  procedure readList(outVal: JList; loader: JClassLoader); cdecl;
  function readLong: Int64; cdecl;
  procedure readLongArray(val: TJavaArray<Int64>); cdecl;
  procedure readMap(outVal: JMap; loader: JClassLoader); cdecl;
  function readParcelable(loader: JClassLoader): JParcelable; cdecl;
  function readParcelableArray(loader: JClassLoader): TJavaObjectArray<JParcelable>; cdecl;
  function readSerializable: JSerializable; cdecl;
  function readSparseArray(loader: JClassLoader): JSparseArray; cdecl;
  function readSparseBooleanArray: JSparseBooleanArray; cdecl;
  function readString: JString; cdecl;
  procedure readStringArray(val: TJavaObjectArray<JString>); cdecl;
  procedure readStringList(list: JList); cdecl;
  function readStrongBinder: JIBinder; cdecl;
  procedure readTypedArray(val: TJavaObjectArray<JObject>; c: JParcelable_Creator); cdecl;
  procedure readTypedList(list: JList; c: JParcelable_Creator); cdecl;
  function readValue(loader: JClassLoader): JObject; cdecl;
  procedure recycle; cdecl;
  procedure setDataCapacity(size: Integer); cdecl;
  procedure setDataPosition(pos: Integer); cdecl;
  procedure setDataSize(size: Integer); cdecl;
  procedure unmarshall(data: TJavaArray<Byte>; offest: Integer; length: Integer); cdecl;
  procedure writeArray(val: TJavaObjectArray<JObject>); cdecl;
  procedure writeBinderArray(val: TJavaObjectArray<JIBinder>); cdecl;
  procedure writeBinderList(val: JList); cdecl;
  procedure writeBooleanArray(val: TJavaArray<Boolean>); cdecl;
  procedure writeBundle(val: JBundle); cdecl;
  procedure writeByte(val: Byte); cdecl;
  procedure writeByteArray(b: TJavaArray<Byte>); cdecl; overload;
  procedure writeByteArray(b: TJavaArray<Byte>; offset: Integer; len: Integer); cdecl; overload;
  procedure writeCharArray(val: TJavaArray<Char>); cdecl;
  procedure writeDouble(val: Double); cdecl;
  procedure writeDoubleArray(val: TJavaArray<Double>); cdecl;
  procedure writeException(e: JException); cdecl;
  procedure writeFileDescriptor(val: JFileDescriptor); cdecl;
  procedure writeFloat(val: Single); cdecl;
  procedure writeFloatArray(val: TJavaArray<Single>); cdecl;
  procedure writeInt(val: Integer); cdecl;
  procedure writeIntArray(val: TJavaArray<Integer>); cdecl;
  procedure writeInterfaceToken(interfaceName: JString); cdecl;
  procedure writeList(val: JList); cdecl;
  procedure writeLong(val: Int64); cdecl;
  procedure writeLongArray(val: TJavaArray<Int64>); cdecl;
  procedure writeMap(val: JMap); cdecl;
  procedure writeNoException; cdecl;
  procedure writeParcelable(p: JParcelable; parcelableFlags: Integer); cdecl;
  procedure writeParcelableArray(value: TJavaObjectArray<JParcelable>; parcelableFlags: Integer); cdecl;
  procedure writeSerializable(s: JSerializable); cdecl;
  procedure writeSparseArray(val: JSparseArray); cdecl;
  procedure writeSparseBooleanArray(val: JSparseBooleanArray); cdecl;
  procedure writeString(val: JString); cdecl;
  procedure writeStringArray(val: TJavaObjectArray<JString>); cdecl;
  procedure writeStringList(val: JList); cdecl;
  procedure writeStrongBinder(val: JIBinder); cdecl;
  procedure writeStrongInterface(val: JIInterface); cdecl;
  procedure writeTypedArray(val: TJavaObjectArray<JParcelable>; parcelableFlags: Integer); cdecl;
  procedure writeTypedList(val: JList); cdecl;
  procedure writeValue(v: JObject); cdecl;
end;
TJParcel = class(TJavaGenericImport<JParcelClass, JParcel>) end;




implementation

begin

end.


