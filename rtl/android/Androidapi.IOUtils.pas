{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2010-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit Androidapi.IOUtils;

interface

//  File Locations in Internal memory. (Accessible only to the program.
// Not accessible to anyone without rooting the Android system.)
// Files written here are deleted when uninstalling the application.)
function GetFilesDir: string;
function GetCacheDir: string;
function GetLibraryPath: string;

//  File Locations in External memory. (Accessible only to the program, but easily
// readable mounting the external storage as a drive in a computer.
// Files written here are deleted when uninstalling the application.)
function GetExternalFilesDir: string;
function GetExternalCacheDir: string;

function GetExternalPicturesDir: string;
function GetExternalCameraDir: string;
function GetExternalDownloadsDir: string;
function GetExternalMoviesDir: string;
function GetExternalMusicDir: string;
function GetExternalAlarmsDir: string;
function GetExternalRingtonesDir: string;


//  File Locations in External memory. (Accessible to all programs, easily
// readable mounting the external storage as a drive in a computer.
// Files written here are preserved when uninstalling the application.)
function GetSharedFilesDir: string;
function GetSharedPicturesDir: string;
function GetSharedCameraDir: string;
function GetSharedDownloadsDir: string;
function GetSharedMoviesDir: string;
function GetSharedMusicDir: string;
function GetSharedAlarmsDir: string;
function GetSharedRingtonesDir: string;


implementation

uses
  Androidapi.Jni,
  Androidapi.NativeActivity;

function GetJniPath(MethodName, Signature: MarshaledAString): string;
var
  PEnv: PJniEnv;
  ActivityClass: JNIClass;
  FileClass: JNIClass;
  GetMethod: JNIMethodID;
  GetPathMethod: JNIMethodID;
  PActivity: PANativeActivity;
  StrPathObject: JNIObject;
  FileObject: JNIObject;
begin

  PActivity := PANativeActivity(System.DelphiActivity);
  PActivity^.vm^.AttachCurrentThread(PActivity^.vm, @PEnv, nil);
  ActivityClass := PEnv^.GetObjectClass(PEnv, PActivity^.clazz);

  GetMethod := PEnv^.GetMethodID(PEnv, ActivityClass, MethodName, Signature);
  FileObject := PEnv^.CallObjectMethodA(PEnv, PActivity^.clazz, GetMethod, PJNIValue(ArgsToJNIValues([nil])));
  if FileObject = nil then
    Exit('');
  FileClass := PEnv^.GetObjectClass(PEnv, FileObject);

  GetPathMethod := PEnv^.GetMethodID(PEnv, FileClass, 'getPath', '()Ljava/lang/String;');
  StrPathObject := PEnv^.CallObjectMethodA(PEnv, FileObject, GetPathMethod, PJNIValue(ArgsToJNIValues([])));

  Result := JNIStringToString(PEnv, StrPathObject);

  PEnv^.DeleteLocalRef(PEnv, StrPathObject);
  PEnv^.DeleteLocalRef(PEnv, FileClass);
  PEnv^.DeleteLocalRef(PEnv, FileObject);
  PEnv^.DeleteLocalRef(PEnv, ActivityClass);

end;
{
public static String 	DIRECTORY_ALARMS 	Standard directory in which to place any audio files that should be in the list of alarms that the user can select (not as regular music).
public static String 	DIRECTORY_DCIM 	The traditional location for pictures and videos when mounting the device as a camera.
public static String 	DIRECTORY_DOWNLOADS 	Standard directory in which to place files that have been downloaded by the user.
public static String 	DIRECTORY_MOVIES 	Standard directory in which to place movies that are available to the user.
public static String 	DIRECTORY_MUSIC 	Standard directory in which to place any audio files that should be in the regular list of music for the user.
public static String 	DIRECTORY_NOTIFICATIONS 	Standard directory in which to place any audio files that should be in the list of notifications that the user can select (not as regular music).
public static String 	DIRECTORY_PICTURES 	Standard directory in which to place pictures that are available to the user.
public static String 	DIRECTORY_PODCASTS 	Standard directory in which to place any audio files that should be in the list of podcasts that the user can select (not as regular music).
public static String 	DIRECTORY_RINGTONES 	Standard directory in which to place any audio files that should be in the list of ringtones that the user can select (not as regular music).}
type
  TCustomPathType = (cpNONE, cpALARMS, cpDCIM, cpDOWNLOADS, cpMOVIES,
      cpMUSIC, cpNOTIFICATIONS, cpPICTURES, cpPODCASTS, cpRINGTONES);

function GetJniCustomPath(MethodName, Signature: MarshaledAString; PathType:TCustomPathType): string;
var
  PEnv: PJniEnv;
  ActivityClass, EnvironmentClass: JNIClass;
  FileClass: JNIClass;
  GetMethod: JNIMethodID;
  GetPathMethod: JNIMethodID;
  PActivity: PANativeActivity;
  StrPathObject: JNIObject;
  CustomPathObject: JNIObject;
  FileObject: JNIObject;
  PathFieldID: JNIFieldID;
begin
  PActivity := PANativeActivity(System.DelphiActivity);
  PActivity^.vm^.AttachCurrentThread(PActivity^.vm, @PEnv, nil);
  ActivityClass := PEnv^.GetObjectClass(PEnv, PActivity^.clazz);
  EnvironmentClass := PEnv^.FindClass(PEnv, 'android/os/Environment');

  case PathType of
    cpPICTURES: PathFieldID := PEnv^.GetStaticFieldID(PEnv, EnvironmentClass, 'DIRECTORY_PICTURES', 'Ljava/lang/String;');
    cpDOWNLOADS: PathFieldID := PEnv^.GetStaticFieldID(PEnv, EnvironmentClass, 'DIRECTORY_DOWNLOADS', 'Ljava/lang/String;');
    cpDCIM: PathFieldID := PEnv^.GetStaticFieldID(PEnv, EnvironmentClass, 'DIRECTORY_DCIM', 'Ljava/lang/String;');
    cpMOVIES: PathFieldID := PEnv^.GetStaticFieldID(PEnv, EnvironmentClass, 'DIRECTORY_MOVIES', 'Ljava/lang/String;');
    cpALARMS: PathFieldID := PEnv^.GetStaticFieldID(PEnv, EnvironmentClass, 'DIRECTORY_ALARMS', 'Ljava/lang/String;');
    cpMUSIC: PathFieldID := PEnv^.GetStaticFieldID(PEnv, EnvironmentClass, 'DIRECTORY_MUSIC', 'Ljava/lang/String;');
    cpNOTIFICATIONS: PathFieldID := PEnv^.GetStaticFieldID(PEnv, EnvironmentClass, 'DIRECTORY_NOTIFICATIONS', 'Ljava/lang/String;');
    cpPODCASTS: PathFieldID := PEnv^.GetStaticFieldID(PEnv, EnvironmentClass, 'DIRECTORY_PODCASTS', 'Ljava/lang/String;');
    cpRINGTONES: PathFieldID := PEnv^.GetStaticFieldID(PEnv, EnvironmentClass, 'DIRECTORY_RINGTONES', 'Ljava/lang/String;');
    else
      PathFieldID := nil;
  end;

  if PathFieldID = nil then
    CustomPathObject := nil
  else
    CustomPathObject := PEnv^.GetStaticObjectField(PEnv, EnvironmentClass, PathFieldID);

  GetMethod := PEnv^.GetMethodID(PEnv, ActivityClass, MethodName, Signature);
  FileObject := PEnv^.CallObjectMethodA(PEnv, PActivity^.clazz, GetMethod, PJNIValue(ArgsToJNIValues([CustomPathObject])));
  if FileObject = nil then
    Exit('');
  FileClass := PEnv^.GetObjectClass(PEnv, FileObject);

  GetPathMethod := PEnv^.GetMethodID(PEnv, FileClass, 'getPath', '()Ljava/lang/String;');
  StrPathObject := PEnv^.CallObjectMethodA(PEnv, FileObject, GetPathMethod, PJNIValue(ArgsToJNIValues([])));

  Result := JNIStringToString(PEnv, StrPathObject);

  PEnv^.DeleteLocalRef(PEnv, StrPathObject);
  if CustomPathObject <> nil then PEnv^.DeleteLocalRef(PEnv, CustomPathObject);
  PEnv^.DeleteLocalRef(PEnv, FileClass);
  PEnv^.DeleteLocalRef(PEnv, FileObject);
  PEnv^.DeleteLocalRef(PEnv, EnvironmentClass);
  PEnv^.DeleteLocalRef(PEnv, ActivityClass);
end;

function GetJniCustomPath2(MethodName, Signature: MarshaledAString; PathType:TCustomPathType): string;
var
  PEnv: PJniEnv;
  ActivityClass, EnvironmentClass: JNIClass;
  FileClass: JNIClass;
  GetMethod: JNIMethodID;
  GetPathMethod: JNIMethodID;
  PActivity: PANativeActivity;
  StrPathObject: JNIObject;
  CustomPathObject: JNIObject;
  FileObject: JNIObject;
  PathFieldID: JNIFieldID;
begin
  PActivity := PANativeActivity(System.DelphiActivity);
  PActivity^.vm^.AttachCurrentThread(PActivity^.vm, @PEnv, nil);
  ActivityClass := PEnv^.GetObjectClass(PEnv, PActivity^.clazz);
  EnvironmentClass := PEnv^.FindClass(PEnv, 'android/os/Environment');

  case PathType of
    cpPICTURES: PathFieldID := PEnv^.GetStaticFieldID(PEnv, EnvironmentClass, 'DIRECTORY_PICTURES', 'Ljava/lang/String;');
    cpDOWNLOADS: PathFieldID := PEnv^.GetStaticFieldID(PEnv, EnvironmentClass, 'DIRECTORY_DOWNLOADS', 'Ljava/lang/String;');
    cpDCIM: PathFieldID := PEnv^.GetStaticFieldID(PEnv, EnvironmentClass, 'DIRECTORY_DCIM', 'Ljava/lang/String;');
    cpMOVIES: PathFieldID := PEnv^.GetStaticFieldID(PEnv, EnvironmentClass, 'DIRECTORY_MOVIES', 'Ljava/lang/String;');
    cpALARMS: PathFieldID := PEnv^.GetStaticFieldID(PEnv, EnvironmentClass, 'DIRECTORY_ALARMS', 'Ljava/lang/String;');
    cpMUSIC: PathFieldID := PEnv^.GetStaticFieldID(PEnv, EnvironmentClass, 'DIRECTORY_MUSIC', 'Ljava/lang/String;');
    cpNOTIFICATIONS: PathFieldID := PEnv^.GetStaticFieldID(PEnv, EnvironmentClass, 'DIRECTORY_NOTIFICATIONS', 'Ljava/lang/String;');
    cpPODCASTS: PathFieldID := PEnv^.GetStaticFieldID(PEnv, EnvironmentClass, 'DIRECTORY_PODCASTS', 'Ljava/lang/String;');
    cpRINGTONES: PathFieldID := PEnv^.GetStaticFieldID(PEnv, EnvironmentClass, 'DIRECTORY_RINGTONES', 'Ljava/lang/String;');
    else
      PathFieldID := nil;
  end;

  if PathFieldID = nil then
    CustomPathObject := nil
  else
    CustomPathObject := PEnv^.GetStaticObjectField(PEnv, EnvironmentClass, PathFieldID);

  GetMethod := PEnv^.GetStaticMethodID(PEnv, EnvironmentClass, MethodName, Signature);
  FileObject := PEnv^.CallStaticObjectMethodA(PEnv, EnvironmentClass, GetMethod, PJNIValue(ArgsToJNIValues([CustomPathObject])));
  if FileObject = nil then
    Exit('');
  FileClass := PEnv^.GetObjectClass(PEnv, FileObject);

  GetPathMethod := PEnv^.GetMethodID(PEnv, FileClass, 'getPath', '()Ljava/lang/String;');
  StrPathObject := PEnv^.CallObjectMethodA(PEnv, FileObject, GetPathMethod, PJNIValue(ArgsToJNIValues([])));

  Result := JNIStringToString(PEnv, StrPathObject);

  PEnv^.DeleteLocalRef(PEnv, StrPathObject);
  if CustomPathObject <> nil then PEnv^.DeleteLocalRef(PEnv, CustomPathObject);
  PEnv^.DeleteLocalRef(PEnv, FileClass);
  PEnv^.DeleteLocalRef(PEnv, FileObject);
  PEnv^.DeleteLocalRef(PEnv, EnvironmentClass);
  PEnv^.DeleteLocalRef(PEnv, ActivityClass);
end;

function GetLibraryPath: string;
var
  PEnv: PJniEnv;
  ActivityClass: JNIClass;
  FileClass: JNIClass;
  GetMethod: JNIMethodID;
  PropertyID: JNIFieldID;
  PActivity: PANativeActivity;
  StrPathObject: JNIObject;
  FileObject: JNIObject;
begin

  PActivity := PANativeActivity(System.DelphiActivity);
  PActivity^.vm^.AttachCurrentThread(PActivity^.vm, @PEnv, nil);
  ActivityClass := PEnv^.GetObjectClass(PEnv, PActivity^.clazz);

  GetMethod := PEnv^.GetMethodID(PEnv, ActivityClass, 'getApplicationInfo', '()Landroid/content/pm/ApplicationInfo;');
  FileObject := PEnv^.CallObjectMethodA(PEnv, PActivity^.clazz, GetMethod, PJNIValue(ArgsToJNIValues([nil])));
  if FileObject = nil then
    Exit('');
  FileClass := PEnv^.GetObjectClass(PEnv, FileObject);

  PropertyID := PEnv^.GetFieldID(PEnv, FileClass, 'nativeLibraryDir', 'Ljava/lang/String;');
  StrPathObject := PEnv^.GetObjectField(PEnv, FileObject, PropertyID);

  Result := JNIStringToString(PEnv, StrPathObject);

  PEnv^.DeleteLocalRef(PEnv, StrPathObject);
  PEnv^.DeleteLocalRef(PEnv, FileClass);
  PEnv^.DeleteLocalRef(PEnv, FileObject);
  PEnv^.DeleteLocalRef(PEnv, ActivityClass);

end;

function GetFilesDir: string;
begin
{ There is a bug inside ndk on versions previous to version 10 that makes
  internal and external paths are wrong in the Native Activity variable
  so we need to retrieve them via jni }
//  if ANativeActivity(system.DelphiActivity^).sdkVersion > 10 then
//    Result := UTF8Tostring(ANativeActivity(system.DelphiActivity^).internalDataPath)
//  else
{  Also retrieving it via JNI, ensures that the retrieved path exists.
  The folder is created if it does not exist. This way we can avoid the problem
  that if we do it via NativeActivity we can have problems trying to write to
  the folder if we do not do a previous ForceDirectories.
}
    Result := GetJniPath('getFilesDir', '()Ljava/io/File;');
end;

function GetExternalFilesDir: string;
begin
{ There is a bug inside ndk on versions previous to version 10(included)(2.3.3)  that makes
  internal and external paths are wrong in the Native Activity variable
  so we need to retrieve them via jni }
//  if ANativeActivity(system.DelphiActivity^).sdkVersion > 10 then
//    Result := UTF8Tostring(ANativeActivity(system.DelphiActivity^).externalDataPath);
//  else
{  Also retrieving it via JNI, ensures that the retrieved path exists.
  The folder is created if it does not exist. This way we can avoid the problem
  that if we do it via NativeActivity we can have problems trying to write to
  the folder if we do not do a previous ForceDirectories.
}
//  Result := GetJniPath('getExternalFilesDir', '(Ljava/lang/String;)Ljava/io/File;');
  Result := GetJniCustomPath('getExternalFilesDir', '(Ljava/lang/String;)Ljava/io/File;', cpNONE);
end;

function GetCacheDir: string;
begin
  Result := GetJniPath('getCacheDir', '()Ljava/io/File;');
end;

function GetExternalCacheDir: string;
begin
  Result := GetJniPath('getExternalCacheDir', '()Ljava/io/File;');
end;

function GetExternalPicturesDir: string;
begin
  Result := GetJniCustomPath('getExternalFilesDir', '(Ljava/lang/String;)Ljava/io/File;', cpPICTURES);
end;

function GetExternalCameraDir: string;
begin
  Result := GetJniCustomPath('getExternalFilesDir', '(Ljava/lang/String;)Ljava/io/File;', cpDCIM);
end;

function GetExternalDownloadsDir: string;
begin
  Result := GetJniCustomPath('getExternalFilesDir', '(Ljava/lang/String;)Ljava/io/File;', cpDOWNLOADS);
end;

function GetExternalMoviesDir: string;
begin
  Result := GetJniCustomPath('getExternalFilesDir', '(Ljava/lang/String;)Ljava/io/File;', cpMOVIES);
end;

function GetExternalMusicDir: string;
begin
  Result := GetJniCustomPath('getExternalFilesDir', '(Ljava/lang/String;)Ljava/io/File;', cpMUSIC);
end;

function GetExternalAlarmsDir: string;
begin
  Result := GetJniCustomPath('getExternalFilesDir', '(Ljava/lang/String;)Ljava/io/File;', cpALARMS);
end;

function GetExternalRingtonesDir: string;
begin
  Result := GetJniCustomPath('getExternalFilesDir', '(Ljava/lang/String;)Ljava/io/File;', cpRINGTONES);
end;

// Shared Folders functions.

function GetSharedFilesDir: string;
begin
  Result := GetJniCustomPath2('getExternalStoragePublicDirectory', '(Ljava/lang/String;)Ljava/io/File;', cpDOWNLOADS);
end;

function GetSharedPicturesDir: string;
begin
  Result := GetJniCustomPath2('getExternalStoragePublicDirectory', '(Ljava/lang/String;)Ljava/io/File;', cpPICTURES);
end;

function GetSharedCameraDir: string;
begin
  Result := GetJniCustomPath2('getExternalStoragePublicDirectory', '(Ljava/lang/String;)Ljava/io/File;', cpDCIM);
end;

function GetSharedDownloadsDir: string;
begin
  Result := GetJniCustomPath2('getExternalStoragePublicDirectory', '(Ljava/lang/String;)Ljava/io/File;', cpDOWNLOADS);
end;

function GetSharedMoviesDir: string;
begin
  Result := GetJniCustomPath2('getExternalStoragePublicDirectory', '(Ljava/lang/String;)Ljava/io/File;', cpMOVIES);
end;

function GetSharedMusicDir: string;
begin
  Result := GetJniCustomPath2('getExternalStoragePublicDirectory', '(Ljava/lang/String;)Ljava/io/File;', cpMUSIC);
end;

function GetSharedAlarmsDir: string;
begin
  Result := GetJniCustomPath2('getExternalStoragePublicDirectory', '(Ljava/lang/String;)Ljava/io/File;', cpALARMS);
end;

function GetSharedRingtonesDir: string;
begin
  Result := GetJniCustomPath2('getExternalStoragePublicDirectory', '(Ljava/lang/String;)Ljava/io/File;', cpRINGTONES);
end;

end.
