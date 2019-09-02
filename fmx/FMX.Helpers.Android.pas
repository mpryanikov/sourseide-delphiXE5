{*******************************************************}
{                                                       }
{         Delphi FireMonkey Notification Service        }
{                                                       }
{           Helpers for Android implementations         }
{                                                       }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit FMX.Helpers.Android;

interface

uses
  Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Net, Androidapi.JNI.App,
  Androidapi.Jni.Util, FMX.Surfaces;

{ Shared instances }

function SharedActivityContext: JContext;
function SharedActivity: JActivity;
function SharedApplicationInfo: JApplicationInfo;
function SharedAlarmManager: JAlarmManager;
function SharedPrivatePreferences: JSharedPreferences;
function GetApplicationTitle: string;
function GetPackagePath: string;

function IsGingerbreadDevice: Boolean;

{ String convertions }

function StrToJCharSequence(const ASource: string): JCharSequence;
function JCharSequenceToString(const ASource: JCharSequence): string;
function StrToJURI(const ASource: string): Jnet_Uri;

{ Check Availible services }

function HasSystemService(const AFeatureName: JString): Boolean;

{ Working with Resources }

function GetResourceID(const ResourceName: string): Integer;
function GetResourceString(const ResourceID: Integer): string;

{ Surface convertions }

function JBitmapToSurface(const SrcBitmap: JBitmap; const DestSurface: TBitmapSurface): Boolean;
function SurfaceToJBitmap(const SrcSurface: TBitmapSurface; const DestBitmap: JBitmap): Boolean;

{ Display }

function GetJDisplay: JDisplay;
function GetJDisplayMetrics: JDisplayMetrics;

{ Work with UI Thread }

type
  TMethodCallback = procedure of object;
  TCallBack = reference to procedure;

procedure CallInUIThread(AMethod: TMethodCallback); overload;
procedure CallInUIThread(AMethod: TCallBack); overload;
procedure CallInUIThreadAndWaitFinishing(AMethod: TMethodCallback); overload;
procedure CallInUIThreadAndWaitFinishing(AMethod: TCallBack); overload;

implementation

uses
  Androidapi.JNIBridge, Androidapi.NativeActivity, Androidapi.Bitmap, System.SysUtils, System.SyncObjs,
  System.Generics.Collections, FMX.Types, FMX.Platform;

function SharedActivityContext: JContext;
begin
  Result := SharedActivity;
end;

function SharedActivity: JActivity;
begin
  Result := TJNativeActivity.Wrap(PANativeActivity(System.DelphiActivity)^.clazz);
end;

function SharedApplicationInfo: JApplicationInfo;
begin
  Result := SharedActivityContext.getApplicationInfo;
end;

function SharedAlarmManager: JAlarmManager;
var
  AlarmManagerService: JObject;
begin
  AlarmManagerService := SharedActivityContext.getSystemService(TJContext.JavaClass.ALARM_SERVICE);
  Result := TJAlarmManager.Wrap((AlarmManagerService as ILocalObject).GetObjectID);
end;

function SharedPrivatePreferences: JSharedPreferences;
begin
  Result := SharedActivity.getPreferences(TJActivity.JavaClass.MODE_PRIVATE);
end;

function GetApplicationTitle: string;
begin
  Result := JCharSequenceToString(SharedActivityContext.getPackageManager.getApplicationLabel(SharedApplicationInfo));
end;

function GetPackagePath: string;
begin
  Result := JStringToString(SharedApplicationInfo.nativeLibraryDir);
end;

function IsGingerbreadDevice: Boolean;
begin
  Result := TOSVersion.Major = 2;
end;

function StrToJCharSequence(const ASource: string): JCharSequence;
begin
  Result := TJCharSequence.Wrap((StringToJString(ASource) as ILocalObject).GetObjectID);
end;

function JCharSequenceToString(const ASource: JCharSequence): string;
begin
  Result := JStringToString(ASource.toString);
end;

function StrToJURI(const ASource: string): Jnet_Uri;
var
  NativeStr: JString;
begin
  NativeStr := StringToJString(ASource);
  Result := TJnet_Uri.JavaClass.parse(NativeStr);
end;

function HasSystemService(const AFeatureName: JString): Boolean;
var
  PackageManager: JPackageManager;
begin
  PackageManager := SharedActivityContext.getPackageManager;
  Result := PackageManager.hasSystemFeature(AFeatureName);
end;

function GetResourceID(const ResourceName: string): Integer;
begin
  Result := SharedActivity.getResources.getIdentifier(StringToJString(ResourceName), nil, nil);
end;

function GetResourceString(const ResourceID: Integer): string;
var
  NativeString: JString;
begin
  NativeString := SharedActivity.getResources.getString(ResourceID);
  if Assigned(NativeString) then
    Result := JStringToString(NativeString)
  else
    Result := EmptyStr;
end;

function JBitmapToSurface(const SrcBitmap: JBitmap; const DestSurface: TBitmapSurface): Boolean;
var
  PixelBuffer: Pointer;
  BitmapInfo: AndroidBitmapInfo;
begin
  Result := False;

  FillChar(BitmapInfo, SizeOf(BitmapInfo), 0);

  if AndroidBitmap_getInfo(TJNIResolver.GetJNIEnv, (SrcBitmap as ILocalObject).GetObjectID, @BitmapInfo) <> 0 then
    Exit;

  if AndroidBitmap_lockPixels(TJNIResolver.GetJNIEnv, (SrcBitmap as ILocalObject).GetObjectID, @PixelBuffer) <> 0 then
    Exit;

  try
    DestSurface.SetSize(BitmapInfo.Width, BitmapInfo.Height);
    Move(PixelBuffer^, DestSurface.Bits^, DestSurface.Pitch * DestSurface.Height);

    Result := True;
  finally
    AndroidBitmap_unlockPixels(TJNIResolver.GetJNIEnv, (SrcBitmap as ILocalObject).GetObjectID);
  end;
end;

function SurfaceToJBitmap(const SrcSurface: TBitmapSurface; const DestBitmap: JBitmap): Boolean;
var
  PixelBuffer: Pointer;
  BitmapInfo: AndroidBitmapInfo;
begin
  Result := False;

  FillChar(BitmapInfo, SizeOf(BitmapInfo), 0);

  if AndroidBitmap_getInfo(TJNIResolver.GetJNIEnv, (DestBitmap as ILocalObject).GetObjectID, @BitmapInfo) <> 0 then
    Exit;

  if AndroidBitmap_lockPixels(TJNIResolver.GetJNIEnv, (DestBitmap as ILocalObject).GetObjectID, @PixelBuffer) <> 0 then
    Exit;

  try
    Move(SrcSurface.Bits^, PixelBuffer^, SrcSurface.Pitch * SrcSurface.Height);

    Result := True;
  finally
    AndroidBitmap_unlockPixels(TJNIResolver.GetJNIEnv, (DestBitmap as ILocalObject).GetObjectID);
  end;
end;

function GetJDisplay: JDisplay;
var
  WinManager: JWindowManager;
begin
  if (SharedActivity <> nil) then
  begin
    WinManager := SharedActivity.getWindowManager;
    if Assigned(WinManager) then
      Result := WinManager.getDefaultDisplay
    else
      Result := nil;
  end else
    Result := nil;
end;

function GetJDisplayMetrics: JDisplayMetrics;
var
  Display: JDisplay;
begin
  Display := GetJDisplay;
  if (Display <> nil) then
  begin
    Result := TJDisplayMetrics.Create;
    Display.getMetrics(Result);
  end else
    Result := nil;
end;

type

  TRunnable = class (TJavaLocal, JRunnable)
  strict private
    FActivity: JActivity;
    FMethodCallback: TMethodCallback;
    FCallback: TCallBack;
    FFinished: Boolean;
  public
    constructor Create; overload;
    constructor Create(ACallback: TCallBack); overload;
    constructor Create(AMethodCallback: TMethodCallback); overload;
    procedure Start;
    { JRunnable }
    procedure run; cdecl;
    property Finished: Boolean read FFinished;
  end;

  TFinishedRunnableCollector = class
  strict private class var
    FFinishedThreadCollector : TFinishedRunnableCollector;
  strict private
    FTimerService: IFMXTimerService;
    FTimerHandle: Cardinal;
    procedure DoTimer;
    function SupportTimer: Boolean;
    function HasTimer: Boolean;
    procedure Start(Timeout: Integer = 2000);
  public
    constructor Create;
    class procedure Call(Timeout: Integer = 2000);
  end;

var
  ActiveJavaRunnables: TThreadList<TRunnable>;

procedure CallInUIThread(AMethod: TMethodCallback); overload;
var
  Runnable: TRunnable;
begin
  Runnable := TRunnable.Create(AMethod);
  ActiveJavaRunnables.Add(Runnable);
  Runnable.Start;
end;

procedure CallInUIThread(AMethod: TCallBack); overload;
var
  Runnable: TRunnable;
begin
  Runnable := TRunnable.Create(AMethod);
  ActiveJavaRunnables.Add(Runnable);
  Runnable.Start;
end;

procedure CallInUIThreadAndWaitFinishing(AMethod: TMethodCallback); overload;
var
  Event: TEvent;
begin
  Event := TEvent.Create;
  try
    CallInUIThread(procedure()
      begin
        if Assigned(AMethod) then
          AMethod;
        Event.SetEvent;
      end);
    Event.WaitFor;
  finally
    Event.DisposeOf;
  end;
end;

procedure CallInUIThreadAndWaitFinishing(AMethod: TCallBack); overload;
var
  Event: TEvent;
begin
  Event := TEvent.Create;
  try
    CallInUIThread(procedure()
      begin
        if Assigned(AMethod) then
          AMethod;
        Event.SetEvent;
      end);
    Event.WaitFor;
  finally
    Event.DisposeOf;
  end;
end;

{ TRunnable }

constructor TRunnable.Create(ACallback: TCallBack);
begin
  Create;
  FCallback := ACallback;
end;

constructor TRunnable.Create(AMethodCallback: TMethodCallback);
begin
  Create;
  FMethodCallback := AMethodCallback;
end;

constructor TRunnable.Create;
begin
  inherited Create;
  FFinished := False;
  FActivity := SharedActivity;
end;

procedure TRunnable.run;
begin
  try
    if Assigned(FCallback) then
      FCallback;
    if Assigned(FMethodCallback) then
      FMethodCallback;
  finally
    FFinished := True;
    TFinishedRunnableCollector.Call;
  end;
end;

procedure TRunnable.Start;
begin
  if Assigned(FActivity) then
    FActivity.runOnUiThread(Self);
end;

{ TFinishedThreadCollector }

procedure TFinishedRunnableCollector.Start(Timeout: Integer = 2000);
begin
  if SupportTimer then
  begin
    if HasTimer then
      FTimerService.DestroyTimer(FTimerHandle);
    FTimerHandle := FTimerService.CreateTimer(Timeout, DoTimer);
  end;
end;

function TFinishedRunnableCollector.SupportTimer: Boolean;
begin
  Result := Assigned(FTimerService);
end;

procedure TFinishedRunnableCollector.DoTimer;
var
  I: Integer;
  JavaThread: TRunnable;
  List: TList<TRunnable>;
begin
  try
    List := ActiveJavaRunnables.LockList;
    try
      I := 0;
      while (I < List.Count) do
      begin
        JavaThread := List[I];
        if JavaThread.Finished then
          List.Remove(JavaThread)
        else
          Inc(I);
      end;
    finally
      ActiveJavaRunnables.UnlockList;
    end;
  finally
    if SupportTimer then
      FTimerService.DestroyTimer(FTimerHandle);
  end;
end;

function TFinishedRunnableCollector.HasTimer: Boolean;
begin
  Result := FTimerHandle <> 0;
end;

constructor TFinishedRunnableCollector.Create;
begin
  TPlatformServices.Current.SupportsPlatformService(IFMXTimerService, IInterface(FTimerService));
  FTimerHandle := 0;
end;

class procedure TFinishedRunnableCollector.Call(Timeout: Integer);
begin
  if not Assigned(FFinishedThreadCollector) then
    FFinishedThreadCollector := TFinishedRunnableCollector.Create;

  FFinishedThreadCollector.Start;
end;

initialization
  ActiveJavaRunnables := TThreadList<TRunnable>.Create;

finalization
  ActiveJavaRunnables.DisposeOf;
end.
