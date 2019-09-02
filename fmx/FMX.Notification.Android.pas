{*******************************************************}
{                                                       }
{         Delphi FireMonkey Notification Service        }
{                                                       }
{     Implementation Notification Center for Android    }
{                                                       }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit FMX.Notification.Android;

interface

uses
  System.SysUtils, Androidapi.JNI.Embarcadero;

procedure RegisterNotificationService;
procedure UnregisterNotificationService;

implementation

uses
  System.DateUtils, System.Classes,
  FMX.Notification, FMX.Platform, FMX.Platform.Android, FMX.Helpers.Android, FMX.Messages, FMX.Forms,
  Androidapi.JNI.App, Androidapi.JNI.Support, Androidapi.JNI.JavaTypes, Androidapi.JNI.Media,
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNIBridge, Androidapi.JNI.Net,
  FMX.Consts;

const
  FMX_NOTIFICATION_CENTER = 'FMX_NOTIFICATION_CENTER';

type

  { TNotificationCenterAndroid }

  TAndroidPreferenceAdapter = class
  const
    SETTINGS_NOTIFICATION_IDS = 'SETTINGS_NOTIFICATION_IDS';
  strict private
    FPreference: JSharedPreferences;
    function FindNotification(const AName: string; out AIndex: Integer; out AID: Integer): Boolean;
    function ExtractName(const AStr: string): string;
    function ExtractID(const AStr: string): Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SaveNotification(const ANotification: TNotification; const AID: Integer);
    procedure RemoveNotification(const AName: string);
    function IndexOf(const AName: string): Integer;
    function Find(const AName: string; out Index: Integer): Boolean;
    function GetID(const AName: string): Integer;
    function Contains(const AName: string): Boolean;
    { DEBUG }
    procedure Print;
  end;

  TNotificationCenterAndroid = class (TInterfacedObject, IFMXNotificationCenter)
  strict private
    FExternalStore: TAndroidPreferenceAdapter;
    FNotificationManager: JNotificationManager;
    { Messages Subscription }
    FSubscriptionFormsLoadedID: Integer;
    FSubscriptionReceivedNotificationID: Integer;
    function CreateNativeNotification(const ANotification: TNotification): JNotification;
    procedure SaveNotificationIntoIntent(var AIntent: JIntent; const ANotification: TNotification; AID: Integer = -1);
    function LoadNotificationFromIntent(const AIntent: JIntent): TNotification;
    { Global FMX event }
    procedure DidFormsLoad(const Sender: TObject; const M: TMessage);
    procedure DidReceiveNotification(const Sender: TObject; const M: TMessage);
  public
    constructor Create;
    destructor Destroy; override;
    { IFMXNotificationCenter }
    procedure ScheduleNotification(const ANotification: TNotification);
    procedure PresentNotification(const ANotification: TNotification);
    procedure CancelNotification(const AName: string); overload;
    procedure CancelNotification(const ANotification: TNotification); overload;
    procedure CancelAllNotifications;
    { Not supported }
    procedure SetIconBadgeNumber(const ACount: Integer);
    function GetIconBadgeNumber: Integer;
    procedure ResetIconBadgeNumber;
  end;

  TGeneratorUniqueID = class
  const
    SETTINGS_NOTIFICATION_UNIQUE_ID = 'SETTINGS_NOTIFICATION_UNIQUE_ID';
  strict private
    class var FNextUniqueID: Int64;
  public
    class constructor Create;
    class function GenerateID: Integer;
  end;

var
  NotificationCenter: TNotificationCenterAndroid;

procedure RegisterNotificationService;
begin
  NotificationCenter := TNotificationCenterAndroid.Create;
  TPlatformServices.Current.AddPlatformService(IFMXNotificationCenter, NotificationCenter);
end;

procedure UnregisterNotificationService;
begin
  TPlatformServices.Current.RemovePlatformService(IFMXNotificationCenter);
  NotificationCenter := nil;
end;

function GetNotificationService: JNotificationManager;
var
  NotificationServiceNative: JObject;
begin
  NotificationServiceNative := SharedActivityContext.getSystemService(TJContext.JavaClass.NOTIFICATION_SERVICE);
  Result := TJNotificationManager.Wrap((NotificationServiceNative as ILocalObject).GetObjectID);
end;

{$REGION 'TNotificationCenterAndroid'}

function DateTimeLocalToUnixMSecGMT(const ADateTime: TDateTime): Int64;
begin
  Result := DateTimeToUnix(ADateTime) * MSecsPerSec - Round(TTimeZone.Local.UtcOffset.TotalMilliseconds);
end;

function TNotificationCenterAndroid.CreateNativeNotification(const ANotification: TNotification): JNotification;

  function GetDefaultNotificationSound: Jnet_Uri;
  begin
    Result := TJRingtoneManager.JavaClass.getDefaultUri(TJRingtoneManager.JavaClass.TYPE_NOTIFICATION);
  end;

  function GetDefaultIconID: Integer;
  begin
    Result := SharedActivityContext.getApplicationInfo.icon;
  end;

  function GetDefaultIcon: JBitmap;
  begin
    Result := TJBitmapFactory.JavaClass.decodeResource(SharedActivityContext.getResources(), GetDefaultIconID);
  end;

  function GetContentTitle: JCharSequence;
  begin
    Result := StrToJCharSequence(GetApplicationTitle);
  end;

  function GetContentText: JCharSequence;
  begin
    Result := StrToJCharSequence(ANotification.AlertBody);
  end;

  function GetContentIntent: JPendingIntent;
  var
    Intent: JIntent;
  begin
    Intent := TJIntent.Create;
    Intent.setClass(SharedActivityContext, SharedActivityContext.getClass);
    Intent.setFlags(TJIntent.JavaClass.FLAG_ACTIVITY_SINGLE_TOP or TJIntent.JavaClass.FLAG_ACTIVITY_CLEAR_TOP);
    SaveNotificationIntoIntent(Intent, ANotification);
    Result := TJPendingIntent.JavaClass.getActivity(SharedActivityContext, TGeneratorUniqueID.GenerateID, Intent, 0);
  end;

var
  NotificationBuilder: JNotificationCompat_Builder;
begin
  NotificationBuilder := TJNotificationCompat_Builder.JavaClass.init(SharedActivityContext);
  NotificationBuilder := NotificationBuilder.setSmallIcon(GetDefaultIconID);
  NotificationBuilder := NotificationBuilder.setContentTitle(GetContentTitle);
  NotificationBuilder := NotificationBuilder.setContentText(GetContentText);
  NotificationBuilder := NotificationBuilder.setTicker(GetContentText);
  NotificationBuilder := NotificationBuilder.setContentIntent(GetContentIntent);
  NotificationBuilder := NotificationBuilder.setNumber(ANotification.Number);
  NotificationBuilder := NotificationBuilder.setAutoCancel(True);
  NotificationBuilder := NotificationBuilder.setWhen(TJDate.Create.getTime);
  if ANotification.EnableSound then
    NotificationBuilder := NotificationBuilder.setSound(GetDefaultNotificationSound);
  // Action buttons won't appear on platforms prior to Android 4.1!!!
  // http://developer.android.com/reference/android/support/v4/app/NotificationCompat.Builder.html#addAction
  // This line are left for a future feature with notification button
  {
    if ANotification.HasAction then
    begin
      NotificationBuilder.addAction(0, StrToJCharSequence(ANotification.AlertAction), GetContentIntent);
      NotificationBuilder.addAction(0, StrToJCharSequence(SNotificationCancel), GetContentIntent);
    end;
  }
  Result := NotificationBuilder.Build;
end;

procedure TNotificationCenterAndroid.SaveNotificationIntoIntent(var AIntent: JIntent;
  const ANotification: TNotification; AID: Integer);
begin
  AIntent.setAction(TJFMXNotificationAlarm.JavaClass.ACTION_FMX_NOTIFICATION);
  AIntent.putExtra(TJFMXNotificationAlarm.JavaClass.EXTRA_UNIQUE_ID, AID);
  AIntent.putExtra(TJFMXNotificationAlarm.JavaClass.EXTRA_NAME, StringToJString(ANotification.Name));
  AIntent.putExtra(TJFMXNotificationAlarm.JavaClass.EXTRA_ALERT_BODY, StringToJString(ANotification.AlertBody));
  AIntent.putExtra(TJFMXNotificationAlarm.JavaClass.EXTRA_ALERT_ACTION, StringToJString(ANotification.AlertAction));
  AIntent.putExtra(TJFMXNotificationAlarm.JavaClass.EXTRA_NUMBER, ANotification.Number);
  AIntent.putExtra(TJFMXNotificationAlarm.JavaClass.EXTRA_FIRE_DATE, ANotification.FireDate);
  AIntent.putExtra(TJFMXNotificationAlarm.JavaClass.EXTRA_FIRE_GMT_DATE, DateTimeLocalToUnixMSecGMT(ANotification.FireDate));
  AIntent.putExtra(TJFMXNotificationAlarm.JavaClass.EXTRA_ENABLE_SOUND, ANotification.EnableSound);
  AIntent.putExtra(TJFMXNotificationAlarm.JavaClass.EXTRA_HAS_ACTION, ANotification.HasAction);
  AIntent.putExtra(TJFMXNotificationAlarm.JavaClass.EXTRA_ACTIVITY_CLASS_NAME, SharedActivity.getClass.getName);
  AIntent.putExtra(TJFMXNotificationAlarm.JavaClass.EXTRA_TITLE, StringToJString(GetApplicationTitle));
end;

function TNotificationCenterAndroid.LoadNotificationFromIntent(const AIntent: JIntent): TNotification;
begin
  Result := TNotification.Create;
  Result.Name := JStringToString(AIntent.getStringExtra(TJFMXNotificationAlarm.JavaClass.EXTRA_NAME));
  Result.AlertBody := JStringToString(AIntent.getStringExtra(TJFMXNotificationAlarm.JavaClass.EXTRA_ALERT_BODY));
  Result.AlertAction := JStringToString(AIntent.getStringExtra(TJFMXNotificationAlarm.JavaClass.EXTRA_ALERT_ACTION));
  Result.Number := AIntent.getIntExtra(TJFMXNotificationAlarm.JavaClass.EXTRA_NUMBER,0);
  Result.FireDate := AIntent.getDoubleExtra(TJFMXNotificationAlarm.JavaClass.EXTRA_FIRE_DATE, Now);
  Result.HasAction := AIntent.getBooleanExtra(TJFMXNotificationAlarm.JavaClass.EXTRA_HAS_ACTION, True);
  Result.EnableSound := AIntent.getBooleanExtra(TJFMXNotificationAlarm.JavaClass.EXTRA_ENABLE_SOUND, True);
end;

procedure TNotificationCenterAndroid.DidFormsLoad(const Sender: TObject; const M: TMessage);

  function IsIntentWithNotification(Intent: JIntent): Boolean;
  begin
    Result := Assigned(Intent) and Assigned(Intent.getAction) and
      Intent.getAction.equals(TJFMXNotificationAlarm.JavaClass.ACTION_FMX_NOTIFICATION);
  end;

var
  InputIntent: JIntent;
  Notification: TNotification;
begin
  InputIntent := SharedActivity.getIntent;
  if IsIntentWithNotification(InputIntent) then
  begin
    Notification := LoadNotificationFromIntent(InputIntent);
    try
      TMessageManager.DefaultManager.SendMessage(Self, TMessage<TNotification>.Create(Notification));
    finally
       Notification.DisposeOf;
    end;
  end;
end;

procedure TNotificationCenterAndroid.DidReceiveNotification(const Sender: TObject; const M: TMessage);

  function IsIntentWithNotification(Intent: JIntent): Boolean;
  begin
    Result := Assigned(Intent) and Assigned(Intent.getAction) and
      Intent.getAction.equals(TJFMXNotificationAlarm.JavaClass.ACTION_FMX_NOTIFICATION);
  end;

var
  InputIntent: JIntent;
  Notification: TNotification;
begin
  if M is TMessageReceivedNotification then
  begin
    InputIntent := (M as TMessageReceivedNotification).Value;
    if IsIntentWithNotification(InputIntent) then
    begin
      Notification := LoadNotificationFromIntent(InputIntent);
      try
        TMessageManager.DefaultManager.SendMessage(Self, TMessage<TNotification>.Create(Notification));
      finally
         Notification.DisposeOf;
      end;
    end;
  end;
end;

constructor TNotificationCenterAndroid.Create;
begin
  FExternalStore := TAndroidPreferenceAdapter.Create;
  FNotificationManager := GetNotificationService;

  { Subscription }
  FSubscriptionFormsLoadedID := TMessageManager.DefaultManager.SubscribeToMessage(TFormsCreatedMessage, DidFormsLoad);
  FSubscriptionReceivedNotificationID := TMessageManager.DefaultManager.SubscribeToMessage(TMessageReceivedNotification, DidReceiveNotification);
end;

destructor TNotificationCenterAndroid.Destroy;
begin
  FExternalStore.DisposeOf;
  FNotificationManager := nil;
  { Unsibscribe }
  TMessageManager.DefaultManager.Unsubscribe(TFormsCreatedMessage, FSubscriptionFormsLoadedID);
  TMessageManager.DefaultManager.Unsubscribe(TMessageReceivedNotification, FSubscriptionReceivedNotificationID);
  inherited Destroy;
end;

procedure TNotificationCenterAndroid.PresentNotification(const ANotification: TNotification);
var
  NativeNotification: JNotification;
begin
  NativeNotification := CreateNativeNotification(ANotification);
  if ANotification.Name.IsEmpty then
    FNotificationManager.notify(TGeneratorUniqueID.GenerateID, NativeNotification)
  else
    FNotificationManager.notify(StringToJString(ANotification.Name), 0, NativeNotification);
  NativeNotification := nil;
end;

procedure TNotificationCenterAndroid.ScheduleNotification(const ANotification: TNotification);

  function CreateNotificationAlarmIntent(const AID: Integer): JPendingIntent;
  var
    Intent: JIntent;
    Alarm: JFMXNotificationAlarm;
  begin
    Alarm := TJFMXNotificationAlarm.Create;
    Intent := TJIntent.Create;
    Intent.setClass(SharedActivityContext, Alarm.getClass);
    Intent.setAction(TJFMXNotificationAlarm.JavaClass.ACTION_FMX_NOTIFICATION);
    SaveNotificationIntoIntent(Intent, ANotification, AID);
    Result := TJPendingIntent.JavaClass.getBroadcast(SharedActivityContext, AID, Intent,
      TJPendingIntent.JavaClass.FLAG_UPDATE_CURRENT);
  end;

var
  PendingIntent: JPendingIntent;
  ID: Integer;
begin
  if not ANotification.Name.IsEmpty and FExternalStore.Contains(ANotification.Name) then
    CancelNotification(ANotification.Name);

  ID := TGeneratorUniqueID.GenerateID;
  PendingIntent := CreateNotificationAlarmIntent(ID);
  FExternalStore.SaveNotification(ANotification, ID);
  SharedAlarmManager.&set(TJAlarmManager.JavaClass.RTC_WAKEUP,
    DateTimeLocalToUnixMSecGMT(ANotification.FireDate), PendingIntent);
end;

procedure TNotificationCenterAndroid.CancelAllNotifications;
begin
  FNotificationManager.cancelAll;
end;

procedure TNotificationCenterAndroid.CancelNotification(const ANotification: TNotification);
begin
  CancelNotification(ANotification.Name);
end;

procedure TNotificationCenterAndroid.CancelNotification(const AName: string);
var
  ID: Integer;
  Intent: JIntent;
  Alarm: JFMXNotificationAlarm;
  PendingIntent: JPendingIntent;
begin
  FNotificationManager.cancel(StringToJString(AName), 0);

  if FExternalStore.Contains(AName) then
  begin
    ID := FExternalStore.GetID(AName);
    Alarm := TJFMXNotificationAlarm.Create;
    Intent := TJIntent.Create;
    Intent.setClass(SharedActivityContext, Alarm.getClass);
    Intent.setAction(TJFMXNotificationAlarm.JavaClass.ACTION_FMX_NOTIFICATION);
    PendingIntent := TJPendingIntent.JavaClass.getBroadcast(SharedActivityContext, ID, Intent,
      TJPendingIntent.JavaClass.FLAG_UPDATE_CURRENT);
    SharedAlarmManager.cancel(PendingIntent);
    FExternalStore.RemoveNotification(AName);
  end;
end;

procedure TNotificationCenterAndroid.SetIconBadgeNumber(const ACount: Integer);
begin
  // Android doesn't have Number icon on application Icon
end;

function TNotificationCenterAndroid.GetIconBadgeNumber: Integer;
begin
  // Android doesn't have Number icon on application Icon
  Result := 0;
end;

procedure TNotificationCenterAndroid.ResetIconBadgeNumber;
begin
  // Android doesn't have Number icon on application Icon
end;

{$ENDREGION}

{ TGeneratorUniqueID }

class constructor TGeneratorUniqueID.Create;
var
  Preference: JSharedPreferences;
begin
  Preference := SharedActivity.getSharedPreferences(StringToJString(FMX_NOTIFICATION_CENTER), TJContext.JavaClass.MODE_PRIVATE);
  FNextUniqueID := Preference.getInt(StringToJString(SETTINGS_NOTIFICATION_UNIQUE_ID), 0);
end;

class function TGeneratorUniqueID.GenerateID: Integer;
var
  PreferenceEditor: JSharedPreferences_Editor;
  Preference: JSharedPreferences;
begin
  Preference := SharedActivity.getPreferences(TJContext.JavaClass.MODE_PRIVATE);
  PreferenceEditor := Preference.edit;
  try
    PreferenceEditor.putInt(StringToJString(SETTINGS_NOTIFICATION_UNIQUE_ID), FNextUniqueID);
  finally
    PreferenceEditor.commit;
  end;
  Result := FNextUniqueID;
  Inc(FNextUniqueID);
end;

{ TAndroidStorageAdapter }

function TAndroidPreferenceAdapter.FindNotification(const AName: string; out AIndex, AID: Integer): Boolean;
var
  Found: Boolean;
  NotificationPair: string;
  NotificationName: string;
  Notifications: TStringList;
  I: Integer;
  NotificationsStr: JString;
  NotificationID: Integer;
begin
  AIndex := -1;
  AID := -1;
  Notifications := TStringList.Create;
  try
    NotificationsStr := FPreference.getString(StringToJString(SETTINGS_NOTIFICATION_IDS), nil);
    Notifications.Text := JStringToString(NotificationsStr);
    Found := False;
    I := 0;
    while (I < Notifications.Count) and not Found do
    begin
      NotificationPair := Notifications[I];
      NotificationName := ExtractName(NotificationPair);
      NotificationID := ExtractID(NotificationPair);
      if (NotificationID > -1) and (NotificationName = AName) then
      begin
        AIndex := I;
        Found := True;
      end;
      Inc(I);
    end;
    Result := Found;
  finally
    Notifications.DisposeOf;
  end;
end;

function TAndroidPreferenceAdapter.ExtractName(const AStr: string): string;
begin
  Result := AStr.Substring(0, AStr.LastIndexOf('='));
end;

function TAndroidPreferenceAdapter.ExtractID(const AStr: string): Integer;
var
  StrTmp: string;
begin
  StrTmp := AStr.Substring(AStr.LastIndexOf('=') + 1);
  if not TryStrToInt(StrTmp, Result) then
    Result := -1;
end;

constructor TAndroidPreferenceAdapter.Create;
begin
  inherited Create;
  FPreference := SharedActivity.getSharedPreferences(StringToJString(FMX_NOTIFICATION_CENTER), TJContext.JavaClass.MODE_PRIVATE);
end;

destructor TAndroidPreferenceAdapter.Destroy;
begin
  FPreference := nil;
  inherited Destroy;
end;

procedure TAndroidPreferenceAdapter.SaveNotification(const ANotification: TNotification; const AID: Integer);
var
  PreferenceEditor: JSharedPreferences_Editor;
  NotificationsList: TStringList;
  Index: Integer;
  Notifications: JString;
begin
  if not ANotification.Name.IsEmpty then
  begin
    Notifications := FPreference.getString(StringToJString(SETTINGS_NOTIFICATION_IDS), nil);
    NotificationsList := TStringList.Create;
    try
      NotificationsList.Text := JStringToString(Notifications);
      if Find(ANotification.Name, Index) then
        NotificationsList.Delete(Index);
      NotificationsList.Add(ANotification.Name + '=' + AID.ToString);
      PreferenceEditor := FPreference.edit;
      try
        PreferenceEditor.putString(StringToJString(SETTINGS_NOTIFICATION_IDS), StringToJString(NotificationsList.Text));
      finally
        PreferenceEditor.commit;
      end;
    finally
      NotificationsList.DisposeOf;
    end;
  end;
end;

procedure TAndroidPreferenceAdapter.RemoveNotification(const AName: string);
var
  NotificationsList: TStringList;
  Notifications: JString;
  I: Integer;
  Found: Boolean;
  PreferenceEditor: JSharedPreferences_Editor;
begin
  NotificationsList := TStringList.Create;
  try
    Notifications := FPreference.getString(StringToJString(SETTINGS_NOTIFICATION_IDS), nil);
    NotificationsList.Text := JStringToString(Notifications);
    I := 0;
    Found := False;
    while not Found and (I < NotificationsList.Count) do
      if ExtractName(NotificationsList[i]) = AName then
        Found := True;

    if Found then
    begin
      PreferenceEditor := FPreference.edit;
      try
        NotificationsList.Delete(I);
        PreferenceEditor.putString(StringToJString(SETTINGS_NOTIFICATION_IDS), StringToJString(NotificationsList.Text));
      finally
        PreferenceEditor.commit;
      end;
    end;
  finally
    NotificationsList.DisposeOf;
  end;
end;

function TAndroidPreferenceAdapter.IndexOf(const AName: string): Integer;
var
  ID: Integer;
begin
  FindNotification(AName, Result, ID);
end;

function TAndroidPreferenceAdapter.Find(const AName: string; out Index: Integer): Boolean;
var
  ID: Integer;
begin
  Result := FindNotification(AName, Index, ID);
end;

function TAndroidPreferenceAdapter.GetID(const AName: string): Integer;
var
  NotificationsStr: JString;
  Notifications: TStringList;
  IDStr: string;
  Notification: string;
begin
  Notifications := TStringList.Create;
  try
    NotificationsStr := FPreference.getString(StringToJString(SETTINGS_NOTIFICATION_IDS), nil);
    Notifications.Text := JStringToString(NotificationsStr);

    for Notification in Notifications do
    begin
      IDStr := Notification.Substring(Notification.IndexOf('=') + 1);
      if TryStrToInt(IDStr, Result) then
        Break
      else
        Result := -1;
    end;
  finally
    Notifications.DisposeOf;
  end;
end;

function TAndroidPreferenceAdapter.Contains(const AName: string): Boolean;
begin
  Result := IndexOf(AName) > -1;
end;

procedure TAndroidPreferenceAdapter.Print;
var
  Notifications: TStringList;
  NotificationsStr: JString;
  Notification: string;
begin
  Notifications := TStringList.Create;
  try
    NotificationsStr := FPreference.getString(StringToJString(SETTINGS_NOTIFICATION_IDS), nil);
    Notifications.Text := JStringToString(NotificationsStr);
  finally
    Notifications.DisposeOf;
  end;
end;

end. 