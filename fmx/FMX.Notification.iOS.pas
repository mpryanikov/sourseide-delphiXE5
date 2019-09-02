{*******************************************************}
{                                                       }
{         Delphi FireMonkey Notification Service        }
{                                                       }
{       Implementation Notification Center for iOS      }
{                                                       }
{ Copyright(c) 2012-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

// Reference on programm guide in Apple developer center:
// https://developer.apple.com/library/ios/#documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/Introduction/Introduction.html

unit FMX.Notification.iOS;

interface

procedure RegisterNotificationService;
procedure UnregisterNotificationService;

implementation

uses
  FMX.Notification, FMX.Platform, FMX.Helpers.iOS, FMX.Messages, FMX.Forms,
  System.SysUtils, System.Classes, System.Generics.Collections,
  Macapi.ObjectiveC,
  iOSapi.Foundation, iOSapi.CocoaTypes, iOSapi.UIKit;

type

{ TNotificationCenterCocoa }

  TNotificationCenterCocoa = class sealed (TInterfacedObject, IFMXNotificationCenter)
  strict private
    FDelayedNotifications: TObjectList<TNotification>;
    FApplicationLoaded: Boolean;
    { Subscriptions }
    FSubscriptionNotificationReceivedID: Integer;
    FSubscriptionFormsLoadedID: Integer;
    { Creation and manipulation with notifications }
    function CreateNativeNotification(const ANotification: TNotification): UILocalNotification;
    function ConvertNativeToDelphiNotification(const ANotification: UILocalNotification): TNotification;
    function FindNativeNotification(const AID: string; var ANotification: UILocalNotification): Boolean;
    { Global External event }
    procedure DoReceiveLocalNotification(const Sender: TObject; const M: TMessage);
    procedure DidFormsLoad(const Sender: TObject; const M: TMessage);
    { Delayed notifications }
    procedure SendDelayedNotifications;
    procedure ClearDelayedNotifications;
  public
    constructor Create;
    destructor Destroy; override;
    { IFMXNotificationCenter }
    function GetCurrentNotifications: TNotifications;
    function FindNotification(const AName: string): TNotification;
    procedure ScheduleNotification(const ANotification: TNotification);
    procedure PresentNotification(const ANotification: TNotification);
    procedure CancelNotification(const AName: string); overload;
    procedure CancelNotification(const ANotification: TNotification); overload;
    procedure CancelAllNotifications;
    procedure SetIconBadgeNumber(const ACount: Integer);
    function GetIconBadgeNumber: Integer;
    procedure ResetIconBadgeNumber;
  end;

var
  NotificationCenter: TNotificationCenterCocoa;

procedure RegisterNotificationService;
begin
  NotificationCenter := TNotificationCenterCocoa.Create;
  TPlatformServices.Current.AddPlatformService(IFMXNotificationCenter, NotificationCenter);
end;

procedure UnregisterNotificationService;
begin
  TPlatformServices.Current.RemovePlatformService(IFMXNotificationCenter);
  NotificationCenter := nil;
end;

{$REGION 'TNotificationCenterCocoa'}

procedure TNotificationCenterCocoa.CancelAllNotifications;
begin
  SharedApplication.cancelAllLocalNotifications;
end;

function TNotificationCenterCocoa.FindNativeNotification(const AID: string; var ANotification: UILocalNotification): Boolean;

  function FindInScheduledNotifications: UILocalNotification;
  var
    Notifications: NSArray;
    NativeNotification: UILocalNotification;
    Found: Boolean;
    I: NSUInteger;
    UserInfo: NSDictionary;
  begin
    Notifications := SharedApplication.scheduledLocalNotifications;
    Found := False;
    I := 0;
    while (I < Notifications.count) and not Found do
    begin
      NativeNotification := TUILocalNotification.Wrap(Notifications.objectAtIndex(I));
      UserInfo := NativeNotification.userInfo;
      if Assigned(UserInfo) and (UTF8ToString(TNSString.Wrap(UserInfo.valueForKey(NSSTR('id'))).UTF8String) = AID) then
        Found := True
      else
        Inc(I);
    end;
    if Found then
      Result := NativeNotification
    else
      Result := nil;
  end;

begin
  // We are searching notification in two list:
  //   1. Notifications, which have not displayed in Notification Center
  //   2. Notifications, which already displayed
  ANotification := FindInScheduledNotifications;
  Result := Assigned(ANotification);
end;

function TNotificationCenterCocoa.FindNotification(
  const AName: string): TNotification;
var
  NativeNotification: UILocalNotification;
begin
  if FindNativeNotification(AName, NativeNotification) then
    Result := ConvertNativeToDelphiNotification(NativeNotification)
  else
    Result := nil;
end;

function TNotificationCenterCocoa.GetCurrentNotifications: TNotifications;
var
  Notifications: NSArray;
  NativeNotification: UILocalNotification;
  I: Integer;
begin
  Notifications := SharedApplication.scheduledLocalNotifications;
  SetLength(Result, Notifications.count);
  for I := 0 to Integer(Notifications.count) - 1 do
  begin
    NativeNotification := TUILocalNotification.Wrap(Notifications.objectAtIndex(I));
    Result[I] := ConvertNativeToDelphiNotification(NativeNotification)
  end;
end;

procedure TNotificationCenterCocoa.CancelNotification(const AName: string);
var
  NativeNotification: UILocalNotification;
begin
  if FindNativeNotification(AName, NativeNotification) then
    SharedApplication.cancelLocalNotification(NativeNotification);
end;

procedure TNotificationCenterCocoa.CancelNotification(const ANotification: TNotification);
begin
  if not Assigned(ANotification) then
    Exit;
  CancelNotification(ANotification.Name);
end;

procedure TNotificationCenterCocoa.ClearDelayedNotifications;
begin
  FDelayedNotifications.Clear;
end;

constructor TNotificationCenterCocoa.Create;
begin
  FSubscriptionNotificationReceivedID := TMessageManager.DefaultManager.SubscribeToMessage(TMessage<UILocalNotification>, DoReceiveLocalNotification);
  FSubscriptionFormsLoadedID := TMessageManager.DefaultManager.SubscribeToMessage(TFormsCreatedMessage, DidFormsLoad);
  FDelayedNotifications := TObjectList<TNotification>.Create;
  FApplicationLoaded := ApplicationState = TApplicationState.asRunning;
end;

function TNotificationCenterCocoa.CreateNativeNotification(const ANotification: TNotification): UILocalNotification;
var
  NativeNotification: UILocalNotification;
  UserInfo: NSDictionary;
  GMTDateTime: TDateTime;
begin
  NativeNotification := TUILocalNotification.Create;
  if not ANotification.Name.IsEmpty then
  begin
    // Set unique identificator
    UserInfo := TNSDictionary.Wrap(TNSDictionary.OCClass.dictionaryWithObject(
      (NSSTR(ANotification.Name) as ILocalObject).GetObjectID, (NSSTR('id') as ILocalObject).GetObjectID));
    NativeNotification.setUserInfo(UserInfo);
  end;
  // Get GMT time and set notification fired date
  GMTDateTime := GetGMTDateTime(ANotification.FireDate);
  NativeNotification.setTimeZone(TNSTimeZone.Wrap(TNSTimeZone.OCClass.defaultTimeZone));
  NativeNotification.setFireDate(DateTimeToNSDate(GMTDateTime));
  NativeNotification.setApplicationIconBadgeNumber(ANotification.Number);
  NativeNotification.setAlertBody(NSSTR(ANotification.AlertBody));
  NativeNotification.setAlertAction(NSSTR(ANotification.AlertAction));
  NativeNotification.setHasAction(ANotification.HasAction);
  if ANotification.EnableSound then
    NativeNotification.setSoundName(UILocalNotificationDefaultSoundName)
  else
    NativeNotification.setSoundName(nil);
  Result := NativeNotification;
end;

destructor TNotificationCenterCocoa.Destroy;
begin
  { Unsibscribe }
  TMessageManager.DefaultManager.Unsubscribe(TMessage<UILocalNotification>, FSubscriptionNotificationReceivedID);
  TMessageManager.DefaultManager.Unsubscribe(TFormsCreatedMessage, FSubscriptionFormsLoadedID);
  { Destroying }
  ClearDelayedNotifications;
  FDelayedNotifications.DisposeOf;
  inherited Destroy;
end;

procedure TNotificationCenterCocoa.DidFormsLoad(const Sender: TObject; const M: TMessage);
begin
  FApplicationLoaded := True;
  SendDelayedNotifications;
end;

procedure TNotificationCenterCocoa.DoReceiveLocalNotification(const Sender: TObject; const M: TMessage);

  procedure SendNotification(Notification: TNotification);
  begin
    TMessageManager.DefaultManager.SendMessage(Self, TMessage<TNotification>.Create(Notification));
    // Sending Delayed notifications
    if FDelayedNotifications.Count > 0 then
      SendDelayedNotifications;
  end;

var
  NativeNotification: UILocalNotification;
  Notification: TNotification;
begin
  if M is TMessage<UILocalNotification> then
  begin
    NativeNotification := (M as TMessage<UILocalNotification>).Value;
    // iOS doesn't provide list of presented notification. So we need to store it
    // in our list for cancelling in future with using ID
    Notification := ConvertNativeToDelphiNotification(NativeNotification);
    try
      if not FApplicationLoaded then
        FDelayedNotifications.Add(Notification)
      else
        SendNotification(Notification);
    finally
      FreeAndNil(Notification);
    end;
  end;
end;

function TNotificationCenterCocoa.ConvertNativeToDelphiNotification(
  const ANotification: UILocalNotification): TNotification;
var
  UserInfo: NSDictionary;
  NotificationTmp: TNotification;
begin
  NotificationTmp := nil;
  if Assigned(ANotification) then
  begin
    NotificationTmp := TNotification.Create;
    UserInfo := ANotification.userInfo;
    if Assigned(UserInfo) then
      NotificationTmp.Name := UTF8ToString(TNSString.Wrap(UserInfo.valueForKey(NSSTR('id'))).UTF8String);
    if Assigned(ANotification.AlertBody) then
      NotificationTmp.AlertBody := UTF8ToString(ANotification.AlertBody.UTF8String);
    if Assigned(ANotification.AlertAction) then
      NotificationTmp.AlertAction := UTF8ToString(ANotification.AlertAction.UTF8String);;
    NotificationTmp.Number := ANotification.ApplicationIconBadgeNumber;
    NotificationTmp.FireDate := NSDateToDateTime(ANotification.FireDate);
    NotificationTmp.EnableSound := Assigned(ANotification.SoundName);
    NotificationTmp.HasAction := ANotification.HasAction;
  end;
  Result := NotificationTmp;
end;

procedure TNotificationCenterCocoa.PresentNotification(const ANotification: TNotification);
var
  NativeNatification: UILocalNotification;
begin
  CancelNotification(ANotification);
  NativeNatification := CreateNativeNotification(ANotification);
  SharedApplication.presentLocalNotificationNow(NativeNatification);
end;

procedure TNotificationCenterCocoa.ScheduleNotification(const ANotification: TNotification);
var
  NativeNatification: UILocalNotification;
begin
  CancelNotification(ANotification);
  NativeNatification := CreateNativeNotification(ANotification);
  SharedApplication.scheduleLocalNotification(NativeNatification);
end;

procedure TNotificationCenterCocoa.SendDelayedNotifications;
var
  Notification: TNotification;
begin
  for Notification in FDelayedNotifications do
    TMessageManager.DefaultManager.SendMessage(Self, TMessage<TNotification>.Create(Notification));
  ClearDelayedNotifications;
end;

function TNotificationCenterCocoa.GetIconBadgeNumber: Integer;
begin
  Result := SharedApplication.ApplicationIconBadgeNumber;
end;

procedure TNotificationCenterCocoa.SetIconBadgeNumber(const ACount: Integer);
begin
  SharedApplication.setApplicationIconBadgeNumber(ACount);
end;

procedure TNotificationCenterCocoa.ResetIconBadgeNumber;
begin
  SharedApplication.setApplicationIconBadgeNumber(0);
end;

{$ENDREGION}

end.