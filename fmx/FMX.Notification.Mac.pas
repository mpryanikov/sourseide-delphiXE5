{*******************************************************}
{                                                       }
{         Delphi FireMonkey Notification Service        }
{                                                       }
{           Helpers for MacOS implementations           }
{                                                       }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

// Reference on programm guide in Apple developer center:
// https://developer.apple.com/library/ios/#documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/Introduction/Introduction.html

unit FMX.Notification.Mac;

interface

procedure RegisterNotificationService;
procedure UnregisterNotificationService;

implementation

uses
  System.Classes, System.SysUtils,
  FMX.Notification, FMX.Platform, FMX.Helpers.Mac, FMX.Types,
  Macapi.Foundation, Macapi.ObjectiveC, Macapi.CocoaTypes, FMX.Messages;

type

{ TNotificationCenterCocoa }

  TNotificationCenterDelegate = class;

  TNotificationCenterCocoa = class (TInterfacedObject, IFMXNotificationCenter)
  strict private
    FNotificationCenter: NSUserNotificationCenter;
    FNotificationCenterDelegate: TNotificationCenterDelegate;
    function CreateNativeNotification(const ANotification: TNotification): NSUserNotification;
    function ConverNativeToDelphiNotification(const ANotification: NSUserNotification): TNotification;
    function FindNativeNotification(const AID: string; ANotification: NSUserNotification): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ReceiveNotification(const ANotification: NSUserNotification);
    { IFMXNotificationCenter }
    function GetCurrentNotifications: TNotifications;
    function FindNotification(const AName: string): TNotification;
    procedure ScheduleNotification(const ANotification: TNotification);
    procedure PresentNotification(const ANotification: TNotification);
    procedure CancelNotification(const AName: string); overload;
    procedure CancelNotification(const ANotification: TNotification); overload;
    procedure CancelAllNotifications;
    { Not supported in Mountain Lion }
    procedure SetIconBadgeNumber(const ACount: Integer);
    function GetIconBadgeNumber: Integer;
    procedure ResetIconBadgeNumber;
  end;

{ Notification Center Delegate }

  TNotificationCenterDelegate = class (TOCLocal, NSUserNotificationCenterDelegate)
  strict private
    FNotificationCenter: TNotificationCenterCocoa;
  public
    constructor Create(ANotificationCenter: TNotificationCenterCocoa);
    procedure userNotificationCenter(center: NSUserNotificationCenter; didActivateNotification: NSUserNotification); cdecl;
  end;

var
  NotificationCenter: TNotificationCenterCocoa;

procedure RegisterNotificationService;
begin
  if TOSVersion.Check(10, 8) then
  begin
    NotificationCenter := TNotificationCenterCocoa.Create;
    TPlatformServices.Current.AddPlatformService(IFMXNotificationCenter, NotificationCenter);
  end;
end;

procedure UnregisterNotificationService;
begin
  TPlatformServices.Current.RemovePlatformService(IFMXNotificationCenter);
  NotificationCenter := nil;
end;

{ TNotificationCenterCocoa }

procedure TNotificationCenterCocoa.CancelAllNotifications;
var
  Notifications: NSArray;
  NativeNotification: NSUserNotification;
  I: NSUInteger;
begin
  Notifications := FNotificationCenter.scheduledNotifications;
  for I := 0 to Notifications.count - 1 do
  begin
    NativeNotification := TNSUserNotification.Wrap(Notifications.objectAtIndex(I));
    FNotificationCenter.removeScheduledNotification(NativeNotification);
  end;
  FNotificationCenter.removeAllDeliveredNotifications;
end;

procedure TNotificationCenterCocoa.CancelNotification(const AName: string);
var
  NativeNotification: NSUserNotification;
begin
  if FindNativeNotification(AName, NativeNotification) then
  begin
    FNotificationCenter.removeScheduledNotification(NativeNotification);
    FNotificationCenter.removeDeliveredNotification(NativeNotification);
  end;
end;

procedure TNotificationCenterCocoa.CancelNotification(
  const ANotification: TNotification);
begin
  if Assigned(ANotification) then
    CancelNotification(ANotification.Name);
end;

constructor TNotificationCenterCocoa.Create;
begin
  FNotificationCenter := TNSUserNotificationCenter.Wrap(TNSUserNotificationCenter.OCClass.defaultUserNotificationCenter);
  FNotificationCenter.retain;
  FNotificationCenterDelegate := TNotificationCenterDelegate.Create(Self);
  FNotificationCenter.setDelegate(FNotificationCenterDelegate);
end;

function TNotificationCenterCocoa.CreateNativeNotification(
  const ANotification: TNotification): NSUserNotification;
var
  NativeNotification: NSUserNotification;
  UserInfo: NSDictionary;
  GMTDateTime: TDateTime;
begin
  NativeNotification := TNSUserNotification.Create;
  if not ANotification.Name.IsEmpty then
  begin
    // Set unique identificator
    UserInfo := TNSDictionary.Wrap(TNSDictionary.OCClass.dictionaryWithObject(
      (NSSTR(ANotification.Name) as ILocalObject).GetObjectID,
      (NSSTR('id') as ILocalObject).GetObjectID));
    NativeNotification.setUserInfo(UserInfo);
  end;
  // Get GMT time and set notification fired date
  GMTDateTime := GetGMTDateTime(ANotification.FireDate);
  NativeNotification.setDeliveryTimeZone(TNSTimeZone.Wrap(TNSTimeZone.OCClass.defaultTimeZone));
  NativeNotification.setDeliveryDate(DateTimeToNSDate(GMTDateTime));
  NativeNotification.setInformativeText(NSSTR(ANotification.AlertBody));
  NativeNotification.setHasActionButton(ANotification.HasAction);
  if ANotification.HasAction then
    NativeNotification.setActionButtonTitle(NSSTR(ANotification.AlertAction));
  if ANotification.EnableSound then
    NativeNotification.setSoundName(NSUserNotificationDefaultSoundName)
  else
    NativeNotification.setSoundName(nil);
  Result := NativeNotification;
end;

function TNotificationCenterCocoa.ConverNativeToDelphiNotification(
  const ANotification: NSUserNotification): TNotification;
var
  UserInfo: NSDictionary;
  NotificationTmp: TNotification;
begin
  NotificationTmp := TNotification.Create;
  UserInfo := ANotification.userInfo;
  if Assigned(UserInfo) then
    NotificationTmp.Name := UTF8ToString(TNSString.Wrap(UserInfo.valueForKey(NSSTR('id'))).UTF8String);
  if Assigned(ANotification.informativeText) then
    NotificationTmp.AlertBody := UTF8ToString(ANotification.informativeText.UTF8String);
  if Assigned(ANotification.actionButtonTitle) then
    NotificationTmp.AlertAction := UTF8ToString(ANotification.actionButtonTitle.UTF8String);;
  NotificationTmp.FireDate := NSDateToDateTime(ANotification.deliveryDate);
  NotificationTmp.EnableSound := Assigned(ANotification.SoundName);
  NotificationTmp.HasAction := ANotification.hasActionButton;
  Result := NotificationTmp;
end;

destructor TNotificationCenterCocoa.Destroy;
begin
  FNotificationCenter.release;
  inherited Destroy;
end;

function TNotificationCenterCocoa.FindNativeNotification(
  const AID: string; ANotification: NSUserNotification): Boolean;
var
  Notifications: NSArray;
  NativeNotification: NSUserNotification;
  Found: Boolean;
  I: NSUInteger;
  UserInfo: NSDictionary;
begin
  Notifications := FNotificationCenter.scheduledNotifications;
  Found := False;
  I := 0;
  while (I < Notifications.count) and not Found do
  begin
    NativeNotification := TNSUserNotification.Wrap(Notifications.objectAtIndex(I));
    UserInfo := NativeNotification.userInfo;
    if Assigned(UserInfo) and (UTF8ToString(TNSString.Wrap(UserInfo.valueForKey(NSSTR('id'))).UTF8String) = AID) then
      Found := True
    else
      Inc(I);
  end;
  if Found then
    ANotification := NativeNotification
  else
    ANotification := nil;
  Result := Found;
end;

function TNotificationCenterCocoa.FindNotification(const AName: string): TNotification;
var
  NativeNotification: NSUserNotification;
begin
  if FindNativeNotification(AName, NativeNotification) then
    Result := ConverNativeToDelphiNotification(NativeNotification)
  else
    Result := nil;
end;

function TNotificationCenterCocoa.GetCurrentNotifications: TNotifications;
var
  Notifications: NSArray;
  NativeNotification: NSUserNotification;
  I: Integer;
begin
  Notifications := FNotificationCenter.scheduledNotifications;
  SetLength(Result, Notifications.count);
  for I := 0 to Integer(Notifications.count) - 1 do
  begin
    NativeNotification := TNSUserNotification.Wrap(Notifications.objectAtIndex(I));
    Result[I] := ConverNativeToDelphiNotification(NativeNotification);
  end;
end;

procedure TNotificationCenterCocoa.PresentNotification(
  const ANotification: TNotification);
var
  NativeNotification: NSUserNotification;
begin
  CancelNotification(ANotification);
  NativeNotification := CreateNativeNotification(ANotification);
  FNotificationCenter.deliverNotification(NativeNotification);
end;

procedure TNotificationCenterCocoa.ScheduleNotification(const ANotification: TNotification);
var
  NativeNotification: NSUserNotification;
begin
  CancelNotification(ANotification);
  NativeNotification := CreateNativeNotification(ANotification);
  FNotificationCenter.scheduleNotification(NativeNotification);
end;

procedure TNotificationCenterCocoa.ReceiveNotification(const ANotification: NSUserNotification);
begin
  TMessageManager.DefaultManager.SendMessage(Self, TMessage<TNotification>.Create(ConverNativeToDelphiNotification(ANotification)));
end;

procedure TNotificationCenterCocoa.SetIconBadgeNumber(const ACount: Integer);
begin
  // Nop supported;
end;

function TNotificationCenterCocoa.GetIconBadgeNumber: Integer;
begin
  Result := 0;
  // Not supported;
end;

procedure TNotificationCenterCocoa.ResetIconBadgeNumber;
begin
  // Not supported;
end;

{ TNotificationCenterDelegate }

constructor TNotificationCenterDelegate.Create(ANotificationCenter: TNotificationCenterCocoa);
begin
  FNotificationCenter := ANotificationCenter;
end;

procedure TNotificationCenterDelegate.userNotificationCenter(center: NSUserNotificationCenter; didActivateNotification: NSUserNotification);
begin
  if Assigned(FNotificationCenter) then
    FNotificationCenter.ReceiveNotification(didActivateNotification);
end;

end.
