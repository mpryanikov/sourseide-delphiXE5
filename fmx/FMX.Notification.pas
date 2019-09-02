{*******************************************************}
{                                                       }
{         Delphi FireMonkey Notification Service        }
{                                                       }
{   Description of interface for notificatione center   }
{                                                       }
{   Local notifications are ways for an application     }
{   that isn’t running in the foreground to let its     }
{   users know it has information for them.             }
{   The information could be a message, an impending    }
{   calendar event. When presented by the operating     }
{   system, local notifications look and sound          }
{   the same. They can display an alert message or      }
{   they can badge the application icon. They can       }
{   also play a sound when the alert or badge number    }
{   is shown.                                           }
{                                                       }
{ Copyright(c) 2012-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit FMX.Notification;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, FMX.Messages;

type

{ TNotification }

  { Discription of notification for Notification Center }
  TNotification = class (TPersistent)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    { Unique identificator for determenation notification in Notification list }
    Name: string;
    AlertBody: string;
    AlertAction: string;
    Number: Integer;
    FireDate: TDateTime;
    EnableSound: Boolean;
    HasAction: Boolean;
    constructor Create;
  end;

{ IFMXNotificationCenter }

  TNotifications = array of TNotification;

  TOnReceiveLocalNotification = procedure (Sender: TObject; ANotification: TNotification) of object;

  { Interface for work with notification center in Apple's platforms }
  IFMXNotificationCenter = interface
    ['{5C3C0232-26EF-45D9-A351-336ECE49EABA}']
    // Schedules a local notification for delivery at its
    // encapsulated date and time.
    procedure ScheduleNotification(const ANotification: TNotification);
    // Presents a local notification immediately.
    procedure PresentNotification(const ANotification: TNotification);
    // Cancels the delivery of the specified scheduled local notification.
    // |AName| - Unique identificator of notification
    procedure CancelNotification(const AName: string); overload;
    // Cancels the delivery of the specified scheduled local notification.
    procedure CancelNotification(const ANotification: TNotification); overload;
    // Cancels the delivery of all scheduled local notifications.
    procedure CancelAllNotifications;
    // The number currently set as the badge of the application icon.
    procedure SetIconBadgeNumber(const ACount: Integer);
    // Getting The number currently set as the badge of the application icon.
    function GetIconBadgeNumber: Integer;
    // Reset the number of the application icon.
    procedure ResetIconBadgeNumber;
  end;

{ TNotificationCenter }

  TCustomNotificationCenter = class (TComponent)
  strict private
    FPlatformNotificationCenter: IFMXNotificationCenter;
    FApplicationIcon: Word;
    FSubscriptionID: Integer;
    FOnReceiveLocalNotification: TOnReceiveLocalNotification;
    function GetApplicationIconNumber: Word;
    procedure SetApplicationIconNumber(const Value: Word);
  protected
    procedure DoReceiveLocalNotification(const Sender: TObject; const M: TMessage); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Supported: Boolean;
    { Building }
    function CreateNotification: TNotification; overload;
    function CreateNotification(const AName, AAlertBody: string; const AFireDate: TDateTime): TNotification; overload;
    { Presentation }
    procedure PresentNotification(const ANotification: TNotification);
    procedure ScheduleNotification(const ANotification: TNotification);
    procedure CancelAll;
    procedure CancelNotification(const AName: string);
  public
    property ApplicationIconBadgeNumber: Word read GetApplicationIconNumber write SetApplicationIconNumber default 0;
    property OnReceiveLocalNotification: TOnReceiveLocalNotification read FOnReceiveLocalNotification
      write FOnReceiveLocalNotification;
  end;

  TNotificationCenter = class (TCustomNotificationCenter)
  published
    property ApplicationIconBadgeNumber;
    property OnReceiveLocalNotification;
  end;

implementation

uses
{$IFDEF IOS}
  FMX.Notification.iOS,
{$ELSE}
  {$IFDEF MACOS}
  FMX.Notification.Mac,
  {$ELSE}
    {$IFDEF ANDROID}
       FMX.Notification.Android,
    {$ENDIF ANDROID}
  {$ENDIF MACOS}
{$ENDIF IOS}
  FMX.Types, FMX.Platform;

{ TNotificationCenter }

procedure TCustomNotificationCenter.CancelAll;
begin
  if Supported then
    FPlatformNotificationCenter.CancelAllNotifications;
end;

procedure TCustomNotificationCenter.CancelNotification(const AName: string);
begin
  if Supported then
    FPlatformNotificationCenter.CancelNotification(AName);
end;

constructor TCustomNotificationCenter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TPlatformServices.Current.SupportsPlatformService(IFMXNotificationCenter, IInterface(FPlatformNotificationCenter));
  FSubscriptionID := TMessageManager.DefaultManager.SubscribeToMessage(TMessage<TNotification>, DoReceiveLocalNotification);
  FApplicationIcon := 0;
end;

function TCustomNotificationCenter.CreateNotification: TNotification;
begin
  Result := TNotification.Create;
end;

function TCustomNotificationCenter.CreateNotification(const AName,
  AAlertBody: string; const AFireDate: TDateTime): TNotification;
begin
  Result := CreateNotification;
  Result.Name := AName;
  Result.AlertBody := AAlertBody;
  Result.FireDate := AFireDate;
end;

destructor TCustomNotificationCenter.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TMessage<TNotification>, FSubscriptionID);
  FPlatformNotificationCenter := nil;
  inherited Destroy;
end;

procedure TCustomNotificationCenter.DoReceiveLocalNotification(const Sender: TObject; const M: TMessage);
begin
  if (M is TMessage<TNotification>) and Assigned(FOnReceiveLocalNotification) then
    FOnReceiveLocalNotification(Self, (M as TMessage<TNotification>).Value);
end;

function TCustomNotificationCenter.GetApplicationIconNumber: Word;
begin
  if Supported then
    Result := FPlatformNotificationCenter.GetIconBadgeNumber
  else
    Result := FApplicationIcon;
end;

procedure TCustomNotificationCenter.PresentNotification(
  const ANotification: TNotification);
begin
  if Supported then
    FPlatformNotificationCenter.PresentNotification(ANotification);
end;

procedure TCustomNotificationCenter.ScheduleNotification(
  const ANotification: TNotification);
begin
  if Supported then
    FPlatformNotificationCenter.ScheduleNotification(ANotification);
end;

procedure TCustomNotificationCenter.SetApplicationIconNumber(
  const Value: Word);
begin
  if Supported then
    FPlatformNotificationCenter.SetIconBadgeNumber(Value)
  else
    FApplicationIcon := Value;
end;

function TCustomNotificationCenter.Supported: Boolean;
begin
  Result := Assigned(FPlatformNotificationCenter);
end;

{ TNotification }

procedure TNotification.AssignTo(Dest: TPersistent);
var
  DestNotification: TNotification;
begin
  if Dest is TNotification then
  begin
    DestNotification := Dest as TNotification;
    DestNotification.AlertBody := AlertBody;
    DestNotification.AlertAction := AlertAction;
    DestNotification.Number := Number;
    DestNotification.FireDate := FireDate;
    DestNotification.EnableSound := EnableSound;
    DestNotification.HasAction := HasAction;
  end
  else
    inherited AssignTo(Dest);
end;

constructor TNotification.Create;
begin
  EnableSound := True;
  HasAction := False;
  FireDate := Now;
  Number := 0;
end;

initialization
  RegisterFmxClasses([TCustomNotificationCenter, TNotificationCenter]);
{$IF Defined(IOS) OR Defined(MACOS) OR Defined(ANDROID)}
  RegisterNotificationService;
{$ENDIF}
end.