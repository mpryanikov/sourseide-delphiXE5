{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit FMX.MediaLibrary.Actions;

interface

uses
  System.Classes, System.Actions,
  FMX.Types, FMX.MediaLibrary, FMX.ActnList, FMX.StdActns, FMX.Consts, FMX.Graphics,
  FMX.Messages;

type

  TCustomTakePhotoAction = class(TSysCommonAction)
  strict private
    FOnDidCancelTaking: TOnDidCancelTaking;
    FOnDidFinishTaking: TOnDidFinishTaking;
  protected
    FMaxHeight: Cardinal;
    FMaxWidth: Cardinal;
    procedure DoCancelTaking; virtual;
    procedure DoFinishedTakingImage(AImage: TBitmap); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function HandlesTarget(Target: TObject): Boolean; override;
    property MaxHeight: Cardinal read FMaxHeight write FMaxHeight default 1024;
    property MaxWidth: Cardinal read FMaxWidth write FMaxWidth default 1024;
    property OnDidCancelTaking: TOnDidCancelTaking read FOnDidCancelTaking write FOnDidCancelTaking;
    property OnDidFinishTaking: TOnDidFinishTaking read FOnDidFinishTaking write FOnDidFinishTaking;
  end;

{ TTakePhotoFromCameraAction }

  TTakePhotoFromCameraAction = class(TCustomTakePhotoAction)
  strict private
    FCameraService: IFMXCameraService;
    FSubscriptionReceivedBitmapID: Integer;
  protected
    procedure CustomTextChanged; override;
    function IsSupportedInterface: Boolean; override;
    procedure DidReceivedBitmap(const Sender: TObject; const M: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property MaxHeight;
    property MaxWidth;
    property OnDidCancelTaking;
    property OnDidFinishTaking;
  end;

{ TTakePhotoFromLibraryAction }

  TTakePhotoFromLibraryAction = class(TCustomTakePhotoAction)
  strict private
    FTakenImageService: IFMXTakenImageService;
    FSubscriptionReceivedBitmapID: Integer;
  protected
    procedure CustomTextChanged; override;
    function IsSupportedInterface: Boolean; override;
    procedure DidReceivedBitmap(const Sender: TObject; const M: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property MaxHeight;
    property MaxWidth;
    property OnDidCancelTaking;
    property OnDidFinishTaking;
  end;

{ TShowShareSheetAction }

  TShowShareSheetAction = class(TSysCommonAction)
  strict private
    FSharingService: IFMXShareSheetActionsService;
    FBitmap: TBitmap;
    FMessage: string;
    FOnBeforeExecute: TNotifyEvent;
  private
    procedure SetBitmap(const Value: TBitmap);
  protected
    procedure DoBeforeExecute;
    procedure CustomTextChanged; override;
    function IsSupportedInterface: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property TextMessage: string read FMessage write FMessage;
    property OnBeforeExecute: TNotifyEvent read FOnBeforeExecute write FOnBeforeExecute;
  end;

implementation

uses
  System.SysUtils, FMX.Platform, FMX.Controls, System.Types;

{ TTakePhotoAction }

constructor TCustomTakePhotoAction.Create(AOwner: TComponent);
begin
  inherited;
  HideIfUnsupportedInterface := True;
  FMaxHeight := 1024;
  FMaxWidth := 1024;
end;

procedure TCustomTakePhotoAction.DoCancelTaking;
begin
  if Assigned(OnDidCancelTaking) then
    OnDidCancelTaking;
end;

procedure TCustomTakePhotoAction.DoFinishedTakingImage(AImage: TBitmap);
begin
  if Assigned(OnDidFinishTaking) then
    OnDidFinishTaking(AImage);
end;

function TCustomTakePhotoAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := Supported;
end;

{ TTakePhotoAction }

constructor TTakePhotoFromCameraAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TPlatformServices.Current.SupportsPlatformService(IFMXCameraService, IInterface(FCameraService));
  { Subscription }
  FSubscriptionReceivedBitmapID := TMessageManager.DefaultManager.SubscribeToMessage(TMessageDidFinishTakingImageFromCamera,
    DidReceivedBitmap);
end;

procedure TTakePhotoFromCameraAction.CustomTextChanged;
begin
  Text := GetDefaultText(STakePhotoFromCamera);
end;

destructor TTakePhotoFromCameraAction.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TMessageDidFinishTakingImageFromCamera, FSubscriptionReceivedBitmapID);
  FCameraService := nil;
  inherited Destroy;
end;

procedure TTakePhotoFromCameraAction.DidReceivedBitmap(const Sender: TObject; const M: TMessage);
begin
  if (M is TMessageDidFinishTakingImageFromCamera) then
    DoFinishedTakingImage((M as TMessageDidFinishTakingImageFromCamera).Value);
end;

procedure TTakePhotoFromCameraAction.ExecuteTarget(Target: TObject);
var
  TargetControl: TControl;
begin
  if Target is TControl then
    TargetControl := Target as TControl
  else
    TargetControl := nil;
  if Assigned(FCameraService) then
    FCameraService.TakePhoto(TargetControl,
      TSize.Create(Longint(MaxWidth), Longint(MaxHeight)),
      DoFinishedTakingImage, DoCancelTaking);
end;

function TTakePhotoFromCameraAction.IsSupportedInterface: Boolean;
begin
  Result := Assigned(FCameraService);
end;

{ TTakePhotoFromLibraryAction }

constructor TTakePhotoFromLibraryAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TPlatformServices.Current.SupportsPlatformService(IFMXTakenImageService, IInterface(FTakenImageService));
  { Subscription }
  FSubscriptionReceivedBitmapID := TMessageManager.DefaultManager.SubscribeToMessage(TMessageDidFinishTakingImageFromLibrary,
    DidReceivedBitmap);
end;

procedure TTakePhotoFromLibraryAction.CustomTextChanged;
begin
  Text := GetDefaultText(STakePhotoFromLibarary);
end;

destructor TTakePhotoFromLibraryAction.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TMessageDidFinishTakingImageFromLibrary, FSubscriptionReceivedBitmapID);
  FTakenImageService := nil;
  inherited Destroy;
end;

procedure TTakePhotoFromLibraryAction.DidReceivedBitmap(const Sender: TObject; const M: TMessage);
begin
  if (M is TMessageDidFinishTakingImageFromLibrary) then
    DoFinishedTakingImage((M as TMessageDidFinishTakingImageFromLibrary).Value);
end;

procedure TTakePhotoFromLibraryAction.ExecuteTarget(Target: TObject);
var
  TargetControl: TControl;
begin
  if Target is TControl then
    TargetControl := Target as TControl
  else
    TargetControl := nil;
  if Assigned(FTakenImageService) then
    FTakenImageService.TakeImageFromLibrary(TargetControl,
      TSize.Create(Longint(MaxWidth), Longint(MaxHeight)),
      DoFinishedTakingImage, DoCancelTaking);
end;

function TTakePhotoFromLibraryAction.IsSupportedInterface: Boolean;
begin
  Result := Assigned(FTakenImageService);
end;

{ TShowShareSheetAction }

constructor TShowShareSheetAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMessage := '';
  FBitmap := TBitmap.Create(0, 0);
  TPlatformServices.Current.SupportsPlatformService(IFMXShareSheetActionsService, IInterface(FSharingService));
end;

procedure TShowShareSheetAction.CustomTextChanged;
begin
  Text := GetDefaultText(SOpenStandartServices);
end;

destructor TShowShareSheetAction.Destroy;
begin
  FSharingService := nil;
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

procedure TShowShareSheetAction.DoBeforeExecute;
begin
  if Assigned(FOnBeforeExecute) then
    FOnBeforeExecute(Self);
end;

procedure TShowShareSheetAction.ExecuteTarget(Target: TObject);
var
  TargetControl: TControl;
begin
  DoBeforeExecute;
  inherited ExecuteTarget(Target);
  if Target is TControl then
    TargetControl := Target as TControl
  else
    TargetControl := nil;
  if Assigned(Bitmap) and Assigned(FSharingService) then
    FSharingService.Share(TargetControl, TextMessage, Bitmap);
end;

function TShowShareSheetAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := Supported;
end;

function TShowShareSheetAction.IsSupportedInterface: Boolean;
begin
  Result := Assigned(FSharingService);
end;

procedure TShowShareSheetAction.SetBitmap(const Value: TBitmap);
begin
  FBitmap.Assign(Value);
end;

end.