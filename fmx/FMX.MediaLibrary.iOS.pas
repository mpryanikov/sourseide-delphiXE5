{*******************************************************}
{                                                       }
{        Delphi FireMonkey media store for IOS          }
{                                                       }
{ Copyright(c) 2012-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit FMX.MediaLibrary.IOS;

interface

procedure RegisterMediaLibraryServices;

implementation

uses
  System.SysUtils, System.Classes, System.Types, System.Math,
  FMX.MediaLibrary, FMX.Types, FMX.Types3D, FMX.Controls, FMX.Platform, FMX.Graphics,
  FMX.Helpers.iOS,
  iOSapi.MediaPlayer, iOSapi.Foundation, iOSapi.UIKit, iOSapi.CoreGraphics,
  Macapi.ObjectiveC;

type
  TImageDelegate = class;

  TImageManagerCocoa = class (TInterfacedObject, IFMXTakenImageService, IFMXCameraService)
  strict private
    FRequiredResolution: TSize;
    FOnDidFinishTaking: TOnDidFinishTaking;
    FOnDidCancelTaking: TOnDidCancelTaking;
    FImageDelegate: TImageDelegate;
    FImagePicker: UIImagePickerController;
    // For iPad
    FPopoverController : UIPopoverController;
  protected
    procedure TakeImage(const AControl: TControl; const ASourceType: UIImagePickerControllerSourceType);
  public
    constructor Create;
    destructor Destroy; override;
    class function IsAvailableSourceType(const ASourceType: UIImagePickerControllerSourceType): Boolean;
    procedure ClosePopover;
    { IFMXTakenImage }
    procedure TakeImageFromLibrary(const AControl: TControl; const ARequiredResolution: TSize;
      const AOnDidDinishTaking: TOnDidFinishTaking; const AOnDidCancelTaking: TOnDidCancelTaking);
    { IFMXCameraService }
    procedure TakePhoto(const AControl: TControl; const ARequiredResolution: TSize; 
      const AOnDidDinishTaking: TOnDidFinishTaking; const AOnDidCancelTaking: TOnDidCancelTaking);
      
    property RequiredResolution: TSize read FRequiredResolution write FRequiredResolution;
  end;

  TImageDelegate = class (TOCLocal, UIImagePickerControllerDelegate)
  strict private
    [weak] FImageManager: TImageManagerCocoa;
    FOnDidFinishTaking: TOnDidFinishTaking;
    FOnDidCancelTaking: TOnDidCancelTaking;
  protected
    procedure DoDidFinishTaking(const AImage: TBitmap);
    procedure DoDidCancelTaking;
    procedure HidePicker(const APicker: UIImagePickerController);
    function GetAngleOfImageOrientation(const AImage: UIImage): Single;
  public
    property ImageManager: TImageManagerCocoa read FImageManager write FImageManager;
    { UIImagePickerControllerDelegate }
    procedure imagePickerController(picker: UIImagePickerController; didFinishPickingImage: UIImage; editingInfo: NSDictionary); overload; cdecl;
    procedure imagePickerController(picker: UIImagePickerController; didFinishPickingMediaWithInfo: NSDictionary); overload; cdecl;
    procedure imagePickerControllerDidCancel(picker: UIImagePickerController); cdecl;
    property OnDidFinishTaking: TOnDidFinishTaking read FOnDidFinishTaking write FOnDidFinishTaking;
    property OnDidCancelTaking: TOnDidCancelTaking read FOnDidCancelTaking write FOnDidCancelTaking;
  end;

  TShareService = class (TInterfacedObject, IFMXShareSheetActionsService)
  strict private
    FUIActivityViewController: UIActivityViewController;
    FActivityItems: NSMutableArray;
    // For iPad
    FPopoverController : UIPopoverController;
  public
    constructor Create;
    destructor Destroy; override;

    { IFMXShareSheetActionsService }
    procedure Share(const AControl: TControl; const AText: string; const ABitmap: TBitmap);
  end;

procedure RegisterMediaLibraryServices;
var
  ImageManager: TImageManagerCocoa;
begin
  ImageManager := TImageManagerCocoa.Create;
  if TImageManagerCocoa.IsAvailableSourceType(UIImagePickerControllerSourceTypeCamera) then
    TPlatformServices.Current.AddPlatformService(IFMXCameraService, IInterface(ImageManager));

  if TImageManagerCocoa.IsAvailableSourceType(UIImagePickerControllerSourceTypeSavedPhotosAlbum) then
    TPlatformServices.Current.AddPlatformService(IFMXTakenImageService, IInterface(ImageManager));

  if TOSVersion.Check(6, 0) then
    TPlatformServices.Current.AddPlatformService(IFMXShareSheetActionsService, IInterface(TShareService.Create));
end;

procedure TImageManagerCocoa.ClosePopover;
begin
  if Assigned(FPopoverController) then
    FPopoverController.dismissPopoverAnimated(True);
end;

constructor TImageManagerCocoa.Create;
begin
  inherited Create;
  FImageDelegate := TImageDelegate.Create;
  FImageDelegate.ImageManager := Self;
  FImagePicker := TUIImagePickerController.Create;
  FImagePicker.retain;
  FImagePicker.setDelegate(FImageDelegate.GetObjectID);
  FImagePicker.setAllowsEditing(True);
end;

destructor TImageManagerCocoa.Destroy;
begin
  FImagePicker.release;
  FImageDelegate.DisposeOf;
  if Assigned(FPopoverController) then
    FPopoverController.release;
  inherited Destroy;
end;

class function TImageManagerCocoa.IsAvailableSourceType(const ASourceType: UIImagePickerControllerSourceType): Boolean;
begin
  Result := TUIImagePickerController.OCClass.isSourceTypeAvailable(ASourceType);
end;

procedure TImageManagerCocoa.TakeImageFromLibrary(const AControl: TControl; const ARequiredResolution: TSize; const AOnDidDinishTaking: TOnDidFinishTaking; const AOnDidCancelTaking: TOnDidCancelTaking);
begin
  FRequiredResolution := ARequiredResolution;
  FOnDidFinishTaking := AOnDidDinishTaking;
  FOnDidCancelTaking := AOnDidCancelTaking;
  if IsAvailableSourceType(UIImagePickerControllerSourceTypeCamera) then
    TakeImage(AControl, UIImagePickerControllerSourceTypePhotoLibrary)
  else
    TakeImage(AControl, UIImagePickerControllerSourceTypeSavedPhotosAlbum);
end;

procedure TImageManagerCocoa.TakePhoto(const AControl: TControl; const ARequiredResolution: TSize; const AOnDidDinishTaking: TOnDidFinishTaking; const AOnDidCancelTaking: TOnDidCancelTaking);
begin
  FRequiredResolution := ARequiredResolution; 
  FOnDidFinishTaking := AOnDidDinishTaking;
  FOnDidCancelTaking := AOnDidCancelTaking;
  TakeImage(AControl, UIImagePickerControllerSourceTypeCamera);
end;

procedure TImageManagerCocoa.TakeImage(const AControl: TControl; const ASourceType: UIImagePickerControllerSourceType);

  procedure ShowInFullScreen;
  var
    Window: UIWindow;
  begin
    Window := SharedApplication.keyWindow;
    if Assigned(Window) and Assigned(Window.rootViewController) then
      Window.rootViewController.presentModalViewController(FImagePicker, True);
  end;

  procedure ShowInPopover;
  var
    Window: UIWindow;
    PopoverRect: CGRect;
    AbsolutePos: TPointF;
  begin
    Window := SharedApplication.keyWindow;
    AbsolutePos := AControl.LocalToAbsolute(PointF(0, 0));
    PopoverRect := CGRectMake(AbsolutePos.X, AbsolutePos.Y, AControl.Width, AControl.Height);
    if not Assigned(FPopoverController) then
      FPopoverController := TUIPopoverController.Alloc;
    FPopoverController.initWithContentViewController(FImagePicker);
    FPopoverController.presentPopoverFromRect(
      PopoverRect, Window.rootViewController.View, UIPopoverArrowDirectionUp, True);
  end;

  procedure ShowPicker;
  begin
    // Camera for iPad is showed only in full screen on iPad and iPhone
    if not IsPad or (ASourceType = UIImagePickerControllerSourceTypeCamera) then
      ShowInFullScreen
    else
      ShowInPopover;
  end;

begin
  if IsAvailableSourceType(ASourceType) then
  begin
    FImageDelegate.OnDidFinishTaking := FOnDidFinishTaking;
    FImageDelegate.OnDidCancelTaking := FOnDidCancelTaking;
    FImagePicker.setSourceType(ASourceType);
    ShowPicker;
  end;
end;

{ TImageDelegate }

procedure TImageDelegate.DoDidCancelTaking;
begin
  if Assigned(OnDidCancelTaking) then
    OnDidCancelTaking;
end;

procedure TImageDelegate.DoDidFinishTaking(const AImage: TBitmap);
begin
  if Assigned(OnDidFinishTaking) then
    OnDidFinishTaking(AImage);
end;

function TImageDelegate.GetAngleOfImageOrientation(const AImage: UIImage): Single;
begin
  case AImage.imageOrientation of
    UIImageOrientationDown,
    UIImageOrientationDownMirrored:
      Result := 180;
    UIImageOrientationLeft,
    UIImageOrientationLeftMirrored:
      Result := -90;
    UIImageOrientationRight,
    UIImageOrientationRightMirrored:
      Result := 90;
    UIImageOrientationUp,
    UIImageOrientationUpMirrored:
      Result := 0;
  else
    Result := 0;
  end;
end;

procedure TImageDelegate.HidePicker(const APicker: UIImagePickerController);
begin
  if Assigned(FImageManager) and IsPad and (APicker.sourceType <> UIImagePickerControllerSourceTypeCamera) then
    FImageManager.ClosePopover
  else
    APicker.dismissModalViewControllerAnimated(True);
end;

procedure TImageDelegate.imagePickerController(picker: UIImagePickerController; didFinishPickingImage: UIImage;
  editingInfo: NSDictionary);
var
  Bitmap: TBitmap;
  RotationAngle: Single;
begin
  HidePicker(picker);
  RotationAngle := GetAngleOfImageOrientation(didFinishPickingImage);
  Bitmap := UIImageToBitmap(didFinishPickingImage, RotationAngle, FImageManager.RequiredResolution);
  try
    DoDidFinishTaking(Bitmap);
  finally
    Bitmap.DisposeOf;
  end;
end;

procedure TImageDelegate.imagePickerController(picker: UIImagePickerController;
  didFinishPickingMediaWithInfo: NSDictionary);
var
  Bitmap: TBitmap;
  ImageTmp: UIImage;
  RotationAngle: Single;
begin
  HidePicker(picker);
  ImageTmp := TUIImage.Wrap(didFinishPickingMediaWithInfo.objectForKey((UIImagePickerControllerOriginalImage as ILocalObject).GetObjectID));

  // The camera does pictures in portrait orientation
  RotationAngle := GetAngleOfImageOrientation(ImageTmp);
  Bitmap := UIImageToBitmap(ImageTmp, RotationAngle, FImageManager.RequiredResolution);
  try
    DoDidFinishTaking(Bitmap);
  finally
    Bitmap.DisposeOf;
  end;
end;

procedure TImageDelegate.imagePickerControllerDidCancel(picker: UIImagePickerController);
begin
  DoDidCancelTaking;
  HidePicker(picker);
end;

{ TSharingService }

constructor TShareService.Create;
begin
  FUIActivityViewController := TUIActivityViewController.alloc;
  FActivityItems := TNSMutableArray.Create;
end;

destructor TShareService.Destroy;
begin
  FUIActivityViewController.release;
  FActivityItems.release;
  inherited Destroy;
end;

procedure TShareService.Share(const AControl: TControl; const AText: string; const ABitmap: TBitmap);

  procedure ShowForPhone;
  var
    Window: UIWindow;
  begin
    Window := SharedApplication.keyWindow;
    if Assigned(Window) and Assigned(Window.rootViewController) then
      Window.rootViewController.presentModalViewController(FUIActivityViewController, True);
  end;

  procedure ShowForPad;
  var
    Window: UIWindow;
    PopoverRect: CGRect;
    AbsolutePos: TPointF;
  begin
    Window := SharedApplication.keyWindow;
    if Assigned(AControl) then
    begin
      AbsolutePos := AControl.LocalToAbsolute(PointF(0, 0));
      PopoverRect := CGRectMake(AbsolutePos.X, AbsolutePos.Y, AControl.Width, AControl.Height);
    end
    else
      PopoverRect := CGRectMake(0, 0, 0, 0);
    if not Assigned(FPopoverController) then
      FPopoverController := TUIPopoverController.Alloc;
    FPopoverController.initWithContentViewController(FUIActivityViewController);
    FPopoverController.presentPopoverFromRect(
      PopoverRect, Window.rootViewController.View, UIPopoverArrowDirectionAny, True);
  end;

  procedure ShowActionsSheet;
  begin
    if IsPad then
      ShowForPad
    else
      ShowForPhone;
  end;

var
  OCImage: UIImage;
begin
  Assert(Assigned(ABitmap) or not AText.IsEmpty);
  FActivityItems.removeAllObjects;

  if not AText.IsEmpty then
    FActivityItems.addObject((NSSTR(AText) as ILocalObject).GetObjectID);

  if Assigned(ABitmap) and not ABitmap.IsEmpty then
  begin
    OCImage := BitampToUIImage(ABitmap);
    FActivityItems.addObject((OCImage as ILocalObject).GetObjectID);
  end;

  try
    if FActivityItems.count > 0 then
    begin
      FUIActivityViewController.initWithActivityItems(FActivityItems , nil);
      ShowActionsSheet;
    end;
  finally
    if Assigned(OCImage) then
      OCImage.release;
  end;
end;

end.
