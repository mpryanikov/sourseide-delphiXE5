{*******************************************************}
{                                                       }
{         Delphi FireMonkey Notification Service        }
{                                                       }
{           Helpers for MacOS implementations           }
{                                                       }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit FMX.MediaLibrary.Android;

interface

procedure RegisterMediaLibraryServices;

implementation

uses
  System.Classes, System.SysUtils, System.IOUtils, System.Types, System.Math,
  FMX.MediaLibrary, FMX.Platform, FMX.Platform.Android, FMX.Controls, FMX.Helpers.Android, FMX.Graphics, FMX.Consts,
  FMX.Surfaces, FMX.Messages, FMX.Forms, FMX.Types3D,
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.App, Androidapi.JNI.JavaTypes, Androidapi.JNI.Net,
  Androidapi.IOUtils, Androidapi.JNIBridge, Androidapi.JNI, Androidapi.Bitmap, Androidapi.JNI.Os,
  Androidapi.JNI.Embarcadero;

type

  TImageManagerAndroid = class sealed (TInterfacedObject, IFMXCameraService, IFMXTakenImageService)
  strict private
    { Message Subscriptions }
    FSubscriptionReceivedBitmapID: Integer;
    FSubscriptionCancelReceivedBitmapID: Integer;
    { Events }
    FOnDidFinishTaking: TOnDidFinishTaking;
    FOnDidCancelTaking: TOnDidCancelTaking;
    { FMX Messages System }
    procedure DidReceiveBitmap(const Sender: TObject; const M: TMessage);
    procedure DidCancelReceiveBitmap(const Sender: TObject; const M: TMessage);
    { The returned size is a frame in which the size of the resultant photo will be adjusted }
    function ApproximateAdmissibleImageSize(const ASize: TSize): TSize;
  public
    constructor Create;
    destructor Destroy; override;

    { IFMXCameraService }
    procedure TakePhoto(const AControl: TControl; const ARequiredResolution: TSize; const AOnDidDinishTaking: TOnDidFinishTaking;
      const AOnDidCancelTaking: TOnDidCancelTaking);

    { IFMXTakenImageService }
    procedure TakeImageFromLibrary(const AControl: TControl; const ARequiredResolution: TSize; const AOnDidFinishTaking: TOnDidFinishTaking;
      const AOnDidCancelTaking: TOnDidCancelTaking);
  end;

  TSharingManagerAndroid = class sealed (TInterfacedObject, IFMXShareSheetActionsService)
  strict private type
    TSharingDataType = (sdtImage, sdtText);
    TSharingDataTypes = set of TSharingDataType;
  strict private
    function GetSharingMIMETypes(const DataTypes: TSharingDataTypes): string;
    function GetChooserCaption(const DataTypes: TSharingDataTypes): string;

    { IFMXShareSheetActionsService }
    procedure Share(const AControl: TControl; const AText: string; const AImage: TBitmap);
  end;

var
  ImageManager: TImageManagerAndroid;
  SharingManager: TSharingManagerAndroid;

procedure RegisterMediaLibraryServices;
begin
  ImageManager := TImageManagerAndroid.Create;
  SharingManager := TSharingManagerAndroid.Create;
  if HasSystemService(TJPackageManager.JavaClass.FEATURE_CAMERA) or (HasSystemService(TJPackageManager.JavaClass.FEATURE_CAMERA_FRONT) ) then
    TPlatformServices.Current.AddPlatformService(IFMXCameraService, IInterface(ImageManager));
  TPlatformServices.Current.AddPlatformService(IFMXTakenImageService, IInterface(ImageManager));
  TPlatformServices.Current.AddPlatformService(IFMXShareSheetActionsService, IInterface(SharingManager));
end;

{ TImageManagerAndroid }

constructor TImageManagerAndroid.Create;
begin
  { Subscription }
  FSubscriptionReceivedBitmapID := TMessageManager.DefaultManager.SubscribeToMessage(TMessageReceivedImage, DidReceiveBitmap);
  FSubscriptionCancelReceivedBitmapID := TMessageManager.DefaultManager.SubscribeToMessage(TMessageCancelReceivingImage, DidCancelReceiveBitmap);
end;

destructor TImageManagerAndroid.Destroy;
begin
  { Unsibscribe }
  TMessageManager.DefaultManager.Unsubscribe(TMessageReceivedImage, FSubscriptionReceivedBitmapID);
  TMessageManager.DefaultManager.Unsubscribe(TMessageCancelReceivingImage, FSubscriptionCancelReceivedBitmapID);
  inherited Destroy;
end;

procedure TImageManagerAndroid.DidCancelReceiveBitmap(const Sender: TObject; const M: TMessage);
begin
  if Assigned(FOnDidCancelTaking) then
    FOnDidCancelTaking;
end;

procedure TImageManagerAndroid.DidReceiveBitmap(const Sender: TObject; const M: TMessage);
var
  BitmapSurface: TBitmapSurface;
  NativeBitmap: JBitmap;
  RequestCode: Integer;
  Photo: TBitmap;
begin
  if M is TMessageReceivedImage then
  begin
    NativeBitmap := (M as TMessageReceivedImage).Value;
    RequestCode := (M as TMessageReceivedImage).RequestCode;
    if Assigned(NativeBitmap) then
    begin
      BitmapSurface := TBitmapSurface.Create;
      try
        BitmapSurface.SetSize(NativeBitmap.getWidth, NativeBitmap.getHeight);
        JBitmapToSurface(NativeBitmap, BitmapSurface);
        Photo := TBitmap.Create(0, 0);
        try
          Photo.Assign(BitmapSurface);

          if Assigned(FOnDidFinishTaking) then
            FOnDidFinishTaking(Photo)
          else
          begin
            if RequestCode = TJFMXMediaLibrary.JavaClass.ACTION_TAKE_IMAGE_FROM_CAMERA then
              TMessageManager.DefaultManager.SendMessage(Self, TMessageDidFinishTakingImageFromCamera.Create(Photo));
            if RequestCode = TJFMXMediaLibrary.JavaClass.ACTION_TAKE_IMAGE_FROM_LIBRARY then
              TMessageManager.DefaultManager.SendMessage(Self, TMessageDidFinishTakingImageFromLibrary.Create(Photo));
          end;
        finally
          Photo.Free;
        end;
      finally
        BitmapSurface.Free;
      end;
    end;
  end;
end;

function TImageManagerAndroid.ApproximateAdmissibleImageSize(const ASize: TSize): TSize;
begin
  Result.cx := Min(TContext3D.MaxTextureSize, ASize.Width);
  Result.cy := Min(TContext3D.MaxTextureSize, ASize.Height);
end;

procedure TImageManagerAndroid.TakeImageFromLibrary(const AControl: TControl;
  const ARequiredResolution: TSize; const AOnDidFinishTaking: TOnDidFinishTaking;
  const AOnDidCancelTaking: TOnDidCancelTaking);
var
  ApproximatedResolution: TSize;
begin
  FOnDidFinishTaking := AOnDidFinishTaking;
  FOnDidCancelTaking := AOnDidCancelTaking;
  ApproximatedResolution := ApproximateAdmissibleImageSize(ARequiredResolution);
  MainActivity.getFMXMediaLibrary.takeImageFromLibrary(ApproximatedResolution.Width, ApproximatedResolution.Height);
end;

procedure TImageManagerAndroid.TakePhoto(const AControl: TControl; const ARequiredResolution: TSize;
  const AOnDidDinishTaking: TOnDidFinishTaking; const AOnDidCancelTaking: TOnDidCancelTaking);
var
  ApproximatedResolution: TSize;
begin
  FOnDidFinishTaking := AOnDidDinishTaking;
  FOnDidCancelTaking := AOnDidCancelTaking;
  ApproximatedResolution := ApproximateAdmissibleImageSize(ARequiredResolution);
  MainActivity.getFMXMediaLibrary.takeImageFromCamera(ApproximatedResolution.Width, ApproximatedResolution.Height);
end;

{ TSharingAndroid }

function TSharingManagerAndroid.GetChooserCaption(const DataTypes: TSharingDataTypes): string;
begin
  if (TSharingDataType.sdtImage in DataTypes) and
     (TSharingDataType.sdtText in DataTypes) then
    Result := SMediaLibraryOpenTextAndImageWith
  else
    if TSharingDataType.sdtText in DataTypes then
      Result := SMediaLibraryOpenTextWith
    else
      if TSharingDataType.sdtImage in DataTypes then
        Result := SMediaLibraryOpenImageWith;
end;

function TSharingManagerAndroid.GetSharingMIMETypes(const DataTypes: TSharingDataTypes): string;
begin
  if TSharingDataType.sdtImage in DataTypes then
    Result := 'image/jpeg;';
  if TSharingDataType.sdtText in DataTypes then
    Result := Result + 'text/plain;';
end;

procedure TSharingManagerAndroid.Share(const AControl: TControl; const AText: string; const AImage: TBitmap);

  function DefineDataTypes: TSharingDataTypes;
  var
    DataTypes: TSharingDataTypes;
  begin
    DataTypes := [];
    if AText.Length > 0 then
      Include(DataTypes, TSharingDataType.sdtText);
    if Assigned(AImage) and not AImage.IsEmpty then
      Include(DataTypes, TSharingDataType.sdtImage);
    Result := DataTypes;
  end;

  procedure SetImage(AIntent: JIntent);
  var
    ImageFile: JFile;
    ImageUri: Jnet_Uri;
    FileNameTemp: JString;
  begin
    FileNameTemp := StringToJString('attachment.jpg');
    SharedActivity.openFileOutput(FileNameTemp, TJActivity.JavaClass.MODE_WORLD_READABLE);
    ImageFile := SharedActivity.getFileStreamPath(FileNameTemp);
    ImageUri := TJnet_Uri.JavaClass.fromFile(ImageFile);
    AImage.SaveToFile(JStringToString(ImageFile.getAbsolutePath));
    AIntent.putExtra(TJIntent.JavaClass.EXTRA_STREAM, TJParcelable.Wrap((ImageUri as ILocalObject).GetObjectID));
  end;

var
  Intent: JIntent;
  IntentChooser: JIntent;
  MIMETypes: string;
  ChooserCaption: string;
  DataTypes: TSharingDataTypes;
begin
  Intent := TJIntent.Create;
  DataTypes := DefineDataTypes;
  if DataTypes <> [] then
  begin
    MIMETypes := GetSharingMIMETypes(DataTypes);
    ChooserCaption := GetChooserCaption(DataTypes);
    { Create chooser of activity }
    Intent.setAction(TJIntent.JavaClass.ACTION_SEND);
    Intent.setType(StringToJString(MIMETypes));
    Intent.addFlags(TJIntent.JavaClass.FLAG_GRANT_READ_URI_PERMISSION);
    if TSharingDataType.sdtText in DataTypes then
      Intent.putExtra(TJIntent.JavaClass.EXTRA_TEXT, StringToJString(AText));
    if TSharingDataType.sdtImage in DataTypes then
      SetImage(Intent);
    Intent.setFlags(TJIntent.JavaClass.FLAG_ACTIVITY_CLEAR_WHEN_TASK_RESET);
    IntentChooser := TJIntent.JavaClass.createChooser(Intent, StrToJCharSequence(ChooserCaption));
    SharedActivity.startActivity(IntentChooser);
  end;
end;

end.