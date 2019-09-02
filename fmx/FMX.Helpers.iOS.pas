{*******************************************************}
{                                                       }
{         Delphi FireMonkey Notification Service        }
{                                                       }
{            Helpers for iOS implementations            }
{                                                       }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit FMX.Helpers.iOS;

interface

uses
  FMX.Types, FMX.Graphics,
  iOSapi.UIKit, iOSapi.Foundation, System.Types;

{ Date time functions }

function DateTimeToNSDate(const ADateTime: TDateTime): NSDate;
function NSDateToDateTime(const ADateTime: NSDate): TDateTime;
function GetGMTDateTime(const ADateTime: TDateTime): TDateTime;
function GetTimeZone: Integer;

{ Singleton instance }

function SharedApplication: UIApplication;
function MainScreen: UIScreen;
function DefaultNotificationCenter: NSNotificationCenter;

{ String convertions }

function StrToNSUrl(const AStr: string): NSUrl;
function NSStrToStr(const ASource: NSString): string;

{ Bitmap convertions }

function BitampToUIImage(const Bitmap: TBitmap): UIImage;
function UIImageToBitmap(const AImage: UIImage; ARotate: Single; AMaxSize: TSize): TBitmap;

{ Device functions }

function IsPhone: Boolean;
function IsPad: Boolean;

implementation

uses
  System.SysUtils, System.Math,
  FMX.Types3D, 
  iOSapi.CocoaTypes, iOSapi.CoreGraphics;

function GetTimeZone: Integer;
begin
  Result := TNSTimeZone.Wrap(TNSTimeZone.OCClass.localTimeZone).secondsFromGMT div SecsPerHour;
end;

function DateTimeToNSDate(const ADateTime: TDateTime): NSDate;
var
  IntervalInterval: NSTimeInterval;
begin
  IntervalInterval := (ADateTime - EncodeDate(2001, 1, 1)) * SecsPerDay;
  Result := TNSDate.Wrap(TNSDate.OCClass.dateWithTimeIntervalSinceReferenceDate(IntervalInterval));
end;

function NSDateToDateTime(const ADateTime: NSDate): TDateTime;
begin
  if Assigned(ADateTime) then
    Result := (ADateTime.TimeIntervalSince1970 + GetTimeZone) / SecsPerDay + EncodeDate(1970, 1, 1)
  else
    Result := 0.0;
end;

function GetGMTDateTime(const ADateTime: TDateTime): TDateTime;
begin
  if GetTimeZone > 0 then
    Result := ADateTime - EncodeTime(GetTimeZone, 0, 0, 0)
  else
    Result := ADateTime + EncodeTime(Abs(GetTimeZone), 0, 0, 0);
end;

function SharedApplication: UIApplication;
begin
  Result := TUIApplication.Wrap(TUIApplication.OCClass.sharedApplication);
end;

function MainScreen: UIScreen;
begin
  Result := TUIScreen.Wrap(TUIScreen.OCClass.mainScreen);
end;

function DefaultNotificationCenter: NSNotificationCenter;
begin
  Result := TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter);
end;

function StrToNSUrl(const AStr: string): NSUrl;
begin
  Result := TNSUrl.Wrap(TNSUrl.OCClass.URLWithString(NSSTR(AStr)));
end;

function NSStrToStr(const ASource: NSString): string;
begin
  if assigned(ASource) then
    Result := UTF8ToString(ASource.UTF8String)
  else
    Result := '';
end;

function IsPhone: Boolean;
begin
  Result := UTF8ToString(TUIDevice.Wrap(TUIDevice.OCClass.currentDevice).model.UTF8String) = 'iPhone';
end;

function IsPad: Boolean;
begin
  Result := TUIDevice.Wrap(TUIDevice.OCClass.currentDevice).userInterfaceIdiom = UIUserInterfaceIdiomPad;
end;

function UIImageToBitmap(const AImage: UIImage; ARotate: Single; AMaxSize: TSize): TBitmap;

  function ReduceImageSize(const AOriginalSize: TSize): TSize;
  var
    ImageRatio: Single;
    ScaleCoef: Single;
    MinWidth: Integer;
    MinHeight: Integer;
  begin
    Result := AOriginalSize;
    MinWidth := Min(AOriginalSize.Width, AMaxSize.Width);
    MinHeight := Min(AOriginalSize.Height, AMaxSize.Height);
    ImageRatio := AOriginalSize.Width / AOriginalSize.Height;
    if MinWidth / MinHeight < ImageRatio then
      Result := TSize.Create(Round(MinWidth * ImageRatio), MinWidth)
    else
      Result := TSize.Create(Round(MinHeight * ImageRatio), MinHeight);

    if Max(AOriginalSize.cx, AOriginalSize.cy) div TContext3D.MaxTextureSize > 0 then
    begin
      ScaleCoef := Max(AOriginalSize.cx, AOriginalSize.cy) / TContext3D.MaxTextureSize;
      Result := TSize.Create(Round(AOriginalSize.cx / ScaleCoef), Round(AOriginalSize.cy / ScaleCoef));
    end;
  end;

var
  ImageRef: CGImageRef;
  Bitmap: TBitmap;
  CtxRef: CGContextRef;
  ColorSpace: CGColorSpaceRef;
  Data: TBitmapData;
  BitmapSize: TSize;
begin
  ImageRef := AImage.CGImage;
  if Assigned(ImageRef) then
  begin
    // We should reduce image size. Because On different devices bitmap can has different available size
    BitmapSize := ReduceImageSize(TSize.Create(CGImageGetWidth(ImageRef), CGImageGetHeight(ImageRef)));
    Bitmap := TBitmap.Create(BitmapSize.cx, BitmapSize.cy);
    ColorSpace := CGColorSpaceCreateDeviceRGB;
    try
      if Bitmap.Map(TMapAccess.maWrite, Data) then
      begin
        CtxRef := CGBitmapContextCreate(Data.Data, Bitmap.Width, Bitmap.Height, 8, 4 * Bitmap.Width, ColorSpace, kCGImageAlphaPremultipliedLast or kCGBitmapByteOrder32Big );
        try
          CGContextDrawImage(CtxRef, CGRectMake(0, 0, Bitmap.Width, BitMap.Height), ImageRef);
        finally
          CGContextRelease(CtxRef);
        end;
      end;
    finally
      CGColorSpaceRelease(ColorSpace);
    end;
    Bitmap.Rotate(ARotate);
    Result := Bitmap;
  end
  else
    Result := nil;
end;

function BitampToUIImage(const Bitmap: TBitmap): UIImage;
var
  ImageRef: CGImageRef;
  CtxRef: CGContextRef;
  ColorSpace: CGColorSpaceRef;
  BitmapData: TBitmapData;
begin
  if Bitmap.IsEmpty then
    Result := TUIImage.Create
  else
  begin
    ColorSpace := CGColorSpaceCreateDeviceRGB;
    try
      if Bitmap.Map(TMapAccess.maRead, BitmapData) then
      begin
        CtxRef := CGBitmapContextCreate(BitmapData.Data, Bitmap.Width, Bitmap.Height, 8, 4 * Bitmap.Width, ColorSpace, kCGImageAlphaPremultipliedLast or kCGBitmapByteOrder32Big );
        try
          ImageRef := CGBitmapContextCreateImage(CtxRef);
          try
            Result := TUIImage.Alloc;
            Result.initWithCGImage(ImageRef);
          finally
            CGImageRelease(ImageRef);
          end;
        finally
          CGContextRelease(CtxRef);
        end;
      end;
    finally
      CGColorSpaceRelease(ColorSpace);
    end;
  end;
end;

end.
