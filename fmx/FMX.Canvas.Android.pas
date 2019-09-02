{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit FMX.Canvas.Android;

interface

implementation

uses
  System.Types, System.SysUtils, System.Classes, FMX.Consts, FMX.Surfaces, FMX.Graphics, FMX.Helpers.Android,
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText;

type
  TBitmapCodecAndroid = class(TCustomBitmapCodec)
  private const
    DefaultSaveQuality = 75;
  public
    class function GetImageSize(const AFileName: string): TPointF; override;
    class function IsValid(const AStream: TStream): Boolean; override;
    function LoadFromFile(const AFileName: string; const Bitmap: TBitmapSurface): Boolean; override;
    function LoadThumbnailFromFile(const AFileName: string; const AFitWidth, AFitHeight: Single;
      const UseEmbedded: Boolean; const Bitmap: TBitmapSurface): Boolean; override;
    function SaveToFile(const AFileName: string; const Bitmap: TBitmapSurface;
       const SaveParams: PBitmapCodecSaveParams): Boolean; override;
    function LoadFromStream(const AStream: TStream; const Bitmap: TBitmapSurface): Boolean; override;
    function SaveToStream(const AStream: TStream; const Bitmap: TBitmapSurface; const Extension: string;
       const SaveParams: PBitmapCodecSaveParams): Boolean; override;
  end;

{ TBitmapCodecAndroid }

class function TBitmapCodecAndroid.GetImageSize(const AFileName: string): TPointF;
var
  Options: JBitmapFactory_Options;
begin
  Options := TJBitmapFactory_Options.JavaClass.init;
  Options.inJustDecodeBounds := True;

  TJBitmapFactory.JavaClass.decodeFile(StringToJString(AFileName), Options);

  Result := TPointF.Create(Options.outWidth, Options.outHeight);
end;

class function TBitmapCodecAndroid.IsValid(const AStream: TStream): Boolean;
var
  MemStream: TMemoryStream;
  SavePosition: Int64;
  BitmapArray: TJavaArray<Byte>;
  Options: JBitmapFactory_Options;
begin
  try
    SavePosition := AStream.Position;
    try
      MemStream := TMemoryStream.Create;
      try
        MemStream.CopyFrom(AStream, AStream.Size);

        BitmapArray := TJavaArray<Byte>.Create(MemStream.Size);
        Move(MemStream.Memory^, BitmapArray.Data^, MemStream.Size);

        Options := TJBitmapFactory_Options.JavaClass.init;
        Options.inJustDecodeBounds := True;

        TJBitmapFactory.JavaClass.decodeByteArray(BitmapArray, 0, MemStream.Size, Options);

        Result := (Options.outWidth > 0) and (Options.outHeight > 0);
      finally
        MemStream.Free;
      end;
    finally
      AStream.Position := SavePosition;
    end;
  except
    Exit(False);
  end;
end;

function TBitmapCodecAndroid.LoadFromStream(const AStream: TStream; const Bitmap: TBitmapSurface): Boolean;
var
  TempStream: TMemoryStream;
  TempArray: TJavaArray<Byte>;
  NativeBitmap: JBitmap;
  LoadOptions: JBitmapFactory_Options;
  SavePosition: Int64;
begin
  Result := False;

  SavePosition := AStream.Position;
  try
    TempStream := TMemoryStream.Create;
    try
      TempStream.CopyFrom(AStream, AStream.Size);

      TempArray := TJavaArray<Byte>.Create(TempStream.Size);
      Move(TempStream.Memory^, TempArray.Data^, TempStream.Size);

      LoadOptions := TJBitmapFactory_Options.JavaClass.init;
      NativeBitmap := TJBitmapFactory.JavaClass.decodeByteArray(TempArray, 0, TempStream.Size, LoadOptions);

      if (LoadOptions.outWidth < 1) or (LoadOptions.outHeight < 1) then
        Exit;

      Result := JBitmapToSurface(NativeBitmap, Bitmap);
    finally
      TempStream.Free;
    end;
  finally
    AStream.Position := SavePosition;
  end;
end;

function TBitmapCodecAndroid.SaveToStream(const AStream: TStream; const Bitmap: TBitmapSurface;
  const Extension: string; const SaveParams: PBitmapCodecSaveParams): Boolean;
var
  NativeBitmap: JBitmap;
  SaveFormat: JBitmap_CompressFormat;
  SaveQuality: Integer;
  OutByteStream: JByteArrayOutputStream;
  ContentBytes: TJavaArray<Byte>;
begin
  NativeBitmap := TJBitmap.JavaClass.createBitmap(Bitmap.Width, Bitmap.Height, TJBitmap_Config.JavaClass.ARGB_8888);

  Result := SurfaceToJBitmap(Bitmap, NativeBitmap);
  if not Result then
    Exit;

  if SameText(Extension, '.PNG') then
    SaveFormat := TJBitmap_CompressFormat.JavaClass.PNG
  else
    SaveFormat := TJBitmap_CompressFormat.JavaClass.JPEG;

  OutByteStream := TJByteArrayOutputStream.JavaClass.init(0);

  SaveQuality := DefaultSaveQuality;

  if Assigned(SaveParams) then
    SaveQuality := SaveParams.Quality;

  Result := NativeBitmap.compress(SaveFormat, SaveQuality, OutByteStream);

  if Result and (OutByteStream.size > 0) then
  begin
    ContentBytes := OutByteStream.toByteArray;
    AStream.WriteBuffer(ContentBytes.Data^, OutByteStream.size);
  end;

  Result := Result and (OutByteStream.size > 0);
end;

function TBitmapCodecAndroid.LoadFromFile(const AFileName: string; const Bitmap: TBitmapSurface): Boolean;
var
  NativeBitmap: JBitmap;
  LoadOptions: JBitmapFactory_Options;
begin
  Result := False;

  LoadOptions := TJBitmapFactory_Options.JavaClass.init;
  NativeBitmap := TJBitmapFactory.JavaClass.decodeFile(StringToJString(AFileName), LoadOptions);

  if (LoadOptions.outWidth < 1) or (LoadOptions.outHeight < 1) then
    Exit;

  Result := JBitmapToSurface(NativeBitmap, Bitmap);
end;

function TBitmapCodecAndroid.LoadThumbnailFromFile(const AFileName: string; const AFitWidth, AFitHeight: Single;
  const UseEmbedded: Boolean; const Bitmap: TBitmapSurface): Boolean;
var
  NativeBitmap1, NativeBitmap2: JBitmap;
  LoadOptions: JBitmapFactory_Options;
begin
  LoadOptions := TJBitmapFactory_Options.JavaClass.init;
  NativeBitmap1 := TJBitmapFactory.JavaClass.decodeFile(StringToJString(AFileName), LoadOptions);

  if (LoadOptions.outWidth < 1) or (LoadOptions.outHeight < 1) then
    Exit(False);

  NativeBitmap2 := TJBitmap.JavaClass.createScaledBitmap(NativeBitmap1, Round(AFitWidth), Round(AFitHeight), True);

  Result := JBitmapToSurface(NativeBitmap2, Bitmap);
end;

function TBitmapCodecAndroid.SaveToFile(const AFileName: string; const Bitmap: TBitmapSurface;
  const SaveParams: PBitmapCodecSaveParams): Boolean;
var
  NativeBitmap: JBitmap;
  SaveFormat: JBitmap_CompressFormat;
  OutFileStream: JFileOutputStream;
  SaveQuality: Integer;
begin
  NativeBitmap := TJBitmap.JavaClass.createBitmap(Bitmap.Width, Bitmap.Height, TJBitmap_Config.JavaClass.ARGB_8888);

  Result := SurfaceToJBitmap(Bitmap, NativeBitmap);
  if not Result then
    Exit;

  if SameText(ExtractFileExt(AFileName), '.PNG') then
    SaveFormat := TJBitmap_CompressFormat.JavaClass.PNG
  else
    SaveFormat := TJBitmap_CompressFormat.JavaClass.JPEG;

  OutFileStream := TJFileOutputStream.JavaClass.init(StringToJString(AFileName));

  SaveQuality := DefaultSaveQuality;

  if Assigned(SaveParams) then
    SaveQuality := SaveParams.Quality;

  Result := NativeBitmap.compress(SaveFormat, SaveQuality, OutFileStream);
end;

initialization
  TBitmapCodecManager.RegisterBitmapCodecClass('.jpg', SVJPGImages, True, TBitmapCodecAndroid);
  TBitmapCodecManager.RegisterBitmapCodecClass('.jpeg', SVJPGImages, True, TBitmapCodecAndroid);
  TBitmapCodecManager.RegisterBitmapCodecClass('.png', SVPNGImages, True, TBitmapCodecAndroid);

end.
