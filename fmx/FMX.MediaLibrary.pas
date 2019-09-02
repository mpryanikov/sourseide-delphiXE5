{*******************************************************}
{                                                       }
{           Delphi FireMonkey Mobile Services           }
{                                                       }
{     Description of interfaces for operation with      }
{        media storage (Audios, Videos, Images)         }
{                                                       }
{ Copyright(c) 2012-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit FMX.MediaLibrary;

interface

uses
  System.Types, FMX.Types, FMX.Controls, FMX.Graphics, FMX.Messages;

type
  IFMXImageManagerService = interface;
  IFMXTakenImageService = interface;
  IFMXCameraService = interface;
  IFMXAudioManagerService = interface;
  IFMXTakenAudioService = interface;
  IFMXVideoManagerService = interface;

{$REGION 'Image manager services'}
  TImageOrientation = (ioUp, ioRight, ioDown, ioLeft);

  { Geographical coordinates of a place of a picture }
  TLocationInfo = record
    Latitude: Double;
    Longitude: Double;
  end;

  { Representation of Image with meta information }
  TImageInfo = class abstract
  public
    // Image context
    function Bitmap: TBitmap; virtual; abstract;
    // Date of shooting of the photo
    function DateTaken: TDateTime; virtual; abstract;
    // Camera orientation at the moment of photo shooting
    function Orientation: TImageOrientation; virtual; abstract;
    // Geographical coordinates of a place of a picture 
    function LocationInfo: TLocationInfo; virtual; abstract;
  end;
  
  TImages = array of TImageInfo;

  { Interface of images manager }
  IFMXImageManagerService = interface (IInterface)
    ['{F06A5B8F-D4BC-4A0D-AD91-BF49208861E4}']
    // Returns the image on an index
    function GetImage(const Index: Integer): TImageInfo;
    // Returns all images from mobile store
    function GetAllImages: TImages;
    // Returns count of all images from mobile store
    function GetCount: Integer;
    // Insert image into mobile store
    procedure InsertImage(const Bitmap: TBitmap);
  end;  

  TOnDidCancelTaking = procedure of object;
  TOnDidFinishTaking = procedure (Image: TBitmap) of object;

  TMessageDidCancelTaking = class (TMessage);
  TMessageDidFinishTakingImageFromCamera = class (TMessage<TBitmap>);
  TMessageDidFinishTakingImageFromLibrary = class (TMessage<TBitmap>);

  { Pick image from Photo Library. Call native UI picker }
  IFMXTakenImageService = interface (IInterface)
    ['{5DADF207-B6CE-4C3A-9E0F-C45B39128DA5}']
    procedure TakeImageFromLibrary(const AControl: TControl; const ARequiredResolution: TSize; const AOnDidDinishTaking: TOnDidFinishTaking; const AOnDidCancelTaking: TOnDidCancelTaking);
  end;

  { Get photo from device's camera }
  IFMXCameraService = interface (IInterface)
    ['{6779963D-633A-4ED0-80C8-C71AB4E26D91}']
    procedure TakePhoto(const AControl: TControl; const ARequiredResolution: TSize; const AOnDidDinishTaking: TOnDidFinishTaking; const AOnDidCancelTaking: TOnDidCancelTaking);
  end;
{$ENDREGION}

{$REGION 'Audio manager services'}
  { Common class represents video or audio information }
  TMediaInfo = class (TObject);
  TPlayList = array of TMediaInfo;

  { Representation of audio information }
  TAudioInfo = class abstract (TMediaInfo)
  public
    function Title: string; virtual; abstract;
    function Artist: string; virtual; abstract;
    function AlbumTitle: string; virtual; abstract;
    function AlbumArtist: string; virtual; abstract;
    function Genre: string; virtual; abstract;
    function Composer: string; virtual; abstract;
    function Duration: TTime; virtual; abstract;
    function AlbumTrackNumber: Cardinal; virtual; abstract;
    function DiscNumber: Cardinal; virtual; abstract;
    function Lyrics: string; virtual; abstract;
    function DiscCover: TBitmap; virtual; abstract;
  end;
  TAudios = array of TAudioInfo;

  { Representation of album information }
  TAlbum = class abstract
  public
    function Title: string; virtual; abstract;
    function Artist: string; virtual; abstract;
    function NumberOfSongs: Cardinal; virtual; abstract;
    function Cover: TBitmap; virtual; abstract;
  end;
  TAlbums = array of TAlbum;

  { Representation of artist information }
  TArtist = class abstract
  public
    function Artist: string; virtual; abstract;
    function NumberOfSongs: Integer; virtual; abstract;
  end;
  TArtists = array of TArtist;

  { Representation of genre information }
  TGenre = class abstract
  public
    function Genre: string; virtual; abstract;
    function CountSongs: Cardinal; virtual; abstract;
  end;
  TGenres = array of TGenre;

  { Interface of audio manager }
  IFMXAudioManagerService = interface (IInterface)
    ['{E2D6C4F8-F365-4E24-A461-C48288E4C710}']
    function GetAudio(const Index: Integer): TAudioInfo;
    function GetAudios: TAudios; overload;
    function GetAudios(const Album: TAlbum): TAudios; overload;
    function GetAudios(const Artist: TArtist): TAudios; overload;
    function GetAudios(const Genre: TGenre): TAudios; overload;
    function GetAlbums: TAlbums;
    function GetArtists: TArtists;
    function GetGenres: TGenres;
  end;

  { Pick image from iPod Library. Uses native UI picker }
  IFMXTakenAudioService = interface (IInterface)
    ['{7114C6A2-2A2A-4CDA-AB63-291C7C8D440E}']
    function GetAudioFromLibrary: TAudios;
  end;
{$ENDREGION}

{$REGION 'Video manager services'}
  { Representation of video information }
  TVideoInfo = class abstract(TMediaInfo)
  public
    function Title: string; virtual; abstract;
    function AlbumTitle: string; virtual; abstract;
    function Artist: string; virtual; abstract;
    function Duration: TTime; virtual; abstract;
    function Resolution: TSize; virtual; abstract;
    function Thumb: TBitmap; virtual; abstract;
  end;
  TVideos = array of TVideoInfo;

  { Interface of video manager }
  IFMXVideoManagerService = interface (IInterface)
    ['{C61ECA12-EF39-4FAF-A301-B8BA43F425BD}']
    function GetVideoFromLibrary: TVideos;
    function GetVideo(const Index: Integer): TVideoInfo;
    function GetAllVideos: TVideos; 
  end;
{$ENDREGION}

{$REGION 'Share Text and Image with other native services'}

  IFMXShareSheetActionsService = interface (IInterface)
  ['{79FCC7B1-C5BF-4533-B31E-084F1A6E2264}']
    procedure Share(const AControl: TControl; const AText: string; const AImage: TBitmap);
  end;

{$ENDREGION}

implementation

{$IFDEF IOS}
uses
  FMX.MediaLibrary.iOS;
{$ENDIF}

{$IFDEF ANDROID}
uses
  FMX.MediaLibrary.Android;
{$ENDIF}

initialization
{$IF Defined(IOS) OR Defined(ANDROID)}
  RegisterMediaLibraryServices;
{$ENDIF}
end.
