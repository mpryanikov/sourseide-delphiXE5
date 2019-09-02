{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit FMX.Graphics;

{$MINENUMSIZE 4}
{$H+}

interface

uses
  System.Types, System.UITypes, System.UIConsts, System.Math, System.Variants,
  System.Classes, System.Generics.Collections, System.Generics.Defaults, System.SysUtils,
  FMX.Types, FMX.PixelFormats, FMX.Surfaces, FMX.Messages;

{$SCOPEDENUMS ON}

type

  TCanvas = class;
  TCanvasClass = class of TCanvas;
  TBitmap = class;

{ TGradientPoint }

  TGradientPoint = class(TCollectionItem)
  private
    FColor: TAlphaColor;
    FOffset: Single;
    function GetColor: TAlphaColor;
    procedure SetColor(const Value: TAlphaColor);
  public
    procedure Assign(Source: TPersistent); override;
    property IntColor: TAlphaColor read FColor write FColor;
  published
    property Color: TAlphaColor read GetColor write SetColor;
    property Offset: Single read FOffset write FOffset nodefault;
  end;

{ TGradientPoints }

  TGradientPoints = class(TCollection)
  private
    function GetPoint(Index: Integer): TGradientPoint;
  public
    property Points[Index: Integer]: TGradientPoint read GetPoint; default;
  end;

{ TGradient }

  TGradientStyle = (gsLinear, gsRadial);

  TGradient = class(TPersistent)
  private
    FPoints: TGradientPoints;
    FOnChanged: TNotifyEvent;
    FStartPosition: TPosition;
    FStopPosition: TPosition;
    FStyle: TGradientStyle;
    FRadialTransform: TTransform;
    procedure SetStartPosition(const Value: TPosition);
    procedure SetStopPosition(const Value: TPosition);
    procedure PositionChanged(Sender: TObject);
    procedure SetColor(const Value: TAlphaColor);
    procedure SetColor1(const Value: TAlphaColor);
    function IsLinearStored: Boolean;
    procedure SetStyle(const Value: TGradientStyle);
    function IsRadialStored: Boolean;
    procedure SetRadialTransform(const Value: TTransform);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Change;
    procedure ApplyOpacity(const AOpacity: Single);
    function InterpolateColor(Offset: Single): TAlphaColor; overload;
    function InterpolateColor(X, Y: Single): TAlphaColor; overload;
    function Equal(const AGradient: TGradient): Boolean;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    { fast access }
    property Color: TAlphaColor write SetColor;
    property Color1: TAlphaColor write SetColor1;
  published
    property Points: TGradientPoints read FPoints write FPoints;
    property Style: TGradientStyle read FStyle write SetStyle default TGradientStyle.gsLinear;
    { linear }
    property StartPosition: TPosition read FStartPosition write SetStartPosition stored IsLinearStored;
    property StopPosition: TPosition read FStopPosition write SetStopPosition stored IsLinearStored;
    { radial }
    property RadialTransform: TTransform read FRadialTransform write SetRadialTransform stored IsRadialStored;
  end;

{ TBrushResource }

  TBrush = class;
  TBrushObject = class;

  TBrushResource = class(TInterfacedPersistent, IFreeNotification)
  private
    FStyleResource: TBrushObject;
    FStyleLookup: string;
    FOnChanged: TNotifyEvent;
    function GetBrush: TBrush;
    procedure SetStyleResource(const Value: TBrushObject);
    function GetStyleLookup: string;
    procedure SetStyleLookup(const Value: string);
    { IFreeNotification }
    procedure FreeNotification(AObject: TObject);
  public
    destructor Destroy; override;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    procedure Assign(Source: TPersistent); override;
    property Brush: TBrush read GetBrush;
  published
    property StyleResource: TBrushObject read FStyleResource write SetStyleResource stored False;
    property StyleLookup: string read GetStyleLookup write SetStyleLookup;
  end;

{ TBrushBitmap }

  TWrapMode = (wmTile, wmTileOriginal, wmTileStretch);

  TBrushBitmap = class(TInterfacedPersistent)
  private
    FOnChanged: TNotifyEvent;
    FBitmap: TBitmap;
    FWrapMode: TWrapMode;
    procedure SetWrapMode(const Value: TWrapMode);
    procedure SetBitmap(const Value: TBitmap);
  protected
    procedure DoChanged; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    procedure Assign(Source: TPersistent); override;
  published
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property WrapMode: TWrapMode read FWrapMode write SetWrapMode;
  end;

{ TBrush }

  TBrushKind = (bkNone, bkSolid, bkGradient, bkBitmap, bkResource);

  TBrush = class(TPersistent)
  private
    FColor: TAlphaColor;
    FKind: TBrushKind;
    FOnChanged: TNotifyEvent;
    FGradient: TGradient;
    FDefaultKind: TBrushKind;
    FDefaultColor: TAlphaColor;
    FResource: TBrushResource;
    FBitmap: TBrushBitmap;
    FOnGradientChanged: TNotifyEvent;
    procedure SetColor(const Value: TAlphaColor);
    procedure SetKind(const Value: TBrushKind);
    procedure SetGradient(const Value: TGradient);
    function IsColorStored: Boolean;
    function IsGradientStored: Boolean;
    function GetColor: TAlphaColor;
    function IsKindStored: Boolean;
    procedure SetResource(const Value: TBrushResource);
    function IsResourceStored: Boolean;
    function IsBitmapStored: Boolean;
  protected
    procedure GradientChanged(Sender: TObject);
    procedure ResourceChanged(Sender: TObject);
    procedure BitmapChanged(Sender: TObject);
  public
    constructor Create(const ADefaultKind: TBrushKind; const ADefaultColor: TAlphaColor);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property OnGradientChanged: TNotifyEvent read FOnGradientChanged write FOnGradientChanged;
    property DefaultColor: TAlphaColor read FDefaultColor write FDefaultColor;
    property DefaultKind: TBrushKind read FDefaultKind write FDefaultKind;
  published
    property Color: TAlphaColor read GetColor write SetColor stored IsColorStored;
    property Bitmap: TBrushBitmap read FBitmap write FBitmap stored IsBitmapStored;
    property Kind: TBrushKind read FKind write SetKind stored IsKindStored;
    property Gradient: TGradient read FGradient write SetGradient stored IsGradientStored;
    property Resource: TBrushResource read FResource write SetResource stored IsResourceStored;
  end;

  TStrokeCap = (scFlat, scRound);

  TStrokeJoin = (sjMiter, sjRound, sjBevel);

  TStrokeDash = (sdSolid, sdDash, sdDot, sdDashDot, sdDashDotDot, sdCustom);

  TDashArray = array of Single;

  TStrokeBrush = class(TBrush)
  public type
    TDashData = record
      DashArray: TDashArray;
      DashOffset: Single;
      constructor Create(const ADashArray: TDashArray; ADashOffset: Single);
    end;
    TDashDevice = (ddScreen, ddPrinter);
    TStdDashes = array [TDashDevice, TStrokeDash] of TDashData;
  private class var
    FStdDash: TStdDashes;
    FStdDashCreated: Boolean;
  private var
    FJoin: TStrokeJoin;
    FThickness: Single;
    FCap: TStrokeCap;
    FDash: TStrokeDash;
    FDashArray: TDashArray;
    FDashOffset: Single;
    function IsThicknessStored: Boolean;
    procedure SetCap(const Value: TStrokeCap);
    procedure SetDash(const Value: TStrokeDash);
    procedure SetJoin(const Value: TStrokeJoin);
    procedure SetThickness(const Value: Single);
    function GetDashArray: TDashArray;

    class function GetStdDash(const Device: TDashDevice;
      const Dash: TStrokeDash): TDashData; static;
  public
    constructor Create(const ADefaultKind: TBrushKind; const ADefaultColor: TAlphaColor); reintroduce;
    procedure Assign(Source: TPersistent); override;
    procedure SetCustomDash(const Dash: array of Single; Offset: Single);
    property DashArray: TDashArray read GetDashArray;
    property DashOffset: Single read FDashOffset;
    class property StdDash[const Device: TDashDevice; const Dash: TStrokeDash]: TDashData read GetStdDash;
  published
    property Thickness: Single read FThickness write SetThickness stored IsThicknessStored nodefault;
    property Cap: TStrokeCap read FCap write SetCap default TStrokeCap.scFlat;
    property Dash: TStrokeDash read FDash write SetDash default TStrokeDash.sdSolid;
    property Join: TStrokeJoin read FJoin write SetJoin default TStrokeJoin.sjMiter;
  end;

{ TFont }

  TFont = class(TPersistent)
  private
    FSize: Single;
    FFamily: TFontName;
    FStyle: TFontStyles;
    FUpdating: boolean;
    FOnChanged: TNotifyEvent;
    procedure SetFamily(const Value: TFontName);
    procedure SetSize(const Value: Single);
    procedure SetStyle(const Value: TFontStyles);
  protected
    function DefaultFamily: string; virtual;
    procedure DoChanged; virtual;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    procedure SetSettings(const AFamily: string; const ASize: Single; const AStyle: TFontStyles);
    function Equals(Obj: TObject): Boolean; override;
    function IsFamilyStored: Boolean;
    function IsSizeStored: Boolean;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  published
    property Family: TFontName read FFamily write SetFamily stored IsFamilyStored;
    property Size: Single read FSize write SetSize stored IsSizeStored nodefault;
    property Style: TFontStyles read FStyle write SetStyle default [];
  end;

{ TBitmapCodec }

  PBitmapCodecSaveParams = ^TBitmapCodecSaveParams;
  TBitmapCodecSaveParams = record
    // encode quality 0..100
    Quality: Integer;
  end;

  TCustomBitmapCodec = class abstract
  public
    class function GetImageSize(const AFileName: string): TPointF; virtual; abstract;
    class function IsValid(const AStream: TStream): Boolean; virtual; abstract;
    function LoadFromFile(const AFileName: string; const Bitmap: TBitmapSurface): Boolean; virtual; abstract;
    function LoadThumbnailFromFile(const AFileName: string; const AFitWidth, AFitHeight: Single;
      const UseEmbedded: Boolean; const Bitmap: TBitmapSurface): Boolean; virtual; abstract;
    function LoadFromStream(const AStream: TStream; const Bitmap: TBitmapSurface): Boolean; virtual; abstract;
    function SaveToFile(const AFileName: string; const Bitmap: TBitmapSurface; const SaveParams: PBitmapCodecSaveParams = nil): Boolean; virtual; abstract;
    function SaveToStream(const AStream: TStream; const Bitmap: TBitmapSurface; const Extension: string;
      const SaveParams: PBitmapCodecSaveParams = nil): Boolean; virtual; abstract;
  end;
  TCustomBitmapCodecClass = class of TCustomBitmapCodec;

{ TBitmapCodecManager }

  EBitmapCodecManagerException = class(Exception);

  TBitmapCodecManager = class sealed
  public type
    TBitmapCodecClassDescriptor = record
      Extension: string;
      Description: string;
      BitmapCodecClass: TCustomBitmapCodecClass;
      CanSave: Boolean;
    end;
  strict private type
    TBitmapCodecDescriptorField = (Extension, Description);
  strict private
    class var FBitmapCodecClassDescriptors: TList<TBitmapCodecClassDescriptor>;
    class function FindBitmapCodecDescriptor(const Name: string; const Field: TBitmapCodecDescriptorField): TBitmapCodecClassDescriptor;
  private
  public
    // Reserved for internal use only - do not call directly!
    class procedure UnInitialize;
    // Register a bitmap codec class with a file extension, description
    class procedure RegisterBitmapCodecClass(const Extension, Description: string; const CanSave: Boolean;
      const BitmapCodecClass: TCustomBitmapCodecClass); overload;
    // Helpful function
    class function GetFileTypes: string;
    class function GetFilterString: string;
    class function IsCodedExists(const AFileName: string): Boolean; overload;
    class function GetImageSize(const AFileName: string): TPointF;
    class function LoadFromFile(const AFileName: string; const Bitmap: TBitmapSurface): Boolean;
    class function LoadThumbnailFromFile(const AFileName: string; const AFitWidth, AFitHeight: Single;
      const UseEmbedded: Boolean; const Bitmap: TBitmapSurface): Boolean;
    class function LoadFromStream(const AStream: TStream; const Bitmap: TBitmapSurface): Boolean;
    class function SaveToStream(const AStream: TStream; const Bitmap: TBitmapSurface; const Extension: string;
      SaveParams: PBitmapCodecSaveParams = nil): Boolean; overload;
    class function SaveToFile(const AFileName: string; const Bitmap: TBitmapSurface; const SaveParams: PBitmapCodecSaveParams = nil): Boolean;
  end;

{ TBitmap }

  IBitmapObject = interface(IFreeNotificationBehavior)
    ['{5C17D001-47C1-462F-856D-8358B7B2C842}']
    function GetBitmap: TBitmap;
    property Bitmap: TBitmap read GetBitmap;
  end;

  IMultiResBitmapObject = interface(IBitmapObject)
    ['{D64BEB1F-D3C5-4C83-BE1C-DBBA319C0EA5}']
    function GetBitmapByScale(const Scale: Single): TBitmap;
  end;

  TBitmapHandle = THandle;

  TBitmapData = record
  private
    FPixelFormat: TPixelFormat;
    FWidth: Integer;
    FHeight: Integer;
    function GetBytesPerPixel: Integer;
    function GetBytesPerLine: Integer;
  public
    Data: Pointer;
    Pitch: Integer;
    constructor Create(const AWidth, AHeight: Integer; const APixelFormat: TPixelFormat);
    function GetPixel(const X, Y: Integer): TAlphaColor;
    procedure SetPixel(const X, Y: Integer; const AColor: TAlphaColor);
    procedure Copy(const Source: TBitmapData);
    // Access to scanline in PixelFormat
    function GetScanline(const I: Integer): Pointer;
    function GetPixelAddr(const I, J: Integer): Pointer;
    property PixelFormat: TPixelFormat read FPixelFormat;
    property BytesPerPixel: Integer read GetBytesPerPixel;
    property BytesPerLine: Integer read GetBytesPerLine;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
  end;

  TMapAccess = (maRead, maWrite, maReadWrite);

  IBitmapAccess = interface
  ['{E6A4D7BB-B9C3-4D52-A4E3-B2BE974F9A93}']
    procedure SetHandle(const Handle: TBitmapHandle);
    procedure SetPixelFormat(const PixelFormat: TPixelFormat);
    procedure SetBitmapScale(const Scale: Single);
    property Handle: TBitmapHandle write SetHandle;
    property PixelFormat: TPixelFormat write SetPixelFormat;
    property BitmapScale: Single write SetBitmapScale;
  end;

  IBitmapLink = interface
  ['{95910ECE-2F2F-4BB3-A79C-0139E743180A}']
    procedure SetResourceBitmap(const ABitmap: TBitmap);
  end;

  TBitmap = class(TInterfacedPersistent, IStreamPersist, IFreeNotification, IBitmapAccess, IBitmapLink)
  private
    FNotifyList: TList<Pointer>;
    FHeight: Integer;
    FWidth: Integer;
    FOnChange: TNotifyEvent;
    FResource: TObject;
    FStyleLookup: string;
    FHandle: TBitmapHandle;
    FCanvas: TCanvas;
    FMapped: Boolean;
    FMapAccess: TMapAccess;
    FPixelFormat: TPixelFormat;
    FBitmapScale: Single;
    FCanvasClass: TCanvasClass;
    function GetCanvasClass: TCanvasClass;
    function GetCanvas: TCanvas;
    function GetStyleLookup: string;
    procedure SetStyleLookup(const Value: string);
    function GetBitmap: TBitmap;
    procedure BitmapChanged;
    { IBitmapLink }
    procedure SetResourceBitmap(const ABitmap: TBitmap);
    { IBitmapAccess }
    procedure SetHandle(const Handle: TBitmapHandle);
    procedure SetPixelFormat(const PixelFormat: TPixelFormat);
    function GetHandle: TBitmapHandle;
    function GetPixelFormat: TPixelFormat;
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    function GetBytesPerLine: Integer;
    function GetBytesPerPixel: Integer;
  protected
    { internal }
    FTexture: TObject;
    FTextureNeedUpdate: Boolean;
    procedure Recreate;
    procedure SetBitmapScale(const Scale: Single);
    { vcl }
    procedure AssignTo(Dest: TPersistent); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadBitmap(Stream: TStream);
    procedure WriteBitmap(Stream: TStream);
    { IFreeNotification }
    procedure FreeNotification(AObject: TObject); virtual;

    procedure DoChange; virtual;
  public
    constructor Create; overload; virtual;
    constructor Create(const AWidth, AHeight: Integer); overload; virtual;
    constructor CreateFromStream(const AStream: TStream); virtual;
    constructor CreateFromFile(const AFileName: string); virtual;
    constructor CreateFromBitmapAndMask(const Bitmap, Mask: TBitmap);
    procedure Assign(Source: TPersistent); override;
    destructor Destroy; override;
    procedure SetSize(const AWidth, AHeight: Integer);
    // Size of source and destination bitmap must be equal
    procedure CopyFromBitmap(const Source: TBitmap); overload;
    procedure CopyFromBitmap(const Source: TBitmap; SrcRect: TRect; DstX, DstY: Integer); overload;
    function IsEmpty: Boolean;
    function HandleAllocated: Boolean;
    { FreeNotify }
    procedure AddFreeNotify(const AObject: IFreeNotification);
    procedure RemoveFreeNotify(const AObject: IFreeNotification);
    { Manipulation }
    procedure Clear(const AColor: TAlphaColor); virtual;
    procedure ClearRect(const ARect: TRectF; const AColor: TAlphaColor = 0);
    procedure Rotate(const Angle: Single);
    procedure Resize(const AWidth, AHeight: Integer);
    procedure FlipHorizontal;
    procedure FlipVertical;
    procedure InvertAlpha;
    procedure ReplaceOpaqueColor(const Color: TAlphaColor);
    { Mask }
    function CreateMask: PByteArray;
    procedure ApplyMask(const Mask: PByteArray; const DstX: Integer = 0; const DstY: Integer = 0);
    { Thumb }
    function CreateThumbnail(const Width, Height: Integer): TBitmap;
    { I/O }
    procedure LoadFromFile(const AFileName: string; const Rotate: Single = 0);
    procedure LoadThumbnailFromFile(const AFileName: string; const AFitWidth, AFitHeight: Single;
      const UseEmbedded: Boolean = True);
    procedure SaveToFile(const AFileName: string; const SaveParams: PBitmapCodecSaveParams = nil);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    { Painting }
    property Canvas: TCanvas read GetCanvas;
    { Direct access  }
    function Map(const Access: TMapAccess; var Data: TBitmapData): Boolean;
    procedure Unmap(var Data: TBitmapData);
    { Properties }
    property CanvasClass: TCanvasClass read GetCanvasClass;
    property Handle: TBitmapHandle read GetHandle;
    property PixelFormat: TPixelFormat read GetPixelFormat;
    property BytesPerPixel: Integer read GetBytesPerPixel;
    property BytesPerLine: Integer read GetBytesPerLine;
    property BitmapScale: Single read FBitmapScale; // hi resolution mode
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    { Internal usage only }
    property ResourceBitmap: TBitmap read GetBitmap; // use to resource link
  published
    property StyleLookup: string read GetStyleLookup write SetStyleLookup;
  end;

{ TPathData }

  TPathData = class;

  IPathObject = interface(IFreeNotificationBehavior)
    ['{8C014863-4F69-48F2-9CF7-E336BFD3F06B}']
    function GetPath: TPathData;
    property Path: TPathData read GetPath;
  end;

  TPathPointKind = (ppMoveTo, ppLineTo, ppCurveTo, ppClose);

  TPathPoint = packed record
    Kind: TPathPointKind;
    Point: TPointF;
  end;

  TPathObject = class;

  TPathData = class(TInterfacedPersistent, IFreeNotification)
  private
    FOnChanged: TNotifyEvent;
    FStyleResource: TObject;
    FStyleLookup: string;
    FStartPoint: TPointF;
    FPathData: array of TPathPoint;
    FRecalcBounds: Boolean;
    FBounds: TRectF;
    function GetPathString: string;
    procedure SetPathString(const Value: string);
    procedure AddArcSvgPart(const Center, Radius: TPointF; StartAngle, SweepAngle: Single);
    procedure AddArcSvg(const P1, Radius: TPointF; Angle: Single; const LargeFlag, SweepFlag: Boolean; const P2: TPointF);
    function GetStyleLookup: string;
    procedure SetStyleLookup(const Value: string);
    function GetPath: TPathData;
    function GetCount: Integer; inline;
    function GetPoint(AIndex: Integer): TPathPoint; inline;
    function GetPointFromStr(const S: string; var Pos: Integer): TPointF;
    function GetNum(const S: string; var Pos: Integer): string;
    function GetTok(const S: string; var Pos: Integer): string;
  protected
    { rtl }
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadPath(Stream: TStream);
    procedure WritePath(Stream: TStream);
    { IFreeNotification }
    procedure FreeNotification(AObject: TObject);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    { creation }
    function LastPoint: TPointF;
    procedure MoveTo(const P: TPointF);
    procedure MoveToRel(const P: TPointF);
    procedure LineTo(const P: TPointF);
    procedure LineToRel(const P: TPointF);
    procedure HLineTo(const X: Single);
    procedure HLineToRel(const X: Single);
    procedure VLineTo(const Y: Single);
    procedure VLineToRel(const Y: Single);
    procedure CurveTo(const ControlPoint1, ControlPoint2, EndPoint: TPointF);
    procedure CurveToRel(const ControlPoint1, ControlPoint2, EndPoint: TPointF);
    procedure SmoothCurveTo(const ControlPoint2, EndPoint: TPointF);
    procedure SmoothCurveToRel(const ControlPoint2, EndPoint: TPointF);
    procedure QuadCurveTo(const ControlPoint, EndPoint: TPointF);
    procedure ClosePath;
    { shapes }
    procedure AddEllipse(const ARect: TRectF);
    procedure AddRectangle(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
      const ACornerType: TCornerType = TCornerType.ctRound);
    procedure AddArc(const Center, Radius: TPointF; StartAngle, SweepAngle: Single);
    { modification }
    procedure AddPath(APath: TPathData);
    procedure Clear;
    procedure Flatten(const Flatness: Single = 0.25);
    procedure Scale(const scaleX, scaleY: Single);
    procedure Translate(const dX, dY: Single);
    procedure FitToRect(const ARect: TRectF);
    procedure ApplyMatrix(const M: TMatrix);
    { params }
    function GetBounds: TRectF;
    { convert }
    function FlattenToPolygon(var Polygon: TPolygon; const Flatness: Single = 0.25): TPointF;
    function IsEmpty: Boolean;
    { access }
    property Count: Integer read GetCount;
    property Points[AIndex: Integer]: TPathPoint read GetPoint; default;
    { resoruces }
    property ResourcePath: TPathData read GetPath;
  published
    property Data: string read GetPathString write SetPathString stored False;
    { This property allow to link path with PathObject by name. }
    property StyleLookup: string read GetStyleLookup write SetStyleLookup;
  end;

{ TCanvasSaveState }

  TCanvasSaveState = class(TPersistent)
  private
  protected
    FAssigned: Boolean;
    FFill: TBrush;
    FStroke: TStrokeBrush;
    FDash: TDashArray;
    FDashOffset: Single;
    FFont: TFont;
    FMatrix: TMatrix;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Assigned: Boolean read FAssigned;
  end;

  TRegion = array of TRectF;
  TRegionArray = array of TRegion;

{ TCanvas }

  ECanvasException = class(Exception);

  TFillTextFlag = (ftRightToLeft);

  TFillTextFlags = set of TFillTextFlag;

  TAbstractPrinter = class(TPersistent);

  PClipRects = ^TClipRects;
  TClipRects = array of TRectF;

  ICanvasObject = interface
  ['{61166E3B-9BC3-41E3-9D9A-5C6AB6460950}']
    function GetCanvas: TCanvas;
    property Canvas: TCanvas read GetCanvas;
  end;

  TCanvasStyle = (NeedGPUSurface, SupportClipRects);
  TCanvasStyles = set of TCanvasStyle;

  TCanvasQuality = (ccSystemDefault, ccHighPerformance, ccHighQuality);

  TCanvas = class abstract(TInterfacedPersistent)
  private
    FBeginSceneCount: Integer;
    FFill: TBrush;
    FStroke: TStrokeBrush;
    FParent: TWindowHandle;
    [Weak] FBitmap: TBitmap;
    FScale: Single;
    FQuality: TCanvasQuality;
    procedure SetFill(const Value: TBrush);
    { deprecated }
    function GetStrokeThickness: Single;
    procedure SetStrokeDash(const Value: TStrokeDash);
    procedure SetStrokeCap(const Value: TStrokeCap);
    procedure SetStrokeJoin(const Value: TStrokeJoin);
    procedure SetStrokeThickness(const Value: Single);
  protected type
    TMatrixMeaning = (mmUnknown, mmIdentity, mmTranslate);
  private
    FMatrixMeaning: TMatrixMeaning;
    FMatrixTranslate: TPointF;
    FBlending: Boolean;
    procedure SetBlending(const Value: Boolean);
  type
    TCanvasSaveStateList = TObjectList<TCanvasSaveState>;
  protected
    FClippingChangeCount: Integer;
    FSavingStateCount: Integer;
    FWidth, FHeight: Integer;
    FMatrix: TMatrix;
    FFont: TFont;
    FCanvasSaveData: TCanvasSaveStateList;
    FPrinter: TAbstractPrinter;
    procedure FontChanged(Sender: TObject); virtual;
    { Window }
    function CreateSaveState: TCanvasSaveState; virtual;
    procedure Initialize;
    procedure UnInitialize;
    function GetCanvasScale: Single; virtual;
    { scene }
    function DoBeginScene(const AClipRects: PClipRects = nil; AContextHandle: THandle = 0): Boolean; virtual;
    procedure DoEndScene; virtual;
    { constructors }
    constructor CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer;
      const AQuality: TCanvasQuality = TCanvasQuality.ccSystemDefault); virtual;
    constructor CreateFromBitmap(const ABitmap: TBitmap; const AQuality: TCanvasQuality = TCanvasQuality.ccSystemDefault); virtual;
    constructor CreateFromPrinter(const APrinter: TAbstractPrinter); virtual;
    { bitmap }
    class procedure DoInitializeBitmap(const Bitmap: TBitmap); virtual; abstract;
    class procedure DoFinalizeBitmap(const Bitmap: TBitmap); virtual; abstract;
    class function DoMapBitmap(const Bitmap: TBitmap; const Access: TMapAccess; var Data: TBitmapData): Boolean; virtual; abstract;
    class procedure DoUnmapBitmap(const Bitmap: TBitmap; var Data: TBitmapData); virtual; abstract;
    class procedure DoCopyBitmap(const Source, Dest: TBitmap); virtual;
    { states }
    procedure DoBlendingChanged; virtual;
    { drawing }
    procedure DoFillRect(const ARect: TRectF; const AOpacity: Single; const ABrush: TBrush); virtual; abstract;
    procedure DoFillRoundRect(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
      const AOpacity: Single; const ABrush: TBrush; const ACornerType: TCornerType = TCornerType.ctRound); virtual;
    procedure DoFillPath(const APath: TPathData; const AOpacity: Single; const ABrush: TBrush); virtual; abstract;
    procedure DoFillEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TBrush); virtual; abstract;
    procedure DoDrawBitmap(const ABitmap: TBitmap; const SrcRect, DstRect: TRectF; const AOpacity: Single; const HighSpeed: Boolean); virtual; abstract;
    procedure DoDrawLine(const APt1, APt2: TPointF; const AOpacity: Single; const ABrush: TStrokeBrush); virtual; abstract;
    procedure DoDrawRect(const ARect: TRectF; const AOpacity: Single; const ABrush: TStrokeBrush); virtual; abstract;
    procedure DoDrawPath(const APath: TPathData; const AOpacity: Single; const ABrush: TStrokeBrush); virtual; abstract;
    procedure DoDrawEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TStrokeBrush); virtual; abstract;
    property Parent: TWindowHandle read FParent;
  protected
    function TransformPoint(const P: TPointF): TPointF; inline;
    function TransformRect(const Rect: TRectF): TRectF; inline;
    property MatrixMeaning: TMatrixMeaning read FMatrixMeaning;
    property MatrixTranslate: TPointF read FMatrixTranslate;
  public
    destructor Destroy; override;
    { style }
    class function GetCanvasStyle: TCanvasStyles; virtual;
    { scene }
    procedure SetSize(const AWidth, AHeight: Integer); virtual;
    function BeginScene(AClipRects: PClipRects = nil; AContextHandle: THandle = 0): Boolean;
    procedure EndScene;
    property BeginSceneCount: integer read FBeginSceneCount;
    { buffer }
    procedure Clear(const Color: TAlphaColor); virtual; abstract;
    procedure ClearRect(const ARect: TRectF; const AColor: TAlphaColor = 0); virtual; abstract;
    { matrix }
    procedure SetMatrix(const M: TMatrix); virtual;
    procedure MultiplyMatrix(const M: TMatrix);
    { state }
    function SaveState: TCanvasSaveState;
    procedure RestoreState(const State: TCanvasSaveState);
    { bitmap }
    class procedure InitializeBitmap(const Bitmap: TBitmap);
    class procedure FinalizeBitmap(const Bitmap: TBitmap);
    class function MapBitmap(const Bitmap: TBitmap; const Access: TMapAccess; var Data: TBitmapData): Boolean;
    class procedure UnmapBitmap(const Bitmap: TBitmap; var Data: TBitmapData);
    class procedure CopyBitmap(const Source, Dest: TBitmap);
    { aligning }
    function AlignToPixel(const Value: TPointF): TPointF; inline;
    function AlignToPixelVertically(const Value: Single): Single; inline;
    function AlignToPixelHorizontally(const Value: Single): Single; inline;
    { clipping }
    procedure IntersectClipRect(const ARect: TRectF); virtual; abstract;
    procedure ExcludeClipRect(const ARect: TRectF); virtual; abstract;
    { drawing }
    procedure FillArc(const Center, Radius: TPointF; StartAngle, SweepAngle: Single; const AOpacity: Single); overload;
    procedure FillArc(const Center, Radius: TPointF; StartAngle, SweepAngle: Single;
      const AOpacity: Single; const ABrush: TBrush); overload;
    procedure FillRect(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
      const AOpacity: Single; const ACornerType: TCornerType = TCornerType.ctRound); overload;
    procedure FillRect(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
      const AOpacity: Single; const ABrush: TBrush; const ACornerType: TCornerType = TCornerType.ctRound); overload;
    procedure FillPath(const APath: TPathData; const AOpacity: Single); overload;
    procedure FillPath(const APath: TPathData; const AOpacity: Single; const ABrush: TBrush); overload;
    procedure FillEllipse(const ARect: TRectF; const AOpacity: Single); overload;
    procedure FillEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TBrush); overload;
    procedure DrawBitmap(const ABitmap: TBitmap; const SrcRect, DstRect: TRectF; const AOpacity: Single;
      const HighSpeed: Boolean = False);
    procedure DrawLine(const APt1, APt2: TPointF; const AOpacity: Single); overload;
    procedure DrawLine(const APt1, APt2: TPointF; const AOpacity: Single; const ABrush: TStrokeBrush); overload;
    procedure DrawRect(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
      const AOpacity: Single; const ACornerType: TCornerType = TCornerType.ctRound); overload;
    procedure DrawRect(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
      const AOpacity: Single; const ABrush: TStrokeBrush; const ACornerType: TCornerType = TCornerType.ctRound); overload;
    procedure DrawPath(const APath: TPathData; const AOpacity: Single); overload;
    procedure DrawPath(const APath: TPathData; const AOpacity: Single; const ABrush: TStrokeBrush); overload;
    procedure DrawEllipse(const ARect: TRectF; const AOpacity: Single); overload;
    procedure DrawEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TStrokeBrush); overload;
    procedure DrawArc(const Center, Radius: TPointF; StartAngle, SweepAngle: Single; const AOpacity: Single); overload;
    procedure DrawArc(const Center, Radius: TPointF; StartAngle, SweepAngle: Single; const AOpacity: Single; const ABrush: TStrokeBrush); overload;
    { mesauring }
    function PtInPath(const APoint: TPointF; const APath: TPathData): Boolean; virtual; abstract;
    { helpers }
    procedure DrawRectSides(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
      const AOpacity: Single; const ASides: TSides; const ACornerType: TCornerType = TCornerType.ctRound); overload;
    procedure DrawRectSides(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
      const AOpacity: Single; const ASides: TSides; const ABrush: TStrokeBrush; const ACornerType: TCornerType = TCornerType.ctRound); overload;
    procedure DrawDashRect(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
      const AOpacity: Single; const AColor: TAlphaColor);
    { linear polygon }
    procedure FillPolygon(const Points: TPolygon; const AOpacity: Single); virtual;
    procedure DrawPolygon(const Points: TPolygon; const AOpacity: Single); virtual;
    { text }
    function LoadFontFromStream(const AStream: TStream): Boolean; virtual;
    { deprecated, use TTextLayout }
    procedure FillText(const ARect: TRectF; const AText: string; const WordWrap: Boolean; const AOpacity: Single;
      const Flags: TFillTextFlags; const ATextAlign: TTextAlign; const AVTextAlign: TTextAlign = TTextAlign.taCenter); virtual;// deprecated 'use TTextLayout';
    procedure MeasureText(var ARect: TRectF; const AText: string; const WordWrap: Boolean; const Flags: TFillTextFlags;
      const ATextAlign: TTextAlign; const AVTextAlign: TTextAlign = TTextAlign.taCenter); virtual;// deprecated 'use TTextLayout';
    procedure MeasureLines(const ALines: TLineMetricInfo; const ARect: TRectF; const AText: string; const WordWrap: Boolean; const Flags: TFillTextFlags;
      const ATextAlign: TTextAlign; const AVTextAlign: TTextAlign = TTextAlign.taCenter); virtual;
    function TextToPath(Path: TPathData; const ARect: TRectF; const AText: string; const WordWrap: Boolean;
      const ATextAlign: TTextAlign; const AVTextAlign: TTextAlign = TTextAlign.taCenter): Boolean; virtual;// deprecated 'use TTextLayout';
    function TextWidth(const AText: string): Single;// deprecated 'use TTextLayout';
    function TextHeight(const AText: string): Single;// deprecated 'use TTextLayout';
    { deprecated - moved to TStokeBrush }
    procedure SetCustomDash(const Dash: array of Single; Offset: Single);
    property StrokeThickness: Single read GetStrokeThickness write SetStrokeThickness;
    property StrokeCap: TStrokeCap write SetStrokeCap;
    property StrokeDash: TStrokeDash write SetStrokeDash;
    property StrokeJoin: TStrokeJoin write SetStrokeJoin;
    { properties }
    property Blending: Boolean read FBlending write SetBlending;
    property Quality: TCanvasQuality read FQuality;
    property Stroke: TStrokeBrush read FStroke;
    property Fill: TBrush read FFill write SetFill;
    property Font: TFont read FFont;
    { usage in PaintTo }
    property Matrix: TMatrix read FMatrix;
    { read only }
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Bitmap: TBitmap read FBitmap;
    property Scale: Single read FScale;
    { statistics }
    property ClippingChangeCount: Integer read FClippingChangeCount;
    property SavingStateCount: Integer read FSavingStateCount;
  end;

  ECanvasManagerException = class(Exception);

  TCanvasDestroyMessage = class(TMessage);

  TCanvasManager = class sealed
  private type
    TCanvasClassRec = record
      CanvasClass: TCanvasClass;
      Default: Boolean;
      PrinterCanvas: Boolean;
    end;
  strict private
    class var FCanvasList: TList<TCanvasClassRec>;
    class var FDefaultCanvasClass: TCanvasClass;
    class var FDefaultPrinterCanvasClass: TCanvasClass;
    class var FMeasureBitmap: TBitmap;
    class var FEnableSoftwareCanvas: Boolean;
  private
    class function GetDefaultCanvas: TCanvasClass; static;
    class function GetMeasureCanvas: TCanvas; static;
    class function GetDefaultPrinterCanvas: TCanvasClass; static;
  public
    // Reserved for internal use only - do not call directly!
    class procedure UnInitialize;
    // Register a rendering Canvas class
    class procedure RegisterCanvas(const CanvasClass: TCanvasClass; const ADefault: Boolean; const APrinterCanvas: Boolean);
    // Return default Canvas
    class property DefaultCanvas: TCanvasClass read GetDefaultCanvas;
    // Return default Canvas
    class property DefaultPrinterCanvas: TCanvasClass read GetDefaultPrinterCanvas;
    // Return canvas instance used for text measuring for example
    class property MeasureCanvas: TCanvas read GetMeasureCanvas;
    // Creation helper
    class function CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer;
      const AQuality: TCanvasQuality = TCanvasQuality.ccSystemDefault): TCanvas;
    class function CreateFromBitmap(const ABitmap: TBitmap;
      const AQuality: TCanvasQuality = TCanvasQuality.ccSystemDefault): TCanvas;
    class function CreateFromPrinter(const APrinter: TAbstractPrinter): TCanvas;
    class procedure RecreateFromPrinter(const Canvas: TCanvas; const APrinter: TAbstractPrinter);
    class procedure EnableSoftwareCanvas(const Enable: Boolean);
  end;

  TPrinterCanvas = class(TCanvas)
  end;

  TPrinterCanvasClass = class of TPrinterCanvas;

{ TBrushObject }

  IBrushObject = interface(IFreeNotificationBehavior)
    ['{BB870DB6-0228-4165-9906-CF75BFF8C7CA}']
    function GetBrush: TBrush;
    property Brush: TBrush read GetBrush;
  end;

  TBrushObject = class(TFmxObject, IBrushObject)
  private
    FBrush: TBrush;
    { IBrushObject }
    function GetBrush: TBrush;
  protected
    procedure SetName(const NewName: TComponentName); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Brush: TBrush read FBrush write FBrush;
  end;

{ TFontObject }

  IFontObject = interface(IFreeNotificationBehavior)
    ['{F87FBCFE-CE5F-430C-8F46-B20B2E395C1B}']
    function GetFont: TFont;
    property Font: TFont read GetFont;
  end;

  TFontObject = class(TFmxObject, IFontObject)
  private
    FFont: TFont;
    { IFontObject }
    function GetFont: TFont;
  protected
    procedure SetName(const NewName: TComponentName); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Font: TFont read FFont write FFont;
  end;

{ TPathObject }

  TPathObject = class(TFmxObject, IPathObject)
  private
    FPath: TPathData;
    { IPathObject }
    function GetPath: TPathData;
  protected
    procedure SetName(const NewName: TComponentName); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Path: TPathData read FPath write FPath;
  end;

{ TBitmapObject }

  TBitmapObject = class(TFmxObject, IBitmapObject)
  private
    FBitmap: TBitmap;
    { IBitmapObject }
    function GetBitmap: TBitmap;
  protected
    procedure SetName(const NewName: TComponentName); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Bitmap: TBitmap read FBitmap write FBitmap;
  end;

{ TColorObject }

  TColorObject = class(TFmxObject)
  private
    FColor: TAlphaColor;
  protected
    procedure SetName(const NewName: TComponentName); override;
  published
    property Color: TAlphaColor read FColor write FColor;
  end;

  /// <summary>
  ///   This class combines some of properties that relate to the text
  /// </summary>
  TTextSettings = class (TPersistent)
  private
    [Weak] FOwner: TPersistent;
    FFont: TFont;
    FUpdateCount: Integer;
    FHorzAlign: TTextAlign;
    FVertAlign: TTextAlign;
    FWordWrap: Boolean;
    FFontColor: TAlphaColor;
    FIsChanged: Boolean;
    FIsAdjustChanged: Boolean;
    FOnChanged: TNotifyEvent;
    FTrimming: TTextTrimming;
    procedure SetFontColor(const Value: TAlphaColor);
    procedure SetHorzAlign(const Value: TTextAlign);
    procedure SetVertAlign(const Value: TTextAlign);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetTrimming(const Value: TTextTrimming);
  protected
    procedure DoChanged; virtual;
    procedure SetFont(const Value: TFont); virtual;
    function GetOwner: TPersistent; override;
  public
    constructor Create(const AOwner: TPersistent); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignNoStyled(const TextSettings: TTextSettings;
                     const StyledSettings: TStyledSettings);
    function Equals(Obj: TObject): Boolean; override;
    procedure Change;
    procedure BeginUpdate;
    procedure EndUpdate;
    property UpdateCount: integer read FUpdateCount;
    property IsChanged: boolean read FIsChanged write FIsChanged;
    property IsAdjustChanged: boolean read FIsAdjustChanged write FIsAdjustChanged;
    property Font: TFont read FFont write SetFont;
    property FontColor: TAlphaColor read FFontColor write SetFontColor default TAlphaColorRec.Black;
    property HorzAlign: TTextAlign read FHorzAlign write SetHorzAlign default TTextAlign.taLeading;
    property VertAlign: TTextAlign read FVertAlign write SetVertAlign default TTextAlign.taCenter;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    property Trimming: TTextTrimming read FTrimming write SetTrimming default TTextTrimming.ttNone;
    property Owner: TPersistent read FOwner;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  ITextSettings = interface
  ['{FD99635D-D8DB-4E26-B36F-97D3AABBCCB3}']
    function GetDefaultTextSettings: TTextSettings;
    function GetTextSettings: TTextSettings;
    procedure SetTextSettings(const Value: TTextSettings);
    function GetStyledSettings: TStyledSettings;
    procedure SetStyledSettings(const Value: TStyledSettings);

    property DefaultTextSettings: TTextSettings read GetDefaultTextSettings;
    property TextSettings: TTextSettings read GetTextSettings write SetTextSettings;
    property StyledSettings: TStyledSettings read GetStyledSettings write SetStyledSettings;
  end;

implementation

uses
  System.Character, FMX.Platform, FMX.Consts, FMX.TextLayout;

{ TGradientPoint }

procedure TGradientPoint.Assign(Source: TPersistent);
begin
  if Source is TGradientPoint then
  begin
    FColor := TGradientPoint(Source).FColor;
    FOffset := TGradientPoint(Source).FOffset;
  end
  else
    inherited;
end;

function TGradientPoint.GetColor: TAlphaColor;
begin
  Result := FColor;
end;

procedure TGradientPoint.SetColor(const Value: TAlphaColor);
begin
  FColor := Value;
end;

{ TGradientPoints }

function TGradientPoints.GetPoint(Index: Integer): TGradientPoint;
begin
  Result := TGradientPoint(Items[Index]);
end;

{ TGradient }

constructor TGradient.Create;
var
  P: TGradientPoint;
begin
  inherited;
  FStartPosition := TPosition.Create(PointF(0, 0));
  FStartPosition.OnChange := PositionChanged;
  FStopPosition := TPosition.Create(PointF(0, 1));
  FStopPosition.OnChange := PositionChanged;
  FRadialTransform := TTransform.Create;
  FRadialTransform.OnChanged := PositionChanged;
  FPoints := TGradientPoints.Create(TGradientPoint);
  P := TGradientPoint(FPoints.Add);
  P.IntColor := $FF000000;
  P := TGradientPoint(FPoints.Add);
  P.IntColor := $FFFFFFFF;
  P.Offset := 1;
end;

procedure TGradient.ApplyOpacity(const AOpacity: Single);
var
  I: Integer;
begin
  if AOpacity < 1.0 then
    for I := 0 to FPoints.Count - 1 do
      FPoints[I].Color := MakeColor(FPoints[I].Color, AOpacity);
end;

procedure TGradient.Assign(Source: TPersistent);
var
  SaveChanged: TNotifyEvent;
begin
  if Source is TGradient then
  begin
    SaveChanged := FOnChanged;
    FOnChanged := nil;
    FPoints.Clear;
    FPoints.Assign(TGradient(Source).FPoints);
    FStyle := TGradient(Source).Style;
    if FStyle = TGradientStyle.gsLinear then
    begin
      FStopPosition.Assign(TGradient(Source).StopPosition);
      FStartPosition.Assign(TGradient(Source).StartPosition);
    end
    else
    begin
      FRadialTransform.Assign(TGradient(Source).RadialTransform);
    end;
    FOnChanged := SaveChanged;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end
  else
    inherited;
end;

destructor TGradient.Destroy;
begin
  FStartPosition.Free;
  FStopPosition.Free;
  FRadialTransform.Free;
  FPoints.Free;
  inherited;
end;

function TGradient.Equal(const AGradient: TGradient): Boolean;
var
  I: Integer;
begin
  Result := True;
  if FPoints.Count <> AGradient.FPoints.Count then Exit(False);
  if not SameValue(FStartPosition.X, AGradient.FStartPosition.X, TEpsilon.Position) then Exit(False);
  if not SameValue(FStartPosition.Y, AGradient.FStartPosition.Y, TEpsilon.Position) then Exit(False);
  if not SameValue(FStopPosition.X, AGradient.FStopPosition.X, TEpsilon.Position) then Exit(False);
  if not SameValue(FStopPosition.Y, AGradient.FStopPosition.Y, TEpsilon.Position) then Exit(False);
  for I := 0 to FPoints.Count - 1 do
  begin
    if FPoints[I].Color <> AGradient.FPoints[I].Color then Exit(False);
    if not SameValue(FPoints[I].Offset, AGradient.FPoints[I].Offset, TEpsilon.Position) then Exit(False);
  end;
end;

procedure TGradient.Change;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TGradient.InterpolateColor(X, Y: Single): TAlphaColor;
var
  A, B: TVector;
  Projection: Single;
begin
  case Style of
    TGradientStyle.gsLinear: begin
      A := (StopPosition.Vector - StartPosition.Vector);
      B := Vector(X, Y) - StartPosition.Vector;
      Projection := A.Normalize.DotProduct(B) / A.Length;
      Result := InterpolateColor(Projection);
    end;
    TGradientStyle.gsRadial: begin
      A := Vector(X, Y) - RadialTransform.RotationCenter.Vector;
      Result := InterpolateColor(1 - (A.Length * 2));
    end;
  else
    Result := 0;
  end;
end;

function TGradient.InterpolateColor(Offset: Single): TAlphaColor;
var
  I: Integer;
begin
  Result := 0;
  if FPoints.Count > 1 then
  begin
    if Offset < 0 then
      Offset := 0;
    if Offset > 1 then
      Offset := 1;
    if Offset < FPoints[0].Offset then
    begin
      Result := Points[0].IntColor;
      Exit;
    end;
    if Offset > FPoints[FPoints.Count - 1].Offset then
    begin
      Result := FPoints[FPoints.Count - 1].IntColor;
      Exit;
    end;
    for I := 0 to FPoints.Count - 2 do
    begin
      if (Offset < Points[I].Offset) then
        Continue;
      if Offset > Points[I + 1].Offset then
        Continue;
      if Points[I + 1].Offset - Points[I].Offset <= 0 then
        Result := Points[I].IntColor
      else if (I = FPoints.Count - 2) and (Offset > Points[Points.Count - 1].Offset) then // last
        Result := Points[Points.Count - 1].IntColor
      else
        Result := FMX.Types.InterpolateColor(Points[I].IntColor, Points[I + 1].IntColor,
          (Offset - Points[I].Offset) / (Points[I + 1].Offset - Points[I].Offset));
    end;
  end;
end;

procedure TGradient.PositionChanged(Sender: TObject);
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TGradient.IsLinearStored: Boolean;
begin
  Result := FStyle = TGradientStyle.gsLinear;
end;

function TGradient.IsRadialStored: Boolean;
begin
  Result := FStyle = TGradientStyle.gsRadial;
end;

procedure TGradient.SetRadialTransform(const Value: TTransform);
begin
  FRadialTransform.Assign(Value);
end;

procedure TGradient.SetStartPosition(const Value: TPosition);
begin
  FStartPosition.Assign(Value);
end;

procedure TGradient.SetStopPosition(const Value: TPosition);
begin
  FStopPosition.Assign(Value);
end;

procedure TGradient.SetColor(const Value: TAlphaColor);
begin
  if (FPoints.Count > 0) and (Points[0].Color <> Value) then
  begin
    Points[0].Color := Value;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end;
end;

procedure TGradient.SetColor1(const Value: TAlphaColor);
begin
  if (FPoints.Count > 1) and (Points[1].Color <> Value) then
  begin
    Points[1].Color := Value;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end;
end;

procedure TGradient.SetStyle(const Value: TGradientStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end;
end;

{ TBrushResource }

destructor TBrushResource.Destroy;
begin
  if Assigned(FStyleResource) then
  begin
    FStyleResource.RemoveFreeNotify(Self);
    FStyleResource := nil;
  end;
  inherited;
end;

procedure TBrushResource.FreeNotification(AObject: TObject);
begin
  if AObject = FStyleResource then
    FStyleResource := nil;
end;

procedure TBrushResource.Assign(Source: TPersistent);
begin
  if Source is TBrushResource then
  begin
    StyleResource := TBrushResource(Source).StyleResource;
    FStyleLookup := TBrushResource(Source).StyleLookup;
  end
  else
    inherited;
end;

procedure TBrushResource.SetStyleResource(const Value: TBrushObject);
begin
  if FStyleResource <> Value then
  begin
    if Assigned(FStyleResource) then
      FStyleResource.RemoveFreeNotify(Self);
    FStyleResource := Value;
    if Assigned(FStyleResource) then
    begin
      FStyleLookup := FStyleResource.StyleName;
      FStyleResource.AddFreeNotify(Self);
    end;
  end;
end;

function TBrushResource.GetStyleLookup: string;
begin
  Result := FStyleLookup;
end;

procedure TBrushResource.SetStyleLookup(const Value: string);
begin
  if Value <> FStyleLookup then
  begin
    FStyleLookup := Value;
  end;
end;

function TBrushResource.GetBrush: TBrush;
var
  O: TFmxObject;
begin
  Result := nil;
  if Assigned(FStyleResource) then
  begin
    Result := TBrushObject(FStyleResource).Brush;
  end
  else if FStyleLookup <> '' then
  begin
    O := FindStyleResource(FStyleLookup);
    if O is TBrushObject then
      StyleResource := TBrushObject(O);
    if Assigned(FStyleResource) then
      Result := TBrushObject(FStyleResource).Brush;
  end;
end;

{ TBrushBitmap }

constructor TBrushBitmap.Create;
begin
  inherited Create;
  FBitmap := TBitmap.Create(0, 0);
end;

destructor TBrushBitmap.Destroy;
begin
  FBitmap.DisposeOf;
  inherited;
end;

procedure TBrushBitmap.DoChanged;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TBrushBitmap.Assign(Source: TPersistent);
begin
  if Source is TBrushBitmap then
  begin
    FWrapMode := TBrushBitmap(Source).FWrapMode;
    FBitmap.Assign(TBrushBitmap(Source).FBitmap);
    DoChanged;
  end
  else
    inherited;
end;

procedure TBrushBitmap.SetWrapMode(const Value: TWrapMode);
begin
  if FWrapMode <> Value then
  begin
    FWrapMode := Value;
    DoChanged;
  end;
end;

procedure TBrushBitmap.SetBitmap(const Value: TBitmap);
begin
  FBitmap.Assign(Value);
  DoChanged;
end;

{ TBrush }

constructor TBrush.Create;
begin
  inherited Create;
  FDefaultKind := ADefaultKind;
  FDefaultColor := ADefaultColor;
  FColor := ADefaultColor;
  FKind := FDefaultKind;
  FGradient := TGradient.Create;
  FGradient.OnChanged := GradientChanged;
  FResource := TBrushResource.Create;
  FResource.OnChanged := ResourceChanged;
  FBitmap := TBrushBitmap.Create;
  FBitmap.OnChanged := BitmapChanged;
  FBitmap.Bitmap.OnChange := BitmapChanged;
end;

destructor TBrush.Destroy;
begin
  FBitmap.DisposeOf;
  FResource.DisposeOf;
  FGradient.DisposeOf;
  inherited;
end;

procedure TBrush.Assign(Source: TPersistent);
var
  SaveChange: TNotifyEvent;
begin
  if Source is TBrush then
  begin
    SaveChange := FOnChanged;
    FOnChanged := nil;
    FDefaultKind := TBrush(Source).FDefaultKind;
    FDefaultColor := TBrush(Source).FDefaultColor;
    FColor := TBrush(Source).FColor;
    FKind := TBrush(Source).FKind;
    case FKind of
      TBrushKind.bkGradient:
        FGradient.Assign(TBrush(Source).Gradient);
      TBrushKind.bkResource:
        FResource.Assign(TBrush(Source).Resource);
      TBrushKind.bkBitmap:
        FBitmap.Assign(TBrush(Source).Bitmap);
    end;
    FOnChanged := SaveChange;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end
  else
    inherited;
end;

procedure TBrush.GradientChanged(Sender: TObject);
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
  if Assigned(FOnGradientChanged) then
    FOnGradientChanged(Self);
end;

procedure TBrush.ResourceChanged(Sender: TObject);
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TBrush.BitmapChanged(Sender: TObject);
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TBrush.IsBitmapStored: Boolean;
begin
  Result := (FKind = TBrushKind.bkBitmap);
end;

function TBrush.IsColorStored: Boolean;
begin
  Result := (FKind = TBrushKind.bkSolid) and (FColor <> FDefaultColor);
end;

function TBrush.IsGradientStored: Boolean;
begin
  Result := FKind = TBrushKind.bkGradient;
end;

function TBrush.IsKindStored: Boolean;
begin
  Result := FKind <> FDefaultKind;
end;

function TBrush.IsResourceStored: Boolean;
begin
  Result := FKind = TBrushKind.bkResource;
end;

procedure TBrush.SetResource(const Value: TBrushResource);
begin
  FResource.Assign(Value);
end;

procedure TBrush.SetGradient(const Value: TGradient);
begin
  FGradient.Assign(Value);
end;

function TBrush.GetColor: TAlphaColor;
begin
  Result := FColor;
end;

procedure TBrush.SetColor(const Value: TAlphaColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    if FKind = TBrushKind.bkGradient then
      FGradient.Color := Value
    else if Assigned(FOnChanged) then
      FOnChanged(Self);
  end;
end;

procedure TBrush.SetKind(const Value: TBrushKind);
begin
  if FKind <> Value then
  begin
    FKind := Value;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end;
end;

{ TStrokeBrush.TDashData }

constructor TStrokeBrush.TDashData.Create(const ADashArray: TDashArray;
  ADashOffset: Single);
begin
  DashArray := ADashArray;
  DashOffset := ADashOffset;
end;

{ TStrokeBrush }

constructor TStrokeBrush.Create(const ADefaultKind: TBrushKind;
  const ADefaultColor: TAlphaColor);
begin
  inherited;
  FThickness := 1;
end;

function TStrokeBrush.GetDashArray: TDashArray;
begin
  Result := Copy(FDashArray);
end;

class function TStrokeBrush.GetStdDash(const Device: TDashDevice;
  const Dash: TStrokeDash): TDashData;
begin
  if not FStdDashCreated then
  begin
    // create the screen line dashes
    FStdDash[TDashDevice.ddScreen, TStrokeDash.sdSolid] :=
      TDashData.Create(nil, 0);
    FStdDash[TDashDevice.ddScreen, TStrokeDash.sdDash] :=
      TDashData.Create(TDashArray.Create(3, 1), 0);
    FStdDash[TDashDevice.ddScreen, TStrokeDash.sdDot] :=
      TDashData.Create(TDashArray.Create(1, 1), 0);
    FStdDash[TDashDevice.ddScreen, TStrokeDash.sdDashDot] :=
      TDashData.Create(TDashArray.Create(3, 1, 1, 1), 0);
    FStdDash[TDashDevice.ddScreen, TStrokeDash.sdDashDotDot] :=
      TDashData.Create(TDashArray.Create(3, 1, 1, 1, 1, 1), 0);
    FStdDash[TDashDevice.ddScreen, TStrokeDash.sdCustom] :=
      TDashData.Create(nil, 0);

    // create the printer line dashes
  {$IFDEF MACOS}
    // MacOS dashes work strange; these values are experimental values that
    // seem to work correctly
    FStdDash[TDashDevice.ddPrinter, TStrokeDash.sdSolid] :=
      TDashData.Create(nil, 0);
    FStdDash[TDashDevice.ddPrinter, TStrokeDash.sdDash] :=
      TDashData.Create(TDashArray.Create(3 * 2, 6 * 2), 0);
    FStdDash[TDashDevice.ddPrinter, TStrokeDash.sdDot] :=
      TDashData.Create(TDashArray.Create(1 * 2, 3 * 2), 0);
    FStdDash[TDashDevice.ddPrinter, TStrokeDash.sdDashDot] :=
      TDashData.Create(TDashArray.Create(3 * 3, 6 * 3, 1 * 3, 3 * 3), 0);
    FStdDash[TDashDevice.ddPrinter, TStrokeDash.sdDashDotDot] :=
      TDashData.Create(TDashArray.Create(3 * 2, 9 * 2, 1 * 2, 3 * 2, 1 * 2, 3 * 2), 0);
    FStdDash[TDashDevice.ddPrinter, TStrokeDash.sdCustom] :=
      TDashData.Create(nil, 0);
  {$ELSE}
    FStdDash[TDashDevice.ddPrinter] := FStdDash[TDashDevice.ddScreen];
  {$ENDIF}

    FStdDashCreated := True;
  end;

  Result.DashArray := Copy(FStdDash[Device, Dash].DashArray);
  Result.DashOffset := FStdDash[Device, Dash].DashOffset;
end;

procedure TStrokeBrush.Assign(Source: TPersistent);
var
  SaveChange: TNotifyEvent;
begin
  if Source is TStrokeBrush then
  begin
    SaveChange := FOnChanged;
    FOnChanged := nil;

    FDefaultKind := TStrokeBrush(Source).FDefaultKind;
    FDefaultColor := TStrokeBrush(Source).FDefaultColor;
    FColor := TStrokeBrush(Source).Color;
    FKind := TstrokeBrush(Source).Kind;
    case FKind of
      TBrushKind.bkGradient:
        FGradient.Assign(TStrokeBrush(Source).Gradient);
      TBrushKind.bkResource:
        FResource.Assign(TStrokeBrush(Source).Resource);
      TBrushKind.bkBitmap:
        FBitmap.Assign(TStrokeBrush(Source).Bitmap);
    end;

    FThickness := TStrokeBrush(Source).Thickness;
    FCap := TStrokeBrush(Source).Cap;
    FDash := TStrokeBrush(Source).Dash;
    FJoin := TStrokeBrush(Source).Join;
    FDashArray := Copy(TStrokeBrush(Source).FDashArray);
    FDashOffset := TStrokeBrush(Source).FDashOffset;
    FOnChanged := SaveChange;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end
  else
    inherited;
end;

procedure TStrokeBrush.SetCustomDash(const Dash: array of Single; Offset: Single);
var
  I: Integer;
begin
  FDash := TStrokeDash.sdCustom;
  SetLength(FDashArray, Length(Dash));
  for I := 0 to High(Dash) do
    FDashArray[I] := Dash[I];
  FDashOffset := Offset;
end;

function TStrokeBrush.IsThicknessStored: Boolean;
begin
  Result := Thickness <> 1;
end;

procedure TStrokeBrush.SetCap(const Value: TStrokeCap);
begin
  if FCap <> Value then
  begin
    FCap := Value;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end;
end;

procedure TStrokeBrush.SetDash(const Value: TStrokeDash);
begin
  if FDash <> Value then
  begin
    FDash := Value;
    case FDash of
      TStrokeDash.sdSolid:
        begin
          FDashOffset := 0;
          SetLength(FDashArray, 0);
        end;
      TStrokeDash.sdDash:
        begin
          FDashOffset := 0;
          SetLength(FDashArray, 2);
          FDashArray[0] := 1 * 3;
          FDashArray[1] := 1;
        end;
      TStrokeDash.sdDot:
        begin
          FDashOffset := 0;
          SetLength(FDashArray, 2);
          FDashArray[0] := 1;
          FDashArray[1] := 1;
        end;
      TStrokeDash.sdDashDot:
        begin
          FDashOffset := 0;
          SetLength(FDashArray, 4);
          FDashArray[0] := 1 * 3;
          FDashArray[1] := 1;
          FDashArray[2] := 1;
          FDashArray[3] := 1;
        end;
      TStrokeDash.sdDashDotDot:
        begin
          FDashOffset := 0;
          SetLength(FDashArray, 6);
          FDashArray[0] := 1 * 3;
          FDashArray[1] := 1;
          FDashArray[2] := 1;
          FDashArray[3] := 1;
          FDashArray[4] := 1;
          FDashArray[5] := 1;
        end;
      TStrokeDash.sdCustom:
        ;
    else
      FDashOffset := 0;
      SetLength(FDashArray, 0);
    end;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end;
end;

procedure TStrokeBrush.SetJoin(const Value: TStrokeJoin);
begin
  if FJoin <> Value then
  begin
    FJoin := Value;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end;
end;

procedure TStrokeBrush.SetThickness(const Value: Single);
begin
  if FThickness <> Value then
  begin
    FThickness := Value;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end;
end;

{ TFont }

constructor TFont.Create;
begin
  inherited;
  FUpdating := True;
  try
    Assign(nil);
  finally
    FUpdating := False;
  end;
end;

procedure TFont.DoChanged;
begin
  if (not FUpdating) and Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TFont.Equals(Obj: TObject): Boolean;
begin
  Result := (Obj is TFont) and
            (SameValue(Size, TFont(Obj).Size, TEpsilon.FontSize)) and
            (Family = TFont(Obj).Family) and
            (Style = TFont(Obj).Style);
end;

function TFont.DefaultFamily: string;
var
  FontSvc: IFMXSystemFontService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXSystemFontService, IInterface(FontSvc)) then
    Result := FontSvc.GetDefaultFontFamilyName
  else
    Result := DefaultFontFamily;
end;

procedure TFont.Assign(Source: TPersistent);
var LFamily: string;
    LSize: single;
    LStyle: TFontStyles;
begin
  if (Source is TFont) or (not Assigned(Source)) then
  begin
    if Assigned(Source) then
    begin
      LFamily := TFont(Source).Family;
      LSize := TFont(Source).Size;
      LStyle := TFont(Source).Style;
    end
    else
    begin
      LFamily := DefaultFamily;
      LSize := DefaultFontSize;
      LStyle := [];
    end;
    if (FFamily <> LFamily) or
       (not SameValue(FSize, LSize, TEpsilon.FontSize)) or
       (FStyle <> LStyle) then
    begin
      FFamily := LFamily;
      FSize := LSize;
      FStyle := LStyle;
      DoChanged;
    end;
  end
  else
    inherited;
end;

function TFont.IsSizeStored: Boolean;
begin
  Result := not SameValue(FSize, DefaultFontSize, TEpsilon.FontSize);
end;

function TFont.IsFamilyStored: Boolean;
begin
  Result := FFamily <> DefaultFamily;
end;

procedure TFont.SetFamily(const Value: TFontName);
begin
  if FFamily <> Value then
  begin
    FFamily := Value;
    DoChanged;
  end;
end;

procedure TFont.SetSettings(const AFamily: string; const ASize: Single; const AStyle: TFontStyles);
begin
  FFamily := AFamily;
  FSize := Max(1, ASize);
  FStyle := AStyle;
  DoChanged;
end;

procedure TFont.SetSize(const Value: Single);
var LSize: Single;
begin
  LSize := Max(1, Value);
  if FSize <> LSize then
  begin
    FSize := LSize;
    DoChanged;
  end;
end;

procedure TFont.SetStyle(const Value: TFontStyles);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    DoChanged;
  end;
end;

{ TTextSettings }

type
  TSettingsFont = class (TFont)
  private
    FTextSettings: TTextSettings;
  protected
    procedure DoChanged; override;
  public
    constructor Create(const ATextSettings: TTextSettings);
    property TextSettings: TTextSettings read FTextSettings;
  end;

{ TSettingsFont }

constructor TSettingsFont.Create(const ATextSettings: TTextSettings);
begin
  inherited Create;
  FTextSettings := ATextSettings;
end;

procedure TSettingsFont.DoChanged;
begin
  if Assigned(FTextSettings) then
  begin
    FTextSettings.IsAdjustChanged := True;
    FTextSettings.Change;
  end;
  inherited;
end;

constructor TTextSettings.Create(const AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
  BeginUpdate;
  try
    Assign(nil);
  finally
    FIsChanged := False;
    FIsAdjustChanged := False;
    EndUpdate;
  end;
end;

destructor TTextSettings.Destroy;
begin
  TSettingsFont(FFont).FTextSettings := nil;
  FreeAndNil(FFont);
  inherited;
end;

procedure TTextSettings.Assign(Source: TPersistent);
begin
  if (not Assigned(Source)) or (Source is TTextSettings) then
  begin
    BeginUpdate;
    try
      if not Assigned(Source) then
      begin
        FreeAndNil(FFont);
        FFont := TSettingsFont.Create(self);
        FontColor := TAlphaColorRec.Black;
        HorzAlign := TTextAlign.taLeading;
        VertAlign := TTextAlign.taCenter;
        Trimming := TTextTrimming.ttNone;
        WordWrap := False;
      end
      else
      begin
        FFont.Assign(TTextSettings(Source).Font);
        FontColor := TTextSettings(Source).FontColor;
        HorzAlign := TTextSettings(Source).HorzAlign;
        VertAlign := TTextSettings(Source).VertAlign;
        WordWrap := TTextSettings(Source).WordWrap;
        Trimming := TTextSettings(Source).Trimming;
      end;
    finally
      EndUpdate;
    end;
  end
  else
    inherited;
end;

function TTextSettings.Equals(Obj: TObject): Boolean;
var Source: TTextSettings;
begin
  Result := Obj is TTextSettings;
  if Result then
  begin
    Source := TTextSettings(Obj);
    Result := (HorzAlign = Source.HorzAlign) and
              (VertAlign = Source.VertAlign) and
              (WordWrap = Source.WordWrap) and
              (FontColor = Source.FontColor) and
              (Trimming = Source.Trimming) and
              (((Font = nil) and (Source.Font = nil)) or
               (Assigned(Font) and Font.Equals(Source.Font)));
  end;
end;

function TTextSettings.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TTextSettings.AssignNoStyled(const TextSettings: TTextSettings;
                                       const StyledSettings: TStyledSettings);
var Tmp: TTextSettings;
begin
  if StyledSettings <> AllStyledSettings then
  begin
    if StyledSettings = [] then
      Assign(TextSettings)
    else
    begin
      if Assigned(TextSettings) then
        Tmp := TextSettings
      else
        Tmp := TTextSettings.Create(nil);
      try
        BeginUpdate;
        try
          if not (TStyledSetting.ssFamily in StyledSettings) then
            Font.Family := Tmp.Font.FFamily;
          if not (TStyledSetting.ssSize in StyledSettings) then
            Font.Size := Tmp.Font.Size;
          if not (TStyledSetting.ssStyle in StyledSettings) then
            Font.Style := Tmp.Font.Style;
          if not (TStyledSetting.ssFontColor in StyledSettings) then
            FontColor := Tmp.FontColor;
          if not (TStyledSetting.ssOther in StyledSettings) then
          begin
            HorzAlign := Tmp.HorzAlign;
            VertAlign := Tmp.VertAlign;
            WordWrap := Tmp.WordWrap;
            Trimming := Tmp.Trimming;
          end;
        finally
          EndUpdate;
        end;
      finally
        if not Assigned(TextSettings) then
          FreeAndNil(Tmp);
      end;
    end;
  end;
end;

procedure TTextSettings.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TTextSettings.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if (FUpdateCount = 0) and (FIsChanged or FIsAdjustChanged) then
    begin
      try
        DoChanged;
      finally
        FIsChanged := False;
        FIsAdjustChanged := False;
      end;
    end;
  end;
end;

procedure TTextSettings.Change;
begin
  FIsChanged := True;
  if (FUpdateCount = 0) then
  begin
    try
      DoChanged;
    finally
      FIsChanged := False;
      FIsAdjustChanged := False;
    end;
  end;
end;

procedure TTextSettings.DoChanged;
begin
  if Assigned(OnChanged) then
    OnChanged(self);
end;

procedure TTextSettings.SetFont(const Value: TFont);
begin
  if not (((Font = nil) and (Value = nil)) or
          ((Assigned(Font) and Font.Equals(Value)))) then
  begin
    Font.Assign(Value);
    IsAdjustChanged := True;
    Change;
  end;
end;

procedure TTextSettings.SetFontColor(const Value: TAlphaColor);
begin
  if FFontColor <> Value then
  begin
    FFontColor := Value;
    Change;
  end;
end;

procedure TTextSettings.SetHorzAlign(const Value: TTextAlign);
begin
  if FHorzAlign <> Value then
  begin
    FHorzAlign := Value;
    IsAdjustChanged := True;
    Change;
  end;
end;

procedure TTextSettings.SetVertAlign(const Value: TTextAlign);
begin
  if FVertAlign <> Value then
  begin
    FVertAlign := Value;
    IsAdjustChanged := True;
    Change;
  end;
end;

procedure TTextSettings.SetTrimming(const Value: TTextTrimming);
begin
  if FTrimming <> Value then
  begin
    FTrimming := Value;
    IsAdjustChanged := True;
    Change;
  end;
end;

procedure TTextSettings.SetWordWrap(const Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    IsAdjustChanged := True;
    Change;
  end;
end;

{ TBitmapCodecManager }

class procedure TBitmapCodecManager.UnInitialize;
begin
  FreeAndNil(FBitmapCodecClassDescriptors);
end;

class function TBitmapCodecManager.FindBitmapCodecDescriptor(const Name: string;
  const Field: TBitmapCodecDescriptorField): TBitmapCodecClassDescriptor;
var
  LResult: Boolean;
  LDescriptor: TBitmapCodecClassDescriptor;
begin
  FillChar(Result, SizeOf(Result), 0);
  if Assigned(FBitmapCodecClassDescriptors) then
    for LDescriptor in FBitmapCodecClassDescriptors do
    begin
      case Field of
        TBitmapCodecDescriptorField.Extension: LResult := SameText(Name, LDescriptor.Extension, loUserLocale);
        TBitmapCodecDescriptorField.Description: LResult := SameText(Name, LDescriptor.Description, loUserLocale);
      else
        LResult := False;
      end;
      if LResult then
        Result := LDescriptor;
    end;
end;

class procedure TBitmapCodecManager.RegisterBitmapCodecClass(const Extension, Description: string; const CanSave: Boolean;
  const BitmapCodecClass: TCustomBitmapCodecClass);
var
  LDescriptor: TBitmapCodecClassDescriptor;
begin
  if Not Assigned(FBitmapCodecClassDescriptors) then
    FBitmapCodecClassDescriptors := TList<TBitmapCodecClassDescriptor>.Create;

  LDescriptor.Extension := Extension;
  LDescriptor.Description := Description;
  LDescriptor.BitmapCodecClass := BitmapCodecClass;
  LDescriptor.CanSave := CanSave;
  FBitmapCodecClassDescriptors.Add(LDescriptor);
end;

class function TBitmapCodecManager.IsCodedExists(const AFileName: string): Boolean;
begin
  Result := Assigned(FindBitmapCodecDescriptor(ExtractFileExt(AFileName), TBitmapCodecDescriptorField.Extension).BitmapCodecClass);
end;

class function TBitmapCodecManager.GetFileTypes: string;
var
  Descriptor: TBitmapCodecClassDescriptor;
begin
  Result := '';
  if Assigned(FBitmapCodecClassDescriptors) then
    for Descriptor in FBitmapCodecClassDescriptors do
      if Result = '' then
        Result := '*' + Descriptor.Extension
      else
        Result := Result + ';' + '*' + Descriptor.Extension;
end;

class function TBitmapCodecManager.GetFilterString: string;
var
  Descriptor: TBitmapCodecClassDescriptor;
begin
  Result := '';
  if Assigned(FBitmapCodecClassDescriptors) then
  begin
    for Descriptor in FBitmapCodecClassDescriptors do
      if Result = '' then
        Result := Descriptor.Description + ' (' + '*' + Descriptor.Extension + ')|' + '*' + Descriptor.Extension
      else
        Result := Result + '|' + Descriptor.Description + ' (' + '*' + Descriptor.Extension + ')|' + '*' + Descriptor.Extension;
    // all files
    Result := SVAllFiles + ' (' + GetFileTypes + ')|' + GetFileTypes + '|' + Result;
  end;
end;

class function TBitmapCodecManager.GetImageSize(const AFileName: string): TPointF;
var
  CodecClass: TCustomBitmapCodecClass;
begin
  CodecClass := FindBitmapCodecDescriptor(ExtractFileExt(AFileName), TBitmapCodecDescriptorField.Extension).BitmapCodecClass;
  if Assigned(CodecClass) then
    Result := CodecClass.GetImageSize(AFileName)
  else
    Result := TPointF.Create(0, 0);
end;

class function TBitmapCodecManager.LoadFromFile(const AFileName: string; const Bitmap: TBitmapSurface): Boolean;
var
  CodecClass: TCustomBitmapCodecClass;
  Codec: TCustomBitmapCodec;
begin
  CodecClass := FindBitmapCodecDescriptor(ExtractFileExt(AFileName), TBitmapCodecDescriptorField.Extension).BitmapCodecClass;
  if Assigned(CodecClass) then
  begin
    Codec := CodecClass.Create;
    try
      Result := Codec.LoadFromFile(AFileName, Bitmap);
    finally
      Codec.Free;
    end;
  end
  else
    Result := False;
end;

class function TBitmapCodecManager.LoadThumbnailFromFile(const AFileName: string; const AFitWidth, AFitHeight: Single;
  const UseEmbedded: Boolean; const Bitmap: TBitmapSurface): Boolean;
var
  CodecClass: TCustomBitmapCodecClass;
  Codec: TCustomBitmapCodec;
begin
  CodecClass := FindBitmapCodecDescriptor(ExtractFileExt(AFileName), TBitmapCodecDescriptorField.Extension).BitmapCodecClass;
  if Assigned(CodecClass) then
  begin
    Codec := CodecClass.Create;
    try
      Result := Codec.LoadThumbnailFromFile(AFileName, AFitWidth, AFitHeight, UseEmbedded, Bitmap);
    finally
      Codec.Free;
    end;
  end
  else
    Result := False;
end;

class function TBitmapCodecManager.LoadFromStream(const AStream: TStream; const Bitmap: TBitmapSurface): Boolean;
var
  Codec: TCustomBitmapCodec;
  Descriptor: TBitmapCodecClassDescriptor;
begin
  Result := False;
  if Assigned(FBitmapCodecClassDescriptors) then
    for Descriptor in FBitmapCodecClassDescriptors do
      if Descriptor.BitmapCodecClass.IsValid(AStream) then
      begin
        Codec := Descriptor.BitmapCodecClass.Create;
        try
          Result := Codec.LoadFromStream(AStream, Bitmap);
        finally
          Codec.Free;
        end;
        Break;
      end;
end;

class function TBitmapCodecManager.SaveToFile(const AFileName: string; const Bitmap: TBitmapSurface;
  const SaveParams: PBitmapCodecSaveParams = nil): Boolean;
var
  Codec: TCustomBitmapCodec;
  Descriptor: TBitmapCodecClassDescriptor;
begin
  Result := False;
  if Assigned(FBitmapCodecClassDescriptors) then
    for Descriptor in FBitmapCodecClassDescriptors do
      if SameText(ExtractFileExt(AFileName), Descriptor.Extension, loUserLocale) and Descriptor.CanSave then
      begin
        Codec := Descriptor.BitmapCodecClass.Create;
        try
          Result := Codec.SaveToFile(AFileName, Bitmap, SaveParams);
        finally
          Codec.Free;
        end;
      end;
end;

class function TBitmapCodecManager.SaveToStream(const AStream: TStream; const Bitmap: TBitmapSurface; const Extension: string;
  SaveParams: PBitmapCodecSaveParams = nil): Boolean;
var
  Codec: TCustomBitmapCodec;
  Descriptor: TBitmapCodecClassDescriptor;
begin
  Result := False;
  if Assigned(FBitmapCodecClassDescriptors) then
    for Descriptor in FBitmapCodecClassDescriptors do
      if (SameText(Extension, Descriptor.Extension, loUserLocale) or
        SameText('.' + Extension, Descriptor.Extension, loUserLocale)) and
        Descriptor.CanSave then
      begin
        Codec := Descriptor.BitmapCodecClass.Create;
        try
          Result := Codec.SaveToStream(AStream, Bitmap, Descriptor.Extension, SaveParams);
        finally
          Codec.Free;
        end;
      end;
end;

{ TBitmapData }

constructor TBitmapData.Create(const AWidth, AHeight: Integer;
  const APixelFormat: TPixelFormat);
begin
  Self.FWidth := AWidth;
  Self.FHeight := AHeight;
  Self.FPixelFormat := APixelFormat;
end;

function TBitmapData.GetBytesPerLine: Integer;
begin
  Result := Width * BytesPerPixel;
end;

function TBitmapData.GetBytesPerPixel: Integer;
begin
  Result := GetPixelFormatBytes(PixelFormat);
end;

function TBitmapData.GetPixel(const X, Y: Integer): TAlphaColor;
begin
  Result := PixelToAlphaColor(@PLongByteArray(Data)[Y * Pitch + (X * BytesPerPixel)], PixelFormat);
end;

function TBitmapData.GetPixelAddr(const I, J: Integer): Pointer;
begin
  Result := @PLongByteArray(Data)[J * Pitch + I * BytesPerPixel];
end;

function TBitmapData.GetScanline(const I: Integer): Pointer;
begin
  Result := @PLongByteArray(Data)[I * Pitch];
end;

procedure TBitmapData.Copy(const Source: TBitmapData);
var
  I: Integer;
begin
  if (Source.Pitch = Pitch) and (Pitch = Width * GetPixelFormatBytes(Source.PixelFormat)) then
      Move(Source.Data^, Data^, Pitch * Height)
  else begin
    for I := 0 to Height - 1 do
      Move(PLongByteArray(Source.Data)[I * Source.Pitch], PLongByteArray(Data)[I * Pitch], BytesPerLine)
  end;
end;

procedure TBitmapData.SetPixel(const X, Y: Integer; const AColor: TAlphaColor);
begin
  AlphaColorToPixel(AColor, @PLongByteArray(Data)[Y * Pitch + (X * BytesPerPixel)], PixelFormat);
end;

{ TBitmap }

constructor TBitmap.Create;
begin
  inherited;
  FBitmapScale := 1;
end;

constructor TBitmap.Create(const AWidth, AHeight: Integer);
begin
  inherited Create;
  FBitmapScale := 1;
  FWidth := AWidth;
  FHeight := AHeight;
end;

constructor TBitmap.CreateFromStream(const AStream: TStream);
begin
  inherited Create;
  LoadFromStream(AStream);
end;

constructor TBitmap.CreateFromFile(const AFileName: string);
begin
  inherited Create;
  LoadFromFile(AFileName);
end;

constructor TBitmap.CreateFromBitmapAndMask(const Bitmap, Mask: TBitmap);
var
  I, J: Integer;
  D, B, M: TBitmapData;
  C: TAlphaColor;
begin
  Create(Bitmap.Width, Bitmap.Height);
  if (Bitmap.Width <> Mask.Width) or (Bitmap.Height <> Mask.Height) then
    Exit;
  if Map(TMapAccess.maWrite, D) and Bitmap.Map(TMapAccess.maRead, B) and Mask.Map(TMapAccess.maRead, M) then
  try
    for I := 0 to FWidth - 1 do
      for J := 0 to FHeight - 1 do
      begin
        C := B.GetPixel(I, J);
        TAlphaColorRec(C).A := TAlphaColorRec(C).R;
        D.SetPixel(I, J, C);
      end;
  finally
    Unmap(D);
    Bitmap.Unmap(B);
    Mask.Unmap(M);
  end;
end;

destructor TBitmap.Destroy;
var
  I: Integer;
  BitmapObject: IBitmapObject;
begin
  if Assigned(FNotifyList) then
  begin
    for I := FNotifyList.Count - 1 downto 0 do
      IFreeNotification(FNotifyList[I]).FreeNotification(Self);
    FNotifyList.DisposeOf;
  end;
  if Assigned(FResource) then
  begin
    if Supports(FResource, IBitmapObject, BitmapObject) then
      BitmapObject.RemoveFreeNotify(Self);
    FResource := nil;
  end;
  if not (TCanvasStyle.NeedGPUSurface in CanvasClass.GetCanvasStyle) and Assigned(FTexture) then
    FTexture.DisposeOf;
  FTexture := nil;
  if Assigned(FCanvas) then
    FCanvas.DisposeOf;
  FCanvas := nil;
  CanvasClass.FinalizeBitmap(Self);
  inherited;
end;

function TBitmap.GetHandle: TBitmapHandle;
begin
  if FHandle = 0 then
    CanvasClass.InitializeBitmap(Self);
  Result := FHandle;
end;

function TBitmap.GetPixelFormat: TPixelFormat;
begin
  if HandleAllocated then
    Result := FPixelFormat
  else
    Result := TPixelFormat.pfUnknown;
end;

procedure TBitmap.AddFreeNotify(const AObject: IFreeNotification);
begin
  if not Assigned(FNotifyList) then
    FNotifyList := TList<Pointer>.Create;
  FNotifyList.Add(Pointer(AObject));
end;

procedure TBitmap.RemoveFreeNotify(const AObject: IFreeNotification);
begin
  if Assigned(FNotifyList) then
    FNotifyList.Remove(Pointer(AObject));
end;

function TBitmap.GetStyleLookup: string;
begin
  Result := FStyleLookup;
end;

function TBitmap.HandleAllocated: Boolean;
begin
  Result := Handle <> 0;
end;

procedure TBitmap.SetStyleLookup(const Value: string);
var
  BitmapObject: IBitmapObject;
begin
  if Value <> FStyleLookup then
  begin
    if Assigned(FResource) then
    begin
      if Supports(FResource, IBitmapObject, BitmapObject) then
        BitmapObject.RemoveFreeNotify(Self);
      FResource := nil;
    end;
    FStyleLookup := Value;
    DoChange;
  end;
end;

procedure TBitmap.SetResourceBitmap(const ABitmap: TBitmap);
var
  BitmapObject: IBitmapObject;
begin
  if Assigned(FResource) then
  begin
    if Supports(FResource, IBitmapObject, BitmapObject) then
      BitmapObject.RemoveFreeNotify(Self);
  end;
  FResource := ABitmap;
  if Assigned(FResource) then
  begin
    if Supports(FResource, IBitmapObject, BitmapObject) then
      BitmapObject.AddFreeNotify(Self);
  end;
  DoChange;
end;

procedure TBitmap.SetWidth(const Value: Integer);
begin
  SetSize(Value, Height);
end;

function TBitmap.GetBitmap: TBitmap;
var
  BitmapObject: IBitmapObject;
  MultiResObject: IMultiResBitmapObject;
begin
  Result := nil;
  if Assigned(FResource) then
  begin
    if Supports(FResource, IMultiResBitmapObject, MultiResObject) then
      Result := MultiResObject.GetBitmapByScale(BitmapScale)
    else if Supports(FResource, IBitmapObject, BitmapObject) then
      Result := BitmapObject.Bitmap
    else
      if FResource is TBitmap then
        Result := TBitmap(FResource);
  end
  else
  if FStyleLookup <> '' then
  begin
    FResource := FindStyleResource(FStyleLookup);
    if Supports(FResource, IMultiResBitmapObject, MultiResObject) then
      Result := MultiResObject.GetBitmapByScale(BitmapScale)
    else if Supports(FResource, IBitmapObject, BitmapObject) then
      Result := BitmapObject.Bitmap;
  end;
end;

function TBitmap.GetBytesPerLine: Integer;
begin
  Result := BytesPerPixel * Width;
end;

function TBitmap.GetBytesPerPixel: Integer;
begin
  Result := GetPixelFormatBytes(FPixelFormat);
end;

procedure TBitmap.SetBitmapScale(const Scale: Single);
begin
  FBitmapScale := Scale;
end;

procedure TBitmap.SetHandle(const Handle: TBitmapHandle);
begin
  FHandle := Handle;
end;

procedure TBitmap.SetHeight(const Value: Integer);
begin
  SetSize(Width, Value);
end;

procedure TBitmap.SetPixelFormat(const PixelFormat: TPixelFormat);
begin
  FPixelFormat := PixelFormat;
end;

procedure TBitmap.SetSize(const AWidth, AHeight: Integer);
begin
  if (FWidth <> AWidth) or (FHeight <> AHeight) then
  begin
    FWidth := AWidth;
    FHeight := AHeight;
    if FWidth < 0 then FWidth := 0;
    if FHeight < 0 then FHeight := 0;
    Recreate;
    BitmapChanged;
  end;
end;

procedure TBitmap.Recreate;
begin
  if not (TCanvasStyle.NeedGPUSurface in CanvasClass.GetCanvasStyle) and Assigned(FTexture) then
    FTexture.DisposeOf;
  FTexture := nil;
  if Assigned(FCanvas) then
    FCanvas.DisposeOf;
  FCanvas := nil;
  CanvasClass.FinalizeBitmap(Self);
  CanvasClass.InitializeBitmap(Self);
end;

function TBitmap.GetCanvasClass: TCanvasClass;
begin
  if not Assigned(FCanvasClass) then
    FCanvasClass := TCanvasManager.GetDefaultCanvas;
  Result := FCanvasClass;
end;

procedure TBitmap.Clear(const AColor: TAlphaColor);
begin
  ClearRect(RectF(0, 0, FWidth, FHeight), AColor);
end;

procedure TBitmap.ClearRect(const ARect: TRectF; const AColor: TAlphaColor);
var
  R: TRectF;
  M: TBitmapData;
  C: Cardinal;
begin
  R := ARect;
  if R.Left < 0 then
    R.Left := 0;
  if R.Top < 0 then
    R.Top := 0;
  if R.Right > FWidth then
    R.Right := FWidth;
  if R.Bottom > FHeight then
    R.Bottom := FHeight;
  if R.Bottom < R.Top then
    R.Bottom := R.Top;
  if R.Right < R.Left then
    R.Right := R.Left;
  if (R.Right < 0) or (R.Top < 0) or (R.Left > FWidth) or (R.Top > FHeight) then
    Exit;
  if Map(TMapAccess.maWrite, M) then
  try
    AlphaColorToPixel(PremultiplyAlpha(AColor), @C, FPixelFormat);
    FillLongwordRect(M.Data, M.Pitch div 4, FHeight, trunc(R.Left), trunc(R.Top), trunc(R.Right), trunc(R.Bottom), C);
  finally
    Unmap(M);
  end;
end;

procedure TBitmap.CopyFromBitmap(const Source: TBitmap);
begin
  TCanvas.CopyBitmap(Source, Self);
  BitmapChanged;
end;

procedure TBitmap.CopyFromBitmap(const Source: TBitmap; SrcRect: TRect; DstX, DstY: Integer);
var
  I: Integer;
  S, D: TBitmapData;
  DstR: TRect;
begin
  if Map(TMapAccess.maWrite, D) then
  begin
    if Source.Map(TMapAccess.maRead, S) then
    begin
      if (DstX = 0) and (DstY = 0) and (SrcRect.Left = 0) and (SrcRect.Top = 0) and (SrcRect.Width = Width) and
         (SrcRect.Height = Height) and (D.Pitch <> Width * 4) and (D.Pitch = S.Pitch) then
        Move(S.Data^, D.Data^, D.Pitch * Height)
      else
      begin
        IntersectRect(SrcRect, Rect(0, 0, Source.Width, Source.Height));
        DstR := Rect(DstX, DstY, DstX + SrcRect.Width, DstY + SrcRect.Height);
        IntersectRect(DstR, Rect(0, 0, Width, Height));
        if (DstR.Width = SrcRect.Width) and (DstR.Height = SrcRect.Height) then
          for I := SrcRect.Top to SrcRect.Bottom - 1 do
            Move(PAlphaColorArray(S.Data)[SrcRect.Left + (I * (S.Pitch div 4))],
              PAlphaColorArray(D.Data)[DstY + (I - SrcRect.Top) * (D.Pitch div 4) + DstX], SrcRect.Width * 4);
      end;
      Source.Unmap(S);
    end;
    Unmap(D);
  end;
end;

procedure TBitmap.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBitmap.BitmapChanged;
begin
  FTextureNeedUpdate := True;
  DoChange;
end;

function TBitmap.IsEmpty: Boolean;
begin
  Result := FWidth * FHeight = 0;
end;

procedure TBitmap.Assign(Source: TPersistent);
var
  D: TBitmapData;
  I: Integer;
begin
  if Source is TBitmap then
  begin
    if Assigned(TBitmap(Source).ResourceBitmap) then
    begin
      FStyleLookup := TBitmap(Source).FStyleLookup;
      BitmapChanged;
    end else
    begin
      FResource := nil;
      FStyleLookup := '';
      SetSize(TBitmap(Source).Width, TBitmap(Source).Height);
      CopyFromBitmap(TBitmap(Source));
    end;
  end else if Source is TBitmapSurface then
  begin
    FResource := nil;
    FStyleLookup := '';
    SetSize(TBitmapSurface(Source).Width, TBitmapSurface(Source).Height);
    if Map(TMapAccess.maWrite, D) then
    try
      if D.Pitch = TBitmapSurface(Source).Pitch then
        Move(TBitmapSurface(Source).Bits^, D.Data^, TBitmapSurface(Source).Pitch * Height)
      else
        for I := 0 to TBitmapSurface(Source).Height - 1 do
          Move(TBitmapSurface(Source).Scanline[I]^, Pointer(NativeInt(D.Data) + I * D.Pitch)^, Width * TBitmapSurface(Source).BytesPerPixel);
    finally
      Unmap(D);
    end;
  end else if Source = nil then
  begin
    Clear(TAlphaColorRec.White);
  end
  else
    inherited;
end;

procedure TBitmap.AssignTo(Dest: TPersistent);
var
  I: Integer;
  M: TBitmapData;
begin
  if Dest is TBitmapSurface then
  begin
    TBitmapSurface(Dest).SetSize(Width, Height, PixelFormat);
    if Map(TMapAccess.maRead, M) then
    try
      if TBitmapSurface(Dest).Pitch <> M.Pitch then
      begin
        for I := 0 to Height - 1 do
          Move(PAlphaColorArray(M.Data)[I * (M.Pitch div 4)],
            TBitmapSurface(Dest).Scanline[I]^, TBitmapSurface(Dest).Pitch);
      end
      else
        Move(M.Data^, TBitmapSurface(Dest).Bits^, TBitmapSurface(Dest).Pitch * Height);
    finally
      Unmap(M);
    end;
  end else
    inherited;
end;

procedure TBitmap.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('PNG', ReadBitmap, WriteBitmap, FWidth * FHeight > 0);
end;

procedure TBitmap.ReadBitmap(Stream: TStream);
begin
  LoadFromStream(Stream);
end;

procedure TBitmap.WriteBitmap(Stream: TStream);
begin
  SaveToStream(Stream);
end;

procedure TBitmap.Rotate(const Angle: Single);
var
  temp: TBitmap;
  M, M2: TMatrix;
  Pts: array of TPointF;
  R: TRectF;
begin
  if Angle = 0 then
    Exit;

  M := TMatrix.Identity;
  M.m31 := -FWidth / 2;
  M.m32 := -FHeight / 2;
  M := M * TMatrix.CreateRotation(DegToRad(Angle));
  { calc new size }
  SetLength(Pts, 4);
  Pts[0] := PointF(0, 0) * M;
  Pts[1] := PointF(FWidth, 0) * M;
  Pts[2] := PointF(FWidth, FHeight) * M;
  Pts[3] := PointF(0, FHeight) * M;
  R := NormalizeRectF(Pts);
  { translate }
  M2 := TMatrix.Identity;
  M2.m31 := RectWidth(R) / 2;
  M2.m32 := RectHeight(R) / 2;
  M := M * M2;
  { rotate }
  temp := TBitmap.Create(trunc(RectWidth(R)), trunc(RectHeight(R)));
  try
    if temp.Canvas.BeginScene then
    try
      temp.Canvas.Clear(0);
      temp.Canvas.SetMatrix(M);
      temp.Canvas.DrawBitmap(Self, RectF(0, 0, FWidth, FHeight), RectF(0, 0, FWidth, FHeight), 1);
    finally
      temp.Canvas.EndScene;
    end;
    Assign(temp);
  finally
    temp.DisposeOf;
  end;
end;

procedure TBitmap.FlipHorizontal;
var
  I, J: Integer;
  tmp: TAlphaColor;
  M: TBitmapData;
begin
  if Map(TMapAccess.maReadWrite, M) then
  try
    for j := 0 to Height - 1 do
      for I := 0 to (Width - 1) div 2 do
      begin
        tmp := PAlphaColorArray(M.Data)[j * (M.Pitch div 4) + Width - 1 - I];
        PAlphaColorArray(M.Data)[j * (M.Pitch div 4) + Width - 1 - I] := PAlphaColorArray(M.Data)[j * (M.Pitch div 4) + I];
        PAlphaColorArray(M.Data)[j * (M.Pitch div 4) + I] := tmp;
      end;
  finally
    Unmap(M);
  end;
end;

procedure TBitmap.FlipVertical;
var
  I: Integer;
  tmp: PAlphaColorArray;
  M: TBitmapData;
begin
  GetMem(tmp, Width * 4);
  if Map(TMapAccess.maReadWrite, M) then
  try
    for I := 0 to (Height - 1) div 2 do
    begin
      System.Move(PAlphaColorArray(M.Data)[(Height - 1 - I) * (M.Pitch div 4)], tmp[0], M.Pitch);
      System.Move(PAlphaColorArray(M.Data)[I * (M.Pitch div 4)], PAlphaColorArray(M.Data)[(Height - 1 - I) * (M.Pitch div 4)], M.Pitch);
      System.Move(tmp[0], PAlphaColorArray(M.Data)[I * (M.Pitch div 4)], M.Pitch);
    end;
  finally
    Unmap(M);
  end;
  FreeMem(tmp, Width * 4);
end;

procedure TBitmap.FreeNotification(AObject: TObject);
begin
  if FResource = AObject then
    FResource := nil;
end;

procedure TBitmap.InvertAlpha;
var
  I, J: Integer;
  M: TBitmapData;
  C: PAlphaColorRec;
begin
  if Map(TMapAccess.maReadWrite, M) then
  try
    for J := 0 to Height - 1 do
      for I := 0 to Width - 1 do
      begin
        C := @PAlphaColorArray(M.Data)[J * (M.Pitch div 4) + I];
        C^.Color := UnpremultiplyAlpha(C^.Color);
        C^.A := $FF - C^.A;
        C^.Color := PremultiplyAlpha(C^.Color);
      end;
  finally
    Unmap(M);
  end;
end;

procedure TBitmap.ReplaceOpaqueColor(const Color: TAlphaColor);
var
  I, J: Integer;
  M: TBitmapData;
  C: PAlphaColorRec;
begin
  if Map(TMapAccess.maReadWrite, M) then
  try
    for J := 0 to Height - 1 do
      for I := 0 to Width - 1 do
      begin
        C := @PAlphaColorArray(M.Data)[J * (M.Pitch div 4) + I];
        if C^.A > 0 then
          C^.Color := PremultiplyAlpha(MakeColor(Color, C^.A / $FF));
      end;
  finally
    Unmap(M);
  end;
end;

procedure TBitmap.Resize(const AWidth, AHeight: Integer);
var
  BufferTmp: TBitmap;
begin
  BufferTmp := CreateThumbnail(AWidth, AHeight);
  try
    Assign(BufferTmp);
  finally
    BufferTmp.Free;
  end;
end;

function TBitmap.CreateMask: PByteArray;
var
  I, J: Integer;
  M: TBitmapData;
  C: PAlphaColorRec;
begin
  GetMem(Result, Width * Height);
  if Map(TMapAccess.maReadWrite, M) then
  try
    for j := 0 to Height - 1 do
      for I := 0 to Width - 1 do
      begin
        C := @PAlphaColorArray(M.Data)[J * (M.Pitch div 4) + I];
        Result[I + (J * Width)] := C^.A;
      end;
  finally
    Unmap(M);
  end;
end;

procedure TBitmap.ApplyMask(const Mask: PByteArray; const DstX: Integer = 0; const DstY: Integer = 0);
var
  I, J: Integer;
  M: TBitmapData;
  C: PAlphaColorRec;
begin
  if Map(TMapAccess.maReadWrite, M) then
  try
    for J := 0 to Height - 1 do
      for I := 0 to Width - 1 do
      begin
        if (I - DstX < 0) or (I - DstX > Width - 1) or (J - DstY < 0) or (J - DstY > Height - 1) then
          Continue;
        if Mask[I - DstX + ((J - DstY) * Width)] > 0 then
        begin
          C := @PAlphaColorArray(M.Data)[J * (M.Pitch div 4) + I];
          C^.Color := PremultiplyAlpha(MakeColor(UnpremultiplyAlpha(C^.Color), ($FF - Mask[I - DstX + ((j - DstY) * Width)]) / $FF));
        end;
      end;
  finally
    Unmap(M);
  end;
end;

function TBitmap.CreateThumbnail(const Width, Height: Integer): TBitmap;
var
  R: TRectF;
begin
  Result := TBitmap.Create(Width, Height);
  if Result.Canvas.BeginScene then
  try
    R := RectF(0, 0, Self.Width, Self.Height);
    R.Fit(RectF(0, 0, Width, Height));
    Result.Canvas.DrawBitmap(Self, RectF(0, 0, Self.Width, Self.Height), R, 1.0);
  finally
    Result.Canvas.EndScene;
  end;
end;

function TBitmap.Map(const Access: TMapAccess; var Data: TBitmapData): Boolean;
begin
  if CanvasClass.MapBitmap(Self, Access, Data) then
  begin
    Data.Create(Width, Height, FPixelFormat);
    FMapped := True;
    FMapAccess := Access;
    if Access in [TMapAccess.maWrite, TMapAccess.maReadWrite] then
      FTextureNeedUpdate := True;
    Result := True;
  end
  else
    Result := False;
end;

procedure TBitmap.Unmap(var Data: TBitmapData);
begin
  if FMapped then
  begin
    CanvasClass.UnmapBitmap(Self, Data);
    FMapped := False;
    if FMapAccess in [TMapAccess.maWrite, TMapAccess.maReadWrite] then
      BitmapChanged;
  end;
end;

procedure TBitmap.LoadFromFile(const AFileName: string; const Rotate: Single = 0);
var
  Surf: TBitmapSurface;
begin
  Surf := TBitmapSurface.Create;
  try
    if TBitmapCodecManager.LoadFromFile(AFileName, Surf) then
      Assign(Surf);
  finally
    Surf.Free;
  end;
end;

procedure TBitmap.LoadThumbnailFromFile(const AFileName: string; const AFitWidth, AFitHeight: Single;
  const UseEmbedded: Boolean = True);
var
  Surf: TBitmapSurface;
begin
  Surf := TBitmapSurface.Create;
  try
    if TBitmapCodecManager.LoadThumbnailFromFile(AFileName, AFitWidth, AFitHeight, UseEmbedded, Surf) then
      Assign(Surf);
  finally
    Surf.Free;
  end;
end;

procedure TBitmap.LoadFromStream(Stream: TStream);
var
  S: TStream;
  Surf: TBitmapSurface;
begin
  if Stream.Position > 0 then
  begin
    // need to create temp stream
    S := TMemoryStream.Create;
    try
      S.CopyFrom(Stream, Stream.Size - Stream.Position);
      S.Position := 0;
      Surf := TBitmapSurface.Create;
      try
        if TBitmapCodecManager.LoadFromStream(S, Surf) then
          Assign(Surf);
      finally
        Surf.Free;
      end;
    finally
      S.Free;
    end;
  end
  else
    if Stream.Size = 0 then
      Clear(0)
    else begin
      Surf := TBitmapSurface.Create;
      try
        if TBitmapCodecManager.LoadFromStream(Stream, Surf) then
          Assign(Surf);
      finally
        Surf.Free;
      end;
    end;
end;

procedure TBitmap.SaveToFile(const AFileName: string; const SaveParams: PBitmapCodecSaveParams = nil);
var
  Surf: TBitmapSurface;
begin
  Surf := TBitmapSurface.Create;
  try
    Surf.Assign(Self);
    TBitmapCodecManager.SaveToFile(AFileName, Surf, SaveParams);
  finally
    Surf.Free;
  end;
end;

procedure TBitmap.SaveToStream(Stream: TStream);
var
  Surf: TBitmapSurface;
begin
  Surf := TBitmapSurface.Create;
  try
    Surf.Assign(Self);
    TBitmapCodecManager.SaveToStream(Stream, Surf, '.png');
  finally
    Surf.Free;
  end;
end;

function TBitmap.GetCanvas: TCanvas;
begin
  if not Assigned(FCanvas) then
    FCanvas := CanvasClass.CreateFromBitmap(Self);
  Result := FCanvas;
end;

{ TPath }

constructor TPathData.Create;
begin
  inherited Create;
  FRecalcBounds := True;
end;

destructor TPathData.Destroy;
var
  PathObject: IPathObject;
begin
  if Assigned(FStyleResource) then
  begin
    if Supports(FStyleResource, IPathObject, PathObject) then
      PathObject.RemoveFreeNotify(Self);
    FStyleResource := nil;
  end;
  inherited;
end;

procedure TPathData.Assign(Source: TPersistent);
begin
  if Source is TPathData then
  begin
    if Assigned(TPathData(Source).ResourcePath) then
    begin
      FStyleLookup := TPathData(Source).StyleLookup;
      if Assigned(FOnChanged) then
        FOnChanged(Self);
    end
    else
    begin
      SetLength(FPathData, TPathData(Source).Count);
      System.Move(TPathData(Source).FPathData[0], FPathData[0], SizeOf(TPathPoint) * Count);
      if Assigned(FOnChanged) then
        FOnChanged(Self);
    end;
  end
  else
    inherited
end;

function TPathData.GetStyleLookup: string;
begin
  Result := FStyleLookup;
end;

procedure TPathData.SetStyleLookup(const Value: string);
begin
  if Value <> FStyleLookup then
  begin
    FStyleLookup := Value;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end;
end;

function TPathData.GetPath: TPathData;
var
  O: TFmxObject;
  PO: IPathObject;
begin
  Result := nil;
  if Assigned(FStyleResource) and Supports(FStyleResource, IPathObject, PO) then
    Result := PO.Path
  else if FStyleLookup <> '' then
  begin
    O := FindStyleResource(FStyleLookup);
    if Supports(O, IPathObject, PO) then
    begin
      if Assigned(FStyleResource) then
        PO.RemoveFreeNotify(Self);
      FStyleResource := O;
      if Assigned(FStyleResource) then
        PO.AddFreeNotify(Self);
      Result := PO.Path;
    end;
  end;
end;

function TPathData.GetCount: Integer;
begin
  Result := Length(FPathData);
end;

function TPathData.GetPoint(AIndex: Integer): TPathPoint;
begin
  Result := FPathData[AIndex];
end;

procedure TPathData.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('Path', ReadPath, WritePath, Count > 0);
end;

procedure TPathData.ReadPath(Stream: TStream);
var
  S: Longint;
  I: Integer;
  k: Byte;
  P: TPointF;
begin
  Stream.Read(S, SizeOf(S));
{$IFDEF FPC_BIG_ENDIAN}
  ReverseBytes(@S, 4);
{$ENDIF}
  SetLength(FPathData, S);
  if S > 0 then
  begin
    if (Stream.Size - 4) div S = 9 then
    begin
      for I := 0 to S - 1 do
      begin
        Stream.Read(k, 1);
        Stream.Read(P, SizeOf(P));
{$IFDEF FPC_BIG_ENDIAN}
        ReverseBytes(@P.X, 4);
        ReverseBytes(@P.Y, 4);
{$ENDIF}
        FPathData[I].Kind := TPathPointKind(k);
        FPathData[I].Point := P;
      end;
    end
    else
    begin
      Stream.Read(FPathData[0], S * SizeOf(TPathPoint));
{$IFDEF FPC_BIG_ENDIAN}
      for I := 0 to S * 3 - 1 do
        ReverseBytes(@PColorArray(PathData)[I], 4);
{$ENDIF}
    end;
  end;
  FRecalcBounds := True;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TPathData.WritePath(Stream: TStream);
var
  S: Longint;
begin
  S := Count;
  Stream.Write(S, SizeOf(S));
  if S > 0 then
    Stream.Write(FPathData[0], S * SizeOf(TPathPoint));
end;

function TPathData.LastPoint: TPointF;
begin
  if Count > 0 then
    Result := FPathData[High(FPathData)].Point
  else
    Result := PointF(0, 0);
end;

procedure TPathData.MoveTo(const P: TPointF);
begin
  SetLength(FPathData, Count + 1);
  FPathData[High(FPathData)].Kind := TPathPointKind.ppMoveTo;
  FPathData[High(FPathData)].Point := P;
  FStartPoint := FPathData[High(FPathData)].Point;
  FRecalcBounds := True;
end;

procedure TPathData.MoveToRel(const P: TPointF);
var
  LP: TPointF;
begin
  LP := LastPoint;
  SetLength(FPathData, Count + 1);
  FPathData[High(FPathData)].Kind := TPathPointKind.ppMoveTo;
  FPathData[High(FPathData)].Point := PointF(LP.X + P.X, LP.Y + P.Y);
  FStartPoint := FPathData[High(FPathData)].Point;
  FRecalcBounds := True;
end;

procedure TPathData.LineTo(const P: TPointF);
begin
  SetLength(FPathData, Count + 1);
  FPathData[High(FPathData)].Kind := TPathPointKind.ppLineTo;
  FPathData[High(FPathData)].Point := P;
  FRecalcBounds := True;
end;

procedure TPathData.LineToRel(const P: TPointF);
var
  LP: TPointF;
begin
  LP := LastPoint;
  SetLength(FPathData, Count + 1);
  FPathData[High(FPathData)].Kind := TPathPointKind.ppLineTo;
  FPathData[High(FPathData)].Point := PointF(LP.X + P.X, LP.Y + P.Y);
  FRecalcBounds := True;
end;

procedure TPathData.HLineTo(const X: Single);
begin
  SetLength(FPathData, Count + 1);
  FPathData[High(FPathData)].Kind := TPathPointKind.ppLineTo;
  FPathData[High(FPathData)].Point := PointF(X, FPathData[High(FPathData) - 1].Point.Y);
  FRecalcBounds := True;
end;

procedure TPathData.HLineToRel(const X: Single);
var
  LP: TPointF;
begin
  LP := LastPoint;
  SetLength(FPathData, Count + 1);
  FPathData[High(FPathData)].Kind := TPathPointKind.ppLineTo;
  FPathData[High(FPathData)].Point := PointF(LP.X + X, LP.Y);
  FRecalcBounds := True;
end;

procedure TPathData.VLineTo(const Y: Single);
begin
  SetLength(FPathData, Count + 1);
  FPathData[High(FPathData)].Kind := TPathPointKind.ppLineTo;
  FPathData[High(FPathData)].Point := PointF(FPathData[High(FPathData) - 1].Point.X, Y);
  FRecalcBounds := True;
end;

procedure TPathData.VLineToRel(const Y: Single);
var
  LP: TPointF;
begin
  LP := LastPoint;
  SetLength(FPathData, Count + 1);
  FPathData[High(FPathData)].Kind := TPathPointKind.ppLineTo;
  FPathData[High(FPathData)].Point := PointF(LP.X, LP.Y + Y);
  FRecalcBounds := True;
end;

procedure TPathData.QuadCurveTo(const ControlPoint, EndPoint: TPointF);
var
  LP, CP1, CP2: TPointF;
begin
  LP := LastPoint;
  CP1.X := (1/3 * LP.X) + (2/3 * ControlPoint.X);
  CP1.Y := (1/3 * LP.Y) + (2/3 * ControlPoint.Y);
  CP2.X := (2/3 * ControlPoint.X) + (1/3 * EndPoint.X);
  CP2.Y := (2/3 * ControlPoint.Y) + (1/3 * EndPoint.Y);
  CurveTo(CP1, CP2, EndPoint);
end;

procedure TPathData.CurveTo(const ControlPoint1, ControlPoint2, EndPoint: TPointF);
begin
  SetLength(FPathData, Count + 1);
  FPathData[High(FPathData)].Kind := TPathPointKind.ppCurveTo;
  FPathData[High(FPathData)].Point := ControlPoint1;
  SetLength(FPathData, Count + 1);
  FPathData[High(FPathData)].Kind := TPathPointKind.ppCurveTo;
  FPathData[High(FPathData)].Point := ControlPoint2;
  SetLength(FPathData, Count + 1);
  FPathData[High(FPathData)].Kind := TPathPointKind.ppCurveTo;
  FPathData[High(FPathData)].Point := EndPoint;
  FRecalcBounds := True;
end;

procedure TPathData.CurveToRel(const ControlPoint1, ControlPoint2, EndPoint: TPointF);
var
  LP: TPointF;
begin
  LP := LastPoint;
  SetLength(FPathData, Count + 1);
  FPathData[High(FPathData)].Kind := TPathPointKind.ppCurveTo;
  FPathData[High(FPathData)].Point := PointF(LP.X + ControlPoint1.X, LP.Y + ControlPoint1.Y);
  SetLength(FPathData, Count + 1);
  FPathData[High(FPathData)].Kind := TPathPointKind.ppCurveTo;
  FPathData[High(FPathData)].Point := PointF(LP.X + ControlPoint2.X, LP.Y + ControlPoint2.Y);;
  SetLength(FPathData, Count + 1);
  FPathData[High(FPathData)].Kind := TPathPointKind.ppCurveTo;
  FPathData[High(FPathData)].Point := PointF(LP.X + EndPoint.X, LP.Y + EndPoint.Y);;
  FRecalcBounds := True;
end;

procedure TPathData.SmoothCurveTo(const ControlPoint2, EndPoint: TPointF);
var
  ControlPoint1: TPointF;
begin
  if Count > 2 then
  begin
    ControlPoint1.X := LastPoint.X + (LastPoint.X - FPathData[High(FPathData) - 1].Point.X);
    ControlPoint1.Y := LastPoint.Y + (LastPoint.Y - FPathData[High(FPathData) - 1].Point.Y);
  end
  else
    ControlPoint1 := ControlPoint2;
  SetLength(FPathData, Count + 1);
  FPathData[High(FPathData)].Kind := TPathPointKind.ppCurveTo;
  FPathData[High(FPathData)].Point := ControlPoint1;
  SetLength(FPathData, Count + 1);
  FPathData[High(FPathData)].Kind := TPathPointKind.ppCurveTo;
  FPathData[High(FPathData)].Point := ControlPoint2;
  SetLength(FPathData, Count + 1);
  FPathData[High(FPathData)].Kind := TPathPointKind.ppCurveTo;
  FPathData[High(FPathData)].Point := EndPoint;
  FRecalcBounds := True;
end;

procedure TPathData.SmoothCurveToRel(const ControlPoint2, EndPoint: TPointF);
var
  LP, ControlPoint1: TPointF;
begin
  LP := LastPoint;
  if Count > 2 then
  begin
    ControlPoint1.X := LastPoint.X + (LastPoint.X - FPathData[High(FPathData) - 1].Point.X);
    ControlPoint1.Y := LastPoint.Y + (LastPoint.Y - FPathData[High(FPathData) - 1].Point.Y);
  end
  else
    ControlPoint1 := ControlPoint2;

  SetLength(FPathData, Count + 1);
  FPathData[High(FPathData)].Kind := TPathPointKind.ppCurveTo;
  FPathData[High(FPathData)].Point := PointF(ControlPoint1.X, ControlPoint1.Y);
  SetLength(FPathData, Count + 1);
  FPathData[High(FPathData)].Kind := TPathPointKind.ppCurveTo;
  FPathData[High(FPathData)].Point := PointF(LP.X + ControlPoint2.X, LP.Y + ControlPoint2.Y);
  SetLength(FPathData, Count + 1);
  FPathData[High(FPathData)].Kind := TPathPointKind.ppCurveTo;
  FPathData[High(FPathData)].Point := PointF(LP.X + EndPoint.X, LP.Y + EndPoint.Y);
  FRecalcBounds := True;
end;

procedure TPathData.ClosePath;
begin
  SetLength(FPathData, Count + 1);
  FPathData[High(FPathData)].Kind := TPathPointKind.ppClose;
  FPathData[High(FPathData)].Point := FStartPoint;
  FRecalcBounds := True;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TPathData.AddPath(APath: TPathData);
var
  OldCount: Integer;
  I: Integer;
begin
  OldCount := Count;
  SetLength(FPathData, OldCount + APath.Count);
  for I := 0 to APath.Count - 1 do
    FPathData[OldCount + I] := APath.Points[I];
end;

procedure TPathData.Clear;
begin
  FRecalcBounds := True;
  SetLength(FPathData, 0);
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TPathData.GetBounds: TRectF;
var
  I: Integer;
begin
  if Length(FPathData) = 0 then
  begin
    Result := RectF(0, 0, 0, 0);
    Exit;
  end;
  if FRecalcBounds then
  begin
    Result := RectF($FFFF, $FFFF, -$FFFF, -$FFFF);
    for I := 0 to High(FPathData) do
    begin
      if FPathData[I].Kind = TPathPointKind.ppClose then
        Continue;

      if FPathData[I].Point.X < Result.Left then
        Result.Left := FPathData[I].Point.X;
      if FPathData[I].Point.X > Result.Right then
        Result.Right := FPathData[I].Point.X;
      if FPathData[I].Point.Y < Result.Top then
        Result.Top := FPathData[I].Point.Y;
      if FPathData[I].Point.Y > Result.Bottom then
        Result.Bottom := FPathData[I].Point.Y;
    end;
    // add small amount
    if RectWidth(Result) = 0 then
      Result.Right := Result.Left + 0.001;
    if RectHeight(Result) = 0 then
      Result.Bottom := Result.Top + 0.001;
    FBounds := Result;
    FRecalcBounds := False;
  end
  else
    Result := FBounds;
end;

procedure TPathData.Scale(const scaleX, scaleY: Single);
var
  I: Integer;
begin
  if Length(FPathData) > 0 then
  begin
    FRecalcBounds := True;
    for I := 0 to High(FPathData) do
      case FPathData[I].Kind of
        TPathPointKind.ppMoveTo, TPathPointKind.ppLineTo, TPathPointKind.ppCurveTo:
          begin
            FPathData[I].Point.X := FPathData[I].Point.X * scaleX;
            FPathData[I].Point.Y := FPathData[I].Point.Y * scaleY;
          end;
        TPathPointKind.ppClose:
          begin
          end;
      end;
  end;
end;

procedure TPathData.Translate(const dX, dY: Single);
var
  I: Integer;
begin
  if Count > 0 then
  begin
    FRecalcBounds := True;
    for I := 0 to High(FPathData) do
      case FPathData[I].Kind of
        TPathPointKind.ppMoveTo, TPathPointKind.ppLineTo, TPathPointKind.ppCurveTo:
          begin
            FPathData[I].Point.X := FPathData[I].Point.X + dX;
            FPathData[I].Point.Y := FPathData[I].Point.Y + dY;
          end;
        TPathPointKind.ppClose:
          begin
          end;
      end;
  end;
end;

procedure TPathData.FitToRect(const ARect: TRectF);
var
  B: TRectF;
begin
  B := GetBounds;
  Translate(-B.Left, -B.Top);
  Scale(RectWidth(ARect) / RectWidth(B), RectHeight(ARect) / RectHeight(B));
  Translate(ARect.Left, ARect.Top);
end;

procedure TPathData.ApplyMatrix(const M: TMatrix);
var
  I: Integer;
begin
  if Length(FPathData) > 0 then
  begin
    FRecalcBounds := True;
    for I := 0 to High(FPathData) do
      case FPathData[I].Kind of
        TPathPointKind.ppMoveTo, TPathPointKind.ppLineTo, TPathPointKind.ppCurveTo:
          begin
            FPathData[I].Point := FPathData[I].Point * M;
          end;
        TPathPointKind.ppClose:
          begin
          end;
      end;
  end;
end;

procedure TPathData.Flatten(const Flatness: Single = 0.25);

  procedure CalculateBezierCoefficients(const Bezier: TCubicBezier; out ax, bx, cx, ay, by, cy: Single);
  begin
    cx := 3.0 * (Bezier[1].X - Bezier[0].X);
    cy := 3.0 * (Bezier[1].Y - Bezier[0].Y);
    bx := 3.0 * (Bezier[2].X - Bezier[1].X) - cx;
    by := 3.0 * (Bezier[2].Y - Bezier[1].Y) - cy;
    ax := Bezier[3].X - Bezier[0].X - cx - bx;
    ay := Bezier[3].Y - Bezier[0].Y - cy - by;
  end;

  function PointOnBezier(const StartPoint: TPointF; const ax, bx, cx, ay, by, cy, t: Single): TPointF;
  var
    tSqr: Single;
    tCube: Single;
  begin
    tSqr := t * t;
    tCube := tSqr * t;
    Result.X := (ax * tCube) + (bx * tSqr) + (cx * t) + StartPoint.X;
    Result.Y := (ay * tCube) + (by * tSqr) + (cy * t) + StartPoint.Y;
  end;

  function CreateBezier(const Bezier: TCubicBezier; const PointCount: Integer): TPolygon;
  var
    ax: Single;
    bx: Single;
    cx: Single;
    ay: Single;
    by: Single;
    cy: Single;
    dT: Single;
    t: Single;
    I: Integer;
  begin
    if PointCount = 0 then
      Exit;
    dT := 1.0 / (1.0 * PointCount - 1.0);
    t := 0.0;
    SetLength(Result, PointCount);
    CalculateBezierCoefficients(Bezier, ax, bx, cx, ay, by, cy);
    for I := 0 to PointCount - 1 do
    begin
      Result[I] := PointOnBezier(Bezier[0], ax, bx, cx, ay, by, cy, t);
      t := t + dT;
    end;
  end;

var
  I, j: Integer;
  BPts: TPolygon;
  B: TCubicBezier;
  F, Len: Single;
  SegCount: Integer;
  OldPathData: array of TPathPoint;
  CurPoint: TPointF;
begin
  { scale }
  if Length(FPathData) > 0 then
  begin
    F := Flatness;
    if F < 0.05 then
      F := 0.05;

    { copy data }
    SetLength(OldPathData, Count);
    System.Move(FPathData[0], OldPathData[0], Count * SizeOf(FPathData[0]));
    SetLength(FPathData, 0);

    I := 0;
    while I < Length(OldPathData) do
    begin
      case OldPathData[I].Kind of
        TPathPointKind.ppMoveTo:
          begin
            MoveTo(OldPathData[I].Point);
            CurPoint := OldPathData[I].Point;
          end;
        TPathPointKind.ppLineTo:
          begin
            LineTo(OldPathData[I].Point);
            CurPoint := OldPathData[I].Point;
          end;
        TPathPointKind.ppCurveTo:
          begin
            B[0] := CurPoint;
            B[1] := OldPathData[I].Point;
            inc(I);
            B[2] := OldPathData[I].Point;
            inc(I);
            B[3] := OldPathData[I].Point;
            // calc length
            BPts := CreateBezier(B, 6);
            Len := 0;
            for j := 0 to High(BPts) - 1 do
              Len := Len + (Vector(BPts[j]) - Vector(BPts[j + 1])).Length;
            // flatten
            SegCount := Round(Len / F);
            if SegCount < 2 then
              LineTo(B[3])
            else
            begin
              BPts := CreateBezier(B, SegCount);
              for j := 0 to High(BPts) do
                LineTo(BPts[j]);
              CurPoint := OldPathData[I].Point;
            end;
          end;
        TPathPointKind.ppClose:
          begin
            ClosePath;
          end;
      end;
      inc(I);
    end;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end;
end;

function TPathData.FlattenToPolygon(var Polygon: TPolygon; const Flatness: Single = 0.25): TPointF;

  procedure CalculateBezierCoefficients(const Bezier: TCubicBezier; out ax, bx, cx, ay, by, cy: Single);
  begin
    cx := 3.0 * (Bezier[1].X - Bezier[0].X);
    cy := 3.0 * (Bezier[1].Y - Bezier[0].Y);
    bx := 3.0 * (Bezier[2].X - Bezier[1].X) - cx;
    by := 3.0 * (Bezier[2].Y - Bezier[1].Y) - cy;
    ax := Bezier[3].X - Bezier[0].X - cx - bx;
    ay := Bezier[3].Y - Bezier[0].Y - cy - by;
  end;

  function PointOnBezier(const StartPoint: TPointF; const ax, bx, cx, ay, by, cy, t: Single): TPointF;
  var
    tSqr: Single;
    tCube: Single;
  begin
    tSqr := t * t;
    tCube := tSqr * t;
    Result.X := (ax * tCube) + (bx * tSqr) + (cx * t) + StartPoint.X;
    Result.Y := (ay * tCube) + (by * tSqr) + (cy * t) + StartPoint.Y;
  end;

  function CreateBezier(const Bezier: TCubicBezier; const PointCount: Integer): TPolygon;
  var
    ax: Single;
    bx: Single;
    cx: Single;
    ay: Single;
    by: Single;
    cy: Single;
    dT: Single;
    t: Single;
    I: Integer;
  begin
    if PointCount = 0 then
      Exit;
    dT := 1.0 / (1.0 * PointCount - 1.0);
    t := 0.0;
    SetLength(Result, PointCount);
    CalculateBezierCoefficients(Bezier, ax, bx, cx, ay, by, cy);
    for I := 0 to PointCount - 1 do
    begin
      Result[I] := PointOnBezier(Bezier[0], ax, bx, cx, ay, by, cy, t);
      t := t + dT;
    end;
  end;

  procedure AddPoint(const P: TPointF);
  begin
    if (Length(Polygon) > 0) and (SameValue(P.X, Polygon[High(Polygon)].X, TEpsilon.Position) and SameValue(P.Y, Polygon[High(Polygon)].Y, TEpsilon.Position)) then
      Exit;
    SetLength(Polygon, Length(Polygon) + 1);
    Polygon[High(Polygon)] := P;
  end;

var
  I, j: Integer;
  BPts: TPolygon;
  B: TCubicBezier;
  SP, CurPoint: TPointF;
  F, Len: Single;
  SegCount: Integer;
begin
  Result := PointF(0, 0);
  SetLength(Polygon, 0);
  if Length(FPathData) > 0 then
  begin
    F := Flatness;
    if F < 0.05 then
      F := 0.05;

    I := 0;
    while I < Count do
    begin
      case FPathData[I].Kind of
        TPathPointKind.ppMoveTo:
          begin
            if Length(Polygon) > 0 then
              AddPoint(ClosePolygon);
            AddPoint(FPathData[I].Point);
            CurPoint := FPathData[I].Point;
            SP := CurPoint;
          end;
        TPathPointKind.ppLineTo:
          begin
            AddPoint(FPathData[I].Point);
            CurPoint := FPathData[I].Point;
          end;
        TPathPointKind.ppCurveTo:
          begin
            B[0] := CurPoint;
            B[1] := FPathData[I].Point;
            inc(I);
            B[2] := FPathData[I].Point;
            inc(I);
            B[3] := FPathData[I].Point;
            // calc length
            BPts := CreateBezier(B, 6);
            Len := 0;
            for j := 0 to High(BPts) - 1 do
              Len := Len + (Vector(BPts[j]) - Vector(BPts[j + 1])).Length;
            // flatten
            SegCount := Round(Len / F);
            if SegCount < 2 then
            begin
              AddPoint(B[0]);
              AddPoint(B[3]);
            end
            else
            begin
              BPts := CreateBezier(B, SegCount);
              for j := 0 to High(BPts) do
                AddPoint(BPts[j]);
            end;
            CurPoint := FPathData[I].Point;
          end;
        TPathPointKind.ppClose:
          begin
            AddPoint(SP);
            AddPoint(ClosePolygon);
          end;
      end;
      inc(I);
    end;
    Result := PointF(Abs(GetBounds.Width), Abs(GetBounds.Height));
  end;
end;

procedure TPathData.FreeNotification(AObject: TObject);
begin
  if FStyleResource = AObject then
    FStyleResource := nil;
end;

procedure TPathData.AddEllipse(const ARect: TRectF);
var
  cx, cy: Single;
  px, py: Single;
begin
  cx := (ARect.Left + ARect.Right) / 2;
  cy := (ARect.Top + ARect.Bottom) / 2;
  px := CurveKappa * (RectWidth(ARect) / 2);
  py := CurveKappa * (RectHeight(ARect) / 2);
  MoveTo(PointF(ARect.Left, cy));
  CurveTo(PointF(ARect.Left, cy - py), PointF(cx - px, ARect.Top), PointF(cx, ARect.Top));
  CurveTo(PointF(cx + px, ARect.Top), PointF(ARect.Right, cy - py), PointF(ARect.Right, cy));
  CurveTo(PointF(ARect.Right, cy + py), PointF(cx + px, ARect.Bottom), PointF(cx, ARect.Bottom));
  CurveTo(PointF(cx - px, ARect.Bottom), PointF(ARect.Left, cy + py), PointF(ARect.Left, cy));
end;

procedure TPathData.AddRectangle(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
  const ACornerType: TCornerType = TCornerType.ctRound);
var
  R: TRectF;
  X1, X2, Y1, Y2: Single;
begin
  R := ARect;
  X1 := XRadius;
  if RectWidth(R) - (X1 * 2) < 0 then
    X1 := (XRadius * (RectWidth(R) / (X1 * 2)));
  X2 := X1 / 2;
  Y1 := YRadius;
  if RectHeight(R) - (Y1 * 2) < 0 then
    Y1 := (YRadius * (RectHeight(R) / (Y1 * 2)));
  Y2 := Y1 / 2;

  MoveTo(PointF(R.Left, R.Top + Y1));
  if TCorner.crTopLeft in ACorners then
  begin
    case ACornerType of
      // ctRound - default
      TCornerType.ctBevel:
        LineTo(PointF(R.Left + X1, R.Top));
      TCornerType.ctInnerRound:
        CurveTo(PointF(R.Left + X2, R.Top + Y1), PointF(R.Left + X1, R.Top + Y2), PointF(R.Left + X1, R.Top));
      TCornerType.ctInnerLine:
        begin
          LineTo(PointF(R.Left + X2, R.Top + Y1));
          LineTo(PointF(R.Left + X1, R.Top + Y2));
          LineTo(PointF(R.Left + X1, R.Top));
        end;
    else
      CurveTo(PointF(R.Left, R.Top + (Y2)), PointF(R.Left + X2, R.Top), PointF(R.Left + X1, R.Top))
    end;
  end
  else
  begin
    LineTo(PointF(R.Left, R.Top));
    LineTo(PointF(R.Left + X1, R.Top));
  end;
  LineTo(PointF(R.Right - X1, R.Top));
  if TCorner.crTopRight in ACorners then
  begin
    case ACornerType of
      // ctRound - default
      TCornerType.ctBevel:
        LineTo(PointF(R.Right, R.Top + Y1));
      TCornerType.ctInnerRound:
        CurveTo(PointF(R.Right - X1, R.Top + Y2), PointF(R.Right - X2, R.Top + Y1), PointF(R.Right, R.Top + Y1));
      TCornerType.ctInnerLine:
        begin
          LineTo(PointF(R.Right - X1, R.Top + Y2));
          LineTo(PointF(R.Right - X2, R.Top + Y1));
          LineTo(PointF(R.Right, R.Top + Y1));
        end;
    else
      CurveTo(PointF(R.Right - X2, R.Top), PointF(R.Right, R.Top + (Y2)), PointF(R.Right, R.Top + Y1))
    end;
  end
  else
  begin
    LineTo(PointF(R.Right, R.Top));
    LineTo(PointF(R.Right, R.Top + Y1));
  end;
  LineTo(PointF(R.Right, R.Bottom - Y1));
  if TCorner.crBottomRight in ACorners then
  begin
    case ACornerType of
      // ctRound - default
      TCornerType.ctBevel:
        LineTo(PointF(R.Right - X1, R.Bottom));
      TCornerType.ctInnerRound:
        CurveTo(PointF(R.Right - X2, R.Bottom - Y1), PointF(R.Right - X1, R.Bottom - Y2), PointF(R.Right - X1, R.Bottom));
      TCornerType.ctInnerLine:
        begin
          LineTo(PointF(R.Right - X2, R.Bottom - Y1));
          LineTo(PointF(R.Right - X1, R.Bottom - Y2));
          LineTo(PointF(R.Right - X1, R.Bottom));
        end;
    else
      CurveTo(PointF(R.Right, R.Bottom - (Y2)), PointF(R.Right - X2, R.Bottom), PointF(R.Right - X1, R.Bottom))
    end;
  end
  else
  begin
    LineTo(PointF(R.Right, R.Bottom));
    LineTo(PointF(R.Right - X1, R.Bottom));
  end;
  LineTo(PointF(R.Left + X1, R.Bottom));
  if TCorner.crBottomLeft in ACorners then
  begin
    case ACornerType of
      // ctRound - default
      TCornerType.ctBevel:
        LineTo(PointF(R.Left, R.Bottom - Y1));
      TCornerType.ctInnerRound:
        CurveTo(PointF(R.Left + X1, R.Bottom - Y2), PointF(R.Left + X2, R.Bottom - Y1), PointF(R.Left, R.Bottom - Y1));
      TCornerType.ctInnerLine:
        begin
          LineTo(PointF(R.Left + X1, R.Bottom - Y2));
          LineTo(PointF(R.Left + X2, R.Bottom - Y1));
          LineTo(PointF(R.Left, R.Bottom - Y1));
        end;
    else
      CurveTo(PointF(R.Left + X2, R.Bottom), PointF(R.Left, R.Bottom - (Y2)), PointF(R.Left, R.Bottom - Y1))
    end;
  end
  else
  begin
    LineTo(PointF(R.Left, R.Bottom));
    LineTo(PointF(R.Left, R.Bottom - Y1));
  end;
  ClosePath;
end;

procedure DrawArcWithBezier(Path: TPathData; CenterX, CenterY, RadiusX, RadiusY, StartAngle, SweepRange: Single;
  UseMoveTo: Boolean);
var
  Coord: array of TPointF;
  Pts: array of TPointF;
  A, B, C, X, Y: Single;
  ss, cc: Single;
  I: Integer;
begin
  if SweepRange = 0 then
  begin
    if UseMoveTo then
    begin
      if (Length(Path.FPathData) = 0) then
        Path.MoveTo(PointF(CenterX + RadiusX * Cos(StartAngle), CenterY - RadiusY * Sin(StartAngle)))
      else
        Path.LineTo(PointF(CenterX + RadiusX * Cos(StartAngle), CenterY - RadiusY * Sin(StartAngle)));
    end;
    Path.LineTo(PointF(CenterX + RadiusX * Cos(StartAngle), CenterY - RadiusY * Sin(StartAngle)));
    Exit;
  end;
  B := Sin(SweepRange / 2);
  C := Cos(SweepRange / 2);
  A := 1 - C;
  X := A * 4.0 / 3.0;
  Y := B - X * C / B;
  ss := Sin(StartAngle + SweepRange / 2);
  cc := Cos(StartAngle + SweepRange / 2);
  SetLength(Coord, 4);
  Coord[0] := PointF(C, -B);
  Coord[1] := PointF(C + X, -Y);
  Coord[2] := PointF(C + X, Y);
  Coord[3] := PointF(C, B);
  SetLength(Pts, 4);
  for I := 0 to 3 do
  begin
    Pts[I] := PointF(CenterX + RadiusX * (Coord[I].X * cc - Coord[I].Y * ss),
      CenterY + RadiusY * (Coord[I].X * ss + Coord[I].Y * cc));
  end;
  if UseMoveTo then
  begin
    if (Length(Path.FPathData) = 0) then
      Path.MoveTo(Pts[0])
    else
      Path.LineTo(Pts[0]);
  end;
  Path.CurveTo(Pts[1], Pts[2], Pts[3]);
end;

procedure TPathData.AddArc(const Center, Radius: TPointF; StartAngle, SweepAngle: Single);
const
  bezier_arc_angle_epsilon = 0.01;
var
  UseMoveTo: Boolean;
  I: Integer;
  F: Single;
  total_sweep, local_sweep, prev_sweep: Single;
  done: Boolean;
begin
  StartAngle := DegToRad(StartAngle);
  SweepAngle := DegToRad(SweepAngle);

  I := trunc(StartAngle / (2.0 * cPI));
  F := StartAngle - (I * 2.0 * cPI);

  StartAngle := F;

  if SweepAngle >= 2.0 * cPI then
    SweepAngle := 2.0 * cPI;
  if SweepAngle <= -2.0 * cPI then
    SweepAngle := -2.0 * cPI;

  if Abs(SweepAngle) < 1E-10 then
  begin
    Exit;
  end;

  total_sweep := 0.0;

  done := False;
  UseMoveTo := True;
  repeat
    if SweepAngle < 0.0 then
    begin
      prev_sweep := total_sweep;
      local_sweep := -cPI * 0.5;
      total_sweep := total_sweep - (cPI * 0.5);
      if total_sweep <= SweepAngle + bezier_arc_angle_epsilon then
      begin
        local_sweep := SweepAngle - prev_sweep;
        done := True;
      end;
    end
    else
    begin
      prev_sweep := total_sweep;
      local_sweep := cPI * 0.5;
      total_sweep := total_sweep + (Pi * 0.5);
      if total_sweep >= SweepAngle - bezier_arc_angle_epsilon then
      begin
        local_sweep := SweepAngle - prev_sweep;
        done := True;
      end;
    end;
    DrawArcWithBezier(Self, Center.X, Center.Y, Radius.X, Radius.Y, StartAngle, local_sweep, UseMoveTo);
    UseMoveTo := False;
    StartAngle := StartAngle + local_sweep;
  until done;
end;

procedure TPathData.AddArcSvgPart(const Center, Radius: TPointF; StartAngle, SweepAngle: Single);
const
  bezier_arc_angle_epsilon = 0.01;
var
  UseMoveTo: Boolean;
  I: Integer;
  F: Single;
  total_sweep, local_sweep, prev_sweep: Single;
  done: Boolean;
begin
  StartAngle := DegToRad(StartAngle);
  SweepAngle := DegToRad(SweepAngle);

  I := trunc(StartAngle / (2.0 * cPI));
  F := StartAngle - (I * 2.0 * cPI);

  StartAngle := F;

  if SweepAngle >= 2.0 * cPI then
    SweepAngle := 2.0 * cPI;
  if SweepAngle <= -2.0 * cPI then
    SweepAngle := -2.0 * cPI;

  if Abs(SweepAngle) < 1E-10 then
  begin
    Exit;
  end;

  total_sweep := 0.0;

  done := False;
  UseMoveTo := False;
  repeat
    if SweepAngle < 0.0 then
    begin
      prev_sweep := total_sweep;
      local_sweep := -cPI * 0.5;
      total_sweep := total_sweep - (cPI * 0.5);
      if total_sweep <= SweepAngle + bezier_arc_angle_epsilon then
      begin
        local_sweep := SweepAngle - prev_sweep;
        done := True;
      end;
    end
    else
    begin
      prev_sweep := total_sweep;
      local_sweep := cPI * 0.5;
      total_sweep := total_sweep + (Pi * 0.5);
      if total_sweep >= SweepAngle - bezier_arc_angle_epsilon then
      begin
        local_sweep := SweepAngle - prev_sweep;
        done := True;
      end;
    end;
    DrawArcWithBezier(Self, Center.X, Center.Y, Radius.X, Radius.Y, StartAngle, local_sweep, UseMoveTo);
    UseMoveTo := False;
    StartAngle := StartAngle + local_sweep;
  until done;
end;

procedure TPathData.AddArcSvg(const P1, Radius: TPointF; Angle: Single; const LargeFlag, SweepFlag: Boolean;
  const P2: TPointF);
var
  I: Integer;
  m_radii_ok: Boolean;
  V, P, N, sq, rx, ry, x0, y0, X1, Y1, X2, Y2, cx, cy, ux, uy, vx, vy,

    dx2, dy2, prx, pry, px1, py1, cx1, cy1, sx2, sy2,

    sign, coef,

    radii_check, start_angle, sweep_angle,

    cos_a, sin_a: Single;
  tm: TMatrix;
  len: Integer;
begin
  // Trivial case: Arc transformate to point
  if P1 = P2 then
    Exit;

  rx := Radius.X;
  ry := Radius.Y;
  x0 := P1.X;
  y0 := P1.Y;
  X2 := P2.X;
  Y2 := P2.Y;
  Angle := DegToRad(Angle);

  m_radii_ok := True;

  if rx < 0.0 then
    rx := -rx;

  if ry < 0.0 then
    ry := -rx;

  // Calculate the middle point between
  // the current and the final points
  dx2 := (x0 - X2) / 2.0;
  dy2 := (y0 - Y2) / 2.0;

  // Convert angle from degrees to radians
  cos_a := Cos(Angle);
  sin_a := Sin(Angle);

  // Calculate (x1, y1)
  X1 := cos_a * dx2 + sin_a * dy2;
  Y1 := -sin_a * dx2 + cos_a * dy2;

  // Ensure radii are large enough
  prx := rx * rx;
  pry := ry * ry;
  px1 := X1 * X1;
  py1 := Y1 * Y1;

  // Check that radii are large enough
  radii_check := px1 / prx + py1 / pry;

  if radii_check > 1.0 then
  begin
    rx := Sqrt(radii_check) * rx;
    ry := Sqrt(radii_check) * ry;
    prx := rx * rx;
    pry := ry * ry;

    if radii_check > 10.0 then
      m_radii_ok := False;

  end;

  // Calculate (cx1, cy1)
  if LargeFlag = SweepFlag then
    sign := -1.0
  else
    sign := 1.0;

  sq := (prx * pry - prx * py1 - pry * px1) / (prx * py1 + pry * px1);

  if sq < 0 then
    coef := sign * Sqrt(0)
  else
    coef := sign * Sqrt(sq);

  cx1 := coef * ((rx * Y1) / ry);
  cy1 := coef * -((ry * X1) / rx);

  // Calculate (cx, cy) from (cx1, cy1)
  sx2 := (x0 + X2) / 2.0;
  sy2 := (y0 + Y2) / 2.0;
  cx := sx2 + (cos_a * cx1 - sin_a * cy1);
  cy := sy2 + (sin_a * cx1 + cos_a * cy1);

  // Calculate the start_angle (angle1) and the sweep_angle (dangle)
  ux := (X1 - cx1) / rx;
  uy := (Y1 - cy1) / ry;
  vx := (-X1 - cx1) / rx;
  vy := (-Y1 - cy1) / ry;

  // Calculate the angle start
  N := Sqrt(ux * ux + uy * uy);
  P := ux; // (1 * ux ) + (0 * uy )

  if uy < 0 then
    sign := -1.0
  else
    sign := 1.0;

  V := P / N;

  if V < -1.0 then
    V := -1.0;

  if V > 1.0 then
    V := 1.0;

  start_angle := sign * ArcCos(V);

  // Calculate the sweep angle
  N := Sqrt((ux * ux + uy * uy) * (vx * vx + vy * vy));
  P := ux * vx + uy * vy;

  if ux * vy - uy * vx < 0 then
    sign := -1.0
  else
    sign := 1.0;

  V := P / N;

  if V < -1.0 then
    V := -1.0;

  if V > 1.0 then
    V := 1.0;

  sweep_angle := sign * ArcCos(V);

  if (not SweepFlag) and (sweep_angle > 0) then
    sweep_angle := sweep_angle - Pi * 2.0
  else if SweepFlag and (sweep_angle < 0) then
    sweep_angle := sweep_angle + Pi * 2.0;

  len := Count;
  AddArcSvgPart(PointF(0, 0), PointF(rx, ry), RadToDeg(start_angle), RadToDeg(sweep_angle));

  tm := TMatrix.Identity;
  tm.m31 := cx;
  tm.m32 := cy;
  tm := TMatrix.CreateRotation(Angle) * tm;

  I := len;
  while I < Count do
  begin
    FPathData[I].Point := FPathData[I].Point * tm;
    Inc(I);
  end;
end;

function TPathData.IsEmpty: Boolean;
begin
  Result := (Length(FPathData) = 0) or (GetBounds.Width * GetBounds.Height = 0);
end;

function TPathData.GetPathString: string;
var
  I: Integer;
  Builder: TStringBuilder;
begin
  Builder := TStringBuilder.Create;
  Result := Builder.ToString;
  try
    I := 0;
    while I < Count do
    begin
      case FPathData[I].Kind of
        TPathPointKind.ppMoveTo:
          Builder.Append('M')
                 .Append(FloatToStr(FPathData[I].Point.X, USFormatSettings))
                 .Append(',')
                 .Append(FloatToStr(FPathData[I].Point.Y, USFormatSettings))
                 .Append(' ');
        TPathPointKind.ppLineTo:
          Builder.Append('L')
                 .Append(FloatToStr(FPathData[I].Point.X, USFormatSettings))
                 .Append(',')
                 .Append(FloatToStr(FPathData[I].Point.Y, USFormatSettings))
                 .Append(' ');
        TPathPointKind.ppCurveTo:
          begin
            Builder.Append('C')
                   .Append(FloatToStr(FPathData[I].Point.X, USFormatSettings))
                   .Append(',')
                   .Append(FloatToStr(FPathData[I].Point.Y, USFormatSettings))
                   .Append(' ');

            Builder.Append(FloatToStr(FPathData[I + 1].Point.X, USFormatSettings))
                   .Append(',')
                   .Append(FloatToStr(FPathData[I + 1].Point.Y, USFormatSettings))
                   .Append(' ');

            Builder.Append(FloatToStr(FPathData[I + 2].Point.X, USFormatSettings))
                   .Append(',')
                   .Append(FloatToStr(FPathData[I + 2].Point.Y, USFormatSettings))
                   .Append(' ');

            Inc(I, 2);
          end;
        TPathPointKind.ppClose:
          Builder.Append('Z ');
      end;
      Inc(I);
    end;
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

function TPathData.GetTok(const S: string; var Pos: Integer): string;
var
  StringBuilder: TStringBuilder;
begin
  StringBuilder := TStringBuilder.Create;
  try
    Result := StringBuilder.ToString;
    if Pos >= Length(S) then
      Exit;
    while (Pos < S.Length) and S.Chars[Pos].IsInArray([' ']) do
      Inc(Pos);

    while Pos < S.Length do
    begin
      if not 'zmlchvsqtaZMLCHVSQTA'.Contains(S.Chars[Pos]) then
        Break;
      StringBuilder.Append(S.Chars[Pos]);
      Inc(Pos);
    end;
    Result := StringBuilder.ToString;
  finally
    StringBuilder.Free;
  end;
end;

function TPathData.GetNum(const S: string; var Pos: Integer): string;
var
  StringBuilder: TStringBuilder;
begin
  StringBuilder := TStringBuilder.Create;
  try
    Result := '';
    if Pos > S.Length - 1 then
      Exit;
    while (Pos < S.Length) and S.Chars[Pos].IsInArray([' ']) do
      Inc(Pos);
    while Pos < S.Length do
    begin
      if (S.Chars[Pos] = 'e') then
      begin
        StringBuilder.Append(S.Chars[Pos]);
        Inc(Pos);
        Continue;
      end;
      if (S.Chars[Pos] = '-') and (StringBuilder.Length > 0) and (StringBuilder.Chars[StringBuilder.Length - 1] = 'e') then
      begin
        StringBuilder.Append(S.Chars[Pos]);
        Inc(Pos);
        Continue;
      end;
      if (StringBuilder.Length > 0) and (S.Chars[Pos] = '-') then
        Break;
     if not '0123456789.'.Contains(S.Chars[Pos]) and not (S.Chars[Pos] = '-') then
        Break;
      StringBuilder.Append(S.Chars[Pos]);
      Inc(Pos);
    end;
    while S.Chars[Pos].IsInArray([' ']) do
      Inc(Pos);
    Result := StringBuilder.ToString;
  finally
    StringBuilder.Free;
  end;
end;

function TPathData.GetPointFromStr(const S: string; var Pos: Integer): TPointF;
var
  X, Y: string;
begin
  Result := PointF(0, 0);
  if Pos >= S.Length then
    Exit;
  while (Pos < S.Length) and S.Chars[Pos].IsInArray([',', ' ']) do
    Inc(Pos);
  X := GetNum(S, Pos);
  while (Pos < S.Length) and S.Chars[Pos].IsInArray([',', ' ']) do
    Inc(Pos);
  Y := GetNum(S, Pos);
  while (Pos < S.Length) and S.Chars[Pos].IsInArray([',', ' ']) do
    Inc(Pos);
  Result := PointF(StrToFloat(X, USFormatSettings), StrToFloat(Y, USFormatSettings));
end;

procedure TPathData.SetPathString(const Value: string);
var
  PathString, toks: string;
  Token: Char;
  R, CP1, CP2: TPointF;
  Angle: Single;
  large, sweet: Boolean;
  Pos, I: Integer;
  PointFTmp: TPointF;
  Builder: TStringBuilder;
  TokenBuilder: TStringBuilder;
  LastLength: Integer;
begin
  Builder := TStringBuilder.Create;
  TokenBuilder := TStringBuilder.Create;
  try
    { change every #10#13 to space }
    for I := 0 to Value.Length - 1 do
    begin
      if Value.Chars[I].IsInArray([#9, #10, #13]) then
        Builder.Append(' ')
      else
        Builder.Append(Value.Chars[I]);
    end;
    PathString := Builder.ToString;

    { }
    SetLength(FPathData, 0);
    Pos := 0;
    LastLength := -1;
    while (Builder.Length > Pos) and (LastLength <> Pos) do
    begin
      LastLength := Pos;
      toks := GetTok(PathString, Pos);
      TokenBuilder.Clear;
      TokenBuilder.Append(toks);

      while TokenBuilder.Length > 0 do
      begin
        Token := TokenBuilder.Chars[0];
        TokenBuilder.Remove(0, 1);
        try
          if Token.IsInArray(['z', 'Z']) then
            ClosePath;
          if (Token = 'M') then
          begin
            MoveTo(GetPointFromStr(PathString, Pos));
            while (PathString.Chars[Pos].IsDigit or (PathString.Chars[Pos] = '-')) do
            begin
              { next points }
              LineTo(GetPointFromStr(PathString, Pos));
            end;
          end;
          if (Token = 'm') then
          begin
            MoveToRel(GetPointFromStr(PathString, Pos));
            while (PathString.Chars[Pos].IsDigit or (PathString.Chars[Pos] = '-')) do
            begin
              { next points }
              LineToRel(GetPointFromStr(PathString, Pos));
            end;
          end;
          if (Token = 'L') then
          begin
            LineTo(GetPointFromStr(PathString, Pos));
            while (PathString.Chars[Pos].IsDigit or (PathString.Chars[Pos] = '-')) do
            begin
              { next points }
              LineTo(GetPointFromStr(PathString, Pos));
            end;
          end;
          if (Token = 'l') then
          begin
            LineToRel(GetPointFromStr(PathString, Pos));
            while (PathString.Chars[Pos].IsDigit or (PathString.Chars[Pos] = '-')) do
            begin
              { next points }
              LineToRel(GetPointFromStr(PathString, Pos));
            end;
          end;
          if (Token = 'C') then
          begin
            CP1 := GetPointFromStr(PathString, Pos);
            CP2 := GetPointFromStr(PathString, Pos);
            CurveTo(CP1, CP2, GetPointFromStr(PathString, Pos));
            while (PathString.Chars[Pos].IsDigit or (PathString.Chars[Pos] = '-')) do
            begin
              { next points }
              CP1 := GetPointFromStr(PathString, Pos);
              CP2 := GetPointFromStr(PathString, Pos);
              CurveTo(CP1, CP2, GetPointFromStr(PathString, Pos));
            end;
          end;
          if (Token = 'c') then
          begin
            CP1 := GetPointFromStr(PathString, Pos);
            CP2 := GetPointFromStr(PathString, Pos);
            CurveToRel(CP1, CP2, GetPointFromStr(PathString, Pos));
            while (PathString.Chars[Pos].IsDigit or (PathString.Chars[Pos] = '-')) do
            begin
              { next points }
              CP1 := GetPointFromStr(PathString, Pos);
              CP2 := GetPointFromStr(PathString, Pos);
              CurveToRel(CP1, CP2, GetPointFromStr(PathString, Pos));
            end;
          end;
          if (Token = 'S') then
          begin
            CP2 := GetPointFromStr(PathString, Pos);
            SmoothCurveTo(CP2, GetPointFromStr(PathString, Pos));
            while (PathString.Chars[Pos].IsDigit or (PathString.Chars[Pos] = '-')) do
            begin
              { next points }
              CP2 := GetPointFromStr(PathString, Pos);
              SmoothCurveTo(CP2, GetPointFromStr(PathString, Pos));
            end;
          end;
          if (Token = 's') then
          begin
            CP2 := GetPointFromStr(PathString, Pos);
            SmoothCurveToRel(CP2, GetPointFromStr(PathString, Pos));
            while (PathString.Chars[Pos].IsDigit or (PathString.Chars[Pos] = '-')) do
            begin
              { next points }
              CP2 := GetPointFromStr(PathString, Pos);
              SmoothCurveToRel(CP2, GetPointFromStr(PathString, Pos));
            end;
          end;
          if (Token = 'H') then
          begin
            // skip horizontal line
            HLineTo(StrToFloat(GetNum(PathString, Pos), USFormatSettings));
          end;
          if (Token = 'h') then
          begin
            // skip horizontal line
            HLineToRel(StrToFloat(GetNum(PathString, Pos), USFormatSettings));
          end;
          if (Token = 'V') then
          begin
            // skip vertical line
            VLineTo(StrToFloat(GetNum(PathString, Pos), USFormatSettings));
          end;
          if (Token = 'v') then
          begin
            // skip vertical line
            VLineToRel(StrToFloat(GetNum(PathString, Pos), USFormatSettings));
          end;
          if (Token = 'Q') then
          begin
            // skip quadratic bezier
            GetPointFromStr(PathString, Pos);
            GetPointFromStr(PathString, Pos);
          end;
          if (Token = 'q') then
          begin
            // skip quadratic bezier
            GetPointFromStr(PathString, Pos);
            GetPointFromStr(PathString, Pos);
          end;
          if (Token = 'T') then
          begin
            // skip show qudratic bezier
            GetPointFromStr(PathString, Pos);
          end;
          if (Token = 't') then
          begin
            // skip show qudratic bezier
            GetPointFromStr(PathString, Pos);
          end;
          if (Token = 'A') then
          begin
            // arc
            if Count > 0 then
              CP1 := FPathData[High(FPathData)].Point
            else
              CP1 := PointF(0, 0);
            R := GetPointFromStr(PathString, Pos);
            Angle := StrToFloat(GetNum(PathString, Pos), USFormatSettings);

            PointFTmp := GetPointFromStr(PathString, Pos);
            large := PointFTmp.X = 1;
            sweet := PointFTmp.Y = 1;
            CP2 := GetPointFromStr(PathString, Pos);
            AddArcSvg(CP1, R, Angle, large, sweet, CP2);
          end;
          if (Token = 'a') then
          begin
            // arc rel
            if Count > 0 then
              CP1 := FPathData[High(FPathData)].Point
            else
              CP1 := PointF(0, 0);
            R := GetPointFromStr(PathString, Pos);
            Angle := StrToFloat(GetNum(PathString, Pos), USFormatSettings);
            PointFTmp := GetPointFromStr(PathString, Pos);
            large := PointFTmp.X = 1;
            sweet := PointFTmp.Y = 1;
            CP2 := GetPointFromStr(PathString, Pos);
            CP2.X := CP1.X + CP2.X;
            CP2.Y := CP1.Y + CP2.Y;
            AddArcSvg(CP1, R, Angle, large, sweet, CP2);
          end;
        except
        end;
      end;
    end;
    FRecalcBounds := True;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  finally
    TokenBuilder.Free;
    Builder.Free;
  end;
end;


{ TCanvasManager }


class procedure TCanvasManager.UnInitialize;
var
  CanvasSrv: IFMXCanvasService;
begin
  FreeAndNil(FMeasureBitmap);
  FreeAndNil(FCanvasList);
  if TPlatformServices.Current.SupportsPlatformService(IFMXCanvasService, IInterface(CanvasSrv)) then
    CanvasSrv.UnregisterCanvasClasses;
end;

class function TCanvasManager.CreateFromBitmap(const ABitmap: TBitmap; const AQuality: TCanvasQuality = TCanvasQuality.ccSystemDefault): TCanvas;
begin
  Result := DefaultCanvas.CreateFromBitmap(ABitmap, AQuality);
end;

class function TCanvasManager.CreateFromPrinter(const APrinter: TAbstractPrinter): TCanvas;
begin
  Result := DefaultPrinterCanvas.CreateFromPrinter(APrinter);
end;

class function TCanvasManager.CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer;
  const AQuality: TCanvasQuality = TCanvasQuality.ccSystemDefault): TCanvas;
begin
  Result := DefaultCanvas.CreateFromWindow(AParent, AWidth, AHeight, AQuality);
end;

class function TCanvasManager.GetDefaultCanvas: TCanvasClass;
var
  CanvasSrv: IFMXCanvasService;
  CanvasClassRec: TCanvasClassRec;
begin
  if not Assigned(FDefaultCanvasClass) then
  begin
    Result := nil;
    if not Assigned(FCanvasList) then
      if TPlatformServices.Current.SupportsPlatformService(IFMXCanvasService, IInterface(CanvasSrv)) then
        CanvasSrv.RegisterCanvasClasses;
    if Assigned(FCanvasList) and (FCanvasList.Count > 0) then
    begin
      for CanvasClassRec in FCanvasList do
      begin
        if CanvasClassRec.Default and
          (not FEnableSoftwareCanvas and (TCanvasStyle.NeedGPUSurface in CanvasClassRec.CanvasClass.GetCanvasStyle)) then
        begin
          Result := CanvasClassRec.CanvasClass;
          Break;
        end;
        if CanvasClassRec.Default and
          (FEnableSoftwareCanvas and not (TCanvasStyle.NeedGPUSurface in CanvasClassRec.CanvasClass.GetCanvasStyle)) then
        begin
          Result := CanvasClassRec.CanvasClass;
          Break;
        end;
      end;
      if not Assigned(Result) and FEnableSoftwareCanvas then
      begin
        for CanvasClassRec in FCanvasList do
        begin
          if not (TCanvasStyle.NeedGPUSurface in CanvasClassRec.CanvasClass.GetCanvasStyle) then
          begin
            Result := CanvasClassRec.CanvasClass;
            Break;
          end;
        end;
      end;
      if not Assigned(Result) then
        Result := FCanvasList[0].CanvasClass;
      FDefaultCanvasClass := Result;
    end
    else
      raise ECanvasManagerException.Create('No TCanvas implementation found');
  end
  else
    Result := FDefaultCanvasClass;
end;

class function TCanvasManager.GetDefaultPrinterCanvas: TCanvasClass;
var
  CanvasSrv: IFMXCanvasService;
  CanvasClassRec: TCanvasClassRec;
begin
  if not Assigned(FDefaultPrinterCanvasClass) then
  begin
    Result := nil;
    if not Assigned(FCanvasList) then
      if TPlatformServices.Current.SupportsPlatformService(IFMXCanvasService, IInterface(CanvasSrv)) then
        CanvasSrv.RegisterCanvasClasses;
    if Assigned(FCanvasList) and (FCanvasList.Count > 0) then
    begin
      for CanvasClassRec in FCanvasList do
        if CanvasClassRec.PrinterCanvas then
        begin
          Result := CanvasClassRec.CanvasClass;
          Break;
        end;
      FDefaultPrinterCanvasClass := Result;
    end
    else
      raise ECanvasManagerException.Create('No TCanvas for printer implementation found');
  end
  else
    Result := FDefaultPrinterCanvasClass;
end;

class function TCanvasManager.GetMeasureCanvas: TCanvas;
begin
  if not Assigned(FMeasureBitmap) then
    FMeasureBitmap := TBitmap.Create(1, 1);
  Result := FMeasureBitmap.Canvas
end;

class procedure TCanvasManager.RecreateFromPrinter(const Canvas: TCanvas; const APrinter: TAbstractPrinter);
begin
  Canvas.UnInitialize;
  Canvas.CreateFromPrinter(APrinter);
end;

class procedure TCanvasManager.EnableSoftwareCanvas(const Enable: Boolean);
begin
  FEnableSoftwareCanvas := Enable;
  FDefaultCanvasClass := nil;
end;

class procedure TCanvasManager.RegisterCanvas(const CanvasClass: TCanvasClass; const ADefault: Boolean; const APrinterCanvas: Boolean);
var
  Rec: TCanvasClassRec;
begin
  if Not Assigned(FCanvasList) then
    FCanvasList := TList<TCanvasClassRec>.Create;
  Rec.CanvasClass := CanvasClass;
  Rec.Default := ADefault;
  Rec.PrinterCanvas := APrinterCanvas;
  FCanvasList.Add(Rec);
end;

{ TCanvas }

constructor TCanvas.CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer;
  const AQuality: TCanvasQuality = TCanvasQuality.ccSystemDefault);
begin
  inherited Create;
  FQuality := AQuality;
  FParent := AParent;
  FWidth := AWidth;
  FHeight := AHeight;
  Initialize;
end;

class procedure TCanvas.CopyBitmap(const Source, Dest: TBitmap);
var
  S, D: TBitmapData;
begin
  if (Source.Width = Dest.Width) and (Source.Height = Dest.Height) then
  begin
    if not Source.IsEmpty then
    begin
      if (Source.CanvasClass = Dest.CanvasClass) then
        Source.CanvasClass.DoCopyBitmap(Source, Dest)
      else begin
        if Source.Map(TMapAccess.maRead, S) and Dest.Map(TMapAccess.maWrite, D) then
        try
          D.Copy(S);
        finally
          Source.Unmap(S);
          Dest.Unmap(D);
        end;
      end;
    end;
  end else
    raise ECanvasException.Create(SBitmapSizeNotEqual);
end;

constructor TCanvas.CreateFromBitmap(const ABitmap: TBitmap; const AQuality: TCanvasQuality = TCanvasQuality.ccSystemDefault);
begin
  inherited Create;
  FQuality := AQuality;
  FBitmap := ABitmap;
  FWidth := ABitmap.Width;
  FHeight := ABitmap.Height;
  Initialize;
end;

constructor TCanvas.CreateFromPrinter(const APrinter: TAbstractPrinter);
begin
  inherited Create;
  Initialize;
  FPrinter := APrinter;
end;

procedure TCanvas.Initialize;
begin
  FScale := GetCanvasScale;
  FStroke := TStrokeBrush.Create(TBrushKind.bkSolid, $FF000000);
  FFill := TBrush.Create(TBrushKind.bkSolid, $FFFFFFFF);
  FFont := TFont.Create;
  FFont.OnChanged := FontChanged;
  FCanvasSaveData := TCanvasSaveStateList.Create;
  FMatrixMeaning := TMatrixMeaning.mmIdentity;
  FBlending := True;
end;

procedure TCanvas.UnInitialize;
begin
  FCanvasSaveData.DisposeOf;
  FFont.DisposeOf;
  FStroke.DisposeOf;
  FFill.DisposeOf;
end;

destructor TCanvas.Destroy;
begin
  TMessageManager.DefaultManager.SendMessage(Self, TCanvasDestroyMessage.Create);
  UnInitialize;
  inherited;
end;

function TCanvas.DoBeginScene(const AClipRects: PClipRects = nil; AContextHandle: THandle = 0): Boolean;
begin
  FClippingChangeCount := 0;
  FSavingStateCount := 0;

  Stroke.Thickness := 1;
  Stroke.Cap := TStrokeCap.scFlat;
  Stroke.Join := TStrokeJoin.sjMiter;
  Stroke.Dash := TStrokeDash.sdSolid;
  Stroke.Kind := TBrushKind.bkSolid;
  Fill.Kind := TBrushKind.bkSolid;
  SetMatrix(TMatrix.Identity);
  Result := True;
end;

procedure TCanvas.DoBlendingChanged;
begin

end;

class procedure TCanvas.DoCopyBitmap(const Source, Dest: TBitmap);
var
  S, D: TBitmapData;
begin
  if Source.Map(TMapAccess.maRead, S) and Dest.Map(TMapAccess.maWrite, D) then
  try
    D.Copy(S);
  finally
    Source.Unmap(S);
    Dest.Unmap(D);
  end;
end;

procedure TCanvas.DoEndScene;
begin
  if FBitmap <> nil then
    FBitmap.BitmapChanged;
end;

function TCanvas.AlignToPixel(const Value: TPointF): TPointF;
begin
  case FMatrixMeaning of
    TMatrixMeaning.mmIdentity:
      Result := TPointF.Create(Round(Value.X * FScale) / FScale, Round(Value.Y * FScale) / FScale);
    TMatrixMeaning.mmTranslate:
      Result := TPointF.Create(Round((Matrix.m31 + Value.X) * FScale) / FScale - Matrix.m31,
        Round((Matrix.m32 + Value.Y) * FScale) / FScale - Matrix.m32);
  else
    Result := Value;
  end;
end;

function TCanvas.AlignToPixelHorizontally(const Value: Single): Single;
begin
  case FMatrixMeaning of
    TMatrixMeaning.mmIdentity: Result := Round(Value * FScale) / FScale;
    TMatrixMeaning.mmTranslate: Result := Round((Matrix.m31 + Value) * FScale) / FScale - Matrix.m31;
  else
    Result := Value;
  end;
end;

function TCanvas.AlignToPixelVertically(const Value: Single): Single;
begin
  case FMatrixMeaning of
    TMatrixMeaning.mmIdentity: Result := Round(Value * FScale) / FScale;
    TMatrixMeaning.mmTranslate: Result := Round((Matrix.m32 + Value) * FScale) / FScale - Matrix.m32;
  else
    Result := Value;
  end;
end;

function TCanvas.BeginScene(AClipRects: PClipRects = nil; AContextHandle: THandle = 0): Boolean;
begin
  if FBeginSceneCount = 0 then
    Result := (Width > 0) and (Height > 0) and DoBeginScene(AClipRects, AContextHandle)
  else
    Result := FBeginSceneCount > 0;
  if Result then
    inc(FBeginSceneCount);
end;

procedure TCanvas.EndScene;
begin
  if FBeginSceneCount = 1 then
    DoEndScene;
  if FBeginSceneCount > 0 then
    dec(FBeginSceneCount);
end;

procedure TCanvas.SetMatrix(const M: TMatrix);
begin
  FMatrixMeaning := TMatrixMeaning.mmUnknown;
  FMatrix := M;

  { Check for identity matrix values. It is assumed that the matrix is composed of
    three vectors of unit length, so by comparing some specific values with one,
    we discard any other possibility of other vectors. }
  if SameValue(FMatrix.m11, 1, TEpsilon.Matrix) and SameValue(FMatrix.m22, 1, TEpsilon.Matrix) and
    SameValue(FMatrix.m33, 1, TEpsilon.Matrix) then
  begin
    if SameValue(FMatrix.m31, 0, TEpsilon.Matrix) and SameValue(FMatrix.m32, 0, TEpsilon.Matrix) then
    begin // If no translation is present, we have an identity matrix.
      FMatrixMeaning := TMatrixMeaning.mmIdentity;
    end
    else
    begin // Translation information is present in the matrix.
      FMatrixMeaning := TMatrixMeaning.mmTranslate;

      FMatrixTranslate.X := FMatrix.m31;
      FMatrixTranslate.Y := FMatrix.m32;
    end;
  end;
end;

procedure TCanvas.MultiplyMatrix(const M: TMatrix);
var
  MulMatrix: TMatrix;
begin
  MulMatrix := FMatrix * M;
  SetMatrix(MulMatrix);
end;

function TCanvas.CreateSaveState: TCanvasSaveState;
begin
  Result := TCanvasSaveState.Create;
end;

procedure TCanvas.RestoreState(const State: TCanvasSaveState);
begin
  if FCanvasSaveData.IndexOf(State) >= 0 then
    Assign(State);
end;

procedure TCanvas.FontChanged(Sender: TObject);
begin
end;

class procedure TCanvas.InitializeBitmap(const Bitmap: TBitmap);
begin
  if Assigned(Bitmap) and not Bitmap.IsEmpty then
    DoInitializeBitmap(Bitmap);
end;

class procedure TCanvas.FinalizeBitmap(const Bitmap: TBitmap);
begin
  if Assigned(Bitmap) and (Bitmap.FHandle <> 0) then
  begin
    DoFinalizeBitmap(Bitmap);
    Bitmap.FHandle := 0;
  end;
end;

class function TCanvas.MapBitmap(const Bitmap: TBitmap; const Access: TMapAccess; var Data: TBitmapData): Boolean;
begin
  if Assigned(Bitmap) and Bitmap.HandleAllocated then
    Result := DoMapBitmap(Bitmap, Access, Data)
  else
    Result := False;
end;

class procedure TCanvas.UnmapBitmap(const Bitmap: TBitmap; var Data: TBitmapData);
begin
  if Assigned(Bitmap) and Bitmap.HandleAllocated then
    DoUnmapBitmap(Bitmap, Data);
end;

function TCanvas.LoadFontFromStream(const AStream: TStream): Boolean;
begin
  Result := False;
                                                       
end;

procedure TCanvas.MeasureLines(const ALines: TLineMetricInfo; const ARect: TRectF; const AText: string; const WordWrap: Boolean; const Flags: TFillTextFlags;
  const ATextAlign: TTextAlign; const AVTextAlign: TTextAlign = TTextAlign.taCenter);
var
  WStartChar, WSaveChar, WCurChar, WCutOffChar: Integer;
  LCurChar: Integer;
  TmpS: string;
  WWidth: Single;
  LEditRectWidth: Single;
  Tok, LText: string;

  function _IsSurrogate(Surrogate: WideChar): Boolean;
  begin
    Result := (Integer(Surrogate) >= $D800) and (Integer(Surrogate) <= $DFFF);
  end;

  procedure _SkipSeparators(var Pos: Integer; const S: string);
  const
    // #$0020   SPACE
    // #$0021 ! EXCLAMATION MARK
    // #$002C , COMMA
    // #$002D - HYPHEN-MINUS
    // #$002E . FULL STOP
    // #$003A : COLON
    // #$003B ; SEMICOLON
    // #$003F ? QUESTION MARK
    BasicSeparatos: string = #$0020#$0021#$002C#$002D#$002E#$003A#$003B#$003F;
    MaxBasicSeparators: WideChar = #$003F;
  var
    ch: WideChar;
  begin
    while Pos < S.Length do
    begin
      ch := S.Chars[Pos];
      if (ch > MaxBasicSeparators) or not BasicSeparatos.Contains(ch) then
        Break;
      if _IsSurrogate(ch) then
        Inc(Pos, 2)
      else
        Inc(Pos);
    end;
  end;

  function _WideGetToken(var Pos: Integer; const S: string): string;
  const
  //#$0020   SPACE
  //#$0021 ! EXCLAMATION MARK
  //#$002C , COMMA
  //#$002D - HYPHEN-MINUS
  //#$002E . FULL STOP
  //#$003A : COLON
  //#$003B ; SEMICOLON
  //#$003F ? QUESTION MARK
    BasicSeparatos: string = #$0020#$0021#$002C#$002D#$002E#$003A#$003B#$003F;
    MaxBasicSeparators: WideChar = #$003F;
  var
    ch: WideChar;
  begin
    Result := '';
    { skip first separators }
    _SkipSeparators(Pos, S);

    { get }
    while Pos < S.Length do
    begin
      ch := S.Chars[Pos];
      if (ch <= MaxBasicSeparators) and BasicSeparatos.Contains(ch) then
        Break;
      if _IsSurrogate(ch) then
      begin
        Result := Result + S.Substring(Pos, 2);
        Inc(Pos, 2)
      end
      else
      begin
        Result := Result + S.Chars[Pos];
        Inc(Pos);
      end;
    end;

    { skip separators }
    _SkipSeparators(Pos, S);
  end;

  function RoundToPowerOf2(I: Integer): Integer;
  begin
    I := I or (I shr 1);
    I := I or (I shr 2);
    I := I or (I shr 4);
    I := I or (I shr 8);
    I := I or (I shr 16);
    Result := I + 1;
  end;

  function CutOffPoint(TmpS: string; Width: Single): integer;
  var
    W : Single;
    Delta: Integer;
  begin
    Delta := RoundToPowerOf2(Tmps.Length) div 2;
    Result := Delta;

    while Delta > 0 do
    begin
      W := TextWidth(TmpS.Substring(0, Result));
      if W > Width then
        Result := Result - Delta;
      Delta := Delta div 2;
      Result := Result + Delta;
    end;
  end;

begin
  ALines.Count := 0;
  if AText = '' then
    Exit;

  ALines.Count := 1;
  LEditRectWidth := ARect.Width;

  // first check linecreaks
  LText := AText;
  TmpS := '';

  LCurChar := 0;

  ALines.Count := 1;
  ALines.Metrics[0].Index := 1;
  while LCurChar < LText.Length do
  begin
    if (LText.Chars[LCurChar] = #13) or (LText.Chars[LCurChar] = #10) then
    begin
      if (LText.Chars[LCurChar] = #13) and (LCurChar + 1 < LText.Length) then
        if LText.Chars[LCurChar + 1] = #10 then
          Inc(LCurChar);

      if WordWrap and (TextWidth(TmpS) > LEditRectWidth) then
      begin
        WCurChar := 0;
        WStartChar := 0;
        WSaveChar := 0;
        Tok := _WideGetToken(WCurChar, TmpS);
        while Tok <> '' do
        begin
          WWidth := TextWidth(TmpS.Substring(WStartChar, WCurChar - WStartChar));
          if WWidth > LEditRectWidth then
          begin
            if WSaveChar = WStartChar then
            begin
              WCutOffChar := CutOffPoint(TmpS.Substring(WStartChar, WCurChar - WStartChar), LEditRectWidth);
              ALines.Metrics[ALines.Count - 1].Len := WCutoffChar;
              WCurChar := WStartChar + WCutOffChar;
              WStartChar := WStartChar + WCutOffChar;
            end
            else
            begin
              ALines.Metrics[ALines.Count - 1].Len := WSaveChar - WStartChar;
              WStartChar := WSaveChar;
            end;
            ALines.Count := ALines.Count + 1;
            ALines.Metrics[ALines.Count - 1].Index :=
              ALines.Metrics[ALines.Count - 2].Index + ALines.Metrics[ALines.Count - 2].Len;
          end;
          WSaveChar := WCurChar;
          Tok := _WideGetToken(WCurChar, TmpS);
          if WSaveChar = WCurChar then
            Break; { !!! - error }
        end;

        ALines.Metrics[ALines.Count - 1].Len := WCurChar - WStartChar;
      end
      else
        ALines.Metrics[ALines.Count - 1].Len := Length(Tmps);

      ALines.Count := ALines.Count + 1;
      ALines.Metrics[ALines.Count - 1].Index := LCurChar + 2;

      TmpS := '';
    end
    else
      TmpS := TmpS + LText.Chars[LCurChar];
    Inc(LCurChar);
  end;

// last line
  if WordWrap and (TextWidth(TmpS) > LEditRectWidth) then
  begin
    WCurChar := 0;
    WStartChar := 0;
    WSaveChar := 0;
    Tok := _WideGetToken(WCurChar, TmpS);
    while Tok <> '' do
    begin
      Tok := TmpS.Substring(WStartChar, WCurChar - WStartChar);
      WWidth := TextWidth(TmpS.Substring(WStartChar, WCurChar - WStartChar));
      if WWidth > LEditRectWidth then
      begin
        if WSaveChar = WStartChar then
        begin
          WCutOffChar := CutOffPoint(TmpS.Substring(WStartChar, WCurChar - WStartChar), LEditRectWidth);
          ALines.Metrics[ALines.Count - 1].Len := WCutoffChar;
          WCurChar := WStartChar + WCutOffChar;
          WStartChar := WStartChar + WCutOffChar;
        end
        else
        begin
          ALines.Metrics[ALines.Count - 1].Len := WSaveChar - WStartChar;
          WStartChar := WSaveChar;
        end;
        ALines.Count := ALines.Count + 1;
        ALines.Metrics[ALines.Count - 1].Index :=
          ALines.Metrics[ALines.Count - 2].Index + ALines.Metrics[ALines.Count - 2].Len;
      end;

      WSaveChar := WCurChar;
      Tok := _WideGetToken(WCurChar, TmpS);
      if WSaveChar = WCurChar then
        Break; { !!! - error }
    end;
    ALines.Metrics[ALines.Count - 1].Len := WCurChar - WStartChar;
  end
  else
    ALines.Metrics[ALines.Count - 1].Len := Length(Tmps);
end;

procedure TCanvas.MeasureText(var ARect: TRectF; const AText: string;
  const WordWrap: Boolean; const Flags: TFillTextFlags; const ATextAlign,
  AVTextAlign: TTextAlign);
var
  Layout: TTextLayout;
begin
  if AText.IsEmpty then
  begin
    ARect.Right := ARect.Left;
    Exit;
  end;

  Layout := TTextLayoutManager.TextLayoutByCanvas(Self.ClassType).Create(Self);
  try
    Layout.BeginUpdate;
    Layout.TopLeft := ARect.TopLeft;
    Layout.MaxSize := PointF(ARect.Width, ARect.Height);
    Layout.Text := AText;
    Layout.WordWrap := WordWrap;
    Layout.HorizontalAlign := ATextAlign;
    Layout.VerticalAlign := AVTextAlign;
    Layout.Font := Self.Font;
    Layout.Color := Self.Fill.Color;
    Layout.RightToLeft := TFillTextFlag.ftRightToLeft in Flags;
    Layout.EndUpdate;
    ARect := Layout.TextRect;
  finally
    FreeAndNil(Layout);
  end;
end;

function TCanvas.TextHeight(const AText: string): Single;
var
  R: TRectF;
begin
  R := RectF(0, 0, 10000, 10000);
  MeasureText(R, AText, False, [], TTextAlign.taLeading, TTextAlign.taLeading);
  Result := R.Bottom;
end;

function TCanvas.TextToPath(Path: TPathData; const ARect: TRectF;
  const AText: string; const WordWrap: Boolean; const ATextAlign,
  AVTextAlign: TTextAlign): Boolean;
var
  Layout: TTextLayout;
begin
  if AText.IsEmpty then
    Exit(False);

  Layout := TTextLayoutManager.TextLayoutByCanvas(Self.ClassType).Create;
  try
    Layout.BeginUpdate;
    Layout.TopLeft := ARect.TopLeft;
    Layout.MaxSize := PointF(ARect.Width, ARect.Height);
    Layout.Text := AText;
    Layout.WordWrap := WordWrap;
    Layout.HorizontalAlign := ATextAlign;
    Layout.VerticalAlign := AVTextAlign;
    Layout.Font := Self.Font;
    Layout.Color := Self.Fill.Color;
    Layout.EndUpdate;
    Layout.ConvertToPath(Path);
    Result := True;
  finally
    FreeAndNil(Layout);
  end;
end;

function TCanvas.TextWidth(const AText: string): Single;
var
  R: TRectF;
begin
  R := RectF(0, 0, 10000, 20);
  MeasureText(R, AText, False, [], TTextAlign.taLeading, TTextAlign.taCenter);
  Result := R.Right;
end;

function TCanvas.TransformPoint(const P: TPointF): TPointF;
begin
  case FMatrixMeaning of
    TMatrixMeaning.mmUnknown:
      Result := P * FMatrix;
    TMatrixMeaning.mmIdentity:
      Result := P;
    TMatrixMeaning.mmTranslate:
      begin
        Result.X := P.X + FMatrixTranslate.X;
        Result.Y := P.Y + FMatrixTranslate.Y;
      end;
  end;
end;

function TCanvas.TransformRect(const Rect: TRectF): TRectF;
var
  V: TVector;
begin
  case FMatrixMeaning of
    TMatrixMeaning.mmUnknown:
      begin
        V := Rect.TopLeft * FMatrix;
        Result.Left := V.X;
        Result.Top := V.Y;

        V := PointF(Rect.Right, Rect.Top) * FMatrix;
        Result.Right := V.X;
        Result.Bottom := V.Y;

        V := Rect.BottomRight * FMatrix;
        Result.Right := V.X;
        Result.Bottom := V.Y;

        V := PointF(Rect.Left, Rect.Bottom) * FMatrix;
        Result.Left := V.X;
        Result.Bottom := V.Y;
      end;
    TMatrixMeaning.mmIdentity:
      Result := Rect;
    TMatrixMeaning.mmTranslate:
      begin
        Result := Rect;
        Result.Offset(FMatrixTranslate.X, FMatrixTranslate.Y)
      end;
  end;
end;

procedure TCanvas.FillArc(const Center, Radius: TPointF; StartAngle, SweepAngle: Single; const AOpacity: Single);
begin
  FillArc(Center, Radius, StartAngle, SweepAngle, AOpacity, FFill);
end;

procedure TCanvas.FillArc(const Center, Radius: TPointF; StartAngle, SweepAngle: Single; const AOpacity: Single; const ABrush: TBrush);
var
  P: TPathData;
begin
  P := TPathData.Create;
  try
    P.AddArc(Center, Radius, StartAngle, SweepAngle);
    FillPath(P, AOpacity, ABrush);
  finally
    P.Free;
  end;
end;

procedure TCanvas.DrawArc(const Center, Radius: TPointF; StartAngle, SweepAngle: Single; const AOpacity: Single);
begin
  DrawArc(Center, Radius, StartAngle, SweepAngle, AOpacity, FStroke);
end;

procedure TCanvas.DrawArc(const Center, Radius: TPointF; StartAngle, SweepAngle: Single; const AOpacity: Single; const ABrush: TStrokeBrush);
var
  P: TPathData;
begin
  P := TPathData.Create;
  try
    P.AddArc(Center, Radius, StartAngle, SweepAngle);
    DrawPath(P, AOpacity, ABrush);
  finally
    P.Free;
  end;
end;

procedure TCanvas.DrawBitmap(const ABitmap: TBitmap; const SrcRect, DstRect: TRectF; const AOpacity: Single; const HighSpeed: Boolean);
begin
  DoDrawBitmap(ABitmap, SrcRect, DstRect, AOpacity, HighSpeed);
end;

procedure TCanvas.DrawEllipse(const ARect: TRectF; const AOpacity: Single);
begin
  DrawEllipse(ARect, AOpacity, FStroke);
end;

procedure TCanvas.DrawEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TStrokeBrush);
begin
  if ABrush.Kind <> TBrushKind.bkNone then
    DoDrawEllipse(ARect, AOpacity, ABrush);
end;

procedure TCanvas.DrawLine(const APt1, APt2: TPointF; const AOpacity: Single);
begin
  DrawLine(APt1, APt2, AOpacity, FStroke);
end;

procedure TCanvas.DrawLine(const APt1, APt2: TPointF; const AOpacity: Single; const ABrush: TStrokeBrush);
begin
  if ABrush.Kind <> TBrushKind.bkNone then
    DoDrawLine(APt1, APt2, AOpacity, ABrush);
end;

function TCanvas.SaveState: TCanvasSaveState;
var
  SaveData : TCanvasSaveState;
begin
  FSavingStateCount := FSavingStateCount + 1;
  for SaveData in FCanvasSaveData do
  begin
    if not SaveData.Assigned then
    begin
      SaveData.Assign(Self);
      Result := SaveData;
      Exit;
    end;
  end;
  Result := CreateSaveState;
  try
    Result.Assign(Self);
    FCanvasSaveData.Add(Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TCanvas.SetBlending(const Value: Boolean);
begin
  if FBlending <> Value then
  begin
    FBlending := Value;
    DoBlendingChanged;
  end;
end;

procedure TCanvas.SetCustomDash(const Dash: array of Single; Offset: Single);
begin
  Stroke.SetCustomDash(Dash, Offset);
end;

procedure TCanvas.SetFill(const Value: TBrush);
begin
  FFill.Assign(Value);
end;

procedure TCanvas.FillPath(const APath: TPathData; const AOpacity: Single);
begin
  FillPath(APath, AOpacity, FFill);
end;

procedure TCanvas.FillPath(const APath: TPathData; const AOpacity: Single; const ABrush: TBrush);
begin
  if (ABrush.Kind <> TBrushKind.bkNone) and not APath.IsEmpty then
    DoFillPath(APath, AOpacity, ABrush);
end;

procedure TCanvas.FillPolygon(const Points: TPolygon; const AOpacity: Single);
var
  I: Integer;
  Path: TPathData;
begin
  Path := TPathData.Create;
  try
    for I := 0 to High(Points) do
    begin
      if I = 0 then
        Path.MoveTo(Points[I])
      else
      if (Points[I].X = ClosePolygon.X) and (Points[I].Y = ClosePolygon.Y) then
        Path.ClosePath
      else
        Path.LineTo(Points[I]);
    end;
    Path.ClosePath;
    FillPath(Path, AOpacity);
  finally
    Path.Free;
  end;
end;

procedure TCanvas.DoFillRoundRect(const ARect: TRectF; const XRadius,
  YRadius: Single; const ACorners: TCorners; const AOpacity: Single;
  const ABrush: TBrush; const ACornerType: TCornerType = TCornerType.ctRound);
var
  Path: TPathData;
  x1, x2, y1, y2: Single;
  R: TRectF;
begin
  R := ARect;
  x1 := XRadius;
  if RectWidth(R) - (x1 * 2) < 0 then
    x1 := RectWidth(R) / 2;
  x2 := XRadius * CurveKappaInv;
  y1 := YRadius;
  if RectHeight(R) - (y1 * 2) < 0 then
    y1 := RectHeight(R) / 2;
  y2 := YRadius * CurveKappaInv;
  Path := TPathData.Create;
  Path.MoveTo(PointF(R.Left, R.Top + y1));
  if TCorner.crTopLeft in ACorners then
  begin
    case ACornerType of
      // ctRound - default
      TCornerType.ctBevel: Path.LineTo(PointF(R.Left + x1, R.Top));
      TCornerType.ctInnerRound: Path.CurveTo(PointF(R.Left + x2, R.Top + y1), PointF(R.Left + x1, R.Top + y2), PointF(R.Left + x1, R.Top));
      TCornerType.ctInnerLine:
        begin
          Path.LineTo(PointF(R.Left + x2, R.Top + y1));
          Path.LineTo(PointF(R.Left + x1, R.Top + y2));
          Path.LineTo(PointF(R.Left + x1, R.Top));
        end;
    else
      Path.CurveTo(PointF(R.Left, R.Top + (y2)), PointF(R.Left + x2, R.Top), PointF(R.Left + x1, R.Top))
    end;
  end
  else
  begin
    Path.LineTo(PointF(R.Left, R.Top));
    Path.LineTo(PointF(R.Left + x1, R.Top));
  end;
  Path.LineTo(PointF(R.Right - x1, R.Top));
  if TCorner.crTopRight in ACorners then
  begin
    case ACornerType of
      // ctRound - default
      TCornerType.ctBevel: Path.LineTo(PointF(R.Right, R.Top + y1));
      TCornerType.ctInnerRound: Path.CurveTo(PointF(R.Right - x1, R.Top + y2), PointF(R.Right - x2, R.Top + y1), PointF(R.Right, R.Top + y1));
      TCornerType.ctInnerLine:
        begin
          Path.LineTo(PointF(R.Right - x1, R.Top + y2));
          Path.LineTo(PointF(R.Right - x2, R.Top + y1));
          Path.LineTo(PointF(R.Right, R.Top + y1));
        end;
    else
      Path.CurveTo(PointF(R.Right - x2, R.Top), PointF(R.Right, R.Top + (y2)), PointF(R.Right, R.Top + y1))
    end;
  end
  else
  begin
    Path.LineTo(PointF(R.Right, R.Top));
    Path.LineTo(PointF(R.Right, R.Top + y1));
  end;
  Path.LineTo(PointF(R.Right, R.Bottom - y1));
  if TCorner.crBottomRight in ACorners then
  begin
    case ACornerType of
      // ctRound - default
      TCornerType.ctBevel: Path.LineTo(PointF(R.Right - x1, R.Bottom));
      TCornerType.ctInnerRound: Path.CurveTo(PointF(R.Right - x2, R.Bottom - y1), PointF(R.Right - x1, R.Bottom - y2), PointF(R.Right - x1, R.Bottom));
      TCornerType.ctInnerLine:
        begin
          Path.LineTo(PointF(R.Right - x2, R.Bottom - y1));
          Path.LineTo(PointF(R.Right - x1, R.Bottom - y2));
          Path.LineTo(PointF(R.Right - x1, R.Bottom));
        end;
    else
      Path.CurveTo(PointF(R.Right, R.Bottom - (y2)), PointF(R.Right - x2, R.Bottom), PointF(R.Right - x1, R.Bottom))
    end;
  end
  else
  begin
    Path.LineTo(PointF(R.Right, R.Bottom));
    Path.LineTo(PointF(R.Right - x1, R.Bottom));
  end;
  Path.LineTo(PointF(R.Left + x1, R.Bottom));
  if TCorner.crBottomLeft in ACorners then
  begin
    case ACornerType of
      // ctRound - default
      TCornerType.ctBevel: Path.LineTo(PointF(R.Left, R.Bottom - y1));
      TCornerType.ctInnerRound: Path.CurveTo(PointF(R.Left + x1, R.Bottom - y2), PointF(R.Left + x2, R.Bottom - y1), PointF(R.Left, R.Bottom - y1));
      TCornerType.ctInnerLine:
        begin
          Path.LineTo(PointF(R.Left + x1, R.Bottom - y2));
          Path.LineTo(PointF(R.Left + x2, R.Bottom - y1));
          Path.LineTo(PointF(R.Left, R.Bottom - y1));
        end;
    else
      Path.CurveTo(PointF(R.Left + x2, R.Bottom), PointF(R.Left, R.Bottom - (y2)), PointF(R.Left, R.Bottom - y1))
    end;
  end
  else
  begin
    Path.LineTo(PointF(R.Left, R.Bottom));
    Path.LineTo(PointF(R.Left, R.Bottom - y1));
  end;
  Path.ClosePath;
  DoFillPath(Path, AOpacity, ABrush);
  Path.Free;
end;

procedure TCanvas.FillRect(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners; const AOpacity: Single;
  const ACornerType: TCornerType);
begin
  FillRect(ARect, XRadius, YRadius, ACorners, AOpacity, FFill, ACornerType);
end;

procedure TCanvas.FillRect(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners; const AOpacity: Single;
  const ABrush: TBrush; const ACornerType: TCornerType);
begin
  if ABrush.Kind <> TBrushKind.bkNone then
  begin
    if ((XRadius = 0) and (YRadius = 0)) or (ACorners = []) then
      DoFillRect(ARect, AOpacity, ABrush)
    else
      DoFillRoundRect(ARect, XRadius, YRadius, ACorners, AOpacity, ABrush, ACornerType)
  end;
end;

procedure TCanvas.FillEllipse(const ARect: TRectF; const AOpacity: Single);
begin
  FillEllipse(ARect, AOpacity, FFill);
end;

procedure TCanvas.FillEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TBrush);
begin
  if ABrush.Kind <> TBrushKind.bkNone then
    DoFillEllipse(ARect, AOpacity, ABrush);
end;

procedure TCanvas.FillText(const ARect: TRectF; const AText: string;
  const WordWrap: Boolean; const AOpacity: Single; const Flags: TFillTextFlags;
  const ATextAlign, AVTextAlign: TTextAlign);
var
  Layout: TTextLayout;
begin
  Layout := TTextLayoutManager.TextLayoutByCanvas(Self.ClassType).Create(Self);
  try
    Layout.BeginUpdate;
    Layout.TopLeft := ARect.TopLeft;
    Layout.MaxSize := PointF(ARect.Width, ARect.Height);
    Layout.Text := AText;
    Layout.WordWrap := WordWrap;
    Layout.Opacity := AOpacity;
    Layout.HorizontalAlign := ATextAlign;
    Layout.VerticalAlign := AVTextAlign;
    Layout.Font := Self.Font;
    Layout.Color := Self.Fill.Color;
    Layout.RightToLeft := TFillTextFlag.ftRightToLeft in Flags;
    Layout.EndUpdate;
    Layout.RenderLayout(Self);
  finally
    FreeAndNil(Layout);
  end;
end;

procedure TCanvas.DrawPath(const APath: TPathData; const AOpacity: Single);
begin
  DrawPath(APath, AOpacity, FStroke);
end;

procedure TCanvas.DrawPath(const APath: TPathData; const AOpacity: Single; const ABrush: TStrokeBrush);
begin
  if (ABrush.Kind <> TBrushKind.bkNone) and not APath.IsEmpty then
    DoDrawPath(APath, AOpacity, ABrush);
end;

procedure TCanvas.DrawPolygon(const Points: TPolygon; const AOpacity: Single);
var
  I: Integer;
  Path: TPathData;
begin
  Path := TPathData.Create;
  try
    for I := 0 to High(Points) do
    begin
      if I = 0 then
        Path.MoveTo(Points[I])
      else
      if (Points[I].X = ClosePolygon.X) and (Points[I].Y = ClosePolygon.Y) then
        Path.ClosePath
      else
        Path.LineTo(Points[I]);
    end;
    DrawPath(Path, AOpacity);
  finally
    Path.Free;
  end;
end;

procedure TCanvas.DrawRect(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners; const AOpacity: Single;
  const ACornerType: TCornerType);
begin
  DrawRect(ARect, XRadius, YRadius, ACorners, AOpacity, FStroke, ACornerType);
end;

procedure TCanvas.DrawRect(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners; const AOpacity: Single;
  const ABrush: TStrokeBrush; const ACornerType: TCornerType);
var
  Path: TPathData;
  x1, x2, y1, y2: Single;
  R: TRectF;
begin
  if ABrush.Kind <> TBrushKind.bkNone then
  begin
    R := ARect;
    if ((XRadius = 0) and (YRadius = 0)) or (ACorners = []) then
      DoDrawRect(ARect, AOpacity, ABrush)
    else begin
      R := ARect;
      x1 := XRadius;
      if RectWidth(R) - (x1 * 2) < 0 then
        x1 := RectWidth(R) / 2;
      x2 := XRadius * CurveKappaInv;
      y1 := YRadius;
      if RectHeight(R) - (y1 * 2) < 0 then
        y1 := RectHeight(R) / 2;
      y2 := YRadius * CurveKappaInv;
      Path := TPathData.Create;
      Path.MoveTo(PointF(R.Left, R.Top + y1));
      if TCorner.crTopLeft in ACorners then
      begin
        case ACornerType of
          // ctRound - default
          TCornerType.ctBevel: Path.LineTo(PointF(R.Left + x1, R.Top));
          TCornerType.ctInnerRound: Path.CurveTo(PointF(R.Left + x2, R.Top + y1), PointF(R.Left + x1, R.Top + y2), PointF(R.Left + x1, R.Top));
          TCornerType.ctInnerLine:
            begin
              Path.LineTo(PointF(R.Left + x2, R.Top + y1));
              Path.LineTo(PointF(R.Left + x1, R.Top + y2));
              Path.LineTo(PointF(R.Left + x1, R.Top));
            end;
        else
          Path.CurveTo(PointF(R.Left, R.Top + (y2)), PointF(R.Left + x2, R.Top), PointF(R.Left + x1, R.Top))
        end;
      end
      else
      begin
        Path.LineTo(PointF(R.Left, R.Top));
        Path.LineTo(PointF(R.Left + x1, R.Top));
      end;
      Path.LineTo(PointF(R.Right - x1, R.Top));
      if TCorner.crTopRight in ACorners then
      begin
        case ACornerType of
          // ctRound - default
          TCornerType.ctBevel: Path.LineTo(PointF(R.Right, R.Top + y1));
          TCornerType.ctInnerRound: Path.CurveTo(PointF(R.Right - x1, R.Top + y2), PointF(R.Right - x2, R.Top + y1), PointF(R.Right, R.Top + y1));
          TCornerType.ctInnerLine:
            begin
              Path.LineTo(PointF(R.Right - x1, R.Top + y2));
              Path.LineTo(PointF(R.Right - x2, R.Top + y1));
              Path.LineTo(PointF(R.Right, R.Top + y1));
            end;
        else
          Path.CurveTo(PointF(R.Right - x2, R.Top), PointF(R.Right, R.Top + (y2)), PointF(R.Right, R.Top + y1))
        end;
      end
      else
      begin
        Path.LineTo(PointF(R.Right, R.Top));
        Path.LineTo(PointF(R.Right, R.Top + y1));
      end;
      Path.LineTo(PointF(R.Right, R.Bottom - y1));
      if TCorner.crBottomRight in ACorners then
      begin
        case ACornerType of
          // ctRound - default
          TCornerType.ctBevel: Path.LineTo(PointF(R.Right - x1, R.Bottom));
          TCornerType.ctInnerRound: Path.CurveTo(PointF(R.Right - x2, R.Bottom - y1), PointF(R.Right - x1, R.Bottom - y2), PointF(R.Right - x1, R.Bottom));
          TCornerType.ctInnerLine:
            begin
              Path.LineTo(PointF(R.Right - x2, R.Bottom - y1));
              Path.LineTo(PointF(R.Right - x1, R.Bottom - y2));
              Path.LineTo(PointF(R.Right - x1, R.Bottom));
            end;
        else
          Path.CurveTo(PointF(R.Right, R.Bottom - (y2)), PointF(R.Right - x2, R.Bottom), PointF(R.Right - x1, R.Bottom))
        end;
      end
      else
      begin
        Path.LineTo(PointF(R.Right, R.Bottom));
        Path.LineTo(PointF(R.Right - x1, R.Bottom));
      end;
      Path.LineTo(PointF(R.Left + x1, R.Bottom));
      if TCorner.crBottomLeft in ACorners then
      begin
        case ACornerType of
          // ctRound - default
          TCornerType.ctBevel: Path.LineTo(PointF(R.Left, R.Bottom - y1));
          TCornerType.ctInnerRound: Path.CurveTo(PointF(R.Left + x1, R.Bottom - y2), PointF(R.Left + x2, R.Bottom - y1), PointF(R.Left, R.Bottom - y1));
          TCornerType.ctInnerLine:
            begin
              Path.LineTo(PointF(R.Left + x1, R.Bottom - y2));
              Path.LineTo(PointF(R.Left + x2, R.Bottom - y1));
              Path.LineTo(PointF(R.Left, R.Bottom - y1));
            end;
        else
          Path.CurveTo(PointF(R.Left + x2, R.Bottom), PointF(R.Left, R.Bottom - (y2)), PointF(R.Left, R.Bottom - y1))
        end;
      end
      else
      begin
        Path.LineTo(PointF(R.Left, R.Bottom));
        Path.LineTo(PointF(R.Left, R.Bottom - y1));
      end;
      Path.ClosePath;
      DoDrawPath(Path, AOpacity, ABrush);
      Path.Free;
    end;
  end;
end;

procedure TCanvas.DrawDashRect(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
      const AOpacity: Single; const AColor: TAlphaColor);
var
  Brush: TStrokeBrush;
begin
  Brush := TStrokeBrush.Create(TBrushKind.bkSolid, AColor);
  Brush.Thickness := 1;
  Brush.Dash := TStrokeDash.sdDash;
  Brush.Kind := TBrushKind.bkSolid;
  DrawRect(ARect, XRadius, YRadius, ACorners, AOpacity, Brush);
  Brush.Free;
end;

procedure TCanvas.DrawRectSides(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
  const AOpacity: Single; const ASides: TSides; const ACornerType: TCornerType = TCornerType.ctRound);
begin
  DrawRectSides(ARect, XRadius, YRadius, ACorners, AOpacity, ASides, FStroke, ACornerType);
end;

procedure TCanvas.DrawRectSides(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
  const AOpacity: Single; const ASides: TSides; const ABrush: TStrokeBrush; const ACornerType: TCornerType = TCornerType.ctRound);
var
  Path: TPathData;
  X1, X2, Y1, Y2: Single;
  R: TRectF;
begin
  R := ARect;
  X1 := XRadius;
  if RectWidth(R) - (X1 * 2) < 0 then
    if X1 <> 0 then // guard divide by zero
      X1 := (XRadius * (RectWidth(R) / (X1 * 2)));
  X2 := X1 / 2;
  Y1 := YRadius;
  if RectHeight(R) - (Y1 * 2) < 0 then
    if Y1 <> 0 then // guard divide by zero
      Y1 := (YRadius * (RectHeight(R) / (Y1 * 2)));
  Y2 := Y1 / 2;
  Path := TPathData.Create;
  try
    Path.MoveTo(PointF(R.Left, R.Top + Y1));
    if TCorner.crTopLeft in ACorners then
    begin
      case ACornerType of
        // ctRound - default
        TCornerType.ctBevel:
          Path.LineTo(PointF(R.Left + X1, R.Top));
        TCornerType.ctInnerRound:
          Path.CurveTo(PointF(R.Left + X2, R.Top + Y1), PointF(R.Left + X1, R.Top + Y2), PointF(R.Left + X1, R.Top));
        TCornerType.ctInnerLine:
          begin
            Path.LineTo(PointF(R.Left + X2, R.Top + Y1));
            Path.LineTo(PointF(R.Left + X1, R.Top + Y2));
            Path.LineTo(PointF(R.Left + X1, R.Top));
          end;
      else
        Path.CurveTo(PointF(R.Left, R.Top + (Y2)), PointF(R.Left + X2, R.Top), PointF(R.Left + X1, R.Top))
      end;
    end
    else
    begin
      if TSide.sdLeft in ASides then
        Path.LineTo(PointF(R.Left, R.Top))
      else
        Path.MoveTo(PointF(R.Left, R.Top));
      if TSide.sdTop in ASides then
        Path.LineTo(PointF(R.Left + X1, R.Top))
      else
        Path.MoveTo(PointF(R.Left + X1, R.Top));
    end;
    if not(TSide.sdTop in ASides) then
      Path.MoveTo(PointF(R.Right - X1, R.Top))
    else
      Path.LineTo(PointF(R.Right - X1, R.Top));
    if TCorner.crTopRight in ACorners then
    begin
      case ACornerType of
        // ctRound - default
        TCornerType.ctBevel:
          Path.LineTo(PointF(R.Right, R.Top + Y1));
        TCornerType.ctInnerRound:
          Path.CurveTo(PointF(R.Right - X1, R.Top + Y2), PointF(R.Right - X2, R.Top + Y1), PointF(R.Right, R.Top + Y1));
        TCornerType.ctInnerLine:
          begin
            Path.LineTo(PointF(R.Right - X1, R.Top + Y2));
            Path.LineTo(PointF(R.Right - X2, R.Top + Y1));
            Path.LineTo(PointF(R.Right, R.Top + Y1));
          end;
      else
        Path.CurveTo(PointF(R.Right - X2, R.Top), PointF(R.Right, R.Top + (Y2)), PointF(R.Right, R.Top + Y1))
      end;
    end
    else
    begin
      if TSide.sdTop in ASides then
        Path.LineTo(PointF(R.Right, R.Top))
      else
        Path.MoveTo(PointF(R.Right, R.Top));
      if TSide.sdRight in ASides then
        Path.LineTo(PointF(R.Right, R.Top + Y1))
      else
        Path.MoveTo(PointF(R.Right, R.Top + Y1));
    end;
    if not(TSide.sdRight in ASides) then
      Path.MoveTo(PointF(R.Right, R.Bottom - Y1))
    else
      Path.LineTo(PointF(R.Right, R.Bottom - Y1));
    if TCorner.crBottomRight in ACorners then
    begin
      case ACornerType of
        // ctRound - default
        TCornerType.ctBevel:
          Path.LineTo(PointF(R.Right - X1, R.Bottom));
        TCornerType.ctInnerRound:
          Path.CurveTo(PointF(R.Right - X2, R.Bottom - Y1), PointF(R.Right - X1, R.Bottom - Y2),
            PointF(R.Right - X1, R.Bottom));
        TCornerType.ctInnerLine:
          begin
            Path.LineTo(PointF(R.Right - X2, R.Bottom - Y1));
            Path.LineTo(PointF(R.Right - X1, R.Bottom - Y2));
            Path.LineTo(PointF(R.Right - X1, R.Bottom));
          end;
      else
        Path.CurveTo(PointF(R.Right, R.Bottom - (Y2)), PointF(R.Right - X2, R.Bottom), PointF(R.Right - X1, R.Bottom))
      end;
    end
    else
    begin
      if TSide.sdRight in ASides then
        Path.LineTo(PointF(R.Right, R.Bottom))
      else
        Path.MoveTo(PointF(R.Right, R.Bottom));
      if TSide.sdBottom in ASides then
        Path.LineTo(PointF(R.Right - X1, R.Bottom))
      else
        Path.MoveTo(PointF(R.Right - X1, R.Bottom));
    end;
    if not(TSide.sdBottom in ASides) then
      Path.MoveTo(PointF(R.Left + X1, R.Bottom))
    else
      Path.LineTo(PointF(R.Left + X1, R.Bottom));
    if TCorner.crBottomLeft in ACorners then
    begin
      case ACornerType of
        // ctRound - default
        TCornerType.ctBevel:
          Path.LineTo(PointF(R.Left, R.Bottom - Y1));
        TCornerType.ctInnerRound:
          Path.CurveTo(PointF(R.Left + X1, R.Bottom - Y2), PointF(R.Left + X2, R.Bottom - Y1),
            PointF(R.Left, R.Bottom - Y1));
        TCornerType.ctInnerLine:
          begin
            Path.LineTo(PointF(R.Left + X1, R.Bottom - Y2));
            Path.LineTo(PointF(R.Left + X2, R.Bottom - Y1));
            Path.LineTo(PointF(R.Left, R.Bottom - Y1));
          end;
      else
        Path.CurveTo(PointF(R.Left + X2, R.Bottom), PointF(R.Left, R.Bottom - (Y2)), PointF(R.Left, R.Bottom - Y1))
      end;
    end
    else
    begin
      if TSide.sdBottom in ASides then
        Path.LineTo(PointF(R.Left, R.Bottom))
      else
        Path.MoveTo(PointF(R.Left, R.Bottom));
      if TSide.sdLeft in ASides then
        Path.LineTo(PointF(R.Left, R.Bottom - Y1))
      else
        Path.MoveTo(PointF(R.Left, R.Bottom - Y1));
    end;
    if (TSide.sdLeft in ASides) then
    begin
      Path.LineTo(PointF(R.Left, R.Top + Y1));
    end;
    DrawPath(Path, AOpacity, ABrush);
  finally
    Path.Free;
  end;
end;

{ Deprecated }

function TCanvas.GetCanvasScale: Single;
begin
  Result := 1.0;
end;

class function TCanvas.GetCanvasStyle: TCanvasStyles;
begin
  Result := [TCanvasStyle.SupportClipRects];
end;

function TCanvas.GetStrokeThickness: Single;
begin
  Result := Stroke.Thickness;
end;

procedure TCanvas.SetSize(const AWidth, AHeight: Integer);
begin
  FWidth := AWidth;
  FHeight := AHeight;
end;

procedure TCanvas.SetStrokeCap(const Value: TStrokeCap);
begin
  FStroke.Cap := Value;
end;

procedure TCanvas.SetStrokeDash(const Value: TStrokeDash);
begin
  FStroke.Dash := Value;
end;

procedure TCanvas.SetStrokeJoin(const Value: TStrokeJoin);
begin
  FStroke.Join := Value;
end;

procedure TCanvas.SetStrokeThickness(const Value: Single);
begin
  FStroke.Thickness := Value;
end;

{ TBrushObject }

constructor TBrushObject.Create(AOwner: TComponent);
begin
  inherited;
  FBrush := TBrush.Create(TBrushKind.bkSolid, $FFFFFFFF);
end;

destructor TBrushObject.Destroy;
begin
  FreeAndNil(FBrush);
  inherited;
end;

function TBrushObject.GetBrush: TBrush;
begin
  Result := FBrush;
end;

procedure TBrushObject.SetName(const NewName: TComponentName);
begin
  inherited;
  if FStyleName = '' then
    FStyleName := Name;
end;

{ TFontObject }

constructor TFontObject.Create(AOwner: TComponent);
begin
  inherited;
  FFont := TFont.Create;
end;

destructor TFontObject.Destroy;
begin
  FreeAndNil(FFont);
  inherited;
end;

function TFontObject.GetFont: TFont;
begin
  Result := FFont;
end;

procedure TFontObject.SetName(const NewName: TComponentName);
begin
  inherited;
  if FStyleName = '' then
    FStyleName := Name;
end;

{ TPathObject }

constructor TPathObject.Create(AOwner: TComponent);
begin
  inherited;
  FPath := TPathData.Create();
end;

destructor TPathObject.Destroy;
begin
  FreeAndNil(FPath);
  inherited;
end;

function TPathObject.GetPath: TPathData;
begin
  Result := FPath;
end;

procedure TPathObject.SetName(const NewName: TComponentName);
begin
  inherited;
  if FStyleName = '' then
    FStyleName := Name;
end;

{ TBitmapObject }

constructor TBitmapObject.Create(AOwner: TComponent);
begin
  inherited;
  FBitmap := TBitmap.Create(1, 1);
end;

destructor TBitmapObject.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited;
end;

function TBitmapObject.GetBitmap: TBitmap;
begin
  Result := FBitmap;
end;

procedure TBitmapObject.SetName(const NewName: TComponentName);
begin
  inherited;
  if FStyleName = '' then
    FStyleName := Name;
end;

{ TCanvasSaveState }

procedure TCanvasSaveState.Assign(Source: TPersistent);
var
  LCanvas: TCanvas;
begin
  if Source is TCanvas then
  begin
    LCanvas := TCanvas(Source);
    Self.FAssigned := True;
    Self.FMatrix := LCanvas.FMatrix;
    Self.FFill.Assign(LCanvas.Fill);
    Self.FStroke.Assign(LCanvas.Stroke);
    Self.FFont.Assign(LCanvas.Font);
  end else
    inherited;
end;

procedure TCanvasSaveState.AssignTo(Dest: TPersistent);
var
  LCanvas: TCanvas;
begin
  if Dest is TCanvas then
  begin
    LCanvas := TCanvas(Dest);
    Self.FAssigned := False;
    LCanvas.SetMatrix(Self.FMatrix);
    LCanvas.Fill.Assign(Self.FFill);
    LCanvas.Stroke.Assign(Self.FStroke);
    LCanvas.Font.Assign(Self.FFont);
  end else
    inherited;
end;

constructor TCanvasSaveState.Create;
begin
  inherited Create;
  FFont := TFont.Create;
  FFill := TBrush.Create(TBrushKind.bkSolid, TAlphaColors.Black);
  FStroke := TStrokeBrush.Create(TBrushKind.bkSolid, TAlphaColors.White);
end;

destructor TCanvasSaveState.Destroy;
begin
  FFont.Free;
  FFill.Free;
  FStroke.Free;
  inherited;
end;

{ TColorObject }

procedure TColorObject.SetName(const NewName: TComponentName);
begin
  inherited;
  if StyleName = '' then
    StyleName := Name;
end;


initialization
  RegisterFmxClasses([TBrushObject, TFontObject, TPathObject, TBitmapObject, TColorObject]);
end.

