{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit FMX.Types;

{$DEFINE STYLERESOURCECACHE}

{$MINENUMSIZE 4}
{$H+}

interface

uses
  System.Types, System.UITypes, System.UIConsts, System.Math, System.Variants,
  System.Classes, System.Actions, System.SysUtils, System.StrUtils, System.Rtti,
  System.Generics.Collections, System.Generics.Defaults, FMX.ActnList,
  FMX.Messages;

{$SCOPEDENUMS ON}

const
{$HPPEMIT '#define FireMonkeyVersion 19.0'}
  FireMonkeyVersion = 19.0;
{$EXTERNALSYM FireMonkeyVersion}

{ Global Settings }

var
  GlobalUseHWEffects: Boolean = True;
  // On low-end hardware or mobile bitmap effects are slowly
  GlobalDisableFocusEffect: Boolean = False;
  // Use Direct3D 10 in Windows Vista or Windows 7 by default
  GlobalUseDX10: Boolean = True;
  // Use Direct3D 10 in Windows Vista or Windows 7 in software mode
  GlobalUseDX10Software: Boolean = False;
  // Use Direct2D in Windows Vista or Windows 7 by default
  GlobalUseDirect2D: Boolean = True;
  // Use ClearType rendering in GDI+ renderer
  GlobalUseGDIPlusClearType: Boolean = True;
  /// <remarks> The digit of rounding, of width and height controls</remarks>
  DigitRoundSize: TRoundToRange = -3;
  // Use GPU Canvas
  GlobalUseGPUCanvas: Boolean = False;

type
  TVKAutoShowMode = (vkasDefinedBySystem, vkasNever, vkasAlways);

var
  VKAutoShowMode: TVKAutoShowMode = TVKAutoShowMode.vkasDefinedBySystem;


type

  TPointArray = array [0..0] of TPointF;

  TLongByteArray = array [0..MaxInt - 1] of Byte;
  PLongByteArray = ^TLongByteArray;

                                                             
  TCorner = (crTopLeft, crTopRight, crBottomLeft, crBottomRight);

  TCorners = set of TCorner;

  TCornerType = (ctRound, ctBevel, ctInnerRound, ctInnerLine);

  { Four courners describing arbitrary 2D rectangle }
  PCornersF = ^TCornersF;
  TCornersF = array [0 .. 3] of TPointF;

  TSide = (sdTop, sdLeft, sdBottom, sdRight);

  TSides = set of TSide;

  PAlphaColorArray = ^TAlphaColorArray;
  TAlphaColorArray = array [0 .. MaxInt div 4 - 1] of TAlphaColor;

  PAlphaColorRecArray = ^TAlphaColorRecArray;
  TAlphaColorRecArray = array [0 .. MaxInt div 4 - 1] of TAlphaColorRec;

  TTextAlign = (taCenter, taLeading, taTrailing);
  TTextTrimming = (ttNone, ttCharacter, ttWord);
  TStyledSetting = (ssFamily, ssSize, ssStyle, ssFontColor, ssOther);
  TStyledSettings = set of TStyledSetting;

  TMenuItemChange = (mcEnabled, mcVisible, mcText, mcShortcut, mcChecked, mcBitmap);
  TMenuItemChanges = set of TMenuItemChange;

  TScreenOrientation = (soPortrait, soLandscape, soInvertedPortrait, soInvertedLandscape);
  TScreenOrientations = set of TScreenOrientation;

  TPointFHelper = record helper for TPointF
  public
    // Calculates angle between two vectors, result lies in range of [0..Pi].
    function AngleBetween(const APoint: TPointF): Single; inline;
    // Calculates angle between two vectors, result lies in range of [-Pi..Pi].
    function AngleBetweenTwoSide(const APoint: TPointF): Single;
  end;

  TVectorHelper = record helper for TVector
  public
    // Calculates angle between two vectors, result lies in range of [0..Pi].
    function AngleBetween(const AVector: TVector): Single; inline;
    // Calculates angle between two vectors, result lies in range of [-Pi..Pi].
    function AngleBetweenTwoSide(const AVector: TVector): Single;
  end;

  TPoint3DHelper = record helper for TPoint3D
  public
    // Calculates angle between two vectors, result lies in range of [0..Pi].
    function AngleBetween(const APoint: TPoint3D): Single; inline;
  end;

  TVector3DHelper = record helper for TVector3D
    // Calculates angle between two vectors, result lies in range of [0..Pi].
    function AngleBetween(const AVector: TVector3D): Single; inline;
    // The fate of the following two methods is yet to be decided.
    function PointProject(const AOrigin, ADirection: TVector3D): Single;
    function CalcPlaneNormal(const AVector3D1, AVector3D2: TVector3D): TVector3D;
  end;

const
  NullRect: TRectF = (Left: 0; Top: 0; Right: 0; Bottom: 0);

  AllCorners: TCorners = [TCorner.crTopLeft, TCorner.crTopRight,
    TCorner.crBottomLeft, TCorner.crBottomRight];

  AllSides: TSides = [TSide.sdTop, TSide.sdLeft, TSide.sdBottom, TSide.sdRight];

  ClosePolygon: TPointF = (X: $FFFF; Y: $FFFF);

  AllStyledSettings: TStyledSettings = [TStyledSetting.ssFamily,
                                        TStyledSetting.ssSize,
                                        TStyledSetting.ssStyle,
                                        TStyledSetting.ssFontColor,
                                        TStyledSetting.ssOther];
  DefaultStyledSettings: TStyledSettings = [TStyledSetting.ssFamily,
                                            TStyledSetting.ssSize,
                                            TStyledSetting.ssStyle,
                                            TStyledSetting.ssFontColor];

type
  TGestureID = rgiFirst .. igiLast;

  TInteractiveGestureFlag = (gfBegin, gfInertia, gfEnd);
  TInteractiveGestureFlags = set of TInteractiveGestureFlag;

  TGestureEventInfo = record
    GestureID: TGestureID;
    Location: TPointF;
    Flags: TInteractiveGestureFlags;
    Angle: Double;
    InertiaVector: TPointF;
    Distance: Integer;
    TapLocation: TPointF;
  end;

  TGestureEvent = procedure(Sender: TObject; const EventInfo: TGestureEventInfo;
     var Handled: Boolean) of object;

type
  TFormStyle = (fsNormal, fsPopup, fsStayOnTop);

  TAlignLayout = (alNone, alTop, alLeft, alRight, alBottom, alMostTop, alMostBottom, alMostLeft, alMostRight, alClient,
    alContents, alCenter, alVertCenter, alHorzCenter, alHorizontal, alVertical, alScale, alFit, alFitLeft, alFitRight);

  TImeMode = (imDontCare, // All IMEs
              imDisable,  // All IMEs
              imClose,    // Chinese and Japanese only
              imOpen,     // Chinese and Japanese only
              imSAlpha,   // Japanese and Korea
              imAlpha,    // Japanese and Korea
              imHira,     // Japanese only
              imSKata,    // Japanese only
              imKata,     // Japanese only
              imChineseClose, // Chinese IME only
              imOnHalf,   // Chinese IME only
              imSHanguel, // Korean IME only
              imHanguel   // Korean IME only
              );

  TDragObject = record
    Source: TObject;
    Files: array of string;
    Data: TValue;
  end;

  TFmxHandle = THandle;
  TFlasherInterval = -1..1000;

const
  cIdNoTimer: TFmxHandle = TFmxHandle(-1);

type
  TCanActionExecEvent = procedure(Sender: TCustomAction; var CanExec: Boolean) of object;

  TFmxObject = class;
  TFmxObjectClass = class of TFmxObject;
  TBounds = class;
  TLineMetricInfo = class;
  TTouchManager = class;
  TCustomPopupMenu = class;

  TWindowHandle = class
  end;

  IFreeNotification = interface
    ['{FEB50EAF-A3B9-4b37-8EDB-1EF9EE2F22D4}']
    procedure FreeNotification(AObject: TObject);
  end;

  IFreeNotificationBehavior = interface
    ['{83F052C5-8696-4AFA-88F5-DCDFEF005480}']
    procedure AddFreeNotify(const AObject: IFreeNotification);
    procedure RemoveFreeNotify(const AObject: IFreeNotification);
  end;

  IIsChecked = interface
    ['{DE946EB7-0A6F-4458-AEB0-C911122630D0}']
    function GetIsChecked: boolean;
    procedure SetIsChecked(const Value: boolean);
    function IsCheckedStored: boolean;
    property IsChecked: boolean read GetIsChecked write SetIsChecked;
  end;

  IGroupName = interface (IIsChecked)
    ['{F5C14792-67AB-41F2-99C1-90C7F94102EE}']
    function GetGroupName: string;
    procedure SetGroupName(const Value: string);
    function GroupNameStored: boolean;
    property GroupName: string read GetGroupName write SetGroupName;
  end;

  TCustomCaret = class;

  ICaret = interface
    ['{F4EFFFB8-E83C-421D-B123-C370FB7BCCC7}']
    function GetObject: TCustomCaret;
    procedure ShowCaret;
    procedure HideCaret;
  end;

  IFlasher = interface
    ['{1A9163B4-47FD-45D6-A54F-70158CB01777}']
    function GetColor: TAlphaColor;
    function GetPos: TPointF;
    function GetSize: TSizeF;
    function GetVisible: Boolean;
    function GetOpacity: Single;
    function GetInterval: TFlasherInterval;
    function GetCaret: TCustomCaret;
    procedure SetCaret(const Value: TCustomCaret);

    property Color: TAlphaColor read GetColor;
    property Pos: TPointF read GetPos;
    property Size: TSizeF read GetSize;
    property Visible: boolean read GetVisible;
    property Opacity: Single read GetOpacity;
    property Interval: TFlasherInterval read GetInterval;
    property Caret: TCustomCaret read GetCaret write SetCaret;
    procedure UpdateState;
  end;

  IContainerObject = interface
    ['{DE635E60-CB00-4741-92BB-3B8F1F29A67C}']
    function GetContainerWidth: Single;
    function GetContainerHeight: Single;
    property ContainerWidth: single read GetContainerWidth;
    property ContainerHeight: single read GetContainerHeight;
  end;

  IOriginalContainerSize = interface
    ['{E76F6097-AF5D-49a1-9C7B-5127D6068059}']
    function GetOriginalContainerSize: TPointF;
    property OriginalContainerSize: TPointF read GetOriginalContainerSize;
  end;


  IContent = interface
    ['{96E89B94-2AD6-4AD3-A07C-92E66B2E6BC8}']
    function GetParent: TFmxObject;
    function GetObject: TFmxObject;
    function GetChildrenCount: Integer;
    property Parent: TFmxObject read GetParent;
    property ChildrenCount: Integer read GetChildrenCount;
  end;

  IFMXCursorService = interface(IInterface)
    ['{5D359E54-2543-414E-8268-A53292E4FDB4}']
    procedure SetCursor(const ACursor: TCursor);
    function GetCursor: TCursor;
  end;

  IFMXMouseService = interface(IInterface)
    ['{2370205F-CF27-4DF6-9B1F-5EBC27271D5A}']
    function GetMousePos: TPointF;
  end;

  IControl = interface(IFreeNotificationBehavior)
    ['{7318D022-D048-49DE-BF55-C5C36A2AD1AC}']
    function GetObject: TFmxObject;
    procedure SetFocus;
    procedure SetIsChildFocused(const Value: Boolean);
    function GetIsFocused: boolean;
    function GetIsChildFocused: Boolean;
    function GetCanFocus: Boolean;
    function GetCanParentFocus: Boolean;
    function GetEnabled: Boolean;
    function GetAbsoluteEnabled: Boolean;
    function GetPopupMenu: TCustomPopupMenu;
    function EnterChildren(AObject: IControl): Boolean;
    function ExitChildren(AObject: IControl): Boolean;
    procedure DoEnter;
    procedure DoExit;
    procedure DoActivate;
    procedure DoDeactivate;
    procedure DoMouseEnter;
    procedure DoMouseLeave;
    function ScreenToLocal(P: TPointF): TPointF;
    function LocalToScreen(P: TPointF): TPointF;
    function ObjectAtPoint(P: TPointF): IControl;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure MouseMove(Shift: TShiftState; X, Y: Single);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
    procedure KeyUp(var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
    procedure DialogKey(var Key: Word; Shift: TShiftState);
    function FindTarget(P: TPointF; const Data: TDragObject): IControl;
    procedure DragEnter(const Data: TDragObject; const Point: TPointF);
    procedure DragOver(const Data: TDragObject; const Point: TPointF; var Accept: Boolean);
    procedure DragDrop(const Data: TDragObject; const Point: TPointF);
    procedure DragLeave;
    procedure DragEnd;
    function CheckForAllowFocus: Boolean;
    function GetTabOrderValue: TTabOrder;
    procedure UpdateTabOrder(Value: TTabOrder);
    procedure Repaint;
    function GetDragMode: TDragMode;
    procedure SetDragMode(const ADragMode: TDragMode);
    procedure BeginAutoDrag;
    function GetParent: TFmxObject;
    function GetLocked: Boolean;
    function GetVisible: Boolean;
    procedure SetVisible(const Value: Boolean);
    function GetHitTest: Boolean;
    function GetCursor: TCursor;
    function GetInheritedCursor: TCursor;
    function GetDesignInteractive: Boolean;
    function GetAcceptsControls: Boolean;
    procedure SetAcceptsControls(const Value: Boolean);
    procedure BeginUpdate;
    procedure EndUpdate;
    { access }
    property AbsoluteEnabled: Boolean read GetAbsoluteEnabled;
    property Cursor: TCursor read GetCursor;
    property InheritedCursor: TCursor read GetInheritedCursor;
    property DragMode: TDragMode read GetDragMode write SetDragMode;
    property DesignInteractive: Boolean read GetDesignInteractive;
    property Enabled: Boolean read GetEnabled;
    property Parent: TFmxObject read GetParent;
    property Locked: Boolean read GetLocked;
    property HitTest: Boolean read GetHitTest;
    property PopupMenu: TCustomPopupMenu read GetPopupMenu;
    property Visible: Boolean read GetVisible write SetVisible;
    property AcceptsControls: Boolean read GetAcceptsControls write SetAcceptsControls;
    property IsFocused: boolean read GetIsFocused;
    property IsChildFocused: boolean read GetIsChildFocused write SetIsChildFocused;
  end;

  IRoot = interface
    ['{7F7BB7B0-5932-49dd-9D35-712B2BA5D8EF}']
    procedure AddObject(const AObject: TFmxObject);
    procedure InsertObject(Index: Integer; const AObject: TFmxObject);
    procedure RemoveObject(const AObject: TFmxObject); overload;
    procedure RemoveObject(Index: Integer); overload;
    procedure BeginInternalDrag(const Source: TObject; const ABitmap: TObject);
    function GetActiveControl: IControl;
    procedure SetActiveControl(const AControl: IControl);
    function GetCaptured: IControl;
    procedure SetCaptured(const Value: IControl);
    function GetFocused: IControl;
    procedure SetFocused(const Value: IControl);
    function NewFocusedControl(const Value: IControl): IControl;
    function GetHovered: IControl;
    procedure SetHovered(const Value: IControl);
    function GetObject: TFmxObject;
    function GetBiDiMode: TBiDiMode;
    { access }
    property Captured: IControl read GetCaptured write SetCaptured;
    property Focused: IControl read GetFocused write SetFocused;
    property Hovered: IControl read GetHovered write SetHovered;
    property BiDiMode: TBiDiMode read GetBiDiMode;
  end;

  IAlignRoot = interface
  ['{86DF30A6-0394-4a0e-8722-1F2CDB242CE8}']
    procedure Realign;
    procedure ChildrenAlignChanged;
  end;

  INativeControl = interface
  ['{3E6F1A17-BAE3-456C-8551-5F6EA92EEE32}']
    function GetHandle: TFmxHandle;
    procedure SetHandle(const Value: TFmxHandle);
    function GetHandleSupported: boolean;
    property HandleSupported: boolean read GetHandleSupported;
    property Handle: TFmxHandle read GetHandle write SetHandle;
  end;

  IPaintControl = interface
  ['{47959F99-CCA5-4ACF-BB8D-357F126E9C78}']
    procedure PaintRects(const UpdateRects: array of TRectF);
    procedure SetContextHandle(const AContextHandle: THandle);
    function GetContextHandle: THandle;
    property ContextHandle: THandle read GetContextHandle write SetContextHandle;
  end;

  TVirtualKeyboardType = (vktDefault, vktNumbersAndPunctuation, vktNumberPad, vktPhonePad,
                          vktAlphabet, vktURL, vktNamePhonePad, vktEmailAddress);

  TVirtualKeyBoardState = set of (vksAutoShow, vksVisible, vksError, vksTransient);

  TReturnKeyType = (rktDefault, rktDone, rktGo, rktNext, rktSearch, rktSend);

  IVirtualKeyboardControl = interface
    ['{41127080-97FC-4C30-A880-AB6CD351A6C4}']
    procedure SetKeyboardType(Value: TVirtualKeyboardType);
    function GetKeyboardType: TVirtualKeyboardType;
    property KeyboardType: TVirtualKeyboardType read GetKeyboardType write SetKeyboardType;
    //
    procedure SetReturnKeyType(Value: TReturnKeyType);
    function GetReturnKeyType: TReturnKeyType;
    property ReturnKeyType: TReturnKeyType read GetReturnKeyType write SetReturnKeyType;
  end;

  TAdjustType = (None, FixedSize, FixedWidth, FixedHeight);

  IAlignableObject = interface
    ['{420D3E98-4433-4cbe-9767-0B494DF08354}']
    function GetAlign: TAlignLayout;
    procedure SetAlign(const Value: TAlignLayout);
    function GetAnchors: TAnchors;
    procedure SetAnchors(const Value: TAnchors);
    function GetMargins: TBounds;
    procedure SetBounds(X, Y, AWidth, AHeight: Single);
    function GetPadding: TBounds;
    function GetWidth: single;
    function GetHeight: single;
    function GetLeft: single;
    function GetTop: single;
    function GetAllowAlign: Boolean;

    function GetAnchorRules: TPointF;
    function GetAnchorOrigin: TPointF;
    function GetOriginalParentSize: TPointF;
    function GetAnchorMove : Boolean;
    procedure SetAnchorMove(Value : Boolean);

    function GetAdjustType: TAdjustType;
    function GetAdjustSizeValue: TSizeF;
    { access }
    property Align: TAlignLayout read GetAlign write SetAlign;
    property AllowAlign: Boolean read GetAllowAlign;
    property Anchors: TAnchors read GetAnchors write SetAnchors;
    property Margins: TBounds read GetMargins;
    property Padding: TBounds read GetPadding;
    property Left: single read GetLeft;
    property Height: single read GetHeight;
    property Width: single read GetWidth;
    property Top: single read GetTop;

    property AnchorRules: TPointF read GetAnchorRules;
    property AnchorOrigin: TPointF read GetAnchorOrigin;
    property OriginalParentSize: TPointF read GetOriginalParentSize;
    property AnchorMove : Boolean read GetAnchorMove write SetAnchorMove;

    property AdjustType: TAdjustType read GetAdjustType;
    property AdjustSizeValue: TSizeF read GetAdjustSizeValue;
  end;

  IItemsContainer = interface
    ['{100B2F87-5DCB-4699-B751-B4439588E82A}']
    function GetItemsCount: Integer;
    function GetItem(const AIndex: Integer): TFmxObject;
    function GetObject: TFmxObject;
  end;

  TTangentPair = record
    I: Single;
    Ip1: Single;
  end;

  TSpline = class(TObject)
  private
    FTangentsX, FTangentsY: array of TTangentPair;
    FValuesX, FValuesY: array of Single;
  public
    constructor Create(const Polygon: TPolygon);
    destructor Destroy; override;
    procedure SplineXY(const t: Single; var X, Y: Single);
  end;

  TDragEnterEvent = procedure(Sender: TObject; const Data: TDragObject; const Point: TPointF) of object;
  TDragOverEvent = procedure(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Accept: Boolean) of object;
  TDragDropEvent = procedure(Sender: TObject; const Data: TDragObject; const Point: TPointF) of object;
  TCanFocusEvent = procedure(Sender: TObject; var ACanFocus: Boolean) of object;

{ TBounds }

  TBounds = class(TPersistent)
  private
    FRight: Single;
    FBottom: Single;
    FTop: Single;
    FLeft: Single;
    FOnChange: TNotifyEvent;
    FDefaultValue: TRectF;
    function GetRect: TRectF;
    procedure SetRect(const Value: TRectF);
    procedure SetBottom(const Value: Single);
    procedure SetLeft(const Value: Single);
    procedure SetRight(const Value: Single);
    procedure SetTop(const Value: Single);
    function IsBottomStored: Boolean;
    function IsLeftStored: Boolean;
    function IsRightStored: Boolean;
    function IsTopStored: Boolean;
    procedure ReadLeftInt(Reader: TReader);
    procedure ReadBottomInt(Reader: TReader);
    procedure ReadRightInt(Reader: TReader);
    procedure ReadTopInt(Reader: TReader);
    procedure ReadRectInt(Reader: TReader);
    procedure ReadRect(Reader: TReader);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoChange; virtual;
  public
    constructor Create(const ADefaultValue: TRectF); virtual;
    procedure Assign(Source: TPersistent); override;
    function Equals(Obj: TObject): Boolean; override;
    function PaddingRect(const R: TRectF): TRectF;
    function MarginRect(const R: TRectF): TRectF;
    function Width: Single;
    function Height: Single;
    property Rect: TRectF read GetRect write SetRect;
    property DefaultValue: TRectF read FDefaultValue write FDefaultValue;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    function Empty: Boolean;
    function MarginEmpty: Boolean;
    function ToString: string; override;
  published
    property Left: Single read FLeft write SetLeft stored IsLeftStored nodefault;
    property Top: Single read FTop write SetTop stored IsTopStored nodefault;
    property Right: Single read FRight write SetRight stored IsRightStored nodefault;
    property Bottom: Single read FBottom write SetBottom stored IsBottomStored nodefault;
  end;

{ TPosition }

  TPosition = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FY: Single;
    FX: Single;
    FDefaultValue: TPointF;
    FStoreAsInt: Boolean;
    procedure SetPoint(const Value: TPointF);
    procedure SetX(const Value: Single);
    procedure SetY(const Value: Single);
    function GetPoint: TPointF;
    function GetVector: TVector;
    procedure SetVector(const Value: TVector);
    function IsXStored: Boolean;
    function IsYStored: Boolean;
    procedure ReadXInt(Reader: TReader);
    procedure WriteXInt(Writer: TWriter);
    procedure ReadYInt(Reader: TReader);
    procedure WriteYInt(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadPoint(Reader: TReader);
    procedure WritePoint(Writer: TWriter);
    procedure DoChange; virtual;
  public
    constructor Create(const ADefaultValue: TPointF); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure SetPointNoChange(const P: TPointF);
    function Empty: Boolean;
    procedure Reflect(const Normal: TVector);
    property Point: TPointF read GetPoint write SetPoint;
    property Vector: TVector read GetVector write SetVector;
    property StoreAsInt: Boolean read FStoreAsInt write FStoreAsInt;
    property DefaultValue: TPointF read FDefaultValue write FDefaultValue;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property X: Single read FX write SetX stored IsXStored nodefault;
    property Y: Single read FY write SetY stored IsYStored nodefault;
  end;

  TCaretDisplayChanged = procedure (Sender: TCustomCaret; const VirtualKeyBoardState: TVirtualKeyBoardState) of object;

  TCaretClass = class of TCustomCaret;

  TCustomCaret = class (TPersistent)
  private
    [Weak]FOwner: TFMXObject;
    FIControl: IControl;
    FVisible: Boolean;
    FDisplayed: Boolean;
    FTemporarilyHidden: Boolean;
    FChanged: Boolean;
    FUpdateCount: Integer;
    FOnDisplayChanged: TCaretDisplayChanged;
    FColor: TAlphaColor;
    FDefaultColor: TAlphaColor;
    FPos: TPointF;
    FSize: TSizeF;
    FInterval: TFlasherInterval;
    FReadOnly: Boolean;
    procedure SetColor(const Value: TAlphaColor);
    procedure SetPos(const Value: TPointF);
    procedure SetSize(const Value: TSizeF);
    procedure SetTemporarilyHidden(const Value: boolean);
    procedure SetVisible(const Value: Boolean);
    procedure SetInterval(const Value: TFlasherInterval);
    procedure SetReadOnly(const Value: boolean);
    procedure StartTimer;
    function GetWidth: Word;
    procedure SetWidth(const Value: Word);
    function GetFlasher: IFlasher;
  protected
    function GetOwner: TPersistent; override;
    /// <summary>
    ///   hide the caret
    /// </summary>
    procedure Hide; virtual;
    /// <summary>
    ///   if possible (CanShow = True and Visible = True), the caret show.
    /// </summary>
    procedure Show; virtual;
    /// <summary>
    ///   This method is performed after changing the Displayed
    /// </summary>
    procedure DoDisplayChanged(const VirtualKeyBoardState: TVirtualKeyBoardState); virtual;
    procedure DoUpdateFlasher; virtual;
    procedure SetDefaultColor(const Value: TAlphaColor);
  public
    constructor Create(const AOwner: TFMXObject); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Pos: TPointF read FPos write SetPos;
    property Size: TSizeF read FSize write SetSize;
    property Color: TAlphaColor read FColor write SetColor default TAlphaColorRec.Null;
    property DefaultColor: TAlphaColor read FDefaultColor;
    property Interval: TFlasherInterval read FInterval write SetInterval default 0;
    property Owner: TFMXObject read FOwner;
    property Control: IControl read FIControl;
    procedure BeginUpdate;
    procedure EndUpdate;
    class function FlasherName: string; virtual; abstract;
    property UpdateCount: integer read FUpdateCount;
    /// <summary>
    ///   The update of the "Flasher", if UpdateCount = 0.
    /// </summary>
    procedure UpdateFlasher;
    /// <summary>
    ///   This property controls the visibility of a caret, for the control in which the input focus.
    /// </summary>
    property Visible: Boolean read FVisible write SetVisible;
    /// <summary>
    ///   The function returns true, if the control is visible, enabled,
    ///   has the input focus and it in an active form
    /// </summary>
    function CanShow: Boolean; virtual;
    /// <summary>
    ///   This property is set to True, after the successful execution of
    ///   method Show, and is set to False after method Hide
    /// </summary>
    property Displayed: Boolean read FDisplayed;
    /// <summary>
    ///   If this property is 'true', the blinking control is invisible
    ///   and does not take values of Visible, Displayed.
    ///   When you change the properties, methods DoShow, DoHide, DoDisplayChanged not met.
    /// </summary>
    property TemporarilyHidden: boolean read FTemporarilyHidden write SetTemporarilyHidden;
    /// <summary>
    ///   Blinking visual component is displayed.
    ///   Usually this line, having a thickness of one or two pixels.
    /// </summary>
    property Flasher: IFlasher read GetFlasher;
    property ReadOnly: boolean read FReadOnly write SetReadOnly;
    property Width: Word read GetWidth write SetWidth default 0;

    property OnDisplayChanged: TCaretDisplayChanged read FOnDisplayChanged write FOnDisplayChanged;
  end;

{ TTransform }

  TTransform = class(TPersistent)
  private
    FMatrix: TMatrix;
    FRotationAngle: Single;
    FPosition: TPosition;
    FScale: TPosition;
    FSkew: TPosition;
    FRotationCenter: TPosition;
    FOnChanged: TNotifyEvent;
    procedure SetRotationAngle(const Value: Single);
    procedure SetScale(const Value: TPosition);
    procedure SetPosition(const Value: TPosition);
  protected
    procedure MatrixChanged(Sender: TObject);
    property Skew: TPosition read FSkew write FSkew;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Matrix: TMatrix read FMatrix;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  published
    property Position: TPosition read FPosition write SetPosition;
    property Scale: TPosition read FScale write SetScale;
    property RotationAngle: Single read FRotationAngle write SetRotationAngle;
    property RotationCenter: TPosition read FRotationCenter write FRotationCenter;
  end;

  TMouseEvent = procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single) of object;
  TMouseMoveEvent = procedure(Sender: TObject; Shift: TShiftState; X, Y: Single) of object;
  TMouseWheelEvent = procedure(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean) of object;
  TKeyEvent = procedure(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState) of object;
  TProcessTickEvent = procedure(Sender: TObject; time, deltaTime: Single) of object;
  TVirtualKeyboardEvent = procedure(Sender: TObject; KeyboardVisible: Boolean; const Bounds : TRect) of object;

  TAnimationType = (atIn, atOut, atInOut);

  TInterpolationType = (itLinear, itQuadratic, itCubic, itQuartic,
    itQuintic, itSinusoidal, itExponential, itCircular,
    itElastic, itBack, itBounce);

  TFmxObjectSortCompare = reference to function (Left, Right: TFmxObject): Integer;

  TFmxObjectList = TList<TFmxObject>;

  TFmxChildrenList = class(TEnumerable<TFmxObject>)
  strict private
    [weak] FChildren: TFmxObjectList;
  protected
    function DoGetEnumerator: TEnumerator<TFmxObject>; override;
    function GetChildCount: Integer; virtual;
    function GetChild(AIndex: Integer): TFmxObject; virtual;
  public
    constructor Create(const AChildren: TFmxObjectList);
    destructor Destroy; override;
    property Count: Integer read GetChildCount;
    function IndexOf(const Obj: TFmxObject): Integer; virtual;
    property Items[Index: Integer]: TFmxObject read GetChild; default;
  end;

{ TFmxObject }


  TFmxObject = class(TComponent, IFreeNotification)
  strict private
    FChildren: TFmxObjectList;
    FChildrenList: TFmxChildrenList;
    FStyleObjectsDict: TDictionary<string, TFmxObject>;
  private
    FStored: Boolean;
    [Weak] FTagObject: TObject;
    FTagFloat: Single;
    FTagString: string;
    FNotifyList: TList<Pointer>;
    FIndex: Integer;
    FActionClient: boolean;
    FActionLink: TActionLink;
    FCallingFreeNotify: Boolean;
    FTouchManager: TTouchManager;
    FOnGesture: TGestureEvent;
    FReleased: Boolean;
    procedure SetStyleName(const Value: string);
    procedure SetStored(const Value: Boolean);
    function GetChildrenCount: Integer; inline;
    function GetIndex: Integer;
    function GetTouchManager: TTouchManager;
    procedure FixupTabList;
    procedure SetIndex(Idx: Integer);
    { Actions }
    function GetAction: TBasicAction;
    procedure SetAction(const Value: TBasicAction);
    procedure SetActionClient(const Value: boolean);
    procedure SetTouchManager(const Value: TTouchManager);
    class constructor Create;
    class destructor Destroy;
  protected
    [Weak] FRoot: IRoot;
    FStyleName: string;
    [Weak] FParent: TFmxObject;
    FTabList: IInterfaceList;
    function CreateChildrenList(const Children: TFmxObjectList): TFmxChildrenList; virtual;
    procedure ResetChildrenIndices;
    function GetBackIndex: Integer; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure IgnoreBindingName(Reader: TReader);
    { RTL }
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure SetParentComponent(Value: TComponent); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    { Actions }
    function GetActionLinkClass: TActionLinkClass; virtual;
    procedure DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean); virtual;
    procedure InitiateAction; virtual;
    procedure DoActionChange(Sender: TObject); virtual;
    procedure ActionChange(Sender: TBasicAction; CheckDefaults: Boolean); virtual;
    procedure DoActionClientChanged; virtual;
    property ActionLink: TActionLink read FActionLink;
    property Action: TBasicAction read GetAction write SetAction;
    procedure AddToTabList(const AObject: TFmxObject); virtual;
    procedure RemoveFromTabList(const AObject: TFmxObject); virtual;
    procedure DoInventory(Dict: TDictionary<string, TFmxObject>; const Prefix: string);
  public
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;
  protected
    procedure AddToResourcePool; virtual;
    procedure RemoveFromResourcePool; virtual;
    { parent }
    procedure SetParent(const Value: TFmxObject); virtual;
    procedure DoRootChanging(const NewRoot: IRoot); virtual;
    procedure DoRootChanged; virtual;
    procedure ChangeParent; virtual;
    procedure ChangeOrder; virtual;
    procedure ChangeChildren; virtual;
    { children }
    procedure DoAddObject(const AObject: TFmxObject); virtual;
    procedure DoInsertObject(Index: Integer; const AObject: TFmxObject); virtual;
    procedure DoRemoveObject(const AObject: TFmxObject); virtual;
    procedure DoDeleteChildren; virtual;
    function SearchInto: Boolean; virtual;
    { ani }
    procedure DoAniFinished(Sender: TObject);
    procedure DoAniProcess(Sender: TObject);
    { IFreeNotification }
    procedure FreeNotification(AObject: TObject); virtual;
    { design }
    function SupportsPlatformService(const AServiceGUID: TGUID; out AService: IInterface): Boolean; virtual;
    { Data }
    function GetData: TValue; virtual;
    procedure SetData(const Value: TValue); virtual;
    procedure IgnoreIntegerValue(Reader: TReader);
    procedure IgnoreFloatValue(Reader: TReader);
    procedure IgnoreBooleanValue(Reader: TReader);
    procedure IgnoreIdentValue(Reader: TReader);
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Release; virtual;
    function Released: Boolean;
    { check for support interface }
    function IsIControl: Boolean; virtual;
    function AsIControl: IControl; virtual;
    procedure SetRoot(ARoot: IRoot);
    { design }
    procedure SetDesign(Value: Boolean; SetChildren: Boolean = True);
    function ItemClass: string; virtual;
    { clone }
    function Clone(const AOwner: TComponent): TFmxObject;
    { childs }
    procedure AddObject(const AObject: TFmxObject);
    procedure InsertObject(Index: Integer; const AObject: TFmxObject);
    procedure RemoveObject(const AObject: TFmxObject); overload;
    procedure RemoveObject(Index: Integer); overload;
    function ContainsObject(AObject: TFmxObject): Boolean; virtual;
    procedure Exchange(const AObject1, AObject2: TFmxObject); virtual;
    procedure DeleteChildren;
    function IsChild(AObject: TFmxObject): Boolean; virtual;
    procedure BringToFront; virtual;
    procedure SendToBack; virtual;
    procedure AddObjectsToList(const AList: TFmxObjectList);
    procedure Sort(Compare: TFmxObjectSortCompare); virtual;
    { notify }
    procedure AddFreeNotify(const AObject: IFreeNotification);
    procedure RemoveFreeNotify(const AObject: IFreeNotification);
    { tab }
    procedure GetTabOrderList(const List: TInterfaceList; AChildren: Boolean); virtual;
    { resource }
    function FindStyleResource(const AStyleLookup: string; const Clone: Boolean = False): TFmxObject; virtual;
    { animations }
    procedure StartAnimation(const AName: string); virtual;
    procedure StopAnimation(const AName: string); virtual;
    procedure StartTriggerAnimation(const AInstance: TFmxObject; const ATrigger: string); virtual;
    procedure StartTriggerAnimationWait(const AInstance: TFmxObject; const ATrigger: string); virtual;
    procedure StopTriggerAnimation(const AInstance: TFmxObject; const ATrigger: string); virtual;
    procedure ApplyTriggerEffect(const AInstance: TFmxObject; const ATrigger: string); virtual;
    { animation property }
    procedure AnimateFloat(const APropertyName: string; const NewValue: Single; Duration: Single = 0.2;
      AType: TAnimationType = TAnimationType.atIn;
      AInterpolation: TInterpolationType = TInterpolationType.itLinear);
    procedure AnimateFloatDelay(const APropertyName: string; const NewValue: Single; Duration: Single = 0.2;
      Delay: Single = 0.0; AType: TAnimationType = TAnimationType.atIn;
      AInterpolation: TInterpolationType = TInterpolationType.itLinear);
    procedure AnimateFloatWait(const APropertyName: string; const NewValue: Single; Duration: Single = 0.2;
      AType: TAnimationType = TAnimationType.atIn;
      AInterpolation: TInterpolationType = TInterpolationType.itLinear);
    procedure AnimateInt(const APropertyName: string; const NewValue: Integer; Duration: Single = 0.2;
      AType: TAnimationType = TAnimationType.atIn;
      AInterpolation: TInterpolationType = TInterpolationType.itLinear);
    procedure AnimateIntWait(const APropertyName: string; const NewValue: Integer; Duration: Single = 0.2;
      AType: TAnimationType = TAnimationType.atIn;
      AInterpolation: TInterpolationType = TInterpolationType.itLinear);
    procedure AnimateColor(const APropertyName: string; NewValue: TAlphaColor; Duration: Single = 0.2;
      AType: TAnimationType = TAnimationType.atIn;
      AInterpolation: TInterpolationType = TInterpolationType.itLinear);
    procedure StopPropertyAnimation(const APropertyName: string);
    procedure CMGesture(var EventInfo: TGestureEventInfo); virtual;
    { }
    property Root: IRoot read FRoot;
    property Stored: Boolean read FStored write SetStored;
    { }
    property TagObject: TObject read FTagObject write FTagObject;
    property TagFloat: Single read FTagFloat write FTagFloat;
    property TagString: string read FTagString write FTagString;
    { children }
    property ChildrenCount: Integer read GetChildrenCount;
    property Children: TFmxChildrenList read FChildrenList;
    property Data: TValue read GetData write SetData;
    property Parent: TFmxObject read FParent write SetParent;
    property Index: Integer read GetIndex write SetIndex;
    property ActionClient: boolean read FActionClient;
    property Touch: TTouchManager read GetTouchManager write SetTouchManager;
    property OnGesture: TGestureEvent read FOnGesture write FOnGesture;
  published
    property StyleName: string read FStyleName write SetStyleName;
  end;

{ TCustomPopupMenu }

  TCustomPopupMenu = class(TFmxObject)
  private
    FPopupComponent: TComponent;
    FOnPopup: TNotifyEvent;
  protected
    procedure DoPopup; virtual;
    property OnPopup: TNotifyEvent read FOnPopup write FOnPopup;
  public
    procedure Popup(X, Y: Single); virtual; abstract;
    property PopupComponent: TComponent read FPopupComponent write FPopupComponent;
  end;

  TStandardGesture = (
    sgLeft            = sgiLeft,
    sgRight           = sgiRight,
    sgUp              = sgiUp,
    sgDown            = sgiDown,
    sgUpLeft          = sgiUpLeft,
    sgUpRight         = sgiUpRight,
    sgDownLeft        = sgiDownLeft,
    sgDownRight       = sgiDownRight,
    sgLeftUp          = sgiLeftUp,
    sgLeftDown        = sgiLeftDown,
    sgRightUp         = sgiRightUp,
    sgRightDown       = sgiRightDown,
    sgUpDown          = sgiUpDown,
    sgDownUp          = sgiDownUp,
    sgLeftRight       = sgiLeftRight,
    sgRightLeft       = sgiRightLeft,
    sgUpLeftLong      = sgiUpLeftLong,
    sgUpRightLong     = sgiUpRightLong,
    sgDownLeftLong    = sgiDownLeftLong,
    sgDownRightLong   = sgiDownRightLong,
    sgScratchout      = sgiScratchout,
    sgTriangle        = sgiTriangle,
    sgSquare          = sgiSquare,
    sgCheck           = sgiCheck,
    sgCurlicue        = sgiCurlicue,
    sgDoubleCurlicue  = sgiDoubleCurlicue,
    sgCircle          = sgiCircle,
    sgDoubleCircle    = sgiDoubleCircle,
    sgSemiCircleLeft  = sgiSemiCircleLeft,
    sgSemiCircleRight = sgiSemiCircleRight,
    sgChevronUp       = sgiChevronUp,
    sgChevronDown     = sgiChevronDown,
    sgChevronLeft     = sgiChevronLeft,
    sgChevronRight    = sgiChevronRight
);

  TStandardGestures = set of TStandardGesture;

  TInteractiveGesture = (igZoom, igPan, igRotate, igTwoFingerTap, igPressAndtap, igLongTap, igDoubleTap);
  TInteractiveGestures = set of TInteractiveGesture;

  TCustomGestureManager = class;
  TCustomGestureCollection = class;
  TCustomGestureCollectionItem = class;

  TGestureType = (gtStandard, gtRecorded, gtRegistered, gtNone);
  TGestureTypes = set of TGestureType;

  TGestureOption = (goUniDirectional, goSkew, goEndpoint, goRotate);
  TGestureOptions = set of TGestureOption;

  TGestureArray = array of TCustomGestureCollectionItem;
  TGesturePointArray = array of TPointF;

  TCustomGestureCollectionItem = class(TCollectionItem)
  strict protected
    function GetAction: TCustomAction; virtual; abstract;
    function GetDeviation: Integer; virtual; abstract;
    function GetErrorMargin: Integer; virtual; abstract;
    function GetGestureID: TGestureID; virtual; abstract;
    function GetGestureType: TGestureType; virtual; abstract;
    function GetName: string; virtual; abstract;
    function GetOptions: TGestureOptions; virtual; abstract;
    function GetPoints: TGesturePointArray; virtual; abstract;
    procedure SetAction(const Value: TCustomAction); virtual; abstract;
    procedure SetDeviation(const Value: Integer); virtual; abstract;
    procedure SetErrorMargin(const Value: Integer); virtual; abstract;
    procedure SetGestureID(const Value: TGestureID); virtual; abstract;
    procedure SetName(const Value: string); virtual; abstract;
    procedure SetOptions(const Value: TGestureOptions); virtual; abstract;
    procedure SetPoints(const Value: TGesturePointArray); virtual; abstract;
  public
    property Deviation: Integer read GetDeviation write SetDeviation default 20;
    property ErrorMargin: Integer read GetErrorMargin write SetErrorMargin default 20;
    property GestureID: TGestureID read GetGestureID write SetGestureID;
    property GestureType: TGestureType read GetGestureType;
    property Name: string read GetName write SetName;
    property Points: TGesturePointArray read GetPoints write SetPoints;
    property Action: TCustomAction read GetAction write SetAction;
    property Options: TGestureOptions read GetOptions write SetOptions default [TGestureOption.goUniDirectional, TGestureOption.goRotate];
  end;

   TCustomGestureCollection = class(TCollection)
  protected
    function GetGestureManager: TCustomGestureManager; virtual; abstract;
    function GetItem(Index: Integer): TCustomGestureCollectionItem;
    procedure SetItem(Index: Integer; const Value: TCustomGestureCollectionItem);
  public
    function AddGesture: TCustomGestureCollectionItem; virtual; abstract;
    function FindGesture(AGestureID: TGestureID): TCustomGestureCollectionItem; overload; virtual; abstract;
    function FindGesture(const AName: string): TCustomGestureCollectionItem; overload; virtual; abstract;
    function GetUniqueGestureID: TGestureID; virtual; abstract;
    procedure RemoveGesture(AGestureID: TGestureID); virtual; abstract;
    property GestureManager: TCustomGestureManager read GetGestureManager;
    property Items[Index: Integer]: TCustomGestureCollectionItem read GetItem write SetItem; default;
  end;

  TCustomGestureEngine = class
  public type
    TGestureEngineFlag = (efMouseEvents, efTouchEvents);
    TGestureEngineFlags = set of TGestureEngineFlag;
  protected
    function GetActive: Boolean; virtual; abstract;
    function GetFlags: TGestureEngineFlags; virtual; abstract;
    procedure SetActive(const Value: Boolean); virtual; abstract;
  public
    constructor Create(const AControl: TFmxObject); virtual; abstract;
    class function Supported: Boolean; virtual;
    procedure BroadcastGesture(const AControl: TFmxObject; EventInfo: TGestureEventInfo); virtual; abstract;
    property Active: Boolean read GetActive write SetActive;
    property Flags: TGestureEngineFlags read GetFlags;
  end;

  TCustomGestureManager = class(TComponent)
  protected
    function GetGestureList(AControl: TFmxObject): TGestureArray; virtual; abstract;
    function GetStandardGestures(AControl: TFmxObject): TStandardGestures; virtual; abstract;
    procedure SetStandardGestures(AControl: TFmxObject; AStandardGestures: TStandardGestures); virtual; abstract;
  public
    function AddRecordedGesture(const Item: TCustomGestureCollectionItem): TGestureID; overload; virtual; abstract;
    function FindCustomGesture(AGestureID: TGestureID): TCustomGestureCollectionItem; overload; virtual; abstract;
    function FindCustomGesture(const AName: string): TCustomGestureCollectionItem; overload; virtual; abstract;
    function FindGesture(const AControl: TFmxObject; AGestureID: TGestureID): TCustomGestureCollectionItem; overload; virtual; abstract;
    function FindGesture(const AControl: TFmxObject; const AName: string): TCustomGestureCollectionItem; overload; virtual; abstract;
    procedure RemoveActionNotification(Action: TCustomAction; Item: TCustomGestureCollectionItem); virtual;
    procedure RegisterControl(const AControl: TFmxObject); virtual; abstract;
    procedure RemoveRecordedGesture(AGestureID: TGestureID); overload; virtual; abstract;
    procedure RemoveRecordedGesture(const AGesture: TCustomGestureCollectionItem); overload; virtual; abstract;
    function SelectGesture(const AControl: TFmxObject; AGestureID: TGestureID): Boolean; overload; virtual; abstract;
    function SelectGesture(const AControl: TFmxObject; const AName: string): Boolean; overload; virtual; abstract;
    procedure UnregisterControl(const AControl: TFmxObject); virtual; abstract;
    procedure UnselectGesture(const AControl: TFmxObject; AGestureID: TGestureID); virtual; abstract;
    property GestureList[AControl: TFmxObject]: TGestureArray read GetGestureList;
    property StandardGestures[AControl: TFmxObject]: TStandardGestures read GetStandardGestures write SetStandardGestures;
  end;

  TCustomTouchManager = class(TPersistent)
  private
  type
    TObjectWrapper = class(TObject)
      [Weak] FObject : TFmxObject;
      constructor Create(const AObject: TFmxObject);
    end;
  private
    [Weak] FControl: TFmxObject;
    FGestureEngine: TCustomGestureEngine;
    FGestureManager: TCustomGestureManager;
    FInteractiveGestures: TInteractiveGestures;
    FDefaultInteractiveGestures: TInteractiveGestures;
    FStandardGestures: TStandardGestures;
    function GetStandardGestures: TStandardGestures;
    function IsInteractiveGesturesStored: Boolean;
    procedure SetInteractiveGestures(const Value: TInteractiveGestures);
    procedure SetGestureEngine(const Value: TCustomGestureEngine);
    procedure SetGestureManager(const Value: TCustomGestureManager);
    procedure SetStandardGestures(const Value: TStandardGestures);
    function GetGestureList: TGestureArray;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function IsDefault: Boolean;
  public
    constructor Create(AControl: TFmxObject);
    destructor Destroy; override;
    procedure ChangeNotification(const AControl: TFmxObject);
    function FindGesture(AGestureID: TGestureID): TCustomGestureCollectionItem; overload;
    function FindGesture(const AName: string): TCustomGestureCollectionItem; overload;
    procedure RemoveChangeNotification(const AControl: TFmxObject);
    function SelectGesture(AGestureID: TGestureID): Boolean; overload;
    function SelectGesture(const AName: string): Boolean; overload;
    procedure UnselectGesture(AGestureID: TGestureID); inline;
    property GestureEngine: TCustomGestureEngine read FGestureEngine write SetGestureEngine;
    property GestureList: TGestureArray read GetGestureList;
    property GestureManager: TCustomGestureManager read FGestureManager write SetGestureManager;
    property InteractiveGestures: TInteractiveGestures
      read FInteractiveGestures write SetInteractiveGestures stored IsInteractiveGesturesStored;
    property DefaultInteractiveGestures: TInteractiveGestures
      read FDefaultInteractiveGestures write FDefaultInteractiveGestures;
    property StandardGestures: TStandardGestures read GetStandardGestures write SetStandardGestures;
  end;

  TTouchManager = class(TCustomTouchManager)
  published
    property GestureManager;
    property InteractiveGestures;
  end;

{ TAnimation }

  TTrigger = type string;

  TAnimation = class(TFmxObject)
  private
  type
    TTriggerRec = record
      Name: string;
      Prop: TRttiProperty;
      Value: Boolean;
    end;
  private
    FTickCount : Integer;
    FDuration: Single;
    FDelay, FDelayTime: Single;
    FTime: Single;
    FInverse: Boolean;
    FTrigger, FTriggerInverse: TTrigger;
    FLoop: Boolean;
    FPause: Boolean;
    FRunning: Boolean;
    FOnFinish: TNotifyEvent;
    FOnProcess: TNotifyEvent;
    FInterpolation: TInterpolationType;
    FAnimationType: TAnimationType;
    FEnabled: Boolean;
    FAutoReverse: Boolean;
    FTriggerList: TList<TTriggerRec>;
    FInverseTriggerList: TList<TTriggerRec>;
    FTargetClass: TClass;
    procedure SetEnabled(const Value: Boolean);
    procedure SetTrigger(const Value: TTrigger);
    procedure SetTriggerInverse(const Value: TTrigger);
    procedure ParseTriggers(const AInstance: TFmxObject; Normal, Inverse: Boolean);
  protected
    function NormalizedTime: Single;
    procedure FirstFrame; virtual;
    procedure ProcessAnimation; virtual; abstract;
    procedure DoProcess; virtual;
    procedure DoFinish; virtual;
    procedure Loaded; override;
    procedure ChangeParent; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; virtual;
    procedure Stop; virtual;
    procedure StopAtCurrent; virtual;
    procedure StartTrigger(const AInstance: TFmxObject; const ATrigger: string);
    procedure StopTrigger(const AInstance: TFmxObject; const ATrigger: string);
    procedure ProcessTick(time, deltaTime: Single);
    property Running: Boolean read FRunning;
    property Pause: Boolean read FPause write FPause;
    property AnimationType: TAnimationType read FAnimationType write FAnimationType default TAnimationType.atIn;
    property AutoReverse: Boolean read FAutoReverse write FAutoReverse default False;
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property Delay: Single read FDelay write FDelay;
    property Duration: Single read FDuration write FDuration nodefault;
    property Interpolation: TInterpolationType read FInterpolation write FInterpolation default TInterpolationType.itLinear;
    property Inverse: Boolean read FInverse write FInverse default False;
    property Loop: Boolean read FLoop write FLoop default False;
    property Trigger: TTrigger read FTrigger write SetTrigger;
    property TriggerInverse: TTrigger read FTriggerInverse write SetTriggerInverse;
    property CurrentTime: Single read FTime;
    property OnProcess: TNotifyEvent read FOnProcess write FOnProcess;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
  end;

  ISizeGrip = interface
    ['{181729B7-53B2-45ea-97C7-91E1F3CBAABE}']
  end;

{ TLang }

  TLang = class(TFmxObject)
  private
    FLang: string;
    FResources: TStrings;
    FOriginal: TStrings;
    FAutoSelect: Boolean;
    FFileName: string;
    FStoreInForm: Boolean;
    procedure SetLang(const Value: string);
    function GetLangStr(const Index: string): TStrings;
  protected
    { vcl }
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadResources(Stream: TStream);
    procedure WriteResources(Stream: TStream);
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddLang(const AName: string);
    procedure LoadFromFile(const AFileName: string);
    procedure SaveToFile(const AFileName: string);
    property Original: TStrings read FOriginal;
    property Resources: TStrings read FResources;
    property LangStr[const Index: string]: TStrings read GetLangStr;
  published
    property AutoSelect: Boolean read FAutoSelect write FAutoSelect default True;
    property FileName: string read FFileName write FFileName;
    property StoreInForm: Boolean read FStoreInForm write FStoreInForm default True;
    property Lang: string read FLang write SetLang;
  end;

{ TTimer }

  TTimerProc = procedure of object;

  IFMXTimerService = interface(IInterface)
    ['{856E938B-FF7B-4E13-85D4-3414A6A9FF2F}']
    function CreateTimer(Interval: Integer; TimerFunc: TTimerProc): TFmxHandle;
    function DestroyTimer(Timer: TFmxHandle): Boolean;
    function GetTick: Extended;
  end;

  TTimer = class(TFmxObject)
  private
    FInterval: Cardinal;
    FTimerHandle: TFmxHandle;
    FOnTimer: TNotifyEvent;
    FEnabled: Boolean;
    FPlatformTimer: IFMXTimerService;
    procedure Timer;
  protected
    procedure SetEnabled(Value: Boolean); virtual;
    procedure SetInterval(Value: Cardinal); virtual;
    procedure SetOnTimer(Value: TNotifyEvent); virtual;
    procedure DoOnTimer; virtual;
    procedure UpdateTimer; virtual;
    procedure KillTimer; virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;
  end;

{ TLineInfo }

  PLineMetric = ^TLineMetric;
  TLineMetric = record
    Index: integer;
    Len: integer;
  end;

  TLineMetricInfo = class
  protected
    FLineMetrics: array of TLineMetric;
    function GetCount: integer; virtual;
    function GetMetrics(Index: Integer): PLineMetric; virtual;
    procedure SetCount(const Value: Integer); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;
    property Count: integer read GetCount write SetCount;
    property Metrics[ind: integer]: PLineMetric read GetMetrics;
  end;

  EInvalidCallingConditions = class(Exception);
  EInvalidRenderingConditions = class(Exception);
  ETextureSizeTooSmall = class(Exception);
  ECannotAcquireBitmapAccess = class(Exception);
  ECannotFindSuitablePixelFormat = class(Exception);
  ECannotCreateDirect3D = class(Exception);
  ECannotCreateD3DDevice = class(Exception);
  ECannotAcquireDXGIFactory = class(Exception);
  ECannotAssociateWindowHandle = class(Exception);
  ECannotRetrieveDisplayMode = class(Exception);
  ECannotRetrieveBufferDesc = class(Exception);
  ECannotCreateSamplerState = class(Exception);
  ECannotRetrieveSurface = class(Exception);
  ECannotCreateTexture = class(Exception);
  ECannotUploadTexture = class(Exception);
  ECannotActivateTexture = class(Exception);
  ECannotAcquireTextureAccess = class(Exception);
  ECannotCopyTextureResource = class(Exception);
  ECannotCreateRenderTargetView = class(Exception);
  ECannotActivateFrameBuffers = class(Exception);
  ECannotCreateRenderBuffers = class(Exception);
  ECannotRetrieveRenderBuffers = class(Exception);
  ECannotActivateRenderBuffers = class(Exception);
  ECannotBeginRenderingScene = class(Exception);
  ECannotSyncDeviceBuffers = class(Exception);
  ECannotUploadDeviceBuffers = class(Exception);
  ECannotCreateDepthStencil = class(Exception);
  ECannotRetrieveDepthStencil = class(Exception);
  ECannotActivateDepthStencil = class(Exception);
  ECannotCreateSwapChain = class(Exception);
  ECannotResizeSwapChain = class(Exception);
  ECannotActivateSwapChain = class(Exception);
  ECannotCreateVertexShader = class(Exception);
  ECannotCreatePixelShader = class(Exception);
  ECannotCreateVertexLayout = class(Exception);
  ECannotCreateVertexDeclaration = class(Exception);
  ECannotCreateVertexBuffer = class(Exception);
  ECannotCreateIndexBuffer = class(Exception);
  EShaderCompilationError = class(Exception);
  EProgramCompilationError = class(Exception);
  ECannotFindShaderVariable = class(Exception);
  ECannotActivateShaderProgram = class(Exception);
  ECannotCreateOpenGLContext = class(Exception);
  ECannotUpdateOpenGLContext = class(Exception);
  ECannotDrawMeshObject = class(Exception);
  EFeatureNotSupported = class(Exception);
  EErrorCompressingStream = class(Exception);
  EErrorDecompressingStream = class(Exception);
  EErrorUnpackingShaderCode = class(Exception);

var
  AniFrameRate: Integer = 60;
  AniThread: TTimer;
  USFormatSettings: TFormatSettings; // used for correct string to float convertion

{ Resources }

procedure AddResource(const AObject: TFmxObject);
procedure RemoveResource(const AObject: TFmxObject);
function FindStyleResource(const AStyleLookup: string; const Clone: Boolean = False): TFmxObject;

{ Lang }

procedure LoadLangFromFile(const AFileName: string);
procedure LoadLangFromStrings(const AStr: TStrings);
procedure ResetLang;

{ Align }

procedure ArrangeControl(const Control: IAlignableObject; AAlign: TAlignLayout;
  const AParentWidth, AParentHeight: Single;
  const ALastWidth, ALastHeight: Single;
  var R: TRectF);

procedure AlignObjects(const AParent: TFmxObject; APadding: TBounds; AParentWidth, AParentHeight: single;
  var ALastWidth, ALastHeight: single; var ADisableAlign: Boolean);

procedure RecalcAnchorRules(const Parent : TFmxObject;
                  Anchors : TAnchors;
                  const BoundsRect : TRectF;
                  const Padding : TBounds;
                  var AOriginalParentSize:TPointF;
                  var AAnchorOrigin:TPointF;
                  var AAnchorRules:TPointF);
procedure RecalcControlOriginalParentSize(const Parent: TFmxObject;
                  ComponentState : TComponentState;
                  var AOriginalParentSize : TPointF);

type
  TCustomTranslateProc = function(const AText: string): string;

var
  CustomTranslateProc: TCustomTranslateProc;

{ This function use to collect string which can be translated. Just place this function at Application start. }

procedure CollectLangStart;
procedure CollectLangFinish;
{ This function return Strings with collected text }
function CollectLangStrings: TStrings;

function Translate(const AText: string): string;
function TranslateText(const AText: string): string;

{ Strings }

function PointToString(R: TPointF): string;
function StringToPoint(S: string): TPointF;

function RectToString(R: TRectF): string;
function StringToRect(S: string): TRectF;

{ Four 2D corners describing arbitrary rectangle. }

function CornersF(Left, Top, Width, Height: Single): TCornersF; overload;
function CornersF(const Pt1, Pt2, Pt3, Pt4: TPointF): TCornersF; overload;
function CornersF(const Rect: TRect): TCornersF; overload;
function CornersF(const Rect: TRectF): TCornersF; overload;

{ Animation }

function InterpolateSingle(const Start, Stop, t: Single): Single;
function InterpolateRotation(Start, Stop, t: Single): Single;
function InterpolateColor(const Start, Stop: TAlphaColor; t: Single): TAlphaColor;

function InterpolateLinear(t, B, C, D: Single): Single;
function InterpolateSine(t, B, C, D: Single; AType: TAnimationType): Single;
function InterpolateQuint(t, B, C, D: Single; AType: TAnimationType): Single;
function InterpolateQuart(t, B, C, D: Single; AType: TAnimationType): Single;
function InterpolateQuad(t, B, C, D: Single; AType: TAnimationType): Single;
function InterpolateExpo(t, B, C, D: Single; AType: TAnimationType): Single;
function InterpolateElastic(t, B, C, D, A, P: Single; AType: TAnimationType): Single;
function InterpolateCubic(t, B, C, D: Single; AType: TAnimationType): Single;
function InterpolateCirc(t, B, C, D: Single; AType: TAnimationType): Single;
function InterpolateBounce(t, B, C, D: Single; AType: TAnimationType): Single;
function InterpolateBack(t, B, C, D, S: Single; AType: TAnimationType): Single;

{ Helper functions }

function IsHandleValid(Hnd: TFmxHandle): Boolean;
procedure ReverseBytes(P: Pointer; Count: Integer);
procedure FillLongword(Src: Pointer; Count: Integer; Value: longword);
procedure FillAlpha(Src: Pointer; Count: Integer; Alpha: Byte);
procedure FillLongwordRect(Src: Pointer; W, H, X1, Y1, X2, Y2: Integer; Value: longword);
function GetToken(var S: string; const Separators: string; const Stop: string = ''): string;
function WideGetToken(var Pos: Integer; const S: string; const Separators: string; const Stop: string = ''): string;

procedure RegisterFmxClasses(const RegClasses: array of TPersistentClass); overload;
procedure RegisterFmxClasses(const RegClasses: array of TPersistentClass;
  const GroupClasses: array of TPersistentClass); overload;

var
  AnchorAlign: array [TAlignLayout] of TAnchors = (
    { alNone }
    [TAnchorKind.akLeft,
     TAnchorKind.akTop],

    { alTop }
    [TAnchorKind.akLeft,
     TAnchorKind.akTop,
     TAnchorKind.akRight],

    { alLeft }
    [TAnchorKind.akLeft,
     TAnchorKind.akTop,
     TAnchorKind.akBottom],

    { alRight }
    [TAnchorKind.akRight,
     TAnchorKind.akTop,
     TAnchorKind.akBottom],

    { alBottom }
    [TAnchorKind.akLeft,
     TAnchorKind.akRight,
     TAnchorKind.akBottom],

    { alMostTop }
    [TAnchorKind.akLeft,
     TAnchorKind.akTop,
     TAnchorKind.akRight],

    { alMostBottom }
    [TAnchorKind.akLeft,
     TAnchorKind.akRight,
     TAnchorKind.akBottom],

    { alMostLeft }
    [TAnchorKind.akLeft,
     TAnchorKind.akTop,
     TAnchorKind.akBottom],

    { alMostRight }
    [TAnchorKind.akRight,
     TAnchorKind.akTop,
     TAnchorKind.akBottom],

    { alClient }
    [TAnchorKind.akLeft,
     TAnchorKind.akTop,
     TAnchorKind.akRight,
     TAnchorKind.akBottom],

    { alContents }
    [TAnchorKind.akLeft,
     TAnchorKind.akTop,
     TAnchorKind.akRight,
     TAnchorKind.akBottom],

    { alCenter }
    [],

    { alVertCenter }
    [TAnchorKind.akLeft,
     TAnchorKind.akRight],

    { alHorzCenter }
    [TAnchorKind.akTop,
     TAnchorKind.akBottom],

    { vaHorizintal }
    [TAnchorKind.akLeft,
     TAnchorKind.akRight],

    { alVertical }
    [TAnchorKind.akTop,
     TAnchorKind.akBottom],

    { alScale }
    [TAnchorKind.akLeft,
     TAnchorKind.akTop,
     TAnchorKind.akRight,
     TAnchorKind.akBottom],

    { alFit }
    [TAnchorKind.akLeft,
     TAnchorKind.akTop,
     TAnchorKind.akRight,
     TAnchorKind.akBottom],

    { alFitLeft }
    [TAnchorKind.akLeft,
     TAnchorKind.akTop,
     TAnchorKind.akRight,
     TAnchorKind.akBottom],

    { alFitRight }
    [TAnchorKind.akLeft,
     TAnchorKind.akTop,
     TAnchorKind.akRight,
     TAnchorKind.akBottom]
  );

{ Debugging }
type
  Log = class abstract
  strict private
    class var FLogger: IInterface;
    class constructor Create;
  public
  type
    ToStringFunc = reference to function(O: TObject) : String;

    class procedure d(const Msg: String); overload;
    class procedure d(const Fmt: String; const Args: array of const); overload;
    class procedure d(const Tag: String; const Instance : TObject; const Msg : String); overload;
    class procedure d(const Tag: String; const Instance : TObject; const Method : String; const Msg : String); overload;
    class procedure TimeStamp(const Msg: String); overload;
    class function ObjToString(const Instance : TObject) : String;
//    class function ArrayToString(AArray : TEnumerable<TControl>) : String; overload;
    class function ArrayToString(const AArray : TEnumerable<TFmxObject>) : String; overload;
    class function ArrayToString(const AArray : TEnumerable<TFmxObject>; MakeStr : ToStringFunc) : String; overload;
    class procedure DumpFmxObject(const O: TFmxObject; Nest: Integer = 0);
  end;

/// <summary>
///   This function removes the char '&' from a string
///  <para> For Example <c>'S&ome text && number' -> 'Some text & number'</c> </para>
/// </summary>
function DelAmp(const Text: string): string;


type
  TEnumerableFilter<F; T: TFmxObject> = class(TEnumerable<T>)
  private
    FBaseEnum: TEnumerable<F>;
    FSelfDestruct: Boolean;
  protected
    function DoGetEnumerator: TEnumerator<T>; override;
  public
    constructor Create(const FullEnum: TEnumerable<F>; SelfDestruct: Boolean = False);
    class function Filter(const Src: TEnumerable<F>): TEnumerableFilter<F,T>;

  type
    TFilterEnumerator = class(TEnumerator<T>)
    private
      FCleanup: TEnumerableFilter<F,T>;
      FRawEnumerator: TEnumerator<F>;
      FCurrent: T;
      function GetCurrent: T;
    protected
      function DoGetCurrent: T; override;
      function DoMoveNext: Boolean; override;
    public
      constructor Create(const Enumerable: TEnumerable<F>; const Cleanup: TEnumerableFilter<F,T>);
      destructor Destroy; override;
      property Current: T read GetCurrent;
      function MoveNext: Boolean;
    end;
  end;

  TIdleMessage = class(FMX.Messages.TMessage)
  end;

/// <summary>
///  Registering the class of flashing control.
///  If at the time perform this function has been instantiated flashing control, it will be destroyed.
///  If FlasherClass = nil, then with passed to function Flasher will be raised exception.
///  If FlasherClass different from nil, then this class must support the interface IFlasher.
/// </summary>
procedure RegisterFlasherClass(const FlasherClass: TFmxObjectClass; const CaretClass: TCaretClass);
/// <summary>
///  Registered class flashing control
/// </summary>
function FlasherClass(const CaretClass: TCaretClass): TFmxObjectClass;
/// <summary>
///  Flashing (blinking) control. The first time, you create a single instance.
/// </summary>
function Flasher(const CaretClass: TCaretClass): TFmxObject;
/// <summary>
///  This function allows you to specify are creating a single instance of Flasher.
/// </summary>
function AssignedFlasher(const CaretClass: TCaretClass): boolean;

type
  TShowVirtualKeyboard = procedure (const Displayed: boolean;
                                    const Caret: TCustomCaret;
                                    var VirtualKeyBoardState: TVirtualKeyBoardState);

procedure RegisterShowVKProc(const ShowVirtualKeyboard: TShowVirtualKeyboard);

var
  SharedContext: TRttiContext;
  ClonePropertiesCache: TDictionary<string, TList<TRttiProperty>>;
  ClonePersistentCache: TDictionary<string, TList<TRttiProperty>>;

type
  TKeyKind = (kkUsual, kkFunctional, kkUnknown);

function RegisterKeyMapping(const PlatformKey, VirtualKey: Word; const KeyKind: TKeyKind): Boolean;
function UnregisterKeyMapping(const PlatformKey: Word): Boolean;
function PlatformKeyToVirtualKey(const PlatformKey: Word; var KeyKind: TKeyKind): Word;

implementation

uses
  System.TypInfo, System.RTLConsts, System.Character,
  FMX.Platform, FMX.Forms, FMX.Ani, FMX.Consts, FMX.Styles, FMX.Effects
  {$IFDEF MACOS}, Macapi.CoreFoundation{$ENDIF}, FMX.BehaviorManager;

type

  TOpenCustomAction = class (TCustomAction)
  end;

function IsHandleValid(Hnd: TFmxHandle): Boolean;
begin
  Result := (Hnd <> 0);
end;

procedure RegisterFmxClasses(const RegClasses: array of TPersistentClass;
  const GroupClasses: array of TPersistentClass);
var
  I: Integer;
begin
  StartClassGroup(TFmxObject);
  ActivateClassGroup(TFmxObject);
  for I := Low(GroupClasses) to High(GroupClasses) do
    System.Classes.GroupDescendentsWith(GroupClasses[I], TFmxObject);
  for I := Low(RegClasses) to High(RegClasses) do
    System.Classes.RegisterClass(RegClasses[I]);
end;

procedure RegisterFmxClasses(const RegClasses: array of TPersistentClass);
begin
  RegisterFmxClasses(RegClasses, []);
end;

function DelAmp(const Text: string): string;
var
  I, J: Integer;
  Sb: TCharArray;
begin
  Result := '';
  if Text <> '' then
  begin
    SetLength(Sb, Text.Length);
    I := Low(Text);
    J := 0;
    while I <= High(Text) do
    begin
      if (Text[I] <> '&') then
      begin
        Sb[J] := Text[I];
        Inc(J);
      end
      else
      begin
        if ((I < High(Text)) and (Text[I + 1] = '&')) then
        begin
          Sb[J] := Text[I];
          Inc(J);
          Inc(I);
        end;
      end;
      Inc(I);
    end;
    Result := string.Create(Sb, 0, J);
  end;
end;

{ TPointFHelper }

function TPointFHelper.AngleBetween(const APoint: TPointF): Single;
begin
  Result := ArcCos(AngleCosine(APoint));
end;

function TPointFHelper.AngleBetweenTwoSide(const APoint: TPointF): Single;
begin
  if Self.CrossProduct(APoint) < 0 then
    Result := ArcCos(Self.AngleCosine(APoint))
  else
    Result := - ArcCos(Self.AngleCosine(APoint));
end;

{ TVectorHelper }

function TVectorHelper.AngleBetween(const AVector: TVector): Single;
begin
  Result := ArcCos(AngleCosine(AVector));
end;

function TVectorHelper.AngleBetweenTwoSide(const AVector: TVector): Single;
begin
  if Self.CrossProduct(AVector) < 0 then
    Result := ArcCos(Self.AngleCosine(AVector))
  else
    Result := - ArcCos(Self.AngleCosine(AVector));
end;

{ TPoint3DHelper }

function TPoint3DHelper.AngleBetween(const APoint: TPoint3D): Single;
begin
  Result := ArcCos(AngleCosine(APoint));
end;

{ TVector3DHelper }

function TVector3DHelper.AngleBetween(const AVector: TVector3D): Single;
begin
  Result := ArcCos(AngleCosine(AVector));
end;

function TVector3DHelper.PointProject(const AOrigin, ADirection: TVector3D): Single;
begin
  Result := ADirection.X * (Self.X - AOrigin.X) + ADirection.Y * (Self.Y - AOrigin.Y) + ADirection.Z
    * (Self.Z - AOrigin.Z);
end;

function TVector3DHelper.CalcPlaneNormal(const AVector3D1, AVector3D2: TVector3D): TVector3D;
begin
{$EXCESSPRECISION OFF}
  Result := (AVector3D1 - Self).CrossProduct(AVector3D2 - Self).Normalize;
{$EXCESSPRECISION ON}
end;

{ Resources }

var
  ResourceDict: TDictionary<string,TFmxObjectList> = nil;

procedure AddResource(const AObject: TFmxObject);
var
  ResourceList: TFmxObjectList;
begin
  if not Assigned(ResourceDict) then
    ResourceDict := TDictionary<string,TFmxObjectList>.Create(100);

  if not ResourceDict.TryGetValue(AObject.StyleName, ResourceList) then
  begin
    ResourceList := TFmxObjectList.Create;
    ResourceDict.Add(AObject.StyleName, ResourceList);
  end;

  if ResourceList.IndexOf(AObject) < 0 then
    ResourceList.Add(AObject);
end;

procedure RemoveResource(const AObject: TFmxObject);
var
  ResourceList: TFmxObjectList;
begin
  if Assigned(ResourceDict) and ResourceDict.TryGetValue(AObject.StyleName, ResourceList) then
    ResourceList.Remove(AObject);
end;

function FindStyleResource(const AStyleLookup: string; const Clone: Boolean = False): TFmxObject;
var
  I: Integer;
  ResourceList: TFmxObjectList;
  StyleObject: TFmxObject;
  SaveStyleName: string;
begin
  StyleObject := nil;
  if Assigned(ResourceDict) and ResourceDict.TryGetValue(AStyleLookup, ResourceList) then
    for I := ResourceList.Count - 1 downto 0 do
      if Assigned(ResourceList[I]) and ResourceList[I].Stored then
      begin
        StyleObject := ResourceList[I];
        Break;
      end;
  if Assigned(StyleObject) and Clone then
  begin
    SaveStyleName := StyleObject.StyleName;
    try
      StyleObject.FStyleName := '';
      Result := StyleObject.Clone(nil);
    finally
      StyleObject.FStyleName := SaveStyleName;
    end;
  end
  else
    Result := StyleObject;
end;

{ Scenes }

var
  NeedResetLang: Boolean;

{ Lang }

var
  CollectLang, Lang: TStrings;

procedure CollectLangStart;
begin
  ResetLang;
  if Not Assigned(CollectLang) then
  begin
    CollectLang := TStringList.Create;
//    TStringList(CollectLang).Sorted := True;
    TStringList(CollectLang).CaseSensitive := True;
  end;
end;

procedure CollectLangFinish;
begin
  if Assigned(CollectLang) then
    FreeAndNil(CollectLang);
end;

function CollectLangStrings: TStrings;
begin
  Result := CollectLang;
end;

procedure CollectLangSave;
begin
  if Assigned(CollectLang) then
  begin
    CollectLang.SaveToFile(ExtractFilePath(ParamStr(0)) + 'lang.lng');
  end;
end;

procedure LoadLangFromFile(const AFileName: string);
begin
  if not FileExists(AFileName) then
    Exit;
  ResetLang;
  if Not Assigned(Lang) then
  begin
    Lang := TStringList.Create;
//    TStringList(Lang).Sorted := True;
    TStringList(Lang).CaseSensitive := True;
  end;
  Lang.LoadFromFile(AFileName);
  TStyleManager.UpdateScenes;
end;

procedure LoadLangFromStrings(const AStr: TStrings);
begin
  if Not Assigned(AStr) then
    Exit;
  ResetLang;
  if Not Assigned(Lang) then
  begin
    Lang := TStringList.Create;
//    TStringList(Lang).Sorted := True;
    TStringList(Lang).CaseSensitive := True;
  end;
  Lang.Assign(AStr);
  TStyleManager.UpdateScenes;
end;

procedure ResetLang;
begin
  if Assigned(Lang) then
  begin
    NeedResetLang := True;
    try
      TStyleManager.UpdateScenes;
    finally
      NeedResetLang := False;
      FreeAndNil(Lang);
    end;
  end;
end;

function IndexOfValueStr(Str: TStrings; const Name: string): Integer;
var
  P: Integer;
  S: string;
begin
  for Result := 0 to Str.Count - 1 do
  begin
    S := Str[Result];
    P := S.IndexOf('=') + 1;
    if (P >= 0) and (CompareStr(S.Substring(P), Name) = 0) then Exit;
  end;
  Result := -1;
end;

function Translate(const AText: string): string;
var
  Idx: Integer;
begin
  if AText = '' then
  begin
    Result := '';
    Exit;
  end;
  if Assigned(CustomTranslateProc) then
  begin
    if CustomTranslateProc(AText) <> '' then
      Result := CustomTranslateProc(AText)
    else
      Result := AText;
    Exit;
  end;
  if Assigned(CollectLang) then
  begin
    if CollectLang.IndexOf(AText) < 0 then
      CollectLang.Add(AText)
  end;
  if Assigned(Lang) then
  begin
    if not NeedResetLang then
    begin
      Idx := Lang.IndexOfName(AText);
      if Idx >= 0 then
        Result := Lang.ValueFromIndex[Idx]
      else
        Result := AText;
    end
    else
    begin
      Idx := IndexOfValueStr(Lang, AText);
      if Idx >= 0 then
        Result := Lang.Names[Idx];
      if Result = '' then
        Result := AText;
    end;
  end
  else
    Result := AText;
end;

function TranslateText(const AText: string): string;
begin
  Result := Translate(AText);
end;

procedure ArrangeControl(const Control: IAlignableObject; AAlign: TAlignLayout;
  const AParentWidth, AParentHeight: Single; const ALastWidth, ALastHeight: Single; var R: TRectF);
var
  NewLeft, NewTop, NewWidth, NewHeight: Single;
  cR, mR: TRectF;
  fitScale: Single;
  LControlBounds: TRectF;
  LAV: TSizeF;
begin
  if (AAlign = TAlignLayout.alNone) or (Control.Anchors <> AnchorAlign[AAlign]) then
  begin
    NewLeft := Control.Left - Control.Margins.Left;
    NewTop := Control.Top - Control.Margins.Top;
    NewWidth := Control.Width + Control.Margins.Left + Control.Margins.Right;
    NewHeight := Control.Height + Control.Margins.Top + Control.Margins.Bottom;

    if TAnchorKind.akRight in Control.Anchors then
      if TAnchorKind.akLeft in Control.Anchors then
        // The AnchorRules.X is the original FContext.Width
        NewWidth := AParentWidth - (Control.OriginalParentSize.X - Control.AnchorRules.X)
      else
        // The AnchorRules.X is the original left
        NewLeft := AParentWidth - (ALastWidth - NewLeft)
    else if not(TAnchorKind.akLeft in Control.Anchors) and (Control.OriginalParentSize.X <> 0) then
      // The AnchorRules.X is the original middle of the AControl
      NewLeft := Control.AnchorRules.X * AParentWidth / Control.OriginalParentSize.X - NewWidth / 2;
    if TAnchorKind.akBottom in Control.Anchors then
      if TAnchorKind.akTop in Control.Anchors then
        // The AnchorRules.Y is the original FContext.Height
        NewHeight := AParentHeight - (Control.OriginalParentSize.Y - Control.AnchorRules.Y)
      else
        // The AnchorRules.Y is the original top
        NewTop := AParentHeight - (ALastHeight - NewTop)
    else if not(TAnchorKind.akTop in Control.Anchors) and (Control.OriginalParentSize.Y <> 0) then
      // The AnchorRules.Y is the original middle of the AControl
      NewTop := Control.AnchorRules.Y * AParentHeight / Control.OriginalParentSize.Y - NewHeight / 2;

    Control.AnchorMove := True;
    Control.SetBounds(NewLeft + Control.Margins.Left, NewTop + Control.Margins.Top,
      NewWidth - Control.Margins.Left - Control.Margins.Right,
      NewHeight - Control.Margins.Top - Control.Margins.Bottom);
    Control.AnchorMove := False;

    if AAlign = TAlignLayout.alNone then
      Exit;
  end;

  NewWidth := R.Width;
  if (AAlign in [TAlignLayout.alLeft, TAlignLayout.alRight, TAlignLayout.alVertical,
    TAlignLayout.alMostLeft, TAlignLayout.alMostRight]) then
    NewWidth := Control.Width + Control.Margins.Left + Control.Margins.Right;

  NewHeight := R.Height;
  if (AAlign in [TAlignLayout.alTop, TAlignLayout.alBottom, TAlignLayout.alMostTop,
    TAlignLayout.alMostBottom, TAlignLayout.alHorizontal]) then
    NewHeight := Control.Height + Control.Margins.Top + Control.Margins.Bottom;

  NewLeft := R.Left;
  if (AAlign in [TAlignLayout.alVertical]) then
    NewLeft := Control.Left + Control.Margins.Left;

  NewTop := R.Top;
  if (AAlign in [TAlignLayout.alHorizontal]) then
    NewTop := Control.Top + Control.Margins.Top;

  case AAlign of
    TAlignLayout.alTop, TAlignLayout.alMostTop:
      R.Top := R.Top + NewHeight;
    TAlignLayout.alBottom, TAlignLayout.alMostBottom:
      begin
        R.Bottom := R.Bottom - NewHeight;
        NewTop := R.Bottom;
      end;
    TAlignLayout.alLeft, TAlignLayout.alMostLeft:
      R.Left := R.Left + NewWidth;
    TAlignLayout.alRight, TAlignLayout.alMostRight:
      begin
        R.Right := R.Right - NewWidth;
        NewLeft := R.Right;
      end;
    TAlignLayout.alContents:
      begin
        NewLeft := 0;
        NewTop := 0;
        NewWidth := AParentWidth;
        NewHeight := AParentHeight;
        Control.SetBounds(NewLeft + Control.Margins.Left, NewTop + Control.Margins.Top,
          NewWidth - Control.Margins.Left - Control.Margins.Right,
          NewHeight - Control.Margins.Top - Control.Margins.Bottom);
        Exit;
      end;
    TAlignLayout.alFit, TAlignLayout.alFitLeft, TAlignLayout.alFitRight:
      begin
        // mR := Rect(Padding.Left, Padding.Top, AParentWidth - Padding.Right, AParentHeight - Padding.Bottom);
        mR := RectF(0, 0, AParentWidth, AParentHeight);
        cR := RectF(Control.Left - Control.Margins.Left, Control.Top - Control.Margins.Top,
          Control.Left + Control.Width + Control.Margins.Right, Control.Top + Control.Height + Control.Margins.Bottom);
        fitScale := cR.Fit(mR);
        if (fitScale > 0) and (fitScale < 1) then
        begin
          cR.Left := cR.Left / fitScale;
          cR.Right := cR.Right / fitScale;
          cR.Top := cR.Top / fitScale;
          cR.Bottom := cR.Bottom / fitScale;
          RectCenter(cR, mR);
          if AAlign = TAlignLayout.alFitLeft then
            OffsetRect(cR, mR.Left - cR.Left, 0);
          if AAlign = TAlignLayout.alFitRight then
            OffsetRect(cR, mR.Right - cR.Right, 0);
          NewLeft := cR.Left;
          NewTop := cR.Top;
          NewWidth := cR.Right - cR.Left;
          NewHeight := cR.Bottom - cR.Top;
        end
        else
        begin
          if AAlign = TAlignLayout.alFitLeft then
            OffsetRect(cR, mR.Left - cR.Left, 0);
          if AAlign = TAlignLayout.alFitRight then
            OffsetRect(cR, mR.Right - cR.Right, 0);
          NewLeft := cR.Left;
          NewTop := cR.Top;
          NewWidth := cR.Right - cR.Left;
          NewHeight := cR.Bottom - cR.Top;
        end;
        Control.SetBounds(NewLeft + Control.Margins.Left, NewTop + Control.Margins.Top,
          NewWidth - Control.Margins.Left - Control.Margins.Right,
          NewHeight - Control.Margins.Top - Control.Margins.Bottom);
        if AAlign = TAlignLayout.alFitLeft then
          R.Left := R.Left + NewWidth;
        if AAlign = TAlignLayout.alFitRight then
          R.Right := R.Right - NewWidth;
        Exit;
      end;
    TAlignLayout.alCenter:
      begin
        NewLeft := R.Left + Trunc((NewWidth - (Control.Width + Control.Margins.Left + Control.Margins.Right)) / 2);
        NewWidth := (Control.Width + Control.Margins.Left + Control.Margins.Right);
        NewTop := R.Top + Trunc((NewHeight - (Control.Height + Control.Margins.Top + Control.Margins.Bottom)) / 2);
        NewHeight := (Control.Height + Control.Margins.Top + Control.Margins.Bottom);
      end;
    TAlignLayout.alHorzCenter:
      begin
        NewLeft := R.Left + Trunc((NewWidth - (Control.Width + Control.Margins.Left + Control.Margins.Right)) / 2);
        NewWidth := (Control.Width + Control.Margins.Left + Control.Margins.Right);
      end;
    TAlignLayout.alVertCenter:
      begin
        NewTop := R.Top + Trunc((NewHeight - (Control.Height + Control.Margins.Top + Control.Margins.Bottom)) / 2);
        NewHeight := (Control.Height + Control.Margins.Top + Control.Margins.Bottom);
      end;
  end;

  { Added for controls with fixed-size }
  if (AAlign = TAlignLayout.alScale) then
  begin
    LControlBounds := TRectF.Create(
      TPointF.Create(Control.Left * (AParentWidth / ALastWidth), Control.Top * (AParentHeight / ALastHeight)),
      Control.Width * (AParentWidth / ALastWidth),
      Control.Height * (AParentHeight / ALastHeight));
  end
  else
  begin
    LControlBounds := TRectF.Create(TPointF.Create(NewLeft, NewTop), NewWidth, NewHeight);
    LControlBounds := Control.Margins.PaddingRect(LControlBounds);
  end;
  LAV := Control.AdjustSizeValue;
  if (Control.AdjustType in [TAdjustType.FixedSize, TAdjustType.FixedHeight]) and (LAV.Height > 0) and
     (AAlign in [TAlignLayout.alLeft, TAlignLayout.alRight, TAlignLayout.alMostLeft, TAlignLayout.alMostRight,
                 TAlignLayout.alVertCenter, TAlignLayout.alHorzCenter, TAlignLayout.alCenter,
                 TAlignLayout.alVertical]) then
  begin
    LControlBounds.Top := LControlBounds.Top + Max(0, Round((LControlBounds.Height - LAV.Height) / 2));
    LControlBounds.Height := LAV.Height;
  end;

  if (Control.AdjustType in [TAdjustType.FixedSize, TAdjustType.FixedWidth]) and (LAV.Width > 0) and
     (AAlign in [TAlignLayout.alTop, TAlignLayout.alBottom, TAlignLayout.alMostTop, TAlignLayout.alMostBottom,
                 TAlignLayout.alVertCenter, TAlignLayout.alHorzCenter, TAlignLayout.alCenter,
                 TAlignLayout.alHorizontal]) then
  begin
    LControlBounds.Left := LControlBounds.Left + Max(0, Round((LControlBounds.Width - LAV.Width) / 2));
    LControlBounds.Width := LAV.Width;
  end;
  Control.SetBounds(LControlBounds.Left, LControlBounds.Top, LControlBounds.Width, LControlBounds.Height);
  if AAlign = TAlignLayout.alScale then
    Exit;

  { Adjust client RectF if control didn't resize as we expected }
  if (Control.Width + Control.Margins.Left + Control.Margins.Right <> NewWidth) or
    (Control.Height + Control.Margins.Top + Control.Margins.Bottom <> NewHeight) then
    case AAlign of
      TAlignLayout.alTop:
        R.Top := R.Top - (NewHeight - (Control.Height + Control.Margins.Left + Control.Margins.Right));
      TAlignLayout.alBottom:
        R.Bottom := R.Bottom + (NewHeight - (Control.Height + Control.Margins.Top + Control.Margins.Bottom));
      TAlignLayout.alLeft:
        R.Left := R.Left - (NewWidth - (Control.Width + Control.Margins.Left + Control.Margins.Right));
      TAlignLayout.alRight:
        R.Right := R.Right + (NewWidth - (Control.Width + Control.Margins.Top + Control.Margins.Bottom));
      TAlignLayout.alClient:
        begin
          R.Right := R.Right + NewWidth - (Control.Width + Control.Margins.Left + Control.Margins.Right);
          R.Bottom := R.Bottom + NewHeight - (Control.Height + Control.Margins.Top + Control.Margins.Bottom);
        end;
    end;
end;

{ Align procedure }

procedure AlignObjects(const AParent: TFmxObject; APadding: TBounds; AParentWidth,
  AParentHeight: single; var ALastWidth, ALastHeight: single;
  var ADisableAlign: Boolean);
var
  R: TRectF;
  AlignList: TInterfaceList;

  function InsertBefore(const C1, C2: IAlignableObject; AAlign: TAlignLayout): Boolean;
  begin
    Result := False;
    case AAlign of
      TAlignLayout.alTop, TAlignLayout.alMostTop:
        Result := C1.Top < C2.Top;
      TAlignLayout.alBottom, TAlignLayout.alMostBottom:
        Result := (C1.Top + C1.Height) >= (C2.Top + C2.Height);
      TAlignLayout.alLeft, TAlignLayout.alMostLeft:
        Result := C1.Left < C2.Left;
      TAlignLayout.alRight, TAlignLayout.alMostRight:
        Result := (C1.Left + C1.Width) >= (C2.Left + C2.Width);
    end;
  end;

  procedure DoPosition(const Control: IAlignableObject; AAlign: TAlignLayout);
  begin
    ArrangeControl(Control, AAlign, AParentWidth, AParentHeight, ALastWidth, ALastHeight, R);
  end;

  procedure DoAlign(AAlign: TAlignLayout);
  var
    i, j: Integer;
    Control: IAlignableObject;
    ALCount: Integer;
  begin
    AlignList.Clear;
    for i := 0 to AParent.Children.Count - 1 do
    begin
      if not Supports(AParent.Children[i], IAlignableObject, Control) then
        Continue;
      // Don't realign object with Anchors if it not loaded yet
      if (AAlign = TALignLayout.alNone) and (csLoading in AParent.Children[i].ComponentState) then
        Continue;

      if (Control.Align = AAlign) and (Control.AllowAlign) then
      begin
        j := 0;
        ALCount := AlignList.Count;
        while (j < ALCount) and (AAlign <> TAlignLayout.alNone) and not InsertBefore(Control, IAlignableObject(AlignList[j]), AAlign) do
          Inc(j);
        AlignList.Insert(j, Control);
      end;
    end;
    ALCount := AlignList.Count;
    for i := 0 to ALCount - 1 do
      DoPosition(IAlignableObject(AlignList[i]), AAlign);
  end;

begin
  if (csDestroying in AParent.ComponentState)
    or (not Assigned(AParent.Children))
    or (AParent.Children.Count = 0)
    or ADisableAlign then Exit;
  if csLoading in AParent.ComponentState then
  begin
    ALastWidth := AParentWidth;
    ALastHeight := AParentHeight;
    Exit;
  end;
  if (AParentWidth < 1) or (AParentWidth < 1) then
    Exit;
  if ALastWidth = 0 then
    ALastWidth := AParentWidth;
  if ALastHeight = 0 then
    ALastHeight := AParentHeight;

  ADisableAlign := True;
  try
    R := RectF(0, 0, AParentWidth, AParentHeight);
    if Assigned(APadding) then
      R := APadding.PaddingRect(R);
    AlignList := TInterfaceList.Create;
    try
      // Align
      DoAlign(TAlignLayout.alMostTop);
      DoAlign(TAlignLayout.alMostBottom);
      DoAlign(TAlignLayout.alMostLeft);
      DoAlign(TAlignLayout.alMostRight);
      DoAlign(TAlignLayout.alTop);
      DoAlign(TAlignLayout.alBottom);
      DoAlign(TAlignLayout.alLeft);
      DoAlign(TAlignLayout.alRight);
      DoAlign(TAlignLayout.alFitLeft);
      DoAlign(TAlignLayout.alFitRight);
      DoAlign(TAlignLayout.alClient);
      DoAlign(TAlignLayout.alHorizontal);
      DoAlign(TAlignLayout.alVertical);
      DoAlign(TAlignLayout.alContents);
      DoAlign(TAlignLayout.alCenter);
      DoAlign(TAlignLayout.alHorzCenter);
      DoAlign(TAlignLayout.alVertCenter);
      DoAlign(TAlignLayout.alScale);
      DoAlign(TAlignLayout.alFit);
      // Anchors
      DoAlign(TAlignLayout.alNone);
    finally
      AlignList.Free;
    end;
    ALastWidth := AParentWidth;
    ALastHeight := AParentHeight;
  finally
    ADisableAlign := False;
  end;
end;

{ Geom }

function GetToken(var S: string; const Separators: string; const Stop: string = ''): string;
var
  I: Integer;
  Breaks: string;
begin
  Breaks := Separators + Stop;
  for I := 0 to S.Length - 1 do
  begin
    if Breaks.Contains(S.Chars[I]) then
      Break;
  end;

  Result := S.Substring(0, I);
  S := S.Substring(I + 1);
end;


function WideGetToken(var Pos: Integer; const S: string; const Separators: string;
  const Stop: string = ''): string;
var
  len: Integer;
begin
  Result := '';
  len := S.Length;
  { skip first separators }
  while Pos <= len do
  begin
    if not Separators.Contains(S.Chars[Pos]) then
      Break;
    Inc(Pos);
  end;
  { get }
  while Pos <= len do
  begin
    if Stop.Contains(S.Chars[Pos]) then
      Break;
    if Separators.Contains(S.Chars[Pos]) then
      Break;
    Result := Result + S.Chars[Pos];
    Inc(Pos);
  end;
  { skip separators }
  while Pos <= len do
  begin
    if not Separators.Contains(S.Chars[Pos]) then
      Break;
    Inc(Pos);
  end;
end;

procedure ReverseBytes(P: Pointer; Count: Integer);
var
  P1: PByte;
  P2: PByte;
  C: Byte;
begin
  P1 := PByte(P);
  P2 := PByte(P) + Count - 1;
  while P1 < P2 do
  begin
    C := P1^;
    P1^ := P2^;
    P2^ := C;
    System.inc(P1);
    System.dec(P2);
  end;
end;

procedure FillAlpha(Src: Pointer; Count: Integer; Alpha: Byte);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    PAlphaColorRecArray(Src)[I].A := Alpha;
end;

procedure FillLongword(Src: Pointer; Count: Integer; Value: longword);
var
  I: Integer;
  S: PAlphaColorArray;
begin
  if Value = 0 then
    FillChar(Src^, Count * 4, 0)
  else if Value = $FFFFFFFF then
    FillChar(Src^, Count * 4, $FF)
  else
  begin
    S := PAlphaColorArray(Src);
    for I := 0 to Count - 1 do
      S[I] := Value;
  end;
end;

procedure FillLongwordRect(Src: Pointer; W, H, X1, Y1, X2, Y2: Integer; Value: longword);
var
  j: Integer;
begin
  if X2 > W then
    X2 := W;
  if Y2 > H then
    Y2 := H;
  if X1 > X2 then
    X1 := X1;
  if Y1 > Y2 then
    Y1 := Y2;
  for j := Y1 to Y2 - 1 do
  begin
    if Value = 0 then
      FillChar(PAlphaColorArray(Src)[X1 + (j * W)], (X2 - X1) * 4, 0)
    else if Value = $FFFFFFFF then
      FillChar(PAlphaColorArray(Src)[X1 + (j * W)], (X2 - X1) * 4, $FF)
    else
      FillLongword(@PAlphaColorArray(Src)[X1 + (j * W)], X2 - X1, Value);
  end;
end;

{$EXCESSPRECISION OFF}

function RectToString(R: TRectF): string;
begin
  Result := '(' + FloatToStr(R.Left, USFormatSettings) + ',' + FloatToStr(R.Top, USFormatSettings) + ',' + FloatToStr(R.Right, USFormatSettings) + ',' +
    FloatToStr(R.Bottom, USFormatSettings) + ')';
end;

function StringToRect(S: string): TRectF;
begin
  try
    GetToken(S, ',()');
    Result.Left := StrToFloat(GetToken(S, ',()'), USFormatSettings);
    Result.Top := StrToFloat(GetToken(S, ',()'), USFormatSettings);
    Result.Right := StrToFloat(GetToken(S, ',()'), USFormatSettings);
    Result.Bottom := StrToFloat(GetToken(S, ',()'), USFormatSettings);
  except
    Result := RectF(0, 0, 0, 0);
  end;
end;

function PointToString(R: TPointF): string;
begin
  Result := '(' + FloatToStr(R.X, USFormatSettings) + ',' + FloatToStr(R.Y, USFormatSettings) + ')';
end;

function StringToPoint(S: string): TPointF;
begin
  try
    GetToken(S, ',()');
    Result.X := StrToFloat(GetToken(S, ',()'), USFormatSettings);
    Result.Y := StrToFloat(GetToken(S, ',()'), USFormatSettings);
  except
    Result := PointF(0, 0);
  end;
end;

function CornersF(const Pt1, Pt2, Pt3, Pt4: TPointF): TCornersF;
begin
  Result[0] := Pt1;
  Result[1] := Pt2;
  Result[2] := Pt3;
  Result[3] := Pt4;
end;

function CornersF(Left, Top, Width, Height: Single): TCornersF;
begin
  Result[0] := PointF(Left, Top);
  Result[1] := PointF(Left + Width, Top);
  Result[2] := PointF(Left + Width, Top + Height);
  Result[3] := PointF(Left, Top + Height);
end;

function CornersF(const Rect: TRectF): TCornersF;
begin
  Result[0] := PointF(Rect.Left, Rect.Top);
  Result[1] := PointF(Rect.Right, Rect.Top);
  Result[2] := PointF(Rect.Right, Rect.Bottom);
  Result[3] := PointF(Rect.Left, Rect.Bottom);
end;

function CornersF(const Rect: TRect): TCornersF;
begin
  Result[0] := PointF(Rect.Left, Rect.Top);
  Result[1] := PointF(Rect.Right, Rect.Top);
  Result[2] := PointF(Rect.Right, Rect.Bottom);
  Result[3] := PointF(Rect.Left, Rect.Bottom);
end;

function InterpolateSingle(const Start, Stop, t: Single): Single;
begin
  Result := Start + (Stop - Start) * t;
end;

function InterpolateRotation(Start, Stop, t: Single): Single;
begin
  Result := InterpolateSingle(Start, Stop, t);
end;

function InterpolateColor(const Start, Stop: TAlphaColor; t: Single): TAlphaColor;
begin
  TAlphaColorRec(Result).A := TAlphaColorRec(Start).A + trunc((TAlphaColorRec(Stop).A - TAlphaColorRec(Start).A) * t);
  TAlphaColorRec(Result).R := TAlphaColorRec(Start).R + trunc((TAlphaColorRec(Stop).R - TAlphaColorRec(Start).R) * t);
  TAlphaColorRec(Result).G := TAlphaColorRec(Start).G + trunc((TAlphaColorRec(Stop).G - TAlphaColorRec(Start).G) * t);
  TAlphaColorRec(Result).B := TAlphaColorRec(Start).B + trunc((TAlphaColorRec(Stop).B - TAlphaColorRec(Start).B) * t);
end;

{ interpolations }

function InterpolateBack(t, B, C, D, S: Single; AType: TAnimationType): Single;
begin
  Result := 0;
  case AType of
    TAnimationType.atIn:
      begin
        if S = 0 then
          S := 1.70158;
        t := t / D;
        Result := C * t * t * ((S + 1) * t - S) + B;
      end;
    TAnimationType.atOut:
      begin
        if S = 0 then
          S := 1.70158;
        t := t / D - 1;
        Result := C * (t * t * ((S + 1) * t + S) + 1) + B;
      end;
    TAnimationType.atInOut:
      begin
        if S = 0 then
          S := 1.70158;
        t := t / (D / 2);
        if t < 1 then
        begin
          S := S * 1.525;
          Result := C / 2 * (t * t * ((S + 1) * t - S)) + B;
        end
        else
        begin
          t := t - 2;
          S := S * 1.525;
          Result := C / 2 * (t * t * ((S + 1) * t + S) + 2) + B;
        end;
      end;
  end;
end;

function InterpolateBounce(t, B, C, D: Single; AType: TAnimationType): Single;
  function _EaseOut(t, B, C, D: Single): Single;
  begin
    t := t / D;
    if t < 1 / 2.75 then
    begin
      Result := C * (7.5625 * t * t) + B;
    end
    else if t < 2 / 2.72 then
    begin
      t := t - (1.5 / 2.75);
      Result := C * (7.5625 * t * t + 0.75) + B;
    end
    else if t < 2.5 / 2.75 then
    begin
      t := t - (2.25 / 2.75);
      Result := C * (7.5625 * t * t + 0.9375) + B;
    end
    else
    begin
      t := t - (2.625 / 2.75);
      Result := C * (7.5625 * t * t + 0.984375) + B;
    end;
  end;
  function _EaseIn(t, B, C, D: Single): Single;
  begin
    Result := C - _EaseOut(D - t, 0, C, D) + B;
  end;

begin
  Result := 0;
  case AType of
    TAnimationType.atIn:
      begin
        Result := _EaseIn(t, B, C, D);
      end;
    TAnimationType.atOut:
      begin
        Result := _EaseOut(t, B, C, D);
      end;
    TAnimationType.atInOut:
      begin
        if t < D / 2 then
          Result := _EaseIn(t * 2, 0, C, D) * 0.5 + B
        else
          Result := _EaseOut(t * 2 - D, 0, C, D) * 0.5 + C * 0.5 + B;
      end;
  end;
end;

function InterpolateCirc(t, B, C, D: Single; AType: TAnimationType): Single;
begin
  Result := 0;
  case AType of
    TAnimationType.atIn:
      begin
        t := t / D;
        Result := -C * (Sqrt(1 - t * t) - 1) + B;
      end;
    TAnimationType.atOut:
      begin
        t := t / D - 1;
        Result := C * Sqrt(1 - t * t) + B;
      end;
    TAnimationType.atInOut:
      begin
        t := t / (D / 2);
        if t < 1 then
          Result := -C / 2 * (Sqrt(1 - t * t) - 1) + B
        else
        begin
          t := t - 2;
          Result := C / 2 * (Sqrt(1 - t * t) + 1) + B;
        end;
      end;
  end;
end;

function InterpolateCubic(t, B, C, D: Single; AType: TAnimationType): Single;
begin
  Result := 0;
  case AType of
    TAnimationType.atIn:
      begin
        t := t / D;
        Result := C * t * t * t + B;
      end;
    TAnimationType.atOut:
      begin
        t := t / D - 1;
        Result := C * (t * t * t + 1) + B;
      end;
    TAnimationType.atInOut:
      begin
        t := t / (D / 2);
        if t < 1 then
          Result := C / 2 * t * t * t + B
        else
        begin
          t := t - 2;
          Result := C / 2 * (t * t * t + 2) + B;
        end;
      end;
  end;
end;

function InterpolateElastic(t, B, C, D, A, P: Single; AType: TAnimationType): Single;
var
  S: Single;
begin
  Result := 0;
  case AType of
    TAnimationType.atIn:
      begin
        if t = 0 then
        begin
          Result := B;
          Exit;
        end;
        t := t / D;
        if t = 1 then
        begin
          Result := B + C;
          Exit;
        end;
        if P = 0 then
          P := D * 0.3;
        if (A = 0) or (A < Abs(C)) then
        begin
          A := C;
          S := P / 4;
        end
        else
        begin
          S := P / (2 * Pi) * ArcSin((C / A));
        end;
        t := t - 1;
        Result := -(A * Power(2, (10 * t)) * Sin((t * D - S) * (2 * Pi) / P)) + B;
      end;
    TAnimationType.atOut:
      begin
        if t = 0 then
        begin
          Result := B;
          Exit;
        end;
        t := t / D;
        if t = 1 then
        begin
          Result := B + C;
          Exit;
        end;
        if P = 0 then
          P := D * 0.3;
        if (A = 0) or (A < Abs(C)) then
        begin
          A := C;
          S := P / 4;
        end
        else
        begin
          S := P / (2 * Pi) * ArcSin((C / A));
        end;
        Result := A * Power(2, (-10 * t)) * Sin((t * D - S) * (2 * Pi) / P) + C + B;
      end;
    TAnimationType.atInOut:
      begin
        if t = 0 then
        begin
          Result := B;
          Exit;
        end;
        t := t / (D / 2);
        if t = 2 then
        begin
          Result := B + C;
          Exit;
        end;
        if P = 0 then
          P := D * (0.3 * 1.5);
        if (A = 0) or (A < Abs(C)) then
        begin
          A := C;
          S := P / 4;
        end
        else
        begin
          S := P / (2 * Pi) * ArcSin((C / A));
        end;

        if t < 1 then
        begin
          t := t - 1;
          Result := -0.5 * (A * Power(2, (10 * t)) * Sin((t * D - S) * (2 * Pi) / P)) + B;
        end
        else
        begin
          t := t - 1;
          Result := A * Power(2, (-10 * t)) * Sin((t * D - S) * (2 * Pi) / P) * 0.5 + C + B;
        end;
      end;
  end;
end;

function InterpolateExpo(t, B, C, D: Single; AType: TAnimationType): Single;
begin
  Result := 0;
  case AType of
    TAnimationType.atIn:
      begin
        If t = 0 Then
          Result := B
        else
          Result := C * Power(2, (10 * (t / D - 1))) + B;
      end;
    TAnimationType.atOut:
      begin
        If t = D then
          Result := B + C
        else
          Result := C * (-Power(2, (-10 * t / D)) + 1) + B;
      end;
    TAnimationType.atInOut:
      begin
        if t = 0 then
        begin
          Result := B;
          Exit;
        end;
        if t = D then
        begin
          Result := B + C;
          Exit;
        end;
        t := t / (D / 2);
        if t < 1 then
          Result := C / 2 * Power(2, (10 * (t - 1))) + B
        else
        begin
          t := t - 1;
          Result := C / 2 * (-Power(2, (-10 * t)) + 2) + B;
        end;
      end;
  end;
end;

function InterpolateLinear(t, B, C, D: Single): Single;
begin
  Result := C * t / D + B;
end;

function InterpolateQuad(t, B, C, D: Single; AType: TAnimationType): Single;
begin
  Result := 0;
  case AType of
    TAnimationType.atIn:
      begin
        t := t / D;
        Result := C * t * t + B;
      end;
    TAnimationType.atOut:
      begin
        t := t / D;
        Result := -C * t * (t - 2) + B;
      end;
    TAnimationType.atInOut:
      begin
        t := t / (D / 2);

        if t < 1 then
          Result := C / 2 * t * t + B
        else
        begin
          t := t - 1;
          Result := -C / 2 * (t * (t - 2) - 1) + B;
        end;
      end;
  end;
end;

function InterpolateQuart(t, B, C, D: Single; AType: TAnimationType): Single;
begin
  Result := 0;
  case AType of
    TAnimationType.atIn:
      begin
        t := t / D;
        Result := C * t * t * t * t + B;
      end;
    TAnimationType.atOut:
      begin
        t := t / D - 1;
        Result := -C * (t * t * t * t - 1) + B;
      end;
    TAnimationType.atInOut:
      begin
        t := t / (D / 2);
        if t < 1 then
          Result := C / 2 * t * t * t * t + B
        else
        begin
          t := t - 2;
          Result := -C / 2 * (t * t * t * t - 2) + B;
        end;
      end;
  end;
end;

function InterpolateQuint(t, B, C, D: Single; AType: TAnimationType): Single;
begin
  Result := 0;
  case AType of
    TAnimationType.atIn:
      begin
        t := t / D;
        Result := C * t * t * t * t * t + B;
      end;
    TAnimationType.atOut:
      begin
        t := t / D - 1;
        Result := C * (t * t * t * t * t + 1) + B;
      end;
    TAnimationType.atInOut:
      begin
        t := t / (D / 2);
        if t < 1 then
          Result := C / 2 * t * t * t * t * t + B
        else
        begin
          t := t - 2;
          Result := C / 2 * (t * t * t * t * t + 2) + B;
        end;
      end;
  end;
end;

function InterpolateSine(t, B, C, D: Single; AType: TAnimationType): Single;
begin
  Result := 0;
  case AType of
    TAnimationType.atIn:
      begin
        Result := -C * Cos(t / D * (Pi / 2)) + C + B;
      end;
    TAnimationType.atOut:
      begin
        Result := C * Sin(t / D * (Pi / 2)) + B;
      end;
    TAnimationType.atInOut:
      begin
        Result := -C / 2 * (Cos(Pi * t / D) - 1) + B;
      end;
  end;
end;

{$EXCESSPRECISION ON}

{ Spline }

{
  See: http://en.wikipedia.org/wiki/Cubic_Hermite_spline
}
function KochanekBartelsInterpolate(const Factor: Single; const Tangents: array of TTangentPair;
  const Values: array of Single; const Closed: Boolean = False): Single;
var
  h00, h10, h01, h11: Single;
  LFactor, LFactorSquared, LFactorCubed: Single;
  I, Ip1: Integer;
begin

  I := Trunc(Factor);
  LFactor := Factor - I;

  Ip1 := I + 1;

  if Ip1 >= Length(Values) then
    if Closed then
      Ip1 := Ip1 - Length(Values)
    else
      Ip1 := Length(Values) - 1;

  LFactorSquared := LFactor * LFactor;
  LFactorCubed := LFactorSquared * LFactor;

  h00 := (2 * LFactorCubed) - (3 * LFactorSquared) + 1;
  h10 := LFactorCubed - (2 * LFactorSquared) + LFactor;
  h01 := (-2 * LFactorCubed) + (3 * LFactorSquared);
  h11 := LFactorCubed - LFactorSquared;

  Result := (h00 * Values[I]) + (h10 * Tangents[I].I) + (h01 * Values[Ip1]) + (h11 * Tangents[I].Ip1);
end;

{
  See: http://en.wikipedia.org/wiki/Kochanek–Bartels_spline
}
procedure CalculateKochanekBartelsTangents(const Tension, Bias, Continuity: Single;
  const Values: array of Single; var Tangents: array of TTangentPair;
  const Closed: Boolean = False);
var
  I, Im1, Ip1, Ip2, LHigh: Integer;
begin
  LHigh := High(Values);
  for I := 0 to LHigh do
  begin
    Im1 := I - 1;
    if Im1 < 0 then
      if Closed then
        Im1 := LHigh
      else
        Im1 := 0;

    Ip1 := I + 1;
    if Ip1 > LHigh then
      if Closed then
        Ip1 := 0
      else
        Ip1 := LHigh;

    Ip2 := I + 2;
    if Ip2 > LHigh then
      if Closed then
        Ip2 := Ip2 - Length(Values)
      else
        Ip2 := LHigh;

    Tangents[I].I := 0.5 * (1 - Tension) * ((1 + Bias) * (1 + Continuity) * (Values[I] - Values[Im1])
      + (1 - Bias) * (1 - Continuity) * (Values[Ip1] - Values[I]));

    Tangents[I].Ip1 := 0.5 * (1 - Tension) * ((1 + Bias) * (1 - Continuity) * (Values[Ip1] - Values[I])
      + (1 - Bias) * (1 + Continuity) * (Values[Ip2] - Values[Ip1]));

  end;

end;

constructor TSpline.Create(const Polygon: TPolygon);
var
  I: Integer;
const
  Tension: Single = 0.0;
  Bias: Single = 0.0;
  Continuity: Single = 0.0;
begin
  inherited Create;
  SetLength(FValuesX, Length(Polygon));
  SetLength(FValuesY, Length(Polygon));
  SetLength(FTangentsX, Length(Polygon));
  SetLength(FTangentsY, Length(Polygon));
  for I := 0 to Length(Polygon) - 1 do
  begin
    FValuesX[I] := Polygon[I].X;
    FValuesY[I] := Polygon[I].Y;
  end;
  CalculateKochanekBartelsTangents(Tension, Bias, Continuity, FValuesX, FTangentsX);
  CalculateKochanekBartelsTangents(Tension, Bias, Continuity, FValuesY, FTangentsY);
end;

destructor TSpline.Destroy;
begin
  inherited;
end;

procedure TSpline.SplineXY(const t: Single; var X, Y: Single);
begin
  X := KochanekBartelsInterpolate(t, FTangentsX, FValuesX);
  Y := KochanekBartelsInterpolate(t, FTangentsY, FValuesY);
end;

{ TBounds }

constructor TBounds.Create(const ADefaultValue: TRectF);
begin
  inherited Create;
  FDefaultValue := ADefaultValue;
  Rect := FDefaultValue;
end;

procedure TBounds.Assign(Source: TPersistent);
begin
  if Source is TBounds then
    Rect := TBounds(Source).Rect
  else if not Assigned(Source) then
    Rect := FDefaultValue
  else
    inherited
end;

function TBounds.GetRect: TRectF;
begin
  Result := TRectF.Create(FLeft, FTop, FRight, FBottom);
end;

procedure TBounds.SetRect(const Value: TRectF);
begin
  if Rect <> Value then
  begin
    FLeft := Value.Left;
    FTop := Value.Top;
    FRight := Value.Right;
    FBottom := Value.Bottom;
    DoChange;
  end;
end;

function TBounds.PaddingRect(const R: TRectF): TRectF;
begin
  Result := TRectF.Create(R.Left + FLeft, R.Top + FTop, R.Right - FRight, R.Bottom - FBottom);
end;

function TBounds.MarginRect(const R: TRectF): TRectF;
begin
  Result := TRectF.Create(R.Left - FLeft, R.Top - FTop, R.Right + FRight, R.Bottom + FBottom);
end;

function TBounds.Width: Single;
begin
  Result := Rect.Width;
end;

function TBounds.Height: Single;
begin
  Result := Rect.Height;
end;

function TBounds.IsBottomStored: Boolean;
begin
  Result := not SameValue(FBottom, FDefaultValue.Bottom, Epsilon);
end;

function TBounds.IsLeftStored: Boolean;
begin
  Result := not SameValue(FLeft, FDefaultValue.Left, Epsilon);
end;

function TBounds.IsRightStored: Boolean;
begin
  Result := not SameValue(FRight, FDefaultValue.Right, Epsilon);
end;

function TBounds.IsTopStored: Boolean;
begin
  Result := not SameValue(FTop, FDefaultValue.Top, Epsilon);
end;

function TBounds.MarginEmpty: Boolean;
begin
  Result := SameValue(FLeft, 0, Epsilon) and
            SameValue(FTop, 0, Epsilon) and
            SameValue(FRight, 0, Epsilon) and
            SameValue(FBottom, 0, Epsilon);
end;

function TBounds.Empty: Boolean;
begin
  Result := IsRectEmpty(Rect)
end;

function TBounds.Equals(Obj: TObject): Boolean;
begin
  if (Obj is TBounds) then
    Result := TBounds(Obj).Rect = Rect
  else
    Result := inherited;
end;

procedure TBounds.SetBottom(const Value: Single);
begin
  if not SameValue(FBottom, Value, Epsilon) then
  begin
    FBottom := Value;
    DoChange;
  end;
end;

procedure TBounds.SetLeft(const Value: Single);
begin
  if not SameValue(FLeft, Value, Epsilon) then
  begin
    FLeft := Value;
    DoChange;
  end;
end;

procedure TBounds.SetRight(const Value: Single);
begin
  if not SameValue(FRight, Value, Epsilon) then
  begin
    FRight := Value;
    DoChange;
  end;
end;

procedure TBounds.SetTop(const Value: Single);
begin
  if not SameValue(FTop, Value, Epsilon) then

  begin
    FTop := Value;
    DoChange;
  end;
end;

function TBounds.ToString: string;
begin
  Result := Format('%s (%4.2f,%4.2f)-(%4.2f,%4.2f)', [inherited ToString, FLeft, FTop, FRight, FBottom]);
end;

procedure TBounds.DefineProperties(Filer: TFiler);
begin
  inherited;
  // Compatible
  Filer.DefineProperty('Rect', ReadRect, nil, False);
  Filer.DefineProperty('RectI', ReadRectInt, nil, False);
  Filer.DefineProperty('LeftI', ReadLeftInt, nil, False);
  Filer.DefineProperty('RightI', ReadRightInt, nil, False);
  Filer.DefineProperty('TopI', ReadTopInt, nil, not False);
  Filer.DefineProperty('BottomI', ReadBottomInt, nil, False);
end;

procedure TBounds.DoChange;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TBounds.ReadLeftInt(Reader: TReader);
begin
  FLeft := Reader.ReadInteger;
end;

procedure TBounds.ReadRightInt(Reader: TReader);
begin
  FRight := Reader.ReadInteger;
end;

procedure TBounds.ReadTopInt(Reader: TReader);
begin
  FTop := Reader.ReadInteger;
end;

procedure TBounds.ReadBottomInt(Reader: TReader);
begin
  FBottom := Reader.ReadInteger;
end;

procedure TBounds.ReadRect(Reader: TReader);
begin
  Rect := StringToRect(Reader.ReadString);
end;

procedure TBounds.ReadRectInt(Reader: TReader);
begin
  Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

{ TPosition }

constructor TPosition.Create(const ADefaultValue: TPointF);
begin
  inherited Create;
  FDefaultValue := ADefaultValue;
  FX := FDefaultValue.X;
  FY := FDefaultValue.Y;
end;

procedure TPosition.Assign(Source: TPersistent);
begin
  if Source is TPosition then
  begin
    Point := TPosition(Source).Point;
  end
  else
    inherited
end;

procedure TPosition.SetPointNoChange(const P: TPointF);
begin
  FX := P.X;
  FY := P.Y;
end;

function TPosition.Empty: Boolean;
begin
  Result := (FX = 0) and (FY = 0);
end;

procedure TPosition.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Point', ReadPoint, WritePoint, False);
  Filer.DefineProperty('XI', ReadXInt, WriteXInt, FStoreAsInt and not SameValue(FX, FDefaultValue.X, Epsilon));
  Filer.DefineProperty('YI', ReadYInt, WriteYInt, FStoreAsInt and not SameValue(FY, FDefaultValue.Y, Epsilon));
end;

procedure TPosition.ReadXInt(Reader: TReader);
begin
  FX := Reader.ReadInteger;
  FStoreAsInt := True;
end;

procedure TPosition.WriteXInt(Writer: TWriter);
begin
  Writer.WriteInteger(Trunc(FX));
end;

procedure TPosition.ReadYInt(Reader: TReader);
begin
  FY := Reader.ReadInteger;
  FStoreAsInt := True;
end;

procedure TPosition.WriteYInt(Writer: TWriter);
begin
  Writer.WriteInteger(Trunc(FY));
end;

procedure TPosition.DoChange;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TPosition.ReadPoint(Reader: TReader);
begin
  Point := StringToPoint(Reader.ReadString);
end;

procedure TPosition.WritePoint(Writer: TWriter);
begin
  Writer.WriteString(String(PointToString(Point)));
end;

function TPosition.IsXStored: Boolean;
begin
  Result := not SameValue(FDefaultValue.X, FX, Epsilon) and not FStoreAsInt;
end;

function TPosition.IsYStored: Boolean;
begin
  Result := not SameValue(FDefaultValue.Y, FY, Epsilon) and not FStoreAsInt;
end;

function TPosition.GetVector: TVector;
begin
  Result := System.Types.Vector(FX, FY);
end;

procedure TPosition.SetVector(const Value: TVector);
begin
  Point := PointF(Value.X, Value.Y);
end;

procedure TPosition.Reflect(const Normal: TVector);
begin
  Vector := Vector.Reflect(Normal);
end;

function TPosition.GetPoint: TPointF;
begin
  Result := PointF(FX, FY);
end;

procedure TPosition.SetPoint(const Value: TPointF);
var
  LChange: Boolean;
begin
  LChange := not (SameValue(FX, Value.X, Epsilon) and SameValue(FY, Value.Y, Epsilon));
  FX := Value.X;
  FY := Value.Y;
  if LChange then
    DoChange;
end;

procedure TPosition.SetX(const Value: Single);
var
  LChange: Boolean;
begin
  LChange := not SameValue(FX, Value, Epsilon);
  FX := Value;
  if LChange then
    DoChange;
end;

procedure TPosition.SetY(const Value: Single);
var
  LChange: Boolean;
begin
  LChange := not SameValue(FY, Value, Epsilon);
  FY := Value;
  if LChange then
    DoChange;
end;

{ TTransform }

constructor TTransform.Create;
begin
  inherited;
  FMatrix := TMatrix.Identity;
  FPosition := TPosition.Create(PointF(0, 0));
  FPosition.OnChange := MatrixChanged;
  FScale := TPosition.Create(PointF(1, 1));
  FScale.OnChange := MatrixChanged;
  FSkew := TPosition.Create(PointF(0, 0));
  FSkew.OnChange := MatrixChanged;
  FRotationCenter := TPosition.Create(PointF(0.5, 0.5));
  FRotationCenter.OnChange := MatrixChanged;
end;

destructor TTransform.Destroy;
begin
  FRotationCenter.Free;
  FScale.Free;
  FSkew.Free;
  FPosition.Free;
  inherited;
end;

procedure TTransform.Assign(Source: TPersistent);
begin
  if Source is TTransform then
  begin
    FPosition.FX := TTransform(Source).Position.FX;
    FPosition.FY := TTransform(Source).Position.FY;
    FScale.FX := TTransform(Source).Scale.FX;
    FScale.FY := TTransform(Source).Scale.FY;
    FSkew.FX := TTransform(Source).Skew.FX;
    FSkew.FY := TTransform(Source).Skew.FY;
    FRotationCenter.FX := TTransform(Source).RotationCenter.FX;
    FRotationCenter.FY := TTransform(Source).RotationCenter.FY;
    MatrixChanged(Self);
  end
  else
    inherited
end;

procedure TTransform.MatrixChanged(Sender: TObject);
begin
  FMatrix := TMatrix.Identity;
  FMatrix.m31 := FPosition.X;
  FMatrix.m32 := FPosition.Y;
  FMatrix.m13 := FSkew.X;
  FMatrix.m23 := FSkew.Y;
  FMatrix.m11 := FScale.X;
  FMatrix.m22 := FScale.Y;
  if FRotationAngle <> 0 then
    FMatrix := TMatrix.CreateRotation(DegToRad(FRotationAngle)) * FMatrix;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TTransform.SetPosition(const Value: TPosition);
begin
  FPosition.Assign(Value);
end;

procedure TTransform.SetRotationAngle(const Value: Single);
begin
  if FRotationAngle <> Value then
  begin
    FRotationAngle := Value;
  end;
end;

procedure TTransform.SetScale(const Value: TPosition);
begin
  FScale.Assign(Value);
end;

{ TAniThread }

type

  TAniThread = class(TTimer)
  private
    FAniList: TList<TAnimation>;
    FTime, FDeltaTime: Extended;
    FTimerService: IFMXTimerService;
    procedure OneStep;
    procedure DoSyncTimer(Sender: TObject);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure AddAnimation(const Ani: TAnimation);
    procedure RemoveAnimation(const Ani: TAnimation);
  end;

constructor TAniThread.Create;
begin
  inherited Create(nil);
  if not TPlatformServices.Current.SupportsPlatformService(IFMXTimerService, IInterface(FTimerService)) then
    raise EUnsupportedPlatformService.Create('IFMXTimerService');
  if AniFrameRate < 5 then
    AniFrameRate := 5;
  if AniFrameRate > 100 then
    AniFrameRate := 100;
  Interval := trunc(1000 / AniFrameRate / 10) * 10;
  if (Interval <= 0) then Interval := 1;

  OnTimer := DoSyncTimer;
  FAniList := TList<TAnimation>.Create;
  FTime := FTimerService.GetTick;

  Enabled := False;
end;

destructor TAniThread.Destroy;
begin
  FreeAndNil(FAniList);
  FTimerService := nil;
  inherited;
end;

procedure TAniThread.AddAnimation(const Ani: TAnimation);
begin
  if FAniList.IndexOf(Ani) < 0 then
    FAniList.Add(Ani);
  if not Enabled and (FAniList.Count > 0) then
    FTime := FTimerService.GetTick;
  Enabled := FAniList.Count > 0;
end;

procedure TAniThread.RemoveAnimation(const Ani: TAnimation);
begin
  FAniList.Remove(Ani);
  Enabled := FAniList.Count > 0;
end;

procedure TAniThread.DoSyncTimer(Sender: TObject);
begin
  OneStep;
  if AniFrameRate < 5 then
    AniFrameRate := 5;
  Interval := trunc(1000 / AniFrameRate / 10) * 10;
  if (Interval <= 0) then Interval := 1;
end;

procedure TAniThread.OneStep;
var
  I: Integer;
  NewTime: Extended;
begin
  NewTime := FTimerService.GetTick;
  FDeltaTime := NewTime - FTime;
  FTime := NewTime;
  if FDeltaTime <= 0 then
    Exit;
  if FAniList.Count > 0 then
  begin
    I := FAniList.Count - 1;
    while I >= 0 do
    begin
      if FAniList[I].FRunning then
      begin
        if (FAniList[I].StyleName <> '') and
          (CompareText(FAniList[I].StyleName, 'caret') = 0) then
        begin
          FAniList[I].Tag := FAniList[I].Tag + 1;
          if FAniList[I].Tag mod 12 = 0 then
          begin
            FAniList[I].ProcessTick(FTime, FDeltaTime);
          end;
        end
        else
          FAniList[I].ProcessTick(FTime, FDeltaTime);
      end;
      dec(I);
      if I >= FAniList.Count then
        I := FAniList.Count - 1;
    end;
  end;
end;

{ TAnimation }

procedure TAnimation.ChangeParent;
begin
  inherited;
  ParseTriggers(nil, False, False);
end;

constructor TAnimation.Create(AOwner: TComponent);
begin
  inherited;
  FEnabled := False;
  Duration := 0.2;
end;

destructor TAnimation.Destroy;
begin
  if Assigned(AniThread) then
    TAniThread(AniThread).FAniList.Remove(Self);
  FreeAndNil(FTriggerList);
  FreeAndNil(FInverseTriggerList);
  inherited;
end;

procedure TAnimation.FirstFrame;
begin

end;

procedure TAnimation.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) and Enabled then
    Start;
end;

procedure TAnimation.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if not(csDesigning in ComponentState) and not(csLoading in ComponentState) and
      not(csReading in ComponentState) then
    begin
      if FEnabled then
        Start
      else
        Stop;
    end;
  end;
end;

procedure TAnimation.SetTrigger(const Value: TTrigger);
begin
  FTrigger := Value;
  ParseTriggers(nil, False, False);
end;

procedure TAnimation.SetTriggerInverse(const Value: TTrigger);
begin
  FTriggerInverse := Value;
  ParseTriggers(nil, False, False);
end;

function TAnimation.NormalizedTime: Single;
begin
  Result := 0;
  if (FDuration > 0) and (FDelayTime <= 0) then
  begin
    case FInterpolation of
      TInterpolationType.itLinear:
        Result := InterpolateLinear(FTime, 0, 1, FDuration);
      TInterpolationType.itQuadratic:
        Result := InterpolateQuad(FTime, 0, 1, FDuration, FAnimationType);
      TInterpolationType.itCubic:
        Result := InterpolateCubic(FTime, 0, 1, FDuration, FAnimationType);
      TInterpolationType.itQuartic:
        Result := InterpolateQuart(FTime, 0, 1, FDuration, FAnimationType);
      TInterpolationType.itQuintic:
        Result := InterpolateQuint(FTime, 0, 1, FDuration, FAnimationType);
      TInterpolationType.itSinusoidal:
        Result := InterpolateSine(FTime, 0, 1, FDuration, FAnimationType);
      TInterpolationType.itExponential:
        Result := InterpolateExpo(FTime, 0, 1, FDuration, FAnimationType);
      TInterpolationType.itCircular:
        Result := InterpolateCirc(FTime, 0, 1, FDuration, FAnimationType);
      TInterpolationType.itElastic:
        Result := InterpolateElastic(FTime, 0, 1, FDuration, 0, 0, FAnimationType);
      TInterpolationType.itBack:
        Result := InterpolateBack(FTime, 0, 1, FDuration, 0, FAnimationType);
      TInterpolationType.itBounce:
        Result := InterpolateBounce(FTime, 0, 1, FDuration, FAnimationType);
    end;
  end;
end;

procedure TAnimation.DoProcess;
begin
  if Assigned(FOnProcess) then
    FOnProcess(Self);
end;

procedure TAnimation.DoFinish;
begin
  if Assigned(FOnFinish) then
    FOnFinish(Self);
end;

procedure TAnimation.ProcessTick(time, deltaTime: Single);
begin
  inherited;
  if (csDesigning in ComponentState) then
    Exit;
  if csDestroying in ComponentState then
    Exit;

  if Assigned(Parent) and (Parent.IsIControl) and (not Parent.AsIControl.Visible) then
    Stop;

  if not FRunning then
    Exit;
  if FPause then
    Exit;

  if (FDelay > 0) and (FDelayTime <> 0) then
  begin
    if FDelayTime > 0 then
    begin
      FDelayTime := FDelayTime - deltaTime;
      if FDelayTime <= 0 then
      begin
        FDelayTime := 0;
        if FInverse then
          FTime := FDuration
        else
          FTime := 0;
        FirstFrame;
        ProcessAnimation;
        DoProcess;
      end;
    end;
    Exit;
  end;

  if FInverse then
    FTime := FTime - deltaTime
  else
    FTime := FTime + deltaTime;
  if FTime >= FDuration then
  begin
    FTime := FDuration;
    if FLoop then
    begin
      if FAutoReverse then
      begin
        FInverse := True;
        FTime := FDuration;
      end
      else
        FTime := 0;
    end
    else
      if FAutoReverse and (FTickCount = 0) then
      begin
        Inc(FTickCount);
        FInverse := True;
        FTime := FDuration;
      end
      else
        FRunning := False;
  end
  else if FTime <= 0 then
  begin
    FTime := 0;
    if FLoop then
    begin
      if FAutoReverse then
      begin
        FInverse := False;
        FTime := 0;
      end
      else
        FTime := FDuration;
    end
    else
      if FAutoReverse and (FTickCount = 0) then
      begin
        Inc(FTickCount);
        FInverse := False;
        FTime := 0;
      end
      else
        FRunning := False;
  end;

  ProcessAnimation;
  DoProcess;

  if not FRunning then
  begin
    if Assigned(AniThread) then
      TAniThread(AniThread).RemoveAnimation(Self);
    DoFinish;
  end;
end;

procedure TAnimation.Start;
begin
  if not FLoop then
    FTickCount := 0;
  if Assigned(Parent) and (Parent.IsIControl) and (not Parent.AsIControl.Visible) then
    Exit;
  if (Abs(FDuration) < 0.001) or (Not Assigned(FRoot)) or (csDesigning in ComponentState) then
  begin
    { immediate animation }
    FDelayTime := 0;
    if FInverse then
    begin
      FTime := 0;
      FDuration := 1;
    end
    else
    begin
      FTime := 1;
      FDuration := 1;
    end;
    FRunning := True;
    ProcessAnimation;
    DoProcess;
    FRunning := False;
    FTime := 0;
    FDuration := 0.00001;
    DoFinish;
  end
  else
  begin
    FDelayTime := FDelay;
    FRunning := True;
    if FInverse then
      FTime := FDuration
    else
      FTime := 0;
    if FDelay = 0 then
    begin
      FirstFrame;
      ProcessAnimation;
      DoProcess;
    end;

    if not Assigned(AniThread) then
      AniThread := TAniThread.Create;

    TAniThread(AniThread).AddAnimation(Self);
    if not AniThread.Enabled then
      Stop
    else
      FEnabled := True;
  end;
end;

procedure TAnimation.Stop;
begin
  if not FRunning then
    Exit;

  if Assigned(AniThread) then
    TAniThread(AniThread).RemoveAnimation(Self);

  if FInverse then
    FTime := 0
  else
    FTime := FDuration;
  ProcessAnimation;
  DoProcess;
  FRunning := False;
  DoFinish;
end;

procedure TAnimation.StopAtCurrent;
begin
  if not FRunning then
    Exit;

  if Assigned(AniThread) then
    TAniThread(AniThread).RemoveAnimation(Self);

  if FInverse then
    FTime := 0
  else
    FTime := FDuration;
  FRunning := False;
  FEnabled := False;
  DoFinish;
end;

procedure TAnimation.ParseTriggers(const AInstance: TFmxObject; Normal, Inverse: Boolean);
var
  T: TRttiType;
  P: TRttiProperty;
  Line, Setter, Prop, Value: string;
  Trigger: TTriggerRec;
begin
  if not Assigned(AInstance) then
  begin
    if Assigned(FTriggerList) then
      FreeAndNil(FTriggerList);
    if Assigned(FInverseTriggerList) then
      FreeAndNil(FInverseTriggerList);
    FTargetClass := nil;
    Exit;
  end;

  if ((Inverse and Assigned(FInverseTriggerList)) or not Inverse)
    and ((Normal and Assigned(FTriggerList)) or not Normal) then Exit;

  T := SharedContext.GetType(AInstance.ClassInfo);
  if not Assigned(T) then Exit;

  while Inverse do
  begin
    if Assigned(FInverseTriggerList) then
      Break
    else
      FInverseTriggerList := TList<TTriggerRec>.Create;

    Line := FTriggerInverse;
    Setter := GetToken(Line, ';');
    while Setter <> '' do
    begin
      Prop := GetToken(Setter, '=');
      Value := Setter;
      P := T.GetProperty(Prop);
      if Assigned(P) and (P.PropertyType.TypeKind = tkEnumeration) then
      begin
        Trigger.Name := Prop;
        Trigger.Prop := P;
        Trigger.Value := StrToBoolDef(Value, True);
        FInverseTriggerList.Add(Trigger);
      end
      else
      begin
        FreeAndNil(FInverseTriggerList);
        Break;
      end;
      Setter := GetToken(Line, ';');
    end;
    Break;
  end;

  while Normal do
  begin
    if Assigned(FTriggerList) then
      Break
    else
      FTriggerList := TList<TTriggerRec>.Create;

    Line := FTrigger;
    Setter := GetToken(Line, ';');
    while Setter <> '' do
    begin
      Prop := GetToken(Setter, '=');
      Value := Setter;
      P := T.GetProperty(Prop);
      if Assigned(P) and (P.PropertyType.TypeKind = tkEnumeration) then
      begin
        Trigger.Name := Prop;
        Trigger.Prop := P;
        Trigger.Value := StrToBoolDef(Value, True);
        FTriggerList.Add(Trigger);
      end
      else
      begin
        FreeAndNil(FTriggerList);
        Break;
      end;
      Setter := GetToken(Line, ';');
    end;
    Break;
  end;

  if Assigned(FInverseTriggerList) or Assigned(FTriggerList) then
    FTargetClass := AInstance.ClassType;
end;

procedure TAnimation.StartTrigger(const AInstance: TFmxObject; const ATrigger: string);
var
  V: TValue;
  StartValue: Boolean;
  ContainsInTrigger, ContainsInTriggerInverse: Boolean;
  I: Integer;
  Trigger: TTriggerRec;
begin
  if not Assigned(AInstance) then
    Exit;

  ContainsInTrigger := ContainsText(FTrigger, ATrigger);
  ContainsInTriggerInverse := ContainsText(FTriggerInverse, ATrigger);

  ParseTriggers(AInstance, ContainsInTrigger, ContainsInTriggerInverse);

  if not AInstance.InheritsFrom(FTargetClass) then
    Exit;

  if ContainsInTrigger or ContainsInTriggerInverse then
  begin
    if Assigned(FInverseTriggerList) and (FInverseTriggerList.Count > 0) and ContainsInTriggerInverse then
    begin
      StartValue := False;
      for I := 0 to FInverseTriggerList.Count - 1 do
      begin
        Trigger := FInverseTriggerList[I];
        V := Trigger.Prop.GetValue(AInstance);
        StartValue := V.AsBoolean = Trigger.Value;
        if not StartValue then
          Break;
      end;
      if StartValue then
      begin
        Inverse := True;
        Start;
        Exit;
      end;
    end;
    if Assigned(FTriggerList) and (FTriggerList.Count > 0) and ContainsInTrigger then
    begin
      StartValue := False;
      for I := 0 to FTriggerList.Count - 1 do
      begin
        Trigger := FTriggerList[I];
        V := Trigger.Prop.GetValue(AInstance);
        StartValue := V.AsBoolean = Trigger.Value;
        if not StartValue then
          Break;
      end;
      if StartValue then
      begin
        if FTriggerInverse <> '' then
          Inverse := False;
        Start;
      end;
    end;
  end;
end;

procedure TAnimation.StopTrigger(const AInstance: TFmxObject; const ATrigger: string);
begin
  if not Assigned(AInstance) then
    Exit;
  if (FTriggerInverse <> '') and string(FTriggerInverse).ToLower.Contains(ATrigger.ToLower) then
    Stop;
  if (FTrigger <> '') and string(FTrigger).ToLower.Contains(ATrigger.ToLower) then
    Stop;
end;

{ TFmxReader }

type
  TFmxReader = class(TReader)
  private
  protected
    procedure SetName(Component: TComponent; var Name: string); override;
    function Error(const Message: string): Boolean; override;
  end;

function TFmxReader.Error(const Message: string): Boolean;
begin
  Result := True;
end;

procedure TFmxReader.SetName(Component: TComponent; var Name: string);
begin
  Name := '';
end;

{$REGION 'implementation of purgatory'}
type
  TPurgatory = class (TComponent)
  public const
    TimerInterval: Integer = 10;
  private
    FInstanceList: TList<Pointer>;
    FTimerHandle: TFMXHandle;
    FPlatformTimer: IFMXTimerService;
    procedure StartTimer;
    procedure StopTimer;
    procedure TimerProc;
    procedure UpdateTimer;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(const Instance: TFmxObject);
    procedure Remove(const Instance: TFmxObject);
    function Contains(const Instance: TFmxObject): Boolean;
  end;

var
  vPurgatory: TPurgatory;

{ TPurgatory }

constructor TPurgatory.Create(AOwner: TComponent);
begin
  inherited;
  FTimerHandle := cIdNoTimer;
  FInstanceList := TList<Pointer>.Create;
end;

destructor TPurgatory.Destroy;
begin
  StopTimer;
  FPlatformTimer := nil;
  Clear;
  FreeAndNil(FInstanceList);
  inherited;
end;

function TPurgatory.Contains(const Instance: TFmxObject): Boolean;
begin
  Result := FInstanceList.Contains(Instance);
end;

procedure TPurgatory.Add(const Instance: TFmxObject);
begin
  if Assigned(Instance) and (not Contains(Instance)) then
  begin
    FreeNotification(Instance);
    FInstanceList.Add(Instance);
    if FInstanceList.Count > 0 then
      StartTimer;
  end;
end;

procedure TPurgatory.Remove(const Instance: TFmxObject);
begin
  if Assigned(Instance) then
  begin
    FInstanceList.Remove(Instance);
    RemoveFreeNotification(Instance);
    UpdateTimer;
  end;
end;

procedure TPurgatory.StartTimer;
begin
  if not (csDestroying in ComponentState) then
  begin
    if not Assigned(FPlatformTimer) then
    begin
      if not TPlatformServices.Current.SupportsPlatformService(IFMXTimerService, IInterface(FPlatformTimer)) then
        raise EUnsupportedPlatformService.Create('IFMXTimerService');
    end;
    if FTimerHandle = cIdNoTimer then
      FTimerHandle := FPlatformTimer.CreateTimer(TimerInterval, TimerProc);
  end;
end;

procedure TPurgatory.StopTimer;
begin
  if Assigned(FPlatformTimer) and (FTimerHandle <> cIdNoTimer) then
  begin
    FPlatformTimer.DestroyTimer(FTimerHandle);
    FTimerHandle := cIdNoTimer;
  end;
end;

procedure TPurgatory.TimerProc;
begin
  Clear;
end;

procedure TPurgatory.UpdateTimer;
begin
  if (FInstanceList.Count = 0) and
     (Assigned(FPlatformTimer)) then
    StopTimer;
end;

procedure TPurgatory.Clear;
var
  Instance: TFmxObject;
begin
  try
    while FInstanceList.Count > 0 do
    begin
      Instance := FInstanceList[0];
      FInstanceList.Delete(0);
      RemoveFreeNotification(Instance);
      Instance.DisposeOf;
    end;
  finally
    UpdateTimer;
  end;
end;

procedure TPurgatory.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent is TFmxObject) then
  begin
    FInstanceList.Remove(TFmxObject(AComponent));
    UpdateTimer;
  end;
end;
{$ENDREGION}

{ TFmxObject }

constructor TFmxObject.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIndex := -1;
  FStored := True;
end;

class constructor TFmxObject.Create;
begin
end;

class destructor TFmxObject.Destroy;
begin
end;


destructor TFmxObject.Destroy;
var
  I: Integer;
begin
  FreeAndNil(FActionLink);
  FreeAndNil(FTouchManager);
  SetActionClient(False);
  { NotifList }
  if Assigned(FNotifyList) then
  begin
    FCallingFreeNotify := True;
    for I := FNotifyList.Count - 1 downto 0 do
      IFreeNotification(FNotifyList[I]).FreeNotification(Self);
    FreeAndNil(FNotifyList);
  end;
  { Remove from ResorcesList }
  if StyleName <> '' then
    RemoveFromResourcePool;
  { }
  if Assigned(FParent) then
    FParent.RemoveObject(Self);
  FRoot := nil;
  DeleteChildren;
  if Assigned(FChildrenList) then
    FChildrenList.Free;
  FreeAndNil(FStyleObjectsDict);
  inherited;
end;

procedure TFmxObject.SetActionClient(const Value: boolean);
begin
  if FActionClient <> Value then
  begin
    FActionClient := Value;
    DoActionClientChanged;
  end;
end;

procedure TFmxObject.Release;
begin
  if not (csDestroying in ComponentState) then
  begin
    if Assigned(vPurgatory) then
    begin
      Parent := nil;
      vPurgatory.Add(self);
      FReleased := True;
    end
    else
      raise EInvalidOperation.CreateFMT(SNotInstance, ['TPurgatory']);
  end;
end;

function TFmxObject.Released: Boolean;
begin
  Result := FReleased;
end;

function TFmxObject.ItemClass: string;
begin
  Result := '';
end;

procedure TFmxObject.Loaded;
begin
  inherited;
  if csDestroying in ComponentState then
    Exit;
  if Assigned(FRoot) then
    FixupTabList;
end;

procedure TFmxObject.AddFreeNotify(const AObject: IFreeNotification);
begin
  if not Assigned(FNotifyList) then
    FNotifyList := TList<Pointer>.Create;
  FNotifyList.Add(Pointer(AObject));
end;

procedure TFmxObject.RemoveFreeNotify(const AObject: IFreeNotification);
begin
  if not FCallingFreeNotify and Assigned(FNotifyList) then
    FNotifyList.Remove(Pointer(AObject));
end;

procedure TFmxObject.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = Action then
      Action := nil;
  end;
end;

procedure TFmxObject.RemoveObject(Index: Integer);
begin
  if Assigned(FChildren) and (Index < FChildren.Count) then
    RemoveObject(FChildren[Index]);
end;

procedure TFmxObject.ResetChildrenIndices;
var
  O: TFmxObject;
begin
  if Assigned(FChildren) then
    for O in FChildren do
      O.FIndex := -1;
end;

procedure TFmxObject.FreeNotification(AObject: TObject);
begin
  if AObject is TComponent then
    Notification(TComponent(AObject), opRemove);
end;

function TFmxObject.IsIControl: Boolean;
begin
  Result := False;
end;

function TFmxObject.IsChild(AObject: TFmxObject): Boolean;
begin
  Result := False;
  while not Result and Assigned(AObject) do
  begin
    Result := AObject.Equals(Self);
    if not Result then
      AObject := AObject.Parent;
  end;
end;

function TFmxObject.AsIControl: IControl;
begin
  Result := nil;
end;

function IsUniqueGlobalNameProc(const Name: string): Boolean;
begin
  if Length(Name) = 0 then
    Result := True
  else
    Result := Not Assigned(FindGlobalComponent(Name));
end;

function TFmxObject.Clone(const AOwner: TComponent): TFmxObject;
var
  S: TStream;
  SaveName: string;
  Reader: TFmxReader;
  FSaveIsUniqueGlobalComponentName: TIsUniqueGlobalComponentName;
begin
  ActivateClassGroup(TFmxObject);
  S := TMemoryStream.Create;
  Result := nil;
  try
    { store }
    SaveName := Name;
    Name := '';
    S.WriteComponent(Self);
    Name := SaveName;
    S.Position := 0;
    { load }
    FSaveIsUniqueGlobalComponentName := IsUniqueGlobalComponentNameProc;
    IsUniqueGlobalComponentNameProc := IsUniqueGlobalNameProc;
    try
      Reader := TFmxReader.Create(S, 4096);
      try
        Result := TFmxObject(Reader.ReadRootComponent(nil));
        if Assigned(AOwner) then
          AOwner.InsertComponent(Result);
      finally
        Reader.Free;
        if not Assigned(Result) then
          Result := TFmxObjectClass(ClassType).Create(AOwner);
      end;
    finally
      IsUniqueGlobalComponentNameProc := FSaveIsUniqueGlobalComponentName;
    end;
  finally
    S.Free;
  end;
end;

procedure TFmxObject.DoGesture(const EventInfo: TGestureEventInfo;
  var Handled: Boolean);
begin
  // Override DoGesture to implement default behaviour
  Handled := False;
end;

procedure TFmxObject.CMGesture(var EventInfo: TGestureEventInfo);
var
  Handled: Boolean;
begin
  Handled := False;

  if Assigned(FOnGesture) then
    try
      FOnGesture(Self, EventInfo, Handled);
    except
      Application.HandleException(Self);
    end;

  if not Handled then
    try
      DoGesture(EventInfo, Handled);
    except
      Application.HandleException(Self);
    end;

  if not Handled then
    if Assigned(FParent) and (EventInfo.GestureID <> sgiNoGesture) then
      FParent.CMGesture(EventInfo);
end;

function TFmxObject.ContainsObject(AObject: TFmxObject): Boolean;
begin
  Result := FChildren.Contains(AObject);
end;

{ Property animation }

procedure TFmxObject.AnimateColor(const APropertyName: string; NewValue: TAlphaColor; Duration: Single = 0.2;
  AType: TAnimationType = TAnimationType.atIn;
  AInterpolation: TInterpolationType = TInterpolationType.itLinear);
var
  A: TColorAnimation;
begin
  StopPropertyAnimation(APropertyName);
  A := TColorAnimation.Create(Self);
  A.Parent := Self;
  A.AnimationType := AType;
  A.Interpolation := AInterpolation;
  A.OnFinish := DoAniFinished;
  A.Duration := Duration;
  A.PropertyName := APropertyName;
  A.StartFromCurrent := True;
  A.StopValue := NewValue;
  A.Start;
end;

procedure TFmxObject.AnimateFloat(const APropertyName: string; const NewValue: Single; Duration: Single = 0.2;
  AType: TAnimationType = TAnimationType.atIn;
  AInterpolation: TInterpolationType = TInterpolationType.itLinear);
var
  A: TFloatAnimation;
begin
  StopPropertyAnimation(APropertyName);
  A := TFloatAnimation.Create(Self);
  A.Parent := Self;
  A.AnimationType := AType;
  A.Interpolation := AInterpolation;
  A.OnFinish := DoAniFinished;
  A.OnProcess := DoAniProcess;
  A.Duration := Duration;
  A.PropertyName := APropertyName;
  A.StartFromCurrent := True;
  A.StopValue := NewValue;
  A.Start;
end;

procedure TFmxObject.AnimateFloatDelay(const APropertyName: string; const NewValue: Single; Duration: Single = 0.2;
  Delay: Single = 0.0; AType: TAnimationType = TAnimationType.atIn;
  AInterpolation: TInterpolationType = TInterpolationType.itLinear);
var
  A: TFloatAnimation;
begin
  A := TFloatAnimation.Create(Self);
  A.Parent := Self;
  A.AnimationType := AType;
  A.Interpolation := AInterpolation;
  A.Delay := Delay;
  A.Duration := Duration;
  A.PropertyName := APropertyName;
  A.StartFromCurrent := True;
  A.StopValue := NewValue;
  A.Start;
end;

procedure TFmxObject.AnimateFloatWait(const APropertyName: string; const NewValue: Single; Duration: Single = 0.2;
  AType: TAnimationType = TAnimationType.atIn;
  AInterpolation: TInterpolationType = TInterpolationType.itLinear);
var
  A: TFloatAnimation;
begin
  StopPropertyAnimation(APropertyName);
  A := TFloatAnimation.Create(Self);
  try
    A.Parent := Self;
    A.AnimationType := AType;
    A.Interpolation := AInterpolation;
    A.Duration := Duration;
    A.PropertyName := APropertyName;
    A.StartFromCurrent := True;
    A.StopValue := NewValue;
    A.Start;
    while A.FRunning do
    begin
      Application.ProcessMessages;
      Sleep(0);
    end;
  finally
    A.DisposeOf;
  end;
end;

procedure TFmxObject.AnimateInt(const APropertyName: string; const NewValue: Integer; Duration: Single = 0.2;
  AType: TAnimationType = TAnimationType.atIn;
  AInterpolation: TInterpolationType = TInterpolationType.itLinear);
var
  A: TIntAnimation;
begin
  StopPropertyAnimation(APropertyName);
  A := TIntAnimation.Create(Self);
  A.Parent := Self;
  A.AnimationType := AType;
  A.Interpolation := AInterpolation;
  A.OnFinish := DoAniFinished;
  A.OnProcess := DoAniProcess;
  A.Duration := Duration;
  A.PropertyName := APropertyName;
  A.StartFromCurrent := True;
  A.StopValue := NewValue;
  A.Start;
end;

procedure TFmxObject.AnimateIntWait(const APropertyName: string; const NewValue: Integer; Duration: Single = 0.2;
  AType: TAnimationType = TAnimationType.atIn;
  AInterpolation: TInterpolationType = TInterpolationType.itLinear);
var
  A: TIntAnimation;
begin
  StopPropertyAnimation(APropertyName);
  A := TIntAnimation.Create(Self);
  try
    A.Parent := Self;
    A.AnimationType := AType;
    A.Interpolation := AInterpolation;
    A.Duration := Duration;
    A.PropertyName := APropertyName;
    A.StartFromCurrent := True;
    A.StopValue := NewValue;
    A.Start;
    while A.FRunning do
    begin
      Application.ProcessMessages;
      Sleep(0);
    end;
  finally
    A.DisposeOf;
  end;
end;

procedure TFmxObject.StopPropertyAnimation(const APropertyName: string);
var
  I: Integer;
begin
  for I := (ChildrenCount - 1) downto 0 do
  begin
    if (Children[I] is TCustomPropertyAnimation) and
       (CompareText(TCustomPropertyAnimation(Children[I]).PropertyName, APropertyName) = 0) then
      TFloatAnimation(Children[I]).Stop;
  end;
end;

procedure TFmxObject.DoAniFinished(Sender: TObject);
begin
  TAnimation(Sender).DisposeOf;
end;

procedure TFmxObject.DoAniProcess(Sender: TObject);
begin

end;

{ Animations }

procedure TFmxObject.StartAnimation(const AName: string);
var
  I: Integer;
  E: TAnimation;
begin
  if Assigned(FChildren) then
    for I := 0 to FChildren.Count - 1 do
    begin
      if TFmxObject(FChildren[I]) is TAnimation then
        if CompareText(TAnimation(FChildren[I]).Name, AName) = 0 then
        begin
          E := TAnimation(FChildren[I]);
          E.Start;
        end;
    end;
end;

procedure TFmxObject.StopAnimation(const AName: string);
var
  I: Integer;
  E: TAnimation;
begin
  if Assigned(FChildren) then
    for I := FChildren.Count - 1 downto 0 do
      if TFmxObject(FChildren[I]) is TAnimation then
        if CompareText(TAnimation(FChildren[I]).Name, AName) = 0 then
        begin
          E := TAnimation(FChildren[I]);
          E.Stop;
        end;
end;

procedure TFmxObject.StartTriggerAnimation(const AInstance: TFmxObject; const ATrigger: string);
var
  I: Integer;
begin
  StopTriggerAnimation(AInstance, ATrigger);
  if Assigned(FChildren) then
    for I := 0 to FChildren.Count - 1 do
    begin
      if FChildren[I] is TAnimation then
        TAnimation(FChildren[I]).StartTrigger(AInstance, ATrigger);
      { locked objects }
      if FChildren[I].IsIControl and FChildren[I].AsIControl.Locked and not FChildren[I].AsIControl.HitTest then
        FChildren[I].StartTriggerAnimation(AInstance, ATrigger);
    end;
end;

procedure TFmxObject.StartTriggerAnimationWait(const AInstance: TFmxObject; const ATrigger: string);
var
  I: Integer;
begin
  StopTriggerAnimation(AInstance, ATrigger);
  if Assigned(FChildren) then
    for I := 0 to FChildren.Count - 1 do
    begin
      if FChildren[I] is TAnimation then
      begin
        TAnimation(FChildren[I]).StartTrigger(AInstance, ATrigger);
        while TAnimation(FChildren[I]).Running do
        begin
          Application.ProcessMessages;
          Sleep(0);
        end;
      end;
      { locked objects }
      if FChildren[I].IsIControl and FChildren[I].AsIControl.Locked and not FChildren[I].AsIControl.HitTest then
        FChildren[I].StartTriggerAnimationWait(AInstance, ATrigger);
    end;
end;

procedure TFmxObject.StopTriggerAnimation(const AInstance: TFmxObject; const ATrigger: string);
var
  Item: TFmxObject;
begin
  if Assigned(Children) then
    for Item in Children do
    begin
      if TFmxObject(Item) is TAnimation then
        TAnimation(Item).StopTrigger(AInstance, ATrigger);
      { locked objects }
      if Item.IsIControl and Item.AsIControl.Locked and
        not Item.AsIControl.HitTest then
        Item.StopTriggerAnimation(AInstance, ATrigger);
    end;
end;

function TFmxObject.SupportsPlatformService(const AServiceGUID: TGUID; out AService: IInterface): Boolean;
begin
  {$IFDEF MSWINDOWS}
  if Assigned(Owner) and (csDesigning in Owner.ComponentState) and (Owner is TCommonCustomForm) and
    Assigned(TCommonCustomForm(Owner).Designer) and
    TCommonCustomForm(Owner).Designer.SupportsPlatformService(AServiceGUID, AService)
  then
    Result := True
  else
  {$ENDIF}
  if TPlatformServices.Current.SupportsPlatformService(AServiceGUID, AService) then
    Result := True
  else begin
    AService := nil;
    Result := False;
  end;
end;

procedure TFmxObject.ApplyTriggerEffect(const AInstance: TFmxObject; const ATrigger: string);
var
  Obj: TFmxObject;
begin
  if Assigned(Children) then
    for Obj in Children do
    begin
      if Obj is TEffect then
        TEffect(Obj).ApplyTrigger(AInstance, ATrigger);
      { locked objects }
      if Obj.IsIControl and Obj.AsIControl.Locked and
        not Obj.AsIControl.HitTest then
      begin
        Obj.ApplyTriggerEffect(AInstance, ATrigger);
      end;
    end;
end;

procedure TFmxObject.DoRootChanged;
begin

end;

procedure TFmxObject.DoRootChanging(const NewRoot: IRoot);
begin

end;

procedure TFmxObject.SetRoot(ARoot: IRoot);
var
  I: Integer;
  OldRoot: IRoot;
begin
  if FRoot <> ARoot then
  begin
    DoRootChanging(ARoot);
    OldRoot := FRoot;
    try
      FRoot := ARoot;
      if Assigned(FChildren) and (FChildren.Count > 0) then
        for I := 0 to FChildren.Count - 1 do
          TFmxObject(FChildren[I]).SetRoot(FRoot);
      DoRootChanged;
    except
      FRoot := OldRoot;
      if Assigned(FChildren) and (FChildren.Count > 0) then
        for I := 0 to FChildren.Count - 1 do
          TFmxObject(FChildren[I]).SetRoot(FRoot);
      raise;
    end;
  end;
end;

procedure TFmxObject.ChangeChildren;
begin
  if Assigned(FStyleObjectsDict) then
    FreeAndNil(FStyleObjectsDict);
end;

procedure TFmxObject.ChangeOrder;
begin
  if not(csLoading in ComponentState) and Assigned(Parent) then
    Parent.ChangeChildren;
end;

procedure TFmxObject.ChangeParent;
begin
end;

procedure TFmxObject.SetParent(const Value: TFmxObject);
begin
  if Value = Self then
    Exit;
  if FParent <> Value then
  begin
    if IsChild(Value) then
       raise EInvalidOperation.Create(SCannotCreateCircularDependence);
    if Assigned(FParent) then
      FParent.RemoveObject(Self);
    if Assigned(Value) then
      Value.AddObject(Self)
    else
      FParent := Value;
  end;
end;

function TFmxObject.GetChildrenCount: Integer;
begin
  if Assigned(FChildrenList) then
    Result := FChildrenList.Count
  else
    Result := 0;
end;

procedure TFmxObject.SetParentComponent(Value: TComponent);
var
  R: IRoot;
begin
  inherited;
  if Assigned(FParent) then
    FParent.RemoveObject(Self);

  if Assigned(Value) and (Value is TFmxObject) then
  begin
    TFmxObject(Value).AddObject(Self);
  end
  else if (IInterface(Value).QueryInterface(IRoot, R) = 0) then
  begin
    R.AddObject(Self);
  end
end;

procedure TFmxObject.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I, J: Integer;
  Content: IContent;
begin
  inherited;
  if Supports(Self, IContent) then
    Exit;
  if Assigned(FChildren) then
    for I := 0 to FChildren.Count - 1 do
    begin
      if Supports(FChildren[I], IContent, Content) and (Content.ChildrenCount > 0) then
      begin
        for J := 0 to Content.ChildrenCount - 1 do
          if Content.GetObject.Children[J].Stored then
            Proc(Content.GetObject.Children[J]);
      end;
      if FChildren[I].Stored then
        Proc(FChildren[I]);
    end;
end;

function TFmxObject.GetParentComponent: TComponent;
var
  Content: IContent;
begin
  if Assigned(FParent) and Supports(FParent, IContent, Content) then
    Result := Content.Parent
  else
    Result := FParent;
  if (Not Assigned(Result)) and Assigned(FRoot) then
    Result := FRoot.GetObject;
end;

function TFmxObject.HasParent: Boolean;
begin
  Result := Assigned(FParent);
end;

function TFmxObject.GetData: TValue;
begin
  Result := Name;
end;

procedure TFmxObject.SetData(const Value: TValue);
begin
end;

function TFmxObject.GetActionLinkClass: TActionLinkClass;
begin
  Result := nil;
end;

procedure TFmxObject.IgnoreBindingName(Reader: TReader);
begin
  Reader.ReadString;
end;

procedure TFmxObject.IgnoreBooleanValue(Reader: TReader);
begin
  Reader.ReadBoolean;
end;

procedure TFmxObject.IgnoreIdentValue(Reader: TReader);
begin
  Reader.ReadIdent;
end;

procedure TFmxObject.IgnoreFloatValue(Reader: TReader);
begin
  Reader.ReadFloat;
end;

procedure TFmxObject.InitiateAction;
var
  LAction: TOpenCustomAction;
begin
  if Assigned(FActionLink) then
  begin
    if FActionLink.Action is TCustomAction then
      LAction := TOpenCustomAction(FActionLink.Action)
    else
      LAction := nil;
    if Assigned(LAction) then
      LAction.SetTarget(self);
    try
      if not FActionLink.Update then
        UpdateAction(FActionLink.Action);
    finally
      if Assigned(LAction) then
        LAction.SetTarget(nil);
    end;
  end;
end;

function TFmxObject.GetAction: TBasicAction;
begin
  if Assigned(FActionLink) then
    Result := FActionLink.Action
  else
    Result := nil;
end;

procedure TFmxObject.SetAction(const Value: TBasicAction);
var
  lClass: TActionLinkClass;
begin
  if Not Assigned(Value) then
  begin
    if Assigned(Application) then
      Application.UnregisterActionClient(Self);
    FreeAndNil(FActionLink);
    SetActionClient(False);
  end
  else
  begin
    if Not Assigned(FActionLink) then
    begin
      lClass := GetActionLinkClass;
      if Not Assigned(lClass) then
        raise EActionError.CreateFMT(StrEActionNoSuported, [ClassName]);
      FActionLink := lClass.Create(Self);
    end;
    ActionLink.Action := Value;
    ActionLink.OnChange := DoActionChange;
    ActionChange(Value, csLoading in Value.ComponentState);
    Value.FreeNotification(Self);
    SetActionClient(True);
    if Assigned(Application) then
      Application.RegisterActionClient(Self);
  end;
end;

procedure TFmxObject.DoActionChange(Sender: TObject);
begin
  if Sender = Action then ActionChange(TBasicAction(Sender), False);
end;

procedure TFmxObject.ActionChange(Sender: TBasicAction; CheckDefaults: Boolean);
begin
end;

procedure TFmxObject.DoActionClientChanged;
begin
end;

procedure TFmxObject.SetDesign(Value, SetChildren: Boolean);
var
  I: Integer;
begin
  SetDesigning(Value, SetChildren);
  if SetChildren then
    for I := 0 to ChildrenCount - 1 do
      FChildren[I].SetDesign(Value);
end;

function TFmxObject.GetBackIndex: Integer;
begin
  Result := 0;
end;

{ }

function TFmxObject.SearchInto: Boolean;
begin
  Result := True;
end;

procedure TFmxObject.DoInventory(Dict: TDictionary<string, TFmxObject>;
  const Prefix: string);
var
  O: TFmxObject;
  IsRoot: Boolean;
  Key: string;
begin
  IsRoot := False;
  if Dict = nil then
  begin
    if FStyleObjectsDict = nil then
    begin
      FStyleObjectsDict := TDictionary<string, TFmxObject>.Create;
      Dict := FStyleObjectsDict;
      IsRoot := True;
    end
    else
      Exit;
  end;

  if Assigned(FChildren) then
    for O in FChildren do
    begin
      if O.StyleName <> '' then
      begin
        Key := O.StyleName.ToLowerInvariant;
        Dict.AddOrSetValue(Key, O);
        if not IsRoot then
        begin
          if Prefix <> '' then
            Dict.AddOrSetValue(Format('%s.%s',[Prefix, Key]), O);
        end;
      end;
      if O.SearchInto then
        O.DoInventory(Dict, O.StyleName);
    end;
end;

function TFmxObject.FindStyleResource(const AStyleLookup: string; const Clone: Boolean = False): TFmxObject;
{$IFDEF NOSTYLERESOURCECACHE}
var
  I, J: Integer;
  LName: string;
{$ENDIF}

  function FindOnTopLevel(const Name: string): TFmxObject;
  var
    Child: TFmxObject;
  begin
    for Child in FChildren do
    begin
      if SameText(Child.StyleName, Name) then
      begin
        Exit(Child);
      end;
    end;
    Result := nil;
  end;

  function FindOnAllLevel(const Name: string): TFmxObject;
  var
    Child: TFmxObject;
  begin
    Result := FindOnTopLevel(Name);
    if not Assigned(Result) then
      for Child in FChildren do
      begin
        if Child.SearchInto then
        begin
          Result := Child.FindStyleResource(Name);
          if Assigned(Result) then
            Exit;
        end;
      end;
  end;

var
  SaveStyleName: string;
  StyleObject: TFmxObject;
begin
  StyleObject := nil;
  if (AStyleLookup <> '') and
     Assigned(FChildren) and
     (FChildren.Count > 0) then
  begin
{$IFDEF NOSTYLERESOURCECACHE}
    J := AStyleLookup.IndexOf('.');
    if J >= 0 then
    begin
      // Match substring to the first point of top-level elements
      LName := AStyleLookup.Substring(0, J);
      StyleObject := FindOnTopLevel(LName);
      // Search the rest of string in the element found
      if Assigned(StyleObject) then
      begin
        LName := AStyleLookup.Substring(J + 1);
        StyleObject := StyleObject.FindStyleResource(LName);
      end;
    end;
    // Match substring after the last dot (old algorithm)
    if (not Assigned(StyleObject)) then
    begin
      if J >= 0 then
      repeat
        I := AStyleLookup.IndexOf('.', J + 1);
        if I >= 0 then J := I;
      until I < 0;
      if J >= 0 then
        StyleObject := FindOnAllLevel(AStyleLookup.Substring(J + 1))
      else
        StyleObject := FindOnAllLevel(AStyleLookup);
    end;
{$ELSE}
    DoInventory(nil, '');
    FStyleObjectsDict.TryGetValue(AStyleLookup.ToLowerInvariant, StyleObject);
{$ENDIF}
  end;
  if Assigned(StyleObject) and Clone then
  begin
    SaveStyleName := StyleObject.StyleName;
    try
      StyleObject.FStyleName := '';
      Result := StyleObject.Clone(nil);
    finally
      StyleObject.FStyleName := SaveStyleName;
    end;
  end else
    Result := StyleObject;
end;

procedure TFmxObject.SetTouchManager(const Value: TTouchManager);
begin
  FTouchManager := Value;
end;

procedure TFmxObject.SetStored(const Value: Boolean);
var
  I: Integer;
begin
  if FStored <> Value then
  begin
    FStored := Value;
    if Assigned(FChildren) and (FChildren.Count > 0) then
    begin
      for I := 0 to FChildren.Count - 1 do
        TFmxObject(FChildren[I]).Stored := Value;
    end;
    if not Stored then
      RemoveFromResourcePool;
  end;
end;

procedure TFmxObject.DoDeleteChildren;
var
  I: Integer;
  Child: TFmxObject;
begin
  if Assigned(FChildren) then
  begin
    for I := FChildren.Count - 1 downto 0 do
    begin
      Child := TFmxObject(FChildren[I]);
      FChildren.Delete(I);
      RemoveFromTabList(Child);
      Child.FParent := nil;
      Child.SetRoot(nil);
      Child.DisposeOf;
    end;
    FreeAndNil(FChildren);
    if Assigned(FChildrenList) then
      FreeAndNil(FChildrenList);
  end;
  FTabList := nil;
end;

procedure TFmxObject.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('BindingName', IgnoreBindingName, nil, False);
end;

procedure TFmxObject.DeleteChildren;
begin
  DoDeleteChildren;
end;

procedure TFmxObject.AddObjectsToList(const AList: TFmxObjectList);
var
  I: Integer;
begin
  AList.Add(Self);
  if Assigned(FChildren) then
    for I := 0 to FChildren.Count - 1 do
      FChildren[I].AddObjectsToList(AList);
end;

procedure TFmxObject.AddToTabList(const AObject: TFmxObject);
begin
  if Assigned(AObject) and AObject.IsIControl then
  begin
    if not Assigned(FTabList) then
      FTabList := TInterfaceList.Create;

    FTabList.Add(AObject.AsIControl);
  end;
end;

procedure TFmxObject.RemoveFromTabList(const AObject: TFmxObject);
begin
  if Assigned(FTabList) and Assigned(AObject) and AObject.IsIControl then
    FTabList.Remove(AObject.AsIControl);
end;

function TFmxObject.CreateChildrenList(const Children: TFmxObjectList): TFmxChildrenList;
begin
  Result := TFmxChildrenList.Create(FChildren);
end;

procedure TFmxObject.DoAddObject(const AObject: TFmxObject);
begin
  if not Assigned(FChildren) then
  begin
    FChildren := TFmxObjectList.Create;
    FChildren.Capacity := 10;
    FChildrenList := CreateChildrenList(FChildren);
//    FChildrenList.SetChildrenList(FChildren);
  end;
  //if FChildren.IndexOf(AObject) < 0 then
  //begin
    AObject.FParent := Self;
    AObject.SetRoot(FRoot);
    AObject.ChangeParent;
    FChildren.Add(AObject);
    if csDesigning in ComponentState then
      AObject.SetDesign(True, True);
    AddToTabList(AObject);
  //end;
end;

procedure TFmxObject.DoRemoveObject(const AObject: TFmxObject);
var
  Idx, I: Integer;
begin
  RemoveFromTabList(AObject);
  if Assigned(FChildren) then
  begin
    Idx := FChildren.IndexOf(AObject);
    if Idx >= 0 then
    begin
      AObject.FParent := nil;
      AObject.SetRoot(nil);
      AObject.ChangeParent;
      if AObject.FIndex >= 0 then
        for I := AObject.FIndex to FChildren.Count - 1 do
          TFmxObject(FChildren[I]).FIndex := -1;
      FChildren.Delete(Idx);
    end;
  end;
end;

procedure TFmxObject.AddObject(const AObject: TFmxObject);
begin
  if Assigned(AObject) and (AObject.Parent <> Self) then
  begin
    if Assigned(AObject.Parent) then
      AObject.Parent := nil;
    DoAddObject(AObject);
  end;
end;

procedure TFmxObject.InsertObject(Index: Integer; const AObject: TFmxObject);
begin
  if Assigned(AObject) and (AObject.Parent <> Self) then
  begin
    if Assigned(AObject.Parent) then
      AObject.Parent := nil;
    DoInsertObject(Index, AObject);
  end;
end;

procedure TFmxObject.AddToResourcePool;
begin
  if Stored then
    FMX.Types.AddResource(Self);
end;

procedure TFmxObject.RemoveFromResourcePool;
begin
  FMX.Types.RemoveResource(Self);
end;

procedure TFmxObject.SetStyleName(const Value: string);
begin
  if FStyleName <> Value then
  begin
    RemoveFromResourcePool;

    FStyleName := Value;
    if (FStyleName <> '') then
      AddToResourcePool;
  end;
end;

procedure TFmxObject.DoInsertObject(Index: Integer; const AObject: TFmxObject);
var
  I: Integer;
begin
  DoAddObject(AObject);
  FChildren.Insert(Index, FChildren[FChildren.Count - 1]);
  FChildren.Delete(FChildren.Count-1);
  ChangeChildren;
  // update index
  for I := Index to FChildren.Count - 1 do
    FChildren[I].FIndex := -1;
end;

procedure TFmxObject.RemoveObject(const AObject: TFmxObject);
begin
  if Assigned(AObject) then
    DoRemoveObject(AObject);
end;

procedure TFmxObject.Sort(Compare: TFmxObjectSortCompare);
var
  Comparer: IComparer<TFmxObject>;
  Comparsion: TComparison<TFmxObject>;
  I: Integer;
begin
  if Assigned(FChildren) then
  begin
    Comparsion := TComparison<TFmxObject>(Compare);
    Comparer := TComparer<TFmxObject>.Construct(Comparsion);
    FChildren.Sort(Comparer);
    ChangeChildren;
    // update index
    for  I := 0 to FChildren.Count - 1 do
      FChildren[I].FIndex := -1;
  end;
end;

function TFmxObject.GetIndex: Integer;
begin
  if (FIndex < 0) and Assigned(FParent) and Assigned(FParent.Children) then
    FIndex := FParent.Children.IndexOf(Self);
  Result := FIndex;
end;

procedure TFmxObject.SetIndex(Idx: Integer);
var
  I: Integer;
begin
  if Assigned(Parent) and (Parent.FChildren.IndexOf(Self) >= 0) then
  begin
    Parent.FChildren.Remove(Self);
    if (Idx < 0) then
      Idx := 0;
    if (Idx > 0) and (Idx > Parent.FChildren.Count) then
      Idx := Parent.FChildren.Count;
    Parent.FChildren.Insert(Idx, Self);
    // recalc Index
    for I := 0 to Parent.FChildren.Count - 1 do
      TFmxObject(Parent.FChildren[I]).FIndex := -1;
    ChangeOrder;
  end;
end;

procedure TFmxObject.Exchange(const AObject1, AObject2: TFmxObject);
var
  Idx: Integer;
begin
  if Assigned(FChildren) and (AObject1.Parent = Self) and (AObject2.Parent = Self) then
  begin
    FChildren.Exchange(AObject1.Index, AObject2.Index);
    Idx := AObject1.FIndex;
    AObject1.FIndex := AObject2.Index;
    AObject1.ChangeOrder;
    AObject2.FIndex := Idx;
    AObject2.ChangeOrder;
  end;
end;

procedure TFmxObject.GetTabOrderList(const List: TInterfaceList; AChildren: Boolean);
var
  I: Integer;
  Control: IControl;
begin
  if Assigned(FTabList) then
    for I := 0 to FTabList.Count - 1 do
    begin
      Control := IControl(FTabList[I]);
      List.Add(Control);
      if AChildren and (Control.GetObject is TFmxObject) then
        TFmxObject(Control).GetTabOrderList(List, AChildren);
    end;
end;

function TFmxObject.GetTouchManager: TTouchManager;
begin
  if not Assigned(FTouchManager) then
    FTouchManager := TTouchManager.Create(Self);

  Result := FTouchManager;
end;

procedure TFmxObject.FixupTabList;
var
  I, J: Integer;
  List: TInterfaceList;
  Control: IControl;
begin
  if not Assigned(FTabList) then
    Exit;
  List := TInterfaceList.Create;
  try
    List.Count := FTabList.Count;
    for I := 0 to FTabList.Count - 1 do
    begin
      Control := IControl(FTabList[I]);
      J := Control.GetTabOrderValue;       // = loaded FTabOrder value
      if (J >= 0) and (J < FTabList.Count) then
        List[J] := Control;
    end;
    for I := 0 to FTabList.Count - 1 do
    begin
      Control := IControl(List[I]);
      if Assigned(Control) then
        Control.UpdateTabOrder(I);
    end;
  finally
    List.Free;
  end;
end;

procedure TFmxObject.BringToFront;
var
  I: Integer;
begin
  if Assigned(Parent) and Assigned(Parent.FChildren) then
  begin
    Parent.FChildren.Remove(Self);
    Parent.FChildren.Add(Self);
    // recalc Index
    for I := 0 to Parent.FChildren.Count - 1 do
      TFmxObject(Parent.FChildren[I]).FIndex := -1;
    ChangeOrder;
  end;
end;

procedure TFmxObject.SendToBack;
var
  I: Integer;
begin
  if Assigned(Parent) and Assigned(Parent.FChildren) then
  begin
    Parent.FChildren.Remove(Self);
    Parent.FChildren.Insert(Parent.GetBackIndex, Self);
    // recalc Index
    for I := 0 to Parent.FChildren.Count - 1 do
      TFmxObject(Parent.FChildren[I]).FIndex := -1;
    ChangeOrder;
  end;
end;

{ TLang }

function ReadString(S: TStream): string;
var
  L: Integer;
begin
  L := 0;
  S.Read(L, SizeOf(L));
  SetLength(Result, L);
  S.Read(Pointer(Result)^, L * 2);
end;

procedure WriteString(S: TStream; const Value: string);
var
  L: Integer;
begin
  L := Length(Value);
  S.Write(L, SizeOf(L));
  S.Write(Pointer(Value)^, L * 2);
end;

constructor TLang.Create(AOwner: TComponent);
begin
  inherited;
  FOriginal := TStringList.Create;
  FResources := TStringList.Create;
  FAutoSelect := True;
  FStoreInForm := True;
end;

destructor TLang.Destroy;
var
  I: Integer;
begin
  for I := 0 to FResources.Count - 1 do
    TStrings(FResources.Objects[I]).DisposeOf;
  FreeAndNil(FResources);
  FreeAndNil(FOriginal);
  inherited;
end;

procedure TLang.Loaded;
var
  LocaleSvc: IFMXLocaleService;
begin
  inherited;
  if not FFileName.IsEmpty then
    if FileExists(FFileName) then
      LoadFromFile(FFileName);
  if FAutoSelect and TPlatformServices.Current.SupportsPlatformService(IFMXLocaleService, IInterface(LocaleSvc)) then
    FLang := LocaleSvc.GetCurrentLangID;
  if FLang <> '' then
    LoadLangFromStrings(LangStr[FLang]);
end;

procedure TLang.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('ResourcesBin', ReadResources, WriteResources, StoreInForm and (FResources.Count > 0));
end;

procedure TLang.ReadResources(Stream: TStream);
var
  len: Cardinal;
  I: Integer;
  N: string;
  Str: TStrings;
begin
  FOriginal.Text := ReadString(Stream);
  Stream.Read(len, 4);
  for I := 0 to len - 1 do
  begin
    N := ReadString(Stream);
    Str := TStringList.Create;
//    TStringList(Str).Sorted := True;
    TStringList(Str).CaseSensitive := True;
    Str.Text := ReadString(Stream);
    FResources.AddObject(N, Str);
  end;
end;

procedure TLang.WriteResources(Stream: TStream);
var
  len: Cardinal;
  I: Integer;
begin
  WriteString(Stream, FOriginal.Text);
  len := FResources.Count;
  Stream.Write(len, 4);
  for I := 0 to len - 1 do
  begin
    WriteString(Stream, FResources[I]);
    WriteString(Stream, TStrings(FResources.Objects[I]).Text);
  end;
end;

procedure TLang.LoadFromFile(const AFileName: string);
var
  S: TFileStream;
begin
  if FileExists(AFileName) then
  begin
    S := TFileStream.Create(AFileName, fmOpenRead);
    try
      ReadResources(S);
    finally
      S.Free;
    end;
  end;
end;

procedure TLang.SaveToFile(const AFileName: string);
var
  S: TFileStream;
begin
  S := TFileStream.Create(AFileName, fmCreate);
  try
    WriteResources(S);
  finally
    S.Free;
  end;
end;

procedure TLang.AddLang(const AName: string);
var
  Idx: Integer;
  Str: TStrings;
begin
  Idx := FResources.IndexOf(AName);
  if Idx < 0 then
  begin
    Str := TStringList.Create;
//    TStringList(Str).Sorted := True;
    TStringList(Str).CaseSensitive := True;
    FResources.AddObject(AName, Str);
  end;
end;

function TLang.GetLangStr(const Index: string): TStrings;
var
  Idx: Integer;
begin
  Idx := FResources.IndexOf(Index);
  if Idx >= 0 then
    Result := TStrings(FResources.Objects[Idx])
  else
    Result := nil;
end;

procedure TLang.SetLang(const Value: string);
begin
  FLang := Value;
  if not(csLoading in ComponentState) then
  begin
    if FLang = 'en' then
      ResetLang
    else
      LoadLangFromStrings(LangStr[FLang]);
  end;
end;

{ TTimer }

constructor TTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInterval := 1000;
  FTimerHandle := cIdNoTimer;
  FEnabled := True;
  if not TPlatformServices.Current.SupportsPlatformService(IFMXTimerService, IInterface(FPlatformTimer)) then
    raise EUnsupportedPlatformService.Create('IFMXTimerService');
end;

destructor TTimer.Destroy;
begin
  FOnTimer := nil;
  FEnabled := False;
  KillTimer;
  inherited Destroy;
end;

procedure TTimer.KillTimer;
begin
  if FTimerHandle <> cIdNoTimer then
  begin
    FPlatformTimer.DestroyTimer(FTimerHandle);
    FTimerHandle := cIdNoTimer;
  end;
end;

procedure TTimer.Loaded;
begin
  inherited Loaded;
  UpdateTimer;
end;

procedure TTimer.UpdateTimer;
begin
  KillTimer;
  if (FEnabled) and (FInterval > 0) and (([csDesigning, csLoading, csDestroying] * ComponentState = [])) and
    Assigned(FOnTimer) then
  begin
    FTimerHandle := FPlatformTimer.CreateTimer(FInterval, Timer);
    if FTimerHandle = 0 then
      FEnabled := False;
  end;
end;

procedure TTimer.Timer;
begin
  if (FEnabled) and (FInterval > 0) then
    DoOnTimer;
end;

procedure TTimer.SetOnTimer(Value: TNotifyEvent);
begin
  if TMethod(Value) = TMethod(FOnTimer) then
    Exit;
  FOnTimer := Value;
  UpdateTimer;
end;

procedure TTimer.DoOnTimer;
begin
  if Assigned(FOnTimer) then
    FOnTimer(Self);
end;

procedure TTimer.SetEnabled(Value: Boolean);
begin
  if (Value <> FEnabled) then
  begin
    FEnabled := Value;
    UpdateTimer;
  end;
end;

procedure TTimer.SetInterval(Value: Cardinal);
begin
  if (Value <> FInterval) then
  begin
    FInterval := Value;
    UpdateTimer;
  end;
end;

{ TLineMetricInfo }

constructor TLineMetricInfo.Create;
begin
  inherited;
  Clear;
end;

destructor TLineMetricInfo.Destroy;
begin
  Clear;
  inherited;
end;

function TLineMetricInfo.GetCount: integer;
begin
  Result := Length(FLineMetrics);
end;

function TLineMetricInfo.GetMetrics(Index: Integer): PLineMetric;
begin
  Result := @FLineMetrics[Index];
end;

procedure TLineMetricInfo.SetCount(const Value: Integer);
begin
  if Count <> Value then
    SetLength(FLineMetrics, Value);
end;

procedure TLineMetricInfo.Clear;
begin
  SetLength(FLineMetrics, 0);
end;

procedure RecalcAnchorRules(const Parent : TFmxObject;
            Anchors : TAnchors;
            const BoundsRect : TRectF;
            const Padding : TBounds;
            var AOriginalParentSize:TPointF;
            var AAnchorOrigin:TPointF;
            var AAnchorRules:TPointF
            );
var
  Rect : TRectF;
begin
  Rect := BoundsRect;
  Rect.Left := Rect.Left - Padding.Left;
  Rect.Top := Rect.Top - Padding.Top;
  Rect.Right := Rect.Right + Padding.Right;
  Rect.Bottom := Rect.Bottom + Padding.Bottom;

  AAnchorOrigin := PointF(Rect.Left + Rect.Width / 2, Rect.Top + Rect.Height / 2);
  if Anchors = [TAnchorKind.akLeft, TAnchorKind.akTop] then
  begin
    AOriginalParentSize.X := 0;
    AOriginalParentSize.Y := 0;
    Exit;
  end;
  if TAnchorKind.akRight in Anchors then
    if TAnchorKind.akLeft in Anchors then
      AAnchorRules.X := Rect.Width else
      AAnchorRules.X := Rect.Left
  else
    AAnchorRules.X := Rect.Left + Rect.Width/2;
  if TAnchorKind.akBottom in Anchors then
    if TAnchorKind.akTop in Anchors then
      AAnchorRules.Y := Rect.Height else
      AAnchorRules.Y := Rect.Top
  else
    AAnchorRules.Y := Rect.Top + Rect.Height/2;
end;

procedure RecalcControlOriginalParentSize(
              const Parent: TFmxObject;
              ComponentState : TComponentState;
              var AOriginalParentSize : TPointF);
var
  Alignable : IAlignableObject;
  Container : IContainerObject;
  OriginalSize: IOriginalContainerSize;
  R : TRectF;
  Margins : TRectF;
begin
  if Supports(Parent, IAlignableObject, Alignable) then
  begin
    R := RectF(0, 0, Alignable.Width, Alignable.Height);
    R.Offset(Alignable.Left, Alignable.Top);
    Margins := Alignable.Margins.GetRect;
  end
  else if Supports(Parent, IOriginalContainerSize, OriginalSize) then
  begin
    R.TopLeft := TPointF.Create(0,0);
    R.BottomRight := OriginalSize.OriginalContainerSize;
    Margins := RectF(0,0,0,0);
  end
  else if Supports(Parent, IContainerObject, Container) then
  begin
    R := RectF(0, 0, Container.ContainerWidth, Container.ContainerHeight);
    Margins := RectF(0,0,0,0);
  end
  else
    Exit;

  if csReading in ComponentState then
  begin
    if not (csDesigning in ComponentState) then
    begin
        AOriginalParentSize.X := R.Left;
        AOriginalParentSize.Y := R.Top;
    end
  end
  else
  begin
    AOriginalParentSize.X := R.Width;
    AOriginalParentSize.Y := R.Height;
  end;

  AOriginalParentSize.X:= AOriginalParentSize.X - (Margins.Left + Margins.Right);
  AOriginalParentSize.Y:= AOriginalParentSize.Y - (Margins.Top + Margins.Bottom);
end;

{ TCustomPopupMenu }

procedure TCustomPopupMenu.DoPopup;
begin
  if Assigned(OnPopup) then
    OnPopup(Self);
end;



{ TCustomTouchManager.TObjectWrapper }

constructor TCustomTouchManager.TObjectWrapper.Create(const AObject: TFmxObject);
begin
  FObject := AObject;
end;

{ TCustomTouchManager }

procedure TCustomTouchManager.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomTouchManager then
    with TCustomTouchManager(Dest) do
    begin
      FControl := Self.FControl;
      FGestureManager := Self.FGestureManager;
      FGestureEngine := Self.FGestureEngine;
      FInteractiveGestures := Self.FInteractiveGestures;
      FStandardGestures := Self.FStandardGestures;
    end
  else
    inherited;
end;

procedure TCustomTouchManager.ChangeNotification(const AControl: TFmxObject);
begin
end;

constructor TCustomTouchManager.Create(AControl: TFmxObject);
begin
  inherited Create;
  FControl := AControl;
  FGestureEngine := nil;
  FGestureManager := nil;
  FDefaultInteractiveGestures := [];
end;

destructor TCustomTouchManager.Destroy;
begin
  if Assigned(GestureEngine) then
  begin
    GestureEngine.DisposeOf;
    GestureEngine := nil;
  end;

  if Assigned(FGestureManager) then
    SetGestureManager(nil);

  inherited;
end;

function TCustomTouchManager.FindGesture(const AName: string): TCustomGestureCollectionItem;
begin
  Result := nil;
  if Assigned(FGestureManager) then
    Result := FGestureManager.FindGesture(FControl, AName);
end;

function TCustomTouchManager.FindGesture(AGestureID: TGestureID): TCustomGestureCollectionItem;
begin
  Result := nil;
  if Assigned(FGestureManager) then
    Result := FGestureManager.FindGesture(FControl, AGestureID);
end;

function TCustomTouchManager.GetGestureList: TGestureArray;
begin
  Result := nil;
  if Assigned(FGestureManager) then
    Result := FGestureManager.GestureList[FControl];
end;

function TCustomTouchManager.GetStandardGestures: TStandardGestures;
begin
  if Assigned(FGestureManager) then
    FStandardGestures := FGestureManager.StandardGestures[FControl];
  Result := FStandardGestures;
end;

function TCustomTouchManager.IsDefault: Boolean;
begin
  Result := (Not Assigned(FGestureManager)) and not IsInteractiveGesturesStored;
end;

function TCustomTouchManager.IsInteractiveGesturesStored: Boolean;
begin
  Result := InteractiveGestures <> FDefaultInteractiveGestures;
end;

procedure TCustomTouchManager.RemoveChangeNotification(const AControl: TFmxObject);
begin
end;

function TCustomTouchManager.SelectGesture(const AName: string): Boolean;
begin
  Result := False;
  if Assigned(FGestureManager) then
    Result := FGestureManager.SelectGesture(FControl, AName);
end;

function TCustomTouchManager.SelectGesture(AGestureID: TGestureID): Boolean;
begin
  Result := False;
  if Assigned(FGestureManager) then
    Result := FGestureManager.SelectGesture(FControl, AGestureID);
end;

procedure TCustomTouchManager.SetGestureEngine(const Value: TCustomGestureEngine);
begin
  if (Value <> FGestureEngine) and (FControl is TFmxObject) then
  begin
    FGestureEngine := Value;
    if (Assigned(FControl) and Assigned(FGestureEngine)) then
      FGestureEngine.Active := True;
  end;
end;

procedure TCustomTouchManager.SetGestureManager(const Value: TCustomGestureManager);
begin
  if Value <> FGestureManager then
  begin
    if Assigned(FGestureManager) then
    begin
      FGestureManager.RemoveFreeNotification(FControl);
      FGestureManager.UnregisterControl(FControl);
      FGestureManager := nil; // Must be set to nil before calling RegisterControl!!
    end;
    if Assigned(Value) then
    begin
      Value.FreeNotification(FControl);
      Value.RegisterControl(FControl);
    end;
    FGestureManager := Value; // Must be assigned after registering the control!!
  end;
end;

procedure TCustomTouchManager.SetInteractiveGestures(const Value: TInteractiveGestures);
{$IFNDEF MSWINDOWS}
var
  Difference: TInteractiveGestures;
  Gesture: TInteractiveGesture;
  OwnerForm: TComponent;
{$ENDIF}
begin
  {$IFNDEF MSWINDOWS}
  OwnerForm := Self.FControl;
  while (Assigned(OwnerForm) and not (OwnerForm is TCommonCustomForm)) do
    OwnerForm := OwnerForm.Owner;

  if Assigned(OwnerForm) then
  begin
    Difference := [];
    Difference := Value - FInteractiveGestures;
    for Gesture in Difference do
      TCommonCustomForm(OwnerForm).AddRecognizer(Gesture);

    Difference := FInteractiveGestures - Value;
    for Gesture in Difference do
      TCommonCustomForm(OwnerForm).RemoveRecognizer(Gesture);
  end;
{$ENDIF}

  if Value <> FInteractiveGestures then
  begin
    FInteractiveGestures := Value;
  end;
end;

procedure TCustomTouchManager.SetStandardGestures(const Value: TStandardGestures);
begin
   if Value <> FStandardGestures then
  begin
    FStandardGestures := Value;
    if Assigned(FGestureManager) then
      FGestureManager.StandardGestures[FControl] := FStandardGestures;
  end;
end;

procedure TCustomTouchManager.UnselectGesture(AGestureID: TGestureID);
begin
  if Assigned(FGestureManager) then
    FGestureManager.UnselectGesture(FControl, AGestureID);
end;

{ TCustomGestureEngine }

class function TCustomGestureEngine.Supported: Boolean;
begin
  Result := False;
end;

{ TCustomGestureCollection }

function TCustomGestureCollection.GetItem(Index: Integer): TCustomGestureCollectionItem;
begin
  Result := TCustomGestureCollectionItem(inherited GetItem(Index));
end;

procedure TCustomGestureCollection.SetItem(Index: Integer; const Value: TCustomGestureCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TCustomGestureManager }

procedure TCustomGestureManager.RemoveActionNotification(Action: TCustomAction;
  Item: TCustomGestureCollectionItem);
begin
  inherited;
end;

{$REGION ' implementation of Caret'}
var
  vKBTimer: TTimer = nil;
  vActiveCaret: TCustomCaret = nil;
  vOldDisplayed: boolean = False;
  vShowVKProc: TShowVirtualKeyboard = nil;

type
  TKBTimer = class(TTimer)
  public
    constructor Create(AOwner: TComponent); override;
    procedure OnTimerProc(sender: TObject);
  end;

procedure RegisterShowVKProc(const ShowVirtualKeyboard: TShowVirtualKeyboard);
var
  VirtualKeyBoardState: TVirtualKeyBoardState;
begin
  if @vShowVKProc <> @ShowVirtualKeyboard then
  begin
    try
      VirtualKeyBoardState := [];
      try
        if Assigned(vShowVKProc) and
           vOldDisplayed and
           Assigned(vKBTimer) then
          vShowVKProc(False, vActiveCaret, VirtualKeyBoardState);
      finally
        vShowVKProc := nil;
      end;
      vShowVKProc := ShowVirtualKeyboard;
      if Assigned(vShowVKProc) and
         vOldDisplayed and
         Assigned(vKBTimer) then
        vShowVKProc(True, vActiveCaret, VirtualKeyBoardState);
    except
      vOldDisplayed := False;
      Raise;
    end;
  end;
end;


{ TKBTimer }

constructor TKBTimer.Create(AOwner: TComponent);
begin
  inherited;
  Enabled := False;
  Interval := 25;
  OnTimer := OnTimerProc;
end;

procedure TKBTimer.OnTimerProc(sender: TObject);
var
  NewDisplayed: boolean;
  VirtualKeyboardState: TVirtualKeyBoardState;
begin
  Enabled := False;
  NewDisplayed := Assigned(vActiveCaret) and (vActiveCaret.Displayed);
  try
    if (Assigned(vShowVKProc)) then
    begin
      VirtualKeyBoardState := [];
      if NewDisplayed then
      begin
        VirtualKeyboardState := [vksTransient];
        vShowVKProc(not NewDisplayed, vActiveCaret, VirtualKeyboardState);
      end;
      vShowVKProc(NewDisplayed, vActiveCaret, VirtualKeyboardState);
    end;
  finally
    VirtualKeyboardState := VirtualKeyboardState - [vksTransient];
    vOldDisplayed := NewDisplayed and (vksVisible in VirtualKeyBoardState);
  end;
end;



{$REGION 'global methods for Flasher'}
type
  TRegisteredFlasher = record
    FlasherClass: TFmxObjectClass;
    Name: string;
    Flasher: TFmxObject;
  end;

var
  vFlashers: TList<TRegisteredFlasher> = nil;

  procedure CheckNameFlasher(const CaretClass: TCaretClass; var Name: string);
  begin
    if Assigned(CaretClass) then
      Name := CaretClass.FlasherName
    else
    begin
      Name := '';
      raise EArgumentNilException.Create(SArgumentNil)
    end;
    Name := Trim(Name);
    if Name = '' then
      raise EArgumentException.Create(sArgumentInvalid);
  end;

  function IndexRegisterFlasher(const CaretClass: TCaretClass): integer;
  var
    I: Integer;
    Name: string;
  begin
    Result := -1;
    if Assigned(CaretClass) then
    begin
      Name := Trim(CaretClass.FlasherName);
      if (Name <> '') and (Assigned(vFlashers)) then
      for I := 0 to vFlashers.Count - 1 do
        if SameText(Name, vFlashers[I].Name) then
        begin
          Result := I;
          Break;
        end;
    end;
  end;

  procedure RegisterFlasherClass(const FlasherClass: TFmxObjectClass; const CaretClass: TCaretClass);
  var
    P: PInterfaceEntry;
    Index: integer;
    RegisteredFlasher: TRegisteredFlasher;
    Name: string;
  begin
    CheckNameFlasher(CaretClass, Name);
    if Assigned(FlasherClass) then
    begin
      P := FlasherClass.GetInterfaceEntry(IFlasher);
      if not Assigned(P) then
        raise EArgumentNilException.CreateFMT(SUnsupportedInterface, [FlasherClass.ClassName, 'IFlasher']);
    end;
    if (not Assigned(vFlashers)) and (not Assigned(FlasherClass)) then
      Exit;

    if not Assigned(vFlashers) then
      vFlashers := TList<TRegisteredFlasher>.Create;
    Index := IndexRegisterFlasher(CaretClass);
    if Index = -1 then
    begin
      RegisteredFlasher.Name := Name;
      RegisteredFlasher.FlasherClass := FlasherClass;
      RegisteredFlasher.Flasher := nil;
      vFlashers.Add(RegisteredFlasher);
    end
    else
    begin
      RegisteredFlasher := vFlashers[Index];
      if RegisteredFlasher.FlasherClass <> FlasherClass then
      begin
        FreeAndNil(RegisteredFlasher.Flasher);
        RegisteredFlasher.Name := Name;
        RegisteredFlasher.FlasherClass := FlasherClass;
        if Assigned(RegisteredFlasher.FlasherClass) then
          vFlashers[Index] := RegisteredFlasher
        else
          vFlashers.Delete(Index);
      end;
    end;
  end;

  function FlasherClass(const CaretClass: TCaretClass): TFmxObjectClass;
  var
    Index: Integer;
  begin
    Result := nil;
    Index := IndexRegisterFlasher(CaretClass);
    if Index >= 0 then
      Result := vFlashers[Index].FlasherClass;
  end;

  function AssignedFlasher(const CaretClass: TCaretClass): boolean;
  var
    Index: Integer;
  begin
    Result := False;
    Index := IndexRegisterFlasher(CaretClass);
    if Index >= 0 then
      Result := Assigned(vFlashers[Index].Flasher);
  end;

  function Flasher(const CaretClass: TCaretClass): TFmxObject;
  var
    RegisteredFlasher: TRegisteredFlasher;
    Index: Integer;
    Name: string;
  begin
    CheckNameFlasher(CaretClass, Name);
    Index := IndexRegisterFlasher(CaretClass);
    if Index >= 0 then
    begin
      RegisteredFlasher := vFlashers[Index];
      if not Assigned(RegisteredFlasher.FlasherClass) then
        raise EClassNotFound.Create(SFlasherNotRegistered);
      if not Assigned(RegisteredFlasher.Flasher) then
      begin
        RegisteredFlasher.Flasher := RegisteredFlasher.FlasherClass.Create(nil);
        RegisteredFlasher.Flasher.Name := Name;
        vFlashers[Index] := RegisteredFlasher;
      end;
      Result := vFlashers[Index].Flasher;
    end
    else
      raise EClassNotFound.Create(SFlasherNotRegistered);
  end;

{$ENDREGION}

{ TCustomCaret }

constructor TCustomCaret.Create(const AOwner: TFMXObject);
begin
  if not Assigned(AOwner) then
    raise EArgumentNilException.Create(SArgumentNil);
  inherited Create;
  FOwner := AOwner;
  if FOwner.QueryInterface(IControl, FIControl) <> S_OK then
    raise EArgumentNilException.CreateFMT(SUnsupportedInterface, [FOwner.ClassName, 'IControl']);
end;

destructor TCustomCaret.Destroy;
var
  LClass: TCaretClass;
begin
  LClass := TCaretClass(ClassType);
  if AssignedFlasher(LClass) then
  begin
    if (FMX.Types.Flasher(LClass) as IFlasher).Caret = self then
    begin
      (FMX.Types.Flasher(LClass) as IFlasher).Caret := nil;
      (FMX.Types.Flasher(LClass) as IFlasher).UpdateState;
    end;
  end;
  if vActiveCaret = self then
  begin
    vActiveCaret := nil;
    if Assigned(vKBTimer) then
      vKBTimer.Enabled := True;
  end;
  inherited;
end;

function TCustomCaret.GetFlasher: IFlasher;
var
  LFlasher: TFmxObject;
begin
  Result := nil;
  if (Owner.ComponentState * [csLoading, csDestroying] = []) then
  begin
    LFlasher := FMX.Types.Flasher(TCaretClass(ClassType));
    if Assigned(LFlasher) then
      Result := LFlasher as IFlasher
  end;
end;

function TCustomCaret.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TCustomCaret.GetWidth: Word;
begin
  Result := Trunc(Size.cx);
end;

procedure TCustomCaret.SetWidth(const Value: Word);
var
  LSize: TSizeF;
begin
  if GetWidth <> Value then
  begin
    LSize := Size;
    LSize.cx := Value;
    Size := LSize;
  end;
end;

function TCustomCaret.CanShow: Boolean;
var
  LParent: TFmxObject;
begin
  Result := (FIControl.Visible) and
            (FIControl.Enabled) and
            (FIControl.IsFocused) and
            ([csLoading, csDestroying] * FOwner.ComponentState = []);
  if Result then
  begin
    LParent := FOwner;
    while Assigned(LParent) and (not (LParent is TCommonCustomForm)) do
      LParent := LParent.Parent;
    if (LParent is TCommonCustomForm) then
      Result := TCommonCustomForm(LParent).Active
    else
      Result := False;
  end;
end;

procedure TCustomCaret.DoDisplayChanged(const VirtualKeyBoardState: TVirtualKeyBoardState);
begin
  if Assigned(FOnDisplayChanged) then
    FOnDisplayChanged(self, VirtualKeyBoardState);
end;

procedure TCustomCaret.DoUpdateFlasher;
begin
  //  NewVisible := Visible and Displayed and (not TemporarilyHidden);
  if Assigned(Flasher) then
  begin
    if Displayed then
      Flasher.Caret := self
    else
      Flasher.Caret := nil;
    Flasher.UpdateState;
  end;
end;

procedure TCustomCaret.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    if FVisible then
      Show
    else
      Hide;
  end;
end;

procedure TCustomCaret.StartTimer;
begin
  if not Assigned(vKBTimer) then
    vKBTimer := TKBTimer.Create(nil);
  if Assigned(vKBTimer) then
    vKBTimer.Enabled := True;
end;

procedure TCustomCaret.Show;
var
  VirtualKeyBoardState: TVirtualKeyBoardState;
  LVisible: Boolean;
  LFlasherClass: TFmxObjectClass;
begin
  LVisible := FVisible and CanShow;
  if LVisible then
  begin
    if Assigned(vActiveCaret) and (vActiveCaret <> self) then
      vActiveCaret.Hide;
    try
      LFlasherClass := FlasherClass(TCaretClass(ClassType));
      if Assigned(LFlasherClass) and Assigned(Flasher) and (not Displayed) then
      begin
        StartTimer;
        FDisplayed := True;
        try
          DoUpdateFlasher;
        finally
          FChanged := False;
          DoDisplayChanged(VirtualKeyBoardState);
        end;
      end;
      if Displayed then
        vActiveCaret := self;
    except
      FVisible := False;
      Raise;
    end;
  end;
end;

procedure TCustomCaret.Hide;
var
  VirtualKeyBoardState: TVirtualKeyBoardState;
begin
  try
    if Displayed then
    begin
      try
        if Assigned(vKBTimer) then
          vKBTimer.Enabled := True;
      finally
        FDisplayed := False;
        try
          DoUpdateFlasher;
        finally
          FChanged := False;
          DoDisplayChanged(VirtualKeyBoardState);
        end;
      end;
    end;
  finally
    if vActiveCaret = self then
      vActiveCaret := nil;
  end;
end;

procedure TCustomCaret.SetTemporarilyHidden(const Value: boolean);
begin
  if FTemporarilyHidden <> Value then
  begin
    FTemporarilyHidden := Value;
    if Displayed then UpdateFlasher;
  end;
end;

procedure TCustomCaret.UpdateFlasher;
begin
  if (FUpdateCount <= 0) then
  begin
    try
      DoUpdateFlasher;
    finally
      FChanged := False;
    end;
  end
  else
    FChanged := True;
end;

procedure TCustomCaret.SetColor(const Value: TAlphaColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    if Displayed then UpdateFlasher;
  end;
end;

procedure TCustomCaret.SetDefaultColor(const Value: TAlphaColor);
begin
  if FDefaultColor <> Value then
  begin
    FDefaultColor := Value;
    if Displayed then UpdateFlasher;
  end;
end;

procedure TCustomCaret.SetInterval(const Value: TFlasherInterval);
begin
  if FInterval <> Value then
  begin
    FInterval := Value;
    if Displayed then UpdateFlasher;
  end;
end;

procedure TCustomCaret.SetPos(const Value: TPointF);
begin
  if not (Value = FPos) then
  begin
    FPos := Value;
    if Displayed then UpdateFlasher;
  end;
end;

procedure TCustomCaret.SetReadOnly(const Value: boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    if Displayed then UpdateFlasher;
    StartTimer;
  end;
end;

procedure TCustomCaret.SetSize(const Value: TSizeF);
begin
  if not (Value = FSize) then
  begin
    FSize := Value;
    if Displayed then UpdateFlasher;
  end;
end;

procedure TCustomCaret.Assign(Source: TPersistent);
begin
  if not Assigned(Source) or (Source is TCustomCaret) then
  begin
    BeginUpdate;
    try
      if Assigned(Source) then
      begin
        Size := TCustomCaret(Source).Size;
        Color := TCustomCaret(Source).Color;
        Interval := TCustomCaret(Source).Interval;
      end
      else
      begin
        Size := PointF(0, 0);
        Color := TAlphaColorRec.Null;
        Interval := 0;
      end;
    finally
      EndUpdate;
    end;
  end
  else
    inherited;
end;

procedure TCustomCaret.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TCustomCaret.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if (FUpdateCount = 0) and (FChanged) then
    try
      DoUpdateFlasher;
    finally
      FChanged := False;
    end;
  end;
end;
{$ENDREGION}

type
  TVirtualKeyInfo = record
    VirtualKey: Word;
    KeyKind: TKeyKind;
  end;
var
  vKeyMapping: TDictionary<Word,TVirtualKeyInfo> = nil;

function RegisterKeyMapping(const PlatformKey, VirtualKey: Word; const KeyKind: TKeyKind): Boolean;
var
  KeyInfo: TVirtualKeyInfo;
begin
  Result := False;
  if not Assigned(vKeyMapping) then
    vKeyMapping := TDictionary<Word,TVirtualKeyInfo>.Create;
  if not vKeyMapping.ContainsKey(PlatformKey) then
  begin
    KeyInfo.VirtualKey := VirtualKey;
    KeyInfo.KeyKind := KeyKind;
    vKeyMapping.Add(PlatformKey, KeyInfo);
    Result := True;
  end;
end;

function UnregisterKeyMapping(const PlatformKey: Word): Boolean;
begin
  Result := False;
  if Assigned(vKeyMapping) and vKeyMapping.ContainsKey(PlatformKey) then
  begin
    vKeyMapping.Remove(PlatformKey);
    Result := True;
  end;
end;

function PlatformKeyToVirtualKey(const PlatformKey: Word; var KeyKind: TKeyKind): Word;
var
  KeyInfo: TVirtualKeyInfo;
begin
  Result := 0;
  KeyKind := TKeyKind.kkUnknown;
  if Assigned(vKeyMapping) and vKeyMapping.ContainsKey(PlatformKey) then
  begin
    KeyInfo := vKeyMapping[PlatformKey];
    Result := KeyInfo.VirtualKey;
    KeyKind := KeyInfo.KeyKind;
  end;
end;



{ Initialization and finalization }

procedure FreeCloneCache;
var
  List: TList<TRttiProperty>;
begin
  for List in ClonePropertiesCache.Values do
    List.DisposeOf;
  for List in ClonePersistentCache.Values do
    List.DisposeOf;

  FreeAndNil(ClonePropertiesCache);
  FreeAndNil(ClonePersistentCache);
  SharedContext.Free;
end;

procedure FreeFmxGlobals;
var
  I: integer;
  List: TFmxObjectList;
begin
  FreeAndNil(AniThread);
  if Assigned(ResourceDict) then
  begin
    for List in ResourceDict.Values do
      List.DisposeOf;
    FreeAndNil(ResourceDict);
  end;


  FreeAndNil(CollectLang);
  FreeAndNil(Lang);
  FreeCloneCache;
  FreeAndNil(vKBTimer);
  if Assigned(vFlashers) then
  begin
    for I := vFlashers.Count - 1 downto 0 do
    try
      vFlashers[I].Flasher.DisposeOf;
      vFlashers.Delete(I);
    except
      Continue;
    end;
    FreeAndNil(vFlashers);
  end;
end;

{$IFDEF TEXTDEBUG}
{$IFDEF TEXTDEBUGX}
type
  TTextControlToString = class helper for TObject
  public
    function ToString : String;
  end;

function TTextControlToString.ToString : String;
begin
  Result := ClassName;
  if Self is TFmxObject then
  begin
    if TFmxObject(Self).Name <> EmptyStr then
      Result := Result + ' "' + TFmxObject(Self).Name + '"';

    if Self is TTextControl then
      Result := Result + '="' + TTextControl(Self).Text + '"';

    if Self is TContent then
      Result := Format('%s (%d)', [Result, TContent(Self).ChildrenCount]);;

    if Self is TControl then
      Result := Format('%s {%s} ', [Result, Log.ArrayToString(TControl(Self).Controls)]);

  end;
end;
{$ENDIF}
{$ENDIF}

{ Log }
class procedure Log.d(const Tag: String; const Instance : TObject; const Msg: String);
begin
  Log.d(Tag, Instance, String.Empty, Msg);
end;

class function Log.ArrayToString(const AArray: TEnumerable<TFmxObject>): String;
begin
  Result := ArrayToString(AArray, function(O : TObject):String begin Exit(O.ToString) end);
end;

class function Log.ArrayToString(const AArray: TEnumerable<TFmxObject>;
  MakeStr: ToStringFunc): String;
const
  Delimiter : array [Boolean] of String = ('', ',');
var
  O : TFmxObject;
begin
  Result := EmptyStr;
  for O in AArray do
  begin
    Result := Result + Format('%s%s', [Delimiter[Length(Result)>0], MakeStr(O)]);
  end;
  Result := '[' + Result + ']';
end;

class constructor Log.Create;
begin
  TPlatformServices.Current.SupportsPlatformService(IFMXLoggingService, FLogger);
end;

class procedure Log.d(const Fmt: String; const Args: array of const);
begin
  if Assigned(FLogger) then
    (FLogger as IFMXLoggingService).Log(Fmt, Args);
end;

class procedure Log.d(const Msg: String);
begin
  if Assigned(FLogger) then
    (FLogger as IFMXLoggingService).Log(Msg, []);
end;

class procedure Log.TimeStamp(const Msg: String);
var
  H, M, S, MS: Word;
begin
  DecodeTime(Time, H, M, S, MS);
  if Assigned(FLogger) then
    (FLogger as IFMXLoggingService).Log('%d:%d:%d %s', [M, S, MS, Msg]);
end;

{class function Log.ArrayToString(AArray: TEnumerable<TControl>): String;
var
  R : TList<TFmxObject>;
  O : TFmxObject;
begin
  if AArray = nil then
    Exit('[nil]');
  R := TList<TFmxObject>.Create;
  for O in AArray do
    R.Add(O);
  Result := ArrayToString(R);
  R.Free;
end; }

class procedure Log.d(const Tag: String; const Instance : TObject; const Method, Msg: String);
begin
  if not Assigned(FLogger) then
    Exit;
  try
    if Length(Method) > 0 then
      (FLogger as IFMXLoggingService).Log('%s (%s.%s): %s', [Tag, Log.ObjToString(Instance), Method, Msg])
    else
      (FLogger as IFMXLoggingService).Log('%s (%s): %s', [Tag, Log.ObjToString(Instance), Msg]);
  except
    on E : Exception do;
  end;
end;

class procedure Log.DumpFmxObject(const O: TFmxObject; Nest: Integer = 0);
var
  I : Integer;
  Child : TObject;
begin
  for I := 0 to Nest-1 do
    Write(' ');
  WriteLn(O.ToString);
  if O.Children = nil then Exit;
  for Child in O.Children do
  begin
    if Child is TFmxObject then
      DumpFmxObject(TFmxObject(Child), Nest + 2);
  end;
end;

class function Log.ObjToString(const Instance : TObject): String;
var
  ClassName : String;
  Addr : NativeInt;
begin
  ClassName := EmptyStr;
  if Instance <> nil then
  begin
    ClassName := Instance.ClassName;
    Addr := NativeInt(Instance);
  end
  else
    Addr := NativeInt(Instance);
  if Addr = 0 then
    Result := 'Nil'
  else
{$IFDEF AUTOREFCOUNT}
    Result := Format('%s@%8x(%d)', [ClassName, Addr, Instance.RefCount]);
{$ELSE}
    Result := Format('%s@%8x)', [ClassName, Addr]);
{$ENDIF}
end;

procedure RegisterShortCuts;
  procedure RegisterAlpha(Shift: Word);
  var S: TShortCut;
  begin
    for S := Byte('A') to Byte('Z') do
    begin
      RegisterShortCut(S + Shift)
    end;
  end;
  procedure RegisterF(Shift: Word);
  var S: TShortCut;
  begin
    for S := vkF1 to vkF12 do
    begin
      RegisterShortCut(S + Shift)
    end;
  end;
  procedure RegisterOther(Shift: Word);
  const OtherKeys: array [0..3] of TShortCut = (vkInsert, vkDelete, vkReturn, vkEscape);
  var I: integer;
  begin
    for I := Low(OtherKeys) to High(OtherKeys) do
    begin
      RegisterShortCut(OtherKeys[I] + Shift);
    end;
  end;
begin
  RegisterAlpha(scCtrl);
  RegisterAlpha(scCtrl or scAlt);
  RegisterAlpha(scCommand);
  RegisterAlpha(scCtrl or scCommand);
  RegisterAlpha(scShift or scCommand);
  RegisterAlpha(scAlt or scCommand);


  RegisterF(0);
  RegisterF(scCtrl);
  RegisterF(scShift);
  RegisterF(scAlt);
  RegisterF(scShift or scCtrl);
  RegisterF(scCommand);
  RegisterF(scCtrl or scCommand);
  RegisterF(scShift or scCommand);

  RegisterOther(scShift);
  RegisterOther(scCtrl);
  RegisterOther(scAlt);
  RegisterOther(scCommand);

  RegisterShortCut(vkBack or scAlt);
  RegisterShortCut(vkBack or scShift or scAlt);
  RegisterShortCut(vkHardwareBack);
end;

procedure TFmxObject.IgnoreIntegerValue(Reader: TReader);
begin
  Reader.ReadInteger;
end;

{ TEnumerationFilter<T> }

constructor TEnumerableFilter<F, T>.Create(const FullEnum: TEnumerable<F>;
  SelfDestruct: Boolean);
begin
  FBaseEnum := FullEnum;
  FSelfDestruct := SelfDestruct;
end;

function TEnumerableFilter<F,T>.DoGetEnumerator: TEnumerator<T>;
begin
  if FSelfDestruct then
    Result := TFilterEnumerator.Create(FBaseEnum, Self)
  else
    Result := TFilterEnumerator.Create(FBaseEnum, nil);
end;

class function TEnumerableFilter<F, T>.Filter(
  const Src: TEnumerable<F>): TEnumerableFilter<F, T>;
begin
  Result := TEnumerableFilter<F,T>.Create(Src, True);
end;

constructor TEnumerableFilter<F, T>.TFilterEnumerator.Create(
  const Enumerable: TEnumerable<F>; const Cleanup: TEnumerableFilter<F, T>);
begin
  FRawEnumerator := Enumerable.GetEnumerator;
  FCleanup := Cleanup;
end;

destructor TEnumerableFilter<F,T>.TFilterEnumerator.Destroy;
begin
  FRawEnumerator.Free;
  inherited;
  if Assigned(FCleanup) then
    FCleanup.Free;
end;

function TEnumerableFilter<F,T>.TFilterEnumerator.DoGetCurrent: T;
begin
  Result := GetCurrent;
end;

function TEnumerableFilter<F,T>.TFilterEnumerator.DoMoveNext: Boolean;
begin
  Result := MoveNext;
end;

function TEnumerableFilter<F,T>.TFilterEnumerator.GetCurrent: T;
begin
  Result := FCurrent;
end;

function TEnumerableFilter<F,T>.TFilterEnumerator.MoveNext: Boolean;
begin
  Result := False;
  while (not Result) and (FRawEnumerator.MoveNext) do
    if FRawEnumerator.Current is T then
    begin
      FCurrent := FRawEnumerator.Current as T;
      Result := Assigned(FCurrent);
    end;
end;

{ TFmxChildrenList }

constructor TFmxChildrenList.Create(const AChildren: TFmxObjectList);
begin
  inherited Create;
  FChildren := AChildren;
end;

destructor TFmxChildrenList.Destroy;
begin
  FChildren := nil;
  inherited;
end;

function TFmxChildrenList.DoGetEnumerator: TEnumerator<TFmxObject>;
begin
  Result := FChildren.GetEnumerator;
end;

function TFmxChildrenList.GetChild(AIndex: Integer): TFmxObject;
begin
  Result := FChildren[AIndex];
end;

function TFmxChildrenList.GetChildCount: Integer;
begin
  if Assigned(FChildren) then
    Result := FChildren.Count
  else
    Result := 0;
end;

function TFmxChildrenList.IndexOf(const Obj: TFmxObject): Integer;
begin
  if Assigned(FChildren) then
    Result := FChildren.IndexOf(Obj)
  else
    Result := -1
end;

initialization
  vPurgatory := TPurgatory.Create(nil);
  ClonePropertiesCache := TDictionary<string , TList<TRttiProperty>>.Create;
  ClonePersistentCache := TDictionary<string , TList<TRttiProperty>>.Create;
  SharedContext := TRttiContext.Create;
  StartClassGroup(TFmxObject);
  ActivateClassGroup(TFmxObject);
  GroupDescendentsWith(TCustomActionList, TFmxObject);
  GroupDescendentsWith(TCustomAction, TFmxObject);
  GroupDescendentsWith(TCustomGestureManager, TFmxObject);

  RegisterFmxClasses([TFmxObject, TTimer], [TBounds, TPosition]);

  RegisterAlphaColorIntegerConsts;
  RegisterCursorIntegerConsts;
  RegisterShortCuts;

  USFormatSettings := TFormatSettings.Create('en-us');
finalization
  FreeAndNil(vKeyMapping);
  FreeFmxGlobals;
  FreeAndNil(vPurgatory);

end.
