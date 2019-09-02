{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit FMX.Platform.Android;

interface

uses
  System.Classes, System.Types, System.UITypes, System.Rtti, System.SyncObjs,
  FMX.Types, FMX.Platform, FMX.Forms, FMX.Types3D, FMX.Messages,
  Androidapi.NativeWindow, Androidapi.NativeActivity, Androidapi.AppGlue, Androidapi.Timer, Androidapi.JNI.App,
  Androidapi.JNIBridge, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Embarcadero, Posix.SysTypes;

type
  TAndroidWindowHandle = class(TWindowHandle)
  strict private
    FTexture: TTexture;
    FBounds: TRectF;
    [weak] FForm: TCommonCustomForm;
    FNeedsUpdate: Boolean;
  private
    procedure SetBounds(const Value: TRectF);
    procedure SetNeedsUpdate(const Value: Boolean);
    function GetIsPopup: Boolean;
  public
    constructor Create(const AForm: TCommonCustomForm);
    procedure CreateTexture;
    procedure DestroyTexture;
    property Bounds: TRectF read FBounds write SetBounds;
    property Form: TCommonCustomForm read FForm;
    property Texture: TTexture read FTexture;
    property NeedsUpdate: Boolean read FNeedsUpdate write SetNeedsUpdate;
    property IsPopup: Boolean read GetIsPopup;
  end;

  { Broadcast messages: Taking images, receive notification }

  TMessageReceivedNotification = class(TMessage<JIntent>);
  TMessageCancelReceivingImage = class(TMessage<Integer>);
  TMessageReceivedImage = class (TMessage<JBitmap>)
  public
    RequestCode: Integer;
  end;

  TScreenScaleOverrideHook = procedure(const UserContext: Pointer; const DensityScale, DensityDPI: Single;
    var ScreenScale: Single);

function WindowHandleToPlatform(const AHandle: TWindowHandle): TAndroidWindowHandle;
function GetAndroidApp: PAndroid_app;
function MainActivity: JFMXNativeActivity;

function ConvertPixelToPoint(const P: TPointF): TPointF;
function ConvertPointToPixel(const P: TPointF): TPointF;

procedure SetScreenScaleOverrideHook(const UserContext: Pointer; const Hook: TScreenScaleOverrideHook);
procedure UnsetScreenScaleOverrideHook;

procedure RegisterCorePlatformServices;
procedure UnregisterCorePlatformServices;

implementation

uses
  System.SysUtils, Posix.Time, System.Math, System.Generics.Collections, System.Character,
  Androidapi.Input, Androidapi.Looper, Androidapi.Jni, Androidapi.Jni.JavaTypes, System.RTLConsts,
  Androidapi.Egl, Androidapi.Gles2, Androidapi.Gles2ext, Androidapi.Jni.Util,
  Androidapi.Log, Androidapi.JNI.Widget, Androidapi.JNI.Os, FMX.KeyMapping,
  FMX.Helpers.Android, FMX.Canvas.Android, FMX.Canvas.GPU, FMX.Context.GLES.Android, FMX.Controls, FMX.Controls.Android,
  FMX.Materials.Canvas, FMX.Pickers, FMX.Gestures, FMX.Gestures.Android, FMX.Dialogs, FMX.VirtualKeyboard,
  FMX.VirtualKeyboard.Android, FMX.Consts, FMX.Text, FMX.Graphics, FMX.TextLayout, Androidapi.Keycodes,
  Androidapi.Rect, FMX.WebBrowser;


const
  DBL_TAP_DELAY = 300;
  SINGLE_TAP_DELAY = 150;
  LONG_TAP_DURATION = 500;
  LONG_TAP_MOVEMENT = 10; //10 pixels - use scale to transform to points to use on each device
  HIDE_PASTE_MENU_DELAY = 2500;

type
  TContextMenuItem = (cmiCopy, cmiCut, cmiPaste);
  TContextMenuItems = set of TContextMenuItem;

const
  AllContextMenuItems = [cmiCopy, cmiCut, cmiPaste];

type
  TWindowManager = class;

  TFMXNativeActivityListener = class (TJavaLocal, JOnActivityListener)
  public
    procedure onReceiveImage(ARequestCode: Integer; ABitmap: JBitmap); cdecl;
    procedure onCancelReceiveImage(ARequestCode: Integer); cdecl;
    procedure onReceiveNotification(P1: JIntent); cdecl;
  end;

  TCopyButtonClickListener = class(TJavaLocal, JView_OnClickListener)
  public
    procedure onClick(P1: JView); cdecl;
  end;

  TCutButtonClickListener = class(TJavaLocal, JView_OnClickListener)
  public
    procedure onClick(P1: JView); cdecl;
  end;

  TPasteButtonClickListener = class(TJavaLocal, JView_OnClickListener)
  public
    procedure onClick(P1: JView); cdecl;
  end;

  TWindowManager = class
  private
    FWindows: TList<TAndroidWindowHandle>;
    FVisibleStack: TStack<TAndroidWindowHandle>;
    FGestureControl: TFmxObject;
    FNeedsRender: Boolean;
    FPause: Boolean;
    FScale: Single;
    FContentRect: TRect;
    FStatusBarHeight: Integer;
    //Text editing
    FTextInput: ITextInput;
    FTextActions: ITextActions;
    FFocusedControl: TFmxObject;
    FContextMenuPopup: JPopupWindow;
    FContextMenuPopupSize: TSize;
    FContextMenuLayout: JLinearLayout;
    FContextButtonsLayout: JLinearLayout;
    FCopyButton: JButton;
    FCopyClickListener: TCopyButtonClickListener;
    FCutButton: JButton;
    FCutClickListener: TCutButtonClickListener;
    FPasteButton: JButton;
    FPasteClickListener: TPasteButtonClickListener;
    FContextMenuVisible: Boolean;
    [Weak] FCapturedWindow: TAndroidWindowHandle;
    FSelectionInProgress: Boolean;
    FPasteMenuTimer: TFmxHandle;
    procedure SetPause(const Value: Boolean);
    function RetreiveContentRect: TRect;
    procedure UpdateContentRect;
    procedure UpdateFormSizes;
    procedure ShowContextMenu(const ItemsToShow: TContextMenuItems = AllContextMenuItems);
    procedure HideContextMenu;
    procedure ClosePopups(SavedWindow: TAndroidWindowHandle);
    class var FInstance: TWindowManager;
    class function GetInstance: TWindowManager; static;
    procedure CreatePasteMenuTimer;
    procedure DestroyPasteMenuTimer;
    procedure PasteMenuTimerCall;
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeginSelection;
    procedure EndSelection;
    procedure SetNeedsRender;
    function RenderIfNeeds: Boolean;
    procedure RenderImmediately;
    procedure Render;
    procedure BringToFront(const AHandle: TAndroidWindowHandle);
    procedure SendToBack(const AHandle: TAndroidWindowHandle);
    procedure AddWindow(const AHandle: TAndroidWindowHandle);
    procedure RemoveWindow(const AHandle: TAndroidWindowHandle);
    function AlignToPixel(const Value: Single): Single; inline;
    function FindForm(const AHandle: TWindowHandle): TCommonCustomForm;
    function FindWindowByPoint(X, Y: Single): TAndroidWindowHandle;
    function FindTopWindow: TAndroidWindowHandle;
    function SendCMGestureMessage(AEventInfo: TGestureEventInfo): Boolean;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure MouseMove(Shift: TShiftState; X, Y: Single);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single; DoCLick: Boolean = True);
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
    procedure KeyUp(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState; KeyDownHandled: Boolean);
    procedure SetCapture(const AForm: TAndroidWindowHandle);
    procedure ReleaseCapture;
    function PixelToPoint(const P: TPointF): TPointF;
    function PointToPixel(const P: TPointF): TPointF;
    function ClientToScreen(const Handle: TAndroidWindowHandle; const Point: TPointF): TPointF;
    function ScreenToClient(const Handle: TAndroidWindowHandle; const Point: TPointF): TPointF;
    function CheckContentRectUpdate: Boolean;
    procedure SingleTap(const APoint: TPointF);
    property ContentRect: TRect read FContentRect;
    property Windows: TList<TAndroidWindowHandle> read FWindows;
    property Pause: Boolean read FPause write SetPause;
    property Scale: Single read FScale;
    property StatusBarHeight: Integer read FStatusBarHeight;
    class property Current: TWindowManager read GetInstance;
  end;

  { TPlatformAndroid }

  TPlatformAndroid = class(TInterfacedObject , IFMXApplicationEventService, IFMXApplicationService,
    IFMXWindowService, IFMXCanvasService, IFMXContextService, IFMXTimerService, IFMXStyleService,
    IFMXLoggingService, IFMXSystemInformationService, IFMXDialogService, IFMXGestureRecognizersService,
    IFMXScreenService, IFMXSystemFontService, IFMXMouseService, IFMXClipboardService,
    IFMXTextService, IFMXLocaleService, IFMXDefaultMetricsService, IFMXDeviceService, IFMXListingService)
  private const
    // Number of seconds to wait before switching to low priority message processing mode (for power saving).
    PriorityProcessingTime = 1;
    // Timeout in milliseconds for different processing priorities when waiting for messages.
    PollTimeoutHighPriority = 1;
    PollTimeoutLowPriority = 10;
    PollTimeoutModalWindow = 33;
  private type
    TMotionEvent = record
      Position: TPointF;
      EventAction: Int32;
      Shift: TShiftState;
    end;
    TMotionEvents = TList<TMotionEvent>;
  private
    AndroidApp: PAndroid_app;
    FOnApplicationEvent: TApplicationEventHandler;
    FActivityListener: TFMXNativeActivityListener;
    FFirstRun: Boolean;
    FLogPrefix: string;
    FActiveInteractiveGestures: TInteractiveGestures;
    FEnabledInteractiveGestures: TInteractiveGestures;
    FOldPoint1, FOldPoint2: TPointF;
    FDoubleTapTimer: TFmxHandle;
    FLongTapTimer: TFmxHandle;
    FSingleTapTimer: TFmxHandle;
    FDblClickFirstMouseUp: Boolean;
    FGestureEnded: Boolean;
    FSingleTap: Boolean;
    FScreenScale: Single;
    FScreenSize: TPointF;
    FMouseCoord: TPointF;
    FMouseDownCoordinates: TPointF;
    FKeyCharacterMap: JKeyCharacterMap;
    FTextEditorProxy: JFMXTextEditorProxy;
    FRotationAngle: Single;
    FInternalEventQueue: TQueue<TThreadProcedure>;
    FEventQueueEmptyEvent: TEvent;
    FMotionEvents: TMotionEvents;
    FDownKey: Word;
    FDownKeyChar: System.WideChar;
    FKeyDownHandled: Boolean;
    PriorityProcessing: Boolean;
    PriorityProcessingWait: Boolean;
    PriorityProcessingStartTime: Extended;
    OrientationMightHaveChanged: Boolean;
    FSkipEventQueue: TQueue<JKeyEvent>;
    procedure CreateDoubleTapTimer;
    procedure DestroyDoubleTapTimer;
    procedure DoubleTapTimerCall;
    procedure CreateLongTapTimer;
    procedure DestroyLongTapTimer;
    procedure LongTapTimerCall;
    procedure SingleTap;
    procedure CreateSingleTapTimer;
    procedure DestroySingleTapTimer;
    procedure SingleTapTimerCall;
    function GetLongTapAllowedMovement: Single;
    function IsPopupForm(const AForm: TCommonCustomForm): Boolean;
    function ShiftStateFromMetaState(const AMetaState: Integer): TShiftState;
    procedure InternalWaitMessage(const TimeoutOverride: Integer = 0);
    function IsZoom(const APoint1, APoint2: TPointF): Boolean;
    function IsRotate(const APoint1, APoint2: TPointF): Boolean;
    function CreateGestureEventInfo(ASecondPointer: TPointF; const AGesture: TInteractiveGesture; const AGestureEnded: Boolean = False): TGestureEventInfo;
    function FindAndHandleInteractiveGesture(const ALookIn: TInteractiveGestures; const APoint: TPointF): Boolean;
    function HandleAndroidKeyEvent(AEvent: PAInputEvent): Int32;
    procedure ProcessAndroidGestureEvents;
    procedure ProcessAndroidMouseEvents;
    function HandleAndroidMotionEvent(AEvent: PAInputEvent): Int32;
    procedure SetKeyboardEventToSkip(event: JKeyEvent);

    function GetListingHeaderBehaviors: TListingHeaderBehaviors;
    function GetListingSearchFeatures: TListingSearchFeatures;
    function GetListingTransitionFeatures: TListingTransitionFeatures;
    function IFMXListingService.GetHeaderBehaviors = GetListingHeaderBehaviors;
    function IFMXListingService.GetSearchFeatures = GetListingSearchFeatures;
    function IFMXListingService.GetTransitionFeatures = GetListingTransitionFeatures;
  public
    constructor Create;
    destructor Destroy; override;
    procedure HandleAndroidCmd(ACmd: Int32); inline;
    function HandleAndroidInputEvent(AEvent: PAInputEvent): Int32; inline;
    procedure RunOnUIThread(Proc: TThreadProcedure);
    procedure SynchronizeOnUIThread(Proc: TThreadProcedure);

    { IFMXMouseService }
    function GetMousePos: TPointF;
    { IFMXTimerService }
    function CreateTimer(Interval: Integer; TimerFunc: TTimerProc): TFmxHandle;
    function DestroyTimer(Timer: TFmxHandle): Boolean;
    function GetTick: Extended;
    { IFMXApplicationService }
    procedure Run;
    procedure Terminate;
    function HandleMessage: Boolean;
    procedure WaitMessage;
    function GetTitle: string;
    function Terminating: Boolean;
    { IFMXApplicationEventService }
    procedure SetApplicationEventHandler(AEventHandler: TApplicationEventHandler);
    function HandleApplicationEvent(AEvent: TApplicationEvent; AContext: TObject): Boolean;
    { IFMXWindowService }
    function FindForm(const AHandle: TWindowHandle): TCommonCustomForm;
    function CreateWindow(const AForm: TCommonCustomForm): TWindowHandle;
    procedure DestroyWindow(const AForm: TCommonCustomForm);
    procedure ReleaseWindow(const AForm: TCommonCustomForm);
    procedure ShowWindow(const AForm: TCommonCustomForm);
    procedure HideWindow(const AForm: TCommonCustomForm);
    procedure BringToFront(const AForm: TCommonCustomForm);
    procedure SendToBack(const AForm: TCommonCustomForm);
    procedure Activate(const AForm: TCommonCustomForm);
    function ShowWindowModal(const AForm: TCommonCustomForm): TModalResult;
    procedure InvalidateWindowRect(const AForm: TCommonCustomForm; R: TRectF);
    procedure InvalidateImmediately(const AForm: TCommonCustomForm);
    procedure SetWindowRect(const AForm: TCommonCustomForm; ARect: TRectF);
    function GetWindowRect(const AForm: TCommonCustomForm): TRectF;
    function GetClientSize(const AForm: TCommonCustomForm): TPointF;
    procedure SetClientSize(const AForm: TCommonCustomForm; const ASize: TPointF);
    procedure SetWindowCaption(const AForm: TCommonCustomForm; const ACaption: string);
    procedure SetCapture(const AForm: TCommonCustomForm);
    procedure SetWindowState(const AForm: TCommonCustomForm; const AState: TWindowState);
    procedure ReleaseCapture(const AForm: TCommonCustomForm);
    function ClientToScreen(const AForm: TCommonCustomForm; const Point: TPointF): TPointF;
    function ScreenToClient(const AForm: TCommonCustomForm; const Point: TPointF): TPointF;
    function GetWindowScale(const AForm: TCommonCustomForm): Single;
    { IFMXScreenService }
    function GetScreenSize: TPointF;
    function GetScreenScale: Single;
    function GetScreenOrientation: TScreenOrientation;
    procedure SetScreenOrientation(AOrientations: TScreenOrientations);
    { IFMXCanvasService }
    procedure RegisterCanvasClasses;
    procedure UnregisterCanvasClasses;
    { IFMXContextService }
    procedure RegisterContextClasses;
    procedure UnregisterContextClasses;
    { IFMXStyleService }
    function GetSystemStyle(const Context: TFmxObject): TFmxObject;
    { IFMXLoggingService }
    procedure Log(Fmt: String; Params: array of const);
    { IFMXSystemInformationService }
    function GetScrollingBehaviour: TScrollingBehaviours;
    function GetMinScrollThumbSize: Single;
    function GetCaretWidth: Integer;
    { IFMXDialogService }
    function DialogOpenFiles(var AFileName: TFileName; const AInitDir, ADefaultExt, AFilter, ATitle: string;
      var AFilterIndex: Integer; var AFiles: TStrings; var AOptions: TOpenOptions): Boolean;
    function DialogPrint(var ACollate, APrintToFile: Boolean;
      var AFromPage, AToPage, ACopies: Integer; AMinPage, AMaxPage: Integer; var APrintRange: TPrintRange;
      AOptions: TPrintDialogOptions): Boolean;
    function PageSetupGetDefaults(var AMargin, AMinMargin: TRect; var APaperSize: TPointF;
      AUnits: TPageMeasureUnits; AOptions: TPageSetupDialogOptions): Boolean;
    function DialogPageSetup(var AMargin, AMinMargin :TRect; var APaperSize: TPointF;
      var AUnits: TPageMeasureUnits; AOptions: TPageSetupDialogOptions): Boolean;
    function DialogSaveFiles(var AFileName: TFileName; const AInitDir, ADefaultExt, AFilter, ATitle: string;
      var AFilterIndex: Integer; var AFiles: TStrings; var AOptions: TOpenOptions): Boolean;
    function DialogPrinterSetup: Boolean;
    function MessageDialog(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons;
      DefaultButton: TMsgDlgBtn; X, Y: Integer; HelpCtx: Longint; const HelpFileName: string): Integer;
    function InputQuery(const ACaption: string; const APrompts: array of string;
      var AValues: array of string; CloseQueryFunc: TInputCloseQueryFunc = nil): Boolean;
    { IFMXGestureRecognizersService }
    procedure AddRecognizer(const ARec: TInteractiveGesture; const AForm: TCommonCustomForm);
    procedure RemoveRecognizer(const ARec: TInteractiveGesture; const AForm: TCommonCustomForm);
    { IFMXSystemFontService }
    function GetDefaultFontFamilyName: string;
    { IFMXClipboardService }
    procedure SetClipboard(Value: TValue);
    function GetClipboard: TValue;
    { IFMXDefaultPropertyValueService }
    function GetDefaultPropertyValue(const AClassName, PropertyName: string): TValue;
    { IFMXTextService }
    function GetTextServiceClass: TTextServiceClass;
    { IFMXLocaleService }
    function GetCurrentLangID: string;
    function GetLocaleFirstDayOfWeek: string;
    { IFMXDefaultMetricsService }
    function SupportsDefaultSize(const AComponent: TComponentKind): Boolean;
    function GetDefaultSize(const AComponent: TComponentKind): TSize;
    { IFMXDeviceService }
    function GetModel: string;
    function GetFeatures: TDeviceFeatures;
    { Android view for IME }
    function GetTextEditorProxy: JFmxTextEditorProxy;
  end;

  TTimerManager = class
  strict private
    FHandlers: TDictionary<Integer, TTimerProc>;
    FCriticalSection: TCriticalSection;

    class var FInstance: TTimerManager;
    class function GetInstance: TTimerManager; static;

    procedure DoOnTimer(TimerHandle: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    function CreateTimer(Interval: Integer; TimerFunc: TTimerProc): TFmxHandle;
    procedure DestroyTimer(TimerHandle: TFmxHandle);
    class property Current: TTimerManager read GetInstance;
  end;

  TWaitableValueBase = class(TJavaLocal)
  public
    Done: TEvent;
    Value: TValue;
    constructor Create;
  end;

  TTextServiceAndroid = class;

  TFMXTextListener = class(TJavaLocal, JFMXTextListener)
  strict private
    [Weak] FTextService: TTextServiceAndroid;
  public
    constructor Create(const TextService: TTextServiceAndroid); overload;
    procedure onTextUpdated(text: JCharSequence; position: Integer); cdecl;
    procedure onComposingText(beginPosition: Integer; endPosition: Integer); cdecl;
    procedure onSkipKeyEvent(event: JKeyEvent); cdecl;
  end;

  TTextServiceAndroid = class(TTextService)
  type
    TRange = record
      location: Integer;
      length: Integer;
    end;
  private
    FCaretPosition: TPoint;
    FText : string;
    FImeMode: TImeMode;
    FTextView: JFmxTextEditorProxy;
    FTextListener: TFMXTextListener;
    FComposingBegin: Integer;
    FComposingEnd: Integer;
  protected
    function GetText: string; override;
    procedure SetText(const Value: string); override;
    function GetCaretPostion: TPoint; override;
    procedure SetCaretPosition(const Value: TPoint); override;
  public
    procedure InternalUpdate;
    procedure InternalUpdateSelection;

    function CombinedText: string; override;
    function TargetClausePosition: TPoint; override;

    procedure EnterControl(const FormHandle: TWindowHandle); override;
    procedure ExitControl(const FormHandle: TWindowHandle); override;

    procedure DrawSingleLine(const  Canvas: TCanvas;
      const ARect: TRectF; const FirstVisibleChar: integer; const Font: TFont;
      const AOpacity: Single; const Flags: TFillTextFlags; const ATextAlign: TTextAlign;
      const AVTextAlign: TTextAlign = TTextAlign.taCenter;
      const AWordWrap: Boolean = False); overload;  override;

    procedure DrawSingleLine(const Canvas: TCanvas;
      const S: string;
      const ARect: TRectF;
      const Font: TFont;
      const AOpacity: Single; const Flags: TFillTextFlags; const ATextAlign: TTextAlign;
      const AVTextAlign: TTextAlign = TTextAlign.taCenter;
      const AWordWrap: Boolean = False); overload; override;

    function HasMarkedText: Boolean; override;

    function GetImeMode: TImeMode; override;
    procedure SetImeMode(const Value: TImeMode); override;

    { Selection }
    procedure BeginSelection; override;
    procedure EndSelection; override;

  public
    constructor Create(const Owner: IControl; SupportMultiLine: Boolean); override;
    destructor Destroy; override;
  end;

var
  PlatformAndroid: TPlatformAndroid;
  VirtualKeyboardAndroid: TVirtualKeyboardAndroid;
  ScreenScaleOverrideHook: TScreenScaleOverrideHook;
  ScreenScaleOverrideHookContext: Pointer;

procedure SetScreenScaleOverrideHook(const UserContext: Pointer; const Hook: TScreenScaleOverrideHook);
begin
  ScreenScaleOverrideHook := Hook;
  ScreenScaleOverrideHookContext := UserContext;
end;

procedure UnsetScreenScaleOverrideHook;
begin
  ScreenScaleOverrideHook := nil;
  ScreenScaleOverrideHookContext := nil;
end;

function WindowHandleToPlatform(const AHandle: TWindowHandle): TAndroidWindowHandle;
begin
  Result := TAndroidWindowHandle(AHandle);
end;

function GetAndroidApp: PAndroid_app;
begin
  Result := PlatformAndroid.AndroidApp;
end;

function MainActivity: JFMXNativeActivity;
begin
  if (PlatformAndroid.AndroidApp <> nil) and (PlatformAndroid.AndroidApp^.activity <> nil) then
    Result := TJFMXNativeActivity.Wrap(PlatformAndroid.AndroidApp^.activity.clazz)
  else
    Result := nil;
end;

function HandleAndroidInputEvent(var App: TAndroid_app; Event: PAInputEvent): Int32;
begin
  Result := PlatformAndroid.HandleAndroidInputEvent(Event);
end;

procedure HandleAndroidCmd(var App: TAndroid_app; Cmd: Int32);
begin
  PlatformAndroid.HandleAndroidCmd(Cmd);
end;

procedure RegisterCorePlatformServices;
begin
  PlatformAndroid := TPlatformAndroid.Create;
  PlatformAndroid.AndroidApp := InitApp;
  if Assigned(PlatformAndroid.AndroidApp) then
  begin
    PlatformAndroid.AndroidApp^.onAppCmd := @HandleAndroidCmd;
    PlatformAndroid.AndroidApp^.onInputEvent := @HandleAndroidInputEvent;
  end;

  TPlatformServices.Current.AddPlatformService(IFMXLoggingService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXApplicationService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXApplicationEventService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXWindowService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXCanvasService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXContextService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXMouseService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXTimerService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXStyleService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXSystemInformationService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXDialogService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXGestureRecognizersService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXScreenService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXClipboardService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXTextService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXSystemFontService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXLocaleService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXDefaultMetricsService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXDeviceService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXListingService, PlatformAndroid);

  VirtualKeyboardAndroid := TVirtualKeyboardAndroid.Create;
  TPlatformServices.Current.AddPlatformService(IFMXVirtualKeyboardService, VirtualKeyboardAndroid);
end;

procedure UnregisterCorePlatformServices;
begin
end;

{ TCopyButtonClickListener }

procedure TCopyButtonClickListener.onClick(P1: JView);
var
  TSA: TTextServiceAndroid;
begin
  TThread.Queue(nil,
    procedure
    begin
      if Assigned(TWindowManager.Current.FTextActions) then
      begin
        TWindowManager.Current.FTextActions.ResetSelection;
      end;
    end);
  if Assigned(TWindowManager.Current.FTextInput) then
  begin
    TSA := TTextServiceAndroid(TWindowManager.Current.FTextInput.GetTextService);
    if Assigned(TSA) and Assigned(TSA.FTextView) then
      TSA.FTextView.copySelectedText;
  end;
  TWindowManager.Current.HideContextMenu;
end;

{ TCutButtonClickListener }

procedure TCutButtonClickListener.onClick(P1: JView);
var
  TSA: TTextServiceAndroid;
begin
  TThread.Queue(nil,
    procedure
    begin
      if Assigned(TWindowManager.Current.FTextActions) then
      begin
        TWindowManager.Current.FTextActions.ResetSelection;
      end;
    end);
  if Assigned(TWindowManager.Current.FTextInput) then
  begin
    TSA := TTextServiceAndroid(TWindowManager.Current.FTextInput.GetTextService);
    if Assigned(TSA) and Assigned(TSA.FTextView) then
      TSA.FTextView.cutSelectedText;
  end;
  TWindowManager.Current.HideContextMenu;
end;

{ TPasteButtonClickListener }

procedure TPasteButtonClickListener.onClick(P1: JView);
var
  TSA: TTextServiceAndroid;
begin
  TThread.Queue(nil,
    procedure
    begin
      if Assigned(TWindowManager.Current.FTextActions) then
      begin
        TWindowManager.Current.FTextActions.ResetSelection;
      end;
    end);
  if Assigned(TWindowManager.Current.FTextInput) then
  begin
    TSA := TTextServiceAndroid(TWindowManager.Current.FTextInput.GetTextService);
    if Assigned(TSA) and Assigned(TSA.FTextView) then
      TSA.FTextView.pasteText;
  end;
  TWindowManager.Current.HideContextMenu;
end;

{ TWindowManager }

constructor TWindowManager.Create;
begin
  inherited Create;
  FWindows := TList<TAndroidWindowHandle>.Create;
  FVisibleStack := TStack<TAndroidWindowHandle>.Create;
  FScale := PlatformAndroid.GetScreenScale;
  UpdateContentRect;
  FTextInput := nil;
  FTextActions := nil;
  FFocusedControl := nil;
end;

procedure TWindowManager.CreatePasteMenuTimer;
begin
  if FPasteMenuTimer = 0 then
    FPasteMenuTimer := TTimerManager.Current.CreateTimer(HIDE_PASTE_MENU_DELAY, PasteMenuTimerCall);
end;

destructor TWindowManager.Destroy;
begin
  FVisibleStack.DisposeOf;
  FWindows.DisposeOf;
  FTextInput := nil;
  FTextActions := nil;
  FFocusedControl := nil;
  FContextMenuPopup := nil;
  FContextMenuLayout := nil;
  FContextButtonsLayout := nil;
  FCopyButton := nil;
  FCopyClickListener := nil;
  FCutButton := nil;
  FCutClickListener := nil;
  FPasteButton := nil;
  FPasteClickListener := nil;
  inherited;
end;

procedure TWindowManager.DestroyPasteMenuTimer;
begin
  if FPasteMenuTimer <> 0 then
    TTimerManager.Current.DestroyTimer(FPasteMenuTimer);
  FPasteMenuTimer := 0;
end;

procedure TWindowManager.EndSelection;
begin
  FSelectionInProgress := False;
end;

procedure TWindowManager.BeginSelection;
begin
  FSelectionInProgress := True;
end;

procedure TWindowManager.AddWindow(const AHandle: TAndroidWindowHandle);
begin
  FWindows.Add(AHandle);
  UpdateContentRect;
end;

procedure TWindowManager.BringToFront(const AHandle: TAndroidWindowHandle);
begin
  if FWindows.Contains(AHandle) then
  begin
    FWindows.Remove(AHandle);
    FWindows.Add(AHandle);
    if (FVisibleStack.Count = 0) or (FVisibleStack.Peek <> AHandle) then
      FVisibleStack.Push(AHandle);
    SetNeedsRender;
  end;
end;

function TWindowManager.ClientToScreen(const Handle: TAndroidWindowHandle; const Point: TPointF): TPointF;
begin
  if Assigned(Handle) then
    Result := Point + Handle.Bounds.TopLeft
  else
    Result := Point;
end;

function TWindowManager.ScreenToClient(const Handle: TAndroidWindowHandle; const Point: TPointF): TPointF;
begin
  if Assigned(Handle) then
    Result := Point - Handle.Bounds.TopLeft
  else
    Result := Point;
end;

function TWindowManager.SendCMGestureMessage(AEventInfo: TGestureEventInfo): Boolean;
var
  Window: TAndroidWindowHandle;
  Obj: IControl;
  OldGestureControl, TmpControl: TFmxObject;
const
  LGestureMap: array [igiZoom .. igiDoubleTap] of TInteractiveGesture =
    (TInteractiveGesture.igZoom, TInteractiveGesture.igPan,
    TInteractiveGesture.igRotate, TInteractiveGesture.igTwoFingerTap,
    TInteractiveGesture.igPressAndtap, TInteractiveGesture.igLongTap,
    TInteractiveGesture.igDoubleTap);
begin
  Result := False;
  OldGestureControl := nil;
  Window := FindWindowByPoint(AEventInfo.Location.X, AEventInfo.Location.Y);
  if TInteractiveGestureFlag.gfBegin in AEventInfo.Flags then
  begin
    if Window <> nil then
    begin
      // find the control from under the gesture
      Obj := Window.Form.ObjectAtPoint(AEventInfo.Location);
      if Assigned(FGestureControl) then
        OldGestureControl := FGestureControl;
      if Assigned(Obj) then
        FGestureControl := Obj.GetObject
      else
        FGestureControl := Window.Form;

      while Assigned(FGestureControl) and
        not (LGestureMap[AEventInfo.GestureID] in FGestureControl.Touch.InteractiveGestures) do
        FGestureControl := FGestureControl.Parent;
    end;
  end;

  if not FSelectionInProgress then
    if (Window <> nil) and (Window.Form.Focused <> nil) then
      if Window.Form.Focused.GetObject <> FFocusedControl then
      begin
        FFocusedControl := Window.Form.Focused.GetObject;
        if not Supports(FFocusedControl, ITextInput, FTextInput) or not Supports(FFocusedControl, ITextActions, FTextActions) then
        begin
          FTextInput := nil;
          FTextActions := nil;
        end;
      end
      else
    else
      if Assigned(FFocusedControl) then
      begin
        FFocusedControl := nil;
        FTextInput := nil;
        FTextActions := nil;
      end;

  if Assigned(FGestureControl) then
  begin
    try
      FGestureControl.CMGesture(AEventInfo);
    except
      Application.HandleException(FGestureControl);
    end;

    if Window <> nil then
    begin
      Obj := Window.Form.ObjectAtPoint(AEventInfo.Location);
      if Obj <> nil then
        TmpControl := Obj.GetObject
      else
        TmpControl := Window.Form;

      if (AEventInfo.GestureID = igiLongTap) and Supports(TmpControl, ITextInput, FTextInput) and Supports(TmpControl, ITextActions, FTextActions) then
      begin
        FTextActions.SelectWord;
        TTextServiceAndroid(FTextInput.GetTextService).InternalUpdateSelection;
        if not FTextInput.GetSelection.IsEmpty then
          ShowContextMenu;
      end;

      if (AEventInfo.GestureID = igiDoubleTap) and Supports(TmpControl, ITextInput, FTextInput) and Supports(TmpControl, ITextActions, FTextActions) then
      begin
        TTextServiceAndroid(FTextInput.GetTextService).InternalUpdateSelection;
        if not FTextInput.GetSelection.IsEmpty then
          ShowContextMenu;
      end;
    end;
    Result := True;
  end else
    FGestureControl := OldGestureControl;

  if TInteractiveGestureFlag.gfEnd in AEventInfo.Flags then
    FGestureControl := nil;
end;

procedure TWindowManager.SendToBack(const AHandle: TAndroidWindowHandle);
begin
  if FWindows.Contains(AHandle) then
  begin
    FWindows.Remove(AHandle);
    FWindows.Insert(0, AHandle);
    SetNeedsRender;
  end;
end;

function TWindowManager.FindForm(const AHandle: TWindowHandle): TCommonCustomForm;
begin
  Result := nil;
  if FWindows.Contains(TAndroidWindowHandle(AHandle)) then
    Result := FWindows[FWindows.IndexOf(TAndroidWindowHandle(AHandle))].Form;
end;

function TWindowManager.FindTopWindow: TAndroidWindowHandle;
var
  I: Integer;
begin
  for I := FWindows.Count - 1 downto 0 do
    if FWindows[I].Form.Visible then
      Exit(FWindows[I]);
  Result := nil;
end;

function TWindowManager.FindWindowByPoint(X, Y: Single): TAndroidWindowHandle;
var
  I: Integer;
begin
  for I := FWindows.Count - 1 downto 0 do
    if FWindows[I].Form.Visible and FWindows[I].Bounds.Contains(PointF(X, Y)) then
      Exit(FWindows[I]);
  Result := nil;
end;

class function TWindowManager.GetInstance: TWindowManager;
begin
  if not Assigned(FInstance) then
    FInstance := TWindowManager.Create;
  Result := FInstance;
end;

procedure TWindowManager.KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
var
  Window: TAndroidWindowHandle;
begin
  HideContextMenu;
  Window := FindTopWindow;
  if Assigned(Window) then
    try
      Window.Form.KeyDown(Key, KeyChar, Shift);
    except
      Application.HandleException(Window.Form);
    end;
end;

procedure TWindowManager.KeyUp(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState; KeyDownHandled: Boolean);
var
  Window: TAndroidWindowHandle;
  function HideVKB: Boolean;
  begin
    Result := VirtualKeyboardAndroid.GetVirtualKeyBoardState * [vksVisible] <> [];
    if Result then
    begin
      Key := 0;
      KeyChar := #0;
      Screen.ActiveForm.Focused := nil;
      VirtualKeyboardAndroid.HideVirtualKeyboard
    end
  end;
begin
  Window := FindTopWindow;
  if Assigned(Window) then
    try
      Window.Form.KeyUp(Key, KeyChar, Shift);
    except
      Application.HandleException(Window.Form);
    end;
  // some actions by default
  if KeyDownHandled then // If you press of key was processed
    case Key of
      vkHardwareBack: HideVKB;
    end
  else // If you press of key wasn't processed
    case Key of
      vkHardwareBack:
        begin
          if (not HideVKB) and Assigned(Window) then
          begin
            try
              Window.Form.Close;
            except
              Application.HandleException(Window.Form);
            end;
            if (FVisibleStack.Count > 0) and (FVisibleStack.Peek = Window) then
                FVisibleStack.Pop;
            if (FVisibleStack.Count > 0) and (FVisibleStack.Peek <> nil) then
            begin
              BringToFront(FVisibleStack.Peek);
              Key := 0;
              KeyChar := #0;
            end;
          end;
        end
    end
end;

procedure TWindowManager.ClosePopups(SavedWindow: TAndroidWindowHandle);
begin
  if Assigned(Screen) then
  begin
    if Assigned(SavedWindow) then
      Screen.ClosePopupForms(SavedWindow.Form)
    else
      Screen.ClosePopupForms(nil);
  end;
end;

procedure TWindowManager.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  Window: TAndroidWindowHandle;
  ClientPoint: TPointF;
  Obj: IControl;
begin
  Window := FindWindowByPoint(X, Y);
  if Assigned(Window) then
  begin
    ClosePopups(Window);
    ClientPoint := ScreenToClient(Window, TPointF.Create(X, Y));
    try
      Window.Form.MouseMove(Shift, ClientPoint.X, ClientPoint.Y); // Require for correct IsMouseOver handle
      Window.Form.MouseDown(TMouseButton.mbLeft, Shift, ClientPoint.X, ClientPoint.Y);
    except
      Application.HandleException(Window.Form);
    end;
    // find the control from under the gesture
    Obj := IControl(Window.Form.ObjectAtPoint(Window.Form.ClientToScreen(ClientPoint)));
    if Assigned(Obj) then
      FGestureControl := Obj.GetObject
    else
      FGestureControl := Window.Form;

    HideContextMenu;

    while Assigned(FGestureControl) and
      not Assigned(FGestureControl.Touch.GestureEngine) do
      FGestureControl := FGestureControl.Parent;

    if Assigned(FGestureControl) then
      if Assigned(FGestureControl.Touch.GestureEngine) then
      begin
        TPlatformGestureEngine(FGestureControl.Touch.GestureEngine).InitialPoint := ClientPoint;

        // Retain the points/touches.
        TPlatformGestureEngine(FGestureControl.Touch.GestureEngine).ClearPoints;
        TPlatformGestureEngine(FGestureControl.Touch.GestureEngine).AddPoint(ClientPoint.X, ClientPoint.Y);
      end;
  end;
end;

procedure TWindowManager.MouseMove(Shift: TShiftState; X, Y: Single);
var
  Window: TAndroidWindowHandle;
  ClientPoint: TPointF;
begin
  if Assigned(FCapturedWindow) then
    Window := FCapturedWindow
  else
    Window := FindWindowByPoint(X, Y);
  if Assigned(Window) then
  begin
    ClientPoint := ScreenToClient(Window, TPointF.Create(X, Y));
    try
      Window.Form.MouseMove(Shift, ClientPoint.X, ClientPoint.Y);
    except
      Application.HandleException(Window.Form);
    end;
    if Assigned(FGestureControl) then
      if Assigned(FGestureControl.Touch.GestureEngine) then
      begin
        TPlatformGestureEngine(FGestureControl.Touch.GestureEngine).AddPoint(ClientPoint.X, ClientPoint.Y);
      end;
  end;
end;

procedure TWindowManager.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single; DoCLick: Boolean);
var
  Window: TAndroidWindowHandle;
  ClientPoint: TPointF;
  EventInfo: TGestureEventInfo;
const
  LGestureTypes: TGestureTypes = [TGestureType.gtStandard, TGestureType.gtRecorded, TGestureType.gtRegistered];
begin
  if Assigned(FCapturedWindow) then
    Window := FCapturedWindow
  else
    Window := FindWindowByPoint(X, Y);
  if Assigned(Window) then
  begin
    ClientPoint := ScreenToClient(Window, TPointF.Create(X, Y));
    try
      Window.Form.MouseUp(TMouseButton.mbLeft, Shift, ClientPoint.X, ClientPoint.Y, DoClick);
      if Window.Form <> nil then
        Window.Form.MouseLeave; // Require for correct IsMouseOver handle
    except
      Application.HandleException(Window.Form);
    end;
    if Assigned(FGestureControl) then
      if Assigned(FGestureControl.Touch.GestureEngine) then
      begin
        if TPlatformGestureEngine(FGestureControl.Touch.GestureEngine).PointCount > 1 then
        begin
          FillChar(EventInfo, Sizeof(EventInfo), 0);
          if TPlatformGestureEngine.IsGesture
            (TPlatformGestureEngine(FGestureControl.Touch.GestureEngine).Points,
            TPlatformGestureEngine(FGestureControl.Touch.GestureEngine).GestureList, LGestureTypes, EventInfo) then
          begin
            TPlatformGestureEngine(FGestureControl.Touch.GestureEngine).BroadcastGesture(FGestureControl, EventInfo);
          end;
        end;
      end;
  end;
end;

procedure TWindowManager.PasteMenuTimerCall;
begin
  DestroyPasteMenuTimer;
  //
  HideContextMenu;
end;

function TWindowManager.PixelToPoint(const P: TPointF): TPointF;
begin
  Result := TPointF.Create(P.X / FScale, P.Y / FScale);
end;

function TWindowManager.PointToPixel(const P: TPointF): TPointF;
begin
  Result := TPointF.Create(P.X * FScale, P.Y * FScale);
end;

procedure TWindowManager.ReleaseCapture;
begin
  FCapturedWindow := nil;
end;

procedure TWindowManager.RemoveWindow(const AHandle: TAndroidWindowHandle);
begin
  if FVisibleStack.Peek = AHandle then
    FVisibleStack.Pop;
  FWindows.Remove(AHandle);
  SetNeedsRender;
end;

function TWindowManager.AlignToPixel(const Value: Single): Single;
begin
  Result := Round(Value * Scale) / Scale;
end;

procedure TWindowManager.Render;

  procedure ContextInit;
  begin
    eglMakeCurrent(eglGetCurrentDisplay, TCustomAndroidContext.SharedSurface, TCustomAndroidContext.SharedSurface,
      TCustomAndroidContext.SharedContext);

    glViewport(Round(FContentRect.Left * FScale), Round(FContentRect.Top * FScale), Round(FContentRect.Width * FScale),
     Round(FContentRect.Height * FScale));

    glDepthMask(1);
    glClearDepthf(1);
    glClearStencil(0);
    glClearColor(0, 0, 0, 0);
    glClear(GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT or GL_COLOR_BUFFER_BIT);
  end;

  procedure ContextFlip;
  begin
    eglSwapBuffers(TCustomAndroidContext.SharedDisplay, TCustomAndroidContext.SharedSurface);
  end;

  function HaveAnyPopupWindowsVisible: Boolean;
  var
    I: Integer;
  begin
    for I := 0 to FWindows.Count - 1 do
      if FWindows[I].Form.Visible and FWindows[I].IsPopup then
        Exit(True);

    Result := False;
  end;

  procedure RenderNormalWindows;
  var
    I: Integer;
    PaintControl: IPaintControl;
  begin
    for I := FWindows.Count - 1 downto 0 do
      if FWindows[I].Form.Visible and (not FWindows[I].IsPopup) and Supports(FWindows[I].Form, IPaintControl,
        PaintControl) then
      begin
        PaintControl.PaintRects([TRectF.Create(0, 0, FContentRect.Width, FContentRect.Height)]);

        // Stop at first encountered non-popup form, which is occluding others.
        if not FWindows[I].IsPopup then
          Break;
      end;
  end;

  procedure RenderPopupWindows;
  var
    I: Integer;
    Mat: TCanvasTextureMaterial;
    Ver: TVertexBuffer;
    Ind: TIndexBuffer;
    CurrentForm: TAndroidWindowHandle;
    FormBounds: TRectF;
    Context: TContext3D;
  begin
    for I := FWindows.Count - 1 downto 0 do
    begin
      CurrentForm := FWindows[I];

      if CurrentForm.IsPopup and Assigned(CurrentForm.Texture) and CurrentForm.Form.Visible then
      begin
        FormBounds := CurrentForm.Bounds;
        if CurrentForm.NeedsUpdate then
        begin
          IPaintControl(CurrentForm.Form).PaintRects([RectF(0, 0, FormBounds.Width, FormBounds.Height)]);
          CurrentForm.NeedsUpdate := False;
        end;
      end;
    end;

    Context := TCustomAndroidContext.CreateContextFromActivity(Round(FContentRect.Width * FScale),
      Round(FContentRect.Height * FScale), TMultisample.msNone, False);
    if Context.BeginScene then
    try
      glViewport(Round(FContentRect.Left * FScale), Round(FContentRect.Top * FScale), Round(FContentRect.Width * FScale),
        Round(FContentRect.Height * FScale));

      Ver := TVertexBuffer.Create([TVertexFormat.vfVertex, TVertexFormat.vfTexCoord0, TVertexFormat.vfColor0], 4);
      Ver.Color0[0] := $FFFFFFFF;
      Ver.Color0[1] := $FFFFFFFF;
      Ver.Color0[2] := $FFFFFFFF;
      Ver.Color0[3] := $FFFFFFFF;
      Ver.TexCoord0[0] := PointF(0.0, 1.0);
      Ver.TexCoord0[1] := PointF(1.0, 1.0);
      Ver.TexCoord0[2] := PointF(1.0, 0.0);
      Ver.TexCoord0[3] := PointF(0.0, 0.0);

      Ind := TIndexBuffer.Create(6);
      Ind[0] := 0;
      Ind[1] := 1;
      Ind[2] := 3;
      Ind[3] := 3;
      Ind[4] := 1;
      Ind[5] := 2;

      Mat := TCanvasTextureMaterial.Create;

      for I := FWindows.Count - 1 downto 0 do
      begin
        CurrentForm := FWindows[I];

        if CurrentForm.IsPopup and Assigned(CurrentForm.Texture) and CurrentForm.Form.Visible then
        begin
          FormBounds := CurrentForm.Bounds;

          FormBounds.Left := Round(FormBounds.Left * Scale);
          FormBounds.Top := Round(FormBounds.Top * Scale);
          FormBounds.Right := FormBounds.Left + CurrentForm.Texture.Width;
          FormBounds.Bottom := FormBounds.Top + CurrentForm.Texture.Height;

          Ver.Vertices[0] := TPoint3D.Create(FormBounds.Left, FormBounds.Top, 0);
          Ver.Vertices[1] := TPoint3D.Create(FormBounds.Right, FormBounds.Top, 0);
          Ver.Vertices[2] := TPoint3D.Create(FormBounds.Right, FormBounds.Bottom, 0);
          Ver.Vertices[3] := TPoint3D.Create(FormBounds.Left, FormBounds.Bottom, 0);

          Mat.Texture := CurrentForm.Texture;

          Context.SetMatrix(TMatrix3D.Identity);
          Context.SetContextState(TContextState.cs2DScene);
          Context.SetContextState(TContextState.csZWriteOff);
          Context.SetContextState(TContextState.csZTestOff);
          Context.SetContextState(TContextState.csAllFace);
          Context.SetContextState(TContextState.csScissorOff);
          if CurrentForm.Form.Transparency then
            Context.SetContextState(TContextState.csAlphaBlendOn)
          else
            Context.SetContextState(TContextState.csAlphaBlendOff);

          Context.DrawTriangles(Ver, Ind, Mat, 1);
        end;
      end;

      Mat.Free;
      Ind.Free;
      Ver.Free;
    finally
      Context.EndScene;
    end;
    Context.Free;
  end;

begin
  if TCustomAndroidContext.IsContextAvailable and not FPause then
  begin
    ContextInit;
    try
      // Render normal windows that are occupying entire client space.
      RenderNormalWindows;

      // If there are any visible popups, render them using buffered texture.
      if HaveAnyPopupWindowsVisible then
        RenderPopupWindows;
    finally
      ContextFlip;
    end;
  end;
  FNeedsRender := False;
end;

function TWindowManager.RenderIfNeeds: Boolean;
begin
  Result := FNeedsRender;

  if FNeedsRender then
    Render;
end;

procedure TWindowManager.RenderImmediately;
begin
  SetNeedsRender;
  RenderIfNeeds;
end;

procedure TWindowManager.SetCapture(const AForm: TAndroidWindowHandle);
begin
  FCapturedWindow := AForm;
end;

procedure TWindowManager.SetNeedsRender;
begin
  FNeedsRender := True;
end;

procedure TWindowManager.SetPause(const Value: Boolean);
begin
  FPause := Value;
end;

procedure TWindowManager.ShowContextMenu(const ItemsToShow: TContextMenuItems);
var
  LA: TTextLayout;
  P: TPoint;
  HasSelection, HasClipboard: Boolean;
  ApproxWidth: Integer;
  ApproxHeight: Integer;
  ClipboardValue: TValue;
  ResID: Integer;
begin
  DestroyPasteMenuTimer;
  ApproxWidth := FContextMenuPopupSize.cx;
  ApproxHeight := FContextMenuPopupSize.cy;
  if not FContextMenuVisible and Assigned(FTextInput) and not FSelectionInProgress then
  begin
    FContextMenuVisible := True;
    HasSelection := not FTextInput.GetSelection.IsEmpty;
    ClipboardValue := PlatformAndroid.GetClipboard;
    HasClipboard := not ClipboardValue.IsEmpty and not ClipboardValue.ToString.IsEmpty;
    if not Assigned(FContextMenuPopup) then
    begin
      FContextMenuLayout := TJLinearLayout.JavaClass.init(SharedActivity);
      FContextButtonsLayout := TJLinearLayout.JavaClass.init(SharedActivity);

      LA := TTextLayoutManager.DefaultTextLayout.Create;
      LA.Font.Style := LA.Font.Style + [TFontStyle.fsBold];

      P := Point(0, 0);

      if HasSelection then
      begin
        //Copy button
        if cmiCopy in ItemsToShow then
        begin
          ResID := GetResourceID('android:string/copy');
          if ResID <> 0 then
            LA.Text := GetResourceString(ResID)
          else
            LA.Text := SEditCopy.ToUpper;
          FCopyButton := TJButton.JavaClass.init(SharedActivity);
          if ResID <> 0 then
            FCopyButton.setText(ResID)
          else
            FCopyButton.setText(StrToJCharSequence(LA.Text));
          FCopyButton.setTypeface(TJTypeface.JavaClass.DEFAULT_BOLD);
          FCopyClickListener := TCopyButtonClickListener.Create;
          FCopyButton.setOnClickListener(FCopyClickListener);
          LA.Font.Size := FCopyButton.getTextSize;
          P.X := P.X + Ceil((LA.TextWidth + 2) * FScale);
          P.Y := Max(P.Y, Ceil((LA.TextHeight + 2) * FScale));
          ApproxHeight := P.Y + FCopyButton.getPaddingTop + FCopyButton.getPaddingBottom;
        end;
        //Cut button
        if cmiCut in ItemsToShow then
        begin
          ResID := GetResourceID('android:string/cut');
          if ResID <> 0 then
            LA.Text := GetResourceString(ResID)
          else
            LA.Text := SEditCut.ToUpper;
          FCutButton := TJButton.JavaClass.init(SharedActivity);
          if ResID <> 0 then
            FCutButton.setText(ResID)
          else
            FCutButton.setText(StrToJCharSequence(LA.Text));
          FCutButton.setTypeface(TJTypeface.JavaClass.DEFAULT_BOLD);
          FCutClickListener := TCutButtonClickListener.Create;
          FCutButton.setOnClickListener(FCutClickListener);
          LA.Font.Size := FCopyButton.getTextSize;
          P.X := P.X + Ceil((LA.TextWidth + 2) * FScale);
          P.Y := Max(P.Y, Ceil((LA.TextHeight + 2) * FScale));
        end;
      end;

      if HasClipboard and (cmiPaste in ItemsToShow) then
      begin
        //Paste button
        ResID := GetResourceID('android:string/paste');
        if ResID <> 0 then
          LA.Text := GetResourceString(ResID)
        else
          LA.Text := SEditPaste.ToUpper;
        FPasteButton := TJButton.JavaClass.init(SharedActivity);
        if ResID <> 0 then
          FPasteButton.setText(ResID)
        else
          FPasteButton.setText(StrToJCharSequence(LA.Text));
        FPasteButton.setTypeface(TJTypeface.JavaClass.DEFAULT_BOLD);
        FPasteClickListener := TPasteButtonClickListener.Create;
        FPasteButton.setOnClickListener(FPasteClickListener);
        LA.Font.Size := FPasteButton.getTextSize;
        P.X := P.X + Ceil((LA.TextWidth + 2) * FScale);
        P.Y := Max(P.Y, Ceil((LA.TextHeight + 2) * FScale));
        if ApproxHeight = 0 then
          ApproxHeight := P.Y + FPasteButton.getPaddingTop + FPasteButton.getPaddingBottom;
      end;

      ApproxWidth := P.X;

      FContextMenuPopup := TJPopupWindow.JavaClass.init(SharedActivity);
      FContextMenuPopup.setBackgroundDrawable(TJColorDrawable.JavaClass.init(0));

      FContextMenuPopup.setContentView(FContextButtonsLayout);
      FContextMenuPopup.setWidth(TJViewGroup_LayoutParams.JavaClass.WRAP_CONTENT);
      FContextMenuPopup.setHeight(TJViewGroup_LayoutParams.JavaClass.WRAP_CONTENT);
    end;

    FContextMenuPopupSize.cx := ApproxWidth;
    if FContextMenuPopupSize.cy <= 0 then
    begin
      FContextMenuPopupSize.cy := ApproxHeight;
    end;

    PlatformAndroid.SynchronizeOnUIThread(
      procedure
      var
        SelRect: TRectF;
        PopupLoc: TPoint;
        TempPointF: TPointF;
      begin
        if Assigned(FCopyButton) then
          FContextButtonsLayout.addView(FCopyButton);
        if Assigned(FCutButton) then
          FContextButtonsLayout.addView(FCutButton);
        if Assigned(FPasteButton) then
          FContextButtonsLayout.addView(FPasteButton);
        if Assigned(FFocusedControl) and Assigned(FContextMenuLayout) and Assigned(FContextMenuPopup) and (TWindowManager.Current.FContextButtonsLayout.getChildCount > 0) then
        begin
          SelRect := FTextInput.GetSelectionRect;
          TempPointF := FFocusedControl.AsIControl.LocalToScreen(SelRect.TopLeft) * FScale;

          TempPointF.Offset(SelRect.Width * FScale / 2 - FContextMenuPopupSize.cx / 2, -FContextMenuPopupSize.cy);
          PopupLoc := TempPointF.Round;
          if PopupLoc.Y < StatusBarHeight then
          begin
            TempPointF := FFocusedControl.AsIControl.LocalToScreen(TPointF.Create(SelRect.Left, SelRect.Bottom)) * FScale;
            TempPointF.Offset(SelRect.Width * FScale / 2 - FContextMenuPopupSize.cx / 2, FContextMenuPopupSize.cy);
            PopupLoc := TempPointF.Round;
          end;
          FContextMenuPopup.showAtLocation(FContextMenuLayout, 0, PopupLoc.X, PopupLoc.Y);
        end
        else
          HideContextMenu;
      end);
  end;
end;

procedure TWindowManager.SingleTap(const APoint: TPointF);
var
  Window: TAndroidWindowHandle;
  ClientPoint: TPointF;
  EventInfo: TGestureEventInfo;
  Obj: IControl;
  TapControl: TFmxObject;
  TextInput: ITextInput;
begin
  if Assigned(FCapturedWindow) then
    Window := FCapturedWindow
  else
    Window := FindWindowByPoint(APoint.X, APoint.Y);
  if Assigned(Window) and Assigned(Window.Form) then
  begin
    ClientPoint := ScreenToClient(Window, APoint);
    try
      if not FSelectionInProgress then
        if Assigned(Window.Form) then
        begin
          Obj := Window.Form.ObjectAtPoint(Window.Form.ClientToScreen(ClientPoint));
          if Assigned(Obj) then
            TapControl := Obj.GetObject
          else
            TapControl := nil;
          if Assigned(TapControl) and Supports(TapControl, ITextInput, FTextInput)  and Supports(TapControl, ITextActions, FTextActions) then
          begin
            FFocusedControl := TapControl;
            ShowContextMenu([cmiPaste]);
            CreatePasteMenuTimer;
          end
          else
            HideContextMenu
        end
        else
          HideContextMenu;
    except
      Application.HandleException(Window.Form);
    end;
  end;
end;

procedure TWindowManager.HideContextMenu;
begin
  DestroyPasteMenuTimer;
  if FContextMenuVisible and Assigned(FContextMenuPopup) then
  begin
    PlatformAndroid.SynchronizeOnUIThread(
      procedure begin
        FContextMenuPopupSize.cx := FContextButtonsLayout.getWidth;
        FContextMenuPopupSize.cy := FContextButtonsLayout.getHeight;
        FContextMenuPopup.dismiss;
      end);
    FContextMenuPopup := nil;
    FCopyButton := nil;
    FCopyClickListener := nil;
    FPasteButton := nil;
    FPasteClickListener := nil;
    FCutButton := nil;
    FCutClickListener := nil;
    FContextMenuLayout := nil;
    FContextButtonsLayout := nil;
  end;
  FContextMenuVisible := False;
end;

function TWindowManager.RetreiveContentRect: TRect;
var
  Activity: JActivity;
  NativeWin: JWindow;
  DecorView: JView;
  ContentRect: JRect;
begin
  Activity := SharedActivity;
  if Assigned(Activity) then
  begin
    NativeWin := Activity.getWindow;
    if Assigned(NativeWin) then
    begin
      ContentRect := TJRect.Create;

      DecorView := NativeWin.getDecorView;
      DecorView.getWindowVisibleDisplayFrame(ContentRect);
      FStatusBarHeight := ContentRect.top;

      DecorView.getDrawingRect(ContentRect);

      Result := TRect.Create(Round(ContentRect.left / FScale), Round((ContentRect.top + StatusBarHeight) / FScale),
        Round(ContentRect.right / FScale), Round(ContentRect.bottom / FScale));
    end;
  end;
end;

procedure TWindowManager.UpdateContentRect;
begin
  FContentRect := RetreiveContentRect;
end;

procedure TWindowManager.UpdateFormSizes;
var
  I: Integer;
  StaticWindowList: TList<TAndroidWindowHandle>;
begin
  StaticWindowList := TList<TAndroidWindowHandle>.Create;
  try
    for I := FWindows.Count - 1 downto 0 do
      if not FWindows[I].IsPopup then
        StaticWindowList.Add(FWindows[I]);

    for I := StaticWindowList.Count - 1 downto 0 do
    begin
      StaticWindowList[I].SetBounds(TRectF.Create(FContentRect));
      try
        StaticWindowList[I].Form.SetBounds(FContentRect.Left, FContentRect.Top, FContentRect.Width, FContentRect.Height);
      except
        Application.HandleException(StaticWindowList[I].Form);
      end;
    end;

    TMessageManager.DefaultManager.SendMessage(Self, TOrientationChangedMessage.Create, True);
  finally
    StaticWindowList.Free;
  end;
end;

function TWindowManager.CheckContentRectUpdate: Boolean;
var
  NewRect: TRect;
begin
  NewRect := RetreiveContentRect;

  if NewRect <> FContentRect then
  begin
    FContentRect := NewRect;

    UpdateFormSizes;
    SetNeedsRender;
    Result := True;
  end
  else
    Result := False;
end;

{$IFDEF ONCONTENTRECT}
procedure OnContentRectChanged(Activity: PANativeActivity; Rect: PARect); cdecl;
begin
  TWindowManager.Current.CheckContentRectUpdate;
end;
{$ENDIF}

{ TPlatformAndroid }

constructor TPlatformAndroid.Create;
begin
  inherited;
  FMotionEvents := TMotionEvents.Create;
  Application := TApplication.Create(nil);
  PlatformAndroid := Self;
  TWindowManager.Current;
  TTimerManager.Current;
  FFirstRun := True;
  FLogPrefix := 'FMX: ' + GetApplicationTitle + ': ';

  FActivityListener := TFMXNativeActivityListener.Create;
  TJFMXNativeActivity.Wrap(PANativeActivity(System.DelphiActivity)^.clazz).setListener(FActivityListener);
{$IFDEF ONCONTENTRECT}
  PANativeActivity(System.DelphiActivity)^.callbacks^.onContentRectChanged := OnContentRectChanged;
{$ENDIF}

  FActiveInteractiveGestures := [];
  FEnabledInteractiveGestures := [];
  FOldPoint1 := PointF(0.0, 0.0);
  FOldPoint2 := PointF(0.0, 0.0);

  FDblClickFirstMouseUp := False;
  FSingleTap := False;
  if TOSVersion.Check(3, 0) then
    FKeyCharacterMap := TJKeyCharacterMap.JavaClass.load(TJKeyCharacterMap.JavaClass.VIRTUAL_KEYBOARD)
  else
    FKeyCharacterMap := TJKeyCharacterMap.JavaClass.load(TJKeyCharacterMap.JavaClass.BUILT_IN_KEYBOARD);
  FRotationAngle := 0;
  FInternalEventQueue := TQueue<TThreadProcedure>.Create;
  FEventQueueEmptyEvent := TEvent.Create;
  FSkipEventQueue := TQueue<JKeyEvent>.Create;

  OrientationMightHaveChanged := False;
end;

destructor TPlatformAndroid.Destroy;
begin
  // Cannot use MainActivity here because if we are here PlatformAndroid is nil
  TJFMXNativeActivity.Wrap(AndroidApp^.activity.clazz).setListener(nil);
  FActivityListener.DisposeOf;
  DestroyDoubleTapTimer;
  DestroyLongTapTimer;
  DestroySingleTapTimer;
  FInternalEventQueue := nil;
  FMotionEvents.Free;
  FreeAndNil(FSkipEventQueue);
  inherited;
end;

procedure TPlatformAndroid.CreateDoubleTapTimer;
begin
  if FDoubleTapTimer = 0 then
    FDoubleTapTimer := TTimerManager.Current.CreateTimer(DBL_TAP_DELAY, DoubleTapTimerCall);
end;

function TPlatformAndroid.CreateGestureEventInfo(ASecondPointer: TPointF;
  const AGesture: TInteractiveGesture;
  const AGestureEnded: Boolean = False): TGestureEventInfo;
begin
  FillChar(Result, Sizeof(Result), 0);
  Result.Location := FMouseCoord;
  Result.GestureID := igiZoom + Ord(AGesture);

  if not(AGesture in FActiveInteractiveGestures) then
    Result.Flags := [TInteractiveGestureFlag.gfBegin];
  if AGestureEnded then
    Result.Flags := [TInteractiveGestureFlag.gfEnd];

  if AGesture = TInteractiveGesture.igZoom then
  begin
    if AGestureEnded and ASecondPointer.IsZero then
      ASecondPointer := FOldPoint2;

    Result.Location := FMouseCoord.MidPoint(ASecondPointer);
    Result.Distance := Round(FMouseCoord.Distance(ASecondPointer));
  end;
  if AGesture = TInteractiveGesture.igRotate then
  begin
    if AGestureEnded and ASecondPointer.IsZero then
      ASecondPointer := FOldPoint2;
    Result.Location := FMouseCoord.MidPoint(ASecondPointer);
    Result.Angle := FRotationAngle;
  end;
  if AGesture = TInteractiveGesture.igPan then
  begin
    if not ASecondPointer.IsZero then
      Result.Distance := Round(FMouseCoord.Distance(ASecondPointer));
  end;
  if AGesture = TInteractiveGesture.igLongTap then
    Result.Location := FMouseDownCoordinates;
end;

{ IFMXDeviceService }

function TPlatformAndroid.GetModel: string;
begin
  Result := JStringToString(TJBuild.JavaClass.MODEL);
end;

function TPlatformAndroid.GetMousePos: TPointF;
begin
  Result := FMouseCoord;
end;

procedure TPlatformAndroid.CreateLongTapTimer;
begin
  if FLongTapTimer = 0 then
    FLongTapTimer := TTimerManager.Current.CreateTimer(LONG_TAP_DURATION, LongTapTimerCall);
end;

procedure TPlatformAndroid.CreateSingleTapTimer;
begin
  if FSingleTapTimer = 0 then
    FSingleTapTimer := TTimerManager.Current.CreateTimer(SINGLE_TAP_DELAY, SingleTapTimerCall);
end;

procedure TPlatformAndroid.DestroyDoubleTapTimer;
begin
  if FDoubleTapTimer <> 0 then
    TTimerManager.Current.DestroyTimer(FDoubleTapTimer);
  FDoubleTapTimer := 0;
  FDblClickFirstMouseUp := False;
end;

procedure TPlatformAndroid.DestroyLongTapTimer;
begin
  if FLongTapTimer <> 0 then
    TTimerManager.Current.DestroyTimer(FLongTapTimer);
  FLongTapTimer := 0;
end;

procedure TPlatformAndroid.DestroySingleTapTimer;
begin
  if FSingleTapTimer <> 0 then
    TTimerManager.Current.DestroyTimer(FSingleTapTimer);
  FSingleTapTimer := 0;
  FSingletap := False;
end;

function TPlatformAndroid.FindAndHandleInteractiveGesture(const ALookIn: TInteractiveGestures; const APoint: TPointF): Boolean;
begin
   Result := False;
    if ALookIn <> [] then
    begin
      if TInteractiveGesture.igZoom in ALookIn then
        if not(FMouseCoord = APoint) and IsZoom(FMouseCoord, APoint) then
          Exit(True);

      if TInteractiveGesture.igRotate in ALookIn then
        if not (FMouseCoord = APoint) and IsRotate(FMouseCoord, APoint) then
          Exit(True);

      if TInteractiveGesture.igPan in ALookIn then
        if (FMouseCoord.Distance(FOldPoint1) > GetLongTapAllowedMovement) and (APoint.Distance(FOldPoint2) > GetLongTapAllowedMovement) and
          TWindowManager.Current.SendCMGestureMessage(CreateGestureEventInfo(APoint, TInteractiveGesture.igPan)) then
          begin
            FActiveInteractiveGestures := FActiveInteractiveGestures + [TInteractiveGesture.igPan];
            Exit(True);
          end;
    end;
end;

procedure TPlatformAndroid.Run;
begin
{
  Log('[MAIN LOOP] Calling InitApp', []);
  AndroidApp := InitApp;
  if Assigned(AndroidApp) then
  begin
    Log('[MAIN LOOP] App assigned: AndroidApp = $%d', [NativeInt(AndroidApp)]);
    AndroidApp^.onAppCmd := @FMX.Platform.Android.HandleAndroidCmd;
    AndroidApp^.onInputEvent := @FMX.Platform.Android.HandleAndroidInputEvent;
  end
  else
    raise Exception.Create('Android Application object not assigned.');

  if TWindowManager.Current.Pause then
    TWindowManager.Current.Pause := False;

  if not TCustomAndroidContext.IsContextAvailable then
  begin
    Log('[MAIN LOOP] Unfreezing Context', []);
    TCustomAndroidContext.UnfreezeSharedContext;
    if TCustomAndroidContext.IsContextAvailable then
      Log('....context available', [])
    else
      Log('....NO context available', []);
  end;
}
  { MAIN LOOP, read events forever }
  while True do
  begin
    WaitMessage;

    { Check if we are exiting. }
    if (AndroidApp^.destroyRequested <> 0) then
      Break;
  end;
end;

procedure TPlatformAndroid.Terminate;
begin
//  ANativeActivity_finish(System.DelphiActivity);
end;

procedure TPlatformAndroid.HandleAndroidCmd(ACmd: Int32);
begin
  case ACmd of
    APP_CMD_INPUT_CHANGED: ;
    APP_CMD_INIT_WINDOW:
      begin
        if FFirstRun then
        begin
          Application.RealCreateForms;
          FFirstRun := False;
          HandleApplicationEvent(TApplicationEvent.aeFinishedLaunching, nil);
        end
        else
          TCustomAndroidContext.UnfreezeSharedContext;
      end;
    APP_CMD_TERM_WINDOW:
      begin
        TCustomAndroidContext.FreezeSharedContext;
      end;
    APP_CMD_WINDOW_RESIZED: ;
    APP_CMD_WINDOW_REDRAW_NEEDED: ;
    APP_CMD_CONTENT_RECT_CHANGED: ;
    APP_CMD_GAINED_FOCUS:
      begin
        TWindowManager.Current.RenderImmediately;
        HandleApplicationEvent(TApplicationEvent.aeBecameActive, nil);
      end;
    APP_CMD_LOST_FOCUS:
      begin
        HandleApplicationEvent(TApplicationEvent.aeWillBecomeInactive, nil);
      end;
    APP_CMD_CONFIG_CHANGED:
      begin
        OrientationMightHaveChanged := True;
      end;
    APP_CMD_LOW_MEMORY:
      begin
        HandleApplicationEvent(TApplicationEvent.aeLowMemory, nil);
      end;
    APP_CMD_START:
      begin
      end;
    APP_CMD_RESUME:
      begin
        TWindowManager.Current.Pause := False;
        HandleApplicationEvent(TApplicationEvent.aeWillBecomeForeground, nil);
      end;
    APP_CMD_SAVE_STATE:
	  begin
        HandleApplicationEvent(TApplicationEvent.aeWillBecomeInactive, nil);
	  end;
    APP_CMD_PAUSE:
      begin
        TWindowManager.Current.Pause := True;
        HandleApplicationEvent(TApplicationEvent.aeEnteredBackground, nil);
      end;
    APP_CMD_STOP:
      begin
      end;
    APP_CMD_DESTROY:
      begin
        HandleApplicationEvent(TApplicationEvent.aeWillTerminate, nil);
        Halt;
      end;
  end;
end;

function TPlatformAndroid.ShiftStateFromMetaState(const AMetaState: Integer): TShiftState;
begin
  Result := [];
  if (AMetaState and AMETA_SHIFT_ON) > 0 then
    Result := Result + [ssShift];
  if (AMetaState and AMETA_ALT_ON) > 0 then
    Result := Result + [ssAlt];
end;

function TPlatformAndroid.HandleAndroidKeyEvent(AEvent: PAInputEvent): Int32;
var
  KeyCode, vkKeyCode: Word;
  Action, MetaState: Integer;
  KeyChar: Char;
  KeyEvent: JKeyEvent;
  KeyEventChars: string;
  C: WideChar;
  SkipEvent: JKeyEvent;
  LKeyDownHandled: Boolean;
  KeyKind: TKeyKind;
  EventTime, DownTime: Int64;
begin
  Result := 0;

  Action := AKeyEvent_getAction(AEvent);
  KeyCode := AKeyEvent_getKeyCode(AEvent);
  MetaState := AKeyEvent_getMetaState(AEvent);
  EventTime := AKeyEvent_getEventTime(AEvent) div 1000000;
  DownTime := AKeyEvent_getDownTime(AEvent) div 1000000;
  if FSkipEventQueue.Count > 0 then
    SkipEvent := FSkipEventQueue.Peek
  else
    SkipEvent := nil;

  if Assigned(SkipEvent) and ((SkipEvent.getEventTime < EventTime) or (SkipEvent.getDownTime < DownTime)) then
  begin
    SkipEvent := nil;
    FSkipEventQueue.Dequeue;
  end;


  if not Assigned(SkipEvent) or (SkipEvent.getAction <> Action) or (SkipEvent.getFlags <> AKeyEvent_getFlags(AEvent)) or
    (SkipEvent.getKeyCode <> KeyCode) or (SkipEvent.getMetaState <> MetaState) or (SkipEvent.getEventTime <> EventTime) or
    (SkipEvent.getDownTime <> DownTime) then
  begin
    KeyChar := #0;

    vkKeyCode := PlatformKeyToVirtualKey(KeyCode, KeyKind);
    if (vkKeyCode <> 0) and (KeyKind <> TKeyKind.kkUsual) then
    begin
      KeyCode := vkKeyCode;
      if KeyCode in [vkEscape] then
        KeyChar := Char(KeyCode);
    end
    else
    begin
      if Assigned(FKeyCharacterMap) then
      begin
        KeyChar := Char(FKeyCharacterMap.get(KeyCode, MetaState));
        if KeyChar <> #0 then
          KeyCode := 0
        else
          KeyCode := vkKeyCode;
      end;
    end;

    case AKeyEvent_getAction(AEvent) of
      AKEY_EVENT_ACTION_DOWN:
        begin
          FDownKey := KeyCode;
          FDownKeyChar := KeyChar;
          TWindowManager.Current.KeyDown(KeyCode, KeyChar, ShiftStateFromMetaState(MetaState));
          FKeyDownHandled := (KeyCode = 0) and (KeyChar = #0);
          if KeyCode = 0 then
            Result := 1;
        end;
      AKEY_EVENT_ACTION_UP:
        begin
          LKeyDownHandled := (FDownKey = KeyCode) and (FDownKeyChar = KeyChar) and FKeyDownHandled;
          TWindowManager.Current.KeyUp(KeyCode, KeyChar, ShiftStateFromMetaState(MetaState), LKeyDownHandled);
          if KeyCode = 0 then
            Result := 1; // indicate that we have handled the event
        end;
      AKEY_EVENT_ACTION_MULTIPLE:
        begin
          KeyEvent := JFMXNativeActivity(MainActivity).getLastEvent;
          if Assigned(KeyEvent) then
          begin
            KeyEventChars := JStringToString(KeyEvent.getCharacters);
            KeyCode := 0;
            for C in KeyEventChars do
            begin
              FDownKey := KeyCode;
              FDownKeyChar := C;
              TWindowManager.Current.KeyDown(KeyCode, FDownKeyChar, ShiftStateFromMetaState(MetaState));
              FKeyDownHandled := (KeyCode = 0) and (FDownKeyChar = #0);
            end;
            Result := 1;
          end;
        end;
    end;
  end
  else
  begin
    FSkipEventQueue.Dequeue;
  end;
end;

procedure TPlatformAndroid.ProcessAndroidGestureEvents;
var
  SecondPointer: TPointF;
begin
  if FMotionEvents.Count < 1 then
    Exit;

  FMouseCoord := FMotionEvents[0].Position;

  if FMotionEvents.Count > 1 then
    SecondPointer := FMotionEvents[1].Position
  else
    SecondPointer := TPointF.Create(0, 0);

  case FMotionEvents[0].EventAction of
    AMOTION_EVENT_ACTION_DOWN:
      begin
        if FSingleTapTimer <> 0 then
          DestroySingleTapTimer
        else
          FSingleTap := True;

        if FDoubleTapTimer = 0 then
          if (TInteractiveGesture.igDoubleTap in FEnabledInteractiveGestures) and (FMotionEvents.Count = 1) then
            CreateDoubleTapTimer;

        if (TInteractiveGesture.igLongTap in FEnabledInteractiveGestures)  and (FMotionEvents.Count = 1) then
          CreateLongTapTimer;
      end;
    AMOTION_EVENT_ACTION_UP:
      begin
        if FSingleTap then
          CreateSingleTapTimer;

        if FDoubleTapTimer <> 0 then
          if not FDblClickFirstMouseUp then
            FDblClickFirstMouseUp := True
          else
          begin
            DestroyDoubleTapTimer;
            FDblClickFirstMouseUp := False;
            TWindowManager.Current.SendCMGestureMessage(CreateGestureEventInfo(TPointF.Create(0, 0),
              TInteractiveGesture.igDoubleTap));
          end;

        //stop longtap timer
        DestroyLongTapTimer;
        //pan gesture ends, if it is active
        if TInteractiveGesture.igPan in FActiveInteractiveGestures then
        begin
          if TWindowManager.Current.SendCMGestureMessage(CreateGestureEventInfo(SecondPointer,
            TInteractiveGesture.igPan, True)) then
            FGestureEnded := True;
          FActiveInteractiveGestures := FActiveInteractiveGestures - [TInteractiveGesture.igPan];
        end;
      end;
    AMOTION_EVENT_ACTION_MOVE:
      begin
        // Stop longtap and double tap timers only if the coordinates did not change (much) since the last event.
        // Allow for some slight finger movement.
        if FMotionEvents.Count = 1 then
        begin
          if FMouseCoord.Distance(FMouseDownCoordinates) > GetLongTapAllowedMovement then
            DestroySingleTapTimer;

          if FMouseCoord.Distance(FOldPoint1) > GetLongTapAllowedMovement then
          begin
            DestroyLongTapTimer;
            DestroyDoubleTapTimer;

            // Since the pointer moved a bit more, we could have a pan.
            if TInteractiveGesture.igPan in FEnabledInteractiveGestures then
            begin
              // pan gesture recognized
              if TWindowManager.Current.SendCMGestureMessage(CreateGestureEventInfo(TPointF.Create(0, 0),
                TInteractiveGesture.igPan)) then
                FActiveInteractiveGestures := FActiveInteractiveGestures + [TInteractiveGesture.igPan];
            end;
          end;

          //end zoom and rotate gestures if they are active
          //sometimes MOTION_EVENT_POINTER_UP is not received
          if TInteractiveGesture.igZoom in FActiveInteractiveGestures then
          begin
            if TWindowManager.Current.SendCMGestureMessage(CreateGestureEventInfo(SecondPointer,
              TInteractiveGesture.igZoom, True)) then
              FGestureEnded := True;
            FActiveInteractiveGestures := FActiveInteractiveGestures - [TInteractiveGesture.igZoom];
          end;
          if TInteractiveGesture.igRotate in FActiveInteractiveGestures then
          begin
            if TWindowManager.Current.SendCMGestureMessage(CreateGestureEventInfo(SecondPointer,
              TInteractiveGesture.igRotate, True)) then
              FGestureEnded := True;
            FActiveInteractiveGestures := FActiveInteractiveGestures - [TInteractiveGesture.igRotate];
            FRotationAngle := 0;
          end;

          FOldPoint2 := TPointF.Create(0, 0);
        end;

        if FMotionEvents.Count = 2 then
        begin
          //sometimes MOTION_EVENT_POINTER_DOWN is not received
          DestroyLongTapTimer;
          DestroyDoubleTapTimer;
          DestroySingleTapTimer;

          //handle already started gestures first
          if not FindAndHandleInteractiveGesture(FActiveInteractiveGestures, SecondPointer) then
            FindAndHandleInteractiveGesture(FEnabledInteractiveGestures - FActiveInteractiveGestures, SecondPointer);
        end;
      end;
    AMOTION_EVENT_ACTION_CANCEL:
      begin
        //stop timers
        DestroyLongTapTimer;
        DestroyDoubleTapTimer;
        DestroySingleTapTimer;
                                       
        FActiveInteractiveGestures := [];
        FRotationAngle := 0;
        //reset FOldPoint values
        FOldPoint1 := TPointF.Create(0, 0);
        FOldPoint2 := TPointF.Create(0, 0);
        FMouseDownCoordinates := TPointF.Create(0, 0);
      end;
    AMOTION_EVENT_ACTION_POINTER_DOWN:
      begin
        //stop timers
        DestroyLongTapTimer;
        DestroyDoubleTapTimer;
        DestroySingleTapTimer;
        if FMotionEvents.Count = 2 then
          FOldPoint2 := SecondPointer;
      end;
    AMOTION_EVENT_ACTION_POINTER_UP:
      begin
        //from 2 pointers now, there will be only 1 pointer
        if FMotionEvents.Count = 2 then
        begin
          if TInteractiveGesture.igZoom in FActiveInteractiveGestures then
          begin
            if TWindowManager.Current.SendCMGestureMessage(CreateGestureEventInfo(SecondPointer,
              TInteractiveGesture.igZoom, True)) then
              FGestureEnded := True;
            FActiveInteractiveGestures := FActiveInteractiveGestures - [TInteractiveGesture.igZoom];
          end;
          if TInteractiveGesture.igRotate in FActiveInteractiveGestures then
          begin
            if TWindowManager.Current.SendCMGestureMessage(CreateGestureEventInfo(SecondPointer,
              TInteractiveGesture.igRotate, True)) then
              FGestureEnded := True;
            FActiveInteractiveGestures := FActiveInteractiveGestures - [TInteractiveGesture.igRotate];
            FRotationAngle := 0;
          end;
          FOldPoint2 := TPointF.Create(0, 0);
        end;
      end;
  end;

  FOldPoint1 := FMotionEvents[0].Position;
  FOldPoint2 := SecondPointer;
end;

procedure TPlatformAndroid.ProcessAndroidMouseEvents;
var
  MotionEvent: TMotionEvent;
begin
  if FMotionEvents.Count > 0 then
  begin
    MotionEvent := FMotionEvents[0];

    case MotionEvent.EventAction of
      AMOTION_EVENT_ACTION_DOWN:
      begin
        TWindowManager.Current.MouseDown(TMouseButton.mbLeft, MotionEvent.Shift, MotionEvent.Position.X, MotionEvent.Position.Y);
        FMouseDownCoordinates := MotionEvent.Position;
      end;

      AMOTION_EVENT_ACTION_UP:
      begin
        TWindowManager.Current.MouseUp(TMouseButton.mbLeft, MotionEvent.Shift, MotionEvent.Position.X, MotionEvent.Position.Y, not FGestureEnded);
        FGestureEnded := False;
        FGestureEnded := False;
      end;

      AMOTION_EVENT_ACTION_MOVE:
        TWindowManager.Current.MouseMove(MotionEvent.Shift, MotionEvent.Position.X, MotionEvent.Position.Y);
    end;

    FMouseCoord := MotionEvent.Position;
  end;
end;

function TPlatformAndroid.HandleAndroidMotionEvent(AEvent: PAInputEvent): Int32;
var
  I: Integer;
  MotionEvent: TMotionEvent;
begin
  Result := 0;

  FMotionEvents.Clear;
  for I := 0 to AMotionEvent_getPointerCount(AEvent) - 1 do
  begin
    MotionEvent.EventAction := AKeyEvent_getAction(AEvent);
    MotionEvent.Position := TWindowManager.Current.PixelToPoint(TPointF.Create(AMotionEvent_getX(AEvent, I),
      AMotionEvent_getY(AEvent, I)));
    MotionEvent.Shift :=[ssLeft];
    if AInputEvent_getType(AEvent) <> AINPUT_SOURCE_MOUSE then
      MotionEvent.Shift := MotionEvent.Shift + [ssTouch];
    FMotionEvents.Add(MotionEvent);
  end;

  if (FActiveInteractiveGestures = []) or (FActiveInteractiveGestures = [TInteractiveGesture.igPan]) then
    ProcessAndroidMouseEvents;
  ProcessAndroidGestureEvents;
end;

function TPlatformAndroid.HandleAndroidInputEvent(AEvent: PAInputEvent): Int32;
var
  EventType: Int64;
begin
  EventType := AInputEvent_getType(AEvent);

  if EventType = AINPUT_EVENT_TYPE_KEY then
  begin // Keyboard input
    Result := HandleAndroidKeyEvent(AEvent);
  end
  else if EventType = AINPUT_EVENT_TYPE_MOTION then
  begin // Motion Event
    Result := HandleAndroidMotionEvent(AEvent);
  end
  else
    Result := 0;
end;

function TPlatformAndroid.HandleMessage: Boolean;
begin
  WaitMessage;
  Result := False;
end;

procedure TPlatformAndroid.WaitMessage;
begin
  InternalWaitMessage;
end;

procedure TPlatformAndroid.InternalWaitMessage(const TimeoutOverride: Integer);

  function InternalProcessPollTimeout: Integer;
  var
    PriorityProcessingEndTime: Extended;
  begin
    Result := PollTimeoutLowPriority;

    if PriorityProcessing then
      Result := 0
    else if PriorityProcessingWait then
      begin
        PriorityProcessingEndTime := GetTick;

        if Abs(PriorityProcessingEndTime - PriorityProcessingStartTime) > PriorityProcessingTime then
        begin
          PriorityProcessingStartTime := PriorityProcessingEndTime;
          PriorityProcessingWait := False;
        end
        else
          Result := PollTimeoutHighPriority;
      end;
  end;

  procedure InternalStartPriorityProcessingWait;
  begin
    PriorityProcessingStartTime := GetTick;
    PriorityProcessingWait := True;
  end;

var
  PollTimeout: Integer;
  IntlPriorityProcessing, RenderPostRotate, Done: Boolean;
  PEventPollSource: Pandroid_poll_source;
  EventPollValue: Integer;
  Events: Integer;
begin
  if TimeoutOverride > 0 then
    PollTimeout := TimeoutOverride
  else
    PollTimeout := InternalProcessPollTimeout;

  { waiting for events:
      -1 means block and wait for event
       0 means not blocking and continue  }
  EventPollValue := ALooper_pollAll(PollTimeout, nil, @Events, PPointer(@PEventPollSource));

  IntlPriorityProcessing := False;

  if EventPollValue = ALOOPER_POLL_ERROR then
    raise Exception.Create('ALOOPER_POLL_ERROR');

  { process the event }
  if Assigned(PEventPollSource) and Assigned(PEventPollSource^.process) then
  begin
    PEventPollSource^.process(AndroidApp, PEventPollSource);
    IntlPriorityProcessing := True;
  end;

  RenderPostRotate := False;

  if OrientationMightHaveChanged then
  begin
    if TWindowManager.Current.CheckContentRectUpdate then
    begin

      OrientationMightHaveChanged := False;
      RenderPostRotate := True;
    end;
  end;

  if TWindowManager.Current.RenderIfNeeds then
    IntlPriorityProcessing := True;

  if (TThread.CurrentThread.ThreadID = MainThreadID) then
    CheckSynchronize;

  if TimeoutOverride < 1 then
  begin
    PriorityProcessing := IntlPriorityProcessing;

    if PriorityProcessing then
      InternalStartPriorityProcessingWait;
  end;

  if RenderPostRotate then
    TWindowManager.Current.SetNeedsRender;

  Done := False;
  if Events = 0 then
    try
      Application.DoIdle(Done);
    except
      Application.HandleException(Application);
    end;
end;

function TPlatformAndroid.Terminating: Boolean;
begin
  Result := False;
end;

{ IFMXTimerService }

function TPlatformAndroid.CreateTimer(Interval: Integer; TimerFunc: TTimerProc): TFmxHandle;
begin
  Result := TTimerManager.Current.CreateTimer(Interval, TimerFunc);
end;

function TPlatformAndroid.DestroyTimer(Timer: TFmxHandle): Boolean;
begin
  TTimerManager.Current.DestroyTimer(Timer);
  Result := True;
end;

function TPlatformAndroid.GetTick: Extended;
var
  Res: timespec;
begin
  clock_gettime(CLOCK_MONOTONIC, @Res);
  Result := (Int64(1000000000) * res.tv_sec + res.tv_nsec) / 1000000000;
end;

procedure TPlatformAndroid.Activate(const AForm: TCommonCustomForm);
var
  BrowserManager : IFMXWBService;
begin  
  if TPlatformServices.Current.SupportsPlatformService(IFMXWBService, IInterface(BrowserManager)) then
    BrowserManager.RealignBrowsers;
end;

function TPlatformAndroid.ClientToScreen(const AForm: TCommonCustomForm;
  const Point: TPointF): TPointF;
begin
  Result := TWindowManager.Current.ClientToScreen(TAndroidWindowHandle(AForm.Handle), Point);
end;

function TPlatformAndroid.ScreenToClient(const AForm: TCommonCustomForm;
  const Point: TPointF): TPointF;
begin
  Result := TWindowManager.Current.ScreenToClient(TAndroidWindowHandle(AForm.Handle), Point);
end;

function TPlatformAndroid.CreateWindow(const AForm: TCommonCustomForm): TWindowHandle;
begin
  Result := nil;
  if Assigned(AForm.Handle) and TWindowManager.Current.Windows.Contains(TAndroidWindowHandle(AForm.Handle)) then
    raise Exception.Create('Window already exists.');

  Result := TAndroidWindowHandle.Create(AForm);
  TWindowManager.Current.AddWindow(TAndroidWindowHandle(Result));
  if not IsPopupForm(AForm) then
    TAndroidWindowHandle(Result).Bounds := TRectF(TWindowManager.Current.ContentRect);
end;

procedure TPlatformAndroid.DestroyWindow(const AForm: TCommonCustomForm);
begin
  if Assigned(AForm.Handle) and TWindowManager.Current.Windows.Contains(TAndroidWindowHandle(AForm.Handle)) then
    TWindowManager.Current.Windows.Remove(TAndroidWindowHandle(AForm.Handle));
end;

procedure TPlatformAndroid.DoubleTapTimerCall;
begin
  //no double tap was made
  DestroyDoubleTapTimer;
end;

procedure TPlatformAndroid.HideWindow(const AForm: TCommonCustomForm);
begin
  if TWindowManager.Current.FVisibleStack.Peek = AForm.Handle then
    TWindowManager.Current.FVisibleStack.Pop;
  if Assigned(AForm.Handle) then
    TWindowManager.Current.SetNeedsRender;
end;

procedure TPlatformAndroid.ReleaseWindow(const AForm: TCommonCustomForm);
begin
  if Assigned(AForm.Handle) and TWindowManager.Current.Windows.Contains(TAndroidWindowHandle(AForm.Handle)) then
    TWindowManager.Current.RemoveWindow(TAndroidWindowHandle(AForm.Handle));
end;

procedure TPlatformAndroid.RemoveRecognizer(const ARec: TInteractiveGesture;
  const AForm: TCommonCustomForm);
begin
  FEnabledInteractiveGestures := FEnabledInteractiveGestures - [ARec];
end;

procedure TPlatformAndroid.ShowWindow(const AForm: TCommonCustomForm);
var
  NativeWin: JWindow;
begin
  if Assigned(AForm.Handle) then
  begin
    if not IsPopupForm(AForm) then
    begin
      NativeWin := SharedActivity.getWindow;
      try
        AForm.SetBounds(TWindowManager.Current.ContentRect.Left, TWindowManager.Current.ContentRect.Top,
          TWindowManager.Current.ContentRect.Width, TWindowManager.Current.ContentRect.Height);
      except
        Application.HandleException(AForm);
      end;
    end
    else
      TWindowManager.Current.SetNeedsRender;
    TWindowManager.Current.BringToFront(TAndroidWindowHandle(AForm.Handle));
  end;
end;

function TPlatformAndroid.ShowWindowModal(const AForm: TCommonCustomForm): TModalResult;
begin
  raise ENotImplemented.CreateFmt(SNotImplementedOnPlatform, ['ShowModal']);
  Result := mrCancel;
end;

procedure TPlatformAndroid.SingleTap;
begin
  TWindowManager.Current.SingleTap(FMouseDownCoordinates);
end;

procedure TPlatformAndroid.SingleTapTimerCall;
begin
  DestroySingleTapTimer;
  SingleTap;
end;

function TPlatformAndroid.FindForm(const AHandle: TWindowHandle): TCommonCustomForm;
begin
  Result := TWindowManager.Current.FindForm(AHandle);
end;

function TPlatformAndroid.GetClientSize(const AForm: TCommonCustomForm): TPointF;
begin
  if not IsPopupForm(AForm) then
    Result := TPointF.Create(TWindowManager.Current.ContentRect.Width, TWindowManager.Current.ContentRect.Height)
  else
    Result := TAndroidWindowHandle(AForm.Handle).Bounds.Size;
end;

function TPlatformAndroid.GetTitle: string;
begin
  Result := GetApplicationTitle;
end;

function TPlatformAndroid.GetWindowRect(const AForm: TCommonCustomForm): TRectF;
begin
  Result := TAndroidWindowHandle(AForm.Handle).Bounds;
end;

procedure TPlatformAndroid.SetWindowRect(const AForm: TCommonCustomForm; ARect: TRectF);
begin
  TAndroidWindowHandle(AForm.Handle).Bounds := ARect;
end;

function TPlatformAndroid.GetWindowScale(const AForm: TCommonCustomForm): Single;
begin
  Result := TWindowManager.Current.Scale;
end;

procedure TPlatformAndroid.InvalidateImmediately(const AForm: TCommonCustomForm);
begin
  TAndroidWindowHandle(AForm.Handle).NeedsUpdate := True;
end;

procedure TPlatformAndroid.InvalidateWindowRect(const AForm: TCommonCustomForm; R: TRectF);
begin
  TAndroidWindowHandle(AForm.Handle).NeedsUpdate := True;
end;

function TPlatformAndroid.IsPopupForm(const AForm: TCommonCustomForm): Boolean;
begin
  Result := Assigned(AForm) and
   ((AForm.FormStyle = TFormStyle.fsPopup) or (AForm.Owner is TPopup));
end;

function TPlatformAndroid.IsRotate(const APoint1, APoint2: TPointF): Boolean;
var
  Angle1, Angle2: Single;
begin
  Result := False;
  // check that there was a previous 2 finger movement
  if not FOldPoint1.IsZero and not FOldPoint2.IsZero then
  begin
    // make sure that either the x or the y values change in opposite directions
    if ((FOldPoint1.X - APoint1.X) * (FOldPoint2.X - APoint2.X) <= 0) or
      ((FOldPoint1.Y - APoint1.Y) * (FOldPoint2.Y - APoint2.Y) <= 0) then
    begin
      Angle1 := Arctan2(APoint1.Y - APoint2.Y, APoint1.X - APoint2.X);
      Angle2 := Arctan2(FOldPoint1.Y - FOldPoint2.Y, FOldPoint1.X - FOldPoint2.X);

                                                                                                                  
      if abs(Angle1 - Angle2) >= 0.01 then
      begin
        Result := True;
        //make rotation value counterclockwise and cumulative
        FRotationAngle := FRotationAngle - Angle1 + Angle2;
      end;
    end;
  end;

  if Result then
  begin
    Result := False;
    if TWindowManager.Current.SendCMGestureMessage(CreateGestureEventInfo(APoint2, TInteractiveGesture.igRotate)) then
    begin
      FActiveInteractiveGestures := FActiveInteractiveGestures + [TInteractiveGesture.igRotate];
      Result := True;
    end;
  end;
end;

function TPlatformAndroid.IsZoom(const APoint1, APoint2: TPointF): Boolean;
var
  Distance1, Distance2: Single;
begin
  Result := False;
  //check that there was a previous 2 finger movement
  if not FOldPoint1.IsZero and not FOldPoint2.IsZero then
  begin
    Distance1 := APoint1.Distance(APoint2);
    Distance2 := FOldPoint1.Distance(FOldPoint2);

    // Take into account an error margin (there is always a distance between two fingers pressed together).
    if (abs(Distance1) > 2) and (abs(Distance1 - Distance2) > 2) then
      Result := True;
  end;

  if Result then
  begin
    // a zoom gesture was recognized
    Result := False;
    if TWindowManager.Current.SendCMGestureMessage(CreateGestureEventInfo(APoint2, TInteractiveGesture.igZoom)) then
    begin
      FActiveInteractiveGestures := FActiveInteractiveGestures + [TInteractiveGesture.igZoom];
      Result := True;
    end;
  end;
end;

procedure TPlatformAndroid.Log(Fmt: String; Params: array of const);
var
  Msg: string;
  M: TMarshaller;
begin
  Msg := Format(FLogPrefix + Fmt, Params);
  LOGI(M.AsAnsi(Msg).ToPointer);
end;

{ IFMXScreenService }

function TPlatformAndroid.GetScreenSize: TPointF;
var
  Metrics: JDisplayMetrics;
begin
  Metrics := GetJDisplayMetrics;
  if Assigned(Metrics) then
    FScreenSize := TPointF.Create(Trunc(Metrics.widthPixels / GetScreenScale), Trunc(Metrics.heightPixels / GetScreenScale))
  else
    FScreenSize := TPointF.Create(0, 0);
  Result := FScreenSize;
end;

function TPlatformAndroid.GetScreenScale: Single;
const
// Default values taken from Android SDK reference:
//   http://developer.android.com/reference/android/util/DisplayMetrics.html#density
  DefaultDensityScale = 1;
  DefaultDensityDPI = 160;
var
  Metrics: JDisplayMetrics;
  DensityScale, DensityDPI: Single;
begin
  if SameValue(FScreenScale, 0, TEpsilon.Scale) then
  begin
    Metrics := GetJDisplayMetrics;
    if Assigned(Metrics) then
    begin
      DensityScale := Metrics.density; // API level 1
      DensityDPI := Metrics.densityDpi; // API level 4
    end
    else
    begin
      DensityScale := DefaultDensityScale;
      DensityDPI := DefaultDensityDPI;
    end;

    FScreenScale := DensityScale;

    if Assigned(ScreenScaleOverrideHook) then
    begin
      ScreenScaleOverrideHook(ScreenScaleOverrideHookContext, DensityScale, DensityDPI, FScreenScale);

      if FScreenScale < 1 then
        FScreenScale := 1
      else if FScreenScale > 3 then
        FScreenScale := 3;
    end;
  end;

  Result := FScreenScale;
end;

function TPlatformAndroid.GetScreenOrientation: TScreenOrientation;
var
  Display: JDisplay;
  Rotation: Integer;
begin
  Result := TScreenOrientation.soPortrait;

  Display := GetJDisplay;
  if Assigned(Display) then
  begin
    Rotation := Display.getRotation;

    if Rotation = TJSurface.JavaClass.ROTATION_180 then
      Result := TScreenOrientation.soInvertedPortrait
    else if Rotation = TJSurface.JavaClass.ROTATION_90 then
      Result := TScreenOrientation.soLandscape
    else if Rotation = TJSurface.JavaClass.ROTATION_270 then
      Result := TScreenOrientation.soInvertedLandscape
  end;
end;

{ IFMXDialogService }

type
  TAlertDialogOnClickListener = class;
  TDismissListener = class;

  TCommonAlertDialogRunner = class(TJavaLocal)
  protected
    Dialog: JAlertDialog;
    Done: TEvent;
    DialogResult: Integer;
    Listeners: array [0..2] of TAlertDialogOnClickListener;
    DismissListener: TDismissListener;
  public
    constructor Create;
    procedure CreateButtons(Buttons: TMsgDlgButtons; Builder: JAlertDialog_Builder);
    procedure ShowDialog(Builder: JAlertDialog_Builder);
  end;

  TAlertDialogOnClickListener = class(TJavaLocal, JDialogInterface_OnClickListener)
  strict private
    [Weak] DialogRunner: TCommonAlertDialogRunner;
    ModalResult: Integer;
  public
    constructor Create(Runner: TCommonAlertDialogRunner; MResult: Integer);
    procedure onClick(P1: JDialogInterface; P2: Integer); cdecl;
  end;

  TDismissListener = class(TJavaLocal, JDialogInterface_OnDismissListener)
  strict private
    [Weak] DialogRunner: TCommonAlertDialogRunner;
  public
    constructor Create(Runner: TCommonAlertDialogRunner);
    procedure onDismiss(dialog: JDialogInterface); cdecl;
  end;


  TMessageDialogRunner = class(TCommonAlertDialogRunner, JRunnable)
  private
    Msg: string;
    Buttons: TMsgDlgButtons;
    DlgType: TMsgDlgType;
  public
    constructor Create(const Msg: string; const DlgType: TMsgDlgType; const Buttons: TMsgDlgButtons);
    procedure run; cdecl;
  end;

  TInputQueryDialogRunner = class(TCommonAlertDialogRunner, JRunnable)
  private
    Title: string;
    Prompts: TArray<string>;
    Values: TArray<string>;
    Edits: array of JEditText;
  public
    constructor Create(const ACaption: string; const APrompts: array of string;
      var AValues: array of string);
    procedure run; cdecl;
  end;

var
  MsgTitles: array[TMsgDlgType] of string = (SMsgDlgWarning, SMsgDlgError, SMsgDlgInformation, SMsgDlgConfirm, '');
  ModalResults: array[TMsgDlgBtn] of Integer = (
    mrYes, mrNo, mrOk, mrCancel, mrAbort, mrRetry, mrIgnore, mrAll, mrNoToAll,
    mrYesToAll, 0, mrClose);
  ButtonCaptions: array[TMsgDlgBtn] of string = (
    SMsgDlgYes, SMsgDlgNo, SMsgDlgOK, SMsgDlgCancel, SMsgDlgAbort,
    SMsgDlgRetry, SMsgDlgIgnore, SMsgDlgAll, SMsgDlgNoToAll, SMsgDlgYesToAll,
    SMsgDlgHelp, SMsgDlgClose);

constructor TCommonAlertDialogRunner.Create;
begin
  inherited Create;
  DialogResult := -1;
  Done := TEvent.Create;
end;

procedure TCommonAlertDialogRunner.CreateButtons(Buttons: TMsgDlgButtons; Builder: JAlertDialog_Builder);
var
  ButtonIndex: Integer;
  B: TMsgDlgBtn;
begin
  ButtonIndex := 0;
  for B in Buttons do
  begin
    Listeners[ButtonIndex] := TAlertDialogOnClickListener.Create(Self, ModalResults[B]);
    case ButtonIndex of
    0: Builder.setPositiveButton(TJCharSequence.Wrap(StringToJNIString(TJNIResolver.GetJNIEnv, ButtonCaptions[B])), Listeners[ButtonIndex]);
    1: Builder.setNegativeButton(TJCharSequence.Wrap(StringToJNIString(TJNIResolver.GetJNIEnv, ButtonCaptions[B])), Listeners[ButtonIndex]);
    2: Builder.setNeutralButton(TJCharSequence.Wrap(StringToJNIString(TJNIResolver.GetJNIEnv, ButtonCaptions[B])), Listeners[ButtonIndex]);
    end;

    Inc(ButtonIndex);
    if ButtonIndex > 2 then
      Break;
  end;
end;

procedure TCommonAlertDialogRunner.ShowDialog(Builder: JAlertDialog_Builder);
begin
  Dialog := builder.create;
  DismissListener := TDismissListener.Create(Self);
  Dialog.setOnDismissListener(DismissListener);
  Dialog.show();
end;

constructor TAlertDialogOnClickListener.Create(Runner: TCommonAlertDialogRunner;
  MResult: Integer);
begin
  inherited Create;
  DialogRunner := Runner;
  ModalResult := MResult;
end;

procedure TAlertDialogOnClickListener.onClick(P1: JDialogInterface; P2: Integer);
begin
  DialogRunner.Dialog.setOnDismissListener(nil);
  P1.dismiss;
  DialogRunner.DialogResult := ModalResult;
  DialogRunner.Done.SetEvent;
end;

constructor TDismissListener.Create(Runner: TCommonAlertDialogRunner);
begin
  inherited Create;
  DialogRunner := Runner;
end;

procedure TDismissListener.onDismiss(dialog: JDialogInterface);
begin
  DialogRunner.DialogResult := ModalResults[TMsgDlgBtn.mbClose];
  DialogRunner.Done.SetEvent;
end;

constructor TMessageDialogRunner.Create(const Msg: string; const DlgType: TMsgDlgType;
  const Buttons: TMsgDlgButtons);
begin
  inherited Create;
  Self.Msg := Msg;
  Self.Buttons := Buttons;
  Self.DlgType := DlgType;
end;

procedure TMessageDialogRunner.run;
var
  Builder : JAlertDialog_Builder;
begin
  Builder := TJAlertDialog_Builder.JavaClass.init(SharedActivity);
  Builder.setTitle(TJCharSequence.Wrap(StringToJNIString(TJNIResolver.GetJNIEnv, MsgTitles[DlgType])));
  Builder.setMessage(TJCharSequence.Wrap(StringToJNIString(TJNIResolver.GetJNIEnv, Msg)));

  CreateButtons(Self.Buttons, Builder);
  ShowDialog(Builder);
end;

function TPlatformAndroid.MessageDialog(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefaultButton: TMsgDlgBtn; X, Y, HelpCtx: Integer;
  const HelpFileName: string): Integer;
var
  DialogRunner: TMessageDialogRunner;
begin
  DialogRunner := TMessageDialogRunner.Create(Msg, DlgType, Buttons);
  SharedActivity.runOnUiThread(DialogRunner);
  DialogRunner.Done.WaitFor(INFINITE);
  Result := DialogRunner.DialogResult;
end;

function TPlatformAndroid.GetTextEditorProxy: JFmxTextEditorProxy;
begin
  if not Assigned(FTextEditorProxy) then
    FTextEditorProxy := MainActivity.getTextEditorProxy;
  Result := FTextEditorProxy;
end;

function TPlatformAndroid.InputQuery(const ACaption: string;
  const APrompts: array of string; var AValues: array of string;
  CloseQueryFunc: TInputCloseQueryFunc): Boolean;
var
  IQRunner: TInputQueryDialogRunner;
  I: Integer;
begin
  Result := False;

  if Length(AValues) < Length(APrompts) then
    raise EInvalidOperation.Create(SPromptArrayTooShort);
  if Length(APrompts) = 0 then
    raise EInvalidOperation.Create(SPromptArrayEmpty);

  IQRunner := TInputQueryDialogRunner.Create(ACaption, APrompts, AValues);
  SharedActivity.runOnUiThread(IQRunner);
  IQRunner.Done.WaitFor(INFINITE);

  if IQRunner.DialogResult = mrOk then
  begin
    for I in [0..Length(AValues)-1] do
      AValues[I] := JCharSequenceToString(JTextView(IQRunner.Edits[I]).getText);
    Result := True;
  end;
end;

{ TInputQueryDialogRunner }

constructor TInputQueryDialogRunner.Create(const ACaption: string;
  const APrompts: array of string; var AValues: array of string);
var
  I: Integer;
begin
  inherited Create;

  Title := ACaption;

  SetLength(Prompts, Length(APrompts));
  for I in [0..Length(APrompts)-1] do
    Prompts[I] := APrompts[I];

  SetLength(Values, Length(AValues));
  for I in [0..Length(AValues)-1] do
    Values[I] := AValues[I];
end;

procedure TInputQueryDialogRunner.run;
var
  Builder: JAlertDialog_Builder;
  Layout: JLinearLayout;
  Labels: array of JTextView;
  I: Integer;
begin
  Builder := TJAlertDialog_Builder.JavaClass.init(SharedActivity);
  Builder.setTitle(TJCharSequence.Wrap(StringToJNIString(TJNIResolver.GetJNIEnv, Self.Title)));

  Layout := TJLinearLayout.JavaClass.init(SharedActivity);
  Layout.setOrientation(TJLinearLayout.JavaClass.VERTICAL);

  SetLength(Labels, Length(Self.Prompts));
  SetLength(Edits, Length(Self.Values));

  for I in [0..Length(Self.Values)-1] do
  begin
    if I < Length(Self.Prompts) then
    begin
      Labels[I] := TJTextView.JavaClass.init(SharedActivity);
      Labels[I].setText(TJCharSequence.Wrap(StringToJNIString(TJNIResolver.GetJNIEnv, Self.Prompts[I])));
      Layout.addView(Labels[I]);
    end;

    Edits[I] := TJEditText.JavaClass.init(SharedActivity);
    Edits[I].setInputType(TJInputType.JavaClass.TYPE_CLASS_TEXT);
    Edits[I].getEditableText().append(TJCharSequence.Wrap(StringToJNIString(TJNIResolver.GetJNIEnv, Self.Values[I])));

    Layout.addView(Edits[I]);
  end;

  Builder.setView(Layout);

  CreateButtons(mbOkCancel, Builder);
  ShowDialog(Builder);
end;

function TPlatformAndroid.DialogOpenFiles(var AFileName: TFileName;
  const AInitDir, ADefaultExt, AFilter, ATitle: string;
  var AFilterIndex: Integer; var AFiles: TStrings;
  var AOptions: TOpenOptions): Boolean;
begin
  Result := False;
end;

function TPlatformAndroid.DialogPageSetup(var AMargin, AMinMargin: TRect;
  var APaperSize: TPointF; var AUnits: TPageMeasureUnits;
  AOptions: TPageSetupDialogOptions): Boolean;
begin
  Result := False;
end;

function TPlatformAndroid.DialogPrint(var ACollate, APrintToFile: Boolean;
  var AFromPage, AToPage, ACopies: Integer; AMinPage, AMaxPage: Integer;
  var APrintRange: TPrintRange; AOptions: TPrintDialogOptions): Boolean;
begin
  Result := False;
end;

function TPlatformAndroid.DialogPrinterSetup: Boolean;
begin
  Result := False;
end;

function TPlatformAndroid.DialogSaveFiles(var AFileName: TFileName;
  const AInitDir, ADefaultExt, AFilter, ATitle: string;
  var AFilterIndex: Integer; var AFiles: TStrings;
  var AOptions: TOpenOptions): Boolean;
begin
  Result := False;
end;


function TPlatformAndroid.PageSetupGetDefaults(var AMargin, AMinMargin: TRect;
  var APaperSize: TPointF; AUnits: TPageMeasureUnits;
  AOptions: TPageSetupDialogOptions): Boolean;
begin
  Result := False;
end;

type
  TSimpleProcedureRunner = class(TJavaLocal, JRunnable)
  strict private
    FSelfLock: TSimpleProcedureRunner;
    FProc: TThreadProcedure;
    FEvent: TEvent;
  public
    constructor Create(Proc: TThreadProcedure); overload;
    procedure run; cdecl;
    property Event: TEvent read FEvent;
  end;

constructor TSimpleProcedureRunner.Create(Proc: TThreadProcedure);
begin
  inherited Create;
  FProc := Proc;
  FSelfLock := Self;
  FEvent := TEvent.Create;
end;

procedure TSimpleProcedureRunner.run;
begin
  FProc;
  FSelfLock := nil;
  FEvent.SetEvent;
end;

procedure TPlatformAndroid.RunOnUIThread(Proc: TThreadProcedure);
begin
  MainActivity.runOnUiThread(TSimpleProcedureRunner.Create(Proc));
end;

procedure TPlatformAndroid.SynchronizeOnUIThread(Proc: TThreadProcedure);
var
  Runner: TSimpleProcedureRunner;
begin
  Runner := TSimpleProcedureRunner.Create(Proc);
  MainActivity.runOnUiThread(Runner);
  Runner.Event.WaitFor;
end;


procedure TPlatformAndroid.LongTapTimerCall;
begin
  //a long press was recognized
  DestroyLongTapTimer;
  DestroySingleTapTimer;
  TWindowManager.Current.SendCMGestureMessage(CreateGestureEventInfo(PointF(0, 0), TInteractiveGesture.igLongTap));
end;

procedure TPlatformAndroid.AddRecognizer(const ARec: TInteractiveGesture;
  const AForm: TCommonCustomForm);
begin
  FEnabledInteractiveGestures := FEnabledInteractiveGestures + [ARec];
end;

procedure TPlatformAndroid.BringToFront(const AForm: TCommonCustomForm);
begin
  if Assigned(AForm.Handle) then
    TWindowManager.Current.BringToFront(TAndroidWindowHandle(AForm.Handle));
end;

procedure TPlatformAndroid.SendToBack(const AForm: TCommonCustomForm);
begin
  if Assigned(AForm.Handle) then
    TWindowManager.Current.SendToBack(TAndroidWindowHandle(AForm.Handle));
end;

procedure TPlatformAndroid.SetApplicationEventHandler(AEventHandler: TApplicationEventHandler);
begin
  FOnApplicationEvent := AEventHandler;
end;

function TPlatformAndroid.HandleApplicationEvent(AEvent: TApplicationEvent; AContext: TObject): Boolean;
begin
  Result := False;
  if Assigned(FOnApplicationEvent) then
    try
      Result := FOnApplicationEvent(AEvent, AContext);
    except
      Application.HandleException(Self);
    end;
end;

procedure TPlatformAndroid.SetClientSize(const AForm: TCommonCustomForm; const ASize: TPointF);
var
  Bounds: TRectF;
begin
  if IsPopupForm(AForm) then
  begin
    Bounds := TAndroidWindowHandle(AForm.Handle).Bounds;
    TAndroidWindowHandle(AForm.Handle).Bounds := TRectF.Create(Bounds.TopLeft, ASize.X, ASize.Y);
  end;
end;

procedure TPlatformAndroid.SetWindowCaption(const AForm: TCommonCustomForm; const ACaption: string);
begin
  // NOP on Android
end;

procedure TPlatformAndroid.SetCapture(const AForm: TCommonCustomForm);
begin
  TWindowManager.Current.SetCapture(TAndroidWindowHandle(AForm.Handle));
end;

procedure TPlatformAndroid.ReleaseCapture(const AForm: TCommonCustomForm);
begin
  TWindowManager.Current.ReleaseCapture;
end;

procedure TPlatformAndroid.SetWindowState(const AForm: TCommonCustomForm; const AState: TWindowState);
begin
  // NOP on Android
end;

procedure TPlatformAndroid.RegisterCanvasClasses;
begin
  FMX.Canvas.GPU.RegisterCanvasClasses;
end;

procedure TPlatformAndroid.RegisterContextClasses;
begin
  FMX.Context.GLES.Android.RegisterContextClasses;
end;

procedure TPlatformAndroid.UnregisterCanvasClasses;
begin
  FMX.Canvas.GPU.UnregisterCanvasClasses;
end;

procedure TPlatformAndroid.UnregisterContextClasses;
begin
  FMX.Context.GLES.Android.UnregisterContextClasses;
end;

{ IFMXSystemInformationService }

function TPlatformAndroid.GetScrollingBehaviour: TScrollingBehaviours;
begin
  Result := [TScrollingBehaviour.sbAnimation, TScrollingBehaviour.sbAutoShowing, TScrollingBehaviour.sbTouchTracking];
end;

function TPlatformAndroid.GetFeatures: TDeviceFeatures;
begin
  Result := [TDeviceFeature.HasTouchScreen];
end;

function TPlatformAndroid.GetMinScrollThumbSize: Single;
begin
  Result := 30;
end;

function TPlatformAndroid.GetCaretWidth: Integer;
begin
  Result := 2;
end;

{ IFMXStyleService }

function TPlatformAndroid.GetSystemStyle(const Context: TFmxObject): TFmxObject;
begin
  Result := FMX.Controls.Android.GetSystemStyle(Context);
end;

{ IFMXSystemFontService }

function TPlatformAndroid.GetDefaultFontFamilyName: string;
begin
  Result := 'Roboto';
end;

{ IFMXDefaultPropertyValueService }

function TPlatformAndroid.GetDefaultPropertyValue(const AClassName, PropertyName: string): TValue;
begin
  Result := TValue.Empty;

  if string.Compare(AClassName, 'tcolorcombobox', True) = 0 then
    Result := TValue.From<TDropDownKind>(TDropDownKind.ddkNative);
end;

{ IFMXDefaultMetricsService }

function TPlatformAndroid.GetDefaultSize(const AComponent: TComponentKind): TSize;
begin
  case AComponent of
    TComponentKind.ckButton: Result := TSize.Create(73, 44);
    TComponentKind.ckLabel: Result := TSize.Create(82, 23);
    TComponentKind.ckEdit: Result := TSize.Create(97, 32);
    TComponentKind.ckScrollBar: Result := TSize.Create(7, 7);
    TComponentKind.ckListBoxItem: Result := TSize.Create(44, 44);
  else
    Result := TSize.Create(80, 22);
  end;
end;

function TPlatformAndroid.SupportsDefaultSize(const AComponent: TComponentKind): Boolean;
begin
  case AComponent of
    TComponentKind.ckButton: Result := True;
    TComponentKind.ckLabel: Result := True;
    TComponentKind.ckEdit: Result := True;
    TComponentKind.ckScrollBar: Result := True;
    TComponentKind.ckListBoxItem: Result := True;
  else
    Result := False;
  end;
end;

{ IFMXLocaleService }

function TPlatformAndroid.GetCurrentLangID: string;
var
  Locale: JLocale;
begin
  Locale := TJLocale.JavaClass.getDefault;
  Result := JStringToString(Locale.getISO3Language);
  if Length(Result) > 2 then
    Delete(Result, 3, MaxInt);
end;

function TPlatformAndroid.GetLocaleFirstDayOfWeek: string;
var
  Calendar: JCalendar;
begin
  Calendar := TJCalendar.JavaClass.getInstance;
  Result := IntToStr(Calendar.getFirstDayOfWeek);
end;

function TPlatformAndroid.GetLongTapAllowedMovement: Single;
begin
  Result := LONG_TAP_MOVEMENT/TWindowManager.Current.Scale;
end;

function TPlatformAndroid.GetListingHeaderBehaviors: TListingHeaderBehaviors;
begin
  Result := [];
end;

function TPlatformAndroid.GetListingSearchFeatures: TListingSearchFeatures;
begin
  Result := [TListingSearchFeature.StayOnTop];
end;

function TPlatformAndroid.GetListingTransitionFeatures: TListingTransitionFeatures;
begin
  Result := [];
end;

{ IFMXClipboardService }

type
  TGenericTextClipboardService = class
  strict private
    TextClipboardManager: JClipboardManager;
    ContentClipboardManager: Jcontent_ClipboardManager;
  private
    constructor Create;
  public
    function HasText: Boolean;
    function GetText: string;
    procedure SetText(const S: string);
  end;

  TClipboardGetter = class(TWaitableValueBase, JRunnable)
  public
    procedure run; cdecl;
  end;

  TClipboardSetter = class(TWaitableValueBase, JRunnable)
  strict private
    FText: string;
  public
    constructor Create(const Text: string);
    procedure run; cdecl;
  end;

constructor TGenericTextClipboardService.Create;
var
  ServiceClassName: string;
  UnknownService: JObject;
begin
  ContentClipboardManager := nil;
  TextClipboardManager := nil;

  UnknownService := SharedActivity.getSystemService(TJContext.JavaClass.CLIPBOARD_SERVICE);
  if Assigned(UnknownService) then
  begin
    ServiceClassName := JStringToString(UnknownService.getClass.getName);
    if ServiceClassName = 'android.content.ClipboardManager' then
    begin
      ContentClipboardManager := TJcontent_ClipboardManager.Wrap((UnknownService as ILocalObject).GetObjectID);
    end
    else if ServiceClassName = 'android.text.ClipboardManager' then
    begin
      TextClipboardManager := TJClipboardManager.Wrap((UnknownService as ILocalObject).GetObjectID);
    end;
  end;
end;

function TGenericTextClipboardService.HasText: Boolean;
begin
  if Assigned(ContentClipboardManager) then Exit(ContentClipboardManager.hasText);
  if Assigned(TextClipboardManager) then Exit(TextClipboardManager.hasText);
  Exit(False);
end;

function TGenericTextClipboardService.GetText: string;
begin
  if Assigned(ContentClipboardManager) and ContentClipboardManager.hasText then
    Exit(JCharSequenceToString(ContentClipboardManager.getText));
  if Assigned(TextClipboardManager) and TextClipboardManager.hasText then
    Exit(JCharSequenceToString(TextClipboardManager.getText));
  Exit(string.Empty);
end;

procedure TGenericTextClipboardService.SetText(const S: string);
begin
  if Assigned(ContentClipboardManager) then
    ContentClipboardManager.setText(StrToJCharSequence(S))
  else
  if Assigned(TextClipboardManager) then
    TextClipboardManager.setText(StrToJCharSequence(S));
end;

constructor TWaitableValueBase.Create;
begin
  inherited Create;

  Done := TEvent.Create;
  Value := TValue.Empty;
end;

procedure TClipboardGetter.run;
begin
  Value := TValue.From<string>(TGenericTextClipboardService.Create.GetText);

  Done.SetEvent;
end;

{ TClipboardSetter }

constructor TClipboardSetter.Create(const Text: string);
begin
  inherited Create;

  FText := Text;
end;

procedure TClipboardSetter.run;
begin
  TGenericTextClipboardService.Create.SetText(FText);

  Done.SetEvent;
end;

procedure TPlatformAndroid.SetClipboard(Value: TValue);
var
  Setter: TClipboardSetter;
begin
  Setter := TClipboardSetter.Create(Value.ToString);
  SharedActivity.runOnUiThread(Setter);
  Setter.Done.WaitFor(INFINITE);
end;


procedure TPlatformAndroid.SetKeyboardEventToSkip(event: JKeyEvent);
begin
  FSkipEventQueue.Enqueue(event);
end;

procedure TPlatformAndroid.SetScreenOrientation(
  AOrientations: TScreenOrientations);
begin
  if (TScreenOrientation.soPortrait in AOrientations) and
     (TScreenOrientation.soLandscape in AOrientations) and
     (TScreenOrientation.soInvertedPortrait in AOrientations) and
     (TScreenOrientation.soInvertedLandscape in AOrientations) then
    MainActivity.embSetOrientation(-1)
  else if TScreenOrientation.soPortrait in AOrientations then
    MainActivity.embSetOrientation(Integer(TScreenOrientation.soPortrait))
  else if TScreenOrientation.soLandscape in AOrientations then
    MainActivity.embSetOrientation(Integer(TScreenOrientation.soLandscape))
  else if TScreenOrientation.soInvertedPortrait in AOrientations then
    MainActivity.embSetOrientation(Integer(TScreenOrientation.soInvertedPortrait))
  else if TScreenOrientation.soInvertedLandscape in AOrientations then
    MainActivity.embSetOrientation(Integer(TScreenOrientation.soInvertedLandscape));
end;

function TPlatformAndroid.GetClipboard: TValue;
var
  Getter: TClipboardGetter;
begin
  Getter := TClipboardGetter.Create;
  SharedActivity.runOnUiThread(Getter);
  Getter.Done.WaitFor(INFINITE);
  Result := Getter.Value;
end;

{ IFMXTextService }

function TPlatformAndroid.GetTextServiceClass: TTextServiceClass;
begin
  Result := TTextServiceAndroid;
end;

{ TAndroidWindowHandle }

constructor TAndroidWindowHandle.Create(const AForm: TCommonCustomForm);
begin
  inherited Create;
  FNeedsUpdate := True;
  FForm := AForm;
  FBounds := TRectF.Create(AForm.Left, AForm.Top, AForm.Left + AForm.Width, AForm.Top + AForm.Height);
end;

procedure TAndroidWindowHandle.CreateTexture;
var
  ScaledSize: TSize;
begin
  if not Assigned(FTexture) then
  begin
    ScaledSize := TSize.Create(Round(Bounds.Width * TWindowManager.Current.Scale),
      Round(Bounds.Height * TWindowManager.Current.Scale));

    FTexture := TTexture.Create;
    FTexture.Style := [TTextureStyle.tsRenderTarget];
    FTexture.SetSize(ScaledSize.Width, ScaledSize.Height);
    FTexture.Initialize;
  end;
end;

procedure TAndroidWindowHandle.DestroyTexture;
begin
  if Assigned(FTexture) then
  begin
    FTexture.DisposeOf;
    FTexture := nil;
  end;
end;

procedure TAndroidWindowHandle.SetBounds(const Value: TRectF);
begin
  if FBounds <> Value then
  begin
    FBounds := Value;
    TWindowManager.Current.SetNeedsRender;
  end;
end;

function TAndroidWindowHandle.GetIsPopup: Boolean;
begin
  Result := PlatformAndroid.IsPopupForm(FForm);
end;

procedure TAndroidWindowHandle.SetNeedsUpdate(const Value: Boolean);
begin
  FNeedsUpdate := Value;
  if FNeedsUpdate then
    TWindowManager.Current.SetNeedsRender;
end;

{ TTimerManager }


constructor TTimerManager.Create;
begin
  FCriticalSection := TCriticalSection.Create;
  FHandlers := TDictionary<Integer, TTimerProc>.Create;
  AndroidTimerSetHandler(DoOnTimer);
end;

destructor TTimerManager.Destroy;
begin
  AndroidTimerSetHandler(nil);
  FHandlers := nil;
  FCriticalSection := nil;
  inherited;
end;

function TTimerManager.CreateTimer(Interval: Integer;
  TimerFunc: TTimerProc): TFmxHandle;
var
  TimerHandle: Integer;
begin
  FCriticalSection.Enter;
  try
    TimerHandle := AndroidTimerCreate;
    AndroidTimerSetInterval(TimerHandle, Interval);
    FHandlers.Add(TimerHandle, TimerFunc);
  finally
    FCriticalSection.Leave;
  end;

  Result := TimerHandle;
end;

procedure TTimerManager.DestroyTimer(TimerHandle: TFmxHandle);
begin
  FCriticalSection.Enter;
  try
    AndroidTimerDestroy(TimerHandle);
    if FHandlers.ContainsKey(TimerHandle) then
      FHandlers.Remove(TimerHandle);
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TTimerManager.DoOnTimer(TimerHandle: Integer);
begin
  TThread.Queue(nil,
    procedure
    var
      Handler: TTimerProc;
    begin
      if FHandlers.TryGetValue(TimerHandle, Handler) then
        Handler;
    end);
end;

class function TTimerManager.GetInstance: TTimerManager;
begin
  if not Assigned(FInstance) then
    FInstance := TTimerManager.Create;
  Result := FInstance;
end;

{ TTextServiceAndroid }

constructor TTextServiceAndroid.Create(const Owner: IControl;
  SupportMultiLine: Boolean);
begin
  inherited Create(Owner, SupportMultiLine);
  FComposingBegin := -1;
  FComposingEnd := -1;
end;

destructor TTextServiceAndroid.Destroy;
begin
  inherited;
end;

procedure TTextServiceAndroid.BeginSelection;
begin
  TWindowManager.Current.BeginSelection;
  TWindowManager.Current.HideContextMenu;
end;

procedure TTextServiceAndroid.EndSelection;
begin
  TWindowManager.Current.EndSelection;
  InternalUpdateSelection;
  TWindowManager.Current.ShowContextMenu;
end;


function TTextServiceAndroid.CombinedText: string;
begin
  Result := GetText;
end;

procedure TTextServiceAndroid.DrawSingleLine(const Canvas: TCanvas;
      const ARect: TRectF; const FirstVisibleChar: integer; const Font: TFont;
      const AOpacity: Single; const Flags: TFillTextFlags; const ATextAlign: TTextAlign;
      const AVTextAlign: TTextAlign = TTextAlign.taCenter;
      const AWordWrap: Boolean = False);
var
  I: Integer;
  S: string;
  Layout: TTextLayout;
  Region: TRegion;
begin
  Layout := TTextLayoutManager.TextLayoutByCanvas(Canvas.ClassType).Create;
  try
    Layout.BeginUpdate;
    Layout.TopLeft := ARect.TopLeft;
    Layout.MaxSize := PointF(ARect.Width, ARect.Height);
    Layout.WordWrap := AWordWrap;
    Layout.HorizontalAlign := ATextAlign;
    Layout.VerticalAlign := AVTextAlign;
    Layout.Font := Font;
    Layout.Color := Canvas.Fill.Color;
    Layout.Opacity := AOpacity;
    Layout.RightToLeft := TFillTextFlag.ftRightToLeft in Flags;
    S := CombinedText;
    Layout.Text := S.Substring(FirstVisibleChar - 1, S.Length - FirstVisibleChar + 1);
    Layout.EndUpdate;
    Layout.RenderLayout(Canvas);

    if (FComposingBegin >= 0) and (FComposingEnd >= 0) and (FComposingBegin < FComposingEnd) then
    try
      Canvas.Stroke.Assign(Canvas.Fill);
      Canvas.StrokeThickness := 1;
      Canvas.StrokeDash := TStrokeDash.sdSolid;

      Region := Layout.RegionForRange(TTextRange.Create(FComposingBegin - (FirstVisibleChar - 1), FComposingEnd - FComposingBegin));
      for I := Low(Region) to High(Region) do
        Canvas.DrawLine(
          PointF(Region[I].Left, Region[I].Bottom),
          PointF(Region[I].Right, Region[I].Bottom),
          AOpacity, Canvas.Stroke);
    finally
    end;
  finally
    FreeAndNil(Layout);
  end;
end;

procedure TTextServiceAndroid.DrawSingleLine(const Canvas: TCanvas;
      const S: string;
      const ARect: TRectF;
      const Font: TFont;
      const AOpacity: Single; const Flags: TFillTextFlags; const ATextAlign: TTextAlign;
      const AVTextAlign: TTextAlign = TTextAlign.taCenter;
      const AWordWrap: Boolean = False);
var
  I: Integer;
  Layout: TTextLayout;
  Region: TRegion;
begin
  Layout := TTextLayoutManager.TextLayoutByCanvas(Canvas.ClassType).Create;
  try
    Layout.BeginUpdate;
    Layout.TopLeft := ARect.TopLeft;
    Layout.MaxSize := PointF(ARect.Width, ARect.Height);
    Layout.WordWrap := AWordWrap;
    Layout.HorizontalAlign := ATextAlign;
    Layout.VerticalAlign := AVTextAlign;
    Layout.Font := Font;
    Layout.Color := Canvas.Fill.Color;
    Layout.Opacity := AOpacity;
    Layout.RightToLeft := TFillTextFlag.ftRightToLeft in Flags;
    Layout.Text := S;
    Layout.EndUpdate;
    Layout.RenderLayout(Canvas);

    if (FComposingBegin >= 0) and (FComposingEnd >= 0) and (FComposingBegin < FComposingEnd) then
    try
      Canvas.Stroke.Assign(Canvas.Fill);
      Canvas.StrokeThickness := 1;
      Canvas.StrokeDash := TStrokeDash.sdSolid;

      Region := Layout.RegionForRange(TTextRange.Create(FComposingBegin, FComposingEnd - FComposingBegin));
      for I := Low(Region) to High(Region) do
        Canvas.DrawLine(
          PointF(Region[I].Left, Region[I].Bottom),
          PointF(Region[I].Right, Region[I].Bottom),
          AOpacity, Canvas.Stroke);
    finally
    end;
  finally
    FreeAndNil(Layout);
  end;
end;

{ TFMXTextListener }

constructor TFMXTextListener.Create(const TextService: TTextServiceAndroid);
begin
  inherited Create;
  FTextService := TextService;
end;

procedure TFMXTextListener.onTextUpdated(text: JCharSequence; position: Integer);
begin
  TWindowManager.Current.HideContextMenu;
  FTextService.FText := JCharSequenceToString(text);
  FTextService.FCaretPosition.X := position;
  TThread.Queue(nil,
    procedure
    begin
      FTextService.InternalUpdate;
    end
  );
end;

procedure TFMXTextListener.onComposingText(beginPosition: Integer; endPosition: Integer);
begin
  TWindowManager.Current.HideContextMenu;
  FTextService.FComposingBegin := beginPosition;
  FTextService.FComposingEnd := endPosition;
end;

procedure TFMXTextListener.onSkipKeyEvent(event: JKeyEvent);
begin
  PlatformAndroid.SetKeyboardEventToSkip(event);
end;

procedure TTextServiceAndroid.EnterControl(const FormHandle: TWindowHandle);
var
  VirtKBControl: IVirtualKeyboardControl;
  KbType: Integer;
  RKType: Integer;
  Bounds: TRect;
begin
  if (FormHandle is TAndroidWindowHandle)
    and Assigned(TAndroidWindowHandle(FormHandle).Form.Focused)
    and Supports(TAndroidWindowHandle(FormHandle).Form.Focused, IVirtualKeyboardControl, VirtKBControl) then
  begin
    case VirtKBControl.ReturnKeyType of
      TReturnKeyType.rktDefault:  RKType := TJFMXTextEditorProxy.JavaClass.ACTION_ENTER;
      TReturnKeyType.rktDone:     RKType := TJFMXTextEditorProxy.JavaClass.ACTION_DONE;
      TReturnKeyType.rktGo:       RKType := TJFMXTextEditorProxy.JavaClass.ACTION_GO;
      TReturnKeyType.rktNext:     RKType := TJFMXTextEditorProxy.JavaClass.ACTION_NEXT;
      TReturnKeyType.rktSearch:   RKType := TJFMXTextEditorProxy.JavaClass.ACTION_SEARCH;
      TReturnKeyType.rktSend:     RKType := TJFMXTextEditorProxy.JavaClass.ACTION_SEND;
    end;

    case VirtKBControl.KeyboardType of
      TVirtualKeyboardType.vktDefault:
        KbType := TJFMXTextEditorProxy.JavaClass.INPUT_TEXT;
      TVirtualKeyboardType.vktNumbersAndPunctuation:
        KbType := TJFMXTextEditorProxy.JavaClass.INPUT_NUMBER_AND_PUNCTUATION;
      TVirtualKeyboardType.vktNumberPad:
        KbType := TJFMXTextEditorProxy.JavaClass.INPUT_NUMBER;
      TVirtualKeyboardType.vktPhonePad:
        KbType := TJFMXTextEditorProxy.JavaClass.INPUT_PHONE;
      TVirtualKeyboardType.vktAlphabet:
        KbType := TJFMXTextEditorProxy.JavaClass.INPUT_ALPHABET;
      TVirtualKeyboardType.vktURL:
        KbType := TJFMXTextEditorProxy.JavaClass.INPUT_URL;
      TVirtualKeyboardType.vktNamePhonePad:
        KbType := TJFMXTextEditorProxy.JavaClass.INPUT_NAME_PHONE_PAD;
      TVirtualKeyboardType.vktEmailAddress:
        KbType := TJFMXTextEditorProxy.JavaClass.INPUT_EMAIL_ADDRESS;
    end;
  end;

  if not Assigned(FTextView) then
    FTextView := PlatformAndroid.GetTextEditorProxy;

  if Assigned(FTextView) then
  begin
    FTextListener := TFMXTextListener.Create(Self);
    Bounds := (FOwner as ITextInput).GetSelectionBounds;
    PlatformAndroid.SynchronizeOnUIThread(
      procedure
      begin
        FTextView.setInputType(KbType);
        FTextView.setEnterAction(RKType);
        FTextView.setText(StrToJCharSequence(FText));
        if Bounds.Width > 0 then
          FTextView.setSelection(Bounds.Left, Bounds.Right)
        else
          FTextView.setCursorPosition(CaretPosition.X);
        FTextView.addTextListener(FTextListener);
        FTextView.requestFocus;
      end);
  end;
end;

procedure TTextServiceAndroid.ExitControl(const FormHandle: TWindowHandle);
begin
  if Assigned(FTextView) and Assigned(FTextListener) then
  begin
    FComposingBegin := -1;
    FComposingEnd := -1;
    PlatformAndroid.SynchronizeOnUIThread(
      procedure
      begin
        FTextView.setCursorPosition(FCaretPosition.X);
        FTextView.removeTextListener(FTextListener);
        FTextView.clearFocus;
        FTextView := nil;
      end);
  end;
end;

function TTextServiceAndroid.GetCaretPostion: TPoint;
begin
  Result := FCaretPosition;
end;

procedure TTextServiceAndroid.SetCaretPosition(const Value: TPoint);
begin
  if FCaretPosition <> Value then
  begin
    FCaretPosition := Value;
    if Assigned(FTextView) then
      PlatformAndroid.SynchronizeOnUIThread(
        procedure
        begin
          FTextView.setCursorPosition(FCaretPosition.X);
        end);
  end;
end;

function TTextServiceAndroid.GetText: string;
begin
  Result := FText;
end;

procedure TTextServiceAndroid.SetText(const Value: string);
begin
  if not SameText(FText, Value) then
  begin
    FText := Value;
    if Assigned(FTextView) then
      PlatformAndroid.SynchronizeOnUIThread(
        procedure
        begin
          FTextView.setText(StrToJCharSequence(Value));
        end);
  end;
end;

function TTextServiceAndroid.HasMarkedText: Boolean;
begin
  Result := (FComposingBegin >= 0) and (FComposingEnd >= 0) and (FComposingBegin < FComposingEnd);
end;

function TTextServiceAndroid.GetImeMode: TImeMode;
begin
  Result := FImeMode;
end;

procedure TTextServiceAndroid.SetImeMode(const Value: TImeMode);
begin
  FImeMode := Value;
end;

procedure TTextServiceAndroid.InternalUpdate;
begin
  (FOwner as ITextInput).UpdateCaretPoint;
end;

procedure TTextServiceAndroid.InternalUpdateSelection;
var
  Bounds: TRect;
begin
  Bounds := (FOwner as ITextInput).GetSelectionBounds;
  PlatformAndroid.SynchronizeOnUIThread(
    procedure
    begin
      if Assigned(FTextView) then
        FTextView.setSelection(Bounds.Left, Bounds.Right);
    end);
end;

function TTextServiceAndroid.TargetClausePosition: TPoint;
begin
  Result := CaretPosition;
end;

{ TFMXNativeActivityListener }

procedure TFMXNativeActivityListener.onCancelReceiveImage(ARequestCode: Integer);
begin
  TThread.Queue(nil, procedure
  begin
    TMessageManager.DefaultManager.SendMessage(nil, TMessageCancelReceivingImage.Create(ARequestCode));
  end);
end;

procedure TFMXNativeActivityListener.onReceiveImage(ARequestCode: Integer; ABitmap: JBitmap);
var
  Message: TMessageReceivedImage;
begin
  TThread.Queue(nil, procedure
  begin
    Message := TMessageReceivedImage.Create(ABitmap);
    Message.RequestCode := ARequestCode;
    TMessageManager.DefaultManager.SendMessage(nil, Message);
  end);
end;

procedure TFMXNativeActivityListener.onReceiveNotification(P1: JIntent);
begin
  TMessageManager.DefaultManager.SendMessage(nil, TMessageReceivedNotification.Create(P1));
end;

function ConvertPixelToPoint(const P: TPointF): TPointF;
begin
  Exit(TWindowManager.Current.PixelToPoint(P));
end;

function ConvertPointToPixel(const P: TPointF): TPointF;
begin
  Exit(TWindowManager.Current.PointToPixel(P));
end;

initialization

end.
