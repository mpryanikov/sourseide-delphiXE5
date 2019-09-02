{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit FMX.ListView.Types;

interface

uses
  System.Types, System.UITypes, System.Classes, System.Generics.Collections, FMX.Types, FMX.Controls,FMX.TextLayout,
  System.Rtti, FMX.Objects, FMX.Graphics, FMX.Styles.Objects;

{$SCOPEDENUMS ON}

{$IF DEFINED(IOS) OR DEFINED(ANDROID)}
{$DEFINE LISTVIEW_TOUCH}
{$ENDIF}

{.$DEFINE PIXEL_ALIGNMENT}
{.$DEFINE DRAW_ITEM_MARGINS}

type
  TListItemAlign = (Leading, Center, Trailing);
  TListItemPurpose = (None, Header, Footer);

  TListItem = class;

  TListItemStyleResources = class;

  TListItemDrawState = (Selected, Deleting, EditMode);
  TListItemDrawStates = set of TListItemDrawState;

  TListItemObject = class(TInterfacedPersistent)
  private
    [weak]FOwner: TListItem;
    FAlign: TListItemAlign;
    FVertAlign: TListItemAlign;
    FVisible: Boolean;
    FPlaceOffset: TPosition;
    FWidth: Single;
    FHeight: Single;
    FOpacity: Single;
    FUpdating: Integer;
    NeedRepaint: Boolean;
    FOnSelect: TNotifyEvent;
    FName: string;

    procedure SetOwner(const Value: TListItem);
    procedure SetSize(const Index: Integer; const Value: Single);
    procedure SetOpacity(const Value: Single);
    procedure SetAlign(const Value: TListItemAlign);
    procedure SetVertAlign(const Value: TListItemAlign);
    procedure SetVisible(const Value: Boolean);
    procedure PlaceOffsetChanged(Sender: TObject);
    function GetData: TValue; virtual;
    procedure SetData(const Value: TValue); virtual;
  protected
    LocalRect: TRectF;

    function GetStyleResources: TListItemStyleResources; virtual;
    procedure DoResize; virtual;
    procedure DoOpacityChange; virtual;
    procedure DoSelect; virtual;
    procedure CalculateLocalRect(const DestRect: TRectF; const SceneScale: Single;
      const DrawStates: TListItemDrawStates); virtual;
    function ObjectAtPoint(const Point: TPointF): TControl; virtual;
    function GetRenderPassCount: Integer; virtual;
    procedure UpdateValuesFromStyle; virtual;

    function MouseDown(const Button: TMouseButton; const Shift: TShiftState; const MousePos: TPointF): Boolean; virtual;
    procedure MouseMove(const Shift: TShiftState; const MousePos: TPointF); virtual;
    procedure MouseUp(const Button: TMouseButton; const Shift: TShiftState; const MousePos: TPointF); virtual;
  public
    constructor Create(const AOwner: TListItem); virtual;
    destructor Destroy; override;

    procedure Invalidate;
    procedure Render(const Canvas: TCanvas; const DrawItemIndex: Integer; const DrawStates: TListItemDrawStates;
      const SubPassNo: Integer = 0); virtual; abstract;

    procedure BeginUpdate;
    procedure EndUpdate;

    property Owner: TListItem read FOwner write SetOwner;

    // Local width of list item inside its designated area.
    property Width: Single index 0 read FWidth write SetSize;
    // Local height of list item inside its designated area.
    property Height: Single index 1 read FHeight write SetSize;

    // Horizontal alignment of list item inside its designated area.
    property Align: TListItemAlign read FAlign write SetAlign;
    // Vertical alignment of list item inside its designated area.
    property VertAlign: TListItemAlign read FVertAlign write SetVertAlign;
    // Determines whether the current item is visible or not.
    property Visible: Boolean read FVisible write SetVisible;
    // The offset in logical units regarding aligned location for finer placement control.
    property PlaceOffset: TPosition read FPlaceOffset;
    property Name: string read FName write FName;

    property Opacity: Single read FOpacity write SetOpacity;

    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;

    // Polymorphic property access
    property Data: TValue read GetData write SetData;
  end;

  TListItemText = class(TListItemObject)
  private
    FFont: TFont;
    FTextLayout: TTextLayout;

    FText: string;
    FTextAlign: TTextAlign;
    FTextVertAlign: TTextAlign;
    FWordWrap: Boolean;

    LayoutChanged: Boolean;
    FTextColor: TAlphaColor;
    FSelectedTextColor: TAlphaColor;
    FTrimming: TTextTrimming;
    FTextShadowOffset: TPosition;
    FTextShadowColor: TAlphaColor;
    FIsDetailText: Boolean;

    procedure FontChanged(Sender: TObject);
    procedure TextShadowOffsetChanged(Sender: TObject);
    procedure SetText(const Value: string);
    procedure SetTextAlign(const Value: TTextAlign);
    procedure SetTextVertAlign(const Value: TTextAlign);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetTextColor(const Value: TAlphaColor);
    procedure SetTrimming(const Value: TTextTrimming);
    procedure SetSelectedTextColor(const Value: TAlphaColor);
    procedure SetTextShadowColor(const Value: TAlphaColor);
    procedure SetIsDetailText(const Value: Boolean);
    procedure SetData(const AValue: TValue); override;
    function GetData: TValue; override;
  protected
    procedure CalculateLocalRect(const DestRect: TRectF; const SceneScale: Single;
      const DrawStates: TListItemDrawStates); override;

    procedure DoResize; override;
    procedure DoOpacityChange; override;
    procedure UpdateValuesFromStyle; override;
  public
    constructor Create(const AOwner: TListItem); override;
    destructor Destroy; override;

    procedure Render(const Canvas: TCanvas; const DrawItemIndex: Integer; const DrawStates: TListItemDrawStates;
      const SubPassNo: Integer = 0); override;

    property Font: TFont read FFont;
    property Text: string read FText write SetText;

    // Horizontal text alignment inside local item rectangle.
    property TextAlign: TTextAlign read FTextAlign write SetTextAlign;
    // Vertical text alignment inside local item rectangle.
    property TextVertAlign: TTextAlign read FTextVertAlign write SetTextVertAlign;

    property WordWrap: Boolean read FWordWrap write SetWordWrap;

    property TextColor: TAlphaColor read FTextColor write SetTextColor;
    property SelectedTextColor: TAlphaColor read FSelectedTextColor write SetSelectedTextColor;

    { Text shadow color and offset. The text shadow will appear behind normal text and only when its color is
      set to non-zero value (default). This is useful for headers or other gradient fills to improve readability. }
    property TextShadowColor: TAlphaColor read FTextShadowColor write SetTextShadowColor;
    property TextShadowOffset: TPosition read FTextShadowOffset;

    property Trimming: TTextTrimming read FTrimming write SetTrimming;

    // Hints regarding the contents of this text object, which affecs visual style.
    property IsDetailText: Boolean read FIsDetailText write SetIsDetailText;
  end;

  TListItemDummy = class(TListItemObject)
  public
    procedure Render(const Canvas: TCanvas; const DrawItemIndex: Integer; const DrawStates: TListItemDrawStates;
      const SubPassNo: Integer = 0); override;
  end;

  TImageScalingMode = (smStretchWithAspect, smOriginal, smStretch);

  TListItemImage = class(TListItemObject)
  private
    StaticBitmap: TBitmap;
    [weak]ReferBitmap: TBitmap;
    FSrcRect: TRectF;
    FOwnsBitmap: Boolean;
    FImageScalingMode: TImageScalingMode;

    function GetBitmap: TBitmap;
    procedure SetBitmap(const Value: TBitmap);
    procedure SetOwnsBitmap(const Value: Boolean);
    procedure SetSrcRect(const Value: TRectF);
    procedure SetImageScalingMode(const Value: TImageScalingMode);
  protected
    procedure CalculateLocalRect(const DestRect: TRectF; const SceneScale: Single;
      const DrawStates: TListItemDrawStates); override;
  public
    constructor Create(const AOwner: TListItem); override;
    destructor Destroy; override;
    procedure Render(const Canvas: TCanvas; const DrawItemIndex: Integer; const DrawStates: TListItemDrawStates;
      const SubPassNo: Integer = 0); override;

    property Bitmap: TBitmap read GetBitmap write SetBitmap;

    { Determines whether this list owns and maintains the bitmap, or whether it is a reference only. It is must faster
      (and memory efficient) to have multiple references to a single bitmap rather than multiple copies of the same
      bitmap copied across the items. }
    property OwnsBitmap: Boolean read FOwnsBitmap write SetOwnsBitmap;

    property SrcRect: TRectF read FSrcRect write SetSrcRect;
    property ScalingMode: TImageScalingMode read FImageScalingMode write SetImageScalingMode
      default TImageScalingMode.smStretchWithAspect;
  end;

  TListItemEmbeddedControl = class;

  TListItemControlScene = class(TFmxObject, IStyleBookOwner, IScene)
  private
    FLayoutSize: TPoint;
    FCanvas: TCanvas;
    [weak]FItemOwner: TListItemEmbeddedControl;
    [weak]FContainer: TControl;
    FDrawing: Boolean;
  protected
    // TFmxObject
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoRemoveObject(const AObject: TFmxObject); override;
    // IStyleBookOwner
    function GetStyleBook: TStyleBook;
    procedure SetStyleBook(const Value: TStyleBook);
    // IScene
    function GetCanvas: TCanvas;
    function GetSceneScale: Single;
    function GetObject: TFmxObject;
    procedure AddUpdateRect(R: TRectF);
    function GetUpdateRectsCount: Integer;
    function GetUpdateRect(const Index: Integer): TRectF;
    function LocalToScreen(P: TPointF): TPointF;
    function ScreenToLocal(P: TPointF): TPointF;
    procedure ChangeScrollingState(const AControl: TControl; const Active: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure RepaintScene(const Canvas: TCanvas);

    property ItemOwner: TListItemEmbeddedControl read FItemOwner;
    property Container: TControl read FContainer;
  end;

  TListItemControlContainer = class(TControl)
  private
    [weak]FItemOwner: TListItemEmbeddedControl;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TListItemEmbeddedControl = class(TListItemObject)
  private
    FScene: TListItemControlScene;
    FContainer: TListItemControlContainer;
  protected
    function ObjectAtPoint(const Point: TPointF): TControl; override;
  public
    constructor Create(const AOwner: TListItem); override;
    destructor Destroy; override;

    procedure Render(const Canvas: TCanvas; const DrawItemIndex: Integer; const DrawStates: TListItemDrawStates;
      const SubPassNo: Integer = 0); override;

    property Container: TListItemControlContainer read FContainer;
  end;

  TListItemSimpleControl = class(TListItemObject)
  private const
    DisabledOpacity = 0.6;
  private
    FEnabled: Boolean;
    FPressed: Boolean;
    FMouseOver: Boolean;
    FOnClick: TNotifyEvent;
    FTouchExpand: Single;
    procedure SetEnabled(const Value: Boolean);
  protected
    function IsClickOpaque: Boolean; virtual;
    function MouseDown(const Button: TMouseButton; const Shift: TShiftState; const MousePos: TPointF): Boolean;
      override;
    procedure MouseMove(const Shift: TShiftState; const MousePos: TPointF); override;
    procedure MouseUp(const Button: TMouseButton; const Shift: TShiftState; const MousePos: TPointF); override;
    procedure DoClick; virtual;
    procedure DoEnabledChange; virtual;
  public
    constructor Create(const AOwner: TListItem); override;
    function PointInLocalRect(const Pos: TPointF): Boolean;

    property Enabled: Boolean read FEnabled write SetEnabled;
    property Pressed: Boolean read FPressed;
    property MouseOver: Boolean read FMouseOver;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;

    // Additional area (in logical units) around the control that is sensitive to touch.
    property TouchExpand: Single read FTouchExpand write FTouchExpand;
  end;

  TAccessoryType = (More, Checkmark, Detail);

  TListItemAccessory = class(TListItemObject)
  private
    FAccessoryType: TAccessoryType;
  protected
    procedure CalculateLocalRect(const DestRect: TRectF; const SceneScale: Single;
      const DrawStates: TListItemDrawStates); override;
  public
    constructor Create(const AOwner: TListItem); override;

    procedure Render(const Canvas: TCanvas; const DrawItemIndex: Integer; const DrawStates: TListItemDrawStates;
      const SubPassNo: Integer = 0); override;

    property AccessoryType: TAccessoryType read FAccessoryType write FAccessoryType;
  end;

  TGlyphButtonType = (Add, Delete, Checkbox);

  TListItemGlyphButton = class(TListItemSimpleControl)
  private const
    CheckedAnimationFrameRate = 60;
    CheckedAnimationDuration = 0.15; // in seconds
  private
    FButtonType: TGlyphButtonType;

    FClickOnSelect: Boolean;
    FChecked: Boolean;
    FOnChange: TNotifyEvent;
    TransitionEnabled: Boolean;
    TransitionAlpha: Single;
    TransitionTimer: TTimer;
    TransitionStartTicks: Extended;
    TimerService: IFMXTimerService;

    procedure SetButtonType(const Value: TGlyphButtonType);
    procedure SetChecked(const Value: Boolean);
    procedure InitCheckedTransition;
    procedure ResetCheckedTransition;
    procedure TransitionTimerNotify(Sender: TObject);
  protected
    procedure CalculateLocalRect(const DestRect: TRectF; const SceneScale: Single;
      const DrawStates: TListItemDrawStates); override;

    procedure DoSelect; override;
    procedure DoClick; override;
    function MouseDown(const Button: TMouseButton; const Shift: TShiftState; const MousePos: TPointF): Boolean;
      override;
    procedure DoChange; virtual;
    procedure SetData(const AValue: TValue); override;
  public
    constructor Create(const AOwner: TListItem); override;
    destructor Destroy; override;

    procedure Render(const Canvas: TCanvas; const DrawItemIndex: Integer; const DrawStates: TListItemDrawStates;
      const SubPassNo: Integer = 0); override;

    property ButtonType: TGlyphButtonType read FButtonType write SetButtonType;

    // If set to True, this button will receive click events from entire list by using of selection.
    property ClickOnSelect: Boolean read FClickOnSelect write FClickOnSelect;
    // Determines whether checkbox is checked, has no effect for other button types.
    property Checked: Boolean read FChecked write SetChecked;
    // This event occurs when Checked property has been changed.
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TTextButtonType = (Normal, Delete);

  TListItemTextButton = class(TListItemSimpleControl)
  private
    FButtonType: TTextButtonType;

    FText: string;
    FFont: TFont;
    FTextLayout: TTextLayout;

    FTextAlign: TTextAlign;
    FTextVertAlign: TTextAlign;
    FWordWrap: Boolean;

    LayoutChanged: Boolean;
    FTextColor: TAlphaColor;
    FTrimming: TTextTrimming;
    FTextShadowOffset: TPosition;
    FTextShadowColor: TAlphaColor;
    FPressedTextColor: TAlphaColor;

    procedure SetButtonType(const Value: TTextButtonType);
    procedure FontChanged(Sender: TObject);
    procedure TextShadowOffsetChanged(Sender: TObject);
    procedure SetText(const Value: string);
    procedure SetTextAlign(const Value: TTextAlign);
    procedure SetTextVertAlign(const Value: TTextAlign);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetTextColor(const Value: TAlphaColor);
    procedure SetTrimming(const Value: TTextTrimming);
    procedure SetTextShadowColor(const Value: TAlphaColor);
    procedure SetPressedTextColor(const Value: TAlphaColor);
  protected
    function IsClickOpaque: Boolean; override;
    procedure UpdateValuesFromStyle; override;
    procedure DoResize; override;
    procedure DoOpacityChange; override;
    procedure DoEnabledChange; override;
    function GetRenderPassCount: Integer; override;
    procedure SetData(const AValue: TValue); override;
  public
    constructor Create(const AOwner: TListItem); override;
    destructor Destroy; override;

    procedure Render(const Canvas: TCanvas; const DrawItemIndex: Integer; const DrawStates: TListItemDrawStates;
      const SubPassNo: Integer = 0); override;

    property ButtonType: TTextButtonType read FButtonType write SetButtonType;

    property Font: TFont read FFont;
    property Text: string read FText write SetText;

    property TextAlign: TTextAlign read FTextAlign write SetTextAlign;
    property TextVertAlign: TTextAlign read FTextVertAlign write SetTextVertAlign;

    property WordWrap: Boolean read FWordWrap write SetWordWrap;

    property TextColor: TAlphaColor read FTextColor write SetTextColor;
    property PressedTextColor: TAlphaColor read FPressedTextColor write SetPressedTextColor;

    property TextShadowColor: TAlphaColor read FTextShadowColor write SetTextShadowColor;
    property TextShadowOffset: TPosition read FTextShadowOffset;

    property Trimming: TTextTrimming read FTrimming write SetTrimming;
  end;

  TListItemObjects = class
  private type
    TItemList = TObjectList<TListItemObject>;
  private
    [weak] FOwner: TListItem;
    function GetCount: Integer;
    function GetObject(Index: Integer): TListItemObject;
    function GetItemList: TItemList; inline;
  protected
    property Owner: TListItem read FOwner;
    procedure Include(const AItem: TListItemObject);
    procedure Exclude(const AItem: TListItemObject);
    property ItemList: TItemList read GetItemList;
  public
    constructor Create(const AOwner: TListItem);
    destructor Destroy; override;
    function Add(const AItem: TListItemObject): Integer;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    function FindObject(const AName: string): TListItemObject;
    function ObjectByName(const AName: string): TListItemObject;
    property Count: Integer read GetCount;
    property Objects[Index: Integer]: TListItemObject read GetObject; default;
  end;

  TListItem = class
  private type
    TItemList = TListItemObjects.TItemList;
  public type
    TListItemObjectsType = class of TListItemObjects;
  private
    FItemList: TItemList;
    FHeight: Integer;
    FPurpose: TListItemPurpose;
    FUpdating: Integer;
    NeedRepaint: Boolean;
    FObjects: TListItemObjects;

    function GetCount: Integer;
    procedure SetHeight(const Value: Integer);
  protected
    [weak]FParent: TControl;
    FHeaderRef: Integer;

    procedure InvalidateHeights; virtual;
    procedure SetPurpose(const AValue: TListItemPurpose); virtual;
    procedure Repaint; virtual;

    function ObjectAtPoint(const Point: TPointF): TControl;

    function MouseDown(const Button: TMouseButton; const Shift: TShiftState; const MousePos: TPointF): Boolean;
    procedure MouseMove(const Shift: TShiftState; const MousePos: TPointF);
    procedure MouseUp(const Button: TMouseButton; const Shift: TShiftState; const MousePos: TPointF);
    procedure MouseSelect;

    function HasClickOnSelectItems: Boolean;
    function ListItemObjectsClass: TListItemObjectsType; virtual;
  public
    procedure Invalidate;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure CreateObjects; virtual;

    constructor Create;
    destructor Destroy; override;

    property Parent: TControl read FParent;

    property Count: Integer read GetCount;
    property Objects: TListItemObjects read FObjects;

    property Height: Integer read FHeight write SetHeight;

    // Which purpose item serves for: normal item (none), header or footer.
    property Purpose: TListItemPurpose read FPurpose write SetPurpose;
  end;

  TListItemStyleResources = class
  public type
    TAccessoryStyleObject = record
      [weak]Normal: TStyleObject;
      [weak]Selected: TStyleObject;
    end;

    TButtonStyleObject = record
      [weak]Normal: TStyleObject;
      [weak]Pressed: TStyleObject;
    end;
  public
    AccessoryImages: array [TAccessoryType] of TAccessoryStyleObject;
    HeaderTextFont: TFont;
    HeaderTextColor: TAlphaColor;
    HeaderTextShadowColor: TAlphaColor;
    DefaultTextFont: TFont;
    DefaultTextColor: TAlphaColor;
    DetailTextFont: TFont;
    DetailTextColor: TAlphaColor;
    DefaultTextSelectedColor: TAlphaColor;
    ButtonAddItemStyleImage: TButtonStyleObject;
    ButtonDeleteItemStyleImage: TButtonStyleObject;
    ButtonNormalStyleImage: TButtonStyleObject;
    ButtonDeleteStyleImage: TButtonStyleObject;
    ButtonCheckboxStyleImage: TButtonStyleObject;
    ButtonTextFont: TFont;
    ButtonTextColor: TAlphaColor;
    ButtonTextPressedColor: TAlphaColor;
    DeleteButtonTextFont: TFont;
    DeleteButtonTextColor: TAlphaColor;
    DeleteButtonTextPressedColor: TAlphaColor;
    constructor Create;
    destructor Destroy; override;
  end;

  IListItemStyleResources = interface
    ['{0328C6F1-432C-4F8B-994B-7AB2543CD172}']
    function GetStyleResources: TListItemStyleResources;
    property StyleResources: TListItemStyleResources read GetStyleResources;
  end;

  IListViewController = interface
    ['{3855EF72-3B32-41BE-9068-7B109B2DD8E5}']
    function GetEditModeTransitionAlpha: Single;
    function GetDeleteModeTransitionAlpha: Single;

    procedure FeedbackItemClickChecked(const Item: TListItem; const Checked: Boolean);
    function GetItemEditOffset(const Item: TListItem): Single;
    function GetItemDeleteCutoff(const Item: TListItem): Single;
    function GetClientMargins: TRectF;

    property EditModeTransitionAlpha: Single read GetEditModeTransitionAlpha;
    property DeleteModeTransitionAlpha: Single read GetDeleteModeTransitionAlpha;
  end;

implementation

uses
  System.RTLConsts, System.UIConsts, System.SysUtils, System.Math, FMX.Styles, FMX.Messages, FMX.Platform;

{$REGION 'Global Functions'}

const
  GlyphOffsetX = -3;
  MinusOffsetX = 7;
  OpacityEpsilon = 0.003921569; // 1 / 255
  AnimationDeltaEpsilon = 0.01;

type
  TOpenControl = class(TControl);
  TOpenStyledControl = class(TStyledControl);

{$IFDEF DRAW_ITEM_MARGINS}
var
  TempBrush: TBrush = nil;

function MarginBrush: TBrush;
begin
  if not Assigned(TempBrush) then
    TempBrush := TBrush.Create(TBrushKind.bkSolid, $50808080);

  Result := TempBrush;
end;

{$ENDIF}

function GetDeletingUnwantedOpacity(const Parent: TControl): Single;
var
  Controller: IListViewController;
begin
  if Supports(Parent, IListViewController, Controller) then
    if Controller.DeleteModeTransitionAlpha > AnimationDeltaEpsilon then
      Result := Max(0, 1 - (Controller.DeleteModeTransitionAlpha * 2))
    else
      Result := 1
  else
    Result := 1;
end;

{$ENDREGION}
{$REGION 'List Item Object'}

constructor TListItemObject.Create(const AOwner: TListItem);
begin
  inherited Create;

  FPlaceOffset := TPosition.Create(TPointF.Create(0, 0));
  FPlaceOffset.OnChange := PlaceOffsetChanged;

  FOwner := AOwner;

  if Assigned(FOwner) then
    FOwner.Objects.Include(Self);

  FWidth := 0;
  FHeight := 0;
  FVisible := True;

  FAlign := TListItemAlign.Leading;
  FVertAlign := TListItemAlign.Leading;

  FUpdating := 0;
  NeedRepaint := False;
  FOpacity := 1.0;
end;

destructor TListItemObject.Destroy;
begin
  FPlaceOffset.Free;

  inherited;
end;

procedure TListItemObject.DoResize;
begin
  Invalidate;
end;

procedure TListItemObject.DoOpacityChange;
begin
  Invalidate;
end;

procedure TListItemObject.DoSelect;
begin
  if Assigned(FOnSelect) then
    FOnSelect(Self);
end;

procedure TListItemObject.SetOwner(const Value: TListItem);
begin
  if FOwner <> Value then
  begin
    if Assigned(FOwner) then
      FOwner.Objects.Exclude(Self);

    FOwner := Value;

    if Assigned(FOwner) then
      FOwner.Objects.Include(Self);
  end;
end;

procedure TListItemObject.SetSize(const Index: Integer; const Value: Single);
var
  NewValue: Single;
begin
  NewValue := Value;
  if NewValue < 0 then
    NewValue := 0;

  case Index of
    0:
      if FWidth <> NewValue then
      begin
        FWidth := NewValue;
        DoResize;
      end;
    1:
      if FHeight <> NewValue then
      begin
        FHeight := NewValue;
        DoResize;
      end;
  end;
end;

procedure TListItemObject.SetAlign(const Value: TListItemAlign);
begin
  if FAlign <> Value then
  begin
    FAlign := Value;
    Invalidate;
  end;
end;

procedure TListItemObject.SetData(const Value: TValue);
begin
  //
end;

procedure TListItemObject.SetVertAlign(const Value: TListItemAlign);
begin
  if FVertAlign <> Value then
  begin
    FVertAlign := Value;
    Invalidate;
  end;
end;

procedure TListItemObject.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Invalidate;
  end;
end;

procedure TListItemObject.UpdateValuesFromStyle;
begin

end;

procedure TListItemObject.SetOpacity(const Value: Single);
var
  NewValue: Single;
begin
  NewValue := Value;
  if NewValue < 0 then
    NewValue := 0;
  if NewValue > 1 then
    NewValue := 1;

  if FOpacity <> NewValue then
  begin
    FOpacity := NewValue;
    DoOpacityChange;
  end;
end;

procedure TListItemObject.PlaceOffsetChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TListItemObject.CalculateLocalRect(const DestRect: TRectF; const SceneScale: Single;
  const DrawStates: TListItemDrawStates);

{$IFDEF PIXEL_ALIGNMENT}
  function PixelAlign(const Value: Single): Single;
  begin
    Result := Int(Value * SceneScale) / SceneScale
  end;
{$ELSE}
  function PixelAlign(const Value: Single): Single; inline;
  begin
    Result := Value;
  end;
{$ENDIF}

  procedure AlignValue(Align: TListItemAlign; out Left, Right: Single;
    Width, DesLeft, DesRight, CustomOffset: Single); inline;
  begin
    case Align of
      TListItemAlign.Center:
        begin
          Left := PixelAlign(CustomOffset + (DesLeft + DesRight - Width) * 0.5);
          Right := Left + Width;
        end;

      TListItemAlign.Trailing:
        begin
          Right := PixelAlign(CustomOffset + DesRight);
          Left := Right - Width;
        end;

    else
      begin
        Left := PixelAlign(CustomOffset + DesLeft);
        Right := Left + Width;
      end;
    end;
  end;

begin
  if (FWidth > 0) and (FWidth < DestRect.Width) then
  begin
    AlignValue(FAlign, LocalRect.Left, LocalRect.Right, FWidth, DestRect.Left, DestRect.Right, FPlaceOffset.X);
  end
  else
  begin
    LocalRect.Left := PixelAlign(DestRect.Left + FPlaceOffset.X);
    LocalRect.Right := LocalRect.Left + DestRect.Width;
  end;

  if (FHeight > 0) and (FHeight < DestRect.Height) then
  begin
    AlignValue(FVertAlign, LocalRect.Top, LocalRect.Bottom, FHeight, DestRect.Top, DestRect.Bottom, FPlaceOffset.Y);
  end
  else
  begin
    LocalRect.Top := PixelAlign(DestRect.Top + FPlaceOffset.Y);
    LocalRect.Bottom := LocalRect.Top + DestRect.Height;
  end;
end;

procedure TListItemObject.BeginUpdate;
begin
  Inc(FUpdating);
end;

procedure TListItemObject.EndUpdate;
begin
  if FUpdating > 0 then
  begin
    Dec(FUpdating);
    if (FUpdating <= 0) and NeedRepaint then
    begin
      NeedRepaint := False;
      Invalidate;
    end;
  end;
end;

procedure TListItemObject.Invalidate;
begin
  if FUpdating < 1 then
  begin
    if Assigned(FOwner) then
      FOwner.Invalidate;
  end
  else
    NeedRepaint := True;
end;


function TListItemObject.ObjectAtPoint(const Point: TPointF): TControl;
begin
  Result := nil;
end;

function TListItemObject.GetData: TValue;
begin
  Result := TValue.Empty;
end;

function TListItemObject.GetStyleResources: TListItemStyleResources;
var
  Intf: IListItemStyleResources;
begin
  if Assigned(Owner) and Supports(Owner.Parent, IListItemStyleResources, Intf) then
    Result := Intf.StyleResources
  else
    Result := nil;
end;

function TListItemObject.GetRenderPassCount: Integer;
begin
  Result := 0;
end;

function TListItemObject.MouseDown(const Button: TMouseButton; const Shift: TShiftState;
  const MousePos: TPointF): Boolean;
begin
  Result := False;
end;

procedure TListItemObject.MouseMove(const Shift: TShiftState; const MousePos: TPointF);
begin
end;

procedure TListItemObject.MouseUp(const Button: TMouseButton; const Shift: TShiftState; const MousePos: TPointF);
begin
end;

{$ENDREGION}
{$REGION 'List Item Text'}

constructor TListItemText.Create(const AOwner: TListItem);
begin
  inherited;

  FTextShadowOffset := TPosition.Create(PointF(0, 1));
  FTextShadowOffset.OnChange := TextShadowOffsetChanged;

  FFont := TFont.Create;
  FFont.Family := 'Helvetica';
  FFont.Size := 14;

  FTextColor := claBlack;
  FSelectedTextColor := claWhite;
  FTextShadowColor := 0;
  FTextAlign := TTextAlign.taLeading;
  FTextVertAlign := TTextAlign.taLeading;

  UpdateValuesFromStyle;

  FFont.OnChanged := FontChanged;

  FWordWrap := False;
  FTrimming := TTextTrimming.ttNone;
end;

destructor TListItemText.Destroy;
begin
  if Assigned(FTextLayout) then
    FreeAndNil(FTextLayout);

  FFont.Free;
  FTextShadowOffset.Free;

  inherited;
end;

procedure TListItemText.UpdateValuesFromStyle;
var
  Resources: TListItemStyleResources;
begin
  Resources := GetStyleResources;
  if Resources <> nil then
  begin
    if Owner.Purpose <> TListItemPurpose.None then
    begin
      if Assigned(Resources.HeaderTextFont) then
        FFont.Assign(Resources.HeaderTextFont);

      if Resources.HeaderTextColor > 0 then
        FTextColor := Resources.HeaderTextColor;

      if Resources.HeaderTextShadowColor > 0 then
        FTextShadowColor := Resources.HeaderTextShadowColor;

      FTextVertAlign := TTextAlign.taCenter;
    end
    else if FIsDetailText then
    begin
      if Assigned(Resources.DetailTextFont) then
        FFont.Assign(Resources.DetailTextFont);

      if Resources.DetailTextColor > 0 then
        FTextColor := Resources.DetailTextColor;

      if Resources.DefaultTextSelectedColor > 0 then
        FSelectedTextColor := Resources.DefaultTextSelectedColor;
    end
    else
    begin
      if Assigned(Resources.DefaultTextFont) then
        FFont.Assign(Resources.DefaultTextFont);

      if Resources.DefaultTextColor > 0 then
        FTextColor := Resources.DefaultTextColor;

      if Resources.DefaultTextSelectedColor > 0 then
        FSelectedTextColor := Resources.DefaultTextSelectedColor;
    end;
  end;
end;

procedure TListItemText.DoResize;
begin
  inherited;

  LayoutChanged := True;
end;

procedure TListItemText.DoOpacityChange;
begin
  inherited;

  LayoutChanged := True;
end;

procedure TListItemText.FontChanged(Sender: TObject);
begin
  LayoutChanged := True;
  Invalidate;
end;

function TListItemText.GetData: TValue;
begin
  Result := Text;
end;

procedure TListItemText.TextShadowOffsetChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TListItemText.SetTextShadowColor(const Value: TAlphaColor);
begin
  if FTextShadowColor <> Value then
  begin
    FTextShadowColor := Value;
    Invalidate;
  end;
end;

procedure TListItemText.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    LayoutChanged := True;
    Invalidate;
  end;
end;

procedure TListItemText.SetTextAlign(const Value: TTextAlign);
begin
  if FTextAlign <> Value then
  begin
    FTextAlign := Value;
    LayoutChanged := True;
    Invalidate;
  end;
end;

procedure TListItemText.SetTextColor(const Value: TAlphaColor);
begin
  if FTextColor <> Value then
  begin
    FTextColor := Value;
    LayoutChanged := True;
    Invalidate;
  end;
end;

procedure TListItemText.SetData(const AValue: TValue);
begin
  if AValue.IsEmpty then
    Text := ''
  else
    Text := AValue.ToString;
end;

procedure TListItemText.SetIsDetailText(const Value: Boolean);
begin
  if FIsDetailText <> Value then
  begin
    FIsDetailText := Value;
    UpdateValuesFromStyle;
  end;
end;

procedure TListItemText.SetSelectedTextColor(const Value: TAlphaColor);
begin
  if FSelectedTextColor <> Value then
  begin
    FSelectedTextColor := Value;
    LayoutChanged := True;
    Invalidate;
  end;
end;

procedure TListItemText.SetTextVertAlign(const Value: TTextAlign);
begin
  if FTextVertAlign <> Value then
  begin
    FTextVertAlign := Value;
    LayoutChanged := True;
    Invalidate;
  end;
end;

procedure TListItemText.SetTrimming(const Value: TTextTrimming);
begin
  if FTrimming <> Value then
  begin
    FTrimming := Value;
    LayoutChanged := True;
    Invalidate;
  end;
end;

procedure TListItemText.SetWordWrap(const Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    LayoutChanged := True;
    Invalidate;
  end;
end;

procedure TListItemText.CalculateLocalRect(const DestRect: TRectF; const SceneScale: Single;
  const DrawStates: TListItemDrawStates);
var
  NewRect: TRectF;
  Controller: IListViewController;
  DeadSpace: Single;
begin
  if TListItemDrawState.EditMode in DrawStates then
  begin
    NewRect := DestRect;

    if Supports(Owner.Parent, IListViewController, Controller) then
    begin
      DeadSpace := Controller.GetItemEditOffset(Owner) * Controller.EditModeTransitionAlpha;
      NewRect.Left := NewRect.Left + DeadSpace;

      if FIsDetailText then
        NewRect.Right := Max(NewRect.Right + DeadSpace, NewRect.Left);
    end
    else
      Controller := nil;

    inherited CalculateLocalRect(NewRect, SceneScale, DrawStates);

    // Text should no go over right client area when moved in edit mode.
    if Assigned(Controller) then
      LocalRect.Right := Min(LocalRect.Right, Controller.GetClientMargins.Right);
  end
  else
    inherited;

  { Delete button cuts through the entire right side, so it needs to be marginalized globally.
    This is because LocalRect may actually leave the boundaries of DestRect depending on PlacementOffset. }
  if (TListItemDrawState.Deleting in DrawStates) and (not FIsDetailText) and Supports(Owner.Parent,
    IListViewController, Controller) then
    LocalRect.Right := Min(LocalRect.Right, Controller.GetItemDeleteCutoff(Owner));
end;

procedure TListItemText.Render(const Canvas: TCanvas; const DrawItemIndex: Integer;
  const DrawStates: TListItemDrawStates; const SubPassNo: Integer);
var
  CurColor: TAlphaColor;
begin
  if SubPassNo <> 0 then
    Exit;

  if FIsDetailText and (TListItemDrawState.Deleting in DrawStates) then
    Exit;

  if not Assigned(FTextLayout) then
  begin
    FTextLayout := TTextLayoutManager.TextLayoutByCanvas(Canvas.ClassType).Create(Canvas);
    LayoutChanged := True
  end;

  if not LayoutChanged then
    LayoutChanged := (FTextLayout.MaxSize.X <> LocalRect.Width) or (FTextLayout.MaxSize.Y <> LocalRect.Height);

  CurColor := FTextColor;
  if TListItemDrawState.Selected in DrawStates then
    CurColor := FSelectedTextColor;

  if (not LayoutChanged) and (FTextLayout.Color <> CurColor) then
    LayoutChanged := True;

  if LayoutChanged then
  begin
    FTextLayout.BeginUpdate;
    FTextLayout.Opacity := FOpacity;
    FTextLayout.HorizontalAlign := FTextAlign;
    FTextLayout.VerticalAlign := FTextVertAlign;
    FTextLayout.Font := FFont;
    FTextLayout.Color := CurColor;
    FTextLayout.RightToLeft := False;
    FTextLayout.MaxSize := PointF(LocalRect.Width, LocalRect.Height);
    FTextLayout.Text := FText;
    FTextLayout.Trimming := FTrimming;
    FTextLayout.WordWrap := FWordWrap;
    FTextLayout.EndUpdate;

    LayoutChanged := False;
  end;

  if TAlphaColorRec(FTextShadowColor).A > 0 then
  begin
    FTextLayout.BeginUpdate;
    FTextLayout.Color := FTextShadowColor;
    FTextLayout.TopLeft := LocalRect.TopLeft + FTextShadowOffset.Point;
    FTextLayout.EndUpdate;
    FTextLayout.RenderLayout(Canvas);

    FTextLayout.BeginUpdate;
    FTextLayout.Color := CurColor;
    FTextLayout.EndUpdate;
  end;

{$IFDEF DRAW_ITEM_MARGINS}
  if FIsDetailText then
    MarginBrush.Color := $50FF0000
  else
    MarginBrush.Color := $5000FF00;

  Canvas.FillRect(LocalRect, 0, 0, AllCorners, 1, MarginBrush);
{$ENDIF}

  FTextLayout.BeginUpdate;
  FTextLayout.TopLeft := LocalRect.TopLeft;
  FTextLayout.EndUpdate;
  FTextLayout.RenderLayout(Canvas);
end;

{$ENDREGION}
{$REGION 'List Item Dummy'}

procedure TListItemDummy.Render(const Canvas: TCanvas; const DrawItemIndex: Integer;
  const DrawStates: TListItemDrawStates; const SubPassNo: Integer);
begin
end;

{$ENDREGION}
{$REGION 'List Item Image'}

constructor TListItemImage.Create(const AOwner: TListItem);
begin
  inherited;

  StaticBitmap := nil;
  ReferBitmap := nil;

  FSrcRect := TRectF.Create(0.0, 0.0, 0.0, 0.0);
  FOwnsBitmap := False;
  FImageScalingMode := TImageScalingMode.smStretchWithAspect;
end;

destructor TListItemImage.Destroy;
begin
  StaticBitmap.Free;
  inherited;
end;

function TListItemImage.GetBitmap: TBitmap;
begin
  if FOwnsBitmap then
    Result := StaticBitmap
  else
    Result := ReferBitmap;
end;

procedure TListItemImage.SetBitmap(const Value: TBitmap);
begin
  if FOwnsBitmap then
    StaticBitmap := Value
  else
    ReferBitmap := Value;

  Invalidate;
end;

procedure TListItemImage.SetOwnsBitmap(const Value: Boolean);
begin
  if FOwnsBitmap = Value then
    Exit;

  if FOwnsBitmap and (not Value) then
  begin
    ReferBitmap := StaticBitmap;
    StaticBitmap := nil;
  end
  else if (not FOwnsBitmap) and Value then
  begin
    StaticBitmap := ReferBitmap;
    ReferBitmap := nil;
  end;

  FOwnsBitmap := Value;
end;

procedure TListItemImage.SetImageScalingMode(const Value: TImageScalingMode);
begin
  if FImageScalingMode <> Value then
  begin
    FImageScalingMode := Value;
    Invalidate;
  end;
end;

procedure TListItemImage.SetSrcRect(const Value: TRectF);
begin
  if FSrcRect <> Value then
  begin
    FSrcRect := Value;
    Invalidate;
  end;
end;

procedure TListItemImage.CalculateLocalRect(const DestRect: TRectF; const SceneScale: Single;
  const DrawStates: TListItemDrawStates);
var
  NewRect: TRectF;
  Controller: IListViewController;
  DeadSpace: Single;
begin
  if TListItemDrawState.EditMode in DrawStates then
  begin
    NewRect := DestRect;

    if Supports(Owner.Parent, IListViewController, Controller) then
    begin
      DeadSpace := Controller.GetItemEditOffset(Owner) * Controller.EditModeTransitionAlpha;
      NewRect.Left := NewRect.Left + DeadSpace;
    end;

    inherited CalculateLocalRect(NewRect, SceneScale, DrawStates);
  end
  else
    inherited;
end;

procedure TListItemImage.Render(const Canvas: TCanvas; const DrawItemIndex: Integer;
  const DrawStates: TListItemDrawStates; const SubPassNo: Integer);

  procedure ClipRects(var InpRect, DestRect: TRectF; const LocalRect: TRectF);
  var
    Delta: Single;
  begin
    if DestRect.Right > LocalRect.Right then
    begin
      Delta := 1 - ((DestRect.Right - LocalRect.Right) / DestRect.Width);

      InpRect.Right := InpRect.Left + InpRect.Width * Delta;
      DestRect.Right := LocalRect.Right;
    end;

    if DestRect.Bottom > LocalRect.Bottom then
    begin
      Delta := 1 - ((DestRect.Bottom - LocalRect.Bottom) / DestRect.Height);

      InpRect.Bottom := InpRect.Top + InpRect.Height * Delta;
      DestRect.Bottom := LocalRect.Bottom;
    end;
  end;

var
  Bitmap: TBitmap;
  LocRect, InpRect, DestRect: TRectF;
  Aspect: Single;
  XAspect, YAspect: Single;
begin
  if SubPassNo <> 0 then
    Exit;

{$IFDEF DRAW_ITEM_MARGINS}
  MarginBrush.Color := $50808000;
  Canvas.FillRect(LocalRect, 0, 0, AllCorners, 1, MarginBrush);
{$ENDIF}

  Bitmap := GetBitmap;
  if not Assigned(Bitmap) then
    Exit;

  LocRect := LocalRect;

  if FSrcRect.Width < 1 then
  begin
    InpRect.Left := 0;
    InpRect.Right := Bitmap.Width;
  end
  else
  begin
    InpRect.Left := FSrcRect.Left;
    InpRect.Right := FSrcRect.Right;
  end;

  if FSrcRect.Height < 1 then
  begin
    InpRect.Top := 0;
    InpRect.Bottom := Bitmap.Height;
  end
  else
  begin
    InpRect.Top := FSrcRect.Top;
    InpRect.Bottom := FSrcRect.Bottom;
  end;

  case FImageScalingMode of
    TImageScalingMode.smOriginal:
      begin
        DestRect.Left := LocRect.Left;
        DestRect.Top := LocRect.Top;
        DestRect.Right := DestRect.Left + InpRect.Width;
        DestRect.Bottom := DestRect.Top + InpRect.Height;

        ClipRects(InpRect, DestRect, LocRect);

        // Center image
        DestRect.Offset((LocalRect.Right - DestRect.Right) / 2, (LocalRect.Bottom - DestRect.Bottom) / 2);
      end;

    TImageScalingMode.smStretch:
      DestRect := LocRect;

    TImageScalingMode.smStretchWithAspect:
    begin
      // Calc ratios
      if InpRect.Width > 0 then
        XAspect := LocalRect.Width / InpRect.Width
      else
        XAspect := 1;

      if InpRect.Height > 0 then
        YAspect := LocalRect.Height / InpRect.Height
      else
        YAspect := 1;

      // Use smallest ratio
      if YAspect < XAspect then
        Aspect := YAspect
      else
        Aspect := XAspect;

      DestRect := LocalRect;
      DestRect.Right := DestRect.Left + InpRect.Width * Aspect;
      DestRect.Bottom := DestRect.Top + InpRect.Height * Aspect;
      // Center image
      DestRect.Offset((LocalRect.Right - DestRect.Right) / 2, (LocalRect.Bottom - DestRect.Bottom) / 2);
    end;
  end;

  Canvas.DrawBitmap(Bitmap, InpRect, DestRect, FOpacity);
end;

{$ENDREGION}
{$REGION 'List Item Simple Control'}

constructor TListItemSimpleControl.Create(const AOwner: TListItem);
begin
  inherited;

  FEnabled := True;

{$IF DEFINED(IOS) OR DEFINED(ANDROID)}
  FTouchExpand := 8;
{$ENDIF}
end;

procedure TListItemSimpleControl.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    DoEnabledChange;
  end;
end;

function TListItemSimpleControl.PointInLocalRect(const Pos: TPointF): Boolean;
var
  LocRect: TRectF;
begin
  LocRect := LocalRect;
  LocRect.Inflate(FTouchExpand, FTouchExpand);

  Result := LocRect.Contains(Pos);
end;

function TListItemSimpleControl.IsClickOpaque: Boolean;
begin
  Result := True;
end;

function TListItemSimpleControl.MouseDown(const Button: TMouseButton; const Shift: TShiftState;
  const MousePos: TPointF): Boolean;
begin
  Result := False;

  if (Button = TMouseButton.mbLeft) and FEnabled and PointInLocalRect(MousePos) and IsClickOpaque then
  begin
      FPressed := True;
    FMouseOver := True;

    Invalidate;

    Result := True;
  end;
end;

procedure TListItemSimpleControl.MouseMove(const Shift: TShiftState; const MousePos: TPointF);
begin
// On iOS, once "mouse" is outside of control area, the control never regains pressed/over state, requiring a new tap.
//  FMouseOver := FMouseOver and PointInLocalRect(MousePos);
  FMouseOver := PointInLocalRect(MousePos);
  Invalidate;
end;

procedure TListItemSimpleControl.MouseUp(const Button: TMouseButton; const Shift: TShiftState; const MousePos: TPointF);
var
  ShouldClick: Boolean;
begin
  if FPressed then
  begin
    ShouldClick := FMouseOver;

    FPressed := False;
    FMouseOver := False;

    if ShouldClick then
      DoClick;

    Invalidate;
  end;
end;

procedure TListItemSimpleControl.DoClick;
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TListItemSimpleControl.DoEnabledChange;
begin
  Invalidate;
end;

{$ENDREGION}
{$REGION 'List Item Accessory'}

constructor TListItemAccessory.Create(const AOwner: TListItem);
begin
  inherited;

end;

procedure TListItemAccessory.CalculateLocalRect(const DestRect: TRectF; const SceneScale: Single;
  const DrawStates: TListItemDrawStates);
var
  NewRect: TRectF;
  Controller: IListViewController;
begin
  if TListItemDrawState.EditMode in DrawStates then
  begin
    NewRect := DestRect;

    if Supports(Owner.Parent, IListViewController, Controller) then
      NewRect.Offset(Controller.GetItemEditOffset(Owner) * Controller.EditModeTransitionAlpha, 0);

    inherited CalculateLocalRect(NewRect, SceneScale, DrawStates);
  end
  else
    inherited;
end;

procedure TListItemAccessory.Render(const Canvas: TCanvas; const DrawItemIndex: Integer;
  const DrawStates: TListItemDrawStates; const SubPassNo: Integer);
var
  Image: TStyleObject;
  Resources: TListItemStyleResources;
  Controller: IListViewController;
  TempOpacity: Single;
begin
  if (SubPassNo <> 0) or (TListItemDrawState.Deleting in DrawStates) then
    Exit;

  TempOpacity := FOpacity;

  if (TListItemDrawState.EditMode in DrawStates) and Supports(Owner.Parent, IListViewController, Controller) then
    TempOpacity := TempOpacity * Max(0, 1 - (Controller.EditModeTransitionAlpha * 2));

  if TempOpacity < OpacityEpsilon then
    Exit;

{$IFDEF DRAW_ITEM_MARGINS}
  MarginBrush.Color := $500040FF;
  Canvas.FillRect(LocalRect, 0, 0, AllCorners, 1, MarginBrush);
{$ENDIF}

  Resources := GetStyleResources;
  if Assigned(Resources) then
  begin
    if TListItemDrawState.Selected in DrawStates then
      Image := Resources.AccessoryImages[FAccessoryType].Selected
    else
      Image := Resources.AccessoryImages[FAccessoryType].Normal;

    if Assigned(Image) then
      Image.DrawToCanvas(Canvas, LocalRect, TempOpacity);
  end;
end;

{$ENDREGION}
{$REGION 'List Item Glyph Button'}

constructor TListItemGlyphButton.Create(const AOwner: TListItem);
begin
  inherited;

  TransitionEnabled := False;
  TransitionAlpha := 0;
end;

destructor TListItemGlyphButton.Destroy;
begin
  if Assigned(TransitionTimer) then
    FreeAndNil(TransitionTimer);

  inherited;
end;

procedure TListItemGlyphButton.SetButtonType(const Value: TGlyphButtonType);
begin
  if FButtonType <> Value then
  begin
    FButtonType := Value;

    Invalidate;
  end;
end;

procedure TListItemGlyphButton.SetData(const AValue: TValue);
var
  LChecked: Boolean;
begin
  if AValue.TryAsType<Boolean>(LChecked) then
    Checked := LChecked;
end;

procedure TListItemGlyphButton.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TListItemGlyphButton.InitCheckedTransition;
begin
  TransitionEnabled := True;

  TPlatformServices.Current.SupportsPlatformService(IFMXTimerService, IInterface(TimerService));

  if not Assigned(TransitionTimer) then
  begin
    TransitionTimer := TTimer.Create(nil);

    if Assigned(Owner) and Assigned(Owner.Parent) then
      TransitionTimer.Parent := Owner.Parent;

    TransitionTimer.Enabled := False;
    TransitionTimer.Interval := Round(1000 / CheckedAnimationFrameRate);
    TransitionTimer.OnTimer := TransitionTimerNotify;
  end;

  TransitionTimer.Enabled := True;

  if Assigned(TimerService) then
    TransitionStartTicks := TimerService.GetTick
  else
    TransitionStartTicks := 0;
end;

procedure TListItemGlyphButton.ResetCheckedTransition;
begin
  TransitionEnabled := False;

  if Assigned(TransitionTimer) then
  begin
    TransitionTimer.Enabled := False;
    TransitionTimer.Parent := nil;
    FreeAndNil(TransitionTimer);
  end;

  if Assigned(TimerService) then
    TimerService := nil;

  if FChecked then
    TransitionAlpha := 1
  else
    TransitionAlpha := 0;
end;

procedure TListItemGlyphButton.TransitionTimerNotify(Sender: TObject);
const
  CheckedAnimationIncrement = 1 / (60 * CheckedAnimationDuration);
begin
  if FChecked then
  begin
    if Assigned(TimerService) then
      TransitionAlpha := Min(Abs(TimerService.GetTick - TransitionStartTicks) / CheckedAnimationDuration, 1)
    else
      TransitionAlpha := TransitionAlpha + CheckedAnimationIncrement;

    if TransitionAlpha >= 1 then
      ResetCheckedTransition;
  end
  else
  begin
    if Assigned(TimerService) then
      TransitionAlpha := Max(1 - (Abs(TimerService.GetTick - TransitionStartTicks) / CheckedAnimationDuration), 0)
    else
      TransitionAlpha := TransitionAlpha - CheckedAnimationIncrement;

    if TransitionAlpha <= 0 then
      ResetCheckedTransition;
  end;

  Invalidate;
end;

procedure TListItemGlyphButton.DoClick;
var
  Controller: IListViewController;
begin
  inherited;

  if FButtonType in [TGlyphButtonType.Delete, TGlyphButtonType.Checkbox] then
  begin
    SetChecked(not FChecked);

    if (FButtonType in [TGlyphButtonType.Delete]) and Supports(Owner.Parent, IListViewController, Controller) then
      Controller.FeedbackItemClickChecked(Owner, FChecked);
  end;
end;

procedure TListItemGlyphButton.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then
  begin
    InitCheckedTransition;
    FChecked := Value;

    if not TransitionEnabled then
      ResetCheckedTransition;

    DoChange;
    Invalidate;
  end;
end;

procedure TListItemGlyphButton.DoSelect;
begin
  inherited;

  if FClickOnSelect then
    DoClick;
end;

function TListItemGlyphButton.MouseDown(const Button: TMouseButton; const Shift: TShiftState;
  const MousePos: TPointF): Boolean;
begin
  if not FClickOnSelect then
    Result := inherited MouseDown(Button, Shift, MousePos)
  else
    Result := False;
end;

procedure TListItemGlyphButton.CalculateLocalRect(const DestRect: TRectF; const SceneScale: Single;
  const DrawStates: TListItemDrawStates);
var
  NewRect: TRectF;
  Controller: IListViewController;
begin
  if TListItemDrawState.EditMode in DrawStates then
  begin
    NewRect := DestRect;

    if Supports(Owner.Parent, IListViewController, Controller) then
      NewRect.Offset(Controller.GetItemEditOffset(Owner) * (Controller.EditModeTransitionAlpha - 1), 0);

    inherited CalculateLocalRect(NewRect, SceneScale, DrawStates);
  end
  else
    inherited;
end;

procedure TListItemGlyphButton.Render(const Canvas: TCanvas; const DrawItemIndex: Integer;
  const DrawStates: TListItemDrawStates; const SubPassNo: Integer);
var
  ReallyPressed: Boolean;
  Resources: TListItemStyleResources;
  TempOpacity: Single;
  PrevMatrix, M: TMatrix;
  LocRect: TRectF;
  CenterShift: TPointF;
  Controller: IListViewController;
begin
  if SubPassNo <> 0 then
    Exit;

  ReallyPressed := FPressed and FMouseOver;

  LocRect := LocalRect;
  TempOpacity := FOpacity;

  if not FEnabled then
    TempOpacity := TempOpacity * DisabledOpacity;

  if (TListItemDrawState.EditMode in DrawStates) and Supports(Owner.Parent, IListViewController, Controller) then
    TempOpacity := TempOpacity * Max(0.1, Controller.EditModeTransitionAlpha * 2 - 1)
  else
    Exit;

{$IFDEF DRAW_ITEM_MARGINS}
  MarginBrush.Color := $50804020;
  Canvas.FillRect(LocalRect, 0, 0, AllCorners, 1, MarginBrush);
{$ENDIF}

  Resources := GetStyleResources;
  if Assigned(Resources) then
  begin
    case FButtonType of
      TGlyphButtonType.Add:
        begin
          if (not ReallyPressed) and Assigned(Resources.ButtonAddItemStyleImage.Normal) then
            Resources.ButtonAddItemStyleImage.Normal.DrawToCanvas(Canvas, LocRect, TempOpacity);

          if ReallyPressed and Assigned(Resources.ButtonAddItemStyleImage.Pressed) then
            Resources.ButtonAddItemStyleImage.Pressed.DrawToCanvas(Canvas, LocRect, TempOpacity);
        end;

      TGlyphButtonType.Delete:
        begin
          LocRect.Left := LocRect.Left + MinusOffsetX;
          if Assigned(Resources.ButtonDeleteItemStyleImage.Normal) then
            Resources.ButtonDeleteItemStyleImage.Normal.DrawToCanvas(Canvas, LocRect, TempOpacity);

          if Assigned(Resources.ButtonDeleteItemStyleImage.Pressed) then
          begin
            if TransitionAlpha <= 0 then
              Resources.ButtonDeleteItemStyleImage.Pressed.DrawToCanvas(Canvas, LocRect, TempOpacity)
            else
            begin
              PrevMatrix := Canvas.Matrix;

              CenterShift.X := LocRect.Left + LocRect.Width * 0.5;
              CenterShift.Y := LocRect.Top + LocRect.Height * 0.5;

              M := TMatrix.CreateTranslation(-CenterShift.X, -CenterShift.Y)
                * TMatrix.CreateRotation(-TransitionAlpha * Pi * 0.5)
                * TMatrix.CreateTranslation(CenterShift.X, CenterShift.Y)
                * PrevMatrix;

              Canvas.SetMatrix(M);

              Resources.ButtonDeleteItemStyleImage.Pressed.DrawToCanvas(Canvas, LocRect, TempOpacity);

              Canvas.SetMatrix(PrevMatrix);
            end;
          end;
        end;

      TGlyphButtonType.Checkbox:
        begin
          if (not FChecked) and Assigned(Resources.ButtonCheckboxStyleImage.Normal) then
            Resources.ButtonCheckboxStyleImage.Normal.DrawToCanvas(Canvas, LocRect, TempOpacity);

          if FChecked and Assigned(Resources.ButtonCheckboxStyleImage.Pressed) then
            Resources.ButtonCheckboxStyleImage.Pressed.DrawToCanvas(Canvas, LocRect, TempOpacity);
        end;
    end;
  end;
end;

{$ENDREGION}
{$REGION 'List Item Text Button'}

constructor TListItemTextButton.Create(const AOwner: TListItem);
begin
  inherited;

  FTextShadowOffset := TPosition.Create(PointF(0, 1));
  FTextShadowOffset.OnChange := TextShadowOffsetChanged;

  FFont := TFont.Create;
  FFont.Family := 'Helvetica';
  FFont.Size := 12.0;

  FTextColor := claBlack;
  FPressedTextColor := claWhite;
  FTextShadowColor := 0;
  FTextAlign := TTextAlign.taCenter;
  FTextVertAlign := TTextAlign.taCenter;

  UpdateValuesFromStyle;

  FFont.OnChanged := FontChanged;

  FWordWrap := False;
  FTrimming := TTextTrimming.ttNone;
end;

destructor TListItemTextButton.Destroy;
begin
  if Assigned(FTextLayout) then
    FreeAndNil(FTextLayout);

  FFont.Free;
  FTextShadowOffset.Free;

  inherited;
end;

procedure TListItemTextButton.UpdateValuesFromStyle;
var
  Resources: TListItemStyleResources;
begin
  Resources := GetStyleResources;
  if Resources <> nil then
    case FButtonType of
      TTextButtonType.Normal:
        begin
          if Assigned(Resources.ButtonTextFont) then
            FFont.Assign(Resources.ButtonTextFont);

          if Resources.ButtonTextColor > 0 then
            FTextColor := Resources.ButtonTextColor;

          if Resources.ButtonTextPressedColor > 0 then
            FPressedTextColor := Resources.ButtonTextPressedColor;
        end;

      TTextButtonType.Delete:
        begin
          if Assigned(Resources.DeleteButtonTextFont) then
            FFont.Assign(Resources.DeleteButtonTextFont);

          if Resources.DeleteButtonTextColor > 0 then
            FTextColor := Resources.DeleteButtonTextColor;

          if Resources.DeleteButtonTextPressedColor > 0 then
            FPressedTextColor := Resources.DeleteButtonTextPressedColor;
        end;
    end;
end;

procedure TListItemTextButton.SetButtonType(const Value: TTextButtonType);
begin
  if FButtonType <> Value then
  begin
    FButtonType := Value;

    UpdateValuesFromStyle;
    Invalidate;
  end;
end;

procedure TListItemTextButton.SetPressedTextColor(const Value: TAlphaColor);
begin
  if FPressedTextColor <> Value then
  begin
    FPressedTextColor := Value;
    LayoutChanged := True;
    Invalidate;
  end;
end;

procedure TListItemTextButton.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    LayoutChanged := True;
    Invalidate;
  end;
end;

procedure TListItemTextButton.DoOpacityChange;
begin
  inherited;

  LayoutChanged := True;
end;

procedure TListItemTextButton.DoResize;
begin
  inherited;

  LayoutChanged := True;
end;

procedure TListItemTextButton.DoEnabledChange;
begin
  inherited;

  LayoutChanged := True;
end;

procedure TListItemTextButton.FontChanged(Sender: TObject);
begin
  LayoutChanged := True;
  Invalidate;
end;

procedure TListItemTextButton.SetTextShadowColor(const Value: TAlphaColor);
begin
  if FTextShadowColor <> Value then
  begin
    FTextShadowColor := Value;
    Invalidate;
  end;
end;

procedure TListItemTextButton.SetTextAlign(const Value: TTextAlign);
begin
  if FTextAlign <> Value then
  begin
    FTextAlign := Value;
    LayoutChanged := True;
    Invalidate;
  end;
end;

procedure TListItemTextButton.SetTextColor(const Value: TAlphaColor);
begin
  if FTextColor <> Value then
  begin
    FTextColor := Value;
    LayoutChanged := True;
    Invalidate;
  end;
end;

procedure TListItemTextButton.SetTextVertAlign(const Value: TTextAlign);
begin
  if FTextVertAlign <> Value then
  begin
    FTextVertAlign := Value;
    LayoutChanged := True;
    Invalidate;
  end;
end;

procedure TListItemTextButton.SetTrimming(const Value: TTextTrimming);
begin
  if FTrimming <> Value then
  begin
    FTrimming := Value;
    LayoutChanged := True;
    Invalidate;
  end;
end;

procedure TListItemTextButton.SetWordWrap(const Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    LayoutChanged := True;
    Invalidate;
  end;
end;

procedure TListItemTextButton.TextShadowOffsetChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TListItemTextButton.SetData(const AValue: TValue);
begin
  if AValue.IsEmpty then
    Text := ''
  else
    Text := AValue.ToString;
end;

function TListItemTextButton.GetRenderPassCount: Integer;
begin
  if FText.Length > 0 then
    Result := 2
  else
    Result := inherited;
end;

function TListItemTextButton.IsClickOpaque: Boolean;
var
  Controller: IListViewController;
begin
  Result := inherited;

  if Result and Supports(Owner.Parent, IListViewController, Controller) and
    (Controller.EditModeTransitionAlpha > AnimationDeltaEpsilon) then
    Result := False;
end;

procedure TListItemTextButton.Render(const Canvas: TCanvas; const DrawItemIndex: Integer;
  const DrawStates: TListItemDrawStates; const SubPassNo: Integer);
var
  ReallyPressed: Boolean;
  CurColor: TAlphaColor;
  Resources: TListItemStyleResources;
  TempOpacity: Single;
  Controller: IListViewController;
begin
  TempOpacity := FOpacity;

  if not FEnabled then
    TempOpacity := FOpacity * DisabledOpacity;

  if TListItemDrawState.Deleting in DrawStates then
    TempOpacity := TempOpacity * GetDeletingUnwantedOpacity(Owner.Parent);

  if TListItemDrawState.EditMode in DrawStates then
    if Supports(Owner.Parent, IListViewController, Controller) then
      TempOpacity := TempOpacity * (1 - Controller.EditModeTransitionAlpha);

  if TempOpacity < OpacityEpsilon then
    Exit;

  ReallyPressed := FPressed and FMouseOver;

  if SubPassNo = 0 then
  begin
{$IFDEF DRAW_ITEM_MARGINS}
    MarginBrush.Color := $50804020;
    Canvas.FillRect(LocalRect, 0, 0, AllCorners, 1, MarginBrush);
{$ENDIF}

    Resources := GetStyleResources;
    if Assigned(Resources) then
    begin
      case FButtonType of
        TTextButtonType.Normal:
          begin
            if (not ReallyPressed) and Assigned(Resources.ButtonNormalStyleImage.Normal) then
              Resources.ButtonNormalStyleImage.Normal.DrawToCanvas(Canvas, LocalRect, TempOpacity);

            if ReallyPressed and Assigned(Resources.ButtonNormalStyleImage.Pressed) then
              Resources.ButtonNormalStyleImage.Pressed.DrawToCanvas(Canvas, LocalRect, TempOpacity);
          end;

        TTextButtonType.Delete:
          begin
            if (not ReallyPressed) and Assigned(Resources.ButtonDeleteStyleImage.Normal) then
              Resources.ButtonDeleteStyleImage.Normal.DrawToCanvas(Canvas, LocalRect, TempOpacity);

            if ReallyPressed and Assigned(Resources.ButtonDeleteStyleImage.Pressed) then
              Resources.ButtonDeleteStyleImage.Pressed.DrawToCanvas(Canvas, LocalRect, TempOpacity);
          end;
      end;
    end;
  end;

  if SubPassNo = 1 then
  begin
    if not Assigned(FTextLayout) then
    begin
      FTextLayout := TTextLayoutManager.TextLayoutByCanvas(Canvas.ClassType).Create(Canvas);
      LayoutChanged := True
    end;

    if not LayoutChanged then
      LayoutChanged := (FTextLayout.MaxSize.X <> LocalRect.Width) or (FTextLayout.MaxSize.Y <> LocalRect.Height);

    CurColor := FTextColor;
    if ReallyPressed then
      CurColor := FPressedTextColor;

    if (not LayoutChanged) and (FTextLayout.Color <> CurColor) then
      LayoutChanged := True;

    if (not LayoutChanged) and (not SameValue(FTextLayout.Opacity, TempOpacity, OpacityEpsilon)) then
      LayoutChanged := True;

    if LayoutChanged then
    begin
      FTextLayout.BeginUpdate;
      FTextLayout.Opacity := TempOpacity;
      FTextLayout.HorizontalAlign := FTextAlign;
      FTextLayout.VerticalAlign := FTextVertAlign;
      FTextLayout.Font := FFont;
      FTextLayout.Color := CurColor;
      FTextLayout.RightToLeft := False;
      FTextLayout.MaxSize := PointF(LocalRect.Width, LocalRect.Height);
      FTextLayout.Text := FText;
      FTextLayout.Trimming := FTrimming;
      FTextLayout.WordWrap := FWordWrap;
      FTextLayout.EndUpdate;

      LayoutChanged := False;
    end;

    if TAlphaColorRec(FTextShadowColor).A > 0 then
    begin
      FTextLayout.BeginUpdate;
      FTextLayout.Color := FTextShadowColor;
      FTextLayout.TopLeft := LocalRect.TopLeft + FTextShadowOffset.Point;
      FTextLayout.EndUpdate;
      FTextLayout.RenderLayout(Canvas);

      FTextLayout.BeginUpdate;
      FTextLayout.Color := CurColor;
      FTextLayout.EndUpdate;
    end;

    FTextLayout.BeginUpdate;
    FTextLayout.TopLeft := LocalRect.TopLeft;
    FTextLayout.EndUpdate;
    FTextLayout.RenderLayout(Canvas);
  end;
end;

{$ENDREGION}
{$REGION 'List Item Control Scene'}

constructor TListItemControlScene.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TListItemControlScene.Destroy;
begin

  inherited;
end;

procedure TListItemControlScene.SetStyleBook(const Value: TStyleBook);
begin
end;

procedure TListItemControlScene.DoAddObject(const AObject: TFmxObject);
begin
  inherited;
  if AObject is TControl then
    TControl(AObject).SetNewScene(Self);
  if AObject is TStyledControl then
    TOpenStyledControl(AObject).DisableDisappear := True;
  AObject.SetRoot(ItemOwner.Owner.Parent.Root);
end;

procedure TListItemControlScene.DoRemoveObject(const AObject: TFmxObject);
begin
  inherited;
  AObject.SetRoot(nil);
  if AObject is TControl then
    TControl(AObject).SetNewScene(nil);
end;

function TListItemControlScene.GetStyleBook: TStyleBook;
begin
  Result := nil;
end;

function TListItemControlScene.GetCanvas: TCanvas;
begin
  Result := FCanvas
end;

function TListItemControlScene.GetSceneScale: Single;
begin
  if Assigned(ItemOwner) and Assigned(ItemOwner.Owner) and Assigned(ItemOwner.Owner.Parent) then
    Result := ItemOwner.Owner.Parent.Scene.GetSceneScale
  else
    Result := 1.0;
end;

function TListItemControlScene.GetObject: TFmxObject;
begin
  Result := Self;
end;

procedure TListItemControlScene.ChangeScrollingState(const AControl: TControl; const Active: Boolean);
begin
end;

function TListItemControlScene.LocalToScreen(P: TPointF): TPointF;
begin
  if Assigned(ItemOwner) and Assigned(ItemOwner.Owner) and Assigned(ItemOwner.Owner.Parent) then
    Result := ItemOwner.Owner.Parent.Scene.LocalToScreen(P)
  else
    Result := P;
end;

function TListItemControlScene.ScreenToLocal(P: TPointF): TPointF;
begin
  if Assigned(ItemOwner) and Assigned(ItemOwner.Owner) and Assigned(ItemOwner.Owner.Parent) then
    Result := ItemOwner.Owner.Parent.Scene.ScreenToLocal(P)
  else
    Result := P;
end;

function TListItemControlScene.GetUpdateRectsCount: Integer;
begin
  Result := 1;
end;

function TListItemControlScene.GetUpdateRect(const Index: Integer): TRectF;
begin
  Result := TRectF.Create(0, 0, FLayoutSize.X, FLayoutSize.Y);
end;

procedure TListItemControlScene.AddUpdateRect(R: TRectF);
begin
  ItemOwner.Invalidate;
end;

procedure TListItemControlScene.RepaintScene(const Canvas: TCanvas);
var
  Index: Integer;
  {$IFDEF CPUARM}[unsafe]{$ENDIF}Control: TOpenControl;
begin
  if (not FDrawing) then
  begin
    FDrawing := True;
    FCanvas := Canvas;
    try
      for Index := 0 to ChildrenCount - 1 do
        if (Children[Index] is TControl) and
          (TControl(Children[Index]).Visible or ((not TControl(Children[Index]).Visible) and
          (csDesigning in ComponentState) and (not TControl(Children[Index]).Locked))) then
        begin
          Control := TOpenControl(Children[Index]);
          TOpenControl(Control).PaintInternal;
        end;
    finally
      FCanvas := nil;
      FDrawing := False;
    end;
  end;
end;

{$ENDREGION}
{$REGION 'List Item Control Container'}

constructor TListItemControlContainer.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TListItemControlContainer.Destroy;
begin
  inherited;
end;

{$ENDREGION}
{$REGION 'List Item Embedded Control'}

constructor TListItemEmbeddedControl.Create(const AOwner: TListItem);
begin
  inherited;

  FScene := TListItemControlScene.Create(nil);
  FScene.FItemOwner := Self;

  FContainer := TListItemControlContainer.Create(nil);
  FContainer.Parent := FScene;
  FContainer.FItemOwner := Self;

  FScene.FContainer := FContainer;
end;

destructor TListItemEmbeddedControl.Destroy;
begin
  FContainer.Free;
  FScene.Free;

  inherited;
end;

procedure TListItemEmbeddedControl.Render(const Canvas: TCanvas; const DrawItemIndex: Integer;
  const DrawStates: TListItemDrawStates; const SubPassNo: Integer);
begin
  if SubPassNo <> 0 then
    Exit;

  FScene.FLayoutSize := TPoint.Create(Trunc(Canvas.Width), Trunc(Canvas.Height));

  FContainer.SetBounds(LocalRect.Left, LocalRect.Top, LocalRect.Width, LocalRect.Height);

  FScene.RepaintScene(Canvas);
end;

function TListItemEmbeddedControl.ObjectAtPoint(const Point: TPointF): TControl;
var
  Control: IControl;
begin
  Control := FContainer.ObjectAtPoint(Point);
  if Assigned(Control) and (Control.GetObject is TControl) and (Control.GetObject <> FContainer) then
    Result := TControl(Control.GetObject)
  else
    Result := nil;
end;

{$ENDREGION}
{$REGION 'List Item'}

constructor TListItem.Create;
begin
  inherited;
  FObjects := ListItemObjectsClass.Create(Self);
  FItemList := TListItemObjects.TItemList.Create;
  FHeaderRef := -1;
  FUpdating := 0;
  NeedRepaint := False;
end;

procedure TListItem.CreateObjects;
begin
//
end;

destructor TListItem.Destroy;
begin
  FItemList.Free;
  FObjects.Free;

  inherited;
end;

function TListItem.GetCount: Integer;
begin
  Result := FObjects.Count;
end;

procedure TListItem.BeginUpdate;
begin
  Inc(FUpdating);
end;

procedure TListItem.EndUpdate;
begin
  if FUpdating > 0 then
  begin
    Dec(FUpdating);
    if (FUpdating <= 0) and NeedRepaint then
    begin
      NeedRepaint := False;
      Invalidate;
    end;
  end;
end;

procedure TListItem.Invalidate;
begin
  if FUpdating < 1 then
  begin
    Repaint;
  end
  else
    NeedRepaint := True;
end;

procedure TListItem.SetHeight(const Value: Integer);
var
  NewValue: Integer;
begin
  NewValue := Value;
  if NewValue < 0 then
    NewValue := 0;

  if NewValue <> FHeight then
  begin
    FHeight := NewValue;

    if Assigned(FParent) then
      InvalidateHeights;
  end;
end;

procedure TListItem.SetPurpose(const AValue: TListItemPurpose);
begin
  if AValue <> FPurpose then
  begin
    FPurpose := AValue;
    InvalidateHeights;
  end;
end;

procedure TListItem.InvalidateHeights;
begin
  //
end;

function TListItem.ListItemObjectsClass: TListItemObjectsType;
begin
  Result := TListItemObjects;
end;

function TListItem.ObjectAtPoint(const Point: TPointF): TControl;
var
  I: Integer;
  Control: TControl;
begin
  Result := nil;
  for I := 0 to FItemList.Count - 1 do
    if Assigned(FItemList[I]) then
    begin
      Control := FItemList[I].ObjectAtPoint(Point);
      if Assigned(Control) then
        Exit(Control);
    end;
end;

procedure TListItem.Repaint;
begin
  if FParent <> nil then
    FParent.Repaint;
end;

function TListItem.MouseDown(const Button: TMouseButton; const Shift: TShiftState; const MousePos: TPointF): Boolean;
var
  I: Integer;
begin
  Result := False;

  for I := 0 to FItemList.Count - 1 do
    if Assigned(FItemList[I]) and FItemList[I].Visible then
    begin
      Result := FItemList[I].MouseDown(Button, Shift, MousePos);
      if Result then
        Break;
    end;
end;

procedure TListItem.MouseMove(const Shift: TShiftState; const MousePos: TPointF);
var
  I: Integer;
begin
  for I := 0 to FItemList.Count - 1 do
    if Assigned(FItemList[I]) and FItemList[I].Visible then
      FItemList[I].MouseMove(Shift, MousePos);
end;

procedure TListItem.MouseUp(const Button: TMouseButton; const Shift: TShiftState; const MousePos: TPointF);
var
  I: Integer;
begin
  for I := 0 to FItemList.Count - 1 do
    if Assigned(FItemList[I]) and FItemList[I].Visible then
      FItemList[I].MouseUp(Button, Shift, MousePos);
end;

procedure TListItem.MouseSelect;
var
  I: Integer;
begin
  for I := 0 to FItemList.Count - 1 do
    if Assigned(FItemList[I]) then
      FItemList[I].DoSelect;
end;

function TListItem.HasClickOnSelectItems: Boolean;
var
  I: Integer;
begin
  Result := False;

  for I := 0 to FItemList.Count - 1 do
    if Assigned(FItemList[I]) and FItemList[I].Visible and (FItemList[I] is TListItemGlyphButton) then
    begin
      Result := TListItemGlyphButton(FItemList[I]).ClickOnSelect;
      if Result then
        Break;
    end;
end;

{$ENDREGION}
{$REGION 'List Item Objects'}

constructor TListItemObjects.Create(const AOwner: TListItem);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TListItemObjects.GetCount: Integer;
begin
  FOwner.CreateObjects;
  Result := FOwner.FItemList.Count;
end;

function TListItemObjects.GetItemList: TItemList;
begin
  Result := FOwner.FItemList;
end;

function TListItemObjects.GetObject(Index: Integer): TListItemObject;
begin
  Result := ItemList[Index];
end;

function TListItemObjects.Add(const AItem: TListItemObject): Integer;
begin
  Result := ItemList.Add(AItem);
end;

procedure TListItemObjects.Clear;
begin
  ItemList.Clear;
end;

procedure TListItemObjects.Delete(Index: Integer);
begin
  ItemList.Delete(Index);
end;

destructor TListItemObjects.Destroy;
begin

  inherited;
end;

procedure TListItemObjects.Include(const AItem: TListItemObject);
begin
  if (ItemList.IndexOf(AItem) = -1) then
    ItemList.Add(AItem);
end;

procedure TListItemObjects.Exclude(const AItem: TListItemObject);
var
  Index: Integer;
begin
  Index := ItemList.IndexOf(AItem);

  if (Index <> -1) then
    ItemList.Delete(Index);
end;


function TListItemObjects.ObjectByName(const AName: string): TListItemObject;
begin
  Result := FindObject(AName);
  if Result = nil then
    raise EListError.Create(SGenericItemNotFound);

end;

function TListItemObjects.FindObject(const AName: string): TListItemObject;
var
  LObject: TListItemObject;
begin
  for LObject in ItemList do
    if LObject.Name = AName then
      Exit(LObject);

  Result := nil;
end;

{$ENDREGION}
{$REGION 'List Item Style Objects'}

{ TListItemStyleObjects }

constructor TListItemStyleResources.Create;
begin
  DefaultTextFont := TFont.Create;
  DetailTextFont := TFont.Create;
  HeaderTextFont := TFont.Create;
  ButtonTextFont := TFont.Create;
  DeleteButtonTextFont := TFont.Create;

end;

destructor TListItemStyleResources.Destroy;
begin
  inherited;
  DefaultTextFont.Free;
  DetailTextFont.Free;
  HeaderTextFont.Free;
  ButtonTextFont.Free;
  DeleteButtonTextFont.Free;

end;
{$ENDREGION}

end.
