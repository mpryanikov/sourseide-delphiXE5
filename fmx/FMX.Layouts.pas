{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit FMX.Layouts;

interface

uses
  System.Classes, System.Types, System.Generics.Collections, System.UITypes, System.SysUtils,
  FMX.Types, FMX.Ani, FMX.StdCtrls, FMX.Platform, FMX.Controls, FMX.Graphics,
  FMX.InertialMovement;

type

{ TLayout }

  TLayout = class(TControl)
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    property DesignVisible default True;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property Height;
    property HitTest default False;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property TouchTargetExpansion;
    property Visible default True;
    property Width;
    { Events }
    property OnPainting;
    property OnPaint;
    property OnResize;
    { Drag and Drop events }
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    { Mouse events }
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
  end;

{ TScaledLayout }

  TScaledLayout = class(TControl)
  private
    FOriginalWidth: Single;
    FOriginalHeight: Single;
    procedure SetOriginalWidth(const Value: Single);
    procedure SetOriginalHeight(const Value: Single);
  protected
    procedure DoRealign; override;
    function GetChildrenMatrix(var Matrix: TMatrix; var Simple: Boolean): Boolean; override;
    procedure SetHeight(const Value: Single); override;
    procedure SetWidth(const Value: Single); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    property DesignVisible default True;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property Height;
    property HitTest default False;
    property Padding;
    property Opacity;
    property OriginalWidth: Single read FOriginalWidth write SetOriginalWidth;
    property OriginalHeight: Single read FOriginalHeight write SetOriginalHeight;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property TouchTargetExpansion;
    property Visible default True;
    property Width;
    { Events }
    property OnPainting;
    property OnPaint;
    property OnResize;
    { Drag and Drop events }
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    { Mouse events }
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
  end;

  TScrollBox = class;

{ TScrollContent }

  TScrollContent = class(TContent)
  private
    FScrollBox: TScrollBox;
  protected
    function GetClipRect: TRectF; override;
    function ObjectAtPoint(P: TPointF): IControl; override;
    function GetUpdateRect: TRectF; override;
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoInsertObject(Index: Integer; const AObject: TFmxObject); override;
    procedure DoRemoveObject(const AObject: TFmxObject); override;
    procedure DoRealign; override;
  public
    constructor Create(AOwner: TComponent); override;
    property ScrollBox: TScrollBox read FScrollBox;
  end;

  TScrollCalculations = class (TAniCalculations)
  private
    FScrollBox: TScrollBox;
  protected
    procedure DoChanged; override;
    procedure DoStart; override;
    procedure DoStop; override;
  public
    constructor Create(AOwner: TPersistent); override;
    property ScrollBox: TScrollBox read FScrollBox;
  end;

{ TScrollBox }

  TPositionChangeEvent = procedure (Sender: TObject;
                              const OldViewportPosition, NewViewportPosition: TPointF;
                              const ContentSizeChanged: Boolean) of object;

  TOnCalcContentBoundsEvent = procedure (Sender: TObject;
                                     var ContentBounds: TRectF) of object;

  TScrollBox = class(TStyledControl)
  private
  type
    TPanDirection = (pdUndecided, pdHorizontal, pdVertical,  pdArbitrary);
    TScrollInfo = record
      Scroll: TScrollBar;
      Align: TAlignLayout;
      Margins: TRectF;
    end;
  var
    FSystemInfoSrv: IFMXSystemInformationService;
    FDisableMouseWheel: Boolean;

    FAniCalculations: TScrollCalculations;
    FLastViewportPosition: TPointF;
    FInInternalAlign: Boolean;

    [weak]FBackground: TControl;
    FContent: TScrollContent;
    [weak]FContentLayout: TControl;
    FContentBounds: TRectF;
    FCachedContentSize: TSizeF;

    FShowScrollBars: Boolean;
    FAutoHide: Boolean;
    FHScrollInfo: array of TScrollInfo;
    FVScrollInfo: array of TScrollInfo;
    FContentMargins: TRectF;
    FVDisablePaint: Boolean;
    FHDisablePaint: Boolean;
    FGDisablePaint: Boolean;

    [weak]FSizeGripContent: TControl;
    [weak]FSizeGripParent: TControl;
    [weak]FSizeGrip: TControl;
    FShowSizeGrip: Boolean;
    FOnViewportPositionChange: TPositionChangeEvent;
    FOnHScrollChange: TNotifyEvent;
    FOnVScrollChange: TNotifyEvent;
    FOnCalcContentBounds: TOnCalcContentBoundsEvent;
    FMouseEvents: Boolean;
    function HScrollIndex: Integer;
    function VScrollIndex: Integer;
    function GetHScrollAlign: TAlignLayout;
    function GetVScrollAlign: TAlignLayout;
    function GetHScrollMargins: TRectF;
    function GetVScrollMargins: TRectF;
    function GetSceneScale: Single;
    procedure SetShowScrollBars(const Value: Boolean);
    procedure SetShowSizeGrip(const Value: Boolean);
    function GetVScrollBar: TScrollBar;
    function GetHScrollBar: TScrollBar;
    procedure InternalAlign;
    procedure HScrollChangeProc(Sender: TObject);
    procedure VScrollChangeProc(Sender: TObject);
    procedure MousePosToAni(var X, Y: Single);
    procedure SetAutoHide(const Value: Boolean);
    procedure SaveDisablePaint;
    procedure RestoreDisablePaint;
    procedure SetDisablePaint;
    function GetViewportPosition: TPointF;
    procedure SetViewportPosition(const Value: TPointF);
    procedure StartScrolling;
    procedure StopScrolling;
  protected
    //Animation mouse events
    procedure AniMouseDown(const Touch: Boolean; const X, Y: Single); virtual;
    procedure AniMouseMove(const Touch: Boolean; const X, Y: Single); virtual;
    procedure AniMouseUp(const Touch: Boolean; const X, Y: Single); virtual;

    function GetScrollingBehaviours: TScrollingBehaviours;
    procedure Loaded; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoRealign; override;
    function IsAddToContent(const AObject: TFmxObject): Boolean; virtual;
    procedure ContentAddObject(const AObject: TFmxObject); virtual;
    procedure ContentInsertObject(Index: Integer; const AObject: TFmxObject); virtual;
    procedure ContentBeforeRemoveObject(AObject: TFmxObject); virtual;
    procedure ContentRemoveObject(const AObject: TFmxObject); virtual;
    procedure HScrollChange; virtual;
    procedure VScrollChange; virtual;
    procedure ViewportPositionChange(const OldViewportPosition, NewViewportPosition: TPointF;
                                     const ContentSizeChanged: boolean); virtual;
    procedure Painting; override;
    procedure AfterPaint; override;
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
    function IsOpaque: Boolean; virtual;
    function ContentRect: TRectF;
    function VScrollBarValue: Single;
    function HScrollBarValue: Single;
    function CreateScrollContent: TScrollContent; virtual;
    function CreateAniCalculations: TScrollCalculations; virtual;
    procedure DoUpdateAniCalculations(const AAniCalculations: TScrollCalculations); virtual;
    procedure UpdateAniCalculations;
    function DoCalcContentBounds: TRectF; virtual;
    procedure DoRealignContent(R: TRectF); virtual;
    function GetContentBounds: TRectF;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseLeave; override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    property ContentLayout: TControl read FContentLayout;
    property Content: TScrollContent read FContent;
    procedure AddToTabList(const AObject: TFmxObject); override;
    property HScrollAlign: TAlignLayout read GetHScrollAlign;
    property VScrollAlign: TAlignLayout read GetVScrollAlign;
    property HScrollMargins: TRectF read GetHScrollMargins;
    property VScrollMargins: TRectF read GetVScrollMargins;
    property InInternalAlign: Boolean read FInInternalAlign;
    property HScrollBar: TScrollBar read GetHScrollBar;
    property VScrollBar: TScrollBar read GetVScrollBar;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CMGesture(var EventInfo: TGestureEventInfo); override;
    property AniCalculations: TScrollCalculations read FAniCalculations;
    property ViewportPosition: TPointF read GetViewportPosition write SetViewportPosition;
    procedure Sort(Compare: TFmxObjectSortCompare); override;
    procedure Center;
    procedure ScrollTo(const Dx, Dy: Single);
    procedure InViewRect(const Rect: TRectF);
    function ClientWidth: Single;
    function ClientHeight: Single;
    procedure GetTabOrderList(const List: TInterfaceList; AChildren: Boolean); override;
    property ContentBounds: TRectF read GetContentBounds;
    procedure InvalidateContentSize;
    procedure RealignContent;
    property OnViewportPositionChange: TPositionChangeEvent read FOnViewportPositionChange write FOnViewportPositionChange;
    property OnHScrollChange: TNotifyEvent read FOnHScrollChange write FOnHScrollChange;
    property OnVScrollChange: TNotifyEvent read FOnVScrollChange write FOnVScrollChange;
    property OnCalcContentBounds: TOnCalcContentBoundsEvent read FOnCalcContentBounds write FOnCalcContentBounds;
  published
    property Align;
    property Anchors;
    property AutoHide: Boolean read FAutoHide write SetAutoHide default True;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    property DesignVisible default True;
    property DragMode default TDragMode.dmManual;
    property DisableMouseWheel: Boolean read FDisableMouseWheel write FDisableMouseWheel default False;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property Height;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property HitTest default True;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property ShowScrollBars: Boolean read FShowScrollBars write SetShowScrollBars default True;
    property ShowSizeGrip: Boolean read FShowSizeGrip write SetShowSizeGrip default False;
    property StyleLookup;
    property TabOrder;
    property TouchTargetExpansion;
    property Visible default True;
    property Width;
    { Events }
    property OnApplyStyleLookup;
    property OnPainting;
    property OnPaint;
    property OnResize;
    { Drag and Drop events }
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    { Mouse events }
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
  end;

{ TVertScrollBox }

  TVertScrollBox = class(TScrollBox)
  private
  protected
    function GetDefaultStyleLookupName: string; override;
    function DoCalcContentBounds: TRectF; override;
    procedure DoUpdateAniCalculations(const AAniCalculations: TScrollCalculations); override;
  end;

  THorzScrollBox = class(TScrollBox)
  protected
    function GetDefaultStyleLookupName: string; override;
    function DoCalcContentBounds: TRectF; override;
    procedure DoUpdateAniCalculations(const AAniCalculations: TScrollCalculations); override;
  end;


{ TFramedScrollBox }

  TFramedScrollBox = class(TScrollBox)
  protected
    function IsOpaque: Boolean; override;
  public
  end;

{ TFramedVertScrollBox }

  TFramedVertScrollBox = class(TVertScrollBox)
  protected
    function IsOpaque: Boolean; override;
    function GetDefaultStyleLookupName: string; override;
  end;

{ TGridLayout }

  TGridLayout = class(TControl)
  private
    FItemWidth: Single;
    FItemHeight: Single;
    FOrientation: TOrientation;
    procedure SetItemHeight(const Value: Single);
    procedure SetItemWidth(const Value: Single);
    procedure SetOrientation(const Value: TOrientation);
  protected
    procedure DoRealign; override;
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoRemoveObject(const AObject: TFmxObject); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Anchors;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    property DesignVisible default True;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property Height;
    property HitTest default True;
    property ItemHeight: Single read FItemHeight write SetItemHeight;
    property ItemWidth: Single read FItemWidth write SetItemWidth;
    property Padding;
    property Opacity;
    property Orientation: TOrientation read FOrientation write SetOrientation;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property TouchTargetExpansion;
    property Visible default True;
    property Width;
    {Drag and Drop events}
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnCanFocus;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPainting;
    property OnPaint;
    property OnResize;
  end;

{ TGridPanelLayout }

  TGridPanelLayout = class(TControl)
  public type
    TSizeStyle = (ssAbsolute, ssPercent, ssAuto);

    EGridLayoutException = class(Exception);

    TCellItem = class(TCollectionItem)
    private
      FSizeStyle: TSizeStyle;
      FValue: Double;
      FSize: Single;
      FAutoAdded: Boolean;
    protected
      procedure AssignTo(Dest: TPersistent); override;
      procedure SetSizeStyle(Value: TSizeStyle);
      procedure SetValue(Value: Double);
      property Size: Single read FSize write FSize;
      property AutoAdded: Boolean read FAutoAdded write FAutoAdded;
    public
      constructor Create(Collection: TCollection); override;
    published
      property SizeStyle: TSizeStyle read FSizeStyle write SetSizeStyle default ssPercent;
      property Value: Double read FValue write SetValue;
    end;

    TRowItem = class(TCellItem);

    TColumnItem = class(TCellItem);

    TCellCollection = class(TOwnedCollection)
    protected
      function GetAttrCount: Integer; override;
      function GetAttr(Index: Integer): string; override;
      function GetItemAttr(Index, ItemIndex: Integer): string; override;
      function GetItem(Index: Integer): TCellItem;
      procedure SetItem(Index: Integer; Value: TCellItem);
      procedure Update(Item: TCollectionItem); override;
    public
      function Owner: TGridPanelLayout;
      property Items[Index: Integer]: TCellItem read GetItem write SetItem; default;
    end;

    TCellSpan = 1..MaxInt;

    TRowCollection = class(TCellCollection)
    protected
      function GetItemAttr(Index, ItemIndex: Integer): string; override;
      procedure Notify(Item: TCollectionItem; Action: System.Classes.TCollectionNotification); override;
    public
      constructor Create(AOwner: TPersistent);
      function Add: TRowItem;
    end;

    TColumnCollection = class(TCellCollection)
    protected
      function GetItemAttr(Index, ItemIndex: Integer): string; override;
      procedure Notify(Item: TCollectionItem; Action: System.Classes.TCollectionNotification); override;
    public
      constructor Create(AOwner: TPersistent);
      function Add: TColumnItem;
    end;

    TControlItem = class(TCollectionItem)
    private
      FControl: TControl;
      FColumn, FRow: Integer;
      FColumnSpan, FRowSpan: TCellSpan;
      FPushed: Integer;
      function GetGridPanelLayout: TGridPanelLayout;
      function GetPushed: Boolean;
      procedure SetColumn(Value: Integer);
      procedure SetColumnSpan(Value: TCellSpan);
      procedure SetControl(Value: TControl);
      procedure SetRow(Value: Integer);
      procedure SetRowSpan(Value: TCellSpan);
    protected
      procedure AssignTo(Dest: TPersistent); override;
      procedure InternalSetLocation(AColumn, ARow: Integer; APushed: Boolean; MoveExisting: Boolean);
      property GridPanelLayout: TGridPanelLayout read GetGridPanelLayout;
      property Pushed: Boolean read GetPushed;
    public
      constructor Create(Collection: TCollection); override;
      destructor Destroy; override;
      procedure SetLocation(AColumn, ARow: Integer; APushed: Boolean = False);
    published
      property Column: Integer read FColumn write SetColumn;
      property ColumnSpan: TCellSpan read FColumnSpan write SetColumnSpan default 1;
      property Control: TControl read FControl write SetControl;
      property Row: Integer read FRow write SetRow;
      property RowSpan: TCellSpan read FRowSpan write SetRowSpan default 1;
    end;

    TControlCollection = class(TOwnedCollection)
    protected
      function GetControl(AColumn, ARow: Integer): TControl;
      function GetControlItem(AColumn, ARow: Integer): TControlItem;
      function GetItem(Index: Integer): TControlItem;
      procedure SetControl(AColumn, ARow: Integer; Value: TControl);
      procedure SetItem(Index: Integer; Value: TControlItem);
      procedure Update(Item: TCollectionItem); override;
    public
      constructor Create(AOwner: TPersistent);
      function Add: TControlItem;
      procedure AddControl(AControl: TControl; AColumn: Integer = -1; ARow: Integer = -1);
      procedure RemoveControl(AControl: TControl);
      function IndexOf(AControl: TControl): Integer;
      function Owner: TGridPanelLayout;
      property Controls[AColumn, ARow: Integer]: TControl read GetControl write SetControl;
      property ControlItems[AColumn, ARow: Integer] : TControlItem read GetControlItem;
      property Items[Index: Integer]: TControlItem read GetItem write SetItem; default;
    end;

    TExpandStyle = (emAddRows, emAddColumns, emFixedSize);

  private
    FRecalcCellSizes: Boolean;
    FExpandStyle: TExpandStyle;

    FRowCollection: TRowCollection;
    FColumnCollection: TColumnCollection;
    FControlCollection: TControlCollection;

    procedure SetColumnCollection(const Value: TColumnCollection);
    procedure SetControlCollection(const Value: TControlCollection);
    procedure SetRowCollection(const Value: TRowCollection);

    function GetCellCount: Integer;
    function GetCellSizes(AColumn, ARow: Integer): TPointF;
    function GetCellRect(AColumn, ARow: Integer): TRectF;
    function GetColumnSpanIndex(AColumn, ARow: Integer): Integer;
    function GetRowSpanIndex(AColumn, ARow: Integer): Integer;
    procedure RecalcCellDimensions(const Rect: TRectF);

  protected
    procedure DoRealign; override;
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoRemoveObject(const AObject: TFmxObject); override;
    function AutoAddColumn: TColumnItem;
    function AutoAddRow: TRowItem;
    procedure RemoveEmptyAutoAddColumns;
    procedure RemoveEmptyAutoAddRows;
    function CellToCellIndex(AColumn, ARow: Integer): Integer;
    procedure CellIndexToCell(AIndex: Integer; var AColumn, ARow: Integer);
    procedure DoPaint; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;

    function IsColumnEmpty(AColumn: Integer): Boolean;
    function IsRowEmpty(ARow: Integer): Boolean;
    procedure UpdateControlsColumn(AColumn: Integer);
    procedure UpdateControlsRow(ARow: Integer);
    property ColumnSpanIndex[AColumn, ARow: Integer]: Integer read GetColumnSpanIndex;
    property CellCount: Integer read GetCellCount;
    property CellSize[AColumn, ARow: Integer]: TPointF read GetCellSizes;
    property CellRect[AColumn, ARow: Integer]: TRectF read GetCellRect;
    property RowSpanIndex[AColumn, ARow: Integer]: Integer read GetRowSpanIndex;

  published
    property Align;
    property Anchors;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    property DesignVisible default True;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property Height;
    property HitTest default True;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property TouchTargetExpansion;
    property Visible default True;
    property Width;
    { Columns and rows }
    property ColumnCollection: TColumnCollection read FColumnCollection write SetColumnCollection;
    property ControlCollection: TControlCollection read FControlCollection write SetControlCollection;
    property ExpandStyle: TExpandStyle read FExpandStyle write FExpandStyle default emAddRows;
    property RowCollection: TRowCollection read FRowCollection write SetRowCollection;

    {Drag and Drop events}
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnCanFocus;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPainting;
    property OnPaint;
    property OnResize;
  end;

  TFlowJustify = (fjLeft, fjRight, fjCenter, fjJustify);
  TFlowDirection = (fdLeftToRight, fdRightToLeft);

  TFlowLayoutRules = record
    Justify : TFlowJustify;
    JustifyLast : TFlowJustify;
    Direction : TFlowDirection;
    HorizontalGap : Single;
    VerticalGap : Single;
  end;

  TFlowLayoutBreak = class(TControl)
  private
    FRules : TFlowLayoutRules;
    FChangesRules : Boolean;
  protected
    procedure SetChangesRules(AChangesRules : Boolean);
    procedure Paint; override;
  public
    constructor Create(AOwner : TComponent); override;
  published
    property ChangesRules : Boolean read FChangesRules write SetChangesRules;

    property Justify : TFlowJustify read FRules.Justify write FRules.Justify;
    property JustifyLastLine : TFlowJustify read FRules.JustifyLast write FRules.JustifyLast;
    property FlowDirection : TFlowDirection read FRules.Direction write FRules.Direction;
    property HorizontalGap : Single read FRules.HorizontalGap write FRules.HorizontalGap;
    property VerticalGap : Single read FRules.VerticalGap write FRules.VerticalGap;

    property Visible default False;
    property DesignVisible default False;
  end;

  TFlowLayout = class(TControl)
  private
    FRules : TFlowLayoutRules;
  protected
    procedure DoRealign; override;
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoRemoveObject(const AObject: TFmxObject); override;

    procedure SetJustify(AJustify : TFlowJustify);
    procedure SetJustifyLast(AJustify : TFlowJustify);
    procedure SetFlowDirection(ADirection : TFlowDirection);
    procedure SetHGap(AHGap : Single);
    procedure SetVGap(AVGap : Single);

  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Anchors;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    property DesignVisible default True;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property Height;
//    property Hint;
    property HitTest default True;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property TouchTargetExpansion;
    property Visible default True;
    property Width;
    {Drag and Drop events}
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    {Mouse events}
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPainting;
    property OnPaint;
    property OnResize;
    property Justify : TFlowJustify read FRules.Justify write SetJustify;
    property JustifyLastLine : TFlowJustify read FRules.JustifyLast write SetJustifyLast;
    property FlowDirection : TFlowDirection read FRules.Direction write SetFlowDirection;
    property HorizontalGap : Single read FRules.HorizontalGap write SetHGap;
    property VerticalGap : Single read FRules.VerticalGap write SetVGap;
  end;

implementation

uses System.Math, System.SysConst, System.RTLConsts, FMX.Styles, FMX.Consts, FMX.Effects;

type

TOpenFxmObject = class (TFMXObject)

end;

{ TLayout }

constructor TLayout.Create(AOwner: TComponent);
begin
  inherited;
  CanParentFocus := True;
  HitTest := False;
end;

destructor TLayout.Destroy;
begin
  inherited;
end;

procedure TLayout.Paint;
var
  R: TRectF;
begin
  if (csDesigning in ComponentState) and not Locked and not FInPaintTo then
  begin
    R := LocalRect;
    InflateRect(R, -0.5, -0.5);
    Canvas.DrawDashRect(R, 0, 0, AllCorners, AbsoluteOpacity, $A0909090);
  end;
end;

{ TScrollContent }

constructor TScrollContent.Create(AOwner: TComponent);
begin
  inherited;
  if AOwner is TScrollBox then
    FScrollBox := TScrollBox(AOwner);
  ClipChildren := True;
  SetAcceptsControls(False);
end;

function TScrollContent.GetClipRect: TRectF;
var
  LScrollBox: TScrollBox;
begin
  if (Owner is TScrollBox) then
  begin
    LScrollBox := TScrollBox(Owner);
    if Assigned(LScrollBox.ContentLayout) then
    begin
      Result := LScrollBox.ContentLayout.AbsoluteRect;
      Result.TopLeft := AbsoluteToLocal(Result.TopLeft);
      Result.BottomRight := AbsoluteToLocal(Result.BottomRight);
      Exit;
    end;
  end;
  Result := inherited GetClipRect;
end;

function TScrollContent.ObjectAtPoint(P: TPointF): IControl;
begin
  Result := inherited ObjectAtPoint(P);
  if Assigned(Result) then
  begin
    if Assigned(FScene) then
      P := FScene.ScreenToLocal(P);
    P := AbsoluteToLocal(P);
    if not ClipRect.Contains(P) then
      Result := nil;
  end;
end;

procedure TScrollContent.DoAddObject(const AObject: TFmxObject);
begin
  inherited;
  if Assigned(Owner) and (Owner is TScrollBox) then
    TScrollBox(Owner).ContentAddObject(AObject);
end;

procedure TScrollContent.DoInsertObject(Index: Integer; const AObject: TFmxObject);
begin
  inherited;
  if Assigned(Owner) and (Owner is TScrollBox) then
    TScrollBox(Owner).ContentInsertObject(Index, AObject);
end;

procedure TScrollContent.DoRealign;
begin
  inherited;
  FLastWidth := Width;
  FLastHeight := Height;
end;

procedure TScrollContent.DoRemoveObject(const AObject: TFmxObject);
begin
  if Assigned(Owner) and (Owner is TScrollBox) then
    TScrollBox(Owner).ContentBeforeRemoveObject(AObject);
  inherited;
  if Assigned(Owner) and (Owner is TScrollBox) then
    TScrollBox(Owner).ContentRemoveObject(AObject);
end;

function TScrollContent.GetUpdateRect: TRectF;
begin
  if FRecalcUpdateRect then
  begin
    if (ParentControl is TScrollBox) then
    begin
      if Assigned(TScrollBox(ParentControl).ContentLayout) then
        FUpdateRect := TScrollBox(ParentControl).ContentLayout.UpdateRect
      else
        FUpdateRect := TScrollBox(ParentControl).UpdateRect;
    end;
    FRecalcUpdateRect := False;
  end;
  Result := FUpdateRect;
end;

{ TScrollCalculations }

constructor TScrollCalculations.Create(AOwner: TPersistent);
begin
  if not (AOwner is TScrollBox) then
    raise EArgumentException.Create(sArgumentInvalid);
  inherited;
  FScrollBox := TScrollBox(AOwner);
end;

procedure TScrollCalculations.DoChanged;
begin
  if Assigned(FScrollBox) and (not (csDestroying in FScrollBox.ComponentState)) then
    FScrollBox.InternalAlign;
  inherited;
end;

procedure TScrollCalculations.DoStart;
begin
  inherited;
  if Assigned(FScrollBox) and (not (csDestroying in FScrollBox.ComponentState)) then
    FScrollBox.StartScrolling;
end;

procedure TScrollCalculations.DoStop;
begin
  inherited;
  if Assigned(FScrollBox) and (not (csDestroying in FScrollBox.ComponentState)) then
    FScrollBox.StopScrolling;
end;

{ TScrollBox }

type
  TOpenScrollBar = class(TScrollBar);
  TOpenControl = class (TControl);

constructor TScrollBox.Create(AOwner: TComponent);
begin
  inherited;
  SetLength(FHScrollInfo, 2);
  SetLength(FVScrollInfo, 2);
  SupportsPlatformService(IFMXSystemInformationService, IInterface(FSystemInfoSrv));
  AutoCapture := True;
  FAutoHide := True;
  FShowScrollBars := True;
  FContent := CreateScrollContent;
  FContent.Parent := Self;
  FContent.Stored := False;
  FContent.Locked := True;
  FContent.HitTest := False;
  UpdateAniCalculations;
  Touch.DefaultInteractiveGestures := Touch.DefaultInteractiveGestures + [TInteractiveGesture.igPan];
  Touch.InteractiveGestures := Touch.InteractiveGestures + [TInteractiveGesture.igPan];
end;

destructor TScrollBox.Destroy;
begin
  FreeAndNil(FContent);
  FreeAndNil(FAniCalculations);
  inherited;
end;

{$REGION 'animation'}
function TScrollBox.CreateAniCalculations: TScrollCalculations;
begin
  Result := TScrollCalculations.Create(self);
end;

procedure TScrollBox.DoUpdateAniCalculations(const AAniCalculations: TScrollCalculations);
begin
  AAniCalculations.Animation := TScrollingBehaviour.sbAnimation in GetScrollingBehaviours;
  if TScrollingBehaviour.sbTouchTracking in GetScrollingBehaviours then
    AAniCalculations.TouchTracking := [ttVertical, ttHorizontal]
  else
    AAniCalculations.TouchTracking := [];
  AAniCalculations.BoundsAnimation := TScrollingBehaviour.sbBoundsAnimation in GetScrollingBehaviours;
  AAniCalculations.AutoShowing := TScrollingBehaviour.sbAutoShowing in GetScrollingBehaviours;
end;

procedure TScrollBox.UpdateAniCalculations;
begin
  if not (csDestroying in ComponentState) then
  begin
    if not Assigned(FAniCalculations) then
      FAniCalculations := CreateAniCalculations;
    FAniCalculations.BeginUpdate;
    try
      DoUpdateAniCalculations(FAniCalculations);
    finally
      FAniCalculations.EndUpdate;
    end;
  end;
end;

function TScrollBox.GetSceneScale: Single;
begin
  Result := 0;
  if Assigned(Scene) then
    Result := Scene.GetSceneScale;
  if Result <= 0 then
    Result := 1;
end;

procedure TScrollBox.InternalAlign;
var Point, NewViewportPosition, LViewportPosition: TPointF;
    ContentLayoutSize: TSizeF;
    R: TRectF;
    I: integer;
    Adjust, SizeChanged: Boolean;
    LScale: Single;
    TmpContentBounds: TRectF;
  procedure UpdateVisibleScrollBars;
  begin
    if Assigned(FVScrollInfo[Integer(not AniCalculations.AutoShowing)].Scroll) then
      FVScrollInfo[Integer(not AniCalculations.AutoShowing)].Scroll.Visible := False;
    if Assigned(FHScrollInfo[Integer(not AniCalculations.AutoShowing)].Scroll) then
      FHScrollInfo[Integer(not AniCalculations.AutoShowing)].Scroll.Visible := False;
    if Assigned(VScrollBar) then
    begin
      VScrollBar.Opacity := AniCalculations.Opacity;
      VScrollBar.Enabled := (FContentBounds.Height > ContentLayoutSize.Height) or
                            (AniCalculations.AutoShowing);
      VScrollBar.Visible := FShowScrollBars and
                           ((FContentBounds.Height > ContentLayoutSize.Height) or
                            (not FAutoHide));
      VScrollBar.HitTest := not AniCalculations.AutoShowing;
      VScrollBar.Locked := VScrollBar.HitTest;
    end;
    if Assigned(HScrollBar) then
    begin
      HScrollBar.Opacity := AniCalculations.Opacity;
      HScrollBar.Enabled := (FContentBounds.Width > ContentLayoutSize.Width) or
                            (AniCalculations.AutoShowing);
      HScrollBar.Visible := FShowScrollBars and
                           ((FContentBounds.Width > ContentLayoutSize.Width) or
                            (not FAutoHide));
      HScrollBar.HitTest := not AniCalculations.AutoShowing;
      HScrollBar.Locked := HScrollBar.HitTest;
    end;
  end;
  procedure UpdateSizeGrip;
  var R, GripRect: TRectF;
      GripLeft, GripTop, SizeGripVisible, Both: Boolean;

  begin
    SizeGripVisible := ShowSizeGrip and
                       Assigned(FSizeGrip) and
                       Assigned(FSizeGripParent) and
                       Assigned(FSizeGripContent);
    Both := (Assigned(VScrollBar) and VScrollBar.Visible) and
            (Assigned(HScrollBar) and HScrollBar.Visible);
    if Both or
       (SizeGripVisible and
        ((Assigned(VScrollBar) and VScrollBar.Visible) or
         (Assigned(HScrollBar) and HScrollBar.Visible))) then
    begin
      {$REGION 'size'}
      GripRect := TRectF.Create(0, 0, 0, 0);
      if Assigned(HScrollBar) then
        GripRect.Height := HScrollBar.Height
      else
        GripRect.Height := VScrollBar.Width;
      if Assigned(VScrollBar) then
        GripRect.Width := VScrollBar.Width
      else
        GripRect.Width := HScrollBar.Height;
      {$ENDREGION}
      {$REGION 'vert align'}
      GripTop := Assigned(HScrollBar) and
                  (not (HScrollAlign in [TAlignLayout.alBottom, TAlignLayout.alMostBottom]));
      if Assigned(FSizeGrip) then
      begin
        if GripTop then
          FSizeGrip.Align := TAlignLayout.alTop
        else
          FSizeGrip.Align := TAlignLayout.alBottom;
        FSizeGrip.BoundsRect := GripRect;
      end;
      {$ENDREGION}
      {$REGION 'horz align'}
      GripLeft := Assigned(VScrollBar) and
                  (not (VScrollAlign in [TAlignLayout.alRight, TAlignLayout.alMostRight]));
      if Assigned(FSizeGripContent) then
        FSizeGripContent.Align := TAlignLayout.alContents;
      if Assigned(FSizeGripParent) then
      begin
        if GripLeft then
          FSizeGripParent.Align := TAlignLayout.alLeft
        else
          FSizeGripParent.Align := TAlignLayout.alRight;
        FSizeGripParent.Width := GripRect.Width;
      end;
      {$ENDREGION}
      if Assigned(HScrollBar) and HScrollBar.Visible then
      begin
        R := HScrollMargins;
        if GripLeft then
          R.Left := R.Left + GripRect.Width
        else
          R.Right := R.Right + GripRect.Width;
        HScrollBar.Margins.Rect := R;
        if Assigned(VScrollBar) then
          VScrollBar.Margins.Rect := VScrollMargins;
      end
      else if Assigned(VScrollBar) and VScrollBar.Visible then
      begin
        R := VScrollMargins;
        if GripTop then
          R.Top := R.Top + GripRect.Height
        else
          R.Bottom := R.Bottom + GripRect.Height;
        VScrollBar.Margins.Rect := R;
      end;
      if Assigned(FSizeGripParent) then
        FSizeGripParent.Visible := True;
      if Assigned(FSizeGripContent) then
        FSizeGripContent.Visible := True;
      if Assigned(FSizeGrip) then
      begin
        FSizeGrip.Opacity := AniCalculations.Opacity;
        FSizeGrip.Visible := SizeGripVisible;
        FSizeGrip.Enabled := (Align in [TAlignLayout.alClient, TAlignLayout.alContents]) and
                             (not GripTop) and (not GripLeft);
      end;
    end
    else
    begin
      if Assigned(FSizeGrip) then
        FSizeGrip.Visible := False;
      if Assigned(FSizeGripContent) then
        FSizeGrip.Visible := False;
      if Assigned(VScrollBar) and Assigned(VScrollBar.Margins) then
        VScrollBar.Margins.Rect := VScrollMargins;
      if Assigned(HScrollBar) and Assigned(HScrollBar.Margins) then
        HScrollBar.Margins.Rect := HScrollMargins;
    end;
  end;
  procedure UpdatePosition;
  var
    R: TRectF;
    procedure UpdateContentLayoutSize;
    begin
      ContentLayoutSize := TSizeF.Create(ContentLayout.Width, ContentLayout.Height);
    end;
  begin
    ContentLayout.Margins.Rect := FContentMargins;
    if (ContentLayout.Align = TAlignLayout.alContents) and
       (Assigned(FBackground)) then
    begin
      R := ContentLayout.Margins.Rect;
      R.Left := R.Left + FBackground.Padding.Left;
      R.Top := R.Top + FBackground.Padding.Top;
      R.Right := R.Right + FBackground.Padding.Right;
      R.Bottom := R.Bottom + FBackground.Padding.Bottom;
      ContentLayout.Margins.Rect := R;
    end;
    Point := ContentLayout.Position.Point;
    UpdateContentLayoutSize;
    UpdateVisibleScrollBars;
    if Assigned(AniCalculations) then
      LViewportPosition := ViewportPosition
    else
      LViewportPosition := TPointF.Create(0, 0);
    Point := Point - LViewportPosition;
    if (FDisableAlign) and Assigned(FBackground) then
      TOpenControl(FBackground).Realign;
    UpdateContentLayoutSize;
  end;
  procedure UpdateTargets;
  var
    I, J: Integer;
    LTargets: array of TAniCalculations.TTarget;
    NewTargets: array of TAniCalculations.TTarget;
  begin
    if Assigned(AniCalculations) then
    begin
      SetLength(LTargets, AniCalculations.TargetCount);
      AniCalculations.GetTargets(LTargets);
      SetLength(NewTargets, 2);
      NewTargets[0].TargetType := TAniCalculations.TTargetType.ttMin;
      NewTargets[0].Point.X := FContentBounds.Left;
      NewTargets[0].Point.Y := FContentBounds.Top;
      NewTargets[1].TargetType := TAniCalculations.TTargetType.ttMax;
      NewTargets[1].Point.X := Max(FContentBounds.Left, FContentBounds.Right - ContentLayoutSize.Width);
      NewTargets[1].Point.Y := Max(FContentBounds.Top, FContentBounds.Bottom - ContentLayoutSize.Height);
      for I := 0 to Length(LTargets) - 1 do
      if not (LTargets[I].TargetType in [TAniCalculations.TTargetType.ttMin, TAniCalculations.TTargetType.ttMax]) then
      begin
        J := Length(NewTargets);
        SetLength(NewTargets, J + 1);
        NewTargets[J].TargetType := LTargets[I].TargetType;
        NewTargets[J].Point := LTargets[I].Point;
      end;
      AniCalculations.SetTargets(NewTargets);
    end;
  end;
begin
  if (not FInInternalAlign) and
      Assigned(ContentLayout) and
      Assigned(Content) and
      Assigned(AniCalculations) then
  begin
    LScale := GetSceneScale;
    FInInternalAlign := True;
    try
      if (not AniCalculations.Down) and
         (AniCalculations.LowVelocity) then
        AniCalculations.Shown := False;
      Adjust := False;
      I := 0;
      repeat
        UpdatePosition;
        inc(I);
        SizeChanged := FCachedContentSize <> ContentLayoutSize;
        if SizeChanged then
        begin
          TmpContentBounds := DoCalcContentBounds;
          if Assigned(OnCalcContentBounds) then
            OnCalcContentBounds(self, TmpContentBounds);
          FContentBounds := TmpContentBounds;
          FCachedContentSize := ContentLayoutSize;
          Adjust := True;
        end;
      until (not SizeChanged) or (I > 5);
      if Adjust then
        UpdateTargets;
      R := TRectF.Create(Point, FContentBounds.Width, FContentBounds.Height);
      DoRealignContent(R);
      {$REGION 'update scroll bar values'}
      if Assigned(VScrollBar) then
      begin
        VScrollBar.ValueRange.BeginUpdate;
        try
          VScrollBar.ValueRange.Min := Min(LViewportPosition.Y, FContentBounds.Top);
          VScrollBar.ValueRange.Max := Max(LViewportPosition.Y + ContentLayoutSize.Height, FContentBounds.Bottom);
          VScrollBar.ValueRange.ViewportSize := ContentLayoutSize.Height;
          VScrollBar.Value := LViewportPosition.Y;
        finally
          VScrollBar.ValueRange.EndUpdate;
        end;
        VScrollBar.SmallChange := VScrollBar.ViewportSize / 5;
      end;
      if Assigned(HScrollBar) then
      begin
        HScrollBar.ValueRange.BeginUpdate;
        try
          HScrollBar.ValueRange.Min := Min(LViewportPosition.X, FContentBounds.Left);
          HScrollBar.ValueRange.Max := Max(LViewportPosition.X + ContentLayoutSize.Width, FContentBounds.Right);
          HScrollBar.ValueRange.ViewportSize := ContentLayoutSize.Width;
          HScrollBar.Value := LViewportPosition.X;
        finally
          HScrollBar.ValueRange.EndUpdate;
        end;
        HScrollBar.SmallChange := HScrollBar.ViewportSize / 5;
      end;
      {$ENDREGION}
      UpdateSizeGrip;
      NewViewportPosition.X := LViewportPosition.X;
      NewViewportPosition.Y := LViewportPosition.Y;
      NewViewportPosition.X := Round(NewViewportPosition.X * LScale) / LScale;
      NewViewportPosition.Y := Round(NewViewportPosition.Y * LScale) / LScale;
      if Adjust or
         (not SameValue(FLastViewportPosition.X, NewViewportPosition.X, Epsilon)) or
         (not SameValue(FLastViewportPosition.Y, NewViewportPosition.Y, Epsilon)) then
      try
        ViewportPositionChange(FLastViewportPosition, NewViewportPosition, Adjust);
      finally
        FLastViewportPosition := NewViewportPosition;
      end;
      if Adjust then
        Repaint;
    finally
      FInInternalAlign := False;
    end;
  end;
end;

{$ENDREGION}

procedure TScrollBox.AddToTabList(const AObject: TFmxObject);
begin
end;

procedure TScrollBox.ApplyStyle;
var
  B: TFmxObject;
  function CheckParent(Control: TControl): boolean;
  begin
    Result := Assigned(Control) and
              (Control.Parent <> FBackground) and
              (Control.Parent <> self) and
              (Control.Parent is TControl)
  end;

  procedure UpdateScroll(AStyleLookup: string;
                         var Info: array of TScrollInfo;
                         Small: Boolean;
                         Proc: TNotifyEvent);
  var
    B: TFmxObject;
  begin
    B := FindStyleResource(AStyleLookup);
    if Assigned(B) and (B is TScrollBar) then
    begin
      Info[Integer(Small)].Scroll := TScrollBar(B);
      Info[Integer(Small)].Scroll.OnChange := Proc;
      Info[Integer(Small)].Scroll.Visible := False;
      Info[Integer(Small)].Scroll.Locked := True;
      Info[Integer(Small)].Align := TScrollBar(B).Align;
      Info[Integer(Small)].Margins := TScrollBar(B).Margins.Rect;
    end
    else
    begin
      Info[Integer(Small)].Scroll := nil;
      Info[Integer(Small)].Align := TAlignLayout.alNone;
      Info[Integer(Small)].Margins := TRectF.Create(0, 0, 0, 0);
    end;
  end;
begin
  inherited;
  B := FindStyleResource('background');
  if Assigned(B) and (B is TControl) then
  begin
    FBackground := TControl(B);
  end;

  UpdateScroll('vscrollbar', FVScrollInfo, False, VScrollChangeProc);
  UpdateScroll('hscrollbar', FHScrollInfo, False, HScrollChangeProc);
  UpdateScroll('vsmallscrollbar', FVScrollInfo, True, VScrollChangeProc);
  UpdateScroll('hsmallscrollbar', FHScrollInfo, True, HScrollChangeProc);

  B := FindStyleResource('sizegrip');
  if Assigned(B) and (B is TControl) then
  begin
    FSizeGrip := TControl(B);
    FSizeGrip.Visible := False;
    FSizeGrip.Align := TAlignLayout.alBottom;

    if CheckParent(FSizeGrip) then
      FSizeGripParent := TControl(FSizeGrip.Parent);
    if CheckParent(FSizeGripParent) then
      FSizeGripContent := TControl(FSizeGripParent.Parent);

    if Assigned(FSizeGripParent) then
      FSizeGripParent.Align := TAlignLayout.alRight;
    if Assigned(FSizeGripContent) then
    begin
      FSizeGripContent.Visible := False;
      FSizeGripParent.Align := TAlignLayout.alContents;
    end;
  end;

  B := FindStyleResource('content');
  if Assigned(B) and (B is TControl) then
  begin
    FContentLayout := TControl(B);
    FContentMargins := FContentLayout.Margins.Rect;
  end;
  Realign;
end;

procedure TScrollBox.FreeStyle;
var
  I: Integer;
begin
  inherited;
  for I := Low(FHScrollInfo) to High(FHScrollInfo) do
  begin
    FHScrollInfo[I].Scroll := nil;
    FHScrollInfo[I].Margins := TRectF.Create(0, 0, 0, 0);
  end;
  for I := Low(FVScrollInfo) to High(FVScrollInfo) do
  begin
    FVScrollInfo[I].Scroll := nil;
    FVScrollInfo[I].Margins := TRectF.Create(0, 0, 0, 0);
  end;
  FSizeGripParent := nil;
  FSizeGripContent := nil;
  FContentLayout := nil;
  FBackground := nil;
  FSizeGrip := nil;
end;

function TScrollBox.DoCalcContentBounds: TRectF;
var
  i: Integer;
  R, LocalR: TRectF;
begin
  Result := TRectF.Create(0, 0, 0, 0);
  if Assigned(FContent) and Assigned(ContentLayout) then
  begin
    R := ContentLayout.LocalRect;
    for i := 0 to FContent.ControlsCount - 1 do
    if FContent.Controls[i].Visible then
    begin
      {$IFDEF MSWINDOWS}
      if (csDesigning in ComponentState)
        and Supports(FContent.Controls[i], IDesignerControl) then Continue;
      {$ENDIF}
      LocalR := FContent.Controls[i].ParentedRect;
      if (FContent.Controls[i].Align in [TAlignLayout.alTop, TAlignLayout.alMostTop, TAlignLayout.alBottom, TAlignLayout.alMostBottom]) or
         (TAnchorKind.akRight in FContent.Controls[i].Anchors) then
        LocalR.Right := R.Right;
      if (FContent.Controls[i].Align in [TAlignLayout.alLeft, TAlignLayout.alMostLeft, TAlignLayout.alRight, TAlignLayout.alMostRight]) or
         (TAnchorKind.akBottom in FContent.Controls[i].Anchors) then
        LocalR.Bottom := R.Bottom;

      Result.Union(LocalR);
    end;
  end;
end;

function TScrollBox.GetContentBounds: TRectF;
begin
  Result := FContentBounds;
end;

function TScrollBox.HScrollIndex: Integer;
var
  B: Boolean;
begin
  B := AniCalculations.AutoShowing;
  if Assigned(FHScrollInfo[Integer(B)].Scroll) then
    Result := Integer(B)
  else
  begin
    B := not B;
    if Assigned(FHScrollInfo[Integer(B)].Scroll) then
      Result := Integer(B)
    else
      Result := -1;
  end;
end;

function TScrollBox.VScrollIndex: Integer;
var
  B: Boolean;
begin
  B := AniCalculations.AutoShowing;
  if Assigned(FVScrollInfo[Integer(B)].Scroll) then
    Result := Integer(B)
  else
  begin
    B := not B;
    if Assigned(FVScrollInfo[Integer(B)].Scroll) then
      Result := Integer(B)
    else
      Result := -1;
  end;
end;

function TScrollBox.GetHScrollAlign: TAlignLayout;
var
  I: Integer;
begin
  I := HScrollIndex;
  if I >= 0 then
    Result := FHScrollInfo[I].Align
  else
    Result := TAlignLayout.alNone;
end;

function TScrollBox.GetHScrollBar: TScrollBar;
var
  I: Integer;
begin
  I := HScrollIndex;
  if I >= 0 then
    Result := FHScrollInfo[I].Scroll
  else
    Result := nil;
end;

function TScrollBox.GetHScrollMargins: TRectF;
var
  I: Integer;
begin
  I := HScrollIndex;
  if I >= 0 then
    Result := FHScrollInfo[I].Margins
  else
    Result := TRectF.Create(0, 0, 0, 0);
end;

function TScrollBox.GetVScrollAlign: TAlignLayout;
var
  I: Integer;
begin
  I := VScrollIndex;
  if I >= 0 then
    Result := FVScrollInfo[I].Align
  else
    Result := TAlignLayout.alNone;
end;

function TScrollBox.GetVScrollBar: TScrollBar;
var
  I: Integer;
begin
  I := VScrollIndex;
  if I >= 0 then
    Result := FVScrollInfo[I].Scroll
  else
    Result := nil;
end;

function TScrollBox.GetVScrollMargins: TRectF;
var
  I: Integer;
begin
  I := VScrollIndex;
  if I >= 0 then
    Result := FVScrollInfo[I].Margins
  else
    Result := TRectF.Create(0, 0, 0, 0);
end;

procedure TScrollBox.GetTabOrderList(const List: TInterfaceList;
  AChildren: Boolean);
begin
  if Assigned(FContent) then
    FContent.GetTabOrderList(List, AChildren)
  else
    inherited;
end;

procedure TScrollBox.DoRealignContent(R: TRectF);
begin
  if Assigned(FContent) then
  begin
    FContent.SetBounds(R.Left, R.Top, R.Width, R.Height);
    FContent.FRecalcUpdateRect := True;
    FContent.Realign;
  end;
end;

procedure TScrollBox.DoRealign;
var
  LDisablePaint, LDisableInternalAlign: Boolean;
begin
  LDisableInternalAlign := (csDestroying in ComponentState) or FDisableAlign or (FUpdating > 0) or
    (csLoading in ComponentState) or (not Assigned(ContentLayout)) or (not Assigned(Content));
  LDisablePaint := FDisablePaint;
  try
    FDisablePaint := True;
    inherited;
    if not LDisableInternalAlign then
    begin
      InternalAlign;
    end;
  finally
    FDisablePaint := LDisablePaint;
  end;
end;

function TScrollBox.ContentRect: TRectF;
begin
  if Assigned(ContentLayout) then
    Result := ContentLayout.ParentedRect
  else
    Result := LocalRect;
end;

procedure TScrollBox.ViewportPositionChange(const OldViewportPosition, NewViewportPosition: TPointF;
                                            const ContentSizeChanged: boolean);
begin
  if Assigned(FOnViewportPositionChange) then
    FOnViewportPositionChange(self,
                              OldViewportPosition,
                              NewViewportPosition,
                              ContentSizeChanged);
end;

function TScrollBox.VScrollBarValue: Single;
begin
  if Assigned(FAniCalculations) then
    Result := ViewportPosition.Y
  else
    Result := 0;
end;

function TScrollBox.HScrollBarValue: Single;
begin
  if Assigned(FAniCalculations) then
    Result := ViewportPosition.X
  else
    Result := 0;
end;

procedure TScrollBox.HScrollChangeProc(Sender: TObject);
begin
  if (not FInInternalAlign) and Assigned(AniCalculations) then
    HScrollChange;
end;

procedure TScrollBox.VScrollChangeProc(Sender: TObject);
begin
  if (not FInInternalAlign) and Assigned(AniCalculations) then
    VScrollChange;
end;

procedure TScrollBox.HScrollChange;
begin
  ViewportPosition := PointF(HScrollBar.Value, ViewportPosition.Y);
  if not IsOpaque then
    UpdateEffects;
  if Assigned(FOnHScrollChange) then
    FOnHScrollChange(self);
end;

procedure TScrollBox.VScrollChange;
begin
  ViewportPosition := PointF(ViewportPosition.X, VScrollBar.Value);
  if not IsOpaque then
    UpdateEffects;
  if Assigned(FOnVScrollChange) then
    FOnVScrollChange(self);
end;

function TScrollBox.CreateScrollContent: TScrollContent;
begin
  Result := TScrollContent.Create(Self);
end;

function TScrollBox.GetScrollingBehaviours: TScrollingBehaviours;
var
  StyleDescriptor: TStyleDescription;
begin
  if Assigned(Scene) then
    StyleDescriptor := TStyleManager.GetStyleDescriptionForControl(Self)
  else
    StyleDescriptor := nil;
  if (Assigned(StyleDescriptor) and StyleDescriptor.PlatformTarget.Contains('[METROPOLISUI]')) then
    Result := [TScrollingBehaviour.sbAutoShowing]
  else begin
    if Assigned(FSystemInfoSrv) then
      Result := FSystemInfoSrv.GetScrollingBehaviour
    else
      Result := [];
  end;
end;

procedure TScrollBox.MousePosToAni(var X, Y: Single);
var
  LPoint: TPointF;
begin
  LPoint := TPointF.Create(X, Y);
  if Assigned(ContentLayout) then
  begin
    LPoint := ContentLayout.AbsoluteToLocal(LocalToAbsolute(LPoint));
    X := LPoint.X;
    Y := LPoint.Y;
  end;
end;

procedure TScrollBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
begin
  FMouseEvents := True;
  inherited;
  if (Button = TMouseButton.mbLeft) then
  begin
    MousePosToAni(X, Y);
    AniMouseDown(ssTouch in Shift, X, Y);
  end;
end;

procedure TScrollBox.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  FMouseEvents := True;
  inherited;
  if AniCalculations.Down then
  begin
    MousePosToAni(X, Y);
    AniMouseMove(ssTouch in Shift, X, Y);
  end;
end;

procedure TScrollBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
begin
  FMouseEvents := True;
  inherited;
  if (Button = TMouseButton.mbLeft) then
  begin
    MousePosToAni(X, Y);
    AniMouseUp(ssTouch in Shift, X, Y);
  end;
end;

procedure TScrollBox.DoMouseLeave;
begin
  inherited;
  if FMouseEvents and AniCalculations.Down then
  begin
    AniCalculations.MouseLeave;
    if (AniCalculations.LowVelocity) or
       (not AniCalculations.Animation) then
      AniCalculations.Shown := False;
  end;
end;

procedure TScrollBox.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
var
  Offset: Single;
begin
  inherited;
  if (not (Handled or FDisableMouseWheel)) and Assigned(ContentLayout) then
  begin
  if ssHorizontal in Shift then
    begin
      if FContentBounds.Width > ContentLayout.Width then
      begin
        AniCalculations.Shown := True;
        if Assigned(HScrollBar) then
          Offset := HScrollBar.SmallChange
        else
          Offset := ContentLayout.Width / 5;
        Offset := Offset * -1 * (WheelDelta / 120);
        AniCalculations.MouseWheel(Offset, 0);
        Handled := True;
      end;
    end
    else if FContentBounds.Height > ContentLayout.Height then
    begin
      AniCalculations.Shown := True;
      if Assigned(VScrollBar) then
        Offset := VScrollBar.SmallChange
      else
        Offset := ContentLayout.Height / 5;
      Offset := Offset * -1 * (WheelDelta / 120);
      AniCalculations.MouseWheel(0, Offset);
      Handled := True;
    end
    else if FContentBounds.Width > ContentLayout.Width then
    begin
      AniCalculations.Shown := True;
      if Assigned(HScrollBar) then
        Offset := HScrollBar.SmallChange
      else
        Offset := ContentLayout.Width / 5;
      Offset := Offset * -1 * (WheelDelta / 120);
      AniCalculations.MouseWheel(Offset, 0);
      Handled := True;
    end;
  end;
end;

procedure TScrollBox.SaveDisablePaint;
begin
  FVDisablePaint := False;
  FHDisablePaint := False;
  FGDisablePaint := False;
  if Assigned(VScrollBar) then
  begin
    FVDisablePaint := TOpenControl(VScrollBar).FDisablePaint;
    TOpenControl(VScrollBar).FDisablePaint := True;
  end;
  if Assigned(HScrollBar) then
  begin
    FHDisablePaint := TOpenControl(HScrollBar).FDisablePaint;
    TOpenControl(HScrollBar).FDisablePaint := True;
  end;
  if Assigned(FSizeGrip) then
  begin
    FGDisablePaint := TOpenControl(FSizeGrip).FDisablePaint;
    TOpenControl(FSizeGrip).FDisablePaint := True;
  end;
end;

procedure TScrollBox.SetDisablePaint;
begin
  if Assigned(VScrollBar) then
    TOpenControl(VScrollBar).FDisablePaint := True;
  if Assigned(HScrollBar) then
    TOpenControl(HScrollBar).FDisablePaint := True;
  if Assigned(FSizeGrip) then
    TOpenControl(FSizeGrip).FDisablePaint := True;
end;

procedure TScrollBox.InvalidateContentSize;
begin
  FCachedContentSize := TSizeF.Create(0, 0);
  FContentBounds := TRectF.Create(0, 0, 0, 0);
end;

procedure TScrollBox.RealignContent;
begin
  InvalidateContentSize;
  Realign;
end;

procedure TScrollBox.RestoreDisablePaint;
begin
  if Assigned(VScrollBar) then
    TOpenControl(VScrollBar).FDisablePaint := FVDisablePaint;
  if Assigned(HScrollBar) then
    TOpenControl(HScrollBar).FDisablePaint := FHDisablePaint;
  if Assigned(FSizeGrip) then
    TOpenControl(FSizeGrip).FDisablePaint := FGDisablePaint;
end;

procedure TScrollBox.Painting;
begin
  inherited;
  SaveDisablePaint;
  try
    SetDisablePaint;
  except
    RestoreDisablePaint;
    raise;
  end;
end;

procedure TScrollBox.AfterPaint;
begin
  try
    RestoreDisablePaint;
    if Assigned(VScrollBar) and
      (VScrollBar.Visible) and
      (VScrollBar.Opacity > 0) then
      TOpenControl(VScrollBar).PaintInternal;
    if Assigned(HScrollBar) and
      (HScrollBar.Visible) and
      (HScrollBar.Opacity > 0) then
      TOpenControl(HScrollBar).PaintInternal;
    if Assigned(FSizeGrip) and
      (FSizeGrip.Visible) and
      (FSizeGrip.Opacity > 0) then
      TOpenControl(FSizeGrip).PaintInternal;
  finally
    inherited;
  end;
end;

procedure TScrollBox.AniMouseDown(const Touch: Boolean; const X, Y: Single);
begin
  AniCalculations.Averaging := Touch;
  AniCalculations.MouseDown(X, Y);
end;

procedure TScrollBox.AniMouseMove(const Touch: Boolean; const X, Y: Single);
begin
  AniCalculations.MouseMove(X, Y);
  if AniCalculations.Moved then
    AniCalculations.Shown := True;
end;

procedure TScrollBox.AniMouseUp(const Touch: Boolean; const X, Y: Single);
begin
  AniCalculations.MouseUp(X, Y);
  if (AniCalculations.LowVelocity) or
     (not AniCalculations.Animation) then
    AniCalculations.Shown := False;
end;

procedure TScrollBox.DoAddObject(const AObject: TFmxObject);
begin
  if IsAddToContent(AObject) then
    FContent.AddObject(AObject)
  else
    inherited;
end;


procedure TScrollBox.Loaded;
begin
  inherited;
  FContent.Loaded; // ensure that FixupTabList is called for FContent
end;

procedure TScrollBox.Center;
begin
  if Assigned(VScrollBar) and (VScrollBar.Visible) then
  begin
    VScrollBar.Value := (VScrollBar.Max - VScrollBar.ViewportSize) / 2;
  end;
  if Assigned(HScrollBar) and (HScrollBar.Visible) then
  begin
    HScrollBar.Value := (HScrollBar.Max - HScrollBar.ViewportSize) / 2;
  end;
end;

procedure TScrollBox.ScrollTo(const Dx, Dy: Single);
begin
  if Assigned(VScrollBar) and (VScrollBar.Visible) then
    VScrollBar.Value := VScrollBar.Value - Dy;
  if Assigned(HScrollBar) and (HScrollBar.Visible) then
    HScrollBar.Value := HScrollBar.Value - Dx;
end;

procedure TScrollBox.InViewRect(const Rect: TRectF);
begin
end;

procedure TScrollBox.SetAutoHide(const Value: Boolean);
begin
  if FAutoHide <> Value then
  begin
    FAutoHide := Value;
    Realign;
  end;
end;

procedure TScrollBox.SetShowScrollBars(const Value: Boolean);
begin
  if FShowScrollBars <> Value then
  begin
    FShowScrollBars := Value;
    Realign;
  end;
end;

procedure TScrollBox.SetShowSizeGrip(const Value: Boolean);
begin
  if FShowSizeGrip <> Value then
  begin
    FShowSizeGrip := Value;
    Realign;
  end;
end;

function TScrollBox.GetViewportPosition: TPointF;
var
  LScale, X, Y: Double;
begin
  LScale := GetSceneScale;
  X := Round(AniCalculations.ViewportPosition.X * LScale) / LScale;
  Y := Round(AniCalculations.ViewportPosition.Y * LScale) / LScale;
  Result := TPointF.Create(X, Y);
end;

procedure TScrollBox.SetViewportPosition(const Value: TPointF);
var
  LScale, X, Y: Double;
begin
  LScale := GetSceneScale;
  X := Value.X;
  Y := Value.Y;
  AniCalculations.ViewportPosition := TPointD.Create(Round(X * LScale) / LScale, Round(Y * LScale) / LScale);
end;

procedure TScrollBox.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('UseSmallScrollBars', IgnoreBooleanValue, nil, False);
  Filer.DefineProperty('MouseTracking', IgnoreBooleanValue, nil, False);
end;

procedure TScrollBox.Sort(Compare: TFmxObjectSortCompare);
begin
  FContent.Sort(Compare);
end;

procedure TScrollBox.StartScrolling;
begin
  if Assigned(Scene) then
    Scene.ChangeScrollingState(Self, True);
end;

procedure TScrollBox.StopScrolling;
begin
  if Assigned(Scene) then
    Scene.ChangeScrollingState(nil, False);
end;

function TScrollBox.ClientHeight: Single;
begin
  if Assigned(ContentLayout) then
    Result := ContentLayout.Height
  else
    Result := Height;
end;

function TScrollBox.ClientWidth: Single;
begin
  if Assigned(ContentLayout) then
    Result := ContentLayout.Width
  else
    Result := Width;
end;

procedure TScrollBox.CMGesture(var EventInfo: TGestureEventInfo);
var
  LP: TPointF;
begin
  //This is used when scrolling with the finger on top of a control (like a TButton on a TListItem).
  if EventInfo.GestureID = igiPan then
  begin
    FMouseEvents := False;
    LP := ContentLayout.AbsoluteToLocal(EventInfo.Location);
    if TInteractiveGestureFlag.gfBegin in EventInfo.Flags then
      AniMouseDown(True, LP.X, LP.Y)
    else
      if EventInfo.Flags = [] then
        AniMouseMove(True, LP.X, LP.Y)
      else
      if AniCalculations.Down then
        AniMouseUp(True, LP.X, LP.Y);
  end
  else
    inherited;
end;

function TScrollBox.IsAddToContent(const AObject: TFmxObject): Boolean;
begin
  Result := Assigned(FContent)
    and (AObject <> FContent)
    and (AObject <> ResourceLink)
    and not (AObject is TEffect)
    and not (AObject is TAnimation)
    and not ((AObject = FVScrollInfo[0].Scroll) or
             (AObject = FVScrollInfo[1].Scroll) or
             (AObject = FHScrollInfo[0].Scroll) or
             (AObject = FHScrollInfo[1].Scroll) or
             (AObject = FSizeGrip));
end;

function TScrollBox.IsOpaque: Boolean;
begin
  Result := False;
end;

procedure TScrollBox.ContentAddObject(const AObject: TFmxObject);
begin
  RealignContent;
end;

procedure TScrollBox.ContentInsertObject(Index: Integer; const AObject: TFmxObject);
begin
  RealignContent;
end;

procedure TScrollBox.ContentRemoveObject(const AObject: TFmxObject);
begin
  RealignContent;
end;

procedure TScrollBox.ContentBeforeRemoveObject(AObject: TFmxObject);
begin
end;

{ TGridLayout }

procedure TGridLayout.DoAddObject(const AObject: TFmxObject);
begin
  inherited;
  Realign;
end;

procedure TGridLayout.DoRemoveObject(const AObject: TFmxObject);
begin
  inherited;
  Realign;
end;

constructor TGridLayout.Create(AOwner: TComponent);
begin
  inherited;
  FItemHeight := 64;
  FItemWidth := 64;
end;

procedure TGridLayout.DoRealign;
var
  I: Integer;
  CurPos: TPointF;
  LControl: TControl;
  LItemWidth, LItemHeight: Single;
begin
  if FDisableAlign then
    Exit;
  FDisableAlign := True;
  { content }
  CurPos := PointF(Padding.Left, Padding.Top);

  LItemWidth := FItemWidth;
  LItemHeight := FItemHeight;
  if (Orientation = TOrientation.orHorizontal)
    and (LItemWidth < 0) and (ControlsCount > 0) then
  begin
    LItemWidth := (Width - Padding.Left - Padding.Right) / ControlsCount;
    LItemHeight := Height - Padding.Top - Padding.Bottom;
  end
  else if (Orientation = TOrientation.orVertical)
    and (LItemHeight < 0) and (ControlsCount > 0) then
  begin
    LItemWidth := Width - Padding.Left - Padding.Right;
    LItemHeight := (Height - Padding.Top - Padding.Bottom) / ControlsCount;
  end;

  for I := 0 to ControlsCount - 1 do
  begin
    LControl := Controls[I];
    // Avoid realigning designer "things" like grab handles
    {$IFDEF MSWINDOWS}
    if (csDesigning in ComponentState)
      and Supports(LControl, IDesignerControl) then Continue;
    {$ENDIF}

    LControl.SetBounds(CurPos.X + LControl.Margins.Left, CurPos.Y + LControl.Margins.Top,
      LItemWidth - LControl.Margins.Left - LControl.Margins.Right, LItemHeight - LControl.Margins.Top -
      LControl.Margins.Bottom);
    if Orientation = TOrientation.orHorizontal then
    begin
      CurPos.X := CurPos.X + LItemWidth;
      if CurPos.X + LItemWidth > Self.Width - Self.Padding.Left -
        Self.Padding.Right then
      begin
        CurPos.X := Self.Padding.Left;
        CurPos.Y := CurPos.Y + LItemHeight;
      end;
    end
    else
    begin
      CurPos.Y := CurPos.Y + LItemHeight;
      if CurPos.Y + LItemHeight > Self.Height - Self.Padding.Top -
        Self.Padding.Bottom then
      begin
        CurPos.Y := Self.Padding.Top;
        CurPos.X := CurPos.X + LItemWidth;
      end;
    end;
  end;
  FDisableAlign := False;
end;

procedure TGridLayout.SetItemHeight(const Value: Single);
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    Realign;
  end;
end;

procedure TGridLayout.SetItemWidth(const Value: Single);
begin
  if FItemWidth <> Value then
  begin
    FItemWidth := Value;
    Realign;
  end;
end;

procedure TGridLayout.SetOrientation(const Value: TOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    Realign;
  end;
end;

{ TScaledLayout }

constructor TScaledLayout.Create(AOwner: TComponent);
begin
  inherited;
  HitTest := False;
  FOriginalWidth := Width;
  FOriginalHeight := Height;
end;

destructor TScaledLayout.Destroy;
begin
  inherited;
end;

procedure TScaledLayout.DoRealign;
begin
  if Assigned(Parent) and (Parent is TScrollBox) and
    (TScrollBox(Parent).FUpdating > 0) then
    Exit;

  if csDesigning in ComponentState then
    inherited DoRealign
  else
  begin
    if FNeedAlign then
      AlignObjects(Self, Padding, FOriginalWidth, FOriginalHeight, FLastWidth, FLastHeight, FDisableAlign);

    RecalcAbsolute;
    FRecalcUpdateRect := True;
  end;
end;

function TScaledLayout.GetChildrenMatrix(var Matrix: TMatrix; var Simple: Boolean): Boolean;
begin
  Result := True;
  if ((csDesigning in ComponentState)) then
  begin
    OriginalHeight := Height;
    OriginalWidth := Width;
  end;
  Matrix := TMatrix.Identity;
  Matrix.m11 := Width / FOriginalWidth;
  Matrix.m22 := Height / FOriginalHeight;
  Simple := SameValue(Matrix.m11, 1.0, Epsilon) and SameValue(Matrix.m22, 1.0, Epsilon);
end;

procedure TScaledLayout.Paint;
var
  R: TRectF;
begin
  if (csDesigning in ComponentState) and not Locked and not FInPaintTo then
  begin
    R := LocalRect;
    InflateRect(R, -0.5, -0.5);
    Canvas.DrawDashRect(R, 0, 0, AllCorners, AbsoluteOpacity, $A0909090);
  end;
  inherited;
end;

procedure TScaledLayout.SetOriginalHeight(const Value: Single);
begin
  if FOriginalHeight <> Value then
  begin
    FOriginalHeight := Value;
    if FOriginalHeight < 1 then
      FOriginalHeight := 1;
    RecalcAbsolute;
  end;
end;

procedure TScaledLayout.SetOriginalWidth(const Value: Single);
begin
  if FOriginalWidth <> Value then
  begin
    FOriginalWidth := Value;
    if FOriginalWidth < 1 then
      FOriginalWidth := 1;
    RecalcAbsolute;
  end;
end;

procedure TScaledLayout.SetHeight(const Value: Single);
begin
  inherited;
  if (csDesigning in ComponentState) then
    OriginalHeight := Height
  else
    RecalcAbsolute;
end;

procedure TScaledLayout.SetWidth(const Value: Single);
begin
  inherited;
  if (csDesigning in ComponentState) then
    OriginalWidth := Width
  else
    RecalcAbsolute;
end;

{ TVertScrollBox }

function TVertScrollBox.DoCalcContentBounds: TRectF;
begin
  if Assigned(FContent) and Assigned(ContentLayout) then
  begin
    FContent.Width := ContentLayout.Width;
  end;
  Result := inherited DoCalcContentBounds;
end;

procedure TVertScrollBox.DoUpdateAniCalculations(const AAniCalculations: TScrollCalculations);
begin
  inherited DoUpdateAniCalculations(AAniCalculations);
  AAniCalculations.TouchTracking := AAniCalculations.TouchTracking - [ttHorizontal];
end;


function TVertScrollBox.GetDefaultStyleLookupName: string;
begin
  Result := 'scrollboxstyle';
end;

{ THorzScrollBox }

function THorzScrollBox.DoCalcContentBounds: TRectF;
begin
  if Assigned(FContent) and Assigned(ContentLayout) then
  begin
    FContent.Height := ContentLayout.Height;
  end;
  Result := inherited DoCalcContentBounds;
end;

procedure THorzScrollBox.DoUpdateAniCalculations(const AAniCalculations: TScrollCalculations);
begin
  inherited DoUpdateAniCalculations(AAniCalculations);
  AAniCalculations.TouchTracking := AAniCalculations.TouchTracking - [ttVertical];
end;


function THorzScrollBox.GetDefaultStyleLookupName: string;
begin
  Result := 'scrollboxstyle';
end;

{ TFramedVertScrollBox }

function TFramedVertScrollBox.GetDefaultStyleLookupName: string;
begin
  Result := 'framedscrollboxstyle';
end;

function TFramedVertScrollBox.IsOpaque: Boolean;
begin
  Result := True;
end;

{ TFlowLayout }

procedure TFlowLayout.DoAddObject(const AObject: TFmxObject);
begin
  inherited;
  Realign;
end;

procedure TFlowLayout.DoRemoveObject(const AObject: TFmxObject);
begin
  inherited;
  Realign;
end;

constructor TFlowLayout.Create(AOwner: TComponent);
begin
  inherited;
  FRules.Justify := TFlowJustify.fjLeft;
  FRules.JustifyLast := TFlowJustify.fjLeft;
  FRules.Direction := TFlowDirection.fdLeftToRight;
  FRules.HorizontalGap := 0;
  FRules.VerticalGap := 0;
end;

procedure TFlowLayout.DoRealign;
  function ControlWidth(Control : TControl) : Single;
  begin
    if Control.Visible then
      Result := Control.Width + Control.Margins.Left + Control.Margins.Right
    else
      Result := 0;
  end;

  function ControlHeight(Control : TControl; const Rules: TFlowLayoutRules) : Single;
  begin
    if Control.Visible then
      Result := Control.Height
              + Control.Margins.Top + Control.Margins.Bottom
              + Rules.VerticalGap
    else
      Result := 0;
  end;

  function GetJustify(LastLine : Boolean; const Rules : TFlowLayoutRules) : TFlowJustify;
  begin
    if LastLine then
      Result := Rules.JustifyLast
    else
      Result := Rules.Justify;
  end;

  function Gap(first : Boolean) : Single;
  begin
    if first then
      Result := 0
    else
      Result := FRules.HorizontalGap;
  end;

type
  TState = (sNewLine, sMeasure, sPreLayout, sLayout, sEnd);
const
  InitialState: array [False..True] of TState = (sEnd, sNewLine);
var
  i : Integer;
  State : TState;
  CurPos : TPointF;
  Control : TControl;
  ControlBounds : TRectF;
  LineStartIdx, LineEndIdx : Integer;
  WidthAccu : Single;
  LineHeight : Single;
  ClientWidth : Single;
  Add : Single;
  NextWidth : Single;

  NextRules : TFlowLayoutRules;
  CurrentRules : TFlowLayoutRules;

begin
  if FDisableAlign or (Controls = nil) then
    Exit;

  FDisableAlign := true;

  CurPos := PointF(Padding.Left, Padding.Top);
  ClientWidth := Self.Width - Self.Padding.Left - Self.Padding.Right;
  LineHeight := 0;

  CurrentRules := Self.FRules;
  NextRules := Self.FRules;

  i := 0;
  LineStartIdx := i;
  LineEndIdx := i;
  WidthAccu := 0;
  Add := 0;
  State := InitialState[Controls.Count <> 0];
  while State <> sEnd do
  begin
    Control := TControl(Controls[i]);

    case State of
      sNewLine:
        begin
          CurrentRules := NextRules;

          LineStartIdx := i;
          LineEndIdx := i;
          WidthAccu := 0;
          CurPos.Y := CurPos.Y + LineHeight;
          LineHeight := ControlHeight(Control, CurrentRules);
          State := sMeasure;
        end;

      sMeasure:
        begin
          NextWidth := WidthAccu + Gap(i = LineStartIdx) + ControlWidth(Control);
          if Control is TFlowLayoutBreak then
          begin
            if TFlowLayoutBreak(Control).ChangesRules then
              NextRules := TFlowLayoutBreak(Control).FRules;
            i := LineStartIdx;
            State := sPreLayout;
          end
          else if (NextWidth > ClientWidth) then
          begin
            // width overflow, layout the line

            // update WidthAccu if the line has only 1 control
            if i = LineStartIdx then
              WidthAccu := NextWidth;

            i := LineStartIdx;
            State := sPreLayout;
          end
          else if i = Controls.Count - 1 then
          begin
            // no more controls, layout the line and done
            WidthAccu := NextWidth;
            LineHeight := Max(LineHeight, ControlHeight(Control, CurrentRules));
            LineEndIdx := i;
            i := LineStartIdx;
            State := sPreLayout;
          end
          else begin
            // keep counting width
            WidthAccu := NextWidth;
            LineEndIdx := i;
            LineHeight := Max(LineHeight, ControlHeight(Control, CurrentRules));
            Inc(i);
          end;
        end;

    sPreLayout:
      begin
        // Prepare to layout the line
        Add := 0;

        case GetJustify(LineEndIdx = (Controls.Count - 1), CurrentRules) of
          fjLeft:
            CurPos.X := Padding.Left;
          fjRight:
            CurPos.X := Self.Width - Padding.Right - WidthAccu;
          fjCenter:
            CurPos.X := Padding.Left + ClientWidth/2 - WidthAccu/2;
          fjJustify:
            begin
              CurPos.X := Padding.Left;
              if LineEndIdx - LineStartIdx > 0 then
                Add := (ClientWidth - WidthAccu) / (LineEndIdx - LineStartIdx);
            end;
        end;

        case CurrentRules.Direction of
          fdLeftToRight: i := LineStartIdx;
          fdRightToLeft: i := LineEndIdx;
        end;

        State := sLayout;
      end;

    sLayout:
      begin
        ControlBounds := Control.BoundsRect;
        ControlBounds.SetLocation(CurPos.X + Control.Margins.Left,
                                  CurPos.Y + Control.Margins.Top);

        if not ((csDesigning in ComponentState)
          and Supports(TControl(Self.Children[i]), IDesignerControl)) then
        begin
          Control.BoundsRect := ControlBounds;
          CurPos.X := CurPos.X + ControlWidth(Control) + Add + CurrentRules.HorizontalGap;
        end;

        // advance to the next control in line
        case CurrentRules.Direction of
          fdLeftToRight:
            begin
              if i = Controls.Count - 1 then
                State := sEnd
              else if i = LineEndIdx then
                State := sNewLine;

              Inc(i);
            end;
          fdRightToLeft:
            begin
              if i = LineStartIdx then
              begin
                i := LineEndIdx + 1;

                if LineEndIdx = Controls.Count - 1 then
                  State := sEnd
                else
                  State := sNewLine;
              end
              else
                Dec(i);
            end;
        end;
      end;
    end;
  end;
  FDisableAlign := false;
end;

procedure TFlowLayout.SetFlowDirection(ADirection: TFlowDirection);
begin
  FRules.Direction := ADirection;
  Realign;
end;

procedure TFlowLayout.SetJustify(AJustify: TFlowJustify);
begin
  FRules.Justify := AJustify;
  Realign;
end;

procedure TFlowLayout.SetJustifyLast(AJustify: TFlowJustify);
begin
  FRules.JustifyLast := AJustify;
  Realign;
end;

procedure TFlowLayout.SetHGap(AHGap: Single);
begin
  FRules.HorizontalGap := AHGap;
  Realign;
end;

procedure TFlowLayout.SetVGap(AVGap: Single);
begin
  FRules.VerticalGap := AVGap;
  Realign;
end;

{ TFlowLayoutBreak }

constructor TFlowLayoutBreak.Create(AOwner: TComponent);
begin
  inherited;
  Width := 4;
  Height := 15;
  Visible := False;
end;

procedure TFlowLayoutBreak.Paint;
begin
  Canvas.FillRect(LocalRect, 0, 0, [], AbsoluteOpacity, TCornerType.ctInnerLine);
  Canvas.DrawRect(LocalRect, 0, 0, [], AbsoluteOpacity, TCornerType.ctInnerLine);
end;

procedure TFlowLayoutBreak.SetChangesRules(AChangesRules: Boolean);
begin
  FChangesRules := AChangesRules;
end;

{ TFramedScrollBox }

function TFramedScrollBox.IsOpaque: Boolean;
begin
  Result := True;
end;

{ TGridPanelLayout }

{ TGridPanelLayout.TCellItem }

procedure TGridPanelLayout.TCellItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TCellItem then
  begin
    TCellItem(Dest).FSizeStyle := Self.FSizeStyle;
    TCellItem(Dest).FValue := Self.FValue;
    TCellItem(Dest).FSize := Self.FSize;
  end;
end;

constructor TGridPanelLayout.TCellItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FSizeStyle := ssPercent;
end;

procedure TGridPanelLayout.TCellItem.SetSizeStyle(Value: TSizeStyle);
begin
  if Value <> FSizeStyle then
  begin
    FSizeStyle := Value;
    Changed(False);
  end;
end;

procedure TGridPanelLayout.TCellItem.SetValue(Value: Double);
begin
  if Value <> FValue then
  begin
    if FSizeStyle = ssAbsolute then
    begin
      FSize := Trunc(Value);
      FValue := FSize;
    end else
      FValue := Value;
    Changed(False);
  end;
end;

{ TGridPanelLayout.TCellCollection }

function TGridPanelLayout.TCellCollection.GetAttr(Index: Integer): string;
begin
  case Index of
    0: Result := SCellMember;
    1: Result := SCellSizeType;
    2: Result := SCellValue;
  else
    Result := '';
  end;
end;

function TGridPanelLayout.TCellCollection.GetAttrCount: Integer;
begin
  Result := 3;
end;

function TGridPanelLayout.TCellCollection.GetItem(Index: Integer): TCellItem;
begin
  Result := TCellItem(inherited GetItem(Index));
end;

function TGridPanelLayout.TCellCollection.GetItemAttr(Index,
  ItemIndex: Integer): string;

  function GetSizeStyleString(Index: Integer): string;
  begin
    case Items[Index].SizeStyle of
      ssAbsolute: Result := SCellAbsoluteSize;
      ssPercent: Result := SCellPercentSize;
      ssAuto: Result := SCellAutoSize;
    else
      Result := '';
    end;
  end;

  function GetValueString(Index: Integer): string;
  var
    Item: TCellItem;
  begin
    Item := Items[Index];
    if Item.SizeStyle = TSizeStyle.ssAbsolute then
      Result := IntToStr(Trunc(Item.Value))
    else if Item.SizeStyle = TSizeStyle.ssPercent then
      Result := Format('%3.2f%%', [Item.Value]) // do not localize
    else Result := SCellAutoSize;
  end;

begin
  case Index of
    1: Result := GetSizeStyleString(ItemIndex);
    2: Result := GetValueString(ItemIndex);
  else
    Result := '';
  end;
end;

function TGridPanelLayout.TCellCollection.Owner: TGridPanelLayout;
begin
  Result := TGridPanelLayout(GetOwner);
end;

procedure TGridPanelLayout.TCellCollection.SetItem(Index: Integer; Value: TCellItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TGridPanelLayout.TCellCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if Owner <> nil then
  begin
    Owner.FRecalcCellSizes := True;
    Owner.Realign;
  end;
end;

{ TGridPanelLayout.TRowCollection }

constructor TGridPanelLayout.TRowCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TRowItem);
end;

function TGridPanelLayout.TRowCollection.Add: TRowItem;
begin
  Result := TRowItem(inherited Add);
end;

function TGridPanelLayout.TRowCollection.GetItemAttr(Index,
  ItemIndex: Integer): string;
begin
  if Index = 0 then
    Result := Format(SCellRow, [ItemIndex])
  else
    Result := inherited GetItemAttr(Index, ItemIndex);
end;

procedure TGridPanelLayout.TRowCollection.Notify(Item: TCollectionItem;
  Action: System.Classes.TCollectionNotification);
begin
  inherited;
  if (Action = System.Classes.TCollectionNotification.cnExtracting) and not (csDestroying in Owner.ComponentState) and
     not (csUpdating in Owner.ComponentState) then
    if not Owner.IsRowEmpty(Item.Index) then
      raise EGridLayoutException.Create(SCannotDeleteRow)
    else Owner.UpdateControlsRow(Item.Index);
end;

{ TGridPanelLayout.TColumnCollection }

function TGridPanelLayout.TColumnCollection.Add: TColumnItem;
begin
  Result := TColumnItem(inherited Add);
end;

constructor TGridPanelLayout.TColumnCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TColumnItem);
end;

function TGridPanelLayout.TColumnCollection.GetItemAttr(Index,
  ItemIndex: Integer): string;
begin
  if Index = 0 then
    Result := Format(SCellColumn, [ItemIndex])
  else
    Result := inherited GetItemAttr(Index, ItemIndex);
end;

procedure TGridPanelLayout.TColumnCollection.Notify(Item: TCollectionItem;
  Action: System.Classes.TCollectionNotification);
begin
  inherited;
  if (Action = System.Classes.TCollectionNotification.cnExtracting) and not (csDestroying in Owner.ComponentState) and
     not (csUpdating in Owner.ComponentState) then
    if not Owner.IsColumnEmpty(Item.Index) then
      raise EGridLayoutException.Create(SCannotDeleteColumn)
    else
      Owner.UpdateControlsColumn(Item.Index);
end;

{ TGridPanelLayout.TControlCollection }

function TGridPanelLayout.TControlCollection.Add: TControlItem;
begin
  Result := TControlItem(inherited Add);
end;

procedure TGridPanelLayout.TControlCollection.AddControl(AControl: TControl; AColumn, ARow: Integer);
  procedure PlaceInCell(ControlItem: TControlItem);
  var
    I, J: Integer;
  begin
    try
      ControlItem.Control := AControl;
      ControlItem.FRow := -1;
      ControlItem.FColumn := -1;
      if (ARow = -1) and (AColumn > -1) then
      begin
        for I := 0 to Owner.RowCollection.Count - 1 do
          if Controls[AColumn, I] = nil then
          begin
            ControlItem.Row := I;
            ControlItem.Column := AColumn;
            Exit;
          end;
        AColumn := -1;
      end;
      if (AColumn = -1) and (ARow > -1) then
      begin
        for I := 0 to Owner.ColumnCollection.Count - 1 do
          if Controls[I, ARow] = nil then
          begin
            ControlItem.Column := I;
            ControlItem.Row := ARow;
            Exit;
          end;
        ARow := -1;
      end;
      if (AColumn > -1) and (ARow > -1) then
      begin
        if Controls[AColumn, ARow] = nil then
        begin
          ControlItem.Column := AColumn;
          ControlItem.Row := ARow;
          Exit;
        end;
        AColumn := -1;
        ARow := -1;
      end;
      if (ARow = -1) and (AColumn = -1) then
      begin
        for J := 0 to Owner.RowCollection.Count - 1 do
          for I := 0 to Owner.ColumnCollection.Count - 1 do
            if Controls[I, J] = nil then
            begin
              ControlItem.Row := J;
              ControlItem.Column := I;
              Exit;
            end;
      end;
      if (ControlItem.Row = -1) or (ControlItem.Column = -1) then
        if (Owner <> nil) and (Owner.ExpandStyle <> emFixedSize) then
        begin
          if Owner.ExpandStyle = emAddRows then
            Owner.AutoAddRow
          else
            Owner.AutoAddColumn;
          PlaceInCell(ControlItem);
        end else
          raise EGridLayoutException.Create(SCannotAddFixedSize);
    except
      ControlItem.Control := nil;
      raise;
    end;
  end;

begin
  if IndexOf(AControl) < 0 then
    PlaceInCell(Add);
end;

constructor TGridPanelLayout.TControlCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TControlItem);
end;

function TGridPanelLayout.TControlCollection.GetControl(AColumn, ARow: Integer): TControl;
var
  ControlItem: TControlItem;
begin
  ControlItem := GetControlItem(AColumn, ARow);
  if ControlItem <> nil then
    Result := ControlItem.Control
  else
    Result := nil;
end;

function TGridPanelLayout.TControlCollection.GetControlItem(AColumn, ARow: Integer): TControlItem;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := TControlItem(Items[I]);
    if (ARow >= Result.Row) and (ARow <= Result.Row + Result.RowSpan - 1) and
      (AColumn >= Result.Column) and (AColumn <= Result.Column + Result.ColumnSpan - 1) then
      Exit;
  end;
  Result := nil;
end;

function TGridPanelLayout.TControlCollection.GetItem(Index: Integer): TControlItem;
begin
  Result := TControlItem(inherited GetItem(Index));
end;

function TGridPanelLayout.TControlCollection.IndexOf(AControl: TControl): Integer;
begin
  for Result := 0 to Count - 1 do
    if TControlItem(Items[Result]).Control = AControl then
      Exit;
  Result := -1;
end;

function TGridPanelLayout.TControlCollection.Owner: TGridPanelLayout;
begin
  Result := TGridPanelLayout(GetOwner);
end;

procedure TGridPanelLayout.TControlCollection.RemoveControl(AControl: TControl);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Items[I].Control = AControl then
    begin
      Items[I].Control := nil;
      Delete(I);
      Exit;
    end;
end;

procedure TGridPanelLayout.TControlCollection.SetControl(AColumn, ARow: Integer; Value: TControl);
var
  Index: Integer;
  ControlItem: TControlItem;
begin
  if Owner <> nil then
  begin
    if (AColumn < 0) or (AColumn >= Owner.ColumnCollection.Count) then
      raise EGridLayoutException.CreateFmt(SInvalidColumnIndex, [AColumn]);
    if (ARow < 0) or (ARow >= Owner.RowCollection.Count) then
      raise EGridLayoutException.CreateFmt(SInvalidRowIndex, [ARow]);
    Index := IndexOf(Value);
    if Index > -1 then
    begin
      ControlItem := Items[Index];
      ControlItem.FColumn := AColumn;
      ControlItem.FRow := ARow;
    end else
      AddControl(Value, AColumn, ARow);
  end;
end;

procedure TGridPanelLayout.TControlCollection.SetItem(Index: Integer; Value: TControlItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TGridPanelLayout.TControlCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if Owner <> nil then
  begin
    Owner.FRecalcCellSizes := True;
    Owner.Realign;
  end;
end;

{ TGridPanelLayout.TControlItem }

procedure TGridPanelLayout.TControlItem.AssignTo(Dest: TPersistent);
var
  DestControlItem: TControlItem;
begin
  if Dest is TControlItem then
  begin
    DestControlItem := TControlItem(Dest);
    DestControlItem.FControl := Self.Control;
    DestControlItem.FRow := Self.Row;
    DestControlItem.FColumn := Self.Column;
    DestControlItem.Changed(False);
  end;
end;

constructor TGridPanelLayout.TControlItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FRowSpan := 1;
  FColumnSpan := 1;
end;

destructor TGridPanelLayout.TControlItem.Destroy;
var
  LControl: TControl;
begin
  if Assigned(FControl) and not (csLoading in GridPanelLayout.ComponentState) and
     not (csUpdating in GridPanelLayout.ComponentState) and
     not (csDestroying in GridPanelLayout.ComponentState) then
  begin
    LControl := FControl;
    FControl := nil;
    LControl.Parent := nil;
    LControl.Free;
  end;
  inherited;
end;

function TGridPanelLayout.TControlItem.GetGridPanelLayout: TGridPanelLayout;
var
  Owner: TControlCollection;
begin
  Owner := TControlCollection(GetOwner);
  if Owner <> nil then
    Result := Owner.Owner
  else
    Result := nil;
end;

procedure TGridPanelLayout.TControlItem.SetControl(Value: TControl);
begin
  if FControl <> Value then
  begin
    if Value = GridPanelLayout then
      raise EGridLayoutException.Create(SInvalidControlItem);
    FControl := Value;
    Changed(False);
  end;
end;

procedure TGridPanelLayout.TControlItem.SetColumn(Value: Integer);
begin
  if FColumn <> Value then
  begin
    if not (csLoading in GridPanelLayout.ComponentState) then
    begin
      if (Value < 0)or (Value > GridPanelLayout.ColumnCollection.Count - 1) then
        raise EGridLayoutException.CreateFmt(SInvalidColumnIndex, [Value]);
      InternalSetLocation(Value, FRow, False, True);
    end
    else
      FColumn := Value;
  end;
end;

procedure TGridPanelLayout.TControlItem.SetRow(Value: Integer);
begin
  if FRow <> Value then
  begin
    if not (csLoading in GridPanelLayout.ComponentState) then
    begin
      if (Value < 0) or (Value > GridPanelLayout.RowCollection.Count - 1) then
        raise EGridLayoutException.CreateFmt(SInvalidRowIndex, [Value]);
      InternalSetLocation(FColumn, Value, False, True);
    end
    else
      FRow := Value;
  end;
end;

type
  TNewLocationRec = record
    ControlItem: TGridPanelLayout.TControlItem;
    NewColumn, NewRow: Integer;
    Pushed: Boolean;
  end;
  TNewLocationRecs = array of TNewLocationRec;

  TNewLocations = class
  private
    FNewLocations: TNewLocationRecs;
    FCount: Integer;
  public
    function AddNewLocation(AControlItem: TGridPanelLayout.TControlItem; ANewColumn, ANewRow: Integer; APushed: Boolean = False): Integer;
    procedure ApplyNewLocations;
    property Count: Integer read FCount;
    property NewLocations: TNewLocationRecs read FNewLocations;
  end;

function TNewLocations.AddNewLocation(AControlItem: TGridPanelLayout.TControlItem; ANewColumn, ANewRow: Integer; APushed: Boolean): Integer;
var
  NLRec: TNewLocationRec;
begin
  if FCount = Length(FNewLocations) then
    SetLength(FNewLocations, Length(FNewLocations) + 10);
  Result := FCount;
  Inc(FCount);
  NLRec := FNewLocations[Result];
  NLRec.ControlItem := AControlItem;
  NLRec.NewColumn := ANewColumn;
  NLRec.NewRow := ANewRow;
  NLRec.Pushed := APushed;
end;

procedure TNewLocations.ApplyNewLocations;
var
  I: Integer;
  NLRec: TNewLocationRec;
begin
  for I := 0 to FCount - 1 do
  begin
    NLRec := FNewLocations[I];
    if NLRec.ControlItem <> nil then
      NLRec.ControlItem.InternalSetLocation(NLRec.NewColumn, NLRec.NewRow, NLRec.Pushed, False);
  end;
end;

procedure TGridPanelLayout.TControlItem.SetRowSpan(Value: TCellSpan);
var
  I, Delta: Integer;
  Collection: TControlCollection;
  ControlItem: TControlItem;
  NumToAdd, NumRows, MoveBy, LColumn, LRow, IndexDelta: Integer;
  Span: TCellSpan;
  NewLocations: TNewLocations;
begin
  if FRowSpan <> Value then
  begin
    if Value < 1 then
      raise EGridLayoutException.CreateFmt(SInvalidSpan, [Value]);
    Collection := TControlCollection(GetOwner);
    if Collection = nil then Exit;
    GridPanelLayout.BeginUpdate;
    try
      NewLocations := TNewLocations.Create;
      try
        if FRowSpan > Value then
        begin
          Delta := FRowSpan - Value;
          FRowSpan := Value;
          if GridPanelLayout.ExpandStyle in [emAddRows, emFixedSize] then
          begin
            NumRows := GridPanelLayout.RowCollection.Count;
            // Move the controls below up to fill in the space.
            for I := FRow + FRowSpan + Delta to NumRows - 1 do
            begin
              ControlItem := Collection.ControlItems[FColumn, I];
              if ControlItem <> nil then
                if ControlItem.Pushed then
                  NewLocations.AddNewLocation(ControlItem, FColumn, I - Delta, False)
                else
                  Break;
            end;
            NewLocations.ApplyNewLocations;
            GridPanelLayout.RemoveEmptyAutoAddRows;
          end else
          begin
            // Scan forward in row primary order, removing Delta from position of each
            // control item, unwrapping where nessesary, until the last control is reached
            // or a non "pushed" control is found.
            for I := GridPanelLayout.RowSpanIndex[FColumn, FRow] to GridPanelLayout.CellCount - 1 do
            begin
              GridPanelLayout.CellIndexToCell(I, LColumn, LRow);
              ControlItem := Collection.ControlItems[LColumn, LRow];
              if ControlItem <> nil then
              begin
                if ControlItem.Pushed then
                begin
                  if (ControlItem.Column = LColumn) and (ControlItem.Row = LRow) then
                  begin
                    GridPanelLayout.CellIndexToCell(I - Delta, LColumn, LRow);
                    if (LRow > 0) and (LRow + ControlItem.RowSpan > GridPanelLayout.RowCollection.Count) then
                    begin
                      Inc(Delta, (LRow + ControlItem.RowSpan) - GridPanelLayout.RowCollection.Count);
                      GridPanelLayout.CellIndexToCell(I - Delta, LColumn, LRow);
                    end;
                    NewLocations.AddNewLocation(ControlItem, LColumn, LRow, False);
                  end;
                end else if ControlItem <> Self then
                  Break
                else
                  NewLocations.AddNewLocation(ControlItem, LColumn, LRow, False);
              end;
            end;
            NewLocations.ApplyNewLocations;
            GridPanelLayout.RemoveEmptyAutoAddRows;
          end;
        end else
        begin
          NumRows := GridPanelLayout.RowCollection.Count;
          Delta := Value - FRowSpan;
          // first check if there is room down to expand and if so remove those
          // rows from the Delta
          for I := Min(FRow + FRowSpan, NumRows) to Min(FRow + Value - 1, NumRows - 1) do
            if Collection.Controls[FColumn, I] = nil then
              Dec(Delta)
            else
              Break;
          MoveBy := Delta;
          // Now find out how many rows to add, if any
          for I := NumRows - 1 downto NumRows - MoveBy do
            if Collection.Controls[FColumn, I] = nil then
              Dec(Delta)
            else
              Break;
          NumToAdd := Delta;
          // Add the rows
          if GridPanelLayout.ExpandStyle in [emAddRows, emFixedSize] then
          begin
            if (GridPanelLayout.ExpandStyle = emFixedSize) and (NumToAdd > 0) then
              raise EGridLayoutException.Create(SCannotAddFixedSize);
            while NumToAdd > 0 do
            begin
              GridPanelLayout.AutoAddRow;
              Dec(NumToAdd);
            end;
            NumRows := GridPanelLayout.RowCollection.Count;
            for I := NumRows - 1 downto NumRows - Delta do
            begin
              ControlItem := Collection.ControlItems[FColumn, I - MoveBy];
              if (ControlItem <> nil) and (ControlItem <> Self) then
                NewLocations.AddNewLocation(ControlItem, FColumn, I, True);
            end;
            NewLocations.ApplyNewLocations;
          end else if (NumToAdd + MoveBy) > 0 then
          begin
            IndexDelta := Max(NumToAdd, Min(MoveBy, NumRows));
            for I := GridPanelLayout.RowSpanIndex[FColumn, FRow] to GridPanelLayout.CellCount - 1 do
            begin
              GridPanelLayout.CellIndexToCell(I, LColumn, LRow);
              ControlItem := Collection.ControlItems[LColumn, LRow];
              if (ControlItem <> nil) and (ControlItem.Column = LColumn) and (ControlItem.Row = LRow) then
              begin
                if ControlItem = Self then
                begin
                  Span := Value;
                  LColumn := FColumn;
                  LRow := FRow;
                end else
                begin
                  Span := ControlItem.RowSpan;
                  GridPanelLayout.CellIndexToCell(I + IndexDelta, LColumn, LRow);
                end;
                if LRow + Span > GridPanelLayout.RowCollection.Count then
                begin
                  if LRow > 0 then
                  begin
                    Inc(IndexDelta, GridPanelLayout.RowCollection.Count - LRow);
                    GridPanelLayout.CellIndexToCell(I + IndexDelta - NumToAdd, LColumn, LRow);
                  end else if ControlItem <> Self then
                  begin
                    Inc(IndexDelta, Min(Span, GridPanelLayout.RowCollection.Count));
                    GridPanelLayout.CellIndexToCell(I + IndexDelta, LColumn, LRow);
                  end else if (ControlItem = Self) and (LRow = 0) then
                    Exit;
                end;
                NumToAdd := 0;
                NewLocations.AddNewLocation(ControlItem, LColumn, LRow, True);
              end;
            end;
            for I := 0 to NewLocations.Count - 1 do
              if NewLocations.NewLocations[I].NewColumn > GridPanelLayout.ColumnCollection.Count - 1 then
                GridPanelLayout.AutoAddColumn;
            NewLocations.ApplyNewLocations;
          end;
          FRowSpan := Value;
        end;
        Changed(False);
      finally
        NewLocations.Free;
      end;
    finally
      GridPanelLayout.EndUpdate;
    end;
  end;
end;

procedure TGridPanelLayout.TControlItem.SetColumnSpan(Value: TCellSpan);
var
  I, Delta: Integer;
  Collection: TControlCollection;
  ControlItem: TControlItem;
  NumToAdd, NumColumns, MoveBy, LColumn, LRow, IndexDelta: Integer;
  Span: TCellSpan;
  NewLocations: TNewLocations;
begin
  if FColumnSpan <> Value then
  begin
    if Value < 1 then
      raise EGridLayoutException.CreateFmt(SInvalidSpan, [Value]);
    Collection := TControlCollection(GetOwner);
    if Collection = nil then Exit;
    GridPanelLayout.BeginUpdate;
    try
      NewLocations := TNewLocations.Create;
      try
        if FColumnSpan > Value then
        begin
          Delta := FColumnSpan - Value;
          FColumnSpan := Value;
          if GridPanelLayout.ExpandStyle in [emAddColumns, emFixedSize] then
          begin
            NumColumns := GridPanelLayout.ColumnCollection.Count;
            // Move the controls to the right back to fill in the space.
            for I := FColumn + FColumnSpan + Delta to NumColumns - 1 do
            begin
              ControlItem := Collection.ControlItems[I, FRow];
              if ControlItem <> nil then
                if ControlItem.Pushed then
                  NewLocations.AddNewLocation(ControlItem, I - Delta, FRow, False)
                else
                  Break;
            end;
            NewLocations.ApplyNewLocations;
            GridPanelLayout.RemoveEmptyAutoAddColumns;
          end else
          begin
            // Scan forward in column primary order, removing Delta from position of each
            // control item, unwrapping where nessesary, until the last control is reached
            // or a non "pushed" control is found.
            for I := GridPanelLayout.ColumnSpanIndex[FColumn, FRow] to GridPanelLayout.CellCount - 1 do
            begin
              GridPanelLayout.CellIndexToCell(I, LColumn, LRow);
              ControlItem := Collection.ControlItems[LColumn, LRow];
              if ControlItem <> nil then
              begin
                if ControlItem.Pushed then
                begin
                  if (ControlItem.Column = LColumn) and (ControlItem.Row = LRow) then
                  begin
                    GridPanelLayout.CellIndexToCell(I - Delta, LColumn, LRow);
                    if (LColumn > 0) and (LColumn + ControlItem.ColumnSpan > GridPanelLayout.ColumnCollection.Count) then
                    begin
                      Inc(Delta, (LColumn + ControlItem.ColumnSpan) - GridPanelLayout.ColumnCollection.Count);
                      GridPanelLayout.CellIndexToCell(I - Delta, LColumn, LRow);
                    end;
                    NewLocations.AddNewLocation(ControlItem, LColumn, LRow, False);
                  end;
                end else if ControlItem <> Self then
                  Break
                else
                  NewLocations.AddNewLocation(ControlItem, LColumn, LRow, False);
              end;
            end;
            NewLocations.ApplyNewLocations;
            GridPanelLayout.RemoveEmptyAutoAddRows;
          end;
        end else
        begin
          NumColumns := GridPanelLayout.ColumnCollection.Count;
          Delta := Value - FColumnSpan;
          // first check if there is room to the right to expand and if so remove those
          // columns from the Delta
          for I := Min(FColumn + FColumnSpan, NumColumns) to Min(FColumn + Value - 1, NumColumns - 1) do
            if Collection.Controls[I, FRow] = nil then
              Dec(Delta)
            else
              Break;
          MoveBy := Delta;
          // Now find out how many columns to add, if any
          for I := NumColumns - 1 downto NumColumns - MoveBy do
            if Collection.Controls[I, FRow] = nil then
              Dec(Delta)
            else
              Break;
          NumToAdd := Delta;
          // Add the columns
          if GridPanelLayout.ExpandStyle in [emAddColumns, emFixedSize] then
          begin
            if (GridPanelLayout.ExpandStyle = emFixedSize) and (NumToAdd > 0) then
              raise EGridLayoutException.Create(sCannotAddFixedSize);
            while NumToAdd > 0 do
            begin
              GridPanelLayout.AutoAddColumn;
              Dec(NumToAdd);
            end;
            NumColumns := GridPanelLayout.ColumnCollection.Count;
            for I := NumColumns - 1 downto NumColumns - Delta do
            begin
              ControlItem := Collection.ControlItems[I - MoveBy, FRow];
              if (ControlItem <> nil) and (ControlItem <> Self) then
                NewLocations.AddNewLocation(ControlItem, I, FRow, True);
            end;
            NewLocations.ApplyNewLocations;
          end else if (NumToAdd + MoveBy) > 0 then
          begin
            IndexDelta := Max(NumToAdd, Min(MoveBy, NumColumns));
            for I := GridPanelLayout.ColumnSpanIndex[FColumn, FRow] to GridPanelLayout.CellCount - 1 do
            begin
              GridPanelLayout.CellIndexToCell(I, LColumn, LRow);
              ControlItem := Collection.ControlItems[LColumn, LRow];
              if (ControlItem <> nil) and (ControlItem.Column = LColumn) and (ControlItem.Row = LRow) then
              begin
                if ControlItem = Self then
                begin
                  Span := Value;
                  LColumn := FColumn;
                  LRow := FRow;
                end else
                begin
                  Span := ControlItem.ColumnSpan;
                  GridPanelLayout.CellIndexToCell(I + IndexDelta, LColumn, LRow);
                end;
                if LColumn + Span > GridPanelLayout.ColumnCollection.Count then
                begin
                  if LColumn > 0 then
                  begin
                    Inc(IndexDelta, GridPanelLayout.ColumnCollection.Count - LColumn);
                    GridPanelLayout.CellIndexToCell(I + IndexDelta - NumToAdd, LColumn, LRow);
                  end else if ControlItem <> Self then
                  begin
                    Inc(IndexDelta, Min(Span, GridPanelLayout.ColumnCollection.Count));
                    GridPanelLayout.CellIndexToCell(I + IndexDelta, LColumn, LRow);
                  end else if (ControlItem = Self) and (LColumn = 0) then
                    Exit;
                end;
                NumToAdd := 0;
                NewLocations.AddNewLocation(ControlItem, LColumn, LRow, True);
              end;
            end;
            for I := 0 to NewLocations.Count - 1 do
              if NewLocations.NewLocations[I].NewRow > GridPanelLayout.RowCollection.Count - 1 then
                GridPanelLayout.AutoAddRow;
            NewLocations.ApplyNewLocations;
          end;
          FColumnSpan := Value;
        end;
        Changed(False);
      finally
        NewLocations.Free;
      end;
    finally
      GridPanelLayout.EndUpdate;
    end;
  end;
end;

procedure TGridPanelLayout.TControlItem.InternalSetLocation(AColumn, ARow: Integer; APushed, MoveExisting: Boolean);
var
  Collection: TControlCollection;
  CurrentItem: TControlItem;
begin
  if (AColumn <> FColumn) or (ARow <> FRow) then
  begin
    if MoveExisting then
    begin
      Collection := TControlCollection(GetOwner);
      if Collection <> nil then
        CurrentItem := Collection.ControlItems[AColumn, ARow]
      else
        CurrentItem := nil;
      if CurrentItem <> nil then
        CurrentItem.InternalSetLocation(FColumn, FRow, False, False);
    end;
    FColumn := AColumn;
    FRow := ARow;
    if APushed then
      Inc(FPushed)
    else if FPushed > 0 then
      Dec(FPushed);
    Changed(False);
  end;
end;

procedure TGridPanelLayout.TControlItem.SetLocation(AColumn, ARow: Integer; APushed: Boolean);
begin
  InternalSetLocation(AColumn, ARow, APushed, True);
end;

function TGridPanelLayout.TControlItem.GetPushed: Boolean;
begin
  Result := FPushed > 0;
end;

procedure TGridPanelLayout.DoAddObject(const AObject: TFmxObject);
begin
  inherited;
  if AObject is TControl
    and (not (csLoading in ComponentState))
      {$IFDEF MSWINDOWS}
      and not ((csDesigning in ComponentState)
        and Supports(AObject, IDesignerControl))
      {$ENDIF}
    then
    begin
      TControl(AObject).Anchors := [];
      FControlCollection.AddControl(TControl(AObject));
    end;
  Realign;
end;

procedure TGridPanelLayout.DoPaint;
var
  I: Integer;
  Size: Single;
  LinePos: Single;
begin
  inherited;
  if csDesigning in ComponentState then
    if Canvas.BeginScene then
      try
        Canvas.Fill.Kind := TBrushKind.bkNone;
        Canvas.Stroke.Kind := TBrushKind.bkSolid;
        Canvas.StrokeDash := TStrokeDash.sdDash;
        Canvas.Stroke.Color := TAlphaColorRec.DarkGrey;
        LinePos := 0;
        for I := 0 to FColumnCollection.Count - 2 do
        begin
          Size := FColumnCollection[I].Size;
          Canvas.DrawLine(TPointF.Create(LinePos + Size, BoundsRect.Top),
                          TPointF.Create(LinePos + Size, BoundsRect.Bottom), 1.0);
          LinePos := LinePos + Size;
        end;
        LinePos := 0;
        for I := 0 to FRowCollection.Count - 2 do
        begin
          Size := FRowCollection[I].Size;
          Canvas.DrawLine(TPointF.Create(BoundsRect.Left, LinePos + Size),
            TPointF.Create(BoundsRect.Right, LinePos + Size), 1.0);
          LinePos := LinePos + Size;
        end;
      finally
        Canvas.EndScene;
      end;
end;

procedure TGridPanelLayout.DoRemoveObject(const AObject: TFmxObject);
begin
  if AObject is TControl then
    FControlCollection.RemoveControl(TControl(AObject));
  inherited;
  Realign;
end;

function TGridPanelLayout.GetCellCount: Integer;
begin
  Result := FRowCollection.Count * FColumnCollection.Count;
end;

function TGridPanelLayout.GetCellRect(AColumn, ARow: Integer): TRectF;
var
  I: Integer;
begin
  Result.Left := 0;
  Result.Top := 0;
  for I := 0 to AColumn - 1 do
    Result.Left := Result.Left + FColumnCollection[I].Size;
  for I := 0 to ARow - 1 do
    Result.Top := Result.Top + FRowCollection[I].Size;
  Result.BottomRight := CellSize[AColumn, ARow];

  Result.Bottom := Result.Bottom + Result.Top;
  Result.Right := Result.Right + Result.Left;
end;

function TGridPanelLayout.GetCellSizes(AColumn, ARow: Integer): TPointF;
begin
  Result := TPointF.Create(FColumnCollection[AColumn].Size, FRowCollection[ARow].Size);
end;

function TGridPanelLayout.GetColumnSpanIndex(AColumn, ARow: Integer): Integer;
begin
  Result := AColumn + (ARow * FColumnCollection.Count);
end;

function TGridPanelLayout.GetRowSpanIndex(AColumn, ARow: Integer): Integer;
begin
  Result := ARow + (AColumn * FRowCollection.Count);
end;

function TGridPanelLayout.IsColumnEmpty(AColumn: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  if (AColumn > -1) and (AColumn < FColumnCollection.Count) then
  begin
    for I := 0 to FRowCollection.Count -1 do
      if ControlCollection.Controls[AColumn, I] <> nil then
        Exit;
    Result := True;
  end else
    raise EGridLayoutException.CreateFmt(SInvalidColumnIndex, [AColumn]);
end;

function TGridPanelLayout.IsRowEmpty(ARow: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  if (ARow > -1) and (ARow < FRowCollection.Count) then
  begin
    for I := 0 to FColumnCollection.Count -1 do
      if ControlCollection.Controls[I, ARow] <> nil then
        Exit;
    Result := True;
  end else
    raise EGridLayoutException.CreateFmt(SInvalidRowIndex, [ARow]);
end;

procedure TGridPanelLayout.RemoveEmptyAutoAddColumns;
var
  I: Integer;
begin
  for I := FColumnCollection.Count - 1 downto 0 do
    if FColumnCollection[I].AutoAdded and IsColumnEmpty(I) then
      FColumnCollection.Delete(I)
    else
      Exit;
end;

procedure TGridPanelLayout.RemoveEmptyAutoAddRows;
var
  I: Integer;
begin
  for I := FRowCollection.Count - 1 downto 0 do
    if FRowCollection[I].AutoAdded and IsRowEmpty(I) then
      FRowCollection.Delete(I)
    else
      Exit;
end;

procedure TGridPanelLayout.Resize;
begin
  inherited;
  FRecalcCellSizes := True;
end;

function TGridPanelLayout.AutoAddColumn: TColumnItem;
begin
  Result := FColumnCollection.Add;
  Result.SizeStyle := ssAuto;
  Result.AutoAdded := True;
end;

function TGridPanelLayout.AutoAddRow: TRowItem;
begin
  Result := FRowCollection.Add;
  Result.SizeStyle := ssAuto;
  Result.AutoAdded := True;
end;

procedure TGridPanelLayout.CellIndexToCell(AIndex: Integer; var AColumn,
  ARow: Integer);
begin
  if FExpandStyle in [emAddColumns, emFixedSize] then
  begin
    AColumn := AIndex div FRowCollection.Count;
    ARow := AIndex mod FRowCollection.Count;
  end else
  begin
    ARow := AIndex div FColumnCollection.Count;
    AColumn := AIndex mod FColumnCollection.Count;
  end;
end;

function TGridPanelLayout.CellToCellIndex(AColumn, ARow: Integer): Integer;
begin
  if FExpandStyle in [emAddColumns, emFixedSize] then
    Result := ColumnSpanIndex[AColumn, ARow]
  else
    Result := RowSpanIndex[AColumn, ARow];
end;

constructor TGridPanelLayout.Create(AOwner: TComponent);
begin
  inherited;
  FRowCollection := TRowCollection.Create(Self);
  FColumnCollection := TColumnCollection.Create(Self);
  FControlCollection := TControlCollection.Create(Self);
  FRowCollection.Add;
  FRowCollection.Add;
  FColumnCollection.Add;
  FColumnCollection.Add;
end;

procedure TGridPanelLayout.RecalcCellDimensions(const Rect: TRectF);
var
  I, J: Integer;
  LSize, XSize, YSize, RemainingX, RemainingY: Single;
  MaxSize, PercentXCount, PercentYCount: Single;
  PercentX, PercentY: Double;
  ControlItem: TControlItem;
  Control: TControl;
begin
  // Subtract the size of absolute size columns and rows and also calculate the total size of
  // the percentage rows and columns
  // Also subtract the width of the widest control in each autosize column
  // or the tallest control in each autosize row and set the column or row size to that value
  XSize := Rect.Right - Rect.Left;
  YSize := Rect.Bottom - Rect.Top;
  PercentX := 0.0;
  PercentY := 0.0;

  PercentXCount := 0;
  for I := 0 to FColumnCollection.Count - 1 do
    if FColumnCollection[I].SizeStyle = TSizeStyle.ssAbsolute then
      XSize := XSize - FColumnCollection[I].Value
    else if FColumnCollection[I].SizeStyle = TSizeStyle.ssPercent then
    begin
      PercentX := PercentX + FColumnCollection[I].Value;
      PercentXCount := PercentXCount + 1;
    end
    else
    begin
      MaxSize := 0;
      for J := 0 to FRowCollection.Count - 1 do
      begin
        ControlItem := FControlCollection.ControlItems[I, J];
        if (ControlItem <> nil) and (ControlItem.Control <> nil) and
           (ControlItem.Column = I) and (ControlItem.Row = J) then
        begin
          Control := ControlItem.Control;
          LSize := Control.Width + Control.Margins.Left + Control.Margins.Right + Self.Padding.Left + Self.Padding.Right;
          if LSize > MaxSize then
            MaxSize := LSize;
        end;
      end;
      XSize := XSize - MaxSize;
      FColumnCollection[I].Size := MaxSize;
    end;

  PercentYCount := 0;
  for I := 0 to FRowCollection.Count - 1 do
    if FRowCollection[I].SizeStyle = TSizeStyle.ssAbsolute then
      YSize := YSize - FRowCollection[I].Value
    else if FRowCollection[I].SizeStyle = TSizeStyle.ssPercent then
    begin
      PercentY := PercentY + FRowCollection[I].Value;
      PercentYCount := PercentYCount + 1;
    end else
    begin
      MaxSize := 0;
      for J := 0 to FColumnCollection.Count - 1 do
      begin
        ControlItem := FControlCollection.ControlItems[J, I];
        if (ControlItem <> nil) and (ControlItem.Control <> nil) and
           (ControlItem.Column = J) and (ControlItem.Row = I) then
        begin
          Control := ControlItem.Control;
          LSize := Control.Height + Control.Margins.Left + Control.Margins.Right + Self.Padding.Top + Self.Padding.Bottom;
          if LSize > MaxSize then
            MaxSize := LSize;
        end;
      end;
      YSize := YSize - MaxSize;
      FRowCollection[I].Size := MaxSize;
    end;

  // Finally Calculate the size of the percentage-based columns and rows based on the remaining
  // X and Y sizes
  RemainingX := XSize;
  RemainingY := YSize;
  for I := 0 to FColumnCollection.Count - 1 do
    if FColumnCollection[I].SizeStyle = TSizeStyle.ssPercent then
    begin
      if IsZero(PercentX) then
        FColumnCollection[I].Value := 100.0 / PercentXCount
      else
        FColumnCollection[I].Value := (FColumnCollection[I].Value / PercentX) * 100.0;
      FColumnCollection[I].Size := XSize * (FColumnCollection[I].Value / 100.0);
      RemainingX := RemainingX - FColumnCollection[I].Size;
      if (RemainingX > 0) and (I = FColumnCollection.Count - 1) then
        FColumnCollection[I].Size := FColumnCollection[I].Size + RemainingX;
    end;

  for I := 0 to FRowCollection.Count - 1 do
    if FRowCollection[I].SizeStyle = TSizeStyle.ssPercent then
    begin
      if IsZero(PercentY) then
        FRowCollection[I].Value := 100.0 / PercentYCount
      else
        FRowCollection[I].Value := (FRowCollection[I].Value / PercentY) * 100.0;
      FRowCollection[I].Size := YSize * (FRowCollection[I].Value / 100.0);
      RemainingY := RemainingY - FRowCollection[I].Size;
      if (RemainingY > 0) and (I = FRowCollection.Count - 1) then
        FRowCollection[I].Size := FRowCollection[I].Size + RemainingY;
    end;
  FRecalcCellSizes := False;
end;


procedure TGridPanelLayout.DoRealign;

  procedure ArrangeControlInCell(AControl: TControl; CellRect: TRectF);
  var
    NewBounds: TRectF;
    AnchorSubset: TAnchors;
  begin
    if AControl.Align <> TAlignLayout.alNone then
      ArrangeControl(AControl, AControl.Align,
        CellRect.Width, CellRect.Height,
        CellRect.Width, CellRect.Height,
        CellRect)
    else
    begin
      AnchorSubset := AControl.Anchors * [TAnchorKind.akLeft, TAnchorKind.akRight];
      if AnchorSubset = [TAnchorKind.akLeft] then
        NewBounds.Left := CellRect.Left
      else if AnchorSubset = [TAnchorKind.akRight] then
        NewBounds.Left := Max(CellRect.Left, CellRect.Right - AControl.Width) (* was: Margins.ExplicitWidth *)
      else
        NewBounds.Left := Max(CellRect.Left, CellRect.Left + ((CellRect.Right - CellRect.Left) - AControl.Width (* was: Margins.ControlWidth *)) / 2);
      NewBounds.Right := NewBounds.Left + Min(CellRect.Right - CellRect.Left, AControl.Width); (* was: Margins.ExplicitWidth *)
      AnchorSubset := AControl.Anchors * [TAnchorKind.akTop, TAnchorKind.akBottom];
      if AnchorSubset = [TAnchorKind.akTop] then
        NewBounds.Top := CellRect.Top
      else if AnchorSubset = [TAnchorKind.akBottom] then
        NewBounds.Top := Max(CellRect.Top, CellRect.Bottom - AControl.Height) (* was: Margins.ExplicitHeight *)
      else
        NewBounds.Top := Max(CellRect.Top, CellRect.Top + ((CellRect.Bottom - CellRect.Top) - AControl.Height) / 2); (* was: Margins.ControlHeight *)
      NewBounds.Bottom := NewBounds.Top + Min(CellRect.Bottom - CellRect.Top, AControl.Height); (* was: Margins.ExplicitHeight *)
      AControl.BoundsRect := NewBounds;
    end;
  end;

  procedure AdjustCellRect(var Rect: TRectF);
  begin
    Rect.Left := Rect.Left + Padding.Left;
    Rect.Top := Rect.Top + Padding.Top;
    Rect.Right := Rect.Right - Padding.Right;
    Rect.Bottom := Rect.Bottom - Padding.Bottom;
  end;

  procedure ArrangeControls;
  var
    I, J, K: Integer;
    CellRect: TRectF;
    SpanRect: TRectF;
    ControlItem: TControlItem;
  begin
    CellRect.Top := BoundsRect.Top;
    for J := 0 to FRowCollection.Count - 1 do
    begin
      CellRect.Left := BoundsRect.Left;
      CellRect.Bottom := CellRect.Top + FRowCollection[J].Size;
      for I := 0 to FColumnCollection.Count - 1 do
      begin
        ControlItem := FControlCollection.ControlItems[I, J];
        CellRect.Right := CellRect.Left + FColumnCollection[I].Size;
        if (ControlItem <> nil) and (ControlItem.Control <> nil) and
           (ControlItem.Column = I) and (ControlItem.Row = J) then
        begin
          SpanRect := CellRect;
          if ControlItem.ColumnSpan > 1 then
            for K := I + 1 to Min(I + ControlItem.ColumnSpan - 1, FColumnCollection.Count - 1) do
              SpanRect.Right := SpanRect.Right + FColumnCollection[K].Size;
          if ControlItem.RowSpan > 1 then
            for K := J + 1 to Min(J + ControlItem.RowSpan - 1, FRowCollection.Count - 1 ) do
              SpanRect.Bottom := SpanRect.Bottom + FRowCollection[K].Size;
          AdjustCellRect(SpanRect);
          ArrangeControlInCell(ControlItem.Control, SpanRect);
        end;
        CellRect.Left := CellRect.Right;
      end;
      CellRect.Top := CellRect.Bottom;
    end;
  end;

begin
  if not FNeedAlign then
    Exit;

  FDisableAlign := True;
  if FRecalcCellSizes then
    RecalcCellDimensions(BoundsRect);
  if ControlsCount > 0 then
    ArrangeControls;
  FDisableAlign := False;
end;


procedure TGridPanelLayout.SetColumnCollection(const Value: TColumnCollection);
begin
  FColumnCollection.Assign(Value);
end;

procedure TGridPanelLayout.SetControlCollection(const Value: TControlCollection);
begin
  FControlCollection.Assign(Value);
end;

procedure TGridPanelLayout.SetRowCollection(const Value: TRowCollection);
begin
  FRowCollection.Assign(Value);
end;

procedure TGridPanelLayout.UpdateControlsColumn(AColumn: Integer);
var
  I, J: Integer;
  AControlItem: TControlItem;
begin
  for I := AColumn + 1 to FColumnCollection.Count - 1 do
    for J := 0 to FRowCollection.Count - 1 do
    begin
      AControlItem := FControlCollection.ControlItems[I, J];
      if (AControlItem <> nil) and (AControlItem.Column = I) and
         (AControlItem.Row = J) then
        AControlItem.SetColumn(AControlItem.Column - 1);
    end;
end;

procedure TGridPanelLayout.UpdateControlsRow(ARow: Integer);
var
  I, J: Integer;
  AControlItem: TControlItem;
begin
  for I := 0 to FColumnCollection.Count - 1 do
    for J := ARow + 1 to FRowCollection.Count - 1 do
    begin
      AControlItem := FControlCollection.ControlItems[I, J];
      if (AControlItem <> nil) and (AControlItem.Column = I) and
         (AControlItem.Row = J) then
        AControlItem.SetRow(AControlItem.Row - 1);
    end;
end;



initialization
  RegisterFmxClasses([TLayout, TScaledLayout, TGridLayout, TGridPanelLayout,
    TFlowLayout, TFlowLayoutBreak, TScrollBox, TVertScrollBox, THorzScrollBox,
    TFramedScrollBox, TFramedVertScrollBox]);
end.
