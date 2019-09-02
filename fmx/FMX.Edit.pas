{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit FMX.Edit;

interface

uses
  System.Classes, System.Actions, System.Types, System.UITypes, System.Rtti,
  FMX.ActnList, FMX.Platform, FMX.Types, FMX.Objects, FMX.StdCtrls, FMX.StdActns,
  FMX.Controls, FMX.ListBox, FMX.Menus, FMX.Pickers, FMX.Text, FMX.MagnifierGlass,
  FMX.TextLayout, FMX.Graphics, FMX.SpellChecker, System.Generics.Collections;

{$SCOPEDENUMS ON}

type

{ TCustomEdit }

  TContentEdit = class(TContent)
  protected
    procedure DoRemoveObject(const AObject: TFmxObject); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TSelectionMode = (smNone, smTextSelection, smCursorPosChanging);
  TSelectionPointType = (sptLeft, sptRight);

  TCustomEdit = class (TStyledControl, ITextInput, ITextActions, IVirtualKeyboardControl,
    IItemsContainer, ITextSettings, ICaret, ITextSpellCheck, ITextSpellCheckActions)
  private
    FTextService: TTextService;
    FOnChange: TNotifyEvent;
    FReadOnly: Boolean;
    FSelStart: Integer;
    FSelLength: Integer;
    FMaxLength: Integer;
    FFirstVisibleChar: Integer;
    FSelectionMode: TSelectionMode;
    FPassword: Boolean;
    FTyping: Boolean;
    FOnTyping: TNotifyEvent;
    FSelectionFill: TBrush;
    FCursorFill: TBrush;
    FOnChangeTracking: TNotifyEvent;
    FKeyboardType : TVirtualkeyboardType;
    FReturnKeyType: TReturnKeyType;
    FImeMode: TImeMode;
    FTextSettingsInfo: TTextSettingsInfo;
    FContent: TControl;
    FButtonsContent: TContentEdit;
    FLeftLayout: TControl;
    FButtonsLayout: TControl;
    FPrompt: TTextControl;
    FTextHeight: Single;
    FLineHeight: Single;
    FLineTop: Single;
    FInputSupport: Boolean;
    FKillFocusByReturn: Boolean;
    FTextLayout: TTextLayout;
    FCharsBuffer: string;
    { Selection }
    FLeftSelPt: TSelectionPoint;
    FRightSelPt: TSelectionPoint;
    { Loupe }
    FLoupeService: ILoupeService;
    { Caret }
    FCaret: TCaret;
    { Spelling }
    FCheckSpelling: Boolean;
    FSpellService: IFMXSpellCheckerService;
    FUpdateSpelling: Boolean;
    FSpellingRegions: TRegion;
    FSpellMenuItems: TList<TMenuItem>;
    FSpellHightlightRect: TRectF;
    FSpellFill: TBrush;
    FSpellUnderlineBrush: TStrokeBrush;
    FTextLayoutUpdated: Boolean;
    procedure UpdateSpelling;
    procedure InsertText(const AText: string);
    function GetSelLength: Integer;
    function GetSelStart: Integer;
    function GetSelText: string;
    procedure SetSelLength(const Value: Integer);
    procedure SetSelStart(const Value: Integer);
    function GetSelRect: TRectF;
    procedure SetCaretPosition(const Value: Integer);
    function GetCaretPosition: Integer;
    function GetOriginCaretPosition: Integer;
    function GetCoordinatePosition(x: Single): Integer;
    procedure SetMaxLength(const Value: Integer);
    procedure UpdateFirstVisibleChar;
    procedure SetPassword(const Value: Boolean);
    { Standart Text Actions: Cut, Copy, Paste, Delete, Select All }
    procedure DoCopy(Sender: TObject);
    procedure DoCut(Sender: TObject);
    procedure DoDelete(Sender: TObject);
    procedure DoPaste(Sender: TObject);
    procedure DoSelectAll(Sender: TObject);
    procedure SetFont(const Value: TFont);
    procedure SetFontColor(const Value: TAlphaColor);
    procedure SetTextAlign(const Value: TTextAlign);
    procedure ReadFontFillColor(Reader: TReader);
    procedure ReadFontFillKind(Reader: TReader);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(AReadOnly: Boolean);
    { ITextSettings }
    function GetDefaultTextSettings: TTextSettings;
    function GetTextSettings: TTextSettings;
    function GetStyledSettings: TStyledSettings;
    function GetTextAlign: TTextAlign;
    function GetVertTextAlign: TTextAlign;
    procedure SetVertTextAlign(const Value: TTextAlign);
    function GetFont: TFont;
    function GetFontColor: TAlphaColor;
    { Selections }
    procedure BeginSelection;
    procedure EndSelection;
    function HaveSelectionPickers: Boolean;
    procedure UpdateSelectionPointPositions;
    procedure DoSelPtMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure DoLeftSelPtChangePosition(Sender: TObject; var X, Y: Single);
    procedure DoLeftSelPtMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure DoRightSelPtChangePosition(Sender: TObject; var X, Y: Single);
    procedure DoRightSelPtMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    { Loupe }
    procedure HideLoupe;
    procedure ShowLoupe;
    procedure SetLoupePosition(const ASelectionPointType: TSelectionPointType); overload;
    procedure SetLoupePosition(const X, Y: Single); overload;
    { ICaret }
    function ICaret.GetObject = GetCaret;
    function GetCaret: TCustomCaret;
    procedure ShowCaret;
    procedure HideCaret;
    procedure SetCaret(const Value: TCaret);
    procedure SetSelectionMode(const AValue: TSelectionMode);
    procedure UpdateTextHeight;
    { Prompt }
    procedure SetPromptText(const Prompt: string);
    function GetPromptText: string;
    function GetLastTextSettings: TTextSettings;
    { ITextSpellCheck }
    function IsSpellCheckEnabled: Boolean;
    function IsCurrentWordWrong: Boolean;
    function GetListOfPrepositions: TArray<string>;
    procedure HighlightSpell;
    procedure HideHighlightSpell;
    { ITextSpellCheckActions }
    procedure Spell(const AWord: string);
    { Spelling }
    procedure SetCheckSpelling(const Value: Boolean);
    procedure UpdateSpellPopupMenu(const APoint: TPointF);
    procedure SpellFixContextMenuHandler(Sender: TObject);
    procedure UpdateTextLayout;
  protected
    FNeedChange: Boolean;
    FFilterChar: string;
    FLastKey: Word;
    FLastChar: System.WideChar;
    FClipboardSvc: IFMXClipboardService;
    FSaveReadOnly: Boolean;
    FEditPopupMenu: TPopupMenu;
    { Context menu }
    procedure CreatePopupMenu;
    procedure UpdatePopupMenuItems; virtual;
    function FindContextMenuItem(const AItemName: string): TMenuItem;
    function GetData: TValue; override;
    procedure SetData(const Value: TValue); override;
    function CanObserve(const ID: Integer): Boolean; override;
    procedure ObserverAdded(const ID: Integer; const Observer: IObserver); override;
    procedure ObserverToggle(const AObserver: IObserver; const Value: Boolean);
    procedure RepaintEdit;
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
    procedure Loaded; override;
    procedure Change; virtual;
    procedure DoChangeTracking; virtual;
    function GetPasswordCharWidth: Single;
    function TextWidth(const AStart, ALength: Integer): Single;
    function GetText: string; virtual;
    procedure SetText(const Value: string); virtual;
    procedure SetTextInternal(const Value: string);
    procedure DoContentPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    { Base Mouse and Keyboard Events }
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    { ITouchEvents }
    procedure LongTap(const X, Y: Single);
    procedure DblTap;
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoInsertObject(Index: Integer; const AObject: TFmxObject); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DoRealign; override;
    function GetImeMode: TImeMode; virtual;
    procedure SetImeMode(const Value: TImeMode); virtual;
    procedure DoTyping; virtual;
    function CreateCaret: TCaret; virtual;
    procedure SetInputSupport(const Value: Boolean); virtual;
    procedure DefineProperties(Filer: TFiler); override;
    function GetDefaultSize: TSizeF; override;
    { ITextSettings }
    procedure SetTextSettings(const Value: TTextSettings); virtual;
    procedure SetStyledSettings(const Value: TStyledSettings); virtual;
    procedure DoChanged; virtual;
    function StyledSettingsStored: Boolean; virtual;
    { ITextInput }
    function GetTextService: TTextService;
    procedure UpdateCaretPoint;
    function GetTargetClausePointF: TPointF;
    procedure StartIMEInput;
    procedure EndIMEInput;
    function ITextInput.GetSelection = GetSelText;
    function ITextInput.GetSelectionRect = GetSelRect;
    function GetSelectionBounds: TRect;
    function HasText: Boolean;
    procedure Replace(const AStartPos: Integer; const ALength: Integer; const AStr: string);
    { IVirtualKeyboardControl }
    procedure SetKeyboardType(Value: TVirtualKeyboardType);
    function GetKeyboardType: TVirtualKeyboardType;
    procedure SetReturnKeyType(Value: TReturnKeyType);
    function GetReturnKeyType: TReturnKeyType;
    property InputSupport: Boolean read FInputSupport write SetInputSupport;
    { IItemsContainer }
    function GetItemsCount: Integer;
    function GetItem(const AIndex: Integer): TFmxObject;
    procedure RealignContent; virtual;
    procedure RealignButtonsContainer; virtual;
    procedure UpdateLayoutSize;
    property Content: TControl read FContent;
    property ButtonsContent: TContentEdit read FButtonsContent;
    property ButtonsLayout: TControl read FButtonsLayout;
    property TextHeight: Single read FTextHeight;
    property LineHeight: Single read FLineHeight;
    property LineTop: Single read FLineTop;
    property SelectionMode: TSelectionMode read FSelectionMode write SetSelectionMode;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ContextMenu(const ScreenPosition: TPointF); override;
    procedure ClearSelection; deprecated 'Use DeleteSelection method instead';
    procedure CMGesture(var EventInfo: TGestureEventInfo); override;
    { ITextActions }
    procedure DeleteSelection;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure PasteFromClipboard;
    procedure SelectAll;
    procedure SelectWord;
    procedure ResetSelection;
    procedure GoToTextEnd;
    procedure GoToTextBegin;
    function HasSelection: Boolean;
    function GetCharX(a: Integer): Single;
    function ContentRect: TRectF;
    property CaretPosition: Integer read GetCaretPosition write SetCaretPosition;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property SelText: string read GetSelText;
    property MaxLength: Integer read FMaxLength write SetMaxLength default 0;
    property SelectionFill: TBrush read FSelectionFill;
    property Password: Boolean read FPassword write SetPassword default False;
    property Text: string read GetText write SetText;
    property FilterChar: string read FFilterChar write FFilterChar;
    property ImeMode: TImeMode read GetImeMode write SetImeMode default TImeMode.imDontCare;
    property Typing: Boolean read FTyping write FTyping default False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChangeTracking: TNotifyEvent read FOnChangeTracking write FOnChangeTracking;
    property OnTyping: TNotifyEvent read FOnTyping write FOnTyping;
    property Font: TFont read GetFont write SetFont;
    property FontColor: TAlphaColor read GetFontColor write SetFontColor default TAlphaColorRec.Black;
    property TextAlign: TTextAlign read GetTextAlign write SetTextAlign default TTextAlign.taLeading;
    property VertTextAlign: TTextAlign read GetVertTextAlign write SetVertTextAlign default TTextAlign.taCenter;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property KeyboardType: TVirtualKeyboardType read GetKeyboardType write SetKeyboardType default TVirtualKeyboardType.vktDefault;
    property ReturnKeyType: TReturnKeyType read GetReturnKeyType write SetReturnKeyType default TReturnKeyType.rktDefault;
    property DefaultTextSettings: TTextSettings read GetDefaultTextSettings;
    property LastTextSettings: TTextSettings read GetLastTextSettings;
    property TextSettings: TTextSettings read GetTextSettings write SetTextSettings;
    property StyledSettings: TStyledSettings read GetStyledSettings write SetStyledSettings stored StyledSettingsStored nodefault;
    property Caret: TCaret read FCaret write SetCaret;
    property TextPrompt: string read GetPromptText write SetPromptText;
    property KillFocusByReturn: Boolean read FKillFocusByReturn write FKillFocusByReturn;
    {spelling}
    property CheckSpelling: Boolean read FCheckSpelling write SetCheckSpelling default False;
  published
    property Align;
    property Anchors;
    property StyleLookup;
    property TabOrder;
  end;

{ TEditButton }

  IEditControl = interface
    ['{4C7EE0A7-06EC-4515-B843-B608FB984468}']
    function BoundsRect: TRectF;
    function GetControl: TControl;
  end;

  TEditButton = class (TCustomButton, IEditControl)
  protected
    function GetDefaultStyleLookupName: string; override;
    procedure SetName(const NewName: TComponentName); override;
    function GetDefaultSize: TSizeF; override;
    { IEditControl }
    function IEditControl.BoundsRect = GetBoundsRect;
    function GetControl: TControl;
  public
    constructor Create(AOwner: TComponent); override;
    function GetEdit: TCustomEdit;
  published
    property Action;
    property Anchors;
    property AutoTranslate default True;
    property CanFocus default True;
    property CanParentFocus;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    property DesignVisible default True;
    property DisableFocusEffect;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Font;
    property Height;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property HitTest default True;
    property IsPressed default False;
    property Locked default False;
    property Padding;
    property ModalResult default mrNone;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RepeatClick default False;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property StaysPressed default False;
    property StyleLookup;
    property TabOrder;
    property Text;
    property TextAlign default TTextAlign.taCenter;
    property TouchTargetExpansion;
    property Visible default True;
    property Width;
    property WordWrap default False;

    {events}
    property OnApplyStyleLookup;
    {Drag and Drop events}
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    {Keyboard events}
    property OnKeyDown;
    property OnKeyUp;
    {Mouse events}
    property OnCanFocus;
    property OnClick;
    property OnDblClick;

    property OnEnter;
    property OnExit;
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

{ TClearEditButton }

  TClearEditButton = class (TEditButton)
  protected
    procedure Click; override;
    function GetDefaultStyleLookupName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TPasswordEditButton }

  TPasswordEditButton = class (TEditButton)
  protected
    function GetDefaultStyleLookupName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Single;
      Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Single;
      Y: Single); override;
  end;

{ TSearchEditButton }

  TSearchEditButton = class (TEditButton)
  protected
    function GetDefaultStyleLookupName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TEllipsesEditButton }

  TEllipsesEditButton = class (TEditButton)
  protected
    function GetDefaultStyleLookupName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TDropDownEditButton }

  TDropDownEditButton = class (TEditButton)
  protected
    function GetDefaultStyleLookupName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TSpinEditButton }

  TSpinEditButton = class (TStyledControl, IEditControl)
  strict private
    FUpButton: TCustomButton;
    FDownButton: TCustomButton;
    { Events }
    FOnUpClick: TNotifyEvent;
    FOnDownClick: TNotifyEvent;
  protected
    { Style }
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
    function GetDefaultStyleLookupName: string; override;
    { Events }
    procedure DoUpButtonClick(Sender: TObject);
    procedure DoDownButtonClick(Sender: TObject);
    { IEditControl }
    function IEditControl.BoundsRect = GetBoundsRect;
    function GetControl: TControl;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Anchors;
    property AutoTranslate default True;
    property CanFocus default True;
    property CanParentFocus;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    property DesignVisible default True;
    property DisableFocusEffect;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Height;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property HitTest default True;
    property Locked default False;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property StyleLookup;
    property TabOrder;
    property TouchTargetExpansion;
    property Visible default True;
    property Width;

    {events}
    property OnApplyStyleLookup;
    {Drag and Drop events}
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    {Keyboard events}
    property OnKeyDown;
    property OnKeyUp;
    {Mouse events}
    property OnDownClick: TNotifyEvent read FOnDownClick write FOnDownClick;
    property OnUpClick: TNotifyEvent read FOnUpClick write FOnUpClick;
    property OnCanFocus;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
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

{ TEdit }

  TEdit = class(TCustomEdit)
  public
    property Action;
  published
    property Cursor default crIBeam;
    property CanFocus default True;
    property CanParentFocus;
    property DisableFocusEffect;
    property Font;
    property FontColor;
    property KeyboardType;
    property ReturnKeyType;
    property Password;
    property ReadOnly;
    property Text;
    property TextAlign;
    property VertTextAlign;
    property ImeMode;
    property Position;
    property Width;
    property Height;
    property ClipChildren default False;
    property ClipParent default False;
    property DesignVisible default True;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property HitTest default True;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property TextPrompt;
    property StyleLookup;
    property StyledSettings;
    property TouchTargetExpansion;
    property Visible default True;
    property Caret;
    property KillFocusByReturn;
    property CheckSpelling;
    { events }
    property OnChange;
    property OnChangeTracking;
    property OnTyping;
    property OnApplyStyleLookup;
    { Drag and Drop events }
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    { Keyboard events }
    property OnKeyDown;
    property OnKeyUp;
    { Mouse events }
    property OnCanFocus;
    property OnClick;
    property OnDblClick;

    property OnEnter;
    property OnExit;
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

{ TCustomEditBox }

  TNumValueType = (vtInteger, vtFloat);

  TCustomEditBox = class(TCustomEdit, IValueRange)
  private
    FValueRange: TValueRange;
    FTextUpdating: Boolean;
    FValueType: TNumValueType;
    FDecimalDigits: Integer;
    FHorzIncrement: Single;
    FDefaultValueRange: TBaseValueRange;
    procedure SetMax(const AValue: Single);
    procedure SetMin(const AValue: Single);
    procedure SetDecimalDigits(const ADecimalDigits: Integer);
    procedure SetValueType(const AValueType: TNumValueType);
    function GetValueRange: TCustomValueRange;
    procedure SetValueRange(const AValue: TCustomValueRange);
    function GetMax: Single;
    function GetMin: Single;
    function GetValue: Single;
    procedure SetValueRange_(const Value: TValueRange);
    function CurrInc: single;
    function DefStored: Boolean;
    function MaxStored: Boolean;
    function MinStored: Boolean;
    function ValueStored: Boolean;
  protected
    function HorzIncrementStored: Boolean; virtual;
    procedure Loaded; override;
    function ConvertTextToValue(const AValue: string): Single;
    function ConvertValueToText(const AValue: Single): string;
    procedure SetText(const AText: string); override;
    procedure SetValue(const AValue: Single); virtual;
    procedure DoExit; override;
    procedure Change; override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    procedure KeyDown(var Key: Word; var KeyChar: Char; Shift: TShiftState); override;
    function GetData: TValue; override;
    procedure SetData(const Value: TValue); override;
    function GetActionLinkClass: TActionLinkClass; override;
    procedure ActionChange(Sender: TBasicAction; CheckDefaults: Boolean); override;
    property DefaultValueRange: TBaseValueRange read FDefaultValueRange;
    procedure ValueInc;
    procedure ValueDec;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterConstruction; override;
    destructor Destroy; override;
    property HorzIncrement: Single read FHorzIncrement write FHorzIncrement stored HorzIncrementStored nodefault;
    property ValueRange: TValueRange read FValueRange write SetValueRange_ stored ValueStored;
  published
    property Cursor default crIBeam;
    property DecimalDigits: Integer read FDecimalDigits write SetDecimalDigits default 2;
    property Font;
    property FontColor;
    property Min: Single read GetMin write SetMin stored MinStored nodefault;
    property Max: Single read GetMax write SetMax stored MaxStored nodefault;
    property Value: Single read GetValue write SetValue stored ValueStored nodefault;
    property ValueType: TNumValueType read FValueType write SetValueType default TNumValueType.vtInteger;
    property Text stored False;
  end;

{ TNumberBox }

  TNumberBox = class(TCustomEditBox)
  private
    FPressed: Boolean;
    FPressedPos: TPointF;
    FPressedVert: Boolean;
    FVertIncrement: Single;
  protected
    procedure SetText(const AText: string); override;
    procedure SetValue(const AValue: Single); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure PaintChildren; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Action;
  published
    property CanFocus default True;
    property CanParentFocus;
    property Cursor default crDefault;
    property DisableFocusEffect;
    property ReadOnly;
    property KeyboardType default TVirtualKeyboardType.vktNumberPad;
    property TextAlign;
    property Position;
    property Width;
    property Height;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property HorzIncrement;
    property VertIncrement: Single read FVertIncrement write FVertIncrement;
    property ClipChildren default False;
    property ClipParent default False;
    property DesignVisible default True;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property HitTest default True;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property StyleLookup;
    property TouchTargetExpansion;
    property Visible default True;
    { events }
    property OnChange;
    property OnChangeTracking;
    property OnTyping;
    property OnApplyStyleLookup;
    { Drag and Drop events }
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    { Keyboard events }
    property OnKeyDown;
    property OnKeyUp;
    { Mouse events }
    property OnCanFocus;
    property OnClick;
    property OnDblClick;

    property OnEnter;
    property OnExit;
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

{ TSpinBox }

  TSpinBox = class(TCustomEditBox)
  private
    FMinus: TCustomButton;
    FPlus: TCustomButton;
    function GetIncrement: Single; inline;
    procedure SetIncrement(const Value: Single); inline;
  protected
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
    procedure DoMinusClick(Sender: TObject);
    procedure DoPlusClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    property Action;
  published
    property CanFocus default True;
    property CanParentFocus;
    property Cursor default crDefault;
    property DisableFocusEffect;
    property Increment: Single read GetIncrement write SetIncrement stored HorzIncrementStored nodefault;
    property ReadOnly;
    property KeyboardType default TVirtualKeyboardType.vktNumberPad;
    property Position;
    property Width;
    property Height;
    property TextAlign default TTextAlign.taCenter;
    property ClipChildren default False;
    property ClipParent default False;
    property DesignVisible default True;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property HitTest default True;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property StyleLookup;
    property StyledSettings;
    property TouchTargetExpansion;
    property Visible default True;
    { events }
    property OnChange;
    property OnChangeTracking;
    property OnTyping;
    property OnApplyStyleLookup;
    { Drag and Drop events }
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    { Keyboard events }
    property OnKeyDown;
    property OnKeyUp;
    { Mouse events }
    property OnCanFocus;
    property OnClick;
    property OnDblClick;

    property OnEnter;
    property OnExit;
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

{ TComboEditListBox }

  TComboEdit = class;

  TComboEditListBox = class(TCustomListBox)
  strict private
    procedure HandleStringsChanged(const S: String; const Op: TCustomListBox.TStringsChangeOp);
  protected
    FComboEdit: TComboEdit;
    function GetObservers: TObservers; override;
    function GetDefaultStyleLookupName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
  end;

  // Base class for edit with the dropdown list.
  // Provides action on a call popup
  TComboEditBase = class(TCustomEdit)
  private
    FArrowButton: TControl;
  protected
    property ArrowButton: TControl read FArrowButton;
    procedure ApplyStyle; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoComboMouseDown(Sender:TObject; Button: TMouseButton; Shift: TShiftState; x, Y: Single); virtual;
    procedure FreeStyle; override;
    procedure Change; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DropDown; virtual; abstract;
  end;

{ TComboEdit }

  TComboEdit = class(TComboEditBase)
  private
    FDropDownCount: Integer;
    FPopup: TPopup;
    FListBox: TComboEditListBox;
    FPlacement: TPlacement;
    FItems: TStrings;
    FItemHeight: Single;
    FDroppedDown: Boolean;
    FOnPopup: TNotifyEvent;
    FOnClosePopup: TNotifyEvent;
    procedure DoItemsChanged(Sender: TObject);
    procedure RebuildList;
    procedure SetItemHeight(const Value: Single);
    procedure SetItems(const Value: TStrings);
    function GetItemIndex: Integer;
    procedure SetItemIndex(const Value: Integer);
    procedure SetListBox(Value: TComboEditListBox);
    function GetCount: Integer;
    function GetListBoxResource: string;
    procedure SetListBoxResource(const Value: string);
    procedure Typing(Sender: TObject);
  protected
    FListPicker: TCustomListPicker;
    FDropDownKind: TDropDownKind;
    procedure DoOnValueChangedFromDropDownList(Sender: TObject; const AValueIndex: Integer);
    function CreateListBox: TComboEditListBox; virtual;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    procedure Notification(Component: TComponent; Operation: TOperation); override;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure DoRealign; override;
    procedure ChangeParent; override;
    procedure DoPopup(Sender: TObject);
    procedure DoClosePopup(Sender: TObject);
    function GetDefaultStyleLookupName: string; override;
    procedure SetText(const Value: string); override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Action;
    procedure DropDown; override;
    procedure Clear;
    property DroppedDown: Boolean read FDroppedDown;
    property ListBox: TComboEditListBox read FListBox write SetListBox;
    property Count: Integer read GetCount;
  published
    property CanFocus default True;
    property CanParentFocus;
    property Cursor default crDefault;
    property DisableFocusEffect;
    property DropDownCount: Integer read FDropDownCount write FDropDownCount default 8;
    property DropDownKind: TDropDownKind read FDropDownKind write FDropDownKind default TDropDownKind.ddkNative;
    property Font;
    property KeyboardType;
    property ReadOnly;
    property ItemHeight: Single read FItemHeight write SetItemHeight;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property Items: TStrings read FItems write SetItems;
    property ListBoxResource: string read GetListBoxResource write SetListBoxResource;
    property Text;
    property TextAlign;
    property Position;
    property Width;
    property Height;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property StyledSettings;
    property StyleLookup;
    property ClipChildren default False;
    property ClipParent default False;
    property DesignVisible default True;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property HitTest default True;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property ShowHint default False;
    property TouchTargetExpansion;
    property Visible default True;
    property KillFocusByReturn;
    { events }
    property OnChange;
    property OnChangeTracking;
    property OnTyping;
    property OnApplyStyleLookup;
    property OnClosePopup: TNotifyEvent read FOnClosePopup write FOnClosePopup;
    property OnPopup: TNotifyEvent read FOnPopup write FOnPopup;
    { Drag and Drop events }
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    { Keyboard events }
    property OnKeyDown;
    property OnKeyUp;
    { Mouse events }
    property OnCanFocus;
    property OnClick;
    property OnDblClick;

    property OnEnter;
    property OnExit;
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

{ TComboTrackBar }

  TComboTrackBar = class(TComboEditBase)
  private
    FPopup: TPopup;
    FTrackBar: TTrackBar;
    FPlacement: TPlacement;
    FDecimalDigits: Byte;
    FValueType: TNumValueType;
    FValueChanged: boolean;
    FTextUpdating: boolean;
    FDefaultValueRange: TBaseValueRange;
    function GetFrequency: Single;
    function GetMax: Single;
    function GetMin: Single;
    function GetValue: Single;
    procedure SetFrequency(const Value: Single);
    procedure SetMax(const Value: Single);
    procedure SetMin(const Value: Single);
    procedure SetValue(const AValue: Single);
    function DefStored: Boolean;
    procedure SetValueType(const AValueType: TNumValueType);
    procedure SetDecimalDigits(const AValue: Byte);
    function MaxStored: Boolean;
    function MinStored: Boolean;
    function ValueStored: Boolean;
  protected
    // converts Value to the given string format
    procedure Loaded; override;
    procedure DoExit; override;
    procedure DoEnter; override;
    procedure SetText(const AValue: string); override;
    function ConvertValueToText(const AValue: Single): String;
    function GetValueAsString: String;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure DoClosePopup(Sender: TObject);
    procedure Change; override;
    procedure DoComboMouseDown(Sender:TObject; Button: TMouseButton; Shift: TShiftState; x, Y: Single); override;
    function GetDefaultStyleLookupName: string; override;
    function GetActionLinkClass: TActionLinkClass; override;
    procedure DoActionClientChanged; override;
    procedure ActionChange(Sender: TBasicAction; CheckDefaults: Boolean); override;
    procedure BeforeChangeProc(Sender: TObject);
    procedure AfterChangeProc(Sender: TObject);
    property DefaultValueRange: TBaseValueRange read FDefaultValueRange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    property Action;
    procedure DropDown; override;
    property TrackBar: TTrackBar read FTrackBar write FTrackBar;
  published
    property CanFocus default True;
    property CanParentFocus;
    property Cursor default crDefault;
    property DisableFocusEffect;
    property Font;
    property KeyboardType;
    property ReadOnly;
    property Min: Single read GetMin write SetMin stored MinStored nodefault;
    property Max: Single read GetMax write SetMax stored MaxStored nodefault;
    property Value: Single read GetValue write SetValue stored ValueStored nodefault;
    property ValueType: TNumValueType read FValueType write SetValueType default TNumValueType.vtInteger;
    property Frequency: Single read GetFrequency write SetFrequency stored ValueStored nodefault;
    property DecimalDigits: Byte read FDecimalDigits write SetDecimalDigits;
    property Text stored False;
    property TextAlign;
    property Position;
    property Width;
    property Height;
    property ClipChildren default False;
    property ClipParent default False;
    property DesignVisible default True;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property HitTest default True;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property StyledSettings;
    property StyleLookup;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property ShowHint default False;
    property TouchTargetExpansion;
    property Visible default True;
    { events }
    property OnChange;
    property OnChangeTracking;
    property OnTyping;
    property OnApplyStyleLookup;
    { Drag and Drop events }
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    { Keyboard events }
    property OnKeyDown;
    property OnKeyUp;
    { Mouse events }
    property OnCanFocus;
    property OnClick;
    property OnDblClick;

    property OnEnter;
    property OnExit;
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

{ TClearingEdit }

  TClearingEdit = class(TCustomEdit)
  private
    FClearBtn: TCustomButton;
  protected
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
    procedure DoClearBtnClick(Sender: TObject);
  public
    property Action;
  published
    property Cursor default crIBeam;
    property CanFocus default True;
    property CanParentFocus;
    property DisableFocusEffect;
    property Font;
    property KeyboardType;
    property Password;
    property ReadOnly;
    property Text;
    property TextAlign;
    property Position;
    property Width;
    property Height;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property ClipChildren default False;
    property ClipParent default False;
    property DesignVisible default True;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property HitTest default True;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property StyleLookup;
    property StyledSettings;
    property TouchTargetExpansion;
    property Visible default True;
    property Caret;
    property KillFocusByReturn;
    { events }
    property OnChange;
    property OnChangeTracking;
    property OnTyping;
    property OnApplyStyleLookup;
    { Drag and Drop events }
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    { Keyboard events }
    property OnKeyDown;
    property OnKeyUp;
    { Mouse events }
    property OnCanFocus;
    property OnClick;
    property OnDblClick;

    property OnEnter;
    property OnExit;
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

  TSearchBox = class(TEdit, IListBoxHeaderTrait)
  strict private
    FResponder: ISearchResponder;
    FMagGlass: TEditButton;
    FClearButton: TEditButton;

    procedure FindButtonResources;
    procedure DoButtonsChanged(Sender: TObject);
  protected
    procedure DoChangeTracking; override;
    procedure RealignButtonsContainer; override;
    procedure FreeStyle; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetParent(const Value: TFmxObject); override;
  end;

function TryTextToValue(AText: string; var AValue: Single; DefaultValue: Single): boolean;

implementation

uses
  System.Variants, System.SysUtils, System.StrUtils, FMX.Ani, FMX.Consts,
  System.Math, System.UIConsts, System.Character, FMX.BehaviorManager
  {$IFDEF MACOS}, Macapi.CoreFoundation{$ENDIF};

const
  LOUPE_OFFSET = 10;

function TryTextToValue(AText: string; var AValue: Single; DefaultValue: Single): boolean;
var TempValue: Single;
begin
  AText := StringReplace(AText, ' ', '', [rfReplaceAll]);
  AText := StringReplace(AText, ',', FormatSettings.DecimalSeparator, [rfReplaceAll]);
  AText := StringReplace(AText, '.', FormatSettings.DecimalSeparator, [rfReplaceAll]);
  Result := TryStrToFloat(AText, TempValue, FormatSettings);
  if Result then
    AValue := TempValue
  else
    AValue := DefaultValue;
end;

function FilterCharByValueType(const AValueType: TNumValueType): string;
begin
  if AValueType = TNumValueType.vtInteger then
    Result := '0123456789-+'
  else
  begin
    Result := '0123456789.,-+';
    if Pos(FormatSettings.DecimalSeparator, Result) = 0 then
      Result := Result + FormatSettings.DecimalSeparator;
  end;
end;

type
  TComboTrackActionLink = class (TControlActionLink)
    function IsCaptionLinked: Boolean; override;
  end;

{ TComboTrackActionLink }

function TComboTrackActionLink.IsCaptionLinked: Boolean;
begin
  Result := False;
end;

type
  TEditSettingsInfo = class (TTextSettingsInfo)
  private
    FEdit: TCustomEdit;
  protected
    procedure DoLastChanged; override;
  public
    constructor Create(AOwner: TPersistent); override;
    property Edit: TCustomEdit read FEdit;
  end;

{ TEditSettingsInfo }

constructor TEditSettingsInfo.Create(AOwner: TPersistent);
begin
  inherited;
  if AOwner is TCustomEdit then
    FEdit := TCustomEdit(AOwner)
  else
    raise EArgumentException.CreateFMT(SEUseHeirs, [TCustomEdit.ClassName]);
end;

procedure TEditSettingsInfo.DoLastChanged;
begin
  FEdit.DoChanged;
  inherited;
end;

{ TCustomEdit }

constructor TCustomEdit.Create(AOwner: TComponent);
var
  PlatformTextService: IFMXTextService;
begin
  inherited;
  FKillFocusByReturn := False;
  EnableExecuteAction := False;

  FTextSettingsInfo := TEditSettingsInfo.Create(self);
  FTextLayout := TTextLayoutManager.DefaultTextLayout.Create;

  if not TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, IInterface(FClipboardSvc)) then
    FClipboardSvc := nil;
  if TPlatformServices.Current.SupportsPlatformService(IFMXTextService, IInterface(PlatformTextService)) then
    FTextService := PlatformTextService.GetTextServiceClass.Create(Self, False);

  TPlatformServices.Current.SupportsPlatformService(ILoupeService, IInterface(FLoupeService));

  DefaultTextSettings.FontColor := TAlphaColorRec.Black;
  DefaultTextSettings.HorzAlign := TTextAlign.taLeading;
  FCursorFill := TBrush.Create(TBrushKind.bkSolid, DefaultTextSettings.FontColor);
  FSelectionFill := TBrush.Create(TBrushKind.bkSolid, $802A8ADF);
  CanFocus := True;
  Cursor := crIBeam;
  TextAlign := TTextAlign.taLeading;
  AutoCapture := True;
  FSelStart := 0;
  FSelLength := 0;
  FFirstVisibleChar := 1;
  FImeMode := TImeMode.imDontcare;
  FInputSupport := True;
  CreatePopupMenu;
  FCaret := CreateCaret;
  SetAcceptsControls(False);
  FButtonsContent := TContentEdit.Create(Self);
  FButtonsContent.Parent := Self;
  FButtonsContent.Locked := True;
  FButtonsContent.Stored := False;
  FButtonsContent.HitTest := False;
  FKeyboardType := TVirtualKeyboardType.vktDefault;
  FReturnKeyType := TReturnKeyType.rktDefault;
  Touch.InteractiveGestures := Touch.InteractiveGestures + [TInteractiveGesture.igDoubleTap, TInteractiveGesture.igLongTap];

  FSpellMenuItems := TList<TMenuItem>.Create;
  FSpellFill := TBrush.Create(TBrushKind.bkSolid, TAlphaColorRec.Red);
  FSpellUnderlineBrush := TStrokeBrush.Create(TBrushKind.bkSolid, TAlphaColorRec.Red);
  FSpellUnderlineBrush.Dash := TStrokeDash.sdDot;
  FSpellUnderlineBrush.Thickness := 1;
  if not FTextLayoutUpdated then
    UpdateTextLayout;
end;

destructor TCustomEdit.Destroy;
begin
  FreeAndNil(FCaret);
  FLoupeService := nil;
  FButtonsContent := nil;
  FreeAndNil(FCursorFill);
  FreeAndNil(FSelectionFill);
  FreeAndNil(FEditPopupMenu);
  FreeAndNil(FTextService);
  FClipboardSvc := nil;
  FreeandNil(FTextSettingsInfo);
  FSpellService := nil;
  Finalize(FSpellingRegions);
  FreeAndNil(FSpellMenuItems);
  FreeAndNil(FSpellFill);
  FreeAndNil(FSpellUnderlineBrush);
  FreeAndNil(FTextLayout);
  inherited;
end;

type
  TOpenCaret = class(TCaret)

  end;

function TCustomEdit.CreateCaret: TCaret;
begin
  Result := TCaret.Create(Self);
  Result.Visible := FInputSupport;
  Result.ReadOnly := ReadOnly;
end;

function TCustomEdit.GetOriginCaretPosition: Integer;
begin
  if Assigned(FTextService) then
    Result := FTextService.CaretPosition.X
  else
    Result := -1;
end;

function TCustomEdit.GetCaret: TCustomCaret;
begin
  Result := FCaret;
end;

procedure TCustomEdit.SetCaret(const Value: TCaret);
begin
  if Assigned(Caret) then
    Caret.Assign(Value);
end;

procedure TCustomEdit.ShowCaret;
begin
  TOpenCaret(Caret).Show;
end;

procedure TCustomEdit.HideCaret;
begin
  TOpenCaret(Caret).Hide;
end;

procedure TCustomEdit.HideHighlightSpell;
begin
  FSpellHightlightRect := TRectF.Empty;
  FCaret.TemporarilyHidden := False;
  RepaintEdit;
end;

procedure TCustomEdit.DefineProperties(Filer: TFiler);
begin
  inherited;
  // Only for backward compatibility with XE2
  Filer.DefineProperty('FontFill.Color', ReadFontFillColor, nil, False);
  Filer.DefineProperty('FontFill.Kind', ReadFontFillKind, nil, False);
end;

function TCustomEdit.GetData: TValue;
begin
  Result := Text;
end;

procedure TCustomEdit.SetData(const Value: TValue);
begin
  if Value.IsEmpty then
    Text := ''
  else
    if Value.IsType<TNotifyEvent> then
      OnChange := Value.AsType<TNotifyEvent>()
    else
      Text := Value.ToString;
end;

procedure TCustomEdit.CreatePopupMenu;
var
  TmpItem: TMenuItem;
begin
  FEditPopupMenu := TPopupMenu.Create(Self);
  FEditPopupMenu.Stored := False;
  FEditPopupMenu.Parent := Self;

  TmpItem := TMenuItem.Create(FEditPopupMenu);
  TmpItem.Parent := FEditPopupMenu;
  TmpItem.Text := SEditCut;
  TmpItem.StyleName := 'cut';
  TmpItem.OnClick := DoCut;

  TmpItem := TMenuItem.Create(FEditPopupMenu);
  TmpItem.Parent := FEditPopupMenu;
  TmpItem.Text := SEditCopy;
  TmpItem.StyleName := 'copy';
  TmpItem.OnClick := DoCopy;

  TmpItem := TMenuItem.Create(FEditPopupMenu);
  TmpItem.Parent := FEditPopupMenu;
  TmpItem.Text := SEditPaste;
  TmpItem.StyleName := 'paste';
  TmpItem.OnClick := DoPaste;

  TmpItem := TMenuItem.Create(FEditPopupMenu);
  TmpItem.Parent := FEditPopupMenu;
  TmpItem.Text := SEditDelete;
  TmpItem.StyleName := 'delete';
  TmpItem.OnClick := DoDelete;

  TmpItem := TMenuItem.Create(FEditPopupMenu);
  TmpItem.Parent := FEditPopupMenu;
  TmpItem.Text := '-';

  TmpItem := TMenuItem.Create(FEditPopupMenu);
  TmpItem.Parent := FEditPopupMenu;
  TmpItem.Text := SEditSelectAll;
  TmpItem.StyleName := 'selectall';
  TmpItem.OnClick := DoSelectAll;
end;

procedure TCustomEdit.DoSelectAll(Sender: TObject);
begin
  SelectAll;
end;

procedure TCustomEdit.DoSelPtMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  EndSelection;
end;

procedure TCustomEdit.DoTyping;
begin
  if Assigned(FOnTyping) then
    FOnTyping(Self);
end;

procedure TCustomEdit.EndIMEInput;
var
  TextChanged: Boolean;
begin
  TextChanged := not SameText(FTextService.Text, FTextService.CombinedText);
  FNeedChange := FNeedChange or TextChanged;
  if TextChanged then
    DoChangeTracking;
  FTextService.Text := FTextService.CombinedText;
  FTextService.CaretPosition := Point(GetOriginCaretPosition + FTextService.CombinedText.Length - FTextService.Text.Length, 0);
  RepaintEdit;
end;

procedure TCustomEdit.EndSelection;
begin
  HideLoupe;
  SelectionMode := TSelectionMode.smNone;
  FTextService.EndSelection;
end;

procedure TCustomEdit.DoCut(Sender: TObject);
begin
  CutToClipboard;
end;

procedure TCustomEdit.DoCopy(Sender: TObject);
begin
  CopyToClipboard;
end;

procedure TCustomEdit.DoDelete(Sender: TObject);
begin
  if Observers.IsObserving(TObserverMapping.EditLinkID) then
    if not TLinkObservers.EditLinkEdit(Observers) then
    begin
      TLinkObservers.EditLinkReset(Observers);
      Exit;
    end
    else
      TLinkObservers.EditLinkModified(Observers);
  if Observers.IsObserving(TObserverMapping.ControlValueID) then
    TLinkObservers.ControlValueModified(Observers);

  DeleteSelection;
end;

procedure TCustomEdit.DoPaste(Sender: TObject);
begin
  PasteFromClipboard;
end;

procedure TCustomEdit.UpdateTextHeight;
begin
  FTextHeight := 0;
  if Assigned(FContent) and Assigned(FContent.Canvas) then
  begin
    TCanvasManager.MeasureCanvas.Font.Assign(LastTextSettings.Font);
    FTextHeight := TCanvasManager.MeasureCanvas.TextHeight('Lb|y');
  end;
end;

procedure TCustomEdit.RealignContent;
var
  XBtn: Single;
  LBtn: Single;
  Size: TSizeF;
  Pos: TPointF;
  OldDisableAlign: Boolean;
begin
  OldDisableAlign := FDisableAlign;
  try
    FDisableAlign := True;
    XBtn := 0;
    LBtn := 0;

    RealignButtonsContainer;

    if Assigned(FContent) and Assigned(FContent.Canvas) then
    begin
      if FLeftLayout <> nil then
        LBtn := FLeftLayout.Width;
      if FButtonsLayout <> nil then
        XBtn := FButtonsLayout.Width;

      if FTextHeight <= 0 then
        UpdateTextHeight;
      Pos.X := Max(FContent.Margins.Left, 0);
      Pos.Y := Max(FContent.Margins.Top, 0);
      Size.cx := Max(Width - XBtn - LBtn - FContent.Margins.Right - Pos.X, 0);
      Size.cy := Max(Height - FContent.Margins.Bottom - Pos.Y, 0);
      FLineHeight := Max(Min(Size.cy, FTextHeight + Max(1, Round(FTextHeight / 10))), 0);
      case LastTextSettings.VertAlign of
        TTextAlign.taCenter:
          FLineTop := Max((Size.cy - FLineHeight) / 2, 0) + Pos.Y;
        TTextAlign.taLeading:
          FLineTop := Pos.Y;
        TTextAlign.taTrailing:
          FLineTop := Max(Size.cy - FLineHeight, 0) + Pos.Y;
      end;
      FContent.SetBounds(Pos.X + LBtn, Pos.Y, Size.cx, Size.cy);
      if Assigned(FPrompt) then
        FPrompt.SetBounds(Pos.X + LBtn, Pos.Y, Size.cx, Size.cy);

      FTextLayout.TopLeft := ContentRect.TopLeft;

      UpdateLayoutSize;
    end;
  finally
    FDisableAlign := OldDisableAlign;
  end;
end;

procedure TCustomEdit.DoRealign;
begin
  RealignContent;
  inherited DoRealign;
  RealignButtonsContainer;
  CaretPosition := GetOriginCaretPosition;
end;

procedure TCustomEdit.DoRightSelPtMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  BeginSelection;
  SetLoupePosition(TSelectionPointType.sptRight);
  ShowLoupe;
end;

procedure TCustomEdit.UpdatePopupMenuItems;
var
  SelTextIsValid: Boolean;

  procedure SetParam(AParamName : string; AValue : Boolean) ;
  var
    LMenuItem : TMenuItem;
  begin
    LMenuITem := FindContextMenuItem(AParamName);
    if Assigned(LMenuItem) then
      LMenuItem.Enabled := AValue;
  end;

begin
  SelTextIsValid := not SelText.IsEmpty;
  SetParam('cut', SelTextIsValid and not ReadOnly and InputSupport and not Password);
  SetParam('copy', SelTextIsValid and not Password);
  if Assigned(FClipboardSvc) then
    SetParam('paste', (not FClipBoardSvc.GetClipboard.IsEmpty) and (not ReadOnly) and InputSupport)
  else
    SetParam('paste', False);
  SetParam('delete', SelTextIsValid and not ReadOnly and InputSupport);
  SetParam('selectall', SelText <> Text);
end;

procedure TCustomEdit.UpdateSelectionPointPositions;
var
  R: TRectF;
begin
  if not HaveSelectionPickers then
    Exit;

  FLeftSelPt.Visible := (SelLength > 0) and IsFocused and (SelStart + 1 >= FFirstVisibleChar);
  FRightSelPt.Visible := (SelLength > 0) and IsFocused and (GetCharX(SelStart + SelLength) < ContentRect.Right);

  Caret.TemporarilyHidden := (SelLength > 0) and IsFocused;
  R := GetSelRect;

  FLeftSelPt.Position.X := R.Left;
  FLeftSelPt.Position.Y := R.Top - 2 * FLeftSelPt.GripSize;

  FRightSelPt.Position.X := R.Right;
  FRightSelPt.Position.Y := R.Bottom + 2 * FLeftSelPt.GripSize;
end;

procedure TCustomEdit.UpdateSpelling;
begin
  if FCheckSpelling then
  begin
    FUpdateSpelling := True;
    SetLength(FSpellingRegions, 0);
  end;
end;

procedure TCustomEdit.UpdateSpellPopupMenu(const APoint: TPointF);
var
  I, J, BP, EP: Integer;
  LPos: Integer;
  Spells: TArray<string>;
  LMenuItem: TMenuItem;
begin
  for I := 0 to FSpellMenuItems.Count - 1 do
    FSpellMenuItems[I].Parent := nil;
  FSpellMenuItems.Clear;
  //
  for I := Low(FSpellingRegions) to High(FSpellingRegions) do
    if FSpellingRegions[I].Contains(APoint) then
    begin
      LPos := FTextLayout.PositionAtPoint(APoint);
      if (LPos > -1) and FMX.Text.FindWordBound(Text, LPos, BP, EP) then
      begin
        Spells := FSpellService.CheckSpelling(Text.Substring(BP, EP - BP + 1));
        if Length(Spells) > 0 then
        begin
          for J := Low(Spells) to High(Spells) do
          begin
            LMenuItem := TMenuItem.Create(FEditPopupMenu);
            LMenuItem.Text := Spells[J];
            LMenuItem.Font.Style := LMenuItem.Font.Style + [TFontStyle.fsBold];
            LMenuItem.Tag := LPos;
            LMenuItem.OnClick := SpellFixContextMenuHandler;
            FEditPopupMenu.InsertObject(FSpellMenuItems.Count, LMenuItem);
            FSpellMenuItems.Add(LMenuItem);
          end;
          LMenuItem := TMenuItem.Create(FEditPopupMenu);
          LMenuItem.Text := '-';
          FEditPopupMenu.InsertObject(FSpellMenuItems.Count, LMenuItem);
          FSpellMenuItems.Add(LMenuItem);
        end;
      end;
      Break;
    end;
end;

procedure TCustomEdit.ReadFontFillColor(Reader: TReader);
var
  LFontColor: TAlphaColor;
begin
  IdentToAlphaColor(Reader.ReadIdent, Longint(LFontColor));
  FontColor := LFontColor;
end;

procedure TCustomEdit.ReadFontFillKind(Reader: TReader);
begin
  Reader.ReadIdent;
end;

procedure TCustomEdit.RealignButtonsContainer;
var
  I: Integer;
  TotalWidth: Single;
  R: TRectF;
  PositionTmp: TPointF;
  EditControl: IEditControl;
begin
  if not Assigned(FButtonsLayout) then
    Exit;

  TotalWidth := 0;
  for I := 0 to FButtonsContent.ChildrenCount - 1 do
  begin
    if FButtonsContent.Children[I].GetInterface(IEditControl, EditControl) then
      TotalWidth := TotalWidth + EditControl.BoundsRect.Width;
  end;
  FButtonsLayout.Width := TotalWidth;
  FButtonsLayout.Height := Height;
  PositionTmp := FButtonsLayout.LocalToAbsolute(PointF(0,0));
  R := FButtonsLayout.AbsoluteRect;
  FButtonsContent.SetBounds(R.Left, R.Top, RectWidth(R), RectHeight(R));
  FButtonsContent.Position.Point := AbsoluteToLocal(PositionTmp);
end;

function TCustomEdit.GetKeyboardType: TVirtualKeyboardType;
begin
  Result := FKeyboardType;
end;

function TCustomEdit.GetListOfPrepositions: TArray<string>;
var
  BP, EP: Integer;
begin
  Result := nil;
  if not Text.IsEmpty and (GetOriginCaretPosition >= 0) then
    if FMX.Text.FindWordBound(Text, GetOriginCaretPosition, BP, EP) then
      Result := FSpellService.CheckSpelling(Text.Substring(BP, EP - BP + 1));
end;

function TCustomEdit.GetLastTextSettings: TTextSettings;
begin
  Result := FTextSettingsInfo.LastTextSettings;
end;

procedure TCustomEdit.ApplyStyle;
var
  T: TFmxObject;
  FontBehavior: IInterface;
  FontFamily: string;
begin
  TextSettings.BeginUpdate;
  try
    FTextSettingsInfo.Design := False;
    inherited ApplyStyle;

    { Container for right-hand buttons}
    T := FindStyleResource('buttons');
    if Assigned(T) and (T is TControl) then
    begin
      FButtonsLayout := T as TControl;
    end;

    { Container for left-hand buttons}
    T := FindStyleResource('leftbuttons');
    if Assigned(T) and (T is TControl) then
    begin
      FLeftLayout := T as TControl;
    end;

    if Assigned(FButtonsLayout) or Assigned(FLeftLayout) then
      RealignButtonsContainer;

    { Text prompt }
    T := FindStyleResource('prompt');
    if Assigned(T) and (T is TTextControl) then
    begin
      FPrompt := TTextControl(T);
      FPrompt.Visible := (Text.Length = 0);
    end;

    T := FindStyleResource('content');
    if Assigned(T) and (T is TControl) then
    begin
      FContent := TControl(T);
      FContent.Align := TAlignLayout.alNone;
      RealignContent;
      FContent.OnPaint := DoContentPaint;
    end;
    T := FindStyleResource('selection');
    if Assigned(T) and (T is TBrushObject) then
    begin
      FSelectionFill.Assign(TBrushObject(T).Brush);
    end;
    { from style }
    T := FindStyleResource('foreground');
    // apply the font fill style only if the user hasn't changed the font fill anteriorly
    if Assigned(T) and
       (T is TBrushObject) then
    begin
      DefaultTextSettings.FontColor := TBrushObject(T).Brush.Color;
    end;

    T := FindStyleResource('caretcolor');
    if Assigned(T) and (T is TColorObject) then
      TOpenCaret(Caret).SetDefaultColor((T as TColorObject).Color)
    else
      TOpenCaret(Caret).SetDefaultColor(TAlphaColorRec.Null);

    T := FindStyleResource('font');
    if Assigned(T) and
       Supports(T, IFontObject) and
       (not Font.IsSizeStored) then
    begin
      DefaultTextSettings.Font := (T as IFontObject).Font;
    end;
    DefaultTextSettings.HorzAlign := TTextAlign.taLeading;

    if Assigned(Scene) then
    begin
      BehaviorServices.SupportsBehaviorService(IFontBehavior, FontBehavior, Scene.GetObject);

      if Assigned(FontBehavior) then
      begin
        IFontBehavior(FontBehavior).GetDefaultFontFamily(Scene.GetObject, FontFamily);
        if FontFamily <> '' then
          DefaultTextSettings.Font.Family := FontFamily;
      end;
    end;

    T := FindStyleResource('leftselectionpoint');
    if Assigned(T) and (T is TSelectionPoint) then
    begin
      FLeftSelPt := T as TSelectionPoint;
      FLeftSelPt.OnTrack := DoLeftSelPtChangePosition;
      FLeftSelPt.OnMouseDown := DoLeftSelPtMouseDown;
      FLeftSelPt.OnMouseUp := DoSelPtMouseUp;
      FLeftSelPt.Visible := False;
    end;
    T := FindStyleResource('rightselectionpoint');
    if Assigned(T) and (T is TSelectionPoint) then
    begin
      FRightSelPt := T as TSelectionPoint;
      FRightSelPt.OnTrack := DoRightSelPtChangePosition;
      FRightSelPt.OnMouseDown := DoRightSelPtMouseDown;
      FRightSelPt.OnMouseUp := DoSelPtMouseUp;
      FRightSelPt.Visible := False;
    end;
    TextSettings.Change;
  finally
    TextSettings.EndUpdate;
    FTextSettingsInfo.Design := csDesigning in ComponentState;
  end;

  if Assigned(FPrompt) then
  begin
    FPrompt.StyledSettings := [];
    FPrompt.TextSettings.Assign(LastTextSettings);
  end;
end;

function TCustomEdit.GetDefaultSize: TSizeF;
var
  DefMetricsSrv: IFMXDefaultMetricsService;
begin
  if SupportsPlatformService(IFMXDefaultMetricsService, IInterface(DefMetricsSrv)) and DefMetricsSrv.SupportsDefaultSize(ckEdit) then
    Result := TSizeF.Create(DefMetricsSrv.GetDefaultSize(ckEdit).Width, DefMetricsSrv.GetDefaultSize(ckEdit).Height)
  else
    Result := TSizeF.Create(100, 22);
end;

function TCustomEdit.GetDefaultTextSettings: TTextSettings;
begin
  Result := FTextSettingsInfo.DefaultTextSettings;
end;

function TCustomEdit.GetTextSettings: TTextSettings;
begin
  Result := FTextSettingsInfo.TextSettings;
end;

function TCustomEdit.GetStyledSettings: TStyledSettings;
begin
  Result := FTextSettingsInfo.StyledSettings;
end;

procedure TCustomEdit.SetTextSettings(const Value: TTextSettings);
begin
  FTextSettingsInfo.TextSettings.Assign(Value);
end;

procedure TCustomEdit.SetStyledSettings(const Value: TStyledSettings);
begin
  FTextSettingsInfo.StyledSettings := Value;
end;

function TCustomEdit.GetFont: TFont;
begin
  Result := FTextSettingsInfo.TextSettings.Font;
end;

procedure TCustomEdit.SetFont(const Value: TFont);
begin
  FTextSettingsInfo.TextSettings.Font := Value;
end;

function TCustomEdit.GetFontColor: TAlphaColor;
begin
  Result := FTextSettingsInfo.TextSettings.FontColor;
end;

procedure TCustomEdit.SetFontColor(const Value: TAlphaColor);
begin
  FTextSettingsInfo.TextSettings.FontColor := Value;
end;

function TCustomEdit.GetTextAlign: TTextAlign;
begin
  Result := FTextSettingsInfo.TextSettings.HorzAlign;
end;

procedure TCustomEdit.SetTextAlign(const Value: TTextAlign);
begin
  FTextSettingsInfo.TextSettings.HorzAlign := Value;
end;

procedure TCustomEdit.SetTextInternal(const Value: String);
begin
  FTextLayout.Text := Value;
  FTextService.Text := Value;
  if not (csLoading in ComponentState) then
    DoChangeTracking;
  if FPrompt <> nil then
    FPrompt.Visible := (Value.Length = 0);
  FNeedChange := True;
end;

function TCustomEdit.GetVertTextAlign: TTextAlign;
begin
  Result := FTextSettingsInfo.TextSettings.VertAlign;
end;

procedure TCustomEdit.SetVertTextAlign(const Value: TTextAlign);
begin
  FTextSettingsInfo.TextSettings.VertAlign := Value;
end;

procedure TCustomEdit.ShowLoupe;
begin
  if Assigned(FLoupeService) then
  begin
    FLoupeService.SetLoupeScale(DEFAULT_LOUPE_SCALE);
    FLoupeService.ShowFor(Self);
  end;
end;

procedure TCustomEdit.Spell(const AWord: string);
var
  BP, EP: Integer;
begin
  if not Text.IsEmpty and (GetOriginCaretPosition >= 0) then
    if FMX.Text.FindWordBound(Text, GetOriginCaretPosition, BP, EP) then
    begin
      Text := Text.Remove(BP, EP - BP + 1).Insert(BP, AWord);
      DoChangeTracking;
      FUpdateSpelling := True;
      HideHighlightSpell;
      UpdateSpelling;
      CaretPosition := BP + AWord.Length;
    end;
end;

procedure TCustomEdit.SpellFixContextMenuHandler(Sender: TObject);
var
  LPos: Integer;
  BP, EP: Integer;
begin
  if Sender is TMenuItem then
  begin
    LPos := TMenuItem(Sender).Tag;
    if (LPos > -1) and FMX.Text.FindWordBound(Text, LPos, BP, EP) then
    begin
      Text := Text.Substring(0, BP) +
              TMenuItem(Sender).Text +
              Text.Substring(EP + 1);
      DoChangeTracking;
    end;
  end;
end;

procedure TCustomEdit.UpdateTextLayout;
begin
  if Assigned(FPrompt) then
  begin
    FPrompt.StyledSettings := [];
    FPrompt.TextSettings.Assign(LastTextSettings);
  end;
  FTextLayout.BeginUpdate;
  try
    FTextLayout.HorizontalAlign := LastTextSettings.HorzAlign;
    FTextLayout.VerticalAlign := LastTextSettings.VertAlign;
    FTextLayout.Font := LastTextSettings.Font;
    FTextLayout.TopLeft := ContentRect.TopLeft;
    UpdateLayoutSize;
  finally
    FTextLayout.EndUpdate;
  end;
  FTextLayoutUpdated := True;
end;

procedure TCustomEdit.DoChanged;
begin
  if ([csLoading, csDesigning] * ComponentState = []) and (not Released) then
  begin
    if LastTextSettings.IsAdjustChanged then
    begin
      UpdateTextHeight;
      if not FDisableAlign then
        RealignContent;
    end;
    RepaintEdit;
  end;
  UpdateTextLayout;
end;

procedure TCustomEdit.DoRightSelPtChangePosition(Sender: TObject; var X,
  Y: Single);
var
  CurrentPoint: TPointF;
  NewSelEnd: Integer;
  OldSelStart: Integer;
  OldSelLength: Integer;
  NewSelLength: Integer;
  OldSelEnd: Integer;
  MinSelEnd: Integer;
begin
  if not Assigned(FRightSelPt) then
    Exit;
  CurrentPoint := FRightSelPt.Position.Point;
  Y := CurrentPoint.Y;

  OldSelStart := SelStart;
  OldSelLength := SelLength;
  OldSelEnd := SelStart + SelLength;
  MinSelEnd := SelStart + 1;
  NewSelEnd := GetCoordinatePosition(X);
  NewSelLength := OldSelLength + NewSelEnd - OldSelEnd;

  if NewSelEnd > MinSelEnd then
  begin
    X := GetCharX(NewSelEnd);
    SelStart := NewSelEnd - NewSelLength;
    SelLength := NewSelLength;
  end
  else
  begin
    X := GetCharX(MinSelEnd);
    SelStart := OldSelStart;
    SelLength := 1;
  end;

  SetLoupePosition(TSelectionPointType.sptRight);
  UpdateSelectionPointPositions;
end;

procedure TCustomEdit.DoLeftSelPtChangePosition(Sender: TObject; var X,
  Y: Single);
var
  CurrentPoint: TPointF;
  NewSelStart: Integer;
  OldSelStart: Integer;
  OldSelLength: Integer;
  NewSelLength: Integer;
  OldSelEnd: Integer;
begin
  if not Assigned(FLeftSelPt) then
    Exit;
  CurrentPoint := FLeftSelPt.Position.Point;

  OldSelStart := SelStart;
  OldSelLength := SelLength;
  OldSelEnd := SelStart + SelLength;
  NewSelStart := GetCoordinatePosition(X);
  NewSelLength := OldSelLength + OldSelStart - NewSelStart;

  if NewSelStart < OldSelEnd - 1 then
  begin
    X := GetCharX(NewSelStart);
    SelStart := NewSelStart;
    SelLength := NewSelLength;
  end
  else
  begin
    X := GetCharX(OldSelEnd - 1);
    SelStart := OldSelEnd - 1;
    SelLength := 1;
  end;
  Y := CurrentPoint.Y;
  SetLoupePosition(TSelectionPointType.sptLeft);
  UpdateSelectionPointPositions;
end;

function TCustomEdit.StyledSettingsStored: Boolean;
begin
  Result := StyledSettings <> DefaultStyledSettings;
end;

function TCustomEdit.ContentRect: TRectF;
begin
  if Assigned(FContent) then
    Result := FContent.ParentedRect
  else
    Result := LocalRect;
end;

procedure TCustomEdit.DoAddObject(const AObject: TFmxObject);
var
  EditControl: IEditControl;
begin
  if AObject.GetInterface(IEditControl, EditControl) then
  begin
    if (Assigned(FButtonsContent.Children) and not FButtonsContent.ContainsObject(AObject)) or
       not Assigned(FButtonsContent.Children) then
    begin
      FButtonsContent.AddObject(AObject);
      if not FDisableAlign then
        RealignContent;
    end;
  end
  else
    inherited DoAddObject(AObject);
end;

procedure TCustomEdit.DoChangeTracking;
begin
  UpdateSpelling;
  FNeedChange := True;
  if Assigned(FOnChangeTracking) then
    FOnChangeTracking(Self);
end;

procedure TCustomEdit.DoContentPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
var
  i: Integer;
  R: TRectF;
  State: TCanvasSaveState;
  WholeTextWidth: Single;
  EditRectWidth: Single;
  T: string;

  function GetSelectionRectInLocalCoordinate: TRectF;
  begin
    Result := GetSelRect;
    OffsetRect(Result, -ContentRect.Left, -ContentRect.Top);
  end;

  procedure DrawSelection;
  var
    SelectionRect: TRectF;
  begin
    SelectionRect := GetSelectionRectInLocalCoordinate;
    Canvas.FillRect(SelectionRect, 0, 0, AllCorners, AbsoluteOpacity, FSelectionFill);
  end;

  procedure DrawLeftAndRightSelectionSide;
  var
    SelectionRect: TRectF;
    HalfCaretWidth: Single;
    SideRect: TRectF;
  begin
    SelectionRect := GetSelectionRectInLocalCoordinate;
    HalfCaretWidth := Caret.Flasher.Size.Width / 2;
    FCursorFill.Color := Caret.Flasher.Color;
    // Draw Left selection side
    SideRect := RectF(SelectionRect.Left - HalfCaretWidth, SelectionRect.Top,
                      SelectionRect.Left + HalfCaretWidth, SelectionRect.Bottom);
    Canvas.FillRect(SideRect, 0, 0, AllCorners, AbsoluteOpacity, FCursorFill);
    // Draw Right selection side
    SideRect := RectF(SelectionRect.Right - HalfCaretWidth, SelectionRect.Top,
                      SelectionRect.Right + HalfCaretWidth, SelectionRect.Bottom);
    Canvas.FillRect(SideRect, 0, 0, AllCorners, AbsoluteOpacity, FCursorFill);
  end;

var
  Shift, BP, EP, J: Integer;
  Rgn: TRegion;
  LText: string;
  VisibleCharPos: Single;
begin
  if (not Assigned(FTextService)) or (FTextService.Text.IsEmpty and (not FTextService.HasMarkedText)) then                         
    Exit;
  State := Canvas.SaveState;
  try
    { draw text }
    Canvas.IntersectClipRect(ARect);
    Canvas.Fill.Color := LastTextSettings.FontColor;
    R := FContent.Padding.PaddingRect(ARect);
    if FPassword then
    begin
      R.Right := R.Left + GetPasswordCharWidth - 1;
      R.Top := LineTop - ContentRect.Top + ((LineHeight - R.Width) / 2);
      R.Bottom := R.Top + RectWidth(R);
      T := FTextService.CombinedText;
      WholeTextWidth := T.Length * GetPasswordCharWidth;
      EditRectWidth := FContent.Padding.PaddingRect(ContentRect).Width;
      if WholeTextWidth < EditRectWidth then
        case LastTextSettings.HorzAlign of
          TTextAlign.taTrailing:
            OffsetRect(R, (EditRectWidth - WholeTextWidth), 0);
          TTextAlign.taCenter:
            OffsetRect(R, ((EditRectWidth - WholeTextWidth) / 2), 0);
        end;
      for i := FFirstVisibleChar to T.Length do
      begin
        Canvas.FillEllipse(R, AbsoluteOpacity, Canvas.Fill);
        OffsetRect(R, R.Width + 1, 0);
      end;
    end
    else
    begin
      FTextService.DrawSingleLine(Canvas,
        R, FFirstVisibleChar, LastTextSettings.Font,
        AbsoluteOpacity, FillTextFlags, LastTextSettings.HorzAlign,
        LastTextSettings.VertAlign);
    end;
    { Draw selection }
    if IsFocused and HasSelection then
    begin
      DrawSelection;
      { left picker -> | selected text | <- right picker }
      if HaveSelectionPickers then
        DrawLeftAndRightSelectionSide;
    end;
    //Spell highlighting
    if FCheckSpelling and Assigned(FSpellService) and not FTextService.HasMarkedText and not Text.IsEmpty then
    begin
      if FUpdateSpelling then
      begin
        LText := Text;
        Shift := 0;
        while (LText.Length > 0) and FMX.Text.FindWordBound(LText, 0, BP, EP) do
        begin
          if Length(FSpellService.CheckSpelling(LText.Substring(BP, EP - BP + 1))) > 0 then
          begin
            Rgn := FTextLayout.RegionForRange(TTextRange.Create(Shift + BP, EP - BP + 1));
            for J := Low(Rgn) to High(Rgn) do
            begin
              SetLength(FSpellingRegions, Length(FSpellingRegions) + 1);
              FSpellingRegions[High(FSpellingRegions)] := Rgn[J];
              R := ContentRect;
              FSpellingRegions[High(FSpellingRegions)].Offset(-R.Left, -R.Top);
            end;
          end;
          LText := LText.Remove(0, EP + 1);
          Inc(Shift, EP + 1);
        end;
        FUpdateSpelling := False;
      end;
      if Length(FSpellingRegions) > 0 then
      begin
        if FFirstVisibleChar > 1 then
        begin
          Rgn := FTextLayout.RegionForRange(TTextRange.Create(FFirstVisibleChar - 1, 1));
          if Length(Rgn) > 0 then
            VisibleCharPos := Rgn[0].Left
          else
            VisibleCharPos := 0;
        end
        else
          VisibleCharPos := 0;
        for I := Low(FSpellingRegions) to High(FSpellingRegions) do
          Canvas.DrawLine(PointF(FSpellingRegions[I].Left - VisibleCharPos, FSpellingRegions[I].Bottom), PointF(FSpellingRegions[I].Right - VisibleCharPos, FSpellingRegions[I].Bottom), AbsoluteOpacity, FSpellUnderlineBrush);
      end;
      if not FSpellHightlightRect.IsEmpty then
        Canvas.FillRect(FSpellHightlightRect, 0, 0, [], 0.2, FSpellFill);
    end;
  finally
    Canvas.RestoreState(State);
  end;
end;

procedure TCustomEdit.InsertText(const AText: string);
var
  OldText: string;
begin
  if ReadOnly and not InputSupport then
    Exit;

  OldText := Text;
  // FActionStack.FragmentDeleted(SelStart + 1, Copy(TmpS, SelStart+1, SelLength));
  OldText := OldText.Remove(SelStart, SelLength);

  // FActionStack.FragmentInserted(SelStart + 1, Length(AText), SelLength <> 0);
  OldText := OldText.Insert(SelStart, AText);

  if (MaxLength <= 0) or (OldText.Length <= MaxLength) then
  begin
    SetTextInternal(OldText);
    CaretPosition := SelStart + AText.Length;
  end;
  SelLength := 0;
end;

function TCustomEdit.IsCurrentWordWrong: Boolean;
var
  Rgn: TRegion;
  I: Integer;
begin
  Result := False;
  Rgn := FTextLayout.RegionForRange(TTextRange.Create(GetOriginCaretPosition, 1));
  if Length(Rgn)> 0 then
    for I := Low(FSpellingRegions) to High(FSpellingRegions) do
      if FSpellingRegions[I].Contains(Rgn[0].TopLeft) then
      begin
        Result := True;
        Break;
      end;
end;

function TCustomEdit.IsSpellCheckEnabled: Boolean;
begin
  Result := FCheckSpelling;
end;

procedure TCustomEdit.UpdateFirstVisibleChar;
var
  MarkedPosition: Integer;
  LEditRect: TRectF;
  TempStr: string;
begin
  if not Assigned(FContent) then
    Exit;

  FTextLayout.Text := FTextService.CombinedText;
  MarkedPosition := FTextService.TargetClausePosition.X;
  if FFirstVisibleChar >= (MarkedPosition + 1) then
  begin
    FFirstVisibleChar := MarkedPosition;
    if FFirstVisibleChar < 1 then
      FFirstVisibleChar := 1;
  end
  else
  begin
    TempStr := FTextService.CombinedText;
    LEditRect := FContent.Padding.PaddingRect(ContentRect);

    if Password then
      while ((MarkedPosition - FFirstVisibleChar + 1) * GetPasswordCharWidth > LEditRect.Width) and
        (FFirstVisibleChar < TempStr.Length) do
        Inc(FFirstVisibleChar)
    else
    begin
      while (TextWidth(FFirstVisibleChar - 1, MarkedPosition - FFirstVisibleChar + 1) > LEditRect.Width)
        and (FFirstVisibleChar < TempStr.Length) do
      begin

        if TextWidth(FFirstVisibleChar + 500 - 1, (MarkedPosition - FFirstVisibleChar + 500) + 1) >
          LEditRect.Width then
          Inc(FFirstVisibleChar, 500)
        else
          if TextWidth(FFirstVisibleChar + 100 - 1, (MarkedPosition - FFirstVisibleChar + 100) + 1) >
            LEditRect.Width then
            Inc(FFirstVisibleChar, 100)
          else
            if TextWidth(FFirstVisibleChar + 50 - 1, (MarkedPosition - FFirstVisibleChar + 50) + 1) >
              LEditRect.Width then
              Inc(FFirstVisibleChar, 50)
            else
              if TextWidth(FFirstVisibleChar + 10 - 1, (MarkedPosition - FFirstVisibleChar + 10) + 1) >
                LEditRect.Width then
                Inc(FFirstVisibleChar, 10)
              else
                Inc(FFirstVisibleChar);
      end;
      //Checking do we need to decrease FFirstVisibleChar
      if FFirstVisibleChar > 1 then
        while (TextWidth(FFirstVisibleChar - 2, MarkedPosition - FFirstVisibleChar + 2) < LEditRect.Width)
          and (FFirstVisibleChar > 1) do
          Dec(FFirstVisibleChar);
    end;
  end;
  if (FFirstVisibleChar > 0) and (FTextLayout.Text.Length > 0) then
    if FTextLayout.Text.Chars[FFirstVisibleChar - 1].IsLowSurrogate then
      Inc(FFirstVisibleChar);
end;

procedure TCustomEdit.UpdateLayoutSize;
var
  LSize: TPointF;
begin
  LSize := ClosePolygon;
  if LastTextSettings.HorzAlign <> TTextAlign.taLeading then
    LSize.X := ContentRect.Width;
  FTextLayout.MaxSize := LSize;
end;

function TCustomEdit.GetPasswordCharWidth: Single;
begin
  Result := LastTextSettings.Font.Size / 2;
end;

function TCustomEdit.TextWidth(const AStart, ALength: Integer): Single;
var
  Rgn: TRegion;
  S, L, I: Integer;
begin
  if FPassword then
    Result := GetPasswordCharWidth * ALength
  else
    if AStart < FTextLayout.Text.Length then
    begin
      S := AStart;
      L := ALength;
      if FTextLayout.Text.Chars[S].IsLowSurrogate then
      begin
        Inc(S);
        Dec(L);
      end;
      Rgn := FTextLayout.RegionForRange(TTextRange.Create(S, L));
      Result := 0;
      for I := Low(Rgn) to High(Rgn) do
        Result := Result + Rgn[I].Width;
    end
    else
      if AStart = FTextLayout.Text.Length then
        Result := FTextLayout.TextWidth
      else
        Result := 0;
end;

function TCustomEdit.GetCoordinatePosition(x: Single): Integer;
var
  TmpX, WholeTextWidth, EditRectWidth: Single;
  T, Str, StrA: string;
begin
  Result := FFirstVisibleChar - 1;
  T := FTextService.CombinedText;

  if T.Length = 0 then
    Exit;

  if FPassword then
    WholeTextWidth := T.Length * GetPasswordCharWidth
  else
    WholeTextWidth := TextWidth(0, T.Length);

  EditRectWidth := FContent.Padding.PaddingRect(ContentRect).Width;
  TmpX := x;
  if WholeTextWidth < EditRectWidth then
    case LastTextSettings.HorzAlign of
      TTextAlign.taTrailing:
        TmpX := x - (EditRectWidth - WholeTextWidth);
      TTextAlign.taCenter:
        TmpX := x - ((EditRectWidth - WholeTextWidth) / 2);
    end;

  if FPassword then
  begin
    Result := Result + Trunc((TmpX - ContentRect.Left) / GetPasswordCharWidth);
    if Result < 0 then
      Result := 0
    else
      if Result > T.Length then
        Result := T.Length;
  end
  else
  begin
    TmpX := TmpX - ContentRect.Left;
    StrA := T.Substring(FFirstVisibleChar - 1, Result - FFirstVisibleChar + 1);
    Str := T.Substring(FFirstVisibleChar - 1, Result - FFirstVisibleChar + 2);
    while (TextWidth(0, StrA.Length) < TmpX) and (Result < T.Length) do
    begin
      if (TmpX > TextWidth(0, StrA.Length) + ((TextWidth(0, Str.Length) - TextWidth(0, StrA.Length)) / 2)) and (TmpX < TextWidth(0, Str.Length)) then
      begin
        Result := Result + 1;
        Break;
      end;
      if TmpX < TextWidth(0, Str.Length) then
        Break;
      Result := Result + 1;
      StrA := Str;
      Str := T.Substring(FFirstVisibleChar - 1, Result - FFirstVisibleChar + 2);
    end;
  end;
end;

function TCustomEdit.GetCharX(a: Integer): Single;
var
  WholeTextWidth: Single;
  EditRectWidth: Single;
  T: string;
  Rgn: TRegion;
begin
  if Assigned(FContent) then
  begin
    if FPassword then
    begin
      T := FTextService.CombinedText;
      WholeTextWidth := T.Length * GetPasswordCharWidth;
      Result := ContentRect.Left;
      if a > 0 then
      begin
        if FPassword then
        begin
          if a <= T.Length then
            Result := Result + (a - FFirstVisibleChar + 1) * GetPasswordCharWidth
          else
            Result := Result + (T.Length - FFirstVisibleChar + 1) * GetPasswordCharWidth;
        end
      end;
      EditRectWidth := FContent.Padding.PaddingRect(ContentRect).Width;
      if WholeTextWidth < EditRectWidth then
        case LastTextSettings.HorzAlign of
          TTextAlign.taTrailing:
            Result := Result + (EditRectWidth - WholeTextWidth);
          TTextAlign.taCenter:
            Result := Result + ((EditRectWidth - WholeTextWidth) / 2);
        end;
    end
    else
    begin
      Rgn := FTextLayout.RegionForRange(TTextRange.Create(0, 1));
      if Length(Rgn) > 0 then
        Result := Rgn[0].Left
      else
        Result := 0;
      Rgn := FTextLayout.RegionForRange(TTextRange.Create(FFirstVisibleChar - 1, a - FFirstVisibleChar + 1));
      if Length(Rgn) > 0 then
        Result := Result + Rgn[High(Rgn)].Width
      else
        Result := Result + FTextLayout.TopLeft.X;
      EditRectWidth := FContent.Padding.PaddingRect(ContentRect).Width;
      WholeTextWidth := ContentRect.Width;
      if WholeTextWidth < EditRectWidth then
        case LastTextSettings.HorzAlign of
          TTextAlign.taTrailing:
            Result := Result + (EditRectWidth - WholeTextWidth);
          TTextAlign.taCenter:
            Result := Result + ((EditRectWidth - WholeTextWidth) / 2);
        end;
    end;
    Result := Result + FContent.Padding.Left;
  end
  else
    Result := 0;
end;

function TCustomEdit.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

function TCustomEdit.GetReturnKeyType: TReturnKeyType;
begin
  Result := FReturnKeyType;
end;

function TCustomEdit.GetSelStart: Integer;
begin
  if FSelLength > 0 then
    Result := FSelStart
  else
    if FSelLength < 0 then
      Result := FSelStart + FSelLength
    else
      Result := GetOriginCaretPosition;
end;

function TCustomEdit.GetSelRect: TRectF;
var
  Offset: Integer;
begin
  Result := ContentRect;
  Result.Top := Trunc(LineTop);
  Result.Bottom := Result.Top + LineHeight;
  if GetOriginCaretPosition <= SelStart then
    Offset := FTextService.CombinedText.Length - FTextService.Text.Length
  else
    Offset := 0;
  Result.Left := GetCharX(SelStart + Offset);
  Result.Right := GetCharX(SelStart + SelLength + Offset);
end;

function TCustomEdit.GetSelectionBounds: TRect;
begin
  Result := Rect(FSelStart, 0, FSelStart + FSelLength, 0);
end;

function TCustomEdit.GetSelLength: Integer;
begin
  Result := Abs(FSelLength);
end;

function TCustomEdit.GetSelText: string;
begin
  Result := Text.Substring(SelStart, SelLength);
end;

procedure TCustomEdit.SetSelectionMode(const AValue: TSelectionMode);
begin
  FSelectionMode := AValue;
  if Assigned(FLoupeService) then
    case AValue of
      TSelectionMode.smNone: ;
      TSelectionMode.smTextSelection:
        FLoupeService.SetLoupeMode(TLoupeMode.lmRectangle);
      TSelectionMode.smCursorPosChanging:
        FLoupeService.SetLoupeMode(TLoupeMode.lmCircle);
    end;
end;

procedure TCustomEdit.SetSelLength(const Value: Integer);
begin
  if FSelLength <> Value then
  begin
    FSelLength := Value;
    UpdateSelectionPointPositions;
    RepaintEdit;
  end;
end;

procedure TCustomEdit.SetSelStart(const Value: Integer);
begin
  if FSelStart <> Value then
  begin
    SelLength := 0;
    FSelStart := Value;
    UpdateSelectionPointPositions;
    RepaintEdit;
  end;
end;

procedure TCustomEdit.SetCaretPosition(const Value: Integer);
var
  P: TPoint;
  Pos: TPointF;
  CaretHeight: single;
begin
  if Assigned(FTextService) then
  begin
    P.X := 0; P.Y := 0;
    if Value < 0 then
      P.X := 0
    else
      if Value > Text.Length then
        P.X := Text.Length
      else
        P.X := Value;
    FTextService.CaretPosition := P;

    UpdateFirstVisibleChar;

    if SelLength <= 0 then
      FSelStart := Value;

    RepaintEdit;

    if IsFocused then
    begin
      {$IFNDEF IOS}
      CaretHeight := Trunc(LineHeight);
      Pos.Y := Trunc(LineTop);
      {$ELSE}
      CaretHeight := Trunc(ContentRect.Height);
      Pos.Y := Trunc(ContentRect.Top);
      {$ENDIF}
      if FTextService.HasMarkedText then
        Pos.X := GetCharX(FTextService.TargetClausePosition.X)
      else
        Pos.X := GetCharX(FTextService.CaretPosition.X);
      Pos.X := Min(Pos.X, ContentRect.Right - Caret.Size.cx + 1);
      Caret.BeginUpdate;
      try
        Caret.Pos := Pos;
        Caret.Size := PointF(Caret.Size.cx, CaretHeight);
      finally
        Caret.EndUpdate;
      end;
    end;
  end;
end;

procedure TCustomEdit.SetCheckSpelling(const Value: Boolean);
var
  I: Integer;
begin
  if FCheckSpelling <> Value then
  begin
    FCheckSpelling := Value;
    if FCheckSpelling then
    begin
      if not TPlatformServices.Current.SupportsPlatformService(IFMXSpellCheckerService, IInterface(FSpellService)) then
        FSpellService := nil;
      FUpdateSpelling := not Text.IsEmpty;
    end
    else
    begin
      for I := 0 to FSpellMenuItems.Count - 1 do
        FSpellMenuItems[I].Parent := nil;
      FSpellMenuItems.Clear;
      FSpellService := nil;
      SetLength(FSpellingRegions, 0);
      FUpdateSpelling := False;
      FSpellHightlightRect := TRectF.Empty;
    end;
  end;
end;

function TCustomEdit.GetCaretPosition: Integer;
begin
  if Assigned(FTextService) then
    Result := FTextService.TargetClausePosition.X
  else
    Result := -1;
end;

procedure TCustomEdit.SetLoupePosition(
  const ASelectionPointType: TSelectionPointType);
var
  SelectionRect: TRectF;
  ZoomCenter: TPointF;
  LoupePos: TPointF;
begin
  SelectionRect := GetSelRect;
  if Assigned(FLoupeService) then
  begin
    case ASelectionPointType of
      TSelectionPointType.sptLeft:
      begin
        ZoomCenter := PointF(SelectionRect.Left, SelectionRect.Top + SelectionRect.Height / 2);
        LoupePos := SelectionRect.TopLeft + PointF(-FLoupeService.GetWidth / 2, -FLoupeService.GetHeight) + PointF(0, -LOUPE_OFFSET);
      end;
      TSelectionPointType.sptRight:
      begin
        ZoomCenter := PointF(SelectionRect.Right, SelectionRect.Top + SelectionRect.Height / 2);
        LoupePos := PointF(SelectionRect.Right, SelectionRect.Top) + PointF(-FLoupeService.GetWidth / 2, -FLoupeService.GetHeight) + PointF(0, -LOUPE_OFFSET);
      end;
    end;
    ZoomCenter := LocalToAbsolute(ZoomCenter);
    LoupePos := LocalToAbsolute(LoupePos);

    FLoupeService.SetPosition(LoupePos);
    FLoupeService.SetZoomRegionCenter(ZoomCenter);
  end;
end;

procedure TCustomEdit.SetMaxLength(const Value: Integer);
begin
  if FMaxLength <> Value then
  begin
    FMaxLength := Value;
  end;
end;

procedure TCustomEdit.CopyToClipboard;
begin
  if not Assigned(FClipboardSvc) or Password then
    Exit;

  if InputSupport and not SelText.IsEmpty then
    FClipboardSvc.SetClipboard(SelText);

  if not InputSupport and not Text.IsEmpty then
    FClipboardSvc.SetClipboard(Text);
end;

procedure TCustomEdit.PasteFromClipboard;
begin
  if ReadOnly or not InputSupport then
    Exit;
  if Observers.IsObserving(TObserverMapping.EditLinkID) then
    if not TLinkObservers.EditLinkEdit(Observers) then
    begin
      TLinkObservers.EditLinkReset(Observers);
      Exit;
    end
    else
      TLinkObservers.EditLinkModified(Observers);
  if Observers.IsObserving(TObserverMapping.ControlValueID) then
    TLinkObservers.ControlValueModified(Observers);

  if Assigned(FClipboardSvc) and (not FClipboardSvc.GetClipboard.IsEmpty) then
  begin
    InsertText(FClipboardSvc.GetClipboard.ToString.Split([sLineBreak], 1, TStringSplitOptions.None)[0]);
    DoChangeTracking;
  end;
end;

procedure TCustomEdit.RepaintEdit;
begin
  if Assigned(FContent) then
    FContent.Repaint;
end;

procedure TCustomEdit.Replace(const AStartPos, ALength: Integer;
  const AStr: string);
begin

end;

procedure TCustomEdit.ResetSelection;
begin
  CaretPosition := SelStart;
  SelLength := 0;
end;

procedure TCustomEdit.DeleteSelection;
var
  TmpS: string;
begin
  if ReadOnly or not InputSupport or (SelLength = 0) then
    Exit;

  TmpS := Text;
  // FActionStack.FragmentDeleted(SelStart+1, Copy(TmpS,SelStart+1,SelLength));
  TmpS := TmpS.Remove(SelStart, SelLength);
  SetTextInternal(TmpS);
  CaretPosition := SelStart;
  SelLength := 0;
end;

procedure TCustomEdit.CutToClipboard;
begin
  // if PasswordKind = pkNone then

  if Observers.IsObserving(TObserverMapping.EditLinkID) then
    if not TLinkObservers.EditLinkEdit(Observers) then
    begin
      TLinkObservers.EditLinkReset(Observers);
      Exit;
    end
    else
      TLinkObservers.EditLinkModified(Observers);
  if Observers.IsObserving(TObserverMapping.ControlValueID) then
    TLinkObservers.ControlValueModified(Observers);

  CopyToClipboard;
  DeleteSelection;
  DoChangeTracking;
end;

procedure TCustomEdit.SelectAll;
begin
  if not InputSupport then
    Exit;

  SelStart := 0;
  SelLength := Text.Length;
  SetCaretPosition(Text.Length);
  RepaintEdit;
end;

procedure TCustomEdit.SelectWord;
var
  SelEnd: Integer;
begin
  if Text.Length = 0 then
    Exit;

  if FindWordBound(Text, GetOriginCaretPosition, FSelStart, SelEnd) then
    SelLength := Max(SelEnd - FSelStart + 1, 0)
  else
    SelLength := 0;
end;

procedure TCustomEdit.KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
var
  TmpS: string;
  OldCaretPosition: Integer;
  LCaret: Integer;
  IsCtrlOrCmd: Boolean;
begin
  if not FInputSupport then
    Exit;

  if Observers.IsObserving(TObserverMapping.EditLinkID) then
  begin
    if (Key = vkBack) or (Key = vkDelete) or ((Key = vkInsert) and (ssShift in Shift)) then
      if not TLinkObservers.EditLinkEdit(Observers) then
      begin
        TLinkObservers.EditLinkReset(Observers);
        Exit;
      end;

    if (KeyChar >= #32) and
      not TLinkObservers.EditLinkIsValidChar(Observers, KeyChar) then
    begin
//      MessageBeep(0);
      KeyChar := #0;
      Exit;
    end;
    case KeyChar of
      ^H, ^V, ^X, #32..High(Char):
        if not TLinkObservers.EditLinkEdit(Observers) then
        begin
          KeyChar := #0;
          TLinkObservers.EditLinkReset(Observers);
          Exit;
        end;
      #27:
        begin
          TLinkObservers.EditLinkReset(Observers);
          SelectAll;
          KeyChar := #0;
          Exit;
        end;
    end;

    if TLinkObservers.EditLinkIsEditing(Observers) then
      TLinkObservers.EditLinkModified(Observers);
  end;
  if Observers.IsObserving(TObserverMapping.ControlValueID) then
    TLinkObservers.ControlValueModified(Observers);

  inherited;
  OldCaretPosition := GetOriginCaretPosition;
  FLastChar := KeyChar;
  FLastKey := Key;
  IsCtrlOrCmd := (Shift = [ssCtrl]) or (Shift = [ssCommand]);
  case Key of
    Ord('A'):
      if IsCtrlOrCmd then
        SelectAll;
    Ord('C'):
      if IsCtrlOrCmd then
        CopyToClipboard;
    Ord('V'):
      if IsCtrlOrCmd then
      begin
        PasteFromClipboard;
        DoTyping;
      end;
    Ord('X'):
      if IsCtrlOrCmd and not ReadOnly then
      begin
        CutToClipboard;
        DoTyping;
      end;
    Ord('Z'):
      if IsCtrlOrCmd then
      begin
        if Observers.IsObserving(TObserverMapping.EditLinkID) then
          TLinkObservers.EditLinkReset(Observers);
        if Observers.IsObserving(TObserverMapping.ControlValueID) then
          TLinkObservers.ControlValueUpdate(Observers);
      end;
    vkReturn:
      begin
        Typing:= False;
        Change;
        if Observers.IsObserving(TObserverMapping.EditLinkID) then
          TLinkObservers.EditLinkUpdate(Observers);
        if Observers.IsObserving(TObserverMapping.ControlValueID) then
          TLinkObservers.ControlValueUpdate(Observers);
        if KillFocusByReturn and Assigned(FRoot) then
          FRoot.SetFocused(nil);
      end;
    vkEnd:
      CaretPosition := Text.Length;
    vkHome:
      CaretPosition := 0;
    vkLeft:
      if IsCtrlOrCmd then
        CaretPosition := GetPrevLexemeBegin(Text, GetOriginCaretPosition)
      else
        if (GetOriginCaretPosition > 0) and not Text.IsEmpty then
          if Text.Chars[GetOriginCaretPosition].IsHighSurrogate then
            CaretPosition := GetOriginCaretPosition - 2
          else
            CaretPosition := GetOriginCaretPosition - 1;
    vkRight:
      if IsCtrlOrCmd then
        CaretPosition := GetNextLexemeBegin(Text, GetOriginCaretPosition)
      else
        if (Text.Length > GetOriginCaretPosition) then
          if Text.Chars[GetOriginCaretPosition].IsHighSurrogate then
            CaretPosition := GetOriginCaretPosition + 2
          else
            CaretPosition := GetOriginCaretPosition + 1;
    vkDelete:
      if not ReadOnly and InputSupport then
      begin
        if SelLength <> 0 then
        begin
          if Shift = [ssShift] then
            CutToClipboard
          else
            DeleteSelection;
          DoTyping;
        end
        else
        begin
          TmpS := Text;
          if not TmpS.IsEmpty then
          begin
            if IsCtrlOrCmd then
            begin
              //Delete whole word
              LCaret := GetNextLexemeBegin(Text, GetOriginCaretPosition);
              if LCaret < 0 then
                Exit;
              TmpS := TmpS.Remove(LCaret, GetOriginCaretPosition - LCaret);
            end
            else
            begin
              LCaret := GetOriginCaretPosition;
              //Delete single character
              if (Text.Length > 1) and (GetOriginCaretPosition < Text.Length) and Text.Chars[GetOriginCaretPosition].IsHighSurrogate then
                TmpS := TmpS.Remove(GetOriginCaretPosition, 2)
              else
                TmpS := TmpS.Remove(GetOriginCaretPosition, 1);
            end;
            SetTextInternal(TmpS);
            CaretPosition := LCaret;
            DoTyping;
          end;
        end;
      end;
    vkBack:
      if not ReadOnly and InputSupport then
      begin
        if SelLength <> 0 then
        begin
          DeleteSelection;
          DoTyping;
        end
        else
        begin
          TmpS := Text;
          if not TmpS.IsEmpty then
          begin
            if IsCtrlOrCmd then
            begin
              //Delete whole word
              LCaret := GetPrevLexemeBegin(Text, GetOriginCaretPosition);
              if LCaret < 0 then
                Exit;
              TmpS := TmpS.Remove(LCaret, GetOriginCaretPosition - LCaret);
              SetTextInternal(TmpS);
            end
            else
            begin
              LCaret := GetOriginCaretPosition - 1;
              if TmpS.Chars[GetOriginCaretPosition - 1].IsLowSurrogate then
                TmpS := TmpS.Remove(GetOriginCaretPosition - 2, 2)
              else
                TmpS := TmpS.Remove(GetOriginCaretPosition - 1, 1);
              SetTextInternal(TmpS);
            end;
            CaretPosition := LCaret;
            DoTyping;
          end;
        end;
      end;
    vkInsert:
      if Shift = [ssShift] then
      begin
        PasteFromClipboard;
        DoTyping;
      end
      else
        if ([ssCtrl, ssCommand] * Shift) <> [] then
          CopyToClipboard;
  end;

  if (KeyChar <> #0) and (FFilterChar <> '') and not FFilterChar.Contains(KeyChar) then
    KeyChar := #0;

  if Key in [vkEnd, vkHome, vkLeft, vkRight] then
  begin
    if ssShift in Shift then
    begin
      if SelLength = 0 then
        FSelStart := OldCaretPosition;
      FSelStart := GetOriginCaretPosition;
      SelLength := FSelLength - (GetOriginCaretPosition - OldCaretPosition);
    end
    else
      SelLength := 0;
    RepaintEdit;
  end;

  if (Ord(KeyChar) >= 32) and (KeyChar <> #0) and not ReadOnly and InputSupport then
  begin
    FCharsBuffer := FCharsBuffer + KeyChar;
    if not KeyChar.IsHighSurrogate then
    begin
      Typing:= True;
      InsertText(FCharsBuffer);
      FCharsBuffer := EmptyStr;
      DoTyping;
    end;
    KeyChar := #0;
  end;
  if Assigned(ResourceControl) then
    ResourceControl.UpdateEffects;
end;

procedure TCustomEdit.Loaded;
begin
  inherited;
  FTextSettingsInfo.Design := csDesigning in ComponentState;
end;

procedure TCustomEdit.LongTap(const X, Y: Single);
begin
  if SelectionMode <> TSelectionMode.smTextSelection then
  begin
    SelectionMode := TSelectionMode.smCursorPosChanging;
    if Assigned(FLoupeService) then
    begin
      FLoupeService.SetLoupeMode(TLoupeMode.lmCircle);
      ShowLoupe;
      SetLoupePosition(X, Y);
    end;
  end;
end;

procedure TCustomEdit.DblTap;
begin
  SelectWord;
end;

function TCustomEdit.GetTextService: TTextService;
begin
  Result := FTextService;
end;

procedure TCustomEdit.GoToTextBegin;
begin
  CaretPosition := 0;
end;

procedure TCustomEdit.GoToTextEnd;
begin
  CaretPosition := Text.Length;
end;

function TCustomEdit.HasSelection: Boolean;
begin
  Result := SelLength > 0;
end;

function TCustomEdit.HasText: Boolean;
begin
  Result := Text.Length > 0;
end;

function TCustomEdit.HaveSelectionPickers: Boolean;
begin
  Result := Assigned(FLeftSelPt) and Assigned(FRightSelPt);
end;

procedure TCustomEdit.HideLoupe;
begin
  if Assigned(FLoupeService) then
    FLoupeService.Hide;
end;

procedure TCustomEdit.HighlightSpell;
var
  BP, EP: Integer;
  Rgn: TRegion;
  R: TRectF;
begin
  if not Text.IsEmpty and (GetOriginCaretPosition >= 0) then
    if FMX.Text.FindWordBound(Text, GetOriginCaretPosition, BP, EP) then
    begin
      Rgn := FTextLayout.RegionForRange(TTextRange.Create(BP, EP - BP + 1));
      if Length(Rgn) > 0 then
      begin
        FSpellHightlightRect := Rgn[0];
        R := ContentRect;
        FSpellHightlightRect.Offset(-R.Left, -R.Top);
      end
      else
        FSpellHightlightRect := TRectF.Empty;
    end;
  FCaret.TemporarilyHidden := True;
  RepaintEdit;
end;

function LinkObserversValueModified(const AObservers: TObservers): Boolean;
begin
  Result := True;
  if AObservers.IsObserving(TObserverMapping.EditLinkID) then
  begin
    Result := TLinkObservers.EditLinkEdit(AObservers);
    if Result then
      TLinkObservers.EditLinkModified(AObservers);
  end;
  if Result and AObservers.IsObserving(TObserverMapping.ControlValueID) then
    TLinkObservers.ControlValueModified(AObservers);
end;

procedure TCustomEdit.UpdateCaretPoint;
var
  CombinedText: string;
  TextChanged: Boolean;
begin
  SelLength := 0;
  CombinedText := FTextService.CombinedText;
  TextChanged := not SameText(FTextLayout.Text, CombinedText);
  FTextLayout.Text := CombinedText;
  SetCaretPosition(GetOriginCaretPosition);
  if Assigned(FPrompt) then
    FPrompt.Visible := CombinedText.Length = 0;
  if TextChanged then
  begin
    LinkObserversValueModified(Self.Observers);
                                                                                             
    DoChangeTracking;
  end;
end;

function TCustomEdit.GetTargetClausePointF: TPointF;
var
  Str: String;
begin
  Str := FTextService.CombinedText.Substring(0, Round(FTextService.TargetClausePosition.X) );
  if FFirstVisibleChar > 1 then
    Str := Str.Substring(FFirstVisibleChar - 1, MaxInt);
  Result.X := TextWidth(0, Str.Length);
  Result.Y := (ContentRect.Height / 2) + LastTextSettings.Font.Size / 2 + 2;  // 2 is small space between conrol and IME window
  Result.X := Result.X + ContentRect.Top;
  Result.Y := Result.Y + ContentRect.Left;
  Result := LocalToAbsolute(Result);
end;

function TCustomEdit.GetImeMode: TImeMode;
begin
  Result := FImeMode;
end;

function TCustomEdit.GetItem(const AIndex: Integer): TFmxObject;
begin
  Result := FButtonsContent.Children[AIndex];
end;

function TCustomEdit.GetItemsCount: Integer;
begin
  Result := FButtonsContent.ChildrenCount;
end;

procedure TCustomEdit.SetImeMode(const Value: TImeMode);
begin
  FImeMode := Value;
  if Password then
    FTextService.SetImeMode(TImeMode.imDisable)
  else
    FTextService.SetImeMode(FImeMode);
end;

procedure TCustomEdit.SetInputSupport(const Value: Boolean);
begin
  if FInputSupport <> Value then
  begin
    FInputSupport := Value;
    Caret.Visible := FInputSupport;
  end;
end;

procedure TCustomEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if (Button = TMouseButton.mbLeft) and FInputSupport then
  begin
    CaretPosition := GetCoordinatePosition(X);
    SelLength := 0;
    if ssDouble in Shift then
    begin
      SelectionMode := TSelectionMode.smNone;
      SelectWord;
    end
    else
    {$IF not Defined(IOS) and not Defined(ANDROID)}
      BeginSelection;
    {$ENDIF}
  end;
end;

procedure TCustomEdit.MouseMove(Shift: TShiftState; X, Y: Single);

  function DefineNewCarretPosition(const AX: Single): Integer;
  begin
    Result := GetCoordinatePosition(AX);
    if AX > ContentRect.Right then
      Inc(Result);
  end;

var
  OldCaretPosition: Integer;
begin
  inherited;
  { Changing cursor position }
  if SelectionMode = TSelectionMode.smCursorPosChanging then
  begin
    if Assigned(FLoupeService) then
    begin
      FLoupeService.SetLoupeMode(TLoupeMode.lmCircle);
      SetLoupePosition(X, Y);
      ShowLoupe;
    end;
    CaretPosition := DefineNewCarretPosition(X);
  end;
  { Changing selection bounds }
  if SelectionMode = TSelectionMode.smTextSelection then
  begin
    OldCaretPosition := GetOriginCaretPosition;
    if SelLength = 0 then
      FSelStart := OldCaretPosition;
    {$IFNDEF ANDROID}
    FSelStart := GetOriginCaretPosition;
    CaretPosition := DefineNewCarretPosition(X);
    {$ELSE}
    FSelStart := DefineNewCarretPosition(X);
    {$ENDIF}
    SelLength := FSelLength - (FSelStart - OldCaretPosition);
  end;
end;

procedure TCustomEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; x, Y: Single);
begin
  inherited;
  HideLoupe;
  if SelectionMode = TSelectionMode.smCursorPosChanging then
    FTextService.EndSelection;
  SelectionMode := TSelectionMode.smNone;
  UpdateSelectionPointPositions;
end;

procedure TCustomEdit.BeginSelection;
begin
  SelectionMode := TSelectionMode.smTextSelection;
  FTextService.BeginSelection;
end;

function TCustomEdit.CanObserve(const ID: Integer): Boolean;
begin
  Result := False;
  if ID = TObserverMapping.EditLinkID then
    Result := True;
  if ID = TObserverMapping.ControlValueID then
    Result := True;
end;

procedure TCustomEdit.ObserverAdded(const ID: Integer; const Observer: IObserver);
begin
  if ID = TObserverMapping.EditLinkID then
    Observer.OnObserverToggle := ObserverToggle;
end;

procedure TCustomEdit.ObserverToggle(const AObserver: IObserver; const Value: Boolean);
var
  LEditLinkObserver: IEditLinkObserver;
  LSaveReadOnly: Boolean;
begin
  if Supports(AObserver, IEditLinkObserver, LEditLinkObserver) then
  begin
    if Value then
    begin
      LSaveReadOnly := ReadOnly;
      if LEditLinkObserver.IsReadOnly then
        ReadOnly := True;
      FSaveReadOnly := LSaveReadOnly;
    end
    else
      if not (csDestroying in ComponentState) then
        ReadOnly := FSaveReadOnly;
  end;
end;

procedure TCustomEdit.Change;
begin
  if FNeedChange then
  begin
    FNeedChange := False;
    if Assigned(FOnChange) and (not (csLoading in ComponentState)) then
      FOnChange(Self);
  end;
end;

procedure TCustomEdit.ClearSelection;
begin
  DeleteSelection;
end;

procedure TCustomEdit.CMGesture(var EventInfo: TGestureEventInfo);
begin
  if EventInfo.GestureID = igiLongTap then
    LongTap(EventInfo.Location.X, EventInfo.Location.Y)
  else if EventInfo.GestureID = igiDoubleTap then
    DblTap
  else
    inherited;
end;

procedure TCustomEdit.ContextMenu(const ScreenPosition: TPointF);
begin
  if Assigned(PopupMenu) then
    inherited ContextMenu(ScreenPosition)
  else
    if not (csDesigning in ComponentState) then
    begin
      UpdatePopupMenuItems;
      if FCheckSpelling and Assigned(FSpellService) and (Length(FSpellingRegions) > 0) then
        UpdateSpellPopupMenu(ScreenToLocal(ScreenPosition));
      FEditPopupMenu.PopupComponent := Self;
      FEditPopupMenu.Popup(round(ScreenPosition.X), round(ScreenPosition.Y));
    end;
end;

procedure TCustomEdit.DoEnter;
begin
  inherited;
  FNeedChange := False;
  if not ReadOnly and InputSupport then
    SelectAll;
end;

procedure TCustomEdit.DoExit;
begin
  if Assigned(FScene) then
  begin
    Change;
    if Observers.IsObserving(TObserverMapping.EditLinkID) then
      TLinkObservers.EditLinkUpdate(Observers);
    if Observers.IsObserving(TObserverMapping.ControlValueID) then
      TLinkObservers.ControlValueUpdate(Observers);
    inherited;
    UpdateSelectionPointPositions;
  end
  else
    inherited;
end;

procedure TCustomEdit.DoInsertObject(Index: Integer; const AObject: TFmxObject);
var
  EditControl: IEditControl;
begin
  if AObject.GetInterface(IEditControl, EditControl) then
  begin
    if (Assigned(FButtonsContent.Children) and not FButtonsContent.ContainsObject(AObject)) or
       not Assigned(FButtonsContent.Children) then
      FButtonsContent.InsertObject(Index, AObject);
  end
  else
    inherited DoInsertObject(Index, AObject);
end;

procedure TCustomEdit.DoLeftSelPtMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  BeginSelection;
  SetLoupePosition(TSelectionPointType.sptLeft);
  ShowLoupe;
end;

function TCustomEdit.GetText: string;
begin
  Result := FTextService.CombinedText;
end;

procedure TCustomEdit.SetText(const Value: string);
begin
  if FTextService.Text <> Value then
  begin
    FTextLayout.Text := Value;
    FTextService.Text := Value;
    if FTextService.CaretPosition.X > Text.Length then
      SetCaretPosition(Text.Length);
    if not (csLoading in ComponentState) then
      DoChangeTracking;
    StylesData['prompt.Visible'] := Value.Length = 0;
    FNeedChange := True;
    SelStart := 0;
    SelLength := 0;
    Change;
    RepaintEdit;
  end;
end;

procedure TCustomEdit.SetPassword(const Value: Boolean);
begin
  if FPassword <> Value then
  begin
    FPassword := Value;
    if FPassword then FTextService.SetImeMode(TImeMode.imDisable)
    else
      FTextService.SetImeMode(Self.ImeMode);
    RepaintEdit;
  end;
end;

procedure TCustomEdit.SetPromptText(const Prompt: string);
begin
  StylesData['prompt.Text'] := Prompt;
end;

function TCustomEdit.GetPromptText: string;
begin
  Exit(StylesData['prompt.Text'].AsString);
end;

procedure TCustomEdit.SetReadOnly(AReadOnly: Boolean);
begin
  FSaveReadOnly := AReadOnly;
  FReadOnly := AReadOnly;
  Caret.ReadOnly := FReadOnly;
end;

procedure TCustomEdit.SetReturnKeyType(Value: TReturnKeyType);
begin
  FReturnKeyType := Value;
end;

procedure TCustomEdit.SetKeyboardType(Value: TVirtualKeyboardType);
begin
  FKeyboardType := Value;
end;

procedure TCustomEdit.SetLoupePosition(const X, Y: Single);
var
  LoupePos: TPointF;
  ZoomPos: TPointF;
begin
  if Assigned(FLoupeService) then
  begin
    LoupePos := PointF(X - FLoupeService.GetWidth / 2, Y - FLoupeService.GetHeight);
    LoupePos := LocalToAbsolute(LoupePos);
    ZoomPos := LocalToAbsolute(PointF(X, Y));
    FLoupeService.SetPosition(LoupePos);
    FLoupeService.SetZoomRegionCenter(ZoomPos);
    ShowLoupe;
  end;
end;

procedure TCustomEdit.StartIMEInput;
begin
  FTextService.CaretPosition := Point(GetOriginCaretPosition, 0);
end;

function TCustomEdit.FindContextMenuItem(const AItemName: string): TMenuItem;
var
  MenuObject: TFmxObject;
begin
  MenuObject := FEditPopupMenu.FindStyleResource(AItemName);
  if Assigned(MenuObject) and (MenuObject is TMenuItem) then
    Result := MenuObject as TMenuItem
  else
    Result := nil;
end;

procedure TCustomEdit.FreeStyle;
begin
  inherited FreeStyle;
  FContent := nil;
  FButtonsLayout := nil;
  FLeftSelPt := nil;
  FRightSelPt := nil;
  FLeftLayout := nil;
  FPrompt := nil;
end;

type
  TValueRangeCustomEditBox = class (TValueRange)
  private
    FCustomEditBox: TCustomEditBox;
    FValueChanged: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DoBeforeChange; override;
    procedure DoAfterChange; override;
    function NeedActionChange: boolean;
    property CustomEditBox: TCustomEditBox read FCustomEditBox;
  end;

{ TValueRangeCustomEditBox }

constructor TValueRangeCustomEditBox.Create(AOwner: TComponent);
begin
  if not (AOwner is TCustomEditBox) then
    raise EActionError.CreateFMT(SEUseHeirs, [TCustomEditBox.ClassName]);
  inherited;
  FCustomEditBox := TCustomEditBox(AOwner);
end;

function TValueRangeCustomEditBox.NeedActionChange: boolean;
begin
  Result := CustomEditBox.ActionClient and
           (not CustomEditBox.DefStored) and
           (not TValueRangeAction(CustomEditBox.Action).ValueRange.Changing);
end;

procedure TValueRangeCustomEditBox.DoAfterChange;
begin
  if FValueChanged then
  begin
    if CustomEditBox.FTextUpdating then
      CustomEditBox.Change
    else
    begin
      CustomEditBox.FTextUpdating := True;
      try
        CustomEditBox.Text := CustomEditBox.ConvertValueToText(Value);
        CustomEditBox.Change
      finally
        CustomEditBox.FTextUpdating := False;
      end;
      CustomEditBox.SelStart := 0;
      CustomEditBox.SelLength := 0;
    end;
    FValueChanged := False;
  end;
end;

procedure TValueRangeCustomEditBox.DoBeforeChange;
var Sigma: Single;
begin
  Sigma := Power(10, -CustomEditBox.FDecimalDigits)/2;
  if not SameValue(New.Value,
                   Value,
                   Sigma) then
    FValueChanged := True;
  if NeedActionChange then
    TValueRangeAction(CustomEditBox.Action).ValueRange.Assign(New);
  inherited;
end;

{ TCustomEditBox }

procedure TCustomEditBox.Change;
begin
  inherited;
  FNeedChange := False;
end;

constructor TCustomEditBox.Create(AOwner: TComponent);
begin
  inherited;
  FValueRange := TValueRangeCustomEditBox.Create(Self);
  FDefaultValueRange := TBaseValueRange.Create;
  FDecimalDigits := 2;
  FHorzIncrement := 1;
  ValueType := TNumValueType.vtInteger;
  FFilterChar := FilterCharByValueType(ValueType);
  Text := '0';
  FKeyboardType := TVirtualKeyboardType.vktNumberPad;
end;

destructor TCustomEditBox.Destroy;
begin
  FreeAndNil(FDefaultValueRange);
  FreeAndNil(FValueRange);
  inherited;
end;

procedure TCustomEditBox.AfterConstruction;
begin
  inherited;
  DefaultValueRange.Assign(FValueRange.New);
end;

function TCustomEditBox.GetData: TValue;
begin
  Result := Value;
end;

function TCustomEditBox.DefStored: boolean;
begin
  Result := not (ActionClient and
                 (Action is TCustomValueRangeAction));
end;

function TCustomEditBox.MaxStored: Boolean;
begin
  Result := DefStored and (not SameValue(Max, DefaultValueRange.Max));
end;

function TCustomEditBox.MinStored: Boolean;
begin
  Result := DefStored and (not SameValue(Min, DefaultValueRange.Min));
end;

function TCustomEditBox.ValueStored: Boolean;
begin
  Result := DefStored and (not SameValue(Value, DefaultValueRange.Value));
end;

function TCustomEditBox.GetValueRange: TCustomValueRange;
begin
  Result := FValueRange;
end;

procedure TCustomEditBox.SetValueRange(const AValue: TCustomValueRange);
begin
  FValueRange.Assign(AValue);
end;

procedure TCustomEditBox.SetValueRange_(const Value: TValueRange);
begin
  FValueRange.Assign(Value);
end;

function TCustomEditBox.GetActionLinkClass: TActionLinkClass;
begin
  Result := TValueRangeActionLink;
end;

procedure TCustomEditBox.ActionChange(Sender: TBasicAction;
  CheckDefaults: Boolean);
begin
  if Sender is TValueRangeAction then
  begin
    if (not CheckDefaults) or (FValueRange.IsEmpty) then
      FValueRange.Assign(TValueRangeAction(Sender).ValueRange);
  end;
  inherited;
end;

function TCustomEditBox.ConvertTextToValue(const AValue: string): Single;
var
  TempValue: Single;
begin
  if TryTextToValue(Text, TempValue, Value) then
    Result := TempValue
  else
    Result := Value;
end;

function TCustomEditBox.ConvertValueToText(const AValue: Single): string;
begin
  if (Frac(AValue) = 0) or (FValueType = TNumValueType.vtInteger) then
    Result := IntToStr(Trunc(AValue))
  else
    Result := FloatToStrF(AValue, ffFixed, 7 + FDecimalDigits, FDecimalDigits);
end;

function TCustomEditBox.HorzIncrementStored: Boolean;
begin
  Result := not SameValue(FHorzIncrement, 1);
end;

procedure TCustomEditBox.SetData(const Value: TValue);
begin
  if Value.IsType<TNotifyEvent> then
    OnChange := Value.AsType<TNotifyEvent>()
  else
    Self.Value := Value.AsExtended;
end;

procedure TCustomEditBox.DoExit;
begin
  if not FTextUpdating then
  begin
    FTextUpdating := True;
    try
      Value := ConvertTextToValue(Text);
      Text := ConvertValueToText(Value);
    finally
      FTextUpdating := False;
    end;
  end;
  inherited;
end;

function TCustomEditBox.CurrInc: single;
begin
  Result := System.Math.Max(HorzIncrement, ValueRange.Frequency);
  if ValueType = TNumValueType.vtInteger then
    Result := System.Math.Max(1, Round(Result));
end;

procedure TCustomEditBox.ValueDec;
begin
  ValueRange.Value := ConvertTextToValue(Text);
  ValueRange.Increment := CurrInc;
  ValueRange.Dec;
end;

procedure TCustomEditBox.ValueInc;
begin
  ValueRange.Value := ConvertTextToValue(Text);
  ValueRange.Increment := CurrInc;
  ValueRange.Inc;
end;

procedure TCustomEditBox.KeyDown(var Key: Word; var KeyChar: Char;
  Shift: TShiftState);

  function IsDuplicateDecimalSeparator(AKeyChar: Char): Boolean;
  begin
    Result := ((AKeyChar = '.') or (AKeyChar = ',') or (AKeyChar = FormatSettings.DecimalSeparator)) and
              (Text.Contains(',') or Text.Contains('.') or Text.Contains(FormatSettings.DecimalSeparator));
  end;

  function IsInvalidSign: Boolean;
  begin
    Result := (SelStart <> 0) and ((KeyChar = '+') or (KeyChar = '-'));
  end;

begin
  case Key of
    vkUp:
    begin
      ValueInc;
      KeyChar := #0;
      Key := 0;
    end;
    vkDown:
    begin
      ValueDec;
      KeyChar := #0;
      Key := 0;
    end;
  else
    if IsDuplicateDecimalSeparator(KeyChar) or IsInvalidSign then
      Exit;
    inherited;
  end;
end;

procedure TCustomEditBox.Loaded;
begin
  if FValueRange.IsChanged then
    FValueRange.Changed(True);
  inherited;
end;

procedure TCustomEditBox.MouseWheel(Shift: TShiftState; WheelDelta: Integer;
  var Handled: Boolean);
begin
  inherited;
  if WheelDelta > 0 then
    ValueInc
  else
    if WheelDelta < 0 then
      ValueDec;
  Handled := True;
end;

procedure TCustomEditBox.SetDecimalDigits(const ADecimalDigits: Integer);
begin
  if (FDecimalDigits <> ADecimalDigits) and (ADecimalDigits >= 0) then
  begin
    FDecimalDigits := ADecimalDigits;
    GetTextService.Text := ConvertValueToText(Value);
    RepaintEdit;
  end;
end;

function TCustomEditBox.GetMax: Single;
begin
  Result := FValueRange.Max;
end;

procedure TCustomEditBox.SetMax(const AValue: Single);
begin
  FValueRange.Max := AValue;
end;

function TCustomEditBox.GetMin: Single;
begin
  Result := FValueRange.Min;
end;

procedure TCustomEditBox.SetMin(const AValue: Single);
begin
  FValueRange.Min := AValue;
end;

function TCustomEditBox.GetValue: Single;
begin
  Result := FValueRange.Value;
end;

procedure TCustomEditBox.SetValue(const AValue: Single);
begin
  FValueRange.Value := AValue;
end;

procedure TCustomEditBox.SetText(const AText: string);
var
  TempValue: Single;
begin
  if AText <> Text then
  begin
    if not FTextUpdating then
    begin
      FTextUpdating := True;
      try
        if TryTextToValue(AText, TempValue, Value) then
          Value := TempValue;
        inherited;
        Repaint;
      finally
        FTextUpdating := False;
      end;
    end
    else
      inherited;
  end;
end;

procedure TCustomEditBox.SetValueType(const AValueType: TNumValueType);
begin
  if FValueType <> AValueType then
  begin
    FValueType := AValueType;
    FFilterChar := FilterCharByValueType(FValueType);
    GetTextService.Text := ConvertValueToText(Value);
    RepaintEdit;
  end;
end;

{ TNumberBox }

constructor TNumberBox.Create(AOwner: TComponent);
begin
  inherited;
  VertIncrement := 5;
  AutoCapture := True;
  KeyboardType := TVirtualKeyboardType.vktNumberPad;
end;

procedure TNumberBox.SetText(const AText: string);
var
  OldText: string;
begin
  OldText := Text;
  inherited;
  if not (csLoading in ComponentState) and not FPressed and (OldText <> Text) then
    Change;
end;

procedure TNumberBox.SetValue(const AValue: Single);
var
  OldValue: Single;
begin
  OldValue := Value;
  inherited;
  if not (csLoading in ComponentState) and not FPressed and not SameValue(OldValue, Value) then
    Change;
end;

procedure TNumberBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if Button = TMouseButton.mbLeft then
  begin
    FPressed := True;
    FPressedPos := PointF(X, Y);
    FPressedVert := False;
  end;
end;

procedure TNumberBox.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if FPressed then
  begin
    if Abs(x - FPressedPos.X) >= Abs(Y - FPressedPos.Y) then
    begin
      { horz }
      if x > FPressedPos.x then
        ValueInc
      else
      if x < FPressedPos.x then
        ValueDec;
      FPressedVert := False;
    end
    else
    begin
      { vert }
      if VertIncrement > 0 then
      begin
        ValueRange.Increment := VertIncrement;
        if Y < FPressedPos.Y then
          ValueRange.Inc
        else
        if Y > FPressedPos.Y then
          ValueRange.Dec;
      end;
      FPressedVert := True;
    end;
    FPressedPos := PointF(X, Y);
  end;
end;

procedure TNumberBox.MouseUp(Button: TMouseButton; Shift: TShiftState; x, Y: Single);
begin
  inherited;
  if FPressed then
  begin
    FPressed := False;
    Change;
    Repaint;
  end;
end;

procedure TNumberBox.PaintChildren;
var
  R: TRectF;
  Fill: TBrush;
  LHidden: boolean;
begin
  LHidden := Caret.TemporarilyHidden;
  if FPressed then
    Caret.TemporarilyHidden := True;
  inherited;
  if FPressed then
  begin
    Fill := TBrush.Create(TBrushKind.bkSolid, $AA505050);
    Canvas.SetMatrix(AbsoluteMatrix);
    R := LocalRect;
    if FPressedVert then
    begin
      InflateRect(R, -1, -1);
      R.Left := R.Right - 5;
      Canvas.FillRect(R, 1, 1, AllCorners, AbsoluteOpacity, Fill);
      InflateRect(R, -1, -1);
    end
    else
    begin
      InflateRect(R, -1, -1);
      R.Top := R.Bottom - 5;
      Canvas.FillRect(R, 1, 1, AllCorners, AbsoluteOpacity, Fill);
      InflateRect(R, -1, -1);
    end;
    Fill.Free;
  end;
  if FPressed then
    Caret.TemporarilyHidden := LHidden;
end;

{ TSpinBox }

constructor TSpinBox.Create(AOwner: TComponent);
begin
  inherited;
  TextAlign := TTextAlign.taCenter;
  KeyboardType := TVirtualKeyboardType.vktNumberPad;
end;

procedure TSpinBox.ApplyStyle;
var
  B: TFmxObject;
begin
  inherited;
  B := FindStyleResource('minusbutton');
  if Assigned(B) and (B is TCustomButton) then
  begin
    FMinus := TCustomButton(B);
    FMinus.OnClick := DoMinusClick;
  end;
  B := FindStyleResource('plusbutton');
  if Assigned(B) and (B is TCustomButton) then
  begin
    FPlus := TCustomButton(B);
    FPlus.OnClick := DoPlusClick;
  end;
end;

procedure TSpinBox.FreeStyle;
begin
  inherited;
  FMinus := nil;
  FPlus := nil;
end;

procedure TSpinBox.DoMinusClick(Sender: TObject);
begin
  if not ReadOnly then
  begin
    SetFocus;
    ValueDec;
  end;
end;

procedure TSpinBox.DoPlusClick(Sender: TObject);
begin
  if not ReadOnly then
  begin
    SetFocus;
    ValueInc;
  end;
end;

function TSpinBox.GetIncrement: Single;
begin
  Result := inherited HorzIncrement;
end;

procedure TSpinBox.SetIncrement(const Value: Single);
begin
  inherited HorzIncrement := Value;
end;

{ TComboEditBase }

procedure TComboEditBase.ApplyStyle;
var
  T : TFmxObject;
  DeviceSrv: IFMXDeviceService;
begin
  inherited ApplyStyle;
  T := FindStyleResource('arrow');
  if Assigned(T) and (T is TControl) then
  begin
    FArrowButton := TControl(T);
    FArrowButton.Cursor := crDefault;
    FArrowButton.HitTest := True;
    FArrowButton.OnMouseDown := Self.DoComboMouseDown;
    if SupportsPlatformService(IFMXDeviceService, IInterface(DeviceSrv)) and (TDeviceFeature.HasTouchScreen in DeviceSrv.GetFeatures) then
      FArrowButton.TouchTargetExpansion.Rect := TRectF.Create(0, DefaultTouchTargetExpansion, DefaultTouchTargetExpansion, 0)
  end;
end;

procedure TComboEditBase.Change;
begin
  if FNeedChange then
    RepaintEdit;
  inherited;
end;

constructor TComboEditBase.Create(AOwner: TComponent);
begin
  inherited;
  MinClipWidth := 14;
  MinClipHeight := 14;
end;

procedure TComboEditBase.DoComboMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if Button = TMouseButton.mbLeft then
  begin
    SetFocus;
    DropDown;
  end;
end;

procedure TComboEditBase.FreeStyle;
begin
  if Assigned(FArrowButton) then
    FArrowButton.OnMouseDown := nil;
  FArrowButton := nil;
  inherited FreeStyle;
end;

procedure TComboEditBase.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited MouseDown(Button, Shift, X, Y);
  // if control does not support text input, then we must show drop down list,
  // when user clicked on control.
  if not InputSupport then
    DoComboMouseDown(Self, Button, Shift, X, Y);
end;

{ TComboEdit }

procedure TComboEdit.Clear;
begin
  FListBox.Clear;
end;

constructor TComboEdit.Create(AOwner: TComponent);
var
  PickerSevice: IFMXPickerService;
begin
  inherited;
  if TPlatformServices.Current.SupportsPlatformService(IFMXPickerService, IInterface(PickerSevice)) then
  begin
    FListPicker := PickerSevice.CreateListPicker;
    FListPicker.Parent := Self;
    FListPicker.OnValueChanged := DoOnValueChangedFromDropDownList;
    FListPicker.OnHide := DoClosePopup;
    FListPicker.OnShow := DoPopup;
  end;
  FDropDownKind := TDropDownKind.ddkNative;
  DropDownCount := 8;
  Cursor := crDefault;
  FItemHeight := 19;
  FDroppedDown := False;
  FItems := TStringList.Create;
  TStringList(FItems).OnChange := DoItemsChanged;
  FPopup := TPopup.Create(Self);
  FPopup.StyleLookup := 'combopopupstyle';
  FPopup.PlacementTarget := Self;
  FPopup.Stored := False;
  FPopup.Parent := Self;
  FPopup.Locked := True;
  FPopup.DesignVisible := False;
  FPopup.OnClosePopup := DoClosePopup;
  FPopup.OnPopup := DoPopup;
  FPopup.DragWithParent := True;
  FListBox := CreateListBox;
  FListBox.Parent := FPopup;
  FListBox.ItemHeight := ItemHeight;
  FListBox.Stored := False;
  FListBox.Align := TAlignLayout.alClient;
  FListBox.ShowCheckboxes := False;
  FListBox.ItemIndex := -1;
  OnTyping := Typing;
end;

function TComboEdit.CreateListBox: TComboEditListBox;
begin
  Result := TComboEditListBox.Create(Self);
end;

destructor TComboEdit.Destroy;
begin
  FreeAndNil(FListPicker);
  FreeAndNil(FItems);
  inherited;
end;

procedure TComboEdit.DoItemsChanged(Sender: TObject);
begin
  RebuildList;
  if InRange(ItemIndex, 0, Count - 1) then
    Text := Items[ItemIndex];
end;

procedure TComboEdit.DoOnValueChangedFromDropDownList(Sender: TObject;
  const AValueIndex: Integer);
var
  LSaveIndex: Integer;
begin
  if not ReadOnly then
  begin
    if Observers.IsObserving(TObserverMapping.EditLinkID) then
      if not TLinkObservers.EditLinkEdit(Observers) then
        // Editing not permitted, ignore change
        Exit;
    LSaveIndex := ItemIndex;
    ItemIndex := AValueIndex;
    if ItemIndex <> LSaveIndex then
    begin
      if Observers.IsObserving(TObserverMapping.EditLinkID) then
      begin
        TLinkObservers.EditLinkModified(Observers);
        TLinkObservers.EditLinkTrackUpdate(Observers);
      end;
      if Observers.IsObserving(TObserverMapping.ControlValueID) then
      begin
        TLinkObservers.ControlValueModified(Observers);
        TLinkObservers.ControlValueTrackUpdate(Observers);
      end;
    end;
  end;
end;

procedure TComboEdit.DoPopup(Sender: TObject);
begin
  if Assigned(FOnPopup) then
    FOnPopup(Self);
end;

procedure TComboEdit.RebuildList;
var
  SavedItemIndex, I: Integer;
  Item: TListBoxItem;
begin
  if csDestroying in ComponentState then
    Exit;
  if not Assigned(Items) then
    Exit;

  FListBox.BeginUpdate;
  try
    SavedItemIndex := FListBox.ItemIndex;
    FListBox.ItemIndex := -1;
    FListBox.Clear;
    for I := 0 to FItems.Count - 1 do
    begin
      Item := TListBoxItem.Create(FListBox);
      Item.Parent := FListBox;
      Item.AutoTranslate := AutoTranslate;
      Item.Height := FItemHeight;
      Item.Stored := False;
      Item.Locked := True;
      Item.Text := FItems[I];
    end;

    if SavedItemIndex >= FListBox.Count then
      SavedItemIndex := FListBox.Count - 1;
    FListBox.ItemIndex := SavedItemIndex;
  finally
    FListBox.EndUpdate;
  end;
end;

procedure TComboEdit.KeyDown(var Key: Word; var KeyChar: System.WideChar;
  Shift: TShiftState);
begin
  if Observers.IsObserving(TObserverMapping.EditLinkID) then
    if (KeyChar > ' ') or (Key in [vkUp, vkDown]) then
      if TLinkObservers.EditLinkIsReadOnly(Observers) then
        Exit
      else
        TLinkObservers.EditLinkEdit(Observers);
  inherited;
  if (Count > 0) and (not ReadOnly) and Enabled then
  begin
    case Key of
      vkUp:
        if ItemIndex > 0 then
          ItemIndex := ItemIndex - 1
        else
          ItemIndex := 0;
      vkDown:
        if ItemIndex < Count - 1 then
          ItemIndex := ItemIndex + 1
        else
          ItemIndex := Count - 1;
    else
      Exit;
    end;
    TLinkObservers.ListSelectionChanged(Observers);
    Key := 0;
    KeyChar := #0;
  end;
end;

procedure TComboEdit.DoRealign;
begin
  inherited;
  if FDisableAlign then
    Exit;
  FDisableAlign := True;
  { FContent }
  if Assigned(FPopup) then
    FPopup.Width := Width;
  if Assigned(FListBox) then
    FListBox.Width := Width;
  FDisableAlign := False;
end;

procedure TComboEdit.DoClosePopup(Sender: TObject);
begin
  if not (csDestroying in ComponentState) then
  begin
    FDroppedDown := False;
    SelStart := GetTextService.CombinedText.Length;
    SelLength := 0;
    ShowCaret;
    if Assigned(FOnClosePopup) then
      FOnClosePopup(Self);
  end;
end;

procedure TComboEdit.DoExit;
begin
  inherited DoExit;
  if (TDropDownKind.ddkNative = DropDownKind) and Assigned(FListPicker) and
     FListPicker.IsShown then
  begin
    FDroppedDown := False;
    FListPicker.Hide;
  end;
  if TDropDownKind.ddkCustom = DropDownKind then
    FPopup.IsOpen := False;
end;

procedure TComboEdit.DropDown;
var
  Count: Integer;
begin
  if (TDropDownKind.ddkNative = DropDownKind) and Assigned(FListPicker) then
  begin
    if FListPicker.IsShown then
    begin
      FDroppedDown := False;
      FListPicker.Hide
    end
    else
    begin
      SelLength := 0;
      HideCaret;
      FDroppedDown := True;
      FListPicker.Values := Items;
      FListPicker.ItemIndex := ItemIndex;
      FListPicker.CountVisibleItems := DropDownCount;
      FListPicker.Show;
    end;
  end
  else
  begin
    if not FPopup.IsOpen then
    begin
      FDroppedDown := True;
      HideCaret;
      FPopup.Placement := FPlacement;
      FPopup.Width := Width;
      Count := DropDownCount;
      if FListBox.Count < Count then
        Count := FListBox.Count;
      if FListBox.ItemHeight > 0 then
        FPopup.Height := Count * FListBox.ItemHeight + 4
      else
        FPopup.Height := Count * (Height - 4);
      FListBox.HitTest := Enabled and (not ReadOnly);
      FListBox.SetFocus;
      FPopup.IsOpen := True;
    end;
  end;
end;

procedure TComboEdit.MouseWheel(Shift: TShiftState; WheelDelta: Integer;
  var Handled: Boolean);
begin
  inherited;
  if WheelDelta < 0 then
  begin
    if ItemIndex < Count - 1 then
      ItemIndex := ItemIndex + 1
  end
  else
  begin
    if ItemIndex > 0 then
      ItemIndex := ItemIndex - 1;
  end;
  Handled := True;
end;

procedure TComboEdit.Notification(Component: TComponent; Operation: TOperation);
begin
  inherited Notification(Component, Operation);
  if (Operation = opRemove) and (Component = FListBox) then
    FListBox := nil;
end;

procedure TComboEdit.SetItemHeight(const Value: Single);
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    FListBox.ItemHeight := FItemHeight;
  end;
end;

procedure TComboEdit.SetItems(const Value: TStrings);
begin
  FItems.Assign(Value);
  ItemIndex := ItemIndex;
end;

procedure TComboEdit.ChangeParent;
begin
  inherited;
end;

function TComboEdit.GetItemIndex: Integer;
begin
  if Assigned(FListBox) then
    Result := FListBox.ItemIndex
  else
    Result := -1;
end;

procedure TComboEdit.SetItemIndex(const Value: Integer);
begin
  if Assigned(FListBox) then
  begin
    if (DropDownKind = TDropDownKind.ddkNative) and Assigned(FListPicker) then
      FListPicker.ItemIndex := Value;
    FListBox.ItemIndex := Value;
    FNeedChange := True;
    if (ItemIndex >= 0) and (ItemIndex < Items.Count) then
    begin
      SetTextInternal(Items[ItemIndex]);
      Caret.Visible := InputSupport;
      if not ReadOnly then
        SelectAll;
      if Assigned(ResourceControl) then
        ResourceControl.UpdateEffects;
      Repaint;
    end;
    Change;
  end;
end;

function TComboEdit.GetCount: Integer;
begin
  if Assigned(FListBox) then
    Result := FListBox.Count
  else
    Result := 0;
end;

function TComboEdit.GetDefaultStyleLookupName: string;
begin
  Result := 'comboeditstyle';
end;

procedure TComboEdit.Typing(Sender: TObject);
var
  I, l: Integer;
  UT: string;
begin
  if (FLastChar = #0) and ((FLastKey = vkDelete) or (FLastKey = 8)) then
    Exit;
{$IFDEF NOVCL}
{$IFDEF DARWIN}
  Exit;
{$ENDIF}
{$ENDIF}
  UT := Text.ToUpper;
  Exit;
  for I := 0 to Items.Count - 1 do
    if Items.Strings[I].ToUpper.IndexOf(UT) = 0 then
    begin
      l := Text.Length;
      Text := Items.Strings[I];
      SelStart := l;
      SelLength := Items.Strings[I].Length - l;
      Exit;
    end;
end;

function TComboEdit.GetListBoxResource: string;
begin
  Result := FListBox.StyleLookup;
end;

procedure TComboEdit.SetListBox(Value: TComboEditListBox);
begin
  if Value <> FListBox then
  begin
    FreeAndNil(FListBox);
    FListBox := Value;
    if Assigned(FListBox) then
      FListBox.FreeNotification(Self);
  end;
end;

procedure TComboEdit.SetListBoxResource(const Value: string);
begin
  FListBox.StyleLookup := Value;
end;

procedure TComboEdit.SetText(const Value: string);
begin
  inherited;
  Change;
end;

{ TComboTrackBar }

constructor TComboTrackBar.Create(AOwner: TComponent);
begin
  inherited;
  FDecimalDigits := 3;
  FValueType := TNumValueType.vtInteger;
  FFilterChar := FilterCharByValueType(FValueType);
  if Pos(FormatSettings.DecimalSeparator, FFilterChar) = 0 then
    FFilterChar := FFilterChar + FormatSettings.DecimalSeparator;
  Width := 100;
  Height := 22;
  FDefaultValueRange := TBaseValueRange.Create;
  FPopup := TPopup.Create(Self);
  FPopup.StyleLookup := 'combopopupstyle';
  FPopup.PlacementTarget := Self;
  FPopup.Stored := False;
  FPopup.Parent := Self;
  FPopup.Locked := True;
  FPopup.DesignVisible := True;
  FPopup.Padding.Rect := RectF(5, 2, 5, 2);
  FPopup.OnClosePopup := DoClosePopup;
  FPopup.DragWithParent := True;
  FTrackBar := TTrackBar.Create(Self);
  FTrackBar.Name := 'FTrackBar';
  FTrackBar.Parent := FPopup;
  FTrackBar.Stored := False;
  FTrackBar.DisableFocusEffect := True;
  FTrackBar.Align := TAlignLayout.alVertCenter;
  FTrackBar.ValueRange.BeforeChange := BeforeChangeProc;
  FTrackBar.ValueRange.AfterChange := AfterChangeProc;
  //
  GetTextService.Text := ConvertValueToText(Value);
end;

procedure TComboTrackBar.AfterConstruction;
begin
  inherited;
  DefaultValueRange.Assign(FTrackBar.ValueRange.New);
end;

destructor TComboTrackBar.Destroy;
begin
  FreeAndNil(FDefaultValueRange);
  inherited;
end;

procedure TComboTrackBar.Loaded;
begin
  if FTrackBar.ValueRange.IsChanged then
    FTrackBar.ValueRange.Changed(True);
  inherited;
end;

function TComboTrackBar.DefStored: Boolean;
begin
  Result := not (ActionClient and
                 (Action is TCustomValueRangeAction));
end;

function TComboTrackBar.MaxStored: Boolean;
begin
  Result := DefStored and (not SameValue(Max, DefaultValueRange.Max));
end;

function TComboTrackBar.MinStored: Boolean;
begin
  Result := DefStored and (not SameValue(Min, DefaultValueRange.Min));
end;

function TComboTrackBar.ValueStored: Boolean;
begin
  Result := DefStored and (not SameValue(Value, DefaultValueRange.Value));
end;

function TComboTrackBar.GetActionLinkClass: TActionLinkClass;
begin
  Result := TComboTrackActionLink;
end;

procedure TComboTrackBar.DoActionClientChanged;
begin
  inherited;
  if (not ActionClient) and Assigned(FTrackBar) then
    FTrackBar.Action := nil;
end;

procedure TComboTrackBar.ActionChange(Sender: TBasicAction;
  CheckDefaults: Boolean);
begin
  inherited;
  if (Sender is TCustomValueRangeAction) and Assigned(FTrackBar) then
    FTrackBar.Action := TCustomValueRangeAction(Action)
  else
    FTrackBar.Action := nil;
end;

procedure TComboTrackBar.KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
var
  Delta: Single;
begin
  if Key = vkReturn then
  begin
    Text := GetValueAsString;
    SelectAll;
    Key := 0;
    Exit;
  end;
  inherited KeyDown(Key, KeyChar, Shift);
  Delta := Frequency;
  if Delta <= 0 then
    Delta := 1;
  FTrackBar.ValueRange.Increment := Delta;
  case Key of
    vkUp:  FTrackBar.ValueRange.Inc;
    vkDown: FTrackBar.ValueRange.Dec;
  else
    Exit;
  end;
  Key := 0;
end;

procedure TComboTrackBar.DoEnter;
begin
  if FPopup.IsOpen then
    DropDown;
  inherited;
end;

procedure TComboTrackBar.DoExit;
begin
  inherited;
  Text := GetValueAsString;
  SelStart := 0;
  SelLength := 0;
end;

procedure TComboTrackBar.DoClosePopup(Sender: TObject);
begin
  Caret.Visible := InputSupport;
  SelectAll;
end;

procedure TComboTrackBar.DropDown;
begin
  if not FPopup.IsOpen then
  begin
    Caret.Visible := False;
    FPopup.Placement := FPlacement;
    if Width < 100 then
      FPopup.Width := 100
    else
      FPopup.Width := Width;
    FPopup.Height := 30;
    FTrackBar.ApplyStyleLookup;
    FTrackBar.Enabled := not ReadOnly;
    FPopup.IsOpen := True;
  end else
  begin
    FPopup.IsOpen := False;
  end;
end;

procedure TComboTrackBar.BeforeChangeProc(Sender: TObject);
var Sigma: Single;
begin
  if Sender is TCustomValueRange then
  begin
    Sigma := Power(10, -FDecimalDigits)/2;
    if not SameValue(TCustomValueRange(Sender).New.Value,
                     TCustomValueRange(Sender).Value,
                     Sigma) then
      FValueChanged := True;
  end;
end;

procedure TComboTrackBar.AfterChangeProc(Sender: TObject);
begin
  if FValueChanged then
  begin
    if FTextUpdating then
      Change
    else
    begin
      FTextUpdating := True;
      try
        Text := GetValueAsString;
        Change
      finally
        FTextUpdating := False;
      end;
      SelStart := 0;
      SelLength := 0;
    end;
    FValueChanged := False;
  end;
end;

procedure TComboTrackBar.SetText(const AValue: string);
var
  TempValue: Single;
begin
  if AValue <> Text then
  begin
    if (not FTextUpdating) then
    begin
      FTextUpdating := True;
      try
        if TryTextToValue(AValue, TempValue, Value) then
          Value := TempValue;
        if csDesigning in ComponentState then
          inherited SetText(ConvertValueToText(Value))
        else
          inherited;
        Repaint;
      finally
        FTextUpdating := False;
      end;
    end
    else
      inherited;
  end;
end;

procedure TComboTrackBar.Change;
var TempValue: Single;
begin
  if (not FTextUpdating) then
  begin
    FTextUpdating := True;
    try
      if TryTextToValue(Text, TempValue, Value) then
        Value := TempValue
      else
      begin
        Text := ConvertValueToText(TempValue);
      end;
    finally
      FTextUpdating := False;
    end;
  end;
  inherited;
end;

procedure TComboTrackBar.DoComboMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if (Button = TMouseButton.mbLeft) then
    if not ReadOnly then
      DropDown
end;

function TComboTrackBar.GetDefaultStyleLookupName: string;
begin
  Result := 'comboeditstyle';
end;

function TComboTrackBar.ConvertValueToText(const AValue: Single): String;
begin
  if (Frac(AValue) = 0) or (FValueType = TNumValueType.vtInteger) then
    Result := IntToStr(Round(AValue))
  else
    Result := FloatToStrF(AValue, ffFixed, 7 + FDecimalDigits, FDecimalDigits);
end;

function TComboTrackBar.GetValueAsString: String;
begin
  Result := ConvertValueToText(FTrackBar.ValueRange.Value);
end;

function TComboTrackBar.GetFrequency: Single;
begin
  Result := FTrackBar.ValueRange.Frequency;
end;

function TComboTrackBar.GetMax: Single;
begin
  Result := FTrackBar.ValueRange.Max;
end;

function TComboTrackBar.GetMin: Single;
begin
  Result := FTrackBar.ValueRange.Min;
end;

function TComboTrackBar.GetValue: Single;
begin
  if ValueType = TNumValueType.vtInteger then
    Result := Round(FTrackBar.ValueRange.Value)
  else
    Result := FTrackBar.ValueRange.Value;
end;

procedure TComboTrackBar.SetDecimalDigits(const AValue: Byte);
var
  TmpValue: Byte;
begin
  TmpValue := System.Math.Max(AValue, 1);
  if TmpValue <> FDecimalDigits then
  begin
    FDecimalDigits := TmpValue;
    GetTextService.Text := ConvertValueToText(Value);
    RepaintEdit;
  end;
end;

procedure TComboTrackBar.SetFrequency(const Value: Single);
begin
  FTrackBar.ValueRange.Frequency := Value;
end;

procedure TComboTrackBar.SetMax(const Value: Single);
begin
  FTrackBar.ValueRange.Max := Value;
end;

procedure TComboTrackBar.SetMin(const Value: Single);
begin
  FTrackBar.ValueRange.Min := Value;
end;

procedure TComboTrackBar.SetValue(const AValue: Single);
begin
  FTrackBar.ValueRange.Value := AValue;
end;

procedure TComboTrackBar.SetValueType(const AValueType: TNumValueType);
begin
  if FValueType <> AValueType then
  begin
    FValueType := AValueType;
    FFilterChar := FilterCharByValueType(FValueType);
    GetTextService.Text := ConvertValueToText(Value);
    RepaintEdit;
  end;
end;

{ TClearingEdit }

procedure TClearingEdit.ApplyStyle;
var
  B: TFmxObject;
begin
  inherited;
  B := FindStyleResource('clearbutton');
  if Assigned(B) and (B is TCustomButton) then
  begin
    FClearBtn := TCustomButton(B);
    FClearBtn.OnClick := DoClearBtnClick;
  end;
end;

procedure TClearingEdit.DoClearBtnClick(Sender: TObject);
begin
  SetFocus;
  if not ReadOnly then
  begin
    Text := '';
    Change;
  end;
end;

procedure TClearingEdit.FreeStyle;
begin
  if Assigned(FClearBtn) then
  begin
    FClearBtn.OnClick := nil;
    FClearBtn := nil;
  end;
  inherited;
end;

{ TComboListBox }

constructor TComboEditListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AOwner is TComboEdit then
    FComboEdit := TComboEdit(AOwner);
  OnStringsChanged := HandleStringsChanged;
  SetAcceptsControls(False);
end;

procedure TComboEditListBox.KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
begin
  inherited;
  if Key = vkRight then
  begin
    if (Parent is TPopup) and TPopup(Parent).IsOpen and Assigned(FComboEdit) then
    begin
      if (FComboEdit.Enabled) and
         (not FComboEdit.ReadOnly) then
                        
        FComboEdit.ItemIndex := ItemIndex;
      TPopup(Parent).IsOpen := False;
    end;
  end;
end;

procedure TComboEditListBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  LListBoxItem : TListBoxItem;
begin
  inherited;
  if (Parent is TPopup) and
     TPopup(Parent).IsOpen and
     Assigned(FComboEdit) then
  begin
    if (FComboEdit.Enabled) and
       (not FComboEdit.ReadOnly) and
       LocalRect.Contains(PointF(X, Y)) then
    begin
      LListBoxItem := ItemByPoint(X, Y);
      if Assigned(LListBoxItem) then
      begin
        if Observers.IsObserving(TObserverMapping.EditLinkID) then
          if not TLinkObservers.EditLinkEdit(Observers) then
            LListBoxItem := nil;

        if Assigned(LListBoxItem) then
        begin
          FComboEdit.ItemIndex := LListBoxItem.Index;

          if Observers.IsObserving(TObserverMapping.EditLinkID) then
          begin
            TLinkObservers.EditLinkModified(Observers);
            TLinkObservers.EditLinkTrackUpdate(Observers);
          end;
          if Observers.IsObserving(TObserverMapping.ControlValueID) then
          begin
            TLinkObservers.ControlValueModified(Observers);
            TLinkObservers.ControlValueTrackUpdate(Observers);
          end;
        end;
      end;
    end;
    TPopup(Parent).IsOpen := False;
  end;
end;

function TComboEditListBox.GetDefaultStyleLookupName: string;
begin
  Result := 'transparentlistboxstyle';
end;

function TComboEditListBox.GetObservers: TObservers;
begin
  if Assigned(FComboEdit) then
    Result := FComboEdit.Observers
  else
    Result := inherited;
end;

procedure TComboEditListBox.HandleStringsChanged(const S: String;
  const Op: TCustomListBox.TStringsChangeOp);
begin
  case Op of
    TCustomListBox.TStringsChangeOp.tsoAdded:
      FComboEdit.Items.Add(S);
    TCustomListBox.TStringsChangeOp.tsoDeleted: ;
    TCustomListBox.TStringsChangeOp.tsoClear: ;
  end;
end;

{ TEditButton }

constructor TEditButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetAcceptsControls(False);
  Align := TAlignLayout.alLeft;
  CanFocus := False;
end;

function TEditButton.GetControl: TControl;
begin
  Result := Self;
end;

function TEditButton.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(28, 28);
end;

function TEditButton.GetDefaultStyleLookupName: string;
begin
  Result := 'buttonstyle';
end;

function TEditButton.GetEdit: TCustomEdit;
var
  P: TFmxObject;
begin
  P := Parent;
  while Assigned(P) do
  begin
    if P is TCustomEdit then
    begin
      Result := TCustomEdit(P);
      Exit;
    end;
    P := P.Parent;
  end;
  Result := nil;
end;

procedure TEditButton.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
  if NewName = Text then
    Text := '';
end;

{ TClearButton }

procedure TClearEditButton.Click;
var
  EditTmp: TCustomEdit;
begin
  inherited Click;
  EditTmp := GetEdit;
  if Assigned(EditTmp) then
  begin
    if EditTmp.Observers.IsObserving(TObserverMapping.EditLinkID) then
      if not TLinkObservers.EditLinkEdit(EditTmp.Observers) then
        Exit; // Can't change
    EditTmp.Text := '';
    if EditTmp.Observers.IsObserving(TObserverMapping.EditLinkID) then
      TLinkObservers.EditLinkModified(EditTmp.Observers);
    if EditTmp.Observers.IsObserving(TObserverMapping.ControlValueID) then
      TLinkObservers.ControlValueModified(EditTmp.Observers);
  end;
end;

constructor TClearEditButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

function TClearEditButton.GetDefaultStyleLookupName: string;
begin
  Result := 'cleareditbutton';
end;

{ TContentEdit }

constructor TContentEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetAcceptsControls(False);
end;

procedure TContentEdit.DoRemoveObject(const AObject: TFmxObject);
begin
  inherited DoRemoveObject(AObject);
  if (AObject is TCustomButton) and (Parent is TCustomEdit) then
    TCustomEdit(Parent).RealignButtonsContainer;
end;

{ TPasswordButton }

constructor TPasswordEditButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

function TPasswordEditButton.GetDefaultStyleLookupName: string;
begin
  Result := 'passwordeditbutton';
end;

procedure TPasswordEditButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  EditTmp: TCustomEdit;
begin
  inherited MouseDown(Button, Shift, X, Y);
  EditTmp := GetEdit;
  if Assigned(EditTmp) then
    EditTmp.Password := False;
end;

procedure TPasswordEditButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
var
  EditTmp: TCustomEdit;
begin
  inherited MouseUp(Button, Shift, X, Y);;
  EditTmp := GetEdit;
  if Assigned(EditTmp) then
    EditTmp.Password := True;
end;

{ TSearchEditButton }

constructor TSearchEditButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

function TSearchEditButton.GetDefaultStyleLookupName: string;
begin
  Result := 'searcheditbutton';
end;

{ TEllipsesEditButton }

constructor TEllipsesEditButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

function TEllipsesEditButton.GetDefaultStyleLookupName: string;
begin
  Result := 'ellipseseditbutton';
end;

{ TDropDownEditButton }

constructor TDropDownEditButton.Create(AOwner: TComponent);
begin
  inherited;
end;

function TDropDownEditButton.GetDefaultStyleLookupName: string;
begin
  Result := 'dropdowneditbutton';
end;

{ TSearchBox }

procedure TSearchBox.FindButtonResources;
var
  R: TFmxObject;
begin
  if not Assigned(FMagGlass) then
  begin
    R := FindStyleResource('magglass');
    if (R <> nil) and (R is TEditButton) then
      FMagGlass := TEditButton(R);
  end;

  if not Assigned(FClearButton) then
  begin
    R := FindStyleResource('clearbutton');
    if (R <> nil) and (R is TEditButton) then
      FClearButton := TEditButton(R);
  end;
end;

constructor TSearchBox.Create(AOwner: TComponent);
begin
  inherited;
  Align := TAlignLayout.alTop;
end;

destructor TSearchBox.Destroy;
begin
  inherited;
end;

procedure TSearchBox.DoButtonsChanged(Sender: TObject);
begin
  Realign;
end;

procedure TSearchBox.DoChangeTracking;
var
  Lower: string;
begin
  inherited;
  Lower := LowerCase(Text.Trim);
  if FResponder <> nil then
    FResponder.SetFilterPredicate(
      function(X: string): Boolean
      begin
        Result :=
          (Lower = EmptyStr) or
            LowerCase(X).Contains(Lower);
      end);
end;

procedure TSearchBox.FreeStyle;
begin
  inherited;
  FMagGlass := nil;
  FClearButton := nil;
end;

procedure TSearchBox.SetParent(const Value: TFmxObject);
var
  ParentObject: TFmxObject;
begin
  inherited;
  ParentObject := Self.Parent;
  while (ParentObject <> nil) and (not (Supports(ParentObject, ISearchResponder, FResponder))) do
    ParentObject := ParentObject.Parent;
end;


procedure TSearchBox.RealignButtonsContainer;
begin
  inherited;
  FindButtonResources;
  if (FLeftLayout <> nil) and (FMagGlass <> nil) then
  begin
    FLeftLayout.Width := FMagGlass.Width;
    FMagGlass.OnApplyStyleLookup := DoButtonsChanged;
  end;

  if (FButtonsLayout <> nil) and (FClearButton <> nil) then
    if Length(Text) > 0 then
      FButtonsLayout.Width := FClearButton.Width
    else
      FButtonsLayout.Width := 0;
end;

{ TSpinEditButton }

procedure TSpinEditButton.ApplyStyle;
var
  StyleObject: TFmxObject;
begin
  inherited ApplyStyle;
  StyleObject := FindStyleResource('upbutton');
  if Assigned(StyleObject) and (StyleObject is TCustomButton) then
  begin
    FUpButton := StyleObject as TCustomButton;
    FUpButton.OnClick := DoUpButtonClick;
  end;

  StyleObject := FindStyleResource('downbutton');
  if Assigned(StyleObject) and (StyleObject is TCustomButton) then
  begin
    FDownButton := StyleObject as TCustomButton;
    FDownButton.OnClick := DoDownButtonClick;
  end;
end;

constructor TSpinEditButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Align := TAlignLayout.alLeft;
  CanFocus := False;
end;

function TSpinEditButton.GetDefaultStyleLookupName: string;
begin
  Result := 'spineditbutton';
end;

procedure TSpinEditButton.DoDownButtonClick(Sender: TObject);
begin
  if Assigned(FOnDownClick) then
    FOnDownClick(Self);
end;

procedure TSpinEditButton.DoUpButtonClick(Sender: TObject);
begin
  if Assigned(FOnUpClick) then
    FOnUpClick(Self);
end;

procedure TSpinEditButton.FreeStyle;
begin
  FUpButton := nil;
  FDownButton := nil;
  inherited FreeStyle;
end;

function TSpinEditButton.GetControl: TControl;
begin
  Result := Self;
end;

initialization
  RegisterFmxClasses([TEdit, TNumberBox, TSpinBox, TComboEdit, TComboTrackBar,
    TClearingEdit, TClearEditButton, TPasswordEditButton, TSearchEditButton,
    TEllipsesEditButton, TDropDownEditButton, TEditButton, TSpinEditButton,
    TSearchBox]);
end.
