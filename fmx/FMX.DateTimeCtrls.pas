{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit FMX.DateTimeCtrls;

interface

uses
  System.Classes, FMX.Pickers, System.Generics.Collections, System.Types,
  System.UITypes, System.Rtti,
  FMX.Types, FMX.Edit, FMX.TextLayout, FMX.Controls, FMX.ListBox, FMX.StdCtrls,
  FMX.ExtCtrls, FMX.Layouts, FMX.VirtualKeyboard, FMX.Menus, FMX.Platform, FMX.Graphics;

type

{ TTimeEdit }

  TDateTimePart = (tpYear, tpMonth, tpDay, tpHour24, tpHour12, tpMin, tpSec, tpMSec,
                   tpAMPM, tpUnknow);

  TRange<T> = record
    Min: T;
    Max: T;
  end;

  // This type incapsulates information about format part of DateTime (Hour, min and other)
  // like as current value, boundary values, display format for FormatDateTime
  // and other
  TDateTimeFormatPartInfo = record
    Part: TDateTimePart;
    Value: Integer;
    DisplayFormat: string;
    Range: TRange<Integer>;
    // Length specifies, how many characters displayed value shall borrow.
    // If length is equal 0, the quantity of displayed characters means isn't
    // restricted.
    //
    // Use <see cref="FMX.DateTimeCtrls|IsSpecifiedFixedLength"/> for determination
    // restrictions are set or not.
    DisplayLength: Integer;
  public
    constructor Create(const APart: TDateTimePart; const AMin, AMax: Integer); overload;
    constructor Create(const APart: TDateTimePart); overload;
    function IsSpecifiedFixedLength: Boolean;
    // Getting value length
    function GetActualLength: Integer;
    { Time section }
    function IsAM: Boolean;
    function ToAMPMString: string;
  end;

  TDTTimeFormat = (dfShort, dfLong);

  TCustomTimeEdit = class (TTextControl, IVirtualKeyboardControl)
  private
    FReadOnly: Boolean;
    FTime: TTime;
    FUseNowTime: Boolean;
    FShowCheckBox: Boolean;
    FIsChecked: Boolean;
    FIsPressed: Boolean;
    FSupportKeyboardInput: Boolean;
    { Virtual Keyboard }
    FKeyboardType : TVirtualkeyboardType;
    FReturnKeyType: TReturnKeyType;
    FKeyboardService: IFMXVirtualKeyboardService;
    { Time Formatting }
    FCustomFormat: string;
    FTimeFormatKind: TDTTimeFormat;
    FFormatParts: TList<TDateTimeFormatPartInfo>;
    FFormatSizePart: TList<TRectF>;
    FIndexCurrentPart: Integer;
    FContinueEnteringValue: Boolean;
    FCurrentValue: string;
    { Style Objects }
    FCheck: TCheckBox;
    FUpButton: TControl;
    FDownButton: TControl;
    { Timer }
    FTimerServise: IFMXTimerService;
    FTimerHandle: TFmxHandle;
    { Painting }
    FSelectionFill: TBrush;
    FTextLayout: TTextLayout;
    { Events }
    FOnTimeChanged: TNotifyEvent;
    FOnCheckChanged: TNotifyEvent;
    FOnOpenPicker: TNotifyEvent;
    FOnClosePicker: TNotifyEvent;
    procedure SetUseNowTime(const Value: Boolean);
    procedure SetCustomFormat(const Value: string);
    procedure SetIndexTimePart(const Value: Integer);
    procedure SetTimeFormat(const Value: TDTTimeFormat);
    procedure SetShowCheckBox(const Value: Boolean);
    procedure SetIsChecked(const Value: Boolean);
    { IVirtualKeyboardControl }
    function GetKeyboardType: TVirtualKeyboardType;
    procedure SetKeyboardType(Value: TVirtualKeyboardType);
    procedure SetReturnKeyType(Value: TReturnKeyType);
    function GetReturnKeyType: TReturnKeyType;
    { Context Menu }
    procedure DoCopy(Sender: TObject);
    procedure DoPaste(Sender: TObject);
  protected
    FDateTimePicker: TCustomDateTimePicker;
    FClipboardService: IFMXClipboardService;
    FContextMenu: TPopupMenu;
    { Format }
    procedure ParseFormat(const AFormat: string); virtual;
    function GetFormat: string; virtual;
    function HasFormat: Boolean;
    function GetCurrentFormatPart(var ATimeSection: TDateTimeFormatPartInfo): Boolean;
    procedure GoToNextFormatPart; virtual;
    procedure BackToPreviousFormatPart; virtual;
    { Events of style objects }
    procedure DoUpButtonClick(Sender: TObject); virtual;
    procedure DoDownButtonClick(Sender: TObject); virtual;
    procedure DoCheckChanged(Sender: TObject); virtual;
    { Pickers }
    procedure DoClosePicker(Sender: TObject); virtual;
    procedure DoOpenPicker(Sender: TObject); virtual;
    procedure DoTimeChanged(Sender: TObject; const ADate: TDateTime); virtual;
    { Painting }
    procedure DoContentPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF); virtual;
    procedure DrawSelection(ACanvas: TCanvas); virtual;
    procedure CalculateTextSize; virtual;
    procedure InitTextLayout; virtual;
    { Keyboard & Mouse }
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single); override;
    procedure EnterValue(var KeyChar: System.WideChar); virtual;
    { Styles }
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
    function HasTextObject: Boolean;
    function HasCheckBox: Boolean;
    property CheckBox: TCheckBox read FCheck;
    { Checkbox }
    procedure UpdateTextOpacity; virtual;
    { Focus }
    procedure DoExit; override;
    { Changing Time }
    procedure SynchronizeDateTimeParts(const ADateTimePartInfo: TDateTimeFormatPartInfo); virtual;
    procedure UpdateTimeSections; virtual;
    procedure SetTime(const Value: TTime); virtual;
    procedure IncrementDateTimePart; virtual;
    procedure DecrementDateTimePart; virtual;
    procedure ChangingTime; virtual;
    procedure SetText(const AValue: string); override;
    procedure DoTimer; virtual;
    { Context Menu }
    procedure CreateContextMenu; virtual;
    procedure CreateContextMenuItem(const AActionName, ATitleName: string; const AEventHandler: TNotifyEvent = nil);
    procedure ContextMenu(const ScreenPosition: TPointF); override;
    { inherited for TextSettings }
    procedure DoChanged; override;
    function GetDefaultSize: TSizeF; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Pickers }
    procedure OpenPicker; virtual;
    procedure ClosePicker; virtual;
    function HasPicker: Boolean;
    { Time changing }
    function CanInputTime: Boolean; virtual;
    { Clipboard }
    procedure CopyToClipboard; virtual;
    procedure PasteFromClipboard; virtual;
    property ShowCheckBox: Boolean read FShowCheckBox write SetShowCheckBox default False;
    property IsChecked: Boolean read FIsChecked write SetIsChecked default False;
    property IsPressed: Boolean read FIsPressed;
    property KeyboardType: TVirtualKeyboardType read GetKeyboardType write SetKeyboardType default TVirtualKeyboardType.vktNumberPad;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property TimeFormatKind: TDTTimeFormat read FTimeFormatKind write SetTimeFormat default TDTTimeFormat.dfShort;
    property Format: string read FCustomFormat write SetCustomFormat;
    property Time: TTime read FTime write SetTime;
    property UseNowTime: Boolean read FUseNowTime write SetUseNowTime default False;
    property OnCheckChanged: TNotifyEvent read FOnCheckChanged write FOnCheckChanged;
    property OnTimeChanged: TNotifyEvent read FOnTimeChanged write FOnTimeChanged;
    property OnClosePicker: TNotifyEvent read FOnClosePicker write FOnClosePicker;
    property OnOpenPicker: TNotifyEvent read FOnOpenPicker write FOnOpenPicker;
  end;

  TTimeEdit = class (TCustomTimeEdit)
  published
    property ShowCheckBox;
    property IsChecked;
    // Custom format for displayed time. TTimeEdit supports only a part of format 
    // ('h', 'hh', 'n', 'nn', 's', 'ss', 'z', 'zzz', 'ap', 'am/pm', 'ampm' )
    // and comments in quote
    // See details about format in |System.SysUtils.FormatDateTime|
    property Format;
    property Time;
    property TimeFormatKind;
    property UseNowTime;
    property OnCheckChanged;
    property OnClosePicker;
    property OnOpenPicker;
    property OnTimeChanged;
    { inherited }
    property Align;
    property Anchors;
    property CanFocus default True;
    property CanParentFocus;
    property Cursor default crDefault;
    property ClipChildren default False;
    property ClipParent default False;
    property DesignVisible default True;
    property DisableFocusEffect;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Font;
    property FontColor;
    property Height;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property HitTest default True;
    property Locked default False;
    property KeyboardType default TVirtualKeyboardType.vktNumberPad;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property ReadOnly;
    property Scale;
    property StyleLookup;
    property StyledSettings;
    property TabOrder;
    property TextAlign;
    property Visible default True;
    property Width;
    { events }
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

{ TCalendarEdit }

  TCustomCalendarEdit = class(TComboEditBase)
  private
    FDateTimePicker: TCustomDateTimePicker;
    FFirstDayOfWeek: TCalDayOfWeek;
    FShowWeekNumbers: Boolean;
    FTodayDefault: Boolean;
    FDateTime: TDateTime;
    FMinDate: TDateTime;
    FMaxDate: TDateTime;
    FIsPressed: Boolean;
    FOnPopup: TNotifyEvent;
    FOnClosePopup: TNotifyEvent;
    function GetDate: TDate;
    procedure SetDate(const Value: TDate);
    procedure SetDateTime(const Value: TDateTime);
    procedure SetDateConstraints(const Index: Integer; const Value: TDateTime);
    function DoStoreDateConstraints(const Index: Integer): Boolean;
  protected
    procedure DoDateChanged(Sender: TObject; const ADate: TDateTime); virtual;
    procedure DoPopupClose(Sender: TObject); virtual;
    procedure DoPopup(Sender: TObject); virtual;
    procedure DoExit; override;
    procedure SetText(const AValue: string); override;
    procedure SetInputSupport(const Value: Boolean); override;
    procedure Change; override;
    function HasPicker: Boolean;
    function GetFormattedDateTimeText(const ADateTime: TDateTime): string;
    function GetDefaultStyleLookupName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DropDown; override;
    procedure ClosePicker;
    property Date: TDate read GetDate write SetDate;
    property DateTime: TDateTime read FDateTime write SetDateTime;
    property MinDate: TDateTime index 1 read FMinDate write SetDateConstraints stored DoStoreDateConstraints;
    property MaxDate: TDateTime index 2 read FMaxDate write SetDateConstraints stored DoStoreDateConstraints;
    property IsPressed: Boolean read FIsPressed;
    property TodayDefault: Boolean read FTodayDefault write FTodayDefault default False;
    property WeekNumbers: Boolean read FShowWeekNumbers write FShowWeekNumbers default False;
    property FirstDayOfWeek: TCalDayOfWeek read FFirstDayOfWeek write FFirstDayOfWeek
      default TCalDayOfWeek.dowLocaleDefault;
    property OnClosePopup: TNotifyEvent read FOnClosePopup write FOnClosePopup;
    property OnPopup: TNotifyEvent read FOnPopup write FOnPopup;
  end;

  TCalendarEdit = class (TCustomCalendarEdit)
  published
    property Anchors;
    property CanFocus default True;
    property CanParentFocus;
    property Cursor default crDefault;
    property ClipChildren default False;
    property ClipParent default False;
    property Date;
    property DesignVisible default True;
    property DisableFocusEffect;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property FirstDayOfWeek;
    property Font;
    property Height;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property HitTest default True;
    property InputSupport default False;
    property Locked default False;
    property KeyboardType default TVirtualKeyboardType.vktNumbersAndPunctuation;
    property Padding;
    property Opacity;
    property Margins;
    property MinDate;
    property MaxDate;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property ReadOnly;
    property Scale;
    property StyleLookup;
    property StyledSettings;
    property ShowHint default False;
    property Text stored False;
    property TextAlign;
    property TodayDefault;
    property Visible default True;
    property WeekNumbers;
    property Width;
    { events }
    property OnClosePopup;
    property OnPopup;
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

{ TCalendar }

  TCalendar = class(TStyledControl)
  private
    FNeedChange: Boolean;
    FDateTime: TDateTime;
    FDays: TListBox;
    FToday, FPrev, FNext: TButton;
    FMonths: TPopupBox;
    FYears: TPopupBox;
    FWeeks: TGridLayout;
    FFirstDayOfWeek: TCalDayOfWeek;
    FFirstDayOfWeekNum: Integer;
    FWeek: TGridLayout;
    FTodayDefault: Boolean;
    FOnChange: TNotifyEvent;
    FWeekNumbers: Boolean;
    FOnDateSelected: TNotifyEvent;
    function GetDate: TDate;
    procedure SetDate(Value: TDate);
    procedure SetDateTime(const Value: TDateTime);
    procedure SetFirstDayOfWeek(const Value: TCalDayOfWeek);
    procedure SetTodayDefault(const Value: Boolean);
    procedure SetWeekNumbers(const Value: Boolean);
  protected
    FDisableDayChange: Integer;
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
    { children }
    procedure DoDeleteChildren; override;
    //
    procedure DoPrevClick(Sender: TObject);
    procedure DoNextClick(Sender: TObject);
    procedure DoTodayClick(Sender: TObject);
    procedure DoDaysMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure DoDaysMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure DoDayChange(Sender: TObject);
    procedure DoMonthChange(Sender: TObject);
    procedure DoYearChange(Sender: TObject);
    procedure FillList;
    procedure DoChange; virtual;
    procedure DoDateSelected; virtual;
    procedure KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    function GetData: TValue; override;
    procedure SetData(const Value: TValue); override;
    procedure DoRealign; override;
  public
    constructor Create(AOwner: TComponent); override;
    property DateTime: TDateTime read FDateTime write SetDateTime;
  published
    property Align;
    property Anchors;
    property ClipChildren default True;
    property ClipParent default False;
    property Cursor default crDefault;
    property Date: TDate read GetDate write SetDate;
    property DesignVisible default True;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property FirstDayOfWeek: TCalDayOfWeek read FFirstDayOfWeek
      write SetFirstDayOfWeek default TCalDayOfWeek.dowLocaleDefault;
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
    property StyleLookup;
    property TabOrder;
    property TodayDefault: Boolean read FTodayDefault write SetTodayDefault default False;
    property Visible default True;
    property WeekNumbers: Boolean read FWeekNumbers write SetWeekNumbers default False;
    property Width;
    {events}
    property OnApplyStyleLookup;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDateSelected: TNotifyEvent read FOnDateSelected write FOnDateSelected;
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

{ TCalendarBox }

  TCalendarBox = class (TCalendarEdit)
  public
    constructor Create(AOwner: TComponent); override;
  end; //{$IFNDEF NEXTGEN_TODO} deprecated 'Duplicate of TCalendarEdit. Use TCalendarEdit instead TCalendarBox'{$ENDIF};


implementation

uses
  System.SysConst, System.SysUtils, System.DateUtils, System.Math, System.Character, FMX.Forms, FMX.Consts, FMX.Text;

{ TCustomTimeEdit }

procedure TCustomTimeEdit.ApplyStyle;
var
  StyleObject: TFmxObject;
begin
  inherited ApplyStyle;
  { Check box }
  StyleObject := FindStyleResource('check');
  if Assigned(StyleObject) and (StyleObject is TCheckBox) then
  begin
    FCheck := StyleObject as TCheckBox;
    FCheck.Visible := ShowCheckBox;
    FCheck.OnChange := DoCheckChanged;
    FCheck.IsChecked := IsChecked;
    UpdateTextOpacity;
  end;
  { Text }
  if Assigned(TextObject) then
  begin
    TextObject.OnPaint := DoContentPaint;
    CalculateTextSize;
  end;
  { Spin buttons }
  StyleObject := FindStyleResource('upbutton');
  if Assigned(StyleObject) and (StyleObject is TControl) then
  begin
    FUpButton := StyleObject as TControl;
    FUpButton.OnClick := DoUpButtonClick;
  end;
  StyleObject := FindStyleResource('downbutton');
  if Assigned(StyleObject) and (StyleObject is TControl) then
  begin
    FDownButton := StyleObject as TControl;
    FDownButton.OnClick := DoDownButtonClick;
  end;
  { Selection brush }
  StyleObject := FindStyleResource('selection');
  if Assigned(StyleObject) and (StyleObject is TBrushObject) then
    FSelectionFill.Assign(TBrushObject(StyleObject).Brush);
end;

procedure TCustomTimeEdit.BackToPreviousFormatPart;
var
  I: Integer;
  Found: Boolean;
begin
  I := FIndexCurrentPart - 1;
  Found := False;
  while (I >= 0) and not Found do
    if FFormatParts[I].Part = TDateTimePart.tpUnknow then
      Dec(I)
    else
      Found := True;

  if Found then
    SetIndexTimePart(I);
end;

procedure TCustomTimeEdit.CalculateTextSize;

  procedure CorrectHorizontalTextAligment(const ATextWidth: Single);
  var
    I: Integer;
    DeltaTextOffset: Extended;
    TmpRect: TRectF;
  begin
    if not (TextAlign in [TTextAlign.taTrailing, TTextAlign.taCenter]) then
      Exit;

    { Correct horizontal aligment }
    case TextAlign of
      TTextAlign.taCenter:
        DeltaTextOffset := TextObject.Width / 2 -  ATextWidth / 2 - FFormatSizePart[0].Left;
      TTextAlign.taTrailing:
        DeltaTextOffset := TextObject.Width - FFormatSizePart[FFormatSizePart.Count - 1].Right;
    else
      DeltaTextOffset := 0;
    end;
    for I := FFormatSizePart.Count - 1 downto 0 do
    begin
       TmpRect := FFormatSizePart[I];
       TmpRect.Offset(DeltaTextOffset, 0);
       FFormatSizePart[I] := TmpRect;
    end;
  end;

var
  TimePart: TDateTimeFormatPartInfo;
  TmpRect: TRectF;
  TotalOffset: Single;
begin
  if not HasTextObject then
    Exit;

  FFormatSizePart.Clear;
  TotalOffset := 0;
  InitTextLayout;
  for TimePart in FFormatParts do
  begin
    { Calculation DateTimePart rect in local coordinates (start from 0, 0) }
    if TimePart.Part = TDateTimePart.tpUnknow then
      FTextLayout.Text := TimePart.DisplayFormat.Substring(0, TimePart.DisplayFormat.Length)
    else
    if TimePart.Part = TDateTimePart.tpAMPM then
      FTextLayout.Text := TimePart.ToAMPMString
    else
      FTextLayout.Text := '9999'.Substring(0, TimePart.GetActualLength);
    TmpRect := FTextLayout.TextRect;
    { Translate local coordinate to local coordinate of FContent }
    TmpRect.Offset(TotalOffset, 0);
    FFormatSizePart.Add(TmpRect);
    TotalOffset := TotalOffset + FTextLayout.TextRect.Width;
  end;
  CorrectHorizontalTextAligment(TotalOffset);
end;

function TCustomTimeEdit.CanInputTime: Boolean;
begin
  Result := not ReadOnly and not UseNowTime;
  Result := Result and ((ShowCheckBox and IsChecked) or not ShowCheckBox);
end;

procedure TCustomTimeEdit.ChangingTime;
var
  Converted: Boolean;
  TempDateTime: TDateTime;
begin
  Converted := TryStrToTime(Text, TempDateTime, FormatSettings);
  TempDateTime := TimeOf(TempDateTime);
  if Converted then
    FTime := TimeOf(TempDateTime)
  else
    Text := FormatDateTime(GetFormat, Time);
  Repaint;
end;

procedure TCustomTimeEdit.ClosePicker;
begin
  if HasPicker and FDateTimePicker.IsShown then
    FDateTimePicker.Hide;
end;

procedure TCustomTimeEdit.ContextMenu(const ScreenPosition: TPointF);
begin
  if Assigned(PopupMenu) then
    inherited ContextMenu(ScreenPosition)
  else
    if not (csDesigning in ComponentState) then
      FContextMenu.Popup(Round(ScreenPosition.X), Round(ScreenPosition.Y));
end;

procedure TCustomTimeEdit.CopyToClipboard;
begin
  if not Assigned(FClipboardService) then
    Exit;

  FClipboardService.SetClipboard(TimeToStr(Time));
end;

constructor TCustomTimeEdit.Create(AOwner: TComponent);
var
  PickerService: IFMXPickerService;
begin
  inherited Create(AOwner);
  TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService, IInterface(FKeyboardService));
  TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, IInterface(FClipboardService));
  if TPlatformServices.Current.SupportsPlatformService(IFMXPickerService, IInterface(PickerService)) then
  begin
    FDateTimePicker := PickerService.CreateDateTimePicker;
    FDateTimePicker.ShowMode := TDatePickerShowMode.psmTime;
    FDateTimePicker.Parent := Self;
    FDateTimePicker.OnHide := DoClosePicker;
    FDateTimePicker.OnShow := DoOpenPicker;
    FDateTimePicker.OnDateChanged := DoTimeChanged;
  end;
  TPlatformServices.Current.SupportsPlatformService(IFMXTimerService, IInterface(FTimerServise));
  FFormatParts := TList<TDateTimeFormatPartInfo>.Create;
  FFormatSizePart := TList<TRectF>.Create;
  FTextLayout := TTextLayoutManager.DefaultTextLayout.Create;
  FSelectionFill := TBrush.Create(TBrushKind.bkSolid, $802A8ADF);
  ParseFormat(GetFormat);
  CanFocus := True;
  FKeyboardType := TVirtualKeyboardType.vktNumberPad;
  FReturnKeyType := TReturnKeyType.rktDefault;
  Time := Now;
  FShowCheckBox := False;
  FIndexCurrentPart := -1;
  FContinueEnteringValue := False;
  CreateContextMenu;
{$IF Defined(IOS) OR Defined(ANDROID)}
  FSupportKeyboardInput := False;
{$ELSE}
  FSupportKeyboardInput := True;
{$ENDIF}
  SetAcceptsControls(False);
end;

procedure TCustomTimeEdit.CreateContextMenu;
begin
  FContextMenu := TPopupMenu.Create(Self);
  FContextMenu.PopupComponent := Self;
  FContextMenu.Stored := False;
  FContextMenu.Parent := Self;

  CreateContextMenuItem('copy', SEditCopy, DoCopy);
  CreateContextMenuItem('paste', SEditPaste, DoPaste);
end;

procedure TCustomTimeEdit.CreateContextMenuItem(const AActionName, ATitleName: string;
  const AEventHandler: TNotifyEvent = nil);
var
  TmpItem: TMenuItem;
begin
  Assert(not ATitleName.IsEmpty);
  Assert(not AActionName.IsEmpty);

  TmpItem := TMenuItem.Create(FContextMenu);
  TmpItem.Parent := FContextMenu;
  TmpItem.Text := ATitleName;
  TmpItem.StyleName := AActionName;
  TmpItem.OnClick := AEventHandler;
end;

procedure TCustomTimeEdit.DecrementDateTimePart;
var
  TimePart: TDateTimeFormatPartInfo;
begin
  if GetCurrentFormatPart(TimePart) and CanInputTime then
  begin
    if TimePart.Value - 1 < TimePart.Range.Min then
      TimePart.Value := TimePart.Range.Max
    else
      TimePart.Value := TimePart.Value - 1;
    FFormatParts[FIndexCurrentPart] := TimePart;
    SynchronizeDateTimeParts(TimePart);
  end;
end;

destructor TCustomTimeEdit.Destroy;
begin
  FreeAndNil(FSelectionFill);
  FKeyboardService := nil;
  FTimerServise := nil;
  FreeAndNil(FFormatParts);
  FreeAndNil(FFormatSizePart);
  if HasPicker and FDateTimePicker.IsShown then
    FDateTimePicker.Hide;
  FreeAndNil(FDateTimePicker);
  inherited Destroy;
end;

procedure TCustomTimeEdit.DoChanged;
begin
  inherited DoChanged;
  CalculateTextSize;
  if Assigned(FOnTimeChanged) and not (csLoading in ComponentState) then
    FOnTimeChanged(Self);
end;

procedure TCustomTimeEdit.DoCheckChanged(Sender: TObject);
begin
  FIsChecked := FCheck.IsChecked;
  UpdateTextOpacity;
  if Assigned(FOnCheckChanged) then
    FOnCheckChanged(Self);
end;

procedure TCustomTimeEdit.DoClosePicker(Sender: TObject);
begin
  if Assigned(FOnClosePicker) then
    FOnClosePicker(Self);
end;

procedure TCustomTimeEdit.DoContentPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  State: TCanvasSaveState;
begin
  State := Canvas.SaveState;
  try
    DrawSelection(Canvas);
  finally
    Canvas.RestoreState(State);
  end;
end;

procedure TCustomTimeEdit.DoCopy(Sender: TObject);
begin
  CopyToClipboard;
end;

procedure TCustomTimeEdit.DoDownButtonClick(Sender: TObject);
begin
  if not IsFocused then
    SetFocus;
  DecrementDateTimePart;
end;

procedure TCustomTimeEdit.DoExit;
begin
  inherited DoExit;
  ClosePicker;
end;

procedure TCustomTimeEdit.DoOpenPicker(Sender: TObject);
begin
  if Assigned(FOnOpenPicker) then
    FOnOpenPicker(Self);
end;

procedure TCustomTimeEdit.DoPaste(Sender: TObject);
begin
  PasteFromClipboard;
end;

procedure TCustomTimeEdit.DoTimeChanged(Sender: TObject; const ADate: TDateTime);
begin
  if ReadOnly then
    Exit;
  Time := TimeOf(ADate);

  inherited Change;
end;

procedure TCustomTimeEdit.DoTimer;
begin
  Time := System.SysUtils.Time;
end;

procedure TCustomTimeEdit.DoUpButtonClick(Sender: TObject);
begin
  if not IsFocused then
    SetFocus;
  IncrementDateTimePart;
end;

procedure TCustomTimeEdit.DrawSelection(ACanvas: TCanvas);

  function IsFormActive: Boolean;
  begin
    Result := (Scene.GetObject is TCustomForm) and (Scene.GetObject as TCustomForm).Active;
  end;

  function IsNeedDrawSelection: Boolean;
  begin
    Result := CanInputTime and FSupportKeyboardInput and IsFormActive and IsFocused;
  end;

var
  TextRect: TRectF;
begin
  if InRange(FIndexCurrentPart, 0, FFormatSizePart.Count - 1) and IsNeedDrawSelection then
  begin
    TextRect := FFormatSizePart[FIndexCurrentPart];
    TextRect := TextRect * TextObject.LocalRect;
    ACanvas.FillRect(TextRect, 0, 0, AllCorners, AbsoluteOpacity, FSelectionFill);
  end;
end;

procedure TCustomTimeEdit.OpenPicker;
begin
  if HasPicker and not FDateTimePicker.IsShown and not UseNowTime then
  begin
    { Initialization of time picker before showing }
    FDateTimePicker.Date := Time;
    FDateTimePicker.ShowMode := TDatePickerShowMode.psmTime;
    FDateTimePicker.Show;
  end;
end;

procedure TCustomTimeEdit.EnterValue(var KeyChar: System.WideChar);
var
  TimePart: TDateTimeFormatPartInfo;
begin
  if GetCurrentFormatPart(TimePart) then
  begin
    FCurrentValue := FCurrentValue + KeyChar;
    if not FCurrentValue.IsEmpty and (TimePart.Range.Max < StrToInt(FCurrentValue)) then
    begin
      FContinueEnteringValue := False;
      FCurrentValue := KeyChar;
    end;

    if FContinueEnteringValue then
      TimePart.Value := StrToInt(FCurrentValue)
    else
      TimePart.Value := Round(KeyChar.GetNumericValue);
    SynchronizeDateTimeParts(TimePart);
    FContinueEnteringValue := True;
    KeyChar := #0;
  end;
end;

procedure TCustomTimeEdit.FreeStyle;
begin
  if HasTextObject then
    TextObject.OnPaint := nil;
  FCheck := nil;
  FUpButton := nil;
  FDownButton := nil;
  inherited FreeStyle;
end;

function TCustomTimeEdit.GetCurrentFormatPart(
  var ATimeSection: TDateTimeFormatPartInfo): Boolean;
begin
  Result := InRange(FIndexCurrentPart, 0, FFormatParts.Count - 1);
  if Result then
     ATimeSection := FFormatParts[FIndexCurrentPart];
end;

function TCustomTimeEdit.GetDefaultSize: TSizeF;
var
  DefMetricsSrv: IFMXDefaultMetricsService;
begin
  if SupportsPlatformService(IFMXDefaultMetricsService, IInterface(DefMetricsSrv)) and DefMetricsSrv.SupportsDefaultSize(ckEdit) then
    Result := TSizeF.Create(DefMetricsSrv.GetDefaultSize(ckEdit).Width, DefMetricsSrv.GetDefaultSize(ckEdit).Height)
  else
    Result := TSizeF.Create(100, 22);
end;

function TCustomTimeEdit.GetFormat: string;
begin
  if Format.IsEmpty then
    // FormatSettings TimeFormat uses 'm' for setting minutes.
    // But FormatDateTime uses 'n' for settings minutes. So we need to replace
    // 'm' -> 'n'
    if FTimeFormatKind = TDTTimeFormat.dfShort then
       Result := FormatSettings.ShortTimeFormat.Replace('m','n')
    else
      Result := FormatSettings.LongTimeFormat.Replace('m','n')
  else
    Result := Format;
end;

function TCustomTimeEdit.GetKeyboardType: TVirtualKeyboardType;
begin
  Result := FKeyboardType;
end;

function TCustomTimeEdit.GetReturnKeyType: TReturnKeyType;
begin
  Result := FReturnKeyType;
end;

procedure TCustomTimeEdit.GoToNextFormatPart;
var
  I: Integer;
  Found: Boolean;
begin
  I := FIndexCurrentPart + 1;
  Found := False;
  while (I < FFormatParts.Count) and not Found do
    if FFormatParts[I].Part = TDateTimePart.tpUnknow then
      Inc(I)
    else
      Found := True;
  if Found then
    SetIndexTimePart(I);
end;

function TCustomTimeEdit.HasCheckBox: Boolean;
begin
  Result := Assigned(FCheck);
end;

function TCustomTimeEdit.HasTextObject: Boolean;
begin
  Result := Assigned(TextObject);
end;

function TCustomTimeEdit.HasFormat: Boolean;
begin
  Result := FFormatParts.Count > 0;
end;

function TCustomTimeEdit.HasPicker: Boolean;
begin
  Result := Assigned(FDateTimePicker);
end;

procedure TCustomTimeEdit.IncrementDateTimePart;
var
  TimePart: TDateTimeFormatPartInfo;
begin
  if GetCurrentFormatPart(TimePart) and CanInputTime then
  begin
    if TimePart.Value + 1 > TimePart.Range.Max then
      TimePart.Value := TimePart.Range.Min
    else
      TimePart.Value := TimePart.Value + 1;
    FFormatParts[FIndexCurrentPart] := TimePart;
    SynchronizeDateTimeParts(TimePart);
  end;
end;

procedure TCustomTimeEdit.InitTextLayout;
begin
  FTextLayout.MaxSize := TPointF.Create(TextObject.Width, TextObject.Height);
  FTextLayout.HorizontalAlign := TTextAlign.taLeading;
  FTextLayout.VerticalAlign := VertTextAlign;
  FTextLayout.Font := Font;
end;

procedure TCustomTimeEdit.KeyDown(var Key: Word; var KeyChar: System.WideChar;
  Shift: TShiftState);
begin
  if not CanInputTime then
    Exit;

  if KeyChar.IsDigit then
    EnterValue(KeyChar);

  case Key of
    vkLeft:
      BackToPreviousFormatPart;
    vkRight:
      GoToNextFormatPart;
    vkUp:
      IncrementDateTimePart;
    vkDown:
      DecrementDateTimePart;
  end;
end;

procedure TCustomTimeEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);

  function FindDateTimeSection(const APos: TPointF): Integer;
  var
    Found: Boolean;
    I: Integer;
  begin
    Found := False;
    I := 0;
    while (I < FFormatSizePart.Count) and not Found do
      if InRange(APos.X, FFormatSizePart[I].Left, FFormatSizePart[I].Right) and (FFormatParts[I].Part <> TDateTimePart.tpUnknow) then
        Found := True
      else
        Inc(I);

    if Found then
      Result := I
    else
    begin
      if APos.X < FFormatSizePart[0].Left then
        Result := 0
      else
      if APos.X > FFormatSizePart[FFormatSizePart.Count - 1].Right then
        Result := FFormatSizePart.Count - 1
      else
        Result := FIndexCurrentPart;
    end;
  end;

var
  AbsoluteMousePos: TPointF;
  ContentMousePos: TPointF;
begin
  if HasTextObject then
  begin
    AbsoluteMousePos := LocalToAbsolute(TPointF.Create(X, Y));
    ContentMousePos := TextObject.AbsoluteToLocal(AbsoluteMousePos);
    SetIndexTimePart(FindDateTimeSection(ContentMousePos));
  end;
  if not FSupportKeyboardInput and HasPicker and CanInputTime then
    if FDateTimePicker.IsShown then
      ClosePicker
    else
      OpenPicker;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomTimeEdit.ParseFormat(const AFormat: string);
const
  DATETIME_FORMAT_SYMBOLS = ['h', 'n', 's', 'z', 'a'];
type
  TStateParser = (spNone, spComment, spFormat, spNoneFormat);

  procedure ParseTime(const ASource: string; const AHourFormatType: TDateTimePart; var AStartIndex: Integer);
  var
    FormatSection: TDateTimeFormatPartInfo;
  begin
    case ASource.Chars[AStartIndex] of
      { Hours }
      'h':
        begin
          FormatSection := TDateTimeFormatPartInfo.Create(AHourFormatType, 0, 23);
          if ASource.IndexOf('hh', AStartIndex) = AStartIndex then
          begin
            FormatSection.DisplayFormat := 'hh';
            FormatSection.DisplayLength := 2;
            Inc(AStartIndex);
          end
          else
          begin
            FormatSection.DisplayFormat := 'h';
            FormatSection.DisplayLength := 0;
          end;
        end;
      { Minutes }
      'n':
        begin
          FormatSection := TDateTimeFormatPartInfo.Create(TDateTimePart.tpMin, 0, 59);
          if ASource.IndexOf('nn', AStartIndex) = AStartIndex then
          begin
            FormatSection.DisplayFormat := 'nn';
            FormatSection.DisplayLength := 2;
            Inc(AStartIndex);
          end
          else
          begin
            FormatSection.DisplayFormat := 'n';
            FormatSection.DisplayLength := 0;
          end;
        end;
      { Seconds }
      's':
        begin
          FormatSection := TDateTimeFormatPartInfo.Create(TDateTimePart.tpSec, 0, 59);
          if ASource.IndexOf('ss', AStartIndex) = AStartIndex then
          begin
            FormatSection.DisplayFormat := 'ss';
            FormatSection.DisplayLength := 2;
            Inc(AStartIndex);
          end
          else
          begin
            FormatSection.DisplayFormat := 's';
            FormatSection.DisplayLength := 0;
          end;
        end;
      { MSecs }
      'z':
      begin
        FormatSection := TDateTimeFormatPartInfo.Create(TDateTimePart.tpMSec, 0, 999);
        if ASource.IndexOf('zzz', AStartIndex) = AStartIndex then
        begin
          FormatSection.DisplayFormat := 'zzz';
          FormatSection.DisplayLength := 3;
          Inc(AStartIndex, 2);
        end
        else
        begin
          FormatSection.DisplayFormat := 'z';
          FormatSection.DisplayLength := 0;
        end;
      end;
      { A.M. or P.M. }
      'a':
      begin
        FormatSection := TDateTimeFormatPartInfo.Create(TDateTimePart.tpAMPM, 0, 1);
        if ASource.IndexOf('ampm', AStartIndex) = AStartIndex then
        begin
          FormatSection.DisplayFormat := 'ampm';
          FormatSection.DisplayLength := 0;
          Inc(AStartIndex, 3);
        end;
        if ASource.IndexOf('am/pm', AStartIndex) = AStartIndex then
        begin
          FormatSection.DisplayFormat := 'am/pm';
          FormatSection.DisplayLength := 0;
          Inc(AStartIndex, 4);
        end;
        if ASource.IndexOf('a/p', AStartIndex) = AStartIndex then
        begin
          FormatSection.DisplayFormat := 'a/p';
          FormatSection.DisplayLength := 0;
          Inc(AStartIndex, 2);
        end;
      end;
    end;
    FFormatParts.Add(FormatSection);
    Inc(AStartIndex);
  end;

var
  FormatTmp: string;
  FormatSection: TDateTimeFormatPartInfo;
  I: Integer;
  Ch: Char;
  HourFormatType: TDateTimePart;
  StateParser: TStateParser;
  StrBuffer: string;
  IndexFirstEditableFormatPart: Integer;
begin
  FormatTmp := AFormat.ToLower;
  FFormatParts.Clear;

  // Define format Hour 24 or 12
  if FormatTmp.Contains('am/pm') or FormatTmp.Contains('a/p') or FormatTmp.Contains('ampm') then
    HourFormatType := TDateTimePart.tpHour12
  else
    HourFormatType := TDateTimePart.tpHour24;

  I := 0;
  IndexFirstEditableFormatPart := -1;
  StateParser := TStateParser.spNone;
  StrBuffer := '';
  while I <= FormatTmp.Length - 1 do
  begin
    Ch := FormatTmp.Chars[I];
    case StateParser of
      spNone:
        begin
          // Is open quote for comment?
          if Ch = '''' then
          begin
            StateParser := TStateParser.spComment;
            StrBuffer := '';
            Inc(I);
          end
          else
          if not CharInSet(Ch, DATETIME_FORMAT_SYMBOLS) then
          begin
            StateParser := TStateParser.spNoneFormat;
            StrBuffer := '';
          end
          else
            StateParser := TStateParser.spFormat;
        end;
      // Comments is a part of string concluded in ''
      spComment:
      begin
        // Is close quote for comment?
        if Ch = '''' then
        begin
          FormatSection := TDateTimeFormatPartInfo.Create(TDateTimePart.tpUnknow);
          FormatSection.DisplayFormat := StrBuffer;
          FormatSection.DisplayLength := StrBuffer.Length;
          FFormatParts.Add(FormatSection);
          StateParser := TStateParser.spNone;
        end
        else
          StrBuffer := StrBuffer + AFormat.Chars[I];
        Inc(I);
      end;
      // NoneFormat is a part of string which doesn't include date time format symbols
      spNoneFormat:
      begin
        if CharInSet(Ch, DATETIME_FORMAT_SYMBOLS) or (Ch = '''') then
        begin
          FormatSection := TDateTimeFormatPartInfo.Create(TDateTimePart.tpUnknow);
          FormatSection.DisplayFormat := StrBuffer;
          FormatSection.DisplayLength := StrBuffer.Length;
          FFormatParts.Add(FormatSection);
          StateParser := TStateParser.spNone;
        end
        else
        begin
          StrBuffer := StrBuffer + AFormat.Chars[I];
          Inc(I);
        end;
      end;
      spFormat:
      begin
        ParseTime(FormatTmp, HourFormatType, I);
        if IndexFirstEditableFormatPart = -1 then
          IndexFirstEditableFormatPart := FFormatParts.Count - 1;
        StateParser := TStateParser.spNone;
      end;
    end;
  end;
  { Define Current index of DateTimePart}
  if FFormatParts.Count > 0 then
    SetIndexTimePart(IndexFirstEditableFormatPart)
  else
    SetIndexTimePart(-1);
  UpdateTimeSections;
  CalculateTextSize;
end;

procedure TCustomTimeEdit.PasteFromClipboard;
var
  TimeTmp: TDateTime;
  BufferString: string;
begin
  if not Assigned(FClipboardService) or not FClipboardService.GetClipboard.IsType<string>
    or not CanInputTime then
    Exit;

  BufferString := FClipboardService.GetClipboard.AsString;
  if TryStrToTime(BufferString, TimeTmp) then
    Time := TimeOf(TimeTmp);
end;

procedure TCustomTimeEdit.SetCustomFormat(const Value: string);
begin
  if Format <> Value then
  begin
    FCustomFormat := Value;
    ParseFormat(GetFormat);
    SetText(FormatDateTime(GetFormat, Time));
  end;
end;

procedure TCustomTimeEdit.SetIndexTimePart(const Value: Integer);
begin
  if FIndexCurrentPart <> Value then
  begin
    FIndexCurrentPart := Value;
    FContinueEnteringValue := False;
    FCurrentValue := '';
    Repaint;
  end;
end;

procedure TCustomTimeEdit.SetIsChecked(const Value: Boolean);
begin
  if FIsChecked <> Value then
  begin
    FIsChecked := Value;
    if HasCheckBox then
      FCheck.IsChecked := FIsChecked;
  end;
end;

procedure TCustomTimeEdit.SetKeyboardType(Value: TVirtualKeyboardType);
begin
  FKeyboardType := Value;
end;

procedure TCustomTimeEdit.SetReturnKeyType(Value: TReturnKeyType);
begin
  FReturnKeyType := Value;
end;

procedure TCustomTimeEdit.SetShowCheckBox(const Value: Boolean);
begin
  if FShowCheckBox <> Value then
  begin
    FShowCheckBox := Value;
    UpdateTextOpacity;
    if HasCheckBox then
      FCheck.Visible := Value;
  end;
end;

procedure TCustomTimeEdit.SetText(const AValue: string);
var
  TempTime: TDateTime;
begin
  inherited SetText(AValue);
  if TryStrToTime(AValue, TempTime, FormatSettings) then
    if TimeOf(TempTime) <> FTime then
      ChangingTime;
end;

procedure TCustomTimeEdit.SetTime(const Value: TTime);
begin
  if not SameValue(FTime, Value) then
  begin
    FTime := Value;
    Text := FormatDateTime(GetFormat, Time);
    UpdateTimeSections;
    inherited Change;
    CalculateTextSize;
    Repaint;
  end;
end;

procedure TCustomTimeEdit.SetTimeFormat(const Value: TDTTimeFormat);
begin
  if FTimeFormatKind <> Value then
  begin
    FTimeFormatKind := Value;
    ParseFormat(GetFormat);
    Text := FormatDateTime(GetFormat, Time);
    Repaint;
  end;
end;

procedure TCustomTimeEdit.SetUseNowTime(const Value: Boolean);
begin
  if UseNowTime <> Value then
  begin
    FUseNowTime := Value;
    if Assigned(FTimerServise) and not (csDesigning in ComponentState) then
      if UseNowTime then
      begin
        FTimerHandle := FTimerServise.CreateTimer(1 * MSecsPerSec, DoTimer);
        Time := System.SysUtils.Time;
        ClosePicker;
      end
      else
        FTimerServise.DestroyTimer(FTimerHandle);
  end;
end;

procedure TCustomTimeEdit.SynchronizeDateTimeParts(
  const ADateTimePartInfo: TDateTimeFormatPartInfo);
var
  Hour: Word;
  Min: Word;
  Sec: Word;
  MSec: Word;
begin
  DecodeTime(Time, Hour, Min, Sec, MSec);
  case ADateTimePartInfo.Part of
    TDateTimePart.tpHour24,
    TDateTimePart.tpHour12:
      Hour := ADateTimePartInfo.Value;
    TDateTimePart.tpMin:
      Min := ADateTimePartInfo.Value;
    TDateTimePart.tpSec:
      Sec := ADateTimePartInfo.Value;
    TDateTimePart.tpMSec:
      MSec := ADateTimePartInfo.Value;
    TDateTimePart.tpAMPM:
      begin
        if (Hour >= 12) and ADateTimePartInfo.IsAM then
          Hour := Hour - 12;
        if (Hour < 12) and not ADateTimePartInfo.IsAM then
          Hour := Hour + 12
      end;
    TDateTimePart.tpUnknow: ;
  end;
  Time := EncodeTime(Hour, Min, Sec, MSec);
  CalculateTextSize;
end;

procedure TCustomTimeEdit.UpdateTextOpacity;
begin
  if HasTextObject then
  begin
    if ShowCheckBox then
      TextObject.Enabled := IsChecked
    else
      TextObject.Enabled := True;
  end;
end;

procedure TCustomTimeEdit.UpdateTimeSections;
var
  TimeSection: TDateTimeFormatPartInfo;
  Hour: Word;
  Min: Word;
  Sec: Word;
  MSec: Word;
  I: Integer;
begin
  DecodeTime(FTime, Hour, Min, Sec, MSec);
  for I := 0 to FFormatParts.Count - 1 do
  begin
    TimeSection := FFormatParts[I];
    case TimeSection.Part of
      TDateTimePart.tpHour24,
      TDateTimePart.tpHour12:
        TimeSection.Value := Hour;
      TDateTimePart.tpMin:
        TimeSection.Value := Min;
      TDateTimePart.tpSec:
        TimeSection.Value := Sec;
      TDateTimePart.tpMSec:
        TimeSection.Value := MSec;
      TDateTimePart.tpAMPM:
        TimeSection.Value := Hour div 12;
      TDateTimePart.tpUnknow: ;
    end;
    FFormatParts[I] := TimeSection;
  end;
end;

{ TDateTimePartInfo }

constructor TDateTimeFormatPartInfo.Create(const APart: TDateTimePart;
  const AMin, AMax: Integer);
begin
  Self.Part := APart;
  Self.Range.Min := AMin;
  Self.Range.Max := AMax;
  Self.Value := 0;
end;

constructor TDateTimeFormatPartInfo.Create(const APart: TDateTimePart);
begin
  Self.Part := APart;
end;

function TDateTimeFormatPartInfo.GetActualLength: Integer;

  function Convert24HourTo12Hour(const AHour24: Integer): Integer;
  begin
    if AHour24 = 0 then
      Result := 12
    else if AHour24 > 12 then
      Result := AHour24 - 12
    else
      Result := AHour24;
  end;

begin
  case Part of
    tpHour12:
      Result := Max(DisplayLength, IntToStr(Convert24HourTo12Hour(Value)).Length);
    tpAMPM:
      Result := Max(DisplayLength, IfThen(IsAM, FormatSettings.TimeAMString.Length, FormatSettings.TimePMString.Length));
  else
    Result := Max(DisplayLength, IntToStr(Value).Length);
  end;
end;

function TDateTimeFormatPartInfo.ToAMPMString: string;
begin
  if IsAM then
    Result := FormatSettings.TimeAMString
  else
    Result := FormatSettings.TimePMString;
end;

function TDateTimeFormatPartInfo.IsAM: Boolean;
begin
  Result := (Part in [TDateTimePart.tpHour24, TDateTimePart.tpHour12]) and (Value <= 12) or
            (Part = TDateTimePart.tpAMPM) and (Value = 0);
end;

function TDateTimeFormatPartInfo.IsSpecifiedFixedLength: Boolean;
begin
  Result := DisplayLength > 0;
end;

{ TCalendar }

procedure TCalendar.ApplyStyle;
var
  StyledControl: TFmxObject;
  i: Integer;
  AYear, AMonth, ADay: Word;
  LLabel: TLabel;
  LItem: TListBoxItem;
begin
  inherited;
  BeginUpdate;
  try
    DecodeDate(FDateTime, AYear, AMonth, ADay);
    //FPrev
    StyledControl := FindStyleResource('prev');
    FPrev := nil;
    if Assigned(StyledControl) and (StyledControl is TButton) then
    begin
      FPrev := TButton(StyledControl);
      FPrev.CanParentFocus := True;
      FPrev.OnClick := DoPrevClick;
    end;
    //FToday
    StyledControl := FindStyleResource('today');
    FToday := nil;
    if Assigned(StyledControl) and (StyledControl is TButton) then
    begin
      FToday := TButton(StyledControl);
      FToday.CanParentFocus := True;
      FToday.OnClick := DoTodayClick;
    end;
    //FNext
    StyledControl := FindStyleResource('next');
    FNext := nil;
    if Assigned(StyledControl) and (StyledControl is TButton) then
    begin
      FNext := TButton(StyledControl);
      FNext.CanParentFocus := True;
      FNext.OnClick := DoNextClick;
    end;
    //FMonths
    StyledControl := FindStyleResource('months');
    FMonths := nil;
    if Assigned(StyledControl) and (StyledControl is TPopupBox) then
    begin
      FMonths := TPopupBox(StyledControl);
      FMonths.CanParentFocus := True;
      for i := 1 to 12 do
        FMonths.Items.Add(FormatSettings.LongMonthNames[i]);
      FMonths.ItemIndex := AMonth - 1;
      FMonths.OnChange := DoMonthChange;
    end;
    //FYears
    StyledControl := FindStyleResource('years');
    FYears := nil;
    if Assigned(StyledControl) and (StyledControl is TPopupBox) then
    begin
      FYears := TPopupBox(StyledControl);
      FYears.BeginUpdate;
      try
        FYears.CanParentFocus := True;
        for i := 1 to 10 do
          FYears.Items.Add(IntToStr(AYear - i));
        FYears.Items.Add(IntToStr(AYear));
        for i := 1 to 10 do
          FYears.Items.Add(IntToStr(AYear + i));
        FYears.ItemIndex := 10;
        FYears.OnChange := DoYearChange;
      finally
        FYears.EndUpdate;
      end;
    end;
    //FWeek
    StyledControl := FindStyleResource('week');
    FWeek := nil;
    if Assigned(StyledControl) and (StyledControl is TGridLayout) then
    begin
      FWeek := TGridLayout(StyledControl);
      FWeek.CanParentFocus := True;
      for i := 0 to 6 do
      begin
        LLabel := TLabel.Create(Self);
        LLabel.Parent := FWeek;
        LLabel.Locked := True;
        LLabel.Stored := False;
        LLabel.TextAlign := TTextAlign.taCenter;
        LLabel.WordWrap := False;
        LLabel.StyleLookup := 'calendarlabelstyle';
      end;
      FWeek.ItemWidth := FWeek.Width / 7 - 0.1;
    end;
    //FWeeks
    StyledControl := FindStyleResource('weeks');
    FWeeks := nil;
    if Assigned(StyledControl) and (StyledControl is TGridLayout) then
    begin
      FWeeks := TGridLayout(StyledControl);
      FWeeks.CanParentFocus := True;
      FWeeks.Visible := WeekNumbers;
      FWeeks.ItemWidth := FWeeks.Width;
      for i := 0 to 5 do
      begin
        LLabel := TLabel.Create(Self);
        LLabel.Parent := FWeeks;
        LLabel.Locked := True;
        LLabel.Stored := False;
        LLabel.TextAlign := TTextAlign.taCenter;
        LLabel.WordWrap := False;
      end;
    end;
    //FDays
    StyledControl := FindStyleResource('days');
    FDays := nil;
    if Assigned(StyledControl) and (StyledControl is TListBox) then
    begin
      FDays := TListBox(StyledControl);
      FDays.AniCalculations.TouchTracking := [];
      FDays.CanParentFocus := True;
      FDays.OnMouseDown := DoDaysMouseDown;
      FDays.OnMouseUp := DoDaysMouseUp;
      FDays.OnChange := DoDayChange;
      for i := 1 to 6 * 7 do
      begin
        LItem := TListBoxItem.Create(Self);
        LItem.Parent := FDays;
        LItem.Locked := True;
        LItem.Stored := False;
        LItem.TextAlign := TTextAlign.taTrailing;
        LItem.WordWrap := False;
      end;
    end;
    FillList;
    if FWeek <> nil then
      FWeek.ItemWidth := FWeek.Width / 7 - 0.1;

  finally
    EndUpdate;
  end;
end;

constructor TCalendar.Create(AOwner: TComponent);
begin
  inherited;
  SetAcceptsControls(False);
  Width := 180;
  Height := 160;
  FDateTime := Now;
  FFirstDayOfWeek := TCalDayOfWeek.dowLocaleDefault;
  CanFocus := True;
  DisableFocusEffect := False;
end;

function TCalendar.GetData: TValue;
begin
  Result := TValue.From<TDateTime>(DateTime);
end;

procedure TCalendar.SetData(const Value: TValue);
var
  D: TDateTime;
begin
  if Value.IsType<TDateTime> then
    Date := Value.AsType<TDateTime>
  else if TryStrToDateTime(Value.ToString, D) then
    Date := D;
end;

function WeekOfYear(aDate: TDateTime): byte;
var
  t, m, year: Word;
  newyear: TDateTime;
  KW: Word;
  wtag_ny: Word;
begin
  DecodeDate(aDate, year, m, t); // calc year
  newyear := EncodeDate(year, 1, 1); // calc 1.1.year
  wtag_ny := ord(DayofWeek(newyear)); // DOW of 1.1.year
  KW := Trunc(((aDate - newyear + ((wtag_ny + 1) Mod 7) - 3) / 7) + 1);
  if (KW = 0) then
  begin
    KW := 0;
  end;
  Result := KW;
end;

procedure TCalendar.FillList;
var
  i: Integer;
  AYear, PreMonth, AMonth, ADay: Word;
  Date: TDate;
  First: Integer;
  A: string;
  Item: TListBoxItem;
  LocaleService: IFMXLocaleService;
begin
  if not Assigned(FMonths) or not Assigned(FYears) or not Assigned(FWeek) or
     (FWeekNumbers and not Assigned(FWeeks)) or not Assigned(FDays) then
    Exit;
  FDisableDayChange := FDisableDayChange + 1;
  try
    { first day }
    if FFirstDayOfWeek = TCalDayOfWeek.dowLocaleDefault then
    begin
      if TPlatformServices.Current.SupportsPlatformService(IFMXLocaleService, IInterface(LocaleService)) then
        A := LocaleService.GetLocaleFirstDayOfWeek
      else
        A := '6';
      FFirstDayOfWeekNum := ord(A.Chars[0]) - ord('0');
      {$IFDEF MACOS}
      FFirstDayOfWeekNum:= ord(A.Chars[0]) + ord('0');
      {$ENDIF}
    end
    else
      FFirstDayOfWeekNum := ord(FFirstDayOfWeek);
    FFirstDayOfWeekNum := (8 + FFirstDayOfWeekNum) mod 7;
    { week days }
    for i := 0 to 6 do
      TLabel(FWeek.Children[i]).Text := FormatSettings.ShortDayNames
        [1 + ((7 + i + FFirstDayOfWeekNum) mod 7)];
    { days }
    DecodeDate(FDateTime, AYear, AMonth, ADay);
    PreMonth := AMonth - 1;
    if PreMonth < 1 then
      PreMonth := 12;
    Date := EncodeDate(AYear, AMonth, 1);
    First := DayofWeek(Date);
    if First - FFirstDayOfWeekNum < 3 then
      First := First + 7;
    if FDays.Count - (First + MonthDays[IsLeapYear(AYear), AMonth] -
      FFirstDayOfWeekNum) < 3 then
      First := First - 7;
    FDays.Tag := First; // store first
    FDays.ItemDown := nil;
    for i := 1 to First do
    begin
      Item := FDays.ListItems[i - 1];
      if Item = nil then
        Continue;
      Item.Opacity := 0.3;
      Item.Text := IntToStr(MonthDays[IsLeapYear(AYear), PreMonth] - First + i +
        1 + FFirstDayOfWeekNum);
    end;
    for i := 1 to MonthDays[IsLeapYear(AYear), AMonth] do
    begin
      Item := FDays.ListItems[First + i - 2 - FFirstDayOfWeekNum];
      if Item = nil then
        Continue;
      Item.Opacity := 1;
      Item.Text := IntToStr(i);
    end;
    for i := First + MonthDays[IsLeapYear(AYear), AMonth] to FDays.Count +
      FFirstDayOfWeekNum do
    begin
      Item := FDays.ListItems[i - 1 - FFirstDayOfWeekNum];
      if Item = nil then
        Continue;
      Item.Opacity := 0.3;
      Item.Text := IntToStr(i - First - MonthDays[IsLeapYear(AYear),
        AMonth] + 1);
    end;
    { weeks number }
    if FWeekNumbers then
    begin
      FWeeks.Visible := True;
      DecodeDate(FDateTime, AYear, AMonth, ADay);
      Date := EncodeDate(AYear, AMonth, 1);
      for i := 0 to 5 do
        if WeekOfYear(Date) + i = 0 then
          TLabel(FWeeks.Children[i]).Text := IntToStr(52)
        else if WeekOfYear(Date) = 0 then
          TLabel(FWeeks.Children[i]).Text := IntToStr(i)
        else if WeekOfYear(Date) + i > 52 then
          TLabel(FWeeks.Children[i]).Text := IntToStr(WeekOfYear(Date) + i - 52)
        else
          TLabel(FWeeks.Children[i]).Text := IntToStr(WeekOfYear(Date) + i);
    end
    else
      FWeeks.Visible := False;
    { selection }
    FDays.ItemIndex := First + ADay - 2 - FFirstDayOfWeekNum;
    { month }
    FMonths.ItemIndex := AMonth - 1;
    { years }
    FYears.BeginUpdate;
    try
      FYears.Items.Clear;
      for i := 10 downto 1 do
        FYears.Items.Add(IntToStr(AYear - i));
      FYears.Items.Add(IntToStr(AYear));
      for i := 1 to 10 do
        FYears.Items.Add(IntToStr(AYear + i));
      FYears.Text := IntToStr(AYear);
    finally
      FYears.EndUpdate;
    end;
  finally
    FDisableDayChange := FDisableDayChange - 1;
  end;
end;

procedure TCalendar.FreeStyle;
begin
  inherited;
  FPrev := nil;
  FToday := nil;
  FNext := nil;
  FMonths := nil;
  FYears := nil;
  FWeek := nil;
  FWeeks := nil;
  FDays := nil;
end;

procedure TCalendar.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCalendar.DoDateSelected;
begin
  if Assigned(FOnDateSelected) then
    FOnDateSelected(Self);
end;

procedure TCalendar.DoDayChange(Sender: TObject);
var
  AYear, AMonth, ADay: Word;
  DaysToAdd: Integer;
begin
  if not Assigned(FDays) then
    Exit;
  DecodeDate(FDateTime, AYear, AMonth, ADay);
  if FDays.ItemIndex - FDays.Tag + 2 + FFirstDayOfWeekNum < 1 then
  begin
    DaysToAdd := FDays.ItemIndex - FDays.Tag + 1 + FFirstDayOfWeekNum;
    Date := IncDay(EncodeDate(AYear, AMonth, 1), DaysToAdd);
  end
  else if FDays.ItemIndex - FDays.Tag + 2 + FFirstDayOfWeekNum >
    MonthDays[IsLeapYear(AYear), AMonth] then
  begin
    DaysToAdd := (FDays.ItemIndex - FDays.Tag + 2 + FFirstDayOfWeekNum) -
      MonthDays[IsLeapYear(AYear), AMonth];
    Date := IncDay(EncodeDate(AYear, AMonth, MonthDays[IsLeapYear(AYear), AMonth]), DaysToAdd);
  end
  else
    Date := EncodeDate(AYear, AMonth, FDays.ItemIndex - FDays.Tag + 2 +
      FFirstDayOfWeekNum);
  if FDisableDayChange = 0 then
    DoDateSelected;
end;

procedure TCalendar.DoDaysMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if FDisableDayChange = 0 then
    Inc(FDisableDayChange);
end;

procedure TCalendar.DoDaysMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if FDisableDayChange > 0 then
  begin
    Dec(FDisableDayChange);
    if (FDisableDayChange = 0) and FNeedChange then
    begin
      FNeedChange := False;
      DoChange;
      DoDateSelected;
    end;
  end;
end;

procedure TCalendar.DoDeleteChildren;
begin
  inherited;
  FPrev := nil;
  FToday := nil;
  FNext := nil;
  FMonths := nil;
  FYears := nil;
  FWeek := nil;
  FWeeks := nil;
  FDays := nil;
end;

procedure TCalendar.DoTodayClick(Sender: TObject);
begin
  Date := Now;
  DoChange;
  if FDisableDayChange = 0 then
    DoDateSelected;
end;

procedure TCalendar.DoNextClick(Sender: TObject);
begin
  Date := IncMonth(Date, 1);
  DoChange;
end;

procedure TCalendar.DoPrevClick(Sender: TObject);
begin
  Date := IncMonth(Date, -1);
  DoChange;
end;

procedure TCalendar.DoMonthChange(Sender: TObject);
var
  AYear, AMonth, ADay: Word;
begin
  DecodeDate(FDateTime, AYear, AMonth, ADay);
  Date := EncodeDate(AYear, FMonths.ItemIndex + 1, ADay);
  DoChange;
end;

procedure TCalendar.DoYearChange(Sender: TObject);
var
  AYear, AMonth, ADay: Word;
begin
  DecodeDate(FDateTime, AYear, AMonth, ADay);
  Date := EncodeDate(StrToInt(FYears.Text), AMonth, ADay);
  DoChange;
end;

procedure TCalendar.DoRealign;
begin
  inherited;
  if FWeek <> nil then
    FWeek.ItemWidth := FWeek.Width / 7 - 0.1;
end;

function TCalendar.GetDate: TDate;
begin
  Result := Trunc(FDateTime);
end;

procedure TCalendar.KeyDown(var Key: Word; var KeyChar: WideChar;
  Shift: TShiftState);
begin
  inherited;
  case Key of
    vkLeft:
      begin
        if ([ssCtrl, ssCommand] * Shift) <> [] then
          Date := IncMonth(Date, -1)
        else
          Date := IncDay(Date, -1);
        DoChange;
        Key := 0;
        KeyChar := #0;
      end;
    vkRight:
      begin
        if ([ssCtrl, ssCommand] * Shift) <> [] then
          Date := IncMonth(Date, -1)
        else
          Date := IncDay(Date, 1);
        DoChange;
        Key := 0;
        KeyChar := #0;
      end;
    vkUp:
      if ([ssCtrl, ssCommand] * Shift) = [] then
      begin
        Date := IncDay(Date, -7);
        DoChange;
        Key := 0;
        KeyChar := #0;
      end;
    vkDown:
      if ([ssCtrl, ssCommand] * Shift) = [] then
      begin
        Date := IncDay(Date, 7);
        DoChange;
        Key := 0;
        KeyChar := #0;
      end;
  end;
end;

procedure TCalendar.SetDate(Value: TDate);
begin
  FDisableDayChange := FDisableDayChange + 1;
  ReplaceTime(TDateTime(Value), FDateTime);
  try
    SetDateTime(Value);
  except
    SetDateTime(FDateTime);
    raise;
  end;
  FDisableDayChange := FDisableDayChange - 1;
end;

procedure TCalendar.SetDateTime(const Value: TDateTime);
begin
  if FDateTime <> Value then
  begin
    FDateTime := Value;
    if (not (csDestroying in ComponentState)) and (not Released) then
    begin
      FillList;
      if FDisableDayChange = 0 then
        DoChange
      else
        FNeedChange := True;
    end;
  end;
end;

procedure TCalendar.SetFirstDayOfWeek(const Value: TCalDayOfWeek);
begin
  if FFirstDayOfWeek <> Value then
  begin
    FFirstDayOfWeek := Value;
    FillList;
  end;
end;

procedure TCalendar.SetTodayDefault(const Value: Boolean);
begin
  FTodayDefault := Value;
  if FTodayDefault then
    Date := Now;
end;

procedure TCalendar.SetWeekNumbers(const Value: Boolean);
begin
  if FWeekNumbers <> Value then
  begin
    FWeekNumbers := Value;
    FillList;
    Realign;
  end;
end;

procedure TCalendar.MouseWheel(Shift: TShiftState; WheelDelta: Integer;
  var Handled: Boolean);
begin
  inherited;
  if not Handled then
  begin
    if WheelDelta > 0 then
      Date := IncMonth(Date, -1)
    else
      Date := IncMonth(Date, 1);
    Handled := True;
  end;
end;

{ TCalendarEdit }

procedure TCustomCalendarEdit.Change;
var
  Converted: Boolean;
  TempDateTime: TDateTime;
begin
  if FNeedChange then
  begin
    Converted := TryStrToDate(Text, TempDateTime, FormatSettings);
    TempDateTime := DateOf(TempDateTime);

    if Converted then
      FDateTime := TempDateTime
    else
      SetTextInternal(GetFormattedDateTimeText(FDateTime));
    RepaintEdit;
  end;
  inherited Change;
end;

procedure TCustomCalendarEdit.ClosePicker;
begin
  if HasPicker and FDateTimePicker.IsShown then
    FDateTimePicker.Hide;
end;

constructor TCustomCalendarEdit.Create(AOwner: TComponent);
var
  PickerService: IFMXPickerService;
begin
  inherited Create(AOwner);
  if TPlatformServices.Current.SupportsPlatformService(IFMXPickerService, IInterface(PickerService)) then
  begin
    FDateTimePicker := PickerService.CreateDateTimePicker;
    FDateTimePicker.Parent := Self;
    FDateTimePicker.OnHide := DoPopupClose;
    FDateTimePicker.OnShow := DoPopup;
    FDateTimePicker.OnDateChanged := DoDateChanged;
  end;
  FMinDate := 0.0;
  FMaxDate := 0.0;
  FFirstDayOfWeek := TCalDayOfWeek.dowLocaleDefault;
  Date := Trunc(Now);
  FFilterChar := '0123456789./';
  KeyboardType := TVirtualKeyboardType.vktNumbersAndPunctuation;
  InputSupport := False;
end;

destructor TCustomCalendarEdit.Destroy;
begin
  if HasPicker and FDateTimePicker.IsShown then
    FDateTimePicker.Hide;
  FreeAndNil(FDateTimePicker);
  inherited;
end;

procedure TCustomCalendarEdit.DoDateChanged(Sender: TObject; const ADate: TDateTime);
var
  OldText: string;
begin
  if ReadOnly then
    Exit;

  FDateTime := DateOf(ADate);

  if Observers.IsObserving(TObserverMapping.EditLinkID) then
    if not TLinkObservers.EditLinkEdit(Observers) then
    begin
      DropDown;
      Exit;
    end;

  OldText := Text;
  SetTextInternal(GetFormattedDateTimeText(DateTime));
  CaretPosition := Text.Length;

  inherited Change;

  { Live Binding }
  if Observers.IsObserving(TObserverMapping.EditLinkID) then
    if (OldText <> Text) and TLinkObservers.EditLinkIsEditing(Observers) then
    begin
      TLinkObservers.EditLinkModified(Observers);
      TLinkObservers.EditLinkTrackUpdate(Observers);
    end;
  if Observers.IsObserving(TObserverMapping.ControlValueID) then
    if (OldText <> Text) then
    begin
      TLinkObservers.ControlValueModified(Observers);
      TLinkObservers.ControlValueTrackUpdate(Observers);
    end;
end;

procedure TCustomCalendarEdit.DoExit;
begin
  inherited DoExit;
  if InputSupport then
    Caret.Visible := True;
  if HasPicker then
    FDateTimePicker.Hide;
end;

procedure TCustomCalendarEdit.DoPopup(Sender: TObject);
begin
  if Assigned(FOnPopup) then
    FOnPopup(Self);
end;

procedure TCustomCalendarEdit.DoPopupClose(Sender: TObject);
var
  TextService: TTextService;
begin
  if not (csDestroying in ComponentState) then
  begin
    TextService := GetTextService;
    if InputSupport and Assigned(TextService) then
    begin
      SelStart := TextService.CombinedText.Length;
      SelLength := 0;
      Caret.Visible := True;
    end;
    if Assigned(FOnClosePopup) then
      FOnClosePopup(Self);
  end;
end;

function TCustomCalendarEdit.DoStoreDateConstraints(const Index: Integer): Boolean;
begin
  case Index of
    1: Result := SameValue(FMinDate, 0.0);
    2: Result := SameValue(FMaxDate, 0.0);
  else
    Result := True;
  end;
end;

procedure TCustomCalendarEdit.DropDown;
begin
  if HasPicker then
  begin
    if FDateTimePicker.IsShown then
      FDateTimePicker.Hide
    else
    begin
      SelLength := 0;
      if InputSupport then
        Caret.Visible := False;
      { Initialization of date picker before showing }
      FDateTimePicker.Date := DateTime;
      FDateTimePicker.MinDate := MinDate;
      FDateTimePicker.MaxDate := MaxDate;
      FDateTimePicker.ShowMode := TDatePickerShowMode.psmDate;
      FDateTimePicker.FirstDayOfWeek := FFirstDayOfWeek;
      FDateTimePicker.ShowWeekNumbers := FShowWeekNumbers;
      FDateTimePicker.TodayDefault := FTodayDefault;
      FDateTimePicker.Show;
    end;
  end;
end;

function TCustomCalendarEdit.GetDate: TDate;
begin
  Result := DateOf(FDateTime);
end;

function TCustomCalendarEdit.GetDefaultStyleLookupName: string;
begin
  if InputSupport then
    Result := 'comboeditstyle'
  else
    Result := 'comboboxstyle';
end;

function TCustomCalendarEdit.GetFormattedDateTimeText(const ADateTime: TDateTime): string;
begin
  Result := DateToStr(ADateTime);
end;

function TCustomCalendarEdit.HasPicker: Boolean;
begin
  Result := Assigned(FDateTimePicker);
end;

procedure TCustomCalendarEdit.SetDate(const Value: TDate);
begin
  if Date <> Value then
  begin
    FDateTime := Value + Time;
    SetTextInternal(GetFormattedDateTimeText(DateTime));
    inherited Change;
  end;
end;

procedure TCustomCalendarEdit.SetDateConstraints(const Index: Integer;
  const Value: TDateTime);
begin
  if (Index = 1) and not SameValue(FMaxDate, 0.0) and (Value > FMaxDate) then
    raise Exception.CreateFmt(SDateTimeMax, [DateToStr(FMaxDate)]);
  if (Index = 2) and not SameValue(FMinDate, 0.0) and (Value < FMinDate) then
    raise Exception.CreateFmt(SDateTimeMin, [DateToStr(FMinDate)]);

  case Index of
    1: FMinDate := Value;
    2: FMaxDate := Value;
  end;
end;

procedure TCustomCalendarEdit.SetDateTime(const Value: TDateTime);
begin
  if DateTime <> Value then
  begin
    FDateTime := Value;
    SetTextInternal(GetFormattedDateTimeText(DateTime));
    inherited Change;
  end;
end;

procedure TCustomCalendarEdit.SetInputSupport(const Value: Boolean);
var
  OldValue: Boolean;
begin
  OldValue := InputSupport;
  inherited SetInputSupport(Value);
  // Default, for same kind of input support we must use other kind of style
  // comboeditstyle and comboboxstyle
  if OldValue <> Value then
    DoStyleChanged;
end;

procedure TCustomCalendarEdit.SetText(const AValue: string);
var
  TempDateTime: TDateTime;
  Converted: Boolean;
begin
  Converted := TryStrToDate(AValue, TempDateTime, FormatSettings);
  if Converted then
  begin
    TempDateTime := DateOf(TempDateTime) + Time;
    if TempDateTime <> FDateTime then
      Change;
  end
  else
  begin
    if (AValue <> '') and (String.Compare(Name, AValue, True) <> 0) then
      raise EConvertError.CreateFMT(System.SysConst.SInvalidDate, [AValue]);
  end;
  inherited SetText(AValue);
end;

{ TCalendarBox }

constructor TCalendarBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InputSupport := False;
end;

initialization
  RegisterFmxClasses([TCalendar, TTimeEdit, TCalendarEdit]);

end.