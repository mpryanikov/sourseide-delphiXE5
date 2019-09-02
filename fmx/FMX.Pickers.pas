{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2012-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit FMX.Pickers;

interface

uses
  System.Classes, System.UITypes, System.Generics.Collections, System.SysUtils,
  FMX.Types, FMX.Controls, FMX.Consts;

type
  TPickerFactoryService = class;

  EFeatureError = class (Exception);

  TDropDownKind = (ddkCustom, ddkNative);

{ TCustomPicker }

  // Basis for creation of any pickers. Each picker belongs to any control.
  // Each picker is able to appear and desappear. Only one picker
  // can displayed (other closed).
  TCustomPicker = class abstract
  strict private
    FPickerService: TPickerFactoryService;
    FOnShow: TNotifyEvent;
    FOnHide: TNotifyEvent;
    FParent: TControl;
  protected
    procedure DoShow; virtual;
    procedure DoHide; virtual;
  public
    constructor Create(const APickerService: TPickerFactoryService); virtual;
    destructor Destroy; override;
    procedure Show; virtual;
    procedure Hide; virtual; abstract;
    function IsShown: Boolean; virtual;
  public
    property Parent: TControl read FParent write FParent;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;    
  end;

{ TCustomDateTimePicker }

  TDatePickerShowMode = (psmDate, psmTime, psmDateTime);

  TOnDateChanged = procedure (Sender: TObject; const ADateTime: TDateTime) of object;

  // Picker for choosen date 
  TCustomDateTimePicker = class abstract (TCustomPicker)
  strict private
    FDate: TDateTime;
    FMinDate: TDateTime;
    FMaxDate: TDateTime;
    FFirstDayOfWeek: TCalDayOfWeek;
    FShowMode: TDatePickerShowMode;
    FShowWeekNumbers: Boolean;
    FTodayDefault: Boolean;
    FOnDateChanged: TOnDateChanged;
  protected
    procedure SetShowMode(const AValue: TDatePickerShowMode); virtual;
    procedure SetMinDate(const AValue: TDateTime); virtual;
    procedure SetMaxDate(const AValue: TDateTime); virtual;
    procedure DoDateChanged(const ADateTime: TDateTime); virtual;
  public
    constructor Create(const APickerService: TPickerFactoryService); override;
    property Date: TDateTime read FDate write FDate;
    property MinDate: TDateTime read FMinDate write SetMinDate;
    property MaxDate: TDateTime read FMaxDate write SetMaxDate;
    property FirstDayOfWeek: TCalDayOfWeek read FFirstDayOfWeek write FFirstDayOfWeek;
    property ShowMode: TDatePickerShowMode read FShowMode write SetShowMode;
    property ShowWeekNumbers: Boolean read FShowWeekNumbers write FShowWeekNumbers;
    property TodayDefault: Boolean read FTodayDefault write FTodayDefault;
    property OnDateChanged: TOnDateChanged read FOnDateChanged write FOnDateChanged;
  end;

{ TCustomListPicker }

  TOnValueChanged = procedure (Sender: TObject; const AValueIndex: Integer) of object;

  { Picker for choosen string value from list of strings. }
  TCustomListPicker = class abstract (TCustomPicker)
  strict private
    FValues: TStrings;
    FCountVisibleItems: Integer;
    FItemIndex: Integer;
    FItemHeight: Single;
    FOnValueChanged: TOnValueChanged;
  protected
    procedure SetValues(AValues: TStrings); virtual;
    procedure SetItemIndex(AValue: Integer); virtual;
    procedure DoItemChanged(const AItemIndex: Integer); virtual;
  public
    constructor Create(const APickerService: TPickerFactoryService); override;
    destructor Destroy; override;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property ItemHeight: Single read FItemHeight write FItemHeight;
    property CountVisibleItems: Integer read FCountVisibleItems write FCountVisibleItems;
    property Values: TStrings read FValues write SetValues;
    property OnValueChanged: TOnValueChanged read FOnValueChanged write FOnValueChanged;
  end;

{ Picker Factory Service }

  { Factory interface for creation picker instance and close all pickers }
  IFMXPickerService = interface (IInterface)
    ['{AE1A8D3C-D5FE-4343-A7B0-2AAEDA3ABB8A}']
    function CreateDateTimePicker: TCustomDateTimePicker;
    function CreateListPicker: TCustomListPicker;
    procedure CloseAllPickers;
  end;

  { Factory of creating pickers: Date Time Picker and List Picker }
  TPickerFactoryService = class abstract (TInterfacedObject, IFMXPickerService)
  strict private
    FPickers: TList<TCustomPicker>;
  protected
    function DoCreateDateTimePicker: TCustomDateTimePicker; virtual; abstract;
    function DoCreateListPicker: TCustomListPicker; virtual; abstract;
    procedure DoPickerRemoving(const APicker: TCustomPicker); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    { IFMXPickerService }
    function CreateDateTimePicker: TCustomDateTimePicker;
    function CreateListPicker: TCustomListPicker;
    procedure CloseAllPickers;
  end;

implementation

uses
  System.Math,
{$IFDEF IOS}
  FMX.Pickers.iOS;
{$ELSE}
  {$IFDEF ANDROID}
    FMX.Pickers.Android;
  {$ELSE}
    FMX.Pickers.Default;
  {$ENDIF}
{$ENDIF}

{ TCustomListPicker }

constructor TCustomListPicker.Create(const APickerService: TPickerFactoryService);
begin
  inherited Create(APickerService);
  FValues := TStringList.Create;
end;

destructor TCustomListPicker.Destroy;
begin
  FreeAndNil(FValues);
  inherited Destroy;
end;

procedure TCustomListPicker.DoItemChanged(const AItemIndex: Integer);
begin
  if Assigned(FOnValueChanged) then
    FOnValueChanged(Parent, AItemIndex);
end;

procedure TCustomListPicker.SetItemIndex(AValue: Integer);
begin
  FItemIndex := AValue;
end;

procedure TCustomListPicker.SetValues(AValues: TStrings);
begin
  FValues.Assign(AValues);
end;

{ TPickerFactoryService }

procedure TPickerFactoryService.CloseAllPickers;
var
  Picker: TCustomPicker;
begin
  for Picker in FPickers do
    Picker.Hide;
end;

constructor TPickerFactoryService.Create;
begin
  inherited Create;
  FPickers := TList<TCustomPicker>.Create;
end;

function TPickerFactoryService.CreateDateTimePicker: TCustomDateTimePicker;
begin
  Result := DoCreateDateTimePicker;
  FPickers.Add(Result);
end;

function TPickerFactoryService.CreateListPicker: TCustomListPicker;
begin
  Result := DoCreateListPicker;
  FPickers.Add(Result);
end;

destructor TPickerFactoryService.Destroy;
begin
  FreeAndNil(FPickers);
  inherited Destroy;
end;

procedure TPickerFactoryService.DoPickerRemoving(const APicker: TCustomPicker);
begin
  if Assigned(FPickers) and Assigned(APicker) then
    FPickers.Remove(APicker);
end;

{ TCustomPicker }

constructor TCustomPicker.Create(const APickerService: TPickerFactoryService);
begin
  Assert(Assigned(APickerService), 'APickerService can not be a nil');
  FPickerService := APickerService;
end;

destructor TCustomPicker.Destroy;
begin
  if Assigned(FPickerService) then
    FPickerService.DoPickerRemoving(Self);
  inherited Destroy;
end;

procedure TCustomPicker.DoHide;
begin
  if Assigned(FOnHide) then
    FOnHide(Parent);
end;

procedure TCustomPicker.DoShow;
begin
  if Assigned(FOnShow) then
    FOnShow(Parent);
end;

function TCustomPicker.IsShown: Boolean;
begin
  Result := False;
end;

procedure TCustomPicker.Show;
begin
  FPickerService.CloseAllPickers;
end;

{ TCustomDateTimePicker }

constructor TCustomDateTimePicker.Create(
  const APickerService: TPickerFactoryService);
begin
  inherited Create(APickerService);
  FMinDate := 0.0;
  FMaxDate := 0.0;
end;

procedure TCustomDateTimePicker.DoDateChanged(const ADateTime: TDateTime);
begin
  if Assigned(FOnDateChanged) then
    FOnDateChanged(Parent, ADateTime);
end;

procedure TCustomDateTimePicker.SetMaxDate(const AValue: TDateTime);
begin
  FMaxDate := AValue;
end;

procedure TCustomDateTimePicker.SetMinDate(const AValue: TDateTime);
begin
  FMinDate := AValue;
end;

procedure TCustomDateTimePicker.SetShowMode(const AValue: TDatePickerShowMode);
begin
  FShowMode := AValue;
end;

initialization
  RegisterPickersService;

end.
