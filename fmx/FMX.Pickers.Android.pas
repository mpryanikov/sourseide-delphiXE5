{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit FMX.Pickers.Android;

interface

procedure RegisterPickersService;
procedure UnregisterPickersService;

implementation

uses
  System.Classes, System.SysUtils, System.DateUtils, System.SyncObjs,
  FMX.Platform, FMX.Platform.Android, FMX.Pickers, FMX.Helpers.Android,
  FMX.Controls, FMX.Consts,
  Androidapi.JNIBridge, Androidapi.JNI.App, Androidapi.JNI.Support,
  Androidapi.JNI.JavaTypes, Androidapi.JNI.Embarcadero, FMX.Styles, FMX.Types;

const
  ANDROID_LIGHT_THEME = '[LIGHTSTYLE]';
  ANDROID_DARK_THEME = '[DARKSTYLE]';
  
type

{ TAndroidDateTimePicker }

  TDateTimeChangedListener = class;

  TOnDateTimeChangedCallback = procedure (const ADateTime: TDateTime) of object;
  TOnCallback = procedure of object;
  
  TAndroidDateTimePicker = class (TCustomDateTimePicker)
  strict private
    FNativeDatePicker: JBaseDateTimePicker;
    FNativeTimePicker: JBaseDateTimePicker;
    FDateTimeChangedListener: TDateTimeChangedListener;
  protected
    procedure SetShowMode(const AValue: TDatePickerShowMode); override;
    procedure DoDateChanged(const ADateTime: TDateTime); override;
  public
    constructor Create(const APickerService: TPickerFactoryService); override;
    destructor Destroy; override;
    procedure Show; override;
    procedure Hide; override;
    function IsShown: Boolean; override;
  end;

  { TDateTimeChangedListener }

  TDateTimeChangedListener = class (TJavaLocal, JOnDateTimeChangedListener)
  strict private
    FChangedCallback: TOnDateTimeChangedCallback;
    FOpenCallback: TOnCallback;
    FCloseCallback: TOnCallback;
    FDateTime: TDateTime;
  protected
    procedure DoDateChanged;
    procedure DoShow;
    procedure DoHide;
  public
    constructor Create(AChangeCallback: TOnDateTimeChangedCallback; AOpenCallback: TOnCallback; CallInUIThreadWithWaitFinishing: TOnCallback); overload;
    { JOnDateTimeChangedListener }
    procedure onDateChanged(date: JDate); cdecl;
    procedure onShow; cdecl;
    procedure onHide; cdecl;
  end;

{ TAndroidListPicker }

  TListChangedListener = class;

  TAndroidListPicker = class (TCustomListPicker)
  strict private
    FNativeListPicker: JBaseListPicker;
    FItemChangedListener: TListChangedListener;
  protected
    procedure SetValues(AValues: TStrings); override;
    procedure SetValuesToNativePicker(AValues: TStrings); virtual;
  public
    constructor Create(const APickerService: TPickerFactoryService); override;
    destructor Destroy; override;
    procedure Show; override;
    procedure Hide; override;
    function IsShown: Boolean; override;
  end;

  TOnItemChangedCallback = procedure (const AItemIndex: Integer) of object;

  { TListChangedListener }

  TListChangedListener = class (TJavaLocal, JOnItemChangedListener)
  strict private
    FChangedCallback: TOnItemChangedCallback;
    FOpenCallback: TOnCallback;
    FCloseCallback: TOnCallback;
    FItemIndex: Integer;
  protected
    procedure DoItemChanged;
    procedure DoShow;
    procedure DoHide;
  public
    constructor Create(AChangeCallback: TOnItemChangedCallback; AOpenCallback: TOnCallback; ACloseCallback: TOnCallback);
    { JOnItemChangedListener }
    procedure onItemChanged(itemIndex: Integer); cdecl;
    procedure onShow; cdecl;
    procedure onHide; cdecl;
  end;

{ Picker Factory Service }

  TAndroidPickerService = class (TPickerFactoryService)
  protected
    function DoCreateDateTimePicker: TCustomDateTimePicker; override;
    function DoCreateListPicker: TCustomListPicker; override;
  end;

var
  PickerService: TPickerFactoryService;

procedure RegisterPickersService;
begin
  PickerService := TAndroidPickerService.Create;
  TPlatformServices.Current.AddPlatformService(IFMXPickerService, PickerService);
end;

procedure UnregisterPickersService;
begin
  TPlatformServices.Current.RemovePlatformService(IFMXPickerService);
  PickerService := nil;
end;

function GetPickersFactory: JBasePickersFactory;
begin
  Result := TJBasePickersFactory.JavaClass.getFactory;
end;

{  GetNativeTheme(const AControl: TControl)

   We define which native theme we need to use for pickers
   using meta information in our FMX style (TStyleDescriptor.PlatformTarget)

   Theme is applyed only on Android Version >= 4.0
   If current OS it's Gengebread, we use 0 - default system theme }

function GetNativeTheme(const AControl: TControl): Integer;
var
  StyleDescriptor: TStyleDescription;
begin
  Result := 0;
  if not IsGingerbreadDevice then
  begin
    StyleDescriptor := TStyleManager.GetStyleDescriptionForControl(AControl);
    if Assigned(StyleDescriptor) then
    begin
      if StyleDescriptor.PlatformTarget.Contains(ANDROID_LIGHT_THEME) then
        Result := TJAlertDialog.JavaClass.THEME_HOLO_LIGHT;

      if StyleDescriptor.PlatformTarget.Contains(ANDROID_DARK_THEME) then
        Result := TJAlertDialog.JavaClass.THEME_HOLO_DARK;
    end;
  end;
end;

{ TAndroidDateTimePicker }

constructor TAndroidDateTimePicker.Create(const APickerService: TPickerFactoryService);
var
  NativePickersFactory: JBasePickersFactory;
begin
  inherited Create(APickerService);
  NativePickersFactory := GetPickersFactory;
  if Assigned(NativePickersFactory) then
  begin
    CallInUIThreadAndWaitFinishing(procedure()
      begin
        FNativeDatePicker := NativePickersFactory.createDatePicker(MainActivity);
        FNativeTimePicker := NativePickersFactory.createTimePicker(MainActivity);
      end);
  end;
  FDateTimeChangedListener := TDateTimeChangedListener.Create(DoDateChanged, DoShow, DoHide);
  FNativeDatePicker.setListener(FDateTimeChangedListener);
  FNativeTimePicker.setListener(FDateTimeChangedListener);
end;

destructor TAndroidDateTimePicker.Destroy;
begin
  FDateTimeChangedListener.DisposeOf;
  FNativeDatePicker := nil;
  FNativeTimePicker := nil;
  inherited Destroy;
end;

procedure TAndroidDateTimePicker.DoDateChanged(const ADateTime: TDateTime);
var
  DateTimeTmp: TDateTime;
begin
  // We need truncate Date or time part
  case ShowMode of
    psmDate: DateTimeTmp := DateOf(ADateTime);
    psmTime: DateTimeTmp := TimeOf(ADateTime);
  else
    DateTimeTmp := ADateTime;
  end;

  inherited DoDateChanged(DateTimeTmp);
end;

procedure TAndroidDateTimePicker.Hide;
begin
  if FNativeDatePicker.isShown then
    FNativeDatePicker.hide;
  if FNativeTimePicker.isShown then
    FNativeTimePicker.hide;
end;

function TAndroidDateTimePicker.IsShown: Boolean;
begin
  case ShowMode of
    psmDate: Result := FNativeDatePicker.isShown;
    psmTime: Result := FNativeTimePicker.isShown;
  else
    Result := False;
  end;
end;

procedure TAndroidDateTimePicker.SetShowMode(const AValue: TDatePickerShowMode);
begin
  if AValue = TDatePickerShowMode.psmDateTime then
    raise EFeatureError.Create(SDateTimePickerShowModeNotSupported)
  else
    inherited SetShowMode(AValue);
end;

procedure TAndroidDateTimePicker.Show;
begin
  inherited Show;
  case ShowMode of
    psmDate:
    begin
      CallInUIThread(procedure()
        begin
          FNativeDatePicker.setTheme(GetNativeTheme(Parent));
          FNativeDatePicker.setDate(DateTimeToUnix(Self.Date) * 1000);
          FNativeDatePicker.show;
        end);
    end;
    psmTime:
    begin
      CallInUIThread(procedure()
        begin
          FNativeTimePicker.setTheme(GetNativeTheme(Parent));
          FNativeTimePicker.setDate(DateTimeToUnix(Self.Date) * 1000);
          FNativeTimePicker.show;
        end);
    end
  else
    raise EFeatureError.Create(SDateTimePickerShowModeNotSupported);
  end;
end;

{ TDateTimeChangedListener }

constructor TDateTimeChangedListener.Create(AChangeCallback: TOnDateTimeChangedCallback;
  AOpenCallback: TOnCallback; CallInUIThreadWithWaitFinishing: TOnCallback);
begin
  inherited Create;
  FChangedCallback := AChangeCallback;
  FOpenCallback := AOpenCallback;
  FCloseCallback := CallInUIThreadWithWaitFinishing;
end;

procedure TDateTimeChangedListener.onHide;
begin
  // We got to this method in Java thread. So We need synchronize event handler in
  // another thread, Because ShowMessage and other thread dependend methods do
  // dead lock for application
  if FDateTime <> 0 then
    TThread.Queue(nil, DoDateChanged);
  TThread.Queue(nil, DoHide);
end;

procedure TDateTimeChangedListener.onShow;
begin
  FDateTime := 0;
  // ! See comments for TDateTimeChangedListener.onDateChanged
  TThread.Queue(nil, DoShow) ;
end;

procedure TDateTimeChangedListener.DoDateChanged;
begin
  if Assigned(FChangedCallback) then
    FChangedCallback(FDateTime);
end;

procedure TDateTimeChangedListener.DoHide;
begin
  if Assigned(FCloseCallback) then
    FCloseCallback;
end;

procedure TDateTimeChangedListener.DoShow;
begin
  if Assigned(FOpenCallback) then
    FOpenCallback;
end;

procedure TDateTimeChangedListener.onDateChanged(date: JDate);
begin
  FDateTime := UnixToDateTime(Trunc(date.getTime / 1000));
end;

{ TAndroidListPicker }

constructor TAndroidListPicker.Create(const APickerService: TPickerFactoryService);
var
  NativePickersFactory: JBasePickersFactory;
begin
  inherited Create(APickerService);
  NativePickersFactory := GetPickersFactory;
  if Assigned(NativePickersFactory) then
  begin
    CallInUIThreadAndWaitFinishing(procedure()
      begin
        FNativeListPicker := NativePickersFactory.createListPicker(MainActivity);
        FItemChangedListener := TListChangedListener.Create(DoItemChanged, DoShow, DoHide);
        FNativeListPicker.setListener(FItemChangedListener);
      end);
  end;
end;

destructor TAndroidListPicker.Destroy;
begin
  FNativeListPicker := nil;
  FItemChangedListener.DisposeOf;
  inherited Destroy;
end;

procedure TAndroidListPicker.Hide;
begin
  if FNativeListPicker.isShown then
    FNativeListPicker.hide;
end;

function TAndroidListPicker.IsShown: Boolean;
begin
  Result := FNativeListPicker.isShown;
end;

procedure TAndroidListPicker.SetValues(AValues: TStrings);
begin
  inherited SetValues(AValues);
  SetValuesToNativePicker(AValues);
end;

procedure TAndroidListPicker.SetValuesToNativePicker(AValues: TStrings);
var
  I: Integer;
  NativeArray: TJavaObjectArray<JCharSequence>;
begin
  NativeArray := TJavaObjectArray<JCharSequence>.Create(AValues.Count);
  for I := 0 to AValues.Count - 1 do
    NativeArray.SetRawItem(I, (StrToJCharSequence(AValues[I]) as ILocalObject).GetObjectID);

  CallInUIThreadAndWaitFinishing(procedure()
    begin
      FNativeListPicker.setItems(NativeArray);
    end);
end;

procedure TAndroidListPicker.Show;
begin
  inherited Show;
  if Values.Count > 0 then
  begin
    SetValuesToNativePicker(Values);
    CallInUIThreadAndWaitFinishing(procedure()
      begin
        FNativeListPicker.setTheme(GetNativeTheme(Parent));
        FNativeListPicker.setItemIndex(ItemIndex);
        FNativeListPicker.show;
      end);
  end;
end;

{ TListChangedListener }

constructor TListChangedListener.Create(AChangeCallback: TOnItemChangedCallback; AOpenCallback,
  ACloseCallback: TOnCallback);
begin
  inherited Create;
  FChangedCallback := AChangeCallback;
  FOpenCallback := AOpenCallback;
  FCloseCallback := ACloseCallback;
end;

procedure TListChangedListener.DoItemChanged;
begin
  if Assigned(FChangedCallback) then
    FChangedCallback(FItemIndex);
end;

procedure TListChangedListener.DoShow;
begin
  if Assigned(FOpenCallback) then
    FOpenCallback;
end;

procedure TListChangedListener.DoHide;
begin
  if Assigned(FCloseCallback) then
    FCloseCallback;
end;

procedure TListChangedListener.onItemChanged(itemIndex: Integer);
begin
  FItemIndex := itemIndex;
end;

procedure TListChangedListener.onHide;
begin
  // We got to this method in Java thread. So We need synchronize event handler in
  // another thread, Because ShowMessage and other thread dependend methods do
  // dead lock for application
  if FItemIndex <> -1 then
    TThread.Queue(nil, DoItemChanged);
  TThread.Queue(nil, DoHide);
end;

procedure TListChangedListener.onShow;
begin
  // ! See comments for TListChangedListener.onHide
  FItemIndex := -1;
  TThread.Queue(nil, DoShow);
end;

{ TAndroidPickerService }

function TAndroidPickerService.DoCreateDateTimePicker: TCustomDateTimePicker;
begin
  Result := TAndroidDateTimePicker.Create(Self);
end;

function TAndroidPickerService.DoCreateListPicker: TCustomListPicker;
begin
  Result := TAndroidListPicker.Create(Self);
end;

end.