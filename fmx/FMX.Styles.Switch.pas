{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit FMX.Styles.Switch;

interface

uses
  System.Classes, System.Types, System.Rtti, System.UITypes, System.UIConsts, System.Math,
  System.SysUtils, System.StrUtils, FMX.Types, FMX.Controls, FMX.Objects, FMX.Graphics,
  FMX.Ani, FMX.StdCtrls, FMX.Styles.Objects, FMX.Platform;

{$SCOPEDENUMS ON}

type

  TSwitchTextKind = (stkNone, stkIO, stkIOandOnOff, stkLocalized, stkBitmap);

  TSwitchTextObject = class(TStyleTextObject)
  private
    FKind: TSwitchTextKind;
    FColorOn: TAlphaColor;
    FValue: Boolean;
    FCurrentLang: string;
    function GetTextForKind(const Kind: TSwitchTextKind; const Value: Boolean): string;
  protected
    function GetCurrentColor: TAlphaColor; override;
    function GetCurrentText: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: Boolean read FValue write FValue;
  published
    property ColorOn: TAlphaColor read FColorOn write FColorOn default claNull;
    property Kind: TSwitchTextKind read FKind write FKind;
  end;

  TSwitchKind = (skClippingTracks, skFadingTracks);

  TCustomSwitchObject = class(TControl, ISwitchObject)
  public const
    SwitchAnimationDuration = 0.2;
    TrackingSensitivity = 3;
    TextClipOffset = 4;
  private
    [Weak] FControl: TControl;
    FThumbRect: TBounds;
    FPressed, FTracking, FAnimating: Boolean;
    FPressedThumbPos, FSavedPos: TPointF;
    FClickAnimation: TRectAnimation;
    FValue: Boolean;
    FOnChange: TNotifyEvent;
    FThumbLength: Single;
    FTrackingThumbScale: Single;
    FThumbPadding: TBounds;
    FTrackText: TSwitchTextObject;
    FThumbText: TSwitchTextObject;
    FKind: TSwitchKind;
    procedure DoAnimationEnd(Sender: TObject);
    procedure SetThumbRect(const Value: TBounds);
    procedure DoThumbRectChanged(Sender: TObject);
    procedure SetThumbLength(const Value: Single);
    procedure SetThumbPadding(const Value: TBounds);
    procedure SetTrackingThumbScale(const Value: Single);
    procedure PaintThumb;
    procedure PaintThumbText;
    procedure PaintTrack;
    procedure PaintTrackText;
    function IsTrackTextStored: Boolean;
    function IsThumbTextStored: Boolean;
    procedure PaintClippingTracks;
    procedure PaintFadingTracks;
  protected
    function GetObservers: TObservers; override;
    procedure AnimateTo(const Value: Boolean);
    function GetThumbValue: Single;
    function GetThumbCenter: Single;
    function GetThumbLength: Single;
    function GetThumbRect: TRectF;
    function GetValueByMousePos(const X, Y: Single): Boolean;
    function GetThumbRectByValue(const Value: Boolean): TRectF; virtual;
    procedure DoChange;
    function IsThumbTextVisible: Boolean; virtual;
    function IsTrackTextVisible: Boolean; virtual;
    procedure PaintThumbForValue(const R: TRectF; const Value: Boolean); virtual;
    procedure PaintThumbTextForValue(const R: TRectF; const Value: Boolean); virtual;
    procedure PaintTrackForValue(const R: TRectF; const Value: Boolean); virtual;
    procedure PaintTrackTextForValue(const R: TRectF; const Value: Boolean); virtual;
  protected
    procedure Paint; override;
    { IPresentationObject }
    procedure DoExit; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure SetControl(const AControl: TControl);
    property Control: TControl read FControl;
    property Pressed: Boolean read FPressed;
    property Tracking: Boolean read FTracking;
    property Animating: Boolean read FAnimating;
    { ISwitchObject }
    function GetValue: Boolean;
    procedure SetValue(const Value: Boolean);
    procedure SetOnChange(const AOnChange: TNotifyEvent);
    property Value: Boolean read FValue write SetValue;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ThumbRect: TBounds read FThumbRect write SetThumbRect;
  published
    property Action;
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
    property Kind: TSwitchKind read FKind write FKind default TSwitchKind.skClippingTracks;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property TabOrder;
    property TouchTargetExpansion;
    property TrackText: TSwitchTextObject read FTrackText stored IsTrackTextStored;
    property TrackingThumbScale: Single read FTrackingThumbScale write SetTrackingThumbScale;
    property ThumbLength: Single read FThumbLength write SetThumbLength;
    property ThumbPadding: TBounds read FThumbPadding write SetThumbPadding;
    property ThumbText: TSwitchTextObject read FThumbText stored IsThumbTextStored;
    property Visible default True;
    property Width;
    property OnApplyStyleLookup;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnKeyDown;
    property OnKeyUp;
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

  TSwitchShape = (ssRectangle, ssRoundRect, ssOval);

  TSwitchObject = class(TCustomSwitchObject)
  private
    FFill, FFillOn: TBrush;
    FStroke: TStrokeBrush;
    FShape: TSwitchShape;
    FThumb, FThumbOn: TBrush;
    FThumbStroke: TStrokeBrush;
    procedure SetFill(const Value: TBrush);
    procedure SetFillOn(const Value: TBrush);
    procedure SetStroke(const Value: TStrokeBrush);
    procedure SetShape(const Value: TSwitchShape);
    procedure SetThumb(const Value: TBrush);
    procedure SetThumbOn(const Value: TBrush);
    procedure SetThumbStroke(const Value: TStrokeBrush);
  protected
    procedure PaintThumbForValue(const R: TRectF; const Value: Boolean); override;
    procedure PaintTrackForValue(const R: TRectF; const Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Fill: TBrush read FFill write SetFill;
    property FillOn: TBrush read FFillOn write SetFillOn;
    property Stroke: TStrokeBrush read FStroke write SetStroke;
    property Thumb: TBrush read FThumb write SetThumb;
    property ThumbOn: TBrush read FThumbOn write SetThumbOn;
    property ThumbStroke: TStrokeBrush read FThumbStroke write SetThumbStroke;
    property Shape: TSwitchShape read FShape write SetShape default TSwitchShape.ssRectangle;
  end;

  TBitmapSwitchObject = class(TCustomSwitchObject)
  public type
    TSwitchPart = (Track, TrackZoom, Thumb, ThumbPressed, TextBitmap, TextBitmapEn, TextBitmapJp);
    TSwitchRec = record
      OnObject: TStyleObject;
      OffObject: TStyleObject;
    end;
  private
    FSourceLookup: string;
    FStyleObjects: array [TSwitchPart] of TSwitchRec;
    FLocaleService: IFMXLocaleService;
    function GetOnObject(Index: TSwitchPart): TBitmapLinks;
    procedure SetOnObject(Index: TSwitchPart; const Value: TBitmapLinks);
    function GetOffObject(Index: TSwitchPart): TBitmapLinks;
    procedure SetOffObject(Index: TSwitchPart; const Value: TBitmapLinks);
    function GetSourceLookup: string;
    procedure SetSourceLookup(const Value: string);
    procedure SetWrapMode(const Index: TSwitchPart; const Value: TImageWrapMode);
    function GetCapMode(const Index: TSwitchPart): TCapWrapMode;
    function GetWrapMode(const Index: TSwitchPart): TImageWrapMode;
    procedure SetCapMode(const Index: TSwitchPart; const Value: TCapWrapMode);
    function FindOrCreateBitmapLink(const StyleObject: TStyleObject; const Scale: Single): TBitmapLink;
    procedure ReadTrackMargins(Reader: TReader);
    procedure ReadThumbRect(Reader: TReader);
    procedure ReadTrackRect(Reader: TReader);
    procedure ReadThumbOnRect(Reader: TReader);
    procedure ReadTrackOnRect(Reader: TReader);
    procedure ReadTextBitmapRect(Reader: TReader);
    procedure ReadTextBitmapOnRect(Reader: TReader);
    procedure ReadTextBitmapEnOnRect(Reader: TReader);
    procedure ReadTextBitmapEnRect(Reader: TReader);
    procedure ReadTextBitmapJpOnRect(Reader: TReader);
    procedure ReadTextBitmapJpRect(Reader: TReader);
    procedure ReadThumbMargins(Reader: TReader);
    procedure CopyCaps(const Index: TSwitchPart); overload;
    procedure CopyCaps(const Source, Dest: TSwitchPart); overload;
    procedure ReadTextBitmapEnOnRect15x(Reader: TReader);
    procedure ReadTextBitmapEnRect15x(Reader: TReader);
    procedure ReadTextBitmapJpOnRect15x(Reader: TReader);
    procedure ReadTextBitmapJpRect15x(Reader: TReader);
    procedure ReadTextBitmapOnRect15x(Reader: TReader);
    procedure ReadTextBitmapRect15x(Reader: TReader);
    procedure ReadThumbMargins15x(Reader: TReader);
    procedure ReadThumbOnRect15x(Reader: TReader);
    procedure ReadThumbRect15x(Reader: TReader);
    procedure ReadTrackMargins15x(Reader: TReader);
    procedure ReadTrackOnRect15x(Reader: TReader);
    procedure ReadTrackRect15x(Reader: TReader);
    procedure ReadTextBitmapEnOnRect20x(Reader: TReader);
    procedure ReadTextBitmapEnOnRect30x(Reader: TReader);
    procedure ReadTextBitmapEnRect20x(Reader: TReader);
    procedure ReadTextBitmapEnRect30x(Reader: TReader);
    procedure ReadTextBitmapJpOnRect20x(Reader: TReader);
    procedure ReadTextBitmapJpOnRect30x(Reader: TReader);
    procedure ReadTextBitmapJpRect20x(Reader: TReader);
    procedure ReadTextBitmapJpRect30x(Reader: TReader);
    procedure ReadTextBitmapOnRect20x(Reader: TReader);
    procedure ReadTextBitmapOnRect30x(Reader: TReader);
    procedure ReadTextBitmapRect20x(Reader: TReader);
    procedure ReadTextBitmapRect30x(Reader: TReader);
    procedure ReadThumbMargins20x(Reader: TReader);
    procedure ReadThumbMargins30x(Reader: TReader);
    procedure ReadThumbOnRect20x(Reader: TReader);
    procedure ReadThumbOnRect30x(Reader: TReader);
    procedure ReadThumbRect20x(Reader: TReader);
    procedure ReadThumbRect30x(Reader: TReader);
    procedure ReadTrackMargins20x(Reader: TReader);
    procedure ReadTrackMargins30x(Reader: TReader);
    procedure ReadTrackOnRect20x(Reader: TReader);
    procedure ReadTrackOnRect30x(Reader: TReader);
    procedure ReadTrackRect20x(Reader: TReader);
    procedure ReadTrackRect30x(Reader: TReader);
    procedure ReadTrackZoomRect(Reader: TReader);
    procedure ReadTrackZoomRect15x(Reader: TReader);
    procedure ReadTrackZoomRect20x(Reader: TReader);
    procedure ReadTrackZoomRect30x(Reader: TReader);
    procedure ReadThumbPressedRect(Reader: TReader);
    procedure ReadThumbPressedRect15x(Reader: TReader);
    procedure ReadThumbPressedRect20x(Reader: TReader);
    procedure ReadThumbPressedRect30x(Reader: TReader);
  protected
    function IsThumbTextVisible: Boolean; override;
    function IsTrackTextVisible: Boolean; override;
    procedure PaintThumbForValue(const R: TRectF; const Value: Boolean); override;
    procedure PaintThumbTextForValue(const R: TRectF; const Value: Boolean); override;
    procedure PaintTrackForValue(const R: TRectF; const Value: Boolean); override;
    procedure PaintTrackTextForValue(const R: TRectF; const Value: Boolean); override;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetNewScene(AScene: IScene); override;
  published
    property Track: TBitmapLinks index TSwitchPart.Track read GetOffObject write SetOffObject;
    property TrackZoom: TBitmapLinks index TSwitchPart.TrackZoom read GetOffObject write SetOffObject;
    property TrackOn: TBitmapLinks index TSwitchPart.Track read GetOnObject write SetOnObject;
    property TrackCapMode: TCapWrapMode index TSwitchPart.Track read GetCapMode write SetCapMode default TCapWrapMode.cmStretch;
    property TrackWrapMode: TImageWrapMode index TSwitchPart.Track read GetWrapMode write SetWrapMode default TImageWrapMode.iwStretch;
    property Thumb: TBitmapLinks index TSwitchPart.Thumb read GetOffObject write SetOffObject;
    property ThumbOn: TBitmapLinks index TSwitchPart.Thumb read GetOnObject write SetOnObject;
    property ThumbPressed: TBitmapLinks index TSwitchPart.ThumbPressed read GetOffObject write SetOffObject;
    property ThumbCapMode: TCapWrapMode index TSwitchPart.Thumb read GetCapMode write SetCapMode default TCapWrapMode.cmStretch;
    property ThumbWrapMode: TImageWrapMode index TSwitchPart.Thumb read GetWrapMode write SetWrapMode default TImageWrapMode.iwStretch;
    property TextBitmap: TBitmapLinks index TSwitchPart.TextBitmap read GetOffObject write SetOffObject;
    property TextBitmapOn: TBitmapLinks index TSwitchPart.TextBitmap read GetOnObject write SetOnObject;
    property TextBitmapEn: TBitmapLinks index TSwitchPart.TextBitmapEn read GetOffObject write SetOffObject;
    property TextBitmapEnOn: TBitmapLinks index TSwitchPart.TextBitmapEn read GetOnObject write SetOnObject;
    property TextBitmapJp: TBitmapLinks index TSwitchPart.TextBitmapJP read GetOffObject write SetOffObject;
    property TextBitmapJpOn: TBitmapLinks index TSwitchPart.TextBitmapJP read GetOnObject write SetOnObject;
    property SourceLookup: string read GetSourceLookup write SetSourceLookup;
  end;

implementation

uses FMX.Consts;

{ TSwitchTextObject }

constructor TSwitchTextObject.Create(AOwner: TComponent);
var
  Loc: IFMXLocaleService;
begin
  inherited;
  ColorOn := claNull;
  if TPlatformServices.Current.SupportsPlatformService(IFMXLocaleService, IInterface(Loc)) then
    FCurrentLang := Loc.GetCurrentLangID
  else
    FCurrentLang := '';
end;

function TSwitchTextObject.GetCurrentColor: TAlphaColor;
begin
  if Value and (ColorOn <> claNull) then
    Result := ColorOn
  else
    Result := Color;
end;

function TSwitchTextObject.GetTextForKind(const Kind: TSwitchTextKind;
  const Value: Boolean): string;
begin
  case Kind of
    TSwitchTextKind.stkIO:
      begin
        if Value then
          Result := 'I'
        else
          Result := 'O';
      end;
    TSwitchTextKind.stkIOandOnOff:
      begin
        if FCurrentLang = 'en' then
        begin
          if Value then
            Result := 'ON' // Do not localize
          else
            Result := 'OFF';  // Do not localize
        end else
        begin
          if Value then
            Result := 'I'
          else
            Result := 'O';
        end;
      end;
    TSwitchTextKind.stkLocalized:
      begin
        if Value then
          Result := UpperCase(SOnText)
        else
          Result := UpperCase(SOffText);
      end;
  else
    Result := '';
  end;
end;

function TSwitchTextObject.GetCurrentText: string;
begin
  Result := GetTextForKind(Kind, Value);
end;

{ TCustomSwitchObject }

constructor TCustomSwitchObject.Create(AOwner: TComponent);
begin
  inherited;
  FKind := TSwitchKind.skClippingTracks;
  FTrackingThumbScale := 1.0;
  FThumbRect := TBounds.Create(NullRect);
  FThumbRect.OnChange := DoThumbRectChanged;
  FThumbPadding := TBounds.Create(NullRect);
  FTrackText := TSwitchTextObject.Create(nil);
  FTrackText.SetSubComponent(True);
  FThumbText := TSwitchTextObject.Create(nil);
  FThumbText.SetSubComponent(True);
  HitTest := False;
end;

destructor TCustomSwitchObject.Destroy;
begin
  FreeAndNil(FThumbText);
  FreeAndNil(FTrackText);
  FreeAndNil(FThumbPadding);
  FreeAndNil(FThumbRect);
  inherited;
end;

procedure TCustomSwitchObject.DoAnimationEnd(Sender: TObject);
begin
  FreeAndNil(FClickAnimation);
  FAnimating := False;
  Repaint;
end;

procedure TCustomSwitchObject.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
  Repaint;
end;

procedure TCustomSwitchObject.DoExit;
begin
  inherited;
  if Observers.IsObserving(TObserverMapping.EditLinkID) then
    TLinkObservers.EditLinkUpdate(Observers);
  if Observers.IsObserving(TObserverMapping.ControlValueID) then
    TLinkObservers.ControlValueUpdate(Observers);
end;

procedure TCustomSwitchObject.DoThumbRectChanged(Sender: TObject);
begin
  Repaint;
end;

procedure TCustomSwitchObject.AnimateTo(const Value: Boolean);
var
  ElapsedDistance, AnimationDistance: Single;
begin
  FreeAndNil(FClickAnimation);
  if ([csLoading, csReading, csDestroying, csDesigning] * ComponentState <> []) or
     (not Assigned(Parent)) or
     (FDisablePaint) or
     (FUpdating > 0) or
     (not Visible) or
     (not ParentedVisible) then
  begin
    Repaint;
  end
  else
  begin
    FAnimating := True;
    FClickAnimation := TRectAnimation.Create(Self);
    FClickAnimation.Parent := Self;
    FClickAnimation.AnimationType := TAnimationType.atIn;
    FClickAnimation.Interpolation := TInterpolationType.itLinear;
    FClickAnimation.PropertyName := 'ThumbRect';
    if FTracking then
      FClickAnimation.StartValue.Rect := ThumbRect.Rect
    else
      FClickAnimation.StartValue.Rect := GetThumbRectByValue(not Value);
    FClickAnimation.StopValue.Rect := GetThumbRectByValue(Value);

    AnimationDistance := Width - FClickAnimation.StopValue.Rect.Width;
    ElapsedDistance := Abs(FClickAnimation.StopValue.Rect.Left - FClickAnimation.StartValue.Rect.Left);

    if AnimationDistance > 0 then
      FClickAnimation.Duration := SwitchAnimationDuration * (ElapsedDistance / AnimationDistance)
    else
      FClickAnimation.Duration := SwitchAnimationDuration;

    FClickAnimation.OnFinish := DoAnimationEnd;
    FClickAnimation.Start;
  end;
end;

function TCustomSwitchObject.GetObservers: TObservers;
begin
  if Assigned(Control) then
    Result := Control.Observers
  else
    Result := inherited;
end;

function TCustomSwitchObject.GetThumbCenter: Single;
var
  R: TRectF;
begin
  R := GetThumbRect;
  Result := (R.Left + R.Right) / 2;
end;

function TCustomSwitchObject.GetThumbLength: Single;
var
  R: TRectF;
begin
  if SameValue(FThumbLength, 0.0, Epsilon) then
  begin
    R := FThumbPadding.PaddingRect(LocalRect);
    Result := R.Height;
  end else
    Result := FThumbLength;

  if (FPressed or FTracking or FAnimating) and not SameValue(TrackingThumbScale, 1) then
    Result := Round(Result * TrackingThumbScale);
end;

function TCustomSwitchObject.GetThumbRect: TRectF;
begin
  Result := FThumbPadding.PaddingRect(LocalRect);
  if FTracking or FAnimating then
    Result := TRectF.Create(FThumbRect.Rect.Round)
  else
    Result := GetThumbRectByValue(Value);
end;

function TCustomSwitchObject.GetThumbRectByValue(const Value: Boolean): TRectF;
begin
  Result := FThumbPadding.PaddingRect(LocalRect);
  if not Value then
    Result.Right := Result.Left + GetThumbLength
  else
    Result.Left := Result.Right - GetThumbLength;
end;

function TCustomSwitchObject.GetThumbValue: Single;
var
  R: TRectF;
begin
  if FTracking or FAnimating then
  begin
    R := GetThumbRect;
    Result := R.Left / (Width - R.Width);
  end else
  if Value then
    Result := 1.0
  else
    Result := 0.0;
end;

function TCustomSwitchObject.GetValue: Boolean;
begin
  Result := FValue;
end;

function TCustomSwitchObject.GetValueByMousePos(const X, Y: Single): Boolean;
var
  HalfThumbWidth: Single;
begin
  if not ThumbRect.Empty then
    HalfThumbWidth := ThumbRect.Width / 2
  else
    HalfThumbWidth := 0;

  if (X - FPressedThumbPos.X) + HalfThumbWidth < Width / 2 then
    Result := False
  else
    Result := True;
end;

function TCustomSwitchObject.IsThumbTextStored: Boolean;
begin
  Result := ThumbText.Kind <> TSwitchTextKind.stkNone;
end;

function TCustomSwitchObject.IsThumbTextVisible: Boolean;
begin
  Result := ThumbText.Kind <> TSwitchTextKind.stkNone;
end;

function TCustomSwitchObject.IsTrackTextStored: Boolean;
begin
  Result := TrackText.Kind <> TSwitchTextKind.stkNone;
end;

function TCustomSwitchObject.IsTrackTextVisible: Boolean;
begin
  Result := TrackText.Kind <> TSwitchTextKind.stkNone;
end;

procedure TCustomSwitchObject.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;
  if (Button = TMouseButton.mbLeft) then
  begin
    if Observers.IsObserving(TObserverMapping.EditLinkID) then
      if TLinkObservers.EditLinkIsReadOnly(Observers) then
        Exit;
    FPressed := True;
    FThumbRect.Rect := GetThumbRectByValue(Value);
    FSavedPos := TPointF.Create(X, Y);
    FPressedThumbPos := FSavedPos - FThumbRect.Rect.TopLeft;
    Repaint;
  end;
end;

procedure TCustomSwitchObject.MouseMove(Shift: TShiftState; X, Y: Single);
var
  NewThumbRect: TRectF;
begin
  inherited;
  if FPressed then
  begin
    if (ssTouch in Shift) and (Abs(X - FSavedPos.X) < TrackingSensitivity) then Exit;

    FTracking := True;

    NewThumbRect := FThumbRect.Rect;
    NewThumbRect.Offset(X - FSavedPos.X, 0);

    if NewThumbRect.Left < 0 then
      NewThumbRect.Offset(-NewThumbRect.Left, 0);
    if NewThumbRect.Right > Width then
      NewThumbRect.Offset(-(NewThumbRect.Right - Width), 0);

    FThumbRect.Rect := NewThumbRect;
    FSavedPos := TPointF.Create(X, Y);
  end;
end;

procedure TCustomSwitchObject.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  LAllowChange: Boolean;
begin
  inherited;
  if FPressed then
  begin
    FPressed := False;

    LAllowChange := True;

    if Observers.IsObserving(TObserverMapping.EditLinkID) then
    begin
      if TLinkObservers.EditLinkEdit(Observers) then
        TLinkObservers.EditLinkModified(Observers)
      else
      begin
        TLinkObservers.EditLinkReset(Observers);
        LAllowChange := False;
      end;
    end;

    if LAllowChange then
    begin
      if not FTracking then
      begin
        SetValue(not Value);
        AnimateTo(Value);
      end else
      begin
        SetValue(GetValueByMousePos(X, Y));
        AnimateTo(Value);
      end;
    end
    else
      AnimateTo(Value);
    FTracking := False;
    Repaint;


    if Observers.IsObserving(TObserverMapping.EditLinkID) then
      TLinkObservers.EditLinkTrackUpdate(Observers);
    if Observers.IsObserving(TObserverMapping.ControlValueID) then
    begin
      TLinkObservers.ControlValueModified(Observers);
      TLinkObservers.ControlValueTrackUpdate(Observers);
    end;

  end;
end;

procedure TCustomSwitchObject.Paint;
begin
  PaintTrack;
  PaintTrackText;
  PaintThumb;
  PaintThumbText;
end;

procedure TCustomSwitchObject.PaintClippingTracks;
var
  R: TRectF;
  State: TCanvasSaveState;
begin
  State := Canvas.SaveState;
  try
    R := LocalRect;
    R.Left := GetThumbCenter;
    Canvas.IntersectClipRect(R);

    R := LocalRect;
    PaintTrackForValue(R, False);
  finally
    Canvas.RestoreState(State);
  end;

  State := Canvas.SaveState;
  try
    R := LocalRect;
    R.Right := GetThumbCenter;
    Canvas.IntersectClipRect(R);

    R := LocalRect;
    PaintTrackForValue(R, True);
  finally
    Canvas.RestoreState(State);
  end;
end;

procedure TCustomSwitchObject.PaintFadingTracks;
var
  R: TRectF;
begin
  R := LocalRect;
  if GetThumbCenter < Width / 2 then
    PaintTrackForValue(R, False)
  else
    PaintTrackForValue(R, True);
end;

procedure TCustomSwitchObject.PaintTrack;
begin
  case Kind of
    TSwitchKind.skClippingTracks: PaintClippingTracks;
    TSwitchKind.skFadingTracks: PaintFadingTracks;
  end;
end;

procedure TCustomSwitchObject.PaintTrackText;
var
  R: TRectF;
  State: TCanvasSaveState;
begin
  if IsTrackTextVisible then
  begin
    State := Canvas.SaveState;
    try
      R := LocalRect;
      R.Inflate(-TextClipOffset, 0);
      Canvas.IntersectClipRect(R);

      R := LocalRect;
      R.Right := R.Right - GetThumbLength;

      R.Offset(Round(GetThumbRect.Right), 0);
      PaintTrackTextForValue(R, False);
      R.Offset(-Width, 0);
      PaintTrackTextForValue(R, True);
    finally
      Canvas.RestoreState(State);
    end;
  end;
end;

procedure TCustomSwitchObject.PaintThumb;
var
  R: TRectF;
begin
  R := GetThumbRect;
  PaintThumbForValue(R, Value);
end;

procedure TCustomSwitchObject.PaintThumbText;
var
  R: TRectF;
begin
  if IsThumbTextVisible then
  begin
    R := TRectF.Create(GetThumbRect.Round);
    if FTracking or FAnimating then
      PaintThumbTextForValue(R, GetThumbValue > 0.5)
    else
      PaintThumbTextForValue(R, Value);
  end;
end;

procedure TCustomSwitchObject.PaintThumbForValue(const R: TRectF;
  const Value: Boolean);
begin

end;

procedure TCustomSwitchObject.PaintThumbTextForValue(const R: TRectF;
  const Value: Boolean);
begin
  FThumbText.Value := Value;
  FThumbText.DrawToCanvas(Canvas, R, AbsoluteOpacity);
end;

procedure TCustomSwitchObject.PaintTrackForValue(const R: TRectF;
  const Value: Boolean);
begin

end;

procedure TCustomSwitchObject.PaintTrackTextForValue(const R: TRectF;
  const Value: Boolean);
begin
  FTrackText.Value := Value;
  FTrackText.DrawToCanvas(Canvas, R, AbsoluteOpacity);
end;

procedure TCustomSwitchObject.SetControl(const AControl: TControl);
begin
  FControl := AControl;
end;

procedure TCustomSwitchObject.SetOnChange(const AOnChange: TNotifyEvent);
begin
  FOnChange := AOnChange;
end;

procedure TCustomSwitchObject.SetThumbLength(const Value: Single);
begin
  FThumbLength := Value;
end;

procedure TCustomSwitchObject.SetThumbPadding(const Value: TBounds);
begin
  FThumbPadding.Assign(Value);
end;

procedure TCustomSwitchObject.SetThumbRect(const Value: TBounds);
begin
  FThumbRect.Assign(Value);
end;

procedure TCustomSwitchObject.SetTrackingThumbScale(const Value: Single);
begin
  FTrackingThumbScale := Value;
end;

procedure TCustomSwitchObject.SetValue(const Value: Boolean);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    DoChange;
  end;
end;

{ TSwitchObject }

constructor TSwitchObject.Create(AOwner: TComponent);
begin
  inherited;
  FFill := TBrush.Create(TBrushKind.bkSolid, claGray);
  FFillOn := TBrush.Create(TBrushKind.bkNone, claWhite);
  FStroke := TStrokeBrush.Create(TBrushKind.bkSolid, claGray);
  FThumb := TBrush.Create(TBrushKind.bkSolid, claGray);
  FThumbOn := TBrush.Create(TBrushKind.bkNone, claWhite);
  FThumbStroke := TStrokeBrush.Create(TBrushKind.bkSolid, claGray);
end;

destructor TSwitchObject.Destroy;
begin
  FreeAndNil(FThumbStroke);
  FreeAndNil(FThumbOn);
  FreeAndNil(FThumb);
  FreeAndNil(FFillOn);
  FreeAndNil(FFill);
  FreeAndNil(FStroke);
  inherited;
end;

procedure TSwitchObject.PaintTrackForValue(const R: TRectF; const Value: Boolean);
var
  Radius: Single;
  FillBrush: TBrush;
begin
  if FStroke.Kind <> TBrushKind.bkNone then
    R.Inflate(-(FStroke.Thickness / 2), -(FStroke.Thickness / 2));

  if Value and (FFillOn.Kind <> TBrushKind.bkNone) then
    FillBrush := FFillOn
  else
    FillBrush := FFill;

  case FShape of
    TSwitchShape.ssRectangle:
      begin
        Canvas.FillRect(R, 0, 0, AllCorners, AbsoluteOpacity, FillBrush);
        Canvas.DrawRect(R, 0, 0, AllCorners, AbsoluteOpacity, FStroke);
      end;
    TSwitchShape.ssRoundRect:
      begin
        Radius := 3;
        Canvas.FillRect(R, Radius, Radius, AllCorners, AbsoluteOpacity, FillBrush);
        Canvas.DrawRect(R, Radius, Radius, AllCorners, AbsoluteOpacity, FStroke);
      end;
    TSwitchShape.ssOval:
      begin
        Radius := R.Height / 2;
        Canvas.FillRect(R, Radius, Radius, AllCorners, AbsoluteOpacity, FillBrush);
        Canvas.DrawRect(R, Radius, Radius, AllCorners, AbsoluteOpacity, FStroke);
      end;
  end;
end;

procedure TSwitchObject.PaintThumbForValue(const R: TRectF;
  const Value: Boolean);
var
  Radius: Single;
  FillBrush: TBrush;
begin
  if FThumbStroke.Kind <> TBrushKind.bkNone then
    R.Inflate(-(FThumbStroke.Thickness / 2), -(FThumbStroke.Thickness / 2));

  if Value and (FThumbOn.Kind <> TBrushKind.bkNone) then
    FillBrush := FThumbOn
  else
    FillBrush := FThumb;

  case FShape of
    TSwitchShape.ssRectangle:
      begin
        Canvas.FillRect(R, 0, 0, AllCorners, AbsoluteOpacity, FillBrush);
        Canvas.DrawRect(R, 0, 0, AllCorners, AbsoluteOpacity, FThumbStroke);
      end;
    TSwitchShape.ssRoundRect:
      begin
        Radius := 3;
        Canvas.FillRect(R, Radius, Radius, AllCorners, AbsoluteOpacity, FillBrush);
        Canvas.DrawRect(R, Radius, Radius, AllCorners, AbsoluteOpacity, FThumbStroke);
      end;
    TSwitchShape.ssOval:
      begin
        Radius := R.Height / 2;
        Canvas.FillRect(R, Radius, Radius, AllCorners, AbsoluteOpacity, FillBrush);
        Canvas.DrawRect(R, Radius, Radius, AllCorners, AbsoluteOpacity, FThumbStroke);
      end;
  end;
end;

procedure TSwitchObject.SetFill(const Value: TBrush);
begin
  FFill.Assign(Value);
end;

procedure TSwitchObject.SetFillOn(const Value: TBrush);
begin
  FFillOn.Assign(Value);
end;

procedure TSwitchObject.SetStroke(const Value: TStrokeBrush);
begin
  FStroke.Assign(Value);
end;

procedure TSwitchObject.SetThumb(const Value: TBrush);
begin
  FThumb.Assign(Value);
end;

procedure TSwitchObject.SetThumbOn(const Value: TBrush);
begin
  FThumbOn.Assign(Value);
end;

procedure TSwitchObject.SetThumbStroke(const Value: TStrokeBrush);
begin
  FThumbStroke.Assign(Value);
end;

procedure TSwitchObject.SetShape(const Value: TSwitchShape);
begin
  if FShape <> Value then
  begin
    FShape := Value;
    Repaint;
  end;
end;

{ TBitmapSwitchObject }

constructor TBitmapSwitchObject.Create(AOwner: TComponent);
var
  I: TSwitchPart;
begin
  inherited;
  for I := Low(TSwitchPart) to High(TSwitchPart) do
  begin
    FStyleObjects[I].OnObject := TStyleObject.Create(nil);
    FStyleObjects[I].OffObject := TStyleObject.Create(nil);
  end;
  FStyleObjects[TSwitchPart.TextBitmap].OnObject.WrapMode := TImageWrapMode.iwCenter;
  FStyleObjects[TSwitchPart.TextBitmap].OffObject.WrapMode := TImageWrapMode.iwCenter;
  FStyleObjects[TSwitchPart.TextBitmapEn].OnObject.WrapMode := TImageWrapMode.iwCenter;
  FStyleObjects[TSwitchPart.TextBitmapEn].OffObject.WrapMode := TImageWrapMode.iwCenter;
  FStyleObjects[TSwitchPart.TextBitmapJp].OnObject.WrapMode := TImageWrapMode.iwCenter;
  FStyleObjects[TSwitchPart.TextBitmapJp].OffObject.WrapMode := TImageWrapMode.iwCenter;
  TPlatformServices.Current.SupportsPlatformService(IFMXLocaleService, IInterface(FLocaleService));
end;

destructor TBitmapSwitchObject.Destroy;
var
  Item: TSwitchRec;
  I: TSwitchPart;
begin
  for I := Low(TSwitchPart) to High(TSwitchPart) do
  begin
    Item := FStyleObjects[I];
    FStyleObjects[I].OnObject := nil;
    FStyleObjects[I].OffObject := nil;
    Item.OnObject.Free;
    Item.OffObject.Free;
  end;
  inherited;
end;

function TBitmapSwitchObject.FindOrCreateBitmapLink(const StyleObject: TStyleObject; const Scale: Single): TBitmapLink;
begin
  Result := StyleObject.SourceLink.LinkByScale(Scale);
  if not Assigned(Result) then
  begin
    Result := TBitmapLink.Create(StyleObject.SourceLink);
    Result.Scale := Scale;
  end;
end;

procedure TBitmapSwitchObject.DefineProperties(Filer: TFiler);
begin
  inherited;
  { Backward compatibility with XE3 }
  Filer.DefineProperty('TrackMargins.Rect', ReadTrackMargins, nil, False);
  Filer.DefineProperty('TrackRect.Rect', ReadTrackRect, nil, False);
  Filer.DefineProperty('TrackOnRect.Rect', ReadTrackOnRect, nil, False);
  Filer.DefineProperty('TrackZoomRect.Rect', ReadTrackZoomRect, nil, False);
  Filer.DefineProperty('ThumbMargins.Rect', ReadThumbMargins, nil, False);
  Filer.DefineProperty('ThumbRect.Rect', ReadThumbRect, nil, False);
  Filer.DefineProperty('ThumbOnRect.Rect', ReadThumbOnRect, nil, False);
  Filer.DefineProperty('ThumbPressedRect.Rect', ReadThumbPressedRect, nil, False);
  Filer.DefineProperty('TextBitmapRect.Rect', ReadTextBitmapRect, nil, False);
  Filer.DefineProperty('TextBitmapOnRect.Rect', ReadTextBitmapOnRect, nil, False);
  Filer.DefineProperty('TextBitmapEnRect.Rect', ReadTextBitmapEnRect, nil, False);
  Filer.DefineProperty('TextBitmapEnOnRect.Rect', ReadTextBitmapEnOnRect, nil, False);
  Filer.DefineProperty('TextBitmapJpRect.Rect', ReadTextBitmapJpRect, nil, False);
  Filer.DefineProperty('TextBitmapJpOnRect.Rect', ReadTextBitmapJpOnRect, nil, False);

  Filer.DefineProperty('TrackMargins15x.Rect', ReadTrackMargins15x, nil, False);
  Filer.DefineProperty('TrackRect15x.Rect', ReadTrackRect15x, nil, False);
  Filer.DefineProperty('TrackOnRect15x.Rect', ReadTrackOnRect15x, nil, False);
  Filer.DefineProperty('TrackZoomRect15x.Rect', ReadTrackZoomRect15x, nil, False);
  Filer.DefineProperty('ThumbMargins15x.Rect', ReadThumbMargins15x, nil, False);
  Filer.DefineProperty('ThumbRect15x.Rect', ReadThumbRect15x, nil, False);
  Filer.DefineProperty('ThumbOnRect15x.Rect', ReadThumbOnRect15x, nil, False);
  Filer.DefineProperty('ThumbPressedRect15x.Rect', ReadThumbPressedRect15x, nil, False);
  Filer.DefineProperty('TextBitmapRect15x.Rect', ReadTextBitmapRect15x, nil, False);
  Filer.DefineProperty('TextBitmapOnRect15x.Rect', ReadTextBitmapOnRect15x, nil, False);
  Filer.DefineProperty('TextBitmapEnRect15x.Rect', ReadTextBitmapEnRect15x, nil, False);
  Filer.DefineProperty('TextBitmapEnOnRect15x.Rect', ReadTextBitmapEnOnRect15x, nil, False);
  Filer.DefineProperty('TextBitmapJpRect15x.Rect', ReadTextBitmapJpRect15x, nil, False);
  Filer.DefineProperty('TextBitmapJpOnRect15x.Rect', ReadTextBitmapJpOnRect15x, nil, False);

  Filer.DefineProperty('TrackMargins20x.Rect', ReadTrackMargins20x, nil, False);
  Filer.DefineProperty('TrackRect20x.Rect', ReadTrackRect20x, nil, False);
  Filer.DefineProperty('TrackOnRect20x.Rect', ReadTrackOnRect20x, nil, False);
  Filer.DefineProperty('TrackZoomRect20x.Rect', ReadTrackZoomRect20x, nil, False);
  Filer.DefineProperty('ThumbMargins20x.Rect', ReadThumbMargins20x, nil, False);
  Filer.DefineProperty('ThumbRect20x.Rect', ReadThumbRect20x, nil, False);
  Filer.DefineProperty('ThumbOnRect20x.Rect', ReadThumbOnRect20x, nil, False);
  Filer.DefineProperty('ThumbPressedRect20x.Rect', ReadThumbPressedRect20x, nil, False);
  Filer.DefineProperty('TextBitmapRect20x.Rect', ReadTextBitmapRect20x, nil, False);
  Filer.DefineProperty('TextBitmapOnRect20x.Rect', ReadTextBitmapOnRect20x, nil, False);
  Filer.DefineProperty('TextBitmapEnRect20x.Rect', ReadTextBitmapEnRect20x, nil, False);
  Filer.DefineProperty('TextBitmapEnOnRect20x.Rect', ReadTextBitmapEnOnRect20x, nil, False);
  Filer.DefineProperty('TextBitmapJpRect20x.Rect', ReadTextBitmapJpRect20x, nil, False);
  Filer.DefineProperty('TextBitmapJpOnRect20x.Rect', ReadTextBitmapJpOnRect20x, nil, False);

  Filer.DefineProperty('TrackMargins30x.Rect', ReadTrackMargins30x, nil, False);
  Filer.DefineProperty('TrackRect30x.Rect', ReadTrackRect30x, nil, False);
  Filer.DefineProperty('TrackOnRect30x.Rect', ReadTrackOnRect30x, nil, False);
  Filer.DefineProperty('TrackZoomRect30x.Rect', ReadTrackZoomRect30x, nil, False);
  Filer.DefineProperty('ThumbMargins30x.Rect', ReadThumbMargins30x, nil, False);
  Filer.DefineProperty('ThumbRect30x.Rect', ReadThumbRect30x, nil, False);
  Filer.DefineProperty('ThumbOnRect30x.Rect', ReadThumbOnRect30x, nil, False);
  Filer.DefineProperty('ThumbPressedRect30x.Rect', ReadThumbPressedRect30x, nil, False);
  Filer.DefineProperty('TextBitmapRect30x.Rect', ReadTextBitmapRect30x, nil, False);
  Filer.DefineProperty('TextBitmapOnRect30x.Rect', ReadTextBitmapOnRect30x, nil, False);
  Filer.DefineProperty('TextBitmapEnRect30x.Rect', ReadTextBitmapEnRect30x, nil, False);
  Filer.DefineProperty('TextBitmapEnOnRect30x.Rect', ReadTextBitmapEnOnRect30x, nil, False);
  Filer.DefineProperty('TextBitmapJpRect30x.Rect', ReadTextBitmapJpRect30x, nil, False);
  Filer.DefineProperty('TextBitmapJpOnRect30x.Rect', ReadTextBitmapJpOnRect30x, nil, False);
end;

{$REGION 'Read properties for 1.0 scale'}

procedure TBitmapSwitchObject.ReadTrackMargins(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.Track].OffObject, 1);
  Link.CapInsets.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadTrackRect(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.Track].OffObject, 1);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadTrackOnRect(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.Track].OnObject, 1);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadTrackZoomRect(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.TrackZoom].OffObject, 1);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadThumbMargins(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.Thumb].OffObject, 1);
  Link.CapInsets.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadThumbRect(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.Thumb].OffObject, 1);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadThumbOnRect(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.Thumb].OnObject, 1);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadThumbPressedRect(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.ThumbPressed].OffObject, 1);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadTextBitmapRect(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.TextBitmap].OffObject, 1);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadTextBitmapOnRect(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.TextBitmap].OnObject, 1);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadTextBitmapEnRect(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.TextBitmapEn].OffObject, 1);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadTextBitmapEnOnRect(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.TextBitmapEn].OnObject, 1);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadTextBitmapJpRect(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.TextBitmapJp].OffObject, 1);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadTextBitmapJpOnRect(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.TextBitmapJp].OnObject, 1);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

{$ENDREGION}

{$REGION 'Read properties for 1.5 scale'}

procedure TBitmapSwitchObject.ReadTrackMargins15x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.Track].OffObject, 1.5);
  Link.CapInsets.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadTrackRect15x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.Track].OffObject, 1.5);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadTrackOnRect15x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.Track].OnObject, 1.5);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadTrackZoomRect15x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.TrackZoom].OffObject, 1.5);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadThumbMargins15x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.Thumb].OffObject, 1.5);
  Link.CapInsets.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadThumbRect15x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.Thumb].OffObject, 1.5);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadThumbOnRect15x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.Thumb].OnObject, 1.5);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadThumbPressedRect15x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.ThumbPressed].OffObject, 1.5);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadTextBitmapRect15x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.TextBitmap].OffObject, 1.5);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadTextBitmapOnRect15x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.TextBitmap].OnObject, 1.5);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadTextBitmapEnRect15x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.TextBitmapEn].OffObject, 1.5);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadTextBitmapEnOnRect15x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.TextBitmapEn].OnObject, 1.5);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadTextBitmapJpRect15x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.TextBitmapJp].OffObject, 1.5);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadTextBitmapJpOnRect15x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.TextBitmapJp].OnObject, 1.5);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

{$ENDREGION}

{$REGION 'Read properties for 2.0 scale'}

procedure TBitmapSwitchObject.ReadTrackMargins20x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.Track].OffObject, 2.0);
  Link.CapInsets.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadTrackRect20x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.Track].OffObject, 2.0);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadTrackOnRect20x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.Track].OnObject, 2.0);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadTrackZoomRect20x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.TrackZoom].OffObject, 2.0);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadThumbMargins20x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.Thumb].OffObject, 2.0);
  Link.CapInsets.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadThumbRect20x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.Thumb].OffObject, 2.0);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadThumbOnRect20x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.Thumb].OnObject, 2.0);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadThumbPressedRect20x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.ThumbPressed].OffObject, 2);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadTextBitmapRect20x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.TextBitmap].OffObject, 2.0);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadTextBitmapOnRect20x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.TextBitmap].OnObject, 2.0);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadTextBitmapEnRect20x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.TextBitmapEn].OffObject, 2.0);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadTextBitmapEnOnRect20x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.TextBitmapEn].OnObject, 2.0);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadTextBitmapJpRect20x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.TextBitmapJp].OffObject, 2.0);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadTextBitmapJpOnRect20x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.TextBitmapJp].OnObject, 2.0);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

{$ENDREGION}

{$REGION 'Read properties for 3.0 scale'}

procedure TBitmapSwitchObject.ReadTrackMargins30x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.Track].OffObject, 3.0);
  Link.CapInsets.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadTrackRect30x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.Track].OffObject, 3.0);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadTrackOnRect30x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.Track].OnObject, 3.0);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadTrackZoomRect30x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.TrackZoom].OffObject, 3.0);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadThumbMargins30x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.Thumb].OffObject, 3.0);
  Link.CapInsets.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadThumbRect30x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.Thumb].OffObject, 3.0);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadThumbOnRect30x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.Thumb].OnObject, 3.0);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadThumbPressedRect30x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.ThumbPressed].OffObject, 3.0);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadTextBitmapRect30x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.TextBitmap].OffObject, 3.0);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadTextBitmapOnRect30x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.TextBitmap].OnObject, 3.0);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadTextBitmapEnRect30x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.TextBitmapEn].OffObject, 3.0);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadTextBitmapEnOnRect30x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.TextBitmapEn].OnObject, 3.0);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadTextBitmapJpRect30x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.TextBitmapJp].OffObject, 3.0);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TBitmapSwitchObject.ReadTextBitmapJpOnRect30x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FStyleObjects[TSwitchPart.TextBitmapJp].OnObject, 3.0);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

{$ENDREGION}

function TBitmapSwitchObject.GetCapMode(const Index: TSwitchPart): TCapWrapMode;
begin
  Result := FStyleObjects[Index].OffObject.CapMode;
end;

function TBitmapSwitchObject.GetOffObject(Index: TSwitchPart): TBitmapLinks;
begin
  Result := FStyleObjects[Index].OffObject.SourceLink;
end;

function TBitmapSwitchObject.GetOnObject(Index: TSwitchPart): TBitmapLinks;
begin
  Result := FStyleObjects[Index].OnObject.SourceLink;
end;

function TBitmapSwitchObject.GetSourceLookup: string;
begin
  Result := FSourceLookup;
end;

function TBitmapSwitchObject.GetWrapMode(const Index: TSwitchPart): TImageWrapMode;
begin
  Result := FStyleObjects[Index].OffObject.WrapMode;
end;

function TBitmapSwitchObject.IsThumbTextVisible: Boolean;
begin
  Result := inherited IsThumbTextVisible;
end;

function TBitmapSwitchObject.IsTrackTextVisible: Boolean;
begin
  Result := inherited IsTrackTextVisible or not FStyleObjects[TSwitchPart.TextBitmap].OffObject.IsEmpty;
end;

procedure TBitmapSwitchObject.Loaded;
begin
  inherited;
  CopyCaps(TSwitchPart.Track);
  CopyCaps(TSwitchPart.Thumb);
  CopyCaps(TSwitchPart.Thumb, TSwitchPart.ThumbPressed);
end;

procedure TBitmapSwitchObject.SetNewScene(AScene: IScene);
var
  Item: TSwitchRec;
begin
  inherited ;
  if Assigned(AScene) and not (csDestroying in ComponentState) then
    for Item in FStyleObjects do
    begin
      Item.OnObject.SetNewScene(Scene);
      Item.OffObject.SetNewScene(Scene);
    end;
end;

procedure TBitmapSwitchObject.CopyCaps(const Index: TSwitchPart);
var
  I: Integer;
  OnObject, OffObject: TStyleObject;
begin
  OnObject := FStyleObjects[Index].OnObject;
  OffObject := FStyleObjects[Index].OffObject;
  if OnObject.SourceLink.Count = OffObject.SourceLink.Count then
    for I := 0 to OffObject.SourceLink.Count - 1 do
      if not OnObject.SourceLink.Links[I].CapInsetsChanged then
        OnObject.SourceLink.Links[I].CapInsets := OffObject.SourceLink.Links[I].CapInsets;
end;

procedure TBitmapSwitchObject.CopyCaps(const Source, Dest: TSwitchPart);
var
  I: Integer;
  SourceObject, DestObject: TStyleObject;
begin
  SourceObject := FStyleObjects[Source].OffObject;
  DestObject := FStyleObjects[Dest].OffObject;
  if SourceObject.SourceLink.Count = DestObject.SourceLink.Count then
    for I := 0 to SourceObject.SourceLink.Count - 1 do
      if not DestObject.SourceLink.Links[I].CapInsetsChanged then
        DestObject.SourceLink.Links[I].CapInsets := SourceObject.SourceLink.Links[I].CapInsets;
  CopyCaps(Dest);
end;

procedure TBitmapSwitchObject.PaintThumbForValue(const R: TRectF;
  const Value: Boolean);
begin
  if Pressed and not FStyleObjects[TSwitchPart.ThumbPressed].OffObject.IsEmpty  then
    FStyleObjects[TSwitchPart.ThumbPressed].OffObject.DrawToCanvas(Canvas, R, AbsoluteOpacity)
  else if Value and not FStyleObjects[TSwitchPart.Thumb].OnObject.IsEmpty then
    FStyleObjects[TSwitchPart.Thumb].OnObject.DrawToCanvas(Canvas, R, AbsoluteOpacity)
  else
    FStyleObjects[TSwitchPart.Thumb].OffObject.DrawToCanvas(Canvas, R, AbsoluteOpacity);
end;

procedure TBitmapSwitchObject.PaintThumbTextForValue(const R: TRectF; const Value: Boolean);
var
  OnObject, OffObject: TStyleObject;
begin
  if ThumbText.Kind = TSwitchTextKind.stkBitmap then
  begin
    OnObject := nil;
    OffObject := nil;
    if Assigned(FLocaleService) then
    begin
      if FLocaleService.GetCurrentLangID = 'en' then
      begin
        if Value then
          OnObject := FStyleObjects[TSwitchPart.TextBitmapEn].OnObject
        else
          OffObject := FStyleObjects[TSwitchPart.TextBitmapEn].OffObject
      end;
      if (FLocaleService.GetCurrentLangID = 'jp') or (FLocaleService.GetCurrentLangID = 'ja') then
      begin
        if Value then
          OnObject := FStyleObjects[TSwitchPart.TextBitmapJp].OnObject
        else
          OffObject := FStyleObjects[TSwitchPart.TextBitmapJp].OffObject
      end;
    end;

    if Value and (not Assigned(OnObject) or OnObject.IsEmpty) then
      OnObject := FStyleObjects[TSwitchPart.TextBitmap].OnObject
    else if not Value and (not Assigned(OffObject) or OffObject.IsEmpty) then
      OffObject := FStyleObjects[TSwitchPart.TextBitmap].OffObject;

    if Value and Assigned(OnObject) then
      OnObject.DrawToCanvas(Canvas, R, AbsoluteOpacity);
    if not Value and Assigned(OffObject) then
      OffObject.DrawToCanvas(Canvas, R, AbsoluteOpacity);
  end else
    inherited ;
end;

procedure TBitmapSwitchObject.PaintTrackForValue(const R: TRectF;
  const Value: Boolean);
begin
  if Value and not FStyleObjects[TSwitchPart.Track].OnObject.IsEmpty then
  begin
    FStyleObjects[TSwitchPart.Track].OnObject.DrawToCanvas(Canvas, R, AbsoluteOpacity);
    if Kind = TSwitchKind.skFadingTracks then
    begin
      if not FStyleObjects[TSwitchPart.TrackZoom].OnObject.IsEmpty then
      begin
        if FTracking or FAnimating then
          R.Inflate(-(R.Width * GetThumbValue) / 2, - (R.Height * GetThumbValue) / 2);
        FStyleObjects[TSwitchPart.TrackZoom].OnObject.DrawToCanvas(Canvas, R, AbsoluteOpacity);
      end;
    end;
  end
  else begin
    FStyleObjects[TSwitchPart.Track].OffObject.DrawToCanvas(Canvas, R, AbsoluteOpacity);
    if Kind = TSwitchKind.skFadingTracks then
    begin
      if not FStyleObjects[TSwitchPart.TrackZoom].OffObject.IsEmpty then
      begin
        if FTracking or FAnimating then
          R.Inflate(-(R.Width * GetThumbValue) / 2, - (R.Height * GetThumbValue) / 2);
        FStyleObjects[TSwitchPart.TrackZoom].OffObject.DrawToCanvas(Canvas, R, AbsoluteOpacity);
      end;
    end;
  end;
end;

procedure TBitmapSwitchObject.PaintTrackTextForValue(const R: TRectF;
  const Value: Boolean);
var
  OnObject, OffObject: TStyleObject;
begin
  if TrackText.Kind = TSwitchTextKind.stkBitmap then
  begin
    OnObject := nil;
    OffObject := nil;
    if Assigned(FLocaleService) then
    begin
      if FLocaleService.GetCurrentLangID = 'en' then
      begin
        if Value then
          OnObject := FStyleObjects[TSwitchPart.TextBitmapEn].OnObject
        else
          OffObject := FStyleObjects[TSwitchPart.TextBitmapEn].OffObject
      end;
      if (FLocaleService.GetCurrentLangID = 'jp') or (FLocaleService.GetCurrentLangID = 'ja') then
      begin
        if Value then
          OnObject := FStyleObjects[TSwitchPart.TextBitmapJp].OnObject
        else
          OffObject := FStyleObjects[TSwitchPart.TextBitmapJp].OffObject
      end;
    end;

    if Value and (not Assigned(OnObject) or OnObject.IsEmpty) then
      OnObject := FStyleObjects[TSwitchPart.TextBitmap].OnObject
    else if not Value and (not Assigned(OffObject) or OffObject.IsEmpty) then
      OffObject := FStyleObjects[TSwitchPart.TextBitmap].OffObject;

    if Value and Assigned(OnObject) then
      OnObject.DrawToCanvas(Canvas, R, AbsoluteOpacity);
    if not Value and Assigned(OffObject) then
      OffObject.DrawToCanvas(Canvas, R, AbsoluteOpacity);
  end else
    inherited ;
end;

procedure TBitmapSwitchObject.SetCapMode(const Index: TSwitchPart; const Value: TCapWrapMode);
begin
  FStyleObjects[Index].OnObject.CapMode := Value;
  FStyleObjects[Index].OffObject.CapMode := Value;
  if Index = TSwitchPart.Thumb then
  begin
    FStyleObjects[TSwitchPart.ThumbPressed].OnObject.CapMode := Value;
    FStyleObjects[TSwitchPart.ThumbPressed].OffObject.CapMode := Value;
  end;
end;

procedure TBitmapSwitchObject.SetOffObject(Index: TSwitchPart;
  const Value: TBitmapLinks);
begin
  FStyleObjects[Index].OffObject.SourceLink.Assign(Value);
end;

procedure TBitmapSwitchObject.SetOnObject(Index: TSwitchPart;
  const Value: TBitmapLinks);
begin
  FStyleObjects[Index].OnObject.SourceLink.Assign(Value);
end;

procedure TBitmapSwitchObject.SetSourceLookup(const Value: string);
var
  Item: TSwitchRec;
begin
  FSourceLookup := Value;
  for Item in FStyleObjects do
  begin
    Item.OnObject.SourceLookup := FSourceLookup;
    Item.OffObject.SourceLookup := FSourceLookup;
  end;
end;

procedure TBitmapSwitchObject.SetWrapMode(const Index: TSwitchPart;
  const Value: TImageWrapMode);
begin
  FStyleObjects[Index].OnObject.WrapMode := Value;
  FStyleObjects[Index].OffObject.WrapMode := Value;
  if Index = TSwitchPart.Thumb then
  begin
    FStyleObjects[TSwitchPart.ThumbPressed].OnObject.WrapMode := Value;
    FStyleObjects[TSwitchPart.ThumbPressed].OffObject.WrapMode := Value;
  end;
end;

initialization
  RegisterFmxClasses([TSwitchObject, TBitmapSwitchObject]);
end.

