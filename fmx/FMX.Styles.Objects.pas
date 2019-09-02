{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit FMX.Styles.Objects;

interface

uses
  System.Classes, System.Types, System.Rtti, System.UITypes, System.UIConsts, System.Math,
  System.SysUtils, System.StrUtils, FMX.Types, FMX.Controls, FMX.TextLayout, FMX.Text,
  FMX.Messages, FMX.Platform, FMX.MultiResBitmap, FMX.Objects, FMX.Graphics;

{$SCOPEDENUMS ON}

type

  TStyleTrigger = (stMouseOver, stPressed, stSelected, stFocused, stChecked, stActive);

{ Image Objects }

  TBitmapLink = class(TCollectionItem)
  private
    FCapInsetsChanged: Boolean;
    FCapInsets: TBounds;
    FSourceRect: TBounds;
    FScale: Single;
    procedure SetCapInsets(const Value: TBounds);
    procedure SetSourceRect(const Value: TBounds);
    procedure SetScale(const Value: Single);
    procedure DoCapInsetsChanged(Sender: TObject);
    function IsScaleStored: Boolean;
  protected
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property CapInsetsChanged: Boolean read FCapInsetsChanged;
  published
    property CapInsets: TBounds read FCapInsets write SetCapInsets;
    property Scale: Single read FScale write SetScale stored IsScaleStored;
    property SourceRect: TBounds read FSourceRect write SetSourceRect;
  end;

  TCapWrapMode = (cmStretch, cmTile);

  TBitmapLinks = class(TCollection)
  private
    function GetLink(AIndex: Integer): TBitmapLink;
    function GetEmpty: Boolean;
  public
    constructor Create;
    function LinkByScale(const AScale: Single): TBitmapLink;
    property Links[AIndex: Integer]: TBitmapLink read GetLink;
    property Empty: Boolean read GetEmpty;
  published
  end;

  TCustomStyleObject = class(TControl)
  private
    FOpaque: Boolean;
    FSource: TImage;
    FSourceLookup: string;
    FCapMode: TCapWrapMode;
    FWrapMode: TImageWrapMode;
    procedure SetCapMode(const Value: TCapWrapMode);
    procedure SetWrapMode(const Value: TImageWrapMode);
    procedure ReadOpaque(Reader: TReader);
    procedure WriteOpaque(Writer: TWriter);
    procedure SetOpaque(const Value: Boolean);
    procedure SetSource(const Value: TImage);
    procedure SetSourceLookup(const Value: string);
    procedure ReadMarginWrapMode(Reader: TReader);
  class var
    FAlignToPixels: Boolean;
  protected
    function FindOrCreateBitmapLink(const Links: TBitmapLinks; const Scale: Single): TBitmapLink;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Paint; override;
    procedure FreeNotification(AObject: TObject); override;
  protected
    function GetCurrentLink: TBitmapLinks; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DrawToCanvas(const Canvas: TCanvas; const ARect: TRectF; const AOpacity: Single = 1.0);
    function IsEmpty: Boolean;
    property Opaque: Boolean read FOpaque write SetOpaque;
    property Source: TImage read FSource write SetSource;
    class function ScreenScaleToStyleScale(const ScreenScale: Single): Single;
    class property AlignToPixels: Boolean read FAlignToPixels write FAlignToPixels;
  published
    property Align;
    property Anchors;
    property CapMode: TCapWrapMode read FCapMode write SetCapMode default TCapWrapMode.cmStretch;
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
    property SourceLookup: string read FSourceLookup write SetSourceLookup;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property WrapMode: TImageWrapMode read FWrapMode write SetWrapMode default TImageWrapMode.iwStretch;
    property Visible default True;
    property Width;
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

  TStyleObject = class(TCustomStyleObject)
  private
    FSourceLink: TBitmapLinks;
    procedure SetSourceLink(const Value: TBitmapLinks);
    procedure ReadBitmapMargin(Reader: TReader);
    procedure ReadSourceRect(Reader: TReader);
    procedure ReadBitmapMargin20x(Reader: TReader);
    procedure ReadSourceRect20x(Reader: TReader);
    procedure ReadBitmapMargin15x(Reader: TReader);
    procedure ReadSourceRect15x(Reader: TReader);
    procedure ReadBitmapMargin30x(Reader: TReader);
    procedure ReadSourceRect30x(Reader: TReader);
    procedure ReadSourceLeftI(Reader: TReader);
    procedure ReadSourceRectI(Reader: TReader);
    procedure ReadSourceBottomI(Reader: TReader);
    procedure ReadSourceRightI(Reader: TReader);
    procedure ReadSourceTopI(Reader: TReader);
    procedure ReadBitmapMarginBottomI(Reader: TReader);
    procedure ReadBitmapMarginI(Reader: TReader);
    procedure ReadBitmapMarginLeftI(Reader: TReader);
    procedure ReadBitmapMarginRightI(Reader: TReader);
    procedure ReadBitmapMarginTopI(Reader: TReader);
  protected
    function GetCurrentLink: TBitmapLinks; override;
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property SourceLink: TBitmapLinks read FSourceLink write SetSourceLink;
  end;

  // XE3 Compatible
  TSubImage = class(TStyleObject);

  TActiveStyleObject = class(TCustomStyleObject)
  private
    FSourceLink: TBitmapLinks;
    FActive: Boolean;
    FActiveLink: TBitmapLinks;
    FActiveAnimation: TAnimation;
    FTrigger: TStyleTrigger;
    procedure SetActiveLink(const Value: TBitmapLinks);
    procedure SetTrigger(const Value: TStyleTrigger);
    procedure ReadBitmapMargin(Reader: TReader);
    procedure ReadSourceRect(Reader: TReader);
    procedure ReadActiveRect(Reader: TReader);
    procedure SetSourceLink(const Value: TBitmapLinks);
    procedure ReadActiveRect15x(Reader: TReader);
    procedure ReadBitmapMargin15x(Reader: TReader);
    procedure ReadSourceRect15x(Reader: TReader);
    procedure ReadActiveRect20x(Reader: TReader);
    procedure ReadBitmapMargin20x(Reader: TReader);
    procedure ReadSourceRect20x(Reader: TReader);
    procedure Triggered(Sender: TObject);
    procedure ReadActiveRect30x(Reader: TReader);
    procedure ReadBitmapMargin30x(Reader: TReader);
    procedure ReadSourceRect30x(Reader: TReader);
  protected
    procedure SetupAnimations; virtual;
    function GetCurrentLink: TBitmapLinks; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartTriggerAnimation(const AInstance: TFmxObject; const ATrigger: string); override;
  published
    property ActiveTrigger: TStyleTrigger read FTrigger write SetTrigger;
    property ActiveLink: TBitmapLinks read FActiveLink write SetActiveLink;
    property SourceLink: TBitmapLinks read FSourceLink write SetSourceLink;
  end;

  TTabStyleObject = class(TActiveStyleObject)
  private
    FHot: Boolean;
    FHotLink: TBitmapLinks;
    FHotAnimation: TAnimation;
    procedure SetHotLink(const Value: TBitmapLinks);
    procedure HotTriggered(Sender: TObject);
    procedure ReadHotRect(Reader: TReader);
    procedure ReadHotRect15x(Reader: TReader);
    procedure ReadHotRect20x(Reader: TReader);
    procedure ReadHotRect30x(Reader: TReader);
  protected
    procedure SetupAnimations; override;
    function GetCurrentLink: TBitmapLinks; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartTriggerAnimation(const AInstance: TFmxObject; const ATrigger: string); override;
  published
    property HotLink: TBitmapLinks read FHotLink write SetHotLink;
  end;

  TCheckStyleObject = class(TTabStyleObject)
  private
    FActiveHotLink: TBitmapLinks;
    procedure SetActiveHotLink(const Value: TBitmapLinks);
    procedure ReadActiveHotRect(Reader: TReader);
    procedure ReadActiveHotRect15x(Reader: TReader);
    procedure ReadActiveHotRect20x(Reader: TReader);
    procedure ReadActiveHotRect30x(Reader: TReader);
  protected
    procedure SetupAnimations; override;
    function GetCurrentLink: TBitmapLinks; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ActiveHotLink: TBitmapLinks read FActiveHotLink write SetActiveHotLink;
  end;

  TButtonAnimation = (Normal, Hot, Pressed, Focused);

  TButtonStyleObject = class(TCustomStyleObject)
  private type
    TTriggerAndLink = record
      Animation: TAnimation;
      Link: TBitmapLinks;
    end;
  private
    FCurrent: TButtonAnimation;
    FTriggerSources: array [TButtonAnimation] of TTriggerAndLink;
    procedure NormalTriggered(Sender: TObject);
    procedure HotTriggered(Sender: TObject);
    procedure PressedTriggered(Sender: TObject);
    procedure FocusedTriggered(Sender: TObject);
    function GetLink(Index: TButtonAnimation): TBitmapLinks;
    procedure SetLink(Index: TButtonAnimation; const Value: TBitmapLinks);
    procedure ReadHotRect(Reader: TReader);
    procedure ReadBitmapMargin(Reader: TReader);
    procedure ReadSourceRect(Reader: TReader);
    procedure ReadPressedRect(Reader: TReader);
    procedure ReadFocusedRect(Reader: TReader);
    procedure ReadSourceRect20x(Reader: TReader);
    procedure ReadFocusedRect20x(Reader: TReader);
    procedure ReadHotRect20x(Reader: TReader);
    procedure ReadPressedRect20x(Reader: TReader);
    procedure ReadBitmapMargin20x(Reader: TReader);
    procedure ReadBitmapMargin15x(Reader: TReader);
    procedure ReadFocusedRect15x(Reader: TReader);
    procedure ReadHotRect15x(Reader: TReader);
    procedure ReadPressedRect15x(Reader: TReader);
    procedure ReadSourceRect15x(Reader: TReader);
    procedure ReadBitmapMargin30x(Reader: TReader);
    procedure ReadFocusedRect30x(Reader: TReader);
    procedure ReadHotRect30x(Reader: TReader);
    procedure ReadPressedRect30x(Reader: TReader);
    procedure ReadSourceRect30x(Reader: TReader);
  protected
    function GetCurrentLink: TBitmapLinks; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartTriggerAnimation(const AInstance: TFmxObject; const ATrigger: string); override;
  published
    property HotLink: TBitmapLinks index TButtonAnimation.Hot read GetLink write SetLink;
    property FocusedLink: TBitmapLinks index TButtonAnimation.Focused read GetLink write SetLink;
    property NormalLink: TBitmapLinks index TButtonAnimation.Normal read GetLink write SetLink;
    property PressedLink: TBitmapLinks index TButtonAnimation.Pressed read GetLink write SetLink;
  end;

  TSystemButtonObject = class(TButtonStyleObject)
  private
    FInactive: Boolean;
    FInactiveAnimation: TAnimation;
    FInactiveLink: TBitmapLinks;
    procedure SetInactiveLink(const Value: TBitmapLinks);
    procedure ReadInactiveRect(Reader: TReader);
    procedure ReadInactiveRect15x(Reader: TReader);
    procedure ReadInactiveRect20x(Reader: TReader);
    procedure InactiveTriggered(Sender: TObject);
    procedure ReadInactiveRect30x(Reader: TReader);
  protected
    function GetCurrentLink: TBitmapLinks; override;
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property InactiveLink: TBitmapLinks read FInactiveLink write SetInactiveLink;
  end;

{ Text Objects }

  TStyleShadow = class(TPersistent)
  private
    FColor: TAlphaColor;
    FOnChanged: TNotifyEvent;
    FOffset: TPosition;
    procedure SetColor(const Value: TAlphaColor);
    procedure SetOffset(const Value: TPosition);
    procedure OffsetChanged(Sender: TObject);
  protected
    procedure DoChanged; virtual;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TAlphaColor read FColor write SetColor;
    property Offset: TPosition read FOffset write SetOffset;
  end;

  TStyleTextObject = class(TText)
  private
    FSavedColor: TAlphaColor;
    FSavedShadow: TStyleShadow;
    FShadow: TStyleShadow;
    FShadowVisible: Boolean;
    FCharCase: TEditCharCase;
    procedure SetShadow(const Value: TStyleShadow);
    procedure SetShadowVisible(const Value: Boolean);
    procedure SetCharCase(const Value: TEditCharCase);
  protected
    procedure Paint; override;
    function ConvertText(const Value: string): string; override;
    procedure ShadowChanged(Sender: TObject);
    procedure Loaded; override;
    property SavedColor: TAlphaColor read FSavedColor;
    property SavedShadow: TStyleShadow read FSavedShadow;
  protected
    function GetCurrentColor: TAlphaColor; virtual;
    function GetCurrentText: string; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DrawToCanvas(const Canvas: TCanvas; const ARect: TRectF; const AOpacity: Single = 1.0);
  published
    property CharCase: TEditCharCase read FCharCase write SetCharCase default TEditCharCase.ecNormal;
    property Shadow: TStyleShadow read FShadow write SetShadow;
    property ShadowVisible: Boolean read FShadowVisible write SetShadowVisible;
    property HitTest default False;
  end;

  TStyleTextAnimation = class(TAnimation)
  private
    FColor: TAlphaColor;
    FShadow: TStyleShadow;
    procedure SetShadow(const Value: TStyleShadow);
  protected
    procedure ProcessAnimation; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function CreateAnimation(const Trigger: string; const Event: TNotifyEvent): TStyleTextAnimation;
    property Color: TAlphaColor read FColor write FColor;
    property Shadow: TStyleShadow read FShadow write SetShadow;
  end;

  TActiveStyleTextObject = class(TStyleTextObject)
  private
    FTrigger: TStyleTrigger;
    FAnimation: TStyleTextAnimation;
    procedure SetTrigger(const Value: TStyleTrigger);
    function GetActiveColor: TAlphaColor;
    function GetActiveShadow: TStyleShadow;
    procedure SetActiveColor(const Value: TAlphaColor);
    procedure SetActiveShadow(const Value: TStyleShadow);
  protected
    procedure Triggered(Sender: TObject);
    procedure SetupAnimations; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartTriggerAnimation(const AInstance: TFmxObject; const ATrigger: string); override;
  published
    property ActiveTrigger: TStyleTrigger read FTrigger write SetTrigger;
    property ActiveColor: TAlphaColor read GetActiveColor write SetActiveColor;
    property ActiveShadow: TStyleShadow read GetActiveShadow write SetActiveShadow;
  end;

  TTabStyleTextObject = class(TActiveStyleTextObject)
  private
    FHotAnimation: TStyleTextAnimation;
    procedure SetHotColor(const Value: TAlphaColor);
    procedure SetHotShadow(const Value: TStyleShadow);
    function GetHotColor: TAlphaColor;
    function GetHotShadow: TStyleShadow;
  protected
    procedure HotTriggered(Sender: TObject);
    procedure SetupAnimations; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartTriggerAnimation(const AInstance: TFmxObject; const ATrigger: string); override;
  published
    property HotColor: TAlphaColor read GetHotColor write SetHotColor;
    property HotShadow: TStyleShadow read GetHotShadow write SetHotShadow;
  end;

  TButtonStyleTextObject = class(TStyleTextObject)
  private
    FTriggerSources: array [TButtonAnimation] of TStyleTextAnimation;
    procedure NormalTriggered(Sender: TObject);
    procedure HotTriggered(Sender: TObject);
    procedure PressedTriggered(Sender: TObject);
    procedure FocusedTriggered(Sender: TObject);
    function GetColor(Index: TButtonAnimation): TAlphaColor;
    procedure SetColor(Index: TButtonAnimation; const Value: TAlphaColor);
    function GetShadow(Index: TButtonAnimation): TStyleShadow;
    procedure SetShadow(Index: TButtonAnimation; const Value: TStyleShadow);
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartTriggerAnimation(const AInstance: TFmxObject; const ATrigger: string); override;
    property NormalShadow: TStyleShadow index TButtonAnimation.Normal read GetShadow write SetShadow;
  published
    property HotColor: TAlphaColor index TButtonAnimation.Hot read GetColor write SetColor;
    property HotShadow: TStyleShadow index TButtonAnimation.Hot read GetShadow write SetShadow;
    property FocusedColor: TAlphaColor index TButtonAnimation.Focused read GetColor write SetColor;
    property FocusedShadow: TStyleShadow index TButtonAnimation.Focused read GetShadow write SetShadow;
    property NormalColor: TAlphaColor index TButtonAnimation.Normal read GetColor write SetColor;
    property PressedColor: TAlphaColor index TButtonAnimation.Pressed read GetColor write SetColor;
    property PressedShadow: TStyleShadow index TButtonAnimation.Pressed read GetShadow write SetShadow;
  end;

  TActiveOpacityObject = class(TControl)
  private
    FTrigger: TStyleTrigger;
    FActiveAnimation: TAnimation;
    FActiveOpacity: Single;
    FSavedOpacity: Single;
    function IsActiveOpacityStored: Boolean;
    procedure SetTrigger(const Value: TStyleTrigger);
    procedure Triggered(Sender: TObject);
  protected
    procedure SetupAnimation; virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartTriggerAnimation(const AInstance: TFmxObject; const ATrigger: string); override;
  published
    property ActiveTrigger: TStyleTrigger read FTrigger write SetTrigger;
    property ActiveOpacity: Single read FActiveOpacity write FActiveOpacity stored IsActiveOpacityStored;
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

implementation

uses System.TypInfo, FMX.Consts;

const
  WrapModes: array[0..4] of TIdentMapEntry = (
    (Value: Integer(TImageWrapMode.iwOriginal); Name: 'iwOriginal'),
    (Value: Integer(TImageWrapMode.iwFit); Name: 'iwFit'),
    (Value: Integer(TImageWrapMode.iwStretch); Name: 'iwStretch'),
    (Value: Integer(TImageWrapMode.iwTile); Name: 'iwTile'),
    (Value: Integer(TImageWrapMode.iwCenter); Name: 'iwCenter')
  );

{ TBitmapLink }

constructor TBitmapLink.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FScale := 1;
  FSourceRect := TBounds.Create(NullRect);
  FCapInsets := TBounds.Create(NullRect);
  FCapInsets.OnChange := DoCapInsetsChanged;
end;

destructor TBitmapLink.Destroy;
begin
  FSourceRect.Free;
  FCapInsets.Free;
  inherited;
end;

procedure TBitmapLink.DoCapInsetsChanged(Sender: TObject);
begin
  FCapInsetsChanged := True;
end;

function TBitmapLink.IsScaleStored: Boolean;
begin
  Result := not SameValue(1.0, FScale, TEpsilon.Scale);
end;

procedure TBitmapLink.SetCapInsets(const Value: TBounds);
begin
  FCapInsets.Assign(Value);
end;

procedure TBitmapLink.SetScale(const Value: Single);
begin
  FScale := Value;
end;

procedure TBitmapLink.SetSourceRect(const Value: TBounds);
begin
  FSourceRect.Assign(Value);
end;

{ TBitmapLinks }

constructor TBitmapLinks.Create;
begin
  inherited Create(TBitmapLink);
end;

function TBitmapLinks.GetEmpty: Boolean;
begin
  Result := (Count = 0);
end;

function TBitmapLinks.GetLink(AIndex: Integer): TBitmapLink;
begin
  Result := TBitmapLink(Items[AIndex]);
end;

function TBitmapLinks.LinkByScale(const AScale: Single): TBitmapLink;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if SameValue(Links[I].Scale, AScale, TEpsilon.Scale) then
      Exit(Links[I]);
  Result := nil;
end;

{ TCustomStyleObject }

constructor TCustomStyleObject.Create(AOwner: TComponent);
begin
  inherited;
  SetAcceptsControls(False);
  HitTest := False;
  FWrapMode := TImageWrapMode.iwStretch;
end;

destructor TCustomStyleObject.Destroy;
begin
  if Assigned(FSource) then
    FSource.RemoveFreeNotify(Self);
  inherited;
end;

class function TCustomStyleObject.ScreenScaleToStyleScale(const ScreenScale: Single): Single;
begin
  if ScreenScale < 1.25 then
    Result := 1
  else if ScreenScale < 1.75 then
    Result := 1.5
  else if ScreenScale < 2.1 then
    Result := 2
  else if ScreenScale < 3.1 then
    Result := 3
  else
    Result := 1;
end;

procedure TCustomStyleObject.DrawToCanvas(const Canvas: TCanvas; const ARect: TRectF; const AOpacity: Single = 1.0);
var
  CurrentLink: TBitmapLinks;
  Link: TBitmapLink;
  Item: TCustomBitmapItem;
  AlignedRect, LR, LRUnscaled, R, IntersectionRect, UnscaledMargins: TRectF;
  i, j: Integer;
  B: TBitmap;
  SR, SRScaled: TRectF;
  ScreenScale: Single;
  StyleScale: Single;
  HorzMarginsOnly, VertMarginsOnly: Boolean;
  SaveState: TCanvasSaveState;
  HighSpeed: Boolean;
  M: TMatrix;
begin
  CurrentLink := GetCurrentLink;
  if not Assigned(CurrentLink) or CurrentLink.Empty or not Assigned(FSource) then
    Exit;

  if Assigned(Scene) then
    ScreenScale := Scene.GetSceneScale
  else
    ScreenScale := Canvas.Scale;

  StyleScale := ScreenScaleToStyleScale(ScreenScale);

  HighSpeed := Abs(Frac(ScreenScale)) < TEpsilon.Scale;

  Link := CurrentLink.LinkByScale(StyleScale);
  if not Assigned(Link) then Exit;

  Item := FSource.MultiResBitmap.ItemByScale(StyleScale, False, False);

  if Assigned(Item) then
    B := Item.Bitmap
  else
    B := nil;

  if Assigned(B) and Assigned(B.ResourceBitmap) then
    B := B.ResourceBitmap;

  if not Assigned(B) or B.IsEmpty then
    Exit;

  SR := Link.SourceRect.Rect;
  if SR.IsEmpty then
    Exit;

  if FOpaque and SameValue(AOpacity, 1.0, TEpsilon.Vector) then
    Canvas.Blending := False
  else
    Canvas.Blending := True;

  SRScaled := TRectF.Create(SR.Left / ScreenScale, SR.Top / ScreenScale, SR.Right / ScreenScale, SR.Bottom / ScreenScale);

  if (TCanvasStyle.NeedGPUSurface in Canvas.GetCanvasStyle) and AlignToPixels then
  begin
    AlignedRect.Left := Canvas.AlignToPixelHorizontally(ARect.Left);
    AlignedRect.Top := Canvas.AlignToPixelVertically(ARect.Top);
    AlignedRect.Right := Canvas.AlignToPixelHorizontally(ARect.Right);
    AlignedRect.Bottom := AlignedRect.Top + Round(ARect.Height * ScreenScale) / ScreenScale; // keep ratio vertically
  end
  else
    AlignedRect := ARect;

  LR := TRectF.Create(AlignedRect.Left * ScreenScale, AlignedRect.Top * ScreenScale, AlignedRect.Right * ScreenScale, AlignedRect.Bottom * ScreenScale);
  LRUnscaled := AlignedRect;

  UnscaledMargins := TRectF.Create(Link.CapInsets.Left / ScreenScale, Link.CapInsets.Top / ScreenScale,
    Link.CapInsets.Right / ScreenScale, Link.CapInsets.Bottom / ScreenScale);


  if (Link.CapInsets.Left + Link.CapInsets.Right > LR.Width) then
  begin
    LR.Width := Link.CapInsets.Left + Link.CapInsets.Right;
    LRUnscaled.Width := LR.Width / ScreenScale;
    HorzMarginsOnly := True;
  end
  else
    HorzMarginsOnly := False;
  if (Link.CapInsets.Top + Link.CapInsets.Bottom > LR.Height) then
  begin
    LR.Height := Link.CapInsets.Top + Link.CapInsets.Bottom;
    LRUnscaled.Height := LR.Height / ScreenScale;
    VertMarginsOnly := True;
  end
  else
    VertMarginsOnly := False;

  if not Link.CapInsets.MarginEmpty then
  begin
    // fixed scale
    if HorzMarginsOnly or VertMarginsOnly then
    begin
      SaveState := Canvas.SaveState;
      Canvas.IntersectClipRect(ARect);
      // Offset for bottom oriented
      if VertMarginsOnly and Assigned(ParentControl) and (Position.Y + Height > ParentControl.Height / 2) then
      begin
        M := TMatrix.Identity;
        M.m32 := Height - LRUnscaled.Height;
        M := AbsoluteMatrix * M;
        Canvas.SetMatrix(M);
      end;
    end
    else
      SaveState := nil;
    { lefttop }
    R := TRectF.Create(LRUnscaled.Left, LRUnscaled.Top, LRUnscaled.Left + UnscaledMargins.Left, LRUnscaled.Top + UnscaledMargins.Top);
    Canvas.DrawBitmap(B, TRectF.Create(SR.Left, SR.Top, SR.Left + Link.CapInsets.Left, SR.Top + Link.CapInsets.Top), R,
      AOpacity, True);
    { righttop }
    R := TRectF.Create(LRUnscaled.Right - UnscaledMargins.Right, LRUnscaled.Top, LRUnscaled.Right, LRUnscaled.Top + UnscaledMargins.Top);
    Canvas.DrawBitmap(B, TRectF.Create(SR.Right - Link.CapInsets.Right, SR.Top, SR.Right,
      SR.Top + Link.CapInsets.Top), R, AOpacity, True);
    { leftbottom }
    R := TRectF.Create(LRUnscaled.Left, LRUnscaled.Bottom - UnscaledMargins.Bottom, LRUnscaled.Left + UnscaledMargins.Left, LRUnscaled.Bottom);
    Canvas.DrawBitmap(B, TRectF.Create(SR.Left, SR.Bottom - Link.CapInsets.Bottom,
      SR.Left + Link.CapInsets.Left, SR.Bottom), R, AOpacity, True);
    { rightbottom }
    R := TRectF.Create(LRUnscaled.Right - UnscaledMargins.Right, LRUnscaled.Bottom - UnscaledMargins.Bottom, LRUnscaled.Right, LRUnscaled.Bottom);
    Canvas.DrawBitmap(B, TRectF.Create(SR.Right - Link.CapInsets.Right,
      SR.Bottom - Link.CapInsets.Bottom, SR.Right, SR.Bottom), R,
      AOpacity, True);
    if not HorzMarginsOnly then
    begin
      { top }
      if CapMode = TCapWrapMode.cmTile then
      begin
        for i := 0 to Ceil((LR.Width - Link.CapInsets.Left - Link.CapInsets.Right) / (SR.Width - Link.CapInsets.Left - Link.CapInsets.Right)) do
        begin
          R := TRectF.Create(LRUnscaled.Left + UnscaledMargins.Left, LRUnscaled.Top, LRUnscaled.Left + SRScaled.Width - UnscaledMargins.Right, LRUnscaled.Top + UnscaledMargins.Top);
          OffsetRect(R, i * R.Width, 0);
          IntersectRect(IntersectionRect, TRectF.Create(LRUnscaled.Left + UnscaledMargins.Left, LRUnscaled.Top, LRUnscaled.Right - UnscaledMargins.Right, LRUnscaled.Top + UnscaledMargins.Top), R);
          Canvas.DrawBitmap(B, TRectF.Create(SR.Left + Link.CapInsets.Left, SR.Top, SR.Right - Link.CapInsets.Right, SR.Top + Link.CapInsets.Top),
            IntersectionRect, AOpacity, True)
        end;
      end
      else
        Canvas.DrawBitmap(B, TRectF.Create(SR.Left + Link.CapInsets.Left, SR.Top, SR.Right - Link.CapInsets.Right, SR.Top + Link.CapInsets.Top),
          TRectF.Create(LRUnscaled.Left + UnscaledMargins.Left, LRUnscaled.Top, LRUnscaled.Right - UnscaledMargins.Right, LRUnscaled.Top + UnscaledMargins.Top),
          AOpacity, True);
      { bottom }
      if CapMode = TCapWrapMode.cmTile then
      begin
        for i := 0 to Ceil((LR.Width - Link.CapInsets.Left - Link.CapInsets.Right) / (SR.Width - Link.CapInsets.Left - Link.CapInsets.Right)) do
        begin
          R := TRectF.Create(LRUnscaled.Left + UnscaledMargins.Left, LRUnscaled.Bottom - UnscaledMargins.Bottom, LRUnscaled.Left + SRScaled.Width - UnscaledMargins.Right, LRUnscaled.Bottom);
          OffsetRect(R, i * R.Width, 0);
          IntersectRect(IntersectionRect, TRectF.Create(LRUnscaled.Left + UnscaledMargins.Left, LRUnscaled.Bottom - UnscaledMargins.Bottom, LRUnscaled.Right - UnscaledMargins.Right,
            LRUnscaled.Bottom), R);
          Canvas.DrawBitmap(B, TRectF.Create(SR.Left + Link.CapInsets.Left, SR.Bottom - Link.CapInsets.Bottom, SR.Right - Link.CapInsets.Right, SR.Bottom),
            IntersectionRect, AOpacity, True)
        end;
      end
      else
        Canvas.DrawBitmap(B, TRectF.Create(SR.Left + Link.CapInsets.Left, SR.Bottom - Link.CapInsets.Bottom, SR.Right - Link.CapInsets.Right, SR.Bottom),
          TRectF.Create(LRUnscaled.Left + UnscaledMargins.Left, LRUnscaled.Bottom - UnscaledMargins.Bottom, LRUnscaled.Right - UnscaledMargins.Right, LRUnscaled.Bottom),
          AOpacity, True);
    end;
    if not VertMarginsOnly then
    begin
      { left }
      if CapMode = TCapWrapMode.cmTile then
      begin
        for j := 0 to Ceil((LR.Height - Link.CapInsets.Bottom - Link.CapInsets.Top) / (SR.Height - Link.CapInsets.Bottom - Link.CapInsets.Top))  do
        begin
          R := TRectF.Create(LRUnscaled.Left, LRUnscaled.Top + UnscaledMargins.Top, LRUnscaled.Left + UnscaledMargins.Left, LRUnscaled.Top + SRScaled.Height - UnscaledMargins.Bottom);
          OffsetRect(R, 0, j * R.Height);
          IntersectRect(IntersectionRect, TRectF.Create(LRUnscaled.Left, LRUnscaled.Top + UnscaledMargins.Top,
            LRUnscaled.Left + UnscaledMargins.Left, LRUnscaled.Bottom - UnscaledMargins.Bottom), R);
          Canvas.DrawBitmap(B, TRectF.Create(SR.Left, SR.Top + Link.CapInsets.Top, SR.Left + Link.CapInsets.Left, SR.Top + Link.CapInsets.Top + IntersectionRect.Height),
            IntersectionRect, AOpacity, True)
        end;
      end
      else
        Canvas.DrawBitmap(B, TRectF.Create(SR.Left, SR.Top + Link.CapInsets.Top, SR.Left + Link.CapInsets.Left, SR.Bottom - Link.CapInsets.Bottom),
          TRectF.Create(LRUnscaled.Left, LRUnscaled.Top + UnscaledMargins.Top, LRUnscaled.Left + UnscaledMargins.Left, LRUnscaled.Bottom - UnscaledMargins.Bottom), AOpacity, True);
      { right }
      if CapMode = TCapWrapMode.cmTile then
      begin
        for j := 0 to Ceil((LR.Height - Link.CapInsets.Bottom - Link.CapInsets.Top) / (SR.Height - Link.CapInsets.Bottom - Link.CapInsets.Top)) do
        begin
          R := TRectF.Create(LRUnscaled.Right - UnscaledMargins.Right, LRUnscaled.Top + UnscaledMargins.Top, LRUnscaled.Right, LRUnscaled.Top + SRScaled.Height - UnscaledMargins.Bottom);
          OffsetRect(R, 0, j * R.Height);
          IntersectRect(IntersectionRect, TRectF.Create(LRUnscaled.Width - UnscaledMargins.Right, LRUnscaled.Top + UnscaledMargins.Top, LRUnscaled.Right,
            LRUnscaled.Bottom - UnscaledMargins.Bottom), R);
          Canvas.DrawBitmap(B, TRectF.Create(SR.Right - Link.CapInsets.Right, SR.Top + Link.CapInsets.Top, SR.Right, SR.Top + Link.CapInsets.Top + IntersectionRect.Height),
            IntersectionRect, AOpacity, True)
        end;
      end
      else
        Canvas.DrawBitmap(B, TRectF.Create(SR.Right - Link.CapInsets.Right, SR.Top + Link.CapInsets.Top, SR.Right, SR.Bottom - Link.CapInsets.Bottom),
          TRectF.Create(LRUnscaled.Right - UnscaledMargins.Right, LRUnscaled.Top + UnscaledMargins.Top, LRUnscaled.Right, LRUnscaled.Bottom - UnscaledMargins.Bottom),
          AOpacity, True);
    end;
    { center }
    if not VertMarginsOnly and not HorzMarginsOnly then
    begin
      if FWrapMode = TImageWrapMode.iwTile then
      begin
        for i := 0 to Ceil((LR.Width - Link.CapInsets.Left - Link.CapInsets.Right) / (SR.Width - Link.CapInsets.Left - Link.CapInsets.Right)) do
          for j := 0 to Ceil((LR.Height - Link.CapInsets.Bottom - Link.CapInsets.Top) / (SR.Height - Link.CapInsets.Bottom - Link.CapInsets.Top)) do
          begin
            R := TRectF.Create(LRUnscaled.Left + UnscaledMargins.Left, LRUnscaled.Top + UnscaledMargins.Top,
              LRUnscaled.Left + SRScaled.Width - UnscaledMargins.Right, LRUnscaled.Top + SRScaled.Height - UnscaledMargins.Bottom);
            OffsetRect(R, i * R.Width, j * R.Height);
            IntersectRect(IntersectionRect, TRectF.Create(LRUnscaled.Left + UnscaledMargins.Left, LRUnscaled.Top + UnscaledMargins.Top,
              LRUnscaled.Right - UnscaledMargins.Right, LRUnscaled.Bottom - UnscaledMargins.Bottom), R);
            Canvas.DrawBitmap(B, TRectF.Create(SR.Left + Link.CapInsets.Left, SR.Top + Link.CapInsets.Top, SR.Left + Link.CapInsets.Left + IntersectionRect.Width, SR.Top + Link.CapInsets.Top + IntersectionRect.Height),
              IntersectionRect, AOpacity, True);
          end;
      end
      else
      begin
        R := TRectF.Create(LRUnscaled.Left + UnscaledMargins.Left, LRUnscaled.Top + UnscaledMargins.Top, LRUnscaled.Right - UnscaledMargins.Right, LRUnscaled.Bottom - UnscaledMargins.Bottom);
        Canvas.DrawBitmap(B, TRectF.Create(SR.Left + Link.CapInsets.Left, SR.Top + Link.CapInsets.Top, SR.Right - Link.CapInsets.Right, SR.Bottom - Link.CapInsets.Bottom),
          R, AOpacity, True);
      end;
    end;
    if HorzMarginsOnly or VertMarginsOnly then
      Canvas.RestoreState(SaveState);
  end
  else
  begin
    case FWrapMode of
      TImageWrapMode.iwOriginal:
        begin
          R := TRectF.Create(LRUnscaled.Left, LRUnscaled.Top, LRUnscaled.Left + SRScaled.Width, LRUnscaled.Top + SRScaled.Height);
          IntersectRect(IntersectionRect, LRUnscaled, R);
          Canvas.DrawBitmap(B, TRectF.Create(SR.Left, SR.Top, SR.Left + IntersectionRect.Width * ScreenScale, SR.Top + IntersectionRect.Height * ScreenScale),
            IntersectionRect, AOpacity, HighSpeed)
        end;
      TImageWrapMode.iwFit:
        begin
          LR := TRectF.Create(0, 0, Width, Height);
          R := Link.SourceRect.Rect;
          R.Fit(LR);
          R.Left := Round(R.Left);
          R.Top := Round(R.Top);
          R.Right := Round(R.Right);
          R.Bottom := Round(R.Bottom);
          Canvas.DrawBitmap(B, SR, R, AOpacity, HighSpeed);
        end;
      TImageWrapMode.iwStretch:
        begin
          Canvas.DrawBitmap(B, SR, LRUnscaled, AOpacity, True)
        end;
      TImageWrapMode.iwTile:
        begin
          for i := 0 to Ceil(LR.Width / SR.Width) do
            for j := 0 to Ceil(LR.Height / SR.Height) do
            begin
              R := TRectF.Create(LRUnscaled.Left, LRUnscaled.Top, LRUnscaled.Left + SRScaled.Width, LRUnscaled.Top + SRScaled.Height);
              OffsetRect(R, i * R.Width, j * R.Height);
              IntersectRect(IntersectionRect, LRUnscaled, R);
              Canvas.DrawBitmap(B, TRectF.Create(SR.Left, SR.Top, SR.Left + IntersectionRect.Width * ScreenScale, SR.Top + IntersectionRect.Height * ScreenScale),
                IntersectionRect, AOpacity, True)
            end;
        end;
      TImageWrapMode.iwCenter:
        begin
          R := RectF(0, 0, SR.Width / Item.Scale, SR.Height / Item.Scale);
          RectCenter(R, LRUnscaled);
          Canvas.DrawBitmap(B, SR, R, AOpacity, HighSpeed);
        end;
    end;
  end;
  Canvas.Blending := True;
end;

procedure TCustomStyleObject.Paint;
var
  R: TRectF;
begin
  DrawToCanvas(Canvas, LocalRect, AbsoluteOpacity);
  if (csDesigning in ComponentState) and not Locked and not FInPaintTo then
  begin
    R := LocalRect;
    InflateRect(R, -0.5, -0.5);
    Canvas.DrawDashRect(R, 0, 0, AllCorners, AbsoluteOpacity, $A0909090);
  end;
end;

procedure TCustomStyleObject.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Opaque', ReadOpaque, WriteOpaque, Opaque);
  { Backward compatibility with XE3 }
  Filer.DefineProperty('DisableInterpolation', IgnoreBooleanValue, nil, False);
  Filer.DefineProperty('BitmapScale', IgnoreFloatValue, nil, False);
  Filer.DefineProperty('MarginWrapMode', ReadMarginWrapMode, nil, False);
end;

function TCustomStyleObject.FindOrCreateBitmapLink(const Links: TBitmapLinks; const Scale: Single): TBitmapLink;
begin
  Result := Links.LinkByScale(Scale);
  if not Assigned(Result) then
  begin
    Result := TBitmapLink.Create(Links);
    Result.Scale := Scale;
  end;
end;

procedure TCustomStyleObject.FreeNotification(AObject: TObject);
begin
  inherited;
  if AObject = Source then
    Source := nil;
end;

function TCustomStyleObject.IsEmpty: Boolean;
var
  CurrentLink: TBitmapLinks;
begin
  CurrentLink := GetCurrentLink;
  Result := (Assigned(CurrentLink) and CurrentLink.Empty) or not Assigned(FSource);
end;

procedure TCustomStyleObject.ReadMarginWrapMode(Reader: TReader);
var
  MarginWrapMode: TImageWrapMode;
  Ident: Integer;
begin
  IdentToInt(Reader.ReadIdent, Ident, WrapModes);
  MarginWrapMode := TImageWrapMode(Ident);
  case MarginWrapMode of
    TImageWrapMode.iwStretch: CapMode := TCapWrapMode.cmStretch;
    TImageWrapMode.iwTile: CapMode := TCapWrapMode.cmTile;
  end;
end;

procedure TCustomStyleObject.ReadOpaque(Reader: TReader);
begin
  FOpaque := Reader.ReadBoolean;
end;

procedure TCustomStyleObject.WriteOpaque(Writer: TWriter);
begin
  Writer.WriteBoolean(FOpaque);
end;

procedure TCustomStyleObject.SetOpaque(const Value: Boolean);
begin
  if FOpaque <> Value then
  begin
    FOpaque := Value;
    Repaint;
  end;
end;

procedure TCustomStyleObject.SetSource(const Value: TImage);
begin
  if FSource <> Value then
  begin
    if Assigned(FSource) then
      FSource.RemoveFreeNotify(Self);
    FSource := Value;
    if Assigned(FSource) then
      FSource.AddFreeNotify(Self);
  end;
end;

procedure TCustomStyleObject.SetSourceLookup(const Value: string);
var
  O: TFmxObject;
begin
  if FSourceLookup <> Value then
  begin
    FSourceLookup := Value;
    if FSourceLookup <> '' then
    begin
      O := FMX.Types.FindStyleResource(FSourceLookup);
      if O is TImage then
        Source := TImage(O);
    end;
  end;
end;

procedure TCustomStyleObject.SetCapMode(const Value: TCapWrapMode);
begin
  FCapMode := Value;
end;

procedure TCustomStyleObject.SetWrapMode(const Value: TImageWrapMode);
begin
  FWrapMode := Value;
end;

{ TStyleObject }

constructor TStyleObject.Create(AOwner: TComponent);
begin
  inherited;
  FSourceLink := TBitmapLinks.Create;
end;

destructor TStyleObject.Destroy;
begin
  FSourceLink.Free;
  inherited;
end;

procedure TStyleObject.DefineProperties(Filer: TFiler);
begin
  inherited;
  { Backward compatibility with XE3 }
  Filer.DefineProperty('SourceRect.Rect', ReadSourceRect, nil, False);
  Filer.DefineProperty('SourceRect.RectI', ReadSourceRectI, nil, False);
  Filer.DefineProperty('SourceRect.LeftI', ReadSourceLeftI, nil, False);
  Filer.DefineProperty('SourceRect.RightI', ReadSourceRightI, nil, False);
  Filer.DefineProperty('SourceRect.TopI', ReadSourceTopI, nil, not False);
  Filer.DefineProperty('SourceRect.BottomI', ReadSourceBottomI, nil, False);

  Filer.DefineProperty('BitmapMargins.Rect', ReadBitmapMargin, nil, False);
  Filer.DefineProperty('BitmapMargins.RectI', ReadBitmapMarginI, nil, False);
  Filer.DefineProperty('BitmapMargins.LeftI', ReadBitmapMarginLeftI, nil, False);
  Filer.DefineProperty('BitmapMargins.RightI', ReadBitmapMarginRightI, nil, False);
  Filer.DefineProperty('BitmapMargins.TopI', ReadBitmapMarginTopI, nil, not False);
  Filer.DefineProperty('BitmapMargins.BottomI', ReadBitmapMarginBottomI, nil, False);

  Filer.DefineProperty('BitmapMargins15x.Rect', ReadBitmapMargin15x, nil, False);
  Filer.DefineProperty('SourceRect15x.Rect', ReadSourceRect15x, nil, False);
  Filer.DefineProperty('BitmapMargins20x.Rect', ReadBitmapMargin20x, nil, False);
  Filer.DefineProperty('SourceRect20x.Rect', ReadSourceRect20x, nil, False);
  Filer.DefineProperty('BitmapMargins30x.Rect', ReadBitmapMargin30x, nil, False);
  Filer.DefineProperty('SourceRect30x.Rect', ReadSourceRect30x, nil, False);
end;

procedure TStyleObject.ReadSourceRect(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FSourceLink, 1);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TStyleObject.ReadSourceRectI(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FSourceLink, 1);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TStyleObject.ReadSourceLeftI(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FSourceLink, 1);
  Link.SourceRect.Left := Reader.ReadInteger;
end;

procedure TStyleObject.ReadSourceRightI(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FSourceLink, 1);
  Link.SourceRect.Right := Reader.ReadInteger;
end;

procedure TStyleObject.ReadSourceTopI(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FSourceLink, 1);
  Link.SourceRect.Top := Reader.ReadInteger;
end;

procedure TStyleObject.ReadSourceBottomI(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FSourceLink, 1);
  Link.SourceRect.Bottom := Reader.ReadInteger;
end;

procedure TStyleObject.ReadBitmapMargin(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FSourceLink, 1);
  Link.CapInsets.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TStyleObject.ReadBitmapMarginI(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FSourceLink, 1);
  Link.CapInsets.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TStyleObject.ReadBitmapMarginLeftI(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FSourceLink, 1);
  Link.CapInsets.Left := Reader.ReadInteger;
end;

procedure TStyleObject.ReadBitmapMarginRightI(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FSourceLink, 1);
  Link.CapInsets.Right := Reader.ReadInteger;
end;

procedure TStyleObject.ReadBitmapMarginTopI(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FSourceLink, 1);
  Link.CapInsets.Top := Reader.ReadInteger;
end;

procedure TStyleObject.ReadBitmapMarginBottomI(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FSourceLink, 1);
  Link.CapInsets.Bottom := Reader.ReadInteger;
end;

procedure TStyleObject.ReadBitmapMargin15x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FSourceLink, 1.5);
  Link.CapInsets.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TStyleObject.ReadSourceRect15x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FSourceLink, 1.5);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TStyleObject.ReadBitmapMargin20x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FSourceLink, 2);
  Link.CapInsets.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TStyleObject.ReadSourceRect20x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FSourceLink, 2);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TStyleObject.ReadBitmapMargin30x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FSourceLink, 3);
  Link.CapInsets.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TStyleObject.ReadSourceRect30x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FSourceLink, 3);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

function TStyleObject.GetCurrentLink: TBitmapLinks;
begin
  Result := FSourceLink;
end;

procedure TStyleObject.SetSourceLink(const Value: TBitmapLinks);
begin
  FSourceLink.Assign(Value);
end;

{ TStyleAnimation }

type

  TStyleAnimation = class(TAnimation)
  protected
    procedure ProcessAnimation; override;
  public
    constructor Create(AOwner: TComponent); override;
    class function CreateAnimation(const Trigger: string; const Event: TNotifyEvent): TStyleAnimation;
  end;

constructor TStyleAnimation.Create(AOwner: TComponent);
begin
  inherited Create(nil);
  AutoReverse := True;
  Duration := 0.0001;
end;

class function TStyleAnimation.CreateAnimation(const Trigger: string;
  const Event: TNotifyEvent): TStyleAnimation;
begin
  Result := TStyleAnimation.Create(nil);
  Result.Trigger := Trigger;
  Result.OnFinish := Event;
end;

procedure TStyleAnimation.ProcessAnimation;
begin
end;

{ TActiveStyleObject }

function StyleTriggerToProperty(const Trigger: TStyleTrigger): string;
begin
  case Trigger of
    TStyleTrigger.stMouseOver: Result := 'IsMouseOver';
    TStyleTrigger.stPressed: Result := 'IsPressed';
    TStyleTrigger.stSelected: Result := 'IsSelected';
    TStyleTrigger.stFocused: Result := 'IsFocused';
    TStyleTrigger.stChecked: Result := 'IsChecked';
    TStyleTrigger.stActive: Result := 'IsActive';
  else
    Result := '';
  end;
end;

constructor TActiveStyleObject.Create(AOwner: TComponent);
begin
  inherited;
  FSourceLink := TBitmapLinks.Create;
  FActiveAnimation := TStyleAnimation.CreateAnimation('', Triggered);
  FActiveLink := TBitmapLinks.Create;
end;

destructor TActiveStyleObject.Destroy;
begin
  FSourceLink.Free;
  FActiveLink.Free;
  FActiveAnimation.Free;
  inherited;
end;

procedure TActiveStyleObject.Loaded;
var
  I: Integer;
begin
  inherited;
  if ActiveLink.Count = SourceLink.Count then
    for I := 0 to ActiveLink.Count - 1 do
      if not ActiveLink.Links[I].CapInsetsChanged then
        ActiveLink.Links[I].CapInsets := SourceLink.Links[I].CapInsets;
end;

procedure TActiveStyleObject.DefineProperties(Filer: TFiler);
begin
  inherited;
  { Backward compatibility with XE3 }
  Filer.DefineProperty('BitmapMargins.Rect', ReadBitmapMargin, nil, False);
  Filer.DefineProperty('SourceRect.Rect', ReadSourceRect, nil, False);
  Filer.DefineProperty('ActiveRect.Rect', ReadActiveRect, nil, False);
  Filer.DefineProperty('BitmapMargins15x.Rect', ReadBitmapMargin15x, nil, False);
  Filer.DefineProperty('SourceRect15x.Rect', ReadSourceRect15x, nil, False);
  Filer.DefineProperty('ActiveRect15x.Rect', ReadActiveRect15x, nil, False);
  Filer.DefineProperty('BitmapMargins20x.Rect', ReadBitmapMargin20x, nil, False);
  Filer.DefineProperty('SourceRect20x.Rect', ReadSourceRect20x, nil, False);
  Filer.DefineProperty('ActiveRect20x.Rect', ReadActiveRect20x, nil, False);
  Filer.DefineProperty('BitmapMargins30x.Rect', ReadBitmapMargin30x, nil, False);
  Filer.DefineProperty('SourceRect30x.Rect', ReadSourceRect30x, nil, False);
  Filer.DefineProperty('ActiveRect30x.Rect', ReadActiveRect30x, nil, False);
end;

procedure TActiveStyleObject.ReadBitmapMargin(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FSourceLink, 1);
  Link.CapInsets.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TActiveStyleObject.ReadSourceRect(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FSourceLink, 1);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TActiveStyleObject.ReadBitmapMargin15x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FSourceLink, 1.5);
  Link.CapInsets.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TActiveStyleObject.ReadSourceRect15x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FSourceLink, 1.5);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TActiveStyleObject.ReadBitmapMargin20x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FSourceLink, 2);
  Link.CapInsets.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TActiveStyleObject.ReadSourceRect20x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FSourceLink, 2);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TActiveStyleObject.ReadActiveRect(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(ActiveLink, 1);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TActiveStyleObject.ReadActiveRect15x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(ActiveLink, 1.5);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TActiveStyleObject.ReadActiveRect20x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(ActiveLink, 2);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TActiveStyleObject.ReadBitmapMargin30x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FSourceLink, 3);
  Link.CapInsets.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TActiveStyleObject.ReadSourceRect30x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FSourceLink, 3);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TActiveStyleObject.ReadActiveRect30x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(ActiveLink, 3);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

function TActiveStyleObject.GetCurrentLink: TBitmapLinks;
begin
  if FActive then
    Result := FActiveLink
  else
    Result := FSourceLink
end;

procedure TActiveStyleObject.SetActiveLink(const Value: TBitmapLinks);
begin
  FActiveLink.Assign(Value);
end;

procedure TActiveStyleObject.SetSourceLink(const Value: TBitmapLinks);
begin
  FSourceLink.Assign(Value);
end;

procedure TActiveStyleObject.SetTrigger(const Value: TStyleTrigger);
begin
  FTrigger := Value;
  SetupAnimations;
end;

procedure TActiveStyleObject.SetupAnimations;
begin
  FActiveAnimation.Trigger := StyleTriggerToProperty(FTrigger) + '=True';
  FActiveAnimation.TriggerInverse := StyleTriggerToProperty(FTrigger) + '=False';
end;

procedure TActiveStyleObject.StartTriggerAnimation(const AInstance: TFmxObject;
  const ATrigger: string);
begin
  inherited ;
  FActiveAnimation.StartTrigger(AInstance, ATrigger);
end;

procedure TActiveStyleObject.Triggered(Sender: TObject);
begin
  FActive := not FActiveAnimation.Inverse;
  Repaint;
end;

{ TTabStyleObject }

constructor TTabStyleObject.Create(AOwner: TComponent);
begin
  inherited;
  FHotLink := TBitmapLinks.Create;
  FHotAnimation := TStyleAnimation.CreateAnimation('', HotTriggered);
end;

destructor TTabStyleObject.Destroy;
begin
  FHotAnimation.Free;
  FHotLink.Free;
  inherited;
end;

function TTabStyleObject.GetCurrentLink: TBitmapLinks;
begin
  if FActive then
    Result := ActiveLink
  else
    if FHot then
      Result := HotLink
    else
      Result := SourceLink;
end;

procedure TTabStyleObject.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('HotRect.Rect', ReadHotRect, nil, False);
  Filer.DefineProperty('HotRect15x.Rect', ReadHotRect15x, nil, False);
  Filer.DefineProperty('HotRect20x.Rect', ReadHotRect20x, nil, False);
  Filer.DefineProperty('HotRect30x.Rect', ReadHotRect30x, nil, False);
end;

procedure TTabStyleObject.ReadHotRect(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(HotLink, 1);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TTabStyleObject.ReadHotRect15x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(HotLink, 1.5);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TTabStyleObject.ReadHotRect20x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(HotLink, 2);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TTabStyleObject.ReadHotRect30x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(HotLink, 3);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TTabStyleObject.HotTriggered(Sender: TObject);
begin
  FHot := not FHotAnimation.Inverse;
  Repaint;
end;

procedure TTabStyleObject.Loaded;
var
  I: Integer;
begin
  inherited;
  if HotLink.Count = SourceLink.Count then
    for I := 0 to HotLink.Count - 1 do
      if not HotLink.Links[I].CapInsetsChanged then
        HotLink.Links[I].CapInsets := SourceLink.Links[I].CapInsets;
end;

procedure TTabStyleObject.SetHotLink(const Value: TBitmapLinks);
begin
  FHotLink.Assign(Value);
end;

procedure TTabStyleObject.SetupAnimations;
begin
  FActiveAnimation.Trigger := StyleTriggerToProperty(FTrigger) + '=True';
  FActiveAnimation.TriggerInverse := StyleTriggerToProperty(FTrigger) + '=False';
  FHotAnimation.Trigger := 'IsMouseOver=True;' + FActiveAnimation.TriggerInverse;
  FHotAnimation.TriggerInverse := 'IsMouseOver=False;' + FActiveAnimation.TriggerInverse;
end;

procedure TTabStyleObject.StartTriggerAnimation(const AInstance: TFmxObject; const ATrigger: string);
begin
  inherited;
  FHotAnimation.StartTrigger(AInstance, ATrigger);
end;

{ TCheckStyleObject }

constructor TCheckStyleObject.Create(AOwner: TComponent);
begin
  inherited;
  FActiveHotLink := TBitmapLinks.Create;
end;

destructor TCheckStyleObject.Destroy;
begin
  FActiveHotLink.Free;
  inherited;
end;

procedure TCheckStyleObject.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('ActiveHotRect.Rect', ReadActiveHotRect, nil, False);
  Filer.DefineProperty('ActiveHotRect15x.Rect', ReadActiveHotRect15x, nil, False);
  Filer.DefineProperty('ActiveHotRect20x.Rect', ReadActiveHotRect20x, nil, False);
  Filer.DefineProperty('ActiveHotRect30x.Rect', ReadActiveHotRect30x, nil, False);
end;

procedure TCheckStyleObject.ReadActiveHotRect(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(ActiveHotLink, 1);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TCheckStyleObject.ReadActiveHotRect15x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(ActiveHotLink, 1.5);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TCheckStyleObject.ReadActiveHotRect20x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(ActiveHotLink, 2);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TCheckStyleObject.ReadActiveHotRect30x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(ActiveHotLink, 3);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

function TCheckStyleObject.GetCurrentLink: TBitmapLinks;
begin
  if FActive then
  begin
    if FHot then
      Result := ActiveHotLink
    else
      Result := ActiveLink
  end else
  begin
    if FHot then
      Result := HotLink
    else
      Result := SourceLink;
  end;
end;

procedure TCheckStyleObject.Loaded;
var
  I: Integer;
begin
  inherited;
  if ActiveHotLink.Count = SourceLink.Count then
    for I := 0 to ActiveHotLink.Count - 1 do
      if not ActiveHotLink.Links[I].CapInsetsChanged then
        ActiveHotLink.Links[I].CapInsets := SourceLink.Links[I].CapInsets;
end;

procedure TCheckStyleObject.SetActiveHotLink(const Value: TBitmapLinks);
begin
  FActiveHotLink.Assign(Value);
end;

procedure TCheckStyleObject.SetupAnimations;
begin
  FActiveAnimation.Trigger := StyleTriggerToProperty(FTrigger) + '=True';
  FActiveAnimation.TriggerInverse := StyleTriggerToProperty(FTrigger) + '=False';
  FHotAnimation.Trigger := 'IsMouseOver=True';
  FHotAnimation.TriggerInverse := 'IsMouseOver=False';
end;

{ TButtonStyleObject }

constructor TButtonStyleObject.Create(AOwner: TComponent);
begin
  inherited;
  FCurrent := TButtonAnimation.Normal;

  FTriggerSources[TButtonAnimation.Normal].Animation :=
    TStyleAnimation.CreateAnimation('IsMouseOver=False;IsPressed=False;IsFocused=False', NormalTriggered);
  FTriggerSources[TButtonAnimation.Normal].Link := TBitmapLinks.Create;

  FTriggerSources[TButtonAnimation.Hot].Animation :=
    TStyleAnimation.CreateAnimation('IsMouseOver=True;IsPressed=False', HotTriggered);
  FTriggerSources[TButtonAnimation.Hot].Link := TBitmapLinks.Create;

  FTriggerSources[TButtonAnimation.Focused].Animation :=
    TStyleAnimation.CreateAnimation('IsMouseOver=False;IsFocused=True;IsPressed=False', FocusedTriggered);
  FTriggerSources[TButtonAnimation.Focused].Link := TBitmapLinks.Create;

  FTriggerSources[TButtonAnimation.Pressed].Animation :=
    TStyleAnimation.CreateAnimation('IsMouseOver=True;IsPressed=True', PressedTriggered);
  FTriggerSources[TButtonAnimation.Pressed].Link := TBitmapLinks.Create;
end;

destructor TButtonStyleObject.Destroy;
var
  I: TButtonAnimation;
begin
  for I := Low(FTriggerSources) to High(FTriggerSources) do
  begin
    FTriggerSources[I].Animation.Free;
    FTriggerSources[I].Link.Free;
  end;
  inherited;
end;

procedure TButtonStyleObject.DefineProperties(Filer: TFiler);
begin
  inherited;
  { Backward compatibility with XE3 }
  Filer.DefineProperty('BitmapMargins.Rect', ReadBitmapMargin, nil, False);
  Filer.DefineProperty('SourceRect.Rect', ReadSourceRect, nil, False);
  Filer.DefineProperty('HotRect.Rect', ReadHotRect, nil, False);
  Filer.DefineProperty('PressedRect.Rect', ReadPressedRect, nil, False);
  Filer.DefineProperty('FocusedRect.Rect', ReadFocusedRect, nil, False);

  Filer.DefineProperty('BitmapMargins15x.Rect', ReadBitmapMargin15x, nil, False);
  Filer.DefineProperty('SourceRect15x.Rect', ReadSourceRect15x, nil, False);
  Filer.DefineProperty('HotRect15x.Rect', ReadHotRect15x, nil, False);
  Filer.DefineProperty('PressedRect15x.Rect', ReadPressedRect15x, nil, False);
  Filer.DefineProperty('FocusedRect15x.Rect', ReadFocusedRect15x, nil, False);

  Filer.DefineProperty('BitmapMargins20x.Rect', ReadBitmapMargin20x, nil, False);
  Filer.DefineProperty('SourceRect20x.Rect', ReadSourceRect20x, nil, False);
  Filer.DefineProperty('HotRect20x.Rect', ReadHotRect20x, nil, False);
  Filer.DefineProperty('PressedRect20x.Rect', ReadPressedRect20x, nil, False);
  Filer.DefineProperty('FocusedRect20x.Rect', ReadFocusedRect20x, nil, False);

  Filer.DefineProperty('BitmapMargins30x.Rect', ReadBitmapMargin30x, nil, False);
  Filer.DefineProperty('SourceRect30x.Rect', ReadSourceRect30x, nil, False);
  Filer.DefineProperty('HotRect30x.Rect', ReadHotRect30x, nil, False);
  Filer.DefineProperty('PressedRect30x.Rect', ReadPressedRect30x, nil, False);
  Filer.DefineProperty('FocusedRect30x.Rect', ReadFocusedRect30x, nil, False);
end;

procedure TButtonStyleObject.ReadBitmapMargin(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FTriggerSources[TButtonAnimation.Normal].Link, 1);
  Link.CapInsets.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TButtonStyleObject.ReadSourceRect(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FTriggerSources[TButtonAnimation.Normal].Link, 1);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TButtonStyleObject.ReadHotRect(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FTriggerSources[TButtonAnimation.Hot].Link, 1);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TButtonStyleObject.ReadPressedRect(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FTriggerSources[TButtonAnimation.Pressed].Link, 1);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TButtonStyleObject.ReadFocusedRect(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FTriggerSources[TButtonAnimation.Focused].Link, 1);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TButtonStyleObject.ReadBitmapMargin15x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FTriggerSources[TButtonAnimation.Normal].Link, 1.5);
  Link.CapInsets.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TButtonStyleObject.ReadSourceRect15x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FTriggerSources[TButtonAnimation.Normal].Link, 1.5);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TButtonStyleObject.ReadHotRect15x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FTriggerSources[TButtonAnimation.Hot].Link, 1.5);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TButtonStyleObject.ReadPressedRect15x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FTriggerSources[TButtonAnimation.Pressed].Link, 1.5);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TButtonStyleObject.ReadFocusedRect15x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FTriggerSources[TButtonAnimation.Focused].Link, 1.5);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TButtonStyleObject.ReadBitmapMargin20x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FTriggerSources[TButtonAnimation.Normal].Link, 2);
  Link.CapInsets.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TButtonStyleObject.ReadSourceRect20x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FTriggerSources[TButtonAnimation.Normal].Link, 2);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TButtonStyleObject.ReadHotRect20x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FTriggerSources[TButtonAnimation.Hot].Link, 2);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TButtonStyleObject.ReadPressedRect20x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FTriggerSources[TButtonAnimation.Pressed].Link, 2);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TButtonStyleObject.ReadFocusedRect20x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FTriggerSources[TButtonAnimation.Focused].Link, 2);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TButtonStyleObject.ReadBitmapMargin30x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FTriggerSources[TButtonAnimation.Normal].Link, 3);
  Link.CapInsets.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TButtonStyleObject.ReadSourceRect30x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FTriggerSources[TButtonAnimation.Normal].Link, 3);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TButtonStyleObject.ReadHotRect30x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FTriggerSources[TButtonAnimation.Hot].Link, 3);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TButtonStyleObject.ReadPressedRect30x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FTriggerSources[TButtonAnimation.Pressed].Link, 3);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TButtonStyleObject.ReadFocusedRect30x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(FTriggerSources[TButtonAnimation.Focused].Link, 3);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

function TButtonStyleObject.GetCurrentLink: TBitmapLinks;
begin
  Result := FTriggerSources[FCurrent].Link;
end;

procedure TButtonStyleObject.FocusedTriggered(Sender: TObject);
begin
  FCurrent := TButtonAnimation.Focused;
  Repaint;
end;

procedure TButtonStyleObject.NormalTriggered(Sender: TObject);
begin
  FCurrent := TButtonAnimation.Normal;
  Repaint;
end;

procedure TButtonStyleObject.PressedTriggered(Sender: TObject);
begin
  FCurrent := TButtonAnimation.Pressed;
  Repaint;
end;

procedure TButtonStyleObject.HotTriggered(Sender: TObject);
begin
  FCurrent := TButtonAnimation.Hot;
  Repaint;
end;

procedure TButtonStyleObject.Loaded;
var
  I: TButtonAnimation;
  NormalLink, Link: TBitmapLinks;
  J: Integer;
begin
  inherited;
  NormalLink := FTriggerSources[TButtonAnimation.Normal].Link;
  for I := TButtonAnimation.Hot to High(FTriggerSources) do
  begin
    Link := FTriggerSources[I].Link;
    if Link.Count = NormalLink.Count then
      for J := 0 to Link.Count - 1 do
        if not Link.Links[J].CapInsetsChanged then
          Link.Links[J].CapInsets := NormalLink.Links[J].CapInsets;
  end;
end;

procedure TButtonStyleObject.StartTriggerAnimation(const AInstance: TFmxObject;
  const ATrigger: string);
var
  I: TButtonAnimation;
begin
  inherited ;
  for I := Low(FTriggerSources) to High(FTriggerSources) do
    FTriggerSources[I].Animation.StartTrigger(AInstance, ATrigger);
end;

function TButtonStyleObject.GetLink(Index: TButtonAnimation): TBitmapLinks;
begin
  Result := FTriggerSources[Index].Link;
end;

procedure TButtonStyleObject.SetLink(Index: TButtonAnimation;
  const Value: TBitmapLinks);
begin
  FTriggerSources[Index].Link.Assign(Value);
end;

{ TSystemButtonObject }

constructor TSystemButtonObject.Create(AOwner: TComponent);
begin
  inherited;
  FInactiveAnimation := TStyleAnimation.CreateAnimation('', InactiveTriggered);
  FInactiveAnimation.Trigger := 'IsActive=False';
  FInactiveAnimation.TriggerInverse := 'IsActive=True';
  FInactiveLink := TBitmapLinks.Create;
end;

destructor TSystemButtonObject.Destroy;
begin
  FInactiveLink.Free;
  inherited;
end;

procedure TSystemButtonObject.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('InactiveRect.Rect', ReadInactiveRect, nil, False);
  Filer.DefineProperty('InactiveRect15x.Rect', ReadInactiveRect15x, nil, False);
  Filer.DefineProperty('InactiveRect20x.Rect', ReadInactiveRect20x, nil, False);
  Filer.DefineProperty('InactiveRect30x.Rect', ReadInactiveRect30x, nil, False);
end;

procedure TSystemButtonObject.ReadInactiveRect(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(InactiveLink, 1);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TSystemButtonObject.ReadInactiveRect15x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(InactiveLink, 1.5);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TSystemButtonObject.ReadInactiveRect20x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(InactiveLink, 2);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

procedure TSystemButtonObject.ReadInactiveRect30x(Reader: TReader);
var
  Link: TBitmapLink;
begin
  Link := FindOrCreateBitmapLink(InactiveLink, 3);
  Link.SourceRect.Rect := TRectF.Create(StringToRect(Reader.ReadString));
end;

function TSystemButtonObject.GetCurrentLink: TBitmapLinks;
begin
  if FInactive then
    Result := FInactiveLink
  else
    Result := inherited ;
end;

procedure TSystemButtonObject.InactiveTriggered(Sender: TObject);
begin
  FInactive := not FInactiveAnimation.Inverse;
  Repaint;
end;

procedure TSystemButtonObject.SetInactiveLink(const Value: TBitmapLinks);
begin
  FInactiveLink.Assign(Value);
end;

{ TStyleShadow }

constructor TStyleShadow.Create;
begin
  FOffset := TPosition.Create(TPointF.Create(0, 0));
  FOffset.StoreAsInt := True;
  FOffset.OnChange := OffsetChanged;
end;

destructor TStyleShadow.Destroy;
begin
  FOffset.Free;
  inherited;
end;

procedure TStyleShadow.Assign(Source: TPersistent);
var
  SStyleShadow: TStyleShadow;
begin
  if Source is TStyleShadow then
  begin
    SStyleShadow := TStyleShadow(Source);
    if (Color <> SStyleShadow.FColor) or (FOffset <> SStyleShadow.FOffset) then
    begin
      FColor := SStyleShadow.FColor;
      FOffset.Assign(SStyleShadow.FOffset);
      DoChanged;
    end;
  end else
    inherited
end;

procedure TStyleShadow.DoChanged;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TStyleShadow.OffsetChanged(Sender: TObject);
begin
  DoChanged;
end;

procedure TStyleShadow.SetColor(const Value: TAlphaColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    DoChanged;
  end;
end;

procedure TStyleShadow.SetOffset(const Value: TPosition);
begin
  FOffset.Assign(Value);
end;

{ TStyleTextObject }

constructor TStyleTextObject.Create(AOwner: TComponent);
begin
  inherited;
  HitTest := False;
  FShadow := TStyleShadow.Create;
  FShadow.OnChanged := ShadowChanged;
  FSavedShadow := TStyleShadow.Create;
end;

destructor TStyleTextObject.Destroy;
begin
  FShadow.Free;
  FSavedShadow.Free;
  inherited;
end;

procedure TStyleTextObject.DrawToCanvas(const Canvas: TCanvas;
  const ARect: TRectF; const AOpacity: Single);
begin
  if FShadowVisible then
  begin
    Layout.BeginUpdate;
    try
      Layout.Text := GetCurrentText;
      Layout.LayoutCanvas := Canvas;
      Layout.TopLeft := ARect.TopLeft + FShadow.Offset.Point;
      Layout.Opacity := AOpacity;
      Layout.MaxSize := PointF(ARect.Width, ARect.Height);
      Layout.Color := FShadow.Color;
    finally
      Layout.EndUpdate;
    end;
    Layout.RenderLayout(Canvas);
  end;

  Layout.BeginUpdate;
  try
    Layout.Text := GetCurrentText;
    Layout.TopLeft := ARect.TopLeft;
    Layout.Opacity := AOpacity;
    Layout.MaxSize := PointF(ARect.Width, ARect.Height);
    Layout.Color := GetCurrentColor;
  finally
    Layout.EndUpdate;
  end;
  Layout.RenderLayout(Canvas);
end;

function TStyleTextObject.GetCurrentColor: TAlphaColor;
begin
  Result := Color;
end;

function TStyleTextObject.GetCurrentText: string;
begin
  Result := Text;
end;

procedure TStyleTextObject.Loaded;
begin
  inherited;
  FSavedShadow.Assign(FShadow);
  FSavedColor := Color;
end;

procedure TStyleTextObject.Paint;
begin
  DrawToCanvas(Canvas, LocalRect, AbsoluteOpacity);
end;

function TStyleTextObject.ConvertText(const Value: string): string;
begin
  case CharCase of
    TEditCharCase.ecUpperCase: Result := Value.ToUpper;
    TEditCharCase.ecLowerCase: Result := Value.ToLower;
  else
    Result := inherited ConvertText(Value);
  end;
end;

procedure TStyleTextObject.SetCharCase(const Value: TEditCharCase);
begin
  FCharCase := Value;
end;

procedure TStyleTextObject.SetShadow(const Value: TStyleShadow);
begin
  FShadow.Assign(Value);
end;

procedure TStyleTextObject.SetShadowVisible(const Value: Boolean);
begin
  FShadowVisible := Value;
end;

procedure TStyleTextObject.ShadowChanged;
begin
  Repaint;
end;

{ TStyleTextAnimation }

constructor TStyleTextAnimation.Create(AOwner: TComponent);
begin
  inherited;
  FShadow := TStyleShadow.Create;
  FColor := claNull;
  Duration := 0.0001;
end;

class function TStyleTextAnimation.CreateAnimation(const Trigger: string; const Event: TNotifyEvent): TStyleTextAnimation;
begin
  Result := TStyleTextAnimation.Create(nil);
  Result.Trigger := Trigger;
  Result.OnFinish := Event;
end;

destructor TStyleTextAnimation.Destroy;
begin
  FShadow.Free;
  inherited;
end;

procedure TStyleTextAnimation.ProcessAnimation;
begin
end;

procedure TStyleTextAnimation.SetShadow(const Value: TStyleShadow);
begin
  FShadow.Assign(Value);
end;

{ TActiveStyleTextObject }

constructor TActiveStyleTextObject.Create(AOwner: TComponent);
begin
  inherited;
  FAnimation := TStyleTextAnimation.CreateAnimation('', Triggered);
end;

destructor TActiveStyleTextObject.Destroy;
begin
  FAnimation.Free;
  inherited;
end;

function TActiveStyleTextObject.GetActiveColor: TAlphaColor;
begin
  Result := FAnimation.Color;
end;

function TActiveStyleTextObject.GetActiveShadow: TStyleShadow;
begin
  Result := FAnimation.Shadow;
end;

procedure TActiveStyleTextObject.SetActiveColor(const Value: TAlphaColor);
begin
  FAnimation.Color := Value;
end;

procedure TActiveStyleTextObject.SetActiveShadow(const Value: TStyleShadow);
begin
  FAnimation.Shadow.Assign(Value);
end;

procedure TActiveStyleTextObject.SetTrigger(const Value: TStyleTrigger);
begin
  FTrigger := Value;
  SetupAnimations;
end;

procedure TActiveStyleTextObject.SetupAnimations;
begin
  FAnimation.Trigger := StyleTriggerToProperty(FTrigger) + '=True';
  FAnimation.TriggerInverse := StyleTriggerToProperty(FTrigger) + '=False';
end;

procedure TActiveStyleTextObject.StartTriggerAnimation(const AInstance: TFmxObject; const ATrigger: string);
begin
  inherited ;
  FAnimation.StartTrigger(AInstance, ATrigger);
end;

procedure TActiveStyleTextObject.Triggered(Sender: TObject);
begin
  if FAnimation.Inverse then
  begin
    Color := SavedColor;
    Shadow := SavedShadow;
  end
  else begin
    Color := ActiveColor;
    Shadow := ActiveShadow;
  end;
end;

{ TTabStyleTextObject }

constructor TTabStyleTextObject.Create(AOwner: TComponent);
begin
  inherited;
  FHotAnimation := TStyleTextAnimation.CreateAnimation('', HotTriggered);
end;

destructor TTabStyleTextObject.Destroy;
begin
  FHotAnimation.Free;
  inherited;
end;

function TTabStyleTextObject.GetHotColor: TAlphaColor;
begin
  Result := FHotAnimation.Color;
end;

function TTabStyleTextObject.GetHotShadow: TStyleShadow;
begin
  Result := FHotAnimation.Shadow;
end;

procedure TTabStyleTextObject.HotTriggered(Sender: TObject);
begin
  if FHotAnimation.Inverse then
  begin
    Color := SavedColor;
    Shadow := SavedShadow;
  end else
  begin
    Color := HotColor;
    Shadow := HotShadow;
  end;
end;

procedure TTabStyleTextObject.Loaded;
begin
  inherited;
  if FHotAnimation.Color = claNull then
    FHotAnimation.Color := Color;
  if FHotAnimation.Shadow.Color = claNull then
    FHotAnimation.Shadow.Color := Shadow.Color;
  if FHotAnimation.Shadow.Offset.Empty then
    FHotAnimation.Shadow.Offset := Shadow.Offset;
end;

procedure TTabStyleTextObject.SetHotColor(const Value: TAlphaColor);
begin
  FHotAnimation.Color := Value;
end;

procedure TTabStyleTextObject.SetHotShadow(const Value: TStyleShadow);
begin
  FHotAnimation.Shadow.Assign(Value);
end;

procedure TTabStyleTextObject.SetupAnimations;
begin
  FAnimation.Trigger := StyleTriggerToProperty(FTrigger) + '=True';
  FAnimation.TriggerInverse := StyleTriggerToProperty(FTrigger) + '=False';
  FHotAnimation.Trigger := 'IsMouseOver=True;' + StyleTriggerToProperty(FTrigger) + '=False';
  FHotAnimation.TriggerInverse := 'IsMouseOver=False;' + StyleTriggerToProperty(FTrigger) + '=False';
end;

procedure TTabStyleTextObject.StartTriggerAnimation(const AInstance: TFmxObject; const ATrigger: string);
begin
  inherited;
  FHotAnimation.StartTrigger(AInstance, ATrigger);
end;

{ TButtonStyleTextObject }

constructor TButtonStyleTextObject.Create(AOwner: TComponent);
begin
  inherited;
  FTriggerSources[TButtonAnimation.Normal] :=
    TStyleTextAnimation.CreateAnimation('IsMouseOver=False;IsPressed=False;IsFocused=False', NormalTriggered);

  FTriggerSources[TButtonAnimation.Hot] :=
    TStyleTextAnimation.CreateAnimation('IsMouseOver=True;IsPressed=False', HotTriggered);

  FTriggerSources[TButtonAnimation.Focused] :=
    TStyleTextAnimation.CreateAnimation('IsMouseOver=False;IsFocused=True;IsPressed=False', FocusedTriggered);

  FTriggerSources[TButtonAnimation.Pressed] :=
    TStyleTextAnimation.CreateAnimation('IsMouseOver=True;IsPressed=True', PressedTriggered);
end;

destructor TButtonStyleTextObject.Destroy;
var
  I: TButtonAnimation;
begin
  for I := Low(FTriggerSources) to High(FTriggerSources) do
    if Assigned(FTriggerSources[I]) then
      FTriggerSources[I].Free;
  inherited;
end;

procedure TButtonStyleTextObject.FocusedTriggered(Sender: TObject);
begin
  Color := FocusedColor;
  Shadow := FocusedShadow;
end;

procedure TButtonStyleTextObject.NormalTriggered(Sender: TObject);
begin
  Color := NormalColor;
  Shadow := NormalShadow;
end;

procedure TButtonStyleTextObject.PressedTriggered(Sender: TObject);
begin
  Color := PressedColor;
  Shadow := PressedShadow;
end;

procedure TButtonStyleTextObject.HotTriggered(Sender: TObject);
begin
  Color := HotColor;
  Shadow := HotShadow;
end;

procedure TButtonStyleTextObject.Loaded;
var
  I: TButtonAnimation;
begin
  inherited;
  FTriggerSources[TButtonAnimation.Normal].Color := Color;
  FTriggerSources[TButtonAnimation.Normal].Shadow := Shadow;
  for I := TButtonAnimation.Hot to High(FTriggerSources) do
  begin
    if FTriggerSources[I].Color = claNull then
      FTriggerSources[I].Color := Color;
    if FTriggerSources[I].Shadow.Color = claNull then
      FTriggerSources[I].Shadow.Color := Shadow.Color;
    if FTriggerSources[I].Shadow.Offset.Empty then
      FTriggerSources[I].Shadow.Offset := Shadow.Offset;
  end;
end;

procedure TButtonStyleTextObject.StartTriggerAnimation(const AInstance: TFmxObject;
  const ATrigger: string);
var
  I: TButtonAnimation;
begin
  inherited ;
  for I := Low(FTriggerSources) to High(FTriggerSources) do
    if Assigned(FTriggerSources[I]) then
      FTriggerSources[I].StartTrigger(AInstance, ATrigger);
end;

function TButtonStyleTextObject.GetColor(Index: TButtonAnimation): TAlphaColor;
begin
  Result := FTriggerSources[Index].Color;
end;

function TButtonStyleTextObject.GetShadow(Index: TButtonAnimation): TStyleShadow;
begin
  Result := FTriggerSources[Index].Shadow;
end;

procedure TButtonStyleTextObject.SetColor(Index: TButtonAnimation; const Value: TAlphaColor);
begin
  FTriggerSources[Index].Color := Value;
end;

procedure TButtonStyleTextObject.SetShadow(Index: TButtonAnimation;
  const Value: TStyleShadow);
begin
  FTriggerSources[Index].Shadow := Value;
end;

{ TActiveOpacityObject }

constructor TActiveOpacityObject.Create(AOwner: TComponent);
begin
  inherited;
  HitTest := False;
  FActiveOpacity := 1.0;
  FActiveAnimation := TStyleAnimation.CreateAnimation('', Triggered);
  SetupAnimation;
end;

destructor TActiveOpacityObject.Destroy;
begin
  FActiveAnimation.Free;
  inherited;
end;

function TActiveOpacityObject.IsActiveOpacityStored: Boolean;
begin
  Result := not SameValue(FActiveOpacity, 1.0);
end;

procedure TActiveOpacityObject.Loaded;
begin
  inherited;
  FSavedOpacity := Opacity;
end;

procedure TActiveOpacityObject.SetTrigger(const Value: TStyleTrigger);
begin
  if FTrigger <> Value then
  begin
    FTrigger := Value;
    SetupAnimation;
  end;
end;

procedure TActiveOpacityObject.SetupAnimation;
begin
  FActiveAnimation.Trigger := StyleTriggerToProperty(FTrigger) + '=True';
  FActiveAnimation.TriggerInverse := StyleTriggerToProperty(FTrigger) + '=False';
end;

procedure TActiveOpacityObject.StartTriggerAnimation(const AInstance: TFmxObject; const ATrigger: string);
begin
  inherited;
  FActiveAnimation.StartTrigger(AInstance, ATrigger);
end;

procedure TActiveOpacityObject.Triggered(Sender: TObject);
begin
  if FActiveAnimation.Inverse then
    Opacity := FSavedOpacity
  else
    Opacity := FActiveOpacity;
end;

initialization
  RegisterFmxClasses([
    TSubImage,
    TStyleObject, TActiveStyleObject, TTabStyleObject, TCheckStyleObject, TButtonStyleObject, TSystemButtonObject,
    TStyleTextObject, TActiveStyleTextObject, TTabStyleTextObject, TButtonStyleTextObject, TActiveOpacityObject
  ]);
  TCustomStyleObject.AlignToPixels := True;
end.

