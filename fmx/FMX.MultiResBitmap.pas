{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit FMX.MultiResBitmap;

interface

uses
  System.SysUtils, System.Generics.Collections, System.Generics.Defaults,
  System.Types, System.UITypes, System.Rtti, System.Classes,
  FMX.Consts, FMX.Graphics;

type
  TCustomMultiResBitmap = class;
  TCustomBitmapItem = class;

  EMultiResBitmap = class(EComponentError)
  end;

  TBitmapOfItem = class(TBitmap)
  private
    [Weak] FBitmapItem: TCustomBitmapItem;
    FIsChanged: Boolean;
  protected
    procedure DoChange; override;
  public
    property BitmapItem: TCustomBitmapItem read FBitmapItem;
    property IsChanged: Boolean read FIsChanged write FIsChanged;
  end;

  TCustomBitmapItem = class(TCollectionItem)
  public const
    ScaleRange = -3;
    ScaleMax = 100;
    ScaleDefault = 1;
  private
    FFixed: Boolean;
    FDormant: Boolean;
    FBitmap: TBitmapOfItem;
    FStream: TMemoryStream;
    [Weak] FMultiResBitmap: TCustomMultiResBitmap;
    FScale: Single;
    FFileName: string;
    FWidth: Word;
    FHeight: Word;
    function GetFixed: Boolean;
    procedure SetScale(const Value: Single);
    procedure SetBitmap(const Value: TBitmapOfItem);
    function GetBitmap: TBitmapOfItem;
    procedure ReadFileName(Reader: TReader);
    procedure WriteFileName(Writer: TWriter);
    function GetComponent: TComponent;
    procedure SetDormant(const Value: Boolean);
    procedure ReadBitmap(Stream: TStream);
    procedure WriteBitmap(Stream: TStream);
    function GetIsEmpty: Boolean;
    procedure ReadHeight(Reader: TReader);
    procedure ReadWidth(Reader: TReader);
    procedure WriteHeight(Writer: TWriter);
    procedure WriteWidth(Writer: TWriter);
  protected
    procedure SetCollection(Value: TCollection); override;
    procedure DefineProperties(Filer: TFiler); override;
    function ScaleStored: Boolean; virtual;
    procedure SetFixed(const Value: Boolean);
    function GetDisplayName: string; override;
    procedure SetIndex(Value: Integer); override;
    function BitmapStored: Boolean; virtual;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    property MultiResBitmap: TCustomMultiResBitmap read FMultiResBitmap;
    property Fixed: Boolean read GetFixed;
    property Component: TComponent read GetComponent;
    class function ScaleOfBitmap(const SourceSize, DestinationSize: TSize): Double;
    class function RectOfBitmap(const SourceSize, DestinationSize: TSize): TRect;
    function CreateBitmap(const AFileName: string = ''): TBitmap; virtual;
    property Scale: Single read FScale write SetScale stored ScaleStored nodefault;
    property Bitmap: TBitmapOfItem read GetBitmap write SetBitmap stored False;
    property FileName: string read FFileName write FFileName stored False;
    property Dormant: Boolean read FDormant write SetDormant;
    property IsEmpty: Boolean read GetIsEmpty;
    property Width: Word read FWidth stored False;
    property Height: Word read FHeight stored False;
  end;

  TCustomBitmapItemClass = class of TCustomBitmapItem;
  TFixedBitmapItemClass = class of TFixedBitmapItem;

  TSizeKind = (skCustom, skDefault, skSource);

  TCustomMultiResBitmap = class(TOwnedCollection)
  private
    FFixed: Boolean;
    FWidth: Word;
    FHeight: Word;
    FSizeKind: TSizeKind;
    FTransparentColor: TColor;
    function GetFixed: Boolean;
    procedure ReadWidth(Reader: TReader);
    procedure WriteWidth(Writer: TWriter);
    procedure ReadHeight(Reader: TReader);
    procedure WriteHeight(Writer: TWriter);
    function GetItem(Index: Integer): TCustomBitmapItem;
    procedure SetItem(Index: Integer; const Value: TCustomBitmapItem);
    function GetComponent: TComponent;
    procedure ReadLoadSize(Reader: TReader);
    procedure WriteLoadSize(Writer: TWriter);
    procedure ReadColor(Reader: TReader);
    procedure WriteColor(Writer: TWriter);
    function GetBitmaps(Scale: Single): TBitmapOfItem;
    procedure SetBitmaps(Scale: Single; const Value: TBitmapOfItem);
  protected
    procedure SetFixed(const Value: Boolean);
    function GetDefaultSize: TSize; virtual;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCustomBitmapItemClass);
    function ScaleArray(IncludeEmpty: Boolean): TArray<Single>;
    function ItemByScale(const AScale: Single; const ExactMatch: Boolean; const IncludeEmpty: Boolean)
      : TCustomBitmapItem;
    procedure LoadItemFromStream(Stream: TStream; Scale: Single);
    procedure LoadFromStream(S: TStream); virtual;
    procedure SaveToStream(S: TStream); virtual;
    procedure Assign(Source: TPersistent); override;
    function Add: TCustomBitmapItem;
    function Insert(Index: Integer): TCustomBitmapItem;
    property Items[Index: Integer]: TCustomBitmapItem read GetItem write SetItem; default;
    property Bitmaps[Scale: Single]: TBitmapOfItem read GetBitmaps write SetBitmaps;
    property Component: TComponent read GetComponent;
    property Fixed: Boolean read GetFixed;
    property DefaultSize: TSize read GetDefaultSize;
    property SizeKind: TSizeKind read FSizeKind write FSizeKind stored False;
    property Width: Word read FWidth write FWidth stored False;
    property Height: Word read FHeight write FHeight stored False;
    property TransparentColor: TColor read FTransparentColor write FTransparentColor stored False;
  end;

  TFixedBitmapItem = class(TCustomBitmapItem)
  private
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
  published
    property Scale;
    property Bitmap;
  end;

  TFixedMultiResBitmap = class(TCustomMultiResBitmap)
  private
    procedure CreateItem(Scale: Single);
    procedure CreateItems;
    function GetItem(Index: Integer): TFixedBitmapItem;
    procedure SetItem(Index: Integer; const Value: TFixedBitmapItem);
    procedure UpdateFixed;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TFixedBitmapItemClass); overload;
    constructor Create(AOwner: TPersistent); overload;
    procedure EndUpdate; override;
    function Add: TFixedBitmapItem;
    function Insert(Index: Integer): TFixedBitmapItem;
    property Items[Index: Integer]: TFixedBitmapItem read GetItem write SetItem; default;
  end;

  TScaleName = record
    Scale: Single;
    Name: string;
  end;

  TScaleList = TList<TScaleName>;

  TScaleNameComparer = class(TInterfacedObject, IComparer<TScaleName>)
    function Compare(const Left, Right: TScaleName): Integer;
  end;

function ScaleList: TScaleList;
function RegisterScaleName(Scale: Single; Name: string): Boolean;
procedure CheckInheritsClass(const AValue: TPersistent; const AClass: TClass; const CanBeNil: Boolean = True);

implementation

uses
  System.RTLConsts, System.Math;

var
  StrScaleFormat: string;
  StrScaleName: string;


procedure CheckInheritsClass(const AValue: TPersistent; const AClass: TClass; const CanBeNil: Boolean = True);
begin
  if Assigned(AClass) and (CanBeNil or Assigned(AValue)) then
  begin
    if Assigned(AValue) and (not(AValue.InheritsFrom(AClass))) then
      raise EArgumentException.CreateFMT(SEUseHeirs, [AClass.ClassName])at ReturnAddress;
  end
  else
    raise EArgumentException.Create(SArgumentNil)at ReturnAddress;
end;

{$REGION 'implementation of TBitmapOfItem'}
{ TBitmapOfItem }

procedure TBitmapOfItem.DoChange;
begin
  inherited;
  FIsChanged := True;
  if Assigned(FBitmapItem) then
  begin
    FBitmapItem.FWidth := Round(Width);
    FBitmapItem.FHeight := Round(Height);
    if (FBitmapItem.Component <> nil) and ([csLoading, csDestroying] * FBitmapItem.Component.ComponentState = []) and
      (FBitmapItem.Collection is TCustomMultiResBitmap) and
      (TCustomMultiResBitmap(FBitmapItem.Collection).UpdateCount = 0) then
      FBitmapItem.Changed(False);
  end;
end;

{$ENDREGION}
{$REGION 'MultiResBitmap'}
{ TCustomBitmapItem }

constructor TCustomBitmapItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  if FScale <= 0 then
    FScale := ScaleDefault;
  FStream := TMemoryStream.Create;
  FDormant := True;
end;

procedure TCustomBitmapItem.SetDormant(const Value: Boolean);
begin
  if FDormant <> Value then
  begin
    if Value then
    begin
      try
        if Assigned(FStream) then
          FStream.Clear
        else
          FStream := TMemoryStream.Create;
        if Assigned(FBitmap) and (not FBitmap.IsEmpty) then
          FBitmap.SaveToStream(FStream);
      except
        FreeAndNil(FStream);
        raise;
      end;
      FreeAndNil(FBitmap);
    end
    else
    begin
      try
        if not Assigned(FBitmap) then
          FBitmap := TBitmapOfItem.Create(0, 0);
        FBitmap.FBitmapItem := self;
        IBitmapAccess(FBitmap).BitmapScale := FScale;
        if Assigned(FStream) then
        begin
          FStream.Position := 0;
          FBitmap.LoadFromStream(FStream);
        end
        else
          FBitmap.SetSize(0, 0);
      except
        FreeAndNil(FBitmap);
        raise;
      end;
      FreeAndNil(FStream);
    end;
    FDormant := Value;
  end;
end;

procedure TCustomBitmapItem.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Width', ReadWidth, WriteWidth, BitmapStored);
  Filer.DefineProperty('Height', ReadHeight, WriteHeight, BitmapStored);
  Filer.DefineBinaryProperty('PNG', ReadBitmap, WriteBitmap, BitmapStored);
  Filer.DefineProperty('FileName', ReadFileName, WriteFileName, FileName <> '');
end;

destructor TCustomBitmapItem.Destroy;
begin
  if Assigned(MultiResBitmap) and MultiResBitmap.Fixed then
    raise EMultiResBitmap.CreateFMT(StrEChangeFixed, ['MultiResBitmap']);
  FreeAndNil(FBitmap);
  FreeAndNil(FStream);
  inherited;
end;

function TCustomBitmapItem.ScaleStored: Boolean;
begin
  Result := FScale <> ScaleDefault;
end;

procedure TCustomBitmapItem.Assign(Source: TPersistent);
var
  Src: TCustomBitmapItem;
begin
  if (not Assigned(Source)) or (Source is TCustomBitmapItem) then
  begin
    Src := TCustomBitmapItem(Source);
    if not Assigned(Src) then
    begin
      if (Width > 0) or (Height > 0) then
      begin
        Clear;
        Changed(False);
      end;
      FileName := '';
      FFixed := False;
    end
    else
    begin
      FFixed := False;
      FileName := Src.FileName;
      Clear;
      if (Src.Dormant) then
      begin
        if (Assigned(Src.FStream)) then
        begin
          Src.FStream.Position := 0;
          FStream.LoadFromStream(Src.FStream);
          FWidth := Src.Width;
          FHeight := Src.Height;
        end;
      end
      else
      begin
        if (Assigned(Src.FBitmap)) then
        begin
          Src.FBitmap.SaveToStream(FStream);
          FWidth := Src.FBitmap.Width;
          FHeight := Src.FBitmap.Height;
        end;
      end;
      FFixed := Src.Fixed;
      Scale := Src.Scale;
      Changed(False);
    end;
  end
  else
    inherited;
end;

procedure TCustomBitmapItem.Clear;
begin
  if Assigned(FStream) then
    FStream.Clear;
  if Assigned(FBitmap) then
    FBitmap.SetSize(0, 0);
  Dormant := True;
  FWidth := 0;
  FHeight := 0;
end;

function TCustomBitmapItem.GetIsEmpty: Boolean;
begin
  if Dormant then
    Result := (not Assigned(FStream)) or (FStream.Size <= 0)
  else
    Result := (not Assigned(FBitmap)) or
      (FBitmap.IsEmpty and ((not Assigned(FBitmap.ResourceBitmap)) or (FBitmap.ResourceBitmap.IsEmpty)));
end;

function TCustomBitmapItem.BitmapStored: Boolean;
begin
  Result := not IsEmpty;
end;

function TCustomBitmapItem.GetBitmap: TBitmapOfItem;
begin
  Dormant := False;
  Result := FBitmap;
end;

procedure TCustomBitmapItem.SetBitmap(const Value: TBitmapOfItem);
begin
  if Dormant then
  begin
    FStream.Clear;
    if Assigned(Value) then
      Value.SaveToStream(FStream);
  end
  else
  begin
    FBitmap.Assign(Value);
    IBitmapAccess(FBitmap).BitmapScale := FScale;
  end;
end;

procedure TCustomBitmapItem.ReadBitmap(Stream: TStream);
begin
  if Dormant then
  begin
    FStream.Clear;
    FStream.LoadFromStream(Stream);
  end
  else
  begin
    FBitmap.LoadFromStream(Stream);
    FWidth := FBitmap.Width;
    FHeight := FBitmap.Height;
    IBitmapAccess(FBitmap).BitmapScale := FScale;
  end;
end;

procedure TCustomBitmapItem.WriteBitmap(Stream: TStream);
begin
  if Dormant then
  begin
    FStream.Position := 0;
    FStream.SaveToStream(Stream);
  end
  else
  begin
    FBitmap.SaveToStream(Stream);
  end;
end;

procedure TCustomBitmapItem.SetCollection(Value: TCollection);
begin
  CheckInheritsClass(Value, TCustomMultiResBitmap);
  if Assigned(Value) and TCustomMultiResBitmap(Value).Fixed then
    raise EMultiResBitmap.CreateFMT(StrEChangeFixed, ['MultiResBitmap']);
  try
    if Assigned(Value) then
    begin
      if FScale <= 0 then
        FScale := ScaleDefault;
      while TCustomMultiResBitmap(Value).ItemByScale(FScale, True, True) <> nil do
        FScale := FScale + 1;
    end;
    inherited SetCollection(Value);
  finally
    FMultiResBitmap := TCustomMultiResBitmap(inherited Collection);
  end;
end;

function TCustomBitmapItem.GetComponent: TComponent;
var
  Parent: TPersistent;
begin
  Parent := self;
  repeat
    if Parent is TOwnedCollection then
      Parent := TOwnedCollection(Parent).Owner
    else if Parent is TCollectionItem then
      Parent := TCollectionItem(Parent).Collection
    else if not(Parent is TComponent) then
      Parent := nil;
  until (not Assigned(Parent)) or (Parent is TComponent);
  Result := TComponent(Parent);
end;

function TCustomBitmapItem.GetDisplayName: string;
begin
  if IsEmpty then
    Result := string.Format(StrScaleName, [SEmpty, Scale])
  else
    Result := string.Format(StrScaleFormat, [Scale])
end;

procedure TCustomBitmapItem.SetIndex(Value: Integer);
begin
  if Value <> inherited Index then
  begin
    if Assigned(MultiResBitmap) and (not MultiResBitmap.Fixed) then
      inherited
    else
      raise EMultiResBitmap.CreateFMT(StrEChangeFixed, ['MultiResBitmap']);
  end;
end;

function TCustomBitmapItem.GetFixed: Boolean;
begin
  if Assigned(MultiResBitmap) and (MultiResBitmap.Owner is TComponent) then
    Result := FFixed and (TComponent(MultiResBitmap.Owner).ComponentState * [csLoading, csDestroying] = [])
  else
    Result := False;
end;

class function TCustomBitmapItem.ScaleOfBitmap(const SourceSize, DestinationSize: TSize): Double;
var
  HScale, VScale: Double;
begin
  if SourceSize.cx > 0 then
    HScale := DestinationSize.cx / SourceSize.cx
  else
    HScale := 1;
  if SourceSize.cy > 0 then
    VScale := DestinationSize.cy / SourceSize.cy
  else
    VScale := 1;
  Result := Min(HScale, VScale);
end;

class function TCustomBitmapItem.RectOfBitmap(const SourceSize, DestinationSize: TSize): TRect;
var
  W, H: Integer;
  Scale: Double;
begin
  Scale := ScaleOfBitmap(SourceSize, DestinationSize);
  W := Round(SourceSize.cx * Scale);
  H := Round(SourceSize.cy * Scale);
  Result.Left := (DestinationSize.cx - W) div (2);
  Result.Top := (DestinationSize.cy - H) div (2);
  Result.Width := W;
  Result.Height := H;
end;

function TCustomBitmapItem.CreateBitmap(const AFileName: string = ''): TBitmap;
var
  C: TAlphaColorRec;
  CC: TColorRec;
  Data: TBitmapData;
  X, Y: Integer;
  LSize: TSize;
  LName: String;
  TmpBitmap: TBitmap;
  R: TRect;
  SrcRect, DstRect: TRectF;
begin
  Result := nil;
  LName := AFileName;
  if Trim(LName) = '' then
    LName := FileName;
  LSize := TSize.Create(0, 0);
  CC.Color := TColors.SysNone;
  if Assigned(MultiResBitmap) then
  begin
    case MultiResBitmap.SizeKind of
      skCustom:
        LSize := TSize.Create(MultiResBitmap.Width, MultiResBitmap.Height);
      skDefault:
        LSize := MultiResBitmap.DefaultSize;
    end;
    CC.Color := MultiResBitmap.TransparentColor;
  end;
  LSize.cx := Round(LSize.cx * Scale);
  LSize.cy := Round(LSize.cy * Scale);
  if not TBitmapCodecManager.IsCodedExists(ExtractFileExt(LName)) then
    raise EMultiResBitmap.CreateFMT(SInvalidFileName, [LName]);
  TmpBitmap := TBitmap.Create(0, 0);
  try
    TmpBitmap.LoadFromFile(LName);
    // replace transparent color
    if CC.Color <> TColors.SysNone then
    begin
      TmpBitmap.Map(TMapAccess.maReadWrite, Data);
      try
        if CC.Color = TColors.SysDefault then
        begin
          C.Color := Data.GetPixel(0, TmpBitmap.Height - 1);
          C.A := 255;
        end
        else
        begin
          CC.Color := TColors.ColorToRGB(CC.Color);
          C.A := 255;
          C.R := CC.R;
          C.G := CC.G;
          C.B := CC.B;
        end;
        for Y := 0 to TmpBitmap.Height - 1 do
          for X := 0 to TmpBitmap.Width - 1 do
          begin
            if C.Color = (TAlphaColorRec.Alpha) or Data.GetPixel(X, Y) then
              Data.SetPixel(X, Y, TAlphaColorRec.Null);
          end;
      finally
        TmpBitmap.Unmap(Data);
      end;
    end;
    // Resize bitmap
    if (LSize.cx > 0) and (LSize.cy > 0) then
    begin
      R := RectOfBitmap(TSize.Create(TmpBitmap.Width, TmpBitmap.Height), LSize);
      if (LSize.cx <> TmpBitmap.Width) or (LSize.cy <> TmpBitmap.Height) then
      begin
        Result := TBitmap.Create(LSize.cx, LSize.cy);
        try
          SrcRect := TRectF.Create(0, 0, TmpBitmap.Width, TmpBitmap.Height);
          DstRect := TRectF.Create(R);
          if Result.Canvas.BeginScene then
            try
              Result.Clear(TAlphaColorRec.Null);
              Result.Canvas.DrawBitmap(TmpBitmap, SrcRect, DstRect, 1, False);
            finally
              Result.Canvas.EndScene;
            end;
        except
          FreeAndNil(Result);
          raise;
        end;
      end
      else
        Result := TmpBitmap; // original size
    end
    else
      Result := TmpBitmap; // original size
  finally
    if TmpBitmap <> Result then
      FreeAndNil(TmpBitmap);
  end;
end;

procedure TCustomBitmapItem.ReadWidth(Reader: TReader);
begin
  FWidth := Reader.ReadInteger;
end;

procedure TCustomBitmapItem.WriteWidth(Writer: TWriter);
begin
  Writer.WriteInteger(FWidth);
end;

procedure TCustomBitmapItem.ReadHeight(Reader: TReader);
begin
  FHeight := Reader.ReadInteger;
end;

procedure TCustomBitmapItem.WriteHeight(Writer: TWriter);
begin
  Writer.WriteInteger(FHeight);
end;

procedure TCustomBitmapItem.ReadFileName(Reader: TReader);
begin
  FileName := Reader.ReadString;
end;

procedure TCustomBitmapItem.WriteFileName(Writer: TWriter);
begin
  Writer.WriteString(FileName);
end;

procedure TCustomBitmapItem.SetFixed(const Value: Boolean);
begin
  FFixed := Value;
end;

procedure TCustomBitmapItem.SetScale(const Value: Single);
var
  LValue: Single;
begin
  LValue := RoundTo(Value, ScaleRange);
  if FScale <> LValue then
  begin
    if Fixed then
      raise EMultiResBitmap.CreateFMT(StrEChangeFixed, ['BitmapItem']);
    if (LValue <= 0) or (LValue > ScaleMax) then
      raise EMultiResBitmap.CreateFMT(SOutOfRange, [0, ScaleMax]);
    if Assigned(Collection) and (TCustomMultiResBitmap(Collection).ItemByScale(LValue, True, True) <> nil) then
      raise EMultiResBitmap.CreateFMT(StrEDupScale, [Format(StrScaleFormat, [LValue])]);
    FScale := LValue;
    if Assigned(FBitmap) then
      IBitmapAccess(FBitmap).BitmapScale := FScale;
    Changed(False);
  end;
end;

{ TCustomMultiResBitmap }

procedure TCustomMultiResBitmap.Assign(Source: TPersistent);
var
  Src: TCustomMultiResBitmap;
  Item: TCustomBitmapItem;
  SourceName: string;
  I: Integer;
begin
  if (not Assigned(Source)) or (Source is TCustomMultiResBitmap) then
  begin
    Src := TCustomMultiResBitmap(Source);
    BeginUpdate;
    try
      if not Assigned(Src) then
      begin
        Clear;
        TransparentColor := TColors.SysNone;
        SizeKind := TSizeKind.skCustom;
        Width := 0;
        Height := 0;
        FFixed := False;
      end
      else
      begin
        FFixed := False;
        Clear;
        TransparentColor := Src.TransparentColor;
        SizeKind := Src.SizeKind;
        Width := Src.Width;
        Height := Src.Height;
        for I := 0 to Src.Count - 1 do
        begin
          Item := Add;
          Item.FFixed := False;
          Item.Assign(Src.Items[I]);
          Item.FFixed := Src.Items[I].Fixed;
        end;
        FFixed := Src.Fixed;
      end;
    finally
      EndUpdate;
    end;
  end
  else
  begin
    SourceName := Source.ClassName;
    raise EConvertError.CreateFMT(SAssignError, [SourceName, ClassName]);
  end;
end;

constructor TCustomMultiResBitmap.Create(AOwner: TPersistent; ItemClass: TCustomBitmapItemClass);
begin
  inherited Create(AOwner, ItemClass);
  FTransparentColor := TColors.SysNone;
end;

function TCustomMultiResBitmap.GetComponent: TComponent;
var
  Parent: TPersistent;
begin
  Parent := self;
  repeat
    if Parent is TOwnedCollection then
      Parent := TOwnedCollection(Parent).Owner
    else if Parent is TCollectionItem then
      Parent := TCollectionItem(Parent).Collection
    else if not(Parent is TComponent) then
      Parent := nil;
  until (not Assigned(Parent)) or (Parent is TComponent);
  Result := TComponent(Parent);
end;

function TCustomMultiResBitmap.GetDefaultSize: TSize;
begin
  Result := TSize.Create(32, 32);
end;

function TCustomMultiResBitmap.GetFixed: Boolean;
begin
  if Owner is TComponent then
  begin
    Result := FFixed and (TComponent(Owner).ComponentState * [csLoading, csDestroying] = [])
  end
  else
    Result := False;
end;

function TCustomMultiResBitmap.Add: TCustomBitmapItem;
begin
  Result := TCustomBitmapItem(inherited Add);
end;

function TCustomMultiResBitmap.Insert(Index: Integer): TCustomBitmapItem;
begin
  Result := TCustomBitmapItem(inherited Insert(Index));
end;

function TCustomMultiResBitmap.GetItem(Index: Integer): TCustomBitmapItem;
begin
  Result := TCustomBitmapItem(inherited GetItem(Index));
end;

procedure TCustomMultiResBitmap.SetItem(Index: Integer; const Value: TCustomBitmapItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TCustomMultiResBitmap.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
  if Assigned(Item) and (Action = cnDeleting) and Fixed then
    raise EMultiResBitmap.CreateFMT(StrEChangeFixed, ['MultiResBitmap']);
  inherited;
end;

procedure TCustomMultiResBitmap.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Height', ReadHeight, WriteHeight, (Height > 0) and (Height <> GetDefaultSize.cy));
  Filer.DefineProperty('Width', ReadWidth, WriteWidth, (Width > 0) and (Width <> GetDefaultSize.cx));
  Filer.DefineProperty('LoadSize', ReadLoadSize, WriteLoadSize, SizeKind <> skCustom);
  Filer.DefineProperty('TransparentColor', ReadColor, WriteColor, TransparentColor <> TColors.SysNone);
end;

function TCustomMultiResBitmap.ScaleArray(IncludeEmpty: Boolean): TArray<Single>;
var
  I, J: Integer;
  List: TList<TScaleName>;
  Comparer: TScaleNameComparer;
  S: Single;
  ScaleName: TScaleName;
  Found: Boolean;

begin
  Comparer := TScaleNameComparer.Create;
  List := TList<TScaleName>.Create(Comparer);
  try
    for I := 0 to Count - 1 do
      if (IncludeEmpty or (not Items[I].IsEmpty)) then
      begin
        S := Items[I].Scale;
        Found := False;
        for J := 0 to List.Count - 1 do
          if SameValue(List[J].Scale, S, TEpsilon.Scale) then
          begin
            Found := True;
            Break;
          end;
        if not Found then
        begin
          ScaleName.Scale := S;
          ScaleName.Name := '';
          List.Add(ScaleName);
        end;
      end;
    List.Sort;
    SetLength(Result, List.Count);
    for I := 0 to List.Count - 1 do
      Result[I] := List[I].Scale;
  finally
    FreeAndNil(List);
  end;
end;

function TCustomMultiResBitmap.ItemByScale(const AScale: Single; const ExactMatch: Boolean; const IncludeEmpty: Boolean)
  : TCustomBitmapItem;
var
  LScale, LCurrentScale: Single;
  I, LCount: Integer;
  LScaleArray: TArray<Single>;
begin
  LScale := RoundTo(AScale, TCustomBitmapItem.ScaleRange);
  Result := nil;
  if not ExactMatch then
  begin
    LScaleArray := ScaleArray(IncludeEmpty);
    LCount := Length(LScaleArray);
    if LCount > 0 then
    begin
      I := -1;
      repeat
        Inc(I)
      until (LScale < LScaleArray[I]) or (I >= LCount - 1);
      if I > 0 then
      begin
        LCurrentScale := LScaleArray[I - 1] + (LScaleArray[I] - LScaleArray[I - 1]) / 5;
        if LScale <= LCurrentScale then
          Dec(I);
      end;
      LScale := LScaleArray[I];
    end;
  end;
  for I := 0 to Count - 1 do
    if SameValue(LScale, Items[I].Scale, TEpsilon.Scale) and (IncludeEmpty or (not Items[I].IsEmpty)) then
    begin
      Result := Items[I];
      Exit;
    end
end;

type
  TWrapperMultiResBitmap = class(TComponent)
  private
    FBitmaps: TCustomMultiResBitmap;
  published
    property Bitmaps: TCustomMultiResBitmap read FBitmaps write FBitmaps;
  end;

procedure TCustomMultiResBitmap.LoadFromStream(S: TStream);
var
  Wrapper: TWrapperMultiResBitmap;
begin
  Wrapper := TWrapperMultiResBitmap.Create(nil);
  try
    BeginUpdate;
    try
      Clear;
      Wrapper.Bitmaps := self;
      S.ReadComponent(Wrapper);
    finally
      EndUpdate
    end;
  finally
    FreeAndNil(Wrapper);
  end;
end;

procedure TCustomMultiResBitmap.SaveToStream(S: TStream);
var
  Wrapper: TWrapperMultiResBitmap;
begin
  Wrapper := TWrapperMultiResBitmap.Create(nil);
  try
    Wrapper.Bitmaps := self;
    S.WriteComponent(Wrapper);
  finally
    FreeAndNil(Wrapper);
  end;
end;

procedure TCustomMultiResBitmap.ReadHeight(Reader: TReader);
begin
  Height := Reader.ReadInteger;
end;

procedure TCustomMultiResBitmap.WriteHeight(Writer: TWriter);
begin
  Writer.WriteInteger(Height);
end;

procedure TCustomMultiResBitmap.ReadWidth(Reader: TReader);
begin
  Width := Reader.ReadInteger;
end;

procedure TCustomMultiResBitmap.WriteWidth(Writer: TWriter);
begin
  Writer.WriteInteger(Width);
end;

procedure TCustomMultiResBitmap.ReadLoadSize(Reader: TReader);
var
  B: Integer;
begin
  B := $0000000F and Reader.ReadInteger;
  if (B >= Byte(Low(TSizeKind))) and (B <= Byte(High(TSizeKind))) then
    SizeKind := TSizeKind(B)
end;

procedure TCustomMultiResBitmap.WriteLoadSize(Writer: TWriter);
begin
  Writer.WriteInteger($0000000F and Integer(SizeKind));
end;

procedure TCustomMultiResBitmap.ReadColor(Reader: TReader);
begin
  FTransparentColor := Reader.ReadInteger;
end;

procedure TCustomMultiResBitmap.WriteColor(Writer: TWriter);
begin
  Writer.WriteInteger(TransparentColor);
end;

procedure TCustomMultiResBitmap.LoadItemFromStream(Stream: TStream; Scale: Single);
var
  B: TBitmap;
  Item: TCustomBitmapItem;
  LFound: Boolean;
begin
  B := nil;
  Item := ItemByScale(Scale, True, True);
  if Assigned(Item) then
    B := Item.Bitmap;
  LFound := Assigned(B);
  if not LFound then
    B := TBitmap.Create(0, 0);
  try
    B.LoadFromStream(Stream);
    if not LFound then
    begin
      BeginUpdate;
      try
        if not Assigned(Item) then
          Item := Add;
        Item.Scale := Scale;
        Item.Bitmap.Assign(B);
        Item.Dormant := True;
      finally
        EndUpdate;
      end;
    end;
  finally
    if not LFound then
      FreeAndNil(B);
  end;
end;

function TCustomMultiResBitmap.GetBitmaps(Scale: Single): TBitmapOfItem;
var
  Item: TCustomBitmapItem;
begin
  Scale := RoundTo(Scale, TCustomBitmapItem.ScaleRange);
  Item := ItemByScale(Scale, True, True);
  if Assigned(Item) then
    Result := Item.Bitmap
  else
    Result := nil;
end;

procedure TCustomMultiResBitmap.SetBitmaps(Scale: Single; const Value: TBitmapOfItem);
var
  Item: TCustomBitmapItem;
begin
  Scale := RoundTo(Scale, TCustomBitmapItem.ScaleRange);
  Item := ItemByScale(Scale, True, True);
  if Assigned(Item) then
  begin
    if Assigned(Value) and (not Value.IsEmpty) then
      Item.Bitmap.Assign(Value)
    else
    begin
      if Fixed then
        Item.Clear
      else
        Delete(Item.Index);
    end;
  end
  else
  begin
    if Assigned(Value) then
    begin
      Item := Add;
      Item.Scale := Scale;
      Item.Bitmap.Assign(Value);
    end;
  end;
end;

procedure TCustomMultiResBitmap.SetFixed(const Value: Boolean);
begin
  FFixed := Value;
end;

var
  vScaleList: TScaleList;
  vScaleNameComparer: TScaleNameComparer;

  { TScaleNameComparer }

function TScaleNameComparer.Compare(const Left, Right: TScaleName): Integer;
begin
  if Left.Scale < Right.Scale then
    Result := -1
  else if Left.Scale > Right.Scale then
    Result := 1
  else
    Result := 0;
end;

function ScaleList: TScaleList;
begin
  if not Assigned(vScaleList) then
  begin
    vScaleNameComparer := TScaleNameComparer.Create;
    vScaleList := TScaleList.Create(vScaleNameComparer);
  end;
  Result := vScaleList;
end;

function RegisterScaleName(Scale: Single; Name: string): Boolean;
var
  R: TScaleName;
  I: Integer;
begin
  Result := False;
  if ScaleList <> nil then
  begin
    R.Scale := RoundTo(Scale, TCustomBitmapItem.ScaleRange);
    R.Name := Name;
    for I := 0 to vScaleList.Count - 1 do
      if ScaleList[I].Scale = R.Scale then
      begin
        ScaleList[I] := R;
        Exit;
      end;
    ScaleList.Add(R);
    Result := True;
  end;
end;

{ TFixedBitmapItem }

constructor TFixedBitmapItem.Create(Collection: TCollection);
begin
  inherited;
  if Collection is TCustomMultiResBitmap then
    SetFixed(TCustomMultiResBitmap(Collection).Fixed);
end;

function TFixedBitmapItem.GetDisplayName: string;
  function GetName: string;
  var
    I: Integer;
  begin
    for I := 0 to ScaleList.Count - 1 do
      if Scale = ScaleList[I].Scale then
      begin
        Result := ScaleList[I].Name;
        Exit;
      end;
    Result := StrOther;
  end;

begin
  if IsEmpty then
    Result := inherited GetDisplayName
  else
    Result := string.Format(StrScaleName, [GetName, Scale]);
end;

{ TFixedMultiResBitmap }

constructor TFixedMultiResBitmap.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TFixedBitmapItem);
  CreateItems;
end;

constructor TFixedMultiResBitmap.Create(AOwner: TPersistent; ItemClass: TFixedBitmapItemClass);
begin
  inherited Create(AOwner, ItemClass);
  CreateItems;
end;

procedure TFixedMultiResBitmap.CreateItem(Scale: Single);
var
  I: Integer;
  NewItem: TCustomBitmapItem;
  OldFixed: Boolean;
begin
  Scale := RoundTo(Scale, TCustomBitmapItem.ScaleRange);
  for I := 0 to Count - 1 do
    if Items[I].Scale = Scale then
      Exit;
  NewItem := Add;
  OldFixed := Fixed;
  NewItem.SetFixed(False);
  try
    NewItem.Scale := Scale;
    for I := 0 to ScaleList.Count - 1 do
      if ScaleList[I].Scale = Scale then
      begin
        NewItem.Index := I;
        Break;
      end;
  finally
    NewItem.SetFixed(OldFixed);
  end;
end;

procedure TFixedMultiResBitmap.UpdateFixed;
var
  I, J: Integer;
begin
  for I := 0 to ScaleList.Count - 1 do
  begin
    for J := 0 to Count - 1 do
      if Items[J].Scale = ScaleList[I].Scale then
      begin
        Items[J].SetFixed(True);
        Break;
      end;
  end;
end;

procedure TFixedMultiResBitmap.CreateItems;
var
  I: Integer;
  OldFixed: Boolean;
begin
  OldFixed := Fixed;
  SetFixed(False);
  try
    for I := 0 to ScaleList.Count - 1 do
      CreateItem(ScaleList[I].Scale);
    UpdateFixed;
  finally
    SetFixed(OldFixed);
  end;
end;

procedure TFixedMultiResBitmap.EndUpdate;
begin
  if UpdateCount = 1 then
    CreateItems;
  inherited;
end;

function TFixedMultiResBitmap.Add: TFixedBitmapItem;
begin
  Result := TFixedBitmapItem(inherited Add);
end;

function TFixedMultiResBitmap.Insert(Index: Integer): TFixedBitmapItem;
begin
  Result := TFixedBitmapItem(inherited Insert(Index));
end;

function TFixedMultiResBitmap.GetItem(Index: Integer): TFixedBitmapItem;
begin
  Result := TFixedBitmapItem(inherited GetItem(Index));
end;

procedure TFixedMultiResBitmap.SetItem(Index: Integer; const Value: TFixedBitmapItem);
begin
  inherited SetItem(Index, Value);
end;

{$ENDREGION}

initialization

vScaleNameComparer := nil;
vScaleList := nil;
StrScaleFormat := Format('(%%5.%df)', [Abs(TCustomBitmapItem.ScaleRange)]);
StrScaleName := Format('%%s (%%5.%df)', [Abs(TCustomBitmapItem.ScaleRange)]);
RegisterScaleName(1, StrScale1);
//RegisterScaleName(2, StrScale2);

finalization

FreeAndNil(vScaleList);

end.
