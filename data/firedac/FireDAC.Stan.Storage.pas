{*******************************************************}
{                                                       }
{               Delphi FireDAC Framework                }
{               FireDAC streaming classes               }
{                                                       }
{ Copyright(c) 2004-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}
{$I FireDAC.inc}

unit FireDAC.Stan.Storage;

interface

uses
  System.TypInfo, System.SysUtils, System.Classes,
{$IFDEF FireDAC_STRG_XML}
  Xml.XMLDoc, Xml.XMLDOM, Xml.XMLIntf,
  {$IFDEF MSWINDOWS}
  Xml.Win.MsXMLDOM,
  {$ENDIF}
  {$IFDEF POSIX}
  Xml.adomxmldom,
  {$ENDIF}
{$ENDIF}
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Util;

type
  TFDStorableObject = class;
  TFDStorableObjectClass = class of TFDStorableObject;
  TFDStorage = class;
  TFDStorageClass = class of TFDStorage;
{$IFDEF FireDAC_STRG_XML}
  TFDXMLStorage = class;
{$ENDIF}
{$IFDEF FireDAC_STRG_BIN}
  TFDBinStorage = class;
{$ENDIF}

  TFDStorageMode = (smRead, smWrite);
  TFDStorageLoaderProc = function (AStorage: TFDStorage): TObject;
  TFDStorageSaverProc = procedure (AObject: TObject; AStorage: TFDStorage);
  TFDStorageDeferredAction = procedure (const APropName, AValue: String) of object;

  TFDStorableObject = class(TObject)
  protected
    class function GetClassPrefix: String; virtual;
    class function GetStreamingName(AStorage: TFDStorage): String; virtual;
    procedure InternalSaveToStorage(AStorage: TFDStorage); virtual;
    procedure InternalLoadFromStorage(AStorage: TFDStorage); virtual;
  public
    constructor Create; overload; virtual;
    procedure SaveToStorage(AStorage: TFDStorage);
    procedure LoadFromStorage(AStorage: TFDStorage);
  end;

  TFDStorageManager = class(TObject)
  private
    FClasses: TFDStringList;
    FLoaders: TFDPtrList;
    FSavers: TFDPtrList;
    FStorages: TFDPtrList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterClass(AClass: TFDStorableObjectClass); overload;
    procedure RegisterClassAlias(const AName: String; AClass: TFDStorableObjectClass);
    procedure RegisterClass(AClass: TClass; const AStreamingName: String;
      ALoader: TFDStorageLoaderProc; ASaver: TFDStorageSaverProc); overload;
    procedure RegisterStorage(AClass: TFDStorageClass);
    function GetStorage(const AFileName: String; AFormat: TFDStorageFormat;
      AResOpts: TFDBottomResourceOptions; AEncoder: TFDEncoder): TFDStorage;
  end;

  TFDStorage = class(TObject)
  private
    FMode: TFDStorageMode;
    FStreamVersion: Integer;
    FPrevName: String;
    FPrevIndex: Integer;
    FActions: TFDObjList;
    FLevel: Integer;
    FStream: TStream;
    FOwnStream: Boolean;
    FBuff: TMemoryStream;
    FResOpts: TFDBottomResourceOptions;
    FEncoder: TFDEncoder;
    FWorkFileName: String;
    FOriginalFileExt: String;
    procedure ClearDeferredActions;
    procedure PerformDeferredActions;
  protected
    function CheckBuffer(ASize: LongWord): TMemoryStream;
    class function CheckExtention(const AExt: String): Boolean; virtual;
    class function CheckFormat(AFmt: TFDStorageFormat): Boolean; virtual;
  public
    constructor Create(AResOpts: TFDBottomResourceOptions; AEncoder: TFDEncoder); virtual;
    destructor Destroy; override;
    procedure Open(const AFileName: string; AStream: TStream; AMode: TFDStorageMode); virtual;
    function IsOpen: Boolean; virtual;
    procedure Close; virtual;
    function IsObjectEnd(const AObjectName: string): Boolean; virtual; abstract;
    function ReadBoolean(const APropName: string; ADefValue: Boolean): Boolean; virtual; abstract;
    function ReadDate(const APropName: string; ADefValue: TDateTime): TDateTime; virtual; abstract;
    function ReadFloat(const APropName: string; ADefValue: Double): Double; virtual; abstract;
    function ReadInteger(const APropName: string; ADefValue: Integer): Integer; virtual; abstract;
    function ReadLongWord(const APropName: string; ADefValue: LongWord): LongWord; virtual; abstract;
    function ReadInt64(const APropName: string; ADefValue: Int64): Int64; virtual; abstract;
    function ReadObjectBegin(const AObjectName: string): String; virtual; abstract;
    procedure ReadObjectEnd(const AObjectName: string); virtual; abstract;
    function ReadString(const APropName, ADefValue: String): String;
    function ReadAnsiString(const APropName: string; const ADefValue: TFDAnsiString): TFDAnsiString; virtual; abstract;
    function ReadWideString(const APropName: string; const ADefValue: UnicodeString): UnicodeString; virtual; abstract;
    function ReadValue(const APropName: String; APropIndex: Word; ADataType: TFDDataType;
      out ABuff: Pointer; out ALen: LongWord): Boolean; virtual; abstract;
    function ReadEnum(APropName: string; ATypeInfo: PTypeInfo;
      ADefValue: Integer): Integer; virtual; abstract;
    function TestObject(const AObjectName: string): Boolean; virtual; abstract;
    function TestProperty(const APropName: string): Boolean; virtual; abstract;
    function TestAndReadProperty(const APropName: string): Boolean; virtual;
    procedure WriteBoolean(const APropName: string; const AValue, ADefValue: Boolean); virtual; abstract;
    procedure WriteDate(const APropName: string; const AValue, ADefValue: TDateTime); virtual; abstract;
    procedure WriteFloat(const APropName: string; const AValue, ADefValue: Double); virtual; abstract;
    procedure WriteInteger(const APropName: string; const AValue, ADefValue: Integer); virtual; abstract;
    procedure WriteLongWord(const APropName: string; const AValue, ADefValue: LongWord); virtual; abstract;
    procedure WriteInt64(const APropName: string; const AValue, ADefValue: Int64); virtual; abstract;
    procedure WriteObjectBegin(const AObjectName: string); virtual; abstract;
    procedure WriteObjectEnd(const AObjectName: string); virtual; abstract;
    procedure WriteString(const APropName, AValue, ADefValue: String);
    procedure WriteAnsiString(const APropName: string; const AValue, ADefValue: TFDAnsiString); virtual; abstract;
    procedure WriteWideString(const APropName: string; const AValue, ADefValue: UnicodeString); virtual; abstract;
    procedure WriteValue(const APropName: String; APropIndex: Word; ADataType: TFDDataType;
      ABuff: Pointer; ALen: LongWord); virtual; abstract;
    procedure WriteEnum(APropName: string; ATypeInfo: PTypeInfo; const AValue,
      ADefValue: Integer); virtual; abstract;
    procedure DeferAction(const APropName, AValue: String; AAction: TFDStorageDeferredAction);
    function GetBookmark: TObject; virtual; abstract;
    procedure SetBookmark(const AValue: TObject); virtual; abstract;
    function IsStored(AItem: TFDStoreItem): Boolean;
    // high level
    function LoadObject: TObject;
    procedure SaveObject(AObject: TObject);
    // properties
    property Mode: TFDStorageMode read FMode;
    property StreamVersion: Integer read FStreamVersion;
    property OwnStream: Boolean read FOwnStream write FOwnStream;
  end;

{$IFDEF FireDAC_STRG_XML}
  TFDXMLStorage = class(TFDStorage)
  private
    FCurrentChildElement: IDOMElement;
    FCurrentElement: IDOMElement;
    FParser: IXMLDocument;
    FDecimalSeparator: Char;
    FLevel: Integer;
    procedure AddAttribute(const AAttrName, AValue: DOMString);
    procedure AddCData(AStream: TMemoryStream);
    procedure AddFormat;
    function SkipText(const AElem: IDOMElement): IDOMElement; overload;
    function SkipText(const AElem: IDOMNode): IDOMElement; overload;
    function AddElement(const AElemName: DOMString): IDOMElement;
    function CurrentElement: IDOMElement;
    procedure ReadCData(AStream: TMemoryStream);
    function ReadProperty(const APropName: DOMString): DOMString;
    function GetXMLObject: IDOMElement;
    function EncodeName(const AName: DOMString): DOMString;
    function DecodeName(const AName: DOMString): DOMString;
  protected
    class function CheckExtention(const AExt: String): Boolean; override;
    class function CheckFormat(AFmt: TFDStorageFormat): Boolean; override;
  public
    constructor Create(AResOpts: TFDBottomResourceOptions; AEncoder: TFDEncoder); override;
    destructor Destroy; override;
    procedure Close; override;
    function IsObjectEnd(const AObjectName: string): Boolean; override;
    procedure Open(const AFileName: string; AStream: TStream; AMode: TFDStorageMode); override;
    function ReadBoolean(const APropName: string; ADefValue: Boolean): Boolean; override;
    function ReadDate(const APropName: string; ADefValue: TDateTime): TDateTime; override;
    function ReadFloat(const APropName: string; ADefValue: Double): Double; override;
    function ReadInteger(const APropName: string; ADefValue: Integer): Integer; override;
    function ReadLongWord(const APropName: string; ADefValue: LongWord): LongWord; override;
    function ReadInt64(const APropName: string; ADefValue: Int64): Int64; override;
    function ReadObjectBegin(const AObjectName: string): String; override;
    procedure ReadObjectEnd(const AObjectName: string); override;
    function ReadAnsiString(const APropName: string; const ADefValue: TFDAnsiString): TFDAnsiString; override;
    function ReadWideString(const APropName: string; const ADefValue: UnicodeString): UnicodeString; override;
    function ReadValue(const APropName: String; APropIndex: Word; ADataType: TFDDataType;
      out ABuff: Pointer; out ALen: LongWord): Boolean; override;
    function ReadEnum(APropName: string; ATypeInfo: PTypeInfo; ADefValue: Integer): Integer; override;
    function TestObject(const AObjectName: string): Boolean; override;
    function TestProperty(const APropName: string): Boolean; override;
    procedure WriteBoolean(const APropName: string; const AValue, ADefValue: Boolean); override;
    procedure WriteDate(const APropName: string; const AValue, ADefValue: TDateTime); override;
    procedure WriteFloat(const APropName: string; const AValue, ADefValue: Double); override;
    procedure WriteInteger(const APropName: string; const AValue, ADefValue: Integer); override;
    procedure WriteLongWord(const APropName: string; const AValue, ADefValue: LongWord); override;
    procedure WriteInt64(const APropName: string; const AValue, ADefValue: Int64); override;
    procedure WriteObjectBegin(const AObjectName: string); override;
    procedure WriteObjectEnd(const AObjectName: string); override;
    procedure WriteAnsiString(const APropName: string; const AValue, ADefValue: TFDAnsiString); override;
    procedure WriteWideString(const APropName: string; const AValue, ADefValue: UnicodeString); override;
    procedure WriteValue(const APropName: String; APropIndex: Word; ADataType: TFDDataType;
      ABuff: Pointer; ALen: LongWord); override;
    procedure WriteEnum(APropName: string; ATypeInfo: PTypeInfo; const AValue,
      ADefValue: Integer); override;
    function GetBookmark: TObject; override;
    procedure SetBookmark(const AValue: TObject); override;
  end;
{$ENDIF}

{$IFDEF FireDAC_STRG_BIN}
  TFDBinStorage = class(TFDStorage)
  private
    FDictionary: TFDStringList;
    FDictionaryIndex: Integer;
    procedure InternalReadProperty(out AName: String);
    procedure InternalWriteProperty(const APropName: string;
      ApValue: Pointer; ALen: LongWord);
    function InternalReadObject(out AName: String): Boolean;
    function InternalReadObjectEnd: Boolean;
    function LookupName(const AName: String): Word;
  protected
    class function CheckExtention(const AExt: String): Boolean; override;
    class function CheckFormat(AFmt: TFDStorageFormat): Boolean; override;
  public
    constructor Create(AResOpts: TFDBottomResourceOptions; AEncoder: TFDEncoder); override;
    destructor Destroy; override;
    procedure Close; override;
    function IsObjectEnd(const AObjectName: string): Boolean; override;
    procedure Open(const AFileName: string; AStream: TStream; AMode: TFDStorageMode); override;
    function ReadBoolean(const APropName: string; ADefValue: Boolean): Boolean; override;
    function ReadDate(const APropName: string; ADefValue: TDateTime): TDateTime; override;
    function ReadFloat(const APropName: string; ADefValue: Double): Double; override;
    function ReadInteger(const APropName: string; ADefValue: Integer): Integer; override;
    function ReadLongWord(const APropName: string; ADefValue: LongWord): LongWord; override;
    function ReadInt64(const APropName: string; ADefValue: Int64): Int64; override;
    function ReadObjectBegin(const AObjectName: string): String; override;
    procedure ReadObjectEnd(const AObjectName: string); override;
    function ReadAnsiString(const APropName: string; const ADefValue: TFDAnsiString): TFDAnsiString; override;
    function ReadWideString(const APropName: string; const ADefValue: UnicodeString): UnicodeString; override;
    function ReadValue(const APropName: String; APropIndex: Word; ADataType: TFDDataType;
      out ABuff: Pointer; out ALen: LongWord): Boolean; override;
    function ReadEnum(APropName: string; ATypeInfo: PTypeInfo; ADefValue: Integer): Integer; override;
    function TestObject(const AObjectName: string): Boolean; override;
    function TestProperty(const APropName: string): Boolean; override;
    function TestAndReadProperty(const APropName: string): Boolean; override;
    procedure WriteBoolean(const APropName: string; const AValue, ADefValue: Boolean); override;
    procedure WriteDate(const APropName: string; const AValue, ADefValue: TDateTime); override;
    procedure WriteFloat(const APropName: string; const AValue, ADefValue: Double); override;
    procedure WriteInteger(const APropName: string; const AValue, ADefValue: Integer); override;
    procedure WriteLongWord(const APropName: string; const AValue, ADefValue: LongWord); override;
    procedure WriteInt64(const APropName: string; const AValue, ADefValue: Int64); override;
    procedure WriteObjectBegin(const AObjectName: string); override;
    procedure WriteObjectEnd(const AObjectName: string); override;
    procedure WriteAnsiString(const APropName: string; const AValue, ADefValue: TFDAnsiString); override;
    procedure WriteWideString(const APropName: string; const AValue, ADefValue: UnicodeString); override;
    procedure WriteValue(const APropName: String; APropIndex: Word; ADataType: TFDDataType;
      ABuff: Pointer; ALen: LongWord); override;
    procedure WriteEnum(APropName: string; ATypeInfo: PTypeInfo; const AValue,
      ADefValue: Integer); override;
    function GetBookmark: TObject; override;
    procedure SetBookmark(const AValue: TObject); override;
  end;
{$ENDIF}

  function FDStorageManager(): TFDStorageManager;

{-------------------------------------------------------------------------------}
implementation

uses
{$IFDEF MSWINDOWS}
  // Preventing from "Inline has not expanded"
  Winapi.Windows,
{$ENDIF}
  Data.FmtBCD, Data.SQLTimSt, Data.DB,
  FireDAC.Stan.Consts, FireDAC.Stan.Error, FireDAC.Stan.SQLTimeInt;

var
  GStorageManager: TFDStorageManager = nil;

{-------------------------------------------------------------------------------}
function FDStorageManager(): TFDStorageManager;
begin
  if GStorageManager = nil then
    GStorageManager := TFDStorageManager.Create;
  Result := GStorageManager;
end;

{-------------------------------------------------------------------------------}
{ TFDStorableObject                                                             }
{-------------------------------------------------------------------------------}
constructor TFDStorableObject.Create;
begin
  inherited Create;
end;

{-------------------------------------------------------------------------------}
class function TFDStorableObject.GetClassPrefix: String;
begin
  Result := 'TFD';
end;

{-------------------------------------------------------------------------------}
class function TFDStorableObject.GetStreamingName(AStorage: TFDStorage): String;
begin
  Result := Copy(ClassName, Length(GetClassPrefix()) + 1, Length(ClassName));
end;

{-------------------------------------------------------------------------------}
procedure TFDStorableObject.InternalLoadFromStorage(AStorage: TFDStorage);
begin
  // nothing
end;

{-------------------------------------------------------------------------------}
procedure TFDStorableObject.InternalSaveToStorage(AStorage: TFDStorage);
begin
  // nothing
end;

{-------------------------------------------------------------------------------}
procedure TFDStorableObject.LoadFromStorage(AStorage: TFDStorage);
var
  sName: String;
begin
  sName := GetStreamingName(AStorage);
  AStorage.ReadObjectBegin(sName);
  Inc(AStorage.FLevel);
  if AStorage.FLevel = 1 then
    AStorage.ClearDeferredActions;
  try
    InternalLoadFromStorage(AStorage);
  finally
    Dec(AStorage.FLevel);
    if AStorage.FLevel = 0 then
      AStorage.PerformDeferredActions;
    AStorage.ReadObjectEnd(sName);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDStorableObject.SaveToStorage(AStorage: TFDStorage);
var
  sName: String;
begin
  sName := GetStreamingName(AStorage);
  AStorage.WriteObjectBegin(sName);
  try
    InternalSaveToStorage(AStorage);
  finally
    AStorage.WriteObjectEnd(sName);
  end;
end;

{-------------------------------------------------------------------------------}
{ TFDStorageManager                                                             }
{-------------------------------------------------------------------------------}
constructor TFDStorageManager.Create;
begin
  inherited Create;
  FClasses := TFDStringList.Create;
  FClasses.CaseSensitive := False;
  FLoaders := TFDPtrList.Create;
  FSavers := TFDPtrList.Create;
  FStorages := TFDPtrList.Create;
end;

{-------------------------------------------------------------------------------}
destructor TFDStorageManager.Destroy;
begin
  FDFreeAndNil(FClasses);
  FDFreeAndNil(FLoaders);
  FDFreeAndNil(FSavers);
  FDFreeAndNil(FStorages);
  inherited Destroy;
end;

{-------------------------------------------------------------------------------}
procedure TFDStorageManager.RegisterClass(AClass: TFDStorableObjectClass);
begin
  FClasses.AddPtr(AClass.GetStreamingName(nil), AClass);
  FLoaders.Add(nil);
  FSavers.Add(nil);
end;

{-------------------------------------------------------------------------------}
procedure TFDStorageManager.RegisterClassAlias(const AName: String; AClass: TFDStorableObjectClass);
begin
  FClasses.AddPtr(AName, AClass);
  FLoaders.Add(nil);
  FSavers.Add(nil);
end;

{-------------------------------------------------------------------------------}
procedure TFDStorageManager.RegisterClass(AClass: TClass; const AStreamingName: String;
  ALoader: TFDStorageLoaderProc; ASaver: TFDStorageSaverProc);
begin
  FClasses.AddPtr(AStreamingName, AClass);
  FLoaders.Add(@ALoader);
  FSavers.Add(@ASaver);
end;

{-------------------------------------------------------------------------------}
procedure TFDStorageManager.RegisterStorage(AClass: TFDStorageClass);
begin
  FStorages.Add(AClass);
end;

{-------------------------------------------------------------------------------}
function TFDStorageManager.GetStorage(const AFileName: String;
  AFormat: TFDStorageFormat; AResOpts: TFDBottomResourceOptions;
  AEncoder: TFDEncoder): TFDStorage;
var
  sExt: String;
  oClass: TFDStorageClass;
  i: Integer;
begin
  oClass := nil;
  sExt := '';
  if AFormat = sfAuto then begin
    sExt := ExtractFileExt(AFileName);
    if sExt = '' then
      sExt := AResOpts.DefaultStoreExt;
    if sExt <> '' then
      for i := 0 to FStorages.Count - 1 do
        if TFDStorageClass(FStorages[i]).CheckExtention(sExt) then begin
          oClass := TFDStorageClass(FStorages[i]);
          Break;
        end;
  end;
  if oClass = nil then begin
    if AFormat = sfAuto then
      AFormat := AResOpts.DefaultStoreFormat;
    for i := 0 to FStorages.Count - 1 do
      if TFDStorageClass(FStorages[i]).CheckFormat(AFormat) then begin
        oClass := TFDStorageClass(FStorages[i]);
        Break;
      end;
  end;
  if oClass = nil then
    FDException(Self, [S_FD_LStan], er_FD_StanStrgUnknownFmt, [sExt]);
  Result := oClass.Create(AResOpts, AEncoder);
end;

{-------------------------------------------------------------------------------}
{ TFDStorage                                                                    }
{-------------------------------------------------------------------------------}
constructor TFDStorage.Create(AResOpts: TFDBottomResourceOptions; AEncoder: TFDEncoder);
begin
  inherited Create;
  FStreamVersion := C_FD_StorageVer;
  FActions := TFDObjList.Create;
  FResOpts := AResOpts;
  FEncoder := AEncoder;
end;

{-------------------------------------------------------------------------------}
destructor TFDStorage.Destroy;
begin
  Close;
  FDFreeAndNil(FActions);
  FDFreeAndNil(FBuff);
  FResOpts := nil;
  FEncoder := nil;
  inherited Destroy;
end;

{-------------------------------------------------------------------------------}
procedure TFDStorage.Open(const AFileName: string; AStream: TStream; AMode: TFDStorageMode);
begin
  ASSERT((AStream <> nil) or (AFileName <> '') or
         (FResOpts <> nil) and (FResOpts.PersistentFileName <> ''));
  FMode := AMode;
  if FMode = smWrite then
    if (FResOpts = nil) or (FResOpts.StoreVersion = -1) then
      FStreamVersion := C_FD_StorageVer
    else
      FStreamVersion := FResOpts.StoreVersion
  else
    FStreamVersion := C_FD_StorageVer;
  if AStream = nil then begin
    if FResOpts <> nil then
      FWorkFileName := FResOpts.ResolveFileName(AFileName)
    else
      FWorkFileName := AFileName;
    if AMode = smWrite then begin
      if (FResOpts <> nil) and FResOpts.Backup then begin
        FOriginalFileExt := ExtractFileExt(FWorkFileName);
        FWorkFileName := ChangeFileExt(FWorkFileName, '.~~~');
      end;
      FStream := TFDFileStream.Create(FWorkFileName, fmCreate or fmShareExclusive);
    end
    else
      FStream := TFDFileStream.Create(FWorkFileName, fmOpenRead or fmShareDenyWrite);
    FOwnStream := True;
  end
  else begin
    FWorkFileName := '';
    FStream := AStream;
    FOwnStream := False;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDStorage.Close;
var
  sTargetFileName: String;
  sBackupFileName: String;
  lStreamed: Boolean;
begin
  ClearDeferredActions;
  lStreamed := FStream <> nil;
  if FOwnStream then
    FDFreeAndNil(FStream)
  else
    FStream := nil;
  if lStreamed and (FResOpts <> nil) and FResOpts.Backup and
     (FMode = smWrite) and (FWorkFileName <> '') then begin
    sTargetFileName := ChangeFileExt(FWorkFileName, FOriginalFileExt);
    if FileExists(sTargetFileName) then begin
      sBackupFileName := FWorkFileName;
      if FResOpts.BackupFolder <> '' then
        sBackupFileName := FDNormPath(FDExpandStr(FResOpts.BackupFolder)) +
          ExtractFileName(sBackupFileName);
      if FResOpts.BackupExt <> '' then
        sBackupFileName := ChangeFileExt(sBackupFileName, FResOpts.BackupExt)
      else
        sBackupFileName := ChangeFileExt(sBackupFileName, '.bak');
      if (sBackupFileName <> '') and
         (AnsiCompareText(ExpandFileName(sTargetFileName), ExpandFileName(sBackupFileName)) <> 0) then
        FDFileMove(sTargetFileName, sBackupFileName);
    end;
    if FileExists(FWorkFileName) then
      FDFileMove(FWorkFileName, sTargetFileName);
  end;
end;

{-------------------------------------------------------------------------------}
function TFDStorage.IsOpen: Boolean;
begin
  Result := FStream <> nil;
end;

{-------------------------------------------------------------------------------}
function TFDStorage.TestAndReadProperty(const APropName: string): Boolean;
begin
  Result := TestProperty(APropName);
end;

{-------------------------------------------------------------------------------}
function TFDStorage.LoadObject: TObject;
var
  sName: String;
  i: Integer;
  oClass: TClass;
  oLoader: TFDStorageLoaderProc;
begin
  sName := ReadObjectBegin('');
  try
    if FPrevName <> sName then begin
      i := FDStorageManager().FClasses.IndexOf(sName);
      if i = -1 then
        FDException(Self, [S_FD_LStan], er_FD_StanStrgClassUnknown, [sName]);
      FPrevName := sName;
      FPrevIndex := i;
    end
    else
      i := FPrevIndex;
    oClass := TClass(FDStorageManager().FClasses.Ptrs[i]);
    oLoader := TFDStorageLoaderProc(FDStorageManager().FLoaders[i]);
    if not Assigned(oLoader) then begin
      Result := TFDStorableObjectClass(oClass).Create;
      TFDStorableObject(Result).InternalLoadFromStorage(Self);
    end
    else
      Result := oLoader(Self);
  finally
    ReadObjectEnd(sName);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDStorage.SaveObject(AObject: TObject);
var
  sName: String;
  i: Integer;
  oSaver: TFDStorageSaverProc;
begin
  if AObject is TFDStorableObject then
    sName := TFDStorableObject(AObject).GetStreamingName(Self)
  else
    sName := AObject.ClassName;
  if FPrevName <> sName then begin
    i := FDStorageManager().FClasses.IndexOf(sName);
    if i = -1 then
      FDException(Self, [S_FD_LStan], er_FD_StanStrgClassUnknown, [sName]);
    FPrevName := sName;
    FPrevIndex := i;
  end
  else
    i := FPrevIndex;
  oSaver := TFDStorageSaverProc(FDStorageManager().FSavers[i]);
  if not Assigned(oSaver) then
    TFDStorableObject(AObject).SaveToStorage(Self)
  else
    oSaver(AObject, Self);
end;

{-------------------------------------------------------------------------------}
type
  TDeferredAction = class(TObject)
    FPropName: String;
    FValue: String;
    FAction: TFDStorageDeferredAction;
  end;

procedure TFDStorage.DeferAction(const APropName, AValue: String;
  AAction: TFDStorageDeferredAction);
var
  oAct: TDeferredAction;
begin
  if (APropName = '') or (AValue = '') or not Assigned(AAction) then
    Exit;
  oAct := TDeferredAction.Create;
  oAct.FPropName := APropName;
  oAct.FValue := AValue;
  oAct.FAction := AAction;
  FActions.Add(oAct);
end;

{-------------------------------------------------------------------------------}
procedure TFDStorage.PerformDeferredActions;
var
  i: Integer;
  oAct: TDeferredAction;
begin
  try
    for i := 0 to FActions.Count - 1 do begin
      oAct := TDeferredAction(FActions.Items[i]);
      oAct.FAction(oAct.FPropName, oAct.FValue);
    end;
  finally
    ClearDeferredActions;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDStorage.ClearDeferredActions;
var
  i: Integer;
begin
  for i := 0 to FActions.Count - 1 do
    FDFree(TDeferredAction(FActions.Items[i]));
  FActions.Clear;
end;

{-------------------------------------------------------------------------------}
function TFDStorage.CheckBuffer(ASize: LongWord): TMemoryStream;
begin
  if FBuff = nil then
    FBuff := TMemoryStream.Create;
  if LongWord(FBuff.Size) < ASize then
    FBuff.Size := ASize;
  Result := FBuff;
end;

{-------------------------------------------------------------------------------}
class function TFDStorage.CheckExtention(const AExt: String): Boolean;
begin
  Result := False;
end;

{-------------------------------------------------------------------------------}
class function TFDStorage.CheckFormat(AFmt: TFDStorageFormat): Boolean;
begin
  Result := AFmt = sfAuto;
end;

{-------------------------------------------------------------------------------}
function TFDStorage.ReadString(const APropName, ADefValue: String): String;
begin
  Result := ReadWideString(APropName, ADefValue);
end;

{-------------------------------------------------------------------------------}
procedure TFDStorage.WriteString(const APropName, AValue, ADefValue: String);
begin
  WriteWideString(APropName, AValue, ADefValue);
end;

{-------------------------------------------------------------------------------}
function TFDStorage.IsStored(AItem: TFDStoreItem): Boolean;
begin
  Result := (FResOpts = nil) or (AItem in FResOpts.StoreItems);
end;

{$IFDEF FireDAC_STRG_XML}
{-------------------------------------------------------------------------------}
{ TFDXMLStorage                                                                 }
{-------------------------------------------------------------------------------}
constructor TFDXMLStorage.Create(AResOpts: TFDBottomResourceOptions; AEncoder: TFDEncoder);
begin
  inherited Create(AResOpts, AEncoder);
  FDecimalSeparator := '.';
end;

{-------------------------------------------------------------------------------}
destructor TFDXMLStorage.Destroy;
begin
  inherited Destroy;
end;

{-------------------------------------------------------------------------------}
{ http://msdn.microsoft.com/en-us/library/xfaz0c52(VS.71).aspx }
{ http://www.w3.org/TR/2000/REC-xml-20001006.html#NT-Name }
function TFDXMLStorage.EncodeName(const AName: DOMString): DOMString;
var
  i, iCur: Integer;

  procedure PutCh(ACh: WideChar);
  begin
    Inc(iCur);
    if iCur > Length(Result) then
      SetLength(Result, Length(Result) * 2);
    Result[iCur] := ACh;
  end;

  procedure PutOrd(ACh: WideChar);
  var
    s: String;
    i: Integer;
  begin
    s := Format('_0x%.4x_', [Integer(ACh)]);
    for i := 1 to Length(s) do
      PutCh(WideChar(s[i]));
  end;

begin
  SetLength(Result, Length(AName));
  if Length(AName) = 0 then
    Exit;
  iCur := 0;
  if not FDInSet(Char(AName[1]), ['a' .. 'z', 'A' .. 'Z', '_', ':']) then
    PutOrd(AName[1])
  else
    PutCh(AName[1]);
  for i := 2 to Length(AName) do
    if not FDInSet(Char(AName[i]), ['a' .. 'z', 'A' .. 'Z', '0' .. '9', '.', '-', '_', ':']) then
      PutOrd(AName[i])
    else
      PutCh(AName[i]);
  SetLength(Result, iCur);
end;

{-------------------------------------------------------------------------------}
function TFDXMLStorage.DecodeName(const AName: DOMString): DOMString;
var
  i, iCur: Integer;

  procedure PutCh(ACh: WideChar);
  begin
    Inc(iCur);
    if iCur > Length(Result) then
      SetLength(Result, Length(Result) * 2);
    Result[iCur] := ACh;
  end;

begin
  SetLength(Result, Length(AName));
  if Length(AName) = 0 then
    Exit;
  iCur := 0;
  i := 1;
  while i <= Length(AName) do
    if (i <= Length(AName) - 6) and
       (AName[i] = '_') and
       (AName[i + 1] = '0') and
       ((AName[i + 2] = 'x') or (AName[i] = 'X')) and
       FDInSet(Char(AName[i + 3]), ['0' .. '9', 'a' .. 'f', 'A' .. 'F']) and
       FDInSet(Char(AName[i + 4]), ['0' .. '9', 'a' .. 'f', 'A' .. 'F']) and
       FDInSet(Char(AName[i + 5]), ['0' .. '9', 'a' .. 'f', 'A' .. 'F']) and
       FDInSet(Char(AName[i + 6]), ['0' .. '9', 'a' .. 'f', 'A' .. 'F']) and
       (AName[i + 7] = '_') then begin
      PutCh(WideChar(StrToInt(Copy(AName, i + 1, 6))));
      Inc(i, 8);
    end
    else begin
      PutCh(AName[i]);
      Inc(i);
    end;
  SetLength(Result, iCur);
end;

{-------------------------------------------------------------------------------}
procedure TFDXMLStorage.AddAttribute(const AAttrName, AValue: DOMString);
begin
  CurrentElement.SetAttribute(EncodeName(AAttrName), AValue);
end;

{-------------------------------------------------------------------------------}
procedure TFDXMLStorage.AddCData(AStream: TMemoryStream);
var
  s: string;
begin
  s := FDBin2Hex(AStream.Memory, AStream.Size);
  CurrentElement.appendChild(FParser.DOMDocument.createCDATASection(s));
end;

{-------------------------------------------------------------------------------}
procedure TFDXMLStorage.AddFormat;
var
  i: Integer;
  s: DOMString;
  pCh: PWideChar;
  oFmt: IDOMText;
begin
  SetLength(s, (FLevel - 1) * 2 + 1);
  pCh := PWideChar(s);
  pCh^ := #10;
  for i := 1 to Length(s) - 1 do
    (pCh + i)^ := ' ';
  oFmt := FParser.DOMDocument.createTextNode(s);
  if CurrentElement = nil then
    FParser.DOMDocument.appendChild(oFmt)
  else
    CurrentElement.appendChild(oFmt);
end;

{-------------------------------------------------------------------------------}
function TFDXMLStorage.SkipText(const AElem: IDOMElement): IDOMElement;
begin
  Result := AElem;
  while (Result <> nil) and (Result.nodeType = TEXT_NODE) do
    Result := IDOMElement(Result.nextSibling);
end;

{-------------------------------------------------------------------------------}
function TFDXMLStorage.SkipText(const AElem: IDOMNode): IDOMElement;
begin
  Result := SkipText(IDOMElement(AElem));
end;

{-------------------------------------------------------------------------------}
function TFDXMLStorage.AddElement(const AElemName: DOMString): IDOMElement;
begin
  Result := FParser.DOMDocument.createElement(EncodeName(AElemName));
  if CurrentElement = nil then
    FParser.DOMDocument.appendChild(Result)
  else
    CurrentElement.appendChild(Result);
end;

{-------------------------------------------------------------------------------}
function TFDXMLStorage.CurrentElement: IDOMElement;
begin
  if FCurrentElement <> nil then
    Result := FCurrentElement
  else
    Result := FParser.DOMDocument.documentElement;
end;

{-------------------------------------------------------------------------------}
procedure TFDXMLStorage.ReadCData(AStream: TMemoryStream);
var
  oNode: IDOMNode;
  i: Integer;
  sHex: String;
{$IFDEF NEXTGEN}
  aBuff: TBytes;
{$ENDIF}
begin
  oNode := nil;
  for i := 0 to CurrentElement.childNodes.length - 1 do
    if CurrentElement.childNodes.item[i].nodeType = CDATA_SECTION_NODE then
      oNode := CurrentElement.childNodes.item[i];
  if oNode = nil then
    FDException(Self, [S_FD_LStan], er_FD_StanStrgCantReadCDATA, [CurrentElement.nodeName]);
  sHex := (oNode as IDOMCDATASection).data;
{$IFDEF NEXTGEN}
  SetLength(aBuff, Length(sHex) div 2);
  HexToBin(BytesOf(sHex), 0, aBuff, 0, Length(aBuff));
  AStream.Size := 0;
  AStream.Write(aBuff, Length(aBuff));
{$ELSE}
  AStream.Size := Length(sHex) div 2;
  AStream.Position := 0;
  HexToBin(PChar(sHex), PAnsiChar(AStream.Memory), AStream.Size);
{$ENDIF}
end;

{-------------------------------------------------------------------------------}
function TFDXMLStorage.ReadProperty(const APropName: DOMString): DOMString;
begin
  if not TestProperty(APropName) then
    FDException(Self, [S_FD_LStan], er_FD_StanStrgCantReadProp, [APropName]);
  Result := CurrentElement.attributes.getNamedItem(EncodeName(APropName)).nodeValue;
end;

{-------------------------------------------------------------------------------}
procedure TFDXMLStorage.Close;
begin
  if IsOpen then
    try
      if FMode = smWrite then begin
        WriteObjectEnd('FDBS');
        FParser.SaveToStream(FStream);
      end
      else if TestObject('FDBS') then
        ReadObjectEnd('FDBS');
    finally
      FCurrentChildElement := nil;
      FCurrentElement := nil;
      FParser := nil;
      inherited Close;
    end;
end;

{-------------------------------------------------------------------------------}
function TFDXMLStorage.IsObjectEnd(const AObjectName: string): Boolean;
begin
  if (FCurrentElement = nil) or
     (CompareText(DecodeName(FCurrentElement.nodeName), AObjectName) <> 0) then
    FDException(Self, [S_FD_LStan], er_FD_StanStrgCantReadObj, [AObjectName]);
  Result := (FCurrentChildElement = nil) and (SkipText(FCurrentElement.firstChild) = nil) or
            (FCurrentChildElement <> nil) and (SkipText(FCurrentChildElement.nextSibling) = nil);
end;

{-------------------------------------------------------------------------------}
procedure TFDXMLStorage.Open(const AFileName: string; AStream: TStream; AMode: TFDStorageMode);
begin
  inherited Open(AFileName, AStream, AMode);
  FLevel := 0;
  FParser := TXMLDocument.Create(nil);
  FParser.Active := True;
  if AMode = smWrite then begin
    FParser.Version := '1.0';
    FParser.Encoding := 'utf-8';
    WriteObjectBegin('FDBS');
    WriteInteger('Version', StreamVersion, -1);
  end
  else begin
    FParser.LoadFromStream(FStream);
    if TestObject('FDBS') then begin
      ReadObjectBegin('FDBS');
      FStreamVersion := ReadInteger('Version', C_FD_StorageVer);
    end;
  end;
end;

{-------------------------------------------------------------------------------}
function TFDXMLStorage.ReadBoolean(const APropName: string; ADefValue: Boolean): Boolean;
begin
  if TestAndReadProperty(APropName) then
    Result := (CompareText(ReadProperty(APropName), S_FD_True) = 0)
  else
    Result := ADefValue;
end;

{-------------------------------------------------------------------------------}
function TFDXMLStorage.ReadDate(const APropName: string; ADefValue: TDateTime): TDateTime;
var
  s: string;
  i: Integer;
begin
  if TestAndReadProperty(APropName) then begin
    s := ReadProperty(APropName);
    if s <> '' then begin
      if FStreamVersion >= 9 then
        i := 1
      else
        i := 0;
      Result := FDEncodeDateTime(
        StrToInt(Copy(s, 1, 4)), StrToInt(Copy(s, 5, 2)), StrToInt(Copy(s, 7, 2)),
        StrToInt(Copy(s, 9 + i, 2)), StrToInt(Copy(s, 11 + i, 2)), StrToInt(Copy(s, 13 + i, 2)), 0);
    end
    else
      Result := ADefValue;
  end
  else
    Result := ADefValue;
end;

{-------------------------------------------------------------------------------}
function TFDXMLStorage.ReadFloat(const APropName: string; ADefValue: Double): Double;
var
  s: string;
begin
  if TestAndReadProperty(APropName) then begin
    s := ReadProperty(APropName);
    if s <> '' then
      Result := FDStr2Float(s, FDecimalSeparator)
    else
      Result := ADefValue;
  end
  else
    Result := ADefValue;
end;

{-------------------------------------------------------------------------------}
function TFDXMLStorage.ReadInteger(const APropName: string; ADefValue: Integer): Integer;
var
  s: string;
begin
  if TestAndReadProperty(APropName) then begin
    s := ReadProperty(APropName);
    if s <> '' then
      Result := StrToInt(s)
    else
      Result := ADefValue;
  end
  else
    Result := ADefValue;
end;

{-------------------------------------------------------------------------------}
function TFDXMLStorage.ReadLongWord(const APropName: string;
  ADefValue: LongWord): LongWord;
var
  s: string;
begin
  if TestAndReadProperty(APropName) then begin
    s := ReadProperty(APropName);
    if s <> '' then
      Result := StrToInt(s)
    else
      Result := ADefValue;
  end
  else
    Result := ADefValue;
end;

{-------------------------------------------------------------------------------}
function TFDXMLStorage.ReadInt64(const APropName: string; ADefValue: Int64): Int64;
var
  s: string;
begin
  if TestAndReadProperty(APropName) then begin
    s := ReadProperty(APropName);
    if s <> '' then
      Result := StrToInt64(s)
    else
      Result := ADefValue;
  end
  else
    Result := ADefValue;
end;

{-------------------------------------------------------------------------------}
function TFDXMLStorage.ReadObjectBegin(const AObjectName: string): String;
var
  oCurrElement: IDOMElement;
begin
  if AObjectName = '' then begin
    oCurrElement := GetXMLObject;
    if oCurrElement = nil then
      FDException(Self, [S_FD_LStan], er_FD_StanStrgCantReadObj, ['<unknown>']);
    Result := DecodeName(oCurrElement.nodeName);
  end
  else begin
    Result := AObjectName;
    if not TestObject(AObjectName) then
      FDException(Self, [S_FD_LStan], er_FD_StanStrgCantReadObj, [AObjectName]);
  end;
  if FCurrentElement = nil then
    FCurrentElement := FParser.DOMDocument.documentElement
  else begin
    if FCurrentChildElement = nil then
      FCurrentElement := IDOMElement(CurrentElement.firstChild)
    else begin
      FCurrentElement := IDOMElement(FCurrentChildElement.nextSibling);
      FCurrentChildElement := nil;
    end;
  end;
  FCurrentElement := SkipText(FCurrentElement);
end;

{-------------------------------------------------------------------------------}
procedure TFDXMLStorage.ReadObjectEnd(const AObjectName: string);
begin
  if CompareText(DecodeName(FCurrentElement.nodeName), AObjectName) <> 0 then
    FDException(Self, [S_FD_LStan], er_FD_StanStrgCantReadObj, [AObjectName]);
  FCurrentChildElement := FCurrentElement;
  FCurrentElement := IDOMElement(FCurrentElement.parentNode);
end;

{-------------------------------------------------------------------------------}
function TFDXMLStorage.ReadAnsiString(const APropName: string;
  const ADefValue: TFDAnsiString): TFDAnsiString;
begin
  if TestAndReadProperty(APropName) then
    Result := TFDAnsiString(ReadProperty(APropName))
  else
    Result := ADefValue;
end;

{-------------------------------------------------------------------------------}
function TFDXMLStorage.ReadWideString(const APropName: string;
  const ADefValue: UnicodeString): UnicodeString;
begin
  if TestAndReadProperty(APropName) then
    Result := ReadProperty(APropName)
  else
    Result := ADefValue;
end;

{-------------------------------------------------------------------------------}
function TFDXMLStorage.GetXMLObject: IDOMElement;
begin
  if FCurrentElement = nil then
    if FParser.DOMDocument = nil then
      Result := nil
    else
      Result := FParser.DOMDocument.documentElement
  else if FCurrentChildElement = nil then
    Result := IDOMElement(CurrentElement.firstChild)
  else
    Result := IDOMElement(FCurrentChildElement.nextSibling);
  Result := SkipText(Result);
end;

{-------------------------------------------------------------------------------}
function TFDXMLStorage.TestObject(const AObjectName: string): Boolean;
var
  oCurrElement: IDOMElement;
begin
  oCurrElement := GetXMLObject;
  Result := (oCurrElement <> nil) and
    (CompareText(DecodeName(oCurrElement.nodeName), AObjectName) = 0);
end;

{-------------------------------------------------------------------------------}
function TFDXMLStorage.TestProperty(const APropName: string): Boolean;
begin
  Result := CurrentElement.attributes.getNamedItem(EncodeName(APropName)) <> nil;
end;

{-------------------------------------------------------------------------------}
procedure TFDXMLStorage.WriteBoolean(const APropName: string; const AValue, ADefValue: Boolean);
begin
  if AValue <> ADefValue then
    if AValue then
      AddAttribute(APropName, S_FD_True)
    else
      AddAttribute(APropName, S_FD_False);
end;

{-------------------------------------------------------------------------------}
procedure TFDXMLStorage.WriteDate(const APropName: string; const AValue, ADefValue: TDateTime);
const
  CShortDateTimeFormat = 'yyyymmddhhnnss';
  CShortDateTimeISO8601Format = 'yyyymmddThhnnss';
begin
  if AValue <> ADefValue then
    if FStreamVersion >= 9 then
      AddAttribute(APropName, FormatDateTime(CShortDateTimeISO8601Format, AValue))
    else
      AddAttribute(APropName, FormatDateTime(CShortDateTimeFormat, AValue));
end;

{-------------------------------------------------------------------------------}
procedure TFDXMLStorage.WriteFloat(const APropName: string; const AValue, ADefValue: Double);
begin
  if Abs(AValue - ADefValue) >= 1e-14 then
    AddAttribute(APropName, FDFloat2Str(AValue, FDecimalSeparator));
end;

{-------------------------------------------------------------------------------}
procedure TFDXMLStorage.WriteInteger(const APropName: string; const AValue, ADefValue: Integer);
begin
  if AValue <> ADefValue then
    AddAttribute(APropName, IntToStr(AValue));
end;

{-------------------------------------------------------------------------------}
procedure TFDXMLStorage.WriteLongWord(const APropName: string; const AValue,
  ADefValue: LongWord);
begin
  if AValue <> ADefValue then
    AddAttribute(APropName, IntToStr(AValue));
end;

{-------------------------------------------------------------------------------}
procedure TFDXMLStorage.WriteInt64(const APropName: string; const AValue,
  ADefValue: Int64);
begin
  if AValue <> ADefValue then
    AddAttribute(APropName, IntToStr(AValue));
end;

{-------------------------------------------------------------------------------}
procedure TFDXMLStorage.WriteObjectBegin(const AObjectName: string);
begin
  Inc(FLevel);
  if FLevel > 1 then
    AddFormat;
  FCurrentElement := AddElement(AObjectName);
end;

{-------------------------------------------------------------------------------}
procedure TFDXMLStorage.WriteObjectEnd(const AObjectName: string);
begin
  if (FLevel >= 1) and (CurrentElement <> nil) and CurrentElement.hasChildNodes then
    AddFormat;
  Dec(FLevel);
  if (FCurrentElement <> nil) and (FCurrentElement.parentNode.nodeType = ELEMENT_NODE) then
    FCurrentElement := IDOMElement(FCurrentElement.parentNode)
  else
    FCurrentElement := nil;
end;

{-------------------------------------------------------------------------------}
procedure TFDXMLStorage.WriteAnsiString(const APropName: string; const AValue,
  ADefValue: TFDAnsiString);
begin
  if AValue <> ADefValue then
    AddAttribute(APropName, DOMString(AValue));
end;

{-------------------------------------------------------------------------------}
procedure TFDXMLStorage.WriteWideString(const APropName: string;
  const AValue, ADefValue: UnicodeString);
begin
  if AValue <> ADefValue then
    AddAttribute(APropName, DOMString(AValue));
end;

{-------------------------------------------------------------------------------}
procedure TFDXMLStorage.WriteValue(const APropName: String; APropIndex: Word;
  ADataType: TFDDataType; ABuff: Pointer; ALen: LongWord);
const
  CDateTimeFormat = '%.4d%.2d%.2d%.2d%.2d%.2d';
  CDateTimeISO8601Format = '%.4d%.2d%.2dT%.2d%.2d%.2d';
var
  sVal: DOMString;
  s: String;
  sSign: String;
  iLen: Integer;
  oMS: TFDReadSharedMemoryStream;
  oMS2: TMemoryStream;
  y, mo, d, h, mi, se, ms: Word;
  dt: TDateTime;
  pTS: PSQLTimeStamp;
  pInt: PFDSQLTimeInterval;
  rTS: TSQLTimeStamp;
begin
  if ABuff = nil then
    Exit;
  case ADataType of
  dtObject,
  dtRowSetRef,
  dtCursorRef,
  dtRowRef,
  dtArrayRef,
  dtParentRowRef:
    ;                                          
  dtMemo,
  dtHMemo,
  dtWideMemo,
  dtXML,
  dtWideHMemo,
  dtByteString,
  dtBlob,
  dtHBlob,
  dtHBFile:
    begin
      oMS := TFDReadSharedMemoryStream.Create;
      try
        WriteObjectBegin(APropName);
        try
          if ADataType in [dtWideMemo, dtXML, dtWideHMemo] then
            ALen := ALen * SizeOf(WideChar);
          oMS.SetData(ABuff, ALen);
          Inc(FLevel);
          try
            AddFormat;
            AddCData(oMS);
          finally
            Dec(FLevel);
          end;
        finally
          WriteObjectEnd(APropName);
        end;
      finally
        FDFree(oMS);
      end;
    end;
  dtBoolean:
    AddAttribute(APropName, S_FD_Bools[PWord(ABuff)^ <> 0]);
  dtSByte:
    AddAttribute(APropName, IntToStr(PShortInt(ABuff)^));
  dtInt16:
    AddAttribute(APropName, IntToStr(PSmallInt(ABuff)^));
  dtInt32:
    AddAttribute(APropName, IntToStr(PInteger(ABuff)^));
  dtInt64:
    AddAttribute(APropName, IntToStr(PInt64(ABuff)^));
  dtByte:
    AddAttribute(APropName, IntToStr(PByte(ABuff)^));
  dtUInt16:
    AddAttribute(APropName, IntToStr(PWord(ABuff)^));
  dtUInt32:
    AddAttribute(APropName, IntToStr(PLongWord(ABuff)^));
  dtUInt64:
    AddAttribute(APropName, IntToStr(PUInt64(ABuff)^));
  dtSingle:
    AddAttribute(APropName, FDFloat2Str(PSingle(ABuff)^, FDecimalSeparator));
  dtDouble:
    AddAttribute(APropName, FDFloat2Str(PDouble(ABuff)^, FDecimalSeparator));
  dtExtended:
    AddAttribute(APropName, FDFloat2Str(PExtended(ABuff)^, FDecimalSeparator));
  dtCurrency:
    AddAttribute(APropName, FDCurr2Str(PCurrency(ABuff)^, FDecimalSeparator));
  dtBCD,
  dtFmtBCD:
    begin
      oMS2 := CheckBuffer(C_FD_MaxFixedSize);
      FDBCD2Str(PChar(oMS2.Memory), iLen, PBcd(ABuff)^, FDecimalSeparator);
      SetString(sVal, PChar(oMS2.Memory), iLen);
      AddAttribute(APropName, sVal);
    end;
  dtDateTime:
    begin
      dt := TimeStampToDateTime(MSecsToTimeStamp(PDateTimeRec(ABuff)^.DateTime));
      DecodeDate(dt, y, mo, d);
      DecodeTime(dt, h, mi, se, ms);
      if FStreamVersion >= 9 then
        sVal := Format(CDateTimeISO8601Format, [y, mo, d, h, mi, se])
      else
        sVal := Format(CDateTimeFormat, [y, mo, d, h, mi, se]);
      AddAttribute(APropName, sVal);
    end;
  dtDateTimeStamp:
    begin
      pTS := PSQLTimeStamp(ABuff);
      if FStreamVersion >= 9 then
        AddAttribute(APropName, Format(CDateTimeISO8601Format, [pTS^.Year,
          pTS^.Month, pTS^.Day, pTS^.Hour, pTS^.Minute, pTS^.Second]))
      else
        AddAttribute(APropName, Format(CDateTimeFormat, [pTS^.Year,
          pTS^.Month, pTS^.Day, pTS^.Hour, pTS^.Minute, pTS^.Second]));
    end;
  dtTimeIntervalFull,
  dtTimeIntervalYM,
  dtTimeIntervalDS:
    begin
      pInt := PFDSQLTimeInterval(ABuff);
      if pInt^.Sign < 0 then
        sSign := '-'
      else
        sSign := '';
      case pInt^.Kind of
      itYear:          sVal := Format('%sP%uY', [sSign, pInt^.Years]);
      itMonth:         sVal := Format('%sP%uM', [sSign, pInt^.Months]);
      itDay:           sVal := Format('%sP%uD', [sSign, pInt^.Days]);
      itHour:          sVal := Format('%sT%uH', [sSign, pInt^.Hours]);
      itMinute:        sVal := Format('%sT%uM', [sSign, pInt^.Minutes]);
      itSecond:        sVal := Format('%sT%uS%uF', [sSign, pInt^.Seconds, pInt^.Fractions]);
      itYear2Month:    sVal := Format('%sP%uY%uM', [sSign, pInt^.Years, pInt^.Months]);
      itDay2Hour:      sVal := Format('%sP%uDT%uH', [sSign, pInt^.Days, pInt^.Hours]);
      itDay2Minute:    sVal := Format('%sP%uDT%uH%uM', [sSign, pInt^.Days, pInt^.Hours, pInt^.Minutes]);
      itDay2Second:    sVal := Format('%sP%uDT%uH%uM%uS%uF', [sSign, pInt^.Days, pInt^.Hours, pInt^.Minutes, pInt^.Seconds, pInt^.Fractions]);
      itHour2Minute:   sVal := Format('%sT%uH%uM', [sSign, pInt^.Hours, pInt^.Minutes]);
      itHour2Second:   sVal := Format('%sT%uH%uM%uS%uF', [sSign, pInt^.Hours, pInt^.Minutes, pInt^.Seconds, pInt^.Fractions]);
      itMinute2Second: sVal := Format('%sT%uM%uS%uF', [sSign, pInt^.Minutes, pInt^.Seconds, pInt^.Fractions]);
      end;
      AddAttribute(APropName, sVal);
    end;
  dtTime:
    begin
      rTS := FDTime2SQLTimeStamp(PLongint(ABuff)^);
      AddAttribute(APropName, Format('%.2d%.2d%.2d', [rTS.Hour, rTS.Minute, rTS.Second]));
    end;
  dtDate:
    begin
      rTS := FDDate2SQLTimeStamp(PLongint(ABuff)^);
      AddAttribute(APropName, Format('%.4d%.2d%.2d', [rTS.Year, rTS.Month, rTS.Day]));
    end;
  dtAnsiString:
    begin
      s := FEncoder.Decode(ABuff, ALen, ecANSI);
      AddAttribute(APropName, DOMString(s));
    end;
  dtWideString:
    begin
      SetString(sVal, PWideChar(ABuff), ALen);
      AddAttribute(APropName, sVal);
    end;
  dtGUID:
    AddAttribute(APropName, GUIDToString(PGUID(ABuff)^));
  end;
end;

{-------------------------------------------------------------------------------}
function TFDXMLStorage.ReadValue(const APropName: String; APropIndex: Word;
  ADataType: TFDDataType; out ABuff: Pointer; out ALen: LongWord): Boolean;
var
  oItem: IDOMNode;
  sName: DOMString;
  sVal: String;
  sb: TFDByteString;
  sw: UnicodeString;
  oMS: TMemoryStream;
  rTS: TSQLTimeStamp;
  pCh: PChar;
  i, iSign: Integer;
  eIntKind: TFDSQLTimeIntervalKind;
  pTS: PSQLTimeStamp;
  pInt: PFDSQLTimeInterval;

  function ScanInt(var AVal: LongWord; AType: Char): Boolean;
  var
    pStCh: PChar;
  begin
    if pCh^ = '-' then begin
      iSign := -1;
      Inc(pCh);
    end
    else if pCh^ = '+' then
      Inc(pCh);
    pStCh := pCh;
    while (pCh^ >= '0') and (pCh^ <= '9') do
      Inc(pCh);
    if pCh^ = AType then begin
      FDStr2Int(pCh, pCh - pStCh, @AVal, SizeOf(AVal), True);
      Result := True;
    end
    else begin
      pCh := pStCh;
      Result := False;
    end;
  end;

  procedure DateIntError;
  begin
    FDException(Self, [S_FD_LStan], er_FD_StanStrgUnknownFmt, [sVal]);
  end;

begin
  ABuff := nil;
  ALen := 0;
  Result := False;
  case ADataType of
  dtObject,
  dtRowSetRef,
  dtCursorRef,
  dtRowRef,
  dtArrayRef,
  dtParentRowRef:
    ;                                         
  dtMemo,
  dtHMemo,
  dtWideMemo,
  dtXML,
  dtWideHMemo,
  dtByteString,
  dtBlob,
  dtHBlob,
  dtHBFile:
    if TestObject(APropName) then begin
      ReadObjectBegin(APropName);
      try
        oMS := CheckBuffer(0);
        ReadCData(oMS);
        ABuff := oMS.Memory;
        ALen := oMS.Size;
        if ADataType in [dtWideMemo, dtXML, dtWideHMemo] then
          ALen := ALen div SizeOf(WideChar);
        Result := True;
      finally
        ReadObjectEnd(APropName);
      end;
    end;
  else
    sName := EncodeName(APropName);
    oItem := CurrentElement.attributes.getNamedItem(sName);
    if oItem = nil then
      Exit;
    sVal := String(oItem.nodeValue);
    ABuff := CheckBuffer(C_FD_MaxFixedSize).Memory;
    Result := True;
    case ADataType of
    dtBoolean:
      PWordBool(ABuff)^ := CompareText(S_FD_True, sVal) = 0;
    dtSByte:
      PShortInt(ABuff)^ := StrToInt(sVal);
    dtInt16:
      PSmallInt(ABuff)^ := StrToInt(sVal);
    dtInt32:
      PInteger(ABuff)^ := StrToInt(sVal);
    dtInt64:
      PInt64(ABuff)^ := StrToInt64(sVal);
    dtByte:
      PByte(ABuff)^ := StrToInt(sVal);
    dtUInt16:
      PWord(ABuff)^ := StrToInt(sVal);
    dtUInt32:
      PLongWord(ABuff)^ := LongWord(StrToInt64(sVal));
    dtUInt64:
      PUInt64(ABuff)^ := StrToInt64(sVal);
    dtSingle:
      PSingle(ABuff)^ := FDStr2Float(sVal, FDecimalSeparator);
    dtDouble:
      PDouble(ABuff)^ := FDStr2Float(sVal, FDecimalSeparator);
    dtExtended:
      PExtended(ABuff)^ := FDStr2Float(sVal, FDecimalSeparator);
    dtCurrency:
      PCurrency(ABuff)^ := FDStr2Curr(sVal, FDecimalSeparator);
    dtBCD,
    dtFmtBCD:
      FDStr2BCD(PChar(sVal), Length(sVal), PBcd(ABuff)^, Char(FDecimalSeparator));
    dtDateTime:
      begin
        if FStreamVersion >= 9 then
          i := 1
        else
          i := 0;
        FDStr2Int(PChar(sVal) + 0, 4, @rTS.Year, SizeOf(rTS.Year), True);
        FDStr2Int(PChar(sVal) + 4, 2, @rTS.Month, SizeOf(rTS.Month), True);
        FDStr2Int(PChar(sVal) + 6, 2, @rTS.Day, SizeOf(rTS.Day), True);
        if (i = 0) or ((PChar(sVal) + 8)^ = 'T') then begin
          FDStr2Int(PChar(sVal) + 8 + i, 2, @rTS.Hour, SizeOf(rTS.Hour), True);
          FDStr2Int(PChar(sVal) + 10 + i, 2, @rTS.Minute, SizeOf(rTS.Minute), True);
          FDStr2Int(PChar(sVal) + 12 + i, 2, @rTS.Second, SizeOf(rTS.Second), True);
        end
        else begin
          rTS.Hour := 0;
          rTS.Minute := 0;
          rTS.Second := 0;
        end;
        rTS.Fractions := 0;
        PDateTimeRec(ABuff)^.DateTime := FDDateTime2MSecs(FDSQLTimeStamp2DateTime(rTS));
      end;
    dtDateTimeStamp:
      begin
        if FStreamVersion >= 9 then
          i := 1
        else
          i := 0;
        pTS := PSQLTimeStamp(ABuff);
        FDStr2Int(PChar(sVal) + 0, 4, @pTS^.Year, SizeOf(pTS^.Year), True);
        FDStr2Int(PChar(sVal) + 4, 2, @pTS^.Month, SizeOf(pTS^.Month), True);
        FDStr2Int(PChar(sVal) + 6, 2, @pTS^.Day, SizeOf(pTS^.Day), True);
        if (i = 0) or ((PChar(sVal) + 8)^ = 'T') then begin
          FDStr2Int(PChar(sVal) + 8 + i, 2, @pTS^.Hour, SizeOf(pTS^.Hour), True);
          FDStr2Int(PChar(sVal) + 10 + i, 2, @pTS^.Minute, SizeOf(pTS^.Minute), True);
          FDStr2Int(PChar(sVal) + 12 + i, 2, @pTS^.Second, SizeOf(pTS^.Second), True);
        end
        else begin
          pTS^.Hour := 0;
          pTS^.Minute := 0;
          pTS^.Second := 0;
        end;
        pTS^.Fractions := 0;
      end;
    dtTimeIntervalFull,
    dtTimeIntervalYM,
    dtTimeIntervalDS:
      begin
        pInt := PFDSQLTimeInterval(ABuff);
        pCh := PChar(sVal);
        if pCh^ = '-' then begin
          iSign := -1;
          Inc(pCh);
        end
        else
          iSign := 1;
        eIntKind := itUnknown;
        if pCh^ <> 'P' then
          DateIntError;
        Inc(pCh); // P
        if (pCh^ <> 'T') and ScanInt(pInt^.Years, 'Y') then
          eIntKind := itYear;
        if (pCh^ <> 'T') and ScanInt(pInt^.Months, 'M') then
          if eIntKind = itYear then
            eIntKind := itYear2Month
          else
            eIntKind := itMonth;
        if (pCh^ <> 'T') and ScanInt(pInt^.Days, 'D') then begin
          if eIntKind <> itUnknown then
            DateIntError;
          eIntKind := itDay;
        end;
        if pCh^ = 'T' then begin
          Inc(pCh); // T
          if ScanInt(pInt^.Hours, 'H') then
            if eIntKind = itDay then
              eIntKind := itDay2Hour
            else if eIntKind = itUnknown then
              eIntKind := itHour
            else
              DateIntError;
          if ScanInt(pInt^.Minutes, 'M') then
            if eIntKind = itDay2Hour then
              eIntKind := itDay2Minute
            else if eIntKind = itHour then
              eIntKind := itHour2Minute
            else if eIntKind = itUnknown then
              eIntKind := itMinute
            else
              DateIntError;
          if ScanInt(pInt^.Seconds, 'S') then begin
            if eIntKind = itDay2Minute then
              eIntKind := itDay2Second
            else if eIntKind = itHour2Minute then
              eIntKind := itHour2Second
            else if eIntKind = itMinute then
              eIntKind := itMinute2Second
            else if eIntKind = itUnknown then
              eIntKind := itSecond
            else
              DateIntError;
            ScanInt(pInt^.Fractions, 'F');
          end;
        end;
        pInt^.Sign := iSign;
        pInt^.Kind := eIntKind;
      end;
    dtTime:
      begin
        rTS.Year := 0;
        rTS.Month := 0;
        rTS.Day := 0;
        FDStr2Int(PChar(sVal) + 0, 2, @rTS.Hour, SizeOf(rTS.Hour), True);
        FDStr2Int(PChar(sVal) + 2, 2, @rTS.Minute, SizeOf(rTS.Minute), True);
        FDStr2Int(PChar(sVal) + 4, 2, @rTS.Second, SizeOf(rTS.Second), True);
        rTS.Fractions := 0;
        PLongint(ABuff)^ := FDSQLTimeStamp2Time(rTS);
      end;
    dtDate:
      begin
        FDStr2Int(PChar(sVal) + 0, 4, @rTS.Year, SizeOf(rTS.Year), True);
        FDStr2Int(PChar(sVal) + 4, 2, @rTS.Month, SizeOf(rTS.Month), True);
        FDStr2Int(PChar(sVal) + 6, 2, @rTS.Day, SizeOf(rTS.Day), True);
        rTS.Hour := 0;
        rTS.Minute := 0;
        rTS.Second := 0;
        rTS.Fractions := 0;
        PLongint(ABuff)^ := FDSQLTimeStamp2Date(rTS);
      end;
    dtAnsiString:
      begin
        sb := FEncoder.Encode(sVal, ecANSI);
        ALen := FDByteStrLength(sb);
        ABuff := CheckBuffer(ALen * SizeOf(Byte)).Memory;
        Move(PByte(sb)^, PByte(ABuff)^, ALen * SizeOf(Byte));
      end;
    dtWideString:
      begin
        sw := oItem.nodeValue;
        ALen := Length(sw);
        ABuff := CheckBuffer(ALen * SizeOf(WideChar)).Memory;
        Move(PWideChar(sw)^, PWideChar(ABuff)^, ALen * SizeOf(WideChar));
      end;
    dtGUID:
      PGUID(ABuff)^ := StringToGUID(sVal);
    end;
  end;
end;

{-------------------------------------------------------------------------------}
function TFDXMLStorage.ReadEnum(APropName: string; ATypeInfo: PTypeInfo;
  ADefValue: Integer): Integer;
begin
  Result := GetEnumValue(ATypeInfo, Copy(GetEnumName(ATypeInfo, Integer(ADefValue)), 1, 2) +
    ReadString(APropName, Copy(GetEnumName(ATypeInfo, Integer(ADefValue)), 3, MAXINT)));
end;

{-------------------------------------------------------------------------------}
procedure TFDXMLStorage.WriteEnum(APropName: string; ATypeInfo: PTypeInfo;
  const AValue, ADefValue: Integer);
begin
  WriteString(APropName, Copy(GetEnumName(ATypeInfo, AValue), 3, MAXINT),
    Copy(GetEnumName(ATypeInfo, ADefValue), 3, MAXINT));
end;

{-------------------------------------------------------------------------------}
class function TFDXMLStorage.CheckExtention(const AExt: String): Boolean;
begin
  Result := CompareText(AExt, '.XML') = 0;
end;

{-------------------------------------------------------------------------------}
class function TFDXMLStorage.CheckFormat(AFmt: TFDStorageFormat): Boolean;
begin
  Result := AFmt = sfXML;
end;

{-------------------------------------------------------------------------------}
type
  TFDXMLStorageBmk = class(TObject)
    FCurrentChildElement: IDOMElement;
    FCurrentElement: IDOMElement;
  end;

function TFDXMLStorage.GetBookmark: TObject;
begin
  Result := TFDXMLStorageBmk.Create;
  TFDXMLStorageBmk(Result).FCurrentElement := FCurrentElement;
  TFDXMLStorageBmk(Result).FCurrentChildElement := FCurrentChildElement;
end;

{-------------------------------------------------------------------------------}
procedure TFDXMLStorage.SetBookmark(const AValue: TObject);
begin
  FCurrentElement := TFDXMLStorageBmk(AValue).FCurrentElement;
  FCurrentChildElement := TFDXMLStorageBmk(AValue).FCurrentChildElement;
end;
{$ENDIF}

{$IFDEF FireDAC_STRG_BIN}
{-------------------------------------------------------------------------------}
{ TFDBinStorage                                                                 }
{-------------------------------------------------------------------------------}
type
  TFDBinStorageHeader = record
    FMagic: array[0..3] of TFDAnsiChar;
    FVersion: Word;
    FDictionaryOffset: LongWord;
  end;

const
  C_ObjBegin = 255;
  C_ObjEnd = 254;

constructor TFDBinStorage.Create(AResOpts: TFDBottomResourceOptions; AEncoder: TFDEncoder);
begin
  inherited Create(AResOpts, AEncoder);
  FDictionary := TFDStringList.Create;
end;

{-------------------------------------------------------------------------------}
destructor TFDBinStorage.Destroy;
begin
  inherited Destroy;
  FDFreeAndNil(FDictionary);
end;

{-------------------------------------------------------------------------------}
procedure TFDBinStorage.Open(const AFileName: string; AStream: TStream; AMode: TFDStorageMode);
var
  rHdr: TFDBinStorageHeader;
  iLen: Word;
  s: UnicodeString;
begin
  inherited Open(AFileName, AStream, AMode);
  if FMode = smWrite then begin
    FillChar(rHdr, SizeOf(rHdr), 0);
    FStream.Write(rHdr, SizeOf(rHdr));
    FDictionary.Sorted := True;
    FDictionaryIndex := 0;
  end
  else begin
    if (FStream.Read(rHdr, SizeOf(rHdr)) <> SizeOf(rHdr)) or
       not ((rHdr.FMagic[0] = TFDAnsiChar('A')) and (rHdr.FMagic[1] = TFDAnsiChar('D')) and
            (rHdr.FMagic[2] = TFDAnsiChar('B')) and (rHdr.FMagic[3] = TFDAnsiChar('S'))) then
      FDException(Self, [S_FD_LStan], er_FD_StanStrgInvBinFmt, []);
    FStreamVersion := rHdr.FVersion;
    FStream.Seek(rHdr.FDictionaryOffset, soBeginning);
    FDictionary.Sorted := False;
    iLen := 0;
    while FStream.Read(iLen, SizeOf(iLen)) = SizeOf(iLen) do begin
      SetLength(s, iLen div SizeOf(WideChar));
      if iLen > 0 then
        FStream.Read(s[1], iLen);
      FDictionary.Add(s);
    end;
    FStream.Seek(SizeOf(rHdr), soBeginning);
  end;
end;

{-------------------------------------------------------------------------------}
function DoCompareObjects(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := Integer(TFDStringList(List).Ints[Index1]) -
    Integer(TFDStringList(List).Ints[Index2]);
end;

procedure TFDBinStorage.Close;
var
  i: Integer;
  iLen: Word;
  iPos: int64;
  rHdr: TFDBinStorageHeader;
  s: UnicodeString;
begin
  if (FStream <> nil) and (FMode = smWrite) then begin
    iPos := FStream.Position;
    FDictionary.Sorted := False;
    FDictionary.CustomSort(DoCompareObjects);
    for i := 0 to FDictionary.Count - 1 do begin
      iLen := Word(Length(FDictionary[i]) * SizeOf(WideChar));
      FStream.Write(iLen, SizeOf(iLen));
      if iLen > 0 then begin
        s := FDictionary[i];
        FStream.Write(s[1], iLen);
      end;
    end;
    rHdr.FMagic[0] := TFDAnsiChar('A');
    rHdr.FMagic[1] := TFDAnsiChar('D');
    rHdr.FMagic[2] := TFDAnsiChar('B');
    rHdr.FMagic[3] := TFDAnsiChar('S');
    rHdr.FVersion := FStreamVersion;
    rHdr.FDictionaryOffset := LongWord(iPos);
    FStream.Seek(0, soBeginning);
    FStream.Write(rHdr, SizeOf(rHdr));
  end;
  FDictionary.Clear;
  inherited Close;
end;

{-------------------------------------------------------------------------------}
procedure TFDBinStorage.InternalReadProperty(out AName: String);
var
  iDict: Word;
begin
  iDict := 0;
  FStream.Read(iDict, SizeOf(iDict));
  if (iDict and $FF) >= C_ObjEnd then
    AName := ''
  else
    AName := FDictionary[iDict];
end;

{-------------------------------------------------------------------------------}
function TFDBinStorage.TestProperty(const APropName: string): Boolean;
var
  iPos: Int64;
  sName: String;
begin
  iPos := FStream.Position;
  try
    InternalReadProperty(sName);
    Result := SameText(sName, APropName);
  finally
    FStream.Position := iPos;
  end;
end;

{-------------------------------------------------------------------------------}
function TFDBinStorage.TestAndReadProperty(const APropName: string): Boolean;
var
  iPos: Int64;
  sName: String;
begin
  iPos := FStream.Position;
  Result := False;
  try
    InternalReadProperty(sName);
    Result := SameText(sName, APropName);
  finally
    if not Result then
      FStream.Position := iPos;
  end;
end;

{-------------------------------------------------------------------------------}
function TFDBinStorage.ReadBoolean(const APropName: string;
  ADefValue: Boolean): Boolean;
begin
  if TestAndReadProperty(APropName) then
    FStream.Read(Result, SizeOf(Result))
  else
    Result := ADefValue;
end;

{-------------------------------------------------------------------------------}
function TFDBinStorage.ReadDate(const APropName: string;
  ADefValue: TDateTime): TDateTime;
begin
  if TestAndReadProperty(APropName) then
    FStream.Read(Result, SizeOf(Result))
  else
    Result := ADefValue;
end;

{-------------------------------------------------------------------------------}
function TFDBinStorage.ReadEnum(APropName: string; ATypeInfo: PTypeInfo;
  ADefValue: Integer): Integer;
var
  sStr: String;
  iDict: Word;
begin
  if FStreamVersion >= 5 then begin
    if TestAndReadProperty(APropName) then begin
      FStream.Read(iDict, SizeOf(iDict));
      Result := GetEnumValue(ATypeInfo, FDictionary[iDict]);
    end
    else
      Result := ADefValue;
  end
  else if FStreamVersion >= 3 then begin
    sStr := ReadString(APropName, '');
    if sStr = '' then
      Result := ADefValue
    else
      Result := GetEnumValue(ATypeInfo, Copy(GetEnumName(ATypeInfo, Integer(ADefValue)), 1, 2) + sStr);
  end
  else begin
    if TestAndReadProperty(APropName) then begin
      FStream.Read(Result, SizeOf(Result));
      if (FStreamVersion = 1) and (ATypeInfo = TypeInfo(TFDDataType)) then begin
        if Result >= Integer(dtXML) then
          Inc(Result);
        if Result >= Integer(dtTimeIntervalFull) then
          Inc(Result, 3);
        if Result >= Integer(dtExtended) then
          Inc(Result);
        if Result >= Integer(dtSingle) then
          Inc(Result);
      end;
    end
    else
      Result := ADefValue;
  end;
end;

{-------------------------------------------------------------------------------}
function TFDBinStorage.ReadFloat(const APropName: string;
  ADefValue: Double): Double;
begin
  if TestAndReadProperty(APropName) then
    FStream.Read(Result, SizeOf(Result))
  else
    Result := ADefValue;
end;

{-------------------------------------------------------------------------------}
function TFDBinStorage.ReadInteger(const APropName: string;
  ADefValue: Integer): Integer;
begin
  if TestAndReadProperty(APropName) then
    FStream.Read(Result, SizeOf(Result))
  else
    Result := ADefValue;
end;

{-------------------------------------------------------------------------------}
function TFDBinStorage.ReadLongWord(const APropName: string;
  ADefValue: LongWord): LongWord;
begin
  if TestAndReadProperty(APropName) then
    FStream.Read(Result, SizeOf(Result))
  else
    Result := ADefValue;
end;

{-------------------------------------------------------------------------------}
function TFDBinStorage.ReadInt64(const APropName: string; ADefValue: Int64): Int64;
begin
  if TestAndReadProperty(APropName) then
    FStream.Read(Result, SizeOf(Result))
  else
    Result := ADefValue;
end;

{-------------------------------------------------------------------------------}
function TFDBinStorage.TestObject(const AObjectName: string): Boolean;
var
  sName: String;
  iPos: Int64;
begin
  iPos := FStream.Position;
  try
    Result := InternalReadObject(sName);
    if Result then
      Result := SameText(AObjectName, sName);
  finally
    FStream.Position := iPos;
  end;
end;

{-------------------------------------------------------------------------------}
function TFDBinStorage.ReadObjectBegin(const AObjectName: string): String;
var
  sName: String;
begin
  if not InternalReadObject(sName) then
    FDException(Self, [S_FD_LStan], er_FD_StanStrgCantReadObj, ['<unknown>']);
  if AObjectName = '' then
    Result := sName
  else begin
    Result := AObjectName;
    if not SameText(sName, AObjectName) then
      FDException(Self, [S_FD_LStan], er_FD_StanStrgCantReadObj, [AObjectName]);
  end;
end;

{-------------------------------------------------------------------------------}
function TFDBinStorage.InternalReadObject(out AName: String): Boolean;
var
  cObjMagic: Byte;
  iDict: Byte;
begin
  Result := (FStream.Read(cObjMagic, SizeOf(Byte)) = SizeOf(Byte)) and (cObjMagic = C_ObjBegin);
  if Result then begin
    FStream.Read(iDict, SizeOf(iDict));
    AName := FDictionary[iDict];
  end
  else
    AName := '';
end;

{-------------------------------------------------------------------------------}
function TFDBinStorage.InternalReadObjectEnd: Boolean;
var
  cObjMagic: Byte;
begin
  Result := (FStream.Read(cObjMagic, SizeOf(Byte)) = SizeOf(Byte)) and (cObjMagic = C_ObjEnd);
end;

{-------------------------------------------------------------------------------}
function TFDBinStorage.IsObjectEnd(const AObjectName: string): Boolean;
var
  iPos: Int64;
begin
  iPos := FStream.Position;
  try
    Result := InternalReadObjectEnd;
  finally
    FStream.Position := iPos;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDBinStorage.ReadObjectEnd(const AObjectName: string);
begin
  InternalReadObjectEnd;
end;

{-------------------------------------------------------------------------------}
function TFDBinStorage.ReadAnsiString(const APropName: string;
  const ADefValue: TFDAnsiString): TFDAnsiString;
var
  iLen: LongWord;
begin
  if TestAndReadProperty(APropName) then begin
    FStream.Read(iLen, SizeOf(iLen));
    SetLength(Result, iLen div SizeOf(TFDAnsiChar));
    if iLen <> 0 then
      FStream.Read(PFDAnsiString(Result)^, iLen);
  end
  else
    Result := ADefValue;
end;

{-------------------------------------------------------------------------------}
function TFDBinStorage.ReadWideString(const APropName: string;
  const ADefValue: UnicodeString): UnicodeString;
var
  iLen: LongWord;
begin
  if TestAndReadProperty(APropName) then begin
    FStream.Read(iLen, SizeOf(iLen));
    SetLength(Result, iLen div SizeOf(WideChar));
    if iLen <> 0 then
      FStream.Read(Result[1], iLen);
  end
  else
    Result := ADefValue;
end;

{-------------------------------------------------------------------------------}
function TFDBinStorage.ReadValue(const APropName: String; APropIndex: Word;
  ADataType: TFDDataType; out ABuff: Pointer; out ALen: LongWord): Boolean;
var
  oMS: TMemoryStream;
  iProp: Word;
  iLen: Byte;
  iBmk: Int64;
begin
  ABuff := nil;
  ALen := 0;
  Result := False;
  if FStreamVersion >= 4 then begin
    iBmk := FStream.Position;
    FStream.Read(iProp, SizeOf(iProp));
    if iProp <> APropIndex then begin
      FStream.Position := iBmk;
      Exit;
    end;
  end
  else begin
    if not TestAndReadProperty(APropName) then
      Exit;
  end;
  Result := True;
  case ADataType of
  dtObject,
  dtRowSetRef,
  dtCursorRef,
  dtRowRef,
  dtArrayRef,
  dtParentRowRef:
    ALen := 0;                                         
  dtMemo,
  dtHMemo,
  dtWideMemo,
  dtXML,
  dtWideHMemo,
  dtByteString,
  dtBlob,
  dtHBlob,
  dtHBFile,
  dtAnsiString,
  dtWideString:
    FStream.Read(ALen, SizeOf(ALen));
  dtBoolean:
    ALen := SizeOf(WordBool);
  dtSByte:
    ALen := SizeOf(ShortInt);
  dtInt16:
    ALen := SizeOf(SmallInt);
  dtInt32:
    ALen := SizeOf(Integer);
  dtInt64:
    ALen := SizeOf(Int64);
  dtByte:
    ALen := SizeOf(Byte);
  dtUInt16:
    ALen := SizeOf(Word);
  dtUInt32:
    ALen := SizeOf(LongWord);
  dtUInt64:
    ALen := SizeOf(UInt64);
  dtSingle:
    ALen := SizeOf(Single);
  dtDouble:
    ALen := SizeOf(Double);
  dtExtended:
    ALen := SizeOf(Extended);
  dtCurrency:
    ALen := SizeOf(Currency);
  dtBCD,
  dtFmtBCD:
    if FStreamVersion >= 4 then begin
      FStream.Read(iLen, SizeOf(iLen));
      ALen := SizeOf(TBcd);
      oMS := CheckBuffer(ALen);
      ABuff := oMS.Memory;
      if iLen <> 0 then
        FStream.Read(ABuff^, iLen);
      FillChar((PByte(ABuff) + iLen)^, ALen - iLen, 0);
      Exit;
    end
    else
      ALen := SizeOf(TBcd);
  dtDateTime:
    ALen := SizeOf(TDateTimeAlias);
  dtDateTimeStamp:
    ALen := SizeOf(TSQLTimeStamp);
  dtTimeIntervalFull,
  dtTimeIntervalYM,
  dtTimeIntervalDS:
    ALen := SizeOf(TFDSQLTimeInterval);
  dtTime:
    ALen := SizeOf(LongInt);
  dtDate:
    ALen := SizeOf(LongInt);
  dtGUID:
    ALen := SizeOf(TGUID);
  end;
  oMS := CheckBuffer(ALen);
  if ALen <> 0 then
    FStream.Read(oMS.Memory^, ALen);
  ABuff := oMS.Memory;
  if ADataType in [dtWideMemo, dtXML, dtWideHMemo, dtWideString] then
    ALen := ALen div SizeOf(WideChar);
end;

{-------------------------------------------------------------------------------}
function TFDBinStorage.LookupName(const AName: String): Word;
var
  i: Integer;
begin
  i := FDictionary.IndexOf(AName);
  if i = -1 then begin
    if FDictionaryIndex >= $FFFF then
      FDException(Self, [S_FD_LStan], er_FD_StanStrgDictOverflow, []);
    FDictionary.AddInt(AName, LongWord(FDictionaryIndex));
    Result := Word(FDictionaryIndex);
    Inc(FDictionaryIndex);
    if FDictionaryIndex = C_ObjEnd then begin
      LookupName('<end>');
      LookupName('<begin>');
    end;
  end
  else
    Result := Word(LongWord(FDictionary.Ints[i]));
end;

{-------------------------------------------------------------------------------}
procedure TFDBinStorage.InternalWriteProperty(const APropName: string;
  ApValue: Pointer; ALen: LongWord);
var
  iDict: Word;
begin
  iDict := LookupName(APropName);
  FStream.Write(iDict, SizeOf(iDict));
  if ApValue <> nil then
    FStream.Write(ApValue^, ALen);
end;

{-------------------------------------------------------------------------------}
procedure TFDBinStorage.WriteBoolean(const APropName: string; const AValue,
  ADefValue: Boolean);
begin
  if AValue <> ADefValue then
    InternalWriteProperty(APropName, @AValue, SizeOf(AValue));
end;

{-------------------------------------------------------------------------------}
procedure TFDBinStorage.WriteDate(const APropName: string; const AValue,
  ADefValue: TDateTime);
begin
  if AValue <> ADefValue then
    InternalWriteProperty(APropName, @AValue, SizeOf(AValue));
end;

{-------------------------------------------------------------------------------}
procedure TFDBinStorage.WriteEnum(APropName: string; ATypeInfo: PTypeInfo;
  const AValue, ADefValue: Integer);
var
  iVal: Integer;
  iDict: Word;
begin
  if AValue <> ADefValue then
    if FStreamVersion <= 2 then begin
      iVal := AValue;
      if (FStreamVersion = 1) and (ATypeInfo = TypeInfo(TFDDataType)) then begin
        if iVal > Integer(dtWideMemo) then
          Dec(iVal);
        if iVal > Integer(dtDateTimeStamp) then
          Dec(iVal, 3);
      end;
      InternalWriteProperty(APropName, @iVal, SizeOf(iVal));
    end
    else if FStreamVersion <= 4 then
      WriteString(APropName, Copy(GetEnumName(ATypeInfo, AValue), 3, MAXINT),
        Copy(GetEnumName(ATypeInfo, ADefValue), 3, MAXINT))
    else begin
      iDict := LookupName(GetEnumName(ATypeInfo, AValue));
      InternalWriteProperty(APropName, @iDict, SizeOf(iDict));
    end;
end;

{-------------------------------------------------------------------------------}
procedure TFDBinStorage.WriteFloat(const APropName: string; const AValue,
  ADefValue: Double);
begin
  if AValue <> ADefValue then
    InternalWriteProperty(APropName, @AValue, SizeOf(AValue));
end;

{-------------------------------------------------------------------------------}
procedure TFDBinStorage.WriteInteger(const APropName: string; const AValue,
  ADefValue: Integer);
begin
  if AValue <> ADefValue then
    InternalWriteProperty(APropName, @AValue, SizeOf(AValue));
end;

{-------------------------------------------------------------------------------}
procedure TFDBinStorage.WriteLongWord(const APropName: string;
  const AValue, ADefValue: LongWord);
begin
  if AValue <> ADefValue then
    InternalWriteProperty(APropName, @AValue, SizeOf(AValue));
end;

{-------------------------------------------------------------------------------}
procedure TFDBinStorage.WriteInt64(const APropName: string; const AValue,
  ADefValue: Int64);
begin
  if AValue <> ADefValue then
    InternalWriteProperty(APropName, @AValue, SizeOf(AValue));
end;

{-------------------------------------------------------------------------------}
procedure TFDBinStorage.WriteObjectBegin(const AObjectName: string);
var
  cObjMagic: Byte;
  iDict: Byte;
begin
  cObjMagic := C_ObjBegin;
  FStream.Write(cObjMagic, SizeOf(Byte));
  iDict := LookupName(AObjectName);
  FStream.Write(iDict, SizeOf(iDict));
end;

{-------------------------------------------------------------------------------}
procedure TFDBinStorage.WriteObjectEnd(const AObjectName: string);
var
  cObjMagic: Byte;
begin
  cObjMagic := C_ObjEnd;
  FStream.Write(cObjMagic, SizeOf(Byte));
end;

{-------------------------------------------------------------------------------}
procedure TFDBinStorage.WriteAnsiString(const APropName: string; const AValue,
  ADefValue: TFDAnsiString);
var
  iLen: LongWord;
begin
  if AValue <> ADefValue then begin
    InternalWriteProperty(APropName, nil, 0);
    iLen := Length(AValue) * SizeOf(TFDAnsiChar);
    FStream.Write(iLen, SizeOf(iLen));
    if iLen <> 0 then
      FStream.Write(PFDAnsiString(AValue)^, iLen);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDBinStorage.WriteWideString(const APropName: string;
  const AValue, ADefValue: UnicodeString);
var
  iLen: LongWord;
begin
  if AValue <> ADefValue then begin
    InternalWriteProperty(APropName, nil, 0);
    iLen := Length(AValue) * SizeOf(WideChar);
    FStream.Write(iLen, SizeOf(iLen));
    if iLen <> 0 then
      FStream.Write(AValue[1], iLen);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDBinStorage.WriteValue(const APropName: String; APropIndex: Word;
  ADataType: TFDDataType; ABuff: Pointer; ALen: LongWord);
var
  iLen: Byte;
begin
  if ABuff = nil then
    Exit;
  if FStreamVersion >= 4 then
    FStream.Write(APropIndex, SizeOf(APropIndex))
  else
    InternalWriteProperty(APropName, nil, 0);
  case ADataType of
  dtObject,
  dtRowSetRef,
  dtCursorRef,
  dtRowRef,
  dtArrayRef,
  dtParentRowRef:
    ;                                         
  dtMemo,
  dtHMemo,
  dtWideMemo,
  dtXML,
  dtWideHMemo,
  dtByteString,
  dtBlob,
  dtHBlob,
  dtHBFile,
  dtAnsiString,
  dtWideString:
    begin
      if ADataType in [dtWideMemo, dtXML, dtWideHMemo, dtWideString] then
        ALen := ALen * SizeOf(WideChar);
      FStream.Write(ALen, SizeOf(ALen));
      FStream.Write(ABuff^, ALen);
    end;
  dtBCD,
  dtFmtBCD:
    if FStreamVersion >= 4 then begin
      iLen := 2 + (PBcd(ABuff)^.Precision + 1) div 2;
      FStream.Write(iLen, SizeOf(iLen));
      FStream.Write(ABuff^, iLen);
    end
    else
      FStream.Write(ABuff^, ALen);
  else
    FStream.Write(ABuff^, ALen);
  end;
end;

{-------------------------------------------------------------------------------}
class function TFDBinStorage.CheckExtention(const AExt: String): Boolean;
begin
  Result := (CompareText(AExt, '.FDB') = 0) or
    (CompareText(AExt, '.BIN') = 0) or
    (CompareText(AExt, '.DAT') = 0);
end;

{-------------------------------------------------------------------------------}
class function TFDBinStorage.CheckFormat(AFmt: TFDStorageFormat): Boolean;
begin
  Result := AFmt in [sfAuto, sfBinary];
end;

{-------------------------------------------------------------------------------}
type
  TFDBinStorageBmk = class(TObject)
    FPos: Int64;
  end;

function TFDBinStorage.GetBookmark: TObject;
begin
  Result := TFDBinStorageBmk.Create;
  TFDBinStorageBmk(Result).FPos := FStream.Position;
end;

{-------------------------------------------------------------------------------}
procedure TFDBinStorage.SetBookmark(const AValue: TObject);
begin
  FStream.Position := TFDBinStorageBmk(AValue).FPos;
end;
{$ENDIF}

{-------------------------------------------------------------------------------}
initialization
{$IFDEF FireDAC_STRG_XML}
  FDStorageManager().RegisterStorage(TFDXMLStorage);
{$ENDIF}
{$IFDEF FireDAC_STRG_BIN}
  FDStorageManager().RegisterStorage(TFDBinStorage);
{$ENDIF}

finalization
  FDFreeAndNil(GStorageManager);

end.
