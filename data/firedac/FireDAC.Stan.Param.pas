{*******************************************************}
{                                                       }
{               Delphi FireDAC Framework                }
{           FireDAC db parameters and macros            }
{                                                       }
{ Copyright(c) 2004-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}
{$I FireDAC.inc}

                                                                 

unit FireDAC.Stan.Param;

interface

uses
  System.Classes, Data.DB, System.Variants, Data.FmtBcd, Data.SqlTimSt,
  FireDAC.Stan.Intf, FireDAC.Stan.SQLTimeInt, FireDAC.Stan.Util;

type
  TFDMacro = class;
  TFDMacros = class;
  TFDParam = class;
  TFDParams = class;

  TFDGetOwner = function: TPersistent of object;

  TFDMacroDataType = (mdUnknown, mdString, mdIdentifier, mdInteger,
    mdBoolean, mdFloat, mdDate, mdTime, mdDateTime, mdRaw);

  TFDMacro = class(TCollectionItem)
  private
    FName: String;
    FValue: Variant;
    FDataType: TFDMacroDataType;
    procedure SetValue(const AValue: Variant);
    function GetAsDateTime: TDateTime;
    function GetAsInteger: Integer;
    function GetAsString: String;
    function GetSQL: String;
    procedure SetAsDateTime(const AValue: TDateTime);
    procedure SetAsInteger(const AValue: Integer);
    procedure SetAsString(const AValue: String);
    function GetIsNull: Boolean;
    function GetAsFloat: Double;
    procedure SetAsFloat(const AValue: Double);
    function GetAsDate: TDateTime;
    procedure SetAsDate(const AValue: TDateTime);
    procedure SetDataType(const AValue: TFDMacroDataType);
    procedure SetData(const AValue: Variant; AType: TFDMacroDataType);
    procedure Changed;
    procedure SetAsIdentifier(const AValue: String);
    function GetAsTime: TDateTime;
    procedure SetAsTime(const AValue: TDateTime);
    function GetAsRaw: String;
    procedure SetAsRaw(const AValue: String);
  protected
    function GetDisplayName: String; override;
    function GetCollectionOwner: TPersistent;
  public
    constructor Create(Collection: TCollection); override;
    procedure Clear;
    procedure Assign(AValue: TPersistent); override;
    function IsEqual(AValue: TFDMacro): Boolean;
    property CollectionOwner: TPersistent read GetCollectionOwner;
    property AsString: String read GetAsString write SetAsString;
    property AsIdentifier: String read GetAsString write SetAsIdentifier;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsDate: TDateTime read GetAsDate write SetAsDate;
    property AsTime: TDateTime read GetAsTime write SetAsTime;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsRaw: String read GetAsRaw write SetAsRaw;
    property IsNull: Boolean read GetIsNull;
    property SQL: String read GetSQL;
  published
    property Value: Variant read FValue write SetValue;
    property Name: String read FName write FName;
    property DataType: TFDMacroDataType read FDataType write SetDataType default mdRaw;
  end;

  TFDMacros = class(TCollection)
  private
    FGetOwner: TFDGetOwner;
    FOnChanged: TNotifyEvent;
    FRefs: TFDRefCounter;
    FTempLockUpdate: Boolean;
    function GetDataSet: TDataSet;
    function GetItem(AIndex: Integer): TFDMacro; inline;
    procedure SetItem(AIndex: Integer; AValue: TFDMacro);
    procedure DoChanged;
    function GetIsRefCounted: Boolean;
  protected
    function GetOwner: TPersistent; override;
    procedure AssignTo(ADest: TPersistent); override;
  public
    constructor Create; overload;
    constructor CreateRefCounted(AGetOwner: TFDGetOwner);
    destructor Destroy; override;
    procedure AddRef;
    procedure RemRef;
    procedure Assign(AValue: TPersistent); override;
    procedure EndUpdate; override;
    procedure TempLockUpdate;
    procedure AssignValues(AValue: TFDMacros);
    function Add: TFDMacro;
    function IsEqual(AValue: TFDMacros): Boolean;
    function MacroByName(const AValue: String): TFDMacro;
    function FindMacro(const AValue: String): TFDMacro;
    property IsRefCounted: Boolean read GetIsRefCounted;
    property Command: TDataSet read GetDataSet;
    property Items[AIndex: Integer]: TFDMacro read GetItem write SetItem; default;
    property UpdateCount;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property OnGetOwner: TFDGetOwner read FGetOwner write FGetOwner;
  end;

  TFDParamArrayType = (atScalar, atArray, atPLSQLTable);
  TFDParamBindMode = (pbByName, pbByNumber);

  TFDParam = class(TCollectionItem)
  private
    FName: String;
    FDataType: TFieldType;
    FADDataType: TFDDataType;
    FParamType: TParamType;
    FSize: Integer;
    FPrecision: Integer;
    FNumericScale: Integer;
    FBound: Boolean;
    FPosition: Integer;
    FIsCaseSensitive: Boolean;
    FArrayType: TFDParamArrayType;
    FValue: TFDVariantArray;
    procedure InternalSetValue(AIndex: Integer; const AValue: Variant);
    function GetDataSet: TDataSet;
    function IsParamStored: Boolean;
    function GetAsBCDs(AIndex: Integer): Currency;
    function GetAsBooleans(AIndex: Integer): Boolean;
    function GetAsByteStrs(AIndex: Integer): TFDByteString;
    function GetAsCurrencys(AIndex: Integer): Currency;
    function GetAsDateTimes(AIndex: Integer): TDateTime;
    function GetAsFloats(AIndex: Integer): Double;
    function GetAsFMTBCDs(AIndex: Integer): TBcd;
    function GetAsIntegers(AIndex: Integer): LongInt;
    function GetAsSQLTimeStamps(AIndex: Integer): TSQLTimeStamp;
    function GetAsStrings(AIndex: Integer): String;
    function GetAsVariants(AIndex: Integer): Variant;
    function GetIsNulls(AIndex: Integer): Boolean;
    function GetAsWideStrings(AIndex: Integer): UnicodeString;
    procedure SetAsBCDs(AIndex: Integer; const AValue: Currency);
    procedure SetAsBlobs(AIndex: Integer; const AValue: TFDByteString);
    procedure SetAsBooleans(AIndex: Integer; const AValue: Boolean);
    procedure SetAsByteStrs(AIndex: Integer; const AValue: TFDByteString);
    procedure SetAsVarByteStrs(AIndex: Integer; const AValue: TFDByteString);
    procedure SetAsCurrencys(AIndex: Integer; const AValue: Currency);
    procedure SetAsDates(AIndex: Integer; const AValue: TDateTime);
    procedure SetAsDateTimes(AIndex: Integer; const AValue: TDateTime);
    procedure SetAsFloats(AIndex: Integer; const AValue: Double);
    procedure SetAsFMTBCDs(AIndex: Integer; const AValue: TBcd);
    procedure SetAsIntegers(AIndex: Integer; const AValue: LongInt);
    procedure SetAsMemos(AIndex: Integer; const AValue: TFDAnsiString);
    procedure SetAsSmallInts(AIndex: Integer; const AValue: LongInt);
    procedure SetAsSQLTimeStamps(AIndex: Integer; const AValue: TSQLTimeStamp);
    procedure SetAsStrings(AIndex: Integer; const AValue: String);
    procedure SetAsTimes(AIndex: Integer; const AValue: TDateTime);
    procedure SetAsVariants(AIndex: Integer; const AValue: Variant);
    procedure SetAsWords(AIndex: Integer; AValue: LongWord);
    procedure SetBytesValue(AIndex: Integer; ABuff: PByte; ASize: LongWord);
    procedure GetBytesValue(AIndex: Integer; ABuff: PByte);
    procedure SetTexts(AIndex: Integer; const AValue: String);
    procedure SetAsWideStrings(AIndex: Integer; const AValue: UnicodeString);
    procedure ErrIndex(AIndex: Integer);
    procedure CheckIndex(var AIndex: Integer);
    function GetArraySize: Integer;
    procedure SetArraySize(AValue: Integer);
    procedure SetArrayType(AValue: TFDParamArrayType);
    procedure UpdateData(ADim: Integer);
    function GetAsLargeInt: LargeInt;
    function GetAsLargeInts(AIndex: Integer): LargeInt;
    procedure SetAsLargeInt(const AValue: LargeInt);
    procedure SetAsLargeInts(AIndex: Integer; const AValue: LargeInt);
    procedure GetVarData(out AVar: PVariant; AIndex: Integer = -1);
    procedure ErrBadFieldType;
    procedure ErrUnknownFieldType;
    function GetAsGUID: TGUID;
    function GetAsGUIDs(AIndex: Integer): TGUID;
    procedure SetAsGUID(const AValue: TGUID);
    procedure SetAsGUIDs(AIndex: Integer; const AValue: TGUID);
    procedure SetAsWideMemo(const AValue: UnicodeString);
    procedure SetAsWideMemos(AIndex: Integer; const AValue: UnicodeString);
    procedure SetAsFixedChar(const AValue: String);
    procedure SetAsFixedChars(AIndex: Integer; const AValue: String);
    function GetAsAnsiString: TFDAnsiString;
    function GetAsAnsiStrings(AIndex: Integer): TFDAnsiString;
    function GetAsExtended: Extended;
    function GetAsExtendeds(AIndex: Integer): Extended;
    function GetAsLongWord: LongWord;
    function GetAsLongWords(AIndex: Integer): LongWord;
    procedure SetAsAnsiString(const AValue: TFDAnsiString);
    procedure SetAsAnsiStrings(AIndex: Integer; const AValue: TFDAnsiString);
    procedure SetAsByte(const AValue: LongWord);
    procedure SetAsBytes(AIndex: Integer; const AValue: LongWord);
    procedure SetAsExtended(const AValue: Extended);
    procedure SetAsExtendeds(AIndex: Integer; const AValue: Extended);
    procedure SetAsLongWord(const AValue: LongWord);
    procedure SetAsLongWords(AIndex: Integer; const AValue: LongWord);
    procedure SetAsShortInt(const AValue: LongInt);
    procedure SetAsShortInts(AIndex: Integer; const AValue: LongInt);
    function GetAsFMTBCD: TBcd;
    function GetAsBCD: Currency;
    function GetAsBoolean: Boolean;
    function GetAsByteStr: TFDByteString;
    function GetAsDateTime: TDateTime;
    function GetAsSQLTimeStamp: TSQLTimeStamp;
    function GetAsCurrency: Currency;
    function GetAsFloat: Double;
    function GetAsInteger: Longint;
    function GetAsString: String;
    function GetAsWideString: UnicodeString;
    function GetAsVariant: Variant;
    function GetIsNull: Boolean;
    procedure SetAsBCD(const AValue: Currency);
    procedure SetAsFMTBCD(const AValue: TBcd);
    procedure SetAsBlob(const AValue: TFDByteString);
    procedure SetAsByteStr(const AValue: TFDByteString);
    procedure SetAsVarByteStr(const AValue: TFDByteString);
    procedure SetAsBoolean(const AValue: Boolean);
    procedure SetAsCurrency(const AValue: Currency);
    procedure SetAsDate(const AValue: TDateTime);
    procedure SetAsDateTime(const AValue: TDateTime);
    procedure SetAsSQLTimeStamp(const AValue: TSQLTimeStamp);
    procedure SetAsFloat(const AValue: Double);
    procedure SetAsInteger(const AValue: Longint);
    procedure SetAsMemo(const AValue: TFDAnsiString);
    procedure SetAsString(const AValue: String);
    procedure SetAsWideString(const AValue: UnicodeString);
    procedure SetAsSmallInt(const AValue: LongInt);
    procedure SetAsTime(const AValue: TDateTime);
    procedure SetAsVariant(const AValue: Variant);
    procedure SetAsWord(const AValue: LongWord);
    procedure SetDataType(AValue: TFieldType);
    procedure SetText(const AValue: String);
    function GetAsSQLTimeInterval: TFDSQLTimeInterval;
    function GetAsSQLTimeIntervals(AIndex: Integer): TFDSQLTimeInterval;
    procedure SetAsSQLTimeInterval(const AValue: TFDSQLTimeInterval);
    procedure SetAsSQLTimeIntervals(AIndex: Integer; const AValue: TFDSQLTimeInterval);
    procedure SetAsXML(const AValue: UnicodeString);
    procedure SetAsXMLs(AIndex: Integer; const AValue: UnicodeString);
    function GetAsSingle: Single;
    function GetAsSingles(AIndex: Integer): Single;
    procedure SetAsSingle(const AValue: Single);
    procedure SetAsSingles(AIndex: Integer; const AValue: Single);
    function GetSQLName: String;
    procedure BeginSetBlobRawData(var AIndex: Integer);
    function EndSetBlobRawData(ALen: LongWord; APtr: PByte;
      AIndex: Integer): PByte;
  protected
    procedure AssignParam(AParam: TFDParam);
    procedure AssignDlpParam(AParam: TParam);
    procedure AssignToDlpParam(AParam: TParam);
    procedure AssignTo(ADest: TPersistent); override;
    function IsEqual(AValue: TFDParam): Boolean;
    function GetDisplayName: String; override;
    property DataSet: TDataSet read GetDataSet;
  public
    constructor Create(Collection: TCollection); overload; override;
    constructor Create(AParams: TFDParams; AParamType: TParamType); reintroduce; overload;

    // value manipulations
    procedure Assign(ASource: TPersistent); override;
    procedure AssignField(AField: TField);
    procedure AssignFieldValue(AField: TField; const AValue: Variant); overload;
    procedure AssignFieldValue(AField: TField); overload;
    procedure Clear(AIndex: Integer = -1);
    procedure AssignVarRec(const AVarRec: TVarRec; AIndex: Integer = -1);

    // raw data methods
    function GetBlobRawData(var ALen: LongWord; var APtr: PByte; AIndex: Integer = -1): Boolean;
    function SetBlobRawData(ALen: LongWord; APtr: PByte; AIndex: Integer = -1): PByte;
    procedure GetData(ABuffer: PByte; AIndex: Integer = -1);
    procedure SetData(ABuffer: PByte; ALen: LongWord = $FFFFFFFF; AIndex: Integer = -1);
    function GetDataLength(AIndex: Integer = -1): Integer;
    function GetDataSize(AIndex: Integer = -1): Integer;
    procedure LoadFromFile(const AFileName: String; ABlobType: TBlobType; AIndex: Integer = -1);
    procedure LoadFromStream(AStream: TStream; ABlobType: TBlobType; AIndex: Integer = -1);

    // value properties
    property AsSingle: Single read GetAsSingle write SetAsSingle;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsExtended: Extended read GetAsExtended write SetAsExtended;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsBCD: Currency read GetAsBCD write SetAsBCD;
    property AsFMTBCD: TBcd read GetAsFMTBCD write SetAsFMTBCD;

    property AsShortInt: LongInt read GetAsInteger write SetAsShortInt;
    property AsByte: LongWord read GetAsLongWord write SetAsByte;
    property AsSmallInt: LongInt read GetAsInteger write SetAsSmallInt;
    property AsWord: LongWord read GetAsLongWord write SetAsWord;
    property AsInteger: LongInt read GetAsInteger write SetAsInteger;
    property AsLongword: LongWord read GetAsLongWord write SetAsLongWord;
    property AsLargeInt: LargeInt read GetAsLargeInt write SetAsLargeInt;

    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;

    property AsByteStr: TFDByteString read GetAsByteStr write SetAsByteStr;
    property AsVarByteStr: TFDByteString read GetAsByteStr write SetAsVarByteStr;
    property AsString: String read GetAsString write SetAsString;
    property AsFixedChar: String read GetAsString write SetAsFixedChar;
    property AsWideString: UnicodeString read GetAsWideString write SetAsWideString;
    property AsAnsiString: TFDAnsiString read GetAsAnsiString write SetAsAnsiString;

    property AsBlob: TFDByteString read GetAsByteStr write SetAsBlob;
    property AsMemo: TFDAnsiString read GetAsAnsiString write SetAsMemo;
    property AsWideMemo: UnicodeString read GetAsWideString write SetAsWideMemo;
    property AsXML: UnicodeString read GetAsWideString write SetAsXML;

    property AsDate: TDateTime read GetAsDateTime write SetAsDate;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsSQLTimeStamp: TSQLTimeStamp read GetAsSQLTimeStamp write SetAsSQLTimeStamp;
    property AsTime: TDateTime read GetAsDateTime write SetAsTime;
    property AsSQLTimeInterval: TFDSQLTimeInterval read GetAsSQLTimeInterval write SetAsSQLTimeInterval;

    property AsGUID: TGUID read GetAsGUID write SetAsGUID;

    // array value properties
    property AsSingles[AIndex: Integer]: Single read GetAsSingles write SetAsSingles;
    property AsFloats[AIndex: Integer]: Double read GetAsFloats write SetAsFloats;
    property AsExtendeds[AIndex: Integer]: Extended read GetAsExtendeds write SetAsExtendeds;
    property AsCurrencys[AIndex: Integer]: Currency read GetAsCurrencys write SetAsCurrencys;
    property AsBCDs[AIndex: Integer]: Currency read GetAsBCDs write SetAsBCDs;
    property AsFMTBCDs[AIndex: Integer]: TBcd read GetAsFMTBCDs write SetAsFMTBCDs;

    property AsShortInts[AIndex: Integer]: LongInt read GetAsIntegers write SetAsShortInts;
    property AsBytes[AIndex: Integer]: LongWord read GetAsLongWords write SetAsBytes;
    property AsSmallInts[AIndex: Integer]: LongInt read GetAsIntegers write SetAsSmallInts;
    property AsWords[AIndex: Integer]: LongWord read GetAsLongWords write SetAsWords;
    property AsIntegers[AIndex: Integer]: LongInt read GetAsIntegers write SetAsIntegers;
    property AsLongwords[AIndex: Integer]: LongWord read GetAsLongWords write SetAsLongWords;
    property AsLargeInts[AIndex: Integer]: LargeInt read GetAsLargeInts write SetAsLargeInts;

    property AsBooleans[AIndex: Integer]: Boolean read GetAsBooleans write SetAsBooleans;

    property AsByteStrs[AIndex: Integer]: TFDByteString read GetAsByteStrs write SetAsByteStrs;
    property AsVarByteStrs[AIndex: Integer]: TFDByteString read GetAsByteStrs write SetAsVarByteStrs;
    property AsStrings[AIndex: Integer]: String read GetAsStrings write SetAsStrings;
    property AsFixedChars[AIndex: Integer]: String read GetAsStrings write SetAsFixedChars;
    property AsWideStrings[AIndex: Integer]: UnicodeString read GetAsWideStrings write SetAsWideStrings;
    property AsAnsiStrings[AIndex: Integer]: TFDAnsiString read GetAsAnsiStrings write SetAsAnsiStrings;

    property AsBlobs[AIndex: Integer]: TFDByteString read GetAsByteStrs write SetAsBlobs;
    property AsMemos[AIndex: Integer]: TFDAnsiString read GetAsAnsiStrings write SetAsMemos;
    property AsWideMemos[AIndex: Integer]: UnicodeString read GetAsWideStrings write SetAsWideMemos;
    property AsXMLs[AIndex: Integer]: UnicodeString read GetAsWideStrings write SetAsXMLs;

    property AsDates[AIndex: Integer]: TDateTime read GetAsDateTimes write SetAsDates;
    property AsDateTimes[AIndex: Integer]: TDateTime read GetAsDateTimes write SetAsDateTimes;
    property AsSQLTimeStamps[AIndex: Integer]: TSQLTimeStamp read GetAsSQLTimeStamps write SetAsSQLTimeStamps;
    property AsTimes[AIndex: Integer]: TDateTime read GetAsDateTimes write SetAsTimes;
    property AsSQLTimeIntervals[AIndex: Integer]: TFDSQLTimeInterval read GetAsSQLTimeIntervals write SetAsSQLTimeIntervals;

    property AsGUIDs[AIndex: Integer]: TGUID read GetAsGUIDs write SetAsGUIDs;

    property Values[AIndex: Integer]: Variant read GetAsVariants write SetAsVariants;
    property IsNulls[AIndex: Integer]: Boolean read GetIsNulls;

    // attributes
    property SQLName: String read GetSQLName;
    property Bound: Boolean read FBound write FBound;
    property IsNull: Boolean read GetIsNull;
    property Text: String read GetAsString write SetText;
    property Texts[AIndex: Integer]: String read GetAsStrings write SetTexts;

  published
    property Position: Integer read FPosition write FPosition default 0;
    property Name: String read FName write FName;
    property IsCaseSensitive: Boolean read FIsCaseSensitive write FIsCaseSensitive default False;
    property ArrayType: TFDParamArrayType read FArrayType write SetArrayType default atScalar;
    property ArraySize: Integer read GetArraySize write SetArraySize default 1;
    property DataType: TFieldType read FDataType write SetDataType default ftUnknown;
    property ADDataType: TFDDataType read FADDataType write FADDataType default dtUnknown;
    property Precision: Integer read FPrecision write FPrecision default 0;
    property NumericScale: Integer read FNumericScale write FNumericScale default 0;
    property ParamType: TParamType read FParamType write FParamType default ptUnknown;
    property Size: Integer read FSize write FSize default 0;
    property Value: Variant read GetAsVariant write SetAsVariant stored IsParamStored;
  end;

  TFDParams = class(TCollection)
  private
    FGetOwner: TFDGetOwner;
    FBindMode: TFDParamBindMode;
    FRefs: TFDRefCounter;
    FMarkers: TStrings;
    function GetItem(Index: Integer): TFDParam; inline;
    procedure SetItem(Index: Integer; const AValue: TFDParam);
    function GetArraySize: Integer;
    procedure SetArraySize(const AValue: Integer);
    function GetParamValues(const AParamName: UnicodeString): Variant;
    procedure SetParamValues(const AParamName: UnicodeString; const AValue: Variant);
    function GetIsRefCounted: Boolean;
    procedure ReadBinaryData(Stream: TStream);
    procedure SetMarkers(const AValue: TStrings);
  protected
    function GetDataSet: TDataSet;
    function GetOwner: TPersistent; override;
    procedure DefineProperties(AFiler: TFiler); override;
    property DataSet: TDataSet read GetDataSet;
  public
    constructor Create; overload;
    constructor CreateRefCounted(AGetOwner: TFDGetOwner); overload;
    destructor Destroy; override;
    procedure AddRef;
    procedure RemRef;
    procedure AssignValues(AValue: TFDParams;
      AParamTypes: TParamTypes = [ptUnknown .. ptResult]);
    procedure Prepare(ADefaultDataType: TFieldType;
      ADefaultParamType: TParamType);
    procedure Assign(AValue: TPersistent); override;
    function Add: TFDParam; overload;
    function Add(const AName: String; const AValue: Variant;
      AParamType: TParamType = ptUnknown): TFDParam; overload;
    function Add(const AName: String; AType: TFieldType; ASize: Integer = -1;
      AParamType: TParamType = ptUnknown): TFDParam; overload;
    function CreateParam(AFldType: TFieldType; const AParamName: String;
      AParamType: TParamType): TFDParam;
    procedure GetParamList(AList: TFDObjList; const AParamNames: String);
    function IsEqual(AValue: TFDParams): Boolean;
    function ParamByName(const AValue: String): TFDParam;
    function FindParam(const AValue: String): TFDParam; overload;
    function ParamByPosition(const AValue: Integer): TFDParam;
    function FindParam(const AValue: Integer): TFDParam; overload;
    procedure ClearValues(AIndex: Integer = -1);
    property IsRefCounted: Boolean read GetIsRefCounted;
    property Items[Index: Integer]: TFDParam read GetItem write SetItem; default;
    property ParamValues[const AParamName: UnicodeString]: Variant read GetParamValues write SetParamValues;
    property ArraySize: Integer read GetArraySize write SetArraySize;
    property BindMode: TFDParamBindMode read FBindMode write FBindMode;
    property Markers: TStrings read FMarkers write SetMarkers;
    property OnGetOwner: TFDGetOwner read FGetOwner write FGetOwner;
  end;

function FDVarType2FieldType(const AValue: Variant): TFDMacroDataType;
function FDVar2SQLTyped(const AValue: Variant; AFieldType: TFDMacroDataType): String;
function FDVar2SQL(const AValue: Variant): String;

var
  C_FieldType2VarType: array[TFieldType] of Integer = (
    varError, varString, varSmallint, varInteger, varSmallint,
    varBoolean, varDouble, varCurrency, varCurrency, varDate, varDate, varDate,
    varString, varString, varInteger, varString, varString, varString, varOleStr,
    varError, varError, varString, varError, varString, varOleStr,
    varInt64, varError, varError, varError, varError, varString, varString,
    varVariant, varUnknown, varDispatch, varString
    , varDate, varCurrency
    , varOleStr, varOleStr, varDate, varString
    , varLongWord, varShortInt, varByte, varDouble, varError, varError, varError
    , varString, varError, varSingle
    );

const    
  C_MacroDataType2VarType: array[TFDMacroDataType] of Integer = (
    varError, varADString, varADString, varInteger, varBoolean, varDouble, varDate,
    varDate, varDate, varADString);

  C_ParamTypeNames: array [TParamType] of string =
    ('ptUnknown', 'ptInput', 'ptOutput', 'ptInputOutput', 'ptResult');

  C_FDDataAttributeNames: array[TFDDataAttribute] of string =
    ('Searchable', 'AllowNull', 'FixedLen', 'BlobData', 'ReadOnly', 'AutoInc',
     'ROWID', 'Default', 'RowVersion', 'Internal', 'Calculated', 'Volatile',
     'Unnamed', 'Virtual', 'Base', 'Expression');

  C_FDDataOptionNames: array[TFDDataOption] of string =
    ('AllowNull', 'Unique', 'ReadOnly', 'InUpdate',
     'InWhere', 'InKey', 'AfterInsChanged', 'AfterUpdChanged');

  C_FDParamArrayTypeNames: array[TFDParamArrayType] of string =
    ('atScalar', 'atArray', 'atPLSQLTable');

  C_MacroTypeNames: array [TFDMacroDataType] of string =
    ('mdUnknown', 'mdString', 'mdIdentifier', 'mdInteger',
     'mdBoolean', 'mdFloat', 'mdDate', 'mdTime', 'mdDateTime', 'mdRaw');

implementation

uses
  System.VarUtils, System.SysUtils, Data.DBConsts,
  FireDAC.Stan.Error, FireDAC.Stan.Consts;

{ ---------------------------------------------------------------------------- }
function FDVarType2FieldType(const AValue: Variant): TFDMacroDataType;
begin
  case VarType(AValue) of
  varEmpty,
  varNull     : Result := mdUnknown;
  varByte,
  varShortInt,
  varWord,
  varSmallint,
  varLongWord,
  varInteger,
  varUInt64,
  varInt64    : Result := mdInteger;
  varSingle,
  varDouble,
  varCurrency : Result := mdFloat;
  varDate     : Result := mdDateTime;
  varUString,
  varByte or varArray,
  varString,
  varOleStr   : Result := mdString;
  varBoolean  : Result := mdBoolean;
  else
    if VarIsSQLTimeStamp(AValue) then
      Result := mdDateTime
    else
    if VarIsFMTBcd(AValue) then
      Result := mdFloat
    else
      Result := mdUnknown;
  end;
end;

{ ---------------------------------------------------------------------------- }
function FDVar2SQLTyped(const AValue: Variant; AFieldType: TFDMacroDataType): String;
var
  l: Boolean;
  dt: TDateTime;
  rFS: TFormatSettings;
begin
  Result := '';
  if (VarType(AValue) and varTypeMask) in [varEmpty, varNull] then begin
    if not (AFieldType in [mdRaw, mdIdentifier]) then
      Result := 'NULL';
  end
  else begin
    case AFieldType of
    mdRaw:
      Result := AValue;
    mdInteger:
      Result := VarAsType(AValue, varInteger);
    mdFloat:
      Result := '{e ' + FDFloat2Str(VarAsType(AValue, varDouble), '.', 15) + '}';
    mdDate:
      begin
        dt := VarAsType(AValue, varDate);
        rFS.DateSeparator := '-';
        Result := FormatDateTime('yyyy/mm/dd', dt, rFS);
        Result := '{d ''' + Result + '''}';
      end;
    mdTime:
      begin
        dt := VarAsType(AValue, varDate);
        rFS.TimeSeparator := ':';
        Result := FormatDateTime('hh:nn:ss', dt);
        Result := '{t ''' + Result + '''}';
      end;
    mdDateTime:
      begin
        dt := VarAsType(AValue, varDate);
        rFS.DateSeparator := '-';
        rFS.TimeSeparator := ':';
        Result := FormatDateTime('yyyy/mm/dd hh:nn:ss', dt, rFS);
        Result := '{dt ''' + Result + '''}';
      end;
    mdUnknown,
    mdString:
      Result := '{s ''' + VarAsType(AValue, varADWString) + '''}';
    mdIdentifier:
      Result := '{id ''' + VarAsType(AValue, varADWString) + '''}';
    mdBoolean:
      begin
        l := VarAsType(AValue, varBoolean);
        if l then
          Result := '{l True}'
        else
          Result := '{l False}';
      end;
    end;
  end;
end;

{ ---------------------------------------------------------------------------- }
function FDVar2SQL(const AValue: Variant): String;
begin
  Result := FDVar2SQLTyped(AValue, FDVarType2FieldType(AValue));
end;

{ ---------------------------------------------------------------------------- }
{ TFDMacro                                                                     }
{ ---------------------------------------------------------------------------- }
constructor TFDMacro.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FValue := Null;
  FDataType := mdRaw;
end;

{ ---------------------------------------------------------------------------- }
function TFDMacro.GetSQL: String;
begin
  Result := FDVar2SQLTyped(FValue, DataType);
end;

{ ---------------------------------------------------------------------------- }
function TFDMacro.GetDisplayName: String;
begin
  if Name <> '' then
    Result := Name
  else
    Result := inherited GetDisplayName;
end;

{ ---------------------------------------------------------------------------- }
function TFDMacro.GetCollectionOwner: TPersistent;
begin
  if not Assigned(Collection) then
    Result := nil
  else
    Result := TFDMacros(Collection).GetOwner;
end;

{ ---------------------------------------------------------------------------- }
function TFDMacro.IsEqual(AValue: TFDMacro): Boolean;
begin
  Result := (Name = AValue.Name) and (DataType = AValue.DataType);
  if Result then
    try
      Result := Value = AValue.Value;
    except
      Result := False;
    end;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDMacro.Assign(AValue: TPersistent);
begin
  if AValue is TFDMacro then
    try
      if Collection <> nil then
        Collection.BeginUpdate;
      FDataType := TFDMacro(AValue).DataType;
      FName := TFDMacro(AValue).Name;
      FValue := TFDMacro(AValue).Value;
    finally
      if Collection <> nil then
        Collection.EndUpdate;
    end
  else
    inherited Assign(AValue);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDMacro.Changed;
begin
  TFDMacros(Collection).DoChanged;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDMacro.SetDataType(const AValue: TFDMacroDataType);
begin
  if FDataType <> AValue then begin
    FDataType := AValue;
    Changed;
  end;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDMacro.SetData(const AValue: Variant; AType: TFDMacroDataType);
var
  lEQ: Boolean;
begin
  try
    lEQ := Value = AValue;
  except
    lEQ := False;
  end;
  if not lEQ then begin
    FValue := AValue;
    if AType <> mdUnknown then
      FDataType := AType
    else
      if FDataType = mdUnknown then
        FDataType := FDVarType2FieldType(AValue);
    Changed;
  end;
end;

{ ---------------------------------------------------------------------------- }
function TFDMacro.GetIsNull: Boolean;
var
  pData: PVarData;
begin
  pData := FindVarData(FValue);
  Result := (pData^.VType = varNull) or (pData^.VType = varEmpty);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDMacro.Clear;
begin
  SetData(Null, mdUnknown);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDMacro.SetValue(const AValue: Variant);
begin
  SetData(AValue, mdUnknown);
end;

{ ---------------------------------------------------------------------------- }
function TFDMacro.GetAsString: String;
begin
  if IsNull then
    Result := ''
  else
    Result := FValue;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDMacro.SetAsString(const AValue: String);
begin
  SetData(AValue, mdString);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDMacro.SetAsIdentifier(const AValue: String);
begin
  SetData(AValue, mdIdentifier);
end;

{ ---------------------------------------------------------------------------- }
function TFDMacro.GetAsInteger: Integer;
begin
  if IsNull then
    Result := 0
  else
    Result := FValue;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDMacro.SetAsInteger(const AValue: Integer);
begin
  SetData(AValue, mdInteger);
end;

{ ---------------------------------------------------------------------------- }
function TFDMacro.GetAsDateTime: TDateTime;
begin
  if IsNull then
    Result := 0.0
  else
    Result := FValue;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDMacro.SetAsDateTime(const AValue: TDateTime);
begin
  SetData(AValue, mdDateTime);
end;

{ ---------------------------------------------------------------------------- }
function TFDMacro.GetAsFloat: Double;
begin
  if IsNull then
    Result := 0.0
  else
    Result := FValue;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDMacro.SetAsFloat(const AValue: Double);
begin
  SetData(AValue, mdFloat);
end;

{ ---------------------------------------------------------------------------- }
function TFDMacro.GetAsDate: TDateTime;
begin
  if IsNull then
    Result := 0.0
  else
    Result := FValue;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDMacro.SetAsDate(const AValue: TDateTime);
var
  dt: TDateTime;
begin
  dt := Trunc(AValue);
  SetData(dt, mdDate);
end;

{ ---------------------------------------------------------------------------- }
function TFDMacro.GetAsTime: TDateTime;
begin
  if IsNull then
    Result := 0.0
  else
    Result := FValue;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDMacro.SetAsTime(const AValue: TDateTime);
var
  dt: TDateTime;
begin
  dt := AValue - Trunc(AValue);
  SetData(dt, mdTime);
end;

{ ---------------------------------------------------------------------------- }
function TFDMacro.GetAsRaw: String;
begin
  if IsNull then
    Result := ''
  else
    Result := FValue;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDMacro.SetAsRaw(const AValue: String);
begin
  SetData(AValue, mdRaw);
end;

{ ---------------------------------------------------------------------------- }
{ TFDMacros                                                                    }
{ ---------------------------------------------------------------------------- }
constructor TFDMacros.Create;
begin
  inherited Create(TFDMacro);
end;

{ ---------------------------------------------------------------------------- }
constructor TFDMacros.CreateRefCounted(AGetOwner: TFDGetOwner);
begin
  FGetOwner := AGetOwner;
  inherited Create(TFDMacro);
  FRefs := TFDRefCounter.Create(Self);
  FRefs.CountRef(1);
end;

{-------------------------------------------------------------------------------}
destructor TFDMacros.Destroy;
begin
  FDFreeAndNil(FRefs);
  inherited Destroy;
end;

{-------------------------------------------------------------------------------}
procedure TFDMacros.AddRef;
begin
  if FRefs <> nil then
    FRefs.AddRef;
end;

{-------------------------------------------------------------------------------}
procedure TFDMacros.RemRef;
begin
  if FRefs <> nil then
    FRefs.RemRef;
end;

{-------------------------------------------------------------------------------}
function TFDMacros.GetIsRefCounted: Boolean;
begin
  Result := (FRefs <> nil) and (FRefs.Refs >= 0);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDMacros.Assign(AValue: TPersistent);
var
  i, j: Integer;
  s: String;
  oMac: TFDMacro;
begin
  if AValue is TStrings then begin
    BeginUpdate;
    try
      Clear;
      for i := 0 to TStrings(AValue).Count - 1 do begin
        oMac := TFDMacro(Add);
        s := TStrings(AValue)[i];
        j := Pos('=', s);
        oMac.Name := Copy(s, 1, j - 1);
        oMac.AsRaw := Copy(s, j + 1, Length(s));
      end;
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(AValue);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDMacros.AssignTo(ADest: TPersistent);
var
  i: Integer;
begin
  if ADest is TStrings then begin
    TStrings(ADest).Clear;
    for i := 0 to Count - 1 do
      TStrings(ADest).Add(Items[i].Name + '=' + Items[i].Value);
  end
  else
    inherited AssignTo(ADest);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDMacros.AssignValues(AValue: TFDMacros);
var
  i: Integer;
  oMacro: TFDMacro;
begin
  BeginUpdate;
  try
    for i := 0 to AValue.Count - 1 do begin
      oMacro := FindMacro(AValue[i].Name);
      if oMacro <> nil then
        oMacro.Assign(AValue[i]);
    end;
  finally
    EndUpdate;
  end;
end;

{ ---------------------------------------------------------------------------- }
function TFDMacros.Add: TFDMacro;
begin
  Result := TFDMacro(inherited Add);
end;

{ ---------------------------------------------------------------------------- }
function TFDMacros.IsEqual(AValue: TFDMacros): Boolean;
var
  i: Integer;
begin
  Result := Count = AValue.Count;
  if Result then                                                               
    for i := 0 to Count - 1 do begin
      Result := Items[i].IsEqual(AValue.Items[i]);
      if not Result then
        Break;
    end
end;

{ ---------------------------------------------------------------------------- }
function TFDMacros.FindMacro(const AValue: String): TFDMacro;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do begin
    Result := Items[i];
    if {$IFDEF FireDAC_NOLOCALE_META} CompareText {$ELSE} AnsiCompareText {$ENDIF}
       (Result.Name, AValue) = 0 then
      Exit;
  end;
  Result := nil;
end;

{ ---------------------------------------------------------------------------- }
function TFDMacros.MacroByName(const AValue: String): TFDMacro;
begin
  Result := FindMacro(AValue);
  if Result = nil then
    FDException(Self, [S_FD_LStan], er_FD_StanMacroNotFound, [AValue]);
end;

{ ---------------------------------------------------------------------------- }
function TFDMacros.GetDataSet: TDataSet;
begin
  if Assigned(FGetOwner) and (FGetOwner() <> nil) and (FGetOwner() is TDataSet) then
    Result := TDataSet(FGetOwner())
  else
    Result := nil;
end;

{ ---------------------------------------------------------------------------- }
function TFDMacros.GetItem(AIndex: Integer): TFDMacro;
begin
  Result := TFDMacro(inherited Items[AIndex]);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDMacros.SetItem(AIndex: Integer; AValue: TFDMacro);
begin
  inherited Items[AIndex] := AValue;
end;

{ ---------------------------------------------------------------------------- }
function TFDMacros.GetOwner: TPersistent;
begin
  if Assigned(FGetOwner) then
    Result := FGetOwner()
  else
    Result := nil;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDMacros.DoChanged;
begin
  if UpdateCount = 0 then
    try
      if Assigned(FOnChanged) and not FTempLockUpdate then
        FOnChanged(Self);
    finally
      FTempLockUpdate := False;
    end;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDMacros.EndUpdate;
begin
  inherited EndUpdate;
  DoChanged;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDMacros.TempLockUpdate;
begin
  FTempLockUpdate := True;
end;

{ ---------------------------------------------------------------------------- }
{ TFDParams                                                                    }
{ ---------------------------------------------------------------------------- }
constructor TFDParams.Create;
begin
  inherited Create(TFDParam);
  FMarkers := TFDStringList.Create;
end;

{ ---------------------------------------------------------------------------- }
constructor TFDParams.CreateRefCounted(AGetOwner: TFDGetOwner);
begin
  FGetOwner := AGetOwner;
  inherited Create(TFDParam);
  FRefs := TFDRefCounter.Create(Self);
  FRefs.CountRef(1);
  FMarkers := TFDStringList.Create;
end;

{-------------------------------------------------------------------------------}
destructor TFDParams.Destroy;
begin
  FDFreeAndNil(FRefs);
  FDFreeAndNil(FMarkers);
  inherited Destroy;
end;

{-------------------------------------------------------------------------------}
procedure TFDParams.AddRef;
begin
  if FRefs <> nil then
    FRefs.AddRef;
end;

{-------------------------------------------------------------------------------}
procedure TFDParams.RemRef;
begin
  if FRefs <> nil then
    FRefs.RemRef;
end;

{-------------------------------------------------------------------------------}
function TFDParams.GetIsRefCounted: Boolean;
begin
  Result := (FRefs <> nil) and (FRefs.Refs >= 0);
end;

{ ---------------------------------------------------------------------------- }
function TFDParams.GetDataSet: TDataSet;
begin
  if Assigned(FGetOwner) and (FGetOwner() <> nil) and (FGetOwner() is TDataSet) then
    Result := TDataSet(FGetOwner())
  else
    Result := nil;
end;

{ ---------------------------------------------------------------------------- }
function TFDParams.GetOwner: TPersistent;
begin
  if Assigned(FGetOwner) then
    Result := FGetOwner()
  else
    Result := nil;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParams.SetMarkers(const AValue: TStrings);
begin
  FMarkers.Assign(AValue);
end;

{ ---------------------------------------------------------------------------- }
function TFDParams.Add: TFDParam;
begin
  Result := TFDParam(inherited Add);
  if (Count > 1) and (Items[0].ArrayType = atArray) then
    Result.ArraySize := Items[0].ArraySize;
end;

{ ---------------------------------------------------------------------------- }
function TFDParams.Add(const AName: String;
  const AValue: Variant; AParamType: TParamType = ptUnknown): TFDParam;
begin
  Result := Add;
  Result.Name := AName;
  Result.Value := AValue;
  if AParamType <> ptUnknown then
    Result.ParamType := AParamType;
end;

{ ---------------------------------------------------------------------------- }
function TFDParams.Add(const AName: String; AType: TFieldType;
  ASize: Integer = -1; AParamType: TParamType = ptUnknown): TFDParam;
begin
  Result := Add;
  Result.Name := AName;
  Result.DataType := AType;
  if ASize <> -1 then
    Result.Size := ASize;
  if AParamType <> ptUnknown then
    Result.ParamType := AParamType;
end;

{ ---------------------------------------------------------------------------- }
function TFDParams.CreateParam(AFldType: TFieldType;
  const AParamName: String; AParamType: TParamType): TFDParam;
begin
  Result := Add(AParamName, AFldType, -1, AParamType);
end;

{ ---------------------------------------------------------------------------- }
function TFDParams.FindParam(const AValue: String): TFDParam;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do begin
    Result := Items[i];
    if {$IFDEF FireDAC_NOLOCALE_META} CompareText {$ELSE} AnsiCompareText {$ENDIF}
       (Result.Name, AValue) = 0 then
      Exit;
  end;
  Result := nil;
end;

{ ---------------------------------------------------------------------------- }
function TFDParams.ParamByName(const AValue: String): TFDParam;
begin
  Result := FindParam(AValue);
  if Result = nil then
    DatabaseErrorFmt(SParameterNotFound, [AValue], GetDataSet);
end;

{ ---------------------------------------------------------------------------- }
function TFDParams.FindParam(const AValue: Integer): TFDParam;
var
  i: Integer;
begin
  if (AValue >= 1) and (AValue <= Count) then begin
    Result := Items[AValue - 1];
    if Result.Position = AValue then
      Exit;
  end;
  for i := 0 to Count - 1 do begin
    Result := Items[i];
    if Result.Position = AValue then
      Exit;
  end;
  Result := nil;
end;

{ ---------------------------------------------------------------------------- }
function TFDParams.ParamByPosition(const AValue: Integer): TFDParam;
begin
  Result := FindParam(AValue);
  if Result = nil then
    DatabaseErrorFmt(SParameterNotFound, ['#' + IntToStr(AValue)], GetDataSet);
end;

{ ---------------------------------------------------------------------------- }
function TFDParams.GetItem(Index: Integer): TFDParam;
begin
  Result := TFDParam(inherited Items[Index]);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParams.SetItem(Index: Integer; const AValue: TFDParam);
begin
  inherited Items[Index] := AValue;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParams.Assign(AValue: TPersistent);
begin
  if AValue = Self then
    Exit;
  if AValue is TFDParams then begin
    FBindMode := TFDParams(AValue).BindMode;
    FMarkers.Assign(TFDParams(AValue).Markers);
  end;
  inherited Assign(AValue);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParams.AssignValues(AValue: TFDParams;
  AParamTypes: TParamTypes = [ptUnknown .. ptResult]);
var
  i, j, n: Integer;
  oPar1, oPar2: TFDParam;
begin
  if AValue = Self then
    Exit;
  if BindMode = pbByName then
    for i := AValue.Count - 1 downto 0 do begin
      oPar1 := AValue[i];
      for j := 0 to Count - 1 do begin
        oPar2 := Items[j];
        if ({$IFDEF FireDAC_NOLOCALE_META} CompareText {$ELSE} AnsiCompareText {$ENDIF}
            (oPar1.Name, oPar2.Name) = 0) and
           (not (Assigned(DataSet) and (csDesigning in DataSet.ComponentState)) or
            (oPar2.DataType = ftUnknown) or (oPar2.DataType = oPar1.DataType)) and
           (oPar2.ParamType in AParamTypes) then
          oPar2.AssignParam(oPar1);
      end;
    end
  else begin
    n := AValue.Count;
    if n > Count then
      n := Count;
    for i := 0 to n - 1 do begin
      oPar2 := Items[i];
      if oPar2.ParamType in AParamTypes then
        oPar2.AssignParam(AValue[i]);
    end;
  end;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParams.Prepare(ADefaultDataType: TFieldType;
  ADefaultParamType: TParamType);
var
  i: Integer;
  oPar: TFDParam;
  lNoName, lNoPos: Boolean;
begin
  if Count = 0 then
    Exit;
  lNoName := True;
  lNoPos := True;
  for i := 0 to Count - 1 do begin
    oPar := Items[i];
    if oPar.DataType = ftUnknown then
      oPar.DataType := ADefaultDataType;
    if oPar.ParamType = ptUnknown then
      oPar.ParamType := ADefaultParamType;
    if oPar.Size < 0 then
      oPar.Size := 0;
    lNoName := lNoName and (oPar.Name = '');
    lNoPos := lNoPos and (oPar.Position <= 0);
  end;
  if lNoPos and ((BindMode = pbByNumber) or lNoName) then begin
    BindMode := pbByNumber;
    for i := 0 to Count - 1 do
      Items[i].Position := i + 1;
  end
  else if lNoName and not lNoPos then
    BindMode := pbByNumber;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParams.GetParamList(AList: TFDObjList; const AParamNames: String);
var
  iPos: Integer;
begin
  iPos := 1;
  while iPos <= Length(AParamNames) do
    AList.Add(ParamByName(FDExtractFieldName(AParamNames, iPos)));
end;

{ ---------------------------------------------------------------------------- }
function TFDParams.IsEqual(AValue: TFDParams): Boolean;
var
  i: Integer;
begin
  Result := (Count = AValue.Count);
  if Result then
    for i := 0 to Count - 1 do begin
      Result := Items[i].IsEqual(AValue.Items[i]);
      if not Result then
        Break;
    end
end;

{ ---------------------------------------------------------------------------- }
function TFDParams.GetArraySize: Integer;
begin
  if (Count = 0) or (Items[0].ArrayType <> atArray) then
    Result := 1
  else
    Result := Items[0].ArraySize;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParams.SetArraySize(const AValue: Integer);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].ArraySize := AValue;
end;

{ ---------------------------------------------------------------------------- }
function TFDParams.GetParamValues(const AParamName: UnicodeString): Variant;
var
  i: Integer;
  oParams: TFDObjList;
begin
  if Pos(';', AParamName) <> 0 then begin
    oParams := TFDObjList.Create;
    try
      GetParamList(oParams, AParamName);
      Result := VarArrayCreate([0, oParams.Count - 1], varVariant);
      for I := 0 to oParams.Count - 1 do
        Result[I] := TFDParam(oParams[I]).Value;
    finally
      FDFree(oParams);
    end;
  end
  else
    Result := ParamByName(AParamName).Value;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParams.SetParamValues(const AParamName: UnicodeString; const AValue: Variant);
var
  i: Integer;
  oParams: TFDObjList;
begin
  if Pos(';', AParamName) <> 0 then begin
    oParams := TFDObjList.Create;
    try
      GetParamList(oParams, AParamName);
      for i := 0 to oParams.Count - 1 do
        TFDParam(oParams[I]).Value := AValue[I];
    finally
      FDFree(oParams);
    end;
  end
  else
    ParamByName(AParamName).Value := AValue;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParams.ClearValues(AIndex: Integer);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Clear(AIndex);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParams.DefineProperties(AFiler: TFiler);
begin
  inherited DefineProperties(AFiler);
  AFiler.DefineBinaryProperty('Data', ReadBinaryData, nil, False);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParams.ReadBinaryData(Stream: TStream);
var
  I, Temp, NumItems: Integer;
  Buffer: array[0..2047] of Byte;
  Version: Word;
  Bool: Boolean;
  oParam: TFDParam;
begin
  Clear;
  Stream.ReadBuffer(Version, SizeOf(Version));
  if Version > 2 then
    DatabaseError(SInvalidVersion);
  NumItems := 0;
  if Version = 2 then
    Stream.ReadBuffer(NumItems, SizeOf(NumItems))
  else
    Stream.ReadBuffer(NumItems, 2);
  for I := 0 to NumItems - 1 do begin
    oParam := TFDParam(Add);
    Temp := 0;
    if Version = 2 then
      Stream.ReadBuffer(Temp, SizeOf(Temp))
    else
      Stream.ReadBuffer(Temp, 1);
    Stream.ReadBuffer(Buffer, Temp);
    oParam.Name := TFDEncoder.Deco(@Buffer, Temp, ecANSI);
    Stream.ReadBuffer(oParam.FParamType, SizeOf(oParam.FParamType));
    Stream.ReadBuffer(oParam.FDataType, SizeOf(oParam.FDataType));
    if oParam.DataType <> ftUnknown then
    begin
      Temp := 0;
      if Version = 2 then
        Stream.ReadBuffer(Temp, SizeOf(Temp))
      else
        Stream.ReadBuffer(Temp, 2);
      Stream.ReadBuffer(Buffer, Temp);
      if oParam.DataType in [ftBlob, ftGraphic..ftTypedBinary, ftOraBlob, ftOraClob] then
        oParam.SetBlobRawData(Temp, @Buffer)
      else
        oParam.SetData(@Buffer);
    end;
    Stream.ReadBuffer(Bool, SizeOf(Bool));
    if Bool then
      oParam.FValue[0] := Null;
    Stream.ReadBuffer(oParam.FBound, SizeOf(oParam.FBound));
  end;
end;

{ ---------------------------------------------------------------------------- }
{ TFDParam                                                                     }
{ ---------------------------------------------------------------------------- }
constructor TFDParam.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  UpdateData(TFDParams(Collection).ArraySize);
end;

{ ---------------------------------------------------------------------------- }
constructor TFDParam.Create(AParams: TFDParams; AParamType: TParamType);
begin
  Create(AParams);
  ParamType := AParamType;
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.IsEqual(AValue: TFDParam): Boolean;

  function CompareValues(const V1, V2: TFDVariantArray): Boolean;
  var
    i: Integer;
  begin
    Result := Length(V1) = Length(V2);
    if Result then
      for i := 0 to Length(V1) - 1 do
        if VarCompareValue(V1[i], V2[i]) <> vrEqual then begin
          Result := False;
          Break;
        end;
  end;

begin
  Result :=
    (Position = AValue.Position) and (IsCaseSensitive = AValue.IsCaseSensitive) and
    (ArrayType = AValue.ArrayType) and (ArraySize = AValue.ArraySize) and
    (DataType = AValue.DataType) and (ADDataType = AValue.ADDataType) and
    (Precision = AValue.Precision) and (NumericScale = AValue.NumericScale) and
    (Name = AValue.Name) and (ParamType = AValue.ParamType) and
    (Size = AValue.Size) and (IsNull = AValue.IsNull) and (Bound = AValue.Bound) and
    CompareValues(FValue, AValue.FValue);
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.IsParamStored: Boolean;
begin
  Result := Bound and (ArrayType = atScalar);
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetSQLName: String;
begin
  Result := Name;
  if (Result = '') and (Collection <> nil) and
     (TFDParams(Collection).BindMode = pbByNumber) then
    if ParamType <> ptResult then
      Result := 'P' + IntToStr(Position)
    else
      Result := 'RESULT';
  if IsCaseSensitive then
    Result := '"' + Result + '"';
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.ErrIndex(AIndex: Integer);
begin
  if AIndex <= 0 then
    AIndex := 0;
  FDException(Self, [S_FD_LStan], er_FD_StanBadParRowIndex,
    [Name, AIndex, Length(FValue) - 1]);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.CheckIndex(var AIndex: Integer);
begin
  if FValue = nil then
    ErrIndex(AIndex)
  else if AIndex <= 0 then
    AIndex := 0
  else if Length(FValue) <= AIndex then
    ErrIndex(AIndex);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.UpdateData(ADim: Integer);
var
  i, iLen: Integer;
begin
  iLen := Length(FValue);
  if iLen <> ADim then begin
    SetLength(FValue, ADim);
    for i := iLen to ADim - 1 do
      FValue[i] := Null;
  end;
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetArraySize: Integer;
begin
  Result := Length(FValue);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetArraySize(AValue: Integer);
begin
  if ArraySize <> AValue then begin
    if AValue = 1 then begin
      if FArrayType = atArray then
        FArrayType := atScalar;
    end
    else if AValue >= 0 then begin
      if FArrayType = atScalar then
        FArrayType := atArray;
    end;
    UpdateData(AValue);
  end;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetArrayType(AValue: TFDParamArrayType);
begin
  if ArrayType <> AValue then begin
    FArrayType := AValue;
    if (ArrayType = atScalar) and (ArraySize <> 1) then
      ArraySize := 1;
  end;
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetIsNull: Boolean;
begin
  Result := GetIsNulls(0);
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetIsNulls(AIndex: Integer): Boolean;
var
  pData: PVarData;
begin
  CheckIndex(AIndex);
  pData := FindVarData(FValue[AIndex]);
  Result := (pData^.VType = varNull) or (pData^.VType = varEmpty);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetDataType(AValue: TFieldType);
var
  vType, i: Integer;
begin
  if FDataType <> AValue then begin
    FDataType := AValue;
    if Assigned(DataSet) and (csDesigning in DataSet.ComponentState) and
       not GetIsNulls(-1) then begin
      vType := C_FieldType2VarType[AValue];
      if vType <> varError then
        try
          for i := 0 to Length(FValue) - 1 do
            VarCast(FValue[i], FValue[i], vType);
        except
          Clear;
        end
      else
        Clear;
    end
    else
      Clear;
  end;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.ErrUnknownFieldType;
begin
  DatabaseErrorFmt(SUnknownFieldType, [Name], DataSet);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.ErrBadFieldType;
begin
  DatabaseErrorFmt(SBadFieldType, [Name], DataSet);
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetDataLength(AIndex: Integer): Integer; 
var
  pVar: PVariant;

  function GetStrVarLen(out AResult: Integer): Boolean;
  begin
    case VarType(pVar^) of
{$IFNDEF NEXTGEN}
    varString:
      begin
        AResult := Length(AnsiString(TVarData(pVar^).VString));
        Result := True;
      end;
{$ENDIF}
    varOleStr:
      begin
        AResult := FDWideStrLen(TVarData(pVar^).VOleStr);
        Result := True;
      end;
    varUString:
      begin
        AResult := Length(UnicodeString(TVarData(pVar^).VUString));
        Result := True;
      end;
    varByte or varArray:
      begin
        AResult := TVarData(pVar^).VArray^.Bounds[0].ElementCount;
        Result := True;
      end;
    else
      Result := False;
    end;
  end;

  procedure GetVarLen(out AResult: Integer);
  var
    s: String;
  begin
    if not VarIsNull(pVar^) then begin
      s := pVar^;
      AResult := Length(s);
    end;
  end;

  procedure GetVar2Len(out AResult: Integer);
  var
    s: String;
  begin
    if not VarIsNull(pVar^) then begin
      s := VarToStr(pVar^);
      AResult := Length(s);
    end;
  end;

  procedure GetVarWideLen(out AResult: Integer);
  var
    ws: UnicodeString;
  begin
    if not VarIsNull(pVar^) then begin
      ws := pVar^;
      Result := Length(ws);
    end;
  end;

begin
  case DataType of
    ftUnknown:
      begin
        Result := 0;
        ErrUnknownFieldType;
      end;
    ftString, ftFixedChar, ftADT:
      begin
        Result := 0;
        GetVarData(pVar, AIndex);
        if not GetStrVarLen(Result) then
          GetVarLen(Result);
      end;
    ftWideString, ftFixedWideChar:
      begin
        Result := 0;
        GetVarData(pVar, AIndex);
        if not GetStrVarLen(Result) then
          GetVarWideLen(Result);
      end;
    ftBoolean:
      Result := SizeOf(WordBool);
    ftFMTBcd,
    ftBCD:
      Result := SizeOf(TBcd);
    ftCurrency:
      Result := SizeOf(Currency);
    ftSingle:
      Result := SizeOf(Single);
    ftFloat:
      Result := SizeOf(Double);
    ftExtended:
      Result := SizeOf(Extended);
    ftTimeStamp, ftOraTimeStamp:
      Result := SizeOf(TSQLTimeStamp);
    ftParadoxOle, ftOraInterval:
      Result := SizeOf(TFDSQLTimeInterval);
    ftDateTime:
      Result := SizeOf(TDateTime);
    ftTime, ftDate:
      Result := SizeOf(Integer);
    ftAutoInc, ftInteger:
      Result := SizeOf(Integer);
    ftLongWord:
      Result := SizeOf(LongWord);
    ftShortint:
      Result := SizeOf(ShortInt);
    ftByte:
      Result := SizeOf(Byte);
    ftSmallint:
      Result := SizeOf(SmallInt);
    ftWord:
      Result := SizeOf(Word);
    ftLargeint:
      Result := SizeOf(Largeint);
    ftBytes, ftVarBytes:
      begin
        Result := 0;
        GetVarData(pVar, AIndex);
        if VarIsArray(pVar^) then
          Result := VarArrayHighBound(pVar^, 1) + 1
        else if not GetStrVarLen(Result) then
          Result := 0;
      end;
    ftBlob, ftGraphic, ftTypedBinary, ftOraBlob:
      begin
        Result := 0;
        GetVarData(pVar, AIndex);
        if VarIsArray(pVar^) then
          Result := VarArrayHighBound(pVar^, 1) + 1
        else if not GetStrVarLen(Result) then
          GetVar2Len(Result);
      end;
    ftMemo, ftFmtMemo, ftDBaseOle, ftOraClob, ftWideMemo:
      begin
        Result := 0;
        GetVarData(pVar, AIndex);
        if not GetStrVarLen(Result) then
          GetVar2Len(Result);
      end;
    ftArray, ftDataSet, ftReference, ftCursor:
      Result := 0;
    ftGUID:
      Result := SizeOf(TGUID);
  else
    Result := 0;
    ErrBadFieldType;
  end;
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetDataSize(AIndex: Integer): Integer;
begin
  Result := GetDataLength(AIndex);
  case DataType of
    ftFixedWideChar,
    ftWideString:
      Result := (Result + 1) * SizeOf(WideChar);
    ftWideMemo,
    ftFmtMemo,
    ftDBaseOle:
      Result := Result * SizeOf(WideChar);
    ftFixedChar,
    ftString,
    ftAdt:
      Result := (Result + 1) * SizeOf(TFDAnsiChar);
    ftMemo:
      Result := Result * SizeOf(TFDAnsiChar);
  end;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.GetData(ABuffer: PByte; AIndex: Integer);
var
  pVar: PVariant;
  cr: Currency;
  buff: array[0 .. 63] of Char;
  iSize: Integer;

  procedure GetAnsiString(AAddOn: Integer);
  var
    SA: TFDAnsiString;
{$IFDEF NEXTGEN}
    SB: TFDByteString;
{$ENDIF}
  begin
    SA := AsAnsiStrings[AIndex];
{$IFDEF NEXTGEN}
    SB := TFDEncoder.Enco(SA, ecANSI);
    Move(PByte(SB)^, ABuffer^, (Length(SA) + AAddOn) * SizeOf(TFDAnsiChar));
{$ELSE}
    Move(PAnsiChar(SA)^, ABuffer^, (Length(SA) + AAddOn) * SizeOf(AnsiChar));
{$ENDIF}
  end;

  procedure GetWideString(AAddOn: Integer);
  var
    SW: UnicodeString;
  begin
    SW := AsWideStrings[AIndex];
    Move(PWideChar(SW)^, ABuffer^, (Length(SW) + AAddOn) * SizeOf(WideChar));
  end;

  procedure GetByteString;
  var
    SB: TFDByteString;
  begin
    SB := AsByteStrs[AIndex];
    Move(PByte(SB)^, ABuffer^, Length(SB) * SizeOf(Byte));
  end;

begin
  CheckIndex(AIndex);
  case DataType of
    ftUnknown:
      ErrUnknownFieldType;
    ftString, ftFixedChar, ftAdt:
      GetAnsiString(1);
    ftMemo:
      GetAnsiString(0);
    ftWideString, ftFixedWideChar:
      GetWideString(1);
    ftFmtMemo, ftWideMemo, ftDBaseOle:
      GetWideString(0);
    ftShortint:
      PShortInt(ABuffer)^ := ShortInt(AsShortInts[AIndex]);
    ftByte:
      PByte(ABuffer)^ := Byte(AsBytes[AIndex]);
    ftSmallint:
      PSmallInt(ABuffer)^ := SmallInt(AsSmallInts[AIndex]);
    ftWord:
      PWord(ABuffer)^ := Word(AsWords[AIndex]);
    ftAutoInc, ftInteger:
      PInteger(ABuffer)^ := AsIntegers[AIndex];
    ftLongWord:
      PLongWord(ABuffer)^ := AsLongWords[AIndex];
    ftLargeint:
      PInt64(ABuffer)^ := AsLargeInts[AIndex];
    ftTime:
      PDateTimeRec(ABuffer)^.Time := DateTimeToTimeStamp(AsDateTimes[AIndex]).Time;
    ftDate:
      PDateTimeRec(ABuffer)^.Date := DateTimeToTimeStamp(AsDateTimes[AIndex]).Date;
    ftDateTime:
      if ADDataType = dtDateTimeStamp then
        PSQLTimeStamp(ABuffer)^ := AsSQLTimeStamps[AIndex]
      else
        PDateTimeRec(ABuffer)^.DateTime := FDDateTime2MSecs(AsDateTimes[AIndex]);
    ftTimeStamp, ftOraTimeStamp:
      PSQLTimeStamp(ABuffer)^ := AsSQLTimeStamps[AIndex];
    ftParadoxOle, ftOraInterval:
      PFDSQLTimeInterval(ABuffer)^ := AsSQLTimeIntervals[AIndex];
    ftBCD:
      begin
        GetVarData(pVar, AIndex);
        if VarIsFmtBCD(pVar^) then
          PBcd(ABuffer)^ := VarToBCD(pVar^)
        else
        begin
          cr := pVar^;
          FDCurr2Str(buff, iSize, cr, Char(FormatSettings.DecimalSeparator));
          FDStr2BCD(buff, iSize, PBcd(ABuffer)^, Char(FormatSettings.DecimalSeparator));
        end;
      end;
    ftFMTBCD:
      PBcd(ABuffer)^ := AsFMTBCDs[AIndex];
    ftSingle:
      PSingle(ABuffer)^ := AsSingles[AIndex];
    ftFloat:
      PDouble(ABuffer)^ := AsFloats[AIndex];
    ftExtended:
      PExtended(ABuffer)^ := AsExtendeds[AIndex];
    ftCurrency:
      PCurrency(ABuffer)^ := AsCurrencys[AIndex];
    ftBoolean:
      PWord(ABuffer)^ := Ord(AsBooleans[AIndex]);
    ftBytes, ftVarBytes:
      GetBytesValue(AIndex, ABuffer);
    ftBlob, ftGraphic, ftTypedBinary, ftOraBlob, ftOraClob:
      GetByteString;
    ftArray, ftDataSet, ftReference, ftCursor:
      {Nothing};
    ftGUID:
      PGUID(ABuffer)^ := AsGUIDs[AIndex];
  else
    ErrBadFieldType;
  end;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetData(ABuffer: PByte; ALen: LongWord; AIndex: Integer);
var
  crValue: Currency;
  TimeStamp: TTimeStamp;
{$IFNDEF NEXTGEN}
  SA: AnsiString;
{$ENDIF}
  SW: UnicodeString;
begin
  if DataType = ftUnknown then
    ErrUnknownFieldType
  else if ABuffer = nil then
    Clear(AIndex)
  else
    case DataType of
      ftString, ftFixedChar, ftADT:
        begin
          if ALen = $FFFFFFFF then
            if (DataType = ftFixedChar) and (Size > 0) then
              ALen := Size
            else
              ALen := FDAnsiStrLen(PFDAnsiString(ABuffer));
{$IFDEF NEXTGEN}
          Values[AIndex] := TFDEncoder.Deco(ABuffer, ALen, ecANSI);
{$ELSE}
          SetString(SA, PAnsiChar(ABuffer), ALen);
          Values[AIndex] := SA;
{$ENDIF}
        end;
      ftWideString, ftFmtMemo, ftWideMemo, ftFixedWideChar, ftDBaseOle:
        begin
          if ALen = $FFFFFFFF then
            if (DataType = ftFixedWideChar) and (Size > 0) then
              ALen := Size
            else
              ALen := FDWideStrLen(PWideChar(ABuffer));
          SetString(SW, PWideChar(ABuffer), ALen);
          Values[AIndex] := SW;
        end;
      ftShortint:
        AsShortInts[AIndex] := PShortInt(ABuffer)^;
      ftByte:
        AsBytes[AIndex] := PByte(ABuffer)^;
      ftSmallint:
        if ADDataType = dtSByte then
          AsSmallInts[AIndex] := PShortint(ABuffer)^
        else if ADDataType = dtByte then
          AsSmallInts[AIndex] := PByte(ABuffer)^
        else
          AsSmallInts[AIndex] := PSmallint(ABuffer)^;
      ftWord:
        if ADDataType = dtByte then
          AsWords[AIndex] := PByte(ABuffer)^
        else
          AsWords[AIndex] := PWord(ABuffer)^;
      ftInteger, ftAutoInc:
        AsIntegers[AIndex] := PInteger(ABuffer)^;
      ftLongWord:
        AsLongWords[AIndex] := PLongWord(ABuffer)^;
      ftLargeint:
        AsLargeInts[AIndex] := PInt64(ABuffer)^;
      ftTime:
        begin
          TimeStamp.Time := PLongInt(ABuffer)^;
          TimeStamp.Date := DateDelta;
          AsTimes[AIndex] := TimeStampToDateTime(TimeStamp);
        end;
      ftDate:
        begin
          TimeStamp.Time := 0;
          TimeStamp.Date := PInteger(ABuffer)^;
          AsDates[AIndex] := TimeStampToDateTime(TimeStamp);
        end;
      ftDateTime:
        if ADDataType = dtDateTimeStamp then
          AsSQLTimeStamps[AIndex] := PSQLTimeStamp(ABuffer)^
        else
          AsDateTimes[AIndex] := FDMSecs2DateTime(PDouble(ABuffer)^);
      ftTimeStamp, ftOraTimeStamp:
        AsSQLTimeStamps[AIndex] := PSQLTimeStamp(ABuffer)^;
      ftParadoxOle, ftOraInterval:
        AsSQLTimeIntervals[AIndex] := PFDSQLTimeInterval(ABuffer)^;
      ftBCD:
        begin
          crValue := 0.0;
          if BCDToCurr(PBcd(ABuffer)^, crValue) then
            AsBCDs[AIndex] := crValue
          else
            raise EOverFlow.CreateFmt(SFieldOutOfRange, [Name]);
        end;
      ftFMTBcd:
        AsFMTBCDs[AIndex] := PBcd(ABuffer)^;
      ftCurrency:
        AsCurrencys[AIndex] := PCurrency(ABuffer)^;
      ftSingle:
        AsSingles[AIndex] := PSingle(ABuffer)^;
      ftFloat:
        case ADDataType of
        dtFmtBCD:   AsFMTBCDs[AIndex] := PBcd(ABuffer)^;
        dtSingle:   AsSingles[AIndex] := PSingle(ABuffer)^;
        else        AsFloats[AIndex] := PDouble(ABuffer)^;
        end;
      ftExtended:
        AsExtendeds[AIndex] := PExtended(ABuffer)^;
      ftBoolean:
        AsBooleans[AIndex] := PWordBool(ABuffer)^;
      ftCursor:
        begin
          if AIndex < 0 then
            AIndex := 0;
          FValue[AIndex] := 0;
        end;
      ftBytes, ftVarBytes:
        begin
          if ALen = $FFFFFFFF then
            if (DataType = ftBytes) and (Size > 0) then
              ALen := Size
            else
              ALen := FDAnsiStrLen(PFDAnsiString(ABuffer));
          SetBytesValue(AIndex, ABuffer, ALen);
        end;
      ftBlob, ftMemo, ftGraphic, ftTypedBinary, ftOraBlob, ftOraClob:
        begin
          if ALen = $FFFFFFFF then
            ALen := FDAnsiStrLen(PFDAnsiString(ABuffer));
          SetBlobRawData(ALen, ABuffer, AIndex);
        end;
      ftGUID:
        AsGUIDs[AIndex] := PGUID(ABuffer)^;
    else
      ErrBadFieldType;
    end;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.BeginSetBlobRawData(var AIndex: Integer);
begin
  CheckIndex(AIndex);
  if not (DataType in [ftString, ftWideString, ftFixedChar,
                       ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftDBaseOle, ftTypedBinary, ftOraBlob, ftOraClob,
                       ftWideMemo, ftFixedWideChar]) then
    FDataType := ftBlob;
  FBound := True;
  FValue[AIndex] := Null;
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.EndSetBlobRawData(ALen: LongWord; APtr: PByte; AIndex: Integer): PByte;
var
  pData: PVarData;
begin
  if ALen > 0 then begin
    pData := @TVarData(FValue[AIndex]);
    if (DataType in [ftWideString, ftFmtMemo, ftWideMemo, ftFixedWideChar, ftDBaseOle]) or
       (DataType = ftOraClob) and (FADDataType in [dtWideHMemo, dtWideMemo, dtXML]) then begin
      pData^.VType := varUString;
      if APtr <> nil then
        SetString(UnicodeString(pData^.VUString), PWideChar(APtr), ALen)
      else
        SetLength(UnicodeString(pData^.VUString), ALen);
      Result := PByte(PWideChar(UnicodeString(pData^.VUString)));
    end
    else begin
{$IFDEF NEXTGEN}
      FValue[AIndex] := VarArrayCreate([0, ALen - 1], varByte);
      Result := PByte(pData^.VArray^.Data);
      if APtr <> nil then
        Move(APtr^, Result^, ALen);
{$ELSE}
      pData^.VType := varString;
      if APtr <> nil then
        SetString(AnsiString(pData^.VString), PAnsiChar(APtr), ALen)
      else
        SetLength(AnsiString(pData^.VString), ALen);
      Result := PByte(PAnsiChar(AnsiString(pData^.VString)));
{$ENDIF}
    end;
  end
  else
    Result := nil;
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.SetBlobRawData(ALen: LongWord; APtr: PByte; AIndex: Integer): PByte;
begin
  BeginSetBlobRawData(AIndex);
  Result := EndSetBlobRawData(ALen, APtr, AIndex);
end;

{ ---------------------------------------------------------------------------- }
                                                                          
                                                                        
function TFDParam.GetBlobRawData(var ALen: LongWord; var APtr: PByte; AIndex: Integer): Boolean;
  procedure ErrorCantGet;
  begin
    FDException(Self, [S_FD_LStan], er_FD_StanCantGetBlob, []);
  end;
begin
  if not (DataType in [ftString, ftWideString, ftFixedChar,
                       ftBytes, ftVarBytes,
                       ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftDBaseOle, ftTypedBinary, ftOraBlob, ftOraClob,
                       ftWideMemo, ftFixedWideChar]) then
    ErrorCantGet;
  Result := not IsNulls[AIndex];
  if not Result then begin
    ALen := 0;
    APtr := nil;
  end
  else begin
    if AIndex < 0 then
      AIndex := 0;
    case VarType(FValue[AIndex]) of
{$IFNDEF NEXTGEN}
    varString:
      begin
        APtr := PByte(AnsiString(TVarData(FValue[AIndex]).VString));
        ALen := Length(AnsiString(TVarData(FValue[AIndex]).VString));
      end;
{$ENDIF}
    varOleStr:
      begin
        APtr := PByte(TVarData(FValue[AIndex]).VOleStr);
        ALen := FDWideStrLen(TVarData(FValue[AIndex]).VOleStr);
      end;
    varUString:
      begin
        APtr := PByte(PWideChar(UnicodeString(TVarData(FValue[AIndex]).VUString)));
        ALen := Length(UnicodeString(TVarData(FValue[AIndex]).VUString));
      end;
    varArray or varByte:
      begin
        APtr := PByte(TVarData(FValue[AIndex]).VArray^.Data);
        ALen := TVarData(FValue[AIndex]).VArray^.Bounds[0].ElementCount;
      end;
    else
      if DataType in [ftWideString, ftFmtMemo, ftWideMemo, ftFixedWideChar] then
        VarCast(FValue[AIndex], FValue[AIndex], varUString)
      else
        VarCast(FValue[AIndex], FValue[AIndex], varString);
      Result := GetBlobRawData(ALen, APtr, AIndex);
    end;
    if (ALen = 0) and (APtr = nil) then
      APtr := PByte(Self);
  end;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetBytesValue(AIndex: Integer; ABuff: PByte; ASize: LongWord);
var
  V: Variant;
  pDest: Pointer;
begin
  if not (DataType in [ftVarBytes, ftBytes]) then
    DataType := ftBytes;
  V := VarArrayCreate([0, ASize - 1], varByte);
  pDest := VarArrayLock(V);
  try
    Move(ABuff^, pDest^, ASize);
  finally
    VarArrayUnlock(V);
  end;
  Values[AIndex] := V;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.GetBytesValue(AIndex: Integer; ABuff: PByte);
var
  V: Variant;
  P: Pointer;
  sb: TFDByteString;
  pData: PVarData;
begin
  V := Values[AIndex];
  if VarIsArray(V) then begin
    P := VarArrayLock(V);
    try
      Move(P^, ABuff^, VarArrayHighBound(V, 1) + 1);
    finally
      VarArrayUnlock(V);
    end;
  end
  else begin
    pData := FindVarData(V);
    if (pData^.VType <> varNull) and (pData^.VType <> varEmpty) then begin
{$IFNDEF NEXTGEN}
      if pData^.VType = varString then
        sb := TFDByteString(V)
      else
{$ENDIF}
        sb := TFDEncoder.Enco(V, ecANSI);
      Move(PByte(sb)^, ABuff^, Length(sb) * SizeOf(Byte));
    end;
  end;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.Assign(ASource: TPersistent);

  procedure LoadFromStreamPersist(const StreamPersist: IStreamPersist);
  var
    MS: TMemoryStream;
  begin
    MS := TMemoryStream.Create;
    try
      StreamPersist.SaveToStream(MS);
      LoadFromStream(MS, ftGraphic);
    finally
      FDFree(MS);
    end;
  end;

  procedure LoadFromStrings(ASource: TStrings);
  begin
    AsWideMemo := ASource.Text;
  end;

var
  StreamPersist: IStreamPersist;
begin
  if ASource is TFDParam then
    AssignParam(TFDParam(ASource))
  else if ASource is TParam then
    AssignDlpParam(TParam(ASource))
  else if ASource is TField then
    AssignField(TField(ASource))
  else if ASource is TStrings then
    LoadFromStrings(TStrings(ASource))
  else if Supports(ASource, IStreamPersist, StreamPersist) then
    LoadFromStreamPersist(StreamPersist)
  else
    inherited Assign(ASource);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.AssignTo(ADest: TPersistent);
begin
  if ADest is TField then
    TField(ADest).Value := FValue[0]
  else if ADest is TParam then
    AssignToDlpParam(TParam(ADest))
  else
    inherited AssignTo(ADest);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.AssignParam(AParam: TFDParam);
begin
  if AParam <> nil then begin
    FDataType := AParam.DataType;
    FADDataType := AParam.ADDataType;
    FBound := AParam.Bound;
    FName := AParam.Name;
    if FParamType = ptUnknown then
      FParamType := AParam.ParamType;
    FSize := AParam.Size;
    FPrecision := AParam.Precision;
    FNumericScale := AParam.NumericScale;
    FPosition := AParam.Position;
    FIsCaseSensitive := AParam.IsCaseSensitive;
    ArraySize := AParam.ArraySize;
    ArrayType := AParam.ArrayType;
    FValue := Copy(AParam.FValue);
  end;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.AssignDlpParam(AParam: TParam);
begin
  if AParam <> nil then begin
    FDataType := AParam.DataType;
    FADDataType := dtUnknown;
    FBound := AParam.Bound;
    FName := AParam.Name;
    if FParamType = ptUnknown then
      FParamType := AParam.ParamType;
    FSize := AParam.Size;
    FPrecision := AParam.Precision;
    FNumericScale := SmallInt(AParam.NumericScale);
    FPosition := AParam.Index + 1;
    FIsCaseSensitive := False;
    ArraySize := 1;
    ArrayType := atScalar;
    case FDataType of
    ftString, ftFixedChar, ftMemo:
      if AParam.IsNull then
        FValue[0] := Null
      else
        FValue[0] := VarAsType(AParam.Value,
          {$IFDEF NEXTGEN} varUString {$ELSE} varString {$ENDIF});
    ftBytes, ftVarBytes, ftBlob, ftGraphic, ftTypedBinary, ftOraBlob:
      if AParam.IsNull then
        FValue[0] := Null
      else if VarType(AParam.Value) = varOleStr then
        FValue[0] := VarAsType(AParam.Value, varADString)
      else
        FValue[0] := AParam.Value;
    else
      FValue[0] := AParam.Value;
    end;
  end;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.AssignToDlpParam(AParam: TParam);
begin
  if AParam <> nil then begin
    AParam.DataType := FDataType;
    AParam.Bound := FBound;
    AParam.Name := FName;
    if AParam.ParamType = ptUnknown then
      AParam.ParamType := FParamType;
    AParam.Size := FSize;
    AParam.Precision := FPrecision;
    AParam.NumericScale := FNumericScale;
    if Length(FValue) > 0 then
      AParam.Value := FValue[0]
    else
      AParam.Value := Null;
  end;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.AssignFieldValue(AField: TField; const AValue: Variant);
begin
  if AField <> nil then begin
    if (AField.DataType = ftString) and TStringField(AField).FixedChar then
      DataType := ftFixedChar
    else if (AField.DataType = ftMemo) and (AField.Size > 255) then
      DataType := ftString
    else if (AField.DataType = ftFmtMemo) and (AField.Size > 255) then
      DataType := ftWideString
    else if (AField.DataType = ftWideString) and TWideStringField(AField).FixedChar then
      DataType := ftFixedWideChar
    else if (AField.DataType = ftWideMemo) and (AField.Size > 255) then
      DataType := ftWideString
    else
      DataType := AField.DataType;
    FADDataType := dtUnknown;
    if DataType = ftWideString then
      Size := AField.Size
    else
      Size := AField.DataSize;
    if AField.DataType in [ftBcd, ftFMTBcd] then
      NumericScale := AField.Size;
    ArraySize := 1;
    ArrayType := atScalar;
    FBound := True;
    if VarIsNull(AValue) then
      Clear
    else
      Value := AValue;
  end;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.AssignFieldValue(AField: TField);
begin
  if AField <> nil then
    AssignFieldValue(AField, AField.Value);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.AssignField(AField: TField);
begin
  if AField <> nil then begin
    AssignFieldValue(AField, AField.Value);
    Name := AField.FieldName;
  end;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.Clear(AIndex: Integer);
var
  i: Integer;
begin
  if (ArrayType = atScalar) then
    FValue[0] := Null
  else if AIndex = -1 then
    for i := 0 to Length(FValue) - 1 do
      FValue[i] := Null
  else begin
    CheckIndex(AIndex);
    FValue[AIndex] := Null;
  end;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.AssignVarRec(const AVarRec: TVarRec; AIndex: Integer = -1);
begin
  case AVarRec.VType of
  vtInteger:       AsIntegers[AIndex] := AVarRec.VInteger;
  vtBoolean:       AsBooleans[AIndex] := AVarRec.VBoolean;
  vtChar:          AsAnsiStrings[AIndex] := {$IFDEF NEXTGEN} String(AVarRec.VChar) {$ELSE}
                                                             AVarRec.VChar {$ENDIF};
  vtExtended:      AsFloats[AIndex] := AVarRec.VExtended^;
  vtString:        AsAnsiStrings[AIndex] := {$IFDEF NEXTGEN} String(AVarRec.VString^) {$ELSE}
                                                             AVarRec.VString^ {$ENDIF};
  vtPointer:       {$IFDEF FireDAC_32} AsIntegers {$ELSE} AsLargeInts {$ENDIF}
                     [AIndex] := NativeInt(AVarRec.VPointer);
  vtPChar:         AsAnsiStrings[AIndex] := {$IFDEF NEXTGEN} String(AVarRec.VPChar) {$ELSE}
                                                             AVarRec.VPChar {$ENDIF};
  vtWideChar:      AsWideStrings[AIndex] := AVarRec.VWideChar;
  vtPWideChar:     AsWideStrings[AIndex] := AVarRec.VPWideChar;
  vtAnsiString:    AsAnsiStrings[AIndex] := TFDAnsiString(AVarRec.VAnsiString);
  vtCurrency:      AsCurrencys[AIndex] := AVarRec.VCurrency^;
  vtVariant:       Values[AIndex] := AVarRec.VVariant^;
  vtWideString:    AsWideStrings[AIndex] := WideString(AVarRec.VWideString);
  vtInt64:         AsLargeInts[AIndex] := AVarRec.VInt64^;
  vtUnicodeString: AsStrings[AIndex] := UnicodeString(AVarRec.VUnicodeString);
  else             ASSERT(False);
  end;
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetDataSet: TDataSet;
begin
  if not Assigned(Collection) then
    Result := nil
  else
    Result := TFDParams(Collection).GetDataSet;
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetDisplayName: String;
begin
  if FName = '' then
    Result := inherited GetDisplayName
  else
    Result := FName;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsBoolean(const AValue: Boolean);
begin
  SetAsBooleans(-1, AValue);
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsBoolean: Boolean;
begin
  Result := GetAsBooleans(-1);
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsByteStr: TFDByteString;
begin
  Result := GetAsByteStrs(-1);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsByteStr(const AValue: TFDByteString);
begin
  SetAsByteStrs(-1, AValue);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsVarByteStr(const AValue: TFDByteString);
begin
  SetAsVarByteStrs(-1, AValue);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsBooleans(AIndex: Integer; const AValue: Boolean);
begin
  FDataType := ftBoolean;
  Values[AIndex] := AValue;
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsBooleans(AIndex: Integer): Boolean;
var
  pVar: PVariant;
begin
  if IsNulls[AIndex] then
    Result := False
  else begin
    GetVarData(pVar, AIndex);
    Result := pVar^;
  end;
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsByteStrs(AIndex: Integer): TFDByteString;
begin
  if IsNulls[AIndex] then
    SetLength(Result, 0)
  else begin
    SetLength(Result, GetDataSize(AIndex));
    GetBytesValue(AIndex, PByte(Result));
  end;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsByteStrs(AIndex: Integer; const AValue: TFDByteString);
begin
  FDataType := ftBytes;
  SetBytesValue(AIndex, PByte(AValue), Length(AValue) * SizeOf(Byte));
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsVarByteStrs(AIndex: Integer; const AValue: TFDByteString);
begin
  FDataType := ftVarBytes;
  SetBytesValue(AIndex, PByte(AValue), Length(AValue) * SizeOf(Byte));
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsSingle(const AValue: Single);
begin
  SetAsSingles(-1, AValue);
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsSingle: Single;
begin
  Result := GetAsSingles(-1);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsSingles(AIndex: Integer; const AValue: Single);
begin
  FDataType := ftSingle;
  Values[AIndex] := AValue;
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsSingles(AIndex: Integer): Single;
var
  pVar: PVariant;
begin
  if IsNulls[AIndex] then
    Result := 0.0
  else begin
    GetVarData(pVar, AIndex);
    Result := pVar^;
  end;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsFloat(const AValue: Double);
begin
  SetAsFloats(-1, AValue);
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsFloat: Double;
begin
  Result := GetAsFloats(-1);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsFloats(AIndex: Integer; const AValue: Double);
begin
  FDataType := ftFloat;
  Values[AIndex] := AValue;
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsFloats(AIndex: Integer): Double;
var
  pVar: PVariant;
begin
  if IsNulls[AIndex] then
    Result := 0.0
  else begin
    GetVarData(pVar, AIndex);
    Result := pVar^;
  end;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsExtended(const AValue: Extended);
begin
  SetAsExtendeds(-1, AValue);
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsExtended: Extended;
begin
  Result := GetAsExtendeds(-1);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsExtendeds(AIndex: Integer; const AValue: Extended);
begin
  FDataType := ftExtended;
  Values[AIndex] := AValue;
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsExtendeds(AIndex: Integer): Extended;
var
  pVar: PVariant;
begin
  if IsNulls[AIndex] then
    Result := 0.0
  else begin
    GetVarData(pVar, AIndex);
    Result := pVar^;
  end;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsCurrency(const AValue: Currency);
begin
  SetAsCurrencys(-1, AValue);
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsCurrency: Currency;
begin
  Result := GetAsCurrencys(-1);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsCurrencys(AIndex: Integer; const AValue: Currency);
begin
  FDataType := ftCurrency;
  Values[AIndex] := AValue;
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsCurrencys(AIndex: Integer): Currency;
var
  pVar: PVariant;
begin
  if IsNulls[AIndex] then
    Result := 0.0
  else begin
    GetVarData(pVar, AIndex);
    Result := pVar^;
  end;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsBCD(const AValue: Currency);
begin
  SetAsBCDs(-1, AValue);
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsBCD: Currency;
begin
  Result := GetAsBCDs(-1);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsBCDs(AIndex: Integer; const AValue: Currency);
begin
  FDataType := ftBCD;
  Values[AIndex] := AValue;
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsBCDs(AIndex: Integer): Currency;
var
  pVar: PVariant;
begin
  if IsNulls[AIndex] then
    Result := 0.0
  else begin
    GetVarData(pVar, AIndex);
    Result := pVar^;
  end;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsFMTBCD(const AValue: TBcd);
begin
  SetAsFMTBCDs(-1, AValue);
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsFMTBCD: TBcd;
begin
  Result := GetAsFMTBCDs(-1);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsFMTBCDs(AIndex: Integer; const AValue: TBcd);
begin
  FDataType := ftFMTBCD;
  Values[AIndex] := VarFMTBcdCreate(AValue);
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsFMTBCDs(AIndex: Integer): TBcd;
var
  pVar: PVariant;
begin
  if IsNulls[AIndex] then
    Result := NullBcd
  else begin
    GetVarData(pVar, AIndex);
    Result := VarToBcd(pVar^);
  end;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsSQLTimeStamp(const AValue: TSQLTimeStamp);
begin
  SetAsSQLTimeStamps(-1, AValue);
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsSQLTimeStamp: TSQLTimeStamp;
begin
  Result := GetAsSQLTimeStamps(-1);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsSQLTimeStamps(AIndex: Integer; const AValue: TSQLTimeStamp);
begin
  if FDataType <> ftOraTimeStamp then
    FDataType := ftTimeStamp;
  Values[AIndex] := VarSQLTimeStampCreate(AValue);
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsSQLTimeStamps(AIndex: Integer): TSQLTimeStamp;
var
  pVar: PVariant;
begin
  if IsNulls[AIndex] then
    Result := NullSQLTimeStamp
  else begin
    GetVarData(pVar, AIndex);
    Result := FDVar2SQLTimeStamp(pVar^);
  end;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsSQLTimeInterval(const AValue: TFDSQLTimeInterval);
begin
  SetAsSQLTimeIntervals(-1, AValue);
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsSQLTimeInterval: TFDSQLTimeInterval;
begin
  Result := GetAsSQLTimeIntervals(-1);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsSQLTimeIntervals(AIndex: Integer; const AValue: TFDSQLTimeInterval);
begin
  FDataType := ftOraInterval;
  Values[AIndex] := FDVarSQLTimeIntervalCreate(AValue);
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsSQLTimeIntervals(AIndex: Integer): TFDSQLTimeInterval;
var
  pVar: PVariant;
begin
  if IsNulls[AIndex] then
    Result := C_NullSQLTimeInterval
  else begin
    GetVarData(pVar, AIndex);
    Result := FDVar2SQLTimeInterval(pVar^);
  end;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsShortInt(const AValue: LongInt);
begin
  SetAsShortInts(-1, AValue);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsShortInts(AIndex: Integer; const AValue: LongInt);
begin
  FDataType := ftShortint;
  Values[AIndex] := AValue;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsByte(const AValue: LongWord);
begin
  SetAsBytes(-1, AValue);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsBytes(AIndex: Integer; const AValue: LongWord);
begin
  FDataType := ftByte;
  Values[AIndex] := Integer(AValue);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsSmallInt(const AValue: LongInt);
begin
  SetAsSmallInts(-1, AValue);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsSmallInts(AIndex: Integer; const AValue: LongInt);
begin
  FDataType := ftSmallint;
  Values[AIndex] := AValue;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsWord(const AValue: LongWord);
begin
  SetAsWords(-1, AValue);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsWords(AIndex: Integer; AValue: LongWord);
begin
  FDataType := ftWord;
  Values[AIndex] := Integer(AValue);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsInteger(const AValue: Longint);
begin
  SetAsIntegers(-1, AValue);
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsInteger: Longint;
begin
  Result := GetAsIntegers(-1);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsIntegers(AIndex: Integer; const AValue: Longint);
begin
  FDataType := ftInteger;
  Values[AIndex] := AValue;
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsIntegers(AIndex: Integer): Longint;
var
  pVar: PVariant;
begin
  if IsNulls[AIndex] then
    Result := 0
  else begin
    GetVarData(pVar, AIndex);
    Result := pVar^;
  end;
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsLongWord: LongWord;
begin
  Result := GetAsLongWords(-1);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsLongWord(const AValue: LongWord);
begin
  SetAsLongWords(-1, AValue);
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsLongWords(AIndex: Integer): LongWord;
var
  pVar: PVariant;
begin
  if IsNulls[AIndex] then
    Result := 0
  else begin
    GetVarData(pVar, AIndex);
    Result := pVar^;
  end;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsLongWords(AIndex: Integer; const AValue: LongWord);
begin
  FDataType := ftLongWord;
  Values[AIndex] := Integer(AValue);
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsLargeInt: LargeInt;
begin
  Result := GetAsLargeInts(-1);
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsLargeInts(AIndex: Integer): LargeInt;
var
  pVar: PVariant;
begin
  if IsNulls[AIndex] then
    Result := 0
  else begin
    GetVarData(pVar, AIndex);
    Result := pVar^;
  end;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsLargeInt(const AValue: LargeInt);
begin
  SetAsLargeInts(-1, AValue);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsLargeInts(AIndex: Integer; const AValue: LargeInt);
begin
  FDataType := ftLargeint;
  Values[AIndex] := AValue;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsString(const AValue: String);
begin
  SetAsStrings(-1, AValue);
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsString: String;
begin
  Result := GetAsStrings(-1);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsStrings(AIndex: Integer; const AValue: String);
begin
  if not (FDataType in [ftString, ftFixedChar, ftWideString, ftFixedWideChar]) then
    FDataType := {$IFDEF NEXTGEN} ftWideString {$ELSE} ftString {$ENDIF};
  Values[AIndex] := AValue;
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsStrings(AIndex: Integer): String;
var
  pVar: PVariant;
begin
  if IsNulls[AIndex] then
    Result := ''
  else if DataType = ftBoolean then begin
    if Values[AIndex] then
      Result := S_FD_True
    else
      Result := S_FD_False;
  end
  else begin
    GetVarData(pVar, AIndex);
    Result := pVar^;
  end;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsFixedChar(const AValue: String);
begin
  SetAsFixedChars(-1, AValue);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsFixedChars(AIndex: Integer; const AValue: String);
begin
  if not (FDataType in [ftString, ftFixedChar, ftWideString, ftFixedWideChar]) then
    FDataType := ftFixedChar;
  Values[AIndex] := AValue;
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsAnsiString: TFDAnsiString;
begin
  Result := GetAsAnsiStrings(-1);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsAnsiString(const AValue: TFDAnsiString);
begin
  SetAsAnsiStrings(-1, AValue);
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsAnsiStrings(AIndex: Integer): TFDAnsiString;
var
  pVar: PVariant;
begin
  if IsNulls[AIndex] then
    Result := ''
  else if DataType = ftBoolean then begin
    if Values[AIndex] then
      Result := S_FD_True
    else
      Result := S_FD_False;
  end
  else begin
    GetVarData(pVar, AIndex);
    Result := TFDAnsiString(pVar^);
  end;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsAnsiStrings(AIndex: Integer; const AValue: TFDAnsiString);
begin
  if not (FDataType in [ftString, ftFixedChar]) then
    FDataType := ftString;
  Values[AIndex] := AValue;
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsWideString: UnicodeString;
begin
  Result := GetAsWideStrings(-1);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsWideString(const AValue: UnicodeString);
begin
  SetAsWideStrings(-1, AValue);
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsWideStrings(AIndex: Integer): UnicodeString;
var
  pVar: PVariant;
begin
  if IsNulls[AIndex] then
    Result := ''
  else begin
    GetVarData(pVar, AIndex);
    Result := pVar^;
  end;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsWideStrings(AIndex: Integer; const AValue: UnicodeString);
begin
  if not (DataType in [ftWideString, ftFixedWideChar]) then
    DataType := ftWideString;
  Values[AIndex] := AValue;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsXML(const AValue: UnicodeString);
begin
  SetAsXMLs(-1, AValue);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsXMLs(AIndex: Integer; const AValue: UnicodeString);
begin
  DataType := ftDBaseOle;
  Values[AIndex] := AValue;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsDate(const AValue: TDateTime);
begin
  SetAsDates(-1, AValue);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsDates(AIndex: Integer; const AValue: TDateTime);
begin
  FDataType := ftDate;
  Values[AIndex] := AValue;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsTime(const AValue: TDateTime);
begin
  SetAsTimes(-1, AValue);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsTimes(AIndex: Integer; const AValue: TDateTime);
begin
  FDataType := ftTime;
  Values[AIndex] := AValue;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsDateTime(const AValue: TDateTime);
begin
  SetAsDateTimes(-1, AValue);
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsDateTime: TDateTime;
begin
  Result := GetAsDateTimes(-1);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsDateTimes(AIndex: Integer; const AValue: TDateTime);
begin
  FDataType := ftDateTime;
  Values[AIndex] := AValue;
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsDateTimes(AIndex: Integer): TDateTime;
var
  pVar: PVariant;
begin
  if IsNulls[AIndex] then
    Result := 0.0
  else begin
    GetVarData(pVar, AIndex);
    Result := VarToDateTime(pVar^);
  end;
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsGUID: TGUID;
begin
  Result := GetAsGUIDs(-1);
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsGUIDs(AIndex: Integer): TGUID;
var
  pVar: PVariant;
begin
  if IsNulls[AIndex] then
    FillChar(Result, SizeOf(TGUID), 0)
  else begin
    GetVarData(pVar, AIndex);
    Result := StringToGUID(VarToStr(pVar^));
  end;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsGUID(const AValue: TGUID);
begin
  SetAsGUIDs(-1, AValue);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsGUIDs(AIndex: Integer; const AValue: TGUID);
begin
  DataType := ftGuid;
  Values[AIndex] := GUIDToString(AValue);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsVariant(const AValue: Variant);
begin
  SetAsVariants(-1, AValue);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.InternalSetValue(AIndex: Integer; const AValue: Variant);
var
  iLen: Integer;
  pData: PVarData;

  procedure ErrorValueToLong;
  begin
    FDException(Self, [S_FD_LStan], er_FD_StanParamOverflow, [DisplayName, iLen, Size]);
  end;

begin
  if (Size > 0) and
     (DataType in [ftWideString, ftFmtMemo, ftString, ftFixedChar, ftMemo,
                   ftOraClob, ftADT, ftBytes, ftVarBytes, ftBlob, ftOraBlob,
                   ftWideMemo, ftFixedWideChar, ftDBaseOle]) then begin
    pData := FindVarData(AValue);
    case pData^.VType of
{$IFNDEF NEXTGEN}
    varString:
      begin
        iLen := Length(AnsiString(pData^.VString));
        if iLen > Size then
          ErrorValueToLong;
      end;
{$ENDIF}
    varOleStr:
      begin
        iLen := FDWideStrLen(pData^.VOleStr);
        if iLen > Size then
          ErrorValueToLong;
      end;
    varUString:
      begin
        iLen := Length(UnicodeString(pData^.VUString));
        if iLen > Size then
          ErrorValueToLong;
      end;
    varArray or varByte:
      begin
        iLen := VarArrayHighBound(AValue, 1) + 1;
        if iLen > Size then
          ErrorValueToLong;
      end;
    end;
  end;
  FValue[AIndex] := AValue;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsVariants(AIndex: Integer; const AValue: Variant);
var
  tp: TVarType;
begin
  CheckIndex(AIndex);
  if ArrayType = atScalar then
    FBound := not VarIsEmpty(AValue)
  else
    FBound := True;
  tp := FindVarData(AValue)^.VType;
  if FDataType = ftUnknown then
    case tp of
      varShortInt:
        FDataType := ftShortint;
      varByte:
        FDataType := ftByte;
      varSmallint:
        FDataType := ftSmallInt;
      varWord:
        FDataType := ftWord;
      varLongWord:
        FDataType := ftLongWord;
      varInteger:
        FDataType := ftInteger;
      varCurrency:
        FDataType := ftCurrency;
      varSingle:
        FDataType := ftSingle;
      varDouble:
        FDataType := ftFloat;
      varDate:
        FDataType := ftDateTime;
      varBoolean:
        FDataType := ftBoolean;
{$IFDEF NEXTGEN}
      varArray or varByte,
{$ENDIF}
      varUString,
      varString,
      varOleStr:
        if not (FDataType in [ftString, ftFixedChar, ftWideString, ftFixedWideChar]) then
          FDataType := {$IFDEF NEXTGEN} ftWideString {$ELSE} ftString {$ENDIF};
{$IFNDEF NEXTGEN}
      varArray or varByte:
        FDataType := ftBlob;
{$ENDIF}
      varUInt64,
      varInt64:
        FDataType := ftLargeInt;
    else
      if tp = VarFMTBcd then
        FDataType := ftFMTBcd
      else
      if tp = VarSQLTimeStamp then
        FDataType := ftTimeStamp
      else if tp = FDVarSQLTimeInterval then
        FDataType := ftOraInterval
      else
        FDataType := ftUnknown;
    end;
  if not ((FDataType in [ftWideString, ftFmtMemo, ftWideMemo, ftFixedWideChar, ftDBaseOle]) or
          (FDataType = ftOraClob) and (FADDataType in [dtWideHMemo, dtWideMemo, dtXML])) and
         ((tp = varOleStr) or (tp = varUString)) then
    InternalSetValue(AIndex, VarAsType(AValue, varADAString))
  else if ((FDataType in [ftWideString, ftFmtMemo, ftWideMemo, ftFixedWideChar, ftDBaseOle]) or
           (FDataType = ftOraClob) and (FADDataType in [dtWideHMemo, dtWideMemo, dtXML])) and
          (tp = varString) then
    InternalSetValue(AIndex, VarAsType(AValue, varADWString))
  else
    InternalSetValue(AIndex, AValue);
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsVariant: Variant;
begin
  Result := GetAsVariants(-1);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.GetVarData(out AVar: PVariant; AIndex: Integer = -1);
begin
  CheckIndex(AIndex);
  AVar := @(FValue[AIndex]);
end;

{ ---------------------------------------------------------------------------- }
function TFDParam.GetAsVariants(AIndex: Integer): Variant;
var
  pVar: PVariant;
  iLen: Integer;
  pRes, pSrc: Pointer;
begin
  GetVarData(pVar, AIndex);
  if VarType(pVar^) = (varArray or varByte) then begin
    iLen := VarArrayHighBound(pVar^, 1);
    Result := VarArrayCreate([0, iLen], varByte);
    pRes := VarArrayLock(Result);
    pSrc := VarArrayLock(pVar^);
    try
      Move(pSrc^, pRes^, iLen + 1);
    finally
      VarArrayUnlock(Result);
      VarArrayUnlock(pVar^);
    end;
  end
  else
    Result := pVar^;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsMemo(const AValue: TFDAnsiString);
begin
  SetAsMemos(-1, AValue);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsMemos(AIndex: Integer; const AValue: TFDAnsiString);
begin
  if not (FDataType in [ftMemo, ftBlob, ftOraBlob, ftOraClob, ftWideMemo]) then
    FDataType := ftMemo;
  Values[AIndex] := AValue;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsBlob(const AValue: TFDByteString);
begin
  SetAsBlobs(-1, AValue);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsBlobs(AIndex: Integer; const AValue: TFDByteString);
begin
  if not (FDataType in [ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftDBaseOle,
                        ftTypedBinary, ftOraBlob, ftOraClob, ftWideMemo]) then
    FDataType := ftBlob;
  Values[AIndex] := AValue;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsWideMemo(const AValue: UnicodeString);
begin
  SetAsWideMemos(-1, AValue);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetAsWideMemos(AIndex: Integer; const AValue: UnicodeString);
begin
  FDataType := ftWideMemo;
  Values[AIndex] := AValue;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetText(const AValue: String);
begin
  SetTexts(0, AValue);
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.SetTexts(AIndex: Integer; const AValue: String);
begin
  Values[AIndex] := AValue;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.LoadFromFile(const AFileName: String; ABlobType: TBlobType;
  AIndex: Integer);
var
  oStream: TStream;
begin
  oStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    LoadFromStream(oStream, ABlobType, AIndex);
  finally
    FDFree(oStream);
  end;
end;

{ ---------------------------------------------------------------------------- }
procedure TFDParam.LoadFromStream(AStream: TStream; ABlobType: TBlobType;
  AIndex: Integer);
const
  CBufSize = $7FFF;
var
  iLen: LongInt;
  iRead: Integer;
begin
  BeginSetBlobRawData(AIndex);
  FDataType := ABlobType;
  AStream.Position := 0;
  iLen := AStream.Size;
  if iLen < 0 then begin
    iLen := 0;
    repeat
      iRead := AStream.Read((EndSetBlobRawData(iLen + CBufSize, nil, AIndex) + iLen)^, CBufSize);
      Inc(iLen, iRead);
    until iRead < CBufSize;
    EndSetBlobRawData(iLen, nil, AIndex);
  end
  else
    AStream.ReadBuffer(EndSetBlobRawData(iLen, nil, AIndex)^, iLen);
end;

{ ---------------------------------------------------------------------------- }
initialization

  C_FieldType2VarType[ftFMTBcd] := VarFMTBcd;
  C_FieldType2VarType[ftTimeStamp] := VarSQLTimeStamp;
  C_FieldType2VarType[ftOraInterval] := FDVarSQLTimeInterval;


end.
