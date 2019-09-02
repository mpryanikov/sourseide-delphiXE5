{*******************************************************}
{                                                       }
{               Delphi FireDAC Framework                }
{              FireDAC standard layer API               }
{                                                       }
{ Copyright(c) 2004-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}
{$I FireDAC.inc}

unit FireDAC.Stan.Intf;

interface

uses
  System.SysUtils, System.Classes, System.IniFiles, System.Variants, Data.FmtBcd,
    Data.SqlTimSt,
{$IFDEF NEXTGEN}
  System.Generics.Collections,
{$ENDIF}
  FireDAC.Stan.Consts;

type
  IFDStanComponentReference = interface;
  TFDComponent = class;
  IFDStanObject = interface;
  IFDStanObjectHost = interface;
  IFDStanObjectFactory = interface;
  IFDStanErrorHandler = interface;
  IFDStanExpressionDataSource = interface;
  IFDStanExpressionParser = interface;
  IFDStanExpressionEvaluator = interface;
  IFDStanDefinitionStorage = interface;
  IFDStanDefinition = interface;
  IFDStanDefinitions = interface;
  IFDStanConnectionDef = interface;
  IFDStanConnectionDefs = interface;
  IFDStanAsyncOperation = interface;
  IFDStanAsyncExecutor = interface;
  TFDMoniAdapterHelper = class;
{$IFDEF FireDAC_MONITOR}
  IFDMoniClientOutputHandler = interface;
  IFDMoniClient = interface;
  IFDMoniRemoteClient = interface;
  IFDMoniFlatFileClient = interface;
  IFDMoniCustomClient = interface;
{$ENDIF}
  IFDMoniAdapter = interface;

  { --------------------------------------------------------------------------}
  { Supported DBMS kinds                                                      }
  { --------------------------------------------------------------------------}
  TFDRDBMSKind = (mkUnknown, mkOracle, mkMSSQL, mkMSAccess, mkMySQL,
    mkDB2, mkASA, mkADS, mkInterbase, mkFirebird, mkSQLite, mkPostgreSQL,
    mkNexus, mkDataSnap, mkOther);

  { --------------------------------------------------------------------------}
  { Data types                                                                }
  { --------------------------------------------------------------------------}
  TFDDataType = (dtUnknown,                                 // unknown
    dtBoolean,                                              // Boolean
    dtSByte, dtInt16, dtInt32, dtInt64,                     // signed int
    dtByte, dtUInt16, dtUInt32, dtUInt64,                   // unsinged int
    dtSingle, dtDouble, dtExtended,                         // float point numbers
    dtCurrency, dtBCD, dtFmtBCD,                            // high-precision numbers
    dtDateTime, dtTime, dtDate, dtDateTimeStamp,            // date and time
    dtTimeIntervalFull, dtTimeIntervalYM, dtTimeIntervalDS, // time interval
    dtAnsiString, dtWideString, dtByteString,               // string
    dtBlob, dtMemo, dtWideMemo, dtXML,                      // value blobs
    dtHBlob, dtHMemo, dtWideHMemo,                          // handle blobs
    dtHBFile,                                               // external files
    dtRowSetRef, dtCursorRef, dtRowRef,
      dtArrayRef, dtParentRowRef,                           // adt -> ftDataSet, ftCursor, ftADT, ftArray
    dtGUID, dtObject);                                      // adt -> IFDDataStoredObject
  TFDDataTypes = set of TFDDataType;

  TFDDataAttribute = (caSearchable, caAllowNull, caFixedLen,
    caBlobData, caReadOnly, caAutoInc, caROWID, caDefault,
    caRowVersion, caInternal, caCalculated, caVolatile, caUnnamed,
    caVirtual, caBase, caExpr);
  TFDDataAttributes = set of TFDDataAttribute;
  TFDDataOption = (coAllowNull, coUnique, coReadOnly, coInUpdate,
    coInWhere, coInKey, coAfterInsChanged, coAfterUpdChanged);
  TFDDataOptions = set of TFDDataOption;

  TFDCompareDataOption = (coNoCase, coPartial, coNullFirst, coDescNullLast,
    coDescending, coCache, coNullsNotEqual, coNoSymbols);
  TFDCompareDataOptions = set of TFDCompareDataOption;

  TFDSortOption = (soNoCase, soNullFirst, soDescNullLast, soDescending,
    soUnique, soPrimary, soNoSymbols);
  TFDSortOptions = set of TFDSortOption;

  TFDLocateOption = (loPartial, loNearest, loNoCase, loLast, loExcludeKeyRow,
    loUseRowID, loFromCurrent, loBackward);
  TFDLocateOptions = set of TFDLocateOption;

  PFDDataStoredObject = ^IFDDataStoredObject;
  IFDDataStoredObject = interface (IUnknown)
    ['{3E9B315B-F456-4175-A864-B2573C4A2000}']
    function Compare(const AOtherObjInt: IFDDataStoredObject;
      AOptions: TFDCompareDataOptions): Integer;
  end;

  TFDStorageFormat = (sfAuto, sfXML, sfBinary);
  TFDStoreItem = (siData, siDelta, siMeta);
  TFDStoreItems = set of TFDStoreItem;

  { --------------------------------------------------------------------------}
  { Other                                                                     }
  { --------------------------------------------------------------------------}
  PIUnknown = ^IUnknown;
  PLargeInt = ^Int64;
  PULargeInt = ^UInt64;
  TFDVariantArray = array of Variant;
  PPByte = ^PByte;

{$IFDEF FireDAC_64}
  {$IFDEF POSIX}
  TFDLongInt = Int64;
  TFDULongInt = UInt64;
  {$ELSE}
  TFDLongInt = Integer;
  TFDULongInt = LongWord;
  {$ENDIF}
{$ELSE}
  TFDLongInt = Integer;
  TFDULongInt = LongWord;
{$ENDIF}

{$IFNDEF NEXTGEN}
  // XE 4
  TFDByteString = RawByteString;
  TFDAnsiString = AnsiString;
  PFDAnsiString = PAnsiChar;
  TFDAnsiChar = AnsiChar;
{$ELSE}
  // NextGen (iOS / Android)
  TFDByteString = TBytes;
  TFDAnsiString = String;
  PFDAnsiString = PByte;
  TFDAnsiChar = Byte;
  TRecordBuffer = PByte;
{$ENDIF}

  TFDCharSet = {$IFDEF NEXTGEN} TSysCharSet {$ELSE} set of AnsiChar {$ENDIF};
  TFDByteSet = set of Byte;
  TFDStringArray = array of String;

  TFDCounter = {$IFDEF FireDAC_64} Int64 {$ELSE} LongInt {$ENDIF};
  TFDVersion = Int64;

  TFDParseFmtSettings = record
    FDelimiter: Char;
    FQuote: Char;
    FQuote1: Char;
    FQuote2: Char;
  end;
  PFDFormatSettings = ^TFormatSettings;

  TFDEncoding = (ecANSI, ecUTF8, ecUTF16, ecDefault);
  TFDTextEndOfLine = (elDefault, elWindows, elUnix, elMac);
  TFDLocalID = LongWord;

  TFDPtrList = {$IFDEF AUTOREFCOUNT} TList<Pointer> {$ELSE} TList {$ENDIF};
  TFDObjList = {$IFDEF AUTOREFCOUNT} TList<TObject> {$ELSE} TList {$ENDIF};
  TFDClassList = {$IFDEF AUTOREFCOUNT} TList<TClass> {$ELSE} TList {$ENDIF};
  TFDThreadObjList = {$IFDEF AUTOREFCOUNT} TThreadList<TObject> {$ELSE} TThreadList {$ENDIF};

  TFDValWrapper = class (TObject)
  private
    FVal: NativeUInt;
    function GetInt: Integer; inline;
    function GetPtr: Pointer; inline;
  public
    constructor Create(APtr: Pointer); overload;
    constructor Create(AVal: LongWord); overload;
    property AsPtr: Pointer read GetPtr;
    property AsInt: Integer read GetInt;
  end;

  TFDStringList = class (TStringList)
  private
    function GetPtrs(AIndex: Integer): Pointer;
    procedure SetPtrs(AIndex: Integer; const AValue: Pointer);
    function GetInts(AIndex: Integer): LongWord;
    procedure SetInts(AIndex: Integer; const AValue: LongWord);
  protected
{$IFDEF FireDAC_NOLOCALE_META}
    function CompareStrings(const S1, S2: string): Integer; override;
{$ENDIF}
  public
    procedure AddPtr(const AStr: String; APtr: Pointer);
    procedure AddInt(const AStr: String; AInt: LongWord);
    property Ptrs[AIndex: Integer]: Pointer read GetPtrs write SetPtrs;
    property Ints[AIndex: Integer]: LongWord read GetInts write SetInts;
  public
    property UpdateCount;
  end;

  { --------------------------------------------------------------------------}
  { Definition, connection definition                                         }
  { --------------------------------------------------------------------------}
  TFDDefinitionState = (asAdded, asModified, asDeleted, asLoading, asLoaded);
  TFDDefinitionStyle = (atPersistent, atPrivate, atTemporary);

  IFDStanDefinitionStorage = interface(IUnknown)
    ['{3E9B315B-F456-4175-A864-B2573C4A2012}']
    // private
    function GetFileName: String;
    procedure SetFileName(const AValue: String);
    function GetGlobalFileName: String;
    procedure SetGlobalFileName(const AValue: String);
    function GetDefaultFileName: String;
    procedure SetDefaultFileName(const AValue: String);
    // public
    function CreateIniFile: TCustomIniFile;
    // R/O
    function ActualFileName: String;
    // R/W
    property FileName: String read GetFileName write SetFileName;
    property GlobalFileName: String read GetGlobalFileName write SetGlobalFileName;
    property DefaultFileName: String read GetDefaultFileName write SetDefaultFileName;
  end;

  IFDStanDefinition = interface(IUnknown)
    ['{3E9B315B-F456-4175-A864-B2573C4A2013}']
    // private
    function GetName: String;
    function GetState: TFDDefinitionState;
    function GetStyle: TFDDefinitionStyle;
    function GetAsBoolean(const AName: String): LongBool;
    function GetAsInteger(const AName: String): LongInt;
    function GetAsString(const AName: String): String;
    function GetAsXString(const AName: String): String;
    function GetParentDefinition: IFDStanDefinition;
    function GetParams: TStrings;
    function GetOnChanging: TNotifyEvent;
    function GetOnChanged: TNotifyEvent;
    function GetUpdatable: Boolean;
    procedure SetName(const AValue: string);
    procedure SetParams(const AValue: TStrings);
    procedure SetAsBoolean(const AName: String; const AValue: LongBool);
    procedure SetAsYesNo(const AName: String; const AValue: LongBool);
    procedure SetAsInteger(const AName: String; const AValue: LongInt);
    procedure SetAsString(const AName, AValue: String);
    procedure SetParentDefinition(const AValue: IFDStanDefinition);
    procedure SetOnChanging(AValue: TNotifyEvent);
    procedure SetOnChanged(AValue: TNotifyEvent);
    // public
    procedure Apply;
    procedure Clear;
    procedure Cancel;
    procedure Delete;
    procedure MarkPersistent;
    procedure MarkUnchanged;
    procedure OverrideBy(const ADefinition: IFDStanDefinition; AAll: Boolean);
    function ParseString(const AStr: String; AKeywords: TStrings = nil): String; overload;
    function ParseString(const AStr: String; AKeywords: TStrings; const AFmt: TFDParseFmtSettings): String; overload;
    function BuildString(AKeywords: TStrings = nil): String; overload;
    function BuildString(AKeywords: TStrings; const AFmt: TFDParseFmtSettings): String; overload;
    function HasValue(const AName: String): Boolean; overload;
    function HasValue(const AName: String; var ALevel: Integer): Boolean; overload;
    function OwnValue(const AName: String): Boolean;
    function IsSpecified(const AName: String): Boolean;
    procedure ToggleUpdates(APassCode: LongWord; ADisableDelete, ADisableModify: Boolean);
{$IFDEF FireDAC_MONITOR}
    procedure BaseTrace(const AMonitor: IFDMoniClient);
    procedure Trace(const AMonitor: IFDMoniClient);
{$ENDIF}
    property State: TFDDefinitionState read GetState;
    property Style: TFDDefinitionStyle read GetStyle;
    property Updatable: Boolean read GetUpdatable;
    property AsXString[const AName: String]: String read GetAsXString;
    property AsString[const AName: String]: String read GetAsString write SetAsString;
    property AsBoolean[const AName: String]: LongBool read GetAsBoolean write SetAsBoolean;
    property AsYesNo[const AName: String]: LongBool read GetAsBoolean write SetAsYesNo;
    property AsInteger[const AName: String]: LongInt read GetAsInteger write SetAsInteger;
    property ParentDefinition: IFDStanDefinition read GetParentDefinition write SetParentDefinition;
    // published
    property Params: TStrings read GetParams write SetParams;
    property Name: String read GetName write SetName;
    property OnChanging: TNotifyEvent read GetOnChanging write SetOnChanging;
    property OnChanged: TNotifyEvent read GetOnChanged write SetOnChanged;
  end;

  IFDStanDefinitions = interface(IUnknown)
    ['{3E9B315B-F456-4175-A864-B2573C4A2014}']
    // private
    function GetCount: Integer;
    function GetItems(AIndex: Integer): IFDStanDefinition;
    function GetAutoLoad: Boolean;
    function GetStorage: IFDStanDefinitionStorage;
    function GetLoaded: Boolean;
    function GetBeforeLoad: TNotifyEvent;
    function GetAfterLoad: TNotifyEvent;
    function GetUpdatable: Boolean;
    function GetName: String;
    procedure SetAutoLoad(AValue: Boolean);
    procedure SetBeforeLoad(AValue: TNotifyEvent);
    procedure SetAfterLoad(AValue: TNotifyEvent);
    procedure SetName(const AValue: String);
    // public
    function Add: IFDStanDefinition;
    function AddTemporary: IFDStanDefinition;
    function FindDefinition(const AName: String): IFDStanDefinition;
    function DefinitionByName(const AName: String): IFDStanDefinition;
    procedure Cancel;
    procedure Save(AIfModified: Boolean = True);
    function Load: Boolean;
    function Refresh: Boolean;
    procedure Clear;
    procedure BeginRead;
    procedure EndRead;
    procedure BeginWrite;
    procedure EndWrite;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: IFDStanDefinition read GetItems; default;
    property Loaded: Boolean read GetLoaded;
    property Updatable: Boolean read GetUpdatable;
    // published
    property Name: String read GetName write SetName;
    property AutoLoad: Boolean read GetAutoLoad write SetAutoLoad;
    property Storage: IFDStanDefinitionStorage read GetStorage;
    property BeforeLoad: TNotifyEvent read GetBeforeLoad write SetBeforeLoad;
    property AfterLoad: TNotifyEvent read GetAfterLoad write SetAfterLoad;
  end;

  IFDStanConnectionDef = interface(IFDStanDefinition)
    ['{3E9B315B-F456-4175-A864-B2573C4A2015}']
    // private
    function GetPooled: Boolean;
    function GetDriverID: String;
    function GetPassword: String;
    function GetUserName: String;
    function GetDatabase: String;
    function GetNewPassword: String;
    function GetExpandedDatabase: String;
    function GetMonitorBy: String;
    function GetServer: String;
    function GetPort: Integer;
    function GetOSAuthent: Boolean;
    function GetMetaDefSchema: String;
    function GetMetaDefCatalog: String;
    procedure SetPooled(AValue: Boolean);
    procedure SetDriverID(const AValue: String);
    procedure SetPassword(const AValue: String);
    procedure SetUserName(const AValue: String);
    procedure SetDatabase(const AValue: String);
    procedure SetNewPassword(const AValue: String);
    procedure SetMonitorBy(const AValue: String);
    procedure SetServer(const AValue: String);
    procedure SetPortI(const AValue: Integer);
    procedure SetOSAuthent(const AValue: Boolean);
    procedure SetMetaDefSchema(const AValue: String);
    procedure SetMetaDefCatalog(const AValue: String);
    // public
    procedure WriteOptions(AFormatOptions: TObject; AUpdateOptions: TObject;
      AFetchOptions: TObject; AResourceOptions: TObject);
    procedure ReadOptions(AFormatOptions: TObject; AUpdateOptions: TObject;
      AFetchOptions: TObject; AResourceOptions: TObject);
    property UserName: String read GetUserName write SetUserName;
    property Password: String read GetPassword write SetPassword;
    property NewPassword: String read GetNewPassword write SetNewPassword;
    property OSAuthent: Boolean read GetOSAuthent write SetOSAuthent;
    property Server: String read GetServer write SetServer;
    property Port: Integer read GetPort write SetPortI;
    property Database: String read GetDatabase write SetDatabase;
    property ExpandedDatabase: String read GetExpandedDatabase;
    property Pooled: Boolean read GetPooled write SetPooled;
    property DriverID: String read GetDriverID write SetDriverID;
    property MonitorBy: String read GetMonitorBy write SetMonitorBy;
    property MetaDefSchema: String read GetMetaDefSchema write SetMetaDefSchema;
    property MetaDefCatalog: String read GetMetaDefCatalog write SetMetaDefCatalog;
  end;

  IFDStanConnectionDefs = interface(IFDStanDefinitions)
    ['{3E9B315B-F456-4175-A864-B2573C4A2016}']
    // private
    function GetConnectionDefs(AIndex: Integer): IFDStanConnectionDef;
    // public
    function AddConnectionDef: IFDStanConnectionDef;
    function FindConnectionDef(const AName: String): IFDStanConnectionDef;
    function ConnectionDefByName(const AName: String): IFDStanConnectionDef;
    property Items[AIndex: Integer]: IFDStanConnectionDef read GetConnectionDefs; default;
  end;

  { --------------------------------------------------------------------------}
  { Objects and factories                                                     }
  { --------------------------------------------------------------------------}
  IFDStanComponentReference = interface (IUnknown)
    ['{3E9B315B-F456-4175-A864-B2573C4A2029}']
    procedure SetComponentReference(const AValue: IInterfaceComponentReference);
  end;

  TFDComponent = class(TComponent)
  end;

  IFDStanObject = interface (IUnknown)
    ['{3E9B315B-F456-4175-A864-B2573C4A2001}']
    // private
    function GetName: TComponentName;
    function GetParent: IFDStanObject;
    // public
    procedure BeforeReuse;
    procedure AfterReuse;
    procedure SetOwner(const AOwner: TObject; const ARole: TComponentName);
    property Name: TComponentName read GetName;
    property Parent: IFDStanObject read GetParent;
  end;

  IFDStanObjectHost = interface(IUnknown)
    ['{3E9B315B-F456-4175-A864-B2573C4A2002}']
    // private
    function GetObjectKindName: TComponentName;
    // public
    procedure CreateObject(out AObject: IFDStanObject);
    property ObjectKindName: TComponentName read GetObjectKindName;
  end;

  IFDStanObjectFactory = interface(IUnknown)
    ['{3E9B315B-F456-4175-A864-B2573C4A2003}']
    procedure Open(const AHost: IFDStanObjectHost; const ADef: IFDStanDefinition);
    procedure Close;
    procedure Acquire(out AObject: IFDStanObject);
    procedure Release(const AObject: IFDStanObject);
  end;

  { --------------------------------------------------------------------------}
  { Error handling interfaces                                                 }
  { --------------------------------------------------------------------------}
  TFDErrorAction = (eaFail, eaSkip, eaRetry, eaApplied, eaDefault,
    eaExitSuccess, eaExitFailure);

  IFDStanErrorHandler = interface(IUnknown)
    ['{3E9B315B-F456-4175-A864-B2573C4A2007}']
    procedure HandleException(const AInitiator: IFDStanObject; var AException: Exception);
  end;

  { --------------------------------------------------------------------------}
  { Expression evaluation                                                     }
  { --------------------------------------------------------------------------}
  TFDParserOption = (poCheck, poAggregate, poDefaultExpr, poFieldNameGiven);
  TFDParserOptions = set of TFDParserOption;

  TFDExpressionOption = (ekNoCase, ekPartial);
  TFDExpressionOptions = set of TFDExpressionOption;
  TFDExpressionScopeKind = (ckUnknown, ckField, ckAgg, ckConst);

  TFDAggregateKind = (akSum, akAvg, akCount, akMin, akMax, akFirst, akLast);

  IFDStanExpressionDataSource = interface(IUnknown)
    ['{3E9B315B-F456-4175-A864-B2573C4A2008}']
    // private
    function GetVarIndex(const AName: String): Integer;
    function GetVarType(AIndex: Integer): TFDDataType;
    function GetVarScope(AIndex: Integer): TFDExpressionScopeKind;
    function GetVarData(AIndex: Integer): Variant;
    procedure SetVarData(AIndex: Integer; const AValue: Variant);
    function GetSubAggregateValue(AIndex: Integer): Variant;
    function GetPosition: Pointer;
    procedure SetPosition(AValue: Pointer);
    function GetRowNum: Integer;
    function GetDatabase: String;
    function GetUser: String;
    // public
    property VarIndex[const AName: String]: Integer read GetVarIndex;
    property VarType[AIndex: Integer]: TFDDataType read GetVarType;
    property VarScope[AIndex: Integer]: TFDExpressionScopeKind read GetVarScope;
    property VarData[AIndex: Integer]: Variant read GetVarData write SetVarData;
    property SubAggregateValue[AIndex: Integer]: Variant read GetSubAggregateValue;
    property Position: Pointer read GetPosition write SetPosition;
    property RowNum: Integer read GetRowNum;
    property Database: String read GetDatabase;
    property User: String read GetUser;
  end;

  IFDStanExpressionEvaluator = interface(IUnknown)
    ['{3E9B315B-F456-4175-A864-B2573C4A2010}']
    // private
    function GetSubAggregateCount: Integer;
    function GetSubAggregateKind(AIndex: Integer): TFDAggregateKind;
    function GetDataSource: IFDStanExpressionDataSource;
    function GetDataType: TFDDataType;
    // public
    function HandleNotification(AKind: Word; AReason: Word;
      AParam1, AParam2: LongWord): Boolean;
    function Evaluate: Variant;
    // support for aggregates
    function EvaluateSubAggregateArg(AIndex: Integer): Variant;
    property SubAggregateCount: Integer read GetSubAggregateCount;
    property SubAggregateKind[AIndex: Integer]: TFDAggregateKind read GetSubAggregateKind;
    property DataSource: IFDStanExpressionDataSource read GetDataSource;
    property DataType: TFDDataType read GetDataType;
  end;

  IFDStanExpressionParser = interface(IUnknown)
    ['{3E9B315B-F456-4175-A864-B2573C4A2009}']
    // private
    function GetDataSource: IFDStanExpressionDataSource;
    // public
    function Prepare(const ADataSource: IFDStanExpressionDataSource;
      const AExpression: String; AOptions: TFDExpressionOptions;
      AParserOptions: TFDParserOptions; const AFixedVarName: String): IFDStanExpressionEvaluator;
    property DataSource: IFDStanExpressionDataSource read GetDataSource;
  end;

  { --------------------------------------------------------------------------}
  { Async execute                                                             }
  { --------------------------------------------------------------------------}
  TFDStanAsyncMode = (amBlocking, amNonBlocking, amCancelDialog, amAsync);
  TFDStanAsyncState = (asInactive, asExecuting, asFinished, asFailed, asAborted, asExpired);

  IFDStanAsyncHandler = interface(IUnknown)
    ['{3E9B315B-F456-4175-A864-B2573C4A2025}']
    procedure HandleFinished(const AInitiator: IFDStanObject;
      AState: TFDStanAsyncState; AException: Exception);
  end;

  IFDStanAsyncOperation = interface(IUnknown)
    ['{3E9B315B-F456-4175-A864-B2573C4A2022}']
    procedure Execute;
    procedure AbortJob;
    function AbortSupported: Boolean;
  end;

  IFDStanAsyncExecutor = interface(IUnknown)
    ['{3E9B315B-F456-4175-A864-B2573C4A2023}']
    // private
    function GetState: TFDStanAsyncState;
    function GetMode: TFDStanAsyncMode;
    function GetTimeout: LongWord;
    function GetOperation: IFDStanAsyncOperation;
    function GetHandler: IFDStanAsyncHandler;
    // public
    procedure Setup(const AOperation: IFDStanAsyncOperation;
      const AMode: TFDStanAsyncMode; const ATimeout: LongWord;
      const AHandler: IFDStanAsyncHandler; ASilentMode: Boolean);
    procedure Run;
    procedure AbortJob;
    procedure Launched;
    // R/O
    property State: TFDStanAsyncState read GetState;
    property Mode: TFDStanAsyncMode read GetMode;
    property Timeout: LongWord read GetTimeout;
    property Operation: IFDStanAsyncOperation read GetOperation;
    property Handler: IFDStanAsyncHandler read GetHandler;
  end;

  { --------------------------------------------------------------------------}
  { Debug monitor interfaces                                                  }
  { --------------------------------------------------------------------------}
  TFDMoniEventKind = (ekLiveCycle, ekError,
    ekConnConnect, ekConnTransact, ekConnService,
    ekCmdPrepare, ekCmdExecute, ekCmdDataIn, ekCmdDataOut,
    ekAdaptUpdate,
    ekVendor,
    ekComponent);
  TFDMoniEventKinds = set of TFDMoniEventKind;
  PFDMoniEventKinds = ^TFDMoniEventKinds;
  TFDMoniEventStep = (esStart, esProgress, esEnd);
  TFDMoniTracing = (eaAuto, eaTrue, eaFalse);
  TFDTraceFileColumns = set of (tiRefNo, tiTime, tiThreadID, tiClassName,
    tiObjID, tiMsgText);

{$IFDEF FireDAC_MONITOR}
  IFDMoniClientOutputHandler = interface (IUnknown)
    ['{3E9B315B-F456-4175-A864-B2573C4A2028}']
    procedure HandleOutput(const AClassName, AObjName, AMessage: String);
  end;

  IFDMoniClient = interface (IUnknown)
    ['{3E9B315B-F456-4175-A864-B2573C4A2005}']
    // private
    function GetTracing: Boolean;
    procedure SetTracing(const AValue: Boolean);
    function GetName: TComponentName;
    procedure SetName(const AValue: TComponentName);
    function GetEventKinds: TFDMoniEventKinds;
    procedure SetEventKinds(const AValue: TFDMoniEventKinds);
    function GetOutputHandler: IFDMoniClientOutputHandler;
    procedure SetOutputHandler(const AValue: IFDMoniClientOutputHandler);
    // public
    procedure Notify(AKind: TFDMoniEventKind; AStep: TFDMoniEventStep;
      ASender: TObject; const AMsg: String; const AArgs: array of const);
    function RegisterAdapter(const AAdapter: IFDMoniAdapter): LongWord;
    procedure UnregisterAdapter(const AAdapter: IFDMoniAdapter);
    procedure AdapterChanged(const AAdapter: IFDMoniAdapter);
    procedure ResetFailure;
    property Tracing: Boolean read GetTracing write SetTracing;
    property Name: TComponentName read GetName write SetName;
    property EventKinds: TFDMoniEventKinds read GetEventKinds write SetEventKinds;
    property OutputHandler: IFDMoniClientOutputHandler read GetOutputHandler
      write SetOutputHandler;
  end;

  IFDMoniRemoteClient = interface (IFDMoniClient)
    ['{3E9B315B-F456-4175-A864-B2573C4A2026}']
    // private
    function GetHost: String;
    procedure SetHost(const AValue: String);
    function GetPort: Integer;
    procedure SetPortI(const AValue: Integer);
    function GetTimeout: Integer;
    procedure SetTimeout(const AValue: Integer);
    // public
    property Host: String read GetHost write SetHost;
    property Port: Integer read GetPort write SetPortI;
    property Timeout: Integer read GetTimeout write SetTimeout;
  end;

  IFDMoniCustomClient = interface (IFDMoniClient)
    ['{3E9B315B-F456-4175-A864-B2573C4A2030}']
    // private
    function GetSynchronize: Boolean;
    procedure SetSynchronize(AValue: Boolean);
    // public
    property Synchronize: Boolean read GetSynchronize write SetSynchronize;
  end;

  IFDMoniFlatFileClient = interface (IFDMoniCustomClient)
    ['{3E9B315B-F456-4175-A864-B2573C4A2027}']
    // private
    function GetFileName: String;
    procedure SetFileName(const Value: String);
    function GetFileAppend: Boolean;
    procedure SetFileAppend(const Value: Boolean);
    function GetFileColumns: TFDTraceFileColumns;
    procedure SetFileColumns(const Value: TFDTraceFileColumns);
    function GetFileEncoding: TFDEncoding;
    procedure SetFileEncoding(const AValue: TFDEncoding);
    // public
    property FileName: String read GetFileName write SetFileName;
    property FileAppend: Boolean read GetFileAppend write SetFileAppend;
    property FileColumns: TFDTraceFileColumns read GetFileColumns write SetFileColumns;
    property FileEncoding: TFDEncoding read GetFileEncoding write SetFileEncoding;
  end;
{$ENDIF}

  TFDMoniAdapterItemKind = (ikSQL, ikParam, ikStat, ikClientInfo, ikSessionInfo,
    ikFireDACInfo);
  TFDMoniAdapterItemKinds = set of TFDMoniAdapterItemKind;
  TFDMoniAdapterHelper = class(TObject)
  private
    FProxy: IFDStanObject;
    FRole: TComponentName;
    FHandle: LongWord;
    {$IFDEF AUTOREFCOUNT} [Weak] {$ENDIF} FAdaptedObj: TObject;
    {$IFDEF AUTOREFCOUNT} [Weak] {$ENDIF} FParentObj: TObject;
{$IFDEF FireDAC_MONITOR}
    FMoniClient: IFDMoniClient;
{$ENDIF}
    function GetIsRegistered: Boolean;
    function GetName: TComponentName;
    function GetParent: IFDStanObject;
  public
    constructor Create(const AAdapterObj, AParentObj: TObject);
    destructor Destroy; override;
    procedure SetOwner(const AOwner: TObject; const ARole: TComponentName);
{$IFDEF FireDAC_MONITOR}
    procedure RegisterClient(const AMoniClient: IFDMoniClient);
    procedure UnRegisterClient;
{$ENDIF}
    property IsRegistered: Boolean read GetIsRegistered;
    property Name: TComponentName read GetName;
    property Parent: IFDStanObject read GetParent;
    property Handle: LongWord read FHandle;
  end;

  IFDMoniAdapter = interface (IUnknown)
    ['{3E9B315B-F456-4175-A864-B2573C4A2006}']
    // private
    function GetHandle: LongWord;
    function GetItemCount: Integer;
    function GetSupportItems: TFDMoniAdapterItemKinds;
    // public
    procedure GetItem(AIndex: Integer; out AName: String; out AValue: Variant;
      out AKind: TFDMoniAdapterItemKind);
    property Handle: LongWord read GetHandle;
    property ItemCount: Integer read GetItemCount;
    property SupportItems: TFDMoniAdapterItemKinds read GetSupportItems;
  end;

  // next GUID -> xxx031

const
  varADString = varUString;
  varADAString = {$IFDEF NEXTGEN} varUString {$ELSE} varString {$ENDIF};
  varADWString = varUString;
  C_FD_MaxInt64: Int64 = 9223372036854775807;
  C_FD_MinInt64: Int64 = -9223372036854775807;
  C_FD_MaxUTF8Len = 3;

  // Monitor default values
  C_FD_MonitorPort = 8050;
  C_FD_MonitorTimeout = 3000;
  C_FD_MonitorFileName = '$(TEMP)' + C_FD_PathDelim + 'trace$(NEXT).txt';
  C_FD_MonitorAppend = False;
  C_FD_MonitorColumns = [tiRefNo, tiTime, tiMsgText];
  C_FD_MonitorEncoding = 0;

  // DatS
  C_FD_AllTypes = [dtBoolean .. dtObject];
  C_FD_InvariantDataTypes = [dtRowSetRef, dtCursorRef, dtRowRef, dtArrayRef, dtParentRowRef];
  C_FD_BlobTypes = [dtBlob, dtMemo, dtWideMemo, dtXML, dtHBlob, dtHBFile, dtHMemo, dtWideHMemo];
  C_FD_StrTypes = [dtAnsiString, dtWideString, dtByteString];
  C_FD_VarLenTypes = C_FD_BlobTypes + C_FD_StrTypes;
  C_FD_WideTypes = [dtWideString, dtWideMemo, dtXML, dtWideHMemo];
  C_FD_AnsiTypes = [dtAnsiString, dtMemo, dtHMemo];
  C_FD_CharTypes = C_FD_WideTypes + C_FD_AnsiTypes;
  C_FD_NumTypes = [dtSByte, dtInt16, dtInt32, dtInt64,
                   dtByte, dtUInt16, dtUInt32, dtUInt64,
                   dtSingle, dtDouble, dtExtended,
                   dtCurrency, dtBCD, dtFmtBCD];
  C_FD_NumUnsignedTypes = [dtByte, dtUInt16, dtUInt32, dtUInt64];
  C_FD_NonSearchableDataTypes = C_FD_InvariantDataTypes + C_FD_BlobTypes;
  C_FD_DataTypeNames: array[TFDDataType] of String = (
    'Unknown',
    'Boolean',
    'SByte', 'Int16', 'Int32', 'Int64',
    'Byte', 'UInt16', 'UInt32', 'UInt64',
    'Single', 'Double', 'Extended',
    'Currency', 'BCD', 'FmtBCD',
    'DateTime', 'Time', 'Date', 'DateTimeStamp',
    'TimeIntervalFull', 'TimeIntervalYM', 'TimeIntervalDS',
    'AnsiString', 'WideString', 'ByteString',
    'Blob', 'Memo', 'WideMemo', 'XML',
    'HBlob', 'HMemo', 'WideHMemo',
    'HBFile',
    'RowSetRef', 'CursorRef', 'RowRef',
      'ArrayRef', 'ParentRowRef',
    'GUID', 'Object');

implementation

{ ----------------------------------------------------------------------------- }
{ TFDValWrapper                                                                 }
{ ----------------------------------------------------------------------------- }
constructor TFDValWrapper.Create(APtr: Pointer);
begin
  inherited Create;
  FVal := NativeUInt(APtr);
end;

{ ----------------------------------------------------------------------------- }
constructor TFDValWrapper.Create(AVal: LongWord);
begin
  inherited Create;
  FVal := NativeUInt(AVal);
end;

{ ----------------------------------------------------------------------------- }
function TFDValWrapper.GetInt: Integer;
begin
  Result := Integer(FVal);
end;

{ ----------------------------------------------------------------------------- }
function TFDValWrapper.GetPtr: Pointer;
begin
  Result := Pointer(FVal);
end;

{ ----------------------------------------------------------------------------- }
{ TFDStringList                                                                 }
{ ----------------------------------------------------------------------------- }
procedure TFDStringList.AddPtr(const AStr: String; APtr: Pointer);
begin
{$IFDEF AUTOREFCOUNT}
  AddObject(AStr, TFDValWrapper.Create(APtr));
{$ELSE}
  AddObject(AStr, TObject(APtr));
{$ENDIF}
end;

{ ----------------------------------------------------------------------------- }
procedure TFDStringList.AddInt(const AStr: String; AInt: LongWord);
begin
{$IFDEF AUTOREFCOUNT}
  AddObject(AStr, TFDValWrapper.Create(AInt));
{$ELSE}
  AddObject(AStr, TObject(AInt));
{$ENDIF}
end;

{ ----------------------------------------------------------------------------- }
function TFDStringList.GetPtrs(AIndex: Integer): Pointer;
begin
{$IFDEF AUTOREFCOUNT}
  Result := Pointer(TFDValWrapper(inherited Objects[AIndex]).FVal);
{$ELSE}
  Result := Pointer(inherited Objects[AIndex]);
{$ENDIF}
end;

{ ----------------------------------------------------------------------------- }
procedure TFDStringList.SetPtrs(AIndex: Integer; const AValue: Pointer);
begin
{$IFDEF AUTOREFCOUNT}
  inherited Objects[AIndex] := TFDValWrapper.Create(AValue);
{$ELSE}
  inherited Objects[AIndex] := TObject(AValue);
{$ENDIF}
end;

{ ----------------------------------------------------------------------------- }
function TFDStringList.GetInts(AIndex: Integer): LongWord;
begin
{$IFDEF AUTOREFCOUNT}
  Result := LongWord(TFDValWrapper(inherited Objects[AIndex]).FVal);
{$ELSE}
  Result := LongWord(Pointer(inherited Objects[AIndex]));
{$ENDIF}
end;

{ ----------------------------------------------------------------------------- }
procedure TFDStringList.SetInts(AIndex: Integer; const AValue: LongWord);
begin
{$IFDEF AUTOREFCOUNT}
  inherited Objects[AIndex] := TFDValWrapper.Create(AValue);
{$ELSE}
  inherited Objects[AIndex] := TObject(AValue);
{$ENDIF}
end;

{-------------------------------------------------------------------------------}
{$IFDEF FireDAC_NOLOCALE_META}
function TFDStringList.CompareStrings(const S1, S2: string): Integer;
begin
  if CaseSensitive then
    Result := CompareStr(S1, S2)
  else
    Result := CompareText(S1, S2);
end;
{$ENDIF}

{ ----------------------------------------------------------------------------- }
{ TFDPhysMoniAdapterComponentProxy                                              }
{ ----------------------------------------------------------------------------- }
type
  TFDMoniAdapterComponentProxy = class(TInterfacedObject, IFDStanObject)
  private
    {$IFDEF AUTOREFCOUNT} [Weak] {$ENDIF} FComp: TComponent;
  protected
    // IFDStanObject
    function GetName: TComponentName;
    function GetParent: IFDStanObject;
    procedure BeforeReuse;
    procedure AfterReuse;
    procedure SetOwner(const AOwner: TObject; const ARole: TComponentName);
  public
    constructor Create(AComp: TComponent);
    destructor Destroy; override;
  end;

{ ----------------------------------------------------------------------------- }
constructor TFDMoniAdapterComponentProxy.Create(AComp: TComponent);
begin
  inherited Create;
  FComp := AComp;
end;

{ ----------------------------------------------------------------------------- }
destructor TFDMoniAdapterComponentProxy.Destroy;
begin
  inherited Destroy;
end;

{ ----------------------------------------------------------------------------- }
function TFDMoniAdapterComponentProxy.GetName: TComponentName;
begin
  if FComp.GetNamePath = '' then
    Result := FComp.ClassName + '($' + IntToHex(Integer(FComp), 8) + ')'
  else
    Result := FComp.GetNamePath;
end;

{ ----------------------------------------------------------------------------- }
function TFDMoniAdapterComponentProxy.GetParent: IFDStanObject;

  function IsApp(AComp: TComponent): Boolean;
  var
    oClass: TClass;
  begin
    oClass := AComp.ClassType;
    while (oClass <> nil) and not oClass.ClassNameIs('TApplication') do
      oClass := oClass.ClassParent;
    Result := (oClass <> nil);
  end;

begin
  Result := nil;
  if (FComp.Owner <> nil) and not IsApp(FComp.Owner) then
    if FComp.Owner.GetInterfaceEntry(IFDStanObject) = nil then
      Result := TFDMoniAdapterComponentProxy.Create(FComp.Owner) as IFDStanObject
    else
      Supports(FComp.Owner, IFDStanObject, Result);
end;

{ ----------------------------------------------------------------------------- }
procedure TFDMoniAdapterComponentProxy.BeforeReuse;
begin
  // nothing
end;

{ ----------------------------------------------------------------------------- }
procedure TFDMoniAdapterComponentProxy.AfterReuse;
begin
  // nothing
end;

{ ----------------------------------------------------------------------------- }
procedure TFDMoniAdapterComponentProxy.SetOwner(
  const AOwner: TObject; const ARole: TComponentName);
begin
  // nothing
end;

{ ----------------------------------------------------------------------------- }
{ TFDMoniAdapterHelper                                                          }
{ ----------------------------------------------------------------------------- }
constructor TFDMoniAdapterHelper.Create(const AAdapterObj, AParentObj: TObject);
begin
  inherited Create;
  FAdaptedObj := AAdapterObj;
  FParentObj := AParentObj;
end;

{ ----------------------------------------------------------------------------- }
destructor TFDMoniAdapterHelper.Destroy;
{$IFDEF FireDAC_MONITOR}
var
  oIntf: IFDMoniAdapter;
{$ENDIF}
begin
{$IFDEF FireDAC_MONITOR}
  if (FMoniClient <> nil) and (FHandle <> 0) then begin
    if Supports(FAdaptedObj, IFDMoniAdapter, oIntf) then
      FMoniClient.UnregisterAdapter(oIntf);
  end;
  FProxy := nil;
{$ENDIF}
  inherited Destroy;
end;

{ ----------------------------------------------------------------------------- }
procedure TFDMoniAdapterHelper.SetOwner(const AOwner: TObject;
  const ARole: TComponentName);
begin
  if AOwner is TComponent then begin
    FProxy := TFDMoniAdapterComponentProxy.Create(TComponent(AOwner));
    FParentObj := nil;
  end
  else begin
    FProxy := nil;
    FParentObj := AOwner;
  end;
  FRole := ARole;
end;

{-------------------------------------------------------------------------------}
function TFDMoniAdapterHelper.GetIsRegistered: Boolean;
begin
{$IFDEF FireDAC_MONITOR}
  Result := FHandle <> 0;
{$ELSE}
  Result := False;
{$ENDIF}
end;

{-------------------------------------------------------------------------------}
{$IFDEF FireDAC_MONITOR}
procedure TFDMoniAdapterHelper.RegisterClient(const AMoniClient: IFDMoniClient);
var
  oIntf: IFDMoniAdapter;
begin
  if not IsRegistered and (AMoniClient <> nil) and AMoniClient.Tracing then begin
    FMoniClient := AMoniClient;
    if Supports(FAdaptedObj, IFDMoniAdapter, oIntf) then begin
      FHandle := FMoniClient.RegisterAdapter(oIntf);
      FMoniClient.AdapterChanged(oIntf);
    end;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDMoniAdapterHelper.UnRegisterClient;
var
  oIntf: IFDMoniAdapter;
begin
  if IsRegistered and (FMoniClient <> nil) then
    try
      if Supports(FAdaptedObj, IFDMoniAdapter, oIntf) then
        FMoniClient.UnregisterAdapter(oIntf);
    finally
      FHandle := 0;
    end;
end;
{$ENDIF}

{-------------------------------------------------------------------------------}
function TFDMoniAdapterHelper.GetName: TComponentName;
begin
  if FProxy <> nil then
    Result := FProxy.Name
  else
    Result := FAdaptedObj.ClassName + '($' + IntToHex(Integer(FAdaptedObj), 8) + ')';
  if FRole <> '' then
    Result := FRole + ': ' + Result;
end;

{-------------------------------------------------------------------------------}
function TFDMoniAdapterHelper.GetParent: IFDStanObject;
begin
  if FProxy <> nil then
    Result := FProxy.Parent
  else
    Supports(FParentObj, IFDStanObject, Result);
end;

end.

