{*******************************************************}
{                                                       }
{               Delphi FireDAC Framework                }
{                 FireDAC Oracle driver                 }
{                                                       }
{ Copyright(c) 2004-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}
{$I FireDAC.inc}
{$IFDEF WIN32}
  {$HPPEMIT '#pragma link "FireDAC.Phys.Oracle.obj"'}
{$ELSE}
  {$HPPEMIT '#pragma link "FireDAC.Phys.Oracle.o"'}
{$ENDIF}

unit FireDAC.Phys.Oracle;

interface

uses
  System.Classes,
  FireDAC.Stan.Intf,
  FireDAC.Phys;

type
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32)]
  TFDPhysOracleDriverLink = class(TFDPhysDriverLink)
  private
    FTNSAdmin: String;
    FNLSLang: String;
    FThreaded: Boolean;
  protected
    function GetBaseDriverID: String; override;
    function IsConfigured: Boolean; override;
    procedure ApplyTo(const AParams: IFDStanDefinition); override;
  public
    constructor Create(AOwner: TComponent); override;
    class procedure GetOracleHomes(AList: TStrings);
    procedure GetTNSServices(AList: TStrings);
  published
    property NLSLang: String read FNLSLang write FNLSLang;
    property TNSAdmin: String read FTNSAdmin write FTNSAdmin;
    property Threaded: Boolean read FThreaded write FThreaded default True;
  end;

{-------------------------------------------------------------------------------}
implementation

uses
  System.Variants, System.SysUtils, Data.DB, Data.SqlTimSt,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.Stan.Option, FireDAC.Stan.Util,
    FireDAC.Stan.Consts, FireDAC.Stan.SQLTimeInt,
  FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.Phys.SQLGenerator, FireDAC.Phys.OracleCli,
    FireDAC.Phys.OracleWrapper, FireDAC.Phys.OracleMeta;

type
  TFDPhysOracleDriver = class;
  TFDPhysOracleConnection = class;
  TFDPhysOracleEventAlerter = class;
  TFDPhysOracleEventAlerter_DBMS_ALERT = class;
  TFDPhysOracleEventAlerter_DBMS_PIPE = class;
  TFDPhysOracleTransaction = class;
  TFDPhysOracleCommand = class;

  TFDPhysOracleCliHandles = array [0..8] of pOCIHandle;
  PFDPhysOracleCliHandles = ^TFDPhysOracleCliHandles;

  TFDOraCommandHasParams = set of (hpHBlobsRet, hpInput, hpOutput, hpCursors,
    hpLongData, hpManyVLobs);
  TFDOraCommandHasFields = set of (hfNChar, hfManyNChars, hfLongData, hfBlob);

  TFDPhysOracleDriver = class(TFDPhysDriver)
  private
    FLib: TOCILib;
  protected
    class function GetBaseDriverID: String; override;
    procedure InternalLoad; override;
    procedure InternalUnload; override;
    function InternalCreateConnection(AConnHost: TFDPhysConnectionHost): TFDPhysConnection; override;
    function GetCliObj: Pointer; override;
    function GetConnParamCount(AKeys: TStrings): Integer; override;
    procedure GetConnParams(AKeys: TStrings; Index: Integer;
      var AName, AType, ADefVal, ACaption: String; var ALoginIndex: Integer); override;
  public
    constructor Create(AManager: TFDPhysManager; const ADriverDef: IFDStanDefinition); override;
    destructor Destroy; override;
  end;

  TFDPhysOracleConnection = class(TFDPhysConnection)
  private
    FEnv: TOCIEnv;
    FService: TOCIService;
    FSession: TOCISession;
    FServer: TOCIServer;
    FCliHandles: TFDPhysOracleCliHandles;
    FServerVersion: TFDVersion;
    FUseDBAViews: Boolean;
    FDecimalSep: Char;
    FDBCharacterSet,
    FNDBCharacterSet: String;
    FBooleanFormat: TOCIVarDataType;
{$IFDEF FireDAC_OCI_NLSParams}
    FNLSParams: TStrings;
{$ENDIF}
{$IFDEF FireDAC_OCI_Versions}
    FVersions: TStrings;
{$ENDIF}
    FLastServerOutput: Boolean;
    FLastServerOutputSize: Integer;
    procedure ParseAliasConnStr(var AUsr, APwd, ASrv: String; var AAuthMode: ub4);
    procedure AlterSession;
    procedure UpdateServerOutput;
    procedure GetServerOutput;
    procedure GetPLSQLErrors;
    procedure UpdateCurrentSchema;
  protected
    procedure InternalConnect; override;
    procedure InternalSetMeta; override;
    procedure InternalDisconnect; override;
    function InternalCreateCommand: TFDPhysCommand; override;
    function InternalCreateTransaction: TFDPhysTransaction; override;
    function InternalCreateEvent(const AEventKind: String): TFDPhysEventAlerter; override;
    procedure InternalChangePassword(const AUserName, AOldPassword, ANewPassword: String); override;
    function InternalCreateMetadata: TObject; override;
    function InternalCreateCommandGenerator(const ACommand: IFDPhysCommand):
      TFDPhysCommandGenerator; override;
    procedure InternalExecuteDirect(const ASQL: String; ATransaction: TFDPhysTransaction); override;
    procedure InternalPing; override;
    procedure GetItem(AIndex: Integer; out AName: String;
      out AValue: Variant; out AKind: TFDMoniAdapterItemKind); override;
    function GetItemCount: Integer; override;
    function GetMessages: EFDDBEngineException; override;
    function GetCliObj: Pointer; override;
    function InternalGetCliHandle: Pointer; override;
    procedure InternalAnalyzeSession(AMessages: TStrings); override;
{$IFDEF FireDAC_MONITOR}
    procedure InternalTracingChanged; override;
{$ENDIF}
  public
    constructor Create(ADriverObj: TFDPhysDriver; AConnHost: TFDPhysConnectionHost); override;
  end;

  TFDPhysOracleTransaction = class(TFDPhysTransaction)
  private
    FTransaction: TOCITransaction;
    function GetOraConnection: TFDPhysOracleConnection;
  protected
    procedure InternalStartTransaction(ATxID: LongWord); override;
    procedure InternalCommit(ATxID: LongWord); override;
    procedure InternalRollback(ATxID: LongWord); override;
    procedure InternalAllocHandle; override;
    procedure InternalReleaseHandle; override;
    procedure InternalSelect; override;
    function GetCliObj: Pointer; override;
    procedure InternalCheckState(ACommandObj: TFDPhysCommand; ASuccess: Boolean); override;
    // other
    procedure UseHandle(AHandle: pOCIHandle);
    function GetAutoCommit: Boolean;
    property OraConnection: TFDPhysOracleConnection read GetOraConnection;
  end;

  TFDPhysOracleEventAlerter = class(TFDPhysEventAlerter)
  private
    FWaitConnection: IFDPhysConnection;
    FWaitCommand: IFDPhysCommand;
    FWaitThread: TThread;
    FSignalCommand: IFDPhysCommand;
  protected
    procedure DoFired; virtual; abstract;
    procedure InternalAllocHandle; override;
    procedure InternalReleaseHandle; override;
    procedure InternalRegister; override;
    procedure InternalUnregister; override;
  end;

  TFDPhysOracleEventAlerter_DBMS_ALERT = class(TFDPhysOracleEventAlerter)
  protected
    // TFDPhysEventAlerter
    procedure DoFired; override;
    procedure InternalRegister; override;
    procedure InternalHandle(AEventMessage: TFDPhysEventMessage); override;
    procedure InternalAbortJob; override;
    procedure InternalUnregister; override;
    procedure InternalSignal(const AEvent: String; const AArgument: Variant); override;
  end;

  TFDPhysOracleEventAlerter_DBMS_PIPE = class(TFDPhysOracleEventAlerter)
  private
    FReadCommand,
    FWriteCommand: IFDPhysCommand;
  protected
    // TFDPhysEventAlerter
    procedure DoFired; override;
    procedure InternalAllocHandle; override;
    procedure InternalRegister; override;
    procedure InternalHandle(AEventMessage: TFDPhysEventMessage); override;
    procedure InternalAbortJob; override;
    procedure InternalReleaseHandle; override;
    procedure InternalSignal(const AEvent: String; const AArgument: Variant); override;
  end;

  PFDOraVarInfoRec = ^TFDOraVarInfoRec;
  PFDOraCrsDataRec = ^TFDOraCrsDataRec;

  TFDOraVarInfoRec = record
    FName,
    FExtName:     String;
    FPos:         sb4;
    FOSrcType,
    FOOutputType: TOCIVarDataType;
    FSrcType,
    FOutputType,
    FDestType:    TFDDataType;
    FByteSize,
    FLogSize:     ub4;
    FVar:         TOCIVariable;
    // FAttrs moved to here out of odDefine case due to C++Builder 5 incompatibility
    FAttrs:       TFDDataAttributes;
    case FVarType: TOCIVarType of
    odDefine: (
      FPrec:     sb4;
      FScale:    sb4;
      FADScale:  Integer;
      FCrsInfo:  PFDOraCrsDataRec;
    );
    odIn, odOut, odInOut, odRet: (
      FArrayLen:        ub4;
      FIsCaseSensitive: Boolean;
      FIsPLSQLTable:    Boolean;
      FParamType:       TParamType;
      FDataType:        TFieldType;
    )
  end;

  TFDOraCrsDataRec = record
    FParent: PFDOraCrsDataRec;
    FStmt: TOCIStatement;
    FColInfos: array of TFDOraVarInfoRec;
    FColIndex: Integer;
    FExecuted: Boolean;
    FHasFields: TFDOraCommandHasFields;
  end;

  TFDPhysOracleCommand = class(TFDPhysCommand)
  private
    FBase: TFDOraCrsDataRec;
    FCurrentCrsInfo: PFDOraCrsDataRec;
    FParInfos: array of TFDOraVarInfoRec;
    FCrsInfos: TFDPtrList;
    FActiveCrs: Integer;
    FInfoStack: TFDPtrList;
    FCursorCanceled: Boolean;
    FBindVarsDirty: Boolean;
    FHasParams: TFDOraCommandHasParams;
    procedure SQL2ADColInfo(AOraDataType: TOCIVarDataType; AOraSize, AOraPrec,
      AOraScale: sb4; var AType: TFDDataType; var AAttrs: TFDDataAttributes;
      var ALen: LongWord; var APrec, AScale: Integer);
    function FDType2OCIType(AADType: TFDDataType; AFixedLen: Boolean): TOCIVarDataType;
    function GenerateStoredProcCallUsingOCI(const AName: TFDPhysParsedName): String;
    procedure DefParInfos(const ANm: String; AVt: TOCIVarType;
      FDt: TOCIVarDataType; ASz, APrec, ASCale: sb4; AIsTable, AIsResult: Boolean);
    procedure SetParamValues(ATimes, AOffset: LongInt; AFromParIndex: Integer;
      AIntoReturning: Boolean);
    procedure GetParamValues(ATimes, AOffset: LongInt);
    procedure CreateDefineInfo(ACrsInfo: PFDOraCrsDataRec);
    procedure CreateBindInfo;
    procedure DestroyDefineInfo(ACrsInfo: PFDOraCrsDataRec);
    procedure DestroyBindInfo;
    procedure ResetDefVars(ACrsInfo: PFDOraCrsDataRec; ARowsetSize: LongWord);
    function ProcessRowSet(ACrsInfo: PFDOraCrsDataRec; ATable: TFDDatSTable;
      AParentRow: TFDDatSRow; ARowsetSize: LongWord): LongWord;
    procedure SetupStatement(AStmt: TOCIStatement);
    procedure DestroyStmt(ACrsInfo: PFDOraCrsDataRec);
    function GetActiveCursor: PFDOraCrsDataRec;
    procedure RebindCursorParams;
    procedure GetServerFeedback;
    procedure DestroyCrsInfo;
    function IsActiveCursorValid: Boolean;
    function GetRowsetSize(ACrsInfo: PFDOraCrsDataRec; ARowsetSize: LongWord): LongWord;
  protected
    procedure InternalAbort; override;
    procedure InternalClose; override;
    procedure InternalExecute(ATimes, AOffset: LongInt; var ACount: TFDCounter); override;
    function InternalFetchRowSet(ATable: TFDDatSTable; AParentRow: TFDDatSRow;
      ARowsetSize: LongWord): LongWord; override;
    function InternalOpen: Boolean; override;
    function InternalNextRecordSet: Boolean; override;
    procedure InternalPrepare; override;
    function InternalColInfoStart(var ATabInfo: TFDPhysDataTableInfo): Boolean; override;
    function InternalColInfoGet(var AColInfo: TFDPhysDataColumnInfo): Boolean; override;
    procedure InternalUnprepare; override;
    function GetCliObj: Pointer; override;
  public
    constructor Create(AConnectionObj: TFDPhysConnection);
    destructor Destroy; override;
  end;

const
  S_FD_Normal = 'Normal';
  S_FD_SysDBA = 'SysDBA';
  S_FD_SysOper = 'SysOper';

  S_FD_Service = 'Service';
  S_FD_Mode = 'Mode';

  S_FD_String = 'String';
  S_FD_Integer = 'Integer';

  ovdt2addt: array [TOCIVarDataType] of TFDDataType = (dtUnknown, dtInt32,
    dtSingle, dtDouble, dtFmtBCD, dtAnsiString, dtAnsiString, dtWideString,
    dtWideString, dtByteString, dtDateTime, dtMemo, dtWideMemo, dtBlob,
    dtAnsiString, dtCursorRef, dtRowSetRef, dtHMemo, dtWideHMemo, dtHBlob,
    dtHBFile, dtHBFile, dtDateTimeStamp, dtTimeIntervalYM, dtTimeIntervalDS,
    dtBoolean);

  addt2ovdt: array [TFDDataType] of TOCIVarDataType = (
    otUnknown,
    otUnknown,
    otInteger, otInteger, otInteger, otNumber,
    otInteger, otInteger, otInteger, otNumber,
    otSingle, otDouble, otDouble,
    otNumber, otNumber, otNumber,
    otDateTime, otDateTime, otDateTime, otTimeStamp,
    otUnknown, otIntervalYM, otIntervalDS,
    otString, otNString, otRaw,
                                      
    otLongRaw, otLong, otNLong, otNLong {XML},
    otBLOB, otCLOB, otNCLOB,
    otBFile,
    otNestedDataSet, otCursor, otUnknown,
      otUnknown, otUnknown,
    otUnknown, otUnknown);

  pt2vt: array [TParamType] of TOCIVarType = (
    odUnknown, odIn, odOut, odInOut, odOut);
  vt2pt: array [TOCIVarType] of TParamType = (
    ptUnknown, ptInput, ptOutput, ptInputOutput, ptOutput, ptUnknown);

{-------------------------------------------------------------------------------}
{ TFDPhysOracleDriverLink                                                       }
{-------------------------------------------------------------------------------}
constructor TFDPhysOracleDriverLink.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FThreaded := True;
end;

{-------------------------------------------------------------------------------}
function TFDPhysOracleDriverLink.GetBaseDriverID: String;
begin
  Result := S_FD_OraId;
end;

{-------------------------------------------------------------------------------}
class procedure TFDPhysOracleDriverLink.GetOracleHomes(AList: TStrings);
begin
  TOCILib.GetOracleHomeList(AList);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleDriverLink.GetTNSServices(AList: TStrings);
begin
  if FDPhysManager.State = dmsInactive then
    FDPhysManager.Open;
  DriverIntf.Employ;
  try
    TOCILib(DriverIntf.CliObj).GetTNSServicesList(AList);
  finally
    DriverIntf.Vacate;
  end;
end;

{-------------------------------------------------------------------------------}
function TFDPhysOracleDriverLink.IsConfigured: Boolean;
begin
  Result := inherited IsConfigured or (NLSLang <> '') or (TNSAdmin <> '') or
    not Threaded;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleDriverLink.ApplyTo(const AParams: IFDStanDefinition);
begin
  inherited ApplyTo(AParams);
  if NLSLang <> '' then
    AParams.AsString[S_FD_ConnParam_Oracle_NLS_LANG] := NLSLang;
  if TNSAdmin <> '' then
    AParams.AsString[S_FD_ConnParam_Oracle_TNS_ADMIN] := TNSAdmin;
  if not Threaded then
    AParams.AsBoolean[S_FD_ConnParam_Oracle_Threaded] := Threaded;
end;

{-------------------------------------------------------------------------------}
{ TFDPhysOracleDriver                                                           }
{-------------------------------------------------------------------------------}
constructor TFDPhysOracleDriver.Create(AManager: TFDPhysManager;
  const ADriverDef: IFDStanDefinition);
begin
  inherited Create(AManager, ADriverDef);
  FLib := TOCILib.Create(Messages, FDPhysManagerObj);
end;

{-------------------------------------------------------------------------------}
destructor TFDPhysOracleDriver.Destroy;
begin
  inherited Destroy;
  FDFreeAndNil(FLib);
end;

{-------------------------------------------------------------------------------}
class function TFDPhysOracleDriver.GetBaseDriverID: String;
begin
  Result := S_FD_OraId;
end;

{ ----------------------------------------------------------------------------- }
procedure TFDPhysOracleDriver.InternalLoad;
var
  sHome, sLib, sNLSLang, sTNSAdmin: String;
  lThreaded: Boolean;
begin
  sHome := '';
  sLib := '';
  sNLSLang := '';
  sTNSAdmin := '';
  lThreaded := True;
  if Params <> nil then begin
    GetVendorParams(sHome, sLib);
    sNLSLang := Params.AsString[S_FD_ConnParam_Oracle_NLS_LANG];
    sTNSAdmin := Params.AsString[S_FD_ConnParam_Oracle_TNS_ADMIN];
    if Params.IsSpecified(S_FD_ConnParam_Oracle_Threaded) then
      lThreaded := Params.AsBoolean[S_FD_ConnParam_Oracle_Threaded];
  end;
  FLib.Load(sHome, sLib, sNLSLang, sTNSAdmin, lThreaded);
end;

{ ----------------------------------------------------------------------------- }
procedure TFDPhysOracleDriver.InternalUnload;
begin
  FLib.Unload;
end;

{ ----------------------------------------------------------------------------- }
function TFDPhysOracleDriver.InternalCreateConnection(
  AConnHost: TFDPhysConnectionHost): TFDPhysConnection;
begin
  Result := TFDPhysOracleConnection.Create(Self, AConnHost);
end;

{ ----------------------------------------------------------------------------- }
function TFDPhysOracleDriver.GetCliObj: Pointer;
begin
  Result := FLib;
end;

{-------------------------------------------------------------------------------}
function TFDPhysOracleDriver.GetConnParamCount(AKeys: TStrings): Integer;
begin
  Result := inherited GetConnParamCount(AKeys) + 8;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleDriver.GetConnParams(AKeys: TStrings; Index: Integer;
  var AName, AType, ADefVal, ACaption: String; var ALoginIndex: Integer);
var
  oList: TFDStringList;
  i: Integer;
begin
  ALoginIndex := -1;
  if Index < inherited GetConnParamCount(AKeys) then begin
    inherited GetConnParams(AKeys, Index, AName, AType, ADefVal, ACaption, ALoginIndex);
    if AName = S_FD_ConnParam_Common_Database then begin
      Employ;
      try
        oList := TFDStringList.Create;
        try
          FLib.GetTNSServicesList(oList);
          AType := '';
          for i := 0 to oList.Count - 1 do begin
            if AType <> '' then
              AType := AType + ';';
            AType := AType + oList[i];
          end;
        finally
          FDFree(oList);
        end;
        ACaption := S_FD_Service;
        ALoginIndex := 2;
      finally
        Vacate;
      end;
    end;
  end
  else begin
    ADefVal := '';
    ACaption := '';
    case Index - inherited GetConnParamCount(AKeys) of
    0:
      begin
        AName := S_FD_ConnParam_Common_OSAuthent;
        AType := '@Y';
      end;
    1:
      begin
        AName := S_FD_ConnParam_Oracle_AuthMode;
        AType := S_FD_Normal + ';' + S_FD_SysDBA + ';' + S_FD_SysOper;
        ADefVal := S_FD_Normal;
        ACaption := S_FD_Mode;
        ALoginIndex := 3;
      end;
    2:
      begin
        AName := S_FD_ConnParam_Common_CharacterSet;
        AType := SNLSDefault + ';' + SUTF8;
        ADefVal := SNLSDefault;
      end;
    3:
      begin
        AName := S_FD_ConnParam_Oracle_BooleanFormat;
        AType := S_FD_Integer + ';' + S_FD_String;
        ADefVal := S_FD_Integer;
      end;
    4:
      begin
        AName := S_FD_ConnParam_Common_ApplicationName;
        AType := '@S';
      end;
    5:
      begin
        AName := S_FD_ConnParam_Oracle_Advanced;
        AType := '@S';
      end;
    6:
      begin
        AName := S_FD_ConnParam_Common_MetaDefSchema;
        AType := '@S';
      end;
    7:
      begin
        AName := S_FD_ConnParam_Common_MetaCurSchema;
        AType := '@S';
      end;
{    8:
      begin
        AName := S_FD_ConnParam_Oracle_Distributed;
        AType := '@L';
        ADefVal := S_FD_False;
      end;
    9:
      begin
        AName := S_FD_ConnParam_Oracle_SrvIntName;
        AType := '@S';
      end;
}
    end;
    if ACaption = '' then
      ACaption := AName;
  end;
end;

{-------------------------------------------------------------------------------}
{ TFDPhysOracleConnection                                                       }
{-------------------------------------------------------------------------------}
constructor TFDPhysOracleConnection.Create(ADriverObj: TFDPhysDriver;
  AConnHost: TFDPhysConnectionHost);
begin
  inherited Create(ADriverObj, AConnHost);
end;

{-------------------------------------------------------------------------------}
function TFDPhysOracleConnection.InternalCreateCommand: TFDPhysCommand;
begin
  Result := TFDPhysOracleCommand.Create(Self);
end;

{-------------------------------------------------------------------------------}
function TFDPhysOracleConnection.InternalCreateTransaction: TFDPhysTransaction;
begin
  Result := TFDPhysOracleTransaction.Create(Self);
end;

{-------------------------------------------------------------------------------}
function TFDPhysOracleConnection.InternalCreateEvent(const AEventKind: String): TFDPhysEventAlerter;
begin
  if CompareText(AEventKind, S_FD_EventKind_Oracle_DBMS_ALERT) = 0 then
    Result := TFDPhysOracleEventAlerter_DBMS_ALERT.Create(Self, AEventKind)
  else if CompareText(AEventKind, S_FD_EventKind_Oracle_DBMS_PIPE) = 0 then
    Result := TFDPhysOracleEventAlerter_DBMS_PIPE.Create(Self, AEventKind)
  else
    Result := nil;
end;

{-------------------------------------------------------------------------------}
function TFDPhysOracleConnection.InternalCreateCommandGenerator(
  const ACommand: IFDPhysCommand): TFDPhysCommandGenerator;
begin
  if ACommand <> nil then
    Result := TFDPhysOraCommandGenerator.Create(ACommand, FUseDBAViews)
  else
    Result := TFDPhysOraCommandGenerator.Create(Self, FUseDBAViews);
end;

{-------------------------------------------------------------------------------}
function TFDPhysOracleConnection.InternalCreateMetadata: TObject;
begin
  Result := TFDPhysOraMetadata.Create(Self, FServerVersion,
    TFDPhysOracleDriver(DriverObj).FLib.Version,
    (FEnv <> nil) and (FEnv.DataEncoder.Encoding in [ecUTF8, ecUTF16]));
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleConnection.ParseAliasConnStr(var AUsr, APwd, ASrv: String;
  var AAuthMode: ub4);
var
  s, sRest, sAuth: String;
  i1, i2, i3, i4: Integer;
begin
  AUsr := '';
  APwd := '';
  ASrv := '';
  sAuth := '';
  sRest := GetConnectionDef.ExpandedDatabase;
  i1 := Pos('/', sRest);
  i2 := Pos('@', sRest);
  i3 := Pos(' AS ', UpperCase(sRest));
  i4 := Pos(':', sRest);
  if (i2 = 0) and (i3 = 0) or (i4 > 0) then
    ASrv := sRest
  else begin
    if i3 > 0 then
      Inc(i3);
    if (i3 <> 0) and
       ((i3 = 1) or (sRest[i3 - 1] = ' ')) and
       ((i3 + 2 > Length(sRest)) or (sRest[i3 + 2] = ' ')) then begin
      sAuth := Trim(Copy(sRest, i3 + 2, Length(sRest)));
      sRest := Copy(sRest, 1, i3 - 1);
    end;
    if i2 <> 0 then begin
      ASrv := Trim(Copy(sRest, i2 + 1, Length(sRest)));
      sRest := Copy(sRest, 1, i2 - 1);
    end;
    if i1 <> 0 then begin
      APwd := Trim(Copy(sRest, i1 + 1, Length(sRest)));
      sRest := Copy(sRest, 1, i1 - 1);
    end;
    AUsr := Trim(Copy(sRest, 1, Length(sRest)));
  end;
  s := GetConnectionDef.UserName;
  if s <> '' then
    AUsr := s;
  s := GetConnectionDef.Password;
  if s <> '' then
    APwd := s;
  if CompareText(ASrv, S_FD_Local) = 0 then
    ASrv := '';
  s := GetConnectionDef.AsString[S_FD_ConnParam_Oracle_AuthMode];
  if s <> '' then
    sAuth := s;
  if CompareText(sAuth, S_FD_SysDBA) = 0 then
    AAuthMode := OCI_SYSDBA
  else if CompareText(sAuth, S_FD_SysOper) = 0 then
    AAuthMode := OCI_SYSOPER
  else
    AAuthMode := OCI_DEFAULT;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleConnection.AlterSession;
var
  sAdv, sCmd: String;
  i: Integer;
begin
  if GetConnectionDef.HasValue(S_FD_ConnParam_Oracle_Advanced) then begin
    sAdv := GetConnectionDef.AsString[S_FD_ConnParam_Oracle_Advanced];
    sCmd := '';
    i := 1;
    while i <= Length(sAdv) do
      sCmd := sCmd + ' ' + FDExtractFieldName(sAdv, i);
    if sCmd <> '' then
      InternalExecuteDirect('ALTER SESSION SET' + sCmd, nil);
  end;

  if GetConnectionDef.HasValue(S_FD_ConnParam_Common_ApplicationName) then begin
    sCmd := GetConnectionDef.AsString[S_FD_ConnParam_Common_ApplicationName];
    InternalExecuteDirect('BEGIN DBMS_APPLICATION_INFO.SET_MODULE(' + QuotedStr(sCmd) + ', NULL); END;', nil);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleConnection.InternalConnect;
var
  pCliHandles: PFDPhysOracleCliHandles;
  iAuthMode: ub4;
  sCharsetName, sUsr, sPwd, sSrv: String;
{$IFDEF FireDAC_MONITOR}
  oMon: IFDMoniClient;
{$ENDIF}
begin
{$IFDEF FireDAC_MONITOR}
  if GetTracing then
    oMon := GetMonitor
  else
    oMon := nil;
{$ENDIF}

  FBooleanFormat := otInteger;
  if CompareText(GetConnectionDef.AsString[S_FD_ConnParam_Oracle_BooleanFormat], S_FD_String) = 0 then
    FBooleanFormat := otString;

  if InternalGetSharedCliHandle() <> nil then begin
    pCliHandles := PFDPhysOracleCliHandles(InternalGetSharedCliHandle());
    FEnv := TOCIEnv.CreateUsingHandle(TFDPhysOracleDriver(DriverObj).FLib,
      Self, pCliHandles^[0], pCliHandles^[5], String(PChar(pCliHandles^[6])),
      ub2(pCliHandles^[7]), Boolean(pCliHandles^[8]) {$IFDEF FireDAC_MONITOR}, oMon {$ENDIF});
    FService := TOCIService.CreateUsingHandle(FEnv, pCliHandles^[1], Self);
    FServer := TOCIServer.CreateUsingHandle(FService, pCliHandles^[2]);
    FSession := TOCISession.CreateUsingHandle(FService, pCliHandles^[3]);
    TFDPhysOracleTransaction(TransactionObj).UseHandle(pCliHandles^[4]);
  end

  else begin
    sCharsetName := GetConnectionDef.AsString[S_FD_ConnParam_Common_CharacterSet];
    if CompareText(sCharsetName, SNLSDefault) = 0 then
      sCharsetName := '';
    FEnv := TOCIEnv.Create(TFDPhysOracleDriver(DriverObj).FLib, Self
      {$IFDEF FireDAC_MONITOR}, oMon {$ENDIF});
{$IFDEF FireDAC_MONITOR}
    InternalTracingChanged;
{$ENDIF}
    FEnv.InitEnv(sCharsetName);
    FService := TOCIService.Create(FEnv, Self);
    FService.NONBLOCKING_MODE := False;
    FServer := TOCIServer.Create(FService);
    FSession := TOCISession.Create(FService);
    sUsr := '';
    sPwd := '';
    sSrv := '';
    iAuthMode := 0;
    ParseAliasConnStr(sUsr, sPwd, sSrv, iAuthMode);
    if GetConnectionDef.OSAuthent then begin
      sUsr := '';
      sPwd := '';
    end;
    FServer.Attach(sSrv);
//    if GetConnectionDef.AsBoolean[S_FD_ConnParam_Oracle_Distributed] and
//       GetConnectionDef.HasValue(S_FD_ConnParam_Oracle_SrvIntName) then
//      FServer.INTERNAL_NAME := GetConnectionDef.AsString[S_FD_ConnParam_Oracle_SrvIntName];
    if GetConnectionDef.NewPassword <> '' then begin
      FSession.Select;
      FSession.ChangePassword(sUsr, sPwd, GetConnectionDef.NewPassword);
      sPwd := GetConnectionDef.NewPassword;
    end;
    FSession.Start(sUsr, sPwd, iAuthMode);
    AlterSession;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleConnection.InternalSetMeta;
var
  oStmt: TOCIStatement;
  oVar1, oVar2, oVar3, oVar4, oVar5, oVar6, oVar7: TOCIVariable;
  i, iFrom: Integer;
  s: String;
  oFtch: TFDFetchOptions;
begin
  inherited InternalSetMeta;
  FUseDBAViews := False;
  FServerVersion := cvOracle90000;
  FDecimalSep := FormatSettings.DecimalSeparator;
  FDBCharacterSet := '';
{$IFDEF FireDAC_OCI_NLSParams}
  FNLSParams := nil;
{$ENDIF}
{$IFDEF FireDAC_OCI_Versions}
  FVersions := nil;
{$ENDIF}
  try
    oStmt := TOCIStatement.Create(FEnv, FService, nil, Self);
    oVar1 := nil;
    oVar2 := nil;
    oVar3 := nil;
    oVar4 := nil;
    oVar5 := nil;
    oVar6 := nil;
    oVar7 := nil;
    try
      oStmt.Prepare(
        'BEGIN'#10 +
          'SELECT COUNT(*) INTO :PRV FROM SESSION_PRIVS WHERE PRIVILEGE = ''SELECT ANY TABLE'' OR PRIVILEGE = ''SELECT ANY DICTIONARY'';'#10 +
          'SELECT BANNER INTO :VER FROM V$VERSION WHERE ROWNUM = 1;'#10 +
          'SELECT VALUE INTO :NUM FROM V$NLS_PARAMETERS WHERE PARAMETER = ''NLS_NUMERIC_CHARACTERS'';'#10 +
          'SELECT GREATEST(LENGTH(CHR(2000000000)), LENGTH(CHR(2000000)), LENGTH(CHR(20000))) INTO :BTS FROM SYS.DUAL;'#10 +
          'SELECT USER INTO :USR FROM SYS.DUAL;'#10 +
          'SELECT VALUE INTO :CHS FROM V$NLS_PARAMETERS WHERE PARAMETER = ''NLS_CHARACTERSET'';'#10 +
          'SELECT VALUE INTO :NCS FROM V$NLS_PARAMETERS WHERE PARAMETER = ''NLS_NCHAR_CHARACTERSET'';'#10 +
        'END;'
      );
      oVar1 := oStmt.AddVar(':PRV', odOut, otInteger);
      oVar2 := oStmt.AddVar(':VER', odOut, otString, 128);
      oVar3 := oStmt.AddVar(':NUM', odOut, otString, 64);
      oVar4 := oStmt.AddVar(':BTS', odOut, otInteger);
      oVar5 := oStmt.AddVar(':USR', odOut, otString, 64);
      oVar6 := oStmt.AddVar(':CHS', odOut, otString, 64);
      oVar7 := oStmt.AddVar(':NCS', odOut, otString, 64);
      oStmt.Execute(1, 0, False, True);
      FUseDBAViews := (oVar1.AsInteger = 2);
      s := oVar2.AsString;
      i := Pos('RELEASE', AnsiUpperCase(s));
      if i <> 0 then begin
        Inc(i, 7);
        while (s[i] = ' ') do
          Inc(i);
        iFrom := i;
        while FDInSet(s[i], ['0'..'9', '.']) do
          Inc(i);
        FServerVersion := FDVerStr2Int(Copy(s, iFrom, i - iFrom));
      end;
      s := oVar3.AsString;
      if Length(s) > 0 then
        FDecimalSep := s[1];
      FService.BytesPerChar := ub1(oVar4.AsInteger);
      if FCurrentSchema = '' then
        FCurrentSchema := oVar5.AsString;
      FDBCharacterSet := oVar6.AsString;
      FNDBCharacterSet := oVar7.AsString;
    finally
      FDFree(oVar1);
      FDFree(oVar2);
      FDFree(oVar3);
      FDFree(oVar4);
      FDFree(oVar5);
      FDFree(oVar6);
      FDFree(oVar7);
      FDFree(oStmt);
    end;
{$IFDEF FireDAC_OCI_NLSParams}
    oStmt := TOCIStatement.Create(FEnv, FService, nil, Self);
    oVar1 := nil;
    oVar2 := nil;
    FNLSParams := TFDStringList.Create;
    try
      try
        oStmt.Prepare('SELECT * FROM V$NLS_PARAMETERS');
        oVar1 := oStmt.AddVar(1, odDefine, otString, 100);
        oVar2 := oStmt.AddVar(2, odDefine, otString, 100);
        oStmt.Execute(1, 0, False, True);
        while not oStmt.Eof do begin
          FNLSParams.Add(oVar1.AsString + '=' + oVar2.AsString);
          oStmt.Fetch(1);
        end;
      except
        // no exceptions visible
      end;
    finally
      FDFree(oVar1);
      FDFree(oVar2);
      FDFree(oStmt);
    end;
{$ENDIF}
{$IFDEF FireDAC_OCI_Versions}
    oStmt := TOCIStatement.Create(FEnv, FService, nil, Self);
    oVar1 := nil;
    FVersions := TFDStringList.Create;
    try
      try
        oStmt.Prepare('SELECT * FROM V$VERSION');
        oVar1 := oStmt.AddVar(1, odDefine, otString, 100);
        oStmt.Execute(1, 0, False, True);
        while not oStmt.Eof do begin
          FVersions.Add(oVar1.AsString);
          oStmt.Fetch(1);
        end;
      except
        // no exceptions visible
      end;
    finally
      FDFree(oVar1);
      FDFree(oStmt);
    end;
{$ENDIF}
  except
    on E: EOCINativeException do
      // The dictionary will be not available, when connecting to the closed database
      if Pos('ORA-01219', E.Message) > 0 then begin
        oFtch := GetOptions.FetchOptions;
        oFtch.Items := oFtch.Items - [fiMeta];
      end
      else
        raise;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleConnection.InternalDisconnect;
begin
  if (FEnv <> nil) and (FEnv.Error <> nil) then
    FEnv.Error.IgnoreErrors := (FState = csRecovering);
{$IFDEF FireDAC_OCI_NLSParams}
  FDFreeAndNil(FNLSParams);
{$ENDIF}
{$IFDEF FireDAC_OCI_Versions}
  FDFreeAndNil(FVersions);
{$ENDIF}
  FDFreeAndNil(FSession);
  FDFreeAndNil(FServer);
  FDFreeAndNil(FService);
  FDFreeAndNil(FEnv);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleConnection.InternalExecuteDirect(const ASQL: String;
  ATransaction: TFDPhysTransaction);
var
  oStmt: TOCIStatement;
begin
  oStmt := TOCIStatement.Create(FEnv, FService, nil, Self);
  try
    oStmt.Prepare(ASQL);
    oStmt.Execute(1, 0, False, False);
  finally
    FDFree(oStmt);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleConnection.InternalChangePassword(const AUserName, AOldPassword,
  ANewPassword: String);
begin
  FSession.ChangePassword(FSession.USERNAME, FSession.PASSWORD, ANewPassword);
end;

{-------------------------------------------------------------------------------}
function TFDPhysOracleConnection.GetItemCount: Integer;
begin
  Result := inherited GetItemCount;
  if DriverObj.State in [drsLoaded, drsActive] then begin
    Inc(Result, 5);
    if FSession <> nil then begin
      Inc(Result, 7);
{$IFDEF FireDAC_OCI_NLSParams}
      if FNLSParams <> nil then
        Inc(Result, FNLSParams.Count);
{$ENDIF}
    end;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleConnection.GetItem(AIndex: Integer; out AName: String;
  out AValue: Variant; out AKind: TFDMoniAdapterItemKind);
{$IFDEF FireDAC_OCI_NLSParams}
var
  i: Integer;
{$ENDIF}
begin
  if AIndex < inherited GetItemCount then
    inherited GetItem(AIndex, AName, AValue, AKind)
  else
    case AIndex - inherited GetItemCount of
    0:
      begin
        AName := 'Home';
        if TFDPhysOracleDriver(DriverObj).FLib.FInstantClient then
          AValue := '<instant client at> ' + TFDPhysOracleDriver(DriverObj).FLib.FOracleHome
        else
          AValue := TFDPhysOracleDriver(DriverObj).FLib.FOracleHome;
        AKind := ikClientInfo;
      end;
    1:
      begin
        AName := 'Version';
        AValue := Integer(TFDPhysOracleDriver(DriverObj).FLib.Version);
        AKind := ikClientInfo;
      end;
    2:
      begin
        AName := 'OCI DLL name';
        AValue := TFDPhysOracleDriver(DriverObj).FLib.DLLName;
        AKind := ikClientInfo;
      end;
    3:
      begin
        AName := 'TNSNAMES dir';
        AValue := TFDPhysOracleDriver(DriverObj).FLib.FTNSNames;
        AKind := ikClientInfo;
      end;
    4:
      begin
        AName := 'NLS_LANG';
        AValue := TFDPhysOracleDriver(DriverObj).FLib.FNLSLang;
        AKind := ikClientInfo;
      end;
    5:
      begin
        AName := 'Server ver';
{$IFDEF FireDAC_OCI_Versions}
        AValue := Trim(FVersions.Text);
{$ELSE}
        AValue := FDFixCRLF(FServer.ServerVersion);
{$ENDIF}
        AKind := ikSessionInfo;
      end;
    6:
      begin
        AName := 'Use DBA views';
        AValue := FUseDBAViews;
        AKind := ikSessionInfo;
      end;
    7:
      begin
        AName := 'Decimal sep';
        AValue := FDecimalSep;
        AKind := ikSessionInfo;
      end;
    8:
      begin
        AName := 'Client character set';
        AValue := FEnv.CharsetName;
        AKind := ikSessionInfo;
      end;
    9:
      begin
        AName := 'DB character set';
        AValue := FDBCharacterSet;
        AKind := ikSessionInfo;
      end;
    10:
      begin
        AName := 'NDB character set';
        AValue := FNDBCharacterSet;
        AKind := ikSessionInfo;
      end;
    11:
      begin
        AName := 'Database bytes/char';
        AValue := FService.BytesPerChar;
        AKind := ikSessionInfo;
      end;
    else
{$IFDEF FireDAC_OCI_NLSParams}
      if FNLSParams <> nil then begin
        i := AIndex - inherited GetItemCount - 11;
        AName := FDNameFromIndex(FNLSParams, i);
        AValue := FDValueFromIndex(FNLSParams, i);
        AKind := ikSessionInfo;
      end;
{$ENDIF}
    end;
end;

{ ----------------------------------------------------------------------------- }
function TFDPhysOracleConnection.GetMessages: EFDDBEngineException;
begin
  if FEnv <> nil then
    Result := FEnv.Error.Info
  else
    Result := nil;
end;

{-------------------------------------------------------------------------------}
function TFDPhysOracleConnection.GetCliObj: Pointer;
begin
  Result := FService;
end;

{-------------------------------------------------------------------------------}
function TFDPhysOracleConnection.InternalGetCliHandle: Pointer;
begin
  if FEnv <> nil then begin
    FCliHandles[0] := FEnv.Handle;
    FCliHandles[1] := FService.Handle;
    FCliHandles[2] := FServer.Handle;
    FCliHandles[3] := FSession.Handle;
    FCliHandles[4] := TFDPhysOracleTransaction(TransactionObj).FTransaction.Handle;
    FCliHandles[5] := FEnv.Error.Handle;
    FCliHandles[6] := pOCIHandle(PChar(FEnv.CharsetName));
    FCliHandles[7] := pOCIHandle(FEnv.ExplicitCharsetID);
    FCliHandles[8] := pOCIHandle(FEnv.ByteSemantic);
    Result := @FCliHandles;
  end
  else
    Result := nil;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleConnection.UpdateServerOutput;
var
  oStmt: TOCIStatement;
  oVar: TOCIVariable;
  iSize: Integer;
  oRes: TFDTopResourceOptions;
begin
  oRes := FOptions.ResourceOptions as TFDTopResourceOptions;
  if not ((FLastServerOutput <> oRes.ServerOutput) or
          oRes.ServerOutput and (FLastServerOutputSize <> oRes.ServerOutputSize)) then
    Exit;
  oVar := nil;
  oStmt := TOCIStatement.Create(FEnv, FService, nil, Self);
  try
    if oRes.ServerOutput then begin
      oStmt.Prepare('BEGIN DBMS_OUTPUT.ENABLE(:SIZE); END;');
      oVar := oStmt.AddVar(':SIZE', odIn, otInteger);
      iSize := oRes.ServerOutputSize;
      oVar.SetData(0, @iSize, 0);
    end
    else
      oStmt.Prepare('BEGIN DBMS_OUTPUT.DISABLE; END;');
    oStmt.Execute(1, 0, False, False);
    FLastServerOutput := oRes.ServerOutput;
    FLastServerOutputSize := oRes.ServerOutputSize;
  finally
    FDFree(oVar);
    FDFree(oStmt);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleConnection.GetServerOutput;
var
  oStmt: TOCIStatement;
  oVar1: TOCIVariable;
  oVar2: TOCIVariable;
  iStatus, i: Integer;
  oList: TFDStringList;
begin
  if not (FOptions.ResourceOptions as TFDTopResourceOptions).ServerOutput then
    Exit;
  oVar1 := nil;
  oVar2 := nil;
  oStmt := TOCIStatement.Create(FEnv, FService, nil, Self);
  oList := TFDStringList.Create;
  try
    oStmt.Prepare('BEGIN DBMS_OUTPUT.GET_LINE(:LINE, :STATUS); END;');
    oVar1 := oStmt.AddVar(':LINE', odOut, otString, 256);
    oVar2 := oStmt.AddVar(':STATUS', odOut, otInteger);
    repeat
      oStmt.Execute(1, 0, False, False);
      iStatus := oVar2.AsInteger;
      if iStatus = 0 then
        oList.Add(oVar1.AsString);
    until iStatus = 1;
    if oList.Count > 0 then begin
      FEnv.Error.CheckAllocateInfo;
      for i := 0 to oList.Count - 1 do
        FEnv.Error.Info.AppendError(0, 0, oList[i], '', ekServerOutput, -1, -1);
    end;
  finally
    FDFree(oList);
    FDFree(oVar1);
    FDFree(oVar2);
    FDFree(oStmt);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleConnection.GetPLSQLErrors;
begin
  if not (FOptions.ResourceOptions as TFDTopResourceOptions).ServerOutput or
     // ORA-24344 Success with compilation error
     (FEnv.Error.Info = nil) or (FEnv.Error.Info[0].ErrorCode <> 24344) then
    Exit;
                                                                                     
                                                                               
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleConnection.InternalPing;
begin
  FService.Ping;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleConnection.UpdateCurrentSchema;
var
  oStmt: TOCIStatement;
  oVar1: TOCIVariable;
  sUser: String;
begin
  oStmt := TOCIStatement.Create(FEnv, FService, nil, Self);
  oVar1 := nil;
  try
    if FServerVersion < cvOracle81500 then
      sUser := 'USER'
    else
      sUser := 'SYS_CONTEXT(''USERENV'', ''CURRENT_SCHEMA'')';
    oStmt.Prepare('SELECT ' + sUser + ' FROM SYS.DUAL');
    oVar1 := oStmt.AddVar(1, odDefine, otString, 64);
    oStmt.Execute(1, 0, False, True);
    FCurrentSchema := oVar1.AsString;
  finally
    FDFree(oVar1);
    FDFree(oStmt);
  end;
end;

{$IFDEF FireDAC_MONITOR}
{-------------------------------------------------------------------------------}
procedure TFDPhysOracleConnection.InternalTracingChanged;
begin
  if FEnv <> nil then begin
    FEnv.Monitor := FMonitor;
    FEnv.Tracing := FTracing;
  end;
end;
{$ENDIF}

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleConnection.InternalAnalyzeSession(AMessages: TStrings);
{$IFNDEF NEXTGEN}
var
  saul, sa: AnsiString;
  sb: TFDByteString;
{$ENDIF}
begin
  inherited InternalAnalyzeSession(AMessages);

  // 2. The low value of minor version number
  if FEnv.Lib.Version mod 10000 <= 200 then
    AMessages.Add(Format('Warning: The client version [%s] with low minor version may be unstable.',
      [FEnv.Lib.VersionStr]));

  // 3. < 9.2 client with Unicode Delphi
  if FEnv.Lib.Version < cvOracle92000 then
    AMessages.Add(Format('Warning: The client version [%s] does not support Unicode in full.',
      [FEnv.Lib.VersionStr]));

  // 4. 10.0 / 10.1 client with > 10.2 servers
  if (FServerVersion >= cvOracle102000) and
     (FEnv.Lib.Version < cvOracle102000) and (FEnv.Lib.Version >= cvOracle100000) then
    AMessages.Add(Format('Warning: The client [%s] and server [%s] versions may lead to various issues.',
      [FEnv.Lib.VersionStr,  FDVerInt2Str(FServerVersion)]));

{$IFNDEF NEXTGEN}
  // 5. US7ASCII leads to conversion losts for Western Europa languages, eg for a"
  SetLength(saul, 2);
  saul[1] := #195;
  saul[2] := #164;
  sa := AnsiString(Utf8ToAnsi(saul));
  sb := AnsiToUtf8(String(sa));
  if (Length(sb) = 2) and (saul[1] = sb[1]) and (saul[2] = sb[2]) and
     (CompareText(FDBCharacterSet, 'US7ASCII') = 0) then
    AMessages.Add('Warning: The US7ASCII for Western Europa languages may lead to conversion losts.');
{$ENDIF}
end;

{-------------------------------------------------------------------------------}
{ TFDPhysOracleTransaction                                                      }
{-------------------------------------------------------------------------------}
function TFDPhysOracleTransaction.GetOraConnection: TFDPhysOracleConnection;
begin
  Result := TFDPhysOracleConnection(ConnectionObj);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleTransaction.UseHandle(AHandle: pOCIHandle);
begin
  ASSERT(FTransaction = nil);
  FTransaction := TOCITransaction.CreateUsingHandle(OraConnection.FService, AHandle);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleTransaction.InternalAllocHandle;
begin
  ASSERT((FTransaction = nil) or not FTransaction.OwnHandle);
  if FTransaction = nil then
    FTransaction := TOCITransaction.Create(OraConnection.FService);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleTransaction.InternalReleaseHandle;
begin
  FDFreeAndNil(FTransaction);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleTransaction.InternalSelect;
begin
  if FTransaction <> nil then
    FTransaction.Select;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleTransaction.InternalStartTransaction(ATxID: LongWord);
const
  ntisol2oci: array[TFDTxIsolation] of TOCITransactionMode = (
    tmDefault, tmReadWrite, tmReadWrite, tmReadWrite, tmReadWrite,
    tmSerializable);
var
  eMode: TOCITransactionMode;
begin
  if GetOptions.ReadOnly then
    eMode := tmReadOnly
  else
    eMode := ntisol2oci[GetOptions.Isolation];
  FTransaction.StartLocal(eMode);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleTransaction.InternalCommit(ATxID: LongWord);
begin
  FTransaction.Commit;
  if Retaining then
    InternalStartTransaction(ATxID);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleTransaction.InternalRollback(ATxID: LongWord);
begin
  FTransaction.RollBack;
  if Retaining then
    InternalStartTransaction(ATxID);
end;

{-------------------------------------------------------------------------------}
function TFDPhysOracleTransaction.GetAutoCommit: Boolean;
begin
  Result := CLIAutoCommit and not GetActive;
end;

{-------------------------------------------------------------------------------}
function TFDPhysOracleTransaction.GetCliObj: Pointer;
begin
  Result := FTransaction;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleTransaction.InternalCheckState(ACommandObj: TFDPhysCommand; ASuccess: Boolean);
begin
  if ASuccess and not GetActive and not GetOptions.AutoCommit then
    case TFDPhysOracleCommand(ACommandObj).GetCommandKind of
    skSelectForLock,
    skDelete,
    skInsert,
    skMerge,
    skUpdate: TransactionStarted;
    end;
  inherited InternalCheckState(ACommandObj, ASuccess);
end;

{-------------------------------------------------------------------------------}
{ TFDPhysOracleEventThread                                                      }
{-------------------------------------------------------------------------------}
type
  TFDPhysOracleEventThread = class(TThread)
  private
    FAlerter: TFDPhysOracleEventAlerter;
  protected
    procedure Execute; override;
  public
    constructor Create(AAlerter: TFDPhysOracleEventAlerter);
    destructor Destroy; override;
  end;

{-------------------------------------------------------------------------------}
constructor TFDPhysOracleEventThread.Create(AAlerter: TFDPhysOracleEventAlerter);
begin
  inherited Create(False);
  FAlerter := AAlerter;
  FreeOnTerminate := True;
end;

{-------------------------------------------------------------------------------}
destructor TFDPhysOracleEventThread.Destroy;
begin
  FAlerter.FWaitThread := nil;
  inherited Destroy;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleEventThread.Execute;
begin
  while not Terminated and FAlerter.IsRunning do
    try
      FAlerter.FWaitCommand.Execute();
      if not Terminated then
        FAlerter.DoFired;
    except
      on E: EFDDBEngineException do
        if E.Kind <> ekCmdAborted then begin
          Terminate;
          FAlerter.AbortJob;
        end;
    end;
end;

{-------------------------------------------------------------------------------}
{ TFDPhysOracleEventAlerter                                                     }
{-------------------------------------------------------------------------------}
procedure TFDPhysOracleEventAlerter.InternalAllocHandle;
begin
  FWaitConnection := GetConnection.Clone;
  if FWaitConnection.State = csDisconnected then
    FWaitConnection.Open;
  FWaitConnection.CreateCommand(FWaitCommand);
  SetupCommand(FWaitCommand);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleEventAlerter.InternalRegister;
begin
  FWaitThread := TFDPhysOracleEventThread.Create(Self);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleEventAlerter.InternalUnregister;
begin
  FWaitThread := nil;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleEventAlerter.InternalReleaseHandle;
begin
  FSignalCommand := nil;
  FWaitCommand := nil;
  FWaitConnection := nil;
end;

{-------------------------------------------------------------------------------}
{ TFDPhysOracleEventMessage_DBMS_ALERT                                          }
{-------------------------------------------------------------------------------}
type
  TFDPhysOracleEventMessage_DBMS_ALERT = class(TFDPhysEventMessage)
  private
    FName,
    FMessage: String;
  public
    constructor Create(const AName, AMessage: String);
  end;

{-------------------------------------------------------------------------------}
constructor TFDPhysOracleEventMessage_DBMS_ALERT.Create(const AName, AMessage: String);
begin
  inherited Create;
  FName := AName;
  FMessage := AMessage;
end;

{-------------------------------------------------------------------------------}
{ TFDPhysOracleEventAlerter_DBMS_ALERT                                          }
{-------------------------------------------------------------------------------}
const
  C_WakeUpEvent = C_FD_SysNamePrefix + 'WAKEUP';

procedure TFDPhysOracleEventAlerter_DBMS_ALERT.InternalRegister;
var
  sSQL: String;
  i: Integer;
  oPar: TFDParam;
begin
  sSQL := 'BEGIN' + C_FD_EOL + 'SYS.DBMS_ALERT.REGISTER(''' + C_WakeUpEvent + ''');' + C_FD_EOL;
  for i := 0 to GetNames().Count - 1 do
    sSQL := sSQL + 'SYS.DBMS_ALERT.REGISTER(' + QuotedStr(Trim(GetNames()[i])) + ');' + C_FD_EOL;
  sSQL := sSQL + 'END;';

  FWaitCommand.Prepare(sSQL);
  FWaitCommand.Execute();

  FWaitCommand.CommandText := 'BEGIN' + C_FD_EOL +
    'SYS.DBMS_ALERT.WAITANY(:name, :message, :status, :timeout);' + C_FD_EOL +
    'END;';
  oPar := FWaitCommand.Params[0];
  oPar.ParamType := ptOutput;
  oPar.DataType := ftString;
  oPar.Size := 30;

  oPar := FWaitCommand.Params[1];
  oPar.ParamType := ptOutput;
  oPar.DataType := ftString;
  oPar.Size := 1800;

  oPar := FWaitCommand.Params[2];
  oPar.ParamType := ptOutput;
  oPar.DataType := ftInteger;

  oPar := FWaitCommand.Params[3];
  oPar.ParamType := ptInput;
  oPar.DataType := ftInteger;
  oPar.AsInteger := 86400;

  FWaitCommand.Prepare();

  inherited InternalRegister;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleEventAlerter_DBMS_ALERT.DoFired;
begin
  if (FWaitCommand.Params[2].AsInteger = 0) and
     (CompareText(FWaitCommand.Params[0].AsString, C_WakeUpEvent) <> 0) then
    FMsgThread.EnqueueMsg(TFDPhysOracleEventMessage_DBMS_ALERT.Create(
      FWaitCommand.Params[0].AsString, FWaitCommand.Params[1].AsString));
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleEventAlerter_DBMS_ALERT.InternalHandle(AEventMessage: TFDPhysEventMessage);
var
  oMsg: TFDPhysOracleEventMessage_DBMS_ALERT;
begin
  if GetHandler <> nil then begin
    oMsg := TFDPhysOracleEventMessage_DBMS_ALERT(AEventMessage);
    GetHandler.HandleEvent(oMsg.FName, oMsg.FMessage);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleEventAlerter_DBMS_ALERT.InternalAbortJob;
begin
  FWaitThread.Terminate;
  InternalSignal(C_WakeUpEvent, Null);
  FWaitCommand.AbortJob(True);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleEventAlerter_DBMS_ALERT.InternalUnregister;
var
  sSQL: String;
  i: Integer;
begin
  inherited InternalUnregister;

  if FWaitCommand <> nil then
    try
      sSQL := 'BEGIN' + C_FD_EOL + 'SYS.DBMS_ALERT.REMOVE(''' + C_WakeUpEvent + ''');' + C_FD_EOL;
      for i := 0 to GetNames().Count - 1 do
        sSQL := sSQL + 'SYS.DBMS_ALERT.REMOVE(' + QuotedStr(Trim(GetNames()[i])) + ');' + C_FD_EOL;
      sSQL := sSQL + 'END;';

      FWaitCommand.Prepare(sSQL);
      FWaitCommand.Execute();
    except
      // hide exception
    end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleEventAlerter_DBMS_ALERT.InternalSignal(const AEvent: String;
  const AArgument: Variant);
var
  oPar: TFDParam;
begin
  if FSignalCommand = nil then begin
    GetConnection.CreateCommand(FSignalCommand);
    SetupCommand(FSignalCommand);
    FSignalCommand.CommandText := 'BEGIN SYS.DBMS_ALERT.SIGNAL(:name, :message); END;';

    oPar := FSignalCommand.Params[0];
    oPar.DataType := ftString;
    oPar.Size := 30;

    oPar := FSignalCommand.Params[1];
    oPar.DataType := ftString;
    oPar.Size := 1800;

    FSignalCommand.Prepare();
  end;

  FSignalCommand.Params[0].AsString := AEvent;
  FSignalCommand.Params[1].AsString := VarToStr(AArgument);
  FSignalCommand.Execute();
end;

{-------------------------------------------------------------------------------}
{ TFDPhysOracleEventMessage_DBMS_ALERT                                          }
{-------------------------------------------------------------------------------}
type
  TFDPhysOraclEventMessage_DBMS_PIPE = class(TFDPhysEventMessage)
  private
    FName: String;
    FValues: Variant;
  public
    constructor Create(const AName: String; const AValues: Variant);
  end;

{-------------------------------------------------------------------------------}
constructor TFDPhysOraclEventMessage_DBMS_PIPE.Create(const AName: String;
  const AValues: Variant);
begin
  inherited Create;
  FName := AName;
  FValues := AValues;
end;

{-------------------------------------------------------------------------------}
{ TFDPhysOracleEventAlerter_DBMS_PIPE                                           }
{-------------------------------------------------------------------------------}
procedure TFDPhysOracleEventAlerter_DBMS_PIPE.InternalAllocHandle;
begin
  if GetNames().Count > 1 then
    FDException(Self, [S_FD_LPhys, S_FD_OraId], er_FD_OraPipeAlertToMany, []);
  inherited InternalAllocHandle;
  FWaitConnection.CreateCommand(FReadCommand);
  SetupCommand(FReadCommand);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleEventAlerter_DBMS_PIPE.InternalRegister;
var
  oPar: TFDParam;
begin
  FWaitCommand.CommandText := 'BEGIN' + C_FD_EOL +
      ':result := SYS.DBMS_PIPE.RECEIVE_MESSAGE(:name, :timeout);' + C_FD_EOL +
      'END;';
  oPar := FWaitCommand.Params[0];
  oPar.ParamType := ptOutput;
  oPar.DataType := ftInteger;

  oPar := FWaitCommand.Params[1];
  oPar.ParamType := ptInput;
  oPar.DataType := ftString;
  oPar.Size := 128;

  oPar := FWaitCommand.Params[2];
  oPar.ParamType := ptInput;
  oPar.DataType := ftInteger;
  oPar.AsInteger := 86400;

  FWaitCommand.Prepare();
  FWaitCommand.Params[1].AsString := GetNames()[0];

  FReadCommand.CommandText := 'BEGIN' + C_FD_EOL +
    ':tp := SYS.DBMS_PIPE.NEXT_ITEM_TYPE;' + C_FD_EOL +
    'IF :tp = 6 THEN' + C_FD_EOL +
    '  SYS.DBMS_PIPE.UNPACK_MESSAGE(:num);' + C_FD_EOL +
    'ELSIF :tp = 9 THEN' + C_FD_EOL +
    '  SYS.DBMS_PIPE.UNPACK_MESSAGE(:vc2);' + C_FD_EOL +
    'ELSIF :tp = 11 THEN' + C_FD_EOL +
    '  SYS.DBMS_PIPE.UNPACK_MESSAGE_ROWID(:vc2);' + C_FD_EOL +
    'ELSIF :tp = 12 THEN' + C_FD_EOL +
    '  SYS.DBMS_PIPE.UNPACK_MESSAGE(:dt);' + C_FD_EOL +
    'ELSIF :tp = 23 THEN' + C_FD_EOL +
    '  SYS.DBMS_PIPE.UNPACK_MESSAGE_RAW(:vc2);' + C_FD_EOL +
    'END IF;' + C_FD_EOL +
    'END;';
  oPar := FReadCommand.Params[0];
  oPar.ParamType := ptOutput;
  oPar.DataType := ftInteger;

  oPar := FReadCommand.Params[1];
  oPar.ParamType := ptOutput;
  oPar.DataType := ftFloat;

  oPar := FReadCommand.Params[2];
  oPar.ParamType := ptOutput;
  oPar.DataType := ftString;
  oPar.Size := 1024;

  oPar := FReadCommand.Params[3];
  oPar.ParamType := ptOutput;
  oPar.DataType := ftDateTime;

  FReadCommand.Prepare();

  inherited InternalRegister;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleEventAlerter_DBMS_PIPE.DoFired;
var
  i: Integer;
  vVal: Variant;
  aValues: Variant;
begin
  case FWaitCommand.Params[0].AsInteger of
  0: {success}
    begin
      i := 0;
      aValues := Null;
      while True do begin
        FReadCommand.Execute();
        case FReadCommand.Params[0].AsInteger of
        0:
          Break;
        9 {vc}, 11 {rowid}, 23 {raw}:
          vVal := FReadCommand.Params[2].AsString;
        6 {num}:
          vVal := FReadCommand.Params[1].AsFloat;
        12 {date}:
          vVal := FReadCommand.Params[3].AsDateTime;
        else
          vVal := Null;
        end;
        if i = 0 then
          aValues := VarArrayCreate([0, 0], varVariant)
        else
          VarArrayRedim(aValues, i);
        aValues[i] := vVal;
        Inc(i);
      end;
      FMsgThread.EnqueueMsg(TFDPhysOraclEventMessage_DBMS_PIPE.Create(
        FWaitCommand.Params[1].AsString, aValues));
    end;
  1: {timeout}
    ;
  else
    FWaitThread.Terminate;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleEventAlerter_DBMS_PIPE.InternalHandle(
  AEventMessage: TFDPhysEventMessage);
var
  oMsg: TFDPhysOraclEventMessage_DBMS_PIPE;
begin
  if GetHandler <> nil then begin
    oMsg := TFDPhysOraclEventMessage_DBMS_PIPE(AEventMessage);
    GetHandler.HandleEvent(oMsg.FName, oMsg.FValues);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleEventAlerter_DBMS_PIPE.InternalAbortJob;
begin
  FWaitThread.Terminate;
  InternalSignal(GetNames()[0], Null);
  FWaitCommand.AbortJob(True);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleEventAlerter_DBMS_PIPE.InternalReleaseHandle;
begin
  FReadCommand := nil;
  FWriteCommand := nil;
  inherited InternalReleaseHandle;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleEventAlerter_DBMS_PIPE.InternalSignal(
  const AEvent: String; const AArgument: Variant);
var
  i: Integer;
  oPar: TFDParam;
begin
  if FSignalCommand = nil then begin
    GetConnection.CreateCommand(FSignalCommand);
    SetupCommand(FSignalCommand);
    FSignalCommand.CommandText := 'DECLARE i INTEGER; BEGIN i := SYS.DBMS_PIPE.SEND_MESSAGE(:name); END;';

    oPar := FSignalCommand.Params[0];
    oPar.DataType := ftString;
    oPar.Size := 128;

    FSignalCommand.Prepare();

    GetConnection.CreateCommand(FWriteCommand);
    SetupCommand(FWriteCommand);
    FWriteCommand.CommandText := 'BEGIN SYS.DBMS_PIPE.PACK_MESSAGE(:name); END;';
  end;

  if VarIsArray(AArgument) then
    for i := VarArrayLowBound(AArgument, 1) to VarArrayHighBound(AArgument, 1) do begin
      FWriteCommand.Disconnect;

      oPar := FWriteCommand.Params[0];
      oPar.Clear();
      oPar.DataType := ftUnknown;
      oPar.Value := AArgument[i];

      FWriteCommand.Execute();
    end
  else if not (VarIsNull(AArgument) or VarIsEmpty(AArgument)) then begin
    FWriteCommand.Disconnect;

    oPar := FWriteCommand.Params[0];
    oPar.Clear();
    oPar.DataType := ftUnknown;
    oPar.Value := AArgument;

    FWriteCommand.Execute();
  end;

  FSignalCommand.Params[0].AsString := AEvent;
  FSignalCommand.Execute();
end;

{-------------------------------------------------------------------------------}
{ TFDPhysOracleCommand                                                          }
{-------------------------------------------------------------------------------}
constructor TFDPhysOracleCommand.Create(AConnectionObj: TFDPhysConnection);
begin
  inherited Create(AConnectionObj);
  FInfoStack := TFDPtrList.Create;
  FCrsInfos := TFDPtrList.Create;
end;

{-------------------------------------------------------------------------------}
destructor TFDPhysOracleCommand.Destroy;
begin
  inherited Destroy;
  FDFreeAndNil(FInfoStack);
  FDFreeAndNil(FCrsInfos);
end;

{-------------------------------------------------------------------------------}
function TFDPhysOracleCommand.GetCliObj: Pointer;
begin
  if FCurrentCrsInfo = nil then
    Result := FBase.FStmt
  else
    Result := FCurrentCrsInfo^.FStmt;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleCommand.SQL2ADColInfo(AOraDataType: TOCIVarDataType;
  AOraSize, AOraPrec, AOraScale: sb4; var AType: TFDDataType;
  var AAttrs: TFDDataAttributes; var ALen: LongWord; var APrec, AScale: Integer);
var
  oFmt: TFDFormatOptions;
begin
  AType := ovdt2addt[AOraDataType];
  ALen := 0;
  APrec := 0;
  AScale := 0;
  Exclude(AAttrs, caFixedLen);
  Exclude(AAttrs, caBlobData);
  Include(AAttrs, caSearchable);
  case AOraDataType of
  otChar,
  otNChar:
    begin
      Include(AAttrs, caFixedLen);
      ALen := AOraSize;
    end;
  otString,
  otNString,
  otRaw:
    ALen := AOraSize;
  otROWID:
    begin
      Include(AAttrs, caFixedLen);
      Include(AAttrs, caAllowNull);
      Include(AAttrs, caROWID);
      ALen := AOraSize;
    end;
  otNumber,
  otDouble:
    begin
      APrec := AOraPrec;
      AScale := AOraScale;
      oFmt := FOptions.FormatOptions;
      if (AType = dtFmtBCD) and
         (APrec <= oFmt.MaxBcdPrecision) and (AScale <= oFmt.MaxBcdScale) then
        AType := dtBCD;
    end;
  otDateTime:
    AScale := 1000;
  otTimeStamp:
    if (AOraScale >= 0) and (AOraScale < 3) then
      AScale := C_FD_ScaleFactor[3 - AOraScale];
  otIntervalYM,
  otIntervalDS:
    begin
      APrec := AOraPrec;
      if (AOraScale >= 0) and (AOraScale < 3) then
        AScale := C_FD_ScaleFactor[3 - AOraScale];
    end;
  otCFile,
  otBFile:
    begin
      Include(AAttrs, caVolatile);
      Include(AAttrs, caBlobData);
      Exclude(AAttrs, caSearchable);
    end;
  otLong,
  otNLong,
  otLongRaw,
  otCLOB,
  otNCLOB,
  otBLOB:
    begin
      Include(AAttrs, caBlobData);
      Exclude(AAttrs, caSearchable);
    end;
  otCursor,
  otNestedDataSet:
    begin
      Include(AAttrs, caAllowNull);
      Exclude(AAttrs, caSearchable);
    end;
  end;
end;

{-------------------------------------------------------------------------------}
function TFDPhysOracleCommand.FDType2OCIType(AADType: TFDDataType;
  AFixedLen: Boolean): TOCIVarDataType;
begin
  if AADType = dtBoolean then
    Result := TFDPhysOracleConnection(FConnectionObj).FBooleanFormat
  else begin
    Result := addt2ovdt[AADType];
    if AFixedLen then
      if Result = otString then
        Result := otChar
      else if Result = otNString then
        Result := otNChar;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleCommand.DefParInfos(const ANm: String; AVt: TOCIVarType;
  FDt: TOCIVarDataType; ASz, APrec, AScale: sb4; AIsTable, AIsResult: Boolean);
var
  oFmt: TFDFormatOptions;
  oPar: TFDParam;
  eSrcType, eDestType: TFDDataType;
  eSrcAttrs: TFDDataAttributes;
  eDestFldType: TFieldType;
  iSize: LongWord;
  iPrec, iScale: Integer;
begin
  oFmt := FOptions.FormatOptions;
  oPar := GetParams.Add;
  oPar.Name := ANm;
  oPar.Position := oPar.Index + 1;
  if AIsResult then
    oPar.ParamType := ptResult
  else
    oPar.ParamType := vt2pt[AVt];
  eSrcAttrs := [];
  SQL2ADColInfo(FDt, ASz, APrec, AScale, eSrcType, eSrcAttrs, iSize,
    iPrec, iScale);
  oFmt.ResolveDataType(ANm, eSrcType, iSize, iPrec, iScale, eDestType, iSize, True);
  oFmt.ColumnDef2FieldDef(eDestType, iSize, iPrec, iScale, eSrcAttrs,
    eDestFldType, iSize, iPrec, iScale);
  oPar.DataType := eDestFldType;
  oPar.ADDataType := eDestType;
  oPar.Size := iSize;
  oPar.Precision := iPrec;
  oPar.NumericScale := iScale;
  if AIsTable then
    oPar.ArrayType := atPLSQLTable
  else
    oPar.ArrayType := atScalar;
  if (GetCommandKind = skStoredProc) and (FDt in [otCursor, otNestedDataSet]) then
    SetCommandKind(skStoredProcWithCrs);
end;

{-------------------------------------------------------------------------------}
function TFDPhysOracleCommand.GenerateStoredProcCallUsingOCI(const AName: TFDPhysParsedName): String;
var
  sPackName, sProcName: String;
  oDescr: TOCIPLSQLDescriber;
begin
  GetParams.Clear;

  sProcName := AName.FObject;
  sPackName := AName.FBaseObject;
  if AName.FSchema <> '' then
    if sPackName <> '' then
      sPackName := AName.FSchema + '.' + sPackName
    else
      sProcName := AName.FSchema + '.' + sProcName;
  if AName.FLink <> '' then
    if sPackName <> '' then
      sPackName := sPackName + '@' + AName.FLink
    else
      sProcName := sProcName + '@' + AName.FLink;

  oDescr := TOCIPLSQLDescriber.CreateForProc(TFDPhysOracleConnection(FConnectionObj).FService,
    sPackName, sProcName, GetOverload, Self);
  try
    oDescr.BoolType := otBoolean;
    oDescr.BindByName := GetParams.BindMode = pbByName;
    case TFDPhysOracleConnection(FConnectionObj).FBooleanFormat of
    otInteger:
      begin
        oDescr.BoolFalse := '0';
        oDescr.BoolTrue := '1';
      end;
    otString:
      begin
        oDescr.BoolFalse := '''F''';
        oDescr.BoolTrue := '''T''';
      end;
    end;
    oDescr.Describe;
    oDescr.LocateProc(False);
    Result := oDescr.BuildSQL(DefParInfos);
  finally
    FDFree(oDescr);
  end;
  if GetCommandKind = skStoredProc then
    SetCommandKind(skStoredProcNoCrs);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleCommand.CreateDefineInfo(ACrsInfo: PFDOraCrsDataRec);
var
  i, iNChars: Integer;
  oSelItem: TOCISelectItem;
  oFmtOpts: TFDFormatOptions;
  rName: TFDPhysParsedName;
  oConnMeta: IFDPhysConnectionMetadata;
  lMetadata: Boolean;
  iLen: LongWord;
  iPrec, iScale: Integer;
  iUnit: ub4;
  pInfo: PFDOraVarInfoRec;
  oVar: TOCIVariable;
begin
  FConnection.CreateMetadata(oConnMeta);
  oFmtOpts := FOptions.FormatOptions;
  lMetadata := GetMetaInfoKind <> FireDAC.Phys.Intf.mkNone;

  iNChars := 0;
  ACrsInfo^.FHasFields := [];
  SetLength(ACrsInfo^.FColInfos, ACrsInfo^.FStmt.PARAM_COUNT);
  for i := 1 to Length(ACrsInfo^.FColInfos) do begin
    oSelItem := TOCISelectItem.Create(ACrsInfo^.FStmt, ACrsInfo^.FStmt.Service, i);
    try
      pInfo := @ACrsInfo^.FColInfos[i - 1];
      pInfo^.FName := oSelItem.NAME;
      pInfo^.FPos := i;
      pInfo^.FVarType := odDefine;
      pInfo^.FOSrcType := oSelItem.DataType;
      pInfo^.FByteSize := oSelItem.DataSize;
      pInfo^.FLogSize := oSelItem.ColumnSize;
      pInfo^.FPrec := oSelItem.DataPrecision;
      pInfo^.FScale := oSelItem.DataScale;

      if oSelItem.IsNull then
        Include(pInfo^.FAttrs, caAllowNull);

      SQL2ADColInfo(pInfo^.FOSrcType, pInfo^.FLogSize, pInfo^.FPrec, pInfo^.FScale,
        pInfo^.FSrcType, pInfo^.FAttrs, iLen, iPrec, iScale);

      if lMetadata then
        // Oracle may return textual metadata columns as VARCHAR,
        // but FireDAC expects Unicode string
        if pInfo^.FSrcType = dtAnsiString then
          pInfo^.FDestType := dtWideString
        else
          pInfo^.FDestType := pInfo^.FSrcType
      else
        // mapping data types
        oFmtOpts.ResolveDataType(pInfo^.FName, pInfo^.FSrcType, iLen, iPrec, iScale,
          pInfo^.FDestType, iLen, True);

      pInfo^.FOOutputType := FDType2OCIType(pInfo^.FDestType,
        pInfo^.FOSrcType in [otChar, otNChar]);
      // OCI fails to fetch a string data as a long data, when OCIStmtExecute(0)
      // & OCIStmtFetch(<rowset>) is called instead of Describe & Execute & Fetch
      if (pInfo^.FOSrcType in [otString, otChar, otNString, otNChar, otRaw]) and
         (pInfo^.FOOutputType in [otLong, otNLong, otLongRaw]) then
        pInfo^.FOOutputType := pInfo^.FOSrcType;

      if pInfo^.FOOutputType <> otUnknown then begin
        SQL2ADColInfo(pInfo^.FOOutputType, iLen, pInfo^.FPrec, pInfo^.FScale,
          pInfo^.FOutputType, pInfo^.FAttrs, iLen, iPrec, iScale);
        if pInfo^.FDestType = pInfo^.FSrcType then
          pInfo^.FOOutputType := pInfo^.FOSrcType;
      end
      else begin
        SQL2ADColInfo(pInfo^.FOSrcType, iLen, pInfo^.FPrec, pInfo^.FScale,
          pInfo^.FOutputType, pInfo^.FAttrs, iLen, iPrec, iScale);
        pInfo^.FOOutputType := pInfo^.FOSrcType;
      end;
      pInfo^.FADScale := iScale;

      // if data type conversion is required and a value will be returned
      // as string, then use the value display size as a bind size
      if pInfo^.FOOutputType <> pInfo^.FOSrcType then begin
        case pInfo^.FDestType of
        dtByteString: iUnit := SizeOf(Byte);
        dtAnsiString: iUnit := SizeOf(TFDAnsiChar);
        dtWideString: iUnit := SizeOf(WideChar);
        else          iUnit := 0;
        end;
        if iUnit <> 0 then begin
          pInfo^.FLogSize := oSelItem.DISP_SIZE;
          if pInfo^.FLogSize = 0 then
            pInfo^.FLogSize := iLen;
          pInfo^.FByteSize := pInfo^.FLogSize * iUnit;
        end;
      end;
      if (pInfo^.FSrcType = dtBCD) and (pInfo^.FOutputType = dtFmtBCD) then
        pInfo^.FOutputType := dtBCD;

      if oSelItem.TYPE_NAME <> '' then begin
        rName.FSchema := oSelItem.SCHEMA_NAME;
        rName.FObject := oSelItem.TYPE_NAME;
        pInfo^.FExtName := oConnMeta.EncodeObjName(rName, Self, [eoNormalize]);
      end;

      if CheckFetchColumn(pInfo^.FSrcType, pInfo^.FAttrs) or
         (pInfo^.FDestType in [dtRowSetRef, dtCursorRef, dtRowRef, dtArrayRef]) then begin
        pInfo^.FVar := TOCIVariable.Create(ACrsInfo^.FStmt);
        oVar := pInfo^.FVar;
        oVar.VarType := odDefine;
        oVar.Position := pInfo^.FPos;
        oVar.DumpLabel := pInfo^.FName;
        oVar.DataType := pInfo^.FOOutputType;
        if pInfo^.FOOutputType in [otString, otChar, otNString, otNChar, otRaw] then begin
          oVar.DataSize := pInfo^.FByteSize;
          oVar.CharSize := pInfo^.FLogSize;
        end;
        if pInfo^.FOOutputType in [otNString, otNChar, otNLong] then begin
          Include(ACrsInfo^.FHasFields, hfNChar);
          Inc(iNChars);
        end
        else if pInfo^.FOOutputType in otHBlobs then
          Include(ACrsInfo^.FHasFields, hfBlob)
        else if oVar.LongData then
          Include(ACrsInfo^.FHasFields, hfLongData);
      end
      else
        pInfo^.FVar := nil;

    finally
      FDFree(oSelItem);
    end;
  end;

  if iNChars > 2 then
    Include(ACrsInfo^.FHasFields, hfManyNChars);

  if ACrsInfo^.FStmt.Handle <> nil then
    ResetDefVars(ACrsInfo, GetRowsetSize(ACrsInfo, FOptions.FetchOptions.ActualRowsetSize));
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleCommand.DestroyDefineInfo(ACrsInfo: PFDOraCrsDataRec);
var
  i: Integer;
  pInfo: PFDOraVarInfoRec;
begin
  if Length(ACrsInfo^.FColInfos) <> 0 then
    try
      for i := 0 to Length(ACrsInfo^.FColInfos) - 1 do begin
        pInfo := @ACrsInfo^.FColInfos[i];
        if pInfo^.FCrsInfo <> nil then begin
          try
            try
              DestroyDefineInfo(pInfo^.FCrsInfo);
            finally
              DestroyStmt(pInfo^.FCrsInfo);
            end;
          except
            // no exceptions visible
          end;
          FreeMem(pInfo^.FCrsInfo);
          pInfo^.FCrsInfo := nil;
        end;
        if pInfo^.FVar <> nil then begin
          try
            pInfo^.FVar.BindOff;
          except
            // no exceptions visible
          end;
          FDFreeAndNil(pInfo^.FVar);
        end;
      end;
    finally
      SetLength(ACrsInfo^.FColInfos, 0);
    end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleCommand.DestroyStmt(ACrsInfo: PFDOraCrsDataRec);
begin
  FDFreeAndNil(ACrsInfo^.FStmt);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleCommand.CreateBindInfo;
var
  i: Integer;
  oParams: TFDParams;
  oParam: TFDParam;
  pInfo: PFDOraVarInfoRec;
  oFmtOpts: TFDFormatOptions;
  oResOpts: TFDResourceOptions;
  eDestFldType: TFieldType;
  iDestPrec, iDestScale: Integer;
  lWasVBlob: Boolean;
  eAttrs: TFDDataAttributes;
  iPrec, iScale: Integer;
  iLen: LongWord;
  iArrSize: Integer;
  oVar: TOCIVariable;
begin
  FBindVarsDirty := False;
  FHasParams := [];
  oParams := GetParams;
  if oParams.Count = 0 then
    Exit;

  oFmtOpts := FOptions.FormatOptions;
  oResOpts := FOptions.ResourceOptions;
  lWasVBlob := False;
  iArrSize := oParams.ArraySize;
  if iArrSize > oResOpts.ArrayDMLSize then
    iArrSize := oResOpts.ArrayDMLSize;

  SetLength(FParInfos, oParams.Count);
  for i := 0 to Length(FParInfos) - 1 do begin
    oParam := oParams[i];
    pInfo := @FParInfos[i];

    // fill in base info
    case GetParams.BindMode of
    pbByName:
      begin
        pInfo^.FPos := 0;
        pInfo^.FName := ':' + oParam.Name;
        if Length(pInfo^.FName) > 30 then
          pInfo^.FName := Copy(pInfo^.FName, 1, 30 - 1 - Length(IntToStr(i + 1))) + '_' + IntToStr(i + 1);
        pInfo^.FExtName := pInfo^.FName;
      end;
    pbByNumber:
      begin
        pInfo^.FPos := oParam.Position;
        pInfo^.FName := '';
        pInfo^.FExtName := IntToStr(pInfo^.FPos);
      end;
    end;
    pInfo^.FIsCaseSensitive := oParam.IsCaseSensitive;
    pInfo^.FIsPLSQLTable := (oParam.ArrayType = atPLSQLTable);
    pInfo^.FParamType := oParam.ParamType;
    pInfo^.FDataType := oParam.DataType;
    pInfo^.FVarType := pt2vt[pInfo^.FParamType];

    // resolve data type
    if oParam.DataType = ftUnknown then
      ParTypeUnknownError(oParam);
    eDestFldType := ftUnknown;
    iDestPrec := 0;
    oFmtOpts.ResolveFieldType('', oParam.DataType, oParam.ADDataType, oParam.Size,
      oParam.Precision, oParam.NumericScale, eDestFldType, pInfo^.FLogSize,
      iDestPrec, iDestScale, pInfo^.FSrcType, pInfo^.FDestType, False);

    // Oracle does not support Delphi Currency data type, so map it to BCD
    if pInfo^.FDestType = dtCurrency then
      pInfo^.FDestType := dtBCD;
    pInfo^.FOSrcType := FDType2OCIType(pInfo^.FSrcType, oParam.DataType in [
      ftFixedChar, ftBytes, ftFixedWideChar]);
    pInfo^.FOOutputType := FDType2OCIType(pInfo^.FDestType, eDestFldType in [
      ftFixedChar, ftBytes, ftFixedWideChar]);
    if (pInfo^.FOOutputType = otUnknown) or (eDestFldType = ftBytes) then
      ParTypeMapError(oParam);

    eAttrs := [];
    SQL2ADColInfo(pInfo^.FOOutputType, oParam.Size, oParam.Precision,
      oParam.NumericScale, pInfo^.FOutputType, eAttrs, iLen, iPrec, iScale);
    // for driver dtBCD and dtFtmBCD are the same
    if (pInfo^.FDestType = dtBCD) and (pInfo^.FOutputType = dtFmtBCD) then
      pInfo^.FOutputType := dtBCD;

    // limit maximum string parameter size in PL/SQL blocks
    if (pInfo^.FOOutputType in [otString, otChar, otNString, otNChar, otRaw]) and
       (pInfo^.FLogSize = 0) and (oFmtOpts.MaxStringSize <> C_FD_DefMaxStrSize) and
       (GetCommandKind in [skStoredProc, skStoredProcWithCrs, skStoredProcNoCrs, skExecute]) then
      pInfo^.FLogSize := oFmtOpts.MaxStringSize;
    // set size in bytes
    if pInfo^.FOOutputType in [otString, otChar, otLong] then
      pInfo^.FByteSize := pInfo^.FLogSize * SizeOf(TFDAnsiChar)
    else if pInfo^.FOOutputType in [otNString, otNChar, otNLong] then
      pInfo^.FByteSize := pInfo^.FLogSize * SizeOf(WideChar)
    else
      pInfo^.FByteSize := pInfo^.FLogSize;

    // At moment table adapter handles BLOB's in UPDATE and INSERT only
    // in RETURNING phrase -> MUST BE CHANGED !!!!
    // Otherwise, INSERT INTO All_types (TCLOB) VALUES (:TCLOB) with
    // temporary CLOB will not work - "capability not supported"
    if (pInfo^.FOOutputType in otHBlobs) and (pInfo^.FVarType in [odIn, odOut, odInOut]) and
       (TFDPhysOracleConnection(FConnectionObj).FEnv.Lib.Version < cvOracle81000) and
       (FBase.FStmt.STMT_TYPE in [OCI_STMT_UPDATE, OCI_STMT_INSERT]) then begin
      pInfo^.FVarType := odRet;
      Include(FHasParams, hpHBlobsRet);
    end
    else begin
      if pInfo^.FVarType in [odUnknown, odIn, odInOut] then
        Include(FHasParams, hpInput);
      if pInfo^.FVarType in [odOut, odInOut, odRet] then
        Include(FHasParams, hpOutput);
    end;
    if pInfo^.FOOutputType in otCrsTypes then
      Include(FHasParams, hpCursors);
    if pInfo^.FOOutputType in otVBlobs then
      if lWasVBlob then
        Include(FHasParams, hpManyVLobs)
      else
        lWasVBlob := True;

    // check if it is array
    if pInfo^.FIsPLSQLTable then
      pInfo^.FArrayLen := oParam.ArraySize
    else
      pInfo^.FArrayLen := iArrSize;

    // create OCI variable
    pInfo^.FVar := TOCIVariable.Create(FBase.FStmt);
    oVar := pInfo^.FVar;
    oVar.VarType := pInfo^.FVarType;
    oVar.Position := pInfo^.FPos;
    oVar.Name := pInfo^.FName;
    oVar.DumpLabel := pInfo^.FExtName;
    oVar.IsCaseSensitive := pInfo^.FIsCaseSensitive;
    oVar.IsPLSQLTable := pInfo^.FIsPLSQLTable;
    oVar.DataType := pInfo^.FOOutputType;
    if (pInfo^.FByteSize <> 0) and
       ((pInfo^.FOOutputType in [otString, otChar, otNString, otNChar, otRaw]) or
        (pInfo^.FOOutputType in otVBlobs)) then begin
      oVar.DataSize := pInfo^.FByteSize;
      oVar.CharSize := pInfo^.FLogSize;
    end;
    oVar.ArrayLen := pInfo^.FArrayLen;
    oVar.Bind;
    if oVar.LongData then
      Include(FHasParams, hpLongData);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleCommand.DestroyBindInfo;
var
  i: Integer;
  pInfo: PFDOraVarInfoRec;
begin
  try
    for i := 0 to Length(FParInfos) - 1 do begin
      pInfo := @FParInfos[i];
      if pInfo^.FVar <> nil then begin
        try
          pInfo^.FVar.BindOff;
        except
          // no exceptions visible
        end;
        FDFreeAndNil(pInfo^.FVar);
      end;
    end;
  finally
    SetLength(FParInfos, 0);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleCommand.DestroyCrsInfo;
var
  i: Integer;
  pCrsInfo: PFDOraCrsDataRec;
  lBaseDestroyed: Boolean;
begin
  lBaseDestroyed := (FCrsInfos.Count > 0) and (FCrsInfos[0] = @FBase);
  try
    for i := 0 to FCrsInfos.Count - 1 do begin
      pCrsInfo := PFDOraCrsDataRec(FCrsInfos[i]);
      DestroyDefineInfo(pCrsInfo);
      DestroyStmt(pCrsInfo);
      if pCrsInfo <> @FBase then
        FreeMem(pCrsInfo, SizeOf(TFDOraCrsDataRec));
    end;
  finally
    FCrsInfos.Clear;
    if not lBaseDestroyed then begin
      DestroyDefineInfo(@FBase);
      DestroyStmt(@FBase);
    end;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleCommand.SetupStatement(AStmt: TOCIStatement);
var
  oFmtOpts: TFDFormatOptions;
begin
  oFmtOpts := FOptions.FormatOptions;
  AStmt.DecimalSep := TFDPhysOracleConnection(FConnectionObj).FDecimalSep;
  AStmt.PieceBuffLen := C_FD_DefPieceBuffLen;
  AStmt.StrsTrim := oFmtOpts.StrsTrim;
  AStmt.StrsEmpty2Null := oFmtOpts.StrsEmpty2Null;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleCommand.InternalPrepare;
var
  i: Integer;
  pParInfo: PFDOraVarInfoRec;
  pCrsInfo: PFDOraCrsDataRec;
  hndl: pOCIHandle;
  uiTmp: ub4;
  oConnMeta: IFDPhysConnectionMetadata;
  rName: TFDPhysParsedName;
  oConn: TFDPhysOracleConnection;
begin
  FInfoStack.Clear;
  FCurrentCrsInfo := nil;
  FActiveCrs := -1;
  FBindVarsDirty := False;
  FHasParams := [];
  FillChar(FBase, SizeOf(TFDOraCrsDataRec), 0);

  // generate metadata SQL command
  if GetMetaInfoKind <> FireDAC.Phys.Intf.mkNone then begin
    GetSelectMetaInfoParams(rName);
    GenerateSelectMetaInfo(rName);
    if FDbCommandText = '' then
      Exit;
  end

  // generate stored proc call SQL command
  else if GetCommandKind in [skStoredProc, skStoredProcWithCrs, skStoredProcNoCrs] then begin
    GetConnection.CreateMetadata(oConnMeta);
    FDbCommandText := '';
    if fiMeta in FOptions.FetchOptions.Items then begin
      oConnMeta.DecodeObjName(Trim(GetCommandText()), rName, Self, [doNormalize]);
      FDbCommandText := GenerateStoredProcCallUsingOCI(rName);
    end;
    if FDbCommandText = '' then begin
      oConnMeta.DecodeObjName(Trim(GetCommandText()), rName, Self, []);
      GenerateStoredProcCall(rName);
    end;
  end;

  // adjust SQL command
  GenerateLimitSelect();
  GenerateParamMarkers();

  oConn := TFDPhysOracleConnection(FConnectionObj);
  FBase.FStmt := TOCIStatement.Create(oConn.FEnv, oConn.FService,
    TFDPhysOracleTransaction(FTransactionObj).FTransaction, Self);
  SetupStatement(FBase.FStmt);
  FBase.FStmt.Prepare(FDbCommandText);
  if not (FBase.FStmt.STMT_TYPE in [OCI_STMT_EXPLAIN1, OCI_STMT_EXPLAIN2]) then
    CreateBindInfo;
  FCursorCanceled := True;
  if FBase.FStmt.STMT_TYPE = OCI_STMT_SELECT then begin
    // CreateDefineInfo is moved to InternalOpen, after a query will be executed.
    // Otherwise, here FStmt.Describe must be called before CreateDefineInfo call.
    // Describe leads to SIR-580 and slowdowns simple queries execution.
    FCrsInfos.Add(@FBase);
    FActiveCrs := 0;
    if GetCommandKind = skUnknown then
      SetCommandKind(skSelect);
  end
  else if FBase.FStmt.STMT_TYPE in [OCI_STMT_BEGIN, OCI_STMT_DECLARE] then begin
    for i := 0 to Length(FParInfos) - 1 do begin
      pParInfo := @FParInfos[i];
      if (pParInfo^.FOOutputType = otCursor) and (pParInfo^.FVar <> nil) then begin
        pParInfo^.FVar.GetData(0, @hndl, uiTmp);
        GetMem(pCrsInfo, SizeOf(TFDOraCrsDataRec));
        FillChar(pCrsInfo^, SizeOf(TFDOraCrsDataRec), 0);
        FCrsInfos.Add(pCrsInfo);
        pCrsInfo^.FParent := @FBase;
        pCrsInfo^.FStmt := TOCIStatement.CreateUsingHandle(oConn.FEnv, oConn.FService, nil, hndl);
        SetupStatement(pCrsInfo^.FStmt);
      end;
    end;
    if FCrsInfos.Count > 0 then begin
      if GetCommandKind in [skUnknown, skStoredProc] then
        SetCommandKind(skStoredProcWithCrs);
      FActiveCrs := 0;
    end
    else begin
      if GetCommandKind in [skUnknown, skStoredProc] then
        SetCommandKind(skStoredProcNoCrs);
    end;
  end
  else
    if GetCommandKind = skUnknown then
      SetCommandKind(skOther);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleCommand.InternalUnprepare;
begin
  if FBase.FStmt = nil then
    Exit;
  InternalClose;
  DestroyBindInfo;
  DestroyCrsInfo;
end;

{-------------------------------------------------------------------------------}
function TFDPhysOracleCommand.IsActiveCursorValid: Boolean;
begin
  Result := (FActiveCrs >= 0) and (FActiveCrs < FCrsInfos.Count);
end;

{-------------------------------------------------------------------------------}
function TFDPhysOracleCommand.GetActiveCursor: PFDOraCrsDataRec;
begin
  if not IsActiveCursorValid then begin
    if (GetCommandKind in [skStoredProc, skStoredProcWithCrs, skStoredProcNoCrs]) and
       (GetParams.Count = 0) and not (fiMeta in FOptions.FetchOptions.Items) then
      FDException(Self, [S_FD_LPhys, S_FD_OraId], er_FD_OraNoCursorParams, [])
    else
      FDException(Self, [S_FD_LPhys, S_FD_OraId], er_FD_OraNoCursor, []);
  end;
  Result := PFDOraCrsDataRec(FCrsInfos[FActiveCrs]);
end;

{-------------------------------------------------------------------------------}
function TFDPhysOracleCommand.InternalColInfoStart(var ATabInfo: TFDPhysDataTableInfo): Boolean;
var
  pColInfo: PFDOraVarInfoRec;
  pParCrs: PFDOraCrsDataRec;
  hndl: pOCIHandle;
  uiTmp: ub4;
begin
  Result := OpenBlocked;
  if Result then
    if ATabInfo.FSourceID = -1 then begin
      ATabInfo.FSourceName := GetCommandText;
      ATabInfo.FSourceID := 1;
      ATabInfo.FOriginName := '';

      FCurrentCrsInfo := GetActiveCursor;
      FCurrentCrsInfo^.FColIndex := 0;
    end

    else begin
      pColInfo := @FCurrentCrsInfo^.FColInfos[ATabInfo.FSourceID - 1];
      ATabInfo.FSourceName := pColInfo^.FName;
      ATabInfo.FSourceID := ATabInfo.FSourceID;
      ATabInfo.FOriginName := '';

      if pColInfo^.FCrsInfo = nil then begin
        GetMem(pColInfo^.FCrsInfo, SizeOf(TFDOraCrsDataRec));
        FillChar(pColInfo^.FCrsInfo^, SizeOf(TFDOraCrsDataRec), 0);
        pColInfo^.FVar.GetData(0, @hndl, uiTmp);
        pColInfo^.FCrsInfo.FStmt := TOCIStatement.CreateUsingHandle(
          TFDPhysOracleConnection(FConnectionObj).FEnv,
          TFDPhysOracleConnection(FConnectionObj).FService, nil, hndl);
        // There must be assigned a parent cursor
        pColInfo^.FCrsInfo^.FParent := @FBase;
        SetupStatement(pColInfo^.FCrsInfo.FStmt);
        // A cursor nested into a nested cursor does not work.
        // Only the first level cursors are supported.
        pParCrs := pColInfo^.FCrsInfo^.FParent;
        if not pParCrs^.FExecuted then begin
          pParCrs^.FExecuted := True;
          pParCrs^.FStmt.Fetch(GetRowsetSize(pColInfo^.FCrsInfo,
            FOptions.FetchOptions.ActualRowsetSize));
        end;
        CreateDefineInfo(pColInfo^.FCrsInfo);
      end;

      FInfoStack.Add(FCurrentCrsInfo);
      FCurrentCrsInfo := pColInfo^.FCrsInfo;
      FCurrentCrsInfo^.FColIndex := 0;
    end;
end;

{-------------------------------------------------------------------------------}
function TFDPhysOracleCommand.InternalColInfoGet(var AColInfo: TFDPhysDataColumnInfo): Boolean;
var
  pColInfo: PFDOraVarInfoRec;
begin
  if FCurrentCrsInfo^.FColIndex < Length(FCurrentCrsInfo^.FColInfos) then begin
    pColInfo := @FCurrentCrsInfo^.FColInfos[FCurrentCrsInfo^.FColIndex];

    AColInfo.FSourceName := pColInfo^.FName;
    AColInfo.FSourceID := pColInfo^.FPos;
    AColInfo.FSourceTypeName := pColInfo^.FExtName;
    AColInfo.FSourceType := pColInfo^.FSrcType;
    AColInfo.FType := pColInfo^.FDestType;
    AColInfo.FLen := pColInfo^.FLogSize;
    AColInfo.FPrec := pColInfo^.FPrec;
    AColInfo.FScale := pColInfo^.FADScale;
    AColInfo.FAttrs := pColInfo^.FAttrs;
    Inc(FCurrentCrsInfo^.FColIndex);
    Result := True;
  end
  else begin
    if FInfoStack.Count > 0 then begin
      FCurrentCrsInfo := PFDOraCrsDataRec(FInfoStack.Last);
      FInfoStack.Delete(FInfoStack.Count - 1);
    end;
    Result := False;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleCommand.SetParamValues(ATimes, AOffset: LongInt; AFromParIndex: Integer;
  AIntoReturning: Boolean);
var
  j, i: Integer;
  uiLen: ub4;
  oParams: TFDParams;
  oParam: TFDParam;
  pParInfo: PFDOraVarInfoRec;
  oFmtOpts: TFDFormatOptions;
  oResOpts: TFDResourceOptions;
  iArrSize: Integer;

  function CreateLocator(AParInfo: PFDOraVarInfoRec; AType: TOCIVarDataType; AIndex: ub4): TOCIIntLocator;
  var
    hndl: pOCIHandle;
    uiTmp: ub4;
  begin
    if AType in [otCFile, otBFile] then
      FDException(Self, [S_FD_LPhys, S_FD_OraId], er_FD_OraCantAssFILE,
        [AParInfo^.FExtName]);
    if AParInfo^.FVar.GetData(AIndex, @hndl, uiTmp) then begin
      Result := TOCIIntLocator.CreateUseHandle(
        TFDPhysOracleConnection(FConnectionObj).FService, hndl);
      Result.National := (AType = otNCLOB);
    end
    else begin
      Result := nil;
      FDCapabilityNotSupported(Self, [S_FD_LPhys, S_FD_OraId]);
    end;
  end;

  function CreateTimeStamp(AParInfo: PFDOraVarInfoRec; AIndex: ub4): TOCITimeStamp;
  var
    hndl: pOCIHandle;
    uiTmp: ub4;
  begin
    AParInfo^.FVar.SetIsNull(AIndex, False);
    AParInfo^.FVar.GetData(AIndex, @hndl, uiTmp);
    Result := TOCITimestamp.CreateUseHandle(
      TFDPhysOracleConnection(FConnectionObj).FEnv, hndl);
  end;

  function CreateTimeInterval(AParInfo: PFDOraVarInfoRec; AIndex: ub4): TOCITimeInterval;
  var
    hndl: pOCIHandle;
    uiTmp: ub4;
  begin
    AParInfo^.FVar.SetIsNull(AIndex, False);
    AParInfo^.FVar.GetData(AIndex, @hndl, uiTmp);
    Result := TOCITimeInterval.CreateUseHandle(
      TFDPhysOracleConnection(FConnectionObj).FEnv, hndl, AParInfo^.FOOutputType);
  end;

  procedure ProcessArrayItem(APar: TFDParam; AParInfo: PFDOraVarInfoRec; AVarIndex, AParIndex: Integer);
  var
    pData: pUb1;
    uiSrcDataLen, uiDestDataLen, uiLen: LongWord;
    oIntLoc: TOCIIntLocator;
    oTS: TOCITimeStamp;
    oTI: TOCITimeInterval;
  begin
    // if null
    if APar.IsNulls[AParIndex] then
      AParInfo^.FVar.SetData(AVarIndex, nil, 0)

    // if conversion is not required
    else if AParInfo^.FSrcType = AParInfo^.FOutputType then
      // if byte string data, then optimizing - get data directly
      case AParInfo^.FOOutputType of
      otString,
      otChar,
      otNString,
      otNChar,
      otLong,
      otNLong,
      otLongRaw,
      otCLOB,
      otNCLOB,
      otBLOB,
      otCFile,
      otBFile:
        begin
          uiLen := 0;
          pData := nil;
          APar.GetBlobRawData(uiLen, PByte(pData), AParIndex);
          if AParInfo^.FOOutputType in [otString, otChar, otNString, otNChar, otLong,
                                        otNLong, otLongRaw] then
            AParInfo^.FVar.SetData(AVarIndex, pData, uiLen)
          else if AParInfo^.FOOutputType in [otCLOB, otNCLOB, otBLOB, otCFile, otBFile] then begin
            oIntLoc := CreateLocator(AParInfo, AParInfo^.FOOutputType, AVarIndex);
            try
              if uiLen <> 0 then
                oIntLoc.Write(pData, uiLen, 1);
            finally
              FDFree(oIntLoc);
            end;
          end;
        end;
      otTimeStamp:
        begin
          oTS := CreateTimeStamp(AParInfo, AVarIndex);
          try
            FBuffer.Check;
            APar.GetData(FBuffer.Ptr, AParIndex);
            oTS.SetAsSQLTimeStamp(PSQLTimeStamp(FBuffer.Ptr)^);
          finally
            FDFree(oTS);
          end;
        end;
      otIntervalYM,
      otIntervalDS:
        begin
          oTI := CreateTimeInterval(AParInfo, AVarIndex);
          try
            FBuffer.Check;
            APar.GetData(FBuffer.Ptr, AParIndex);
            oTI.SetAsSQLTimeInterval(PFDSQLTimeInterval(FBuffer.Ptr)^);
          finally
            FDFree(oTI);
          end;
        end;
      else
        FBuffer.Check;
        APar.GetData(FBuffer.Ptr, AParIndex);
        AParInfo^.FVar.SetData(AVarIndex, FBuffer.Ptr, APar.GetDataLength(AParIndex));
      end

    // if conversion is required
    else begin
      // calculate buffer size to move param values
      uiSrcDataLen := APar.GetDataLength(AParIndex);
      uiDestDataLen := 0;
      FBuffer.Extend(uiSrcDataLen, uiDestDataLen, AParInfo^.FSrcType, AParInfo^.FOutputType);

      // get, convert and set parameter value
      APar.GetData(FBuffer.Ptr, AParIndex);
      oFmtOpts.ConvertRawData(AParInfo^.FSrcType, AParInfo^.FOutputType,
        FBuffer.Ptr, uiSrcDataLen, FBuffer.FBuffer, FBuffer.Size, uiDestDataLen,
        TFDPhysOracleConnection(FConnectionObj).FEnv.DataEncoder);

      case AParInfo^.FOOutputType of
      otCLOB,
      otNCLOB,
      otBLOB,
      otCFile,
      otBFile:
        begin
          oIntLoc := CreateLocator(AParInfo, AParInfo^.FOOutputType, AVarIndex);
          try
            if uiDestDataLen <> 0 then
              oIntLoc.Write(FBuffer.Ptr, uiDestDataLen, 1);
          finally
            FDFree(oIntLoc);
          end;
        end;
      otTimeStamp:
        begin
          oTS := CreateTimeStamp(AParInfo, AVarIndex);
          try
            oTS.SetAsSQLTimeStamp(PSQLTimeStamp(FBuffer.Ptr)^);
          finally
            FDFree(oTS);
          end;
        end;
      otIntervalYM,
      otIntervalDS:
        begin
          oTI := CreateTimeInterval(AParInfo, AVarIndex);
          try
            oTI.SetAsSQLTimeInterval(PFDSQLTimeInterval(FBuffer.Ptr)^);
          finally
            FDFree(oTI);
          end;
        end;
      else
        AParInfo^.FVar.SetData(AVarIndex, FBuffer.Ptr, uiDestDataLen);
      end;
    end;
  end;

begin
  oParams := GetParams;
  if oParams.Count = 0 then
    Exit;

  if oParams.Count <> Length(FParInfos) then
    ParSetChangedError(Length(FParInfos), oParams.Count);

  oFmtOpts := GetOptions.FormatOptions;
  oResOpts := FOptions.ResourceOptions;
  iArrSize := oParams.ArraySize;
  if iArrSize > oResOpts.ArrayDMLSize then
    iArrSize := oResOpts.ArrayDMLSize;

  for i := 0 to oParams.Count - 1 do begin
    oParam := oParams[i];
    pParInfo := @FParInfos[i];
    CheckParamMatching(oParam, pParInfo^.FDataType, pParInfo^.FParamType, 0);
    if pParInfo^.FVar <> nil then
      if not AIntoReturning then begin

        // check that parameter array size matches to bind variable array len
        if (AFromParIndex = -1) or (oParam.ArrayType <> atScalar) then
          if pParInfo^.FIsPLSQLTable then
            uiLen := ub4(oParam.ArraySize)
          else
            uiLen := ub4(iArrSize)
        else
          uiLen := 1;
        if pParInfo^.FVar.ArrayLen <> uiLen then begin
          pParInfo^.FVar.ArrayLen := uiLen;
          pParInfo^.FVar.Bind;
        end
        else if FBindVarsDirty then
          // if the sizes are the same and variables was modified, then init them
          if oParam.ArrayType <> atArray then
            pParInfo^.FVar.ResetBuffer(-1, -1)
          else
            pParInfo^.FVar.ResetBuffer(ATimes, AOffset);

        if pParInfo^.FVarType in [odUnknown, odIn, odInOut] then
          if oParam.ArrayType = atScalar then
            ProcessArrayItem(oParam, pParInfo, 0, -1)
          else if oParam.ArrayType = atPLSQLTable then begin
            if uiLen > 0 then
              for j := 0 to uiLen - 1 do
                ProcessArrayItem(oParam, pParInfo, j, j);
          end
          else if AFromParIndex = -1 then
            for j := AOffset to ATimes - 1 do
              ProcessArrayItem(oParam, pParInfo, j, j)
          else
            ProcessArrayItem(oParam, pParInfo, 0, AFromParIndex);
      end
      else
        if (pParInfo^.FVarType = odRet) and (pParInfo^.FOOutputType in otHBlobs) then
          ProcessArrayItem(oParam, pParInfo, 0, -1);
  end;
  if not AIntoReturning then
    FBindVarsDirty := True;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleCommand.GetParamValues(ATimes, AOffset: LongInt);
var
  i, j: Integer;
  oParams: TFDParams;
  oParam: TFDParam;
  pParInfo: PFDOraVarInfoRec;
  oFmtOpts: TFDFormatOptions;

  function CreateLocator(AParInfo: PFDOraVarInfoRec; AType: TOCIVarDataType;
    AIndex: ub4): TOCILobLocator;
  var
    hndl: pOCIHandle;
    uiTmp: ub4;
  begin
    AParInfo^.FVar.GetData(AIndex, @hndl, uiTmp);
    if AType in [otCLOB, otNCLOB, otBLOB] then begin
      Result := TOCIIntLocator.CreateUseHandle(
        TFDPhysOracleConnection(FConnectionObj).FService, hndl);
      Result.National := (AType = otNCLOB);
    end
    else begin
      Result := TOCIExtLocator.CreateUseHandle(
        TFDPhysOracleConnection(FConnectionObj).FService, hndl);
      if not Result.IsOpen then
        Result.Open(True);
    end;
  end;

  function CreateTimeStamp(AParInfo: PFDOraVarInfoRec; AIndex: ub4): TOCITimeStamp;
  var
    hndl: pOCIHandle;
    uiTmp: ub4;
  begin
    AParInfo^.FVar.GetData(AIndex, @hndl, uiTmp);
    Result := TOCITimestamp.CreateUseHandle(
      TFDPhysOracleConnection(FConnectionObj).FEnv, hndl);
  end;

  function CreateTimeInterval(AParInfo: PFDOraVarInfoRec; AIndex: ub4): TOCITimeInterval;
  var
    hndl: pOCIHandle;
    uiTmp: ub4;
  begin
    AParInfo^.FVar.GetData(AIndex, @hndl, uiTmp);
    Result := TOCITimeInterval.CreateUseHandle(
      TFDPhysOracleConnection(FConnectionObj).FEnv, hndl, AParInfo^.FOOutputType);
  end;

  procedure ProcessArrayItem(APar: TFDParam; AParInfo: PFDOraVarInfoRec; AVarIndex, AParIndex: Integer);
  var
    pData: pUb1;
    uiLen, uiByteLen: ub4;
    iDestDataLen: LongWord;
    oLoc: TOCILobLocator;
    oTS: TOCITimeStamp;
    oTI: TOCITimeInterval;
  begin
    // null
    pData := nil;
    uiLen := 0;
    if not AParInfo^.FVar.GetData(AVarIndex, pData, uiLen, True) then
      APar.Clear(AParIndex)

    // conversion is not required
    else if AParInfo^.FOutputType = AParInfo^.FSrcType then begin

      // byte string data, then optimizing - get data directly
      case AParInfo^.FOOutputType of
      otCLOB,
      otNCLOB,
      otBLOB,
      otCFile,
      otBFile:
        begin
          oLoc := CreateLocator(AParInfo, AParInfo^.FOOutputType, AVarIndex);
          try
            uiLen := oLoc.Length;
            pData := pUb1(APar.SetBlobRawData(uiLen, nil, AParIndex));
            if uiLen > 0 then
              uiLen := oLoc.Read(pData, uiLen, 1);
          finally
            FDFree(oLoc);
          end;
        end;
      otLongRaw:
        APar.SetBlobRawData(uiLen, PByte(pData), AParIndex);
      otString,
      otChar,
      otLong,
      otNChar,
      otNString,
      otNLong:
        APar.SetData(PByte(pData), uiLen, AParIndex);
      otTimeStamp:
        begin
          uiLen := SizeOf(TSQLTimeStamp);
          oTS := CreateTimeStamp(AParInfo, AVarIndex);
          try
            FBuffer.Check(uiLen);
            oTS.GetAsSQLTimeStamp(PSQLTimeStamp(FBuffer.Ptr)^);
            APar.SetData(FBuffer.Ptr, uiLen, AParIndex);
          finally
            FDFree(oTS);
          end;
        end;
      otIntervalYM,
      otIntervalDS:
        begin
          uiLen := SizeOf(TFDSQLTimeInterval);
          oTI := CreateTimeInterval(AParInfo, AVarIndex);
          try
            FBuffer.Check(uiLen);
            oTI.GetAsSQLTimeInterval(PFDSQLTimeInterval(FBuffer.Ptr)^);
            APar.SetData(FBuffer.Ptr, uiLen, AParIndex);
          finally
            FDFree(oTI);
          end;
        end;
      else
        FBuffer.Check(uiLen);
        AParInfo^.FVar.GetData(AVarIndex, FBuffer.Ptr, uiLen);
        APar.SetData(FBuffer.Ptr, uiLen, AParIndex);
      end;
    end

    // conversion is required
    else begin
      case AParInfo^.FOOutputType of
      otCLOB,
      otNCLOB,
      otBLOB,
      otCFile,
      otBFile:
        begin
          oLoc := CreateLocator(AParInfo, AParInfo^.FOOutputType, AVarIndex);
          try
            uiLen := oLoc.Length;
            uiByteLen := uiLen;
            if oLoc.National then
              uiByteLen := uiByteLen * SizeOf(WideChar);
            pData := pUb1(FBuffer.Check(uiByteLen));
            if uiLen > 0 then
              uiLen := oLoc.Read(pData, uiLen, 1);
          finally
            FDFree(oLoc);
          end;
        end;
      otTimeStamp:
        begin
          uiLen := SizeOf(TSQLTimeStamp);
          oTS := CreateTimeStamp(AParInfo, AVarIndex);
          try
            FBuffer.Check(uiLen);
            oTS.GetAsSQLTimeStamp(PSQLTimeStamp(FBuffer.Ptr)^);
          finally
            FDFree(oTS);
          end;
        end;
      otIntervalYM,
      otIntervalDS:
        begin
          uiLen := SizeOf(TFDSQLTimeInterval);
          oTI := CreateTimeInterval(AParInfo, AVarIndex);
          try
            FBuffer.Check(uiLen);
            oTI.GetAsSQLTimeInterval(PFDSQLTimeInterval(FBuffer.Ptr)^);
          finally
            FDFree(oTI);
          end;
        end;
      else
        FBuffer.Check(uiLen);
        AParInfo^.FVar.GetData(AVarIndex, FBuffer.Ptr, uiLen);
      end;

      iDestDataLen := 0;
      oFmtOpts.ConvertRawData(AParInfo^.FOutputType, AParInfo^.FSrcType,
        FBuffer.Ptr, uiLen, FBuffer.FBuffer, FBuffer.Size, iDestDataLen,
        TFDPhysOracleConnection(FConnectionObj).FEnv.DataEncoder);
      APar.SetData(FBuffer.Ptr, iDestDataLen, AParIndex);
    end;
  end;

begin
  oParams := GetParams;
  if oParams.Count = 0 then
    Exit;

  if oParams.Count <> Length(FParInfos) then
    ParSetChangedError(Length(FParInfos), oParams.Count);

  oFmtOpts := FOptions.FormatOptions;
  for i := 0 to oParams.Count - 1 do begin
    oParam := oParams[i];
    pParInfo := @FParInfos[i];
    CheckParamMatching(oParam, pParInfo^.FDataType, pParInfo^.FParamType, 0);
    if (pParInfo^.FVar <> nil) and
       (pParInfo^.FVarType in [odOut, odInOut, odRet]) and
       not (pParInfo^.FOOutputType in [otCursor, otNestedDataSet]) and
       (not (pParInfo^.FOOutputType in otHBlobs) or
            not (pParInfo^.FParamType in [ptUnknown, ptInput])) then
      if oParam.ArrayType <> atScalar then begin
        oParam.ArraySize := pParInfo^.FVar.ArrayLen;
        if pParInfo^.FVar.ArrayLen > 0 then
          for j := 0 to pParInfo^.FVar.ArrayLen - 1 do
            ProcessArrayItem(oParam, pParInfo, j, j);
      end
      else
        for j := AOffset to ATimes - 1 do
          ProcessArrayItem(oParam, pParInfo, j, j);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleCommand.RebindCursorParams;
var
  i, j, iCrsInfo: Integer;
  pParInfo: PFDOraVarInfoRec;
  pCrsInfo: PFDOraCrsDataRec;
  pColInfo: PFDOraVarInfoRec;
  hndl: pOCIHandle;
  uiTmp: ub4;
begin
  if FCrsInfos.Count = 0 then
    Exit;
  iCrsInfo := 0;
  for i := 0 to Length(FParInfos) - 1 do begin
    pParInfo := @FParInfos[i];
    if (pParInfo^.FOOutputType = otCursor) and (pParInfo^.FVar <> nil) then begin
      pParInfo^.FVar.GetData(0, @hndl, uiTmp);
      pCrsInfo := PFDOraCrsDataRec(FCrsInfos[iCrsInfo]);
      pCrsInfo^.FStmt.Handle := hndl;
      pCrsInfo^.FExecuted := False;
      DestroyDefineInfo(pCrsInfo);
      CreateDefineInfo(pCrsInfo);
      for j := 0 to Length(pCrsInfo^.FColInfos) - 1 do begin
        pColInfo := @pCrsInfo^.FColInfos[j];
        if pColInfo^.FVar <> nil then begin
          pColInfo^.FVar.BindOff;
          pColInfo^.FVar.BindTo(pCrsInfo^.FStmt);
        end;
      end;
      Inc(iCrsInfo);
    end;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleCommand.GetServerFeedback;
begin
  case FBase.FStmt.SQLFNCODE of
  // ALTER SESSION
  52:
    TFDPhysOracleConnection(FConnectionObj).UpdateCurrentSchema;
  // CREATE and ALTER PL/SQL objects
  24, 25, 91, 92, 94, 95, 97, 98, 59, 60, 77, 80, 81, 82:
    TFDPhysOracleConnection(FConnectionObj).GetPLSQLErrors;
  end;
  case FBase.FStmt.STMT_TYPE of
  OCI_STMT_UPDATE,
  OCI_STMT_DELETE,
  OCI_STMT_INSERT,
  OCI_STMT_BEGIN,
  OCI_STMT_DECLARE:
    TFDPhysOracleConnection(FConnectionObj).GetServerOutput;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleCommand.InternalExecute(ATimes, AOffset: LongInt; var ACount: TFDCounter);
var
  i: Integer;
  oFO: TFDFetchOptions;
begin
  ACount := 0;
  TFDPhysOracleConnection(FConnectionObj).UpdateServerOutput;
  oFO := FOptions.FetchOptions;
  if (hpLongData in FHasParams) and
     not (GetCommandKind in [skStoredProc, skStoredProcWithCrs,
                             skStoredProcNoCrs, skExecute]) then
    for i := AOffset to ATimes - 1 do begin
      if FHasParams <> [] then
        SetParamValues(1, 0, i, False);
      FBase.FStmt.Execute(1, 0, oFO.Mode = fmExactRecsMax,
        TFDPhysOracleTransaction(FTransactionObj).GetAutoCommit);
      Inc(ACount, FBase.FStmt.LastRowCount);
    end
  else begin
    if FHasParams <> [] then
      SetParamValues(ATimes, AOffset, -1, False);
    try
      FBase.FStmt.Execute(ATimes, AOffset, oFO.Mode = fmExactRecsMax,
        TFDPhysOracleTransaction(FTransactionObj).GetAutoCommit);
    finally
      if FBase.FStmt <> nil then
        ACount := FBase.FStmt.LastRowCount;
    end;
  end;
  FBase.FExecuted := True;
  if hpHBlobsRet in FHasParams then
    SetParamValues(ATimes, AOffset, -1, True);
  if hpOutput in FHasParams then
    GetParamValues(ATimes, AOffset);
  if hpCursors in FHasParams then
    RebindCursorParams;
  GetServerFeedback;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleCommand.InternalAbort;
begin
  TFDPhysOracleConnection(FConnectionObj).FService.Break(True);
end;

{-------------------------------------------------------------------------------}
function TFDPhysOracleCommand.GetRowsetSize(ACrsInfo: PFDOraCrsDataRec;
  ARowsetSize: LongWord): LongWord;
begin
  Result := ARowsetSize;
  if (hfNChar in ACrsInfo^.FHasFields) and ([hfLongData, hfBlob] * ACrsInfo^.FHasFields <> []) or
     (hfManyNChars in ACrsInfo^.FHasFields) then
    Result := 1;
end;

{-------------------------------------------------------------------------------}
function TFDPhysOracleCommand.InternalOpen: Boolean;
var
  oFO: TFDFetchOptions;
  pCrsInfo: PFDOraCrsDataRec;
  lMainCursor, lExactFetch: Boolean;
  iCount: TFDCounter;
  iRowsetSize, iPrefetchRows, iFetchRows: Integer;
begin
  if not GetNextRecordSet then
    FActiveCrs := 0;
  if not FCursorCanceled then begin
    Result := True;
    Exit;
  end;
  if not IsActiveCursorValid then begin
    iCount := 0;
    if FCrsInfos.Count = 0 then
      InternalExecute(GetParams.ArraySize, 0, iCount);
    Result := False;
    Exit;
  end;
  pCrsInfo := GetActiveCursor;
  oFO := FOptions.FetchOptions;
  iPrefetchRows := oFO.ActualRowsetSize;
  iRowsetSize := GetRowsetSize(pCrsInfo, iPrefetchRows);
  lMainCursor := (pCrsInfo = @FBase);
  lExactFetch := (oFO.Mode = fmExactRecsMax);
  if lMainCursor then begin
    iFetchRows := 0;
    if FHasParams <> [] then
      SetParamValues(1, 0, -1, False);
  end
  else begin
    iFetchRows := iRowsetSize;
    if not FBase.FExecuted then begin
      iCount := 0;
      InternalExecute(1, 0, iCount);
    end;
  end;
  ResetDefVars(pCrsInfo, iRowsetSize);
  try
    // When iPrefetchRows will lead to "strange" issues,
    // then change it back to iRowsetSize
    if lExactFetch then
      pCrsInfo^.FStmt.PREFETCH_ROWS := iPrefetchRows + 1
    else
      pCrsInfo^.FStmt.PREFETCH_ROWS := iPrefetchRows;
    pCrsInfo^.FStmt.Execute(iFetchRows, 0, False,
      TFDPhysOracleTransaction(FTransactionObj).GetAutoCommit);
    if lMainCursor then begin
      // If defines are not yet created, then create them here,
      // after a query was executed with iFetchRows = 0.
      if Length(FBase.FColInfos) = 0 then
        CreateDefineInfo(@FBase);
      if hpHBlobsRet in FHasParams then
        SetParamValues(1, 0, -1, True);
      if hpOutput in FHasParams then
        GetParamValues(1, 0);
      GetServerFeedback;
    end;
    FCursorCanceled := False;
    pCrsInfo^.FExecuted := not lMainCursor;
    Result := True;
  except
    on E: Exception do begin
      if lExactFetch and (E is EOCINativeException) and
         (EOCINativeException(E).Kind in [ekNoDataFound, ekTooManyRows]) then
        // FDQA checks FDCode = er_FD_AccExactFetchMismatch in fmExactRecsMax tests
        EOCINativeException(E).FDCode := er_FD_AccExactMismatch;
      FCursorCanceled := True;
      InternalClose;
      raise;
    end;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleCommand.InternalClose;
var
  pCrsInfo: PFDOraCrsDataRec;
begin
  if IsActiveCursorValid then begin
    pCrsInfo := GetActiveCursor;
    if not FCursorCanceled then begin
      FCursorCanceled := True;
      if pCrsInfo^.FExecuted then
        pCrsInfo^.FStmt.CancelCursor;
    end;
    pCrsInfo^.FExecuted := False;
  end;
  FInfoStack.Clear;
  FCurrentCrsInfo := nil;
  if not GetNextRecordSet then
    FBase.FExecuted := False;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysOracleCommand.ResetDefVars(ACrsInfo: PFDOraCrsDataRec;
  ARowsetSize: LongWord);
var
  i: Integer;
  pInfo: PFDOraVarInfoRec;
begin
  for i := 0 to Length(ACrsInfo^.FColInfos) - 1 do begin
    pInfo := @ACrsInfo^.FColInfos[i];
    if pInfo^.FVar <> nil then
      if (pInfo^.FVar.Handle = nil) or (pInfo^.FVar.ArrayLen <> ARowsetSize) then begin
        pInfo^.FVar.ArrayLen := ARowsetSize;
        pInfo^.FVar.Bind;
      end
      else if not ACrsInfo^.FExecuted then
        pInfo^.FVar.ResetBuffer(-1, -1);
  end;
end;

{-------------------------------------------------------------------------------}
function TFDPhysOracleCommand.ProcessRowSet(ACrsInfo: PFDOraCrsDataRec;
  ATable: TFDDatSTable; AParentRow: TFDDatSRow; ARowsetSize: LongWord): LongWord;
var
  i, j: Integer;
  oRow: TFDDatSRow;
  oFmt: TFDFormatOptions;
  lMetadata: Boolean;
  oCol: TFDDatSColumn;

  function CreateLocator(AColInfo: PFDOraVarInfoRec; AType: TOCIVarDataType;
    AIndex: ub4): TOCILobLocator;
  var
    hndl: pOCIHandle;
    uiTmp: ub4;
  begin
    AColInfo^.FVar.GetData(AIndex, @hndl, uiTmp);
    if AType in [otCLOB, otNCLOB, otBLOB] then begin
      Result := TOCIIntLocator.CreateUseHandle(
        TFDPhysOracleConnection(FConnectionObj).FService, hndl);
      Result.National := (AType = otNCLOB);
    end
    else begin
      Result := TOCIExtLocator.CreateUseHandle(
        TFDPhysOracleConnection(FConnectionObj).FService, hndl);
      if not Result.IsOpen then
        Result.Open(True);
    end;
  end;

  function CreateTimeStamp(AColInfo: PFDOraVarInfoRec; AIndex: ub4): TOCITimeStamp;
  var
    hndl: pOCIHandle;
    uiTmp: ub4;
  begin
    AColInfo^.FVar.GetData(AIndex, @hndl, uiTmp);
    Result := TOCITimestamp.CreateUseHandle(
      TFDPhysOracleConnection(FConnectionObj).FEnv, hndl);
  end;

  function CreateTimeInterval(AColInfo: PFDOraVarInfoRec; AIndex: ub4): TOCITimeInterval;
  var
    hndl: pOCIHandle;
    uiTmp: ub4;
  begin
    AColInfo^.FVar.GetData(AIndex, @hndl, uiTmp);
    Result := TOCITimeInterval.CreateUseHandle(
      TFDPhysOracleConnection(FConnectionObj).FEnv, hndl, AColInfo^.FOOutputType);
  end;

  procedure ProcessColumn(AColIndex: Integer; ARow: TFDDatSRow;
    AColInfo: PFDOraVarInfoRec; ARowIndex, ARowsetSize: LongWord);
  var
    pData: pUb1;
    uiLen, uiByteLen: ub4;
    iDestDataLen: LongWord;
    oLoc: TOCILobLocator;
    oTS: TOCITimeStamp;
    oTI: TOCITimeInterval;
    pCrsInfo: PFDOraCrsDataRec;
    oNestedTable: TFDDatSTable;
    i: Integer;
    iRowsetSize: LongWord;
    oVar: TOCIVariable;
  begin
    pData := nil;
    uiLen := 0;
    if (AColInfo^.FVar = nil) or not CheckFetchColumn(AColInfo^.FSrcType, AColInfo^.FAttrs) then
      Exit

    // null
    else if not AColInfo^.FVar.GetData(ARowIndex, pData, uiLen, True) then
      ARow.SetData(AColIndex, nil, 0)

    // nested dataset
    else if AColInfo^.FOOutputType = otNestedDataSet then begin
      pCrsInfo := AColInfo^.FCrsInfo;
      iRowsetSize := GetRowsetSize(pCrsInfo, ARowsetSize);

      pCrsInfo^.FStmt.AttachToHandle(ppOCIHandle(pData)^);
      for i := 0 to Length(pCrsInfo^.FColInfos) - 1 do begin
        oVar := pCrsInfo^.FColInfos[i].FVar;
        oVar.BindOff;
        oVar.ArrayLen := iRowsetSize;
        oVar.BindTo(pCrsInfo^.FStmt);
      end;
      pCrsInfo^.FExecuted := False;
      pCrsInfo^.FStmt.Execute(iRowsetSize, 0, False,
        TFDPhysOracleTransaction(FTransactionObj).GetAutoCommit);
      pCrsInfo^.FExecuted := True;

      oNestedTable := ARow.Table.Columns[AColIndex].NestedTable;
      while ProcessRowSet(pCrsInfo, oNestedTable, ARow, iRowsetSize) = iRowsetSize do
        ;
      ARow.Fetched[AColIndex] := True;
    end

    // conversion is not required
    else if AColInfo^.FOutputType = AColInfo^.FDestType then begin
      // byte string data, then optimizing - get data directly
      case AColInfo^.FOOutputType of
      otCLOB,
      otNCLOB,
      otBLOB,
      otCFile,
      otBFile:
        begin
          oLoc := CreateLocator(AColInfo, AColInfo^.FOOutputType, ARowIndex);
          try
            uiLen := oLoc.Length;
            pData := pUb1(ARow.BeginDirectWriteBlob(AColIndex, uiLen));
            try
              if uiLen > 0 then
                uiLen := oLoc.Read(pData, uiLen, 1);
            finally
              ARow.EndDirectWriteBlob(AColIndex, uiLen);
            end;
          finally
            FDFree(oLoc);
          end;
        end;
      otTimeStamp:
        begin
          oTS := CreateTimeStamp(AColInfo, ARowIndex);
          try
            if not oTS.IsNull then begin
              oTS.GetAsSQLTimeStamp(PSQLTimeStamp(FBuffer.Ptr)^);
              ARow.SetData(AColIndex, FBuffer.Ptr, SizeOf(TSQLTimeStamp));
            end;
          finally
            FDFree(oTS);
          end;
        end;
      otIntervalYM,
      otIntervalDS:
        begin
          oTI := CreateTimeInterval(AColInfo, ARowIndex);
          try
            if not oTI.IsNull then begin
              oTI.GetAsSQLTimeInterval(PFDSQLTimeInterval(FBuffer.Ptr)^);
              ARow.SetData(AColIndex, FBuffer.Ptr, SizeOf(TFDSQLTimeInterval));
            end;
          finally
            FDFree(oTI);
          end;
        end;
      otString,
      otNString,
      otChar,
      otNChar,
      otLong,
      otNLong,
      otLongRaw:
        ARow.SetData(AColIndex, pData, uiLen);
      else
        FBuffer.Check(uiLen);
        AColInfo^.FVar.GetData(ARowIndex, FBuffer.Ptr, uiLen);
        ARow.SetData(AColIndex, FBuffer.Ptr, uiLen);
      end;
    end

    // if conversion is required
    else begin
      case AColInfo^.FOOutputType of
      otCLOB,
      otBLOB,
      otCFile,
      otBFile:
        begin
          oLoc := CreateLocator(AColInfo, AColInfo^.FOOutputType, ARowIndex);
          try
            uiLen := oLoc.Length;
            uiByteLen := uiLen;
            if oLoc.National then
              uiByteLen := uiByteLen * SizeOf(WideChar);
            pData := pUb1(FBuffer.Check(uiByteLen));
            if uiLen > 0 then
              uiLen := oLoc.Read(pData, uiLen, 1);
          finally
            FDFree(oLoc);
          end;
        end;
      else
        FBuffer.Check(uiLen);
        AColInfo^.FVar.GetData(ARowIndex, FBuffer.Ptr, uiLen);
      end;
      iDestDataLen := 0;
      oFmt.ConvertRawData(AColInfo^.FOutputType, AColInfo^.FDestType,
        FBuffer.Ptr, uiLen, FBuffer.FBuffer, FBuffer.Size, iDestDataLen,
        TFDPhysOracleConnection(FConnectionObj).FEnv.DataEncoder);
      ARow.SetData(AColIndex, FBuffer.Ptr, iDestDataLen);
    end;
  end;

  procedure ProcessMetaColumn(AColIndex: Integer; ARow: TFDDatSRow;
    AColInfo: PFDOraVarInfoRec; ARowIndex: LongWord);
  var
    pData: pUb1;
    uiLen, iDestDataLen: ub4;
    oVar, oVar2: TOCIVariable;

    procedure RemoveDoubleQuotas;
    begin
      if AColInfo^.FOOutputType in [otString, otChar] then begin
        if PFDAnsiString(pData)^ = TFDAnsiChar('"') then begin
          pData := pUb1(PFDAnsiString(pData) + 1);
          Dec(uiLen, 2);
        end;
      end
      else begin
        if PWideChar(pData)^ = '"' then begin
          pData := pUb1(PWideChar(pData) + 1);
          Dec(uiLen, 2);
        end;
      end;
    end;

  begin
    pData := nil;
    uiLen := 0;
    oVar := AColInfo^.FVar;
    oVar2 := nil;
    // The mkIndexFields special case COLUMN_EXPRESSION vs COLUMN_NAME
    if (GetMetaInfoKind = mkIndexFields) and (AColIndex = 5) then begin
      oVar2 := ACrsInfo^.FColInfos[9].FVar;
      if oVar2.GetData(ARowIndex, pData, uiLen, True) then
        oVar := oVar2;
    end;

    if oVar = nil then
      Exit

    else if AColInfo^.FPos = 1 then
      ARow.SetData(0, ATable.Rows.Count + 1)

    else if not oVar.GetData(ARowIndex, pData, uiLen, True) then
      ARow.SetData(AColIndex, nil, 0)

    else if AColInfo^.FOOutputType in [otString, otNString, otChar, otNChar] then begin
      // The mkIndexFields special case COLUMN_EXPRESSION vs COLUMN_NAME
      if oVar = oVar2 then
        RemoveDoubleQuotas;
      if uiLen > ATable.Columns[AColIndex].Size then
        uiLen := ATable.Columns[AColIndex].Size;
      ARow.SetData(AColIndex, pData, uiLen);
    end

    else if AColInfo^.FOOutputType in [otLong, otNLong, otLongRaw] then
      ARow.SetData(AColIndex, pData, uiLen)

    else begin
      FBuffer.Check(uiLen);
      oVar.GetData(ARowIndex, FBuffer.Ptr, uiLen);
      iDestDataLen := 0;
      if AColInfo^.FDestType in [dtInt32, dtDouble, dtBCD, dtFmtBCD] then
        oFmt.ConvertRawData(AColInfo^.FOutputType, ATable.Columns[AColIndex].DataType,
          FBuffer.Ptr, uiLen, FBuffer.FBuffer, FBuffer.Size, iDestDataLen,
          TFDPhysOracleConnection(FConnectionObj).FEnv.DataEncoder);
      ARow.SetData(AColIndex, FBuffer.Ptr, iDestDataLen);
    end;
  end;

begin
  if not ACrsInfo^.FExecuted then begin
    ResetDefVars(ACrsInfo, ARowsetSize);
    ACrsInfo^.FStmt.Fetch(ARowsetSize);
  end
  else
    ACrsInfo^.FExecuted := False;
  oFmt := FOptions.FormatOptions;
  lMetadata := GetMetaInfoKind <> FireDAC.Phys.Intf.mkNone;
  for i := 0 to Integer(ACrsInfo^.FStmt.LastRowCount) - 1 do begin
    oRow := ATable.NewRow(False);
    try
      for j := 0 to ATable.Columns.Count - 1 do begin
        oCol := ATable.Columns[j];
        if oCol.SourceID > 0 then
          if lMetadata then
            ProcessMetaColumn(j, oRow, @ACrsInfo^.FColInfos[oCol.SourceID - 1], i)
          else
            ProcessColumn(j, oRow, @ACrsInfo^.FColInfos[oCol.SourceID - 1], i, ARowsetSize);
      end;
      if AParentRow <> nil then begin
        oRow.ParentRow := AParentRow;
        AParentRow.Fetched[ATable.Columns.ParentCol] := True;
      end;
      ATable.Rows.Add(oRow);
    except
      FDFree(oRow);
      raise;
    end;
  end;
  Result := ACrsInfo^.FStmt.LastRowCount;
end;

{-------------------------------------------------------------------------------}
function TFDPhysOracleCommand.InternalFetchRowSet(ATable: TFDDatSTable;
  AParentRow: TFDDatSRow; ARowsetSize: LongWord): LongWord;
var
  iRowsetSize, iRows: LongWord;
  pCrsInfo: PFDOraCrsDataRec;
begin
  Result := 0;
  if (GetMetaInfoKind <> mkNone) and not IsActiveCursorValid then
    Exit;
  pCrsInfo := GetActiveCursor;
  iRowsetSize := GetRowsetSize(pCrsInfo, ARowsetSize);
  while Result < ARowsetSize do begin
    iRows := ProcessRowSet(pCrsInfo, ATable, AParentRow, iRowsetSize);
    Inc(Result, iRows);
    if iRows <> iRowsetSize then
      Break;
  end;
end;

{-------------------------------------------------------------------------------}
function TFDPhysOracleCommand.InternalNextRecordSet: Boolean;
begin
  if not FBase.FExecuted or (FActiveCrs < 0) then begin
    Result := False;
    Exit;
  end;
  if FActiveCrs < FCrsInfos.Count then
    Inc(FActiveCrs);
  if FActiveCrs < FCrsInfos.Count then
    Result := InternalOpen
  else begin
    FBase.FExecuted := False;
    Result := False;
  end;
end;

{-----------------------------------------------------------------------------}
initialization
  FDPhysManager();
  FDPhysManagerObj.RegisterDriverClass(TFDPhysOracleDriver);

end.
