{*******************************************************}
{                                                       }
{               Delphi FireDAC Framework                }
{    FireDAC InterBase/Firebird driver base classes     }
{                                                       }
{ Copyright(c) 2004-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}
{$I FireDAC.inc}

unit FireDAC.Phys.IBBase;

interface

uses
  System.SysUtils, System.Classes, Data.DB,
  FireDAC.Stan.Intf, FireDAC.Stan.Error, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.Phys, FireDAC.Phys.SQLGenerator, FireDAC.Phys.IBCli, 
    FireDAC.Phys.IBWrapper, FireDAC.Phys.IBMeta;

type
  TFDPhysIBBaseDriverLink = class;

  TFDIBService = class;
  TFDIBBackup = class;
  TFDIBRestore = class;
  TFDIBValidate = class;
  TFDIBSecurity = class;
  TFDIBConfig = class;

  TFDPhysIBDriverBase = class;
  TFDPhysIBConnectionBase = class;
  TFDPhysIBTransactionBase = class;
  TFDPhysIBEventAlerterBase = class;
  TFDPhysIBCommandBase = class;

  TFDPhysIBCliHandles = array [0..1] of PPointer;
  PFDPhysIBCliHandles = ^TFDPhysIBCliHandles;

  TFDIBProtocol = (ipLocal, ipTCPIP, ipNetBEUI);

  TFDPhysIBBaseDriverLink = class(TFDPhysDriverLink)
  end;

  TFDIBService = class (TFDPhysDriverService)
  private
    FConnectTimeout: LongWord;
    FQueryTimeout: LongWord;
    FProtocol: TFDIBProtocol;
    FPassword: String;
    FSEPassword: String;
    FHost: String;
    FUserName: String;
    FOnProgress: TFDPhysServiceProgressEvent;
    FSqlRoleName: String;
    FInstanceName: String;
    function GetDriverLink: TFDPhysIBBaseDriverLink;
    procedure SetDriverLink(const AValue: TFDPhysIBBaseDriverLink);
    procedure SetHost(const AValue: String);
  protected
    // TFDPhysDriverService
    procedure InternalExecute; override;
    // introduced
    function CreateService(AEnv: TIBEnv): TIBService; virtual; abstract;
    procedure SetupService(AService: TIBService); virtual;
    procedure QueryService(AService: TIBService); virtual;
    procedure DeleteService(var AService: TIBService); virtual;
    procedure DoProgress(AService: TIBService; const AMessage: String); virtual;
    // other
    function GetConnection(const ADatabase: String): IFDPhysConnection;
    procedure ExecSQL(const ADatabase, ASQL: String);
  published
    property DriverLink: TFDPhysIBBaseDriverLink read GetDriverLink write SetDriverLink;
    property Protocol: TFDIBProtocol read FProtocol write FProtocol default ipLocal;
    property Host: String read FHost write SetHost;
    property InstanceName: String read FInstanceName write FInstanceName;
    property UserName: String read FUserName write FUserName;
    property Password: String read FPassword write FPassword;
    property SEPassword: String read FSEPassword write FSEPassword;
    property SqlRoleName: String read FSqlRoleName write FSqlRoleName;
    property ConnectTimeout: LongWord read FConnectTimeout write FConnectTimeout default 0;
    property QueryTimeout: LongWord read FQueryTimeout write FQueryTimeout default 0;
    property OnProgress: TFDPhysServiceProgressEvent read FOnProgress write FOnProgress;
  end;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or 
    pidiOSSimulator or pidiOSDevice or pidAndroid)]
  TFDIBBackup = class (TFDIBService)
  private
    FBackupFiles: TStrings;
    FVerbose: Boolean;
    FDatabase: String;
    FOptions: TIBBackupOptions;
    procedure SetBackupFiles(const AValue: TStrings);
  protected
    function CreateService(AEnv: TIBEnv): TIBService; override;
    procedure SetupService(AService: TIBService); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Backup;
  published
    property Database: String read FDatabase write FDatabase;
    property BackupFiles: TStrings read FBackupFiles write SetBackupFiles;
    property Verbose: Boolean read FVerbose write FVerbose default False;
    property Options: TIBBackupOptions read FOptions write FOptions default [];
  end;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or 
    pidiOSSimulator or pidiOSDevice or pidAndroid)]
  TFDIBRestore = class (TFDIBService)
  private
    FBackupFiles: TStrings;
    FVerbose: Boolean;
    FPageSize: LongWord;
    FDatabase: String;
    FOptions: TIBRestoreOptions;
    FPageBuffers: LongWord;
    FEUAPassword: String;
    FEUAUserName: String;
    FFixCharSet: String;
    procedure SetBackupFiles(const AValue: TStrings);
  protected
    function CreateService(AEnv: TIBEnv): TIBService; override;
    procedure SetupService(AService: TIBService); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Restore;
  published
    property Database: String read FDatabase write FDatabase;
    property BackupFiles: TStrings read FBackupFiles write SetBackupFiles;
    property Verbose: Boolean read FVerbose write FVerbose default False;
    property Options: TIBRestoreOptions read FOptions write FOptions default [];
    property PageSize: LongWord read FPageSize write FPageSize default 0;
    property PageBuffers: LongWord read FPageBuffers write FPageBuffers default 0;
    property FixCharSet: String read FFixCharSet write FFixCharSet;
    property EUAUserName: String read FEUAUserName write FEUAUserName;
    property EUAPassword: String read FEUAPassword write FEUAPassword;
  end;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or 
    pidiOSSimulator or pidiOSDevice or pidAndroid)]
  TFDIBValidate = class(TFDIBService)
  private
    FMode: TIBRepairMode;
    FDatabase: String;
    FOptions: TIBRepairOptions;
  protected
    function CreateService(AEnv: TIBEnv): TIBService; override;
    procedure SetupService(AService: TIBService); override;
  public
    procedure CheckOnly;
    procedure Repair;
    procedure Sweep;
    procedure Analyze(const ATable: String = ''; const AIndex: String = '');
  published
    property Database: String read FDatabase write FDatabase;
    property Options: TIBRepairOptions read FOptions write FOptions default [];
  end;

  TIBEncryptionType = (ecDES, ecAES);
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or 
    pidiOSSimulator or pidiOSDevice or pidAndroid)]
  TFDIBSecurity = class (TFDIBService)
  private
    FAction: TIBSecurityAction;
    FValues: array [Low(TIBSecurityValue) .. High(TIBSecurityValue)] of String;
    FModified: TIBSecurityValues;
    FUsers: TFDDatSTable;
    FEUADatabase: String;
    FKeyName: String;
    function GetInt(const AIndex: Integer): Integer;
    function GetStr(const AIndex: Integer): String;
    procedure SetInt(const AIndex, AValue: Integer);
    procedure SetStr(const AIndex: Integer; const AValue: String);
    function IsStored(const AIndex: Integer): Boolean;
    function GetEUAActive: Boolean;
    procedure SetEUAActive(const AValue: Boolean);
    function GetDBEncrypted: Boolean;
    procedure SetDBEncrypted(const AValue: Boolean);
  protected
    function CreateService(AEnv: TIBEnv): TIBService; override;
    procedure SetupService(AService: TIBService); override;
    procedure QueryService(AService: TIBService); override;
    procedure DeleteService(var AService: TIBService); override;
  public
    // Users management
    procedure AddUser;
    procedure DeleteUser;
    procedure ModifyUser;
    procedure DisplayUser;
    procedure DisplayUsers;
    property Users: TFDDatSTable read FUsers;
    // Interbase encryption
    procedure CreateSYSDSO(const APassword: String);
    procedure CreateSEPassword(const APassword: String; AExternal: Boolean);
    procedure CreateKey(ADefault: Boolean; AType: TIBEncryptionType;
      ALength: Integer; const APassword: String; ARandomVect, ARandomPad: Boolean;
      const ADesc: String);
    procedure DropKey(ACascade: Boolean);
    procedure GrantKey(const AUserName: String);
    procedure RevokeKey(const AUserName: String);
    procedure EncryptColumn(const ATableName, AColumnName: String);
    procedure DecryptColumn(const ATableName, AColumnName: String);

    procedure SetEncryption(const ADSOPassword, ASEPassword: String;
      AAlwaysUseSEP: Boolean; AType: TIBEncryptionType; ALength: Integer);
    procedure RemoveEncryption(const ADSOPassword: String);
    procedure ChangeEncryption(const ADSOPassword, ASEPassword,
      ANewSEPassword: String; ANewAlwaysUseSEP: Boolean);

    property EUAActive: Boolean read GetEUAActive write SetEUAActive;
    property DBEncrypted: Boolean read GetDBEncrypted write SetDBEncrypted;
  published
    property Modified: TIBSecurityValues read FModified write FModified default [];
    property AUserName: String index ord(svUserName) read GetStr write SetStr stored IsStored;
    property APassword: String index ord(svPassword) read GetStr write SetStr stored IsStored;
    property AFirstName: String index ord(svFirstName) read GetStr write SetStr stored IsStored;
    property AMiddleName: String index ord(svMiddleName) read GetStr write SetStr stored IsStored;
    property ALastName: String index ord(svLastName) read GetStr write SetStr stored IsStored;
    property AUserID: Integer index ord(svUserID) read GetInt write SetInt stored IsStored;
    property AGroupID: Integer index ord(svGroupID) read GetInt write SetInt stored IsStored;
    property AGroupName: String index ord(svGroupName) read GetStr write SetStr stored IsStored;
    property ARoleName: String index ord(svRoleName) read GetStr write SetStr stored IsStored;
    // Interbase encryption
    property EUADatabase: String read FEUADatabase write FEUADatabase;
    property KeyName: String read FKeyName write FKeyName;
  end;

  TFDIBConfigMode = (cmPageBuffers, cmSQLDialect, cmSweepInterval,
    cmReserveSpace, cmWriteMode, cmAccessMode, cmShutdown, cmOnline,
    cmActivateShadow);
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or 
    pidiOSSimulator or pidiOSDevice or pidAndroid)]
  TFDIBConfig = class (TFDIBService)
  private
    FDatabase: String;
    FMode: TFDIBConfigMode;
    FValue: LongWord;
    FShutdownMode: TIBShutdownMode;
  protected
    function CreateService(AEnv: TIBEnv): TIBService; override;
    procedure SetupService(AService: TIBService); override;
  public
    // properties
    procedure SetPageBuffers(const AValue: LongWord);
    procedure SetSQLDialect(const AValue: LongWord);
    procedure SetSweepInterval(const AValue: LongWord);
    procedure SetReserveSpace(const AValue: TIBReserveSpace);
    procedure SetWriteMode(const AValue: TIBWriteMode);
    procedure SetAccessMode(const AValue: TIBAccessMode);
    // actions
    procedure ShutdownDB(AMode: TIBShutdownMode; ATimeout: LongWord);
    procedure OnlineDB;
    procedure ActivateShadow;
  published
    property Database: String read FDatabase write FDatabase;
  end;

  TFDPhysIBDriverBase = class(TFDPhysDriver)
  protected
    FLib: TIBLib;
    procedure InternalUnload; override;
    function GetCliObj: Pointer; override;
    function GetConnParamCount(AKeys: TStrings): Integer; override;
    procedure GetConnParams(AKeys: TStrings; AIndex: Integer;
      var AName, AType, ADefVal, ACaption: String; var ALoginIndex: Integer); override;
  public
    constructor Create(AManager: TFDPhysManager; const ADriverDef: IFDStanDefinition); override;
    destructor Destroy; override;
  end;

  TFDPhysIBConnectionBase = class(TFDPhysConnection)
  private
    FEnv: TIBEnv;
    FDatabase: TIBDatabase;
    FServerVersion: TFDVersion;
    FServerBrand: TFDPhysIBBrand;
    FDialect: Word;
    FCliHandles: TFDPhysIBCliHandles;
    FSharedTxHandle: IscTrHandle;
    FMetaTransaction: IFDPhysTransaction;
    FExtendedMetadata: Boolean;
  protected
    procedure InternalConnect; override;
    procedure InternalDisconnect; override;
    procedure InternalPing; override;
    function InternalCreateTransaction: TFDPhysTransaction; override;
    function InternalCreateEvent(const AEventKind: String): TFDPhysEventAlerter; override;
    function InternalCreateMetaInfoCommand: TFDPhysCommand; override;
    function InternalCreateMetadata: TObject; override;
    function InternalCreateCommandGenerator(const ACommand: IFDPhysCommand): TFDPhysCommandGenerator; override;
{$IFDEF FireDAC_MONITOR}
    procedure InternalTracingChanged; override;
{$ENDIF}
    procedure InternalExecuteDirect(const ASQL: String; ATransaction: TFDPhysTransaction); override;
    procedure GetItem(AIndex: Integer; out AName: String;
      out AValue: Variant; out AKind: TFDMoniAdapterItemKind); override;
    function GetItemCount: Integer; override;
    function GetMessages: EFDDBEngineException; override;
    function GetCliObj: Pointer; override;
    function InternalGetCliHandle: Pointer; override;
    procedure InternalAnalyzeSession(AMessages: TStrings); override;
    procedure BuildIBConnectParams(AParams: TStrings;
      const AConnectionDef: IFDStanConnectionDef); virtual;
  public
    constructor Create(ADriverObj: TFDPhysDriver; AConnHost: TFDPhysConnectionHost); override;
    destructor Destroy; override;
    property IBEnv: TIBEnv read FEnv;
    property IBDatabase: TIBDatabase read FDatabase;
    property ServerVersion: TFDVersion read FServerVersion;
    property ServerBrand: TFDPhysIBBrand read FServerBrand;
  end;

  TFDPhysIBTransactionBase = class(TFDPhysTransaction)
  private
    FParams: TStrings;
    FTransaction: TIBTransaction;
    function GetIBConnection: TFDPhysIBConnectionBase;
  protected
    // IFDMoniAdapter
    function GetItemCount: Integer; override;
    procedure GetItem(AIndex: Integer; out AName: String; out AValue: Variant;
      out AKind: TFDMoniAdapterItemKind); override;
    // TFDPhysTransaction
    procedure InternalStartTransaction(AID: LongWord); override;
    procedure InternalCommit(AID: LongWord); override;
    procedure InternalRollback(AID: LongWord); override;
    procedure InternalChanged; override;
    procedure InternalAllocHandle; override;
    procedure InternalReleaseHandle; override;
    procedure InternalNotify(ANotification: TFDPhysTxNotification;
      ACommandObj: TFDPhysCommand); override;
    function GetCliObj: Pointer; override;
  public
    property IBConnection: TFDPhysIBConnectionBase read GetIBConnection;
    property IBTransaction: TIBTransaction read FTransaction;
  end;

  TFDPhysIBEventAlerterBase = class(TFDPhysEventAlerter)
  private
    FEvents: TIBEvents;
    FEventsConnection: IFDPhysConnection;
    procedure DoFired(AEvents: TIBEvents; ABaseIndex: Integer;
      const ACounts: TISCStatusVector);
  protected
    // TFDPhysEventAlerter
    procedure InternalAllocHandle; override;
    procedure InternalRegister; override;
    procedure InternalHandle(AEventMessage: TFDPhysEventMessage); override;
    procedure InternalUnregister; override;
    procedure InternalReleaseHandle; override;
    procedure InternalSignal(const AEvent: String; const AArgument: Variant); override;
  end;

  TFDIBColInfoRec = record
    FVar: TIBVariable;
    FName: String;
    FOriginTabName,
    FOriginColName: String;
    FPos: Integer;
    FLen,
    FPrec,
    FScale: SmallInt;
    FSrcSQLDataType,
    FSrcSQLSubType: SmallInt;
    FSrcDataType,
    FDestDataType: TFDDataType;
    FSrcTypeName: String;
    FAttrs: TFDDataAttributes;
    FADLen: LongWord;
    FInPK: Boolean;
  end;
  PFDIBColInfoRec = ^TFDIBColInfoRec;

  TFDIBParInfoRec = record
    FVar: TIBVariable;
    FParamIndex: Integer;
    FLen,
    FPrec,
    FScale: SmallInt;
    FDestSQLDataType,
    FDestSQLSubType: SmallInt;
    FDestDataType,
    FSrcDataType: TFDDataType;
    FADLen: LongWord;
    FSrcFieldType: TFieldType;
    FSrcSize: LongWord;
    FSrcPrec: Integer;
    FSrcScale: Integer;
    FParamType: TParamType;
  end;
  PFDIBParInfoRec = ^TFDIBParInfoRec;

  TFDPhysIBCommandBase = class(TFDPhysCommand)
  private
    FColumnIndex: Integer;
    FSPParsedName: TFDPhysParsedName;
    FCursorCanceled: Boolean;
    FMaxInputPos: Integer;
    FRetWithOut: Boolean;
    function GetConnection: TFDPhysIBConnectionBase; inline;
    procedure CreateColInfos;
    procedure DestroyColInfos;
    procedure CreateParamInfos(AVars: TIBVariables; AInput, AFB2Batch: Boolean);
    procedure DestroyParamInfos;
    procedure SQL2ADColInfo(ASQLDataType, ASQLSubType, ASQLLen, ASQLPrec,
      ASQLScale: Smallint; out AType: TFDDataType; var AAttrs: TFDDataAttributes;
      out ALen: LongWord; out APrec, AScale: Integer; AFmtOpts: TFDFormatOptions);
    procedure FetchRow(ATable: TFDDatSTable; AParentRow: TFDDatSRow);
    function UseExecDirect: Boolean;
    function IsReturning: Boolean;
    procedure CheckColInfos;
    procedure PrepareBase;
  protected
    FStmt: TIBStatement;
    FColInfos: array of TFDIBColInfoRec;
    FParInfos: array of TFDIBParInfoRec;
    // TFDPhysCommand
    procedure InternalAbort; override;
    procedure InternalClose; override;
    function InternalFetchRowSet(ATable: TFDDatSTable; AParentRow: TFDDatSRow;
      ARowsetSize: LongWord): LongWord; override;
    function InternalOpen: Boolean; override;
    function InternalNextRecordSet: Boolean; override;
    procedure InternalPrepare; override;
    function InternalUseStandardMetadata: Boolean; override;
    function InternalColInfoStart(var ATabInfo: TFDPhysDataTableInfo): Boolean; override;
    function InternalColInfoGet(var AColInfo: TFDPhysDataColumnInfo): Boolean; override;
    procedure InternalUnprepare; override;
    function GetCliObj: Pointer; override;
    function GetItemCount: Integer; override;
    procedure GetItem(AIndex: Integer; out AName: String;
      out AValue: Variant; out AKind: TFDMoniAdapterItemKind); override;
    // introduced
    procedure ProcessColumn(ATable: TFDDatSTable; AFmtOpts: TFDFormatOptions;
      AColIndex: Integer; ARow: TFDDatSRow; ApInfo: PFDIBColInfoRec; ARowIndex: Integer); virtual;
    procedure ProcessMetaColumn(ATable: TFDDatSTable; AFmtOpts: TFDFormatOptions;
      AColIndex: Integer; ARow: TFDDatSRow; ApInfo: PFDIBColInfoRec; ARowIndex: Integer); virtual;
    // other
    procedure SetupStatement(AStmt: TIBStatement);
    procedure CheckSPPrepared(ASPUsage: TFDPhysCommandKind);
    procedure CheckParamInfos;
    procedure DoExecute(ATimes, AOffset: Integer; var ACount: TFDCounter);
    procedure SetParamValue(AFmtOpts: TFDFormatOptions; AParam: TFDParam;
      AVar: TIBVariable; ApInfo: PFDIBParInfoRec; AVarIndex, AParIndex: Integer);
    procedure GetParamValue(AFmtOpts: TFDFormatOptions; AParam: TFDParam;
      AVar: TIBVariable; ApInfo: PFDIBParInfoRec; AVarIndex,
      AParIndex: Integer);
    procedure SetParamValues(ATimes, AOffset: Integer);
    procedure GetParamValues(ATimes, AOffset: Integer);
  public
    property IBConnection: TFDPhysIBConnectionBase read GetConnection;
    property IBStatement: TIBStatement read FStmt;
  end;

{-------------------------------------------------------------------------------}
implementation

uses
  System.Variants,
  FireDAC.Stan.Util, FireDAC.Stan.Consts;

const
  S_FD_UTF8 = 'UTF8';
  S_FD_UnicodeFSS = 'UNICODE_FSS';
  S_FD_CharacterSets = 'NONE;ASCII;BIG_5;CYRL;DOS437;DOS850;DOS852;DOS857;' +
    'DOS860;DOS861;DOS863;DOS865;EUCJ_0208;GB_2312;ISO8859_1;' +
    'ISO8859_2;KSC_5601;NEXT;OCTETS;SJIS_0208;' + S_FD_UnicodeFSS + ';' +
    'WIN1250;WIN1251;WIN1252;WIN1253;WIN1254;' +
    // FB 1.5
    'DOS737;DOS775;DOS858;DOS862;DOS864;DOS866;DOS869;' +
    'WIN1255;WIN1256;WIN1257;ISO8859_3;ISO8859_4;ISO8859_5;' +
    'ISO8859_6;ISO8859_7;ISO8859_8;ISO8859_9;ISO8859_13;' +
    // IB 7.1
    'ISO8859_15;' +
    // FB 2.0
    'KOI8R;KOI8U;' + S_FD_UTF8 + ';' +
    // IB 2007
    'UNICODE_LE;UNICODE_BE';
  S_FD_Local = 'Local';
  S_FD_NetBEUI = 'NetBEUI';
  S_FD_NovelSPX = 'SPX';
  S_FD_TCPIP = 'TCPIP';

{-------------------------------------------------------------------------------}
{ TFDIBService                                                                  }
{-------------------------------------------------------------------------------}
function TFDIBService.GetDriverLink: TFDPhysIBBaseDriverLink;
begin
  Result := inherited DriverLink as TFDPhysIBBaseDriverLink;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBService.SetDriverLink(const AValue: TFDPhysIBBaseDriverLink);
begin
  inherited DriverLink := AValue;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBService.SetHost(const AValue: String);
begin
  if FHost <> AValue then begin
    FHost := AValue;
    if (FHost <> '') and (Protocol = ipLocal) then
      Protocol := ipTCPIP;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBService.SetupService(AService: TIBService);
begin
  AService.Protocol := Integer(Protocol);
  AService.Host := Host;
  AService.InstanceName := InstanceName;
  AService.UserName := UserName;
  AService.Password := Password;
  AService.SEPassword := SEPassword;
  AService.SqlRoleName := SqlRoleName;
  AService.ConnectTimeout := ConnectTimeout;
  AService.QueryTimeout := QueryTimeout;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBService.DeleteService(var AService: TIBService);
begin
  FDFree(AService);
  AService := nil;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBService.DoProgress(AService: TIBService; const AMessage: String);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, AMessage);
end;

{-------------------------------------------------------------------------------}
procedure TFDIBService.QueryService(AService: TIBService);
var
  sMsg: String;
begin
  while not AService.QueryString(sMsg) do
    DoProgress(AService, sMsg);
end;

{-------------------------------------------------------------------------------}
procedure TFDIBService.InternalExecute;
var
  oEnv: TIBEnv;
  oSvc: TIBService;
begin
  oEnv := TIBEnv.Create(CliObj as TIBLib, Self);
  oSvc := CreateService(oEnv);
  try
    SetupService(oSvc);
    oSvc.Attach;
    try
      oSvc.Start;
      QueryService(oSvc);
    finally
      oSvc.Detach;
    end;
  finally
    DeleteService(oSvc);
    FDFree(oEnv);
  end;
end;

{-------------------------------------------------------------------------------}
function TFDIBService.GetConnection(const ADatabase: String): IFDPhysConnection;
const
  C_Prots: array [TFDIBProtocol] of String = (S_FD_Local, S_FD_TCPIP, S_FD_NetBEUI);
var
  sConnStr: String;

  procedure Add(const AParam, AValue: String);
  begin
    if AValue <> '' then begin
      if sConnStr <> '' then
        sConnStr := sConnStr + ';';
      sConnStr := sConnStr + AParam + '=' + AValue;
    end;
  end;

begin
  Add(S_FD_ConnParam_Common_DriverID, DriverLink.ActualDriverID);
  Add(S_FD_ConnParam_IB_Protocol, C_Prots[Protocol]);
  Add(S_FD_ConnParam_Common_Server, Host);
  Add(S_FD_ConnParam_IBS_InstanceName, InstanceName);
  Add(S_FD_ConnParam_Common_UserName, UserName);
  Add(S_FD_ConnParam_Common_Password, Password);
  Add(S_FD_ConnParam_IBS_SEPassword, SEPassword);
  Add(S_FD_ConnParam_Common_Database, FDExpandStr(ADatabase));
  Add(S_FD_ConnParam_IB_SQLDialect, '3');
  FDPhysManager.CreateConnection(sConnStr, Result);
  Result.ErrorHandler := Self as IFDStanErrorHandler;
  Result.Open;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBService.ExecSQL(const ADatabase, ASQL: String);
var
  oConn: IFDPhysConnection;
  oCmd: IFDPhysCommand;
begin
  oConn := GetConnection(ADatabase);
  oConn.CreateCommand(oCmd);
  oCmd.Options.ResourceOptions.CmdExecMode := amBlocking;
  oCmd.Options.ResourceOptions.PreprocessCmdText := False;
  oCmd.CommandText := ASQL;
  oCmd.Prepare;
  oCmd.Execute;
end;

{-------------------------------------------------------------------------------}
{ TFDIBBackup                                                                   }
{-------------------------------------------------------------------------------}
constructor TFDIBBackup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBackupFiles := TFDStringList.Create;
end;

{-------------------------------------------------------------------------------}
destructor TFDIBBackup.Destroy;
begin
  FDFreeAndNil(FBackupFiles);
  inherited Destroy;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBBackup.SetBackupFiles(const AValue: TStrings);
begin
  FBackupFiles.Assign(AValue);
end;

{-------------------------------------------------------------------------------}
function TFDIBBackup.CreateService(AEnv: TIBEnv): TIBService;
begin
  Result := TIBBackup.Create(AEnv, Self);
end;

{-------------------------------------------------------------------------------}
procedure TFDIBBackup.SetupService(AService: TIBService);
begin
  inherited SetupService(AService);
  TIBBackup(AService).DatabaseName := FDExpandStr(Database);
  TIBBackup(AService).BackupFiles := BackupFiles;
  FDExpandStrs(TIBBackup(AService).BackupFiles);
  TIBBackup(AService).Verbose := Verbose;
  TIBBackup(AService).Options := Options;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBBackup.Backup;
begin
  Execute;
end;

{-------------------------------------------------------------------------------}
{ TFDIBRestore                                                                  }
{-------------------------------------------------------------------------------}
constructor TFDIBRestore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBackupFiles := TFDStringList.Create;
end;

{-------------------------------------------------------------------------------}
destructor TFDIBRestore.Destroy;
begin
  FDFreeAndNil(FBackupFiles);
  inherited Destroy;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBRestore.SetBackupFiles(const AValue: TStrings);
begin
  FBackupFiles.Assign(AValue);
end;

{-------------------------------------------------------------------------------}
function TFDIBRestore.CreateService(AEnv: TIBEnv): TIBService;
begin
  Result := TIBRestore.Create(AEnv, Self);
end;

{-------------------------------------------------------------------------------}
procedure TFDIBRestore.SetupService(AService: TIBService);
begin
  inherited SetupService(AService);
  TIBRestore(AService).DatabaseName := FDExpandStr(Database);
  TIBRestore(AService).BackupFiles := BackupFiles;
  FDExpandStrs(TIBRestore(AService).BackupFiles);
  TIBRestore(AService).Verbose := Verbose;
  TIBRestore(AService).Options := Options;
  TIBRestore(AService).FixCharSet := FixCharSet;
  TIBRestore(AService).PageSize := PageSize;
  TIBRestore(AService).PageBuffers := PageBuffers;
  TIBRestore(AService).EUAUserName := EUAUserName;
  TIBRestore(AService).EUAPassword := EUAPassword;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBRestore.Restore;
begin
  Execute;
end;

{-------------------------------------------------------------------------------}
{ TFDIBValidate                                                                 }
{-------------------------------------------------------------------------------}
function TFDIBValidate.CreateService(AEnv: TIBEnv): TIBService;
begin
  Result := TIBRepair.Create(AEnv, Self);
end;

{-------------------------------------------------------------------------------}
procedure TFDIBValidate.SetupService(AService: TIBService);
begin
  inherited SetupService(AService);
  TIBRepair(AService).DatabaseName := FDExpandStr(Database);
  TIBRepair(AService).Mode := FMode;
  TIBRepair(AService).Options := Options;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBValidate.CheckOnly;
begin
  FMode := rmCheckOnly;
  Execute;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBValidate.Repair;
begin
  FMode := rmRepair;
  Execute;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBValidate.Sweep;
begin
  FMode := rmSweep;
  Execute;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBValidate.Analyze(const ATable, AIndex: String);
begin
  DoBeforeExecute;
  CheckActive(False, False);
  if AIndex <> '' then
    ExecSQL(Database, 'SET STATISTICS INDEX ' + AIndex)
  else if ATable <> '' then
    ExecSQL(Database, 'EXECUTE BLOCK AS' + C_FD_EOL +
      'DECLARE VARIABLE index_name VARCHAR(31);' + C_FD_EOL +
      'BEGIN' + C_FD_EOL +
      'FOR SELECT RDB$INDEX_NAME FROM RDB$INDICES INTO :index_name WHERE RDB$RELATION_NAME = ''' + ATable + ''' DO' + C_FD_EOL +
      'EXECUTE STATEMENT ''SET STATISTICS INDEX "'' || :index_name || ''";'';' + C_FD_EOL +
      'END')
  else
    ExecSQL(Database, 'EXECUTE BLOCK AS' + C_FD_EOL +
      'DECLARE VARIABLE index_name VARCHAR(31);' + C_FD_EOL +
      'BEGIN' + C_FD_EOL +
      'FOR SELECT RDB$INDEX_NAME FROM RDB$INDICES INTO :index_name DO' + C_FD_EOL +
      'EXECUTE STATEMENT ''SET STATISTICS INDEX "'' || :index_name || ''";'';' + C_FD_EOL +
      'END');
  DoAfterExecute;
end;

{-------------------------------------------------------------------------------}
{ TFDIBSecurity                                                                 }
{-------------------------------------------------------------------------------}
function TFDIBSecurity.GetInt(const AIndex: Integer): Integer;
begin
  Result := StrToIntDef(GetStr(AIndex), 0);
end;

{-------------------------------------------------------------------------------}
procedure TFDIBSecurity.SetInt(const AIndex, AValue: Integer);
begin
  SetStr(AIndex, IntToStr(AValue));
end;

{-------------------------------------------------------------------------------}
function TFDIBSecurity.GetStr(const AIndex: Integer): String;
begin
  Result := FValues[TIBSecurityValue(AIndex)];
end;

{-------------------------------------------------------------------------------}
procedure TFDIBSecurity.SetStr(const AIndex: Integer; const AValue: String);
begin
  Include(FModified, TIBSecurityValue(AIndex));
  FValues[TIBSecurityValue(AIndex)] := AValue;
end;

{-------------------------------------------------------------------------------}
function TFDIBSecurity.IsStored(const AIndex: Integer): Boolean;
begin
  Result := TIBSecurityValue(AIndex) in FModified;
end;

{-------------------------------------------------------------------------------}
function TFDIBSecurity.CreateService(AEnv: TIBEnv): TIBService;
begin
  Result := TIBSecurity.Create(AEnv, Self);
end;

{-------------------------------------------------------------------------------}
procedure TFDIBSecurity.SetupService(AService: TIBService);
var
  i: TIBSecurityValue;
begin
  inherited SetupService(AService);
  TIBSecurity(AService).Action := FAction;
  TIBSecurity(AService).EUADatabase := FDExpandStr(EUADatabase);
  TIBSecurity(AService).Modified := [];
  for i := Low(TIBSecurityValue) to High(TIBSecurityValue) do
    if i in Modified then
      TIBSecurity(AService).SetStr(Integer(i), FValues[i]);
end;

{-------------------------------------------------------------------------------}
procedure TFDIBSecurity.QueryService(AService: TIBService);
begin
  if TIBSecurity(AService).Action in [saDisplayUser, saDisplayUsers] then
    TIBSecurity(AService).QuerySecurity
  else
    inherited QueryService(AService);
end;

{-------------------------------------------------------------------------------}
procedure TFDIBSecurity.DeleteService(var AService: TIBService);
var
  i: TIBSecurityValue;
begin
  case TIBSecurity(AService).Action of
  saDisplayUser:
    begin
      for i := Low(TIBSecurityValue) to High(TIBSecurityValue) do
        SetStr(Integer(i), TIBSecurity(AService).GetStr(Integer(i)));
      Modified := [];
    end;
  saDisplayUsers:
    begin
      FUsers := TIBSecurity(AService).Users;
      if FUsers <> nil then
        FUsers.AddRef;
    end;
  end;
  inherited DeleteService(AService);
end;

{-------------------------------------------------------------------------------}
procedure TFDIBSecurity.AddUser;
begin
  FAction := saAddUser;
  Execute;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBSecurity.DeleteUser;
begin
  FAction := saDeleteUser;
  Execute;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBSecurity.ModifyUser;
begin
  FAction := saModifyUser;
  Execute;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBSecurity.DisplayUser;
begin
  FAction := saDisplayUser;
  Execute;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBSecurity.DisplayUsers;
begin
  FAction := saDisplayUsers;
  Execute;
end;

{-------------------------------------------------------------------------------}
function TFDIBSecurity.GetEUAActive: Boolean;
var
  oConn: IFDPhysConnection;
begin
  DoBeforeExecute;
  CheckActive(False, False);
  oConn := GetConnection(EUADatabase);
  Result := TIBDatabase(oConn.CliObj).db_eua_active <> 0;
  DoAfterExecute;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBSecurity.SetEUAActive(const AValue: Boolean);
var
  sSQL: String;
begin
  DoBeforeExecute;
  CheckActive(False, False);
  sSQL := 'ALTER DATABASE';
  if AValue then
    sSQL := sSQL + ' ADD ADMIN OPTION'
  else
    sSQL := sSQL + ' DROP ADMIN OPTION';
  ExecSQL(EUADatabase, sSQL);
  DoAfterExecute;
end;

{-------------------------------------------------------------------------------}
function TFDIBSecurity.GetDBEncrypted: Boolean;
var
  oConn: IFDPhysConnection;
begin
  oConn := GetConnection(EUADatabase);
  Result := TIBDatabase(oConn.CliObj).db_encrypted <> 0;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBSecurity.SetDBEncrypted(const AValue: Boolean);
var
  sSQL: String;
begin
  DoBeforeExecute;
  CheckActive(False, False);
  sSQL := 'ALTER DATABASE';
  if AValue then begin
    sSQL := sSQL + ' ENCRYPT';
    if KeyName <> '' then
      sSQL := sSQL + ' WITH ' + KeyName;
  end
  else
    sSQL := sSQL + ' DECRYPT';
  ExecSQL(EUADatabase, sSQL);
  DoAfterExecute;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBSecurity.CreateSYSDSO(const APassword: String);
var
  sSQL: String;
begin
  DoBeforeExecute;
  CheckActive(False, False);
  sSQL := 'CREATE USER SYSDSO';
  if APassword <> '' then
    sSQL := sSQL + ' SET PASSWORD ' + QuotedStr(APassword);
  ExecSQL(EUADatabase, sSQL);
  DoAfterExecute;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBSecurity.CreateSEPassword(const APassword: String; AExternal: Boolean);
var
  sSQL: String;
begin
  DoBeforeExecute;
  CheckActive(False, False);
  sSQL := 'ALTER DATABASE SET';
  if APassword = '' then
    sSQL := sSQL + ' NO SYSTEM ENCRYPTION PASSWORD'
  else begin
    sSQL := sSQL + ' SYSTEM ENCRYPTION PASSWORD ' + QuotedStr(APassword);
    if AExternal then
      sSQL := sSQL + ' EXTERNAL';
  end;
  ExecSQL(EUADatabase, sSQL);
  SEPassword := APassword;
  DoAfterExecute;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBSecurity.CreateKey(ADefault: Boolean; AType: TIBEncryptionType;
  ALength: Integer; const APassword: String; ARandomVect, ARandomPad: Boolean;
  const ADesc: String);
var
  sSQL: String;
begin
  DoBeforeExecute;
  CheckActive(False, False);
  sSQL := 'CREATE ENCRYPTION ' + KeyName;
  if ADefault then
    sSQL := sSQL + ' AS DEFAULT';
  if AType = ecDES then
    sSQL := sSQL + ' FOR DES'
  else
    sSQL := sSQL + ' FOR AES';
  if ALength <> 0 then
    sSQL := sSQL + ' WITH LENGTH ' + IntToStr(ALength);
  if APassword <> '' then
    sSQL := sSQL + ' PASSWORD ' + QuotedStr(APassword);
  if ARandomVect then
    sSQL := sSQL + ' INIT_VECTOR RANDOM';
  if ARandomPad then
    sSQL := sSQL + ' PAD RANDOM';
  if ADesc <> '' then
    sSQL := sSQL + ' DESCRIPTION ' + QuotedStr(ADesc);
  ExecSQL(EUADatabase, sSQL);
  DoAfterExecute;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBSecurity.DropKey(ACascade: Boolean);
var
  sSQL: String;
begin
  DoBeforeExecute;
  CheckActive(False, False);
  sSQL := 'DROP ENCRYPTION ' + KeyName;
  if ACascade then
    sSQL := sSQL + ' CASCADE';
  ExecSQL(EUADatabase, sSQL);
  DoAfterExecute;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBSecurity.GrantKey(const AUserName: String);
begin
  DoBeforeExecute;
  CheckActive(False, False);
  ExecSQL(EUADatabase, 'GRANT ENCRYPT ON ENCRYPTION ' + KeyName +
    ' TO ' + AUserName);
  DoAfterExecute;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBSecurity.RevokeKey(const AUserName: String);
begin
  DoBeforeExecute;
  CheckActive(False, False);
  ExecSQL(EUADatabase, 'REVOKE ENCRYPT ON ENCRYPTION ' + KeyName +
    ' FROM ' + AUserName);
  DoAfterExecute;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBSecurity.EncryptColumn(const ATableName, AColumnName: String);
begin
  DoBeforeExecute;
  CheckActive(False, False);
  ExecSQL(EUADatabase, 'ALTER TABLE ' + ATableName + ' ALTER COLUMN ' +
    AColumnName + ' ENCRYPT WITH ' + KeyName);
  DoAfterExecute;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBSecurity.DecryptColumn(const ATableName, AColumnName: String);
begin
  DoBeforeExecute;
  CheckActive(False, False);
  ExecSQL(EUADatabase, 'ALTER TABLE ' + ATableName + ' ALTER COLUMN ' +
    AColumnName + ' DECRYPT');
  DoAfterExecute;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBSecurity.SetEncryption(const ADSOPassword, ASEPassword: String;
  AAlwaysUseSEP: Boolean; AType: TIBEncryptionType; ALength: Integer);
var
  sPrevUser, sPrevPwd: String;
begin
  sPrevUser := UserName;
  sPrevPwd := Password;
  try
    // 1. Ensure that Embedded User Authentication (EUA) is enabled
    EUAActive := True;

    // 2. Create a System Database Security Owner (SYSDSO) account
    CreateSYSDSO(ADSOPassword);

    // -- login as SYSDSO
    UserName := 'sysdso';
    Password := ADSOPassword;

    // 3. Create a System Encryption Password (SEP)
    // and set SEPassword to ASEPassword
    CreateSEPassword(ASEPassword, AAlwaysUseSEP);

    // 4. Create an encryption key for the database
    CreateKey(True, AType, ALength, '', True, True, '');

    // 5. Grant the database owner privileges to use key
    GrantKey(sPrevUser);

  finally
    // -- login as SYSDBA
    UserName := sPrevUser;
    Password := sPrevPwd;
  end;

  // 6. Encrypt the database
  DBEncrypted := True;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBSecurity.RemoveEncryption(const ADSOPassword: String);
var
  sPrevUser, sPrevPwd: String;
begin
  // 1. Unencrypt the database
  DBEncrypted := False;

  sPrevUser := UserName;
  sPrevPwd := Password;
  try
    // -- login as SYSDSO
    UserName := 'sysdso';
    Password := ADSOPassword;

    // 2. Drop encryption key
    DropKey(True);

    // 3. Remove a System Encryption Password (SEP)
    CreateSEPassword('', False);

  finally
    // -- login as SYSDBA
    UserName := sPrevUser;
    Password := sPrevPwd;
  end;

  // 4. Drop a System Database Security Owner (SYSDSO) account
  ExecSQL(EUADatabase, 'DROP USER SYSDSO');

  // 5. Turn off Embedded User Authentication (EUA)
  EUAActive := False;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBSecurity.ChangeEncryption(const ADSOPassword, ASEPassword: String;
  const ANewSEPassword: String; ANewAlwaysUseSEP: Boolean);
var
  sPrevUser, sPrevPwd: String;
begin
  sPrevUser := UserName;
  sPrevPwd := Password;
  try
    // -- login as SYSDSO
    UserName := 'sysdso';
    Password := ADSOPassword;
    SEPassword := ASEPassword;

    // Create a System Encryption Password (SEP)
    // and set SEPassword to ASEPassword
    CreateSEPassword(ANewSEPassword, ANewAlwaysUseSEP);
  finally
    // -- login as SYSDBA
    UserName := sPrevUser;
    Password := sPrevPwd;
  end;
end;

{-------------------------------------------------------------------------------}
{ TFDIBConfig                                                                   }
{-------------------------------------------------------------------------------}
function TFDIBConfig.CreateService(AEnv: TIBEnv): TIBService;
begin
  Result := TIBConfig.Create(AEnv, Self);
end;

{-------------------------------------------------------------------------------}
procedure TFDIBConfig.SetupService(AService: TIBService);
begin
  inherited SetupService(AService);
  TIBConfig(AService).DatabaseName := FDExpandStr(Database);
  case FMode of
  cmPageBuffers:    TIBConfig(AService).SetPageBuffers(FValue);
  cmSQLDialect:     TIBConfig(AService).SetSQLDialect(FValue);
  cmSweepInterval:  TIBConfig(AService).SetSweepInterval(FValue);
  cmReserveSpace:   TIBConfig(AService).SetReserveSpace(TIBReserveSpace(FValue));
  cmWriteMode:      TIBConfig(AService).SetWriteMode(TIBWriteMode(FValue));
  cmAccessMode:     TIBConfig(AService).SetAccessMode(TIBAccessMode(FValue));
  cmShutdown:       TIBConfig(AService).ShutdownDB(FShutdownMode, FValue);
  cmOnline:         TIBConfig(AService).OnlineDB;
  cmActivateShadow: TIBConfig(AService).ActivateShadow;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBConfig.SetAccessMode(const AValue: TIBAccessMode);
begin
  FMode := cmAccessMode;
  FValue := LongWord(AValue);
  Execute;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBConfig.SetPageBuffers(const AValue: LongWord);
begin
  FMode := cmPageBuffers;
  FValue := AValue;
  Execute;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBConfig.SetReserveSpace(const AValue: TIBReserveSpace);
begin
  FMode := cmReserveSpace;
  FValue := LongWord(AValue);
  Execute;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBConfig.SetSQLDialect(const AValue: LongWord);
begin
  FMode := cmSQLDialect;
  FValue := AValue;
  Execute;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBConfig.SetSweepInterval(const AValue: LongWord);
begin
  FMode := cmSweepInterval;
  FValue := AValue;
  Execute;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBConfig.SetWriteMode(const AValue: TIBWriteMode);
begin
  FMode := cmWriteMode;
  FValue := LongWord(AValue);
  Execute;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBConfig.ShutdownDB(AMode: TIBShutdownMode; ATimeout: LongWord);
begin
  FMode := cmShutdown;
  FShutdownMode := AMode;
  FValue := ATimeout;
  Execute;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBConfig.OnlineDB;
begin
  FMode := cmOnline;
  Execute;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBConfig.ActivateShadow;
begin
  FMode := cmActivateShadow;
  Execute;
end;

{-------------------------------------------------------------------------------}
{ TFDPhysIBDriverBase                                                           }
{-------------------------------------------------------------------------------}
constructor TFDPhysIBDriverBase.Create(AManager: TFDPhysManager;
  const ADriverDef: IFDStanDefinition);
begin
  inherited Create(AManager, ADriverDef);
  FLib := TIBLib.Create(GetBaseDriverID, FDPhysManagerObj);
end;

{-------------------------------------------------------------------------------}
destructor TFDPhysIBDriverBase.Destroy;
begin
  inherited Destroy;
  FDFreeAndNil(FLib);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBDriverBase.InternalUnload;
begin
  FLib.Unload;
end;

{-------------------------------------------------------------------------------}
function TFDPhysIBDriverBase.GetCliObj: Pointer;
begin
  Result := FLib;
end;

{-------------------------------------------------------------------------------}
function TFDPhysIBDriverBase.GetConnParamCount(AKeys: TStrings): Integer;
begin
  Result := inherited GetConnParamCount(AKeys) + 10;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBDriverBase.GetConnParams(AKeys: TStrings; AIndex: Integer;
  var AName, AType, ADefVal, ACaption: String; var ALoginIndex: Integer);
begin
  ALoginIndex := -1;
  if AIndex < inherited GetConnParamCount(AKeys) then begin
    inherited GetConnParams(AKeys, AIndex, AName, AType, ADefVal, ACaption, ALoginIndex);
    if AName = S_FD_ConnParam_Common_Database then
      ALoginIndex := 2;
  end
  else begin
    case AIndex - inherited GetConnParamCount(AKeys) of
    0:
      begin
        AName := S_FD_ConnParam_Common_OSAuthent;
        AType := '@Y';
        ADefVal := '';
      end;
    1:
      begin
        AName := S_FD_ConnParam_IB_Protocol;
        AType := S_FD_Local + ';' + S_FD_NetBEUI + ';' + S_FD_NovelSPX + ';' + S_FD_TCPIP;
        ADefVal := S_FD_Local;
      end;
    2:
      begin
        AName := S_FD_ConnParam_Common_Server;
        AType := '@S';
        ADefVal := '';
      end;
    3:
      begin
        AName := S_FD_ConnParam_IB_SQLDialect;
        AType := '@I';
        ADefVal := '3';
      end;
    4:
      begin
        AName := S_FD_ConnParam_IB_RoleName;
        AType := '@S';
        ADefVal := '';
      end;
    5:
      begin
        AName := S_FD_ConnParam_Common_CharacterSet;
        AType := S_FD_CharacterSets;
        ADefVal := '';
      end;
    6:
      begin
        AName := S_FD_ConnParam_Common_ExtendedMetadata;
        AType := '@L';
        ADefVal := S_FD_False;
      end;
    7:
      begin
        AName := S_FD_ConnParam_IB_CreateDatabase;
        AType := '@Y';
        ADefVal := S_FD_No;
      end;
    8:
      begin
        AName := S_FD_ConnParam_IB_PageSize;
        AType := '1024;2048;4096;8192;16384';
        ADefVal := '4096';
      end;
    9:
      begin
        AName := S_FD_ConnParam_IB_Advanced;
        AType := '@S';
        ADefVal := '';
      end;
    end;
    ACaption := AName;
  end;
end;

{-------------------------------------------------------------------------------}
{ TFDPhysIBConnectionBase                                                       }
{-------------------------------------------------------------------------------}
constructor TFDPhysIBConnectionBase.Create(ADriverObj: TFDPhysDriver;
  AConnHost: TFDPhysConnectionHost);
var
  oOpts: TFDTxOptions;
begin
  FEnv := TIBEnv.Create(TFDPhysIBDriverBase(ADriverObj).FLib, Self);
  inherited Create(ADriverObj, AConnHost);
  CreateTransaction(FMetaTransaction);
  _Release;
  oOpts := FMetaTransaction.Options;
  oOpts.AutoCommit := True;
  oOpts.ReadOnly := True;
  oOpts.Isolation := xiReadCommitted;
end;

{-------------------------------------------------------------------------------}
destructor TFDPhysIBConnectionBase.Destroy;
begin
  FDHighRefCounter(FRefCount);
  ForceDisconnect;
  FMetaTransaction := nil;
  FDFreeAndNil(FEnv);
  inherited Destroy;
end;

{-------------------------------------------------------------------------------}
function TFDPhysIBConnectionBase.InternalCreateTransaction: TFDPhysTransaction;
begin
  Result := TFDPhysIBTransactionBase.Create(Self);
end;

{-------------------------------------------------------------------------------}
function TFDPhysIBConnectionBase.InternalCreateEvent(const AEventKind: String): TFDPhysEventAlerter;
begin
  if CompareText(AEventKind, S_FD_EventKind_IB_Events) = 0 then
    Result := TFDPhysIBEventAlerterBase.Create(Self, AEventKind)
  else
    Result := nil;
end;

{-------------------------------------------------------------------------------}
function TFDPhysIBConnectionBase.InternalCreateMetaInfoCommand: TFDPhysCommand;
begin
  Result := inherited InternalCreateMetaInfoCommand;
  TFDPhysIBCommandBase(Result).SetTransaction(FMetaTransaction);
end;

{-------------------------------------------------------------------------------}
function TFDPhysIBConnectionBase.InternalCreateCommandGenerator(
  const ACommand: IFDPhysCommand): TFDPhysCommandGenerator;
begin
  if ACommand <> nil then
    Result := TFDPhysIBCommandGenerator.Create(ACommand,
      FEnv.Lib.IDUTF8, FDatabase.Encoder.Encoding)
  else
    Result := TFDPhysIBCommandGenerator.Create(Self,
      FEnv.Lib.IDUTF8, FDatabase.Encoder.Encoding);
end;

{-------------------------------------------------------------------------------}
function TFDPhysIBConnectionBase.InternalCreateMetadata: TObject;
begin
  Result := TFDPhysIBMetadata.Create(Self, FEnv.Lib.Brand, ServerVersion,
    FEnv.Lib.Version, FDialect,
    (FDatabase <> nil) and (FDatabase.Encoder.Encoding in [ecUTF8, ecUTF16]));
end;

{$IFDEF FireDAC_MONITOR}
{-------------------------------------------------------------------------------}
procedure TFDPhysIBConnectionBase.InternalTracingChanged;
begin
  if FEnv <> nil then begin
    FEnv.Monitor := FMonitor;
    FEnv.Tracing := FTracing;
  end;
end;
{$ENDIF}

{-------------------------------------------------------------------------------}
procedure TFDPhysIBConnectionBase.BuildIBConnectParams(AParams: TStrings;
  const AConnectionDef: IFDStanConnectionDef);
var
  i: Integer;
  sAdv: String;
  sCharSet: String;
begin
  i := 1;
  sAdv := GetConnectionDef.AsString[S_FD_ConnParam_IB_Advanced];
  while i <= Length(sAdv) do
    AParams.Add(FDExtractFieldName(sAdv, i));
  if GetConnectionDef.HasValue(S_FD_ConnParam_Common_OSAuthent) and
     GetConnectionDef.GetOSAuthent then
    AParams.Add('trusted_auth')
  else begin
    if GetConnectionDef.HasValue(S_FD_ConnParam_Common_UserName) then
      AParams.Add('user_name=' + GetConnectionDef.UserName);
    if GetConnectionDef.HasValue(S_FD_ConnParam_Common_Password) then
      AParams.Add('password=' + GetConnectionDef.Password);
  end;
  if GetConnectionDef.HasValue(S_FD_ConnParam_IB_RoleName) then
    AParams.Add('sql_role_name=' + GetConnectionDef.AsString[S_FD_ConnParam_IB_RoleName]);
  sCharSet := GetConnectionDef.AsString[S_FD_ConnParam_Common_CharacterSet];
  if sCharSet <> '' then
    AParams.Add('lc_ctype=' + sCharSet);
  if GetConnectionDef.HasValue(S_FD_ConnParam_IB_PageSize) then
    AParams.Add('page_size=' + GetConnectionDef.AsString[S_FD_ConnParam_IB_PageSize]);
  AParams.Add('sql_dialect=' + IntToStr(FDialect));
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBConnectionBase.InternalConnect;
var
  oParams: TFDStringList;
  sCharSet: String;
  pCliHandles: PFDPhysIBCliHandles;
  i: Integer;
  oStrs: TStrings;
  sDB, sProt, sSrv: String;
begin
  if InternalGetSharedCliHandle() <> nil then begin
    pCliHandles := PFDPhysIBCliHandles(InternalGetSharedCliHandle());
    FDatabase := TIBDatabase.CreateUsingHandle(FEnv, pCliHandles^[0], Self);
    FSharedTxHandle := pCliHandles^[1];
  end
  else begin
    FDatabase := TIBDatabase.Create(FEnv, Self);
    FSharedTxHandle := nil;
  end;
{$IFDEF FireDAC_MONITOR}
  InternalTracingChanged;
{$ENDIF}

  sCharSet := GetConnectionDef.AsString[S_FD_ConnParam_Common_CharacterSet];
  if (CompareText(sCharSet, S_FD_UTF8) = 0) or (CompareText(sCharSet, S_FD_UnicodeFSS) = 0) then
    FDatabase.Encoder.Encoding := ecUTF8
  else
    FDatabase.Encoder.Encoding := ecANSI;
  if GetConnectionDef.HasValue(S_FD_ConnParam_Common_ExtendedMetadata) then
    FExtendedMetadata := GetConnectionDef.AsBoolean[S_FD_ConnParam_Common_ExtendedMetadata]
  else
    FExtendedMetadata := False;

  if GetConnectionDef.HasValue(S_FD_ConnParam_IB_SQLDialect) then
    FDialect := Word(GetConnectionDef.AsInteger[S_FD_ConnParam_IB_SQLDialect])
  else
    FDialect := 3;

  sDB := GetConnectionDef.ExpandedDatabase;
  sSrv := GetConnectionDef.Server;
  if GetConnectionDef.HasValue(S_FD_ConnParam_IB_Protocol) or (sSrv <> '') then begin
    sProt := GetConnectionDef.AsString[S_FD_ConnParam_IB_Protocol];
    if CompareText(sProt, S_FD_NetBEUI) = 0 then
      sDB := '\\' + sSrv + '\' + sDB
    else if CompareText(sProt, S_FD_NovelSPX) = 0 then
      sDB := sSrv + '@' + sDB
    else if (CompareText(sProt, S_FD_TCPIP) = 0) or (sSrv <> '') then
      sDB := sSrv + ':' + sDB
  end;

  if InternalGetSharedCliHandle() = nil then begin
    oParams := TFDStringList.Create;
    try
      oParams.CaseSensitive := False;
      BuildIBConnectParams(oParams, GetConnectionDef);
      if GetConnectionDef.AsYesNo[S_FD_ConnParam_IB_CreateDatabase] then
        FDatabase.CreateDatabase(sDB, oParams)
      else
        FDatabase.Attach(sDB, oParams);
    finally
      FDFree(oParams);
    end;
  end;

  if FEnv.Lib.Brand in [ibFirebird, ibYaffil] then
    oStrs := FDatabase.firebird_version
  else
    oStrs := FDatabase.isc_version;
  if oStrs.Count >= 2 then
    i := 1
  else
    i := 0;
  FServerVersion := FDVerStr2Int(oStrs[i]);
  if Pos('Firebird', oStrs[i]) > 0 then
    FServerBrand := ibFirebird
  else if (Pos('Interbase', oStrs[i]) > 0) or
          (Pos('WI-V', oStrs[i]) > 0) or
          (Pos('DE-V', oStrs[i]) > 0) then
    FServerBrand := ibInterbase
  else
    FServerBrand := ibYaffil;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBConnectionBase.InternalDisconnect;
begin
  if GetConnectionDef.AsYesNo[S_FD_ConnParam_IB_DropDatabase] then
    FDatabase.Drop;
  FDFreeAndNil(FDatabase);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBConnectionBase.InternalPing;
begin
  FDatabase.Ping;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBConnectionBase.InternalExecuteDirect(const ASQL: String;
  ATransaction: TFDPhysTransaction);
var
  oStmt: TIBStatement;
begin
  oStmt := TIBStatement.Create(FDatabase,
    TFDPhysIBTransactionBase(ATransaction).FTransaction, Self);
  try
    oStmt.InVars.VarCount := 0;
    oStmt.ExecuteImmediate(ASQL);
  finally
    FDFree(oStmt);
  end;
end;

{-------------------------------------------------------------------------------}
function TFDPhysIBConnectionBase.GetItemCount: Integer;
begin
  Result := inherited GetItemCount;
  if FEnv <> nil then begin
    Inc(Result, 3);
    if (FDatabase <> nil) and (FDatabase.FDBHandle <> nil) then
      Inc(Result, 1);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBConnectionBase.GetItem(AIndex: Integer; out AName: String;
  out AValue: Variant; out AKind: TFDMoniAdapterItemKind);
begin
  if AIndex < inherited GetItemCount then
    inherited GetItem(AIndex, AName, AValue, AKind)
  else
    case AIndex - inherited GetItemCount of
    0:
      begin
        AName := 'Brand';
        case FEnv.Lib.Brand of
        ibInterbase: AValue := 'InterBase';
        ibFirebird:  AValue := 'Firebird';
        ibYaffil:    AValue := 'Yaffil';
        end;
        AKind := ikClientInfo;
      end;
    1:
      begin
        AName := 'Client version';
        AValue := Integer(FEnv.Lib.Version);
        AKind := ikClientInfo;
      end;
    2:
      begin
        AName := 'Client DLL name';
        AValue := FEnv.Lib.DLLName;
        AKind := ikClientInfo;
      end;
    3:
      begin
        AName := 'Server version';
        if FEnv.Lib.Brand in [ibFirebird, ibYaffil] then
          AValue := FDFixCRLF(FDatabase.firebird_version.Text)
        else
          AValue := FDFixCRLF(FDatabase.isc_version.Text);
        AKind := ikSessionInfo;
      end;
    end;
end;

{-------------------------------------------------------------------------------}
function TFDPhysIBConnectionBase.GetMessages: EFDDBEngineException;
begin
  Result := nil;
end;

{-------------------------------------------------------------------------------}
function TFDPhysIBConnectionBase.GetCliObj: Pointer;
begin
  Result := FDatabase;
end;

{-------------------------------------------------------------------------------}
function TFDPhysIBConnectionBase.InternalGetCliHandle: Pointer;
begin
  if FDatabase <> nil then begin
    FCliHandles[0] := FDatabase.FDBHandle;
    FCliHandles[1] := TFDPhysIBTransactionBase(TransactionObj).FTransaction.FTRHandle;
    Result := @FCliHandles;
  end
  else
    Result := nil;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBConnectionBase.InternalAnalyzeSession(AMessages: TStrings);
begin
  inherited InternalAnalyzeSession(AMessages);

  // 2. Use FB client for FB server, and IB client for IB server
  if (ServerBrand = ibFirebird) and (FEnv.Lib.Brand = ibInterbase) then
    AMessages.Add('Warning: For Firebird use the fbclient.dll')
  else if (ServerBrand = ibInterbase) and (FEnv.Lib.Brand = ibFirebird) then
    AMessages.Add('Warning: For InterBase use the gds32.dll');
end;

{-------------------------------------------------------------------------------}
{ TFDPhysIBTransactionBase                                                      }
{-------------------------------------------------------------------------------}
function TFDPhysIBTransactionBase.GetIBConnection: TFDPhysIBConnectionBase;
begin
  Result := TFDPhysIBConnectionBase(ConnectionObj);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBTransactionBase.InternalAllocHandle;
begin
  FParams := TFDStringList.Create;
  if IBConnection.FSharedTxHandle <> nil then begin
    FTransaction := TIBTransaction.CreateUsingHandle(IBConnection.FEnv,
      IBConnection.FSharedTxHandle, Self);
    IBConnection.FSharedTxHandle := nil;
  end
  else
    FTransaction := TIBTransaction.Create(IBConnection.FEnv, Self);
  FTransaction.AddDatabase(IBConnection.FDatabase);
  InternalChanged;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBTransactionBase.InternalReleaseHandle;
begin
  FDFreeAndNil(FParams);
  if FTransaction <> nil then begin
    FTransaction.RemoveDatabase(IBConnection.FDatabase);
    FDFreeAndNil(FTransaction);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBTransactionBase.InternalChanged;
var
  i, j: Integer;
  oCustom, oParams: TStrings;
begin
  oParams := FTransaction.Params;
  oParams.BeginUpdate;
  try
    oParams.Clear;
    oCustom := GetOptions.Params;
    if (oCustom.IndexOf('read') = -1) and (oCustom.IndexOf('write') = -1) then
      if GetOptions.ReadOnly then
        oParams.Add('read')
      else
        oParams.Add('write');
    if (oCustom.IndexOf('read_committed') = -1) and (oCustom.IndexOf('rec_version') = -1) and
       (oCustom.IndexOf('concurrency') = -1) and (oCustom.IndexOf('consistency') = -1) then
      case GetOptions.Isolation of
      xiUnspecified:
        ;
      xiDirtyRead,
      xiReadCommitted:
        begin
          oParams.Add('read_committed');
          oParams.Add('rec_version');
        end;
      xiRepeatableRead,
      xiSnapshot:
        oParams.Add('concurrency');
      xiSerializible:
        oParams.Add('consistency');
      end;
    if (oCustom.IndexOf('wait') = -1) and (oCustom.IndexOf('nowait') = -1) then
      if IBConnection.GetOptions.UpdateOptions.LockWait then
        oParams.Add('wait')
      else
        oParams.Add('nowait');
    for i := 0 to oCustom.Count - 1 do begin
      j := oParams.IndexOfName(FDNameFromIndex(oCustom, i));
      if j = -1 then
        j := oParams.IndexOf(oCustom[i]);
      if j = -1 then
        oParams.Add(oCustom[i])
      else
        oParams.Strings[j] := oCustom[i];
    end;
  finally
    oParams.EndUpdate;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBTransactionBase.InternalStartTransaction(AID: LongWord);
begin
  FTransaction.StartTransaction();
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBTransactionBase.InternalCommit(AID: LongWord);
begin
  if Retaining then
    FTransaction.CommitRetaining()
  else begin
    DisconnectCommands(nil, dmOffline);
    FTransaction.Commit();
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBTransactionBase.InternalRollback(AID: LongWord);
begin
  if Retaining then
    FTransaction.RollbackRetaining()
  else begin
    DisconnectCommands(nil, dmOffline);
    FTransaction.Rollback();
  end;
end;

{-------------------------------------------------------------------------------}
function TFDPhysIBTransactionBase.GetCliObj: Pointer;
begin
  Result := FTransaction;
end;

{-------------------------------------------------------------------------------}
function DisconnectionBeforeStart(ACmdObj: TObject): Boolean;
var
  oCmd: TFDPhysIBCommandBase;
begin
  oCmd := TFDPhysIBCommandBase(TIBStatement(ACmdObj).OwningObj);
  Result := not ((oCmd.GetCommandKind in [skCommit, skRollback]) and
    (oCmd.GetState = csExecuting));
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBTransactionBase.InternalNotify(ANotification: TFDPhysTxNotification;
  ACommandObj: TFDPhysCommand);
begin
  if ANotification = cpAfterCmdPrepareSuccess then
    Exit;
  ConnectionObj.Lock;
  try
    case ANotification of
    cpBeforeCmdPrepare:
      if not (TFDPhysIBCommandBase(ACommandObj).GetCommandKind in [skStartTransaction, skCommit, skRollback]) then
        if GetOptions.AutoStart and not GetActive then begin
          StartTransaction;
          FIDAutoCommit := GetSerialID;
        end;
    cpBeforeCmdExecute:
      if TFDPhysIBCommandBase(ACommandObj).GetCommandKind in [skCommit, skRollback] then
        DisconnectCommands(DisconnectionBeforeStart, dmOffline);
    cpAfterCmdPrepareFailure,
    cpAfterCmdUnprepare,
    cpAfterCmdExecuteSuccess,
    cpAfterCmdExecuteFailure:
      if not (TFDPhysIBCommandBase(ACommandObj).GetCommandKind in [skStartTransaction, skCommit, skRollback]) then begin
        if GetOptions.AutoStop and GetActive and
           (not (xoIfAutoStarted in GetOptions.StopOptions) or
            (FIDAutoCommit <> 0) and (FIDAutoCommit = GetSerialID)) then
          // allowunprepare, force, success
          CheckStoping(not (ANotification in [cpAfterCmdUnprepare, cpAfterCmdPrepareFailure]),
                       not (xoIfCmdsInactive in GetOptions.StopOptions),
                       not (ANotification in [cpAfterCmdPrepareFailure, cpAfterCmdExecuteFailure]),
                       ACommandObj);
      end
      else if ANotification = cpAfterCmdExecuteSuccess then
        InternalCheckState(ACommandObj, True);
    end;
  finally
    ConnectionObj.UnLock;
  end;
end;

{-------------------------------------------------------------------------------}
function TFDPhysIBTransactionBase.GetItemCount: Integer;
begin
  Result := inherited GetItemCount;
  if FTransaction <> nil then
    Inc(Result);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBTransactionBase.GetItem(AIndex: Integer; out AName: String;
  out AValue: Variant; out AKind: TFDMoniAdapterItemKind);
begin
  if AIndex < inherited GetItemCount then
    inherited GetItem(AIndex, AName, AValue, AKind)
  else
    case AIndex - inherited GetItemCount of
    0:
      begin
        AName := '@Params';
        AValue := FTransaction.Params.Text;
        AKind := ikSQL;
      end;
    end;
end;

{-------------------------------------------------------------------------------}
{ TFDPhysIBEventMessage                                                         }
{-------------------------------------------------------------------------------}
type
  TFDPhysIBEventMessage = class(TFDPhysEventMessage)
  private
    FBaseIndex: Integer;
    FCounts: TISCStatusVector;
  public
    constructor Create(ABaseIndex: Integer; const ACounts: TISCStatusVector);
  end;

{-------------------------------------------------------------------------------}
constructor TFDPhysIBEventMessage.Create(ABaseIndex: Integer;
  const ACounts: TISCStatusVector);
begin
  inherited Create;
  FBaseIndex := ABaseIndex;
  FCounts := ACounts;
end;

{-------------------------------------------------------------------------------}
{ TFDPhysIBEventAlerterBase                                                     }
{-------------------------------------------------------------------------------}
procedure TFDPhysIBEventAlerterBase.InternalAllocHandle;
begin
  FEventsConnection := GetConnection.Clone;
  if FEventsConnection.State = csDisconnected then
    FEventsConnection.Open;
  FEvents := TIBEvents.Create(TIBDatabase(FEventsConnection.CliObj), Self);
  FEvents.Names.Assign(GetNames);
  FEvents.OnFired := DoFired;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBEventAlerterBase.DoFired(AEvents: TIBEvents; ABaseIndex: Integer;
  const ACounts: TISCStatusVector);
begin
  if IsRunning then
    FMsgThread.EnqueueMsg(TFDPhysIBEventMessage.Create(ABaseIndex, ACounts));
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBEventAlerterBase.InternalHandle(AEventMessage: TFDPhysEventMessage);
var
  i, iFrom, iTo: Integer;
begin
  iFrom := TFDPhysIBEventMessage(AEventMessage).FBaseIndex;
  iTo := iFrom + 15 - 1;
  if iTo >= GetNames.Count then
    iTo := GetNames.Count - 1;
  for i := iFrom to iTo do begin
    if GetHandler = nil then
      Break;
    if TFDPhysIBEventMessage(AEventMessage).FCounts[i - iFrom] <> 0 then
      GetHandler.HandleEvent(GetNames[i],
        TFDPhysIBEventMessage(AEventMessage).FCounts[i - iFrom]);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBEventAlerterBase.InternalRegister;
begin
  FEvents.Start;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBEventAlerterBase.InternalUnregister;
begin
  FEvents.Stop;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBEventAlerterBase.InternalReleaseHandle;
begin
  FDFreeAndNil(FEvents);
  FEventsConnection := nil;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBEventAlerterBase.InternalSignal(const AEvent: String;
  const AArgument: Variant);
var
  oCmd: IFDPhysCommand;
begin
  if TFDPhysIBConnectionBase(ConnectionObj).FEnv.Lib.Brand <> ibFirebird then
    FDCapabilityNotSupported(Self, [S_FD_LPhys, ConnectionObj.DriverID]);
  GetConnection.CreateCommand(oCmd);
  SetupCommand(oCmd);
  oCmd.Prepare('EXECUTE BLOCK AS BEGIN POST_EVENT ' + QuotedStr(AEvent) + '; END');
  oCmd.Execute();
end;

{-------------------------------------------------------------------------------}
{ TFDPhysIBCommandBase                                                          }
{-------------------------------------------------------------------------------}
function TFDPhysIBCommandBase.GetConnection: TFDPhysIBConnectionBase;
begin
  Result := TFDPhysIBConnectionBase(FConnectionObj);
end;

{-------------------------------------------------------------------------------}
function TFDPhysIBCommandBase.GetCliObj: Pointer;
begin
  Result := IBStatement;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBCommandBase.SetupStatement(AStmt: TIBStatement);
var
  oFmtOpts: TFDFormatOptions;
begin
  oFmtOpts := FOptions.FormatOptions;
  if GetMetaInfoKind <> mkNone then begin
    AStmt.StrsTrim := True;
    AStmt.StrsEmpty2Null := True;
  end
  else begin
    AStmt.StrsTrim := oFmtOpts.StrsTrim;
    AStmt.StrsEmpty2Null := oFmtOpts.StrsEmpty2Null;
  end;
  AStmt.Dialect := IBConnection.FDialect;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBCommandBase.SQL2ADColInfo(ASQLDataType, ASQLSubType, ASQLLen,
  ASQLPrec, ASQLScale: Smallint; out AType: TFDDataType; var AAttrs: TFDDataAttributes;
  out ALen: LongWord; out APrec, AScale: Integer; AFmtOpts: TFDFormatOptions);
begin
  AType := dtUnknown;
  ALen := 0;
  APrec := 0;
  AScale := 0;
  Exclude(AAttrs, caFixedLen);
  Exclude(AAttrs, caBlobData);
  Include(AAttrs, caSearchable);
  case ASQLDataType of
  SQL_TEXT,
  SQL_VARYING,
  SQL_NULL:
    if LongWord(ASQLLen) <= AFmtOpts.MaxStringSize then begin
      if ASQLSubType = csIDOctets then begin
        AType := dtByteString;
        ALen := ASQLLen;
      end
      else if IBConnection.FEnv.Lib.IsUTF8(ASQLSubType) then begin
        AType := dtWideString;
        // this is workaround for InterBase behaviour, where for SELECT item:
        //    select ('abc' || 'de') from "Region" where RegionID=1
        // SQLSubType = 59, but SQLLen = 5 instead of 5 * 4
        if IBConnection.FEnv.Lib.Brand = ibFirebird then
          ALen := ASQLLen div IBConnection.FEnv.Lib.GetBytesPerChar(ASQLSubType)
        else
          ALen := ASQLLen;
        if (ALen = 0) and (ASQLLen > 0) then
          ALen := 1;
      end
      else begin
        AType := dtAnsiString;
        ALen := ASQLLen div SizeOf(TFDAnsiChar);
      end;
      if (ASQLDataType = SQL_TEXT) or (ASQLDataType = SQL_NULL) then
        Include(AAttrs, caFixedLen);
    end
    else begin
      if ASQLSubType = csIDOctets then
        AType := dtBlob
      else if IBConnection.FDatabase.Encoder.Encoding = ecUTF8 then
        AType := dtWideMemo
      else
        AType := dtMemo;
      Include(AAttrs, caBlobData);
    end;
  SQL_SHORT:
    if ASQLScale = 0 then
      AType := dtInt16
    else begin
      APrec := 4;
      AScale := -ASQLScale;
      if AScale <= 4 then
        AType := dtCurrency
      else if (APrec <= AFmtOpts.MaxBcdPrecision) and
              (AScale <= AFmtOpts.MaxBcdScale) then
        AType := dtBCD
      else
        AType := dtFmtBCD;
    end;
  SQL_LONG:
    if ASQLScale = 0 then
      AType := dtInt32
    else begin
      APrec := 9;
      AScale := -ASQLScale;
      if AScale <= 4 then
        AType := dtCurrency
      else if (APrec <= AFmtOpts.MaxBcdPrecision) and
              (AScale <= AFmtOpts.MaxBcdScale) then
        AType := dtBCD
      else
        AType := dtFmtBCD;
    end;
  SQL_INT64,
  SQL_QUAD:
    if ASQLScale = 0 then
      AType := dtInt64
    else begin
      APrec := 18;
      AScale := -ASQLScale;
      if (APrec <= AFmtOpts.MaxBcdPrecision) and
         (AScale <= AFmtOpts.MaxBcdScale) then
        AType := dtBCD
      else
        AType := dtFmtBCD;
    end;
  SQL_FLOAT:
    begin
      AType := dtSingle;
      APrec := 8;
    end;
  SQL_DOUBLE,
  SQL_D_FLOAT:
    begin
      AType := dtDouble;
      APrec := 16;
    end;
  SQL_TIMESTAMP:
    AType := dtDateTimeStamp;
  SQL_TYPE_TIME:
    AType := dtTime;
  SQL_TYPE_DATE:
    AType := dtDate;
  SQL_BOOLEAN_IB,
  SQL_BOOLEAN_FB:
    AType := dtBoolean;
  SQL_BLOB:
    begin
      if ASQLSubType = isc_blob_text then
        if IBConnection.FDatabase.Encoder.Encoding = ecUTF8 then
          AType := dtWideMemo
        else
          AType := dtMemo
      else
        AType := dtBlob;
      Include(AAttrs, caBlobData);
      Exclude(AAttrs, caSearchable);
    end;
  SQL_ARRAY:
    begin
      AType := dtArrayRef;
      Exclude(AAttrs, caSearchable);
    end;
  else
    AType := dtUnknown;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBCommandBase.CreateColInfos;
var
  i, j, k: Integer;
  oVar: TIBVariable;
  oFmtOpts: TFDFormatOptions;
  oFetchOpts: TFDFetchOptions;
  iPrec, iScale: Integer;
  oTabs: TFDStringList;
  sTableNames: String;
  lGetDSInfo: Boolean;
  oConnMeta: IFDPhysConnectionMetadata;
  oView: TFDDatSView;
  pColInfo: PFDIBColInfoRec;
  oRow: TFDDatSRow;
begin
  oFmtOpts := FOptions.FormatOptions;
  oFetchOpts := FOptions.FetchOptions;
  sTableNames := '';
  oTabs := nil;
  lGetDSInfo := (GetMetaInfoKind = mkNone) and (fiMeta in oFetchOpts.Items) and
    IBConnection.FExtendedMetadata;

  SetLength(FColInfos, FStmt.OutVars.VarCount);
  for i := 0 to Length(FColInfos) - 1 do begin
    oVar := FStmt.OutVars[i];

    pColInfo := @FColInfos[i];
    pColInfo^.FVar := oVar;
    pColInfo^.FName := oVar.aliasname;
    pColInfo^.FOriginTabName := oVar.relname;
    if pColInfo^.FOriginTabName <> '' then
      pColInfo^.FOriginColName := oVar.sqlname;
    pColInfo^.FPos := i + 1;

    if lGetDSInfo and (pColInfo^.FOriginTabName <> '') and
      (pColInfo^.FOriginColName <> '') then begin
      // If there is only a single table, then GetSelectMetaInfo will use a
      // parameter to substitute a table name. So, oTabs must be unquoted.
      // Otherwise - oTabs must be a list of quoted table names. And will
      // be concatenated with the mkResultSetFields SQL query.
      if oTabs = nil then begin
        oTabs := TFDStringList.Create;
        oTabs.Sorted := True;
      end;
      if oTabs.IndexOf(pColInfo^.FOriginTabName) = -1 then
        oTabs.Add(pColInfo^.FOriginTabName);
    end;

    pColInfo^.FLen := oVar.sqllen;
    pColInfo^.FPrec := oVar.sqlprecision;
    pColInfo^.FScale := oVar.sqlscale;
    pColInfo^.FSrcSQLDataType := oVar.SQLDataType;
    pColInfo^.FSrcSQLSubType := oVar.sqlsubtype;

    pColInfo^.FAttrs := [];
    pColInfo^.FInPK := False;

    if oVar.IsNullable then
      Include(pColInfo^.FAttrs, caAllowNull);
    if CompareText(pColInfo^.FOriginColName, 'DB_KEY') = 0 then
      pColInfo^.FOriginColName := 'RDB$DB_KEY';
    if CompareText(pColInfo^.FOriginColName, 'RDB$DB_KEY') = 0 then begin
      Include(pColInfo^.FAttrs, caROWID);
      Include(pColInfo^.FAttrs, caAllowNull);
    end;

    SQL2ADColInfo(pColInfo^.FSrcSQLDataType, pColInfo^.FSrcSQLSubType,
      pColInfo^.FLen, pColInfo^.FPrec, pColInfo^.FScale, pColInfo^.FSrcDataType,
      pColInfo^.FAttrs, pColInfo^.FADLen, iPrec, iScale, oFmtOpts);
    pColInfo^.FPrec := iPrec;
    pColInfo^.FScale := iScale;
  end;

  if lGetDSInfo and (oTabs <> nil) and (oTabs.Count > 0) then begin
    GetConnection.CreateMetadata(oConnMeta);
    if oTabs.Count = 1 then
      sTableNames := oTabs[0]
    else
      for k := 0 to oTabs.Count - 1 do begin
        if sTableNames <> '' then
          sTableNames := sTableNames + ', ';
        sTableNames := sTableNames + QuotedStr(oTabs[k]);
      end;
    FDFree(oTabs);
    oView := oConnMeta.GetResultSetFields(sTableNames);
    try
      for i := 0 to Length(FColInfos) - 1 do begin
        pColInfo := @FColInfos[i];
        if (pColInfo^.FOriginTabName <> '') and
           (pColInfo^.FOriginColName <> '') then begin
          j := oView.Find([pColInfo^.FOriginTabName, pColInfo^.FOriginColName],
            'TABLE_NAME;COLUMN_NAME', []);
          if j <> -1 then begin
            oRow := oView.Rows[j];
            pColInfo^.FSrcTypeName := AnsiUpperCase(oRow.AsString['DOMAIN_NAME']);
            if StrLComp(PChar(pColInfo^.FSrcTypeName), PChar('RDB$'), 4) = 0 then
              pColInfo^.FSrcTypeName := '';
            if Pos('BOOL', pColInfo^.FSrcTypeName) <> 0 then
              pColInfo^.FDestDataType := dtBoolean
            else if Pos('GUID', pColInfo^.FSrcTypeName) <> 0 then
              pColInfo^.FDestDataType := dtGUID;
            if not VarIsNull(oRow.ValueS['IN_PKEY']) then
              pColInfo^.FInPK := True;
            if not VarIsNull(oRow.ValueS['GENERATOR_NAME']) then begin
              Include(pColInfo^.FAttrs, caAutoInc);
              Include(pColInfo^.FAttrs, caAllowNull);
            end;
            if oRow.ValueS['ISCOMPUTED'] = 1 then begin
              Include(pColInfo^.FAttrs, caReadOnly);
              Include(pColInfo^.FAttrs, caCalculated);
            end;
            if oRow.ValueS['HASDEFAULT'] = 1 then
              Include(pColInfo^.FAttrs, caDefault);
            if (IBConnection.ServerBrand = ibFirebird) and (IBConnection.ServerVersion >= ivFB030000) then
              if not VarIsNull(oRow.ValueS['IDENT_TYPE']) and (oRow.ValueS['IDENT_TYPE'] >= 1) then
                Include(pColInfo^.FAttrs, caAutoInc);
          end;
        end;
      end;
    finally
      FDClearMetaView(oView, oFetchOpts);
    end;
  end;

  for i := 0 to Length(FColInfos) - 1 do begin
    pColInfo := @FColInfos[i];
    // mapping data types
    if pColInfo^.FDestDataType = dtUnknown then
      if GetMetaInfoKind = mkNone then
        oFmtOpts.ResolveDataType(pColInfo^.FName, pColInfo^.FSrcDataType,
          pColInfo^.FADLen, pColInfo^.FPrec, pColInfo^.FScale,
          pColInfo^.FDestDataType, pColInfo^.FADLen, True)
      else
        pColInfo^.FDestDataType := pColInfo^.FSrcDataType;

    if not CheckFetchColumn(pColInfo^.FSrcDataType, pColInfo^.FAttrs) then
      pColInfo^.FVar := nil;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBCommandBase.DestroyColInfos;
begin
  SetLength(FColInfos, 0);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBCommandBase.CreateParamInfos(AVars: TIBVariables; AInput, AFB2Batch: Boolean);
var
  iBase, iPar, i: Integer;
  oVar: TIBVariable;
  oFmtOpts: TFDFormatOptions;
  iPrec, iScale: Integer;
  eAttrs: TFDDataAttributes;
  oParams: TFDParams;
  oParam: TFDParam;
  pInfo: PFDIBParInfoRec;

  procedure SetupStringVariable(AParam: TFDParam; var AInfo: TFDIBParInfoRec;
    var AVar: TIBVariable);
  begin
    if caFixedLen in eAttrs then
      AVar.SQLDataType := SQL_TEXT
    else
      AVar.SQLDataType := SQL_VARYING;
    case AInfo.FSrcDataType of
    dtAnsiString: AVar.sqlsubtype := csIDNone;
    dtByteString: AVar.sqlsubtype := csIDOctets;
    dtWideString: AVar.sqlsubtype := FStmt.Lib.IDUTF8;
    end;
    if AParam.Size <> 0 then
      AVar.sqllen := AParam.Size
    else
      AVar.sqllen := C_FD_DefStrSize;
    AVar.sqlprecision := 0;
    AVar.sqlscale := 0;
    AInfo.FLen := AVar.sqllen;
    AInfo.FPrec := AVar.sqlprecision;
    AInfo.FScale := AVar.sqlscale;
    AInfo.FDestSQLDataType := AVar.SQLDataType;
    AInfo.FDestSQLSubType := AVar.sqlsubtype;
    AInfo.FDestDataType := AInfo.FSrcDataType;
  end;

begin
  oFmtOpts := FOptions.FormatOptions;
  oParams := GetParams;
  iBase := Length(FParInfos);
  iPar := iBase - 1;
  SetLength(FParInfos, iBase + AVars.VarCount);

  for i := 0 to AVars.VarCount - 1 do begin

    if iPar >= oParams.Markers.Count - 1 then
      if AFB2Batch then
        iPar := -1
      else
        Break;

    oParam := nil;
    while iPar < oParams.Markers.Count - 1 do begin
      Inc(iPar);
      case GetParams.BindMode of
      pbByName:   oParam := oParams.FindParam(oParams.Markers[iPar]);
      pbByNumber: oParam := oParams.FindParam(iPar + 1);
      end;
      if (oParam = nil) or
         (oParam.DataType <> ftCursor) and (
          AInput and (oParam.ParamType in [ptUnknown, ptInput, ptInputOutput]) or
          not AInput and (oParam.ParamType in [ptOutput, ptInputOutput, ptResult])) then
        Break;
    end;

    pInfo := @FParInfos[iBase + i];
    if oParam = nil then begin
      pInfo^.FParamIndex := -1;
      Continue;
    end;
    oVar := AVars[i];

    pInfo^.FVar := oVar;
    pInfo^.FParamIndex := oParam.Index;

    if AInput and not oVar.IsNullable then
      oVar.IsNullable := True;
    if (oVar.DumpLabel = '') or (oVar.DumpLabel[1] = '#') then
      oVar.DumpLabel := oParam.DisplayName;
    pInfo^.FParamType := oParam.ParamType;

    pInfo^.FLen := oVar.sqllen;
    pInfo^.FPrec := oVar.sqlprecision;
    pInfo^.FScale := oVar.sqlscale;
    pInfo^.FDestSQLDataType := oVar.SQLDataType;
    pInfo^.FDestSQLSubType := oVar.sqlsubtype;

    eAttrs := [];
    SQL2ADColInfo(pInfo^.FDestSQLDataType, pInfo^.FDestSQLSubType, pInfo^.FLen,
      pInfo^.FPrec, pInfo^.FScale, pInfo^.FDestDataType, eAttrs, pInfo^.FADLen,
      iPrec, iScale, oFmtOpts);

    if oParam.DataType = ftUnknown then begin
      oFmtOpts.ResolveDataType('', pInfo^.FDestDataType, pInfo^.FADLen,
        iPrec, iScale, pInfo^.FSrcDataType, pInfo^.FADLen, False);
      if pInfo^.FDestDataType <> pInfo^.FSrcDataType then
        eAttrs := [];

      oFmtOpts.ColumnDef2FieldDef(pInfo^.FSrcDataType, pInfo^.FADLen, iPrec,
        iScale, eAttrs, pInfo^.FSrcFieldType, pInfo^.FSrcSize, pInfo^.FSrcPrec,
        pInfo^.FSrcScale);
      oParam.DataType := pInfo^.FSrcFieldType;
      oParam.Size := pInfo^.FSrcSize;
      oParam.Precision := pInfo^.FSrcPrec;
      oParam.NumericScale := pInfo^.FSrcScale;
    end

    else begin
      pInfo^.FSrcFieldType := oParam.DataType;
      pInfo^.FSrcSize := oParam.Size;
      pInfo^.FSrcPrec := oParam.Precision;
      pInfo^.FSrcScale := oParam.NumericScale;
      oFmtOpts.FieldDef2ColumnDef(pInfo^.FSrcFieldType, pInfo^.FSrcSize,
        pInfo^.FSrcPrec, pInfo^.FSrcScale, pInfo^.FSrcDataType, pInfo^.FADLen,
        iPrec, iScale, eAttrs);
      case pInfo^.FSrcDataType of
      dtAnsiString,
      dtByteString,
      dtWideString:
        if (pInfo^.FDestSQLDataType <> SQL_VARYING) and
           (pInfo^.FDestSQLDataType <> SQL_TEXT) and
           (pInfo^.FDestSQLDataType <> SQL_BLOB) then
          // When assigning a textual parameter value to a non-textual parameter,
          // then let FB handle a conversion, as it is FB data format dependent.
          // For example '1996-07-05' to a DATE parameter. In other cases the
          // conversion will be performed by FireDAC.
          SetupStringVariable(oParam, FParInfos[iBase + i], oVar)
        else if pInfo^.FADLen <> 0 then
          oVar.sqllen := pInfo^.FADLen;
      dtBCD,
      dtFmtBCD,
      dtCurrency:
        begin
          if iPrec <> 0 then
            oVar.sqlprecision := iPrec;
          if iScale <> 0 then
            oVar.sqlscale := - iScale;
        end;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBCommandBase.DestroyParamInfos;
begin
  SetLength(FParInfos, 0);
end;

{-------------------------------------------------------------------------------}
function TFDPhysIBCommandBase.UseExecDirect: Boolean;
begin
  // ResourceOptions.DirectExecute is not used. Because then TIBStatement.GetRowCounts
  // cannot be called and updated rows count will be zero.
  Result := GetCommandKind in [skCreate, skAlter, skDrop, skStartTransaction,
    skCommit, skRollback, skSet, skSetSchema];
end;

{-------------------------------------------------------------------------------}
function TFDPhysIBCommandBase.IsReturning: Boolean;
begin
  Result := (FStmt.Lib.Brand = ibFirebird) and (FStmt.Lib.Version >= ivFB020000) and
    (FStmt.sql_stmt_type = isc_info_sql_stmt_exec_procedure) and
    (GetCommandKind in [skDelete, skInsert, skMerge, skUpdate]);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBCommandBase.PrepareBase;
var
  i: Integer;
begin
  FStmt.Prepare(FDbCommandText);
  FStmt.DescribeSelect;
  FStmt.DescribeBind;
  if FStmt.InVars.VarCount > 0 then
    CreateParamInfos(FStmt.InVars, True, False);
  // Do not create here column infos or output param infos, we does not
  // know how programmer will use command - Execute or Open. If Execute,
  // then create param infos, else column infos.

  FRetWithOut := False;
  if IsReturning then
    for i := 0 to GetParams.Count - 1 do
      if GetParams[i].ParamType in [ptOutput, ptInputOutput, ptResult] then begin
        FRetWithOut := True;
        Break;
      end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBCommandBase.InternalPrepare;
var
  oConnMeta: IFDPhysConnectionMetadata;
  rName: TFDPhysParsedName;
begin
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
    oConnMeta.DecodeObjName(Trim(GetCommandText()), FSPParsedName, Self, []);
    FDbCommandText := '';
    if fiMeta in FOptions.FetchOptions.Items then
      GenerateStoredProcParams(FSPParsedName);
    // If an exact stored proc kind is not specified, then do not build a stored
    // proc call SQL, we does not know how a programmer will use a command - Execute
    // or Open. If Execute, then - EXEC PROC, else - SELECT.
    if (FDbCommandText = '') and (GetCommandKind <> skStoredProc) then
      GenerateStoredProcCall(FSPParsedName, GetCommandKind);
  end;

  // adjust SQL command
  GenerateLimitSelect();
  GenerateParamMarkers();

  FStmt := TIBStatement.Create(IBConnection.FDatabase,
    TFDPhysIBTransactionBase(FTransactionObj).FTransaction, Self);
  SetupStatement(FStmt);
  if (GetCommandKind <> skStoredProc) and not UseExecDirect then
    PrepareBase;
  FCursorCanceled := True;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBCommandBase.CheckSPPrepared(ASPUsage: TFDPhysCommandKind);
begin
  if (FDbCommandText <> '') or (GetCommandKind <> skStoredProc) then
    Exit;
  GenerateStoredProcCall(FSPParsedName, ASPUsage);
  PrepareBase;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBCommandBase.CheckColInfos;
begin
  if (FStmt.OutVars.VarCount > 0) and (Length(FColInfos) = 0) then
    CreateColInfos;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBCommandBase.CheckParamInfos;
begin
  if (FStmt.OutVars.VarCount > 0) and (FStmt.InVars.VarCount = Length(FParInfos)) then
    CreateParamInfos(FStmt.OutVars, False, False);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBCommandBase.InternalUnprepare;
begin
  if FStmt = nil then
    Exit;
  FStmt.Unprepare;
  DestroyColInfos;
  DestroyParamInfos;
  FDFreeAndNil(FStmt);
end;

{-------------------------------------------------------------------------------}
function TFDPhysIBCommandBase.InternalUseStandardMetadata: Boolean;
begin
  Result := not IBConnection.FExtendedMetadata;
end;

{-------------------------------------------------------------------------------}
function TFDPhysIBCommandBase.InternalColInfoStart(var ATabInfo: TFDPhysDataTableInfo): Boolean;
begin
  // After Prepare call the columns are defined. But here a
  // command is executing to make OpenOrExecute method working.
  Result := InternalOpen;
  if ATabInfo.FSourceID = -1 then begin
    ATabInfo.FSourceName := GetCommandText;
    ATabInfo.FSourceID := 1;
    FColumnIndex := 0;
  end;
end;

{-------------------------------------------------------------------------------}
function TFDPhysIBCommandBase.InternalColInfoGet(var AColInfo: TFDPhysDataColumnInfo): Boolean;
var
  pColInfo: PFDIBColInfoRec;
begin
  if FColumnIndex < Length(FColInfos) then begin
    pColInfo := @FColInfos[FColumnIndex];
    AColInfo.FSourceName := pColInfo^.FName;
    AColInfo.FSourceID := pColInfo^.FPos;
    AColInfo.FSourceType := pColInfo^.FSrcDataType;
    AColInfo.FSourceTypeName := pColInfo^.FSrcTypeName;
    AColInfo.FOriginTabName.FObject := pColInfo^.FOriginTabName;
    AColInfo.FOriginColName := pColInfo^.FOriginColName;
    AColInfo.FType := pColInfo^.FDestDataType;
    AColInfo.FLen := pColInfo^.FADLen;
    AColInfo.FPrec := pColInfo^.FPrec;
    AColInfo.FScale := Abs(pColInfo^.FScale);
    AColInfo.FAttrs := pColInfo^.FAttrs;
    AColInfo.FForceAddOpts := [];
    if pColInfo^.FInPK then
      Include(AColInfo.FForceAddOpts, coInKey);
    Inc(FColumnIndex);
    Result := True;
  end
  else
    Result := False;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBCommandBase.SetParamValue(AFmtOpts: TFDFormatOptions; AParam: TFDParam;
  AVar: TIBVariable; ApInfo: PFDIBParInfoRec; AVarIndex, AParIndex: Integer);

  function CreateBlob(ADestDataType: TFDDataType): TIBBlob;
  begin
    Result := TIBBlob.Create(FStmt.Database,
      TFDPhysIBTransactionBase(FTransactionObj).FTransaction, Self);
    if ADestDataType in C_FD_CharTypes then
      Result.sqlsubtype := isc_blob_text;
    Result.Add;
  end;

var
  pData: PByte;
  iSize, iSrcSize: LongWord;
  oBlob: TIBBlob;
begin
  pData := nil;
  iSize := 0;

  // null
  if AParam.IsNulls[AParIndex] then
    AVar.SetData(AVarIndex, nil, 0)

  // conversion is not required
  else if ApInfo^.FSrcDataType = ApInfo^.FDestDataType then begin

    // byte string data, then optimizing - get data directly
    if ApInfo^.FDestSQLDataType = SQL_BLOB then begin
      oBlob := CreateBlob(ApInfo^.FDestDataType);
      try
        AParam.GetBlobRawData(iSize, pData, AParIndex);
        oBlob.Write(PByte(pData), iSize);
        AVar.SetData(AVarIndex, @oBlob.FBlobID, SizeOf(oBlob.FBlobID),
          ApInfo^.FDestDataType);
      finally
        FDFree(oBlob);
      end;
    end

    else if ApInfo^.FDestDataType in (C_FD_VarLenTypes {$IFDEF NEXTGEN} - C_FD_AnsiTypes {$ENDIF}) then begin
      AParam.GetBlobRawData(iSize, pData, AParIndex);
      AVar.SetData(AVarIndex, pData, iSize, ApInfo^.FDestDataType);
    end

    else begin
      iSize := AParam.GetDataLength(AParIndex);
      FBuffer.Check(iSize);
      AParam.GetData(FBuffer.Ptr, AParIndex);
      AVar.SetData(AVarIndex, FBuffer.Ptr, iSize, ApInfo^.FDestDataType);
    end;
  end

  // conversion is required
  else begin
    // calculate buffer size to move param values
    iSrcSize := AParam.GetDataLength(AParIndex);
    FBuffer.Extend(iSrcSize, iSize, ApInfo^.FSrcDataType, ApInfo^.FDestDataType);

    // get, convert and set parameter value
    AParam.GetData(FBuffer.Ptr, AParIndex);
    AFmtOpts.ConvertRawData(ApInfo^.FSrcDataType, ApInfo^.FDestDataType,
      FBuffer.Ptr, iSrcSize, FBuffer.FBuffer, FBuffer.Size, iSize,
      IBConnection.FDatabase.Encoder);

    if ApInfo^.FDestSQLDataType = SQL_BLOB then begin
      oBlob := CreateBlob(ApInfo^.FDestDataType);
      try
        oBlob.Write(FBuffer.Ptr, iSize);
        AVar.SetData(AVarIndex, @oBlob.FBlobID, SizeOf(oBlob.FBlobID),
          ApInfo^.FDestDataType);
      finally
        FDFree(oBlob);
      end;
    end

    else
      AVar.SetData(AVarIndex, FBuffer.Ptr, iSize, ApInfo^.FDestDataType);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBCommandBase.SetParamValues(ATimes, AOffset: Integer);
var
  oParams: TFDParams;
  oFmtOpts: TFDFormatOptions;
  oParam: TFDParam;
  iArraySize, i, j: Integer;
  pParInfo: PFDIBParInfoRec;
begin
  oParams := GetParams;
  if oParams.Count = 0 then
    Exit;

  oFmtOpts := GetOptions.FormatOptions;
  iArraySize := oParams.ArraySize;
  if ATimes < iArraySize then
    iArraySize := ATimes;

  for i := 0 to Length(FParInfos) - 1 do begin
    pParInfo := @FParInfos[i];
    if pParInfo^.FParamIndex <> -1 then begin
      oParam := oParams[pParInfo^.FParamIndex];
      if (pParInfo^.FVar <> nil) and
         (oParam.DataType <> ftCursor) and
         (oParam.ParamType in [ptInput, ptInputOutput, ptUnknown]) then begin
        CheckParamMatching(oParam, pParInfo^.FSrcFieldType, pParInfo^.FParamType,
          pParInfo^.FSrcPrec);
        for j := AOffset to iArraySize - 1 do
          SetParamValue(oFmtOpts, oParam, pParInfo^.FVar, pParInfo, j - AOffset, j);
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBCommandBase.GetParamValue(AFmtOpts: TFDFormatOptions;
  AParam: TFDParam; AVar: TIBVariable; ApInfo: PFDIBParInfoRec;
  AVarIndex, AParIndex: Integer);

  function CreateBlob(const ABlobID: TISCQuad; ADestDataType: TFDDataType): TIBBlob;
  begin
    Result := TIBBlob.Create(FStmt.Database,
      TFDPhysIBTransactionBase(FTransactionObj).FTransaction, Self);
    Result.BlobID := ABlobID;
    if ADestDataType in C_FD_CharTypes then
      Result.sqlsubtype := isc_blob_text;
    Result.Open;
  end;

var
  pData: Pointer;
  iSize, iByteSize, iDestSize: LongWord;
  oBlob: TIBBlob;
begin
  pData := nil;
  iSize := 0;

  // null
  if not AVar.GetData(AVarIndex, pData, iSize, True) then
    AParam.Clear(AParIndex)

  // conversion is not required
  else if ApInfo^.FDestDataType = ApInfo^.FSrcDataType then begin

    // byte string data, then optimizing - get data directly
    if ApInfo^.FDestSQLDataType = SQL_BLOB then begin
      oBlob := CreateBlob(PISCQuad(pData)^, ApInfo^.FDestDataType);
      try
        iSize := oBlob.total_length;
        pData := AParam.SetBlobRawData(iSize, nil, AParIndex);
        if iSize > 0 then
          oBlob.Read(pData, iSize);
      finally
        FDFree(oBlob);
      end;
    end

    else if ApInfo^.FDestDataType in C_FD_VarLenTypes then
      AParam.SetData(PByte(pData), iSize, AParIndex)

    else begin
      FBuffer.Check(iSize);
      AVar.GetData(AVarIndex, FBuffer.FBuffer, iSize, False);
      AParam.SetData(FBuffer.Ptr, iSize, AParIndex);
    end;
  end

  // conversion is required
  else begin
    if ApInfo^.FDestSQLDataType = SQL_BLOB then begin
      oBlob := CreateBlob(PISCQuad(pData)^, ApInfo^.FDestDataType);
      try
        iSize := oBlob.total_length;
        iByteSize := iSize;
        if oBlob.National then
          iByteSize := iByteSize * SizeOf(WideChar);
        pData := FBuffer.Check(iByteSize);
        if iSize > 0 then
          oBlob.Read(pData, iSize);
      finally
        FDFree(oBlob);
      end;
    end

    else begin
      FBuffer.Check(iSize);
      AVar.GetData(AVarIndex, FBuffer.FBuffer, iSize, False);
    end;

    iDestSize := 0;
    AFmtOpts.ConvertRawData(ApInfo^.FDestDataType, ApInfo^.FSrcDataType,
      FBuffer.Ptr, iSize, FBuffer.FBuffer, FBuffer.Size, iDestSize,
      IBConnection.FDatabase.Encoder);
    AParam.SetData(FBuffer.Ptr, iDestSize, AParIndex);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBCommandBase.GetParamValues(ATimes, AOffset: Integer);
var
  oParams: TFDParams;
  oFmtOpts: TFDFormatOptions;
  iArraySize, i, j: Integer;
  oParam: TFDParam;
  pParInfo: PFDIBParInfoRec;
begin
  oParams := GetParams;
  if oParams.Count = 0 then
    Exit;

  oFmtOpts := FOptions.FormatOptions;
  iArraySize := oParams.ArraySize;
  if ATimes < iArraySize then
    iArraySize := ATimes;

  for i := 0 to Length(FParInfos) - 1 do begin
    pParInfo := @FParInfos[i];
    if pParInfo^.FParamIndex <> -1 then begin
      oParam := oParams[pParInfo^.FParamIndex];
      if (pParInfo^.FVar <> nil) and
         (oParam.DataType <> ftCursor) and
         (oParam.ParamType in [ptOutput, ptResult, ptInputOutput]) then begin
        CheckParamMatching(oParam, pParInfo^.FSrcFieldType, pParInfo^.FParamType,
          pParInfo^.FSrcPrec);
        for j := AOffset to iArraySize - 1 do
          GetParamValue(oFmtOpts, oParam, pParInfo^.FVar, pParInfo, j - AOffset, j);
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBCommandBase.DoExecute(ATimes, AOffset: Integer;
  var ACount: TFDCounter);
var
  i: Integer;
  iCount: TFDCounter;
begin
  if ATimes - AOffset > 1 then begin
    for i := AOffset to ATimes - 1 do begin
      iCount := 0;
      DoExecute(i + 1, i, iCount);
      Inc(ACount, iCount);
    end;
  end
  else begin
    FStmt.InVars.RowCount := 1;
    SetParamValues(ATimes, AOffset);
    try
      try
        if UseExecDirect then
          FStmt.ExecuteImmediate(FDbCommandText)
        else
          // If it is INSERT / UPDATE ... RETURNING, then Firebird describes it
          // as EXECUTE PROCEDURE. In this case force to count inserted / updated
          // records.
          FStmt.Execute(IsReturning);
      except
        on E: EIBNativeException do begin
          E.Errors[0].RowIndex := AOffset;
          raise;
        end;
      end;
    finally
      if FStmt <> nil then
        Inc(ACount, FStmt.RowsAffected);
    end;
    if FStmt.OutVars.VarCount > 0 then
      GetParamValues(ATimes, AOffset);
    FStmt.Close;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBCommandBase.InternalAbort;
begin
  if FStmt <> nil then
    FStmt.Cancel;
end;

{-------------------------------------------------------------------------------}
function TFDPhysIBCommandBase.InternalOpen: Boolean;
var
  oFetchOpts: TFDFetchOptions;
  iCount: TFDCounter;
  lExactFetch: Boolean;
begin
  if not FCursorCanceled then begin
    Result := True;
    Exit;
  end
  else if (GetMetaInfoKind <> mkNone) and (FDbCommandText = '') then begin
    Result := False;
    Exit;
  end
  else if GetMetaInfoKind = mkProcArgs then
    FMaxInputPos := 0;
  if (GetCommandKind <> skStoredProc) and UseExecDirect then
    PrepareBase
  else
    CheckSPPrepared(skStoredProcWithCrs);
  CheckColInfos;
  Result := Length(FColInfos) > 0;
  if not Result then begin
    iCount := 0;
    InternalExecute(GetParams.ArraySize, 0, iCount);
  end
  else begin
    oFetchOpts := FOptions.FetchOptions;
    SetParamValues(1, 0);
    lExactFetch := (oFetchOpts.Mode = fmExactRecsMax) and (oFetchOpts.RecsMax = 1) and
      // isc_dsql_execute2 returns "Attempt to reopen an open cursor" on
      // second call for SELECT FOR UPDATE statement. For simple SELECT - OK.
      (FStmt.sql_stmt_type <> isc_info_sql_stmt_select_for_upd);
    try
      FStmt.Open(lExactFetch or (FStmt.sql_stmt_type = isc_info_sql_stmt_exec_procedure));
    except
      on E: EIBNativeException do begin
        if lExactFetch and (E.Kind in [ekNoDataFound, ekTooManyRows]) then
          // FDQA checks FDCode = er_FD_AccExactFetchMismatch in fmExactRecsMax tests
          E.FDCode := er_FD_AccExactMismatch;
        raise;
      end;
    end;
    FCursorCanceled := False;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBCommandBase.InternalClose;
begin
  if (FStmt <> nil) and not FCursorCanceled then begin
    if FRetWithOut then begin
      if not FStmt.Eof then
        FStmt.Fetch;
      CheckParamInfos;
      GetParamValues(1, 0);
    end;
    FCursorCanceled := True;
    FStmt.Close;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBCommandBase.ProcessColumn(ATable: TFDDatSTable; AFmtOpts: TFDFormatOptions;
  AColIndex: Integer; ARow: TFDDatSRow; ApInfo: PFDIBColInfoRec; ARowIndex: Integer);

  function CreateBlob(const ABlobID: TISCQuad; ASrcDataType: TFDDataType): TIBBlob;
  begin
    Result := TIBBlob.Create(FStmt.Database,
      TFDPhysIBTransactionBase(FTransactionObj).FTransaction, Self);
    Result.BlobID := ABlobID;
    if ASrcDataType in C_FD_CharTypes then
      Result.sqlsubtype := isc_blob_text;
    Result.Open;
  end;

var
  pData: Pointer;
  iSize, iByteSize, iDestSize: LongWord;
  oBlob: TIBBlob;
begin
  pData := nil;
  iSize := 0;

  // null
  if not ApInfo^.FVar.GetData(ARowIndex, pData, iSize, True) then
    ARow.SetData(AColIndex, nil, 0)

  // conversion is not required
  else if ApInfo^.FSrcDataType = ApInfo^.FDestDataType then
    if ApInfo^.FSrcSQLDataType = SQL_BLOB then begin
      oBlob := CreateBlob(PISCQuad(pData)^, ApInfo^.FSrcDataType);
      try
        iSize := oBlob.total_length;
        pData := ARow.BeginDirectWriteBlob(AColIndex, iSize);
        try
          if iSize > 0 then
            iSize := oBlob.Read(pData, iSize);
        finally
          ARow.EndDirectWriteBlob(AColIndex, iSize);
        end;
      finally
        FDFree(oBlob);
      end;
    end

    else if ApInfo^.FDestDataType in C_FD_VarLenTypes then
      ARow.SetData(AColIndex, pData, iSize)
    else begin
      FBuffer.Check(iSize);
      ApInfo^.FVar.GetData(ARowIndex, FBuffer.FBuffer, iSize, False);
      ARow.SetData(AColIndex, FBuffer.Ptr, iSize);
    end

  // conversion is required
  else begin
    if ApInfo^.FSrcSQLDataType = SQL_BLOB then begin
      oBlob := CreateBlob(PISCQuad(pData)^, ApInfo^.FSrcDataType);
      try
        iSize := oBlob.total_length;
        iByteSize := iSize;
        if oBlob.National then
          iByteSize := iByteSize * SizeOf(WideChar);
        pData := FBuffer.Check(iByteSize);
        if iSize > 0 then
          oBlob.Read(pData, iSize);
      finally
        FDFree(oBlob);
      end;
    end
    else begin
      iByteSize := iSize;
      if ApInfo^.FVar.National then
        iByteSize := iByteSize * SizeOf(WideChar);
      FBuffer.Check(iByteSize);
      ApInfo^.FVar.GetData(ARowIndex, FBuffer.FBuffer, iSize, False);
    end;
    iDestSize := 0;
    AFmtOpts.ConvertRawData(ApInfo^.FSrcDataType, ApInfo^.FDestDataType,
      FBuffer.Ptr, iSize, FBuffer.FBuffer, FBuffer.Size, iDestSize,
      IBConnection.FDatabase.Encoder);
    ARow.SetData(AColIndex, FBuffer.Ptr, iDestSize);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBCommandBase.ProcessMetaColumn(ATable: TFDDatSTable;
  AFmtOpts: TFDFormatOptions; AColIndex: Integer; ARow: TFDDatSRow;
  ApInfo: PFDIBColInfoRec; ARowIndex: Integer);
var
  pData: Pointer;
  iSize, iDestSize: Longword;
begin
  pData := nil;
  iSize := 0;
  if AColIndex = 0 then
    ARow.SetData(0, ATable.Rows.Count + 1)

  else if not ApInfo^.FVar.GetData(ARowIndex, pData, iSize, True) then
    ARow.SetData(AColIndex, nil, 0)

  else if ApInfo^.FDestDataType in C_FD_VarLenTypes then begin
    if iSize > ATable.Columns[AColIndex].Size then
      iSize := ATable.Columns[AColIndex].Size;
    if ApInfo^.FDestDatatype in C_FD_AnsiTypes then begin
      FBuffer.Check(iSize * SizeOf(WideChar));
      AFmtOpts.ConvertRawData(dtAnsiString, dtWideString,
        pData, iSize, FBuffer.FBuffer, FBuffer.Size, iDestSize,
        IBConnection.FDatabase.Encoder);
      ARow.SetData(AColIndex, FBuffer.Ptr, iDestSize);
    end
    else
      ARow.SetData(AColIndex, pData, iSize);
  end

  else begin
    FBuffer.Check(iSize);
    ApInfo^.FVar.GetData(ARowIndex, FBuffer.FBuffer, iSize, False);
    iDestSize := 0;
    if ApInfo^.FDestDataType in C_FD_NumTypes then
      AFmtOpts.ConvertRawData(ApInfo^.FSrcDataType, ATable.Columns[AColIndex].DataType,
        FBuffer.Ptr, iSize, FBuffer.FBuffer, FBuffer.Size, iDestSize,
        IBConnection.FDatabase.Encoder)
    else
      iDestSize := iSize;
    ARow.SetData(AColIndex, FBuffer.Ptr, iDestSize);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBCommandBase.FetchRow(ATable: TFDDatSTable; AParentRow: TFDDatSRow);
var
  oRow: TFDDatSRow;
  oFmtOpts: TFDFormatOptions;
  pColInfo: PFDIBColInfoRec;
  j: Integer;
  lMetadata: Boolean;
  oCol: TFDDatSColumn;

  procedure AdjustProcArgPos;
  var
    eParamType: TParamType;
    iPos: Integer;
  begin
    eParamType := TParamType(oRow.GetData(8));
    case eParamType of
    ptInput:
      begin
        iPos := oRow.GetData(7);
        if iPos > FMaxInputPos then
          FMaxInputPos := iPos;
      end;
    ptOutput:
      oRow.SetData(7, oRow.GetData(7) + FMaxInputPos);
    end;
  end;

begin
  oFmtOpts := FOptions.FormatOptions;
  oRow := ATable.NewRow(False);
  lMetadata := GetMetaInfoKind <> mkNone;
  try
    for j := 0 to ATable.Columns.Count - 1 do begin
      oCol := ATable.Columns[j];
      if (oCol.SourceID > 0) and CheckFetchColumn(oCol.SourceDataType, oCol.Attributes) then begin
        pColInfo := @FColInfos[oCol.SourceID - 1];
        if pColInfo^.FVar <> nil then
          if lMetadata then
            ProcessMetaColumn(ATable, oFmtOpts, j, oRow, pColInfo, 0)
          else
            ProcessColumn(ATable, oFmtOpts, j, oRow, pColInfo, 0);
        end;
      end;
    if lMetadata and (GetMetaInfoKind = mkProcArgs) then
      AdjustProcArgPos;
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

{-------------------------------------------------------------------------------}
function TFDPhysIBCommandBase.InternalFetchRowSet(ATable: TFDDatSTable;
  AParentRow: TFDDatSRow; ARowsetSize: LongWord): LongWord;
var
  i: LongWord;
begin
  Result := 0;
  if FStmt <> nil then
    for i := 1 to ARowsetSize do begin
      if (GetState <> csFetching) or not FStmt.Fetch then
        Break;
      FetchRow(ATable, AParentRow);
      Inc(Result);
    end;
end;

{-------------------------------------------------------------------------------}
function TFDPhysIBCommandBase.InternalNextRecordSet: Boolean;
begin
  Result := False;
end;

{-------------------------------------------------------------------------------}
function TFDPhysIBCommandBase.GetItemCount: Integer;
begin
  Result := inherited GetItemCount;
  if FStmt <> nil then
    Inc(Result, 4);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBCommandBase.GetItem(AIndex: Integer; out AName: String;
  out AValue: Variant; out AKind: TFDMoniAdapterItemKind);
begin
  if AIndex < inherited GetItemCount then
    inherited GetItem(AIndex, AName, AValue, AKind)
  else
    case AIndex - inherited GetItemCount of
    0:
      begin
        AName := 'Rows inserted';
        AValue := FStmt.RowsInserted;
        AKind := ikStat;
      end;
    1:
      begin
        AName := 'Rows updated';
        AValue := FStmt.RowsUpdated;
        AKind := ikStat;
      end;
    2:
      begin
        AName := 'Rows deleted';
        AValue := FStmt.RowsDeleted;
        AKind := ikStat;
      end;
    3:
      begin
        AName := 'Rows selected';
        AValue := FStmt.RowsSelected;
        AKind := ikStat;
      end;
    end;
end;

end.
