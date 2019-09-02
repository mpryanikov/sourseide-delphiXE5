{*******************************************************}
{                                                       }
{               Delphi FireDAC Framework                }
{                 FireDAC MSSQL driver                  }
{                                                       }
{ Copyright(c) 2004-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}
{$I FireDAC.inc}
{$IFDEF WIN32}
  {$HPPEMIT '#pragma link "FireDAC.Phys.MSSQL.obj"'}
{$ELSE}
  {$HPPEMIT '#pragma link "FireDAC.Phys.MSSQL.o"'}
{$ENDIF}

unit FireDAC.Phys.MSSQL;

interface

uses
  System.Classes,
  FireDAC.Stan.Error, FireDAC.Stan.Storage, 
  FireDAC.Phys, FireDAC.Phys.ODBCCli, FireDAC.Phys.ODBCWrapper, FireDAC.Phys.ODBCBase;

type
  TFDMSSQLError = class;
  EMSSQLNativeException = class;
  TFDPhysMSSQLDriverLink = class;

  TFDMSSQLError = class(TFDODBCNativeError)
  private
    FLine: Integer;
    FMessageState: Integer;
    FSeverity: Integer;
    FProcName: String;
    FServerName: String;
  protected
    procedure Assign(ASrc: TFDDBError); override;
    procedure LoadFromStorage(AStorage: TFDStorage); override;
    procedure SaveToStorage(AStorage: TFDStorage); override;
  public
    property Line: Integer read FLine;
    property MessageState: Integer read FMessageState;
    property Severity: Integer read FSeverity;
    property ProcName: String read FProcName;
    property ServerName: String read FServerName;
  end;

  EMSSQLNativeException = class(EODBCNativeException)
  private
    function GetErrors(AIndex: Integer): TFDMSSQLError;
  protected
    function GetErrorClass: TFDDBErrorClass; override;
  public
    function AppendError(AHandle: TODBCHandle; ARecNum: SQLSmallint;
      const ASQLState: String; ANativeError: SQLInteger; const ADiagMessage,
      AResultMessage, ACommandText, AObject: String;
      AKind: TFDCommandExceptionKind; ACmdOffset, ARowIndex: Integer): TFDDBError; override;
    property Errors[Index: Integer]: TFDMSSQLError read GetErrors; default;
  end;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32)]
  TFDPhysMSSQLDriverLink = class(TFDPhysODBCBaseDriverLink)
  protected
    function GetBaseDriverID: String; override;
  end;

{-------------------------------------------------------------------------------}
implementation

uses
  System.SysUtils,
{$IFDEF MSWINDOWS}
  System.Win.ComObj, Winapi.ActiveX,
{$ENDIF}
  System.StrUtils, System.Variants,
  FireDAC.Stan.Intf, FireDAC.Stan.Consts, FireDAC.Stan.Util, FireDAC.Stan.Factory, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.Phys.SQLGenerator, FireDAC.Phys.MSSQLMeta;

const
{$IFDEF POSIX}
  C_FreeTDS = 'FreeTDS';
  C_FreeTDSLib = 'libtdsodbc.so';
{$ENDIF}
{$IFDEF MSWINDOWS}
  C_2012 = 'SQL SERVER NATIVE CLIENT 11.0';
  C_2008 = 'SQL SERVER NATIVE CLIENT 10.0';
  C_2005 = 'SQL NATIVE CLIENT';
  C_2000 = 'SQL SERVER';
{$ENDIF}
  C_String = 'String';
  C_Binary = 'Binary';

var
  GSQLServerList: String = '*';

type
  TFDPhysMSSQLDriver = class;
  TFDPhysMSSQLConnection = class;
{$IFDEF MSWINDOWS}
  TFDPhysMSSQLEventAlerter = class;
{$ENDIF}

  TFDPhysMSSQLDriver = class(TFDPhysODBCDriverBase)
  protected
    class function GetBaseDriverID: String; override;
    procedure InternalLoad; override;
    function InternalCreateConnection(AConnHost: TFDPhysConnectionHost): TFDPhysConnection; override;
    procedure GetODBCConnectStringKeywords(AKeywords: TStrings); override;
    function GetConnParamCount(AKeys: TStrings): Integer; override;
    procedure GetConnParams(AKeys: TStrings; AIndex: Integer; var AName, AType,
      ADefVal, ACaption: String; var ALoginIndex: Integer); override;
  public
    function BuildODBCConnectString(const AConnectionDef: IFDStanConnectionDef): string; override;
  end;

  TFDPhysMSSQLConnection = class(TFDPhysODBCConnectionBase)
  private
    FCatalogCaseSensitive: Boolean;
    FExtendedMetadata: Boolean;
    procedure CheckPasswordChange;
  protected
    function InternalCreateEvent(const AEventKind: String): TFDPhysEventAlerter; override;
    function InternalCreateCommandGenerator(const ACommand:
      IFDPhysCommand): TFDPhysCommandGenerator; override;
    function InternalCreateMetadata: TObject; override;
    procedure GetStrsMaxSizes(AStrDataType: SQLSmallint; AFixedLen: Boolean;
      out ACharSize, AByteSize: Integer); override;
    function GetExceptionClass: EODBCNativeExceptionClass; override;
    procedure InternalSetMeta; override;
    procedure SetupConnection; override;
    procedure InternalChangePassword(const AUserName, AOldPassword,
      ANewPassword: String); override;
    procedure InternalAnalyzeSession(AMessages: TStrings); override;
  end;

{$IFDEF MSWINDOWS}
  TFDPhysMSSQLEventAlerter = class (TFDPhysEventAlerter)
  private
    FWaitConnection: IFDPhysConnection;
    FWaitCommand: IFDPhysCommand;
    FMessageTab: TFDDatSTable;
    FWaitThread: TThread;
    FServiceName, FQueueName, FEventTabName: String;
    FDropService, FDropQueue, FEventTab: Boolean;
    procedure DoFired;
    procedure InternalRegisterBase;
  protected
    procedure InternalAllocHandle; override;
    procedure InternalRegister; override;
    procedure InternalHandle(AEventMessage: TFDPhysEventMessage); override;
    procedure InternalAbortJob; override;
    procedure InternalUnregister; override;
    procedure InternalReleaseHandle; override;
    procedure InternalSignal(const AEvent: String; const AArgument: Variant); override;
  end;
{$ENDIF}

{-------------------------------------------------------------------------------}
{ TFDMSSQLError                                                                 }
{-------------------------------------------------------------------------------}
procedure TFDMSSQLError.Assign(ASrc: TFDDBError);
begin
  inherited Assign(ASrc);
  if ASrc is TFDMSSQLError then begin
    FLine := TFDMSSQLError(ASrc).FLine;
    FMessageState := TFDMSSQLError(ASrc).FMessageState;
    FSeverity := TFDMSSQLError(ASrc).FSeverity;
    FProcName := TFDMSSQLError(ASrc).FProcName;
    FServerName := TFDMSSQLError(ASrc).FServerName;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDMSSQLError.LoadFromStorage(AStorage: TFDStorage);
begin
  inherited LoadFromStorage(AStorage);
  FLine := AStorage.ReadInteger('Line', 0);
  FMessageState := AStorage.ReadInteger('MessageState', 0);
  FSeverity := AStorage.ReadInteger('Severity', 0);
  FProcName := AStorage.ReadString('ProcName', '');
  FServerName := AStorage.ReadString('ServerName', '');
end;

{-------------------------------------------------------------------------------}
procedure TFDMSSQLError.SaveToStorage(AStorage: TFDStorage);
begin
  inherited SaveToStorage(AStorage);
  AStorage.WriteInteger('Line', Line, 0);
  AStorage.WriteInteger('MessageState', MessageState, 0);
  AStorage.WriteInteger('Severity', Severity, 0);
  AStorage.WriteString('ProcName', ProcName, '');
  AStorage.WriteString('ServerName', ServerName, '');
end;

{-------------------------------------------------------------------------------}
{ EMSSQLNativeException                                                         }
{-------------------------------------------------------------------------------}
function EMSSQLNativeException.AppendError(AHandle: TODBCHandle;
  ARecNum: SQLSmallint; const ASQLState: String; ANativeError: SQLInteger;
  const ADiagMessage, AResultMessage, ACommandText, AObject: String;
  AKind: TFDCommandExceptionKind; ACmdOffset, ARowIndex: Integer): TFDDBError;
var
  sObj, sUDiagMessage, sRes: String;
  i, j: Integer;
  oErr: TFDMSSQLError;
  oStmt: TODBCStatementBase;

  procedure ExtractObjName;
  var
    i1, i2: Integer;
  begin
    i1 := Pos('''', ADiagMessage);
    if i1 <> 0 then begin
      i2 := FDPosEx('''', ADiagMessage, i1 + 1);
      if i2 <> 0 then
        sObj := Copy(ADiagMessage, i1 + 1, i2 - i1 - 1);
    end;
  end;

begin
  // following is not supported by MSSQL:
  // ekNoDataFound
  // ekUserPwdWillExpire
  sObj := AObject;
  sRes := AResultMessage;
  case ANativeError of
  1204,
  1222:
    AKind := ekRecordLocked;
  2601,
  2627:
    begin
      AKind := ekUKViolated;
      // first 'xxxx' - constraint name
      // second 'xxxx' - table name
      ExtractObjName;
    end;
  547:
    begin
      sUDiagMessage := UpperCase(ADiagMessage);
      if (Pos('COLUMN FOREIGN KEY', sUDiagMessage) <> 0) or
         (Pos('CONFLICTED', sUDiagMessage) <> 0) and
          (Pos('REFERENCE', sUDiagMessage) <> 0) and
          (Pos('CONSTRAINT', sUDiagMessage) <> 0) then begin
        AKind := ekFKViolated;
        // first 'xxxx' - constraint name
        // next 3 'xxxx' - full column name (database, table, column)
        ExtractObjName;
      end;
    end;
  208,
  3701:
    begin
      AKind := ekObjNotExists;
      // first 'xxxx' - object name
      ExtractObjName;
    end;
  18456,
  18463,
  18464,
  18465,
  18466,
  18467,
  18468:
    AKind := ekUserPwdInvalid;
  18487,
  18488:
    AKind := ekUserPwdExpired;
  170:
    // strange "incorrect syntax near '{'" at Array INSERT execute,
    // for example on PKSP_Merge DOCS_OUT table
    if (Pos('''{''', UpperCase(ADiagMessage)) <> 0) and (Pos('{', ACommandText) = 0) and
       (AHandle is TODBCCommandStatement) and (TODBCCommandStatement(AHandle).PARAMSET_SIZE > 1) then
      AKind := ekArrExecMalfunc;
  0:
    if ASQLState = '01000' then begin
      AKind := ekServerOutput;
      i := 0;
      for j := 1 to 3 do
        i := FDPosEx(']', sRes, i + 1);
      if i <> 0 then
        sRes := Copy(sRes, i + 1, Length(sRes));
    end
    // strange errors at Array DML:
    // - "Invalid character value for cast specification", 22018.
    //   GS has provided the bug report for 22018.
    // - "Invalid cursor state", 24000.
    //   FDQA fails on Batch execute -> Error handling on MSSQL 2005 with 24000 state.
    // - "String data, length mismatch", 22026
    //   Insert into VARBINARY(MAX) on MSSQL 2008 may give 22026.
    else if ((ASQLState = '22018') or (ASQLState = '24000') or (ASQLState = '22026')) and
            (AHandle is TODBCCommandStatement) and (TODBCCommandStatement(AHandle).PARAMSET_SIZE > 1) then
      AKind := ekArrExecMalfunc;
  201,
  8144:
    AKind := ekInvalidParams;
  end;
  Result := inherited AppendError(AHandle, ARecNum, ASQLState, ANativeError,
    ADiagMessage, sRes, ACommandText, sObj, AKind, ACmdOffset, ARowIndex);
  if AHandle is TODBCStatementBase then begin
    oErr := TFDMSSQLError(Result);
    oStmt := TODBCStatementBase(AHandle);
    try
      oStmt.IgnoreErrors := True;
      oErr.FLine         := oStmt.DIAG_SS_LINE[ARecNum];
      oErr.FMessageState := oStmt.DIAG_SS_MSGSTATE[ARecNum];
      oErr.FSeverity     := oStmt.DIAG_SS_SEVERITY[ARecNum];
      oErr.FProcName     := oStmt.DIAG_SS_PROCNAME[ARecNum];
      if oErr.ObjName = '' then
        oErr.ObjName     := oErr.FProcName;
      oErr.FServerName   := oStmt.DIAG_SS_SRVNAME[ARecNum];
    finally
      oStmt.IgnoreErrors := False;
    end;
  end;
end;

{-------------------------------------------------------------------------------}
function EMSSQLNativeException.GetErrorClass: TFDDBErrorClass;
begin
  Result := TFDMSSQLError;
end;

{-------------------------------------------------------------------------------}
function EMSSQLNativeException.GetErrors(AIndex: Integer): TFDMSSQLError;
begin
  Result := inherited Errors[AIndex] as TFDMSSQLError;
end;

{-------------------------------------------------------------------------------}
{ TFDPhysMSSQLDriverLink                                                        }
{-------------------------------------------------------------------------------}
function TFDPhysMSSQLDriverLink.GetBaseDriverID: String;
begin
  Result := S_FD_MSSQLId;
end;

{-------------------------------------------------------------------------------}
{ TFDPhysMSSQLDriver                                                            }
{-------------------------------------------------------------------------------}
class function TFDPhysMSSQLDriver.GetBaseDriverID: String;
begin
  Result := S_FD_MSSQLId;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMSSQLDriver.InternalLoad;
begin
  inherited InternalLoad;
  if ODBCDriver = '' then
    ODBCDriver := FindBestDriver({$IFDEF MSWINDOWS} [C_2012, C_2008, C_2005, C_2000] {$ENDIF}
                                 {$IFDEF POSIX} [C_FreeTDS], C_FreeTDSLib {$ENDIF});
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLDriver.InternalCreateConnection(
  AConnHost: TFDPhysConnectionHost): TFDPhysConnection;
begin
  Result := TFDPhysMSSQLConnection.Create(Self, AConnHost);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMSSQLDriver.GetODBCConnectStringKeywords(AKeywords: TStrings);
begin
  inherited GetODBCConnectStringKeywords(AKeywords);
  AKeywords.Add(S_FD_ConnParam_Common_Server);
  AKeywords.Add(S_FD_ConnParam_Common_Database);
  AKeywords.Add(S_FD_ConnParam_MSSQL_Language);
  AKeywords.Add(S_FD_ConnParam_MSSQL_Address);
  AKeywords.Add(S_FD_ConnParam_MSSQL_Workstation + '=WSID');
{$IFDEF POSIX}
  AKeywords.Add(S_FD_ConnParam_Common_Port);
{$ENDIF}
{$IFDEF MSWINDOWS}
  AKeywords.Add(S_FD_ConnParam_MSSQL_Network);
  AKeywords.Add(S_FD_ConnParam_Common_OSAuthent + '=Trusted_Connection');
  AKeywords.Add(S_FD_ConnParam_MSSQL_MARS + '=MARS_Connection');
  AKeywords.Add(S_FD_ConnParam_MSSQL_Encrypt);
{$ENDIF}
  AKeywords.Add(S_FD_ConnParam_Common_ApplicationName + '=APP');
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLDriver.BuildODBCConnectString(const AConnectionDef: IFDStanConnectionDef): string;
begin
  Result := inherited BuildODBCConnectString(AConnectionDef);
{$IFDEF MSWINDOWS}
  if ((CompareText(ODBCDriver, C_2012) = 0) or
      (CompareText(ODBCDriver, C_2005) = 0) or
      (CompareText(ODBCDriver, C_2008) = 0)) and
     not (AConnectionDef.HasValue('DSN') or AConnectionDef.HasValue('FIL')) and
     (Pos('MARS_CONNECTION=', UpperCase(Result)) = 0) then
    Result := Result + ';MARS_Connection=yes';
{$ENDIF}
{$IFDEF POSIX}
  if not (AConnectionDef.HasValue('DSN') or AConnectionDef.HasValue('FIL')) and
     (Pos('TDS_VERSION=', UpperCase(Result)) = 0) then
    Result := Result + ';TDS_VERSION=8.0';
{$ENDIF}
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLDriver.GetConnParamCount(AKeys: TStrings): Integer;
begin
  Result := inherited GetConnParamCount(AKeys) + 16;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMSSQLDriver.GetConnParams(AKeys: TStrings; AIndex: Integer;
  var AName, AType, ADefVal, ACaption: String; var ALoginIndex: Integer);
var
  oList: TFDStringList;
  i: Integer;
  oODBCConn: TODBCConnection;
begin
  ALoginIndex := -1;
  ADefVal := '';
  if AIndex < inherited GetConnParamCount(AKeys) then begin
    inherited GetConnParams(AKeys, AIndex, AName, AType, ADefVal, ACaption, ALoginIndex);
    if AName = S_FD_ConnParam_Common_Database then
      ALoginIndex := 4;
  end
  else begin
    case AIndex - inherited GetConnParamCount(AKeys) of
    0:
      begin
        AName := S_FD_ConnParam_Common_Server;
        if GSQLServerList = '*' then begin
          GSQLServerList := '';
          Employ;
          oODBCConn := TODBCConnection.Create(ODBCEnvironment, Self);
          oList := TFDStringList.Create;
          try
            oODBCConn.SS_BROWSE_CONNECT := SQL_MORE_INFO_YES;
            oODBCConn.ListServers('DRIVER=' + TODBCLib.DecorateKeyValue(ODBCDriver), oList);
            for i := 0 to oList.Count - 1 do begin
              if GSQLServerList <> '' then
                GSQLServerList := GSQLServerList + ';';
              GSQLServerList := GSQLServerList + oList[i];
            end;
          finally
            FDFree(oList);
            FDFree(oODBCConn);
            Vacate;
          end;
          if GSQLServerList = '' then
            GSQLServerList := '@S';
        end;
        AType := GSQLServerList;
        ALoginIndex := 3;
      end;
    1:
      begin
        AName := S_FD_ConnParam_MSSQL_Network;
        AType := '@S';
      end;
    2:
      begin
        AName := S_FD_ConnParam_MSSQL_Address;
        AType := '@S';
      end;
    3:
      begin
        AName := S_FD_ConnParam_Common_OSAuthent;
        AType := '@Y';
        ALoginIndex := 2;
      end;
    4:
      begin
        AName := S_FD_ConnParam_MSSQL_MARS;
        AType := '@Y';
        ADefVal := S_FD_Yes;
      end;
    5:
      begin
        AName := S_FD_ConnParam_MSSQL_Workstation;
        AType := '@S';
      end;
    6:
      begin
        AName := S_FD_ConnParam_MSSQL_Language;
        AType := '@S';
      end;
    7:
      begin
        AName := S_FD_ConnParam_MSSQL_Encrypt;
        AType := '@Y';
      end;
    8:
      begin
        AName := S_FD_ConnParam_MSSQL_VariantFormat;
        AType := C_String + ';' + C_Binary;
        ADefVal := C_String;
      end;
    9:
      begin
        AName := S_FD_ConnParam_Common_ExtendedMetadata;
        AType := '@L';
        ADefVal := S_FD_False;
      end;
   10:
      begin
        AName := S_FD_ConnParam_Common_ApplicationName;
        AType := '@S';
      end;
   11:
      begin
        AName := S_FD_ConnParam_Common_MetaDefCatalog;
        AType := '@S';
      end;
   12:
      begin
        AName := S_FD_ConnParam_Common_MetaDefSchema;
        AType := '@S';
      end;
   13:
      begin
        AName := S_FD_ConnParam_Common_MetaCurCatalog;
        AType := '@S';
      end;
   14:
      begin
        AName := S_FD_ConnParam_Common_MetaCurSchema;
        AType := '@S';
      end;
   15:
      begin
        AName := S_FD_ConnParam_Common_MetaCaseIns;
        AType := '@L';
        ADefVal := S_FD_False;
      end;
    end;
    ACaption := AName;
  end;
end;

{-------------------------------------------------------------------------------}
{ TFDPhysMSSQLConnection                                                        }
{-------------------------------------------------------------------------------}
function TFDPhysMSSQLConnection.InternalCreateEvent(const AEventKind: String): TFDPhysEventAlerter;
{$IFDEF MSWINDOWS}
var
  oConnMeta: IFDPhysConnectionMetadata;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  CreateMetadata(oConnMeta);
  if (oConnMeta.ServerVersion >= svMSSQL2005) and (ODBCConnection.DriverKind = dkSQLNC) and
     (CompareText(AEventKind, S_FD_EventKind_MSSQL_Events) = 0) then
    Result := TFDPhysMSSQLEventAlerter.Create(Self, AEventKind)
  else
{$ENDIF}
    Result := nil;
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLConnection.InternalCreateCommandGenerator(
  const ACommand: IFDPhysCommand): TFDPhysCommandGenerator;
begin
  if ACommand <> nil then
    Result := TFDPhysMSSQLCommandGenerator.Create(ACommand)
  else
    Result := TFDPhysMSSQLCommandGenerator.Create(Self);
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLConnection.InternalCreateMetadata: TObject;
var
  iSrvVer, iClntVer: TFDVersion;
  iCase: SQLUSmallint;
begin
  if ODBCConnection <> nil then begin
    GetVersions(iSrvVer, iClntVer);
    iCase := ODBCConnection.IDENTIFIER_CASE;
    Result := TFDPhysMSSQLMetadata.Create(Self, FCatalogCaseSensitive,
      iCase in [SQL_IC_SENSITIVE, SQL_IC_MIXED], iCase in [SQL_IC_MIXED],
      ODBCConnection.IDENTIFIER_QUOTE_CHAR = '"',
      ODBCConnection.DriverKind in [dkSQLSrv, dkSQLNC], iSrvVer, iClntVer,
      GetKeywords, FExtendedMetadata);
  end
  else
    Result := TFDPhysMSSQLMetadata.Create(Self, False, True, True, True, False,
      0, 0, GetKeywords, FExtendedMetadata);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMSSQLConnection.GetStrsMaxSizes(AStrDataType: SQLSmallint;
  AFixedLen: Boolean; out ACharSize, AByteSize: Integer);
begin
  AByteSize := 8000;
  ACharSize := AByteSize;
  case AStrDataType of
  SQL_C_CHAR, SQL_C_BINARY:
    ;
  SQL_C_WCHAR:
    ACharSize := AByteSize div SizeOf(SQLWChar);
  else
    FDCapabilityNotSupported(Self, [S_FD_LPhys, S_FD_MSSQLId]);
  end;
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLConnection.GetExceptionClass: EODBCNativeExceptionClass;
begin
  Result := EMSSQLNativeException;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMSSQLConnection.CheckPasswordChange;
var
  oConnMeta: IFDPhysConnectionMetadata;
begin
  CreateMetadata(oConnMeta);
  if not ((oConnMeta.ServerVersion >= svMSSQL2005) and (ODBCConnection.DriverKind = dkSQLNC)) then
    FDCapabilityNotSupported(Self, [S_FD_LPhys, S_FD_MSSQLId]);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMSSQLConnection.SetupConnection;
var
  oPrevChanging: TNotifyEvent;
begin
  if ConnectionDef.NewPassword <> '' then begin
    CheckPasswordChange;
    ODBCConnection.SS_OLDPWD := ConnectionDef.Password;
    oPrevChanging := ConnectionDef.OnChanging;
    ConnectionDef.OnChanging := nil;
    ConnectionDef.Password := ConnectionDef.NewPassword;
    ConnectionDef.NewPassword := '';
    ConnectionDef.OnChanging := oPrevChanging;
  end;
  inherited SetupConnection;
  ODBCConnection.MSSQLVariantBinary :=
    CompareText(GetConnectionDef.AsString[S_FD_ConnParam_MSSQL_VariantFormat], C_Binary) = 0;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMSSQLConnection.InternalChangePassword(const AUserName,
  AOldPassword, ANewPassword: String);
var
  oConnDef: IFDStanConnectionDef;
  oConn: TODBCConnection;
begin
  CheckPasswordChange;
  FDCreateInterface(IFDStanConnectionDef, oConnDef);
  oConnDef.ParentDefinition := ConnectionDef;
  oConnDef.UserName := AUserName;
  oConnDef.Password := ANewPassword;
  oConn := TODBCConnection.Create(ODBCEnvironment, Self);
  try
    oConn.SS_OLDPWD := AOldPassword;
    oConn.Connect(TFDPhysODBCDriverBase(DriverObj).BuildODBCConnectString(oConnDef), False);
  finally
    FDFree(oConn);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMSSQLConnection.InternalSetMeta;
var
  oStmt: TODBCCommandStatement;
  oCol1, oCol2: TODBCColumn;
  oConnMeta: IFDPhysConnectionMetadata;
  iCount: SQLLen;
  sCollProp, sCurCatalog: String;
begin
  inherited InternalSetMeta;
  oStmt := TODBCCommandStatement.Create(ODBCConnection, Self);
  try
    CreateMetadata(oConnMeta);
    if FCurrentCatalog <> '' then
      sCurCatalog := FCurrentCatalog
    else
      sCurCatalog := InternalGetCurrentCatalog;
    if sCurCatalog <> '' then begin
      if oConnMeta.ServerVersion >= svMSSQL2005 then
        sCollProp := 'DATABASEPROPERTYEX('''
      else
        sCollProp := 'DATABASEPROPERTY(''';
      sCollProp := sCollProp + sCurCatalog + ''', ''COLLATION'')';
    end
    else
      sCollProp := 'SERVERPROPERTY(''COLLATION'')';
    if oConnMeta.ServerVersion >= svMSSQL2005 then
      oStmt.Open(1, 'SELECT SCHEMA_NAME(), ' + sCollProp)
    else
      oStmt.Open(1, 'SELECT USER_NAME(), ' + sCollProp);
    oCol1 := oStmt.AddCol(1, SQL_WVARCHAR, SQL_C_WCHAR, 128);
    oCol2 := oStmt.AddCol(2, SQL_WVARCHAR, SQL_C_WCHAR, 128);
    oStmt.Fetch(1);
    if FCurrentSchema = '' then begin
      FCurrentSchema := oCol1.AsString;
      // Under MSSQL 2005 the SCHEMA_NAME() may return NULL
      if FCurrentSchema = '' then
        FCurrentSchema := 'dbo';
    end;
    FCatalogCaseSensitive := Pos('_CI_', UpperCase(oCol2.AsString)) = 0;
    FExtendedMetadata := GetConnectionDef.AsBoolean[S_FD_ConnParam_Common_ExtendedMetadata];
    if FExtendedMetadata then begin
      oStmt.Unprepare;
      oStmt.Execute(1, 0, iCount, False, 'SET NO_BROWSETABLE ON');
    end;
  finally
    FDFree(oStmt);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMSSQLConnection.InternalAnalyzeSession(AMessages: TStrings);
var
  oConnMeta: IFDPhysConnectionMetadata;
begin
  inherited InternalAnalyzeSession(AMessages);
  CreateMetadata(oConnMeta);

  // 2. SQLNC 2008 fails with SQL2000 at some tests
  if (oConnMeta.ServerVersion >= svMSSQL2000) and (oConnMeta.ServerVersion < svMSSQL2005) and
     (oConnMeta.ClientVersion >= svMSSQL2008) then
    AMessages.Add('Warning: SQL NC 2008 is not full compatible with SQL Server 2000.');
end;

{$IFDEF MSWINDOWS}
{-------------------------------------------------------------------------------}
{ TFDPhysMSSQLEventThread                                                       }
{-------------------------------------------------------------------------------}
type
  TFDPhysMSSQLEventThread = class(TThread)
  private
    FAlerter: TFDPhysMSSQLEventAlerter;
  protected
    procedure Execute; override;
  public
    constructor Create(AAlerter: TFDPhysMSSQLEventAlerter);
    destructor Destroy; override;
  end;

{-------------------------------------------------------------------------------}
constructor TFDPhysMSSQLEventThread.Create(AAlerter: TFDPhysMSSQLEventAlerter);
begin
  inherited Create(False);
  FAlerter := AAlerter;
  FreeOnTerminate := True;
end;

{-------------------------------------------------------------------------------}
destructor TFDPhysMSSQLEventThread.Destroy;
begin
  FAlerter.FWaitThread := nil;
  inherited Destroy;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMSSQLEventThread.Execute;
begin
  FAlerter.InternalRegisterBase;
  while not Terminated and FAlerter.IsRunning do
    try
      if FAlerter.FMessageTab.Columns.Count = 0 then
        FAlerter.FWaitCommand.Define(FAlerter.FMessageTab);
      FAlerter.FWaitCommand.Open();
      if not Terminated then begin
        FAlerter.FWaitCommand.Fetch(FAlerter.FMessageTab, True);
        if FAlerter.FMessageTab.Rows.Count > 0 then begin
          FAlerter.DoFired;
          FAlerter.InternalRegisterBase;
        end;
      end;
    except
      on E: EFDDBEngineException do
        if E.Kind <> ekCmdAborted then begin
          Terminate;
          FAlerter.AbortJob;
        end;
    end;
end;

{-------------------------------------------------------------------------------}
{ TFDPhysMSSQLEventMessage                                                      }
{-------------------------------------------------------------------------------}
type
  TFDPhysMSSQLEventMessage = class(TFDPhysEventMessage)
  private
    FName,
    FMessage: String;
  public
    constructor Create(const AName, AMessage: String);
  end;

{-------------------------------------------------------------------------------}
constructor TFDPhysMSSQLEventMessage.Create(const AName, AMessage: String);
begin
  inherited Create;
  FName := AName;
  FMessage := AMessage;
end;

{-------------------------------------------------------------------------------}
{ TFDPhysMSSQLEventAlerter                                                      }
{-------------------------------------------------------------------------------}
// See:
// 1) http://msdn.microsoft.com/en-us/library/ms181122.aspx
// Creating a Query for Notification - supported SELECT's, connection settings, etc
// 2) http://msdn.microsoft.com/en-us/library/ms188323.aspx
// Understanding When Query Notifications Occur
// 3) http://www.code-magazine.com/article.aspx?quickid=0605061&page=1
// SQL Server 2005 Query Notifications Tell .NET 2.0 Apps When Critical Data Changes

// SERVICE=<service>
// QUEUE=<queue>
// CHANGE1=<message>;<SELECT>
// ...
// CHANGEn=<message>;<SELECT>
// <EV1>=<message>
// ...
// <EVn>=<message>

const
  C_Service = 'SERVICE';
  C_Queue = 'QUEUE';
  C_Change = 'CHANGE';
  C_Events = 'EVENTS';

procedure TFDPhysMSSQLEventAlerter.InternalAllocHandle;
var
  i: Integer;
  sName, sVal: String;
begin
  FWaitConnection := GetConnection.Clone;
  if FWaitConnection.State = csDisconnected then
    FWaitConnection.Open;
  FWaitConnection.CreateCommand(FWaitCommand);
  SetupCommand(FWaitCommand);
  FMessageTab := TFDDatSTable.Create;
  FServiceName := C_FD_SysNamePrefix + C_Service;
  FQueueName := C_FD_SysNamePrefix + C_Queue;
  FEventTabName := C_FD_SysNamePrefix + C_Events;
  FDropService := False;
  FDropQueue := False;
  FEventTab := False;
  for i := 0 to GetNames.Count - 1 do begin
    sName := FDNameFromIndex(GetNames(), i);
    sVal := FDValueFromIndex(GetNames(), i);
    if CompareText(sName, C_Service) = 0 then
      if sVal = '?' then begin
        FServiceName := C_FD_SysNamePrefix + C_Service + CreateClassID;
        FDropService := True;
      end
      else
        FServiceName := sVal
    else if CompareText(sName, C_Queue) = 0 then
      if sVal = '?' then begin
        FQueueName := C_FD_SysNamePrefix + C_Queue + CreateClassID;
        FDropQueue := True;
      end
      else
        FQueueName := sVal
    else if StrLIComp(PChar(sName), PChar(C_Change), Length(C_Change)) <> 0 then
      FEventTab := True;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMSSQLEventAlerter.InternalRegister;
var
  sSQL: String;
begin
  sSQL :=
    'IF OBJECT_ID(''' + FQueueName + ''') IS NULL BEGIN ' +
    '  CREATE QUEUE [' + FQueueName + ']; ' +
    'END; ' +
    'IF (SELECT COUNT(*) FROM sys.services WHERE NAME = ''' + FServiceName + ''') = 0 BEGIN ' +
    '  CREATE SERVICE [' + FServiceName + '] ON QUEUE [' + FQueueName + '] ' +
    '    ([http://schemas.microsoft.com/SQL/Notifications/PostQueryNotification]); ' +
    '  IF (SELECT COUNT(*) FROM sys.database_principals WHERE name = ''sql_dependency_subscriber'' AND type = ''R'') <> 0 BEGIN ' +
    '    GRANT SEND ON SERVICE::[' + FServiceName + '] TO sql_dependency_subscriber; ' +
    '  END; ' +
    'END;';
  if FEventTab then
    sSQL := sSQL +
      'IF OBJECT_ID(''' + FEventTabName + ''') IS NULL BEGIN ' +
      '  CREATE TABLE [' + FEventTabName + '] (Name NVARCHAR(128) NOT NULL PRIMARY KEY CLUSTERED, Value BIGINT); ' +
      'END; ';
  FWaitCommand.Prepare(sSQL);
  FWaitCommand.Execute;
  FWaitThread := TFDPhysMSSQLEventThread.Create(Self);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMSSQLEventAlerter.InternalRegisterBase;
var
  i, j: Integer;
  oStmt: TODBCStatementBase;
  sName, sVal: String;
  sMsg, sSQL: String;
begin
  for i := 0 to GetNames.Count - 1 do begin
    sName := FDNameFromIndex(GetNames(), i);
    sVal := FDValueFromIndex(GetNames(), i);
    if not ((CompareText(sName, C_Service) = 0) or (CompareText(sName, C_Queue) = 0)) then begin
      if StrLIComp(PChar(sName), PChar(C_Change), Length(C_Change)) = 0 then begin
        j := Pos(';', sVal);
        sMsg := Copy(sVal, 1, j - 1);
        sSQL := Copy(sVal, j + 1, MAXINT);
      end
      else begin
        sName := QuotedStr(sName);
        try
          FWaitCommand.Prepare('INSERT INTO [' + FEventTabName + '] VALUES(' + sName + ', 0)');
          FWaitCommand.Execute();
        except
          on E: EMSSQLNativeException do
            if E.Kind <> ekUKViolated then
              raise;
        end;
        sSQL := 'SELECT Value FROM [' + FEventTabName + '] WHERE Name = ' + sName;
      end;
      FWaitCommand.Prepare(sSQL);
      oStmt := TODBCStatementBase(FWaitCommand.CliObj);
      if GetOptions.Timeout > 0 then
        oStmt.SS_QUERYNOTIFICATION_TIMEOUT := GetOptions.Timeout div MSecsPerSec;
      oStmt.SS_QUERYNOTIFICATION_MSGTEXT := sMsg;
      oStmt.SS_QUERYNOTIFICATION_OPTIONS := 'SERVICE=' + FServiceName +
        ';LOCAL DATABASE=' + FWaitConnection.ConnectionDef.Database;
      FWaitCommand.Define(FMessageTab);
      FWaitCommand.Open;
      FWaitCommand.Disconnect;
      FMessageTab.Reset;
    end;
  end;

  sSQL := 'WAITFOR (RECEIVE message_body FROM [' + FQueueName + '])';
  if GetOptions.Timeout > 0 then
    sSQL := sSQL + ', TIMEOUT ' + IntToStr(GetOptions.Timeout);
  FWaitCommand.Prepare(sSQL);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMSSQLEventAlerter.DoFired;
const
  C_Start: String = '<qn:Message>';
  C_End: String = '</qn:Message>';
var
  i, i1, i2: Integer;
  sMsg: String;
begin
  for i := 0 to FMessageTab.Rows.Count - 1 do begin
    sMsg := FMessageTab.Rows[i].AsString['message_body'];
    i1 := Pos(C_Start, sMsg);
    i2 := Pos(C_End, sMsg);
    if (i1 > 0) and (i2 > 0) then begin
      i1 := i1 + Length(C_Start);
      i2 := i2 - 1;
      sMsg := Copy(sMsg, i1, i2 - i1 + 1);
    end;
    FMsgThread.EnqueueMsg(TFDPhysMSSQLEventMessage.Create(sMsg, ''));
  end;
  FMessageTab.Clear;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMSSQLEventAlerter.InternalHandle(AEventMessage: TFDPhysEventMessage);
var
  oMsg: TFDPhysMSSQLEventMessage;
begin
  if GetHandler <> nil then begin
    oMsg := TFDPhysMSSQLEventMessage(AEventMessage);
    GetHandler.HandleEvent(oMsg.FName, oMsg.FMessage);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMSSQLEventAlerter.InternalAbortJob;
begin
  FWaitThread.Terminate;
  FWaitCommand.AbortJob(True);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMSSQLEventAlerter.InternalUnregister;
var
  sSQL: String;
begin
  FWaitThread := nil;
  sSQL := '';
  if FDropService then
    sSQL := sSQL +
      'IF (SELECT COUNT(*) FROM sys.services WHERE NAME = ''' + FServiceName + ''') <> 0 BEGIN ' +
      '  DROP SERVICE [' + FServiceName + ']; ' +
      'END; ';
  if FDropQueue then
    sSQL := sSQL +
      'IF OBJECT_ID(''' + FQueueName + ''') IS NOT NULL BEGIN ' +
      '  DROP QUEUE [' + FQueueName + ']; ' +
      'END; ';
  if sSQL <> '' then begin
    FWaitCommand.Prepare(sSQL);
    FWaitCommand.Execute;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMSSQLEventAlerter.InternalReleaseHandle;
begin
  FWaitCommand := nil;
  FWaitConnection := nil;
  FDFreeAndNil(FMessageTab);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMSSQLEventAlerter.InternalSignal(const AEvent: String;
  const AArgument: Variant);
var
  oCmd: IFDPhysCommand;
begin
  if FEventTab then begin
    GetConnection.CreateCommand(oCmd);
    SetupCommand(oCmd);
    oCmd.Prepare('UPDATE [' + FEventTabName + '] SET Value = Value + 1 WHERE Name = ' + QuotedStr(AEvent));
    oCmd.Execute();
  end
  else
    FDCapabilityNotSupported(Self, [S_FD_LPhys, S_FD_MSSQLId]);
end;
{$ENDIF}

{-------------------------------------------------------------------------------}
function MSSQLNativeExceptionLoad(AStorage: TFDStorage): TObject;
begin
  Result := EMSSQLNativeException.Create;
  EMSSQLNativeException(Result).LoadFromStorage(AStorage);
end;

{-------------------------------------------------------------------------------}
initialization
  FDPhysManager();
  FDPhysManagerObj.RegisterDriverClass(TFDPhysMSSQLDriver);
  FDStorageManager().RegisterClass(EMSSQLNativeException, 'MSSQLNativeException',
    @MSSQLNativeExceptionLoad, @FDExceptionSave);

end.
