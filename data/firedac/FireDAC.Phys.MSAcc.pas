{*******************************************************}
{                                                       }
{               Delphi FireDAC Framework                }
{             FireDAC ODBC MS Access driver             }
{                                                       }
{ Copyright(c) 2004-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}
{$I FireDAC.inc}
{$IFDEF WIN32}
  {$HPPEMIT '#pragma link "FireDAC.Phys.MSAcc.obj"'}
{$ELSE}
  {$HPPEMIT '#pragma link "FireDAC.Phys.MSAcc.o"'}
{$ENDIF}

unit FireDAC.Phys.MSAcc;

interface

uses
  System.Classes,
  FireDAC.Stan.Error, 
  FireDAC.Phys, FireDAC.Phys.ODBCCli, FireDAC.Phys.ODBCWrapper, FireDAC.Phys.ODBCBase;

type
  EMSAccessNativeException = class;
  TFDPhysMSAccessDriverLink = class;
  TFDMSAccessService = class;

  EMSAccessNativeException = class(EODBCNativeException)
  public
    function AppendError(AHandle: TODBCHandle; ARecNum: SQLSmallint;
      const ASQLState: String; ANativeError: SQLInteger; const ADiagMessage,
      AResultMessage, ACommandText, AObject: String; AKind: TFDCommandExceptionKind;
      ACmdOffset, ARowIndex: Integer): TFDDBError; override;
  end;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  TFDPhysMSAccessDriverLink = class(TFDPhysODBCBaseDriverLink)
  protected
    function GetBaseDriverID: String; override;
  end;

  TFDMSAccessDBVersion = (avDefault, avAccess2, avAccess95, avAccess97,
    avAccess2000, avAccess2003, avAccess2007);
  TFDMSAccessAction = (aaCompactDB, aaRepairDB, aaCreateDB, aaDropDB);
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  TFDMSAccessService = class (TFDPhysODBCBaseService)
  private
    FAction: TFDMSAccessAction;
    FDatabase: String;
    FDestDatabase: String;
    FDBVersion: TFDMSAccessDBVersion;
    FSortOrder: String;
    FEncrypted: Boolean;
    FPassword: String;
    FResetPassword: Boolean;
    function GetDriverLink: TFDPhysMSAccessDriverLink;
    procedure SetDriverLink(const AValue: TFDPhysMSAccessDriverLink);
  protected
    procedure InternalExecute; override;
  public
    procedure CreateDB;
    procedure Drop;
    procedure Repair;
    procedure Compact;
  published
    property DriverLink: TFDPhysMSAccessDriverLink read GetDriverLink write SetDriverLink;
    property Database: String read FDatabase write FDatabase;
    property DestDatabase: String read FDestDatabase write FDestDatabase;
    property SortOrder: String read FSortOrder write FSortOrder;
    property DBVersion: TFDMSAccessDBVersion read FDBVersion write FDBVersion default avDefault;
    property Encrypted: Boolean read FEncrypted write FEncrypted default False;
    property Password: String read FPassword write FPassword;
    property ResetPassword: Boolean read FResetPassword write FResetPassword default False;
  end;

{-------------------------------------------------------------------------------}
implementation

uses
{$IFDEF MSWINDOWS}
  // Preventing from "Inline has not expanded"
  Winapi.Windows, System.Win.ComObj,
{$ENDIF}
{$IFDEF POSIX}
  Posix.UniStd,
{$ENDIF}
  System.SysUtils, System.Variants,
  FireDAC.Stan.Consts, FireDAC.Stan.ResStrs, FireDAC.Stan.Intf, FireDAC.Stan.Util, FireDAC.Stan.Storage,
  FireDAC.Phys.Intf, FireDAC.Phys.SQLGenerator, FireDAC.Phys.MSAccMeta;

type
  TFDPhysMSAccessDriver = class;
  TFDPhysMSAccessConnection = class;

  TFDPhysMSAccessDriver = class(TFDPhysODBCDriverBase)
  protected
    class function GetBaseDriverID: String; override;
    procedure InternalLoad; override;
    function InternalCreateConnection(AConnHost: TFDPhysConnectionHost): TFDPhysConnection; override;
    procedure GetODBCConnectStringKeywords(AKeywords: TStrings); override;
    function GetConnParamCount(AKeys: TStrings): Integer; override;
    procedure GetConnParams(AKeys: TStrings; AIndex: Integer; var AName,
      AType, ADefVal, ACaption: String; var ALoginIndex: Integer); override;
  end;

  TFDPhysMSAccessConnection = class(TFDPhysODBCConnectionBase)
  private
    FStringFormat: TFDDataType;
  protected
    function InternalCreateCommandGenerator(const ACommand:
      IFDPhysCommand): TFDPhysCommandGenerator; override;
    function InternalCreateMetadata: TObject; override;
    procedure InternalConnect; override;
    procedure GetStrsMaxSizes(AStrDataType: SQLSmallint; AFixedLen: Boolean;
      out ACharSize, AByteSize: Integer); override;
    function GetStrsType: TFDDataType; override;
    function GetExceptionClass: EODBCNativeExceptionClass; override;
  end;

const
  S_FD_Choose = 'Choose';
  S_FD_Unicode = 'Unicode';
  S_FD_ANSI = 'ANSI';

{-------------------------------------------------------------------------------}
{ EMSAccessNativeException                                                      }
{-------------------------------------------------------------------------------}
function EMSAccessNativeException.AppendError(AHandle: TODBCHandle;
  ARecNum: SQLSmallint; const ASQLState: String; ANativeError: SQLInteger;
  const ADiagMessage, AResultMessage, ACommandText, AObject: String;
  AKind: TFDCommandExceptionKind; ACmdOffset, ARowIndex: Integer): TFDDBError;
var
  sObj: String;

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
  // following is not supported by MSAccess:
  // ekNoDataFound
  // ekUserPwdExpired
  // ekUserPwdWillExpire
  sObj := AObject;
  case ANativeError of
  -1102:
    AKind := ekRecordLocked;
  -1605:
    AKind := ekUKViolated;
  -1613:
    AKind := ekFKViolated;
  -1305:
    begin
      AKind := ekObjNotExists;
      // first 'xxxx' - object name
      ExtractObjName;
    end;
  -1905:
    AKind := ekUserPwdInvalid;
  end;
  Result := inherited AppendError(AHandle, ARecNum, ASQLState, ANativeError,
    ADiagMessage, AResultMessage, ACommandText, sObj, AKind, ACmdOffset, ARowIndex);
end;

{-------------------------------------------------------------------------------}
{ TFDPhysMSAccessDriverLink                                                     }
{-------------------------------------------------------------------------------}
function TFDPhysMSAccessDriverLink.GetBaseDriverID: String;
begin
  Result := S_FD_MSAccId;
end;

{-------------------------------------------------------------------------------}
{ TFDMSAccessService                                                            }
{-------------------------------------------------------------------------------}
function TFDMSAccessService.GetDriverLink: TFDPhysMSAccessDriverLink;
begin
  Result := inherited DriverLink as TFDPhysMSAccessDriverLink;
end;

{-------------------------------------------------------------------------------}
procedure TFDMSAccessService.SetDriverLink(const AValue: TFDPhysMSAccessDriverLink);
begin
  inherited DriverLink := AValue;
end;

{-------------------------------------------------------------------------------}
procedure TFDMSAccessService.CreateDB;
begin
  FAction := aaCreateDB;
  Execute;
end;

{-------------------------------------------------------------------------------}
procedure TFDMSAccessService.Drop;
begin
  FAction := aaDropDB;
  Execute;
end;

{-------------------------------------------------------------------------------}
procedure TFDMSAccessService.Compact;
begin
  FAction := aaCompactDB;
  Execute;
end;

{-------------------------------------------------------------------------------}
procedure TFDMSAccessService.Repair;
begin
  FAction := aaRepairDB;
  Execute;
end;

{-------------------------------------------------------------------------------}
procedure TFDMSAccessService.InternalExecute;
var
  sStr, sDrv, sDb: String;
  lSort, lLastDrv: Boolean;

  function NormFileName(AName: String): String;
  begin
    Result := FDExpandStr(AName);
    if (Pos(' ', Result) > 0) and (Result <> '') and (Result[1] <> '"') then
      Result := '"' + Result + '"';
  end;

  procedure DeleteDatabase(const AName: String);
  var
    sFile: String;
  begin
    sFile := FDExpandStr(AName);
    System.SysUtils.DeleteFile(sFile);
    System.SysUtils.DeleteFile(ChangeFileExt(sFile, '.ldb'));
    System.SysUtils.DeleteFile(ChangeFileExt(sFile, '.laccdb'));
  end;

{$IFDEF MSWINDOWS}
  function RenameDatabase(const AOldName, ANewName: String): Boolean;
  var
    sOldFile, sNewFile, sLdbFile: String;
  begin
    sOldFile := FDExpandStr(AOldName);
    sNewFile := FDExpandStr(ANewName);
    Result := RenameFile(sOldFile, sNewFile);
    if Result then begin
      sLdbFile := ChangeFileExt(sOldFile, '.ldb');
      if FileExists(sLdbFile) then
        RenameFile(sLdbFile, ChangeFileExt(sNewFile, '.ldb'));
      sLdbFile := ChangeFileExt(sOldFile, '.laccdb');
      if FileExists(sLdbFile) then
        RenameFile(sLdbFile, ChangeFileExt(sNewFile, '.laccdb'));
    end;
  end;

  function GenTempFileName(const AName: String): String;
  var
    sPath: String;
  begin
    sPath := ExtractFilePath(FDExpandStr(AName));
    SetLength(Result, MAX_PATH);
    GetTempFileName(PChar(sPath), 'FD', 0, PChar(Result));
    Result := PChar(Result);
  end;

  procedure ProcessOLEException(AExc: EOleSysError);
  var
    sMsg: String;
    iCode: Integer;
    oExc: EFDDBEngineException;
    oErr: TFDDBError;
  begin
    sMsg := AExc.Message;
    iCode := er_FD_AccUnknownOleError;
    if AExc is EOLEException then begin
      if Pos('Class not registered', sMsg) > 0 then
        iCode := er_FD_AccClassNotRegistered
      else if Pos('Unrecognized database format', sMsg) > 0 then
        iCode := er_FD_AccUnrecognizedDbFormat
      else if Pos('Not a valid password', sMsg) > 0 then
        iCode := er_FD_AccNotValidPassword;
    end
    else
      if Pos('Class not registered', sMsg) > 0 then
        iCode := er_FD_AccSysClassNotRegistered;

    oExc := ADDBEngineExceptionCreate(EMSAccessNativeException, iCode, []);
    oErr := TFDDBError.Create;
    oErr.Message := sMsg;
    oExc.Append(oErr);
    FDException(Self, oExc{$IFDEF FireDAC_Monitor}, False{$ENDIF});
  end;

  procedure CompactRepair;
  var
    vJro: Variant;
    sSource, sDest, sProvider, sDestDatabase: String;
    iEngine: Integer;
    lRename: Boolean;
  begin
    if not FileExists(Database) then
      FDException(Self, ADDBEngineExceptionCreate(EMSAccessNativeException,
        er_FD_AccDbNotExists, [Database]){$IFDEF FireDAC_Monitor}, False{$ENDIF});
    try
      vJro := CreateOLEObject('JRO.JetEngine');
      try
        lRename := (DestDatabase = '') or (DestDatabase = Database);
        if lRename then
          sDestDatabase := GenTempFileName(Database)
        else
          sDestDatabase := DestDatabase;
        DeleteDatabase(sDestDatabase);
        sSource := 'Provider=%s;Data Source=%s;Jet OLEDB:Engine Type=%d';
        sDest := sSource;
        if Password <> '' then begin
          sSource := sSource + ';Jet OLEDB:Database Password=' + Password;
          if not ResetPassword then
            sDest := sSource;
        end;
        if lSort then
          sDest := sDest + ';LocaleIdentifier=' + SortOrder;
        if Encrypted then
          sDest := sDest + ';Jet OLEDB:Encrypt Database=True';
        iEngine := 5;
        case DBVersion of
        avDefault:
          begin
            if sDrv = 'Microsoft Access Driver (*.mdb, *.accdb)' then
              sProvider := 'Microsoft.ACE.OLEDB.12.0'
            else
              sProvider := 'Microsoft.Jet.OLEDB.4.0';
          end;
        avAccess2:
          begin
            sProvider := 'Microsoft.Jet.OLEDB.4.0';
            iEngine := 3;
          end;
        avAccess95,
        avAccess97:
          begin
            sProvider := 'Microsoft.Jet.OLEDB.4.0';
            iEngine := 4;
          end;
        avAccess2000,
        avAccess2003:
          begin
            sProvider := 'Microsoft.Jet.OLEDB.4.0';
            iEngine := 5;
          end;
        avAccess2007:
          begin
            sProvider := 'Microsoft.ACE.OLEDB.12.0';
            iEngine := 5;
          end;
        end;
        vJro.CompactDatabase(Format(sSource, [sProvider, FDExpandStr(Database),
          iEngine]), Format(sDest, [sProvider, FDExpandStr(sDestDatabase), iEngine]));
        if lRename then begin
          DeleteDatabase(Database);
          if RenameDatabase(sDestDatabase, Database) then
            DeleteDatabase(sDestDatabase);
        end;
      finally
        vJro := Unassigned;
      end;
    except
      on E: EOleSysError do
        ProcessOLEException(EOleSysError(E))
    end;
  end;
{$ENDIF}

begin
  sDrv := (DriverLink.DriverIntf as IFDPhysODBCDriver).ODBCDriver;
  case FAction of
  aaCompactDB:
{$IFDEF MSWINDOWS}
    CompactRepair;
{$ELSE}
    begin
      sDb := NormFileName(Database);
      sStr := 'COMPACT_DB=' + sDb + ' ';
      if DestDatabase = '' then
        sStr := sStr + sDb
      else
        sStr := sStr + NormFileName(DestDatabase);
      if SortOrder <> '' then
        sStr := sStr + ' ' + SortOrder;
      ExecuteBase(ODBC_ADD_DSN, sDrv, sStr);
    end;
{$ENDIF}
  aaRepairDB:
{$IFDEF MSWINDOWS}
    CompactRepair;
{$ELSE}
    begin
      sStr := 'REPAIR_DB=' + NormFileName(Database);
      ExecuteBase(ODBC_ADD_DSN, sDrv, sStr);
    end;
{$ENDIF}
  aaCreateDB:
    begin
      sStr := 'CREATE_DB';
      lLastDrv := sDrv = 'Microsoft Access Driver (*.mdb, *.accdb)';
      case DBVersion of
      avDefault:    ;
      avAccess2:    sStr := sStr + 'V2';
      avAccess95,
      avAccess97:   sStr := sStr + 'V3';
      avAccess2000: sStr := sStr + 'V4';
      avAccess2003,
      avAccess2007:
        begin
          sDrv := 'Microsoft Access Driver (*.mdb, *.accdb)';
          lLastDrv := True;
        end;
      end;
      sStr := sStr + '=' + NormFileName(Database);
      lSort := SortOrder <> '';
      if lSort and (Pos('0x', SortOrder) = 0) then begin
        sStr := sStr + ' ' + SortOrder;
{$IFDEF MSWINDOWS}
        lSort := False;
{$ENDIF}
      end;
      if Encrypted and not lLastDrv then
        sStr := sStr + ' ENCRYPT';
      ExecuteBase(ODBC_ADD_DSN, sDrv, sStr);
{$IFDEF MSWINDOWS}
      if (Password <> '') or lSort or (Encrypted and lLastDrv) then begin
        sDb := DestDatabase;
        DestDatabase := '';
        try
          CompactRepair;
        finally
          DestDatabase := sDb;
        end;
      end;
{$ENDIF}
    end;
  aaDropDB:
    DeleteDatabase(Database);
  end;
end;

{-------------------------------------------------------------------------------}
{ TFDPhysMSAccessDriver                                                         }
{-------------------------------------------------------------------------------}
class function TFDPhysMSAccessDriver.GetBaseDriverID: String;
begin
  Result := S_FD_MSAccId;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMSAccessDriver.InternalLoad;
begin
  ODBCAdvanced := 'ExtendedAnsiSQL=1';
  inherited InternalLoad;
  if ODBCDriver = '' then
    ODBCDriver := FindBestDriver([
      'Microsoft Access Driver (*.mdb, *.accdb)',
      'Microsoft Access Driver (*.mdb)',
      'Driver do Microsoft Access (*.mdb)',
      'Microsoft Access-Treiber (*.mdb)']);
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSAccessDriver.InternalCreateConnection(
  AConnHost: TFDPhysConnectionHost): TFDPhysConnection;
begin
  Result := TFDPhysMSAccessConnection.Create(Self, AConnHost);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMSAccessDriver.GetODBCConnectStringKeywords(AKeywords: TStrings);
var
  i: Integer;
begin
  inherited GetODBCConnectStringKeywords(AKeywords);
  i := AKeywords.IndexOfName(S_FD_ConnParam_Common_Password);
  if i <> -1 then
    AKeywords[i] := S_FD_ConnParam_Common_Password + '=PWD*';
  AKeywords.Add(S_FD_ConnParam_Common_Database + '=DBQ*');
  AKeywords.Add(S_FD_ConnParam_MSAcc_SysDB);
  AKeywords.Add(S_FD_ConnParam_MSAcc_RO);
  AKeywords.Add('=ExtendedAnsiSQL');
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSAccessDriver.GetConnParamCount(AKeys: TStrings): Integer;
begin
  Result := inherited GetConnParamCount(AKeys) + 3;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMSAccessDriver.GetConnParams(AKeys: TStrings; AIndex: Integer;
  var AName, AType, ADefVal, ACaption: String; var ALoginIndex: Integer);
begin
  ALoginIndex := -1;
  ADefVal := '';
  if AIndex < inherited GetConnParamCount(AKeys) then begin
    inherited GetConnParams(AKeys, AIndex, AName, AType, ADefVal, ACaption, ALoginIndex);
    if AName = S_FD_ConnParam_Common_Database then begin
      ALoginIndex := 2;
      AType := '@F:Microsoft Access Database|*.mdb;*.accdb';
    end;
  end
  else begin
    case AIndex - inherited GetConnParamCount(AKeys) of
    0:
      begin
        AName := S_FD_ConnParam_MSAcc_SysDB;
        AType := '@S';
      end;
    1:
      begin
        AName := S_FD_ConnParam_MSAcc_RO;
        AType := '@L';
      end;
    2:
      begin
        AName := S_FD_ConnParam_MSAcc_StringFormat;
        AType := S_FD_Choose + ';' + S_FD_Unicode + ';' + S_FD_ANSI;
        ADefVal := S_FD_Choose;
      end;
    end;
    ACaption := AName;
  end;
end;

{-------------------------------------------------------------------------------}
{ TFDPhysMSAccessConnection                                                     }
{-------------------------------------------------------------------------------}
function TFDPhysMSAccessConnection.InternalCreateCommandGenerator(
  const ACommand: IFDPhysCommand): TFDPhysCommandGenerator;
begin
  if ACommand <> nil then
    Result := TFDPhysMSAccCommandGenerator.Create(ACommand)
  else
    Result := TFDPhysMSAccCommandGenerator.Create(Self);
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSAccessConnection.InternalCreateMetadata: TObject;
var
  iSrvVer, iClntVer: TFDVersion;
begin
  GetVersions(iSrvVer, iClntVer);
  Result := TFDPhysMSAccMetadata.Create(Self, iSrvVer, iClntVer, GetKeywords);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMSAccessConnection.InternalConnect;
var
  s: String;
begin
  inherited InternalConnect;
  FStringFormat := dtUnknown;
  s := GetConnectionDef.AsString[S_FD_ConnParam_MSAcc_StringFormat];
  if CompareText(s, S_FD_Unicode) = 0 then
    FStringFormat := dtWideString
  else if CompareText(s, S_FD_ANSI) = 0 then
    FStringFormat := dtAnsiString;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMSAccessConnection.GetStrsMaxSizes(AStrDataType: SQLSmallint;
  AFixedLen: Boolean; out ACharSize, AByteSize: Integer);
begin
  case AStrDataType of
  SQL_C_CHAR, SQL_C_WCHAR:
    ACharSize := 255;
  SQL_C_BINARY:
    ACharSize := 510;
  else
    FDCapabilityNotSupported(Self, [S_FD_LPhys, S_FD_MSAccId]);
  end;
  AByteSize := ACharSize;
  if AStrDataType = SQL_C_WCHAR then
    AByteSize := AByteSize * SizeOf(SQLWChar);
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSAccessConnection.GetStrsType: TFDDataType;
begin
  Result := FStringFormat;
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSAccessConnection.GetExceptionClass: EODBCNativeExceptionClass;
begin
  Result := EMSAccessNativeException;
end;

{-------------------------------------------------------------------------------}
function MSAccessNativeExceptionLoad(AStorage: TFDStorage): TObject;
begin
  Result := EMSAccessNativeException.Create;
  EMSAccessNativeException(Result).LoadFromStorage(AStorage);
end;

{-------------------------------------------------------------------------------}
initialization
  FDPhysManager();
  FDPhysManagerObj.RegisterDriverClass(TFDPhysMSAccessDriver);
  FDStorageManager().RegisterClass(EMSAccessNativeException, 'MSAccessNativeException',
    @MSAccessNativeExceptionLoad, @FDExceptionSave);

end.
