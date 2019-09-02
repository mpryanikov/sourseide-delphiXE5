{*******************************************************}
{                                                       }
{               Delphi FireDAC Framework                }
{              FireDAC ODBC Bridge driver               }
{                                                       }
{ Copyright(c) 2004-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}
{$I FireDAC.inc}
{$IFDEF WIN32}
  {$HPPEMIT '#pragma link "FireDAC.Phys.ODBC.obj"'}
{$ELSE}
  {$HPPEMIT '#pragma link "FireDAC.Phys.ODBC.o"'}
{$ENDIF}

unit FireDAC.Phys.ODBC;

interface

uses
  System.Classes,
  FireDAC.Phys, FireDAC.Phys.ODBCBase;

type
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32)]
  TFDPhysODBCDriverLink = class(TFDPhysODBCBaseDriverLink)
  protected
    function GetBaseDriverID: String; override;
  end;

{-------------------------------------------------------------------------------}
implementation

uses
  System.SysUtils,
  FireDAC.Stan.Intf, FireDAC.Stan.Consts, FireDAC.Stan.Util,
  FireDAC.Phys.Intf, FireDAC.Phys.SQLGenerator, FireDAC.Phys.Meta,
  FireDAC.Phys.ODBCCli, FireDAC.Phys.ODBCWrapper, FireDAC.Phys.ODBCMeta
{$IFDEF FireDAC_ODBC_ALLMETA}
    , FireDAC.Phys.MSSQLMeta, FireDAC.Phys.MSAccMeta, FireDAC.Phys.DB2Meta,
    FireDAC.Phys.OracleMeta, FireDAC.Phys.ASAMeta, FireDAC.Phys.MySQLMeta,
    FireDAC.Phys.ADSMeta, FireDAC.Phys.IBMeta, FireDAC.Phys.PGMeta,
    FireDAC.Phys.SQLiteMeta, FireDAC.Phys.NexusMeta
{$ENDIF}
    ;

type
  TFDPhysODBCDriver = class;
  TFDPhysODBCConnection = class;

  TFDPhysODBCDriver = class(TFDPhysODBCDriverBase)
  protected
    class function GetBaseDriverID: String; override;
    function InternalCreateConnection(AConnHost: TFDPhysConnectionHost): TFDPhysConnection; override;
    procedure GetODBCConnectStringKeywords(AKeywords: TStrings); override;
    function GetConnParamCount(AKeys: TStrings): Integer; override;
    procedure GetConnParams(AKeys: TStrings; AIndex: Integer; var AName, AType,
      ADefVal, ACaption: String; var ALoginIndex: Integer); override;
  end;

  TFDPhysODBCConnection = class(TFDPhysODBCConnectionBase)
  private
    FRdbmsKind: TFDRDBMSKind;
    FNumericFormat: TFDDataType;
    procedure UpdateRDBMSKind;
  protected
    function InternalCreateCommandGenerator(
      const ACommand: IFDPhysCommand): TFDPhysCommandGenerator; override;
    function InternalCreateMetadata: TObject; override;
    procedure InternalConnect; override;
    function GetNumType: TFDDataType; override;
  public
    constructor Create(ADriverObj: TFDPhysDriver; AConnHost: TFDPhysConnectionHost); override;
  end;

const
  S_FD_Binary = 'Binary';
  S_FD_String = 'String';

{-------------------------------------------------------------------------------}
{ TFDPhysODBCDriverLink                                                         }
{-------------------------------------------------------------------------------}
function TFDPhysODBCDriverLink.GetBaseDriverID: String;
begin
  Result := S_FD_ODBCId;
end;

{-------------------------------------------------------------------------------}
{ TFDPhysODBCDriver                                                             }
{-------------------------------------------------------------------------------}
class function TFDPhysODBCDriver.GetBaseDriverID: String;
begin
  Result := S_FD_ODBCId;
end;

{-------------------------------------------------------------------------------}
function TFDPhysODBCDriver.InternalCreateConnection(
  AConnHost: TFDPhysConnectionHost): TFDPhysConnection;
begin
  Result := TFDPhysODBCConnection.Create(Self, AConnHost);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysODBCDriver.GetODBCConnectStringKeywords(AKeywords: TStrings);
var
  i: Integer;
begin
  inherited GetODBCConnectStringKeywords(AKeywords);
  i := AKeywords.IndexOf('=DRIVER');
  if i >= 0 then
    AKeywords.Delete(i);
  i := AKeywords.IndexOf('DSN');
  if i >= 0 then
    AKeywords.Delete(i);
  AKeywords.Add(S_FD_ConnParam_ODBC_Driver + '=DRIVER');
  AKeywords.Add(S_FD_ConnParam_ODBC_DataSource + '=DSN');
  AKeywords.Add(S_FD_ConnParam_Common_Database + '=DBQ');
  AKeywords.Add(S_FD_ConnParam_Common_Database + '=DATABASE');
end;

{-------------------------------------------------------------------------------}
function TFDPhysODBCDriver.GetConnParamCount(AKeys: TStrings): Integer;
begin
  Result := inherited GetConnParamCount(AKeys) + 8;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysODBCDriver.GetConnParams(AKeys: TStrings; AIndex: Integer;
  var AName, AType, ADefVal, ACaption: String; var ALoginIndex: Integer);
var
  j: TFDRDBMSKind;
  i: Integer;
  oList: TFDStringList;
begin
  ALoginIndex := -1;
  ADefVal := '';
  if AIndex < inherited GetConnParamCount(AKeys) then
    inherited GetConnParams(AKeys, AIndex, AName, AType, ADefVal, ACaption, ALoginIndex)
  else begin
    case AIndex - inherited GetConnParamCount(AKeys) of
    0:
      begin
        Employ;
        oList := TFDStringList.Create;
        try
          AName := S_FD_ConnParam_ODBC_Driver;
          AType := '';
          ODBCEnvironment.GetDrivers(oList, True);
          for i := 0 to oList.Count - 1 do begin
            if AType <> '' then
              AType := AType + ';';
            AType := AType + oList[i];
          end;
        finally
          FDFree(oList);
          Vacate;
        end;
      end;
    1:
      begin
        Employ;
        oList := TFDStringList.Create;
        try
          AName := S_FD_ConnParam_ODBC_DataSource;
          AType := '';
          ODBCEnvironment.GetDSNs(oList, False, True);
          for i := 0 to oList.Count - 1 do begin
            if AType <> '' then
              AType := AType + ';';
            AType := AType + oList[i];
          end;
          ALoginIndex := 2;
        finally
          FDFree(oList);
          Vacate;
        end;
      end;
    2:
      begin
        AName := S_FD_ConnParam_ODBC_NumericFormat;
        AType := S_FD_Binary + ';' + S_FD_String;
        ADefVal := S_FD_Binary;
      end;
    3:
      begin
        AName := S_FD_ConnParam_Common_MetaDefCatalog;
        AType := '@S';
      end;
    4:
      begin
        AName := S_FD_ConnParam_Common_MetaDefSchema;
        AType := '@S';
      end;
    5:
      begin
        AName := S_FD_ConnParam_Common_MetaCurCatalog;
        AType := '@S';
      end;
    6:
      begin
        AName := S_FD_ConnParam_Common_MetaCurSchema;
        AType := '@S';
      end;
    7:
      begin
        AName := S_FD_ConnParam_Common_RDBMSKind;
        AType := '';
        for j := Low(TFDRDBMSKind) to High(TFDRDBMSKind) do begin
          if AType <> '' then
            AType := AType + ';';
          AType := AType + C_FD_PhysRDBMSKinds[j];
        end;
      end;
    end;
    ACaption := AName;
  end;
end;

{-------------------------------------------------------------------------------}
{ TFDPhysODBCConnection                                                         }
{-------------------------------------------------------------------------------}
constructor TFDPhysODBCConnection.Create(ADriverObj: TFDPhysDriver;
  AConnHost: TFDPhysConnectionHost);
begin
  inherited Create(ADriverObj, AConnHost);
  UpdateRDBMSKind;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysODBCConnection.UpdateRDBMSKind;
var
  i: TFDRDBMSKind;
  s: string;

  function Match(const AName: String): Boolean;
  begin
    Result := Copy(s, 1, Length(AName)) = AName;
  end;

begin
  if FRdbmsKind = mkUnknown then begin
    s := UpperCase(GetConnectionDef.AsString[S_FD_ConnParam_Common_RDBMSKind]);
    if s <> '' then
      for i := Low(TFDRDBMSKind) to High(TFDRDBMSKind) do
        if C_FD_PhysRDBMSKinds[i] = s then begin
          FRdbmsKind := i;
          Exit;
        end;
  end;
  if (FRdbmsKind = mkUnknown) and (ODBCConnection <> nil) then
    FRdbmsKind := ODBCConnection.RdbmsKind;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysODBCConnection.InternalConnect;
var
  sFmt: String;
begin
  inherited InternalConnect;
  FRdbmsKind := mkUnknown;
  sFmt := GetConnectionDef.AsString[S_FD_ConnParam_ODBC_NumericFormat];
  if CompareText(sFmt, S_FD_Binary) = 0 then
    FNumericFormat := dtBCD
  else
    FNumericFormat := dtAnsiString;
end;

{-------------------------------------------------------------------------------}
function TFDPhysODBCConnection.GetNumType: TFDDataType;
begin
  Result := FNumericFormat;
end;

{-------------------------------------------------------------------------------}
function TFDPhysODBCConnection.InternalCreateCommandGenerator(const ACommand:
  IFDPhysCommand): TFDPhysCommandGenerator;
begin
  UpdateRDBMSKind;
{$IFDEF FireDAC_ODBC_ALLMETA}
  if ACommand <> nil then
    case FRdbmsKind of
    mkOracle:    Result := TFDPhysOraCommandGenerator.Create(ACommand, False);
    mkMSSQL:     Result := TFDPhysMSSQLCommandGenerator.Create(ACommand);
    mkMSAccess:  Result := TFDPhysMSAccCommandGenerator.Create(ACommand);
    mkMySQL:     Result := TFDPhysMySQLCommandGenerator.Create(ACommand);
    mkDb2:       Result := TFDPhysDb2CommandGenerator.Create(ACommand);
    mkASA:       Result := TFDPhysASACommandGenerator.Create(ACommand);
    mkADS:       Result := TFDPhysADSCommandGenerator.Create(ACommand);
    mkInterbase,
    mkFirebird:  Result := TFDPhysIBCommandGenerator.Create(ACommand, 0, ecANSI);
    mkPostgreSQL:Result := TFDPhysPgCommandGenerator.Create(ACommand);
    mkSQLite:    Result := TFDPhysSQLiteCommandGenerator.Create(ACommand);
    mkNexus:     Result := TFDPhysNexusCommandGenerator.Create(ACommand);
    else         Result := TFDPhysCommandGenerator.Create(ACommand);
    end
  else
    case FRdbmsKind of
    mkOracle:    Result := TFDPhysOraCommandGenerator.Create(Self, False);
    mkMSSQL:     Result := TFDPhysMSSQLCommandGenerator.Create(Self);
    mkMSAccess:  Result := TFDPhysMSAccCommandGenerator.Create(Self);
    mkMySQL:     Result := TFDPhysMySQLCommandGenerator.Create(Self);
    mkDb2:       Result := TFDPhysDb2CommandGenerator.Create(Self);
    mkASA:       Result := TFDPhysASACommandGenerator.Create(Self);
    mkADS:       Result := TFDPhysADSCommandGenerator.Create(Self);
    mkInterbase,
    mkFirebird:  Result := TFDPhysIBCommandGenerator.Create(Self, 0, ecANSI);
    mkPostgreSQL:Result := TFDPhysPgCommandGenerator.Create(Self);
    mkSQLite:    Result := TFDPhysSQLiteCommandGenerator.Create(Self);
    mkNexus:     Result := TFDPhysNexusCommandGenerator.Create(Self);
    else         Result := TFDPhysCommandGenerator.Create(Self);
    end;
{$ELSE}
  if ACommand <> nil then
    Result := TFDPhysCommandGenerator.Create(ACommand)
  else
    Result := TFDPhysCommandGenerator.Create(Self);
{$ENDIF}
end;

{-------------------------------------------------------------------------------}
function TFDPhysODBCConnection.InternalCreateMetadata: TObject;
var
  iSrvVer, iClntVer: TFDVersion;
begin
  GetVersions(iSrvVer, iClntVer);
  UpdateRDBMSKind;
{$IFDEF FireDAC_ODBC_ALLMETA}
  case FRdbmsKind of
  mkOracle:    Result := TFDPhysOraMetadata.Create(Self, iSrvVer, iClntVer, False);
  mkMSSQL:     Result := TFDPhysMSSQLMetadata.Create(Self, False, True, True, True,
    (ODBCConnection <> nil) and (ODBCConnection.DriverKind in [dkSQLSrv, dkSQLNC]),
    iSrvVer, iClntVer, GetKeywords, False);
  mkMSAccess:  Result := TFDPhysMSAccMetadata.Create(Self, iSrvVer, iClntVer, GetKeywords);
  mkMySQL:     Result := TFDPhysMySQLMetadata.Create(Self, False, iSrvVer, iClntVer,
    [nmCaseSens, nmDBApply], False);
  mkDb2:       Result := TFDPhysDb2Metadata.Create(Self, iSrvVer, iClntVer, GetKeywords);
  mkASA:       Result := TFDPhysASAMetadata.Create(Self, iSrvVer, iClntVer, GetKeywords);
  mkADS:       Result := TFDPhysADSMetadata.Create(Self, iSrvVer, iClntVer);
  mkInterbase: Result := TFDPhysIBMetadata.Create(Self, ibInterbase, iSrvVer, iClntVer, 3, False);
  mkFirebird:  Result := TFDPhysIBMetadata.Create(Self, ibFirebird, iSrvVer, iClntVer, 3, False);
  mkPostgreSQL:Result := TFDPhysPgMetadata.Create(Self, iSrvVer, iClntVer, False, False, False);
  mkSQLite:    Result := TFDPhysSQLiteMetadata.Create(Self, sbSQLite, iSrvVer,
    iClntVer, False, False);
  mkNexus:     Result := TFDPhysNexusMetadata.Create(Self, iSrvVer, iClntVer);
  else         Result := nil;
  end;
{$ELSE}
  Result := nil;
{$ENDIF}
  if (ODBCConnection <> nil) and ODBCConnection.Connected then
    Result := TFDPhysODBCMetadata.Create(Self, iSrvVer, iClntVer, TFDPhysConnectionMetadata(Result))
  else if Result = nil then
    Result := TFDPhysConnectionMetadata.Create(Self, iSrvVer, iClntVer, False);
end;

{-------------------------------------------------------------------------------}
initialization
  FDPhysManager();
  FDPhysManagerObj.RegisterDriverClass(TFDPhysODBCDriver);

end.
