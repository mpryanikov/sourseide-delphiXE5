{*******************************************************}
{                                                       }
{               Delphi FireDAC Framework                }
{              FireDAC DBX 4 Bridge driver              }
{                                                       }
{ Copyright(c) 2004-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}
{$I FireDAC.inc}
{$IFDEF WIN32}
  {$HPPEMIT '#pragma link "FireDAC.Phys.TDBX.obj"'}
{$ELSE}
  {$HPPEMIT '#pragma link "FireDAC.Phys.TDBX.o"'}
{$ENDIF}

unit FireDAC.Phys.TDBX;

interface

uses
  System.Classes,
  FireDAC.Phys.TDBXBase;

type
  TFDPhysTDBXDriverLink = class;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or 
    pidiOSSimulator or pidiOSDevice or pidAndroid)]
  TFDPhysTDBXDriverLink = class(TFDPhysTDBXBaseDriverLink)
  protected
    function GetBaseDriverID: String; override;
  end;

{-------------------------------------------------------------------------------}
implementation

uses
  System.SysUtils, System.IniFiles, Data.DBXCommon,
  FireDAC.Stan.Intf, FireDAC.Stan.Consts, FireDAC.Stan.Util,
  FireDAC.Phys.Intf, FireDAC.Phys, FireDAC.Phys.SQLGenerator, FireDAC.Phys.Meta,
{$IFNDEF FireDAC_MOBILE}
    FireDAC.Phys.MSAccMeta, FireDAC.Phys.MSSQLMeta, FireDAC.Phys.MySQLMeta,
    FireDAC.Phys.OracleMeta, FireDAC.Phys.DB2Meta, FireDAC.Phys.ASAMeta,
    FireDAC.Phys.ADSMeta, FireDAC.Phys.PGMeta, FireDAC.Phys.NexusMeta,
{$ENDIF}
    FireDAC.Phys.TDBXMeta, FireDAC.Phys.IBMeta, FireDAC.Phys.SQLiteMeta;

type
  TFDPhysTDBXDriver = class;
  TFDPhysTDBXConnection = class;

  TDBXConnectionEx = TDBXConnection;
  TDBXMetaDataCommandsEx = TDBXMetaDataCommands;

  TFDPhysTDBXDriver = class(TFDPhysTDBXDriverBase)
  private
    function GetDriverParams(AKeys: TStrings): TStrings;
  protected
    class function GetBaseDriverID: String; override;
    function InternalCreateConnection(AConnHost: TFDPhysConnectionHost): TFDPhysConnection; override;
    function GetConnParamCount(AKeys: TStrings): Integer; override;
    procedure GetConnParams(AKeys: TStrings; AIndex: Integer;
      var AName, AType, ADefVal, ACaption: String; var ALoginIndex: Integer); override;
  end;

  TFDPhysTDBXConnection = class(TFDPhysTDBXConnectionBase)
  protected
    function InternalCreateMetadata: TObject; override;
    function InternalCreateCommandGenerator(const ACommand: IFDPhysCommand):
      TFDPhysCommandGenerator; override;
  end;

{-------------------------------------------------------------------------------}
{ TFDPhysTDBXDriverLink                                                         }
{-------------------------------------------------------------------------------}
function TFDPhysTDBXDriverLink.GetBaseDriverID: String;
begin
  Result := S_FD_TDBXId;
end;

{-------------------------------------------------------------------------------}
{ TFDPhysTDBXDriver                                                             }
{-------------------------------------------------------------------------------}
class function TFDPhysTDBXDriver.GetBaseDriverID: String;
begin
  Result := S_FD_TDBXId;
end;

{-------------------------------------------------------------------------------}
function TFDPhysTDBXDriver.InternalCreateConnection(
  AConnHost: TFDPhysConnectionHost): TFDPhysConnection;
begin
  Result := TFDPhysTDBXConnection.Create(Self, AConnHost);
end;

{-------------------------------------------------------------------------------}
function TFDPhysTDBXDriver.GetDriverParams(AKeys: TStrings): TStrings;
var
  sDrv, sName, sType, sDefVal, sCaption: String;
  oIniFile: TCustomIniFile;
  i, j, iLogin: Integer;
begin
  Result := TFDStringList.Create;
  try
    sDrv := FDUnquote(AKeys.Values[TDBXPropertyNames.DriverName]);
    if sDrv <> '' then begin
      oIniFile := TFDIniFile.Create(CfgFile);
      try
        oIniFile.ReadSectionValues(sDrv, Result);
        i := inherited GetConnParamCount(AKeys) - 1;
        while i >= 0 do begin
          inherited GetConnParams(AKeys, i, sName, sType, sDefVal, sCaption, iLogin);
          j := Result.IndexOfName(sName);
          if j <> -1 then
            Result.Delete(j);
          Dec(i);
        end;
      finally
        FDFree(oIniFile);
      end;
    end;
  except
    FDFree(Result);
    raise;
  end;
end;

{-------------------------------------------------------------------------------}
function TFDPhysTDBXDriver.GetConnParamCount(AKeys: TStrings): Integer;
var
  oList: TStrings;
begin
  Result := inherited GetConnParamCount(AKeys);
  oList := GetDriverParams(AKeys);
  try
    Result := Result + oList.Count + 6;
  finally
    FDFree(oList);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysTDBXDriver.GetConnParams(AKeys: TStrings; AIndex: Integer;
  var AName, AType, ADefVal, ACaption: String; var ALoginIndex: Integer);
var
  oList, oList2: TStrings;
  oIniFile: TCustomIniFile;
  j: TFDRDBMSKind;
begin
  ALoginIndex := -1;
  if AIndex < inherited GetConnParamCount(AKeys) then begin
    inherited GetConnParams(AKeys, AIndex, AName, AType, ADefVal, ACaption, ALoginIndex);
    if AName = S_FD_ConnParam_Common_Database then
      ALoginIndex := 2;
  end
  else begin
    AIndex := AIndex - inherited GetConnParamCount(AKeys);
    if AIndex = 0 then begin
      AName := TDBXPropertyNames.DriverName;
      oIniFile := TIniFile.Create(CfgFile);
      oList := TStringList.Create;
      oList.Delimiter := ';';
      oList.QuoteChar := '"';
      try
        oIniFile.ReadSection(TDBXPropertyNames.InstalledDrivers, oList);
        AType := oList.DelimitedText;
      finally
        oList.Free;
        oIniFile.Free;
      end;
      ADefVal := '';
      ACaption := AName;
    end
    else begin
      Dec(AIndex);
      oList := GetDriverParams(AKeys);
      try
        if AIndex < oList.Count then begin
          AName := FDNameFromIndex(oList, AIndex);
          ADefVal := FDValueFromIndex(oList, AIndex);
          oIniFile := TFDIniFile.Create(CfgFile);
          oList2 := TFDStringList.Create;
          oList2.Delimiter := ';';
          oList2.QuoteChar := '"';
          try
            oIniFile.ReadSection(AName, oList2);
            AType := oList2.DelimitedText;
          finally
            FDFree(oList2);
            FDFree(oIniFile);
          end;
        end
        else
          case AIndex - oList.Count of
          0:
            begin
              AName := S_FD_ConnParam_Common_MetaDefCatalog;
              AType := '@S';
              ADefVal := '';
            end;
          1:
            begin
              AName := S_FD_ConnParam_Common_MetaDefSchema;
              AType := '@S';
              ADefVal := '';
            end;
          2:
            begin
              AName := S_FD_ConnParam_Common_MetaCurCatalog;
              AType := '@S';
              ADefVal := '';
            end;
          3:
            begin
              AName := S_FD_ConnParam_Common_MetaCurSchema;
              AType := '@S';
              ADefVal := '';
            end;
          4:
            begin
              AName := S_FD_ConnParam_Common_RDBMSKind;
              AType := '';
              for j := Low(TFDRDBMSKind) to High(TFDRDBMSKind) do begin
                if AType <> '' then
                  AType := AType + ';';
                AType := AType + C_FD_PhysRDBMSKinds[j];
              end;
              ADefVal := '';
            end;
          end;
      finally
        FDFree(oList);
      end;
      ACaption := AName;
    end;
  end;
end;

{-------------------------------------------------------------------------------}
{ TFDPhysTDBXConnection                                                         }
{-------------------------------------------------------------------------------}
function TFDPhysTDBXConnection.InternalCreateCommandGenerator(
  const ACommand: IFDPhysCommand): TFDPhysCommandGenerator;
begin
  if ACommand <> nil then
    case GetRDBMSKindFromAlias of
{$IFNDEF FireDAC_MOBILE}
    mkOracle:    Result := TFDPhysOraCommandGenerator.Create(ACommand, False);
    mkMSSQL:     Result := TFDPhysMSSQLCommandGenerator.Create(ACommand);
    mkMSAccess:  Result := TFDPhysMSAccCommandGenerator.Create(ACommand);
    mkMySQL:     Result := TFDPhysMySQLCommandGenerator.Create(ACommand);
    mkDb2:       Result := TFDPhysDb2CommandGenerator.Create(ACommand);
    mkASA:       Result := TFDPhysASACommandGenerator.Create(ACommand);
    mkADS:       Result := TFDPhysADSCommandGenerator.Create(ACommand);
    mkPostgreSQL:Result := TFDPhysPgCommandGenerator.Create(ACommand);
    mkNexus:     Result := TFDPhysNexusCommandGenerator.Create(ACommand);
{$ENDIF}
    mkInterbase,
    mkFirebird:  Result := TFDPhysIBCommandGenerator.Create(ACommand, 0, ecANSI);
    mkSQLite:    Result := TFDPhysSQLiteCommandGenerator.Create(ACommand);
    else         Result := TFDPhysCommandGenerator.Create(ACommand);
    end
  else
    case GetRDBMSKindFromAlias of
{$IFNDEF FireDAC_MOBILE}
    mkOracle:    Result := TFDPhysOraCommandGenerator.Create(Self, False);
    mkMSSQL:     Result := TFDPhysMSSQLCommandGenerator.Create(Self);
    mkMSAccess:  Result := TFDPhysMSAccCommandGenerator.Create(Self);
    mkMySQL:     Result := TFDPhysMySQLCommandGenerator.Create(Self);
    mkDb2:       Result := TFDPhysDb2CommandGenerator.Create(Self);
    mkASA:       Result := TFDPhysASACommandGenerator.Create(Self);
    mkADS:       Result := TFDPhysADSCommandGenerator.Create(Self);
    mkPostgreSQL:Result := TFDPhysPgCommandGenerator.Create(Self);
    mkNexus:     Result := TFDPhysNexusCommandGenerator.Create(Self);
{$ENDIF}
    mkInterbase,
    mkFirebird:  Result := TFDPhysIBCommandGenerator.Create(Self, 0, ecANSI);
    mkSQLite:    Result := TFDPhysSQLiteCommandGenerator.Create(Self);
    else         Result := TFDPhysCommandGenerator.Create(Self);
    end
end;

{-------------------------------------------------------------------------------}
function TFDPhysTDBXConnection.InternalCreateMetadata: TObject;
var
  iSQLDialect: Integer;
  iClntVer: TFDVersion;
  eBrand: TFDPhysIBBrand;
begin
  if (DbxConnection <> nil) and (DbxConnection is TDBXConnectionEx) then
    iClntVer := FDVerStr2Int(TDBXConnectionEx(DbxConnection).ProductVersion)
  else
    iClntVer := 0;
  case GetRDBMSKindFromAlias of
{$IFNDEF FireDAC_MOBILE}
  mkOracle:    Result := TFDPhysOraMetadata.Create(Self, iClntVer, iClntVer, False);
  mkMSSQL:     Result := TFDPhysMSSQLMetadata.Create(Self, False, True, True, True,
    False, 0, iClntVer, GetKeywords, False);
  mkMSAccess:  Result := TFDPhysMSAccMetadata.Create(Self, 0, iClntVer, GetKeywords);
  mkMySQL:     Result := TFDPhysMySQLMetadata.Create(Self, False, 0, iClntVer,
    [nmCaseSens, nmDBApply], False);
  mkDb2:       Result := TFDPhysDb2Metadata.Create(Self, 0, iClntVer, GetKeywords);
  mkASA:       Result := TFDPhysASAMetadata.Create(Self, 0, iClntVer, GetKeywords);
  mkADS:       Result := TFDPhysADSMetadata.Create(Self, 0, iClntVer);
  mkPostgreSQL:Result := TFDPhysPgMetadata.Create(Self, 0, iClntVer, False, False, False);
  mkNexus:     Result := TFDPhysNexusMetadata.Create(Self, 0, iClntVer);
{$ENDIF}
  mkInterbase,
  mkFirebird:
    begin
      iSQLDialect := GetConnectionDef.AsInteger[S_FD_ConnParam_IB_SQLDialect];
      if iSQLDialect = 0 then
        iSQLDialect := 3;
      if CompareText(DriverName, 'firebird') = 0 then
        eBrand := ibFirebird
      else
        eBrand := ibInterbase;
      Result := TFDPhysIBMetadata.Create(Self, eBrand, 0, iClntVer, iSQLDialect, False);
    end;
  mkSQLite:    Result := TFDPhysSQLiteMetadata.Create(Self, sbSQLite, 0,
    iClntVer, False, False);
  else
    if (DbxConnection <> nil) and DbxConnection.IsOpen then
      Result := TFDPhysTDBXMetadata.Create(Self, GetKeywords)
    else
      Result := TFDPhysConnectionMetadata.Create(Self, 0, 0, False);
  end;
end;

{-------------------------------------------------------------------------------}
initialization
  FDPhysManager();
  try
    TDBXConnectionFactory.GetConnectionFactory;
    // if previous line passed OK, then TDBX is installed correctly
    FDPhysManagerObj.RegisterDriverClass(TFDPhysTDBXDriver);
  except
    // none
  end;

end.
