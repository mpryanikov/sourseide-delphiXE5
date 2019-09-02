{*******************************************************}
{                                                       }
{               Delphi FireDAC Framework                }
{               FireDAC ADO UDL Importer                }
{                                                       }
{ Copyright(c) 2004-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

(*
UDL importer supports classic ADO connection string and UDL files for
the following DBMS's: Oracle, MSSQL, MSAccess, Interbase, IBM DB2, Sybase
SQL Anywhere. For example:

  [oledb]
  ; Everything after this line is an OLE DB initstring
  Provider=MSDASQL.1;Persist Security Info=False;Data Source=SqlEx2008

FireDAC specific connection definition parameters may be specified in
"Extended Properties" attribute with "FD$" prefix. For example:

  [oledb]
  ; Everything after this line is an OLE DB initstring
  Provider=MSDASQL.1;Persist Security Info=False;Data Source=SqlEx2008;Extended Properties="FD$Database=Northwind;FD$User_Name=sa"

To specify ADO-incompatible connection using ADO connection string format,
use Provider=FD$DRIVER_ID_XXXX provider attribute. Where XXXX is the FireDAC
driver ID. All connection parameters then must be specified as described
above. For example:

  [oledb]
  ; Everything after this line is an OLE DB initstring
  Provider=FD$DRIVER_ID_MSSQL;Extended Properties="FD$Server=127.0.0.1;FD$Database=Northwind;FD$User_Name=sa"

  [oledb]
  ; Everything after this line is an OLE DB initstring
  Provider=FD$DRIVER_ID_SQLite;Extended Properties="FD$Database=$(FDHOME)\DB\Data\FDDemo.sdb"
*)

{$I FireDAC.inc}

unit FireDAC.Import.ADO;

interface

uses
  System.Classes,
  FireDAC.Stan.Intf, FireDAC.Stan.Consts, FireDAC.Stan.Option;

type
  TFDUDLImportOverwriteEvent = procedure (const AName: String;
    var AOverwrite: Integer) of object;
  TFDUDLImportBeforeWriteOptions = procedure (const ADriverID: String;
    AFormatOptions: TFDFormatOptions; AUpdateOptions: TFDUpdateOptions;
    AFetchOptions: TFDFetchOptions; AResourceOptions: TFDResourceOptions) of object;
  TFDUDLImportBeforeWriteSettings = procedure (AParams: TStrings) of object;
  TFDADOUDLImporter = class(TObject)
  private
    FSrcParams: TStrings;
    FDestParams: TStrings;
    FOpts: IFDStanOptions;
    FFetch: TFDFetchOptions;
    FFmt: TFDFormatOptions;
    FUpd: TFDUpdateOptions;
    FRes: TFDResourceOptions;
    FOverwriteDefs: Boolean;
    FUDLToImport: TStrings;
    FOnOverwrite: TFDUDLImportOverwriteEvent;
    FOnBeforeWriteOptions: TFDUDLImportBeforeWriteOptions;
    FOnBeforeWriteSettings: TFDUDLImportBeforeWriteSettings;
    FMapDataTypes: Boolean;
    FEnableBCD: Boolean;
    FForceWideString: Boolean;
    FMarkDefsPersistent: Boolean;
    FMaxBcdPrecision: Integer;
    FMaxBcdScale: Integer;
    FBCDType: TFDDataType;
    procedure FillSrcParams(const AConnStr: String);
    function GetSrcBool(const AName: String; var AResult: Boolean): Boolean;
    function GetSrcStr(const AName: String; var AResult: String): Boolean;
    procedure MergeExtendedProps;
    procedure ImportSQLCommon;
    procedure ImportOracle(AImportMode: Boolean);
    procedure ImportIB(AImportMode: Boolean);
    procedure ImportMSSQL(AImportMode: Boolean);
    procedure ImportMSAccess(AImportMode: Boolean);
    procedure ImportDB2(AImportMode: Boolean);
    procedure ImportASA(AImportMode: Boolean);
    procedure ImportODBC(AImportMode: Boolean);
    procedure ImportOther(AImportMode: Boolean; const AProvider: String);
    function GetRDBMSKind(out AProvider: String; out AADDirect: Boolean): TFDRDBMSKind; overload;
    function GetADDirectDriverID(const AProvider: String): String;
  public
    constructor Create;
    destructor Destroy; override;
    // returns True when AConnStr is ADO connection string
    function IsADOConnectingString(const AConnStr: String): Boolean;
    // returns provider name and FireDAC DBMS kind from ADO connection string
    function GetRDBMSKind(const AConnStr: String; out AProvider: String;
      out AADDirect: Boolean): TFDRDBMSKind; overload;
    // makes specified connection definition compatible by data types mapping with ADO
    procedure MakeADOCompatible(const AConnectionDef: IFDStanConnectionDef);
    // converts connection string into connection definition with name ADefName and
    // store it into ADefs list
    function ImportConnStr(const AConnStr: String; ADefs: IFDStanConnectionDefs;
      const ADefName: String = ''): IFDStanConnectionDef;
    // converts UDL file into connection definition with name ADefName and
    // store it into ADefs list
    function ImportUDL(const AFileName: String; ADefs: IFDStanConnectionDefs;
      const ADefName: String = ''): IFDStanConnectionDef;
    // imports all UDL's from path AUDLPath into ADefs list
    procedure ImportAllUDLs(const AUDLPath: String; ADefs: IFDStanConnectionDefs);
    // restricts list of imported UDL's to specified ones
    property UDLToImport: TStrings read FUDLToImport;
    // Enables / disables data type mapping at connection string import.
    property MapDataTypes: Boolean read FMapDataTypes write FMapDataTypes;
    // Controls data type mapping at connection string import:
    // see TADODataSet.EnableBCD
    property EnableBCD: Boolean read FEnableBCD write FEnableBCD;
    // Controls data type mapping at connection string import:
    // force all ftString to ftWideString
    property ForceWideString: Boolean read FForceWideString write FForceWideString;
    // If True, then connection definition in the list may be overwritten
    property OverwriteDefs: Boolean read FOverwriteDefs write FOverwriteDefs;
    // If True, then all imported connection definition will be marked as persistent
    property MarkDefsPersistent: Boolean read FMarkDefsPersistent
      write FMarkDefsPersistent;
    property OnOverwrite: TFDUDLImportOverwriteEvent read FOnOverwrite
      write FOnOverwrite;
    property OnBeforeWriteOptions: TFDUDLImportBeforeWriteOptions
      read FOnBeforeWriteOptions write FOnBeforeWriteOptions;
    property OnBeforeWriteSettings: TFDUDLImportBeforeWriteSettings
      read FOnBeforeWriteSettings write FOnBeforeWriteSettings;
  end;

implementation

uses
  System.SysUtils,
  FireDAC.Stan.Factory, FireDAC.Stan.ResStrs, FireDAC.Stan.Error, FireDAC.Stan.Util,
  FireDAC.Phys.Intf, System.TypInfo;

const
  C_FDDirect = 'FD$DRIVER_ID';

{-------------------------------------------------------------------------------}
constructor TFDADOUDLImporter.Create;
begin
  inherited Create;
  FUDLToImport := TFDStringList.Create;
  TFDStringList(FUDLToImport).CaseSensitive := False;
  TFDStringList(FUDLToImport).Sorted := True;
  TFDStringList(FUDLToImport).Duplicates := dupAccept;

  FSrcParams := TFDStringList.Create;
  TFDStringList(FSrcParams).CaseSensitive := False;
  TFDStringList(FSrcParams).Duplicates := dupAccept;

  FDestParams := TFDStringList.Create;
  FOpts := TFDOptionsContainer.Create(nil, TFDFetchOptions, TFDUpdateOptions,
    TFDResourceOptions, nil);
  FFetch := FOpts.FetchOptions;
  FFmt := FOpts.FormatOptions;
  FUpd := FOpts.UpdateOptions;
  FRes := FOpts.ResourceOptions;
  FMapDataTypes := True;
  FMaxBcdPrecision := 0;
  FMaxBcdScale := 0;
  FBCDType := dtFMTBcd;
end;

{-------------------------------------------------------------------------------}
destructor TFDADOUDLImporter.Destroy;
begin
  FDFreeAndNil(FUDLToImport);
  FDFreeAndNil(FSrcParams);
  FDFreeAndNil(FDestParams);
  FFetch := nil;
  FFmt := nil;
  FUpd := nil;
  FRes := nil;
  FOpts := nil;
  inherited Destroy;
end;

{-------------------------------------------------------------------------------}
procedure TFDADOUDLImporter.FillSrcParams(const AConnStr: String);
var
  iPos: Integer;
begin
  iPos := 1;
  FSrcParams.Clear;
  while iPos <= Length(AConnStr) do
    FSrcParams.Add(FDExtractFieldName(AConnStr, iPos));
end;

{-------------------------------------------------------------------------------}
function TFDADOUDLImporter.IsADOConnectingString(const AConnStr: String): Boolean;
var
  i1, i2: Integer;
begin
  FillSrcParams(AConnStr);
  i1 := FSrcParams.IndexOfName('FILE NAME');
  i2 := FSrcParams.IndexOfName('Provider');
  Result := (i1 <> -1) or (i2 <> -1);
end;

{-------------------------------------------------------------------------------}
function TFDADOUDLImporter.GetRDBMSKind(out AProvider: String;
  out AADDirect: Boolean): TFDRDBMSKind;
var
  sId: String;
  i: TFDRDBMSKind;
begin
  AADDirect := False;
  AProvider := UpperCase(FSrcParams.Values['Provider']);
  if FDStrLike(AProvider, C_FDDirect + '\_%') then begin
    AADDirect := True;
    Result := mkUnknown;
    sId := GetADDirectDriverID(AProvider);
    for i := Low(TFDRDBMSKind) to High(TFDRDBMSKind) do
      if CompareText(C_FD_PhysRDBMSIds[i], sId) = 0 then begin
        Result := i;
        Break;
      end;
  end
  else if FDStrLike(AProvider, 'SQLOLEDB.%') or FDStrLike(AProvider, 'SQLNCLI.%') then
    Result := mkMSSQL
  else if FDStrLike(AProvider, 'MSDAORA.%') or FDStrLike(AProvider, 'ORAOLEDB.ORACLE.%') then
    Result := mkOracle
  else if FDStrLike(AProvider, 'MICROSOFT.JET.OLEDB%') or FDStrLike(AProvider, 'MICROSOFT.ACE.OLEDB%') then
    Result := mkMSAccess
  else if FDStrLike(AProvider, 'IBMDADB2.%') then
    Result := mkDB2
  else if FDStrLike(AProvider, 'ASAPROV.%') or FDStrLike(AProvider, 'SAOLEDB.%') then
    Result := mkASA
  else if FDStrLike(AProvider, 'SIBPROVIDER%') or FDStrLike(AProvider, 'LCPI.IBPROVIDER%') then
    Result := mkInterbase
  else if FDStrLike(AProvider, 'MSDASQL.%') then
    Result := mkOther
  else
    Result := mkUnknown;
end;

{-------------------------------------------------------------------------------}
function TFDADOUDLImporter.GetADDirectDriverID(const AProvider: String): String;
begin
  if FDStrLike(AProvider, C_FDDirect + '\_%', True) then
    Result := Copy(AProvider, Length(C_FDDirect) + 2, Length(AProvider))
  else
    Result := AProvider;
end;

{-------------------------------------------------------------------------------}
function TFDADOUDLImporter.GetRDBMSKind(const AConnStr: String;
  out AProvider: String; out AADDirect: Boolean): TFDRDBMSKind;
begin
  FillSrcParams(AConnStr);
  Result := GetRDBMSKind(AProvider, AADDirect);
end;

{-------------------------------------------------------------------------------}
function TFDADOUDLImporter.GetSrcBool(const AName: String; var AResult: Boolean): Boolean;
var
  i: Integer;
begin
  i := FSrcParams.IndexOfName(AName);
  Result := i <> -1;
  AResult := False;
  if Result then
    AResult := (CompareText(FSrcParams.Values[AName], S_FD_True) = 0) or
      (CompareText(FSrcParams.Values[AName], S_FD_Yes) = 0) or
      (CompareText(FSrcParams.Values[AName], '1') = 0);
end;

{-------------------------------------------------------------------------------}
function TFDADOUDLImporter.GetSrcStr(const AName: String; var AResult: String): Boolean;
var
  i: Integer;
begin
  i := FSrcParams.IndexOfName(AName);
  Result := i <> -1;
  AResult := '';
  if Result then
    AResult := FSrcParams.Values[AName];
  if AResult <> '' then
    if (AResult[1] = '"') and (AResult[Length(AResult)] = '"') then
      AResult := Copy(AResult, 2, Length(AResult) - 2)
    else if (AResult[1] = '''') and (AResult[Length(AResult)] = '''') then
      AResult := Copy(AResult, 2, Length(AResult) - 2);
end;

{-------------------------------------------------------------------------------}
procedure TFDADOUDLImporter.MergeExtendedProps;
var
  i: Integer;
  sVal, sItem: String;
begin
  if GetSrcStr('Extended Properties', sVal) and (sVal <> '') then begin
    i := 1;
    while i <= Length(sVal) do begin
      sItem := FDExtractFieldName(sVal, i);
      if sItem <> '' then begin
        if (sItem[1] = '"') and (sItem[Length(sItem)] = '"') then
          sItem := Copy(sItem, 2, Length(sItem) - 2)
        else if (sItem[1] = '''') and (sItem[Length(sItem)] = '''') then
          sItem := Copy(sItem, 2, Length(sItem) - 2);
        FSrcParams.Add(sItem);
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDADOUDLImporter.ImportSQLCommon;
var
  sVal, sName: String;
  i: Integer;
begin
  if (GetSrcStr('User ID', sVal) or GetSrcStr('UID', sVal)) and (sVal <> '') then
    FDestParams.Values[S_FD_ConnParam_Common_UserName] := sVal;
  if (GetSrcStr('Password', sVal) or GetSrcStr('PWD', sVal)) and (sVal <> '') then
    FDestParams.Values[S_FD_ConnParam_Common_Password] := sVal;
  for i := 0 to FSrcParams.Count - 1 do begin
    sName := FDNameFromIndex(FSrcParams, i);
    if CompareText(Copy(sName, 1, 3), 'FD$') = 0 then
      FDestParams.Add(Copy(FSrcParams[i], 4, MAXINT));
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDADOUDLImporter.ImportMSSQL(AImportMode: Boolean);
var
  sVal: String;
  lVal: Boolean;
  oRule: TFDMapRule;
begin
  if AImportMode then
    FDestParams.Values[S_FD_ConnParam_Common_DriverID] := S_FD_MSSQLId;

  if MapDataTypes then begin
    FFmt.OwnMapRules := True;
    FFmt.MaxBcdPrecision := FMaxBcdPrecision;
    FFmt.MaxBcdScale := FMaxBcdScale;

    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := FBCDType;
    oRule.PrecMax := 9;
    oRule.ScaleMin := 0;
    oRule.ScaleMax := 0;
    oRule.TargetDataType := dtInt32;

    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := dtDouble;
    oRule.PrecMin := 19;
    oRule.PrecMax := 19;
    oRule.ScaleMin := 4;
    oRule.ScaleMax := 4;
    oRule.TargetDataType := FBCDType;

    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := dtCurrency;
    oRule.TargetDataType := FBCDType;

    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := dtByte;
    oRule.TargetDataType := dtUInt16;

    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := dtSByte;
    oRule.TargetDataType := dtInt16;

    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := dtUInt32;
    oRule.TargetDataType := dtInt32;

    if not EnableBCD then begin
      oRule := FFmt.MapRules.Add;
      oRule.SourceDataType := FBCDType;
      oRule.TargetDataType := dtDouble;
    end;
    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := dtDateTimeStamp;
    oRule.TargetDataType := dtDateTime;
  end;

  if AImportMode then begin
    MergeExtendedProps;
    if (GetSrcStr('Data Source', sVal) or GetSrcStr('Server', sVal)) and (sVal <> '') then
      FDestParams.Values[S_FD_ConnParam_Common_Server] := sVal;
    if (GetSrcStr('Initial Catalog', sVal) or GetSrcStr('Catalog', sVal) or
        GetSrcStr('Database', sVal)) and (sVal <> '') then
      FDestParams.Values[S_FD_ConnParam_Common_Database] := sVal;
    if GetSrcStr('Integrated Security', sVal) and (CompareText(sVal, 'SSPI') = 0) or
       GetSrcBool('Trusted_Connection', lVal) and lVal then
      FDestParams.Values[S_FD_ConnParam_Common_OSAuthent] := S_FD_Yes;
    if GetSrcStr('Network Library', sVal) or (sVal <> '') then
      FDestParams.Values[S_FD_ConnParam_MSSQL_Network] := sVal;
    if GetSrcStr('Network Address', sVal) or (sVal <> '') then
      FDestParams.Values[S_FD_ConnParam_MSSQL_Address] := sVal;
    if GetSrcStr('Current Language', sVal) or (sVal <> '') then
      FDestParams.Values[S_FD_ConnParam_MSSQL_Language] := sVal;
    if GetSrcStr('Application Name', sVal) or (sVal <> '') then
      FDestParams.Values[S_FD_ConnParam_Common_ApplicationName] := sVal;
    if GetSrcStr('Workstation ID', sVal) or (sVal <> '') then
      FDestParams.Values[S_FD_ConnParam_MSSQL_Workstation] := sVal;
    ImportSQLCommon;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDADOUDLImporter.ImportOracle(AImportMode: Boolean);
var
  sVal: String;
  lVal: Boolean;
  oRule: TFDMapRule;
begin
  if AImportMode then
    FDestParams.Values[S_FD_ConnParam_Common_DriverID] := S_FD_OraId;

  if MapDataTypes then begin
    FFmt.OwnMapRules := True;
    FFmt.MaxBcdPrecision := FMaxBcdPrecision;
    FFmt.MaxBcdScale := FMaxBcdScale;

    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := FBCDType;
    oRule.PrecMax := 9;
    oRule.ScaleMin := 0;
    oRule.ScaleMax := 0;
    oRule.TargetDataType := dtInt32;

    if not EnableBCD then begin
      oRule := FFmt.MapRules.Add;
      oRule.SourceDataType := FBCDType;
      oRule.TargetDataType := dtDouble;
    end;
    if ForceWideString then begin
      oRule := FFmt.MapRules.Add;
      oRule.SourceDataType := dtAnsiString;
      oRule.TargetDataType := dtWideString;
    end;
  end;

  if AImportMode then begin
    MergeExtendedProps;
    if GetSrcStr('Data Source', sVal) and (sVal <> '') then
      FDestParams.Values[S_FD_ConnParam_Common_Database] := sVal;
    if GetSrcBool('Integrated Security', lVal) and lVal or
       GetSrcBool('OSAuthent', lVal) and lVal then
      FDestParams.Values[S_FD_ConnParam_Common_OSAuthent] := S_FD_Yes;
    ImportSQLCommon;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDADOUDLImporter.ImportIB(AImportMode: Boolean);
var
  sVal1, sVal2: String;
//  oRule: TFDMapRule;
begin
  // all TX related settings are not imported
  if AImportMode then
    FDestParams.Values[S_FD_ConnParam_Common_DriverID] := S_FD_IBId;
                                           
(*
  if MapDataTypes then begin
    FFmt.OwnMapRules := True;
    FFmt.MaxBcdPrecision := FMaxBcdPrecision;
    FFmt.MaxBcdScale := FMaxBcdScale;
    if EnableBCD then begin
      oRule := FFmt.MapRules.Add;
      oRule.SourceDataType := dtCurrency;
      oRule.TargetDataType := FBCDType;
    end
    else begin
      oRule := FFmt.MapRules.Add;
      oRule.SourceDataType := FBCDType;
      oRule.TargetDataType := dtDouble;
    end;
    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := dtAnsiString;
    oRule.SizeMin := 256;
    oRule.TargetDataType := dtMemo;

    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := dtByteString;
    oRule.SizeMin := 256;
    oRule.TargetDataType := dtMemo;

    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := dtWideString;
    oRule.SizeMax := 255;
    oRule.TargetDataType := dtAnsiString;

    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := dtWideString;
    oRule.SizeMin := 256;
    oRule.TargetDataType := dtMemo;

    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := dtDateTimeStamp;
    oRule.TargetDataType := dtDateTime;
  end;
*)
  if AImportMode then begin
    MergeExtendedProps;
    if GetSrcStr('Location', sVal1) and (sVal1 <> '') or
       GetSrcStr('Data Source', sVal2) and (sVal2 <> '') then
      FDestParams.Values[S_FD_ConnParam_Common_Database] := sVal1 + sVal2;
    if GetSrcStr('Role', sVal1) and (sVal1 <> '') then
      FDestParams.Values[S_FD_ConnParam_IB_RoleName] := sVal1;
    if GetSrcStr('Character Set', sVal1) and (sVal1 <> '') then
      FDestParams.Values[S_FD_ConnParam_Common_CharacterSet] := sVal1;
    ImportSQLCommon;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDADOUDLImporter.ImportMSAccess(AImportMode: Boolean);
var
  sVal: String;
  oRule: TFDMapRule;
begin
  if AImportMode then
    FDestParams.Values[S_FD_ConnParam_Common_DriverID] := S_FD_MSAccId;

  if MapDataTypes then begin
    FFmt.OwnMapRules := True;
    FFmt.MaxBcdPrecision := FMaxBcdPrecision;
    FFmt.MaxBcdScale := FMaxBcdScale;

    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := dtCurrency;
    oRule.TargetDataType := FBCDType;

    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := dtByte;
    oRule.TargetDataType := dtUInt16;

    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := dtSByte;
    oRule.TargetDataType := dtInt16;

    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := dtUInt32;
    oRule.TargetDataType := dtInt32;

    if not EnableBCD then begin
      oRule := FFmt.MapRules.Add;
      oRule.SourceDataType := FBCDType;
      oRule.PrecMax := 16;
      oRule.PrecMin := 16;
      oRule.ScaleMax := 6;
      oRule.ScaleMin := 6;
      oRule.TargetDataType := dtDouble;
    end;
    if ForceWideString then begin
      oRule := FFmt.MapRules.Add;
      oRule.SourceDataType := dtAnsiString;
      oRule.TargetDataType := dtWideString;
    end;
    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := dtDateTimeStamp;
    oRule.TargetDataType := dtDateTime;
  end;

  if AImportMode then begin
    MergeExtendedProps;
    if GetSrcStr('Data Source', sVal) and (sVal <> '') then
      FDestParams.Values[S_FD_ConnParam_Common_Database] := sVal;
    if GetSrcStr('Jet OLEDB:System Database', sVal) and (sVal <> '') then
      FDestParams.Values[S_FD_ConnParam_MSAcc_SysDB] := sVal;
    ImportSQLCommon;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDADOUDLImporter.ImportDB2(AImportMode: Boolean);
var
  sVal: String;
  lByAlias: boolean;
  iPos: integer;
  oRule: TFDMapRule;
begin
  if AImportMode then
    FDestParams.Values[S_FD_ConnParam_Common_DriverID] := S_FD_DB2Id;

  if MapDataTypes then begin
    FFmt.OwnMapRules := True;
    FFmt.MaxBcdPrecision := FMaxBcdPrecision;
    FFmt.MaxBcdScale := FMaxBcdScale;

    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := FBCDType;
    oRule.PrecMax := 9;
    oRule.ScaleMin := 0;
    oRule.ScaleMax := 0;
    oRule.TargetDataType := dtInt32;

    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := dtCurrency;
    oRule.TargetDataType := FBCDType;

    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := dtByte;
    oRule.TargetDataType := dtUInt16;

    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := dtSByte;
    oRule.TargetDataType := dtInt16;

    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := dtUInt32;
    oRule.TargetDataType := dtInt32;

    if not EnableBCD then begin
      oRule := FFmt.MapRules.Add;
      oRule.SourceDataType := FBCDType;
      oRule.TargetDataType := dtDouble;
    end;
    if ForceWideString then begin
      oRule := FFmt.MapRules.Add;
      oRule.SourceDataType := dtAnsiString;
      oRule.TargetDataType := dtWideString;
    end;
    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := dtDateTimeStamp;
    oRule.TargetDataType := dtDateTime;
  end;

  // 2 approaches supported
  // by Alias
  // UDL Contents: Provider=IBMDADB2.DB2COPY1;Data Source=SAPHIR;Location="";...
  // by direct definition (9.7 Client)
  // UDL Contents: Provider=IBMDADB2.DB2COPY1;Data Source=SAPHIR_U;Location=vmsvdb6:50000; ...
  // or older style see: http://www.connectionstrings.com/ibm-db2
  // UDL Contents: Provider=IBMDADB2;Database=myDataBase;Hostname=myServerAddress;Protocol=TCPIP;Port=50000;Uid=myUsername;Pwd=myPassword;

  // UDL Contents: Provider=IBMDADB2.DB2COPY1;Persist Security Info=True;User ID="";Data Source=SAPHIR_U;Location="";Extended Properties="trusted_connection=yes"

  if AImportMode then begin
    MergeExtendedProps;
    lByAlias:= not ((GetSrcStr('Hostname', sVal) or GetSrcStr('Location', sVal)) and (sVal <> ''));
    if lByAlias then
    begin
      if (GetSrcStr('Data Source', sVal)) and (sVal <> '') then
        FDestParams.Values[S_FD_ConnParam_DB2_Alias] := sVal;
    end else begin
      if (GetSrcStr('Database', sVal) or GetSrcStr('Data Source', sVal)) and (sVal <> '') then
        FDestParams.Values[S_FD_ConnParam_Common_Database] := sVal;
      if (GetSrcStr('Hostname', sVal) or GetSrcStr('Location', sVal)) and (sVal <> '') then
      begin
        iPos:= Pos(':',sVal); // vmsvdb6:50000
        if iPos=0 then
        begin
          FDestParams.Values[S_FD_ConnParam_Common_Server] := sVal;
        end else begin
          FDestParams.Values[S_FD_ConnParam_Common_Server] := Copy(sVal,1,iPos-1);
          FDestParams.Values[S_FD_ConnParam_Common_Port] := Copy(sVal,iPos+1,Length(sVal));
        end;
      end;
      if GetSrcStr('Protocol', sVal) and (sVal <> '') then
        FDestParams.Values[S_FD_ConnParam_DB2_Protocol] := sVal;
      if GetSrcStr('Port', sVal) and (sVal <> '') then
        FDestParams.Values[S_FD_ConnParam_Common_Port] := sVal;
    end;
    // parameters for both modes (lByAlias true or false)
    if GetSrcStr('trusted_connection', sVal) and (CompareText(sVal, 'yes') = 0) then // switch available for both
      FDestParams.Values[S_FD_ConnParam_DB2_Trusted] := 'YES';
    ImportSQLCommon;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDADOUDLImporter.ImportASA(AImportMode: Boolean);
var
  sVal: String;
  lVal: Boolean;
  oRule: TFDMapRule;
begin
  if AImportMode then
    FDestParams.Values[S_FD_ConnParam_Common_DriverID] := S_FD_ASAId;

  if MapDataTypes then begin
    FFmt.OwnMapRules := True;
    FFmt.MaxBcdPrecision := FMaxBcdPrecision;
    FFmt.MaxBcdScale := FMaxBcdScale;

    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := dtCurrency;
    oRule.TargetDataType := FBCDType;

    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := dtByte;
    oRule.TargetDataType := dtUInt16;

    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := dtSByte;
    oRule.TargetDataType := dtInt16;

    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := dtUInt32;
    oRule.TargetDataType := dtInt32;

    if not EnableBCD then begin
      oRule := FFmt.MapRules.Add;
      oRule.SourceDataType := FBCDType;
      oRule.TargetDataType := dtDouble;
    end;
    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := dtDateTimeStamp;
    oRule.TargetDataType := dtDateTime;
  end;

  if AImportMode then begin
    MergeExtendedProps;
    if (GetSrcStr('Data Source', sVal) or GetSrcStr('DSN', sVal) or
        GetSrcStr('ServerName', sVal) or GetSrcStr('Srvr', sVal) or
        GetSrcStr('EngineName', sVal) or GetSrcStr('Eng', sVal)) and (sVal <> '') then
      FDestParams.Values[S_FD_ConnParam_Common_Server] := sVal;
    if (GetSrcStr('DatabaseName', sVal) or GetSrcStr('DBN', sVal) or
        GetSrcStr('Initial Catalog', sVal) or GetSrcStr('Catalog', sVal)) and (sVal <> '') then
      FDestParams.Values[S_FD_ConnParam_Common_Database] := sVal;
    if (GetSrcStr('DatabaseFile', sVal) or GetSrcStr('DBF', sVal)) and (sVal <> '') then
      FDestParams.Values[S_FD_ConnParam_ASA_DatabaseFile] := sVal;
    if (GetSrcStr('CommLinks', sVal) or GetSrcStr('Links', sVal)) and (sVal <> '') then
      FDestParams.Values[S_FD_ConnParam_ODBC_Advanced] := 'CommLinks=' + sVal;
    if GetSrcBool('Integrated', lVal) and lVal then
      FDestParams.Values[S_FD_ConnParam_Common_OSAuthent] := S_FD_Yes;
    if GetSrcStr('Encryption', sVal) and (sVal <> '') then
      FDestParams.Values[S_FD_ConnParam_ASA_Encrypt] := sVal;
    if GetSrcBool('Compress', lVal) and lVal then
      FDestParams.Values[S_FD_ConnParam_ASA_Compress] := S_FD_Yes;
    ImportSQLCommon;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDADOUDLImporter.ImportODBC(AImportMode: Boolean);
var
  sVal: String;
  oRule: TFDMapRule;
begin
  if AImportMode then
    FDestParams.Values[S_FD_ConnParam_Common_DriverID] := S_FD_ODBCId;

  if MapDataTypes then begin
    FFmt.OwnMapRules := True;
    FFmt.MaxBcdPrecision := FMaxBcdPrecision;
    FFmt.MaxBcdScale := FMaxBcdScale;

    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := dtCurrency;
    oRule.TargetDataType := FBCDType;

    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := dtByte;
    oRule.TargetDataType := dtUInt16;

    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := dtSByte;
    oRule.TargetDataType := dtInt16;

    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := dtUInt32;
    oRule.TargetDataType := dtInt32;

    if not EnableBCD then begin
      oRule := FFmt.MapRules.Add;
      oRule.SourceDataType := FBCDType;
      oRule.TargetDataType := dtDouble;
    end;
    if ForceWideString then begin
      oRule := FFmt.MapRules.Add;
      oRule.SourceDataType := dtAnsiString;
      oRule.TargetDataType := dtWideString;
    end;
    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := dtDateTimeStamp;
    oRule.TargetDataType := dtDateTime;
  end;

  if AImportMode then begin
    if (GetSrcStr('Data Source', sVal) or GetSrcStr('DSN', sVal)) and (sVal <> '') then
      FDestParams.Values[S_FD_ConnParam_ODBC_DataSource] := sVal;
    if GetSrcStr('Extended Properties', sVal) and (sVal <> '') then
      FDestParams.Values[S_FD_ConnParam_ODBC_Advanced] := sVal;
    ImportSQLCommon;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDADOUDLImporter.ImportOther(AImportMode: Boolean; const AProvider: String);
var
  oRule: TFDMapRule;
begin
  if AImportMode then
    FDestParams.Values[S_FD_ConnParam_Common_DriverID] := GetADDirectDriverID(AProvider);

  if MapDataTypes then begin
    FFmt.OwnMapRules := True;
    FFmt.MaxBcdPrecision := FMaxBcdPrecision;
    FFmt.MaxBcdScale := FMaxBcdScale;

    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := dtCurrency;
    oRule.TargetDataType := FBCDType;

    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := dtByte;
    oRule.TargetDataType := dtUInt16;

    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := dtSByte;
    oRule.TargetDataType := dtInt16;

    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := dtUInt32;
    oRule.TargetDataType := dtInt32;

    if not EnableBCD then begin
      oRule := FFmt.MapRules.Add;
      oRule.SourceDataType := FBCDType;
      oRule.TargetDataType := dtDouble;
    end;
    if ForceWideString then begin
      oRule := FFmt.MapRules.Add;
      oRule.SourceDataType := dtAnsiString;
      oRule.TargetDataType := dtWideString;
    end;
    oRule := FFmt.MapRules.Add;
    oRule.SourceDataType := dtDateTimeStamp;
    oRule.TargetDataType := dtDateTime;
  end;

  if AImportMode then begin
    MergeExtendedProps;
    ImportSQLCommon;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDADOUDLImporter.MakeADOCompatible(const AConnectionDef: IFDStanConnectionDef);
var
  oPrevParams: TStrings;
  sType: String;
begin
  oPrevParams := FDestParams;
  FDestParams := AConnectionDef.Params;
  AConnectionDef.ReadOptions(FFmt, FUpd, FFetch, FRes);

  try
    sType := AConnectionDef.DriverID;
    if CompareText(sType, S_FD_OraId) = 0 then
      ImportOracle(False)
    else if CompareText(sType, S_FD_IBId) = 0 then
      ImportIB(False)
    else if CompareText(sType, S_FD_MSSQLId) = 0 then
      ImportMSSQL(False)
    else if CompareText(sType, S_FD_MSAccId) = 0 then
      ImportMSAccess(False)
    else if CompareText(sType, S_FD_DB2Id) = 0 then
      ImportDB2(False)
    else if CompareText(sType, S_FD_ASAId) = 0 then
      ImportASA(False)
    else if CompareText(sType, S_FD_ODBCId) = 0 then
      ImportODBC(False)
    else
      ImportOther(False, '');
    if Assigned(FOnBeforeWriteOptions) then
      FOnBeforeWriteOptions(sType,FFmt, FUpd, FFetch, FRes);
    AConnectionDef.WriteOptions(FFmt, FUpd, FFetch, FRes);

  finally
    FDestParams := oPrevParams;
  end;
end;

{-------------------------------------------------------------------------------}
function TFDADOUDLImporter.ImportConnStr(const AConnStr: String;
  ADefs: IFDStanConnectionDefs; const ADefName: String = ''): IFDStanConnectionDef;
var
  sCurName, sProv: String;
  i, iOverwrite: Integer;
  lADDirect: Boolean;
begin
  TFDStringList(FSrcParams).Sorted := True;
  try
    FillSrcParams(AConnStr);
    i := FSrcParams.IndexOfName('FILE NAME');
    if i <> -1 then begin
      Result := ImportUDL(FDValueFromIndex(FSrcParams, i), ADefs, ADefName);
      Exit;
    end;

    FDestParams.Clear;
    case GetRDBMSKind(sProv, lADDirect) of
    mkOracle:    ImportOracle(True);
    mkInterbase: ImportIB(True);
    mkMSSQL:     ImportMSSQL(True);
    mkMSAccess:  ImportMSAccess(True);
    mkDB2:       ImportDB2(True);
    mkASA:       ImportASA(True);
    mkOther:     ImportODBC(True);
    else
      if not lADDirect then
        raise Exception.CreateFmt('UDL import for provider [%s] is not supported', [sProv])
      else
        ImportOther(True, sProv);
    end;

    if OverwriteDefs then begin
      Result := ADefs.FindConnectionDef(ADefName);
      if (Result <> nil) and Assigned(FOnOverwrite) then begin
        iOverwrite := 1;
        FOnOverwrite(ADefName, iOverwrite);
        if iOverwrite = 0 then
          Result := nil
        else if iOverwrite = -1 then
          Exit;
      end
    end
    else
      Result := nil;
    if Result = nil then
      Result := ADefs.Add as IFDStanConnectionDef;
    if Assigned(FOnBeforeWriteSettings) then
      FOnBeforeWriteSettings(FDestParams);
    Result.Params.Assign(FDestParams);
    sCurName := ADefName;
    if not OverwriteDefs then begin
      i := 0;
      while ADefs.FindConnectionDef(sCurName) <> nil do begin
        Inc(i);
        sCurName := Format('%s_%d', [ADefName, i]);
      end;
    end;
    Result.Name := sCurName;
    if Assigned(FOnBeforeWriteOptions) then
      FOnBeforeWriteOptions(Result.DriverID, FFmt, FUpd, FFetch, FRes);
    Result.WriteOptions(FFmt, FUpd, FFetch, FRes);
    if MarkDefsPersistent then
      Result.MarkPersistent;
  finally
    TFDStringList(FSrcParams).Sorted := False;
  end;
end;

{-------------------------------------------------------------------------------}
function TFDADOUDLImporter.ImportUDL(const AFileName: String;
  ADefs: IFDStanConnectionDefs; const ADefName: String = ''): IFDStanConnectionDef;
var
  oUDLFile: TFileStream;
  ws: UnicodeString;
  sName: String;
  cw: WideChar;
  iSize: Integer;
begin
  oUDLFile := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    iSize := oUDLFile.Size;
    oUDLFile.Read(cw, SizeOf(WideChar));
    if cw <> #$FEFF then
      oUDLFile.Position := 0
    else
      Dec(iSize, SizeOf(WideChar));
    SetLength(ws, iSize div 2);
    oUDLFile.Read(ws[1], iSize);
    FSrcParams.Text := ws;
    if (FSrcParams[0] <> '[oledb]') or
       (FSrcParams[1] <> '; Everything after this line is an OLE DB initstring') then
      raise Exception.CreateFmt('UDL file [%s] has invalid format', [AFileName]);
    if ADefName = '' then
      sName := ChangeFileExt(ExtractFileName(AFileName), '')
    else
      sName := ADefName;
    Result := ImportConnStr(FSrcParams[2], ADefs, sName);
  finally
    FDFree(oUDLFile);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDADOUDLImporter.ImportAllUDLs(const AUDLPath: String;
  ADefs: IFDStanConnectionDefs);
var
  i: Integer;
  oDef: IFDStanConnectionDef;
  rSR: TSearchRec;
  iRes: Integer;
  lClearUDLs: Boolean;
begin
  lClearUDLs := False;

  try
    if (FUDLToImport.Count = 0) and (AUDLPath <> '') then begin
      lClearUDLs := True;
      iRes := FindFirst(FDNormPath(AUDLPath) + '*.udl', faAnyFile, rSR);
      while iRes = 0 do begin
        FUDLToImport.Add(rSR.Name);
        iRes := FindNext(rSR);
      end;
      FindClose(rSR);
    end;

    for i := 0 to FUDLToImport.Count - 1 do begin
      oDef := ImportUDL(FUDLToImport[i], ADefs, '');
      oDef.MarkPersistent;
    end;

  finally
    if lClearUDLs then
      FUDLToImport.Clear;
  end;
end;

(*
--------------------------
Common

User Id | UID                          Username
Password | PWD                         Password

--------------------------
MSSQL
? Provider=SQLOLEDB | SQLNCLI

Data Source | Server                   Server
Initial Catalog | Database             Database
Integrated Security=SSPI |
  Trusted_Connection=yes               OSAuthent=Y
Network Library                        Network
Network Address                        Address
Current Language                       Language
Application Name                       App
Workstation ID                         Workstation

--------------------------
Oracle
? Provider=MSDAORA | ORAOLEDB.ORACLE

Data Source                            Database
Integrated Security=Yes | OSAuthent=1  OSAuthent=Y

--------------------------
IB
? Provider=SIBPROVIDER[.N] | LCPI.IBProvider[.xxx]

Location + Data Source                 Database
Role                                   RoleName
Character set                          CharacterSet

--------------------------
DB2
? Provider=IBMDADB2

DBAlias                                Database
Database + Hostname + Protocol + Port  ODBCAdvanced

--------------------------
ASA
? Provider=ASAPROV | SAOLEDB

ServerName | EngineName       EngineName
DatabaseName                  Database
DatabaseFile                  DatabaseFile
Integrated (yes|no)           OSAuthent
Encryption (str)              Encrypt (str)
Compress                      Compress

--------------------------
MSAccess
? Provider=Microsoft.Jet.OLEDB[.xxxx]

Data Source                  Database
Jet OLEDB:System Database    SystemDB

*)
end.

