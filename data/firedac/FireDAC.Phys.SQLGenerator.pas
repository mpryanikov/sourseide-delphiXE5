{*******************************************************}
{                                                       }
{               Delphi FireDAC Framework                }
{             FireDAC SQL command generator             }
{                                                       }
{ Copyright(c) 2004-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}
{$I FireDAC.inc}

unit FireDAC.Phys.SQLGenerator;

interface

uses
  Data.DB,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.DatS,
  FireDAC.Phys.Intf;

type
  TFDPhysCommandPart = (cpSELECT, cpFROM, cpWHERE, cpORDERBY, cpVALUES, cpSET,
    cpRETURNING);

  TFDPhysCommandGenerator = class(TInterfacedObject, IFDPhysCommandGenerator)
  private
    FColumn: TFDDatSColumn;
    FTabAlias: String;
    FFlags: TFDPhysCommandGeneratorFlags;
    FUpdateRowOptions: TFDUpdateRowOptions;
    FFillRowOptions: TFDPhysFillRowOptions;
    FGenOptions: TFDPhysCommandGeneratorOptions;
    FMappingHandler: IFDPhysMappingHandler;
    FLastColumn: TFDDatSColumn;
    FLastColumnAttrs: TFDDataAttributes;
    FLastColumnOptions: TFDDataOptions;
    FLastColumnObjName: String;
    FLastColumnAliasName: String;
    FSQLOrderByPos: Integer;
    constructor Create; overload;
    function NormalizeName(const AName: String; ASubObj, AForceNoQuote,
      AForceQuote: Boolean): String;
    function GenerateInlineRefresh(const AStmt: String;
      ARequest: TFDUpdateRequest): String;
    function GenerateIdentityInsert(const ATable, AStmt: String;
      ARequest: TFDUpdateRequest): String;
    function ColumnIdentityAssigned(ARow: TFDDatSRow;
      AColumn: TFDDatSColumn; AAlready: Boolean): Boolean;
    function GetNestedRow(ARow: TFDDatSRow; AColIndex: Integer): TFDDatSRow;
  protected
    FTable: TFDDatSTable;
    FRow: TFDDatSRow;
    FParams: TFDParams;
    FConnMeta: IFDPhysConnectionMetadata;
    FCommand: IFDPhysCommand;
    FOptions: IFDStanOptions;
    FCommandKind: TFDPhysCommandKind;
    FCommandPart: TFDPhysCommandPart;
    function GetSelectList(AAllowIdentityExp, AFlatFieldList: Boolean;
      out ANeedFrom: Boolean): String;
    function GetFrom: String;
    function GetWhere(AInInsert, ARequired, ANotNull: Boolean): String;
    function GetColumn(const AParentField: String; ARowIndex: Integer;
      AColumn: TFDDatSColumn): String;
    function ColumnChanged(ARow: TFDDatSRow; AColumn: TFDDatSColumn): Boolean;
    function ColumnInKey(AColumn: TFDDatSColumn): Boolean;
    function ColumnSearchable(AColumn: TFDDatSColumn): Boolean;
    function ColumnStorable(AColumn: TFDDatSColumn): Boolean;
    function ColumnReqRefresh(ARequest: TFDUpdateRequest;
      AColumn: TFDDatSColumn): Boolean;
    function ColumnIsHBLOB(AColumn: TFDDatSColumn): Boolean;
    function AddColumnParam(AColumn: TFDDatSColumn; AValueAge: Integer;
      AType: TParamType): String;
    function ColumnUpdatable(AColumn: TFDDatSColumn; AInInsert: Boolean): Boolean;
    function NormalizeColName(const AName: String): String;
    function NormalizeTabName(const AName: String): String;
    procedure GetColumnAttributes(AColumn: TFDDatSColumn;
      var AAttrs: TFDDataAttributes; var AOptions: TFDDataOptions;
      var AObjName, AAliasName: String);
    function BRK: String;
    function GetReturning(ARequest: TFDUpdateRequest; AWithInto: Boolean): String;
    // IFDPhysCommandGenerator
    // private
    function GetFillRowOptions: TFDPhysFillRowOptions;
    function GetGenOptions: TFDPhysCommandGeneratorOptions;
    function GetFlags: TFDPhysCommandGeneratorFlags;
    function GetParams: TFDParams;
    function GetRow: TFDDatSRow;
    function GetTable: TFDDatSTable;
    function GetUpdateRowOptions: TFDUpdateRowOptions;
    procedure SetParams(const AValue: TFDParams);
    procedure SetRow(const AValue: TFDDatSRow);
    procedure SetTable(const AValue: TFDDatSTable);
    procedure SetUpdateRowOptions(const AValue: TFDUpdateRowOptions);
    function GetCol: TFDDatSColumn;
    procedure SetCol(const AValue: TFDDatSColumn);
    procedure SetFillRowOptions(const AValue: TFDPhysFillRowOptions);
    procedure SetGenOptions(const AValue: TFDPhysCommandGeneratorOptions);
    function GetCommandKind: TFDPhysCommandKind;
    function GetSQLOrderByPos: Integer;
    function GetOptions: IFDStanOptions;
    procedure SetOptions(const AValue: IFDStanOptions);
    function GetMappingHandler: IFDPhysMappingHandler;
    procedure SetMappingHandler(const AValue: IFDPhysMappingHandler);
    // public
    function GenerateSelect(ARequired: Boolean): String; virtual;
    function GenerateInsert: String;
    function GenerateUpdateHBlobs: String;
    function GenerateUpdate: String;
    function GenerateDelete: String;
    function GenerateDeleteAll(ANoUndo: Boolean): String;
    function GenerateLock: String;
    function GenerateUnLock: String;
    function GenerateSavepoint(const AName: String): String;
    function GenerateRollbackToSavepoint(const AName: String): String;
    function GenerateCommitSavepoint(const AName: String): String;
    function GenerateFetchGenerators: String;
    function GenerateReadGenerator(const AName, AAlias: String;
      ANextValue, AFullSelect: Boolean): String;
    function GenerateCall(const AName: String): String;
    function GenerateEval(const AExpr: String): String;
    function GeneratePing: String;
    function GenerateSelectTable(ATableParams: TFDPhysTableParams): String;
    function GenerateStoredProcParams(const ACatalog, ASchema, APackage, AProc: String;
      AOverload: Word = 0): String;
    function GenerateStoredProcCall(const ACatalog, ASchema, APackage, AProc: String;
      ASPUsage: TFDPhysCommandKind = skStoredProc): String;
    function GenerateSelectMetaInfo(AKind: TFDPhysMetaInfoKind;
      const ACatalog, ASchema, ABaseObject, AObject, AWildcard: String;
      AObjectScopes: TFDPhysObjectScopes; ATableKinds: TFDPhysTableKinds;
      AOverload: Word): String;
    function GenerateLimitSelect(const ASQL: String; ASkip, ARows: Integer;
      AOneMore: Boolean): String;
    function GenerateCountSelect(const ASQL: String): String;
    // overridable
    function GetSubColumn(const AParentField, ASubField: String): String; virtual;
    function GetRowConstructor(const AValues: String; ARowCol: TFDDatSColumn): String; virtual;
    function GetCastColumn(const AValue: String; ACol: TFDDatSColumn): String; virtual;
    function GetInlineRefresh(const AStmt: String;
      ARequest: TFDUpdateRequest): String; virtual;
    function GetIdentityInsert(const ATable, AStmt: String;
      ARequest: TFDUpdateRequest): String; virtual;
    function GetIdentity(ASessionScope: Boolean): String; virtual;
    function GetSingleRowTable: String; virtual;
    function GetPessimisticLock: String; virtual;
    function GetPessimisticUnLock: String; virtual;
    function GetSavepoint(const AName: String): String; virtual;
    function GetRollbackToSavepoint(const AName: String): String; virtual;
    function GetCommitSavepoint(const AName: String): String; virtual;
    function GetReadGenerator(const AName, AAlias: String;
      ANextValue, AFullSelect: Boolean): String; virtual;
    function GetCall(const AName: String): String; virtual;
    function GetPing: String; virtual;
    function GetStoredProcParams(const ACatalog, ASchema, APackage,
      AProc: String; AOverload: Word): String; virtual;
    function GetStoredProcCall(const ACatalog, ASchema, APackage, AProc: String;
      ASPUsage: TFDPhysCommandKind): String; virtual;
    function GetSelectMetaInfo(AKind: TFDPhysMetaInfoKind;
      const ACatalog, ASchema, ABaseObject, AObject, AWildcard: String;
      AObjectScopes: TFDPhysObjectScopes; ATableKinds: TFDPhysTableKinds;
      AOverload: Word): String; virtual;
    function HasKW(const ASQL, AKW: String): Boolean;
    function GetLimitSelect(const ASQL: String; ASkip, ARows: Integer): String; virtual;
    function GetCountSelect(const ASQL: String): String; virtual;
  public
    constructor Create(const ACommand: IFDPhysCommand); overload;
    constructor Create(const AConnection: IFDPhysConnection); overload;
  end;

implementation

uses
  System.Variants, Data.FmtBCD, System.SysUtils, System.Classes,
  FireDAC.Stan.Consts, FireDAC.Stan.Error, FireDAC.Stan.Util;

{-------------------------------------------------------------------------------}
{ TFDPhysCommandGenerator                                                       }
{-------------------------------------------------------------------------------}
constructor TFDPhysCommandGenerator.Create;
begin
  inherited Create;
  FTabAlias := C_FD_CmdGenAlias;
  FFillRowOptions := [foBlobs, foDetails, foData, foAfterIns, foAfterUpd,
    foUpdatable, foClear];
end;

{-------------------------------------------------------------------------------}
constructor TFDPhysCommandGenerator.Create(const ACommand: IFDPhysCommand);
begin
  ASSERT((ACommand <> nil) and (ACommand.Connection <> nil));
  Create;
  ACommand.Connection.CreateMetadata(FConnMeta);
  FCommand := ACommand;
  FOptions := ACommand.Options;
  FCommandKind := ACommand.CommandKind;
  FSQLOrderByPos := ACommand.SQLOrderByPos;
  FGenOptions := [goBeautify];
end;

{-------------------------------------------------------------------------------}
constructor TFDPhysCommandGenerator.Create(const AConnection: IFDPhysConnection);
begin
  ASSERT(AConnection <> nil);
  Create;
  AConnection.CreateMetadata(FConnMeta);
  FCommand := nil;
  FOptions := AConnection.Options;
  FCommandKind := skUnknown;
  FSQLOrderByPos := 0;
  FGenOptions := [goBeautify];
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetFillRowOptions: TFDPhysFillRowOptions;
begin
  Result := FFillRowOptions;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetGenOptions: TFDPhysCommandGeneratorOptions;
begin
  Result := FGenOptions;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetFlags: TFDPhysCommandGeneratorFlags;
begin
  Result := FFlags;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetParams: TFDParams;
begin
  Result := FParams;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetRow: TFDDatSRow;
begin
  Result := FRow;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetTable: TFDDatSTable;
begin
  Result := FTable;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetCol: TFDDatSColumn;
begin
  Result := FColumn;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetUpdateRowOptions: TFDUpdateRowOptions;
begin
  Result := FUpdateRowOptions;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysCommandGenerator.SetCol(const AValue: TFDDatSColumn);
begin
  FColumn := AValue;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysCommandGenerator.SetParams(const AValue: TFDParams);
begin
  FParams := AValue;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysCommandGenerator.SetRow(const AValue: TFDDatSRow);
begin
  FRow := AValue;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysCommandGenerator.SetTable(const AValue: TFDDatSTable);
begin
  FTable := AValue;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysCommandGenerator.SetUpdateRowOptions(
  const AValue: TFDUpdateRowOptions);
begin
  FUpdateRowOptions := AValue;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysCommandGenerator.SetFillRowOptions(
  const AValue: TFDPhysFillRowOptions);
begin
  FFillRowOptions := AValue;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysCommandGenerator.SetGenOptions(
  const AValue: TFDPhysCommandGeneratorOptions);
begin
  FGenOptions := AValue;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetCommandKind: TFDPhysCommandKind;
begin
  Result := FCommandKind;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetSQLOrderByPos: Integer;
begin
  Result := FSQLOrderByPos;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetOptions: IFDStanOptions;
begin
  Result := FOptions;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysCommandGenerator.SetOptions(const AValue: IFDStanOptions);
begin
  FOptions := AValue;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetMappingHandler: IFDPhysMappingHandler;
begin
  Result := FMappingHandler;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysCommandGenerator.SetMappingHandler(const AValue: IFDPhysMappingHandler);
begin
  FMappingHandler := AValue;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.BRK: String;
begin
  if goBeautify in FGenOptions then
    Result := C_FD_EOL
  else
    Result := ' ';
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.NormalizeName(const AName: String; ASubObj,
  AForceNoQuote, AForceQuote: Boolean): String;
var
  rName: TFDPhysParsedName;
  eEncOpts: TFDPhysEncodeOptions;
  eDecOpts: TFDPhysDecodeOptions;
begin
  eDecOpts := [];
  if ASubObj then
    Include(eDecOpts, doSubObj);
  if not FOptions.FormatOptions.QuoteIdentifiers then
    Include(eDecOpts, doNormalize);
  if AForceNoQuote then
    Include(eDecOpts, doUnquote);
  FConnMeta.DecodeObjName(AName, rName, FCommand, eDecOpts);
  eEncOpts := [];
  if AForceQuote or FOptions.FormatOptions.QuoteIdentifiers then
    Include(eEncOpts, eoQuote);
  if goBeautify in FGenOptions then
    Include(eEncOpts, eoBeautify);
  Result := FConnMeta.EncodeObjName(rName, FCommand, eEncOpts);
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.NormalizeColName(const AName: String): String;
begin
  Result := NormalizeName(AName, True, goForceNoQuoteCol in FGenOptions,
    goForceQuoteCol in FGenOptions);
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.NormalizeTabName(const AName: String): String;
begin
  Result := NormalizeName(AName, False, goForceNoQuoteTab in FGenOptions,
    goForceQuoteTab in FGenOptions);
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetSubColumn(const AParentField,
  ASubField: String): String;
begin
  Result := AParentField + '.' + ASubField;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetRowConstructor(const AValues: String;
  ARowCol: TFDDatSColumn): String;
begin
  Result := NormalizeColName(ARowCol.SourceDataTypeName) + '(' + AValues + ')';
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetCastColumn(const AValue: String;
  ACol: TFDDatSColumn): String;
begin
  Result := AValue;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetColumn(const AParentField: String;
  ARowIndex: Integer; AColumn: TFDDatSColumn): String;
var
  eAttrs: TFDDataAttributes;
  eOpts: TFDDataOptions;
  sName, sAlias: String;
begin
  if ARowIndex >= 0 then
    Result := AParentField + '[' + IntToStr(ARowIndex) + ']'
  else begin
    sName := '';
    sAlias := '';
    eOpts := [];
    eAttrs := [];
    GetColumnAttributes(AColumn, eAttrs, eOpts, sName, sAlias);
    Result := NormalizeColName(sName);
    if AParentField <> '' then
      Result := GetSubColumn(AParentField, Result);
    if (AParentField = '') and (FTabAlias <> '') then
      Result := FTabAlias + '.' + Result;
  end;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetNestedRow(ARow: TFDDatSRow; AColIndex: Integer): TFDDatSRow;
begin
  if ARow = nil then
    Result := nil
  else
    Result := ARow.NestedRow[AColIndex];
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetFrom: String;
var
  iTmp: Integer;
  sTmp: String;
  oTmp: TFDDatSTable;
  pTable: Variant;
  eKind: TFDPhysNameKind;
begin
  FCommandPart := cpFROM;
  if FTable = nil then begin
    pTable := 0;
    eKind := nkDefault;
  end
  else begin
    pTable := FTable.Name;
    eKind := nkDatS;
  end;
  iTmp := 0;
  sTmp := '';
  oTmp := nil;
  Result := '';
  if (GetMappingHandler = nil) or
     (GetMappingHandler.MapRecordSet(pTable, eKind, iTmp, sTmp,
                                     sTmp, Result, oTmp) = mrDefault) then
    Result := FTable.ActualOriginName;
  Result := NormalizeTabName(Result);
  if Result = '' then
    FDException(Self, [S_FD_LPhys], er_FD_AccUpdateTabUndefined, []);
  if FTabAlias <> '' then
    Result := Result + ' ' + FTabAlias;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysCommandGenerator.GetColumnAttributes(AColumn: TFDDatSColumn;
  var AAttrs: TFDDataAttributes; var AOptions: TFDDataOptions;
  var AObjName, AAliasName: String);
var
  eMapResult: TFDPhysMappingResult;
  iTmp: Integer;
  sTmp, sAliasName, sUpdateName: String;
  oTmp: TFDDatSColumn;
begin
  if FLastColumn = AColumn then begin
    AAttrs := FLastColumnAttrs;
    AOptions := FLastColumnOptions;
    AObjName := FLastColumnObjName;
    AAliasName := FLastColumnAliasName;
    Exit;
  end;
  // Do not map column, if it is nested. At moment TFDRdbmsDataSet creates a
  // TFDTableAdapter, which always return mrNotMapped for columns of a sub-table.
  if (GetMappingHandler = nil) or (AColumn.ColumnList.ParentRowRefCol <> -1) then
    eMapResult := mrDefault
  else begin
    iTmp := 0;
    sTmp := '';
    sAliasName := '';
    sUpdateName := '';
    oTmp := nil;
    eMapResult := GetMappingHandler.MapRecordSetColumn(NativeInt(AColumn.Table),
      nkObj, AColumn.Name, nkDatS, iTmp, sAliasName, sTmp, sUpdateName, oTmp);
  end;
  case eMapResult of
  mrDefault:
    begin
      AOptions := AColumn.ActualOptions;
      AAttrs := AColumn.ActualAttributes;
      AObjName := AColumn.ActualOriginColName;
      AAliasName := AColumn.Name;
    end;
  mrMapped:
    begin
      AOptions := AColumn.ActualOptions;
      AAttrs := AColumn.ActualAttributes;
      AObjName := sUpdateName;
      AAliasName := sAliasName;
    end;
  else
    AOptions := [];
    AAttrs := [];
    AObjName := '';
    AAliasName := '';
  end;
  FLastColumn := AColumn;
  FLastColumnAttrs := AAttrs;
  FLastColumnOptions := AOptions;
  FLastColumnObjName := AObjName;
  if CompareText(AObjName, AAliasName) = 0 then
    AAliasName := '';
  FLastColumnAliasName := AAliasName;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.ColumnStorable(AColumn: TFDDatSColumn): Boolean;
var
  eAttrs: TFDDataAttributes;
  eOpts: TFDDataOptions;
  sName, sAlias: String;
begin
  sName := '';
  sAlias := '';
  eAttrs := [];
  eOpts := [];
  GetColumnAttributes(AColumn, eAttrs, eOpts, sName, sAlias);
  Result :=
    not (caUnnamed in eAttrs) and (sName <> '') and
    not ((caCalculated in eAttrs) and (AColumn.SourceID <= 0)) and
    not ((caInternal in eAttrs) and not (caROWID in eAttrs)) and
    ((caBase in eAttrs) or FOptions.UpdateOptions.UpdateNonBaseFields);
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.ColumnReqRefresh(ARequest: TFDUpdateRequest;
  AColumn: TFDDatSColumn): Boolean;
var
  eAttrs: TFDDataAttributes;
  eOpts: TFDDataOptions;
  eRefreshMode: TFDRefreshMode;
  sName, sAlias: String;
begin
  sName := '';
  sAlias := '';
  eAttrs := [];
  eOpts := [];
  eRefreshMode := FOptions.UpdateOptions.RefreshMode;
  GetColumnAttributes(AColumn, eAttrs, eOpts, sName, sAlias);
  case ARequest of
  arInsert:
    Result := (eRefreshMode = rmAll) or (eRefreshMode = rmOnDemand) and (
        (coAfterInsChanged in eOpts) or
        (caDefault in eAttrs) and VarIsNull(FRow.GetData(AColumn, rvCurrent)) or
        ([caAutoInc, caCalculated, caDefault, caROWID, caRowVersion, caVolatile] * eAttrs <> [])
      );
  arUpdate:
    Result := (eRefreshMode = rmAll) or (eRefreshMode = rmOnDemand) and (
        (coAfterUpdChanged in eOpts) or
        ([caCalculated, caRowVersion, caVolatile] * eAttrs <> [])
      );
  arFetchRow:
    Result :=
      (foAfterIns in FFillRowOptions) and
      // following protects from [SELECT IncFld FROM MyTab WHERE IncFld IS NULL]
      not ((caAutoInc in eAttrs) and (coInKey in eOpts) and (GetIdentity(False) = '')) and
      ((eRefreshMode = rmAll) or (eRefreshMode = rmOnDemand) and (
        (coAfterInsChanged in eOpts) or
        (caDefault in eAttrs) and VarIsNull(FRow.GetData(AColumn, rvCurrent)) or
        ([caAutoInc, caCalculated, caROWID, caRowVersion, caVolatile] * eAttrs <> [])
      )) or
      (foAfterUpd in FFillRowOptions) and
      ((eRefreshMode = rmAll) or (eRefreshMode = rmOnDemand) and (
        (coAfterUpdChanged in eOpts) or
        ([caCalculated, caRowVersion, caVolatile] * eAttrs <> [])
      ));
  else
    Result := False;
  end;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.ColumnSearchable(AColumn: TFDDatSColumn): Boolean;
var
  eAttrs: TFDDataAttributes;
  eOpts: TFDDataOptions;
  sName, sAlias: String;
begin
  sName := '';
  sAlias := '';
  eAttrs := [];
  eOpts := [];
  GetColumnAttributes(AColumn, eAttrs, eOpts, sName, sAlias);
  Result := ColumnStorable(AColumn) and (caSearchable in eAttrs) and
    (eOpts * [coInWhere, coInKey] <> []);
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.ColumnUpdatable(AColumn: TFDDatSColumn;
  AInInsert: Boolean): Boolean;
var
  eAttrs: TFDDataAttributes;
  eOpts: TFDDataOptions;
  sName, sAlias: String;
begin
  sName := '';
  sAlias := '';
  eAttrs := [];
  eOpts := [];
  GetColumnAttributes(AColumn, eAttrs, eOpts, sName, sAlias);
  Result := ColumnStorable(AColumn) and (coInUpdate in eOpts) and
    ([caInternal, caCalculated] * eAttrs = []) and (
      not (caReadOnly in eAttrs) or
      not FOptions.UpdateOptions.CheckReadOnly and not (caAutoInc in eAttrs) or
      (caAutoInc in eAttrs) and AInInsert and FConnMeta.IdentityInsertSupported
    );
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.ColumnChanged(ARow: TFDDatSRow;
  AColumn: TFDDatSColumn): Boolean;
begin
  Result := (ARow = nil) or
    not ARow.CompareColumnVersions(AColumn.Index, rvCurrent, rvOriginal);
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.ColumnIdentityAssigned(ARow: TFDDatSRow;
  AColumn: TFDDatSColumn; AAlready: Boolean): Boolean;
var
  V1, V2: Variant;
begin
  if not AAlready and
     (FOptions.UpdateOptions.FetchGeneratorsPoint <> gpNone) and
     FConnMeta.GeneratorSupported and
     ((AColumn.ActualGenerator <> '') or (FOptions.UpdateOptions.GeneratorName <> '')) then
    Result := True
  else if (ARow <> nil) and ColumnChanged(ARow, AColumn) then begin
    V1 := ARow.GetData(AColumn, rvCurrent);
    V2 := AColumn.ColumnList.CurrValues[AColumn.Index];
    Result := VarIsNull(V1) or (VarIsNull(V2) or (V2 = 0)) or not (
      (AColumn.DataType in C_FD_NumUnsignedTypes) and
        (V1 > 0) and (V2 > 0) and (V1 >= V2) or
      (AColumn.DataType in (C_FD_NumTypes - C_FD_NumUnsignedTypes)) and
        (V1 < 0) and (V2 < 0) and (V1 >= V2));
  end
  else
    Result := False;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.ColumnInKey(AColumn: TFDDatSColumn): Boolean;
var
  eAttrs: TFDDataAttributes;
  eOpts: TFDDataOptions;
  sName, sAlias: String;
begin
  sName := '';
  sAlias := '';
  eAttrs := [];
  eOpts := [];
  GetColumnAttributes(AColumn, eAttrs, eOpts, sName, sAlias);
  Result := coInKey in eOpts;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.AddColumnParam(AColumn: TFDDatSColumn;
  AValueAge: Integer; AType: TParamType): String;
const
  C_ValAge: array[-1 .. 1] of String = ('OLD_', '', 'NEW_');
var
  oParam: TFDParam;
  iSize: LongWord;
  i, iPrecision, iScale: Integer;
  eType: TFieldType;
  sName: String;
  lBeautify: Boolean;

  function GetColumnPath(AColumn: TFDDatSColumn): String;
  var
    oCol: TFDDatSColumn;
  begin
    oCol := AColumn.ParentColumn;
    if oCol <> nil then
      Result := GetColumnPath(oCol)
    else
      Result := 'P';
    Result := Result + '$_' + IntToStr(AColumn.Index);
  end;

begin
  lBeautify := (goBeautify in FGenOptions) and
    ((FTable = nil) or not (ctInvars in FTable.Columns.HasThings));
  if lBeautify then
    Result := C_ValAge[AValueAge] + AColumn.Name
  else
    Result := C_ValAge[AValueAge] + GetColumnPath(AColumn);
  if FParams <> nil then begin
    // Unique parameter name is required for Oracle:
    // INSERT INTO tab (id) VALUES (:NEW_ID) RETURNING id INTO :NEW_ID$#1
    // If :NEW_ID will be used twice, then Oracle connection fails.
    // For details see TFDQACompQRYTsHolder.TestInsertSequence.
    i := 0;
    sName := Result;
    while FParams.FindParam(Result) <> nil do begin
      Inc(i);
      Result := Format('%s$#%d', [sName, i]);
    end;
    oParam := TFDParam(FParams.Add);
    oParam.Position := FParams.Count;
    FOptions.FormatOptions.ColumnDef2FieldDef(AColumn.DataType,
      AColumn.Size, AColumn.Precision, AColumn.Scale,
      AColumn.Attributes, eType, iSize, iPrecision, iScale);
    oParam.Name := Result;
    oParam.ParamType := AType;
    oParam.DataType := eType;
    oParam.ADDataType := AColumn.DataType;
    oParam.Precision := iPrecision;
    oParam.NumericScale := iScale;
    oParam.Size := iSize;
  end;
  if lBeautify then
    Result := ':' + NormalizeColName(Result)
  else
    case FConnMeta.PositionedParamMark of
    prQMark:   Result := '?';
    prNumber:  Result := ':' + IntToStr(FParams.Count - 1);
    prName:    Result := ':' + Result;
    prDollar:  Result := '$' + IntToStr(FParams.Count);
    prQNUmber: Result := '?' + IntToStr(FParams.Count);
    end;
  Result := GetCastColumn(Result, AColumn);
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.ColumnIsHBLOB(AColumn: TFDDatSColumn): Boolean;
begin
  Result := AColumn.DataType in [dtHBlob, dtHBFile, dtHMemo, dtWideHMemo];
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetSelectList(AAllowIdentityExp, AFlatFieldList: Boolean;
  out ANeedFrom: Boolean): String;
var
  iLockMode: TFDLockMode;
  iTotalLen: Integer;

  procedure ProcessDataTableFields(ATable: TFDDatSTable;
    const AParentField: String; var S: String);
  var
    i: Integer;
    sField, sName, sAlias: String;
    eAttrs: TFDDataAttributes;
    eOpts: TFDDataOptions;
    oCol: TFDDatSColumn;
    lIdentity: Boolean;
  begin
    for i := 0 to ATable.Columns.Count - 1 do begin
      oCol := ATable.Columns[i];
      sName := '';
      sAlias := '';
      eAttrs := [];
      eOpts := [];
      GetColumnAttributes(oCol, eAttrs, eOpts, sName, sAlias);
      if AFlatFieldList and (oCol.DataType = dtRowRef) then
        ProcessDataTableFields(oCol.NestedTable, GetColumn(AParentField, -1, oCol), S)
      else if ColumnStorable(oCol) and (
        (
          (caBlobData in oCol.Attributes) and (foBlobs in FFillRowOptions) or
          (oCol.DataType in [dtRowSetRef, dtCursorRef]) and (foDetails in FFillRowOptions) or
          (foData in FFillRowOptions)
        ) and (
          not (foUpdatable in FFillRowOptions) or (
            ColumnUpdatable(oCol, False) or
            (iLockMode = lmPessimistic) and ColumnIsHBLOB(oCol)
          )
        ) and (
          ([foAfterIns, foAfterUpd] * FFillRowOptions = []) or
          // used to generate generic FetchRow SQL
          ([foAfterIns, foAfterUpd] * FFillRowOptions = [foAfterIns, foAfterUpd]) or
          ColumnReqRefresh(arFetchRow, oCol)
        )
      ) then begin
        sField := '';
        lIdentity := False;
        case oCol.DataType of
        dtBlob, dtMemo, dtHBlob, dtHBFile, dtHMemo, dtWideHMemo:
          if foBlobs in FFillRowOptions then begin
            sField := GetColumn(AParentField, -1, oCol);
            ANeedFrom := True;
          end;
        dtRowSetRef, dtCursorRef:
          if foDetails in FFillRowOptions then begin
            sField := GetColumn(AParentField, -1, oCol);
            ANeedFrom := True;
          end;
        else
          if AAllowIdentityExp and (foAfterIns in FFillRowOptions) and
             (caAutoInc in eAttrs) and (GetIdentity(False) <> '') then begin
            if not (foNoIdentity in FFillRowOptions) then begin
              sField := GetIdentity(False);
              if sAlias = '' then
                sAlias := sName;
              lIdentity := True;
            end;
          end
          else begin
            sField := GetColumn(AParentField, -1, oCol);
            ANeedFrom := True;
          end;
        end;
        if sField <> '' then begin
          if S <> '' then
            S := S + ', ';
          if (goBeautify in FGenOptions) and (Length(S) - iTotalLen >= C_FD_CmdGenRight) then begin
            iTotalLen := Length(S);
            S := S + BRK + '  ';
          end;
          S := S + sField;
          if sAlias <> '' then
            S := S + ' AS ' + NormalizeColName(sAlias);
          if lIdentity and not FConnMeta.IdentityInWhere then
            Exit;
        end;
      end;
    end;
  end;

begin
  FCommandPart := cpSELECT;
  iLockMode := FOptions.UpdateOptions.LockMode;
  Result := '';
  ANeedFrom := False;
  iTotalLen := 0;
  if FColumn <> nil then begin
    if not AFlatFieldList and (FColumn.DataType = dtRowRef) then
      ProcessDataTableFields(FColumn.NestedTable, GetColumn('', -1, FColumn), Result)
    else
      Result := GetColumn('', -1, FColumn);
  end
  else
    ProcessDataTableFields(FTable, '', Result);
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetWhere(AInInsert, ARequired, ANotNull: Boolean): String;
var
  eUpdMode: TUpdateMode;
  eRowVersion: TFDDatSRowVersion;
  iTotalLen, iValueAge: Integer;

  procedure ProcessDataTableFields(ATable: TFDDatSTable; ARow: TFDDatSRow;
    const AParentField: String; var S: String);
  var
    i: Integer;
    oCol: TFDDatSColumn;
    eAttrs: TFDDataAttributes;
    eOpts: TFDDataOptions;
    sName, sAlias: String;
  begin
    for i := 0 to ATable.Columns.Count - 1 do begin
      oCol := ATable.Columns[i];
      sName := '';
      sAlias := '';
      eAttrs := [];
      eOpts := [];
      GetColumnAttributes(oCol, eAttrs, eOpts, sName, sAlias);
      if oCol.DataType = dtRowRef then
        ProcessDataTableFields(oCol.NestedTable, GetNestedRow(ARow, i),
          GetColumn(AParentField, -1, oCol), S)
      else if ColumnSearchable(oCol) and
              ((eUpdMode = upWhereAll) or
               (eUpdMode = upWhereChanged) and (ColumnChanged(ARow, oCol) or ColumnInKey(oCol)) or
               (eUpdMode = upWhereKeyOnly) and ColumnInKey(oCol)) and
              not (AInInsert and ([caROWID, caRowVersion] * eAttrs <> [])) and
              not (AInInsert and (coAfterInsChanged in eOpts) and
                   not ((caAutoInc in eAttrs) and (GetIdentity(False) <> ''))) and
              (not ANotNull or (ARow <> nil) and not VarIsNull(ARow.GetData(i, eRowVersion))) and
              not (AInInsert and (caDefault in eAttrs) and VarIsNull(ARow.GetData(i, eRowVersion))) then begin
        if S <> '' then
          S := S + ' AND ';
        if (goBeautify in FGenOptions) and (Length(S) - iTotalLen >= C_FD_CmdGenRight) then begin
          iTotalLen := Length(S);
          S := S + BRK + '  ';
        end;
        if AInInsert and
           (caAutoInc in eAttrs) and (GetIdentity(False) <> '') and
           not ColumnIdentityAssigned(ARow, oCol, True) then begin
          if not FConnMeta.IdentityInWhere then begin
            S := '';
            Exit;
          end;
          S := S + GetColumn(AParentField, -1, oCol) + ' = ' + GetIdentity(False);
        end
        else if (ARow <> nil) and VarIsNull(ARow.GetData(i, eRowVersion)) then
          S := S + GetColumn(AParentField, -1, oCol) + ' IS NULL'
        else
          S := S + GetColumn(AParentField, -1, oCol) + ' = ' + AddColumnParam(oCol,
            iValueAge, ptInput);
      end;
    end;
  end;

begin
  FCommandPart := cpWHERE;
  Result := '';
  if AInInsert then begin
    eRowVersion := rvCurrent;
    iValueAge := 1;
    eUpdMode := upWhereKeyOnly;
  end
  // used to generate generic FetchRow SQL
  else if [foAfterIns, foAfterUpd] * FFillRowOptions = [foAfterIns, foAfterUpd] then begin
    eRowVersion := rvCurrent;
    iValueAge := 0;
    eUpdMode := upWhereKeyOnly;
  end
  else if [foAfterIns, foAfterUpd] * FFillRowOptions = [foAfterUpd] then begin
    eRowVersion := rvCurrent;
    iValueAge := 1;
    eUpdMode := FOptions.UpdateOptions.UpdateMode;
  end
  else begin
    eRowVersion := rvOriginal;
    iValueAge := -1;
    eUpdMode := FOptions.UpdateOptions.UpdateMode;
  end;
  iTotalLen := 0;
  ProcessDataTableFields(FTable, FRow, '', Result);
  if (Result = '') and (eUpdMode = upWhereKeyOnly) then begin
    eUpdMode := upWhereAll;
    ProcessDataTableFields(FTable, FRow, '', Result);
  end;
  if (Result = '') and ARequired then
    FDException(Self, [S_FD_LPhys], er_FD_AccWhereIsEmpty, []);
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GenerateInlineRefresh(const AStmt: String;
  ARequest: TFDUpdateRequest): String;
var
  prevFillOpts: TFDPhysFillRowOptions;
begin
  prevFillOpts := FFillRowOptions;
  Include(FFillRowOptions, foData);
  if ARequest = arInsert then
    Include(FFillRowOptions, foAfterIns)
  else if ARequest = arUpdate then
    Include(FFillRowOptions, foAfterUpd);
  try
    Result := GetInlineRefresh(AStmt, ARequest);
  finally
    FFillRowOptions := prevFillOpts;
  end;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GenerateIdentityInsert(
  const ATable, AStmt: String; ARequest: TFDUpdateRequest): String;
begin
  Result := GetIdentityInsert(ATable, AStmt, ARequest);
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GenerateUpdate: String;
var
  lUpdChngFields: Boolean;
  iTotalLen: Integer;

  procedure ProcessDataTableFields(ATable: TFDDatSTable; ARow: TFDDatSRow;
    const AParentField: String; ARowIndex: Integer; var S: String);
  var
    i, j: Integer;
    oCol: TFDDatSColumn;
    lFldChng: Boolean;
    oRows: TFDDatSNestedRowList;
  begin
    for i := 0 to ATable.Columns.Count - 1 do begin
      oCol := ATable.Columns[i];
      if ColumnUpdatable(oCol, False) then
        if oCol.DataType = dtRowRef then
          ProcessDataTableFields(oCol.NestedTable, GetNestedRow(ARow, i),
            GetColumn(AParentField, ARowIndex, oCol), -1, S)
        else if oCol.DataType = dtRowSetRef then begin
          oRows := ARow.NestedRows[oCol.Index];
          for j := 0 to oRows.Count - 1 do
            ProcessDataTableFields(oCol.NestedTable, oRows[j],
              GetColumn(AParentField, ARowIndex, oCol), j, S)
        end
        else begin
          lFldChng := not lUpdChngFields or ColumnChanged(ARow, oCol);
          if lFldChng then begin
            if S <> '' then
              S := S + ', ';
            if (goBeautify in FGenOptions) and (Length(S) - iTotalLen >= C_FD_CmdGenRight) then begin
              iTotalLen := Length(S);
              S := S + BRK + '  ';
            end;
            S := S + GetColumn(AParentField, ARowIndex, oCol) + ' = ';
            if FConnMeta.InsertHBlobMode = hmSetAfterReturning then
              case oCol.DataType of
              dtHBlob:
                begin
                  Include(FFlags, gfHasHBlob);
                  S := S + 'EMPTY_BLOB()';
                end;
              dtHBFile:
                begin
                  Include(FFlags, gfHasHBlob);
                  S := S + 'BFILENAME(''' + oCol.SourceDirectory + ''', ''' +
                    VarToStr(ARow.GetData(i, rvCurrent)) + ''')';
                end;
              dtHMemo,
              dtWideHMemo:
                begin
                  Include(FFlags, gfHasHBlob);
                  S := S + 'EMPTY_CLOB()';
                end;
              else
                S := S + AddColumnParam(oCol, 1, ptInput);
              end
            else begin
              if ColumnIsHBLOB(oCol) then
                Include(FFlags, gfHasHBlob);
              S := S + AddColumnParam(oCol, 1, ptInput);
            end;
          end;
        end;
    end;
  end;

begin
  Result := '';
  FTabAlias := '';
  FFlags := [];
  FCommandKind := skUnknown;
  lUpdChngFields := FOptions.UpdateOptions.UpdateChangedFields;
  iTotalLen := 0;
  FCommandPart := cpSET;
  ProcessDataTableFields(FTable, FRow, '', -1, Result);
  if Result <> '' then begin
    Result := 'UPDATE ' + GetFrom + BRK + 'SET ' + Result + BRK + 'WHERE ' +
      GetWhere(False, True, False);
    if FConnMeta.InlineRefresh then
      Result := GenerateInlineRefresh(Result, arUpdate);
  end;
  if FCommandKind = skUnknown then
    FCommandKind := skUpdate;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GenerateInsert: String;
var
  s1, s2, s3, sFrom: String;
  lUpdChngFields: Boolean;
  iTotalLen: Integer;

  procedure ProcessDataTableFields(ATable: TFDDatSTable; ARow: TFDDatSRow;
    const AParentField: String; var AFieldList, AValueList, AFirstField: String;
    AForceAddField: Boolean);
  var
    i: Integer;
    oCol: TFDDatSColumn;
    lFldChng: Boolean;
    sName, sAlias, s1, s2, s3, sReserveFirst: String;
    eAttrs: TFDDataAttributes;
    eOpts: TFDDataOptions;
  begin
    sReserveFirst := '';
    for i := 0 to ATable.Columns.Count - 1 do begin
      oCol := ATable.Columns[i];
      sName := '';
      sAlias := '';
      eAttrs := [];
      eOpts := [];
      GetColumnAttributes(oCol, eAttrs, eOpts, sName, sAlias);
      if ColumnUpdatable(oCol, True) then begin
        if oCol.DataType = dtRowRef then begin
          s1 := '';
          s2 := '';
          ProcessDataTableFields(oCol.NestedTable, GetNestedRow(ARow, i),
            GetColumn(AParentField, -1, oCol), s1, s2, AFirstField, True);
          if AFieldList <> '' then begin
            AFieldList := AFieldList + ', ';
            AValueList := AValueList + ', ';
          end;
          AFieldList := AFieldList + GetColumn(AParentField, -1, oCol);
          AValueList := AValueList + GetRowConstructor(s2, oCol);
        end
        else begin
          lFldChng := AForceAddField or
            not lUpdChngFields or
              (caAutoInc in eAttrs) and ColumnIdentityAssigned(ARow, oCol, False) or
              not (caAutoInc in eAttrs) and ColumnChanged(ARow, oCol) or
            ColumnIsHBLOB(oCol);
          if lFldChng then begin
            if AFieldList <> '' then begin
              AFieldList := AFieldList + ', ';
              AValueList := AValueList + ', ';
              if (goBeautify in FGenOptions) and (
                  (Length(AFieldList) - iTotalLen >= C_FD_CmdGenRight) or
                  (Length(AValueList) - iTotalLen >= C_FD_CmdGenRight)
                 ) then begin
                iTotalLen := Length(AFieldList);
                if iTotalLen < Length(AValueList) then
                  iTotalLen := Length(AValueList);
                AFieldList := AFieldList + BRK + '  ';
                AValueList := AValueList + BRK + '  ';
              end;
            end;
            if FConnMeta.InsertHBlobMode = hmSetAfterReturning then
              case oCol.DataType of
              dtHBlob:
                begin
                  Include(FFlags, gfHasHBlob);
                  s3 := 'EMPTY_BLOB()';
                end;
              dtHBFile:
                begin
                  Include(FFlags, gfHasHBlob);
                  s3 := 'BFILENAME(''' + oCol.SourceDirectory + ''', ''' +
                    VarToStr(ARow.GetData(i, rvCurrent)) + ''')';
                end;
              dtHMemo,
              dtWideHMemo:
                begin
                  Include(FFlags, gfHasHBlob);
                  s3 := 'EMPTY_CLOB()';
                end;
              else
                s3 := AddColumnParam(oCol, 1, ptInput);
              end
            else begin
              if ColumnIsHBLOB(oCol) then
                Include(FFlags, gfHasHBlob);
              if oCol.DataType = dtHBFile then
                s3 := 'BFILENAME(''' + oCol.SourceDirectory + ''', ''' +
                  VarToStr(ARow.GetData(i, rvCurrent)) + ''')'
              else
                s3 := AddColumnParam(oCol, 1, ptInput);
            end;
            AFieldList := AFieldList + GetColumn(AParentField, -1, oCol);
            AValueList := AValueList + s3;
          end;
          if caAutoInc in eAttrs then
            if FConnMeta.GeneratorSupported and
               ((oCol.ActualGenerator <> '') or (FOptions.UpdateOptions.GeneratorName <> '')) then begin
              if (FOptions.UpdateOptions.FetchGeneratorsPoint <> gpNone) and
                 not ColumnIdentityAssigned(ARow, oCol, True) then
                Include(FFlags, gfFetchGenerator);
            end
            else begin
              if lFldChng then
                Include(FFlags, gfIdentityInsert);
            end;
          if (AFirstField = '') and (coAllowNull in eOpts) then begin
            if not (caDefault in eAttrs) then
              AFirstField := GetColumn(AParentField, -1, oCol)
            else if sReserveFirst = '' then
              sReserveFirst := GetColumn(AParentField, -1, oCol);
          end;
        end;
      end;
    end;
    if AFirstField = '' then
      AFirstField := sReserveFirst;
  end;

begin
  s1 := '';
  s2 := '';
  s3 := '';
  FTabAlias := '';
  FFlags := [];
  FCommandKind := skUnknown;
  lUpdChngFields := FOptions.UpdateOptions.UpdateChangedFields;
  iTotalLen := 0;
  FCommandPart := cpVALUES;
  ProcessDataTableFields(FTable, FRow, '', s1, s2, s3, False);
  sFrom := GetFrom;
  Result := 'INSERT INTO ' + sFrom + BRK;
  if s1 = '' then
    case FConnMeta.DefValuesSupported of
    dvNone:    Result := Result + '(' + s3 + ') VALUES (NULL)';
    dvDefVals: Result := Result + 'DEFAULT VALUES';
    dvDef:     Result := Result + '(' + s3 + ') VALUES (DEFAULT)';
    end
  else
    Result := Result + '(' + s1 + ')' + BRK + 'VALUES (' + s2 + ')';
  if FConnMeta.InlineRefresh or
     (gfHasHBlob in FFlags) and (FConnMeta.InsertHBlobMode = hmSetAfterReturning) then
    Result := GenerateInlineRefresh(Result, arInsert);
  if (gfIdentityInsert in FFlags) and FConnMeta.IdentityInsertSupported then
    Result := GenerateIdentityInsert(sFrom, Result, arInsert);
  if FCommandKind = skUnknown then
    FCommandKind := skInsert;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GenerateUpdateHBlobs: String;
var
  iTotalLen: Integer;

  procedure ProcessBlobFields(ATable: TFDDatSTable; ARow: TFDDatSRow;
    const AParentField: String; var S: String);
  var
    i: Integer;
    oCol: TFDDatSColumn;
  begin
    for i := 0 to ATable.Columns.Count - 1 do begin
      oCol := ATable.Columns[i];
      if ColumnUpdatable(oCol, False) then
        if oCol.DataType = dtRowRef then
          ProcessBlobFields(oCol.NestedTable, GetNestedRow(ARow, i),
            GetColumn(AParentField, -1, oCol), S)
        else if ColumnIsHBLOB(oCol) and ColumnChanged(ARow, oCol) then begin
          if S <> '' then
            S := S + ', ';
          if (goBeautify in FGenOptions) and (Length(S) - iTotalLen >= C_FD_CmdGenRight) then begin
            iTotalLen := Length(S);
            S := S + BRK + '  ';
          end;
          S := S + GetColumn(AParentField, -1, oCol) + ' = ' +
            AddColumnParam(oCol, 1, ptInput);
        end;
    end;
  end;

begin
  Result := '';
  FTabAlias := '';
  iTotalLen := 0;
  FCommandPart := cpSET;
  ProcessBlobFields(FTable, FRow, '', Result);
  if Result <> '' then
    Result := 'UPDATE ' + GetFrom + BRK + 'SET ' + Result + BRK + 'WHERE ' +
      GetWhere(True, True, False);
  FCommandKind := skUpdate;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GenerateDelete: String;
begin
  FTabAlias := '';
  Result := 'DELETE FROM ' + GetFrom + BRK + 'WHERE ' + GetWhere(False, True, False);
  FCommandKind := skDelete;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GenerateDeleteAll(ANoUndo: Boolean): String;
begin
  FTabAlias := '';
  if ANoUndo and FConnMeta.TruncateSupported then begin
    Result := 'TRUNCATE TABLE ' + GetFrom;
    FCommandKind := skOther;
  end
  else begin
    Result := 'DELETE FROM ' + GetFrom;
    FCommandKind := skDelete;
  end;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GenerateLock: String;
var
  sList: String;
  lNeedFrom: Boolean;
begin
  FCommandKind := skSelect;
  if FOptions.UpdateOptions.LockMode = lmPessimistic then
    Result := GetPessimisticLock;
  if Result = '' then begin
    lNeedFrom := False;
    sList := GetSelectList(False, False, lNeedFrom);
    if sList <> '' then begin
      Result := 'SELECT ' + sList;
      if lNeedFrom then
        Result := Result + BRK + 'FROM ' + GetFrom + BRK + 'WHERE ' +
          GetWhere(False, True, False)
      else if GetSingleRowTable <> '' then
        Result := Result + ' FROM ' + GetSingleRowTable;
    end;
  end;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GenerateUnLock: String;
begin
  Result := '';
  FCommandKind := skUnknown;
  if FOptions.UpdateOptions.LockMode = lmPessimistic then
    Result := GetPessimisticUnLock;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GenerateSelect(ARequired: Boolean): String;
var
  sList, sWhere: String;
  lNeedFrom: Boolean;
begin
  lNeedFrom := False;
  sList := GetSelectList(FConnMeta.SelectWithoutFrom, False, lNeedFrom);
  if sList <> '' then begin
    FCommandKind := skSelect;
    Result := 'SELECT ' + sList;
    if lNeedFrom then begin
      sWhere := GetWhere([foAfterIns, foAfterUpd] * FFillRowOptions = [foAfterIns],
        ARequired, foUnkRec in FFillRowOptions);
      if sWhere = '' then
        Result := ''
      else
        Result := Result + BRK + 'FROM ' + GetFrom + BRK + 'WHERE ' + sWhere;
    end
    else if GetSingleRowTable <> '' then
      Result := Result + ' FROM ' + GetSingleRowTable;
  end;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GenerateSavepoint(const AName: String): String;
begin
  FCommandKind := skOther;
  Result := GetSavepoint(AName);
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GenerateRollbackToSavepoint(const AName: String): String;
begin
  FCommandKind := skOther;
  Result := GetRollbackToSavepoint(AName);
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GenerateCommitSavepoint(const AName: String): String;
begin
  FCommandKind := skOther;
  Result := GetCommitSavepoint(AName);
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GenerateFetchGenerators: String;

  procedure ProcessAutoGenFields(ATable: TFDDatSTable; ARow: TFDDatSRow;
    const AParentField: String; var S: String);
  var
    i: Integer;
    oCol: TFDDatSColumn;
    eAttrs: TFDDataAttributes;
    eOpts: TFDDataOptions;
    sName, sAlias, sGen: String;
  begin
    for i := 0 to ATable.Columns.Count - 1 do begin
      oCol := ATable.Columns[i];
      if ColumnUpdatable(oCol, False) then
        if oCol.DataType = dtRowRef then
          ProcessAutoGenFields(oCol.NestedTable, GetNestedRow(ARow, i),
            GetColumn(AParentField, -1, oCol), S)
        else begin
          sName := '';
          sAlias := '';
          eAttrs := [];
          eOpts := [];
          GetColumnAttributes(oCol, eAttrs, eOpts, sName, sAlias);
          if (caAutoInc in eAttrs) and
             ((oCol.ActualGenerator <> '') or (FOptions.UpdateOptions.GeneratorName <> '')) then begin
            if S <> '' then
              S := S + ', ';
            if oCol.ActualGenerator <> '' then
              sGen := oCol.ActualGenerator
            else
              sGen := FOptions.UpdateOptions.GeneratorName;
            if sAlias = '' then
              sAlias := sName;
            S := S + GetReadGenerator(sGen, NormalizeColName(sAlias), True, False);
          end;
        end;
    end;
  end;

begin
  Result := '';
  if not FConnMeta.GeneratorSupported then
    Exit;
  ProcessAutoGenFields(FTable, FRow, '', Result);
  if Result <> '' then begin
    Result := 'SELECT ' + Result;
    if GetSingleRowTable <> '' then
      Result := Result + BRK + 'FROM ' + GetSingleRowTable;
  end;
  FCommandKind := skSelect;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GenerateReadGenerator(const AName, AAlias: String;
  ANextValue, AFullSelect: Boolean): String;
begin
  FCommandKind := skSelect;
  Result := GetReadGenerator(AName, AAlias, ANextValue, AFullSelect);
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GenerateCall(const AName: String): String;
begin
  FCommandKind := skExecute;
  Result := GetCall(AName);
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GenerateEval(const AExpr: String): String;
begin
  Result := 'SELECT ' + AExpr + ' AS V';
  if not FConnMeta.SelectWithoutFrom then
    Result := Result + ' FROM ' + GetSingleRowTable;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GeneratePing: String;
begin
  FCommandKind := skSelect;
  Result := GetPing;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GenerateSelectTable(ATableParams: TFDPhysTableParams): String;
var
  rName: TFDPhysParsedName;
  sDescFields: String;
  sNocaseFields: String;
  sMastNullFields: String;
  i1: Integer;
  i2: Integer;
  il: Integer;
  sField: String;
  sMasterField: String;
  sTabAlias: String;
  lOrder: Boolean;
  sSQLSelect: String;
  sSQLFrom: String;
  sSQLWhere: String;
  sSQLOrderBy: String;
  sSQLLimit: String;
  sSQLField: String;
  asKeys: array of String;
  asSQLKeys: array of String;
  sLocateField: String;
  sLocateParam: String;
  sIndexFields: String;
  oCol: TFDDatSColumn;

  {-----------------------------------------------------------------------------}
  function ParamName(const AName: String): String;
  begin
    Result := NormalizeColName(AName);
  end;

  {-----------------------------------------------------------------------------}
  function EncloseBrackets(const AString: String): String;
  begin
    if AString <> '' then
      Result := '(' + AString + ')'
    else
      Result := '';
  end;

  {-----------------------------------------------------------------------------}
  procedure AddWhere(const ACond: String);
  begin
    if ACond = '' then
      Exit;
    if sSQLWhere <> '' then
      sSQLWhere := sSQLWhere + ' AND' + BRK;
    sSQLWhere := sSQLWhere + EncloseBrackets(ACond);
  end;

  {-----------------------------------------------------------------------------}
  procedure AddOrderBy(const AFieldName: String; AAscending: Boolean);
  begin
    if AFieldName = '' then
      Exit;
    if sSQLOrderBy <> '' then
      sSQLOrderBy := sSQLOrderBy + ', ';
    sSQLOrderBy := sSQLOrderBy + AFieldName;
    if AAscending then
      sSQLOrderBy := sSQLOrderBy + ' ASC'
    else
      sSQLOrderBy := sSQLOrderBy + ' DESC'
  end;

  {-----------------------------------------------------------------------------}
  function GetRange(const AFieldName, ASQLFieldName: String; AExclusive: Boolean): String;
  var
    vStartKeyValue: Variant;
    vEndKeyValue: Variant;
    lEQ: Boolean;
  begin
    Result := '';
    if Assigned(ATableParams.FRangeStartRow) then
      vStartKeyValue := ATableParams.FRangeStartRow.GetData(AFieldName, rvDefault)
    else
      vStartKeyValue := Null;
    if Assigned(ATableParams.FRangeEndRow) then
      vEndKeyValue := ATableParams.FRangeEndRow.GetData(AFieldName, rvDefault)
    else
      vEndKeyValue := Null;

    // no range borders set - exit
    if VarIsNull(vStartKeyValue) and VarIsNull(vEndKeyValue) then
      Exit;

    // both range border are set
    if not VarIsNull(vStartKeyValue) and not VarIsNull(vEndKeyValue) then
      if AExclusive then
        // exclude border
        Result := ASQLFieldName + ' > :' + ParamName(C_FD_CmdGenRangeStart + AFieldName) + ' AND ' +
          ASQLFieldName + ' < :' + ParamName(C_FD_CmdGenRangeFinish + AFieldName)
      else begin
        // include border
        try
          lEQ := vStartKeyValue = vEndKeyValue;
        except
          lEQ := False;
        end;
        // BETWEEN may be more slow than EQUAL
        if lEQ then
          Result := ASQLFieldName + ' = :'  + ParamName(C_FD_CmdGenRangeStart + AFieldName)
        else
          Result := ASQLFieldName + ' BETWEEN :'  + ParamName(C_FD_CmdGenRangeStart + AFieldName) + ' AND :' +
            ParamName(C_FD_CmdGenRangeFinish + AFieldName);
      end;

    // upper border is not set
    if VarIsNull(vStartKeyValue) and not VarIsNull(vEndKeyValue) then
      if AExclusive then
        // exclude border
        Result := ASQLFieldName + ' < :' + ParamName(C_FD_CmdGenRangeFinish + AFieldName)
	    else
        // include border
        Result := ASQLFieldName + ' <= :' + ParamName(C_FD_CmdGenRangeFinish + AFieldName);

    // lower border is not set
    if not VarIsNull(vStartKeyValue) and VarIsNull(vEndKeyValue) then
      if AExclusive then
	      // exclude border
        Result := ASQLFieldName + ' > :' + ParamName(C_FD_CmdGenRangeStart + AFieldName)
      else
	      // include border
        Result := ASQLFieldName + ' >= :' + ParamName(C_FD_CmdGenRangeStart + AFieldName);
  end;

  {-----------------------------------------------------------------------------}
  // When "down":
  //   (f1 > :p1) or
  //   (f1 = :p1) and (f2 > :p2) or
  //   (f1 = :p1) and (f2 = :p2) and (f3 > :p3)
  function GetWindow(const AKeys, ASQLKeys: array of String; ADown, AInclude: Boolean): String;
  var
    sKey, sSQLKey: String;
    sKeyItem, sCond: String;
    oCol: TFDDatSColumn;
    nKeys, iKeyCount, i: Integer;
    lIsNull, lIncluded: Boolean;
  begin
    Result := '';
    lIncluded := False;
    nKeys := Length(AKeys);
    if (ATableParams.FKeyFieldCount > 0) and (ATableParams.FKeyFieldCount < nKeys) then
      nKeys := ATableParams.FKeyFieldCount;

    for iKeyCount := nKeys - 1 downto 0 do begin

      sKeyItem := '';
      for i := 0 to iKeyCount do begin
        sKey := AKeys[i];
        sSQLKey := ASQLKeys[i];
        oCol := FRow.Table.Columns.ColumnByName(sKey);
        lIsNull := VarIsNull(FRow.GetData(oCol.Index, rvDefault));
        if sKeyItem <> '' then
		      sKeyItem := sKeyItem + ' AND ';

        if i < iKeyCount then
          if lIsNull then
            sKeyItem := sKeyItem + sSQLKey + ' IS NULL'
          else
            sKeyItem := sKeyItem + EncloseBrackets(sSQLKey + ' = :' +
              ParamName(C_FD_CmdGenWindow + sKey))

        else begin
          if lIsNull then
            if AInclude and not lIncluded then begin
              sKeyItem := sKeyItem + sSQLKey + ' IS NULL';
              lIncluded := True;
            end
            else if (caAllowNull in oCol.Attributes) and (
                      ADown and (nlAscFirst in ATableParams.FNullLocation) or
                      not ADown and (nlAscLast in ATableParams.FNullLocation)) then
              sKeyItem := sKeyItem + sSQLKey + ' IS NOT NULL'
            else begin
              sKeyItem := '';
              lIncluded := False;
              Break;
            end

          else begin
            if AInclude and not lIncluded then begin
              if ADown then
                sCond := ' >= '
              else
                sCond := ' <= ';
              lIncluded := True;
            end
            else
              if ADown then
                sCond := ' > '
              else
                sCond := ' < ';
            sCond := sSQLKey + sCond + ':' + ParamName(C_FD_CmdGenWindow + sKey);
            if (caAllowNull in oCol.Attributes) and (
                  ADown and (nlAscLast in ATableParams.FNullLocation) or
                  not ADown and (nlAscFirst in ATableParams.FNullLocation)) then
              sCond := sCond + ' OR ' + sSQLKey + ' IS NULL';
            sKeyItem := sKeyItem + EncloseBrackets(sCond);
          end;
        end;
      end;

      if sKeyItem <> '' then begin
        if Result <> '' then
          Result := Result + ' OR ';
        Result := Result + EncloseBrackets(sKeyItem);
      end;
    end;
    if Result = '' then
      Result := '(0 = 1)';
  end;

  {-----------------------------------------------------------------------------}
  function GetWindowUp(const AKeys, ASQLKeys: array of String; AInclude: Boolean = False): String;
  begin
    Result := GetWindow(AKeys, ASQLKeys, False, AInclude);
  end;

  {-----------------------------------------------------------------------------}
  function GetWindowDown(const AKeys, ASQLKeys: array of String; AInclude: Boolean = False): String;
  begin
    Result := GetWindow(AKeys, ASQLKeys, True, AInclude);
  end;

  {-----------------------------------------------------------------------------}
  function BuildFilter(const AFlt: String): String;
  begin
                                                     
    Result := AFlt;
  end;

begin
  if FTabAlias <> '' then
    sTabAlias := FTabAlias + '.'
  else
    sTabAlias := '';
  Result := '';
  sSQLWhere := '';
  sSQLOrderBy := '';
  sSQLSelect := '';

  // Build SELECT list
  if not (ATableParams.FTableCommand in [tcGetRecNo, tcGetRowCount]) then begin
    if ATableParams.FSelectFields = '' then
      sSQLSelect := sTabAlias + '*'
    else begin
      i1 := 1;
      while i1 <= Length(ATableParams.FSelectFields) do begin
        sField := FDExtractFieldName(ATableParams.FSelectFields, i1);
        sSQLField := NormalizeColName(sField);
        if sSQLSelect <> '' then
          sSQLSelect := sSQLSelect + ', ';
        sSQLSelect := sSQLSelect + sTabAlias + sSQLField;
      end;
    end;

    // Add Unique Row Identifier, depending on a DBMS
    case FConnMeta.Kind of
    mkOracle,
    mkADS,
    mkSQLite:    sSQLSelect := sSQLSelect + ', ' + sTabAlias + 'ROWID AS ' + C_FD_SysColumnPrefix + 'ROWID';
    mkInterbase,
    mkFirebird:  sSQLSelect := sSQLSelect + ', ' + sTabAlias + 'RDB$DB_KEY AS ' + C_FD_SysColumnPrefix + 'DB_KEY';
    end;
  end
  else
    sSQLSelect := 'COUNT(*)';

  // Build FROM
  rName.FCatalog := ATableParams.FCatalog;
  rName.FSchema := ATableParams.FSchema;
  rName.FObject := ATableParams.FTable;
  sSQLFrom := FConnMeta.EncodeObjName(rName, FCommand, [eoBeautify]);
  if FTabAlias <> '' then
    sSQLFrom := sSQLFrom + ' ' + FTabAlias;

  // merge primary key fields when IndexFields does not contain them
  sIndexFields := FDMergeFieldNames(ATableParams.FIndexFields, ATableParams.FPrimaryKeyFields);

  // Do all tasks in one loop
  if sIndexFields <> '' then begin
    sDescFields := ';' + UpperCase(ATableParams.FDescFields) + ';';
    sNocaseFields := ';' + UpperCase(ATableParams.FNoCaseFields) + ';';
    sMastNullFields := ';' + UpperCase(ATableParams.FMasterNullFields) + ';';

    i1 := 1;
    i2 := 1;
    while i1 <= Length(sIndexFields) do begin
      sField := FDExtractFieldName(sIndexFields, i1);
      sSQLField := sTabAlias + NormalizeColName(sField);
      // Build master / detail WHERE part
      if i2 <= Length(ATableParams.FMasterFields) then begin
        sMasterField := FDExtractFieldName(ATableParams.FMasterFields, i2);
        if Pos(';' + UpperCase(sMasterField) + ';', sMastNullFields) <> 0 then
          AddWhere(sSQLField + ' IS NULL')
        else
          AddWhere(sSQLField + ' = :' + ParamName(sMasterField));
      end

      // Build range WHERE part
      else if ATableParams.FRanged then
        AddWhere(GetRange(sField, sSQLField, ATableParams.FExclusive));

      // Build FindKey WHERE part
      if ATableParams.FTableCommand = tcFindKey then
        if not VarIsNull(FRow.GetData(sField, rvDefault)) then
          AddWhere(sSQLField + ' = :' + ParamName(C_FD_CmdGenWindow + sField));

      // Build ORDER BY
      // Note, when table has record with every index fields is null,
      // then window works correctly only when all sort orders are ASC

      // No need for ORDER BY in case of GetRecNo or GetRowCount
      if not (ATableParams.FTableCommand in [tcGetRecNo, tcGetRowCount]) then begin
        // When window command is Eof, PageUp or LocateBackward, then ORDER BY is inverted
        lOrder := Pos(';' + UpperCase(sField) + ';', sDescFields) = 0;
        if (ATableParams.FTableCommand in [tcPageUp, tcEof]) or
           ((ATableParams.FTableCommand = tcLocate) and ATableParams.FLocateBackward) then
          lOrder := not lOrder;

        if (Pos(';' + UpperCase(sField) + ';', sNocaseFields) <> 0) {and
           (ATableParams.FRow.Table.Columns.ColumnByName(sField).DataType in C_FD_CharTypes)} then
          AddOrderBy('{FN UCASE(' + sSQLField + ')}', lOrder)
        else
          AddOrderBy(sSQLField, lOrder);
      end;

      // collect key fields
      il := Length(asKeys);
      SetLength(asKeys, il + 1);
      SetLength(asSQLKeys, il + 1);
      asKeys[il] := sField;
      asSQLKeys[il] := sSQLField;
    end;

    // Build key tree for window (if needed)
    case ATableParams.FTableCommand of
    tcPageUp:
      AddWhere(GetWindowUp(asKeys, asSQLKeys));

    tcPageDown:
      AddWhere(GetWindowDown(asKeys, asSQLKeys));

    tcFindNearest:
      AddWhere(GetWindowDown(asKeys, asSQLKeys, not ATableParams.FExclusive));

    tcLocate:
      if ATableParams.FLocateFromCurrent then
        if ATableParams.FLocateBackward then
          AddWhere(GetWindowUp(asKeys, asSQLKeys))
        else
          AddWhere(GetWindowDown(asKeys, asSQLKeys));

    tcGetRecNo:
      AddWhere(GetWindowUp(asKeys, asSQLKeys, True));
    end;
  end
  else if ATableParams.FRecordCount > 0 then
    // expression index is not supported
    FDCapabilityNotSupported(Self, [S_FD_LPhys]);

  // Build filter
  if ATableParams.FFiltered then
    // Currently just insert filter expression into WHERE
    AddWhere(BuildFilter(ATableParams.FFilter));

  // Build locate WHERE part
  if ATableParams.FTableCommand = tcLocate then begin
    if ATableParams.FLocateExpression = '' then begin
      i1 := 1;
      while i1 <= Length(ATableParams.FLocateFields) do begin
        sField := FDExtractFieldName(ATableParams.FLocateFields, i1);
        sSQLField := sTabAlias + NormalizeColName(sField);
        oCol := ATableParams.FLocateRow.Table.Columns.ColumnByName(sField);

        // NULL handling
        if VarIsNull(ATableParams.FLocateRow.GetData(oCol.Index, rvDefault)) then begin
          sLocateField := sSQLField;
          AddWhere(sLocateField + ' IS NULL');
        end

        // not NULL handling
        else begin
          // use UCASE for the case-insensitive compare
          if ATableParams.FLocateIgnoreCase and (oCol.DataType in C_FD_CharTypes) then begin
            sLocateField := '{FN UCASE(' + sSQLField + ')}';
            sLocateParam := '{FN UCASE('+ ':' + ParamName(C_FD_CmdGenLocate + sField) + ')}';
          end
          else begin
            sLocateField := sSQLField;
            sLocateParam := ':' + ParamName(C_FD_CmdGenLocate + sField);
          end;

          // use LIKE for partial compare
          if ATableParams.FLocatePartial and (oCol.DataType in C_FD_CharTypes) then begin
            // Firebird, VC(N) LIKE MASK(N+1) gives "string right truncation"
            if FConnMeta.Kind in [mkInterbase, mkFirebird] then
              sLocateField := '{FN CONVERT(' + sLocateField + ', VARCHAR(250))}';
		        AddWhere(sLocateField + ' LIKE ' + sLocateParam);
          end
          else
            AddWhere(sLocateField + ' = ' + sLocateParam);
		    end;
      end;
      // lxoFromCurrent option has been attached in window section
    end
    else
      AddWhere(ATableParams.FLocateExpression);
  end;

  // Build CurrentRecord WHERE part
  if ATableParams.FTableCommand = tcCurrentRecord then begin
    i1 := 1;
    while i1 <= Length(ATableParams.FPrimaryKeyFields) do begin
      sField := FDExtractFieldName(ATableParams.FPrimaryKeyFields, i1);
      sSQLField := sTabAlias + NormalizeColName(sField);
      if not VarIsNull(FRow.GetData(sField, rvDefault)) then
        AddWhere(sSQLField + ' = :' + ParamName(C_FD_CmdGenWindow + sField));
    end;
  end;

  // Add custom WHERE
  if ATableParams.FCustomWhere <> '' then
    AddWhere(ATableParams.FCustomWhere);

  // Do not use {LIMIT} when ODBC / dbExp bridge or DBMS does not support {LIMIT}
  if (loRows in FConnMeta.LimitOptions) and not (FConnMeta.Kind in [mkUnknown, mkOther]) and
     (ATableParams.FRecordCount > 0) then
    // Append LIMIT macro. Limit is recommended to set.
    if ATableParams.FTableCommand = tcSetRecNo then
      // LIMIT works from zero
      sSQLLimit := Format('{LIMIT(%d,1)}', [ATableParams.FRecordNumber - 1])
    else
      sSQLLimit := Format('{LIMIT(%d)}', [ATableParams.FRecordCount]);

                                                              
  // Access: --
  // Advantage: --
  // DB2: SELECT ... OPTIMIZE FOR 100 ROWS
  // Firebird: --
  // Interbase: --
  // MySQL: --
  // Oracle: SELECT /*+ first_rows(100) */ ...
  // PostgreSQL: --
  // SQL Anywhere:
  // SQL Server: SELECT ... ORDER BY ... OPTION (FAST 100)
  // SQLite: --

  // Final SQL Text
  Result := 'SELECT ' + sSQLSelect + BRK + 'FROM ' + sSQLFrom;
  if sSQLWhere <> '' then
    Result := Result + BRK + 'WHERE ' + sSQLWhere;
  if sSQLOrderBy <> '' then
    Result := Result + BRK + 'ORDER BY ' + sSQLOrderBy;
  if sSQLLimit <> '' then
    Result := Result + BRK + sSQLLimit;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GenerateStoredProcParams(const ACatalog, ASchema,
  APackage, AProc: String; AOverload: Word = 0): String;
begin
  FParams.Clear;
  Result := GetStoredProcParams(ACatalog, ASchema, APackage, AProc, AOverload);
  if (Result <> '') and (FCommandKind = skUnknown) then
    FCommandKind := skStoredProc;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GenerateStoredProcCall(const ACatalog, ASchema,
  APackage, AProc: String; ASPUsage: TFDPhysCommandKind = skStoredProc): String;
begin
  Result := GetStoredProcCall(ACatalog, ASchema, APackage, AProc, ASPUsage);
  if (FCommandKind = skStoredProc) and
     (ASPUsage in [skStoredProcWithCrs, skStoredProcNoCrs]) then
    FCommandKind := ASPUsage;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GenerateSelectMetaInfo(AKind: TFDPhysMetaInfoKind;
  const ACatalog, ASchema, ABaseObject, AObject, AWildcard: String;
  AObjectScopes: TFDPhysObjectScopes; ATableKinds: TFDPhysTableKinds;
  AOverload: Word): String;
begin
  FCommandKind := skUnknown;
  Result := GetSelectMetaInfo(AKind, ACatalog, ASchema, ABaseObject, AObject,
    AWildcard, AObjectScopes, ATableKinds, AOverload);
  if (Result <> '') and (FCommandKind = skUnknown) then
    FCommandKind := skSelect;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GenerateLimitSelect(const ASQL: String;
  ASkip, ARows: Integer; AOneMore: Boolean): String;
begin
  if ((ARows >= 0) or (ASkip >= 0)) and (FCommandKind in [skUnknown, skSelect]) and
     ((CompareText(Copy(ASQL, 1, 6), 'SELECT') = 0) or (CompareText(Copy(ASQL, 1, 4), 'WITH') = 0)) then begin
    if AOneMore and (ARows >= 0) then
      Inc(ARows);
    if ASkip < 0 then
      ASkip := 0;
    if ARows < 0 then
      ARows := MAXINT - ASkip;
    Result := GetLimitSelect(ASQL, ASkip, ARows);
    // ORDER BY cannot be striped anymore, because user asked for TOP N rows
    // using currently specified ORDER BY order
    FSQLOrderByPos := 0;
  end
  else
    Result := ASQL;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GenerateCountSelect(const ASQL: String): String;
begin
  if FSQLOrderByPos <> 0 then begin
    Result := Copy(ASQL, 1, FSQLOrderByPos - 1);
    FSQLOrderByPos := 0;
  end
  else
    Result := ASQL;
  FCommandKind := skSelect;
  Result := GetCountSelect(Result);
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetReturning(ARequest: TFDUpdateRequest;
  AWithInto: Boolean): String;
var
  s1, s2: String;
  i: Integer;
  oCols: TFDDatSColumnList;
  oCol: TFDDatSColumn;
  lRefresh, lPutBlob: Boolean;
  eParType: TParamType;
begin
  FCommandPart := cpRETURNING;
  s1 := '';
  s2 := '';
  oCols := FTable.Columns;
  for i := 0 to oCols.Count - 1 do begin
    oCol := oCols[i];
    if ColumnStorable(oCol) then begin
      lRefresh := ColumnReqRefresh(ARequest, oCol);
      lPutBlob := (FConnMeta.InsertHBlobMode = hmSetAfterReturning) and
        ColumnIsHBLOB(oCol) and ColumnChanged(FRow, oCol) and (oCol.DataType <> dtHBFile);
      if lRefresh or lPutBlob then begin
        if s1 <> '' then begin
          s1 := s1 + ', ';
          if AWithInto then
            s2 := s2 + ', ';
        end;
        s1 := s1 + GetColumn('', -1, oCol);
        if AWithInto then begin
          // if Insert then
          //   after exec put
          // else if Update then
          //   if Set then
          //     after exec put
          //   else if Refr then
          //     after exec get
          // else
          //   after exec get
          if lPutBlob then
            eParType := ptInput
          else if lRefresh then
            eParType := ptOutput
          else
            eParType := ptUnknown;
          s2 := s2 + AddColumnParam(oCol, 1, eParType);
        end;
      end;
    end;
  end;
  if s1 <> '' then begin
    Result := BRK + 'RETURNING ' + s1;
    if AWithInto then
      Result := Result + ' INTO ' + s2;
  end
  else
    Result := '';
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetInlineRefresh(const AStmt: String;
  ARequest: TFDUpdateRequest): String;
begin
  // overridden by MSSQL, Oracle, DB2, IB descendant classes
  Result := AStmt;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetIdentityInsert(const ATable, AStmt: String;
  ARequest: TFDUpdateRequest): String;
begin
  // overridden by MSSQL descendant class
  Result := AStmt;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetIdentity(ASessionScope: Boolean): String;
begin
  // overridden by MSSQL, MySQL, ADS descendant classes
  Result := '';
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetSingleRowTable: String;
begin
  // overridden by Oracle, ADS, IB, DB2 descendant classes
  Result := '';
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetPessimisticLock: String;
begin
  // overridden by Oracle, MSSQL, MySQL, IB descendant classes
  Result := '';
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetPessimisticUnLock: String;
begin
  // overridden by ADS descendant classes
  Result := '';
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetRollbackToSavepoint(const AName: String): String;
begin
  // overridden by Oracle, MSSQL, MySQL, IB, PGSQL, SQLite descendant classes
  Result := '';
  FDCapabilityNotSupported(Self, [S_FD_LPhys]);
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetCommitSavepoint(const AName: String): String;
begin
  // overridden by DB2, IB, SQLite descendant classes
  Result := '';
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetSavepoint(const AName: String): String;
begin
  // overridden by Oracle, MSSQL, MySQL, IB, PGSQL, SQLite descendant classes
  Result := '';
  FDCapabilityNotSupported(Self, [S_FD_LPhys]);
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetReadGenerator(const AName, AAlias: String;
  ANextValue, AFullSelect: Boolean): String;
var
  sIdent: String;
begin
  if ANextValue or (AName <> '') then
    FDCapabilityNotSupported(Self, [S_FD_LPhys]);
  FCommandPart := cpSELECT;
  sIdent := GetIdentity(True);
  if sIdent <> '' then begin
    Result := sIdent;
    if AAlias <> '' then
      Result := Result + ' AS ' + AAlias;
    if AFullSelect then begin
      Result := 'SELECT ' + Result;
      if GetSingleRowTable <> '' then begin
        FCommandPart := cpFROM;
        Result := Result + ' FROM ' + GetSingleRowTable;
      end;
    end;
  end
  else
    FDCapabilityNotSupported(Self, [S_FD_LPhys]);
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetCall(const AName: String): String;
begin
  // overridden by all descendant classes
  Result := '';
  FDCapabilityNotSupported(Self, [S_FD_LPhys]);
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetPing: String;
begin
  // overridden by all descendant classes
  Result := '';
  FDCapabilityNotSupported(Self, [S_FD_LPhys]);
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetStoredProcParams(const ACatalog, ASchema,
  APackage, AProc: String; AOverload: Word): String;
const
  ResultParam = 'Result';
var
  oView: TFDDatSView;
  i, j, iPosDelta: Integer;
  oPar: TFDParam;
  V: Variant;
  iDestDataType: TFieldType;
  iSrcSize, iDestSize: LongWord;
  iSrcPrec, iSrcScale, iDestPrec, iDestScale: Integer;
  iSrcADDataType, iDestADDataType: TFDDataType;
  iSrcAttrs: TFDDataAttributes;
  lHasCursors, lMayHaveCursor, lUnifyParams: Boolean;
  oRow: TFDDatSRow;
  oFmt: TFDFormatOptions;
begin
  oFmt := FOptions.FormatOptions;
  oView := FConnMeta.GetProcArgs(ACatalog, ASchema, APackage, AProc, '', AOverload);
  try
    lHasCursors := False;
    lMayHaveCursor := False;
    iPosDelta := 0;
    lUnifyParams := FOptions.ResourceOptions.UnifyParams;
    for i := 0 to oView.Rows.Count - 1 do begin
      oRow := oView.Rows[i];
      oPar := FParams.Add;

      V := oRow.GetData(7, rvDefault);
      if not VarIsNull(V) then
        oPar.Position := V - iPosDelta
      else
        oPar.Position := 0;

      V := oRow.GetData(8, rvDefault);
      if not VarIsNull(V) then
        oPar.ParamType := V;

      V := oRow.GetData(6, rvDefault);
      if VarIsNull(V) then
        oPar.Name := ResultParam
      else
        oPar.Name := V;

      if lUnifyParams then
        if (oPar.Position = 1) and (oPar.ParamType = ptResult) and
           (oPar.Name = '@RETURN_VALUE') then begin
          FDFree(oPar);
          iPosDelta := 1;
          Continue;
        end
        else if Copy(oPar.Name, 1, 1) = '@' then
          oPar.Name := Copy(oPar.Name, 2, MAXINT);

      V := oRow.GetData(9, rvDefault);
      if not VarIsNull(V) then
        iSrcADDataType := TFDDataType(Integer(V))
      else
        iSrcADDataType := dtUnknown;

      V := oRow.GetData(11, rvDefault);
      if not VarIsNull(V) then begin
        j := Integer(V);
        iSrcAttrs := TFDDataAttributes(Pointer(@j)^);
      end
      else
        iSrcAttrs := [];

      V := oRow.GetData(14, rvDefault);
      if not VarIsNull(V) then
        iSrcSize := V
      else
        iSrcSize := 0;

      V := oRow.GetData(12, rvDefault);
      if not VarIsNull(V) then
        iSrcPrec := V
      else
        iSrcPrec := 0;

      V := oRow.GetData(13, rvDefault);
      if not VarIsNull(V) then
        iSrcScale := V
      else
        iSrcScale := 0;

      iDestADDataType := dtUnknown;
      iDestDataType := ftUnknown;
      iDestSize := 0;
      iDestPrec := 0;
      oFmt.ResolveDataType(oPar.Name, iSrcADDataType, iSrcSize, iSrcPrec, iSrcScale,
        iDestADDataType, iSrcSize, True);
      oFmt.ColumnDef2FieldDef(iDestADDataType, iSrcSize, iSrcPrec, iSrcScale,
        iSrcAttrs, iDestDataType, iDestSize, iDestPrec, iDestScale);

      oPar.DataType := iDestDataType;
      if iDestADDataType = dtWideHMemo then
        oPar.ADDataType := iDestADDataType;
      oPar.Size := iDestSize;
      oPar.Precision := iDestPrec;
      oPar.NumericScale := iDestScale;

      lHasCursors := lHasCursors or (iDestDataType in [ftCursor, ftDataSet]);
      lMayHaveCursor := lMayHaveCursor or ((oPar.ParamType = ptOutput) and
        (FConnMeta.Kind in [mkInterbase, mkFirebird, mkADS, mkPostgreSQL]));
    end;
    if lHasCursors then
      FCommandKind := skStoredProcWithCrs
    else if not lMayHaveCursor then
      FCommandKind := skStoredProcNoCrs;
  finally
    FDClearMetaView(oView, FOptions.FetchOptions);
  end;
  Result := '';
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetStoredProcCall(const ACatalog, ASchema,
  APackage, AProc: String; ASPUsage: TFDPhysCommandKind): String;
var
  i: Integer;
  oParam: TFDParam;
  rName: TFDPhysParsedName;
  lWasParam: Boolean;
begin
  // overridden by Oracle, MySQL, IB, PGSQL descendant classes
  Result := '{';
  for i := 0 to FParams.Count - 1 do
    if FParams[i].ParamType = ptResult then begin
      Result := Result + '? = ';
      Break;
    end;
  Result := Result + 'CALL ';

  rName.FCatalog := ACatalog;
  rName.FSchema := ASchema;
  rName.FBaseObject := APackage;
  rName.FObject := AProc;
  Result := Result + FConnMeta.EncodeObjName(rName, FCommand, [eoQuote, eoNormalize]);

  lWasParam := False;
  for i := 0 to FParams.Count - 1 do begin
    oParam := FParams[i];
    if oParam.ParamType <> ptResult then begin
      if lWasParam then
        Result := Result + ', '
      else begin
        Result := Result + '(';
        lWasParam := True;
      end;
      if oParam.ArrayType = atPLSQLTable then
        Result := Result + '{RESULTSET ' + IntToStr(oParam.ArraySize) + ', ' + oParam.SQLName + '}'
      else
        Result := Result + '?';
    end;
  end;

  if lWasParam then
    Result := Result + ')';
  Result := Result + '}';
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetSelectMetaInfo(AKind: TFDPhysMetaInfoKind;
  const ACatalog, ASchema, ABaseObject, AObject, AWildcard: String;
  AObjectScopes: TFDPhysObjectScopes; ATableKinds: TFDPhysTableKinds;
  AOverload: Word): String;
var
  lWasWhere: Boolean;
  eParts: TFDPhysNameParts;

  procedure AddWhere(const ACond: String; const AParam: String = '');
  var
    oParam: TFDParam;
  begin
    if lWasWhere then
      Result := Result + ' AND ' + ACond
    else begin
      Result := Result + ' WHERE ' + ACond;
      lWasWhere := True;
    end;
    if AParam <> '' then begin
      case FConnMeta.NamedParamMark of
      prQMark:   Result := Result + '?';
      prName:    Result := Result + ':' + AParam;
      prNumber:  Result := Result + ':' + IntToStr(GetParams.Count + 1);
      prDollar:  Result := Result + '$' + IntToStr(GetParams.Count + 1);
      prQNumber: Result := Result + '?' + IntToStr(GetParams.Count + 1);
      end;
      oParam := GetParams.Add;
      oParam.Name := AParam;
      oParam.DataType := ftString;
      oParam.Size := 70;
    end;
  end;

  function FK_EncodeRule(const ARuleField: String): String;
  begin
    Result :=
      'CASE' +
      ' WHEN ' + ARuleField + ' = ''CASCADE'' THEN ' + IntToStr(Integer(ckCascade)) +
      ' WHEN ' + ARuleField + ' = ''SET NULL'' THEN ' + IntToStr(Integer(ckSetNull)) +
      ' WHEN ' + ARuleField + ' = ''RESTRICT'' THEN ' + IntToStr(Integer(ckRestrict)) +
      ' WHEN ' + ARuleField + ' = ''SET DEFAULT'' THEN ' + IntToStr(Integer(ckSetDefault)) +
      ' ELSE ' + IntToStr(Integer(ckNone)) +
      ' END';
  end;

  function GetFK_TABLE_CATALOG: String;
  begin
    if FConnMeta.Kind <> mkMySQL then
      Result := 'B.TABLE_CATALOG'
    else
      Result := 'B.TABLE_SCHEMA';
  end;

  function GetFK_TABLE_SCHEMA: String;
  begin
    if FConnMeta.Kind <> mkMySQL then
      Result := 'B.TABLE_SCHEMA'
    else
      Result := '''''';
  end;

  function GetFK_PKEY_CONSTRAINT_CATALOG: String;
  begin
    if FConnMeta.Kind <> mkMySQL then
      Result := 'C.TABLE_CATALOG'
    else
      Result := 'C.TABLE_SCHEMA';
  end;

  function GetFK_PKEY_CONSTRAINT_SCHEMA: String;
  begin
    if FConnMeta.Kind <> mkMySQL then
      Result := 'C.TABLE_SCHEMA'
    else
      Result := '''''';
  end;

  function GetFK_REFERENCED_COLUMN: String;
  begin
    if FConnMeta.Kind <> mkMySQL then
      Result := 'E.COLUMN_NAME'
    else
      Result := 'D.REFERENCED_COLUMN_NAME';
  end;


begin
  // overridden by descendant classes
  Result := '';
  eParts := FConnMeta.NameParts;
  if FConnMeta.Kind = mkMySQL then
    eParts := eParts - [npCatalog] + [npSchema];
  case AKind of
  mkForeignKeys:
    begin
      Result := 'SELECT 0 AS RECNO, ' +
        GetFK_TABLE_CATALOG + ' AS CATALOG_NAME, ' +
        GetFK_TABLE_SCHEMA + ' AS SCHEMA_NAME, ' +
        'B.TABLE_NAME, ' +
        'A.CONSTRAINT_NAME AS FKEY_NAME, ' +
        GetFK_PKEY_CONSTRAINT_CATALOG + ' AS PKEY_CATALOG_NAME, ' +
        GetFK_PKEY_CONSTRAINT_SCHEMA + ' AS PKEY_SCHEMA_NAME, ' +
        'C.TABLE_NAME AS PKEY_TABLE_NAME, ' +
        FK_EncodeRule('A.DELETE_RULE') + ' AS DELETE_RULE, ' +
        FK_EncodeRule('A.UPDATE_RULE') + ' AS UPDATE_RULE ' +
        'FROM INFORMATION_SCHEMA.REFERENTIAL_CONSTRAINTS A ' +
        'INNER JOIN INFORMATION_SCHEMA.TABLE_CONSTRAINTS B ON';
      if FConnMeta.Kind = mkMySQL then
        Result := Result + ' B.TABLE_NAME = A.TABLE_NAME AND';
      Result := Result + ' B.CONSTRAINT_NAME = A.CONSTRAINT_NAME';
      if npCatalog in eParts then
        Result := Result + ' AND B.CONSTRAINT_CATALOG = A.CONSTRAINT_CATALOG';
      if npSchema in eParts then
        Result := Result + ' AND B.CONSTRAINT_SCHEMA = A.CONSTRAINT_SCHEMA';
      Result := Result + ' INNER JOIN INFORMATION_SCHEMA.TABLE_CONSTRAINTS C ON';
      if FConnMeta.Kind = mkMySQL then
        Result := Result + ' C.TABLE_NAME = A.REFERENCED_TABLE_NAME AND';
      Result := Result + ' C.CONSTRAINT_NAME = A.UNIQUE_CONSTRAINT_NAME';
      if npCatalog in eParts then
        Result := Result + ' AND C.CONSTRAINT_CATALOG = A.UNIQUE_CONSTRAINT_CATALOG';
      if npSchema in eParts then
        Result := Result + ' AND C.CONSTRAINT_SCHEMA = A.UNIQUE_CONSTRAINT_SCHEMA';
      if (ACatalog <> '') and (npCatalog in eParts) then
        AddWhere(GetFK_TABLE_CATALOG + ' = ', 'CAT');
      if (ASchema <> '') and (npSchema in eParts) then
        AddWhere(GetFK_TABLE_SCHEMA + ' = ', 'SCH');
      AddWhere('B.TABLE_NAME = ', 'OBJ');
      if AWildcard <> '' then
        AddWhere('A.CONSTRAINT_NAME LIKE ', 'WIL');
      Result := Result + ' ORDER BY 5';
    end;
  mkForeignKeyFields:
    begin
      Result := 'SELECT 0 AS RECNO, ' +
        GetFK_TABLE_CATALOG + ' AS CATALOG_NAME, ' +
        GetFK_TABLE_SCHEMA + ' AS SCHEMA_NAME, ' +
        'B.TABLE_NAME, ' +
        'A.CONSTRAINT_NAME AS FKEY_NAME, ' +
        'D.COLUMN_NAME, ' +
        GetFK_REFERENCED_COLUMN + ' AS PKEY_COLUMN_NAME, ' +
        'D.ORDINAL_POSITION AS COLUMN_POSITION ' +
        'FROM INFORMATION_SCHEMA.REFERENTIAL_CONSTRAINTS A ' +
        'INNER JOIN INFORMATION_SCHEMA.TABLE_CONSTRAINTS B ON';
      if FConnMeta.Kind = mkMySQL then
        Result := Result + ' B.TABLE_NAME = A.TABLE_NAME AND';
      Result := Result + ' B.CONSTRAINT_NAME = A.CONSTRAINT_NAME';
      if npCatalog in eParts then
        Result := Result + ' AND B.CONSTRAINT_CATALOG = A.CONSTRAINT_CATALOG';
      if npSchema in eParts then
        Result := Result + ' AND B.CONSTRAINT_SCHEMA = A.CONSTRAINT_SCHEMA';
      Result := Result + ' INNER JOIN INFORMATION_SCHEMA.TABLE_CONSTRAINTS C ON';
      if FConnMeta.Kind = mkMySQL then
        Result := Result + ' C.TABLE_NAME = A.REFERENCED_TABLE_NAME AND';
      Result := Result + ' C.CONSTRAINT_NAME = A.UNIQUE_CONSTRAINT_NAME';
      if npCatalog in eParts then
        Result := Result + ' AND C.CONSTRAINT_CATALOG = A.UNIQUE_CONSTRAINT_CATALOG';
      if npSchema in eParts then
        Result := Result + ' AND C.CONSTRAINT_SCHEMA = A.UNIQUE_CONSTRAINT_SCHEMA';
      Result := Result + ' INNER JOIN INFORMATION_SCHEMA.KEY_COLUMN_USAGE D ON';
      if FConnMeta.Kind = mkMySQL then
        Result := Result + ' D.TABLE_NAME = A.TABLE_NAME AND';
      Result := Result + ' D.CONSTRAINT_NAME = A.CONSTRAINT_NAME';
      if npCatalog in eParts then
        Result := Result + ' AND D.CONSTRAINT_CATALOG = A.CONSTRAINT_CATALOG';
      if npSchema in eParts then
        Result := Result + ' AND D.CONSTRAINT_SCHEMA = A.CONSTRAINT_SCHEMA';
      if FConnMeta.Kind <> mkMySQL then begin
        Result := Result + ' INNER JOIN INFORMATION_SCHEMA.KEY_COLUMN_USAGE E ON';
        Result := Result + ' E.CONSTRAINT_NAME = A.UNIQUE_CONSTRAINT_NAME';
        if npCatalog in eParts then
          Result := Result + ' AND E.CONSTRAINT_CATALOG = A.UNIQUE_CONSTRAINT_CATALOG';
        if npSchema in eParts then
          Result := Result + ' AND E.CONSTRAINT_SCHEMA = A.UNIQUE_CONSTRAINT_SCHEMA';
      end;
      if (ACatalog <> '') and (npCatalog in eParts) then
        AddWhere(GetFK_TABLE_CATALOG + ' = ', 'CAT');
      if (ASchema <> '') and (npSchema in eParts) then
        AddWhere(GetFK_TABLE_SCHEMA + ' = ', 'SCH');
      AddWhere('B.TABLE_NAME = ', 'BAS');
      AddWhere('A.CONSTRAINT_NAME = ', 'OBJ');
      if AWildcard <> '' then
        AddWhere('D.COLUMN_NAME LIKE ', 'WIL');
      Result := Result + ' ORDER BY 8';
    end;
  end;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.HasKW(const ASQL, AKW: String): Boolean;
var
  i: Integer;
begin
  i := Pos(AKW, ASQL);
  Result := (i > 0) and
    ((i = 1) or FDInSet(ASQL[i - 1], [' ', #13, #10, #9, '(', ')'])) and
    ((i + Length(AKW) - 1 = Length(ASQL)) or FDInSet(ASQL[i + Length(AKW)], [' ', #13, #10, #9, '(', ')']));
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetLimitSelect(const ASQL: String;
  ASkip, ARows: Integer): String;
var
  s: String;
begin
  // overridden by descendant classes
  if (ASkip + ARows <> MAXINT) and
     (CompareText(Copy(ASQL, 1, 6), 'SELECT') = 0) and
     (CompareText(Copy(ASQL, 1, 10), 'SELECT TOP') <> 0) then begin
    Result := 'SELECT TOP ';
    if GetSQLOrderByPos > 0 then begin
      s := UpperCase(ASQL);
      if HasKW(s, 'UNION') or HasKW(s, 'MINUS') or HasKW(s, 'INTERSECT') or
         HasKW(s, 'EXCEPT') then
        Result := Result + IntToStr(ASkip + ARows) + ' * FROM (' + BRK +
          Copy(ASQL, 1, GetSQLOrderByPos - 1) + BRK + ') A' + BRK +
          Copy(ASQL, GetSQLOrderByPos, MAXINT)
      else if CompareText(Copy(ASQL, 1, 15), 'SELECT DISTINCT') = 0 then
        Result := 'SELECT DISTINCT TOP ' + IntToStr(ASkip + ARows) +
          Copy(ASQL, 16, MAXINT)
      else
        Result := Result + IntToStr(ASkip + ARows) + Copy(ASQL, 7, MAXINT);
    end
    else if FConnMeta.Kind in [mkMSSQL, mkASA, mkADS] then begin
      // SQL Server, SQL Anywhere and Advantage does not support subqueries
      // with unaliased expressions in SELECT list. So, try to avoid
      // SELECT * FROM (<original query>)
      s := UpperCase(ASQL);
      if HasKW(s, 'UNION') or HasKW(s, 'MINUS') or HasKW(s, 'INTERSECT') or
         HasKW(s, 'EXCEPT') then
        Result := Result + IntToStr(ASkip + ARows) + ' * FROM (' + BRK +
          ASQL + BRK + ') A'
      else if CompareText(Copy(ASQL, 1, 15), 'SELECT DISTINCT') = 0 then
        Result := 'SELECT DISTINCT TOP ' + IntToStr(ASkip + ARows) +
          Copy(ASQL, 16, MAXINT)
      else
        Result := Result + IntToStr(ASkip + ARows) + Copy(ASQL, 7, MAXINT);
    end
    else
      Result := Result + IntToStr(ASkip + ARows) + ' * FROM (' + BRK +
        ASQL + BRK + ') A';
  end
  else
    Result := ASQL;
end;

{-------------------------------------------------------------------------------}
function TFDPhysCommandGenerator.GetCountSelect(const ASQL: String): String;
begin
  Result := 'SELECT COUNT(*) FROM (' + BRK + ASQL + BRK + ') A';
end;

end.