{*******************************************************}
{                                                       }
{               Delphi FireDAC Framework                }
{                 FireDAC ODBC metadata                 }
{                                                       }
{ Copyright(c) 2004-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}
{$I FireDAC.inc}

unit FireDAC.Phys.ODBCMeta;

interface

uses
  System.Classes,
  FireDAC.Stan.Intf,
  FireDAC.Phys.Intf, FireDAC.Phys, FireDAC.Phys.Meta, FireDAC.Phys.ODBCBase,
    FireDAC.Phys.ODBCCli;

type
  __TADPhysConnectionMetadata = class(TFDPhysConnectionMetadata)
  end;

  TFDPhysODBCMetadata = class(TFDPhysConnectionMetadata)
  private
    FNameMaxLength: Integer;
    FNameQuoteChar: Char;
    FNameQuoteChar1: Char;
    FNameQuoteChar2: Char;
    FTxNested: Boolean;
    FTxSupported: Boolean;
    FQuotedIdentifierCase: SQLUSmallint;
    FIdentifierCase: SQLUSmallint;
    FCatalogUsage: SQLUInteger;
    FSchemaUsage: SQLUInteger;
    FCancelSupported: Boolean;
    FCatalogSeparator: Char;
    FNullLocations: TFDPhysNullLocations;
    FParentMeta: __TADPhysConnectionMetadata;
  protected
    function GetAsyncAbortSupported: Boolean; override;
    function GetCommandSeparator: String; override;
    function GetDefValuesSupported: TFDPhysDefaultValues; override;
    function GetIdentityInsertSupported: Boolean; override;
    function GetInlineRefresh: Boolean; override;
    function GetKind: TFDRDBMSKind; override;
    function GetLockNoWait: Boolean; override;
    function GetNameCaseSensParts: TFDPhysNameParts; override;
    function GetNameDefLowCaseParts: TFDPhysNameParts; override;
    function GetNameParts: TFDPhysNameParts; override;
    function GetNameQuoteChar(AQuote: TFDPhysNameQuoteLevel; ASide: TFDPhysNameQuoteSide): Char; override;
    function GetCatalogSeparator: Char; override;
    function GetNameQuotedCaseSensParts: TFDPhysNameParts; override;
    function GetParamNameMaxLength: Integer; override;
    function GetSelectWithoutFrom: Boolean; override;
    function GetTruncateSupported: Boolean; override;
    function GetTxAutoCommit: Boolean; override;
    function GetTxNested: Boolean; override;
    function GetTxSavepoints: Boolean; override;
    function GetTxSupported: Boolean; override;
    function GetLimitOptions: TFDPhysLimitOptions; override;
    function GetNullLocation: TFDPhysNullLocations; override;
    function GetColumnOriginProvided: Boolean; override;
    function InternalDecodeObjName(const AName: String;
      out AParsedName: TFDPhysParsedName; const ACommand: IFDPhysCommand;
      ARaise: Boolean): Boolean; override;
    function InternalEncodeObjName(const AParsedName: TFDPhysParsedName;
      const ACommand: IFDPhysCommand): String; override;
    function InternalGetSQLCommandKind(const ATokens: TStrings): TFDPhysCommandKind; override;
    function InternalEscapeDate(const AStr: String): String; override;
    function InternalEscapeDateTime(const AStr: String): String; override;
    function InternalEscapeTime(const AStr: String): String; override;
    function InternalEscapeFunction(const ASeq: TFDPhysEscapeData): String; override;
    function TranslateEscapeSequence(var ASeq: TFDPhysEscapeData): String; override;
  public
    constructor Create(const AConnection: TFDPhysConnection;
      AServerVersion, AClientVersion: TFDVersion; AParentMeta: TFDPhysConnectionMetadata);
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,
  FireDAC.Stan.Consts, FireDAC.Stan.Util,
  FireDAC.Phys.ODBCWrapper;

{------------------------------------------------------------------------------}
{ TFDPhysODBCMetadata                                                          }
{------------------------------------------------------------------------------}
constructor TFDPhysODBCMetadata.Create(const AConnection: TFDPhysConnection;
  AServerVersion, AClientVersion: TFDVersion; AParentMeta: TFDPhysConnectionMetadata);
var
  C: String;
  oConn: TODBCConnection;
begin
  inherited Create(AConnection, AServerVersion, AClientVersion, False);
  FParentMeta := __TADPhysConnectionMetadata(AParentMeta);
  oConn := TODBCConnection(AConnection.CliObj);
  FTxSupported := oConn.TXN_CAPABLE <> SQL_TC_NONE;
  FTxNested := oConn.MULTIPLE_ACTIVE_TXN = 'Y';
  FNameMaxLength := oConn.MAX_COL_NAME_LEN;
  C := oConn.IDENTIFIER_QUOTE_CHAR;
  FNameQuoteChar := #0;
  FNameQuoteChar1 := #0;
  FNameQuoteChar2 := #0;
  if Length(C) = 2 then begin
    FNameQuoteChar1 := C[1];
    FNameQuoteChar2 := C[2];
  end
  else if Length(C) = 1 then
    FNameQuoteChar := C[1];
  C := oConn.CATALOG_NAME_SEPARATOR;
  if Length(C) = 1 then
    FCatalogSeparator := C[1]
  else
    FCatalogSeparator := #0;
  if oConn.DriverKind = dkElevateDB then begin
    FQuotedIdentifierCase := SQL_IC_SENSITIVE;
    FIdentifierCase := SQL_IC_SENSITIVE;
    FCatalogUsage := oConn.CATALOG_USAGE;
    FSchemaUsage := 0;
  end
  else begin
    FQuotedIdentifierCase := oConn.QUOTED_IDENTIFIER_CASE;
    FIdentifierCase := oConn.IDENTIFIER_CASE;
    FCatalogUsage := oConn.CATALOG_USAGE;
    FSchemaUsage := oConn.SCHEMA_USAGE;
  end;
  FCancelSupported := oConn.GetFunctions(SQL_API_SQLCANCEL) = SQL_TRUE;
  FKeywords.CommaText := oConn.KEYWORDS;
  case oConn.NULL_COLLATION of
  SQL_NC_START: FNullLocations := [nlAscFirst, nlDescFirst];
  SQL_NC_END:   FNullLocations := [nlAscLast, nlDescLast];
  SQL_NC_HIGH:  FNullLocations := [nlAscLast, nlDescFirst];
  SQL_NC_LOW:   FNullLocations := [nlAscFirst, nlDescLast];
  else          FNullLocations := inherited GetNullLocation;
  end;
  ConfigNameParts;
  ConfigQuoteChars;
end;

{------------------------------------------------------------------------------}
destructor TFDPhysODBCMetadata.Destroy;
begin
  if FParentMeta <> nil then
    FDFreeAndNil(FParentMeta);
  inherited Destroy;
end;

{------------------------------------------------------------------------------}
function TFDPhysODBCMetadata.GetAsyncAbortSupported: Boolean;
begin
  Result := FCancelSupported;
end;

{------------------------------------------------------------------------------}
function TFDPhysODBCMetadata.GetCommandSeparator: String;
begin
  if FParentMeta <> nil then
    Result := FParentMeta.GetCommandSeparator
  else
    Result := inherited GetCommandSeparator;
end;

{------------------------------------------------------------------------------}
function TFDPhysODBCMetadata.GetDefValuesSupported: TFDPhysDefaultValues;
begin
  if FParentMeta <> nil then
    Result := FParentMeta.GetDefValuesSupported
  else
    Result := inherited GetDefValuesSupported;
end;

{------------------------------------------------------------------------------}
function TFDPhysODBCMetadata.GetIdentityInsertSupported: Boolean;
begin
  if FParentMeta <> nil then
    Result := FParentMeta.GetIdentityInsertSupported
  else
    Result := inherited GetIdentityInsertSupported;
end;

{------------------------------------------------------------------------------}
function TFDPhysODBCMetadata.GetInlineRefresh: Boolean;
begin
  if FParentMeta <> nil then
    Result := FParentMeta.GetInlineRefresh
  else
    Result := inherited GetInlineRefresh;
end;

{------------------------------------------------------------------------------}
function TFDPhysODBCMetadata.GetKind: TFDRDBMSKind;
begin
  if FParentMeta <> nil then
    Result := FParentMeta.GetKind
  else
    Result := inherited GetKind;
end;

{------------------------------------------------------------------------------}
function TFDPhysODBCMetadata.GetLockNoWait: Boolean;
begin
  if FParentMeta <> nil then
    Result := FParentMeta.GetLockNoWait
  else
    Result := inherited GetLockNoWait;
end;

{------------------------------------------------------------------------------}
function TFDPhysODBCMetadata.GetNameCaseSensParts: TFDPhysNameParts;
begin
  if FParentMeta <> nil then
    Result := FParentMeta.GetNameCaseSensParts
  else if FIdentifierCase = SQL_IC_SENSITIVE then
    Result := [npCatalog, npSchema, npDBLink, npBaseObject, npObject]
  else
    Result := [];
end;

{------------------------------------------------------------------------------}
function TFDPhysODBCMetadata.GetNameDefLowCaseParts: TFDPhysNameParts;
begin
  if FParentMeta <> nil then
    Result := FParentMeta.GetNameDefLowCaseParts
  else if FIdentifierCase = SQL_IC_LOWER then
    Result := [npCatalog, npSchema, npDBLink, npBaseObject, npObject]
  else
    Result := [];
end;

{------------------------------------------------------------------------------}
function TFDPhysODBCMetadata.GetNameParts: TFDPhysNameParts;
begin
  if FParentMeta <> nil then
    Result := FParentMeta.GetNameParts
  else begin
    Result := inherited GetNameParts;
    if FCatalogUsage and SQL_CU_DML_STATEMENTS = SQL_CU_DML_STATEMENTS then
      Include(Result, npCatalog);
    if FSchemaUsage and SQL_SU_DML_STATEMENTS = SQL_SU_DML_STATEMENTS then
      Include(Result, npSchema);
  end;
end;

{------------------------------------------------------------------------------}
function TFDPhysODBCMetadata.GetNameQuoteChar(AQuote: TFDPhysNameQuoteLevel;
  ASide: TFDPhysNameQuoteSide): Char;
begin
  if FParentMeta <> nil then
    Result := FParentMeta.GetNameQuoteChar(AQuote, ASide)
  else begin
    Result := #0;
    if AQuote = ncDefault then
      if FNameQuoteChar <> #0 then
        Result := FNameQuoteChar
      else if ASide = nsLeft then
        Result := FNameQuoteChar1
      else
        Result := FNameQuoteChar2;
  end;
end;

{------------------------------------------------------------------------------}
function TFDPhysODBCMetadata.GetCatalogSeparator: Char;
begin
  if FParentMeta <> nil then
    Result := FParentMeta.GetCatalogSeparator
  else
    Result := FCatalogSeparator;
end;

{------------------------------------------------------------------------------}
function TFDPhysODBCMetadata.GetNameQuotedCaseSensParts: TFDPhysNameParts;
begin
  if FParentMeta <> nil then
    Result := FParentMeta.GetNameQuotedCaseSensParts
  else if FQuotedIdentifierCase = SQL_IC_SENSITIVE then
    Result := [npCatalog, npSchema, npDBLink, npBaseObject, npObject]
  else
    Result := [];
end;

{------------------------------------------------------------------------------}
function TFDPhysODBCMetadata.GetParamNameMaxLength: Integer;
begin
  if FParentMeta <> nil then
    Result := FParentMeta.GetParamNameMaxLength
  else
    Result := FNameMaxLength;
end;

{------------------------------------------------------------------------------}
function TFDPhysODBCMetadata.GetSelectWithoutFrom: Boolean;
begin
  if FParentMeta <> nil then
    Result := FParentMeta.GetSelectWithoutFrom
  else
    Result := inherited GetSelectWithoutFrom;
end;

{------------------------------------------------------------------------------}
function TFDPhysODBCMetadata.GetTruncateSupported: Boolean;
begin
  if FParentMeta <> nil then
    Result := FParentMeta.GetTruncateSupported
  else
    Result := inherited GetTruncateSupported;
end;

{------------------------------------------------------------------------------}
function TFDPhysODBCMetadata.GetTxAutoCommit: Boolean;
begin
  if FParentMeta <> nil then
    Result := FParentMeta.GetTxAutoCommit
  else
    Result := inherited GetTxAutoCommit;
end;

{------------------------------------------------------------------------------}
function TFDPhysODBCMetadata.GetTxNested: Boolean;
begin
  if FParentMeta <> nil then
    Result := FParentMeta.GetTxNested
  else
    Result := FTxNested;
end;

{------------------------------------------------------------------------------}
function TFDPhysODBCMetadata.GetTxSavepoints: Boolean;
begin
  if FParentMeta <> nil then
    Result := FParentMeta.GetTxSavepoints
  else
    Result := inherited GetTxSavepoints;
end;

{------------------------------------------------------------------------------}
function TFDPhysODBCMetadata.GetTxSupported: Boolean;
begin
  if FParentMeta <> nil then
    Result := FParentMeta.GetTxSupported
  else
    Result := FTxSupported;
end;

{------------------------------------------------------------------------------}
function TFDPhysODBCMetadata.GetLimitOptions: TFDPhysLimitOptions;
begin
  if FParentMeta <> nil then
    Result := FParentMeta.GetLimitOptions
  else
    Result := inherited GetLimitOptions;
end;

{-------------------------------------------------------------------------------}
function TFDPhysODBCMetadata.GetNullLocation: TFDPhysNullLocations;
begin
  if FParentMeta <> nil then
    Result := FParentMeta.GetNullLocation
  else
    Result := FNullLocations;
end;

{------------------------------------------------------------------------------}
function TFDPhysODBCMetadata.GetColumnOriginProvided: Boolean;
begin
  if FParentMeta <> nil then
    Result := FParentMeta.GetColumnOriginProvided
  else
    // This is pessimistic, but Informix does not provide origin table name.
    // If here will be True, then all Informix result sets will be ReadOnly.
    Result := False;
end;

{------------------------------------------------------------------------------}
function TFDPhysODBCMetadata.InternalDecodeObjName(const AName: String;
  out AParsedName: TFDPhysParsedName; const ACommand: IFDPhysCommand;
  ARaise: Boolean): Boolean;
begin
  if FParentMeta <> nil then
    Result := FParentMeta.InternalDecodeObjName(AName, AParsedName, ACommand, ARaise)
  else
    Result := inherited InternalDecodeObjName(AName, AParsedName, ACommand, ARaise);
end;

{------------------------------------------------------------------------------}
function TFDPhysODBCMetadata.InternalEncodeObjName(const AParsedName: TFDPhysParsedName;
  const ACommand: IFDPhysCommand): String;
begin
  if FParentMeta <> nil then
    Result := FParentMeta.InternalEncodeObjName(AParsedName, ACommand)
  else
    Result := inherited InternalEncodeObjName(AParsedName, ACommand);
end;

{------------------------------------------------------------------------------}
function TFDPhysODBCMetadata.InternalGetSQLCommandKind(const ATokens: TStrings):
  TFDPhysCommandKind;
begin
  if FParentMeta <> nil then
    Result := FParentMeta.GetSQLCommandKind(ATokens)
  else
    Result := inherited InternalGetSQLCommandKind(ATokens);
end;

{-------------------------------------------------------------------------------}
function TFDPhysODBCMetadata.InternalEscapeDate(const AStr: String): String;
begin
  Result := '{D ' + inherited InternalEscapeDate(AStr) + '}';
end;

{-------------------------------------------------------------------------------}
function TFDPhysODBCMetadata.InternalEscapeDateTime(const AStr: String): String;
begin
  Result := '{TS ' + inherited InternalEscapeDateTime(AStr) + '}';
end;

{-------------------------------------------------------------------------------}
function TFDPhysODBCMetadata.InternalEscapeTime(const AStr: String): String;
begin
  Result := '{T ' + inherited InternalEscapeTime(AStr) + '}';
end;

{------------------------------------------------------------------------------}
function TFDPhysODBCMetadata.InternalEscapeFunction(const ASeq: TFDPhysEscapeData): String;
begin
  if ASeq.FFunc = efCONVERT then
    ASeq.FArgs[1] := 'SQL_' + Trim(ASeq.FArgs[1]);
  Result := '{FN ' + inherited InternalEscapeFunction(ASeq) + '}';
end;

{------------------------------------------------------------------------------}
function TFDPhysODBCMetadata.TranslateEscapeSequence(var ASeq: TFDPhysEscapeData): String;
begin
  if FParentMeta <> nil then
    Result := FParentMeta.TranslateEscapeSequence(ASeq)
  else
    Result := inherited TranslateEscapeSequence(ASeq);
end;

end.

