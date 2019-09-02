{*******************************************************}
{                                                       }
{               Delphi FireDAC Framework                }
{                FireDAC MSSQL metadata                 }
{                                                       }
{ Copyright(c) 2004-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}
{$I FireDAC.inc}

unit FireDAC.Phys.MSSQLMeta;

interface

uses
  System.Classes,
  FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Phys.Intf, FireDAC.Phys, FireDAC.Phys.Meta, FireDAC.Phys.SQLGenerator;

type
  TFDPhysMSSQLMetadata = class (TFDPhysConnectionMetadata)
  private
    FSchemaCaseSensitive,
    FCatalogCaseSensitive: Boolean;
    FCaseSensParts: TFDPhysNameParts;
    FNameDoubleQuote: Boolean;
    FMSDriver: Boolean;
    FColumnOriginProvided: Boolean;
  protected
    function GetKind: TFDRDBMSKind; override;
    function GetTxSavepoints: Boolean; override;
    function GetEventSupported: Boolean; override;
    function GetEventKinds: String; override;
    function GetParamNameMaxLength: Integer; override;
    function GetNameParts: TFDPhysNameParts; override;
    function GetNameQuotedCaseSensParts: TFDPhysNameParts; override;
    function GetNameCaseSensParts: TFDPhysNameParts; override;
    function GetNameDefLowCaseParts: TFDPhysNameParts; override;
    function GetNameQuoteChar(AQuote: TFDPhysNameQuoteLevel; ASide: TFDPhysNameQuoteSide): Char; override;
    function GetInlineRefresh: Boolean; override;
    function GetIdentityInsertSupported: Boolean; override;
    function GetDefValuesSupported: TFDPhysDefaultValues; override;
    function GetLockNoWait: Boolean; override;
    function GetAsyncAbortSupported: Boolean; override;
    function GetCommandSeparator: String; override;
    function GetLineSeparator: TFDTextEndOfLine; override;
    function GetArrayExecMode: TFDPhysArrayExecMode; override;
    function GetColumnOriginProvided: Boolean; override;
    function InternalEscapeBoolean(const AStr: String): String; override;
    function InternalEscapeDate(const AStr: String): String; override;
    function InternalEscapeDateTime(const AStr: String): String; override;
    function InternalEscapeFloat(const AStr: String): String; override;
    function InternalEscapeFunction(const ASeq: TFDPhysEscapeData): String; override;
    function InternalEscapeTime(const AStr: String): String; override;
    function InternalGetSQLCommandKind(const ATokens: TStrings): TFDPhysCommandKind; override;
  public
    constructor Create(const AConnectionObj: TFDPhysConnection;
      ACatalogCaseSensitive, ASchemaCaseSensitive, ASchemaCaseInsSearch,
      ANameDoubleQuote, AMSDriver: Boolean; AServerVersion, AClientVersion: TFDVersion;
      const ACSVKeywords: String; AColumnOriginProvided: Boolean);
  end;

  TFDPhysMSSQLCommandGenerator = class(TFDPhysCommandGenerator)
  protected
    function GetInlineRefresh(const AStmt: String;
      ARequest: TFDUpdateRequest): String; override;
    function GetIdentityInsert(const ATable, AStmt: String;
      ARequest: TFDUpdateRequest): String; override;
    function GetIdentity(ASessionScope: Boolean): String; override;
    function GetPessimisticLock: String; override;
    function GetSavepoint(const AName: String): String; override;
    function GetRollbackToSavepoint(const AName: String): String; override;
    function GetCommitSavepoint(const AName: String): String; override;
    function GetCall(const AName: String): String; override;
    function GetPing: String; override;
    function GetLimitSelect(const ASQL: String; ASkip, ARows: Integer): String; override;
  end;

implementation

uses
  System.SysUtils,
  FireDAC.Stan.Consts, FireDAC.Stan.Util;

{-------------------------------------------------------------------------------}
{ TFDPhysOraMetadata                                                            }
{-------------------------------------------------------------------------------}
constructor TFDPhysMSSQLMetadata.Create(const AConnectionObj: TFDPhysConnection;
  ACatalogCaseSensitive, ASchemaCaseSensitive, ASchemaCaseInsSearch,
  ANameDoubleQuote, AMSDriver: Boolean; AServerVersion, AClientVersion: TFDVersion;
  const ACSVKeywords: String; AColumnOriginProvided: Boolean);
begin
  inherited Create(AConnectionObj, AServerVersion, AClientVersion, False);
  FCatalogCaseSensitive := ACatalogCaseSensitive;
  FSchemaCaseSensitive := ASchemaCaseSensitive;
  FNameDoubleQuote := ANameDoubleQuote;
  FMSDriver := AMSDriver;
  FCaseSensParts := [];
  if FCatalogCaseSensitive then
    FCaseSensParts := FCaseSensParts + [npCatalog];
  if FSchemaCaseSensitive then
    FCaseSensParts := FCaseSensParts + [npSchema, npBaseObject, npObject];
  if ASchemaCaseInsSearch then
    FCISearch := True;
  if ACSVKeywords <> '' then
    FKeywords.CommaText := ACSVKeywords;
  ConfigQuoteChars;
  FColumnOriginProvided := AColumnOriginProvided;
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLMetadata.GetKind: TFDRDBMSKind;
begin
  Result := mkMSSQL;
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLMetadata.GetNameQuotedCaseSensParts: TFDPhysNameParts;
begin
  Result := FCaseSensParts;
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLMetadata.GetNameCaseSensParts: TFDPhysNameParts;
begin
  Result := FCaseSensParts;
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLMetadata.GetNameDefLowCaseParts: TFDPhysNameParts;
begin
  Result := [npSchema, npBaseObject, npObject];
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLMetadata.GetParamNameMaxLength: Integer;
begin
  Result := 128;
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLMetadata.GetNameParts: TFDPhysNameParts;
begin
  Result := [npCatalog, npSchema, npBaseObject, npObject];
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLMetadata.GetNameQuoteChar(AQuote: TFDPhysNameQuoteLevel;
  ASide: TFDPhysNameQuoteSide): Char;
begin
  Result := #0;
  case AQuote of
  ncDefault:
    if ASide = nsLeft then
      Result := '['
    else
      Result := ']';
  ncSecond:
    if FNameDoubleQuote then
      Result := '"';
  end;
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLMetadata.GetTxSavepoints: Boolean;
begin
  Result := True;
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLMetadata.GetEventSupported: Boolean;
begin
  Result := True;
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLMetadata.GetEventKinds: String;
begin
  Result := S_FD_EventKind_MSSQL_Events;
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLMetadata.GetInlineRefresh: Boolean;
begin
  Result := True;
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLMetadata.GetIdentityInsertSupported: Boolean;
begin
  Result := True;
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLMetadata.GetDefValuesSupported: TFDPhysDefaultValues;
begin
  Result := dvDefVals;
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLMetadata.GetLockNoWait: Boolean;
begin
  Result := True;
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLMetadata.GetAsyncAbortSupported: Boolean;
begin
  Result := True;
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLMetadata.GetCommandSeparator: String;
begin
  Result := 'GO';
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLMetadata.GetLineSeparator: TFDTextEndOfLine;
begin
  Result := elWindows;
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLMetadata.GetArrayExecMode: TFDPhysArrayExecMode;
begin
  if FMSDriver then
    Result := aeCollectAllErrors
  else
    // FreeTDS has broken Array DML implementation
    Result := aeUpToFirstError;
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLMetadata.GetColumnOriginProvided: Boolean;
begin
  Result := FMSDriver and FColumnOriginProvided;
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLMetadata.InternalEscapeBoolean(
  const AStr: String): String;
begin
  if CompareText(AStr, S_FD_True) = 0 then
    Result := '1'
  else
    Result := '0';
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLMetadata.InternalEscapeDate(const AStr: String): String;
begin
  Result := 'CONVERT(DATETIME, ' + AnsiQuotedStr(AStr, '''') + ', 20)';
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLMetadata.InternalEscapeDateTime(const AStr: String): String;
begin
  Result := 'CONVERT(DATETIME, ' + AnsiQuotedStr(AStr, '''') + ', 20)';
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLMetadata.InternalEscapeTime(const AStr: String): String;
begin
  Result := 'CONVERT(DATETIME, ' + AnsiQuotedStr(AStr, '''') + ', 114)';
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLMetadata.InternalEscapeFloat(const AStr: String): String;
begin
  Result := AStr;
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLMetadata.InternalEscapeFunction(const ASeq: TFDPhysEscapeData): String;
var
  sName: String;
  A1, A2, A3: String;
  rSeq: TFDPhysEscapeData;
  i: Integer;

  function AddArgs: string;
  begin
    Result := '(' + AddEscapeSequenceArgs(ASeq) + ')';
  end;

begin
  sName := ASeq.FName;
  if Length(ASeq.FArgs) >= 1 then begin
    A1 := ASeq.FArgs[0];
    if Length(ASeq.FArgs) >= 2 then begin
      A2 := ASeq.FArgs[1];
      if Length(ASeq.FArgs) >= 3 then
        A3 := ASeq.FArgs[2];
    end;
  end;
  case ASeq.FFunc of
  // the same
  // char
  efASCII,
  efCHAR,
  efLEFT,
  efLTRIM,
  efREPLACE,
  efRIGHT,
  efRTRIM,
  efSPACE,
  efSUBSTRING,
  // numeric
  efACOS,
  efASIN,
  efATAN,
  efABS,
  efCEILING,
  efCOS,
  efCOT,
  efDEGREES,
  efEXP,
  efFLOOR,
  efLOG,
  efLOG10,
  efPOWER,
  efPI,
  efSIGN,
  efSIN,
  efSQRT,
  efTAN:         Result := sName + AddArgs;
  // character
  efBIT_LENGTH:  Result := '(LEN(' + A1 + ') * 8)';
  efCHAR_LENGTH: Result := 'LEN' + AddArgs;
  efCONCAT:      Result := '(' + A1 + ' + ' + A2 + ')';
  efINSERT:      Result := 'STUFF' + AddArgs;
  efLCASE:       Result := 'LOWER' + AddArgs;
  efLENGTH:      Result := 'LEN(RTRIM(' + A1 + '))';
  efLOCATE:
    begin
      Result := 'CHARINDEX(' + A1 + ', ' + A2;
      if Length(ASeq.FArgs) = 3 then
        Result := Result + ', ' + A3;
      Result := Result + ')'
    end;
  efOCTET_LENGTH:Result := 'LEN' + AddArgs;
  efPOSITION:    Result := 'CHARINDEX' + AddArgs;
  efREPEAT:      Result := 'REPLICATE' + AddArgs;
  efUCASE:       Result := 'UPPER' + AddArgs;
  // numeric
  efRANDOM:      Result := 'RAND' + AddArgs;
  efTRUNCATE:    Result := 'CAST(' + A1 + ' AS BIGINT)';
  efATAN2:       Result := 'ATN2' + AddArgs;
  efMOD:         Result := '((' + A1 + ') % (' + A2 + '))';
  efROUND:
    if A2 = '' then
      Result := sName + '(' + A1 + ', 0)'
    else
      Result := sName + AddArgs;
  efRADIANS:
    Result := sName + '(' + A1 + ' + 0.0)';
  // date and time
  efCURDATE,
  efCURTIME,
  efNOW:         Result := 'GETDATE()';
  efDAYNAME:     Result := 'DATENAME(WEEKDAY, ' + A1 + ')';
  efDAYOFMONTH:  Result := 'DATEPART(DAY, ' + A1 + ')';
  efDAYOFWEEK:   Result := '((DATEPART(WEEKDAY, ' + A1 + ') + @@DATEFIRST - 1) % 7 + 1)';
  efDAYOFYEAR:   Result := 'DATEPART(DAYOFYEAR, ' + A1 + ')';
  efEXTRACT:
    begin
      rSeq.FKind := eskFunction;
      A1 := UpperCase(FDUnquote(Trim(A1), ''''));
      if A1 = 'DAY' then
        A1 := 'DAYOFMONTH';
      rSeq.FName := A1;
      SetLength(rSeq.FArgs, 1);
      rSeq.FArgs[0] := ASeq.FArgs[1];
      EscapeFuncToID(rSeq);
      Result := InternalEscapeFunction(rSeq);
    end;
  efHOUR:        Result := 'DATEPART(HOUR, ' + A1 + ')';
  efMINUTE:      Result := 'DATEPART(MINUTE, ' + A1 + ')';
  efMONTH:       Result := 'DATEPART(MONTH, ' + A1 + ')';
  efMONTHNAME:   Result := 'DATENAME(MONTH, ' + A1 + ')';
  efQUARTER:     Result := 'DATEPART(QUARTER, ' + A1 + ')';
  efSECOND:      Result := 'DATEPART(SECOND, ' + A1 + ')';
  efTIMESTAMPADD:
    begin
      A1 := UpperCase(FDUnquote(Trim(A1), ''''));
      if A1 = 'FRAC_SECOND' then begin
        A1 := 'MILLISECOND';
        A2 := '(' + A2 + ' / 1000)';
      end;
      ASeq.FArgs[0] := A1;
      ASeq.FArgs[1] := A2;
      Result := 'DATEADD' + AddArgs;
    end;
  efTIMESTAMPDIFF:
    begin
      A1 := UpperCase(FDUnquote(Trim(A1), ''''));
      if A1 = 'FRAC_SECOND' then
        A1 := 'MILLISECOND';
      ASeq.FArgs[0] := A1;
      Result := 'DATEDIFF' + AddArgs;
      if A1 = 'MILLISECOND' then
        Result := '(' + Result + ' * 1000.0)';
    end;
  efWEEK:        Result := 'DATEPART(WEEK, ' + A1 + ')';
  efYEAR:        Result := 'DATEPART(YEAR, ' + A1 + ')';
  // system
  efCATALOG:     Result := 'DB_NAME()';
  efSCHEMA:
    if GetServerVersion >= svMSSQL2005 then
      Result := 'SCHEMA_NAME()'
    else
      Result := 'CURRENT_USER()';
  efIFNULL:      Result := 'CASE WHEN ' + A1 + ' IS NULL THEN ' + A2 + ' ELSE ' + A1 + ' END';
  efIF:          Result := 'CASE WHEN ' + A1 + ' THEN ' + A2 + ' ELSE ' + A3 + ' END';
  efDECODE:
    begin
      Result := 'CASE ' + ASeq.FArgs[0];
      i := 1;
      while i < Length(ASeq.FArgs) - 1 do begin
        Result := Result + ' WHEN ' + ASeq.FArgs[i] + ' THEN ' + ASeq.FArgs[i + 1];
        Inc(i, 2);
      end;
      if i = Length(ASeq.FArgs) - 1 then
        Result := Result + ' ELSE ' + ASeq.FArgs[i];
      Result := Result + ' END';
    end;
  // convert
  efCONVERT:
    Result := 'CONVERT(' + A2 + ', ' + A1 + ')';
  else
    UnsupportedEscape(ASeq);
  end;
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLMetadata.InternalGetSQLCommandKind(
  const ATokens: TStrings): TFDPhysCommandKind;
var
  sToken: String;
begin
  sToken := ATokens[0];
  if (sToken = 'EXEC') or (sToken = 'EXECUTE') then
    Result := skExecute
  else if sToken = 'BEGIN' then
    if ATokens.Count = 1 then
      Result := skNotResolved
    else if (ATokens[1] = 'TRAN') or (ATokens[1] = 'TRANSACTION') then
      Result := skStartTransaction
    else
      Result := inherited InternalGetSQLCommandKind(ATokens)
  else if sToken = 'IF' then
    // IF @@TRANCOUNT > 0 COMMIT | ROLLBACK
    if ATokens.Count = 1 then
      Result := skNotResolved
    else if ATokens[1] = 'COMMIT' then
      Result := skCommit
    else if ATokens[1] = 'ROLLBACK' then
      Result := skRollback
    else
      Result := inherited InternalGetSQLCommandKind(ATokens)
  else if sToken = 'RECEIVE' then
    Result := skSelect
  else
    Result := inherited InternalGetSQLCommandKind(ATokens);
end;

{-------------------------------------------------------------------------------}
{ TFDPhysMSSQLCommandGenerator                                                  }
{-------------------------------------------------------------------------------}
function TFDPhysMSSQLCommandGenerator.GetInlineRefresh(const AStmt: String;
  ARequest: TFDUpdateRequest): String;
begin
  Result := GenerateSelect(False);
  if Result <> '' then
    Result := AStmt + ';' + BRK + Result
  else
    Result := AStmt;
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLCommandGenerator.GetIdentityInsert(
  const ATable, AStmt: String; ARequest: TFDUpdateRequest): String;
begin
  Result := 'SET IDENTITY_INSERT ' + ATable + ' ON;' + BRK + AStmt +
    ';' + BRK + 'SET IDENTITY_INSERT ' + ATable + ' OFF';
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLCommandGenerator.GetIdentity(ASessionScope: Boolean): String;
begin
  if not ASessionScope and
     ((FConnMeta.ServerVersion = 0) or (FConnMeta.ServerVersion >= svMSSQL2000)) then
    Result := 'SCOPE_IDENTITY()'
  else
    Result := '@@IDENTITY';
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLCommandGenerator.GetPessimisticLock: String;
var
  lNeedFrom: Boolean;
begin
  if not FOptions.UpdateOptions.LockWait then
    Result := 'SET LOCK_TIMEOUT 0;' + BRK;
  lNeedFrom := False;
  Result := Result + 'SELECT ' + GetSelectList(True, False, lNeedFrom) + BRK +
    'FROM ' + GetFrom + ' WITH(ROWLOCK,UPDLOCK)' + BRK +
    'WHERE ' + GetWhere(False, True, False);
  FCommandKind := skSelectForLock;
  ASSERT(lNeedFrom);
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLCommandGenerator.GetSavepoint(const AName: String): String;
begin
  Result := 'IF @@TRANCOUNT = 0 BEGIN SET IMPLICIT_TRANSACTIONS OFF BEGIN TRANSACTION END SAVE TRANSACTION ' + AName;
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLCommandGenerator.GetRollbackToSavepoint(const AName: String): String;
begin
  Result := 'ROLLBACK TRANSACTION ' + AName;
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLCommandGenerator.GetCommitSavepoint(const AName: String): String;
begin
  Result := '';
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLCommandGenerator.GetCall(const AName: String): String;
begin
  Result := 'EXECUTE ' + AName;
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLCommandGenerator.GetPing: String;
begin
  Result := 'SELECT 1';
end;

{-------------------------------------------------------------------------------}
function TFDPhysMSSQLCommandGenerator.GetLimitSelect(const ASQL: String;
  ASkip, ARows: Integer): String;
var
  s: string;
begin
  if (GetSQLOrderByPos > 0) and (FConnMeta.ServerVersion >= svMSSQL2012) then begin
    s := UpperCase(ASQL);
    if not (HasKW(s, 'UNION') or HasKW(s, 'INTERSECT') or HasKW(s, 'EXCEPT')) then
      if (ASkip > 0) and (ARows + ASkip <> MAXINT) then
        Result := ASQL + BRK + 'OFFSET ' + IntToStr(ASkip) + ' ROWS FETCH FIRST ' + IntToStr(ARows) + ' ROWS ONLY'
      else if ASkip > 0 then
        Result := ASQL + BRK + 'OFFSET ' + IntToStr(ASkip) + ' ROWS'
      else if ARows >= 0 then
        Result := ASQL + BRK + 'OFFSET 0 ROWS FETCH FIRST ' + IntToStr(ARows) + ' ROWS ONLY'
      else
        Result := ASQL
    else
      Result := inherited GetLimitSelect(ASQL, ASkip, ARows);
  end
  else
    Result := inherited GetLimitSelect(ASQL, ASkip, ARows);
end;

end.
