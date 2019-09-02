{*******************************************************}
{                                                       }
{               Delphi FireDAC Framework                }
{               FireDAC DataSnap metadata               }
{                                                       }
{ Copyright(c) 2004-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}
{$I FireDAC.inc}

unit FireDAC.Phys.DataSnapMeta;

interface

uses
  System.Classes,
  FireDAC.Stan.Intf, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.Phys, FireDAC.Phys.Meta;

type
  TFDPhysDataSnapMetadata = class (TFDPhysConnectionMetadata)
  protected
    // IFDPhysConnectionMetadata
    function GetKind: TFDRDBMSKind; override;
    function GetTxSupported: Boolean; override;
    function GetEventSupported: Boolean; override;
    function GetEventKinds: String; override;
    function GetParamNameMaxLength: Integer; override;
    function GetNameParts: TFDPhysNameParts; override;
    function GetNameQuotedCaseSensParts: TFDPhysNameParts; override;
    function GetNameQuoteChar(AQuote: TFDPhysNameQuoteLevel; ASide: TFDPhysNameQuoteSide): Char; override;
    function GetIdentitySupported: Boolean; override;
    function GetNamedParamMark: TFDPhysParamMark; override;
    function GetAsyncAbortSupported: Boolean; override;
    function GetLimitOptions: TFDPhysLimitOptions; override;
    function GetServerCursorSupported: Boolean; override;
    function GetColumnOriginProvided: Boolean; override;
    function GetTruncateSupported: Boolean; override;
    function GetDefValuesSupported: TFDPhysDefaultValues; override;
    // other
    function InternalGetSQLCommandKind(const ATokens: TStrings): TFDPhysCommandKind; override;
  public
    constructor Create(const AConnectionObj: TFDPhysConnection;
      AServerVersion, AClientVersion: TFDVersion; AIsUnicode: Boolean);
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  // Preventing from "Inline has not expanded"
  Winapi.Windows,
{$ENDIF}
  System.SysUtils,
  FireDAC.Stan.Error, FireDAC.Stan.Consts, FireDAC.Stan.Util;

{-------------------------------------------------------------------------------}
{ TFDPhysDataSnapMetadata                                                       }
{-------------------------------------------------------------------------------}
constructor TFDPhysDataSnapMetadata.Create(const AConnectionObj: TFDPhysConnection;
  AServerVersion, AClientVersion: TFDVersion; AIsUnicode: Boolean);
begin
  inherited Create(AConnectionObj, AServerVersion, AClientVersion, AIsUnicode);
  FKeywords.CommaText := '';
end;

{-------------------------------------------------------------------------------}
function TFDPhysDataSnapMetadata.InternalGetSQLCommandKind(
  const ATokens: TStrings): TFDPhysCommandKind;
var
  sToken: String;
begin
  sToken := ATokens[0];
  if sToken = 'CALL' then
    Result := skExecute
  else
    Result := inherited InternalGetSQLCommandKind(ATokens);
end;

{-------------------------------------------------------------------------------}
function TFDPhysDataSnapMetadata.GetIdentitySupported: Boolean;
begin
  Result := False;
end;

{-------------------------------------------------------------------------------}
function TFDPhysDataSnapMetadata.GetKind: TFDRDBMSKind;
begin
  Result := mkDataSnap;
end;

{-------------------------------------------------------------------------------}
function TFDPhysDataSnapMetadata.GetNameQuotedCaseSensParts: TFDPhysNameParts;
begin
  Result := [];
end;

{-------------------------------------------------------------------------------}
function TFDPhysDataSnapMetadata.GetParamNameMaxLength: Integer;
begin
  Result := 255;
end;

{-------------------------------------------------------------------------------}
function TFDPhysDataSnapMetadata.GetNameParts: TFDPhysNameParts;
begin
  Result := [npSchema, npBaseObject, npObject];
end;

{-------------------------------------------------------------------------------}
function TFDPhysDataSnapMetadata.GetNameQuoteChar(AQuote: TFDPhysNameQuoteLevel;
  ASide: TFDPhysNameQuoteSide): Char;
begin
  Result := #0;
end;

{-------------------------------------------------------------------------------}
function TFDPhysDataSnapMetadata.GetNamedParamMark: TFDPhysParamMark;
begin
  Result := prName;
end;

{-------------------------------------------------------------------------------}
function TFDPhysDataSnapMetadata.GetTxSupported: Boolean;
begin
  Result := False;
end;

{-------------------------------------------------------------------------------}
function TFDPhysDataSnapMetadata.GetEventSupported: Boolean;
begin
  Result := True;
end;

{-------------------------------------------------------------------------------}
function TFDPhysDataSnapMetadata.GetEventKinds: String;
begin
  Result := S_FD_EventKind_DataSnap_Events;
end;

{-------------------------------------------------------------------------------}
function TFDPhysDataSnapMetadata.GetTruncateSupported: Boolean;
begin
  Result := False;
end;

{-------------------------------------------------------------------------------}
function TFDPhysDataSnapMetadata.GetDefValuesSupported: TFDPhysDefaultValues;
begin
  Result := dvNone;
end;

{-------------------------------------------------------------------------------}
function TFDPhysDataSnapMetadata.GetAsyncAbortSupported: Boolean;
begin
  Result := False;
end;

{-------------------------------------------------------------------------------}
function TFDPhysDataSnapMetadata.GetLimitOptions: TFDPhysLimitOptions;
begin
  Result := [];
end;

{-------------------------------------------------------------------------------}
function TFDPhysDataSnapMetadata.GetServerCursorSupported: Boolean;
begin
  Result := False;
end;

{-------------------------------------------------------------------------------}
function TFDPhysDataSnapMetadata.GetColumnOriginProvided: Boolean;
begin
  Result := False;
end;

end.
