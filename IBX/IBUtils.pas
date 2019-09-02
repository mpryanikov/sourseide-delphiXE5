{$A8} {$R-}
{*************************************************************}
{                                                             }
{       CodeGear Delphi Visual Component Library              }
{       InterBase Express core components                     }
{                                                             }
{       Copyright (c) 1998-2013 Embarcadero                   }
{                                                             }
{    InterBase Express is based in part on the product        }
{    Free IB Components, written by Gregory H. Deatz for      }
{    Hoagland, Longo, Moran, Dunst & Doukas Company.          }
{    Free IB Components is used under license.                }
{                                                             }
{    Additional code created by Jeff Overcash and used        }
{    with permission.                                         }
{*************************************************************}

unit IBUtils;

interface

uses
  {$IFDEF MSWINDOWS} Winapi.Messages,{$ENDIF}
  System.Classes, Data.DB;


type
  TIBProtocols = (ibTCP, ibNamedPipe, ibSPX, ibLocal);

function Max(n1, n2: Integer): Integer;
function Min(n1, n2: Integer): Integer;
function RandomString(iLength: Integer): String;
function RandomInteger(iLow, iHigh: Integer): Integer;
function StripString(st: String; CharsToStrip: String): String;
function FormatIdentifier(Dialect: Integer; Value: String): String;
function FormatIdentifierValue(Dialect: Integer; Value: String): String;
function ExtractIdentifier(Dialect: Integer; Value: String): String;
function QuoteIdentifier(Dialect: Integer; Value: String): String;
function AddIBParamSQLForDetail(Params: TParams; SQL: String; Native: Boolean; Dialect : Integer): string;
procedure DecomposeDatabaseName(DatabaseName : String;
            var ServerName, Protocol, DatabasePath : String); overload; deprecated; {use either the SSL version or the TIBProtocols version}
procedure DecomposeDatabaseName(DatabaseName : String;
            var ServerName, Port : string;
            var Protocol : TIBProtocols;
            var DatabasePath : String); overload;
procedure DecomposeDatabaseName(DatabaseName : String;
            var ServerName, Port : String;
            var Protocol : TIBProtocols;
            var DatabasePath : String;
            var SSL : Boolean;
            var ServerPublicFile, ServerPublicPath, ClientCertFile,
                ClientPassPhraseFile, ClientPassPhrase : String); overload;
function ComposeDatabaseName(ServerName, Port : String;
            Protocol : TIBProtocols;
            DatabasePath : String;
            SSL : Boolean = false;
            ServerPublicFile : String = '';  ServerPublicPath : String = '';
            ClientCertFile : String = ''; ClientPassPhraseFile  : String = '';
            ClientPassPhrase : String = '') : string;

type

  TIBTimer = class(TComponent)
  private
    FInterval: Cardinal;
    {$IFDEF MSWINDOWS}
    FWindowHandle: THandle;
    {$ENDIF MSWINDOWS}
    FOnTimer: TNotifyEvent;
    FEnabled: Boolean;
    procedure UpdateTimer;
    procedure SetEnabled(Value: Boolean);
    procedure SetInterval(Value: Cardinal);
    procedure SetOnTimer(Value: TNotifyEvent);
    {$IFDEF MSWINDOWS}
    procedure WndProc(var Msg: TMessage);
    {$ENDIF}
  protected
    procedure Timer; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;
  end;

var
  CopyMasterFieldToDetail : Boolean;

implementation

uses {$ifdef MSWINDOWS} Winapi.Windows, {$endif} System.SysUtils, IBXConst,
     IB, Data.DBCommon;

function Max(n1, n2: Integer): Integer;
begin
  if (n1 > n2) then
    result := n1
  else
    result := n2;
end;

function Min(n1, n2: Integer): Integer;
begin
  if (n1 < n2) then
    result := n1
  else
    result := n2;
end;

function RandomString(iLength: Integer): String;
begin
  Result := '';
  while Result.Length < iLength do
    Result := result + IntToStr(RandomInteger(0, High(Integer)));
  if Result.Length > iLength then
    Result := Result.Substring(0, iLength);
end;

function RandomInteger(iLow, iHigh: Integer): Integer;
begin
  result := Random(iHigh - iLow) + iLow;
end;

function StripString(st: String; CharsToStrip: String): String;
var
  c : Char;
begin
  result := '';
  for c in st do
  begin
    if not CharsToStrip.Contains(c) then
      result := result + c;
  end;
end;

function FormatIdentifier(Dialect: Integer; Value: String): String;
begin
  Value := Trim(Value);
  if Dialect = 1 then
    Value := AnsiUpperCase(Value)
  else
    if (Value <> '') and (Value[Low(Value)] = '"') then
      Value := '"' + StringReplace (TrimRight(Value), '"', '""', [rfReplaceAll]) + '"'
    else
      Value := AnsiUpperCase(Value);
  Result := Value;
end;

function FormatIdentifierValue(Dialect: Integer; Value: String): String;
begin
  Value := Value.Trim;
  if Dialect = 1 then
    Value := Value.ToUpper
  else
  begin
    if (Value <> '') and (Value[Low(Value)] = '"') then
    begin
      Value := Value.Remove(0, 1);
      Value := Value.Remove(Value.Length - 1);
      Value := StringReplace (Value, '""', '"', [rfReplaceAll]);
    end
    else
      Value := Value.ToUpper;
  end;
  Result := Value;
end;

function ExtractIdentifier(Dialect: Integer; Value: String): String;
begin
  Value := Value.Trim;
  if Dialect = 1 then
    Value := Value.ToUpper
  else
  begin
    if (Value <> '') and (Value[Low(Value)] = '"') then
    begin
      Value := Value.Remove(0, 1);
      Value := Value.Remove(Value.Length - 1, 1);
      Value := StringReplace (Value, '""', '"', [rfReplaceAll]);
    end
    else
      Value := Value.ToUpper;
  end;
  Result := Value;
end;

function QuoteIdentifier(Dialect: Integer; Value: String): String;
begin
  if Dialect = 1 then
    Value := Value.Trim.ToUpper
  else
    Value := '"' + StringReplace (Value, '"', '""', [rfReplaceAll]) + '"';
  Result := Value;
end;

function AddIBParamSQLForDetail(Params: TParams; SQL: String; Native: Boolean; Dialect : Integer): string;
const
  SWhere = ' where ';     { do not localize }
  SAnd = ' and ';         { do not localize }

  function GenerateParamSQL: string;  
  var
    I: Integer;
  begin
    for I := 0 to Params.Count -1 do
    begin
      if I > 0 then Result := Result + SAnd;
      if Native then
        Result := Result + format('%s = ?', [QuoteIdentifier(Dialect, Params[I].Name)]) {do not localize}
      else
        Result := Result + format('%0:s = :%0:s', [QuoteIdentifier(Dialect, Params[I].Name)]); {do not localize}
    end;
    if Result.ToLower.Contains(SWhere) then
      Result := SAnd + Result
    else
      Result := SWhere + Result;
  end;

  function AddWhereClause: String;
  var
    Start: PWideChar;
    Rest, FName: String;
    SQLToken, CurSection: TSQLToken;
  begin
    Start := PWideChar(SQL);
    CurSection := stUnknown;
    repeat
      SQLToken := NextSQLToken(Start, FName, CurSection);
    until SQLToken in [stFrom, stEnd];
    if SQLToken = stFrom then
      NextSQLToken(Start, FName, CurSection);
    Rest := Start;
    if Rest = '' then                {do not localize}
      Result := SQL + ' ' + GenerateParamSQL   {do not localize}
    else
      Result := SQL.Substring(0, SQL.IndexOf(Rest)) + ' ' + GenerateParamSQL + Rest;   {do not localize}
  end;

begin
  Result := SQL;
  if (Params.Count > 0) then
    Result := AddWhereClause;
end;

procedure DecomposeDatabaseName(DatabaseName : String;
  var ServerName, Protocol, DatabasePath : String);
var
  p : TIBProtocols;
  port : string;
begin
  DecomposeDatabaseName(DatabaseName, ServerName, Port, p, DatabasePath);
  case p of
    ibTCP: Protocol := 'TCP';
    ibNamedPipe: Protocol := 'NamedPipe';
    ibSPX: Protocol := 'SPX';
    ibLocal: Protocol := 'Local';
  end;
end;

function ComposeDatabaseName(ServerName, Port : String;
            Protocol : TIBProtocols;
            DatabasePath : String;
            SSL : Boolean = false;
            ServerPublicFile : String = '';  ServerPublicPath : String = '';
            ClientCertFile : String = ''; ClientPassPhraseFile  : String = '';
            ClientPassPhrase : String = '') : string;
const
  SPF = '?serverPublicFile='; {do nit localize}
  SPP = '?serverPublicPath=';  {do not localize}
  CCF = '?clientCertFile=';   {do not localize}
  CPPF = '?clientPassPhraseFile=';  {do not localize}
  CPP = '?clientPassPhrase=';    {do not localize}

begin
  Result := ServerName;
  if Port <> '' then     {do not localize}
    Result := Result + '/' + Port;     {do not localize}
  if SSL then
  begin
    if (ServerPublicFile <> '') and     {do not localize}
       (ServerPublicPath <> '') then    {do not localize}
      raise Exception.Create(SSSLSeverExclusive);
    if (ClientPassPhraseFile <> '') and    {do not localize}
       (ClientPassPhrase <> '') then       {do not localize}
      raise Exception.Create(SSSLClientExclusive);

    Result := Result + '?ssl=true';      {do not localize}
    if ServerPublicFile <> '' then        {do not localize}
      Result := Result + SPF + ServerPublicFile;
    if ServerPublicPath <> '' then        {do not localize}
      Result := Result + SPP + ServerPublicPath;
    if ClientCertFile <> '' then            {do not localize}
      Result := Result + CCF + ClientCertFile;
    if ClientPassPhraseFile <> '' then       {do not localize}
      Result := Result + CPPF + ClientPassPhraseFile;
    if ClientPassPhrase <> '' then             {do not localize}
      Result := Result + CPP + ClientPassPhrase;
    Result := Result + '??';                   {do not localize}
  end;
  case Protocol of
    ibTCP : Result := Result + ':' + DatabasePath;  {do not localize}
    ibNamedPipe : Result := '\\' + Result + '\' + DatabasePath;  {do not localize}
    ibSPX : Result := Result + '@' + DatabasePath;   {do not localize}
    ibLocal : Result := DatabasePath;
  end;
end;

procedure DecomposeDatabaseName(DatabaseName : String;
            var ServerName, Port : string;
            var Protocol : TIBProtocols;
            var DatabasePath : String); overload;
var
  Idx1, Idx2, Idx3: Integer;

  procedure SplitServerFromPort(PathIdx : Integer);
  begin
    Idx3 := ServerName.IndexOf('/');  {do not localize}
    if Idx3 >= 0 then
    begin
      Port := ServerName.Substring(Idx3+ 1);
      ServerName := ServerName.Substring(0, Idx3);
    end;
    DatabasePath := DatabaseName.SubString(PathIdx);
  end;

begin
  Port := '';                       {do not localize}
  ServerName := '';                {do not localize}
  DatabasePath := '';              {do not localize}
  if DatabaseName.Contains('\\') then {do not localize}
  begin
    Protocol := ibNamedPipe;
    DatabaseName := DatabaseName.Substring(2);
    Idx1 := DatabaseName.IndexOf('\'); {do not localize}
    if Idx1 < 0 then
      IBError(ibxeUnknownError, [nil])
    else
    begin
      ServerName := DatabaseName.Substring(0, Idx1);
      SplitServerFromPort(Idx1 + 1);
    end;
  end
  else
  begin
    Idx1 := DatabaseName.IndexOf(':'); {do not localize}
    Idx2 := DatabaseName.IndexOf('@'); {do not localize}
    If ((Idx1 < 0) or (Idx1 = 1)) and
        (idx2 < 0) then
    begin
      DatabasePath := DatabaseName;
      Protocol := ibLocal;
    end
    else
    begin
      if Idx2 < 0 then
      begin
        Protocol := ibTCP;
        ServerName := DatabaseName.Substring(0, Idx1);
        SplitServerFromPort(Idx1 + 1);
      end
      else
      begin
        Protocol := ibSPX;
        ServerName := DatabaseName.Substring(0, Idx2);
        SplitServerFromPort(Idx2 + 1);
      end;
    end;
  end;
end;

procedure DecomposeDatabaseName(DatabaseName : String;
            var ServerName, Port : String;
            var Protocol : TIBProtocols;
            var DatabasePath : String;
            var SSL : Boolean;
            var ServerPublicFile, ServerPublicPath, ClientCertFile,
                ClientPassPhraseFile, ClientPassPhrase : String);
const
  SPF = '?serverpublicfile='; {do nit localize}
  SPP = '?serverpublicpath=';  {do not localize}
  CCF = '?clientcertfile=';   {do not localize}
  CPPF = '?clientpassphrasefile=';  {do not localize}
  CPP = '?clientpassphrase=';    {do not localize}

var
  lDBName, SSLSection : String;
  idx1 : Integer;

  procedure StripSSL;
  var
    idx1, idx2 : Integer;
  begin
    idx1 := DatabaseName.ToLower.IndexOf('?ssl=true');  {do not localize}
    idx2 := DatabaseName.IndexOf('??');  {do not localize}
    if idx1 >= 0 then
    begin
      if idx2 < 0 then
        raise Exception.Create(SMalformedSSLConnection);
      if idx1 >= idx2 then
        raise Exception.Create(SMalformedSSLConnection2);
      lDBName := DatabaseName.Remove(idx1, idx2 - idx1 + 2);
      SSLSection := DatabaseName.Substring(idx1, idx2 - idx1 + 2);
    end
    else
      lDBName := DatabaseName;
  end;

  function ExtractPiece(Piece : String; starting : Integer) : string;
  var
    i : Integer;
    sSub : String;
  begin
    sSub := SSLSection.Substring(starting);
    i := sSub.IndexOf('?');
    if i >= 0 then
      Result := sSub.Substring(0, i)
    else
      Result := '';
  end;


begin
  ServerPublicfile := '';           {do not localize}
  ServerPublicPath := '';           {do not localize}
  ClientCertFile := '';             {do not localize}
  ClientPassPhraseFile := '';        {do not localize}
  ClientPassPhrase := '';            {do not localize}
  // strip it into the components of the connection string and the SSL section
  StripSSL;
  // Decompose jsust the connection part
  DecomposeDatabaseName(lDBName, ServerName, Port, Protocol, DatabasePath);
  if SSLSection = '' then            {do not localize}
    SSL := false
  else
  begin
    // Decompose the SSL pieces
    SSL := true;
    idx1 := SSLSection.ToLower.IndexOf(SPF);
    if idx1 >= 0 then
      ServerPublicFile := ExtractPiece(SSLSection, idx1 + SPF.Length);

    idx1 := SSLSection.ToLower.IndexOf(SPP);
    if idx1 >= 0 then
      ServerPublicPath := ExtractPiece(SSLSection, idx1 + SPP.Length);

    idx1 := SSLSection.ToLower.IndexOf(CCF);
    if idx1 >= 0 then
      ClientCertFile := ExtractPiece(SSLSection, idx1 + CCF.Length);

    idx1 := SSLSection.ToLower.IndexOf(CPPF);
    if idx1 >= 0 then
      ClientPassPhraseFile := ExtractPiece(SSLSection, idx1 + CPPF.Length);

    idx1 := SSLSection.ToLower.IndexOf(CPP);
    if idx1 >= 0 then
      ClientPassPhrase := ExtractPiece(SSLSection, idx1 + SPP.Length);
  end;
end;


{ TIBTimer }

constructor TIBTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := True;
  FInterval := 1000;
  {$IFDEF MSWINDOWS}
  FWindowHandle := AllocateHWnd(WndProc);
  {$ENDIF}
end;

destructor TIBTimer.Destroy;
begin
  FEnabled := False;
  UpdateTimer;
  {$IFDEF MSWINDOWS}
  DeallocateHWnd(FWindowHandle);
  {$ENDIF}
  inherited Destroy;
end;

{$IFDEF MSWINDOWS}
procedure TIBTimer.WndProc(var Msg: TMessage);
begin
  if Msg.Msg = WM_TIMER then
    try
      Timer;
    except
      if Assigned(ApplicationHandleException) then
        ApplicationHandleException(Self);
    end
  else
    Msg.Result := DefWindowProc(FWindowHandle, Msg.Msg, Msg.wParam, Msg.lParam);
end;
{$ENDIF}

procedure TIBTimer.UpdateTimer;
begin
{$IFDEF MSWINDOWS}
  KillTimer(FWindowHandle, 1);
  if (FInterval <> 0) and FEnabled and Assigned(FOnTimer) then
    if SetTimer(FWindowHandle, 1, FInterval, nil) = 0 then
      raise EOutOfResources.Create(SNoTimers);
{$ENDIF}
end;

procedure TIBTimer.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    UpdateTimer;
  end;
end;

procedure TIBTimer.SetInterval(Value: Cardinal);
begin
  if Value <> FInterval then
  begin
    FInterval := Value;
    UpdateTimer;
  end;
end;

procedure TIBTimer.SetOnTimer(Value: TNotifyEvent);
begin
  FOnTimer := Value;
  UpdateTimer;
end;

procedure TIBTimer.Timer;
begin
  if Assigned(FOnTimer) then FOnTimer(Self);
end;

initialization
  CopyMasterFieldToDetail := false;
end.
