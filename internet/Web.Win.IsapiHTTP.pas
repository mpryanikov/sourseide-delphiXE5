{*******************************************************}
{                                                       }
{                Delphi Runtime Library                 }
{                                                       }
{ Copyright(c) 1995-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit Web.Win.IsapiHTTP;

interface

uses Winapi.Windows, System.Classes, Web.HTTPApp, Winapi.Isapi2;

type
  TISAPIRequest = class(TWebRequest)
  private
    FECB: PEXTENSION_CONTROL_BLOCK;
    function GetHost: AnsiString;
  protected
    function GetStringVariable(Index: Integer): AnsiString; override;
    function GetDateVariable(Index: Integer): TDateTime; override;
    function GetIntegerVariable(Index: Integer): Integer; override;
    function GetRawPathInfo: AnsiString; override;
  public
    constructor Create(AECB: PEXTENSION_CONTROL_BLOCK);
    function GetFieldByName(const Name: AnsiString): AnsiString; override;
    function ReadClient(var Buffer; Count: Integer): Integer; override;
    function ReadString(Count: Integer): AnsiString; override;
    function TranslateURI(const URI: string): string; override;
    function WriteClient(var Buffer; Count: Integer): Integer; override;
    function WriteString(const AString: AnsiString): Boolean; override;
    function WriteHeaders(StatusCode: Integer; const StatusString, Headers: AnsiString): Boolean; override;
    property ECB: PEXTENSION_CONTROL_BLOCK read FECB;
  end;

  TISAPIResponse = class(TWebResponse)
  private
    FStatusCode: Integer;
    FStringVariables: array[0..MAX_STRINGS - 1] of AnsiString;
    FIntegerVariables: array[0..MAX_INTEGERS - 1] of Integer;
    FDateVariables: array[0..MAX_DATETIMES - 1] of TDateTime;
    FContent: AnsiString;
    FSent: Boolean;
  protected
    function GetContent: AnsiString; override;
    function GetDateVariable(Index: Integer): TDateTime; override;
    function GetIntegerVariable(Index: Integer): Integer; override;
    function GetLogMessage: string; override;
    function GetStatusCode: Integer; override;
    function GetStringVariable(Index: Integer): AnsiString; override;
    procedure SetContent(const Value: AnsiString); override;
    procedure SetDateVariable(Index: Integer; const Value: TDateTime); override;
    procedure SetIntegerVariable(Index: Integer; Value: Integer); override;
    procedure SetLogMessage(const Value: string); override;
    procedure SetStatusCode(Value: Integer); override;
    procedure SetStringVariable(Index: Integer; const Value: AnsiString); override;
    procedure InitResponse; virtual;
  public
    constructor Create(HTTPRequest: TWebRequest);
    procedure SendResponse; override;
    procedure SendRedirect(const URI: AnsiString); override;
    procedure SendStream(AStream: TStream); override;
    function Sent: Boolean; override;
  end;

implementation

uses System.SysUtils, Web.BrkrConst;

const
  ServerVariables: array[0..28] of AnsiString = (
    '',
    'SERVER_PROTOCOL',
    'URL',
    '',
    '',
    '',
    'HTTP_CACHE_CONTROL',
    'HTTP_DATE',
    'HTTP_ACCEPT',
    'HTTP_FROM',
    'HTTP_HOST',
    'HTTP_IF_MODIFIED_SINCE',
    'HTTP_REFERER',
    'HTTP_USER_AGENT',
    'HTTP_CONTENT_ENCODING',
    'CONTENT_TYPE',
    'CONTENT_LENGTH',
    'HTTP_CONTENT_VERSION',
    'HTTP_DERIVED_FROM',
    'HTTP_EXPIRES',
    'HTTP_TITLE',
    'REMOTE_ADDR',
    'REMOTE_HOST',
    'SCRIPT_NAME',
    'SERVER_PORT',
    '',
    'HTTP_CONNECTION',
    'HTTP_COOKIE',
    'HTTP_AUTHORIZATION');

{ TISAPIRequest }

constructor TISAPIRequest.Create(AECB: PEXTENSION_CONTROL_BLOCK);
begin
  FECB := AECB;
  inherited Create;
end;

function TISAPIRequest.GetFieldByName(const Name: AnsiString): AnsiString;
var
  Buffer: array[0..4095] of AnsiChar;
  Size: DWORD;
begin
  Size := SizeOf(Buffer);
  if ECB.GetServerVariable(ECB.ConnID, PAnsiChar(Name), @Buffer, Size) or
     ECB.GetServerVariable(ECB.ConnID, PAnsiChar(AnsiString('HTTP_') + Name), @Buffer, Size) then  { do not localize }
  begin
    if Size > 0 then Dec(Size);
    SetString(Result, Buffer, Size);
  end else Result := '';
end;

function TISAPIRequest.GetStringVariable(Index: Integer): AnsiString;
begin
  case Index of
    0: Result := ECB.lpszMethod;
    3: Result := ECB.lpszQueryString;
    4: Result := ECB.lpszPathInfo;
    5: Result := ECB.lpszPathTranslated;
    10: Result := GetHost;
    1..2, 6..9, 11..24, 26..28: Result := GetFieldByName(ServerVariables[Index]);
    25: if ECB.cbAvailable > 0 then
      SetString(Result, PAnsiChar(ECB.lpbData), ECB.cbAvailable);
   else
      Result := '';
  end;
end;

// Get untranslated URL so that application can support URL's with encoded characters (e.g.; encoded '/' = %2F).
function TISAPIRequest.GetRawPathInfo: AnsiString;
var
  LHttpURL: AnsiString;
  LURL: AnsiString;
  LIndex: Integer;
begin
  LHttpUrl := GetFieldByName('HTTP_URL');
  LUrl := GetFieldByName('URL');
  LIndex := Pos(LUrl, LHttpUrl);
  if LIndex > 0 then
  begin
    Result := Copy(LHttpUrl, LIndex + Length(LUrl), MaxInt);
    // Strip the query string
    if ECB.lpszQueryString <> '' then
    begin
      LIndex := Pos(AnsiString('?'), Result);
      if LIndex > 0 then
         Delete(Result, LIndex, MaxInt);
    end;
  end
  else
    Result := ECB.lpszPathInfo; // Revert to non-raw
end;

// Strip the port from the host name
function TISAPIRequest.GetHost: AnsiString;
var
  I: Integer;
begin
  Result := GetFieldByName('HTTP_HOST');
  I := Pos(AnsiString(':'), Result);
  if I > 0 then
    Delete(Result, I, MaxInt);
end;

function TISAPIRequest.GetDateVariable(Index: Integer): TDateTime;
var
  Value: string;
begin
  Value := string(GetStringVariable(Index));
  if Value <> '' then
    Result := ParseDate(Value)
  else Result := -1;
end;

function TISAPIRequest.GetIntegerVariable(Index: Integer): Integer;
var
  Value: string;
begin
  Value := string(GetStringVariable(Index));
  if Value <> '' then
    Result := StrToInt(Value)
  else Result := -1;
end;

function TISAPIRequest.ReadClient(var Buffer; Count: Integer): Integer;
begin
  Result := Count;
  if not ECB.ReadClient(ECB.ConnID, @Buffer, DWORD(Result)) then
    Result := -1;
end;

function TISAPIRequest.ReadString(Count: Integer): AnsiString;
var
  Len: Integer;
begin
  SetLength(Result, Count);
  Len := ReadClient(Pointer(Result)^, Count);
  if Len > 0 then
    SetLength(Result, Len)
  else Result := '';
end;

function TISAPIRequest.TranslateURI(const URI: string): string;
var
  PathBuffer: array[0..1023] of AnsiChar;
  Size: Integer;
begin
  StrCopy(PathBuffer, PAnsiChar(AnsiString(URI)));
  Size := SizeOf(PathBuffer);
  if ECB.ServerSupportFunction(ECB.ConnID, HSE_REQ_MAP_URL_TO_PATH,
    @PathBuffer, @Size, nil) then
      Result := string(PathBuffer)
  else Result := '';
end;

function TISAPIRequest.WriteClient(var Buffer; Count: Integer): Integer;
begin
  Result := Count;
  if not ECB.WriteClient(ECB.ConnID, @Buffer, DWORD(Result), 0) then
    Result := -1;
end;

function TISAPIRequest.WriteString(const AString: AnsiString): Boolean;
begin
  Result := WriteClient(Pointer(AString)^, Length(AString)) = Length(AString);
end;

function TISAPIRequest.WriteHeaders(StatusCode: Integer;
  const StatusString, Headers: AnsiString): Boolean;
begin
  TISAPIRequest(Self).ECB.dwHttpStatusCode := StatusCode;
  with TISAPIRequest(Self) do
    ECB.ServerSupportFunction(ECB.ConnID, HSE_REQ_SEND_RESPONSE_HEADER,
      PAnsiChar(AnsiString(StatusString)), nil, LPDWORD(AnsiString(Headers)));
  Result := True;
end;

{ TISAPIResponse }

constructor TISAPIResponse.Create(HTTPRequest: TWebRequest);
begin
  inherited Create(HTTPRequest);
  InitResponse;
end;

procedure TISAPIResponse.InitResponse;
begin
  if FHTTPRequest.ProtocolVersion = '' then
    Version := '1.0'; 
  StatusCode := 200;
  LastModified := -1;
  Expires := -1;
  Date := -1;
  ContentType := 'text/html';  { do not localize }
end;

function TISAPIResponse.GetContent: AnsiString;
begin
  Result := FContent;
end;

function TISAPIResponse.GetDateVariable(Index: Integer): TDateTime;
begin
  if (Index >= Low(FDateVariables)) and (Index <= High(FDateVariables)) then
    Result := FDateVariables[Index]
  else Result := 0.0;
end;

function TISAPIResponse.GetIntegerVariable(Index: Integer): Integer;
begin
  if (Index >= Low(FIntegerVariables)) and (Index <= High(FIntegerVariables)) then
    Result := FIntegerVariables[Index]
  else Result := -1;
end;

function TISAPIResponse.GetLogMessage: string;
begin
  Result := string(TISAPIRequest(HTTPRequest).ECB.lpszLogData);
end;

function TISAPIResponse.GetStatusCode: Integer;
begin
  Result := FStatusCode;
end;

function TISAPIResponse.GetStringVariable(Index: Integer): AnsiString;
begin
  if (Index >= Low(FStringVariables)) and (Index <= High(FStringVariables)) then
    Result := FStringVariables[Index];
end;

function TISAPIResponse.Sent: Boolean;
begin
  Result := FSent;
end;

procedure TISAPIResponse.SetContent(const Value: AnsiString);
begin
  FContent := Value;
  if ContentStream = nil then
    ContentLength := Length(FContent);
end;

procedure TISAPIResponse.SetDateVariable(Index: Integer; const Value: TDateTime);
begin
  if (Index >= Low(FDateVariables)) and (Index <= High(FDateVariables)) then
    if Value <> FDateVariables[Index] then
      FDateVariables[Index] := Value;
end;

procedure TISAPIResponse.SetIntegerVariable(Index: Integer; Value: Integer);
begin
  if (Index >= Low(FIntegerVariables)) and (Index <= High(FIntegerVariables)) then
    if Value <> FIntegerVariables[Index] then
      FIntegerVariables[Index] := Value;
end;

procedure TISAPIResponse.SetLogMessage(const Value: string);
begin
  StrPLCopy(TISAPIRequest(HTTPRequest).ECB.lpszLogData, AnsiString(Value), HSE_LOG_BUFFER_LEN - 1);
end;

{+ ! Strings not to be resourced !!}
procedure TISAPIResponse.SetStatusCode(Value: Integer);
begin
  if FStatusCode <> Value then
  begin
    FStatusCode := Value;
    ReasonString := StatusString(Value);
  end;
end;

procedure TISAPIResponse.SetStringVariable(Index: Integer; const Value: AnsiString);
begin
  if (Index >= Low(FStringVariables)) and (Index <= High(FStringVariables)) then
    FStringVariables[Index] := Value;
end;

procedure TISAPIResponse.SendResponse;
var
  StatusString: string;
  Headers: string;
  I: Integer;

  procedure AddHeaderItem(const Item: AnsiString; FormatStr: string); overload;
  begin
    if Item <> '' then
      Headers := Headers + Format(FormatStr, [Item]);
  end;

  procedure AddHeaderItem(const Item: UnicodeString; FormatStr: string); overload;
  begin
    if Item <> '' then
      Headers := Headers + Format(FormatStr, [Item]);
  end;

begin
  if HTTPRequest.ProtocolVersion <> '' then
  begin
    if (ReasonString <> '') and (StatusCode > 0) then
      StatusString := Format('%d %s', [StatusCode, ReasonString])
    else StatusString := '200 OK';                                              {do not localize }
    AddHeaderItem(Location, 'Location: %s'#13#10);                              {do not localize }
    AddHeaderItem(Allow, 'Allow: %s'#13#10);                                    {do not localize }
    for I := 0 to Cookies.Count - 1 do
      AddHeaderItem(Cookies[I].HeaderValue, 'Set-Cookie: %s'#13#10);            {do not localize }
    AddHeaderItem(DerivedFrom, 'Derived-From: %s'#13#10);                       {do not localize }
    if Expires > 0 then
      AddHeaderItem(Format(FormatDateTime(sDateFormat + ' "GMT"',               {do not localize}
        Expires), [DayOfWeekStr(Expires), MonthStr(Expires)]), 'Expires: %s'#13#10);  {do not localize}

    if LastModified > 0 then
      AddHeaderItem(Format(FormatDateTime(sDateFormat +
        ' "GMT"', LastModified), [DayOfWeekStr(LastModified),                   {do not localize}
        MonthStr(LastModified)]), 'Last-Modified: %s'#13#10);                         {do not localize}
    AddHeaderItem(Title, 'Title: %s'#13#10);                                    {do not localize }
    AddHeaderItem(FormatAuthenticate, 'WWW-Authenticate: %s'#13#10);               {do not localize }
    AddCustomHeaders(Headers);
    AddHeaderItem(ContentVersion, 'Content-Version: %s'#13#10);                 {do not localize }
    AddHeaderItem(ContentEncoding, 'Content-Encoding: %s'#13#10);               {do not localize }
    AddHeaderItem(ContentType, 'Content-Type: %s'#13#10);                       {do not localize }
    if (RawContent <> '') or (ContentStream <> nil) then
      AddHeaderItem(IntToStr(ContentLength), 'Content-Length: %s'#13#10);       {do not localize }
    Headers := Headers + #13#10;
    HTTPRequest.WriteHeaders(StatusCode, AnsiString(StatusString), AnsiString(Headers));
  end;
  if ContentStream = nil then
    HTTPRequest.WriteString(RawContent)
  else if ContentStream <> nil then
  begin
    SendStream(ContentStream);
    ContentStream := nil; // Drop the stream
  end;
  FSent := True;
end;

procedure TISAPIResponse.SendRedirect(const URI: AnsiString);
begin
  with TISAPIRequest(FHTTPRequest) do
    ECB.ServerSupportFunction(ECB.ConnID, HSE_REQ_SEND_URL_REDIRECT_RESP,
      PAnsiChar(URI), nil, nil);
  FSent := True;
end;

procedure TISAPIResponse.SendStream(AStream: TStream);
var
  Buffer: array[0..8191] of Byte;
  BytesToSend: Integer;
begin
  while AStream.Position < AStream.Size do
  begin
    BytesToSend := AStream.Read(Buffer, SizeOf(Buffer));
    FHTTPRequest.WriteClient(Buffer, BytesToSend);
  end;
end;

end.
