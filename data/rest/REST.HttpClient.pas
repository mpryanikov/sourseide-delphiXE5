{*******************************************************}
{                                                       }
{             Delphi REST Client Framework              }
{                                                       }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}
unit REST.HttpClient;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  IPPeerAPI;

// Note that IPPeerClient must be used by the application in order to register
// the actual IPPeerAPI Indy implementation

type

  /// <summary>
  /// EHTTPProtocolException is an alias for  EIPHTTPProtocolExceptionPeer, so that
  /// there is no need to use IPPeerClient just for getting access to that exception type.
  /// </summary>
  EHTTPProtocolException = EIPHTTPProtocolExceptionPeer;

  /// <summary>
  /// Provides HTTP client functionality
  /// </summary>
  TRESTHTTP = class(TObject)
  private
    FPeer: IIPHTTP;
    FIPImplementationID: string;
    FPeerProcs: IIPPeerProcs;
    function GetProtocol: string;
    function GetRequest: IIPHTTPRequest;
    function GetResponse: IIPHTTPResponse;
    function GetHTTPOptions: TIPHTTPOptionsPeer;
    procedure SetHTTPOptions(options: TIPHTTPOptionsPeer);
    function GetProxyParams: IIPProxyConnectionInfo;
    function GetResponseCode: Integer;
    function GetURL: IIPURI;
    function GetHandleRedirects: Boolean;
    procedure SetHandleRedirects(AValue: Boolean);
    function GetAllowCookies: Boolean;
    procedure SetAllowCookies(AValue: Boolean);
    function GetCookieManager: IIPCookieManager;
    procedure SetCookieManager(const Value: IIPCookieManager);
    procedure SetProtocol(const Value: string);
    function GetConnectTimeout: integer;
    procedure SetConnectTimeout(const Value: integer);
    function GetReadTimeout: integer;
    procedure SetReadTimeout(const Value: integer);
  public
    // Utilities
    class function CreateMultipartFormStream(const AIPImplementationID: string): IIPMultipartFormDataStream;
      overload; static;
    class function FixupURL(const AIPImplementationID: string; const AURL: string): string; overload;

    function CreateMultipartFormStream: IIPMultipartFormDataStream; overload;
    function FixupURL(const AURL: string): string; overload;
    function URLEncode(const AURL: string): string;
    function ParamsEncode(const AURL: string): string;
    function HashStringAsHex(const AValue: string): string;
    function HashHMACSHA1(const AData, AKey: string): string;
    function CreateBasicAuthenticationString(const AUserName, APassword: string): string;
    function UriFromString(AUriString: string): IIPURI;
    function ReadStringAsCharset(AStream: TStream; const ACharset: string): string;

  public
    constructor Create; reintroduce; overload; virtual;
    constructor Create(const AIPImplementationID: string); reintroduce; overload; virtual;
    destructor Destroy; override;

    function Delete(AURL: string): string; overload;
    function Delete(AURL: string; AResponseStream: TStream): string; overload;
    procedure Merge(AURL: string; RequestStream: TStream);
    procedure Head(AURL: string);

    /// <summary>
    /// PUT command with empty content.
    /// </summary>
    function Put(AURL: string): string; overload;
    function Put(AURL: string; ASource: TStream): string; overload;
    procedure Put(AURL: string; ASource, AResponseContent: TStream); overload;
    function Post(AURL: string; ASource: TStream): string; overload;
    procedure Post(AURL: string; ASource, AResponseContent: TStream); overload;
    function Get(AURL: string): string; overload;
    function Get(AURL: string; AResponseContent: TStream): string; overload;
    procedure Disconnect;
    procedure AddServerCookie(const ACookie: string; const AURL: string);
    property Protocol: string read GetProtocol write SetProtocol;
    property Request: IIPHTTPRequest read GetRequest;
    property Response: IIPHTTPResponse read GetResponse;
    property HTTPOptions: TIPHTTPOptionsPeer read GetHTTPOptions write SetHTTPOptions;
    property ProxyParams: IIPProxyConnectionInfo read GetProxyParams;
    property ResponseCode: Integer read GetResponseCode;
    property URL: IIPURI read GetURL;
    property Peer: IIPHTTP read FPeer;
    property IPImplementationID: string read FIPImplementationID;
    property HandleRedirects: Boolean read GetHandleRedirects write SetHandleRedirects;
    property AllowCookies: Boolean read GetAllowCookies write SetAllowCookies;
    property CookieManager: IIPCookieManager read GetCookieManager write SetCookieManager;
    property ConnectTimeout: integer read GetConnectTimeout write SetConnectTimeout;
    property ReadTimeout: integer read GetReadTimeout write SetReadTimeout;
  end;

implementation

{$IFDEF MACOS}
uses
  Macapi.CoreFoundation;
{$ENDIF}
{ TRESTHTTP }

function TRESTHTTP.Delete(AURL: string): string;
var
  LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    FPeer.DoRequestDelete(AURL, nil, LStream, []);
    LStream.Position := 0;
    // This is here instead of a TStringStream for .net conversions?
    Result := IPProcs(IPImplementationID).ReadStringFromStream(LStream, -1);
    // , ContentTypeToEncoding(Response.ContentType));
  finally
    FreeAndNil(LStream);
  end;
end;

constructor TRESTHTTP.Create;
begin
  Create('');
end;

procedure TRESTHTTP.AddServerCookie(const ACookie, AURL: string);
begin
  FPeer.CookieManager.AddServerCookie(ACookie, AURL);
end;

constructor TRESTHTTP.Create(const AIPImplementationID: string);
begin
  inherited Create;
  FIPImplementationID := AIPImplementationID;
  FPeer := PeerFactory.CreatePeer(AIPImplementationID, IIPHTTP, nil) as IIPHTTP;
  FPeerProcs := IPProcs(AIPImplementationID);
  HTTPOptions := Self.HTTPOptions - [hoForceEncodeParams];
  Protocol := 'http'; //to have a default
  AllowCookies := true;
end;

function TRESTHTTP.CreateBasicAuthenticationString(const AUserName, APassword: string): string;
var
  LAuthentication: IIPBasicAuthentication;
begin
  LAuthentication := PeerFactory.CreatePeer(FIPImplementationID, IIPBasicAuthentication) as IIPBasicAuthentication;
  Assert(LAuthentication <> nil);
  if LAuthentication <> nil then
  begin
    LAuthentication.Password := APassword;
    LAuthentication.Username := AUserName;
    Result := LAuthentication.GetAuthentication;
  end;
end;

class function TRESTHTTP.CreateMultipartFormStream(const AIPImplementationID: string): IIPMultipartFormDataStream;
begin
  Result := PeerFactory.CreatePeer(AIPImplementationID, IPPeerAPI.IIPMultipartFormDataStream)
    as IIPMultipartFormDataStream;
end;

function TRESTHTTP.CreateMultipartFormStream: IIPMultipartFormDataStream;
begin
  Result := CreateMultipartFormStream(FIPImplementationID);
end;

function TRESTHTTP.Delete(AURL: string; AResponseStream: TStream): string;
begin
  FPeer.DoRequestDelete(AURL, nil, AResponseStream, []);
end;

destructor TRESTHTTP.Destroy;
begin
  // this is done internally : FPeer.IOHandler := nil;
  FPeer := nil;
  inherited;
end;

procedure TRESTHTTP.Disconnect;
begin
  FPeer.Disconnect;
end;

function TRESTHTTP.FixupURL(const AURL: string): string;
begin
  Result := TRESTHTTP.FixupURL(Self.IPImplementationID, AURL);
end;

/// <summary>
/// Clean up/fix the given URL. Add a possibly missing protocol (http is default), remove trailing white spaces.
/// Ensures no trailing slash exists.
/// </summary>
/// <example>
/// <see href="http://www.example.com">www.example.com</see> -&gt; <see href="http://www.example.com/" /><br />
/// <see href="http://www.example.com/some/path">www.example.com/some/path</see> -&gt;
/// <see href="http://www.example.com/some/path" />
/// </example>
class function TRESTHTTP.FixupURL(const AIPImplementationID: string; const AURL: string): string;
var
  LUri: IIPURI;
begin
  if trim(AURL) = '' then
    Result := ''
  else
  begin
    LUri := PeerFactory.CreatePeer(AIPImplementationID, IPPeerAPI.IIPURI) as IIPURI;
    LUri.URI := AURL;
    if LUri.GetProtocol = '' then
    begin
      LUri.URI := 'http://' + AURL;
    end;
    Result := trim(LUri.GetFullURI);
    // we don't want a trailing slash!
    if Result.Chars[Length(Result)-1] = '/' then
      System.Delete(Result, Length(Result), 1);
  end;
end;

function TRESTHTTP.Get(AURL: string; AResponseContent: TStream): string;
begin
  Result := FPeer.DoGet(AURL, AResponseContent);
end;

function TRESTHTTP.GetAllowCookies: Boolean;
begin
  Result := FPeer.AllowCookies;
end;

function TRESTHTTP.GetConnectTimeout: integer;
begin
  result := FPeer.ConnectTimeout;
end;

function TRESTHTTP.GetCookieManager: IIPCookieManager;
begin
  Result := FPeer.CookieManager;
end;

function TRESTHTTP.Get(AURL: string): string;
begin
  Result := FPeer.DoGet(AURL);
end;

function TRESTHTTP.GetHandleRedirects: Boolean;
begin
  Result := FPeer.HandleRedirects;
end;

function TRESTHTTP.GetHTTPOptions: TIPHTTPOptionsPeer;
begin
  Result := FPeer.HTTPOptions;
end;

function TRESTHTTP.GetProtocol: string;
begin
  Result := FPeer.Protocol;
end;

function TRESTHTTP.GetProxyParams: IIPProxyConnectionInfo;
begin
  Result := FPeer.ProxyParams;
end;

function TRESTHTTP.GetReadTimeout: integer;
begin
  result := FPeer.ReadTimeout;
end;

function TRESTHTTP.GetRequest: IIPHTTPRequest;
begin
  Result := FPeer.Request;
end;

function TRESTHTTP.GetResponse: IIPHTTPResponse;
begin
  Result := FPeer.Response;
end;

function TRESTHTTP.GetResponseCode: Integer;
begin
  Result := FPeer.ResponseCode;
end;

function TRESTHTTP.GetURL: IIPURI;
begin
  Result := FPeer.URL;
end;

// class function TRESTHTTP.HasPeerCertificate: Boolean;
// begin
// Result := False;
// end;

function TRESTHTTP.HashHMACSHA1(const AData, AKey: string): string;
begin
  Result := FPeerProcs.HashHMACSHA1(AData, AKey);
end;

function TRESTHTTP.HashStringAsHex(const AValue: string): string;
var
  LMD5: IIPHashMessageDigest5;
begin
  LMD5 := PeerFactory.CreatePeer(FIPImplementationID, IPPeerAPI.IIPHashMessageDigest5) as IIPHashMessageDigest5;
  Assert(LMD5 <> nil);
  if LMD5 <> nil then
    Result := LMD5.HashStringAsHex(AValue)
end;

procedure TRESTHTTP.Head(AURL: string);
begin
  FPeer.DoRequestHead(AURL, nil, nil, []);
end;

procedure TRESTHTTP.Merge(AURL: string; RequestStream: TStream);
begin
  FPeer.DoRequestMethod('MERGE', AURL, RequestStream, nil, []);
end;

function TRESTHTTP.Put(AURL: string): string;
var
  emptyStream: TMemoryStream;
begin
  emptyStream := TMemoryStream.Create;
  try
    Result := FPeer.DoPut(AURL, emptyStream);
  finally
    emptyStream.Free;
  end;
end;

// class procedure TRESTHTTP.RegisterProtocol(const AName: string;
// AClass: TRESTHTTPClass);
// begin
// FProtocols.Add(LowerCase(AName), AClass);
// end;

// procedure TRESTHTTP.SetAuthentication(auth: IIPAuthentication);
// begin
// FAuthentication := auth;
// FPeer.Request.Authentication := auth;
// end;
//
// procedure TRESTHTTP.SetBasicAuthentication(const user, password: string);
// begin
// FAuthentication := PeerFactory.CreatePeer(FIPImplementationID, IIPBasicAuthentication) as IIPBasicAuthentication;
// FAuthentication.Password := password;
// FAuthentication.Username := user;
// FPeer.Request.Authentication := FAuthentication;
// end;

procedure TRESTHTTP.SetAllowCookies(AValue: Boolean);
begin
  FPeer.AllowCookies := AValue;
end;

procedure TRESTHTTP.SetConnectTimeout(const Value: integer);
begin
   FPeer.ConnectTimeout := value;
end;

procedure TRESTHTTP.SetCookieManager(const Value: IIPCookieManager);
begin
  // FPeer.CookieManager := Value;
end;

procedure TRESTHTTP.SetHandleRedirects(AValue: Boolean);
begin
  FPeer.HandleRedirects := AValue;
end;

procedure TRESTHTTP.SetHTTPOptions(options: TIPHTTPOptionsPeer);
begin
  FPeer.HTTPOptions := options;
end;

procedure TRESTHTTP.SetProtocol(const Value: string);
var
  LProtocol: string;
  LIOHandler: IIPIOHandler;
begin
  LProtocol := Value.ToLower.trim;
  if FPeer.Protocol <> LProtocol then
  begin
    // for now we only support http and https. Other protocols via a "Register Protocol" mechanism
    // might be implemented later
    if (LProtocol = 'http') or (LProtocol = 'https') then
    begin
      Disconnect;
      FPeer.Protocol := Value;
      if LProtocol = 'http' then
      begin
        // HTTP - Create a plain Socket IOHandler with no owner
        LIOHandler := PeerFactory.CreatePeer(IPImplementationID, IIPIOHandlerSocket, nil) as IIPIOHandlerSocket;
      end
      else
      begin
        // HTPPS - Create a TIdSSLIOHandlerSocketOpenSSL with no owner
        LIOHandler := PeerFactory.CreatePeer(IPImplementationID, IIPSSLIOHandlerSocketOpenSSL, nil)
          as IIPSSLIOHandlerSocketOpenSSL;
                                                                                                             
        // LIOHandler.OnVerifyPeer := IdValidateCertificate;
        // LIOHandler.SSLOptions.VerifyMode := [TIPSSLVerifyModePeer.sslvrfClientOnce];
      end;
      FPeer.IOHandler := LIOHandler;
    end
    else
      raise EIPPeerException.Create('Unsupported Protocol!');
  end;
end;

procedure TRESTHTTP.SetReadTimeout(const Value: integer);
begin
  FPeer.ReadTimeout := Value;
end;

function TRESTHTTP.UriFromString(AUriString: string): IIPURI;
begin
  Result := PeerFactory.CreatePeer(Self.FIPImplementationID, IPPeerAPI.IIPURI) as IIPURI;
  Result.URI := AUriString;
end;

function TRESTHTTP.URLEncode(const AURL: string): string;
begin
  Result := FPeerProcs.URLEncode(AURL);
end;

function TRESTHTTP.ParamsEncode(const AURL: string): string;
begin
  Result := FPeerProcs.ParamsEncode(AURL);
end;

// class procedure TRESTHTTP.UnregisterProtocol(const AName: string);
// begin
// if FProtocols <> nil then
// FProtocols.Remove(LowerCase(AName));
// end;
//
// class function TRESTHTTP.RegisteredProtocolList: TArray<string>;
// var
// List: TArray<string>;
// LName: string;
// I: Integer;
// begin
// SetLength(List, FProtocols.Count);
// I := 0;
// for LName in FProtocols.Keys do
// begin
// List[I] := LName;
// Inc(I);
// end;
// Result := List;
// end;

procedure TRESTHTTP.Post(AURL: string; ASource, AResponseContent: TStream);
begin
  FPeer.DoPost(AURL, ASource, AResponseContent);
end;

function TRESTHTTP.Post(AURL: string; ASource: TStream): string;
begin
  Result := FPeer.DoPost(AURL, ASource);
end;

// class function TRESTHTTP.ProtocolClass(const AName: string): TRESTHTTPClass;
// begin
// Result := FProtocols[LowerCase(AName)];
// end;

procedure TRESTHTTP.Put(AURL: string; ASource, AResponseContent: TStream);
begin
  FPeer.DoPut(AURL, ASource, AResponseContent);
end;

function TRESTHTTP.ReadStringAsCharset(AStream: TStream; const ACharset: string): string;
begin
  Result := IPProcs(IPImplementationID).ReadStringAsCharset(AStream, ACharset);
end;

function TRESTHTTP.Put(AURL: string; ASource: TStream): string;
begin
  Result := FPeer.DoPut(AURL, ASource);
end;

// function TRESTHTTPS.IdValidateCertificate(Certificate: IIPX509; AOk: Boolean; ADepth, AError: Integer): Boolean;
// var
// Cert: TX509Certificate;
// begin
// Result := true;
// if Assigned(FOnValidateCertificate) then
// begin
// Cert := TX509CertificateIndy.Create(Certificate);
// try
// // prepare the TX509Ceritifcate and invoke user method
// FOnValidateCertificate(self, Cert, ADepth, Result);
// finally
// Cert.Free;
// end;
// end;
// end;

end.
