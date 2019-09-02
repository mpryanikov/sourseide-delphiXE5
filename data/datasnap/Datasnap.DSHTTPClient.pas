{*******************************************************}
{                                                       }
{               Delphi DataSnap Framework               }
{ Copyright(c) 2012-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit Datasnap.DSHTTPClient;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  Data.DBXCommon, Data.DBXCommonIndy, IPPeerAPI;

type
  TDSHTTPClass = class of TDSHTTP;

  ///  <summary> Provides HTTP client functionality
  ///  </summary>
  TDSHTTP = class(TComponent)
  private
    FPeer: IIPHTTP;
    FOnValidateCertificate: TValidateCertificate;
    FAuthentication: IIPAuthentication;
    FIPImplementationID: string;
    class var FProtocols: TDictionary<string, TDSHTTPClass>;
    function GetProtocol: string;
    function GetRequest: IIPHTTPRequest;
    function GetResponse: IIPHTTPResponse;
    function GetHTTPOptions: TIPHTTPOptionsPeer;
    procedure SetHTTPOptions(options: TIPHTTPOptionsPeer);
    function GetProxyParams: IIPProxyConnectionInfo;
    function GetResponseCode: Integer;
    function GetURL: IIPURI;
  public
    class constructor Create;
    class destructor Destroy;
    constructor Create; reintroduce; overload; virtual;
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(const AIPImplementationID: string); reintroduce; overload; virtual;
    constructor Create(AOwner: TComponent; const AIPImplementationID: string); reintroduce; overload; virtual;
    destructor Destroy; override;
    class function HasPeerCertificate: Boolean; virtual;
    class procedure RegisterProtocol(const AName: string; AClass: TDSHTTPClass);
    class procedure UnregisterProtocol(const AName: string);
    class function RegisteredProtocolList: TArray<string>; static;
    class function ProtocolClass(const AName: string): TDSHTTPClass; static;

    ///  <summary> Sends a DELETE command type request. It returns the response.
    ///  </summary>
    ///  <param name="AURL">URL where the delete command is sent</param>
    ///  <returns>Server response</returns>
    function Delete(AURL: string): string; overload;
    function Delete(AURL: string; AResponseStream: TStream): string; overload;
    procedure Merge(AURL: string; RequestStream: TStream);
    procedure Head(AURL: string);
    ///  <summary>PUT command with empty content</summary>
    function Put(AURL: string): string; overload;
    function Put(AURL: string; ASource: TStream): string; overload;
    procedure Put(AURL: string; ASource, AResponseContent: TStream); overload;
    function Post(AURL: string; ASource: TStream): string; overload;
    procedure Post(AURL: string; ASource, AResponseContent: TStream); overload;
    function Get(AURL: string): string; overload;
    function Get(AURL: string; AResponseContent: TStream): string; overload;
    procedure SetBasicAuthentication(const user: string; const password: string);
    procedure SetAuthentication(auth: IIPAuthentication);
    property OnValidatePeerCertificate: TValidateCertificate read FOnValidateCertificate write FOnValidateCertificate;
    procedure Disconnect;
    property Protocol: string read GetProtocol;
    property Request: IIPHTTPRequest read GetRequest;
    property Response: IIPHTTPResponse read GetResponse;
    property HTTPOptions: TIPHTTPOptionsPeer read GetHTTPOptions write SetHTTPOptions;
    property ProxyParams: IIPProxyConnectionInfo read GetProxyParams;
    property ResponseCode: Integer read GetResponseCode;
    property URL: IIPURI read GetURL;
    property Peer: IIPHTTP read FPeer;
    property IPImplementationID: string read FIPImplementationID;
  end;

  TDSHTTPS = class(TDSHTTP)
  private
    function IdValidateCertificate(Certificate: IIPX509; AOk: Boolean;
      ADepth, AError: Integer): Boolean;
  public
    constructor Create; overload; override;
    constructor Create(const AIPImplementationID: string); overload; override;
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; const AIPImplementationID: string); overload; override;
    class function HasPeerCertificate: Boolean; override;
  end;

implementation

uses
  Data.DBXClientResStrs;

{TDSHTTP}

constructor TDSHTTP.Create;
begin
  Create(nil);
end;

constructor TDSHTTP.Create(const AIPImplementationID: string);
begin
  Create(nil, AIPImplementationID);
end;

function TDSHTTP.Delete(AURL: string): string;
var
  LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    FPeer.DoRequestDelete(AURL, nil, LStream, []);
    LStream.Position := 0;
    // This is here instead of a TStringStream for .net conversions?
    Result := IPProcs(IPImplementationID).ReadStringFromStream(LStream, -1);//, ContentTypeToEncoding(Response.ContentType));
  finally
    FreeAndNil(LStream);
  end;
end;

class constructor TDSHTTP.Create;
begin
  FProtocols := TDictionary<string, TDSHTTPClass>.Create;
end;

constructor TDSHTTP.Create(AOwner: TComponent);
begin
  Create(AOwner, '');
end;

constructor TDSHTTP.Create(AOwner: TComponent; const AIPImplementationID: string);
begin
  inherited Create(AOwner);
  FIPImplementationID := AIPImplementationID;
  FPeer := PeerFactory.CreatePeer(AIPImplementationID, IIPHTTP, nil) as IIPHTTP;
end;

function TDSHTTP.Delete(AURL: string; AResponseStream: TStream): string;
begin
  FPeer.DoRequestDelete(AURL, nil, AResponseStream, []);
end;

destructor TDSHTTP.Destroy;
begin
  FPeer := nil;
  inherited;
end;

class destructor TDSHTTP.Destroy;
begin
  FreeAndNil(FProtocols);
end;

procedure TDSHTTP.Disconnect;
begin
  FPeer.Disconnect;
end;

function TDSHTTP.Get(AURL: string; AResponseContent: TStream): string;
begin
  Result := FPeer.DoGet(AURL, AResponseContent);
end;

function TDSHTTP.Get(AURL: string): string;
begin
  Result := FPeer.DoGet(AURL);
end;

function TDSHTTP.GetHTTPOptions: TIPHTTPOptionsPeer;
begin
  Result := FPeer.HTTPOptions;
end;

function TDSHTTP.GetProtocol: string;
begin
  Result := FPeer.Protocol;
end;

function TDSHTTP.GetProxyParams: IIPProxyConnectionInfo;
begin
  Result := FPeer.ProxyParams;
end;

function TDSHTTP.GetRequest: IIPHTTPRequest;
begin
  Result := FPeer.Request;
end;

function TDSHTTP.GetResponse: IIPHTTPResponse;
begin
  Result := FPeer.Response;
end;

function TDSHTTP.GetResponseCode: Integer;
begin
  Result := FPeer.ResponseCode;
end;

function TDSHTTP.GetURL: IIPURI;
begin
  Result := FPeer.URL;
end;

class function TDSHTTP.HasPeerCertificate: Boolean;
begin
  Result := False;
end;

procedure TDSHTTP.Head(AURL: string);
begin
  FPeer.DoRequestHead(AURL, nil, nil, []);
end;

procedure TDSHTTP.Merge(AURL: string; RequestStream: TStream);
begin
  FPeer.DoRequestMethod('MERGE', AURL, RequestStream, nil, []);
end;

function TDSHTTP.Put(AURL: string): string;
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

class procedure TDSHTTP.RegisterProtocol(const AName: string;
  AClass: TDSHTTPClass);
begin
  FProtocols.Add(LowerCase(AName), AClass);
end;

procedure TDSHTTP.SetAuthentication(auth: IIPAuthentication);
begin
  FAuthentication := auth;
  FPeer.Request.Authentication := auth;
end;

procedure TDSHTTP.SetBasicAuthentication(const user, password: string);
begin
  FAuthentication := PeerFactory.CreatePeer(FIPImplementationID, IIPBasicAuthentication) as IIPBasicAuthentication;
  FAuthentication.Password := password;
  FAuthentication.Username := user;
  FPeer.Request.Authentication := FAuthentication;
end;

procedure TDSHTTP.SetHTTPOptions(options: TIPHTTPOptionsPeer);
begin
  FPeer.HTTPOptions := options;
end;

class procedure TDSHTTP.UnregisterProtocol(const AName: string);
begin
  if FProtocols <> nil then
    FProtocols.Remove(LowerCase(AName));
end;

class function TDSHTTP.RegisteredProtocolList: TArray<string>;
var
  List: TArray<string>;
  LName: string;
  I: Integer;
begin
  SetLength(List, FProtocols.Count);
  I := 0;
  for LName in FProtocols.Keys do
  begin
    List[I] := LName;
    Inc(I);
  end;
  Result := List;
end;

procedure TDSHTTP.Post(AURL: string; ASource, AResponseContent: TStream);
begin
  FPeer.DoPost(AURL, ASource, AResponseContent);
end;

function TDSHTTP.Post(AURL: string; ASource: TStream): string;
begin
  Result := FPeer.DoPost(AURL, ASource);
end;

class function TDSHTTP.ProtocolClass(const AName: string): TDSHTTPClass;
begin
  Result := FProtocols[LowerCase(AName)];
end;

procedure TDSHTTP.Put(AURL: string; ASource, AResponseContent: TStream);
begin
  FPeer.DoPut(AURL, ASource, AResponseContent);
end;

function TDSHTTP.Put(AURL: string; ASource: TStream): string;
begin
  Result := FPeer.DoPut(AURL, ASource);
end;

{ TDSHTTPS }

constructor TDSHTTPS.Create;
begin
  Create(nil, '');
end;

constructor TDSHTTPS.Create(const AIPImplementationID: string);
begin
  Create(nil, AIPImplementationID);
end;

constructor TDSHTTPS.Create(AOwner: TComponent);
begin
  Create(AOwner, '');
end;

constructor TDSHTTPS.Create(AOwner: TComponent;
  const AIPImplementationID: string);
var
  LIOHandler: IIPSSLIOHandlerSocketOpenSSL;
begin
  inherited Create(AOwner, AIPImplementationID);
  FPeer.Protocol := 'https';
  LIOHandler := PeerFactory.CreatePeer(AIPImplementationID, IIPSSLIOHandlerSocketOpenSSL, Self) as IIPSSLIOHandlerSocketOpenSSL; //TIdSSLIOHandlerSocketOpenSSL.Create(Self);
  LIOHandler.OnVerifyPeer := IdValidateCertificate;
  LIOHandler.SSLOptions.VerifyMode := [TIPSSLVerifyModePeer.sslvrfClientOnce];
  FPeer.IOHandler := LIOHandler;
end;

class function TDSHTTPS.HasPeerCertificate: Boolean;
begin
  Result := True;
end;

function TDSHTTPS.IdValidateCertificate(Certificate: IIPX509; AOk: Boolean; ADepth, AError: Integer): Boolean;
var
  Cert: TX509Certificate;
begin
  Result := true;
  if Assigned(FOnValidateCertificate) then
  begin
    Cert := TX509CertificateIndy.Create(Certificate);
    try
      // prepare the TX509Ceritifcate and invoke user method
      FOnValidateCertificate(Self, Cert, ADepth, Result);
    finally
      Cert.Free;
    end;
  end;
end;

initialization
  TDSHTTP.RegisterProtocol('http', TDSHTTP);
  TDSHTTPS.RegisterProtocol('https', TDSHTTPS);
finalization
  TDSHTTP.UnregisterProtocol('http');
  TDSHTTPS.UnregisterProtocol('https');

end.
