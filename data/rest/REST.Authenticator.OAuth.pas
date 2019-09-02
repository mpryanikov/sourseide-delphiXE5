{*******************************************************}
{                                                       }
{             Delphi REST Client Framework              }
{                                                       }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}
unit REST.Authenticator.OAuth;

interface

uses
  System.Classes,
  Data.Bind.ObjectScope, Data.Bind.Components,
  REST.Client,
  REST.Exception,
  REST.Types,
  REST.Consts,
  REST.Utils,
  REST.BindSource;

{$SCOPEDENUMS ON}

type
  TOAuth1SignatureMethod = class;
  TOAuth1Authenticator = class;
  TOAuth2Authenticator = class;

  TOAuth1SignatureMethod = class(TPersistent)
  public
    class function GetName: string; virtual; abstract;
    function BuildSignature(ARequest: TCustomRESTRequest; AAuthenticator: TOAuth1Authenticator): string;
      virtual; abstract;
    property Name: string read GetName;
  end;

  TOAuth1SignatureMethodClass = class of TOAuth1SignatureMethod;

  TOAuth1SignatureMethod_PLAINTEXT = class(TOAuth1SignatureMethod)
  public
    class function GetName: string; override;
    function BuildSignature(ARequest: TCustomRESTRequest; AAuthenticator: TOAuth1Authenticator): string; override;
  end;

  TOAuth1SignatureMethod_HMAC_SHA1 = class(TOAuth1SignatureMethod)
  protected
    function Hash_HMAC_SHA1(const AData, AKey: string): string; virtual;
  public
    class function GetName: string; override;
    function BuildSignature(ARequest: TCustomRESTRequest; AAuthenticator: TOAuth1Authenticator): string; override;
  end;

  TSubOAuth1AuthenticationBindSource = class;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice or pidAndroid)]
  TOAuth1Authenticator = class(TCustomAuthenticator)
  private
    FNonce: string;
    FSigningClass: TOAuth1SignatureMethod;
    FSigningClassName: string;
    FBindSource: TSubOAuth1AuthenticationBindSource;
    FTimestamp: string;
    FVersion: string;
    FAccessToken: string;
    FAccessTokenSecret: string;
    FAccessTokenEndpoint: string;
    FRequestTokenEndpoint: string;
    FAuthenticationEndpoint: string;
    FCallbackEndpoint: string;
    FConsumerKey: string;
    FConsumerSecret: string;
    FVerifierPIN: string;
    FRequestToken: string;
    FRequestTokenSecret: string;
    function GetAccessToken: string;
    function GetAccessTokenSecret: string;
    function GetCallback: string;
    function GetConsumerKey: string;
    function GetConsumerSecret: string;
    function GetNonce: string;
    function GetTimestamp: string;
    procedure SetSigningClassName(const AValue: string);
    procedure SetSigningClass(const AValue: TOAuth1SignatureMethod);
    procedure SetAccessToken(const AValue: string);
    procedure SetAccessTokenEndpoint(const AValue: string);
    procedure SetAccessTokenSecret(const AValue: string);
    procedure SetAuthenticationEndpoint(const AValue: string);
    procedure SetCallbackEndpoint(const AValue: string);
    procedure SetConsumerSecret(const AValue: string);
    procedure SetConsumerKey(const AValue: string);
    procedure SetRequestToken(const AValue: string);
    procedure SetRequestTokenEndpoint(const AValue: string);
    procedure SetRequestTokenSecret(const AValue: string);
    procedure SetVerifierPIN(const AValue: string);
    function SigningClassNameIsStored: Boolean;
  protected
    function CreateBindSource: TBaseObjectBindSource; override;
    procedure DoAuthenticate(ARequest: TCustomRESTRequest); override;
    /// <summary>
    /// creates a 32 characters long unique string that will be used internally to identify a request
    /// </summary>
    class function GenerateNonce: string; virtual;
    /// <summary>
    /// creates the signature for OAuth1.0 requests
    /// </summary>
    function GenerateSignature(ARequest: TCustomRESTRequest): string; virtual;
    class function GenerateTimeStamp: string; virtual;
    function GetSignatureMethod: string; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(ASource: TOAuth1Authenticator); reintroduce;
    /// <summary>
    /// Resets all values of the authenticator to default values - empty strings in most cases.
    /// </summary>
    procedure ResetToDefaults; override;
    /// <summary>
    /// A random, unique string for each request. Automatically maintained by the authenticator-class.
    /// </summary>
    property Nonce: string read GetNonce;
    property SignatureMethod: string read GetSignatureMethod;
    /// <summary>
    /// Timestamp of the current request. Automatically maintained by the authenticator-class.
    /// </summary>
    property Timestamp: string read GetTimestamp;
    /// <summary>
    /// Version of OAuth that this Authenticator supports (is always 1.0(a))
    /// </summary>
    property Version: string read FVersion;

    property SigningClass: TOAuth1SignatureMethod read FSigningClass write SetSigningClass;
  published
    /// <summary>
    /// the access-token provided by the service-provider. this value is used to sign the requests
    /// </summary>
    property AccessToken: string read GetAccessToken write SetAccessToken;
    /// <summary>
    /// the access-token-secret provided by the service-provider. this value is used to sign the requests
    /// </summary>
    property AccessTokenSecret: string read GetAccessTokenSecret write SetAccessTokenSecret;
    /// <summary>
    /// the request-token provided by the service-provider. a request-token is always
    /// a temporary token and must be changed to an access-token. after successfully
    /// requesting the access-token, the request-token becomes invalid.
    /// </summary>
    property RequestToken: string read FRequestToken write SetRequestToken;
    property RequestTokenSecret: string read FRequestTokenSecret write SetRequestTokenSecret;

    /// <summary>
    /// Complete address to the endpoint of the service-provider where an access-token can be obtained
    /// </summary>
    property AccessTokenEndpoint: string read FAccessTokenEndpoint write SetAccessTokenEndpoint;
    /// <summary>
    /// Complete address to the endpoint of the service-provider where an request-token can be obtained
    /// </summary>
    property RequestTokenEndpoint: string read FRequestTokenEndpoint write SetRequestTokenEndpoint;
    /// <summary>
    /// Complete address to the endpoint of the service-provider where the authentication can be done
    /// </summary>
    property AuthenticationEndpoint: string read FAuthenticationEndpoint write SetAuthenticationEndpoint;
    /// <summary>
    /// Complete address to the endpoint for redirecting the user to after authentication
    /// </summary>
    property CallbackEndpoint: string read GetCallback write SetCallbackEndpoint;
    /// <summary>
    /// The consumer-key (or "client-id" or "app-id") is provided by the service-provider after registering an application
    /// </summary>
    property ConsumerKey: string read GetConsumerKey write SetConsumerKey;
    /// <summary>
    /// The consumer-secret (or client-secret or app-secret) is provided by the service-provider after registering an application. DO NOT SHARE THIS VALUE.
    /// </summary>
    property ConsumerSecrect: string read GetConsumerSecret write SetConsumerSecret;
    property SigningClassName: string read FSigningClassName write SetSigningClassName stored SigningClassNameIsStored;
    /// <summary>
    /// The verifier ("pin", "auth-code") is used to change a request-token into an access-token and is provided by the service-provider.
    /// after changing the tokens, this value becomes invalid.
    /// </summary>
    property VerifierPIN: string read FVerifierPIN write SetVerifierPIN;

    property BindSource: TSubOAuth1AuthenticationBindSource read FBindSource;
  end;

  /// <summary>
  /// LiveBindings bindsource for TOAuth1Authenticator. Publishes subcomponent properties.
  /// </summary>
  TSubOAuth1AuthenticationBindSource = class(TRESTAuthenticatorBindSource<TOAuth1Authenticator>)
  protected
    function CreateAdapterT: TRESTAuthenticatorAdapter<TOAuth1Authenticator>; override;
  end;

  /// <summary>
  /// LiveBindings adapter for TOAuth1Authenticator. Create bindable members.
  /// </summary>
  TOAuth1AuthenticatorAdapter = class(TRESTAuthenticatorAdapter<TOAuth1Authenticator>)
  protected
    procedure AddFields; override;
  end;

  TOAuth2ResponseType = (
    /// <summary>
    /// Default workflow including the authentication of the client
    /// </summary>
    rtCODE,
    /// <summary>
    /// Implicit workflow for direct requesting an accesstoken
    /// </summary>
    rtTOKEN);

  TOAuth2TokenType = (ttNONE, ttBEARER);

  TOAuth2Exception = class(ERESTException);

  TSubOAuth2AuthenticationBindSource = class;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice or pidAndroid)]
  TOAuth2Authenticator = class(TCustomAuthenticator)
  private
    FBindSource: TSubOAuth2AuthenticationBindSource;
    FAccessToken: string;
    FAccessTokenEndpoint: string;
    FAccessTokenExpiry: TDateTime;
    FAccessTokenParamName: string;
    FAuthCode: string;
    FAuthorizationEndpoint: string;
    FClientID: string;
    FClientSecret: string;
    FLocalState: string;
    FRedirectionEndpoint: string;
    FRefreshToken: string;
    FResponseType: TOAuth2ResponseType;
    FScope: string;
    FTokenType: TOAuth2TokenType;
    procedure SetAccessTokenEndpoint(const AValue: string);
    procedure SetAccessTokenParamName(const AValue: string);
    procedure SetAuthCode(const AValue: string);
    procedure SetAuthorizationEndpoint(const AValue: string);
    procedure SetClientID(const AValue: string);
    procedure SetClientSecret(const AValue: string);
    procedure SetLocalState(const AValue: string);
    procedure SetRedirectionEndpoint(const AValue: string);
    procedure SetRefreshToken(const AValue: string);
    procedure SetResponseType(const AValue: TOAuth2ResponseType);
    procedure SetScope(const AValue: string);
    function ResponseTypeIsStored: Boolean;
    function TokenTypeIsStored: Boolean;
    function AccessTokenParamNameIsStored: Boolean;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoAuthenticate(ARequest: TCustomRESTRequest); override;
    procedure ReadAccessTokenExpiryData(AReader: TReader); virtual;
    procedure SetAccessToken(const AValue: string); virtual;
    procedure SetAccessTokenExpiry(const AExpiry: TDateTime); virtual;
    procedure SetTokenType(const AType: TOAuth2TokenType); virtual;
    procedure WriteAccessTokenExpiryData(AWriter: TWriter); virtual;
    function CreateBindSource: TBaseObjectBindSource; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(ASource: TOAuth2Authenticator); reintroduce;
    function AuthorizationRequestURI: string; virtual;
    procedure ChangeAuthCodeToAccesToken; virtual;
    /// <summary>
    /// Resets a request to default values and clears all entries.
    /// </summary>
    procedure ResetToDefaults; override;
  published
    property AccessToken: string read FAccessToken write SetAccessToken;
    property AccessTokenEndpoint: string read FAccessTokenEndpoint write SetAccessTokenEndpoint;
    property AccessTokenExpiry: TDateTime read FAccessTokenExpiry write SetAccessTokenExpiry;
    property AccessTokenParamName: string read FAccessTokenParamName write SetAccessTokenParamName stored AccessTokenParamNameIsStored;
    property AuthCode: string read FAuthCode write SetAuthCode;
    property AuthorizationEndpoint: string read FAuthorizationEndpoint write SetAuthorizationEndpoint;
    property ClientID: string read FClientID write SetClientID;
    property ClientSecret: string read FClientSecret write SetClientSecret;
    property LocalState: string read FLocalState write SetLocalState;
    property RedirectionEndpoint: string read FRedirectionEndpoint write SetRedirectionEndpoint;
    property RefreshToken: string read FRefreshToken write SetRefreshToken;
    property ResponseType: TOAuth2ResponseType read FResponseType write SetResponseType stored ResponseTypeIsStored;
    property Scope: string read FScope write SetScope;
    property TokenType: TOAuth2TokenType read FTokenType write SetTokenType stored TokenTypeIsStored;

    property BindSource: TSubOAuth2AuthenticationBindSource read FBindSource;
  end;

  /// <summary>
  /// LiveBindings bindsource for TOAuth2Authenticator. Publishes subcomponent properties.
  /// </summary>
  TSubOAuth2AuthenticationBindSource = class(TRESTAuthenticatorBindSource<TOAuth2Authenticator>)
  protected
    function CreateAdapterT: TRESTAuthenticatorAdapter<TOAuth2Authenticator>; override;
  end;

  /// <summary>
  /// LiveBindings adapter for TOAuth2Authenticator. Create bindable members.
  /// </summary>
  TOAuth2AuthenticatorAdapter = class(TRESTAuthenticatorAdapter<TOAuth2Authenticator>)
  protected
    procedure AddFields; override;
  end;

function OAuth2ResponseTypeToString(const AType: TOAuth2ResponseType): string;
function OAuth2ResponseTypeFromString(const ATypeString: string): TOAuth2ResponseType;

function OAuth2TokenTypeToString(const AType: TOAuth2TokenType): string;
function OAuth2TokenTypeFromString(const ATypeString: string): TOAuth2TokenType;

var
  DefaultOAuth1SignatureClass: TOAuth1SignatureMethodClass = TOAuth1SignatureMethod_HMAC_SHA1;
  DefaultOAuth2ResponseType: TOAuth2ResponseType = TOAuth2ResponseType.rtCODE;
  DefaultOAuth2TokenType: TOAuth2TokenType = TOAuth2TokenType.ttNONE;
  DefaultOAuth2AccessTokenParamName: string = 'access_token'; // do not localize

implementation

uses
  System.SysUtils, System.DateUtils,
  REST.HttpClient;

function OAuth2ResponseTypeToString(const AType: TOAuth2ResponseType): string;
begin
  case AType of
    TOAuth2ResponseType.rtCODE:
      result := 'code'; // do not localize
    TOAuth2ResponseType.rtTOKEN:
      result := 'token'; // do not localize
  else
    result := ''; // do not localize
  end;
end;

function OAuth2ResponseTypeFromString(const ATypeString: string): TOAuth2ResponseType;
var
  LType: TOAuth2ResponseType;
begin
  result := DefaultOAuth2ResponseType;

  for LType IN [Low(TOAuth2ResponseType) .. High(TOAuth2ResponseType)] do
  begin
    if SameText(ATypeString, OAuth2ResponseTypeToString(LType)) then
    begin
      result := LType;
      BREAK;
    end;
  end;
end;

function OAuth2TokenTypeToString(const AType: TOAuth2TokenType): string;
begin
  case AType of
    TOAuth2TokenType.ttBEARER:
      result := 'bearer'; // do not localize
  else
    result := ''; // do not localize
  end;
end;

function OAuth2TokenTypeFromString(const ATypeString: string): TOAuth2TokenType;
var
  LType: TOAuth2TokenType;
begin
  result := DefaultOAuth2TokenType;

  for LType IN [Low(TOAuth2TokenType) .. High(TOAuth2TokenType)] do
  begin
    if SameText(ATypeString, OAuth2TokenTypeToString(LType)) then
    begin
      result := LType;
      BREAK;
    end;
  end;
end;

{ TOAuth1SignatureMethod_PLAINTEXT }

function TOAuth1SignatureMethod_PLAINTEXT.BuildSignature(ARequest: TCustomRESTRequest;
  AAuthenticator: TOAuth1Authenticator): string;
begin
  (*
    oauth_signature is set to the concatenated encoded values
    of the Consumer Secret and Token Secret, separated by a ‘&’
    character (ASCII code 38), even if either secret is empty.
    The result MUST be encoded again.
  *)
  result := URIEncode(AAuthenticator.ConsumerSecrect + #38 + AAuthenticator.AccessTokenSecret);
end;

class function TOAuth1SignatureMethod_PLAINTEXT.GetName: string;
begin
  result := 'PLAINTEXT'; // do not localize
end;

{ TOAuthSignatureMethod_HMAC_SHA1 }

function TOAuth1SignatureMethod_HMAC_SHA1.BuildSignature(ARequest: TCustomRESTRequest;
  AAuthenticator: TOAuth1Authenticator): string;
var
  LPayLoadParams: TRESTRequestParameterArray;
  LParamList: TStringList;
  LURL: string;
  LParamsStr: string;
  LSigBaseStr: string;
  LSigningKey: string;
  LParam: TRESTRequestParameter;
  i: integer;
begin
  Assert(Assigned(ARequest) AND Assigned(AAuthenticator));

  result := '';

  /// Step #1 - collect all relevant parameters, this includes
  /// all oauth_params as well as the params from the payload
  /// (payload-params ==> params from client and request)
  LParamList := TStringList.Create;
  LParamList.Add('oauth_signature_method=' + self.Name); // do not localize
  if (AAuthenticator.CallbackEndpoint <> '') then
    LParamList.Add('oauth_callback=' + AAuthenticator.CallbackEndpoint); // do not localize
  if (AAuthenticator.ConsumerKey <> '') then
    LParamList.Add('oauth_consumer_key=' + AAuthenticator.ConsumerKey); // do not localize
  LParamList.Add('oauth_nonce=' + AAuthenticator.Nonce); // do not localize
  LParamList.Add('oauth_timestamp=' + AAuthenticator.Timestamp); // do not localize
  if (AAuthenticator.AccessToken <> '') then
    LParamList.Add('oauth_token=' + AAuthenticator.AccessToken); // do not localize
  LParamList.Add('oauth_version=' + AAuthenticator.Version); // do not localize

  /// now collect the parameters from the payload. we do need the
  /// union of the client-parameters and the request-parameters
  LPayLoadParams := ARequest.CreateUnionParameterList;
  TRY
    for LParam IN LPayLoadParams do
    begin
      if (LParam.Kind = TRESTRequestParameterKind.pkGETorPOST) then
        LParamList.Add(LParam.Name + '=' + LParam.Value);
    end;
  FINALLY
    Finalize(LPayLoadParams);
  END;

  /// OAuth-spec requires the parameters to be sorted by their name
  LParamList.Sort;

  /// Step #2 - build a single string from the params
  LParamsStr := '';
  for i := 0 to LParamList.Count - 1 do
  begin
    LParamsStr := LParamsStr + URIEncode(LParamList.Names[i]) + '=' + URIEncode(LParamList.ValueFromIndex[i]);
    if (i < LParamList.Count - 1) then
      LParamsStr := LParamsStr + '&';
  end;
  LParamList.Free;

  /// as per oauth-spec we do need the full URL without (!) any query-params
  LURL := ARequest.GetFullRequestURL(FALSE);

  /// Step #3 - build the SignatureBaseString, the LSigningKey and the Signature
  LSigBaseStr := UpperCase(RESTRequestMethodToString(ARequest.Method)) + '&' + URIEncode(LURL) + '&' +
    URIEncode(LParamsStr); // do not localize

  LSigningKey := AAuthenticator.ConsumerSecrect + '&' + AAuthenticator.AccessTokenSecret; // do not localize

  result := Hash_HMAC_SHA1(LSigBaseStr, LSigningKey);
end;

class function TOAuth1SignatureMethod_HMAC_SHA1.GetName: string;
begin
  result := 'HMAC-SHA1'; // do not localize
end;

function TOAuth1SignatureMethod_HMAC_SHA1.Hash_HMAC_SHA1(const AData, AKey: string): string;
var
  LClient: TRESTHttp;
begin
  LClient := TRESTHttp.Create;
  TRY
    result := LClient.HashHMACSHA1(AData, AKey)
  FINALLY
    FreeAndNil(LClient);
  END;
end;

procedure TOAuth1Authenticator.Assign(ASource: TOAuth1Authenticator);
begin
  ResetToDefaults;

  self.AccessTokenEndpoint := ASource.AccessTokenEndpoint;
  self.RequestTokenEndpoint := ASource.RequestTokenEndpoint;
  self.AuthenticationEndpoint := ASource.AuthenticationEndpoint;
  self.CallbackEndpoint := ASource.CallbackEndpoint;

  self.AccessToken := ASource.AccessToken;
  self.AccessTokenSecret := ASource.AccessTokenSecret;
  self.RequestToken := ASource.RequestToken;
  self.RequestTokenSecret := ASource.RequestTokenSecret;

  self.VerifierPIN := ASource.VerifierPIN;

  self.ConsumerKey := ASource.ConsumerKey;
  self.ConsumerSecrect := ASource.ConsumerSecrect;
  self.SigningClassName := ASource.SigningClassName;
end;

constructor TOAuth1Authenticator.Create(AOwner: TComponent);
begin
  inherited;
  ResetToDefaults;
  FVersion := '1.0';
end;

function TOAuth1Authenticator.CreateBindSource: TBaseObjectBindSource;
begin
  FBindSource := TSubOAuth1AuthenticationBindSource.Create(self);
  FBindSource.Name := 'BindSource'; { Do not localize }
  FBindSource.SetSubComponent(True);
  FBindSource.Authenticator := self;

  result := FBindSource;
end;

destructor TOAuth1Authenticator.Destroy;
begin
  if Assigned(FSigningClass) then
  begin
    FreeAndNil(FSigningClass);
  end;

  inherited;
end;

{ TOAuth1Authenticator }

procedure TOAuth1Authenticator.DoAuthenticate(ARequest: TCustomRESTRequest);
var
  LToken: string;
begin
  inherited;

  /// update timestamp and nonce for this request
  /// --> these values must not change while a request is running
  FNonce := GenerateNonce;
  FTimestamp := GenerateTimeStamp;

  LToken := 'OAuth '; // do not localize, trailing space IS important

  if (CallbackEndpoint <> '') then
    LToken := LToken + Format('%s="%s"', ['oauth_callback', URIEncode(CallbackEndpoint)]) + ', ';
  if (ConsumerKey <> '') then
    LToken := LToken + Format('%s="%s"', ['oauth_consumer_key', URIEncode(ConsumerKey)]) + ', ';
  LToken := LToken + Format('%s="%s"', ['oauth_nonce', URIEncode(Nonce)]) + ', ';
  LToken := LToken + Format('%s="%s"', ['oauth_signature', URIEncode(GenerateSignature(ARequest))]) + ', ';
  LToken := LToken + Format('%s="%s"', ['oauth_signature_method', URIEncode(SignatureMethod)]) + ', ';
  LToken := LToken + Format('%s="%s"', ['oauth_timestamp', URIEncode(Timestamp)]) + ', ';

  /// if we have an access-token, we use it. only if we do not have
  /// an access-token, we check, if we have an request-token. if yes,
  /// we use this one as access-token (see oauth1-workflow)
  if (AccessToken <> '') then
    LToken := LToken + Format('%s="%s"', ['oauth_token', URIEncode(AccessToken)]) + ', '
  else if (RequestToken <> '') then
    LToken := LToken + Format('%s="%s"', ['oauth_token', URIEncode(RequestToken)]) + ', ';

  LToken := LToken + Format('%s="%s"', ['oauth_version', URIEncode(Version)]);

  ARequest.AddAuthParameter(HTTP_HEADERFIELD_AUTH, LToken, TRESTRequestParameterKind.pkHTTPHEADER,
    [TRESTRequestParameterOption.poDoNotEncode]);
end;

class function TOAuth1Authenticator.GenerateNonce: string;
var
  LClient: TRESTHttp;
begin
  LClient := TRESTHttp.Create;
  TRY
    result := LClient.HashStringAsHex(GenerateTimeStamp + IntToStr(Random(MAXINT)));
  FINALLY
    FreeAndNil(LClient);
  END;
end;

function TOAuth1Authenticator.GenerateSignature(ARequest: TCustomRESTRequest): string;
begin
  Assert(Assigned(FSigningClass));

  result := FSigningClass.BuildSignature(ARequest, self);
end;

class function TOAuth1Authenticator.GenerateTimeStamp: string;
begin
  result := IntToStr(DateTimeToUnix(Now));
end;

function TOAuth1Authenticator.GetAccessToken: string;
begin
  result := FAccessToken;
end;

function TOAuth1Authenticator.GetAccessTokenSecret: string;
begin
  result := FAccessTokenSecret;
end;

function TOAuth1Authenticator.GetCallback: string;
begin
  result := FCallbackEndpoint;
end;

function TOAuth1Authenticator.GetConsumerKey: string;
begin
  result := FConsumerKey;
end;

function TOAuth1Authenticator.GetConsumerSecret: string;
begin
  result := FConsumerSecret;
end;

function TOAuth1Authenticator.GetNonce: string;
begin
  result := FNonce;
end;

function TOAuth1Authenticator.GetSignatureMethod: string;
begin
  result := FSigningClass.Name;
end;

function TOAuth1Authenticator.GetTimestamp: string;
begin
  result := FTimestamp;
end;

procedure TOAuth1Authenticator.ResetToDefaults;
begin
  inherited;

  if Assigned(FSigningClass) then
    FreeAndNil(FSigningClass);

  FSigningClassName := DefaultOAuth1SignatureClass.ClassName;
  FSigningClass := DefaultOAuth1SignatureClass.Create;

  FAccessTokenEndpoint := '';
  FRequestTokenEndpoint := '';
  FAuthenticationEndpoint := '';
  FCallbackEndpoint := '';

  FAccessToken := '';
  FAccessTokenSecret := '';
  FRequestToken := '';
  FRequestTokenSecret := '';
  FVerifierPIN := '';

  FConsumerKey := '';
  FConsumerSecret := '';

  FNonce := GenerateNonce;
  FTimestamp := GenerateTimeStamp;
end;

procedure TOAuth1Authenticator.SetAccessToken(const AValue: string);
begin
  if (AValue <> FAccessToken) then
  begin
    FAccessToken := AValue;
    PropertyValueChanged;
  end;
end;

procedure TOAuth1Authenticator.SetAccessTokenEndpoint(const AValue: string);
begin
  if (AValue <> FAccessTokenEndpoint) then
  begin
    FAccessTokenEndpoint := AValue;
    PropertyValueChanged;
  end;
end;

procedure TOAuth1Authenticator.SetAccessTokenSecret(const AValue: string);
begin
  if (AValue <> FAccessTokenSecret) then
  begin
    FAccessTokenSecret := AValue;
    PropertyValueChanged;
  end;
end;

procedure TOAuth1Authenticator.SetAuthenticationEndpoint(const AValue: string);
begin
  if (AValue <> FAuthenticationEndpoint) then
  begin
    FAuthenticationEndpoint := AValue;
    PropertyValueChanged;
  end;
end;

procedure TOAuth1Authenticator.SetCallbackEndpoint(const AValue: string);
begin
  if (AValue <> FCallbackEndpoint) then
  begin
    FCallbackEndpoint := AValue;
    PropertyValueChanged;
  end;
end;

procedure TOAuth1Authenticator.SetConsumerSecret(const AValue: string);
begin
  if (AValue <> FConsumerSecret) then
  begin
    FConsumerSecret := AValue;
    PropertyValueChanged;
  end;
end;

procedure TOAuth1Authenticator.SetConsumerKey(const AValue: string);
begin
  if (AValue <> FConsumerKey) then
  begin
    FConsumerKey := AValue;
    PropertyValueChanged;
  end;
end;

procedure TOAuth1Authenticator.SetRequestToken(const AValue: string);
begin
  if (AValue <> FRequestToken) then
  begin
    FRequestToken := AValue;
    PropertyValueChanged;
  end;
end;

procedure TOAuth1Authenticator.SetRequestTokenEndpoint(const AValue: string);
begin
  if (AValue <> FRequestTokenEndpoint) then
  begin
    FRequestTokenEndpoint := AValue;
    PropertyValueChanged;
  end;
end;

procedure TOAuth1Authenticator.SetRequestTokenSecret(const AValue: string);
begin
  if (AValue <> FRequestTokenSecret) then
  begin
    FRequestTokenSecret := AValue;
    PropertyValueChanged;
  end;
end;

procedure TOAuth1Authenticator.SetSigningClass(const AValue: TOAuth1SignatureMethod);
begin
  if (AValue <> FSigningClass) then
  begin
    if Assigned(FSigningClass) then
      FreeAndNil(FSigningClass);

    FSigningClass := AValue;

    if Assigned(FSigningClass) then
      FSigningClassName := FSigningClass.ClassName
    else
      FSigningClassName := '';
  end;
end;

procedure TOAuth1Authenticator.SetSigningClassName(const AValue: string);
var
  LFinder: TClassFinder;
  LSignClass: TPersistentClass;
begin
  if (AValue <> FSigningClassName) then
  begin
    FSigningClassName := AValue;

    /// we want to allow "PLAINTEXT" or "HMAC-SHA1" as names and
    /// translate them into the correct classnames
    if SameText(FSigningClassName, TOAuth1SignatureMethod_PLAINTEXT.GetName) then
      FSigningClassName := TOAuth1SignatureMethod_PLAINTEXT.ClassName
    else if SameText(FSigningClassName, TOAuth1SignatureMethod_HMAC_SHA1.GetName) then
      FSigningClassName := TOAuth1SignatureMethod_HMAC_SHA1.ClassName;

    if Assigned(FSigningClass) then
      FreeAndNil(FSigningClass);

    LFinder := TClassFinder.Create;
    TRY
      LSignClass := LFinder.GetClass(FSigningClassName);
      if Assigned(LSignClass) then
        FSigningClass := TOAuth1SignatureMethod(LSignClass.Create);
    FINALLY
      FreeAndNil(LFinder);
    END;
  end;
end;

procedure TOAuth1Authenticator.SetVerifierPIN(const AValue: string);
begin
  if (AValue <> FVerifierPIN) then
  begin
    FVerifierPIN := AValue;
    PropertyValueChanged;
  end;
end;

function TOAuth1Authenticator.SigningClassNameIsStored: Boolean;
begin
  Result := SigningClassName <> DefaultOAuth1SignatureClass.ClassName;
end;

{ TOAuth2Authenticator }

constructor TOAuth2Authenticator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ResetToDefaults;
end;

function TOAuth2Authenticator.CreateBindSource: TBaseObjectBindSource;
begin
  FBindSource := TSubOAuth2AuthenticationBindSource.Create(self);
  FBindSource.Name := 'BindSource'; { Do not localize }
  FBindSource.SetSubComponent(True);
  FBindSource.Authenticator := self;

  result := FBindSource;
end;

function TOAuth2Authenticator.AccessTokenParamNameIsStored: Boolean;
begin
  Result := AccessTokenParamName <> DefaultOAuth2AccessTokenParamName;
end;

procedure TOAuth2Authenticator.Assign(ASource: TOAuth2Authenticator);
begin
  ResetToDefaults;

  self.ClientID := ASource.ClientID;
  self.ClientSecret := ASource.ClientSecret;
  self.AuthCode := ASource.AuthCode;
  self.AccessToken := ASource.AccessToken;
  self.AccessTokenParamName := ASource.AccessTokenParamName;

  self.AccessTokenExpiry := ASource.AccessTokenExpiry;

  self.Scope := ASource.Scope;
  self.RefreshToken := ASource.RefreshToken;
  self.LocalState := ASource.LocalState;

  self.TokenType := ASource.TokenType;

  self.ResponseType := ASource.ResponseType;
  self.AuthorizationEndpoint := ASource.AuthorizationEndpoint;
  self.AccessTokenEndpoint := ASource.AccessTokenEndpoint;
  self.RedirectionEndpoint := ASource.RedirectionEndpoint;
end;

function TOAuth2Authenticator.AuthorizationRequestURI: string;
begin
  result := FAuthorizationEndpoint;
  result := result + '?response_type=' + URIEncode(OAuth2ResponseTypeToString(FResponseType));
  if (FClientID <> '') then
    result := result + '&client_id=' + URIEncode(FClientID);
  if (FRedirectionEndpoint <> '') then
    result := result + '&redirect_uri=' + URIEncode(FRedirectionEndpoint);
  if (FScope <> '') then
    result := result + '&scope=' + URIEncode(FScope);
end;

procedure TOAuth2Authenticator.ChangeAuthCodeToAccesToken;
var
  LClient: TRestClient;
  LRequest: TRESTRequest;
  LToken: string;
  LIntValue: int64;
begin

  /// we do need an authorization-code here, because we want
  /// to send it to the servce and exchange the code into an
  /// access-token.
  if (FAuthCode = '') then
    raise TOAuth2Exception.Create('An authorization-code is needed before it can be changed into an access-token.');

  try
    LClient := TRestClient.Create(FAccessTokenEndpoint);

    LRequest := TRESTRequest.Create(nil);
    LRequest.Method := TRESTRequestMethod.rmPOST;
    LRequest.Client := LClient;

    LRequest.AddAuthParameter('code', FAuthCode, TRESTRequestParameterKind.pkGETorPOST);
    LRequest.AddAuthParameter('client_id', FClientID, TRESTRequestParameterKind.pkGETorPOST);
    LRequest.AddAuthParameter('client_secret', FClientSecret, TRESTRequestParameterKind.pkGETorPOST);
    LRequest.AddAuthParameter('redirect_uri', FRedirectionEndpoint, TRESTRequestParameterKind.pkGETorPOST);
    LRequest.AddAuthParameter('grant_type', 'authorization_code', TRESTRequestParameterKind.pkGETorPOST);

    LRequest.Execute;

    if LRequest.Response.GetSimpleValue('access_token', LToken) then
      FAccessToken := LToken;
    if LRequest.Response.GetSimpleValue('refresh_token', LToken) then
      FRefreshToken := LToken;

    /// detect token-type. this is important for how using it later
    if LRequest.Response.GetSimpleValue('token_type', LToken) then
      FTokenType := OAuth2TokenTypeFromString(LToken);

    /// if provided by the service, the field "expires_in" contains
    /// the number of seconds an access-token will be valid
    if LRequest.Response.GetSimpleValue('expires_in', LToken) then
    begin
      LIntValue := StrToIntdef(LToken, -1);
      if (LIntValue > -1) then
        FAccessTokenExpiry := IncSecond(Now, LIntValue)
      else
        FAccessTokenExpiry := 0.0;
    end;

    /// an authentication-code may only be used once.
    /// if we succeeded here and got an access-token, then
    /// we do clear the auth-code as is is not valid anymore
    /// and also not needed anymore.
    if (FAccessToken <> '') then
      FAuthCode := '';

  finally
    FreeAndNil(LClient);
    FreeAndNil(LRequest);
  end;

end;

procedure TOAuth2Authenticator.DefineProperties(Filer: TFiler);
begin
  inherited;

  Filer.DefineProperty('AccessTokenExpiryDate', ReadAccessTokenExpiryData, WriteAccessTokenExpiryData,
    (FAccessTokenExpiry > 0.1));
end;

procedure TOAuth2Authenticator.DoAuthenticate(ARequest: TCustomRESTRequest);
var
  LName: string;
begin
  inherited;

  case FTokenType of
    TOAuth2TokenType.ttBEARER:
      begin
        ARequest.AddAuthParameter(HTTP_HEADERFIELD_AUTH, 'Bearer ' + FAccessToken,
          TRESTRequestParameterKind.pkHTTPHEADER, [TRESTRequestParameterOption.poDoNotEncode]);
      end;
  else
    begin
      /// depending on the service-provider, some of them want the access-token
      /// submitted as "access_token", while others require it as "oauth_token"
      /// please see documentation of your service-provider
      LName := FAccessTokenParamName;
      if (Trim(LName) = '') then
        LName := DefaultOAuth2AccessTokenParamName;

      ARequest.AddAuthParameter(LName, FAccessToken, TRESTRequestParameterKind.pkGETorPOST,
        [TRESTRequestParameterOption.poDoNotEncode]);
    end;
  end;
end;

procedure TOAuth2Authenticator.ReadAccessTokenExpiryData(AReader: TReader);
begin
  FAccessTokenExpiry := AReader.ReadDate;
end;

procedure TOAuth2Authenticator.ResetToDefaults;
begin
  inherited;

  ClientID := '';
  ClientSecret := '';
  AuthCode := '';
  AccessToken := '';
  FAccessTokenExpiry := 0.0;
  Scope := '';
  RefreshToken := '';
  LocalState := '';

  FTokenType := DefaultOAuth2TokenType;
  ResponseType := DefaultOAuth2ResponseType;
  AccessTokenParamName := DefaultOAuth2AccessTokenParamName;

  AuthorizationEndpoint := '';
  AccessTokenEndpoint := '';
  RedirectionEndpoint := '';
end;

function TOAuth2Authenticator.ResponseTypeIsStored: Boolean;
begin
  Result := ResponseType <> DefaultOAuth2ResponseType;
end;

procedure TOAuth2Authenticator.SetAccessToken(const AValue: string);
begin
  if (AValue <> FAccessToken) then
  begin
    FAccessToken := AValue;
    PropertyValueChanged;
  end;
end;

procedure TOAuth2Authenticator.SetAccessTokenEndpoint(const AValue: string);
begin
  if (AValue <> FAccessTokenEndpoint) then
  begin
    FAccessTokenEndpoint := AValue;
    PropertyValueChanged;
  end;
end;

procedure TOAuth2Authenticator.SetAccessTokenExpiry(const AExpiry: TDateTime);
begin
  if (AExpiry <> FAccessTokenExpiry) then
  begin
    FAccessTokenExpiry := AExpiry;
    PropertyValueChanged;
  end;
end;

procedure TOAuth2Authenticator.SetAccessTokenParamName(const AValue: string);
begin
  if (AValue <> FAccessTokenParamName) then
  begin
    FAccessTokenParamName := AValue;
    PropertyValueChanged;
  end;
end;

procedure TOAuth2Authenticator.SetAuthCode(const AValue: string);
begin
  if (AValue <> FAuthCode) then
  begin
    FAuthCode := AValue;
    PropertyValueChanged;
  end;
end;

procedure TOAuth2Authenticator.SetAuthorizationEndpoint(const AValue: string);
begin
  if (AValue <> FAuthorizationEndpoint) then
  begin
    FAuthorizationEndpoint := AValue;
    PropertyValueChanged;
  end;
end;

procedure TOAuth2Authenticator.SetClientID(const AValue: string);
begin
  if (AValue <> FClientID) then
  begin
    FClientID := AValue;
    PropertyValueChanged;
  end;
end;

procedure TOAuth2Authenticator.SetClientSecret(const AValue: string);
begin
  if (AValue <> FClientSecret) then
  begin
    FClientSecret := AValue;
    PropertyValueChanged;
  end;
end;

procedure TOAuth2Authenticator.SetLocalState(const AValue: string);
begin
  if (AValue <> FLocalState) then
  begin
    FLocalState := AValue;
    PropertyValueChanged;
  end;
end;

procedure TOAuth2Authenticator.SetRedirectionEndpoint(const AValue: string);
begin
  if (AValue <> FRedirectionEndpoint) then
  begin
    FRedirectionEndpoint := AValue;
    PropertyValueChanged;
  end;
end;

procedure TOAuth2Authenticator.SetRefreshToken(const AValue: string);
begin
  if (AValue <> FRefreshToken) then
  begin
    FRefreshToken := AValue;
    PropertyValueChanged;
  end;
end;

procedure TOAuth2Authenticator.SetResponseType(const AValue: TOAuth2ResponseType);
begin
  if (AValue <> FResponseType) then
  begin
    FResponseType := AValue;
    PropertyValueChanged;
  end;
end;

procedure TOAuth2Authenticator.SetScope(const AValue: string);
begin
  if (AValue <> FScope) then
  begin
    FScope := AValue;
    PropertyValueChanged;
  end;
end;

procedure TOAuth2Authenticator.SetTokenType(const AType: TOAuth2TokenType);
begin
  if (AType <> FTokenType) then
  begin
    FTokenType := AType;
    PropertyValueChanged;
  end;
end;

function TOAuth2Authenticator.TokenTypeIsStored: Boolean;
begin
  Result := TokenType <> DefaultOAuth2TokenType;
end;

procedure TOAuth2Authenticator.WriteAccessTokenExpiryData(AWriter: TWriter);
begin
  AWriter.WriteDate(FAccessTokenExpiry);
end;

{ TSubOAuth1AuthenticationBindSource }

function TSubOAuth1AuthenticationBindSource.CreateAdapterT: TRESTAuthenticatorAdapter<TOAuth1Authenticator>;
begin
  result := TOAuth1AuthenticatorAdapter.Create(self);
end;

{ TOAuth1AuthenticatorAdapter }

procedure TOAuth1AuthenticatorAdapter.AddFields;
const
  sAccessToken = 'AccessToken';
  sAccessTokenSecret = 'AccessTokenSecret';
  sAccessTokenEndpoint = 'AccessTokenEndpoint';
  sRequestToken = 'RequestToken';
  sRequestTokenSecret = 'RequestTokenSecret';
  sRequestTokenEndpoint = 'RequestTokenEndpoint';
  sAuthenticationEndpoint = 'AuthenticationEndpoint';
  sCallbackEndpoint = 'CallbackEndpoint';
  sConsumerKey = 'ConsumerKey';
  sConsumerSecret = 'ConsumerSecret';
  sVerifierPIN = 'VerifierPIN';
var
  LGetMemberObject: IGetMemberObject;
begin
  CheckInactive;
  ClearFields;
  if Authenticator <> nil then
  begin
    LGetMemberObject := TBindSourceAdapterGetMemberObject.Create(self);

    CreateReadWriteField<string>(sAccessToken, LGetMemberObject, TScopeMemberType.mtText,
      function: string
      begin
        result := Authenticator.AccessToken;
      end,
      procedure(AValue: string)
      begin
        Authenticator.AccessToken := AValue;
      end);

    CreateReadWriteField<string>(sAccessTokenSecret, LGetMemberObject, TScopeMemberType.mtText,
      function: string
      begin
        result := Authenticator.AccessTokenSecret;
      end,
      procedure(AValue: string)
      begin
        Authenticator.AccessTokenSecret := AValue;
      end);

    CreateReadWriteField<string>(sAccessTokenEndpoint, LGetMemberObject, TScopeMemberType.mtText,
      function: string
      begin
        result := Authenticator.AccessTokenEndpoint;
      end,
      procedure(AValue: string)
      begin
        Authenticator.AccessTokenEndpoint := AValue;
      end);

    CreateReadWriteField<string>(sRequestToken, LGetMemberObject, TScopeMemberType.mtText,
      function: string
      begin
        result := Authenticator.RequestToken;
      end,
      procedure(AValue: string)
      begin
        Authenticator.RequestToken := AValue;
      end);

    CreateReadWriteField<string>(sRequestTokenSecret, LGetMemberObject, TScopeMemberType.mtText,
      function: string
      begin
        result := Authenticator.RequestTokenSecret;
      end,
      procedure(AValue: string)
      begin
        Authenticator.RequestTokenSecret := AValue;
      end);

    CreateReadWriteField<string>(sRequestTokenEndpoint, LGetMemberObject, TScopeMemberType.mtText,
      function: string
      begin
        result := Authenticator.RequestTokenEndpoint;
      end,
      procedure(AValue: string)
      begin
        Authenticator.RequestTokenEndpoint := AValue;
      end);

    CreateReadWriteField<string>(sAuthenticationEndpoint, LGetMemberObject, TScopeMemberType.mtText,
      function: string
      begin
        result := Authenticator.AuthenticationEndpoint;
      end,
      procedure(AValue: string)
      begin
        Authenticator.AuthenticationEndpoint := AValue;
      end);

    CreateReadWriteField<string>(sCallbackEndpoint, LGetMemberObject, TScopeMemberType.mtText,
      function: string
      begin
        result := Authenticator.CallbackEndpoint;
      end,
      procedure(AValue: string)
      begin
        Authenticator.CallbackEndpoint := AValue;
      end);

    CreateReadWriteField<string>(sConsumerKey, LGetMemberObject, TScopeMemberType.mtText,
      function: string
      begin
        result := Authenticator.ConsumerKey;
      end,
      procedure(AValue: string)
      begin
        Authenticator.ConsumerKey := AValue;
      end);

    CreateReadWriteField<string>(sConsumerSecret, LGetMemberObject, TScopeMemberType.mtText,
      function: string
      begin
        result := Authenticator.ConsumerSecrect;
      end,
      procedure(AValue: string)
      begin
        Authenticator.ConsumerSecrect := AValue;
      end);

    CreateReadWriteField<string>(sVerifierPIN, LGetMemberObject, TScopeMemberType.mtText,
      function: string
      begin
        result := Authenticator.VerifierPIN;
      end,
      procedure(AValue: string)
      begin
        Authenticator.VerifierPIN := AValue;
      end);

  end;
end;

{ TSubOAuth2AuthenticationBindSource }

function TSubOAuth2AuthenticationBindSource.CreateAdapterT: TRESTAuthenticatorAdapter<TOAuth2Authenticator>;
begin
  result := TOAuth2AuthenticatorAdapter.Create(self);
end;

{ TOAuth2AuthenticatorAdapter }

procedure TOAuth2AuthenticatorAdapter.AddFields;
const
  sAccessToken = 'AccessToken';
  sAccessTokenEndpoint = 'AccessTokenEndpoint';
  sRefreshToken = 'RefreshToken';
  sAuthCode = 'AuthCode';
  sClientID = 'ClientID';
  sClientSecret = 'ClientSecret';
  sAuthorizationEndpoint = 'AuthorizationEndpoint';
  sRedirectionEndpoint = 'RedirectionEndpoint';
  sScope = 'Scope';
  sLocalState = 'LocalState';
var
  LGetMemberObject: IGetMemberObject;
begin
  CheckInactive;
  ClearFields;
  if Authenticator <> nil then
  begin
    LGetMemberObject := TBindSourceAdapterGetMemberObject.Create(self);

    CreateReadWriteField<string>(sAccessToken, LGetMemberObject, TScopeMemberType.mtText,
      function: string
      begin
        result := Authenticator.AccessToken;
      end,
      procedure(AValue: string)
      begin
        Authenticator.AccessToken := AValue;
      end);

    CreateReadWriteField<string>(sAccessTokenEndpoint, LGetMemberObject, TScopeMemberType.mtText,
      function: string
      begin
        result := Authenticator.AccessTokenEndpoint;
      end,
      procedure(AValue: string)
      begin
        Authenticator.AccessTokenEndpoint := AValue;
      end);

    CreateReadWriteField<string>(sRefreshToken, LGetMemberObject, TScopeMemberType.mtText,
      function: string
      begin
        result := Authenticator.RefreshToken;
      end,
      procedure(AValue: string)
      begin
        Authenticator.RefreshToken := AValue;
      end);

    CreateReadWriteField<string>(sAuthCode, LGetMemberObject, TScopeMemberType.mtText,
      function: string
      begin
        result := Authenticator.AuthCode;
      end,
      procedure(AValue: string)
      begin
        Authenticator.AuthCode := AValue;
      end);

    CreateReadWriteField<string>(sClientID, LGetMemberObject, TScopeMemberType.mtText,
      function: string
      begin
        result := Authenticator.ClientID;
      end,
      procedure(AValue: string)
      begin
        Authenticator.ClientID := AValue;
      end);

    CreateReadWriteField<string>(sClientSecret, LGetMemberObject, TScopeMemberType.mtText,
      function: string
      begin
        result := Authenticator.ClientSecret;
      end,
      procedure(AValue: string)
      begin
        Authenticator.ClientSecret := AValue;
      end);

    CreateReadWriteField<string>(sAuthorizationEndpoint, LGetMemberObject, TScopeMemberType.mtText,
      function: string
      begin
        result := Authenticator.AuthorizationEndpoint;
      end,
      procedure(AValue: string)
      begin
        Authenticator.AuthorizationEndpoint := AValue;
      end);

    CreateReadWriteField<string>(sRedirectionEndpoint, LGetMemberObject, TScopeMemberType.mtText,
      function: string
      begin
        result := Authenticator.RedirectionEndpoint;
      end,
      procedure(AValue: string)
      begin
        Authenticator.RedirectionEndpoint := AValue;
      end);

    CreateReadWriteField<string>(sScope, LGetMemberObject, TScopeMemberType.mtText,
      function: string
      begin
        result := Authenticator.Scope;
      end,
      procedure(AValue: string)
      begin
        Authenticator.Scope := AValue;
      end);

    CreateReadWriteField<string>(sLocalState, LGetMemberObject, TScopeMemberType.mtText,
      function: string
      begin
        result := Authenticator.LocalState;
      end,
      procedure(AValue: string)
      begin
        Authenticator.LocalState := AValue;
      end);

  end;
end;

initialization

RegisterClasses([TOAuth1SignatureMethod_PLAINTEXT, TOAuth1SignatureMethod_HMAC_SHA1]);

end.
