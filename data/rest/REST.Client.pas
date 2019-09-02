{*******************************************************}
{                                                       }
{             Delphi REST Client Framework              }
{                                                       }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}
/// <summary>
/// REST.Client is a foundation for simplifying the creation of specific REST API Clients.
/// </summary>
unit REST.Client;

interface

uses
  System.SysUtils, System.StrUtils, System.Classes, System.Generics.Collections,
  System.TypInfo, System.Rtti,
  Data.Bind.ObjectScope,
  Data.Bind.Components,
  REST.HTTPClient,
  Data.DBXJson,
  REST.Consts,
  REST.Types,
  REST.BindSource;

{$SCOPEDENUMS ON}

const
  RESTCLIENT_VERSION = '1.0';

type
  IAuthenticator = interface;
  TRESTRequestParameter = class;
  TRESTRequestParameterList = class;

  TCustomRESTRequest = class;
  TCustomRESTResponse = class;
  TCustomRESTClient = class;
  TCustomAuthenticator = class;

  /// <summary>
  /// Any Authenticator class needs to implement IAuthenticator. To simplify things Authenticators can be descended
  /// from TCistomAuthenticator
  /// </summary>
  IAuthenticator = interface
    ['{0842F4AC-5052-45EC-A0B5-DC98DB1689AB}']
    /// <summary>
    /// Authenticate has to take the required actions on AClient and ARequest that are required to successfully
    /// authenticate with the server and the specific protocol implemented by any Authenticator class implementing
    /// this interface.
    /// </summary>
    /// <param name="ARequest">
    /// The current RESTRequest asking for authentication.
    /// </param>
    procedure Authenticate(ARequest: TCustomRESTRequest);
    /// <summary>
    /// Resets an authenticator to default values and clears all entries. An authenticator
    /// should not keep any stae information after this method being called.
    /// </summary>
    procedure ResetToDefaults;
  end;

  /// <summary>
  /// Parameter for REST requests
  /// </summary>
  TRESTRequestParameter = class(TCollectionItem)
  private
    FName: string;
    FValue: string;
    FKind: TRESTRequestParameterKind;
    FContentType: TRESTContentType;
    FOptions: TRESTRequestParameterOptions;
    function KindIsStored: Boolean;
  protected
    procedure SetKind(const AValue: TRESTRequestParameterKind); virtual;
    procedure SetName(const AValue: string); virtual;
    procedure SetOptions(const AValue: TRESTRequestParameterOptions); virtual;
    procedure SetValue(const AValue: string); virtual;
    procedure SetContentType(const AValue: TRESTContentType); virtual;
  public
    constructor Create(ACollection: TCollection); override;

    procedure Assign(ASource: TPersistent); override;

    /// <summary>
    /// Return a human-readable representation of this parameter
    /// </summary>
    /// <returns>String</returns>
    function ToString: string; override;
    function GetDisplayName: string; override;
  published
    /// <summary>
    /// Type of the parameter
    /// </summary>
    property Kind: TRESTRequestParameterKind read FKind write SetKind stored KindIsStored;
    /// <summary>
    /// Name of the parameter
    /// </summary>
    property name: string read FName write SetName;
    /// <summary>
    /// Additional processing options
    /// </summary>
    property Options: TRESTRequestParameterOptions read FOptions write SetOptions default [];
    /// <summary>
    /// Value of the parameter
    /// </summary>
    property Value: string read FValue write SetValue;
    /// <summary>
    /// <para>
    /// If ContentType is set to a specific value, then this will be used as actual content-type for the
    /// corresponding PUT or POST request - in this case the developer is responsible for applying any necessary
    /// encoding, as no standard encoding will be applied. The poDoNotEncode option has no meaning in this case.
    /// </para>
    /// <para>
    /// If left empty, the content type will be chosen basically depending on the number of existing parameters,
    /// that go into the requests body.
    /// </para>
    /// <para>
    /// Single parameter will use application/x-www-form-urlencoded, multiple parameters multipart/mixed instead.
    /// </para>
    /// </summary>
    /// <remarks>
    /// This property is only relevant for PUT and POST request. GET/DELETE request do not have a content-type.
    /// </remarks>
    property ContentType: TRESTContentType read FContentType write SetContentType default TRESTContentType.ctNone;
  end;

  TRESTRequestParameterDict = TDictionary<string, TRESTRequestParameter>;
  TRESTRequestParameterArray = TArray<TRESTRequestParameter>;

  TRESTRequestParameterListOwnerNotify = interface
    ['{48713C06-9469-4648-BCF9-FF83C6A58F5D}']
    procedure ParameterListChanged;
    procedure ParameterValueChanged;
  end;

  /// <summary>
  /// Container for parameters that will be associated with a REST request.
  /// </summary>
  /// <seealso cref="TRESTRequestParameter" />
  TRESTRequestParameterList = class(TOwnedCollection)
  private type
    TEnumerator = class(TCollectionEnumerator)
    public
      function GetCurrent: TRESTRequestParameter; inline;
      property Current: TRESTRequestParameter read GetCurrent;
    end;

  protected
    function GetItem(AIndex: integer): TRESTRequestParameter;
    procedure SetItem(AIndex: integer; const AValue: TRESTRequestParameter);
    function GetAttrCount: integer; override;
    function GetAttr(Index: integer): string; override;
    function GetItemAttr(Index, ItemIndex: integer): string; override;
    procedure Update(AItem: TCollectionItem); override;
  public
    constructor Create(const AOwner: TComponent);
    destructor Destroy; override;

    function GetEnumerator: TEnumerator;

    /// <summary>
    /// Shortcut to Add(AName, AValue, Cookie) overload
    /// </summary>
    /// <param name="AName">Name of the cookie to add</param>
    /// <param name="AValue">Value of the cookie to add</param>
    function AddCookie(const AName, AValue: string): TRESTRequestParameter; virtual;
    /// <summary>
    /// Shortcut to Add(name, value, HttpHeader) overload
    /// </summary>
    /// <param name="AName">Name of the header to add</param>
    /// <param name="AValue">Value of the header to add</param>
    function AddHeader(const AName, AValue: string): TRESTRequestParameter; virtual;
    /// <summary>
    /// Calls Add() for all public, published &readable properties of obj
    /// </summary>
    /// <param name="AObject">The object with properties to add as parameters</param>
    procedure AddObject(AObject: TObject); overload; virtual;

    /// <summary>
    /// Calls Add() for all public, readable properties specified in the white list
    /// </summary>
    /// <example>
    /// request.AddObject(product, "ProductId", "Price", ...);
    /// </example>
    /// <param name="AObject">The object with properties to add as parameters</param>
    /// <param name="WhiteList">The names of the properties to include</param>
    procedure AddObject(AObject: TObject; WhiteList: TStrings); overload; virtual;

    /// <summary>
    /// Adds an empty parameter to the collection.
    /// </summary>
    function AddItem: TRESTRequestParameter; overload;

    /// <summary>
    /// Adds a parameter to the collection. If a parameter of the same name already exists, then the previous parameter
    /// will be overwritten. There are five kinds of parameters:
    /// - GetOrPost: Either a QueryString value or encoded form value based on method
    /// - HttpHeader: Adds the name/value pair to the HTTP request's Headers collection
    /// - UrlSegment: Inserted into URL if there is a matching url token e.g. {AccountId}
    /// - Cookie: Adds the name/value pair to the HTTP request's Cookies collection
    /// - RequestBody: Used by AddBody() (not recommended to use directly)
    /// </summary>
    /// <param name="AName">Name of the parameter</param>
    /// <param name="AValue">Value of the parameter</param>
    /// <param name="AKind">The kind of parameter to add</param>
    /// <param name="AOptions">Set of parameter options </param>
    function AddItem(const AName, AValue: string; AKind: TRESTRequestParameterKind;
      AOptions: TRESTRequestParameterOptions = []): TRESTRequestParameter; overload;

    property Items[index: integer]: TRESTRequestParameter read GetItem write SetItem; default;

    procedure Delete(const AParam: TRESTRequestParameter); overload;
    procedure Delete(const AName: string); overload;
    function IndexOf(const AParam: TRESTRequestParameter): integer; overload; virtual;
    function IndexOf(const AName: string): integer; overload; virtual;

    /// <summary>
    /// Adds a HTTP parameter to the request. If a parameter of the same name already exists, then the previous parameter
    /// will be removed and freed. (QueryString for GET, DELETE, OPTIONS and HEAD; Encoded form for POST and PUT)
    /// </summary>
    /// <param name="AName">Name of the parameter</param>
    /// <param name="AValue">Value of the parameter</param>
    function AddItem(const AName, AValue: string): TRESTRequestParameter; overload; virtual;
    /// <summary>
    /// Adds a parameter to the request. If a parameter of the same name already exists, then the previous parameter
    /// will be removed and freed.<br /><br />There are five kinds of parameters: - GetOrPost: Either a QueryString
    /// value or encoded form value based on method - HttpHeader: Adds the name/value pair to the HTTP request's
    /// Headers collection - UrlSegment: Inserted into URL if there is a matching url token e.g. {AccountId} -
    /// Cookie: Adds the name/value pair to the HTTP request's Cookies collection - RequestBody: Used by AddBody()
    /// (not recommended to use directly)
    /// </summary>
    /// <param name="AName">
    /// Name of the parameter
    /// </param>
    /// <param name="AValue">
    /// Value of the parameter
    /// </param>
    /// <param name="AKind">
    /// The kind of parameter to add
    /// </param>
    /// <param name="AOptions">
    /// Options for processing the parameter
    /// </param>
    /// <param name="AContentType">
    /// Content type of the parameter, for body parameters. Set to TRESTContentType.ctNone if no specific content type is needed.
    /// </param>

    function AddItem(const AName, AValue: string; const AKind: TRESTRequestParameterKind;
      const AOptions: TRESTRequestParameterOptions; AContentType: TRESTContentType): TRESTRequestParameter;
      overload; virtual;
    /// <summary>
    /// Shortcut to Add(name, value, UrlSegment) overload
    /// </summary>
    /// <param name="AName">Name of the segment to add</param>
    /// <param name="AValue">Value of the segment to add</param>
    procedure AddUrlSegment(const AName, AValue: string); virtual;
    /// Locates a request parameter by its name. Returns nil if no matching parameter is found.
    /// </summary>
    /// <param name="AName">
    /// The parameter's name (Key)
    /// </param>
    function ParameterByName(AName: string): TRESTRequestParameter; virtual;

    /// Locates a request parameter by its index.
    /// </summary>
    /// <param name="AIndex">
    /// The parameter's index
    /// </param>
    function ParameterByIndex(AIndex: integer): TRESTRequestParameter; virtual;

                    
    /// <summary>
    /// Adds a file to the Files collection to be included with a POST or PUT request
    /// (other methods do not support file uploads).
    /// </summary>
    /// <param name="AName">The parameter name to use in the request</param>
    /// <param name="APath">Full path to file to upload</param>
    // procedure AddFile(const AName, APath: string); overload; virtual; abstract;

    /// <summary>
    /// Adds the bytes to the Files collection with the specified file name
    /// </summary>
    /// <param name="AName">The parameter name to use in the request</param>
    /// <param name="Bytes">The file data</param>
    /// <param name="AFilename">The file name to use for the uploaded file</param>
    // procedure AddFile(const AName: string; Bytes: TBytes; const AFilename: string); overload; virtual; abstract;

    /// <summary>
    /// Adds the bytes to the Files collection with the specified file name and content type
    /// </summary>
    /// <param name="AName">The parameter name to use in the request</param>
    /// <param name="Bytes">The file data</param>
    /// <param name="AFilename">The file name to use for the uploaded file</param>
    /// <param name="AContentType">The MIME type of the file to upload</param>
    // procedure AddFile(const AName: string; Bytes: TBytes; const AFilename, AContentType: string); overload;
    // virtual; abstract;

    /// <summary>
    /// checks the existance of a parameter with the given name
    /// </summary>
    /// <param name="AName">Name of the Parameter</param>
    function ContainsParameter(const AName: string): boolean;

    function FromString(const OldValue, NewValue: string): boolean;
  end;

  TExecuteMethod = procedure of object;

  /// <summary>
  /// Provides classes and support for asynchronous execution of REST client requests.
  /// </summary>
  TRESTExecutionThread = class(TThread)
  private
    FCompletionHandler: TCompletionHandler;
    FExecuteMethod: TExecuteMethod;
    FSynchronized: boolean;
    FRequest: TCustomRESTRequest;
  protected

    procedure HandleCompletion;
    procedure Execute; override;
  public
    constructor Create(AExecuteMethod: TExecuteMethod; ARequest: TCustomRESTRequest;
      ACompletionHandler: TCompletionHandler; ASynchronized: boolean; AFreeThread: boolean = true);
  end;

  TCustomRESTRequestNotifyEvent = procedure(Sender: TCustomRESTRequest) of object;

  TSubRESTRequestBindSource = class;

  /// <summary>
  /// Container for data used to make requests
  /// </summary>
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice or pidAndroid)]
  TCustomRESTRequest = class(TBaseObjectBindSourceDelegate, TRESTRequestParameterListOwnerNotify)
  public type
    TNotify = class(TRESTComponentNotify)
    public
      procedure ParameterListChanged(Sender: TObject); virtual;
    end;

    TNotifyList = TRESTComponentNotifyList<TNotify>;
  private
    FAccept: string;
    FAcceptCharset: string;
    FAcceptEncoding: string;
    FHandleRedirects: boolean;
    FMethod: TRESTRequestMethod;
    FPosting: Boolean;
    FAutoCreateParams: boolean;
    FParams: TRESTRequestParameterList;
    FTransientParams: TRESTRequestParameterList;
    FResource: string;
    FTimeout: integer;
    FClient: TCustomRESTClient;
    FResponse: TCustomRESTResponse;
    FBindSource: TSubRESTRequestBindSource;
    FNotifyList: TNotifyList;

    FOnAfterExecute: TCustomRESTRequestNotifyEvent;
    FExecutionPerformance: TExecutionPerformance;

    FOnHTTPProtocolError: TCustomRESTRequestNotifyEvent;
    FSynchronizedEvents: boolean;
    function AcceptIsStored: Boolean;
    function AcceptCharSetIsStored: Boolean;
    function MethodIsStored: Boolean;
    procedure SetAccept(const AValue: string);
    procedure SetAcceptCharset(const AValue: string);
    procedure SetHandleRedirects(const AValue: boolean);
    procedure SetClient(const AValue: TCustomRESTClient);
    function GetParams: TRESTRequestParameterList;
    function GetExecutionPerformance: TExecutionPerformance;
    procedure SetParams(const AValue: TRESTRequestParameterList);
    procedure SetAutoCreateParams(const AValue: boolean);
  protected
    function GetClient: TCustomRESTClient; virtual;
    function CreateBindSource: TBaseObjectBindSource; override;
    procedure ParameterListChanged; virtual;
    procedure ParameterValueChanged; virtual;
    procedure PropertyValueChanged; virtual;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    /// <summary>
    /// OnAfterExecute dispatch method. Called internally.
    /// </summary>
    procedure DoAfterExecute; virtual;

    /// <summary>
    /// OnHTTPProtocol dispatch method. Called internally.
    /// </summary>
    procedure DoHTTPProtocolError;

    procedure DoApplyCookieParams(const AParamList: TRESTRequestParameterArray; const CookieURL: string); virtual;
    procedure DoApplyHeaderParams(const AParamList: TRESTRequestParameterArray); virtual;
    procedure DoApplyURLSegments(const AParamList: TRESTRequestParameterArray; var AURL: string); virtual;

    /// <summary>
    /// Prepares the actual query string to have all parameters, values encoded properly
    /// </summary>
    /// <param name="AParamlist">
    /// The list where query parameters are taken from
    /// </param>
    /// <param name="AURL">
    /// Will be appended with all parameters
    /// </param>
    procedure DoPrepareQueryString(const AParamList: TRESTRequestParameterArray; var AURL: string);

    /// <summary>
    /// Creates a stream that contains all relevant parameters in a stream that represents a either list of
    /// name-value pairs or a HTTP Multi-Part structure - depending on the requests content-type.
    /// </summary>
    /// <param name="ABodyStream">
    /// Will point to a newly created stream, containing the body. Caller is responsible for destruction.
    /// </param>
    procedure DoPrepareRequestBody(AParamList: TRESTRequestParameterArray; var ABodyStream: TStream); virtual;

    /// <summary>
    /// HandleEvent ensures that an event handler (like DoAfterExecute) is ran synchronized or not depending on
    /// SynchronizedEvents
    /// </summary>
    /// <seealso cref="TCustomRESTClient.SynchronizedEvents" />
    procedure HandleEvent(AEventHandler: TMethod);

    function GetMethod: TRESTRequestMethod; virtual;
    function GetResource: string; virtual;
    function GetTimeout: integer; virtual;
    procedure SetAcceptEncoding(const AValue: string);
    procedure SetMethod(const AValue: TRESTRequestMethod); virtual;
    procedure SetResource(const AValue: string); virtual;
    procedure SetTimeout(const AValue: integer); virtual;
    function GetResponse: TCustomRESTResponse;
    procedure SetResponse(const AResponse: TCustomRESTResponse);
    procedure SetSynchronizedEvents(const AValue: boolean); virtual;

    property NotifyList: TNotifyList read FNotifyList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    (*
      /// <summary>
      /// Used by the default deserializers to explicitly set which date format string to use when parsing dates.
      /// </summary>
      string DateFormat { get; set; }
    *)

    /// <summary>
    /// Based on the parameters the content-type is "x-www-form-urlencoded",  "multipart/formdata" (e.g. for
    /// file-transfer etc.) or a specific content-type as set by at least one of the parameters.
    /// </summary>
    function ContentType: TRESTContentType; overload; virtual;
    function ContentType(const AParamsArray: TRESTRequestParameterArray): TRESTContentType; overload; virtual;

    /// <summary>
    /// Adds a HTTP parameter to the request. If a parameter of the same name already exists, then the previous parameter
    /// will be removed and freed. (QueryString for GET, DELETE, OPTIONS and HEAD; Encoded form for POST and PUT)
    /// </summary>
    /// <param name="AName">Name of the parameter</param>
    /// <param name="AValue">Value of the parameter</param>
    procedure AddParameter(const AName, AValue: string); overload; virtual;
    /// <summary>
    /// Adds a parameter to the request. If a parameter of the same name already exists, then the previous parameter
    /// will be removed and freed. There are five kinds of parameters:
    /// - GetOrPost: Either a QueryString value or encoded form value based on method
    /// - HttpHeader: Adds the name/value pair to the HTTP request's Headers collection
    /// - UrlSegment: Inserted into URL if there is a matching url token e.g. {AccountId}
    /// - Cookie: Adds the name/value pair to the HTTP request's Cookies collection
    /// - RequestBody: Used by AddBody() (not recommended to use directly)
    /// </summary>
    /// <param name="AName">Name of the parameter</param>
    /// <param name="AValue">Value of the parameter</param>
    /// <param name="AKind">The kind of parameter to add</param>
    procedure AddParameter(const AName, AValue: string; const AKind: TRESTRequestParameterKind); overload; virtual;

    /// <summary>
    /// Adds a parameter to the request. If a parameter of the same name already exists, then the previous parameter
    /// will be removed and freed.<br /><br />There are five kinds of parameters: - GetOrPost: Either a QueryString
    /// value or encoded form value based on method - HttpHeader: Adds the name/value pair to the HTTP request's
    /// Headers collection - UrlSegment: Inserted into URL if there is a matching url token e.g. {AccountId} -
    /// Cookie: Adds the name/value pair to the HTTP request's Cookies collection - RequestBody: Used by AddBody()
    /// (not recommended to use directly)
    /// </summary>
    /// <param name="AName">
    /// Name of the parameter
    /// </param>
    /// <param name="AValue">
    /// Value of the parameter
    /// </param>
    /// <param name="AKind">
    /// The kind of parameter to add
    /// </param>
    /// <param name="AOptions">
    /// Options for processing the parameter
    /// </param>
    procedure AddParameter(const AName, AValue: string; const AKind: TRESTRequestParameterKind;
      const AOptions: TRESTRequestParameterOptions); overload; virtual;

    /// <summary>
    /// Add an authentication parameter
    /// </summary>
    procedure AddAuthParameter(const AName, AValue: string; const AKind: TRESTRequestParameterKind;
      const AOptions: TRESTRequestParameterOptions = []); overload; virtual;

    function CreateUnionParameterList: TRESTRequestParameterArray;

    /// <summary>
    /// Adds ABodyContent as Body parameter to the request.
    /// </summary>
    /// <remarks>
    /// An already existing body parameter will NOT be replaced/deleted!
    /// </remarks>
    procedure AddBody(ABodyContent: string; AContentType: TRESTContentType = ctNone); overload; virtual;
    /// <summary>
    /// Serializes obj to JSON and adds it to the request body.
    /// </summary>
    /// <param name="AObject">The object to serialize</param>
    /// <param name="AContentType">
    /// Content type for the body. Defaults to ctNone, which means no specific content type will be set.
    /// </param>
    /// <remarks>
    /// An already existing body parameter will NOT be replaced/deleted!
    /// </remarks>
    procedure AddBody(AObject: TObject; AContentType: TRESTContentType = ctNone); overload; virtual;
    /// <summary>
    /// Adds ABodyContent as Body parameter to the request.
    /// </summary>
    /// <remarks>
    /// An already existing body parameter will NOT be replaced/deleted!
    /// </remarks>
    procedure AddBody(ABodyContent: TStream; AContentType: TRESTContentType = ctNone); overload; virtual;

    /// <summary>
    /// Removes all body parameters (TRESTRequestParameterKind.pkREQUESTBODY) from the request.
    /// </summary>
    /// <summary>
    procedure ClearBody; virtual;

    /// <summary>
    /// Sets all fields to their default value. No state information is kept. If Response
    /// is assigned, then Response.ResetToDefaults is called too.
    /// </summary>
    procedure ResetToDefaults; virtual;

    /// <summary>
    /// Exceutes an actual HTTP request
    /// </summary>
    /// <exception cref="ERESTException">
    /// Anything which is NOT an HTTP protocol exception.
    /// </exception>
    /// <remarks>
    /// Execute does NOT raise HTTP protocol exceptions, instead TCustomRESTClient.Response.StatusCode should be checked
    /// for desired values. In many cases a 2xx StatusCode signals "succes" - other status codes don't need to be
    /// errors though. It depends on the actual REST API.
    /// </remarks>
    procedure Execute;

    /// <summary>
    /// <para>
    /// Executes a request asynchronously, i.e. run it in its own thread. There is no automatic serialization op
    /// property access though, which means that while the execution thread runs, properties of all involved
    /// TCustomRESTClient and TCustomRESTRequest instances should not be touched from other threads (including the main thread)
    /// <br /><br />Using ExecuteAsync is strongly recommended on mobile platforms. iOS (and likely Android) will
    /// terminate an application if it considers the main thread to be unresponsive, which would be the case if
    /// there is a running request which takes more than a second or two to return.
    /// </para>
    /// <para>
    /// The idea behind this is that the UI runs in the main thread and mobile devices should respond to user
    /// interaction basically immediately. Sluggish behaviour (caused by blocking the main thread) is considered
    /// unacceptable on these small devices.
    /// </para>
    /// </summary>
    /// <param name="ARequest">
    /// The request to be executed
    /// </param>
    /// <param name="ACompletionHandler">
    /// An anonymous method that will be run after the execution completed
    /// </param>
    /// <param name="ASynchronized">
    /// Specifies if ACompletioHandler will in the main thread's (true) or execution thread's (false) context
    /// </param>
    /// <param name="AFreeThread">
    /// If true, then the execution thread will be freed after it completed
    /// </param>
    /// <returns>
    /// Returns a reference to the execution thread. Should only be used if AFreeThread=false, as other wise the
    /// reference may get invalid unexpectedly.
    /// </returns>
    function ExecuteAsync(ACompletionHandler: TCompletionHandler = nil; ASynchronized: boolean = true;
      AFreeThread: boolean = true): TRESTExecutionThread; virtual;

    /// <summary>
    /// Builds the final URL that will be executed. this includes also the placeholders for url-segments
    /// as well as query-parameters
    /// </summary>
    function GetFullRequestURL(AIncludeParams: boolean = true): string;

    /// <summary>
    /// Transient Params are typically create and re-created by
    /// Authenticators. They are not persistent and will not be visible at
    /// designtime.
    /// In case of a naming-conflict any transient param will override a
    /// normal parameter.
    /// </summary>
    property TransientParams: TRESTRequestParameterList read FTransientParams;

    /// <summary>
    /// Specifies if "url-segment" parameters should be created automatically from the baseurl/resource.
    /// No other parameter types are affected.
    /// </summary>
    /// <example>
    /// request.Resource = "Products/{ProductId}";
    /// This will create a parameter "ProductID" (with no value assigned).
    /// If a parameter of the same name already existed
    /// the old parameter will <i>not</i> be overwritten.
    /// </example>
    property AutoCreateParams: boolean read FAutoCreateParams write SetAutoCreateParams default true;

    /// <summary>
    /// <para>
    /// Specifies the Content-Type that is accepted for the response.
    /// </para>
    /// <para>
    /// Defaults to : application/json,text/plain;q=0.9,text/html;q=0.8<br />We are after JSON, which is why it has
    /// the highest quality factor (default 1.0)
    /// </para>
    /// </summary>
    property Accept: string read FAccept write SetAccept stored AcceptIsStored;
    /// <summary>
    /// Specifies the charset that the response is expected to be encoded in. Defaults to UTF-8.
    /// </summary>
    property AcceptCharset: string read FAcceptCharset write SetAcceptCharset stored AcceptCharSetIsStored;
    /// <summary>
    /// Specifies the accepted encoding. Defaults to empty string, which means "identity encoding".
    /// </summary>
    /// <value>
    /// To allow for compressed responses set to "gzip, deflate"
    /// </value>
    property AcceptEncoding: string read FAcceptEncoding write SetAcceptEncoding;

    /// <summary>
    /// Specifies if the HTTP client should follow possible redirects (301, 303) for this request. Default =TRUE
    /// </summary>
    /// <remarks>
    /// See http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html
    /// </remarks>
    property HandleRedirects: boolean read FHandleRedirects write SetHandleRedirects default True;

    property Client: TCustomRESTClient read GetClient write SetClient;

    /// <summary>
    /// Determines what HTTP method to use for this request. Supported methods: GET, POST, PUT, DELETE, HEAD, OPTIONS
    /// Default is GET
    /// </summary>
    property Method: TRESTRequestMethod read GetMethod write SetMethod stored MethodIsStored;
    /// <summary>
    /// Container of all HTTP parameters to be passed with the request.
    /// See AddParameter() for explanation of the types of parameters that can be passed
    /// </summary>
    property Params: TRESTRequestParameterList read GetParams write SetParams;
    /// <summary>
    /// The Resource path to make the request against.
    /// Tokens are substituted with UrlSegment parameters and match by name.
    /// Should not include the scheme or domain. Do not include leading slash.
    /// Combined with RestClient.BaseUrl to assemble final URL:
    /// {BaseUrl}/{Resource} (BaseUrl is scheme + domain, e.g. http://example.com)
    /// </summary>
    /// <example>
    /// // example for url token replacement
    /// request.Resource = "Products/{ProductId}";
    /// request.AddParameter("ProductId", 123, ParameterType.UrlSegment);
    /// </example>
    property Resource: string read GetResource write SetResource;

    /// <summary>
    /// After executing a Request, it will fill in all values from the HTTP answer to this TCustomRESTResponse
    /// instance. An instance can either be assigned explicitly (e.g. in the IDE) or can be left empty, in which case
    /// TCustomRESTRequest will create an instance on demand.
    /// </summary>
    property Response: TCustomRESTResponse read GetResponse write SetResponse;

    /// <summary>
    /// Timeout in milliseconds to be used for the request.
    /// </summary>
    property Timeout: integer read GetTimeout write SetTimeout default 30000;

    /// <summary>
    /// OnAfterExecute gets fired after TRESTCRequest.Execute has finished and the Reponse has been processed. Any
    /// observers will get notified <i>before</i> OnAfterExecute fires.
    /// </summary>
    property OnAfterExecute: TCustomRESTRequestNotifyEvent read FOnAfterExecute write FOnAfterExecute;

    /// <summary>
    /// Provides detailed information about the performance of the execution of a request.
    /// </summary>
    property ExecutionPerformance: TExecutionPerformance read GetExecutionPerformance;
    /// <summary>
    /// Specifies if Events (such as OnAfterExecute) should run in the context of the main
    /// thread (true) or in the context of an arbitrary thread - which was created by the developer or by using
    /// ExecuteAsync.
    /// </summary>
    /// <seealso cref="TCustomRESTRequest.ExecuteAsync" />
    property SynchronizedEvents: boolean read FSynchronizedEvents write SetSynchronizedEvents default true;

    /// <summary>
    /// <para>
    /// OnHTTPProtocolError gets fired when the server returns a status code other than 200 - OK
    /// </para>
    /// <para>
    /// Check AClient.Response.StatusCode for the actual status.
    /// </para>
    /// <para>
    /// This event will not get fired if a non HTTP-related exception occurs during execution. This includes, but
    /// is not limited to timeout and server not found related exceptions.
    /// </para>
    /// </summary>
    property OnHTTPProtocolError: TCustomRESTRequestNotifyEvent read FOnHTTPProtocolError write FOnHTTPProtocolError;
    property BindSource: TSubRESTRequestBindSource read FBindSource;
  end;

  /// <summary>
  /// LiveBindings adapter for TCustomRESTRequest. Create bindable members
  /// </summary>
  TRESTRequestAdapter = class(TRESTComponentAdapter)
  public type
    TNotify = class(TCustomRESTRequest.TNotify)
    private
      FAdapter: TRESTRequestAdapter;
      constructor Create(const AAdapter: TRESTRequestAdapter);
    public
      procedure ParameterListChanged(Sender: TObject); override;
      procedure PropertyValueChanged(Sender: TObject); override;
    end;
  private
    FRequest: TCustomRESTRequest;
    FNotify: TNotify;
    procedure SetRequest(const ARequest: TCustomRESTRequest);
    procedure AddParameterFields;
    procedure ParameterListChanged;
    procedure AddPropertyFields;
  protected
    procedure DoChangePosting; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetCanActivate: boolean; override;
    procedure AddFields; override;
    function GetSource: TBaseLinkingBindSource; override;
  public
    constructor Create(AComponent: TComponent); override;
    destructor Destroy; override;
    procedure GetMemberNames(AList: TStrings); override;
    property Request: TCustomRESTRequest read FRequest write SetRequest;
  end;

  /// <summary>
  /// LiveBindings bindsource for TCustomRESTRequest.  Creates adapter.
  /// </summary>
  TCustomRESTRequestBindSource = class(TRESTComponentBindSource)
  private
    FAdapter: TRESTRequestAdapter;
    function GetRequest: TCustomRESTRequest;
    procedure SetRequest(const Value: TCustomRESTRequest);
  protected
    function CreateAdapter: TRESTComponentAdapter; override;
  public
    property Request: TCustomRESTRequest read GetRequest write SetRequest;
    property Adapter: TRESTRequestAdapter read FAdapter;
  end;

  /// <summary>
  /// LiveBindings bindsource for TCustomRESTRequest.  Publishes subcomponent properties
  /// </summary>
  TSubRESTRequestBindSource = class(TCustomRESTRequestBindSource)
  published
    property AutoActivate;
    property AutoEdit;
    property AutoPost;
  end;

  TRESTRequest = class(TCustomRESTRequest)
  published
    property Accept;
    property AcceptCharset;
    property AcceptEncoding;
    property AutoCreateParams;
    property HandleRedirects;
    property Client;
    property Method;
    property Params;
    property Resource;
    property Response;
    property Timeout;
    property OnAfterExecute;
    property ExecutionPerformance;
    property SynchronizedEvents;
    property OnHTTPProtocolError;
    property BindSource;
  end;

  TSubRESTResponseBindSource = class;

  /// <summary>
  /// Base class for TCustomRESTResponse
  /// </summary>
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice or pidAndroid)]
  TCustomRESTResponse = class(TBaseObjectBindSourceDelegate)
  public type
    TNotify = class(TRESTComponentNotify)
    protected
      procedure JSONValueChanged(ASender: TObject); virtual;
    end;

    TNotifyList = TRESTComponentNotifyList<TNotify>;
    TUpdateOption = (PropertyValueChanged, JSONValueChanged);
    TUpdateOptions = set of TUpdateOption;
  private
    FUpdating: Integer;
    FUpdateOptions: TUpdateOptions;
    FContent: string;
    FContentEncoding: string;
    FContentType: string;
    FErrorMessage: string;
    FHeaders: TStrings;
    FRAWBytes: TBytes;
    FFullRequestURI: string;
    FJSONValue: TJsonValue;
    FRootElement: string;
    FServer: string;
    FStatusCode: integer;
    FStatusText: string;

    FBindSource: TSubRESTResponseBindSource;
    FNotifyList: TNotifyList;

    function GetHeaders: TStrings;
    function GetJSONValue: TJsonValue;
    procedure SetRootElement(const AValue: string);
    function GetStatusCode: integer;
    procedure SetStatusCode(const AValue: integer);
    function GetStatusText: string;
    procedure SetStatusText(const AValue: string);
    function GetContentEncoding: string;
    function GetContent: string; virtual;
    function GetContentLength: cardinal; virtual;
    procedure SetContentEncoding(const AValue: string);
    function GetContentType: string;
    procedure SetContentType(const AValue: string);
    function GetErrorMessage: string;
    procedure SetErrorMessage(const AValue: string);
    function GetFullRequestURI: string;
    procedure SetFullRequestURI(const AValue: string);
    function GetServer: string;
    procedure SetServer(const AValue: string);
  protected
    procedure BeginUpdate;
    procedure EndUpdate;
    function IsUpdating: Boolean;
    function CreateBindSource: TBaseObjectBindSource; override;
    procedure PropertyValueChanged; virtual;
    procedure JSONValueChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// <summary>
    /// Simple and rough attempt to extract simple parameters out of a response content. Will certainly not work for
    /// all scenarios, but might be convenient in some situations
    /// </summary>
    /// <remarks>
    /// Works on text/html and application/json conten-types.
    /// </remarks>
    /// <example>
    /// <para>
    /// text/html
    /// </para>
    /// <para>
    /// oauth_token=abc123&amp;oauth_token_secret=def456&amp;oauth_callback_confirmed=true
    /// </para>
    /// <para>
    /// application/json
    /// </para>
    /// <para>
    /// { "oauth_token" : "abc123", "oauth_token_secret" : "def456", "oauth_callback_confirmed" : true }
    /// </para>
    /// </example>
    function GetSimpleValue(const AName: string; var AValue: string): boolean;

    /// <summary>
    /// Sets all fields to their default value. No state information, such as Content,
    /// StatusCode etc is kept.
    /// </summary>
    procedure ResetToDefaults; virtual;
    procedure SetContent(AStream: TStream);
    /// <summary>
    /// Encoding of the response content. Used for compression (e.g. deflate, gzip).
    /// The actual RAWBytes or Content string would already be decoded though.
    /// Thus this property is for reference purposes only.
    /// </summary>
    property ContentEncoding: string read GetContentEncoding write SetContentEncoding;
    /// <summary>
    /// Length in bytes of the response content
    /// </summary>
    property ContentLength: cardinal read GetContentLength;
    /// <summary>
    /// MIME content type of response
    /// </summary>
    property ContentType: string read GetContentType write SetContentType;
    (*
      /// <summary>
      /// Cookies returned by server with the response
      /// </summary>
      IList<RestResponseCookie> Cookies { get; }
    *)

    (*
      /// <summary>
      /// Headers returned by server with the response
      /// </summary>
      IList<Parameter> Headers { get; }
    *)

    (*
      /// <summary>
      /// Status of the request. Will return Error for transport errors.
      /// HTTP errors will still return ResponseStatus.Completed, check StatusCode instead
      /// </summary>
      ResponseStatus ResponseStatus { get; set; }
    *)

    /// <summary>
    /// HTTP protocol error that occured during request
    /// </summary>
    property ErrorMessage: string read GetErrorMessage write SetErrorMessage;
    /// <summary>
    /// HTTP response header-lines
    /// </summary>
    property Headers: TStrings read GetHeaders;

    /// <summary>
    /// <para>
    /// Response content string parsed as JSON value. Nil, if content does not successfully parse.
    /// </para>
    /// <para>
    /// If TCustomRESTRequest.RootValue has a value, then the content needs to represent a Json object (not an array that
    /// is), and the result will be the value of the pair with the name "RootElement". RootElement supports a "dot" syntax. Example:
    /// {response:{"name":"Smith", "phone":"1234", "orders":[{...}, {...}]}}
    /// In that case setting RootElement to "response" would return the whol customer object.
    /// Setting RootElement to "response.orders" would return the array of order objects.
    /// </para>
    /// </summary>
    /// <remarks>
    /// This method returns TJSONValue by intention. The developer has to know and decide if this would cast to
    /// TJSONObject or TJSONArray (or possibly other types), as this depends on the actual API call.
    /// The returned JSONValue is owned by the RESTResponse, which means that the developer must not free this.
    /// </remarks>
    /// <example>
    /// <para>
    /// {result : {customer:"John Doe", address:"Silicon Valley"}}
    /// </para>
    /// <para>
    /// With RootElement set to "result", JSONValue will return the inner customer object:
    /// </para>
    /// <para>
    /// {customer:"John Doe", address:"Silicon Valley"}
    /// </para>
    /// </example>
    /// <seealso cref="RootElement" />
    property JSONValue: TJsonValue read GetJSONValue;
    (*
      /// <summary>
      /// Description of HTTP status returned
      /// </summary>
      string StatusDescription { get; set; }
    *)

    /// <summary>
    /// Response content as bytes array.
    /// </summary>
    /// <remarks>
    /// RAWBytes is already decoded using the charset as sent by the sever. They are in default Unicode encoding that
    /// is.
    /// </remarks>
    property RawBytes: TBytes read FRAWBytes;
    /// <summary>
    /// The fully qualified request URI that lead to this response.
    /// </summary>
    property FullRequestURI: string read GetFullRequestURI write SetFullRequestURI;
    /// <summary>
    /// Server identifier, e.g. "Microsoft-IIS/7.0"
    /// </summary>
    property Server: string read GetServer write SetServer;
    /// <summary>
    /// HTTP response status code
    /// </summary>
    property StatusCode: integer read GetStatusCode write SetStatusCode;
    /// <summary>
    /// HTTP response status text
    /// </summary>
    property StatusText: string read GetStatusText write SetStatusText;

    /// <summary>
    /// String representation of response content
    /// </summary>
    property Content: string read GetContent;

    /// <summary>
    /// Used by the JSON deserializer to determine where to start deserializing from. Can be used to skip container
    /// or root elements that do not have corresponding deserialization targets.
    /// </summary>
    /// <seealso cref="TCustomRESTResponse.JsonValue" />
    property RootElement: string read FRootElement write SetRootElement;

    property BindSource: TSubRESTResponseBindSource read FBindSource;
    property NotifyList: TNotifyList read FNotifyList;
  end;

  /// <summary>
  /// LiveBindings adapter for TCustomRESTResponse. Create bindable members
  /// </summary>
  TRESTResponseAdapter = class(TRESTComponentAdapter)
  public type
    TNotify = class(TCustomRESTResponse.TNotify)
    private
      FAdapter: TRESTResponseAdapter;
      constructor Create(const AAdapter: TRESTResponseAdapter);
    public
      procedure PropertyValueChanged(Sender: TObject); override;
    end;
  private
    FResponse: TCustomRESTResponse;
    FNotify: TNotify;
    procedure SetResponse(const AResponse: TCustomRESTResponse);
    procedure AddPropertyFields;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetCanActivate: boolean; override;
    procedure AddFields; override;
    function GetSource: TBaseLinkingBindSource; override;
  public
    constructor Create(AComponent: TComponent); override;
    destructor Destroy; override;
    property Response: TCustomRESTResponse read FResponse write SetResponse;
  end;

  /// <summary>
  /// LiveBindings bindsource for TCustomRESTResponse. Creates adapter
  /// </summary>
  TCustomRESTResponseBindSource = class(TRESTComponentBindSource)
  private
    FAdapter: TRESTResponseAdapter;
    function GetResponse: TCustomRESTResponse;
    procedure SetResponse(const Value: TCustomRESTResponse);
  protected
    function CreateAdapter: TRESTComponentAdapter; override;
  public
    property Response: TCustomRESTResponse read GetResponse write SetResponse;
    property Adapter: TRESTResponseAdapter read FAdapter;
  end;

  /// <summary>
  /// LiveBindings bindsource for TCustomRESTRequest. Publishes subcomponent properties
  /// </summary>
  TSubRESTResponseBindSource = class(TCustomRESTResponseBindSource)
  published
    property AutoActivate;
    property AutoEdit;
    property AutoPost;
  end;

  TRESTResponse = class(TCustomRESTResponse)
  published
    property Content;
    property RootElement;
    property BindSource;
  end;

  /// <summary>
  /// Used for event handlers of TCustomRESTClient
  /// </summary>
  TCustomRESTClientNotifyEvent = procedure(Sender: TCustomRESTClient) of object;

  TSubRESTClientBindSource = class;

  /// <summary>
  /// TCustomRESTClient is an easy to use class for accessing REST APIs. It is typically used by inheriting from it in a
  /// specific REST API class.
  /// </summary>
  /// <remarks>
  /// <para>
  /// TCustomRESTClient works with JSON only. XML as transport format is not supported that is.<br />HTTP and HTTPS urls
  /// are supported, to actually use HTTPS, you need to download OpenSSL libraries for Indy (which is utilized in
  /// the background to handle HTTP communications).
  /// </para>
  /// <para>
  /// Pre-compiled SSL Libraries are available here:<br /><see href="http://indy.fulgan.com/SSL/" />
  /// </para>
  /// <para>
  /// Disclaimer: Certain export restrictions may apply to using SSL libraries. The site above is not affiliated
  /// with Embarcadero Technologies in any way. Embarcadero does not provide these SSL libraries.
  /// </para>
  /// </remarks>
  /// <example>
  /// TTwitterchAPI = class(TCustomRESTClient)<br />  procedure Tweet(AMessage:string);<br />end;
  /// </example>

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice or pidAndroid)]
  TCustomRESTClient = class(TBaseObjectBindSourceDelegate, TRESTRequestParameterListOwnerNotify)
  public type
    TNotify = class(TRESTComponentNotify)
    public
      procedure ParameterListChanged(Sender: TObject); virtual;
    end;

    TNotifyList = TRESTComponentNotifyList<TNotify>;
  private
    FBindSource: TSubRESTClientBindSource;
    FNotifyList: TNotifyList;
    FOnHTTPProtocolError: TCustomRESTClientNotifyEvent;
    FAutoCreateParams: boolean;
    FParams: TRESTRequestParameterList;
    FTransientParams: TRESTRequestParameterList;
    FAuthenticator: TCustomAuthenticator;
    FBaseURL: string;
    FFallbackCharsetEncoding: string;
    FSynchronizedEvents: boolean;
    FRaiseExceptionOn500: boolean;
    FIPImplementationID: string;
    FHttpClient: TRESTHTTP;
    FPosting: Boolean;
    procedure SetIPImplementationID(const Value: string);
    function FallbackCharsetEncodingIsStored: Boolean;
    function UserAgentIsStored: Boolean;
    procedure SetParams(const AValue: TRESTRequestParameterList);
    procedure SetAutoCreateParams(const AValue: boolean);
  protected
    property NotifyList: TNotifyList read FNotifyList;
    procedure ParameterListChanged; virtual;
    procedure ParameterValueChanged; virtual;
    procedure PropertyValueChanged; virtual;
    function CreateBindSource: TBaseObjectBindSource; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    /// <summary>
    /// (Re-)creates the actual Http client and applies default values.
    /// </summary>
    procedure CreateHttpClient; virtual;
    /// <summary>
    /// OnHTTPProtocol dispatch method. Called internally.
    /// </summary>
    procedure DoHTTPProtocolError; virtual;

    // Getters and Setters
    function GetProxyPassword: string; virtual;
    function GetProxyPort: integer; virtual;
    function GetProxyServer: string; virtual;
    function GetProxyUsername: string; virtual;
    /// <summary>
    /// HandleEvent ensures that an event handler is ran synchronized or not depending on
    /// SynchronizedEvents
    /// </summary>
    /// <seealso cref="TCustomRESTClient.SynchronizedEvents" />
    procedure HandleEvent(AEventHandler: TMethod);
    procedure SetBaseURL(const AValue: string); virtual;
    procedure SetProxyPassword(const AValue: string); virtual;
    procedure SetProxyPort(const AValue: integer); virtual;
    procedure SetProxyServer(const AValue: string); virtual;
    procedure SetProxyUsername(const AValue: string); virtual;
    procedure SetUserAgent(const AValue: string); virtual;
    function GetParams: TRESTRequestParameterList; virtual;
    function GetFallbackCharsetEncoding: string; virtual;
    procedure SetFallbackCharsetEncoding(const AValue: string); virtual;
    function GetAuthenticator: TCustomAuthenticator; virtual;
    procedure SetAuthenticator(const AValue: TCustomAuthenticator); virtual;
    function GetBaseURL: string; virtual;
    function GetAccept: string; virtual;
    procedure SetAccept(const AValue: string); virtual;
    function GetUserAgent: string; virtual;
    function GetAllowCookies: boolean; virtual;
    procedure SetAllowCookies(const AValue: boolean); virtual;
    function GetHandleRedirects: boolean; virtual;
    procedure SetHandleRedirects(const AValue: boolean); virtual;
    function GetAcceptCharset: string; virtual;
    procedure SetAcceptCharset(const AValue: string); virtual;
    function GetAcceptEncoding: string; virtual;
    procedure SetAcceptEncoding(const AValue: string); virtual;
    function GetContentType: string; virtual;
    procedure SetContentType(const AValue: string); virtual;
    function GetHttpClient: TRESTHTTP; virtual;
    procedure SetSynchronizedEvents(const AValue: boolean); virtual;
  public
    /// <summary>
    /// Instantiates a client to the given REST API service.
    /// </summary>
    /// <param name="ABaseApiURL">
    /// This is the base URL for the REST API. E.g.: http://your.server.com/service/api/
    /// </param>
    constructor Create(const ABaseApiURL: string); reintroduce; overload;
    /// <summary>
    /// Instantiates a client to the given REST API service, with no BaseURL being set.
    /// </summary>
    constructor Create(AOwner: TComponent); overload; override;
    destructor Destroy; override;

    /// <summary>
    /// Adds a HTTP parameter to the request. If a parameter of the same name already exists, then the previous parameter
    /// will be removed and freed. (QueryString for GET, DELETE, OPTIONS and HEAD; Encoded form for POST and PUT)
    /// </summary>
    /// <param name="AName">Name of the parameter</param>
    /// <param name="AValue">Value of the parameter</param>
    procedure AddParameter(const AName, AValue: string); overload; virtual;

    /// <summary>
    /// Adds a parameter to the request. If a parameter of the same name already exists, then the previous parameter
    /// will be removed and freed. There are five kinds of parameters:
    /// - GetOrPost: Either a QueryString value or encoded form value based on method
    /// - HttpHeader: Adds the name/value pair to the HTTP request's Headers collection
    /// - UrlSegment: Inserted into URL if there is a matching url token e.g. {AccountId}
    /// - Cookie: Adds the name/value pair to the HTTP request's Cookies collection
    /// - RequestBody: Used by AddBody() (not recommended to use directly)
    /// </summary>
    /// <param name="AName">Name of the parameter</param>
    /// <param name="AValue">Value of the parameter</param>
    /// <param name="AKind">The kind of parameter to add</param>
    procedure AddParameter(const AName, AValue: string; const AKind: TRESTRequestParameterKind); overload; virtual;

    /// <summary>
    /// Adds a parameter to the request. If a parameter of the same name already exists, then the previous parameter
    /// will be removed and freed.<br /><br />There are five kinds of parameters: - GetOrPost: Either a QueryString
    /// value or encoded form value based on method - HttpHeader: Adds the name/value pair to the HTTP request's
    /// Headers collection - UrlSegment: Inserted into URL if there is a matching url token e.g. {AccountId} -
    /// Cookie: Adds the name/value pair to the HTTP request's Cookies collection - RequestBody: Used by AddBody()
    /// (not recommended to use directly)
    /// </summary>
    /// <param name="AName">
    /// Name of the parameter
    /// </param>
    /// <param name="AValue">
    /// Value of the parameter
    /// </param>
    /// <param name="AKind">
    /// The kind of parameter to add
    /// </param>
    /// <param name="AOptions">
    /// Options for processing the parameter
    /// </param>
    procedure AddParameter(const AName, AValue: string; const AKind: TRESTRequestParameterKind;
      const AOptions: TRESTRequestParameterOptions); overload; virtual;

    /// <summary>
    /// Add an authentication parameter. Auth-params are always transient.
    /// </summary>
    procedure AddAuthParameter(const AName, AValue: string; const AKind: TRESTRequestParameterKind;
      const AOptions: TRESTRequestParameterOptions = []); overload; virtual;

    procedure SetCookie(const Data, CookieURL: string);
    procedure SetHTTPHeader(const AName, AValue: string);

    function GetEntity<T: class, constructor>(const AResource: string): T;
    /// <summary>
    /// Sends a GET request and tries to map the result to a List of instances
    /// of class &lt;T&gt;
    /// </summary>
    function GetEntityList<T: class, constructor>(const AResource: string): TObjectList<T>;
    /// <summary>
    /// Sends a GET request and tries to map the result to an Array of instances
    /// of class &lt;T&gt; This function calls GetEntityList internally.
    /// </summary>
    function GetEntityArray<T: class, constructor>(const AQuery: string): TArray<T>;
    /// <summary>
    /// Sends a POST request to "insert" an AEntity to the service resources
    /// </summary>
    function PostEntity<T: class, constructor>(const AResource: string; AEntity: T): TJSONObject;

    /// <summary>
    /// Resets the RESTClient to default values and clears all entries. The internal
    ///  HTTPClient will also be recreated.
    /// </summary>
    procedure ResetToDefaults; virtual;

    procedure Disconnect;

    property HTTPClient: TRESTHTTP read GetHttpClient;

    /// <summary>
    /// Transient Params are typically created and re-created by
    /// Authenticators. They are not persistent and will not be visible at
    /// designtime.
    /// In case of a naming-conflict any transient param will override a
    /// normal parameter.
    /// </summary>
    property TransientParams: TRESTRequestParameterList read FTransientParams;

    /// <summary>
    /// Controls the automatic creation of request-params of type "url-segment" only. no
    /// other param-types are afftected.
    /// </summary>
    property AutoCreateParams: boolean read FAutoCreateParams write SetAutoCreateParams default true;

    /// <summary>
    /// Specifies a to be used Authenticator. If left empty, no specific authentication will be used.
    /// </summary>
    /// <example>
    /// <para>
    /// LClient := TCustomRESTClient.Create('http://www.example.com');<br />//This is a professional password on a closed
    /// course. Do not try this at home!<br />LBasicAuth := THTTPBasicAuthenticator.Create('JohnDoe', 'secret');
    /// </para>
    /// <para>
    /// // RestClient owns its Authenticator through an Interface reference, so technically you do not need to
    /// destroy<br /> // an Authenticator on its own.<br />LClient.Authenticator := LBasicAuth;
    /// </para>
    /// </example>
    // <seealso cref="TSimpleAuthenticator">
    // Simple Authentication
    // </seealso>
    // <seealso cref="THTTPBasicAuthenticator">
    // Basic Authentication
    // </seealso>
    // <seealso cref="TOAuth1Authenticator">
    // OAuth1 Authentication
    // </seealso>
    // <seealso cref="TOAuth2Authenticator">
    // OAuth2 Authentication
    // </seealso>
    property Authenticator: TCustomAuthenticator read GetAuthenticator write SetAuthenticator;

    property Accept: string read GetAccept write SetAccept;
    property AcceptCharset: string read GetAcceptCharset write SetAcceptCharset;
    property AcceptEncoding: string read GetAcceptEncoding write SetAcceptEncoding;
    property AllowCookies: boolean read GetAllowCookies write SetAllowCookies default True;

    /// <summary>
    /// Base URL for all API calls. All request resources and parameters will be appended to this URL
    /// </summary>
    /// <remarks>
    /// Setting the BaseURL ensures, that a possibly trailing slash is removed.
    /// </remarks>
    /// <example>
    /// https://api.twitter.com/
    /// </example>
    property BaseURL: string read GetBaseURL write SetBaseURL;

    /// <summary>
    /// ContentType for a POST or PUT requests. This property will be used internally while TRESTClient.Execute.
    /// </summary>
    property ContentType: string read GetContentType write SetContentType;

    /// <summary>
    /// <para>
    /// Specifies a character encoding for strings, which will be used if the server does not specify one.
    /// </para>
    /// <para>
    /// RFC2616 technically specifies ISO-8859-1 as default character, but
    /// <see href="http://www.w3.org/TR/html4/charset.html" /> section 5.2.2 clearly says that this specification
    /// has proven useless.
    /// </para>
    /// <para>
    /// There are some (many?) servers/APIs that actually use UTF-8, but "forget" to specify that in their
    /// responses. As UTF-8 is a defacto-standard for Web/HTPP anyway, 'UTF-8' is used as default here.
    /// </para>
    /// </summary>
    property FallbackCharsetEncoding: string read GetFallbackCharsetEncoding write SetFallbackCharsetEncoding stored FallbackCharsetEncodingIsStored;

    /// <summary>
    /// Container of all HTTP parameters to be passed with each request.
    /// See Params.Add() for explanation of the types of parameters that can be passed.
    /// Priority of Params is as follows - highest priority first:
    /// 1 : Transient Parameters
    /// 2 : Parameters supplied by the RESTRequest
    /// 3 : Parameters supplied by the RESTClient
    /// Example: if there are two parameters with the same name, one transient, the other
    /// </summary>
    property Params: TRESTRequestParameterList read GetParams write SetParams;

    property HandleRedirects: boolean read GetHandleRedirects write SetHandleRedirects;

    /// <summary>
    /// Password for proxy authentication (optional)
    /// </summary>
    property ProxyPassword: string read GetProxyPassword write SetProxyPassword;
    /// <summary>
    /// Port for HTTP proxy server.
    /// </summary>
    /// <value>
    /// Will be ignored if 0
    /// </value>
    property ProxyPort: integer read GetProxyPort write SetProxyPort default 0;
    /// <summary>
    /// Server name for proxy server. Specify a fully qualified domain name or IP address. Proxy settings will be
    /// ignored if left empty.
    /// </summary>
    property ProxyServer: string read GetProxyServer write SetProxyServer;
    /// <summary>
    /// User name for proxy authentication (optional).
    /// </summary>
    property ProxyUsername: string read GetProxyUsername write SetProxyUsername;

    /// <summary>
    /// Specifies if an Exception should be raised if a 500 Protocol exception occurs. Defaults to TRUE;
    /// </summary>
    property RaiseExceptionOn500: boolean read FRaiseExceptionOn500 write FRaiseExceptionOn500 default true;
    /// <summary>
    /// Specifies if Events (such as OnAfterExecute or OnHTTPProtocolError) should run in the context of the main
    /// thread (true) or in the context of an arbitrary thread - which was created by the developer or by using
    /// ExecuteAsync.
    /// </summary>
    /// <seealso cref="TCustomRESTRequest.ExecuteAsync" />

    property SynchronizedEvents: boolean read FSynchronizedEvents write SetSynchronizedEvents default true;
    /// <summary>
    /// Defaults to 'Enbaracdero RESTClient/{version}'
    /// </summary>
    /// <remarks>
    /// See http://tools.ietf.org/html/rfc2616#section-14.43
    /// </remarks>
    property UserAgent: string read GetUserAgent write SetUserAgent stored UserAgentIsStored;
    /// <summary>
    /// <para>
    /// OnHTTPProtocolError gets fired when the server returns a status code other than 200 - OK
    /// </para>
    /// <para>
    /// Check AClient.Response.StatusCode for the actual status.
    /// </para>
    /// <para>
    /// This event will not get fired if a non HTTP-related exception occurs during execution. This includes, but
    /// is not limited to timeout and server not found related exceptions.
    /// </para>
    /// </summary>
    property OnHTTPProtocolError: TCustomRESTClientNotifyEvent read FOnHTTPProtocolError write FOnHTTPProtocolError;
      /// <summary>
    /// HTTP abstraction implementation ID
    /// </summary>
    property IPImplementationID: string read FIPImplementationID write SetIPImplementationID;
    property BindSource: TSubRESTClientBindSource read FBindSource;
  end;

  /// <summary>
  /// LiveBindings adapter for TCustomRESTClient. Create bindable members
  /// </summary>
  TRESTClientAdapter = class(TRESTComponentAdapter)
  public type
    TNotify = class(TCustomRESTClient.TNotify)
    private
      FAdapter: TRESTClientAdapter;
      constructor Create(const AAdapter: TRESTClientAdapter);
    public
      procedure ParameterListChanged(Sender: TObject); override;
      procedure PropertyValueChanged(Sender: TObject); override;
    end;
  private
    FClient: TCustomRESTClient;
    FNotify: TNotify;
    procedure SetClient(const AClient: TCustomRESTClient);
    procedure AddParameterFields;
    procedure ParameterListChanged;
    procedure AddPropertyFields;
  protected
    procedure DoChangePosting; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetCanActivate: boolean; override;
    procedure AddFields; override;
    function GetSource: TBaseLinkingBindSource; override;
  public
    constructor Create(AComponent: TComponent); override;
    destructor Destroy; override;
    procedure GetMemberNames(AList: TStrings); override;
    property Client: TCustomRESTClient read FClient write SetClient;
  end;

  /// <summary>
  /// LiveBindings bindsource for TCustomRESTResponse. Creates adapter
  /// </summary>
  TCustomRESTClientBindSource = class(TRESTComponentBindSource)
  private
    FAdapter: TRESTClientAdapter;
    function GetClient: TCustomRESTClient;
    procedure SetClient(const AValue: TCustomRESTClient);
  protected
    function CreateAdapter: TRESTComponentAdapter; override;
  public
    property Client: TCustomRESTClient read GetClient write SetClient;
    property Adapter: TRESTClientAdapter read FAdapter;
  end;

  /// <summary>
  /// LiveBindings bindsource for TCustomRESTClient. Publishes subcomponent properties
  /// </summary>
  TSubRESTClientBindSource = class(TCustomRESTClientBindSource)
  published
    property AutoActivate;
    property AutoEdit;
    property AutoPost;
  end;

  TRESTClient = class(TCustomRESTClient)
  published
    property Authenticator;
    property Accept;
    property AcceptCharset;
    property AcceptEncoding;
    property AllowCookies;
    property AutoCreateParams;
    property BaseURL;
    property ContentType;
    property FallbackCharsetEncoding;
    property Params;
    property HandleRedirects;
    property ProxyPassword;
    property ProxyPort;
    property ProxyServer;
    property ProxyUsername;
    property RaiseExceptionOn500;
    property SynchronizedEvents;
    property UserAgent;
    property OnHTTPProtocolError;
    property IPImplementationID;
    property BindSource;
  end;

  TAuthenticateEvent = procedure(ARequest: TCustomRESTRequest; var ADone: boolean) of object;

  /// <summary>
  /// Base class for all authenticators authenticators are attached to the rest-clients
  /// </summary>
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice or pidAndroid)]
  TCustomAuthenticator = class(TBaseObjectBindSourceDelegate, IAuthenticator)
  protected type
    TNotify = TRESTComponentNotify;
    TNotifyList = TRESTComponentNotifyList<TNotify>;
  private
    FNotifyList: TNotifyList;
    FOnAuthenticate: TAuthenticateEvent;
  protected
    procedure PropertyValueChanged; virtual;

    property NotifyList: TNotifyList read FNotifyList;

    /// <summary>
    /// does the actual authentication by extending the request with parameters and data as needed
    /// </summary>
    procedure DoAuthenticate(ARequest: TCustomRESTRequest); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Authenticate(ARequest: TCustomRESTRequest); virtual;
    procedure ResetToDefaults; virtual;
  published
    property OnAuthenticate: TAuthenticateEvent read FOnAuthenticate write FOnAuthenticate;
  end;

  /// <summary>
  /// LiveBindings adapter for TCustomAuthenticator.  Descendents add bindable members.
  /// </summary>
  TRESTAuthenticatorAdapter<T: TCustomAuthenticator> = class(TRESTComponentAdapter)
  private
    FAuthenticator: T;
    procedure SetAuthenticator(const AAuthenticator: T);
  protected type
    TNotify = TRESTComponentAdapter.TNotify;
  private
    FNotify: TNotify;
  protected
    function GetSource: TBaseLinkingBindSource; override;
    procedure DoAuthenticatorChanging; virtual;
    procedure DoAuthenticatorChanged; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetCanActivate: boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Authenticator: T read FAuthenticator write SetAuthenticator;
  end;

  /// <summary>
  /// LiveBindings bindsource for TCustomAuthenticator.  Creates adapter.
  /// </summary>
  TRESTAuthenticatorBindSource<T: TCustomAuthenticator> = class(TRESTComponentBindSource)
  private
    FAdapter: TRESTAuthenticatorAdapter<T>;
    function GetAuthenticator: TCustomAuthenticator;
    procedure SetAuthenticator(const Value: TCustomAuthenticator);
  protected
    function CreateAdapter: TRESTComponentAdapter; override;
    function CreateAdapterT: TRESTAuthenticatorAdapter<T>; virtual;
  public
    property Authenticator: TCustomAuthenticator read GetAuthenticator write SetAuthenticator;
    property Adapter: TRESTAuthenticatorAdapter<T> read FAdapter;
  published
    property AutoActivate;
    property AutoEdit;
    property AutoPost;
  end;

function RESTFindDefaultRequest(AComp: TComponent): TCustomRESTRequest;
function RESTFindDefaultResponse(AComp: TComponent): TCustomRESTResponse;
function RESTFindDefaultClient(AComp: TComponent): TCustomRESTClient;
function RESTFindDefaultAuthenticator(AComp: TComponent): TCustomAuthenticator;

implementation

uses
{$IFDEF MACOS}
  Macapi.CoreFoundation,
{$ENDIF}
  System.Math,
  System.Types,
  IPPeerAPI,
  REST.Json,
  REST.Exception,
  REST.Utils,
  Data.Bind.Json; // JSON LiveBindings converters

function RESTFindDefaultRequest(AComp: TComponent): TCustomRESTRequest;
var
  LRoot: TComponent;
  i: integer;
begin
  result := nil;
  LRoot := AComp;
  while (LRoot.Owner <> nil) and (result = nil) do
  begin
    LRoot := LRoot.Owner;
    for i := 0 to LRoot.ComponentCount - 1 do
      if LRoot.Components[i] is TCustomRESTRequest then
      begin
        result := TCustomRESTRequest(LRoot.Components[i]);
        Break;
      end;
  end;
end;

function RESTFindDefaultResponse(AComp: TComponent): TCustomRESTResponse;
var
  LRoot: TComponent;
  i: integer;
begin
  result := nil;
  LRoot := AComp;
  while (LRoot.Owner <> nil) and (result = nil) do
  begin
    LRoot := LRoot.Owner;
    for i := 0 to LRoot.ComponentCount - 1 do
      if LRoot.Components[i] is TCustomRESTResponse then
      begin
        result := TCustomRESTResponse(LRoot.Components[i]);
        Break;
      end;
  end;
end;

function RESTFindDefaultClient(AComp: TComponent): TCustomRESTClient;
var
  LRoot: TComponent;
  i: integer;
begin
  result := nil;
  LRoot := AComp;
  while (LRoot.Owner <> nil) and (result = nil) do
  begin
    LRoot := LRoot.Owner;
    for i := 0 to LRoot.ComponentCount - 1 do
      if LRoot.Components[i] is TCustomRESTClient then
      begin
        result := TCustomRESTClient(LRoot.Components[i]);
        Break;
      end;
  end;
end;

function RESTFindDefaultAuthenticator(AComp: TComponent): TCustomAuthenticator;
var
  LRoot: TComponent;
  i: integer;
begin
  result := nil;
  LRoot := AComp;
  while (LRoot.Owner <> nil) and (result = nil) do
  begin
    LRoot := LRoot.Owner;
    for i := 0 to LRoot.ComponentCount - 1 do
      if LRoot.Components[i] is TCustomAuthenticator then
      begin
        result := TCustomAuthenticator(LRoot.Components[i]);
        Break;
      end;
  end;
end;

/// <summary>
/// Iterates through all components of the same owner (typically a form)
/// and looks for TRESTRequests having no response assigned. if found,
/// the new component is just assigned as response.
/// </summary>
procedure AssignResponseToRESTRequests(AResponse: TCustomRESTResponse);
var
  LRoot: TComponent;
  i: integer;
begin
  LRoot := AResponse;
  while (LRoot.Owner <> nil) do
  begin
    LRoot := LRoot.Owner;
    for i := 0 to LRoot.ComponentCount - 1 do
      if (LRoot.Components[i] is TCustomRESTRequest) then
      begin
        if (TCustomRESTRequest(LRoot.Components[i]).Response = nil) then
          TCustomRESTRequest(LRoot.Components[i]).Response := AResponse;
      end;
  end;
end;

/// <summary>
/// Iterates through all components of the same owner (typically a form)
/// and looks for TRESTRequests having no client assigned. if found, the
/// new component is just assigned as client.
/// </summary>
procedure AssignClientToRESTRequests(AClient: TCustomRESTClient);
var
  LRoot: TComponent;
  i: integer;
begin
  LRoot := AClient;
  while (LRoot.Owner <> nil) do
  begin
    LRoot := LRoot.Owner;
    for i := 0 to LRoot.ComponentCount - 1 do
      if (LRoot.Components[i] is TCustomRESTRequest) then
      begin
        if (TCustomRESTRequest(LRoot.Components[i]).Client = nil) then
          TCustomRESTRequest(LRoot.Components[i]).Client := AClient;
      end;
  end;
end;

/// <summary>
/// Iterates through all components of the same owner (typically a form)
/// and looks for TRESTClients having no authenticator assigned. if found,
/// the new component is just assigned as authenticator.
/// </summary>
procedure AssignAuthenticatorToRESTClients(AAuthenticator: TCustomAuthenticator);
var
  LRoot: TComponent;
  i: integer;
begin
  LRoot := AAuthenticator;
  while (LRoot.Owner <> nil) do
  begin
    LRoot := LRoot.Owner;
    for i := 0 to LRoot.ComponentCount - 1 do
      if (LRoot.Components[i] is TCustomRESTClient) then
      begin
        if (TCustomRESTClient(LRoot.Components[i]).Authenticator = nil) then
          TCustomRESTClient(LRoot.Components[i]).Authenticator := AAuthenticator;
      end;
  end;
end;

{ TRESTRequestParameter }

procedure TRESTRequestParameter.Assign(ASource: TPersistent);
var
  LParam: TRESTRequestParameter;
begin
  if (ASource is TRESTRequestParameter) then
  begin
    LParam := TRESTRequestParameter(ASource);

    FName := LParam.Name;
    FValue := LParam.Value;
    FKind := LParam.Kind;
    FOptions := LParam.Options;
    FContentType := LParam.ContentType;
  end
  else
    inherited;
end;

constructor TRESTRequestParameter.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);

  FName := '';
  FValue := '';
  FKind := DefaulTRESTRequestParameterKind;
  FOptions := [];
  FContentType := DefaultRESTContentType;
end;

function TRESTRequestParameter.GetDisplayName: string;
begin
  Result := FName;
end;

function TRESTRequestParameter.KindIsStored: Boolean;
begin
  Result := Kind <> DefaultRESTRequestParameterKind;
end;

procedure TRESTRequestParameter.SetContentType(const AValue: TRESTContentType);
begin
  if FContentType <> AValue then
  begin
    FContentType := AValue;
    Changed(FALSE);
  end;
end;

procedure TRESTRequestParameter.SetKind(const AValue: TRESTRequestParameterKind);
begin
  if FKind <> AValue then
  begin
    FKind := AValue;
    Changed(FALSE);
  end;
end;

procedure TRESTRequestParameter.SetName(const AValue: string);
var
  notify: TRESTRequestParameterListOwnerNotify;
begin
  if FName <> AValue then
  begin
    FName := AValue;
    Changed(FALSE);

    if (self.Collection is TRESTRequestParameterList) and (self.Collection.Owner <> nil) then
    begin
      if TRESTREquestParameterList(Self.Collection).UpdateCount = 0 then
        if Supports(self.Collection.Owner, TRESTRequestParameterListOwnerNotify, notify) then
          notify.ParameterListChanged;
    end;
  end;
end;

procedure TRESTRequestParameter.SetOptions(const AValue: TRESTRequestParameterOptions);
begin
  if FOptions <> AValue then
  begin
    FOptions := AValue;
    Changed(FALSE);
  end;
end;

procedure TRESTRequestParameter.SetValue(const AValue: string);
var
  notify: TRESTRequestParameterListOwnerNotify;
begin
  if FValue <> AValue then
  begin
    FValue := AValue;
    Changed(FALSE);

    if (self.Collection is TRESTRequestParameterList) and (self.Collection.Owner <> nil) then
    begin
      if Supports(self.Collection.Owner, TRESTRequestParameterListOwnerNotify, notify) then
        notify.ParameterValueChanged;
    end;
  end;
end;

function TRESTRequestParameter.ToString: string;
begin
  result := Format('[%s] %s=%s', [RESTRequestParameterKindToString(FKind), FName, FValue])
end;

{ TRESTRequestParameterList }

constructor TRESTRequestParameterList.Create(const AOwner: TComponent);
begin
  inherited Create(AOwner, TRESTRequestParameter);
end;

procedure TRESTRequestParameterList.Delete(const AParam: TRESTRequestParameter);
var
  idx: integer;
begin
  idx := self.IndexOf(AParam);
  if (idx > -1) and (idx < Count) then
    self.Delete(idx);
end;

procedure TRESTRequestParameterList.Delete(const AName: string);
begin
  self.Delete(ParameterByName(AName));
end;

destructor TRESTRequestParameterList.Destroy;
begin

  inherited;
end;

function TRESTRequestParameterList.FromString(const OldValue, NewValue: string): boolean;
var
  LOldParams, LNewParams: TStringList;
  LName: string;
  LParam: TRESTRequestParameter;
  i: integer;
begin
  result := FALSE;
  LOldParams := nil;
  LNewParams := nil;
  try
    LOldParams := TStringList.Create;
    LNewParams := TStringList.Create;
    ExtractURLSegmentNames(OldValue, LOldParams);
    ExtractURLSegmentNames(NewValue, LNewParams);

    /// first, delete unmatched parameters - parameters that
    /// were defined here in the collection but are not
    /// part of the new url-string any more
    for i := self.Count - 1 downto 0 do
    begin
      LParam := self.Items[i];
      /// we only process autocreated params of type url-segment
      if (LParam.Kind = TRESTRequestParameterKind.pkURLSEGMENT) and
        (TRESTRequestParameterOption.poAutoCreated in LParam.Options) then
      begin
        if (LNewParams.IndexOf(LParam.Name) < 0) then
        begin
          if not result then
            self.BeginUpdate;
          self.Delete(LParam);
          result := true; // return true to indicate changes made
        end;
      end;
    end;

    /// now add new parameters but without overriding existing
    /// params in case of naming-conflicts
    for LName in LNewParams do
    begin
      if (LName <> '') and not self.ContainsParameter(LName) then
      begin
        if not result then
          self.BeginUpdate;
        LParam := self.AddItem;
        LParam.Name := LName;
        LParam.Kind := TRESTRequestParameterKind.pkURLSEGMENT;
        LParam.Options := LParam.Options + [TRESTRequestParameterOption.poAutoCreated];

        result := true; // return true to indicate changes made
      end;
    end;
  finally
    if result then
      self.EndUpdate;
    FreeAndNIL(LOldParams);
    FreeAndNIL(LNewParams);
  end;
end;

function TRESTRequestParameterList.GetAttr(Index: integer): string;
begin
  case index of
    0:
      result := sParameterName;
    1:
      result := sParameterValue;
    2:
      result := sParameterKind;
  else
    result := ''; { do not localize }
  end;
end;

function TRESTRequestParameterList.GetAttrCount: integer;
begin
  result := 3;
end;

function TRESTRequestParameterList.GetEnumerator: TEnumerator;
begin
  result := TEnumerator.Create(self);
end;

function TRESTRequestParameterList.GetItem(AIndex: integer): TRESTRequestParameter;
begin
  result := TRESTRequestParameter(inherited Items[AIndex]);
end;

function TRESTRequestParameterList.GetItemAttr(Index, ItemIndex: integer): string;
begin
  case index of
    0:
      begin
        result := Items[ItemIndex].Name;
        if result = '' then
          result := IntToStr(ItemIndex);
      end;
    1:
      result := Items[ItemIndex].Value;
    2:
      result := System.TypInfo.GetEnumName(TypeInfo(TRESTRequestParameterKind), integer(Items[ItemIndex].Kind));
  else
    result := '';
  end;
end;

function TRESTRequestParameterList.IndexOf(const AName: string): integer;
var
  i: integer;
  LName: string;
begin
  result := -1;
  if AName = '' then
    EXIT;

  LName := AName.ToLower;

  for i := 0 to Count - 1 do
  begin
    if (Items[i].Name.ToLower = LName) then
    begin
      result := i;
      Break;
    end;
  end;
end;

function TRESTRequestParameterList.IndexOf(const AParam: TRESTRequestParameter): integer;
var
  i: integer;
begin
  result := -1;
  if not Assigned(AParam) then
    EXIT;

  for i := 0 to Count - 1 do
  begin
    if (AParam = Items[i]) then
    begin
      result := i;
      Break;
    end;
  end;
end;

function TRESTRequestParameterList.AddCookie(const AName, AValue: string): TRESTRequestParameter;
begin
  result := self.AddItem(AName, AValue, TRESTRequestParameterKind.pkCOOKIE);
end;

function TRESTRequestParameterList.AddHeader(const AName, AValue: string): TRESTRequestParameter;
begin
  result := self.AddItem(AName, AValue, TRESTRequestParameterKind.pkHTTPHEADER);
end;

function TRESTRequestParameterList.AddItem: TRESTRequestParameter;
begin
  result := Add as TRESTRequestParameter;
end;

procedure TRESTRequestParameterList.AddObject(AObject: TObject);
var
  LContext: TRttiContext;
  LType: TRttiType;
  LProperty: TRttiProperty;
begin
  LContext := TRttiContext.Create;
  LType := LContext.GetType(AObject.ClassType);
  for LProperty in LType.GetProperties do
  begin
    if LProperty.Visibility in [TMemberVisibility.mvPublic, TMemberVisibility.mvPublished] then
    begin
      AddItem(LProperty.Name, LProperty.GetValue(AObject).ToString);
    end;
  end;
end;

procedure TRESTRequestParameterList.AddObject(AObject: TObject; WhiteList: TStrings);
var
  LContext: TRttiContext;
  LType: TRttiType;
  LProperty: TRttiProperty;
begin
  LContext := TRttiContext.Create;
  LType := LContext.GetType(AObject.ClassType);
  for LProperty in LType.GetProperties do
  begin
    if LProperty.Visibility in [TMemberVisibility.mvPublic, TMemberVisibility.mvPublished] then
    begin
      if WhiteList.IndexOf(LProperty.Name) >= 0 then
      begin
        AddItem(LProperty.Name, LProperty.GetValue(AObject).ToString);
      end;
    end;
  end;
end;

function TRESTRequestParameterList.AddItem(const AName, AValue: string): TRESTRequestParameter;
begin
  result := self.AddItem(AName, AValue, TRESTRequestParameterKind.pkGETorPOST);
end;

function TRESTRequestParameterList.AddItem(const AName, AValue: string; AKind: TRESTRequestParameterKind;
  AOptions: TRESTRequestParameterOptions): TRESTRequestParameter;
begin
  result := AddItem(AName, AValue, AKind, AOptions, DefaultRESTContentType);
end;

function TRESTRequestParameterList.AddItem(const AName, AValue: string; const AKind: TRESTRequestParameterKind;
  const AOptions: TRESTRequestParameterOptions; AContentType: TRESTContentType): TRESTRequestParameter;
var
  i: integer;
begin
  self.BeginUpdate;
  try
    i := IndexOf(AName);
    if i = -1 then
      result := Add as TRESTRequestParameter
    else
      result := Items[i];
    result.Name := AName;
    result.Value := AValue;
    result.Kind := AKind;
    result.Options := AOptions;
    result.ContentType := AContentType;
  finally
    self.EndUpdate;
  end;
end;

procedure TRESTRequestParameterList.AddUrlSegment(const AName, AValue: string);
begin
  self.AddItem(AName, AValue, TRESTRequestParameterKind.pkURLSEGMENT);
end;

function TRESTRequestParameterList.ParameterByIndex(AIndex: integer): TRESTRequestParameter;
begin
  result := Items[AIndex];
end;

function TRESTRequestParameterList.ParameterByName(AName: string): TRESTRequestParameter;
var
  LParam: TRESTRequestParameter;
begin
  result := nil;
  for LParam in self do
  begin
    if SameText(LParam.Name, AName) then
    begin
      result := LParam;
      Break;
    end;
  end;
end;

procedure TRESTRequestParameterList.SetItem(AIndex: integer; const AValue: TRESTRequestParameter);
begin
  inherited SetItem(AIndex, TCollectionItem(AValue));
end;

procedure TRESTRequestParameterList.Update(AItem: TCollectionItem);
var
  notify: TRESTRequestParameterListOwnerNotify;
begin
  inherited;

  if (self.Owner <> nil) then
  begin
    if AItem = nil then  // Entire list changed
      if Supports(self.Owner, TRESTRequestParameterListOwnerNotify, notify) then
        notify.ParameterListChanged;
  end;
end;

function TRESTRequestParameterList.ContainsParameter(const AName: string): boolean;
begin
  result := ParameterByName(AName) <> nil;
end;

{ TRESTRequestParameterList.TEnumerator }

function TRESTRequestParameterList.TEnumerator.GetCurrent: TRESTRequestParameter;
begin
  result := TRESTRequestParameter(inherited GetCurrent);
end;

{ TRESTExceutionThread }
constructor TRESTExecutionThread.Create(AExecuteMethod: TExecuteMethod; ARequest: TCustomRESTRequest;
  ACompletionHandler: TCompletionHandler; ASynchronized: boolean; AFreeThread: boolean = true);
begin
  // In older versions, before "Resume" was deprecated and "Start" had been introduced,
  // you would call inherited Create(true) to prevent the thread to be started right away.
  // In current versions it is ensured that the thread will start AFTER the constructor has run.

  inherited Create(FALSE);
  FreeOnTerminate := AFreeThread;
  FCompletionHandler := ACompletionHandler;
  FExecuteMethod := AExecuteMethod;
  FSynchronized := ASynchronized;
  FRequest := ARequest;
end;

procedure TRESTExecutionThread.Execute;
begin
  try
    FExecuteMethod;
    if FSynchronized then
    begin
      Synchronize(HandleCompletion); // Just synchronize. No need to Queue
    end
    else
    begin
      HandleCompletion;
    end;
  except
                              
  end;
end;

procedure TRESTExecutionThread.HandleCompletion;
begin
  if Assigned(FCompletionHandler) then
  begin
    FCompletionHandler;
  end;
  FRequest.DoAfterExecute;
end;

{ TCustomRESTRequest }
constructor TCustomRESTRequest.Create(AOwner: TComponent);
begin
  /// it is important to create the notify-list before
  /// calling the inherited constructor
  FNotifyList := TNotifyList.Create;
  inherited;

  /// if we do get a client as owner, we just use
  /// it. otherwise we look around and try to find
  /// an existing client (maybe on the same form)
  if (AOwner is TCustomRESTClient) then
    Client := (AOwner as TCustomRESTClient)
  else if RESTComponentIsDesigning(self) then
    Client := RESTFindDefaultClient(self);

  if (AOwner is TCustomRESTResponse) then
    Response := (AOwner as TCustomRESTResponse)
  else if RESTComponentIsDesigning(self) then
    Response := RESTFindDefaultResponse(self);

  FOnAfterExecute := nil;
  FOnHTTPProtocolError := nil;

  FAutoCreateParams := true;
  FParams := TRESTRequestParameterList.Create(self);
  FTransientParams := TRESTRequestParameterList.Create(self);
  ResetToDefaults;
end;

function TCustomRESTRequest.CreateBindSource: TBaseObjectBindSource;
begin
  FBindSource := TSubRESTRequestBindSource.Create(self);
  FBindSource.Name := 'BindSource'; { Do not localize }
  FBindSource.SetSubComponent(true);
  FBindSource.Request := self;

  result := FBindSource;
end;

destructor TCustomRESTRequest.Destroy;
begin
  FreeAndNIL(FNotifyList);
  FResponse := nil;
  FClient := nil;
  FreeAndNIL(FParams);
  FreeAndNIL(FTransientParams);
  inherited;
end;

procedure TCustomRESTRequest.DoAfterExecute;
begin
  if Assigned(FOnAfterExecute) then
    FOnAfterExecute(self);
end;

procedure TCustomRESTRequest.DoApplyCookieParams(const AParamList: TRESTRequestParameterArray; const CookieURL: string);
var
  LParameter: TRESTRequestParameter;
  LName: string;
  LValue: string;
begin
  for LParameter in AParamList do
  begin
    if (LParameter.Kind = TRESTRequestParameterKind.pkCOOKIE) then
    begin

      if (TRESTRequestParameterOption.poDoNotEncode in LParameter.Options) then
      begin
        LName := LParameter.Name;
        LValue := LParameter.Value;
      end
      else
      begin
        LName := URIEncode(LParameter.Name);
        LValue := URIEncode(LParameter.Value);
      end;
      FClient.SetCookie(LName + '=' + LValue, CookieURL);
    end;
  end;
end;

procedure TCustomRESTRequest.DoApplyHeaderParams(const AParamList: TRESTRequestParameterArray);
var
  LParameter: TRESTRequestParameter;
  LValue: string;
  LName: string;
begin
  for LParameter in AParamList do
  begin
    if (LParameter.Kind = TRESTRequestParameterKind.pkHTTPHEADER) then
    begin

      if (TRESTRequestParameterOption.poDoNotEncode in LParameter.Options) then
      begin
        LName := LParameter.Name;
        LValue := LParameter.Value;
      end
      else
      begin
        LName := URIEncode(LParameter.Name);
        LValue := URIEncode(LParameter.Value);
      end;
      FClient.SetHTTPHeader(LName, LValue);
    end;
  end;
end;

procedure TCustomRESTRequest.DoApplyURLSegments(const AParamList: TRESTRequestParameterArray; var AURL: string);
var
  LParameter: TRESTRequestParameter;
begin
  for LParameter in AParamList do
  begin
    if (LParameter.Kind = TRESTRequestParameterKind.pkURLSEGMENT) then
    begin
      AURL := StringReplace(AURL, '{' + LParameter.Name + '}', LParameter.Value,
        [rfReplaceAll, rfIgnoreCase]);
    end;
  end;
end;

procedure TCustomRESTRequest.DoHTTPProtocolError;
begin
  if Assigned(FOnHTTPProtocolError) then
    FOnHTTPProtocolError(self);
end;

function TCustomRESTRequest.CreateUnionParameterList: TRESTRequestParameterArray;
var
  LList: TList<TRESTRequestParameter>;
  LParam: TRESTRequestParameter;
  i: integer;

  function IndexOfParam(const AParam: TRESTRequestParameter; const AList: TList<TRESTRequestParameter>): integer;
  var
    i: integer;
  begin
    result := -1;

    for i := 0 to AList.Count - 1 do
    begin
      if SameText(LParam.Name, LList[i].Name) then
      begin
        result := i;
        Break;
      end;
    end;
  end;

begin
  Setlength(result, 0);

  /// create the complete list of parameters for this request
  /// please note that we do have four sources:
  /// (1) a list of parameters belonging to the client
  /// (2) a list of TRANSIENT parameters belonging to the client
  /// (3) a list of parameters belonging to the request
  /// (4) a list of TRANSIENT parameters belonging to the request
  ///
  /// --> in case of a conflict,
  /// * the request overrides the client
  LList := TList<TRESTRequestParameter>.Create;
  try

    /// first, the default params from the client
    if Assigned(FClient) then
    begin
      for LParam in FClient.Params do
        LList.Add(LParam);
    end;

    /// next, our own default params, overriding any existing params
    for LParam in self.Params do
    begin
      i := IndexOfParam(LParam, LList);
      if (i > -1) then
      begin
        LList.Delete(i);
        LList.Insert(i, LParam);
      end
      else
        LList.Add(LParam);
    end;

    /// next, the transient params from the client, overriding any existing params
    if Assigned(FClient) then
    begin
      for LParam in FClient.TransientParams do
      begin
        i := IndexOfParam(LParam, LList);
        if (i > -1) then
        begin
          LList.Delete(i);
          LList.Insert(i, LParam);
        end
        else
          LList.Add(LParam);
      end;
    end;

    /// now our own transient params, overriding any existing params
    for LParam in self.TransientParams do
    begin
      i := IndexOfParam(LParam, LList);
      if (i > -1) then
      begin
        LList.Delete(i);
        LList.Insert(i, LParam);
      end
      else
        LList.Add(LParam);
    end;

    result := LList.ToArray;
  finally
    LList.Free;
  end;
end;

procedure TCustomRESTRequest.DoPrepareRequestBody(AParamList: TRESTRequestParameterArray; var ABodyStream: TStream);
var
  LContentType: TRESTContentType;
  LParam: TRESTRequestParameter;
  LParamString: string;
  LParamName: string;
  LParamValue: string;
  LMultipartPeerStream: IIPMultipartFormDataStream;
begin
  Assert(Assigned(Client));
  Assert(Assigned(AParamList));
  Assert(self.Method in [TRESTRequestMethod.rmPUT, TRESTRequestMethod.rmPOST]);

  LContentType := self.ContentType(AParamList);

  /// the goal is to embedd all relevant params into a stream

  case LContentType of
    TRESTContentType.ctAPPLICATION_X_WWW_FORM_URLENCODED:
      begin
        ABodyStream := TStringStream.Create;
      end;
    TRESTContentType.ctMULTIPART_FORM_DATA:
      begin
        LMultipartPeerStream := Client.HTTPClient.CreateMultipartFormStream;
        ABodyStream := LMultipartPeerStream.GetStream(FALSE);
      end;
  else
    begin
      ABodyStream := TStringStream.Create;
    end;
  end;

  for LParam in AParamList do
  begin
    if (LParam.Kind in [TRESTRequestParameterKind.pkGETorPOST, TRESTRequestParameterKind.pkREQUESTBODY]) then
    begin
      if LContentType = TRESTContentType.ctMULTIPART_FORM_DATA then
      begin
        // Multipart
        // For multipart names of body parameters are written - in contrast to WWWForm  (see below)
        LMultipartPeerStream.AddFormField(LParam.Name, LParam.Value);
      end
      else if LContentType = TRESTContentType.ctAPPLICATION_X_WWW_FORM_URLENCODED then
      begin
        // WWWForm
        // http://www.w3.org/MarkUp/html-spec/html-spec_8.html#SEC8.2.1
        // Both Key and Value are escaped and URL encoded - by default
        if not(TRESTRequestParameterOption.poDoNotEncode in LParam.Options) then
        begin
          LParamName := URIEncode(LParam.Name);
          LParamValue := URIEncode(LParam.Value);
        end
        else
        begin
          LParamName := LParam.Name;
          LParamValue := LParam.Value;
        end;
        // If this is a body parameter, then we are interested in its value only.
        // In contrast to multipart its name is ignored that is (see above)
        if LParam.Kind = TRESTRequestParameterKind.pkREQUESTBODY then
          LParamString := LParamValue
        else
          LParamString := LParamName + '=' + LParamValue;

        // Parameters are separated using an Apmersand,
        // so if there is already something the stream, we need a separator first
        if ABodyStream.Position > 0 then
        begin
          TStringStream(ABodyStream).WriteString('&');
        end;
        TStringStream(ABodyStream).WriteString(LParamString);
      end
      else
      begin
        // All others go in just as plain text
        TStringStream(ABodyStream).WriteString(LParam.Value);
      end;
    end;
  end;
end;

function TCustomRESTRequest.ExecuteAsync(ACompletionHandler: TCompletionHandler = nil; ASynchronized: boolean = true;
  AFreeThread: boolean = true): TRESTExecutionThread;
begin
  result := TRESTExecutionThread.Create(Execute, self, ACompletionHandler, ASynchronized, AFreeThread);
end;

procedure TCustomRESTRequest.Execute;
var
  LParamsList: TRESTRequestParameterArray;
  LURL: string;
  LResponseStream: TMemoryStream;
  LContentType: TRESTContentType;
  LBodyStream: TStream;
  LErrorBytes: TArray<System.Byte>;
  i: integer;
  S: string;
begin
  FExecutionPerformance.Start;

  // If no client then raise an exception
  if not Assigned(FClient) then
    raise ERESTException.Create('The given request-object is not assigned. Operation aborted.');

  /// If a response-object was attached to this response we will use it
  /// If no response exists yet, then one is created on the fly and will be managed
  /// otherwise we do create a new response-object and return a reference
  /// to it.
  if not Assigned(FResponse) then
    FResponse := TCustomRESTResponse.Create(self);

  FResponse.BeginUpdate;
  try
  // This might not be nesseccary, but shouldn't hurt as we do our own authentication anyway
  // Indy may set Request.Authentication when 40x.  This causes all subsequent requests to fail on 40x..
  FClient.HTTPClient.Request.Authentication := nil;

  if Assigned(FClient.Authenticator) then
  begin
    FClient.Authenticator.Authenticate(self);
  end;

  /// no we build the full request-url - excluding any
  /// query-params. they will be added a little later
  LURL := GetFullRequestURL(FALSE);

  LParamsList := CreateUnionParameterList;

  LContentType := ContentType;

  //URL encoding BEFORE paramters are attached, which may bring their own encoding
  LURL := FClient.HTTPClient.URLEncode(LURL);

  DoApplyURLSegments(LParamsList, LURL);
  DoApplyCookieParams(LParamsList, LURL);
  DoApplyHeaderParams(LParamsList);

  // Set several default headers for handling content-types, encoding, acceptance etc.
  FClient.Accept := self.Accept;

  FClient.HandleRedirects := self.HandleRedirects;
  // We always allow cookies. Many API's just send session cookies.
  // Making that a configuration option is pointless
  FClient.AllowCookies := true;
  FClient.AcceptCharset := self.AcceptCharset;
  FClient.AcceptEncoding := self.AcceptEncoding;
  FClient.HTTPClient.ConnectTimeout := Timeout;
  FClient.HTTPClient.ReadTimeout := Timeout;

  LBodyStream := nil;
  LResponseStream := nil;
  try
    case self.Method of
      TRESTRequestMethod.rmGET, TRESTRequestMethod.rmDELETE:
        begin
          /// A GET request come without a body, so all relevant params
          /// go into to a query-string .. or maybe to rio (should be nicer)
          DoPrepareQueryString(LParamsList, LURL);
        end;
      TRESTRequestMethod.rmPOST, TRESTRequestMethod.rmPUT:
        begin
          /// POST and PUT requests typically include a body, so all relevant params
          /// go into a body stream.
          /// The body stream has to consider the actual content-type (wwwform vs. multipart)
          /// content.type is only relevant for PUT/POST
          FClient.ContentType := ContentTypeToString(LContentType);
          DoPrepareRequestBody(LParamsList, LBodyStream);
        end;
    else
      begin
        raise EInvalidOperation.Create('Unknown request-method. Cannot execute request.');
      end;
    end;

    LResponseStream := TMemoryStream.Create;
    FExecutionPerformance.PreProcessingDone;
    try
      case self.Method of
        TRESTRequestMethod.rmGET:
          begin
            FClient.HTTPClient.Get(LURL, LResponseStream);
          end;
        TRESTRequestMethod.rmPOST:
          begin
            FClient.HTTPClient.Post(LURL, LBodyStream, LResponseStream)
          end;
        TRESTRequestMethod.rmPUT:
          begin
            FClient.HTTPClient.Put(LURL, LBodyStream, LResponseStream)
          end;
        TRESTRequestMethod.rmDELETE:
          begin
            FClient.HTTPClient.Delete(LURL);
          end;
      else
        raise EInvalidOperation.Create('unknown request-method required. cannot execute request.');
      end;
      FExecutionPerformance.ExecutionDone;

      // There is a subtle, undocumented feature in Indy, that might not be obvious:
      // HTTP responses coming through a stream (like FHttpClient.Get(LURL, LResponseStream);)
      // are NOT encoded accordingly to a possibly present Encoding or Content-Type Charset parameter
      // only the string-based variants (e.g. s := FHttpClient.Get(LURL)) would apply a propper encoding.
      //
      // IOW: the stream has "raw" content, where as the string would be properly re-coded to the
      // current default encoding
      //
      // This behaviour is not documented, and is in our opinion a bug. If the server specifies a charset
      // parameter, then the content HAS to be encoded in this charset, no matter what.
      // Thus the client has to respect that - no matter if a plain string or stream is returned.
      // IOW handling a stream without applying the specified charset (Encoding) is pointless.

      // so we have to re-code from Response.Charset encoding to default encoding manually here:
      if FClient.HTTPClient.Response.CharSet = '' then
        FClient.HTTPClient.Response.CharSet := FClient.FallbackCharsetEncoding;

      if FClient.HTTPClient.Response.CharSet > '' then
      begin
        LResponseStream.Position := 0;
        S := FClient.HTTPClient.ReadStringAsCharset(LResponseStream, FClient.HTTPClient.Response.CharSet);
        LResponseStream.Free;
        LResponseStream := TStringStream.Create(S);
      end;

    except
      // any kind of server/protocol error
      on E: EHTTPProtocolException do
      begin
        FExecutionPerformance.ExecutionDone;
        // we keep measuring only for protocal errors, i.e. where the server actually anwered, not for other exceptions.
        FResponse.StatusCode := E.ErrorCode; // Nummerical error code
        FResponse.StatusText := E.Message; // Short error text(RFC compliant)
        FResponse.ErrorMessage := E.Message + ' - ' + E.ErrorMessage; // Full error description
        // Use error description (E.ErrorMessage as sent by server)  as actual content. May be html, json, text depeding on server
        LErrorBytes := TEncoding.Default.GetBytes(E.ErrorMessage);
        LResponseStream.Write(LErrorBytes, Length(LErrorBytes));

        if (E.ErrorCode >= 500) and Client.RaiseExceptionOn500 then
          raise ERESTException.Create(E.Message);
        HandleEvent(DoHTTPProtocolError);
      end;
      // Unknown error, might even be on the client side. raise it!
      on E: Exception do
      begin
        // If Execute raises an Exception, then the developer should have look into the actual BaseException
        raise ERESTException.Create('REST request failed! ' + E.Message);
      end;
    end;

    /// Important: do this AFTER exception Handling!
    /// transfer the received data from the http-request to the rest-response
    FResponse.ResetToDefaults;
    FResponse.FullRequestURI := LURL;
    FResponse.Server := FClient.HTTPClient.Response.RawHeaders.Values['server'];
    FResponse.ContentType := FClient.HTTPClient.Response.ContentType;
    FResponse.ContentEncoding := FClient.HTTPClient.Response.ContentEncoding;
    FResponse.StatusCode := FClient.HTTPClient.ResponseCode;
    FResponse.StatusText := FClient.HTTPClient.Response.ResponseText;

    for i := 0 to FClient.HTTPClient.Response.RawHeaders.Count - 1 do
      FResponse.Headers.Add(FClient.HTTPClient.Response.RawHeaders.Names[i] + '=' +
        FClient.HTTPClient.Response.RawHeaders.Values[FClient.HTTPClient.Response.RawHeaders.Names[i]]);
    for i := 0 to FClient.HTTPClient.Response.CustomHeaders.Count - 1 do
      FResponse.Headers.Add(FClient.HTTPClient.Response.CustomHeaders.Names[i] + '=' +
        FClient.HTTPClient.Response.CustomHeaders.Values[FClient.HTTPClient.Response.CustomHeaders.Names[i]]);
    FResponse.SetContent(LResponseStream);

    // Performance timers do not care about events or observers
    FExecutionPerformance.PostProcessingDone;

    // Eventhandlers AFTER Observers
    HandleEvent(DoAfterExecute);

  finally
    FClient.Disconnect;

    FreeAndNIL(LResponseStream);
    FreeAndNIL(LBodyStream);
    Finalize(LParamsList);
  end;
  finally
    FResponse.EndUpdate;
  end;
end;

{ TCustomRESTRequestFileParameter }

{ TCustomRESTRequest }

const
  sBody = 'body';

procedure TCustomRESTRequest.AddBody(ABodyContent: string; AContentType: TRESTContentType = TRESTContentType.ctNone);
var
  LGUID: TGUID;
  LGuidString: string;
begin
  // A body does not have a specific name, but as names need to be unique, we are using a GUID here
  CreateGUID(LGUID);
  LGuidString := LGUID.ToString;
  LGuidString := LGuidString.Replace('{', '', [rfReplaceAll]);
  LGuidString := LGuidString.Replace('}', '', [rfReplaceAll]);
  LGuidString := LGuidString.Replace('-', '', [rfReplaceAll]);
  LGuidString := sBody + LGuidString;

  Params.AddItem(LGUID.ToString, ABodyContent, TRESTRequestParameterKind.pkREQUESTBODY, [], AContentType);
end;

procedure TCustomRESTRequest.AddBody(AObject: TObject; AContentType: TRESTContentType = TRESTContentType.ctNone);
begin
  Params.AddItem(sBody, TJson.ObjectToJsonString(AObject), TRESTRequestParameterKind.pkREQUESTBODY, [], AContentType);
end;

const
  sRequestDefaultAccept = CONTENTTYPE_APPLICATION_JSON + ', ' + CONTENTTYPE_TEXT_PLAIN + '; q=0.9, ' + CONTENTTYPE_TEXT_HTML +
    ';q=0.8,';
  sRequestDefaultAcceptCharset = 'UTF-8, *;q=0.8'; // UTF-8 is prefered, any other is good, but marked down

function TCustomRESTRequest.AcceptIsStored: Boolean;
begin
  Result := Accept <> sRequestDefaultAccept;
end;

function TCustomRESTRequest.AcceptCharSetIsStored: Boolean;
begin
  Result := AcceptCharset <> sRequestDefaultAcceptCharSet;
end;

procedure TCustomRESTRequest.AddAuthParameter(const AName, AValue: string; const AKind: TRESTRequestParameterKind;
  const AOptions: TRESTRequestParameterOptions);
begin
  self.AddParameter(AName, AValue, AKind, AOptions + [TRESTRequestParameterOption.poTransient]);
end;

procedure TCustomRESTRequest.AddBody(ABodyContent: TStream; AContentType: TRESTContentType = TRESTContentType.ctNone);
var
  LBody: string;
  LStream: TStringStream;
begin
  LStream := TStringStream.Create;
  try
    LStream.LoadFromStream(ABodyContent);
    LBody := LStream.DataString;
    Params.AddItem(sBody, LBody, TRESTRequestParameterKind.pkREQUESTBODY, [], AContentType);
  finally
    FreeAndNIL(LStream);
  end;
end;

procedure TCustomRESTRequest.AddParameter(const AName, AValue: string);
begin
  self.AddParameter(AName, AValue, TRESTRequestParameterKind.pkGETorPOST, []);
end;

procedure TCustomRESTRequest.AddParameter(const AName, AValue: string; const AKind: TRESTRequestParameterKind);
begin
  self.AddParameter(AName, AValue, AKind, []);
end;

procedure TCustomRESTRequest.AddParameter(const AName, AValue: string; const AKind: TRESTRequestParameterKind;
  const AOptions: TRESTRequestParameterOptions);
begin
  /// decide where to put the parameter to
  if (TRESTRequestParameterOption.poTransient in AOptions) then
  begin
    if (TransientParams.ParameterByName(AName) <> nil) then
      TransientParams.Delete(AName);
    TransientParams.AddItem(AName, AValue, AKind, AOptions);
  end
  else
  begin
    if (Params.ParameterByName(AName) <> nil) then
      Params.Delete(AName);
    Params.AddItem(AName, AValue, AKind, AOptions);
  end;
end;

procedure TCustomRESTRequest.ClearBody;
var
  LParam: TRESTRequestParameter;
  i: integer;
begin
  for i := Params.Count - 1 downto 0 do
  begin
    LParam := Params.Items[i];
    if (LParam.Kind = TRESTRequestParameterKind.pkREQUESTBODY) then
    begin
      Params.Delete(LParam);
    end;
  end;
end;

function TCustomRESTRequest.ContentType(const AParamsArray: TRESTRequestParameterArray): TRESTContentType;
var
  LParam: TRESTRequestParameter;
  LNumBodyParams: integer;
  LNumPostGetParams: integer;
  LSpecificContentType: TRESTContentType;
begin
  LNumBodyParams := 0;
  LNumPostGetParams := 0;
  LSpecificContentType := ctNone;

  for LParam in AParamsArray do
  begin
    LSpecificContentType := LParam.ContentType;
    if (LParam.Kind = TRESTRequestParameterKind.pkREQUESTBODY) then
      inc(LNumBodyParams)
    else if (LParam.Kind = TRESTRequestParameterKind.pkGETorPOST) then
      inc(LNumPostGetParams);
  end;

  // If there is a specific ContentType set in at least one of the params, then we use that one
  if LSpecificContentType <> ctNone then
    result := LSpecificContentType
    /// "multipart" is neccessary if we have more than one
    /// body-parameter or body-parameters as well as post/get-parameters
    /// (wich should be quite unusual, but "multipart" is the only
    /// way to go in that situation) - otherwise we just do simple
    /// form-urlencoded to keep the request as small as possible
  else if (LNumBodyParams > 1) then
    result := TRESTContentType.ctMULTIPART_FORM_DATA
  else if (LNumBodyParams > 0) and (LNumPostGetParams > 0) then
    result := TRESTContentType.ctMULTIPART_FORM_DATA
  else
    result := TRESTContentType.ctAPPLICATION_X_WWW_FORM_URLENCODED;
end;

function TCustomRESTRequest.ContentType: TRESTContentType;
var
  LParams: TRESTRequestParameterArray;
begin
  LParams := self.CreateUnionParameterList;
  try
    result := ContentType(LParams);
  finally
    Finalize(LParams);
  end;
end;

procedure TCustomRESTRequest.DoPrepareQueryString(const AParamList: TRESTRequestParameterArray; var AURL: string);
var
  LParam: TRESTRequestParameter;
  LValue: string;
begin
  /// the goal is to embedd all relevant params into the url.
  for LParam in AParamList do
  begin
    if (LParam.Kind in [TRESTRequestParameterKind.pkGETorPOST]) then
    begin
      if (TRESTRequestParameterOption.poDoNotEncode in LParam.Options) then
        LValue := LParam.Value
      else
        LValue := URIEncode(LParam.Value);

      AURL := AURL + iif(Pos('?', AURL) = 0, '?', '&') + URIEncode(LParam.Name) + '=' + LValue;
    end;
  end;
end;

function TCustomRESTRequest.GetClient: TCustomRESTClient;
begin
  result := FClient;
end;

function TCustomRESTRequest.GetExecutionPerformance: TExecutionPerformance;
begin
  result := FExecutionPerformance;
end;

function TCustomRESTRequest.GetFullRequestURL(AIncludeParams: boolean): string;
var
  AParamList: TRESTRequestParameterArray;
begin
  if Assigned(FClient) then
    result := FClient.BaseURL
  else
    result := '';

  if (self.Resource > '') then
  begin
    /// as we do know, there is no trailing slash at the
    /// end of the base-url, and also no trailing slash at the
    /// beginning of a resource, so we have to add one.
    result := result + '/' + self.Resource;
  end;

  AParamList := CreateUnionParameterList;
  try
    /// now we have to process the segment-placeholders
    DoApplyURLSegments(AParamList, result);

    if AIncludeParams and (self.Method in [TRESTRequestMethod.rmGET, TRESTRequestMethod.rmDELETE]) then
    begin
      DoPrepareQueryString(AParamList, result);
    end;
  finally
    Finalize(AParamList);
  end;
end;

function TCustomRESTRequest.GetMethod: TRESTRequestMethod;
begin
  result := FMethod;
end;

function TCustomRESTRequest.GetParams: TRESTRequestParameterList;
begin
  result := FParams;
end;

function TCustomRESTRequest.GetResource: string;
begin
  result := FResource;
end;

function TCustomRESTRequest.GetResponse: TCustomRESTResponse;
begin
  result := FResponse;
end;

function TCustomRESTRequest.GetTimeout: integer;
begin
  result := FTimeout;
end;

procedure TCustomRESTRequest.HandleEvent(AEventHandler: TMethod);
begin
  // Handle Synchronized if we are NOT already in the main thread
  // NEVER call synchronize on the MainThread - that might shift the island!
  if SynchronizedEvents and (System.MainThreadID <> TThread.CurrentThread.ThreadID) then
    TThread.Synchronize(TThread.CurrentThread, AEventHandler)
  else
    AEventHandler;
end;

function TCustomRESTRequest.MethodIsStored: Boolean;
begin
  Result := Method <> DefaultRESTRequestMethod;
end;

procedure TCustomRESTRequest.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if (Operation = opRemove) then
  begin
    if (AComponent = FClient) then
      FClient := nil
    else if (AComponent = FResponse) then
      FResponse := nil;
  end;
end;

procedure TCustomRESTRequest.ParameterListChanged;
begin
  if (NotifyList <> nil) then
  begin
    NotifyList.notify(
      procedure(ANotify: TNotify)
      begin
        ANotify.ParameterListChanged(self);
      end);
  end;
end;

procedure TCustomRESTRequest.ParameterValueChanged;
begin
  PropertyValueChanged;
end;
procedure TCustomRESTRequest.PropertyValueChanged;
begin
  if (NotifyList <> nil) then
  begin
    NotifyList.notify(
      procedure(ANotify: TNotify)
      begin
        ANotify.PropertyValueChanged(self);
      end);
  end;
end;

procedure TCustomRESTRequest.ResetToDefaults;
begin
  Method := DefaulTRESTRequestMethod;
  Resource := '';
  Timeout := 30000; // Some servers may be slow. Esp if they just recycled and need to start up on their first request
  Accept := sRequestDefaultAccept;
  AcceptCharset := sRequestDefaultAcceptCharSet;
  HandleRedirects := true;
  FExecutionPerformance.Clear;
  SynchronizedEvents := true;
  FParams.Clear;
  FTransientParams.Clear;
  if Assigned(FResponse) then
    FResponse.ResetToDefaults;
  /// we intentionally do not reset "FAutoCreateParams"
end;

procedure TCustomRESTRequest.SetAccept(const AValue: string);
begin
  if FAccept <> AValue then
  begin
    FAccept := AValue;
    PropertyValueChanged;
  end;
end;

procedure TCustomRESTRequest.SetAcceptCharset(const AValue: string);
begin
  if FAcceptCharset <> AValue then
  begin
    FAcceptCharset := AValue;
    PropertyValueChanged;
  end;
end;

procedure TCustomRESTRequest.SetAcceptEncoding(const AValue: string);
begin
  if FAcceptEncoding <> AValue then
  begin
    FAcceptEncoding := AValue;
    PropertyValueChanged;
  end;
end;

procedure TCustomRESTRequest.SetAutoCreateParams(const AValue: boolean);
begin
  if (AValue <> FAutoCreateParams) then
  begin
    FAutoCreateParams := AValue;
    PropertyValueChanged;

    /// if this option gets activated, we can just
    /// start to create some params from the resource
    if (FAutoCreateParams) then
    begin
      FParams.FromString('', FResource);
    end;
  end;
end;

procedure TCustomRESTRequest.SetClient(const AValue: TCustomRESTClient);
begin
  if (AValue <> FClient) then
  begin
    if Assigned(FClient) then
      FClient.RemoveFreeNotification(self);

    FClient := AValue;

    if Assigned(FClient) then
      FClient.FreeNotification(self);

    PropertyValueChanged;
  end;
end;

procedure TCustomRESTRequest.SetHandleRedirects(const AValue: boolean);
begin
  if FHandleRedirects <> AValue then
  begin
    FHandleRedirects := AValue;
    PropertyValueChanged;
  end;
end;

procedure TCustomRESTRequest.SetMethod(const AValue: TRESTRequestMethod);
begin
  if FMethod <> AValue then
  begin
    FMethod := AValue;
    PropertyValueChanged;
  end;
end;

procedure TCustomRESTRequest.SetParams(const AValue: TRESTRequestParameterList);
begin
  FParams.Assign(AValue);
end;

procedure TCustomRESTRequest.SetResource(const AValue: string);
var
  LOldValue: string;
begin
  if (AValue <> FResource) then
  begin
    LOldValue := FResource;
    FResource := AValue;

    /// resources should not begin with a slash
    while StartsText('/', FResource) do
      System.Delete(FResource, 1, 1);
    /// resources should also not end with a slash
    while EndsText('/', FResource) do
      System.Delete(FResource, Length(FResource), 1);

    if FAutoCreateParams and not FPosting then
    begin
      // Can't delete parameters when expressions are executing
      FParams.FromString(LOldValue, FResource);
    end;

    PropertyValueChanged;
  end;
end;

procedure TCustomRESTRequest.SetResponse(const AResponse: TCustomRESTResponse);
begin
  if (AResponse <> FResponse) then
  begin
    if Assigned(FResponse) then
      FResponse.RemoveFreeNotification(self);

    FResponse := AResponse;

    if Assigned(FResponse) then
      FResponse.FreeNotification(self);
  end;
end;

procedure TCustomRESTRequest.SetSynchronizedEvents(const AValue: boolean);
begin
  if FSynchronizedEvents <> AValue then
  begin
    FSynchronizedEvents := AValue;
    PropertyValueChanged;
  end;
end;

procedure TCustomRESTRequest.SetTimeout(const AValue: integer);
begin
  if (AValue <> FTimeout) then
  begin
    FTimeout := AValue;
    PropertyValueChanged;
  end;
end;

{ TRESTRequestAdapter }

procedure TRESTRequestAdapter.AddFields;
begin
  AddPropertyFields;
  AddParameterFields;
end;

procedure TRESTRequestAdapter.AddParameterFields;

  procedure ClearParameterFields;
  var
    i: integer;
  begin
    for i := Fields.Count - 1 downto 0 do
      if (Fields[i] is TReadWriteField<string>) and (TReadWriteField<string>(Fields[i]).Persistent <> nil) then
        Fields.Delete(i);
  end;

  procedure MakeParameterFieldNames(const ADictionary: TDictionary<string, TRESTRequestParameter>);
  const
    sPrefix = 'Params.';
  var
    i: integer;
    LParams: TRESTRequestParameterList;
    LParam: TRESTRequestParameter;
    LName: string;
    LIndex: integer;
    LSuffix: string;
  begin
    Assert(ADictionary.Count = 0);
    LParams := FRequest.Params;
    for i := 0 to LParams.Count - 1 do
    begin
      LParam := LParams[i];
      if TRESTRequestParameterOption.poTransient in LParam.Options then
        // Skip authentication header, for example
        continue;
      LName := LParam.Name;
      if LName = '' then
        LName := IntToStr(LParam.Index);
      LName := sPrefix + LName;
      LIndex := 1;
      LSuffix := '';
      while ADictionary.ContainsKey(LName + LSuffix) do
      begin
        LSuffix := IntToStr(LIndex);
        inc(LIndex);
      end;
      ADictionary.Add(LName + LSuffix, LParam);
    end;
  end;

  procedure MakeParameterField(const AParam: TRESTRequestParameter; const AFieldName: string;
  const AGetMemberObject: IGetMemberObject);
  begin
    CreateReadWriteField<string>(AFieldName, AGetMemberObject, TScopeMemberType.mtText,
      function: string
      begin
        result := AParam.Value;
      end,
      procedure(AValue: string)
      begin
        AParam.Value := AValue;
      end, AParam); // Parameter is member of field

  end;

var
  LDictionary: TDictionary<string, TRESTRequestParameter>;
  LPair: TPair<string, TRESTRequestParameter>;
  LGetMemberObject: IGetMemberObject;
begin
  CheckInactive;
  ClearParameterFields;
  if FRequest <> nil then
  begin
    LDictionary := TDictionary<string, TRESTRequestParameter>.Create;
    try
      MakeParameterFieldNames(LDictionary);
      LGetMemberObject := TBindSourceAdapterGetMemberObject.Create(self);
      for LPair in LDictionary do
        MakeParameterField(LPair.Value, LPair.Key, LGetMemberObject);
    finally
      LDictionary.Free;
    end;
  end;
end;

procedure TRESTRequestAdapter.AddPropertyFields;
  procedure ClearPropertyFields;
  var
    i: integer;
  begin
    for i := Fields.Count - 1 downto 0 do
      if not(Fields[i] is TReadWriteField<string>) or (TReadWriteField<string>(Fields[i]).Persistent = nil) then
        Fields.Delete(i);
  end;

const
  sResource = 'Resource';
var
  LGetMemberObject: IGetMemberObject;
begin
  CheckInactive;
  ClearPropertyFields;
  if FRequest <> nil then
  begin
    LGetMemberObject := TBindSourceAdapterGetMemberObject.Create(self);

    CreateReadWriteField<string>(sResource, LGetMemberObject, TScopeMemberType.mtText,
      function: string
      begin
        result := FRequest.Resource;
      end,
      procedure(AValue: string)
      begin
        FRequest.Resource := AValue;
      end);
  end;
end;

constructor TRESTRequestAdapter.Create(AComponent: TComponent);
begin
  inherited;
  FNotify := TNotify.Create(self);
end;

destructor TRESTRequestAdapter.Destroy;
begin
  inherited;
  if FRequest <> nil then
    if FRequest.NotifyList <> nil then
      FRequest.NotifyList.RemoveNotify(FNotify);
  FNotify.Free;
end;

function TRESTRequestAdapter.GetCanActivate: boolean;
begin
  result := FRequest <> nil;
end;

procedure TRESTRequestAdapter.GetMemberNames(AList: TStrings);
var
  LField: TBindSourceAdapterField;
begin
  for LField in Fields do
  begin
    if (LField is TReadWriteField<string>) then
      // Provide object so that LiveBindings designer can select in designer when member is clicked
      AList.AddObject(LField.MemberName, TReadWriteField<string>(LField).Persistent)
    else
      AList.Add(LField.MemberName);
  end;
end;

function TRESTRequestAdapter.GetSource: TBaseLinkingBindSource;
begin
  result := FRequest;
end;

procedure TRESTRequestAdapter.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  /// clean up component-references
  if (Operation = opRemove) then
  begin
    if (AComponent = FRequest) then
      Request := nil
  end;
end;

procedure TRESTRequestAdapter.DoChangePosting;
begin
  if FRequest <> nil then
    FRequest.FPosting := Posting;
end;

procedure TRESTRequestAdapter.ParameterListChanged;
begin
  if (FRequest <> nil) and not(csLoading in FRequest.ComponentState) then
  begin
    if Active and not Posting then
    begin
      // Force parameters to be recreated
      Active := FALSE;
      Active := true;
    end;
  end;
end;

procedure TRESTRequestAdapter.SetRequest(const ARequest: TCustomRESTRequest);
var
  LActive: boolean;
begin
  if FRequest <> ARequest then
  begin
    if FRequest <> nil then
    begin
      if FRequest.NotifyList <> nil then
        FRequest.NotifyList.RemoveNotify(FNotify);
      FRequest.RemoveFreeNotification(self);
    end;
    LActive := Active;
    Active := FALSE;
    // Assert(FRequest = nil);  // expect one-to-one request/adapter
    FRequest := ARequest;
    if FRequest <> nil then
    begin
      if FRequest.NotifyList <> nil then
        FRequest.NotifyList.AddNotify(FNotify);
      FRequest.FreeNotification(self);
    end;
    if LActive and CanActivate then
      Active := true;
  end;
end;

{ TRESTRequestAdapter.TRequestNotify }

constructor TRESTRequestAdapter.TNotify.Create(const AAdapter: TRESTRequestAdapter);
begin
  FAdapter := AAdapter;
end;

procedure TRESTRequestAdapter.TNotify.ParameterListChanged(Sender: TObject);
begin
  if Assigned(FAdapter) then
    FAdapter.ParameterListChanged;
end;

procedure TRESTRequestAdapter.TNotify.PropertyValueChanged(Sender: TObject);
begin
  if Assigned(FAdapter) then
    FAdapter.RefreshFields;
end;

{ TCustomRESTRequest.TNotify }

procedure TCustomRESTRequest.TNotify.ParameterListChanged(Sender: TObject);
begin
  //
end;

{ TCustomRESTRequestBindSource }

function TCustomRESTRequestBindSource.CreateAdapter: TRESTComponentAdapter;
begin
  FAdapter := TRESTRequestAdapter.Create(self);
  result := FAdapter;
end;

function TCustomRESTRequestBindSource.GetRequest: TCustomRESTRequest;
begin
  result := FAdapter.Request;
end;

procedure TCustomRESTRequestBindSource.SetRequest(const Value: TCustomRESTRequest);
begin
  FAdapter.Request := Value;
end;

{ TCustomRESTResponse }

constructor TCustomRESTResponse.Create(AOwner: TComponent);
begin
  /// it is important to create the notify-list before
  /// calling the inherited constructor
  FNotifyList := TNotifyList.Create;
  inherited;

  /// if the owner is a form or a tdatamodule, we will iterate
  /// through all components and see if there's a request without
  /// a response. if there is, then we just assign ourself to it.
  if RESTComponentIsDesigning(self) then
  begin
    AssignResponseToRESTRequests(self);
  end;

  FHeaders := TStringList.Create;
  ResetToDefaults;
end;

function TCustomRESTResponse.CreateBindSource: TBaseObjectBindSource;
begin
  FBindSource := TSubRESTResponseBindSource.Create(self);
  FBindSource.Name := 'BindSource'; { Do not localize }
  FBindSource.SetSubComponent(true);
  FBindSource.Response := self;

  result := FBindSource;
end;

destructor TCustomRESTResponse.Destroy;
begin
  FreeAndNIL(FHeaders);
  FreeAndNIL(FNotifyList);
  FreeAndNIL(FJSONValue);
  inherited;
end;

function TCustomRESTResponse.GetContent: string;
begin
  // RAWBytes are already decoded from the charset as sent by the server
  if (FContent = '') and (Length(FRAWBytes) > 0) then
    FContent := TEncoding.Default.GetString(FRAWBytes);
  result := FContent;
end;

function TCustomRESTResponse.GetContentEncoding: string;
begin
  result := FContentEncoding;
end;

function TCustomRESTResponse.GetContentLength: cardinal;
begin
  result := Length(FRAWBytes);
end;

function TCustomRESTResponse.GetContentType: string;
begin
  result := FContentType;
end;

function TCustomRESTResponse.GetErrorMessage: string;
begin
  result := FErrorMessage;
end;

function TCustomRESTResponse.GetFullRequestURI: string;
begin
  result := FFullRequestURI;
end;

function TCustomRESTResponse.GetHeaders: TStrings;
begin
  result := FHeaders;
end;

function TCustomRESTResponse.GetJSONValue: TJsonValue;
var
  LJSONValue: TJsonValue;
  LJsonRootPair: TJSONPair;
  LToken: string;
  LJSONValueNew: TJsonValue;
  LTokens: TStringDynArray;
begin
  if not Assigned(FJSONValue) then
  begin
    try
    LJSONValue := TJSONObject.ParseJSONValue(Content);
    if (RootElement <> '') and Assigned(LJSONValue) and (LJSONValue is TJSONObject) then
    begin
      LTokens := SplitString(RootElement, '.');
      for LToken in LTokens do
      begin
        LJsonRootPair := TJSONObject(LJSONValue).Get(LToken);
        if Assigned(LJsonRootPair) then
        begin
          LJSONValueNew := LJsonRootPair.JSONValue;
          LJSONValueNew.Owned := FALSE; // Need to decouple form parent, to avoid memleak
          FreeAndNIL(LJSONValue);
          LJSONValue := LJSONValueNew;
        end
        else
        begin
          LJSONValue := nil;
          Break;
        end;
      end;
    end;
    FJSONValue := LJSONValue;
    except
      FJsonValue := nil;
    end;
  end;
  result := FJSONValue;
end;

function TCustomRESTResponse.GetServer: string;
begin
  result := FServer;
end;

function TCustomRESTResponse.GetSimpleValue(const AName: string; var AValue: string): boolean;
var
  LJSONObj: TJSONObject;
  LJSONPair: TJSONPair;
begin
  result := FALSE;
  // plain text
  if SameText(ContentType, CONTENTTYPE_TEXT_HTML) then
  begin
    if ContainsText(Content, AName + '=') then
    begin
      AValue := Copy(Content, Pos(AName + '=', Content) + Length(AName) + 1, Length(Content));
      if (Pos('&', AValue) > 0) then
        AValue := Copy(AValue, 1, Pos('&', AValue) - 1);
      result := true;
    end;
  end
  // Json
  else if SameText(ContentType, CONTENTTYPE_APPLICATION_JSON) then
  begin
    LJSONObj := TJSONObject.ParseJSONValue(Content) as TJSONObject;
    if Assigned(LJSONObj) then
    begin
      LJSONPair := LJSONObj.Get(AName);
      if Assigned(LJSONPair) then
      begin
        AValue := LJSONPair.JSONValue.Value;
        result := true;
      end;
      FreeAndNIL(LJSONObj); // <-- the jsonobject will also free the jsonpair!
    end;
  end;
end;

function TCustomRESTResponse.GetStatusCode: integer;
begin
  result := FStatusCode;
end;

function TCustomRESTResponse.GetStatusText: string;
begin
  result := FStatusText;
end;

procedure TCustomRESTResponse.JSONValueChanged;
begin
  if IsUpdating then
    Include(FUpdateOptions, TUpdateOption.JSONValueChanged)
  else if (NotifyList <> nil) then
  begin
    NotifyList.notify(
      procedure(ANotify: TNotify)
      begin
        ANotify.JSONValueChanged(self);
      end);
  end;
end;

function TCustomRESTResponse.IsUpdating: Boolean;
begin
  Result := FUpdating > 0;
end;

procedure TCustomRESTResponse.BeginUpdate;
begin
  if not IsUpdating then
    FUpdateOptions := [];

  Inc(FUpdating);
end;

procedure TCustomRESTResponse.EndUpdate;
begin
  if IsUpdating then
  begin
    Dec(FUpdating);
    if not IsUpdating then
    begin
      if TUpdateOption.PropertyValueChanged in FUpdateOptions then
        PropertyValueChanged;
      if TUpdateOption.JSONValueChanged in FUpdateOptions then
        JSONValueChanged;
      FUpdateOptions := [];
    end;
  end;
end;
procedure TCustomRESTResponse.PropertyValueChanged;
begin
  if IsUpdating then
    Include(FUpdateOptions, TUpdateOption.PropertyValueChanged)
  else if (NotifyList <> nil) then
    begin
      NotifyList.notify(
        procedure(ANotify: TNotify)
        begin
          ANotify.PropertyValueChanged(self);
        end);
    end;
end;

procedure TCustomRESTResponse.ResetToDefaults;
begin
  BeginUpdate;
  try
    Setlength(FRAWBytes, 0);
    FContent := '';
    ContentEncoding := '';
    ContentType := '';
    ErrorMessage := '';
    FullRequestURI := '';
    Server := '';
    StatusCode := 0;
    StatusText := '';
    Headers.Clear;
    FreeAndNil(FJSONValue);
    // Content property changed
    PropertyValueChanged;
    JSONValueChanged;
  finally
    EndUpdate;
  end;
end;

procedure TCustomRESTResponse.SetContent(AStream: TStream);
var
  LStreamPos: int64;
begin
  Assert(Assigned(AStream));
  FContent := '';
  FJSONValue := nil;

  LStreamPos := AStream.Position;
  try
    AStream.Seek(0, soFromBeginning);

    Setlength(FRAWBytes, AStream.Size);
    AStream.Read(FRAWBytes, AStream.Size);
  finally
    AStream.Position := LStreamPos;
  end;

  // Content property changed
  PropertyValueChanged;
  JSONValueChanged;
end;

procedure TCustomRESTResponse.SetContentEncoding(const AValue: string);
begin
  if (AValue <> FContentEncoding) then
  begin
    FContentEncoding := AValue;
    PropertyValueChanged;
  end;
end;

procedure TCustomRESTResponse.SetContentType(const AValue: string);
begin
  if (AValue <> FContentType) then
  begin
    FContentType := AValue;
    PropertyValueChanged;
  end;
end;

procedure TCustomRESTResponse.SetErrorMessage(const AValue: string);
begin
  if (AValue <> FErrorMessage) then
  begin
    FErrorMessage := AValue;
    PropertyValueChanged;
  end;
end;

procedure TCustomRESTResponse.SetFullRequestURI(const AValue: string);
begin
  if (AValue <> FFullRequestURI) then
  begin
    FFullRequestURI := AValue;
    PropertyValueChanged;
  end;
end;

procedure TCustomRESTResponse.SetRootElement(const AValue: string);
begin
  if FRootElement <> AValue then
  begin
    FRootElement := AValue;
    FreeAndNIL(FJSONValue);

    PropertyValueChanged;
    JSONValueChanged;
  end;
end;

procedure TCustomRESTResponse.SetServer(const AValue: string);
begin
  if (AValue <> FServer) then
  begin
    FServer := AValue;
    PropertyValueChanged;
  end;
end;

procedure TCustomRESTResponse.SetStatusCode(const AValue: integer);
begin
  if (AValue <> FStatusCode) then
  begin
    FStatusCode := AValue;
    PropertyValueChanged;
  end;
end;

procedure TCustomRESTResponse.SetStatusText(const AValue: string);
begin
  if (AValue <> FStatusText) then
  begin
    FStatusText := AValue;
    PropertyValueChanged;
  end;
end;

{ TCustomRESTClient }

constructor TCustomRESTClient.Create(const ABaseApiURL: string);
begin
  /// sometimes the order *IS* important.
  /// first we do create the notifylist,
  /// then we can call the inherited constructor.
  FNotifyList := TNotifyList.Create;
  inherited Create(nil);
  FHttpClient := nil;
  FParams := TRESTRequestParameterList.Create(self);
  FTransientParams := TRESTRequestParameterList.Create(self);
  ResetToDefaults;
  // This calls CreateHTTPClient internally
  BaseURL := ABaseApiURL;
end;

constructor TCustomRESTClient.Create(AOwner: TComponent);
begin
  /// sometimes the order *IS* important.
  /// first we do create the notifylist,
  /// then we can call the inherited constructor.
  FNotifyList := TNotifyList.Create;
  inherited Create(AOwner);
  FIPImplementationID := '';
  FHttpClient := nil;
  FParams := TRESTRequestParameterList.Create(self);
  FTransientParams := TRESTRequestParameterList.Create(self);
  ResetToDefaults;
  if (AOwner is TCustomAuthenticator) then
    Authenticator := (AOwner as TCustomAuthenticator)
  else if RESTComponentIsDesigning(self) then
  begin
    Authenticator := RESTFindDefaultAuthenticator(self);
    AssignClientToRESTRequests(self);
  end;
end;

function TCustomRESTClient.CreateBindSource: TBaseObjectBindSource;
begin
  FBindSource := TSubRESTClientBindSource.Create(self);
  FBindSource.Name := 'BindSource'; { Do not localize }
  FBindSource.SetSubComponent(true);
  FBindSource.Client := self;

  result := FBindSource;
end;

destructor TCustomRESTClient.Destroy;
begin
  // Disconnect and go home
  FHttpClient.Disconnect;
  FreeAndNIL(FHttpClient);
  FreeAndNIL(FParams);
  FreeAndNIL(FTransientParams);
  FAuthenticator := nil;
  inherited;
  FreeAndNIL(FNotifyList);
end;

procedure TCustomRESTClient.Disconnect;
begin
  if (HTTPClient <> nil) then
    HTTPClient.Disconnect;
end;

procedure TCustomRESTClient.SetCookie(const Data, CookieURL: string);
begin
  HTTPClient.CookieManager.AddServerCookie(Data, CookieURL);
end;

procedure TCustomRESTClient.AddParameter(const AName, AValue: string);
begin
  self.AddParameter(AName, AValue, TRESTRequestParameterKind.pkGETorPOST, []);
end;

procedure TCustomRESTClient.AddParameter(const AName, AValue: string; const AKind: TRESTRequestParameterKind);
begin
  self.AddParameter(AName, AValue, AKind, []);
end;

procedure TCustomRESTClient.AddAuthParameter(const AName, AValue: string; const AKind: TRESTRequestParameterKind;
const AOptions: TRESTRequestParameterOptions);
begin
  self.AddParameter(AName, AValue, AKind, AOptions + [TRESTRequestParameterOption.poTransient]);
end;

procedure TCustomRESTClient.AddParameter(const AName, AValue: string; const AKind: TRESTRequestParameterKind;
const AOptions: TRESTRequestParameterOptions);
begin
  /// decide where to put the parameter to
  if (TRESTRequestParameterOption.poTransient in AOptions) then
  begin
    if (TransientParams.ParameterByName(AName) <> nil) then
      TransientParams.Delete(AName);
    TransientParams.AddItem(AName, AValue, AKind, AOptions);
  end
  else
  begin
    if (Params.ParameterByName(AName) <> nil) then
      Params.Delete(AName);
    Params.AddItem(AName, AValue, AKind, AOptions);
  end;
end;

procedure TCustomRESTClient.CreateHttpClient;
begin
  FreeAndNIL(FHttpClient);
    FHttpClient := TRESTHTTP.Create(FIPImplementationID);
  // This is handled by default in THttpClient, but to be safe we do this here anyway
  FHttpClient.HTTPOptions := FHttpClient.HTTPOptions - [hoForceEncodeParams];
  FHttpClient.AllowCookies := true;
end;

procedure TCustomRESTClient.DoHTTPProtocolError;
begin
  if Assigned(FOnHTTPProtocolError) then
    FOnHTTPProtocolError(self);
end;

function TCustomRESTClient.GetAccept: string;
begin
  result := HTTPClient.Request.Accept;
end;

function TCustomRESTClient.GetAcceptCharset: string;
begin
  if (HTTPClient.Request <> nil) then
    result := HTTPClient.Request.AcceptCharset
  else
    result := '';
end;

function TCustomRESTClient.GetAcceptEncoding: string;
begin
  if (HTTPClient.Request <> nil) then
    result := HTTPClient.Request.AcceptEncoding
  else
    result := '';
end;

function TCustomRESTClient.GetAllowCookies: boolean;
begin
  if (HTTPClient.Request <> nil) then
    result := HTTPClient.AllowCookies
  else
    result := FALSE;
end;

function TCustomRESTClient.GetAuthenticator: TCustomAuthenticator;
begin
  result := FAuthenticator;
end;

function TCustomRESTClient.GetBaseURL: string;
begin
  result := FBaseURL;
end;

function TCustomRESTClient.GetContentType: string;
begin
  if (HTTPClient <> nil) then
    result := HTTPClient.Request.ContentType
  else
    result := '';
end;

function TCustomRESTClient.GetEntity<T>(const AResource: string): T;
var
  LRequest: TRESTRequest;
begin
  LRequest := TRESTRequest.Create(self);
  try
    LRequest.Method := rmGET;
    LRequest.Resource := AResource;
    LRequest.Execute;
    result := LRequest.Response.JSONValue;
  finally
    FreeAndNIL(LRequest);
  end;
end;

function TCustomRESTClient.GetEntityArray<T>(const AQuery: string): TArray<T>;
var
  LList: TObjectList<T>;
  xEntity: T;
  i: integer;
begin
  LList := GetEntityList<T>(AQuery);
  Setlength(result, LList.Count);
  i := 0;
  for xEntity in LList do
  begin
    result[i] := xEntity;
    inc(i);
  end;
end;

function TCustomRESTClient.GetEntityList<T>(const AResource: string): TObjectList<T>;
var
  LResponse: string;
  LJsonResponse: TJSONObject;
  LResponseArray: TJSONArray;
  i: integer;
  LItem: T;
  LJSONObject: TJSONObject;
  LRequest: TRESTRequest;
begin

  LRequest := TRESTRequest.Create(self);
  try
    LRequest.Method := rmGET;
    LRequest.Resource := AResource;
    LRequest.Execute;

    // Parse response as Json and try interpreting it as Array
    LResponseArray := LRequest.Response.JSONValue as TJSONArray;
    if Assigned(LResponseArray) then
    begin
      result := TObjectList<T>.Create;
      // The array's items are supposed to be representations of class <T>
      for i := 0 to LResponseArray.Size - 1 do
      begin
        LJSONObject := LResponseArray.Get(i) as TJSONObject;
        LItem := TJson.JsonToObject<T>(LJSONObject.ToString);
        result.Add(LItem);
      end;
    end
    else
    begin
      raise ERESTException.Create('Response did not return an Array of ' + T.Classname);
    end;

  finally
    FreeAndNIL(LRequest);
  end;
end;

function TCustomRESTClient.GetFallbackCharsetEncoding: string;
begin
  result := FFallbackCharsetEncoding;
end;

function TCustomRESTClient.GetHandleRedirects: boolean;
begin
  if (HTTPClient <> nil) then
    result := HTTPClient.HandleRedirects
  else
    result := FALSE;
end;

function TCustomRESTClient.GetHttpClient: TRESTHTTP;
begin
  result := FHttpClient;
end;

function TCustomRESTClient.GetParams: TRESTRequestParameterList;
begin
  result := FParams;
end;

function TCustomRESTClient.GetProxyPassword: string;
begin
  result := FHttpClient.ProxyParams.ProxyPassword;
end;

function TCustomRESTClient.GetProxyPort: integer;
begin
  result := FHttpClient.ProxyParams.ProxyPort;
end;

function TCustomRESTClient.GetProxyServer: string;
begin
  result := FHttpClient.ProxyParams.ProxyServer;
end;

function TCustomRESTClient.GetProxyUsername: string;
begin
  result := FHttpClient.ProxyParams.ProxyUsername;
end;

function TCustomRESTClient.GetUserAgent: string;
begin
  if HTTPClient.Request <> nil then
  begin
    result := HTTPClient.Request.UserAgent;
  end
  else
  begin
    result := '';
  end;
end;

procedure TCustomRESTClient.HandleEvent(AEventHandler: TMethod);
begin
  // Handle Synchronized if we are NOT already in the main thread
  // NEVER call synchronize on the MainThread - that might shift the island!
  if SynchronizedEvents and (System.MainThreadID <> TThread.CurrentThread.ThreadID) then
    TThread.Synchronize(TThread.CurrentThread, AEventHandler)
  else
    AEventHandler;
end;

const
  sDefaultFallbackCharSetEncoding = 'UTF-8';
  sDefaultUserAgent = 'Embarcadero RESTClient/' + RESTCLIENT_VERSION;
function TCustomRESTClient.FallbackCharsetEncodingIsStored: Boolean;
begin
  Result := sDefaultFallbackCharSetEncoding <> FallbackCharSetEncoding;
end;
function TCustomRESTClient.PostEntity<T>(const AResource: string; AEntity: T): TJSONObject;
var
  LRequest: TRESTRequest;
begin
  LRequest := TRESTRequest.Create(self);
  try
    LRequest.Method := rmPOST;
    LRequest.Resource := AResource;
    LRequest.AddBody(AEntity);
    LRequest.Execute;
    if Assigned(LRequest.Response.JSONValue) then
      result := LRequest.Response.JSONValue as TJSONObject;
  finally
    FreeAndNIL(LRequest);
  end;
end;

procedure TCustomRESTClient.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if (Operation = opRemove) then
  begin
    if (AComponent = FAuthenticator) then
      FAuthenticator := nil;
  end;
end;

procedure TCustomRESTClient.ParameterListChanged;
begin
  if (NotifyList <> nil) then
  begin
    NotifyList.notify(
      procedure(ANotify: TNotify)
      begin
        ANotify.ParameterListChanged(self);
      end);
  end;
end;

procedure TCustomRESTClient.ParameterValueChanged;
begin
  PropertyValueChanged;
end;
procedure TCustomRESTClient.PropertyValueChanged;
begin
  if (NotifyList <> nil) then
  begin
    NotifyList.notify(
      procedure(ANotify: TNotify)
      begin
        ANotify.PropertyValueChanged(self);
      end);
  end;
end;

procedure TCustomRESTClient.ResetToDefaults;
begin
  CreateHttpClient;
  BaseURL := '';
  Authenticator := nil;
  ProxyServer := '';
  ProxyPort := 0;
  ProxyUsername := '';
  ProxyPassword := '';
  UserAgent := sDefaultUserAgent;
  FallbackCharsetEncoding := sDefaultFallbackCharSetEncoding;
  FSynchronizedEvents := true;
  FRaiseExceptionOn500 := true;
  FAutoCreateParams := true;
  FParams.Clear;
  FTransientParams.Clear;
end;

procedure TCustomRESTClient.SetAccept(const AValue: string);
begin
  if (AValue <> HTTPClient.Request.Accept) then
  begin
    HTTPClient.Request.Accept := AValue;

    PropertyValueChanged;
  end;
end;

procedure TCustomRESTClient.SetAcceptCharset(const AValue: string);
begin
  if (HTTPClient.Request <> nil) then
    HTTPClient.Request.AcceptCharset := AValue;
end;

procedure TCustomRESTClient.SetAcceptEncoding(const AValue: string);
begin
  if (HTTPClient <> nil) then
    HTTPClient.Request.AcceptEncoding := AValue;
end;

procedure TCustomRESTClient.SetAllowCookies(const AValue: boolean);
begin
  if (HTTPClient <> nil) then
    HTTPClient.AllowCookies := AValue;
end;

procedure TCustomRESTClient.SetAuthenticator(const AValue: TCustomAuthenticator);
begin
  if (AValue <> FAuthenticator) then
  begin
    if Assigned(FAuthenticator) then
      FAuthenticator.RemoveFreeNotification(self);

    FAuthenticator := AValue;

    if Assigned(FAuthenticator) then
      FAuthenticator.FreeNotification(self);

    PropertyValueChanged;
  end;
end;

procedure TCustomRESTClient.SetAutoCreateParams(const AValue: boolean);
begin
  if (AValue <> FAutoCreateParams) then
  begin
    FAutoCreateParams := AValue;
    PropertyValueChanged;

    /// if this option gets activated, we can just
    /// start to create some params from the resource
    if (FAutoCreateParams) then
    begin
      FParams.FromString('', FBaseURL);
    end;
  end;
end;

procedure TCustomRESTClient.SetBaseURL(const AValue: string);
var
  LOldValue: string;
begin
  if ((AValue <> FBaseURL) or (AValue = '')) then
  begin
    LOldValue := FBaseURL;
    FBaseURL := TRESTHTTP.FixupURL(IPImplementationID, AValue);

    /// delete all trailing slashes
    while EndsText('/', FBaseURL) do
      System.Delete(FBaseURL, Length(FBaseURL), 1);

    if FAutoCreateParams and not FPosting then
    begin
      // Can't delete parameters when expressions are executing
      FParams.FromString(LOldValue, FBaseURL);
    end;

    if StartsText('https', FBaseURL) then
      HTTPClient.Protocol := 'https'
    else
      HTTPClient.Protocol := 'http';
    // This is handled by default in THttpClient, but to be safe we do this here anyway
    PropertyValueChanged;
  end;
end;

procedure TCustomRESTClient.SetContentType(const AValue: string);
begin
  if (HTTPClient <> nil) then
    HTTPClient.Request.ContentType := AValue;
end;

procedure TCustomRESTClient.SetFallbackCharsetEncoding(const AValue: string);
begin
  if AValue <> FFallbackCharsetEncoding then
  begin
    FFallbackCharsetEncoding := AValue;
    PropertyValueChanged;
  end;
end;

procedure TCustomRESTClient.SetHandleRedirects(const AValue: boolean);
begin
  if (HTTPClient <> nil) then
    HTTPClient.HandleRedirects := AValue;
end;

procedure TCustomRESTClient.SetHTTPHeader(const AName, AValue: string);
begin
  HTTPClient.Request.CustomHeaders.Values[AName] := AValue;
end;

procedure TCustomRESTClient.SetIPImplementationID(const Value: string);
begin
  if FIPImplementationID <> Value then
  begin
    FIPImplementationID := Value;
    if not (csLoading in ComponentState) then
      // Recreate peers
      CreateHttpClient;
  end;
end;

procedure TCustomRESTClient.SetParams(const AValue: TRESTRequestParameterList);
begin
  FParams.Assign(AValue);
end;

procedure TCustomRESTClient.SetProxyPassword(const AValue: string);
begin
  if (AValue <> FHttpClient.ProxyParams.ProxyPassword) then
  begin
    FHttpClient.ProxyParams.ProxyPassword := AValue;
    PropertyValueChanged;
  end;
end;

procedure TCustomRESTClient.SetProxyPort(const AValue: integer);
begin
  if (AValue <> FHttpClient.ProxyParams.ProxyPort) then
  begin
    FHttpClient.Disconnect;
    FHttpClient.ProxyParams.ProxyPort := AValue;
    PropertyValueChanged;
  end;
end;

procedure TCustomRESTClient.SetProxyServer(const AValue: string);
begin
  if (AValue <> FHttpClient.ProxyParams.ProxyServer) then
  begin
    FHttpClient.Disconnect;
    FHttpClient.ProxyParams.ProxyServer := AValue;
    PropertyValueChanged;
  end;
end;

procedure TCustomRESTClient.SetProxyUsername(const AValue: string);
begin
  if (AValue <> FHttpClient.ProxyParams.ProxyUsername) then
  begin
    FHttpClient.ProxyParams.ProxyUsername := AValue;
    PropertyValueChanged;
  end;
end;

procedure TCustomRESTClient.SetSynchronizedEvents(const AValue: boolean);
begin
  if (AValue <> FSynchronizedEvents) then
  begin
    FSynchronizedEvents := AValue;
    PropertyValueChanged;
  end;
end;

procedure TCustomRESTClient.SetUserAgent(const AValue: string);
begin
  if (HTTPClient.Request <> nil) then
  begin
    if (AValue <> HTTPClient.Request.UserAgent) then
    begin
      HTTPClient.Request.UserAgent := AValue;
      PropertyValueChanged;
    end;
  end;
end;

function TCustomRESTClient.UserAgentIsStored: Boolean;
begin
  Result := UserAgent <> sDefaultUserAgent;
end;
{ TCustomAuthenticator }

procedure TCustomAuthenticator.Authenticate(ARequest: TCustomRESTRequest);
var
  LDone: boolean;
begin
  LDone := FALSE;
  if Assigned(FOnAuthenticate) then
    FOnAuthenticate(ARequest, LDone);

  if not LDone then
  begin
    Assert(Assigned(ARequest));

    DoAuthenticate(ARequest);
  end;
end;

constructor TCustomAuthenticator.Create(AOwner: TComponent);
begin
  /// sometimes the order *IS* important.
  /// first we do create the notifylist,
  /// then we can call the inherited constructor.
  FNotifyList := TNotifyList.Create;
  inherited;

  if RESTComponentIsDesigning(self) then
  begin
    AssignAuthenticatorToRESTClients(self);
  end;
end;

destructor TCustomAuthenticator.Destroy;
begin
  inherited;
  FreeAndNIL(FNotifyList);
end;

procedure TCustomAuthenticator.PropertyValueChanged;
begin
  if (NotifyList <> nil) then
  begin
    NotifyList.notify(
      procedure(ANotify: TNotify)
      begin
        ANotify.PropertyValueChanged(self);
      end);
  end;
end;

procedure TCustomAuthenticator.ResetToDefaults;
begin
  // nothing here
  // Do NOT delete an assigned OnAuthenticate event!
end;

{ TRESTAuthenticatorAdapter }

procedure TRESTAuthenticatorAdapter<T>.SetAuthenticator(const AAuthenticator: T);
var
  LActive: boolean;
begin
  if FAuthenticator <> AAuthenticator then
  begin
    DoAuthenticatorChanging;
    if FAuthenticator <> nil then
    begin
      FAuthenticator.RemoveFreeNotification(self);
    end;
    LActive := Active;
    Active := FALSE;
    FAuthenticator := AAuthenticator;
    DoAuthenticatorChanged;
    if FAuthenticator <> nil then
    begin
      FAuthenticator.FreeNotification(self);
    end;
    if LActive and CanActivate then
      Active := true;
  end;
end;

procedure TRESTAuthenticatorAdapter<T>.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  /// clean up component-references
  if (Operation = opRemove) then
  begin
    if (AComponent = TComponent(FAuthenticator)) then
      Authenticator := nil
  end;
end;

constructor TRESTAuthenticatorAdapter<T>.Create(AOwner: TComponent);
begin
  inherited;

  FNotify := TNotify.Create(self);
end;

destructor TRESTAuthenticatorAdapter<T>.Destroy;
begin
  FreeAndNIL(FNotify);

  inherited;
end;

procedure TRESTAuthenticatorAdapter<T>.DoAuthenticatorChanged;
begin
  if (Authenticator <> nil) then
  begin
    if (Authenticator.NotifyList <> nil) then
      Authenticator.NotifyList.AddNotify(FNotify);
  end;
end;

procedure TRESTAuthenticatorAdapter<T>.DoAuthenticatorChanging;
begin
  if (Authenticator <> nil) then
  begin
    if (Authenticator.NotifyList <> nil) then
      Authenticator.NotifyList.RemoveNotify(FNotify);
  end;
end;

function TRESTAuthenticatorAdapter<T>.GetCanActivate: boolean;
begin
  result := FAuthenticator <> nil;
end;

function TRESTAuthenticatorAdapter<T>.GetSource: TBaseLinkingBindSource;
begin
  result := FAuthenticator;
end;

{ TRESTAuthenticatorBindSource }

function TRESTAuthenticatorBindSource<T>.CreateAdapter: TRESTComponentAdapter;
begin
  FAdapter := CreateAdapterT;
  result := FAdapter;
end;

function TRESTAuthenticatorBindSource<T>.CreateAdapterT: TRESTAuthenticatorAdapter<T>;
begin
  result := TRESTAuthenticatorAdapter<T>.Create(self);
end;

function TRESTAuthenticatorBindSource<T>.GetAuthenticator: TCustomAuthenticator;
begin
  result := FAdapter.Authenticator;
end;

procedure TRESTAuthenticatorBindSource<T>.SetAuthenticator(const Value: TCustomAuthenticator);
begin
  FAdapter.Authenticator := Value;
end;

{ TCustomRESTResponseBindSource }

function TCustomRESTResponseBindSource.CreateAdapter: TRESTComponentAdapter;
begin
  FAdapter := TRESTResponseAdapter.Create(self);
  result := FAdapter;
end;

function TCustomRESTResponseBindSource.GetResponse: TCustomRESTResponse;
begin
  result := FAdapter.Response;
end;

procedure TCustomRESTResponseBindSource.SetResponse(const Value: TCustomRESTResponse);
begin
  FAdapter.Response := Value;
end;

{ TCustomRESTResponse.TNotify }

procedure TCustomRESTResponse.TNotify.JSONValueChanged(ASender: TObject);
begin
  //
end;

{ TRESTResponseAdapter }

procedure TRESTResponseAdapter.SetResponse(const AResponse: TCustomRESTResponse);
var
  LActive: boolean;
begin
  if FResponse <> AResponse then
  begin
    if FResponse <> nil then
    begin
      if FResponse.NotifyList <> nil then
        FResponse.NotifyList.RemoveNotify(FNotify);
      FResponse.RemoveFreeNotification(self);
    end;
    LActive := Active;
    Active := FALSE;
    FResponse := AResponse;
    if FResponse <> nil then
    begin
      if FResponse.NotifyList <> nil then
        FResponse.NotifyList.AddNotify(FNotify);
      FResponse.FreeNotification(self);
    end;
    if LActive and CanActivate then
      Active := true;
  end;
end;

procedure TRESTResponseAdapter.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  /// clean up component-references
  if (Operation = opRemove) then
  begin
    if (AComponent = FResponse) then
      Response := nil
  end;
end;

constructor TRESTResponseAdapter.Create(AComponent: TComponent);
begin
  inherited;
  FNotify := TNotify.Create(self);
end;

destructor TRESTResponseAdapter.Destroy;
begin
  inherited;
  if (FResponse <> nil) then
  begin
    if FResponse.NotifyList <> nil then
      FResponse.NotifyList.RemoveNotify(FNotify);
  end;
  FNotify.Free;
end;

procedure TRESTResponseAdapter.AddFields;
begin
  AddPropertyFields;
end;

procedure TRESTResponseAdapter.AddPropertyFields;
const
  sContent = 'Content';
  sContentType = 'ContentType';
  sContentLength = 'ContentLength'; // Integer
  sContentEncoding = 'ContentEncoding';
  sErrorMessage = 'ErrorMessage';
  sFullRequestURI = 'FullRequestURI'; // read only
  sRootElement = 'RootElement';
  sServer = 'Server';
  sStatusCode = 'StatusCode'; // Integer
  sStatusText = 'StatusText';
  sHeaders = 'Headers';
  sJSONValue = 'JSONValue';
var
  LGetMemberObject: IGetMemberObject;
begin
  CheckInactive;
  ClearFields;
  if FResponse <> nil then
  begin
    LGetMemberObject := TBindSourceAdapterGetMemberObject.Create(self);
    CreateReadOnlyField<string>(sContent, LGetMemberObject, TScopeMemberType.mtText,
      function: string
      begin
        result := FResponse.Content;
      end);
    CreateReadOnlyField<string>(sContentType, LGetMemberObject, TScopeMemberType.mtText,
      function: string
      begin
        result := FResponse.ContentType;
      end);
    CreateReadOnlyField<integer>(sContentLength, LGetMemberObject, TScopeMemberType.mtInteger,
      function: integer
      begin
        result := FResponse.ContentLength;
      end);
    CreateReadOnlyField<string>(sContentEncoding, LGetMemberObject, TScopeMemberType.mtText,
      function: string
      begin
        result := FResponse.ContentEncoding;
      end);
    CreateReadOnlyField<string>(sErrorMessage, LGetMemberObject, TScopeMemberType.mtText,
      function: string
      begin
        result := FResponse.ErrorMessage;
      end);
    CreateReadOnlyField<string>(sFullRequestURI, LGetMemberObject, TScopeMemberType.mtText,
      function: string
      begin
        result := FResponse.FullRequestURI;
      end);
    CreateReadWriteField<string>(sRootElement, LGetMemberObject, TScopeMemberType.mtText,
      function: string
      begin
        result := FResponse.RootElement;
      end,
      procedure(AValue: string)
      begin
        FResponse.RootElement := AValue;
      end);
    CreateReadOnlyField<string>(sServer, LGetMemberObject, TScopeMemberType.mtText,
      function: string
      begin
        result := FResponse.Server;
      end);
    CreateReadOnlyField<integer>(sStatusCode, LGetMemberObject, TScopeMemberType.mtInteger,
      function: integer
      begin
        result := FResponse.StatusCode;
      end);
    CreateReadOnlyField<string>(sStatusText, LGetMemberObject, TScopeMemberType.mtText,
      function: string
      begin
        result := FResponse.StatusText;
      end);
    CreateReadOnlyField<TStrings>(sHeaders, LGetMemberObject, TScopeMemberType.mtMemo,
      function: TStrings
      begin
        result := FResponse.Headers;
      end);
    CreateReadOnlyField<TJsonValue>(sJSONValue, LGetMemberObject, TScopeMemberType.mtObject,
      function: TJsonValue
      begin
        result := FResponse.JSONValue;
      end);
  end;
end;

function TRESTResponseAdapter.GetCanActivate: boolean;
begin
  result := (FResponse <> nil);
end;

function TRESTResponseAdapter.GetSource: TBaseLinkingBindSource;
begin
  result := FResponse;
end;

{ TRESTResponseAdapter.TNotify }

constructor TRESTResponseAdapter.TNotify.Create(const AAdapter: TRESTResponseAdapter);
begin
  FAdapter := AAdapter;
end;

procedure TRESTResponseAdapter.TNotify.PropertyValueChanged(Sender: TObject);
begin
  if Assigned(FAdapter) then
    FAdapter.RefreshFields;
end;

{ TRESTClientAdapter }

procedure TRESTClientAdapter.SetClient(const AClient: TCustomRESTClient);
var
  LActive: boolean;
begin
  if (FClient <> AClient) then
  begin
    if (FClient <> nil) then
    begin
      if FClient.NotifyList <> nil then
        FClient.NotifyList.RemoveNotify(FNotify);
      FClient.RemoveFreeNotification(self);
    end;
    LActive := Active;
    Active := FALSE;
    FClient := AClient;
    if (FClient <> nil) then
    begin
      if (FClient.NotifyList <> nil) then
        FClient.NotifyList.AddNotify(FNotify);
      FClient.FreeNotification(self);
    end;
    if LActive and CanActivate then
      Active := true;
  end;
end;

procedure TRESTClientAdapter.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  /// clean up component-references
  if (Operation = opRemove) then
  begin
    if (AComponent = FClient) then
      Client := nil
  end;
end;

procedure TRESTClientAdapter.ParameterListChanged;
begin
  if (FClient <> nil) and not(csLoading in FClient.ComponentState) then
  begin
    if Active and not Posting then
    begin
      // Force parameters to be recreated
      Active := FALSE;
      Active := true;
    end;
  end;
end;

constructor TRESTClientAdapter.Create(AComponent: TComponent);
begin
  inherited;
  FNotify := TNotify.Create(self);
end;

destructor TRESTClientAdapter.Destroy;
begin
  inherited;
  if (FClient <> nil) then
  begin
    if FClient.NotifyList <> nil then
      FClient.NotifyList.RemoveNotify(FNotify);
  end;
  FNotify.Free;
end;

procedure TRESTClientAdapter.DoChangePosting;
begin
  if FClient <> nil then
    FClient.FPosting := Posting;
end;

procedure TRESTClientAdapter.AddFields;
begin
  AddPropertyFields;
  AddParameterFields;
end;

procedure TRESTClientAdapter.AddParameterFields;

  procedure ClearParameterFields;
  var
    i: integer;
  begin
    for i := Fields.Count - 1 downto 0 do
      if (Fields[i] is TReadWriteField<string>) and (TReadWriteField<string>(Fields[i]).Persistent <> nil) then
        Fields.Delete(i);
  end;

  procedure MakeParameterFieldNames(const ADictionary: TDictionary<string, TRESTRequestParameter>);
  const
    sPrefix = 'Params.';
  var
    i: integer;
    LParams: TRESTRequestParameterList;
    LParam: TRESTRequestParameter;
    LName: string;
    LIndex: integer;
    LSuffix: string;
  begin
    Assert(ADictionary.Count = 0);
    LParams := FClient.Params;
    for i := 0 to LParams.Count - 1 do
    begin
      LParam := LParams[i];
      if TRESTRequestParameterOption.poTransient in LParam.Options then
        // Skip authentication header, for example
        continue;
      LName := LParam.Name;
      if LName = '' then
        LName := IntToStr(LParam.Index);
      LName := sPrefix + LName;
      LIndex := 1;
      LSuffix := '';
      while ADictionary.ContainsKey(LName + LSuffix) do
      begin
        LSuffix := IntToStr(LIndex);
        inc(LIndex);
      end;
      ADictionary.Add(LName + LSuffix, LParam);
    end;
  end;

  procedure MakeParameterField(const AParam: TRESTRequestParameter; const AFieldName: string;
  const AGetMemberObject: IGetMemberObject);
  begin
    CreateReadWriteField<string>(AFieldName, AGetMemberObject, TScopeMemberType.mtText,
      function: string
      begin
        result := AParam.Value;
      end,
      procedure(AValue: string)
      begin
        AParam.Value := AValue;
      end, AParam); // Parameter is member of field

  end;

var
  LDictionary: TDictionary<string, TRESTRequestParameter>;
  LPair: TPair<string, TRESTRequestParameter>;
  LGetMemberObject: IGetMemberObject;
begin
  CheckInactive;
  ClearParameterFields;
  if (FClient <> nil) then
  begin
    LDictionary := TDictionary<string, TRESTRequestParameter>.Create;
    try
      MakeParameterFieldNames(LDictionary);
      LGetMemberObject := TBindSourceAdapterGetMemberObject.Create(self);
      for LPair in LDictionary do
        MakeParameterField(LPair.Value, LPair.Key, LGetMemberObject);
    finally
      LDictionary.Free;
    end;
  end;
end;

procedure TRESTClientAdapter.AddPropertyFields;
const
  sBaseURL = 'BaseURL';
var
  LGetMemberObject: IGetMemberObject;
begin
  CheckInactive;
  ClearFields;
  if (FClient <> nil) then
  begin
    LGetMemberObject := TBindSourceAdapterGetMemberObject.Create(self);
    CreateReadWriteField<string>(sBaseURL, LGetMemberObject, TScopeMemberType.mtText,
      function: string
      begin
        result := FClient.BaseURL;
      end,
      procedure(AValue: string)
      begin
        FClient.BaseURL := AValue;
      end);
  end;
end;

function TRESTClientAdapter.GetCanActivate: boolean;
begin
  result := (FClient <> nil);
end;

procedure TRESTClientAdapter.GetMemberNames(AList: TStrings);
var
  LField: TBindSourceAdapterField;
begin
  for LField in Fields do
  begin
    if (LField is TReadWriteField<string>) then
      // Provide object so that LiveBindings designer can select in designer when member is clicked
      AList.AddObject(LField.MemberName, TReadWriteField<string>(LField).Persistent)
    else
      AList.Add(LField.MemberName);
  end;
end;

function TRESTClientAdapter.GetSource: TBaseLinkingBindSource;
begin
  result := FClient;
end;

{ TCustomRESTClient.TNotify }

procedure TCustomRESTClient.TNotify.ParameterListChanged(Sender: TObject);
begin
  //
end;

{ TCustomRESTClientBindSource }

function TCustomRESTClientBindSource.CreateAdapter: TRESTComponentAdapter;
begin
  FAdapter := TRESTClientAdapter.Create(self);
  result := FAdapter;
end;

function TCustomRESTClientBindSource.GetClient: TCustomRESTClient;
begin
  result := FAdapter.Client;
end;

procedure TCustomRESTClientBindSource.SetClient(const AValue: TCustomRESTClient);
begin
  FAdapter.Client := AValue;
end;

{ TRESTClientAdapter.TNotify }

constructor TRESTClientAdapter.TNotify.Create(const AAdapter: TRESTClientAdapter);
begin
  FAdapter := AAdapter;
end;

procedure TRESTClientAdapter.TNotify.ParameterListChanged(Sender: TObject);
begin
  if Assigned(FAdapter) then
    FAdapter.ParameterListChanged;
end;

procedure TRESTClientAdapter.TNotify.PropertyValueChanged(Sender: TObject);
begin
  if Assigned(FAdapter) then
    FAdapter.RefreshFields;
end;

end.

