{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit Androidapi.JNI.Net;

interface

uses 
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os;

type

  {Class forward declarations}
  JSslError = interface;//android.net.http.SslError
  JSslCertificate_DName = interface;//android.net.http.SslCertificate$DName
  JSslCertificate = interface;//android.net.http.SslCertificate
  JUri_Builder = interface;//android.net.Uri$Builder
  Jnet_Uri = interface;//android.net.Uri

JSslErrorClass = interface(JObjectClass)
['{46C60DBF-8FB1-4C8B-912D-AA3914A70766}']
  {Property Methods}
  function _GetSSL_DATE_INVALID: Integer;
  function _GetSSL_EXPIRED: Integer;
  function _GetSSL_IDMISMATCH: Integer;
  function _GetSSL_INVALID: Integer;
  function _GetSSL_MAX_ERROR: Integer;
  function _GetSSL_NOTYETVALID: Integer;
  function _GetSSL_UNTRUSTED: Integer;
  {Methods}
  function init(error: Integer; certificate: JSslCertificate): JSslError; cdecl; overload;//Deprecated
  function init(error: Integer; certificate: JSslCertificate; url: JString): JSslError; cdecl; overload;
  {Properties}
  property SSL_DATE_INVALID: Integer read _GetSSL_DATE_INVALID;
  property SSL_EXPIRED: Integer read _GetSSL_EXPIRED;
  property SSL_IDMISMATCH: Integer read _GetSSL_IDMISMATCH;
  property SSL_INVALID: Integer read _GetSSL_INVALID;
  property SSL_MAX_ERROR: Integer read _GetSSL_MAX_ERROR;
  property SSL_NOTYETVALID: Integer read _GetSSL_NOTYETVALID;
  property SSL_UNTRUSTED: Integer read _GetSSL_UNTRUSTED;
end;

[JavaSignature('android/net/http/SslError')]
JSslError = interface(JObject)
['{F52273BE-40F1-4502-9E59-72B4E06C0A47}']
  {Methods}
  function addError(error: Integer): Boolean; cdecl;
  function getCertificate: JSslCertificate; cdecl;
  function getPrimaryError: Integer; cdecl;
  function getUrl: JString; cdecl;
  function hasError(error: Integer): Boolean; cdecl;
  function toString: JString; cdecl;
end;
TJSslError = class(TJavaGenericImport<JSslErrorClass, JSslError>) end;

JSslCertificate_DNameClass = interface(JObjectClass)
['{0E7DC9E1-A295-4FB0-B51C-58BC02DD2443}']
  {Methods}
  function init(dName: JString): JSslCertificate_DName; cdecl;
end;

[JavaSignature('android/net/http/SslCertificate$DName')]
JSslCertificate_DName = interface(JObject)
['{EC2D4B89-D8F4-4D92-8861-C012DB574347}']
  {Methods}
  function getCName: JString; cdecl;
  function getDName: JString; cdecl;
  function getOName: JString; cdecl;
  function getUName: JString; cdecl;
end;
TJSslCertificate_DName = class(TJavaGenericImport<JSslCertificate_DNameClass, JSslCertificate_DName>) end;

JSslCertificateClass = interface(JObjectClass)
['{015060F6-A45B-43B1-99F2-25FF14860D34}']
  {Methods}
  function init(issuedTo: JString; issuedBy: JString; validNotBefore: JString; validNotAfter: JString): JSslCertificate; cdecl; overload;//Deprecated
  function init(issuedTo: JString; issuedBy: JString; validNotBefore: JDate; validNotAfter: JDate): JSslCertificate; cdecl; overload;//Deprecated
  function restoreState(bundle: JBundle): JSslCertificate; cdecl;
  function saveState(certificate: JSslCertificate): JBundle; cdecl;
end;

[JavaSignature('android/net/http/SslCertificate')]
JSslCertificate = interface(JObject)
['{71905E46-8089-401A-ACA0-7C8CAB5BDCFB}']
  {Methods}
  function getIssuedBy: JSslCertificate_DName; cdecl;
  function getIssuedTo: JSslCertificate_DName; cdecl;
  function getValidNotAfter: JString; cdecl;//Deprecated
  function getValidNotAfterDate: JDate; cdecl;
  function getValidNotBefore: JString; cdecl;//Deprecated
  function getValidNotBeforeDate: JDate; cdecl;
  function toString: JString; cdecl;
end;
TJSslCertificate = class(TJavaGenericImport<JSslCertificateClass, JSslCertificate>) end;

JUri_BuilderClass = interface(JObjectClass)
['{A28CFEC0-2796-4427-A612-4F8D7389ECEB}']
  {Methods}
  function init: JUri_Builder; cdecl;
end;

[JavaSignature('android/net/Uri$Builder')]
JUri_Builder = interface(JObject)
['{764C6F4F-0EE9-4CC4-98C4-C2FDE6E73B7A}']
  {Methods}
  function appendEncodedPath(newSegment: JString): JUri_Builder; cdecl;
  function appendPath(newSegment: JString): JUri_Builder; cdecl;
  function appendQueryParameter(key: JString; value: JString): JUri_Builder; cdecl;
  function authority(authority: JString): JUri_Builder; cdecl;
  function build: Jnet_Uri; cdecl;
  function clearQuery: JUri_Builder; cdecl;
  function encodedAuthority(authority: JString): JUri_Builder; cdecl;
  function encodedFragment(fragment: JString): JUri_Builder; cdecl;
  function encodedOpaquePart(opaquePart: JString): JUri_Builder; cdecl;
  function encodedPath(path: JString): JUri_Builder; cdecl;
  function encodedQuery(query: JString): JUri_Builder; cdecl;
  function fragment(fragment: JString): JUri_Builder; cdecl;
  function opaquePart(opaquePart: JString): JUri_Builder; cdecl;
  function path(path: JString): JUri_Builder; cdecl;
  function query(query: JString): JUri_Builder; cdecl;
  function scheme(scheme: JString): JUri_Builder; cdecl;
  function toString: JString; cdecl;
end;
TJUri_Builder = class(TJavaGenericImport<JUri_BuilderClass, JUri_Builder>) end;

Jnet_UriClass = interface(JObjectClass)
['{26008A37-774A-4F63-817A-639A3B749539}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  function _GetEMPTY: Jnet_Uri;
  {Methods}
  function decode(s: JString): JString; cdecl;
  function encode(s: JString): JString; cdecl; overload;
  function encode(s: JString; allow: JString): JString; cdecl; overload;
  function fromFile(file_: JFile): Jnet_Uri; cdecl;
  function fromParts(scheme: JString; ssp: JString; fragment: JString): Jnet_Uri; cdecl;
  function parse(uriString: JString): Jnet_Uri; cdecl;
  function withAppendedPath(baseUri: Jnet_Uri; pathSegment: JString): Jnet_Uri; cdecl;
  procedure writeToParcel(out_: JParcel; uri: Jnet_Uri); cdecl;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
  property EMPTY: Jnet_Uri read _GetEMPTY;
end;

[JavaSignature('android/net/Uri')]
Jnet_Uri = interface(JObject)
['{FBC1913D-A35B-4E62-ABD1-7575BEC0488E}']
  {Methods}
  function buildUpon: JUri_Builder; cdecl;
  function compareTo(other: Jnet_Uri): Integer; cdecl;
  function equals(o: JObject): Boolean; cdecl;
  function getAuthority: JString; cdecl;
  function getBooleanQueryParameter(key: JString; defaultValue: Boolean): Boolean; cdecl;
  function getEncodedAuthority: JString; cdecl;
  function getEncodedFragment: JString; cdecl;
  function getEncodedPath: JString; cdecl;
  function getEncodedQuery: JString; cdecl;
  function getEncodedSchemeSpecificPart: JString; cdecl;
  function getEncodedUserInfo: JString; cdecl;
  function getFragment: JString; cdecl;
  function getHost: JString; cdecl;
  function getLastPathSegment: JString; cdecl;
  function getPath: JString; cdecl;
  function getPathSegments: JList; cdecl;
  function getPort: Integer; cdecl;
  function getQuery: JString; cdecl;
  function getQueryParameter(key: JString): JString; cdecl;
  function getQueryParameterNames: JSet; cdecl;
  function getQueryParameters(key: JString): JList; cdecl;
  function getScheme: JString; cdecl;
  function getSchemeSpecificPart: JString; cdecl;
  function getUserInfo: JString; cdecl;
  function hashCode: Integer; cdecl;
  function isAbsolute: Boolean; cdecl;
  function isHierarchical: Boolean; cdecl;
  function isOpaque: Boolean; cdecl;
  function isRelative: Boolean; cdecl;
  function normalizeScheme: Jnet_Uri; cdecl;
  function toString: JString; cdecl;
end;
TJnet_Uri = class(TJavaGenericImport<Jnet_UriClass, Jnet_Uri>) end;




implementation

begin

end.


