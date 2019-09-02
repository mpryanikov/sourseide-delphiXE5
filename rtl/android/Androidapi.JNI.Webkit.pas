{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit Androidapi.JNI.Webkit;

interface

uses 
  Androidapi.JNIBridge,
  Androidapi.JNI.Os,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.App,
  Androidapi.JNI.Net,
  Androidapi.JNI.Widget,
  Androidapi.JNI.Util,
  Androidapi.JNI.GraphicsContentViewText;

type

  {Class forward declarations}
  JPluginStub = interface;//android.webkit.PluginStub
  JGeolocationPermissions_Callback = interface;//android.webkit.GeolocationPermissions$Callback
  JWebChromeClient_CustomViewCallback = interface;//android.webkit.WebChromeClient$CustomViewCallback
  JURLUtil = interface;//android.webkit.URLUtil
  JMimeTypeMap = interface;//android.webkit.MimeTypeMap
  JWebChromeClient = interface;//android.webkit.WebChromeClient
  JWebViewDatabase = interface;//android.webkit.WebViewDatabase
  JWebStorage_QuotaUpdater = interface;//android.webkit.WebStorage$QuotaUpdater
  JWebStorage = interface;//android.webkit.WebStorage
  JWebIconDatabase = interface;//android.webkit.WebIconDatabase
  JWebSettings_ZoomDensity = interface;//android.webkit.WebSettings$ZoomDensity
  JWebSettings_LayoutAlgorithm = interface;//android.webkit.WebSettings$LayoutAlgorithm
  JDateSorter = interface;//android.webkit.DateSorter
  JWebView = interface;//android.webkit.WebView
  JJavascriptInterface = interface;//android.webkit.JavascriptInterface
  JWebStorage_Origin = interface;//android.webkit.WebStorage$Origin
  JDownloadListener = interface;//android.webkit.DownloadListener
  JFindActionModeCallback_NoAction = interface;//android.webkit.FindActionModeCallback$NoAction
  JWebSettings_TextSize = interface;//android.webkit.WebSettings$TextSize
  JConsoleMessage_MessageLevel = interface;//android.webkit.ConsoleMessage$MessageLevel
  JByteArrayBuilder_Chunk = interface;//android.webkit.ByteArrayBuilder$Chunk
  JWebSettings_RenderPriority = interface;//android.webkit.WebSettings$RenderPriority
  JWebBackForwardList = interface;//android.webkit.WebBackForwardList
  JJsResult = interface;//android.webkit.JsResult
  JJsPromptResult = interface;//android.webkit.JsPromptResult
  JConsoleMessage = interface;//android.webkit.ConsoleMessage
  JCookieSyncManager = interface;//android.webkit.CookieSyncManager
  JWebViewClient = interface;//android.webkit.WebViewClient
  JWebResourceResponse = interface;//android.webkit.WebResourceResponse
  JHttpAuthHandler = interface;//android.webkit.HttpAuthHandler
  JWebView_HitTestResult = interface;//android.webkit.WebView$HitTestResult
  JWebViewFragment = interface;//android.webkit.WebViewFragment
  JGeolocationPermissions = interface;//android.webkit.GeolocationPermissions
  JValueCallback = interface;//android.webkit.ValueCallback
  JWebView_WebViewTransport = interface;//android.webkit.WebView$WebViewTransport
  JCookieManager = interface;//android.webkit.CookieManager
  JWebView_FindListener = interface;//android.webkit.WebView$FindListener
  JWebHistoryItem = interface;//android.webkit.WebHistoryItem
  JWebIconDatabase_IconListener = interface;//android.webkit.WebIconDatabase$IconListener
  JSslErrorHandler = interface;//android.webkit.SslErrorHandler
  JWebView_PictureListener = interface;//android.webkit.WebView$PictureListener
  JWebSettings_PluginState = interface;//android.webkit.WebSettings$PluginState
  JWebSettings = interface;//android.webkit.WebSettings

JPluginStubClass = interface(IJavaClass)
['{91084F0F-96A4-45B7-B49C-DD7FC6559594}']
end;

[JavaSignature('android/webkit/PluginStub')]
JPluginStub = interface(IJavaInstance)
['{243F5981-F358-4C6C-AB67-08F1B0C2D706}']
  {Methods}
  function getEmbeddedView(NPP: Integer; context: JContext): JView; cdecl;
  function getFullScreenView(NPP: Integer; context: JContext): JView; cdecl;
end;
TJPluginStub = class(TJavaGenericImport<JPluginStubClass, JPluginStub>) end;

JGeolocationPermissions_CallbackClass = interface(IJavaClass)
['{38C6FDC6-38D6-4C2E-A2DA-2F548B75F1EA}']
end;

[JavaSignature('android/webkit/GeolocationPermissions$Callback')]
JGeolocationPermissions_Callback = interface(IJavaInstance)
['{A8E62A13-B89F-4680-94F1-FAC7FC900D31}']
  {Methods}
  procedure invoke(origin: JString; allow: Boolean; retain: Boolean); cdecl;
end;
TJGeolocationPermissions_Callback = class(TJavaGenericImport<JGeolocationPermissions_CallbackClass, JGeolocationPermissions_Callback>) end;

JWebChromeClient_CustomViewCallbackClass = interface(IJavaClass)
['{4B9AF0A9-F503-4054-B3BF-498AD5CD50F8}']
end;

[JavaSignature('android/webkit/WebChromeClient$CustomViewCallback')]
JWebChromeClient_CustomViewCallback = interface(IJavaInstance)
['{2C042439-AB81-4675-9E99-CA4E3C8D84E1}']
  {Methods}
  procedure onCustomViewHidden; cdecl;
end;
TJWebChromeClient_CustomViewCallback = class(TJavaGenericImport<JWebChromeClient_CustomViewCallbackClass, JWebChromeClient_CustomViewCallback>) end;

JURLUtilClass = interface(JObjectClass)
['{F338F90A-949B-4324-ABB8-FEAE849ED7C9}']
  {Methods}
  function init: JURLUtil; cdecl;
  function composeSearchUrl(inQuery: JString; template: JString; queryPlaceHolder: JString): JString; cdecl;
  function decode(url: TJavaArray<Byte>): TJavaArray<Byte>; cdecl;
  function guessFileName(url: JString; contentDisposition: JString; mimeType: JString): JString; cdecl;
  function guessUrl(inUrl: JString): JString; cdecl;
  function isAboutUrl(url: JString): Boolean; cdecl;
  function isAssetUrl(url: JString): Boolean; cdecl;
  function isContentUrl(url: JString): Boolean; cdecl;
  function isCookielessProxyUrl(url: JString): Boolean; cdecl;//Deprecated
  function isDataUrl(url: JString): Boolean; cdecl;
  function isFileUrl(url: JString): Boolean; cdecl;
  function isHttpUrl(url: JString): Boolean; cdecl;
  function isHttpsUrl(url: JString): Boolean; cdecl;
  function isJavaScriptUrl(url: JString): Boolean; cdecl;
  function isNetworkUrl(url: JString): Boolean; cdecl;
  function isValidUrl(url: JString): Boolean; cdecl;
  function stripAnchor(url: JString): JString; cdecl;
end;

[JavaSignature('android/webkit/URLUtil')]
JURLUtil = interface(JObject)
['{C40526A5-9EB7-414D-90AC-36D4F2B55993}']
end;
TJURLUtil = class(TJavaGenericImport<JURLUtilClass, JURLUtil>) end;

JMimeTypeMapClass = interface(JObjectClass)
['{590CAE2F-22DD-48BB-8A6A-4B73936BA71C}']
  {Methods}
  function getFileExtensionFromUrl(url: JString): JString; cdecl;
  function getSingleton: JMimeTypeMap; cdecl;
end;

[JavaSignature('android/webkit/MimeTypeMap')]
JMimeTypeMap = interface(JObject)
['{40155926-B3E3-4365-99C5-A928F3EB30C7}']
  {Methods}
  function getExtensionFromMimeType(mimeType: JString): JString; cdecl;
  function getMimeTypeFromExtension(extension: JString): JString; cdecl;
  function hasExtension(extension: JString): Boolean; cdecl;
  function hasMimeType(mimeType: JString): Boolean; cdecl;
end;
TJMimeTypeMap = class(TJavaGenericImport<JMimeTypeMapClass, JMimeTypeMap>) end;

JWebChromeClientClass = interface(JObjectClass)
['{0DEC9AF7-FCC8-4616-86FD-4974D4FCB026}']
  {Methods}
  function init: JWebChromeClient; cdecl;
end;

[JavaSignature('android/webkit/WebChromeClient')]
JWebChromeClient = interface(JObject)
['{B8E15ECA-8A1A-4321-8E4D-5881F6D8F9EF}']
  {Methods}
  function getDefaultVideoPoster: JBitmap; cdecl;
  function getVideoLoadingProgressView: JView; cdecl;
  procedure getVisitedHistory(callback: TJavaObjectArray<JValueCallback>); cdecl;
  procedure onCloseWindow(window: JWebView); cdecl;
  procedure onConsoleMessage(message: JString; lineNumber: Integer; sourceID: JString); cdecl; overload;//Deprecated
  function onConsoleMessage(consoleMessage: JConsoleMessage): Boolean; cdecl; overload;
  function onCreateWindow(view: JWebView; isDialog: Boolean; isUserGesture: Boolean; resultMsg: JMessage): Boolean; cdecl;
  procedure onExceededDatabaseQuota(url: JString; databaseIdentifier: JString; quota: Int64; estimatedDatabaseSize: Int64; totalQuota: Int64; quotaUpdater: JWebStorage_QuotaUpdater); cdecl;
  procedure onGeolocationPermissionsHidePrompt; cdecl;
  procedure onGeolocationPermissionsShowPrompt(origin: JString; callback: JGeolocationPermissions_Callback); cdecl;
  procedure onHideCustomView; cdecl;
  function onJsAlert(view: JWebView; url: JString; message: JString; result: JJsResult): Boolean; cdecl;
  function onJsBeforeUnload(view: JWebView; url: JString; message: JString; result: JJsResult): Boolean; cdecl;
  function onJsConfirm(view: JWebView; url: JString; message: JString; result: JJsResult): Boolean; cdecl;
  function onJsPrompt(view: JWebView; url: JString; message: JString; defaultValue: JString; result: JJsPromptResult): Boolean; cdecl;
  function onJsTimeout: Boolean; cdecl;//Deprecated
  procedure onProgressChanged(view: JWebView; newProgress: Integer); cdecl;
  procedure onReachedMaxAppCacheSize(requiredStorage: Int64; quota: Int64; quotaUpdater: JWebStorage_QuotaUpdater); cdecl;
  procedure onReceivedIcon(view: JWebView; icon: JBitmap); cdecl;
  procedure onReceivedTitle(view: JWebView; title: JString); cdecl;
  procedure onReceivedTouchIconUrl(view: JWebView; url: JString; precomposed: Boolean); cdecl;
  procedure onRequestFocus(view: JWebView); cdecl;
  procedure onShowCustomView(view: JView; callback: JWebChromeClient_CustomViewCallback); cdecl; overload;
  procedure onShowCustomView(view: JView; requestedOrientation: Integer; callback: JWebChromeClient_CustomViewCallback); cdecl; overload;
end;
TJWebChromeClient = class(TJavaGenericImport<JWebChromeClientClass, JWebChromeClient>) end;

JWebViewDatabaseClass = interface(JObjectClass)
['{DEA27CB2-E5C8-497E-9A3E-D9491479FFD9}']
  {Methods}
  function getInstance(context: JContext): JWebViewDatabase; cdecl;
end;

[JavaSignature('android/webkit/WebViewDatabase')]
JWebViewDatabase = interface(JObject)
['{CDF3B654-354D-4CA9-A8ED-C6295AAABC7C}']
  {Methods}
  procedure clearFormData; cdecl;
  procedure clearHttpAuthUsernamePassword; cdecl;
  procedure clearUsernamePassword; cdecl;
  function hasFormData: Boolean; cdecl;
  function hasHttpAuthUsernamePassword: Boolean; cdecl;
  function hasUsernamePassword: Boolean; cdecl;
end;
TJWebViewDatabase = class(TJavaGenericImport<JWebViewDatabaseClass, JWebViewDatabase>) end;

JWebStorage_QuotaUpdaterClass = interface(IJavaClass)
['{C5E7DCCF-EBD2-466F-9916-D3C4F092EA21}']
end;

[JavaSignature('android/webkit/WebStorage$QuotaUpdater')]
JWebStorage_QuotaUpdater = interface(IJavaInstance)
['{D6DE6F2C-845A-45FD-BC3E-10286ABF0E01}']
  {Methods}
  procedure updateQuota(newQuota: Int64); cdecl;
end;
TJWebStorage_QuotaUpdater = class(TJavaGenericImport<JWebStorage_QuotaUpdaterClass, JWebStorage_QuotaUpdater>) end;

JWebStorageClass = interface(JObjectClass)
['{98C06137-20FC-44E2-B2F0-2D72D0FC3010}']
  {Methods}
  function getInstance: JWebStorage; cdecl;
end;

[JavaSignature('android/webkit/WebStorage')]
JWebStorage = interface(JObject)
['{702F2A6D-0A1E-4916-B16D-8F8C492A03C5}']
  {Methods}
  procedure deleteAllData; cdecl;
  procedure deleteOrigin(origin: JString); cdecl;
  procedure getOrigins(callback: JValueCallback); cdecl;
  procedure getQuotaForOrigin(origin: JString; callback: JValueCallback); cdecl;
  procedure getUsageForOrigin(origin: JString; callback: JValueCallback); cdecl;
  procedure setQuotaForOrigin(origin: JString; quota: Int64); cdecl;
end;
TJWebStorage = class(TJavaGenericImport<JWebStorageClass, JWebStorage>) end;

JWebIconDatabaseClass = interface(JObjectClass)
['{1D432D65-2ABB-4EFD-A989-7D0DF89F8868}']
  {Methods}
  function getInstance: JWebIconDatabase; cdecl;
end;

[JavaSignature('android/webkit/WebIconDatabase')]
JWebIconDatabase = interface(JObject)
['{C036A43D-DD31-4FFF-8CAD-0ED5844378B6}']
  {Methods}
  procedure close; cdecl;
  procedure open(path: JString); cdecl;
  procedure releaseIconForPageUrl(url: JString); cdecl;
  procedure removeAllIcons; cdecl;
  procedure requestIconForPageUrl(url: JString; listener: JWebIconDatabase_IconListener); cdecl;
  procedure retainIconForPageUrl(url: JString); cdecl;
end;
TJWebIconDatabase = class(TJavaGenericImport<JWebIconDatabaseClass, JWebIconDatabase>) end;

JWebSettings_ZoomDensityClass = interface(JEnumClass)
['{CF1B1626-5547-47A4-98A4-6CCFA10457DC}']
  {Property Methods}
  function _GetCLOSE: JWebSettings_ZoomDensity;
  function _GetFAR: JWebSettings_ZoomDensity;
  function _GetMEDIUM: JWebSettings_ZoomDensity;
  {Methods}
  function valueOf(name: JString): JWebSettings_ZoomDensity; cdecl;
  function values: TJavaObjectArray<JWebSettings_ZoomDensity>; cdecl;
  {Properties}
  property CLOSE: JWebSettings_ZoomDensity read _GetCLOSE;
  property FAR: JWebSettings_ZoomDensity read _GetFAR;
  property MEDIUM: JWebSettings_ZoomDensity read _GetMEDIUM;
end;

[JavaSignature('android/webkit/WebSettings$ZoomDensity')]
JWebSettings_ZoomDensity = interface(JEnum)
['{99F8B33F-ADFF-4C54-9151-F0DC57EFF989}']
end;
TJWebSettings_ZoomDensity = class(TJavaGenericImport<JWebSettings_ZoomDensityClass, JWebSettings_ZoomDensity>) end;

JWebSettings_LayoutAlgorithmClass = interface(JEnumClass)
['{6D48AC59-3F0B-4CE1-B9E3-C7A0894915A2}']
  {Property Methods}
  function _GetNARROW_COLUMNS: JWebSettings_LayoutAlgorithm;
  function _GetNORMAL: JWebSettings_LayoutAlgorithm;
  function _GetSINGLE_COLUMN: JWebSettings_LayoutAlgorithm;
  {Methods}
  function valueOf(name: JString): JWebSettings_LayoutAlgorithm; cdecl;
  function values: TJavaObjectArray<JWebSettings_LayoutAlgorithm>; cdecl;
  {Properties}
  property NARROW_COLUMNS: JWebSettings_LayoutAlgorithm read _GetNARROW_COLUMNS;
  property NORMAL: JWebSettings_LayoutAlgorithm read _GetNORMAL;
  property SINGLE_COLUMN: JWebSettings_LayoutAlgorithm read _GetSINGLE_COLUMN;
end;

[JavaSignature('android/webkit/WebSettings$LayoutAlgorithm')]
JWebSettings_LayoutAlgorithm = interface(JEnum)
['{907CB7D6-3ECB-449F-9BE5-D3E4FAF0A877}']
end;
TJWebSettings_LayoutAlgorithm = class(TJavaGenericImport<JWebSettings_LayoutAlgorithmClass, JWebSettings_LayoutAlgorithm>) end;

JDateSorterClass = interface(JObjectClass)
['{E5ED8EFC-3CB5-4650-A9B3-6B09646B0C50}']
  {Property Methods}
  function _GetDAY_COUNT: Integer;
  {Methods}
  function init(context: JContext): JDateSorter; cdecl;
  {Properties}
  property DAY_COUNT: Integer read _GetDAY_COUNT;
end;

[JavaSignature('android/webkit/DateSorter')]
JDateSorter = interface(JObject)
['{566DE920-3B0F-4AF9-A8EF-BC81E5E3499B}']
  {Methods}
  function getBoundary(index: Integer): Int64; cdecl;
  function getIndex(time: Int64): Integer; cdecl;
  function getLabel(index: Integer): JString; cdecl;
end;
TJDateSorter = class(TJavaGenericImport<JDateSorterClass, JDateSorter>) end;

JWebViewClass = interface(JAbsoluteLayoutClass)
['{57C30F7F-F8C7-4C19-859E-073DC4DA4250}']
  {Property Methods}
  function _GetSCHEME_GEO: JString;
  function _GetSCHEME_MAILTO: JString;
  function _GetSCHEME_TEL: JString;
  {Methods}
  function init(context: JContext): JWebView; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet): JWebView; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet; defStyle: Integer): JWebView; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet; defStyle: Integer; privateBrowsing: Boolean): JWebView; cdecl; overload;//Deprecated
  function findAddress(addr: JString): JString; cdecl;
  {Properties}
  property SCHEME_GEO: JString read _GetSCHEME_GEO;
  property SCHEME_MAILTO: JString read _GetSCHEME_MAILTO;
  property SCHEME_TEL: JString read _GetSCHEME_TEL;
end;

[JavaSignature('android/webkit/WebView')]
JWebView = interface(JAbsoluteLayout)
['{0001776D-86A0-43B3-A64C-C6FEA095AD91}']
  {Methods}
  procedure addJavascriptInterface(object_: JObject; name: JString); cdecl;
  function canGoBack: Boolean; cdecl;
  function canGoBackOrForward(steps: Integer): Boolean; cdecl;
  function canGoForward: Boolean; cdecl;
  function canZoomIn: Boolean; cdecl;//Deprecated
  function canZoomOut: Boolean; cdecl;//Deprecated
  function capturePicture: JPicture; cdecl;
  procedure clearCache(includeDiskFiles: Boolean); cdecl;
  procedure clearFormData; cdecl;
  procedure clearHistory; cdecl;
  procedure clearMatches; cdecl;
  procedure clearSslPreferences; cdecl;
  procedure clearView; cdecl;
  procedure computeScroll; cdecl;
  function copyBackForwardList: JWebBackForwardList; cdecl;
  procedure destroy; cdecl;
  function dispatchKeyEvent(event: JKeyEvent): Boolean; cdecl;
  procedure documentHasImages(response: JMessage); cdecl;
  function findAll(find: JString): Integer; cdecl;//Deprecated
  procedure findAllAsync(find: JString); cdecl;
  procedure findNext(forward: Boolean); cdecl;
  procedure flingScroll(vx: Integer; vy: Integer); cdecl;
  procedure freeMemory; cdecl;
  function getCertificate: JSslCertificate; cdecl;
  function getContentHeight: Integer; cdecl;
  function getFavicon: JBitmap; cdecl;
  function getHitTestResult: JWebView_HitTestResult; cdecl;
  function getHttpAuthUsernamePassword(host: JString; realm: JString): TJavaObjectArray<JString>; cdecl;
  function getOriginalUrl: JString; cdecl;
  function getProgress: Integer; cdecl;
  function getScale: Single; cdecl;//Deprecated
  function getSettings: JWebSettings; cdecl;
  function getTitle: JString; cdecl;
  function getUrl: JString; cdecl;
  procedure goBack; cdecl;
  procedure goBackOrForward(steps: Integer); cdecl;
  procedure goForward; cdecl;
  procedure invokeZoomPicker; cdecl;
  function isPrivateBrowsingEnabled: Boolean; cdecl;
  procedure loadData(data: JString; mimeType: JString; encoding: JString); cdecl;
  procedure loadDataWithBaseURL(baseUrl: JString; data: JString; mimeType: JString; encoding: JString; historyUrl: JString); cdecl;
  procedure loadUrl(url: JString; additionalHttpHeaders: JMap); cdecl; overload;
  procedure loadUrl(url: JString); cdecl; overload;
  procedure onChildViewAdded(parent: JView; child: JView); cdecl;//Deprecated
  procedure onChildViewRemoved(p: JView; child: JView); cdecl;//Deprecated
  function onCreateInputConnection(outAttrs: JEditorInfo): JInputConnection; cdecl;
  function onGenericMotionEvent(event: JMotionEvent): Boolean; cdecl;
  procedure onGlobalFocusChanged(oldFocus: JView; newFocus: JView); cdecl;//Deprecated
  function onHoverEvent(event: JMotionEvent): Boolean; cdecl;
  function onKeyDown(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  function onKeyMultiple(keyCode: Integer; repeatCount: Integer; event: JKeyEvent): Boolean; cdecl;
  function onKeyUp(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  procedure onPause; cdecl;
  procedure onResume; cdecl;
  function onTouchEvent(event: JMotionEvent): Boolean; cdecl;
  function onTrackballEvent(event: JMotionEvent): Boolean; cdecl;
  procedure onWindowFocusChanged(hasWindowFocus: Boolean); cdecl;
  function overlayHorizontalScrollbar: Boolean; cdecl;
  function overlayVerticalScrollbar: Boolean; cdecl;
  function pageDown(bottom: Boolean): Boolean; cdecl;
  function pageUp(top: Boolean): Boolean; cdecl;
  procedure pauseTimers; cdecl;
  function performAccessibilityAction(action: Integer; arguments: JBundle): Boolean; cdecl;
  function performLongClick: Boolean; cdecl;
  procedure postUrl(url: JString; postData: TJavaArray<Byte>); cdecl;
  procedure reload; cdecl;
  procedure removeJavascriptInterface(name: JString); cdecl;
  function requestChildRectangleOnScreen(child: JView; rect: JRect; immediate: Boolean): Boolean; cdecl;
  function requestFocus(direction: Integer; previouslyFocusedRect: JRect): Boolean; cdecl;
  procedure requestFocusNodeHref(hrefMsg: JMessage); cdecl;
  procedure requestImageRef(msg: JMessage); cdecl;
  function restoreState(inState: JBundle): JWebBackForwardList; cdecl;
  procedure resumeTimers; cdecl;
  procedure savePassword(host: JString; username: JString; password: JString); cdecl;
  function saveState(outState: JBundle): JWebBackForwardList; cdecl;
  procedure saveWebArchive(filename: JString); cdecl; overload;
  procedure saveWebArchive(basename: JString; autoname: Boolean; callback: JValueCallback); cdecl; overload;
  procedure setBackgroundColor(color: Integer); cdecl;
  procedure setCertificate(certificate: JSslCertificate); cdecl;//Deprecated
  procedure setDownloadListener(listener: JDownloadListener); cdecl;
  procedure setFindListener(listener: JWebView_FindListener); cdecl;
  procedure setHorizontalScrollbarOverlay(overlay: Boolean); cdecl;
  procedure setHttpAuthUsernamePassword(host: JString; realm: JString; username: JString; password: JString); cdecl;
  procedure setInitialScale(scaleInPercent: Integer); cdecl;
  procedure setLayerType(layerType: Integer; paint: JPaint); cdecl;
  procedure setLayoutParams(params: JViewGroup_LayoutParams); cdecl;
  procedure setMapTrackballToArrowKeys(setMap: Boolean); cdecl;//Deprecated
  procedure setNetworkAvailable(networkUp: Boolean); cdecl;
  procedure setOverScrollMode(mode: Integer); cdecl;
  procedure setPictureListener(listener: JWebView_PictureListener); cdecl;//Deprecated
  procedure setScrollBarStyle(style: Integer); cdecl;
  procedure setVerticalScrollbarOverlay(overlay: Boolean); cdecl;
  procedure setWebChromeClient(client: JWebChromeClient); cdecl;
  procedure setWebViewClient(client: JWebViewClient); cdecl;
  function shouldDelayChildPressedState: Boolean; cdecl;//Deprecated
  function showFindDialog(text: JString; showIme: Boolean): Boolean; cdecl;
  procedure stopLoading; cdecl;
  function zoomIn: Boolean; cdecl;
  function zoomOut: Boolean; cdecl;
end;
TJWebView = class(TJavaGenericImport<JWebViewClass, JWebView>) end;

JJavascriptInterfaceClass = interface(JObjectClass)
['{7D851A79-CBFE-4C5B-A5DC-44A4877FCF7B}']
end;

[JavaSignature('android/webkit/JavascriptInterface')]
JJavascriptInterface = interface(JObject)
['{4F64688D-578A-4D9A-9FD3-1014C2FDFE50}']
end;
TJJavascriptInterface = class(TJavaGenericImport<JJavascriptInterfaceClass, JJavascriptInterface>) end;

JWebStorage_OriginClass = interface(JObjectClass)
['{2EBC8D88-82DC-40C6-86D1-F5F8407B9DB1}']
end;

[JavaSignature('android/webkit/WebStorage$Origin')]
JWebStorage_Origin = interface(JObject)
['{EF23BA1B-CC0C-46D4-960F-DDB999281541}']
  {Methods}
  function getOrigin: JString; cdecl;
  function getQuota: Int64; cdecl;
  function getUsage: Int64; cdecl;
end;
TJWebStorage_Origin = class(TJavaGenericImport<JWebStorage_OriginClass, JWebStorage_Origin>) end;

JDownloadListenerClass = interface(IJavaClass)
['{DAFCF031-2702-4450-A30A-8CBB59C687FA}']
end;

[JavaSignature('android/webkit/DownloadListener')]
JDownloadListener = interface(IJavaInstance)
['{BB9F3B4B-B375-4870-AE50-66DDCB0253F8}']
  {Methods}
  procedure onDownloadStart(url: JString; userAgent: JString; contentDisposition: JString; mimetype: JString; contentLength: Int64); cdecl;
end;
TJDownloadListener = class(TJavaGenericImport<JDownloadListenerClass, JDownloadListener>) end;

JFindActionModeCallback_NoActionClass = interface(JObjectClass)
['{2D3A01CA-9E0E-469F-94E5-EE9F445434CD}']
  {Methods}
  function init: JFindActionModeCallback_NoAction; cdecl;
end;

[JavaSignature('android/webkit/FindActionModeCallback$NoAction')]
JFindActionModeCallback_NoAction = interface(JObject)
['{51F46775-C62A-41BD-AB3B-AC1555BD3F40}']
  {Methods}
  function onActionItemClicked(mode: JActionMode; item: JMenuItem): Boolean; cdecl;
  function onCreateActionMode(mode: JActionMode; menu: JMenu): Boolean; cdecl;
  procedure onDestroyActionMode(mode: JActionMode); cdecl;
  function onPrepareActionMode(mode: JActionMode; menu: JMenu): Boolean; cdecl;
end;
TJFindActionModeCallback_NoAction = class(TJavaGenericImport<JFindActionModeCallback_NoActionClass, JFindActionModeCallback_NoAction>) end;

JWebSettings_TextSizeClass = interface(JEnumClass)
['{ED845FF6-09FA-41C4-B3CD-81718A39E35D}']
  {Property Methods}
  function _GetLARGER: JWebSettings_TextSize;
  function _GetLARGEST: JWebSettings_TextSize;
  function _GetNORMAL: JWebSettings_TextSize;
  function _GetSMALLER: JWebSettings_TextSize;
  function _GetSMALLEST: JWebSettings_TextSize;
  {Methods}
  function valueOf(name: JString): JWebSettings_TextSize; cdecl;
  function values: TJavaObjectArray<JWebSettings_TextSize>; cdecl;
  {Properties}
  property LARGER: JWebSettings_TextSize read _GetLARGER;
  property LARGEST: JWebSettings_TextSize read _GetLARGEST;
  property NORMAL: JWebSettings_TextSize read _GetNORMAL;
  property SMALLER: JWebSettings_TextSize read _GetSMALLER;
  property SMALLEST: JWebSettings_TextSize read _GetSMALLEST;
end;

[JavaSignature('android/webkit/WebSettings$TextSize')]
JWebSettings_TextSize = interface(JEnum)
['{416C0F01-FDDA-4159-9491-92F0241FFA7D}']
end;
TJWebSettings_TextSize = class(TJavaGenericImport<JWebSettings_TextSizeClass, JWebSettings_TextSize>) end;

JConsoleMessage_MessageLevelClass = interface(JEnumClass)
['{EBA1E801-E409-4FD2-B5D0-A21E3D73C05A}']
  {Property Methods}
  function _GetDEBUG: JConsoleMessage_MessageLevel;
  function _GetERROR: JConsoleMessage_MessageLevel;
  function _GetLOG: JConsoleMessage_MessageLevel;
  function _GetTIP: JConsoleMessage_MessageLevel;
  function _GetWARNING: JConsoleMessage_MessageLevel;
  {Methods}
  function valueOf(name: JString): JConsoleMessage_MessageLevel; cdecl;
  function values: TJavaObjectArray<JConsoleMessage_MessageLevel>; cdecl;
  {Properties}
  property DEBUG: JConsoleMessage_MessageLevel read _GetDEBUG;
  property ERROR: JConsoleMessage_MessageLevel read _GetERROR;
  property LOG: JConsoleMessage_MessageLevel read _GetLOG;
  property TIP: JConsoleMessage_MessageLevel read _GetTIP;
  property WARNING: JConsoleMessage_MessageLevel read _GetWARNING;
end;

[JavaSignature('android/webkit/ConsoleMessage$MessageLevel')]
JConsoleMessage_MessageLevel = interface(JEnum)
['{D2D9F689-DB98-41EE-A319-0BE781C70A98}']
end;
TJConsoleMessage_MessageLevel = class(TJavaGenericImport<JConsoleMessage_MessageLevelClass, JConsoleMessage_MessageLevel>) end;

JByteArrayBuilder_ChunkClass = interface(JObjectClass)
['{146DD94F-5E88-437F-825F-A4C9442C1E07}']
  {Methods}
  function init(length: Integer): JByteArrayBuilder_Chunk; cdecl;
end;

[JavaSignature('android/webkit/ByteArrayBuilder$Chunk')]
JByteArrayBuilder_Chunk = interface(JObject)
['{6C7BCC12-C130-47E1-824D-2224CB0DCBC3}']
  {Property Methods}
  function _GetmArray: TJavaArray<Byte>;
  procedure _SetmArray(Value: TJavaArray<Byte>);
  function _GetmLength: Integer;
  procedure _SetmLength(Value: Integer);
  {Methods}
  procedure release; cdecl;
  {Properties}
  property mArray: TJavaArray<Byte> read _GetmArray write _SetmArray;
  property mLength: Integer read _GetmLength write _SetmLength;
end;
TJByteArrayBuilder_Chunk = class(TJavaGenericImport<JByteArrayBuilder_ChunkClass, JByteArrayBuilder_Chunk>) end;

JWebSettings_RenderPriorityClass = interface(JEnumClass)
['{ABDCE7A7-56D3-4DAE-BD5E-6D8F882C652F}']
  {Property Methods}
  function _GetHIGH: JWebSettings_RenderPriority;
  function _GetLOW: JWebSettings_RenderPriority;
  function _GetNORMAL: JWebSettings_RenderPriority;
  {Methods}
  function valueOf(name: JString): JWebSettings_RenderPriority; cdecl;
  function values: TJavaObjectArray<JWebSettings_RenderPriority>; cdecl;
  {Properties}
  property HIGH: JWebSettings_RenderPriority read _GetHIGH;
  property LOW: JWebSettings_RenderPriority read _GetLOW;
  property NORMAL: JWebSettings_RenderPriority read _GetNORMAL;
end;

[JavaSignature('android/webkit/WebSettings$RenderPriority')]
JWebSettings_RenderPriority = interface(JEnum)
['{00AA47AC-5C9B-4C40-9266-3DCCDB4E849B}']
end;
TJWebSettings_RenderPriority = class(TJavaGenericImport<JWebSettings_RenderPriorityClass, JWebSettings_RenderPriority>) end;

JWebBackForwardListClass = interface(JObjectClass)
['{A07CD657-C6A4-4BB3-9DE1-5FAD34D2FAB3}']
end;

[JavaSignature('android/webkit/WebBackForwardList')]
JWebBackForwardList = interface(JObject)
['{CA2E0F5D-8FD8-4DDC-BDB4-73EB780AE1B9}']
  {Methods}
  function getCurrentIndex: Integer; cdecl;
  function getCurrentItem: JWebHistoryItem; cdecl;
  function getItemAtIndex(index: Integer): JWebHistoryItem; cdecl;
  function getSize: Integer; cdecl;
end;
TJWebBackForwardList = class(TJavaGenericImport<JWebBackForwardListClass, JWebBackForwardList>) end;

JJsResultClass = interface(JObjectClass)
['{EC8170B1-2E48-4399-8F0B-690249282CD8}']
end;

[JavaSignature('android/webkit/JsResult')]
JJsResult = interface(JObject)
['{FB00569F-6419-40C9-AFE2-97632E5CB25B}']
  {Methods}
  procedure cancel; cdecl;
  procedure confirm; cdecl;
end;
TJJsResult = class(TJavaGenericImport<JJsResultClass, JJsResult>) end;

JJsPromptResultClass = interface(JJsResultClass)
['{87DB5C55-9E24-4E97-AEB9-5175CE44962F}']
end;

[JavaSignature('android/webkit/JsPromptResult')]
JJsPromptResult = interface(JJsResult)
['{D88E5DFB-C1B1-444D-AA87-E71D6A50ED21}']
  {Methods}
  procedure confirm(result: JString); cdecl;
end;
TJJsPromptResult = class(TJavaGenericImport<JJsPromptResultClass, JJsPromptResult>) end;

JConsoleMessageClass = interface(JObjectClass)
['{CBB5BC88-0DE7-4587-8D99-B2F2B69BD91C}']
  {Methods}
  function init(message: JString; sourceId: JString; lineNumber: Integer; msgLevel: JConsoleMessage_MessageLevel): JConsoleMessage; cdecl;
end;

[JavaSignature('android/webkit/ConsoleMessage')]
JConsoleMessage = interface(JObject)
['{08638DAF-1309-4F99-9B6D-8998BF31A730}']
  {Methods}
  function lineNumber: Integer; cdecl;
  function message: JString; cdecl;
  function messageLevel: JConsoleMessage_MessageLevel; cdecl;
  function sourceId: JString; cdecl;
end;
TJConsoleMessage = class(TJavaGenericImport<JConsoleMessageClass, JConsoleMessage>) end;

JCookieSyncManagerClass = interface(JObjectClass)
['{96CCECF6-1D55-49E3-9DB6-A260AFD605BF}']
  {Methods}
  function createInstance(context: JContext): JCookieSyncManager; cdecl;
  function getInstance: JCookieSyncManager; cdecl;
end;

[JavaSignature('android/webkit/CookieSyncManager')]
JCookieSyncManager = interface(JObject)
['{A548B195-9703-456F-9ACB-26DF88014690}']
end;
TJCookieSyncManager = class(TJavaGenericImport<JCookieSyncManagerClass, JCookieSyncManager>) end;

JWebViewClientClass = interface(JObjectClass)
['{A5B717AB-A760-4941-A8D5-5BBBB610EE9C}']
  {Property Methods}
  function _GetERROR_AUTHENTICATION: Integer;
  function _GetERROR_BAD_URL: Integer;
  function _GetERROR_CONNECT: Integer;
  function _GetERROR_FAILED_SSL_HANDSHAKE: Integer;
  function _GetERROR_FILE: Integer;
  function _GetERROR_FILE_NOT_FOUND: Integer;
  function _GetERROR_HOST_LOOKUP: Integer;
  function _GetERROR_IO: Integer;
  function _GetERROR_PROXY_AUTHENTICATION: Integer;
  function _GetERROR_REDIRECT_LOOP: Integer;
  function _GetERROR_TIMEOUT: Integer;
  function _GetERROR_TOO_MANY_REQUESTS: Integer;
  function _GetERROR_UNKNOWN: Integer;
  function _GetERROR_UNSUPPORTED_AUTH_SCHEME: Integer;
  function _GetERROR_UNSUPPORTED_SCHEME: Integer;
  {Methods}
  function init: JWebViewClient; cdecl;
  {Properties}
  property ERROR_AUTHENTICATION: Integer read _GetERROR_AUTHENTICATION;
  property ERROR_BAD_URL: Integer read _GetERROR_BAD_URL;
  property ERROR_CONNECT: Integer read _GetERROR_CONNECT;
  property ERROR_FAILED_SSL_HANDSHAKE: Integer read _GetERROR_FAILED_SSL_HANDSHAKE;
  property ERROR_FILE: Integer read _GetERROR_FILE;
  property ERROR_FILE_NOT_FOUND: Integer read _GetERROR_FILE_NOT_FOUND;
  property ERROR_HOST_LOOKUP: Integer read _GetERROR_HOST_LOOKUP;
  property ERROR_IO: Integer read _GetERROR_IO;
  property ERROR_PROXY_AUTHENTICATION: Integer read _GetERROR_PROXY_AUTHENTICATION;
  property ERROR_REDIRECT_LOOP: Integer read _GetERROR_REDIRECT_LOOP;
  property ERROR_TIMEOUT: Integer read _GetERROR_TIMEOUT;
  property ERROR_TOO_MANY_REQUESTS: Integer read _GetERROR_TOO_MANY_REQUESTS;
  property ERROR_UNKNOWN: Integer read _GetERROR_UNKNOWN;
  property ERROR_UNSUPPORTED_AUTH_SCHEME: Integer read _GetERROR_UNSUPPORTED_AUTH_SCHEME;
  property ERROR_UNSUPPORTED_SCHEME: Integer read _GetERROR_UNSUPPORTED_SCHEME;
end;

[JavaSignature('android/webkit/WebViewClient')]
JWebViewClient = interface(JObject)
['{04427D5F-0978-486C-9ABC-14621B34FB9D}']
  {Methods}
  procedure doUpdateVisitedHistory(view: JWebView; url: JString; isReload: Boolean); cdecl;
  procedure onFormResubmission(view: JWebView; dontResend: JMessage; resend: JMessage); cdecl;
  procedure onLoadResource(view: JWebView; url: JString); cdecl;
  procedure onPageFinished(view: JWebView; url: JString); cdecl;
  procedure onPageStarted(view: JWebView; url: JString; favicon: JBitmap); cdecl;
  procedure onReceivedError(view: JWebView; errorCode: Integer; description: JString; failingUrl: JString); cdecl;
  procedure onReceivedHttpAuthRequest(view: JWebView; handler: JHttpAuthHandler; host: JString; realm: JString); cdecl;
  procedure onReceivedLoginRequest(view: JWebView; realm: JString; account: JString; args: JString); cdecl;
  procedure onReceivedSslError(view: JWebView; handler: JSslErrorHandler; error: JSslError); cdecl;
  procedure onScaleChanged(view: JWebView; oldScale: Single; newScale: Single); cdecl;
  procedure onTooManyRedirects(view: JWebView; cancelMsg: JMessage; continueMsg: JMessage); cdecl;//Deprecated
  procedure onUnhandledKeyEvent(view: JWebView; event: JKeyEvent); cdecl;
  function shouldInterceptRequest(view: JWebView; url: JString): JWebResourceResponse; cdecl;
  function shouldOverrideKeyEvent(view: JWebView; event: JKeyEvent): Boolean; cdecl;
  function shouldOverrideUrlLoading(view: JWebView; url: JString): Boolean; cdecl;
end;
TJWebViewClient = class(TJavaGenericImport<JWebViewClientClass, JWebViewClient>) end;

JWebResourceResponseClass = interface(JObjectClass)
['{E3E5D0F5-A281-4D60-8941-A07A10D9151E}']
  {Methods}
  function init(mimeType: JString; encoding: JString; data: JInputStream): JWebResourceResponse; cdecl;
end;

[JavaSignature('android/webkit/WebResourceResponse')]
JWebResourceResponse = interface(JObject)
['{00365ED2-0D7C-4CF5-99AB-D8057E8322F2}']
  {Methods}
  function getData: JInputStream; cdecl;
  function getEncoding: JString; cdecl;
  function getMimeType: JString; cdecl;
  procedure setData(data: JInputStream); cdecl;
  procedure setEncoding(encoding: JString); cdecl;
  procedure setMimeType(mimeType: JString); cdecl;
end;
TJWebResourceResponse = class(TJavaGenericImport<JWebResourceResponseClass, JWebResourceResponse>) end;

JHttpAuthHandlerClass = interface(JHandlerClass)
['{F4F240E8-5AD8-4E01-A7F6-3F014FC5ED61}']
end;

[JavaSignature('android/webkit/HttpAuthHandler')]
JHttpAuthHandler = interface(JHandler)
['{0E61C17C-91B0-4DF0-B85E-E270B4F69B4B}']
  {Methods}
  procedure cancel; cdecl;
  procedure proceed(username: JString; password: JString); cdecl;
  function useHttpAuthUsernamePassword: Boolean; cdecl;
end;
TJHttpAuthHandler = class(TJavaGenericImport<JHttpAuthHandlerClass, JHttpAuthHandler>) end;

JWebView_HitTestResultClass = interface(JObjectClass)
['{5AE29C91-64C1-4694-B2E3-A5C4077ABBA5}']
  {Property Methods}
  function _GetANCHOR_TYPE: Integer;
  function _GetEDIT_TEXT_TYPE: Integer;
  function _GetEMAIL_TYPE: Integer;
  function _GetGEO_TYPE: Integer;
  function _GetIMAGE_ANCHOR_TYPE: Integer;
  function _GetIMAGE_TYPE: Integer;
  function _GetPHONE_TYPE: Integer;
  function _GetSRC_ANCHOR_TYPE: Integer;
  function _GetSRC_IMAGE_ANCHOR_TYPE: Integer;
  function _GetUNKNOWN_TYPE: Integer;
  {Properties}
  property ANCHOR_TYPE: Integer read _GetANCHOR_TYPE;
  property EDIT_TEXT_TYPE: Integer read _GetEDIT_TEXT_TYPE;
  property EMAIL_TYPE: Integer read _GetEMAIL_TYPE;
  property GEO_TYPE: Integer read _GetGEO_TYPE;
  property IMAGE_ANCHOR_TYPE: Integer read _GetIMAGE_ANCHOR_TYPE;
  property IMAGE_TYPE: Integer read _GetIMAGE_TYPE;
  property PHONE_TYPE: Integer read _GetPHONE_TYPE;
  property SRC_ANCHOR_TYPE: Integer read _GetSRC_ANCHOR_TYPE;
  property SRC_IMAGE_ANCHOR_TYPE: Integer read _GetSRC_IMAGE_ANCHOR_TYPE;
  property UNKNOWN_TYPE: Integer read _GetUNKNOWN_TYPE;
end;

[JavaSignature('android/webkit/WebView$HitTestResult')]
JWebView_HitTestResult = interface(JObject)
['{5350D51E-FE08-48D8-A771-CA8ABEC70D95}']
  {Methods}
  function getExtra: JString; cdecl;
  function getType: Integer; cdecl;
end;
TJWebView_HitTestResult = class(TJavaGenericImport<JWebView_HitTestResultClass, JWebView_HitTestResult>) end;

JWebViewFragmentClass = interface(JFragmentClass)
['{75DB2D86-E8B2-40D6-94D0-253CDADCD376}']
  {Methods}
  function init: JWebViewFragment; cdecl;
end;

[JavaSignature('android/webkit/WebViewFragment')]
JWebViewFragment = interface(JFragment)
['{65D4F5A5-7470-429E-8544-CE88856852D8}']
  {Methods}
  function getWebView: JWebView; cdecl;
  function onCreateView(inflater: JLayoutInflater; container: JViewGroup; savedInstanceState: JBundle): JView; cdecl;
  procedure onDestroy; cdecl;
  procedure onDestroyView; cdecl;
  procedure onPause; cdecl;
  procedure onResume; cdecl;
end;
TJWebViewFragment = class(TJavaGenericImport<JWebViewFragmentClass, JWebViewFragment>) end;

JGeolocationPermissionsClass = interface(JObjectClass)
['{0EDFDB2D-5AA5-4B9F-AFF1-48B33D4DB768}']
  {Methods}
  function getInstance: JGeolocationPermissions; cdecl;
end;

[JavaSignature('android/webkit/GeolocationPermissions')]
JGeolocationPermissions = interface(JObject)
['{3DB62872-CE2B-441A-880F-61853F0D1429}']
  {Methods}
  procedure allow(origin: JString); cdecl;
  procedure clear(origin: JString); cdecl;
  procedure clearAll; cdecl;
  procedure getAllowed(origin: JString; callback: JValueCallback); cdecl;
  procedure getOrigins(callback: JValueCallback); cdecl;
end;
TJGeolocationPermissions = class(TJavaGenericImport<JGeolocationPermissionsClass, JGeolocationPermissions>) end;

JValueCallbackClass = interface(IJavaClass)
['{5CE4D0B0-6C4F-43BD-B57F-A06401A5FB2F}']
end;

[JavaSignature('android/webkit/ValueCallback')]
JValueCallback = interface(IJavaInstance)
['{3B24779A-3678-4AD8-B421-A8A9C6F3E742}']
  {Methods}
  procedure onReceiveValue(value: JObject); cdecl;
end;
TJValueCallback = class(TJavaGenericImport<JValueCallbackClass, JValueCallback>) end;

JWebView_WebViewTransportClass = interface(JObjectClass)
['{8CFEE3EF-625F-4465-8EF9-2ED5CEF3FBCB}']
  {Methods}
  function init: JWebView_WebViewTransport; cdecl;
end;

[JavaSignature('android/webkit/WebView$WebViewTransport')]
JWebView_WebViewTransport = interface(JObject)
['{4AA5F44F-DDEE-4DDB-8FAD-58C0C20F0A9B}']
  {Methods}
  function getWebView: JWebView; cdecl;
  procedure setWebView(webview: JWebView); cdecl;
end;
TJWebView_WebViewTransport = class(TJavaGenericImport<JWebView_WebViewTransportClass, JWebView_WebViewTransport>) end;

JCookieManagerClass = interface(JObjectClass)
['{AA56EEB3-9124-4EA5-9C2D-F2A34B85A746}']
  {Methods}
  function allowFileSchemeCookies: Boolean; cdecl;
  function getInstance: JCookieManager; cdecl;
  procedure setAcceptFileSchemeCookies(accept: Boolean); cdecl;
end;

[JavaSignature('android/webkit/CookieManager')]
JCookieManager = interface(JObject)
['{D034D488-20B0-40B1-B8E8-DBA885920119}']
  {Methods}
  function acceptCookie: Boolean; cdecl;
  function getCookie(url: JString): JString; cdecl;
  function hasCookies: Boolean; cdecl;
  procedure removeAllCookie; cdecl;
  procedure removeExpiredCookie; cdecl;
  procedure removeSessionCookie; cdecl;
  procedure setAcceptCookie(accept: Boolean); cdecl;
  procedure setCookie(url: JString; value: JString); cdecl;
end;
TJCookieManager = class(TJavaGenericImport<JCookieManagerClass, JCookieManager>) end;

JWebView_FindListenerClass = interface(IJavaClass)
['{41A75C2A-7028-4424-B3FD-A06BECE9A089}']
end;

[JavaSignature('android/webkit/WebView$FindListener')]
JWebView_FindListener = interface(IJavaInstance)
['{4DFED237-9B2D-41ED-8DBB-A5EBF5B36A6C}']
  {Methods}
  procedure onFindResultReceived(activeMatchOrdinal: Integer; numberOfMatches: Integer; isDoneCounting: Boolean); cdecl;
end;
TJWebView_FindListener = class(TJavaGenericImport<JWebView_FindListenerClass, JWebView_FindListener>) end;

JWebHistoryItemClass = interface(JObjectClass)
['{58FE1F44-7620-49B7-BA50-CF97126D6C75}']
end;

[JavaSignature('android/webkit/WebHistoryItem')]
JWebHistoryItem = interface(JObject)
['{B4724762-EFAC-4E05-962A-0D3E7684730D}']
  {Methods}
  function getFavicon: JBitmap; cdecl;
  function getOriginalUrl: JString; cdecl;
  function getTitle: JString; cdecl;
  function getUrl: JString; cdecl;
end;
TJWebHistoryItem = class(TJavaGenericImport<JWebHistoryItemClass, JWebHistoryItem>) end;

JWebIconDatabase_IconListenerClass = interface(IJavaClass)
['{9C2AE70A-59E2-4133-9A2D-794B6288C89F}']
end;

[JavaSignature('android/webkit/WebIconDatabase$IconListener')]
JWebIconDatabase_IconListener = interface(IJavaInstance)
['{9030BE72-BB1D-4A40-A452-7133B9C166B3}']
  {Methods}
  procedure onReceivedIcon(url: JString; icon: JBitmap); cdecl;
end;
TJWebIconDatabase_IconListener = class(TJavaGenericImport<JWebIconDatabase_IconListenerClass, JWebIconDatabase_IconListener>) end;

JSslErrorHandlerClass = interface(JHandlerClass)
['{9C90D0D5-1B1D-4969-979F-CA60801EE6BD}']
end;

[JavaSignature('android/webkit/SslErrorHandler')]
JSslErrorHandler = interface(JHandler)
['{03C0743D-557A-4B5D-942C-8C2B15F645C3}']
  {Methods}
  procedure cancel; cdecl;
  procedure proceed; cdecl;
end;
TJSslErrorHandler = class(TJavaGenericImport<JSslErrorHandlerClass, JSslErrorHandler>) end;

JWebView_PictureListenerClass = interface(IJavaClass)
['{126AE669-1AF8-4472-9CA6-67D139C407BD}']
end;

[JavaSignature('android/webkit/WebView$PictureListener')]
JWebView_PictureListener = interface(IJavaInstance)
['{C19F2468-28E4-4DE6-818D-BFCEFD4716E8}']
  {Methods}
  procedure onNewPicture(view: JWebView; picture: JPicture); cdecl;//Deprecated
end;
TJWebView_PictureListener = class(TJavaGenericImport<JWebView_PictureListenerClass, JWebView_PictureListener>) end;

JWebSettings_PluginStateClass = interface(JEnumClass)
['{3EEEE1A8-1A1C-4273-8FDA-795F1EC37C77}']
  {Property Methods}
  function _GetOFF: JWebSettings_PluginState;
  function _GetON: JWebSettings_PluginState;
  function _GetON_DEMAND: JWebSettings_PluginState;
  {Methods}
  function valueOf(name: JString): JWebSettings_PluginState; cdecl;
  function values: TJavaObjectArray<JWebSettings_PluginState>; cdecl;
  {Properties}
  property OFF: JWebSettings_PluginState read _GetOFF;
  property ON: JWebSettings_PluginState read _GetON;
  property ON_DEMAND: JWebSettings_PluginState read _GetON_DEMAND;
end;

[JavaSignature('android/webkit/WebSettings$PluginState')]
JWebSettings_PluginState = interface(JEnum)
['{9E1FE39E-6E4B-41B5-A4F5-117A32289576}']
end;
TJWebSettings_PluginState = class(TJavaGenericImport<JWebSettings_PluginStateClass, JWebSettings_PluginState>) end;

JWebSettingsClass = interface(JObjectClass)
['{602E6B04-981D-4A05-BA19-6ED02ADF24CA}']
  {Property Methods}
  function _GetLOAD_CACHE_ELSE_NETWORK: Integer;
  function _GetLOAD_CACHE_ONLY: Integer;
  function _GetLOAD_DEFAULT: Integer;
  function _GetLOAD_NORMAL: Integer;
  function _GetLOAD_NO_CACHE: Integer;
  {Methods}
  function getDefaultUserAgent(context: JContext): JString; cdecl;
  {Properties}
  property LOAD_CACHE_ELSE_NETWORK: Integer read _GetLOAD_CACHE_ELSE_NETWORK;
  property LOAD_CACHE_ONLY: Integer read _GetLOAD_CACHE_ONLY;
  property LOAD_DEFAULT: Integer read _GetLOAD_DEFAULT;
  property LOAD_NORMAL: Integer read _GetLOAD_NORMAL;
  property LOAD_NO_CACHE: Integer read _GetLOAD_NO_CACHE;
end;

[JavaSignature('android/webkit/WebSettings')]
JWebSettings = interface(JObject)
['{6F9320FD-C646-45CB-90A3-7985E5A1BAE8}']
  {Methods}
  function enableSmoothTransition: Boolean; cdecl;//Deprecated
  function getAllowContentAccess: Boolean; cdecl;
  function getAllowFileAccess: Boolean; cdecl;
  function getAllowFileAccessFromFileURLs: Boolean; cdecl;
  function getAllowUniversalAccessFromFileURLs: Boolean; cdecl;
  function getBlockNetworkImage: Boolean; cdecl;
  function getBlockNetworkLoads: Boolean; cdecl;
  function getBuiltInZoomControls: Boolean; cdecl;
  function getCacheMode: Integer; cdecl;
  function getCursiveFontFamily: JString; cdecl;
  function getDatabaseEnabled: Boolean; cdecl;
  function getDatabasePath: JString; cdecl;
  function getDefaultFixedFontSize: Integer; cdecl;
  function getDefaultFontSize: Integer; cdecl;
  function getDefaultTextEncodingName: JString; cdecl;
  function getDefaultZoom: JWebSettings_ZoomDensity; cdecl;
  function getDisplayZoomControls: Boolean; cdecl;
  function getDomStorageEnabled: Boolean; cdecl;
  function getFantasyFontFamily: JString; cdecl;
  function getFixedFontFamily: JString; cdecl;
  function getJavaScriptCanOpenWindowsAutomatically: Boolean; cdecl;
  function getJavaScriptEnabled: Boolean; cdecl;
  function getLayoutAlgorithm: JWebSettings_LayoutAlgorithm; cdecl;
  function getLightTouchEnabled: Boolean; cdecl;
  function getLoadWithOverviewMode: Boolean; cdecl;
  function getLoadsImagesAutomatically: Boolean; cdecl;
  function getMediaPlaybackRequiresUserGesture: Boolean; cdecl;
  function getMinimumFontSize: Integer; cdecl;
  function getMinimumLogicalFontSize: Integer; cdecl;
  function getPluginState: JWebSettings_PluginState; cdecl;
  function getPluginsEnabled: Boolean; cdecl;//Deprecated
  function getPluginsPath: JString; cdecl;//Deprecated
  function getSansSerifFontFamily: JString; cdecl;
  function getSaveFormData: Boolean; cdecl;
  function getSavePassword: Boolean; cdecl;
  function getSerifFontFamily: JString; cdecl;
  function getStandardFontFamily: JString; cdecl;
  function getTextSize: JWebSettings_TextSize; cdecl;//Deprecated
  function getTextZoom: Integer; cdecl;
  function getUseWideViewPort: Boolean; cdecl;
  function getUserAgentString: JString; cdecl;
  procedure setAllowContentAccess(allow: Boolean); cdecl;
  procedure setAllowFileAccess(allow: Boolean); cdecl;
  procedure setAllowFileAccessFromFileURLs(flag: Boolean); cdecl;
  procedure setAllowUniversalAccessFromFileURLs(flag: Boolean); cdecl;
  procedure setAppCacheEnabled(flag: Boolean); cdecl;
  procedure setAppCacheMaxSize(appCacheMaxSize: Int64); cdecl;
  procedure setAppCachePath(appCachePath: JString); cdecl;
  procedure setBlockNetworkImage(flag: Boolean); cdecl;
  procedure setBlockNetworkLoads(flag: Boolean); cdecl;
  procedure setBuiltInZoomControls(enabled: Boolean); cdecl;
  procedure setCacheMode(mode: Integer); cdecl;
  procedure setCursiveFontFamily(font: JString); cdecl;
  procedure setDatabaseEnabled(flag: Boolean); cdecl;
  procedure setDatabasePath(databasePath: JString); cdecl;
  procedure setDefaultFixedFontSize(size: Integer); cdecl;
  procedure setDefaultFontSize(size: Integer); cdecl;
  procedure setDefaultTextEncodingName(encoding: JString); cdecl;
  procedure setDefaultZoom(zoom: JWebSettings_ZoomDensity); cdecl;
  procedure setDisplayZoomControls(enabled: Boolean); cdecl;
  procedure setDomStorageEnabled(flag: Boolean); cdecl;
  procedure setEnableSmoothTransition(enable: Boolean); cdecl;//Deprecated
  procedure setFantasyFontFamily(font: JString); cdecl;
  procedure setFixedFontFamily(font: JString); cdecl;
  procedure setGeolocationDatabasePath(databasePath: JString); cdecl;
  procedure setGeolocationEnabled(flag: Boolean); cdecl;
  procedure setJavaScriptCanOpenWindowsAutomatically(flag: Boolean); cdecl;
  procedure setJavaScriptEnabled(flag: Boolean); cdecl;
  procedure setLayoutAlgorithm(l: JWebSettings_LayoutAlgorithm); cdecl;
  procedure setLightTouchEnabled(enabled: Boolean); cdecl;
  procedure setLoadWithOverviewMode(overview: Boolean); cdecl;
  procedure setLoadsImagesAutomatically(flag: Boolean); cdecl;
  procedure setMediaPlaybackRequiresUserGesture(require: Boolean); cdecl;
  procedure setMinimumFontSize(size: Integer); cdecl;
  procedure setMinimumLogicalFontSize(size: Integer); cdecl;
  procedure setNeedInitialFocus(flag: Boolean); cdecl;
  procedure setPluginState(state: JWebSettings_PluginState); cdecl;
  procedure setPluginsEnabled(flag: Boolean); cdecl;//Deprecated
  procedure setPluginsPath(pluginsPath: JString); cdecl;//Deprecated
  procedure setRenderPriority(priority: JWebSettings_RenderPriority); cdecl;
  procedure setSansSerifFontFamily(font: JString); cdecl;
  procedure setSaveFormData(save: Boolean); cdecl;
  procedure setSavePassword(save: Boolean); cdecl;
  procedure setSerifFontFamily(font: JString); cdecl;
  procedure setStandardFontFamily(font: JString); cdecl;
  procedure setSupportMultipleWindows(support: Boolean); cdecl;
  procedure setSupportZoom(support: Boolean); cdecl;
  procedure setTextSize(t: JWebSettings_TextSize); cdecl;//Deprecated
  procedure setTextZoom(textZoom: Integer); cdecl;
  procedure setUseWideViewPort(use: Boolean); cdecl;
  procedure setUserAgentString(ua: JString); cdecl;
  function supportMultipleWindows: Boolean; cdecl;
  function supportZoom: Boolean; cdecl;
end;
TJWebSettings = class(TJavaGenericImport<JWebSettingsClass, JWebSettings>) end;




implementation

begin

end.


