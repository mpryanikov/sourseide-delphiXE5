{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit Androidapi.JNI.Analytics;

interface

uses 
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.App,
  Androidapi.JNI.GraphicsContentViewText;

type

  {Class forward declarations}
  JGoogleAnalytics_AppOptOutCallback = interface;//com.google.analytics.tracking.android.GoogleAnalytics$AppOptOutCallback
  JExceptionParser = interface;//com.google.analytics.tracking.android.ExceptionParser
  JStandardExceptionParser = interface;//com.google.analytics.tracking.android.StandardExceptionParser
  JTransaction_Builder = interface;//com.google.analytics.tracking.android.Transaction$Builder
  JCampaignTrackingService = interface;//com.google.analytics.tracking.android.CampaignTrackingService
  JAnalyticsThread_ClientIdCallback = interface;//com.google.analytics.tracking.android.AnalyticsThread$ClientIdCallback
  JModelFields = interface;//com.google.analytics.tracking.android.ModelFields
  JGAServiceManager = interface;//com.google.analytics.tracking.android.GAServiceManager
  JTracker = interface;//com.google.analytics.tracking.android.Tracker
  JEasyTracker = interface;//com.google.analytics.tracking.android.EasyTracker
  JCampaignTrackingReceiver = interface;//com.google.analytics.tracking.android.CampaignTrackingReceiver
  JGoogleAnalytics = interface;//com.google.analytics.tracking.android.GoogleAnalytics
  JTransaction_Item = interface;//com.google.analytics.tracking.android.Transaction$Item
  JItem_Builder = interface;//com.google.analytics.tracking.android.Transaction$Item$Builder
  JExceptionReporter = interface;//com.google.analytics.tracking.android.ExceptionReporter
  JServiceManager = interface;//com.google.analytics.tracking.android.ServiceManager
  JTransaction = interface;//com.google.analytics.tracking.android.Transaction
  Jandroid_Log = interface;//com.google.analytics.tracking.android.Log

JGoogleAnalytics_AppOptOutCallbackClass = interface(IJavaClass)
['{0D3DAF93-8A52-4EED-8F9A-9F986495BA4D}']
end;

[JavaSignature('com/google/analytics/tracking/android/GoogleAnalytics$AppOptOutCallback')]
JGoogleAnalytics_AppOptOutCallback = interface(IJavaInstance)
['{DB8992E3-FEFA-4BB1-98B9-F44BB465F278}']
  {Methods}
  procedure reportAppOptOut(paramBoolean: Boolean); cdecl;
end;
TJGoogleAnalytics_AppOptOutCallback = class(TJavaGenericImport<JGoogleAnalytics_AppOptOutCallbackClass, JGoogleAnalytics_AppOptOutCallback>) end;

JExceptionParserClass = interface(IJavaClass)
['{27FAA6C4-A988-4B87-B703-03A90F6BEE78}']
end;

[JavaSignature('com/google/analytics/tracking/android/ExceptionParser')]
JExceptionParser = interface(IJavaInstance)
['{427A0D60-A7AD-49A6-ADEA-8F49CE492A1D}']
  {Methods}
  function getDescription(paramString: JString; paramThrowable: JThrowable): JString; cdecl;
end;
TJExceptionParser = class(TJavaGenericImport<JExceptionParserClass, JExceptionParser>) end;

JStandardExceptionParserClass = interface(JObjectClass)
['{B1126ECF-50B9-442F-BEAF-ED15D973DBCD}']
  {Methods}
  function init(context: JContext; additionalPackages: JCollection): JStandardExceptionParser; cdecl;
end;

[JavaSignature('com/google/analytics/tracking/android/StandardExceptionParser')]
JStandardExceptionParser = interface(JObject)
['{87DE064D-2FE7-46C0-A45F-77387B2A1A98}']
  {Methods}
  function getDescription(threadName: JString; t: JThrowable): JString; cdecl;
  procedure setIncludedPackages(context: JContext; additionalPackages: JCollection); cdecl;
end;
TJStandardExceptionParser = class(TJavaGenericImport<JStandardExceptionParserClass, JStandardExceptionParser>) end;

JTransaction_BuilderClass = interface(JObjectClass)
['{042AF3D5-7D57-4656-8A94-A8D5938BE96F}']
  {Methods}
  function init(transactionId: JString; totalCostInMicros: Int64): JTransaction_Builder; cdecl;
end;

[JavaSignature('com/google/analytics/tracking/android/Transaction$Builder')]
JTransaction_Builder = interface(JObject)
['{AAFE40C1-0964-4EB7-B035-983C84BE1F13}']
  {Methods}
  function build: JTransaction; cdecl;
  function setAffiliation(affiliation: JString): JTransaction_Builder; cdecl;
  function setCurrencyCode(currencyCode: JString): JTransaction_Builder; cdecl;
  function setShippingCostInMicros(shippingCostInMicros: Int64): JTransaction_Builder; cdecl;
  function setTotalTaxInMicros(totalTaxInMicros: Int64): JTransaction_Builder; cdecl;
end;
TJTransaction_Builder = class(TJavaGenericImport<JTransaction_BuilderClass, JTransaction_Builder>) end;

JCampaignTrackingServiceClass = interface(JIntentServiceClass)
['{72BFE8D6-3F6D-402B-9D5D-EE14ADB67970}']
  {Methods}
  function init(name: JString): JCampaignTrackingService; cdecl; overload;
  function init: JCampaignTrackingService; cdecl; overload;
end;

[JavaSignature('com/google/analytics/tracking/android/CampaignTrackingService')]
JCampaignTrackingService = interface(JIntentService)
['{0AF1D352-0263-48DB-AE7B-F9D08FD54140}']
end;
TJCampaignTrackingService = class(TJavaGenericImport<JCampaignTrackingServiceClass, JCampaignTrackingService>) end;

JAnalyticsThread_ClientIdCallbackClass = interface(IJavaClass)
['{F5905273-CD1E-4B68-90FE-CD12E7E85A50}']
end;

[JavaSignature('com/google/analytics/tracking/android/AnalyticsThread$ClientIdCallback')]
JAnalyticsThread_ClientIdCallback = interface(IJavaInstance)
['{56B9191B-76F9-42B3-AD76-9759A4B77883}']
  {Methods}
  procedure reportClientId(paramString: JString); cdecl;
end;
TJAnalyticsThread_ClientIdCallback = class(TJavaGenericImport<JAnalyticsThread_ClientIdCallbackClass, JAnalyticsThread_ClientIdCallback>) end;

JModelFieldsClass = interface(JObjectClass)
['{87CD99C9-E1DB-4D40-BBBB-67ED070AAE4D}']
  {Property Methods}
  function _GetANDROID_APP_UID: JString;
  function _GetANONYMIZE_IP: JString;
  function _GetAPI_VERSION: JString;
  function _GetAPP_ID: JString;
  function _GetAPP_INSTALLER_ID: JString;
  function _GetAPP_NAME: JString;
  function _GetAPP_SCREEN: JString;
  function _GetAPP_VERSION: JString;
  function _GetAPP_VIEW: JString;
  function _GetCACHE_BUSTER: JString;
  function _GetCAMPAIGN: JString;
  function _GetCAMPAIGN_CONTENT: JString;
  function _GetCAMPAIGN_ID: JString;
  function _GetCAMPAIGN_KEYWORD: JString;
  function _GetCAMPAIGN_MEDIUM: JString;
  function _GetCAMPAIGN_NAME: JString;
  function _GetCAMPAIGN_SOURCE: JString;
  function _GetCLIENT_ID: JString;
  function _GetCONTENT_GROUPING: JString;
  function _GetCUSTOM_DIMENSION: JString;
  function _GetCUSTOM_METRIC: JString;
  function _GetDCLID: JString;
  function _GetDESCRIPTION: JString;
  function _GetENCODING: JString;
  function _GetEVENT: JString;
  function _GetEVENT_ACTION: JString;
  function _GetEVENT_CATEGORY: JString;
  function _GetEVENT_LABEL: JString;
  function _GetEVENT_VALUE: JString;
  function _GetEXCEPTION: JString;
  function _GetEXCEPTION_THREAD_NAME: JString;
  function _GetEX_DESCRIPTION: JString;
  function _GetEX_FATAL: JString;
  function _GetFLASH_VERSION: JString;
  function _GetGCLID: JString;
  function _GetGMOB_T: JString;
  function _GetHIT_TYPE: JString;
  function _GetITEM: JString;
  function _GetITEM_CATEGORY: JString;
  function _GetITEM_CODE: JString;
  function _GetITEM_NAME: JString;
  function _GetITEM_PRICE: JString;
  function _GetITEM_QUANTITY: JString;
  function _GetJAVA_ENABLED: JString;
  function _GetLANGUAGE: JString;
  function _GetNON_INTERACTION: JString;
  function _GetPAGE: JString;
  function _GetQUEUE_TIME: JString;
  function _GetRAW_EXCEPTION: JString;
  function _GetREFERRER: JString;
  function _GetSAMPLE_RATE: JString;
  function _GetSCREEN_COLORS: JString;
  function _GetSCREEN_RESOLUTION: JString;
  function _GetSESSION_CONTROL: JString;
  function _GetSOCIAL: JString;
  function _GetSOCIAL_ACTION: JString;
  function _GetSOCIAL_NETWORK: JString;
  function _GetSOCIAL_TARGET: JString;
  function _GetTIMING: JString;
  function _GetTIMING_CATEGORY: JString;
  function _GetTIMING_LABEL: JString;
  function _GetTIMING_VALUE: JString;
  function _GetTIMING_VAR: JString;
  function _GetTITLE: JString;
  function _GetTRACKING_ID: JString;
  function _GetTRANSACTION: JString;
  function _GetTRANSACTION_AFFILIATION: JString;
  function _GetTRANSACTION_ID: JString;
  function _GetTRANSACTION_SHIPPING: JString;
  function _GetTRANSACTION_TAX: JString;
  function _GetTRANSACTION_TOTAL: JString;
  function _GetUSE_SECURE: JString;
  function _GetVIEWPORT_SIZE: JString;
  {Methods}
  function init: JModelFields; cdecl;
  {Properties}
  property ANDROID_APP_UID: JString read _GetANDROID_APP_UID;
  property ANONYMIZE_IP: JString read _GetANONYMIZE_IP;
  property API_VERSION: JString read _GetAPI_VERSION;
  property APP_ID: JString read _GetAPP_ID;
  property APP_INSTALLER_ID: JString read _GetAPP_INSTALLER_ID;
  property APP_NAME: JString read _GetAPP_NAME;
  property APP_SCREEN: JString read _GetAPP_SCREEN;
  property APP_VERSION: JString read _GetAPP_VERSION;
  property APP_VIEW: JString read _GetAPP_VIEW;
  property CACHE_BUSTER: JString read _GetCACHE_BUSTER;
  property CAMPAIGN: JString read _GetCAMPAIGN;
  property CAMPAIGN_CONTENT: JString read _GetCAMPAIGN_CONTENT;
  property CAMPAIGN_ID: JString read _GetCAMPAIGN_ID;
  property CAMPAIGN_KEYWORD: JString read _GetCAMPAIGN_KEYWORD;
  property CAMPAIGN_MEDIUM: JString read _GetCAMPAIGN_MEDIUM;
  property CAMPAIGN_NAME: JString read _GetCAMPAIGN_NAME;
  property CAMPAIGN_SOURCE: JString read _GetCAMPAIGN_SOURCE;
  property CLIENT_ID: JString read _GetCLIENT_ID;
  property CONTENT_GROUPING: JString read _GetCONTENT_GROUPING;
  property CUSTOM_DIMENSION: JString read _GetCUSTOM_DIMENSION;
  property CUSTOM_METRIC: JString read _GetCUSTOM_METRIC;
  property DCLID: JString read _GetDCLID;
  property DESCRIPTION: JString read _GetDESCRIPTION;
  property ENCODING: JString read _GetENCODING;
  property EVENT: JString read _GetEVENT;
  property EVENT_ACTION: JString read _GetEVENT_ACTION;
  property EVENT_CATEGORY: JString read _GetEVENT_CATEGORY;
  property EVENT_LABEL: JString read _GetEVENT_LABEL;
  property EVENT_VALUE: JString read _GetEVENT_VALUE;
  property EXCEPTION: JString read _GetEXCEPTION;
  property EXCEPTION_THREAD_NAME: JString read _GetEXCEPTION_THREAD_NAME;
  property EX_DESCRIPTION: JString read _GetEX_DESCRIPTION;
  property EX_FATAL: JString read _GetEX_FATAL;
  property FLASH_VERSION: JString read _GetFLASH_VERSION;
  property GCLID: JString read _GetGCLID;
  property GMOB_T: JString read _GetGMOB_T;
  property HIT_TYPE: JString read _GetHIT_TYPE;
  property ITEM: JString read _GetITEM;
  property ITEM_CATEGORY: JString read _GetITEM_CATEGORY;
  property ITEM_CODE: JString read _GetITEM_CODE;
  property ITEM_NAME: JString read _GetITEM_NAME;
  property ITEM_PRICE: JString read _GetITEM_PRICE;
  property ITEM_QUANTITY: JString read _GetITEM_QUANTITY;
  property JAVA_ENABLED: JString read _GetJAVA_ENABLED;
  property LANGUAGE: JString read _GetLANGUAGE;
  property NON_INTERACTION: JString read _GetNON_INTERACTION;
  property PAGE: JString read _GetPAGE;
  property QUEUE_TIME: JString read _GetQUEUE_TIME;
  property RAW_EXCEPTION: JString read _GetRAW_EXCEPTION;
  property REFERRER: JString read _GetREFERRER;
  property SAMPLE_RATE: JString read _GetSAMPLE_RATE;
  property SCREEN_COLORS: JString read _GetSCREEN_COLORS;
  property SCREEN_RESOLUTION: JString read _GetSCREEN_RESOLUTION;
  property SESSION_CONTROL: JString read _GetSESSION_CONTROL;
  property SOCIAL: JString read _GetSOCIAL;
  property SOCIAL_ACTION: JString read _GetSOCIAL_ACTION;
  property SOCIAL_NETWORK: JString read _GetSOCIAL_NETWORK;
  property SOCIAL_TARGET: JString read _GetSOCIAL_TARGET;
  property TIMING: JString read _GetTIMING;
  property TIMING_CATEGORY: JString read _GetTIMING_CATEGORY;
  property TIMING_LABEL: JString read _GetTIMING_LABEL;
  property TIMING_VALUE: JString read _GetTIMING_VALUE;
  property TIMING_VAR: JString read _GetTIMING_VAR;
  property TITLE: JString read _GetTITLE;
  property TRACKING_ID: JString read _GetTRACKING_ID;
  property TRANSACTION: JString read _GetTRANSACTION;
  property TRANSACTION_AFFILIATION: JString read _GetTRANSACTION_AFFILIATION;
  property TRANSACTION_ID: JString read _GetTRANSACTION_ID;
  property TRANSACTION_SHIPPING: JString read _GetTRANSACTION_SHIPPING;
  property TRANSACTION_TAX: JString read _GetTRANSACTION_TAX;
  property TRANSACTION_TOTAL: JString read _GetTRANSACTION_TOTAL;
  property USE_SECURE: JString read _GetUSE_SECURE;
  property VIEWPORT_SIZE: JString read _GetVIEWPORT_SIZE;
end;

[JavaSignature('com/google/analytics/tracking/android/ModelFields')]
JModelFields = interface(JObject)
['{B50F6065-F244-4D64-9CCB-4095FF443D0D}']
end;
TJModelFields = class(TJavaGenericImport<JModelFieldsClass, JModelFields>) end;

JGAServiceManagerClass = interface(JObjectClass)
['{CB723915-880E-40E1-AD91-2EB757F17443}']
  {Methods}
  function getInstance: JGAServiceManager; cdecl;
end;

[JavaSignature('com/google/analytics/tracking/android/GAServiceManager')]
JGAServiceManager = interface(JObject)
['{27A7DD22-6714-41C9-B78A-7DF99AE31B7F}']
  {Methods}
  procedure dispatch; cdecl;
  procedure setDispatchPeriod(dispatchPeriodInSeconds: Integer); cdecl;
  procedure updateConnectivityStatus(connected: Boolean); cdecl;
end;
TJGAServiceManager = class(TJavaGenericImport<JGAServiceManagerClass, JGAServiceManager>) end;

JTrackerClass = interface(JObjectClass)
['{B2627961-C102-411C-9B33-CE86DC1CBDC7}']
end;

[JavaSignature('com/google/analytics/tracking/android/Tracker')]
JTracker = interface(JObject)
['{2AE4F241-3111-4159-A659-40955A45B7E3}']
  {Methods}
  procedure close; cdecl;
  function constructEvent(category: JString; action: JString; label_: JString; value: JLong): JMap; cdecl;
  function constructException(exceptionDescription: JString; fatal: Boolean): JMap; cdecl;
  function constructRawException(threadName: JString; exception: JThrowable; fatal: Boolean): JMap; cdecl;
  function constructSocial(network: JString; action: JString; target: JString): JMap; cdecl;
  function constructTiming(category: JString; intervalInMilliseconds: Int64; name: JString; label_: JString): JMap; cdecl;
  function constructTransaction(trans: JTransaction): JMap; cdecl;
  function &get(key: JString): JString; cdecl;
  function getAppId: JString; cdecl;
  function getAppInstallerId: JString; cdecl;
  function getExceptionParser: JExceptionParser; cdecl;
  function getSampleRate: Double; cdecl;
  function getTrackingId: JString; cdecl;
  function isAnonymizeIpEnabled: Boolean; cdecl;
  function isUseSecure: Boolean; cdecl;
  procedure send(hitType: JString; params: JMap); cdecl;
  procedure sendEvent(category: JString; action: JString; label_: JString; value: JLong); cdecl;
  procedure sendException(description: JString; fatal: Boolean); cdecl; overload;
  procedure sendException(threadName: JString; exception: JThrowable; fatal: Boolean); cdecl; overload;
  procedure sendSocial(network: JString; action: JString; target: JString); cdecl;
  procedure sendTiming(category: JString; intervalInMilliseconds: Int64; name: JString; label_: JString); cdecl;
  procedure sendTransaction(transaction: JTransaction); cdecl;
  procedure sendView; cdecl; overload;
  procedure sendView(appScreen: JString); cdecl; overload;
  procedure &set(key: JString; value: JString); cdecl;
  procedure setAnonymizeIp(anonymizeIp: Boolean); cdecl;
  procedure setAppId(appId: JString); cdecl;
  procedure setAppInstallerId(appInstallerId: JString); cdecl;
  procedure setAppName(appName: JString); cdecl;
  procedure setAppScreen(appScreen: JString); cdecl;
  procedure setAppVersion(appVersion: JString); cdecl;
  procedure setCampaign(campaign: JString); cdecl;
  procedure setCustomDimension(index: Integer; value: JString); cdecl;
  procedure setCustomDimensionsAndMetrics(dimensions: JMap; metrics: JMap); cdecl;
  procedure setCustomMetric(index: Integer; value: JLong); cdecl;
  procedure setExceptionParser(exceptionParser: JExceptionParser); cdecl;
  procedure setReferrer(referrer: JString); cdecl;
  procedure setSampleRate(sampleRate: Double); cdecl;
  procedure setStartSession(startSession: Boolean); cdecl;
  procedure setThrottlingEnabled(throttlingEnabled: Boolean); cdecl;
  procedure setUseSecure(useSecure: Boolean); cdecl;
  procedure trackEvent(category: JString; action: JString; label_: JString; value: JLong); cdecl;//Deprecated
  procedure trackException(description: JString; fatal: Boolean); cdecl; overload;//Deprecated
  procedure trackException(threadName: JString; exception: JThrowable; fatal: Boolean); cdecl; overload;//Deprecated
  procedure trackSocial(network: JString; action: JString; target: JString); cdecl;//Deprecated
  procedure trackTiming(category: JString; intervalInMilliseconds: Int64; name: JString; label_: JString); cdecl;//Deprecated
  procedure trackTransaction(transaction: JTransaction); cdecl;//Deprecated
  procedure trackView; cdecl; overload;//Deprecated
  procedure trackView(appScreen: JString); cdecl; overload;//Deprecated
end;
TJTracker = class(TJavaGenericImport<JTrackerClass, JTracker>) end;

JEasyTrackerClass = interface(JObjectClass)
['{8499D0D6-D2B3-431C-954C-7250D3ABABDD}']
  {Methods}
  function getInstance: JEasyTracker; cdecl;
  function getTracker: JTracker; cdecl;
end;

[JavaSignature('com/google/analytics/tracking/android/EasyTracker')]
JEasyTracker = interface(JObject)
['{BD4D421D-03C7-45BA-B4AA-7DBA624F5642}']
  {Methods}
  procedure activityStart(activity: JActivity); cdecl;
  procedure activityStop(activity: JActivity); cdecl;
  procedure dispatch; cdecl;
  procedure setContext(ctx: JContext); cdecl;
end;
TJEasyTracker = class(TJavaGenericImport<JEasyTrackerClass, JEasyTracker>) end;

JCampaignTrackingReceiverClass = interface(JBroadcastReceiverClass)
['{9BBDCFAD-93AC-40E9-9D5C-D4A6848033D2}']
  {Methods}
  function init: JCampaignTrackingReceiver; cdecl;
end;

[JavaSignature('com/google/analytics/tracking/android/CampaignTrackingReceiver')]
JCampaignTrackingReceiver = interface(JBroadcastReceiver)
['{5ABB75C6-3E3B-4B83-AA3C-ACBD240EA4E6}']
  {Methods}
  procedure onReceive(ctx: JContext; intent: JIntent); cdecl;
end;
TJCampaignTrackingReceiver = class(TJavaGenericImport<JCampaignTrackingReceiverClass, JCampaignTrackingReceiver>) end;

JGoogleAnalyticsClass = interface(JObjectClass)
['{69864765-05B2-4142-8709-A377F7A47F98}']
  {Methods}
  function getInstance(context: JContext): JGoogleAnalytics; cdecl;
end;

[JavaSignature('com/google/analytics/tracking/android/GoogleAnalytics')]
JGoogleAnalytics = interface(JObject)
['{2D7EB39C-3279-41C5-B6BA-0B42DF6C613D}']
  {Methods}
  procedure closeTracker(tracker: JTracker); cdecl;
  function getDefaultTracker: JTracker; cdecl;
  function getTracker(trackingId: JString): JTracker; cdecl;
  function isDebugEnabled: Boolean; cdecl;
  procedure requestAppOptOut(callback: JGoogleAnalytics_AppOptOutCallback); cdecl;
  procedure sendHit(hit: JMap); cdecl;
  procedure setAppOptOut(optOut: Boolean); cdecl;
  procedure setDebug(debug: Boolean); cdecl;
  procedure setDefaultTracker(tracker: JTracker); cdecl;
end;
TJGoogleAnalytics = class(TJavaGenericImport<JGoogleAnalyticsClass, JGoogleAnalytics>) end;

JTransaction_ItemClass = interface(JObjectClass)
['{2A64E0A4-BF0B-4DFA-9658-0682CE2AD6AA}']
end;

[JavaSignature('com/google/analytics/tracking/android/Transaction$Item')]
JTransaction_Item = interface(JObject)
['{A9B5E013-D50F-4BAB-BECA-0E443060C755}']
  {Methods}
  function getCategory: JString; cdecl;
  function getName: JString; cdecl;
  function getPriceInMicros: Int64; cdecl;
  function getQuantity: Int64; cdecl;
  function getSKU: JString; cdecl;
end;
TJTransaction_Item = class(TJavaGenericImport<JTransaction_ItemClass, JTransaction_Item>) end;

JItem_BuilderClass = interface(JObjectClass)
['{0934FF65-7A08-434E-B03F-695732A0E409}']
  {Methods}
  function init(SKU: JString; name: JString; priceInMicros: Int64; quantity: Int64): JItem_Builder; cdecl;
end;

[JavaSignature('com/google/analytics/tracking/android/Transaction$Item$Builder')]
JItem_Builder = interface(JObject)
['{2F7E85FE-FA41-4F65-91DD-055211F20FC4}']
  {Methods}
  function build: JTransaction_Item; cdecl;
  function setProductCategory(productCategory: JString): JItem_Builder; cdecl;
end;
TJItem_Builder = class(TJavaGenericImport<JItem_BuilderClass, JItem_Builder>) end;

JExceptionReporterClass = interface(JObjectClass)
['{AF7794E6-3812-4DD2-8ED6-77B29824DA3B}']
  {Methods}
  function init(tracker: JTracker; serviceManager: JServiceManager; originalHandler: JThread_UncaughtExceptionHandler; context: JContext): JExceptionReporter; cdecl;
end;

[JavaSignature('com/google/analytics/tracking/android/ExceptionReporter')]
JExceptionReporter = interface(JObject)
['{25437F3C-CB55-4607-9126-568AA64C8E6B}']
  {Methods}
  function getExceptionParser: JExceptionParser; cdecl;
  procedure setExceptionParser(exceptionParser: JExceptionParser); cdecl;
  procedure uncaughtException(t: JThread; e: JThrowable); cdecl;
end;
TJExceptionReporter = class(TJavaGenericImport<JExceptionReporterClass, JExceptionReporter>) end;

JServiceManagerClass = interface(IJavaClass)
['{B7235021-91AE-4B45-B235-65E57379A58F}']
end;

[JavaSignature('com/google/analytics/tracking/android/ServiceManager')]
JServiceManager = interface(IJavaInstance)
['{38258C12-E458-47CE-A63E-12782C752B1C}']
  {Methods}
  procedure dispatch; cdecl;
  procedure setDispatchPeriod(paramInt: Integer); cdecl;
  procedure updateConnectivityStatus(paramBoolean: Boolean); cdecl;
end;
TJServiceManager = class(TJavaGenericImport<JServiceManagerClass, JServiceManager>) end;

JTransactionClass = interface(JObjectClass)
['{28763069-CEB0-4D82-9B5F-023427DB6153}']
end;

[JavaSignature('com/google/analytics/tracking/android/Transaction')]
JTransaction = interface(JObject)
['{90F8AAE4-659D-4A92-91F5-FEF97EEECEA0}']
  {Methods}
  procedure addItem(item: JTransaction_Item); cdecl;
  function getAffiliation: JString; cdecl;
  function getCurrencyCode: JString; cdecl;
  function getItems: JList; cdecl;
  function getShippingCostInMicros: Int64; cdecl;
  function getTotalCostInMicros: Int64; cdecl;
  function getTotalTaxInMicros: Int64; cdecl;
  function getTransactionId: JString; cdecl;
end;
TJTransaction = class(TJavaGenericImport<JTransactionClass, JTransaction>) end;

Jandroid_LogClass = interface(JObjectClass)
['{3042F41F-E9CB-4B25-8731-D36CC6C53B36}']
  {Methods}
  function init: Jandroid_Log; cdecl;
  function d(msg: JString): Integer; cdecl;
  function dDebug(msg: JString): Integer; cdecl;
  function e(msg: JString): Integer; cdecl;
  function eDebug(msg: JString): Integer; cdecl;
  function i(msg: JString): Integer; cdecl;
  function iDebug(msg: JString): Integer; cdecl;
  function isDebugEnabled: Boolean; cdecl;
  procedure setDebug(debug: Boolean); cdecl;
  function v(msg: JString): Integer; cdecl;
  function vDebug(msg: JString): Integer; cdecl;
  function w(msg: JString): Integer; cdecl;
  function wDebug(msg: JString): Integer; cdecl;
end;

[JavaSignature('com/google/analytics/tracking/android/Log')]
Jandroid_Log = interface(JObject)
['{E246D977-8DBD-4CDB-BD16-8D4AB1F84DCE}']
end;
TJandroid_Log = class(TJavaGenericImport<Jandroid_LogClass, Jandroid_Log>) end;




implementation

begin

end.


