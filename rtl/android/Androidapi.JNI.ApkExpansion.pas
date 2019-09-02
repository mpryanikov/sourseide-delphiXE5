{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit Androidapi.JNI.ApkExpansion;

interface

uses 
  Androidapi.JNIBridge,
  Androidapi.JNI.Os,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.App,
  Androidapi.JNI.Net,
  Androidapi.JNI.GraphicsContentViewText;

type

  {Class forward declarations}
  JIStub = interface;//com.google.android.vending.expansion.downloader.IStub
  JHelpers = interface;//com.google.android.vending.expansion.downloader.Helpers
  JAPEZProvider = interface;//com.android.vending.expansion.zipfile.APEZProvider
  JIDownloaderService = interface;//com.google.android.vending.expansion.downloader.IDownloaderService
  JDownloadsDB = interface;//com.google.android.vending.expansion.downloader.impl.DownloadsDB
  JIDownloaderClient = interface;//com.google.android.vending.expansion.downloader.IDownloaderClient
  JDownloaderService_GenerateSaveFileError = interface;//com.google.android.vending.expansion.downloader.impl.DownloaderService$GenerateSaveFileError
  JDownloadsDB_DownloadColumns = interface;//com.google.android.vending.expansion.downloader.impl.DownloadsDB$DownloadColumns
  JV14CustomNotification = interface;//com.google.android.vending.expansion.downloader.impl.V14CustomNotification
  JV3CustomNotification = interface;//com.google.android.vending.expansion.downloader.impl.V3CustomNotification
  JDownloadThread = interface;//com.google.android.vending.expansion.downloader.impl.DownloadThread
  JZipResourceFile_ZipEntryRO = interface;//com.android.vending.expansion.zipfile.ZipResourceFile$ZipEntryRO
  JDownloadNotification_ICustomNotification = interface;//com.google.android.vending.expansion.downloader.impl.DownloadNotification$ICustomNotification
  JDownloaderClientMarshaller = interface;//com.google.android.vending.expansion.downloader.DownloaderClientMarshaller
  Jimpl_AndroidHttpClient = interface;//com.google.android.vending.expansion.downloader.impl.AndroidHttpClient
  JAPKExpansionSupport = interface;//com.android.vending.expansion.zipfile.APKExpansionSupport
  JDownloadNotification = interface;//com.google.android.vending.expansion.downloader.impl.DownloadNotification
  JZipResourceFile = interface;//com.android.vending.expansion.zipfile.ZipResourceFile
  JCustomIntentService = interface;//com.google.android.vending.expansion.downloader.impl.CustomIntentService
  JConstants = interface;//com.google.android.vending.expansion.downloader.Constants
  JDownloaderService = interface;//com.google.android.vending.expansion.downloader.impl.DownloaderService
  JCustomNotificationFactory = interface;//com.google.android.vending.expansion.downloader.impl.CustomNotificationFactory
  JDownloaderServiceMarshaller = interface;//com.google.android.vending.expansion.downloader.DownloaderServiceMarshaller
  JDownloadProgressInfo = interface;//com.google.android.vending.expansion.downloader.DownloadProgressInfo
  JDownloadsDB_MetadataColumns = interface;//com.google.android.vending.expansion.downloader.impl.DownloadsDB$MetadataColumns
  JHttpDateTime = interface;//com.google.android.vending.expansion.downloader.impl.HttpDateTime
  JDownloadInfo = interface;//com.google.android.vending.expansion.downloader.impl.DownloadInfo

JIStubClass = interface(IJavaClass)
['{7DE9F188-261B-4291-8D56-CEFDA3A45919}']
end;

[JavaSignature('com/google/android/vending/expansion/downloader/IStub')]
JIStub = interface(IJavaInstance)
['{B84B97DB-7E4D-410E-A35B-2FC426E3BB0E}']
  {Methods}
  procedure connect(c: JContext); cdecl;
  procedure disconnect(c: JContext); cdecl;
  function getMessenger: JMessenger; cdecl;
end;
TJIStub = class(TJavaGenericImport<JIStubClass, JIStub>) end;

JHelpersClass = interface(JObjectClass)
['{A1509108-FA42-4AE4-A17B-29DCB2DABCE0}']
  {Property Methods}
  function _GetsRandom: JRandom;
  procedure _SetsRandom(Value: JRandom);
  {Methods}
  function doesFileExist(c: JContext; fileName: JString; fileSize: Int64; deleteFileOnMismatch: Boolean): Boolean; cdecl;
  function generateSaveFileName(c: JContext; fileName: JString): JString; cdecl;
  function getAvailableBytes(root: JFile): Int64; cdecl;
  function getDownloadProgressPercent(overallProgress: Int64; overallTotal: Int64): JString; cdecl;
  function getDownloadProgressString(overallProgress: Int64; overallTotal: Int64): JString; cdecl;
  function getDownloadProgressStringNotification(overallProgress: Int64; overallTotal: Int64): JString; cdecl;
  function getDownloaderStringResourceIDFromState(state: Integer): Integer; cdecl;
  function getExpansionAPKFileName(c: JContext; mainFile: Boolean; versionCode: Integer): JString; cdecl;
  function getFilesystemRoot(path: JString): JFile; cdecl;
  function getSaveFilePath(c: JContext): JString; cdecl;
  function getSpeedString(bytesPerMillisecond: Single): JString; cdecl;
  function getTimeRemaining(durationInMilliseconds: Int64): JString; cdecl;
  function isExternalMediaMounted: Boolean; cdecl;
  function isFilenameValid(filename: JString): Boolean; cdecl;
  {Properties}
  property sRandom: JRandom read _GetsRandom write _SetsRandom;
end;

[JavaSignature('com/google/android/vending/expansion/downloader/Helpers')]
JHelpers = interface(JObject)
['{0024D8E0-E52D-47C1-95D5-414438B5AAFD}']
end;
TJHelpers = class(TJavaGenericImport<JHelpersClass, JHelpers>) end;

JAPEZProviderClass = interface(JContentProviderClass)
['{13E601D1-B12A-4FF8-8D90-656CB7C976C2}']
  {Property Methods}
  function _GetALL_FIELDS: TJavaObjectArray<JString>;
  function _GetALL_FIELDS_INT: TJavaArray<Integer>;
  function _GetCOMPLEN_IDX: Integer;
  function _GetCOMPRESSEDLEN: JString;
  function _GetCOMPRESSIONTYPE: JString;
  function _GetCOMPTYPE_IDX: Integer;
  function _GetCRC32: JString;
  function _GetCRC_IDX: Integer;
  function _GetFILEID: JString;
  function _GetFILEID_IDX: Integer;
  function _GetFILENAME: JString;
  function _GetFILENAME_IDX: Integer;
  function _GetMODIFICATION: JString;
  function _GetMOD_IDX: Integer;
  function _GetUNCOMPLEN_IDX: Integer;
  function _GetUNCOMPRESSEDLEN: JString;
  function _GetZIPFILE: JString;
  function _GetZIPFILE_IDX: Integer;
  {Methods}
  function init: JAPEZProvider; cdecl;
  {Properties}
  property ALL_FIELDS: TJavaObjectArray<JString> read _GetALL_FIELDS;
  property ALL_FIELDS_INT: TJavaArray<Integer> read _GetALL_FIELDS_INT;
  property COMPLEN_IDX: Integer read _GetCOMPLEN_IDX;
  property COMPRESSEDLEN: JString read _GetCOMPRESSEDLEN;
  property COMPRESSIONTYPE: JString read _GetCOMPRESSIONTYPE;
  property COMPTYPE_IDX: Integer read _GetCOMPTYPE_IDX;
  property CRC32: JString read _GetCRC32;
  property CRC_IDX: Integer read _GetCRC_IDX;
  property FILEID: JString read _GetFILEID;
  property FILEID_IDX: Integer read _GetFILEID_IDX;
  property FILENAME: JString read _GetFILENAME;
  property FILENAME_IDX: Integer read _GetFILENAME_IDX;
  property MODIFICATION: JString read _GetMODIFICATION;
  property MOD_IDX: Integer read _GetMOD_IDX;
  property UNCOMPLEN_IDX: Integer read _GetUNCOMPLEN_IDX;
  property UNCOMPRESSEDLEN: JString read _GetUNCOMPRESSEDLEN;
  property ZIPFILE: JString read _GetZIPFILE;
  property ZIPFILE_IDX: Integer read _GetZIPFILE_IDX;
end;

[JavaSignature('com/android/vending/expansion/zipfile/APEZProvider')]
JAPEZProvider = interface(JContentProvider)
['{3FDC8284-1F6B-46BB-BBFD-4E49A690287E}']
  {Methods}
  function applyBatch(operations: JArrayList): TJavaObjectArray<JContentProviderResult>; cdecl;
  function delete(arg0: Jnet_Uri; arg1: JString; arg2: TJavaObjectArray<JString>): Integer; cdecl;
  function getAuthority: JString; cdecl;
  function getType(uri: Jnet_Uri): JString; cdecl;
  function insert(uri: Jnet_Uri; values: JContentValues): Jnet_Uri; cdecl;
  function onCreate: Boolean; cdecl;
  function openAssetFile(uri: Jnet_Uri; mode: JString): JAssetFileDescriptor; cdecl;
  function openFile(uri: Jnet_Uri; mode: JString): JParcelFileDescriptor; cdecl;
  function query(uri: Jnet_Uri; projection: TJavaObjectArray<JString>; selection: JString; selectionArgs: TJavaObjectArray<JString>; sortOrder: JString): JCursor; cdecl;
  function update(uri: Jnet_Uri; values: JContentValues; selection: JString; selectionArgs: TJavaObjectArray<JString>): Integer; cdecl;
end;
TJAPEZProvider = class(TJavaGenericImport<JAPEZProviderClass, JAPEZProvider>) end;

JIDownloaderServiceClass = interface(IJavaClass)
['{41544B3F-F012-4833-8764-661EDF6F52C3}']
  {Property Methods}
  function _GetFLAGS_DOWNLOAD_OVER_CELLULAR: Integer;
  {Properties}
  property FLAGS_DOWNLOAD_OVER_CELLULAR: Integer read _GetFLAGS_DOWNLOAD_OVER_CELLULAR;
end;

[JavaSignature('com/google/android/vending/expansion/downloader/IDownloaderService')]
JIDownloaderService = interface(IJavaInstance)
['{0D21EF00-EF7F-47D7-971C-4A9D263AA7FC}']
  {Methods}
  procedure onClientUpdated(clientMessenger: JMessenger); cdecl;
  procedure requestAbortDownload; cdecl;
  procedure requestContinueDownload; cdecl;
  procedure requestDownloadStatus; cdecl;
  procedure requestPauseDownload; cdecl;
  procedure setDownloadFlags(flags: Integer); cdecl;
end;
TJIDownloaderService = class(TJavaGenericImport<JIDownloaderServiceClass, JIDownloaderService>) end;

JDownloadsDBClass = interface(JObjectClass)
['{6A0BA0DE-13E4-48F2-B4CB-730B6BB30451}']
  {Property Methods}
  function _GetLOG_TAG: JString;
  {Methods}
  function getDB(paramContext: JContext): JDownloadsDB; cdecl;
  {Properties}
  property LOG_TAG: JString read _GetLOG_TAG;
end;

[JavaSignature('com/google/android/vending/expansion/downloader/impl/DownloadsDB')]
JDownloadsDB = interface(JObject)
['{47CC25B2-3ADA-4611-83ED-E834E01C3C1D}']
  {Methods}
  procedure close; cdecl;
  function getDownloadInfoFromCursor(cur: JCursor): JDownloadInfo; cdecl;
  function getDownloads: TJavaObjectArray<JDownloadInfo>; cdecl;
  function getFlags: Integer; cdecl;
  function getIDByIndex(index: Integer): Int64; cdecl;
  function getIDForDownloadInfo(di: JDownloadInfo): Int64; cdecl;
  function getLastCheckedVersionCode: Integer; cdecl;
  function isDownloadRequired: Boolean; cdecl;
  procedure setDownloadInfoFromCursor(di: JDownloadInfo; cur: JCursor); cdecl;
  function updateDownload(di: JDownloadInfo): Boolean; cdecl; overload;
  function updateDownload(di: JDownloadInfo; cv: JContentValues): Boolean; cdecl; overload;
  procedure updateDownloadCurrentBytes(di: JDownloadInfo); cdecl;
  function updateFlags(flags: Integer): Boolean; cdecl;
  function updateFromDb(di: JDownloadInfo): Boolean; cdecl;
  function updateMetadata(cv: JContentValues): Boolean; cdecl; overload;
  function updateMetadata(apkVersion: Integer; downloadStatus: Integer): Boolean; cdecl; overload;
  function updateStatus(status: Integer): Boolean; cdecl;
end;
TJDownloadsDB = class(TJavaGenericImport<JDownloadsDBClass, JDownloadsDB>) end;

JIDownloaderClientClass = interface(IJavaClass)
['{8159CCD5-7CC0-4895-AD28-8FFFA4662F17}']
  {Property Methods}
  function _GetSTATE_COMPLETED: Integer;
  function _GetSTATE_CONNECTING: Integer;
  function _GetSTATE_DOWNLOADING: Integer;
  function _GetSTATE_FAILED: Integer;
  function _GetSTATE_FAILED_CANCELED: Integer;
  function _GetSTATE_FAILED_FETCHING_URL: Integer;
  function _GetSTATE_FAILED_SDCARD_FULL: Integer;
  function _GetSTATE_FAILED_UNLICENSED: Integer;
  function _GetSTATE_FETCHING_URL: Integer;
  function _GetSTATE_IDLE: Integer;
  function _GetSTATE_PAUSED_BY_REQUEST: Integer;
  function _GetSTATE_PAUSED_NEED_CELLULAR_PERMISSION: Integer;
  function _GetSTATE_PAUSED_NEED_WIFI: Integer;
  function _GetSTATE_PAUSED_NETWORK_SETUP_FAILURE: Integer;
  function _GetSTATE_PAUSED_NETWORK_UNAVAILABLE: Integer;
  function _GetSTATE_PAUSED_ROAMING: Integer;
  function _GetSTATE_PAUSED_SDCARD_UNAVAILABLE: Integer;
  function _GetSTATE_PAUSED_WIFI_DISABLED: Integer;
  function _GetSTATE_PAUSED_WIFI_DISABLED_NEED_CELLULAR_PERMISSION: Integer;
  {Properties}
  property STATE_COMPLETED: Integer read _GetSTATE_COMPLETED;
  property STATE_CONNECTING: Integer read _GetSTATE_CONNECTING;
  property STATE_DOWNLOADING: Integer read _GetSTATE_DOWNLOADING;
  property STATE_FAILED: Integer read _GetSTATE_FAILED;
  property STATE_FAILED_CANCELED: Integer read _GetSTATE_FAILED_CANCELED;
  property STATE_FAILED_FETCHING_URL: Integer read _GetSTATE_FAILED_FETCHING_URL;
  property STATE_FAILED_SDCARD_FULL: Integer read _GetSTATE_FAILED_SDCARD_FULL;
  property STATE_FAILED_UNLICENSED: Integer read _GetSTATE_FAILED_UNLICENSED;
  property STATE_FETCHING_URL: Integer read _GetSTATE_FETCHING_URL;
  property STATE_IDLE: Integer read _GetSTATE_IDLE;
  property STATE_PAUSED_BY_REQUEST: Integer read _GetSTATE_PAUSED_BY_REQUEST;
  property STATE_PAUSED_NEED_CELLULAR_PERMISSION: Integer read _GetSTATE_PAUSED_NEED_CELLULAR_PERMISSION;
  property STATE_PAUSED_NEED_WIFI: Integer read _GetSTATE_PAUSED_NEED_WIFI;
  property STATE_PAUSED_NETWORK_SETUP_FAILURE: Integer read _GetSTATE_PAUSED_NETWORK_SETUP_FAILURE;
  property STATE_PAUSED_NETWORK_UNAVAILABLE: Integer read _GetSTATE_PAUSED_NETWORK_UNAVAILABLE;
  property STATE_PAUSED_ROAMING: Integer read _GetSTATE_PAUSED_ROAMING;
  property STATE_PAUSED_SDCARD_UNAVAILABLE: Integer read _GetSTATE_PAUSED_SDCARD_UNAVAILABLE;
  property STATE_PAUSED_WIFI_DISABLED: Integer read _GetSTATE_PAUSED_WIFI_DISABLED;
  property STATE_PAUSED_WIFI_DISABLED_NEED_CELLULAR_PERMISSION: Integer read _GetSTATE_PAUSED_WIFI_DISABLED_NEED_CELLULAR_PERMISSION;
end;

[JavaSignature('com/google/android/vending/expansion/downloader/IDownloaderClient')]
JIDownloaderClient = interface(IJavaInstance)
['{E35F3579-F34C-48D4-8657-37970D83E7B1}']
  {Methods}
  procedure onDownloadProgress(progress: JDownloadProgressInfo); cdecl;
  procedure onDownloadStateChanged(newState: Integer); cdecl;
  procedure onServiceConnected(m: JMessenger); cdecl;
end;
TJIDownloaderClient = class(TJavaGenericImport<JIDownloaderClientClass, JIDownloaderClient>) end;

JDownloaderService_GenerateSaveFileErrorClass = interface(JExceptionClass)
['{AD77F5D6-1F2A-4BAE-B67F-C7637A7B3CFD}']
  {Methods}
  function init(status: Integer; message: JString): JDownloaderService_GenerateSaveFileError; cdecl;
end;

[JavaSignature('com/google/android/vending/expansion/downloader/impl/DownloaderService$GenerateSaveFileError')]
JDownloaderService_GenerateSaveFileError = interface(JException)
['{329A95DC-A15B-4B76-B251-145B96A34D64}']
end;
TJDownloaderService_GenerateSaveFileError = class(TJavaGenericImport<JDownloaderService_GenerateSaveFileErrorClass, JDownloaderService_GenerateSaveFileError>) end;

JDownloadsDB_DownloadColumnsClass = interface(JObjectClass)
['{309FB969-9E60-4FEE-9083-5708B1FEB032}']
  {Property Methods}
  function _GetCONTROL: JString;
  function _GetCURRENTBYTES: JString;
  function _GetETAG: JString;
  function _GetFILENAME: JString;
  function _GetINDEX: JString;
  function _GetLASTMOD: JString;
  function _GetNUM_FAILED: JString;
  function _GetREDIRECT_COUNT: JString;
  function _GetRETRY_AFTER: JString;
  function _GetSCHEMA: TJavaObjectBiArray<JString>;
  function _GetSTATUS: JString;
  function _GetTABLE_NAME: JString;
  function _GetTOTALBYTES: JString;
  function _GetURI: JString;
  function _Get_ID: JString;
  {Methods}
  function init: JDownloadsDB_DownloadColumns; cdecl;
  {Properties}
  property CONTROL: JString read _GetCONTROL;
  property CURRENTBYTES: JString read _GetCURRENTBYTES;
  property ETAG: JString read _GetETAG;
  property FILENAME: JString read _GetFILENAME;
  property INDEX: JString read _GetINDEX;
  property LASTMOD: JString read _GetLASTMOD;
  property NUM_FAILED: JString read _GetNUM_FAILED;
  property REDIRECT_COUNT: JString read _GetREDIRECT_COUNT;
  property RETRY_AFTER: JString read _GetRETRY_AFTER;
  property SCHEMA: TJavaObjectBiArray<JString> read _GetSCHEMA;
  property STATUS: JString read _GetSTATUS;
  property TABLE_NAME: JString read _GetTABLE_NAME;
  property TOTALBYTES: JString read _GetTOTALBYTES;
  property URI: JString read _GetURI;
  property _ID: JString read _Get_ID;
end;

[JavaSignature('com/google/android/vending/expansion/downloader/impl/DownloadsDB$DownloadColumns')]
JDownloadsDB_DownloadColumns = interface(JObject)
['{70F2EEAB-F288-4E3C-A81F-86DAAB2E3810}']
end;
TJDownloadsDB_DownloadColumns = class(TJavaGenericImport<JDownloadsDB_DownloadColumnsClass, JDownloadsDB_DownloadColumns>) end;

JV14CustomNotificationClass = interface(JObjectClass)
['{D16E2048-BF59-443A-A0D7-446AB9F09FE9}']
  {Methods}
  function init: JV14CustomNotification; cdecl;
end;

[JavaSignature('com/google/android/vending/expansion/downloader/impl/V14CustomNotification')]
JV14CustomNotification = interface(JObject)
['{EB90C5CB-A513-4FF8-9327-E7FF28DF2AEC}']
  {Methods}
  procedure setCurrentBytes(currentBytes: Int64); cdecl;
  procedure setIcon(icon: Integer); cdecl;
  procedure setPendingIntent(contentIntent: JPendingIntent); cdecl;
  procedure setTicker(ticker: JCharSequence); cdecl;
  procedure setTimeRemaining(timeRemaining: Int64); cdecl;
  procedure setTitle(title: JCharSequence); cdecl;
  procedure setTotalBytes(totalBytes: Int64); cdecl;
  function updateNotification(c: JContext): JNotification; cdecl;
end;
TJV14CustomNotification = class(TJavaGenericImport<JV14CustomNotificationClass, JV14CustomNotification>) end;

JV3CustomNotificationClass = interface(JObjectClass)
['{4E619072-0DB1-4DBD-A7D8-F8DA4BC36366}']
  {Methods}
  function init: JV3CustomNotification; cdecl;
end;

[JavaSignature('com/google/android/vending/expansion/downloader/impl/V3CustomNotification')]
JV3CustomNotification = interface(JObject)
['{AF9A3534-5317-46CD-A1E2-03D78E324E8E}']
  {Methods}
  procedure setCurrentBytes(currentBytes: Int64); cdecl;
  procedure setIcon(icon: Integer); cdecl;
  procedure setPendingIntent(contentIntent: JPendingIntent); cdecl;
  procedure setTicker(ticker: JCharSequence); cdecl;
  procedure setTimeRemaining(timeRemaining: Int64); cdecl;
  procedure setTitle(title: JCharSequence); cdecl;
  procedure setTotalBytes(totalBytes: Int64); cdecl;
  function updateNotification(c: JContext): JNotification; cdecl;
end;
TJV3CustomNotification = class(TJavaGenericImport<JV3CustomNotificationClass, JV3CustomNotification>) end;

JDownloadThreadClass = interface(JObjectClass)
['{1BDE2D91-16DD-4B28-A84A-7D73A434B5AA}']
  {Methods}
  function init(info: JDownloadInfo; service: JDownloaderService; notification: JDownloadNotification): JDownloadThread; cdecl;
end;

[JavaSignature('com/google/android/vending/expansion/downloader/impl/DownloadThread')]
JDownloadThread = interface(JObject)
['{4F1F71DC-33F8-4F2D-B528-ED3397ABE958}']
  {Methods}
  procedure run; cdecl;
end;
TJDownloadThread = class(TJavaGenericImport<JDownloadThreadClass, JDownloadThread>) end;

JZipResourceFile_ZipEntryROClass = interface(JObjectClass)
['{D07787A2-F375-4CCF-8657-1B754ADBC6F5}']
  {Methods}
  function init(zipFileName: JString; file_: JFile; fileName: JString): JZipResourceFile_ZipEntryRO; cdecl;
end;

[JavaSignature('com/android/vending/expansion/zipfile/ZipResourceFile$ZipEntryRO')]
JZipResourceFile_ZipEntryRO = interface(JObject)
['{35231A8E-8FB0-4267-A675-EC98A8ACD0CF}']
  {Property Methods}
  function _GetmCRC32: Int64;
  procedure _SetmCRC32(Value: Int64);
  function _GetmCompressedLength: Int64;
  procedure _SetmCompressedLength(Value: Int64);
  function _GetmFile: JFile;
  function _GetmFileName: JString;
  function _GetmLocalHdrOffset: Int64;
  procedure _SetmLocalHdrOffset(Value: Int64);
  function _GetmMethod: Integer;
  procedure _SetmMethod(Value: Integer);
  function _GetmOffset: Int64;
  procedure _SetmOffset(Value: Int64);
  function _GetmUncompressedLength: Int64;
  procedure _SetmUncompressedLength(Value: Int64);
  function _GetmWhenModified: Int64;
  procedure _SetmWhenModified(Value: Int64);
  function _GetmZipFileName: JString;
  {Methods}
  function getAssetFileDescriptor: JAssetFileDescriptor; cdecl;
  function getOffset: Int64; cdecl;
  function getZipFile: JFile; cdecl;
  function getZipFileName: JString; cdecl;
  function isUncompressed: Boolean; cdecl;
  {Properties}
  property mCRC32: Int64 read _GetmCRC32 write _SetmCRC32;
  property mCompressedLength: Int64 read _GetmCompressedLength write _SetmCompressedLength;
  property mFile: JFile read _GetmFile;
  property mFileName: JString read _GetmFileName;
  property mLocalHdrOffset: Int64 read _GetmLocalHdrOffset write _SetmLocalHdrOffset;
  property mMethod: Integer read _GetmMethod write _SetmMethod;
  property mOffset: Int64 read _GetmOffset write _SetmOffset;
  property mUncompressedLength: Int64 read _GetmUncompressedLength write _SetmUncompressedLength;
  property mWhenModified: Int64 read _GetmWhenModified write _SetmWhenModified;
  property mZipFileName: JString read _GetmZipFileName;
end;
TJZipResourceFile_ZipEntryRO = class(TJavaGenericImport<JZipResourceFile_ZipEntryROClass, JZipResourceFile_ZipEntryRO>) end;

JDownloadNotification_ICustomNotificationClass = interface(IJavaClass)
['{8158EF16-A61B-4085-9B67-7A7BD35D139B}']
end;

[JavaSignature('com/google/android/vending/expansion/downloader/impl/DownloadNotification$ICustomNotification')]
JDownloadNotification_ICustomNotification = interface(IJavaInstance)
['{56E88756-E5A1-47EF-8506-84EB871A0EE8}']
  {Methods}
  procedure setCurrentBytes(currentBytes: Int64); cdecl;
  procedure setIcon(iconResource: Integer); cdecl;
  procedure setPendingIntent(mContentIntent: JPendingIntent); cdecl;
  procedure setTicker(ticker: JCharSequence); cdecl;
  procedure setTimeRemaining(timeRemaining: Int64); cdecl;
  procedure setTitle(title: JCharSequence); cdecl;
  procedure setTotalBytes(totalBytes: Int64); cdecl;
  function updateNotification(c: JContext): JNotification; cdecl;
end;
TJDownloadNotification_ICustomNotification = class(TJavaGenericImport<JDownloadNotification_ICustomNotificationClass, JDownloadNotification_ICustomNotification>) end;

JDownloaderClientMarshallerClass = interface(JObjectClass)
['{75A82EEA-6851-420D-94F6-FD0ECA9F2DE5}']
  {Property Methods}
  function _GetDOWNLOAD_REQUIRED: Integer;
  function _GetLVL_CHECK_REQUIRED: Integer;
  function _GetMSG_ONDOWNLOADPROGRESS: Integer;
  function _GetMSG_ONDOWNLOADSTATE_CHANGED: Integer;
  function _GetMSG_ONSERVICECONNECTED: Integer;
  function _GetNO_DOWNLOAD_REQUIRED: Integer;
  function _GetPARAM_MESSENGER: JString;
  function _GetPARAM_NEW_STATE: JString;
  function _GetPARAM_PROGRESS: JString;
  {Methods}
  function init: JDownloaderClientMarshaller; cdecl;
  function CreateProxy(msg: JMessenger): JIDownloaderClient; cdecl;
  function CreateStub(itf: JIDownloaderClient; downloaderService: Jlang_Class): JIStub; cdecl;
  function startDownloadServiceIfRequired(context: JContext; notificationClient: JPendingIntent; serviceClass: Jlang_Class): Integer; cdecl; overload;
  function startDownloadServiceIfRequired(context: JContext; notificationClient: JIntent; serviceClass: Jlang_Class): Integer; cdecl; overload;
  {Properties}
  property DOWNLOAD_REQUIRED: Integer read _GetDOWNLOAD_REQUIRED;
  property LVL_CHECK_REQUIRED: Integer read _GetLVL_CHECK_REQUIRED;
  property MSG_ONDOWNLOADPROGRESS: Integer read _GetMSG_ONDOWNLOADPROGRESS;
  property MSG_ONDOWNLOADSTATE_CHANGED: Integer read _GetMSG_ONDOWNLOADSTATE_CHANGED;
  property MSG_ONSERVICECONNECTED: Integer read _GetMSG_ONSERVICECONNECTED;
  property NO_DOWNLOAD_REQUIRED: Integer read _GetNO_DOWNLOAD_REQUIRED;
  property PARAM_MESSENGER: JString read _GetPARAM_MESSENGER;
  property PARAM_NEW_STATE: JString read _GetPARAM_NEW_STATE;
  property PARAM_PROGRESS: JString read _GetPARAM_PROGRESS;
end;

[JavaSignature('com/google/android/vending/expansion/downloader/DownloaderClientMarshaller')]
JDownloaderClientMarshaller = interface(JObject)
['{86C0E465-3E15-47C3-8021-19E7D3E46A16}']
end;
TJDownloaderClientMarshaller = class(TJavaGenericImport<JDownloaderClientMarshallerClass, JDownloaderClientMarshaller>) end;

Jimpl_AndroidHttpClientClass = interface(JObjectClass)
['{B158F44C-9ADC-4417-96E2-2975241988A2}']
  {Property Methods}
  function _GetDEFAULT_SYNC_MIN_GZIP_BYTES: Int64;
  procedure _SetDEFAULT_SYNC_MIN_GZIP_BYTES(Value: Int64);
  {Methods}
  function getMinGzipSize(resolver: JContentResolver): Int64; cdecl;
  function newInstance(userAgent: JString; context: JContext): Jimpl_AndroidHttpClient; cdecl; overload;
  function newInstance(userAgent: JString): Jimpl_AndroidHttpClient; cdecl; overload;
  function parseDate(dateString: JString): Int64; cdecl;
  {Properties}
  property DEFAULT_SYNC_MIN_GZIP_BYTES: Int64 read _GetDEFAULT_SYNC_MIN_GZIP_BYTES write _SetDEFAULT_SYNC_MIN_GZIP_BYTES;
end;

[JavaSignature('com/google/android/vending/expansion/downloader/impl/AndroidHttpClient')]
Jimpl_AndroidHttpClient = interface(JObject)
['{187A726C-B84D-407D-8B93-E2F1D9E52884}']
  {Methods}
  procedure close; cdecl;
  procedure disableCurlLogging; cdecl;
  procedure enableCurlLogging(name: JString; level: Integer); cdecl;
end;
TJimpl_AndroidHttpClient = class(TJavaGenericImport<Jimpl_AndroidHttpClientClass, Jimpl_AndroidHttpClient>) end;

JAPKExpansionSupportClass = interface(JObjectClass)
['{E5FEF31B-C67C-482E-8A40-9B5CC76EEBAF}']
  {Methods}
  function init: JAPKExpansionSupport; cdecl;
  function getAPKExpansionZipFile(ctx: JContext; mainVersion: Integer; patchVersion: Integer): JZipResourceFile; cdecl;
  function getResourceZipFile(expansionFiles: TJavaObjectArray<JString>): JZipResourceFile; cdecl;
end;

[JavaSignature('com/android/vending/expansion/zipfile/APKExpansionSupport')]
JAPKExpansionSupport = interface(JObject)
['{246F29E7-8207-4E28-A130-743ADACCE9D6}']
end;
TJAPKExpansionSupport = class(TJavaGenericImport<JAPKExpansionSupportClass, JAPKExpansionSupport>) end;

JDownloadNotificationClass = interface(JObjectClass)
['{92E75B92-17A5-49AA-8C2B-0866FD212482}']
end;

[JavaSignature('com/google/android/vending/expansion/downloader/impl/DownloadNotification')]
JDownloadNotification = interface(JObject)
['{637B8622-9735-48D9-99DD-ECE406B60F73}']
  {Methods}
  function getClientIntent: JPendingIntent; cdecl;
  procedure onDownloadProgress(progress: JDownloadProgressInfo); cdecl;
  procedure onDownloadStateChanged(newState: Integer); cdecl;
  procedure onServiceConnected(m: JMessenger); cdecl;
  procedure resendState; cdecl;
  procedure setClientIntent(mClientIntent: JPendingIntent); cdecl;
  procedure setMessenger(msg: JMessenger); cdecl;
end;
TJDownloadNotification = class(TJavaGenericImport<JDownloadNotificationClass, JDownloadNotification>) end;

JZipResourceFileClass = interface(JObjectClass)
['{B8558FFF-0D9D-410A-8D09-2D29A2A06206}']
  {Methods}
  function init(zipFileName: JString): JZipResourceFile; cdecl;
end;

[JavaSignature('com/android/vending/expansion/zipfile/ZipResourceFile')]
JZipResourceFile = interface(JObject)
['{1C126860-FBD8-4756-B59C-99C3CDD7DEA7}']
  {Property Methods}
  function _GetmZipFiles: JHashMap;
  procedure _SetmZipFiles(Value: JHashMap);
  {Methods}
  function getAllEntries: TJavaObjectArray<JZipResourceFile_ZipEntryRO>; cdecl;
  function getAssetFileDescriptor(assetPath: JString): JAssetFileDescriptor; cdecl;
  function getInputStream(assetPath: JString): JInputStream; cdecl;
  {Properties}
  property mZipFiles: JHashMap read _GetmZipFiles write _SetmZipFiles;
end;
TJZipResourceFile = class(TJavaGenericImport<JZipResourceFileClass, JZipResourceFile>) end;

JCustomIntentServiceClass = interface(JServiceClass)
['{847A1962-3CB2-4980-B2AF-F1D559D224F7}']
  {Methods}
  function init(paramString: JString): JCustomIntentService; cdecl;
end;

[JavaSignature('com/google/android/vending/expansion/downloader/impl/CustomIntentService')]
JCustomIntentService = interface(JService)
['{BA491650-C614-4C1C-836A-A8B0BA7DBCF2}']
  {Methods}
  function onBind(paramIntent: JIntent): JIBinder; cdecl;
  procedure onCreate; cdecl;
  procedure onDestroy; cdecl;
  procedure onStart(paramIntent: JIntent; startId: Integer); cdecl;
  function onStartCommand(paramIntent: JIntent; flags: Integer; startId: Integer): Integer; cdecl;
  procedure setIntentRedelivery(enabled: Boolean); cdecl;
end;
TJCustomIntentService = class(TJavaGenericImport<JCustomIntentServiceClass, JCustomIntentService>) end;

JConstantsClass = interface(JObjectClass)
['{BAD49A84-76B7-441E-840A-75BFA37F76AE}']
  {Property Methods}
  function _GetACTION_HIDE: JString;
  function _GetACTION_LIST: JString;
  function _GetACTION_OPEN: JString;
  function _GetACTION_RETRY: JString;
  function _GetACTIVE_THREAD_WATCHDOG: Int64;
  function _GetBUFFER_SIZE: Integer;
  function _GetDEFAULT_USER_AGENT: JString;
  function _GetEXP_PATH: JString;
  function _GetFILENAME_SEQUENCE_SEPARATOR: JString;
  function _GetLOGV: Boolean;
  function _GetLOGVV: Boolean;
  function _GetLOGX: Boolean;
  function _GetMAX_DOWNLOADS: Integer;
  function _GetMAX_REDIRECTS: Integer;
  function _GetMAX_RETRIES: Integer;
  function _GetMAX_RETRY_AFTER: Integer;
  function _GetMIN_ARTIFICIAL_ERROR_STATUS: Integer;
  function _GetMIN_PROGRESS_STEP: Integer;
  function _GetMIN_PROGRESS_TIME: Int64;
  function _GetMIN_RETRY_AFTER: Integer;
  function _GetRETRY_FIRST_DELAY: Integer;
  function _GetSTATUS_BAD_REQUEST: Integer;
  function _GetSTATUS_CANCELED: Integer;
  function _GetSTATUS_CANNOT_RESUME: Integer;
  function _GetSTATUS_DEVICE_NOT_FOUND_ERROR: Integer;
  function _GetSTATUS_FILE_ALREADY_EXISTS_ERROR: Integer;
  function _GetSTATUS_FILE_ERROR: Integer;
  function _GetSTATUS_HTTP_DATA_ERROR: Integer;
  function _GetSTATUS_HTTP_EXCEPTION: Integer;
  function _GetSTATUS_INSUFFICIENT_SPACE_ERROR: Integer;
  function _GetSTATUS_LENGTH_REQUIRED: Integer;
  function _GetSTATUS_NOT_ACCEPTABLE: Integer;
  function _GetSTATUS_PRECONDITION_FAILED: Integer;
  function _GetSTATUS_SUCCESS: Integer;
  function _GetSTATUS_TOO_MANY_REDIRECTS: Integer;
  function _GetSTATUS_UNHANDLED_HTTP_CODE: Integer;
  function _GetSTATUS_UNHANDLED_REDIRECT: Integer;
  function _GetSTATUS_UNKNOWN_ERROR: Integer;
  function _GetTAG: JString;
  function _GetWATCHDOG_WAKE_TIMER: Int64;
  {Methods}
  function init: JConstants; cdecl;
  {Properties}
  property ACTION_HIDE: JString read _GetACTION_HIDE;
  property ACTION_LIST: JString read _GetACTION_LIST;
  property ACTION_OPEN: JString read _GetACTION_OPEN;
  property ACTION_RETRY: JString read _GetACTION_RETRY;
  property ACTIVE_THREAD_WATCHDOG: Int64 read _GetACTIVE_THREAD_WATCHDOG;
  property BUFFER_SIZE: Integer read _GetBUFFER_SIZE;
  property DEFAULT_USER_AGENT: JString read _GetDEFAULT_USER_AGENT;
  property EXP_PATH: JString read _GetEXP_PATH;
  property FILENAME_SEQUENCE_SEPARATOR: JString read _GetFILENAME_SEQUENCE_SEPARATOR;
  property LOGV: Boolean read _GetLOGV;
  property LOGVV: Boolean read _GetLOGVV;
  property LOGX: Boolean read _GetLOGX;
  property MAX_DOWNLOADS: Integer read _GetMAX_DOWNLOADS;
  property MAX_REDIRECTS: Integer read _GetMAX_REDIRECTS;
  property MAX_RETRIES: Integer read _GetMAX_RETRIES;
  property MAX_RETRY_AFTER: Integer read _GetMAX_RETRY_AFTER;
  property MIN_ARTIFICIAL_ERROR_STATUS: Integer read _GetMIN_ARTIFICIAL_ERROR_STATUS;
  property MIN_PROGRESS_STEP: Integer read _GetMIN_PROGRESS_STEP;
  property MIN_PROGRESS_TIME: Int64 read _GetMIN_PROGRESS_TIME;
  property MIN_RETRY_AFTER: Integer read _GetMIN_RETRY_AFTER;
  property RETRY_FIRST_DELAY: Integer read _GetRETRY_FIRST_DELAY;
  property STATUS_BAD_REQUEST: Integer read _GetSTATUS_BAD_REQUEST;
  property STATUS_CANCELED: Integer read _GetSTATUS_CANCELED;
  property STATUS_CANNOT_RESUME: Integer read _GetSTATUS_CANNOT_RESUME;
  property STATUS_DEVICE_NOT_FOUND_ERROR: Integer read _GetSTATUS_DEVICE_NOT_FOUND_ERROR;
  property STATUS_FILE_ALREADY_EXISTS_ERROR: Integer read _GetSTATUS_FILE_ALREADY_EXISTS_ERROR;
  property STATUS_FILE_ERROR: Integer read _GetSTATUS_FILE_ERROR;
  property STATUS_HTTP_DATA_ERROR: Integer read _GetSTATUS_HTTP_DATA_ERROR;
  property STATUS_HTTP_EXCEPTION: Integer read _GetSTATUS_HTTP_EXCEPTION;
  property STATUS_INSUFFICIENT_SPACE_ERROR: Integer read _GetSTATUS_INSUFFICIENT_SPACE_ERROR;
  property STATUS_LENGTH_REQUIRED: Integer read _GetSTATUS_LENGTH_REQUIRED;
  property STATUS_NOT_ACCEPTABLE: Integer read _GetSTATUS_NOT_ACCEPTABLE;
  property STATUS_PRECONDITION_FAILED: Integer read _GetSTATUS_PRECONDITION_FAILED;
  property STATUS_SUCCESS: Integer read _GetSTATUS_SUCCESS;
  property STATUS_TOO_MANY_REDIRECTS: Integer read _GetSTATUS_TOO_MANY_REDIRECTS;
  property STATUS_UNHANDLED_HTTP_CODE: Integer read _GetSTATUS_UNHANDLED_HTTP_CODE;
  property STATUS_UNHANDLED_REDIRECT: Integer read _GetSTATUS_UNHANDLED_REDIRECT;
  property STATUS_UNKNOWN_ERROR: Integer read _GetSTATUS_UNKNOWN_ERROR;
  property TAG: JString read _GetTAG;
  property WATCHDOG_WAKE_TIMER: Int64 read _GetWATCHDOG_WAKE_TIMER;
end;

[JavaSignature('com/google/android/vending/expansion/downloader/Constants')]
JConstants = interface(JObject)
['{A453331C-B467-49F3-813F-42088D599D91}']
end;
TJConstants = class(TJavaGenericImport<JConstantsClass, JConstants>) end;

JDownloaderServiceClass = interface(JCustomIntentServiceClass)
['{2F4F734A-5053-469C-A97C-2BD2AF8D24BF}']
  {Property Methods}
  function _GetACTION_DOWNLOADS_CHANGED: JString;
  function _GetACTION_DOWNLOAD_COMPLETE: JString;
  function _GetACTION_DOWNLOAD_STATUS: JString;
  function _GetDOWNLOAD_REQUIRED: Integer;
  function _GetEXTRA_FILE_NAME: JString;
  function _GetEXTRA_IS_WIFI_REQUIRED: JString;
  function _GetEXTRA_MESSAGE_HANDLER: JString;
  function _GetEXTRA_PACKAGE_NAME: JString;
  function _GetEXTRA_PENDING_INTENT: JString;
  function _GetEXTRA_STATUS_CURRENT_FILE_SIZE: JString;
  function _GetEXTRA_STATUS_CURRENT_PROGRESS: JString;
  function _GetEXTRA_STATUS_STATE: JString;
  function _GetEXTRA_STATUS_TOTAL_PROGRESS: JString;
  function _GetEXTRA_STATUS_TOTAL_SIZE: JString;
  function _GetLVL_CHECK_REQUIRED: Integer;
  function _GetNETWORK_CANNOT_USE_ROAMING: Integer;
  function _GetNETWORK_MOBILE: Integer;
  function _GetNETWORK_NO_CONNECTION: Integer;
  function _GetNETWORK_OK: Integer;
  function _GetNETWORK_RECOMMENDED_UNUSABLE_DUE_TO_SIZE: Integer;
  function _GetNETWORK_TYPE_DISALLOWED_BY_REQUESTOR: Integer;
  function _GetNETWORK_UNUSABLE_DUE_TO_SIZE: Integer;
  function _GetNETWORK_WIFI: Integer;
  function _GetNO_DOWNLOAD_REQUIRED: Integer;
  function _GetSTATUS_CANNOT_RESUME: Integer;
  function _GetSTATUS_FILE_ALREADY_EXISTS_ERROR: Integer;
  function _GetSTATUS_FILE_DELIVERED_INCORRECTLY: Integer;
  function _GetSTATUS_FORBIDDEN: Integer;
  function _GetSTATUS_PAUSED_BY_APP: Integer;
  function _GetSTATUS_PENDING: Integer;
  function _GetSTATUS_QUEUED_FOR_WIFI: Integer;
  function _GetSTATUS_QUEUED_FOR_WIFI_OR_CELLULAR_PERMISSION: Integer;
  function _GetSTATUS_RUNNING: Integer;
  function _GetSTATUS_UNKNOWN_ERROR: Integer;
  function _GetSTATUS_WAITING_FOR_NETWORK: Integer;
  function _GetSTATUS_WAITING_TO_RETRY: Integer;
  {Methods}
  function init: JDownloaderService; cdecl;
  function isStatusClientError(status: Integer): Boolean; cdecl;
  function isStatusCompleted(status: Integer): Boolean; cdecl;
  function isStatusError(status: Integer): Boolean; cdecl;
  function isStatusInformational(status: Integer): Boolean; cdecl;
  function isStatusServerError(status: Integer): Boolean; cdecl;
  function isStatusSuccess(status: Integer): Boolean; cdecl;
  function startDownloadServiceIfRequired(context: JContext; intent: JIntent; serviceClass: Jlang_Class): Integer; cdecl; overload;
  function startDownloadServiceIfRequired(context: JContext; pendingIntent: JPendingIntent; serviceClass: Jlang_Class): Integer; cdecl; overload;
  function startDownloadServiceIfRequired(context: JContext; pendingIntent: JPendingIntent; classPackage: JString; className: JString): Integer; cdecl; overload;
  {Properties}
  property ACTION_DOWNLOADS_CHANGED: JString read _GetACTION_DOWNLOADS_CHANGED;
  property ACTION_DOWNLOAD_COMPLETE: JString read _GetACTION_DOWNLOAD_COMPLETE;
  property ACTION_DOWNLOAD_STATUS: JString read _GetACTION_DOWNLOAD_STATUS;
  property DOWNLOAD_REQUIRED: Integer read _GetDOWNLOAD_REQUIRED;
  property EXTRA_FILE_NAME: JString read _GetEXTRA_FILE_NAME;
  property EXTRA_IS_WIFI_REQUIRED: JString read _GetEXTRA_IS_WIFI_REQUIRED;
  property EXTRA_MESSAGE_HANDLER: JString read _GetEXTRA_MESSAGE_HANDLER;
  property EXTRA_PACKAGE_NAME: JString read _GetEXTRA_PACKAGE_NAME;
  property EXTRA_PENDING_INTENT: JString read _GetEXTRA_PENDING_INTENT;
  property EXTRA_STATUS_CURRENT_FILE_SIZE: JString read _GetEXTRA_STATUS_CURRENT_FILE_SIZE;
  property EXTRA_STATUS_CURRENT_PROGRESS: JString read _GetEXTRA_STATUS_CURRENT_PROGRESS;
  property EXTRA_STATUS_STATE: JString read _GetEXTRA_STATUS_STATE;
  property EXTRA_STATUS_TOTAL_PROGRESS: JString read _GetEXTRA_STATUS_TOTAL_PROGRESS;
  property EXTRA_STATUS_TOTAL_SIZE: JString read _GetEXTRA_STATUS_TOTAL_SIZE;
  property LVL_CHECK_REQUIRED: Integer read _GetLVL_CHECK_REQUIRED;
  property NETWORK_CANNOT_USE_ROAMING: Integer read _GetNETWORK_CANNOT_USE_ROAMING;
  property NETWORK_MOBILE: Integer read _GetNETWORK_MOBILE;
  property NETWORK_NO_CONNECTION: Integer read _GetNETWORK_NO_CONNECTION;
  property NETWORK_OK: Integer read _GetNETWORK_OK;
  property NETWORK_RECOMMENDED_UNUSABLE_DUE_TO_SIZE: Integer read _GetNETWORK_RECOMMENDED_UNUSABLE_DUE_TO_SIZE;
  property NETWORK_TYPE_DISALLOWED_BY_REQUESTOR: Integer read _GetNETWORK_TYPE_DISALLOWED_BY_REQUESTOR;
  property NETWORK_UNUSABLE_DUE_TO_SIZE: Integer read _GetNETWORK_UNUSABLE_DUE_TO_SIZE;
  property NETWORK_WIFI: Integer read _GetNETWORK_WIFI;
  property NO_DOWNLOAD_REQUIRED: Integer read _GetNO_DOWNLOAD_REQUIRED;
  property STATUS_CANNOT_RESUME: Integer read _GetSTATUS_CANNOT_RESUME;
  property STATUS_FILE_ALREADY_EXISTS_ERROR: Integer read _GetSTATUS_FILE_ALREADY_EXISTS_ERROR;
  property STATUS_FILE_DELIVERED_INCORRECTLY: Integer read _GetSTATUS_FILE_DELIVERED_INCORRECTLY;
  property STATUS_FORBIDDEN: Integer read _GetSTATUS_FORBIDDEN;
  property STATUS_PAUSED_BY_APP: Integer read _GetSTATUS_PAUSED_BY_APP;
  property STATUS_PENDING: Integer read _GetSTATUS_PENDING;
  property STATUS_QUEUED_FOR_WIFI: Integer read _GetSTATUS_QUEUED_FOR_WIFI;
  property STATUS_QUEUED_FOR_WIFI_OR_CELLULAR_PERMISSION: Integer read _GetSTATUS_QUEUED_FOR_WIFI_OR_CELLULAR_PERMISSION;
  property STATUS_RUNNING: Integer read _GetSTATUS_RUNNING;
  property STATUS_UNKNOWN_ERROR: Integer read _GetSTATUS_UNKNOWN_ERROR;
  property STATUS_WAITING_FOR_NETWORK: Integer read _GetSTATUS_WAITING_FOR_NETWORK;
  property STATUS_WAITING_TO_RETRY: Integer read _GetSTATUS_WAITING_TO_RETRY;
end;

[JavaSignature('com/google/android/vending/expansion/downloader/impl/DownloaderService')]
JDownloaderService = interface(JCustomIntentService)
['{2DFE6D5F-4CC2-4712-B66C-A12CBB7116D1}']
  {Methods}
  function generateSaveFile(filename: JString; filesize: Int64): JString; cdecl;
  function generateTempSaveFileName(fileName: JString): JString; cdecl;
  function getAlarmReceiverClassName: JString; cdecl;
  function getControl: Integer; cdecl;
  function getLogMessageForNetworkError(networkError: Integer): JString; cdecl;
  function getNetworkAvailabilityState(db: JDownloadsDB): Integer; cdecl;
  function getPublicKey: JString; cdecl;
  function getSALT: TJavaArray<Byte>; cdecl;
  function getStatus: Integer; cdecl;
  function handleFileUpdated(db: JDownloadsDB; index: Integer; filename: JString; fileSize: Int64): Boolean; cdecl;
  function isWiFi: Boolean; cdecl;
  procedure notifyUpdateBytes(totalBytesSoFar: Int64); cdecl;
  function onBind(paramIntent: JIntent): JIBinder; cdecl;
  procedure onClientUpdated(clientMessenger: JMessenger); cdecl;
  procedure onCreate; cdecl;
  procedure onDestroy; cdecl;
  procedure requestAbortDownload; cdecl;
  procedure requestContinueDownload; cdecl;
  procedure requestDownloadStatus; cdecl;
  procedure requestPauseDownload; cdecl;
  procedure setDownloadFlags(flags: Integer); cdecl;
  procedure updateLVL(context: JContext); cdecl;
end;
TJDownloaderService = class(TJavaGenericImport<JDownloaderServiceClass, JDownloaderService>) end;

JCustomNotificationFactoryClass = interface(JObjectClass)
['{7F8598EB-108D-49B1-8A24-1DCAC828734C}']
  {Methods}
  function init: JCustomNotificationFactory; cdecl;
  function createCustomNotification: JDownloadNotification_ICustomNotification; cdecl;
end;

[JavaSignature('com/google/android/vending/expansion/downloader/impl/CustomNotificationFactory')]
JCustomNotificationFactory = interface(JObject)
['{FC2DE5B2-6F5C-424E-A8A3-47071E65B729}']
end;
TJCustomNotificationFactory = class(TJavaGenericImport<JCustomNotificationFactoryClass, JCustomNotificationFactory>) end;

JDownloaderServiceMarshallerClass = interface(JObjectClass)
['{C0AA10E1-4A1A-40AA-A7B5-E700C2F8179F}']
  {Property Methods}
  function _GetMSG_REQUEST_ABORT_DOWNLOAD: Integer;
  function _GetMSG_REQUEST_CLIENT_UPDATE: Integer;
  function _GetMSG_REQUEST_CONTINUE_DOWNLOAD: Integer;
  function _GetMSG_REQUEST_DOWNLOAD_STATE: Integer;
  function _GetMSG_REQUEST_PAUSE_DOWNLOAD: Integer;
  function _GetMSG_SET_DOWNLOAD_FLAGS: Integer;
  function _GetPARAMS_FLAGS: JString;
  function _GetPARAM_MESSENGER: JString;
  {Methods}
  function init: JDownloaderServiceMarshaller; cdecl;
  function CreateProxy(msg: JMessenger): JIDownloaderService; cdecl;
  function CreateStub(itf: JIDownloaderService): JIStub; cdecl;
  {Properties}
  property MSG_REQUEST_ABORT_DOWNLOAD: Integer read _GetMSG_REQUEST_ABORT_DOWNLOAD;
  property MSG_REQUEST_CLIENT_UPDATE: Integer read _GetMSG_REQUEST_CLIENT_UPDATE;
  property MSG_REQUEST_CONTINUE_DOWNLOAD: Integer read _GetMSG_REQUEST_CONTINUE_DOWNLOAD;
  property MSG_REQUEST_DOWNLOAD_STATE: Integer read _GetMSG_REQUEST_DOWNLOAD_STATE;
  property MSG_REQUEST_PAUSE_DOWNLOAD: Integer read _GetMSG_REQUEST_PAUSE_DOWNLOAD;
  property MSG_SET_DOWNLOAD_FLAGS: Integer read _GetMSG_SET_DOWNLOAD_FLAGS;
  property PARAMS_FLAGS: JString read _GetPARAMS_FLAGS;
  property PARAM_MESSENGER: JString read _GetPARAM_MESSENGER;
end;

[JavaSignature('com/google/android/vending/expansion/downloader/DownloaderServiceMarshaller')]
JDownloaderServiceMarshaller = interface(JObject)
['{8F9F546F-191C-4489-8222-7E979F78F41A}']
end;
TJDownloaderServiceMarshaller = class(TJavaGenericImport<JDownloaderServiceMarshallerClass, JDownloaderServiceMarshaller>) end;

JDownloadProgressInfoClass = interface(JObjectClass)
['{881ECB1E-C9CC-4787-9B0D-F57D90261FFD}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Methods}
  function init(p: JParcel): JDownloadProgressInfo; cdecl; overload;
  function init(overallTotal: Int64; overallProgress: Int64; timeRemaining: Int64; currentSpeed: Single): JDownloadProgressInfo; cdecl; overload;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('com/google/android/vending/expansion/downloader/DownloadProgressInfo')]
JDownloadProgressInfo = interface(JObject)
['{3798A8DB-D57D-4516-ABEC-1442E421C3A3}']
  {Property Methods}
  function _GetmCurrentSpeed: Single;
  procedure _SetmCurrentSpeed(Value: Single);
  function _GetmOverallProgress: Int64;
  procedure _SetmOverallProgress(Value: Int64);
  function _GetmOverallTotal: Int64;
  procedure _SetmOverallTotal(Value: Int64);
  function _GetmTimeRemaining: Int64;
  procedure _SetmTimeRemaining(Value: Int64);
  {Methods}
  function describeContents: Integer; cdecl;
  procedure writeToParcel(p: JParcel; i: Integer); cdecl;
  {Properties}
  property mCurrentSpeed: Single read _GetmCurrentSpeed write _SetmCurrentSpeed;
  property mOverallProgress: Int64 read _GetmOverallProgress write _SetmOverallProgress;
  property mOverallTotal: Int64 read _GetmOverallTotal write _SetmOverallTotal;
  property mTimeRemaining: Int64 read _GetmTimeRemaining write _SetmTimeRemaining;
end;
TJDownloadProgressInfo = class(TJavaGenericImport<JDownloadProgressInfoClass, JDownloadProgressInfo>) end;

JDownloadsDB_MetadataColumnsClass = interface(JObjectClass)
['{8DFD2602-E83D-44A1-AA48-1AC95679C976}']
  {Property Methods}
  function _GetAPKVERSION: JString;
  function _GetDOWNLOAD_STATUS: JString;
  function _GetFLAGS: JString;
  function _GetSCHEMA: TJavaObjectBiArray<JString>;
  function _GetTABLE_NAME: JString;
  function _Get_ID: JString;
  {Methods}
  function init: JDownloadsDB_MetadataColumns; cdecl;
  {Properties}
  property APKVERSION: JString read _GetAPKVERSION;
  property DOWNLOAD_STATUS: JString read _GetDOWNLOAD_STATUS;
  property FLAGS: JString read _GetFLAGS;
  property SCHEMA: TJavaObjectBiArray<JString> read _GetSCHEMA;
  property TABLE_NAME: JString read _GetTABLE_NAME;
  property _ID: JString read _Get_ID;
end;

[JavaSignature('com/google/android/vending/expansion/downloader/impl/DownloadsDB$MetadataColumns')]
JDownloadsDB_MetadataColumns = interface(JObject)
['{96D3961A-9DEF-4421-9BA9-0927F877356F}']
end;
TJDownloadsDB_MetadataColumns = class(TJavaGenericImport<JDownloadsDB_MetadataColumnsClass, JDownloadsDB_MetadataColumns>) end;

JHttpDateTimeClass = interface(JObjectClass)
['{E5F4C13B-B479-41BB-AB05-9D6E1881D102}']
  {Methods}
  function init: JHttpDateTime; cdecl;
  function parse(timeString: JString): Int64; cdecl;
end;

[JavaSignature('com/google/android/vending/expansion/downloader/impl/HttpDateTime')]
JHttpDateTime = interface(JObject)
['{686B6AD2-8C07-48F0-AE6F-12278C24A12E}']
end;
TJHttpDateTime = class(TJavaGenericImport<JHttpDateTimeClass, JHttpDateTime>) end;

JDownloadInfoClass = interface(JObjectClass)
['{F3074D64-CC05-491E-BBD6-4222F2AE918D}']
  {Methods}
  function init(index: Integer; fileName: JString; pkg: JString): JDownloadInfo; cdecl;
end;

[JavaSignature('com/google/android/vending/expansion/downloader/impl/DownloadInfo')]
JDownloadInfo = interface(JObject)
['{BE027323-52FF-4D36-8793-F04E2FF47E1E}']
  {Property Methods}
  function _GetmControl: Integer;
  procedure _SetmControl(Value: Integer);
  function _GetmCurrentBytes: Int64;
  procedure _SetmCurrentBytes(Value: Int64);
  function _GetmETag: JString;
  procedure _SetmETag(Value: JString);
  function _GetmFileName: JString;
  function _GetmFuzz: Integer;
  procedure _SetmFuzz(Value: Integer);
  function _GetmIndex: Integer;
  function _GetmLastMod: Int64;
  procedure _SetmLastMod(Value: Int64);
  function _GetmNumFailed: Integer;
  procedure _SetmNumFailed(Value: Integer);
  function _GetmRedirectCount: Integer;
  procedure _SetmRedirectCount(Value: Integer);
  function _GetmRetryAfter: Integer;
  procedure _SetmRetryAfter(Value: Integer);
  function _GetmStatus: Integer;
  procedure _SetmStatus(Value: Integer);
  function _GetmTotalBytes: Int64;
  procedure _SetmTotalBytes(Value: Int64);
  function _GetmUri: JString;
  procedure _SetmUri(Value: JString);
  {Methods}
  procedure logVerboseInfo; cdecl;
  procedure resetDownload; cdecl;
  function restartTime(now: Int64): Int64; cdecl;
  {Properties}
  property mControl: Integer read _GetmControl write _SetmControl;
  property mCurrentBytes: Int64 read _GetmCurrentBytes write _SetmCurrentBytes;
  property mETag: JString read _GetmETag write _SetmETag;
  property mFileName: JString read _GetmFileName;
  property mFuzz: Integer read _GetmFuzz write _SetmFuzz;
  property mIndex: Integer read _GetmIndex;
  property mLastMod: Int64 read _GetmLastMod write _SetmLastMod;
  property mNumFailed: Integer read _GetmNumFailed write _SetmNumFailed;
  property mRedirectCount: Integer read _GetmRedirectCount write _SetmRedirectCount;
  property mRetryAfter: Integer read _GetmRetryAfter write _SetmRetryAfter;
  property mStatus: Integer read _GetmStatus write _SetmStatus;
  property mTotalBytes: Int64 read _GetmTotalBytes write _SetmTotalBytes;
  property mUri: JString read _GetmUri write _SetmUri;
end;
TJDownloadInfo = class(TJavaGenericImport<JDownloadInfoClass, JDownloadInfo>) end;




implementation

begin

end.


