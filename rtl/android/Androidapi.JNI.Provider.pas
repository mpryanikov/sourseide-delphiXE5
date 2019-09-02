{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit Androidapi.JNI.Provider;

interface

uses 
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Webkit,
  Androidapi.JNI.Net,
  Androidapi.JNI.Util,
  Androidapi.JNI.GraphicsContentViewText;

type

  {Class forward declarations}
  JCalendarContract_CalendarCache = interface;//android.provider.CalendarContract$CalendarCache
  JOpenableColumns = interface;//android.provider.OpenableColumns
  JMediaStore_Images = interface;//android.provider.MediaStore$Images
  JCalendarContract_Colors = interface;//android.provider.CalendarContract$Colors
  JArtists_Albums = interface;//android.provider.MediaStore$Audio$Artists$Albums
  JBrowser_SearchColumns = interface;//android.provider.Browser$SearchColumns
  JSettings_NameValueTable = interface;//android.provider.Settings$NameValueTable
  JSettings_Global = interface;//android.provider.Settings$Global
  JContactsContract_Contacts = interface;//android.provider.ContactsContract$Contacts
  JBaseColumns = interface;//android.provider.BaseColumns
  JSyncStateContract_Columns = interface;//android.provider.SyncStateContract$Columns
  JContacts_SettingsColumns = interface;//android.provider.Contacts$SettingsColumns
  JCommonDataKinds_Organization = interface;//android.provider.ContactsContract$CommonDataKinds$Organization
  JStreamItems_StreamItemPhotos = interface;//android.provider.ContactsContract$StreamItems$StreamItemPhotos
  JMediaStore = interface;//android.provider.MediaStore
  JPeople_Phones = interface;//android.provider.Contacts$People$Phones
  JContacts_Organizations = interface;//android.provider.Contacts$Organizations
  JCallLog_Calls = interface;//android.provider.CallLog$Calls
  JContacts_Settings = interface;//android.provider.Contacts$Settings
  JPlaylists_Members = interface;//android.provider.MediaStore$Audio$Playlists$Members
  JSettings_System = interface;//android.provider.Settings$System
  JSettings_Secure = interface;//android.provider.Settings$Secure
  JContactsContract_CommonDataKinds = interface;//android.provider.ContactsContract$CommonDataKinds
  JMediaStore_MediaColumns = interface;//android.provider.MediaStore$MediaColumns
  JCommonDataKinds_StructuredPostal = interface;//android.provider.ContactsContract$CommonDataKinds$StructuredPostal
  JContacts_ContactMethodsColumns = interface;//android.provider.Contacts$ContactMethodsColumns
  JContactsContract_AggregationExceptions = interface;//android.provider.ContactsContract$AggregationExceptions
  JContactsContract_Profile = interface;//android.provider.ContactsContract$Profile
  JCalendarContract_EventDays = interface;//android.provider.CalendarContract$EventDays
  JContactsContract_DisplayNameSources = interface;//android.provider.ContactsContract$DisplayNameSources
  JContactsContract_Directory = interface;//android.provider.ContactsContract$Directory
  JCommonDataKinds_GroupMembership = interface;//android.provider.ContactsContract$CommonDataKinds$GroupMembership
  JRawContacts_Entity = interface;//android.provider.ContactsContract$RawContacts$Entity
  JCalendarContract = interface;//android.provider.CalendarContract
  JContacts_PhonesColumns = interface;//android.provider.Contacts$PhonesColumns
  JSettings = interface;//android.provider.Settings
  JContactsContract_DisplayPhoto = interface;//android.provider.ContactsContract$DisplayPhoto
  JContactsContract_StreamItems = interface;//android.provider.ContactsContract$StreamItems
  JFiles_FileColumns = interface;//android.provider.MediaStore$Files$FileColumns
  JBrowser_BookmarkColumns = interface;//android.provider.Browser$BookmarkColumns
  JCommonDataKinds_Email = interface;//android.provider.ContactsContract$CommonDataKinds$Email
  JCallLog = interface;//android.provider.CallLog
  JContacts_OrganizationColumns = interface;//android.provider.Contacts$OrganizationColumns
  JAudio_GenresColumns = interface;//android.provider.MediaStore$Audio$GenresColumns
  JContacts_PhotosColumns = interface;//android.provider.Contacts$PhotosColumns
  JAudio_Artists = interface;//android.provider.MediaStore$Audio$Artists
  JContactsContract_Intents = interface;//android.provider.ContactsContract$Intents
  JCommonDataKinds_Note = interface;//android.provider.ContactsContract$CommonDataKinds$Note
  JContacts_StreamItems = interface;//android.provider.ContactsContract$Contacts$StreamItems
  JAudio_AudioColumns = interface;//android.provider.MediaStore$Audio$AudioColumns
  JRawContacts_Data = interface;//android.provider.ContactsContract$RawContacts$Data
  JCommonDataKinds_Relation = interface;//android.provider.ContactsContract$CommonDataKinds$Relation
  JAudio_Playlists = interface;//android.provider.MediaStore$Audio$Playlists
  JContacts = interface;//android.provider.Contacts
  JVoicemailContract_Status = interface;//android.provider.VoicemailContract$Status
  JImages_Thumbnails = interface;//android.provider.MediaStore$Images$Thumbnails
  JContacts_PresenceColumns = interface;//android.provider.Contacts$PresenceColumns
  JSearchRecentSuggestions = interface;//android.provider.SearchRecentSuggestions
  JGenres_Members = interface;//android.provider.MediaStore$Audio$Genres$Members
  JPeople_Extensions = interface;//android.provider.Contacts$People$Extensions
  JCalendarContract_Calendars = interface;//android.provider.CalendarContract$Calendars
  JSyncStateContract = interface;//android.provider.SyncStateContract
  JContactsContract_PhoneLookup = interface;//android.provider.ContactsContract$PhoneLookup
  JCommonDataKinds_Im = interface;//android.provider.ContactsContract$CommonDataKinds$Im
  JLiveFolders = interface;//android.provider.LiveFolders
  JCommonDataKinds_Phone = interface;//android.provider.ContactsContract$CommonDataKinds$Phone
  JSyncStateContract_Helpers = interface;//android.provider.SyncStateContract$Helpers
  JContactsContract_PhoneticNameStyle = interface;//android.provider.ContactsContract$PhoneticNameStyle
  JContactsContract_RawContacts = interface;//android.provider.ContactsContract$RawContacts
  JCalendarContract_CalendarEntity = interface;//android.provider.CalendarContract$CalendarEntity
  JVideo_VideoColumns = interface;//android.provider.MediaStore$Video$VideoColumns
  JContactsContract = interface;//android.provider.ContactsContract
  JContacts_Photos = interface;//android.provider.Contacts$Photos
  JContacts_Phones = interface;//android.provider.Contacts$Phones
  JCalendarContract_ExtendedProperties = interface;//android.provider.CalendarContract$ExtendedProperties
  JCommonDataKinds_StructuredName = interface;//android.provider.ContactsContract$CommonDataKinds$StructuredName
  JContactsContract_StreamItemPhotos = interface;//android.provider.ContactsContract$StreamItemPhotos
  JCommonDataKinds_SipAddress = interface;//android.provider.ContactsContract$CommonDataKinds$SipAddress
  JBrowser = interface;//android.provider.Browser
  JCalendarContract_Reminders = interface;//android.provider.CalendarContract$Reminders
  JContactsContract_StatusUpdates = interface;//android.provider.ContactsContract$StatusUpdates
  JContactsContract_FullNameStyle = interface;//android.provider.ContactsContract$FullNameStyle
  JAlarmClock = interface;//android.provider.AlarmClock
  JSettings_SettingNotFoundException = interface;//android.provider.Settings$SettingNotFoundException
  JAudio_Media = interface;//android.provider.MediaStore$Audio$Media
  JContacts_GroupMembership = interface;//android.provider.Contacts$GroupMembership
  JCommonDataKinds_Identity = interface;//android.provider.ContactsContract$CommonDataKinds$Identity
  JAudio_AlbumColumns = interface;//android.provider.MediaStore$Audio$AlbumColumns
  JVoicemailContract = interface;//android.provider.VoicemailContract
  JImages_ImageColumns = interface;//android.provider.MediaStore$Images$ImageColumns
  JCommonDataKinds_BaseTypes = interface;//android.provider.ContactsContract$CommonDataKinds$BaseTypes
  JContactsContract_DataUsageFeedback = interface;//android.provider.ContactsContract$DataUsageFeedback
  JCalendarContract_Events = interface;//android.provider.CalendarContract$Events
  JContactsContract_RawContactsEntity = interface;//android.provider.ContactsContract$RawContactsEntity
  JContacts_ExtensionsColumns = interface;//android.provider.Contacts$ExtensionsColumns
  JAudio_ArtistColumns = interface;//android.provider.MediaStore$Audio$ArtistColumns
  JContactsContract_SyncState = interface;//android.provider.ContactsContract$SyncState
  JMediaStore_Files = interface;//android.provider.MediaStore$Files
  JCalendarContract_Instances = interface;//android.provider.CalendarContract$Instances
  JCalendarContract_EventsEntity = interface;//android.provider.CalendarContract$EventsEntity
  JCalendarContract_Attendees = interface;//android.provider.CalendarContract$Attendees
  JContacts_Intents = interface;//android.provider.Contacts$Intents
  JVideo_Media = interface;//android.provider.MediaStore$Video$Media
  JContacts_People = interface;//android.provider.Contacts$People
  JContactsContract_Data = interface;//android.provider.ContactsContract$Data
  JContacts_AggregationSuggestions = interface;//android.provider.ContactsContract$Contacts$AggregationSuggestions
  JContacts_Photo = interface;//android.provider.ContactsContract$Contacts$Photo
  JSyncStateContract_Constants = interface;//android.provider.SyncStateContract$Constants
  JImages_Media = interface;//android.provider.MediaStore$Images$Media
  JContacts_PeopleColumns = interface;//android.provider.Contacts$PeopleColumns
  JUserDictionary_Words = interface;//android.provider.UserDictionary$Words
  JAudio_Genres = interface;//android.provider.MediaStore$Audio$Genres
  JContactsContract_Settings = interface;//android.provider.ContactsContract$Settings
  JAudio_PlaylistsColumns = interface;//android.provider.MediaStore$Audio$PlaylistsColumns
  JPeople_ContactMethods = interface;//android.provider.Contacts$People$ContactMethods
  JRawContacts_DisplayPhoto = interface;//android.provider.ContactsContract$RawContacts$DisplayPhoto
  JContactsContract_Intents_Insert = interface;//android.provider.ContactsContract$Intents$Insert
  JContacts_Groups = interface;//android.provider.Contacts$Groups
  JAudio_Albums = interface;//android.provider.MediaStore$Audio$Albums
  JCommonDataKinds_Photo = interface;//android.provider.ContactsContract$CommonDataKinds$Photo
  JContacts_ContactMethods = interface;//android.provider.Contacts$ContactMethods
  JContacts_Extensions = interface;//android.provider.Contacts$Extensions
  JCommonDataKinds_Website = interface;//android.provider.ContactsContract$CommonDataKinds$Website
  JMediaStore_Audio = interface;//android.provider.MediaStore$Audio
  JContactsContract_Presence = interface;//android.provider.ContactsContract$Presence
  JVideo_Thumbnails = interface;//android.provider.MediaStore$Video$Thumbnails
  JRawContacts_StreamItems = interface;//android.provider.ContactsContract$RawContacts$StreamItems
  JContacts_Entity = interface;//android.provider.ContactsContract$Contacts$Entity
  JContactsContract_Groups = interface;//android.provider.ContactsContract$Groups
  JCommonDataKinds_Event = interface;//android.provider.ContactsContract$CommonDataKinds$Event
  JCalendarContract_CalendarAlerts = interface;//android.provider.CalendarContract$CalendarAlerts
  JVoicemailContract_Voicemails = interface;//android.provider.VoicemailContract$Voicemails
  JContacts_Data = interface;//android.provider.ContactsContract$Contacts$Data
  JContactsContract_ProfileSyncState = interface;//android.provider.ContactsContract$ProfileSyncState
  JIntents_UI = interface;//android.provider.Contacts$Intents$UI
  JContacts_GroupsColumns = interface;//android.provider.Contacts$GroupsColumns
  JContactsContract_QuickContact = interface;//android.provider.ContactsContract$QuickContact
  JMediaStore_Video = interface;//android.provider.MediaStore$Video
  JIntents_Insert = interface;//android.provider.Contacts$Intents$Insert
  JCalendarContract_SyncState = interface;//android.provider.CalendarContract$SyncState
  JCommonDataKinds_Nickname = interface;//android.provider.ContactsContract$CommonDataKinds$Nickname
  JUserDictionary = interface;//android.provider.UserDictionary

JCalendarContract_CalendarCacheClass = interface(JObjectClass)
['{218F5BAB-B94C-4D5E-BE36-88694940EE2A}']
  {Property Methods}
  function _GetKEY_TIMEZONE_INSTANCES: JString;
  function _GetKEY_TIMEZONE_INSTANCES_PREVIOUS: JString;
  function _GetKEY_TIMEZONE_TYPE: JString;
  function _GetTIMEZONE_TYPE_AUTO: JString;
  function _GetTIMEZONE_TYPE_HOME: JString;
  function _GetURI: Jnet_Uri;
  {Properties}
  property KEY_TIMEZONE_INSTANCES: JString read _GetKEY_TIMEZONE_INSTANCES;
  property KEY_TIMEZONE_INSTANCES_PREVIOUS: JString read _GetKEY_TIMEZONE_INSTANCES_PREVIOUS;
  property KEY_TIMEZONE_TYPE: JString read _GetKEY_TIMEZONE_TYPE;
  property TIMEZONE_TYPE_AUTO: JString read _GetTIMEZONE_TYPE_AUTO;
  property TIMEZONE_TYPE_HOME: JString read _GetTIMEZONE_TYPE_HOME;
  property URI: Jnet_Uri read _GetURI;
end;

[JavaSignature('android/provider/CalendarContract$CalendarCache')]
JCalendarContract_CalendarCache = interface(JObject)
['{17DD3DF3-DD4D-4045-92D3-71A868EE73FF}']
end;
TJCalendarContract_CalendarCache = class(TJavaGenericImport<JCalendarContract_CalendarCacheClass, JCalendarContract_CalendarCache>) end;

JOpenableColumnsClass = interface(IJavaClass)
['{C93AB313-45FA-49F2-9C32-83F4F92A1CF5}']
  {Property Methods}
  function _GetDISPLAY_NAME: JString;
  function _GetSIZE: JString;
  {Properties}
  property DISPLAY_NAME: JString read _GetDISPLAY_NAME;
  property SIZE: JString read _GetSIZE;
end;

[JavaSignature('android/provider/OpenableColumns')]
JOpenableColumns = interface(IJavaInstance)
['{68A240FC-99B9-4153-9CE6-E15D4BD68F80}']
end;
TJOpenableColumns = class(TJavaGenericImport<JOpenableColumnsClass, JOpenableColumns>) end;

JMediaStore_ImagesClass = interface(JObjectClass)
['{F81B9015-5DDD-40B1-A2DD-752301D001BA}']
  {Methods}
  function init: JMediaStore_Images; cdecl;
end;

[JavaSignature('android/provider/MediaStore$Images')]
JMediaStore_Images = interface(JObject)
['{52A5CB82-5EC5-41C2-A32D-DAB61080B566}']
end;
TJMediaStore_Images = class(TJavaGenericImport<JMediaStore_ImagesClass, JMediaStore_Images>) end;

JCalendarContract_ColorsClass = interface(JObjectClass)
['{F160C731-69A5-4284-AF64-C6DF9F89B96B}']
  {Property Methods}
  function _GetCONTENT_URI: Jnet_Uri;
  {Properties}
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
end;

[JavaSignature('android/provider/CalendarContract$Colors')]
JCalendarContract_Colors = interface(JObject)
['{0FD3621A-9D1E-403C-B345-3C47F40F891F}']
end;
TJCalendarContract_Colors = class(TJavaGenericImport<JCalendarContract_ColorsClass, JCalendarContract_Colors>) end;

JArtists_AlbumsClass = interface(JObjectClass)
['{35A10FA0-4A8A-4DF3-A1F0-EE3B0D7495E0}']
  {Methods}
  function init: JArtists_Albums; cdecl;
  function getContentUri(volumeName: JString; artistId: Int64): Jnet_Uri; cdecl;
end;

[JavaSignature('android/provider/MediaStore$Audio$Artists$Albums')]
JArtists_Albums = interface(JObject)
['{2CF80079-CF70-477E-9F0A-2C8DBE6C6023}']
end;
TJArtists_Albums = class(TJavaGenericImport<JArtists_AlbumsClass, JArtists_Albums>) end;

JBrowser_SearchColumnsClass = interface(JObjectClass)
['{E8D6CFF1-C7BC-4DB2-8000-240EF4D598FB}']
  {Property Methods}
  function _GetDATE: JString;
  function _GetSEARCH: JString;
  function _GetURL: JString;
  {Methods}
  function init: JBrowser_SearchColumns; cdecl;
  {Properties}
  property DATE: JString read _GetDATE;
  property SEARCH: JString read _GetSEARCH;
  property URL: JString read _GetURL;
end;

[JavaSignature('android/provider/Browser$SearchColumns')]
JBrowser_SearchColumns = interface(JObject)
['{EE842EBD-4C9B-462B-B1B6-372F6A732032}']
end;
TJBrowser_SearchColumns = class(TJavaGenericImport<JBrowser_SearchColumnsClass, JBrowser_SearchColumns>) end;

JSettings_NameValueTableClass = interface(JObjectClass)
['{38866C77-AF70-4058-818C-0A8DA4654E99}']
  {Property Methods}
  function _GetNAME: JString;
  function _GetVALUE: JString;
  {Methods}
  function init: JSettings_NameValueTable; cdecl;
  function getUriFor(uri: Jnet_Uri; name: JString): Jnet_Uri; cdecl;
  {Properties}
  property NAME: JString read _GetNAME;
  property VALUE: JString read _GetVALUE;
end;

[JavaSignature('android/provider/Settings$NameValueTable')]
JSettings_NameValueTable = interface(JObject)
['{F3A8C46A-507C-4F88-A045-8FE3B5E58945}']
end;
TJSettings_NameValueTable = class(TJavaGenericImport<JSettings_NameValueTableClass, JSettings_NameValueTable>) end;

JSettings_GlobalClass = interface(JSettings_NameValueTableClass)
['{FFCD002F-D78F-46C7-9C25-A8B48EC838E9}']
  {Property Methods}
  function _GetADB_ENABLED: JString;
  function _GetAIRPLANE_MODE_ON: JString;
  function _GetAIRPLANE_MODE_RADIOS: JString;
  function _GetALWAYS_FINISH_ACTIVITIES: JString;
  function _GetANIMATOR_DURATION_SCALE: JString;
  function _GetAUTO_TIME: JString;
  function _GetAUTO_TIME_ZONE: JString;
  function _GetBLUETOOTH_ON: JString;
  function _GetCONTENT_URI: Jnet_Uri;
  function _GetDATA_ROAMING: JString;
  function _GetDEBUG_APP: JString;
  function _GetDEVELOPMENT_SETTINGS_ENABLED: JString;
  function _GetDEVICE_PROVISIONED: JString;
  function _GetHTTP_PROXY: JString;
  function _GetINSTALL_NON_MARKET_APPS: JString;
  function _GetMODE_RINGER: JString;
  function _GetNETWORK_PREFERENCE: JString;
  function _GetRADIO_BLUETOOTH: JString;
  function _GetRADIO_CELL: JString;
  function _GetRADIO_NFC: JString;
  function _GetRADIO_WIFI: JString;
  function _GetSHOW_PROCESSES: JString;
  function _GetSTAY_ON_WHILE_PLUGGED_IN: JString;
  function _GetSYS_PROP_SETTING_VERSION: JString;
  function _GetTRANSITION_ANIMATION_SCALE: JString;
  function _GetUSB_MASS_STORAGE_ENABLED: JString;
  function _GetUSE_GOOGLE_MAIL: JString;
  function _GetWAIT_FOR_DEBUGGER: JString;
  function _GetWIFI_MAX_DHCP_RETRY_COUNT: JString;
  function _GetWIFI_MOBILE_DATA_TRANSITION_WAKELOCK_TIMEOUT_MS: JString;
  function _GetWIFI_NETWORKS_AVAILABLE_NOTIFICATION_ON: JString;
  function _GetWIFI_NETWORKS_AVAILABLE_REPEAT_DELAY: JString;
  function _GetWIFI_NUM_OPEN_NETWORKS_KEPT: JString;
  function _GetWIFI_ON: JString;
  function _GetWIFI_SLEEP_POLICY: JString;
  function _GetWIFI_SLEEP_POLICY_DEFAULT: Integer;
  function _GetWIFI_SLEEP_POLICY_NEVER: Integer;
  function _GetWIFI_SLEEP_POLICY_NEVER_WHILE_PLUGGED: Integer;
  function _GetWIFI_WATCHDOG_ON: JString;
  function _GetWINDOW_ANIMATION_SCALE: JString;
  {Methods}
  function init: JSettings_Global; cdecl;
  function getFloat(cr: JContentResolver; name: JString; def: Single): Single; cdecl; overload;
  function getFloat(cr: JContentResolver; name: JString): Single; cdecl; overload;
  function getInt(cr: JContentResolver; name: JString; def: Integer): Integer; cdecl; overload;
  function getInt(cr: JContentResolver; name: JString): Integer; cdecl; overload;
  function getLong(cr: JContentResolver; name: JString; def: Int64): Int64; cdecl; overload;
  function getLong(cr: JContentResolver; name: JString): Int64; cdecl; overload;
  function getString(resolver: JContentResolver; name: JString): JString; cdecl;
  function getUriFor(name: JString): Jnet_Uri; cdecl;
  function putFloat(cr: JContentResolver; name: JString; value: Single): Boolean; cdecl;
  function putInt(cr: JContentResolver; name: JString; value: Integer): Boolean; cdecl;
  function putLong(cr: JContentResolver; name: JString; value: Int64): Boolean; cdecl;
  function putString(resolver: JContentResolver; name: JString; value: JString): Boolean; cdecl;
  {Properties}
  property ADB_ENABLED: JString read _GetADB_ENABLED;
  property AIRPLANE_MODE_ON: JString read _GetAIRPLANE_MODE_ON;
  property AIRPLANE_MODE_RADIOS: JString read _GetAIRPLANE_MODE_RADIOS;
  property ALWAYS_FINISH_ACTIVITIES: JString read _GetALWAYS_FINISH_ACTIVITIES;
  property ANIMATOR_DURATION_SCALE: JString read _GetANIMATOR_DURATION_SCALE;
  property AUTO_TIME: JString read _GetAUTO_TIME;
  property AUTO_TIME_ZONE: JString read _GetAUTO_TIME_ZONE;
  property BLUETOOTH_ON: JString read _GetBLUETOOTH_ON;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
  property DATA_ROAMING: JString read _GetDATA_ROAMING;
  property DEBUG_APP: JString read _GetDEBUG_APP;
  property DEVELOPMENT_SETTINGS_ENABLED: JString read _GetDEVELOPMENT_SETTINGS_ENABLED;
  property DEVICE_PROVISIONED: JString read _GetDEVICE_PROVISIONED;
  property HTTP_PROXY: JString read _GetHTTP_PROXY;
  property INSTALL_NON_MARKET_APPS: JString read _GetINSTALL_NON_MARKET_APPS;
  property MODE_RINGER: JString read _GetMODE_RINGER;
  property NETWORK_PREFERENCE: JString read _GetNETWORK_PREFERENCE;
  property RADIO_BLUETOOTH: JString read _GetRADIO_BLUETOOTH;
  property RADIO_CELL: JString read _GetRADIO_CELL;
  property RADIO_NFC: JString read _GetRADIO_NFC;
  property RADIO_WIFI: JString read _GetRADIO_WIFI;
  property SHOW_PROCESSES: JString read _GetSHOW_PROCESSES;
  property STAY_ON_WHILE_PLUGGED_IN: JString read _GetSTAY_ON_WHILE_PLUGGED_IN;
  property SYS_PROP_SETTING_VERSION: JString read _GetSYS_PROP_SETTING_VERSION;
  property TRANSITION_ANIMATION_SCALE: JString read _GetTRANSITION_ANIMATION_SCALE;
  property USB_MASS_STORAGE_ENABLED: JString read _GetUSB_MASS_STORAGE_ENABLED;
  property USE_GOOGLE_MAIL: JString read _GetUSE_GOOGLE_MAIL;
  property WAIT_FOR_DEBUGGER: JString read _GetWAIT_FOR_DEBUGGER;
  property WIFI_MAX_DHCP_RETRY_COUNT: JString read _GetWIFI_MAX_DHCP_RETRY_COUNT;
  property WIFI_MOBILE_DATA_TRANSITION_WAKELOCK_TIMEOUT_MS: JString read _GetWIFI_MOBILE_DATA_TRANSITION_WAKELOCK_TIMEOUT_MS;
  property WIFI_NETWORKS_AVAILABLE_NOTIFICATION_ON: JString read _GetWIFI_NETWORKS_AVAILABLE_NOTIFICATION_ON;
  property WIFI_NETWORKS_AVAILABLE_REPEAT_DELAY: JString read _GetWIFI_NETWORKS_AVAILABLE_REPEAT_DELAY;
  property WIFI_NUM_OPEN_NETWORKS_KEPT: JString read _GetWIFI_NUM_OPEN_NETWORKS_KEPT;
  property WIFI_ON: JString read _GetWIFI_ON;
  property WIFI_SLEEP_POLICY: JString read _GetWIFI_SLEEP_POLICY;
  property WIFI_SLEEP_POLICY_DEFAULT: Integer read _GetWIFI_SLEEP_POLICY_DEFAULT;
  property WIFI_SLEEP_POLICY_NEVER: Integer read _GetWIFI_SLEEP_POLICY_NEVER;
  property WIFI_SLEEP_POLICY_NEVER_WHILE_PLUGGED: Integer read _GetWIFI_SLEEP_POLICY_NEVER_WHILE_PLUGGED;
  property WIFI_WATCHDOG_ON: JString read _GetWIFI_WATCHDOG_ON;
  property WINDOW_ANIMATION_SCALE: JString read _GetWINDOW_ANIMATION_SCALE;
end;

[JavaSignature('android/provider/Settings$Global')]
JSettings_Global = interface(JSettings_NameValueTable)
['{BE959C5F-1913-4DE9-ADF7-FDC2C040553E}']
end;
TJSettings_Global = class(TJavaGenericImport<JSettings_GlobalClass, JSettings_Global>) end;

JContactsContract_ContactsClass = interface(JObjectClass)
['{3BA68A03-57B0-426D-B452-93635322BBDE}']
  {Property Methods}
  function _GetCONTENT_FILTER_URI: Jnet_Uri;
  function _GetCONTENT_GROUP_URI: Jnet_Uri;
  function _GetCONTENT_ITEM_TYPE: JString;
  function _GetCONTENT_LOOKUP_URI: Jnet_Uri;
  function _GetCONTENT_STREQUENT_FILTER_URI: Jnet_Uri;
  function _GetCONTENT_STREQUENT_URI: Jnet_Uri;
  function _GetCONTENT_TYPE: JString;
  function _GetCONTENT_URI: Jnet_Uri;
  function _GetCONTENT_VCARD_TYPE: JString;
  function _GetCONTENT_VCARD_URI: Jnet_Uri;
  {Methods}
  function getLookupUri(resolver: JContentResolver; contactUri: Jnet_Uri): Jnet_Uri; cdecl; overload;
  function getLookupUri(contactId: Int64; lookupKey: JString): Jnet_Uri; cdecl; overload;
  function lookupContact(resolver: JContentResolver; lookupUri: Jnet_Uri): Jnet_Uri; cdecl;
  procedure markAsContacted(resolver: JContentResolver; contactId: Int64); cdecl;//Deprecated
  function openContactPhotoInputStream(cr: JContentResolver; contactUri: Jnet_Uri; preferHighres: Boolean): JInputStream; cdecl; overload;
  function openContactPhotoInputStream(cr: JContentResolver; contactUri: Jnet_Uri): JInputStream; cdecl; overload;
  {Properties}
  property CONTENT_FILTER_URI: Jnet_Uri read _GetCONTENT_FILTER_URI;
  property CONTENT_GROUP_URI: Jnet_Uri read _GetCONTENT_GROUP_URI;
  property CONTENT_ITEM_TYPE: JString read _GetCONTENT_ITEM_TYPE;
  property CONTENT_LOOKUP_URI: Jnet_Uri read _GetCONTENT_LOOKUP_URI;
  property CONTENT_STREQUENT_FILTER_URI: Jnet_Uri read _GetCONTENT_STREQUENT_FILTER_URI;
  property CONTENT_STREQUENT_URI: Jnet_Uri read _GetCONTENT_STREQUENT_URI;
  property CONTENT_TYPE: JString read _GetCONTENT_TYPE;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
  property CONTENT_VCARD_TYPE: JString read _GetCONTENT_VCARD_TYPE;
  property CONTENT_VCARD_URI: Jnet_Uri read _GetCONTENT_VCARD_URI;
end;

[JavaSignature('android/provider/ContactsContract$Contacts')]
JContactsContract_Contacts = interface(JObject)
['{F174D654-2BBC-4854-BB3A-23F403CE38D8}']
end;
TJContactsContract_Contacts = class(TJavaGenericImport<JContactsContract_ContactsClass, JContactsContract_Contacts>) end;

JBaseColumnsClass = interface(IJavaClass)
['{41940D1F-2151-4E46-BC9E-038A03919A4B}']
  {Property Methods}
  function _Get_COUNT: JString;
  function _Get_ID: JString;
  {Properties}
  property _COUNT: JString read _Get_COUNT;
  property _ID: JString read _Get_ID;
end;

[JavaSignature('android/provider/BaseColumns')]
JBaseColumns = interface(IJavaInstance)
['{3F8E1C71-B6F5-4A75-A44C-4F8B0968D57B}']
end;
TJBaseColumns = class(TJavaGenericImport<JBaseColumnsClass, JBaseColumns>) end;

JSyncStateContract_ColumnsClass = interface(JBaseColumnsClass)
['{2A1A3A43-15BD-40BF-8CAE-D86529C38491}']
  {Property Methods}
  function _GetACCOUNT_NAME: JString;
  function _GetACCOUNT_TYPE: JString;
  function _GetDATA: JString;
  {Properties}
  property ACCOUNT_NAME: JString read _GetACCOUNT_NAME;
  property ACCOUNT_TYPE: JString read _GetACCOUNT_TYPE;
  property DATA: JString read _GetDATA;
end;

[JavaSignature('android/provider/SyncStateContract$Columns')]
JSyncStateContract_Columns = interface(JBaseColumns)
['{62738508-F2E0-4EE2-89E2-57F10B1D5869}']
end;
TJSyncStateContract_Columns = class(TJavaGenericImport<JSyncStateContract_ColumnsClass, JSyncStateContract_Columns>) end;

JContacts_SettingsColumnsClass = interface(IJavaClass)
['{94EAFAE0-EE2B-495C-847A-4CE7F4828115}']
  {Property Methods}
  function _GetKEY: JString;
  function _GetVALUE: JString;
  function _Get_SYNC_ACCOUNT: JString;
  function _Get_SYNC_ACCOUNT_TYPE: JString;
  {Properties}
  property KEY: JString read _GetKEY;
  property VALUE: JString read _GetVALUE;
  property _SYNC_ACCOUNT: JString read _Get_SYNC_ACCOUNT;
  property _SYNC_ACCOUNT_TYPE: JString read _Get_SYNC_ACCOUNT_TYPE;
end;

[JavaSignature('android/provider/Contacts$SettingsColumns')]
JContacts_SettingsColumns = interface(IJavaInstance)
['{E6D7EC94-D966-471A-BADB-FCC9C25645D2}']
end;
TJContacts_SettingsColumns = class(TJavaGenericImport<JContacts_SettingsColumnsClass, JContacts_SettingsColumns>) end;

JCommonDataKinds_OrganizationClass = interface(JObjectClass)
['{FFBF559E-7463-43F7-86DA-A2D66F987D92}']
  {Property Methods}
  function _GetCOMPANY: JString;
  function _GetCONTENT_ITEM_TYPE: JString;
  function _GetDEPARTMENT: JString;
  function _GetJOB_DESCRIPTION: JString;
  function _GetOFFICE_LOCATION: JString;
  function _GetPHONETIC_NAME: JString;
  function _GetSYMBOL: JString;
  function _GetTITLE: JString;
  function _GetTYPE_OTHER: Integer;
  function _GetTYPE_WORK: Integer;
  {Methods}
  function getTypeLabel(res: JResources; type_: Integer; label_: JCharSequence): JCharSequence; cdecl;
  function getTypeLabelResource(type_: Integer): Integer; cdecl;
  {Properties}
  property COMPANY: JString read _GetCOMPANY;
  property CONTENT_ITEM_TYPE: JString read _GetCONTENT_ITEM_TYPE;
  property DEPARTMENT: JString read _GetDEPARTMENT;
  property JOB_DESCRIPTION: JString read _GetJOB_DESCRIPTION;
  property OFFICE_LOCATION: JString read _GetOFFICE_LOCATION;
  property PHONETIC_NAME: JString read _GetPHONETIC_NAME;
  property SYMBOL: JString read _GetSYMBOL;
  property TITLE: JString read _GetTITLE;
  property TYPE_OTHER: Integer read _GetTYPE_OTHER;
  property TYPE_WORK: Integer read _GetTYPE_WORK;
end;

[JavaSignature('android/provider/ContactsContract$CommonDataKinds$Organization')]
JCommonDataKinds_Organization = interface(JObject)
['{64CCF643-55DA-4D5E-89BC-45B6B9581FEE}']
end;
TJCommonDataKinds_Organization = class(TJavaGenericImport<JCommonDataKinds_OrganizationClass, JCommonDataKinds_Organization>) end;

JStreamItems_StreamItemPhotosClass = interface(JObjectClass)
['{DDBF87F4-419E-4FAA-9B21-4C6649003589}']
  {Property Methods}
  function _GetCONTENT_DIRECTORY: JString;
  function _GetCONTENT_ITEM_TYPE: JString;
  function _GetCONTENT_TYPE: JString;
  {Properties}
  property CONTENT_DIRECTORY: JString read _GetCONTENT_DIRECTORY;
  property CONTENT_ITEM_TYPE: JString read _GetCONTENT_ITEM_TYPE;
  property CONTENT_TYPE: JString read _GetCONTENT_TYPE;
end;

[JavaSignature('android/provider/ContactsContract$StreamItems$StreamItemPhotos')]
JStreamItems_StreamItemPhotos = interface(JObject)
['{DA9164CD-79AD-44BC-9326-16FD67568177}']
end;
TJStreamItems_StreamItemPhotos = class(TJavaGenericImport<JStreamItems_StreamItemPhotosClass, JStreamItems_StreamItemPhotos>) end;

JMediaStoreClass = interface(JObjectClass)
['{278F3F27-3ECC-494B-8425-1D29F602F5A5}']
  {Property Methods}
  function _GetACTION_IMAGE_CAPTURE: JString;
  function _GetACTION_IMAGE_CAPTURE_SECURE: JString;
  function _GetACTION_VIDEO_CAPTURE: JString;
  function _GetAUTHORITY: JString;
  function _GetEXTRA_DURATION_LIMIT: JString;
  function _GetEXTRA_FINISH_ON_COMPLETION: JString;
  function _GetEXTRA_FULL_SCREEN: JString;
  function _GetEXTRA_MEDIA_ALBUM: JString;
  function _GetEXTRA_MEDIA_ARTIST: JString;
  function _GetEXTRA_MEDIA_FOCUS: JString;
  function _GetEXTRA_MEDIA_TITLE: JString;
  function _GetEXTRA_OUTPUT: JString;
  function _GetEXTRA_SCREEN_ORIENTATION: JString;
  function _GetEXTRA_SHOW_ACTION_ICONS: JString;
  function _GetEXTRA_SIZE_LIMIT: JString;
  function _GetEXTRA_VIDEO_QUALITY: JString;
  function _GetINTENT_ACTION_MEDIA_PLAY_FROM_SEARCH: JString;
  function _GetINTENT_ACTION_MEDIA_SEARCH: JString;
  function _GetINTENT_ACTION_MUSIC_PLAYER: JString;
  function _GetINTENT_ACTION_STILL_IMAGE_CAMERA: JString;
  function _GetINTENT_ACTION_STILL_IMAGE_CAMERA_SECURE: JString;
  function _GetINTENT_ACTION_TEXT_OPEN_FROM_SEARCH: JString;
  function _GetINTENT_ACTION_VIDEO_CAMERA: JString;
  function _GetINTENT_ACTION_VIDEO_PLAY_FROM_SEARCH: JString;
  function _GetMEDIA_IGNORE_FILENAME: JString;
  function _GetMEDIA_SCANNER_VOLUME: JString;
  function _GetUNKNOWN_STRING: JString;
  {Methods}
  function init: JMediaStore; cdecl;
  function getMediaScannerUri: Jnet_Uri; cdecl;
  function getVersion(context: JContext): JString; cdecl;
  {Properties}
  property ACTION_IMAGE_CAPTURE: JString read _GetACTION_IMAGE_CAPTURE;
  property ACTION_IMAGE_CAPTURE_SECURE: JString read _GetACTION_IMAGE_CAPTURE_SECURE;
  property ACTION_VIDEO_CAPTURE: JString read _GetACTION_VIDEO_CAPTURE;
  property AUTHORITY: JString read _GetAUTHORITY;
  property EXTRA_DURATION_LIMIT: JString read _GetEXTRA_DURATION_LIMIT;
  property EXTRA_FINISH_ON_COMPLETION: JString read _GetEXTRA_FINISH_ON_COMPLETION;
  property EXTRA_FULL_SCREEN: JString read _GetEXTRA_FULL_SCREEN;
  property EXTRA_MEDIA_ALBUM: JString read _GetEXTRA_MEDIA_ALBUM;
  property EXTRA_MEDIA_ARTIST: JString read _GetEXTRA_MEDIA_ARTIST;
  property EXTRA_MEDIA_FOCUS: JString read _GetEXTRA_MEDIA_FOCUS;
  property EXTRA_MEDIA_TITLE: JString read _GetEXTRA_MEDIA_TITLE;
  property EXTRA_OUTPUT: JString read _GetEXTRA_OUTPUT;
  property EXTRA_SCREEN_ORIENTATION: JString read _GetEXTRA_SCREEN_ORIENTATION;
  property EXTRA_SHOW_ACTION_ICONS: JString read _GetEXTRA_SHOW_ACTION_ICONS;
  property EXTRA_SIZE_LIMIT: JString read _GetEXTRA_SIZE_LIMIT;
  property EXTRA_VIDEO_QUALITY: JString read _GetEXTRA_VIDEO_QUALITY;
  property INTENT_ACTION_MEDIA_PLAY_FROM_SEARCH: JString read _GetINTENT_ACTION_MEDIA_PLAY_FROM_SEARCH;
  property INTENT_ACTION_MEDIA_SEARCH: JString read _GetINTENT_ACTION_MEDIA_SEARCH;
  property INTENT_ACTION_MUSIC_PLAYER: JString read _GetINTENT_ACTION_MUSIC_PLAYER;
  property INTENT_ACTION_STILL_IMAGE_CAMERA: JString read _GetINTENT_ACTION_STILL_IMAGE_CAMERA;
  property INTENT_ACTION_STILL_IMAGE_CAMERA_SECURE: JString read _GetINTENT_ACTION_STILL_IMAGE_CAMERA_SECURE;
  property INTENT_ACTION_TEXT_OPEN_FROM_SEARCH: JString read _GetINTENT_ACTION_TEXT_OPEN_FROM_SEARCH;
  property INTENT_ACTION_VIDEO_CAMERA: JString read _GetINTENT_ACTION_VIDEO_CAMERA;
  property INTENT_ACTION_VIDEO_PLAY_FROM_SEARCH: JString read _GetINTENT_ACTION_VIDEO_PLAY_FROM_SEARCH;
  property MEDIA_IGNORE_FILENAME: JString read _GetMEDIA_IGNORE_FILENAME;
  property MEDIA_SCANNER_VOLUME: JString read _GetMEDIA_SCANNER_VOLUME;
  property UNKNOWN_STRING: JString read _GetUNKNOWN_STRING;
end;

[JavaSignature('android/provider/MediaStore')]
JMediaStore = interface(JObject)
['{0E08BF0A-0312-4F84-85B7-261A5AABA092}']
end;
TJMediaStore = class(TJavaGenericImport<JMediaStoreClass, JMediaStore>) end;

JPeople_PhonesClass = interface(JObjectClass)
['{8D325165-B643-414C-A87C-AB58A8CE3E43}']
  {Property Methods}
  function _GetCONTENT_DIRECTORY: JString;
  function _GetDEFAULT_SORT_ORDER: JString;
  {Properties}
  property CONTENT_DIRECTORY: JString read _GetCONTENT_DIRECTORY;
  property DEFAULT_SORT_ORDER: JString read _GetDEFAULT_SORT_ORDER;
end;

[JavaSignature('android/provider/Contacts$People$Phones')]
JPeople_Phones = interface(JObject)
['{8E1EBD27-A95D-4FA9-B210-D575BB44806D}']
end;
TJPeople_Phones = class(TJavaGenericImport<JPeople_PhonesClass, JPeople_Phones>) end;

JContacts_OrganizationsClass = interface(JObjectClass)
['{8359A187-9035-4B6E-A86B-5322FAD849DF}']
  {Property Methods}
  function _GetCONTENT_DIRECTORY: JString;
  function _GetCONTENT_URI: Jnet_Uri;
  function _GetDEFAULT_SORT_ORDER: JString;
  {Methods}
  function getDisplayLabel(context: JContext; type_: Integer; label_: JCharSequence): JCharSequence; cdecl;//Deprecated
  {Properties}
  property CONTENT_DIRECTORY: JString read _GetCONTENT_DIRECTORY;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
  property DEFAULT_SORT_ORDER: JString read _GetDEFAULT_SORT_ORDER;
end;

[JavaSignature('android/provider/Contacts$Organizations')]
JContacts_Organizations = interface(JObject)
['{08123B12-1675-456F-91E2-41D83C19724D}']
end;
TJContacts_Organizations = class(TJavaGenericImport<JContacts_OrganizationsClass, JContacts_Organizations>) end;

JCallLog_CallsClass = interface(JObjectClass)
['{C1F91074-4530-4BD5-A988-EED3E9684F7F}']
  {Property Methods}
  function _GetCACHED_NAME: JString;
  function _GetCACHED_NUMBER_LABEL: JString;
  function _GetCACHED_NUMBER_TYPE: JString;
  function _GetCONTENT_FILTER_URI: Jnet_Uri;
  function _GetCONTENT_ITEM_TYPE: JString;
  function _GetCONTENT_TYPE: JString;
  function _GetCONTENT_URI: Jnet_Uri;
  function _GetDATE: JString;
  function _GetDEFAULT_SORT_ORDER: JString;
  function _GetDURATION: JString;
  function _GetINCOMING_TYPE: Integer;
  function _GetIS_READ: JString;
  function _GetLIMIT_PARAM_KEY: JString;
  function _GetMISSED_TYPE: Integer;
  function _GetNEW: JString;
  function _GetNUMBER: JString;
  function _GetOFFSET_PARAM_KEY: JString;
  function _GetOUTGOING_TYPE: Integer;
  function _GetTYPE: JString;
  {Methods}
  function init: JCallLog_Calls; cdecl;
  function getLastOutgoingCall(context: JContext): JString; cdecl;
  {Properties}
  property CACHED_NAME: JString read _GetCACHED_NAME;
  property CACHED_NUMBER_LABEL: JString read _GetCACHED_NUMBER_LABEL;
  property CACHED_NUMBER_TYPE: JString read _GetCACHED_NUMBER_TYPE;
  property CONTENT_FILTER_URI: Jnet_Uri read _GetCONTENT_FILTER_URI;
  property CONTENT_ITEM_TYPE: JString read _GetCONTENT_ITEM_TYPE;
  property CONTENT_TYPE: JString read _GetCONTENT_TYPE;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
  property DATE: JString read _GetDATE;
  property DEFAULT_SORT_ORDER: JString read _GetDEFAULT_SORT_ORDER;
  property DURATION: JString read _GetDURATION;
  property INCOMING_TYPE: Integer read _GetINCOMING_TYPE;
  property IS_READ: JString read _GetIS_READ;
  property LIMIT_PARAM_KEY: JString read _GetLIMIT_PARAM_KEY;
  property MISSED_TYPE: Integer read _GetMISSED_TYPE;
  property NEW: JString read _GetNEW;
  property NUMBER: JString read _GetNUMBER;
  property OFFSET_PARAM_KEY: JString read _GetOFFSET_PARAM_KEY;
  property OUTGOING_TYPE: Integer read _GetOUTGOING_TYPE;
  property &TYPE: JString read _GetTYPE;
end;

[JavaSignature('android/provider/CallLog$Calls')]
JCallLog_Calls = interface(JObject)
['{A7EE4C05-1CD7-4AFB-94F2-EF23965CCBFA}']
end;
TJCallLog_Calls = class(TJavaGenericImport<JCallLog_CallsClass, JCallLog_Calls>) end;

JContacts_SettingsClass = interface(JObjectClass)
['{F5925633-0876-4493-B0D3-24F02B6A2DCC}']
  {Property Methods}
  function _GetCONTENT_DIRECTORY: JString;
  function _GetCONTENT_URI: Jnet_Uri;
  function _GetDEFAULT_SORT_ORDER: JString;
  function _GetSYNC_EVERYTHING: JString;
  {Methods}
  function getSetting(cr: JContentResolver; account: JString; key: JString): JString; cdecl;//Deprecated
  procedure setSetting(cr: JContentResolver; account: JString; key: JString; value: JString); cdecl;//Deprecated
  {Properties}
  property CONTENT_DIRECTORY: JString read _GetCONTENT_DIRECTORY;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
  property DEFAULT_SORT_ORDER: JString read _GetDEFAULT_SORT_ORDER;
  property SYNC_EVERYTHING: JString read _GetSYNC_EVERYTHING;
end;

[JavaSignature('android/provider/Contacts$Settings')]
JContacts_Settings = interface(JObject)
['{D0B1ABA3-0E95-4148-912F-10AE6B3B81DA}']
end;
TJContacts_Settings = class(TJavaGenericImport<JContacts_SettingsClass, JContacts_Settings>) end;

JPlaylists_MembersClass = interface(JObjectClass)
['{AF0599FB-D445-4BA5-9016-8A255805AA8E}']
  {Property Methods}
  function _GetAUDIO_ID: JString;
  function _GetCONTENT_DIRECTORY: JString;
  function _GetDEFAULT_SORT_ORDER: JString;
  function _GetPLAYLIST_ID: JString;
  function _GetPLAY_ORDER: JString;
  function _Get_ID: JString;
  {Methods}
  function init: JPlaylists_Members; cdecl;
  function getContentUri(volumeName: JString; playlistId: Int64): Jnet_Uri; cdecl;
  function moveItem(res: JContentResolver; playlistId: Int64; from: Integer; to_: Integer): Boolean; cdecl;
  {Properties}
  property AUDIO_ID: JString read _GetAUDIO_ID;
  property CONTENT_DIRECTORY: JString read _GetCONTENT_DIRECTORY;
  property DEFAULT_SORT_ORDER: JString read _GetDEFAULT_SORT_ORDER;
  property PLAYLIST_ID: JString read _GetPLAYLIST_ID;
  property PLAY_ORDER: JString read _GetPLAY_ORDER;
  property _ID: JString read _Get_ID;
end;

[JavaSignature('android/provider/MediaStore$Audio$Playlists$Members')]
JPlaylists_Members = interface(JObject)
['{704AE7AB-DEB5-4015-B94D-D7A3A14CB5FA}']
end;
TJPlaylists_Members = class(TJavaGenericImport<JPlaylists_MembersClass, JPlaylists_Members>) end;

JSettings_SystemClass = interface(JSettings_NameValueTableClass)
['{E632F660-1FE5-4669-9255-ED47ECAE3731}']
  {Property Methods}
  function _GetACCELEROMETER_ROTATION: JString;
  function _GetADB_ENABLED: JString;
  function _GetAIRPLANE_MODE_ON: JString;
  function _GetAIRPLANE_MODE_RADIOS: JString;
  function _GetALARM_ALERT: JString;
  function _GetALWAYS_FINISH_ACTIVITIES: JString;
  function _GetANDROID_ID: JString;
  function _GetANIMATOR_DURATION_SCALE: JString;
  function _GetAPPEND_FOR_LAST_AUDIBLE: JString;
  function _GetAUTO_TIME: JString;
  function _GetAUTO_TIME_ZONE: JString;
  function _GetBLUETOOTH_DISCOVERABILITY: JString;
  function _GetBLUETOOTH_DISCOVERABILITY_TIMEOUT: JString;
  function _GetBLUETOOTH_ON: JString;
  function _GetCONTENT_URI: Jnet_Uri;
  function _GetDATA_ROAMING: JString;
  function _GetDATE_FORMAT: JString;
  function _GetDEBUG_APP: JString;
  function _GetDEFAULT_ALARM_ALERT_URI: Jnet_Uri;
  function _GetDEFAULT_NOTIFICATION_URI: Jnet_Uri;
  function _GetDEFAULT_RINGTONE_URI: Jnet_Uri;
  function _GetDEVICE_PROVISIONED: JString;
  function _GetDIM_SCREEN: JString;
  function _GetDTMF_TONE_WHEN_DIALING: JString;
  function _GetEND_BUTTON_BEHAVIOR: JString;
  function _GetFONT_SCALE: JString;
  function _GetHAPTIC_FEEDBACK_ENABLED: JString;
  function _GetHTTP_PROXY: JString;
  function _GetINSTALL_NON_MARKET_APPS: JString;
  function _GetLOCATION_PROVIDERS_ALLOWED: JString;
  function _GetLOCK_PATTERN_ENABLED: JString;
  function _GetLOCK_PATTERN_TACTILE_FEEDBACK_ENABLED: JString;
  function _GetLOCK_PATTERN_VISIBLE: JString;
  function _GetLOGGING_ID: JString;
  function _GetMODE_RINGER: JString;
  function _GetMODE_RINGER_STREAMS_AFFECTED: JString;
  function _GetMUTE_STREAMS_AFFECTED: JString;
  function _GetNETWORK_PREFERENCE: JString;
  function _GetNEXT_ALARM_FORMATTED: JString;
  function _GetNOTIFICATION_SOUND: JString;
  function _GetPARENTAL_CONTROL_ENABLED: JString;
  function _GetPARENTAL_CONTROL_LAST_UPDATE: JString;
  function _GetPARENTAL_CONTROL_REDIRECT_URL: JString;
  function _GetRADIO_BLUETOOTH: JString;
  function _GetRADIO_CELL: JString;
  function _GetRADIO_NFC: JString;
  function _GetRADIO_WIFI: JString;
  function _GetRINGTONE: JString;
  function _GetSCREEN_BRIGHTNESS: JString;
  function _GetSCREEN_BRIGHTNESS_MODE: JString;
  function _GetSCREEN_BRIGHTNESS_MODE_AUTOMATIC: Integer;
  function _GetSCREEN_BRIGHTNESS_MODE_MANUAL: Integer;
  function _GetSCREEN_OFF_TIMEOUT: JString;
  function _GetSETTINGS_CLASSNAME: JString;
  function _GetSETUP_WIZARD_HAS_RUN: JString;
  function _GetSHOW_GTALK_SERVICE_STATUS: JString;
  function _GetSHOW_PROCESSES: JString;
  function _GetSHOW_WEB_SUGGESTIONS: JString;
  function _GetSOUND_EFFECTS_ENABLED: JString;
  function _GetSTAY_ON_WHILE_PLUGGED_IN: JString;
  function _GetSYS_PROP_SETTING_VERSION: JString;
  function _GetTEXT_AUTO_CAPS: JString;
  function _GetTEXT_AUTO_PUNCTUATE: JString;
  function _GetTEXT_AUTO_REPLACE: JString;
  function _GetTEXT_SHOW_PASSWORD: JString;
  function _GetTIME_12_24: JString;
  function _GetTRANSITION_ANIMATION_SCALE: JString;
  function _GetUSB_MASS_STORAGE_ENABLED: JString;
  function _GetUSER_ROTATION: JString;
  function _GetUSE_GOOGLE_MAIL: JString;
  function _GetVIBRATE_ON: JString;
  function _GetVOLUME_ALARM: JString;
  function _GetVOLUME_BLUETOOTH_SCO: JString;
  function _GetVOLUME_MUSIC: JString;
  function _GetVOLUME_NOTIFICATION: JString;
  function _GetVOLUME_RING: JString;
  function _GetVOLUME_SETTINGS: TJavaObjectArray<JString>;
  function _GetVOLUME_SYSTEM: JString;
  function _GetVOLUME_VOICE: JString;
  function _GetWAIT_FOR_DEBUGGER: JString;
  function _GetWALLPAPER_ACTIVITY: JString;
  function _GetWIFI_MAX_DHCP_RETRY_COUNT: JString;
  function _GetWIFI_MOBILE_DATA_TRANSITION_WAKELOCK_TIMEOUT_MS: JString;
  function _GetWIFI_NETWORKS_AVAILABLE_NOTIFICATION_ON: JString;
  function _GetWIFI_NETWORKS_AVAILABLE_REPEAT_DELAY: JString;
  function _GetWIFI_NUM_OPEN_NETWORKS_KEPT: JString;
  function _GetWIFI_ON: JString;
  function _GetWIFI_SLEEP_POLICY: JString;
  function _GetWIFI_SLEEP_POLICY_DEFAULT: Integer;
  function _GetWIFI_SLEEP_POLICY_NEVER: Integer;
  function _GetWIFI_SLEEP_POLICY_NEVER_WHILE_PLUGGED: Integer;
  function _GetWIFI_STATIC_DNS1: JString;
  function _GetWIFI_STATIC_DNS2: JString;
  function _GetWIFI_STATIC_GATEWAY: JString;
  function _GetWIFI_STATIC_IP: JString;
  function _GetWIFI_STATIC_NETMASK: JString;
  function _GetWIFI_USE_STATIC_IP: JString;
  function _GetWIFI_WATCHDOG_ACCEPTABLE_PACKET_LOSS_PERCENTAGE: JString;
  function _GetWIFI_WATCHDOG_AP_COUNT: JString;
  function _GetWIFI_WATCHDOG_BACKGROUND_CHECK_DELAY_MS: JString;
  function _GetWIFI_WATCHDOG_BACKGROUND_CHECK_ENABLED: JString;
  function _GetWIFI_WATCHDOG_BACKGROUND_CHECK_TIMEOUT_MS: JString;
  function _GetWIFI_WATCHDOG_INITIAL_IGNORED_PING_COUNT: JString;
  function _GetWIFI_WATCHDOG_MAX_AP_CHECKS: JString;
  function _GetWIFI_WATCHDOG_ON: JString;
  function _GetWIFI_WATCHDOG_PING_COUNT: JString;
  function _GetWIFI_WATCHDOG_PING_DELAY_MS: JString;
  function _GetWIFI_WATCHDOG_PING_TIMEOUT_MS: JString;
  function _GetWINDOW_ANIMATION_SCALE: JString;
  {Methods}
  function init: JSettings_System; cdecl;
  procedure getConfiguration(cr: JContentResolver; outConfig: JConfiguration); cdecl;
  function getFloat(cr: JContentResolver; name: JString; def: Single): Single; cdecl; overload;
  function getFloat(cr: JContentResolver; name: JString): Single; cdecl; overload;
  function getInt(cr: JContentResolver; name: JString; def: Integer): Integer; cdecl; overload;
  function getInt(cr: JContentResolver; name: JString): Integer; cdecl; overload;
  function getLong(cr: JContentResolver; name: JString; def: Int64): Int64; cdecl; overload;
  function getLong(cr: JContentResolver; name: JString): Int64; cdecl; overload;
  function getShowGTalkServiceStatus(cr: JContentResolver): Boolean; cdecl;//Deprecated
  function getString(resolver: JContentResolver; name: JString): JString; cdecl;
  function getUriFor(name: JString): Jnet_Uri; cdecl;
  function putConfiguration(cr: JContentResolver; config: JConfiguration): Boolean; cdecl;
  function putFloat(cr: JContentResolver; name: JString; value: Single): Boolean; cdecl;
  function putInt(cr: JContentResolver; name: JString; value: Integer): Boolean; cdecl;
  function putLong(cr: JContentResolver; name: JString; value: Int64): Boolean; cdecl;
  function putString(resolver: JContentResolver; name: JString; value: JString): Boolean; cdecl;
  procedure setShowGTalkServiceStatus(cr: JContentResolver; flag: Boolean); cdecl;//Deprecated
  {Properties}
  property ACCELEROMETER_ROTATION: JString read _GetACCELEROMETER_ROTATION;
  property ADB_ENABLED: JString read _GetADB_ENABLED;
  property AIRPLANE_MODE_ON: JString read _GetAIRPLANE_MODE_ON;
  property AIRPLANE_MODE_RADIOS: JString read _GetAIRPLANE_MODE_RADIOS;
  property ALARM_ALERT: JString read _GetALARM_ALERT;
  property ALWAYS_FINISH_ACTIVITIES: JString read _GetALWAYS_FINISH_ACTIVITIES;
  property ANDROID_ID: JString read _GetANDROID_ID;
  property ANIMATOR_DURATION_SCALE: JString read _GetANIMATOR_DURATION_SCALE;
  property APPEND_FOR_LAST_AUDIBLE: JString read _GetAPPEND_FOR_LAST_AUDIBLE;
  property AUTO_TIME: JString read _GetAUTO_TIME;
  property AUTO_TIME_ZONE: JString read _GetAUTO_TIME_ZONE;
  property BLUETOOTH_DISCOVERABILITY: JString read _GetBLUETOOTH_DISCOVERABILITY;
  property BLUETOOTH_DISCOVERABILITY_TIMEOUT: JString read _GetBLUETOOTH_DISCOVERABILITY_TIMEOUT;
  property BLUETOOTH_ON: JString read _GetBLUETOOTH_ON;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
  property DATA_ROAMING: JString read _GetDATA_ROAMING;
  property DATE_FORMAT: JString read _GetDATE_FORMAT;
  property DEBUG_APP: JString read _GetDEBUG_APP;
  property DEFAULT_ALARM_ALERT_URI: Jnet_Uri read _GetDEFAULT_ALARM_ALERT_URI;
  property DEFAULT_NOTIFICATION_URI: Jnet_Uri read _GetDEFAULT_NOTIFICATION_URI;
  property DEFAULT_RINGTONE_URI: Jnet_Uri read _GetDEFAULT_RINGTONE_URI;
  property DEVICE_PROVISIONED: JString read _GetDEVICE_PROVISIONED;
  property DIM_SCREEN: JString read _GetDIM_SCREEN;
  property DTMF_TONE_WHEN_DIALING: JString read _GetDTMF_TONE_WHEN_DIALING;
  property END_BUTTON_BEHAVIOR: JString read _GetEND_BUTTON_BEHAVIOR;
  property FONT_SCALE: JString read _GetFONT_SCALE;
  property HAPTIC_FEEDBACK_ENABLED: JString read _GetHAPTIC_FEEDBACK_ENABLED;
  property HTTP_PROXY: JString read _GetHTTP_PROXY;
  property INSTALL_NON_MARKET_APPS: JString read _GetINSTALL_NON_MARKET_APPS;
  property LOCATION_PROVIDERS_ALLOWED: JString read _GetLOCATION_PROVIDERS_ALLOWED;
  property LOCK_PATTERN_ENABLED: JString read _GetLOCK_PATTERN_ENABLED;
  property LOCK_PATTERN_TACTILE_FEEDBACK_ENABLED: JString read _GetLOCK_PATTERN_TACTILE_FEEDBACK_ENABLED;
  property LOCK_PATTERN_VISIBLE: JString read _GetLOCK_PATTERN_VISIBLE;
  property LOGGING_ID: JString read _GetLOGGING_ID;
  property MODE_RINGER: JString read _GetMODE_RINGER;
  property MODE_RINGER_STREAMS_AFFECTED: JString read _GetMODE_RINGER_STREAMS_AFFECTED;
  property MUTE_STREAMS_AFFECTED: JString read _GetMUTE_STREAMS_AFFECTED;
  property NETWORK_PREFERENCE: JString read _GetNETWORK_PREFERENCE;
  property NEXT_ALARM_FORMATTED: JString read _GetNEXT_ALARM_FORMATTED;
  property NOTIFICATION_SOUND: JString read _GetNOTIFICATION_SOUND;
  property PARENTAL_CONTROL_ENABLED: JString read _GetPARENTAL_CONTROL_ENABLED;
  property PARENTAL_CONTROL_LAST_UPDATE: JString read _GetPARENTAL_CONTROL_LAST_UPDATE;
  property PARENTAL_CONTROL_REDIRECT_URL: JString read _GetPARENTAL_CONTROL_REDIRECT_URL;
  property RADIO_BLUETOOTH: JString read _GetRADIO_BLUETOOTH;
  property RADIO_CELL: JString read _GetRADIO_CELL;
  property RADIO_NFC: JString read _GetRADIO_NFC;
  property RADIO_WIFI: JString read _GetRADIO_WIFI;
  property RINGTONE: JString read _GetRINGTONE;
  property SCREEN_BRIGHTNESS: JString read _GetSCREEN_BRIGHTNESS;
  property SCREEN_BRIGHTNESS_MODE: JString read _GetSCREEN_BRIGHTNESS_MODE;
  property SCREEN_BRIGHTNESS_MODE_AUTOMATIC: Integer read _GetSCREEN_BRIGHTNESS_MODE_AUTOMATIC;
  property SCREEN_BRIGHTNESS_MODE_MANUAL: Integer read _GetSCREEN_BRIGHTNESS_MODE_MANUAL;
  property SCREEN_OFF_TIMEOUT: JString read _GetSCREEN_OFF_TIMEOUT;
  property SETTINGS_CLASSNAME: JString read _GetSETTINGS_CLASSNAME;
  property SETUP_WIZARD_HAS_RUN: JString read _GetSETUP_WIZARD_HAS_RUN;
  property SHOW_GTALK_SERVICE_STATUS: JString read _GetSHOW_GTALK_SERVICE_STATUS;
  property SHOW_PROCESSES: JString read _GetSHOW_PROCESSES;
  property SHOW_WEB_SUGGESTIONS: JString read _GetSHOW_WEB_SUGGESTIONS;
  property SOUND_EFFECTS_ENABLED: JString read _GetSOUND_EFFECTS_ENABLED;
  property STAY_ON_WHILE_PLUGGED_IN: JString read _GetSTAY_ON_WHILE_PLUGGED_IN;
  property SYS_PROP_SETTING_VERSION: JString read _GetSYS_PROP_SETTING_VERSION;
  property TEXT_AUTO_CAPS: JString read _GetTEXT_AUTO_CAPS;
  property TEXT_AUTO_PUNCTUATE: JString read _GetTEXT_AUTO_PUNCTUATE;
  property TEXT_AUTO_REPLACE: JString read _GetTEXT_AUTO_REPLACE;
  property TEXT_SHOW_PASSWORD: JString read _GetTEXT_SHOW_PASSWORD;
  property TIME_12_24: JString read _GetTIME_12_24;
  property TRANSITION_ANIMATION_SCALE: JString read _GetTRANSITION_ANIMATION_SCALE;
  property USB_MASS_STORAGE_ENABLED: JString read _GetUSB_MASS_STORAGE_ENABLED;
  property USER_ROTATION: JString read _GetUSER_ROTATION;
  property USE_GOOGLE_MAIL: JString read _GetUSE_GOOGLE_MAIL;
  property VIBRATE_ON: JString read _GetVIBRATE_ON;
  property VOLUME_ALARM: JString read _GetVOLUME_ALARM;
  property VOLUME_BLUETOOTH_SCO: JString read _GetVOLUME_BLUETOOTH_SCO;
  property VOLUME_MUSIC: JString read _GetVOLUME_MUSIC;
  property VOLUME_NOTIFICATION: JString read _GetVOLUME_NOTIFICATION;
  property VOLUME_RING: JString read _GetVOLUME_RING;
  property VOLUME_SETTINGS: TJavaObjectArray<JString> read _GetVOLUME_SETTINGS;
  property VOLUME_SYSTEM: JString read _GetVOLUME_SYSTEM;
  property VOLUME_VOICE: JString read _GetVOLUME_VOICE;
  property WAIT_FOR_DEBUGGER: JString read _GetWAIT_FOR_DEBUGGER;
  property WALLPAPER_ACTIVITY: JString read _GetWALLPAPER_ACTIVITY;
  property WIFI_MAX_DHCP_RETRY_COUNT: JString read _GetWIFI_MAX_DHCP_RETRY_COUNT;
  property WIFI_MOBILE_DATA_TRANSITION_WAKELOCK_TIMEOUT_MS: JString read _GetWIFI_MOBILE_DATA_TRANSITION_WAKELOCK_TIMEOUT_MS;
  property WIFI_NETWORKS_AVAILABLE_NOTIFICATION_ON: JString read _GetWIFI_NETWORKS_AVAILABLE_NOTIFICATION_ON;
  property WIFI_NETWORKS_AVAILABLE_REPEAT_DELAY: JString read _GetWIFI_NETWORKS_AVAILABLE_REPEAT_DELAY;
  property WIFI_NUM_OPEN_NETWORKS_KEPT: JString read _GetWIFI_NUM_OPEN_NETWORKS_KEPT;
  property WIFI_ON: JString read _GetWIFI_ON;
  property WIFI_SLEEP_POLICY: JString read _GetWIFI_SLEEP_POLICY;
  property WIFI_SLEEP_POLICY_DEFAULT: Integer read _GetWIFI_SLEEP_POLICY_DEFAULT;
  property WIFI_SLEEP_POLICY_NEVER: Integer read _GetWIFI_SLEEP_POLICY_NEVER;
  property WIFI_SLEEP_POLICY_NEVER_WHILE_PLUGGED: Integer read _GetWIFI_SLEEP_POLICY_NEVER_WHILE_PLUGGED;
  property WIFI_STATIC_DNS1: JString read _GetWIFI_STATIC_DNS1;
  property WIFI_STATIC_DNS2: JString read _GetWIFI_STATIC_DNS2;
  property WIFI_STATIC_GATEWAY: JString read _GetWIFI_STATIC_GATEWAY;
  property WIFI_STATIC_IP: JString read _GetWIFI_STATIC_IP;
  property WIFI_STATIC_NETMASK: JString read _GetWIFI_STATIC_NETMASK;
  property WIFI_USE_STATIC_IP: JString read _GetWIFI_USE_STATIC_IP;
  property WIFI_WATCHDOG_ACCEPTABLE_PACKET_LOSS_PERCENTAGE: JString read _GetWIFI_WATCHDOG_ACCEPTABLE_PACKET_LOSS_PERCENTAGE;
  property WIFI_WATCHDOG_AP_COUNT: JString read _GetWIFI_WATCHDOG_AP_COUNT;
  property WIFI_WATCHDOG_BACKGROUND_CHECK_DELAY_MS: JString read _GetWIFI_WATCHDOG_BACKGROUND_CHECK_DELAY_MS;
  property WIFI_WATCHDOG_BACKGROUND_CHECK_ENABLED: JString read _GetWIFI_WATCHDOG_BACKGROUND_CHECK_ENABLED;
  property WIFI_WATCHDOG_BACKGROUND_CHECK_TIMEOUT_MS: JString read _GetWIFI_WATCHDOG_BACKGROUND_CHECK_TIMEOUT_MS;
  property WIFI_WATCHDOG_INITIAL_IGNORED_PING_COUNT: JString read _GetWIFI_WATCHDOG_INITIAL_IGNORED_PING_COUNT;
  property WIFI_WATCHDOG_MAX_AP_CHECKS: JString read _GetWIFI_WATCHDOG_MAX_AP_CHECKS;
  property WIFI_WATCHDOG_ON: JString read _GetWIFI_WATCHDOG_ON;
  property WIFI_WATCHDOG_PING_COUNT: JString read _GetWIFI_WATCHDOG_PING_COUNT;
  property WIFI_WATCHDOG_PING_DELAY_MS: JString read _GetWIFI_WATCHDOG_PING_DELAY_MS;
  property WIFI_WATCHDOG_PING_TIMEOUT_MS: JString read _GetWIFI_WATCHDOG_PING_TIMEOUT_MS;
  property WINDOW_ANIMATION_SCALE: JString read _GetWINDOW_ANIMATION_SCALE;
end;

[JavaSignature('android/provider/Settings$System')]
JSettings_System = interface(JSettings_NameValueTable)
['{E306E2DB-D461-4827-9D0D-E21DF13BB926}']
end;
TJSettings_System = class(TJavaGenericImport<JSettings_SystemClass, JSettings_System>) end;

JSettings_SecureClass = interface(JSettings_NameValueTableClass)
['{242F3E98-E010-462C-B6DE-78C89954898F}']
  {Property Methods}
  function _GetACCESSIBILITY_ENABLED: JString;
  function _GetACCESSIBILITY_SPEAK_PASSWORD: JString;
  function _GetADB_ENABLED: JString;
  function _GetALLOWED_GEOLOCATION_ORIGINS: JString;
  function _GetALLOW_MOCK_LOCATION: JString;
  function _GetANDROID_ID: JString;
  function _GetBACKGROUND_DATA: JString;
  function _GetBLUETOOTH_ON: JString;
  function _GetCONTENT_URI: Jnet_Uri;
  function _GetDATA_ROAMING: JString;
  function _GetDEFAULT_INPUT_METHOD: JString;
  function _GetDEVELOPMENT_SETTINGS_ENABLED: JString;
  function _GetDEVICE_PROVISIONED: JString;
  function _GetENABLED_ACCESSIBILITY_SERVICES: JString;
  function _GetENABLED_INPUT_METHODS: JString;
  function _GetHTTP_PROXY: JString;
  function _GetINPUT_METHOD_SELECTOR_VISIBILITY: JString;
  function _GetINSTALL_NON_MARKET_APPS: JString;
  function _GetLOCATION_PROVIDERS_ALLOWED: JString;
  function _GetLOCK_PATTERN_ENABLED: JString;
  function _GetLOCK_PATTERN_TACTILE_FEEDBACK_ENABLED: JString;
  function _GetLOCK_PATTERN_VISIBLE: JString;
  function _GetLOGGING_ID: JString;
  function _GetNETWORK_PREFERENCE: JString;
  function _GetPARENTAL_CONTROL_ENABLED: JString;
  function _GetPARENTAL_CONTROL_LAST_UPDATE: JString;
  function _GetPARENTAL_CONTROL_REDIRECT_URL: JString;
  function _GetSELECTED_INPUT_METHOD_SUBTYPE: JString;
  function _GetSETTINGS_CLASSNAME: JString;
  function _GetSYS_PROP_SETTING_VERSION: JString;
  function _GetTOUCH_EXPLORATION_ENABLED: JString;
  function _GetTTS_DEFAULT_COUNTRY: JString;
  function _GetTTS_DEFAULT_LANG: JString;
  function _GetTTS_DEFAULT_PITCH: JString;
  function _GetTTS_DEFAULT_RATE: JString;
  function _GetTTS_DEFAULT_SYNTH: JString;
  function _GetTTS_DEFAULT_VARIANT: JString;
  function _GetTTS_ENABLED_PLUGINS: JString;
  function _GetTTS_USE_DEFAULTS: JString;
  function _GetUSB_MASS_STORAGE_ENABLED: JString;
  function _GetUSE_GOOGLE_MAIL: JString;
  function _GetWIFI_MAX_DHCP_RETRY_COUNT: JString;
  function _GetWIFI_MOBILE_DATA_TRANSITION_WAKELOCK_TIMEOUT_MS: JString;
  function _GetWIFI_NETWORKS_AVAILABLE_NOTIFICATION_ON: JString;
  function _GetWIFI_NETWORKS_AVAILABLE_REPEAT_DELAY: JString;
  function _GetWIFI_NUM_OPEN_NETWORKS_KEPT: JString;
  function _GetWIFI_ON: JString;
  function _GetWIFI_WATCHDOG_ACCEPTABLE_PACKET_LOSS_PERCENTAGE: JString;
  function _GetWIFI_WATCHDOG_AP_COUNT: JString;
  function _GetWIFI_WATCHDOG_BACKGROUND_CHECK_DELAY_MS: JString;
  function _GetWIFI_WATCHDOG_BACKGROUND_CHECK_ENABLED: JString;
  function _GetWIFI_WATCHDOG_BACKGROUND_CHECK_TIMEOUT_MS: JString;
  function _GetWIFI_WATCHDOG_INITIAL_IGNORED_PING_COUNT: JString;
  function _GetWIFI_WATCHDOG_MAX_AP_CHECKS: JString;
  function _GetWIFI_WATCHDOG_ON: JString;
  function _GetWIFI_WATCHDOG_PING_COUNT: JString;
  function _GetWIFI_WATCHDOG_PING_DELAY_MS: JString;
  function _GetWIFI_WATCHDOG_PING_TIMEOUT_MS: JString;
  function _GetWIFI_WATCHDOG_WATCH_LIST: JString;
  {Methods}
  function init: JSettings_Secure; cdecl;
  function getFloat(cr: JContentResolver; name: JString; def: Single): Single; cdecl; overload;
  function getFloat(cr: JContentResolver; name: JString): Single; cdecl; overload;
  function getInt(cr: JContentResolver; name: JString; def: Integer): Integer; cdecl; overload;
  function getInt(cr: JContentResolver; name: JString): Integer; cdecl; overload;
  function getLong(cr: JContentResolver; name: JString; def: Int64): Int64; cdecl; overload;
  function getLong(cr: JContentResolver; name: JString): Int64; cdecl; overload;
  function getString(resolver: JContentResolver; name: JString): JString; cdecl;
  function getUriFor(name: JString): Jnet_Uri; cdecl;
  function isLocationProviderEnabled(cr: JContentResolver; provider: JString): Boolean; cdecl;
  function putFloat(cr: JContentResolver; name: JString; value: Single): Boolean; cdecl;
  function putInt(cr: JContentResolver; name: JString; value: Integer): Boolean; cdecl;
  function putLong(cr: JContentResolver; name: JString; value: Int64): Boolean; cdecl;
  function putString(resolver: JContentResolver; name: JString; value: JString): Boolean; cdecl;
  procedure setLocationProviderEnabled(cr: JContentResolver; provider: JString; enabled: Boolean); cdecl;
  {Properties}
  property ACCESSIBILITY_ENABLED: JString read _GetACCESSIBILITY_ENABLED;
  property ACCESSIBILITY_SPEAK_PASSWORD: JString read _GetACCESSIBILITY_SPEAK_PASSWORD;
  property ADB_ENABLED: JString read _GetADB_ENABLED;
  property ALLOWED_GEOLOCATION_ORIGINS: JString read _GetALLOWED_GEOLOCATION_ORIGINS;
  property ALLOW_MOCK_LOCATION: JString read _GetALLOW_MOCK_LOCATION;
  property ANDROID_ID: JString read _GetANDROID_ID;
  property BACKGROUND_DATA: JString read _GetBACKGROUND_DATA;
  property BLUETOOTH_ON: JString read _GetBLUETOOTH_ON;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
  property DATA_ROAMING: JString read _GetDATA_ROAMING;
  property DEFAULT_INPUT_METHOD: JString read _GetDEFAULT_INPUT_METHOD;
  property DEVELOPMENT_SETTINGS_ENABLED: JString read _GetDEVELOPMENT_SETTINGS_ENABLED;
  property DEVICE_PROVISIONED: JString read _GetDEVICE_PROVISIONED;
  property ENABLED_ACCESSIBILITY_SERVICES: JString read _GetENABLED_ACCESSIBILITY_SERVICES;
  property ENABLED_INPUT_METHODS: JString read _GetENABLED_INPUT_METHODS;
  property HTTP_PROXY: JString read _GetHTTP_PROXY;
  property INPUT_METHOD_SELECTOR_VISIBILITY: JString read _GetINPUT_METHOD_SELECTOR_VISIBILITY;
  property INSTALL_NON_MARKET_APPS: JString read _GetINSTALL_NON_MARKET_APPS;
  property LOCATION_PROVIDERS_ALLOWED: JString read _GetLOCATION_PROVIDERS_ALLOWED;
  property LOCK_PATTERN_ENABLED: JString read _GetLOCK_PATTERN_ENABLED;
  property LOCK_PATTERN_TACTILE_FEEDBACK_ENABLED: JString read _GetLOCK_PATTERN_TACTILE_FEEDBACK_ENABLED;
  property LOCK_PATTERN_VISIBLE: JString read _GetLOCK_PATTERN_VISIBLE;
  property LOGGING_ID: JString read _GetLOGGING_ID;
  property NETWORK_PREFERENCE: JString read _GetNETWORK_PREFERENCE;
  property PARENTAL_CONTROL_ENABLED: JString read _GetPARENTAL_CONTROL_ENABLED;
  property PARENTAL_CONTROL_LAST_UPDATE: JString read _GetPARENTAL_CONTROL_LAST_UPDATE;
  property PARENTAL_CONTROL_REDIRECT_URL: JString read _GetPARENTAL_CONTROL_REDIRECT_URL;
  property SELECTED_INPUT_METHOD_SUBTYPE: JString read _GetSELECTED_INPUT_METHOD_SUBTYPE;
  property SETTINGS_CLASSNAME: JString read _GetSETTINGS_CLASSNAME;
  property SYS_PROP_SETTING_VERSION: JString read _GetSYS_PROP_SETTING_VERSION;
  property TOUCH_EXPLORATION_ENABLED: JString read _GetTOUCH_EXPLORATION_ENABLED;
  property TTS_DEFAULT_COUNTRY: JString read _GetTTS_DEFAULT_COUNTRY;
  property TTS_DEFAULT_LANG: JString read _GetTTS_DEFAULT_LANG;
  property TTS_DEFAULT_PITCH: JString read _GetTTS_DEFAULT_PITCH;
  property TTS_DEFAULT_RATE: JString read _GetTTS_DEFAULT_RATE;
  property TTS_DEFAULT_SYNTH: JString read _GetTTS_DEFAULT_SYNTH;
  property TTS_DEFAULT_VARIANT: JString read _GetTTS_DEFAULT_VARIANT;
  property TTS_ENABLED_PLUGINS: JString read _GetTTS_ENABLED_PLUGINS;
  property TTS_USE_DEFAULTS: JString read _GetTTS_USE_DEFAULTS;
  property USB_MASS_STORAGE_ENABLED: JString read _GetUSB_MASS_STORAGE_ENABLED;
  property USE_GOOGLE_MAIL: JString read _GetUSE_GOOGLE_MAIL;
  property WIFI_MAX_DHCP_RETRY_COUNT: JString read _GetWIFI_MAX_DHCP_RETRY_COUNT;
  property WIFI_MOBILE_DATA_TRANSITION_WAKELOCK_TIMEOUT_MS: JString read _GetWIFI_MOBILE_DATA_TRANSITION_WAKELOCK_TIMEOUT_MS;
  property WIFI_NETWORKS_AVAILABLE_NOTIFICATION_ON: JString read _GetWIFI_NETWORKS_AVAILABLE_NOTIFICATION_ON;
  property WIFI_NETWORKS_AVAILABLE_REPEAT_DELAY: JString read _GetWIFI_NETWORKS_AVAILABLE_REPEAT_DELAY;
  property WIFI_NUM_OPEN_NETWORKS_KEPT: JString read _GetWIFI_NUM_OPEN_NETWORKS_KEPT;
  property WIFI_ON: JString read _GetWIFI_ON;
  property WIFI_WATCHDOG_ACCEPTABLE_PACKET_LOSS_PERCENTAGE: JString read _GetWIFI_WATCHDOG_ACCEPTABLE_PACKET_LOSS_PERCENTAGE;
  property WIFI_WATCHDOG_AP_COUNT: JString read _GetWIFI_WATCHDOG_AP_COUNT;
  property WIFI_WATCHDOG_BACKGROUND_CHECK_DELAY_MS: JString read _GetWIFI_WATCHDOG_BACKGROUND_CHECK_DELAY_MS;
  property WIFI_WATCHDOG_BACKGROUND_CHECK_ENABLED: JString read _GetWIFI_WATCHDOG_BACKGROUND_CHECK_ENABLED;
  property WIFI_WATCHDOG_BACKGROUND_CHECK_TIMEOUT_MS: JString read _GetWIFI_WATCHDOG_BACKGROUND_CHECK_TIMEOUT_MS;
  property WIFI_WATCHDOG_INITIAL_IGNORED_PING_COUNT: JString read _GetWIFI_WATCHDOG_INITIAL_IGNORED_PING_COUNT;
  property WIFI_WATCHDOG_MAX_AP_CHECKS: JString read _GetWIFI_WATCHDOG_MAX_AP_CHECKS;
  property WIFI_WATCHDOG_ON: JString read _GetWIFI_WATCHDOG_ON;
  property WIFI_WATCHDOG_PING_COUNT: JString read _GetWIFI_WATCHDOG_PING_COUNT;
  property WIFI_WATCHDOG_PING_DELAY_MS: JString read _GetWIFI_WATCHDOG_PING_DELAY_MS;
  property WIFI_WATCHDOG_PING_TIMEOUT_MS: JString read _GetWIFI_WATCHDOG_PING_TIMEOUT_MS;
  property WIFI_WATCHDOG_WATCH_LIST: JString read _GetWIFI_WATCHDOG_WATCH_LIST;
end;

[JavaSignature('android/provider/Settings$Secure')]
JSettings_Secure = interface(JSettings_NameValueTable)
['{2DB590FD-770A-45A0-8923-84883FF0B03B}']
end;
TJSettings_Secure = class(TJavaGenericImport<JSettings_SecureClass, JSettings_Secure>) end;

JContactsContract_CommonDataKindsClass = interface(JObjectClass)
['{3B30D827-3181-426D-AB34-2E7F3BE8630C}']
end;

[JavaSignature('android/provider/ContactsContract$CommonDataKinds')]
JContactsContract_CommonDataKinds = interface(JObject)
['{563ED7E9-395F-4B5E-9653-57B10CA41BFB}']
end;
TJContactsContract_CommonDataKinds = class(TJavaGenericImport<JContactsContract_CommonDataKindsClass, JContactsContract_CommonDataKinds>) end;

JMediaStore_MediaColumnsClass = interface(JBaseColumnsClass)
['{E1FD16C8-BECB-4C8D-B895-2133F7F1C341}']
  {Property Methods}
  function _GetDATA: JString;
  function _GetDATE_ADDED: JString;
  function _GetDATE_MODIFIED: JString;
  function _GetDISPLAY_NAME: JString;
  function _GetHEIGHT: JString;
  function _GetMIME_TYPE: JString;
  function _GetSIZE: JString;
  function _GetTITLE: JString;
  function _GetWIDTH: JString;
  {Properties}
  property DATA: JString read _GetDATA;
  property DATE_ADDED: JString read _GetDATE_ADDED;
  property DATE_MODIFIED: JString read _GetDATE_MODIFIED;
  property DISPLAY_NAME: JString read _GetDISPLAY_NAME;
  property HEIGHT: JString read _GetHEIGHT;
  property MIME_TYPE: JString read _GetMIME_TYPE;
  property SIZE: JString read _GetSIZE;
  property TITLE: JString read _GetTITLE;
  property WIDTH: JString read _GetWIDTH;
end;

[JavaSignature('android/provider/MediaStore$MediaColumns')]
JMediaStore_MediaColumns = interface(JBaseColumns)
['{464D6D3C-96B3-46A3-9328-4D46DEDF10F4}']
end;
TJMediaStore_MediaColumns = class(TJavaGenericImport<JMediaStore_MediaColumnsClass, JMediaStore_MediaColumns>) end;

JCommonDataKinds_StructuredPostalClass = interface(JObjectClass)
['{B3118CAC-F54A-4BAF-9B70-46C347F1B282}']
  {Property Methods}
  function _GetCITY: JString;
  function _GetCONTENT_ITEM_TYPE: JString;
  function _GetCONTENT_TYPE: JString;
  function _GetCONTENT_URI: Jnet_Uri;
  function _GetCOUNTRY: JString;
  function _GetFORMATTED_ADDRESS: JString;
  function _GetNEIGHBORHOOD: JString;
  function _GetPOBOX: JString;
  function _GetPOSTCODE: JString;
  function _GetREGION: JString;
  function _GetSTREET: JString;
  function _GetTYPE_HOME: Integer;
  function _GetTYPE_OTHER: Integer;
  function _GetTYPE_WORK: Integer;
  {Methods}
  function getTypeLabel(res: JResources; type_: Integer; label_: JCharSequence): JCharSequence; cdecl;
  function getTypeLabelResource(type_: Integer): Integer; cdecl;
  {Properties}
  property CITY: JString read _GetCITY;
  property CONTENT_ITEM_TYPE: JString read _GetCONTENT_ITEM_TYPE;
  property CONTENT_TYPE: JString read _GetCONTENT_TYPE;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
  property COUNTRY: JString read _GetCOUNTRY;
  property FORMATTED_ADDRESS: JString read _GetFORMATTED_ADDRESS;
  property NEIGHBORHOOD: JString read _GetNEIGHBORHOOD;
  property POBOX: JString read _GetPOBOX;
  property POSTCODE: JString read _GetPOSTCODE;
  property REGION: JString read _GetREGION;
  property STREET: JString read _GetSTREET;
  property TYPE_HOME: Integer read _GetTYPE_HOME;
  property TYPE_OTHER: Integer read _GetTYPE_OTHER;
  property TYPE_WORK: Integer read _GetTYPE_WORK;
end;

[JavaSignature('android/provider/ContactsContract$CommonDataKinds$StructuredPostal')]
JCommonDataKinds_StructuredPostal = interface(JObject)
['{8A5E45CA-E950-489E-98D9-79C21BB9C9AA}']
end;
TJCommonDataKinds_StructuredPostal = class(TJavaGenericImport<JCommonDataKinds_StructuredPostalClass, JCommonDataKinds_StructuredPostal>) end;

JContacts_ContactMethodsColumnsClass = interface(IJavaClass)
['{A3629EC7-5EA7-4E30-9912-9BE3B3116CAB}']
  {Property Methods}
  function _GetAUX_DATA: JString;
  function _GetDATA: JString;
  function _GetISPRIMARY: JString;
  function _GetKIND: JString;
  function _GetLABEL: JString;
  function _GetTYPE: JString;
  function _GetTYPE_CUSTOM: Integer;
  function _GetTYPE_HOME: Integer;
  function _GetTYPE_OTHER: Integer;
  function _GetTYPE_WORK: Integer;
  {Properties}
  property AUX_DATA: JString read _GetAUX_DATA;
  property DATA: JString read _GetDATA;
  property ISPRIMARY: JString read _GetISPRIMARY;
  property KIND: JString read _GetKIND;
  property &LABEL: JString read _GetLABEL;
  property &TYPE: JString read _GetTYPE;
  property TYPE_CUSTOM: Integer read _GetTYPE_CUSTOM;
  property TYPE_HOME: Integer read _GetTYPE_HOME;
  property TYPE_OTHER: Integer read _GetTYPE_OTHER;
  property TYPE_WORK: Integer read _GetTYPE_WORK;
end;

[JavaSignature('android/provider/Contacts$ContactMethodsColumns')]
JContacts_ContactMethodsColumns = interface(IJavaInstance)
['{4E8D661C-6E9F-4A55-A794-E8BEFF607E5C}']
end;
TJContacts_ContactMethodsColumns = class(TJavaGenericImport<JContacts_ContactMethodsColumnsClass, JContacts_ContactMethodsColumns>) end;

JContactsContract_AggregationExceptionsClass = interface(JObjectClass)
['{235B7BA0-F0C0-4283-9BC4-22ED73583C0C}']
  {Property Methods}
  function _GetCONTENT_ITEM_TYPE: JString;
  function _GetCONTENT_TYPE: JString;
  function _GetCONTENT_URI: Jnet_Uri;
  function _GetRAW_CONTACT_ID1: JString;
  function _GetRAW_CONTACT_ID2: JString;
  function _GetTYPE: JString;
  function _GetTYPE_AUTOMATIC: Integer;
  function _GetTYPE_KEEP_SEPARATE: Integer;
  function _GetTYPE_KEEP_TOGETHER: Integer;
  {Properties}
  property CONTENT_ITEM_TYPE: JString read _GetCONTENT_ITEM_TYPE;
  property CONTENT_TYPE: JString read _GetCONTENT_TYPE;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
  property RAW_CONTACT_ID1: JString read _GetRAW_CONTACT_ID1;
  property RAW_CONTACT_ID2: JString read _GetRAW_CONTACT_ID2;
  property &TYPE: JString read _GetTYPE;
  property TYPE_AUTOMATIC: Integer read _GetTYPE_AUTOMATIC;
  property TYPE_KEEP_SEPARATE: Integer read _GetTYPE_KEEP_SEPARATE;
  property TYPE_KEEP_TOGETHER: Integer read _GetTYPE_KEEP_TOGETHER;
end;

[JavaSignature('android/provider/ContactsContract$AggregationExceptions')]
JContactsContract_AggregationExceptions = interface(JObject)
['{4B9D218A-5020-456E-81AD-44960BF43B6A}']
end;
TJContactsContract_AggregationExceptions = class(TJavaGenericImport<JContactsContract_AggregationExceptionsClass, JContactsContract_AggregationExceptions>) end;

JContactsContract_ProfileClass = interface(JObjectClass)
['{6792B81E-AC9B-463B-BB59-168BA466F12B}']
  {Property Methods}
  function _GetCONTENT_RAW_CONTACTS_URI: Jnet_Uri;
  function _GetCONTENT_URI: Jnet_Uri;
  function _GetCONTENT_VCARD_URI: Jnet_Uri;
  function _GetMIN_ID: Int64;
  {Properties}
  property CONTENT_RAW_CONTACTS_URI: Jnet_Uri read _GetCONTENT_RAW_CONTACTS_URI;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
  property CONTENT_VCARD_URI: Jnet_Uri read _GetCONTENT_VCARD_URI;
  property MIN_ID: Int64 read _GetMIN_ID;
end;

[JavaSignature('android/provider/ContactsContract$Profile')]
JContactsContract_Profile = interface(JObject)
['{14F9921F-5E34-49FB-8932-21264E5C0385}']
end;
TJContactsContract_Profile = class(TJavaGenericImport<JContactsContract_ProfileClass, JContactsContract_Profile>) end;

JCalendarContract_EventDaysClass = interface(JObjectClass)
['{CC23AE6F-BF06-44AA-A288-A1C7FBA55E52}']
  {Property Methods}
  function _GetCONTENT_URI: Jnet_Uri;
  {Methods}
  function query(cr: JContentResolver; startDay: Integer; numDays: Integer; projection: TJavaObjectArray<JString>): JCursor; cdecl;
  {Properties}
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
end;

[JavaSignature('android/provider/CalendarContract$EventDays')]
JCalendarContract_EventDays = interface(JObject)
['{FC84FBAD-5A8A-4DB1-AC93-9FF52E71A775}']
end;
TJCalendarContract_EventDays = class(TJavaGenericImport<JCalendarContract_EventDaysClass, JCalendarContract_EventDays>) end;

JContactsContract_DisplayNameSourcesClass = interface(IJavaClass)
['{4DB9FB24-F2D2-4E1B-859D-2928196E6FD1}']
  {Property Methods}
  function _GetEMAIL: Integer;
  function _GetNICKNAME: Integer;
  function _GetORGANIZATION: Integer;
  function _GetPHONE: Integer;
  function _GetSTRUCTURED_NAME: Integer;
  function _GetUNDEFINED: Integer;
  {Properties}
  property EMAIL: Integer read _GetEMAIL;
  property NICKNAME: Integer read _GetNICKNAME;
  property ORGANIZATION: Integer read _GetORGANIZATION;
  property PHONE: Integer read _GetPHONE;
  property STRUCTURED_NAME: Integer read _GetSTRUCTURED_NAME;
  property UNDEFINED: Integer read _GetUNDEFINED;
end;

[JavaSignature('android/provider/ContactsContract$DisplayNameSources')]
JContactsContract_DisplayNameSources = interface(IJavaInstance)
['{3EB81D20-435B-4001-B1DA-3E218F014458}']
end;
TJContactsContract_DisplayNameSources = class(TJavaGenericImport<JContactsContract_DisplayNameSourcesClass, JContactsContract_DisplayNameSources>) end;

JContactsContract_DirectoryClass = interface(JObjectClass)
['{6E43711D-2668-4932-8010-D5FB9B363AAE}']
  {Property Methods}
  function _GetACCOUNT_NAME: JString;
  function _GetACCOUNT_TYPE: JString;
  function _GetCONTENT_ITEM_TYPE: JString;
  function _GetCONTENT_TYPE: JString;
  function _GetCONTENT_URI: Jnet_Uri;
  function _GetDEFAULT: Int64;
  function _GetDIRECTORY_AUTHORITY: JString;
  function _GetDISPLAY_NAME: JString;
  function _GetEXPORT_SUPPORT: JString;
  function _GetEXPORT_SUPPORT_ANY_ACCOUNT: Integer;
  function _GetEXPORT_SUPPORT_NONE: Integer;
  function _GetEXPORT_SUPPORT_SAME_ACCOUNT_ONLY: Integer;
  function _GetLOCAL_INVISIBLE: Int64;
  function _GetPACKAGE_NAME: JString;
  function _GetPHOTO_SUPPORT: JString;
  function _GetPHOTO_SUPPORT_FULL: Integer;
  function _GetPHOTO_SUPPORT_FULL_SIZE_ONLY: Integer;
  function _GetPHOTO_SUPPORT_NONE: Integer;
  function _GetPHOTO_SUPPORT_THUMBNAIL_ONLY: Integer;
  function _GetSHORTCUT_SUPPORT: JString;
  function _GetSHORTCUT_SUPPORT_DATA_ITEMS_ONLY: Integer;
  function _GetSHORTCUT_SUPPORT_FULL: Integer;
  function _GetSHORTCUT_SUPPORT_NONE: Integer;
  function _GetTYPE_RESOURCE_ID: JString;
  {Methods}
  procedure notifyDirectoryChange(resolver: JContentResolver); cdecl;
  {Properties}
  property ACCOUNT_NAME: JString read _GetACCOUNT_NAME;
  property ACCOUNT_TYPE: JString read _GetACCOUNT_TYPE;
  property CONTENT_ITEM_TYPE: JString read _GetCONTENT_ITEM_TYPE;
  property CONTENT_TYPE: JString read _GetCONTENT_TYPE;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
  property DEFAULT: Int64 read _GetDEFAULT;
  property DIRECTORY_AUTHORITY: JString read _GetDIRECTORY_AUTHORITY;
  property DISPLAY_NAME: JString read _GetDISPLAY_NAME;
  property EXPORT_SUPPORT: JString read _GetEXPORT_SUPPORT;
  property EXPORT_SUPPORT_ANY_ACCOUNT: Integer read _GetEXPORT_SUPPORT_ANY_ACCOUNT;
  property EXPORT_SUPPORT_NONE: Integer read _GetEXPORT_SUPPORT_NONE;
  property EXPORT_SUPPORT_SAME_ACCOUNT_ONLY: Integer read _GetEXPORT_SUPPORT_SAME_ACCOUNT_ONLY;
  property LOCAL_INVISIBLE: Int64 read _GetLOCAL_INVISIBLE;
  property PACKAGE_NAME: JString read _GetPACKAGE_NAME;
  property PHOTO_SUPPORT: JString read _GetPHOTO_SUPPORT;
  property PHOTO_SUPPORT_FULL: Integer read _GetPHOTO_SUPPORT_FULL;
  property PHOTO_SUPPORT_FULL_SIZE_ONLY: Integer read _GetPHOTO_SUPPORT_FULL_SIZE_ONLY;
  property PHOTO_SUPPORT_NONE: Integer read _GetPHOTO_SUPPORT_NONE;
  property PHOTO_SUPPORT_THUMBNAIL_ONLY: Integer read _GetPHOTO_SUPPORT_THUMBNAIL_ONLY;
  property SHORTCUT_SUPPORT: JString read _GetSHORTCUT_SUPPORT;
  property SHORTCUT_SUPPORT_DATA_ITEMS_ONLY: Integer read _GetSHORTCUT_SUPPORT_DATA_ITEMS_ONLY;
  property SHORTCUT_SUPPORT_FULL: Integer read _GetSHORTCUT_SUPPORT_FULL;
  property SHORTCUT_SUPPORT_NONE: Integer read _GetSHORTCUT_SUPPORT_NONE;
  property TYPE_RESOURCE_ID: JString read _GetTYPE_RESOURCE_ID;
end;

[JavaSignature('android/provider/ContactsContract$Directory')]
JContactsContract_Directory = interface(JObject)
['{B39A38F0-64A3-4CF6-A9D8-3463C165954D}']
end;
TJContactsContract_Directory = class(TJavaGenericImport<JContactsContract_DirectoryClass, JContactsContract_Directory>) end;

JCommonDataKinds_GroupMembershipClass = interface(JObjectClass)
['{D88A5BF9-6278-4B5D-8E77-A63069118869}']
  {Property Methods}
  function _GetCONTENT_ITEM_TYPE: JString;
  function _GetGROUP_ROW_ID: JString;
  function _GetGROUP_SOURCE_ID: JString;
  {Properties}
  property CONTENT_ITEM_TYPE: JString read _GetCONTENT_ITEM_TYPE;
  property GROUP_ROW_ID: JString read _GetGROUP_ROW_ID;
  property GROUP_SOURCE_ID: JString read _GetGROUP_SOURCE_ID;
end;

[JavaSignature('android/provider/ContactsContract$CommonDataKinds$GroupMembership')]
JCommonDataKinds_GroupMembership = interface(JObject)
['{E3EDD408-58E0-4623-AFC3-2B22AFB40EA4}']
end;
TJCommonDataKinds_GroupMembership = class(TJavaGenericImport<JCommonDataKinds_GroupMembershipClass, JCommonDataKinds_GroupMembership>) end;

JRawContacts_EntityClass = interface(JObjectClass)
['{F3CDFCCB-4DBF-4F5D-948B-B01D2CDC24A7}']
  {Property Methods}
  function _GetCONTENT_DIRECTORY: JString;
  function _GetDATA_ID: JString;
  {Properties}
  property CONTENT_DIRECTORY: JString read _GetCONTENT_DIRECTORY;
  property DATA_ID: JString read _GetDATA_ID;
end;

[JavaSignature('android/provider/ContactsContract$RawContacts$Entity')]
JRawContacts_Entity = interface(JObject)
['{37F7049D-1CDA-4275-93E6-CA0FFF28DE8A}']
end;
TJRawContacts_Entity = class(TJavaGenericImport<JRawContacts_EntityClass, JRawContacts_Entity>) end;

JCalendarContractClass = interface(JObjectClass)
['{A2128576-E669-4326-B3F3-6656D161C7D0}']
  {Property Methods}
  function _GetACCOUNT_TYPE_LOCAL: JString;
  function _GetACTION_EVENT_REMINDER: JString;
  function _GetACTION_HANDLE_CUSTOM_EVENT: JString;
  function _GetAUTHORITY: JString;
  function _GetCALLER_IS_SYNCADAPTER: JString;
  function _GetCONTENT_URI: Jnet_Uri;
  function _GetEXTRA_CUSTOM_APP_URI: JString;
  function _GetEXTRA_EVENT_ALL_DAY: JString;
  function _GetEXTRA_EVENT_BEGIN_TIME: JString;
  function _GetEXTRA_EVENT_END_TIME: JString;
  {Properties}
  property ACCOUNT_TYPE_LOCAL: JString read _GetACCOUNT_TYPE_LOCAL;
  property ACTION_EVENT_REMINDER: JString read _GetACTION_EVENT_REMINDER;
  property ACTION_HANDLE_CUSTOM_EVENT: JString read _GetACTION_HANDLE_CUSTOM_EVENT;
  property AUTHORITY: JString read _GetAUTHORITY;
  property CALLER_IS_SYNCADAPTER: JString read _GetCALLER_IS_SYNCADAPTER;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
  property EXTRA_CUSTOM_APP_URI: JString read _GetEXTRA_CUSTOM_APP_URI;
  property EXTRA_EVENT_ALL_DAY: JString read _GetEXTRA_EVENT_ALL_DAY;
  property EXTRA_EVENT_BEGIN_TIME: JString read _GetEXTRA_EVENT_BEGIN_TIME;
  property EXTRA_EVENT_END_TIME: JString read _GetEXTRA_EVENT_END_TIME;
end;

[JavaSignature('android/provider/CalendarContract')]
JCalendarContract = interface(JObject)
['{82B0ED4F-8A46-4C4F-9CA6-D060EC7D3E3B}']
end;
TJCalendarContract = class(TJavaGenericImport<JCalendarContractClass, JCalendarContract>) end;

JContacts_PhonesColumnsClass = interface(IJavaClass)
['{C5F22A6B-2AED-48CE-84BB-3F2C3A43DD2B}']
  {Property Methods}
  function _GetISPRIMARY: JString;
  function _GetLABEL: JString;
  function _GetNUMBER: JString;
  function _GetNUMBER_KEY: JString;
  function _GetTYPE: JString;
  function _GetTYPE_CUSTOM: Integer;
  function _GetTYPE_FAX_HOME: Integer;
  function _GetTYPE_FAX_WORK: Integer;
  function _GetTYPE_HOME: Integer;
  function _GetTYPE_MOBILE: Integer;
  function _GetTYPE_OTHER: Integer;
  function _GetTYPE_PAGER: Integer;
  function _GetTYPE_WORK: Integer;
  {Properties}
  property ISPRIMARY: JString read _GetISPRIMARY;
  property &LABEL: JString read _GetLABEL;
  property NUMBER: JString read _GetNUMBER;
  property NUMBER_KEY: JString read _GetNUMBER_KEY;
  property &TYPE: JString read _GetTYPE;
  property TYPE_CUSTOM: Integer read _GetTYPE_CUSTOM;
  property TYPE_FAX_HOME: Integer read _GetTYPE_FAX_HOME;
  property TYPE_FAX_WORK: Integer read _GetTYPE_FAX_WORK;
  property TYPE_HOME: Integer read _GetTYPE_HOME;
  property TYPE_MOBILE: Integer read _GetTYPE_MOBILE;
  property TYPE_OTHER: Integer read _GetTYPE_OTHER;
  property TYPE_PAGER: Integer read _GetTYPE_PAGER;
  property TYPE_WORK: Integer read _GetTYPE_WORK;
end;

[JavaSignature('android/provider/Contacts$PhonesColumns')]
JContacts_PhonesColumns = interface(IJavaInstance)
['{5967B38B-9C44-40B9-B035-30694E363B53}']
end;
TJContacts_PhonesColumns = class(TJavaGenericImport<JContacts_PhonesColumnsClass, JContacts_PhonesColumns>) end;

JSettingsClass = interface(JObjectClass)
['{DA4ED294-FDE4-4090-BB0A-1013E7217DE7}']
  {Property Methods}
  function _GetACTION_ACCESSIBILITY_SETTINGS: JString;
  function _GetACTION_ADD_ACCOUNT: JString;
  function _GetACTION_AIRPLANE_MODE_SETTINGS: JString;
  function _GetACTION_APN_SETTINGS: JString;
  function _GetACTION_APPLICATION_DETAILS_SETTINGS: JString;
  function _GetACTION_APPLICATION_DEVELOPMENT_SETTINGS: JString;
  function _GetACTION_APPLICATION_SETTINGS: JString;
  function _GetACTION_BLUETOOTH_SETTINGS: JString;
  function _GetACTION_DATA_ROAMING_SETTINGS: JString;
  function _GetACTION_DATE_SETTINGS: JString;
  function _GetACTION_DEVICE_INFO_SETTINGS: JString;
  function _GetACTION_DISPLAY_SETTINGS: JString;
  function _GetACTION_INPUT_METHOD_SETTINGS: JString;
  function _GetACTION_INPUT_METHOD_SUBTYPE_SETTINGS: JString;
  function _GetACTION_INTERNAL_STORAGE_SETTINGS: JString;
  function _GetACTION_LOCALE_SETTINGS: JString;
  function _GetACTION_LOCATION_SOURCE_SETTINGS: JString;
  function _GetACTION_MANAGE_ALL_APPLICATIONS_SETTINGS: JString;
  function _GetACTION_MANAGE_APPLICATIONS_SETTINGS: JString;
  function _GetACTION_MEMORY_CARD_SETTINGS: JString;
  function _GetACTION_NETWORK_OPERATOR_SETTINGS: JString;
  function _GetACTION_NFCSHARING_SETTINGS: JString;
  function _GetACTION_NFC_SETTINGS: JString;
  function _GetACTION_PRIVACY_SETTINGS: JString;
  function _GetACTION_QUICK_LAUNCH_SETTINGS: JString;
  function _GetACTION_SEARCH_SETTINGS: JString;
  function _GetACTION_SECURITY_SETTINGS: JString;
  function _GetACTION_SETTINGS: JString;
  function _GetACTION_SOUND_SETTINGS: JString;
  function _GetACTION_SYNC_SETTINGS: JString;
  function _GetACTION_USER_DICTIONARY_SETTINGS: JString;
  function _GetACTION_WIFI_IP_SETTINGS: JString;
  function _GetACTION_WIFI_SETTINGS: JString;
  function _GetACTION_WIRELESS_SETTINGS: JString;
  function _GetAUTHORITY: JString;
  function _GetEXTRA_AUTHORITIES: JString;
  function _GetEXTRA_INPUT_METHOD_ID: JString;
  {Methods}
  function init: JSettings; cdecl;
  {Properties}
  property ACTION_ACCESSIBILITY_SETTINGS: JString read _GetACTION_ACCESSIBILITY_SETTINGS;
  property ACTION_ADD_ACCOUNT: JString read _GetACTION_ADD_ACCOUNT;
  property ACTION_AIRPLANE_MODE_SETTINGS: JString read _GetACTION_AIRPLANE_MODE_SETTINGS;
  property ACTION_APN_SETTINGS: JString read _GetACTION_APN_SETTINGS;
  property ACTION_APPLICATION_DETAILS_SETTINGS: JString read _GetACTION_APPLICATION_DETAILS_SETTINGS;
  property ACTION_APPLICATION_DEVELOPMENT_SETTINGS: JString read _GetACTION_APPLICATION_DEVELOPMENT_SETTINGS;
  property ACTION_APPLICATION_SETTINGS: JString read _GetACTION_APPLICATION_SETTINGS;
  property ACTION_BLUETOOTH_SETTINGS: JString read _GetACTION_BLUETOOTH_SETTINGS;
  property ACTION_DATA_ROAMING_SETTINGS: JString read _GetACTION_DATA_ROAMING_SETTINGS;
  property ACTION_DATE_SETTINGS: JString read _GetACTION_DATE_SETTINGS;
  property ACTION_DEVICE_INFO_SETTINGS: JString read _GetACTION_DEVICE_INFO_SETTINGS;
  property ACTION_DISPLAY_SETTINGS: JString read _GetACTION_DISPLAY_SETTINGS;
  property ACTION_INPUT_METHOD_SETTINGS: JString read _GetACTION_INPUT_METHOD_SETTINGS;
  property ACTION_INPUT_METHOD_SUBTYPE_SETTINGS: JString read _GetACTION_INPUT_METHOD_SUBTYPE_SETTINGS;
  property ACTION_INTERNAL_STORAGE_SETTINGS: JString read _GetACTION_INTERNAL_STORAGE_SETTINGS;
  property ACTION_LOCALE_SETTINGS: JString read _GetACTION_LOCALE_SETTINGS;
  property ACTION_LOCATION_SOURCE_SETTINGS: JString read _GetACTION_LOCATION_SOURCE_SETTINGS;
  property ACTION_MANAGE_ALL_APPLICATIONS_SETTINGS: JString read _GetACTION_MANAGE_ALL_APPLICATIONS_SETTINGS;
  property ACTION_MANAGE_APPLICATIONS_SETTINGS: JString read _GetACTION_MANAGE_APPLICATIONS_SETTINGS;
  property ACTION_MEMORY_CARD_SETTINGS: JString read _GetACTION_MEMORY_CARD_SETTINGS;
  property ACTION_NETWORK_OPERATOR_SETTINGS: JString read _GetACTION_NETWORK_OPERATOR_SETTINGS;
  property ACTION_NFCSHARING_SETTINGS: JString read _GetACTION_NFCSHARING_SETTINGS;
  property ACTION_NFC_SETTINGS: JString read _GetACTION_NFC_SETTINGS;
  property ACTION_PRIVACY_SETTINGS: JString read _GetACTION_PRIVACY_SETTINGS;
  property ACTION_QUICK_LAUNCH_SETTINGS: JString read _GetACTION_QUICK_LAUNCH_SETTINGS;
  property ACTION_SEARCH_SETTINGS: JString read _GetACTION_SEARCH_SETTINGS;
  property ACTION_SECURITY_SETTINGS: JString read _GetACTION_SECURITY_SETTINGS;
  property ACTION_SETTINGS: JString read _GetACTION_SETTINGS;
  property ACTION_SOUND_SETTINGS: JString read _GetACTION_SOUND_SETTINGS;
  property ACTION_SYNC_SETTINGS: JString read _GetACTION_SYNC_SETTINGS;
  property ACTION_USER_DICTIONARY_SETTINGS: JString read _GetACTION_USER_DICTIONARY_SETTINGS;
  property ACTION_WIFI_IP_SETTINGS: JString read _GetACTION_WIFI_IP_SETTINGS;
  property ACTION_WIFI_SETTINGS: JString read _GetACTION_WIFI_SETTINGS;
  property ACTION_WIRELESS_SETTINGS: JString read _GetACTION_WIRELESS_SETTINGS;
  property AUTHORITY: JString read _GetAUTHORITY;
  property EXTRA_AUTHORITIES: JString read _GetEXTRA_AUTHORITIES;
  property EXTRA_INPUT_METHOD_ID: JString read _GetEXTRA_INPUT_METHOD_ID;
end;

[JavaSignature('android/provider/Settings')]
JSettings = interface(JObject)
['{296C670C-8213-4C1D-9C0C-928939EC7E14}']
end;
TJSettings = class(TJavaGenericImport<JSettingsClass, JSettings>) end;

JContactsContract_DisplayPhotoClass = interface(JObjectClass)
['{2F313095-DDD3-4C10-861E-B0E5208A6CFA}']
  {Property Methods}
  function _GetCONTENT_MAX_DIMENSIONS_URI: Jnet_Uri;
  function _GetCONTENT_URI: Jnet_Uri;
  function _GetDISPLAY_MAX_DIM: JString;
  function _GetTHUMBNAIL_MAX_DIM: JString;
  {Properties}
  property CONTENT_MAX_DIMENSIONS_URI: Jnet_Uri read _GetCONTENT_MAX_DIMENSIONS_URI;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
  property DISPLAY_MAX_DIM: JString read _GetDISPLAY_MAX_DIM;
  property THUMBNAIL_MAX_DIM: JString read _GetTHUMBNAIL_MAX_DIM;
end;

[JavaSignature('android/provider/ContactsContract$DisplayPhoto')]
JContactsContract_DisplayPhoto = interface(JObject)
['{6961D3C7-ACAE-4FAC-8C4C-4866924849AB}']
end;
TJContactsContract_DisplayPhoto = class(TJavaGenericImport<JContactsContract_DisplayPhotoClass, JContactsContract_DisplayPhoto>) end;

JContactsContract_StreamItemsClass = interface(JObjectClass)
['{FA93FCE5-BD4F-4871-ABAC-4DC4C58F7D0C}']
  {Property Methods}
  function _GetCONTENT_ITEM_TYPE: JString;
  function _GetCONTENT_LIMIT_URI: Jnet_Uri;
  function _GetCONTENT_PHOTO_URI: Jnet_Uri;
  function _GetCONTENT_TYPE: JString;
  function _GetCONTENT_URI: Jnet_Uri;
  function _GetMAX_ITEMS: JString;
  {Properties}
  property CONTENT_ITEM_TYPE: JString read _GetCONTENT_ITEM_TYPE;
  property CONTENT_LIMIT_URI: Jnet_Uri read _GetCONTENT_LIMIT_URI;
  property CONTENT_PHOTO_URI: Jnet_Uri read _GetCONTENT_PHOTO_URI;
  property CONTENT_TYPE: JString read _GetCONTENT_TYPE;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
  property MAX_ITEMS: JString read _GetMAX_ITEMS;
end;

[JavaSignature('android/provider/ContactsContract$StreamItems')]
JContactsContract_StreamItems = interface(JObject)
['{B9393420-79B3-44BA-AF4F-57813DBD8008}']
end;
TJContactsContract_StreamItems = class(TJavaGenericImport<JContactsContract_StreamItemsClass, JContactsContract_StreamItems>) end;

JFiles_FileColumnsClass = interface(JMediaStore_MediaColumnsClass)
['{7B8F8B47-E137-4D52-9861-713F016D32DF}']
  {Property Methods}
  function _GetMEDIA_TYPE: JString;
  function _GetMEDIA_TYPE_AUDIO: Integer;
  function _GetMEDIA_TYPE_IMAGE: Integer;
  function _GetMEDIA_TYPE_NONE: Integer;
  function _GetMEDIA_TYPE_PLAYLIST: Integer;
  function _GetMEDIA_TYPE_VIDEO: Integer;
  function _GetMIME_TYPE: JString;
  function _GetPARENT: JString;
  function _GetTITLE: JString;
  {Properties}
  property MEDIA_TYPE: JString read _GetMEDIA_TYPE;
  property MEDIA_TYPE_AUDIO: Integer read _GetMEDIA_TYPE_AUDIO;
  property MEDIA_TYPE_IMAGE: Integer read _GetMEDIA_TYPE_IMAGE;
  property MEDIA_TYPE_NONE: Integer read _GetMEDIA_TYPE_NONE;
  property MEDIA_TYPE_PLAYLIST: Integer read _GetMEDIA_TYPE_PLAYLIST;
  property MEDIA_TYPE_VIDEO: Integer read _GetMEDIA_TYPE_VIDEO;
{MIME_TYPE is defined in parent interface}
  property PARENT: JString read _GetPARENT;
{TITLE is defined in parent interface}
end;

[JavaSignature('android/provider/MediaStore$Files$FileColumns')]
JFiles_FileColumns = interface(JMediaStore_MediaColumns)
['{B441D8C2-8F1C-4088-954E-53E85ADEAA10}']
end;
TJFiles_FileColumns = class(TJavaGenericImport<JFiles_FileColumnsClass, JFiles_FileColumns>) end;

JBrowser_BookmarkColumnsClass = interface(JObjectClass)
['{7AE30AE7-56DD-483A-A4A4-888276CC5CA3}']
  {Property Methods}
  function _GetBOOKMARK: JString;
  function _GetCREATED: JString;
  function _GetDATE: JString;
  function _GetFAVICON: JString;
  function _GetTITLE: JString;
  function _GetURL: JString;
  function _GetVISITS: JString;
  {Methods}
  function init: JBrowser_BookmarkColumns; cdecl;
  {Properties}
  property BOOKMARK: JString read _GetBOOKMARK;
  property CREATED: JString read _GetCREATED;
  property DATE: JString read _GetDATE;
  property FAVICON: JString read _GetFAVICON;
  property TITLE: JString read _GetTITLE;
  property URL: JString read _GetURL;
  property VISITS: JString read _GetVISITS;
end;

[JavaSignature('android/provider/Browser$BookmarkColumns')]
JBrowser_BookmarkColumns = interface(JObject)
['{964347F7-64CB-45CE-B69C-F3F376B7EE44}']
end;
TJBrowser_BookmarkColumns = class(TJavaGenericImport<JBrowser_BookmarkColumnsClass, JBrowser_BookmarkColumns>) end;

JCommonDataKinds_EmailClass = interface(JObjectClass)
['{04DE5162-FD75-4DB3-BC26-F5431C853935}']
  {Property Methods}
  function _GetADDRESS: JString;
  function _GetCONTENT_FILTER_URI: Jnet_Uri;
  function _GetCONTENT_ITEM_TYPE: JString;
  function _GetCONTENT_LOOKUP_URI: Jnet_Uri;
  function _GetCONTENT_TYPE: JString;
  function _GetCONTENT_URI: Jnet_Uri;
  function _GetDISPLAY_NAME: JString;
  function _GetTYPE_HOME: Integer;
  function _GetTYPE_MOBILE: Integer;
  function _GetTYPE_OTHER: Integer;
  function _GetTYPE_WORK: Integer;
  {Methods}
  function getTypeLabel(res: JResources; type_: Integer; label_: JCharSequence): JCharSequence; cdecl;
  function getTypeLabelResource(type_: Integer): Integer; cdecl;
  {Properties}
  property ADDRESS: JString read _GetADDRESS;
  property CONTENT_FILTER_URI: Jnet_Uri read _GetCONTENT_FILTER_URI;
  property CONTENT_ITEM_TYPE: JString read _GetCONTENT_ITEM_TYPE;
  property CONTENT_LOOKUP_URI: Jnet_Uri read _GetCONTENT_LOOKUP_URI;
  property CONTENT_TYPE: JString read _GetCONTENT_TYPE;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
  property DISPLAY_NAME: JString read _GetDISPLAY_NAME;
  property TYPE_HOME: Integer read _GetTYPE_HOME;
  property TYPE_MOBILE: Integer read _GetTYPE_MOBILE;
  property TYPE_OTHER: Integer read _GetTYPE_OTHER;
  property TYPE_WORK: Integer read _GetTYPE_WORK;
end;

[JavaSignature('android/provider/ContactsContract$CommonDataKinds$Email')]
JCommonDataKinds_Email = interface(JObject)
['{E1AC9554-07BA-4399-8907-B92FA714B486}']
end;
TJCommonDataKinds_Email = class(TJavaGenericImport<JCommonDataKinds_EmailClass, JCommonDataKinds_Email>) end;

JCallLogClass = interface(JObjectClass)
['{0C575B19-4E9D-4A3B-A864-F7C4BB9A1744}']
  {Property Methods}
  function _GetAUTHORITY: JString;
  function _GetCONTENT_URI: Jnet_Uri;
  {Methods}
  function init: JCallLog; cdecl;
  {Properties}
  property AUTHORITY: JString read _GetAUTHORITY;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
end;

[JavaSignature('android/provider/CallLog')]
JCallLog = interface(JObject)
['{F29F58F2-1E89-4D8F-AD31-96DF80305D8F}']
end;
TJCallLog = class(TJavaGenericImport<JCallLogClass, JCallLog>) end;

JContacts_OrganizationColumnsClass = interface(IJavaClass)
['{BE6F7681-BECC-414F-9D3A-546CC869EBC7}']
  {Property Methods}
  function _GetCOMPANY: JString;
  function _GetISPRIMARY: JString;
  function _GetLABEL: JString;
  function _GetPERSON_ID: JString;
  function _GetTITLE: JString;
  function _GetTYPE: JString;
  function _GetTYPE_CUSTOM: Integer;
  function _GetTYPE_OTHER: Integer;
  function _GetTYPE_WORK: Integer;
  {Properties}
  property COMPANY: JString read _GetCOMPANY;
  property ISPRIMARY: JString read _GetISPRIMARY;
  property &LABEL: JString read _GetLABEL;
  property PERSON_ID: JString read _GetPERSON_ID;
  property TITLE: JString read _GetTITLE;
  property &TYPE: JString read _GetTYPE;
  property TYPE_CUSTOM: Integer read _GetTYPE_CUSTOM;
  property TYPE_OTHER: Integer read _GetTYPE_OTHER;
  property TYPE_WORK: Integer read _GetTYPE_WORK;
end;

[JavaSignature('android/provider/Contacts$OrganizationColumns')]
JContacts_OrganizationColumns = interface(IJavaInstance)
['{51903653-253E-4641-806A-E34515848A92}']
end;
TJContacts_OrganizationColumns = class(TJavaGenericImport<JContacts_OrganizationColumnsClass, JContacts_OrganizationColumns>) end;

JAudio_GenresColumnsClass = interface(IJavaClass)
['{3A8F64EE-DF92-4EE7-9199-28BE3DD9A3F7}']
  {Property Methods}
  function _GetNAME: JString;
  {Properties}
  property NAME: JString read _GetNAME;
end;

[JavaSignature('android/provider/MediaStore$Audio$GenresColumns')]
JAudio_GenresColumns = interface(IJavaInstance)
['{90711D49-47C9-447C-B455-B3F3E57E100F}']
end;
TJAudio_GenresColumns = class(TJavaGenericImport<JAudio_GenresColumnsClass, JAudio_GenresColumns>) end;

JContacts_PhotosColumnsClass = interface(IJavaClass)
['{D754AED5-EAE1-4475-A430-DA8FF3C82D5F}']
  {Property Methods}
  function _GetDATA: JString;
  function _GetDOWNLOAD_REQUIRED: JString;
  function _GetEXISTS_ON_SERVER: JString;
  function _GetLOCAL_VERSION: JString;
  function _GetPERSON_ID: JString;
  function _GetSYNC_ERROR: JString;
  {Properties}
  property DATA: JString read _GetDATA;
  property DOWNLOAD_REQUIRED: JString read _GetDOWNLOAD_REQUIRED;
  property EXISTS_ON_SERVER: JString read _GetEXISTS_ON_SERVER;
  property LOCAL_VERSION: JString read _GetLOCAL_VERSION;
  property PERSON_ID: JString read _GetPERSON_ID;
  property SYNC_ERROR: JString read _GetSYNC_ERROR;
end;

[JavaSignature('android/provider/Contacts$PhotosColumns')]
JContacts_PhotosColumns = interface(IJavaInstance)
['{40B8ECB5-8B18-4678-818A-3880485BF463}']
end;
TJContacts_PhotosColumns = class(TJavaGenericImport<JContacts_PhotosColumnsClass, JContacts_PhotosColumns>) end;

JAudio_ArtistsClass = interface(JObjectClass)
['{873AC565-C58A-444E-BDED-5E1FF5A6CBFF}']
  {Property Methods}
  function _GetCONTENT_TYPE: JString;
  function _GetDEFAULT_SORT_ORDER: JString;
  function _GetENTRY_CONTENT_TYPE: JString;
  function _GetEXTERNAL_CONTENT_URI: Jnet_Uri;
  function _GetINTERNAL_CONTENT_URI: Jnet_Uri;
  {Methods}
  function init: JAudio_Artists; cdecl;
  function getContentUri(volumeName: JString): Jnet_Uri; cdecl;
  {Properties}
  property CONTENT_TYPE: JString read _GetCONTENT_TYPE;
  property DEFAULT_SORT_ORDER: JString read _GetDEFAULT_SORT_ORDER;
  property ENTRY_CONTENT_TYPE: JString read _GetENTRY_CONTENT_TYPE;
  property EXTERNAL_CONTENT_URI: Jnet_Uri read _GetEXTERNAL_CONTENT_URI;
  property INTERNAL_CONTENT_URI: Jnet_Uri read _GetINTERNAL_CONTENT_URI;
end;

[JavaSignature('android/provider/MediaStore$Audio$Artists')]
JAudio_Artists = interface(JObject)
['{C5095A12-83FA-4BB2-B4F2-EF64F6AFB0A2}']
end;
TJAudio_Artists = class(TJavaGenericImport<JAudio_ArtistsClass, JAudio_Artists>) end;

JContactsContract_IntentsClass = interface(JObjectClass)
['{85920F24-502A-40B0-8362-3BED71F6F159}']
  {Property Methods}
  function _GetATTACH_IMAGE: JString;
  function _GetEXTRA_CREATE_DESCRIPTION: JString;
  function _GetEXTRA_FORCE_CREATE: JString;
  function _GetINVITE_CONTACT: JString;
  function _GetSEARCH_SUGGESTION_CLICKED: JString;
  function _GetSEARCH_SUGGESTION_CREATE_CONTACT_CLICKED: JString;
  function _GetSEARCH_SUGGESTION_DIAL_NUMBER_CLICKED: JString;
  function _GetSHOW_OR_CREATE_CONTACT: JString;
  {Methods}
  function init: JContactsContract_Intents; cdecl;
  {Properties}
  property ATTACH_IMAGE: JString read _GetATTACH_IMAGE;
  property EXTRA_CREATE_DESCRIPTION: JString read _GetEXTRA_CREATE_DESCRIPTION;
  property EXTRA_FORCE_CREATE: JString read _GetEXTRA_FORCE_CREATE;
  property INVITE_CONTACT: JString read _GetINVITE_CONTACT;
  property SEARCH_SUGGESTION_CLICKED: JString read _GetSEARCH_SUGGESTION_CLICKED;
  property SEARCH_SUGGESTION_CREATE_CONTACT_CLICKED: JString read _GetSEARCH_SUGGESTION_CREATE_CONTACT_CLICKED;
  property SEARCH_SUGGESTION_DIAL_NUMBER_CLICKED: JString read _GetSEARCH_SUGGESTION_DIAL_NUMBER_CLICKED;
  property SHOW_OR_CREATE_CONTACT: JString read _GetSHOW_OR_CREATE_CONTACT;
end;

[JavaSignature('android/provider/ContactsContract$Intents')]
JContactsContract_Intents = interface(JObject)
['{C5E2ADB9-00FF-45EC-B335-30D7B8945D2D}']
end;
TJContactsContract_Intents = class(TJavaGenericImport<JContactsContract_IntentsClass, JContactsContract_Intents>) end;

JCommonDataKinds_NoteClass = interface(JObjectClass)
['{64340864-E653-4857-879D-91B3BCC9FBE5}']
  {Property Methods}
  function _GetCONTENT_ITEM_TYPE: JString;
  function _GetNOTE: JString;
  {Properties}
  property CONTENT_ITEM_TYPE: JString read _GetCONTENT_ITEM_TYPE;
  property NOTE: JString read _GetNOTE;
end;

[JavaSignature('android/provider/ContactsContract$CommonDataKinds$Note')]
JCommonDataKinds_Note = interface(JObject)
['{E8511BB9-03CF-44DD-A770-7769DFB2FAA1}']
end;
TJCommonDataKinds_Note = class(TJavaGenericImport<JCommonDataKinds_NoteClass, JCommonDataKinds_Note>) end;

JContacts_StreamItemsClass = interface(JObjectClass)
['{203681AA-C7E2-4E2B-ADED-51004DA02BB5}']
  {Property Methods}
  function _GetCONTENT_DIRECTORY: JString;
  {Properties}
  property CONTENT_DIRECTORY: JString read _GetCONTENT_DIRECTORY;
end;

[JavaSignature('android/provider/ContactsContract$Contacts$StreamItems')]
JContacts_StreamItems = interface(JObject)
['{C8FBA8C6-1B77-420F-8630-DE0A6743EF24}']
end;
TJContacts_StreamItems = class(TJavaGenericImport<JContacts_StreamItemsClass, JContacts_StreamItems>) end;

JAudio_AudioColumnsClass = interface(JMediaStore_MediaColumnsClass)
['{A9488C47-84FD-4900-AA5A-6DDC65B00EAB}']
  {Property Methods}
  function _GetALBUM: JString;
  function _GetALBUM_ID: JString;
  function _GetALBUM_KEY: JString;
  function _GetARTIST: JString;
  function _GetARTIST_ID: JString;
  function _GetARTIST_KEY: JString;
  function _GetBOOKMARK: JString;
  function _GetCOMPOSER: JString;
  function _GetDURATION: JString;
  function _GetIS_ALARM: JString;
  function _GetIS_MUSIC: JString;
  function _GetIS_NOTIFICATION: JString;
  function _GetIS_PODCAST: JString;
  function _GetIS_RINGTONE: JString;
  function _GetTITLE_KEY: JString;
  function _GetTRACK: JString;
  function _GetYEAR: JString;
  {Properties}
  property ALBUM: JString read _GetALBUM;
  property ALBUM_ID: JString read _GetALBUM_ID;
  property ALBUM_KEY: JString read _GetALBUM_KEY;
  property ARTIST: JString read _GetARTIST;
  property ARTIST_ID: JString read _GetARTIST_ID;
  property ARTIST_KEY: JString read _GetARTIST_KEY;
  property BOOKMARK: JString read _GetBOOKMARK;
  property COMPOSER: JString read _GetCOMPOSER;
  property DURATION: JString read _GetDURATION;
  property IS_ALARM: JString read _GetIS_ALARM;
  property IS_MUSIC: JString read _GetIS_MUSIC;
  property IS_NOTIFICATION: JString read _GetIS_NOTIFICATION;
  property IS_PODCAST: JString read _GetIS_PODCAST;
  property IS_RINGTONE: JString read _GetIS_RINGTONE;
  property TITLE_KEY: JString read _GetTITLE_KEY;
  property TRACK: JString read _GetTRACK;
  property YEAR: JString read _GetYEAR;
end;

[JavaSignature('android/provider/MediaStore$Audio$AudioColumns')]
JAudio_AudioColumns = interface(JMediaStore_MediaColumns)
['{A8403679-E9A9-416A-829E-F39D1E4772D2}']
end;
TJAudio_AudioColumns = class(TJavaGenericImport<JAudio_AudioColumnsClass, JAudio_AudioColumns>) end;

JRawContacts_DataClass = interface(JObjectClass)
['{A561B9F3-0954-447B-9F40-561B3C49B2F3}']
  {Property Methods}
  function _GetCONTENT_DIRECTORY: JString;
  {Properties}
  property CONTENT_DIRECTORY: JString read _GetCONTENT_DIRECTORY;
end;

[JavaSignature('android/provider/ContactsContract$RawContacts$Data')]
JRawContacts_Data = interface(JObject)
['{E8D7D61B-005D-4E36-83F0-5C2B6CE67895}']
end;
TJRawContacts_Data = class(TJavaGenericImport<JRawContacts_DataClass, JRawContacts_Data>) end;

JCommonDataKinds_RelationClass = interface(JObjectClass)
['{010209B7-605E-4193-86C8-20E4DACCAA34}']
  {Property Methods}
  function _GetCONTENT_ITEM_TYPE: JString;
  function _GetNAME: JString;
  function _GetTYPE_ASSISTANT: Integer;
  function _GetTYPE_BROTHER: Integer;
  function _GetTYPE_CHILD: Integer;
  function _GetTYPE_DOMESTIC_PARTNER: Integer;
  function _GetTYPE_FATHER: Integer;
  function _GetTYPE_FRIEND: Integer;
  function _GetTYPE_MANAGER: Integer;
  function _GetTYPE_MOTHER: Integer;
  function _GetTYPE_PARENT: Integer;
  function _GetTYPE_PARTNER: Integer;
  function _GetTYPE_REFERRED_BY: Integer;
  function _GetTYPE_RELATIVE: Integer;
  function _GetTYPE_SISTER: Integer;
  function _GetTYPE_SPOUSE: Integer;
  {Methods}
  function getTypeLabel(res: JResources; type_: Integer; label_: JCharSequence): JCharSequence; cdecl;
  function getTypeLabelResource(type_: Integer): Integer; cdecl;
  {Properties}
  property CONTENT_ITEM_TYPE: JString read _GetCONTENT_ITEM_TYPE;
  property NAME: JString read _GetNAME;
  property TYPE_ASSISTANT: Integer read _GetTYPE_ASSISTANT;
  property TYPE_BROTHER: Integer read _GetTYPE_BROTHER;
  property TYPE_CHILD: Integer read _GetTYPE_CHILD;
  property TYPE_DOMESTIC_PARTNER: Integer read _GetTYPE_DOMESTIC_PARTNER;
  property TYPE_FATHER: Integer read _GetTYPE_FATHER;
  property TYPE_FRIEND: Integer read _GetTYPE_FRIEND;
  property TYPE_MANAGER: Integer read _GetTYPE_MANAGER;
  property TYPE_MOTHER: Integer read _GetTYPE_MOTHER;
  property TYPE_PARENT: Integer read _GetTYPE_PARENT;
  property TYPE_PARTNER: Integer read _GetTYPE_PARTNER;
  property TYPE_REFERRED_BY: Integer read _GetTYPE_REFERRED_BY;
  property TYPE_RELATIVE: Integer read _GetTYPE_RELATIVE;
  property TYPE_SISTER: Integer read _GetTYPE_SISTER;
  property TYPE_SPOUSE: Integer read _GetTYPE_SPOUSE;
end;

[JavaSignature('android/provider/ContactsContract$CommonDataKinds$Relation')]
JCommonDataKinds_Relation = interface(JObject)
['{61EC4712-3967-49D6-B267-534F68E726AF}']
end;
TJCommonDataKinds_Relation = class(TJavaGenericImport<JCommonDataKinds_RelationClass, JCommonDataKinds_Relation>) end;

JAudio_PlaylistsClass = interface(JObjectClass)
['{7C1CAE82-7939-4928-9E06-D8D7B8731381}']
  {Property Methods}
  function _GetCONTENT_TYPE: JString;
  function _GetDEFAULT_SORT_ORDER: JString;
  function _GetENTRY_CONTENT_TYPE: JString;
  function _GetEXTERNAL_CONTENT_URI: Jnet_Uri;
  function _GetINTERNAL_CONTENT_URI: Jnet_Uri;
  {Methods}
  function init: JAudio_Playlists; cdecl;
  function getContentUri(volumeName: JString): Jnet_Uri; cdecl;
  {Properties}
  property CONTENT_TYPE: JString read _GetCONTENT_TYPE;
  property DEFAULT_SORT_ORDER: JString read _GetDEFAULT_SORT_ORDER;
  property ENTRY_CONTENT_TYPE: JString read _GetENTRY_CONTENT_TYPE;
  property EXTERNAL_CONTENT_URI: Jnet_Uri read _GetEXTERNAL_CONTENT_URI;
  property INTERNAL_CONTENT_URI: Jnet_Uri read _GetINTERNAL_CONTENT_URI;
end;

[JavaSignature('android/provider/MediaStore$Audio$Playlists')]
JAudio_Playlists = interface(JObject)
['{8F3F37B9-A743-41A0-8E48-A4E0A339E338}']
end;
TJAudio_Playlists = class(TJavaGenericImport<JAudio_PlaylistsClass, JAudio_Playlists>) end;

JContactsClass = interface(JObjectClass)
['{084AE141-8566-4CCE-B5E2-7ECA373E1A0F}']
  {Property Methods}
  function _GetAUTHORITY: JString;
  function _GetCONTENT_URI: Jnet_Uri;
  function _GetKIND_EMAIL: Integer;
  function _GetKIND_IM: Integer;
  function _GetKIND_ORGANIZATION: Integer;
  function _GetKIND_PHONE: Integer;
  function _GetKIND_POSTAL: Integer;
  {Properties}
  property AUTHORITY: JString read _GetAUTHORITY;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
  property KIND_EMAIL: Integer read _GetKIND_EMAIL;
  property KIND_IM: Integer read _GetKIND_IM;
  property KIND_ORGANIZATION: Integer read _GetKIND_ORGANIZATION;
  property KIND_PHONE: Integer read _GetKIND_PHONE;
  property KIND_POSTAL: Integer read _GetKIND_POSTAL;
end;

[JavaSignature('android/provider/Contacts')]
JContacts = interface(JObject)
['{31576FEB-6779-489E-9B8D-BE98F11BB7FC}']
end;
TJContacts = class(TJavaGenericImport<JContactsClass, JContacts>) end;

JVoicemailContract_StatusClass = interface(JObjectClass)
['{556B834A-A1B6-4A3F-8C01-81C9AFEF2BC7}']
  {Property Methods}
  function _GetCONFIGURATION_STATE: JString;
  function _GetCONFIGURATION_STATE_CAN_BE_CONFIGURED: Integer;
  function _GetCONFIGURATION_STATE_NOT_CONFIGURED: Integer;
  function _GetCONFIGURATION_STATE_OK: Integer;
  function _GetCONTENT_URI: Jnet_Uri;
  function _GetDATA_CHANNEL_STATE: JString;
  function _GetDATA_CHANNEL_STATE_NO_CONNECTION: Integer;
  function _GetDATA_CHANNEL_STATE_OK: Integer;
  function _GetDIR_TYPE: JString;
  function _GetITEM_TYPE: JString;
  function _GetNOTIFICATION_CHANNEL_STATE: JString;
  function _GetNOTIFICATION_CHANNEL_STATE_MESSAGE_WAITING: Integer;
  function _GetNOTIFICATION_CHANNEL_STATE_NO_CONNECTION: Integer;
  function _GetNOTIFICATION_CHANNEL_STATE_OK: Integer;
  function _GetSETTINGS_URI: JString;
  function _GetSOURCE_PACKAGE: JString;
  function _GetVOICEMAIL_ACCESS_URI: JString;
  {Methods}
  function buildSourceUri(packageName: JString): Jnet_Uri; cdecl;
  {Properties}
  property CONFIGURATION_STATE: JString read _GetCONFIGURATION_STATE;
  property CONFIGURATION_STATE_CAN_BE_CONFIGURED: Integer read _GetCONFIGURATION_STATE_CAN_BE_CONFIGURED;
  property CONFIGURATION_STATE_NOT_CONFIGURED: Integer read _GetCONFIGURATION_STATE_NOT_CONFIGURED;
  property CONFIGURATION_STATE_OK: Integer read _GetCONFIGURATION_STATE_OK;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
  property DATA_CHANNEL_STATE: JString read _GetDATA_CHANNEL_STATE;
  property DATA_CHANNEL_STATE_NO_CONNECTION: Integer read _GetDATA_CHANNEL_STATE_NO_CONNECTION;
  property DATA_CHANNEL_STATE_OK: Integer read _GetDATA_CHANNEL_STATE_OK;
  property DIR_TYPE: JString read _GetDIR_TYPE;
  property ITEM_TYPE: JString read _GetITEM_TYPE;
  property NOTIFICATION_CHANNEL_STATE: JString read _GetNOTIFICATION_CHANNEL_STATE;
  property NOTIFICATION_CHANNEL_STATE_MESSAGE_WAITING: Integer read _GetNOTIFICATION_CHANNEL_STATE_MESSAGE_WAITING;
  property NOTIFICATION_CHANNEL_STATE_NO_CONNECTION: Integer read _GetNOTIFICATION_CHANNEL_STATE_NO_CONNECTION;
  property NOTIFICATION_CHANNEL_STATE_OK: Integer read _GetNOTIFICATION_CHANNEL_STATE_OK;
  property SETTINGS_URI: JString read _GetSETTINGS_URI;
  property SOURCE_PACKAGE: JString read _GetSOURCE_PACKAGE;
  property VOICEMAIL_ACCESS_URI: JString read _GetVOICEMAIL_ACCESS_URI;
end;

[JavaSignature('android/provider/VoicemailContract$Status')]
JVoicemailContract_Status = interface(JObject)
['{BB6D0E7D-48D7-4F15-9F84-69867E29D65C}']
end;
TJVoicemailContract_Status = class(TJavaGenericImport<JVoicemailContract_StatusClass, JVoicemailContract_Status>) end;

JImages_ThumbnailsClass = interface(JObjectClass)
['{A25C11DA-B72B-480D-8A09-670018A80071}']
  {Property Methods}
  function _GetDATA: JString;
  function _GetDEFAULT_SORT_ORDER: JString;
  function _GetEXTERNAL_CONTENT_URI: Jnet_Uri;
  function _GetFULL_SCREEN_KIND: Integer;
  function _GetHEIGHT: JString;
  function _GetIMAGE_ID: JString;
  function _GetINTERNAL_CONTENT_URI: Jnet_Uri;
  function _GetKIND: JString;
  function _GetMICRO_KIND: Integer;
  function _GetMINI_KIND: Integer;
  function _GetTHUMB_DATA: JString;
  function _GetWIDTH: JString;
  {Methods}
  function init: JImages_Thumbnails; cdecl;
  procedure cancelThumbnailRequest(cr: JContentResolver; origId: Int64); cdecl; overload;
  procedure cancelThumbnailRequest(cr: JContentResolver; origId: Int64; groupId: Int64); cdecl; overload;
  function getContentUri(volumeName: JString): Jnet_Uri; cdecl;
  function getThumbnail(cr: JContentResolver; origId: Int64; kind: Integer; options: JBitmapFactory_Options): JBitmap; cdecl; overload;
  function getThumbnail(cr: JContentResolver; origId: Int64; groupId: Int64; kind: Integer; options: JBitmapFactory_Options): JBitmap; cdecl; overload;
  function query(cr: JContentResolver; uri: Jnet_Uri; projection: TJavaObjectArray<JString>): JCursor; cdecl;
  function queryMiniThumbnail(cr: JContentResolver; origId: Int64; kind: Integer; projection: TJavaObjectArray<JString>): JCursor; cdecl;
  function queryMiniThumbnails(cr: JContentResolver; uri: Jnet_Uri; kind: Integer; projection: TJavaObjectArray<JString>): JCursor; cdecl;
  {Properties}
  property DATA: JString read _GetDATA;
  property DEFAULT_SORT_ORDER: JString read _GetDEFAULT_SORT_ORDER;
  property EXTERNAL_CONTENT_URI: Jnet_Uri read _GetEXTERNAL_CONTENT_URI;
  property FULL_SCREEN_KIND: Integer read _GetFULL_SCREEN_KIND;
  property HEIGHT: JString read _GetHEIGHT;
  property IMAGE_ID: JString read _GetIMAGE_ID;
  property INTERNAL_CONTENT_URI: Jnet_Uri read _GetINTERNAL_CONTENT_URI;
  property KIND: JString read _GetKIND;
  property MICRO_KIND: Integer read _GetMICRO_KIND;
  property MINI_KIND: Integer read _GetMINI_KIND;
  property THUMB_DATA: JString read _GetTHUMB_DATA;
  property WIDTH: JString read _GetWIDTH;
end;

[JavaSignature('android/provider/MediaStore$Images$Thumbnails')]
JImages_Thumbnails = interface(JObject)
['{29CB9363-CD45-4C6E-86EF-FD421A2C7A11}']
end;
TJImages_Thumbnails = class(TJavaGenericImport<JImages_ThumbnailsClass, JImages_Thumbnails>) end;

JContacts_PresenceColumnsClass = interface(IJavaClass)
['{801ACDB6-3761-4304-B5FA-0E22F24A2674}']
  {Property Methods}
  function _GetAVAILABLE: Integer;
  function _GetAWAY: Integer;
  function _GetDO_NOT_DISTURB: Integer;
  function _GetIDLE: Integer;
  function _GetIM_ACCOUNT: JString;
  function _GetIM_HANDLE: JString;
  function _GetIM_PROTOCOL: JString;
  function _GetINVISIBLE: Integer;
  function _GetOFFLINE: Integer;
  function _GetPRESENCE_CUSTOM_STATUS: JString;
  function _GetPRESENCE_STATUS: JString;
  function _GetPRIORITY: JString;
  {Properties}
  property AVAILABLE: Integer read _GetAVAILABLE;
  property AWAY: Integer read _GetAWAY;
  property DO_NOT_DISTURB: Integer read _GetDO_NOT_DISTURB;
  property IDLE: Integer read _GetIDLE;
  property IM_ACCOUNT: JString read _GetIM_ACCOUNT;
  property IM_HANDLE: JString read _GetIM_HANDLE;
  property IM_PROTOCOL: JString read _GetIM_PROTOCOL;
  property INVISIBLE: Integer read _GetINVISIBLE;
  property OFFLINE: Integer read _GetOFFLINE;
  property PRESENCE_CUSTOM_STATUS: JString read _GetPRESENCE_CUSTOM_STATUS;
  property PRESENCE_STATUS: JString read _GetPRESENCE_STATUS;
  property PRIORITY: JString read _GetPRIORITY;
end;

[JavaSignature('android/provider/Contacts$PresenceColumns')]
JContacts_PresenceColumns = interface(IJavaInstance)
['{90EAC3A6-5FF6-42F5-8248-94E41BE20054}']
end;
TJContacts_PresenceColumns = class(TJavaGenericImport<JContacts_PresenceColumnsClass, JContacts_PresenceColumns>) end;

JSearchRecentSuggestionsClass = interface(JObjectClass)
['{1F92F95B-3389-47E1-BE54-81F6764D2F1C}']
  {Property Methods}
  function _GetQUERIES_PROJECTION_1LINE: TJavaObjectArray<JString>;
  function _GetQUERIES_PROJECTION_2LINE: TJavaObjectArray<JString>;
  function _GetQUERIES_PROJECTION_DATE_INDEX: Integer;
  function _GetQUERIES_PROJECTION_DISPLAY1_INDEX: Integer;
  function _GetQUERIES_PROJECTION_DISPLAY2_INDEX: Integer;
  function _GetQUERIES_PROJECTION_QUERY_INDEX: Integer;
  {Methods}
  function init(context: JContext; authority: JString; mode: Integer): JSearchRecentSuggestions; cdecl;
  {Properties}
  property QUERIES_PROJECTION_1LINE: TJavaObjectArray<JString> read _GetQUERIES_PROJECTION_1LINE;
  property QUERIES_PROJECTION_2LINE: TJavaObjectArray<JString> read _GetQUERIES_PROJECTION_2LINE;
  property QUERIES_PROJECTION_DATE_INDEX: Integer read _GetQUERIES_PROJECTION_DATE_INDEX;
  property QUERIES_PROJECTION_DISPLAY1_INDEX: Integer read _GetQUERIES_PROJECTION_DISPLAY1_INDEX;
  property QUERIES_PROJECTION_DISPLAY2_INDEX: Integer read _GetQUERIES_PROJECTION_DISPLAY2_INDEX;
  property QUERIES_PROJECTION_QUERY_INDEX: Integer read _GetQUERIES_PROJECTION_QUERY_INDEX;
end;

[JavaSignature('android/provider/SearchRecentSuggestions')]
JSearchRecentSuggestions = interface(JObject)
['{B35840AF-5E60-430E-B147-9F41195FC142}']
  {Methods}
  procedure clearHistory; cdecl;
  procedure saveRecentQuery(queryString: JString; line2: JString); cdecl;
end;
TJSearchRecentSuggestions = class(TJavaGenericImport<JSearchRecentSuggestionsClass, JSearchRecentSuggestions>) end;

JGenres_MembersClass = interface(JObjectClass)
['{9F92E76B-593A-4B90-AA93-C6ACD6AB0E42}']
  {Property Methods}
  function _GetAUDIO_ID: JString;
  function _GetCONTENT_DIRECTORY: JString;
  function _GetDEFAULT_SORT_ORDER: JString;
  function _GetGENRE_ID: JString;
  {Methods}
  function init: JGenres_Members; cdecl;
  function getContentUri(volumeName: JString; genreId: Int64): Jnet_Uri; cdecl;
  {Properties}
  property AUDIO_ID: JString read _GetAUDIO_ID;
  property CONTENT_DIRECTORY: JString read _GetCONTENT_DIRECTORY;
  property DEFAULT_SORT_ORDER: JString read _GetDEFAULT_SORT_ORDER;
  property GENRE_ID: JString read _GetGENRE_ID;
end;

[JavaSignature('android/provider/MediaStore$Audio$Genres$Members')]
JGenres_Members = interface(JObject)
['{C2CE0AE2-C138-4167-A4CB-3C66BC3460FD}']
end;
TJGenres_Members = class(TJavaGenericImport<JGenres_MembersClass, JGenres_Members>) end;

JPeople_ExtensionsClass = interface(JObjectClass)
['{F1ED3823-09EE-4C45-9887-ADF2A764C108}']
  {Property Methods}
  function _GetCONTENT_DIRECTORY: JString;
  function _GetDEFAULT_SORT_ORDER: JString;
  function _GetPERSON_ID: JString;
  {Properties}
  property CONTENT_DIRECTORY: JString read _GetCONTENT_DIRECTORY;
  property DEFAULT_SORT_ORDER: JString read _GetDEFAULT_SORT_ORDER;
  property PERSON_ID: JString read _GetPERSON_ID;
end;

[JavaSignature('android/provider/Contacts$People$Extensions')]
JPeople_Extensions = interface(JObject)
['{BCE87217-DDFC-47F1-9673-02D23EA359DC}']
end;
TJPeople_Extensions = class(TJavaGenericImport<JPeople_ExtensionsClass, JPeople_Extensions>) end;

JCalendarContract_CalendarsClass = interface(JObjectClass)
['{C87B42D5-88AD-4981-95D6-C30643133988}']
  {Property Methods}
  function _GetCALENDAR_LOCATION: JString;
  function _GetCONTENT_URI: Jnet_Uri;
  function _GetDEFAULT_SORT_ORDER: JString;
  function _GetNAME: JString;
  {Properties}
  property CALENDAR_LOCATION: JString read _GetCALENDAR_LOCATION;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
  property DEFAULT_SORT_ORDER: JString read _GetDEFAULT_SORT_ORDER;
  property NAME: JString read _GetNAME;
end;

[JavaSignature('android/provider/CalendarContract$Calendars')]
JCalendarContract_Calendars = interface(JObject)
['{D2EB7AF9-43F8-4D96-9D85-2147E35F75FC}']
end;
TJCalendarContract_Calendars = class(TJavaGenericImport<JCalendarContract_CalendarsClass, JCalendarContract_Calendars>) end;

JSyncStateContractClass = interface(JObjectClass)
['{52C4E2D7-E8D9-4B81-9EE2-13089945D523}']
  {Methods}
  function init: JSyncStateContract; cdecl;
end;

[JavaSignature('android/provider/SyncStateContract')]
JSyncStateContract = interface(JObject)
['{85043796-9477-4DB1-95DD-9AD7BE985712}']
end;
TJSyncStateContract = class(TJavaGenericImport<JSyncStateContractClass, JSyncStateContract>) end;

JContactsContract_PhoneLookupClass = interface(JObjectClass)
['{6580BC4C-30AE-4823-B6C0-220944F74C6D}']
  {Property Methods}
  function _GetCONTENT_FILTER_URI: Jnet_Uri;
  {Properties}
  property CONTENT_FILTER_URI: Jnet_Uri read _GetCONTENT_FILTER_URI;
end;

[JavaSignature('android/provider/ContactsContract$PhoneLookup')]
JContactsContract_PhoneLookup = interface(JObject)
['{67220900-E64C-4EBA-A0C9-30759B6DD2C1}']
end;
TJContactsContract_PhoneLookup = class(TJavaGenericImport<JContactsContract_PhoneLookupClass, JContactsContract_PhoneLookup>) end;

JCommonDataKinds_ImClass = interface(JObjectClass)
['{3BE10F94-DF30-4E24-9652-7E5AC7500A33}']
  {Property Methods}
  function _GetCONTENT_ITEM_TYPE: JString;
  function _GetCUSTOM_PROTOCOL: JString;
  function _GetPROTOCOL: JString;
  function _GetPROTOCOL_AIM: Integer;
  function _GetPROTOCOL_CUSTOM: Integer;
  function _GetPROTOCOL_GOOGLE_TALK: Integer;
  function _GetPROTOCOL_ICQ: Integer;
  function _GetPROTOCOL_JABBER: Integer;
  function _GetPROTOCOL_MSN: Integer;
  function _GetPROTOCOL_NETMEETING: Integer;
  function _GetPROTOCOL_QQ: Integer;
  function _GetPROTOCOL_SKYPE: Integer;
  function _GetPROTOCOL_YAHOO: Integer;
  function _GetTYPE_HOME: Integer;
  function _GetTYPE_OTHER: Integer;
  function _GetTYPE_WORK: Integer;
  {Methods}
  function getProtocolLabel(res: JResources; type_: Integer; label_: JCharSequence): JCharSequence; cdecl;
  function getProtocolLabelResource(type_: Integer): Integer; cdecl;
  function getTypeLabel(res: JResources; type_: Integer; label_: JCharSequence): JCharSequence; cdecl;
  function getTypeLabelResource(type_: Integer): Integer; cdecl;
  {Properties}
  property CONTENT_ITEM_TYPE: JString read _GetCONTENT_ITEM_TYPE;
  property CUSTOM_PROTOCOL: JString read _GetCUSTOM_PROTOCOL;
  property PROTOCOL: JString read _GetPROTOCOL;
  property PROTOCOL_AIM: Integer read _GetPROTOCOL_AIM;
  property PROTOCOL_CUSTOM: Integer read _GetPROTOCOL_CUSTOM;
  property PROTOCOL_GOOGLE_TALK: Integer read _GetPROTOCOL_GOOGLE_TALK;
  property PROTOCOL_ICQ: Integer read _GetPROTOCOL_ICQ;
  property PROTOCOL_JABBER: Integer read _GetPROTOCOL_JABBER;
  property PROTOCOL_MSN: Integer read _GetPROTOCOL_MSN;
  property PROTOCOL_NETMEETING: Integer read _GetPROTOCOL_NETMEETING;
  property PROTOCOL_QQ: Integer read _GetPROTOCOL_QQ;
  property PROTOCOL_SKYPE: Integer read _GetPROTOCOL_SKYPE;
  property PROTOCOL_YAHOO: Integer read _GetPROTOCOL_YAHOO;
  property TYPE_HOME: Integer read _GetTYPE_HOME;
  property TYPE_OTHER: Integer read _GetTYPE_OTHER;
  property TYPE_WORK: Integer read _GetTYPE_WORK;
end;

[JavaSignature('android/provider/ContactsContract$CommonDataKinds$Im')]
JCommonDataKinds_Im = interface(JObject)
['{3856508C-FFDF-4100-96F4-BECF719C7479}']
end;
TJCommonDataKinds_Im = class(TJavaGenericImport<JCommonDataKinds_ImClass, JCommonDataKinds_Im>) end;

JLiveFoldersClass = interface(JObjectClass)
['{6572B14B-4141-4AEC-AFE4-07EB06364DBB}']
  {Property Methods}
  function _GetACTION_CREATE_LIVE_FOLDER: JString;
  function _GetDESCRIPTION: JString;
  function _GetDISPLAY_MODE_GRID: Integer;
  function _GetDISPLAY_MODE_LIST: Integer;
  function _GetEXTRA_LIVE_FOLDER_BASE_INTENT: JString;
  function _GetEXTRA_LIVE_FOLDER_DISPLAY_MODE: JString;
  function _GetEXTRA_LIVE_FOLDER_ICON: JString;
  function _GetEXTRA_LIVE_FOLDER_NAME: JString;
  function _GetICON_BITMAP: JString;
  function _GetICON_PACKAGE: JString;
  function _GetICON_RESOURCE: JString;
  function _GetINTENT: JString;
  function _GetNAME: JString;
  {Properties}
  property ACTION_CREATE_LIVE_FOLDER: JString read _GetACTION_CREATE_LIVE_FOLDER;
  property DESCRIPTION: JString read _GetDESCRIPTION;
  property DISPLAY_MODE_GRID: Integer read _GetDISPLAY_MODE_GRID;
  property DISPLAY_MODE_LIST: Integer read _GetDISPLAY_MODE_LIST;
  property EXTRA_LIVE_FOLDER_BASE_INTENT: JString read _GetEXTRA_LIVE_FOLDER_BASE_INTENT;
  property EXTRA_LIVE_FOLDER_DISPLAY_MODE: JString read _GetEXTRA_LIVE_FOLDER_DISPLAY_MODE;
  property EXTRA_LIVE_FOLDER_ICON: JString read _GetEXTRA_LIVE_FOLDER_ICON;
  property EXTRA_LIVE_FOLDER_NAME: JString read _GetEXTRA_LIVE_FOLDER_NAME;
  property ICON_BITMAP: JString read _GetICON_BITMAP;
  property ICON_PACKAGE: JString read _GetICON_PACKAGE;
  property ICON_RESOURCE: JString read _GetICON_RESOURCE;
  property INTENT: JString read _GetINTENT;
  property NAME: JString read _GetNAME;
end;

[JavaSignature('android/provider/LiveFolders')]
JLiveFolders = interface(JObject)
['{DBB7EE2D-C583-4C1E-805C-AE62526AE6D7}']
end;
TJLiveFolders = class(TJavaGenericImport<JLiveFoldersClass, JLiveFolders>) end;

JCommonDataKinds_PhoneClass = interface(JObjectClass)
['{26C70162-6E79-45BB-9BE5-6C1EF293FD07}']
  {Property Methods}
  function _GetCONTENT_FILTER_URI: Jnet_Uri;
  function _GetCONTENT_ITEM_TYPE: JString;
  function _GetCONTENT_TYPE: JString;
  function _GetCONTENT_URI: Jnet_Uri;
  function _GetNORMALIZED_NUMBER: JString;
  function _GetNUMBER: JString;
  function _GetSEARCH_DISPLAY_NAME_KEY: JString;
  function _GetSEARCH_PHONE_NUMBER_KEY: JString;
  function _GetTYPE_ASSISTANT: Integer;
  function _GetTYPE_CALLBACK: Integer;
  function _GetTYPE_CAR: Integer;
  function _GetTYPE_COMPANY_MAIN: Integer;
  function _GetTYPE_FAX_HOME: Integer;
  function _GetTYPE_FAX_WORK: Integer;
  function _GetTYPE_HOME: Integer;
  function _GetTYPE_ISDN: Integer;
  function _GetTYPE_MAIN: Integer;
  function _GetTYPE_MMS: Integer;
  function _GetTYPE_MOBILE: Integer;
  function _GetTYPE_OTHER: Integer;
  function _GetTYPE_OTHER_FAX: Integer;
  function _GetTYPE_PAGER: Integer;
  function _GetTYPE_RADIO: Integer;
  function _GetTYPE_TELEX: Integer;
  function _GetTYPE_TTY_TDD: Integer;
  function _GetTYPE_WORK: Integer;
  function _GetTYPE_WORK_MOBILE: Integer;
  function _GetTYPE_WORK_PAGER: Integer;
  {Methods}
  function getTypeLabel(res: JResources; type_: Integer; label_: JCharSequence): JCharSequence; cdecl;
  function getTypeLabelResource(type_: Integer): Integer; cdecl;
  {Properties}
  property CONTENT_FILTER_URI: Jnet_Uri read _GetCONTENT_FILTER_URI;
  property CONTENT_ITEM_TYPE: JString read _GetCONTENT_ITEM_TYPE;
  property CONTENT_TYPE: JString read _GetCONTENT_TYPE;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
  property NORMALIZED_NUMBER: JString read _GetNORMALIZED_NUMBER;
  property NUMBER: JString read _GetNUMBER;
  property SEARCH_DISPLAY_NAME_KEY: JString read _GetSEARCH_DISPLAY_NAME_KEY;
  property SEARCH_PHONE_NUMBER_KEY: JString read _GetSEARCH_PHONE_NUMBER_KEY;
  property TYPE_ASSISTANT: Integer read _GetTYPE_ASSISTANT;
  property TYPE_CALLBACK: Integer read _GetTYPE_CALLBACK;
  property TYPE_CAR: Integer read _GetTYPE_CAR;
  property TYPE_COMPANY_MAIN: Integer read _GetTYPE_COMPANY_MAIN;
  property TYPE_FAX_HOME: Integer read _GetTYPE_FAX_HOME;
  property TYPE_FAX_WORK: Integer read _GetTYPE_FAX_WORK;
  property TYPE_HOME: Integer read _GetTYPE_HOME;
  property TYPE_ISDN: Integer read _GetTYPE_ISDN;
  property TYPE_MAIN: Integer read _GetTYPE_MAIN;
  property TYPE_MMS: Integer read _GetTYPE_MMS;
  property TYPE_MOBILE: Integer read _GetTYPE_MOBILE;
  property TYPE_OTHER: Integer read _GetTYPE_OTHER;
  property TYPE_OTHER_FAX: Integer read _GetTYPE_OTHER_FAX;
  property TYPE_PAGER: Integer read _GetTYPE_PAGER;
  property TYPE_RADIO: Integer read _GetTYPE_RADIO;
  property TYPE_TELEX: Integer read _GetTYPE_TELEX;
  property TYPE_TTY_TDD: Integer read _GetTYPE_TTY_TDD;
  property TYPE_WORK: Integer read _GetTYPE_WORK;
  property TYPE_WORK_MOBILE: Integer read _GetTYPE_WORK_MOBILE;
  property TYPE_WORK_PAGER: Integer read _GetTYPE_WORK_PAGER;
end;

[JavaSignature('android/provider/ContactsContract$CommonDataKinds$Phone')]
JCommonDataKinds_Phone = interface(JObject)
['{B3BC4EC6-2FEB-4F10-9D45-275FDE754A78}']
end;
TJCommonDataKinds_Phone = class(TJavaGenericImport<JCommonDataKinds_PhoneClass, JCommonDataKinds_Phone>) end;

JSyncStateContract_HelpersClass = interface(JObjectClass)
['{4B4C89EA-788B-4E3C-90B9-E56A611ED30A}']
  {Methods}
  function init: JSyncStateContract_Helpers; cdecl;
  function newUpdateOperation(uri: Jnet_Uri; data: TJavaArray<Byte>): JContentProviderOperation; cdecl;
  procedure update(provider: JContentProviderClient; uri: Jnet_Uri; data: TJavaArray<Byte>); cdecl;
end;

[JavaSignature('android/provider/SyncStateContract$Helpers')]
JSyncStateContract_Helpers = interface(JObject)
['{C649769F-D61C-406E-A8E9-2E2E0B77CD28}']
end;
TJSyncStateContract_Helpers = class(TJavaGenericImport<JSyncStateContract_HelpersClass, JSyncStateContract_Helpers>) end;

JContactsContract_PhoneticNameStyleClass = interface(IJavaClass)
['{6A9E8D5F-60FD-4102-A949-55783262E4C9}']
  {Property Methods}
  function _GetJAPANESE: Integer;
  function _GetKOREAN: Integer;
  function _GetPINYIN: Integer;
  function _GetUNDEFINED: Integer;
  {Properties}
  property JAPANESE: Integer read _GetJAPANESE;
  property KOREAN: Integer read _GetKOREAN;
  property PINYIN: Integer read _GetPINYIN;
  property UNDEFINED: Integer read _GetUNDEFINED;
end;

[JavaSignature('android/provider/ContactsContract$PhoneticNameStyle')]
JContactsContract_PhoneticNameStyle = interface(IJavaInstance)
['{57AC607C-87D6-4766-9EFF-C3144DA285C9}']
end;
TJContactsContract_PhoneticNameStyle = class(TJavaGenericImport<JContactsContract_PhoneticNameStyleClass, JContactsContract_PhoneticNameStyle>) end;

JContactsContract_RawContactsClass = interface(JObjectClass)
['{E0D65538-AB84-4AD8-A420-4BD244B76A83}']
  {Property Methods}
  function _GetAGGREGATION_MODE_DEFAULT: Integer;
  function _GetAGGREGATION_MODE_DISABLED: Integer;
  function _GetAGGREGATION_MODE_IMMEDIATE: Integer;
  function _GetAGGREGATION_MODE_SUSPENDED: Integer;
  function _GetCONTENT_ITEM_TYPE: JString;
  function _GetCONTENT_TYPE: JString;
  function _GetCONTENT_URI: Jnet_Uri;
  {Methods}
  function getContactLookupUri(resolver: JContentResolver; rawContactUri: Jnet_Uri): Jnet_Uri; cdecl;
  function newEntityIterator(cursor: JCursor): JEntityIterator; cdecl;
  {Properties}
  property AGGREGATION_MODE_DEFAULT: Integer read _GetAGGREGATION_MODE_DEFAULT;
  property AGGREGATION_MODE_DISABLED: Integer read _GetAGGREGATION_MODE_DISABLED;
  property AGGREGATION_MODE_IMMEDIATE: Integer read _GetAGGREGATION_MODE_IMMEDIATE;
  property AGGREGATION_MODE_SUSPENDED: Integer read _GetAGGREGATION_MODE_SUSPENDED;
  property CONTENT_ITEM_TYPE: JString read _GetCONTENT_ITEM_TYPE;
  property CONTENT_TYPE: JString read _GetCONTENT_TYPE;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
end;

[JavaSignature('android/provider/ContactsContract$RawContacts')]
JContactsContract_RawContacts = interface(JObject)
['{E594CABA-4493-4DDB-89CA-5DC683D8EE69}']
end;
TJContactsContract_RawContacts = class(TJavaGenericImport<JContactsContract_RawContactsClass, JContactsContract_RawContacts>) end;

JCalendarContract_CalendarEntityClass = interface(JObjectClass)
['{434CBEE7-C980-4F19-8785-4807E9729F4F}']
  {Property Methods}
  function _GetCONTENT_URI: Jnet_Uri;
  {Methods}
  function newEntityIterator(cursor: JCursor): JEntityIterator; cdecl;
  {Properties}
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
end;

[JavaSignature('android/provider/CalendarContract$CalendarEntity')]
JCalendarContract_CalendarEntity = interface(JObject)
['{B684C007-08AE-4C6D-812C-1F072BFB2CB7}']
end;
TJCalendarContract_CalendarEntity = class(TJavaGenericImport<JCalendarContract_CalendarEntityClass, JCalendarContract_CalendarEntity>) end;

JVideo_VideoColumnsClass = interface(JMediaStore_MediaColumnsClass)
['{A2BE0519-5009-47E0-AA45-28A55F103AB9}']
  {Property Methods}
  function _GetALBUM: JString;
  function _GetARTIST: JString;
  function _GetBOOKMARK: JString;
  function _GetBUCKET_DISPLAY_NAME: JString;
  function _GetBUCKET_ID: JString;
  function _GetCATEGORY: JString;
  function _GetDATE_TAKEN: JString;
  function _GetDESCRIPTION: JString;
  function _GetDURATION: JString;
  function _GetIS_PRIVATE: JString;
  function _GetLANGUAGE: JString;
  function _GetLATITUDE: JString;
  function _GetLONGITUDE: JString;
  function _GetMINI_THUMB_MAGIC: JString;
  function _GetRESOLUTION: JString;
  function _GetTAGS: JString;
  {Properties}
  property ALBUM: JString read _GetALBUM;
  property ARTIST: JString read _GetARTIST;
  property BOOKMARK: JString read _GetBOOKMARK;
  property BUCKET_DISPLAY_NAME: JString read _GetBUCKET_DISPLAY_NAME;
  property BUCKET_ID: JString read _GetBUCKET_ID;
  property CATEGORY: JString read _GetCATEGORY;
  property DATE_TAKEN: JString read _GetDATE_TAKEN;
  property DESCRIPTION: JString read _GetDESCRIPTION;
  property DURATION: JString read _GetDURATION;
  property IS_PRIVATE: JString read _GetIS_PRIVATE;
  property LANGUAGE: JString read _GetLANGUAGE;
  property LATITUDE: JString read _GetLATITUDE;
  property LONGITUDE: JString read _GetLONGITUDE;
  property MINI_THUMB_MAGIC: JString read _GetMINI_THUMB_MAGIC;
  property RESOLUTION: JString read _GetRESOLUTION;
  property TAGS: JString read _GetTAGS;
end;

[JavaSignature('android/provider/MediaStore$Video$VideoColumns')]
JVideo_VideoColumns = interface(JMediaStore_MediaColumns)
['{79F06E41-8BA5-483A-93F2-11E1CE4186F2}']
end;
TJVideo_VideoColumns = class(TJavaGenericImport<JVideo_VideoColumnsClass, JVideo_VideoColumns>) end;

JContactsContractClass = interface(JObjectClass)
['{CFD9D03D-AEFB-488B-B966-5CFEC3CF916B}']
  {Property Methods}
  function _GetAUTHORITY: JString;
  function _GetAUTHORITY_URI: Jnet_Uri;
  function _GetCALLER_IS_SYNCADAPTER: JString;
  function _GetDIRECTORY_PARAM_KEY: JString;
  function _GetLIMIT_PARAM_KEY: JString;
  function _GetPRIMARY_ACCOUNT_NAME: JString;
  function _GetPRIMARY_ACCOUNT_TYPE: JString;
  {Methods}
  function init: JContactsContract; cdecl;
  function isProfileId(id: Int64): Boolean; cdecl;
  {Properties}
  property AUTHORITY: JString read _GetAUTHORITY;
  property AUTHORITY_URI: Jnet_Uri read _GetAUTHORITY_URI;
  property CALLER_IS_SYNCADAPTER: JString read _GetCALLER_IS_SYNCADAPTER;
  property DIRECTORY_PARAM_KEY: JString read _GetDIRECTORY_PARAM_KEY;
  property LIMIT_PARAM_KEY: JString read _GetLIMIT_PARAM_KEY;
  property PRIMARY_ACCOUNT_NAME: JString read _GetPRIMARY_ACCOUNT_NAME;
  property PRIMARY_ACCOUNT_TYPE: JString read _GetPRIMARY_ACCOUNT_TYPE;
end;

[JavaSignature('android/provider/ContactsContract')]
JContactsContract = interface(JObject)
['{906F2B5A-9759-4CC8-B2B5-D9693E60778C}']
end;
TJContactsContract = class(TJavaGenericImport<JContactsContractClass, JContactsContract>) end;

JContacts_PhotosClass = interface(JObjectClass)
['{27A63125-9A2B-4C7B-94C8-B29146841362}']
  {Property Methods}
  function _GetCONTENT_DIRECTORY: JString;
  function _GetCONTENT_URI: Jnet_Uri;
  function _GetDEFAULT_SORT_ORDER: JString;
  {Properties}
  property CONTENT_DIRECTORY: JString read _GetCONTENT_DIRECTORY;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
  property DEFAULT_SORT_ORDER: JString read _GetDEFAULT_SORT_ORDER;
end;

[JavaSignature('android/provider/Contacts$Photos')]
JContacts_Photos = interface(JObject)
['{5498AAAC-BE8D-4BD4-9F39-F351C7F85213}']
end;
TJContacts_Photos = class(TJavaGenericImport<JContacts_PhotosClass, JContacts_Photos>) end;

JContacts_PhonesClass = interface(JObjectClass)
['{CF5F27C1-2C0C-4EAC-A328-BB28D0EF7D48}']
  {Property Methods}
  function _GetCONTENT_FILTER_URL: Jnet_Uri;
  function _GetCONTENT_ITEM_TYPE: JString;
  function _GetCONTENT_TYPE: JString;
  function _GetCONTENT_URI: Jnet_Uri;
  function _GetDEFAULT_SORT_ORDER: JString;
  function _GetPERSON_ID: JString;
  {Methods}
  function getDisplayLabel(context: JContext; type_: Integer; label_: JCharSequence; labelArray: TJavaObjectArray<JCharSequence>): JCharSequence; cdecl; overload;//Deprecated
  function getDisplayLabel(context: JContext; type_: Integer; label_: JCharSequence): JCharSequence; cdecl; overload;//Deprecated
  {Properties}
  property CONTENT_FILTER_URL: Jnet_Uri read _GetCONTENT_FILTER_URL;
  property CONTENT_ITEM_TYPE: JString read _GetCONTENT_ITEM_TYPE;
  property CONTENT_TYPE: JString read _GetCONTENT_TYPE;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
  property DEFAULT_SORT_ORDER: JString read _GetDEFAULT_SORT_ORDER;
  property PERSON_ID: JString read _GetPERSON_ID;
end;

[JavaSignature('android/provider/Contacts$Phones')]
JContacts_Phones = interface(JObject)
['{AE4C71A4-CE42-4B53-B595-62274D7123D2}']
end;
TJContacts_Phones = class(TJavaGenericImport<JContacts_PhonesClass, JContacts_Phones>) end;

JCalendarContract_ExtendedPropertiesClass = interface(JObjectClass)
['{D2DF19F5-C43F-4701-AF44-2869FFBDDFEF}']
  {Property Methods}
  function _GetCONTENT_URI: Jnet_Uri;
  {Properties}
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
end;

[JavaSignature('android/provider/CalendarContract$ExtendedProperties')]
JCalendarContract_ExtendedProperties = interface(JObject)
['{90564D52-DB23-4CE1-8692-5056843DCD9D}']
end;
TJCalendarContract_ExtendedProperties = class(TJavaGenericImport<JCalendarContract_ExtendedPropertiesClass, JCalendarContract_ExtendedProperties>) end;

JCommonDataKinds_StructuredNameClass = interface(JObjectClass)
['{F784BAC1-A531-4018-8FCA-A37D5DEB6B3D}']
  {Property Methods}
  function _GetCONTENT_ITEM_TYPE: JString;
  function _GetDISPLAY_NAME: JString;
  function _GetFAMILY_NAME: JString;
  function _GetGIVEN_NAME: JString;
  function _GetMIDDLE_NAME: JString;
  function _GetPHONETIC_FAMILY_NAME: JString;
  function _GetPHONETIC_GIVEN_NAME: JString;
  function _GetPHONETIC_MIDDLE_NAME: JString;
  function _GetPREFIX: JString;
  function _GetSUFFIX: JString;
  {Properties}
  property CONTENT_ITEM_TYPE: JString read _GetCONTENT_ITEM_TYPE;
  property DISPLAY_NAME: JString read _GetDISPLAY_NAME;
  property FAMILY_NAME: JString read _GetFAMILY_NAME;
  property GIVEN_NAME: JString read _GetGIVEN_NAME;
  property MIDDLE_NAME: JString read _GetMIDDLE_NAME;
  property PHONETIC_FAMILY_NAME: JString read _GetPHONETIC_FAMILY_NAME;
  property PHONETIC_GIVEN_NAME: JString read _GetPHONETIC_GIVEN_NAME;
  property PHONETIC_MIDDLE_NAME: JString read _GetPHONETIC_MIDDLE_NAME;
  property PREFIX: JString read _GetPREFIX;
  property SUFFIX: JString read _GetSUFFIX;
end;

[JavaSignature('android/provider/ContactsContract$CommonDataKinds$StructuredName')]
JCommonDataKinds_StructuredName = interface(JObject)
['{94F6949A-A527-4FF2-97D3-0A214660CFDD}']
end;
TJCommonDataKinds_StructuredName = class(TJavaGenericImport<JCommonDataKinds_StructuredNameClass, JCommonDataKinds_StructuredName>) end;

JContactsContract_StreamItemPhotosClass = interface(JObjectClass)
['{55E9E387-5C6F-47F0-8863-5193AEB84B20}']
  {Property Methods}
  function _GetPHOTO: JString;
  {Properties}
  property PHOTO: JString read _GetPHOTO;
end;

[JavaSignature('android/provider/ContactsContract$StreamItemPhotos')]
JContactsContract_StreamItemPhotos = interface(JObject)
['{560E9727-2B3F-458D-9B4C-E677C9F76C32}']
end;
TJContactsContract_StreamItemPhotos = class(TJavaGenericImport<JContactsContract_StreamItemPhotosClass, JContactsContract_StreamItemPhotos>) end;

JCommonDataKinds_SipAddressClass = interface(JObjectClass)
['{80EDB7D1-1410-41F1-99B1-62711133DD8C}']
  {Property Methods}
  function _GetCONTENT_ITEM_TYPE: JString;
  function _GetSIP_ADDRESS: JString;
  function _GetTYPE_HOME: Integer;
  function _GetTYPE_OTHER: Integer;
  function _GetTYPE_WORK: Integer;
  {Methods}
  function getTypeLabel(res: JResources; type_: Integer; label_: JCharSequence): JCharSequence; cdecl;
  function getTypeLabelResource(type_: Integer): Integer; cdecl;
  {Properties}
  property CONTENT_ITEM_TYPE: JString read _GetCONTENT_ITEM_TYPE;
  property SIP_ADDRESS: JString read _GetSIP_ADDRESS;
  property TYPE_HOME: Integer read _GetTYPE_HOME;
  property TYPE_OTHER: Integer read _GetTYPE_OTHER;
  property TYPE_WORK: Integer read _GetTYPE_WORK;
end;

[JavaSignature('android/provider/ContactsContract$CommonDataKinds$SipAddress')]
JCommonDataKinds_SipAddress = interface(JObject)
['{A14B21AA-B8CC-404C-85FA-2B34F0A8C064}']
end;
TJCommonDataKinds_SipAddress = class(TJavaGenericImport<JCommonDataKinds_SipAddressClass, JCommonDataKinds_SipAddress>) end;

JBrowserClass = interface(JObjectClass)
['{E886757C-AC6C-4EB5-9A05-E0F0A02027AC}']
  {Property Methods}
  function _GetBOOKMARKS_URI: Jnet_Uri;
  function _GetEXTRA_APPLICATION_ID: JString;
  function _GetEXTRA_CREATE_NEW_TAB: JString;
  function _GetEXTRA_HEADERS: JString;
  function _GetHISTORY_PROJECTION: TJavaObjectArray<JString>;
  function _GetHISTORY_PROJECTION_BOOKMARK_INDEX: Integer;
  function _GetHISTORY_PROJECTION_DATE_INDEX: Integer;
  function _GetHISTORY_PROJECTION_FAVICON_INDEX: Integer;
  function _GetHISTORY_PROJECTION_ID_INDEX: Integer;
  function _GetHISTORY_PROJECTION_TITLE_INDEX: Integer;
  function _GetHISTORY_PROJECTION_URL_INDEX: Integer;
  function _GetHISTORY_PROJECTION_VISITS_INDEX: Integer;
  function _GetINITIAL_ZOOM_LEVEL: JString;
  function _GetSEARCHES_PROJECTION: TJavaObjectArray<JString>;
  function _GetSEARCHES_PROJECTION_DATE_INDEX: Integer;
  function _GetSEARCHES_PROJECTION_SEARCH_INDEX: Integer;
  function _GetSEARCHES_URI: Jnet_Uri;
  function _GetTRUNCATE_HISTORY_PROJECTION: TJavaObjectArray<JString>;
  function _GetTRUNCATE_HISTORY_PROJECTION_ID_INDEX: Integer;
  function _GetTRUNCATE_N_OLDEST: Integer;
  {Methods}
  function init: JBrowser; cdecl;
  procedure addSearchUrl(cr: JContentResolver; search: JString); cdecl;
  function canClearHistory(cr: JContentResolver): Boolean; cdecl;
  procedure clearHistory(cr: JContentResolver); cdecl;
  procedure clearSearches(cr: JContentResolver); cdecl;
  procedure deleteFromHistory(cr: JContentResolver; url: JString); cdecl;
  procedure deleteHistoryTimeFrame(cr: JContentResolver; begin_: Int64; end_: Int64); cdecl;
  function getAllBookmarks(cr: JContentResolver): JCursor; cdecl;
  function getAllVisitedUrls(cr: JContentResolver): JCursor; cdecl;
  procedure requestAllIcons(cr: JContentResolver; where: JString; listener: JWebIconDatabase_IconListener); cdecl;
  procedure saveBookmark(c: JContext; title: JString; url: JString); cdecl;
  procedure sendString(context: JContext; string_: JString); cdecl;
  procedure truncateHistory(cr: JContentResolver); cdecl;
  procedure updateVisitedHistory(cr: JContentResolver; url: JString; real: Boolean); cdecl;
  {Properties}
  property BOOKMARKS_URI: Jnet_Uri read _GetBOOKMARKS_URI;
  property EXTRA_APPLICATION_ID: JString read _GetEXTRA_APPLICATION_ID;
  property EXTRA_CREATE_NEW_TAB: JString read _GetEXTRA_CREATE_NEW_TAB;
  property EXTRA_HEADERS: JString read _GetEXTRA_HEADERS;
  property HISTORY_PROJECTION: TJavaObjectArray<JString> read _GetHISTORY_PROJECTION;
  property HISTORY_PROJECTION_BOOKMARK_INDEX: Integer read _GetHISTORY_PROJECTION_BOOKMARK_INDEX;
  property HISTORY_PROJECTION_DATE_INDEX: Integer read _GetHISTORY_PROJECTION_DATE_INDEX;
  property HISTORY_PROJECTION_FAVICON_INDEX: Integer read _GetHISTORY_PROJECTION_FAVICON_INDEX;
  property HISTORY_PROJECTION_ID_INDEX: Integer read _GetHISTORY_PROJECTION_ID_INDEX;
  property HISTORY_PROJECTION_TITLE_INDEX: Integer read _GetHISTORY_PROJECTION_TITLE_INDEX;
  property HISTORY_PROJECTION_URL_INDEX: Integer read _GetHISTORY_PROJECTION_URL_INDEX;
  property HISTORY_PROJECTION_VISITS_INDEX: Integer read _GetHISTORY_PROJECTION_VISITS_INDEX;
  property INITIAL_ZOOM_LEVEL: JString read _GetINITIAL_ZOOM_LEVEL;
  property SEARCHES_PROJECTION: TJavaObjectArray<JString> read _GetSEARCHES_PROJECTION;
  property SEARCHES_PROJECTION_DATE_INDEX: Integer read _GetSEARCHES_PROJECTION_DATE_INDEX;
  property SEARCHES_PROJECTION_SEARCH_INDEX: Integer read _GetSEARCHES_PROJECTION_SEARCH_INDEX;
  property SEARCHES_URI: Jnet_Uri read _GetSEARCHES_URI;
  property TRUNCATE_HISTORY_PROJECTION: TJavaObjectArray<JString> read _GetTRUNCATE_HISTORY_PROJECTION;
  property TRUNCATE_HISTORY_PROJECTION_ID_INDEX: Integer read _GetTRUNCATE_HISTORY_PROJECTION_ID_INDEX;
  property TRUNCATE_N_OLDEST: Integer read _GetTRUNCATE_N_OLDEST;
end;

[JavaSignature('android/provider/Browser')]
JBrowser = interface(JObject)
['{28900B47-F2A9-496E-AEA4-BE635BEFEF9B}']
end;
TJBrowser = class(TJavaGenericImport<JBrowserClass, JBrowser>) end;

JCalendarContract_RemindersClass = interface(JObjectClass)
['{BA36FD20-EC66-4D37-ADDA-3C031DF34C5B}']
  {Property Methods}
  function _GetCONTENT_URI: Jnet_Uri;
  {Methods}
  function query(cr: JContentResolver; eventId: Int64; projection: TJavaObjectArray<JString>): JCursor; cdecl;
  {Properties}
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
end;

[JavaSignature('android/provider/CalendarContract$Reminders')]
JCalendarContract_Reminders = interface(JObject)
['{EA76BFBF-0882-453C-8447-CC92FEEBCCE5}']
end;
TJCalendarContract_Reminders = class(TJavaGenericImport<JCalendarContract_RemindersClass, JCalendarContract_Reminders>) end;

JContactsContract_StatusUpdatesClass = interface(JObjectClass)
['{EB026425-5A27-43D5-BB4F-5729721A88DF}']
  {Property Methods}
  function _GetCONTENT_ITEM_TYPE: JString;
  function _GetCONTENT_TYPE: JString;
  function _GetCONTENT_URI: Jnet_Uri;
  function _GetPROFILE_CONTENT_URI: Jnet_Uri;
  {Methods}
  function getPresenceIconResourceId(status: Integer): Integer; cdecl;
  function getPresencePrecedence(status: Integer): Integer; cdecl;
  {Properties}
  property CONTENT_ITEM_TYPE: JString read _GetCONTENT_ITEM_TYPE;
  property CONTENT_TYPE: JString read _GetCONTENT_TYPE;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
  property PROFILE_CONTENT_URI: Jnet_Uri read _GetPROFILE_CONTENT_URI;
end;

[JavaSignature('android/provider/ContactsContract$StatusUpdates')]
JContactsContract_StatusUpdates = interface(JObject)
['{EFB001B8-804C-4D5B-8814-BF9153DB7EAF}']
end;
TJContactsContract_StatusUpdates = class(TJavaGenericImport<JContactsContract_StatusUpdatesClass, JContactsContract_StatusUpdates>) end;

JContactsContract_FullNameStyleClass = interface(IJavaClass)
['{C4D11ABF-9A3E-4AD8-A8BD-FC2943ED20B9}']
  {Property Methods}
  function _GetCHINESE: Integer;
  function _GetCJK: Integer;
  function _GetJAPANESE: Integer;
  function _GetKOREAN: Integer;
  function _GetUNDEFINED: Integer;
  function _GetWESTERN: Integer;
  {Properties}
  property CHINESE: Integer read _GetCHINESE;
  property CJK: Integer read _GetCJK;
  property JAPANESE: Integer read _GetJAPANESE;
  property KOREAN: Integer read _GetKOREAN;
  property UNDEFINED: Integer read _GetUNDEFINED;
  property WESTERN: Integer read _GetWESTERN;
end;

[JavaSignature('android/provider/ContactsContract$FullNameStyle')]
JContactsContract_FullNameStyle = interface(IJavaInstance)
['{3E6883E0-BF39-4589-B1F9-4A2D261FF009}']
end;
TJContactsContract_FullNameStyle = class(TJavaGenericImport<JContactsContract_FullNameStyleClass, JContactsContract_FullNameStyle>) end;

JAlarmClockClass = interface(JObjectClass)
['{966CFFB3-96A0-42EE-84D7-89AED6C9DBA7}']
  {Property Methods}
  function _GetACTION_SET_ALARM: JString;
  function _GetEXTRA_HOUR: JString;
  function _GetEXTRA_MESSAGE: JString;
  function _GetEXTRA_MINUTES: JString;
  function _GetEXTRA_SKIP_UI: JString;
  {Methods}
  function init: JAlarmClock; cdecl;
  {Properties}
  property ACTION_SET_ALARM: JString read _GetACTION_SET_ALARM;
  property EXTRA_HOUR: JString read _GetEXTRA_HOUR;
  property EXTRA_MESSAGE: JString read _GetEXTRA_MESSAGE;
  property EXTRA_MINUTES: JString read _GetEXTRA_MINUTES;
  property EXTRA_SKIP_UI: JString read _GetEXTRA_SKIP_UI;
end;

[JavaSignature('android/provider/AlarmClock')]
JAlarmClock = interface(JObject)
['{4B84B481-1CF9-484F-9F66-F9378600D231}']
end;
TJAlarmClock = class(TJavaGenericImport<JAlarmClockClass, JAlarmClock>) end;

JSettings_SettingNotFoundExceptionClass = interface(JAndroidExceptionClass)
['{5CE395AE-F059-4125-9F72-3B29390BE29D}']
  {Methods}
  function init(msg: JString): JSettings_SettingNotFoundException; cdecl;
end;

[JavaSignature('android/provider/Settings$SettingNotFoundException')]
JSettings_SettingNotFoundException = interface(JAndroidException)
['{1E398CF0-857F-40AC-A940-02474CAFA0AE}']
end;
TJSettings_SettingNotFoundException = class(TJavaGenericImport<JSettings_SettingNotFoundExceptionClass, JSettings_SettingNotFoundException>) end;

JAudio_MediaClass = interface(JObjectClass)
['{091F5BE5-A7B0-4613-875B-50FD1ECDCB21}']
  {Property Methods}
  function _GetCONTENT_TYPE: JString;
  function _GetDEFAULT_SORT_ORDER: JString;
  function _GetEXTERNAL_CONTENT_URI: Jnet_Uri;
  function _GetEXTRA_MAX_BYTES: JString;
  function _GetINTERNAL_CONTENT_URI: Jnet_Uri;
  function _GetRECORD_SOUND_ACTION: JString;
  {Methods}
  function init: JAudio_Media; cdecl;
  function getContentUri(volumeName: JString): Jnet_Uri; cdecl;
  function getContentUriForPath(path: JString): Jnet_Uri; cdecl;
  {Properties}
  property CONTENT_TYPE: JString read _GetCONTENT_TYPE;
  property DEFAULT_SORT_ORDER: JString read _GetDEFAULT_SORT_ORDER;
  property EXTERNAL_CONTENT_URI: Jnet_Uri read _GetEXTERNAL_CONTENT_URI;
  property EXTRA_MAX_BYTES: JString read _GetEXTRA_MAX_BYTES;
  property INTERNAL_CONTENT_URI: Jnet_Uri read _GetINTERNAL_CONTENT_URI;
  property RECORD_SOUND_ACTION: JString read _GetRECORD_SOUND_ACTION;
end;

[JavaSignature('android/provider/MediaStore$Audio$Media')]
JAudio_Media = interface(JObject)
['{9071FFB7-40D5-4090-AA01-51DD92852D3D}']
end;
TJAudio_Media = class(TJavaGenericImport<JAudio_MediaClass, JAudio_Media>) end;

JContacts_GroupMembershipClass = interface(JObjectClass)
['{C5CE55C3-D97D-49CB-9D8A-6462647A7D11}']
  {Property Methods}
  function _GetCONTENT_DIRECTORY: JString;
  function _GetCONTENT_ITEM_TYPE: JString;
  function _GetCONTENT_TYPE: JString;
  function _GetCONTENT_URI: Jnet_Uri;
  function _GetDEFAULT_SORT_ORDER: JString;
  function _GetGROUP_ID: JString;
  function _GetGROUP_SYNC_ACCOUNT: JString;
  function _GetGROUP_SYNC_ACCOUNT_TYPE: JString;
  function _GetGROUP_SYNC_ID: JString;
  function _GetPERSON_ID: JString;
  function _GetRAW_CONTENT_URI: Jnet_Uri;
  {Properties}
  property CONTENT_DIRECTORY: JString read _GetCONTENT_DIRECTORY;
  property CONTENT_ITEM_TYPE: JString read _GetCONTENT_ITEM_TYPE;
  property CONTENT_TYPE: JString read _GetCONTENT_TYPE;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
  property DEFAULT_SORT_ORDER: JString read _GetDEFAULT_SORT_ORDER;
  property GROUP_ID: JString read _GetGROUP_ID;
  property GROUP_SYNC_ACCOUNT: JString read _GetGROUP_SYNC_ACCOUNT;
  property GROUP_SYNC_ACCOUNT_TYPE: JString read _GetGROUP_SYNC_ACCOUNT_TYPE;
  property GROUP_SYNC_ID: JString read _GetGROUP_SYNC_ID;
  property PERSON_ID: JString read _GetPERSON_ID;
  property RAW_CONTENT_URI: Jnet_Uri read _GetRAW_CONTENT_URI;
end;

[JavaSignature('android/provider/Contacts$GroupMembership')]
JContacts_GroupMembership = interface(JObject)
['{E374AA3B-4721-48DE-AA8D-29DDA3425586}']
end;
TJContacts_GroupMembership = class(TJavaGenericImport<JContacts_GroupMembershipClass, JContacts_GroupMembership>) end;

JCommonDataKinds_IdentityClass = interface(JObjectClass)
['{C3B1DA70-10FF-4B3C-9D64-A563B10F011A}']
  {Property Methods}
  function _GetCONTENT_ITEM_TYPE: JString;
  function _GetIDENTITY: JString;
  function _GetNAMESPACE: JString;
  {Properties}
  property CONTENT_ITEM_TYPE: JString read _GetCONTENT_ITEM_TYPE;
  property IDENTITY: JString read _GetIDENTITY;
  property NAMESPACE: JString read _GetNAMESPACE;
end;

[JavaSignature('android/provider/ContactsContract$CommonDataKinds$Identity')]
JCommonDataKinds_Identity = interface(JObject)
['{0B4C7B05-838D-4327-A2CF-80C35FD274C3}']
end;
TJCommonDataKinds_Identity = class(TJavaGenericImport<JCommonDataKinds_IdentityClass, JCommonDataKinds_Identity>) end;

JAudio_AlbumColumnsClass = interface(IJavaClass)
['{98AF12FB-6DE3-4F9E-8B06-8463A45D06E2}']
  {Property Methods}
  function _GetALBUM: JString;
  function _GetALBUM_ART: JString;
  function _GetALBUM_ID: JString;
  function _GetALBUM_KEY: JString;
  function _GetARTIST: JString;
  function _GetFIRST_YEAR: JString;
  function _GetLAST_YEAR: JString;
  function _GetNUMBER_OF_SONGS: JString;
  function _GetNUMBER_OF_SONGS_FOR_ARTIST: JString;
  {Properties}
  property ALBUM: JString read _GetALBUM;
  property ALBUM_ART: JString read _GetALBUM_ART;
  property ALBUM_ID: JString read _GetALBUM_ID;
  property ALBUM_KEY: JString read _GetALBUM_KEY;
  property ARTIST: JString read _GetARTIST;
  property FIRST_YEAR: JString read _GetFIRST_YEAR;
  property LAST_YEAR: JString read _GetLAST_YEAR;
  property NUMBER_OF_SONGS: JString read _GetNUMBER_OF_SONGS;
  property NUMBER_OF_SONGS_FOR_ARTIST: JString read _GetNUMBER_OF_SONGS_FOR_ARTIST;
end;

[JavaSignature('android/provider/MediaStore$Audio$AlbumColumns')]
JAudio_AlbumColumns = interface(IJavaInstance)
['{B212F20F-FD71-44F3-8D04-D8E75E357D47}']
end;
TJAudio_AlbumColumns = class(TJavaGenericImport<JAudio_AlbumColumnsClass, JAudio_AlbumColumns>) end;

JVoicemailContractClass = interface(JObjectClass)
['{C1ACEF9D-8E13-4C59-90AF-A0EC53AC2340}']
  {Property Methods}
  function _GetACTION_FETCH_VOICEMAIL: JString;
  function _GetACTION_NEW_VOICEMAIL: JString;
  function _GetAUTHORITY: JString;
  function _GetEXTRA_SELF_CHANGE: JString;
  function _GetPARAM_KEY_SOURCE_PACKAGE: JString;
  {Properties}
  property ACTION_FETCH_VOICEMAIL: JString read _GetACTION_FETCH_VOICEMAIL;
  property ACTION_NEW_VOICEMAIL: JString read _GetACTION_NEW_VOICEMAIL;
  property AUTHORITY: JString read _GetAUTHORITY;
  property EXTRA_SELF_CHANGE: JString read _GetEXTRA_SELF_CHANGE;
  property PARAM_KEY_SOURCE_PACKAGE: JString read _GetPARAM_KEY_SOURCE_PACKAGE;
end;

[JavaSignature('android/provider/VoicemailContract')]
JVoicemailContract = interface(JObject)
['{DA45A3AE-788A-4567-895D-972B3BF4DA89}']
end;
TJVoicemailContract = class(TJavaGenericImport<JVoicemailContractClass, JVoicemailContract>) end;

JImages_ImageColumnsClass = interface(JMediaStore_MediaColumnsClass)
['{05BF2BE0-ECA6-4780-8827-99C767171BE4}']
  {Property Methods}
  function _GetBUCKET_DISPLAY_NAME: JString;
  function _GetBUCKET_ID: JString;
  function _GetDATE_TAKEN: JString;
  function _GetDESCRIPTION: JString;
  function _GetIS_PRIVATE: JString;
  function _GetLATITUDE: JString;
  function _GetLONGITUDE: JString;
  function _GetMINI_THUMB_MAGIC: JString;
  function _GetORIENTATION: JString;
  function _GetPICASA_ID: JString;
  {Properties}
  property BUCKET_DISPLAY_NAME: JString read _GetBUCKET_DISPLAY_NAME;
  property BUCKET_ID: JString read _GetBUCKET_ID;
  property DATE_TAKEN: JString read _GetDATE_TAKEN;
  property DESCRIPTION: JString read _GetDESCRIPTION;
  property IS_PRIVATE: JString read _GetIS_PRIVATE;
  property LATITUDE: JString read _GetLATITUDE;
  property LONGITUDE: JString read _GetLONGITUDE;
  property MINI_THUMB_MAGIC: JString read _GetMINI_THUMB_MAGIC;
  property ORIENTATION: JString read _GetORIENTATION;
  property PICASA_ID: JString read _GetPICASA_ID;
end;

[JavaSignature('android/provider/MediaStore$Images$ImageColumns')]
JImages_ImageColumns = interface(JMediaStore_MediaColumns)
['{0098B778-4993-4F4C-89E8-214907928444}']
end;
TJImages_ImageColumns = class(TJavaGenericImport<JImages_ImageColumnsClass, JImages_ImageColumns>) end;

JCommonDataKinds_BaseTypesClass = interface(IJavaClass)
['{819B8EB6-DA73-4589-92AF-38EBBC858372}']
  {Property Methods}
  function _GetTYPE_CUSTOM: Integer;
  {Properties}
  property TYPE_CUSTOM: Integer read _GetTYPE_CUSTOM;
end;

[JavaSignature('android/provider/ContactsContract$CommonDataKinds$BaseTypes')]
JCommonDataKinds_BaseTypes = interface(IJavaInstance)
['{73411F43-0F68-4EB0-B294-A28A3D7228E2}']
end;
TJCommonDataKinds_BaseTypes = class(TJavaGenericImport<JCommonDataKinds_BaseTypesClass, JCommonDataKinds_BaseTypes>) end;

JContactsContract_DataUsageFeedbackClass = interface(JObjectClass)
['{E10A91B1-9740-49BC-A38D-CA014866272E}']
  {Property Methods}
  function _GetDELETE_USAGE_URI: Jnet_Uri;
  function _GetFEEDBACK_URI: Jnet_Uri;
  function _GetUSAGE_TYPE: JString;
  function _GetUSAGE_TYPE_CALL: JString;
  function _GetUSAGE_TYPE_LONG_TEXT: JString;
  function _GetUSAGE_TYPE_SHORT_TEXT: JString;
  {Methods}
  function init: JContactsContract_DataUsageFeedback; cdecl;
  {Properties}
  property DELETE_USAGE_URI: Jnet_Uri read _GetDELETE_USAGE_URI;
  property FEEDBACK_URI: Jnet_Uri read _GetFEEDBACK_URI;
  property USAGE_TYPE: JString read _GetUSAGE_TYPE;
  property USAGE_TYPE_CALL: JString read _GetUSAGE_TYPE_CALL;
  property USAGE_TYPE_LONG_TEXT: JString read _GetUSAGE_TYPE_LONG_TEXT;
  property USAGE_TYPE_SHORT_TEXT: JString read _GetUSAGE_TYPE_SHORT_TEXT;
end;

[JavaSignature('android/provider/ContactsContract$DataUsageFeedback')]
JContactsContract_DataUsageFeedback = interface(JObject)
['{CC44D7E3-874C-4E91-8C19-FAFA86384392}']
end;
TJContactsContract_DataUsageFeedback = class(TJavaGenericImport<JContactsContract_DataUsageFeedbackClass, JContactsContract_DataUsageFeedback>) end;

JCalendarContract_EventsClass = interface(JObjectClass)
['{6A3B6590-2744-4952-A83A-7EE65D628A32}']
  {Property Methods}
  function _GetCONTENT_EXCEPTION_URI: Jnet_Uri;
  function _GetCONTENT_URI: Jnet_Uri;
  {Properties}
  property CONTENT_EXCEPTION_URI: Jnet_Uri read _GetCONTENT_EXCEPTION_URI;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
end;

[JavaSignature('android/provider/CalendarContract$Events')]
JCalendarContract_Events = interface(JObject)
['{7F79FF44-33D2-4066-A899-E599A9D0A4A7}']
end;
TJCalendarContract_Events = class(TJavaGenericImport<JCalendarContract_EventsClass, JCalendarContract_Events>) end;

JContactsContract_RawContactsEntityClass = interface(JObjectClass)
['{83990A10-0BEC-444E-82CB-217C8B0B4EB7}']
  {Property Methods}
  function _GetCONTENT_TYPE: JString;
  function _GetCONTENT_URI: Jnet_Uri;
  function _GetDATA_ID: JString;
  function _GetPROFILE_CONTENT_URI: Jnet_Uri;
  {Properties}
  property CONTENT_TYPE: JString read _GetCONTENT_TYPE;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
  property DATA_ID: JString read _GetDATA_ID;
  property PROFILE_CONTENT_URI: Jnet_Uri read _GetPROFILE_CONTENT_URI;
end;

[JavaSignature('android/provider/ContactsContract$RawContactsEntity')]
JContactsContract_RawContactsEntity = interface(JObject)
['{05AE5B58-E000-4C2D-9956-5B775749E1FC}']
end;
TJContactsContract_RawContactsEntity = class(TJavaGenericImport<JContactsContract_RawContactsEntityClass, JContactsContract_RawContactsEntity>) end;

JContacts_ExtensionsColumnsClass = interface(IJavaClass)
['{F0C26E1A-5DA1-487E-BCC4-207D5860F239}']
  {Property Methods}
  function _GetNAME: JString;
  function _GetVALUE: JString;
  {Properties}
  property NAME: JString read _GetNAME;
  property VALUE: JString read _GetVALUE;
end;

[JavaSignature('android/provider/Contacts$ExtensionsColumns')]
JContacts_ExtensionsColumns = interface(IJavaInstance)
['{02CDF856-3141-48EA-B6B6-44B498BEB144}']
end;
TJContacts_ExtensionsColumns = class(TJavaGenericImport<JContacts_ExtensionsColumnsClass, JContacts_ExtensionsColumns>) end;

JAudio_ArtistColumnsClass = interface(IJavaClass)
['{8509DA18-5D02-41F0-8F88-2773FB4BA13D}']
  {Property Methods}
  function _GetARTIST: JString;
  function _GetARTIST_KEY: JString;
  function _GetNUMBER_OF_ALBUMS: JString;
  function _GetNUMBER_OF_TRACKS: JString;
  {Properties}
  property ARTIST: JString read _GetARTIST;
  property ARTIST_KEY: JString read _GetARTIST_KEY;
  property NUMBER_OF_ALBUMS: JString read _GetNUMBER_OF_ALBUMS;
  property NUMBER_OF_TRACKS: JString read _GetNUMBER_OF_TRACKS;
end;

[JavaSignature('android/provider/MediaStore$Audio$ArtistColumns')]
JAudio_ArtistColumns = interface(IJavaInstance)
['{F6666306-DFED-4271-ABB4-941BFBEF0EBE}']
end;
TJAudio_ArtistColumns = class(TJavaGenericImport<JAudio_ArtistColumnsClass, JAudio_ArtistColumns>) end;

JContactsContract_SyncStateClass = interface(JObjectClass)
['{817DC34B-E666-4002-BCFE-7BC4524722F8}']
  {Property Methods}
  function _GetCONTENT_DIRECTORY: JString;
  function _GetCONTENT_URI: Jnet_Uri;
  {Properties}
  property CONTENT_DIRECTORY: JString read _GetCONTENT_DIRECTORY;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
end;

[JavaSignature('android/provider/ContactsContract$SyncState')]
JContactsContract_SyncState = interface(JObject)
['{5691696E-16F0-4DDD-B204-E6443D30A6A0}']
end;
TJContactsContract_SyncState = class(TJavaGenericImport<JContactsContract_SyncStateClass, JContactsContract_SyncState>) end;

JMediaStore_FilesClass = interface(JObjectClass)
['{68751751-AB25-4D16-9AE0-07B3F4168DCC}']
  {Methods}
  function init: JMediaStore_Files; cdecl;
  function getContentUri(volumeName: JString): Jnet_Uri; cdecl; overload;
  function getContentUri(volumeName: JString; rowId: Int64): Jnet_Uri; cdecl; overload;
end;

[JavaSignature('android/provider/MediaStore$Files')]
JMediaStore_Files = interface(JObject)
['{D5BC722B-B676-42A4-8A64-450095C56B3F}']
end;
TJMediaStore_Files = class(TJavaGenericImport<JMediaStore_FilesClass, JMediaStore_Files>) end;

JCalendarContract_InstancesClass = interface(JObjectClass)
['{8D61CB60-F9C0-440F-9FA5-BB56CB6BD0DB}']
  {Property Methods}
  function _GetBEGIN: JString;
  function _GetCONTENT_BY_DAY_URI: Jnet_Uri;
  function _GetCONTENT_SEARCH_BY_DAY_URI: Jnet_Uri;
  function _GetCONTENT_SEARCH_URI: Jnet_Uri;
  function _GetCONTENT_URI: Jnet_Uri;
  function _GetEND: JString;
  function _GetEND_DAY: JString;
  function _GetEND_MINUTE: JString;
  function _GetEVENT_ID: JString;
  function _GetSTART_DAY: JString;
  function _GetSTART_MINUTE: JString;
  {Methods}
  function query(cr: JContentResolver; projection: TJavaObjectArray<JString>; begin_: Int64; end_: Int64): JCursor; cdecl; overload;
  function query(cr: JContentResolver; projection: TJavaObjectArray<JString>; begin_: Int64; end_: Int64; searchQuery: JString): JCursor; cdecl; overload;
  {Properties}
  property &BEGIN: JString read _GetBEGIN;
  property CONTENT_BY_DAY_URI: Jnet_Uri read _GetCONTENT_BY_DAY_URI;
  property CONTENT_SEARCH_BY_DAY_URI: Jnet_Uri read _GetCONTENT_SEARCH_BY_DAY_URI;
  property CONTENT_SEARCH_URI: Jnet_Uri read _GetCONTENT_SEARCH_URI;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
  property &END: JString read _GetEND;
  property END_DAY: JString read _GetEND_DAY;
  property END_MINUTE: JString read _GetEND_MINUTE;
  property EVENT_ID: JString read _GetEVENT_ID;
  property START_DAY: JString read _GetSTART_DAY;
  property START_MINUTE: JString read _GetSTART_MINUTE;
end;

[JavaSignature('android/provider/CalendarContract$Instances')]
JCalendarContract_Instances = interface(JObject)
['{9FC7533E-0D27-4B35-998B-3D9C2CEE79F7}']
end;
TJCalendarContract_Instances = class(TJavaGenericImport<JCalendarContract_InstancesClass, JCalendarContract_Instances>) end;

JCalendarContract_EventsEntityClass = interface(JObjectClass)
['{157ECBB3-77A9-4D6B-98FA-67E172F0EFAA}']
  {Property Methods}
  function _GetCONTENT_URI: Jnet_Uri;
  {Methods}
  function newEntityIterator(cursor: JCursor; resolver: JContentResolver): JEntityIterator; cdecl; overload;
  function newEntityIterator(cursor: JCursor; provider: JContentProviderClient): JEntityIterator; cdecl; overload;
  {Properties}
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
end;

[JavaSignature('android/provider/CalendarContract$EventsEntity')]
JCalendarContract_EventsEntity = interface(JObject)
['{89461BC2-A5DD-4854-92BD-25549D22BCC3}']
end;
TJCalendarContract_EventsEntity = class(TJavaGenericImport<JCalendarContract_EventsEntityClass, JCalendarContract_EventsEntity>) end;

JCalendarContract_AttendeesClass = interface(JObjectClass)
['{FCB32CF5-B56E-459B-A11C-E24A3A1A884D}']
  {Property Methods}
  function _GetCONTENT_URI: Jnet_Uri;
  {Methods}
  function query(cr: JContentResolver; eventId: Int64; projection: TJavaObjectArray<JString>): JCursor; cdecl;
  {Properties}
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
end;

[JavaSignature('android/provider/CalendarContract$Attendees')]
JCalendarContract_Attendees = interface(JObject)
['{7775F06D-0FCB-4FB1-8AD1-35C74951AE5E}']
end;
TJCalendarContract_Attendees = class(TJavaGenericImport<JCalendarContract_AttendeesClass, JCalendarContract_Attendees>) end;

JContacts_IntentsClass = interface(JObjectClass)
['{42C14BF0-54D4-46B7-8255-16CC534167BC}']
  {Property Methods}
  function _GetATTACH_IMAGE: JString;
  function _GetEXTRA_CREATE_DESCRIPTION: JString;
  function _GetEXTRA_FORCE_CREATE: JString;
  function _GetSEARCH_SUGGESTION_CLICKED: JString;
  function _GetSEARCH_SUGGESTION_CREATE_CONTACT_CLICKED: JString;
  function _GetSEARCH_SUGGESTION_DIAL_NUMBER_CLICKED: JString;
  function _GetSHOW_OR_CREATE_CONTACT: JString;
  {Methods}
  function init: JContacts_Intents; cdecl;//Deprecated
  {Properties}
  property ATTACH_IMAGE: JString read _GetATTACH_IMAGE;
  property EXTRA_CREATE_DESCRIPTION: JString read _GetEXTRA_CREATE_DESCRIPTION;
  property EXTRA_FORCE_CREATE: JString read _GetEXTRA_FORCE_CREATE;
  property SEARCH_SUGGESTION_CLICKED: JString read _GetSEARCH_SUGGESTION_CLICKED;
  property SEARCH_SUGGESTION_CREATE_CONTACT_CLICKED: JString read _GetSEARCH_SUGGESTION_CREATE_CONTACT_CLICKED;
  property SEARCH_SUGGESTION_DIAL_NUMBER_CLICKED: JString read _GetSEARCH_SUGGESTION_DIAL_NUMBER_CLICKED;
  property SHOW_OR_CREATE_CONTACT: JString read _GetSHOW_OR_CREATE_CONTACT;
end;

[JavaSignature('android/provider/Contacts$Intents')]
JContacts_Intents = interface(JObject)
['{3899A52D-E908-44CB-8722-0331ACB5B43D}']
end;
TJContacts_Intents = class(TJavaGenericImport<JContacts_IntentsClass, JContacts_Intents>) end;

JVideo_MediaClass = interface(JObjectClass)
['{1689E04F-43C7-4B53-936B-886E4A5F677A}']
  {Property Methods}
  function _GetCONTENT_TYPE: JString;
  function _GetDEFAULT_SORT_ORDER: JString;
  function _GetEXTERNAL_CONTENT_URI: Jnet_Uri;
  function _GetINTERNAL_CONTENT_URI: Jnet_Uri;
  {Methods}
  function init: JVideo_Media; cdecl;
  function getContentUri(volumeName: JString): Jnet_Uri; cdecl;
  {Properties}
  property CONTENT_TYPE: JString read _GetCONTENT_TYPE;
  property DEFAULT_SORT_ORDER: JString read _GetDEFAULT_SORT_ORDER;
  property EXTERNAL_CONTENT_URI: Jnet_Uri read _GetEXTERNAL_CONTENT_URI;
  property INTERNAL_CONTENT_URI: Jnet_Uri read _GetINTERNAL_CONTENT_URI;
end;

[JavaSignature('android/provider/MediaStore$Video$Media')]
JVideo_Media = interface(JObject)
['{8B179599-8165-4E55-B17D-E576BC51E3C6}']
end;
TJVideo_Media = class(TJavaGenericImport<JVideo_MediaClass, JVideo_Media>) end;

JContacts_PeopleClass = interface(JObjectClass)
['{79BB5A75-B7DB-4CEC-B4DE-0BA93F8782BA}']
  {Property Methods}
  function _GetCONTENT_FILTER_URI: Jnet_Uri;
  function _GetCONTENT_ITEM_TYPE: JString;
  function _GetCONTENT_TYPE: JString;
  function _GetCONTENT_URI: Jnet_Uri;
  function _GetDEFAULT_SORT_ORDER: JString;
  function _GetDELETED_CONTENT_URI: Jnet_Uri;
  function _GetPRIMARY_EMAIL_ID: JString;
  function _GetPRIMARY_ORGANIZATION_ID: JString;
  function _GetPRIMARY_PHONE_ID: JString;
  {Methods}
  function addToGroup(resolver: JContentResolver; personId: Int64; groupName: JString): Jnet_Uri; cdecl; overload;//Deprecated
  function addToGroup(resolver: JContentResolver; personId: Int64; groupId: Int64): Jnet_Uri; cdecl; overload;//Deprecated
  function addToMyContactsGroup(resolver: JContentResolver; personId: Int64): Jnet_Uri; cdecl;//Deprecated
  function createPersonInMyContactsGroup(resolver: JContentResolver; values: JContentValues): Jnet_Uri; cdecl;//Deprecated
  function loadContactPhoto(context: JContext; person: Jnet_Uri; placeholderImageResource: Integer; options: JBitmapFactory_Options): JBitmap; cdecl;//Deprecated
  procedure markAsContacted(resolver: JContentResolver; personId: Int64); cdecl;//Deprecated
  function openContactPhotoInputStream(cr: JContentResolver; person: Jnet_Uri): JInputStream; cdecl;//Deprecated
  function queryGroups(resolver: JContentResolver; person: Int64): JCursor; cdecl;//Deprecated
  procedure setPhotoData(cr: JContentResolver; person: Jnet_Uri; data: TJavaArray<Byte>); cdecl;//Deprecated
  {Properties}
  property CONTENT_FILTER_URI: Jnet_Uri read _GetCONTENT_FILTER_URI;
  property CONTENT_ITEM_TYPE: JString read _GetCONTENT_ITEM_TYPE;
  property CONTENT_TYPE: JString read _GetCONTENT_TYPE;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
  property DEFAULT_SORT_ORDER: JString read _GetDEFAULT_SORT_ORDER;
  property DELETED_CONTENT_URI: Jnet_Uri read _GetDELETED_CONTENT_URI;
  property PRIMARY_EMAIL_ID: JString read _GetPRIMARY_EMAIL_ID;
  property PRIMARY_ORGANIZATION_ID: JString read _GetPRIMARY_ORGANIZATION_ID;
  property PRIMARY_PHONE_ID: JString read _GetPRIMARY_PHONE_ID;
end;

[JavaSignature('android/provider/Contacts$People')]
JContacts_People = interface(JObject)
['{29599D8E-C652-4CEE-9B1A-EB1F3D3BD974}']
end;
TJContacts_People = class(TJavaGenericImport<JContacts_PeopleClass, JContacts_People>) end;

JContactsContract_DataClass = interface(JObjectClass)
['{9144ECE7-6E2F-4A08-A26F-B790DB083635}']
  {Property Methods}
  function _GetCONTENT_TYPE: JString;
  function _GetCONTENT_URI: Jnet_Uri;
  {Methods}
  function getContactLookupUri(resolver: JContentResolver; dataUri: Jnet_Uri): Jnet_Uri; cdecl;
  {Properties}
  property CONTENT_TYPE: JString read _GetCONTENT_TYPE;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
end;

[JavaSignature('android/provider/ContactsContract$Data')]
JContactsContract_Data = interface(JObject)
['{B149289E-DE66-4F78-BE4A-E7236D4FF10B}']
end;
TJContactsContract_Data = class(TJavaGenericImport<JContactsContract_DataClass, JContactsContract_Data>) end;

JContacts_AggregationSuggestionsClass = interface(JObjectClass)
['{9893192B-8FB2-431D-854E-4093F6197FFC}']
  {Property Methods}
  function _GetCONTENT_DIRECTORY: JString;
  {Properties}
  property CONTENT_DIRECTORY: JString read _GetCONTENT_DIRECTORY;
end;

[JavaSignature('android/provider/ContactsContract$Contacts$AggregationSuggestions')]
JContacts_AggregationSuggestions = interface(JObject)
['{CF42AF54-D723-4D78-9131-5A0A0F5CE081}']
end;
TJContacts_AggregationSuggestions = class(TJavaGenericImport<JContacts_AggregationSuggestionsClass, JContacts_AggregationSuggestions>) end;

JContacts_PhotoClass = interface(JObjectClass)
['{090384EF-4948-44BF-872A-1C059D2FF535}']
  {Property Methods}
  function _GetCONTENT_DIRECTORY: JString;
  function _GetDISPLAY_PHOTO: JString;
  function _GetPHOTO: JString;
  function _GetPHOTO_FILE_ID: JString;
  {Properties}
  property CONTENT_DIRECTORY: JString read _GetCONTENT_DIRECTORY;
  property DISPLAY_PHOTO: JString read _GetDISPLAY_PHOTO;
  property PHOTO: JString read _GetPHOTO;
  property PHOTO_FILE_ID: JString read _GetPHOTO_FILE_ID;
end;

[JavaSignature('android/provider/ContactsContract$Contacts$Photo')]
JContacts_Photo = interface(JObject)
['{DFDAEDC0-B587-4AFA-81C5-1AE7AEED9E9E}']
end;
TJContacts_Photo = class(TJavaGenericImport<JContacts_PhotoClass, JContacts_Photo>) end;

JSyncStateContract_ConstantsClass = interface(JObjectClass)
['{13965A6D-CFFD-4471-9B6D-E58B4673F910}']
  {Property Methods}
  function _GetCONTENT_DIRECTORY: JString;
  {Methods}
  function init: JSyncStateContract_Constants; cdecl;
  {Properties}
  property CONTENT_DIRECTORY: JString read _GetCONTENT_DIRECTORY;
end;

[JavaSignature('android/provider/SyncStateContract$Constants')]
JSyncStateContract_Constants = interface(JObject)
['{540C4A22-0C3F-4AB3-AF71-D227A2DADA77}']
end;
TJSyncStateContract_Constants = class(TJavaGenericImport<JSyncStateContract_ConstantsClass, JSyncStateContract_Constants>) end;

JImages_MediaClass = interface(JObjectClass)
['{01ABDE1D-B100-4BB4-9F36-39209366844E}']
  {Property Methods}
  function _GetCONTENT_TYPE: JString;
  function _GetDEFAULT_SORT_ORDER: JString;
  function _GetEXTERNAL_CONTENT_URI: Jnet_Uri;
  function _GetINTERNAL_CONTENT_URI: Jnet_Uri;
  {Methods}
  function init: JImages_Media; cdecl;
  function getBitmap(cr: JContentResolver; url: Jnet_Uri): JBitmap; cdecl;
  function getContentUri(volumeName: JString): Jnet_Uri; cdecl;
  function insertImage(cr: JContentResolver; imagePath: JString; name: JString; description: JString): JString; cdecl; overload;
  function insertImage(cr: JContentResolver; source: JBitmap; title: JString; description: JString): JString; cdecl; overload;
  function query(cr: JContentResolver; uri: Jnet_Uri; projection: TJavaObjectArray<JString>): JCursor; cdecl; overload;
  function query(cr: JContentResolver; uri: Jnet_Uri; projection: TJavaObjectArray<JString>; where: JString; orderBy: JString): JCursor; cdecl; overload;
  function query(cr: JContentResolver; uri: Jnet_Uri; projection: TJavaObjectArray<JString>; selection: JString; selectionArgs: TJavaObjectArray<JString>; orderBy: JString): JCursor; cdecl; overload;
  {Properties}
  property CONTENT_TYPE: JString read _GetCONTENT_TYPE;
  property DEFAULT_SORT_ORDER: JString read _GetDEFAULT_SORT_ORDER;
  property EXTERNAL_CONTENT_URI: Jnet_Uri read _GetEXTERNAL_CONTENT_URI;
  property INTERNAL_CONTENT_URI: Jnet_Uri read _GetINTERNAL_CONTENT_URI;
end;

[JavaSignature('android/provider/MediaStore$Images$Media')]
JImages_Media = interface(JObject)
['{FCE5B254-93C2-4308-9296-9A78C66135D5}']
end;
TJImages_Media = class(TJavaGenericImport<JImages_MediaClass, JImages_Media>) end;

JContacts_PeopleColumnsClass = interface(IJavaClass)
['{4934A41B-BB4E-44BB-B9FE-44455BDA5C93}']
  {Property Methods}
  function _GetCUSTOM_RINGTONE: JString;
  function _GetDISPLAY_NAME: JString;
  function _GetLAST_TIME_CONTACTED: JString;
  function _GetNAME: JString;
  function _GetNOTES: JString;
  function _GetPHONETIC_NAME: JString;
  function _GetPHOTO_VERSION: JString;
  function _GetSEND_TO_VOICEMAIL: JString;
  function _GetSTARRED: JString;
  function _GetTIMES_CONTACTED: JString;
  {Properties}
  property CUSTOM_RINGTONE: JString read _GetCUSTOM_RINGTONE;
  property DISPLAY_NAME: JString read _GetDISPLAY_NAME;
  property LAST_TIME_CONTACTED: JString read _GetLAST_TIME_CONTACTED;
  property NAME: JString read _GetNAME;
  property NOTES: JString read _GetNOTES;
  property PHONETIC_NAME: JString read _GetPHONETIC_NAME;
  property PHOTO_VERSION: JString read _GetPHOTO_VERSION;
  property SEND_TO_VOICEMAIL: JString read _GetSEND_TO_VOICEMAIL;
  property STARRED: JString read _GetSTARRED;
  property TIMES_CONTACTED: JString read _GetTIMES_CONTACTED;
end;

[JavaSignature('android/provider/Contacts$PeopleColumns')]
JContacts_PeopleColumns = interface(IJavaInstance)
['{8C727BCF-E0E2-42F0-8C80-00905562DFCF}']
end;
TJContacts_PeopleColumns = class(TJavaGenericImport<JContacts_PeopleColumnsClass, JContacts_PeopleColumns>) end;

JUserDictionary_WordsClass = interface(JObjectClass)
['{906070B3-8F20-4983-8A60-2D81ADCEE971}']
  {Property Methods}
  function _GetAPP_ID: JString;
  function _GetCONTENT_ITEM_TYPE: JString;
  function _GetCONTENT_TYPE: JString;
  function _GetCONTENT_URI: Jnet_Uri;
  function _GetDEFAULT_SORT_ORDER: JString;
  function _GetFREQUENCY: JString;
  function _GetLOCALE: JString;
  function _GetLOCALE_TYPE_ALL: Integer;
  function _GetLOCALE_TYPE_CURRENT: Integer;
  function _GetSHORTCUT: JString;
  function _GetWORD: JString;
  function _Get_ID: JString;
  {Methods}
  function init: JUserDictionary_Words; cdecl;
  procedure addWord(context: JContext; word: JString; frequency: Integer; localeType: Integer); cdecl; overload;//Deprecated
  procedure addWord(context: JContext; word: JString; frequency: Integer; shortcut: JString; locale: JLocale); cdecl; overload;
  {Properties}
  property APP_ID: JString read _GetAPP_ID;
  property CONTENT_ITEM_TYPE: JString read _GetCONTENT_ITEM_TYPE;
  property CONTENT_TYPE: JString read _GetCONTENT_TYPE;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
  property DEFAULT_SORT_ORDER: JString read _GetDEFAULT_SORT_ORDER;
  property FREQUENCY: JString read _GetFREQUENCY;
  property LOCALE: JString read _GetLOCALE;
  property LOCALE_TYPE_ALL: Integer read _GetLOCALE_TYPE_ALL;
  property LOCALE_TYPE_CURRENT: Integer read _GetLOCALE_TYPE_CURRENT;
  property SHORTCUT: JString read _GetSHORTCUT;
  property WORD: JString read _GetWORD;
  property _ID: JString read _Get_ID;
end;

[JavaSignature('android/provider/UserDictionary$Words')]
JUserDictionary_Words = interface(JObject)
['{B5F406C1-D387-4674-8F58-407A1A835C13}']
end;
TJUserDictionary_Words = class(TJavaGenericImport<JUserDictionary_WordsClass, JUserDictionary_Words>) end;

JAudio_GenresClass = interface(JObjectClass)
['{2D5E7697-D2AE-469F-919A-C895055A7936}']
  {Property Methods}
  function _GetCONTENT_TYPE: JString;
  function _GetDEFAULT_SORT_ORDER: JString;
  function _GetENTRY_CONTENT_TYPE: JString;
  function _GetEXTERNAL_CONTENT_URI: Jnet_Uri;
  function _GetINTERNAL_CONTENT_URI: Jnet_Uri;
  {Methods}
  function init: JAudio_Genres; cdecl;
  function getContentUri(volumeName: JString): Jnet_Uri; cdecl;
  function getContentUriForAudioId(volumeName: JString; audioId: Integer): Jnet_Uri; cdecl;
  {Properties}
  property CONTENT_TYPE: JString read _GetCONTENT_TYPE;
  property DEFAULT_SORT_ORDER: JString read _GetDEFAULT_SORT_ORDER;
  property ENTRY_CONTENT_TYPE: JString read _GetENTRY_CONTENT_TYPE;
  property EXTERNAL_CONTENT_URI: Jnet_Uri read _GetEXTERNAL_CONTENT_URI;
  property INTERNAL_CONTENT_URI: Jnet_Uri read _GetINTERNAL_CONTENT_URI;
end;

[JavaSignature('android/provider/MediaStore$Audio$Genres')]
JAudio_Genres = interface(JObject)
['{100DAE19-058A-48EA-B0DA-E9C18A2138C2}']
end;
TJAudio_Genres = class(TJavaGenericImport<JAudio_GenresClass, JAudio_Genres>) end;

JContactsContract_SettingsClass = interface(JObjectClass)
['{13D5FAD0-90BB-4AB2-91E6-BF70605DAF36}']
  {Property Methods}
  function _GetCONTENT_ITEM_TYPE: JString;
  function _GetCONTENT_TYPE: JString;
  function _GetCONTENT_URI: Jnet_Uri;
  {Properties}
  property CONTENT_ITEM_TYPE: JString read _GetCONTENT_ITEM_TYPE;
  property CONTENT_TYPE: JString read _GetCONTENT_TYPE;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
end;

[JavaSignature('android/provider/ContactsContract$Settings')]
JContactsContract_Settings = interface(JObject)
['{BBF9385B-DFA5-4639-9EC1-892A91427919}']
end;
TJContactsContract_Settings = class(TJavaGenericImport<JContactsContract_SettingsClass, JContactsContract_Settings>) end;

JAudio_PlaylistsColumnsClass = interface(IJavaClass)
['{D6BF287D-2B2D-43DC-B70B-82B3AF310A8A}']
  {Property Methods}
  function _GetDATA: JString;
  function _GetDATE_ADDED: JString;
  function _GetDATE_MODIFIED: JString;
  function _GetNAME: JString;
  {Properties}
  property DATA: JString read _GetDATA;
  property DATE_ADDED: JString read _GetDATE_ADDED;
  property DATE_MODIFIED: JString read _GetDATE_MODIFIED;
  property NAME: JString read _GetNAME;
end;

[JavaSignature('android/provider/MediaStore$Audio$PlaylistsColumns')]
JAudio_PlaylistsColumns = interface(IJavaInstance)
['{00754FBD-3E95-45F3-AAF3-C35C76BA486C}']
end;
TJAudio_PlaylistsColumns = class(TJavaGenericImport<JAudio_PlaylistsColumnsClass, JAudio_PlaylistsColumns>) end;

JPeople_ContactMethodsClass = interface(JObjectClass)
['{FFBB0FBA-6AEC-4E4C-B0DF-A1280FA674C9}']
  {Property Methods}
  function _GetCONTENT_DIRECTORY: JString;
  function _GetDEFAULT_SORT_ORDER: JString;
  {Properties}
  property CONTENT_DIRECTORY: JString read _GetCONTENT_DIRECTORY;
  property DEFAULT_SORT_ORDER: JString read _GetDEFAULT_SORT_ORDER;
end;

[JavaSignature('android/provider/Contacts$People$ContactMethods')]
JPeople_ContactMethods = interface(JObject)
['{33E39BD1-FFF2-417C-ABDE-5E19E6AEFDA4}']
end;
TJPeople_ContactMethods = class(TJavaGenericImport<JPeople_ContactMethodsClass, JPeople_ContactMethods>) end;

JRawContacts_DisplayPhotoClass = interface(JObjectClass)
['{BF4BD582-6AEF-4BA5-928B-B0A425DF93C0}']
  {Property Methods}
  function _GetCONTENT_DIRECTORY: JString;
  {Properties}
  property CONTENT_DIRECTORY: JString read _GetCONTENT_DIRECTORY;
end;

[JavaSignature('android/provider/ContactsContract$RawContacts$DisplayPhoto')]
JRawContacts_DisplayPhoto = interface(JObject)
['{2019B5CE-9DFE-486C-83FB-98A4D556D5B0}']
end;
TJRawContacts_DisplayPhoto = class(TJavaGenericImport<JRawContacts_DisplayPhotoClass, JRawContacts_DisplayPhoto>) end;

JContactsContract_Intents_InsertClass = interface(JObjectClass)
['{63E7DE40-278E-4636-A5C3-6328373C2F61}']
  {Property Methods}
  function _GetACTION: JString;
  function _GetCOMPANY: JString;
  function _GetDATA: JString;
  function _GetEMAIL: JString;
  function _GetEMAIL_ISPRIMARY: JString;
  function _GetEMAIL_TYPE: JString;
  function _GetFULL_MODE: JString;
  function _GetIM_HANDLE: JString;
  function _GetIM_ISPRIMARY: JString;
  function _GetIM_PROTOCOL: JString;
  function _GetJOB_TITLE: JString;
  function _GetNAME: JString;
  function _GetNOTES: JString;
  function _GetPHONE: JString;
  function _GetPHONETIC_NAME: JString;
  function _GetPHONE_ISPRIMARY: JString;
  function _GetPHONE_TYPE: JString;
  function _GetPOSTAL: JString;
  function _GetPOSTAL_ISPRIMARY: JString;
  function _GetPOSTAL_TYPE: JString;
  function _GetSECONDARY_EMAIL: JString;
  function _GetSECONDARY_EMAIL_TYPE: JString;
  function _GetSECONDARY_PHONE: JString;
  function _GetSECONDARY_PHONE_TYPE: JString;
  function _GetTERTIARY_EMAIL: JString;
  function _GetTERTIARY_EMAIL_TYPE: JString;
  function _GetTERTIARY_PHONE: JString;
  function _GetTERTIARY_PHONE_TYPE: JString;
  {Methods}
  function init: JContactsContract_Intents_Insert; cdecl;
  {Properties}
  property ACTION: JString read _GetACTION;
  property COMPANY: JString read _GetCOMPANY;
  property DATA: JString read _GetDATA;
  property EMAIL: JString read _GetEMAIL;
  property EMAIL_ISPRIMARY: JString read _GetEMAIL_ISPRIMARY;
  property EMAIL_TYPE: JString read _GetEMAIL_TYPE;
  property FULL_MODE: JString read _GetFULL_MODE;
  property IM_HANDLE: JString read _GetIM_HANDLE;
  property IM_ISPRIMARY: JString read _GetIM_ISPRIMARY;
  property IM_PROTOCOL: JString read _GetIM_PROTOCOL;
  property JOB_TITLE: JString read _GetJOB_TITLE;
  property NAME: JString read _GetNAME;
  property NOTES: JString read _GetNOTES;
  property PHONE: JString read _GetPHONE;
  property PHONETIC_NAME: JString read _GetPHONETIC_NAME;
  property PHONE_ISPRIMARY: JString read _GetPHONE_ISPRIMARY;
  property PHONE_TYPE: JString read _GetPHONE_TYPE;
  property POSTAL: JString read _GetPOSTAL;
  property POSTAL_ISPRIMARY: JString read _GetPOSTAL_ISPRIMARY;
  property POSTAL_TYPE: JString read _GetPOSTAL_TYPE;
  property SECONDARY_EMAIL: JString read _GetSECONDARY_EMAIL;
  property SECONDARY_EMAIL_TYPE: JString read _GetSECONDARY_EMAIL_TYPE;
  property SECONDARY_PHONE: JString read _GetSECONDARY_PHONE;
  property SECONDARY_PHONE_TYPE: JString read _GetSECONDARY_PHONE_TYPE;
  property TERTIARY_EMAIL: JString read _GetTERTIARY_EMAIL;
  property TERTIARY_EMAIL_TYPE: JString read _GetTERTIARY_EMAIL_TYPE;
  property TERTIARY_PHONE: JString read _GetTERTIARY_PHONE;
  property TERTIARY_PHONE_TYPE: JString read _GetTERTIARY_PHONE_TYPE;
end;

[JavaSignature('android/provider/ContactsContract$Intents$Insert')]
JContactsContract_Intents_Insert = interface(JObject)
['{458E71C3-1413-45B2-865E-F8346EC6C884}']
end;
TJContactsContract_Intents_Insert = class(TJavaGenericImport<JContactsContract_Intents_InsertClass, JContactsContract_Intents_Insert>) end;

JContacts_GroupsClass = interface(JObjectClass)
['{B066518A-FA76-46E5-B324-16A10C77B766}']
  {Property Methods}
  function _GetCONTENT_ITEM_TYPE: JString;
  function _GetCONTENT_TYPE: JString;
  function _GetCONTENT_URI: Jnet_Uri;
  function _GetDEFAULT_SORT_ORDER: JString;
  function _GetDELETED_CONTENT_URI: Jnet_Uri;
  function _GetGROUP_ANDROID_STARRED: JString;
  function _GetGROUP_MY_CONTACTS: JString;
  {Properties}
  property CONTENT_ITEM_TYPE: JString read _GetCONTENT_ITEM_TYPE;
  property CONTENT_TYPE: JString read _GetCONTENT_TYPE;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
  property DEFAULT_SORT_ORDER: JString read _GetDEFAULT_SORT_ORDER;
  property DELETED_CONTENT_URI: Jnet_Uri read _GetDELETED_CONTENT_URI;
  property GROUP_ANDROID_STARRED: JString read _GetGROUP_ANDROID_STARRED;
  property GROUP_MY_CONTACTS: JString read _GetGROUP_MY_CONTACTS;
end;

[JavaSignature('android/provider/Contacts$Groups')]
JContacts_Groups = interface(JObject)
['{68A7D006-DC19-4CF4-BD69-1F4C75163C5A}']
end;
TJContacts_Groups = class(TJavaGenericImport<JContacts_GroupsClass, JContacts_Groups>) end;

JAudio_AlbumsClass = interface(JObjectClass)
['{2A26AD71-03E9-4D6D-B102-8EA01278768D}']
  {Property Methods}
  function _GetCONTENT_TYPE: JString;
  function _GetDEFAULT_SORT_ORDER: JString;
  function _GetENTRY_CONTENT_TYPE: JString;
  function _GetEXTERNAL_CONTENT_URI: Jnet_Uri;
  function _GetINTERNAL_CONTENT_URI: Jnet_Uri;
  {Methods}
  function init: JAudio_Albums; cdecl;
  function getContentUri(volumeName: JString): Jnet_Uri; cdecl;
  {Properties}
  property CONTENT_TYPE: JString read _GetCONTENT_TYPE;
  property DEFAULT_SORT_ORDER: JString read _GetDEFAULT_SORT_ORDER;
  property ENTRY_CONTENT_TYPE: JString read _GetENTRY_CONTENT_TYPE;
  property EXTERNAL_CONTENT_URI: Jnet_Uri read _GetEXTERNAL_CONTENT_URI;
  property INTERNAL_CONTENT_URI: Jnet_Uri read _GetINTERNAL_CONTENT_URI;
end;

[JavaSignature('android/provider/MediaStore$Audio$Albums')]
JAudio_Albums = interface(JObject)
['{828C1395-B307-49DC-B92A-D7D54BA5F2F7}']
end;
TJAudio_Albums = class(TJavaGenericImport<JAudio_AlbumsClass, JAudio_Albums>) end;

JCommonDataKinds_PhotoClass = interface(JObjectClass)
['{DB6F4109-261D-4991-A976-4B785F2D5F53}']
  {Property Methods}
  function _GetCONTENT_ITEM_TYPE: JString;
  function _GetPHOTO: JString;
  function _GetPHOTO_FILE_ID: JString;
  {Properties}
  property CONTENT_ITEM_TYPE: JString read _GetCONTENT_ITEM_TYPE;
  property PHOTO: JString read _GetPHOTO;
  property PHOTO_FILE_ID: JString read _GetPHOTO_FILE_ID;
end;

[JavaSignature('android/provider/ContactsContract$CommonDataKinds$Photo')]
JCommonDataKinds_Photo = interface(JObject)
['{967ABFE6-3015-4455-9C76-618C9422689D}']
end;
TJCommonDataKinds_Photo = class(TJavaGenericImport<JCommonDataKinds_PhotoClass, JCommonDataKinds_Photo>) end;

JContacts_ContactMethodsClass = interface(JObjectClass)
['{E7392E3D-EEE3-412C-B71D-E0B6320DDA7B}']
  {Property Methods}
  function _GetCONTENT_EMAIL_ITEM_TYPE: JString;
  function _GetCONTENT_EMAIL_TYPE: JString;
  function _GetCONTENT_EMAIL_URI: Jnet_Uri;
  function _GetCONTENT_IM_ITEM_TYPE: JString;
  function _GetCONTENT_POSTAL_ITEM_TYPE: JString;
  function _GetCONTENT_POSTAL_TYPE: JString;
  function _GetCONTENT_TYPE: JString;
  function _GetCONTENT_URI: Jnet_Uri;
  function _GetDEFAULT_SORT_ORDER: JString;
  function _GetPERSON_ID: JString;
  function _GetPOSTAL_LOCATION_LATITUDE: JString;
  function _GetPOSTAL_LOCATION_LONGITUDE: JString;
  function _GetPROTOCOL_AIM: Integer;
  function _GetPROTOCOL_GOOGLE_TALK: Integer;
  function _GetPROTOCOL_ICQ: Integer;
  function _GetPROTOCOL_JABBER: Integer;
  function _GetPROTOCOL_MSN: Integer;
  function _GetPROTOCOL_QQ: Integer;
  function _GetPROTOCOL_SKYPE: Integer;
  function _GetPROTOCOL_YAHOO: Integer;
  {Methods}
  function decodeImProtocol(encodedString: JString): JObject; cdecl;//Deprecated
  function encodeCustomImProtocol(protocolString: JString): JString; cdecl;//Deprecated
  function encodePredefinedImProtocol(protocol: Integer): JString; cdecl;//Deprecated
  function getDisplayLabel(context: JContext; kind: Integer; type_: Integer; label_: JCharSequence): JCharSequence; cdecl;//Deprecated
  {Properties}
  property CONTENT_EMAIL_ITEM_TYPE: JString read _GetCONTENT_EMAIL_ITEM_TYPE;
  property CONTENT_EMAIL_TYPE: JString read _GetCONTENT_EMAIL_TYPE;
  property CONTENT_EMAIL_URI: Jnet_Uri read _GetCONTENT_EMAIL_URI;
  property CONTENT_IM_ITEM_TYPE: JString read _GetCONTENT_IM_ITEM_TYPE;
  property CONTENT_POSTAL_ITEM_TYPE: JString read _GetCONTENT_POSTAL_ITEM_TYPE;
  property CONTENT_POSTAL_TYPE: JString read _GetCONTENT_POSTAL_TYPE;
  property CONTENT_TYPE: JString read _GetCONTENT_TYPE;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
  property DEFAULT_SORT_ORDER: JString read _GetDEFAULT_SORT_ORDER;
  property PERSON_ID: JString read _GetPERSON_ID;
  property POSTAL_LOCATION_LATITUDE: JString read _GetPOSTAL_LOCATION_LATITUDE;
  property POSTAL_LOCATION_LONGITUDE: JString read _GetPOSTAL_LOCATION_LONGITUDE;
  property PROTOCOL_AIM: Integer read _GetPROTOCOL_AIM;
  property PROTOCOL_GOOGLE_TALK: Integer read _GetPROTOCOL_GOOGLE_TALK;
  property PROTOCOL_ICQ: Integer read _GetPROTOCOL_ICQ;
  property PROTOCOL_JABBER: Integer read _GetPROTOCOL_JABBER;
  property PROTOCOL_MSN: Integer read _GetPROTOCOL_MSN;
  property PROTOCOL_QQ: Integer read _GetPROTOCOL_QQ;
  property PROTOCOL_SKYPE: Integer read _GetPROTOCOL_SKYPE;
  property PROTOCOL_YAHOO: Integer read _GetPROTOCOL_YAHOO;
end;

[JavaSignature('android/provider/Contacts$ContactMethods')]
JContacts_ContactMethods = interface(JObject)
['{4336FCFA-8273-4814-A060-BCEF8613EC38}']
  {Methods}
  procedure addPostalLocation(context: JContext; postalId: Int64; latitude: Double; longitude: Double); cdecl;//Deprecated
end;
TJContacts_ContactMethods = class(TJavaGenericImport<JContacts_ContactMethodsClass, JContacts_ContactMethods>) end;

JContacts_ExtensionsClass = interface(JObjectClass)
['{F4FD4B8D-3F96-4C0E-8C12-6BB78F3A584A}']
  {Property Methods}
  function _GetCONTENT_ITEM_TYPE: JString;
  function _GetCONTENT_TYPE: JString;
  function _GetCONTENT_URI: Jnet_Uri;
  function _GetDEFAULT_SORT_ORDER: JString;
  function _GetPERSON_ID: JString;
  {Properties}
  property CONTENT_ITEM_TYPE: JString read _GetCONTENT_ITEM_TYPE;
  property CONTENT_TYPE: JString read _GetCONTENT_TYPE;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
  property DEFAULT_SORT_ORDER: JString read _GetDEFAULT_SORT_ORDER;
  property PERSON_ID: JString read _GetPERSON_ID;
end;

[JavaSignature('android/provider/Contacts$Extensions')]
JContacts_Extensions = interface(JObject)
['{458FE1B4-F7BD-4125-B8D1-803192A5DC0D}']
end;
TJContacts_Extensions = class(TJavaGenericImport<JContacts_ExtensionsClass, JContacts_Extensions>) end;

JCommonDataKinds_WebsiteClass = interface(JObjectClass)
['{FE9A4E27-0823-4810-A23F-C500700DF6E6}']
  {Property Methods}
  function _GetCONTENT_ITEM_TYPE: JString;
  function _GetTYPE_BLOG: Integer;
  function _GetTYPE_FTP: Integer;
  function _GetTYPE_HOME: Integer;
  function _GetTYPE_HOMEPAGE: Integer;
  function _GetTYPE_OTHER: Integer;
  function _GetTYPE_PROFILE: Integer;
  function _GetTYPE_WORK: Integer;
  function _GetURL: JString;
  {Properties}
  property CONTENT_ITEM_TYPE: JString read _GetCONTENT_ITEM_TYPE;
  property TYPE_BLOG: Integer read _GetTYPE_BLOG;
  property TYPE_FTP: Integer read _GetTYPE_FTP;
  property TYPE_HOME: Integer read _GetTYPE_HOME;
  property TYPE_HOMEPAGE: Integer read _GetTYPE_HOMEPAGE;
  property TYPE_OTHER: Integer read _GetTYPE_OTHER;
  property TYPE_PROFILE: Integer read _GetTYPE_PROFILE;
  property TYPE_WORK: Integer read _GetTYPE_WORK;
  property URL: JString read _GetURL;
end;

[JavaSignature('android/provider/ContactsContract$CommonDataKinds$Website')]
JCommonDataKinds_Website = interface(JObject)
['{CB6FDE41-ED22-4AD6-AAFA-C0A2BAC0EF21}']
end;
TJCommonDataKinds_Website = class(TJavaGenericImport<JCommonDataKinds_WebsiteClass, JCommonDataKinds_Website>) end;

JMediaStore_AudioClass = interface(JObjectClass)
['{CA79759B-EAAE-44BE-A4B3-4240E9B55D60}']
  {Methods}
  function init: JMediaStore_Audio; cdecl;
  function keyFor(name: JString): JString; cdecl;
end;

[JavaSignature('android/provider/MediaStore$Audio')]
JMediaStore_Audio = interface(JObject)
['{492E32A0-816F-4BCE-9EFA-BB24D53518F3}']
end;
TJMediaStore_Audio = class(TJavaGenericImport<JMediaStore_AudioClass, JMediaStore_Audio>) end;

JContactsContract_PresenceClass = interface(JContactsContract_StatusUpdatesClass)
['{CADB7F8A-65D0-43B4-B90F-B44936C5F743}']
  {Methods}
  function init: JContactsContract_Presence; cdecl;
end;

[JavaSignature('android/provider/ContactsContract$Presence')]
JContactsContract_Presence = interface(JContactsContract_StatusUpdates)
['{5AD58EB3-195F-4C71-9D7B-18CF95F21DA9}']
end;
TJContactsContract_Presence = class(TJavaGenericImport<JContactsContract_PresenceClass, JContactsContract_Presence>) end;

JVideo_ThumbnailsClass = interface(JObjectClass)
['{5E05E9B0-E775-4D97-A7DC-6C5A9DB081D0}']
  {Property Methods}
  function _GetDATA: JString;
  function _GetDEFAULT_SORT_ORDER: JString;
  function _GetEXTERNAL_CONTENT_URI: Jnet_Uri;
  function _GetFULL_SCREEN_KIND: Integer;
  function _GetHEIGHT: JString;
  function _GetINTERNAL_CONTENT_URI: Jnet_Uri;
  function _GetKIND: JString;
  function _GetMICRO_KIND: Integer;
  function _GetMINI_KIND: Integer;
  function _GetVIDEO_ID: JString;
  function _GetWIDTH: JString;
  {Methods}
  function init: JVideo_Thumbnails; cdecl;
  procedure cancelThumbnailRequest(cr: JContentResolver; origId: Int64); cdecl; overload;
  procedure cancelThumbnailRequest(cr: JContentResolver; origId: Int64; groupId: Int64); cdecl; overload;
  function getContentUri(volumeName: JString): Jnet_Uri; cdecl;
  function getThumbnail(cr: JContentResolver; origId: Int64; kind: Integer; options: JBitmapFactory_Options): JBitmap; cdecl; overload;
  function getThumbnail(cr: JContentResolver; origId: Int64; groupId: Int64; kind: Integer; options: JBitmapFactory_Options): JBitmap; cdecl; overload;
  {Properties}
  property DATA: JString read _GetDATA;
  property DEFAULT_SORT_ORDER: JString read _GetDEFAULT_SORT_ORDER;
  property EXTERNAL_CONTENT_URI: Jnet_Uri read _GetEXTERNAL_CONTENT_URI;
  property FULL_SCREEN_KIND: Integer read _GetFULL_SCREEN_KIND;
  property HEIGHT: JString read _GetHEIGHT;
  property INTERNAL_CONTENT_URI: Jnet_Uri read _GetINTERNAL_CONTENT_URI;
  property KIND: JString read _GetKIND;
  property MICRO_KIND: Integer read _GetMICRO_KIND;
  property MINI_KIND: Integer read _GetMINI_KIND;
  property VIDEO_ID: JString read _GetVIDEO_ID;
  property WIDTH: JString read _GetWIDTH;
end;

[JavaSignature('android/provider/MediaStore$Video$Thumbnails')]
JVideo_Thumbnails = interface(JObject)
['{67417EE7-B1A0-45B3-A1D2-B6ADFE8A5446}']
end;
TJVideo_Thumbnails = class(TJavaGenericImport<JVideo_ThumbnailsClass, JVideo_Thumbnails>) end;

JRawContacts_StreamItemsClass = interface(JObjectClass)
['{E927EB76-75C6-42C3-9A69-5033BF6B621D}']
  {Property Methods}
  function _GetCONTENT_DIRECTORY: JString;
  {Properties}
  property CONTENT_DIRECTORY: JString read _GetCONTENT_DIRECTORY;
end;

[JavaSignature('android/provider/ContactsContract$RawContacts$StreamItems')]
JRawContacts_StreamItems = interface(JObject)
['{207CD228-EECA-4D59-89B7-A2E030873D3E}']
end;
TJRawContacts_StreamItems = class(TJavaGenericImport<JRawContacts_StreamItemsClass, JRawContacts_StreamItems>) end;

JContacts_EntityClass = interface(JObjectClass)
['{0E60DA08-DCE1-4876-8F1A-30ADCC551FC1}']
  {Property Methods}
  function _GetCONTENT_DIRECTORY: JString;
  function _GetDATA_ID: JString;
  function _GetRAW_CONTACT_ID: JString;
  {Properties}
  property CONTENT_DIRECTORY: JString read _GetCONTENT_DIRECTORY;
  property DATA_ID: JString read _GetDATA_ID;
  property RAW_CONTACT_ID: JString read _GetRAW_CONTACT_ID;
end;

[JavaSignature('android/provider/ContactsContract$Contacts$Entity')]
JContacts_Entity = interface(JObject)
['{E0F3068A-8287-4080-BA0B-A716EEF40A69}']
end;
TJContacts_Entity = class(TJavaGenericImport<JContacts_EntityClass, JContacts_Entity>) end;

JContactsContract_GroupsClass = interface(JObjectClass)
['{8BAC9601-BC22-441B-90A1-57E326B97E09}']
  {Property Methods}
  function _GetCONTENT_ITEM_TYPE: JString;
  function _GetCONTENT_SUMMARY_URI: Jnet_Uri;
  function _GetCONTENT_TYPE: JString;
  function _GetCONTENT_URI: Jnet_Uri;
  {Methods}
  function newEntityIterator(cursor: JCursor): JEntityIterator; cdecl;
  {Properties}
  property CONTENT_ITEM_TYPE: JString read _GetCONTENT_ITEM_TYPE;
  property CONTENT_SUMMARY_URI: Jnet_Uri read _GetCONTENT_SUMMARY_URI;
  property CONTENT_TYPE: JString read _GetCONTENT_TYPE;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
end;

[JavaSignature('android/provider/ContactsContract$Groups')]
JContactsContract_Groups = interface(JObject)
['{73EBA953-50F0-4133-A32B-4CB129246BE9}']
end;
TJContactsContract_Groups = class(TJavaGenericImport<JContactsContract_GroupsClass, JContactsContract_Groups>) end;

JCommonDataKinds_EventClass = interface(JObjectClass)
['{2DD258B5-0F79-4BA8-B1D2-080AA0E1DC54}']
  {Property Methods}
  function _GetCONTENT_ITEM_TYPE: JString;
  function _GetSTART_DATE: JString;
  function _GetTYPE_ANNIVERSARY: Integer;
  function _GetTYPE_BIRTHDAY: Integer;
  function _GetTYPE_OTHER: Integer;
  {Methods}
  function getTypeResource(type_: JInteger): Integer; cdecl;
  {Properties}
  property CONTENT_ITEM_TYPE: JString read _GetCONTENT_ITEM_TYPE;
  property START_DATE: JString read _GetSTART_DATE;
  property TYPE_ANNIVERSARY: Integer read _GetTYPE_ANNIVERSARY;
  property TYPE_BIRTHDAY: Integer read _GetTYPE_BIRTHDAY;
  property TYPE_OTHER: Integer read _GetTYPE_OTHER;
end;

[JavaSignature('android/provider/ContactsContract$CommonDataKinds$Event')]
JCommonDataKinds_Event = interface(JObject)
['{3F9DB079-4945-46C0-9D3E-53705F415401}']
end;
TJCommonDataKinds_Event = class(TJavaGenericImport<JCommonDataKinds_EventClass, JCommonDataKinds_Event>) end;

JCalendarContract_CalendarAlertsClass = interface(JObjectClass)
['{776130ED-7CB2-473E-8A3F-F0F6C58C83FC}']
  {Property Methods}
  function _GetCONTENT_URI: Jnet_Uri;
  function _GetCONTENT_URI_BY_INSTANCE: Jnet_Uri;
  {Properties}
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
  property CONTENT_URI_BY_INSTANCE: Jnet_Uri read _GetCONTENT_URI_BY_INSTANCE;
end;

[JavaSignature('android/provider/CalendarContract$CalendarAlerts')]
JCalendarContract_CalendarAlerts = interface(JObject)
['{EAB59478-59F1-4E35-9288-1A0E560548A1}']
end;
TJCalendarContract_CalendarAlerts = class(TJavaGenericImport<JCalendarContract_CalendarAlertsClass, JCalendarContract_CalendarAlerts>) end;

JVoicemailContract_VoicemailsClass = interface(JObjectClass)
['{F1E6537F-292F-49F3-936A-60A5106676E9}']
  {Property Methods}
  function _GetCONTENT_URI: Jnet_Uri;
  function _GetDATE: JString;
  function _GetDIR_TYPE: JString;
  function _GetDURATION: JString;
  function _GetHAS_CONTENT: JString;
  function _GetIS_READ: JString;
  function _GetITEM_TYPE: JString;
  function _GetMIME_TYPE: JString;
  function _GetNUMBER: JString;
  function _GetSOURCE_DATA: JString;
  function _GetSOURCE_PACKAGE: JString;
  {Methods}
  function buildSourceUri(packageName: JString): Jnet_Uri; cdecl;
  {Properties}
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
  property DATE: JString read _GetDATE;
  property DIR_TYPE: JString read _GetDIR_TYPE;
  property DURATION: JString read _GetDURATION;
  property HAS_CONTENT: JString read _GetHAS_CONTENT;
  property IS_READ: JString read _GetIS_READ;
  property ITEM_TYPE: JString read _GetITEM_TYPE;
  property MIME_TYPE: JString read _GetMIME_TYPE;
  property NUMBER: JString read _GetNUMBER;
  property SOURCE_DATA: JString read _GetSOURCE_DATA;
  property SOURCE_PACKAGE: JString read _GetSOURCE_PACKAGE;
end;

[JavaSignature('android/provider/VoicemailContract$Voicemails')]
JVoicemailContract_Voicemails = interface(JObject)
['{4E57943B-60BB-4159-B61E-8A8390CBB320}']
end;
TJVoicemailContract_Voicemails = class(TJavaGenericImport<JVoicemailContract_VoicemailsClass, JVoicemailContract_Voicemails>) end;

JContacts_DataClass = interface(JObjectClass)
['{8975D2D1-26B6-45B3-BCB6-DDEFA37947A4}']
  {Property Methods}
  function _GetCONTENT_DIRECTORY: JString;
  {Properties}
  property CONTENT_DIRECTORY: JString read _GetCONTENT_DIRECTORY;
end;

[JavaSignature('android/provider/ContactsContract$Contacts$Data')]
JContacts_Data = interface(JObject)
['{4339CD6A-21C0-4A45-B5C0-A35F80BDDA1C}']
end;
TJContacts_Data = class(TJavaGenericImport<JContacts_DataClass, JContacts_Data>) end;

JContactsContract_ProfileSyncStateClass = interface(JObjectClass)
['{0DB260ED-4251-4A79-A10E-B5C1BAEC5834}']
  {Property Methods}
  function _GetCONTENT_DIRECTORY: JString;
  function _GetCONTENT_URI: Jnet_Uri;
  {Properties}
  property CONTENT_DIRECTORY: JString read _GetCONTENT_DIRECTORY;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
end;

[JavaSignature('android/provider/ContactsContract$ProfileSyncState')]
JContactsContract_ProfileSyncState = interface(JObject)
['{72E51072-AF64-48B6-8674-547E0F3E616B}']
end;
TJContactsContract_ProfileSyncState = class(TJavaGenericImport<JContactsContract_ProfileSyncStateClass, JContactsContract_ProfileSyncState>) end;

JIntents_UIClass = interface(JObjectClass)
['{F573A146-D60E-48A3-92CC-28AECCEA7A58}']
  {Property Methods}
  function _GetFILTER_CONTACTS_ACTION: JString;
  function _GetFILTER_TEXT_EXTRA_KEY: JString;
  function _GetGROUP_NAME_EXTRA_KEY: JString;
  function _GetLIST_ALL_CONTACTS_ACTION: JString;
  function _GetLIST_CONTACTS_WITH_PHONES_ACTION: JString;
  function _GetLIST_DEFAULT: JString;
  function _GetLIST_FREQUENT_ACTION: JString;
  function _GetLIST_GROUP_ACTION: JString;
  function _GetLIST_STARRED_ACTION: JString;
  function _GetLIST_STREQUENT_ACTION: JString;
  function _GetTITLE_EXTRA_KEY: JString;
  {Methods}
  function init: JIntents_UI; cdecl;//Deprecated
  {Properties}
  property FILTER_CONTACTS_ACTION: JString read _GetFILTER_CONTACTS_ACTION;
  property FILTER_TEXT_EXTRA_KEY: JString read _GetFILTER_TEXT_EXTRA_KEY;
  property GROUP_NAME_EXTRA_KEY: JString read _GetGROUP_NAME_EXTRA_KEY;
  property LIST_ALL_CONTACTS_ACTION: JString read _GetLIST_ALL_CONTACTS_ACTION;
  property LIST_CONTACTS_WITH_PHONES_ACTION: JString read _GetLIST_CONTACTS_WITH_PHONES_ACTION;
  property LIST_DEFAULT: JString read _GetLIST_DEFAULT;
  property LIST_FREQUENT_ACTION: JString read _GetLIST_FREQUENT_ACTION;
  property LIST_GROUP_ACTION: JString read _GetLIST_GROUP_ACTION;
  property LIST_STARRED_ACTION: JString read _GetLIST_STARRED_ACTION;
  property LIST_STREQUENT_ACTION: JString read _GetLIST_STREQUENT_ACTION;
  property TITLE_EXTRA_KEY: JString read _GetTITLE_EXTRA_KEY;
end;

[JavaSignature('android/provider/Contacts$Intents$UI')]
JIntents_UI = interface(JObject)
['{E21B86C2-6E55-4DF8-ACB9-78362AFEEC1F}']
end;
TJIntents_UI = class(TJavaGenericImport<JIntents_UIClass, JIntents_UI>) end;

JContacts_GroupsColumnsClass = interface(IJavaClass)
['{9F717155-86DF-4B07-8494-815CCE57D0DA}']
  {Property Methods}
  function _GetNAME: JString;
  function _GetNOTES: JString;
  function _GetSHOULD_SYNC: JString;
  function _GetSYSTEM_ID: JString;
  {Properties}
  property NAME: JString read _GetNAME;
  property NOTES: JString read _GetNOTES;
  property SHOULD_SYNC: JString read _GetSHOULD_SYNC;
  property SYSTEM_ID: JString read _GetSYSTEM_ID;
end;

[JavaSignature('android/provider/Contacts$GroupsColumns')]
JContacts_GroupsColumns = interface(IJavaInstance)
['{0DE73B43-0D39-4CB9-A65A-100DCDF8DE94}']
end;
TJContacts_GroupsColumns = class(TJavaGenericImport<JContacts_GroupsColumnsClass, JContacts_GroupsColumns>) end;

JContactsContract_QuickContactClass = interface(JObjectClass)
['{BFFF22CB-54E7-4771-AF1C-DCD44BF0B754}']
  {Property Methods}
  function _GetMODE_LARGE: Integer;
  function _GetMODE_MEDIUM: Integer;
  function _GetMODE_SMALL: Integer;
  {Methods}
  function init: JContactsContract_QuickContact; cdecl;
  procedure showQuickContact(context: JContext; target: JView; lookupUri: Jnet_Uri; mode: Integer; excludeMimes: TJavaObjectArray<JString>); cdecl; overload;
  procedure showQuickContact(context: JContext; target: JRect; lookupUri: Jnet_Uri; mode: Integer; excludeMimes: TJavaObjectArray<JString>); cdecl; overload;
  {Properties}
  property MODE_LARGE: Integer read _GetMODE_LARGE;
  property MODE_MEDIUM: Integer read _GetMODE_MEDIUM;
  property MODE_SMALL: Integer read _GetMODE_SMALL;
end;

[JavaSignature('android/provider/ContactsContract$QuickContact')]
JContactsContract_QuickContact = interface(JObject)
['{58793703-580F-461A-AE92-379CE3C5D499}']
end;
TJContactsContract_QuickContact = class(TJavaGenericImport<JContactsContract_QuickContactClass, JContactsContract_QuickContact>) end;

JMediaStore_VideoClass = interface(JObjectClass)
['{D9A58E17-710A-4A56-9458-9EF0DB46F648}']
  {Property Methods}
  function _GetDEFAULT_SORT_ORDER: JString;
  {Methods}
  function init: JMediaStore_Video; cdecl;
  function query(cr: JContentResolver; uri: Jnet_Uri; projection: TJavaObjectArray<JString>): JCursor; cdecl;
  {Properties}
  property DEFAULT_SORT_ORDER: JString read _GetDEFAULT_SORT_ORDER;
end;

[JavaSignature('android/provider/MediaStore$Video')]
JMediaStore_Video = interface(JObject)
['{CAE402A2-BA43-49F5-AD99-E94DA89BD851}']
end;
TJMediaStore_Video = class(TJavaGenericImport<JMediaStore_VideoClass, JMediaStore_Video>) end;

JIntents_InsertClass = interface(JObjectClass)
['{47D453BB-E6FF-4579-9263-B3154E0ECC6C}']
  {Property Methods}
  function _GetACTION: JString;
  function _GetCOMPANY: JString;
  function _GetEMAIL: JString;
  function _GetEMAIL_ISPRIMARY: JString;
  function _GetEMAIL_TYPE: JString;
  function _GetFULL_MODE: JString;
  function _GetIM_HANDLE: JString;
  function _GetIM_ISPRIMARY: JString;
  function _GetIM_PROTOCOL: JString;
  function _GetJOB_TITLE: JString;
  function _GetNAME: JString;
  function _GetNOTES: JString;
  function _GetPHONE: JString;
  function _GetPHONETIC_NAME: JString;
  function _GetPHONE_ISPRIMARY: JString;
  function _GetPHONE_TYPE: JString;
  function _GetPOSTAL: JString;
  function _GetPOSTAL_ISPRIMARY: JString;
  function _GetPOSTAL_TYPE: JString;
  function _GetSECONDARY_EMAIL: JString;
  function _GetSECONDARY_EMAIL_TYPE: JString;
  function _GetSECONDARY_PHONE: JString;
  function _GetSECONDARY_PHONE_TYPE: JString;
  function _GetTERTIARY_EMAIL: JString;
  function _GetTERTIARY_EMAIL_TYPE: JString;
  function _GetTERTIARY_PHONE: JString;
  function _GetTERTIARY_PHONE_TYPE: JString;
  {Methods}
  function init: JIntents_Insert; cdecl;//Deprecated
  {Properties}
  property ACTION: JString read _GetACTION;
  property COMPANY: JString read _GetCOMPANY;
  property EMAIL: JString read _GetEMAIL;
  property EMAIL_ISPRIMARY: JString read _GetEMAIL_ISPRIMARY;
  property EMAIL_TYPE: JString read _GetEMAIL_TYPE;
  property FULL_MODE: JString read _GetFULL_MODE;
  property IM_HANDLE: JString read _GetIM_HANDLE;
  property IM_ISPRIMARY: JString read _GetIM_ISPRIMARY;
  property IM_PROTOCOL: JString read _GetIM_PROTOCOL;
  property JOB_TITLE: JString read _GetJOB_TITLE;
  property NAME: JString read _GetNAME;
  property NOTES: JString read _GetNOTES;
  property PHONE: JString read _GetPHONE;
  property PHONETIC_NAME: JString read _GetPHONETIC_NAME;
  property PHONE_ISPRIMARY: JString read _GetPHONE_ISPRIMARY;
  property PHONE_TYPE: JString read _GetPHONE_TYPE;
  property POSTAL: JString read _GetPOSTAL;
  property POSTAL_ISPRIMARY: JString read _GetPOSTAL_ISPRIMARY;
  property POSTAL_TYPE: JString read _GetPOSTAL_TYPE;
  property SECONDARY_EMAIL: JString read _GetSECONDARY_EMAIL;
  property SECONDARY_EMAIL_TYPE: JString read _GetSECONDARY_EMAIL_TYPE;
  property SECONDARY_PHONE: JString read _GetSECONDARY_PHONE;
  property SECONDARY_PHONE_TYPE: JString read _GetSECONDARY_PHONE_TYPE;
  property TERTIARY_EMAIL: JString read _GetTERTIARY_EMAIL;
  property TERTIARY_EMAIL_TYPE: JString read _GetTERTIARY_EMAIL_TYPE;
  property TERTIARY_PHONE: JString read _GetTERTIARY_PHONE;
  property TERTIARY_PHONE_TYPE: JString read _GetTERTIARY_PHONE_TYPE;
end;

[JavaSignature('android/provider/Contacts$Intents$Insert')]
JIntents_Insert = interface(JObject)
['{2C3128E1-F765-46FC-A4E9-5B8A570A006F}']
end;
TJIntents_Insert = class(TJavaGenericImport<JIntents_InsertClass, JIntents_Insert>) end;

JCalendarContract_SyncStateClass = interface(JObjectClass)
['{83EFBFA0-5214-4567-8824-3214ECFAE45F}']
  {Property Methods}
  function _GetCONTENT_URI: Jnet_Uri;
  {Properties}
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
end;

[JavaSignature('android/provider/CalendarContract$SyncState')]
JCalendarContract_SyncState = interface(JObject)
['{42477479-2128-49DA-9126-CDF5C5067B35}']
end;
TJCalendarContract_SyncState = class(TJavaGenericImport<JCalendarContract_SyncStateClass, JCalendarContract_SyncState>) end;

JCommonDataKinds_NicknameClass = interface(JObjectClass)
['{BC47785B-5605-4F0C-AB9B-86B1605ABDDC}']
  {Property Methods}
  function _GetCONTENT_ITEM_TYPE: JString;
  function _GetNAME: JString;
  function _GetTYPE_DEFAULT: Integer;
  function _GetTYPE_INITIALS: Integer;
  function _GetTYPE_MAIDEN_NAME: Integer;
  function _GetTYPE_MAINDEN_NAME: Integer;
  function _GetTYPE_OTHER_NAME: Integer;
  function _GetTYPE_SHORT_NAME: Integer;
  {Properties}
  property CONTENT_ITEM_TYPE: JString read _GetCONTENT_ITEM_TYPE;
  property NAME: JString read _GetNAME;
  property TYPE_DEFAULT: Integer read _GetTYPE_DEFAULT;
  property TYPE_INITIALS: Integer read _GetTYPE_INITIALS;
  property TYPE_MAIDEN_NAME: Integer read _GetTYPE_MAIDEN_NAME;
  property TYPE_MAINDEN_NAME: Integer read _GetTYPE_MAINDEN_NAME;
  property TYPE_OTHER_NAME: Integer read _GetTYPE_OTHER_NAME;
  property TYPE_SHORT_NAME: Integer read _GetTYPE_SHORT_NAME;
end;

[JavaSignature('android/provider/ContactsContract$CommonDataKinds$Nickname')]
JCommonDataKinds_Nickname = interface(JObject)
['{3F2247FD-38FB-4830-9686-090B87EE3D8B}']
end;
TJCommonDataKinds_Nickname = class(TJavaGenericImport<JCommonDataKinds_NicknameClass, JCommonDataKinds_Nickname>) end;

JUserDictionaryClass = interface(JObjectClass)
['{AE056B74-BB4E-4A68-AD7A-4972DE859D01}']
  {Property Methods}
  function _GetAUTHORITY: JString;
  function _GetCONTENT_URI: Jnet_Uri;
  {Methods}
  function init: JUserDictionary; cdecl;
  {Properties}
  property AUTHORITY: JString read _GetAUTHORITY;
  property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
end;

[JavaSignature('android/provider/UserDictionary')]
JUserDictionary = interface(JObject)
['{0253D7A9-6E15-406B-A6DD-501E4E31452F}']
end;
TJUserDictionary = class(TJavaGenericImport<JUserDictionaryClass, JUserDictionary>) end;




implementation

begin

end.


