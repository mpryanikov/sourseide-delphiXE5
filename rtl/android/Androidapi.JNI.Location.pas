{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit Androidapi.JNI.Location;

interface

uses 
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
  Androidapi.JNI.App,
  Androidapi.JNI.GraphicsContentViewText;

type

  {Class forward declarations}
  JGpsSatellite = interface;//android.location.GpsSatellite
  JAddress = interface;//android.location.Address
  JLocationListener = interface;//android.location.LocationListener
  JLocationProvider = interface;//android.location.LocationProvider
  JGpsStatus_Listener = interface;//android.location.GpsStatus$Listener
  JLocationManager = interface;//android.location.LocationManager
  JGeocoder = interface;//android.location.Geocoder
  JCriteria = interface;//android.location.Criteria
  JGpsStatus_NmeaListener = interface;//android.location.GpsStatus$NmeaListener
  JGpsStatus = interface;//android.location.GpsStatus
  JLocation = interface;//android.location.Location

JGpsSatelliteClass = interface(JObjectClass)
['{8B2B813E-3F20-40F5-AAFF-CA38016DBF99}']
end;

[JavaSignature('android/location/GpsSatellite')]
JGpsSatellite = interface(JObject)
['{23BDBF98-46F8-46EE-93FB-07D1EB30C7F7}']
  {Methods}
  function getAzimuth: Single; cdecl;
  function getElevation: Single; cdecl;
  function getPrn: Integer; cdecl;
  function getSnr: Single; cdecl;
  function hasAlmanac: Boolean; cdecl;
  function hasEphemeris: Boolean; cdecl;
  function usedInFix: Boolean; cdecl;
end;
TJGpsSatellite = class(TJavaGenericImport<JGpsSatelliteClass, JGpsSatellite>) end;

JAddressClass = interface(JObjectClass)
['{11E2A62D-94CD-4C84-9BAB-651BDB8AD89F}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Methods}
  function init(locale: JLocale): JAddress; cdecl;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/location/Address')]
JAddress = interface(JObject)
['{BEDE6B0A-3FCA-4A73-BC74-0FB86CE8E02F}']
  {Methods}
  procedure clearLatitude; cdecl;
  procedure clearLongitude; cdecl;
  function describeContents: Integer; cdecl;
  function getAddressLine(index: Integer): JString; cdecl;
  function getAdminArea: JString; cdecl;
  function getCountryCode: JString; cdecl;
  function getCountryName: JString; cdecl;
  function getExtras: JBundle; cdecl;
  function getFeatureName: JString; cdecl;
  function getLatitude: Double; cdecl;
  function getLocale: JLocale; cdecl;
  function getLocality: JString; cdecl;
  function getLongitude: Double; cdecl;
  function getMaxAddressLineIndex: Integer; cdecl;
  function getPhone: JString; cdecl;
  function getPostalCode: JString; cdecl;
  function getPremises: JString; cdecl;
  function getSubAdminArea: JString; cdecl;
  function getSubLocality: JString; cdecl;
  function getSubThoroughfare: JString; cdecl;
  function getThoroughfare: JString; cdecl;
  function getUrl: JString; cdecl;
  function hasLatitude: Boolean; cdecl;
  function hasLongitude: Boolean; cdecl;
  procedure setAddressLine(index: Integer; line: JString); cdecl;
  procedure setAdminArea(adminArea: JString); cdecl;
  procedure setCountryCode(countryCode: JString); cdecl;
  procedure setCountryName(countryName: JString); cdecl;
  procedure setExtras(extras: JBundle); cdecl;
  procedure setFeatureName(featureName: JString); cdecl;
  procedure setLatitude(latitude: Double); cdecl;
  procedure setLocality(locality: JString); cdecl;
  procedure setLongitude(longitude: Double); cdecl;
  procedure setPhone(phone: JString); cdecl;
  procedure setPostalCode(postalCode: JString); cdecl;
  procedure setPremises(premises: JString); cdecl;
  procedure setSubAdminArea(subAdminArea: JString); cdecl;
  procedure setSubLocality(sublocality: JString); cdecl;
  procedure setSubThoroughfare(subthoroughfare: JString); cdecl;
  procedure setThoroughfare(thoroughfare: JString); cdecl;
  procedure setUrl(Url: JString); cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(parcel: JParcel; flags: Integer); cdecl;
end;
TJAddress = class(TJavaGenericImport<JAddressClass, JAddress>) end;

JLocationListenerClass = interface(IJavaClass)
['{C0D51209-3DC1-46D5-91C1-CC77097564DB}']
end;

[JavaSignature('android/location/LocationListener')]
JLocationListener = interface(IJavaInstance)
['{D1CF3FB5-3BCB-4959-98D7-BD4D8F93D839}']
  {Methods}
  procedure onLocationChanged(location: JLocation); cdecl;
  procedure onProviderDisabled(provider: JString); cdecl;
  procedure onProviderEnabled(provider: JString); cdecl;
  procedure onStatusChanged(provider: JString; status: Integer; extras: JBundle); cdecl;
end;
TJLocationListener = class(TJavaGenericImport<JLocationListenerClass, JLocationListener>) end;

JLocationProviderClass = interface(JObjectClass)
['{45951F9E-53D1-428F-9B17-954C136F0DAA}']
  {Property Methods}
  function _GetAVAILABLE: Integer;
  function _GetOUT_OF_SERVICE: Integer;
  function _GetTEMPORARILY_UNAVAILABLE: Integer;
  {Properties}
  property AVAILABLE: Integer read _GetAVAILABLE;
  property OUT_OF_SERVICE: Integer read _GetOUT_OF_SERVICE;
  property TEMPORARILY_UNAVAILABLE: Integer read _GetTEMPORARILY_UNAVAILABLE;
end;

[JavaSignature('android/location/LocationProvider')]
JLocationProvider = interface(JObject)
['{1A055914-E219-4691-A746-CBA8DB4BA34F}']
  {Methods}
  function getAccuracy: Integer; cdecl;
  function getName: JString; cdecl;
  function getPowerRequirement: Integer; cdecl;
  function hasMonetaryCost: Boolean; cdecl;
  function meetsCriteria(criteria: JCriteria): Boolean; cdecl;
  function requiresCell: Boolean; cdecl;
  function requiresNetwork: Boolean; cdecl;
  function requiresSatellite: Boolean; cdecl;
  function supportsAltitude: Boolean; cdecl;
  function supportsBearing: Boolean; cdecl;
  function supportsSpeed: Boolean; cdecl;
end;
TJLocationProvider = class(TJavaGenericImport<JLocationProviderClass, JLocationProvider>) end;

JGpsStatus_ListenerClass = interface(IJavaClass)
['{1B4E4734-00D1-4DD8-856F-5B15E4BA31F3}']
end;

[JavaSignature('android/location/GpsStatus$Listener')]
JGpsStatus_Listener = interface(IJavaInstance)
['{12C48E82-7BEA-4E6E-973D-95CD0B42B3FE}']
  {Methods}
  procedure onGpsStatusChanged(event: Integer); cdecl;
end;
TJGpsStatus_Listener = class(TJavaGenericImport<JGpsStatus_ListenerClass, JGpsStatus_Listener>) end;

JLocationManagerClass = interface(JObjectClass)
['{031AE82C-2EEE-41E4-94F8-24C4D7059246}']
  {Property Methods}
  function _GetGPS_PROVIDER: JString;
  function _GetKEY_LOCATION_CHANGED: JString;
  function _GetKEY_PROVIDER_ENABLED: JString;
  function _GetKEY_PROXIMITY_ENTERING: JString;
  function _GetKEY_STATUS_CHANGED: JString;
  function _GetNETWORK_PROVIDER: JString;
  function _GetPASSIVE_PROVIDER: JString;
  function _GetPROVIDERS_CHANGED_ACTION: JString;
  {Properties}
  property GPS_PROVIDER: JString read _GetGPS_PROVIDER;
  property KEY_LOCATION_CHANGED: JString read _GetKEY_LOCATION_CHANGED;
  property KEY_PROVIDER_ENABLED: JString read _GetKEY_PROVIDER_ENABLED;
  property KEY_PROXIMITY_ENTERING: JString read _GetKEY_PROXIMITY_ENTERING;
  property KEY_STATUS_CHANGED: JString read _GetKEY_STATUS_CHANGED;
  property NETWORK_PROVIDER: JString read _GetNETWORK_PROVIDER;
  property PASSIVE_PROVIDER: JString read _GetPASSIVE_PROVIDER;
  property PROVIDERS_CHANGED_ACTION: JString read _GetPROVIDERS_CHANGED_ACTION;
end;

[JavaSignature('android/location/LocationManager')]
JLocationManager = interface(JObject)
['{42C3E256-9370-43B5-93E3-EDF5CE51FB28}']
  {Methods}
  function addGpsStatusListener(listener: JGpsStatus_Listener): Boolean; cdecl;
  function addNmeaListener(listener: JGpsStatus_NmeaListener): Boolean; cdecl;
  procedure addProximityAlert(latitude: Double; longitude: Double; radius: Single; expiration: Int64; intent: JPendingIntent); cdecl;
  procedure addTestProvider(name: JString; requiresNetwork: Boolean; requiresSatellite: Boolean; requiresCell: Boolean; hasMonetaryCost: Boolean; supportsAltitude: Boolean; supportsSpeed: Boolean; supportsBearing: Boolean; powerRequirement: Integer; accuracy: Integer); cdecl;
  procedure clearTestProviderEnabled(provider: JString); cdecl;
  procedure clearTestProviderLocation(provider: JString); cdecl;
  procedure clearTestProviderStatus(provider: JString); cdecl;
  function getAllProviders: JList; cdecl;
  function getBestProvider(criteria: JCriteria; enabledOnly: Boolean): JString; cdecl;
  function getGpsStatus(status: JGpsStatus): JGpsStatus; cdecl;
  function getLastKnownLocation(provider: JString): JLocation; cdecl;
  function getProvider(name: JString): JLocationProvider; cdecl;
  function getProviders(enabledOnly: Boolean): JList; cdecl; overload;
  function getProviders(criteria: JCriteria; enabledOnly: Boolean): JList; cdecl; overload;
  function isProviderEnabled(provider: JString): Boolean; cdecl;
  procedure removeGpsStatusListener(listener: JGpsStatus_Listener); cdecl;
  procedure removeNmeaListener(listener: JGpsStatus_NmeaListener); cdecl;
  procedure removeProximityAlert(intent: JPendingIntent); cdecl;
  procedure removeTestProvider(provider: JString); cdecl;
  procedure removeUpdates(listener: JLocationListener); cdecl; overload;
  procedure removeUpdates(intent: JPendingIntent); cdecl; overload;
  procedure requestLocationUpdates(provider: JString; minTime: Int64; minDistance: Single; listener: JLocationListener); cdecl; overload;
  procedure requestLocationUpdates(provider: JString; minTime: Int64; minDistance: Single; listener: JLocationListener; looper: JLooper); cdecl; overload;
  procedure requestLocationUpdates(minTime: Int64; minDistance: Single; criteria: JCriteria; listener: JLocationListener; looper: JLooper); cdecl; overload;
  procedure requestLocationUpdates(provider: JString; minTime: Int64; minDistance: Single; intent: JPendingIntent); cdecl; overload;
  procedure requestLocationUpdates(minTime: Int64; minDistance: Single; criteria: JCriteria; intent: JPendingIntent); cdecl; overload;
  procedure requestSingleUpdate(provider: JString; listener: JLocationListener; looper: JLooper); cdecl; overload;
  procedure requestSingleUpdate(criteria: JCriteria; listener: JLocationListener; looper: JLooper); cdecl; overload;
  procedure requestSingleUpdate(provider: JString; intent: JPendingIntent); cdecl; overload;
  procedure requestSingleUpdate(criteria: JCriteria; intent: JPendingIntent); cdecl; overload;
  function sendExtraCommand(provider: JString; command: JString; extras: JBundle): Boolean; cdecl;
  procedure setTestProviderEnabled(provider: JString; enabled: Boolean); cdecl;
  procedure setTestProviderLocation(provider: JString; loc: JLocation); cdecl;
  procedure setTestProviderStatus(provider: JString; status: Integer; extras: JBundle; updateTime: Int64); cdecl;
end;
TJLocationManager = class(TJavaGenericImport<JLocationManagerClass, JLocationManager>) end;

JGeocoderClass = interface(JObjectClass)
['{A21F3CB7-F053-464B-B59B-EDD0E4C784F2}']
  {Methods}
  function init(context: JContext; locale: JLocale): JGeocoder; cdecl; overload;
  function init(context: JContext): JGeocoder; cdecl; overload;
  function isPresent: Boolean; cdecl;
end;

[JavaSignature('android/location/Geocoder')]
JGeocoder = interface(JObject)
['{32343023-DA3D-41DE-9A9A-69935156D5B7}']
  {Methods}
  function getFromLocation(latitude: Double; longitude: Double; maxResults: Integer): JList; cdecl;
  function getFromLocationName(locationName: JString; maxResults: Integer): JList; cdecl; overload;
  function getFromLocationName(locationName: JString; maxResults: Integer; lowerLeftLatitude: Double; lowerLeftLongitude: Double; upperRightLatitude: Double; upperRightLongitude: Double): JList; cdecl; overload;
end;
TJGeocoder = class(TJavaGenericImport<JGeocoderClass, JGeocoder>) end;

JCriteriaClass = interface(JObjectClass)
['{5A00B9BC-1915-4D8F-A077-CED7234A64D1}']
  {Property Methods}
  function _GetACCURACY_COARSE: Integer;
  function _GetACCURACY_FINE: Integer;
  function _GetACCURACY_HIGH: Integer;
  function _GetACCURACY_LOW: Integer;
  function _GetACCURACY_MEDIUM: Integer;
  function _GetCREATOR: JParcelable_Creator;
  function _GetNO_REQUIREMENT: Integer;
  function _GetPOWER_HIGH: Integer;
  function _GetPOWER_LOW: Integer;
  function _GetPOWER_MEDIUM: Integer;
  {Methods}
  function init: JCriteria; cdecl; overload;
  function init(criteria: JCriteria): JCriteria; cdecl; overload;
  {Properties}
  property ACCURACY_COARSE: Integer read _GetACCURACY_COARSE;
  property ACCURACY_FINE: Integer read _GetACCURACY_FINE;
  property ACCURACY_HIGH: Integer read _GetACCURACY_HIGH;
  property ACCURACY_LOW: Integer read _GetACCURACY_LOW;
  property ACCURACY_MEDIUM: Integer read _GetACCURACY_MEDIUM;
  property CREATOR: JParcelable_Creator read _GetCREATOR;
  property NO_REQUIREMENT: Integer read _GetNO_REQUIREMENT;
  property POWER_HIGH: Integer read _GetPOWER_HIGH;
  property POWER_LOW: Integer read _GetPOWER_LOW;
  property POWER_MEDIUM: Integer read _GetPOWER_MEDIUM;
end;

[JavaSignature('android/location/Criteria')]
JCriteria = interface(JObject)
['{105327BB-E655-4565-BCB8-796CB73B811C}']
  {Methods}
  function describeContents: Integer; cdecl;
  function getAccuracy: Integer; cdecl;
  function getBearingAccuracy: Integer; cdecl;
  function getHorizontalAccuracy: Integer; cdecl;
  function getPowerRequirement: Integer; cdecl;
  function getSpeedAccuracy: Integer; cdecl;
  function getVerticalAccuracy: Integer; cdecl;
  function isAltitudeRequired: Boolean; cdecl;
  function isBearingRequired: Boolean; cdecl;
  function isCostAllowed: Boolean; cdecl;
  function isSpeedRequired: Boolean; cdecl;
  procedure setAccuracy(accuracy: Integer); cdecl;
  procedure setAltitudeRequired(altitudeRequired: Boolean); cdecl;
  procedure setBearingAccuracy(accuracy: Integer); cdecl;
  procedure setBearingRequired(bearingRequired: Boolean); cdecl;
  procedure setCostAllowed(costAllowed: Boolean); cdecl;
  procedure setHorizontalAccuracy(accuracy: Integer); cdecl;
  procedure setPowerRequirement(level: Integer); cdecl;
  procedure setSpeedAccuracy(accuracy: Integer); cdecl;
  procedure setSpeedRequired(speedRequired: Boolean); cdecl;
  procedure setVerticalAccuracy(accuracy: Integer); cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(parcel: JParcel; flags: Integer); cdecl;
end;
TJCriteria = class(TJavaGenericImport<JCriteriaClass, JCriteria>) end;

JGpsStatus_NmeaListenerClass = interface(IJavaClass)
['{19E69909-A8B8-4723-866A-49435EC8D942}']
end;

[JavaSignature('android/location/GpsStatus$NmeaListener')]
JGpsStatus_NmeaListener = interface(IJavaInstance)
['{7CEBB6D7-85A9-40EA-9360-BE3E6124336B}']
  {Methods}
  procedure onNmeaReceived(timestamp: Int64; nmea: JString); cdecl;
end;
TJGpsStatus_NmeaListener = class(TJavaGenericImport<JGpsStatus_NmeaListenerClass, JGpsStatus_NmeaListener>) end;

JGpsStatusClass = interface(JObjectClass)
['{875EC1CA-A26D-41DD-83C3-4C361DE1EA50}']
  {Property Methods}
  function _GetGPS_EVENT_FIRST_FIX: Integer;
  function _GetGPS_EVENT_SATELLITE_STATUS: Integer;
  function _GetGPS_EVENT_STARTED: Integer;
  function _GetGPS_EVENT_STOPPED: Integer;
  {Properties}
  property GPS_EVENT_FIRST_FIX: Integer read _GetGPS_EVENT_FIRST_FIX;
  property GPS_EVENT_SATELLITE_STATUS: Integer read _GetGPS_EVENT_SATELLITE_STATUS;
  property GPS_EVENT_STARTED: Integer read _GetGPS_EVENT_STARTED;
  property GPS_EVENT_STOPPED: Integer read _GetGPS_EVENT_STOPPED;
end;

[JavaSignature('android/location/GpsStatus')]
JGpsStatus = interface(JObject)
['{6ED3905E-5FBC-4E7C-A363-10A07297FF5E}']
  {Methods}
  function getMaxSatellites: Integer; cdecl;
  function getSatellites: JIterable; cdecl;
  function getTimeToFirstFix: Integer; cdecl;
end;
TJGpsStatus = class(TJavaGenericImport<JGpsStatusClass, JGpsStatus>) end;

JLocationClass = interface(JObjectClass)
['{A0BF1527-8922-4124-94AC-D2361F1EF984}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  function _GetFORMAT_DEGREES: Integer;
  function _GetFORMAT_MINUTES: Integer;
  function _GetFORMAT_SECONDS: Integer;
  {Methods}
  function init(provider: JString): JLocation; cdecl; overload;
  function init(l: JLocation): JLocation; cdecl; overload;
  function convert(coordinate: Double; outputType: Integer): JString; cdecl; overload;
  function convert(coordinate: JString): Double; cdecl; overload;
  procedure distanceBetween(startLatitude: Double; startLongitude: Double; endLatitude: Double; endLongitude: Double; results: TJavaArray<Single>); cdecl;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
  property FORMAT_DEGREES: Integer read _GetFORMAT_DEGREES;
  property FORMAT_MINUTES: Integer read _GetFORMAT_MINUTES;
  property FORMAT_SECONDS: Integer read _GetFORMAT_SECONDS;
end;

[JavaSignature('android/location/Location')]
JLocation = interface(JObject)
['{B6A7FB34-312C-41BC-B02D-0107E6F39395}']
  {Methods}
  function bearingTo(dest: JLocation): Single; cdecl;
  function describeContents: Integer; cdecl;
  function distanceTo(dest: JLocation): Single; cdecl;
  function getAccuracy: Single; cdecl;
  function getAltitude: Double; cdecl;
  function getBearing: Single; cdecl;
  function getElapsedRealtimeNanos: Int64; cdecl;
  function getExtras: JBundle; cdecl;
  function getLatitude: Double; cdecl;
  function getLongitude: Double; cdecl;
  function getProvider: JString; cdecl;
  function getSpeed: Single; cdecl;
  function getTime: Int64; cdecl;
  function hasAccuracy: Boolean; cdecl;
  function hasAltitude: Boolean; cdecl;
  function hasBearing: Boolean; cdecl;
  function hasSpeed: Boolean; cdecl;
  procedure removeAccuracy; cdecl;
  procedure removeAltitude; cdecl;
  procedure removeBearing; cdecl;
  procedure removeSpeed; cdecl;
  procedure reset; cdecl;
  procedure &set(l: JLocation); cdecl;
  procedure setAccuracy(accuracy: Single); cdecl;
  procedure setAltitude(altitude: Double); cdecl;
  procedure setBearing(bearing: Single); cdecl;
  procedure setElapsedRealtimeNanos(time: Int64); cdecl;
  procedure setExtras(extras: JBundle); cdecl;
  procedure setLatitude(latitude: Double); cdecl;
  procedure setLongitude(longitude: Double); cdecl;
  procedure setProvider(provider: JString); cdecl;
  procedure setSpeed(speed: Single); cdecl;
  procedure setTime(time: Int64); cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(parcel: JParcel; flags: Integer); cdecl;
end;
TJLocation = class(TJavaGenericImport<JLocationClass, JLocation>) end;




implementation

begin

end.


