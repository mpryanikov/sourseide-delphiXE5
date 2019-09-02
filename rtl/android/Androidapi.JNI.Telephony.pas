{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit Androidapi.JNI.Telephony;

interface

uses 
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
  Androidapi.JNI.App,
  Androidapi.JNI.GraphicsContentViewText;

type

  {Class forward declarations}
  JCellSignalStrength = interface;//android.telephony.CellSignalStrength
  JCellSignalStrengthLte = interface;//android.telephony.CellSignalStrengthLte
  JSmsMessage = interface;//android.telephony.SmsMessage
  JCellInfo = interface;//android.telephony.CellInfo
  JCellInfoGsm = interface;//android.telephony.CellInfoGsm
  JPhoneNumberUtils = interface;//android.telephony.PhoneNumberUtils
  JSmsMessage_SubmitPdu = interface;//android.telephony.SmsMessage$SubmitPdu
  JSmsMessage_MessageClass = interface;//android.telephony.SmsMessage$MessageClass
  JCellLocation = interface;//android.telephony.CellLocation
  JGsmCellLocation = interface;//android.telephony.gsm.GsmCellLocation
  JCellIdentityCdma = interface;//android.telephony.CellIdentityCdma
  JCdmaCellLocation = interface;//android.telephony.cdma.CdmaCellLocation
  JNeighboringCellInfo = interface;//android.telephony.NeighboringCellInfo
  JCellInfoCdma = interface;//android.telephony.CellInfoCdma
  JCellInfoLte = interface;//android.telephony.CellInfoLte
  JPhoneStateListener = interface;//android.telephony.PhoneStateListener
  JCustomPhoneStateListener = interface;//android.telephony.CustomPhoneStateListener
  JSignalStrength = interface;//android.telephony.SignalStrength
  Jgsm_SmsMessage_SubmitPdu = interface;//android.telephony.gsm.SmsMessage$SubmitPdu
  JCellSignalStrengthGsm = interface;//android.telephony.CellSignalStrengthGsm
  Jgsm_SmsMessage_MessageClass = interface;//android.telephony.gsm.SmsMessage$MessageClass
  JCellIdentityLte = interface;//android.telephony.CellIdentityLte
  JCellIdentityGsm = interface;//android.telephony.CellIdentityGsm
  JTelephonyManager = interface;//android.telephony.TelephonyManager
  JICustomPhoneStateListener = interface;//android.telephony.ICustomPhoneStateListener
  JSmsManager = interface;//android.telephony.SmsManager
  JCellSignalStrengthCdma = interface;//android.telephony.CellSignalStrengthCdma
  JServiceState = interface;//android.telephony.ServiceState
  Jgsm_SmsManager = interface;//android.telephony.gsm.SmsManager
  JPhoneNumberFormattingTextWatcher = interface;//android.telephony.PhoneNumberFormattingTextWatcher
  Jgsm_SmsMessage = interface;//android.telephony.gsm.SmsMessage

JCellSignalStrengthClass = interface(JObjectClass)
['{2E408E0B-E6EF-450A-BE16-FE6258E458B9}']
end;

[JavaSignature('android/telephony/CellSignalStrength')]
JCellSignalStrength = interface(JObject)
['{E28FDB30-AFA1-40FC-98B8-C2267E1532E0}']
  {Methods}
  function equals(o: JObject): Boolean; cdecl;
  function getAsuLevel: Integer; cdecl;
  function getDbm: Integer; cdecl;
  function getLevel: Integer; cdecl;
  function hashCode: Integer; cdecl;
end;
TJCellSignalStrength = class(TJavaGenericImport<JCellSignalStrengthClass, JCellSignalStrength>) end;

JCellSignalStrengthLteClass = interface(JCellSignalStrengthClass)
['{B2AEC25C-986C-49A8-85F0-5CF96A9E0856}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/telephony/CellSignalStrengthLte')]
JCellSignalStrengthLte = interface(JCellSignalStrength)
['{C0E93090-5675-481F-8BB4-5642161E3393}']
  {Methods}
  function describeContents: Integer; cdecl;
  function equals(o: JObject): Boolean; cdecl;
  function getAsuLevel: Integer; cdecl;
  function getDbm: Integer; cdecl;
  function getLevel: Integer; cdecl;
  function getTimingAdvance: Integer; cdecl;
  function hashCode: Integer; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
end;
TJCellSignalStrengthLte = class(TJavaGenericImport<JCellSignalStrengthLteClass, JCellSignalStrengthLte>) end;

JSmsMessageClass = interface(JObjectClass)
['{227C1E7B-3064-474A-8930-D244309E7D27}']
  {Property Methods}
  function _GetENCODING_16BIT: Integer;
  function _GetENCODING_7BIT: Integer;
  function _GetENCODING_8BIT: Integer;
  function _GetENCODING_UNKNOWN: Integer;
  function _GetMAX_USER_DATA_BYTES: Integer;
  function _GetMAX_USER_DATA_BYTES_WITH_HEADER: Integer;
  function _GetMAX_USER_DATA_SEPTETS: Integer;
  function _GetMAX_USER_DATA_SEPTETS_WITH_HEADER: Integer;
  {Methods}
  function calculateLength(msgBody: JCharSequence; use7bitOnly: Boolean): TJavaArray<Integer>; cdecl; overload;
  function calculateLength(messageBody: JString; use7bitOnly: Boolean): TJavaArray<Integer>; cdecl; overload;
  function createFromPdu(pdu: TJavaArray<Byte>): JSmsMessage; cdecl;
  function getSubmitPdu(scAddress: JString; destinationAddress: JString; message: JString; statusReportRequested: Boolean): JSmsMessage_SubmitPdu; cdecl; overload;
  function getSubmitPdu(scAddress: JString; destinationAddress: JString; destinationPort: SmallInt; data: TJavaArray<Byte>; statusReportRequested: Boolean): JSmsMessage_SubmitPdu; cdecl; overload;
  function getTPLayerLengthForPDU(pdu: JString): Integer; cdecl;
  {Properties}
  property ENCODING_16BIT: Integer read _GetENCODING_16BIT;
  property ENCODING_7BIT: Integer read _GetENCODING_7BIT;
  property ENCODING_8BIT: Integer read _GetENCODING_8BIT;
  property ENCODING_UNKNOWN: Integer read _GetENCODING_UNKNOWN;
  property MAX_USER_DATA_BYTES: Integer read _GetMAX_USER_DATA_BYTES;
  property MAX_USER_DATA_BYTES_WITH_HEADER: Integer read _GetMAX_USER_DATA_BYTES_WITH_HEADER;
  property MAX_USER_DATA_SEPTETS: Integer read _GetMAX_USER_DATA_SEPTETS;
  property MAX_USER_DATA_SEPTETS_WITH_HEADER: Integer read _GetMAX_USER_DATA_SEPTETS_WITH_HEADER;
end;

[JavaSignature('android/telephony/SmsMessage')]
JSmsMessage = interface(JObject)
['{012AA641-B8DD-4F6A-9056-98498FB0B05C}']
  {Methods}
  function getDisplayMessageBody: JString; cdecl;
  function getDisplayOriginatingAddress: JString; cdecl;
  function getEmailBody: JString; cdecl;
  function getEmailFrom: JString; cdecl;
  function getIndexOnIcc: Integer; cdecl;
  function getIndexOnSim: Integer; cdecl;//Deprecated
  function getMessageBody: JString; cdecl;
  function getMessageClass: JSmsMessage_MessageClass; cdecl;
  function getOriginatingAddress: JString; cdecl;
  function getPdu: TJavaArray<Byte>; cdecl;
  function getProtocolIdentifier: Integer; cdecl;
  function getPseudoSubject: JString; cdecl;
  function getServiceCenterAddress: JString; cdecl;
  function getStatus: Integer; cdecl;
  function getStatusOnIcc: Integer; cdecl;
  function getStatusOnSim: Integer; cdecl;//Deprecated
  function getTimestampMillis: Int64; cdecl;
  function getUserData: TJavaArray<Byte>; cdecl;
  function isCphsMwiMessage: Boolean; cdecl;
  function isEmail: Boolean; cdecl;
  function isMWIClearMessage: Boolean; cdecl;
  function isMWISetMessage: Boolean; cdecl;
  function isMwiDontStore: Boolean; cdecl;
  function isReplace: Boolean; cdecl;
  function isReplyPathPresent: Boolean; cdecl;
  function isStatusReportMessage: Boolean; cdecl;
end;
TJSmsMessage = class(TJavaGenericImport<JSmsMessageClass, JSmsMessage>) end;

JCellInfoClass = interface(JObjectClass)
['{D5A9AC63-0BEA-45CB-A8E3-6DCBA1B9FD89}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/telephony/CellInfo')]
JCellInfo = interface(JObject)
['{726F121A-E75C-4A37-B6CB-9DA886794FDF}']
  {Methods}
  function describeContents: Integer; cdecl;
  function equals(other: JObject): Boolean; cdecl;
  function getTimeStamp: Int64; cdecl;
  function hashCode: Integer; cdecl;
  function isRegistered: Boolean; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
end;
TJCellInfo = class(TJavaGenericImport<JCellInfoClass, JCellInfo>) end;

JCellInfoGsmClass = interface(JCellInfoClass)
['{EF7AB5EA-EE8E-42F6-8652-E50E630E46B4}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Properties}
{CREATOR is defined in parent interface}
end;

[JavaSignature('android/telephony/CellInfoGsm')]
JCellInfoGsm = interface(JCellInfo)
['{0E1045BB-B110-4AC7-803F-1BECCD8F9C3C}']
  {Methods}
  function describeContents: Integer; cdecl;
  function equals(other: JObject): Boolean; cdecl;
  function getCellIdentity: JCellIdentityGsm; cdecl;
  function getCellSignalStrength: JCellSignalStrengthGsm; cdecl;
  function hashCode: Integer; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
end;
TJCellInfoGsm = class(TJavaGenericImport<JCellInfoGsmClass, JCellInfoGsm>) end;

JPhoneNumberUtilsClass = interface(JObjectClass)
['{B641F06B-F83E-4853-BAA9-1939B5B5C231}']
  {Property Methods}
  function _GetFORMAT_JAPAN: Integer;
  function _GetFORMAT_NANP: Integer;
  function _GetFORMAT_UNKNOWN: Integer;
  function _GetPAUSE: Char;
  function _GetTOA_International: Integer;
  function _GetTOA_Unknown: Integer;
  function _GetWAIT: Char;
  function _GetWILD: Char;
  {Methods}
  function init: JPhoneNumberUtils; cdecl;
  function calledPartyBCDFragmentToString(bytes: TJavaArray<Byte>; offset: Integer; length: Integer): JString; cdecl;
  function calledPartyBCDToString(bytes: TJavaArray<Byte>; offset: Integer; length: Integer): JString; cdecl;
  function compare(a: JString; b: JString): Boolean; cdecl; overload;
  function compare(context: JContext; a: JString; b: JString): Boolean; cdecl; overload;
  function convertKeypadLettersToDigits(input: JString): JString; cdecl;
  function extractNetworkPortion(phoneNumber: JString): JString; cdecl;
  function extractPostDialPortion(phoneNumber: JString): JString; cdecl;
  procedure formatJapaneseNumber(text: JEditable); cdecl;
  procedure formatNanpNumber(text: JEditable); cdecl;
  function formatNumber(source: JString): JString; cdecl; overload;
  procedure formatNumber(text: JEditable; defaultFormattingType: Integer); cdecl; overload;
  function getFormatTypeForLocale(locale: JLocale): Integer; cdecl;
  function getNumberFromIntent(intent: JIntent; context: JContext): JString; cdecl;
  function getStrippedReversed(phoneNumber: JString): JString; cdecl;
  function is12Key(c: Char): Boolean; cdecl;
  function isDialable(c: Char): Boolean; cdecl;
  function isEmergencyNumber(number: JString): Boolean; cdecl;
  function isGlobalPhoneNumber(phoneNumber: JString): Boolean; cdecl;
  function isISODigit(c: Char): Boolean; cdecl;
  function isNonSeparator(c: Char): Boolean; cdecl;
  function isReallyDialable(c: Char): Boolean; cdecl;
  function isStartsPostDial(c: Char): Boolean; cdecl;
  function isWellFormedSmsAddress(address: JString): Boolean; cdecl;
  function networkPortionToCalledPartyBCD(s: JString): TJavaArray<Byte>; cdecl;
  function networkPortionToCalledPartyBCDWithLength(s: JString): TJavaArray<Byte>; cdecl;
  function numberToCalledPartyBCD(number: JString): TJavaArray<Byte>; cdecl;
  function stringFromStringAndTOA(s: JString; TOA: Integer): JString; cdecl;
  function stripSeparators(phoneNumber: JString): JString; cdecl;
  function toCallerIDMinMatch(phoneNumber: JString): JString; cdecl;
  function toaFromString(s: JString): Integer; cdecl;
  {Properties}
  property FORMAT_JAPAN: Integer read _GetFORMAT_JAPAN;
  property FORMAT_NANP: Integer read _GetFORMAT_NANP;
  property FORMAT_UNKNOWN: Integer read _GetFORMAT_UNKNOWN;
  property PAUSE: Char read _GetPAUSE;
  property TOA_International: Integer read _GetTOA_International;
  property TOA_Unknown: Integer read _GetTOA_Unknown;
  property WAIT: Char read _GetWAIT;
  property WILD: Char read _GetWILD;
end;

[JavaSignature('android/telephony/PhoneNumberUtils')]
JPhoneNumberUtils = interface(JObject)
['{8217405F-564B-4CA0-AF0A-3FEEE87A0444}']
end;
TJPhoneNumberUtils = class(TJavaGenericImport<JPhoneNumberUtilsClass, JPhoneNumberUtils>) end;

JSmsMessage_SubmitPduClass = interface(JObjectClass)
['{3B92B0B3-1E7C-418F-B5DD-222AD433A5AC}']
end;

[JavaSignature('android/telephony/SmsMessage$SubmitPdu')]
JSmsMessage_SubmitPdu = interface(JObject)
['{D69C5B7B-6E92-4599-A3ED-EBB174FEDB68}']
  {Property Methods}
  function _GetencodedMessage: TJavaArray<Byte>;
  procedure _SetencodedMessage(Value: TJavaArray<Byte>);
  function _GetencodedScAddress: TJavaArray<Byte>;
  procedure _SetencodedScAddress(Value: TJavaArray<Byte>);
  {Methods}
  function toString: JString; cdecl;
  {Properties}
  property encodedMessage: TJavaArray<Byte> read _GetencodedMessage write _SetencodedMessage;
  property encodedScAddress: TJavaArray<Byte> read _GetencodedScAddress write _SetencodedScAddress;
end;
TJSmsMessage_SubmitPdu = class(TJavaGenericImport<JSmsMessage_SubmitPduClass, JSmsMessage_SubmitPdu>) end;

JSmsMessage_MessageClassClass = interface(JEnumClass)
['{02240F3E-83D9-4ED3-9C44-6F739AD319F1}']
  {Property Methods}
  function _GetCLASS_0: JSmsMessage_MessageClass;
  function _GetCLASS_1: JSmsMessage_MessageClass;
  function _GetCLASS_2: JSmsMessage_MessageClass;
  function _GetCLASS_3: JSmsMessage_MessageClass;
  function _GetUNKNOWN: JSmsMessage_MessageClass;
  {Methods}
  function valueOf(name: JString): JSmsMessage_MessageClass; cdecl;
  function values: TJavaObjectArray<JSmsMessage_MessageClass>; cdecl;
  {Properties}
  property CLASS_0: JSmsMessage_MessageClass read _GetCLASS_0;
  property CLASS_1: JSmsMessage_MessageClass read _GetCLASS_1;
  property CLASS_2: JSmsMessage_MessageClass read _GetCLASS_2;
  property CLASS_3: JSmsMessage_MessageClass read _GetCLASS_3;
  property UNKNOWN: JSmsMessage_MessageClass read _GetUNKNOWN;
end;

[JavaSignature('android/telephony/SmsMessage$MessageClass')]
JSmsMessage_MessageClass = interface(JEnum)
['{A7AA6F95-590D-48F9-9EAD-7DEF27FB0AD3}']
end;
TJSmsMessage_MessageClass = class(TJavaGenericImport<JSmsMessage_MessageClassClass, JSmsMessage_MessageClass>) end;

JCellLocationClass = interface(JObjectClass)
['{048C4A3E-4C00-40E1-A471-364959ED7986}']
  {Methods}
  function init: JCellLocation; cdecl;
  function getEmpty: JCellLocation; cdecl;
  procedure requestLocationUpdate; cdecl;
end;

[JavaSignature('android/telephony/CellLocation')]
JCellLocation = interface(JObject)
['{769C920D-0E68-4A1D-ABA3-42894B5742C8}']
end;
TJCellLocation = class(TJavaGenericImport<JCellLocationClass, JCellLocation>) end;

JGsmCellLocationClass = interface(JCellLocationClass)
['{31FDA1F7-0041-4FF2-91D2-F15D974EAC2C}']
  {Methods}
  function init: JGsmCellLocation; cdecl; overload;
  function init(bundle: JBundle): JGsmCellLocation; cdecl; overload;
end;

[JavaSignature('android/telephony/gsm/GsmCellLocation')]
JGsmCellLocation = interface(JCellLocation)
['{D09EE10D-C54B-40AB-AE80-461A6AC0DB66}']
  {Methods}
  function equals(o: JObject): Boolean; cdecl;
  procedure fillInNotifierBundle(m: JBundle); cdecl;
  function getCid: Integer; cdecl;
  function getLac: Integer; cdecl;
  function getPsc: Integer; cdecl;
  function hashCode: Integer; cdecl;
  procedure setLacAndCid(lac: Integer; cid: Integer); cdecl;
  procedure setStateInvalid; cdecl;
  function toString: JString; cdecl;
end;
TJGsmCellLocation = class(TJavaGenericImport<JGsmCellLocationClass, JGsmCellLocation>) end;

JCellIdentityCdmaClass = interface(JObjectClass)
['{47D53089-08D3-430B-8D56-2B54959C6F8A}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/telephony/CellIdentityCdma')]
JCellIdentityCdma = interface(JObject)
['{D2456865-988D-40AE-B4B3-42471181056E}']
  {Methods}
  function describeContents: Integer; cdecl;
  function equals(other: JObject): Boolean; cdecl;
  function getBasestationId: Integer; cdecl;
  function getLatitude: Integer; cdecl;
  function getLongitude: Integer; cdecl;
  function getNetworkId: Integer; cdecl;
  function getSystemId: Integer; cdecl;
  function hashCode: Integer; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
end;
TJCellIdentityCdma = class(TJavaGenericImport<JCellIdentityCdmaClass, JCellIdentityCdma>) end;

JCdmaCellLocationClass = interface(JCellLocationClass)
['{0287239E-831E-4687-B06D-87243B3FC994}']
  {Methods}
  function init: JCdmaCellLocation; cdecl; overload;
  function init(bundle: JBundle): JCdmaCellLocation; cdecl; overload;
  function convertQuartSecToDecDegrees(quartSec: Integer): Double; cdecl;
end;

[JavaSignature('android/telephony/cdma/CdmaCellLocation')]
JCdmaCellLocation = interface(JCellLocation)
['{30F5155B-56EB-4449-A3B2-90BFE8F170EB}']
  {Methods}
  function equals(o: JObject): Boolean; cdecl;
  procedure fillInNotifierBundle(bundleToFill: JBundle); cdecl;
  function getBaseStationId: Integer; cdecl;
  function getBaseStationLatitude: Integer; cdecl;
  function getBaseStationLongitude: Integer; cdecl;
  function getNetworkId: Integer; cdecl;
  function getSystemId: Integer; cdecl;
  function hashCode: Integer; cdecl;
  procedure setCellLocationData(baseStationId: Integer; baseStationLatitude: Integer; baseStationLongitude: Integer); cdecl; overload;
  procedure setCellLocationData(baseStationId: Integer; baseStationLatitude: Integer; baseStationLongitude: Integer; systemId: Integer; networkId: Integer); cdecl; overload;
  procedure setStateInvalid; cdecl;
  function toString: JString; cdecl;
end;
TJCdmaCellLocation = class(TJavaGenericImport<JCdmaCellLocationClass, JCdmaCellLocation>) end;

JNeighboringCellInfoClass = interface(JObjectClass)
['{EB3E8F37-13C7-4E29-9015-FE80FD8BD4A7}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  function _GetUNKNOWN_CID: Integer;
  function _GetUNKNOWN_RSSI: Integer;
  {Methods}
  function init: JNeighboringCellInfo; cdecl; overload;//Deprecated
  function init(rssi: Integer; cid: Integer): JNeighboringCellInfo; cdecl; overload;//Deprecated
  function init(rssi: Integer; location: JString; radioType: Integer): JNeighboringCellInfo; cdecl; overload;
  function init(in_: JParcel): JNeighboringCellInfo; cdecl; overload;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
  property UNKNOWN_CID: Integer read _GetUNKNOWN_CID;
  property UNKNOWN_RSSI: Integer read _GetUNKNOWN_RSSI;
end;

[JavaSignature('android/telephony/NeighboringCellInfo')]
JNeighboringCellInfo = interface(JObject)
['{8BF5BF7B-3E55-4C97-A2A5-11C1D079EF73}']
  {Methods}
  function describeContents: Integer; cdecl;
  function getCid: Integer; cdecl;
  function getLac: Integer; cdecl;
  function getNetworkType: Integer; cdecl;
  function getPsc: Integer; cdecl;
  function getRssi: Integer; cdecl;
  procedure setCid(cid: Integer); cdecl;//Deprecated
  procedure setRssi(rssi: Integer); cdecl;//Deprecated
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
end;
TJNeighboringCellInfo = class(TJavaGenericImport<JNeighboringCellInfoClass, JNeighboringCellInfo>) end;

JCellInfoCdmaClass = interface(JCellInfoClass)
['{553A50F7-6149-45ED-9077-17F3A93B024A}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Properties}
{CREATOR is defined in parent interface}
end;

[JavaSignature('android/telephony/CellInfoCdma')]
JCellInfoCdma = interface(JCellInfo)
['{AF6F61BE-45A9-46D4-A45F-D1B71A4F7152}']
  {Methods}
  function describeContents: Integer; cdecl;
  function equals(other: JObject): Boolean; cdecl;
  function getCellIdentity: JCellIdentityCdma; cdecl;
  function getCellSignalStrength: JCellSignalStrengthCdma; cdecl;
  function hashCode: Integer; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
end;
TJCellInfoCdma = class(TJavaGenericImport<JCellInfoCdmaClass, JCellInfoCdma>) end;

JCellInfoLteClass = interface(JCellInfoClass)
['{ED725D53-684B-4A2F-814F-9A9650AAF0F4}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Properties}
{CREATOR is defined in parent interface}
end;

[JavaSignature('android/telephony/CellInfoLte')]
JCellInfoLte = interface(JCellInfo)
['{8C7BB823-B037-49B8-8FBA-8C8AFE1F5177}']
  {Methods}
  function describeContents: Integer; cdecl;
  function equals(other: JObject): Boolean; cdecl;
  function getCellIdentity: JCellIdentityLte; cdecl;
  function getCellSignalStrength: JCellSignalStrengthLte; cdecl;
  function hashCode: Integer; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
end;
TJCellInfoLte = class(TJavaGenericImport<JCellInfoLteClass, JCellInfoLte>) end;

JPhoneStateListenerClass = interface(JObjectClass)
['{897EC9F9-957F-4942-859D-50BCED64009B}']
  {Property Methods}
  function _GetLISTEN_CALL_FORWARDING_INDICATOR: Integer;
  function _GetLISTEN_CALL_STATE: Integer;
  function _GetLISTEN_CELL_INFO: Integer;
  function _GetLISTEN_CELL_LOCATION: Integer;
  function _GetLISTEN_DATA_ACTIVITY: Integer;
  function _GetLISTEN_DATA_CONNECTION_STATE: Integer;
  function _GetLISTEN_MESSAGE_WAITING_INDICATOR: Integer;
  function _GetLISTEN_NONE: Integer;
  function _GetLISTEN_SERVICE_STATE: Integer;
  function _GetLISTEN_SIGNAL_STRENGTH: Integer;
  function _GetLISTEN_SIGNAL_STRENGTHS: Integer;
  {Methods}
  function init: JPhoneStateListener; cdecl;
  {Properties}
  property LISTEN_CALL_FORWARDING_INDICATOR: Integer read _GetLISTEN_CALL_FORWARDING_INDICATOR;
  property LISTEN_CALL_STATE: Integer read _GetLISTEN_CALL_STATE;
  property LISTEN_CELL_INFO: Integer read _GetLISTEN_CELL_INFO;
  property LISTEN_CELL_LOCATION: Integer read _GetLISTEN_CELL_LOCATION;
  property LISTEN_DATA_ACTIVITY: Integer read _GetLISTEN_DATA_ACTIVITY;
  property LISTEN_DATA_CONNECTION_STATE: Integer read _GetLISTEN_DATA_CONNECTION_STATE;
  property LISTEN_MESSAGE_WAITING_INDICATOR: Integer read _GetLISTEN_MESSAGE_WAITING_INDICATOR;
  property LISTEN_NONE: Integer read _GetLISTEN_NONE;
  property LISTEN_SERVICE_STATE: Integer read _GetLISTEN_SERVICE_STATE;
  property LISTEN_SIGNAL_STRENGTH: Integer read _GetLISTEN_SIGNAL_STRENGTH;
  property LISTEN_SIGNAL_STRENGTHS: Integer read _GetLISTEN_SIGNAL_STRENGTHS;
end;

[JavaSignature('android/telephony/PhoneStateListener')]
JPhoneStateListener = interface(JObject)
['{50CE10E1-C584-4145-BC72-65BFA48BB281}']
  {Methods}
  procedure onCallForwardingIndicatorChanged(cfi: Boolean); cdecl;
  procedure onCallStateChanged(state: Integer; incomingNumber: JString); cdecl;
  procedure onCellInfoChanged(cellInfo: JList); cdecl;
  procedure onCellLocationChanged(location: JCellLocation); cdecl;
  procedure onDataActivity(direction: Integer); cdecl;
  procedure onDataConnectionStateChanged(state: Integer); cdecl; overload;
  procedure onDataConnectionStateChanged(state: Integer; networkType: Integer); cdecl; overload;
  procedure onMessageWaitingIndicatorChanged(mwi: Boolean); cdecl;
  procedure onServiceStateChanged(serviceState: JServiceState); cdecl;
  procedure onSignalStrengthChanged(asu: Integer); cdecl;//Deprecated
  procedure onSignalStrengthsChanged(signalStrength: JSignalStrength); cdecl;
end;
TJPhoneStateListener = class(TJavaGenericImport<JPhoneStateListenerClass, JPhoneStateListener>) end;

JCustomPhoneStateListenerClass = interface(JPhoneStateListenerClass)
['{94FF7ECE-DF9C-41E3-B9E7-9E44DC0127BB}']
  {Methods}
  function init(listener: JICustomPhoneStateListener): JCustomPhoneStateListener; cdecl;
end;

[JavaSignature('android/telephony/CustomPhoneStateListener')]
JCustomPhoneStateListener = interface(JPhoneStateListener)
['{753BA66D-E86F-4345-89FC-7BC79F6B5C5A}']
  {Methods}
  procedure onCallForwardingIndicatorChanged(cfi: Boolean); cdecl;
  procedure onCallStateChanged(state: Integer; incomingNumber: JString); cdecl;
  procedure onCellInfoChanged(cellInfo: JList); cdecl;
  procedure onCellLocationChanged(location: JCellLocation); cdecl;
  procedure onDataActivity(direction: Integer); cdecl;
  procedure onDataConnectionStateChanged(state: Integer); cdecl; overload;
  procedure onDataConnectionStateChanged(state: Integer; networkType: Integer); cdecl; overload;
  procedure onMessageWaitingIndicatorChanged(mwi: Boolean); cdecl;
  procedure onServiceStateChanged(serviceState: JServiceState); cdecl;
  procedure onSignalStrengthChanged(asu: Integer); cdecl;
  procedure onSignalStrengthsChanged(signalStrength: JSignalStrength); cdecl;
end;
TJCustomPhoneStateListener = class(TJavaGenericImport<JCustomPhoneStateListenerClass, JCustomPhoneStateListener>) end;

JSignalStrengthClass = interface(JObjectClass)
['{C579E9F5-9291-4F6F-B4BE-8C8522EE8C5E}']
end;

[JavaSignature('android/telephony/SignalStrength')]
JSignalStrength = interface(JObject)
['{1260215B-42EC-4F73-8FFB-168FABD93E9A}']
  {Methods}
  function describeContents: Integer; cdecl;
  function equals(o: JObject): Boolean; cdecl;
  function getCdmaDbm: Integer; cdecl;
  function getCdmaEcio: Integer; cdecl;
  function getEvdoDbm: Integer; cdecl;
  function getEvdoEcio: Integer; cdecl;
  function getEvdoSnr: Integer; cdecl;
  function getGsmBitErrorRate: Integer; cdecl;
  function getGsmSignalStrength: Integer; cdecl;
  function hashCode: Integer; cdecl;
  function isGsm: Boolean; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
end;
TJSignalStrength = class(TJavaGenericImport<JSignalStrengthClass, JSignalStrength>) end;

Jgsm_SmsMessage_SubmitPduClass = interface(JObjectClass)
['{1F8AD068-26C6-4018-AC53-2568912E18AC}']
  {Methods}
  function init: Jgsm_SmsMessage_SubmitPdu; cdecl;//Deprecated
end;

[JavaSignature('android/telephony/gsm/SmsMessage$SubmitPdu')]
Jgsm_SmsMessage_SubmitPdu = interface(JObject)
['{F5357F33-764B-4656-9332-78162CB99DEF}']
  {Property Methods}
  function _GetencodedMessage: TJavaArray<Byte>;
  procedure _SetencodedMessage(Value: TJavaArray<Byte>);
  function _GetencodedScAddress: TJavaArray<Byte>;
  procedure _SetencodedScAddress(Value: TJavaArray<Byte>);
  {Methods}
  function toString: JString; cdecl;//Deprecated
  {Properties}
  property encodedMessage: TJavaArray<Byte> read _GetencodedMessage write _SetencodedMessage;
  property encodedScAddress: TJavaArray<Byte> read _GetencodedScAddress write _SetencodedScAddress;
end;
TJgsm_SmsMessage_SubmitPdu = class(TJavaGenericImport<Jgsm_SmsMessage_SubmitPduClass, Jgsm_SmsMessage_SubmitPdu>) end;

JCellSignalStrengthGsmClass = interface(JCellSignalStrengthClass)
['{BC14D3AE-FBE8-4778-9922-2DC549FAA3A3}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/telephony/CellSignalStrengthGsm')]
JCellSignalStrengthGsm = interface(JCellSignalStrength)
['{17546ED7-86F4-4A5F-A432-9D0477B4F29C}']
  {Methods}
  function describeContents: Integer; cdecl;
  function equals(o: JObject): Boolean; cdecl;
  function getAsuLevel: Integer; cdecl;
  function getDbm: Integer; cdecl;
  function getLevel: Integer; cdecl;
  function hashCode: Integer; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
end;
TJCellSignalStrengthGsm = class(TJavaGenericImport<JCellSignalStrengthGsmClass, JCellSignalStrengthGsm>) end;

Jgsm_SmsMessage_MessageClassClass = interface(JEnumClass)
['{39AF35BE-04BA-4BA5-815F-EAF9192F6413}']
  {Property Methods}
  function _GetCLASS_0: Jgsm_SmsMessage_MessageClass;
  function _GetCLASS_1: Jgsm_SmsMessage_MessageClass;
  function _GetCLASS_2: Jgsm_SmsMessage_MessageClass;
  function _GetCLASS_3: Jgsm_SmsMessage_MessageClass;
  function _GetUNKNOWN: Jgsm_SmsMessage_MessageClass;
  {Methods}
  function valueOf(name: JString): Jgsm_SmsMessage_MessageClass; cdecl;
  function values: TJavaObjectArray<Jgsm_SmsMessage_MessageClass>; cdecl;
  {Properties}
  property CLASS_0: Jgsm_SmsMessage_MessageClass read _GetCLASS_0;
  property CLASS_1: Jgsm_SmsMessage_MessageClass read _GetCLASS_1;
  property CLASS_2: Jgsm_SmsMessage_MessageClass read _GetCLASS_2;
  property CLASS_3: Jgsm_SmsMessage_MessageClass read _GetCLASS_3;
  property UNKNOWN: Jgsm_SmsMessage_MessageClass read _GetUNKNOWN;
end;

[JavaSignature('android/telephony/gsm/SmsMessage$MessageClass')]
Jgsm_SmsMessage_MessageClass = interface(JEnum)
['{2A8F0F0C-A782-40C9-A015-DEB6470488E3}']
end;
TJgsm_SmsMessage_MessageClass = class(TJavaGenericImport<Jgsm_SmsMessage_MessageClassClass, Jgsm_SmsMessage_MessageClass>) end;

JCellIdentityLteClass = interface(JObjectClass)
['{3F1A9F22-BF62-4D7C-ABA7-ECECF9725F68}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/telephony/CellIdentityLte')]
JCellIdentityLte = interface(JObject)
['{A1796A99-CC9C-47E8-97EA-E7AF5B27A5BE}']
  {Methods}
  function describeContents: Integer; cdecl;
  function equals(other: JObject): Boolean; cdecl;
  function getCi: Integer; cdecl;
  function getMcc: Integer; cdecl;
  function getMnc: Integer; cdecl;
  function getPci: Integer; cdecl;
  function getTac: Integer; cdecl;
  function hashCode: Integer; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
end;
TJCellIdentityLte = class(TJavaGenericImport<JCellIdentityLteClass, JCellIdentityLte>) end;

JCellIdentityGsmClass = interface(JObjectClass)
['{F238DC50-40FA-4093-BDA2-2839208E79CE}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/telephony/CellIdentityGsm')]
JCellIdentityGsm = interface(JObject)
['{9DD941A0-7403-4CD9-B000-22D4E7500A6D}']
  {Methods}
  function describeContents: Integer; cdecl;
  function equals(other: JObject): Boolean; cdecl;
  function getCid: Integer; cdecl;
  function getLac: Integer; cdecl;
  function getMcc: Integer; cdecl;
  function getMnc: Integer; cdecl;
  function getPsc: Integer; cdecl;
  function hashCode: Integer; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
end;
TJCellIdentityGsm = class(TJavaGenericImport<JCellIdentityGsmClass, JCellIdentityGsm>) end;

JTelephonyManagerClass = interface(JObjectClass)
['{543679F3-38DC-4FCA-ADCD-648A27B9B5DD}']
  {Property Methods}
  function _GetACTION_PHONE_STATE_CHANGED: JString;
  function _GetCALL_STATE_IDLE: Integer;
  function _GetCALL_STATE_OFFHOOK: Integer;
  function _GetCALL_STATE_RINGING: Integer;
  function _GetDATA_ACTIVITY_DORMANT: Integer;
  function _GetDATA_ACTIVITY_IN: Integer;
  function _GetDATA_ACTIVITY_INOUT: Integer;
  function _GetDATA_ACTIVITY_NONE: Integer;
  function _GetDATA_ACTIVITY_OUT: Integer;
  function _GetDATA_CONNECTED: Integer;
  function _GetDATA_CONNECTING: Integer;
  function _GetDATA_DISCONNECTED: Integer;
  function _GetDATA_SUSPENDED: Integer;
  function _GetEXTRA_INCOMING_NUMBER: JString;
  function _GetEXTRA_STATE: JString;
  function _GetEXTRA_STATE_IDLE: JString;
  function _GetEXTRA_STATE_OFFHOOK: JString;
  function _GetEXTRA_STATE_RINGING: JString;
  function _GetNETWORK_TYPE_1xRTT: Integer;
  function _GetNETWORK_TYPE_CDMA: Integer;
  function _GetNETWORK_TYPE_EDGE: Integer;
  function _GetNETWORK_TYPE_EHRPD: Integer;
  function _GetNETWORK_TYPE_EVDO_0: Integer;
  function _GetNETWORK_TYPE_EVDO_A: Integer;
  function _GetNETWORK_TYPE_EVDO_B: Integer;
  function _GetNETWORK_TYPE_GPRS: Integer;
  function _GetNETWORK_TYPE_HSDPA: Integer;
  function _GetNETWORK_TYPE_HSPA: Integer;
  function _GetNETWORK_TYPE_HSPAP: Integer;
  function _GetNETWORK_TYPE_HSUPA: Integer;
  function _GetNETWORK_TYPE_IDEN: Integer;
  function _GetNETWORK_TYPE_LTE: Integer;
  function _GetNETWORK_TYPE_UMTS: Integer;
  function _GetNETWORK_TYPE_UNKNOWN: Integer;
  function _GetPHONE_TYPE_CDMA: Integer;
  function _GetPHONE_TYPE_GSM: Integer;
  function _GetPHONE_TYPE_NONE: Integer;
  function _GetPHONE_TYPE_SIP: Integer;
  function _GetSIM_STATE_ABSENT: Integer;
  function _GetSIM_STATE_NETWORK_LOCKED: Integer;
  function _GetSIM_STATE_PIN_REQUIRED: Integer;
  function _GetSIM_STATE_PUK_REQUIRED: Integer;
  function _GetSIM_STATE_READY: Integer;
  function _GetSIM_STATE_UNKNOWN: Integer;
  {Properties}
  property ACTION_PHONE_STATE_CHANGED: JString read _GetACTION_PHONE_STATE_CHANGED;
  property CALL_STATE_IDLE: Integer read _GetCALL_STATE_IDLE;
  property CALL_STATE_OFFHOOK: Integer read _GetCALL_STATE_OFFHOOK;
  property CALL_STATE_RINGING: Integer read _GetCALL_STATE_RINGING;
  property DATA_ACTIVITY_DORMANT: Integer read _GetDATA_ACTIVITY_DORMANT;
  property DATA_ACTIVITY_IN: Integer read _GetDATA_ACTIVITY_IN;
  property DATA_ACTIVITY_INOUT: Integer read _GetDATA_ACTIVITY_INOUT;
  property DATA_ACTIVITY_NONE: Integer read _GetDATA_ACTIVITY_NONE;
  property DATA_ACTIVITY_OUT: Integer read _GetDATA_ACTIVITY_OUT;
  property DATA_CONNECTED: Integer read _GetDATA_CONNECTED;
  property DATA_CONNECTING: Integer read _GetDATA_CONNECTING;
  property DATA_DISCONNECTED: Integer read _GetDATA_DISCONNECTED;
  property DATA_SUSPENDED: Integer read _GetDATA_SUSPENDED;
  property EXTRA_INCOMING_NUMBER: JString read _GetEXTRA_INCOMING_NUMBER;
  property EXTRA_STATE: JString read _GetEXTRA_STATE;
  property EXTRA_STATE_IDLE: JString read _GetEXTRA_STATE_IDLE;
  property EXTRA_STATE_OFFHOOK: JString read _GetEXTRA_STATE_OFFHOOK;
  property EXTRA_STATE_RINGING: JString read _GetEXTRA_STATE_RINGING;
  property NETWORK_TYPE_1xRTT: Integer read _GetNETWORK_TYPE_1xRTT;
  property NETWORK_TYPE_CDMA: Integer read _GetNETWORK_TYPE_CDMA;
  property NETWORK_TYPE_EDGE: Integer read _GetNETWORK_TYPE_EDGE;
  property NETWORK_TYPE_EHRPD: Integer read _GetNETWORK_TYPE_EHRPD;
  property NETWORK_TYPE_EVDO_0: Integer read _GetNETWORK_TYPE_EVDO_0;
  property NETWORK_TYPE_EVDO_A: Integer read _GetNETWORK_TYPE_EVDO_A;
  property NETWORK_TYPE_EVDO_B: Integer read _GetNETWORK_TYPE_EVDO_B;
  property NETWORK_TYPE_GPRS: Integer read _GetNETWORK_TYPE_GPRS;
  property NETWORK_TYPE_HSDPA: Integer read _GetNETWORK_TYPE_HSDPA;
  property NETWORK_TYPE_HSPA: Integer read _GetNETWORK_TYPE_HSPA;
  property NETWORK_TYPE_HSPAP: Integer read _GetNETWORK_TYPE_HSPAP;
  property NETWORK_TYPE_HSUPA: Integer read _GetNETWORK_TYPE_HSUPA;
  property NETWORK_TYPE_IDEN: Integer read _GetNETWORK_TYPE_IDEN;
  property NETWORK_TYPE_LTE: Integer read _GetNETWORK_TYPE_LTE;
  property NETWORK_TYPE_UMTS: Integer read _GetNETWORK_TYPE_UMTS;
  property NETWORK_TYPE_UNKNOWN: Integer read _GetNETWORK_TYPE_UNKNOWN;
  property PHONE_TYPE_CDMA: Integer read _GetPHONE_TYPE_CDMA;
  property PHONE_TYPE_GSM: Integer read _GetPHONE_TYPE_GSM;
  property PHONE_TYPE_NONE: Integer read _GetPHONE_TYPE_NONE;
  property PHONE_TYPE_SIP: Integer read _GetPHONE_TYPE_SIP;
  property SIM_STATE_ABSENT: Integer read _GetSIM_STATE_ABSENT;
  property SIM_STATE_NETWORK_LOCKED: Integer read _GetSIM_STATE_NETWORK_LOCKED;
  property SIM_STATE_PIN_REQUIRED: Integer read _GetSIM_STATE_PIN_REQUIRED;
  property SIM_STATE_PUK_REQUIRED: Integer read _GetSIM_STATE_PUK_REQUIRED;
  property SIM_STATE_READY: Integer read _GetSIM_STATE_READY;
  property SIM_STATE_UNKNOWN: Integer read _GetSIM_STATE_UNKNOWN;
end;

[JavaSignature('android/telephony/TelephonyManager')]
JTelephonyManager = interface(JObject)
['{7725226B-9F82-4BFA-B1DE-3960703BB92C}']
  {Methods}
  function getAllCellInfo: JList; cdecl;
  function getCallState: Integer; cdecl;
  function getCellLocation: JCellLocation; cdecl;
  function getDataActivity: Integer; cdecl;
  function getDataState: Integer; cdecl;
  function getDeviceId: JString; cdecl;
  function getDeviceSoftwareVersion: JString; cdecl;
  function getLine1Number: JString; cdecl;
  function getNeighboringCellInfo: JList; cdecl;
  function getNetworkCountryIso: JString; cdecl;
  function getNetworkOperator: JString; cdecl;
  function getNetworkOperatorName: JString; cdecl;
  function getNetworkType: Integer; cdecl;
  function getPhoneType: Integer; cdecl;
  function getSimCountryIso: JString; cdecl;
  function getSimOperator: JString; cdecl;
  function getSimOperatorName: JString; cdecl;
  function getSimSerialNumber: JString; cdecl;
  function getSimState: Integer; cdecl;
  function getSubscriberId: JString; cdecl;
  function getVoiceMailAlphaTag: JString; cdecl;
  function getVoiceMailNumber: JString; cdecl;
  function hasIccCard: Boolean; cdecl;
  function isNetworkRoaming: Boolean; cdecl;
  procedure listen(listener: JPhoneStateListener; events: Integer); cdecl;
end;
TJTelephonyManager = class(TJavaGenericImport<JTelephonyManagerClass, JTelephonyManager>) end;

JICustomPhoneStateListenerClass = interface(IJavaClass)
['{2B3FDA31-7A12-4ADC-B81C-B7E9D37ADD09}']
end;

[JavaSignature('android/telephony/ICustomPhoneStateListener')]
JICustomPhoneStateListener = interface(IJavaInstance)
['{C8ABB706-0034-43DA-B768-F095F9ABE215}']
  {Methods}
  procedure onCallForwardingIndicatorChanged(cfi: Boolean); cdecl;
  procedure onCallStateChanged(state: Integer; incomingNumber: JString); cdecl;
  procedure onCellInfoChanged(cellInfo: JList); cdecl;
  procedure onCellLocationChanged(location: JCellLocation); cdecl;
  procedure onDataActivity(direction: Integer); cdecl;
  procedure onDataConnectionStateChanged(state: Integer); cdecl; overload;
  procedure onDataConnectionStateChanged(state: Integer; networkType: Integer); cdecl; overload;
  procedure onMessageWaitingIndicatorChanged(mwi: Boolean); cdecl;
  procedure onServiceStateChanged(serviceState: JServiceState); cdecl;
  procedure onSignalStrengthChanged(asu: Integer); cdecl;
  procedure onSignalStrengthsChanged(signalStrength: JSignalStrength); cdecl;
end;
TJICustomPhoneStateListener = class(TJavaGenericImport<JICustomPhoneStateListenerClass, JICustomPhoneStateListener>) end;

JSmsManagerClass = interface(JObjectClass)
['{5FD2ABA7-01A0-4AA1-BDE6-B125CD6A6752}']
  {Property Methods}
  function _GetRESULT_ERROR_GENERIC_FAILURE: Integer;
  function _GetRESULT_ERROR_NO_SERVICE: Integer;
  function _GetRESULT_ERROR_NULL_PDU: Integer;
  function _GetRESULT_ERROR_RADIO_OFF: Integer;
  function _GetSTATUS_ON_ICC_FREE: Integer;
  function _GetSTATUS_ON_ICC_READ: Integer;
  function _GetSTATUS_ON_ICC_SENT: Integer;
  function _GetSTATUS_ON_ICC_UNREAD: Integer;
  function _GetSTATUS_ON_ICC_UNSENT: Integer;
  {Methods}
  function getDefault: JSmsManager; cdecl;
  {Properties}
  property RESULT_ERROR_GENERIC_FAILURE: Integer read _GetRESULT_ERROR_GENERIC_FAILURE;
  property RESULT_ERROR_NO_SERVICE: Integer read _GetRESULT_ERROR_NO_SERVICE;
  property RESULT_ERROR_NULL_PDU: Integer read _GetRESULT_ERROR_NULL_PDU;
  property RESULT_ERROR_RADIO_OFF: Integer read _GetRESULT_ERROR_RADIO_OFF;
  property STATUS_ON_ICC_FREE: Integer read _GetSTATUS_ON_ICC_FREE;
  property STATUS_ON_ICC_READ: Integer read _GetSTATUS_ON_ICC_READ;
  property STATUS_ON_ICC_SENT: Integer read _GetSTATUS_ON_ICC_SENT;
  property STATUS_ON_ICC_UNREAD: Integer read _GetSTATUS_ON_ICC_UNREAD;
  property STATUS_ON_ICC_UNSENT: Integer read _GetSTATUS_ON_ICC_UNSENT;
end;

[JavaSignature('android/telephony/SmsManager')]
JSmsManager = interface(JObject)
['{8C75DE6B-0BC5-4B5B-9B70-E714A01033E0}']
  {Methods}
  function divideMessage(text: JString): JArrayList; cdecl;
  procedure sendDataMessage(destinationAddress: JString; scAddress: JString; destinationPort: SmallInt; data: TJavaArray<Byte>; sentIntent: JPendingIntent; deliveryIntent: JPendingIntent); cdecl;
  procedure sendMultipartTextMessage(destinationAddress: JString; scAddress: JString; parts: JArrayList; sentIntents: JArrayList; deliveryIntents: JArrayList); cdecl;
  procedure sendTextMessage(destinationAddress: JString; scAddress: JString; text: JString; sentIntent: JPendingIntent; deliveryIntent: JPendingIntent); cdecl;
end;
TJSmsManager = class(TJavaGenericImport<JSmsManagerClass, JSmsManager>) end;

JCellSignalStrengthCdmaClass = interface(JCellSignalStrengthClass)
['{01B4BDE1-698E-4773-B65E-89BEC2EB3074}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
end;

[JavaSignature('android/telephony/CellSignalStrengthCdma')]
JCellSignalStrengthCdma = interface(JCellSignalStrength)
['{06C07B11-232F-40E1-9072-885B7D40F92D}']
  {Methods}
  function describeContents: Integer; cdecl;
  function equals(o: JObject): Boolean; cdecl;
  function getAsuLevel: Integer; cdecl;
  function getCdmaDbm: Integer; cdecl;
  function getCdmaEcio: Integer; cdecl;
  function getCdmaLevel: Integer; cdecl;
  function getDbm: Integer; cdecl;
  function getEvdoDbm: Integer; cdecl;
  function getEvdoEcio: Integer; cdecl;
  function getEvdoLevel: Integer; cdecl;
  function getEvdoSnr: Integer; cdecl;
  function getLevel: Integer; cdecl;
  function hashCode: Integer; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
end;
TJCellSignalStrengthCdma = class(TJavaGenericImport<JCellSignalStrengthCdmaClass, JCellSignalStrengthCdma>) end;

JServiceStateClass = interface(JObjectClass)
['{5E369BCE-8950-478C-88C1-1612B0C7999C}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  function _GetSTATE_EMERGENCY_ONLY: Integer;
  function _GetSTATE_IN_SERVICE: Integer;
  function _GetSTATE_OUT_OF_SERVICE: Integer;
  function _GetSTATE_POWER_OFF: Integer;
  {Methods}
  function init: JServiceState; cdecl; overload;
  function init(s: JServiceState): JServiceState; cdecl; overload;
  function init(in_: JParcel): JServiceState; cdecl; overload;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
  property STATE_EMERGENCY_ONLY: Integer read _GetSTATE_EMERGENCY_ONLY;
  property STATE_IN_SERVICE: Integer read _GetSTATE_IN_SERVICE;
  property STATE_OUT_OF_SERVICE: Integer read _GetSTATE_OUT_OF_SERVICE;
  property STATE_POWER_OFF: Integer read _GetSTATE_POWER_OFF;
end;

[JavaSignature('android/telephony/ServiceState')]
JServiceState = interface(JObject)
['{981444BD-E25C-4943-97AE-C21D85913DEF}']
  {Methods}
  function describeContents: Integer; cdecl;
  function equals(o: JObject): Boolean; cdecl;
  function getIsManualSelection: Boolean; cdecl;
  function getOperatorAlphaLong: JString; cdecl;
  function getOperatorAlphaShort: JString; cdecl;
  function getOperatorNumeric: JString; cdecl;
  function getRoaming: Boolean; cdecl;
  function getState: Integer; cdecl;
  function hashCode: Integer; cdecl;
  procedure setIsManualSelection(isManual: Boolean); cdecl;
  procedure setOperatorName(longName: JString; shortName: JString; numeric: JString); cdecl;
  procedure setRoaming(roaming: Boolean); cdecl;
  procedure setState(state: Integer); cdecl;
  procedure setStateOff; cdecl;
  procedure setStateOutOfService; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
end;
TJServiceState = class(TJavaGenericImport<JServiceStateClass, JServiceState>) end;

Jgsm_SmsManagerClass = interface(JObjectClass)
['{17F8CCF3-8CA3-4E7C-86AA-CD21CF9AE9C5}']
  {Property Methods}
  function _GetRESULT_ERROR_GENERIC_FAILURE: Integer;
  function _GetRESULT_ERROR_NO_SERVICE: Integer;
  function _GetRESULT_ERROR_NULL_PDU: Integer;
  function _GetRESULT_ERROR_RADIO_OFF: Integer;
  function _GetSTATUS_ON_SIM_FREE: Integer;
  function _GetSTATUS_ON_SIM_READ: Integer;
  function _GetSTATUS_ON_SIM_SENT: Integer;
  function _GetSTATUS_ON_SIM_UNREAD: Integer;
  function _GetSTATUS_ON_SIM_UNSENT: Integer;
  {Methods}
  function getDefault: Jgsm_SmsManager; cdecl;//Deprecated
  {Properties}
  property RESULT_ERROR_GENERIC_FAILURE: Integer read _GetRESULT_ERROR_GENERIC_FAILURE;
  property RESULT_ERROR_NO_SERVICE: Integer read _GetRESULT_ERROR_NO_SERVICE;
  property RESULT_ERROR_NULL_PDU: Integer read _GetRESULT_ERROR_NULL_PDU;
  property RESULT_ERROR_RADIO_OFF: Integer read _GetRESULT_ERROR_RADIO_OFF;
  property STATUS_ON_SIM_FREE: Integer read _GetSTATUS_ON_SIM_FREE;
  property STATUS_ON_SIM_READ: Integer read _GetSTATUS_ON_SIM_READ;
  property STATUS_ON_SIM_SENT: Integer read _GetSTATUS_ON_SIM_SENT;
  property STATUS_ON_SIM_UNREAD: Integer read _GetSTATUS_ON_SIM_UNREAD;
  property STATUS_ON_SIM_UNSENT: Integer read _GetSTATUS_ON_SIM_UNSENT;
end;

[JavaSignature('android/telephony/gsm/SmsManager')]
Jgsm_SmsManager = interface(JObject)
['{0E3A70E1-7A4B-4480-896E-62E41FBF913E}']
  {Methods}
  function divideMessage(text: JString): JArrayList; cdecl;//Deprecated
  procedure sendDataMessage(destinationAddress: JString; scAddress: JString; destinationPort: SmallInt; data: TJavaArray<Byte>; sentIntent: JPendingIntent; deliveryIntent: JPendingIntent); cdecl;//Deprecated
  procedure sendMultipartTextMessage(destinationAddress: JString; scAddress: JString; parts: JArrayList; sentIntents: JArrayList; deliveryIntents: JArrayList); cdecl;//Deprecated
  procedure sendTextMessage(destinationAddress: JString; scAddress: JString; text: JString; sentIntent: JPendingIntent; deliveryIntent: JPendingIntent); cdecl;//Deprecated
end;
TJgsm_SmsManager = class(TJavaGenericImport<Jgsm_SmsManagerClass, Jgsm_SmsManager>) end;

JPhoneNumberFormattingTextWatcherClass = interface(JObjectClass)
['{41B14776-2EE7-4DE2-8E4E-C1E936C866C1}']
  {Methods}
  function init: JPhoneNumberFormattingTextWatcher; cdecl;
end;

[JavaSignature('android/telephony/PhoneNumberFormattingTextWatcher')]
JPhoneNumberFormattingTextWatcher = interface(JObject)
['{891E6B45-DD52-484F-8E52-5F5272FC56DC}']
  {Methods}
  procedure afterTextChanged(s: JEditable); cdecl;
  procedure beforeTextChanged(s: JCharSequence; start: Integer; count: Integer; after: Integer); cdecl;
  procedure onTextChanged(s: JCharSequence; start: Integer; before: Integer; count: Integer); cdecl;
end;
TJPhoneNumberFormattingTextWatcher = class(TJavaGenericImport<JPhoneNumberFormattingTextWatcherClass, JPhoneNumberFormattingTextWatcher>) end;

Jgsm_SmsMessageClass = interface(JObjectClass)
['{F5132C01-2558-4829-8125-7EACA7B45F08}']
  {Property Methods}
  function _GetENCODING_16BIT: Integer;
  function _GetENCODING_7BIT: Integer;
  function _GetENCODING_8BIT: Integer;
  function _GetENCODING_UNKNOWN: Integer;
  function _GetMAX_USER_DATA_BYTES: Integer;
  function _GetMAX_USER_DATA_SEPTETS: Integer;
  function _GetMAX_USER_DATA_SEPTETS_WITH_HEADER: Integer;
  {Methods}
  function init: Jgsm_SmsMessage; cdecl;//Deprecated
  function calculateLength(messageBody: JCharSequence; use7bitOnly: Boolean): TJavaArray<Integer>; cdecl; overload;//Deprecated
  function calculateLength(messageBody: JString; use7bitOnly: Boolean): TJavaArray<Integer>; cdecl; overload;//Deprecated
  function createFromPdu(pdu: TJavaArray<Byte>): Jgsm_SmsMessage; cdecl;//Deprecated
  function getSubmitPdu(scAddress: JString; destinationAddress: JString; message: JString; statusReportRequested: Boolean): Jgsm_SmsMessage_SubmitPdu; cdecl; overload;//Deprecated
  function getSubmitPdu(scAddress: JString; destinationAddress: JString; destinationPort: SmallInt; data: TJavaArray<Byte>; statusReportRequested: Boolean): Jgsm_SmsMessage_SubmitPdu; cdecl; overload;//Deprecated
  function getTPLayerLengthForPDU(pdu: JString): Integer; cdecl;//Deprecated
  {Properties}
  property ENCODING_16BIT: Integer read _GetENCODING_16BIT;
  property ENCODING_7BIT: Integer read _GetENCODING_7BIT;
  property ENCODING_8BIT: Integer read _GetENCODING_8BIT;
  property ENCODING_UNKNOWN: Integer read _GetENCODING_UNKNOWN;
  property MAX_USER_DATA_BYTES: Integer read _GetMAX_USER_DATA_BYTES;
  property MAX_USER_DATA_SEPTETS: Integer read _GetMAX_USER_DATA_SEPTETS;
  property MAX_USER_DATA_SEPTETS_WITH_HEADER: Integer read _GetMAX_USER_DATA_SEPTETS_WITH_HEADER;
end;

[JavaSignature('android/telephony/gsm/SmsMessage')]
Jgsm_SmsMessage = interface(JObject)
['{71AD7E64-87CA-4EC5-BB5B-91DCB9A3F8D5}']
  {Methods}
  function getDisplayMessageBody: JString; cdecl;//Deprecated
  function getDisplayOriginatingAddress: JString; cdecl;//Deprecated
  function getEmailBody: JString; cdecl;//Deprecated
  function getEmailFrom: JString; cdecl;//Deprecated
  function getIndexOnSim: Integer; cdecl;//Deprecated
  function getMessageBody: JString; cdecl;//Deprecated
  function getMessageClass: Jgsm_SmsMessage_MessageClass; cdecl;//Deprecated
  function getOriginatingAddress: JString; cdecl;//Deprecated
  function getPdu: TJavaArray<Byte>; cdecl;//Deprecated
  function getProtocolIdentifier: Integer; cdecl;//Deprecated
  function getPseudoSubject: JString; cdecl;//Deprecated
  function getServiceCenterAddress: JString; cdecl;//Deprecated
  function getStatus: Integer; cdecl;//Deprecated
  function getStatusOnSim: Integer; cdecl;//Deprecated
  function getTimestampMillis: Int64; cdecl;//Deprecated
  function getUserData: TJavaArray<Byte>; cdecl;//Deprecated
  function isCphsMwiMessage: Boolean; cdecl;//Deprecated
  function isEmail: Boolean; cdecl;//Deprecated
  function isMWIClearMessage: Boolean; cdecl;//Deprecated
  function isMWISetMessage: Boolean; cdecl;//Deprecated
  function isMwiDontStore: Boolean; cdecl;//Deprecated
  function isReplace: Boolean; cdecl;//Deprecated
  function isReplyPathPresent: Boolean; cdecl;//Deprecated
  function isStatusReportMessage: Boolean; cdecl;//Deprecated
end;
TJgsm_SmsMessage = class(TJavaGenericImport<Jgsm_SmsMessageClass, Jgsm_SmsMessage>) end;




implementation

begin

end.


