{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit Androidapi.JNI.JavaTypes;

interface

uses Androidapi.Jni,

  Androidapi.JNIBridge;

type
  {Class forward declarations}
  JObject = interface;//java.lang.Object
  JClassLoader = interface;//java.lang.ClassLoader
  JInputStream = interface;//java.io.InputStream
  JByteArrayInputStream = interface;//java.io.ByteArrayInputStream
  JNumber = interface;//java.lang.Number
  JInteger = interface;//java.lang.Integer
  JBoolean = interface;//java.lang.Boolean
  JByte = interface;//java.lang.Byte
  JEnumeration = interface;//java.util.Enumeration
  JCalendar = interface;//java.util.Calendar
  JAbstractCollection = interface;//java.util.AbstractCollection
  JAbstractSet = interface;//java.util.AbstractSet
  JHashSet = interface;//java.util.HashSet
  JAbstractList = interface;//java.util.AbstractList
  JSerializable = interface;//java.io.Serializable
  JEnum = interface;//java.lang.Enum
  JThread_State = interface;//java.lang.Thread$State
  JThread_UncaughtExceptionHandler = interface;//java.lang.Thread$UncaughtExceptionHandler
  JFileDescriptor = interface;//java.io.FileDescriptor
  JFile = interface;//java.io.File
  JArrayList = interface;//java.util.ArrayList
  JRunnable = interface;//java.lang.Runnable
  JCloseable = interface;//java.io.Closeable
  JWriter = interface;//java.io.Writer
  JIterable = interface;//java.lang.Iterable
  JCollection = interface;//java.util.Collection
  JList = interface;//java.util.List
  JOutputStream = interface;//java.io.OutputStream
  Jlang_Class = interface;//java.lang.Class
  JThrowable = interface;//java.lang.Throwable
  JException = interface;//java.lang.Exception
  JJSONException = interface;//org.json.JSONException
  JUUID = interface;//java.util.UUID
  JAbstractMap = interface;//java.util.AbstractMap
  JHashMap = interface;//java.util.HashMap
  JRandom = interface;//java.util.Random
  JAnnotation = interface;//java.lang.annotation.Annotation
  JThread = interface;//java.lang.Thread
  JDate = interface;//java.util.Date
  JFilterOutputStream = interface;//java.io.FilterOutputStream
  JPrintStream = interface;//java.io.PrintStream
  JRuntimeException = interface;//java.lang.RuntimeException
  JObserver = interface;//java.util.Observer
  JFloat = interface;//java.lang.Float
  JDouble = interface;//java.lang.Double
  JPrintWriter = interface;//java.io.PrintWriter
  JIterator = interface;//java.util.Iterator
  JListIterator = interface;//java.util.ListIterator
  JByteArrayOutputStream = interface;//java.io.ByteArrayOutputStream
  JStackTraceElement = interface;//java.lang.StackTraceElement
  JFileOutputStream = interface;//java.io.FileOutputStream
  JAbstractStringBuilder = interface;//java.lang.AbstractStringBuilder
  JStringBuilder = interface;//java.lang.StringBuilder
  JCharSequence = interface;//java.lang.CharSequence
  JGregorianCalendar = interface;//java.util.GregorianCalendar
  JJSONTokener = interface;//org.json.JSONTokener
  JMap = interface;//java.util.Map
  JLocale = interface;//java.util.Locale
  JTimeZone = interface;//java.util.TimeZone
  JFileFilter = interface;//java.io.FileFilter
  JEnumSet = interface;//java.util.EnumSet
  Jutil_Observable = interface;//java.util.Observable
  JFilenameFilter = interface;//java.io.FilenameFilter
  JJSONObject = interface;//org.json.JSONObject
  JString = interface;//java.lang.String
  JSet = interface;//java.util.Set
  JShort = interface;//java.lang.Short
  JThreadGroup = interface;//java.lang.ThreadGroup
  JComparator = interface;//java.util.Comparator
  JJSONArray = interface;//org.json.JSONArray
  JLong = interface;//java.lang.Long
  JFileInputStream = interface;//java.io.FileInputStream
  JStringBuffer = interface;//java.lang.StringBuffer

JObjectClass = interface(IJavaClass)
['{83BD30EE-FE9B-470D-AD6C-23AEAABB7FFA}']
  {Methods}
  function init: JObject; cdecl;
end;

[JavaSignature('java/lang/Object')]
JObject = interface(IJavaInstance)
['{32321F8A-4001-4BF8-92E7-6190D070988D}']
  {Methods}
  function equals(o: JObject): Boolean; cdecl;
  function getClass: Jlang_Class; cdecl;
  function hashCode: Integer; cdecl;
  procedure notify; cdecl;
  procedure notifyAll; cdecl;
  function toString: JString; cdecl;
  procedure wait; cdecl; overload;
  procedure wait(millis: Int64); cdecl; overload;
  procedure wait(millis: Int64; nanos: Integer); cdecl; overload;
end;
TJObject = class(TJavaGenericImport<JObjectClass, JObject>) end;

JClassLoaderClass = interface(JObjectClass)
['{453BE0D7-B813-4C83-A30C-F24C026FD112}']
  {Methods}
  function getSystemClassLoader: JClassLoader; cdecl;
  function getSystemResourceAsStream(resName: JString): JInputStream; cdecl;
  function getSystemResources(resName: JString): JEnumeration; cdecl;
end;

[JavaSignature('java/lang/ClassLoader')]
JClassLoader = interface(JObject)
['{17B43D0A-2016-44ED-84B5-9EAB55AF8FDD}']
  {Methods}
  procedure clearAssertionStatus; cdecl;
  function getParent: JClassLoader; cdecl;
  function getResourceAsStream(resName: JString): JInputStream; cdecl;
  function getResources(resName: JString): JEnumeration; cdecl;
  function loadClass(className: JString): Jlang_Class; cdecl;
  procedure setClassAssertionStatus(cname: JString; enable: Boolean); cdecl;
  procedure setDefaultAssertionStatus(enable: Boolean); cdecl;
  procedure setPackageAssertionStatus(pname: JString; enable: Boolean); cdecl;
end;
TJClassLoader = class(TJavaGenericImport<JClassLoaderClass, JClassLoader>) end;

JInputStreamClass = interface(JObjectClass)
['{8D8C2F8A-AD54-42D0-ADA4-FC30FD95A933}']
  {Methods}
  function init: JInputStream; cdecl;
end;

[JavaSignature('java/io/InputStream')]
JInputStream = interface(JObject)
['{5FD3C203-8A19-42A2-8FD2-643501DF62BC}']
  {Methods}
  function available: Integer; cdecl;
  procedure close; cdecl;
  procedure mark(readlimit: Integer); cdecl;
  function markSupported: Boolean; cdecl;
  function read: Integer; cdecl; overload;
  function read(buffer: TJavaArray<Byte>): Integer; cdecl; overload;
  function read(buffer: TJavaArray<Byte>; offset: Integer; length: Integer): Integer; cdecl; overload;
  procedure reset; cdecl;
  function skip(byteCount: Int64): Int64; cdecl;
end;
TJInputStream = class(TJavaGenericImport<JInputStreamClass, JInputStream>) end;

JByteArrayInputStreamClass = interface(JInputStreamClass)
['{1C0763C7-3F23-4531-A6E1-65AF97251C2F}']
  {Methods}
  function init(buf: TJavaArray<Byte>): JByteArrayInputStream; cdecl; overload;
  function init(buf: TJavaArray<Byte>; offset: Integer; length: Integer): JByteArrayInputStream; cdecl; overload;
end;

[JavaSignature('java/io/ByteArrayInputStream')]
JByteArrayInputStream = interface(JInputStream)
['{D8AE245D-6831-48AC-A6F5-1E480815A22D}']
  {Methods}
  function available: Integer; cdecl;
  procedure close; cdecl;
  procedure mark(readlimit: Integer); cdecl;
  function markSupported: Boolean; cdecl;
  function read: Integer; cdecl; overload;
  function read(buffer: TJavaArray<Byte>; offset: Integer; length: Integer): Integer; cdecl; overload;
  procedure reset; cdecl;
  function skip(byteCount: Int64): Int64; cdecl;
end;
TJByteArrayInputStream = class(TJavaGenericImport<JByteArrayInputStreamClass, JByteArrayInputStream>) end;

JNumberClass = interface(JObjectClass)
['{9A30B143-2018-4C7B-9E9B-316F62D643C5}']
  {Methods}
  function init: JNumber; cdecl;
end;

[JavaSignature('java/lang/Number')]
JNumber = interface(JObject)
['{DFF915A9-AFBE-4EDA-89AC-D0FE32A85482}']
  {Methods}
  function byteValue: Byte; cdecl;
  function doubleValue: Double; cdecl;
  function floatValue: Single; cdecl;
  function intValue: Integer; cdecl;
  function longValue: Int64; cdecl;
  function shortValue: SmallInt; cdecl;
end;
TJNumber = class(TJavaGenericImport<JNumberClass, JNumber>) end;

JIntegerClass = interface(JNumberClass)
['{DA48E911-AB80-4875-993F-316B9F310559}']
  {Property Methods}
  function _GetMAX_VALUE: Integer;
  function _GetMIN_VALUE: Integer;
  function _GetSIZE: Integer;
  function _GetTYPE: Jlang_Class;
  {Methods}
  function init(value: Integer): JInteger; cdecl; overload;
  function init(string_: JString): JInteger; cdecl; overload;
  function bitCount(i: Integer): Integer; cdecl;
  function decode(string_: JString): JInteger; cdecl;
  function getInteger(string_: JString): JInteger; cdecl; overload;
  function getInteger(string_: JString; defaultValue: Integer): JInteger; cdecl; overload;
  function getInteger(string_: JString; defaultValue: JInteger): JInteger; cdecl; overload;
  function highestOneBit(i: Integer): Integer; cdecl;
  function lowestOneBit(i: Integer): Integer; cdecl;
  function numberOfLeadingZeros(i: Integer): Integer; cdecl;
  function numberOfTrailingZeros(i: Integer): Integer; cdecl;
  function parseInt(string_: JString): Integer; cdecl; overload;
  function parseInt(string_: JString; radix: Integer): Integer; cdecl; overload;
  function reverse(i: Integer): Integer; cdecl;
  function reverseBytes(i: Integer): Integer; cdecl;
  function rotateLeft(i: Integer; distance: Integer): Integer; cdecl;
  function rotateRight(i: Integer; distance: Integer): Integer; cdecl;
  function signum(i: Integer): Integer; cdecl;
  function toBinaryString(i: Integer): JString; cdecl;
  function toHexString(i: Integer): JString; cdecl;
  function toOctalString(i: Integer): JString; cdecl;
  function toString(i: Integer): JString; cdecl; overload;
  function toString(i: Integer; radix: Integer): JString; cdecl; overload;
  function valueOf(string_: JString): JInteger; cdecl; overload;
  function valueOf(string_: JString; radix: Integer): JInteger; cdecl; overload;
  function valueOf(i: Integer): JInteger; cdecl; overload;
  {Properties}
  property MAX_VALUE: Integer read _GetMAX_VALUE;
  property MIN_VALUE: Integer read _GetMIN_VALUE;
  property SIZE: Integer read _GetSIZE;
  property &TYPE: Jlang_Class read _GetTYPE;
end;

[JavaSignature('java/lang/Integer')]
JInteger = interface(JNumber)
['{A07D13BE-2418-4FCB-8CEB-F4160E5884D5}']
  {Methods}
  function byteValue: Byte; cdecl;
  function compareTo(object_: JInteger): Integer; cdecl;
  function doubleValue: Double; cdecl;
  function equals(o: JObject): Boolean; cdecl;
  function floatValue: Single; cdecl;
  function hashCode: Integer; cdecl;
  function intValue: Integer; cdecl;
  function longValue: Int64; cdecl;
  function shortValue: SmallInt; cdecl;
  function toString: JString; cdecl; overload;
end;
TJInteger = class(TJavaGenericImport<JIntegerClass, JInteger>) end;

JBooleanClass = interface(JObjectClass)
['{CD51CE90-BCDA-4291-99B0-7BC70033C3CB}']
  {Property Methods}
  function _GetFALSE: JBoolean;
  function _GetTRUE: JBoolean;
  function _GetTYPE: Jlang_Class;
  {Methods}
  function init(string_: JString): JBoolean; cdecl; overload;
  function init(value: Boolean): JBoolean; cdecl; overload;
  function getBoolean(string_: JString): Boolean; cdecl;
  function parseBoolean(s: JString): Boolean; cdecl;
  function toString(value: Boolean): JString; cdecl; overload;
  function valueOf(string_: JString): JBoolean; cdecl; overload;
  function valueOf(b: Boolean): JBoolean; cdecl; overload;
  {Properties}
  property FALSE: JBoolean read _GetFALSE;
  property TRUE: JBoolean read _GetTRUE;
  property &TYPE: Jlang_Class read _GetTYPE;
end;

[JavaSignature('java/lang/Boolean')]
JBoolean = interface(JObject)
['{21EAFAED-5848-48C2-9998-141B57439F6F}']
  {Methods}
  function booleanValue: Boolean; cdecl;
  function compareTo(that: JBoolean): Integer; cdecl;
  function equals(o: JObject): Boolean; cdecl;
  function hashCode: Integer; cdecl;
  function toString: JString; cdecl; overload;
end;
TJBoolean = class(TJavaGenericImport<JBooleanClass, JBoolean>) end;

JByteClass = interface(JNumberClass)
['{EDEFB599-A2A8-49AD-B413-C2FCEBD19B11}']
  {Property Methods}
  function _GetMAX_VALUE: Byte;
  function _GetMIN_VALUE: Byte;
  function _GetSIZE: Integer;
  function _GetTYPE: Jlang_Class;
  {Methods}
  function init(value: Byte): JByte; cdecl; overload;
  function init(string_: JString): JByte; cdecl; overload;
  function decode(string_: JString): JByte; cdecl;
  function parseByte(string_: JString): Byte; cdecl; overload;
  function parseByte(string_: JString; radix: Integer): Byte; cdecl; overload;
  function toString(value: Byte): JString; cdecl; overload;
  function valueOf(string_: JString): JByte; cdecl; overload;
  function valueOf(string_: JString; radix: Integer): JByte; cdecl; overload;
  function valueOf(b: Byte): JByte; cdecl; overload;
  {Properties}
  property MAX_VALUE: Byte read _GetMAX_VALUE;
  property MIN_VALUE: Byte read _GetMIN_VALUE;
  property SIZE: Integer read _GetSIZE;
  property &TYPE: Jlang_Class read _GetTYPE;
end;

[JavaSignature('java/lang/Byte')]
JByte = interface(JNumber)
['{882439AC-111F-445F-B6CD-2E1E8D793CDE}']
  {Methods}
  function byteValue: Byte; cdecl;
  function compareTo(object_: JByte): Integer; cdecl;
  function doubleValue: Double; cdecl;
  function equals(object_: JObject): Boolean; cdecl;
  function floatValue: Single; cdecl;
  function hashCode: Integer; cdecl;
  function intValue: Integer; cdecl;
  function longValue: Int64; cdecl;
  function shortValue: SmallInt; cdecl;
  function toString: JString; cdecl; overload;
end;
TJByte = class(TJavaGenericImport<JByteClass, JByte>) end;

JEnumerationClass = interface(IJavaClass)
['{5E393BCD-3EF2-4764-A59C-37B4D44C289A}']
end;

[JavaSignature('java/util/Enumeration')]
JEnumeration = interface(IJavaInstance)
['{8F9F8780-E6BE-4B67-A4F5-8EC28E1AE2EE}']
  {Methods}
  function hasMoreElements: Boolean; cdecl;
  function nextElement: JObject; cdecl;
end;
TJEnumeration = class(TJavaGenericImport<JEnumerationClass, JEnumeration>) end;

JCalendarClass = interface(JObjectClass)
['{51237FAA-7CDF-4E7E-9AE8-282DC2A930A1}']
  {Property Methods}
  function _GetALL_STYLES: Integer;
  function _GetAM: Integer;
  function _GetAM_PM: Integer;
  function _GetAPRIL: Integer;
  function _GetAUGUST: Integer;
  function _GetDATE: Integer;
  function _GetDAY_OF_MONTH: Integer;
  function _GetDAY_OF_WEEK: Integer;
  function _GetDAY_OF_WEEK_IN_MONTH: Integer;
  function _GetDAY_OF_YEAR: Integer;
  function _GetDECEMBER: Integer;
  function _GetDST_OFFSET: Integer;
  function _GetERA: Integer;
  function _GetFEBRUARY: Integer;
  function _GetFIELD_COUNT: Integer;
  function _GetFRIDAY: Integer;
  function _GetHOUR: Integer;
  function _GetHOUR_OF_DAY: Integer;
  function _GetJANUARY: Integer;
  function _GetJULY: Integer;
  function _GetJUNE: Integer;
  function _GetLONG: Integer;
  function _GetMARCH: Integer;
  function _GetMAY: Integer;
  function _GetMILLISECOND: Integer;
  function _GetMINUTE: Integer;
  function _GetMONDAY: Integer;
  function _GetMONTH: Integer;
  function _GetNOVEMBER: Integer;
  function _GetOCTOBER: Integer;
  function _GetPM: Integer;
  function _GetSATURDAY: Integer;
  function _GetSECOND: Integer;
  function _GetSEPTEMBER: Integer;
  function _GetSHORT: Integer;
  function _GetSUNDAY: Integer;
  function _GetTHURSDAY: Integer;
  function _GetTUESDAY: Integer;
  function _GetUNDECIMBER: Integer;
  function _GetWEDNESDAY: Integer;
  function _GetWEEK_OF_MONTH: Integer;
  function _GetWEEK_OF_YEAR: Integer;
  function _GetYEAR: Integer;
  function _GetZONE_OFFSET: Integer;
  {Methods}
  function getAvailableLocales: TJavaObjectArray<JLocale>; cdecl;
  function getInstance: JCalendar; cdecl; overload;
  function getInstance(locale: JLocale): JCalendar; cdecl; overload;
  function getInstance(timezone: JTimeZone): JCalendar; cdecl; overload;
  function getInstance(timezone: JTimeZone; locale: JLocale): JCalendar; cdecl; overload;
  {Properties}
  property ALL_STYLES: Integer read _GetALL_STYLES;
  property AM: Integer read _GetAM;
  property AM_PM: Integer read _GetAM_PM;
  property APRIL: Integer read _GetAPRIL;
  property AUGUST: Integer read _GetAUGUST;
  property DATE: Integer read _GetDATE;
  property DAY_OF_MONTH: Integer read _GetDAY_OF_MONTH;
  property DAY_OF_WEEK: Integer read _GetDAY_OF_WEEK;
  property DAY_OF_WEEK_IN_MONTH: Integer read _GetDAY_OF_WEEK_IN_MONTH;
  property DAY_OF_YEAR: Integer read _GetDAY_OF_YEAR;
  property DECEMBER: Integer read _GetDECEMBER;
  property DST_OFFSET: Integer read _GetDST_OFFSET;
  property ERA: Integer read _GetERA;
  property FEBRUARY: Integer read _GetFEBRUARY;
  property FIELD_COUNT: Integer read _GetFIELD_COUNT;
  property FRIDAY: Integer read _GetFRIDAY;
  property HOUR: Integer read _GetHOUR;
  property HOUR_OF_DAY: Integer read _GetHOUR_OF_DAY;
  property JANUARY: Integer read _GetJANUARY;
  property JULY: Integer read _GetJULY;
  property JUNE: Integer read _GetJUNE;
  property LONG: Integer read _GetLONG;
  property MARCH: Integer read _GetMARCH;
  property MAY: Integer read _GetMAY;
  property MILLISECOND: Integer read _GetMILLISECOND;
  property MINUTE: Integer read _GetMINUTE;
  property MONDAY: Integer read _GetMONDAY;
  property MONTH: Integer read _GetMONTH;
  property NOVEMBER: Integer read _GetNOVEMBER;
  property OCTOBER: Integer read _GetOCTOBER;
  property PM: Integer read _GetPM;
  property SATURDAY: Integer read _GetSATURDAY;
  property SECOND: Integer read _GetSECOND;
  property SEPTEMBER: Integer read _GetSEPTEMBER;
  property SHORT: Integer read _GetSHORT;
  property SUNDAY: Integer read _GetSUNDAY;
  property THURSDAY: Integer read _GetTHURSDAY;
  property TUESDAY: Integer read _GetTUESDAY;
  property UNDECIMBER: Integer read _GetUNDECIMBER;
  property WEDNESDAY: Integer read _GetWEDNESDAY;
  property WEEK_OF_MONTH: Integer read _GetWEEK_OF_MONTH;
  property WEEK_OF_YEAR: Integer read _GetWEEK_OF_YEAR;
  property YEAR: Integer read _GetYEAR;
  property ZONE_OFFSET: Integer read _GetZONE_OFFSET;
end;

[JavaSignature('java/util/Calendar')]
JCalendar = interface(JObject)
['{2C0409E5-97A4-47CA-9E75-6ACB1CA4515E}']
  {Methods}
  procedure add(field: Integer; value: Integer); cdecl;
  function after(calendar: JObject): Boolean; cdecl;
  function before(calendar: JObject): Boolean; cdecl;
  procedure clear; cdecl; overload;
  procedure clear(field: Integer); cdecl; overload;
  function clone: JObject; cdecl;
  function compareTo(anotherCalendar: JCalendar): Integer; cdecl;
  function equals(object_: JObject): Boolean; cdecl;
  function &get(field: Integer): Integer; cdecl;
  function getActualMaximum(field: Integer): Integer; cdecl;
  function getActualMinimum(field: Integer): Integer; cdecl;
  function getDisplayName(field: Integer; style: Integer; locale: JLocale): JString; cdecl;
  function getDisplayNames(field: Integer; style: Integer; locale: JLocale): JMap; cdecl;
  function getFirstDayOfWeek: Integer; cdecl;
  function getGreatestMinimum(field: Integer): Integer; cdecl;
  function getLeastMaximum(field: Integer): Integer; cdecl;
  function getMaximum(field: Integer): Integer; cdecl;
  function getMinimalDaysInFirstWeek: Integer; cdecl;
  function getMinimum(field: Integer): Integer; cdecl;
  function getTime: JDate; cdecl;
  function getTimeInMillis: Int64; cdecl;
  function getTimeZone: JTimeZone; cdecl;
  function hashCode: Integer; cdecl;
  function isLenient: Boolean; cdecl;
  function isSet(field: Integer): Boolean; cdecl;
  procedure roll(field: Integer; value: Integer); cdecl; overload;
  procedure roll(field: Integer; increment: Boolean); cdecl; overload;
  procedure &set(field: Integer; value: Integer); cdecl; overload;
  procedure &set(year: Integer; month: Integer; day: Integer); cdecl; overload;
  procedure &set(year: Integer; month: Integer; day: Integer; hourOfDay: Integer; minute: Integer); cdecl; overload;
  procedure &set(year: Integer; month: Integer; day: Integer; hourOfDay: Integer; minute: Integer; second: Integer); cdecl; overload;
  procedure setFirstDayOfWeek(value: Integer); cdecl;
  procedure setLenient(value: Boolean); cdecl;
  procedure setMinimalDaysInFirstWeek(value: Integer); cdecl;
  procedure setTime(date: JDate); cdecl;
  procedure setTimeInMillis(milliseconds: Int64); cdecl;
  procedure setTimeZone(timezone: JTimeZone); cdecl;
  function toString: JString; cdecl;
end;
TJCalendar = class(TJavaGenericImport<JCalendarClass, JCalendar>) end;

JAbstractCollectionClass = interface(JObjectClass)
['{27541496-F538-45DB-BFC7-9ED05E5680C3}']
end;

[JavaSignature('java/util/AbstractCollection')]
JAbstractCollection = interface(JObject)
['{4A5BA15A-2B07-4768-AA91-4BA9C93882C1}']
  {Methods}
  function add(object_: JObject): Boolean; cdecl;
  function addAll(collection: JCollection): Boolean; cdecl;
  procedure clear; cdecl;
  function &contains(object_: JObject): Boolean; cdecl;
  function containsAll(collection: JCollection): Boolean; cdecl;
  function isEmpty: Boolean; cdecl;
  function iterator: JIterator; cdecl;
  function remove(object_: JObject): Boolean; cdecl;
  function removeAll(collection: JCollection): Boolean; cdecl;
  function retainAll(collection: JCollection): Boolean; cdecl;
  function size: Integer; cdecl;
  function toArray: TJavaObjectArray<JObject>; cdecl; overload;
  function toArray(contents: TJavaObjectArray<JObject>): TJavaObjectArray<JObject>; cdecl; overload;
  function toString: JString; cdecl;
end;
TJAbstractCollection = class(TJavaGenericImport<JAbstractCollectionClass, JAbstractCollection>) end;

JAbstractSetClass = interface(JAbstractCollectionClass)
['{C8EA147C-D0DB-4E27-B8B5-77A04711A2F3}']
end;

[JavaSignature('java/util/AbstractSet')]
JAbstractSet = interface(JAbstractCollection)
['{A520B68E-843E-46B8-BBB3-1A40DE9E92CE}']
  {Methods}
  function equals(object_: JObject): Boolean; cdecl;
  function hashCode: Integer; cdecl;
  function removeAll(collection: JCollection): Boolean; cdecl;
end;
TJAbstractSet = class(TJavaGenericImport<JAbstractSetClass, JAbstractSet>) end;

JHashSetClass = interface(JAbstractSetClass)
['{7828E4D4-4F9F-493D-869E-92BE600444D5}']
  {Methods}
  function init: JHashSet; cdecl; overload;
  function init(capacity: Integer): JHashSet; cdecl; overload;
  function init(capacity: Integer; loadFactor: Single): JHashSet; cdecl; overload;
  function init(collection: JCollection): JHashSet; cdecl; overload;
end;

[JavaSignature('java/util/HashSet')]
JHashSet = interface(JAbstractSet)
['{A57B696D-8331-4C96-8759-7F2009371640}']
  {Methods}
  function add(object_: JObject): Boolean; cdecl;
  procedure clear; cdecl;
  function clone: JObject; cdecl;
  function &contains(object_: JObject): Boolean; cdecl;
  function isEmpty: Boolean; cdecl;
  function iterator: JIterator; cdecl;
  function remove(object_: JObject): Boolean; cdecl;
  function size: Integer; cdecl;
end;
TJHashSet = class(TJavaGenericImport<JHashSetClass, JHashSet>) end;

JAbstractListClass = interface(JAbstractCollectionClass)
['{4495F751-BABA-4349-8D4B-997761ED3876}']
end;

[JavaSignature('java/util/AbstractList')]
JAbstractList = interface(JAbstractCollection)
['{2E98325B-7293-4E06-A775-240FDD287E27}']
  {Methods}
  procedure add(location: Integer; object_: JObject); cdecl; overload;
  function add(object_: JObject): Boolean; cdecl; overload;
  function addAll(location: Integer; collection: JCollection): Boolean; cdecl;
  procedure clear; cdecl;
  function equals(object_: JObject): Boolean; cdecl;
  function &get(location: Integer): JObject; cdecl;
  function hashCode: Integer; cdecl;
  function indexOf(object_: JObject): Integer; cdecl;
  function iterator: JIterator; cdecl;
  function lastIndexOf(object_: JObject): Integer; cdecl;
  function listIterator: JListIterator; cdecl; overload;
  function listIterator(location: Integer): JListIterator; cdecl; overload;
  function remove(location: Integer): JObject; cdecl;
  function &set(location: Integer; object_: JObject): JObject; cdecl;
  function subList(start: Integer; end_: Integer): JList; cdecl;
end;
TJAbstractList = class(TJavaGenericImport<JAbstractListClass, JAbstractList>) end;

JSerializableClass = interface(IJavaClass)
['{BFE14BCE-11F1-41B5-A14F-3217521E82BA}']
end;

[JavaSignature('java/io/Serializable')]
JSerializable = interface(IJavaInstance)
['{D24AB8DC-4E6F-411D-9C40-2210F71A3B0D}']
end;
TJSerializable = class(TJavaGenericImport<JSerializableClass, JSerializable>) end;

JEnumClass = interface(JObjectClass)
['{2DB4C98D-F244-4372-9487-E9B9E2F48391}']
  {Methods}
  function valueOf(enumType: Jlang_Class; name: JString): JEnum; cdecl;
end;

[JavaSignature('java/lang/Enum')]
JEnum = interface(JObject)
['{0CFB5F00-FBF2-469D-806C-471A09BE1BAF}']
  {Methods}
  function compareTo(o: JEnum): Integer; cdecl;
  function equals(other: JObject): Boolean; cdecl;
  function getDeclaringClass: Jlang_Class; cdecl;
  function hashCode: Integer; cdecl;
  function name: JString; cdecl;
  function ordinal: Integer; cdecl;
  function toString: JString; cdecl;
end;
TJEnum = class(TJavaGenericImport<JEnumClass, JEnum>) end;

JThread_StateClass = interface(JEnumClass)
['{493F7CE3-3BE4-4CE5-9F96-7563BC2DC814}']
  {Property Methods}
  function _GetBLOCKED: JThread_State;
  function _GetNEW: JThread_State;
  function _GetRUNNABLE: JThread_State;
  function _GetTERMINATED: JThread_State;
  function _GetTIMED_WAITING: JThread_State;
  function _GetWAITING: JThread_State;
  {Methods}
  function valueOf(name: JString): JThread_State; cdecl;
  function values: TJavaObjectArray<JThread_State>; cdecl;
  {Properties}
  property BLOCKED: JThread_State read _GetBLOCKED;
  property NEW: JThread_State read _GetNEW;
  property RUNNABLE: JThread_State read _GetRUNNABLE;
  property TERMINATED: JThread_State read _GetTERMINATED;
  property TIMED_WAITING: JThread_State read _GetTIMED_WAITING;
  property WAITING: JThread_State read _GetWAITING;
end;

[JavaSignature('java/lang/Thread$State')]
JThread_State = interface(JEnum)
['{E3910394-C461-461E-9C1D-64E9BC367F84}']
end;
TJThread_State = class(TJavaGenericImport<JThread_StateClass, JThread_State>) end;

JThread_UncaughtExceptionHandlerClass = interface(IJavaClass)
['{3E2F71F3-BF00-457C-9970-9F1DA9EA7498}']
end;

[JavaSignature('java/lang/Thread$UncaughtExceptionHandler')]
JThread_UncaughtExceptionHandler = interface(IJavaInstance)
['{C9E75389-E9B3-45FF-9EA2-D7BC024DB9DA}']
  {Methods}
  procedure uncaughtException(thread: JThread; ex: JThrowable); cdecl;
end;
TJThread_UncaughtExceptionHandler = class(TJavaGenericImport<JThread_UncaughtExceptionHandlerClass, JThread_UncaughtExceptionHandler>) end;

JFileDescriptorClass = interface(JObjectClass)
['{B01F2343-4F8E-4FF8-838E-8FB9CFE304E2}']
  {Property Methods}
  function _Geterr: JFileDescriptor;
  function _Getin: JFileDescriptor;
  function _Getout: JFileDescriptor;
  {Methods}
  function init: JFileDescriptor; cdecl;
  {Properties}
  property err: JFileDescriptor read _Geterr;
  property &in: JFileDescriptor read _Getin;
  property &out: JFileDescriptor read _Getout;
end;

[JavaSignature('java/io/FileDescriptor')]
JFileDescriptor = interface(JObject)
['{B6D7B003-DD99-4563-93A3-F902501CD6C1}']
  {Methods}
  procedure sync; cdecl;
  function toString: JString; cdecl;
  function valid: Boolean; cdecl;
end;
TJFileDescriptor = class(TJavaGenericImport<JFileDescriptorClass, JFileDescriptor>) end;

JFileClass = interface(JObjectClass)
['{D2CE81B7-01CE-468B-A2F2-B85DD35642EC}']
  {Property Methods}
  function _GetpathSeparator: JString;
  function _GetpathSeparatorChar: Char;
  function _Getseparator: JString;
  function _GetseparatorChar: Char;
  {Methods}
  function init(dir: JFile; name: JString): JFile; cdecl; overload;
  function init(path: JString): JFile; cdecl; overload;
  function init(dirPath: JString; name: JString): JFile; cdecl; overload;
  function createTempFile(prefix: JString; suffix: JString): JFile; cdecl; overload;
  function createTempFile(prefix: JString; suffix: JString; directory: JFile): JFile; cdecl; overload;
  function listRoots: TJavaObjectArray<JFile>; cdecl;
  {Properties}
  property pathSeparator: JString read _GetpathSeparator;
  property pathSeparatorChar: Char read _GetpathSeparatorChar;
  property separator: JString read _Getseparator;
  property separatorChar: Char read _GetseparatorChar;
end;

[JavaSignature('java/io/File')]
JFile = interface(JObject)
['{38C3EB7E-315A-47D2-9052-1E61170EB37F}']
  {Methods}
  function canExecute: Boolean; cdecl;
  function canRead: Boolean; cdecl;
  function canWrite: Boolean; cdecl;
  function compareTo(another: JFile): Integer; cdecl;
  function createNewFile: Boolean; cdecl;
  function delete: Boolean; cdecl;
  procedure deleteOnExit; cdecl;
  function equals(obj: JObject): Boolean; cdecl;
  function exists: Boolean; cdecl;
  function getAbsoluteFile: JFile; cdecl;
  function getAbsolutePath: JString; cdecl;
  function getCanonicalFile: JFile; cdecl;
  function getCanonicalPath: JString; cdecl;
  function getFreeSpace: Int64; cdecl;
  function getName: JString; cdecl;
  function getParent: JString; cdecl;
  function getParentFile: JFile; cdecl;
  function getPath: JString; cdecl;
  function getTotalSpace: Int64; cdecl;
  function getUsableSpace: Int64; cdecl;
  function hashCode: Integer; cdecl;
  function isAbsolute: Boolean; cdecl;
  function isDirectory: Boolean; cdecl;
  function isFile: Boolean; cdecl;
  function isHidden: Boolean; cdecl;
  function lastModified: Int64; cdecl;
  function length: Int64; cdecl;
  function list: TJavaObjectArray<JString>; cdecl; overload;
  function list(filter: JFilenameFilter): TJavaObjectArray<JString>; cdecl; overload;
  function listFiles: TJavaObjectArray<JFile>; cdecl; overload;
  function listFiles(filter: JFilenameFilter): TJavaObjectArray<JFile>; cdecl; overload;
  function listFiles(filter: JFileFilter): TJavaObjectArray<JFile>; cdecl; overload;
  function mkdir: Boolean; cdecl;
  function mkdirs: Boolean; cdecl;
  function renameTo(newPath: JFile): Boolean; cdecl;
  function setExecutable(executable: Boolean; ownerOnly: Boolean): Boolean; cdecl; overload;
  function setExecutable(executable: Boolean): Boolean; cdecl; overload;
  function setLastModified(time: Int64): Boolean; cdecl;
  function setReadOnly: Boolean; cdecl;
  function setReadable(readable: Boolean; ownerOnly: Boolean): Boolean; cdecl; overload;
  function setReadable(readable: Boolean): Boolean; cdecl; overload;
  function setWritable(writable: Boolean; ownerOnly: Boolean): Boolean; cdecl; overload;
  function setWritable(writable: Boolean): Boolean; cdecl; overload;
  function toString: JString; cdecl;
end;
TJFile = class(TJavaGenericImport<JFileClass, JFile>) end;

JArrayListClass = interface(JAbstractListClass)
['{0CC7FC88-8B13-4F0A-9635-26FEEED49F94}']
  {Methods}
  function init(capacity: Integer): JArrayList; cdecl; overload;
  function init: JArrayList; cdecl; overload;
  function init(collection: JCollection): JArrayList; cdecl; overload;
end;

[JavaSignature('java/util/ArrayList')]
JArrayList = interface(JAbstractList)
['{B1D54E97-F848-4301-BA5B-F32921164AFA}']
  {Methods}
  function add(object_: JObject): Boolean; cdecl; overload;
  procedure add(index: Integer; object_: JObject); cdecl; overload;
  function addAll(collection: JCollection): Boolean; cdecl; overload;
  function addAll(index: Integer; collection: JCollection): Boolean; cdecl; overload;
  procedure clear; cdecl;
  function clone: JObject; cdecl;
  function &contains(object_: JObject): Boolean; cdecl;
  procedure ensureCapacity(minimumCapacity: Integer); cdecl;
  function equals(o: JObject): Boolean; cdecl;
  function &get(index: Integer): JObject; cdecl;
  function hashCode: Integer; cdecl;
  function indexOf(object_: JObject): Integer; cdecl;
  function isEmpty: Boolean; cdecl;
  function iterator: JIterator; cdecl;
  function lastIndexOf(object_: JObject): Integer; cdecl;
  function remove(index: Integer): JObject; cdecl; overload;
  function remove(object_: JObject): Boolean; cdecl; overload;
  function &set(index: Integer; object_: JObject): JObject; cdecl;
  function size: Integer; cdecl;
  function toArray: TJavaObjectArray<JObject>; cdecl; overload;
  function toArray(contents: TJavaObjectArray<JObject>): TJavaObjectArray<JObject>; cdecl; overload;
  procedure trimToSize; cdecl;
end;
TJArrayList = class(TJavaGenericImport<JArrayListClass, JArrayList>) end;

JRunnableClass = interface(IJavaClass)
['{49A6EA8E-0ADB-4D8E-8FA3-F13D4ADCF281}']
end;

[JavaSignature('java/lang/Runnable')]
JRunnable = interface(IJavaInstance)
['{BC131B27-7A72-4CAF-BB8E-170B8359B22E}']
  {Methods}
  procedure run; cdecl;
end;
TJRunnable = class(TJavaGenericImport<JRunnableClass, JRunnable>) end;

JCloseableClass = interface(IJavaClass)
['{CAFF3044-E3EC-444F-AF50-403A65BFA20B}']
end;

[JavaSignature('java/io/Closeable')]
JCloseable = interface(IJavaInstance)
['{DD3E86BD-46E1-44D8-84DD-B7607A3F9C56}']
  {Methods}
  procedure close; cdecl;
end;
TJCloseable = class(TJavaGenericImport<JCloseableClass, JCloseable>) end;

JWriterClass = interface(JObjectClass)
['{1B3FE1C9-6FF8-45AE-89D6-267E4CC1F003}']
end;

[JavaSignature('java/io/Writer')]
JWriter = interface(JObject)
['{50C5DAA8-B851-43A7-8FF9-E827DC14E67B}']
  {Methods}
  function append(c: Char): JWriter; cdecl; overload;
  function append(csq: JCharSequence): JWriter; cdecl; overload;
  function append(csq: JCharSequence; start: Integer; end_: Integer): JWriter; cdecl; overload;
  procedure close; cdecl;
  procedure flush; cdecl;
  procedure write(buf: TJavaArray<Char>); cdecl; overload;
  procedure write(buf: TJavaArray<Char>; offset: Integer; count: Integer); cdecl; overload;
  procedure write(oneChar: Integer); cdecl; overload;
  procedure write(str: JString); cdecl; overload;
  procedure write(str: JString; offset: Integer; count: Integer); cdecl; overload;
end;
TJWriter = class(TJavaGenericImport<JWriterClass, JWriter>) end;

JIterableClass = interface(IJavaClass)
['{EEADA3A8-2116-491E-ACC7-21F84F84D65A}']
end;

[JavaSignature('java/lang/Iterable')]
JIterable = interface(IJavaInstance)
['{ABC85F3B-F161-4206-882A-FFD5F1DEFEA2}']
  {Methods}
  function iterator: JIterator; cdecl;
end;
TJIterable = class(TJavaGenericImport<JIterableClass, JIterable>) end;

JCollectionClass = interface(JIterableClass)
['{2737AA1B-2E7C-406D-AF35-8B012C7D5803}']
end;

[JavaSignature('java/util/Collection')]
JCollection = interface(JIterable)
['{9E58EE70-C0A7-4660-BF62-945FAE9F5EC3}']
  {Methods}
  function add(object_: JObject): Boolean; cdecl;
  function addAll(collection: JCollection): Boolean; cdecl;
  procedure clear; cdecl;
  function &contains(object_: JObject): Boolean; cdecl;
  function containsAll(collection: JCollection): Boolean; cdecl;
  function equals(object_: JObject): Boolean; cdecl;
  function hashCode: Integer; cdecl;
  function isEmpty: Boolean; cdecl;
  function iterator: JIterator; cdecl;
  function remove(object_: JObject): Boolean; cdecl;
  function removeAll(collection: JCollection): Boolean; cdecl;
  function retainAll(collection: JCollection): Boolean; cdecl;
  function size: Integer; cdecl;
  function toArray: TJavaObjectArray<JObject>; cdecl; overload;
  function toArray(array_: TJavaObjectArray<JObject>): TJavaObjectArray<JObject>; cdecl; overload;
end;
TJCollection = class(TJavaGenericImport<JCollectionClass, JCollection>) end;

JListClass = interface(JCollectionClass)
['{8EA06296-143F-4381-9369-A77209B622F0}']
end;

[JavaSignature('java/util/List')]
JList = interface(JCollection)
['{3F85C565-F3F4-42D8-87EE-F724F72113C7}']
  {Methods}
  procedure add(location: Integer; object_: JObject); cdecl; overload;
  function add(object_: JObject): Boolean; cdecl; overload;
  function addAll(location: Integer; collection: JCollection): Boolean; cdecl; overload;
  function addAll(collection: JCollection): Boolean; cdecl; overload;
  procedure clear; cdecl;
  function &contains(object_: JObject): Boolean; cdecl;
  function containsAll(collection: JCollection): Boolean; cdecl;
  function equals(object_: JObject): Boolean; cdecl;
  function &get(location: Integer): JObject; cdecl;
  function hashCode: Integer; cdecl;
  function indexOf(object_: JObject): Integer; cdecl;
  function isEmpty: Boolean; cdecl;
  function iterator: JIterator; cdecl;
  function lastIndexOf(object_: JObject): Integer; cdecl;
  function listIterator: JListIterator; cdecl; overload;
  function listIterator(location: Integer): JListIterator; cdecl; overload;
  function remove(location: Integer): JObject; cdecl; overload;
  function remove(object_: JObject): Boolean; cdecl; overload;
  function removeAll(collection: JCollection): Boolean; cdecl;
  function retainAll(collection: JCollection): Boolean; cdecl;
  function &set(location: Integer; object_: JObject): JObject; cdecl;
  function size: Integer; cdecl;
  function subList(start: Integer; end_: Integer): JList; cdecl;
  function toArray: TJavaObjectArray<JObject>; cdecl; overload;
  function toArray(array_: TJavaObjectArray<JObject>): TJavaObjectArray<JObject>; cdecl; overload;
end;
TJList = class(TJavaGenericImport<JListClass, JList>) end;

JOutputStreamClass = interface(JObjectClass)
['{769D969C-3DFB-417B-8B7E-AA5662FB1539}']
  {Methods}
  function init: JOutputStream; cdecl;
end;

[JavaSignature('java/io/OutputStream')]
JOutputStream = interface(JObject)
['{308A10DA-ACF9-4EC3-B4BD-D9F9CEEB29A5}']
  {Methods}
  procedure close; cdecl;
  procedure flush; cdecl;
  procedure write(buffer: TJavaArray<Byte>); cdecl; overload;
  procedure write(buffer: TJavaArray<Byte>; offset: Integer; count: Integer); cdecl; overload;
  procedure write(oneByte: Integer); cdecl; overload;
end;
TJOutputStream = class(TJavaGenericImport<JOutputStreamClass, JOutputStream>) end;

Jlang_ClassClass = interface(JObjectClass)
['{E1A7F20A-FD87-4D67-9469-7492FD97D55D}']
  {Methods}
  function forName(className: JString): Jlang_Class; cdecl; overload;
  function forName(className: JString; initializeBoolean: Boolean; classLoader: JClassLoader): Jlang_Class; cdecl; overload;
end;

[JavaSignature('java/lang/Class')]
Jlang_Class = interface(JObject)
['{B056EDE6-77D8-4CDD-9864-147C201FD87C}']
  {Methods}
  function asSubclass(clazz: Jlang_Class): Jlang_Class; cdecl;
  function cast(obj: JObject): JObject; cdecl;
  function desiredAssertionStatus: Boolean; cdecl;
  function getAnnotation(annotationType: Jlang_Class): JAnnotation; cdecl;
  function getAnnotations: TJavaObjectArray<JAnnotation>; cdecl;
  function getCanonicalName: JString; cdecl;
  function getClassLoader: JClassLoader; cdecl;
  function getClasses: TJavaObjectArray<Jlang_Class>; cdecl;
  function getComponentType: Jlang_Class; cdecl;
  function getDeclaredAnnotations: TJavaObjectArray<JAnnotation>; cdecl;
  function getDeclaredClasses: TJavaObjectArray<Jlang_Class>; cdecl;
  function getDeclaringClass: Jlang_Class; cdecl;
  function getEnclosingClass: Jlang_Class; cdecl;
  function getEnumConstants: TJavaObjectArray<JObject>; cdecl;
  function getInterfaces: TJavaObjectArray<Jlang_Class>; cdecl;
  function getModifiers: Integer; cdecl;
  function getName: JString; cdecl;
  function getResourceAsStream(resName: JString): JInputStream; cdecl;
  function getSigners: TJavaObjectArray<JObject>; cdecl;
  function getSimpleName: JString; cdecl;
  function getSuperclass: Jlang_Class; cdecl;
  function isAnnotation: Boolean; cdecl;
  function isAnnotationPresent(annotationType: Jlang_Class): Boolean; cdecl;
  function isAnonymousClass: Boolean; cdecl;
  function isArray: Boolean; cdecl;
  function isAssignableFrom(cls: Jlang_Class): Boolean; cdecl;
  function isEnum: Boolean; cdecl;
  function isInstance(object_: JObject): Boolean; cdecl;
  function isInterface: Boolean; cdecl;
  function isLocalClass: Boolean; cdecl;
  function isMemberClass: Boolean; cdecl;
  function isPrimitive: Boolean; cdecl;
  function isSynthetic: Boolean; cdecl;
  function newInstance: JObject; cdecl;
  function toString: JString; cdecl;
end;
TJlang_Class = class(TJavaGenericImport<Jlang_ClassClass, Jlang_Class>) end;

JThrowableClass = interface(JObjectClass)
['{9B871585-74E6-4B49-B4C2-4DB387B0E599}']
  {Methods}
  function init: JThrowable; cdecl; overload;
  function init(detailMessage: JString): JThrowable; cdecl; overload;
  function init(detailMessage: JString; throwable: JThrowable): JThrowable; cdecl; overload;
  function init(throwable: JThrowable): JThrowable; cdecl; overload;
end;

[JavaSignature('java/lang/Throwable')]
JThrowable = interface(JObject)
['{44BECA0F-21B9-45A8-B21F-8806ABE80CE2}']
  {Methods}
  function fillInStackTrace: JThrowable; cdecl;
  function getCause: JThrowable; cdecl;
  function getLocalizedMessage: JString; cdecl;
  function getMessage: JString; cdecl;
  function getStackTrace: TJavaObjectArray<JStackTraceElement>; cdecl;
  function initCause(throwable: JThrowable): JThrowable; cdecl;
  procedure printStackTrace; cdecl; overload;
  procedure printStackTrace(err: JPrintStream); cdecl; overload;
  procedure printStackTrace(err: JPrintWriter); cdecl; overload;
  procedure setStackTrace(trace: TJavaObjectArray<JStackTraceElement>); cdecl;
  function toString: JString; cdecl;
end;
TJThrowable = class(TJavaGenericImport<JThrowableClass, JThrowable>) end;

JExceptionClass = interface(JThrowableClass)
['{6E1BA58E-A106-4CC0-A40C-99F4E1188B10}']
  {Methods}
  function init: JException; cdecl; overload;
  function init(detailMessage: JString): JException; cdecl; overload;
  function init(detailMessage: JString; throwable: JThrowable): JException; cdecl; overload;
  function init(throwable: JThrowable): JException; cdecl; overload;
end;

[JavaSignature('java/lang/Exception')]
JException = interface(JThrowable)
['{6EA7D981-2F3C-44C4-B9D2-F581529C08E0}']
end;
TJException = class(TJavaGenericImport<JExceptionClass, JException>) end;

JJSONExceptionClass = interface(JExceptionClass)
['{D92F06D5-D459-4309-AE86-21A7EF971C64}']
  {Methods}
  function init(s: JString): JJSONException; cdecl;
end;

[JavaSignature('org/json/JSONException')]
JJSONException = interface(JException)
['{236AB196-CC66-40D5-91E5-C3D202A9293C}']
end;
TJJSONException = class(TJavaGenericImport<JJSONExceptionClass, JJSONException>) end;

JUUIDClass = interface(JObjectClass)
['{F254C874-67C8-4832-9619-9F686CB8E466}']
  {Methods}
  function init(mostSigBits: Int64; leastSigBits: Int64): JUUID; cdecl;
  function fromString(uuid: JString): JUUID; cdecl;
  function nameUUIDFromBytes(name: TJavaArray<Byte>): JUUID; cdecl;
  function randomUUID: JUUID; cdecl;
end;

[JavaSignature('java/util/UUID')]
JUUID = interface(JObject)
['{B280C48F-E064-4030-BFD0-FB5970A78101}']
  {Methods}
  function clockSequence: Integer; cdecl;
  function compareTo(uuid: JUUID): Integer; cdecl;
  function equals(object_: JObject): Boolean; cdecl;
  function getLeastSignificantBits: Int64; cdecl;
  function getMostSignificantBits: Int64; cdecl;
  function hashCode: Integer; cdecl;
  function node: Int64; cdecl;
  function timestamp: Int64; cdecl;
  function toString: JString; cdecl;
  function variant: Integer; cdecl;
  function version: Integer; cdecl;
end;
TJUUID = class(TJavaGenericImport<JUUIDClass, JUUID>) end;

JAbstractMapClass = interface(JObjectClass)
['{05119E45-9501-4270-B2BB-EE7E314695CB}']
end;

[JavaSignature('java/util/AbstractMap')]
JAbstractMap = interface(JObject)
['{63FD2094-7BFB-41B4-AED8-F781B97F6EB6}']
  {Methods}
  procedure clear; cdecl;
  function containsKey(key: JObject): Boolean; cdecl;
  function containsValue(value: JObject): Boolean; cdecl;
  function entrySet: JSet; cdecl;
  function equals(object_: JObject): Boolean; cdecl;
  function &get(key: JObject): JObject; cdecl;
  function hashCode: Integer; cdecl;
  function isEmpty: Boolean; cdecl;
  function keySet: JSet; cdecl;
  function put(key: JObject; value: JObject): JObject; cdecl;
  procedure putAll(map: JMap); cdecl;
  function remove(key: JObject): JObject; cdecl;
  function size: Integer; cdecl;
  function toString: JString; cdecl;
  function values: JCollection; cdecl;
end;
TJAbstractMap = class(TJavaGenericImport<JAbstractMapClass, JAbstractMap>) end;

JHashMapClass = interface(JAbstractMapClass)
['{AC953BC1-405B-4CDD-93D2-FBA77D171B56}']
  {Methods}
  function init: JHashMap; cdecl; overload;
  function init(capacity: Integer): JHashMap; cdecl; overload;
  function init(capacity: Integer; loadFactor: Single): JHashMap; cdecl; overload;
  function init(map: JMap): JHashMap; cdecl; overload;
end;

[JavaSignature('java/util/HashMap')]
JHashMap = interface(JAbstractMap)
['{FD560211-A7FE-4AB5-B510-BB43A31AA75D}']
  {Methods}
  procedure clear; cdecl;
  function clone: JObject; cdecl;
  function containsKey(key: JObject): Boolean; cdecl;
  function containsValue(value: JObject): Boolean; cdecl;
  function entrySet: JSet; cdecl;
  function &get(key: JObject): JObject; cdecl;
  function isEmpty: Boolean; cdecl;
  function keySet: JSet; cdecl;
  function put(key: JObject; value: JObject): JObject; cdecl;
  procedure putAll(map: JMap); cdecl;
  function remove(key: JObject): JObject; cdecl;
  function size: Integer; cdecl;
  function values: JCollection; cdecl;
end;
TJHashMap = class(TJavaGenericImport<JHashMapClass, JHashMap>) end;

JRandomClass = interface(JObjectClass)
['{C50FE36A-6283-4523-BF77-15BB7A7B0F92}']
  {Methods}
  function init: JRandom; cdecl; overload;
  function init(seed: Int64): JRandom; cdecl; overload;
end;

[JavaSignature('java/util/Random')]
JRandom = interface(JObject)
['{F1C05381-73F2-4991-853B-B22575DB43D2}']
  {Methods}
  function nextBoolean: Boolean; cdecl;
  procedure nextBytes(buf: TJavaArray<Byte>); cdecl;
  function nextDouble: Double; cdecl;
  function nextFloat: Single; cdecl;
  function nextGaussian: Double; cdecl;
  function nextInt: Integer; cdecl; overload;
  function nextInt(n: Integer): Integer; cdecl; overload;
  function nextLong: Int64; cdecl;
  procedure setSeed(seed: Int64); cdecl;
end;
TJRandom = class(TJavaGenericImport<JRandomClass, JRandom>) end;

JAnnotationClass = interface(IJavaClass)
['{E8A654D9-AA21-468D-AEF1-9261C6E3F760}']
end;

[JavaSignature('java/lang/annotation/Annotation')]
JAnnotation = interface(IJavaInstance)
['{508C3063-7E6D-4963-B22F-27538F9D20CE}']
  {Methods}
  function annotationType: Jlang_Class; cdecl;
  function equals(obj: JObject): Boolean; cdecl;
  function hashCode: Integer; cdecl;
  function toString: JString; cdecl;
end;
TJAnnotation = class(TJavaGenericImport<JAnnotationClass, JAnnotation>) end;

JThreadClass = interface(JObjectClass)
['{AC2B33CB-D349-4506-8809-B9762209222B}']
  {Property Methods}
  function _GetMAX_PRIORITY: Integer;
  function _GetMIN_PRIORITY: Integer;
  function _GetNORM_PRIORITY: Integer;
  {Methods}
  function init: JThread; cdecl; overload;
  function init(runnable: JRunnable): JThread; cdecl; overload;
  function init(runnable: JRunnable; threadName: JString): JThread; cdecl; overload;
  function init(threadName: JString): JThread; cdecl; overload;
  function init(group: JThreadGroup; runnable: JRunnable): JThread; cdecl; overload;
  function init(group: JThreadGroup; runnable: JRunnable; threadName: JString): JThread; cdecl; overload;
  function init(group: JThreadGroup; threadName: JString): JThread; cdecl; overload;
  function init(group: JThreadGroup; runnable: JRunnable; threadName: JString; stackSize: Int64): JThread; cdecl; overload;
  function activeCount: Integer; cdecl;
  function currentThread: JThread; cdecl;
  procedure dumpStack; cdecl;
  function enumerate(threads: TJavaObjectArray<JThread>): Integer; cdecl;
  function getAllStackTraces: TJavaObjectArray<JMap>; cdecl;
  function getDefaultUncaughtExceptionHandler: JThread_UncaughtExceptionHandler; cdecl;
  function holdsLock(object_: JObject): Boolean; cdecl;
  function interrupted: Boolean; cdecl;
  procedure setDefaultUncaughtExceptionHandler(handler: JThread_UncaughtExceptionHandler); cdecl;
  procedure sleep(time: Int64); cdecl; overload;
  procedure sleep(millis: Int64; nanos: Integer); cdecl; overload;
  procedure yield; cdecl;
  {Properties}
  property MAX_PRIORITY: Integer read _GetMAX_PRIORITY;
  property MIN_PRIORITY: Integer read _GetMIN_PRIORITY;
  property NORM_PRIORITY: Integer read _GetNORM_PRIORITY;
end;

[JavaSignature('java/lang/Thread')]
JThread = interface(JObject)
['{8E288CBE-F5A4-4D6E-98B7-D0B5075A0FCA}']
  {Methods}
  procedure checkAccess; cdecl;
  function countStackFrames: Integer; cdecl;//Deprecated
  procedure destroy; cdecl;//Deprecated
  function getContextClassLoader: JClassLoader; cdecl;
  function getId: Int64; cdecl;
  function getName: JString; cdecl;
  function getPriority: Integer; cdecl;
  function getStackTrace: TJavaObjectArray<JStackTraceElement>; cdecl;
  function getState: JThread_State; cdecl;
  function getThreadGroup: JThreadGroup; cdecl;
  function getUncaughtExceptionHandler: JThread_UncaughtExceptionHandler; cdecl;
  procedure interrupt; cdecl;
  function isAlive: Boolean; cdecl;
  function isDaemon: Boolean; cdecl;
  function isInterrupted: Boolean; cdecl;
  procedure join; cdecl; overload;
  procedure join(millis: Int64); cdecl; overload;
  procedure join(millis: Int64; nanos: Integer); cdecl; overload;
  procedure resume; cdecl;//Deprecated
  procedure run; cdecl;
  procedure setContextClassLoader(cl: JClassLoader); cdecl;
  procedure setDaemon(isDaemon: Boolean); cdecl;
  procedure setName(threadName: JString); cdecl;
  procedure setPriority(priority: Integer); cdecl;
  procedure setUncaughtExceptionHandler(handler: JThread_UncaughtExceptionHandler); cdecl;
  procedure start; cdecl;
  procedure stop; cdecl; overload;//Deprecated
  procedure stop(throwable: JThrowable); cdecl; overload;//Deprecated
  procedure suspend; cdecl;//Deprecated
  function toString: JString; cdecl;
end;
TJThread = class(TJavaGenericImport<JThreadClass, JThread>) end;

JDateClass = interface(JObjectClass)
['{37EABF6D-C7EE-4AB5-BE8B-5E439112E116}']
  {Methods}
  function init: JDate; cdecl; overload;
  function init(year: Integer; month: Integer; day: Integer): JDate; cdecl; overload;//Deprecated
  function init(year: Integer; month: Integer; day: Integer; hour: Integer; minute: Integer): JDate; cdecl; overload;//Deprecated
  function init(year: Integer; month: Integer; day: Integer; hour: Integer; minute: Integer; second: Integer): JDate; cdecl; overload;//Deprecated
  function init(milliseconds: Int64): JDate; cdecl; overload;
  function init(string_: JString): JDate; cdecl; overload;//Deprecated
  function UTC(year: Integer; month: Integer; day: Integer; hour: Integer; minute: Integer; second: Integer): Int64; cdecl;//Deprecated
  function parse(string_: JString): Int64; cdecl;//Deprecated
end;

[JavaSignature('java/util/Date')]
JDate = interface(JObject)
['{282E2836-B390-44E4-A14F-EF481460BDF7}']
  {Methods}
  function after(date: JDate): Boolean; cdecl;
  function before(date: JDate): Boolean; cdecl;
  function clone: JObject; cdecl;
  function compareTo(date: JDate): Integer; cdecl;
  function equals(object_: JObject): Boolean; cdecl;
  function getDate: Integer; cdecl;//Deprecated
  function getDay: Integer; cdecl;//Deprecated
  function getHours: Integer; cdecl;//Deprecated
  function getMinutes: Integer; cdecl;//Deprecated
  function getMonth: Integer; cdecl;//Deprecated
  function getSeconds: Integer; cdecl;//Deprecated
  function getTime: Int64; cdecl;
  function getTimezoneOffset: Integer; cdecl;//Deprecated
  function getYear: Integer; cdecl;//Deprecated
  function hashCode: Integer; cdecl;
  procedure setDate(day: Integer); cdecl;//Deprecated
  procedure setHours(hour: Integer); cdecl;//Deprecated
  procedure setMinutes(minute: Integer); cdecl;//Deprecated
  procedure setMonth(month: Integer); cdecl;//Deprecated
  procedure setSeconds(second: Integer); cdecl;//Deprecated
  procedure setTime(milliseconds: Int64); cdecl;
  procedure setYear(year: Integer); cdecl;//Deprecated
  function toGMTString: JString; cdecl;//Deprecated
  function toLocaleString: JString; cdecl;//Deprecated
  function toString: JString; cdecl;
end;
TJDate = class(TJavaGenericImport<JDateClass, JDate>) end;

JFilterOutputStreamClass = interface(JOutputStreamClass)
['{4273682E-2DA8-4BA9-BFC7-A9356DC43D40}']
  {Methods}
  function init(out_: JOutputStream): JFilterOutputStream; cdecl;
end;

[JavaSignature('java/io/FilterOutputStream')]
JFilterOutputStream = interface(JOutputStream)
['{B0DB7F97-9758-43B1-8FC4-19A12503CD3F}']
  {Methods}
  procedure close; cdecl;
  procedure flush; cdecl;
  procedure write(buffer: TJavaArray<Byte>; offset: Integer; length: Integer); cdecl; overload;
  procedure write(oneByte: Integer); cdecl; overload;
end;
TJFilterOutputStream = class(TJavaGenericImport<JFilterOutputStreamClass, JFilterOutputStream>) end;

JPrintStreamClass = interface(JFilterOutputStreamClass)
['{4B5683E3-32D0-4225-9A80-FB961D9B334F}']
  {Methods}
  function init(out_: JOutputStream): JPrintStream; cdecl; overload;
  function init(out_: JOutputStream; autoFlush: Boolean): JPrintStream; cdecl; overload;
  function init(out_: JOutputStream; autoFlush: Boolean; enc: JString): JPrintStream; cdecl; overload;
  function init(file_: JFile): JPrintStream; cdecl; overload;
  function init(file_: JFile; csn: JString): JPrintStream; cdecl; overload;
  function init(fileName: JString): JPrintStream; cdecl; overload;
  function init(fileName: JString; csn: JString): JPrintStream; cdecl; overload;
end;

[JavaSignature('java/io/PrintStream')]
JPrintStream = interface(JFilterOutputStream)
['{8B23171F-06EF-4D87-A463-863F687A7918}']
  {Methods}
  function append(c: Char): JPrintStream; cdecl; overload;
  function append(charSequence: JCharSequence): JPrintStream; cdecl; overload;
  function append(charSequence: JCharSequence; start: Integer; end_: Integer): JPrintStream; cdecl; overload;
  function checkError: Boolean; cdecl;
  procedure close; cdecl;
  procedure flush; cdecl;
  procedure print(chars: TJavaArray<Char>); cdecl; overload;
  procedure print(c: Char); cdecl; overload;
  procedure print(d: Double); cdecl; overload;
  procedure print(f: Single); cdecl; overload;
  procedure print(i: Integer); cdecl; overload;
  procedure print(l: Int64); cdecl; overload;
  procedure print(o: JObject); cdecl; overload;
  procedure print(str: JString); cdecl; overload;
  procedure print(b: Boolean); cdecl; overload;
  procedure println; cdecl; overload;
  procedure println(chars: TJavaArray<Char>); cdecl; overload;
  procedure println(c: Char); cdecl; overload;
  procedure println(d: Double); cdecl; overload;
  procedure println(f: Single); cdecl; overload;
  procedure println(i: Integer); cdecl; overload;
  procedure println(l: Int64); cdecl; overload;
  procedure println(o: JObject); cdecl; overload;
  procedure println(str: JString); cdecl; overload;
  procedure println(b: Boolean); cdecl; overload;
  procedure write(buffer: TJavaArray<Byte>; offset: Integer; length: Integer); cdecl; overload;
  procedure write(oneByte: Integer); cdecl; overload;
end;
TJPrintStream = class(TJavaGenericImport<JPrintStreamClass, JPrintStream>) end;

JRuntimeExceptionClass = interface(JExceptionClass)
['{58C58616-58EF-4783-92DB-5AE4F2A079A7}']
  {Methods}
  function init: JRuntimeException; cdecl; overload;
  function init(detailMessage: JString): JRuntimeException; cdecl; overload;
  function init(detailMessage: JString; throwable: JThrowable): JRuntimeException; cdecl; overload;
  function init(throwable: JThrowable): JRuntimeException; cdecl; overload;
end;

[JavaSignature('java/lang/RuntimeException')]
JRuntimeException = interface(JException)
['{7CEA4E55-B247-4073-A601-7C2C6D8BEE22}']
end;
TJRuntimeException = class(TJavaGenericImport<JRuntimeExceptionClass, JRuntimeException>) end;

JObserverClass = interface(IJavaClass)
['{8582EA20-ECD9-4C10-95BD-2C89B4D5BA6E}']
end;

[JavaSignature('java/util/Observer')]
JObserver = interface(IJavaInstance)
['{452A1BDA-4B4E-406E-B455-BC56F012C1B7}']
  {Methods}
  procedure update(observable: Jutil_Observable; data: JObject); cdecl;
end;
TJObserver = class(TJavaGenericImport<JObserverClass, JObserver>) end;

JFloatClass = interface(JNumberClass)
['{E2E64017-238D-4910-8DF8-BD66A034BDFE}']
  {Property Methods}
  function _GetMAX_EXPONENT: Integer;
  function _GetMAX_VALUE: Single;
  function _GetMIN_EXPONENT: Integer;
  function _GetMIN_NORMAL: Single;
  function _GetMIN_VALUE: Single;
  function _GetNEGATIVE_INFINITY: Single;
  function _GetNaN: Single;
  function _GetPOSITIVE_INFINITY: Single;
  function _GetSIZE: Integer;
  function _GetTYPE: Jlang_Class;
  {Methods}
  function init(value: Single): JFloat; cdecl; overload;
  function init(value: Double): JFloat; cdecl; overload;
  function init(string_: JString): JFloat; cdecl; overload;
  function compare(float1: Single; float2: Single): Integer; cdecl;
  function floatToIntBits(value: Single): Integer; cdecl;
  function floatToRawIntBits(value: Single): Integer; cdecl;
  function intBitsToFloat(bits: Integer): Single; cdecl;
  function isInfinite(f: Single): Boolean; cdecl; overload;
  function isNaN(f: Single): Boolean; cdecl; overload;
  function parseFloat(string_: JString): Single; cdecl;
  function toHexString(f: Single): JString; cdecl;
  function toString(f: Single): JString; cdecl; overload;
  function valueOf(string_: JString): JFloat; cdecl; overload;
  function valueOf(f: Single): JFloat; cdecl; overload;
  {Properties}
  property MAX_EXPONENT: Integer read _GetMAX_EXPONENT;
  property MAX_VALUE: Single read _GetMAX_VALUE;
  property MIN_EXPONENT: Integer read _GetMIN_EXPONENT;
  property MIN_NORMAL: Single read _GetMIN_NORMAL;
  property MIN_VALUE: Single read _GetMIN_VALUE;
  property NEGATIVE_INFINITY: Single read _GetNEGATIVE_INFINITY;
  property NaN: Single read _GetNaN;
  property POSITIVE_INFINITY: Single read _GetPOSITIVE_INFINITY;
  property SIZE: Integer read _GetSIZE;
  property &TYPE: Jlang_Class read _GetTYPE;
end;

[JavaSignature('java/lang/Float')]
JFloat = interface(JNumber)
['{F13BF843-909A-4866-918B-B1B2B1A8F483}']
  {Methods}
  function byteValue: Byte; cdecl;
  function compareTo(object_: JFloat): Integer; cdecl;
  function doubleValue: Double; cdecl;
  function equals(object_: JObject): Boolean; cdecl;
  function floatValue: Single; cdecl;
  function hashCode: Integer; cdecl;
  function intValue: Integer; cdecl;
  function isInfinite: Boolean; cdecl; overload;
  function isNaN: Boolean; cdecl; overload;
  function longValue: Int64; cdecl;
  function shortValue: SmallInt; cdecl;
  function toString: JString; cdecl; overload;
end;
TJFloat = class(TJavaGenericImport<JFloatClass, JFloat>) end;

JDoubleClass = interface(JNumberClass)
['{1B133955-7ECE-4429-97CD-9118396AC3AE}']
  {Property Methods}
  function _GetMAX_EXPONENT: Integer;
  function _GetMAX_VALUE: Double;
  function _GetMIN_EXPONENT: Integer;
  function _GetMIN_NORMAL: Double;
  function _GetMIN_VALUE: Double;
  function _GetNEGATIVE_INFINITY: Double;
  function _GetNaN: Double;
  function _GetPOSITIVE_INFINITY: Double;
  function _GetSIZE: Integer;
  function _GetTYPE: Jlang_Class;
  {Methods}
  function init(value: Double): JDouble; cdecl; overload;
  function init(string_: JString): JDouble; cdecl; overload;
  function compare(double1: Double; double2: Double): Integer; cdecl;
  function doubleToLongBits(value: Double): Int64; cdecl;
  function doubleToRawLongBits(value: Double): Int64; cdecl;
  function isInfinite(d: Double): Boolean; cdecl; overload;
  function isNaN(d: Double): Boolean; cdecl; overload;
  function longBitsToDouble(bits: Int64): Double; cdecl;
  function parseDouble(string_: JString): Double; cdecl;
  function toHexString(d: Double): JString; cdecl;
  function toString(d: Double): JString; cdecl; overload;
  function valueOf(string_: JString): JDouble; cdecl; overload;
  function valueOf(d: Double): JDouble; cdecl; overload;
  {Properties}
  property MAX_EXPONENT: Integer read _GetMAX_EXPONENT;
  property MAX_VALUE: Double read _GetMAX_VALUE;
  property MIN_EXPONENT: Integer read _GetMIN_EXPONENT;
  property MIN_NORMAL: Double read _GetMIN_NORMAL;
  property MIN_VALUE: Double read _GetMIN_VALUE;
  property NEGATIVE_INFINITY: Double read _GetNEGATIVE_INFINITY;
  property NaN: Double read _GetNaN;
  property POSITIVE_INFINITY: Double read _GetPOSITIVE_INFINITY;
  property SIZE: Integer read _GetSIZE;
  property &TYPE: Jlang_Class read _GetTYPE;
end;

[JavaSignature('java/lang/Double')]
JDouble = interface(JNumber)
['{81639AF9-E21C-4CB0-99E6-1E7F013E11CC}']
  {Methods}
  function byteValue: Byte; cdecl;
  function compareTo(object_: JDouble): Integer; cdecl;
  function doubleValue: Double; cdecl;
  function equals(object_: JObject): Boolean; cdecl;
  function floatValue: Single; cdecl;
  function hashCode: Integer; cdecl;
  function intValue: Integer; cdecl;
  function isInfinite: Boolean; cdecl; overload;
  function isNaN: Boolean; cdecl; overload;
  function longValue: Int64; cdecl;
  function shortValue: SmallInt; cdecl;
  function toString: JString; cdecl; overload;
end;
TJDouble = class(TJavaGenericImport<JDoubleClass, JDouble>) end;

JPrintWriterClass = interface(JWriterClass)
['{0176F2C9-CDCB-40D9-B26E-E983BE269B0D}']
  {Methods}
  function init(out_: JOutputStream): JPrintWriter; cdecl; overload;
  function init(out_: JOutputStream; autoFlush: Boolean): JPrintWriter; cdecl; overload;
  function init(wr: JWriter): JPrintWriter; cdecl; overload;
  function init(wr: JWriter; autoFlush: Boolean): JPrintWriter; cdecl; overload;
  function init(file_: JFile): JPrintWriter; cdecl; overload;
  function init(file_: JFile; csn: JString): JPrintWriter; cdecl; overload;
  function init(fileName: JString): JPrintWriter; cdecl; overload;
  function init(fileName: JString; csn: JString): JPrintWriter; cdecl; overload;
end;

[JavaSignature('java/io/PrintWriter')]
JPrintWriter = interface(JWriter)
['{1C7483CD-045F-4478-A223-A9FBBF9C1D80}']
  {Methods}
  function append(c: Char): JPrintWriter; cdecl; overload;
  function append(csq: JCharSequence): JPrintWriter; cdecl; overload;
  function append(csq: JCharSequence; start: Integer; end_: Integer): JPrintWriter; cdecl; overload;
  function checkError: Boolean; cdecl;
  procedure close; cdecl;
  procedure flush; cdecl;
  procedure print(charArray: TJavaArray<Char>); cdecl; overload;
  procedure print(ch: Char); cdecl; overload;
  procedure print(dnum: Double); cdecl; overload;
  procedure print(fnum: Single); cdecl; overload;
  procedure print(inum: Integer); cdecl; overload;
  procedure print(lnum: Int64); cdecl; overload;
  procedure print(obj: JObject); cdecl; overload;
  procedure print(str: JString); cdecl; overload;
  procedure print(bool: Boolean); cdecl; overload;
  procedure println; cdecl; overload;
  procedure println(chars: TJavaArray<Char>); cdecl; overload;
  procedure println(c: Char); cdecl; overload;
  procedure println(d: Double); cdecl; overload;
  procedure println(f: Single); cdecl; overload;
  procedure println(i: Integer); cdecl; overload;
  procedure println(l: Int64); cdecl; overload;
  procedure println(obj: JObject); cdecl; overload;
  procedure println(str: JString); cdecl; overload;
  procedure println(b: Boolean); cdecl; overload;
  procedure write(buf: TJavaArray<Char>); cdecl; overload;
  procedure write(buf: TJavaArray<Char>; offset: Integer; count: Integer); cdecl; overload;
  procedure write(oneChar: Integer); cdecl; overload;
  procedure write(str: JString); cdecl; overload;
  procedure write(str: JString; offset: Integer; count: Integer); cdecl; overload;
end;
TJPrintWriter = class(TJavaGenericImport<JPrintWriterClass, JPrintWriter>) end;

JIteratorClass = interface(IJavaClass)
['{2E525F5D-C766-4F79-B800-BA5FFA909E90}']
end;

[JavaSignature('java/util/Iterator')]
JIterator = interface(IJavaInstance)
['{435EBC1F-CFE0-437C-B49B-45B5257B6953}']
  {Methods}
  function hasNext: Boolean; cdecl;
  function next: JObject; cdecl;
  procedure remove; cdecl;
end;
TJIterator = class(TJavaGenericImport<JIteratorClass, JIterator>) end;

JListIteratorClass = interface(JIteratorClass)
['{7541F5DD-8E71-44AE-ACD9-142ED2D42810}']
end;

[JavaSignature('java/util/ListIterator')]
JListIterator = interface(JIterator)
['{B66BDA33-5CDD-43B1-B320-7353AE09C418}']
  {Methods}
  procedure add(object_: JObject); cdecl;
  function hasNext: Boolean; cdecl;
  function hasPrevious: Boolean; cdecl;
  function next: JObject; cdecl;
  function nextIndex: Integer; cdecl;
  function previous: JObject; cdecl;
  function previousIndex: Integer; cdecl;
  procedure remove; cdecl;
  procedure &set(object_: JObject); cdecl;
end;
TJListIterator = class(TJavaGenericImport<JListIteratorClass, JListIterator>) end;

JByteArrayOutputStreamClass = interface(JOutputStreamClass)
['{2F08E462-5F49-4A89-9ACD-F0A5CD01C3A8}']
  {Methods}
  function init: JByteArrayOutputStream; cdecl; overload;
  function init(size: Integer): JByteArrayOutputStream; cdecl; overload;
end;

[JavaSignature('java/io/ByteArrayOutputStream')]
JByteArrayOutputStream = interface(JOutputStream)
['{6AD653C4-3A67-4CCD-9B53-2E57D0DC0727}']
  {Methods}
  procedure close; cdecl;
  procedure reset; cdecl;
  function size: Integer; cdecl;
  function toByteArray: TJavaArray<Byte>; cdecl;
  function toString: JString; cdecl; overload;
  function toString(hibyte: Integer): JString; cdecl; overload;//Deprecated
  function toString(enc: JString): JString; cdecl; overload;
  procedure write(buffer: TJavaArray<Byte>; offset: Integer; len: Integer); cdecl; overload;
  procedure write(oneByte: Integer); cdecl; overload;
  procedure writeTo(out_: JOutputStream); cdecl;
end;
TJByteArrayOutputStream = class(TJavaGenericImport<JByteArrayOutputStreamClass, JByteArrayOutputStream>) end;

JStackTraceElementClass = interface(JObjectClass)
['{21CBE31F-4A81-4CB3-ADB1-EA9B3166692E}']
  {Methods}
  function init(cls: JString; method: JString; file_: JString; line: Integer): JStackTraceElement; cdecl;
end;

[JavaSignature('java/lang/StackTraceElement')]
JStackTraceElement = interface(JObject)
['{3304B89A-29EB-4B53-943F-E70F4252E8FF}']
  {Methods}
  function equals(obj: JObject): Boolean; cdecl;
  function getClassName: JString; cdecl;
  function getFileName: JString; cdecl;
  function getLineNumber: Integer; cdecl;
  function getMethodName: JString; cdecl;
  function hashCode: Integer; cdecl;
  function isNativeMethod: Boolean; cdecl;
  function toString: JString; cdecl;
end;
TJStackTraceElement = class(TJavaGenericImport<JStackTraceElementClass, JStackTraceElement>) end;

JFileOutputStreamClass = interface(JOutputStreamClass)
['{4808736C-4C9B-46DF-A1B6-EB94324D9666}']
  {Methods}
  function init(file_: JFile): JFileOutputStream; cdecl; overload;
  function init(file_: JFile; append: Boolean): JFileOutputStream; cdecl; overload;
  function init(fd: JFileDescriptor): JFileOutputStream; cdecl; overload;
  function init(path: JString): JFileOutputStream; cdecl; overload;
  function init(path: JString; append: Boolean): JFileOutputStream; cdecl; overload;
end;

[JavaSignature('java/io/FileOutputStream')]
JFileOutputStream = interface(JOutputStream)
['{3D49DAFB-A222-4001-9DBE-7FAE66E23404}']
  {Methods}
  procedure close; cdecl;
  function getFD: JFileDescriptor; cdecl;
  procedure write(buffer: TJavaArray<Byte>; byteOffset: Integer; byteCount: Integer); cdecl; overload;
  procedure write(oneByte: Integer); cdecl; overload;
end;
TJFileOutputStream = class(TJavaGenericImport<JFileOutputStreamClass, JFileOutputStream>) end;

JAbstractStringBuilderClass = interface(JObjectClass)
['{A3321EF2-EA76-44CD-90CE-DFDADB9936BD}']
end;

[JavaSignature('java/lang/AbstractStringBuilder')]
JAbstractStringBuilder = interface(JObject)
['{39A0E6C5-8F79-44ED-BECB-02252CA2F5C0}']
  {Methods}
  function capacity: Integer; cdecl;
  function charAt(index: Integer): Char; cdecl;
  function codePointAt(index: Integer): Integer; cdecl;
  function codePointBefore(index: Integer): Integer; cdecl;
  function codePointCount(start: Integer; end_: Integer): Integer; cdecl;
  procedure ensureCapacity(min: Integer); cdecl;
  procedure getChars(start: Integer; end_: Integer; dst: TJavaArray<Char>; dstStart: Integer); cdecl;
  function indexOf(string_: JString): Integer; cdecl; overload;
  function indexOf(subString: JString; start: Integer): Integer; cdecl; overload;
  function lastIndexOf(string_: JString): Integer; cdecl; overload;
  function lastIndexOf(subString: JString; start: Integer): Integer; cdecl; overload;
  function length: Integer; cdecl;
  function offsetByCodePoints(index: Integer; codePointOffset: Integer): Integer; cdecl;
  procedure setCharAt(index: Integer; ch: Char); cdecl;
  procedure setLength(length: Integer); cdecl;
  function subSequence(start: Integer; end_: Integer): JCharSequence; cdecl;
  function substring(start: Integer): JString; cdecl; overload;
  function substring(start: Integer; end_: Integer): JString; cdecl; overload;
  function toString: JString; cdecl;
  procedure trimToSize; cdecl;
end;
TJAbstractStringBuilder = class(TJavaGenericImport<JAbstractStringBuilderClass, JAbstractStringBuilder>) end;

JStringBuilderClass = interface(JAbstractStringBuilderClass)
['{D9FACB66-EE60-4BCB-B5B2-248751CCF1B4}']
  {Methods}
  function init: JStringBuilder; cdecl; overload;
  function init(capacity: Integer): JStringBuilder; cdecl; overload;
  function init(seq: JCharSequence): JStringBuilder; cdecl; overload;
  function init(str: JString): JStringBuilder; cdecl; overload;
end;

[JavaSignature('java/lang/StringBuilder')]
JStringBuilder = interface(JAbstractStringBuilder)
['{F8A75A66-EA10-4337-9ECC-B0CA4FF4D9C5}']
  {Methods}
  function append(b: Boolean): JStringBuilder; cdecl; overload;
  function append(c: Char): JStringBuilder; cdecl; overload;
  function append(i: Integer): JStringBuilder; cdecl; overload;
  function append(l: Int64): JStringBuilder; cdecl; overload;
  function append(f: Single): JStringBuilder; cdecl; overload;
  function append(d: Double): JStringBuilder; cdecl; overload;
  function append(obj: JObject): JStringBuilder; cdecl; overload;
  function append(str: JString): JStringBuilder; cdecl; overload;
  function append(sb: JStringBuffer): JStringBuilder; cdecl; overload;
  function append(chars: TJavaArray<Char>): JStringBuilder; cdecl; overload;
  function append(str: TJavaArray<Char>; offset: Integer; len: Integer): JStringBuilder; cdecl; overload;
  function append(csq: JCharSequence): JStringBuilder; cdecl; overload;
  function append(csq: JCharSequence; start: Integer; end_: Integer): JStringBuilder; cdecl; overload;
  function appendCodePoint(codePoint: Integer): JStringBuilder; cdecl;
  function delete(start: Integer; end_: Integer): JStringBuilder; cdecl;
  function deleteCharAt(index: Integer): JStringBuilder; cdecl;
  function insert(offset: Integer; b: Boolean): JStringBuilder; cdecl; overload;
  function insert(offset: Integer; c: Char): JStringBuilder; cdecl; overload;
  function insert(offset: Integer; i: Integer): JStringBuilder; cdecl; overload;
  function insert(offset: Integer; l: Int64): JStringBuilder; cdecl; overload;
  function insert(offset: Integer; f: Single): JStringBuilder; cdecl; overload;
  function insert(offset: Integer; d: Double): JStringBuilder; cdecl; overload;
  function insert(offset: Integer; obj: JObject): JStringBuilder; cdecl; overload;
  function insert(offset: Integer; str: JString): JStringBuilder; cdecl; overload;
  function insert(offset: Integer; ch: TJavaArray<Char>): JStringBuilder; cdecl; overload;
  function insert(offset: Integer; str: TJavaArray<Char>; strOffset: Integer; strLen: Integer): JStringBuilder; cdecl; overload;
  function insert(offset: Integer; s: JCharSequence): JStringBuilder; cdecl; overload;
  function insert(offset: Integer; s: JCharSequence; start: Integer; end_: Integer): JStringBuilder; cdecl; overload;
  function replace(start: Integer; end_: Integer; str: JString): JStringBuilder; cdecl;
  function reverse: JStringBuilder; cdecl;
  function toString: JString; cdecl;
end;
TJStringBuilder = class(TJavaGenericImport<JStringBuilderClass, JStringBuilder>) end;

JCharSequenceClass = interface(IJavaClass)
['{85DCA69A-F296-4BB4-8FE2-5ECE0EBE6611}']
end;

[JavaSignature('java/lang/CharSequence')]
JCharSequence = interface(IJavaInstance)
['{D026566C-D7C6-43E7-AECA-030E2C23A8B8}']
  {Methods}
  function charAt(index: Integer): Char; cdecl;
  function length: Integer; cdecl;
  function subSequence(start: Integer; end_: Integer): JCharSequence; cdecl;
  function toString: JString; cdecl;
end;
TJCharSequence = class(TJavaGenericImport<JCharSequenceClass, JCharSequence>) end;

JGregorianCalendarClass = interface(JCalendarClass)
['{69F4EF00-93DA-4249-8A30-3A3E4A71DA03}']
  {Property Methods}
  function _GetAD: Integer;
  function _GetBC: Integer;
  {Methods}
  function init: JGregorianCalendar; cdecl; overload;
  function init(year: Integer; month: Integer; day: Integer): JGregorianCalendar; cdecl; overload;
  function init(year: Integer; month: Integer; day: Integer; hour: Integer; minute: Integer): JGregorianCalendar; cdecl; overload;
  function init(year: Integer; month: Integer; day: Integer; hour: Integer; minute: Integer; second: Integer): JGregorianCalendar; cdecl; overload;
  function init(locale: JLocale): JGregorianCalendar; cdecl; overload;
  function init(timezone: JTimeZone): JGregorianCalendar; cdecl; overload;
  function init(timezone: JTimeZone; locale: JLocale): JGregorianCalendar; cdecl; overload;
  {Properties}
  property AD: Integer read _GetAD;
  property BC: Integer read _GetBC;
end;

[JavaSignature('java/util/GregorianCalendar')]
JGregorianCalendar = interface(JCalendar)
['{CB851885-16EA-49E7-8AAF-DBFE900DA328}']
  {Methods}
  procedure add(field: Integer; value: Integer); cdecl;
  function clone: JObject; cdecl;
  function equals(object_: JObject): Boolean; cdecl;
  function getActualMaximum(field: Integer): Integer; cdecl;
  function getActualMinimum(field: Integer): Integer; cdecl;
  function getGreatestMinimum(field: Integer): Integer; cdecl;
  function getGregorianChange: JDate; cdecl;
  function getLeastMaximum(field: Integer): Integer; cdecl;
  function getMaximum(field: Integer): Integer; cdecl;
  function getMinimum(field: Integer): Integer; cdecl;
  function hashCode: Integer; cdecl;
  function isLeapYear(year: Integer): Boolean; cdecl;
  procedure roll(field: Integer; value: Integer); cdecl; overload;
  procedure roll(field: Integer; increment: Boolean); cdecl; overload;
  procedure setFirstDayOfWeek(value: Integer); cdecl;
  procedure setGregorianChange(date: JDate); cdecl;
  procedure setMinimalDaysInFirstWeek(value: Integer); cdecl;
end;
TJGregorianCalendar = class(TJavaGenericImport<JGregorianCalendarClass, JGregorianCalendar>) end;

JJSONTokenerClass = interface(JObjectClass)
['{CFDB19D3-6222-4DBF-9012-1EF6EA1D518D}']
  {Methods}
  function init(in_: JString): JJSONTokener; cdecl;
  function dehexchar(hex: Char): Integer; cdecl;
end;

[JavaSignature('org/json/JSONTokener')]
JJSONTokener = interface(JObject)
['{A7330D36-4304-4864-BACD-547E8AF8AAAD}']
  {Methods}
  procedure back; cdecl;
  function more: Boolean; cdecl;
  function next: Char; cdecl; overload;
  function next(c: Char): Char; cdecl; overload;
  function next(length: Integer): JString; cdecl; overload;
  function nextClean: Char; cdecl;
  function nextString(quote: Char): JString; cdecl;
  function nextTo(excluded: JString): JString; cdecl; overload;
  function nextTo(excluded: Char): JString; cdecl; overload;
  function nextValue: JObject; cdecl;
  procedure skipPast(thru: JString); cdecl;
  function skipTo(to_: Char): Char; cdecl;
  function syntaxError(message: JString): JJSONException; cdecl;
  function toString: JString; cdecl;
end;
TJJSONTokener = class(TJavaGenericImport<JJSONTokenerClass, JJSONTokener>) end;

JMapClass = interface(IJavaClass)
['{2A7CE403-063B-45CA-9F4D-EA1E64304F1C}']
end;

[JavaSignature('java/util/Map')]
JMap = interface(IJavaInstance)
['{BE6A5DBF-B121-4BF2-BC18-EB64729C7811}']
  {Methods}
  procedure clear; cdecl;
  function containsKey(key: JObject): Boolean; cdecl;
  function containsValue(value: JObject): Boolean; cdecl;
  function entrySet: JSet; cdecl;
  function equals(object_: JObject): Boolean; cdecl;
  function &get(key: JObject): JObject; cdecl;
  function hashCode: Integer; cdecl;
  function isEmpty: Boolean; cdecl;
  function keySet: JSet; cdecl;
  function put(key: JObject; value: JObject): JObject; cdecl;
  procedure putAll(map: JMap); cdecl;
  function remove(key: JObject): JObject; cdecl;
  function size: Integer; cdecl;
  function values: JCollection; cdecl;
end;
TJMap = class(TJavaGenericImport<JMapClass, JMap>) end;

JLocaleClass = interface(JObjectClass)
['{0A5D70AA-C01B-437F-97C8-FEE25C595AE7}']
  {Property Methods}
  function _GetCANADA: JLocale;
  function _GetCANADA_FRENCH: JLocale;
  function _GetCHINA: JLocale;
  function _GetCHINESE: JLocale;
  function _GetENGLISH: JLocale;
  function _GetFRANCE: JLocale;
  function _GetFRENCH: JLocale;
  function _GetGERMAN: JLocale;
  function _GetGERMANY: JLocale;
  function _GetITALIAN: JLocale;
  function _GetITALY: JLocale;
  function _GetJAPAN: JLocale;
  function _GetJAPANESE: JLocale;
  function _GetKOREA: JLocale;
  function _GetKOREAN: JLocale;
  function _GetPRC: JLocale;
  function _GetROOT: JLocale;
  function _GetSIMPLIFIED_CHINESE: JLocale;
  function _GetTAIWAN: JLocale;
  function _GetTRADITIONAL_CHINESE: JLocale;
  function _GetUK: JLocale;
  function _GetUS: JLocale;
  {Methods}
  function init(language: JString): JLocale; cdecl; overload;
  function init(language: JString; country: JString): JLocale; cdecl; overload;
  function init(language: JString; country: JString; variant: JString): JLocale; cdecl; overload;
  function getAvailableLocales: TJavaObjectArray<JLocale>; cdecl;
  function getDefault: JLocale; cdecl;
  function getISOCountries: TJavaObjectArray<JString>; cdecl;
  function getISOLanguages: TJavaObjectArray<JString>; cdecl;
  procedure setDefault(locale: JLocale); cdecl;
  {Properties}
  property CANADA: JLocale read _GetCANADA;
  property CANADA_FRENCH: JLocale read _GetCANADA_FRENCH;
  property CHINA: JLocale read _GetCHINA;
  property CHINESE: JLocale read _GetCHINESE;
  property ENGLISH: JLocale read _GetENGLISH;
  property FRANCE: JLocale read _GetFRANCE;
  property FRENCH: JLocale read _GetFRENCH;
  property GERMAN: JLocale read _GetGERMAN;
  property GERMANY: JLocale read _GetGERMANY;
  property ITALIAN: JLocale read _GetITALIAN;
  property ITALY: JLocale read _GetITALY;
  property JAPAN: JLocale read _GetJAPAN;
  property JAPANESE: JLocale read _GetJAPANESE;
  property KOREA: JLocale read _GetKOREA;
  property KOREAN: JLocale read _GetKOREAN;
  property PRC: JLocale read _GetPRC;
  property ROOT: JLocale read _GetROOT;
  property SIMPLIFIED_CHINESE: JLocale read _GetSIMPLIFIED_CHINESE;
  property TAIWAN: JLocale read _GetTAIWAN;
  property TRADITIONAL_CHINESE: JLocale read _GetTRADITIONAL_CHINESE;
  property UK: JLocale read _GetUK;
  property US: JLocale read _GetUS;
end;

[JavaSignature('java/util/Locale')]
JLocale = interface(JObject)
['{877ADE25-1D13-4963-9A17-17EE17B3A0A8}']
  {Methods}
  function clone: JObject; cdecl;
  function equals(object_: JObject): Boolean; cdecl;
  function getCountry: JString; cdecl;
  function getDisplayCountry: JString; cdecl; overload;
  function getDisplayCountry(locale: JLocale): JString; cdecl; overload;
  function getDisplayLanguage: JString; cdecl; overload;
  function getDisplayLanguage(locale: JLocale): JString; cdecl; overload;
  function getDisplayName: JString; cdecl; overload;
  function getDisplayName(locale: JLocale): JString; cdecl; overload;
  function getDisplayVariant: JString; cdecl; overload;
  function getDisplayVariant(locale: JLocale): JString; cdecl; overload;
  function getISO3Country: JString; cdecl;
  function getISO3Language: JString; cdecl;
  function getLanguage: JString; cdecl;
  function getVariant: JString; cdecl;
  function hashCode: Integer; cdecl;
  function toString: JString; cdecl;
end;
TJLocale = class(TJavaGenericImport<JLocaleClass, JLocale>) end;

JTimeZoneClass = interface(JObjectClass)
['{8F823620-CE10-44D5-82BA-24BFD63DCF80}']
  {Property Methods}
  function _GetLONG: Integer;
  function _GetSHORT: Integer;
  {Methods}
  function init: JTimeZone; cdecl;
  function getAvailableIDs: TJavaObjectArray<JString>; cdecl; overload;
  function getAvailableIDs(offsetMillis: Integer): TJavaObjectArray<JString>; cdecl; overload;
  function getDefault: JTimeZone; cdecl;
  function getTimeZone(id: JString): JTimeZone; cdecl;
  procedure setDefault(timeZone: JTimeZone); cdecl;
  {Properties}
  property LONG: Integer read _GetLONG;
  property SHORT: Integer read _GetSHORT;
end;

[JavaSignature('java/util/TimeZone')]
JTimeZone = interface(JObject)
['{9D5215F4-A1B5-4B24-8B0B-EB3B88A0328D}']
  {Methods}
  function clone: JObject; cdecl;
  function getDSTSavings: Integer; cdecl;
  function getDisplayName: JString; cdecl; overload;
  function getDisplayName(locale: JLocale): JString; cdecl; overload;
  function getDisplayName(daylightTime: Boolean; style: Integer): JString; cdecl; overload;
  function getDisplayName(daylightTime: Boolean; style: Integer; locale: JLocale): JString; cdecl; overload;
  function getID: JString; cdecl;
  function getOffset(time: Int64): Integer; cdecl; overload;
  function getOffset(era: Integer; year: Integer; month: Integer; day: Integer; dayOfWeek: Integer; timeOfDayMillis: Integer): Integer; cdecl; overload;
  function getRawOffset: Integer; cdecl;
  function hasSameRules(timeZone: JTimeZone): Boolean; cdecl;
  function inDaylightTime(time: JDate): Boolean; cdecl;
  procedure setID(id: JString); cdecl;
  procedure setRawOffset(offsetMillis: Integer); cdecl;
  function useDaylightTime: Boolean; cdecl;
end;
TJTimeZone = class(TJavaGenericImport<JTimeZoneClass, JTimeZone>) end;

JFileFilterClass = interface(IJavaClass)
['{74779212-F9FA-40FE-A5C2-41FBC7919220}']
end;

[JavaSignature('java/io/FileFilter')]
JFileFilter = interface(IJavaInstance)
['{5A5564B5-D25E-4D6A-AF92-F0725E9011DE}']
  {Methods}
  function accept(pathname: JFile): Boolean; cdecl;
end;
TJFileFilter = class(TJavaGenericImport<JFileFilterClass, JFileFilter>) end;

JEnumSetClass = interface(JAbstractSetClass)
['{67EF0287-D91B-44E0-9574-4CA9974FBC38}']
  {Methods}
  function allOf(elementType: Jlang_Class): JEnumSet; cdecl;
  function complementOf(s: JEnumSet): JEnumSet; cdecl;
  function copyOf(s: JEnumSet): JEnumSet; cdecl; overload;
  function copyOf(c: JCollection): JEnumSet; cdecl; overload;
  function noneOf(elementType: Jlang_Class): JEnumSet; cdecl;
  function &of(e: JEnum): JEnumSet; cdecl; overload;
  function &of(e1: JEnum; e2: JEnum): JEnumSet; cdecl; overload;
  function &of(e1: JEnum; e2: JEnum; e3: JEnum): JEnumSet; cdecl; overload;
  function &of(e1: JEnum; e2: JEnum; e3: JEnum; e4: JEnum): JEnumSet; cdecl; overload;
  function &of(e1: JEnum; e2: JEnum; e3: JEnum; e4: JEnum; e5: JEnum): JEnumSet; cdecl; overload;
  function range(start: JEnum; end_: JEnum): JEnumSet; cdecl;
end;

[JavaSignature('java/util/EnumSet')]
JEnumSet = interface(JAbstractSet)
['{C8A6B028-B797-406A-9EE4-B65671555D97}']
  {Methods}
  function clone: JEnumSet; cdecl;
end;
TJEnumSet = class(TJavaGenericImport<JEnumSetClass, JEnumSet>) end;

Jutil_ObservableClass = interface(JObjectClass)
['{2BD8C696-02FF-4378-A514-ACD431BEE106}']
  {Methods}
  function init: Jutil_Observable; cdecl;
end;

[JavaSignature('java/util/Observable')]
Jutil_Observable = interface(JObject)
['{B8443F0E-B41C-4475-934B-1C917FCF617B}']
  {Methods}
  procedure addObserver(observer: JObserver); cdecl;
  function countObservers: Integer; cdecl;
  procedure deleteObserver(observer: JObserver); cdecl;
  procedure deleteObservers; cdecl;
  function hasChanged: Boolean; cdecl;
  procedure notifyObservers; cdecl; overload;
  procedure notifyObservers(data: JObject); cdecl; overload;
end;
TJutil_Observable = class(TJavaGenericImport<Jutil_ObservableClass, Jutil_Observable>) end;

JFilenameFilterClass = interface(IJavaClass)
['{E466E540-E65D-43EC-8913-E8F8AEEA354F}']
end;

[JavaSignature('java/io/FilenameFilter')]
JFilenameFilter = interface(IJavaInstance)
['{B55A4F67-1AE9-41F5-BCF8-305D3902A782}']
  {Methods}
  function accept(dir: JFile; filename: JString): Boolean; cdecl;
end;
TJFilenameFilter = class(TJavaGenericImport<JFilenameFilterClass, JFilenameFilter>) end;

JJSONObjectClass = interface(JObjectClass)
['{32FBF926-19C3-45AF-A29E-C312D95B34CC}']
  {Property Methods}
  function _GetNULL: JObject;
  {Methods}
  function init: JJSONObject; cdecl; overload;
  function init(copyFrom: JMap): JJSONObject; cdecl; overload;
  function init(readFrom: JJSONTokener): JJSONObject; cdecl; overload;
  function init(json: JString): JJSONObject; cdecl; overload;
  function init(copyFrom: JJSONObject; names: TJavaObjectArray<JString>): JJSONObject; cdecl; overload;
  function numberToString(number: JNumber): JString; cdecl;
  function quote(data: JString): JString; cdecl;
  {Properties}
  property NULL: JObject read _GetNULL;
end;

[JavaSignature('org/json/JSONObject')]
JJSONObject = interface(JObject)
['{7B4F68E8-ADFC-40EC-A119-37FA9778A11C}']
  {Methods}
  function accumulate(name: JString; value: JObject): JJSONObject; cdecl;
  function &get(name: JString): JObject; cdecl;
  function getBoolean(name: JString): Boolean; cdecl;
  function getDouble(name: JString): Double; cdecl;
  function getInt(name: JString): Integer; cdecl;
  function getJSONArray(name: JString): JJSONArray; cdecl;
  function getJSONObject(name: JString): JJSONObject; cdecl;
  function getLong(name: JString): Int64; cdecl;
  function getString(name: JString): JString; cdecl;
  function has(name: JString): Boolean; cdecl;
  function isNull(name: JString): Boolean; cdecl;
  function keys: JIterator; cdecl;
  function length: Integer; cdecl;
  function names: JJSONArray; cdecl;
  function opt(name: JString): JObject; cdecl;
  function optBoolean(name: JString): Boolean; cdecl; overload;
  function optBoolean(name: JString; fallback: Boolean): Boolean; cdecl; overload;
  function optDouble(name: JString): Double; cdecl; overload;
  function optDouble(name: JString; fallback: Double): Double; cdecl; overload;
  function optInt(name: JString): Integer; cdecl; overload;
  function optInt(name: JString; fallback: Integer): Integer; cdecl; overload;
  function optJSONArray(name: JString): JJSONArray; cdecl;
  function optJSONObject(name: JString): JJSONObject; cdecl;
  function optLong(name: JString): Int64; cdecl; overload;
  function optLong(name: JString; fallback: Int64): Int64; cdecl; overload;
  function optString(name: JString): JString; cdecl; overload;
  function optString(name: JString; fallback: JString): JString; cdecl; overload;
  function put(name: JString; value: Boolean): JJSONObject; cdecl; overload;
  function put(name: JString; value: Double): JJSONObject; cdecl; overload;
  function put(name: JString; value: Integer): JJSONObject; cdecl; overload;
  function put(name: JString; value: Int64): JJSONObject; cdecl; overload;
  function put(name: JString; value: JObject): JJSONObject; cdecl; overload;
  function putOpt(name: JString; value: JObject): JJSONObject; cdecl;
  function remove(name: JString): JObject; cdecl;
  function toJSONArray(names: JJSONArray): JJSONArray; cdecl;
  function toString: JString; cdecl; overload;
  function toString(indentSpaces: Integer): JString; cdecl; overload;
end;
TJJSONObject = class(TJavaGenericImport<JJSONObjectClass, JJSONObject>) end;

JStringClass = interface(JObjectClass)
['{E61829D1-1FD3-49B2-BAC6-FB0FFDB1A495}']
  {Property Methods}
  function _GetCASE_INSENSITIVE_ORDER: JComparator;
  {Methods}
  function init: JString; cdecl; overload;
  function init(data: TJavaArray<Byte>): JString; cdecl; overload;
  function init(data: TJavaArray<Byte>; high: Integer): JString; cdecl; overload;//Deprecated
  function init(data: TJavaArray<Byte>; offset: Integer; byteCount: Integer): JString; cdecl; overload;
  function init(data: TJavaArray<Byte>; high: Integer; offset: Integer; byteCount: Integer): JString; cdecl; overload;//Deprecated
  function init(data: TJavaArray<Byte>; offset: Integer; byteCount: Integer; charsetName: JString): JString; cdecl; overload;
  function init(data: TJavaArray<Byte>; charsetName: JString): JString; cdecl; overload;
  function init(data: TJavaArray<Char>): JString; cdecl; overload;
  function init(data: TJavaArray<Char>; offset: Integer; charCount: Integer): JString; cdecl; overload;
  function init(toCopy: JString): JString; cdecl; overload;
  function init(stringBuffer: JStringBuffer): JString; cdecl; overload;
  function init(codePoints: TJavaArray<Integer>; offset: Integer; count: Integer): JString; cdecl; overload;
  function init(stringBuilder: JStringBuilder): JString; cdecl; overload;
  function copyValueOf(data: TJavaArray<Char>): JString; cdecl; overload;
  function copyValueOf(data: TJavaArray<Char>; start: Integer; length: Integer): JString; cdecl; overload;
  function valueOf(data: TJavaArray<Char>): JString; cdecl; overload;
  function valueOf(data: TJavaArray<Char>; start: Integer; length: Integer): JString; cdecl; overload;
  function valueOf(value: Char): JString; cdecl; overload;
  function valueOf(value: Double): JString; cdecl; overload;
  function valueOf(value: Single): JString; cdecl; overload;
  function valueOf(value: Integer): JString; cdecl; overload;
  function valueOf(value: Int64): JString; cdecl; overload;
  function valueOf(value: JObject): JString; cdecl; overload;
  function valueOf(value: Boolean): JString; cdecl; overload;
  {Properties}
  property CASE_INSENSITIVE_ORDER: JComparator read _GetCASE_INSENSITIVE_ORDER;
end;

[JavaSignature('java/lang/String')]
JString = interface(JObject)
['{8579B374-1E68-4729-AE3C-C8DA0A6D6F9F}']
  {Methods}
  function charAt(index: Integer): Char; cdecl;
  function codePointAt(index: Integer): Integer; cdecl;
  function codePointBefore(index: Integer): Integer; cdecl;
  function codePointCount(start: Integer; end_: Integer): Integer; cdecl;
  function compareTo(string_: JString): Integer; cdecl;
  function compareToIgnoreCase(string_: JString): Integer; cdecl;
  function concat(string_: JString): JString; cdecl;
  function &contains(cs: JCharSequence): Boolean; cdecl;
  function contentEquals(strbuf: JStringBuffer): Boolean; cdecl; overload;
  function contentEquals(cs: JCharSequence): Boolean; cdecl; overload;
  function endsWith(suffix: JString): Boolean; cdecl;
  function equals(object_: JObject): Boolean; cdecl;
  function equalsIgnoreCase(string_: JString): Boolean; cdecl;
  procedure getBytes(start: Integer; end_: Integer; data: TJavaArray<Byte>; index: Integer); cdecl; overload;//Deprecated
  function getBytes: TJavaArray<Byte>; cdecl; overload;
  function getBytes(charsetName: JString): TJavaArray<Byte>; cdecl; overload;
  procedure getChars(start: Integer; end_: Integer; buffer: TJavaArray<Char>; index: Integer); cdecl;
  function hashCode: Integer; cdecl;
  function indexOf(c: Integer): Integer; cdecl; overload;
  function indexOf(c: Integer; start: Integer): Integer; cdecl; overload;
  function indexOf(string_: JString): Integer; cdecl; overload;
  function indexOf(subString: JString; start: Integer): Integer; cdecl; overload;
  function intern: JString; cdecl;
  function isEmpty: Boolean; cdecl;
  function lastIndexOf(c: Integer): Integer; cdecl; overload;
  function lastIndexOf(c: Integer; start: Integer): Integer; cdecl; overload;
  function lastIndexOf(string_: JString): Integer; cdecl; overload;
  function lastIndexOf(subString: JString; start: Integer): Integer; cdecl; overload;
  function length: Integer; cdecl;
  function matches(regularExpression: JString): Boolean; cdecl;
  function offsetByCodePoints(index: Integer; codePointOffset: Integer): Integer; cdecl;
  function regionMatches(thisStart: Integer; string_: JString; start: Integer; length: Integer): Boolean; cdecl; overload;
  function regionMatches(ignoreCase: Boolean; thisStart: Integer; string_: JString; start: Integer; length: Integer): Boolean; cdecl; overload;
  function replace(oldChar: Char; newChar: Char): JString; cdecl; overload;
  function replace(target: JCharSequence; replacement: JCharSequence): JString; cdecl; overload;
  function replaceAll(regularExpression: JString; replacement: JString): JString; cdecl;
  function replaceFirst(regularExpression: JString; replacement: JString): JString; cdecl;
  function split(regularExpression: JString): TJavaObjectArray<JString>; cdecl; overload;
  function split(regularExpression: JString; limit: Integer): TJavaObjectArray<JString>; cdecl; overload;
  function startsWith(prefix: JString): Boolean; cdecl; overload;
  function startsWith(prefix: JString; start: Integer): Boolean; cdecl; overload;
  function subSequence(start: Integer; end_: Integer): JCharSequence; cdecl;
  function substring(start: Integer): JString; cdecl; overload;
  function substring(start: Integer; end_: Integer): JString; cdecl; overload;
  function toCharArray: TJavaArray<Char>; cdecl;
  function toLowerCase: JString; cdecl; overload;
  function toLowerCase(locale: JLocale): JString; cdecl; overload;
  function toString: JString; cdecl;
  function toUpperCase: JString; cdecl; overload;
  function toUpperCase(locale: JLocale): JString; cdecl; overload;
  function trim: JString; cdecl;
end;
TJString = class(TJavaGenericImport<JStringClass, JString>) end;

JSetClass = interface(JCollectionClass)
['{A3E290FD-FD46-4DA8-B728-07B04920F5DE}']
end;

[JavaSignature('java/util/Set')]
JSet = interface(JCollection)
['{07BF19A2-0C1C-4ABF-9028-1F99DD0E0A79}']
  {Methods}
  function add(object_: JObject): Boolean; cdecl;
  function addAll(collection: JCollection): Boolean; cdecl;
  procedure clear; cdecl;
  function &contains(object_: JObject): Boolean; cdecl;
  function containsAll(collection: JCollection): Boolean; cdecl;
  function equals(object_: JObject): Boolean; cdecl;
  function hashCode: Integer; cdecl;
  function isEmpty: Boolean; cdecl;
  function iterator: JIterator; cdecl;
  function remove(object_: JObject): Boolean; cdecl;
  function removeAll(collection: JCollection): Boolean; cdecl;
  function retainAll(collection: JCollection): Boolean; cdecl;
  function size: Integer; cdecl;
  function toArray: TJavaObjectArray<JObject>; cdecl; overload;
  function toArray(array_: TJavaObjectArray<JObject>): TJavaObjectArray<JObject>; cdecl; overload;
end;
TJSet = class(TJavaGenericImport<JSetClass, JSet>) end;

JShortClass = interface(JNumberClass)
['{FAD495F3-40B7-46DB-B3B6-8DBBD38D8E16}']
  {Property Methods}
  function _GetMAX_VALUE: SmallInt;
  function _GetMIN_VALUE: SmallInt;
  function _GetSIZE: Integer;
  function _GetTYPE: Jlang_Class;
  {Methods}
  function init(string_: JString): JShort; cdecl; overload;
  function init(value: SmallInt): JShort; cdecl; overload;
  function decode(string_: JString): JShort; cdecl;
  function parseShort(string_: JString): SmallInt; cdecl; overload;
  function parseShort(string_: JString; radix: Integer): SmallInt; cdecl; overload;
  function reverseBytes(s: SmallInt): SmallInt; cdecl;
  function toString(value: SmallInt): JString; cdecl; overload;
  function valueOf(string_: JString): JShort; cdecl; overload;
  function valueOf(string_: JString; radix: Integer): JShort; cdecl; overload;
  function valueOf(s: SmallInt): JShort; cdecl; overload;
  {Properties}
  property MAX_VALUE: SmallInt read _GetMAX_VALUE;
  property MIN_VALUE: SmallInt read _GetMIN_VALUE;
  property SIZE: Integer read _GetSIZE;
  property &TYPE: Jlang_Class read _GetTYPE;
end;

[JavaSignature('java/lang/Short')]
JShort = interface(JNumber)
['{48D3B355-1222-4BD6-94BF-F40B5EE8EF02}']
  {Methods}
  function byteValue: Byte; cdecl;
  function compareTo(object_: JShort): Integer; cdecl;
  function doubleValue: Double; cdecl;
  function equals(object_: JObject): Boolean; cdecl;
  function floatValue: Single; cdecl;
  function hashCode: Integer; cdecl;
  function intValue: Integer; cdecl;
  function longValue: Int64; cdecl;
  function shortValue: SmallInt; cdecl;
  function toString: JString; cdecl; overload;
end;
TJShort = class(TJavaGenericImport<JShortClass, JShort>) end;

JThreadGroupClass = interface(JObjectClass)
['{D7D65FE0-0CB7-4C72-9129-C344705D0F4C}']
  {Methods}
  function init(name: JString): JThreadGroup; cdecl; overload;
  function init(parent: JThreadGroup; name: JString): JThreadGroup; cdecl; overload;
end;

[JavaSignature('java/lang/ThreadGroup')]
JThreadGroup = interface(JObject)
['{5BF3F856-7BFB-444A-8059-341CBC2A10B2}']
  {Methods}
  function activeCount: Integer; cdecl;
  function activeGroupCount: Integer; cdecl;
  function allowThreadSuspension(b: Boolean): Boolean; cdecl;//Deprecated
  procedure checkAccess; cdecl;
  procedure destroy; cdecl;
  function enumerate(threads: TJavaObjectArray<JThread>): Integer; cdecl; overload;
  function enumerate(threads: TJavaObjectArray<JThread>; recurse: Boolean): Integer; cdecl; overload;
  function enumerate(groups: TJavaObjectArray<JThreadGroup>): Integer; cdecl; overload;
  function enumerate(groups: TJavaObjectArray<JThreadGroup>; recurse: Boolean): Integer; cdecl; overload;
  function getMaxPriority: Integer; cdecl;
  function getName: JString; cdecl;
  function getParent: JThreadGroup; cdecl;
  procedure interrupt; cdecl;
  function isDaemon: Boolean; cdecl;
  function isDestroyed: Boolean; cdecl;
  procedure list; cdecl;
  function parentOf(g: JThreadGroup): Boolean; cdecl;
  procedure resume; cdecl;//Deprecated
  procedure setDaemon(isDaemon: Boolean); cdecl;
  procedure setMaxPriority(newMax: Integer); cdecl;
  procedure stop; cdecl;//Deprecated
  procedure suspend; cdecl;//Deprecated
  function toString: JString; cdecl;
  procedure uncaughtException(t: JThread; e: JThrowable); cdecl;
end;
TJThreadGroup = class(TJavaGenericImport<JThreadGroupClass, JThreadGroup>) end;

JComparatorClass = interface(IJavaClass)
['{BFB6395F-2694-4292-A1B5-87CC1138FB77}']
end;

[JavaSignature('java/util/Comparator')]
JComparator = interface(IJavaInstance)
['{0754C41C-92B8-483B-88F0-B48BFE216D46}']
  {Methods}
  function compare(lhs: JObject; rhs: JObject): Integer; cdecl;
  function equals(object_: JObject): Boolean; cdecl;
end;
TJComparator = class(TJavaGenericImport<JComparatorClass, JComparator>) end;

JJSONArrayClass = interface(JObjectClass)
['{34FBA399-2B13-49B4-BD53-5BDFE4653285}']
  {Methods}
  function init: JJSONArray; cdecl; overload;
  function init(copyFrom: JCollection): JJSONArray; cdecl; overload;
  function init(readFrom: JJSONTokener): JJSONArray; cdecl; overload;
  function init(json: JString): JJSONArray; cdecl; overload;
end;

[JavaSignature('org/json/JSONArray')]
JJSONArray = interface(JObject)
['{34738D80-ED10-413D-9467-36A3785DBFF4}']
  {Methods}
  function equals(o: JObject): Boolean; cdecl;
  function &get(index: Integer): JObject; cdecl;
  function getBoolean(index: Integer): Boolean; cdecl;
  function getDouble(index: Integer): Double; cdecl;
  function getInt(index: Integer): Integer; cdecl;
  function getJSONArray(index: Integer): JJSONArray; cdecl;
  function getJSONObject(index: Integer): JJSONObject; cdecl;
  function getLong(index: Integer): Int64; cdecl;
  function getString(index: Integer): JString; cdecl;
  function hashCode: Integer; cdecl;
  function isNull(index: Integer): Boolean; cdecl;
  function join(separator: JString): JString; cdecl;
  function length: Integer; cdecl;
  function opt(index: Integer): JObject; cdecl;
  function optBoolean(index: Integer): Boolean; cdecl; overload;
  function optBoolean(index: Integer; fallback: Boolean): Boolean; cdecl; overload;
  function optDouble(index: Integer): Double; cdecl; overload;
  function optDouble(index: Integer; fallback: Double): Double; cdecl; overload;
  function optInt(index: Integer): Integer; cdecl; overload;
  function optInt(index: Integer; fallback: Integer): Integer; cdecl; overload;
  function optJSONArray(index: Integer): JJSONArray; cdecl;
  function optJSONObject(index: Integer): JJSONObject; cdecl;
  function optLong(index: Integer): Int64; cdecl; overload;
  function optLong(index: Integer; fallback: Int64): Int64; cdecl; overload;
  function optString(index: Integer): JString; cdecl; overload;
  function optString(index: Integer; fallback: JString): JString; cdecl; overload;
  function put(value: Boolean): JJSONArray; cdecl; overload;
  function put(value: Double): JJSONArray; cdecl; overload;
  function put(value: Integer): JJSONArray; cdecl; overload;
  function put(value: Int64): JJSONArray; cdecl; overload;
  function put(value: JObject): JJSONArray; cdecl; overload;
  function put(index: Integer; value: Boolean): JJSONArray; cdecl; overload;
  function put(index: Integer; value: Double): JJSONArray; cdecl; overload;
  function put(index: Integer; value: Integer): JJSONArray; cdecl; overload;
  function put(index: Integer; value: Int64): JJSONArray; cdecl; overload;
  function put(index: Integer; value: JObject): JJSONArray; cdecl; overload;
  function toJSONObject(names: JJSONArray): JJSONObject; cdecl;
  function toString: JString; cdecl; overload;
  function toString(indentSpaces: Integer): JString; cdecl; overload;
end;
TJJSONArray = class(TJavaGenericImport<JJSONArrayClass, JJSONArray>) end;

JLongClass = interface(JNumberClass)
['{BA567CF5-58F3-41A7-BAA4-538606294DE9}']
  {Property Methods}
  function _GetMAX_VALUE: Int64;
  function _GetMIN_VALUE: Int64;
  function _GetSIZE: Integer;
  function _GetTYPE: Jlang_Class;
  {Methods}
  function init(value: Int64): JLong; cdecl; overload;
  function init(string_: JString): JLong; cdecl; overload;
  function bitCount(v: Int64): Integer; cdecl;
  function decode(string_: JString): JLong; cdecl;
  function getLong(string_: JString): JLong; cdecl; overload;
  function getLong(string_: JString; defaultValue: Int64): JLong; cdecl; overload;
  function getLong(string_: JString; defaultValue: JLong): JLong; cdecl; overload;
  function highestOneBit(v: Int64): Int64; cdecl;
  function lowestOneBit(v: Int64): Int64; cdecl;
  function numberOfLeadingZeros(v: Int64): Integer; cdecl;
  function numberOfTrailingZeros(v: Int64): Integer; cdecl;
  function parseLong(string_: JString): Int64; cdecl; overload;
  function parseLong(string_: JString; radix: Integer): Int64; cdecl; overload;
  function reverse(v: Int64): Int64; cdecl;
  function reverseBytes(v: Int64): Int64; cdecl;
  function rotateLeft(v: Int64; distance: Integer): Int64; cdecl;
  function rotateRight(v: Int64; distance: Integer): Int64; cdecl;
  function signum(v: Int64): Integer; cdecl;
  function toBinaryString(v: Int64): JString; cdecl;
  function toHexString(v: Int64): JString; cdecl;
  function toOctalString(v: Int64): JString; cdecl;
  function toString(n: Int64): JString; cdecl; overload;
  function toString(v: Int64; radix: Integer): JString; cdecl; overload;
  function valueOf(string_: JString): JLong; cdecl; overload;
  function valueOf(string_: JString; radix: Integer): JLong; cdecl; overload;
  function valueOf(v: Int64): JLong; cdecl; overload;
  {Properties}
  property MAX_VALUE: Int64 read _GetMAX_VALUE;
  property MIN_VALUE: Int64 read _GetMIN_VALUE;
  property SIZE: Integer read _GetSIZE;
  property &TYPE: Jlang_Class read _GetTYPE;
end;

[JavaSignature('java/lang/Long')]
JLong = interface(JNumber)
['{F2E23531-34CC-4607-94D6-F85B4F95FB43}']
  {Methods}
  function byteValue: Byte; cdecl;
  function compareTo(object_: JLong): Integer; cdecl;
  function doubleValue: Double; cdecl;
  function equals(o: JObject): Boolean; cdecl;
  function floatValue: Single; cdecl;
  function hashCode: Integer; cdecl;
  function intValue: Integer; cdecl;
  function longValue: Int64; cdecl;
  function shortValue: SmallInt; cdecl;
  function toString: JString; cdecl; overload;
end;
TJLong = class(TJavaGenericImport<JLongClass, JLong>) end;

JFileInputStreamClass = interface(JInputStreamClass)
['{A1EB6AE5-8562-4E38-8182-61F57E51733A}']
  {Methods}
  function init(file_: JFile): JFileInputStream; cdecl; overload;
  function init(fd: JFileDescriptor): JFileInputStream; cdecl; overload;
  function init(path: JString): JFileInputStream; cdecl; overload;
end;

[JavaSignature('java/io/FileInputStream')]
JFileInputStream = interface(JInputStream)
['{55CBCA4D-B04C-442A-BD74-ADFFD715A1A5}']
  {Methods}
  function available: Integer; cdecl;
  procedure close; cdecl;
  function getFD: JFileDescriptor; cdecl;
  function read: Integer; cdecl; overload;
  function read(buffer: TJavaArray<Byte>; byteOffset: Integer; byteCount: Integer): Integer; cdecl; overload;
  function skip(byteCount: Int64): Int64; cdecl;
end;
TJFileInputStream = class(TJavaGenericImport<JFileInputStreamClass, JFileInputStream>) end;

JStringBufferClass = interface(JAbstractStringBuilderClass)
['{F6BF4ECD-EA63-4AF3-A901-99D4221796D7}']
  {Methods}
  function init: JStringBuffer; cdecl; overload;
  function init(capacity: Integer): JStringBuffer; cdecl; overload;
  function init(string_: JString): JStringBuffer; cdecl; overload;
  function init(cs: JCharSequence): JStringBuffer; cdecl; overload;
end;

[JavaSignature('java/lang/StringBuffer')]
JStringBuffer = interface(JAbstractStringBuilder)
['{3CECFBBE-9C21-4D67-9F6F-52BB1DB2C638}']
  {Methods}
  function append(b: Boolean): JStringBuffer; cdecl; overload;
  function append(ch: Char): JStringBuffer; cdecl; overload;
  function append(d: Double): JStringBuffer; cdecl; overload;
  function append(f: Single): JStringBuffer; cdecl; overload;
  function append(i: Integer): JStringBuffer; cdecl; overload;
  function append(l: Int64): JStringBuffer; cdecl; overload;
  function append(obj: JObject): JStringBuffer; cdecl; overload;
  function append(string_: JString): JStringBuffer; cdecl; overload;
  function append(sb: JStringBuffer): JStringBuffer; cdecl; overload;
  function append(chars: TJavaArray<Char>): JStringBuffer; cdecl; overload;
  function append(chars: TJavaArray<Char>; start: Integer; length: Integer): JStringBuffer; cdecl; overload;
  function append(s: JCharSequence): JStringBuffer; cdecl; overload;
  function append(s: JCharSequence; start: Integer; end_: Integer): JStringBuffer; cdecl; overload;
  function appendCodePoint(codePoint: Integer): JStringBuffer; cdecl;
  function charAt(index: Integer): Char; cdecl;
  function codePointAt(index: Integer): Integer; cdecl;
  function codePointBefore(index: Integer): Integer; cdecl;
  function codePointCount(beginIndex: Integer; endIndex: Integer): Integer; cdecl;
  function delete(start: Integer; end_: Integer): JStringBuffer; cdecl;
  function deleteCharAt(location: Integer): JStringBuffer; cdecl;
  procedure ensureCapacity(min: Integer); cdecl;
  procedure getChars(start: Integer; end_: Integer; buffer: TJavaArray<Char>; idx: Integer); cdecl;
  function indexOf(subString: JString; start: Integer): Integer; cdecl;
  function insert(index: Integer; ch: Char): JStringBuffer; cdecl; overload;
  function insert(index: Integer; b: Boolean): JStringBuffer; cdecl; overload;
  function insert(index: Integer; i: Integer): JStringBuffer; cdecl; overload;
  function insert(index: Integer; l: Int64): JStringBuffer; cdecl; overload;
  function insert(index: Integer; d: Double): JStringBuffer; cdecl; overload;
  function insert(index: Integer; f: Single): JStringBuffer; cdecl; overload;
  function insert(index: Integer; obj: JObject): JStringBuffer; cdecl; overload;
  function insert(index: Integer; string_: JString): JStringBuffer; cdecl; overload;
  function insert(index: Integer; chars: TJavaArray<Char>): JStringBuffer; cdecl; overload;
  function insert(index: Integer; chars: TJavaArray<Char>; start: Integer; length: Integer): JStringBuffer; cdecl; overload;
  function insert(index: Integer; s: JCharSequence): JStringBuffer; cdecl; overload;
  function insert(index: Integer; s: JCharSequence; start: Integer; end_: Integer): JStringBuffer; cdecl; overload;
  function lastIndexOf(subString: JString; start: Integer): Integer; cdecl;
  function offsetByCodePoints(index: Integer; codePointOffset: Integer): Integer; cdecl;
  function replace(start: Integer; end_: Integer; string_: JString): JStringBuffer; cdecl;
  function reverse: JStringBuffer; cdecl;
  procedure setCharAt(index: Integer; ch: Char); cdecl;
  procedure setLength(length: Integer); cdecl;
  function subSequence(start: Integer; end_: Integer): JCharSequence; cdecl;
  function substring(start: Integer): JString; cdecl; overload;
  function substring(start: Integer; end_: Integer): JString; cdecl; overload;
  function toString: JString; cdecl;
  procedure trimToSize; cdecl;
end;
TJStringBuffer = class(TJavaGenericImport<JStringBufferClass, JStringBuffer>) end;



function JStringToString(const JStr: JString): string;
function StringToJString(const Str: string): JString;

implementation

function JStringToString(const JStr: JString): string;
begin
  if JStr = nil then
    Result := ''
  else
    Result:= JNIStringToString(TJNIResolver.GetJNIEnv, JNIString((JStr as ILocalObject).GetObjectID));
end;

function StringToJString(const Str: string): JString;
var
  LocalRef: JNIObject;
begin
  LocalRef := StringToJNIString(TJNIResolver.GetJNIEnv, Str);
  Result := TJString.Wrap(LocalRef);
  TJNIResolver.DeleteLocalRef(LocalRef);
end;

end.

