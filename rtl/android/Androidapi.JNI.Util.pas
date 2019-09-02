{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit Androidapi.JNI.Util;

interface

uses 
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes;

type

  {Class forward declarations}
  JAndroidException = interface;//android.util.AndroidException
  JSparseBooleanArray = interface;//android.util.SparseBooleanArray
  JAttributeSet = interface;//android.util.AttributeSet
  JTypedValue = interface;//android.util.TypedValue
  JAndroidRuntimeException = interface;//android.util.AndroidRuntimeException
  JDisplayMetrics = interface;//android.util.DisplayMetrics
  JProperty = interface;//android.util.Property
  JSparseArray = interface;//android.util.SparseArray

JAndroidExceptionClass = interface(JExceptionClass)
['{316A42C1-5EB3-4252-BD9B-DF12B2FDF470}']
  {Methods}
  function init: JAndroidException; cdecl; overload;
  function init(name: JString): JAndroidException; cdecl; overload;
  function init(name: JString; cause: JThrowable): JAndroidException; cdecl; overload;
  function init(cause: JException): JAndroidException; cdecl; overload;
end;

[JavaSignature('android/util/AndroidException')]
JAndroidException = interface(JException)
['{9DD82875-9F43-483A-83BC-980F36CBCA5A}']
end;
TJAndroidException = class(TJavaGenericImport<JAndroidExceptionClass, JAndroidException>) end;

JSparseBooleanArrayClass = interface(JObjectClass)
['{D23BD7F9-76E0-4E9D-AAF4-AFE4660DEE0F}']
  {Methods}
  function init: JSparseBooleanArray; cdecl; overload;
  function init(initialCapacity: Integer): JSparseBooleanArray; cdecl; overload;
end;

[JavaSignature('android/util/SparseBooleanArray')]
JSparseBooleanArray = interface(JObject)
['{5DD9C005-5426-4BEB-A03E-CA66DE454B01}']
  {Methods}
  procedure append(key: Integer; value: Boolean); cdecl;
  procedure clear; cdecl;
  function clone: JSparseBooleanArray; cdecl;
  procedure delete(key: Integer); cdecl;
  function &get(key: Integer): Boolean; cdecl; overload;
  function &get(key: Integer; valueIfKeyNotFound: Boolean): Boolean; cdecl; overload;
  function indexOfKey(key: Integer): Integer; cdecl;
  function indexOfValue(value: Boolean): Integer; cdecl;
  function keyAt(index: Integer): Integer; cdecl;
  procedure put(key: Integer; value: Boolean); cdecl;
  function size: Integer; cdecl;
  function valueAt(index: Integer): Boolean; cdecl;
end;
TJSparseBooleanArray = class(TJavaGenericImport<JSparseBooleanArrayClass, JSparseBooleanArray>) end;

JAttributeSetClass = interface(IJavaClass)
['{38053FDE-577B-41AB-9D3F-0572B876D9A1}']
end;

[JavaSignature('android/util/AttributeSet')]
JAttributeSet = interface(IJavaInstance)
['{A68EE3B4-0C0D-4B03-A0A0-163C13A234CC}']
  {Methods}
  function getAttributeBooleanValue(namespace: JString; attribute: JString; defaultValue: Boolean): Boolean; cdecl; overload;
  function getAttributeBooleanValue(index: Integer; defaultValue: Boolean): Boolean; cdecl; overload;
  function getAttributeCount: Integer; cdecl;
  function getAttributeFloatValue(namespace: JString; attribute: JString; defaultValue: Single): Single; cdecl; overload;
  function getAttributeFloatValue(index: Integer; defaultValue: Single): Single; cdecl; overload;
  function getAttributeIntValue(namespace: JString; attribute: JString; defaultValue: Integer): Integer; cdecl; overload;
  function getAttributeIntValue(index: Integer; defaultValue: Integer): Integer; cdecl; overload;
  function getAttributeListValue(namespace: JString; attribute: JString; options: TJavaObjectArray<JString>; defaultValue: Integer): Integer; cdecl; overload;
  function getAttributeListValue(index: Integer; options: TJavaObjectArray<JString>; defaultValue: Integer): Integer; cdecl; overload;
  function getAttributeName(index: Integer): JString; cdecl;
  function getAttributeNameResource(index: Integer): Integer; cdecl;
  function getAttributeResourceValue(namespace: JString; attribute: JString; defaultValue: Integer): Integer; cdecl; overload;
  function getAttributeResourceValue(index: Integer; defaultValue: Integer): Integer; cdecl; overload;
  function getAttributeUnsignedIntValue(namespace: JString; attribute: JString; defaultValue: Integer): Integer; cdecl; overload;
  function getAttributeUnsignedIntValue(index: Integer; defaultValue: Integer): Integer; cdecl; overload;
  function getAttributeValue(index: Integer): JString; cdecl; overload;
  function getAttributeValue(namespace: JString; name: JString): JString; cdecl; overload;
  function getClassAttribute: JString; cdecl;
  function getIdAttribute: JString; cdecl;
  function getIdAttributeResourceValue(defaultValue: Integer): Integer; cdecl;
  function getPositionDescription: JString; cdecl;
  function getStyleAttribute: Integer; cdecl;
end;
TJAttributeSet = class(TJavaGenericImport<JAttributeSetClass, JAttributeSet>) end;

JTypedValueClass = interface(JObjectClass)
['{772C1CBF-A712-432D-9AAE-54E62D0DC745}']
  {Property Methods}
  function _GetCOMPLEX_MANTISSA_MASK: Integer;
  function _GetCOMPLEX_MANTISSA_SHIFT: Integer;
  function _GetCOMPLEX_RADIX_0p23: Integer;
  function _GetCOMPLEX_RADIX_16p7: Integer;
  function _GetCOMPLEX_RADIX_23p0: Integer;
  function _GetCOMPLEX_RADIX_8p15: Integer;
  function _GetCOMPLEX_RADIX_MASK: Integer;
  function _GetCOMPLEX_RADIX_SHIFT: Integer;
  function _GetCOMPLEX_UNIT_DIP: Integer;
  function _GetCOMPLEX_UNIT_FRACTION: Integer;
  function _GetCOMPLEX_UNIT_FRACTION_PARENT: Integer;
  function _GetCOMPLEX_UNIT_IN: Integer;
  function _GetCOMPLEX_UNIT_MASK: Integer;
  function _GetCOMPLEX_UNIT_MM: Integer;
  function _GetCOMPLEX_UNIT_PT: Integer;
  function _GetCOMPLEX_UNIT_PX: Integer;
  function _GetCOMPLEX_UNIT_SHIFT: Integer;
  function _GetCOMPLEX_UNIT_SP: Integer;
  function _GetDENSITY_DEFAULT: Integer;
  function _GetDENSITY_NONE: Integer;
  function _GetTYPE_ATTRIBUTE: Integer;
  function _GetTYPE_DIMENSION: Integer;
  function _GetTYPE_FIRST_COLOR_INT: Integer;
  function _GetTYPE_FIRST_INT: Integer;
  function _GetTYPE_FLOAT: Integer;
  function _GetTYPE_FRACTION: Integer;
  function _GetTYPE_INT_BOOLEAN: Integer;
  function _GetTYPE_INT_COLOR_ARGB4: Integer;
  function _GetTYPE_INT_COLOR_ARGB8: Integer;
  function _GetTYPE_INT_COLOR_RGB4: Integer;
  function _GetTYPE_INT_COLOR_RGB8: Integer;
  function _GetTYPE_INT_DEC: Integer;
  function _GetTYPE_INT_HEX: Integer;
  function _GetTYPE_LAST_COLOR_INT: Integer;
  function _GetTYPE_LAST_INT: Integer;
  function _GetTYPE_NULL: Integer;
  function _GetTYPE_REFERENCE: Integer;
  function _GetTYPE_STRING: Integer;
  {Methods}
  function init: JTypedValue; cdecl;
  function applyDimension(unit_: Integer; value: Single; metrics: JDisplayMetrics): Single; cdecl;
  function coerceToString(type_: Integer; data: Integer): JString; cdecl; overload;
  function complexToDimension(data: Integer; metrics: JDisplayMetrics): Single; cdecl;
  function complexToDimensionNoisy(data: Integer; metrics: JDisplayMetrics): Single; cdecl;
  function complexToDimensionPixelOffset(data: Integer; metrics: JDisplayMetrics): Integer; cdecl;
  function complexToDimensionPixelSize(data: Integer; metrics: JDisplayMetrics): Integer; cdecl;
  function complexToFloat(complex: Integer): Single; cdecl;
  function complexToFraction(data: Integer; base: Single; pbase: Single): Single; cdecl;
  {Properties}
  property COMPLEX_MANTISSA_MASK: Integer read _GetCOMPLEX_MANTISSA_MASK;
  property COMPLEX_MANTISSA_SHIFT: Integer read _GetCOMPLEX_MANTISSA_SHIFT;
  property COMPLEX_RADIX_0p23: Integer read _GetCOMPLEX_RADIX_0p23;
  property COMPLEX_RADIX_16p7: Integer read _GetCOMPLEX_RADIX_16p7;
  property COMPLEX_RADIX_23p0: Integer read _GetCOMPLEX_RADIX_23p0;
  property COMPLEX_RADIX_8p15: Integer read _GetCOMPLEX_RADIX_8p15;
  property COMPLEX_RADIX_MASK: Integer read _GetCOMPLEX_RADIX_MASK;
  property COMPLEX_RADIX_SHIFT: Integer read _GetCOMPLEX_RADIX_SHIFT;
  property COMPLEX_UNIT_DIP: Integer read _GetCOMPLEX_UNIT_DIP;
  property COMPLEX_UNIT_FRACTION: Integer read _GetCOMPLEX_UNIT_FRACTION;
  property COMPLEX_UNIT_FRACTION_PARENT: Integer read _GetCOMPLEX_UNIT_FRACTION_PARENT;
  property COMPLEX_UNIT_IN: Integer read _GetCOMPLEX_UNIT_IN;
  property COMPLEX_UNIT_MASK: Integer read _GetCOMPLEX_UNIT_MASK;
  property COMPLEX_UNIT_MM: Integer read _GetCOMPLEX_UNIT_MM;
  property COMPLEX_UNIT_PT: Integer read _GetCOMPLEX_UNIT_PT;
  property COMPLEX_UNIT_PX: Integer read _GetCOMPLEX_UNIT_PX;
  property COMPLEX_UNIT_SHIFT: Integer read _GetCOMPLEX_UNIT_SHIFT;
  property COMPLEX_UNIT_SP: Integer read _GetCOMPLEX_UNIT_SP;
  property DENSITY_DEFAULT: Integer read _GetDENSITY_DEFAULT;
  property DENSITY_NONE: Integer read _GetDENSITY_NONE;
  property TYPE_ATTRIBUTE: Integer read _GetTYPE_ATTRIBUTE;
  property TYPE_DIMENSION: Integer read _GetTYPE_DIMENSION;
  property TYPE_FIRST_COLOR_INT: Integer read _GetTYPE_FIRST_COLOR_INT;
  property TYPE_FIRST_INT: Integer read _GetTYPE_FIRST_INT;
  property TYPE_FLOAT: Integer read _GetTYPE_FLOAT;
  property TYPE_FRACTION: Integer read _GetTYPE_FRACTION;
  property TYPE_INT_BOOLEAN: Integer read _GetTYPE_INT_BOOLEAN;
  property TYPE_INT_COLOR_ARGB4: Integer read _GetTYPE_INT_COLOR_ARGB4;
  property TYPE_INT_COLOR_ARGB8: Integer read _GetTYPE_INT_COLOR_ARGB8;
  property TYPE_INT_COLOR_RGB4: Integer read _GetTYPE_INT_COLOR_RGB4;
  property TYPE_INT_COLOR_RGB8: Integer read _GetTYPE_INT_COLOR_RGB8;
  property TYPE_INT_DEC: Integer read _GetTYPE_INT_DEC;
  property TYPE_INT_HEX: Integer read _GetTYPE_INT_HEX;
  property TYPE_LAST_COLOR_INT: Integer read _GetTYPE_LAST_COLOR_INT;
  property TYPE_LAST_INT: Integer read _GetTYPE_LAST_INT;
  property TYPE_NULL: Integer read _GetTYPE_NULL;
  property TYPE_REFERENCE: Integer read _GetTYPE_REFERENCE;
  property TYPE_STRING: Integer read _GetTYPE_STRING;
end;

[JavaSignature('android/util/TypedValue')]
JTypedValue = interface(JObject)
['{776A314F-EFF7-401A-A02D-7FF0AA43F435}']
  {Property Methods}
  function _GetassetCookie: Integer;
  procedure _SetassetCookie(Value: Integer);
  function _GetchangingConfigurations: Integer;
  procedure _SetchangingConfigurations(Value: Integer);
  function _Getdata: Integer;
  procedure _Setdata(Value: Integer);
  function _Getdensity: Integer;
  procedure _Setdensity(Value: Integer);
  function _GetresourceId: Integer;
  procedure _SetresourceId(Value: Integer);
  function _Getstring: JCharSequence;
  procedure _Setstring(Value: JCharSequence);
  function _Gettype: Integer;
  procedure _Settype(Value: Integer);
  {Methods}
  function coerceToString: JCharSequence; cdecl; overload;
  function getDimension(metrics: JDisplayMetrics): Single; cdecl;
  function getFloat: Single; cdecl;
  function getFraction(base: Single; pbase: Single): Single; cdecl;
  procedure setTo(other: JTypedValue); cdecl;
  function toString: JString; cdecl;
  {Properties}
  property assetCookie: Integer read _GetassetCookie write _SetassetCookie;
  property changingConfigurations: Integer read _GetchangingConfigurations write _SetchangingConfigurations;
  property data: Integer read _Getdata write _Setdata;
  property density: Integer read _Getdensity write _Setdensity;
  property resourceId: Integer read _GetresourceId write _SetresourceId;
  property &string: JCharSequence read _Getstring write _Setstring;
  property &type: Integer read _Gettype write _Settype;
end;
TJTypedValue = class(TJavaGenericImport<JTypedValueClass, JTypedValue>) end;

JAndroidRuntimeExceptionClass = interface(JRuntimeExceptionClass)
['{0858B0B4-AAB9-4F0B-850F-33B575ACFE7B}']
  {Methods}
  function init: JAndroidRuntimeException; cdecl; overload;
  function init(name: JString): JAndroidRuntimeException; cdecl; overload;
  function init(name: JString; cause: JThrowable): JAndroidRuntimeException; cdecl; overload;
  function init(cause: JException): JAndroidRuntimeException; cdecl; overload;
end;

[JavaSignature('android/util/AndroidRuntimeException')]
JAndroidRuntimeException = interface(JRuntimeException)
['{83257DE1-7FFC-4373-B4B3-ED0330D090ED}']
end;
TJAndroidRuntimeException = class(TJavaGenericImport<JAndroidRuntimeExceptionClass, JAndroidRuntimeException>) end;

JDisplayMetricsClass = interface(JObjectClass)
['{5B98B374-49AD-4739-873F-C979B3F824CD}']
  {Property Methods}
  function _GetDENSITY_DEFAULT: Integer;
  function _GetDENSITY_HIGH: Integer;
  function _GetDENSITY_LOW: Integer;
  function _GetDENSITY_MEDIUM: Integer;
  function _GetDENSITY_TV: Integer;
  function _GetDENSITY_XHIGH: Integer;
  function _GetDENSITY_XXHIGH: Integer;
  {Methods}
  function init: JDisplayMetrics; cdecl;
  {Properties}
  property DENSITY_DEFAULT: Integer read _GetDENSITY_DEFAULT;
  property DENSITY_HIGH: Integer read _GetDENSITY_HIGH;
  property DENSITY_LOW: Integer read _GetDENSITY_LOW;
  property DENSITY_MEDIUM: Integer read _GetDENSITY_MEDIUM;
  property DENSITY_TV: Integer read _GetDENSITY_TV;
  property DENSITY_XHIGH: Integer read _GetDENSITY_XHIGH;
  property DENSITY_XXHIGH: Integer read _GetDENSITY_XXHIGH;
end;

[JavaSignature('android/util/DisplayMetrics')]
JDisplayMetrics = interface(JObject)
['{D3EC59F1-BB9A-4820-BBB6-47A518711902}']
  {Property Methods}
  function _Getdensity: Single;
  procedure _Setdensity(Value: Single);
  function _GetdensityDpi: Integer;
  procedure _SetdensityDpi(Value: Integer);
  function _GetheightPixels: Integer;
  procedure _SetheightPixels(Value: Integer);
  function _GetscaledDensity: Single;
  procedure _SetscaledDensity(Value: Single);
  function _GetwidthPixels: Integer;
  procedure _SetwidthPixels(Value: Integer);
  function _Getxdpi: Single;
  procedure _Setxdpi(Value: Single);
  function _Getydpi: Single;
  procedure _Setydpi(Value: Single);
  {Methods}
  function equals(o: JObject): Boolean; cdecl; overload;
  function equals(other: JDisplayMetrics): Boolean; cdecl; overload;
  function hashCode: Integer; cdecl;
  procedure setTo(o: JDisplayMetrics); cdecl;
  procedure setToDefaults; cdecl;
  function toString: JString; cdecl;
  {Properties}
  property density: Single read _Getdensity write _Setdensity;
  property densityDpi: Integer read _GetdensityDpi write _SetdensityDpi;
  property heightPixels: Integer read _GetheightPixels write _SetheightPixels;
  property scaledDensity: Single read _GetscaledDensity write _SetscaledDensity;
  property widthPixels: Integer read _GetwidthPixels write _SetwidthPixels;
  property xdpi: Single read _Getxdpi write _Setxdpi;
  property ydpi: Single read _Getydpi write _Setydpi;
end;
TJDisplayMetrics = class(TJavaGenericImport<JDisplayMetricsClass, JDisplayMetrics>) end;

JPropertyClass = interface(JObjectClass)
['{9189FC50-84D8-4091-95ED-15B3B8FACC15}']
  {Methods}
  function init(type_: Jlang_Class; name: JString): JProperty; cdecl;
  function &of(hostType: Jlang_Class; valueType: Jlang_Class; name: JString): JProperty; cdecl;
end;

[JavaSignature('android/util/Property')]
JProperty = interface(JObject)
['{2803C5A6-4347-4D46-AEAA-28C87BB457B5}']
  {Methods}
  function &get(object_: JObject): JObject; cdecl;
  function getName: JString; cdecl;
  function getType: Jlang_Class; cdecl;
  function isReadOnly: Boolean; cdecl;
  procedure &set(object_: JObject; value: JObject); cdecl;
end;
TJProperty = class(TJavaGenericImport<JPropertyClass, JProperty>) end;

JSparseArrayClass = interface(JObjectClass)
['{4E51484A-CD9E-4170-8CCF-B3C8A8CA3F26}']
  {Methods}
  function init: JSparseArray; cdecl; overload;
  function init(initialCapacity: Integer): JSparseArray; cdecl; overload;
end;

[JavaSignature('android/util/SparseArray')]
JSparseArray = interface(JObject)
['{AFB1BC39-0C23-4B91-A7E9-C03FD965EA44}']
  {Methods}
  procedure append(key: Integer; value: JObject); cdecl;
  procedure clear; cdecl;
  function clone: JSparseArray; cdecl;
  procedure delete(key: Integer); cdecl;
  function &get(key: Integer): JObject; cdecl; overload;
  function &get(key: Integer; valueIfKeyNotFound: JObject): JObject; cdecl; overload;
  function indexOfKey(key: Integer): Integer; cdecl;
  function indexOfValue(value: JObject): Integer; cdecl;
  function keyAt(index: Integer): Integer; cdecl;
  procedure put(key: Integer; value: JObject); cdecl;
  procedure remove(key: Integer); cdecl;
  procedure removeAt(index: Integer); cdecl;
  procedure setValueAt(index: Integer; value: JObject); cdecl;
  function size: Integer; cdecl;
  function valueAt(index: Integer): JObject; cdecl;
end;
TJSparseArray = class(TJavaGenericImport<JSparseArrayClass, JSparseArray>) end;




implementation

begin

end.


