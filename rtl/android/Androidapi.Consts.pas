{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2010-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit Androidapi.Consts;

interface

resourcestring
  SJNIRTTINotAvailable = 'RTTI for type %s was not found.  Perhaps missing {$M+}?';
  SJNIClassNotFound = 'Java class %s could not be found';
  SJNIType = '%s is not a valid Java type';
  SInternalBindingError = 'Internal error creating binding for ''%s''';
  SBadJNIClass = '''%s'' must be an interface derived from IJavaClass';
  SBadJNIInstance = '''%s'' must be an interface derived from IJavaInstance';
  SErrorCreatingJNIObject = 'Unable to create Java object for instance of ''%s''';
  SJNIInvokeError = 'Fatal error invoking interface';
  SInvalidJNIType = 'The type ''%s'' is not supported with jni interoperability';
  SJNINoInterface = 'No implemented interface found in ''%s''';
  SJNIMethodNotFound = 'Invoke error: method ''%s'' not found';
  SJNIInvalidSetter = 'Method ''%s'' is not a valid property setter';
  SJNIInvalidGetter = 'Method ''%s'' is not a valid property getter';

implementation

end.
