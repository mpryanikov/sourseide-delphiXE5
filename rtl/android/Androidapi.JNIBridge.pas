{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2010-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit Androidapi.JNIBridge;

interface


uses
  System.SysUtils, System.Rtti, System.TypInfo,
  System.Generics.Collections, Androidapi.Jni,
  Androidapi.JNIMarshal;

const
  IID_ILocalObject_Name = '{81DD0F8E-7E6C-4D63-BE42-EB691CAAE686}';
  IID_ILocalObject: TGUID = IID_ILocalObject_Name;

  IID_IJava_Name = '{2866B78E-2258-4DE6-A075-C37264E2C837}';
  IID_IJava: TGUID = IID_IJava_Name;
  IID_IJavaInstance_Name = '{E9063409-906F-4C8D-ABF2-E10318F8E72C}';

  IID_IJavaInstance: TGUID = IID_IJavaInstance_Name;
  IID_IJavaClass_Name = '{ABFF4754-6EC8-4B84-B541-4FC471500856}';
  IID_IJavaClass: TGUID = IID_IJavaClass_Name;

  IID_IJavaInterface_Name = '{024CE73D-3FF3-423B-B8B1-DCE9DAC254C6}';
  IID_IJavaInterface: TGUID = IID_IJavaInterface_Name;

type
{$M+}
  ///
  ///  All Java objects, whether local or imports must use this interface
  ///  as the 'root' interface.  Local objects are Delphi implemented objects
  ///  that implement some java protocol or interface.  Imports are
  ///  java objects, created by the java runtime that have been
  ///  wrapped in dynamically created interfaces for access from Delphi.
  ///
  ///  Interfaces that describe Java class methods must derive from
  ///  <code>IJavaClass</code>.  Interfaces that describe Java
  ///  instance methods must derive from <code>IJavaInstance</code>.
  IJava = interface(IInterface)
    [IID_IJava_Name]
  end;

  ///
  ///  Root interface describing Java classes.  All classes
  ///  include the <code>inir</code> method.  Custom allocators
  ///  may be defined by Java class interfaces in addition to
  ///  this method.
  IJavaClass = interface(IJava)
    [IID_IJavaClass_Name]
  end;

  ///
  ///  Root interface describing Java instance methods.
  IJavaInstance = interface(IJava)
    [IID_IJavaInstance_Name]
  end;

  ///
  ///  Root interface describing Java interfaces
  ///  Mainly used for listeners:
  ///  A class can implement a Java interface and pass
  ///  this class as listener parameter to get events
  ///  from the Java side.
  IJavaInterface = interface(IJava)
    [IID_IJavaInterface_Name]
  end;
{$M-}


  ///
  ///  Alias type to make reading this code a little easier.
  TRawVTable = TRawVirtualClass.TVTable;

  ///
  ///  Java objects are accessed via interfaces.  This vtable object
  ///  implements a Delphi vtable that will invoke the Java runtime
  ///  for method calls.  These vtables will be used to construct
  ///  <code>TJavaImport</code> objects, which are virtual classes.
  ///
  TJavaVTable = class(TRawVTable)
  private
    ///  We hold on to this because the vtable entries will be pointing
    ///  into it.
    FMethodInfoData: TArray<JNIMethodInvokeData>;

    ///  See the <code>Guids</code> property.
    FGuids: TArray<TGUID>;

  protected
    ///
    ///  When we construct the virtual class using this vtable, we need
    ///  to give a list of guids that the vtable matches.  Since we are
    ///  representing Java objects as a 'hiearchy' of interfaces,
    ///  a single vtable can safely represent a swatch of Java
    ///  interfaces.  We need to hold all those guids to allow us to
    ///  manage casting up and down the hiearchy in Delphi code
    property Guids: TArray<TGUID> read FGuids;

    ///
    ///  Creates a vtable for a Java object given the type info
    ///  of the interface.  The vtable may be for a Java instance,
    ///  or class.
    constructor Create(P: PTypeInfo; ClsID: Pointer; IsClassTable: Boolean);
  end;

  ///
  ///  Exception thrown for various failures.  Most failures are due to incorrect
  ///  type declarations, where a Delphi interface type for a Java type
  ///  does not match.  Some are pure runtime issues.  The message and context
  ///  of the exception will provide some diagnostic information.
  EJNI = class(Exception);

  ///
  ///  Fatal exception issued when something sufficiently had happens that it is
  ///  questionable about the state of the runtime.  
  EJNIFatal = class(EJNI);

  ///
  ///  Both import objects and local objects implement this interface.  We use it
  ///  as a common means of getting to key data when dealing with marshaling parameters.
  ILocalObject = interface(IJava)
    [IID_ILocalObject_Name]
    ///  Returns the Java instance ID of this object.
    function GetObjectID: Pointer;
  end;

  ///
  ///  TOCImport needs to hold relatively little, because the vast bulk of
  ///  information required to invoke methods is actually referred to by
  ///  the vtable thunks themselves.  The only thing needed from the import
  ///  instances proper are the object and class ids, the latter being for
  ///  super class references.
  TJavaImport = class(TRawVirtualClass, ILocalObject)
  protected
    FID: Pointer;
    FClsID: Pointer;
    FVTable: TJavaVTable;
  public
    ///
    ///  <param name="ID">Java ID.  This can either be an instance or a class,
    ///  depending on which we are creating.</param>
    ///  <param name="ClsID">Java Class ID.  This is only set to non-zero
    ///  if we are creating a wrapper for a super-class reference to the given
    ///  ID parameter.  The <code>ClsID</code> should be the Java class of the
    ///  parent of the given <code>ID</code>.</param>
    ///  <param name="VTable">The vtable that will be used for invoking methods.
    ///  If this is a super-class reference, then it should be the appropriate
    ///  vtable.</param>
    constructor Create(ID: Pointer; ClsID: Pointer; VTable: TJavaVTable);
    destructor Destroy; override;
    function GetObjectID: Pointer; inline;
  end;

  ///
  ///  A generic class that we use to make the declaration of Java
  ///  object factories easier. In our model, we split the class methods
  ///  and instance methods into two interfaces.  This class blends the
  ///  two interfaces into one factory that can produce instances of Java
  ///  objects, or provide a reference to a singleton instance representing
  ///  the Java class.
  TJavaGenericImport<C: IJavaClass; T: IJavaInstance> = class(TJavaImport)
  strict private
    class var FInstanceVTable: TJavaVtable;
    class var FClassVTable: TJavaVTable;
    class var FJavaClass: C;
    class var FClsID: Pointer;
    class function GetJavaClass: C; static;
    class function GetInstanceVTable(ClsID: Pointer; ObjID: Pointer): TJavaVTable;
    class function Alloc(var ObjID: Pointer): T; overload;

  public
    class function Wrap(P: Pointer): T;

    ///
    ///  Provides the Java class object for this type.  You can use the
    ///  resulting interface to call Java class methods, rather than
    ///  instance methods.
    class property JavaClass: C read GetJavaClass;

    ///
    ///  Creates an instance of the object.  This may raise an <code>EJNI</code>
    ///  exception, in the event that we were unable to create the meta-data for
    ///  the class, or if there is an error in the Java runtime.  Both are
    ///  usually due to errors in the type descriptions in the Java interface
    ///  declarations.  Do not use this for instances that you want to call some
    ///  custom constructor on.  This method will call the default 'init' constructor
    ///  on the instance, which can be bad in some cases.
    class function Create: T; overload;

    class procedure Init(O: T; P: Pointer);
    class function GetClsID: Pointer;
  end;

  TJavaLocal = class(TInterfacedObject, ILocalObject, IJava)
  private
    FObjectID: Pointer;
    FLocalRefObjectID: Pointer;
  protected
    function GetObjectID: Pointer;
    constructor Create;
  public
    destructor Destroy; override;
    function hashCode: Integer; cdecl;
    function equals(Obj: Pointer): Boolean; reintroduce; overload; cdecl;
    function toString: JNIString; reintroduce; overload; cdecl;
    function getName: JNIString;
  end;


  ///
  ///  This cache holds vtables for all Java
  ///  import types.  A vtable in here might be for an Java class, or
  ///  for an Java instance.  We create vtables on the fly, when they
  ///  are requested.
  TVTableCache = class
  strict private
    class var VTables: TDictionary<PTypeInfo, TJavaVTable>;
    class var SuperVTables: TDictionary<PTypeInfo, TJavaVTable>;

    class constructor Create;

  protected
    ///
    ///  The primary source for instance vtables.  If the vtable isn't present in
    ///  the cache, we'll create it.  May throw an exception if the type
    ///  info doesn't represent a valid Java type description.
    class function GetVTable(P: PTypeInfo; ClsID: Pointer): TJavaVTable;
  end;

  TClassLoader = class
  private
    LoadClassID: JNIMethodID;
    Instance: JNIObject;
    function CreateDefaultClassLoader: Boolean;
    function GetActivityClassLoader: JNIObject;
  public
    constructor Create;
    destructor Destroy; override;
    function LoadClass(const AClass: string): JNIClass; inline;
  end;

  TJavaValidate = class
  strict private
    class procedure ValidateTypeHierarchy(P: PTypeInfo; RequiredRoot: PTypeInfo;
                                          ErrMsg: PResStringRec); static;
  protected
    class procedure ValidateClassHierarchy(P: PTypeInfo); static;
    class procedure ValidateInstanceHierarchy(P: PTypeInfo); static;
  end;


  TJNIResolver = class
  private
    class var ClassRefCache: TDictionary<string, JNIClass>;
    { This values are cached, we use them to retrieve listener parameters }
    class var GetIntMethodID: JNIMethodID;
    class var GetCharMethodID: JNIMethodID;
    class var GetBooleanMethodID: JNIMethodID;
    class var GetLongMethodID: JNIMethodID;
    class var GetShortMethodID: JNIMethodID;
    class var GetByteMethodID: JNIMethodID;
    class var GetFloatMethodID: JNIMethodID;
    class var GetDoubleMethodID: JNIMethodID;
    class procedure CheckJGetter(JNIGetter: PJNIMethodID; const ClassSig: string; const ClassGetMethod: string; const ClassGetMethodSig: string);
    class function InternalGetJavaClassID(AClass: string): JNIClass;
  public
    class threadvar JNIEnvRes: PJNIEnv;
    class var ClassLoader: TClassLoader;
    class function GetJNIEnv: PJNIEnv;
    class function GetJavaClassID(AClass: string): JNIClass;
    class function GetJavaMethodID(AClass: JNIClass; AMethod: string; AMethodSig: string): JNIMethodID; static; inline;
    class function GetJavaStaticMethodID(AClass: JNIClass; AMethod: string; AMethodSig: string): JNIMethodID; static; inline;
    class function GetJavaStaticFieldID(AClass: JNIClass; AField: string; AFieldSig: string): JNIFieldID; static; inline;
    class function GetJavaFieldID(AClass: JNIClass; AField: string; AFieldSig: string): JNIFieldID; static; inline;
    class procedure DeleteLocalRef(AObject: JNIObject);
    class procedure DeleteGlobalRef(AObject: JNIObject);
    class procedure ExceptionCheck; static; inline;
    class function NewObject(AClass: JNIClass): JNIObject;
    class function NewGlobalRef(AJNIObject: JNIObject): JNIObject;
    class function GetObjectArrayElement(AArray: JNIObjectArray; Index: JNISize): JNIObject;
    class procedure SetObjectArrayElement(AArray: JNIObjectArray; Index: JNISize; const Val);
    class procedure SetRawObjectArrayElement(AArray: JNIObjectArray; Index: JNISize; const Val);
    class function GetRawValueFromJInteger(AJInteger: JNIObject): Integer;
    class function GetRawValueFromJBoolean(AJBoolean: JNIObject): Boolean;
    class function GetRawValueFromJCharacter(AJCharacter: JNIObject): Char;
    class function GetRawValueFromJLong(AJLong: JNIObject): Int64;
    class function GetRawValueFromJShort(AJShort: JNIObject): SmallInt;
    class function GetRawValueFromJByte(AJByte: JNIObject): ShortInt;
    class function GetRawValueFromJDouble(AJDouble: JNIObject): Double;
    class function GetRawValueFromJFloat(AJFloat: JNIObject): Single;
    class function GetArrayLength(AJNIArray: JNIArray): JNISize;
    class constructor Create;
    class destructor Destroy;
  end;

  /// To create a java class we need the class signature
  JavaSignatureAttribute = class(TCustomAttribute)
  private
    FSignature: string;
  public
    constructor Create(const S: string);
    property Signature: string read FSignature;
  end;

  TJavaBasicArray = class
  private
    Handle: JNIArray;
    procedure ProcessArray(CreateArray: Boolean; Len: Integer; AHandle: Pointer); virtual;
    function GetArrayLength: JNISize; inline;
  public
    class var Context: TRttiContext;
    class constructor Create;
    class destructor Destroy;
    function ToPointer: JNIArray; inline;
    constructor Create(AJNIArray: JNIArray); overload;
    constructor Create; overload;
    property Length: JNISize read GetArrayLength;
  end;
  PJavaBasicArray = ^TJavaBasicArray;

  TJavaArray<T> = class(TJavaBasicArray)
  type
    PJavaArrayBaseType = ^T;
    TJavaArrayReleaser = procedure(Env: PJNIEnv; AArray: JNIArray; Elems: PJavaArrayBaseType; Mode: JNIInt); cdecl;
  private
    FArrayElem: PJavaArrayBaseType;
    FArrayReleaser: TJavaArrayReleaser;
    FBaseType: TRttiType;
    FClassID: JNIClass;
    function GetItem(AIndex: Integer): T; virtual;
    procedure SetItem(AIndex: Integer; AValue: T); virtual;
    procedure ProcessArray(CreateArray: Boolean; Len: Integer; AHandle: Pointer); override;
  public
    constructor Create(Len: Integer); overload;
    constructor Create; overload;  // used in wrap functions, do not call
    destructor Destroy; override;
    property Items[AIndex: Integer]: T read GetItem write SetItem; default;
    property Data: PJavaArrayBaseType read FArrayElem;
  end;

  TJavaObjectArray<T> = class(TJavaArray<T>)
  private
    function GetItem(AIndex: Integer): T; override;
    procedure SetItem(AIndex: Integer; AValue: T); override;
  public
    function GetRawItem(AIndex: Integer): JNIObject;
    procedure SetRawItem(AIndex: Integer; AValue: JNIObject);
  end;

  TJavaBiArray<T> = class(TJavaArray<T>)
  end;

  TJavaObjectBiArray<T> = class(TJavaArray<T>)
  end;


// Do not localize this block
const
  DefaultJConstructor = '<init>';
  DefaultJConstructorName = 'init';
  DefaultJGetter = '_Get';
  DefaultJSetter = '_Set';
  DefaultJSetterLength = 4;
  DefaultJGetterLength = 4;
  ProxyInterfaceName = 'com/embarcadero/dex/lib/ProxyInterface';
  DexClassConstructorSig = '(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/ClassLoader;)V';
  DexLoadClassMethod = 'loadClass';
  DexLoadClassSig = '(Ljava/lang/String;)Ljava/lang/Class;';
  ActClassLoader = 'loadClass';
  ActClassLoaderSig = '(Ljava/lang/String;)Ljava/lang/Class;';
  DexOutputDir = 'outdex';
  CreateProxyClassMethod = 'CreateProxyClass';
  CreateProxyClassSig = '(Ljava/lang/Class;I)Ljava/lang/Object;';
  GetClassLoaderMethod = 'getClassLoader';
  GetClassLoaderSig = '()Ljava/lang/ClassLoader;';

  JIntegerSig = 'java/lang/Integer';
  JIntegerGetMethod = 'intValue';
  JIntegerGetMethodSig = '()I';
  JCharSig = 'java/lang/Character';
  JCharGetMethod = 'charValue';
  JCharGetMethodSig = '()C';
  JBooleanSig = 'java/lang/Boolean';
  JBooleanGetMethod = 'booleanValue';
  JBooleanGetMethodSig ='()Z';
  JLongSig = 'java/lang/Long';
  JLongGetMethod = 'longValue';
  JLongGetMethodSig = '()J';
  JShortSig = 'java/lang/Short';
  JShortGetMethod = 'shortValue';
  JShortGetMethodSig = '()S';
  JByteSig = 'java/lang/Byte';
  JByteGetMethod = 'byteValue';
  JByteGetMethodSig = '()B';
  JFloatSig = 'java/lang/Float';
  JFloatGetMethod = 'floatValue';
  JFloatGetMethodSig = '()F';
  JDoubleSig = 'java/lang/Double';
  JDoubleGetMethod = 'doubleValue';
  JDoubleGetMethodSig = '()D';


function dispatchToNative(Env: PJNIEnv; This: JNIObject; Method: JNIString; Args: JNIObjectArray; DelphiObject: Pointer): JNIObject; cdecl;
procedure DispatchToImport; cdecl; varargs; external 'librtlhelper.a' name 'DispatchToImport';
procedure WrapJNIReturn(ObjID: Pointer; ClsID: Pointer; ObjType: Pointer; out Obj);
function WrapJNIArray(ObjID: Pointer; ClsID: Pointer; ObjType: Pointer): TJavaBasicArray;
procedure XFormInterface(Src: Pointer; Dest: Pointer);
procedure XFormClass(Src: Pointer; Dest: Pointer);
function MangleType(AType: TRttiType): string;

implementation

uses
  AndroidApi.NativeActivity, Androidapi.Consts, Androidapi.IOUtils;

function GetClassSignature(ClassType: TRttiType): Pointer;
var
  Attrs: TArray<TCustomAttribute>;
  Sig: JavaSignatureAttribute;
begin
  Attrs := ClassType.GetAttributes;
  if Length(Attrs) > 0 then
  begin
    Sig := JavaSignatureAttribute(Attrs[0]);
    Result := TJNIResolver.InternalGetJavaClassID(Sig.Signature);
  end
  else
    Result := nil;
end;

function FindMethod(MethodName: string; ClassType: TRttiType): TRttiMethod;
var
  Methods: TArray<TRttiMethod>;
  LMethod: TRttiMethod;
begin
  Methods := ClassType.GetMethods;
  for LMethod in Methods do
    if CompareText(MethodName, LMethod.Name) = 0 then
      Exit(LMethod);

  Result := nil;
end;

function GetParamFromJSource(APar: TRttiType; AJParameter: JNIObject): TValue;
begin
  case APar.TypeKind of
    tkEnumeration:
      Result := TValue.From<Boolean>(TJNIResolver.GetRawValueFromJBoolean(AJParameter));

    tkWChar:
      Result := TValue.From<Char>(TJNIResolver.GetRawValueFromJCharacter(AJParameter));

    tkInteger:
    begin
      if APar is TRttiOrdinalType then
      begin
        case APar.AsOrdinal.OrdType of
          otSByte, otUByte:
            Result := TValue.From<Byte>(TJNIResolver.GetRawValueFromJByte(AJParameter));
          otSWord, otUWord:
            Result := TValue.From<Word>(TJNIResolver.GetRawValueFromJShort(AJParameter));
          otSLong, otULong:
            Result := TValue.From<Integer>(TJNIResolver.GetRawValueFromJInteger(AJParameter));
        end
      end
      else
        Result := TValue.From<Integer>(TJNIResolver.GetRawValueFromJInteger(AJParameter));
    end;

    tkInt64:
      Result := TValue.From<Int64>(TJNIResolver.GetRawValueFromJLong(AJParameter));

    tkFloat:
    begin
      if APar is TRttiFloatType then
        case TRttiFloatType(APar).FloatType of
          ftSingle:
            Result := TValue.From<Single>(TJNIResolver.GetRawValueFromJFloat(AJParameter));
          ftDouble:
            Result := TValue.From<Double>(TJNIResolver.GetRawValueFromJDouble(AJParameter));
        end;
    end;

    tkClass:
      Result := WrapJNIArray(AJParameter, nil, APar.Handle);

    else
      Result := TValue.From<Pointer>(AJParameter);
  end;
end;

function GetJReturnFromValue(RetValue: TValue; Method: TRttiMethod): JNIObject;
var
  AClass: JNIClass;
  AObject: JNIObject;
  ConstructorID: JNIMethodID;
  GlobalRef: JNIObject;
begin
  case RetValue.Kind of
    tkInteger:
    begin
      AClass := TJNIResolver.InternalGetJavaClassID(JIntegerSig);
      if AClass = nil then
        raise EJNIFatal.CreateResFmt(@SJNIClassNotFound, [JIntegerSig]);

      ConstructorID := TJNIResolver.GetJavaMethodID(AClass, '<init>', '(I)V');
      AObject := TJNIResolver.GetJNIEnv^.NewObjectA(TJNIResolver.GetJNIEnv, AClass, ConstructorID, PJNIValue(ArgsToJNIValues(
        [RetValue.AsInteger])));
      GlobalRef := TJNIResolver.NewGlobalRef(AObject);
      TJNIResolver.DeleteLocalRef(AObject);
      Result := GlobalRef;
    end;

    tkEnumeration:
    begin
      AClass := TJNIResolver.InternalGetJavaClassID(JBooleanSig);
      if AClass = nil then
        raise EJNIFatal.CreateResFmt(@SJNIClassNotFound, [JBooleanSig]);

      ConstructorID := TJNIResolver.GetJavaMethodID(AClass, '<init>', '(Z)V');
      AObject := TJNIResolver.GetJNIEnv^.NewObjectA(TJNIResolver.GetJNIEnv, AClass, ConstructorID, PJNIValue(ArgsToJNIValues(
        [RetValue.AsBoolean])));
      GlobalRef := TJNIResolver.NewGlobalRef(AObject);
      TJNIResolver.DeleteLocalRef(AObject);
      Result := GlobalRef;
    end

    else
      Result := nil;
  end;
end;

function dispatchToNative(Env: PJNIEnv; This: JNIObject; Method: JNIString; Args: JNIObjectArray; DelphiObject: Pointer): JNIObject; cdecl;
var
  AObject: TJavaLocal;
  AContext: TRttiContext;
  MethodName: string;
  AType: TRttiType;
  LMethod: TRttiMethod;
  NewArgs: TArray<TValue>;
  I: Integer;
  APar: TRttiParameter;
  Params: TArray<TRttiParameter>;
  Wrapped: IInterface;
  SourcePar: JNIObject;
  JNIEnv: PJNIEnv;
  ClsID: JNIClass;
  RetValue: TValue;
begin
  Result := nil;
  AObject := TJavaLocal(DelphiObject);
  JNIEnv := TJNIResolver.GetJNIEnv;
  MethodName := JNIStringToString(JNIEnv, Method);

  AContext := TRttiContext.Create;
  try
    AType := AContext.GetType(AObject.ClassInfo);
    if assigned(AType) then
    begin
      LMethod := FindMethod(MethodName, AType);
      if assigned(LMethod) then
      begin
        Params := LMethod.GetParameters;
        SetLength(NewArgs, Length(Params));
        for I := 0 to Length(Params) - 1 do
        begin
          APar := Params[I];
          SourcePar := JNIEnv^.GetObjectArrayElement(JNIEnv, Args, I);
          { if the paremeter is an interface, wrap it! }
          if (APar.ParamType.TypeKind = tkInterface) then
          begin
            ClsID := GetClassSignature(APar.ParamType);
            WrapJNIReturn(Pointer(SourcePar), ClsID, APar.ParamType.Handle, Wrapped);
            TValue.Make(@Wrapped, APar.ParamType.Handle, NewArgs[I]);
          end
          else
            NewArgs[I] := GetParamFromJSource(APar.ParamType, SourcePar);
        end;
        RetValue := LMethod.Invoke(Aobject, NewArgs);
        if assigned(LMethod.ReturnType) then
           Result := GetJReturnFromValue(RetValue, LMethod);
      end
      else
        raise EJNIFatal.CreateResFmt(@SJNIMethodNotFound, [MethodName]);
    end
    else
      raise EJNIFatal.CreateResFmt(@SJNIRTTINotAvailable, [MethodName]);

  finally
    AContext.Free;
  end;
end;

procedure XFormInterface(Src: Pointer; Dest: Pointer);
var
  Intf: IJava;
  LocalObject: ILocalObject;
begin
  if Pointer(Src^) = nil then
    PPointer(Dest)^ := nil
  else
  begin
    Intf := IJava(PPointer(Src)^);
    if Intf.QueryInterface(IID_ILocalObject, LocalObject) <> 0 then
      raise EJNIFatal.CreateRes(@SJNIInvokeError);
    PPointer(Dest)^ := LocalObject.GetObjectID;
  end;
end;

procedure XFormClass(Src: Pointer; Dest: Pointer);
var
  Arr: TJavaBasicArray;
begin
  if Pointer(Src^) = nil then
    PPointer(Dest)^ := nil
  else
  begin
    Arr := TJavaBasicArray(PPointer(Src)^);
    PPointer(Dest)^ := Arr.ToPointer;
  end;
end;

procedure WrapJNIReturn(ObjID: Pointer; ClsID: Pointer; ObjType: Pointer; out Obj);
var
  Imp: TJavaImport;
  P: PTypeInfo;
  V: TJavaVTable;
begin
  if ObjID = nil then
    Pointer(Obj) := nil
  else
  begin
    P := ObjType;
    V := TVTableCache.GetVTable(P, ClsID);

    Imp := TJavaImport.Create(ObjID, ClsID, V);
    TJNIResolver.DeleteLocalRef(ObjID);
    if Imp.QueryInterface(GetTypeData(P)^.Guid, Obj) <> S_OK then
      raise EJNI.CreateResFmt(@SInternalBindingError, [string(PTypeInfo(ObjType)^.NameFld.ToString)]);
  end;
end;

function WrapJNIArray(ObjID: Pointer; ClsID: Pointer; ObjType: Pointer): TJavaBasicArray;
var
  RetArray: TJavaBasicArray;
  AContext: TRttiContext;
  AType: TRttiType;
  CType: TClass;
  GlobalRef: JNIArray;
begin
  if ObjID = nil then
    Result := nil
  else
  begin
    AContext := TRttiContext.Create;
    try
       GlobalRef := TJNIResolver.NewGlobalRef(ObjID);
       TJNIResolver.DeleteLocalRef(ObjID);
       AType := AContext.GetType(ObjType);
       CType := (AType as TRttiInstanceType).MetaclassType;
       RetArray := TJavaBasicArray(CType.Create);
       RetArray.ProcessArray(False, 0, GlobalRef);
       Result := RetArray;
    finally
       AContext.Free;
    end;
  end;
end;


{ JavaSignatureAttribute }

constructor JavaSignatureAttribute.Create(const S: string);
begin
  FSignature := S;
end;

{ TClassLoader }

function TClassLoader.GetActivityClassLoader: JNIObject;
var
  NativeActivity: PANativeActivity;
  JNIEnvRes: PJNIEnv;
  DclLoadclassID: JNIMethodID;
  ActivityClass: JNIClass;
begin
  JNIEnvRes :=  TJNIResolver.GetJNIEnv;
  // Get the default ClassLoader from the activity
  NativeActivity := PANativeActivity(System.DelphiActivity);
  ActivityClass := JNIEnvRes^.GetObjectClass(JNIEnvRes, NativeActivity^.clazz);
  DclLoadclassID := JNIEnvRes^.GetMethodID(JNIEnvRes, ActivityClass, GetClassLoaderMethod, GetClassLoaderSig);
  if not assigned(DclLoadClassID) then
    raise EJNIFatal.CreateResFmt(@SJNIMethodNotFound, [GetClassLoaderMethod]);
  Result := JNIEnvRes^.CallObjectMethod(JNIEnvRes,  NativeActivity^.clazz, DclLoadclassID);
  JNIEnvRes^.DeleteLocalRef(JNIEnvRes, ActivityClass);
end;

function TClassLoader.CreateDefaultClassLoader: Boolean;
var
  JNIEnvRes: PJNIEnv;
  clObject, clClass: JNIObject;
begin
  JNIEnvRes :=  TJNIResolver.GetJNIEnv;
  // Get the default ClassLoader
  clObject := GetActivityClassLoader;
  Instance := JNIEnvRes^.NewGlobalRef(JNIEnvRes, clObject);

  // Get the LoasClass method
  clClass := JNIEnvRes^.GetObjectClass(JNIEnvRes, clObject);
  LoadClassID := JNIEnvRes^.GetMethodID(JNIEnvRes, clClass, ActClassLoader, ActClassLoaderSig);

  // Delete Local Refs
  JNIEnvRes^.DeleteLocalRef(JNIEnvRes, clObject);
  JNIEnvRes^.DeleteLocalRef(JNIEnvRes, clClass);

  Result := True;
end;

constructor TClassLoader.Create;
begin
    CreateDefaultClassLoader;
end;

destructor TClassLoader.Destroy;
begin
  TJNIResolver.JNIEnvRes^.DeleteGlobalRef(TJNIResolver.JNIEnvRes, Instance);
end;

function TClassLoader.LoadClass(const AClass: string): JNIClass;
var
  AClassStr: JNIObject;
begin
  AClassStr := StringToJNIString(TJNIResolver.JNIEnvRes, AClass);
  Result := TJNIResolver.JNIEnvRes^.CallObjectMethodA(TJNIResolver.JNIEnvRes, Instance, LoadClassID,
    PJNIValue(ArgsToJNIValues([AClassStr])));
  if TJNIResolver.JNIEnvRes^.ExceptionCheck(TJNIResolver.JNIEnvRes) = 1 then
    Result := nil;
  TJNIResolver.JNIEnvRes^.DeleteLocalRef(TJNIResolver.JNIEnvRes, AClassStr);
end;

{ TJNIResolver }

class procedure TJNIResolver.CheckJGetter(JNIGetter: PJNIMethodID; const ClassSig, ClassGetMethod, ClassGetMethodSig: string);
var
  AClass: JNIClass;

begin
  if JNIGetter^ = nil then
  begin
    AClass := TJNIResolver.InternalGetJavaClassID(ClassSig);
    if AClass = nil then
      raise EJNIFatal.CreateResFmt(@SJNIClassNotFound, [ClassSig]);
    JNIGetter^ := TJNIResolver.GetJavaMethodID(AClass, ClassGetMethod, ClassGetMethodSig);
    if JNIGetter^ = nil then
      raise EJNIFatal.CreateResFmt(@SJNIMethodNotFound, [ClassSig + ':' + ClassGetMethod]);
  end;
end;

class constructor TJNIResolver.Create;
begin
  ClassLoader := nil;
  GetIntMethodID := nil;
  GetCharMethodID := nil;
  GetBooleanMethodID := nil;
  GetLongMethodID := nil;
  GetShortMethodID := nil;
  GetByteMethodID := nil;
  GetFloatMethodID := nil;
  GetDoubleMethodID := nil;
  ClassRefCache := TDictionary<string, JNIClass>.Create;
  inherited;
end;

class function TJNIResolver.GetJNIEnv: PJNIEnv;
var
  PActivity: PANativeActivity;
begin
 if JNIEnvRes = nil then
  begin
    PActivity := PANativeActivity(System.DelphiActivity);
    PActivity^.vm^.AttachCurrentThread(PActivity^.vm, @JNIEnvRes, nil);
  end;
  Result := JNIEnvRes;
end;

class function TJNIResolver.GetObjectArrayElement(AArray: JNIObjectArray; Index: JNISize): JNIObject;
begin
  GetJNIEnv;
  Result := JNIEnvRes^.GetObjectArrayElement(JNIEnvRes, AArray, Index);
end;

class function TJNIResolver.GetRawValueFromJBoolean(AJBoolean: JNIObject): Boolean;
begin
  GetJNIEnv;
  CheckJGetter(@GetBooleanMethodID, JBooleanSig, JBooleanGetMethod, JBooleanGetMethodSig);
  Result := JNIEnvRes^.CallBooleanMethod(JNIEnvRes, AJBoolean, GetBooleanMethodID).ToBoolean;
end;

class function TJNIResolver.GetRawValueFromJCharacter(AJCharacter: JNIObject): Char;
begin
  GetJNIEnv;
  CheckJGetter(@GetCharMethodID, JCharSig, JCharGetMethod, JCharGetMethodSig);
  Result := JNIEnvRes^.CallCharMethod(JNIEnvRes, AJCharacter, GetCharMethodID);
end;

class function TJNIResolver.GetRawValueFromJInteger(AJInteger: JNIObject): Integer;
begin
  GetJNIEnv;
  CheckJGetter(@GetIntMethodID, JIntegerSig, JIntegerGetMethod, JIntegerGetMethodSig);
  Result := JNIEnvRes^.CallIntMethod(JNIEnvRes, AJInteger, GetIntMethodID);
end;

class function TJNIResolver.GetRawValueFromJLong(AJLong: JNIObject): Int64;
begin
  GetJNIEnv;
  CheckJGetter(@GetLongMethodID, JLongSig, JLongGetMethod, JLongGetMethodSig);
  Result := JNIEnvRes^.CallLongMethod(JNIEnvRes, AJLong, GetLongMethodID);
end;

class function TJNIResolver.GetRawValueFromJShort(AJShort: JNIObject): SmallInt;
begin
  GetJNIEnv;
  CheckJGetter(@GetShortMethodID, JShortSig, JShortGetMethod, JShortGetMethodSig);
  Result := JNIEnvRes^.CallShortMethod(JNIEnvRes, AJShort, GetShortMethodID);
end;

class function TJNIResolver.InternalGetJavaClassID(AClass: string): JNIClass;
var
  LocalRef: JNIObject;
begin
  ClassRefCache.TryGetValue(AClass, Result);
  if Result = nil then
  begin
     LocalRef := TJNIResolver.GetJavaClassID(AClass);
     if assigned(LocalRef) then
     begin
       Result := TJNIResolver.NewGlobalRef(LocalRef);
       TJNIResolver.DeleteLocalRef(LocalRef);
       ClassRefCache.Add(AClass, Result);
     end;
  end;
end;

class function TJNIResolver.GetRawValueFromJByte(AJByte: JNIObject): ShortInt;
begin
  GetJNIEnv;
  CheckJGetter(@GetByteMethodID, JByteSig, JByteGetMethod, JByteGetMethodSig);
  Result := JNIEnvRes^.CallByteMethod(JNIEnvRes, AJByte, GetByteMethodID);
end;

class function TJNIResolver.GetRawValueFromJFloat(AJFloat: JNIObject): Single;
begin
  GetJNIEnv;
  CheckJGetter(@GetFloatMethodID, JFloatSig, JFloatGetMethod, JFloatGetMethodSig);
  Result := JNIEnvRes^.CallFloatMethod(JNIEnvRes, AJFloat, GetFloatMethodID);
end;

class function TJNIResolver.GetRawValueFromJDouble(AJDouble: JNIObject): Double;
begin
  GetJNIEnv;
  CheckJGetter(@GetDoubleMethodID, JDoubleSig, JDoubleGetMethod, JDoubleGetMethodSig);
  Result := JNIEnvRes^.CallDoubleMethod(JNIEnvRes, AJDouble, GetDoubleMethodID);
end;

class function TJNIResolver.NewGlobalRef(AJNIObject: JNIObject): JNIObject;
begin
  GetJNIEnv;
  Result := JNIEnvRes^.NewGlobalRef(JNIEnvRes, AJNIObject);
end;

class function TJNIResolver.NewObject(AClass: JNIClass): JNIObject;
var
  ConstructorID: JNIMethodID;
begin
  GetJNIEnv;
  ConstructorID := JNIEnvRes^.GetMethodID(JNIEnvRes, AClass, DefaultJConstructor, '()V');  // default constructor name & signature
  Result := JNIEnvRes^.NewObject(JNIEnvRes, AClass, ConstructorID);
end;

class procedure TJNIResolver.SetObjectArrayElement(AArray: JNIObjectArray; Index: JNISize; const Val);
var
  AJava: ILocalObject;
begin
  GetJNIEnv;
  AJava := (TInterfacedObject(Val) as ILocalObject);
  JNIEnvRes^.SetObjectArrayElement(JNIEnvRes, AArray, Index, AJava.GetObjectID);
end;

class procedure TJNIResolver.SetRawObjectArrayElement(AArray: JNIObjectArray; Index: JNISize; const Val);
begin
  GetJNIEnv;
  JNIEnvRes^.SetObjectArrayElement(JNIEnvRes, AArray, Index, Pointer(Val));
end;

class function TJNIResolver.GetArrayLength(AJNIArray: JNIArray): JNISize;
begin
  GetJNIEnv;
  Result := JNIEnvRes^.GetArrayLength(JNIEnvRes, AJNIArray);
  ExceptionCheck;
end;

class function TJNIResolver.GetJavaClassID(AClass: string): JNIClass;
var
  M: TMarshaller;
begin
  if not assigned(ClassLoader) then
    ClassLoader := TClassLoader.Create;
  GetJNIEnv;
  Result := JNIEnvRes^.FindClass(JNIEnvRes, M.AsAnsi(AClass, CP_UTF8).ToPointer);
  if Result = nil then
  begin
    ExceptionCheck;
    Result := ClassLoader.LoadClass(AClass);
    if Result = nil then
      ExceptionCheck;
  end;
end;

class function TJNIResolver.GetJavaMethodID(AClass: JNIClass; AMethod: string; AMethodSig: string): JNIMethodID;
var
  M: TMarshaller;
begin
  GetJNIEnv;
  if AMethod = DefaultJConstructorName then
    AMethod := DefaultJConstructor;
  Result := JNIEnvRes^.GetMethodID(JNIEnvRes, AClass, M.AsAnsi(AMethod, CP_UTF8).ToPointer, M.AsAnsi(AMethodSig, CP_UTF8).ToPointer);
  if Result = nil then
    ExceptionCheck;
end;

class function TJNIResolver.GetJavaStaticMethodID(AClass: JNIClass; AMethod: string; AMethodSig: string): JNIMethodID;
var
  M: TMarshaller;
begin
  GetJNIEnv;
  Result := JNIEnvRes^.GetStaticMethodID(JNIEnvRes, AClass, M.AsAnsi(AMethod, CP_UTF8).ToPointer, M.AsAnsi(AMethodSig, CP_UTF8).ToPointer);
  if Result = nil then
    ExceptionCheck;
end;

class function TJNIResolver.GetJavaStaticFieldID(AClass: JNIClass; AField: string; AFieldSig: string): JNIFieldID;
var
  M: TMarshaller;
begin
  GetJNIEnv;
  Result := JNIEnvRes^.GetStaticFieldID(JNIEnvRes, AClass, M.AsAnsi(AField, CP_UTF8).ToPointer, M.AsAnsi(AFieldSig, CP_UTF8).ToPointer);
  if Result = nil then
    ExceptionCheck;
end;

class function TJNIResolver.GetJavaFieldID(AClass: JNIClass; AField: string; AFieldSig: string): JNIFieldID;
var
  M: TMarshaller;
begin
  GetJNIEnv;
  Result := JNIEnvRes^.GetFieldID(JNIEnvRes, AClass, M.AsAnsi(AField, CP_UTF8).ToPointer, M.AsAnsi(AFieldSig, CP_UTF8).ToPointer);
  if Result = nil then
   ExceptionCheck;
end;

class procedure TJNIResolver.DeleteLocalRef(AObject: JNIObject);
begin
  GetJNIEnv;
  JNIEnvRes^.DeleteLocalRef(JNIEnvRes, AObject);
  ExceptionCheck;
end;

class procedure TJNIResolver.DeleteGlobalRef(AObject: JNIObject);
begin
  GetJNIEnv;
  JNIEnvRes^.DeleteGlobalRef(JNIEnvRes, AObject);
  ExceptionCheck;
end;

class destructor TJNIResolver.Destroy;
var
  ToDelete: JNIClass;
begin
  if ClassLoader <> nil then
    ClassLoader.Free;
  if ClassRefCache <> nil then
  begin
    for ToDelete in ClassRefCache.Values do
      TJNIResolver.DeleteGlobalRef(ToDelete);
    ClassRefCache.Free;
  end;

  inherited;
end;

class procedure TJNIResolver.ExceptionCheck;
begin
  if JNIEnvRes^.ExceptionCheck(JNIEnvRes) = 1 then
    JNIEnvRes^.ExceptionClear(JNIEnvRes);
end;

///
///  Call this whenever you go to look up RTTI for something and you couldn't
///  find it.  Note that you should only do this for cases where you reasonably
///  believe that the user passed in the right sort of type, and you just can't
///  find the RTTI for it, with the probably cause being that the user forgot to
///  include {$M+}.  Don't call this if you were hoping to find a method in
///  a class, and it wasn't there, possibly because the user forgot to put it there.
///
///  <param name="Name">Name of the type we couldn't find RTTI for</param>
procedure NoRtti(Name: string);
begin
  raise EJNI.CreateResFmt(@SJNIRTTINotAvailable, [Name]);
end;

function BuildBasicImportMarshalingInfo(const Method: TRttiMethod; var Ops: TArray<TMarshalOp>): Integer;
var
  Size, Cnt: Integer;
  Param: TRttiParameter;
begin
  Size := 0;
  Cnt := 0;
  SetLength(Ops, 20); //Max 20 params
  for Param in Method.GetParameters do
  begin
    if (Param.ParamType.TypeKind = tkInterface) then
    begin
      Ops[Cnt].Size := 4;
      Ops[Cnt].Kind := moJNIObject;
    end
    else
    begin
      if (Param.ParamType.TypeKind = tkClass) then
      begin
        Ops[Cnt].Size := 4;
        Ops[Cnt].Kind := moJNIArray;
      end
      else
      begin
        Ops[Cnt].Size := Param.ParamType.TypeSize;
        Ops[Cnt].Kind := moCopy;
      end;
    end;
    Size := Size + Ops[Cnt].Size;
    Inc(Cnt);
  end;
  SetLength(Ops, Cnt); //Adjust params
  Result := Size;
end;

function MangleGenericType(AType: TRttiType): string;
var
  BaseName: string;
  Context: TRttiContext;
  ArrType: TRttiType;
begin
  BaseName := AType.ToString;
  BaseName := BaseName.Substring(BaseName.IndexOf('<') + 1);
  BaseName := BaseName.Substring(0, BaseName.IndexOf('>'));
  Result := BaseName;

  Context := TRttiContext.Create;
  try
    ArrType := Context.FindType(BaseName);
    Result := '[' + MangleType(ArrType);
  finally
    Context.Free;
  end;
end;

function MangleType(AType: TRttiType): string;
var
  Attrs: TArray<TCustomAttribute>;
  SigAttr: JavaSignatureAttribute;
  OrdType: TRttiOrdinalType;
begin
  Result := '';

  case AType.TypeKind of
    tkEnumeration: Result := 'Z'; // Boolean type

    tkWChar: Result := 'C';

    tkInteger:
    begin
      if AType.IsOrdinal then
      begin
        OrdType := AType.AsOrdinal;
        case OrdType.OrdType of
          otSWord, otUWord: Result := 'S';
          otUByte, otSByte: Result := 'B';
          otSLong, otULong: Result := 'I';
        end
      end
      else
        Result := 'I';
    end;

    tkInt64: Result := 'J';

    tkClass:  // We use tkClass to detect array types
      Result := MangleGenericType(AType);

    tkInterface:
    begin
      Attrs := AType.GetAttributes;  // We need the class signature
      if Length(Attrs) > 0 then
      begin
        SigAttr := JavaSignatureAttribute(Attrs[0]);
        Result := 'L' + SigAttr.Signature + ';';
      end;
    end;

    tkFloat:
    begin
      case TRttiFloatType(AType).FloatType of
        ftSingle: Result := 'F';
        ftDouble: Result := 'D';
      end
    end
  end;
end;

function GetMethodSignature(const Method: TRttiMethod): string;
var
  Param: TRttiParameter;
begin
  Result := '(';
  for Param in Method.GetParameters do
    Result := Result + MangleType(Param.ParamType);
  Result := Result + ')';

  if Assigned(Method.ReturnType) and (Method.Name <> DefaultJConstructorName) then
    Result := Result + MangleType(Method.ReturnType)
  else
    Result := Result + 'V';
end;

function RttiTypeToPlain(AType: TRttiType): TRetKind;
var
  OrdType: TRttiOrdinalType;
begin
  Result := rkInt;
  case AType.TypeKind of
    tkEnumeration: Result := rkBoolean; // Boolean type

    tkWChar: Result := rkChar;

    tkInteger:
    begin
      if AType.IsOrdinal then
      begin
        OrdType := AType.AsOrdinal;
        case OrdType.OrdType of
          otUByte, otSByte: Result := rkByte;
          otSWord, otUWord: Result := rkShort;
          otSLong, otULong: Result := rkInt;
        end
      end
      else
        Result := rkInt;
    end;

    tkInt64: Result := rkLong;

    tkClass:  // We use tkClass to detect array types
      Result := rkInstance;

    tkInterface: Result := rkObject;

    tkFloat:
    begin
      case TRttiFloatType(AType).FloatType of
        ftSingle: Result := rkFloat;
        ftDouble: Result := rkDouble;
      end
    end
  end;
end;

function MethodIDFor(const Method: TRttiMethod; ClsID: Pointer; IsClassTable: Boolean): JNIMethodID;
var
  MethodSig: string;
begin
  MethodSig := GetMethodSignature(Method);
  if IsClassTable and (Method.Name <> DefaultJConstructorName) then
    Result := TJNIResolver.GetJavaStaticMethodID(ClsID, Method.Name, MethodSig)
  else
    Result := TJNIResolver.GetJavaMethodID(ClsID, Method.Name, MethodSig);
end;

function MethodIDForProperty(const Method: TRttiMethod; ClsID: Pointer; IsClassTable: Boolean; var MethodKind: TMethodKind; var RetKind: TRetKind; var JavaType: PTypeInfo; var RetClsID: Pointer): Pointer;
var
  Params: TArray<TRttiParameter>;
  PropType: TRttiType;
begin
  if Method.MethodKind = mkProcedure then // Setter
  begin
    if IsClassTable then
      MethodKind := mkClassPropertySetMethod
    else
      MethodKind := mkPropertySetMethod;
    JavaType := nil;
    Params := Method.GetParameters;
    if Length(Params) = 1 then
      PropType := Params[0].ParamType
    else
      raise EJNIFatal.CreateResFmt(@SJNIInvalidSetter, [Method.QualifiedClassName]);
  end
  else
  begin // Getter
    if IsClassTable then
      MethodKind := mkClassPropertyGetMethod
    else
      MethodKind := mkPropertyGetMethod;
    if assigned(Method.ReturnType) then
    begin
      PropType := Method.ReturnType;
      JavaType := PropType.Handle;
      RetClsID := GetClassSignature(PropType);
    end
    else
    raise EJNIFatal.CreateResFmt(@SJNIInvalidGetter, [Method.QualifiedClassName]);
  end;
  RetKind := RttiTypeToPlain(PropType);
  if IsClassTable then
    Result := TJNIResolver.GetJavaStaticFieldID(ClsID, Method.Name.Substring(DefaultJGetterLength), MangleType(PropType))
  else
    Result := TJNIResolver.GetJavaFieldID(ClsID, Method.Name.Substring(DefaultJGetterLength), MangleType(PropType));
end;

function IsSetterOrGetter(const Method: TRttiMethod): Boolean;
var
  Prefix: string;
begin
  Prefix := Method.Name.SubString(0, DefaultJSetter.Length);
  if (Prefix.CompareText(Prefix, DefaultJSetter) = 0) or
    (Prefix.CompareText(Prefix, DefaultJGetter) = 0) then
    Result := True
  else
    Result := False;
end;

function BuildJNIMethodInvokeData(const Method: TRttiMethod; ClsID: Pointer; IsClassTable: Boolean): JNIMethodInvokeData;
begin
  if IsSetterOrGetter(Method) then
    Result.MethodID := MethodIDForProperty(Method, ClsID, ISClassTable, Result.MethodType, Result.RetKind, Result.JavaType, Result.RetClsID)
  else
  begin
    Result.MethodID := MethodIDFor(Method, ClsID, IsClassTable);
    if IsClassTable then
      Result.MethodType := mkClassMethod
    else
      Result.MethodType := mkObjectMethod;

    if assigned(Method.ReturnType) then
    begin
      Result.RetKind := RttiTypeToPlain(Method.ReturnType);
      if (Method.ReturnType.TypeKind = tkInterface) or (Method.ReturnType.TypeKind = tkClass) then
      begin
        Result.JavaType := Method.ReturnType.Handle;
        Result.RetClsID := GetClassSignature(Method.ReturnType);
      end
      else
        Result.JavaType := nil;
      if IsClassTable and (Method.Name = DefaultJConstructorName) then
        Result.RetKind := rkConstructor;
    end
    else
      Result.RetKind := rkVoid;
  end;
  BuildBasicImportMarshalingInfo(Method, Result.Params);
  Result.Size := Length(Result.Params) * 8;
  if Result.Size = 0 then
    Result.Size := 8;
  Result.Instance := ClsID;
end;

procedure GetMethodsInVTableOrder(const RttiType: TRttiType; Methods:TList<TRttiMethod>); overload;
var
  BaseType: TRttiType;
  Method: TRttiMethod;
begin
  BaseType := RttiType.BaseType;
  if Assigned(BaseType) then
    GetMethodsInVTableOrder(BaseType, Methods);
  for Method in RttiType.GetDeclaredMethods do
    Methods.Add(Method);
end;

function GetMethodsInVTableOrder(const RttiType: TRttiType): TList<TRttiMethod>; overload;
begin
  Result := TList<TRttiMethod>.Create;
  GetMethodsInVTableOrder(RttiType, Result);
end;

function BuildInvokeData(P: PTypeInfo; ClsID: Pointer; IsClassTable: Boolean): TArray<JNIMethodInvokeData>;
var
  Method: TRttiMethod;
  RttiType: TRttiType;
  Context: TRttiContext;
  List: TList<JNIMethodInvokeData>;
  Methods: TList<TRttiMethod>;
begin
  List := TList<JNIMethodInvokeData>.Create;
  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(P);
    if not Assigned(RttiType) then
      NoRtti(string(P^.NameFld.ToString));

    Methods := GetMethodsInVTableOrder(RttiType);
    try
      for Method in Methods do
        List.Add(BuildJNIMethodInvokeData(Method, ClsID, IsClassTable));

      Result := List.ToArray;
    finally
      List.Free;
    end;

  finally
    Methods.Free;
    Context.Free;
  end;
end;

constructor TJavaVTable.Create(P: PTypeInfo; ClsID: Pointer; IsClassTable: Boolean);
var
  I: Integer;
  GuidCount: Integer;
  Parent: PTypeInfo;
  function GetParent(P: PTypeInfo): PTypeInfo;
  var
    PP: PPTypeInfo;
  begin
    PP := GetTypeData(P).IntfParent;
    if Assigned(PP) then
      Result := PP^
    else
      Result := nil;
  end;
begin
  FMethodInfoData := BuildInvokeData(P, ClsID, IsClassTable);
  inherited Create(Length(FMethodInfoData));
  Parent := GetParent(P);
  GuidCount := 1;
  while Parent <> nil do
  begin
    Inc(GuidCount);
    Parent := GetParent(Parent);
  end;

  SetLength(FGuids, GuidCount);

  for I := 0 to GuidCount - 1 do
  begin
    FGuids[I] := GetTypeData(P)^.Guid;
    P := GetParent(P);
  end;

  for I := 0 to Length(FMethodInfoData) - 1 do
      SetVTableSlot(I, @DispatchToImport, @FMethodInfoData[I]);
end;

{ TJavaImport }

function TJavaImport.GetObjectID: Pointer;
begin
  Result := FID;
end;

constructor TJavaImport.Create(ID: Pointer; ClsID: Pointer; VTable: TJavaVTable);
var
  GlobalRef: JNIObject;
begin
  inherited Create(VTable.Guids, VTable);
  if ID <> nil then
  begin
    GlobalRef := TJNIResolver.NewGlobalRef(ID);
    FID := GlobalRef;
  end
  else FID := nil;
  FClsID := ClsID;
end;

destructor TJavaImport.Destroy;
begin
  TJNIResolver.DeleteGlobalRef(GetObjectID);
end;

{ TJavaGenericImport }

class function TJavaGenericImport<C,T>.GetJavaClass: C;
var
  ClassImport: TJavaImport;
  ITypeInfo: PTypeInfo;
  CTypeInfo: PTypeInfo;
  M: TMarshaller;
  Context: TRttiContext;
  RttiType: TRttiType;
  Attrs: TArray<TCustomAttribute>;
  SigAttr: JavaSignatureAttribute;
begin
  if not Assigned(FJavaClass) then
  begin
    TJavaValidate.ValidateClassHierarchy(TypeInfo(C));
    TJavaValidate.ValidateInstanceHierarchy(TypeInfo(T));
    ITypeInfo := TypeInfo(T);
    CTypeInfo := TypeInfo(C);
    Context := TRttiContext.Create;
    try
      RttiType := Context.GetType(ITypeInfo);
      Attrs := RttiType.GetAttributes;  // We need the class signature
      if Length(Attrs) > 0 then
      begin
        SigAttr := JavaSignatureAttribute(Attrs[0]);
        FClsID := TJNIResolver.InternalGetJavaClassID(SigAttr.Signature);
        if not Assigned(FClsId) then
          raise EJNI.CreateResFmt(@SJNIClassNotFound, [string(ITypeInfo^.NameFld.ToString)]);
        FClassVTable := TJavaVTable.Create(CTypeInfo, FClsID, True);
        //ClassImport := TJavaImport.Create(FClsId, nil, FClassVTable);
        ClassImport := TJavaImport.Create(nil, FClsId, FClassVTable);
        if ClassImport.QueryInterface(GetTypeData(CTypeInfo)^.Guid, FJavaClass) <> 0 then
          raise EJNI.CreateResFmt(@SInternalBindingError, [string(ITypeInfo^.NameFld.ToString)]);
      end;
    finally
      Context.Free;
    end;
  end;
  Result := FJavaClass;
end;

class function TJavaGenericImport<C, T>.GetClsID: Pointer;
var
  LTypeInfo: PTypeInfo;
  Context: TRttiContext;
  ClassType: TRttiType;
  Attrs: TArray<TCustomAttribute>;
  SigAttr: JavaSignatureAttribute;
begin
  if FClsID = nil then
  begin
    LTypeInfo := Typeinfo(T);
    Context := TRttiContext.Create;
    try
      ClassType := Context.GetType(LTypeInfo);
      Attrs := ClassType.GetAttributes;  // We need the class signature
      if Length(Attrs) > 0 then
      begin
        SigAttr := JavaSignatureAttribute(Attrs[0]);
        FClsID := TJNIResolver.InternalGetJavaClassID(SigAttr.Signature);
      end;
    finally
      Context.Free;
    end;
  end;
  Result := FClsID;
end;

class function TJavaGenericImport<C,T>.GetInstanceVTable(ClsID: Pointer; ObjID: Pointer): TJavaVTable;
begin
  if not Assigned(FInstanceVTable) then
    FInstanceVTable := TVTableCache.GetVTable(TypeInfo(T), ClsID);
  Result := FInstanceVTable;
end;

class function TJavaGenericImport<C,T>.Alloc(var ObjID: Pointer): T;
var
  Cls: C;
  Obj: TJavaImport;
  ITypeInfo: PTypeInfo;
  ClsID: Pointer;
begin
  ITypeInfo := TypeInfo(T);
  //Cls := GetJavaClass;
  ClsID := GetClsID;
  ObjId := nil;
  try
    ObjID := TJNIResolver.NewObject(ClsID);
    HandleJNIException(TJNIResolver.GetJNIEnv);
    Obj := TJavaImport.Create(ObjID, nil, GetInstanceVTable(ClsID, ObjID));
  finally
    if ObjId <> nil then
      TJNIResolver.DeleteLocalRef(ObjID);
  end;

  ObjID := Obj.FID;
  if Obj.QueryInterface(GetTypeData(ITypeInfo)^.Guid, Result) <> 0 then
    raise EJNI.CreateResFmt(@SInternalBindingError, [string(ITypeInfo^.NameFld.ToString)]);
end;

class function TJavaGenericImport<C,T>.Wrap(P: Pointer): T;
var
  ObjID: Pointer;
  Obj: TJavaImport;
  ITypeInfo: PTypeInfo;
  ClsID: Pointer;
begin
  ITypeInfo := TypeInfo(T);
  ClsID := GetClsID;
  Obj := TJavaImport.Create(P, nil, GetInstanceVTable(ClsID, P));
  if Obj.QueryInterface(GetTypeData(ITypeInfo)^.Guid, Result) <> 0 then
    raise EJNI.CreateResFmt(@SInternalBindingError, [string(ITypeInfo^.NameFld.ToString)]);
end;

class function TJavaGenericImport<C,T>.Create: T;
var
  ObjID: Pointer;
begin
  Result := Alloc(ObjID);
  Init(Result, ObjID);
end;

class procedure TJavaGenericImport<C,T>.Init(O: T; P: Pointer);
var
  Imp: TJavaImport;
begin
  Imp := TJavaImport(TRawVirtualClass.GetInstanceFromInterface(PPointer(@O)^));
  Imp.FID := P;
end;

{ TVTableCache }

class constructor TVTableCache.Create;
begin
  VTables := TDictionary<PTypeInfo, TJavaVTable>.Create;
  SuperVTables := TDictionary<PTypeInfo, TJavaVTable>.Create;
end;

class function TVTableCache.GetVTable(P: PTypeInfo; ClsID: Pointer): TJavaVTable;
begin
  TMonitor.Enter(VTables);
  try
    if not VTables.ContainsKey(P) then
    begin
      Result := TJavaVTable.Create(P, ClsID, False);
      VTables.Add(P, Result);
    end
    else
      Result := VTables.Items[P];
  finally
    TMonitor.Exit(VTables);
  end;
end;

{ TJavaArray<T> }

procedure TJavaArray<T>.ProcessArray(CreateArray: Boolean; Len: Integer; AHandle: Pointer);
var
  LInfo: PTypeInfo;
  AType: TRttiType;
  JNIEnv: PJNIEnv;
  Attrs: TArray<TCustomAttribute>;
  SigAttr: JavaSignatureAttribute;
  IntType: TRttiType;
  IsCopy: Boolean;
  LocalRef: JNIArray;
begin
  LInfo := TypeInfo(T);
  AType := Context.GetType(LInfo);
  JNIEnv := TJNIResolver.GetJNIEnv;
  FArrayReleaser := nil;
  FArrayElem := nil;
  FBaseType := AType;
  case AType.TypeKind of
    tkEnumeration:
    begin
      if CreateArray then
        Handle := JNIEnv^.NewBooleanArray(JNIEnv, Len)
      else
        Handle := AHandle;
      FArrayElem := Pointer(JNIEnv^.GetBooleanArrayElements(JNIEnv, Handle, @IsCopy));
      FArrayReleaser := @JNIEnv^.ReleaseBooleanArrayElements;
    end;

    tkWChar:
    begin
      if CreateArray then
        Handle := JNIEnv^.NewCharArray(JNIEnv, Len)
      else Handle := AHandle;
      FArrayElem := Pointer(JNIEnv^.GetCharArrayElements(JNIEnv, Handle, @IsCopy));
      FArrayReleaser := @JNIEnv^.ReleaseCharArrayElements;
    end;

    tkInteger:
    begin
      if AType.IsOrdinal then
      begin
        case AType.AsOrdinal.OrdType of
          otSByte, otUByte:
          begin
            if CreateArray then
              Handle := JNIEnv^.NewByteArray(JNIEnv, Len)
            else
              Handle := AHandle;
            FArrayElem := Pointer(JNIEnv^.GetByteArrayElements(JNIEnv, Handle, @IsCopy));
            FArrayReleaser := @JNIEnv^.ReleaseByteArrayElements;
          end;
          otSWord, otUWord:
          begin
            if CreateArray then
              Handle := JNIEnv^.NewShortArray(JNIEnv, Len)
            else
              Handle := AHandle;
            FArrayElem := Pointer(JNIEnv^.GetShortArrayElements(JNIEnv, Handle, @IsCopy));
            FArrayReleaser := @JNIEnv^.ReleaseShortArrayElements;
          end;
          otSLong, otULong:
          begin
            if CreateArray then
              Handle := JNIEnv^.NewIntArray(JNIEnv, Len)
            else
              Handle := AHandle;
            FArrayElem := Pointer(JNIEnv^.GetIntArrayElements(JNIEnv, Handle, @IsCopy));
            FArrayReleaser := @JNIEnv^.ReleaseIntArrayElements;
          end;
        end
      end
      else
      begin
        if CreateArray then
          Handle := JNIEnv^.NewIntArray(JNIEnv, Len)
        else
          Handle := AHandle;
        FArrayElem := Pointer(JNIEnv^.GetIntArrayElements(JNIEnv, Handle, @IsCopy));
        FArrayReleaser := @JNIEnv^.ReleaseIntArrayElements;
      end;
    end;

    tkInt64:
    begin
      if CreateArray then
        Handle := JNIEnv^.NewLongArray(JNIEnv, Len)
      else
        Handle := AHandle;
      FArrayElem := Pointer(JNIEnv^.GetLongArrayElements(JNIEnv, Handle, @IsCopy));
      FArrayReleaser := @JNIEnv^.ReleaseLongArrayElements;
    end;

    tkInterface:
    begin
      if CreateArray then begin
        IntType := Context.FindType(AType.Name);
        Attrs := AType.GetAttributes;  // We need the class signature
        if System.Length(Attrs) > 0 then
        begin
          SigAttr := JavaSignatureAttribute(Attrs[0]);
          FClassID := TJNIResolver.InternalGetJavaClassID(SigAttr.Signature);
          Handle := JNIEnv^.NewObjectArray(JNIEnv, Len, FClassID, nil);
        end;
      end
      else
        Handle := AHandle;
    end;

    tkFloat:
    begin
      case TRttiFloatType(AType).FloatType of
        ftSingle:
        begin
          if CreateArray then
            Handle := JNIEnv^.NewFloatArray(JNIEnv, Len)
          else
            Handle := AHandle;
          FArrayElem := Pointer(JNIEnv^.GetFloatArrayElements(JNIEnv, Handle, @IsCopy));
          FArrayReleaser := @JNIEnv^.ReleaseFloatArrayElements;
        end;

        ftDouble:
        begin
          if CreateArray then
            Handle := JNIEnv^.NewDoubleArray(JNIEnv, Len)
          else
            Handle := AHandle;
          FArrayElem := Pointer(JNIEnv^.GetDoubleArrayElements(JNIEnv, Handle, @IsCopy));
          FArrayReleaser := @JNIEnv^.ReleaseDoubleArrayElements;
        end;
      end;
    end
  end;
  if CreateArray then
  begin
    LocalRef := Handle;
    Handle := TJNIResolver.NewGlobalRef(LocalRef);
    TJNIResolver.DeleteLocalRef(LocalRef);
  end;
end;

constructor TJavaArray<T>.Create(Len: Integer);
begin
  inherited Create;

  ProcessArray(True, Len, nil);
end;

constructor TJavaArray<T>.Create;
begin
  inherited;
end;

destructor TJavaArray<T>.destroy;
begin
  if assigned(FArrayReleaser) then
    FArrayReleaser(TJNIResolver.GetJNIEnv, Handle, FArrayElem, 0);
  TJNIResolver.DeleteGlobalRef(Handle);
  inherited;
end;

function TJavaArray<T>.GetItem(AIndex: Integer): T;
var
  AElem: PJavaArrayBaseType;
  JNIEnv: PJNIEnv;
  NewElem: Pointer;
begin
  AElem := FArrayElem;
  Inc(AElem, AIndex);
  Result := AElem^;
end;

procedure TJavaArray<T>.SetItem(AIndex: Integer; AValue: T);
var
  AElem: PJavaArrayBaseType;
  JNIEnv: PJNIEnv;
begin
  AElem := FArrayElem;
  Inc(AElem, AIndex);
  AElem^ := AValue;
end;

{ TJavaBasicArray }

constructor TJavaBasicArray.create(AJNIArray: JNIArray);
begin
  Inherited Create;

  Handle := AJNIArray;
end;

class constructor TJavaBasicArray.create;
begin
  Context := TRttiContext.Create;
end;

constructor TJavaBasicArray.Create;
begin
  Inherited;
end;

class destructor TJavaBasicArray.destroy;
begin
  Context.Free;
  Inherited;
end;

function TJavaBasicArray.GetArrayLength: JNISize;
begin
  Result := TJNIResolver.GetArrayLength(Handle);
end;

procedure TJavaBasicArray.ProcessArray(CreateArray: Boolean; Len: Integer; AHandle: Pointer);
begin
  // Override
end;

function TJavaBasicArray.ToPointer: JNIArray;
begin
  Result := Handle;
end;

{ TJavaObjectArray<T> }

function TJavaObjectArray<T>.GetItem(AIndex: Integer): T;
var
  AObject: JNIObject;
begin
  AObject := TJNIResolver.GetObjectArrayElement(Handle, AIndex);
  WrapJNIReturn(AObject, FClassID, FBaseType.Handle, Result);
end;

function TJavaObjectArray<T>.GetRawItem(AIndex: Integer): JNIObject;
begin
  Result := TJNIResolver.GetObjectArrayElement(Handle, AIndex);
end;

procedure TJavaObjectArray<T>.SetItem(AIndex: Integer; AValue: T);
begin
  TJNIResolver.SetObjectArrayElement(Handle, AIndex, AValue);
end;

procedure TJavaObjectArray<T>.SetRawItem(AIndex: Integer; AValue: JNIObject);
begin
  TJNIResolver.SetRawObjectArrayElement(Handle, AIndex, AValue);
end;

procedure RegisterdispatchToNative(Cls: JNIClass);
var
  Exp: JNINativeMethod;
begin
  Exp.Name := 'dispatchToNative';
  Exp.Signature := '(Ljava/lang/String;[Ljava/lang/Object;I)Ljava/lang/Object;';
  Exp.FnPtr := @Androidapi.JNIBridge.dispatchToNative;

  TJNIResolver.GetJNIEnv^.RegisterNatives(TJNIResolver.GetJNIEnv, Cls, PJNINativeMethod(@Exp), 1);
end;

{ TJavaLocal }

constructor TJavaLocal.Create;
var
  AContext: TRttiContext;
  AType: TRttiType;
  AInstance: TRttiInstanceType;
  Interfaces: TArray<TRttiInterfaceType>;
  AInterface: TRttiInterfaceType;
  ClsID: JNIClass;
  ProxyClass: JNIClass;
  ProxyConstructor: JNIMethodID;
  CreateProxyClass: JNIMethodID;
  AJNIObject: JNIObject;
  AJNIEnv: PJNIEnv;
begin
  AContext := TRttiContext.Create;
  try
    AType := AContext.GetType(Self.ClassInfo);
    AInstance := AType.AsInstance;
    Interfaces := AInstance.GetDeclaredImplementedInterfaces;
    if Length(Interfaces) > 0 then
    begin
      // Need first declared interface
      AInterface := Interfaces[0];
      ClsID := GetClassSignature(AInterface);
      ProxyClass := TJNIResolver.InternalGetJavaClassID(ProxyInterfaceName);
      if not assigned(ProxyClass) then
        raise EJNIFatal.CreateResFmt(@SJNIClassNotFound, [ProxyInterfaceName]);
      RegisterdispatchToNative(ProxyClass);
      ProxyConstructor := TJNIResolver.GetJavaMethodID(ProxyClass, DefaultJConstructor, '()V');
      CreateProxyClass := TJNIResolver.GetJavaMethodID(ProxyClass, CreateProxyClassMethod, CreateProxyClassSig);
      AJNIEnv := TJNIResolver.GetJNIEnv;
      AJNIObject := nil;
      FLocalRefObjectID := nil;
      try
        AJNIObject := AJNIEnv^.NewObject(AJNIEnv, ProxyClass, ProxyConstructor);
        HandleJNIException(AJNIEnv);
        FLocalRefObjectID := AJNIEnv^.CallObjectMethodA(AJNIEnv, AJNIObject, CreateProxyClass, PJNIValue(ArgsToJNIValues([ClsID, Self])));
        HandleJNIException(AJNIEnv);
        FObjectID := AJNIEnv^.NewGlobalRef(AJNIEnv, FLocalRefObjectID);
      finally
        if FLocalRefObjectID <> nil then
          AJNIEnv^.DeleteLocalRef(AJNIEnv, FLocalRefObjectID);
        if AJNIObject <> nil then
          AJNIEnv^.DeleteLocalRef(AJNIEnv, AJNIObject);
      end;
    end
    else
      raise EJNIFatal.CreateResFmt(@SJNINoInterface, [AType.ToString]);

  finally
    AContext.Free;
  end;
end;

destructor TJavaLocal.Destroy;
begin
  TJNIResolver.DeleteGlobalRef(FObjectID);

  inherited;
end;

function TJavaLocal.GetObjectID: Pointer;
begin
  Result := FObjectID;
end;

function TJavaLocal.equals(Obj: Pointer): Boolean;
begin
  if (Obj = FObjectID) or (Obj = FLocalRefObjectID) then
    Result := True
  else
    Result := False;
end;

function TJavaLocal.getName: JNIString;
begin
  Result := StringToJNIString(TJNIResolver.GetJNIEnv, ClassName);
end;

function TJavaLocal.hashCode: Integer;
begin
  Result := Integer(Self);
end;

function TJavaLocal.toString: JNIString;
begin
  Result := StringToJNIString(TJNIResolver.GetJNIEnv, ClassName);
end;

{ TJavaValidate }

class procedure TJavaValidate.ValidateClassHierarchy(P: PTypeInfo);
begin
  TJavaValidate.ValidateTypeHierarchy(P, TypeInfo(IJavaClass), @SBadJNIClass);
end;

class procedure TJavaValidate.ValidateInstanceHierarchy(P: PTypeInfo);
begin
  TJavaValidate.ValidateTypeHierarchy(P, TypeInfo(IJavaInstance), @SBadJNIInstance);
end;

procedure CheckForRtti(P: PTypeInfo);
var
  MethodTable: PIntfMethodTable;
begin
  MethodTable := GetTypeData(P).IntfMethods;
  if (MethodTable^.Count > 0) and (MethodTable^.RttiCount = $FFFF) then
    NoRtti(string(P^.NameFld.ToString));
end;

class procedure TJavaValidate.ValidateTypeHierarchy(P, RequiredRoot: PTypeInfo;
  ErrMsg: PResStringRec);
var
  PP: PPTypeInfo;
  procedure BadType;
  begin
    raise EJNI.CreateResFmt(ErrMsg, [string(P^.NameFld.ToString)]);
  end;
begin
  if P^.Kind <> tkInterface then
    BadType;
  while P <> nil do
  begin
    CheckForRtti(P);
    if P = RequiredRoot then
      Exit;
    PP := GetTypeData(P).IntfParent;
    if Assigned(PP) then
      P := PP^
    else
      P := nil;
  end;
  BadType;
end;

end.
