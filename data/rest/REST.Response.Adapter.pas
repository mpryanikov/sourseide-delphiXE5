{*******************************************************}
{                                                       }
{             Delphi REST Client Framework              }
{                                                       }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}
unit REST.Response.Adapter;

interface

uses
  System.SysUtils, System.Classes, System.StrUtils, System.Types,
  System.Rtti,
  Data.DB, Data.DBXJSON, Data.DBXPlatform, Data.Bind.Components,
  REST.Client,
  REST.Types;

type

  TOnBeforeOpenDataSetEvent = procedure(Sender: TObject) of object;

  TJSONParserObjectCallback = procedure(const AJSONObject: TJSONObject) of object;

  /// <summary>
  /// Updates a dataset with the content of a TJSONValue.  Can dynamically generate field definitions or
  /// use user-specified field definitions.
  /// </summary>
  TCustomJSONDataSetAdapter = class abstract(TComponent)
  strict private
    FDataSet: TDataSet;
    FFieldDefsDS: TDataSet;
    FFieldDefs: TFieldDefs;
    FInternalFieldDefsDS: TDataSet;
    FInternalFieldDefs: TFieldDefs;

    procedure SetFieldDefs(AValue: TFieldDefs);
    procedure SetDataSet(const AValue: TDataSet);
  protected
    FJSONValue: TJSONValue;
    procedure InvalidateJSONValue;
    procedure DoBeforeOpenDataSet; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure CB_CollectFieldDefs(const AJSONObject: TJSONObject);
    procedure CB_CollectFieldData(const AJSONObject: TJSONObject);

    function GetFieldDefsClass: TFieldDefsClass; virtual;

    procedure DoParseJSONForObjects(const ACallback: TJSONParserObjectCallback);
    procedure DoScanForFields; virtual;
    function DoAddDataSetFieldDef(const AFieldName: string; const ADataType: TFieldType): boolean; virtual;
    procedure DoTransferData; virtual;
    function DoSetFieldValue(const AFieldName: string; AFieldValue: variant): boolean; virtual;
    procedure DoCreateDataSetFields; virtual;
    /// <summary>
    /// Provides JSONValue to write to the dataset.
    /// </summary>
    function GetJSONValue: TJSONValue; virtual; abstract;
    function HasJSONValue: boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// <summary>
    /// Resets an adapter to default values and clears all entries.
    /// Also disconnects from other rest-components. leaves an attached
    /// dataset untouched.
    /// </summary>
    procedure ResetToDefaults; virtual;
    /// <summary>
    /// Creates or re-creates an attached dataset. this includes the creation of the fields
    /// as well as filling the dataset with the data found in the rest-response. this method is
    /// called automatically after an attched TRESTResponse-Object has new response-content
    /// </summary>
    procedure UpdateDataSet;

    procedure ClearDataSet;

    property Dataset: TDataSet read FDataSet write SetDataSet;
    /// <summary>
    /// If fielddefs are present, an attached dataset will be formed by this pattern.
    /// No additonal fields will be added automatically, no scanning for possible fields
    /// will be performed. pre-defining the fields might have a positive effect on the
    /// performance for large responses.
    /// </summary>
    property FieldDefs: TFieldDefs read FFieldDefs write SetFieldDefs;

  end;

  /// <summary>
  /// Updates a dataset with the JSONValue from a REST response.
  /// </summary>
  TCustomRESTResponseDataSetAdapter = class(TComponent)
  private type
    /// <summary>
    /// Declare adapter responsible for writing the JSONValue to the dataset
    /// </summary>
    TAdapter = class(TCustomJSONDataSetAdapter)
    private
      FOwner: TCustomRESTResponseDataSetAdapter;
    protected
      function GetJSONValue: TJSONValue; override;
      function HasJSONValue: boolean; override;
      procedure DoBeforeOpenDataSet; override;
    end;

  type
    TNotify = class(TCustomRESTResponse.TNotify)
    private
      FOwner: TCustomRESTResponseDataSetAdapter;
      constructor Create(const AOwner: TCustomRESTResponseDataSetAdapter);
    protected
      procedure JSONValueChanged(ASender: TObject); override;
    end;
  strict private
    FOnBeforeOpenDataSet: TOnBeforeOpenDataSetEvent;
    FAdapter: TAdapter;
    FResponse: TCustomRESTResponse;
    FAutoUpdate: boolean;
    FNotify: TNotify;
    procedure SetResponse(const AValue: TCustomRESTResponse);
    function GetDataSet: TDataSet;
    function GetFieldDefs: TFieldDefs;
    procedure SetDataSet(const Value: TDataSet);
    procedure SetFieldDefs(const Value: TFieldDefs);
    procedure SetActive(const Value: boolean);
    function GetActive: boolean;
    procedure SetAutoUpdate(const Value: boolean);
  protected
    FRootElement: string;
    procedure DoJSONValueChanged; virtual;
    procedure DoBeforeOpenDataSet; virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetRootElement(const AValue: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Active: boolean read GetActive write SetActive default False;
    /// <summary>
    /// Set to automatically update dataset when the request is executed.
    /// </summary>
    property AutoUpdate: boolean read FAutoUpdate write SetAutoUpdate default True;
    property Response: TCustomRESTResponse read FResponse write SetResponse;
    /// <summary>
    /// Resets an adapter to default values and clears all entries.
    /// Also disconnects from other rest-components. leaves an attached
    /// dataset untouched.
    /// </summary>
    procedure ResetToDefaults; virtual;
    /// <summary>
    /// Creates or re-creates an attached dataset. this includes the creation of the fields
    /// as well as filling the dataset with the data found in the rest-response. this method is
    /// called automatically after an attched TRESTResponse-Object has new response-content
    /// </summary>
    procedure UpdateDataSet;
    /// <summary>
    /// Clears an attached dataset.
    /// </summary>
    procedure ClearDataSet;
    property Dataset: TDataSet read GetDataSet write SetDataSet;
    /// <summary>
    /// If fielddefs are present, an attached dataset will be formed by this pattern.
    /// No additonal fields will be added automatically, no scanning for possible fields
    /// will be performed. pre-defining the fields might have a positive effect on the
    /// performance for large responses.
    /// </summary>
    property FieldDefs: TFieldDefs read GetFieldDefs write SetFieldDefs;
    /// <summary>
    /// Used by the JSON deserializer to determine where to start deserializing from. Can be used to skip container
    /// or root elements that do not have corresponding deserialization targets.  This RootElement is applied after
    /// an optionally set RootElement of the corresponding TRESTResponse was applied.
    /// This way a response container can be filter on Response level {response: {some actual response object}}
    /// while multiple DataSetAdapters can filter their own, specific root elements.
    /// </summary>
    // <seealso cref="TCustomRESTResponse.JsonValue" />
    // <seealso cref="TCustomRESTResponseDataSetAdapter.JsonValue" />
    property RootElement: string read FRootElement write SetRootElement;

    property OnBeforeOpenDataSet: TOnBeforeOpenDataSetEvent read FOnBeforeOpenDataSet write FOnBeforeOpenDataSet;
  end;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice or pidAndroid)]
  TRESTResponseDataSetAdapter = class(TCustomRESTResponseDataSetAdapter)
  published
    property Active;
    property AutoUpdate;
    property Dataset;
    property FieldDefs;
    property Response;
    property OnBeforeOpenDataSet;
    property RootElement;
  end;

implementation

uses
  REST.Utils;

type
  /// <summary>
  /// we need a TDataSet-Class to maintain the FieldDefs (they're requiring a
  /// "living" dataset. we cannot just use TDataSet as it contains abstract
  /// methods. so we need a minimal implementation without abstract methods.
  /// USED ONLY INTERNALLY
  /// </summary>
  TRESTAdapterDataSet = class(TDataSet)
  protected
    procedure InternalClose; override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    function IsCursorOpen: boolean; override;
{$IFDEF NEXTGEN}
    function GetRecord(Buffer: TRecBuf; GetMode: TGetMode; DoCheck: boolean): TGetResult; override;
{$ENDIF !NEXTGEN}
  end;

function RESTFindDefaultDataSet(AComp: TComponent): TDataSet;
var
  LRoot: TComponent;
  i: integer;
begin
  result := nil;
  LRoot := AComp;
  while (LRoot.Owner <> nil) and (result = nil) do
  begin
    LRoot := LRoot.Owner;
    for i := 0 to LRoot.ComponentCount - 1 do
      if LRoot.Components[i] is TDataSet then
      begin
        result := TDataSet(LRoot.Components[i]);
        Break;
      end;
  end;
end;

(*
  function ExtractJSONRootElement(const ARootElement: string; AJSONValue: TJSONValue): TJSONValue;
  var
  LTokens: TStringDynArray;
  LToken: string;
  LJSONObject: TJSONObject;
  LJSONPair: TJSONPair;
  LJSONValue: TJSONValue;
  begin
  if (ARootElement = '') OR (NOT Assigned(AJSONValue)) then
  EXIT(AJSONValue);
  result := NIL;
  LJSONValue := nil;
  if (AJSONValue IS TJSONObject) then
  begin
  LJSONObject := (AJSONValue AS TJSONObject);

  LTokens := SplitString(ARootElement, '.');
  for LToken IN LTokens do
  begin
  if Assigned(LJSONObject) then
  begin
  LJSONPair := LJSONObject.Get(LToken);
  if LJSONPair <> nil then
  begin
  LJSONValue := LJSONPair.JsonValue;
  if (LJSONPair.JsonValue IS TJSONObject) then
  LJSONObject := TJSONObject(LJSONValue)
  else
  LJSONObject := NIL;
  end
  else
  LJSONObject := nil;
  end
  else
  Break;
  end;

  result := LJSONValue;
  end;
  end;

*)

{ TCustomJSONDataSetAdapter }

procedure TCustomJSONDataSetAdapter.CB_CollectFieldData(const AJSONObject: TJSONObject);
var
  LJSONPair: TJSONPair;
begin
  Assert(FDataSet <> nil);
  FDataSet.Append;
  try
    for LJSONPair in AJSONObject do
    begin
      DoSetFieldValue(LJSONPair.JsonString.Value, LJSONPair.JsonValue.Value);
    end;
  finally
    FDataSet.Post;
  end;
end;

procedure TCustomJSONDataSetAdapter.CB_CollectFieldDefs(const AJSONObject: TJSONObject);
var
  LJSONPair: TJSONPair;
begin
  for LJSONPair in AJSONObject do
  begin
    DoAddDataSetFieldDef(LJSONPair.JsonString.Value, ftString);
  end;
end;

procedure TCustomJSONDataSetAdapter.ClearDataSet;
begin
  if Assigned(FDataSet) then
  begin
    FDataSet.Close;
    FDataSet.FieldDefs.Clear;
  end;
end;

constructor TCustomJSONDataSetAdapter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDataSet := nil;

  FFieldDefsDS := TRESTAdapterDataSet.Create(self);
  FFieldDefs := GetFieldDefsClass.Create(FFieldDefsDS);
  FInternalFieldDefsDS := TRESTAdapterDataSet.Create(self);
  FInternalFieldDefs := GetFieldDefsClass.Create(FInternalFieldDefsDS);
end;

destructor TCustomJSONDataSetAdapter.Destroy;
begin
  FreeAndNIL(FFieldDefs);
  FreeAndNIL(FFieldDefsDS);
  FreeAndNIL(FInternalFieldDefs);
  FreeAndNIL(FInternalFieldDefsDS);
  FreeAndNIL(FJSONValue);
  inherited;
end;

function TCustomJSONDataSetAdapter.DoAddDataSetFieldDef(const AFieldName: string; const ADataType: TFieldType): boolean;
var
  LFieldDef: TFieldDef;
begin
  result := FALSE;

  /// we add each field only once

  if (FInternalFieldDefs.IndexOf(AFieldName) < 0) then
  begin
    LFieldDef := FInternalFieldDefs.AddFieldDef;
    LFieldDef.Name := AFieldName;
    LFieldDef.DataType := ADataType;
    /// set size for stringtype-fields
    if (ADataType in [ftString, ftWideString]) then
      LFieldDef.Size := 255;

    result := TRUE;
  end;
end;

procedure TCustomJSONDataSetAdapter.DoBeforeOpenDataSet;
begin
  //
end;

procedure TCustomJSONDataSetAdapter.DoCreateDataSetFields;
begin
  Assert(Assigned(FDataSet));

  if FDataSet.Active then
    FDataSet.Close;

  FDataSet.DisableControls;
  try
    FDataSet.Fields.Clear;
    FDataSet.FieldDefs.Clear;

    FDataSet.FieldDefs.Assign(FInternalFieldDefs);
  finally
    FDataSet.EnableControls;
  end;
end;

procedure TCustomJSONDataSetAdapter.DoParseJSONForObjects(const ACallback: TJSONParserObjectCallback);
var
  LJSONRoot: TJSONValue;
  LValue: TJSONValue;
begin
  LJSONRoot := GetJSONValue;

  if Assigned(LJSONRoot) then
  begin
    if (LJSONRoot is TJSONObject) then
    begin
      ACallback((LJSONRoot as TJSONObject));
    end
    else if (LJSONRoot is TJSONArray) then
    begin
      for LValue in (LJSONRoot as TJSONArray) do
      begin
        if (LValue is TJSONObject) then
        begin
          ACallback((LValue as TJSONObject));
        end;
      end;
    end;
  end;
end;

procedure TCustomJSONDataSetAdapter.DoScanForFields;
begin
  Assert(Assigned(FDataSet));

  /// we cannot add fields to an active dataset
  if FDataSet.Active then
    FDataSet.Close;

  if (HasJSONValue) then
  begin
    DoParseJSONForObjects(CB_CollectFieldDefs);
  end
  else
  begin
                                                       
  end;

end;

procedure TCustomJSONDataSetAdapter.DoTransferData;
var
  LContext: TRTTIContext;
  LType: TRTTIType;
  LMethod: TRTTIMethod;
begin
  Assert(Assigned(FDataSet));

  /// dataset MUST have fields before we can continue
  if (FDataSet.FieldDefs.Count = 0) then
    EXIT;

  // Disable controls while creating dataset so that
  // activate notification is sent after populate
  FDataSet.DisableControls;
  try
    if not FDataSet.Active then
    begin
      DoBeforeOpenDataSet;
      if not FDataSet.Active then
      begin
        LType := LContext.GetType(FDataSet.ClassType);
        if LType <> nil then
        begin
          // Support TClientDataSet
          LMethod := LType.GetMethod('CreateDataSet');
          if (LMethod <> nil) and (Length(LMethod.GetParameters) = 0) then
            LMethod.Invoke(FDataSet, []);
        end;
      end;
      if not FDataSet.Active then
        FDataSet.Open;
    end;

    if (HasJSONValue) then
    begin
      DoParseJSONForObjects(CB_CollectFieldData);
    end
    else
    begin
                                                         
    end;

    /// just for convenience we relocate
    /// the dataset to the first record
    FDataSet.First;
  finally
    FDataSet.EnableControls;
  end;
end;

function TCustomRESTResponseDataSetAdapter.GetActive: boolean;
begin
  result := (Dataset <> nil) and Dataset.Active;
end;

function TCustomJSONDataSetAdapter.GetFieldDefsClass: TFieldDefsClass;
begin
  result := DefaultFieldDefsClass;
end;

function TCustomJSONDataSetAdapter.HasJSONValue: boolean;
begin
  result := GetJSONValue <> nil;
end;

procedure TCustomJSONDataSetAdapter.InvalidateJSONValue;
begin
  FreeAndNIL(FJSONValue);
end;

procedure TCustomJSONDataSetAdapter.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  /// clean up component-references
  if (Operation = opRemove) then
  begin
    if (AComponent = FDataSet) then
      FDataSet := nil
  end;
end;

procedure TCustomJSONDataSetAdapter.ResetToDefaults;
begin
  FFieldDefs.Clear;
  FFieldDefsDS.Fields.Clear;
  FFieldDefsDS.FieldDefs.Clear;
  FInternalFieldDefsDS.Fields.Clear;
  FInternalFieldDefsDS.FieldDefs.Clear;

end;

function TCustomJSONDataSetAdapter.DoSetFieldValue(const AFieldName: string; AFieldValue: variant): boolean;
var
  LField: TField;
begin
  Assert(Assigned(FDataSet));

  result := FALSE;

  if (FDataSet.State in dsEditModes) then
  begin
    LField := FDataSet.FindField(AFieldName);
    if Assigned(LField) then
    begin
      LField.Value := AFieldValue;
      result := TRUE;
    end;
  end;
end;

procedure TCustomJSONDataSetAdapter.SetDataSet(const AValue: TDataSet);
begin
  if Assigned(FDataSet) then
    FDataSet.RemoveFreeNotification(self);

  FDataSet := AValue;

  if Assigned(FDataSet) then
    FDataSet.FreeNotification(self);
end;

procedure TCustomJSONDataSetAdapter.SetFieldDefs(AValue: TFieldDefs);
begin
  FieldDefs.Assign(AValue);
end;

procedure TCustomJSONDataSetAdapter.UpdateDataSet;
begin
  /// this code might get triggered even without a tdataset
  /// being attached. so we should check, if we have a dataset
  /// or not - both is okay. really.
  if Assigned(FDataSet) and not (csDestroying in FDataSet.ComponentState) then
  begin
    /// if we do not have any predefined fields-defs,
    /// we scan the JSON for available fields
    if (FFieldDefs.Count = 0) then
    begin
      FInternalFieldDefs.Clear;
      DoScanForFields;
    end
    else
    begin
      FInternalFieldDefs.Assign(FFieldDefs);
    end;

    DoCreateDataSetFields;
      DoTransferData;
  end;
end;

{ TCustomRESTResponseDataSetAdapter }

procedure TCustomRESTResponseDataSetAdapter.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  /// clean up component-references
  if (Operation = opRemove) then
  begin
    if (AComponent = Response) then
      FResponse := nil;
  end;
end;

procedure TCustomRESTResponseDataSetAdapter.ResetToDefaults;
begin
  FAdapter.ResetToDefaults;
end;

procedure TCustomRESTResponseDataSetAdapter.SetActive(const Value: boolean);
begin
  if Value <> Active then
  begin
    if Value then
      FAdapter.UpdateDataSet
    else
      FAdapter.Dataset.Close;
  end;

end;

procedure TCustomRESTResponseDataSetAdapter.SetAutoUpdate(const Value: boolean);
begin
  // Changing autoupdate does not cause an update.
  FAutoUpdate := Value;
end;

procedure TCustomRESTResponseDataSetAdapter.ClearDataSet;
begin
  if Assigned(FAdapter) then
    FAdapter.ClearDataSet;
end;

constructor TCustomRESTResponseDataSetAdapter.Create(AOwner: TComponent);
begin
  inherited;
  FAutoUpdate := TRUE;
  FNotify := TNotify.Create(self);
  FAdapter := TAdapter.Create(nil);
  FAdapter.FOwner := self;
end;

destructor TCustomRESTResponseDataSetAdapter.Destroy;
begin
  inherited;
  FAdapter.Free;
  if FResponse <> nil then
    if FResponse.NotifyList <> nil then
      FResponse.NotifyList.RemoveNotify(FNotify);
  FNotify.Free;
end;

procedure TCustomRESTResponseDataSetAdapter.DoBeforeOpenDataSet;
begin
  if Assigned(FOnBeforeOpenDataSet) then
    FOnBeforeOpenDataSet(self)
end;

procedure TCustomRESTResponseDataSetAdapter.DoJSONValueChanged;
begin
  if Assigned(FAdapter) then
    FAdapter.InvalidateJSONValue;

  if not(csLoading in ComponentState) then
    if AutoUpdate then
      UpdateDataSet;
end;

function TCustomRESTResponseDataSetAdapter.GetDataSet: TDataSet;
begin
  if Assigned(FAdapter) then
    result := FAdapter.Dataset
  else
    result := nil;
end;

function TCustomRESTResponseDataSetAdapter.GetFieldDefs: TFieldDefs;
begin
  if Assigned(FAdapter) then
    result := FAdapter.FieldDefs
  else
    result := nil;
end;

procedure TCustomRESTResponseDataSetAdapter.Loaded;
begin
  inherited;
  if AutoUpdate then
    UpdateDataSet;
end;

procedure TCustomRESTResponseDataSetAdapter.SetDataSet(const Value: TDataSet);
begin
  if Assigned(FAdapter) then
    FAdapter.Dataset := Value;
end;

procedure TCustomRESTResponseDataSetAdapter.SetFieldDefs(const Value: TFieldDefs);
begin
  if Assigned(FAdapter) then
    FAdapter.FieldDefs := Value;
end;

procedure TCustomRESTResponseDataSetAdapter.SetResponse(const AValue: TCustomRESTResponse);
begin
  if Assigned(FResponse) then
  begin
    FResponse.RemoveFreeNotification(self);
    if FResponse.NotifyList <> nil then
      FResponse.NotifyList.RemoveNotify(FNotify);
    FResponse.RemoveFreeNotification(self);
  end;

  FResponse := AValue;

  if Assigned(FResponse) then
  begin
    FResponse.FreeNotification(self);
    if FResponse.NotifyList <> nil then
      FResponse.NotifyList.AddNotify(FNotify);
    FResponse.RemoveFreeNotification(self);
  end;
end;

procedure TCustomRESTResponseDataSetAdapter.SetRootElement(const AValue: string);
begin
  if FRootElement <> AValue then
  begin
    FRootElement := AValue;
    DoJSONValueChanged;
  end;
end;

procedure TCustomRESTResponseDataSetAdapter.UpdateDataSet;
begin
  if Assigned(FAdapter) then
    FAdapter.UpdateDataSet;
end;

{ TCustomRESTResponseDataSetAdapter.TAdapter }

procedure TCustomRESTResponseDataSetAdapter.TAdapter.DoBeforeOpenDataSet;
begin
  FOwner.DoBeforeOpenDataSet;
end;

function TCustomRESTResponseDataSetAdapter.TAdapter.GetJSONValue: TJSONValue;
var
  LJsonValue: TJSONValue;
  LTokens: TStringDynArray;
  LToken: string;
  LJsonRootPair: TJSONPair;
  LJSONValueNew: TJSONValue;
begin
  if not Assigned(FJSONValue) then
  begin
    if HasJSONValue then
    begin
      LJsonValue := FOwner.Response.JsonValue.Clone as TJSONValue;
      if (FOwner.RootElement <> '') and Assigned(LJsonValue) and (LJsonValue is TJSONObject) then
      begin
        LTokens := SplitString(FOwner.RootElement, '.');
        for LToken in LTokens do
        begin
          LJsonRootPair := TJSONObject(LJsonValue).Get(LToken);
          if Assigned(LJsonRootPair) then
          begin
            LJSONValueNew := LJsonRootPair.JsonValue;
            LJSONValueNew.Owned := FALSE; // Need to decouple form parent, to avoid memleak
            FreeAndNIL(LJsonValue);
            LJsonValue := LJSONValueNew;
          end
          else
          begin
            LJsonValue := nil;
            Break;
          end;
        end;
      end;
      FJSONValue := LJsonValue;
    end;
  end;
  result := FJSONValue;
end;

function TCustomRESTResponseDataSetAdapter.TAdapter.HasJSONValue: boolean;
begin
  result := Assigned(FOwner) and Assigned(FOwner.Response) and Assigned(FOwner.Response.JsonValue)
end;

{ TRESTAdapterDataSet }

{$IFDEF NEXTGEN}

function TRESTAdapterDataSet.GetRecord(Buffer: TRecBuf; GetMode: TGetMode; DoCheck: boolean): TGetResult;
begin
  result := grError;
end;
{$ENDIF}

procedure TRESTAdapterDataSet.InternalClose;
begin
  inherited;
  // nothing to do here, implementation just to avoid
  // abstract methods
end;

procedure TRESTAdapterDataSet.InternalHandleException;
begin
  inherited;
  // nothing to do here, implementation just to avoid
  // abstract methods
end;

procedure TRESTAdapterDataSet.InternalInitFieldDefs;
begin
  inherited;
  // nothing to do here, implementation just to avoid
  // abstract methods
end;

procedure TRESTAdapterDataSet.InternalOpen;
begin
  inherited;
  // nothing to do here, implementation just to avoid
  // abstract methods
end;

function TRESTAdapterDataSet.IsCursorOpen: boolean;
begin
  result := FALSE;
end;

{ TCustomRESTResponseDataSetAdapter.TNotify }

constructor TCustomRESTResponseDataSetAdapter.TNotify.Create(const AOwner: TCustomRESTResponseDataSetAdapter);
begin
  FOwner := AOwner;
end;

procedure TCustomRESTResponseDataSetAdapter.TNotify.JSONValueChanged(ASender: TObject);
begin
  FOwner.DoJSONValueChanged;
end;

end.
