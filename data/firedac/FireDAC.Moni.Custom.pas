{*******************************************************}
{                                                       }
{               Delphi FireDAC Framework                }
{         FireDAC monitor custom implementation         }
{                                                       }
{ Copyright(c) 2004-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}
{$I FireDAC.inc}
{$IFDEF WIN32}
  {$HPPEMIT '#pragma link "FireDAC.Moni.Custom.obj"'}
{$ELSE}
  {$HPPEMIT '#pragma link "FireDAC.Moni.Custom.o"'}
{$ENDIF}

unit FireDAC.Moni.Custom;

interface

uses
  System.Classes,
  FireDAC.Stan.Intf, FireDAC.Moni.Base;

{$IFDEF FireDAC_MONITOR}
type
  {-----------------------------------------------------------------------------}
  { TFDMoniCustomClientLink                                                     }
  {-----------------------------------------------------------------------------}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or 
    pidiOSSimulator or pidiOSDevice or pidAndroid)]
  TFDMoniCustomClientLink = class(TFDMoniClientLinkBase)
  private
    FCClient: IFDMoniCustomClient;
    function GetSynchronize: Boolean;
    procedure SetSynchronize(AValue: Boolean);
  protected
    function GetMoniClient: IFDMoniClient; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property CClient: IFDMoniCustomClient read FCClient;
  published
    property Tracing;
    property Synchronize: Boolean read GetSynchronize write SetSynchronize
      default False;
  end;

  {-----------------------------------------------------------------------------}
  { TFDMoniCustomClient                                                         }
  {-----------------------------------------------------------------------------}
  TFDMoniCustomClient = class(TFDMoniClientBase, IFDMoniCustomClient)
  private
    FLevel: Integer;
    FSynchronize: Boolean;
  protected
    // IFDMoniClient
    procedure Notify(AKind: TFDMoniEventKind; AStep: TFDMoniEventStep;
      ASender: TObject; const AMsg: String; const AArgs: array of const); override;
    // IFDMoniCustomClient
    function GetSynchronize: Boolean;
    procedure SetSynchronize(AValue: Boolean);
    // other
    function DoTracingChanged: Boolean; override;
    procedure DoTraceMsg(const AClassName, AObjName, AMessage: String); virtual;
  public
    destructor Destroy; override;
  end;
{$ENDIF}

{-------------------------------------------------------------------------------}
implementation

{$IFDEF FireDAC_MONITOR}
uses
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
{$IFDEF POSIX}
  Posix.Pthread,
{$ENDIF}
  System.SysUtils, System.Variants,
  FireDAC.Stan.Consts, FireDAC.Stan.Util, FireDAC.Stan.Factory, FireDAC.Stan.Error;

{-------------------------------------------------------------------------------}
{ TFDMoniCustomClientMsg                                                        }
{-------------------------------------------------------------------------------}
type
  TFDMoniCustomClientMsg = class (TObject)
  private
    FClassName,
    FObjName,
    FMessage: String;
    FClient: IFDMoniCustomClient;
  protected
    procedure DoOutput;
  public
    constructor Create(const AClassName, AObjName, AMessage: String;
      const AClient: IFDMoniCustomClient);
  end;

{-------------------------------------------------------------------------------}
constructor TFDMoniCustomClientMsg.Create(const AClassName, AObjName, AMessage: String;
  const AClient: IFDMoniCustomClient);
begin
  inherited Create;
  FClassName := AClassName;
  FObjName := AObjName;
  FMessage := AMessage;
  FClient := AClient;
end;

{-------------------------------------------------------------------------------}
procedure TFDMoniCustomClientMsg.DoOutput;
begin
  if FClient.OutputHandler <> nil then
    FClient.OutputHandler.HandleOutput(FClassName, FObjName, FMessage);
  Destroy;
end;

{-------------------------------------------------------------------------------}
{ TFDMoniCustomClient                                                           }
{-------------------------------------------------------------------------------}
destructor TFDMoniCustomClient.Destroy;
begin
  SetTracing(False);
  inherited Destroy;
end;

{-------------------------------------------------------------------------------}
function TFDMoniCustomClient.GetSynchronize: Boolean;
begin
  Result := FSynchronize;
end;

{-------------------------------------------------------------------------------}
procedure TFDMoniCustomClient.SetSynchronize(AValue: Boolean);
begin
  FSynchronize := AValue;
end;

{-------------------------------------------------------------------------------}
function TFDMoniCustomClient.DoTracingChanged: Boolean;
begin
  Result := True;
end;

{-------------------------------------------------------------------------------}
procedure TFDMoniCustomClient.DoTraceMsg(const AClassName, AObjName,
  AMessage: String);
begin
  // nothing
end;

{-------------------------------------------------------------------------------}
function VarRec2Variant(AArg: PVarRec): Variant;
begin
  case AArg^.VType of
    vtInteger:    Result := AArg^.VInteger;
    vtBoolean:    Result := AArg^.VBoolean;
    vtChar:       Result := {$IFDEF NEXTGEN} String(AArg^.VChar) {$ELSE}
                                             AArg^.VChar {$ENDIF};
    vtExtended:   Result := AArg^.VExtended^;
    vtString:     Result := {$IFDEF NEXTGEN} String(AArg^.VString^) {$ELSE}
                                             AArg^.VString^ {$ENDIF};
    vtPointer:    Result := NativeUInt(AArg^.VPointer);
    vtPChar:      Result := {$IFDEF NEXTGEN} String(AArg^.VPChar) {$ELSE}
                                             AnsiString(AArg^.VPChar) {$ENDIF};
    vtWideChar:   Result := AArg^.VWideChar;
    vtPWideChar:  Result := UnicodeString(AArg^.VPWideChar);
    vtAnsiString: Result := {$IFDEF NEXTGEN} String(AArg^.VAnsiString) {$ELSE}
                                             AnsiString(AArg^.VAnsiString) {$ENDIF};
    vtCurrency:   Result := AArg^.VCurrency^;
    vtWideString: Result := UnicodeString(AArg^.VWideString);
    vtInt64:      Result := AArg^.VInt64^;
    vtObject:
      // special case - for NULL and Unassigned
      if Byte(LongWord(AArg^.VObject)) = 0 then
        Result := Null
      else if Byte(LongWord(AArg^.VObject)) = 1 then
        Result := Unassigned
      else
        ASSERT(False);
    vtUnicodeString: Result := UnicodeString(AArg^.VUnicodeString);
    else
      // vtClass, vtVariant, vtInterface
      ASSERT(False);
    end;
end;

{-------------------------------------------------------------------------------}
procedure TFDMoniCustomClient.Notify(AKind: TFDMoniEventKind;
  AStep: TFDMoniEventStep; ASender: TObject; const AMsg: String;
  const AArgs: array of const);
var
  s, sClassName, sName: String;
  i, iHigh, iLow: Integer;
  lQuotes: Boolean;
  oMsg: TFDMoniCustomClientMsg;
begin
  if AStep = esEnd then
    Dec(FLevel);
  if GetTracing and (AKind in GetEventKinds) then begin
    s := StringOfChar(' ', FLevel * 4);
    case AStep of
    esStart:    s := s + '>> ';
    esProgress: s := s + ' . ';
    esEnd:      s := s + '<< ';
    end;
    s := s + AMsg;
    iHigh := High(AArgs);
    iLow := Low(AArgs);
    if iHigh - iLow + 1 >= 2 then begin
      s := s + ' [';
      i := iLow;
      while i < iHigh do begin
        if i <> iLow then
          s := s + ', ';
        lQuotes := False;
        s := s + FDIdentToStr(VarRec2Variant(@AArgs[i]), lQuotes) + '=' +
          FDValToStr(VarRec2Variant(@AArgs[i + 1]), lQuotes);
        Inc(i, 2);
      end;
      s := s + ']';
    end;
    GetObjectNames(ASender, sClassName, sName);
    DoTraceMsg(sClassName, sName, s);
    if GetOutputHandler <> nil then
      if not GetSynchronize or (GetCurrentThreadID = MainThreadID) then
        GetOutputHandler.HandleOutput(sClassName, sName, s)
      else begin
        oMsg := TFDMoniCustomClientMsg.Create(sClassName, sName, s, Self);
        TThread.Queue(nil, oMsg.DoOutput);
      end;
  end;
  if AStep = esStart then
    Inc(FLevel);
end;

{-------------------------------------------------------------------------------}
{ TFDMoniCustomClientLink                                                       }
{-------------------------------------------------------------------------------}
constructor TFDMoniCustomClientLink.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCClient := MoniClient as IFDMoniCustomClient;
end;

{-------------------------------------------------------------------------------}
destructor TFDMoniCustomClientLink.Destroy;
begin
  FCClient := nil;
  inherited Destroy;
end;

{-------------------------------------------------------------------------------}
function TFDMoniCustomClientLink.GetMoniClient: IFDMoniClient;
var
  oCClient: IFDMoniCustomClient;
begin
  FDCreateInterface(IFDMoniCustomClient, oCClient);
  Result := oCClient as IFDMoniClient;
end;

{-------------------------------------------------------------------------------}
function TFDMoniCustomClientLink.GetSynchronize: Boolean;
begin
  Result := FCClient.Synchronize;
end;

{-------------------------------------------------------------------------------}
procedure TFDMoniCustomClientLink.SetSynchronize(AValue: Boolean);
begin
  FCClient.Synchronize := AValue;
end;

{-------------------------------------------------------------------------------}
var
  oFact: TFDFactory;

initialization
  oFact := TFDSingletonFactory.Create(TFDMoniCustomClient, IFDMoniCustomClient);

finalization
  FDReleaseFactory(oFact);
{$ENDIF}

end.
