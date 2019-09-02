{*******************************************************}
{                                                       }
{               Delphi FireDAC Framework                }
{                FireDAC DataSnap driver                }
{                                                       }
{ Copyright(c) 2004-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}
{$I FireDAC.inc}
{$IFDEF WIN32}
  {$HPPEMIT '#pragma link "FireDAC.Phys.DataSnap.obj"'}
{$ELSE}
  {$HPPEMIT '#pragma link "FireDAC.Phys.DataSnap.o"'}
{$ENDIF}

unit FireDAC.Phys.DataSnap;

interface

uses
  System.Classes,
  FireDAC.Phys.TDBXBase;

type
  TFDPhysDataSnapDriverLink = class;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or 
    pidiOSSimulator or pidiOSDevice or pidAndroid)]
  TFDPhysDataSnapDriverLink = class(TFDPhysTDBXBaseDriverLink)
  protected
    function GetBaseDriverID: String; override;
  end;

{-------------------------------------------------------------------------------}
implementation

uses
  System.SysUtils, System.IniFiles, System.Variants,
  Data.DBXCommon, Data.DbxDatasnap, Data.DbxJSON, Datasnap.DSProxy, Datasnap.DSCommon,
  FireDAC.Stan.Intf, FireDAC.Stan.Consts, FireDAC.Stan.Util, FireDAC.Stan.Error,
  FireDAC.Phys.Intf, FireDAC.Phys, FireDAC.Phys.SQLGenerator, FireDAC.Phys.Meta,
    FireDAC.Phys.DataSnapMeta;

type
  TFDPhysDataSnapDriver = class;
  TFDPhysDataSnapConnection = class;
  TFDPhysDataSnapEventAlerter = class;
  TFDPhysDataSnapCommand = class;

  TFDPhysDataSnapDriver = class(TFDPhysTDBXDriverBase)
  protected
    class function GetBaseDriverID: String; override;
    function InternalCreateConnection(AConnHost: TFDPhysConnectionHost): TFDPhysConnection; override;
    function GetConnParamCount(AKeys: TStrings): Integer; override;
    procedure GetConnParams(AKeys: TStrings; Index: Integer;
      var AName, AType, ADefVal, ACaption: String; var ALoginIndex: Integer); override;
  end;

  TFDPhysDataSnapConnection = class(TFDPhysTDBXConnectionBase)
  private
    FProductVersion: TFDVersion;
    function CheckNetworkError(AExc: Exception; AObj: TObject): Boolean;
  protected
    function InternalCreateEvent(const AEventKind: String): TFDPhysEventAlerter; override;
    function InternalCreateCommand: TFDPhysCommand; override;
    function InternalCreateCommandGenerator(const ACommand: IFDPhysCommand):
      TFDPhysCommandGenerator; override;
    function InternalCreateMetadata: TObject; override;
    procedure InternalConnect; override;
    procedure InternalPing; override;
    procedure CheckDBXDriver; override;
    procedure BuildDBXConnParams(const AConnectionDef: IFDStanConnectionDef;
      AConnProps: TDBXProperties); override;
  end;

  TFDPhysDataSnapEventAlerter = class(TFDPhysEventAlerter)
  private
    FManagers: TFDObjList;
    FCallbacks: TFDStringList;
    function GetDSConn: TFDPhysDataSnapConnection;
  protected
    procedure InternalAllocHandle; override;
    procedure InternalReleaseHandle; override;
    procedure InternalHandle(AEventMessage: TFDPhysEventMessage); override;
    procedure InternalRegister; override;
    procedure InternalUnregister; override;
    procedure InternalSignal(const AEvent: String; const AArgument: Variant); override;
  public
    constructor Create(AConnection: TFDPhysConnection; const AKind: String); override;
    destructor Destroy; override;
    property DSConn: TFDPhysDataSnapConnection read GetDSConn;
  end;

  TFDPhysDataSnapCommand = class(TFDPhysTDBXCommand)
  private
    function GetDSConn: TFDPhysDataSnapConnection;
  protected
    procedure InternalExecute(ATimes, AOffset: Integer; var ACount: TFDCounter); override;
    function InternalOpen: Boolean; override;
    function InternalNextRecordSet: Boolean; override;
    procedure InternalPrepare; override;
  public
    property DSConn: TFDPhysDataSnapConnection read GetDSConn;
  end;

const
  S_FD_TCPIP = 'TCP/IP';
  S_FD_HTTP = 'HTTP';

{-------------------------------------------------------------------------------}
{ TFDPhysDataSnapDriverLink                                                     }
{-------------------------------------------------------------------------------}
function TFDPhysDataSnapDriverLink.GetBaseDriverID: String;
begin
  Result := S_FD_DataSnapId;
end;

{-------------------------------------------------------------------------------}
{ TFDPhysDataSnapDriver                                                         }
{-------------------------------------------------------------------------------}
class function TFDPhysDataSnapDriver.GetBaseDriverID: String;
begin
  Result := S_FD_DataSnapId;
end;

{-------------------------------------------------------------------------------}
function TFDPhysDataSnapDriver.InternalCreateConnection(
  AConnHost: TFDPhysConnectionHost): TFDPhysConnection;
begin
  Result := TFDPhysDataSnapConnection.Create(Self, AConnHost);
end;

{-------------------------------------------------------------------------------}
function TFDPhysDataSnapDriver.GetConnParamCount(AKeys: TStrings): Integer;
begin
  Result := inherited GetConnParamCount(AKeys) + 5;
  if (AKeys <> nil) and
     (CompareText(AKeys.Values[S_FD_ConnParam_DataSnap_Protocol], S_FD_HTTP) = 0) then
    Result := Result + 9;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysDataSnapDriver.GetConnParams(AKeys: TStrings; Index: Integer;
  var AName, AType, ADefVal, ACaption: String; var ALoginIndex: Integer);
begin
  ALoginIndex := -1;
  ADefVal := '';
  if Index < inherited GetConnParamCount(AKeys) then begin
    inherited GetConnParams(AKeys, Index, AName, AType, ADefVal, ACaption, ALoginIndex);
    if AName = S_FD_ConnParam_Common_Database then
      ALoginIndex := -1;
  end
  else begin
    Index := Index - inherited GetConnParamCount(AKeys);
    case Index of
    0:
      begin
        AName := S_FD_ConnParam_DataSnap_Protocol;
        AType := S_FD_TCPIP + ';' + S_FD_HTTP;
        ADefVal := S_FD_TCPIP;
      end;
    1:
      begin
        AName := S_FD_ConnParam_Common_Server;
        AType := '@S';
        ADefVal := '127.0.0.1';
        ALoginIndex := 2;
      end;
    2:
      begin
        AName := TDBXPropertyNames.Port;
        AType := '@I';
        if (AKeys <> nil) and
           (CompareText(AKeys.Values[S_FD_ConnParam_DataSnap_Protocol], S_FD_HTTP) = 0) then
          ADefVal := '8080'
        else
          ADefVal := '211';
        ALoginIndex := 3;
      end;
    3:
      begin
        AName := TDBXPropertyNames.BufferKBSize;
        AType := '@I';
        ADefVal := '32';
      end;
    4:
      begin
        AName := TDBXPropertyNames.Filters;
        AType := '@S';
      end;
    else
      if (AKeys <> nil) and
         (CompareText(AKeys.Values[S_FD_ConnParam_DataSnap_Protocol], S_FD_HTTP) = 0) then
        case Index of
        5:
          begin
            AName := TDBXPropertyNames.URLPath;
            AType := '@S';
          end;
        6:
          begin
            AName := TDBXPropertyNames.DatasnapContext;
            AType := '@S';
            ADefVal := DS_CONTEXT;
          end;
        7:
          begin
            AName := TDBXPropertyNames.DSProxyHost;
            AType := '@S';
          end;
        8:
          begin
            AName := TDBXPropertyNames.DSProxyPort;
            AType := '@I';
          end;
        9:
          begin
            AName := TDBXPropertyNames.DSProxyUsername;
            AType := '@S';
          end;
        10:
          begin
            AName := TDBXPropertyNames.DSProxyPassword;
            AType := '@S';
          end;
        11:
          begin
            AName := TDBXPropertyNames.DSAuthenticationScheme;
            AType := ';basic';
          end;
        12:
          begin
            AName := S_FD_ConnParam_Common_LoginTimeout;
            AType := '@I';
          end;
        13:
          begin
            AName := TDBXPropertyNames.CommunicationTimeout;
            AType := '@I';
          end;
        end;
    end;
    ACaption := AName;
  end;
end;

{-------------------------------------------------------------------------------}
{ TFDPhysDataSnapConnection                                                     }
{-------------------------------------------------------------------------------}
function TFDPhysDataSnapConnection.InternalCreateEvent(const AEventKind: String): TFDPhysEventAlerter;
begin
  if CompareText(AEventKind, S_FD_EventKind_DataSnap_Events) = 0 then
    Result := TFDPhysDataSnapEventAlerter.Create(Self, AEventKind)
  else
    Result := nil;
end;

{-------------------------------------------------------------------------------}
function TFDPhysDataSnapConnection.InternalCreateCommand: TFDPhysCommand;
begin
  Result := TFDPhysDataSnapCommand.Create(Self);
end;

{-------------------------------------------------------------------------------}
function TFDPhysDataSnapConnection.InternalCreateCommandGenerator(
  const ACommand: IFDPhysCommand): TFDPhysCommandGenerator;
begin
  Result := TFDPhysCommandGenerator.Create(ACommand);
end;

{-------------------------------------------------------------------------------}
function TFDPhysDataSnapConnection.InternalCreateMetadata: TObject;
begin
  Result := TFDPhysDataSnapMetadata.Create(Self, 0, FProductVersion, True);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysDataSnapConnection.CheckDBXDriver;
begin
  FDriverName := 'DataSnap';
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysDataSnapConnection.BuildDBXConnParams(
  const AConnectionDef: IFDStanConnectionDef; AConnProps: TDBXProperties);

  procedure SetParam(const ADbxPar, AADPar, ADef: String);
  var
    s: String;
  begin
    s := AADPar;
    if s = '' then
      s := ADbxPar;
    s := AConnectionDef.AsString[s];
    if s = '' then
      s := ADef;
    if s <> '' then
      AConnProps.Values[ADbxPar] := s;
  end;

begin
  AConnProps.Values[TDBXPropertyNames.DriverName] := FDriverName;
  AConnProps.Values[TDBXPropertyNames.DriverUnit] := 'Data.DbxDatasnap';
  SetParam(TDBXPropertyNames.CommunicationProtocol, S_FD_ConnParam_DataSnap_Protocol, S_FD_TCPIP);
  SetParam(TDBXPropertyNames.HostName, S_FD_ConnParam_Common_Server, '127.0.0.1');
  if CompareText(AConnectionDef.AsString[S_FD_ConnParam_DataSnap_Protocol], S_FD_TCPIP) = 0 then
    SetParam(TDBXPropertyNames.Port, '', '211')
  else
    SetParam(TDBXPropertyNames.Port, '', '8080');
  SetParam(TDBXPropertyNames.BufferKBSize, '', '32');
  SetParam(TDBXPropertyNames.Filters, '', '');
  SetParam(TDBXPropertyNames.URLPath, '', '');
  SetParam(TDBXPropertyNames.DatasnapContext, '', DS_CONTEXT);
  SetParam(TDBXPropertyNames.DSProxyHost, '', '');
  SetParam(TDBXPropertyNames.DSProxyPort, '', '');
  SetParam(TDBXPropertyNames.DSProxyUsername, '', '');
  SetParam(TDBXPropertyNames.DSProxyPassword, '', '');
  SetParam(TDBXPropertyNames.DSAuthenticationUser, S_FD_ConnParam_Common_UserName, '');
  SetParam(TDBXPropertyNames.DSAuthenticationPassword, S_FD_ConnParam_Common_Password, '');
  SetParam(TDBXPropertyNames.DSAuthenticationScheme, '', '');
  SetParam(TDBXPropertyNames.ConnectTimeout, S_FD_ConnParam_Common_LoginTimeout, '');
  SetParam(TDBXPropertyNames.CommunicationTimeout, '', '');
end;

{-------------------------------------------------------------------------------}
function TFDPhysDataSnapConnection.CheckNetworkError(AExc: Exception; AObj: TObject): Boolean;
var
  oExc: ETDBXNativeException;
  oExcClass: TClass;
begin
  oExcClass := AExc.ClassType;
  while (oExcClass <> nil) and (CompareText(oExcClass.ClassName, 'EIdException') <> 0) do
    oExcClass := oExcClass.ClassParent;
  if oExcClass <> nil then begin
    oExc := ETDBXNativeException.Create(er_FD_DBXGeneral,
      FDExceptionLayers([S_FD_LPhys, S_FD_TDBXId, S_FD_DataSnapId]) + ' ' + AExc.Message);
    oExc.AppendError(1, TDBXErrorCodes.ConnectionFailed, AExc.Message, '', ekServerGone, -1, -1);
    FDException(AObj, oExc {$IFDEF FireDAC_Monitor}, False {$ENDIF});
    Result := True;
  end
  else
    Result := False;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysDataSnapConnection.InternalConnect;
begin
  try
    inherited InternalConnect;
    FProductVersion := FDVerStr2Int(DbxConnection.ProductVersion);
  except
    on E: Exception do
      if not CheckNetworkError(E, Self) then
        raise;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysDataSnapConnection.InternalPing;
begin
  try
    // simplest DataSnap command
    DbxConnection.GetVendorProperty(TDBXPropertyNames.ProductVersion);
  except
    on E: Exception do
      if not CheckNetworkError(E, Self) then
        raise;
  end;
end;

{-------------------------------------------------------------------------------}
{ TFDPhysDataSnapEventMessage                                                   }
{-------------------------------------------------------------------------------}
type
  TFDPhysDataSnapEventMessage = class(TFDPhysEventMessage)
  private
    FCBName: String;
    FArgs: Variant;
  public
    constructor Create(const ACBName: String; const AArgs: Variant);
  end;

{-------------------------------------------------------------------------------}
constructor TFDPhysDataSnapEventMessage.Create(const ACBName: String;
  const AArgs: Variant);
begin
  inherited Create;
  FCBName := ACBName;
  FArgs := AArgs;
end;

{-------------------------------------------------------------------------------}
{ TFDDataSnapCallback                                                           }
{-------------------------------------------------------------------------------}
type
  TFDDataSnapCallback = class(TDBXCallback)
  private
    FAlerter: TFDPhysDataSnapEventAlerter;
    FName: String;
  public
    constructor Crate(AAlerter: TFDPhysDataSnapEventAlerter; const AName: String);
    function Execute(const AArg: TJSONValue): TJSONValue; overload; override;
    function Execute(AArg: TObject): TObject; overload; override;
  end;

{-------------------------------------------------------------------------------}
constructor TFDDataSnapCallback.Crate(AAlerter: TFDPhysDataSnapEventAlerter;
  const AName: String);
begin
  inherited Create;
  FAlerter := AAlerter;
  FName := AName;
end;

{-------------------------------------------------------------------------------}
function TFDDataSnapCallback.Execute(const AArg: TJSONValue): TJSONValue;
var
  V: Variant;
  i: Integer;
begin
  if AArg is TJSONArray then begin
    V := VarArrayCreate([0, TJSONArray(AArg).Size - 1], varADWString);
    for i := 0 to TJSONArray(AArg).Size - 1 do
      V[i] := TJSONArray(AArg).Get(i).Value;
  end
  else
    V := AArg.Value;

  if FAlerter.IsRunning then
    FAlerter.FMsgThread.EnqueueMsg(TFDPhysDataSnapEventMessage.Create(FName, V));

  Result := TJSONTrue.Create;
end;

{-------------------------------------------------------------------------------}
function TFDDataSnapCallback.Execute(AArg: TObject): TObject;
begin
  Result := Execute(TDSClientCallbackChannelManager(FAlerter.FManagers[0]).
    MarshalData(AArg));
end;

{-------------------------------------------------------------------------------}
{ TFDPhysDataSnapEventAlerter                                                   }
{-------------------------------------------------------------------------------}
constructor TFDPhysDataSnapEventAlerter.Create(AConnection: TFDPhysConnection;
  const AKind: String);
begin
  inherited Create(AConnection, AKind);
  FManagers := TFDObjList.Create;
  FCallbacks := TFDStringList.Create;
end;

{-------------------------------------------------------------------------------}
destructor TFDPhysDataSnapEventAlerter.Destroy;
begin
  inherited Destroy;
  FDFreeAndNil(FManagers);
  FDFreeAndNil(FCallbacks);
end;

{-------------------------------------------------------------------------------}
function TFDPhysDataSnapEventAlerter.GetDSConn: TFDPhysDataSnapConnection;
begin
  Result := TFDPhysDataSnapConnection(ConnectionObj);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysDataSnapEventAlerter.InternalAllocHandle;
var
  oDef: IFDStanConnectionDef;
  oNames: TStrings;
  i: Integer;
  oMan: TDSClientCallbackChannelManager;
  sCBName: String;
  sChnlName: String;

  function Def(const AValue, ADef: String): String;
  begin
    if AValue = '' then
      Result := ADef
    else
      Result := AValue;
  end;

  function GenSessionId: String;
  begin
    Result := TDSSessionHelper.GenerateSessionId;
  end;

begin
  oDef := DSConn.ConnectionDef;
  oNames := GetNames;

  for i := 0 to oNames.Count - 1 do begin
    oMan := TDSClientCallbackChannelManager.Create(nil);
    FManagers.Add(oMan);

    oMan.CommunicationProtocol := Def(oDef.AsString[S_FD_ConnParam_DataSnap_Protocol], S_FD_TCPIP);
    oMan.DSHostname := Def(oDef.Server, '127.0.0.1');
    if CompareText(oMan.CommunicationProtocol, S_FD_TCPIP) = 0 then
      oMan.DSPort := Def(oDef.AsString[S_FD_ConnParam_Common_Port], '211')
    else
      oMan.DSPort := Def(oDef.AsString[S_FD_ConnParam_Common_Port], '8080');
    oMan.DSPath := oDef.AsString[TDBXPropertyNames.URLPath];
    oMan.UserName := oDef.UserName;
    oMan.Password := oDef.Password;
    oMan.ProxyHost := oDef.AsString[TDBXPropertyNames.DSProxyHost];
    oMan.ProxyPort := oDef.AsInteger[TDBXPropertyNames.DSProxyPort];
    oMan.ProxyUsername := oDef.AsString[TDBXPropertyNames.DSProxyUsername];
    oMan.ProxyPassword := oDef.AsString[TDBXPropertyNames.DSProxyPassword];
    oMan.ConnectionTimeout := oDef.AsString[S_FD_ConnParam_Common_LoginTimeout];
    oMan.CommunicationTimeout := oDef.AsString[TDBXPropertyNames.CommunicationTimeout];
    oMan.ManagerId := GenSessionId;

    sChnlName := FDNameFromIndex(oNames, i);
    if sChnlName = '' then
      sChnlName := GenSessionId;
    oMan.ChannelName := sChnlName;

    sCBName := FDValueFromIndex(oNames, i);
    if sCBName = '' then
      sCBName := GenSessionId;
    FCallbacks.Add(sCBName);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysDataSnapEventAlerter.InternalHandle(
  AEventMessage: TFDPhysEventMessage);
begin
  if GetHandler = nil then
    Exit;
  GetHandler.HandleEvent(TFDPhysDataSnapEventMessage(AEventMessage).FCBName,
                         TFDPhysDataSnapEventMessage(AEventMessage).FArgs);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysDataSnapEventAlerter.InternalRegister;
var
  i: Integer;
  oCB: TFDDataSnapCallback;
begin
  for i := 0 to FManagers.Count - 1 do begin
    oCB := TFDDataSnapCallback.Crate(Self, FCallbacks[i]);
    TDSClientCallbackChannelManager(FManagers[i]).RegisterCallback(oCB.FName, oCB);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysDataSnapEventAlerter.InternalUnregister;
var
  i: Integer;
begin
  for i := 0 to FManagers.Count - 1 do
    TDSClientCallbackChannelManager(FManagers[i]).UnregisterCallback(FCallbacks[i]);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysDataSnapEventAlerter.InternalReleaseHandle;
var
  i: Integer;
begin
  for i := 0 to FManagers.Count - 1 do
    FDFree(TDSClientCallbackChannelManager(FManagers[i]));
  FManagers.Clear;
  FCallbacks.Clear;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysDataSnapEventAlerter.InternalSignal(const AEvent: String;
  const AArgument: Variant);
var
  iChnl, i: Integer;
  oClnt: TDSAdminClient;
  oArr: TJSONArray;
  oMsg: TJSONValue;
  oRsp: TJSONValue;
begin
  iChnl := GetNames.IndexOfName(AEvent);
  if iChnl = -1 then
    iChnl := GetNames.IndexOf(AEvent);

  if VarIsArray(AArgument) then begin
    oArr := TJSONArray.Create;
    for i := VarArrayLowBound(AArgument, 1) to VarArrayHighBound(AArgument, 1) do
      oArr.Add(VarToStrDef(AArgument[i], ''));
    oMsg := oArr;
  end
  else
    oMsg := TJSONString.Create(VarToStrDef(AArgument, ''));

  oClnt := TDSAdminClient.Create(DSConn.DbxConnection);
  try
    if iChnl <> -1 then
      oClnt.BroadcastToChannel(AEvent, oMsg)
    else
      oClnt.NotifyCallback(AArgument[0], AEvent, oMsg, oRsp);
  finally
    FDFree(oClnt);
  end;
end;

{-------------------------------------------------------------------------------}
{ TFDPhysDataSnapCommand                                                        }
{-------------------------------------------------------------------------------}
function TFDPhysDataSnapCommand.GetDSConn: TFDPhysDataSnapConnection;
begin
  Result := TFDPhysDataSnapConnection(TDBXConn);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysDataSnapCommand.InternalPrepare;
begin
  try
    inherited InternalPrepare;
  except
    on E: Exception do
      if not DSConn.CheckNetworkError(E, Self) then
        raise;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysDataSnapCommand.InternalExecute(ATimes, AOffset: Integer;
  var ACount: TFDCounter);
begin
  try
    inherited InternalExecute(ATimes, AOffset, ACount);
  except
    on E: Exception do
      if not DSConn.CheckNetworkError(E, Self) then
        raise;
  end;
end;

{-------------------------------------------------------------------------------}
function TFDPhysDataSnapCommand.InternalOpen: Boolean;
begin
  Result := False;
  try
    Result := inherited InternalOpen;
  except
    on E: Exception do
      if not DSConn.CheckNetworkError(E, Self) then
        raise;
  end;
end;

{-------------------------------------------------------------------------------}
function TFDPhysDataSnapCommand.InternalNextRecordSet: Boolean;
begin
  Result := False;
  try
    Result := inherited InternalNextRecordSet;
  except
    on E: Exception do
      if not DSConn.CheckNetworkError(E, Self) then
        raise;
  end;
end;

{-------------------------------------------------------------------------------}
initialization
  FDPhysManager();
  FDPhysManagerObj.RegisterDriverClass(TFDPhysDataSnapDriver);

finalization
//  FDPhysManagerObj.UnRegisterDriverClass(TFDPhysDataSnapDriver);

end.
