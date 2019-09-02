{*******************************************************}
{                                                       }
{               Delphi FireDAC Framework                }
{          FireDAC Sybase SQL Anywhere driver           }
{                                                       }
{ Copyright(c) 2004-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}
{$I FireDAC.inc}
{$IFDEF WIN32}
  {$HPPEMIT '#pragma link "FireDAC.Phys.ASA.obj"'}
{$ELSE}
  {$HPPEMIT '#pragma link "FireDAC.Phys.ASA.o"'}
{$ENDIF}

unit FireDAC.Phys.ASA;

interface

uses
  System.Classes,
  FireDAC.Stan.Intf,
  FireDAC.Phys, FireDAC.Phys.ODBCWrapper, FireDAC.Phys.ODBCBase, FireDAC.Phys.ASAWrapper;

type
  TFDPhysASADriverLink = class;
{$IFDEF MSWINDOWS}
  TFDASAService = class;
  TFDASABackup = class;
  TFDASAValidate = class;
{$ENDIF}

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32)]
  TFDPhysASADriverLink = class(TFDPhysODBCBaseDriverLink)
  private
    FToolLib: String;
  protected
    function GetBaseDriverID: String; override;
    function IsConfigured: Boolean; override;
    procedure ApplyTo(const AParams: IFDStanDefinition); override;
  published
    property ToolLib: String read FToolLib write FToolLib;
  end;

{$IFDEF MSWINDOWS}
  TFDASAProgressEvent = type TASAToolMessageEvent;

  TFDASAService = class (TFDPhysODBCBaseService)
  private
    FOnProgress: TFDASAProgressEvent;
    function GetDriverLink: TFDPhysASADriverLink;
    procedure SetDriverLink(const AValue: TFDPhysASADriverLink);
    function GetToolLib: TASAToolLib;
  protected
    function GetEnv: TODBCEnvironment; override;
    procedure CheckActive(AAutoActivate, ANeedActivation: Boolean); override;
  public
    property ToolLib: TASAToolLib read GetToolLib;
  published
    property DriverLink: TFDPhysASADriverLink read GetDriverLink write SetDriverLink;
    property OnProgress: TFDASAProgressEvent read FOnProgress write FOnProgress;
  end;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  TFDASABackup = class (TFDASAService)
  private
    FConnectParams: String;
    FCheckpointLogType: TASABackupCheckpointLogType;
    FPageBlocksize: Longword;
    FHotlogFilename: String;
    FOutputDir: String;
    FFlags: TASABackupFlags;
    FStartLine: String;
  protected
    procedure InternalExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Backup;
  published
    property ConnectParams: String read FConnectParams write FConnectParams;
    property StartLine: String read FStartLine write FStartLine;
    property OutputDir: String read FOutputDir write FOutputDir;
    property HotlogFilename: String read FHotlogFilename write FHotlogFilename;
    property CheckpointLogType: TASABackupCheckpointLogType read FCheckpointLogType
      write FCheckpointLogType default bclDefault;
    property PageBlocksize: Longword read FPageBlocksize write FPageBlocksize
      default 0;
    property Flags: TASABackupFlags read FFlags write FFlags default [];
  end;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  TFDASAValidate = class (TFDASAService)
  private
    FConnectParams: String;
    FStartLine: String;
    FValidateType: TASAValidateType;
    FTables: TStrings;
    FFlags: TASAValidateFlags;
    procedure SetTables(const AValue: TStrings);
  protected
    procedure InternalExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Validate;
  published
    property ConnectParams: String read FConnectParams write FConnectParams;
    property StartLine: String read FStartLine write FStartLine;
    property Tables: TStrings read FTables write SetTables;
    property Flags: TASAValidateFlags read FFlags write FFlags default [];
    property ValidateType: TASAValidateType read FValidateType write FValidateType default vtNormal;
  end;
{$ENDIF}

{-------------------------------------------------------------------------------}
implementation

uses
  System.SysUtils, System.StrUtils, System.Variants,
  FireDAC.Stan.Consts, FireDAC.Stan.Error, FireDAC.Stan.Util, FireDAC.Stan.Factory,
  FireDAC.Phys.Intf, FireDAC.Phys.SQLGenerator, FireDAC.Phys.ODBCCli, FireDAC.Phys.ASACli, 
    FireDAC.Phys.ASAMeta;

type
  TFDPhysASADriver = class;
  TFDPhysASAConnection = class;
  TFDPhysASAEventAlerter = class;

  TFDPhysASACliHandles = array [0..1] of Pointer;
  PFDPhysASACliHandles = ^TFDPhysODBCCliHandles;

  TFDPhysASADriver = class(TFDPhysODBCDriverBase)
  private
    FToolLib: TASAToolLib;
    FCliObj: TFDPhysASACliHandles;
  protected
    class function GetBaseDriverID: String; override;
    procedure InternalLoad; override;
    procedure InternalUnload; override;
    function InternalCreateConnection(AConnHost: TFDPhysConnectionHost): TFDPhysConnection; override;
    function GetCliObj: Pointer; override;
    procedure GetODBCConnectStringKeywords(AKeywords: TStrings); override;
    function GetConnParamCount(AKeys: TStrings): Integer; override;
    procedure GetConnParams(AKeys: TStrings; AIndex: Integer; var AName, AType,
      ADefVal, ACaption: String; var ALoginIndex: Integer); override;
  public
    constructor Create(AManager: TFDPhysManager; const ADriverDef: IFDStanDefinition); override;
    destructor Destroy; override;
  end;

  TFDPhysASAConnection = class (TFDPhysODBCConnectionBase)
  private
{$IFDEF MSWINDOWS}
    FMsgCbkThunk: TFDMethodThunk;
    procedure DoMsgCallback(SQLCA: Pointer; msg_type: byte; code: LongWord;
      len: Word; msg: PChar); stdcall;
{$ENDIF}
    procedure CheckPasswordChange;
  protected
    function InternalCreateEvent(const AEventKind: String): TFDPhysEventAlerter; override;
    function InternalCreateCommandGenerator(const ACommand:
      IFDPhysCommand): TFDPhysCommandGenerator; override;
    function InternalCreateMetadata: TObject; override;
    procedure GetStrsMaxSizes(AStrDataType: SQLSmallint; AFixedLen: Boolean;
      out ACharSize, AByteSize: Integer); override;
    function GetExceptionClass: EODBCNativeExceptionClass; override;
    procedure SetupConnection; override;
    procedure InternalConnect; override;
    procedure InternalDisconnect; override;
    procedure InternalChangePassword(const AUserName, AOldPassword,
      ANewPassword: String); override;
  public
    constructor Create(ADriverObj: TFDPhysDriver; AConnHost: TFDPhysConnectionHost); override;
    destructor Destroy; override;
  end;

  TFDPhysASAEventAlerter = class (TFDPhysEventAlerter)
  private
{$IFDEF MSWINDOWS}
    function ParseMessage(const AMessage: String): Boolean;
{$ENDIF}
  protected
    procedure InternalHandle(AEventMessage: TFDPhysEventMessage); override;
    procedure InternalSignal(const AEvent: String; const AArgument: Variant); override;
  end;

{-------------------------------------------------------------------------------}
{ TFDPhysASADriverLink                                                          }
{-------------------------------------------------------------------------------}
function TFDPhysASADriverLink.GetBaseDriverID: String;
begin
  Result := S_FD_ASAId;
end;

{-------------------------------------------------------------------------------}
function TFDPhysASADriverLink.IsConfigured: Boolean;
begin
  Result := inherited IsConfigured or (FToolLib <> '');
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysASADriverLink.ApplyTo(const AParams: IFDStanDefinition);
begin
  inherited ApplyTo(AParams);
  if FToolLib <> '' then
    AParams.AsString[S_FD_ConnParam_ASA_ToolLib] := FToolLib;
end;

{-------------------------------------------------------------------------------}
{ TFDASAService                                                                 }
{-------------------------------------------------------------------------------}
{$IFDEF MSWINDOWS}
function TFDASAService.GetDriverLink: TFDPhysASADriverLink;
begin
  Result := inherited DriverLink as TFDPhysASADriverLink;
end;

{-------------------------------------------------------------------------------}
procedure TFDASAService.SetDriverLink(const AValue: TFDPhysASADriverLink);
begin
  inherited DriverLink := AValue;
end;

{-------------------------------------------------------------------------------}
function TFDASAService.GetEnv: TODBCEnvironment;
begin
  Result := TODBCEnvironment(PFDPhysASACliHandles(CliObj)^[0]);
end;

{-------------------------------------------------------------------------------}
function TFDASAService.GetToolLib: TASAToolLib;
begin
  Result := TASAToolLib(PFDPhysASACliHandles(CliObj)^[1]);
end;

{-------------------------------------------------------------------------------}
procedure TFDASAService.CheckActive(AAutoActivate, ANeedActivation: Boolean);
begin
  inherited CheckActive(AAutoActivate, ANeedActivation);
  if not Assigned(ToolLib.DBToolsInit) then
    FDException(Self, [S_FD_LPhys, DriverLink.ActualDriverID], er_FD_ASADBToolNotFound, []);
end;

{-------------------------------------------------------------------------------}
{ TFDASABackup                                                                  }
{-------------------------------------------------------------------------------}
constructor TFDASABackup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCheckpointLogType := bclDefault;
end;

{-------------------------------------------------------------------------------}
procedure TFDASABackup.InternalExecute;
var
  oBckp: TASABackup;
begin
  oBckp := TASABackup.Create(ToolLib, Self);
  try
    oBckp.ConnectParams := ConnectParams;
    oBckp.StartLine := StartLine;
    oBckp.OutputDir := OutputDir;
    oBckp.HotlogFilename := HotlogFilename;
    oBckp.CheckpointLogType := CheckpointLogType;
    oBckp.PageBlocksize := PageBlocksize;
    oBckp.Flags := Flags;
    oBckp.OnMessage := OnProgress;
    oBckp.Backup;
  finally
    FDFree(oBckp);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDASABackup.Backup;
begin
  Execute;
end;

{-------------------------------------------------------------------------------}
{ TFDASAValidate                                                                }
{-------------------------------------------------------------------------------}
constructor TFDASAValidate.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTables := TFDStringList.Create;
end;

{-------------------------------------------------------------------------------}
destructor TFDASAValidate.Destroy;
begin
  FDFreeAndNil(FTables);
  inherited Destroy;
end;

{-------------------------------------------------------------------------------}
procedure TFDASAValidate.SetTables(const AValue: TStrings);
begin
  FTables.Assign(AValue);
end;

{-------------------------------------------------------------------------------}
procedure TFDASAValidate.InternalExecute;
var
  oVldt: TASAValidate;
begin
  oVldt := TASAValidate.Create(ToolLib, Self);
  try
    oVldt.ConnectParams := ConnectParams;
    oVldt.StartLine := StartLine;
    oVldt.Tables := Tables;
    oVldt.Flags := Flags;
    oVldt.ValidateType := ValidateType;
    oVldt.OnMessage := OnProgress;
    oVldt.Validate;
  finally
    FDFree(oVldt);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDASAValidate.Validate;
begin
  Execute;
end;
{$ENDIF}

{-------------------------------------------------------------------------------}
{ TFDPhysASADriver                                                              }
{-------------------------------------------------------------------------------}
constructor TFDPhysASADriver.Create(AManager: TFDPhysManager;
  const ADriverDef: IFDStanDefinition);
begin
  inherited Create(AManager, ADriverDef);
  FToolLib := TASAToolLib.Create(Self);
end;

{-------------------------------------------------------------------------------}
destructor TFDPhysASADriver.Destroy;
begin
  inherited Destroy;
  FDFreeAndNil(FToolLib);
end;

{-------------------------------------------------------------------------------}
class function TFDPhysASADriver.GetBaseDriverID: String;
begin
  Result := S_FD_ASAId;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysASADriver.InternalLoad;
var
  sToolLib: String;
begin
  ODBCAdvanced := 'CommLinks=ShMem';
  inherited InternalLoad;
  if ODBCDriver = '' then
    ODBCDriver := FindBestDriver(
      {$IFDEF MSWINDOWS} ['SQL Anywhere 16', 'SQL Anywhere 12', 'SQL Anywhere 11',
      'SQL Anywhere 10', 'Adaptive Server Anywhere 9.%', 'Adaptive Server Anywhere 8.%',
      'Adaptive Server Anywhere 7.%', 'Sybase SQL Anywhere %'] {$ENDIF}
      {$IFDEF POSIX} ['SQL Anywhere%', 'SQLAnywhere%'], C_SQLAnywhere12Lib {$ENDIF});
  if Params <> nil then
    sToolLib := Params.AsXString[S_FD_ConnParam_ASA_ToolLib]
  else
    sToolLib := '';
  FToolLib.Load(sToolLib);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysASADriver.InternalUnload;
begin
  FToolLib.Unload;
  inherited InternalUnload;
end;

{-------------------------------------------------------------------------------}
function TFDPhysASADriver.InternalCreateConnection(
  AConnHost: TFDPhysConnectionHost): TFDPhysConnection;
begin
  Result := TFDPhysASAConnection.Create(Self, AConnHost);
end;

{-------------------------------------------------------------------------------}
function TFDPhysASADriver.GetCliObj: Pointer;
begin
  FCliObj[0] := ODBCEnvironment;
  FCliObj[1] := FToolLib;
  Result := @FCliObj[0];
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysASADriver.GetODBCConnectStringKeywords(AKeywords: TStrings);
var
  i: Integer;
begin
  inherited GetODBCConnectStringKeywords(AKeywords);
  i := AKeywords.IndexOfName(S_FD_ConnParam_Common_Password);
  if i <> -1 then
    AKeywords[i] := S_FD_ConnParam_Common_Password + '=PWD*';
  AKeywords.Add(S_FD_ConnParam_Common_Server + '=EngineName');
  AKeywords.Add(S_FD_ConnParam_Common_Database + '=DatabaseName');
  AKeywords.Add(S_FD_ConnParam_ASA_DatabaseFile + '=' +
    S_FD_ConnParam_ASA_DatabaseFile + '*');
  AKeywords.Add(S_FD_ConnParam_Common_OSAuthent + '=Integrated');
  AKeywords.Add(S_FD_ConnParam_ASA_Compress);
  AKeywords.Add(S_FD_ConnParam_ASA_Encrypt + '=Encryption');
  AKeywords.Add(S_FD_ConnParam_Common_ApplicationName + '=AppInfo');
  AKeywords.Add('=COMMLINKS');
  AKeywords.Add(S_FD_ConnParam_Common_NewPassword + '=NEWPWD*');
end;

{-------------------------------------------------------------------------------}
function TFDPhysASADriver.GetConnParamCount(AKeys: TStrings): Integer;
begin
  Result := inherited GetConnParamCount(AKeys) + 10;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysASADriver.GetConnParams(AKeys: TStrings; AIndex: Integer;
  var AName, AType, ADefVal, ACaption: String; var ALoginIndex: Integer);
begin
  ALoginIndex := -1;
  ADefVal := '';
  if AIndex < inherited GetConnParamCount(AKeys) then begin
    inherited GetConnParams(AKeys, AIndex, AName, AType, ADefVal, ACaption, ALoginIndex);
    if AName = S_FD_ConnParam_Common_Database then
      ALoginIndex := 4;
  end
  else begin
    case AIndex - inherited GetConnParamCount(AKeys) of
    0:
      begin
        AName := S_FD_ConnParam_Common_Server;
        AType := '@S';
        ALoginIndex := 3;
      end;
    1:
      begin
        AName := S_FD_ConnParam_ASA_DatabaseFile;
        AType := '@F:SQL Anywhere Database|*.db';
      end;
    2:
      begin
        AName := S_FD_ConnParam_Common_OSAuthent;
        AType := '@Y';
        ALoginIndex := 2;
      end;
    3:
      begin
        AName := S_FD_ConnParam_ASA_Compress;
        AType := '@Y';
      end;
    4:
      begin
        AName := S_FD_ConnParam_ASA_Encrypt;
        AType := '@S';
      end;
    5:
      begin
        AName := S_FD_ConnParam_Common_ApplicationName;
        AType := '@S';
      end;
    6:
      begin
        AName := S_FD_ConnParam_Common_MetaDefCatalog;
        AType := '@S';
      end;
    7:
      begin
        AName := S_FD_ConnParam_Common_MetaDefSchema;
        AType := '@S';
      end;
    8:
      begin
        AName := S_FD_ConnParam_Common_MetaCurCatalog;
        AType := '@S';
      end;
    9:
      begin
        AName := S_FD_ConnParam_Common_MetaCurSchema;
        AType := '@S';
      end;
    end;
    ACaption := AName;
  end;
end;

{-------------------------------------------------------------------------------}
{ TFDPhysASAConnection                                                          }
{-------------------------------------------------------------------------------}
constructor TFDPhysASAConnection.Create(ADriverObj: TFDPhysDriver;
  AConnHost: TFDPhysConnectionHost);
begin
  inherited Create(ADriverObj, AConnHost);
{$IFDEF MSWINDOWS}
  FMsgCbkThunk := TFDMethodThunk.Create(Self, @TFDPhysASAConnection.DoMsgCallback);
{$ENDIF}
end;

{-------------------------------------------------------------------------------}
destructor TFDPhysASAConnection.Destroy;
begin
  inherited Destroy;
{$IFDEF MSWINDOWS}
  FDFreeAndNil(FMsgCbkThunk);
{$ENDIF}
end;

{-------------------------------------------------------------------------------}
function TFDPhysASAConnection.InternalCreateEvent(
  const AEventKind: String): TFDPhysEventAlerter;
begin
  if CompareText(AEventKind, S_FD_EventKind_ASA_Events) = 0 then
    Result := TFDPhysASAEventAlerter.Create(Self, AEventKind)
  else
    Result := nil;
end;

{-------------------------------------------------------------------------------}
function TFDPhysASAConnection.InternalCreateCommandGenerator(
  const ACommand: IFDPhysCommand): TFDPhysCommandGenerator;
begin
  if ACommand <> nil then
    Result := TFDPhysASACommandGenerator.Create(ACommand)
  else
    Result := TFDPhysASACommandGenerator.Create(Self);
end;

{-------------------------------------------------------------------------------}
function TFDPhysASAConnection.InternalCreateMetadata: TObject;
var
  iSrvVer, iClntVer: TFDVersion;
begin
  GetVersions(iSrvVer, iClntVer);
  Result := TFDPhysASAMetadata.Create(Self, iSrvVer, iClntVer, GetKeywords);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysASAConnection.GetStrsMaxSizes(AStrDataType: SQLSmallint;
  AFixedLen: Boolean; out ACharSize, AByteSize: Integer);
begin
  AByteSize := 32766;
  ACharSize := AByteSize;
  case AStrDataType of
  SQL_C_CHAR, SQL_C_BINARY:
    ;
  SQL_C_WCHAR:
    ACharSize := AByteSize div SizeOf(SQLWChar);
  else
    FDCapabilityNotSupported(Self, [S_FD_LPhys, S_FD_ASAId]);
  end;
end;

{-------------------------------------------------------------------------------}
function TFDPhysASAConnection.GetExceptionClass: EODBCNativeExceptionClass;
begin
  Result := EASANativeException;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysASAConnection.CheckPasswordChange;
var
  oConnMeta: IFDPhysConnectionMetadata;
begin
  CreateMetadata(oConnMeta);
  if oConnMeta.ServerVersion < cvSybaseASA11 then
    FDCapabilityNotSupported(Self, [S_FD_LPhys, S_FD_ASAId]);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysASAConnection.SetupConnection;
begin
  if ConnectionDef.NewPassword <> '' then
    CheckPasswordChange;
  inherited SetupConnection;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysASAConnection.InternalConnect;
begin
  inherited InternalConnect;
{$IFDEF MSWINDOWS}
  if FDVerStr2Int(ODBCConnection.DRIVER_VER) >= cvSybaseASA9 then
    ODBCConnection.REGISTER_MESSAGE_CALLBACK := FMsgCbkThunk.CallAddress;
{$ENDIF}
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysASAConnection.InternalDisconnect;
begin
{$IFDEF MSWINDOWS}
  if ODBCConnection.Connected and (FDVerStr2Int(ODBCConnection.DRIVER_VER) >= cvSybaseASA9) then
    ODBCConnection.REGISTER_MESSAGE_CALLBACK := nil;
{$ENDIF}
  inherited InternalDisconnect;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysASAConnection.InternalChangePassword(const AUserName,
  AOldPassword, ANewPassword: String);
var
  oConnDef: IFDStanConnectionDef;
  oConn: TODBCConnection;
begin
  CheckPasswordChange;
  FDCreateInterface(IFDStanConnectionDef, oConnDef);
  oConnDef.ParentDefinition := ConnectionDef;
  oConnDef.UserName := AUserName;
  oConnDef.NewPassword := ANewPassword;
  oConnDef.Password := AOldPassword;
  oConn := TODBCConnection.Create(ODBCEnvironment, Self);
  try
    oConn.Connect(TFDPhysODBCDriverBase(DriverObj).BuildODBCConnectString(oConnDef), False);
  finally
    FDFree(oConn);
  end;
end;

{-------------------------------------------------------------------------------}
{$IFDEF MSWINDOWS}
procedure TFDPhysASAConnection.DoMsgCallback(SQLCA: Pointer; msg_type: byte;
  code: LongWord; len: Word; msg: PChar); stdcall;
var
  sMsg: String;
  lParse: Boolean;
  i: Integer;
begin
  SetString(sMsg, msg, len);
  lParse := False;
  for i := 0 to GetEventCount - 1 do
    lParse := lParse or TFDPhysASAEventAlerter(FEventList[i]).ParseMessage(sMsg);
  if not lParse then begin
    if ODBCConnection.Info = nil then
      ODBCConnection.Info := EASANativeException.Create(SQL_SUCCESS, ODBCConnection);
    EASANativeException(ODBCConnection.Info).AppendError(ODBCConnection,
      SQLSmallint(ODBCConnection.Info.ErrorCount + 1), '', code, sMsg, sMsg,
      '', '', ekServerOutput, 0, -1);
  end;
end;
{$ENDIF}

{-------------------------------------------------------------------------------}
{ TFDPhysASAEventMessage                                                        }
{-------------------------------------------------------------------------------}
type
  TFDPhysASAEventMessage = class(TFDPhysEventMessage)
  private
    FEvent,
    FMessage: String;
  public
    constructor Create(const AEvent, AMessage: String);
  end;

{-------------------------------------------------------------------------------}
constructor TFDPhysASAEventMessage.Create(const AEvent, AMessage: String);
begin
  inherited Create;
  FEvent := AEvent;
  FMessage := AMessage;
end;

{-------------------------------------------------------------------------------}
{ TFDPhysASAEventAlerter                                                        }
{-------------------------------------------------------------------------------}
const
  C_Delim = '$$';

{$IFDEF MSWINDOWS}
function TFDPhysASAEventAlerter.ParseMessage(const AMessage: String): Boolean;
var
  sEvent, sMsg: String;
  i1, i2: Integer;
begin
  if Pos(C_FD_SysNamePrefix + C_Delim, AMessage) = 1 then begin
    i1 := Length(C_FD_SysNamePrefix) + Length(C_Delim) + 1;
    i2 := FDPosEx(C_Delim, AMessage, i1);
    if i2 = 0 then
      i2 := Length(AMessage) + 1;
    sEvent := Copy(AMessage, i1, i2 - i1);
    sMsg := Copy(AMessage, i2, Length(AMessage) - i2);
    if GetNames.IndexOf(sEvent) >= 0 then
      FMsgThread.EnqueueMsg(TFDPhysASAEventMessage.Create(sEvent, sMsg));
    Result := True;
  end
  else
    Result := False;
end;
{$ENDIF}

{-------------------------------------------------------------------------------}
procedure TFDPhysASAEventAlerter.InternalHandle(AEventMessage: TFDPhysEventMessage);
var
  oMsg: TFDPhysASAEventMessage;
begin
  if GetHandler <> nil then begin
    oMsg := TFDPhysASAEventMessage(AEventMessage);
    GetHandler.HandleEvent(oMsg.FEvent, oMsg.FMessage);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysASAEventAlerter.InternalSignal(const AEvent: String;
  const AArgument: Variant);
var
  s: String;
  oCmd: IFDPhysCommand;
begin
  // _FD_$$event$$argument
  s := C_FD_SysNamePrefix + C_Delim + AEvent;
  if not (VarIsNull(AArgument) or VarIsEmpty(AArgument)) then
    s := s + C_Delim + VarToStr(AArgument);
  GetConnection.CreateCommand(oCmd);
  SetupCommand(oCmd);
  oCmd.Prepare('MESSAGE ' + QuotedStr(s) + ' TYPE INFO TO CLIENT FOR ALL');
  oCmd.Execute();
end;

{-------------------------------------------------------------------------------}
initialization
  FDPhysManager();
  FDPhysManagerObj.RegisterDriverClass(TFDPhysASADriver);

end.
