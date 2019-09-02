{*******************************************************}
{                                                       }
{               Delphi FireDAC Framework                }
{            FireDAC GUIx layer base classes            }
{                                                       }
{ Copyright(c) 2004-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}
{$I FireDAC.inc}

unit FireDAC.UI;

interface

uses
  System.Classes,
  FireDAC.Stan.Factory, FireDAC.Stan.Intf, FireDAC.Stan.Util, FireDAC.Stan.Consts,
  FireDAC.UI.Intf;

type
  TFDGUIxObject = class(TFDObject)
  protected
    class function IsFactoryEnabled(const AMyProvider, AReqProvider: String): Boolean; override;
  end;

  PFDLoginItem = ^TFDLoginItem;
  TFDLoginItem = record
    FParam: String;
    FType: String;
    FCaption: String;
    FOrder: Integer;
    FValue: String;
  end;

  TFDGUIxLoginDialogImplBase = class(TFDGUIxObject, IFDGUIxLoginDialog)
  private
    FOnHide: TNotifyEvent;
    FOnShow: TNotifyEvent;
    FLoginRetries: Integer;
    FOnChangePassword: TFDGUIxLoginDialogEvent;
    FOnLogin: TFDGUIxLoginDialogEvent;
    FChangeExpiredPassword: Boolean;
  protected
    FCaption: String;
    FConnectionDef: IFDStanConnectionDef;
    FHistoryEnabled: Boolean;
    FHistoryKey: String;
    FHistoryStorage: TFDGUIxLoginHistoryStorage;
    FHistoryWithPassword: Boolean;
    FVisibleItems: TStrings;
    // IFDGUIxLoginDialog
    function GetCaption: String;
    function GetChangeExpiredPassword: Boolean;
    function GetHistoryEnabled: Boolean;
    function GetHistoryKey: String;
    function GetHistoryStorage: TFDGUIxLoginHistoryStorage;
    function GetHistoryWithPassword: Boolean;
    function GetLoginRetries: Integer;
    function GetVisibleItems: TStrings;
    function GetConnectionDef: IFDStanConnectionDef;
    function GetOnHide: TNotifyEvent;
    function GetOnShow: TNotifyEvent;
    function GetOnChangePassword: TFDGUIxLoginDialogEvent;
    function GetOnLogin: TFDGUIxLoginDialogEvent;
    procedure SetCaption(const AValue: String);
    procedure SetChangeExpiredPassword(const AValue: Boolean);
    procedure SetHistoryEnabled(const AValue: Boolean);
    procedure SetHistoryKey(const AValue: String);
    procedure SetHistoryStorage(const AValue: TFDGUIxLoginHistoryStorage);
    procedure SetHistoryWithPassword(const AValue: Boolean);
    procedure SetLoginRetries(const AValue: Integer);
    procedure SetVisibleItems(const AValue: TStrings);
    procedure SetConnectionDef(const AValue: IFDStanConnectionDef);
    procedure SetOnHide(const AValue: TNotifyEvent);
    procedure SetOnShow(const AValue: TNotifyEvent);
    procedure SetOnChangePassword(const AValue: TFDGUIxLoginDialogEvent);
    procedure SetOnLogin(const AValue: TFDGUIxLoginDialogEvent);
    function Execute(ALoginAction: TFDGUIxLoginAction): Boolean;
    procedure GetAllLoginParams;
    // Forms & FMX
    function ExecuteLogin: Boolean; virtual; abstract;
    function ExecuteChngPwd: Boolean; virtual; abstract;
  public
    procedure Initialize; override;
    destructor Destroy; override;
  end;

  TFDGUIxScriptImplBase = class(TFDGUIxObject, IFDGUIxScriptDialog)
  protected
    FOnShow: TNotifyEvent;
    FOnHide: TNotifyEvent;
    FOnProgress: TFDGUIxScriptProgressEvent;
    FOnOutput: TFDGUIxScriptOutputEvent;
    FOnInput: TFDGUIxScriptInputEvent;
    FOnPause: TFDGUIxScriptPauseEvent;
    FCaption: String;
    FOptions: TFDGUIxScriptOptions;
    // IFDGUIxScriptDialog
    function GetOnShow: TNotifyEvent;
    procedure SetOnShow(const AValue: TNotifyEvent);
    function GetOnHide: TNotifyEvent;
    procedure SetOnHide(const AValue: TNotifyEvent);
    function GetOnProgress: TFDGUIxScriptProgressEvent;
    procedure SetOnProgress(const AValue: TFDGUIxScriptProgressEvent);
    function GetOnOutput: TFDGUIxScriptOutputEvent;
    procedure SetOnOutput(const AValue: TFDGUIxScriptOutputEvent);
    function GetOnInput: TFDGUIxScriptInputEvent;
    procedure SetOnInput(const AValue: TFDGUIxScriptInputEvent);
    function GetOnPause: TFDGUIxScriptPauseEvent;
    procedure SetOnPause(const AValue: TFDGUIxScriptPauseEvent);
    function GetCaption: String;
    procedure SetCaption(const AValue: String);
    function GetOptions: TFDGUIxScriptOptions;
    procedure SetOptions(AValue: TFDGUIxScriptOptions);
    procedure Show; virtual; abstract;
    procedure Progress(const AInfoProvider: IFDGUIxScriptDialogInfoProvider); virtual; abstract;
    procedure Output(const AStr: String; AKind: TFDScriptOuputKind); virtual; abstract;
    procedure Input(const APrompt: String; var AResult: String); virtual; abstract;
    procedure Pause(const APrompt: String); virtual; abstract;
    procedure Hide; virtual; abstract;
  end;

  TFDGUIxVisualScriptImplBase = class(TFDGUIxScriptImplBase)
  public
    procedure Initialize; override;
  end;

  TFDGUIxWaitCursorImplBase = class(TFDGUIxObject, IFDGUIxWaitCursor)
  protected
    FWaitCursor: TFDGUIxScreenCursor;
    FOnShow: TNotifyEvent;
    FOnHide: TNotifyEvent;
    // IFDGUIxWaitCursor
    procedure StartWait; virtual; abstract;
    procedure StopWait; virtual; abstract;
    procedure PushWait; virtual; abstract;
    procedure PopWait; virtual; abstract;
    procedure ForceStopWait; virtual; abstract;
    function GetWaitCursor: TFDGUIxScreenCursor;
    procedure SetWaitCursor(const AValue: TFDGUIxScreenCursor);
    function GetOnShow: TNotifyEvent;
    procedure SetOnShow(const AValue: TNotifyEvent);
    function GetOnHide: TNotifyEvent;
    procedure SetOnHide(const AValue: TNotifyEvent);
  end;

  TFDGUIxVisualWaitCursorImplBase = class(TFDGUIxWaitCursorImplBase)
  private
    FTimer: TFDTimer;
    FStopRequestTime: LongWord;
    FWasActive: Integer;
    function IsVisible: Boolean;
    procedure HideCursor;
    procedure ShowCursor;
    procedure TimerProc;
    procedure StopTimer;
    procedure StartTimer;
  protected
    function CheckCurCursor: Boolean; virtual; abstract;
    function InternalHideCursor: Boolean; virtual; abstract;
    procedure InternalShowCursor; virtual; abstract;
    // IFDGUIxWaitCursor
    procedure StartWait; override;
    procedure StopWait; override;
    procedure PushWait; override;
    procedure PopWait; override;
    procedure ForceStopWait; override;
  public
    procedure Initialize; override;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,
  FireDAC.Stan.ResStrs, FireDAC.Stan.Error,
  FireDAC.Phys.Intf;

var
  FWait: TFDGUIxWaitCursorImplBase;

{-------------------------------------------------------------------------------}
{ TFDGUIxObject                                                                 }
{-------------------------------------------------------------------------------}
class function TFDGUIxObject.IsFactoryEnabled(const AMyProvider,
  AReqProvider: String): Boolean;
begin
  if FDIsDesignTime then
    Result := CompareText(AMyProvider, C_FD_GUIxFormsProvider) = 0
  else if AReqProvider = '' then
    Result := CompareText(AMyProvider, FADGUIxProvider) = 0
  else
    Result := CompareText(AMyProvider, AReqProvider) = 0;
end;

{-------------------------------------------------------------------------------}
{ TFDGUIxLoginDialogImplBase                                                    }
{-------------------------------------------------------------------------------}
procedure TFDGUIxLoginDialogImplBase.Initialize;
begin
  inherited Initialize;
{$IFDEF MSWINDOWS}
  FHistoryStorage := hsRegistry;
  FHistoryKey := S_FD_CfgKeyName + '\' + S_FD_Profiles;
{$ENDIF}
{$IFDEF POSIX}
  FHistoryStorage := hsFile;
  FHistoryKey := S_FD_Profiles;
{$ENDIF}
  FHistoryWithPassword := True;
  FHistoryEnabled := False;
  FCaption := S_FD_LoginDialogDefCaption;
  FVisibleItems := TFDStringList.Create;
  FVisibleItems.Add(S_FD_ConnParam_Common_UserName);
  FVisibleItems.Add(S_FD_ConnParam_Common_Password);
  FLoginRetries := 3;
  FChangeExpiredPassword := True;
end;

{-------------------------------------------------------------------------------}
destructor TFDGUIxLoginDialogImplBase.Destroy;
begin
  FDFreeAndNil(FVisibleItems);
  inherited Destroy;
end;

{-------------------------------------------------------------------------------}
function TFDGUIxLoginDialogImplBase.GetCaption: String;
begin
  Result := FCaption;
end;

{-------------------------------------------------------------------------------}
function TFDGUIxLoginDialogImplBase.GetChangeExpiredPassword: Boolean;
begin
  Result := FChangeExpiredPassword;
end;

{-------------------------------------------------------------------------------}
function TFDGUIxLoginDialogImplBase.GetConnectionDef: IFDStanConnectionDef;
begin
  Result := FConnectionDef;
end;

{-------------------------------------------------------------------------------}
function TFDGUIxLoginDialogImplBase.GetHistoryEnabled: Boolean;
begin
  Result := FHistoryEnabled;
end;

{-------------------------------------------------------------------------------}
function TFDGUIxLoginDialogImplBase.GetHistoryKey: String;
begin
  Result := FHistoryKey;
end;

{-------------------------------------------------------------------------------}
function TFDGUIxLoginDialogImplBase.GetHistoryStorage: TFDGUIxLoginHistoryStorage;
begin
  Result := FHistoryStorage;
end;

{-------------------------------------------------------------------------------}
function TFDGUIxLoginDialogImplBase.GetHistoryWithPassword: Boolean;
begin
  Result := FHistoryWithPassword;
end;

{-------------------------------------------------------------------------------}
function TFDGUIxLoginDialogImplBase.GetLoginRetries: Integer;
begin
  Result := FLoginRetries;
end;

{-------------------------------------------------------------------------------}
function TFDGUIxLoginDialogImplBase.GetVisibleItems: TStrings;
begin
  Result := FVisibleItems;
end;

{-------------------------------------------------------------------------------}
function TFDGUIxLoginDialogImplBase.GetOnHide: TNotifyEvent;
begin
  Result := FOnHide;
end;

{-------------------------------------------------------------------------------}
function TFDGUIxLoginDialogImplBase.GetOnShow: TNotifyEvent;
begin
  Result := FOnShow;
end;

{-------------------------------------------------------------------------------}
function TFDGUIxLoginDialogImplBase.GetOnChangePassword: TFDGUIxLoginDialogEvent;
begin
  Result := FOnChangePassword;
end;

{-------------------------------------------------------------------------------}
function TFDGUIxLoginDialogImplBase.GetOnLogin: TFDGUIxLoginDialogEvent;
begin
  Result := FOnLogin;
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxLoginDialogImplBase.SetOnHide(const AValue: TNotifyEvent);
begin
  FOnHide := AValue;
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxLoginDialogImplBase.SetCaption(const AValue: String);
begin
  FCaption := AValue;
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxLoginDialogImplBase.SetChangeExpiredPassword(const AValue: Boolean);
begin
  FChangeExpiredPassword := AValue;
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxLoginDialogImplBase.SetConnectionDef(const AValue: IFDStanConnectionDef);
begin
  FConnectionDef := AValue;
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxLoginDialogImplBase.SetHistoryEnabled(const AValue: Boolean);
begin
  FHistoryEnabled := AValue;
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxLoginDialogImplBase.SetHistoryKey(const AValue: String);
begin
  FHistoryKey := AValue;
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxLoginDialogImplBase.SetHistoryStorage(const AValue: TFDGUIxLoginHistoryStorage);
begin
  FHistoryStorage := AValue;
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxLoginDialogImplBase.SetHistoryWithPassword(const AValue: Boolean);
begin
  FHistoryWithPassword := AValue;
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxLoginDialogImplBase.SetLoginRetries(const AValue: Integer);
begin
  FLoginRetries := AValue;
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxLoginDialogImplBase.SetVisibleItems(const AValue: TStrings);
begin
  FVisibleItems.Assign(AValue);
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxLoginDialogImplBase.SetOnShow(const AValue: TNotifyEvent);
begin
  FOnShow := AValue;
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxLoginDialogImplBase.SetOnChangePassword(const AValue: TFDGUIxLoginDialogEvent);
begin
  FOnChangePassword := AValue;
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxLoginDialogImplBase.SetOnLogin(const AValue: TFDGUIxLoginDialogEvent);
begin
  FOnLogin := AValue;
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxLoginDialogImplBase.GetAllLoginParams;
var
  oMeta: IFDPhysManagerMetadata;
  oDrv: IFDPhysDriverMetadata;
  i, iLogin: Integer;
  sName, sCaption, sTmp: String;
begin
  FDPhysManager.CreateMetadata(oMeta);
  if FDPhysManager.State <> dmsActive then
    FDPhysManager.Open;
  oMeta.CreateDriverMetadata(FConnectionDef.DriverID, oDrv);
  FVisibleItems.Clear;
  for i := 0 to oDrv.GetConnParamCount(FConnectionDef.Params) - 1 do begin
    oDrv.GetConnParams(FConnectionDef.Params, i, sName, sTmp, sTmp, sCaption, iLogin);
    if iLogin <> -1 then
      FVisibleItems.Add(sName + '=' + sCaption);
  end;
end;

{-------------------------------------------------------------------------------}
function TFDGUIxLoginDialogImplBase.Execute(ALoginAction: TFDGUIxLoginAction): Boolean;
var
  iRetries: Integer;
  lLoged, lLoginPrompt: Boolean;
begin
  if FDGUIxSilent() then begin
    if Assigned(ALoginAction) then
      ALoginAction;
    Result := True;
    Exit;
  end;
  if Assigned(FOnShow) then
    FOnShow(Self);
  iRetries := 0;
  lLoged := True;
  lLoginPrompt := True;
  repeat
    try
      if lLoginPrompt then begin
        if Assigned(FOnLogin) then begin
          Result := True;
          FOnLogin(Self, Result);
        end
        else
          Result := ExecuteLogin;
      end
      else begin
        lLoginPrompt := True;
        Result := True;
      end;
      if Result and Assigned(ALoginAction) then
        ALoginAction;
      lLoged := True;
    except on E: EFDDBEngineException do
      // invalid password/user name
      if (E.Kind = ekUserPwdInvalid) and (FLoginRetries >= 0) then begin
        FDHandleException;
        Inc(iRetries);
        if iRetries >= FLoginRetries then
          FDException(Self, [S_FD_LGUIx, S_FD_LGUIx_PForms], er_FD_AccToManyLogins,
            [FLoginRetries]);
        lLoged := False;
      end
      // password expired
      else if (E.Kind = ekUserPwdExpired) and FChangeExpiredPassword then begin
        FDHandleException;
        if Assigned(FOnChangePassword) then begin
          Result := True;
          FOnChangePassword(Self, Result);
        end
        else
          Result := ExecuteChngPwd;
        lLoginPrompt := False;
        lLoged := False;
      end
      // will expired
      else if E.Kind = ekUserPwdWillExpire then
        FDHandleException
      else
        raise;
    end;
  until lLoged;
  if Result and Assigned(FOnHide) then
    FOnHide(Self);
end;

{-------------------------------------------------------------------------------}
{ TFDGUIxScriptImplBase                                                         }
{-------------------------------------------------------------------------------}
function TFDGUIxScriptImplBase.GetOnHide: TNotifyEvent;
begin
  Result := FOnHide;
end;

{-------------------------------------------------------------------------------}
function TFDGUIxScriptImplBase.GetOnShow: TNotifyEvent;
begin
  Result := FOnShow;
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxScriptImplBase.SetOnHide(const AValue: TNotifyEvent);
begin
  FOnHide := AValue;
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxScriptImplBase.SetOnShow(const AValue: TNotifyEvent);
begin
  FOnShow := AValue;
end;

{-------------------------------------------------------------------------------}
function TFDGUIxScriptImplBase.GetOnOutput: TFDGUIxScriptOutputEvent;
begin
  Result := FOnOutput;
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxScriptImplBase.SetOnOutput(const AValue: TFDGUIxScriptOutputEvent);
begin
  FOnOutput := AValue;
end;

{-------------------------------------------------------------------------------}
function TFDGUIxScriptImplBase.GetOnInput: TFDGUIxScriptInputEvent;
begin
  Result := FOnInput;
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxScriptImplBase.SetOnInput(const AValue: TFDGUIxScriptInputEvent);
begin
  FOnInput := AValue;
end;

{-------------------------------------------------------------------------------}
function TFDGUIxScriptImplBase.GetOnPause: TFDGUIxScriptPauseEvent;
begin
  Result := FOnPause;
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxScriptImplBase.SetOnPause(const AValue: TFDGUIxScriptPauseEvent);
begin
  FOnPause := AValue;
end;

{-------------------------------------------------------------------------------}
function TFDGUIxScriptImplBase.GetOnProgress: TFDGUIxScriptProgressEvent;
begin
  Result := FOnProgress;
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxScriptImplBase.SetOnProgress(const AValue: TFDGUIxScriptProgressEvent);
begin
  FOnProgress := AValue;
end;

{-------------------------------------------------------------------------------}
function TFDGUIxScriptImplBase.GetCaption: String;
begin
  Result := FCaption;
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxScriptImplBase.SetCaption(const AValue: String);
begin
  FCaption := AValue;
end;

{-------------------------------------------------------------------------------}
function TFDGUIxScriptImplBase.GetOptions: TFDGUIxScriptOptions;
begin
  Result := FOptions;
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxScriptImplBase.SetOptions(AValue: TFDGUIxScriptOptions);
begin
  FOptions := AValue;
end;

{-------------------------------------------------------------------------------}
{ TFDGUIxVisualScriptImplBase                                                   }
{-------------------------------------------------------------------------------}
procedure TFDGUIxVisualScriptImplBase.Initialize;
begin
  inherited Initialize;
  FCaption := S_FD_ScriptDialogDefCaption;
  FOptions := [ssCallstack, ssConsole, ssAutoHide];
  FOnShow := nil;
  FOnHide := nil;
  FOnProgress := nil;
  FOnOutput := nil;
  FOnInput := nil;
  FOnPause := nil;
end;

{-------------------------------------------------------------------------------}
{ TFDGUIxWaitCursorImplBase                                                     }
{-------------------------------------------------------------------------------}
function TFDGUIxWaitCursorImplBase.GetWaitCursor: TFDGUIxScreenCursor;
begin
  Result := FWaitCursor;
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxWaitCursorImplBase.SetWaitCursor(const AValue: TFDGUIxScreenCursor);
begin
  FWaitCursor := AValue;
end;

{-------------------------------------------------------------------------------}
function TFDGUIxWaitCursorImplBase.GetOnHide: TNotifyEvent;
begin
  Result := FOnHide;
end;

{-------------------------------------------------------------------------------}
function TFDGUIxWaitCursorImplBase.GetOnShow: TNotifyEvent;
begin
  Result := FOnShow;
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxWaitCursorImplBase.SetOnHide(const AValue: TNotifyEvent);
begin
  FOnHide := AValue;
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxWaitCursorImplBase.SetOnShow(const AValue: TNotifyEvent);
begin
  FOnShow := AValue;
end;

{-------------------------------------------------------------------------------}
{ TFDGUIxVisualWaitCursorImplBase                                               }
{-------------------------------------------------------------------------------}
procedure TFDGUIxVisualWaitCursorImplBase.Initialize;
begin
  inherited Initialize;
  FWait := Self;
  FWaitCursor := gcrSQLWait;
end;

{-------------------------------------------------------------------------------}
destructor TFDGUIxVisualWaitCursorImplBase.Destroy;
begin
  ForceStopWait;
  FDFreeAndNil(FTimer);
  inherited Destroy;
  FWait := nil;
end;

{-------------------------------------------------------------------------------}
function TFDGUIxVisualWaitCursorImplBase.IsVisible: Boolean;
begin
  Result := not FDGUIxSilent() and (FWaitCursor <> gcrNone);
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxVisualWaitCursorImplBase.StopTimer;
begin
  if FTimer <> nil then
    FTimer.Enabled := False;
  FStopRequestTime := 0;
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxVisualWaitCursorImplBase.TimerProc;
begin
  if FDTimeout(FStopRequestTime, C_FD_HideCursorDelay) then begin
    if IsVisible then
      HideCursor;
    StopTimer;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxVisualWaitCursorImplBase.StartTimer;
begin
  if FTimer = nil then begin
    FTimer := TFDTimer.Create(nil);
    FTimer.Interval := C_FD_HideCursorDelay;
    FTimer.Options := [toSynchronized];
    FTimer.OnTimer := TimerProc;
  end;
  FTimer.Enabled := True;
  FStopRequestTime := FDGetTickCount();
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxVisualWaitCursorImplBase.ShowCursor;
begin
  if not CheckCurCursor then begin
    if Assigned(FOnShow) then
      FOnShow(Self);
    InternalShowCursor;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxVisualWaitCursorImplBase.HideCursor;
begin
  if InternalHideCursor then
    if Assigned(FOnHide) then
      FOnHide(Self);
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxVisualWaitCursorImplBase.StartWait;
begin
  if not IsVisible then
    Exit;
  StopTimer;
  ShowCursor;
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxVisualWaitCursorImplBase.StopWait;
begin
  if not IsVisible then
    Exit;
  StartTimer;
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxVisualWaitCursorImplBase.PopWait;
begin
  if not IsVisible then
    Exit;
  if CheckCurCursor then
    if FStopRequestTime > 0 then
      FWasActive := 2
    else
      FWasActive := 1
  else
    FWasActive := 0;
  HideCursor;
  StopTimer;
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxVisualWaitCursorImplBase.PushWait;
begin
  if not IsVisible then
    Exit;
  case FWasActive of
  2: begin
      ShowCursor;
      StartTimer;
     end;
  1: ShowCursor;
  0: ;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxVisualWaitCursorImplBase.ForceStopWait;
begin
  StopTimer;
  HideCursor;
  FWasActive := 0;
end;

end.
