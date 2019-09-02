{*******************************************************}
{                                                       }
{               Delphi FireDAC Framework                }
{                 FireDAC Login dialog                  }
{                                                       }
{ Copyright(c) 2004-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}
{$I FireDAC.inc}
{$IFDEF WIN32}
  {$HPPEMIT '#pragma link "FireDAC.VCLUI.Login.obj"'}
{$ELSE}
  {$HPPEMIT '#pragma link "FireDAC.VCLUI.Login.o"'}
{$ENDIF}

unit FireDAC.VCLUI.Login;

interface

uses
{$IFDEF MSWINDOWS}
  Winapi.Messages, Winapi.Windows,
{$ENDIF}
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls,
    System.IniFiles, Vcl.Dialogs, Vcl.ImgList,
  FireDAC.Stan.Intf,
  FireDAC.UI.Intf, FireDAC.UI, FireDAC.VCLUI.OptsBase;

type
  TfrmFDGUIxFormsLogin = class(TfrmFDGUIxFormsOptsBase)
    pnlLogin: TPanel;
    pnlControls: TPanel;
    pnlHistory: TPanel;
    Label1: TLabel;
    SpeedButton1: TSpeedButton;
    cbxProfiles: TComboBox;
    Bevel1: TBevel;
    pnlChngPwd: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    edtNewPassword: TEdit;
    edtNewPassword2: TEdit;
    OpenDialog1: TOpenDialog;
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure cbxProfilesClick(Sender: TObject);
    procedure edtNewPasswordChange(Sender: TObject);
    procedure cbxProfilesChange(Sender: TObject);
    procedure FileEditClick(Sender: TObject);
  private
    { Private declarations }
    FAddToOptionsHeight: Integer;
    FParams: array of TFDLoginItem;
    FConnectionDef: IFDStanConnectionDef;
    FVisibleItems: TStrings;
    FHistoryEnabled: Boolean;
    FHistoryWithPassword: Boolean;
    FHistoryStorage: TFDGUIxLoginHistoryStorage;
    FHistoryKey: String;
    function CreateIniFile: TCustomIniFile;
  public
    { Public declarations }
    function ExecuteLogin: Boolean;
    function ExecuteChngPwd: Boolean;
  end;

var
  frmFDGUIxFormsLogin: TfrmFDGUIxFormsLogin;

implementation

uses
{$IFDEF MSWINDOWS}
  System.Win.Registry,
{$ENDIF}
  FireDAC.Stan.Consts, FireDAC.Stan.Error, FireDAC.Stan.Util, FireDAC.Stan.Factory, FireDAC.Stan.ResStrs,
  FireDAC.Phys.Intf,
  FireDAC.VCLUI.Controls;

{$R *.dfm}

{-------------------------------------------------------------------------------}
{ TfrmFDGUIxFormsLogin                                                          }
{-------------------------------------------------------------------------------}
type
  __TControl = class(TControl)
  end;

{-------------------------------------------------------------------------------}
function TfrmFDGUIxFormsLogin.ExecuteLogin: Boolean;
var
  oMeta: IFDPhysManagerMetadata;
  oDrv: IFDPhysDriverMetadata;
  i, j, iLogin: Integer;
  rTmp: TFDLoginItem;
  sName, sType, sDef, sCaption: String;
  oPanel: TPanel;
  oLabel: TLabel;
  oCombo: TComboBox;
  oEdit: TEdit;
  oChk: TCheckBox;
  oBEdit: TButtonedEdit;
  oCtrl: TControl;
  oActiveCtrl: TWinControl;
  oList: TFDStringList;
  oIni: TCustomIniFile;

  procedure AddItem(const AName, AType, ACaption: String; AOrder: Integer;
    const AValue: String);
  var
    i: Integer;
    s, sCaption: String;
    lOk: Boolean;
    pItem: PFDLoginItem;
  begin
    sCaption := ACaption;
    lOk := FVisibleItems.Count = 0;
    if not lOk then begin
      for i := 0 to FVisibleItems.Count - 1 do
        if CompareText(FDNameFromIndex(FVisibleItems, i), AName) = 0 then begin
          s := FDValueFromIndex(FVisibleItems, i);
          if s <> '' then
            sCaption := s;
          lOk := True;
          Break;
        end;
      if not lOk then begin
        s := Trim(FVisibleItems.Text);
        lOk := (s = '') or (s = '*');
      end;
    end;
    if lOk then begin
      SetLength(FParams, Length(FParams) + 1);
      pItem := @FParams[Length(FParams) - 1];
      pItem^.FParam := AName;
      pItem^.FType := AType;
      pItem^.FCaption := sCaption;
      pItem^.FOrder := AOrder;
      pItem^.FValue := AValue;
    end;
  end;

begin
  if FConnectionDef = nil then begin
    Result := False;
    Exit;
  end;

  pnlChngPwd.Visible := False;
  pnlLogin.Align := alClient;
  pnlLogin.Visible := True;
  lblPrompt.Caption := S_FD_LoginCredentials;

  SetLength(FParams, 0);
  if FDPhysManager.State = dmsInactive then
    FDPhysManager.Open;
  FDPhysManager.CreateMetadata(oMeta);
  oMeta.CreateDriverMetadata(FConnectionDef.DriverID, oDrv);
  for i := 0 to oDrv.GetConnParamCount(FConnectionDef.Params) - 1 do begin
    oDrv.GetConnParams(FConnectionDef.Params, i, sName, sType, sDef, sCaption, iLogin);
    if iLogin <> -1 then
      AddItem(sName, sType, sCaption, iLogin, FConnectionDef.AsString[sName]);
  end;
  if Length(FParams) = 0 then begin
    Result := True;
    Exit;
  end;

  while pnlControls.ControlCount > 0 do
    pnlControls.Controls[0].Free;

  for i := 0 to Length(FParams) - 1 do
    for j := i to Length(FParams) - 2 do
      if FParams[j].FOrder > FParams[j + 1].FOrder then begin
        rTmp := FParams[j + 1];
        FParams[j + 1] := FParams[j];
        FParams[j] := rTmp;
      end;

  oActiveCtrl := nil;
  for i := 0 to Length(FParams) - 1 do begin
    oPanel := TPanel.Create(Self);
    oPanel.BevelInner := bvNone;
    oPanel.BevelOuter := bvNone;
    oPanel.BorderStyle := bsNone;
    oPanel.Parent := pnlControls;
    oPanel.Top := i * 26;
    oPanel.Height := 26;
    oPanel.Left := 3;
    oPanel.Width := pnlControls.ClientWidth;

    oLabel := TLabel.Create(Self);
    oLabel.Parent := oPanel;
    oLabel.Top := 5;
    oLabel.Left := 3;
    oLabel.Caption := '&' + FParams[i].FCaption + ':';

    if (FParams[i].FType = '@L') or (FParams[i].FType = '@Y') then begin
      oChk := TCheckBox.Create(Self);
      oChk.Parent := oPanel;
      oChk.Top := 4;
      oChk.Left := 70;
      oChk.Tag := i;
      oChk.Checked := (CompareText(FParams[i].FValue, S_FD_True) = 0) or
        (CompareText(FParams[i].FValue, S_FD_Yes) = 0);
      oLabel.FocusControl := oChk;
    end
    else if Copy(FParams[i].FType, 1, 2) = '@F' then begin
      oBEdit := TButtonedEdit.Create(Self);
      oBEdit.Parent := oPanel;
      oBEdit.Top := 2;
      oBEdit.Left := 70;
      oBEdit.Width := 166;
      oBEdit.Tag := i;
      oBEdit.Images := ImageList1;
      oBEdit.RightButton.ImageIndex := 0;
      oBEdit.RightButton.Visible := True;
      oBEdit.OnRightButtonClick := FileEditClick;
      FDGUIxSetupEditor(nil, nil, oBEdit, OpenDialog1, FParams[i].FType);
      oBEdit.Text := FParams[i].FValue;
      if (oBEdit.Text = '') and (oActiveCtrl = nil) then
        oActiveCtrl := oBEdit;
      oLabel.FocusControl := oBEdit;
    end
    else if (FParams[i].FType <> '@S') and (FParams[i].FType <> '@I') and
            (FParams[i].FType <> '@P') then begin
      oCombo := TComboBox.Create(Self);
      oCombo.Parent := oPanel;
      oCombo.Top := 2;
      oCombo.Left := 70;
      oCombo.Width := 166;
      oCombo.Tag := i;
      FDGUIxSetupEditor(oCombo, nil, nil, nil, FParams[i].FType);
      oCombo.Text := FParams[i].FValue;
      if (oCombo.Text = '') and (oActiveCtrl = nil) then
        oActiveCtrl := oCombo;
      oLabel.FocusControl := oCombo;
    end
    else begin
      oEdit := TEdit.Create(Self);
      if FParams[i].FType = '@P' then
        oEdit.PasswordChar := '#';
      oEdit.Parent := oPanel;
      oEdit.Top := 2;
      oEdit.Left := 70;
      oEdit.Width := 166;
      oEdit.Tag := i;
      oEdit.Text := FParams[i].FValue;
      if (oEdit.Text = '') and (oActiveCtrl = nil) then
        oActiveCtrl := oEdit;
      oLabel.FocusControl := oEdit;
    end;
  end;

  ClientHeight := FAddToOptionsHeight + Length(FParams) * 26;
  Width := 100 + 150;
  if oActiveCtrl = nil then
    ActiveControl := btnOk
  else
    ActiveControl := oActiveCtrl;

  cbxProfiles.Items.Clear;
  if FHistoryEnabled then begin
    oIni := CreateIniFile;
    oList := TFDStringList.Create;
    try
      oIni.ReadSections(oList);
      i := 0;
      while i < oList.Count do
        if Pos(FConnectionDef.DriverID + ' - ', oList[i]) = 0 then
          oList.Delete(i)
        else begin
          oList[i] := FDStrReplace(oList[i], '/', '\');
          Inc(i);
        end;
      cbxProfiles.Items := oList;
      cbxProfiles.ItemIndex := cbxProfiles.Items.IndexOf(FConnectionDef.DriverID + ' - ' +
        AnsiUpperCase(FConnectionDef.Database) + ' - ' + FConnectionDef.UserName);
{$IFDEF MSWINDOWS}
      SendMessage(cbxProfiles.Handle, CB_SETDROPPEDWIDTH, Width, 0);
{$ENDIF}
      cbxProfiles.OnChange(nil);
    finally
      FDFree(oList);
      FDFree(oIni);
    end
  end
  else begin
    Height := Height - pnlHistory.Height;
    pnlHistory.Visible := False;
  end;

  Result := (ShowModal = mrOK);

  if Result then begin
    for i := 0 to pnlControls.ControlCount - 1 do begin
      oCtrl := TPanel(pnlControls.Controls[i]).Controls[1];
      if oCtrl is TCheckBox then
        if FParams[oCtrl.Tag].FType = '@Y' then
          FConnectionDef.AsYesNo[FParams[oCtrl.Tag].FParam] := TCheckBox(oCtrl).Checked
        else
          FConnectionDef.AsBoolean[FParams[oCtrl.Tag].FParam] := TCheckBox(oCtrl).Checked
      else
        FConnectionDef.AsString[FParams[oCtrl.Tag].FParam] := __TControl(oCtrl).Caption;
    end;

    if FHistoryEnabled then begin
      oIni := CreateIniFile;
      try
        sName := FConnectionDef.DriverID + ' - ' +
          FDStrReplace(AnsiUpperCase(FConnectionDef.Database), '\', '/') + ' - ' +
          FConnectionDef.UserName;
        oIni.EraseSection(sName);
        for i := 0 to Length(FParams) - 1 do
          if FHistoryWithPassword or
             (CompareText(FParams[i].FParam, S_FD_ConnParam_Common_Password) <> 0) then
            oIni.WriteString(sName, FParams[i].FParam, FConnectionDef.AsString[FParams[i].FParam]);
      finally
        FDFree(oIni);
      end;
    end;

  end;
  SetLength(FParams, 0);
  Application.ProcessMessages;
end;

{-------------------------------------------------------------------------------}
function TfrmFDGUIxFormsLogin.ExecuteChngPwd: Boolean;
begin
  ClientHeight := pnlButtons.Height + pnlTop.Height + pnlChngPwd.Height;
  Width := 250;

  pnlLogin.Visible := False;
  pnlChngPwd.Align := alClient;
  pnlChngPwd.Visible := True;
  lblPrompt.Caption := S_FD_LoginNewPassword;

  btnOk.Enabled := False;
  ActiveControl := edtNewPassword;

  Result := (ShowModal = mrOK);
  if Result then
    FConnectionDef.NewPassword := edtNewPassword.Text;
end;

{-------------------------------------------------------------------------------}
function TfrmFDGUIxFormsLogin.CreateIniFile: TCustomIniFile;
begin
  if FHistoryStorage = hsRegistry then
{$IFDEF MSWINDOWS}
    Result := TRegistryIniFile.Create(FHistoryKey)
{$ELSE}
    FDCapabilityNotSupported(nil, [S_FD_LGUIx, S_FD_LGUIx_PForms])
{$ENDIF}
  else if FHistoryStorage = hsFile then
    Result := TFDIniFile.Create(FHistoryKey)
  else
    Result := nil;
end;

{-------------------------------------------------------------------------------}
procedure TfrmFDGUIxFormsLogin.FormCreate(Sender: TObject);
begin
  FAddToOptionsHeight := pnlButtons.Height + pnlTop.Height + pnlHistory.Height;
end;

{-------------------------------------------------------------------------------}
procedure TfrmFDGUIxFormsLogin.SpeedButton1Click(Sender: TObject);
var
  oIni: TCustomIniFile;
begin
  oIni := CreateIniFile;
  try
    oIni.EraseSection(FDStrReplace(cbxProfiles.Text, '\', '/'));
    cbxProfiles.Items.Delete(cbxProfiles.ItemIndex);
    cbxProfiles.ItemIndex := -1;
    cbxProfiles.Text := '';
  finally
    FDFree(oIni);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TfrmFDGUIxFormsLogin.cbxProfilesClick(Sender: TObject);
var
  i: Integer;
  oCtrl: TControl;
  sName: String;
  oIni: TCustomIniFile;
begin
  oIni := CreateIniFile;
  try
    sName := FDStrReplace(cbxProfiles.Text, '\', '/');
    for i := 0 to pnlControls.ControlCount - 1 do begin
      oCtrl := TPanel(pnlControls.Controls[i]).Controls[1];
      __TControl(oCtrl).Caption := oIni.ReadString(sName, FParams[oCtrl.Tag].FParam, '');
    end;
  finally
    FDFree(oIni);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TfrmFDGUIxFormsLogin.cbxProfilesChange(Sender: TObject);
begin
  SpeedButton1.Enabled := cbxProfiles.Text <> '';
end;

{-------------------------------------------------------------------------------}
procedure TfrmFDGUIxFormsLogin.edtNewPasswordChange(Sender: TObject);
begin
  btnOk.Enabled := (edtNewPassword.Text = edtNewPassword2.Text);
end;

{-------------------------------------------------------------------------------}
procedure TfrmFDGUIxFormsLogin.FileEditClick(Sender: TObject);
begin
  OpenDialog1.FileName := TCustomEdit(Sender).Text;
  if OpenDialog1.Execute then
    TCustomEdit(Sender).Text := OpenDialog1.FileName;
end;

{-------------------------------------------------------------------------------}
{ TFDGUIxFormsLoginDialogImpl                                                   }
{-------------------------------------------------------------------------------}
type
  TFDGUIxFormsLoginDialogImpl = class(TFDGUIxLoginDialogImplBase)
  private
    function CreateDialog: TfrmFDGUIxFormsLogin;
  protected
    function ExecuteLogin: Boolean; override;
    function ExecuteChngPwd: Boolean; override;
  end;

{-------------------------------------------------------------------------------}
function TFDGUIxFormsLoginDialogImpl.CreateDialog: TfrmFDGUIxFormsLogin;
begin
  Result := TfrmFDGUIxFormsLogin.Create(Application);
  Result.FConnectionDef := FConnectionDef;
  Result.Caption := FCaption;
  Result.FVisibleItems := FVisibleItems;
  Result.FHistoryEnabled := FHistoryEnabled;
  Result.FHistoryWithPassword := FHistoryWithPassword;
  Result.FHistoryStorage := FHistoryStorage;
  Result.FHistoryKey := FHistoryKey;
end;

{-------------------------------------------------------------------------------}
function TFDGUIxFormsLoginDialogImpl.ExecuteLogin: Boolean;
var
  oLogin: TfrmFDGUIxFormsLogin;
begin
  oLogin := CreateDialog;
  Result := oLogin.ExecuteLogin;
end;

{-------------------------------------------------------------------------------}
function TFDGUIxFormsLoginDialogImpl.ExecuteChngPwd: Boolean;
var
  oLogin: TfrmFDGUIxFormsLogin;
begin
  oLogin := CreateDialog;
  Result := oLogin.ExecuteChngPwd;
end;

{-------------------------------------------------------------------------------}
var
  oFact: TFDFactory;

initialization
  oFact := TFDMultyInstanceFactory.Create(TFDGUIxFormsLoginDialogImpl,
    IFDGUIxLoginDialog, C_FD_GUIxFormsProvider, 'FireDAC.VCLUI.Login');

finalization
  FDReleaseFactory(oFact);

end.
