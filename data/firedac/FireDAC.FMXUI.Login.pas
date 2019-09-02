{*******************************************************}
{                                                       }
{               Delphi FireDAC Framework                }
{            FireDAC FireMonkey Login dialog            }
{                                                       }
{ Copyright(c) 2004-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}
{$I FireDAC.inc}
{$IFDEF WIN32}
  {$HPPEMIT '#pragma link "FireDAC.FMXUI.Login.obj"'}
{$ELSE}
  {$HPPEMIT '#pragma link "FireDAC.FMXUI.Login.o"'}
{$ENDIF}

unit FireDAC.FMXUI.Login;

interface

uses
{$IFDEF MSWINDOWS}
  Winapi.Messages, Winapi.Windows,
{$ENDIF}
  System.SysUtils, System.Classes, System.IniFiles,
  FMX.Forms, FMX.Dialogs, FMX.Edit, FMX.ListBox, FMX.Controls, FMX.Objects, 
    FMX.Types, FMX.Effects, FMX.StdCtrls,
  FireDAC.Stan.Intf,
  FireDAC.UI.Intf, FireDAC.UI, FireDAC.FMXUI.OptsBase;

type
  TfrmFDGUIxFMXLogin = class(TfrmFDGUIxFMXOptsBase)
    pnlLogin: TPanel;
    pnlControls: TPanel;
    pnlHistory: TPanel;
    Label1: TLabel;
    cbxProfiles: TComboBox;
    pnlChngPwd: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    edtNewPassword: TEdit;
    edtNewPassword2: TEdit;
    OpenDialog1: TOpenDialog;
    Line5: TLine;
    Line6: TLine;
    imgEnabled: TImage;
    imgDisabled: TImage;
    imgOpen: TImage;
    pnlButton: TPanel;
    pnlCombo: TPanel;
    ShadowEffect1: TShadowEffect;
    procedure FormCreate(Sender: TObject);
    procedure cbxProfilesClick(Sender: TObject);
    procedure edtNewPasswordChange(Sender: TObject);
    procedure cbxProfilesChange(Sender: TObject);
    procedure FileEditClick(Sender: TObject);
    procedure imgEnabledClick(Sender: TObject);
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
  frmFDGUIxFormsLogin: TfrmFDGUIxFMXLogin;

implementation

uses
{$IFDEF MSWINDOWS}
  System.Win.Registry,
{$ENDIF}
  System.UITypes,
  FireDAC.Stan.Consts, FireDAC.Stan.Error, FireDAC.Stan.Util, FireDAC.Stan.Factory,
    FireDAC.Stan.ResStrs,
  FireDAC.Phys.Intf;

{$R *.fmx}

{-------------------------------------------------------------------------------}
{ TfrmFDGUIxFMXLogin                                                            }
{-------------------------------------------------------------------------------}
function TfrmFDGUIxFMXLogin.ExecuteLogin: Boolean;
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
  oChk: TSwitch;
  oBEdit: TEdit;
  oImg: TImage;
  oEff: TShadowEffect;
  oStream: TMemoryStream;
  oComp: TComponent;
  oActiveCtrl: TStyledControl;
  oList: TFDStringList;
  oWait: IFDGUIxWaitCursor;
  pParam: PFDLoginItem;
  oIni: TCustomIniFile;

  procedure AddItem(const AName, AType, ACaption: String; AOrder: Integer;
    const AValue: String);
  var
    i: Integer;
    s, sCaption: String;
    lOk: Boolean;
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
      pParam := @FParams[Length(FParams) - 1];
      pParam^.FParam := AName;
      pParam^.FType := AType;
      pParam^.FCaption := sCaption;
      pParam^.FOrder := AOrder;
      pParam^.FValue := AValue;
    end;
  end;

begin
  if FConnectionDef = nil then begin
    Result := False;
    Exit;
  end;

  pnlChngPwd.Visible := False;
  pnlLogin.Align := TAlignLayout.alClient;
  pnlLogin.Visible := True;
  lblPrompt.Text := S_FD_LoginCredentials;

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

  while pnlControls.ComponentCount > 0 do
    FDFree(pnlControls.Components[0]);

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
    oPanel.StyleLookup := 'backgroundstyle';
    oPanel.Parent := pnlControls;
    oPanel.Position.Y := i * 26;
    oPanel.Position.X := 3;
    oPanel.Height := 26;
    oPanel.Width := pnlControls.Width;

    oLabel := TLabel.Create(Self);
    oLabel.Parent := oPanel;
    oLabel.Position.Y := 5;
    oLabel.Position.X := 3;
    oLabel.Text := FParams[i].FCaption + ':';

    if (FParams[i].FType = '@L') or (FParams[i].FType = '@Y') then begin
      oChk := TSwitch.Create(Self);
      oChk.Parent := oPanel;
      oChk.Position.Y := 2;
      oChk.Position.X := 206;
      oChk.Width := 40;
      oChk.Height := 20;
      oChk.Tag := i;
      oChk.IsChecked := (CompareText(FParams[i].FValue, S_FD_True) = 0) or
        (CompareText(FParams[i].FValue, S_FD_Yes) = 0);
    end
    else if Copy(FParams[i].FType, 1, 2) = '@F' then begin
      oBEdit := TEdit.Create(Self);
      oBEdit.Parent := oPanel;
      oBEdit.Position.Y := 2;
      oBEdit.Position.X := 80;
      oBEdit.Width := 166;
      oBEdit.Tag := i;
      oImg := TImage.Create(Self);
      oImg.Parent := oBEdit;
      oImg.Width := 22;
      oImg.Align := TAlignLayout.alRight;
      oImg.OnClick := FileEditClick;
      oImg.Cursor := crArrow;
      oEff := TShadowEffect.Create(Self);
      oEff.Parent := oImg;
      oEff.Trigger := 'IsMouseOver=true';
      oStream := TMemoryStream.Create;
      try
        imgOpen.Bitmap.SaveToStream(oStream);
        oImg.Bitmap.LoadFromStream(oStream);
      finally
        FDFree(oStream);
      end;
      FDGUIxFMXSetupEditor(nil, nil, oBEdit, OpenDialog1, FParams[i].FType);
      oBEdit.Text := FParams[i].FValue;
      if (oBEdit.Text = '') and (oActiveCtrl = nil) then
        oActiveCtrl := oBEdit;
    end
    else if (FParams[i].FType <> '@S') and (FParams[i].FType <> '@I') and
            (FParams[i].FType <> '@P') then begin
      oCombo := TComboBox.Create(Self);
      oCombo.Parent := oPanel;
      oCombo.Position.Y := 2;
      oCombo.Position.X := 80;
      oCombo.Width := 166;
      oCombo.Tag := i;
      FDGUIxFMXSetupEditor(oCombo, nil, nil, nil, FParams[i].FType);
      oCombo.ItemIndex := oCombo.Items.IndexOf(FParams[i].FValue);
      if (oCombo.ItemIndex = -1) and (FParams[i].FValue <> '') then begin
        oCombo.Items.Add(FParams[i].FValue);
        oCombo.ItemIndex := oCombo.Items.Count - 1;
      end;
      if (oCombo.ItemIndex = -1) and (oActiveCtrl = nil) then
        oActiveCtrl := oCombo;
    end
    else begin
      oEdit := TEdit.Create(Self);
      oEdit.Password := FParams[i].FType = '@P';
      oEdit.Parent := oPanel;
      oEdit.Position.Y := 2;
      oEdit.Position.X := 80;
      oEdit.Width := 166;
      oEdit.Tag := i;
      oEdit.Text := FParams[i].FValue;
      if (oEdit.Text = '') and (oActiveCtrl = nil) then
        oActiveCtrl := oEdit;
    end;
  end;

  ClientHeight := FAddToOptionsHeight + Length(FParams) * 26;
  Width := 100 + 160;
  if oActiveCtrl = nil then
    ActiveControl := btnOk
  else
    ActiveControl := oActiveCtrl;

  cbxProfiles.Items.Clear;
  if FHistoryEnabled then begin
    oIni := CreateIniFile;
    try
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
        cbxProfiles.Items.Assign(oList);
        cbxProfiles.ItemIndex := cbxProfiles.Items.IndexOf(FConnectionDef.DriverID + ' - ' +
          AnsiUpperCase(FConnectionDef.Database) + ' - ' + FConnectionDef.UserName);
        cbxProfiles.OnChange(nil);
      finally
        FDFree(oList);
      end;
    finally
      FDFree(oIni);
    end
  end
  else begin
    Height := Height - Trunc(pnlHistory.Height);
    pnlHistory.Visible := False;
  end;

  FDCreateInterface(IFDGUIxWaitCursor, oWait);
  try
    oWait.PopWait;
    Result := (ShowModal = mrOK);
  finally
    oWait.PushWait;
  end;

  if Result then begin
    for i := 0 to pnlControls.ComponentCount - 1 do begin
      oComp := pnlControls.Components[i];
      if oComp is TCheckBox then
        if FParams[oComp.Tag].FType = '@Y' then
          FConnectionDef.AsYesNo[FParams[oComp.Tag].FParam] := TCheckBox(oComp).IsChecked
        else
          FConnectionDef.AsBoolean[FParams[oComp.Tag].FParam] := TCheckBox(oComp).IsChecked
      else if oComp is TEdit then
        FConnectionDef.AsString[FParams[oComp.Tag].FParam] := TEdit(oComp).Text
      else if oComp is TComboBox then
        if TComboBox(oComp).ItemIndex <> -1 then
          FConnectionDef.AsString[FParams[oComp.Tag].FParam] := TComboBox(oComp).Selected.Text;
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
function TfrmFDGUIxFMXLogin.ExecuteChngPwd: Boolean;
begin
  ClientHeight := Trunc(pnlButtons.Height + pnlTop.Height + pnlChngPwd.Height);
  Width := 250;

  pnlLogin.Visible := False;
  pnlChngPwd.Align := TALignLayout.alClient;
  pnlChngPwd.Visible := True;
  lblPrompt.Text := S_FD_LoginNewPassword;

  btnOk.Enabled := False;
  ActiveControl := edtNewPassword;

  Result := (ShowModal = mrOK);
  if Result then
    FConnectionDef.NewPassword := edtNewPassword.Text;
end;

{-------------------------------------------------------------------------------}
function TfrmFDGUIxFMXLogin.CreateIniFile: TCustomIniFile;
begin
  Result := nil;
  if FHistoryStorage = hsRegistry then
{$IFDEF MSWINDOWS}
    Result := TRegistryIniFile.Create(FHistoryKey)
{$ELSE}
    FDCapabilityNotSupported(nil, [S_FD_LGUIx, S_FD_LGUIx_PForms])
{$ENDIF}
  else if FHistoryStorage = hsFile then
    Result := TFDIniFile.Create(FHistoryKey);
end;

{-------------------------------------------------------------------------------}
procedure TfrmFDGUIxFMXLogin.FormCreate(Sender: TObject);
begin
  FAddToOptionsHeight := Trunc(pnlButtons.Height + pnlTop.Height +
    pnlHistory.Height);
end;

{-------------------------------------------------------------------------------}
procedure TfrmFDGUIxFMXLogin.imgEnabledClick(Sender: TObject);
var
  oIni: TCustomIniFile;
begin
  oIni := CreateIniFile;
  try
    if cbxProfiles.ItemIndex <> -1 then begin
      oIni.EraseSection(FDStrReplace(cbxProfiles.Selected.Text, '\', '/'));
      cbxProfiles.Items.Delete(cbxProfiles.ItemIndex);
      cbxProfiles.ItemIndex := -1;
    end;
  finally
    FDFree(oIni);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TfrmFDGUIxFMXLogin.cbxProfilesClick(Sender: TObject);
var
  i: Integer;
  oComp: TComponent;
  sName, sVal: String;
  oIni: TCustomIniFile;
  oCombo: TComboBox;
begin
  oIni := CreateIniFile;
  try
    if cbxProfiles.ItemIndex <> -1 then begin
      sName := FDStrReplace(cbxProfiles.Selected.Text, '\', '/');
      for i := 0 to pnlControls.ComponentCount - 1 do begin
        oComp := pnlControls.Components[i];
        if oComp is TCheckBox then
          TCheckBox(oComp).IsChecked := oIni.ReadBool(sName,
            FParams[oComp.Tag].FParam, False)
        else begin
          sVal := oIni.ReadString(sName, FParams[oComp.Tag].FParam, '');
          if oComp is TEdit then
            TEdit(oComp).Text := sVal
          else if oComp is TComboBox then begin
            oCombo := TComboBox(oComp);
            oCombo.ItemIndex := oCombo.Items.IndexOf(sVal);
          end;
        end;
      end;
    end;
  finally
    FDFree(oIni);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TfrmFDGUIxFMXLogin.cbxProfilesChange(Sender: TObject);
begin
  imgEnabled.Visible := cbxProfiles.ItemIndex <> -1;
  imgDisabled.Enabled := not imgEnabled.Visible;
end;

{-------------------------------------------------------------------------------}
procedure TfrmFDGUIxFMXLogin.edtNewPasswordChange(Sender: TObject);
begin
  btnOk.Enabled := (edtNewPassword.Text = edtNewPassword2.Text);
end;

{-------------------------------------------------------------------------------}
procedure TfrmFDGUIxFMXLogin.FileEditClick(Sender: TObject);
var
  oEdit: TEdit;
begin
  oEdit := TEdit(TImage(Sender).Parent);
  OpenDialog1.FileName := oEdit.Text;
  if OpenDialog1.Execute then
    TCustomEdit(Sender).Text := OpenDialog1.FileName;
end;

{-------------------------------------------------------------------------------}
{ TFDGUIxFMXLoginDialogImpl                                                     }
{-------------------------------------------------------------------------------}
type
  TFDGUIxFMXLoginDialogImpl = class(TFDGUIxLoginDialogImplBase)
  private
    function CreateDialog: TfrmFDGUIxFMXLogin;
  protected
    function ExecuteLogin: Boolean; override;
    function ExecuteChngPwd: Boolean; override;
  end;

{-------------------------------------------------------------------------------}
function TFDGUIxFMXLoginDialogImpl.CreateDialog: TfrmFDGUIxFMXLogin;
begin
  Result := TfrmFDGUIxFMXLogin.Create(Application);
  Result.FConnectionDef := FConnectionDef;
  Result.Caption := FCaption;
  Result.FVisibleItems := FVisibleItems;
  Result.FHistoryEnabled := FHistoryEnabled;
  Result.FHistoryWithPassword := FHistoryWithPassword;
  Result.FHistoryStorage := FHistoryStorage;
  Result.FHistoryKey := FHistoryKey;
end;

{-------------------------------------------------------------------------------}
function TFDGUIxFMXLoginDialogImpl.ExecuteLogin: Boolean;
var
  oLogin: TfrmFDGUIxFMXLogin;
begin
  oLogin := CreateDialog;
  Result := oLogin.ExecuteLogin;
end;

{-------------------------------------------------------------------------------}
function TFDGUIxFMXLoginDialogImpl.ExecuteChngPwd: Boolean;
var
  oLogin: TfrmFDGUIxFMXLogin;
begin
  oLogin := CreateDialog;
  Result := oLogin.ExecuteChngPwd;
end;

{-------------------------------------------------------------------------------}
var
  oFact: TFDFactory;

initialization
  oFact := TFDMultyInstanceFactory.Create(TFDGUIxFMXLoginDialogImpl,
    IFDGUIxLoginDialog, C_FD_GUIxFMXProvider, 'FireDAC.FMXUI.Login');

finalization
  FDReleaseFactory(oFact);

end.
