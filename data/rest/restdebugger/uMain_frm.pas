{*******************************************************}
{                                                       }
{             Delphi REST Client Framework              }
{                                                       }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}
unit uMain_frm;

                                                                               
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, System.IOUtils, System.IniFiles,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.Objects, FMX.Edit, FMX.ListBox, FMX.Layouts, FMX.Memo,
  FMX.TabControl, FMX.Grid, FMX.Bind.DBEngExt, FMX.Bind.Grid,
  uRESTObjects, uMRUList,
  REST.Authenticator.OAuth,
  REST.Authenticator.Basic,
  REST.Authenticator.Simple,
  REST.Response.Adapter,
  REST.Client,
  REST.Exception,
  REST.Consts,
  REST.Json,
  REST.Types,
  REST.Utils,
  Data.DB,
  Data.Bind.EngExt,
  System.Bindings.Outputs, FMX.Bind.Editors, Data.Bind.Components,
  Data.Bind.Grid, Data.Bind.DBScope, System.Generics.Collections, uWait,
  Datasnap.DBClient, Data.Bind.ObjectScope,
  IPPeerAPI, IPPeerClient;

type
  Tfrm_Main = class(TForm)
    pnl_Header: TLayout;
    lbl_MainTitle: TLabel;
    img_Logo: TImage;
    gb_Request: TGroupBox;
    gb_Response: TGroupBox;
    Splitter1: TSplitter;
    tc_Response: TTabControl;
    ti_Response_Headers: TTabItem;
    ti_Response_Body: TTabItem;
    Rectangle1: TRectangle;
    memo_ResponseHeader: TMemo;
    memo_ResponseBody: TMemo;
    lbl_LastRequestStats: TLabel;
    dlg_LoadRequestSettings: TOpenDialog;
    dlg_SaveRequestSettings: TSaveDialog;
    RESTClient: TRESTClient;
    RESTRequest: TRESTRequest;
    RESTResponse: TRESTResponse;
    RESTResponseDataSetAdapter: TRESTResponseDataSetAdapter;
    SimpleAuthenticator: TSimpleAuthenticator;
    HTTPBasicAuthenticator: THTTPBasicAuthenticator;
    OAuth1Authenticator: TOAuth1Authenticator;
    OAuth2Authenticator: TOAuth2Authenticator;
    ti_Response_TableView: TTabItem;
    StringGrid1: TStringGrid;
    BindSourceRESTResponse: TBindSourceDB;
    BindingsList1: TBindingsList;
    StatusBar: TStatusBar;
    lbl_ProxyState: TLabel;
    ClientDataSet: TClientDataSet;
    BindSourceDB1: TBindSourceDB;
    layout_Request: TLayout;
    lbl_LastRequestURL: TLabel;
    LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource;
    ToolBar1: TToolBar;
    LabelJson: TLabel;
    LabelRootElement: TLabel;
    ButtonRootElement: TButton;
    EditRootElement: TClearingEdit;
    Label15: TLabel;
    btn_ExecuteRequest: TButton;
    btn_Newrequest: TButton;
    btn_LoadRequest: TButton;
    btn_SaveRequest: TButton;
    tc_Request: TTabControl;
    ti_Request_Basic: TTabItem;
    cmb_RequestMethod: TComboBox;
    lbl_RequestMethodCaption: TLabel;
    memo_RequestBody: TMemo;
    cmb_RequestURL: TComboEdit;
    lbl_RequestBodyCaption: TLabel;
    lbl_BaseURLCaption: TLabel;
    btn_ClearMRUList: TButton;
    edt_ContentType: TComboEdit;
    Label16: TLabel;
    ti_Request_Advanced: TTabItem;
    lb_CustomParameters: TListBox;
    btn_AddCustomParameter: TButton;
    btn_EditCustomParameter: TButton;
    btn_DeleteCustomParameter: TButton;
    edt_Resource: TEdit;
    Label10: TLabel;
    lbl_CustomHeadersCaption: TLabel;
    ti_Request_Auth: TTabItem;
    cmb_AuthMethod: TComboBox;
    Label5: TLabel;
    edt_AuthPassword: TEdit;
    Label4: TLabel;
    edt_AuthUsername: TEdit;
    Label1: TLabel;
    edt_AuthClientID: TEdit;
    Label2: TLabel;
    edt_AuthAccessToken: TEdit;
    Label3: TLabel;
    edt_AuthRequestToken: TEdit;
    Label6: TLabel;
    edt_AuthClientSecret: TEdit;
    Label7: TLabel;
    edt_AuthPasswordKey: TEdit;
    Label8: TLabel;
    edt_AuthUsernameKey: TEdit;
    Label9: TLabel;
    Connection: TTabItem;
    edt_ProxyServer: TEdit;
    lbl_ProxyServer: TLabel;
    edt_ProxyUser: TEdit;
    Label11: TLabel;
    edt_ProxyPass: TEdit;
    Label12: TLabel;
    cbProxy: TCheckBox;
    edt_ProxyPort: TNumberBox;
    Label13: TLabel;
    btn_OAuthAssistant: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btn_ExecuteRequestClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btn_LoadRequestClick(Sender: TObject);
    procedure btn_SaveRequestClick(Sender: TObject);
    procedure btn_EditCustomParameterClick(Sender: TObject);
    procedure lb_CustomParametersDblClick(Sender: TObject);
    procedure btn_AddCustomParameterClick(Sender: TObject);
    procedure btn_DeleteCustomParameterClick(Sender: TObject);
    procedure cbProxyClick(Sender: TObject);
    procedure cmb_AuthMethodChange(Sender: TObject);
    procedure cmb_RequestMethodChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure tc_RequestChange(Sender: TObject);
    procedure RESTResponseDataSetAdapterBeforeOpenDataSet(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btn_NewrequestClick(Sender: TObject);
    procedure btn_ClearMRUListClick(Sender: TObject);
    procedure ButtonRootElementClick(Sender: TObject);
    procedure cmb_RequestURLClosePopup(Sender: TObject);
    procedure edt_ResourceExit(Sender: TObject);
    procedure btn_OAuthAssistantClick(Sender: TObject);
    procedure lbl_BaseURLCaptionClick(Sender: TObject);
    procedure cmb_RequestURLChange(Sender: TObject);
  private
    { Private declarations }

    FMRUList: TMRUList;
    FRESTParams: TRESTRequestParams;
    FProxyINI: string;

    FPopupClosed : boolean;

    procedure InitRequestMethodCombo;
    procedure InitAuthMethodCombo;

    procedure DoLoadRequestSettings;
    procedure DoSaveRequestSettings;

    procedure DoResetControls;
    procedure DoFetchRequestParamsFromControls;
    procedure DoPushRequestParamsToControls;
    procedure DoUpdateProxyStateLabel;

    procedure DoAddCustomParameter;
    procedure DoEditCustomParameter;
    procedure DoDeleteCustomParameter;

    procedure DoUpdateAuthEditFields;

    procedure DoExecuteRequest;

    procedure DoAddToMRUList(const AParams: TRESTRequestParams);
    procedure DoClearMRUList;
    procedure DoUpdateMRUList;
    procedure DoClearRequest;

    procedure DoCallOAuthAssistant;

    procedure DoDisplayHTTPResponse(ARequest: TRESTRequest; AClient: TRESTClient; AResponse: TRESTResponse);

    /// <summary>
    /// If a proxy server is to be used, then apply its parameters to the RestClient.
    /// </summary>
    procedure ConfigureProxyServer;
    procedure LoadProxySettings(const AFilename: string);
    procedure SaveProxySettings(const AFilename: string);
    procedure FillReponseContentMemo;
  public
    { Public declarations }
    procedure KeyDown(var Key: Word; var KeyChar: Char; Shift: TShiftState); override;
  end;

var
  frm_Main: Tfrm_Main;

var
  DefaultStorageFolder: string = '';
  /// will be initialized on startup

implementation

uses
  System.UIConsts,
  Data.DBXJson,
  uOAuth1_frm,
  uOAuth2_frm,
  uCustomHeaderDlg_frm;

{$R *.fmx}

const
  PROXYDBFILE = 'proxy.dat'; // do not localize

procedure Tfrm_Main.btn_AddCustomParameterClick(Sender: TObject);
begin
  DoAddCustomParameter;
end;

procedure Tfrm_Main.btn_ClearMRUListClick(Sender: TObject);
begin
  DoClearMRUList;
end;

procedure Tfrm_Main.btn_DeleteCustomParameterClick(Sender: TObject);
begin
  DoDeleteCustomParameter;
end;

procedure Tfrm_Main.btn_EditCustomParameterClick(Sender: TObject);
begin
  DoEditCustomParameter;
end;

procedure Tfrm_Main.btn_ExecuteRequestClick(Sender: TObject);
begin
  TWait.Start;
  try
    DoExecuteRequest;
  finally
    TWait.Done;
  end;
end;

procedure Tfrm_Main.btn_LoadRequestClick(Sender: TObject);
begin
  DoLoadRequestSettings;
end;

procedure Tfrm_Main.btn_NewrequestClick(Sender: TObject);
begin
  DoClearRequest;
end;

procedure Tfrm_Main.btn_OAuthAssistantClick(Sender: TObject);
begin
  DoCallOAuthAssistant;
end;

procedure Tfrm_Main.btn_SaveRequestClick(Sender: TObject);
begin
  DoSaveRequestSettings;
end;

procedure Tfrm_Main.ButtonRootElementClick(Sender: TObject);
begin
  RESTResponse.RootElement := EditRootElement.Text;
  FillReponseContentMemo;
end;

procedure Tfrm_Main.cbProxyClick(Sender: TObject);
begin
  edt_ProxyServer.Enabled := not cbProxy.IsChecked;
  edt_ProxyPort.Enabled := not cbProxy.IsChecked;
  edt_ProxyUser.Enabled := not cbProxy.IsChecked;
  edt_ProxyPass.Enabled := not cbProxy.IsChecked;
end;

procedure Tfrm_Main.cmb_AuthMethodChange(Sender: TObject);
begin
  DoUpdateAuthEditFields;
end;

procedure Tfrm_Main.cmb_RequestMethodChange(Sender: TObject);
var
  LMethod: TRESTRequestMethod;
begin
  if (cmb_RequestMethod.ItemIndex > -1) then
  begin
    LMethod := RESTRequestMethodFromString(cmb_RequestMethod.Items[cmb_RequestMethod.ItemIndex]);
    memo_RequestBody.Enabled := LMethod in [TRESTRequestMethod.rmPOST, TRESTRequestMethod.rmPUT];
  end
  else
    memo_RequestBody.Enabled := false;
end;

procedure Tfrm_Main.cmb_RequestURLChange(Sender: TObject);
var
  LItem: TRESTRequestParams;
begin
  if (FPopupClosed) then
  begin
    FPopupClosed:= FALSE;

    if (cmb_RequestURL.ItemIndex > -1) then
    begin
      LItem := (cmb_RequestURL.Items.Objects[cmb_RequestURL.ItemIndex] AS TRESTRequestParams);
      FRESTParams.Assign(LItem);

      DoPushRequestParamsToControls;
    end;
  end;
end;

procedure Tfrm_Main.cmb_RequestURLClosePopup(Sender: TObject);
begin
  FPopupClosed:= TRUE;

  /// this is not optimal, we have to invoke the onchange-event again
  cmb_RequestURLChange( Sender );
end;

procedure Tfrm_Main.DoAddCustomParameter;
var
  LParameter: TRESTRequestParameter;
  LDialog: Tfrm_CustomHeaderDlg;
  LKind: TRESTRequestParameterKind;
begin
  self.DoFetchRequestParamsFromControls;

  LDialog := Tfrm_CustomHeaderDlg.Create(self, NIL);
  try
    if (LDialog.ShowModal = mrOK) then
    begin

      if (LDialog.cmb_ParameterKind.ItemIndex > -1) then
        LKind := RESTRequestParameterKindFromString
          (LDialog.cmb_ParameterKind.Items[LDialog.cmb_ParameterKind.ItemIndex])
      else
        LKind := DefaultRESTRequestParameterKind;

      LParameter := FRESTParams.CustomParams.AddItem;
      LParameter.Name := LDialog.cmb_ParameterName.Text;
      LParameter.Value := LDialog.edt_ParameterValue.Text;
      LParameter.Kind := LKind;

      self.DoPushRequestParamsToControls;
    end;
  finally
    LDialog.Release;
  end;
end;

procedure Tfrm_Main.DoAddToMRUList(const AParams: TRESTRequestParams);
begin
  Assert(Assigned(AParams));

  FMRUList.AddItem(FRESTParams);
  DoUpdateMRUList;
end;

procedure Tfrm_Main.DoCallOAuthAssistant;
var
  frm1: Tfrm_OAuth1;
  frm2: Tfrm_OAuth2;
  res: integer;
begin
  DoFetchRequestParamsFromControls;

  if (FRESTParams.AuthMethod = amOAUTH) then
  begin
    frm1 := Tfrm_OAuth1.Create(self);
    TRY
      frm1.PushParamsToControls(FRESTParams);
      res := frm1.ShowModal;

      if IsPositiveResult(res) then
      begin
        frm1.FetchParamsFromControls(FRESTParams);
        DoPushRequestParamsToControls;
      end;
    FINALLY
      frm1.Release;
    END;
  end
  else if (FRESTParams.AuthMethod = amOAUTH2) then
  begin
    frm2 := Tfrm_OAuth2.Create(self);
    TRY
      frm2.PushParamsToControls(FRESTParams);
      res := frm2.ShowModal;

      if IsPositiveResult(res) then
      begin
        frm2.FetchParamsFromControls(FRESTParams);
        DoPushRequestParamsToControls;
      end;
    FINALLY
      frm2.Release;
    END;
  end;

end;

procedure Tfrm_Main.DoClearMRUList;
begin
  if (MessageDlg('Are you sure that you want to clear list of the most recent requests?', TMsgDlgType.mtConfirmation,
    [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0, TMsgDlgBtn.mbNo) = mrYes) then
  begin
    DoFetchRequestParamsFromControls;

    FMRUList.Clear;
    cmb_RequestURL.Items.Clear;

    DoPushRequestParamsToControls;
  end;
end;

procedure Tfrm_Main.DoClearRequest;
begin
  FRESTParams.ResetToDefaults;
  DoPushRequestParamsToControls;
end;

procedure Tfrm_Main.DoEditCustomParameter;
var
  LParameter: TRESTRequestParameter;
  LDialog: Tfrm_CustomHeaderDlg;
begin
  if (lb_CustomParameters.ItemIndex < 0) then
  begin
    MessageDlg('No custom parameter selected.', TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
    EXIT;
  end;

  self.DoFetchRequestParamsFromControls;

  LParameter := FRESTParams.CustomParams.ParameterByIndex(lb_CustomParameters.ItemIndex);

  LDialog := Tfrm_CustomHeaderDlg.Create(self, LParameter);
  try
    if (LDialog.ShowModal = mrOK) then
    begin
      LParameter.Name := LDialog.cmb_ParameterName.Text;
      LParameter.Value := LDialog.edt_ParameterValue.Text;
      if (LDialog.cmb_ParameterKind.ItemIndex > -1) then
        LParameter.Kind := RESTRequestParameterKindFromString
          (LDialog.cmb_ParameterKind.Items[LDialog.cmb_ParameterKind.ItemIndex])
      else
        LParameter.Kind := DefaultRESTRequestParameterKind;

      self.DoPushRequestParamsToControls;
    end;
  finally
    LDialog.Release;
  end;
end;

procedure Tfrm_Main.DoDeleteCustomParameter;
var
  LParameter: TRESTRequestParameter;
begin
  if (lb_CustomParameters.ItemIndex < 0) then
  begin
    MessageDlg('No custom parameter selected.', TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
    EXIT;
  end;

  self.DoFetchRequestParamsFromControls;

  LParameter := FRESTParams.CustomParams.ParameterByIndex(lb_CustomParameters.ItemIndex);

  if (MessageDlg('Are you sure you want to delete the custom parameter' + LineFeed + '"' + LParameter.ToString + '"?',
    TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0, TMsgDlgBtn.mbNo) = mrYes) then
  begin
    FRESTParams.CustomParams.Delete(LParameter);
    DoPushRequestParamsToControls;
  end;
end;

procedure Tfrm_Main.DoDisplayHTTPResponse(ARequest: TRESTRequest; AClient: TRESTClient; AResponse: TRESTResponse);
var
  i: integer;
begin
  if AResponse.StatusCode >= 300 then
  begin
    lbl_LastRequestStats.FontColor := claRed;
  end
  else
  begin
    lbl_LastRequestStats.FontColor := claBlack;
  end;

  /// we need to duplicate the ampersands to display them in a label ... ...
  lbl_LastRequestURL.Text := StringReplace( AResponse.FullRequestURI, '&', '&&', [rfReplaceAll] );

  lbl_LastRequestStats.Text :=
    Format('%d : %s - %d bytes of data returned. Timing: Pre: %dms - Exec: %dms - Post: %dms - Total: %dms',
    [AResponse.StatusCode, AResponse.StatusText, AResponse.ContentLength,
    ARequest.ExecutionPerformance.PreProcessingTime, ARequest.ExecutionPerformance.ExecutionTime,
    ARequest.ExecutionPerformance.PostProcessingTime, ARequest.ExecutionPerformance.TotalExecutionTime]);

  /// transfer http-headers into memo
  memo_ResponseHeader.Lines.Clear;
  for i := 0 to AResponse.Headers.Count - 1 do
    memo_ResponseHeader.Lines.Add(AResponse.Headers[i]);

  FillReponseContentMemo;
end;

procedure Tfrm_Main.DoExecuteRequest;
begin
  EditRootElement.Text := '';
  DoFetchRequestParamsFromControls;
  ConfigureProxyServer;

  // Fielddefs are recreated with every request. If FieldDefs already exist,
  // then RestResponseDatasetadpater will try to re-use them - which means that
  // we would end up with no-matching Responses and FieldDefs for different requests
  RESTResponseDataSetAdapter.FieldDefs.Clear;

  RESTRequest.ResetToDefaults;
  RESTResponse.ResetToDefaults;

  RESTClient.BaseURL := cmb_RequestURL.Text;
  if (Trim(RESTClient.BaseURL) = '') then
  begin
    MessageDlg(sRESTErrorEmptyURL, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
    EXIT;
  end;

  RESTRequest.Resource := FRESTParams.Resource;

  RESTRequest.Params.Clear;
  RESTRequest.Params.Assign(FRESTParams.CustomParams);

  if FRESTParams.CustomBody.Size > 0 then
  begin
    RESTRequest.AddBody(FRESTParams.CustomBody, ContentTypeFromString(FRESTParams.ContentType));
  end;

  RESTRequest.Method := FRESTParams.Method;

  case FRESTParams.AuthMethod of
    TRESTAuthMethod.amNONE:
      begin
        RESTClient.Authenticator := NIL;
      end;
    TRESTAuthMethod.amSIMPLE:
      begin
        RESTClient.Authenticator := SimpleAuthenticator;
        SimpleAuthenticator.Username := FRESTParams.AuthUsername;
        SimpleAuthenticator.UsernameKey := FRESTParams.AuthUsernameKey;
        SimpleAuthenticator.Password := FRESTParams.AuthPassword;
        SimpleAuthenticator.PasswordKey := FRESTParams.AuthPasswordKey;
      end;
    TRESTAuthMethod.amBASIC:
      begin
        RESTClient.Authenticator := HTTPBasicAuthenticator;
        HTTPBasicAuthenticator.Username := FRESTParams.AuthUsername;
        HTTPBasicAuthenticator.Password := FRESTParams.AuthPassword;
      end;
    TRESTAuthMethod.amOAUTH:
      begin
        RESTClient.Authenticator := OAuth1Authenticator;
        OAuth1Authenticator.ConsumerKey := FRESTParams.ClientID;
        OAuth1Authenticator.ConsumerSecrect := FRESTParams.ClientSecret;
        OAuth1Authenticator.AccessToken := FRESTParams.AccessToken;
        OAuth1Authenticator.AccessTokenSecret := FRESTParams.AccessTokenSecret;
      end;
    TRESTAuthMethod.amOAUTH2:
      begin
        RESTClient.Authenticator := OAuth2Authenticator;
        OAuth2Authenticator.AccessToken := FRESTParams.AccessToken;
      end;
  else
    raise ERESTException.Create(sRESTUnsupportedAuthMethod);
  end;

  RESTRequest.Client := RESTClient;
  RESTRequest.Execute;

  DoDisplayHTTPResponse(RESTRequest, RESTClient, RESTResponse);

  /// add the current request to the MRU-list
  DoAddToMRUList(FRESTParams);
end;

procedure Tfrm_Main.DoFetchRequestParamsFromControls;
begin
  /// workaround for a bug in FMX - the onchange-events are triggered too early.
  /// this is not a problem for our objects, but we get an AV while querying a
  /// tedit for it's text.
  if NOT Visible then
    EXIT;

  if (cmb_RequestMethod.ItemIndex > -1) then
    FRESTParams.Method := RESTRequestMethodFromString(cmb_RequestMethod.Items[cmb_RequestMethod.ItemIndex])
  else
    FRESTParams.Method := DefaultRESTRequestMethod;

  cmb_RequestURL.Text := RESTClient.HTTPClient.FixupURL(cmb_RequestURL.Text);
  FRESTParams.URL := cmb_RequestURL.Text;
  FRESTParams.Resource := edt_Resource.Text;
  FRESTParams.ContentType := edt_ContentType.Text;

  /// after fetching the resource, we try to re-create the parameter-list
  FRESTParams.CustomParams.FromString('', FRESTParams.Resource);

  if (cmb_AuthMethod.ItemIndex > -1) then
    FRESTParams.AuthMethod := RESTAuthMethodFromString(cmb_AuthMethod.Items[cmb_AuthMethod.ItemIndex])
  else
    FRESTParams.AuthMethod := DefaultRESTAuthMethod;

  FRESTParams.AuthUsername := edt_AuthUsername.Text;
  FRESTParams.AuthUsernameKey := edt_AuthUsernameKey.Text;
  FRESTParams.AuthPassword := edt_AuthPassword.Text;
  FRESTParams.AuthPasswordKey := edt_AuthPasswordKey.Text;
  FRESTParams.ClientID := edt_AuthClientID.Text;
  FRESTParams.ClientSecret := edt_AuthClientSecret.Text;
  FRESTParams.AccessToken := edt_AuthAccessToken.Text;
  FRESTParams.RequestToken := edt_AuthRequestToken.Text;

  DoUpdateProxyStateLabel;

  FRESTParams.CustomBody.Clear;
  memo_RequestBody.Lines.SaveToStream(FRESTParams.CustomBody);
end;

procedure Tfrm_Main.DoPushRequestParamsToControls;
var
  s: string;
  LParameter: TRESTRequestParameter;
begin
  s := RESTRequestMethodToString(FRESTParams.Method);
  if (cmb_RequestMethod.Items.IndexOf(s) > -1) then
    cmb_RequestMethod.ItemIndex := cmb_RequestMethod.Items.IndexOf(s)
  else
    cmb_RequestMethod.ItemIndex := -1;

  cmb_RequestURL.Text := FRESTParams.URL;
  edt_Resource.Text := FRESTParams.Resource;

  if (edt_ContentType.Items.IndexOf(FRESTParams.ContentType) > -1) then
  begin
    edt_ContentType.ItemIndex := edt_ContentType.Items.IndexOf(FRESTParams.ContentType);
    edt_ContentType.Text := FRESTParams.ContentType;
  end
  else
  begin
    edt_ContentType.ItemIndex := -1;
    edt_ContentType.Text := '';
  end;

  s := RESTAuthMethodToString(FRESTParams.AuthMethod);
  if (cmb_AuthMethod.Items.IndexOf(s) > -1) then
    cmb_AuthMethod.ItemIndex := cmb_AuthMethod.Items.IndexOf(s)
  else
    cmb_RequestMethod.ItemIndex := -1;

  edt_AuthUsername.Text := FRESTParams.AuthUsername;
  edt_AuthUsernameKey.Text := FRESTParams.AuthUsernameKey;
  edt_AuthPassword.Text := FRESTParams.AuthPassword;
  edt_AuthPasswordKey.Text := FRESTParams.AuthPasswordKey;
  edt_AuthClientID.Text := FRESTParams.ClientID;
  edt_AuthClientSecret.Text := FRESTParams.ClientSecret;
  edt_AuthAccessToken.Text := FRESTParams.AccessToken;
  edt_AuthRequestToken.Text := FRESTParams.RequestToken;

  DoUpdateProxyStateLabel;

  lb_CustomParameters.BeginUpdate;
  lb_CustomParameters.Items.BeginUpdate;
  TRY
    lb_CustomParameters.Clear;
    for LParameter IN FRESTParams.CustomParams do
    begin
      lb_CustomParameters.Items.AddObject(LParameter.ToString, LParameter);
    end;
  FINALLY
    lb_CustomParameters.Items.EndUpdate;
    lb_CustomParameters.EndUpdate;
  END;

  if (FRESTParams.CustomBody.Size > 0) then
  begin
    FRESTParams.CustomBody.Seek(0, soFromBeginning);
    memo_RequestBody.Lines.LoadFromStream(FRESTParams.CustomBody);
  end
  else
    memo_RequestBody.Lines.Clear;
end;

procedure Tfrm_Main.DoResetControls;
begin
  lbl_LastRequestStats.Text := '';

  memo_RequestBody.Lines.Clear;
  memo_ResponseHeader.Lines.Clear;
  memo_ResponseBody.Lines.Clear;

  cmb_RequestMethod.ItemIndex := cmb_RequestMethod.Items.IndexOf(RESTRequestMethodToString(DefaultRESTRequestMethod));

  /// try to set the itemindex to the default-value
  cmb_AuthMethod.ItemIndex := cmb_AuthMethod.Items.IndexOf(RESTAuthMethodToString(DefaultRESTAuthMethod));
  edt_AuthUsername.Text := '';
  edt_AuthPassword.Text := '';

  DoUpdateAuthEditFields;

  cbProxy.IsChecked := false;
  edt_ProxyServer.Text := '';
  edt_ProxyUser.Text := '';
  edt_ProxyPass.Text := '';
  edt_ProxyPort.Value := 0;

  lb_CustomParameters.Clear;
end;

procedure Tfrm_Main.DoLoadRequestSettings;
begin
  if dlg_LoadRequestSettings.Execute then
  begin
    FRESTParams.LoadFromFile(dlg_LoadRequestSettings.FileName);
    DoPushRequestParamsToControls;
  end;
end;

procedure Tfrm_Main.DoSaveRequestSettings;
begin
  DoFetchRequestParamsFromControls;
  if dlg_SaveRequestSettings.Execute then
  begin
    FRESTParams.SaveToFile(dlg_SaveRequestSettings.FileName);
  end;
end;

procedure Tfrm_Main.DoUpdateAuthEditFields;
var
  LSelectedMethod: TRESTAuthMethod;
begin
  if (cmb_AuthMethod.ItemIndex > -1) then
    LSelectedMethod := RESTAuthMethodFromString(cmb_AuthMethod.Items[cmb_AuthMethod.ItemIndex])
  else
    LSelectedMethod := DefaultRESTAuthMethod;

  case LSelectedMethod of
    amNONE:
      begin
        edt_AuthUsername.Enabled := false;
        edt_AuthUsernameKey.Enabled := false;
        edt_AuthPassword.Enabled := false;
        edt_AuthPasswordKey.Enabled := false;
        edt_AuthClientID.Enabled := false;
        edt_AuthClientSecret.Enabled := false;
        edt_AuthAccessToken.Enabled := false;
        edt_AuthClientSecret.Enabled := false;
        edt_AuthRequestToken.Enabled := false;

        btn_OAuthAssistant.Enabled := false;
      end;
    amSIMPLE:
      begin
        edt_AuthUsername.Enabled := true;
        edt_AuthUsernameKey.Enabled := true;
        edt_AuthPassword.Enabled := true;
        edt_AuthPasswordKey.Enabled := true;
        edt_AuthClientID.Enabled := false;
        edt_AuthClientSecret.Enabled := false;
        edt_AuthAccessToken.Enabled := false;
        edt_AuthClientSecret.Enabled := false;
        edt_AuthRequestToken.Enabled := false;

        btn_OAuthAssistant.Enabled := false;
      end;
    amBASIC:
      begin
        edt_AuthUsername.Enabled := true;
        edt_AuthUsernameKey.Enabled := false;
        edt_AuthPassword.Enabled := true;
        edt_AuthPasswordKey.Enabled := false;
        edt_AuthClientID.Enabled := false;
        edt_AuthClientSecret.Enabled := false;
        edt_AuthAccessToken.Enabled := false;
        edt_AuthClientSecret.Enabled := false;
        edt_AuthRequestToken.Enabled := false;

        btn_OAuthAssistant.Enabled := false;
      end;
    amOAUTH:
      begin
        edt_AuthUsername.Enabled := false;
        edt_AuthUsernameKey.Enabled := false;
        edt_AuthPassword.Enabled := false;
        edt_AuthPasswordKey.Enabled := false;
        edt_AuthClientID.Enabled := true;
        edt_AuthClientSecret.Enabled := true;
        edt_AuthAccessToken.Enabled := true;
        edt_AuthClientSecret.Enabled := true;
        edt_AuthRequestToken.Enabled := true;

        btn_OAuthAssistant.Enabled := true;
      end;
    amOAUTH2:
      begin
        edt_AuthUsername.Enabled := false;
        edt_AuthUsernameKey.Enabled := false;
        edt_AuthPassword.Enabled := false;
        edt_AuthPasswordKey.Enabled := false;
        edt_AuthClientID.Enabled := true;
        edt_AuthClientSecret.Enabled := true;
        edt_AuthAccessToken.Enabled := true;
        edt_AuthClientSecret.Enabled := true;
        edt_AuthRequestToken.Enabled := true;

        btn_OAuthAssistant.Enabled := true;
      end;
  end;
end;

procedure Tfrm_Main.DoUpdateMRUList;
var
  LItem: TRESTRequestParams;
begin
  cmb_RequestURL.Items.BeginUpdate;
  cmb_RequestURL.Items.Clear;
  for LItem IN FMRUList.Items do
    cmb_RequestURL.Items.AddObject(LItem.ToString, LItem);
  cmb_RequestURL.Items.EndUpdate;

  /// we do know that the last executed request is on top of the
  /// mru-list. so the item-index of the dropdown must be set to
  /// zero.
  if (cmb_RequestURL.Items.Count > 0) AND (cmb_RequestURL.Text <> '') then
    cmb_RequestURL.ItemIndex := 0
  else
    cmb_RequestURL.ItemIndex := -1;

  if (cmb_RequestURL.ItemIndex > -1) then
    cmb_RequestURL.Text := TRESTRequestParams(cmb_RequestURL.Items.Objects[cmb_RequestURL.ItemIndex]).URL;
end;

procedure Tfrm_Main.DoUpdateProxyStateLabel;
begin
  if cbProxy.IsChecked then
  begin
    lbl_ProxyState.Text := 'Proxy-server enabled: ' + edt_ProxyServer.Text;
    if (edt_ProxyPort.Value > 0.1) then
      lbl_ProxyState.Text := lbl_ProxyState.Text + ':' + IntToStr(Trunc(edt_ProxyPort.Value));
  end
  else
    lbl_ProxyState.Text := 'Proxy-server disabled';
end;

procedure Tfrm_Main.edt_ResourceExit(Sender: TObject);
begin
  DoFetchRequestParamsFromControls;
  FRESTParams.CustomParams.FromString('', edt_Resource.Text);
  DoPushRequestParamsToControls;
end;

procedure Tfrm_Main.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveProxySettings(FProxyINI);

  FreeAndNil(FRESTParams);
  FreeAndNil(FMRUList);

  Action := TCloseAction.caFree;
end;

procedure Tfrm_Main.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := true;
end;

procedure Tfrm_Main.FormCreate(Sender: TObject);
begin
  /// init the form
  tc_Request.ActiveTab := ti_Request_Basic;
  tc_Response.ActiveTab := ti_Response_Headers;

  InitRequestMethodCombo;
  InitAuthMethodCombo;
  DoResetControls;

  FPopupClosed:= FALSE;

  dlg_LoadRequestSettings.InitialDir := DefaultStorageFolder;
  dlg_SaveRequestSettings.InitialDir := DefaultStorageFolder;

  FRESTParams := TRESTRequestParams.Create;
  FMRUList := TMRUList.Create(DefaultStorageFolder + MRUDBFILE);

  FProxyINI := DefaultStorageFolder + PROXYDBFILE;
  LoadProxySettings(FProxyINI);

  DoUpdateMRUList;
  DoPushRequestParamsToControls;
end;

procedure Tfrm_Main.FormDestroy(Sender: TObject);
begin
  SaveProxySettings(FProxyINI);
end;

procedure Tfrm_Main.InitAuthMethodCombo;
var
  LAuthMethod: TRESTAuthMethod;
begin
  cmb_AuthMethod.BeginUpdate;
  TRY
    cmb_AuthMethod.Clear;
    for LAuthMethod IN [Low(TRESTAuthMethod) .. High(TRESTAuthMethod)] do
      cmb_AuthMethod.Items.Add(RESTAuthMethodToString(LAuthMethod));
  FINALLY
    cmb_AuthMethod.EndUpdate;
  END;

  /// try to set the itemindex to the default-value
  if (cmb_AuthMethod.Items.IndexOf(RESTAuthMethodToString(DefaultRESTAuthMethod)) > -1) then
    cmb_AuthMethod.ItemIndex := cmb_AuthMethod.Items.IndexOf(RESTAuthMethodToString(DefaultRESTAuthMethod));
end;

procedure Tfrm_Main.InitRequestMethodCombo;
var
  LRequestMethod: TRESTRequestMethod;
begin
  cmb_RequestMethod.BeginUpdate;
  TRY
    cmb_RequestMethod.Clear;
    for LRequestMethod IN [Low(TRESTRequestMethod) .. High(TRESTRequestMethod)] do
      cmb_RequestMethod.Items.Add(RESTRequestMethodToString(LRequestMethod));
  FINALLY
    cmb_RequestMethod.EndUpdate;
  END;

  /// try to set the itemindex to the default-value
  if (cmb_RequestMethod.Items.IndexOf(RESTRequestMethodToString(DefaultRESTRequestMethod)) > -1) then
    cmb_RequestMethod.ItemIndex := cmb_RequestMethod.Items.IndexOf(RESTRequestMethodToString(DefaultRESTRequestMethod));
end;

procedure Tfrm_Main.KeyDown(var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if (Key = vkF9) then
  begin
    Key := 0;
    DoExecuteRequest;
  end;

  inherited;
end;

procedure Tfrm_Main.FillReponseContentMemo;
var
  LJson: TJSONValue;
begin
  // Json or not?
  LJson := RESTResponse.JSONValue;
  if Assigned(LJson) then
  begin
    // it's Json
    memo_ResponseBody.Lines.Text := TJson.Format(LJson);
    LabelJson.Text := 'Content is valid JSON';
    // EditRootElement.Enabled := true;
    // ButtonRootElement.Enabled := true;
  end
  else
  begin
    // pure text
    memo_ResponseBody.Lines.Text := RESTResponse.Content;
    if RESTResponse.RootElement <> '' then
      LabelJson.Text := 'Invalid root element'
    else
      LabelJson.Text := 'Content is not JSON';
    // EditRootElement.Enabled := false;
    // ButtonRootElement.Enabled := false;
  end;
  memo_ResponseBody.Repaint;
end;

procedure Tfrm_Main.ConfigureProxyServer;
begin
  if cbProxy.IsChecked then
  begin
    RESTClient.ProxyServer := edt_ProxyServer.Text;
    RESTClient.ProxyPort := Trunc(edt_ProxyPort.Value);
    RESTClient.ProxyUsername := edt_ProxyUser.Text;
    RESTClient.ProxyPassword := edt_ProxyPass.Text;
  end
  else
  begin
    RESTClient.ProxyServer := '';
    RESTClient.ProxyPort := 0;
    RESTClient.ProxyUsername := '';
    RESTClient.ProxyPassword := '';
  end;
end;

procedure Tfrm_Main.lbl_BaseURLCaptionClick(Sender: TObject);
begin
  DoFetchRequestParamsFromControls;
  DoAddToMRUList(FRESTParams);
end;

procedure Tfrm_Main.lb_CustomParametersDblClick(Sender: TObject);
begin
  DoEditCustomParameter;
end;

procedure Tfrm_Main.LoadProxySettings(const AFilename: string);
var
  LINI: TMemIniFile;
begin
  LINI := TMemIniFile.Create(AFilename);
  TRY
    cbProxy.IsChecked := LINI.ReadBool('proxy', 'enabled', false);
    edt_ProxyServer.Text := LINI.ReadString('proxy', 'server', '');
    edt_ProxyPort.Value := LINI.ReadInteger('proxy', 'port', 0);
    edt_ProxyUser.Text := SimpleDecryptStr(LINI.ReadString('proxy', 'username', SimpleEncryptStr('', AUTH_CRYPTO_VALUE)
      ), AUTH_CRYPTO_VALUE);
    edt_ProxyPass.Text := SimpleDecryptStr(LINI.ReadString('proxy', 'password', SimpleEncryptStr('', AUTH_CRYPTO_VALUE)
      ), AUTH_CRYPTO_VALUE);
  FINALLY
    FreeAndNil(LINI);
  END;

  /// update proxy-controls
  edt_ProxyServer.Enabled := cbProxy.IsChecked;
  edt_ProxyPort.Enabled := cbProxy.IsChecked;
  edt_ProxyUser.Enabled := cbProxy.IsChecked;
  edt_ProxyPass.Enabled := cbProxy.IsChecked;
end;

procedure Tfrm_Main.RESTResponseDataSetAdapterBeforeOpenDataSet(Sender: TObject);
begin
  ClientDataSet.CreateDataSet;
end;

procedure Tfrm_Main.SaveProxySettings(const AFilename: string);
var
  LINI: TMemIniFile;
begin
  LINI := TMemIniFile.Create(AFilename);
  TRY
    LINI.WriteBool('proxy', 'enabled', cbProxy.IsChecked);
    LINI.WriteString('proxy', 'server', edt_ProxyServer.Text);
    LINI.WriteInteger('proxy', 'port', Trunc(edt_ProxyPort.Value));
    LINI.WriteString('proxy', 'username', SimpleEncryptStr(edt_ProxyUser.Text, AUTH_CRYPTO_VALUE));
    LINI.WriteString('proxy', 'password', SimpleEncryptStr(edt_ProxyPass.Text, AUTH_CRYPTO_VALUE));

    LINI.UpdateFile;
  FINALLY
    FreeAndNil(LINI);
  END;
end;

procedure Tfrm_Main.tc_RequestChange(Sender: TObject);
begin
  DoFetchRequestParamsFromControls;
end;

initialization

DefaultStorageFolder := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetDocumentsPath) + 'RESTDebugger' +
  PathDelim;

if NOT TDirectory.Exists(DefaultStorageFolder) then
  TDirectory.CreateDirectory(DefaultStorageFolder);

end.
