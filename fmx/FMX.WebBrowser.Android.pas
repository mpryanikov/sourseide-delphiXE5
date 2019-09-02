{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit FMX.WebBrowser.Android;

interface

procedure RegisterWebBrowserService;
procedure UnRegisterWebBrowserService;

implementation

uses
  System.Classes, System.Types, System.StrUtils, System.SysUtils,
  System.IOUtils, System.RTLConsts, FMX.Platform, FMX.Platform.Android, FMX.WebBrowser,
  FMX.Types, FMX.Forms, Androidapi.JNI.Webkit, Androidapi.JNI.Embarcadero,
  Androidapi.JNI.Widget, FMX.Helpers.Android, Androidapi.JNI.JavaTypes,
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNIBridge, Androidapi.JNI.Os,
  Androidapi.JNI.Net, Androidapi.NativeActivity, Androidapi.IOUtils;

type

  TAndroidWebBrowserService = class;

  TAndroidWBService = class(TWBFactoryService)
  protected
    function DoCreateWebBrowser: ICustomBrowser; override;
  end;

{ TAndroidWebBrowserService }

  TAndroidWebBrowserService = class(TInterfacedObject, ICustomBrowser)
  private
    type
      TWebBrowserListener = class(TJavaLocal,JOnWebViewListener)
      private
        FWBService : TAndroidWebBrowserService;
      public
        constructor Create(AWBService : TAndroidWebBrowserService);
        procedure doUpdateVisitedHistory(P1: JWebView; P2: JString; P3: Boolean); cdecl;
        procedure onFormResubmission(P1: JWebView; P2: JMessage; P3: JMessage); cdecl;
        procedure onLoadResource(P1: JWebView; P2: JString); cdecl;
        procedure onPageFinished(P1: JWebView; P2: JString); cdecl;
        procedure onPageStarted(P1: JWebView; P2: JString; P3: JBitmap); cdecl;
        procedure onReceivedError(P1: JWebView; P2: Integer; P3: JString; P4: JString); cdecl;
        procedure onReceivedHttpAuthRequest(P1: JWebView; P2: JHttpAuthHandler; P3: JString; P4: JString); cdecl;
        procedure onReceivedSslError(P1: JWebView; P2: JSslErrorHandler; P3: JSslError); cdecl;
        procedure onScaleChanged(P1: JWebView; P2: Single; P3: Single); cdecl;
        procedure onUnhandledKeyEvent(P1: JWebView; P2: JKeyEvent); cdecl;
        function shouldOverrideKeyEvent(P1: JWebView; P2: JKeyEvent): Boolean; cdecl;
        function shouldOverrideUrlLoading(P1: JWebView; P2: JString): Boolean; cdecl;
      end;
  private
    FListener : TWebBrowserListener;
    FScale : Single;
    FJNativeLayout: JNativeLayout;
    FJWebBrowser : JWebBrowser;
    FURL: string;
    FWebControl: TCustomWebBrowser;
    FNeedUpdateBounds: Boolean;
    FBounds: TRect;
    FRealBounds : TRect;
    procedure InitUIThread;
    procedure SetPositionUIThread;
    procedure CalcRealBorder;
    procedure SetFocus(AFocus : Boolean);
    procedure FocusEnable(Sender: TObject);
    procedure FocusDisable(Sender: TObject);
  protected
    function GetParent : TFmxObject;
    function GetVisible : Boolean;
    procedure Show;
    procedure Hide;
    procedure UpdateContentFromControl;
    procedure DoNavigate(const URL: string);
    procedure DoGoBack;
    procedure DoGoForward;
    { IFMXWebBrowserService }
    function GetURL: string;
    function GetCanGoBack: Boolean;
    function GetCanGoForward: Boolean;
    procedure SetURL(const AValue: string);
    procedure SetWebBrowserControl(const AValue: TCustomWebBrowser);
    procedure Navigate;
    procedure GoBack;
    procedure GoForward;
    procedure GoHome;

    procedure StartLoading;
    procedure FinishLoading;
    procedure FailLoadingWithError;
    procedure ShouldStartLoading(const URL: string);
  public
    constructor Create;
    destructor Free;
    property URL: string read GetURL write SetURL;
    property CanGoBack: Boolean read GetCanGoBack;
    property CanGoForward: Boolean read GetCanGoForward;
  end;

var
  WBService : TAndroidWBService;

procedure RegisterWebBrowserService;
begin
  WBService := TAndroidWBService.Create;
  TPlatformServices.Current.AddPlatformService(IFMXWBService, WBService);
end;

procedure UnregisterWebBrowserService;
begin
  TPlatformServices.Current.RemovePlatformService(IFMXWBService);
end;

function TAndroidWebBrowserService.GetCanGoBack: Boolean;
var
  CanGoBack: Boolean;
begin
  CallInUIThreadAndWaitFinishing(procedure begin
    CanGoBack := FJWebBrowser.canGoBack;
  end);
  Result := CanGoBack;
end;

function TAndroidWebBrowserService.GetCanGoForward: Boolean;
var
  CanGoForward: Boolean;
begin
  CallInUIThreadAndWaitFinishing(procedure begin
    CanGoForward := FJWebBrowser.canGoForward;
  end);
  Result := CanGoForward;
end;

function TAndroidWebBrowserService.GetParent: TFmxObject;
begin
  Result := FWebControl.Parent;
end;

function TAndroidWebBrowserService.GetURL: string;
begin
  Result := FURL;
end;

function TAndroidWebBrowserService.GetVisible: Boolean;
begin
  Result := False;
  if Assigned(FWebControl) then
    Result := FWebControl.Visible;
end;

procedure TAndroidWebBrowserService.GoBack;
begin
  CallInUIThread(DoGoBack);
end;

procedure TAndroidWebBrowserService.GoForward;
begin
  CallInUIThread(DoGoForward);
end;

procedure TAndroidWebBrowserService.GoHome;
begin

end;

procedure TAndroidWebBrowserService.Hide;
begin
  CallInUIThread(
    procedure
    begin
      if FJWebBrowser.getVisibility <> TJView.JavaClass.INVISIBLE then
      begin
        FJWebBrowser.setVisibility(TJView.JavaClass.INVISIBLE);
        FJNativeLayout.SetPosition(FRealBounds.Right * 2 , FRealBounds.Height * 2);
      end;
    end);
end;

procedure TAndroidWebBrowserService.InitUIThread;
begin
  FJWebBrowser := TJWebBrowser.JavaClass.init(SharedActivity);
  FJWebBrowser.getSettings.setJavaScriptEnabled(True);
  FListener := TWebBrowserListener.Create(Self);
  FJWebBrowser.SetWebViewListener(FListener);
  FJNativeLayout := TJNativeLayout.JavaClass.init(SharedActivity,
    MainActivity.getTextEditorProxy.getWindowToken);
  FJNativeLayout.SetPosition(100,100);
  FJNativeLayout.SetSize(300,300);
  FJNativeLayout.SetControl(FJWebBrowser);
  FJNativeLayout.SetFocus(False);
end;

procedure TAndroidWebBrowserService.Navigate;
begin
  DoNavigate(URL);
end;

procedure TAndroidWebBrowserService.SetFocus(AFocus: Boolean);
begin
  if Assigned(FJNativeLayout) then
    CallInUIThreadAndWaitFinishing(
      procedure
      begin
        FJNativeLayout.SetFocus(AFocus);
      end);
end;

procedure TAndroidWebBrowserService.SetPositionUIThread;
begin
  FJNativeLayout.SetPosition(FBounds.Left, FBounds.Top);
  FJNativeLayout.SetSize(FBounds.Right, FBounds.Bottom);
end;

procedure TAndroidWebBrowserService.SetURL(const AValue: string);
begin
  if FURL <> AValue then
    FURL:= AValue;
end;

procedure TAndroidWebBrowserService.SetWebBrowserControl(const AValue: TCustomWebBrowser);
begin
  FWebControl := AValue;
  FWebControl.OnEnter := FocusEnable;
  FWebControl.OnExit := FocusDisable;
end;

procedure TAndroidWebBrowserService.ShouldStartLoading(const URL: string);
begin
  if Assigned(FWebControl) then
    FWebControl.ShouldStartLoading(URL);
end;

procedure TAndroidWebBrowserService.Show;
begin
  CallInUIThread(SetPositionUIThread );
  CallInUIThread(
    procedure
    begin
      if FJWebBrowser.getVisibility <> TJView.JavaClass.VISIBLE then
      begin
        FJWebBrowser.setVisibility(TJView.JavaClass.VISIBLE);        
      end;
    end);
end;

procedure TAndroidWebBrowserService.StartLoading;
begin
  if Assigned(FWebControl) then
    FWebControl.StartLoading;
end;

procedure TAndroidWebBrowserService.CalcRealBorder;
var
  NativeWin: JWindow;
  DecorView: JView;
  ContentRect: JRect;
begin
  NativeWin := SharedActivity.getWindow;
  if Assigned(NativeWin) then
  begin
    ContentRect := TJRect.Create;
    DecorView := NativeWin.getDecorView;
    DecorView.getWindowVisibleDisplayFrame(ContentRect);
    FRealBounds := Rect(ContentRect.left,ContentRect.top, ContentRect.right,ContentRect.bottom);
  end;
end;

constructor TAndroidWebBrowserService.Create;
var
  ScreenSrv: IFMXScreenService;
begin
  CalcRealBorder;
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, IInterface(ScreenSrv)) then
    FScale := ScreenSrv.GetScreenScale
  else
    FScale := 1;
  CallInUIThreadAndWaitFinishing(InitUIThread);
end;

procedure TAndroidWebBrowserService.DoGoBack;
begin
  inherited;
  FJWebBrowser.goBack;
end;

procedure TAndroidWebBrowserService.DoGoForward;
begin
  inherited;
  FJWebBrowser.goForward;
end;

procedure TAndroidWebBrowserService.DoNavigate(const URL: string);
var
  NewURL: string;
begin
  NewURL := URL;
  if Pos('file://', URL) <> 0 then
  begin
    NewURL := ReplaceStr(NewURL,'file://','file:///');
    FJWebBrowser.loadUrl(StringToJString(NewURL));
  end
  else
  begin
    if Pos('http', URL) = 0 then
      Insert('http://', NewURL, 0);
    FJWebBrowser.loadUrl(StringToJString(NewURL));
  end;
  UpdateContentFromControl;
end;

procedure TAndroidWebBrowserService.FailLoadingWithError;
begin
  if Assigned(FWebControl) then
    FWebControl.FailLoadingWithError;
end;

procedure TAndroidWebBrowserService.FinishLoading;
begin
  if Assigned(FWebControl) then
    FWebControl.FinishLoading;
end;

procedure TAndroidWebBrowserService.FocusDisable(Sender: TObject);
begin
  SetFocus(False);
end;

procedure TAndroidWebBrowserService.FocusEnable(Sender: TObject);
begin
  SetFocus(True);
end;

destructor TAndroidWebBrowserService.Free;
begin

end;

procedure TAndroidWebBrowserService.UpdateContentFromControl;
var
  Pos : TPointF;

begin
  while not Assigned(FJNativeLayout) do Application.ProcessMessages;
  if Assigned(FJNativeLayout) then
  begin
    if (FWebControl <> nil)
      and not (csDesigning in FWebControl.ComponentState)
      and (FWebControl.Root <> nil)
      and (FWebControl.Root.GetObject is TCommonCustomForm) then
    begin
      if FWebControl.Parent is TCommonCustomForm then
        Pos := FRealBounds.TopLeft + FWebControl.Position.Point
      else
        Pos := FWebControl.Parent.AsIControl.LocalToScreen(FWebControl.Position.Point);
      FNeedUpdateBounds := True;
      FBounds := Rect(Round(Pos.X * FScale),Round(Pos.Y * FScale),
      Round(FWebControl.Width * FScale), Round(FWebControl.Height * FScale));
      if FWebControl.Visible and FWebControl.ParentedVisible
      and (FWebControl.Root.GetObject as TCommonCustomForm).Visible
      and (FWebControl.Root.GetObject as TCommonCustomForm).Active
      then
      begin        
        Show;
      end
      else
        Hide;
    end
    else
      Hide;
  end;
end;

{ TAndroidWBService }

function TAndroidWBService.DoCreateWebBrowser: ICustomBrowser;
var
  Browser : TAndroidWebBrowserService;
begin
  Browser := TAndroidWebBrowserService.Create;
  Result := Browser;
end;

{ TAndroidWebBrowserService.TWebBrowserListener }

constructor TAndroidWebBrowserService.TWebBrowserListener.Create(
  AWBService: TAndroidWebBrowserService);
begin
  inherited Create;
  FWBService := AWBService;
end;

procedure TAndroidWebBrowserService.TWebBrowserListener.doUpdateVisitedHistory(
  P1: JWebView; P2: JString; P3: Boolean);
begin

end;

procedure TAndroidWebBrowserService.TWebBrowserListener.onFormResubmission(
  P1: JWebView; P2, P3: JMessage);
begin

end;

procedure TAndroidWebBrowserService.TWebBrowserListener.onLoadResource(
  P1: JWebView; P2: JString);
begin

end;

procedure TAndroidWebBrowserService.TWebBrowserListener.onPageFinished(
  P1: JWebView; P2: JString);
begin
  FWBService.FURL := JStringToString(P2);
  FWBService.FinishLoading;
end;

procedure TAndroidWebBrowserService.TWebBrowserListener.onPageStarted(
  P1: JWebView; P2: JString; P3: JBitmap);
begin
  FWBService.StartLoading;
end;

procedure TAndroidWebBrowserService.TWebBrowserListener.onReceivedError(
  P1: JWebView; P2: Integer; P3, P4: JString);
begin
  FWBService.FailLoadingWithError;
end;

procedure TAndroidWebBrowserService.TWebBrowserListener.onReceivedHttpAuthRequest(
  P1: JWebView; P2: JHttpAuthHandler; P3, P4: JString);
begin

end;

procedure TAndroidWebBrowserService.TWebBrowserListener.onReceivedSslError(
  P1: JWebView; P2: JSslErrorHandler; P3: JSslError);
begin
  FWBService.FailLoadingWithError;
end;

procedure TAndroidWebBrowserService.TWebBrowserListener.onScaleChanged(
  P1: JWebView; P2, P3: Single);
begin

end;

procedure TAndroidWebBrowserService.TWebBrowserListener.onUnhandledKeyEvent(
  P1: JWebView; P2: JKeyEvent);
begin

end;

function TAndroidWebBrowserService.TWebBrowserListener.shouldOverrideKeyEvent(
  P1: JWebView; P2: JKeyEvent): Boolean;
begin
  Result := False;
end;

function TAndroidWebBrowserService.TWebBrowserListener.shouldOverrideUrlLoading(
  P1: JWebView; P2: JString): Boolean;
begin
  Result := False;
end;

end.
