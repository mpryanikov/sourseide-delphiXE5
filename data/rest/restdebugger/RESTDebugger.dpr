{*******************************************************}
{                                                       }
{             Delphi REST Client Framework              }
{                                                       }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}
program RESTDebugger;

uses
  FMX.Forms,
  uMain_frm in 'uMain_frm.pas' {frm_Main},
  uCustomHeaderDlg_frm in 'uCustomHeaderDlg_frm.pas' {frm_CustomHeaderDlg},
  uRESTObjects in 'uRESTObjects.pas',
  uMRUList in 'uMRUList.pas',
  uWait in 'uWait.pas' {Wait},
  uOAuth2_frm in 'uOAuth2_frm.pas' {frm_OAuth2},
  uOSUtils in 'uOSUtils.pas',
  uOAuth1_frm in 'uOAuth1_frm.pas' {frm_OAuth1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tfrm_Main, frm_Main);
  Application.Run;
end.
