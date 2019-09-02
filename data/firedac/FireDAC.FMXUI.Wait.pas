{*******************************************************}
{                                                       }
{               Delphi FireDAC Framework                }
{           FireDAC wait time user interface            }
{                                                       }
{ Copyright(c) 2004-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}
{$I FireDAC.inc}
{$IFDEF WIN32}
  {$HPPEMIT '#pragma link "FireDAC.FMXUI.Wait.obj"'}
{$ELSE}
  {$HPPEMIT '#pragma link "FireDAC.FMXUI.Wait.o"'}
{$ENDIF}

unit FireDAC.FMXUI.Wait;

interface

implementation

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Platform, FMX.Forms,
  FireDAC.Stan.Factory, FireDAC.Stan.Consts, FireDAC.Stan.Util,
  FireDAC.UI.Intf, FireDAC.Comp.UI, FireDAC.UI;

{-------------------------------------------------------------------------------}
{- TFDGUIxFMXWaitCursorImpl                                                    -}
{-------------------------------------------------------------------------------}
type
  TFDGUIxFMXWaitCursorImpl = class(TFDGUIxVisualWaitCursorImplBase)
  private
    function CheckGetCursor(out ACrs: TCursor): Boolean;
  protected
    function CheckCurCursor: Boolean; override;
    function InternalHideCursor: Boolean;  override;
    procedure InternalShowCursor; override;
  end;

{-------------------------------------------------------------------------------}
function TFDGUIxFMXWaitCursorImpl.CheckGetCursor(out ACrs: TCursor): Boolean;
begin
  case FWaitCursor of
    gcrDefault:   ACrs := crDefault;
    gcrHourGlass: ACrs := crHourGlass;
    gcrSQLWait:   ACrs := crSQLWait;
    gcrAppWait:   ACrs := crAppStart;
  else            ACrs := crNone;
  end;
  Result := ACrs <> crNone;
end;

{-------------------------------------------------------------------------------}
function TFDGUIxFMXWaitCursorImpl.CheckCurCursor: Boolean;
begin
  Result := False;
end;

{-------------------------------------------------------------------------------}
function TFDGUIxFMXWaitCursorImpl.InternalHideCursor: Boolean;
var
  oCrsSrv: IFMXCursorService;
begin
  Result := TPlatformServices.Current.SupportsPlatformService(IFMXCursorService, IInterface(oCrsSrv));
  if Result then
    oCrsSrv.SetCursor(crDefault);
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxFMXWaitCursorImpl.InternalShowCursor;
var
  iCrs: TCursor;
  oCrsSrv: IFMXCursorService;
begin
  if CheckGetCursor(iCrs) then
  if TPlatformServices.Current.SupportsPlatformService(IFMXCursorService, IInterface(oCrsSrv)) then
    oCrsSrv.SetCursor(iCrs);
end;

{-------------------------------------------------------------------------------}
function CheckGuiRunning: Boolean;
begin
  Result := (Application <> nil) and not Application.Terminated;
end;

{-------------------------------------------------------------------------------}
var
  oFact: TFDFactory;

initialization
  oFact := TFDSingletonFactory.Create(TFDGUIxFMXWaitCursorImpl, IFDGUIxWaitCursor,
    C_FD_GUIxFMXProvider, 'FireDAC.FMXUI.Wait');

  GCheckGuiRunning := CheckGuiRunning;

finalization
  FDReleaseFactory(oFact);

end.
