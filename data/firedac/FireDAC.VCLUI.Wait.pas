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
  {$HPPEMIT '#pragma link "FireDAC.VCLUI.Wait.obj"'}
{$ELSE}
  {$HPPEMIT '#pragma link "FireDAC.VCLUI.Wait.o"'}
{$ENDIF}

unit FireDAC.VCLUI.Wait;

interface

implementation

uses
  System.Classes, System.SysUtils, Vcl.Controls, Vcl.Forms,
  FireDAC.Stan.Consts, FireDAC.Stan.Util, FireDAC.Stan.Factory,
  FireDAC.UI.Intf, FireDAC.UI;

{-------------------------------------------------------------------------------}
{- TFDGUIxWaitCursorImpl                                                       -}
{-------------------------------------------------------------------------------}
type
  TFDGUIxWaitCursorImpl = class(TFDGUIxVisualWaitCursorImplBase)
  private
    function CheckGetCursor(out ACrs: TCursor): Boolean;
  protected
    function CheckCurCursor: Boolean; override;
    function InternalHideCursor: Boolean;  override;
    procedure InternalShowCursor; override;
  end;

{-------------------------------------------------------------------------------}
function TFDGUIxWaitCursorImpl.CheckGetCursor(out ACrs: TCursor): Boolean;
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
function TFDGUIxWaitCursorImpl.CheckCurCursor: Boolean;
var
  iCrs: TCursor;
begin
  Result := (Screen <> nil) and CheckGetCursor(iCrs) and (Screen.Cursor = iCrs);
end;

{-------------------------------------------------------------------------------}
function TFDGUIxWaitCursorImpl.InternalHideCursor: Boolean;
begin
  Result := Screen <> nil;
  if Result then
    Screen.Cursor := crDefault;
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxWaitCursorImpl.InternalShowCursor;
var
  iCrs: TCursor;
begin
  if CheckGetCursor(iCrs) then
    Screen.Cursor := iCrs;
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
  oFact := TFDSingletonFactory.Create(TFDGUIxWaitCursorImpl, IFDGUIxWaitCursor,
    C_FD_GUIxFormsProvider, 'FireDAC.VCLUI.Wait');

  GCheckGuiRunning := CheckGuiRunning;

finalization
  FDReleaseFactory(oFact);

end.
