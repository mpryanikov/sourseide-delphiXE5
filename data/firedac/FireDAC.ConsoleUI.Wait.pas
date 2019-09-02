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
  {$HPPEMIT '#pragma link "FireDAC.ConsoleUI.Wait.obj"'}
{$ELSE}
  {$HPPEMIT '#pragma link "FireDAC.ConsoleUI.Wait.o"'}
{$ENDIF}

unit FireDAC.ConsoleUI.Wait;

interface

implementation

uses
  System.Classes, System.SysUtils,
  FireDAC.Stan.Factory, FireDAC.Stan.Consts,
  FireDAC.UI.Intf, FireDAC.UI;

{-------------------------------------------------------------------------------}
{- TFDGUIxWaitCursorConsoleImpl                                                -}
{-------------------------------------------------------------------------------}
type
  TFDGUIxWaitCursorConsoleImpl = class(TFDGUIxWaitCursorImplBase)
  protected
    // IFDGUIxWaitCursor
    procedure StartWait; override;
    procedure StopWait; override;
    procedure PushWait; override;
    procedure PopWait; override;
    procedure ForceStopWait; override;
  end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxWaitCursorConsoleImpl.StartWait;
begin
  if FDGUIxSilent() then
    Exit;
  if Assigned(FOnShow) then
    FOnShow(Self);
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxWaitCursorConsoleImpl.StopWait;
begin
  if FDGUIxSilent() then
    Exit;
  if Assigned(FOnHide) then
    FOnHide(Self);
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxWaitCursorConsoleImpl.PopWait;
begin
  // nothing
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxWaitCursorConsoleImpl.PushWait;
begin
  // nothing
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxWaitCursorConsoleImpl.ForceStopWait;
begin
  // nothing
end;

{-------------------------------------------------------------------------------}
var
  oFact: TFDFactory;

initialization
  oFact := TFDSingletonFactory.Create(TFDGUIxWaitCursorConsoleImpl,
    IFDGUIxWaitCursor, C_FD_GUIxConsoleProvider, 'FireDAC.ConsoleUI.Wait');

finalization
  FDReleaseFactory(oFact);

end.
