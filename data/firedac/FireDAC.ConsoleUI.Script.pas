{*******************************************************}
{                                                       }
{               Delphi FireDAC Framework                }
{             FireDAC script console output             }
{                                                       }
{ Copyright(c) 2004-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}
{$I FireDAC.inc}
{$IFDEF WIN32}
  {$HPPEMIT '#pragma link "FireDAC.ConsoleUI.Script.obj"'}
{$ELSE}
  {$HPPEMIT '#pragma link "FireDAC.ConsoleUI.Script.o"'}
{$ENDIF}

unit FireDAC.ConsoleUI.Script;

interface

implementation

uses
  System.Classes, System.SysUtils,
  FireDAC.Stan.Factory, FireDAC.Stan.Consts,
  FireDAC.UI.Intf, FireDAC.UI;

{-------------------------------------------------------------------------------}
{- TFDGUIxConsoleScriptImpl                                                    -}
{-------------------------------------------------------------------------------}
type
  TFDGUIxConsoleScriptImpl = class(TFDGUIxScriptImplBase)
  protected
    // IFDGUIxScriptDialog
    procedure Show; override;
    procedure Progress(const AInfoProvider: IFDGUIxScriptDialogInfoProvider); override;
    procedure Output(const AStr: String; AKind: TFDScriptOuputKind); override;
    procedure Input(const APrompt: String; var AResult: String); override;
    procedure Pause(const APrompt: String); override;
    procedure Hide; override;
  end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxConsoleScriptImpl.Show;
begin
  if FDGUIxSilent() then
    Exit;
  if Assigned(FOnShow) then
    FOnShow(Self);
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxConsoleScriptImpl.Progress(
  const AInfoProvider: IFDGUIxScriptDialogInfoProvider);
begin
  if FDGUIxSilent() then
    Exit;
  if Assigned(FOnProgress) then
    FOnProgress(Self, AInfoProvider);
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxConsoleScriptImpl.Output(const AStr: String; AKind: TFDScriptOuputKind);
begin
  if FDGUIxSilent() then
    Exit;
  if Assigned(FOnOutput) then
    FOnOutput(Self, AStr);
  Writeln(AStr);
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxConsoleScriptImpl.Input(const APrompt: String; var AResult: String);
begin
  if FDGUIxSilent() then
    Exit;
  if APrompt <> '' then
    Writeln(APrompt);
  if Assigned(FOnInput) then
    FOnInput(Self, APrompt, AResult);
  Readln(AResult);
  Writeln;
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxConsoleScriptImpl.Pause(const APrompt: String);
begin
  if FDGUIxSilent() then
    Exit;
  if APrompt <> '' then
    Writeln(APrompt)
  else
    Writeln('Press Enter to continue ...');
  if Assigned(FOnPause) then
    FOnPause(Self, APrompt);
  Readln;
end;

{-------------------------------------------------------------------------------}
procedure TFDGUIxConsoleScriptImpl.Hide;
begin
  if FDGUIxSilent() then
    Exit;
  if Assigned(FOnHide) then
    FOnHide(Self);
end;

{-------------------------------------------------------------------------------}
var
  oFact: TFDFactory;

initialization
  oFact := TFDSingletonFactory.Create(TFDGUIxConsoleScriptImpl,
    IFDGUIxScriptDialog, C_FD_GUIxConsoleProvider, 'FireDAC.ConsoleUI.Script');

finalization
  FDReleaseFactory(oFact);

end.
