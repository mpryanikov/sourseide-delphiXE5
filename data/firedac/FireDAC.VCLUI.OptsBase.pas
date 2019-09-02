{*******************************************************}
{                                                       }
{               Delphi FireDAC Framework                }
{               FireDAC Base dialog form                }
{                                                       }
{ Copyright(c) 2004-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}
{$I FireDAC.inc}

unit FireDAC.VCLUI.OptsBase;

interface

uses
  Winapi.Messages,
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, System.IniFiles;

type
  TfrmFDGUIxFormsOptsBase = class(TForm)
    pnlTop: TPanel;
    Image2: TImage;
    lblPrompt: TLabel;
    pnlButtons: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    Bevel2: TBevel;
    Bevel3: TBevel;
  protected
    procedure LoadFormState(AIni: TCustomIniFile); virtual;
    procedure SaveFormState(AIni: TCustomIniFile); virtual;
  public
    procedure LoadState;
    procedure SaveState;
    constructor Create(AOwner: TComponent); override;
  end;

var
  frmFDGUIxFormsOptsBase: TfrmFDGUIxFormsOptsBase;

implementation

{$R *.dfm}

uses
  Vcl.Consts,
{$IFDEF MSWINDOWS}
  System.Win.Registry,
{$ENDIF}
  FireDAC.Stan.Consts, FireDAC.Stan.Util;

{------------------------------------------------------------------------------}
{ TfrmFDGUIxFormsOptsBase                                                      }
{------------------------------------------------------------------------------}
constructor TfrmFDGUIxFormsOptsBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  btnOk.Caption := '&' + SOKButton;
  if btnCancel.Caption = '&Cancel' then
    btnCancel.Caption := '&' + SCancelButton;
end;

{------------------------------------------------------------------------------}
procedure TfrmFDGUIxFormsOptsBase.LoadFormState(AIni: TCustomIniFile);
var
  eWinState: TWindowState;
begin
  eWinState := TWindowState(AIni.ReadInteger(Name, 'State', Integer(WindowState)));
  if eWinState = wsNormal then begin
    Position := poDesigned;
    Width := AIni.ReadInteger(Name, 'Width', Width);
    Height := AIni.ReadInteger(Name, 'Height', Height);
    Top := AIni.ReadInteger(Name, 'Top', Top);
    Left := AIni.ReadInteger(Name, 'Left', Left);
  end
  else if eWinState = wsMaximized then
    WindowState := wsMaximized;
end;

{------------------------------------------------------------------------------}
procedure TfrmFDGUIxFormsOptsBase.SaveFormState(AIni: TCustomIniFile);
begin
  AIni.WriteInteger(Name, 'State', Integer(WindowState));
  if WindowState = wsNormal then begin
    AIni.WriteInteger(Name, 'Width', Width);
    AIni.WriteInteger(Name, 'Height', Height);
    AIni.WriteInteger(Name, 'Top', Top);
    AIni.WriteInteger(Name, 'Left', Left);
  end;
end;

{------------------------------------------------------------------------------}
procedure TfrmFDGUIxFormsOptsBase.LoadState;
var
  oIni: TCustomIniFile;
begin
  oIni := TFDConfigFile.Create(True);
  try
    Position := poMainFormCenter;
    if oIni.SectionExists(Name) then
      LoadFormState(oIni);
  except
  end;
  FDFree(oIni);
end;

{------------------------------------------------------------------------------}
procedure TfrmFDGUIxFormsOptsBase.SaveState;
var
  oIni: TCustomIniFile;
begin
  oIni := TFDConfigFile.Create(False);
  try
    SaveFormState(oIni);
  except
  end;
  FDFree(oIni);
end;

end.
