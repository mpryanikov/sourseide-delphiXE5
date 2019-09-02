{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit FMX.Controls.Android;

interface

uses
  FMX.Types;

function GetSystemStyle(const Context: TFmxObject): TFmxObject;

implementation

uses FMX.Styles, System.Types, System.Classes, System.SysUtils;

{$R *.res}

function GetSystemStyle(const Context: TFmxObject): TFmxObject;
var
  ResName: string;
begin
  ResName := 'androidstyle';
  if FindResource(HInstance, PChar(ResName), RT_RCDATA) <> 0 then
    Result := TStyleManager.LoadFromResource(HInstance, ResName, RT_RCDATA)
  else
    Result := nil;
end;

end.





