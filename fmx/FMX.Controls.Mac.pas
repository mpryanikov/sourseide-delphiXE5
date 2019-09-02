{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit FMX.Controls.Mac;

interface

uses
  FMX.Types;

function GetSystemStyle(const Context: TFmxObject): TFmxObject;
function GetSystemStyleHiRes(const Context: TFmxObject): TFmxObject;

implementation

uses FMX.Styles, System.Types, System.Classes;

{$R *.res}

function GetSystemStyle(const Context: TFmxObject): TFmxObject;
var
  ResName: string;
begin
  ResName := 'lionstyle';
  if FindResource(HInstance, PChar(ResName), RT_RCDATA) <> 0 then
    Result := TStyleManager.LoadFromResource(HInstance, ResName, RT_RCDATA)
  else
    Result := nil;
end;

function GetSystemStyleHiRes(const Context: TFmxObject): TFmxObject;
var
  ResName: string;
begin
  ResName := 'lion2xstyle';
  if FindResource(HInstance, PChar(ResName), RT_RCDATA) <> 0 then
    Result := TStyleManager.LoadFromResource(HInstance, ResName, RT_RCDATA)
  else
    Result := nil;
end;

end.
