{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit FMX.Controls.iOS;

interface

uses
  FMX.Types;

function GetSystemStyle(const Context: TFmxObject): TFmxObject;

implementation

uses FMX.Styles, System.Types, System.Classes, System.SysUtils,
  Fmx.BehaviorManager, FMX.Controls.Style.iOS;

{$R *.res}

function GetModel(const Context: TFmxObject): string;
var
  DeviceNameBehavior: IInterface;
begin
  DeviceNameBehavior := BehaviorServices.GetBehaviorService(IDeviceNameBehavior, Context);
  (DeviceNameBehavior as IDeviceNameBehavior).GetDeviceName(Context, Result);
end;

function GetVersion(const Context: TFmxObject): Integer;
var
  DeviceNameBehavior: IInterface;
begin
  DeviceNameBehavior := BehaviorServices.GetBehaviorService(IOSVersionForStyleBehavior, Context);
  (DeviceNameBehavior as IOSVersionForStyleBehavior).GetMajorOSVersion(Context, Result);
end;

function GetSystemStyle(const Context: TFmxObject): TFmxObject;
var
  ResName: string;
begin
  Result := nil;
  if LowerCase(GetModel(Context)).Substring(0,4) = 'ipad' then
    ResName :='ipadstyle' // ipad
  else
    ResName := 'iphonestyle';
  if GetVersion(Context) >= 7 then
  begin
    if FindResource(HInstance, 'iphonepadstyle_Modern', RT_RCDATA) <> 0 then
      Result := TStyleManager.LoadFromResource(HInstance, 'iphonepadstyle_Modern', RT_RCDATA)
  end;
  if (Result = nil) and (FindResource(HInstance, PChar(ResName), RT_RCDATA) <> 0) then
    Result := TStyleManager.LoadFromResource(HInstance, ResName, RT_RCDATA)
end;

end.



