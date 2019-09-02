{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit FMX.BehaviorManager;

interface

uses
  System.Generics.Collections, FMX.Types;

type
  IStyleBehavior = interface
    ['{665EC261-3E3C-41F1-9964-ADCB39B47662}']
    procedure GetSystemStyle(const Context: TFmxObject; var Style: TFmxObject);
  end;

  IDeviceNameBehavior = interface
    ['{820055FF-2005-4160-8751-6BCD492C117E}']
    procedure GetDeviceName(const Context: TFmxObject; var DeviceName: string);
  end;


  IOSVersionForStyleBehavior = interface
    ['{F55F7580-3E67-4916-8D16-A33AFC171888}']
    procedure GetMajorOSVersion(const Context: TFMXObject; var OSVersion: Integer);
  end;


  IFontBehavior = interface
    ['{25D83842-FF28-4748-90C0-7E8610141190}']
    procedure GetDefaultFontFamily(const Context: TFmxObject; var FontFamily: string);
  end;


  IListener = interface
    ['{9D325387-E1F1-4B3F-A9FB-BAFF9CE03B8C}']
    procedure GetBehaviorService(const AServiceGUID: TGUID;
      var AService: IInterface; const Context: TFmxObject);
    procedure SupportsBehaviorService(const AServiceGUID: TGUID;
      const Context: TFmxObject; var Found: Boolean); overload;
    procedure SupportsBehaviorService(const AServiceGUID: TGUID;
      var AService: IInterface; const Context: TFmxObject; var Found: Boolean); overload;
  end;

  TBehaviorServices = class
  private
    FServicesList: TDictionary<TGUID, IInterface>;
    FListenerList: TList<IListener>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddBehaviorListener(const Listener: IListener);
    procedure AddBehaviorService(const AServiceGUID: TGUID; const AService: IInterface);
    function GetBehaviorService(const AServiceGUID: TGUID;
      const Context: TFmxObject): IInterface;
    procedure RemoveBehaviorListener(const Listener: IListener);
    procedure RemoveBehaviorService(const AServiceGUID: TGUID);
    function SupportsBehaviorService(const AServiceGUID: TGUID;
      const Context: TFmxObject): Boolean; overload;
    function SupportsBehaviorService(const AServiceGUID: TGUID;
      out AService: IInterface; const Context: TFmxObject): Boolean; overload;
  end;

var
  BehaviorServices: TBehaviorServices = nil;

implementation

uses
  System.SysUtils;
{ TBehaviorServices }

procedure TBehaviorServices.AddBehaviorListener(const Listener: IListener);
begin
  FListenerList.Add(Listener);
end;

procedure TBehaviorServices.AddBehaviorService(const AServiceGUID: TGUID;
  const AService: IInterface);
begin
  if not FServicesList.ContainsKey(AServiceGUID) then
    FServicesList.Add(AServiceGUID, AService);
end;

constructor TBehaviorServices.Create;
begin
  inherited;
  FServicesList := TDictionary<TGUID, IInterface>.Create;
  FListenerList := TList<IListener>.Create;
end;

destructor TBehaviorServices.Destroy;
begin
  FServicesList.Free;
  FListenerList.Free;
  inherited;
end;

function TBehaviorServices.GetBehaviorService(
  const AServiceGUID: TGUID; const Context: TFmxObject): IInterface;
var
  Listener: IListener;
begin
  Supports(FServicesList.Items[AServiceGUID], AServiceGUID, Result);
  for Listener in FListenerList do
    Listener.GetBehaviorService(AServiceGUID, Result, Context);
end;

procedure TBehaviorServices.RemoveBehaviorListener(const Listener: IListener);
begin
  FListenerList.Remove(Listener);
end;

procedure TBehaviorServices.RemoveBehaviorService(const AServiceGUID: TGUID);
begin
  FServicesList.Remove(AServiceGUID);
end;

function TBehaviorServices.SupportsBehaviorService(const AServiceGUID: TGUID;
  out AService: IInterface; const Context: TFmxObject): Boolean;
var
  Listener: IListener;
begin
  if FServicesList.ContainsKey(AServiceGUID) then
    Result := Supports(FServicesList.Items[AServiceGUID], AServiceGUID, AService)
  else
  begin
    AService := nil;
    Result := False;
  end;
  for Listener in FListenerList do
    Listener.SupportsBehaviorService(AServiceGUID, AService, Context, Result);
end;

function TBehaviorServices.SupportsBehaviorService(
  const AServiceGUID: TGUID; const Context: TFmxObject): Boolean;
var
  Listener: IListener;
begin
  Result := FServicesList.ContainsKey(AServiceGUID);
  for Listener in FListenerList do
    Listener.SupportsBehaviorService(AServiceGUID, Context, Result);
end;

initialization
  BehaviorServices := TBehaviorServices.Create;
finalization
  BehaviorServices.Free;
end.
