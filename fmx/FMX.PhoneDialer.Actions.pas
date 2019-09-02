{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit FMX.PhoneDialer.Actions;

interface

uses
  System.Classes, System.Actions,
  FMX.Types, FMX.PhoneDialer, FMX.ActnList, FMX.StdActns, FMX.Consts;

type

  TPhoneCallAction = class (TSysCommonAction)
  strict private
    FPhoneDialerService: IFMXPhoneDialerService;
    FTelephoneNumber: string;
  protected
    function IsSupportedInterface: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property TelephoneNumber: string read FTelephoneNumber write FTelephoneNumber;
  end;

implementation

uses
  FMX.Platform;

{ TTakePhotoFromCameraAction }

constructor TPhoneCallAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TPlatformServices.Current.SupportsPlatformService(IFMXPhoneDialerService, IInterface(FPhoneDialerService));
end;

destructor TPhoneCallAction.Destroy;
begin
  FPhoneDialerService := nil;
  inherited Destroy;
end;

procedure TPhoneCallAction.ExecuteTarget(Target: TObject);
begin
  if Assigned(FPhoneDialerService) then
    FPhoneDialerService.Call(TelephoneNumber);
end;

function TPhoneCallAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := Supported;
end;

function TPhoneCallAction.IsSupportedInterface: Boolean;
begin
   Result := Assigned(FPhoneDialerService);
end;

end.
