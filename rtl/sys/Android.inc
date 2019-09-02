{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2013 Embarcadero Technologies, Inc. }
{                                                       }
{   Copyright and license exceptions noted in source    }
{                                                       }
{*******************************************************}
const
  AndroidLib = '/usr/lib/libandroid.so';

type
  PAConfiguration = Pointer;
  PAAssetManager = Pointer;

  ANativeActivity = record
    callbacks : Pointer;
    vm : Pointer;
    env : Pointer;
    clazz : Pointer;
    internalDataPath : MarshaledAString;
    externalDataPath : MarshaledAString;
    sdkVersion : Int32;
    instance : Pointer;
    assetManager : PAAssetManager;
  end;
  PANativeActivity = ^ANativeActivity;

function AConfiguration_new: PAConfiguration; cdecl;
  external AndroidLib name 'AConfiguration_new'

procedure AConfiguration_delete(Config: PAConfiguration); cdecl;
  external AndroidLib name 'AConfiguration_delete'

procedure AConfiguration_fromAssetManager(OutConfig: PAConfiguration;AssetManager: PAAssetManager); cdecl;
  external AndroidLib name 'AConfiguration_fromAssetManager'

procedure AConfiguration_getLanguage(Config: PAConfiguration; OutLanguage: MarshaledAString); cdecl;
  external AndroidLib name 'AConfiguration_getLanguage'

procedure AConfiguration_getCountry(Config: PAConfiguration; OutCountry:  MarshaledAString); cdecl;
  external AndroidLib name 'AConfiguration_getCountry'

procedure ANativeActivity_finish(Activity: Pointer); cdecl;
  external AndroidLib name 'ANativeActivity_finish'


function GetAndroidLocaleName: string;
var
  Lang, Country: MarshaledAString;
  AConfiguration: PAConfiguration;
begin
  Lang := System.AllocMem(3);
  Country := System.AllocMem(3);
  AConfiguration := AConfiguration_new();
  AConfiguration_fromAssetManager(AConfiguration, PANativeActivity(System.DelphiActivity)^.assetManager);
  AConfiguration_getLanguage(AConfiguration, Lang);
  AConfiguration_getCountry(AConfiguration, Country);
  Result := string(Lang + '_' + Country);
  FreeMem(Lang);
  FreeMem(Country);
  AConfiguration_delete(AConfiguration);
end;

