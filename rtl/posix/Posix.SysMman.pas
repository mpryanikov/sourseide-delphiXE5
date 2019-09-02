{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2013 Embarcadero Technologies, Inc. }
{                                                       }
{   Copyright and license exceptions noted in source    }
{                                                       }
{*******************************************************}

unit Posix.SysMman;

{$WEAKPACKAGEUNIT}

interface

uses
  Posix.Base, Posix.SysTypes;

{$IFDEF MACOS}
{$I osx/SysMmanTypes.inc}
{$ELSEIF defined(LINUX)}
{$I linux/SysMmanTypes.inc}
{$ELSEIF defined(ANDROID)}
{$I android/SysMmanTypes.inc}
{$ENDIF}

{$I SysMmanAPI.inc}

implementation

end.
