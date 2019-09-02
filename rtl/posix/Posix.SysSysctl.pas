{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2013 Embarcadero Technologies, Inc. }
{                                                       }
{   Copyright and license exceptions noted in source    }
{                                                       }
{*******************************************************}

unit Posix.SysSysctl;

{$WEAKPACKAGEUNIT}

interface

uses Posix.Base, Posix.SysTypes;

{$HPPEMIT '#include <sys/sysctl.h>' }

{$IFDEF MACOS}
{$I osx/SysSysctlTypes.inc}
{$ELSEIF defined(LINUX)}
{$I linux/SysSysctlTypes.inc}
{$ELSEIF defined(ANDROID)}
{$I android/SysSysctlTypes.inc}
{$ENDIF}

{$I SysSysctlAPI.inc}

implementation

end.
