{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2013 Embarcadero Technologies, Inc. }
{                                                       }
{   Copyright and license exceptions noted in source    }
{                                                       }
{*******************************************************}

unit Posix.SysTimes;

{$WEAKPACKAGEUNIT}

interface

uses Posix.Base, Posix.SysTypes, Posix.Time;

{$HPPEMIT '#include <sys/times.h>' }

{$IFDEF MACOS}
{$I osx/SysTimesTypes.inc}
{$ELSEIF defined(LINUX)}
{$I linux/SysTimesTypes.inc}
{$ELSEIF defined(ANDROID)}
{$I android/SysTimesTypes.inc}
{$ENDIF}

{$I SysTimesAPI.inc}

implementation

end.
