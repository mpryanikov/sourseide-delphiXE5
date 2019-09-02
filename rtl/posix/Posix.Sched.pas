{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2013 Embarcadero Technologies, Inc. }
{                                                       }
{   Copyright and license exceptions noted in source    }
{                                                       }
{*******************************************************}

unit Posix.Sched;

{$WEAKPACKAGEUNIT}

interface

uses Posix.Base, Posix.SysTypes;

{$HPPEMIT '#include <sched.h>' }

{$IFDEF MACOS}
{$I osx/SchedTypes.inc}
{$ELSEIF defined(LINUX)}
{$I linux/SchedTypes.inc}
{$ELSEIF defined(ANDROID)}
{$I android/SchedTypes.inc}
{$ENDIF}

{$I SchedAPI.inc}

implementation

end.
