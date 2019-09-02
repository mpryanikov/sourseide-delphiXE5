{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2013 Embarcadero Technologies, Inc. }
{                                                       }
{   Copyright and license exceptions noted in source    }
{                                                       }
{*******************************************************}

unit Posix.Pwd;

{$WEAKPACKAGEUNIT}

interface

uses Posix.Base, Posix.SysTypes;

{$HPPEMIT '#include <pwd.h>' }

// Not defined on mobile platforms (IOS and ANDROID)

{$IFDEF MACOS}
  {$IFNDEF IOS}
  {$I osx/PwdTypes.inc}
  {$ENDIF !IOS}
{$ELSEIF defined(LINUX)}
{$I linux/PwdTypes.inc}
{$ENDIF}

{$IF not defined(IOS) and not defined(ANDROID)}
{$I PwdAPI.inc}
{$ENDIF !IOS and !ANDROID}

implementation

end.
