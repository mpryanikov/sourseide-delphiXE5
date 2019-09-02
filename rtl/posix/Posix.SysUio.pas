{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2013 Embarcadero Technologies, Inc. }
{                                                       }
{   Copyright and license exceptions noted in source    }
{                                                       }
{*******************************************************}

unit Posix.SysUio;

{$WEAKPACKAGEUNIT}

interface

uses Posix.Base, Posix.SysTypes;

{$HPPEMIT '#include <sys/uio.h>' }

{$IFDEF MACOS}
{$I osx/SysUioTypes.inc}
{$ELSEIF defined(LINUX)}
{$I linux/SysUioTypes.inc}
{$ELSEIF defined(ANDROID)}
{$I android/SysUioTypes.inc}
{$ENDIF}

{$I SysUioAPI.inc}

implementation

end.
