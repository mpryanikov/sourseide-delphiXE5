{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2013 Embarcadero Technologies, Inc. }
{                                                       }
{   Copyright and license exceptions noted in source    }
{                                                       }
{*******************************************************}

unit Posix.SysUn;

{$WEAKPACKAGEUNIT}

interface

uses Posix.Base, Posix.SysSocket;

{$HPPEMIT '#include <sys/un.h>' }

{$IFDEF MACOS}
{$I osx/SysUnTypes.inc}
{$ELSEIF defined(LINUX)}
{$I linux/SysUnTypes.inc}
{$ELSEIF defined(ANDROID)}
{$I android/SysUnTypes.inc}
{$ENDIF}

implementation

end.
