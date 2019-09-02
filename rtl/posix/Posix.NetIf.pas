{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2013 Embarcadero Technologies, Inc. }
{                                                       }
{   Copyright and license exceptions noted in source    }
{                                                       }
{*******************************************************}

unit Posix.NetIf;

{$WEAKPACKAGEUNIT}

interface

uses Posix.Base, Posix.SysTypes, Posix.SysSocket;

{$HPPEMIT '#include <net/if.h>' }
{$HPPEMIT '#include <ifaddrs.h>' }

{$IFDEF MACOS}
{$I osx/NetIfTypes.inc}
{$ELSEIF defined(LINUX)}
{$I linux/NetIfTypes.inc}
{$ELSEIF defined(ANDROID)}
{$I android/NetIfTypes.inc}
{$ENDIF}

{$I NetIfAPI.inc}

implementation

end.
