{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2013 Embarcadero Technologies, Inc. }
{                                                       }
{   Copyright and license exceptions noted in source    }
{                                                       }
{*******************************************************}

unit Posix.NetDB;

{$WEAKPACKAGEUNIT}

interface

uses Posix.Base, Posix.SysSocket, Posix.NetinetIn;

{$HPPEMIT '#include <netdb.h>' }

{$IFDEF MACOS}
{$I osx/NetDBTypes.inc}
{$ELSEIF defined(LINUX)}
{$I linux/NetDBTypes.inc}
{$ELSEIF defined(ANDROID)}
{$I android/NetDBTypes.inc}
{$ENDIF}

{$I NetDBAPI.inc}

implementation

end.
