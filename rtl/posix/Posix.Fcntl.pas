{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2013 Embarcadero Technologies, Inc. }
{                                                       }
{   Copyright and license exceptions noted in source    }
{                                                       }
{*******************************************************}

unit Posix.Fcntl;

{$WEAKPACKAGEUNIT}

interface

uses Posix.Base, Posix.SysTypes;

(*$HPPEMIT '#include <fcntl.h>' *)
{$DEFINE IN_POSIXFCNTL}

{$IFDEF MACOS}
{$I osx/FcntlTypes.inc}
{$ELSEIF defined(LINUX)}
{$I linux/FcntlTypes.inc}
{$ELSEIF defined(ANDROID)}
{$I android/FcntlTypes.inc}
{$ENDIF}

{$I FcntlAPI.inc}


implementation

end.
