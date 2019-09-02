{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2013 Embarcadero Technologies, Inc. }
{                                                       }
{   Copyright and license exceptions noted in source    }
{                                                       }
{*******************************************************}

unit Posix.Wordexp;

{$WEAKPACKAGEUNIT}

interface

uses Posix.Base, Posix.SysTypes;

{$IFDEF MACOS}
{$I osx/WordexpTypes.inc}
{$ELSEIF defined(LINUX)}
{$I linux/WordexpTypes.inc}
{$ENDIF}

{$HPPEMIT '#include <wordexp.h>'}

{ Wordexp functions not implement in Android }
{$I WordexpAPI.inc}

implementation

end.
