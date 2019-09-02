{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2013 Embarcadero Technologies, Inc. }
{                                                       }
{   Copyright and license exceptions noted in source    }
{                                                       }
{*******************************************************}

unit Posix.SysTypes;

{$WEAKPACKAGEUNIT}

interface

uses Posix.StdDef;

{$IFDEF MACOS}
{$I osx/SysTypesTypes.inc}
{$ELSEIF defined(LINUX)}
{$I linux/SysTypesTypes.inc}
{$ELSEIF defined(ANDROID)}
{$I android/SysTypesTypes.inc}
{$ENDIF}

{$HPPEMIT '#include <pthread.h>'}


type
  size_t = Posix.StdDef.size_t;
  {$EXTERNALSYM size_t}
  Psize_t = Posix.StdDef.Psize_t;

implementation

end.
