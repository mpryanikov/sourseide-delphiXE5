{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2013 Embarcadero Technologies, Inc. }
{                                                       }
{   Copyright and license exceptions noted in source    }
{                                                       }
{*******************************************************}

unit Posix.Iconv;

{$WEAKPACKAGEUNIT}

interface

uses Posix.Base, Posix.SysTypes;

{$IFDEF MACOS}
{$I osx/IconvTypes.inc}
{$ELSEIF defined(LINUX)}
{$I linux/IconvTypes.inc}
{$ENDIF}
{ Iconv functions not implement in Android }

{$I IconvAPI.inc}

implementation

end.
