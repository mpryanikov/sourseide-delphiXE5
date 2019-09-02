{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2013 Embarcadero Technologies, Inc. }
{                                                       }
{   Copyright and license exceptions noted in source    }
{                                                       }
{*******************************************************}

unit Posix.Time;

{$WEAKPACKAGEUNIT}

interface

uses Posix.Base, Posix.SysTypes, Posix.Signal;

{$IFDEF MACOS}
{$I osx/TimeTypes.inc}
{$ELSEIF defined(LINUX)}
{$I linux/TimeTypes.inc}
{$ELSEIF defined(ANDROID)}
{$I android/TimeTypes.inc}
{$ENDIF}

{$I TimeAPI.inc}

implementation

end.
