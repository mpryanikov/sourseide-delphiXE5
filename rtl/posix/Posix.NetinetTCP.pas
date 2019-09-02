{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2013 Embarcadero Technologies, Inc. }
{                                                       }
{   Copyright and license exceptions noted in source    }
{                                                       }
{*******************************************************}

unit Posix.NetinetTCP;

{$WEAKPACKAGEUNIT}

interface

uses Posix.Base;

{$IFDEF MACOS}
{$I osx/NetinetTCPTypes.inc}
{$ELSEIF defined(LINUX)}
{$I linux/NetinetTCPTypes.inc}
{$ELSEIF defined(ANDROID)}
{$I android/NetinetTCPTypes.inc}
{$ENDIF}

implementation

end.
