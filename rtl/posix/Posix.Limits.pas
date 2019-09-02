{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2013 Embarcadero Technologies, Inc. }
{                                                       }
{   Copyright and license exceptions noted in source    }
{                                                       }
{*******************************************************}


unit Posix.Limits;

{$WEAKPACKAGEUNIT}

interface

uses
  Posix.Base;

{$IFDEF MACOS}
{$I osx/LimitsTypes.inc}
{$ELSEIF defined(LINUX)}
{$I linux/LimitsTypes.inc}
{$ELSEIF defined(ANDROID)}
{$I android/LimitsTypes.inc}
{$ENDIF}

implementation

end.
