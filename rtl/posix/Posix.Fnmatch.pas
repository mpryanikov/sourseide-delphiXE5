{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2013 Embarcadero Technologies, Inc. }
{                                                       }
{   Copyright and license exceptions noted in source    }
{                                                       }
{*******************************************************}

unit Posix.Fnmatch;

{$WEAKPACKAGEUNIT}

interface

uses
  Posix.Base;

{$IFDEF MACOS}
{$I osx/FnMatchTypes.inc}
{$ELSEIF defined(LINUX)}
{$I linux/FnMatchTypes.inc}
{$ELSEIF defined(ANDROID)}
{$I android/FnMatchTypes.inc}
{$ENDIF}

{$I FnMatchAPI.inc}

implementation

end.
