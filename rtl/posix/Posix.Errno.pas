{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2013 Embarcadero Technologies, Inc. }
{                                                       }
{   Copyright and license exceptions noted in source    }
{                                                       }
{*******************************************************}

unit Posix.Errno;

{$WEAKPACKAGEUNIT}

interface

uses Posix.Base;

{$IFDEF MACOS}
{$I osx/ErrnoTypes.inc}
{$ELSEIF defined(LINUX)}
{$I linux/ErrnoTypes.inc}
{$ELSEIF defined(ANDROID)}
{$I android/ErrnoTypes.inc}
{$ENDIF LINUX}

{$I ErrnoAPI.inc}

function errno: Integer; inline;
{$EXTERNALSYM errno}

implementation

function errno: Integer;
begin
  Result := __error()^;
end;

end.
