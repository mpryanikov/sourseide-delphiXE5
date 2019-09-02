{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2013 Embarcadero Technologies, Inc. }
{                                                       }
{   Copyright and license exceptions noted in source    }
{                                                       }
{*******************************************************}

unit Posix.Stdio;

{$WEAKPACKAGEUNIT}

interface

uses Posix.Base;

{$IFDEF MACOS}
{$I osx/StdioTypes.inc}
{$ELSEIF defined(LINUX)}
{$I linux/StdioTypes.inc}
{$ELSEIF defined(ANDROID)}
{$I android/StdioTypes.inc}
{$ENDIF}

{$I StdioAPI.inc}

implementation

end.
