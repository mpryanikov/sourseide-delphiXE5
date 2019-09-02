{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

const
  UNIX_PATH_MAX = 108;


type
  sockaddr_un = record
    sun_family: sa_family_t;
    sun_path: packed array[0..UNIX_PATH_MAX-1] of Byte;  { Path name.  }
  end;
  {$EXTERNALSYM sockaddr_un}
  Psockaddr_un = ^sockaddr_un;
