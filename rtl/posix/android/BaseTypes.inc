{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

const
  libc = '/system/lib/libc.so';
  libpthread = '/system/lib/libc.so';
  //libiconv = '/system/lib/libc.so';  //not supported on android
  libdl = '/system/lib/libdl.so';

{$IF not Declared(_PU)}
const
  // On Android, cdecl names don't have a preceeding underscore
  _PU = '';
  {$EXTERNALSYM _PU}
{$ENDIF}

const
  _INODE_SUFFIX = '';
