{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

const
{ from sys/limits.h}

  CHAR_BIT = 8;
  {$EXTERNALSYM CHAR_BIT}

  SCHAR_MAX = $7f;
  {$EXTERNALSYM SCHAR_MAX}

  SCHAR_MIN = -$7f-1;
  {$EXTERNALSYM SCHAR_MIN}

  UCHAR_MAX = $ff;
  {$EXTERNALSYM UCHAR_MAX}

  CHAR_MAX = $7f;
  {$EXTERNALSYM CHAR_MAX}

  CHAR_MIN = -$7f-1;
  {$EXTERNALSYM CHAR_MIN}

  USHRT_MAX = $ffff;
  {$EXTERNALSYM USHRT_MAX}

  SHRT_MAX = $7fff;
  {$EXTERNALSYM SHRT_MAX}

  SHRT_MIN = -$7fff-1;
  {$EXTERNALSYM SHRT_MIN}

  UINT_MAX = $ffffffff;
  {$EXTERNALSYM UINT_MAX}

  INT_MAX = $7fffffff;
  {$EXTERNALSYM INT_MAX}

  INT_MIN = -$7fffffff-1;
  {$EXTERNALSYM INT_MIN}

  ULONG_MAX	= $ffffffff;
  {$EXTERNALSYM ULONG_MAX}

  LONG_MAX = $7fffffff;
  {$EXTERNALSYM LONG_MAX}

  LONG_MIN = -$7fffffff-1;
  {$EXTERNALSYM LONG_MIN}

  LONG_BIT = 32;
  {$EXTERNALSYM LONG_BIT}

  DBL_DIG	= 15;
  {$EXTERNALSYM DBL_DIG}

  DBL_MAX	= 1.7976931348623157E+308;
  {$EXTERNALSYM DBL_MAX}

  DBL_MIN	= 2.2250738585072014E-308;
  {$EXTERNALSYM DBL_MIN}

  FLT_DIG	= 6;
  {$EXTERNALSYM FLT_DIG}

  FLT_MAX	= 3.40282347E+38;
  {$EXTERNALSYM FLT_MAX}

  FLT_MIN	= 1.17549435E-38;
  {$EXTERNALSYM FLT_MIN}

  CHILD_MAX = 999;
  {$EXTERNALSYM CHILD_MAX}

  OPEN_MAX = 256;
  {$EXTERNALSYM OPEN_MAX}

  PAGE_SHIFT = 12;
  {$EXTERNALSYM PAGE_SHIFT}

  PAGE_SIZE = 1 shl PAGE_SHIFT;
  {$EXTERNALSYM PAGE_SIZE}

  PAGE_MASK  = not (PAGE_SIZE-1);
  {$EXTERNALSYM PAGE_MASK}

  PAGESIZE = PAGE_SIZE;
  {$EXTERNALSYM PAGESIZE}

  ULLONG_MAX = $ffffffffffffffff;
  {$EXTERNALSYM ULLONG_MAX}

  LLONG_MAX	= $7fffffffffffffff;
  {$EXTERNALSYM LLONG_MAX}

  LLONG_MIN	= -$7fffffffffffffff-1;
  {$EXTERNALSYM LLONG_MIN}

  LONG_LONG_MIN = LLONG_MIN;
  {$EXTERNALSYM LONG_LONG_MIN}

  LONG_LONG_MAX = LLONG_MAX;
  {$EXTERNALSYM LONG_LONG_MAX}

  ULONG_LONG_MAX = ULLONG_MAX;
  {$EXTERNALSYM ULONG_LONG_MAX}

{from limits.h}
  _POSIX_ARG_MAX   =  4096;
  {$EXTERNALSYM _POSIX_ARG_MAX}

  _POSIX_CHILD_MAX = 25;
  {$EXTERNALSYM _POSIX_CHILD_MAX}

  _POSIX_LINK_MAX = 8;
  {$EXTERNALSYM _POSIX_LINK_MAX}

  _POSIX_MAX_CANON = 255;
  {$EXTERNALSYM _POSIX_MAX_CANON}

  _POSIX_MAX_INPUT = 255;
  {$EXTERNALSYM _POSIX_MAX_INPUT}

  _POSIX_NAME_MAX = 14;
  {$EXTERNALSYM _POSIX_NAME_MAX}

  _POSIX_NGROUPS_MAX = 0;
  {$EXTERNALSYM _POSIX_NGROUPS_MAX}

  _POSIX_OPEN_MAX = 16;
  {$EXTERNALSYM _POSIX_OPEN_MAX}

  _POSIX_PATH_MAX = 256;
  {$EXTERNALSYM _POSIX_PATH_MAX}

  _POSIX_PIPE_BUF = 512;
  {$EXTERNALSYM _POSIX_PIPE_BUF}

  _POSIX_RE_DUP_MAX	= 255;
  {$EXTERNALSYM _POSIX_RE_DUP_MAX}

  _POSIX_SSIZE_MAX = 32767;
  {$EXTERNALSYM _POSIX_SSIZE_MAX}

  _POSIX_STREAM_MAX	= 8;
  {$EXTERNALSYM _POSIX_STREAM_MAX}

  _POSIX_SYMLINK_MAX = 255;
  {$EXTERNALSYM _POSIX_SYMLINK_MAX}

  _POSIX_SYMLOOP_MAX = 8;
  {$EXTERNALSYM _POSIX_SYMLOOP_MAX}

  _POSIX_TZNAME_MAX = 3;
  {$EXTERNALSYM _POSIX_TZNAME_MAX}

  _POSIX2_BC_BASE_MAX	= 99;
  {$EXTERNALSYM _POSIX2_BC_BASE_MAX}

  _POSIX2_BC_DIM_MAX = 2048;
  {$EXTERNALSYM _POSIX2_BC_DIM_MAX}

  _POSIX2_BC_SCALE_MAX = 99;
  {$EXTERNALSYM _POSIX2_BC_SCALE_MAX}

  _POSIX2_BC_STRING_MAX	= 1000;
  {$EXTERNALSYM _POSIX2_BC_STRING_MAX}

  _POSIX2_COLL_WEIGHTS_MAX = 2;
  {$EXTERNALSYM _POSIX2_COLL_WEIGHTS_MAX}

  _POSIX2_EXPR_NEST_MAX	= 32;
  {$EXTERNALSYM _POSIX2_EXPR_NEST_MAX}

  _POSIX2_LINE_MAX = 2048;
  {$EXTERNALSYM _POSIX2_LINE_MAX}

  _POSIX2_RE_DUP_MAX = _POSIX_RE_DUP_MAX;
  {$EXTERNALSYM _POSIX2_RE_DUP_MAX}

  _POSIX_TTY_NAME_MAX	= 9;
  {$EXTERNALSYM _POSIX_TTY_NAME_MAX}

  _POSIX_LOGIN_NAME_MAX	= 9;
  {$EXTERNALSYM _POSIX_LOGIN_NAME_MAX}

  PASS_MAX = 128;
  {$EXTERNALSYM PASS_MAX}

  NL_ARGMAX = 9;
  {$EXTERNALSYM NL_ARGMAX}

  NL_LANGMAX = 14;
  {$EXTERNALSYM NL_LANGMAX}

  NL_MSGMAX = 32767;
  {$EXTERNALSYM NL_MSGMAX}

  NL_NMAX = 1;
  {$EXTERNALSYM NL_NMAX}

  NL_SETMAX = 255;
  {$EXTERNALSYM NL_SETMAX}

  NL_TEXTMAX = 255;
  {$EXTERNALSYM NL_TEXTMAX}

  TMP_MAX = 308915776;
  {$EXTERNALSYM TMP_MAX}

 { from linux/limits.h}

  NR_OPEN = 1024;
  {$EXTERNALSYM NR_OPEN}

  NGROUPS_MAX = 65536;
  {$EXTERNALSYM NGROUPS_MAX}

  ARG_MAX = 131072;
  {$EXTERNALSYM ARG_MAX}

  LINK_MAX = 127;
  {$EXTERNALSYM LINK_MAX}

  MAX_CANON = 255;
  {$EXTERNALSYM MAX_CANON}

  MAX_INPUT = 255;
  {$EXTERNALSYM MAX_INPUT}

  NAME_MAX = 255;
  {$EXTERNALSYM NAME_MAX}

  PATH_MAX = 4096;
  {$EXTERNALSYM PATH_MAX}

  PIPE_BUF = 4096;
  {$EXTERNALSYM PIPE_BUF}

  XATTR_NAME_MAX = 255;
  {$EXTERNALSYM XATTR_NAME_MAX}

  XATTR_SIZE_MAX = 65536;
  {$EXTERNALSYM XATTR_SIZE_MAX}

  XATTR_LIST_MAX = 65536;
  {$EXTERNALSYM XATTR_LIST_MAX}

  RTSIG_MAX = 32;
  {$EXTERNALSYM RTSIG_MAX}

 { from machine/limits.h}

  MB_LEN_MAX = 1;
  {$EXTERNALSYM MB_LEN_MAX}

  SIZE_MAX = UINT_MAX;
  {$EXTERNALSYM SIZE_MAX}

  SSIZE_MAX	= INT_MAX;
  {$EXTERNALSYM SSIZE_MAX}

  LONGLONG_BIT = 64;
  {$EXTERNALSYM LONGLONG_BIT}

  LONGLONG_MIN = -$7fffffffffffffff-1;
  {$EXTERNALSYM LONGLONG_MIN}

  LONGLONG_MAX = $7fffffffffffffff;
  {$EXTERNALSYM LONGLONG_MAX}

  ULONGLONG_MAX = $ffffffffffffffff;
  {$EXTERNALSYM ULONGLONG_MAX}

