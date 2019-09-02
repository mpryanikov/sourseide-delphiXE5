{*******************************************************}
{                                                       }
{               Delphi FireDAC Framework                }
{                FireDAC PostgreSQL API                 }
{                                                       }
{ Copyright(c) 2004-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}
{$I FireDAC.inc}

unit FireDAC.Phys.PGCli;

interface

uses
  FireDAC.Stan.Intf;

const
  // Base PostgreSQL datatypes
  SQL_BOOL = 16;
  SQL_BYTEA = 17;
  SQL_CHAR = 18;         // Internal
  SQL_NAME = 19;         // Internal
  SQL_INT8 = 20;
  SQL_INT2 = 21;
  SQL_INT2VECTOR = 22;   // Internal
  SQL_INT4 = 23;
  SQL_REGPROC = 24;
  SQL_TEXT = 25;
  SQL_OID = 26;          // Internal
  SQL_TID = 27;
  SQL_XID = 28;
  SQL_CID = 29;          // Internal
  SQL_OIDVECTOR = 30;    // Internal
  SQL_XML = 142;
  SQL_SMGR = 210;
  SQL_POINT = 600;
  SQL_LSEG = 601;
  SQL_PATH = 602;
  SQL_BOX = 603;
  SQL_POLYGON = 604;
  SQL_LINE = 628;
  SQL_CIDR = 650;        // Internal
  SQL_FLOAT4 = 700;
  SQL_FLOAT8 = 701;
  SQL_ABSTIME = 702;
  SQL_RELTIME = 703;
  SQL_TINTERVAL = 704;
  SQL_UNKNOWN = 705;     // Internal
  SQL_CIRCLE = 718;
  SQL_CASH = 790;
  SQL_MACADDR = 829;     // Internal
  SQL_INET = 869;        // Internal
  SQL_ACLITEM = 1033;
  SQL_BPCHAR = 1042;
  SQL_VARCHAR = 1043;
  SQL_DATE = 1082;
  SQL_TIME = 1083;
  SQL_TIMESTAMP = 1114;
  SQL_TIMESTAMPTZ = 1184;
  SQL_INTERVAL = 1186;
  SQL_TIMETZ = 1266;
  SQL_BIT = 1560;
  SQL_VARBIT = 1562;
  SQL_NUMERIC = 1700;
  SQL_REFCURSOR = 1790;
  SQL_REGPROCEDURE = 2202;
  SQL_REGOPER = 2203;
  SQL_REGOPERATOR = 2204;
  SQL_REGCLASS = 2205;
  SQL_REGTYPE = 2206;
  SQL_CSTRING = 2275;
  SQL_ANY = 2276;
  SQL_VOID = 2278;       // Internal
  SQL_UUID = 2950;

  // connection status constants
  CONNECTION_OK = 0;
  CONNECTION_BAD  = 1;
  CONNECTION_STARTED = 2;
  CONNECTION_MADE = 3;
  CONNECTION_AWAITING_RESPONSE = 4;
  CONNECTION_AUTH_OK = 5;
  CONNECTION_SETENV = 6;
  CONNECTION_SSL_STARTUP = 7;
  CONNECTION_NEEDED = 8;

  // exec status constants
  PGRES_EMPTY_QUERY = 0;
  PGRES_COMMAND_OK = 1;
  PGRES_TUPLES_OK = 2;
  PGRES_COPY_OUT = 3;
  PGRES_COPY_IN = 4;
  PGRES_BAD_RESPONSE = 5;
  PGRES_NONFATAL_ERROR = 6;
  PGRES_FATAL_ERROR = 7;

  // error fields, copied from postgres_ext.h
  PG_DIAG_SEVERITY = Ord('S');
  PG_DIAG_SQLSTATE = Ord('C');
  PG_DIAG_MESSAGE_PRIMARY = Ord('M');
  PG_DIAG_MESSAGE_DETAIL = Ord('D');
  PG_DIAG_MESSAGE_HINT = Ord('H');
  PG_DIAG_STATEMENT_POSITION = Ord('P');
  PG_DIAG_INTERNAL_POSITION = Ord('p');
  PG_DIAG_INTERNAL_QUERY = Ord('q');
  PG_DIAG_CONTEXT = Ord('W');
  PG_DIAG_SOURCE_FILE = Ord('F');
  PG_DIAG_SOURCE_LINE = Ord('L');
  PG_DIAG_SOURCE_FUNCTION = Ord('R');

  // PGTransactionStatusType
	PQTRANS_IDLE = 0;		 // connection idle
	PQTRANS_ACTIVE = 1;	 // command in progress
	PQTRANS_INTRANS = 2; // idle, within transaction block
	PQTRANS_INERROR = 3; // idle, within failed transaction
	PQTRANS_UNKNOWN = 4; // cannot determine status

  // Attribute numbers for the system-defined attributes
  // Source: src\include\access\htup.h
  SelfItemPointerAttributeNumber  = -1;
  ObjectIdAttributeNumber         = -2;
  MinTransactionIdAttributeNumber = -3;
  MinCommandIdAttributeNumber     = -4;
  MaxTransactionIdAttributeNumber = -5;
  MaxCommandIdAttributeNumber     = -6;
  TableOidAttributeNumber         = -7;

  // other
  POSTGRES_EPOCH_JDATE = 2451545;
  NAMEMAXLEN = 64;
  InvalidOid = 0;
  VARHDRSZ = 4;

  NULL_LEN = -1;

type
  PGConn = Pointer;
  PPGConn = Pointer;

  // Internal pqlib structure
  pgresAttValue = record
    len: Integer;
    value: PFDAnsiString;
  end;
  PPGresAttValue = ^pgresAttValue;
  PPPGresAttValue = ^PPGresAttValue;

  // Internal pqlib structure
  pg_result = record
	  ntups: Integer;
	  numAttributes: Integer;
	  attDescs: Pointer;
	  tuples: PPPGresAttValue;	// each PGresTuple is an array of PGresAttValue's
    // ..........
  end;
  PGresult = pg_result;
  PPGresult = ^PGresult;

  PGcancel = Pointer;
  PPGcancel = Pointer;

  Oid = LongWord;
  POid = ^Oid;
  Tid = array[0..5] of Byte;
  PTid = ^Tid;

  PQConninfoOptions = record
    keyword: PFDAnsiString;   // The keyword of the option
    envvar: PFDAnsiString;    // Fallback environment variable name
    compiled: PFDAnsiString;  // Fallback compiled in default value
    val: PFDAnsiString;       // Option's current value, or NULL
    lbl: PFDAnsiString;       // Label for field in connect dialog
    dispchar: PFDAnsiString;  // Character to display for this field in a
                          // connect dialog. Values are: "" Display
                          // entered value as is "*" Password field -
                          // hide value "D"  Debug option - don't show
                          // by default
    dispsize: integer;    // Field size in characters for dialog
  end;
  PPQconninfoOption = ^PQConninfoOptions;

  PGnotify = record
    relname: PFDAnsiString;   // notification condition name
    be_pid: Integer;      // process ID of notifying server process
    extra: PFDAnsiString;     // notification parameter
  end;
  PPGnotify = ^PGnotify;

  // Notice Processing
  TPQnoticeReceiver = procedure(arg: Pointer; res: PPGresult); cdecl;
  TPQnoticeProcessor = procedure(arg: Pointer; msg: PFDAnsiString); cdecl;

{$IFDEF FireDAC_PGSQL_STATIC}

  {$L crypt.obj}
//  {$L dirent.obj}
//  {$L dirmod.obj}
  {$L encnames.obj}
  {$L fe-auth.obj}
  {$L fe-connect.obj}
  {$L fe-exec.obj}
  {$L fe-lobj.obj}
  {$L fe-misc.obj}
  {$L fe-print.obj}
  {$L fe-protocol2.obj}
  {$L fe-protocol3.obj}
  {$L fe-secure.obj}
  {$L getaddrinfo.obj}
  {$L inet_aton.obj}
  {$L ip.obj}
  {$L md5.obj}
  {$L noblock.obj}
  {$L open.obj}
  {$L pgsleep.obj}
  {$L pgstrcasecmp.obj}
  {$L pqexpbuffer.obj}
  {$L pqsignal.obj}
  {$L pthread-win32.obj}
  {$L snprintf.obj}
  {$L strlcpy.obj}
  {$L thread.obj}
  {$L wchar.obj}
  {$L win32.obj}
  {$L win32error.obj}

  // Database Connection Control Functions
  function PQconnectdb(ConnInfo: PFDAnsiString): PPGconn; cdecl; external;
  //  function PQsetdbLogin(Host, Port, Options, Tty, Db, User, Passwd: PFDAnsiString): PPGconn; cdecl; external;
  function PQconndefaults: PPQconninfoOption; cdecl; external;
  procedure PQfinish(conn: PPGconn); cdecl; external;
  procedure PQreset(conn: PPGconn); cdecl; external;

  // Connection Status Functions
  function PQdb(conn: PPGconn): PFDAnsiString; cdecl; external;
  function PQuser(conn: PPGconn): PFDAnsiString; cdecl; external;
  function PQpass(conn: PPGconn): PFDAnsiString; cdecl; external;
  function PQhost(conn: PPGconn): PFDAnsiString; cdecl; external;
  function PQport(conn: PPGconn): PFDAnsiString; cdecl; external;
  function PQtty(conn: PPGconn): PFDAnsiString; cdecl; external;
  function PQoptions(conn: PPGconn): PFDAnsiString; cdecl; external;
  function PQstatus(conn: PPGconn): Integer; cdecl; external;

  function PQparameterStatus(conn: PGconn; ParamName: PFDAnsiString): PFDAnsiString; cdecl; external;
  function PQtransactionStatus(conn: PGconn): Integer; cdecl; external;
  function PQserverVersion(conn: PPGconn): Integer; cdecl; external;
  function PQprotocolVersion(conn: PPGconn): Integer; cdecl; external;
  function PQsetClientEncoding(conn: PPGconn; encoding: PFDAnsiString): Integer; cdecl; external;
  function PQclientEncoding(conn: PPGconn): Integer; cdecl; external;
  function pg_encoding_to_char(encoding_id: Integer): PFDAnsiString; cdecl; external;

  function PQerrorMessage(conn: PPGconn): PByte; cdecl; external;
  function PQbackendPID(conn: PPGconn): Integer; cdecl; external;
  // SSL *_PQgetssl(const PGconn *conn);

  // Command Execution Functions
  function PQexec(conn: PPGconn; command: PByte): PPGresult; cdecl; external;

  function PQexecParams(conn: PPGconn; command: PByte; nParams: integer;
    paramTypes: PInteger; ParamValues: Pointer; paramLengths: PInteger;
    paramFormats: Integer; resultFormat: Integer): PPGresult; cdecl; external;
  function PQprepare(conn: PPGconn; stmtName: PFDAnsiString; query: PByte;
    nParams: Integer; ParamTypes: POid): PPGresult; cdecl; external;
  function PQexecPrepared(conn: PPGconn; stmtName: PFDAnsiString; nParams: Integer;
    paramValues: Pointer; paramLengths: PInteger; paramFormats: PInteger;
    resultFormat: Integer): PPGresult; cdecl; external;
  function PQdescribePrepared(conn: PPGconn; stmtName: PFDAnsiString): PPGresult; cdecl; external;
  function PQdescribePortal(conn: PPGconn; portalName: PFDAnsiString): PPGresult; cdecl; external;
  function PQresultStatus(res: PPGresult): Integer; cdecl; external;
  function PQresultErrorMessage(res: PPGresult): PByte; cdecl; external;
  function PQresultErrorField(res: PPGResult; fieldcode: integer): PByte; cdecl; external;

  procedure PQclear(res: PPGresult); cdecl; external;
  function PQmakeEmptyPGresult(conn: PPGconn; status: Integer): PPGresult; cdecl; external;
  function PQntuples(res: PPGresult): Integer; cdecl; external;
  function PQnfields(res: PPGresult): Integer; cdecl; external;
  function PQfname(res: PPGresult; field_num: Integer): PByte; cdecl; external;
  function PQfnumber(res: PPGresult; field_name: PByte): Integer; cdecl; external;
  function PQftable(res: PPGresult; column_number: Integer): Oid; cdecl; external;
  function PQftablecol(res: PPGresult; column_number: Integer): Integer; cdecl; external;
  function PQfformat(res: PPGresult; column_number: Integer): Integer; cdecl; external;
  function PQftype(res: PPGresult; field_num: Integer): Oid; cdecl; external;
  function PQfmod(res: PPGresult; field_num: Integer): Integer; cdecl; external;
  function PQfsize(res: PPGresult; field_num: Integer): Integer; cdecl; external;
  function PQbinaryTuples(res: PPGresult): Integer; cdecl; external;
  function PQgetvalue(res: PPGresult; row_number, column_number: Integer): PByte; cdecl; external;
  function PQgetisnull(res: PPGresult; tup_num, field_num: Integer): Integer; cdecl; external;
  function PQgetlength(res: PPGresult; tup_num, field_num: Integer): Integer; cdecl; external;
  function PQnparams(res: PPGresult): Integer; cdecl; external;
  function PQparamtype(res: PPGresult; param_number: Integer): Oid; cdecl; external;

  function PQcmdStatus(res: PPGresult): PFDAnsiString; cdecl; external;
  function PQoidValue(res: PPGresult): Oid; cdecl; external;
  function PQoidStatus(res: PPGresult): PFDAnsiString; cdecl; external;
  function PQcmdTuples(res: PPGresult): PFDAnsiString; cdecl; external;

  // Notice Processing
  function PQsetNoticeReceiver(conn: PPGconn; proc: TPQnoticeReceiver;
    arg: Pointer): TPQnoticeReceiver; cdecl; external;
  function PQsetNoticeProcessor(conn: PPGconn; proc: TPQnoticeProcessor;
    arg: Pointer): TPQnoticeProcessor; cdecl; external;

  // Cancelling Queries in Progress
  function PQgetCancel(conn: PPGconn): PPGcancel; cdecl; external;
  procedure PQfreeCancel(cancel: PPGcancel); cdecl; external;
  function PQcancel(cancel: PPGcancel; errbuf: PFDAnsiString; errbufsize: Integer): Integer; cdecl; external;

  // Large Objects support
  function lo_creat(conn: PPGconn; mode: Integer): Oid; cdecl; external;
  function lo_open(conn: PPGconn; objoid: Oid; mode: Integer): Integer; cdecl; external;
  function lo_write(conn: PPGconn; fd: Integer; buf: Pointer; len: Integer): Integer; cdecl; external;
  function lo_read(conn: PPGconn; fd: Integer; buf: Pointer; len: Integer): Integer; cdecl; external;
  function lo_lseek(conn: PPGconn; fd: Integer; offset: Integer; whence: Integer): Integer; cdecl; external;
  function lo_tell(conn: PPGconn; fd: Integer): Integer; cdecl; external;
  function lo_close(conn: PPGconn; fd: Integer): Integer; cdecl; external;
  function lo_unlink(conn: PPGconn; objoid: Oid): Integer; cdecl; external;

  function PQgetResult(conn: PPGconn): PPGresult; cdecl; external;

  // Functions Associated with the COPY Command
  function PQputCopyData(conn: PPGconn; buffer: Pointer; nbytes: Integer): Integer; cdecl; external;
  function PQputCopyEnd(conn: PPGconn; errormsg: Pointer): Integer; cdecl; external;

  // Async notifications
  // Check after: prepare, execute, describe, isbusy, getresult, putcopydata
  function PQnotifies(conn: PPGconn): PPGnotify; cdecl; external;
  procedure PQfreemem(ptr: Pointer); cdecl; external;
  function PQconsumeInput(conn: PPGconn): Integer; cdecl; external;
  function PQsocket(conn: PPGconn): Integer; cdecl; external;
{$ENDIF}

type
  // Database Connection Control Functions
  TPQconnectdb = function(ConnInfo: PFDAnsiString): PPGconn; cdecl;
  //  TPQsetdbLogin = function(Host, Port, Options, Tty, Db, User, Passwd: PFDAnsiString): PPGconn; cdecl;
  TPQconndefaults = function: PPQconninfoOption; cdecl;
  TPQfinish = procedure(conn: PPGconn); cdecl;
  TPQreset = procedure(conn: PPGconn); cdecl;

  // Connection Status Functions
  TPQdb = function(conn: PPGconn): PFDAnsiString; cdecl;
  TPQuser = function(conn: PPGconn): PFDAnsiString; cdecl;
  TPQpass = function(conn: PPGconn): PFDAnsiString; cdecl;
  TPQhost = function(conn: PPGconn): PFDAnsiString; cdecl;
  TPQport = function(conn: PPGconn): PFDAnsiString; cdecl;
  TPQtty = function(conn: PPGconn): PFDAnsiString; cdecl;
  TPQoptions = function(conn: PPGconn): PFDAnsiString; cdecl;
  TPQstatus = function(conn: PPGconn): Integer; cdecl;

  TPQparameterStatus = function(conn: PGconn; ParamName: PFDAnsiString): PFDAnsiString; cdecl;
  TPQtransactionStatus = function(conn: PGconn): Integer; cdecl;
  TPQserverVersion = function(conn: PPGconn): Integer; cdecl;
  TPQprotocolVersion = function(conn: PPGconn): Integer; cdecl;
  TPQsetClientEncoding = function(conn: PPGconn; encoding: PFDAnsiString): Integer; cdecl;
  TPQclientEncoding = function(conn: PPGconn): Integer; cdecl;
  Tpg_encoding_to_char = function(encoding_id: Integer): PFDAnsiString; cdecl;

  TPQerrorMessage = function(conn: PPGconn): PByte; cdecl;
  TPQbackendPID = function(conn: PPGconn): Integer; cdecl;
  // SSL *PQgetssl(const PGconn *conn);

  // Command Execution Functions
  TPQexec = function(conn: PPGconn; command: PByte): PPGresult; cdecl;

  TPQexecParams = function(conn: PPGconn; command: PByte; nParams: integer;
    paramTypes: PInteger; ParamValues: Pointer; paramLengths: PInteger;
    paramFormats: PInteger; resultFormat: Integer): PPGresult; cdecl;
  TPQprepare = function(conn: PPGconn; stmtName: PFDAnsiString; query: PByte;
    nParams: Integer; ParamTypes: POid): PPGresult; cdecl;
  TPQexecPrepared = function(conn: PPGconn; stmtName: PFDAnsiString; nParams: Integer;
    paramValues: Pointer; paramLengths: PInteger; paramFormats: PInteger;
    resultFormat: Integer): PPGresult; cdecl;
  TPQdescribePrepared = function(conn: PPGconn; stmtName: PFDAnsiString): PPGresult; cdecl;
  TPQdescribePortal = function(conn: PPGconn; portalName: PFDAnsiString): PPGresult; cdecl;
  TPQresultStatus = function(res: PPGresult): Integer; cdecl;
  TPQresultErrorMessage = function(res: PPGresult): PByte; cdecl;
  TPQresultErrorField = function(res: PPGResult; fieldcode: integer): PByte; cdecl;

  TPQclear = procedure(res: PPGresult); cdecl;
  TPQmakeEmptyPGresult = function(conn: PPGconn; status: Integer): PPGresult; cdecl;
  TPQntuples = function(res: PPGresult): Integer; cdecl;
  TPQnfields = function(res: PPGresult): Integer; cdecl;
  TPQfname = function(res: PPGresult; field_num: Integer): PByte; cdecl;
  TPQfnumber = function(res: PPGresult; field_name: PByte): Integer; cdecl;
  TPQftable = function(res: PPGresult; column_number: Integer): Oid; cdecl;
  TPQftablecol = function(res: PPGresult; column_number: Integer): Integer; cdecl;
  TPQfformat = function(res: PPGresult; column_number: Integer): Integer; cdecl;
  TPQftype = function(res: PPGresult; field_num: Integer): Oid; cdecl;
  TPQfmod  = function(res: PPGresult; field_num: Integer): Integer; cdecl;
  TPQfsize = function(res: PPGresult; field_num: Integer): Integer; cdecl;
  TPQbinaryTuples = function(res: PPGresult): Integer; cdecl;
  TPQgetvalue = function(res: PPGresult; row_number, column_number: Integer): PByte; cdecl;
  TPQgetisnull = function(res: PPGresult; tup_num, field_num: Integer): Integer; cdecl;
  TPQgetlength = function(res: PPGresult; tup_num, field_num: Integer): Integer; cdecl;
  TPQnparams = function(res: PPGresult): Integer; cdecl;
  TPQparamtype = function(res: PPGresult; param_number: Integer): Oid; cdecl;

  TPQcmdStatus = function(res: PPGresult): PFDAnsiString; cdecl;
  TPQoidValue = function(res: PPGresult): Oid; cdecl;
  TPQoidStatus = function(res: PPGresult): PFDAnsiString; cdecl;
  TPQcmdTuples = function(res: PPGresult): PFDAnsiString; cdecl;

  // Notice Processing
  TPQsetNoticeReceiver = function(conn: PPGconn; proc: TPQnoticeReceiver;
    arg: Pointer): TPQnoticeReceiver; cdecl;
  TPQsetNoticeProcessor = function(conn: PPGconn; proc: TPQnoticeProcessor;
    arg: Pointer): TPQnoticeProcessor; cdecl;

  // Cancelling Queries in Progress
  TPQgetCancel = function(conn: PPGconn): PPGcancel; cdecl;
  TPQfreeCancel = procedure(cancel: PPGcancel); cdecl;
  TPQcancel = function(cancel: PPGcancel; errbuf: PFDAnsiString; errbufsize: Integer): Integer; cdecl;

  // Large Objects support
  Tlo_creat = function(conn: PPGconn; mode: Integer): Oid; cdecl;
  Tlo_open = function(conn: PPGconn; objoid: Oid; mode: Integer): Integer; cdecl;
  Tlo_write = function(conn: PPGconn; fd: Integer; buf: Pointer; len: Integer): Integer; cdecl;
  Tlo_read = function(conn: PPGconn; fd: Integer; buf: Pointer; len: Integer): Integer; cdecl;
  Tlo_lseek = function(conn: PPGconn; fd: Integer; offset: Integer; whence: Integer): Integer; cdecl;
  Tlo_tell = function(conn: PPGconn; fd: Integer): Integer; cdecl;
  Tlo_close = function(conn: PPGconn; fd: Integer): Integer; cdecl;
  Tlo_unlink = function(conn: PPGconn; objoid: Oid): Integer; cdecl;

  TPQgetResult = function(conn: PPGconn): PPGresult; cdecl;

  // Functions Associated with the COPY Command
  TPQputCopyData = function(conn: PPGconn; buffer: Pointer; nbytes: Integer): Integer; cdecl;
  TPQputCopyEnd = function(conn: PPGconn; errormsg: Pointer): Integer; cdecl;

  // Async notifications
  // Check after: prepare, execute, describe, isbusy, getresult, putcopydata
  TPQnotifies = function(conn: PPGconn): PPGnotify; cdecl;
  TPQfreemem = procedure(ptr: Pointer); cdecl;
  TPQconsumeInput = function(conn: PPGconn): Integer; cdecl;
  TPQsocket = function(conn: PPGconn): Integer; cdecl;

  // Functions mentioned below are not used in FireDAC PostgreSQL driver
  // Asynchronous Command Processing
  // PQsendQuery
  // PQsendQueryParams
  // PQsendPrepare
  // PQsendQueryPrepared
  // PQsendDescribePrepared
  // PQsendDescribePortal
  // PQgetResult
  // PQconsumeInput
  // PQisBusy
  // PQsetnonblocking
  // PQisnonblocking
  // PQflush

  // The Fast-Path Interface
  // PQfn

implementation

{$IFDEF FireDAC_PGSQL_STATIC}
uses
  Winapi.Windows, Winapi.WinSock, System.ShlObj,
  FireDAC.Stan.CRTL;
{$ENDIF}

end.

