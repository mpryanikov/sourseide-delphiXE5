{*******************************************************}
{                                                       }
{               Delphi FireDAC Framework                }
{               FireDAC resource strings                }
{                                                       }
{ Copyright(c) 2004-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit FireDAC.Stan.ResStrs;

interface

uses
  FireDAC.Stan.Consts;

resourcestring
  {-------------------------------------------------------------------------------}
  // Dialog captions

  S_FD_ErrorDialogDefCaption = 'FireDAC Error';
  S_FD_LoginDialogDefCaption = 'FireDAC Login';
  S_FD_AsyncDialogDefCaption = 'FireDAC Working';
  S_FD_ScriptDialogDefCaption = 'FireDAC Processing';
  S_FD_LoginDialogTestOk = 'Connection established successfully.';
  S_FD_WizardNotAccessible = 'The wizard is not implemented for this driver.';
  S_FD_LoginCredentials = 'Enter your credentials';
  S_FD_LoginNewPassword = 'Enter your new password';
  S_FD_AsyncDialogDefPrompt = 'Please wait, application is busy ...';

  {-------------------------------------------------------------------------------}
  // Error messages

  S_FD_DuplicatedName = 'Name [%s] is duplicated in the list';
  S_FD_NameNotFound = 'Object [%s] is not found';
  S_FD_ColTypeUndefined = 'Column [%s] type is unknown or undefined';
  S_FD_NoColsDefined = 'No columns defined for table';
  S_FD_CheckViolated = 'Check condition violated. Constraint [%s]';
  S_FD_CantBeginEdit = 'Cannot begin edit row';
  S_FD_CantCreateChildView = 'Cannot create child view. Relation [%s]';
  S_FD_RowCantBeDeleted = 'Cannot delete row';
  S_FD_ColMBBLob = 'Column [%s] must have blob value';
  S_FD_FixedLenDataMismatch = 'Fixed length column [%s] data length mismatch. Value length - [%d], column fixed length - [%d]';
  S_FD_RowNotInEditableState = 'Row is not in editable state';
  S_FD_ColIsReadOnly = 'Column [%s] is read only';
  S_FD_RowCantBeInserted = 'Cannot insert row into table';
  S_FD_RowColMBNotNull = 'Column [%s] value must be not null';
  S_FD_DuplicateRows = 'Duplicate row found on unique index. Constraint [%s]';
  S_FD_NoMasterRow = 'Cannot process - no parent row. Constraint [%s]';
  S_FD_HasChildRows = 'Cannot process - child rows found. Constraint [%s]';
  S_FD_CantCompareRows = 'Cannot compare rows';
  S_FD_ConvIsNotSupported = 'Data type conversion is not supported';
  S_FD_ColIsNotSearchable = 'Column [%s] is not searchable';
  S_FD_RowMayHaveSingleParent = 'Row may have only single column of [dtParentRowRef] data type';
  S_FD_CantOperateInvObj = 'Cannot read data from or write data to the invariant column [%s]. Hint: use properties and methods, like a NestedTable';
  S_FD_CantSetParentRow = 'Cannot set parent row';
  S_FD_RowIsNotNested = 'Row is not nested';
  S_FD_ColumnIsNotRef = 'Column [%s] is not reference to other row';
  S_FD_ColumnIsNotSetRef = 'Column [%s] is not reference to row set';
  S_FD_OperCNBPerfInState = 'Cannot perform operation for row state';
  S_FD_CantSetUpdReg = 'Cannot change updates registry for DatS manager [%s]';
  S_FD_TooManyAggs = 'Too many aggregate values per view';
  S_FD_GrpLvlExceeds = 'Grouping level exceeds maximum allowed for aggregate [%s]';
  S_FD_VarLenDataMismatch = 'Variable length column [%s] overflow. Value length - [%d], column maximum length - [%d]';
  S_FD_BadForeignKey = 'Invalid foreign key [%s]';
  S_FD_BadUniqueKey = 'Invalid unique key [%s]';
  S_FD_CantChngColType = 'Cannot change column [%s] data type';
  S_FD_BadRelation = 'Invalid relation [%s]';
  S_FD_CantCreateParentView = 'Cannot create parent view. Relation [%s]';
  S_FD_CantChangeTableStruct = 'Cannot change table [%s] structure, when table has rows';
  S_FD_FoundCascadeLoop = 'Found a cascading actions loop at checking foreign key [%s]';
  S_FD_RecLocked = 'Record already locked';
  S_FD_RecNotLocked = 'Record is not locked';
  S_FD_TypeIncompat = 'Assigning value [%s] is not compatible with column [%s] data type. %s';
  S_FD_ValueOutOfRange = 'Value [%s] is out of range of [%s] data type';

  S_FD_ColumnDoesnotFound = 'Column or function [%s] is not found. Hint: if the name is a function name, then add FireDAC.Stan.ExprFuncs to uses clause';
  S_FD_ExprTermination = 'Expression unexpectedly terminated';
  S_FD_ExprMBAgg = 'Expression must be aggregated';
  S_FD_ExprCantAgg = 'Expression cannot be aggregated';
  S_FD_ExprTypeMis = 'Type mismatch in expression';
  S_FD_ExprIncorrect = 'Expression is incorrect';
  S_FD_InvalidKeywordUse = 'Invalid use of keyword';
  S_FD_ExprInvalidChar = 'Invalid character found [%s]';
  S_FD_ExprNameError = 'Name is not terminated properly';
  S_FD_ExprStringError = 'String constant is not terminated properly';
  S_FD_ExprNoLParen = '''('' expected but [%s] found';
  S_FD_ExprNoRParenOrComma = ''')'' or '','' expected but [%s] found';
  S_FD_ExprNoRParen = ''')'' expected but [%s] found';
  S_FD_ExprEmptyInList = 'IN predicate list may not be empty';
  S_FD_ExprExpected = 'Expected [%s]';
  S_FD_ExprNoArith = 'Arithmetic in filter expressions not supported';
  S_FD_ExprBadScope = 'Operation cannot mix aggregate value with record-varying value';
  S_FD_ExprEmpty = 'Empty expression';
  S_FD_ExprEvalError = 'Error evaluating expression. %s';

  S_FD_DSNoBookmark = 'Bookmark is not found for dataset [%s]';
  S_FD_DSViewNotSorted = 'View [%s] is not a sorted view';
  S_FD_DSNoAdapter = 'Adapter interface must be supplied';
  S_FD_DSNoNestedMasterSource = 'Cannot set MasterSource for dataset [%s]. Nested datasets cannot have a MasterSource';
  S_FD_DSCircularDataLink = 'Cannot set MasterSource for dataset [%s]. Circular datalinks are not allowed';
  S_FD_DSRefreshError = 'Cannot refresh dataset [%s]. Cached updates must be commited or canceled and batch mode terminated before refreshing';
  S_FD_DSNoDataTable = 'Cannot open dataset [%s]. A DataTable or a DataView must be supplied. Hint: if that is TFDMemTable, use CreateDataSet or CloneCursor to open dataset';
  S_FD_DSIndNotFound = 'Index [%s] is not found for dataset [%s]';
  S_FD_DSAggNotFound = 'Aggregate [%s] is not found for dataset [%s]';
  S_FD_DSIndNotComplete = 'Index [%s] definition is not complete for dataset [%s]';
  S_FD_DSAggNotComplete = 'Aggregate [%s] definition is not complete for dataset [%s]';
  S_FD_DSCantUnidir = 'Cannot perform operation on unidirectional dataset [%s]';
  S_FD_DSIncompatBmkFields = 'Bookmark key fields [%s] are incompatible with dataset [%s] key fields [%s]';
  S_FD_DSCantEdit = 'Record editing for dataset [%s] is disabled';
  S_FD_DSCantInsert = 'Record inserting for dataset [%s] is disabled';
  S_FD_DSCantDelete = 'Record deleting for dataset [%s] is disabled';
  S_FD_DSFieldNotFound = 'Field [%s] specified within %s of DataSet [%s] does not exist';
  S_FD_DSCantOffline = 'Cannot set dataset [%s] to offline mode. Hint: check that FetchOptions.AutoFetchAll is not afDisable';
  S_FD_DSCantOffCachedUpdates = 'Cannot turn off cached updates mode for DataSet [%s]. Hint: dataset has updated rows, cancel or apply updates before action';

  S_FD_DefCircular = 'Cannot make definition [%s] circular reference';
  S_FD_DefRO = 'Cannot %s definition [%s]. It has associated connection';
  S_FD_DefCantMakePers = 'Cannot make definition persistent';
  S_FD_DefAlreadyLoaded = 'Cannot load definition list, because it is already loaded';
  S_FD_DefNotExists = 'Definition [%s] is not found in [%s]';
  S_FD_DefDupName = 'Definition name [%s] is duplicated';

  S_FD_AccSrvNotFound = 'Driver [%s] is not registered. %s';
  S_FD_AccCannotReleaseDrv = 'Driver [%s] cannot be released. Hint: Close all TFDConnection objects and release pools';
  S_FD_AccSrcNotFoundExists =
    'To register it, you can drop component [TFDPhys%sDriverLink] into your project';
  S_FD_AccSrcNotFoundNotExists =
    'Correct driver ID or define [%s] virtual driver in %s';
  S_FD_AccSrvNotDefined = 'Driver ID is not defined. Set TFDConnection.DriverName or add DriverID to your connection definition';
  S_FD_AccSrvMBConnected = 'Connection must be active';
  S_FD_AccCapabilityNotSup = 'Capability is not supported';
  S_FD_AccTxMBActive = 'Transaction [%s] must be active';
  S_FD_AccTxMBInActive = 'Transaction [%s] must be inactive. Nested transactions are disabled';
  S_FD_AccCantChngCommandState = 'Cannot change command state';
  S_FD_AccCommandMBFilled = 'Command text must not be empty';
  S_FD_AccEscapeEmptyName = 'Escape function name must not be empty';
  S_FD_AccCmdMHRowSet = 'Cannot open / define command, which does not return result sets. Hint: use Execute / ExecSQL method for non-SELECT commands';
  S_FD_AccCmdMBPrepared = 'Command must be is prepared state';
  S_FD_AccCantExecCmdWithRowSet = 'Cannot execute command returning result sets. Hint: use Open method for SELECT-like commands';
  S_FD_AccCmdMBOpen4Fetch = 'Command must be open for fetching';
  S_FD_AccExactMismatch = 'Exact %s [%d] rows, while [%d] was requested';
  S_FD_AccMetaInfoMismatch = 'Meta information mismatch';
  S_FD_AccCantLoadLibrary = 'Cannot load vendor library [%s]. %s';
  S_FD_AccCantLoadLibraryHint = 'Hint: check it is in the PATH or application EXE directories, and has %s bitness.';

  S_FD_AccCantGetLibraryEntry = 'Cannot get vendor library entry point[s]. [%s]';
  S_FD_AccSrvMBDisConnected = 'Connection must be inactive';
  S_FD_AccToManyLogins = 'Too many login retries. Allowed [%d] times';
  S_FD_AccDrvMngrMB = 'To perform operation driver manager, must be [%s]';
  S_FD_AccPrepMissed = 'Character [%s] is missed';
  S_FD_AccPrepTooLongIdent = 'Too long identifier (> 255)';
  S_FD_AccParamArrayMismatch = 'Parameter [%s] ArraySize [%d] is less than ATimes [%d]';
  S_FD_AccAsyncOperInProgress = 'Cannot perform the action, because the previous action is in progress';
  S_FD_AccEscapeIsnotSupported = 'Escape function [%s] is not supported';
  S_FD_AccMetaInfoReset = 'Define(mmReset) is only supported for metainfo retrieval';
  S_FD_AccWhereIsEmpty = 'Cannot generate update query. WHERE condition is empty';
  S_FD_AccUpdateTabUndefined = 'Cannot generate update query. Update table undefined';
  S_FD_AccNameHasErrors = 'Cannot parse object name - [%s]';
  S_FD_AccEscapeBadSyntax = 'Syntax error in escape function [%s]. %s';
  S_FD_AccShutdownTO = 'FDPhysManager shutdown timeout. Possible reason: application has not released all connection interfaces';
  S_FD_AccParTypeUnknown = 'Parameter [%s] data type is unknown. Hint: specify TFDParam.DataType or assign TFDParam value before Prepare/Execute call';
  S_FD_AccParDataMapNotSup = 'Parameter [%s] data type is not supported';
  S_FD_AccColDataMapNotSup = 'Column [%s] data type is not supported';
  S_FD_AccParDefChanged = 'Param [%s] type changed from [ft%s] to [ft%s]. Query must be reprepared. Possible reason: an assignment to a TFDParam.AsXXX property implicitly changed the parameter data type. Hint: use the TFDParam.Value or appropriate TFDParam.AsXXX property';
  S_FD_AccMetaInfoNotDefined = 'A meta data argument [%s] value must be specified';
  S_FD_AccCantAssignTxIntf = 'Cannot set default transaction';
  S_FD_AccParSetChanged = 'The set of parameters is changed. Query must be reprepared. Expected number of parameters is [%d], but actual number is [%d]. Possible reason: a parameter was added or deleted';
  S_FD_AccDataToLarge = 'Data too large for variable [%s]. Max len = [%d], actual len = [%d] Hint: set the TFDParam.Size to a greater value';
  S_FD_AccDbNotExists = 'Database [%s] does not exist';
  S_FD_AccClassNotRegistered = 'Required OLEDB provider is missing on client machine. Hint: set exact DBVersion value or install respective MS Access Database Engine: ' +
      'Access 2003 or earlier: http://support.microsoft.com/kb/239114 Access 2007: http://www.microsoft.com/download/en/details.aspx?displaylang=en&id=23734 Access 2010: http://www.microsoft.com/download/en/details.aspx?id=13255';

  S_FD_AccSysClassNotRegistered = 'JRO.JetEngine class is missing on client machine. Hint: install latest engine from: http://support.microsoft.com/kb/239114';
  S_FD_AccUnrecognizedDbFormat = 'Database format is not recognized. Possible reason: DBVersion value mismatches database version.';
  S_FD_AccNotValidPassword = 'Specified database password is invalid';
  S_FD_AccUnknownOleError = 'Unknown OLE error';

  S_FD_SvcLinkMBSet = 'To perform operation DriverLink must be specified';
  S_FD_SvcMBActive = 'To perform operation service must be active';
  S_FD_SvcCannotUninstall = 'Cannot deinstall a SQLite collation, while there are active connections';

  S_FD_DAptRecordIsDeleted = '%s command %s [%d] instead of [1] record. Possible reasons: %s';
  S_FD_DAptRecordIsDeletedReasons = 'update table does not have PK or row identifier, record has been changed/deleted by another user';
  S_FD_DAptNoSelectCmd = 'Operation cannot be performed without assigned SelectCommand';
  S_FD_DAptApplyUpdateFailed = 'Update post failed';
  S_FD_DAptCantEdit = 'Row editing disabled';
  S_FD_DAptCantInsert = 'Row inserting disabled';
  S_FD_DAptCantDelete = 'Row deleting disabled';

  S_FD_ClntSessMBSingle = 'Application must have only single FDManager';
  S_FD_ClntSessMBInactive = 'FDManager must be inactive';
  S_FD_ClntSessMBActive = 'FDManager must be active';
  S_FD_ClntDbDupName = 'Connection name [%s] must be unique';
  S_FD_ClntDbMBInactive = 'Connection [%s] must be inactive';
  S_FD_ClntDbMBActive = 'Connection [%s] must be active';
  S_FD_ClntDbLoginAborted = 'Connection [%s] establishment is canceled';
  S_FD_ClntDbCantConnPooled = 'Connection [%s] cannot be pooled. Possible reason: connection definition is not in the FDManager.ConnectionDefs list or TFDConnection.Params has additional parameters';
  S_FD_ClntDBNotFound = 'Connection [%s] is not found Possible reason: [%s] ConnectionName property is misspelled or references to nonexistent connection';
  S_FD_ClntAdaptMBActive = 'Command [%s] must be in active state';
  S_FD_ClntAdaptMBInactive = 'Command [%s] must be in inactive state';
  S_FD_ClntNotCachedUpdates = 'Dataset [%s] must be in cached update mode';
  S_FD_ClntDbNotDefined = 'Connection is not defined for [%s]. Possible reason: Connection and ConnectionName property values are both empty';
  S_FD_ClntDbMBOnline = 'Connection [%s] must be online';
  S_FD_ClntCantShareAdapt = 'Table adapter [%s] cannot be assigned to [%s], because it is already assigned to [%s] and cannot be shared across few datasets';
  S_FD_ClntConnNotMatch = 'Dataset connection does not match to called connection';
  S_FD_ClntPKNotFound = 'Table [%s] must have primary key';
  S_FD_ClntLocalSQLMisuse = 'Local SQL engine misusage by [%s]. Hint: activate connection before activating dataset';
  S_FD_ClntWrongIndex = 'Table [%s] index [%s] must be existing non-expressional index';
  S_FD_ClntDSNameEmpty = 'Dataset name must be not empty';
  S_FD_ClntDSNameNotUnique = 'Dataset name [%s] must be unique across Local SQL [%s] datasets';

  S_FD_DPNoTxtFld = 'Text field [%s] is not found';
  S_FD_DPNoSrcDS = 'Source dataset not set';
  S_FD_DPNoDestDS = 'Destination dataset not set';
  S_FD_DPNoTxtDest = 'Destination text data file name or stream must be specified';
  S_FD_DPNoTxtSrc = 'Source text data file name or stream must be specified';
  S_FD_DPBadFixedSize = 'Text field [%s] size is undefined in Fixed Size Record format';
  S_FD_DPTxtFldDup = 'Text field [%s] name is Duplicated';
  S_FD_DPBadTextFmt = 'Bad text value [%s] format for mapping item [%s]. %s';
  S_FD_DPSrcUndefined = 'Undefined source field or expression for destination field [%s]';

  S_FD_StanTimeout = 'Timeout expired';
  S_FD_StanCantGetBlob = 'Cannot get access to BLOB raw data';
  S_FD_StanParamOverflow = 'Variable length data parameter [%s] overflow. Value length - [%d], parameter data maximum length - [%d]';
  S_FD_StanCantNonblocking = 'Cannot perform nonblocking action, while other nonblocking action is in progress';
  S_FD_StanMacroNotFound = 'Macro [%s] is not found';
  S_FD_StanBadParRowIndex = 'Parameter [%s] value index [%d] is out of range [0..%d]';
  S_FD_StanPoolTooManyItems = 'Cannot acquire item (connection) from pool. Maximal number [%d] of simultaneous items (connections) reached.';
  S_FD_StanHowToReg = 'To register it, you can drop component [%s] into your project';
  S_FD_StanHowToInc = 'To register it, you can include unit [%s] into your project';
  S_FD_StanStrgInvBinFmt = 'Invalid binary storage format';
  S_FD_StanStrgCantReadProp = 'Cannot read [%s] property';
  S_FD_StanStrgCantReadObj = 'Cannot read [%s] object';
  S_FD_StanStrgCantReadCDATA = 'Cannot read RAW data of [%s] object';
  S_FD_StanStrgDictOverflow = 'Dictionary overflow';
  S_FD_StanStrgClassUnknown = 'Class [%s] is not registered';
  S_FD_StanStrgUnknownFmt = 'Unknown storage format [%s]';
  S_FD_StanStrgFileError = 'Cannot move file [%s] to [%s]. %s';
  S_FD_StanStrgInvDIntFmt = 'Invalid date interval format [%s]';

  S_FD_ScrCantExecHostCmd = 'Cannot execute host command [%s]. %s';
  S_FD_ScrStrSize1 = 'String size must be of 1 character length';
  S_FD_ScrStrNotAlphaNum = 'Character cannot be alphanumeric or whitespace';
  S_FD_ScrSetArgInvalid = 'Invalid SET command argument';
  S_FD_ScrInvalidSyntax = 'Invalid command [%s] syntax';
  S_FD_ScrAccMustSpecVar = 'ACCEPT statement must specify a variable name';
  S_FD_ScrDefReqValue = 'DEFINE requires a value following equal sign';
  S_FD_ScrVarMissedCloseBrace = 'VARIABLE has missed right brace';
  S_FD_ScrVarUnsupType = 'VARIABLE has unsupported data type';
  S_FD_ScrNotLogged = 'Cannot execute command. Not logged on';
  S_FD_ScrNoCmds = 'No script commands registered. Possible reason: FireDAC.Comp.ScriptCommands unit is not linked to the application';
  S_FD_ScrNoScript = 'No script to execute for [%s]. Possible reason: SQLScriptFileName and SQLScripts both are empty';

  S_FD_DBXParMBNotEmpty = 'Connection parameter [%s] must be not empty';
  S_FD_DBXNoDriverCfg = 'DbExpress driver configuration file [%s] is not found. Possible reason: dbExpress is not properly installed on this machine';

  S_FD_MySQLBadVersion = 'Unsupported MySQL version [%d]. Supported are client and server from v 3.20 to v 6.2';
  S_FD_MySQLCantSetPort = 'Port number cannot be changed';
  S_FD_MySQLBadParams = 'Error in parameter [%s] definition. %s';
  S_FD_MySQLCantInitEmbeddedServer = 'Failed to initialize embedded server. See MySQL log files for details';

  S_FD_OdbcVarDataTypeUnsup = 'Variable [%s] C data type [%d] is not supported';

  S_FD_OraNoCursor = 'No cursors available';
  S_FD_OraCantSetCharset = 'Cannot initialize OCI with character set [%s]. Possible reason: %s';
  S_FD_OraCantAssFILE = 'Cannot assign value to BFILE/CFILE parameter [%s]';
  S_FD_OraNoCursorParams = 'No cursor parameters are defined. Include fiMeta into FetchOptions.Items';
  S_FD_OraNotInstalled = 'OCI is not properly installed on this machine (NOE1/INIT)';
  S_FD_OraBadVersion = 'Unsupported OCI library [%s] version [%s]. At least version 8.0.3 is required (NOE2/INIT)';
  S_FD_OraBadVarType = 'Bad or undefined variable param type (NOE12/VAR)';
  S_FD_OraTooLongGTRID = 'Maximum length (%d) of GTRID exceeded - %d (NOE18/TX)';
  S_FD_OraTooLongBQUAL = 'Maximum length (%d) of BQUAL exceeded - %d (NOE19/TX)';
  S_FD_OraTooLongTXName = 'Maximum length (%d) of transaction name exceeded - %d (NOE20/TX)';
  S_FD_OraDBTNManyClBraces = 'Too many close braces in names file after alias [%s] (NOE105/DB)';
  S_FD_OraNotPLSQLObj = '[%s] is not a callable PL/SQL object (NOE130/SP)';
  S_FD_OraNotPackageProc = '[%s, #%d] is not found in [%s] package (NOE134/SP)';
  S_FD_OraBadTableType = 'Parameter with type TABLE OF BOOLEAN/RECORD not supported (use TFDQuery) (NOE135/SP)';
  S_FD_OraUnNamedRecParam = 'Parameter with type RECORD must be of named type (use TFDQuery) (NOE142/SP)';
  S_FD_OraCantUTF16 = 'To initialize OCI in UTF16 mode, OCI must be of version 8.1 or higher';
  S_FD_OraCantSetCharsetInUT16 = 'To set character set in UTF16 mode, OCI must be of version 9.2 or higher';
  S_FD_OraCantSetDiffCharset = 'Character set must be the same for all sessions';
  S_FD_OraCantConvNum = 'Cannot convert Oracle Number [%s] to TBcd';
  S_FD_OraPipeAlertToMany = 'DBMS_PIPE event alerter supports only single event name';

  S_FD_IBTraceIsActive = 'Cannot start a trace session, when there is an active one';

  S_FD_PgProcNotFound = 'Stored procedure [%s] is not found';
  S_FD_PgMultiDimArrNotSup = 'Array-typed variable [%s] dimensions [%d] are not supported. Only sigle dimensional simple type arrays are supported';
  S_FD_PgUnsupArrValueTypeNotSup = 'Array-typed variable [%s] unsupported element type [%d]. Only sigle dimensional simple type arrays are supported';
  S_FD_PgArrIndexOutOfBound = 'Array-typed variable [%s] item index [%d] is out of bounds [%d, %d]';
  S_FD_PgCannotDescribeType = 'Cannot describe type [%d]. %s';

  S_FD_SQLiteInitFailed = 'SQLite library initialization failed. Main code [%d], extended code [%d]';
  S_FD_SQLiteDBNotFound = 'Database specified by [%p] handle was not found';
  S_FD_SQLitePwdInvalid = 'Invalid password specified';
  S_FD_SQLiteVTabInvalidArgs = 'VTab: Invalid number of arguments at VTabCreate. Expected [%d], got [%d]';
  S_FD_SQLiteVTabDSNotFoundOrEmpty = 'VTab: Dataset [%s] is not found or empty';
  S_FD_SQLiteVTabDSNotSupported = 'VTab: Operation is not supported';
  S_FD_SQLiteVTabDSSPNotFound = 'VTab: Savepoint [%d] is not found';
  S_FD_SQLiteVTabDSDataModFailed = 'VTab: Dataset modification failed';
  S_FD_SQLiteVTabDSRowidInvalid = 'VTab: Explicit ROWID at INSERT is not supported';
  S_FD_SQLiteVTabDSChangedOrFreed = 'VTab: Dataset state was changed. Cannot perform operation';
  S_FD_SQLiteVTabDSNoRowExists = 'VTab: Specified row does not exist';
  S_FD_SQLiteVTabDSCursorInvalid = 'VTab: Invalid cursor';
  S_FD_SQLiteVTabCannotAttach = 'TFDLocalSQL must be attached to an active SQLite connection';
  S_FD_SQLiteVTabDataSetBusy = 'VTab: DataSet [%s] is busy by another result set';

  S_FD_ASADBToolNotFound = 'Cannot perform action. DBTOOLn.DLL is not found';

  S_FD_NexusQueryPrepareFailed = 'Query preparation failed: %s%s';
  S_FD_NexusQuerySetParamsFailed = 'Query set parameters failed: %s%s';
  S_FD_NexusQueryGetParamsFailed = 'Query get parameters failed: %s%s';
  S_FD_NexusQueryExecFailed = 'Query execution failed: %s%s';

  {-------------------------------------------------------------------------------}
  // FireDAC.Import.BDE

  S_FD_CantMakeConnDefBDEComp = 'Cannot make connection definition compatible with BDE. Reason - driver and RDBMS kind pair is unsupported';

  {-------------------------------------------------------------------------------}
  // FireDAC.Comp.DataMove

  S_FD_StartLog = 'Start Log';
  S_FD_NoErrorsLogged = 'No Errors Logged';
  S_FD_EndLog = 'End Log';

  {-------------------------------------------------------------------------------}
  // FireDAC.Dcl.Reg

  S_FD_RegIniFilter = 'Ini Files (*.ini)|*.ini|All files (*.*)|*.*';
  S_FD_RegTxtFilter = 'Text Files (*.txt)|*.txt|CSV Files (*.csv)|*.csv|All files (*.*)|*.*';
  S_FD_RegLogFilter = 'Log Files (*.log)|*.log|Text Files (*.txt)|*.txt|All files (*.*)|*.*';
  S_FD_RegSQLFilter = 'SQL Files (*.sql)|*.sql|All files (*.*)|*.*';
  S_FD_RegADSFilter = 'XML Files (*.xml)|*.xml|Binary Files (*.adb)|*.adb|All files (*.*)|*.*';
  S_FD_RegDLLFilter = 'DLL Files (*.dll)|*.dll|All files (*.*)|*.*';
  S_FD_RegExecute = '&Execute';
  S_FD_RegValidate = '&Validate';
  S_FD_RegGuessTxt = '&Guess Text Format';
  S_FD_RegAddMappins = '&Add All Mappings';
  S_FD_RegConnectionEditor = '&Connection Editor ...';
  S_FD_RegRdbmsNextRS = '&Next record set';
  S_FD_RegUpdSQLEditor = '&Update SQL Editor ...';
  S_FD_RegQEditor = '&Query Editor ...';
  S_FD_Executor = 'E&xecutor';
  S_FD_Explorer = '&Explorer';
  S_FD_Monitor = '&Monitor';
  S_FD_Preferences = '&Preferences ...';
  S_FD_Help = '&Help';
  S_FD_FAQ = 'FA&Q';
  S_FD_Forums = '&Forums';
  S_FD_About = '&About ...';
  S_FD_ADHome = 'Home Page';

  {-------------------------------------------------------------------------------}
  // FireDAC.DatS

  S_FD_NotFound = '<not found>';
  S_FD_Unnamed = 'Unnamed';

  {-------------------------------------------------------------------------------}
  // FireDAC.VCLUI.About

  S_FD_ProductAbout = '%s About';

  {-------------------------------------------------------------------------------}
  // FireDAC.VCLUI.ConnEdit

  S_FD_ParParameter = 'Parameter';
  S_FD_ParValue = 'Value';
  S_FD_ParDefault = 'Default';
  S_FD_ConnEditCaption = 'FireDAC Connection Editor - [%s]';

  {-------------------------------------------------------------------------------}
  // FireDAC.VCLUI.QEdit

  S_FD_QEditCaption = 'FireDAC Query Editor - [%s]';

  {-------------------------------------------------------------------------------}
  // FireDAC.VCLUI.USEdit

  S_FD_USEditCaption = 'FireDAC Update SQL Editor - [%s]';
  S_FD_USEditCantEdit = 'Cannot edit TFDUpdateSQL - connection is undefined';
  S_FD_USEditOpenDS = 'To get columns information would you like to execute the query ?';

  {-------------------------------------------------------------------------------}
  // FireDAC.Moni.RemoteBase

  S_FD_MonNoConnection = 'No connection';
  S_FD_MonEncounterType = 'Encounter unexpected parameter type';
  S_FD_MonEncounterParamName = 'Encounter unexpected parameter name';
  S_FD_MonEncounterBlock = 'Encounter unexpected block of parameters';
  S_FD_MonEncounterEOF = 'Encounter EOF';


  {-------------------------------------------------------------------------------}
  // FireDAC.Stan.Tracer

  sMb_TracerPropertyChangeOnlyActiveFalse = 'Tracer has to be Active=false to change the properties';
  sMb_TracerTraceFileHasToBeOpen = 'Trace file has to be open for this action';
  sMb_TracerTraceFileHasToBeClosed = 'Trace file has to be closed for this action';
  sMb_TracerTraceFileNameNotAssigned = 'Trace file name has to be assigned';

implementation

end.
