{*******************************************************}
{                                                       }
{               Delphi FireDAC Framework                }
{               FireDAC InterBase driver                }
{                                                       }
{ Copyright(c) 2004-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}
{$I FireDAC.inc}
{$IFDEF WIN32}
  {$HPPEMIT '#pragma link "FireDAC.Phys.IB.obj"'}
{$ELSE}
  {$HPPEMIT '#pragma link "FireDAC.Phys.IB.o"'}
{$ENDIF}

unit FireDAC.Phys.IB;

interface

uses
  System.Classes,
  FireDAC.DatS,
  FireDAC.Phys, FireDAC.Phys.IBWrapper, FireDAC.Phys.IBBase;

type
  TFDPhysIBDriverLink = class;
  TFDIBSDump = class;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or 
    pidiOSSimulator or pidiOSDevice or pidAndroid)]
  TFDPhysIBDriverLink = class(TFDPhysIBBaseDriverLink)
  protected
    function GetBaseDriverID: String; override;
  end;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or 
    pidiOSSimulator or pidiOSDevice or pidAndroid)]
  TFDIBSDump = class (TFDIBService)
  private
    FDatabase: String;
    FBackupFiles: TStrings;
    FOverwrite: Boolean;
    procedure SetBackupFiles(const AValue: TStrings);
  protected
    function CreateService(AEnv: TIBEnv): TIBService; override;
    procedure SetupService(AService: TIBService); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Dump;
  published
    property Database: String read FDatabase write FDatabase;
    property BackupFiles: TStrings read FBackupFiles write SetBackupFiles;
    property Overwrite: Boolean read FOverwrite write FOverwrite;
  end;

{-------------------------------------------------------------------------------}
implementation

uses
  System.Variants, System.SysUtils, Data.DB,
  FireDAC.Stan.Intf, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.Stan.Option,
    FireDAC.Stan.Util, FireDAC.Stan.Consts,
  FireDAC.Phys.Intf, FireDAC.Phys.IBMeta, FireDAC.Phys.SQLGenerator, FireDAC.Phys.IBCli;

type
  TFDPhysIBDriver = class;
  TFDPhysIBConnection = class;
  TFDPhysIBCommand = class;

  TFDPhysIBDriver = class(TFDPhysIBDriverBase)
  protected
    procedure InternalLoad; override;
    function InternalCreateConnection(AConnHost: TFDPhysConnectionHost): TFDPhysConnection; override;
    class function GetBaseDriverID: String; override;
    function GetConnParamCount(AKeys: TStrings): Integer; override;
    procedure GetConnParams(AKeys: TStrings; AIndex: Integer;
      var AName, AType, ADefVal, ACaption: String; var ALoginIndex: Integer); override;
  end;

  TFDPhysIBConnection = class(TFDPhysIBConnectionBase)
  protected
    function InternalCreateCommand: TFDPhysCommand; override;
    procedure BuildIBConnectParams(AParams: TStrings;
      const AConnectionDef: IFDStanConnectionDef); override;
    procedure InternalAnalyzeSession(AMessages: TStrings); override;
  end;

  TFDPhysIBCommand = class(TFDPhysIBCommandBase)
  private
    procedure DoExecuteIB2007Batch(ATimes, AOffset: Integer; var ACount: TFDCounter);
  protected
    procedure InternalExecute(ATimes, AOffset: Integer; var ACount: TFDCounter); override;
    procedure ProcessMetaColumn(ATable: TFDDatSTable; AFmtOpts: TFDFormatOptions;
      AColIndex: Integer; ARow: TFDDatSRow; ApInfo: PFDIBColInfoRec; ARowIndex: Integer); override;
  end;

{-------------------------------------------------------------------------------}
{ TFDPhysIBDriverLink                                                           }
{-------------------------------------------------------------------------------}
function TFDPhysIBDriverLink.GetBaseDriverID: String;
begin
  Result := S_FD_IBId;
end;

{-------------------------------------------------------------------------------}
{ TFDIBSDump                                                                    }
{-------------------------------------------------------------------------------}
constructor TFDIBSDump.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBackupFiles := TFDStringList.Create;
end;

{-------------------------------------------------------------------------------}
destructor TFDIBSDump.Destroy;
begin
  FDFreeAndNil(FBackupFiles);
  inherited Destroy;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBSDump.SetBackupFiles(const AValue: TStrings);
begin
  FBackupFiles.Assign(AValue);
end;

{-------------------------------------------------------------------------------}
function TFDIBSDump.CreateService(AEnv: TIBEnv): TIBService;
begin
  Result := TIBSDump.Create(AEnv, Self);
end;

{-------------------------------------------------------------------------------}
procedure TFDIBSDump.SetupService(AService: TIBService);
begin
  inherited SetupService(AService);
  TIBSDump(AService).DatabaseName := FDExpandStr(Database);
  TIBSDump(AService).BackupFiles := BackupFiles;
  FDExpandStrs(TIBSDump(AService).BackupFiles);
  TIBSDump(AService).Overwrite := Overwrite;
end;

{-------------------------------------------------------------------------------}
procedure TFDIBSDump.Dump;
begin
  Execute;
end;

{-------------------------------------------------------------------------------}
{ TFDPhysIBDriver                                                               }
{-------------------------------------------------------------------------------}
procedure TFDPhysIBDriver.InternalLoad;
var
  sHome, sLib: String;
begin
  sHome := '';
  sLib := '';
  GetVendorParams(sHome, sLib);
  FLib.LoadIB(sHome, sLib);
end;

{-------------------------------------------------------------------------------}
function TFDPhysIBDriver.InternalCreateConnection(
  AConnHost: TFDPhysConnectionHost): TFDPhysConnection;
begin
  Result := TFDPhysIBConnection.Create(Self, AConnHost);
end;

{-------------------------------------------------------------------------------}
class function TFDPhysIBDriver.GetBaseDriverID: String;
begin
  Result := S_FD_IBId;
end;

{-------------------------------------------------------------------------------}
function TFDPhysIBDriver.GetConnParamCount(AKeys: TStrings): Integer;
begin
  Result := inherited GetConnParamCount(AKeys) + 2;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBDriver.GetConnParams(AKeys: TStrings; AIndex: Integer;
  var AName, AType, ADefVal, ACaption: String; var ALoginIndex: Integer);
begin
  ALoginIndex := -1;
  if AIndex < inherited GetConnParamCount(AKeys) then begin
    inherited GetConnParams(AKeys, AIndex, AName, AType, ADefVal, ACaption, ALoginIndex);
    if AName = S_FD_ConnParam_Common_Database then
      AType := '@F:InterBase Database|*.gdb;*.ib';
  end
  else begin
    case AIndex - inherited GetConnParamCount(AKeys) of
    0:
      begin
        AName := S_FD_ConnParam_IBS_InstanceName;
        AType := '@S';
        ADefVal := '';
      end;
    1:
      begin
        AName := S_FD_ConnParam_IBS_SEPassword;
        AType := '@P';
      end;
    end;
    ACaption := AName;
  end;
end;

{-------------------------------------------------------------------------------}
{ TFDPhysIBConnection                                                           }
{-------------------------------------------------------------------------------}
function TFDPhysIBConnection.InternalCreateCommand: TFDPhysCommand;
begin
  Result := TFDPhysIBCommand.Create(Self);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBConnection.BuildIBConnectParams(AParams: TStrings;
  const AConnectionDef: IFDStanConnectionDef);
begin
  inherited BuildIBConnectParams(AParams, AConnectionDef);
  if GetConnectionDef.HasValue(S_FD_ConnParam_IBS_InstanceName) then
    AParams.Add('instance_name=' + GetConnectionDef.AsString[S_FD_ConnParam_IBS_InstanceName]);
  if GetConnectionDef.HasValue(S_FD_ConnParam_IBS_SEPassword) then
    AParams.Add('sys_encrypt_password=' + GetConnectionDef.AsString[S_FD_ConnParam_IBS_SEPassword]);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBConnection.InternalAnalyzeSession(AMessages: TStrings);
begin
  inherited InternalAnalyzeSession(AMessages);

  // 3. Use driver to connect to IB server with gds32.dll
  if ServerBrand <> ibInterbase then
    AMessages.Add('Warning: Use InterBase driver to connect to InterBase');
  if IBEnv.Lib.Brand <> ibInterbase then
    AMessages.Add('Warning: Use gds32.dll with InterBase driver ');
end;

{-------------------------------------------------------------------------------}
{ TFDPhysIBCommand                                                              }
{-------------------------------------------------------------------------------}
procedure TFDPhysIBCommand.DoExecuteIB2007Batch(ATimes, AOffset: Integer;
  var ACount: TFDCounter);
var
  iBatchSize, iRows: LongWord;
  iCurTimes, iCurOffset, i: Integer;
  oResOpts: TFDResourceOptions;
begin
  oResOpts := FOptions.ResourceOptions;
  // total size of XSQLVAR, which will be send to server in single packet
  iBatchSize := FStmt.MaximumBatchSize;
  if iBatchSize > LongWord(oResOpts.ArrayDMLSize) then
    iBatchSize := LongWord(oResOpts.ArrayDMLSize);

  // If block will have only single command or there are OUT params, then go
  // by standard route - execute command once for each param array item.
  if (iBatchSize <= 1) or (FStmt.OutVars.VarCount > 0) then begin
    DoExecute(ATimes, AOffset, ACount);
    Exit;
  end;

  iCurOffset := AOffset;
  iCurTimes := LongWord(AOffset) + iBatchSize;
  while iCurOffset < ATimes do begin
    if iCurTimes > ATimes then
      iCurTimes := ATimes;
    FStmt.InVars.RowCount := Word(iCurTimes - iCurOffset);
    SetParamValues(iCurTimes, iCurOffset);
    try
      try
        FStmt.ExecuteBatch;
      finally
        if FStmt <> nil then
          for i := 0 to FStmt.InVars.RowCount - 1 do begin
            iRows := FStmt.InVars.FRowsAffected[i];
            if iRows = $FFFFFFFF then
              Break;
            if iRows > 0 then
              Inc(ACount);
          end;
      end;
    except
      on E: EIBNativeException do begin
        E.Errors[0].RowIndex := iCurOffset + ACount;
        raise;
      end;
    end;
    if FStmt.OutVars.VarCount > 0 then
      GetParamValues(iCurTimes, iCurOffset);
    FStmt.Close;
    Inc(iCurOffset, iBatchSize);
    Inc(iCurTimes, iBatchSize);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBCommand.InternalExecute(ATimes, AOffset: Integer;
  var ACount: TFDCounter);
begin
  CheckSPPrepared(skStoredProcNoCrs);
  CheckParamInfos;
  ACount := 0;
  if (ATimes - AOffset > 1) and
     (FStmt.Lib.Brand = ibInterbase) and (FStmt.Lib.Version >= ivIB110000) then
    DoExecuteIB2007Batch(ATimes, AOffset, ACount)
  else
    DoExecute(ATimes, AOffset, ACount);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysIBCommand.ProcessMetaColumn(ATable: TFDDatSTable;
  AFmtOpts: TFDFormatOptions; AColIndex: Integer; ARow: TFDDatSRow;
  ApInfo: PFDIBColInfoRec; ARowIndex: Integer);
var
  iVal: Integer;
  iSize: Longword;
  eIndexType: TFDPhysIndexKind;
  eTableType: TFDPhysTableKind;
  eScope: TFDPhysObjectScope;
  pBlob: PISCQuad;
  eDataType: TFDDataType;
  eRule: TFDPhysCascadeRuleKind;
  s: String;
  lUseBase: Boolean;

  procedure SetScope;
  begin
    if not ApInfo^.FVar.GetData(iVal, iSize) or (iVal = 0) then
      eScope := osMy
    else
      eScope := osSystem;
    ARow.SetData(AColIndex, Integer(eScope));
  end;

  procedure SetDataType;
  begin
    if not ApInfo^.FVar.GetData(iVal, iSize) or (iVal = 0) then
      eDataType := dtUnknown
    else
      case iVal of
      7: eDataType := dtInt16;
      8: eDataType := dtInt32;
      9: eDataType := dtInt64;
      10: eDataType := dtSingle;
      11: eDataType := dtDouble;
      12: eDataType := dtDate;
      13: eDataType := dtTime;
      14: eDataType := dtAnsiString;
      16: eDataType := dtInt64;
      17: eDataType := dtBoolean;
      27: eDataType := dtDouble;
      35: eDataType := dtDateTimeStamp;
      37: eDataType := dtAnsiString;
      40: eDataType := dtAnsiString;
      45: eDataType := dtBlob;
      261: eDataType := dtMemo;
      else eDataType := dtUnknown;
      end;
    ARow.SetData(AColIndex, Integer(eDataType));
  end;

begin
  lUseBase := True;
  if (IBConnection.ServerBrand = ibInterbase) and
     (IBConnection.ServerVersion < ivIB070500) then
    case GetMetaInfoKind of
    mkIndexes:
      if AColIndex = 6 then begin
        if not ApInfo^.FVar.GetData(iVal, iSize) or (iVal = 0) then
          eIndexType := ikNonUnique
        else if (iVal = 1) and not VarIsNull(ARow.GetData(5)) then
          eIndexType := ikPrimaryKey
        else
          eIndexType := ikUnique;
        ARow.SetData(AColIndex, Integer(eIndexType));
        lUseBase := False;
      end;
    mkTables:
      if AColIndex = 4 then begin
        if not ApInfo^.FVar.GetData(pBlob, iSize) then
          eTableType := tkTable
        else
          eTableType := tkView;
        ARow.SetData(AColIndex, Integer(eTableType));
        lUseBase := False;
      end
      else if AColIndex = 5 then begin
        SetScope;
        lUseBase := False;
      end;
    mkTableFields:
      if AColIndex = 6 then begin
        SetDataType;
        lUseBase := False;
      end;
    mkForeignKeys:
      if (AColIndex = 8) or (AColIndex = 9) then begin
        s := ApInfo^.FVar.AsString;
        if CompareText('RESTRICT', s) = 0 then
          eRule := ckRestrict
        else if CompareText('CASCADE', s) = 0 then
          eRule := ckCascade
        else if CompareText('SET NULL', s) = 0 then
          eRule := ckSetNull
        else if CompareText('SET DEFAULT', s) = 0 then
          eRule := ckSetDefault
        else
          eRule := ckNone;
        ARow.SetData(AColIndex, Integer(eRule));
        lUseBase := False;
      end;
    mkProcs:
      if AColIndex = 7 then begin
        SetScope;
        lUseBase := False;
      end;
    mkProcArgs:
      if AColIndex = 9 then begin
        SetDataType;
        lUseBase := False;
      end;
    mkGenerators:
      if AColIndex = 4 then begin
        SetScope;
        lUseBase := False;
      end;
    mkResultSetFields:
      if (AColIndex = 7) or (AColIndex = 8) then begin
        if not ApInfo^.FVar.GetData(pBlob, iSize) then
          iVal := 0
        else
          iVal := 1;
        ARow.SetData(AColIndex, iVal);
        lUseBase := False;
      end;
    end;
  if lUseBase then
    inherited ProcessMetaColumn(ATable, AFmtOpts, AColIndex, ARow, ApInfo, ARowIndex);
end;

{-------------------------------------------------------------------------------}
initialization
  FDPhysManager();
  FDPhysManagerObj.RegisterDriverClass(TFDPhysIBDriver);

end.