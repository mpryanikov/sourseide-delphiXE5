{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit System.StartUpCopy;

interface

uses
  System.SysUtils;

type
  EStartUpCopyException = class(Exception);

implementation

uses
{$IFDEF ANDROID}
  Androidapi.AssetManager,
  Androidapi.NativeActivity,
  Androidapi.IOUtils,
  Posix.Unistd,
  System.RTLConsts,
{$ENDIF ANDROID}
  System.IOUtils;

{$IFDEF ANDROID}
const
  MAX_BUFF_LEN = 65536;
  ASSETS_ROOT = 'assets';
  ASSETS_ROOT_D = 'assets' + PathDelim;
  ASSETS_ROOT_D_LENGTH = 7;
  ASSETS_INTERNAL = 'internal';
  ASSETS_INTERNAL_D = 'internal' + PathDelim;
  ASSETS_DEPLOY_DIR = 'deployinfo';
  ASSETS_FILENAME = 'deployedassets.txt';
  ASSETS_FILENAME_PATH = ASSETS_DEPLOY_DIR + PathDelim + ASSETS_FILENAME;
{$ENDIF ANDROID}

{$IFDEF ANDROID}

function CopyAssetToFile(LAssetManager: PAAssetManager; const AssetFolder, AssetName: string;
              const DestinationRoot, DestFolder, FileName: string): Boolean;
var
  OrigFileName,
  DestFileName,
  DestinationPath: string;
  ReadCount, WriteCount: Integer;
  LAssetFile: PAAsset;
  FileHandle: THandle;
  Buffer: TBytes;
  M: TMarshaller;
begin
  Result := True;

  if AssetFolder = '' then
    OrigFileName := AssetName
  else
    OrigFileName := IncludeTrailingPathDelimiter(AssetFolder) +  AssetName;

  if DestFolder <> '' then
  begin
    DestinationPath := IncludeTrailingPathDelimiter(DestinationRoot) + DestFolder;
    DestFileName := IncludeTrailingPathDelimiter(DestinationRoot) + IncludeTrailingPathDelimiter(DestFolder) + FileName;
  end
  else
  begin
    DestinationPath := DestinationRoot;
    DestFileName := IncludeTrailingPathDelimiter(DestinationRoot) + FileName
  end;

  if not FileExists(DestFileName) then //do not overwrite files
  begin
    // Second Create an intermediate buffer.
    SetLength(Buffer, MAX_BUFF_LEN);
    LAssetFile := nil;
    try
      if not DirectoryExists(DestinationPath) then
      begin
        if not ForceDirectories(DestinationPath) then
        begin
          Exit(False);
        end;
      end;
      // We have a valid AssetManager. Start
      LAssetFile := AAssetManager_open(LAssetManager, M.AsUtf8(OrigFileName).ToPointer, AASSET_MODE_BUFFER);
      FileHandle := FileCreate(DestFileName);
      try
        if FileHandle = THandle(-1) then
        begin
          Exit(False);
        end;
        repeat
          ReadCount := AAsset_read(LAssetFile, @Buffer[0], MAX_BUFF_LEN);
          WriteCount := FileWrite(FileHandle, Buffer, 0, ReadCount);
        until (ReadCount <= 0) or (ReadCount <> WriteCount);

      finally
        FileClose(FileHandle);
      end;
    finally
      if (LAssetFile <> nil) then
        AAsset_close(LAssetFile);
      SetLength(Buffer, 0);
    end;
  end;
end;

function ReadAssetsDeployFile(AssetManager: PAAssetManager; var FileContent:string): boolean;
var
  Buffer: array [0..MAX_BUFF_LEN-1] of char;
  LAssetFile: PAAsset;
  ReadCount: Integer;
  M: TMarshaller;
begin
  Result := False;
  LAssetFile := AAssetManager_open(AssetManager, M.AsUTF8(ASSETS_FILENAME_PATH).ToPointer, AASSET_MODE_BUFFER);
  if Assigned(LAssetFile) then
  begin
    try
      repeat
        ReadCount := AAsset_read(LAssetFile, @Buffer, MAX_BUFF_LEN);
        if ReadCount > 0 then
          FileContent := FileContent + UTF8Tostring(@Buffer);
      until (ReadCount <= 0);
      Result := True;
    finally
      AAsset_close(LAssetFile);
    end;
  end;
end;

procedure CopyAssetsToFiles;
var
  AssetManager: PAAssetManager;
  RootDir: string;
  InternalPath: string;
  ExternalPath: string;

  procedure CopyAssetFolder(const LAssetManager:PAAssetManager; const FromFolder, ToFolder: string; IsInternal: Boolean = False);
  var
    LAssetDir: PAAssetDir;
    LFile : MarshaledAString;
    FileName: string;
    M: TMarshaller;
  begin
    // Listing the files on root directory
    LAssetDir := AAssetManager_openDir(LAssetManager, M.AsUtf8(FromFolder).ToPointer);
    if LAssetDir <> nil then
    begin
      try
        LFile := AAssetDir_getNextFileName(LAssetdir);
        while LFile <> nil do
        begin
          FileName := UTF8Tostring(LFile);
          if IsInternal then
            CopyAssetToFile(LAssetManager, FromFolder, FileName, InternalPath, ToFolder, FileName)
          else
          begin
            if ExternalPath = '' then
              raise EStartUpCopyException.Create(SExternalExtorageNotAvailable);
            CopyAssetToFile(LAssetManager, FromFolder, FileName, ExternalPath, ToFolder, FileName);
          end;
          LFile := AAssetDir_getNextFileName(LAssetDir);
        end;
      finally
        AAssetDir_close(LAssetDir);
      end;
    end;
  end;

  procedure CopyAssetFile(const LAssetManager:PAAssetManager; const FromFile, ToFile: string; IsInternal: Boolean = False);
  var
    FileName: string;
    FromFolder: string;
    ToFolder: string;
  begin
    FromFolder := ExtractFilePath(FromFile);
    ToFolder := ExtractFilePath(ToFile);
    FileName := ExtractFilename(FromFile);

    if IsInternal then
      CopyAssetToFile(LAssetManager, FromFolder, FileName, InternalPath, ToFolder, FileName)
    else
    begin
      if ExternalPath = '' then
        raise EStartUpCopyException.Create(SExternalExtorageNotAvailable);
      CopyAssetToFile(LAssetManager, FromFolder, FileName, ExternalPath, ToFolder, FileName);
    end;
  end;

  procedure ProcessDeployedFiles(const LAssetManager:PAAssetManager; aFiles: string);
  var
    I, J, LLen: integer;
    FileName: string;
  begin
    aFiles := aFiles.Replace('\','/');
    aFiles := aFiles.Replace('./','');
    I := aFiles.IndexOf(ASSETS_ROOT_D, 0);
    while (I >= 0) do
    begin
      J := aFiles.IndexOf(ASSETS_ROOT_D, I + 1);
      if J < 0 then
        LLen := aFiles.Length - I - ASSETS_ROOT_D_LENGTH
      else
        LLen := J - ASSETS_ROOT_D_LENGTH - I - 1;

      FileName := aFiles.Substring(I + ASSETS_ROOT_D_LENGTH, LLen).Trim;
      I := J;
      if FileName.StartsWith(ASSETS_INTERNAL_D) then
        CopyAssetFile(AssetManager, FileName, FileName.Substring(length(ASSETS_INTERNAL_D)), True)
      else if FileName = ASSETS_INTERNAL then
        CopyAssetFile(AssetManager, FileName, '', True)
      else
        CopyAssetFile(AssetManager, FileName, FileName, False);
    end;
  end;

var
  DeployedFiles: string;
begin
  InternalPath := GetFilesDir;
  ExternalPath := GetExternalFilesDir;

  AssetManager := ANativeActivity(system.DelphiActivity^).assetManager;
  if (AssetManager <> nil) then
  begin
    if ReadAssetsDeployFile(AssetManager, DeployedFiles) then
      ProcessDeployedFiles(AssetManager, DeployedFiles)
    else
    begin
      RootDir := '';
      CopyAssetFolder(AssetManager, RootDir, RootDir, False);

      RootDir := ASSETS_INTERNAL;
      CopyAssetFolder(AssetManager, RootDir, '', True);

      RootDir := 'StartUp';
      CopyAssetFolder(AssetManager, RootDir, RootDir, False);
    end;
  end;
end;

procedure CopyStartUpFiles;
begin
  CopyAssetsToFiles;
end;
{$ELSE !ANDROID}
procedure CopyStartUpFiles;
var
  Source, Destination : string;

  procedure DoCopyFiles(const Src:string; const Dst: string);
  var
    SearchRec : TSearchRec;
    Res : Integer;
  begin
    Res := FindFirst(src + '*', faAnyFile, SearchRec);
    while Res = 0 do
    begin
      if (SearchRec.Attr and faDirectory) = faDirectory then
      begin
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        begin
          if ForceDirectories(Dst+SearchRec.Name) then
            // Do the recurse thing...
            DoCopyFiles(Src+SearchRec.Name+PathDelim, Dst+SearchRec.Name+PathDelim);
        end;
      end
      else begin
        if not FileExists(Dst+SearchRec.Name) then
        begin
          TFile.Copy(Src+SearchRec.Name, Dst+SearchRec.Name, False); // copy without overwriting.
        end
      end;
      Res := FindNext(SearchRec);
    end;
  end;
begin
  Source := ExtractFilePath(ParamStr(0)) + 'StartUp' + PathDelim;
  if DirectoryExists(Source) then
  begin
    Destination := GetHomePath+PathDelim;
    DoCopyFiles(Source, Destination);
  end;
end;
{$ENDIF ANDROID}

initialization
begin
  CopyStartUpFiles;
end;

end.
