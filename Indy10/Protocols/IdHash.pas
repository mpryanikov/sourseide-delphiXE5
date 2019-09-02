{
  $Project$
  $Workfile$
  $Revision$
  $DateUTC$
  $Id$

  This file is part of the Indy (Internet Direct) project, and is offered
  under the dual-licensing agreement described on the Indy website.
  (http://www.indyproject.org/)

  Copyright:
   (c) 1993-2005, Chad Z. Hower and the Indy Pit Crew. All rights reserved.
}
{
  $Log$
}
{
  Rev 1.10    7/24/04 12:54:32 PM  RLebeau
  Compiler fix for TIdHash128.HashValue()

  Rev 1.9    7/23/04 7:09:12 PM  RLebeau
  Added extra exception handling to various HashValue() methods

  Rev 1.8    2004.05.20 11:37:06 AM  czhower
  IdStreamVCL

  Rev 1.7    2004.03.03 11:54:30 AM  czhower
  IdStream change

  Rev 1.6    2004.02.03 5:44:48 PM  czhower
  Name changes

  Rev 1.5    1/27/2004 4:00:08 PM  SPerry
  StringStream ->IdStringStream

  Rev 1.4    11/10/2003 7:39:22 PM  BGooijen
  Did all todo's ( TStream to TIdStream mainly )

  Rev 1.3    2003.10.24 10:43:08 AM  czhower
  TIdSTream to dos

  Rev 1.2    10/18/2003 4:28:30 PM  BGooijen
  Removed the pchar for DotNet

  Rev 1.1    10/8/2003 10:15:10 PM  GGrieve
  replace TIdReadMemoryStream (might be fast, but not compatible with DotNet)

  Rev 1.0    11/13/2002 08:30:24 AM  JPMugaas
  Initial import from FTP VC.
}

unit IdHash;

interface

{$i IdCompilerDefines.inc}

uses
  Classes,
  IdFIPS,
  IdGlobal;

type
  TIdHash = class(TObject)
  protected
    function GetHashBytes(AStream: TStream; ASize: TIdStreamSize): TIdBytes; virtual; abstract;
    function HashToHex(const AHash: TIdBytes): String; virtual; abstract;
    function WordHashToHex(const AHash: TIdBytes; const ACount: Integer): String;
    function LongWordHashToHex(const AHash: TIdBytes; const ACount: Integer): String;
  public
    constructor Create; virtual;
    class function IsAvailable : Boolean; virtual;
    function HashString(const ASrc: string; ADestEncoding: IIdTextEncoding = nil{$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}): TIdBytes;
    function HashStringAsHex(const AStr: String; ADestEncoding: IIdTextEncoding = nil{$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}): String;
    function HashBytes(const ASrc: TIdBytes): TIdBytes;
    function HashBytesAsHex(const ASrc: TIdBytes): String;
    function HashStream(AStream: TStream): TIdBytes; overload;
    function HashStreamAsHex(AStream: TStream): String; overload;
    function HashStream(AStream: TStream; const AStartPos, ASize: TIdStreamSize): TIdBytes; overload;
    function HashStreamAsHex(AStream: TStream; const AStartPos, ASize: TIdStreamSize): String; overload;
  end;

  TIdHash16 = class(TIdHash)
  protected
    function GetHashBytes(AStream: TStream; ASize: TIdStreamSize): TIdBytes; override;
    function HashToHex(const AHash: TIdBytes): String; override;
  public
    function HashValue(const ASrc: string; ADestEncoding: IIdTextEncoding = nil{$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}): Word; overload;
    function HashValue(const ASrc: TIdBytes): Word; overload;
    function HashValue(AStream: TStream): Word; overload;
    function HashValue(AStream: TStream; const AStartPos, ASize: TIdStreamSize): Word; overload;
    procedure HashStart(var VRunningHash : Word); virtual; abstract;
    procedure HashEnd(var VRunningHash : Word); virtual;
    procedure HashByte(var VRunningHash : Word; const AByte : Byte); virtual; abstract;
  end;

  TIdHash32 = class(TIdHash)
  protected
    function GetHashBytes(AStream: TStream; ASize: TIdStreamSize): TIdBytes; override;
    function HashToHex(const AHash: TIdBytes): String; override;
  public
    function HashValue(const ASrc: string; ADestEncoding: IIdTextEncoding = nil{$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}): LongWord; overload;
    function HashValue(const ASrc: TIdBytes): LongWord; overload;
    function HashValue(AStream: TStream): LongWord; overload;
    function HashValue(AStream: TStream; const AStartPos, ASize: TIdStreamSize): LongWord; overload;
    procedure HashStart(var VRunningHash : LongWord); virtual; abstract;
    procedure HashEnd(var VRunningHash : LongWord); virtual;
    procedure HashByte(var VRunningHash : LongWord; const AByte : Byte); virtual; abstract;
  end;

  TIdHashClass = class of TIdHash;

  TIdHashIntF = class(TIdHash)
  protected
    function HashToHex(const AHash: TIdBytes): String; override;
    function InitHash : TIdHashIntCtx; virtual; abstract;
    procedure UpdateHash(ACtx : TIdHashIntCtx; const AIn : TIdBytes);
    function FinalHash(ACtx : TIdHashIntCtx) : TIdBytes;
    function GetHashBytes(AStream: TStream; ASize: TIdStreamSize): TIdBytes; override;
  public
    class function IsAvailable : Boolean; override;
    class function IsIntfAvailable : Boolean; virtual;
  end;
  TIdHashNativeAndIntF = class(TIdHashIntF)
  protected
    function NativeGetHashBytes(AStream: TStream; ASize: TIdStreamSize): TIdBytes; virtual;
    function GetHashBytes(AStream: TStream; ASize: TIdStreamSize): TIdBytes; override;

  end;

  {$IFDEF DOTNET}
  EIdSecurityAPIException = class(EIdException);
  EIdSHA224NotSupported = class(EIdSecurityAPIException);
  {$ENDIF}

implementation

uses
  {$IFDEF DOTNET}
  IdStreamNET,
  {$ELSE}
  IdStreamVCL,
  {$ENDIF}
  IdGlobalProtocols, SysUtils;

{ TIdHash }

constructor TIdHash.Create;
begin
  inherited Create;
end;

function TIdHash.HashString(const ASrc: string; ADestEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
  ): TIdBytes;
var
  LStream: TStream;  // not TIdStringStream -  Unicode on DotNet!
begin
  LStream := TMemoryStream.Create; try
    WriteStringToStream(LStream, ASrc, ADestEncoding{$IFDEF STRING_IS_ANSI}, ASrcEncoding{$ENDIF});
    LStream.Position := 0;
    Result := HashStream(LStream);
  finally FreeAndNil(LStream); end;
end;

function TIdHash.HashStringAsHex(const AStr: String; ADestEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
  ): String;
begin
  Result := HashToHex(HashString(AStr, ADestEncoding{$IFDEF STRING_IS_ANSI}, ASrcEncoding{$ENDIF}));
end;

function TIdHash.HashBytes(const ASrc: TIdBytes): TIdBytes;
var
  LStream: TStream;
begin
  LStream := TMemoryStream.Create; try
    WriteTIdBytesToStream(LStream, ASrc);
    LStream.Position := 0;
    Result := HashStream(LStream);
  finally FreeAndNil(LStream); end;
end;

function TIdHash.HashBytesAsHex(const ASrc: TIdBytes): String;
begin
  Result := HashToHex(HashBytes(ASrc));
end;

function TIdHash.HashStream(AStream: TStream): TIdBytes;
begin
  Result := HashStream(AStream, -1, -1);
end;

function TIdHash.HashStreamAsHex(AStream: TStream): String;
begin
  Result := HashToHex(HashStream(AStream));
end;

function TIdHash.HashStream(AStream: TStream; const AStartPos, ASize: TIdStreamSize): TIdBytes;
var
  LSize, LAvailable: TIdStreamSize;
begin
  if AStartPos >= 0 then begin
    AStream.Position := AStartPos;
  end;
  LAvailable := AStream.Size - AStream.Position;
  if ASize < 0 then begin
    LSize := LAvailable;
  end else begin
    LSize := IndyMin(LAvailable, ASize);
  end;
  Result := GetHashBytes(AStream, LSize);
end;

function TIdHash.HashStreamAsHex(AStream: TStream; const AStartPos, ASize: TIdStreamSize): String;
begin
  Result := HashToHex(HashStream(AStream, AStartPos, ASize));
end;

function TIdHash.WordHashToHex(const AHash: TIdBytes; const ACount: Integer): String;
var
  LValue: Word;
  I: Integer;
begin
  Result := '';
  for I := 0 to ACount-1 do begin
    LValue := BytesToWord(AHash, SizeOf(Word)*I);
    Result := Result + IntToHex(LValue, 4);
  end;
end;

function TIdHash.LongWordHashToHex(const AHash: TIdBytes; const ACount: Integer): String;
begin
  Result := ToHex(AHash, ACount*SizeOf(LongWord));
end;

class function TIdHash.IsAvailable : Boolean;
begin
  Result := True;
end;

{ TIdHash16 }

function TIdHash16.GetHashBytes(AStream: TStream; ASize: TIdStreamSize): TIdBytes;
const
  cBufSize = 1024; // Keep it small for dotNet
var
  I: Integer;
  LBuffer: TIdBytes;
  LSize: Integer;
  LHash: Word;
begin
  Result := nil;
  HashStart(LHash);

  SetLength(LBuffer, cBufSize);

  while ASize > 0 do
  begin
    LSize := ReadTIdBytesFromStream(AStream, LBuffer, IndyMin(cBufSize, ASize));
    if LSize < 1 then begin
      Break;                                                
    end;
    for i := 0 to LSize - 1 do begin
      HashByte(LHash, LBuffer[i]);
    end;
    Dec(ASize, LSize);
  end;

  HashEnd(LHash);

  SetLength(Result, SizeOf(Word));
  CopyTIdWord(LHash, Result, 0);
end;

function TIdHash16.HashToHex(const AHash: TIdBytes): String;
begin
  Result := IntToHex(BytesToWord(AHash), 4);
end;

procedure TIdHash16.HashEnd(var VRunningHash : Word);
begin
end;

function TIdHash16.HashValue(const ASrc: string; ADestEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
  ): Word;
begin
  Result := BytesToWord(HashString(ASrc, ADestEncoding{$IFDEF STRING_IS_ANSI}, ASrcEncoding{$ENDIF}));
end;

function TIdHash16.HashValue(const ASrc: TIdBytes): Word;
begin
  Result := BytesToWord(HashBytes(ASrc));
end;

function TIdHash16.HashValue(AStream: TStream): Word;
begin
  Result := BytesToWord(HashStream(AStream));
end;

function TIdHash16.HashValue(AStream: TStream; const AStartPos, ASize: TIdStreamSize): Word;
begin
  Result := BytesToWord(HashStream(AStream, AStartPos, ASize));
end;

{ TIdHash32 }

function TIdHash32.GetHashBytes(AStream: TStream; ASize: TIdStreamSize): TIdBytes;
const
  cBufSize = 1024; // Keep it small for dotNet
var
  I: Integer;
  LBuffer: TIdBytes;
  LSize: Integer;
  LHash: LongWord;
begin
  Result := nil;
  HashStart(LHash);

  SetLength(LBuffer, cBufSize);

  while ASize > 0 do
  begin
    LSize := ReadTIdBytesFromStream(AStream, LBuffer, IndyMin(cBufSize, ASize));
    if LSize < 1 then begin
      Break;                                                 
    end;
    for i := 0 to LSize - 1 do begin
      HashByte(LHash, LBuffer[i]);
    end;
    Dec(ASize, LSize);
  end;

  HashEnd(LHash); // RLebeau: TIdHashCRC32 uses this to XOR the hash with $FFFFFFFF

  SetLength(Result, SizeOf(LongWord));
  CopyTIdLongWord(LHash, Result, 0);
end;

function TIdHash32.HashToHex(const AHash: TIdBytes): String;
begin
  Result := LongWordToHex(BytesToLongWord(AHash)); //IntToHex(BytesToLongWord(AHash), 8);
end;

procedure TIdHash32.HashEnd(var VRunningHash : LongWord);
begin
end;

function TIdHash32.HashValue(const ASrc: string; ADestEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
  ): LongWord;
begin
  Result := BytesToLongWord(HashString(ASrc, ADestEncoding{$IFDEF STRING_IS_ANSI}, ASrcEncoding{$ENDIF}));
end;

function TIdHash32.HashValue(const ASrc: TIdBytes): LongWord;
begin
  Result := BytesToLongWord(HashBytes(ASrc));
end;

function TIdHash32.HashValue(AStream: TStream) : LongWord;
begin
  Result := BytesToLongWord(HashStream(AStream));
end;

function TIdHash32.HashValue(AStream: TStream; const AStartPos, ASize: TIdStreamSize) : LongWord;
begin
  Result := BytesToLongWord(HashStream(AStream, AStartPos, ASize));
end;


{ TIdHashIntf }

function TIdHashIntf.FinalHash(ACtx: TIdHashIntCtx): TIdBytes;
{$IFDEF DOTNET}
var
  LDummy : TIdBytes;
{$ENDIF}
begin
  {$IFDEF DOTNET}
  //This is a funny way of coding.  I have to pass a dummy value to
  //TransformFinalBlock so that things can work similarly to the OpenSSL
  //Crypto API.  You can't pass nul to TransformFinalBlock without an exception.
  SetLength(LDummy,0);
  ACtx.TransformFinalBlock(LDummy,0,0);
  Result := ACtx.Hash;
  {$ELSE}
  Result := IdFIPS.FinalHashInst(ACtx);
  {$ENDIF}
end;

function TIdHashIntf.GetHashBytes(AStream: TStream; ASize: TIdStreamSize): TIdBytes;
var
  LBuf : TIdBytes;
  LSize : Int64;
  LCtx : TIdHashIntCtx;
begin
  LCtx := InitHash;
  try
    SetLength(LBuf,2048);
    repeat
      LSize := ReadTIdBytesFromStream(AStream,LBuf,2048);
      if LSize = 0 then begin
        break;
      end;
      if LSize < 2048 then begin
        SetLength(LBuf,LSize);
        UpdateHash(LCtx,LBuf);
        break;
      end else begin
        UpdateHash(LCtx,LBuf);
      end;
    until False;
  finally
    Result := FinalHash(LCtx);
  end;
end;

function TIdHashIntf.HashToHex(const AHash: TIdBytes): String;
begin
  Result := ToHex(AHash);
end;

{$IFDEF DOTNET}
class function TIdHashIntf.IsAvailable: Boolean;
begin
  Result := True;
end;

class function TIdHashIntF.IsIntfAvailable: Boolean;
begin
   Result := False;
end;
{$ELSE}
                                                                  
//implementation.


class function TIdHashIntf.IsAvailable: Boolean;
begin
  Result := IsIntfAvailable;
end;

class function TIdHashIntF.IsIntfAvailable: Boolean;
begin
   Result := IsHashingIntfAvail;
end;
{$ENDIF}

procedure TIdHashIntf.UpdateHash(ACtx: TIdHashIntCtx; const AIn: TIdBytes);
begin
  UpdateHashInst(ACtx,AIn);
  {$IFDEF DOTNET}
  ACtx.TransformBlock(AIn,0,Length(AIn),AIn,0);
  {$ELSE}
  {$ENDIF}
end;

{ TIdHashNativeAndIntF }

function TIdHashNativeAndIntF.GetHashBytes(AStream: TStream;
  ASize: TIdStreamSize): TIdBytes;
begin
  if IsIntfAvailable then begin
    Result := inherited GetHashBytes(AStream, ASize);
  end else begin
    Result := NativeGetHashBytes(AStream, ASize);
  end;
end;

function TIdHashNativeAndIntF.NativeGetHashBytes(AStream: TStream;
  ASize: TIdStreamSize): TIdBytes;
begin

end;

end.
