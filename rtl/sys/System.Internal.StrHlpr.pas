{*****************************************************************}
{                                                                 }
{           CodeGear Delphi Runtime Library                       }
{                                                                 }
{      Copyright(c) 1995-2013 Embarcadero Technologies, Inc.      }
{                                                                 }
{*****************************************************************}

{*****************************************************************}
{  Helpers for C++ AnsiString/WideString/UnicodeString binding.   }
{*****************************************************************}

unit System.Internal.StrHlpr;

{$WEAKPACKAGEUNIT}

{$WARN IMPLICIT_STRING_CAST OFF}
{$WARN IMPLICIT_STRING_CAST_LOSS OFF}

(*$HPPEMIT '#define NO_USING_NAMESPACE_SYSTEM_INTERNAL_STRHLPR'*)
(*$HPPEMIT '#define NO_USING_NAMESPACE_SYSTEM_INTERNAL'*)

interface

type
{$IFNDEF NEXTGEN}
  _RawByteString = System.RawByteString;
  _PWideChar = System.PWideChar;
  _PAnsiChar = System.PAnsiChar;
{$ELSE}
  _RawByteString = System.TPtrWrapper;
  _PWideChar = ^System.TPtrWrapper;
  _PAnsiChar = ^System.TPtrWrapper;
{$ENDIF}  

function AnsiCat(var x, y: _RawByteString): _RawByteString;
function AnsiCopy(var src: _RawByteString; index, count: Integer): _RawByteString;
function AnsiPos(var src, sub: _RawByteString): Integer;
procedure AnsiAppend(var dst: _RawByteString; var src: _RawByteString);
procedure AnsiInsert(var dst: _RawByteString; var src: _RawByteString; index: Integer);
function AnsiEqual(var x, y: _RawByteString): Boolean;
function AnsiGreater(var x, y: _RawByteString): Boolean;
function AnsiLess(var x, y: _RawByteString): Boolean;
procedure AnsiDelete(var dst: _RawByteString; index, count: Integer);
procedure AnsiSetLength(var dst: _RawByteString; len: Integer);
procedure AnsiFree(var s: _RawByteString);
procedure AnsiAssign(var dst: _RawByteString; var src: _RawByteString);

{$IFNDEF NEXTGEN}
procedure WideAppend(var dst: WideString; var src: WideString);
function WideCat(var x, y: WideString): WideString;
function WideCopy(var src: WideString; index, count: Integer): WideString;
function WideEqual(var x, y: WideString): Boolean;
function WideGreater(var x, y: WideString): Boolean;
function WideLength(var src: WideString): Integer;
function WideLess(var x, y: WideString): Boolean;
function WidePos(var src, sub: WideString): Integer;
procedure WideFromAnsi(var dst: WideString; var src: RawByteString);
procedure WideFromUnicode(var dst: WideString; var src: UnicodeString);
procedure WideInsert(var dst: WideString; var src: WideString; index: Integer);

procedure WideFromPChar(var dst: WideString; src: PAnsiChar; IsUTF8: Boolean = False);
procedure WideDelete(var dst: WideString; index, count: Integer);
procedure WideSetLength(var dst: WideString; len: Integer);
procedure WideFree(var s: WideString);

{ WideAssign is called from the C++ WideString's assignment constructor, so it
  must use pass-by-reference to avoid infinite recursion.  This will
  require const_cast<>'s in the C++ call:( }
procedure WideAssign(var dst: WideString; var src: WideString);
procedure WideFromUTF8(var dst: WideString; const src: PAnsiChar);
procedure UnicodeFromWide(var dst: UnicodeString; var src: WideString);
{$ENDIF} // !NEXTGEN

procedure UnicodeFromAnsi(var dst: UnicodeString; var src: _RawByteString);
procedure UnicodeFromPChar(var dst: UnicodeString; src: _PAnsiChar; IsUTF8: Boolean = False);
procedure UnicodeFromUTF8(var dst: UnicodeString; const src: _PAnsiChar);


function UnicodeCat(var x, y: UnicodeString): UnicodeString;
function UnicodeCopy(var src: UnicodeString; index, count: Integer): UnicodeString;
function UnicodePos(var src, sub: UnicodeString): Integer;
procedure UnicodeAppend(var dst: UnicodeString; var src: UnicodeString);
procedure UnicodeDelete(var dst: UnicodeString; index, count: Integer);
procedure UnicodeInsert(var dst: UnicodeString; var src: UnicodeString; index: Integer);
function UnicodeEqual(var x, y: UnicodeString): Boolean;
function UnicodeGreater(var x, y: UnicodeString): Boolean;
function UnicodeLess(var x, y: UnicodeString): Boolean;

procedure UnicodeFromPWideChar(var dst: UnicodeString; src: _PWideChar);
procedure UnicodeSetLength(var dst: UnicodeString; len: Integer);
procedure UnicodeFree(var s: UnicodeString);
procedure UnicodeAssign(var dst: UnicodeString; var src: UnicodeString);

implementation

const
  CP_ACP  = 0;
  CP_UTF8 = 65001;

{ AnsiString }
procedure AnsiAppend(var dst: _RawByteString; var src: _RawByteString);
begin
{$IFNDEF NEXTGEN}
  dst := dst + src;
{$ELSE}
{$ENDIF}  
end;

function AnsiCat(var x, y: _RawByteString): _RawByteString;
begin
{$IFNDEF NEXTGEN}
  Result := x + y;
{$ELSE}
{$ENDIF}  
end;

procedure AnsiDelete(var dst: _RawByteString; index, count: Integer);
begin
{$IFNDEF NEXTGEN}
  Delete(dst, index, count);
{$ELSE}
{$ENDIF}  
end;

procedure AnsiSetLength(var dst: _RawByteString; len: Integer);
begin
{$IFNDEF NEXTGEN}
  SetLength(dst, len);
{$ELSE}
{$ENDIF}  
end;

function AnsiPos(var src, sub: _RawByteString): Integer;
begin
{$IFNDEF NEXTGEN}
  Result := Pos(sub, src);
{$ELSE}
  // FIXME
  Result := -1;
{$ENDIF}  
end;

function AnsiCopy(var src: _RawByteString; index, count: Integer): _RawByteString;
begin
{$IFNDEF NEXTGEN}
  Result := Copy(src, index, count);
{$ELSE}
  // FIXME
  Result := TPtrWrapper.Create(nil);
{$ENDIF}  
end;

procedure AnsiInsert(var dst: _RawByteString; var src: _RawByteString; index: Integer);
begin
{$IFNDEF NEXTGEN}
  Insert(src, dst, index);
{$ELSE}
{$ENDIF}  
end;

function AnsiEqual(var x, y: _RawByteString): Boolean;
begin
{$IFNDEF NEXTGEN}
  Result := x = y;
{$ELSE}
  // FIXME
  Result := False;
{$ENDIF}  
end;

function AnsiGreater(var x, y: _RawByteString): Boolean;
begin
{$IFNDEF NEXTGEN}
  Result := x > y;
{$ELSE}
  // FIXME
  Result := False;
{$ENDIF}  
end;

function AnsiLess(var x, y: _RawByteString): Boolean;
begin
{$IFNDEF NEXTGEN}
  Result := x < y;
{$ELSE}
  // FIXME
  Result := False;
{$ENDIF}  
end;

procedure AnsiAssign(var dst: _RawByteString; var src: _RawByteString);
begin
{$IFNDEF NEXTGEN}
  dst := src;
{$ELSE}
{$ENDIF}  
end;

procedure AnsiFree(var s: _RawByteString);
begin
{$IFNDEF NEXTGEN}
  s := '';
{$ELSE}
{$ENDIF}  
end;

{$IFNDEF NEXTGEN}
{ WideString }
procedure WideAssign(var dst: WideString; var src: WideString);
begin
  dst := src;
end;

procedure WideFree(var s: WideString);
begin
  s := '';
end;

procedure WideFromAnsi(var dst: WideString; var src: RawByteString);
begin
  dst := src;
end;

procedure WideFromUnicode(var dst: WideString; var src: UnicodeString);
begin
  dst := src;
end;

procedure WideFromPChar(var dst: WideString; src: PAnsiChar; IsUTF8: Boolean);
begin
  if not isUTF8 then
    dst := src
  else
    dst := UTF8String(src);
end;

function WideEqual(var x, y: WideString): Boolean;
begin
  Result := x = y;
end;

function WideLess(var x, y: WideString): Boolean;
begin
  Result := x < y;
end;

function WideGreater(var x, y: WideString): Boolean;
begin
  Result := x > y;
end;

function WideCat(var x, y: WideString): WideString;
begin
  Result := x + y;
end;

function WideLength(var src: WideString): Integer;
begin
  Result := Length(src);
end;

function WidePos(var src, sub: WideString): Integer;
begin
  Result := Pos(sub, src);
end;

procedure WideSetLength(var dst: WideString; len: Integer);
begin
  SetLength(dst, len);
end;

procedure WideDelete(var dst: WideString; index, count: Integer);
begin
  Delete(dst, index, count);
end;

procedure WideInsert(var dst: WideString; var src: WideString; index: Integer);
begin
  Insert(src, dst, index);
end;

function WideCopy(var src: WideString; index, count: Integer): WideString;
begin
  Result := Copy(src, index, count);
end;

procedure WideAppend(var dst: WideString; var src: WideString);
begin
  dst := dst + src;
end;

procedure WideFromUTF8(var dst: WideString; const src: PAnsiChar);
begin
  dst := UTF8String(src);
end;

procedure UnicodeFromWide(var dst: UnicodeString; var src: WideString);
begin
  dst := src;
end;
{$ENDIF}  // !NEXTGEN


{ UnicodeString }
procedure UnicodeFromPChar(var dst: UnicodeString; src: _PAnsiChar; IsUTF8: Boolean);
begin
{$IFNDEF NEXTGEN}
  if not isUTF8 then
    dst := src
  else
    dst := UTF8String(src);
{$ELSE}
  if not isUTF8 then
    dst := TMarshal.ReadStringAsAnsi(src^)
  else
    dst := TMarshal.ReadStringAsAnsi(CP_UTF8, src^);
{$ENDIF}  
end;

procedure UnicodeFromUTF8(var dst: UnicodeString; const src: _PAnsiChar);
begin
{$IFNDEF NEXTGEN}
  dst := UTF8ToUnicodeString(src);
{$ELSE}
  dst := TMarshal.ReadStringAsAnsi(65001, src^);
{$ENDIF}  
end;

procedure UnicodeFromAnsi(var dst: UnicodeString; var src: _RawByteString);
begin
{$IFNDEF NEXTGEN}
  dst := src;
{$ELSE}
{$ENDIF}  
end;

procedure UnicodeFromPWideChar(var dst: UnicodeString; src: _PWideChar);
begin
{$IFNDEF NEXTGEN}
  dst := src;
{$ELSE}
{$ENDIF}  
end;

procedure UnicodeAppend(var dst: UnicodeString; var src: UnicodeString);
begin
  dst := dst + src;
end;

function UnicodeCat(var x, y: UnicodeString): UnicodeString;
begin
  Result := x + y;
end;

procedure UnicodeDelete(var dst: UnicodeString; index, count: Integer);
begin
  Delete(dst, index, count);
end;

procedure UnicodeSetLength(var dst: UnicodeString; len: Integer);
begin
  SetLength(dst, len);
end;

function UnicodePos(var src, sub: UnicodeString): Integer;
begin
  Result := Pos(sub, src);
end;

function UnicodeCopy(var src: UnicodeString; index, count: Integer): UnicodeString;
begin
  Result := Copy(src, index, count);
end;

procedure UnicodeInsert(var dst: UnicodeString; var src: UnicodeString; index: Integer);
begin
  Insert(src, dst, index);
end;

procedure UnicodeAssign(var dst: UnicodeString; var src: UnicodeString);
begin
  dst := src;
end;

procedure UnicodeFree(var s: UnicodeString);
begin
  s := '';
end;

function UnicodeEqual(var x, y: UnicodeString): Boolean;
begin
  Result := x = y;
end;

function UnicodeLess(var x, y: UnicodeString): Boolean;
begin
  Result := x < y;
end;

function UnicodeGreater(var x, y: UnicodeString): Boolean;
begin
  Result := x > y;
end;

end.
