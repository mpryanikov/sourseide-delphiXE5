{*******************************************************}
{                                                       }
{               Delphi FireDAC Framework                }
{        FireDAC static linking helper routines         }
{                                                       }
{ Copyright(c) 2004-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}
{$I FireDAC.inc}

unit FireDAC.Stan.CRTL;

interface

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows, Winapi.Winsock,
{$ENDIF}
  System.SysUtils, System.Classes,
  FireDAC.Stan.Intf, FireDAC.Stan.Util;

type
  size_t = NativeUInt;
  qsort_compare_func = function(P1, P2: Pointer): Integer; cdecl;
{$IFDEF MSWINDOWS}
  time_t = Integer;
  Ptime_t = ^time_t;
  _time64_t = Int64;
  P_time64_t = ^_time64_t;
  TTm = packed record
    tm_sec: Integer;            { Seconds.      [0-60] (1 leap second) }
    tm_min: Integer;            { Minutes.      [0-59]  }
    tm_hour: Integer;           { Hours.        [0-23]  }
    tm_mday: Integer;           { Day.          [1-31]  }
    tm_mon: Integer;            { Month.        [0-11]  }
    tm_year: Integer;           { Year          - 1900. }
    tm_wday: Integer;           { Day of week.  [0-6]   }
    tm_yday: Integer;           { Days in year. [0-365] }
    tm_isdst: Integer;          { DST.          [-1/0/1]}
  end;
  PTm = ^TTm;
{$ENDIF}

{$IFDEF CPU386}
function _ftol: Int64; cdecl;
function _ftoul: Int64; cdecl;
{$ENDIF}
function malloc(size: size_t): Pointer; cdecl;
procedure free(P: Pointer); cdecl;
function realloc(P: Pointer; Size: size_t): Pointer; cdecl;
function memset(P: Pointer; B: Integer; count: size_t): pointer; cdecl;
procedure memmove(dest, source: pointer; count: size_t); cdecl;
procedure memcpy(dest, source: Pointer; count: size_t); cdecl;
function memcmp(p1, p2: Pointer; Size: size_t): integer; cdecl;
{$IFDEF CPU386}
procedure _lldiv;
procedure _lludiv;
procedure _llmod;
procedure _llmul;
procedure _llumod;
procedure _llshl;
procedure _llshr;
procedure _llushr;
{$ENDIF}
procedure qsort(baseP: PByte; NElem, Width: size_t; comparF: qsort_compare_func); cdecl;
{$IFDEF MSWINDOWS}
function localtime(t: Ptime_t): pointer; cdecl;
function _localtime64_s(tm: PTm; time: P_time64_t): Integer; cdecl;
{$ENDIF}
function strncmp(p1, p2: PFDAnsiString; Size: size_t): integer; cdecl;
function strcmp(p1, p2: PFDAnsiString): integer; cdecl;
function wcscpy(dest, src: PWideChar): PWideChar; cdecl;

var
  __turbofloat: Word;
{$IFDEF MSWINDOWS}
  _fltused: Integer = 0;
  winSysInfo: SYSTEM_INFO;
{$ENDIF}

implementation

{$IFDEF POSIX}
uses
  Posix.Stdlib;
{$ENDIF}

{$HINTS OFF}

{-------------------------------------------------------------------------------}
{ Static linking                                                                }
{-------------------------------------------------------------------------------}
{$IFDEF CPU386}
function _ftol: Int64; cdecl;
asm
  jmp System.@Trunc  // FST(0) -> EDX:EAX, as expected by BCC32 compiler
end;

{-------------------------------------------------------------------------------}
function _ftoul: Int64; cdecl;
asm
  jmp System.@Trunc  // FST(0) -> EDX:EAX, as expected by BCC32 compiler
end;
{$ENDIF}

{-------------------------------------------------------------------------------}
function malloc(size: size_t): Pointer; cdecl;
begin
  GetMem(Result, size);
end;

{-------------------------------------------------------------------------------}
procedure free(P: Pointer); cdecl;
begin
  FreeMem(P);
end;

{-------------------------------------------------------------------------------}
function realloc(P: Pointer; Size: size_t): Pointer; cdecl;
begin
  Result := P;
  ReallocMem(Result,Size);
end;

{-------------------------------------------------------------------------------}
function memset(P: Pointer; B: Integer; count: size_t): pointer; cdecl;
begin
  Result := P;
  FillChar(P^, count, B);
end;

{-------------------------------------------------------------------------------}
procedure memmove(dest, source: pointer; count: size_t); cdecl;
begin
  Move(source^, dest^, count);
end;

{-------------------------------------------------------------------------------}
procedure memcpy(dest, source: Pointer; count: size_t); cdecl;
begin
  Move(source^, dest^, count);
end;

{-------------------------------------------------------------------------------}
function memcmp(p1, p2: Pointer; Size: size_t): integer; cdecl;
begin
  if (p1 <> p2) and (Size <> 0) then
    if p1 <> nil then
      if p2 <> nil then begin
        repeat
          if PByte(p1)^ <> PByte(p2)^ then begin
            Result := PByte(p1)^ - PByte(p2)^;
            Exit;
          end;
          Dec(Size);
          Inc(PByte(p1));
          Inc(PByte(p2));
        until Size = 0;
        Result := 0;
      end
      else
        Result := 1
    else
      Result := -1
  else
    Result := 0;
end;

{-------------------------------------------------------------------------------}
{$IFDEF CPU386}
procedure _lldiv;
asm
  jmp System.@_lldiv
end;

{-------------------------------------------------------------------------------}
procedure _lludiv;
asm
  jmp System.@_lludiv
end;

{-------------------------------------------------------------------------------}
procedure _llmod;
asm
  jmp System.@_llmod
end;

{-------------------------------------------------------------------------------}
procedure _llmul;
asm
  jmp System.@_llmul
end;

{-------------------------------------------------------------------------------}
procedure _llumod;
asm
  jmp System.@_llumod
end;

{-------------------------------------------------------------------------------}
procedure _llshl;
asm
  jmp System.@_llshl
end;

{-------------------------------------------------------------------------------}
procedure _llshr;
asm
{$IFNDEF FireDAC_D2005}
  shrd    eax, edx, cl
  sar     edx, cl
  cmp     cl, 32
  jl      @@Done
  cmp     cl, 64
  jge     @@RetSign
  mov     eax, edx
  sar     edx, 31
  ret
@@RetSign:
  sar     edx, 31
  mov     eax, edx
@@Done:
{$ELSE}
  jmp System.@_llshr
{$ENDIF}
end;

{-------------------------------------------------------------------------------}
procedure _llushr;
asm
  jmp System.@_llushr
end;
{$ENDIF}

{-------------------------------------------------------------------------------}
procedure QuickSort(baseP: PByte; Width: size_t; L, R: size_t; comparF: qsort_compare_func);

  procedure Exchg(P1, P2: PByte; Size: size_t);
  var
    B: Byte;
    i: size_t;
  begin
    for i := 1 to Size do begin
      B := P1^;
      P1^ := P2^;
      P2^ := B;
      Inc(P1);
      Inc(P2);
    end;
  end;

var
  I, J, P: size_t;
  PP, C: PByte;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      PP := baseP + P * Width;
      C := baseP + I * Width;
      while comparF(C, PP) < 0 do begin
        Inc(I);
        Inc(C, width);
      end;
      C := baseP + J * Width;
      while comparF(C, PP) > 0 do begin
        Dec(J);
        Dec(C, width);
      end;
      if I <= J then begin
        Exchg(baseP + I * Width, baseP + J * Width, Width);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(baseP, Width, L, J, comparF);
    L := I;
  until I >= R;
end;

procedure qsort(baseP: PByte; NElem, Width: size_t; comparF: qsort_compare_func); cdecl;
begin
  if (NElem > 1) and (Width > 0) then
    QuickSort(baseP, Width, 0, NElem - 1, comparF);
end;

{-------------------------------------------------------------------------------}
{$IFDEF MSWINDOWS}
threadvar
  rTm: TTm;

function localtime(t: Ptime_t): pointer; cdecl;
var
  uTm: TFileTime;
  lTm: TFileTime;
  sTm: TSystemTime;
  p: PTm;
begin
  Int64(uTm) := (Int64(t^) + 11644473600) * 10000000;
  FileTimeToLocalFileTime(uTM, lTM);
  FileTimeToSystemTime(lTM, sTm);
  p := @rTm;
  p^.tm_sec := sTm.wSecond;
  p^.tm_min := sTm.wMinute;
  p^.tm_hour := sTm.wHour;
  p^.tm_mday := sTm.wDay;
  p^.tm_mon := sTm.wMonth - 1;
  p^.tm_year := sTm.wYear - 1900;
  p^.tm_wday := sTm.wDayOfWeek;
  Result := p;
end;

{-------------------------------------------------------------------------------}
function _localtime64_s(tm: PTm; time: P_time64_t): Integer; cdecl;
begin
  tm^ := PTm(localtime(Ptime_t(time)))^;
  Result := 0;
end;
{$ENDIF}

{-------------------------------------------------------------------------------}
function strncmp(p1, p2: PFDAnsiString; Size: size_t): integer; cdecl;
var
  I: size_t;
begin
  I := 0;
  while I < Size do begin
    if (P1^ <> P2^) or (TFDAnsiChar(P2^) = TFDAnsiChar(#0)) then begin
      Result := Integer(P1^) - Integer(P2^);
      Exit;
    end;
    Inc(P1);
    Inc(P2);
    Inc(I);
  end;
  Result := 0;
end;

{-------------------------------------------------------------------------------}
function strcmp(p1, p2: PFDAnsiString): integer; cdecl;
begin
  while True do begin
    if (P1^ <> P2^) or (TFDAnsiChar(P2^) = TFDAnsiChar(#0)) then begin
      Result := Integer(P1^) - Integer(P2^);
      Exit;
    end;
    Inc(P1);
    Inc(P2);
  end;
  Result := 0;
end;

{-------------------------------------------------------------------------------}
function wcscpy(dest, src: PWideChar): PWideChar; cdecl;
begin
  Move(src^, dest^, (FDWideStrLen(src) + 1) * SizeOf(WideChar));
  Result := dest;
end;

{-------------------------------------------------------------------------------}
{$IFDEF MSWINDOWS}
initialization
  GetSystemInfo(winSysInfo);
{$ENDIF}

end.
