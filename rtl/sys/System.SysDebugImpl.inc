{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

//=============== SYSDEBUG ====================
const
  PF_INET       = 2; { IP protocol family.  }
  AF_INET       = PF_INET;
  SOCK_STREAM     = 1;
  IPPROTO_TCP = 6;

type
  TBytes = array of Byte;
  sa_family_t = Word;
  in_port_t = UInt16;
  in_addr_t = UInt32;
  socklen_t = UInt32;
//  size_t = UInt32;
//  ssize_t = LongInt ;

  in_addr = record
    s_addr: in_addr_t;
  end;

  sockaddr_in = record
    sin_family: sa_family_t;
    sin_port: in_port_t;
    sin_addr: in_addr;
    sin_zero: packed array [0..7] of Byte;
  end;

  sockaddr = record
      sa_family: sa_family_t;
      sa_data: packed array[0..13] of Byte;
  end;
  Psockaddr_in = ^sockaddr_in;


function socket(domain, atype, protocol: Integer): Integer; cdecl;
  external libc name _PU + 'socket';
function inet_addr(cp: MarshaledAString): in_addr_t; cdecl;
  external libc name _PU + 'inet_addr';
function connect(socket: Integer; const [Ref] address: sockaddr;
  address_len: socklen_t): Integer; cdecl;
  external libc name _PU + 'connect';
function send(socket: Integer; const message; length: size_t;
  flags: Integer): ssize_t; cdecl;
  external libc name _PU + 'send';


function swap16(x: UInt16): UInt16; inline;
begin
  Result := ((x and $ff) shl 8) or ((x and $ff00) shr 8);
end;

function htons(x: Integer): UInt16; inline;
begin
  Result := swap16(UInt16(x));
end;

var
  DebugLevel: integer;
  SocketHandle: Integer;
  Connected: Boolean;
  StartOfLine: Boolean;

procedure ConnectToClient(const AIP: string; Port : Integer);
var
  Address: sockaddr_in;
  ip: TBytes;

  function StringToCString(const str: string): TBytes;
  var
    I,J: Integer;
  begin
    J := 0;
    SetLength(Result, Length(str) + 1);
    for I:=Low(string) to High(str) do
    begin
      Result[J] := Byte(str[I]);
      Inc(J);
    end;
  end;

begin
  if Connected then Exit;

  SocketHandle := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if SocketHandle > 0 then
  begin
    Address.sin_family := AF_INET;
    Address.sin_port := htons(Port);
    ip :=StringToCString(AIP);
    Address.sin_addr.s_addr := inet_addr(@ip[0]);

    Connected := connect(SocketHandle, sockaddr(Address), SizeOf(Address)) = 0;
    if Connected then
    begin
      SetLength(SysBuffer, 8*1024);
      SysBuffered := False;
      SysBufferPos := 0;
    end;
  end;
  SysActive := Connected;
end;


procedure SysDebugPrintBuffer;
var
  Buffer: array[0..127] of Byte;
  str: string;
begin
  if SysBufferPos > 0 then
  begin
    FillChar(Buffer,Length(Buffer), 0);
    str := '{block}' + SysIntToStr(SysBufferPos) + #13#10;
    Move(str[Low(string)], Buffer, Length(str)*2);
    Send(SocketHandle, Buffer, Length(str)*2, 0);
    Send(SocketHandle, SysBuffer, SysBufferPos, 0);
  end;
end;

procedure PrintNoNewLine(const Msg: string);
var
  Buffer: array[0..16383] of Byte;
begin
  if (not Connected) or (not SysActive) then Exit;
  FillChar(Buffer,Length(Buffer), 0);
  Move(Msg[Low(string)], Buffer, Length(Msg)*2);
  Send(SocketHandle, Buffer, Length(Msg)*2, 0);
end;

procedure Print(const Msg: string);
begin
  PrintNoNewLine(Msg + #13#10);
end;

procedure SysDebugAddBuffer(const Msg :string);
var
  len: Integer;
  s: string;
begin
  s := Msg + #13#10;
  len := Length(s)*2;
  if (SysBufferPos + len) > (8*1024) then Exit;
  Move(s[Low(string)], SysBuffer[SysBufferPos], len);
  SysBufferPos := SysBufferPos + len;
end;

function SysCreateConnection(const AIP: string; Port : Integer):Boolean;
begin
  if not Connected then ConnectToClient(AIP, Port);
  Result := Connected;
  StartOfLine := true;
end;

function PrefixCad(level:integer):string;
var i:integer;
begin
  if StartOfLine then
    for i:=1 to level do
      result := result + ' ';
end;

procedure SysDebugEnter(const Msg :string);
begin
  StartOfLine := true;
  Print(PrefixCad(DebugLevel)+'Entering '+Msg);
  Inc(DebugLevel);
end;

procedure SysDebugLeave(const Msg :string);
begin
  StartOfLine := true;
  Dec(DebugLevel);
  Print(PrefixCad(DebugLevel)+'Leaving '+Msg);
end;

procedure SysDebugNNL(const Msg: string);
begin
  PrintNoNewLine(PrefixCad(DebugLevel)+Msg);
  StartOfLine := false;
end;

procedure SysDebug(const Msg :string);
begin
  Print(PrefixCad(DebugLevel)+Msg);
  StartOfLine := true;
end;

function SysIntToStr(val: Int64): string;
begin
  Result := string(_Str0Int64(val));
end;

procedure SysDebugOn;
begin
  SysActive := True;
end;

procedure SysDebugOff;
begin
  SysActive := False;
end;

procedure SysDebugBuffered(Activate: Boolean);
begin

end;


//============= END SYSDEBUG ======================
