{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

type
  UBool = ShortInt;
  UChar = Char;
  PUChar = PChar;
  UErrorCode = Int32;
  PUErrorCode = ^UErrorCode;
  TByteArray = array of Byte;
  TStarterArray = array [0..255] of UBool;
  PStarterArray = ^TStarterArray;
  UConverter = record end;
  PUConverter = ^UConverter;

  PUCollator = ^UCollator;
  UCollator = record end;
  UCollationResult = (UCOL_LESS = -1, UCOL_EQUAL = 0, UCOL_GREATER = 1);

var
{ ICU ustring.h File Reference }
{ http://www.icu-project.org/apiref/icu4c/ustring_8h.html }
  // Convert a UTF-16 string to UTF-8.
  u_strToUTF8: function(dest: MarshaledAString; destCapacity: Int32; var DestLength: Int32;
      const src: PUChar; srcLength: Int32; var ErrorCode: UErrorCode): MarshaledAString; cdecl;

  //Convert a UTF-8 string to UTF-16.
  u_strFromUTF8: function(dest: PUChar; destCapacity: Int32; var DestLength: Int32;
      const src: MarshaledAString; srcLength: Int32; var ErrorCode: UErrorCode): PUChar; cdecl;

  // Compare two strings case-insensitively using full case folding.
  u_strcasecmp: function(const s1:PUChar; const s2:PUChar; options: Int32): Int32; cdecl;


{ ICU ucnv.h File Reference }
{ http://www.icu-project.org/apiref/icu4c/ucnv_8h.html }
  //Creates a UConverter object with the name of a coded character set specified as a C string.
  ucnv_open: function(const converterName: MarshaledAString; var err: UErrorCode): PUConverter; cdecl;

  //Resets the state of a converter to the default state.
  ucnv_reset: procedure(converter: PUConverter); cdecl;

  //Deletes the unicode converter and releases resources associated with just this instance.
  ucnv_close: procedure(converter: PUConverter); cdecl;

 	//Convert the Unicode string into a codepage string using an existing UConverter.
  ucnv_fromUChars: function(cnv: PUConverter; dest: MarshaledAString; destCapacity: Int32;
                      const src: PUChar; srcLength: Int32; var ErrorCode: UErrorCode): Int32; cdecl;

  //Convert the codepage string into a Unicode string using an existing UConverter.
	ucnv_toUChars: function(cnv: PUConverter; dest: PUChar; destCapacity: Int32;
                      const src: MarshaledAString ; srcLength: Int32; var ErrorCode: UErrorCode): Int32; cdecl;

  // 	Sets the substitution chars when converting from unicode to a codepage.
  ucnv_setSubstChars: procedure(converter: PUConverter; subChars: MarshaledAString; len: word; var err: UErrorCode); cdecl;

  ucnv_getStarters: procedure(converter: PUConverter; starters :TStarterArray; var err: UErrorCode); cdecl;

  { ICU ucol.h File Reference}
  { http://www.icu-project.org/apiref/icu4c/ucol_8h.html }

  // Open a UCollator for comparing strings.
  ucol_open: function(const loc: MarshaledAString; var status: UErrorCode): PUCollator; cdecl;

 	// Close a UCollator.
  ucol_close: procedure(coll: PUCollator); cdecl;

  // 	Compare two strings in UTF-8. ICU 5.0
  ucol_strcollUTF8: function(const coll: PUCollator; const source: PUChar; sourceLength: Int32;
      const target: PUChar; targetLength: Int32; var status: UErrorCode): UCollationResult; cdecl;


function InitICU: Boolean;
var
  CStr: MarshaledAString;

  //We haven't Marshall yet
  function CStrToStr(const Str: MarshaledAString; len: Byte; var Dest: string): string;  // Only ASCII chars
    var
    I:Byte;
  begin
    SetLength(Dest, len);
    for I := 0 to len - 1 do
    begin
      Dest[I] := Char(Str[I]);
    end;
    Result := Dest;
  end;

  function StrToCStr(const Str: string; Dest: MarshaledAString): MarshaledAString;  // Only ASCII chars
  var
    I: Integer;
  begin
    Result := Dest;
    for I := Low(Str) to High(Str) do
    begin
      Byte(Dest^) := Byte(Str[I]);
      Inc(Dest);
    end;
    Byte(Dest^) := 0;
  end;

  function GetICUVersion(hICU: NativeUInt): string;
  var
    I: Integer;
    mangle: string;
    Version: string;
    Temp: _ShortStr;
  begin
    mangle := 'u_strToUTF8';
    if dlsym(hICU, StrToCStr(mangle, CStr)) <> nil then Exit('');  // Apple has no version
    for I := 40 to 99 do
    begin
      Temp := _Str0Int64(I);
      CStrToStr(MarshaledAString(@Temp[1]), Byte(Temp[0]), Version);
      Result := '_' + Version;
      mangle := 'u_strToUTF8' + Result;
      if dlsym(hICU, StrToCStr(mangle, CStr)) <> nil then Exit;
      Result := '_' + Version[Low(string)] + '_' + Version[Low(string) + 1];
      mangle := 'u_strToUTF8' + Result;
      if dlsym(hICU, StrToCStr(mangle, CStr)) <> nil then Exit;
    end;
    Result := 'error';
  end;

  function GetFncAddress(Handle: NativeUInt; FunctionName: string): Pointer;
  begin
    Result := dlsym(Handle, StrToCStr(FunctionName + LibICUSuffix, CStr));
  end;

begin
  Result := False;
{$IFDEF MACOS}
  HICUUC := dlopen('/system/lib/libicucore.so', RTLD_LAZY);
  HICUI18N := HICUUC;
{$ELSE}
  HICUUC := dlopen('/system/lib/libicuuc.so', RTLD_LAZY);
  HICUI18N := dlopen('/system/lib/libicui18n.so', RTLD_LAZY);
{$ENDIF}
  if HICUUC > 0 then
  begin
    CStr := System.AllocMem(256);
    LibICUSuffix := GetICUVersion(HICUUC);
    if LibICUSuffix = 'error' then
    begin
      System.FreeMem(CStr);
      Exit;
    end;

    @u_strToUTF8 := GetFncAddress(HICUUC, 'u_strToUTF8');
    @u_strFromUTF8 := GetFncAddress(HICUUC, 'u_strFromUTF8');
    @u_strcasecmp := GetFncAddress(HICUUC, 'u_strcasecmp');

    @ucnv_open := GetFncAddress(HICUUC, 'ucnv_open');
    @ucnv_reset := GetFncAddress(HICUUC, 'ucnv_reset');
    @ucnv_close := GetFncAddress(HICUUC, 'ucnv_close');
    @ucnv_fromUChars := GetFncAddress(HICUUC, 'ucnv_fromUChars');
    @ucnv_toUChars := GetFncAddress(HICUUC, 'ucnv_toUChars');
    @ucnv_setSubstChars := GetFncAddress(System.HICUUC, 'ucnv_setSubstChars');
    @ucnv_getStarters := GetFncAddress(HICUUC, 'ucnv_getStarters');

    @ucol_open := GetFncAddress(HICUI18N, 'ucol_open');
    @ucol_close := GetFncAddress(HICUI18N, 'ucol_close');
    @ucol_strcollUTF8 := GetFncAddress(HICUI18N, 'ucol_strcollUTF8');

    System.FreeMem(CStr);
    Result := True;
  end;

end;
