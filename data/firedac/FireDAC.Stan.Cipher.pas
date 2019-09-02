{*******************************************************}
{                                                       }
{               Delphi FireDAC Framework                }
{                FireDAC cipher classes                 }
{                                                       }
{ Copyright(c) 2004-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}
{ --------------------------------------------------------------------------- }
{                                                                             }
{ Encryption scheme contributed by StreamSec                                  }
{ Portions (c)opyright StreamSec 2010-2012                                    }
{                                                                             }
{ A license to use this code is granted at no additional charge to owners of  }
{ a license to use the FireDAC code base this unit is part of.                }
{                                                                             }
{ The KDF, CTR-mode, CBC-MAC and the PRNG are all implemented in accordance   }
{ with various NIST standards.                                                }
{                                                                             }
{ --------------------------------------------------------------------------- }
{$I FireDAC.inc}

unit FireDAC.Stan.Cipher;

interface

uses
  System.Classes,
  FireDAC.Stan.Intf;

const
  C_FD_AESBlockSize = 16;
  C_FD_AESMaxRounds = 14;

type
  TFDMD5Hash = class;
  TFDCipher = class;
  TFDCipherClass = class of TFDCipher;
  TFDCipherAESBase = class;
  TFDCipherAES = class;
  TFDCipherAESCTR = class;
  TFDCipherAESECB = class;

  TFDDigest128 = packed record
    A, B, C, D: longword;
  end;

  TFDMD5Hash = class(TObject)
  public
    class function Hash(AStream: TStream; ACount: Integer = 0): TFDDigest128; overload;
    class function Digest2String(const AValue: TFDDigest128; ALowerCase: Boolean = False): String;
  end;

  PAESBlock = ^TAESBlock;
  TAESBlock = packed array[0 .. C_FD_AESBlockSize - 1] of Byte;

  TAESKey128 = packed array [0..15] of Byte;
  TAESKey192 = packed array [0..24] of Byte;
  TAESKey256 = packed array [0..32] of Byte;

  TRoundKey = packed array[0 .. (C_FD_AESBlockSize div 4) - 1] of LongWord;
  TKeyArray = packed array[0 .. C_FD_AESMaxRounds] of TRoundKey;

  TFDBlockAESCryptor = class(TObject)
  private
    FRK: TKeyArray;
    FRounds: LongWord;
    FInitialized: Boolean;
  protected
    function EncryptInit(const AKey; AKeySize: Cardinal): Boolean;
    function DecryptInit(const AKey; AKeySize: Cardinal): Boolean;
  public
    procedure Encrypt(const BI: TAESBlock; var BO: TAESBlock); overload;
    procedure Decrypt(const BI: TAESBlock; var BO: TAESBlock); overload;
    procedure Encrypt(var B: TAESBlock); overload;
    procedure Decrypt(var B: TAESBlock); overload;
    function Initialize(const AKey; AKeySize: Cardinal; AEncrypt: Boolean): Boolean;
  end;

  TFDCipher = class(TObject)
  protected
    procedure Finalize; virtual; abstract;
  public
    class function ReserveLength: Cardinal; virtual;
    procedure Initialize(aKeyMaterial: PByte; aKeyLength: Integer;
      const aSalt: TAESBlock; aSaltLength: Integer; aKeySize: Word); virtual; abstract;
    // Return values
    //    0: OK
    //    1: Corrupt input (decrypt only)
    //    2: Not properly initialized (e.g. unsupported key size)
    function Process(ApIn, ApOut: PAESBlock; ACount: Integer;
      APageNum: Cardinal; AEncrypt: Boolean): Integer; virtual; abstract;
    destructor Destroy; override;
  end;

  TFDCipherAESBase = class(TFDCipher)
  private
    fEncryptor: TFDBlockAESCryptor;
    fMac: TFDBlockAESCryptor;
    fIV: TFDBlockAESCryptor;
    fIVSeed: TAESBlock;
    procedure BCC(aCryptor: TFDBlockAESCryptor; aSrc: TMemoryStream; var aDst: TAESBlock);
    procedure KDF128(aKeyMaterial: PByte; aKeyLength: Integer;
      aLabel: Char; const aSalt: TAESBlock; var aKey: TAESKey128);
    procedure KDF192(aKeyMaterial: PByte; aKeyLength: Integer;
      aLabel: Char; const aSalt: TAESBlock; var aKey: TAESKey192);
    procedure KDF256(aKeyMaterial: PByte; aKeyLength: Integer;
      aLabel: Char; const aSalt: TAESBlock; var aKey: TAESKey256);
    function GenerateIV(out aIV: TAESBlock): Integer;
  protected
    procedure Finalize; override;
  end;

  // Confidentiality and integrity. The scheme is a conventional generic
  // composition of CTR mode and CBC-MAC.
  TFDCipherAES = class(TFDCipherAESBase)
  public
    class function ReserveLength: Cardinal; override;
    procedure Initialize(aKeyMaterial: PByte; aKeyLength: Integer;
      const aSalt: TAESBlock; aSaltLength: Integer; aKeySize: Word); override;
    function Process(ApIn, ApOut: PAESBlock; ACount: Integer;
      APageNum: Cardinal; AEncrypt: Boolean): Integer; override;
  end;

  // Confidentiality only. This implementation is vulnerable to adaptive attacks.
  // CTR mode with a 96 bit effective nonce.
  TFDCipherAESCTR = class(TFDCipherAESBase)
  public
    class function ReserveLength: Cardinal; override;
    procedure Initialize(aKeyMaterial: PByte; aKeyLength: Integer;
      const aSalt: TAESBlock; aSaltLength: Integer; aKeySize: Word); override;
    function Process(ApIn, ApOut: PAESBlock; ACount: Integer;
      APageNum: Cardinal; AEncrypt: Boolean): Integer; override;
  end;

  // Weak confidentiality and no integrity. But more fast and less DB size.
  TFDCipherAESECB = class(TFDCipher)
  private
    fEncryptor: TFDBlockAESCryptor;
    fDecryptor: TFDBlockAESCryptor;
  protected
    procedure Finalize; override;
  public
    class function ReserveLength: Cardinal; override;
    procedure Initialize(aKeyMaterial: PByte; aKeyLength: Integer;
      const aSalt: TAESBlock; aSaltLength: Integer; aKeySize: Word); override;
    function Process(ApIn, ApOut: PAESBlock; ACount: Integer;
      APageNum: Cardinal; AEncrypt: Boolean): Integer; override;
  end;

  function FDCipherParsePassword(APwd: PFDAnsiString; APwdLen: Integer;
    out ACipherClass: TFDCipherClass; out AKeyLen, ANameLen: Integer): Boolean;
  function FDCipherGetClasses: String;
  function FDCipherGetName(ACipherClass: TFDCipherClass; AKeyLen: Integer): PFDAnsiString;

implementation

uses
  System.SysUtils, System.Math,
  FireDAC.Stan.Util;

{-------------------------------------------------------------------------------}
{ TFDMD5Hash                                                                    }
{-------------------------------------------------------------------------------}
{$UNDEF SaveQ} {$IFOPT Q+} {$Q-} {$DEFINE SaveQ} {$ENDIF}
{$UNDEF SaveR} {$IFOPT R+} {$R-} {$DEFINE SaveR} {$ENDIF}
class function TFDMD5Hash.Hash(AStream: TStream; ACount: Integer): TFDDigest128;
var
  FA, FB, FC, FD: LongWord;
  FAA, FBB, FCC, FDD: LongWord;
  Buffer: array [0..4159] of Byte;
  Temp: array [0..15] of LongWord;
  I, iRead: Integer;
  iCount64: Int64;
  lDone: Boolean;
begin
  if ACount = 0 then begin
    AStream.Position := 0;
    ACount := AStream.Size;
  end
  else
    ACount := Min(ACount, AStream.Size - AStream.Position);
  FillChar(Result, SizeOf(Result), 0);
  // intializing
  lDone := False;
  FA := $67452301;
  FB := $EFCDAB89;
  FC := $98BADCFE;
  FD := $10325476;
  iCount64 := ACount;
  // processing
  repeat
    iRead := AStream.Read(Buffer, Min(4096, ACount));
    Dec(ACount, iRead);
    if iRead < 4096 then begin
      // the end of stream is reached
      Buffer[iRead] := $80;
      Inc(iRead);
      while (iRead mod 64) <> 56 do begin
        Buffer[iRead] := 0;
        Inc(iRead);
      end;
      iCount64 := iCount64 * 8;
      Move(iCount64, Buffer[iRead], 8);
      Inc(iRead, 8);
      lDone := True;
    end;
    I := 0;
    repeat
      // transforming
      Move(Buffer[I], Temp, SizeOf(Temp));
      FAA := FA;
      FBB := FB;
      FCC := FC;
      FDD := FD;
      // round 1
      Inc(FA, ((FB and FC) or (not FB and FD)) + Temp[0] + $D76AA478);
      FA := ((FA shl 7) or (FA shr 25)) + FB;
      Inc(FD, ((FA and FB) or (not FA and FC)) + Temp[1] + $E8C7B756);
      FD := ((FD shl 12) or (FD shr 20)) + FA;
      Inc(FC, ((FD and FA) or (not FD and FB)) + Temp[2] + $242070DB);
      FC := ((FC shl 17) or (FC shr 15)) + FD;
      Inc(FB, ((FC and FD) or (not FC and FA)) + Temp[3] + $C1BDCEEE);
      FB := ((FB shl 22) or (FB shr 10)) + FC;
      Inc(FA, ((FB and FC) or (not FB and FD)) + Temp[4] + $F57C0FAF);
      FA := ((FA shl 7) or (FA shr 25)) + FB;
      Inc(FD, ((FA and FB) or (not FA and FC)) + Temp[5] + $4787C62A);
      FD := ((FD shl 12) or (FD shr 20)) + FA;
      Inc(FC, ((FD and FA) or (not FD and FB)) + Temp[6] + $A8304613);
      FC := ((FC shl 17) or (FC shr 15)) + FD;
      Inc(FB, ((FC and FD) or (not FC and FA)) + Temp[7] + $FD469501);
      FB := ((FB shl 22) or (FB shr 10)) + FC;
      Inc(FA, ((FB and FC) or (not FB and FD)) + Temp[8] + $698098D8);
      FA := ((FA shl 7) or (FA shr 25)) + FB;
      Inc(FD, ((FA and FB) or (not FA and FC)) + Temp[9] + $8B44F7AF);
      FD := ((FD shl 12) or (FD shr 20)) + FA;
      Inc(FC, ((FD and FA) or (not FD and FB)) + Temp[10] + $FFFF5BB1);
      FC := ((FC shl 17) or (FC shr 15)) + FD;
      Inc(FB, ((FC and FD) or (not FC and FA)) + Temp[11] + $895CD7BE);
      FB := ((FB shl 22) or (FB shr 10)) + FC;
      Inc(FA, ((FB and FC) or (not FB and FD)) + Temp[12] + $6B901122);
      FA := ((FA shl 7) or (FA shr 25)) + FB;
      Inc(FD, ((FA and FB) or (not FA and FC)) + Temp[13] + $FD987193);
      FD := ((FD shl 12) or (FD shr 20)) + FA;
      Inc(FC, ((FD and FA) or (not FD and FB)) + Temp[14] + $A679438E);
      FC := ((FC shl 17) or (FC shr 15)) + FD;
      Inc(FB, ((FC and FD) or (not FC and FA)) + Temp[15] + $49B40821);
      FB := ((FB shl 22) or (FB shr 10)) + FC;
      // round 2
      Inc(FA, ((FB and FD) or (FC and not FD)) + Temp[1] + $F61E2562);
      FA := ((FA shl 5) or (FA shr 27)) + FB;
      Inc(FD, ((FA and FC) or (FB and not FC)) + Temp [6] + $C040B340);
      FD := ((FD shl 9) or (FD shr 23)) + FA;
      Inc(FC, ((FD and FB) or (FA and not FB)) + Temp[11] + $265E5A51);
      FC := ((FC shl 14) or (FC shr 18)) + FD;
      Inc(FB, ((FC and FA) or (FD and not FA)) + Temp[0] + $E9B6C7AA);
      FB := ((FB shl 20) or (FB shr 12)) + FC;
      Inc(FA, ((FB and FD) or (FC and not FD)) + Temp[5] + $D62F105D);
      FA := ((FA shl 5) or (FA shr 27)) + FB;
      Inc(FD, ((FA and FC) or (FB and not FC)) + Temp [10] + $2441453);
      FD := ((FD shl 9) or (FD shr 23)) + FA;
      Inc(FC, ((FD and FB) or (FA and not FB)) + Temp[15] + $D8A1E681);
      FC := ((FC shl 14) or (FC shr 18)) + FD;
      Inc(FB, ((FC and FA) or (FD and not FA)) + Temp[4] + $E7D3FBC8);
      FB := ((FB shl 20) or (FB shr 12)) + FC;
      Inc(FA, ((FB and FD) or (FC and not FD)) + Temp[9] + $21E1CDE6);
      FA := ((FA shl 5) or (FA shr 27)) + FB;
      Inc(FD, ((FA and FC) or (FB and not FC)) + Temp [14] + $C33707D6);
      FD := ((FD shl 9) or (FD shr 23)) + FA;
      Inc(FC, ((FD and FB) or (FA and not FB)) + Temp[3] + $F4D50D87);
      FC := ((FC shl 14) or (FC shr 18)) + FD;
      Inc(FB, ((FC and FA) or (FD and not FA)) + Temp[8] + $455A14ED);
      FB := ((FB shl 20) or (FB shr 12)) + FC;
      Inc(FA, ((FB and FD) or (FC and not FD)) + Temp[13] + $A9E3E905);
      FA := ((FA shl 5) or (FA shr 27)) + FB;
      Inc(FD, ((FA and FC) or (FB and not FC)) + Temp [2] + $FCEFA3F8);
      FD := ((FD shl 9) or (FD shr 23)) + FA;
      Inc(FC, ((FD and FB) or (FA and not FB)) + Temp[7] + $676F02D9);
      FC := ((FC shl 14) or (FC shr 18)) + FD;
      Inc(FB, ((FC and FA) or (FD and not FA)) + Temp[12] + $8D2A4C8A);
      FB := ((FB shl 20) or (FB shr 12)) + FC;
      // round 3
      Inc(FA, (FB xor FC xor FD) + Temp[5] + $FFFA3942);
      FA := FB + ((FA shl 4) or (FA shr 28));
      Inc(FD, (FA xor FB xor FC) + Temp[8] + $8771F681);
      FD := FA + ((FD shl 11) or (FD shr 21));
      Inc(FC, (FD xor FA xor FB) + Temp[11] + $6D9D6122);
      FC := FD + ((FC shl 16) or (FC shr 16));
      Inc(FB, (FC xor FD xor FA) + Temp[14] + $FDE5380C);
      FB := FC + ((FB shl 23) or (FB shr 9));
      Inc(FA, (FB xor FC xor FD) + Temp[1] + $A4BEEA44);
      FA := FB + ((FA shl 4) or (FA shr 28));
      Inc(FD, (FA xor FB xor FC) + Temp[4] + $4BDECFA9);
      FD := FA + ((FD shl 11) or (FD shr 21));
      Inc(FC, (FD xor FA xor FB) + Temp[7] + $F6BB4B60);
      FC := FD + ((FC shl 16) or (FC shr 16));
      Inc(FB, (FC xor FD xor FA) + Temp[10] + $BEBFBC70);
      FB := FC + ((FB shl 23) or (FB shr 9));
      Inc(FA, (FB xor FC xor FD) + Temp[13] + $289B7EC6);
      FA := FB + ((FA shl 4) or (FA shr 28));
      Inc(FD, (FA xor FB xor FC) + Temp[0] + $EAA127FA);
      FD := FA + ((FD shl 11) or (FD shr 21));
      Inc(FC, (FD xor FA xor FB) + Temp[3] + $D4EF3085);
      FC := FD + ((FC shl 16) or (FC shr 16));
      Inc(FB, (FC xor FD xor FA) + Temp[6] + $4881D05);
      FB := FC + ((FB shl 23) or (FB shr 9));
      Inc(FA, (FB xor FC xor FD) + Temp[9] + $D9D4D039);
      FA := FB + ((FA shl 4) or (FA shr 28));
      Inc(FD, (FA xor FB xor FC) + Temp[12] + $E6DB99E5);
      FD := FA + ((FD shl 11) or (FD shr 21));
      Inc(FC, (FD xor FA xor FB) + Temp[15] + $1FA27CF8);
      FC := FD + ((FC shl 16) or (FC shr 16));
      Inc(FB, (FC xor FD xor FA) + Temp[2] + $C4AC5665);
      FB := FC + ((FB shl 23) or (FB shr 9));
      // round 4
      Inc(FA, (FC xor (FB or not FD)) + Temp[0] + $F4292244);
      FA := FB + ((FA shl 6) or (FA shr 26));
      Inc(FD, (FB xor (FA or not FC)) + Temp[7] + $432AFF97);
      FD := FA + ((FD shl 10) or (FD shr 22));
      Inc(FC, (FA xor (FD or not FB)) + Temp[14] + $AB9423A7);
      FC := FD + ((FC shl 15) or (FC shr 17));
      Inc(FB, (FD xor (FC or not FA)) + Temp[5] + $FC93A039);
      FB := FC + ((FB shl 21) or (FB shr 11));
      Inc(FA, (FC xor (FB or not FD)) + Temp[12] + $655B59C3);
      FA := FB + ((FA shl 6) or (FA shr 26));
      Inc(FD, (FB xor (FA or not FC)) + Temp[3] + $8F0CCC92);
      FD := FA + ((FD shl 10) or (FD shr 22));
      Inc(FC, (FA xor (FD or not FB)) + Temp[10] + $FFEFF47D);
      FC := FD + ((FC shl 15) or (FC shr 17));
      Inc(FB, (FD xor (FC or not FA)) + Temp[1] + $85845DD1);
      FB := FC + ((FB shl 21) or (FB shr 11));
      Inc(FA, (FC xor (FB or not FD)) + Temp[8] + $6FA87E4F);
      FA := FB + ((FA shl 6) or (FA shr 26));
      Inc(FD, (FB xor (FA or not FC)) + Temp[15] + $FE2CE6E0);
      FD := FA + ((FD shl 10) or (FD shr 22));
      Inc(FC, (FA xor (FD or not FB)) + Temp[6] + $A3014314);
      FC := FD + ((FC shl 15) or (FC shr 17));
      Inc(FB, (FD xor (FC or not FA)) + Temp[13] + $4E0811A1);
      FB := FC + ((FB shl 21) or (FB shr 11));
      Inc(FA, (FC xor (FB or not FD)) + Temp[4] + $F7537E82);
      FA := FB + ((FA shl 6) or (FA shr 26));
      Inc(FD, (FB xor (FA or not FC)) + Temp[11] + $BD3AF235);
      FD := FA + ((FD shl 10) or (FD shr 22));
      Inc(FC, (FA xor (FD or not FB)) + Temp[2] + $2AD7D2BB);
      FC := FD + ((FC shl 15) or (FC shr 17));
      Inc(FB, (FD xor (FC or not FA)) + Temp[9] + $EB86D391);
      FB := FC + ((FB shl 21) or (FB shr 11));

      Inc(FA, FAA);
      Inc(FB, FBB);
      Inc(FC, FCC);
      Inc(FD, FDD);
      Inc(I, 64);
    until I = iRead;
  until lDone;
  // finalizing
  Result.A := FA;
  Result.B := FB;
  Result.C := FC;
  Result.D := FD;
end;
{$IFDEF SaveQ} {$Q+} {$UNDEF SaveQ} {$ENDIF}
{$IFDEF SaveR} {$R+} {$UNDEF SaveR} {$ENDIF}

{-------------------------------------------------------------------------------}
class function TFDMD5Hash.Digest2String(const AValue: TFDDigest128; ALowerCase: Boolean = False): String;
const
  LowerAlphabet: String = '0123456789abcdef';
  UpperAlphabet: String = '0123456789ABCDEF';
var
  Buffer: array [0..15] of Byte absolute AValue;
  i: Integer;
  pAlphabet: PChar;
begin
  Result := '';
  if ALowerCase then
    pAlphabet := PChar(LowerAlphabet)
  else
    pAlphabet := PChar(UpperAlphabet);
  for i := 0 to 15 do
    Result := Result + (pAlphabet + (Buffer[i] div 16))^ +
      (pAlphabet + (Buffer[i] mod 16))^;
end;

{-------------------------------------------------------------------------------}
{ AES                                                                           }
{-------------------------------------------------------------------------------}
const
  RCon: array[0..9] of Cardinal = ($01,$02,$04,$08,$10,$20,$40,$80,$1b,$36);

  SBoxAES: array [0..255,0..7] of Byte = (
    ($c6,$63,$63,$a5,$c6,$63,$63,$a5), ($f8,$7c,$7c,$84,$f8,$7c,$7c,$84),
    ($ee,$77,$77,$99,$ee,$77,$77,$99), ($f6,$7b,$7b,$8d,$f6,$7b,$7b,$8d),
    ($ff,$f2,$f2,$0d,$ff,$f2,$f2,$0d), ($d6,$6b,$6b,$bd,$d6,$6b,$6b,$bd),
    ($de,$6f,$6f,$b1,$de,$6f,$6f,$b1), ($91,$c5,$c5,$54,$91,$c5,$c5,$54),
    ($60,$30,$30,$50,$60,$30,$30,$50), ($02,$01,$01,$03,$02,$01,$01,$03),
    ($ce,$67,$67,$a9,$ce,$67,$67,$a9), ($56,$2b,$2b,$7d,$56,$2b,$2b,$7d),
    ($e7,$fe,$fe,$19,$e7,$fe,$fe,$19), ($b5,$d7,$d7,$62,$b5,$d7,$d7,$62),
    ($4d,$ab,$ab,$e6,$4d,$ab,$ab,$e6), ($ec,$76,$76,$9a,$ec,$76,$76,$9a),
    ($8f,$ca,$ca,$45,$8f,$ca,$ca,$45), ($1f,$82,$82,$9d,$1f,$82,$82,$9d),
    ($89,$c9,$c9,$40,$89,$c9,$c9,$40), ($fa,$7d,$7d,$87,$fa,$7d,$7d,$87),
    ($ef,$fa,$fa,$15,$ef,$fa,$fa,$15), ($b2,$59,$59,$eb,$b2,$59,$59,$eb),
    ($8e,$47,$47,$c9,$8e,$47,$47,$c9), ($fb,$f0,$f0,$0b,$fb,$f0,$f0,$0b),
    ($41,$ad,$ad,$ec,$41,$ad,$ad,$ec), ($b3,$d4,$d4,$67,$b3,$d4,$d4,$67),
    ($5f,$a2,$a2,$fd,$5f,$a2,$a2,$fd), ($45,$af,$af,$ea,$45,$af,$af,$ea),
    ($23,$9c,$9c,$bf,$23,$9c,$9c,$bf), ($53,$a4,$a4,$f7,$53,$a4,$a4,$f7),
    ($e4,$72,$72,$96,$e4,$72,$72,$96), ($9b,$c0,$c0,$5b,$9b,$c0,$c0,$5b),
    ($75,$b7,$b7,$c2,$75,$b7,$b7,$c2), ($e1,$fd,$fd,$1c,$e1,$fd,$fd,$1c),
    ($3d,$93,$93,$ae,$3d,$93,$93,$ae), ($4c,$26,$26,$6a,$4c,$26,$26,$6a),
    ($6c,$36,$36,$5a,$6c,$36,$36,$5a), ($7e,$3f,$3f,$41,$7e,$3f,$3f,$41),
    ($f5,$f7,$f7,$02,$f5,$f7,$f7,$02), ($83,$cc,$cc,$4f,$83,$cc,$cc,$4f),
    ($68,$34,$34,$5c,$68,$34,$34,$5c), ($51,$a5,$a5,$f4,$51,$a5,$a5,$f4),
    ($d1,$e5,$e5,$34,$d1,$e5,$e5,$34), ($f9,$f1,$f1,$08,$f9,$f1,$f1,$08),
    ($e2,$71,$71,$93,$e2,$71,$71,$93), ($ab,$d8,$d8,$73,$ab,$d8,$d8,$73),
    ($62,$31,$31,$53,$62,$31,$31,$53), ($2a,$15,$15,$3f,$2a,$15,$15,$3f),
    ($08,$04,$04,$0c,$08,$04,$04,$0c), ($95,$c7,$c7,$52,$95,$c7,$c7,$52),
    ($46,$23,$23,$65,$46,$23,$23,$65), ($9d,$c3,$c3,$5e,$9d,$c3,$c3,$5e),
    ($30,$18,$18,$28,$30,$18,$18,$28), ($37,$96,$96,$a1,$37,$96,$96,$a1),
    ($0a,$05,$05,$0f,$0a,$05,$05,$0f), ($2f,$9a,$9a,$b5,$2f,$9a,$9a,$b5),
    ($0e,$07,$07,$09,$0e,$07,$07,$09), ($24,$12,$12,$36,$24,$12,$12,$36),
    ($1b,$80,$80,$9b,$1b,$80,$80,$9b), ($df,$e2,$e2,$3d,$df,$e2,$e2,$3d),
    ($cd,$eb,$eb,$26,$cd,$eb,$eb,$26), ($4e,$27,$27,$69,$4e,$27,$27,$69),
    ($7f,$b2,$b2,$cd,$7f,$b2,$b2,$cd), ($ea,$75,$75,$9f,$ea,$75,$75,$9f),
    ($12,$09,$09,$1b,$12,$09,$09,$1b), ($1d,$83,$83,$9e,$1d,$83,$83,$9e),
    ($58,$2c,$2c,$74,$58,$2c,$2c,$74), ($34,$1a,$1a,$2e,$34,$1a,$1a,$2e),
    ($36,$1b,$1b,$2d,$36,$1b,$1b,$2d), ($dc,$6e,$6e,$b2,$dc,$6e,$6e,$b2),
    ($b4,$5a,$5a,$ee,$b4,$5a,$5a,$ee), ($5b,$a0,$a0,$fb,$5b,$a0,$a0,$fb),
    ($a4,$52,$52,$f6,$a4,$52,$52,$f6), ($76,$3b,$3b,$4d,$76,$3b,$3b,$4d),
    ($b7,$d6,$d6,$61,$b7,$d6,$d6,$61), ($7d,$b3,$b3,$ce,$7d,$b3,$b3,$ce),
    ($52,$29,$29,$7b,$52,$29,$29,$7b), ($dd,$e3,$e3,$3e,$dd,$e3,$e3,$3e),
    ($5e,$2f,$2f,$71,$5e,$2f,$2f,$71), ($13,$84,$84,$97,$13,$84,$84,$97),
    ($a6,$53,$53,$f5,$a6,$53,$53,$f5), ($b9,$d1,$d1,$68,$b9,$d1,$d1,$68),
    ($00,$00,$00,$00,$00,$00,$00,$00), ($c1,$ed,$ed,$2c,$c1,$ed,$ed,$2c),
    ($40,$20,$20,$60,$40,$20,$20,$60), ($e3,$fc,$fc,$1f,$e3,$fc,$fc,$1f),
    ($79,$b1,$b1,$c8,$79,$b1,$b1,$c8), ($b6,$5b,$5b,$ed,$b6,$5b,$5b,$ed),
    ($d4,$6a,$6a,$be,$d4,$6a,$6a,$be), ($8d,$cb,$cb,$46,$8d,$cb,$cb,$46),
    ($67,$be,$be,$d9,$67,$be,$be,$d9), ($72,$39,$39,$4b,$72,$39,$39,$4b),
    ($94,$4a,$4a,$de,$94,$4a,$4a,$de), ($98,$4c,$4c,$d4,$98,$4c,$4c,$d4),
    ($b0,$58,$58,$e8,$b0,$58,$58,$e8), ($85,$cf,$cf,$4a,$85,$cf,$cf,$4a),
    ($bb,$d0,$d0,$6b,$bb,$d0,$d0,$6b), ($c5,$ef,$ef,$2a,$c5,$ef,$ef,$2a),
    ($4f,$aa,$aa,$e5,$4f,$aa,$aa,$e5), ($ed,$fb,$fb,$16,$ed,$fb,$fb,$16),
    ($86,$43,$43,$c5,$86,$43,$43,$c5), ($9a,$4d,$4d,$d7,$9a,$4d,$4d,$d7),
    ($66,$33,$33,$55,$66,$33,$33,$55), ($11,$85,$85,$94,$11,$85,$85,$94),
    ($8a,$45,$45,$cf,$8a,$45,$45,$cf), ($e9,$f9,$f9,$10,$e9,$f9,$f9,$10),
    ($04,$02,$02,$06,$04,$02,$02,$06), ($fe,$7f,$7f,$81,$fe,$7f,$7f,$81),
    ($a0,$50,$50,$f0,$a0,$50,$50,$f0), ($78,$3c,$3c,$44,$78,$3c,$3c,$44),
    ($25,$9f,$9f,$ba,$25,$9f,$9f,$ba), ($4b,$a8,$a8,$e3,$4b,$a8,$a8,$e3),
    ($a2,$51,$51,$f3,$a2,$51,$51,$f3), ($5d,$a3,$a3,$fe,$5d,$a3,$a3,$fe),
    ($80,$40,$40,$c0,$80,$40,$40,$c0), ($05,$8f,$8f,$8a,$05,$8f,$8f,$8a),
    ($3f,$92,$92,$ad,$3f,$92,$92,$ad), ($21,$9d,$9d,$bc,$21,$9d,$9d,$bc),
    ($70,$38,$38,$48,$70,$38,$38,$48), ($f1,$f5,$f5,$04,$f1,$f5,$f5,$04),
    ($63,$bc,$bc,$df,$63,$bc,$bc,$df), ($77,$b6,$b6,$c1,$77,$b6,$b6,$c1),
    ($af,$da,$da,$75,$af,$da,$da,$75), ($42,$21,$21,$63,$42,$21,$21,$63),
    ($20,$10,$10,$30,$20,$10,$10,$30), ($e5,$ff,$ff,$1a,$e5,$ff,$ff,$1a),
    ($fd,$f3,$f3,$0e,$fd,$f3,$f3,$0e), ($bf,$d2,$d2,$6d,$bf,$d2,$d2,$6d),
    ($81,$cd,$cd,$4c,$81,$cd,$cd,$4c), ($18,$0c,$0c,$14,$18,$0c,$0c,$14),
    ($26,$13,$13,$35,$26,$13,$13,$35), ($c3,$ec,$ec,$2f,$c3,$ec,$ec,$2f),
    ($be,$5f,$5f,$e1,$be,$5f,$5f,$e1), ($35,$97,$97,$a2,$35,$97,$97,$a2),
    ($88,$44,$44,$cc,$88,$44,$44,$cc), ($2e,$17,$17,$39,$2e,$17,$17,$39),
    ($93,$c4,$c4,$57,$93,$c4,$c4,$57), ($55,$a7,$a7,$f2,$55,$a7,$a7,$f2),
    ($fc,$7e,$7e,$82,$fc,$7e,$7e,$82), ($7a,$3d,$3d,$47,$7a,$3d,$3d,$47),
    ($c8,$64,$64,$ac,$c8,$64,$64,$ac), ($ba,$5d,$5d,$e7,$ba,$5d,$5d,$e7),
    ($32,$19,$19,$2b,$32,$19,$19,$2b), ($e6,$73,$73,$95,$e6,$73,$73,$95),
    ($c0,$60,$60,$a0,$c0,$60,$60,$a0), ($19,$81,$81,$98,$19,$81,$81,$98),
    ($9e,$4f,$4f,$d1,$9e,$4f,$4f,$d1), ($a3,$dc,$dc,$7f,$a3,$dc,$dc,$7f),
    ($44,$22,$22,$66,$44,$22,$22,$66), ($54,$2a,$2a,$7e,$54,$2a,$2a,$7e),
    ($3b,$90,$90,$ab,$3b,$90,$90,$ab), ($0b,$88,$88,$83,$0b,$88,$88,$83),
    ($8c,$46,$46,$ca,$8c,$46,$46,$ca), ($c7,$ee,$ee,$29,$c7,$ee,$ee,$29),
    ($6b,$b8,$b8,$d3,$6b,$b8,$b8,$d3), ($28,$14,$14,$3c,$28,$14,$14,$3c),
    ($a7,$de,$de,$79,$a7,$de,$de,$79), ($bc,$5e,$5e,$e2,$bc,$5e,$5e,$e2),
    ($16,$0b,$0b,$1d,$16,$0b,$0b,$1d), ($ad,$db,$db,$76,$ad,$db,$db,$76),
    ($db,$e0,$e0,$3b,$db,$e0,$e0,$3b), ($64,$32,$32,$56,$64,$32,$32,$56),
    ($74,$3a,$3a,$4e,$74,$3a,$3a,$4e), ($14,$0a,$0a,$1e,$14,$0a,$0a,$1e),
    ($92,$49,$49,$db,$92,$49,$49,$db), ($0c,$06,$06,$0a,$0c,$06,$06,$0a),
    ($48,$24,$24,$6c,$48,$24,$24,$6c), ($b8,$5c,$5c,$e4,$b8,$5c,$5c,$e4),
    ($9f,$c2,$c2,$5d,$9f,$c2,$c2,$5d), ($bd,$d3,$d3,$6e,$bd,$d3,$d3,$6e),
    ($43,$ac,$ac,$ef,$43,$ac,$ac,$ef), ($c4,$62,$62,$a6,$c4,$62,$62,$a6),
    ($39,$91,$91,$a8,$39,$91,$91,$a8), ($31,$95,$95,$a4,$31,$95,$95,$a4),
    ($d3,$e4,$e4,$37,$d3,$e4,$e4,$37), ($f2,$79,$79,$8b,$f2,$79,$79,$8b),
    ($d5,$e7,$e7,$32,$d5,$e7,$e7,$32), ($8b,$c8,$c8,$43,$8b,$c8,$c8,$43),
    ($6e,$37,$37,$59,$6e,$37,$37,$59), ($da,$6d,$6d,$b7,$da,$6d,$6d,$b7),
    ($01,$8d,$8d,$8c,$01,$8d,$8d,$8c), ($b1,$d5,$d5,$64,$b1,$d5,$d5,$64),
    ($9c,$4e,$4e,$d2,$9c,$4e,$4e,$d2), ($49,$a9,$a9,$e0,$49,$a9,$a9,$e0),
    ($d8,$6c,$6c,$b4,$d8,$6c,$6c,$b4), ($ac,$56,$56,$fa,$ac,$56,$56,$fa),
    ($f3,$f4,$f4,$07,$f3,$f4,$f4,$07), ($cf,$ea,$ea,$25,$cf,$ea,$ea,$25),
    ($ca,$65,$65,$af,$ca,$65,$65,$af), ($f4,$7a,$7a,$8e,$f4,$7a,$7a,$8e),
    ($47,$ae,$ae,$e9,$47,$ae,$ae,$e9), ($10,$08,$08,$18,$10,$08,$08,$18),
    ($6f,$ba,$ba,$d5,$6f,$ba,$ba,$d5), ($f0,$78,$78,$88,$f0,$78,$78,$88),
    ($4a,$25,$25,$6f,$4a,$25,$25,$6f), ($5c,$2e,$2e,$72,$5c,$2e,$2e,$72),
    ($38,$1c,$1c,$24,$38,$1c,$1c,$24), ($57,$a6,$a6,$f1,$57,$a6,$a6,$f1),
    ($73,$b4,$b4,$c7,$73,$b4,$b4,$c7), ($97,$c6,$c6,$51,$97,$c6,$c6,$51),
    ($cb,$e8,$e8,$23,$cb,$e8,$e8,$23), ($a1,$dd,$dd,$7c,$a1,$dd,$dd,$7c),
    ($e8,$74,$74,$9c,$e8,$74,$74,$9c), ($3e,$1f,$1f,$21,$3e,$1f,$1f,$21),
    ($96,$4b,$4b,$dd,$96,$4b,$4b,$dd), ($61,$bd,$bd,$dc,$61,$bd,$bd,$dc),
    ($0d,$8b,$8b,$86,$0d,$8b,$8b,$86), ($0f,$8a,$8a,$85,$0f,$8a,$8a,$85),
    ($e0,$70,$70,$90,$e0,$70,$70,$90), ($7c,$3e,$3e,$42,$7c,$3e,$3e,$42),
    ($71,$b5,$b5,$c4,$71,$b5,$b5,$c4), ($cc,$66,$66,$aa,$cc,$66,$66,$aa),
    ($90,$48,$48,$d8,$90,$48,$48,$d8), ($06,$03,$03,$05,$06,$03,$03,$05),
    ($f7,$f6,$f6,$01,$f7,$f6,$f6,$01), ($1c,$0e,$0e,$12,$1c,$0e,$0e,$12),
    ($c2,$61,$61,$a3,$c2,$61,$61,$a3), ($6a,$35,$35,$5f,$6a,$35,$35,$5f),
    ($ae,$57,$57,$f9,$ae,$57,$57,$f9), ($69,$b9,$b9,$d0,$69,$b9,$b9,$d0),
    ($17,$86,$86,$91,$17,$86,$86,$91), ($99,$c1,$c1,$58,$99,$c1,$c1,$58),
    ($3a,$1d,$1d,$27,$3a,$1d,$1d,$27), ($27,$9e,$9e,$b9,$27,$9e,$9e,$b9),
    ($d9,$e1,$e1,$38,$d9,$e1,$e1,$38), ($eb,$f8,$f8,$13,$eb,$f8,$f8,$13),
    ($2b,$98,$98,$b3,$2b,$98,$98,$b3), ($22,$11,$11,$33,$22,$11,$11,$33),
    ($d2,$69,$69,$bb,$d2,$69,$69,$bb), ($a9,$d9,$d9,$70,$a9,$d9,$d9,$70),
    ($07,$8e,$8e,$89,$07,$8e,$8e,$89), ($33,$94,$94,$a7,$33,$94,$94,$a7),
    ($2d,$9b,$9b,$b6,$2d,$9b,$9b,$b6), ($3c,$1e,$1e,$22,$3c,$1e,$1e,$22),
    ($15,$87,$87,$92,$15,$87,$87,$92), ($c9,$e9,$e9,$20,$c9,$e9,$e9,$20),
    ($87,$ce,$ce,$49,$87,$ce,$ce,$49), ($aa,$55,$55,$ff,$aa,$55,$55,$ff),
    ($50,$28,$28,$78,$50,$28,$28,$78), ($a5,$df,$df,$7a,$a5,$df,$df,$7a),
    ($03,$8c,$8c,$8f,$03,$8c,$8c,$8f), ($59,$a1,$a1,$f8,$59,$a1,$a1,$f8),
    ($09,$89,$89,$80,$09,$89,$89,$80), ($1a,$0d,$0d,$17,$1a,$0d,$0d,$17),
    ($65,$bf,$bf,$da,$65,$bf,$bf,$da), ($d7,$e6,$e6,$31,$d7,$e6,$e6,$31),
    ($84,$42,$42,$c6,$84,$42,$42,$c6), ($d0,$68,$68,$b8,$d0,$68,$68,$b8),
    ($82,$41,$41,$c3,$82,$41,$41,$c3), ($29,$99,$99,$b0,$29,$99,$99,$b0),
    ($5a,$2d,$2d,$77,$5a,$2d,$2d,$77), ($1e,$0f,$0f,$11,$1e,$0f,$0f,$11),
    ($7b,$b0,$b0,$cb,$7b,$b0,$b0,$cb), ($a8,$54,$54,$fc,$a8,$54,$54,$fc),
    ($6d,$bb,$bb,$d6,$6d,$bb,$bb,$d6), ($2c,$16,$16,$3a,$2c,$16,$16,$3a)
  );
  IBoxAES: array [0..255,0..7] of Byte = (
    ($51,$f4,$a7,$50,$51,$f4,$a7,$52), ($7e,$41,$65,$53,$7e,$41,$65,$09),
    ($1a,$17,$a4,$c3,$1a,$17,$a4,$6a), ($3a,$27,$5e,$96,$3a,$27,$5e,$d5),
    ($3b,$ab,$6b,$cb,$3b,$ab,$6b,$30), ($1f,$9d,$45,$f1,$1f,$9d,$45,$36),
    ($ac,$fa,$58,$ab,$ac,$fa,$58,$a5), ($4b,$e3,$03,$93,$4b,$e3,$03,$38),
    ($20,$30,$fa,$55,$20,$30,$fa,$bf), ($ad,$76,$6d,$f6,$ad,$76,$6d,$40),
    ($88,$cc,$76,$91,$88,$cc,$76,$a3), ($f5,$02,$4c,$25,$f5,$02,$4c,$9e),
    ($4f,$e5,$d7,$fc,$4f,$e5,$d7,$81), ($c5,$2a,$cb,$d7,$c5,$2a,$cb,$f3),
    ($26,$35,$44,$80,$26,$35,$44,$d7), ($b5,$62,$a3,$8f,$b5,$62,$a3,$fb),
    ($de,$b1,$5a,$49,$de,$b1,$5a,$7c), ($25,$ba,$1b,$67,$25,$ba,$1b,$e3),
    ($45,$ea,$0e,$98,$45,$ea,$0e,$39), ($5d,$fe,$c0,$e1,$5d,$fe,$c0,$82),
    ($c3,$2f,$75,$02,$c3,$2f,$75,$9b), ($81,$4c,$f0,$12,$81,$4c,$f0,$2f),
    ($8d,$46,$97,$a3,$8d,$46,$97,$ff), ($6b,$d3,$f9,$c6,$6b,$d3,$f9,$87),
    ($03,$8f,$5f,$e7,$03,$8f,$5f,$34), ($15,$92,$9c,$95,$15,$92,$9c,$8e),
    ($bf,$6d,$7a,$eb,$bf,$6d,$7a,$43), ($95,$52,$59,$da,$95,$52,$59,$44),
    ($d4,$be,$83,$2d,$d4,$be,$83,$c4), ($58,$74,$21,$d3,$58,$74,$21,$de),
    ($49,$e0,$69,$29,$49,$e0,$69,$e9), ($8e,$c9,$c8,$44,$8e,$c9,$c8,$cb),
    ($75,$c2,$89,$6a,$75,$c2,$89,$54), ($f4,$8e,$79,$78,$f4,$8e,$79,$7b),
    ($99,$58,$3e,$6b,$99,$58,$3e,$94), ($27,$b9,$71,$dd,$27,$b9,$71,$32),
    ($be,$e1,$4f,$b6,$be,$e1,$4f,$a6), ($f0,$88,$ad,$17,$f0,$88,$ad,$c2),
    ($c9,$20,$ac,$66,$c9,$20,$ac,$23), ($7d,$ce,$3a,$b4,$7d,$ce,$3a,$3d),
    ($63,$df,$4a,$18,$63,$df,$4a,$ee), ($e5,$1a,$31,$82,$e5,$1a,$31,$4c),
    ($97,$51,$33,$60,$97,$51,$33,$95), ($62,$53,$7f,$45,$62,$53,$7f,$0b),
    ($b1,$64,$77,$e0,$b1,$64,$77,$42), ($bb,$6b,$ae,$84,$bb,$6b,$ae,$fa),
    ($fe,$81,$a0,$1c,$fe,$81,$a0,$c3), ($f9,$08,$2b,$94,$f9,$08,$2b,$4e),
    ($70,$48,$68,$58,$70,$48,$68,$08), ($8f,$45,$fd,$19,$8f,$45,$fd,$2e),
    ($94,$de,$6c,$87,$94,$de,$6c,$a1), ($52,$7b,$f8,$b7,$52,$7b,$f8,$66),
    ($ab,$73,$d3,$23,$ab,$73,$d3,$28), ($72,$4b,$02,$e2,$72,$4b,$02,$d9),
    ($e3,$1f,$8f,$57,$e3,$1f,$8f,$24), ($66,$55,$ab,$2a,$66,$55,$ab,$b2),
    ($b2,$eb,$28,$07,$b2,$eb,$28,$76), ($2f,$b5,$c2,$03,$2f,$b5,$c2,$5b),
    ($86,$c5,$7b,$9a,$86,$c5,$7b,$a2), ($d3,$37,$08,$a5,$d3,$37,$08,$49),
    ($30,$28,$87,$f2,$30,$28,$87,$6d), ($23,$bf,$a5,$b2,$23,$bf,$a5,$8b),
    ($02,$03,$6a,$ba,$02,$03,$6a,$d1), ($ed,$16,$82,$5c,$ed,$16,$82,$25),
    ($8a,$cf,$1c,$2b,$8a,$cf,$1c,$72), ($a7,$79,$b4,$92,$a7,$79,$b4,$f8),
    ($f3,$07,$f2,$f0,$f3,$07,$f2,$f6), ($4e,$69,$e2,$a1,$4e,$69,$e2,$64),
    ($65,$da,$f4,$cd,$65,$da,$f4,$86), ($06,$05,$be,$d5,$06,$05,$be,$68),
    ($d1,$34,$62,$1f,$d1,$34,$62,$98), ($c4,$a6,$fe,$8a,$c4,$a6,$fe,$16),
    ($34,$2e,$53,$9d,$34,$2e,$53,$d4), ($a2,$f3,$55,$a0,$a2,$f3,$55,$a4),
    ($05,$8a,$e1,$32,$05,$8a,$e1,$5c), ($a4,$f6,$eb,$75,$a4,$f6,$eb,$cc),
    ($0b,$83,$ec,$39,$0b,$83,$ec,$5d), ($40,$60,$ef,$aa,$40,$60,$ef,$65),
    ($5e,$71,$9f,$06,$5e,$71,$9f,$b6), ($bd,$6e,$10,$51,$bd,$6e,$10,$92),
    ($3e,$21,$8a,$f9,$3e,$21,$8a,$6c), ($96,$dd,$06,$3d,$96,$dd,$06,$70),
    ($dd,$3e,$05,$ae,$dd,$3e,$05,$48), ($4d,$e6,$bd,$46,$4d,$e6,$bd,$50),
    ($91,$54,$8d,$b5,$91,$54,$8d,$fd), ($71,$c4,$5d,$05,$71,$c4,$5d,$ed),
    ($04,$06,$d4,$6f,$04,$06,$d4,$b9), ($60,$50,$15,$ff,$60,$50,$15,$da),
    ($19,$98,$fb,$24,$19,$98,$fb,$5e), ($d6,$bd,$e9,$97,$d6,$bd,$e9,$15),
    ($89,$40,$43,$cc,$89,$40,$43,$46), ($67,$d9,$9e,$77,$67,$d9,$9e,$57),
    ($b0,$e8,$42,$bd,$b0,$e8,$42,$a7), ($07,$89,$8b,$88,$07,$89,$8b,$8d),
    ($e7,$19,$5b,$38,$e7,$19,$5b,$9d), ($79,$c8,$ee,$db,$79,$c8,$ee,$84),
    ($a1,$7c,$0a,$47,$a1,$7c,$0a,$90), ($7c,$42,$0f,$e9,$7c,$42,$0f,$d8),
    ($f8,$84,$1e,$c9,$f8,$84,$1e,$ab), ($00,$00,$00,$00,$00,$00,$00,$00),
    ($09,$80,$86,$83,$09,$80,$86,$8c), ($32,$2b,$ed,$48,$32,$2b,$ed,$bc),
    ($1e,$11,$70,$ac,$1e,$11,$70,$d3), ($6c,$5a,$72,$4e,$6c,$5a,$72,$0a),
    ($fd,$0e,$ff,$fb,$fd,$0e,$ff,$f7), ($0f,$85,$38,$56,$0f,$85,$38,$e4),
    ($3d,$ae,$d5,$1e,$3d,$ae,$d5,$58), ($36,$2d,$39,$27,$36,$2d,$39,$05),
    ($0a,$0f,$d9,$64,$0a,$0f,$d9,$b8), ($68,$5c,$a6,$21,$68,$5c,$a6,$b3),
    ($9b,$5b,$54,$d1,$9b,$5b,$54,$45), ($24,$36,$2e,$3a,$24,$36,$2e,$06),
    ($0c,$0a,$67,$b1,$0c,$0a,$67,$d0), ($93,$57,$e7,$0f,$93,$57,$e7,$2c),
    ($b4,$ee,$96,$d2,$b4,$ee,$96,$1e), ($1b,$9b,$91,$9e,$1b,$9b,$91,$8f),
    ($80,$c0,$c5,$4f,$80,$c0,$c5,$ca), ($61,$dc,$20,$a2,$61,$dc,$20,$3f),
    ($5a,$77,$4b,$69,$5a,$77,$4b,$0f), ($1c,$12,$1a,$16,$1c,$12,$1a,$02),
    ($e2,$93,$ba,$0a,$e2,$93,$ba,$c1), ($c0,$a0,$2a,$e5,$c0,$a0,$2a,$af),
    ($3c,$22,$e0,$43,$3c,$22,$e0,$bd), ($12,$1b,$17,$1d,$12,$1b,$17,$03),
    ($0e,$09,$0d,$0b,$0e,$09,$0d,$01), ($f2,$8b,$c7,$ad,$f2,$8b,$c7,$13),
    ($2d,$b6,$a8,$b9,$2d,$b6,$a8,$8a), ($14,$1e,$a9,$c8,$14,$1e,$a9,$6b),
    ($57,$f1,$19,$85,$57,$f1,$19,$3a), ($af,$75,$07,$4c,$af,$75,$07,$91),
    ($ee,$99,$dd,$bb,$ee,$99,$dd,$11), ($a3,$7f,$60,$fd,$a3,$7f,$60,$41),
    ($f7,$01,$26,$9f,$f7,$01,$26,$4f), ($5c,$72,$f5,$bc,$5c,$72,$f5,$67),
    ($44,$66,$3b,$c5,$44,$66,$3b,$dc), ($5b,$fb,$7e,$34,$5b,$fb,$7e,$ea),
    ($8b,$43,$29,$76,$8b,$43,$29,$97), ($cb,$23,$c6,$dc,$cb,$23,$c6,$f2),
    ($b6,$ed,$fc,$68,$b6,$ed,$fc,$cf), ($b8,$e4,$f1,$63,$b8,$e4,$f1,$ce),
    ($d7,$31,$dc,$ca,$d7,$31,$dc,$f0), ($42,$63,$85,$10,$42,$63,$85,$b4),
    ($13,$97,$22,$40,$13,$97,$22,$e6), ($84,$c6,$11,$20,$84,$c6,$11,$73),
    ($85,$4a,$24,$7d,$85,$4a,$24,$96), ($d2,$bb,$3d,$f8,$d2,$bb,$3d,$ac),
    ($ae,$f9,$32,$11,$ae,$f9,$32,$74), ($c7,$29,$a1,$6d,$c7,$29,$a1,$22),
    ($1d,$9e,$2f,$4b,$1d,$9e,$2f,$e7), ($dc,$b2,$30,$f3,$dc,$b2,$30,$ad),
    ($0d,$86,$52,$ec,$0d,$86,$52,$35), ($77,$c1,$e3,$d0,$77,$c1,$e3,$85),
    ($2b,$b3,$16,$6c,$2b,$b3,$16,$e2), ($a9,$70,$b9,$99,$a9,$70,$b9,$f9),
    ($11,$94,$48,$fa,$11,$94,$48,$37), ($47,$e9,$64,$22,$47,$e9,$64,$e8),
    ($a8,$fc,$8c,$c4,$a8,$fc,$8c,$1c), ($a0,$f0,$3f,$1a,$a0,$f0,$3f,$75),
    ($56,$7d,$2c,$d8,$56,$7d,$2c,$df), ($22,$33,$90,$ef,$22,$33,$90,$6e),
    ($87,$49,$4e,$c7,$87,$49,$4e,$47), ($d9,$38,$d1,$c1,$d9,$38,$d1,$f1),
    ($8c,$ca,$a2,$fe,$8c,$ca,$a2,$1a), ($98,$d4,$0b,$36,$98,$d4,$0b,$71),
    ($a6,$f5,$81,$cf,$a6,$f5,$81,$1d), ($a5,$7a,$de,$28,$a5,$7a,$de,$29),
    ($da,$b7,$8e,$26,$da,$b7,$8e,$c5), ($3f,$ad,$bf,$a4,$3f,$ad,$bf,$89),
    ($2c,$3a,$9d,$e4,$2c,$3a,$9d,$6f), ($50,$78,$92,$0d,$50,$78,$92,$b7),
    ($6a,$5f,$cc,$9b,$6a,$5f,$cc,$62), ($54,$7e,$46,$62,$54,$7e,$46,$0e),
    ($f6,$8d,$13,$c2,$f6,$8d,$13,$aa), ($90,$d8,$b8,$e8,$90,$d8,$b8,$18),
    ($2e,$39,$f7,$5e,$2e,$39,$f7,$be), ($82,$c3,$af,$f5,$82,$c3,$af,$1b),
    ($9f,$5d,$80,$be,$9f,$5d,$80,$fc), ($69,$d0,$93,$7c,$69,$d0,$93,$56),
    ($6f,$d5,$2d,$a9,$6f,$d5,$2d,$3e), ($cf,$25,$12,$b3,$cf,$25,$12,$4b),
    ($c8,$ac,$99,$3b,$c8,$ac,$99,$c6), ($10,$18,$7d,$a7,$10,$18,$7d,$d2),
    ($e8,$9c,$63,$6e,$e8,$9c,$63,$79), ($db,$3b,$bb,$7b,$db,$3b,$bb,$20),
    ($cd,$26,$78,$09,$cd,$26,$78,$9a), ($6e,$59,$18,$f4,$6e,$59,$18,$db),
    ($ec,$9a,$b7,$01,$ec,$9a,$b7,$c0), ($83,$4f,$9a,$a8,$83,$4f,$9a,$fe),
    ($e6,$95,$6e,$65,$e6,$95,$6e,$78), ($aa,$ff,$e6,$7e,$aa,$ff,$e6,$cd),
    ($21,$bc,$cf,$08,$21,$bc,$cf,$5a), ($ef,$15,$e8,$e6,$ef,$15,$e8,$f4),
    ($ba,$e7,$9b,$d9,$ba,$e7,$9b,$1f), ($4a,$6f,$36,$ce,$4a,$6f,$36,$dd),
    ($ea,$9f,$09,$d4,$ea,$9f,$09,$a8), ($29,$b0,$7c,$d6,$29,$b0,$7c,$33),
    ($31,$a4,$b2,$af,$31,$a4,$b2,$88), ($2a,$3f,$23,$31,$2a,$3f,$23,$07),
    ($c6,$a5,$94,$30,$c6,$a5,$94,$c7), ($35,$a2,$66,$c0,$35,$a2,$66,$31),
    ($74,$4e,$bc,$37,$74,$4e,$bc,$b1), ($fc,$82,$ca,$a6,$fc,$82,$ca,$12),
    ($e0,$90,$d0,$b0,$e0,$90,$d0,$10), ($33,$a7,$d8,$15,$33,$a7,$d8,$59),
    ($f1,$04,$98,$4a,$f1,$04,$98,$27), ($41,$ec,$da,$f7,$41,$ec,$da,$80),
    ($7f,$cd,$50,$0e,$7f,$cd,$50,$ec), ($17,$91,$f6,$2f,$17,$91,$f6,$5f),
    ($76,$4d,$d6,$8d,$76,$4d,$d6,$60), ($43,$ef,$b0,$4d,$43,$ef,$b0,$51),
    ($cc,$aa,$4d,$54,$cc,$aa,$4d,$7f), ($e4,$96,$04,$df,$e4,$96,$04,$a9),
    ($9e,$d1,$b5,$e3,$9e,$d1,$b5,$19), ($4c,$6a,$88,$1b,$4c,$6a,$88,$b5),
    ($c1,$2c,$1f,$b8,$c1,$2c,$1f,$4a), ($46,$65,$51,$7f,$46,$65,$51,$0d),
    ($9d,$5e,$ea,$04,$9d,$5e,$ea,$2d), ($01,$8c,$35,$5d,$01,$8c,$35,$e5),
    ($fa,$87,$74,$73,$fa,$87,$74,$7a), ($fb,$0b,$41,$2e,$fb,$0b,$41,$9f),
    ($b3,$67,$1d,$5a,$b3,$67,$1d,$93), ($92,$db,$d2,$52,$92,$db,$d2,$c9),
    ($e9,$10,$56,$33,$e9,$10,$56,$9c), ($6d,$d6,$47,$13,$6d,$d6,$47,$ef),
    ($9a,$d7,$61,$8c,$9a,$d7,$61,$a0), ($37,$a1,$0c,$7a,$37,$a1,$0c,$e0),
    ($59,$f8,$14,$8e,$59,$f8,$14,$3b), ($eb,$13,$3c,$89,$eb,$13,$3c,$4d),
    ($ce,$a9,$27,$ee,$ce,$a9,$27,$ae), ($b7,$61,$c9,$35,$b7,$61,$c9,$2a),
    ($e1,$1c,$e5,$ed,$e1,$1c,$e5,$f5), ($7a,$47,$b1,$3c,$7a,$47,$b1,$b0),
    ($9c,$d2,$df,$59,$9c,$d2,$df,$c8), ($55,$f2,$73,$3f,$55,$f2,$73,$eb),
    ($18,$14,$ce,$79,$18,$14,$ce,$bb), ($73,$c7,$37,$bf,$73,$c7,$37,$3c),
    ($53,$f7,$cd,$ea,$53,$f7,$cd,$83), ($5f,$fd,$aa,$5b,$5f,$fd,$aa,$53),
    ($df,$3d,$6f,$14,$df,$3d,$6f,$99), ($78,$44,$db,$86,$78,$44,$db,$61),
    ($ca,$af,$f3,$81,$ca,$af,$f3,$17), ($b9,$68,$c4,$3e,$b9,$68,$c4,$2b),
    ($38,$24,$34,$2c,$38,$24,$34,$04), ($c2,$a3,$40,$5f,$c2,$a3,$40,$7e),
    ($16,$1d,$c3,$72,$16,$1d,$c3,$ba), ($bc,$e2,$25,$0c,$bc,$e2,$25,$77),
    ($28,$3c,$49,$8b,$28,$3c,$49,$d6), ($ff,$0d,$95,$41,$ff,$0d,$95,$26),
    ($39,$a8,$01,$71,$39,$a8,$01,$e1), ($08,$0c,$b3,$de,$08,$0c,$b3,$69),
    ($d8,$b4,$e4,$9c,$d8,$b4,$e4,$14), ($64,$56,$c1,$90,$64,$56,$c1,$63),
    ($7b,$cb,$84,$61,$7b,$cb,$84,$55), ($d5,$32,$b6,$70,$d5,$32,$b6,$21),
    ($48,$6c,$5c,$74,$48,$6c,$5c,$0c), ($d0,$b8,$57,$42,$d0,$b8,$57,$7d)
  );

{-------------------------------------------------------------------------------}
type
  TWA4 = packed array[0 .. 3] of Cardinal;     // AES block as array of Cardinal
  TBA4 = packed array[0 .. 3] of Byte;         // AES "word" as array of Byte
  TAWk = packed array[0 .. 4 * (C_FD_AESMaxRounds + 1) - 1] of Cardinal; // Key as array of Cardinal
  PWA4 = ^TWA4;
  PAWk = ^TAWk;
  DWord = Cardinal;
  PDWord= ^DWord;

{-------------------------------------------------------------------------------}
function ByteSwap(aValue: LongWord): LongWord; inline;
begin
  Result := (aValue shl 24) + ((aValue shl 8) and $FF0000) + ((aValue shr 8) and $FF00) + (aValue shr 24);
end;

{-------------------------------------------------------------------------------}
function XorAESBlock(const aSrc1, aSrc2: TAESBlock): TAESBlock; inline;
begin
  PWA4(@Result)[0] := PWA4(@aSrc1)[0] xor PWA4(@aSrc2)[0];
  PWA4(@Result)[1] := PWA4(@aSrc1)[1] xor PWA4(@aSrc2)[1];
  PWA4(@Result)[2] := PWA4(@aSrc1)[2] xor PWA4(@aSrc2)[2];
  PWA4(@Result)[3] := PWA4(@aSrc1)[3] xor PWA4(@aSrc2)[3];
end;

{-------------------------------------------------------------------------------}
{ TFDBlockAESCryptor                                                            }
{-------------------------------------------------------------------------------}
procedure TFDBlockAESCryptor.Decrypt(const BI: TAESBlock; var BO: TAESBlock);
type
  pBlock = ^tBlock;
  tBlock = array [0..3] of LongWord;

  function bval(aX: LongWord; aC: Integer): Byte; inline;
  begin
    Result := (aX shr (8*aC)) {$IFOPT R+} and $FF {$ENDIF};
  end;

  procedure Round(var aY0,
                      aY1,
                      aY2,
                      aY3: LongWord;
                      aX0,
                      aX1,
                      aX2,
                      aX3: LongWord;
                const aK : TRoundKey); inline;
  begin
    aY0 := aK[0] xor PLongWord(@IBoxAES[bval(aX0,0),0])^ xor
                     PLongWord(@IBoxAES[bval(aX3,1),3])^ xor
                     PLongWord(@IBoxAES[bval(aX2,2),2])^ xor
                     PLongWord(@IBoxAES[bval(aX1,3),1])^;
    aY1 := aK[1] xor PLongWord(@IBoxAES[bval(aX1,0),0])^ xor
                     PLongWord(@IBoxAES[bval(aX0,1),3])^ xor
                     PLongWord(@IBoxAES[bval(aX3,2),2])^ xor
                     PLongWord(@IBoxAES[bval(aX2,3),1])^;
    aY2 := aK[2] xor PLongWord(@IBoxAES[bval(aX2,0),0])^ xor
                     PLongWord(@IBoxAES[bval(aX1,1),3])^ xor
                     PLongWord(@IBoxAES[bval(aX0,2),2])^ xor
                     PLongWord(@IBoxAES[bval(aX3,3),1])^;
    aY3 := aK[3] xor PLongWord(@IBoxAES[bval(aX3,0),0])^ xor
                     PLongWord(@IBoxAES[bval(aX2,1),3])^ xor
                     PLongWord(@IBoxAES[bval(aX1,2),2])^ xor
                     PLongWord(@IBoxAES[bval(aX0,3),1])^;
  end;

  procedure LastRound(var aY: tBlock;
                          aX0,
                          aX1,
                          aX2,
                          aX3: LongWord;
                    const aK : TRoundKey); inline;
  begin
    aY[0] := aK[0] xor IBoxAES[bval(aX0,0),7] xor
                       IBoxAES[bval(aX3,1),7] shl 8 xor
                       IBoxAES[bval(aX2,2),7] shl 16 xor
                       IBoxAES[bval(aX1,3),7] shl 24;
    aY[1] := aK[1] xor IBoxAES[bval(aX1,0),7] xor
                       IBoxAES[bval(aX0,1),7] shl 8 xor
                       IBoxAES[bval(aX3,2),7] shl 16 xor
                       IBoxAES[bval(aX2,3),7] shl 24;
    aY[2] := aK[2] xor IBoxAES[bval(aX2,0),7] xor
                       IBoxAES[bval(aX1,1),7] shl 8 xor
                       IBoxAES[bval(aX0,2),7] shl 16 xor
                       IBoxAES[bval(aX3,3),7] shl 24;
    aY[3] := aK[3] xor IBoxAES[bval(aX3,0),7] xor
                       IBoxAES[bval(aX2,1),7] shl 8 xor
                       IBoxAES[bval(aX1,2),7] shl 16 xor
                       IBoxAES[bval(aX0,3),7] shl 24;
  end;

var
  lB0, lB1, lB2, lB3: LongWord;
  lT0, lT1, lT2, lT3: LongWord;
  I: Integer;
  lRKIdx: Integer;
begin
  lB0 := pBlock(@BI)^[0] xor FRk[0,0];
  lB1 := pBlock(@BI)^[1] xor FRk[0,1];
  lB2 := pBlock(@BI)^[2] xor FRk[0,2];
  lB3 := pBlock(@BI)^[3] xor FRk[0,3];
  lRKIdx := 1;
  for I := 1 to (FRounds div 2) - 1 do begin
    Round(lT0, lT1, lT2, lT3, lB0, lB1, lB2, lB3, FRk[lRKIdx]); Inc(lRKIdx);
    Round(lB0, lB1, lB2, lB3, lT0, lT1, lT2, lT3, FRk[lRKIdx]); Inc(lRKIdx);
  end;
  Round(lT0, lT1, lT2, lT3, lB0, lB1, lB2, lB3, FRk[lRKIdx]); Inc(lRKIdx);
  LastRound(pBlock(@BO)^, lT0, lT1, lT2, lT3, FRk[lRKIdx]);
end;

{-------------------------------------------------------------------------------}
procedure TFDBlockAESCryptor.Decrypt(var B: TAESBlock);
begin
  Decrypt(B,B);
end;

{-------------------------------------------------------------------------------}
function TFDBlockAESCryptor.DecryptInit(const AKey; AKeySize: Cardinal): Boolean;

  function InvMixCol(a: LongWord): LongWord;
  begin
    Result := PDWord(@IBoxAES[SBoxAES[a and $FF,1],0])^ xor
              PDWord(@IBoxAES[SBoxAES[(a shr 8) and $FF,1],3])^ xor
              PDWord(@IBoxAES[SBoxAES[(a shr 16) and $FF,1],2])^ xor
              PDWord(@IBoxAES[SBoxAES[a shr 24,1],1])^;
  end;

  procedure AESDecryptKey128(var aRK: TKeyArray);
  var
    I, J: Integer;
    A, B: LongWord;
  begin
    for I := 0 to 3 do
      aRK[1,I] := InvMixCol(aRK[1,I]);
    for I := 2 to 9 do begin
      A := InvMixCol(aRK[I,0]);
      aRK[I,0] := A;
      for J := 1 to 3 do begin
        A := A xor aRK[I-1,J];
        aRK[I,J] := A;
      end;
    end;
    for I := 0 to 4 do begin
      for J := 0 to 3 do begin
        A := aRK[I,J];
        B := aRK[10-I,J];
        aRK[I,J] := B;
        aRK[10-I,J] := A;
      end;
    end;
  end;

  procedure AESDecryptKey192(var aRK: tKeyArray);
  type
    pKeyArray192 = ^tKeyArray192;
    tKeyArray192 = array[0..7,0..5] of LongWord;
  var
    lRK: pKeyArray192;
    I, J: Integer;
    A, B: LongWord;
  begin
    lRK := @aRK;
    for I := 4 to 5 do
      lRK[0,I] := InvMixCol(lRK[0,I]);
    for I := 0 to 5 do
      lRK[1,I] := InvMixCol(lRK[1,I]);
    for I := 2 to 7 do begin
      A := InvMixCol(lRK[I,0]);
      lRK[I,0] := A;
      for J := 1 to 5 do begin
        A := A xor lRK[I-1,J];
        lRK[I,J] := A;
      end;
    end;
    for I := 0 to 5 do begin
      for J := 0 to 3 do begin
        A := aRK[I,J];
        B := aRK[12-I,J];
        aRK[I,J] := B;
        aRK[12-I,J] := A;
      end;
    end;
  end;

  procedure AESDecryptKey256(var aRK: tKeyArray);
  type
    pKeyArray256 = ^tKeyArray256;
    tKeyArray256 = array[0..6,0..7] of LongWord;
  var
    lRK: pKeyArray256;
    I, J: Integer;
    A, B: LongWord;
  begin
    lRK := @aRK;
    for I := 4 to 7 do
      lRK[0,I] := InvMixCol(lRK[0,I]);
    for I := 0 to 7 do
      lRK[1,I] := InvMixCol(lRK[1,I]);
    for I := 2 to 6 do begin
      A := InvMixCol(lRK[I,0]);
      lRK[I,0] := A;
      for J := 1 to 3 do begin
        A := A xor lRK[I-1,J];
        lRK[I,J] := A;
      end;
      A := InvMixCol(lRK[I,4]);
      lRK[I,4] := A;
      for J := 5 to 7 do begin
        A := A xor lRK[I-1,J];
        lRK[I,J] := A;
      end;
    end;
    for I := 0 to 6 do begin
      for J := 0 to 3 do begin
        A := aRK[I,J];
        B := aRK[14-I,J];
        aRK[I,J] := B;
        aRK[14-I,J] := A;
      end;
    end;
  end;

begin
  Result := EncryptInit(AKey, AKeySize); // contains FInitialized := True
  if not Result then
    Exit;
  case AKeySize of
    128:
      AESDecryptKey128(FRK);
    192:
      AESDecryptKey192(FRK);
    256:
      AESDecryptKey256(FRK);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDBlockAESCryptor.Encrypt(const BI: TAESBlock; var BO: TAESBlock);
type
  pBlock = ^tBlock;
  tBlock = array [0..3] of LongWord;
  pRoundKey = ^tRoundKey;

  function bval(aX: LongWord; aC: Integer): Byte; inline;
  begin
    Result := (aX shr (8*aC)) {$IFOPT R+} and $FF {$ENDIF};
  end;

var
  lB0, lB1, lB2, lB3: LongWord;
  lT0, lT1, lT2, lT3: LongWord;
  lRK: pRoundKey;
  I: Integer;
begin
  lRK := @FRK;
  lB0 := pBlock(@BI)^[0] xor lRK[0];
  lB1 := pBlock(@BI)^[1] xor lRK[1];
  lB2 := pBlock(@BI)^[2] xor lRK[2];
  lB3 := pBlock(@BI)^[3] xor lRK[3];
  Inc(lRK);
  for I := 1 to (FRounds div 2)-1 do begin
    lT0 := PLongWord(@SBoxAES[bval(lB0,0),0])^ xor
           PLongWord(@SBoxAES[bval(lB1,1),3])^ xor
           PLongWord(@SBoxAES[bval(lB2,2),2])^ xor
           PLongWord(@SBoxAES[bval(lB3,3),1])^ xor
           lRK[0];
    lT1 := PLongWord(@SBoxAES[bval(lB1,0),0])^ xor
           PLongWord(@SBoxAES[bval(lB2,1),3])^ xor
           PLongWord(@SBoxAES[bval(lB3,2),2])^ xor
           PLongWord(@SBoxAES[bval(lB0,3),1])^ xor
           lRK[1];
    lT2 := PLongWord(@SBoxAES[bval(lB2,0),0])^ xor
           PLongWord(@SBoxAES[bval(lB3,1),3])^ xor
           PLongWord(@SBoxAES[bval(lB0,2),2])^ xor
           PLongWord(@SBoxAES[bval(lB1,3),1])^ xor
           lRK[2];
    lT3 := PLongWord(@SBoxAES[bval(lB3,0),0])^ xor
           PLongWord(@SBoxAES[bval(lB0,1),3])^ xor
           PLongWord(@SBoxAES[bval(lB1,2),2])^ xor
           PLongWord(@SBoxAES[bval(lB2,3),1])^ xor
           lRK[3];
    Inc(lRK);
    lB0 := PLongWord(@SBoxAES[bval(lT0,0),0])^ xor
           PLongWord(@SBoxAES[bval(lT1,1),3])^ xor
           PLongWord(@SBoxAES[bval(lT2,2),2])^ xor
           PLongWord(@SBoxAES[bval(lT3,3),1])^ xor
           lRK[0];
    lB1 := PLongWord(@SBoxAES[bval(lT1,0),0])^ xor
           PLongWord(@SBoxAES[bval(lT2,1),3])^ xor
           PLongWord(@SBoxAES[bval(lT3,2),2])^ xor
           PLongWord(@SBoxAES[bval(lT0,3),1])^ xor
           lRK[1];
    lB2 := PLongWord(@SBoxAES[bval(lT2,0),0])^ xor
           PLongWord(@SBoxAES[bval(lT3,1),3])^ xor
           PLongWord(@SBoxAES[bval(lT0,2),2])^ xor
           PLongWord(@SBoxAES[bval(lT1,3),1])^ xor
           lRK[2];
    lB3 := PLongWord(@SBoxAES[bval(lT3,0),0])^ xor
           PLongWord(@SBoxAES[bval(lT0,1),3])^ xor
           PLongWord(@SBoxAES[bval(lT1,2),2])^ xor
           PLongWord(@SBoxAES[bval(lT2,3),1])^ xor
           lRK[3];
    Inc(lRK);
  end;
  lT0 := PLongWord(@SBoxAES[bval(lB0,0),0])^ xor
         PLongWord(@SBoxAES[bval(lB1,1),3])^ xor
         PLongWord(@SBoxAES[bval(lB2,2),2])^ xor
         PLongWord(@SBoxAES[bval(lB3,3),1])^ xor
         lRK[0];
  lT1 := PLongWord(@SBoxAES[bval(lB1,0),0])^ xor
         PLongWord(@SBoxAES[bval(lB2,1),3])^ xor
         PLongWord(@SBoxAES[bval(lB3,2),2])^ xor
         PLongWord(@SBoxAES[bval(lB0,3),1])^ xor
         lRK[1];
  lT2 := PLongWord(@SBoxAES[bval(lB2,0),0])^ xor
         PLongWord(@SBoxAES[bval(lB3,1),3])^ xor
         PLongWord(@SBoxAES[bval(lB0,2),2])^ xor
         PLongWord(@SBoxAES[bval(lB1,3),1])^ xor
         lRK[2];
  lT3 := PLongWord(@SBoxAES[bval(lB3,0),0])^ xor
         PLongWord(@SBoxAES[bval(lB0,1),3])^ xor
         PLongWord(@SBoxAES[bval(lB1,2),2])^ xor
         PLongWord(@SBoxAES[bval(lB2,3),1])^ xor
         lRK[3];
  Inc(lRK);
  pBlock(@BO)^[0] := lRK[0] xor SBoxAES[bval(lT0,0),1] xor
                                SBoxAES[bval(lT1,1),1] shl 8 xor
                                SBoxAES[bval(lT2,2),1] shl 16 xor
                                SBoxAES[bval(lT3,3),1] shl 24;
  pBlock(@BO)^[1] := lRK[1] xor SBoxAES[bval(lT1,0),1] xor
                                SBoxAES[bval(lT2,1),1] shl 8 xor
                                SBoxAES[bval(lT3,2),1] shl 16 xor
                                SBoxAES[bval(lT0,3),1] shl 24;
  pBlock(@BO)^[2] := lRK[2] xor SBoxAES[bval(lT2,0),1] xor
                                SBoxAES[bval(lT3,1),1] shl 8 xor
                                SBoxAES[bval(lT0,2),1] shl 16 xor
                                SBoxAES[bval(lT1,3),1] shl 24;
  pBlock(@BO)^[3] := lRK[3] xor SBoxAES[bval(lT3,0),1] xor
                                SBoxAES[bval(lT0,1),1] shl 8 xor
                                SBoxAES[bval(lT1,2),1] shl 16 xor
                                SBoxAES[bval(lT2,3),1] shl 24;
end;

{-------------------------------------------------------------------------------}
procedure TFDBlockAESCryptor.Encrypt(var B: TAESBlock);
begin
  Encrypt(B,B);
end;

{-------------------------------------------------------------------------------}
function TFDBlockAESCryptor.EncryptInit(const AKey; AKeySize: Cardinal): Boolean;
  // AES key expansion, error if invalid key size

  procedure AESEncryptKey128(const aKey; var aRK; var aRKLen: LongWord);
  var
    lRK: PByteArray;
    I: Integer;
  begin
    Move(aKey, aRK, 16);
    lRK := @aRK;
    aRKLen := 10*16;
    for I := 1 to 10 do begin
      lRK[I*16+0] := lRK[I*16-16] xor SBoxAES[lRK[I*16-3],1] xor rcon[I-1];
      lRK[I*16+1] := lRK[I*16-15] xor SBoxAES[lRK[I*16-2],1];
      lRK[I*16+2] := lRK[I*16-14] xor SBoxAES[lRK[I*16-1],1];
      lRK[I*16+3] := lRK[I*16-13] xor SBoxAES[lRK[I*16-4],1];
      PDWord(@lRK[I*16+ 4])^ := PDWord(@lRK[I*16-12])^ xor PDWord(@lRK[I*16+0])^;
      PDWord(@lRK[I*16+ 8])^ := PDWord(@lRK[I*16- 8])^ xor PDWord(@lRK[I*16+4])^;
      PDWord(@lRK[I*16+12])^ := PDWord(@lRK[I*16- 4])^ xor PDWord(@lRK[I*16+8])^;
    end;
  end;

  procedure AESEncryptKey192(const aKey; var aRK; var aRKLen: LongWord);
  var
    lRK: PByteArray;
    I: Integer;
  begin
    Move(aKey, aRK, 24);
    lRK := @aRK;
    aRKLen := 12*16;
    for I := 1 to 7 do begin
      lRK[I*24+0] := lRK[I*24-24] xor SBoxAES[lRK[I*24-3],1] xor rcon[I-1];
      lRK[I*24+1] := lRK[I*24-23] xor SBoxAES[lRK[I*24-2],1];
      lRK[I*24+2] := lRK[I*24-22] xor SBoxAES[lRK[I*24-1],1];
      lRK[I*24+3] := lRK[I*24-21] xor SBoxAES[lRK[I*24-4],1];
      PDWord(@lRK[I*24+ 4])^ := PDWord(@lRK[I*24-20])^ xor PDWord(@lRK[I*24+ 0])^;
      PDWord(@lRK[I*24+ 8])^ := PDWord(@lRK[I*24-16])^ xor PDWord(@lRK[I*24+ 4])^;
      PDWord(@lRK[I*24+12])^ := PDWord(@lRK[I*24-12])^ xor PDWord(@lRK[I*24+ 8])^;
      PDWord(@lRK[I*24+16])^ := PDWord(@lRK[I*24- 8])^ xor PDWord(@lRK[I*24+12])^;
      PDWord(@lRK[I*24+20])^ := PDWord(@lRK[I*24- 4])^ xor PDWord(@lRK[I*24+16])^;
    end;
    I := 8;
    lRK[I*24+0] := lRK[I*24-24] xor SBoxAES[lRK[I*24-3],1] xor rcon[I-1];
    lRK[I*24+1] := lRK[I*24-23] xor SBoxAES[lRK[I*24-2],1];
    lRK[I*24+2] := lRK[I*24-22] xor SBoxAES[lRK[I*24-1],1];
    lRK[I*24+3] := lRK[I*24-21] xor SBoxAES[lRK[I*24-4],1];
    PDWord(@lRK[I*24+ 4])^ := PDWord(@lRK[I*24-20])^ xor PDWord(@lRK[I*24+ 0])^;
    PDWord(@lRK[I*24+ 8])^ := PDWord(@lRK[I*24-16])^ xor PDWord(@lRK[I*24+ 4])^;
    PDWord(@lRK[I*24+12])^ := PDWord(@lRK[I*24-12])^ xor PDWord(@lRK[I*24+ 8])^;
  end;

  procedure AESEncryptKey256(const aKey; var aRK; var aRKLen: LongWord);
  var
    lRK: PByteArray;
    I: Integer;
  begin
    Move(aKey, aRK, 32);
    lRK := @aRK;
    aRKLen := 14*16;
    for I := 1 to 6 do begin
      lRK[I*32+0] := lRK[I*32-32] xor SBoxAES[lRK[I*32-3],1] xor rcon[I-1];
      lRK[I*32+1] := lRK[I*32-31] xor SBoxAES[lRK[I*32-2],1];
      lRK[I*32+2] := lRK[I*32-30] xor SBoxAES[lRK[I*32-1],1];
      lRK[I*32+3] := lRK[I*32-29] xor SBoxAES[lRK[I*32-4],1];
      PDWord(@lRK[I*32+ 4])^ := PDWord(@lRK[I*32-28])^ xor PDWord(@lRK[I*32+0])^;
      PDWord(@lRK[I*32+ 8])^ := PDWord(@lRK[I*32-24])^ xor PDWord(@lRK[I*32+4])^;
      PDWord(@lRK[I*32+12])^ := PDWord(@lRK[I*32-20])^ xor PDWord(@lRK[I*32+8])^;
      lRK[I*32+16] := lRK[I*32-16] xor SBoxAES[lRK[I*32+12],1];
      lRK[I*32+17] := lRK[I*32-15] xor SBoxAES[lRK[I*32+13],1];
      lRK[I*32+18] := lRK[I*32-14] xor SBoxAES[lRK[I*32+14],1];
      lRK[I*32+19] := lRK[I*32-13] xor SBoxAES[lRK[I*32+15],1];
      PDWord(@lRK[I*32+20])^ := PDWord(@lRK[I*32-12])^ xor PDWord(@lRK[I*32+16])^;
      PDWord(@lRK[I*32+24])^ := PDWord(@lRK[I*32- 8])^ xor PDWord(@lRK[I*32+20])^;
      PDWord(@lRK[I*32+28])^ := PDWord(@lRK[I*32- 4])^ xor PDWord(@lRK[I*32+24])^;
    end;
    I := 7;
    lRK[I*32+0] := lRK[I*32-32] xor SBoxAES[lRK[I*32-3],1] xor rcon[I-1];
    lRK[I*32+1] := lRK[I*32-31] xor SBoxAES[lRK[I*32-2],1];
    lRK[I*32+2] := lRK[I*32-30] xor SBoxAES[lRK[I*32-1],1];
    lRK[I*32+3] := lRK[I*32-29] xor SBoxAES[lRK[I*32-4],1];
    PDWord(@lRK[I*32+ 4])^ := PDWord(@lRK[I*32-28])^ xor PDWord(@lRK[I*32+0])^;
    PDWord(@lRK[I*32+ 8])^ := PDWord(@lRK[I*32-24])^ xor PDWord(@lRK[I*32+4])^;
    PDWord(@lRK[I*32+12])^ := PDWord(@lRK[I*32-20])^ xor PDWord(@lRK[I*32+8])^;
  end;

var
  lRKLen: LongWord;
begin
  FInitialized := True;
  Result := True;
  lRKLen := 0;
  case AKeySize of
    128:
      AESEncryptKey128(aKey,FRK,lRKLen);
    192:
      AESEncryptKey192(aKey,FRK,lRKLen);
    256:
      AESEncryptKey256(aKey,FRK,lRKLen);
  else
    FInitialized := False;
    Result := False;
    Exit;
  end;
  FRounds := lRKLen div 16;
end;

{-------------------------------------------------------------------------------}
function TFDBlockAESCryptor.Initialize(const AKey; AKeySize: Cardinal; AEncrypt: Boolean): Boolean;
begin
  if AEncrypt then
    Result := EncryptInit(AKey, AKeySize)
  else
    Result := DecryptInit(AKey, AKeySize);
end;

{-------------------------------------------------------------------------------}
{ TFDCipher                                                                     }
{-------------------------------------------------------------------------------}
destructor TFDCipher.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

{-------------------------------------------------------------------------------}
class function TFDCipher.ReserveLength: Cardinal;
begin
  Result := 0;
end;

{-------------------------------------------------------------------------------}
{ TFDCipherAESBase                                                              }
{-------------------------------------------------------------------------------}
procedure TFDCipherAESBase.Finalize;
begin
  FDFreeAndNil(fEncryptor);
  FDFreeAndNil(fMac);
  FDFreeAndNil(fIV);
end;

{-------------------------------------------------------------------------------}
procedure TFDCipherAESBase.BCC(aCryptor: TFDBlockAESCryptor; aSrc: TMemoryStream;
  var aDst: TAESBlock);
var
  lBlock: TAESBlock;
begin
  aCryptor.Encrypt(aDst);
  aSrc.Position := 0;
  while aSrc.Read(lBlock, SizeOf(lBlock)) > 0 do begin
    lBlock := XorAESBlock(aDst, lBlock);
    aCryptor.Encrypt(TAESBlock(lBlock), aDst);
  end;
end;

{-------------------------------------------------------------------------------}
function TFDCipherAESBase.GenerateIV(out aIV: TAESBlock): Integer;
var
  lDT, lI: TAESBlock;
  lNow: TDateTime;
begin
  if fIV = nil then
    Result := 2
  else begin
    Result := 0;
    FillChar(lDT, SizeOf(lDT), 0);
    lNow := Now;
    Move(lNow, lDT, SizeOf(lNow));
    fIV.Encrypt(lDT, lI);
    fIVSeed := XorAESBlock(fIVSeed, lI);
    fIV.Encrypt(fIVSeed, aIV);
    lI := XorAESBlock(aIV, lI);
    fIV.Encrypt(lI, fIVSeed);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDCipherAESBase.KDF128(aKeyMaterial: PByte; aKeyLength: Integer;
  aLabel: Char; const aSalt: TAESBlock; var aKey: TAESKey128);
const
  cBCCKey: array [0..15] of Byte =
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
var
  lLen: LongWord;
  lMS: TMemoryStream;
  lC: TFDBlockAESCryptor;
  lB: TFDAnsiChar;
  lT: Byte;
  lTmp: array [0..1] of TAESBlock;
  I: Integer;
begin
  lMS := TMemoryStream.Create;
  try
    lC := TFDBlockAESCryptor.Create;
    try
      lLen := ByteSwap(1 + SizeOf(aSalt) + aKeyLength);
      lMS.Write(lLen, SizeOf(LongWord));
      lLen := ByteSwap(SizeOf(aKey));
      lMS.Write(lLen, SizeOf(LongWord));
      lB := TFDAnsiChar(aLabel);
      lMS.Write(lB, SizeOf(TFDAnsiChar));
      lMS.Write(aSalt, SizeOf(aSalt));
      lMS.Write(aKeyMaterial^, aKeyLength);
      lT := $80;
      lMS.Write(lT, SizeOf(Byte));
      lT := 0;
      while lMS.Size and $F <> 0 do
        lMS.Write(lT, SizeOf(Byte));
      lC.Initialize(cBCCKey, 128, True);
      FillChar(lTmp, SizeOf(lTmp), 0);
      for I := 0 to High(lTmp) do begin
        lTmp[I][15] := I;
        BCC(lC, lMS, lTmp[I]);
      end;
      lC.Initialize(lTmp[0], 128, True);
      lC.Encrypt(lTmp[1], PAESBlock(@aKey)^);
    finally
      FDFree(lC);
    end;
  finally
    FDFree(lMS);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDCipherAESBase.KDF192(aKeyMaterial: PByte; aKeyLength: Integer;
  aLabel: Char; const aSalt: TAESBlock; var aKey: TAESKey192);
const
  cBCCKey: array [0..23] of Byte =
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23);
var
  lLen: LongWord;
  lMS: TMemoryStream;
  lC: TFDBlockAESCryptor;
  lB: TFDAnsiChar;
  lT: Byte;
  lTmp: array [0..2] of TAESBlock;
  I: Integer;
begin
  lMS := TMemoryStream.Create;
  try
    lC := TFDBlockAESCryptor.Create;
    try
      lLen := ByteSwap(1 + SizeOf(aSalt) + aKeyLength);
      lMS.Write(lLen, SizeOf(LongWord));
      lLen := ByteSwap(SizeOf(aKey));
      lMS.Write(lLen, SizeOf(LongWord));
      lB := TFDAnsiChar(aLabel);
      lMS.Write(lB, SizeOf(TFDAnsiChar));
      lMS.Write(aSalt, SizeOf(aSalt));
      lMS.Write(aKeyMaterial^, aKeyLength);
      lT := $80;
      lMS.Write(lT, SizeOf(Byte));
      lT := 0;
      while lMS.Size and $F <> 0 do
        lMS.Write(lT, SizeOf(Byte));
      lC.Initialize(cBCCKey, 192, True);
      FillChar(lTmp, SizeOf(lTmp), 0);
      for I := 0 to High(lTmp) do begin
        lTmp[I][15] := I;
        BCC(lC, lMS, lTmp[I]);
      end;
      lC.Initialize(lTmp, 192, True);
      Move(lTmp[1][8], lTmp[2], 16);
      lC.Encrypt(lTmp[2], PAESBlock(@aKey)^);
      lC.Encrypt(lTmp[2]);
      Move(lTmp[1], aKey[16], 8);
    finally
      FDFree(lC);
    end;
  finally
    FDFree(lMS);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDCipherAESBase.KDF256(aKeyMaterial: PByte; aKeyLength: Integer;
  aLabel: Char; const aSalt: TAESBlock; var aKey: TAESKey256);
const
  cBCCKey: array [0..31] of Byte =
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
     16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31);
var
  lLen: LongWord;
  lMS: TMemoryStream;
  lC: TFDBlockAESCryptor;
  lB: TFDAnsiChar;
  lT: Byte;
  lTmp: array [0..2] of TAESBlock;
  I: Integer;
begin
  lMS := TMemoryStream.Create;
  try
    lC := TFDBlockAESCryptor.Create;
    try
      lLen := ByteSwap(1 + SizeOf(aSalt) + aKeyLength);
      lMS.Write(lLen, SizeOf(LongWord));
      lLen := ByteSwap(SizeOf(aKey));
      lMS.Write(lLen, SizeOf(LongWord));
      lB := TFDAnsiChar(aLabel);
      lMS.Write(lB, SizeOf(TFDAnsiChar));
      lMS.Write(aSalt,SizeOf(aSalt));
      lMS.Write(aKeyMaterial^, aKeyLength);
      lT := $80;
      lMS.Write(lT, SizeOf(Byte));
      lT := 0;
      while lMS.Size and $F <> 0 do
        lMS.Write(lT, SizeOf(Byte));
      lC.Initialize(cBCCKey, 256, True);
      FillChar(lTmp, SizeOf(lTmp), 0);
      for I := 0 to High(lTmp) do begin
        lTmp[I][15] := I;
        BCC(lC, lMS, lTmp[I]);
      end;
      lC.Initialize(lTmp[0], 256, True);
      lC.Encrypt(lTmp[2], PAESBlock(@aKey[0])^);
      lC.Encrypt(lTmp[2], PAESBlock(@aKey[16])^);
    finally
      FDFree(lC);
    end;
  finally
    FDFree(lMS);
  end;
end;

{-------------------------------------------------------------------------------}
{ TFDCipherAES                                                                  }
{-------------------------------------------------------------------------------}
class function TFDCipherAES.ReserveLength: Cardinal;
begin
  Result := 32;
end;

{-------------------------------------------------------------------------------}
procedure TFDCipherAES.Initialize(aKeyMaterial: PByte; aKeyLength: Integer;
  const aSalt: TAESBlock; aSaltLength: Integer; aKeySize: Word);
var
  lEKey, lMKey, lRKey:
    record
      case Word of
      128: (lKey128: TAESKey128);
      192: (lKey192: TAESKey192);
      256: (lKey256: TAESKey256);
    end;
begin
  Finalize;
  fEncryptor := TFDBlockAESCryptor.Create;
  fMac := TFDBlockAESCryptor.Create;
  fIV := TFDBlockAESCryptor.Create;
  case aKeySize of
  128:
    begin
      KDF128(aKeyMaterial, aKeyLength, 'E', aSalt, lEKey.lKey128);
      KDF128(aKeyMaterial, aKeyLength, 'A', aSalt, lMKey.lKey128);
      KDF128(aKeyMaterial, aKeyLength, 'R', aSalt, lRKey.lKey128);
    end;
  192:
    begin
      KDF192(aKeyMaterial, aKeyLength, 'E', aSalt, lEKey.lKey192);
      KDF192(aKeyMaterial, aKeyLength, 'A', aSalt, lMKey.lKey192);
      KDF192(aKeyMaterial, aKeyLength, 'R', aSalt, lRKey.lKey192);
    end;
  256:
    begin
      KDF256(aKeyMaterial, aKeyLength, 'E', aSalt, lEKey.lKey256);
      KDF256(aKeyMaterial, aKeyLength, 'A', aSalt, lMKey.lKey256);
      KDF256(aKeyMaterial, aKeyLength, 'R', aSalt, lRKey.lKey256);
    end;
  else
    Finalize;
  end;
  if fEncryptor <> nil then
    fEncryptor.Initialize(lEKey, aKeySize, True);
  if fMac <> nil then
    fMac.Initialize(lMKey, aKeySize, True);
  if fIV <> nil then
    fIV.Initialize(lMKey, aKeySize, True);
end;

{-------------------------------------------------------------------------------}
function TFDCipherAES.Process(ApIn, ApOut: PAESBlock; ACount: Integer;
  APageNum: Cardinal; AEncrypt: Boolean): Integer;
var
  lIV, lMac, lCtr, lCtrPad: TAESBlock;
  I: Integer;
  lCtr32: LongWord;

  procedure InitState;
  var
    lPageNum, lCtr32: LongWord;
  begin
    lPageNum := ByteSwap(APageNum);
    Move(lPageNum, lCtr[0], 4);
    Move(lIV[0], lCtr[4], 8);
    FillChar(lCtr[12], 4, 0);
    Move(lPageNum, lMac[0], 4);
    Move(lIV[8], lMac[4], 8);
    lCtr32 := ByteSwap(ACount);
    Move(lCtr32, lMac[12], 4);
    fMac.Encrypt(lMac);
  end;

begin
  if (fEncryptor = nil) or (fMac = nil) or (fIV = nil) then
    Result := 2
  else if AEncrypt then begin
    Result := GenerateIV(lIV);
    if Result = 0 then begin
      InitState;
      Move(lIV, ApOut^, SizeOf(lIV));
      lMac := XorAESBlock(lMac, lIV);
      fMac.Encrypt(lMac);
      Inc(ApOut);
      for I := 0 to ACount - 3 do begin
        lCtr32 := ByteSwap(Cardinal(I));
        Move(lCtr32, lCtr[12], 4);
        fEncryptor.Encrypt(lCtr, lCtrPad);
        ApOut^ := XorAESBlock(ApIn^, lCtrPad);
        lMac := XorAESBlock(lMac, ApOut^);
        Inc(ApIn);
        Inc(ApOut);
        fMac.Encrypt(lMac);
      end;
      Move(lMac, ApOut^, SizeOf(lMac));
    end;
  end
  else begin
    Move(ApIn^, lIV, SizeOf(lIV));
    InitState;
    lMac := XorAESBlock(lMac, lIV);
    fMac.Encrypt(lMac);
    Inc(ApIn);
    for I := 0 to ACount - 3 do begin
      lCtr32 := ByteSwap(Cardinal(I));
      Move(lCtr32, lCtr[12], 4);
      fEncryptor.Encrypt(lCtr, lCtrPad);
      lMac := XorAESBlock(lMac, ApIn^);
      ApOut^ := XorAESBlock(ApIn^, lCtrPad);
      Inc(ApIn);
      Inc(ApOut);
      fMac.Encrypt(lMac);
    end;
    if CompareMem(ApIn, @lMac, SizeOf(lMac)) then
      Result := 0
    else
      Result := 1;
    FillChar(ApOut^, 16, 0);
    Inc(ApOut);
    FillChar(ApOut^, 16, 0);
  end;
end;

{-------------------------------------------------------------------------------}
{ TFDCipherAESCTR                                                               }
{-------------------------------------------------------------------------------}
class function TFDCipherAESCTR.ReserveLength: Cardinal;
begin
  Result := 16;
end;

{-------------------------------------------------------------------------------}
procedure TFDCipherAESCTR.Initialize(aKeyMaterial: PByte; aKeyLength: Integer;
  const aSalt: TAESBlock; aSaltLength: Integer; aKeySize: Word);
var
  lEKey, lRKey:
    record
      case Word of
      128: (lKey128: TAESKey128);
      192: (lKey192: TAESKey192);
      256: (lKey256: TAESKey256);
    end;
begin
  Finalize;
  fEncryptor := TFDBlockAESCryptor.Create;
  fIV := TFDBlockAESCryptor.Create;
  case aKeySize of
  128:
    begin
      KDF128(aKeyMaterial, aKeyLength, 'E', aSalt, lEKey.lKey128);
      KDF128(aKeyMaterial, aKeyLength, 'R', aSalt, lRKey.lKey128);
    end;
  192:
    begin
      KDF192(aKeyMaterial, aKeyLength, 'E', aSalt, lEKey.lKey192);
      KDF192(aKeyMaterial, aKeyLength, 'R', aSalt, lRKey.lKey192);
    end;
  256:
    begin
      KDF256(aKeyMaterial, aKeyLength, 'E', aSalt, lEKey.lKey256);
      KDF256(aKeyMaterial, aKeyLength, 'R', aSalt, lRKey.lKey256);
    end;
  else
    Finalize;
  end;
  if fEncryptor <> nil then
    fEncryptor.Initialize(lEKey, aKeySize, True);
  if fIV <> nil then
    fIV.Initialize(lRKey, aKeySize, True);
end;

{-------------------------------------------------------------------------------}
function TFDCipherAESCTR.Process(ApIn, ApOut: PAESBlock; ACount: Integer;
  APageNum: Cardinal; AEncrypt: Boolean): Integer;
var
  lCtr, lCtrPad: TAESBlock;
  I: Integer;
  lCtr32: LongWord;
begin
  if (fEncryptor = nil) or (fIV = nil) then
    Result := 2
  else if AEncrypt then begin
    Result := GenerateIV(lCtr);
    if Result = 0 then begin
      Move(lCtr, ApOut^, SizeOf(lCtr));
      Inc(ApOut);
      for I := 0 to ACount - 2 do begin
        lCtr32 := ByteSwap(Cardinal(I));
        Move(lCtr32, lCtr[12], 4);
        fEncryptor.Encrypt(lCtr, lCtrPad);
        ApOut^ := XorAESBlock(ApIn^, lCtrPad);
        Inc(ApIn);
        Inc(ApOut);
      end;
    end;
  end
  else begin
    Move(ApIn^,lCtr,SizeOf(lCtr));
    Inc(ApIn);
    for I := 0 to ACount - 2 do begin
      lCtr32 := ByteSwap(Cardinal(I));
      Move(lCtr32, lCtr[12], 4);
      fEncryptor.Encrypt(lCtr, lCtrPad);
      ApOut^ := XorAESBlock(ApIn^, lCtrPad);
      Inc(ApIn);
      Inc(ApOut);
    end;
    FillChar(ApOut^, 16, 0);
    Result := 3;
  end;
end;

{-------------------------------------------------------------------------------}
{ TFDCipherAESECB                                                               }
{-------------------------------------------------------------------------------}
class function TFDCipherAESECB.ReserveLength: Cardinal;
begin
  Result := 0;
end;

{-------------------------------------------------------------------------------}
procedure TFDCipherAESECB.Finalize;
begin
  FDFreeAndNil(FEncryptor);
  FDFreeAndNil(FDecryptor);
end;

{-------------------------------------------------------------------------------}
procedure TFDCipherAESECB.Initialize(aKeyMaterial: PByte; aKeyLength: Integer;
  const aSalt: TAESBlock; aSaltLength: Integer; aKeySize: Word);
var
  lEKey: TAESKey128;
  lMS: TMemoryStream;
  lH: TFDMD5Hash;
  lEKey192: TAESKey192;
  lEKey256: TAESKey256;
begin
  Finalize;
  lMS := TMemoryStream.Create;
  try
    lMS.Write(aKeyMaterial^, aKeyLength);
    lMS.Write(aSalt, SizeOf(aSalt));
    lH := TFDMD5Hash.Create;
    try
      lEKey := TAESKey128(lH.Hash(lMS));
    finally
      FDFree(lH);
    end;
  finally
    FDFree(lMS);
  end;
  fEncryptor := TFDBlockAESCryptor.Create;
  fDecryptor := TFDBlockAESCryptor.Create;
  case aKeySize of
  128:
    begin
      fEncryptor.Initialize(lEKey, aKeySize, True);
      fDecryptor.Initialize(lEKey, aKeySize, False);
    end;
  192:
    begin
      Move(lEKey, lEKey192, 16);
      Move(lEKey, lEKey192[16], 8);
      fEncryptor.Initialize(lEKey192, aKeySize, True);
      fDecryptor.Initialize(lEKey192, aKeySize, False);
    end;
  256:
    begin
      Move(lEKey, lEKey256, 16);
      Move(lEKey, lEKey256[16], 16);
      fEncryptor.Initialize(lEKey256, aKeySize, True);
      fDecryptor.Initialize(lEKey256, aKeySize, False);
    end;
  else
    Finalize;
  end;
end;

{-------------------------------------------------------------------------------}
function TFDCipherAESECB.Process(ApIn, ApOut: PAESBlock; ACount: Integer;
  APageNum: Cardinal; AEncrypt: Boolean): Integer;
var
  i: Integer;
begin
  if AEncrypt and (fEncryptor = nil) or
     not AEncrypt and (fDecryptor = nil) then
    Result := 2
  else if AEncrypt then begin
    for I := 1 to ACount do begin
      fEncryptor.Encrypt(ApIn^, ApOut^);
      Inc(ApIn);
      Inc(ApOut);
    end;
    Result := 3;
  end
  else begin
    for I := 1 to ACount do begin
      fDecryptor.Decrypt(ApIn^, ApOut^);
      Inc(ApIn);
      Inc(ApOut);
    end;
    Result := 3;
  end;
end;

{-------------------------------------------------------------------------------}
type
  TFDCipherReg = record
    FClass: TFDCipherClass;
    FKeyLen: Integer;
    FName: String;
    FBytes: TFDByteString;
  end;

var
  C_CipherRegs: array [0 .. 8] of TFDCipherReg = (
    (FClass: TFDCipherAES;    FKeyLen: 16; FName: 'aes-128'),
    (FClass: TFDCipherAES;    FKeyLen: 24; FName: 'aes-192'),
    (FClass: TFDCipherAES;    FKeyLen: 32; FName: 'aes-256'),
    (FClass: TFDCipherAESCTR; FKeyLen: 16; FName: 'aes-ctr-128'),
    (FClass: TFDCipherAESCTR; FKeyLen: 24; FName: 'aes-ctr-192'),
    (FClass: TFDCipherAESCTR; FKeyLen: 32; FName: 'aes-ctr-256'),
    (FClass: TFDCipherAESECB; FKeyLen: 16; FName: 'aes-ecb-128'),
    (FClass: TFDCipherAESECB; FKeyLen: 24; FName: 'aes-ecb-192'),
    (FClass: TFDCipherAESECB; FKeyLen: 32; FName: 'aes-ecb-256')
  );

function FDCipherParsePassword(APwd: PFDAnsiString; APwdLen: Integer;
  out ACipherClass: TFDCipherClass; out AKeyLen, ANameLen: Integer): Boolean;
var
  i: Integer;
  rCR: TFDCipherReg;
  sPwd: String;
begin
  ACipherClass := nil;
  AKeyLen := 0;
  ANameLen := 0;
  Result := False;
  sPwd := TFDEncoder.Deco(APwd, APwdLen, ecANSI);
  for i := Low(C_CipherRegs) to High(C_CipherRegs) do begin
    rCR := C_CipherRegs[i];
    if (APwdLen >= Length(rCR.FName) + 1) and
       (StrLIComp(PChar(sPwd), PChar(rCR.FName), Length(rCR.FName)) = 0) and
       (sPwd[Length(rCR.FName)] = ':') then begin
      ACipherClass := rCR.FClass;
      AKeyLen := rCR.FKeyLen;
      ANameLen := Length(rCR.FName);
      Result := True;
      Break;
    end;
  end;
end;

{-------------------------------------------------------------------------------}
function FDCipherGetClasses: String;
var
  i: Integer;
begin
  Result := '';
  for i := Low(C_CipherRegs) to High(C_CipherRegs) do begin
    if Result <> '' then
      Result := Result + ';';
    Result := Result + C_CipherRegs[i].FName;
  end;
end;

{-------------------------------------------------------------------------------}
function FDCipherGetName(ACipherClass: TFDCipherClass; AKeyLen: Integer): PFDAnsiString;
var
  i: Integer;
begin
  Result := nil;
  for i := Low(C_CipherRegs) to High(C_CipherRegs) do
    if (C_CipherRegs[i].FClass = ACipherClass) and
       (C_CipherRegs[i].FKeyLen = AKeyLen) then begin
      if Length(C_CipherRegs[i].FBytes) = 0 then
        C_CipherRegs[i].FBytes := TFDEncoder.Enco(C_CipherRegs[i].FName, ecANSI);
      Result := PFDAnsiString(PByte(C_CipherRegs[i].FBytes));
      Break;
    end;
end;

end.
