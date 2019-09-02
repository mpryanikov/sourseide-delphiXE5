{*******************************************************}
{                                                       }
{             Delphi REST Client Framework              }
{                                                       }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}
unit REST.Utils;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Character;


//This URIEncode variant implements an encoding as specified by OAuth mechanisms
function URIEncode(const S: string): string;
function iif(const ACondition: boolean; const BranchTRUE, BranchFALSE: string): string;

procedure ExtractURLSegmentNames(const AString: string; AList: TStrings);

function RESTComponentIsDesigning(AComp: TComponent): boolean;

implementation

const
  URL_UNRESERVED_CHARS = ['A' .. 'Z', 'a' .. 'z', '0' .. '9', '-', '_', '.', '~'];

Var
  GURLUnreservedChars: Array of Char;

procedure InitUnreservedChars;
var
  c: Char;
  i: integer;
begin
  SetLength(GURLUnreservedChars, 1024);
  i := 0;
  for c in URL_UNRESERVED_CHARS do
  begin
    inc(i);
    GURLUnreservedChars[i] := c;
  end;
  SetLength(GURLUnreservedChars, i);
end;

function URIEncode(const S: string): string;
var
  c: Char;
begin
  result := '';
  for c in S do
    if c.IsInArray(GURLUnreservedChars) then
      result := result + WideChar(c)
    else
      result := result + '%' + IntToHex(Ord(c), 2);
end;

function iif(const ACondition: boolean; const BranchTRUE, BranchFALSE: string): string;
begin
  if ACondition then
    result := BranchTRUE
  else
    result := BranchFALSE;
end;

function RESTComponentIsDesigning(AComp: TComponent): boolean;
begin
  result := ([csDesigning, csLoading] * AComp.ComponentState = [csDesigning]) and
    ((AComp.Owner = nil) or ([csDesigning, csLoading] * AComp.Owner.ComponentState = [csDesigning]));
end;

procedure ExtractURLSegmentNames(const AString: string; AList: TStrings);
var
  LIndex: integer;
  LResource: string;
  LName: string;
begin
  LResource := AString;
  LIndex := LResource.IndexOf('{');
  while (LIndex >= 0) do
  begin
    LResource := LResource.Substring(LIndex + 1);
    LIndex := LResource.IndexOf('}');
    if (LIndex >= 0) then
    begin
      LName := LResource.Substring(0, LIndex);
      if (LName <> '') AND (AList.IndexOf(LName) < 0) then
        AList.Add(LName);
      LResource := LResource.Substring(LIndex + 1);
      LIndex := LResource.IndexOf('{');
    end;
  end;
end;

initialization

InitUnreservedChars;

end.
