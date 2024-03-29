{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit FMX.SpellChecker.Mac;

interface

implementation

uses
  System.SysUtils, FMX.SpellChecker, FMX.Platform, Macapi.AppKit,
  Macapi.Foundation, Macapi.ObjectiveC, FMX.Types, Macapi.CocoaTypes;


type
  TCocoaSpellCheckerService = class(TInterfacedObject, IFMXSpellCheckerService)
  private
    FChecker: NSSpellChecker;
  public
    constructor Create;
    destructor Destroy; override;
    //IFMXSpellCheckerService
    function CheckSpelling(Word: string): TArray<string>;
    function GuessCompletions(Word: string): TArray<string>;
  end;


var
  CocoaSpellCheckerService: TCocoaSpellCheckerService;


procedure RegisterSpellCheckerService;
begin
  CocoaSpellCheckerService := TCocoaSpellCheckerService.Create;
  TPlatformServices.Current.AddPlatformService(IFMXSpellCheckerService, CocoaSpellCheckerService);
end;

procedure UnregisterSpellCheckerService;
begin
  TPlatformServices.Current.RemovePlatformService(IFMXSpellCheckerService);
  FreeAndNil(CocoaSpellCheckerService);
end;


{ TOSXSpellCheckerService }

function TCocoaSpellCheckerService.CheckSpelling(Word: string): TArray<string>;
var
  I: Integer;
  SpellWords: NSArray;
  Range: NSRange;
begin
  Assert(Assigned(FChecker), 'Native bridge was not created');
  Result := nil;
  if FChecker.checkSpellingOfString(NSSTR(Word), 0).location <> NSNotFound then
  begin
    Range.location := 0;
    Range.length := Word.Length;
    SpellWords := FChecker.guessesForWordRange(Range, NSSTR(Word), nil, 0);
    if SpellWords.count > 0 then
    begin
      SetLength(Result, SpellWords.count);
      for I := 0 to SpellWords.count - 1 do
        Result[I] := UTF8ToUnicodeString(TNSString.Wrap(SpellWords.objectAtIndex(I)).UTF8String);
    end;
  end;
end;

constructor TCocoaSpellCheckerService.Create;
begin
  inherited;
  FChecker := TNSSpellChecker.Create;
end;

destructor TCocoaSpellCheckerService.Destroy;
begin
  if Assigned(FChecker) then
  begin
    FChecker.release;
    FChecker := nil;
  end;
  inherited;
end;

function TCocoaSpellCheckerService.GuessCompletions(
  Word: string): TArray<string>;
var
  I: Integer;
  Words: NSArray;
  Range: NSRange;
begin
  Assert(Assigned(FChecker), 'Native bridge was not created');
  Result := nil;
  Range.location := 0;
  Range.length := Word.Length;
  Words := FChecker.completionsForPartialWordRange(Range, NSSTR(Word), nil, 0);
  if Words.count > 0 then
  begin
    SetLength(Result, Words.count);
    for I := 0 to Words.count - 1 do
      Result[I] := UTF8ToUnicodeString(TNSString.Wrap(Words.objectAtIndex(I)).UTF8String);
  end;
end;


initialization
  RegisterSpellCheckerService;
finalization
  UnregisterSpellCheckerService;
end.
