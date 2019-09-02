{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit FMX.Helpers.Mac;

interface

uses
  Macapi.Foundation;

{ Date Time }

function GetTimeZone: Integer;
function DateTimeToNSDate(const ADateTime: TDateTime): NSDate;
function NSDateToDateTime(const ADateTime: NSDate): TDateTime;
function GetGMTDateTime(const ADateTime: TDateTime): TDateTime;

implementation

uses
  System.SysUtils, System.Types, 
  Macapi.CocoaTypes;
                               
function GetTimeZone: Integer;
begin
  Result := TNSTimeZone.Wrap(TNSTimeZone.OCClass.localTimeZone).secondsFromGMT div SecsPerHour;
end;

function DateTimeToNSDate(const ADateTime: TDateTime): NSDate;
var
  IntervalInterval: NSTimeInterval;
begin
  IntervalInterval := (ADateTime  - EncodeDate(2001, 1, 1)) * SecsPerDay;
  Result := TNSDate.Wrap(TNSDate.OCClass.dateWithTimeIntervalSinceReferenceDate(IntervalInterval));
end;

function NSDateToDateTime(const ADateTime: NSDate): TDateTime;
begin
  Result := (ADateTime.TimeIntervalSince1970 + GetTimeZone) / SecsPerDay + EncodeDate(1970, 1, 1);
end;

function GetGMTDateTime(const ADateTime: TDateTime): TDateTime;
begin
  if GetTimeZone > 0 then
    Result := ADateTime - EncodeTime(GetTimeZone, 0, 0, 0)
  else
    Result := ADateTime + EncodeTime(Abs(GetTimeZone), 0, 0, 0);
end;

end.
