{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit Androidapi.Rect;

interface

{$I Androidapi.inc}

type
  ARect = record
    left: Int32;
    top: Int32;
    right: Int32;
    bottom: Int32;
  end;
  {$EXTERNALSYM ARect}
  PARect = ^ARect;
  
implementation

end.

