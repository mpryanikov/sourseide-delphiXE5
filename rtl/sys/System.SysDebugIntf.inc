{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

(* =================================================================== *)
var
  SysBuffer: array of Byte;
  SysBufferPos: Integer;
  SysActive, SysBuffered: Boolean;

function SysCreateConnection(const AIP: string; Port: Integer):Boolean;
procedure SysDebugEnter(const Msg: string);
procedure SysDebugLeave(const Msg: string);
procedure SysDebug(const Msg: string);
procedure SysDebugOn;
procedure SysDebugOff;
procedure SysDebugBuffered(Activate: Boolean);
procedure SysDebugPrintBuffer;
procedure SysDebugAddBuffer(const Msg :string);
procedure SysDebugNNL(const Msg: string);

function SysIntToStr(val: Int64): string;
(* =================================================================== *)
