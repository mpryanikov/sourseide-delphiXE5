{*******************************************************}
{                                                       }
{             Delphi REST Client Framework              }
{                                                       }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}
unit REST.Exception;

interface

uses
  System.SysUtils;

type
  /// <summary>
  /// Exceptions coming from the REST Library have this common ancestor
  /// </summary>
  ERESTException = class(Exception)
  private
  public
    constructor Create(const AMsg: string);
  end;

implementation

{ ERESTException }

constructor ERESTException.Create(const AMsg: string);
begin
  inherited Create(AMsg);
end;

end.
