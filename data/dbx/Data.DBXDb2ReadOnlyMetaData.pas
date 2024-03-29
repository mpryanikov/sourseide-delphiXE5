{*******************************************************}
{                                                       }
{               Delphi DBX Framework                    }
{                                                       }
{ Copyright(c) 1995-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

{$HPPEMIT '#pragma link "Data.DBXDb2ReadOnlyMetaData"'}    {Do not Localize}
unit Data.DBXDb2ReadOnlyMetaData;

interface

uses
  Data.DBXMetaDataReader,
  Data.DBXMetaDataCommandFactory;

type
  TDBXDb2MetaDataCommandFactory = class(TDBXMetaDataCommandFactory)
  public
    function CreateMetaDataReader: TDBXMetaDataReader; override;
  end;

implementation

uses
  Data.DBXDb2MetaDataReader;

function TDBXDb2MetaDataCommandFactory.CreateMetaDataReader: TDBXMetaDataReader;
begin
  Result := TDBXDb2MetaDataReader.Create;
end;

initialization
  TDBXMetaDataCommandFactory.RegisterMetaDataCommandFactory(TDBXDb2MetaDataCommandFactory);
finalization
  TDBXMetaDataCommandFactory.UnRegisterMetaDataCommandFactory(TDBXDb2MetaDataCommandFactory);
end.
