{*******************************************************}
{                                                       }
{             Delphi REST Client Framework              }
{                                                       }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}
unit REST.Consts;

interface

const
  HTTP_HEADERFIELD_AUTH = 'Authorization'; // do not localize

resourcestring
  RNullableNoValue = 'Nullable type has no value';
  sExecuteRequestNilClient = 'Can''t execute TCustomRESTRequest when Client property is nil.';
  sParameterName = 'Name';
  sParameterValue = 'Value';
  sParameterKind = 'Kind';
  sOperationNotAllowedOnActiveComponent = 'Operation not allowed on active %s';
  sConfigureRESTComponent = 'Configure...';
  sExecuteRESTComponent = 'Execute...';
  sResetRESTClient = 'Reset Client';
  sClearRESTRequest = 'Clear Request Data';
  sClearRESTResponse = 'Clear Response Data';

  sResetRESTClientQuestion = 'Are you sure that you want to reset this Client to its default settings?';
  sClearRESTRequestQuestion ='Are you sure that you want to clear this Request''s data (including its Response)?';
  sClearRESTResponseQuestion ='Are you sure that you want to clear this Response''s data?';
  sInvalidRESTComponent = 'Invalid component-type: "%s"';
  sRESTAdapterUpdateDS = 'Update DataSet';
  sRESTAdapterClearDS = 'Clear DataSet';
  sRESTUnsupportedAuthMethod = 'Unsupported Authentication Method!';
  sRESTUnsupportedRequestMethod = 'Unsupported Request Method';
  sRESTErrorEmptyURL = 'URL for a request must not be empty';
  sRESTErrorEmptyParamName = 'Name of a parameter must not be empty';

implementation

end.
