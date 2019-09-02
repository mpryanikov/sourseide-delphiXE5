{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

const
  UIO_FASTIOV = 8;
  UIO_MAXIOV  = 1024;

type
  iovec = record
    iov_base: Pointer;    { Pointer to data.  }
    iov_len: size_t;      { Length of data.  }
  end;
  {$EXTERNALSYM iovec}
  Piovec = ^iovec;
