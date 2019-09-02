{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit Androidapi.EglPlatform;

interface

uses Androidapi.KhrPlatform,
     Androidapi.NativeWindow;

{$I Androidapi.inc}

type
  egl_native_pixmap_t  = record end;
  {$EXTERNALSYM egl_native_pixmap_t}
  Pegl_native_pixmap_t = ^egl_native_pixmap_t;
  
  EGLNativeWindowType  = PANativeWindow;
  {$EXTERNALSYM EGLNativeWindowType}
  EGLNativePixmapType  = Pegl_native_pixmap_t;
  {$EXTERNALSYM EGLNativePixmapType}
  EGLNativeDisplayType = Pointer;
  {$EXTERNALSYM EGLNativeDisplayType}
  
  
  NativeDisplayType = EGLNativeDisplayType;
  {$EXTERNALSYM NativeDisplayType}
  NativePixmapType  = EGLNativePixmapType;
  {$EXTERNALSYM NativePixmapType}
  NativeWindowType  = EGLNativeWindowType;
  {$EXTERNALSYM NativeWindowType}
   
implementation

end.
