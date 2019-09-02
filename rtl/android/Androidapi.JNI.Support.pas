{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit Androidapi.JNI.Support;

interface

uses 
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Widget,
  Androidapi.JNI.App,
  Androidapi.JNI.Net,
  Androidapi.JNI.GraphicsContentViewText;

type

  {Class forward declarations}
  JNotificationCompat_Builder = interface;//android.support.v4.app.NotificationCompat$Builder
  JNotificationCompat_Style = interface;//android.support.v4.app.NotificationCompat$Style

JNotificationCompat_BuilderClass = interface(JObjectClass)
['{6EC74C2C-EBCC-4A55-98B6-6DD36DE3BA8C}']
  {Methods}
  function init(context: JContext): JNotificationCompat_Builder; cdecl;
end;

[JavaSignature('android/support/v4/app/NotificationCompat$Builder')]
JNotificationCompat_Builder = interface(JObject)
['{7DE9C385-1C34-413C-9E85-D8FA90028065}']
  {Methods}
  function addAction(icon: Integer; title: JCharSequence; intent: JPendingIntent): JNotificationCompat_Builder; cdecl;
  function build: JNotification; cdecl;
  function getNotification: JNotification; cdecl;//Deprecated
  function setAutoCancel(autoCancel: Boolean): JNotificationCompat_Builder; cdecl;
  function setContent(views: JRemoteViews): JNotificationCompat_Builder; cdecl;
  function setContentInfo(info: JCharSequence): JNotificationCompat_Builder; cdecl;
  function setContentIntent(intent: JPendingIntent): JNotificationCompat_Builder; cdecl;
  function setContentText(text: JCharSequence): JNotificationCompat_Builder; cdecl;
  function setContentTitle(title: JCharSequence): JNotificationCompat_Builder; cdecl;
  function setDefaults(defaults: Integer): JNotificationCompat_Builder; cdecl;
  function setDeleteIntent(intent: JPendingIntent): JNotificationCompat_Builder; cdecl;
  function setFullScreenIntent(intent: JPendingIntent; highPriority: Boolean): JNotificationCompat_Builder; cdecl;
  function setLargeIcon(icon: JBitmap): JNotificationCompat_Builder; cdecl;
  function setLights(argb: Integer; onMs: Integer; offMs: Integer): JNotificationCompat_Builder; cdecl;
  function setNumber(number: Integer): JNotificationCompat_Builder; cdecl;
  function setOngoing(ongoing: Boolean): JNotificationCompat_Builder; cdecl;
  function setOnlyAlertOnce(onlyAlertOnce: Boolean): JNotificationCompat_Builder; cdecl;
  function setPriority(pri: Integer): JNotificationCompat_Builder; cdecl;
  function setProgress(max: Integer; progress: Integer; indeterminate: Boolean): JNotificationCompat_Builder; cdecl;
  function setSmallIcon(icon: Integer): JNotificationCompat_Builder; cdecl; overload;
  function setSmallIcon(icon: Integer; level: Integer): JNotificationCompat_Builder; cdecl; overload;
  function setSound(sound: Jnet_Uri): JNotificationCompat_Builder; cdecl; overload;
  function setSound(sound: Jnet_Uri; streamType: Integer): JNotificationCompat_Builder; cdecl; overload;
  function setStyle(style: JNotificationCompat_Style): JNotificationCompat_Builder; cdecl;
  function setSubText(text: JCharSequence): JNotificationCompat_Builder; cdecl;
  function setTicker(tickerText: JCharSequence): JNotificationCompat_Builder; cdecl; overload;
  function setTicker(tickerText: JCharSequence; views: JRemoteViews): JNotificationCompat_Builder; cdecl; overload;
  function setUsesChronometer(b: Boolean): JNotificationCompat_Builder; cdecl;
  function setVibrate(pattern: TJavaArray<Int64>): JNotificationCompat_Builder; cdecl;
  function setWhen(when: Int64): JNotificationCompat_Builder; cdecl;
end;
TJNotificationCompat_Builder = class(TJavaGenericImport<JNotificationCompat_BuilderClass, JNotificationCompat_Builder>) end;

JNotificationCompat_StyleClass = interface(JObjectClass)
['{A76478B0-8BCB-4AFA-AFCD-CB0460219CDA}']
  {Methods}
  function init: JNotificationCompat_Style; cdecl;
end;

[JavaSignature('android/support/v4/app/NotificationCompat$Style')]
JNotificationCompat_Style = interface(JObject)
['{5C782C73-8C4B-4ADA-994D-4293E0D2D282}']
  {Methods}
  function build: JNotification; cdecl;
  procedure setBuilder(builder: JNotificationCompat_Builder); cdecl;
end;
TJNotificationCompat_Style = class(TJavaGenericImport<JNotificationCompat_StyleClass, JNotificationCompat_Style>) end;




implementation

begin

end.


