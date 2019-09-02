{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2012-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit FMX.Media.Android;

interface

uses
  Androidapi.JNI.Media, System.Types, FMX.Media, Androidapi.JNI.VideoView,
  Androidapi.JNI.App, Androidapi.JNI.Widget, FMX.Messages;

type
  TAndroidCaptureDeviceManager = class(TCaptureDeviceManager)
  public
    constructor Create; override;
  end;

  TAndroidMedia = class(TMedia)
  private
    FPlayer: JMediaPlayer;
    FVolume: Single;
  protected
    procedure SeekToBegin;
    function GetDuration: TMediaTime; override;
    function GetCurrent: TMediaTime; override;
    procedure SetCurrent(const Value: TMediaTime); override;
    function GetVideoSize: TPointF; override;
    function GetMediaState: TMediaState; override;
    function GetVolume: Single; override;
    procedure SetVolume(const Value: Single); override;
    procedure UpdateMediaFromControl; override;
    procedure DoPlay; override;
    procedure DoStop; override;
  public
    constructor Create(const AFileName: string); override;
    destructor Destroy; override;
  end;

  TAndroidMediaCodec = class(TCustomMediaCodec)
  public
    function CreateFromFile(const AFileName: string): TMedia; override;
  end;

  TAndroidVideo = class(TMedia)
  private
    FVideoPlayer: JVideoView;
    FLayout: JLinearLayout;
    FDialog : JDialog;
    FVideoSize: TSize;
    FOrientationChangedId: Integer;
    function AllAssigned : Boolean;
    procedure RealignView;
    procedure RetreiveVideoSize;
    procedure OrientationChangedHandler(const Sender: TObject; const Msg : TMessage);
  protected
    procedure SeekToBegin;
    function GetDuration: TMediaTime; override;
    function GetCurrent: TMediaTime; override;
    procedure SetCurrent(const Value: TMediaTime); override;
    function GetVideoSize: TPointF; override;
    function GetMediaState: TMediaState; override;
    function GetVolume: Single; override;
    procedure SetVolume(const Value: Single); override;
    procedure UpdateMediaFromControl; override;
    procedure DoPlay; override;
    procedure DoStop; override;
  public
    constructor Create(const AFileName: string); override;
    destructor Destroy; override;
  end;

  TAndroidVideoCodec = class(TCustomMediaCodec)
  public
    function CreateFromFile(const AFileName: string): TMedia; override;
  end;

implementation

uses
  Androidapi.Bitmap, Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Hardware, System.RTLConsts, System.Math, System.SysUtils, System.SyncObjs, FMX.Consts, FMX.Types,
  FMX.PixelFormats, FMX.Surfaces, FMX.Graphics, FMX.Helpers.Android, FMX.Forms;

{$REGION 'Local Class Declarations'}

type
  TAndroidAudioCaptureDevice = class(TAudioCaptureDevice)
  private
    FRecorder: JMediaRecorder;
  protected
    procedure DoStartCapture; override;
    procedure DoStopCapture; override;
    function GetDeviceState: TCaptureDeviceState; override;
  public
    destructor Destroy; override;
  end;

  TAndroidVideoCaptureDevice = class;

  TAndroidVideoCaptureCallback = class(TJavaLocal, JCamera_PreviewCallback)
  private
    [weak] FCaptureDevice: TAndroidVideoCaptureDevice;
  public
    procedure onPreviewFrame(P1: TJavaArray<Byte>; P2: JCamera); cdecl;
  end;

  TAndroidVideoCaptureDevice = class(TVideoCaptureDevice)
  private const
    CaptureTimerInterval = 33;
    VideoConversionJPEGQuality = 75;
  private
    SurfaceSection: TCriticalSection;
    UpdatedSection: TCriticalSection;
    QueueSection: TCriticalSection;

    FCameraId: Integer;
    FCamera: JCamera;
    FCapturing: Boolean;
    FQuality: TVideoCaptureQuality;
    FCallback: TAndroidVideoCaptureCallback;
    SharedBuffer: TJavaArray<Byte>;
    SharedBufferSize: TPoint;
    SharedBufferBytes: Integer;
    QueuedBufferCount: Integer;
    SharedBufferFormat: Integer;
    SharedSurface: TBitmapSurface;
    SharedSurfaceUpdated: Boolean;
    CapturePollingTimer: TTimer;

    procedure CopyBufferToSurface;
    function GetCamera: JCamera;
    procedure AddQueueBuffer;
    procedure RemoveQueueBuffer;
    procedure OnCaptureTimer(Sender: TObject);
  protected
    procedure DoStartCapture; override;
    procedure DoStopCapture; override;
    procedure DoSampleBufferToBitmap(const ABitmap: TBitmap; const ASetSize: Boolean); override;
    function GetDeviceProperty(const Prop: TCaptureDevice.TProperty): string; override;
    function GetDeviceState: TCaptureDeviceState; override;
    function GetPosition: TDevicePosition; override;
    function GetQuality: TVideoCaptureQuality; override;
    procedure SetQuality(const Value: TVideoCaptureQuality); override;
    function GetHasFlash: Boolean; override;
    function GetFlashMode: TFlashMode; override;
    procedure SetFlashMode(const Value: TFlashMode); override;
    function GetHasTorch: Boolean; override;
    function GetTorchMode: TTorchMode; override;
    procedure SetTorchMode(const Value: TTorchMode); override;
    function GetFocusMode: TFocusMode; override;
    procedure SetFocusMode(const Value: TFocusMode); override;
  public
    property CameraId: Integer read FCameraId;
    property Camera: JCamera read GetCamera;

    constructor Create(const AManager: TCaptureDeviceManager; const ADefault: Boolean); override;
    destructor Destroy; override;
  end;

{$ENDREGION}
{$REGION 'TAndroidCaptureDeviceManager'}

constructor TAndroidCaptureDeviceManager.Create;
var
  I: Integer;
  CameraDevice: TAndroidVideoCaptureDevice;
begin
  inherited;

  TAndroidAudioCaptureDevice.Create(Self, True);

  for I := 0 to TJCamera.JavaClass.getNumberOfCameras - 1 do
  begin
    CameraDevice := TAndroidVideoCaptureDevice.Create(Self, I = 0);
    CameraDevice.FCameraId := I;
  end;
end;

{$ENDREGION}
{$REGION 'TAndroidAudioCaptureDevice'}

destructor TAndroidAudioCaptureDevice.Destroy;
begin
  if Assigned(FRecorder) then
    FRecorder := nil;

  inherited;
end;

procedure TAndroidAudioCaptureDevice.DoStartCapture;
begin
  FRecorder := TJMediaRecorder.JavaClass.init;
  FRecorder.setAudioSource(TJMediaRecorder_AudioSource.JavaClass.MIC);
  FRecorder.setOutputFormat(TJMediaRecorder_OutputFormat.JavaClass.THREE_GPP);
  FRecorder.setAudioEncoder(TJMediaRecorder_AudioEncoder.JavaClass.AMR_NB);
  FRecorder.setOutputFile(StringToJString(FileName));
  FRecorder.prepare;
  FRecorder.start;
end;

procedure TAndroidAudioCaptureDevice.DoStopCapture;
begin
  if Assigned(FRecorder) then
  begin
    FRecorder.stop;
    FRecorder := nil;
  end;
end;

function TAndroidAudioCaptureDevice.GetDeviceState: TCaptureDeviceState;
begin
  if Assigned(FRecorder) then
    Result := TCaptureDeviceState.Capturing
  else
    Result := TCaptureDeviceState.Stopped;
end;

{$ENDREGION}
{$REGION 'TAndroidVideoCaptureDevice'}

procedure TAndroidVideoCaptureCallback.onPreviewFrame(P1: TJavaArray<Byte>; P2: JCamera);
begin
  if Assigned(FCaptureDevice) then
  begin
    if FCaptureDevice.FCapturing then
      FCaptureDevice.CopyBufferToSurface;

    FCaptureDevice.RemoveQueueBuffer;
  end;
end;

constructor TAndroidVideoCaptureDevice.Create(const AManager: TCaptureDeviceManager; const ADefault: Boolean);
begin
  inherited;

  SurfaceSection:= TCriticalSection.Create;
  UpdatedSection:= TCriticalSection.Create;
  QueueSection:= TCriticalSection.Create;

  FCapturing := False;
end;

destructor TAndroidVideoCaptureDevice.Destroy;
begin
  if FCapturing then
    Self.DoStopCapture;

  if Assigned(FCamera) then
  begin
    FCamera.setPreviewCallbackWithBuffer(nil);
    FCamera := nil;
  end;

  if Assigned(FCallback) then
    FCallback := nil;

  QueueSection.Free;
  UpdatedSection.Free;
  SurfaceSection.Free;

  inherited;
end;

function TAndroidVideoCaptureDevice.GetCamera: JCamera;
begin
  if not Assigned(FCamera) then
    FCamera := TJCamera.JavaClass.open(FCameraId);

  Result := FCamera;
end;

function TAndroidVideoCaptureDevice.GetDeviceProperty(const Prop: TCaptureDevice.TProperty): string;
begin
  case Prop of
    TCaptureDevice.TProperty.UniqueID:
      Result := FCameraId.ToString;

    else
      Result := '';
  end;
end;

function TAndroidVideoCaptureDevice.GetDeviceState: TCaptureDeviceState;
begin
  if FCapturing then
    Result := TCaptureDeviceState.Capturing
  else
    Result := TCaptureDeviceState.Stopped;
end;

function TAndroidVideoCaptureDevice.GetQuality: TVideoCaptureQuality;
begin
  Result := FQuality;
end;

procedure TAndroidVideoCaptureDevice.SetQuality(const Value: TVideoCaptureQuality);
begin
  FQuality := Value;

  case FQuality of
    TVideoCaptureQuality.vcPhotoQuality: ;
    TVideoCaptureQuality.vcHighQuality: ;
    TVideoCaptureQuality.vcMediumQuality: ;
    TVideoCaptureQuality.vcLowQuality: ;
  end;
end;

function TAndroidVideoCaptureDevice.GetPosition: TDevicePosition;
begin
  Result := TDevicePosition.dpUnspecified;
end;

function TAndroidVideoCaptureDevice.GetHasFlash: Boolean;
var
  Params: JCamera_Parameters;
  ModeList: JList;
begin
  Params := Camera.getParameters;
  if not Assigned(Params) then
    Exit(False);

  ModeList := Params.getSupportedFlashModes;

  if not Assigned(ModeList) then
    Exit(False);

  Result := ModeList.contains(TJCamera_Parameters.JavaClass.FLASH_MODE_ON) or
    ModeList.contains(TJCamera_Parameters.JavaClass.FLASH_MODE_AUTO);
end;

function TAndroidVideoCaptureDevice.GetHasTorch: Boolean;
var
  Params: JCamera_Parameters;
  ModeList: JList;
begin
  Params := Camera.getParameters;
  if not Assigned(Params) then
    Exit(False);

  ModeList := Params.getSupportedFlashModes;

  if not Assigned(ModeList) then
    Exit(False);

  Result := ModeList.contains(TJCamera_Parameters.JavaClass.FLASH_MODE_TORCH);
end;

function TAndroidVideoCaptureDevice.GetFlashMode: TFlashMode;
var
  Params: JCamera_Parameters;
  FlashMode: JString;
  FlashModeText: string;
begin
  Params := Camera.getParameters;
  if not Assigned(Params) then
    Exit(inherited);

  FlashMode := Params.getFlashMode;
  if not Assigned(FlashMode) then
    Exit(inherited);

  FlashModeText := JStringToString(FlashMode);

  if SameText(FlashModeText, JStringToString(TJCamera_Parameters.JavaClass.FLASH_MODE_ON)) then
    Result := TFlashMode.fmFlashOn
  else if SameText(FlashModeText, JStringToString(TJCamera_Parameters.JavaClass.FLASH_MODE_AUTO)) then
    Result := TFlashMode.fmAutoFlash
  else
    Result := TFlashMode.fmFlashOff;
end;

procedure TAndroidVideoCaptureDevice.SetFlashMode(const Value: TFlashMode);
var
  Params: JCamera_Parameters;
begin
  Params := Camera.getParameters;
  if not Assigned(Params) then
    Exit;

  case Value of
    TFlashMode.fmAutoFlash:
      Params.setFlashMode(TJCamera_Parameters.JavaClass.FLASH_MODE_AUTO);

    TFlashMode.fmFlashOff:
      Params.setFlashMode(TJCamera_Parameters.JavaClass.FLASH_MODE_OFF);

    TFlashMode.fmFlashOn:
      Params.setFlashMode(TJCamera_Parameters.JavaClass.FLASH_MODE_ON);
  end;

  Camera.setParameters(Params);
end;

function TAndroidVideoCaptureDevice.GetFocusMode: TFocusMode;
var
  Params: JCamera_Parameters;
  FocusMode: JString;
  FocusModeText: string;
begin
  Params := Camera.getParameters;
  if not Assigned(Params) then
    Exit(inherited);

  FocusMode := Params.getFocusMode;
  if not Assigned(FocusMode) then
    Exit(inherited);

  FocusModeText := JStringToString(FocusMode);

  if SameText(FocusModeText, JStringToString(TJCamera_Parameters.JavaClass.FOCUS_MODE_AUTO)) then
    Result := TFocusMode.fmAutoFocus
  else if SameText(FocusModeText, JStringToString(TJCamera_Parameters.JavaClass.FOCUS_MODE_CONTINUOUS_VIDEO)) then
    Result := TFocusMode.fmContinuousAutoFocus
  else if SameText(FocusModeText, JStringToString(TJCamera_Parameters.JavaClass.FOCUS_MODE_CONTINUOUS_PICTURE)) then
    Result := TFocusMode.fmContinuousAutoFocus
  else
    Result := TFocusMode.fmLocked;
end;

procedure TAndroidVideoCaptureDevice.SetFocusMode(const Value: TFocusMode);
var
  Params: JCamera_Parameters;
begin
  Params := Camera.getParameters;
  if not Assigned(Params) then
    Exit;

  case Value of
    TFocusMode.fmAutoFocus:
      Params.setFocusMode(TJCamera_Parameters.JavaClass.FOCUS_MODE_AUTO);

    TFocusMode.fmContinuousAutoFocus:
      Params.setFocusMode(TJCamera_Parameters.JavaClass.FOCUS_MODE_CONTINUOUS_PICTURE);

    TFocusMode.fmLocked:
      Params.setFocusMode(TJCamera_Parameters.JavaClass.FOCUS_MODE_FIXED);
  end;

  Camera.setParameters(Params);
end;

function TAndroidVideoCaptureDevice.GetTorchMode: TTorchMode;
var
  Params: JCamera_Parameters;
  FlashMode: JString;
begin
  Params := Camera.getParameters;
  if not Assigned(Params) then
    Exit(inherited);

  FlashMode := Params.getFlashMode;
  if not Assigned(FlashMode) then
    Exit(inherited);

  if SameText(JStringToString(FlashMode), JStringToString(TJCamera_Parameters.JavaClass.FLASH_MODE_TORCH)) then
    Result := TTorchMode.tmModeOn
  else
    Result := TTorchMode.tmModeOff
end;

procedure TAndroidVideoCaptureDevice.SetTorchMode(const Value: TTorchMode);
var
  Params: JCamera_Parameters;
begin
  Params := Camera.getParameters;
  if not Assigned(Params) then
    Exit;

  case Value of
    TTorchMode.tmModeOff:
      if GetTorchMode = TTorchMode.tmModeOn then
      begin
        Params.setFlashMode(TJCamera_Parameters.JavaClass.FLASH_MODE_OFF);
        Camera.setParameters(Params);
      end;

    TTorchMode.tmModeOn:
      if GetTorchMode = TTorchMode.tmModeOff then
      begin
        Params.setFlashMode(TJCamera_Parameters.JavaClass.FLASH_MODE_TORCH);
        Camera.setParameters(Params);
      end;
  end;
end;

procedure TAndroidVideoCaptureDevice.AddQueueBuffer;
begin
  QueueSection.Acquire;
  try
    if QueuedBufferCount < 1 then
    begin
      if Assigned(SharedBuffer) then
        FreeAndNil(SharedBuffer);

      SharedBuffer := TJavaArray<Byte>.Create(SharedBufferBytes);
      Camera.addCallbackBuffer(SharedBuffer);
      Inc(QueuedBufferCount);
    end;
  finally
    QueueSection.Release;
  end;
end;

procedure TAndroidVideoCaptureDevice.RemoveQueueBuffer;
begin
  QueueSection.Acquire;
  try
    QueuedBufferCount := Max(QueuedBufferCount - 1, 0);
  finally
    QueueSection.Release;
  end;
end;

procedure TAndroidVideoCaptureDevice.DoStartCapture;
var
  Params: JCamera_Parameters;
  PreviewSize: JCamera_Size;
begin
  if FCapturing then
    Exit;

  Params := Camera.getParameters;
  if not Assigned(Params) then
    Exit;

  PreviewSize := Params.getPreviewSize;

  SharedBufferSize := TPoint.Create(PreviewSize.width, PreviewSize.height);
  SharedBufferFormat := Params.getPreviewFormat;
  SharedBufferBytes := SharedBufferSize.X * SharedBufferSize.Y *
    (TJImageFormat.JavaClass.getBitsPerPixel(SharedBufferFormat) div 8);

  FCallback := TAndroidVideoCaptureCallback.Create;
  FCallback.FCaptureDevice := Self;

  SharedSurface := TBitmapSurface.Create;
  SharedSurface.SetSize(SharedBufferSize.X, SharedBufferSize.Y, pfA8B8G8R8);
  SharedSurfaceUpdated := False;

  AddQueueBuffer;

  Camera.setPreviewCallbackWithBuffer(FCallback);
  Camera.startPreview;

  FCapturing := True;

  CapturePollingTimer := TTimer.Create(nil);
  CapturePollingTimer.Interval := CaptureTimerInterval;
  CapturePollingTimer.OnTimer := OnCaptureTimer;
  CapturePollingTimer.Enabled := True;
end;

procedure TAndroidVideoCaptureDevice.DoStopCapture;
begin
  if FCapturing then
  begin
    if Assigned(CapturePollingTimer) then
      FreeAndNil(CapturePollingTimer);

    Camera.stopPreview;
    Camera.setPreviewCallbackWithBuffer(nil);

    if Assigned(FCallback) then
      FCallback := nil;

    if Assigned(SharedSurface) then
      FreeAndNil(SharedSurface);

    FCapturing := False;
  end;
end;

procedure TAndroidVideoCaptureDevice.CopyBufferToSurface;
var
  Image: JYuvImage;
  Rect: JRect;
  Stream: JByteArrayOutputStream;
  LoadOptions: JBitmapFactory_Options;
  Bitmap: JBitmap;
begin
  if Assigned(SharedBuffer) and Assigned(SharedSurface) then
  begin
    SurfaceSection.Acquire;
    try
      Image := TJYuvImage.JavaClass.init(SharedBuffer, SharedBufferFormat, SharedBufferSize.X, SharedBufferSize.Y, nil);
    finally
      SurfaceSection.Release;
    end;

    Rect := TJRect.JavaClass.init(0, 0, SharedBufferSize.X, SharedBufferSize.Y);
    Stream := TJByteArrayOutputStream.JavaClass.init(0);

    Image.compressToJpeg(Rect, VideoConversionJPEGQuality, Stream);

    // Some resources are freed as early as possible to reduce impact on working memory.
    Rect := nil;
    Image := nil;

    LoadOptions := TJBitmapFactory_Options.JavaClass.init;
    Bitmap := TJBitmapFactory.JavaClass.decodeByteArray(Stream.toByteArray, 0, Stream.Size, LoadOptions);

    Stream := nil;

    JBitmapToSurface(Bitmap, SharedSurface);
    Bitmap := nil;

    UpdatedSection.Acquire;
    try
      SharedSurfaceUpdated := True;
    finally
      UpdatedSection.Release;
    end;
  end;
end;

procedure TAndroidVideoCaptureDevice.OnCaptureTimer(Sender: TObject);
var
  UpdatePending: Boolean;
begin
  UpdatedSection.Acquire;
  try
    UpdatePending := SharedSurfaceUpdated;
    SharedSurfaceUpdated := False;
  finally
    UpdatedSection.Release;
  end;

  if UpdatePending then
  begin
    if Assigned(OnSampleBufferReady) then
      OnSampleBufferReady(Self, 0);
  end;

  AddQueueBuffer;
end;

procedure TAndroidVideoCaptureDevice.DoSampleBufferToBitmap(const ABitmap: TBitmap; const ASetSize: Boolean);
var
  BiData: TBitmapData;
  I: Integer;
begin
  if (not FCapturing) or (not Assigned(SharedSurface)) then
    Exit;

  if ASetSize then
    ABitmap.SetSize(SharedBufferSize.X, SharedBufferSize.Y);

  SurfaceSection.Acquire;
  try
    if ABitmap.Map(TMapAccess.maWrite, BiData) then
    try
      for I := 0 to SharedBufferSize.Y - 1 do
        Move(SharedSurface.Scanline[I]^, BiData.GetScanline(I)^, SharedBufferSize.X * SharedSurface.BytesPerPixel);
    finally
      ABitmap.Unmap(BiData);
    end;
  finally
    SurfaceSection.Release;
  end;
end;

{$ENDREGION}
{$REGION 'TAndroidMedia'}

constructor TAndroidMedia.Create(const AFileName: string);
var
  AudioService: JObject;
  AudioManager: JAudioManager;
  MaxVolume : Integer;
begin
  inherited Create(AFileName);
  FPlayer := TJMediaPlayer.JavaClass.init;
  FPlayer.setDataSource(StringToJString(FileName));
  FPlayer.prepare;
  AudioService := SharedActivity.getSystemService(TJContext.JavaClass.AUDIO_SERVICE);
  if Assigned(AudioService) then
    AudioManager := TJAudioManager.Wrap((AudioService as ILocalObject).GetObjectID);
  if Assigned(AudioManager) then
  begin
    MaxVolume := AudioManager.getStreamMaxVolume(TJAudioManager.JavaClass.STREAM_MUSIC);
    FVolume := AudioManager.getStreamVolume(TJAudioManager.JavaClass.STREAM_MUSIC);
    if MaxVolume > 0 then
      FVolume := FVolume / MaxVolume ;
    if FVolume > 1 then
      FVolume := 1 ;
  end;
end;

destructor TAndroidMedia.Destroy;
begin
  FPlayer.release;
  FPlayer := nil;
  inherited Destroy;
end;

function TAndroidMedia.GetCurrent: TMediaTime;
begin
  if Assigned(FPlayer) then
    Result := FPlayer.getCurrentPosition
  else
    Result := 0;
end;

function TAndroidMedia.GetDuration: TMediaTime;
begin
  Result := FPlayer.getDuration;
end;

function TAndroidMedia.GetMediaState: TMediaState;
begin
  if Assigned(FPlayer) then
  begin
    if FPlayer.isPlaying then
      Result := TMediaState.Playing
    else
      Result := TMediaState.Stopped;
  end
  else
    Result := TMediaState.Unavailable;
end;

function TAndroidMedia.GetVideoSize: TPointF;
begin
  if Assigned(FPlayer) then
    Result := TPointF.Create(FPlayer.getVideoWidth, FPlayer.getVideoHeight)
  else
    Result := TPointF.Create(0, 0);
end;

function TAndroidMedia.GetVolume: Single;
begin
  if Assigned(FPlayer) then
    Result := FVolume
  else
    Result := 0;
end;

procedure TAndroidMedia.SeekToBegin;
begin
  FPlayer.seekTo(0);
end;

procedure TAndroidMedia.SetCurrent(const Value: TMediaTime);
var
  NewPos : Integer;
begin
  if Assigned(FPlayer) then
  begin
    NewPos := Value;
    if NewPos < 0 then
      NewPos := 0;
    FPlayer.seekTo(NewPos);
  end;
end;

procedure TAndroidMedia.SetVolume(const Value: Single);
begin
  FVolume := Value;

  if FVolume < 0 then
    FVolume := 0
  else if FVolume > 1 then
    FVolume := 1;

  if Assigned(FPlayer) then
    FPlayer.setVolume(FVolume, FVolume);
end;

procedure TAndroidMedia.UpdateMediaFromControl;
begin
end;

procedure TAndroidMedia.DoStop;
begin
  FPlayer.stop;
end;

procedure TAndroidMedia.DoPlay;
begin
  FPlayer.start;
end;

{$ENDREGION}
{$REGION 'TAndroidMediaCodec'}

function TAndroidMediaCodec.CreateFromFile(const AFileName: string): TMedia;
begin
  Result := TAndroidMedia.Create(AFileName);
end;

{$ENDREGION}

{$REGION 'TAndroidVideoCodec'}

function TAndroidVideoCodec.CreateFromFile(const AFileName: string): TMedia;
begin
  Result := TAndroidVideo.Create(AFileName);
end;
{$ENDREGION}

{$REGION 'TAndroidVideo'}

function TAndroidVideo.AllAssigned: Boolean;
begin
  Result := Assigned(FVideoPlayer) and Assigned(FDialog);
end;

constructor TAndroidVideo.Create(const AFileName: string);
begin
  inherited Create(AFileName);

  RetreiveVideoSize;

  CallInUIThread(
    procedure
    begin
      FDialog := TJDialog.JavaClass.init(SharedActivity,-1);
      FDialog.setCancelable(True);
      FVideoPlayer := TJVideoView.JavaClass.init(SharedActivity);
      FVideoPlayer.setVideoPath(StringToJString(FileName));
      FVideoPlayer.setMediaController(TJMediaController.JavaClass.init(SharedActivity));
      FVideoPlayer.requestFocus(0);

      FLayout := TJLinearLayout.JavaClass.init(SharedActivity);
      FLayout.addView(FVideoPlayer);

      RealignView;

      FDialog.setContentView(FLayout);
    end);

  FOrientationChangedId := TMessageManager.DefaultManager.SubscribeToMessage(TOrientationChangedMessage, OrientationChangedHandler);
end;

procedure TAndroidVideo.OrientationChangedHandler(const Sender: TObject; const Msg: TMessage);
begin
  CallInUIThread(
    procedure
    begin
      RealignView;
    end
  );
end;

procedure TAndroidVideo.RealignView;
const
  GreatThatScreen = 100; // To be sure that destination rect will fit to fullscreen
var
  VideoRect: TRectF;
begin
  if Assigned(FLayout) then
  begin
    VideoRect := TRectF.Create(0, 0, FVideoSize.Width * GreatThatScreen, FVideoSize.Height * GreatThatScreen);
    VideoRect.Fit(TRectF.Create(0, 0, Screen.Size.Width, Screen.Size.Height));

    FLayout.setPadding(VideoRect.Round.Left, VideoRect.Round.Top, Screen.Size.Width - VideoRect.Round.Right,
      Screen.Size.Height - VideoRect.Round.Bottom);
  end;
end;

procedure TAndroidVideo.RetreiveVideoSize;
var
  MediaPlayer: JMediaPlayer;
begin
  MediaPlayer := TJMediaPlayer.JavaClass.init;
  MediaPlayer.setDataSource(StringToJString(FileName));
  MediaPlayer.prepare;
  FVideoSize := TSize.Create(MediaPlayer.getVideoWidth, MediaPlayer.getVideoHeight);
  MediaPlayer := nil;
end;

destructor TAndroidVideo.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TOrientationChangedMessage, FOrientationChangedId);
  if Assigned(FVideoPlayer) then
    FVideoPlayer := nil;
  inherited Destroy;
end;

procedure TAndroidVideo.DoPlay;
begin
  inherited;
  CallInUIThread(
    procedure
    begin
      if not FDialog.isShowing then
        FDialog.show;
      if not FVideoPlayer.isPlaying then
        FVideoPlayer.start;
    end);
end;

procedure TAndroidVideo.DoStop;
begin
  inherited;
  CallInUIThread(
    procedure
    begin
      if FDialog.isShowing then
        FDialog.hide;
      if FVideoPlayer.isPlaying then
        FVideoPlayer.stopPlayback;
    end);
end;

function TAndroidVideo.GetCurrent: TMediaTime;
begin
  Result := 0;
  if AllAssigned then
    Result := FVideoPlayer.getCurrentPosition;
end;

function TAndroidVideo.GetDuration: TMediaTime;
begin
  Result := 0;
  if AllAssigned then
    Result := FVideoPlayer.getDuration;
end;

function TAndroidVideo.GetMediaState: TMediaState;
begin
  if Assigned(FVideoPlayer) then
  begin
    if FVideoPlayer.isPlaying then
      Result := TMediaState.Playing
    else
      Result := TMediaState.Stopped;
  end
  else
    Result := TMediaState.Unavailable;
end;

function TAndroidVideo.GetVideoSize: TPointF;
begin
  Result := PointF(FVideoSize.Width, FVideoSize.Height);
end;

function TAndroidVideo.GetVolume: Single;
begin
  Result := NaN;
end;

procedure TAndroidVideo.SeekToBegin;
begin
  if AllAssigned then
    CallInUIThread(
      procedure
      begin
        if FVideoPlayer.isPlaying then
          FVideoPlayer.stopPlayback;
        FVideoPlayer.seekTo(0);
      end);
end;

procedure TAndroidVideo.SetCurrent(const Value: TMediaTime);
begin
  inherited;
  if AllAssigned then
    CallInUIThread(
      procedure
      begin
        FVideoPlayer.seekTo(Value);
      end);
end;

procedure TAndroidVideo.SetVolume(const Value: Single);
begin
  inherited;

end;

procedure TAndroidVideo.UpdateMediaFromControl;
begin
  inherited;
end;

{$ENDREGION}

initialization
  TMediaCodecManager.RegisterMediaCodecClass('.mov', SVMOVFiles, TMediaType.Video, TAndroidVideoCodec);
  TMediaCodecManager.RegisterMediaCodecClass('.m4v', SVM4VFiles, TMediaType.Video, TAndroidVideoCodec);
  TMediaCodecManager.RegisterMediaCodecClass('.mp4', SVMP4Files, TMediaType.Video, TAndroidVideoCodec);
  TMediaCodecManager.RegisterMediaCodecClass('.mp3', SVMP3Files, TMediaType.Audio, TAndroidMediaCodec);
  TMediaCodecManager.RegisterMediaCodecClass('.caf', SVCAFFiles, TMediaType.Audio, TAndroidMediaCodec);
  TMediaCodecManager.RegisterMediaCodecClass('.3gp', SV3GPFiles, TMediaType.Audio, TAndroidMediaCodec);

end.
