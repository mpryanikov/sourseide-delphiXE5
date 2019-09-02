{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit Androidapi.JNI.PlayServices;

interface

uses 
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
  Androidapi.JNI.App,
  Androidapi.JNI.Net,
  Androidapi.JNI.Location,
  Androidapi.JNI.Widget,
  Androidapi.JNI.Util,
  Androidapi.JNI.GraphicsContentViewText;

type

  {Class forward declarations}
  JICameraUpdateFactoryDelegate_a = interface;//com.google.android.gms.maps.internal.ICameraUpdateFactoryDelegate$a
  JRoomEntity = interface;//com.google.android.gms.games.multiplayer.realtime.RoomEntity
  JPlayerEntityCreator = interface;//com.google.android.gms.games.PlayerEntityCreator
  JGeofence = interface;//com.google.android.gms.location.Geofence
  JImageManager_OnImageLoadedListener = interface;//com.google.android.gms.common.images.ImageManager$OnImageLoadedListener
  JIUiSettingsDelegate = interface;//com.google.android.gms.maps.internal.IUiSettingsDelegate
  JOrganizations_Type = interface;//com.google.android.gms.plus.model.people.Person$Organizations$Type
  JPolyline = interface;//com.google.android.gms.maps.model.Polyline
  JPanoramaClient_OnPanoramaInfoLoadedListener = interface;//com.google.android.gms.panorama.PanoramaClient$OnPanoramaInfoLoadedListener
  JUrls_Type = interface;//com.google.android.gms.plus.model.people.Person$Urls$Type
  JActivityRecognitionResultCreator = interface;//com.google.android.gms.location.ActivityRecognitionResultCreator
  JCameraPosition_Builder = interface;//com.google.android.gms.maps.model.CameraPosition$Builder
  JOnStateDeletedListener = interface;//com.google.android.gms.appstate.OnStateDeletedListener
  JPlusClient_Builder = interface;//com.google.android.gms.plus.PlusClient$Builder
  JOnInvitationReceivedListener = interface;//com.google.android.gms.games.multiplayer.OnInvitationReceivedListener
  JPerson_Collection = interface;//com.google.android.gms.plus.model.people.Person$Collection
  JDataBufferUtils = interface;//com.google.android.gms.common.data.DataBufferUtils
  JCameraPosition = interface;//com.google.android.gms.maps.model.CameraPosition
  JAppStateClient_Builder = interface;//com.google.android.gms.appstate.AppStateClient$Builder
  JPlusOneButton = interface;//com.google.android.gms.plus.PlusOneButton
  JFreezable = interface;//com.google.android.gms.common.data.Freezable
  JInvitation = interface;//com.google.android.gms.games.multiplayer.Invitation
  JEmails_Type = interface;//com.google.android.gms.plus.model.people.Person$Emails$Type
  JCameraPositionCreator = interface;//com.google.android.gms.maps.model.CameraPositionCreator
  JRoom = interface;//com.google.android.gms.games.multiplayer.realtime.Room
  JLocationRequest = interface;//com.google.android.gms.location.LocationRequest
  JIUiSettingsDelegate_a = interface;//com.google.android.gms.maps.internal.IUiSettingsDelegate$a
  JPerson_Urls = interface;//com.google.android.gms.plus.model.people.Person$Urls
  JPerson_Name = interface;//com.google.android.gms.plus.model.people.Person$Name
  JLatLngBounds_Builder = interface;//com.google.android.gms.maps.model.LatLngBounds$Builder
  JGameEntity = interface;//com.google.android.gms.games.GameEntity
  JGroundOverlayOptions = interface;//com.google.android.gms.maps.model.GroundOverlayOptions
  JIPolylineDelegate_a = interface;//com.google.android.gms.maps.model.internal.IPolylineDelegate$a
  JDataBuffer = interface;//com.google.android.gms.common.data.DataBuffer
  JGameBuffer = interface;//com.google.android.gms.games.GameBuffer
  JLocationClient_OnAddGeofencesResultListener = interface;//com.google.android.gms.location.LocationClient$OnAddGeofencesResultListener
  JGame = interface;//com.google.android.gms.games.Game
  JILocationSourceDelegate = interface;//com.google.android.gms.maps.internal.ILocationSourceDelegate
  JOnStateListLoadedListener = interface;//com.google.android.gms.appstate.OnStateListLoadedListener
  JLocationSource_OnLocationChangedListener = interface;//com.google.android.gms.maps.LocationSource$OnLocationChangedListener
  JParticipant = interface;//com.google.android.gms.games.multiplayer.Participant
  JUiSettings = interface;//com.google.android.gms.maps.UiSettings
  JVisibleRegionCreator = interface;//com.google.android.gms.maps.model.VisibleRegionCreator
  JOnLeaderboardScoresLoadedListener = interface;//com.google.android.gms.games.leaderboard.OnLeaderboardScoresLoadedListener
  JCover_CoverInfo = interface;//com.google.android.gms.plus.model.people.Person$Cover$CoverInfo
  JLatLngCreator = interface;//com.google.android.gms.maps.model.LatLngCreator
  Jgames_OnSignOutCompleteListener = interface;//com.google.android.gms.games.OnSignOutCompleteListener
  JIGoogleMapDelegate = interface;//com.google.android.gms.maps.internal.IGoogleMapDelegate
  JPanoramaClient_a = interface;//com.google.android.gms.panorama.PanoramaClient$a
  JTileOverlayOptionsCreator = interface;//com.google.android.gms.maps.model.TileOverlayOptionsCreator
  JPerson_Gender = interface;//com.google.android.gms.plus.model.people.Person$Gender
  JParticipantEntity = interface;//com.google.android.gms.games.multiplayer.ParticipantEntity
  JPerson_Emails = interface;//com.google.android.gms.plus.model.people.Person$Emails
  JPlusOneButton_OnPlusOneClickListener = interface;//com.google.android.gms.plus.PlusOneButton$OnPlusOneClickListener
  JCameraUpdateFactory = interface;//com.google.android.gms.maps.CameraUpdateFactory
  JPlusClient_OnAccessRevokedListener = interface;//com.google.android.gms.plus.PlusClient$OnAccessRevokedListener
  JLocationStatusCodes = interface;//com.google.android.gms.location.LocationStatusCodes
  JGoogleMap_OnMarkerDragListener = interface;//com.google.android.gms.maps.GoogleMap$OnMarkerDragListener
  JGoogleMapOptions = interface;//com.google.android.gms.maps.GoogleMapOptions
  JLeaderboardBuffer = interface;//com.google.android.gms.games.leaderboard.LeaderboardBuffer
  JMoment_Builder = interface;//com.google.android.gms.plus.model.moments.Moment$Builder
  JCircle = interface;//com.google.android.gms.maps.model.Circle
  JPerson = interface;//com.google.android.gms.plus.model.people.Person
  JCircleOptions = interface;//com.google.android.gms.maps.model.CircleOptions
  JPlayer = interface;//com.google.android.gms.games.Player
  JGoogleMap_InfoWindowAdapter = interface;//com.google.android.gms.maps.GoogleMap$InfoWindowAdapter
  JGoogleAuthException = interface;//com.google.android.gms.auth.GoogleAuthException
  JAppStateClient = interface;//com.google.android.gms.appstate.AppStateClient
  JIMapViewDelegate_a = interface;//com.google.android.gms.maps.internal.IMapViewDelegate$a
  JGoogleCloudMessaging = interface;//com.google.android.gms.gcm.GoogleCloudMessaging
  JVisibleRegion = interface;//com.google.android.gms.maps.model.VisibleRegion
  JIProjectionDelegate = interface;//com.google.android.gms.maps.internal.IProjectionDelegate
  JOnAchievementsLoadedListener = interface;//com.google.android.gms.games.achievement.OnAchievementsLoadedListener
  JBitmapDescriptorFactory = interface;//com.google.android.gms.maps.model.BitmapDescriptorFactory
  JPolylineOptionsCreator = interface;//com.google.android.gms.maps.model.PolylineOptionsCreator
  JSafeParcelable = interface;//com.google.android.gms.common.internal.safeparcel.SafeParcelable
  JLeaderboardScore = interface;//com.google.android.gms.games.leaderboard.LeaderboardScore
  JItemScope = interface;//com.google.android.gms.plus.model.moments.ItemScope
  JPlusClient_b = interface;//com.google.android.gms.plus.PlusClient$b
  JGooglePlayServicesClient_OnConnectionFailedListener = interface;//com.google.android.gms.common.GooglePlayServicesClient$OnConnectionFailedListener
  JRoomConfig = interface;//com.google.android.gms.games.multiplayer.realtime.RoomConfig
  JSignInButton = interface;//com.google.android.gms.common.SignInButton
  JGamesClient_Builder = interface;//com.google.android.gms.games.GamesClient$Builder
  JRealTimeReliableMessageSentListener = interface;//com.google.android.gms.games.multiplayer.realtime.RealTimeReliableMessageSentListener
  JActivityRecognitionClient = interface;//com.google.android.gms.location.ActivityRecognitionClient
  JRealTimeSocket = interface;//com.google.android.gms.games.RealTimeSocket
  JGoogleMap_OnMapClickListener = interface;//com.google.android.gms.maps.GoogleMap$OnMapClickListener
  JInvitationEntityCreator = interface;//com.google.android.gms.games.multiplayer.InvitationEntityCreator
  JGameEntityCreator = interface;//com.google.android.gms.games.GameEntityCreator
  JMarkerOptions = interface;//com.google.android.gms.maps.model.MarkerOptions
  JAchievementBuffer = interface;//com.google.android.gms.games.achievement.AchievementBuffer
  JLeaderboardVariant = interface;//com.google.android.gms.games.leaderboard.LeaderboardVariant
  JUserRecoverableAuthException = interface;//com.google.android.gms.auth.UserRecoverableAuthException
  JGooglePlayServicesAvailabilityException = interface;//com.google.android.gms.auth.GooglePlayServicesAvailabilityException
  JAchievement = interface;//com.google.android.gms.games.achievement.Achievement
  JPolygonOptionsCreator = interface;//com.google.android.gms.maps.model.PolygonOptionsCreator
  JPerson_Organizations = interface;//com.google.android.gms.plus.model.people.Person$Organizations
  JScopes = interface;//com.google.android.gms.common.Scopes
  JRoomConfig_Builder = interface;//com.google.android.gms.games.multiplayer.realtime.RoomConfig$Builder
  JTileCreator = interface;//com.google.android.gms.maps.model.TileCreator
  JPolylineOptions = interface;//com.google.android.gms.maps.model.PolylineOptions
  JActivityRecognitionResult = interface;//com.google.android.gms.location.ActivityRecognitionResult
  JGoogleMap_OnMyLocationButtonClickListener = interface;//com.google.android.gms.maps.GoogleMap$OnMyLocationButtonClickListener
  JGoogleMapOptionsCreator = interface;//com.google.android.gms.maps.GoogleMapOptionsCreator
  JGoogleMap_SnapshotReadyCallback = interface;//com.google.android.gms.maps.GoogleMap$SnapshotReadyCallback
  JGooglePlayServicesNotAvailableException = interface;//com.google.android.gms.common.GooglePlayServicesNotAvailableException
  JPerson_AgeRange = interface;//com.google.android.gms.plus.model.people.Person$AgeRange
  JLeaderboard = interface;//com.google.android.gms.games.leaderboard.Leaderboard
  JInvitationEntity = interface;//com.google.android.gms.games.multiplayer.InvitationEntity
  JRealTimeMessage = interface;//com.google.android.gms.games.multiplayer.realtime.RealTimeMessage
  JInvitationBuffer = interface;//com.google.android.gms.games.multiplayer.InvitationBuffer
  JLocationSource = interface;//com.google.android.gms.maps.LocationSource
  JGoogleMap = interface;//com.google.android.gms.maps.GoogleMap
  JOnScoreSubmittedListener = interface;//com.google.android.gms.games.leaderboard.OnScoreSubmittedListener
  JPlusShare = interface;//com.google.android.gms.plus.PlusShare
  JParticipantUtils = interface;//com.google.android.gms.games.multiplayer.ParticipantUtils
  JItemScope_Builder = interface;//com.google.android.gms.plus.model.moments.ItemScope$Builder
  JPerson_Cover = interface;//com.google.android.gms.plus.model.people.Person$Cover
  JOnSignOutCompleteListener = interface;//com.google.android.gms.appstate.OnSignOutCompleteListener
  JOnLeaderboardMetadataLoadedListener = interface;//com.google.android.gms.games.leaderboard.OnLeaderboardMetadataLoadedListener
  JLocationClient_OnRemoveGeofencesResultListener = interface;//com.google.android.gms.location.LocationClient$OnRemoveGeofencesResultListener
  JGeofence_Builder = interface;//com.google.android.gms.location.Geofence$Builder
  JPanoramaClient = interface;//com.google.android.gms.panorama.PanoramaClient
  JTileProvider = interface;//com.google.android.gms.maps.model.TileProvider
  JIGoogleMapDelegate_a = interface;//com.google.android.gms.maps.internal.IGoogleMapDelegate$a
  JGooglePlayServicesClient = interface;//com.google.android.gms.common.GooglePlayServicesClient
  JPlusShare_Builder = interface;//com.google.android.gms.plus.PlusShare$Builder
  JMoment = interface;//com.google.android.gms.plus.model.moments.Moment
  JPersonBuffer = interface;//com.google.android.gms.plus.model.people.PersonBuffer
  JMarker = interface;//com.google.android.gms.maps.model.Marker
  JMapFragment = interface;//com.google.android.gms.maps.MapFragment
  JBitmapDescriptor = interface;//com.google.android.gms.maps.model.BitmapDescriptor
  JMarkerOptionsCreator = interface;//com.google.android.gms.maps.model.MarkerOptionsCreator
  JOnGamesLoadedListener = interface;//com.google.android.gms.games.OnGamesLoadedListener
  JAppState = interface;//com.google.android.gms.appstate.AppState
  JPolygon = interface;//com.google.android.gms.maps.model.Polygon
  JTile = interface;//com.google.android.gms.maps.model.Tile
  JDetectedActivity = interface;//com.google.android.gms.location.DetectedActivity
  JRoomStatusUpdateListener = interface;//com.google.android.gms.games.multiplayer.realtime.RoomStatusUpdateListener
  JGoogleMap_CancelableCallback = interface;//com.google.android.gms.maps.GoogleMap$CancelableCallback
  JPlusClient_OnMomentsLoadedListener = interface;//com.google.android.gms.plus.PlusClient$OnMomentsLoadedListener
  JPlusClient = interface;//com.google.android.gms.plus.PlusClient
  JLatLngBounds = interface;//com.google.android.gms.maps.model.LatLngBounds
  JICameraUpdateFactoryDelegate = interface;//com.google.android.gms.maps.internal.ICameraUpdateFactoryDelegate
  Jlocation_LocationListener = interface;//com.google.android.gms.location.LocationListener
  JIMapFragmentDelegate = interface;//com.google.android.gms.maps.internal.IMapFragmentDelegate
  JUserRecoverableNotifiedException = interface;//com.google.android.gms.auth.UserRecoverableNotifiedException
  JParticipatable = interface;//com.google.android.gms.games.multiplayer.Participatable
  JGoogleMap_OnMarkerClickListener = interface;//com.google.android.gms.maps.GoogleMap$OnMarkerClickListener
  JGroundOverlayOptionsCreator = interface;//com.google.android.gms.maps.model.GroundOverlayOptionsCreator
  JIPolylineDelegate = interface;//com.google.android.gms.maps.model.internal.IPolylineDelegate
  JPlusClient_OrderBy = interface;//com.google.android.gms.plus.PlusClient$OrderBy
  JOnAchievementUpdatedListener = interface;//com.google.android.gms.games.achievement.OnAchievementUpdatedListener
  JOnInvitationsLoadedListener = interface;//com.google.android.gms.games.multiplayer.OnInvitationsLoadedListener
  JIMapViewDelegate = interface;//com.google.android.gms.maps.internal.IMapViewDelegate
  JGoogleAuthUtil = interface;//com.google.android.gms.auth.GoogleAuthUtil
  JLatLngBoundsCreator = interface;//com.google.android.gms.maps.model.LatLngBoundsCreator
  JSupportMapFragment = interface;//com.google.android.gms.maps.SupportMapFragment
  JPerson_OrderBy = interface;//com.google.android.gms.plus.model.people.Person$OrderBy
  JGroundOverlay = interface;//com.google.android.gms.maps.model.GroundOverlay
  JPerson_RelationshipStatus = interface;//com.google.android.gms.plus.model.people.Person$RelationshipStatus
  JDetectedActivityCreator = interface;//com.google.android.gms.location.DetectedActivityCreator
  JCover_CoverPhoto = interface;//com.google.android.gms.plus.model.people.Person$Cover$CoverPhoto
  JPlusClient_OnPersonLoadedListener = interface;//com.google.android.gms.plus.PlusClient$OnPersonLoadedListener
  JPlusClient_a = interface;//com.google.android.gms.plus.PlusClient$a
  JGoogleMap_OnMyLocationChangeListener = interface;//com.google.android.gms.maps.GoogleMap$OnMyLocationChangeListener
  JLocationRequestCreator = interface;//com.google.android.gms.location.LocationRequestCreator
  JParticipantBuffer = interface;//com.google.android.gms.games.multiplayer.ParticipantBuffer
  JMapView = interface;//com.google.android.gms.maps.MapView
  JLocationClient = interface;//com.google.android.gms.location.LocationClient
  JPerson_PlacesLived = interface;//com.google.android.gms.plus.model.people.Person$PlacesLived
  JLifecycleDelegate = interface;//com.google.android.gms.dynamic.LifecycleDelegate
  JRuntimeRemoteException = interface;//com.google.android.gms.maps.model.RuntimeRemoteException
  JSubmitScoreResult_Result = interface;//com.google.android.gms.games.leaderboard.SubmitScoreResult$Result
  JConnectionResult = interface;//com.google.android.gms.common.ConnectionResult
  JSubmitScoreResult = interface;//com.google.android.gms.games.leaderboard.SubmitScoreResult
  JLeaderboardScoreBuffer = interface;//com.google.android.gms.games.leaderboard.LeaderboardScoreBuffer
  JParticipantEntityCreator = interface;//com.google.android.gms.games.multiplayer.ParticipantEntityCreator
  JILocationSourceDelegate_a = interface;//com.google.android.gms.maps.internal.ILocationSourceDelegate$a
  JCircleOptionsCreator = interface;//com.google.android.gms.maps.model.CircleOptionsCreator
  JMapsInitializer = interface;//com.google.android.gms.maps.MapsInitializer
  JGooglePlayServicesClient_ConnectionCallbacks = interface;//com.google.android.gms.common.GooglePlayServicesClient$ConnectionCallbacks
  JRoomEntityCreator = interface;//com.google.android.gms.games.multiplayer.realtime.RoomEntityCreator
  JOnPlayersLoadedListener = interface;//com.google.android.gms.games.OnPlayersLoadedListener
  JPlusClient_OnPeopleLoadedListener = interface;//com.google.android.gms.plus.PlusClient$OnPeopleLoadedListener
  JGoogleMap_OnCameraChangeListener = interface;//com.google.android.gms.maps.GoogleMap$OnCameraChangeListener
  JGoogleMap_OnInfoWindowClickListener = interface;//com.google.android.gms.maps.GoogleMap$OnInfoWindowClickListener
  JMomentBuffer = interface;//com.google.android.gms.plus.model.moments.MomentBuffer
  JPolygonOptions = interface;//com.google.android.gms.maps.model.PolygonOptions
  JAppStateBuffer = interface;//com.google.android.gms.appstate.AppStateBuffer
  JImageManager = interface;//com.google.android.gms.common.images.ImageManager
  JAccountPicker = interface;//com.google.android.gms.common.AccountPicker
  JGoogleMap_OnMapLongClickListener = interface;//com.google.android.gms.maps.GoogleMap$OnMapLongClickListener
  JCameraUpdate = interface;//com.google.android.gms.maps.CameraUpdate
  JGooglePlayServicesUtil = interface;//com.google.android.gms.common.GooglePlayServicesUtil
  JPageDirection = interface;//com.google.android.gms.games.PageDirection
  JOnStateLoadedListener = interface;//com.google.android.gms.appstate.OnStateLoadedListener
  JTileOverlayOptions = interface;//com.google.android.gms.maps.model.TileOverlayOptions
  JGamesClient = interface;//com.google.android.gms.games.GamesClient
  JIProjectionDelegate_a = interface;//com.google.android.gms.maps.internal.IProjectionDelegate$a
  JTileOverlay = interface;//com.google.android.gms.maps.model.TileOverlay
  JPlayerEntity = interface;//com.google.android.gms.games.PlayerEntity
  JRealTimeMessageReceivedListener = interface;//com.google.android.gms.games.multiplayer.realtime.RealTimeMessageReceivedListener
  JGamesActivityResultCodes = interface;//com.google.android.gms.games.GamesActivityResultCodes
  JPerson_ObjectType = interface;//com.google.android.gms.plus.model.people.Person$ObjectType
  JLatLng = interface;//com.google.android.gms.maps.model.LatLng
  JPlayerBuffer = interface;//com.google.android.gms.games.PlayerBuffer
  JProjection = interface;//com.google.android.gms.maps.Projection
  JCover_Layout = interface;//com.google.android.gms.plus.model.people.Person$Cover$Layout
  JPerson_Image = interface;//com.google.android.gms.plus.model.people.Person$Image
  JIMapFragmentDelegate_a = interface;//com.google.android.gms.maps.internal.IMapFragmentDelegate$a
  JUrlTileProvider = interface;//com.google.android.gms.maps.model.UrlTileProvider
  JRoomUpdateListener = interface;//com.google.android.gms.games.multiplayer.realtime.RoomUpdateListener

JICameraUpdateFactoryDelegate_aClass = interface(JBinderClass)
['{73E02C5D-CDD1-445D-AF17-D641CFA7BBE6}']
  {Methods}
  function init: JICameraUpdateFactoryDelegate_a; cdecl;
  function r(paramIBinder: JIBinder): JICameraUpdateFactoryDelegate; cdecl;
end;

[JavaSignature('com/google/android/gms/maps/internal/ICameraUpdateFactoryDelegate$a')]
JICameraUpdateFactoryDelegate_a = interface(JBinder)
['{3C070845-158F-4878-BFD1-E3B5C798B8DF}']
  {Methods}
  function onTransact(code: Integer; data: JParcel; reply: JParcel; flags: Integer): Boolean; cdecl;
end;
TJICameraUpdateFactoryDelegate_a = class(TJavaGenericImport<JICameraUpdateFactoryDelegate_aClass, JICameraUpdateFactoryDelegate_a>) end;

JRoomEntityClass = interface(JObjectClass)
['{DD7BE552-5612-4010-A094-1491EF21EC2D}']
  {Property Methods}
  function _GetCREATOR: JRoomEntityCreator;
  {Methods}
  function init(room: JRoom): JRoomEntity; cdecl;
  function a(paramRoom: JRoom): Integer; cdecl; overload;
  function a(paramRoom: JRoom; paramObject: JObject): Boolean; cdecl; overload;
  function b(paramRoom: JRoom): JString; cdecl;
  {Properties}
  property CREATOR: JRoomEntityCreator read _GetCREATOR;
end;

[JavaSignature('com/google/android/gms/games/multiplayer/realtime/RoomEntity')]
JRoomEntity = interface(JObject)
['{9BAEB83E-389F-452D-9BA4-6A069EAF143C}']
  {Methods}
  function F: Integer; cdecl;
  function describeContents: Integer; cdecl;
  function equals(obj: JObject): Boolean; cdecl;
  function freeze: JRoom; cdecl;
  function getAutoMatchCriteria: JBundle; cdecl;
  function getCreationTimestamp: Int64; cdecl;
  function getCreatorId: JString; cdecl;
  function getDescription: JString; cdecl; overload;
  procedure getDescription(dataOut: Jdatabase_CharArrayBuffer); cdecl; overload;
  function getParticipantId(playerId: JString): JString; cdecl;
  function getParticipantIds: JArrayList; cdecl;
  function getParticipantStatus(participantId: JString): Integer; cdecl;
  function getParticipants: JArrayList; cdecl;
  function getRoomId: JString; cdecl;
  function getStatus: Integer; cdecl;
  function getVariant: Integer; cdecl;
  function hashCode: Integer; cdecl;
  function isDataValid: Boolean; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
end;
TJRoomEntity = class(TJavaGenericImport<JRoomEntityClass, JRoomEntity>) end;

JPlayerEntityCreatorClass = interface(JObjectClass)
['{51616587-CFC5-47DF-A072-D9C2E638D55C}']
  {Property Methods}
  function _GetCONTENT_DESCRIPTION: Integer;
  {Methods}
  function init: JPlayerEntityCreator; cdecl;
  {Properties}
  property CONTENT_DESCRIPTION: Integer read _GetCONTENT_DESCRIPTION;
end;

[JavaSignature('com/google/android/gms/games/PlayerEntityCreator')]
JPlayerEntityCreator = interface(JObject)
['{40BC89AE-6768-4E93-B168-789CB60473B8}']
  {Methods}
  function createFromParcel(parcel: JParcel): JPlayerEntity; cdecl;
  function newArray(size: Integer): TJavaObjectArray<JPlayerEntity>; cdecl;
end;
TJPlayerEntityCreator = class(TJavaGenericImport<JPlayerEntityCreatorClass, JPlayerEntityCreator>) end;

JGeofenceClass = interface(IJavaClass)
['{CF428A28-2F37-48E7-9D9F-C67737813FE0}']
  {Property Methods}
  function _GetGEOFENCE_TRANSITION_ENTER: Integer;
  function _GetGEOFENCE_TRANSITION_EXIT: Integer;
  function _GetNEVER_EXPIRE: Int64;
  {Properties}
  property GEOFENCE_TRANSITION_ENTER: Integer read _GetGEOFENCE_TRANSITION_ENTER;
  property GEOFENCE_TRANSITION_EXIT: Integer read _GetGEOFENCE_TRANSITION_EXIT;
  property NEVER_EXPIRE: Int64 read _GetNEVER_EXPIRE;
end;

[JavaSignature('com/google/android/gms/location/Geofence')]
JGeofence = interface(IJavaInstance)
['{03F411F4-AF94-4BCE-B572-036C56891F2B}']
  {Methods}
  function getRequestId: JString; cdecl;
end;
TJGeofence = class(TJavaGenericImport<JGeofenceClass, JGeofence>) end;

JImageManager_OnImageLoadedListenerClass = interface(IJavaClass)
['{4A5F4833-04FC-4164-99F6-96619A719D1F}']
end;

[JavaSignature('com/google/android/gms/common/images/ImageManager$OnImageLoadedListener')]
JImageManager_OnImageLoadedListener = interface(IJavaInstance)
['{332D053E-9E93-4340-B0D6-BEA18CF1E0FF}']
  {Methods}
  procedure onImageLoaded(paramUri: Jnet_Uri; paramDrawable: JDrawable); cdecl;
end;
TJImageManager_OnImageLoadedListener = class(TJavaGenericImport<JImageManager_OnImageLoadedListenerClass, JImageManager_OnImageLoadedListener>) end;

JIUiSettingsDelegateClass = interface(JIInterfaceClass)
['{6B0678EE-DEF6-4D18-B5A4-32AE60895ABA}']
end;

[JavaSignature('com/google/android/gms/maps/internal/IUiSettingsDelegate')]
JIUiSettingsDelegate = interface(JIInterface)
['{D43A9F19-A008-4E96-9779-009F6373AC2D}']
  {Methods}
  function isCompassEnabled: Boolean; cdecl;
  function isMyLocationButtonEnabled: Boolean; cdecl;
  function isRotateGesturesEnabled: Boolean; cdecl;
  function isScrollGesturesEnabled: Boolean; cdecl;
  function isTiltGesturesEnabled: Boolean; cdecl;
  function isZoomControlsEnabled: Boolean; cdecl;
  function isZoomGesturesEnabled: Boolean; cdecl;
  procedure setAllGesturesEnabled(paramBoolean: Boolean); cdecl;
  procedure setCompassEnabled(paramBoolean: Boolean); cdecl;
  procedure setMyLocationButtonEnabled(paramBoolean: Boolean); cdecl;
  procedure setRotateGesturesEnabled(paramBoolean: Boolean); cdecl;
  procedure setScrollGesturesEnabled(paramBoolean: Boolean); cdecl;
  procedure setTiltGesturesEnabled(paramBoolean: Boolean); cdecl;
  procedure setZoomControlsEnabled(paramBoolean: Boolean); cdecl;
  procedure setZoomGesturesEnabled(paramBoolean: Boolean); cdecl;
end;
TJIUiSettingsDelegate = class(TJavaGenericImport<JIUiSettingsDelegateClass, JIUiSettingsDelegate>) end;

JOrganizations_TypeClass = interface(JObjectClass)
['{F3B7E31B-1878-4F86-BDF0-14672C1CB488}']
  {Property Methods}
  function _GetSCHOOL: Integer;
  function _GetWORK: Integer;
  {Methods}
  function init: JOrganizations_Type; cdecl;
  {Properties}
  property SCHOOL: Integer read _GetSCHOOL;
  property WORK: Integer read _GetWORK;
end;

[JavaSignature('com/google/android/gms/plus/model/people/Person$Organizations$Type')]
JOrganizations_Type = interface(JObject)
['{D2D199DB-8AE7-41D8-BE84-4BFA75045008}']
end;
TJOrganizations_Type = class(TJavaGenericImport<JOrganizations_TypeClass, JOrganizations_Type>) end;

JPolylineClass = interface(JObjectClass)
['{E56467A2-5381-457A-B96F-E67567F6FABB}']
  {Methods}
  function init(delegate: JIPolylineDelegate): JPolyline; cdecl;
end;

[JavaSignature('com/google/android/gms/maps/model/Polyline')]
JPolyline = interface(JObject)
['{803D778F-39F8-4FF2-B9E5-8EC0BD513133}']
  {Methods}
  function equals(other: JObject): Boolean; cdecl;
  function getColor: Integer; cdecl;
  function getId: JString; cdecl;
  function getPoints: JList; cdecl;
  function getWidth: Single; cdecl;
  function getZIndex: Single; cdecl;
  function hashCode: Integer; cdecl;
  function isGeodesic: Boolean; cdecl;
  function isVisible: Boolean; cdecl;
  procedure remove; cdecl;
  procedure setColor(color: Integer); cdecl;
  procedure setGeodesic(geodesic: Boolean); cdecl;
  procedure setPoints(points: JList); cdecl;
  procedure setVisible(visible: Boolean); cdecl;
  procedure setWidth(width: Single); cdecl;
  procedure setZIndex(zIndex: Single); cdecl;
end;
TJPolyline = class(TJavaGenericImport<JPolylineClass, JPolyline>) end;

JPanoramaClient_OnPanoramaInfoLoadedListenerClass = interface(IJavaClass)
['{98EE122A-4A2E-4086-947A-CAE0E786F3DC}']
end;

[JavaSignature('com/google/android/gms/panorama/PanoramaClient$OnPanoramaInfoLoadedListener')]
JPanoramaClient_OnPanoramaInfoLoadedListener = interface(IJavaInstance)
['{2EEE0D0D-9E71-4292-B757-2B5A8DC23C92}']
  {Methods}
  procedure onPanoramaInfoLoaded(paramConnectionResult: JConnectionResult; paramIntent: JIntent); cdecl;
end;
TJPanoramaClient_OnPanoramaInfoLoadedListener = class(TJavaGenericImport<JPanoramaClient_OnPanoramaInfoLoadedListenerClass, JPanoramaClient_OnPanoramaInfoLoadedListener>) end;

JUrls_TypeClass = interface(JObjectClass)
['{FCDE35BA-BA1A-4AAB-A7B8-BA4BA79C6116}']
  {Property Methods}
  function _GetBLOG: Integer;
  function _GetCONTRIBUTOR: Integer;
  function _GetHOME: Integer;
  function _GetOTHER: Integer;
  function _GetOTHER_PROFILE: Integer;
  function _GetPROFILE: Integer;
  function _GetWEBSITE: Integer;
  function _GetWORK: Integer;
  {Methods}
  function init: JUrls_Type; cdecl;
  {Properties}
  property BLOG: Integer read _GetBLOG;
  property CONTRIBUTOR: Integer read _GetCONTRIBUTOR;
  property HOME: Integer read _GetHOME;
  property OTHER: Integer read _GetOTHER;
  property OTHER_PROFILE: Integer read _GetOTHER_PROFILE;
  property PROFILE: Integer read _GetPROFILE;
  property WEBSITE: Integer read _GetWEBSITE;
  property WORK: Integer read _GetWORK;
end;

[JavaSignature('com/google/android/gms/plus/model/people/Person$Urls$Type')]
JUrls_Type = interface(JObject)
['{B7224F92-BC58-4572-88F0-8AA7266F7B88}']
end;
TJUrls_Type = class(TJavaGenericImport<JUrls_TypeClass, JUrls_Type>) end;

JActivityRecognitionResultCreatorClass = interface(JObjectClass)
['{6A3F9406-3D43-411E-8C6B-9EFD51DAD599}']
  {Property Methods}
  function _GetCONTENT_DESCRIPTION: Integer;
  {Methods}
  function init: JActivityRecognitionResultCreator; cdecl;
  {Properties}
  property CONTENT_DESCRIPTION: Integer read _GetCONTENT_DESCRIPTION;
end;

[JavaSignature('com/google/android/gms/location/ActivityRecognitionResultCreator')]
JActivityRecognitionResultCreator = interface(JObject)
['{0CD362C5-4246-4771-9F67-0079A2D4889A}']
  {Methods}
  function createFromParcel(parcel: JParcel): JActivityRecognitionResult; cdecl;
  function newArray(size: Integer): TJavaObjectArray<JActivityRecognitionResult>; cdecl;
end;
TJActivityRecognitionResultCreator = class(TJavaGenericImport<JActivityRecognitionResultCreatorClass, JActivityRecognitionResultCreator>) end;

JCameraPosition_BuilderClass = interface(JObjectClass)
['{84517F2E-069C-457D-B297-C29BCEA99D39}']
  {Methods}
  function init: JCameraPosition_Builder; cdecl; overload;
  function init(previous: JCameraPosition): JCameraPosition_Builder; cdecl; overload;
end;

[JavaSignature('com/google/android/gms/maps/model/CameraPosition$Builder')]
JCameraPosition_Builder = interface(JObject)
['{10539B19-24B1-414A-A8C9-09FF64A805A1}']
  {Methods}
  function bearing(bearing: Single): JCameraPosition_Builder; cdecl;
  function build: JCameraPosition; cdecl;
  function target(location: JLatLng): JCameraPosition_Builder; cdecl;
  function tilt(tilt: Single): JCameraPosition_Builder; cdecl;
  function zoom(zoom: Single): JCameraPosition_Builder; cdecl;
end;
TJCameraPosition_Builder = class(TJavaGenericImport<JCameraPosition_BuilderClass, JCameraPosition_Builder>) end;

JOnStateDeletedListenerClass = interface(IJavaClass)
['{24C55881-93FA-4197-8908-F950CAEB591D}']
end;

[JavaSignature('com/google/android/gms/appstate/OnStateDeletedListener')]
JOnStateDeletedListener = interface(IJavaInstance)
['{AF51B33D-A93B-48AC-BB0D-827C8334E2CE}']
  {Methods}
  procedure onStateDeleted(paramInt1: Integer; paramInt2: Integer); cdecl;
end;
TJOnStateDeletedListener = class(TJavaGenericImport<JOnStateDeletedListenerClass, JOnStateDeletedListener>) end;

JPlusClient_BuilderClass = interface(JObjectClass)
['{DBA13DC0-04DE-4A35-9287-C39E7D157EE6}']
  {Methods}
  function init(context: JContext; connectionCallbacks: JGooglePlayServicesClient_ConnectionCallbacks; connectionFailedListener: JGooglePlayServicesClient_OnConnectionFailedListener): JPlusClient_Builder; cdecl;
end;

[JavaSignature('com/google/android/gms/plus/PlusClient$Builder')]
JPlusClient_Builder = interface(JObject)
['{29ED300A-FFB2-4DB9-8DED-319CABCE900B}']
  {Methods}
  function build: JPlusClient; cdecl;
  function clearScopes: JPlusClient_Builder; cdecl;
  function setAccountName(accountName: JString): JPlusClient_Builder; cdecl;
  function setScopes(scopes: TJavaObjectArray<JString>): JPlusClient_Builder; cdecl;
  function setVisibleActivities(visibleActivities: TJavaObjectArray<JString>): JPlusClient_Builder; cdecl;
end;
TJPlusClient_Builder = class(TJavaGenericImport<JPlusClient_BuilderClass, JPlusClient_Builder>) end;

JOnInvitationReceivedListenerClass = interface(IJavaClass)
['{124C239D-5B88-4ED3-911D-F45488ECCB02}']
end;

[JavaSignature('com/google/android/gms/games/multiplayer/OnInvitationReceivedListener')]
JOnInvitationReceivedListener = interface(IJavaInstance)
['{6860A363-DEA1-45A1-99C6-328598B0AE2A}']
  {Methods}
  procedure onInvitationReceived(paramInvitation: JInvitation); cdecl;
end;
TJOnInvitationReceivedListener = class(TJavaGenericImport<JOnInvitationReceivedListenerClass, JOnInvitationReceivedListener>) end;

JPerson_CollectionClass = interface(IJavaClass)
['{076B9EBC-2C08-4664-895E-DAF89C072FE2}']
  {Property Methods}
  function _GetVISIBLE: Integer;
  {Properties}
  property VISIBLE: Integer read _GetVISIBLE;
end;

[JavaSignature('com/google/android/gms/plus/model/people/Person$Collection')]
JPerson_Collection = interface(IJavaInstance)
['{02FC2F9D-5E92-4D92-88F4-ACBA8EB862D1}']
end;
TJPerson_Collection = class(TJavaGenericImport<JPerson_CollectionClass, JPerson_Collection>) end;

JDataBufferUtilsClass = interface(JObjectClass)
['{A6F05052-1AD4-4A5F-9581-E6CD07AE5CA0}']
  {Methods}
  function init: JDataBufferUtils; cdecl;
  function freezeAndClose(buffer: JDataBuffer): JArrayList; cdecl;
end;

[JavaSignature('com/google/android/gms/common/data/DataBufferUtils')]
JDataBufferUtils = interface(JObject)
['{0E0D0AA8-DA82-4041-83E7-F5D1BC7E12F1}']
end;
TJDataBufferUtils = class(TJavaGenericImport<JDataBufferUtilsClass, JDataBufferUtils>) end;

JCameraPositionClass = interface(JObjectClass)
['{BBC26A98-4E32-43A4-A67C-B0FF586E1071}']
  {Property Methods}
  function _GetCREATOR: JCameraPositionCreator;
  {Methods}
  function init(target: JLatLng; zoom: Single; tilt: Single; bearing: Single): JCameraPosition; cdecl;
  function builder: JCameraPosition_Builder; cdecl; overload;
  function builder(camera: JCameraPosition): JCameraPosition_Builder; cdecl; overload;
  function createFromAttributes(context: JContext; attrs: JAttributeSet): JCameraPosition; cdecl;
  function fromLatLngZoom(target: JLatLng; zoom: Single): JCameraPosition; cdecl;
  {Properties}
  property CREATOR: JCameraPositionCreator read _GetCREATOR;
end;

[JavaSignature('com/google/android/gms/maps/model/CameraPosition')]
JCameraPosition = interface(JObject)
['{CBD6FBEA-582B-473B-959B-C7EB300574A2}']
  {Property Methods}
  function _Getbearing: Single;
  function _Gettarget: JLatLng;
  function _Gettilt: Single;
  function _Getzoom: Single;
  {Methods}
  function F: Integer; cdecl;
  function describeContents: Integer; cdecl;
  function equals(o: JObject): Boolean; cdecl;
  function hashCode: Integer; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
  {Properties}
  property bearing: Single read _Getbearing;
  property target: JLatLng read _Gettarget;
  property tilt: Single read _Gettilt;
  property zoom: Single read _Getzoom;
end;
TJCameraPosition = class(TJavaGenericImport<JCameraPositionClass, JCameraPosition>) end;

JAppStateClient_BuilderClass = interface(JObjectClass)
['{EDA9C2BB-0AB4-415E-B809-6EEDAC289D74}']
  {Methods}
  function init(context: JContext; connectedListener: JGooglePlayServicesClient_ConnectionCallbacks; connectionFailedListener: JGooglePlayServicesClient_OnConnectionFailedListener): JAppStateClient_Builder; cdecl;
end;

[JavaSignature('com/google/android/gms/appstate/AppStateClient$Builder')]
JAppStateClient_Builder = interface(JObject)
['{7FDFEE12-FCB9-4EC7-BEA3-4C556361E924}']
  {Methods}
  function create: JAppStateClient; cdecl;
  function setAccountName(accountName: JString): JAppStateClient_Builder; cdecl;
  function setScopes(scopes: TJavaObjectArray<JString>): JAppStateClient_Builder; cdecl;
end;
TJAppStateClient_Builder = class(TJavaGenericImport<JAppStateClient_BuilderClass, JAppStateClient_Builder>) end;

JPlusOneButtonClass = interface(JViewGroupClass)
['{3657EE37-DA35-4156-BE68-79159FB480AD}']
  {Property Methods}
  function _GetANNOTATION_BUBBLE: Integer;
  function _GetANNOTATION_INLINE: Integer;
  function _GetANNOTATION_NONE: Integer;
  function _GetSIZE_MEDIUM: Integer;
  function _GetSIZE_SMALL: Integer;
  function _GetSIZE_STANDARD: Integer;
  function _GetSIZE_TALL: Integer;
  {Methods}
  function init(context: JContext): JPlusOneButton; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet): JPlusOneButton; cdecl; overload;
  {Properties}
  property ANNOTATION_BUBBLE: Integer read _GetANNOTATION_BUBBLE;
  property ANNOTATION_INLINE: Integer read _GetANNOTATION_INLINE;
  property ANNOTATION_NONE: Integer read _GetANNOTATION_NONE;
  property SIZE_MEDIUM: Integer read _GetSIZE_MEDIUM;
  property SIZE_SMALL: Integer read _GetSIZE_SMALL;
  property SIZE_STANDARD: Integer read _GetSIZE_STANDARD;
  property SIZE_TALL: Integer read _GetSIZE_TALL;
end;

[JavaSignature('com/google/android/gms/plus/PlusOneButton')]
JPlusOneButton = interface(JViewGroup)
['{673E8BED-682D-4DD7-AFE5-24B67EB89D9E}']
  {Methods}
  procedure initialize(plusClient: JPlusClient; url: JString; activityRequestCode: Integer); cdecl; overload;
  procedure initialize(plusClient: JPlusClient; url: JString; plusOneClickListener: JPlusOneButton_OnPlusOneClickListener); cdecl; overload;
  procedure setAnnotation(annotation: Integer); cdecl;
  procedure setOnPlusOneClickListener(listener: JPlusOneButton_OnPlusOneClickListener); cdecl;
  procedure setSize(size: Integer); cdecl;
end;
TJPlusOneButton = class(TJavaGenericImport<JPlusOneButtonClass, JPlusOneButton>) end;

JFreezableClass = interface(IJavaClass)
['{7F96F79F-1482-4695-8F2F-F9B3ECD4D9C5}']
end;

[JavaSignature('com/google/android/gms/common/data/Freezable')]
JFreezable = interface(IJavaInstance)
['{B39CBE77-7C0F-4FD9-BA32-ADA3BC739705}']
  {Methods}
  function freeze: JObject; cdecl;
  function isDataValid: Boolean; cdecl;
end;
TJFreezable = class(TJavaGenericImport<JFreezableClass, JFreezable>) end;

JInvitationClass = interface(JFreezableClass)
['{14EB41F9-050E-4B83-B80C-BE0DAC596C38}']
end;

[JavaSignature('com/google/android/gms/games/multiplayer/Invitation')]
JInvitation = interface(JFreezable)
['{2F9236D2-0094-44BE-A00A-41D28EB1C749}']
  {Methods}
  function aJ: Integer; cdecl;
  function getCreationTimestamp: Int64; cdecl;
  function getGame: JGame; cdecl;
  function getInvitationId: JString; cdecl;
  function getInviter: JParticipant; cdecl;
  function getVariant: Integer; cdecl;
end;
TJInvitation = class(TJavaGenericImport<JInvitationClass, JInvitation>) end;

JEmails_TypeClass = interface(JObjectClass)
['{5892F9B6-716C-4154-B327-1D9037CC84BE}']
  {Property Methods}
  function _GetHOME: Integer;
  function _GetOTHER: Integer;
  function _GetWORK: Integer;
  {Methods}
  function init: JEmails_Type; cdecl;
  {Properties}
  property HOME: Integer read _GetHOME;
  property OTHER: Integer read _GetOTHER;
  property WORK: Integer read _GetWORK;
end;

[JavaSignature('com/google/android/gms/plus/model/people/Person$Emails$Type')]
JEmails_Type = interface(JObject)
['{499F67BD-7C17-48B4-9020-2F584FBCA038}']
end;
TJEmails_Type = class(TJavaGenericImport<JEmails_TypeClass, JEmails_Type>) end;

JCameraPositionCreatorClass = interface(JObjectClass)
['{04C0775C-11D5-402C-B7CA-E1AB6C45E868}']
  {Property Methods}
  function _GetCONTENT_DESCRIPTION: Integer;
  {Methods}
  function init: JCameraPositionCreator; cdecl;
  {Properties}
  property CONTENT_DESCRIPTION: Integer read _GetCONTENT_DESCRIPTION;
end;

[JavaSignature('com/google/android/gms/maps/model/CameraPositionCreator')]
JCameraPositionCreator = interface(JObject)
['{F0A53DA0-6E24-48D0-BA90-2D15862EAE2C}']
  {Methods}
  function createFromParcel(parcel: JParcel): JCameraPosition; cdecl;
  function newArray(size: Integer): TJavaObjectArray<JCameraPosition>; cdecl;
end;
TJCameraPositionCreator = class(TJavaGenericImport<JCameraPositionCreatorClass, JCameraPositionCreator>) end;

JRoomClass = interface(JFreezableClass)
['{A93A517C-84FF-4021-929F-41332F02AA6B}']
  {Property Methods}
  function _GetROOM_STATUS_ACTIVE: Integer;
  function _GetROOM_STATUS_AUTO_MATCHING: Integer;
  function _GetROOM_STATUS_CONNECTING: Integer;
  function _GetROOM_STATUS_INVITING: Integer;
  function _GetROOM_VARIANT_ANY: Integer;
  {Properties}
  property ROOM_STATUS_ACTIVE: Integer read _GetROOM_STATUS_ACTIVE;
  property ROOM_STATUS_AUTO_MATCHING: Integer read _GetROOM_STATUS_AUTO_MATCHING;
  property ROOM_STATUS_CONNECTING: Integer read _GetROOM_STATUS_CONNECTING;
  property ROOM_STATUS_INVITING: Integer read _GetROOM_STATUS_INVITING;
  property ROOM_VARIANT_ANY: Integer read _GetROOM_VARIANT_ANY;
end;

[JavaSignature('com/google/android/gms/games/multiplayer/realtime/Room')]
JRoom = interface(JFreezable)
['{2C389670-651B-424F-88E5-93850089A77C}']
  {Methods}
  function getAutoMatchCriteria: JBundle; cdecl;
  function getCreationTimestamp: Int64; cdecl;
  function getCreatorId: JString; cdecl;
  function getDescription: JString; cdecl; overload;
  procedure getDescription(paramCharArrayBuffer: Jdatabase_CharArrayBuffer); cdecl; overload;
  function getParticipantId(paramString: JString): JString; cdecl;
  function getParticipantIds: JArrayList; cdecl;
  function getParticipantStatus(paramString: JString): Integer; cdecl;
  function getRoomId: JString; cdecl;
  function getStatus: Integer; cdecl;
  function getVariant: Integer; cdecl;
end;
TJRoom = class(TJavaGenericImport<JRoomClass, JRoom>) end;

JLocationRequestClass = interface(JObjectClass)
['{05119402-67C2-4B28-9127-852977268AC1}']
  {Property Methods}
  function _GetCREATOR: JLocationRequestCreator;
  function _GetPRIORITY_BALANCED_POWER_ACCURACY: Integer;
  function _GetPRIORITY_HIGH_ACCURACY: Integer;
  function _GetPRIORITY_LOW_POWER: Integer;
  function _GetPRIORITY_NO_POWER: Integer;
  {Methods}
  function init: JLocationRequest; cdecl;
  function I(paramInt: Integer): JString; cdecl;
  function create: JLocationRequest; cdecl;
  {Properties}
  property CREATOR: JLocationRequestCreator read _GetCREATOR;
  property PRIORITY_BALANCED_POWER_ACCURACY: Integer read _GetPRIORITY_BALANCED_POWER_ACCURACY;
  property PRIORITY_HIGH_ACCURACY: Integer read _GetPRIORITY_HIGH_ACCURACY;
  property PRIORITY_LOW_POWER: Integer read _GetPRIORITY_LOW_POWER;
  property PRIORITY_NO_POWER: Integer read _GetPRIORITY_NO_POWER;
end;

[JavaSignature('com/google/android/gms/location/LocationRequest')]
JLocationRequest = interface(JObject)
['{C5E84DA4-6E65-4EBC-85C2-E084C59E736B}']
  {Methods}
  function describeContents: Integer; cdecl;
  function equals(object_: JObject): Boolean; cdecl;
  function getExpirationTime: Int64; cdecl;
  function getFastestInterval: Int64; cdecl;
  function getInterval: Int64; cdecl;
  function getNumUpdates: Integer; cdecl;
  function getPriority: Integer; cdecl;
  function getSmallestDisplacement: Single; cdecl;
  function hashCode: Integer; cdecl;
  function setExpirationDuration(millis: Int64): JLocationRequest; cdecl;
  function setExpirationTime(millis: Int64): JLocationRequest; cdecl;
  function setFastestInterval(millis: Int64): JLocationRequest; cdecl;
  function setInterval(millis: Int64): JLocationRequest; cdecl;
  function setNumUpdates(numUpdates: Integer): JLocationRequest; cdecl;
  function setPriority(priority: Integer): JLocationRequest; cdecl;
  function setSmallestDisplacement(smallestDisplacementMeters: Single): JLocationRequest; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(parcel: JParcel; flags: Integer); cdecl;
end;
TJLocationRequest = class(TJavaGenericImport<JLocationRequestClass, JLocationRequest>) end;

JIUiSettingsDelegate_aClass = interface(JBinderClass)
['{4FED08FF-0EA2-4FBB-81A9-FB51E26EAE9B}']
  {Methods}
  function init: JIUiSettingsDelegate_a; cdecl;
  function K(paramIBinder: JIBinder): JIUiSettingsDelegate; cdecl;
end;

[JavaSignature('com/google/android/gms/maps/internal/IUiSettingsDelegate$a')]
JIUiSettingsDelegate_a = interface(JBinder)
['{97B9E7A1-84D7-4067-9756-EBFCB1D3A7B9}']
  {Methods}
  function onTransact(code: Integer; data: JParcel; reply: JParcel; flags: Integer): Boolean; cdecl;
end;
TJIUiSettingsDelegate_a = class(TJavaGenericImport<JIUiSettingsDelegate_aClass, JIUiSettingsDelegate_a>) end;

JPerson_UrlsClass = interface(JFreezableClass)
['{F4F0CAD5-A2F2-4749-BA4B-294B87D8827A}']
end;

[JavaSignature('com/google/android/gms/plus/model/people/Person$Urls')]
JPerson_Urls = interface(JFreezable)
['{75FAAEDA-782C-46FA-9A33-33C1D2CE45CB}']
  {Methods}
  function getLabel: JString; cdecl;
  function getType: Integer; cdecl;
  function getValue: JString; cdecl;
  function hasLabel: Boolean; cdecl;
  function hasPrimary: Boolean; cdecl;
  function hasType: Boolean; cdecl;
  function hasValue: Boolean; cdecl;
  function isPrimary: Boolean; cdecl;
end;
TJPerson_Urls = class(TJavaGenericImport<JPerson_UrlsClass, JPerson_Urls>) end;

JPerson_NameClass = interface(JFreezableClass)
['{1F3419CD-2F77-4055-9DF0-19AF842C36F3}']
end;

[JavaSignature('com/google/android/gms/plus/model/people/Person$Name')]
JPerson_Name = interface(JFreezable)
['{1524AC78-0D17-4E1F-9374-6DAE864F2F3D}']
  {Methods}
  function getFamilyName: JString; cdecl;
  function getFormatted: JString; cdecl;
  function getGivenName: JString; cdecl;
  function getHonorificPrefix: JString; cdecl;
  function getHonorificSuffix: JString; cdecl;
  function getMiddleName: JString; cdecl;
  function hasFamilyName: Boolean; cdecl;
  function hasFormatted: Boolean; cdecl;
  function hasGivenName: Boolean; cdecl;
  function hasHonorificPrefix: Boolean; cdecl;
  function hasHonorificSuffix: Boolean; cdecl;
  function hasMiddleName: Boolean; cdecl;
end;
TJPerson_Name = class(TJavaGenericImport<JPerson_NameClass, JPerson_Name>) end;

JLatLngBounds_BuilderClass = interface(JObjectClass)
['{FD670806-8BF8-4499-A135-6A46A4905B7C}']
  {Methods}
  function init: JLatLngBounds_Builder; cdecl;
end;

[JavaSignature('com/google/android/gms/maps/model/LatLngBounds$Builder')]
JLatLngBounds_Builder = interface(JObject)
['{BC10B895-4AAC-4B39-9F7A-207B7D80DFE4}']
  {Methods}
  function build: JLatLngBounds; cdecl;
  function include(point: JLatLng): JLatLngBounds_Builder; cdecl;
end;
TJLatLngBounds_Builder = class(TJavaGenericImport<JLatLngBounds_BuilderClass, JLatLngBounds_Builder>) end;

JGameEntityClass = interface(JObjectClass)
['{835EE6B2-7BD0-44A8-A1E7-3708D7584E7F}']
  {Property Methods}
  function _GetCREATOR: JGameEntityCreator;
  {Methods}
  function init(game: JGame): JGameEntity; cdecl;
  function a(paramGame: JGame): Integer; cdecl; overload;
  function a(paramGame: JGame; paramObject: JObject): Boolean; cdecl; overload;
  function b(paramGame: JGame): JString; cdecl;
  {Properties}
  property CREATOR: JGameEntityCreator read _GetCREATOR;
end;

[JavaSignature('com/google/android/gms/games/GameEntity')]
JGameEntity = interface(JObject)
['{02EEA614-C865-445F-AE4E-F4F9E6994329}']
  {Methods}
  function F: Integer; cdecl;
  function describeContents: Integer; cdecl;
  function equals(obj: JObject): Boolean; cdecl;
  function freeze: JGame; cdecl;
  function getAchievementTotalCount: Integer; cdecl;
  function getApplicationId: JString; cdecl;
  function getDescription: JString; cdecl; overload;
  procedure getDescription(dataOut: Jdatabase_CharArrayBuffer); cdecl; overload;
  function getDeveloperName: JString; cdecl; overload;
  procedure getDeveloperName(dataOut: Jdatabase_CharArrayBuffer); cdecl; overload;
  function getDisplayName: JString; cdecl; overload;
  procedure getDisplayName(dataOut: Jdatabase_CharArrayBuffer); cdecl; overload;
  function getFeaturedImageUri: Jnet_Uri; cdecl;
  function getGameplayAclStatus: Integer; cdecl;
  function getHiResImageUri: Jnet_Uri; cdecl;
  function getIconImageUri: Jnet_Uri; cdecl;
  function getInstancePackageName: JString; cdecl;
  function getLeaderboardCount: Integer; cdecl;
  function getPrimaryCategory: JString; cdecl;
  function getSecondaryCategory: JString; cdecl;
  function hashCode: Integer; cdecl;
  function isDataValid: Boolean; cdecl;
  function isInstanceInstalled: Boolean; cdecl;
  function isPlayEnabledGame: Boolean; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
end;
TJGameEntity = class(TJavaGenericImport<JGameEntityClass, JGameEntity>) end;

JGroundOverlayOptionsClass = interface(JObjectClass)
['{BAAAFFA3-3D94-421F-A2A0-BAFEC6D6DB68}']
  {Property Methods}
  function _GetCREATOR: JGroundOverlayOptionsCreator;
  function _GetNO_DIMENSION: Single;
  {Methods}
  function init: JGroundOverlayOptions; cdecl;
  {Properties}
  property CREATOR: JGroundOverlayOptionsCreator read _GetCREATOR;
  property NO_DIMENSION: Single read _GetNO_DIMENSION;
end;

[JavaSignature('com/google/android/gms/maps/model/GroundOverlayOptions')]
JGroundOverlayOptions = interface(JObject)
['{9109D62E-A7BA-44ED-9BB3-9460A31F3B94}']
  {Methods}
  function F: Integer; cdecl;
  function anchor(u: Single; v: Single): JGroundOverlayOptions; cdecl;
  function bearing(bearing: Single): JGroundOverlayOptions; cdecl;
  function bo: JIBinder; cdecl;
  function describeContents: Integer; cdecl;
  function getAnchorU: Single; cdecl;
  function getAnchorV: Single; cdecl;
  function getBearing: Single; cdecl;
  function getBounds: JLatLngBounds; cdecl;
  function getHeight: Single; cdecl;
  function getImage: JBitmapDescriptor; cdecl;
  function getLocation: JLatLng; cdecl;
  function getTransparency: Single; cdecl;
  function getWidth: Single; cdecl;
  function getZIndex: Single; cdecl;
  function image(image: JBitmapDescriptor): JGroundOverlayOptions; cdecl;
  function isVisible: Boolean; cdecl;
  function position(location: JLatLng; width: Single): JGroundOverlayOptions; cdecl; overload;
  function position(location: JLatLng; width: Single; height: Single): JGroundOverlayOptions; cdecl; overload;
  function positionFromBounds(bounds: JLatLngBounds): JGroundOverlayOptions; cdecl;
  function transparency(transparency: Single): JGroundOverlayOptions; cdecl;
  function visible(visible: Boolean): JGroundOverlayOptions; cdecl;
  procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
  function zIndex(zIndex: Single): JGroundOverlayOptions; cdecl;
end;
TJGroundOverlayOptions = class(TJavaGenericImport<JGroundOverlayOptionsClass, JGroundOverlayOptions>) end;

JIPolylineDelegate_aClass = interface(JBinderClass)
['{0C401F68-74CC-4BEC-A82B-95E2F076ED87}']
  {Methods}
  function init: JIPolylineDelegate_a; cdecl;
  function Q(paramIBinder: JIBinder): JIPolylineDelegate; cdecl;
end;

[JavaSignature('com/google/android/gms/maps/model/internal/IPolylineDelegate$a')]
JIPolylineDelegate_a = interface(JBinder)
['{3A8451FC-EC2F-4B94-8850-CC0DE0DE365D}']
  {Methods}
  function onTransact(code: Integer; data: JParcel; reply: JParcel; flags: Integer): Boolean; cdecl;
end;
TJIPolylineDelegate_a = class(TJavaGenericImport<JIPolylineDelegate_aClass, JIPolylineDelegate_a>) end;

JDataBufferClass = interface(JObjectClass)
['{49C7D692-3653-40EB-B4D9-256184DAFDC3}']
  {Methods}
  function init(dataHolder: JString): JDataBuffer; cdecl;
end;

[JavaSignature('com/google/android/gms/common/data/DataBuffer')]
JDataBuffer = interface(JObject)
['{E66E2584-412C-4878-B2BF-22E564A67E5B}']
  {Methods}
  procedure close; cdecl;
  function describeContents: Integer; cdecl;
  function &get(paramInt: Integer): JObject; cdecl;
  function getCount: Integer; cdecl;
  function isClosed: Boolean; cdecl;
  function iterator: JIterator; cdecl;
end;
TJDataBuffer = class(TJavaGenericImport<JDataBufferClass, JDataBuffer>) end;

JGameBufferClass = interface(JDataBufferClass)
['{C55FFCFE-4032-4AA2-B0DF-15E3988DE884}']
  {Methods}
  function init(dataHolder: JObject): JGameBuffer; cdecl;
end;

[JavaSignature('com/google/android/gms/games/GameBuffer')]
JGameBuffer = interface(JDataBuffer)
['{E2BDBCD5-6EF6-4F2B-B81D-215B1B3DB8F0}']
  {Methods}
  function &get(position: Integer): JGame; cdecl;
end;
TJGameBuffer = class(TJavaGenericImport<JGameBufferClass, JGameBuffer>) end;

JLocationClient_OnAddGeofencesResultListenerClass = interface(IJavaClass)
['{6F178CD1-33E6-47D5-BA34-1C1C6C0CD9E9}']
end;

[JavaSignature('com/google/android/gms/location/LocationClient$OnAddGeofencesResultListener')]
JLocationClient_OnAddGeofencesResultListener = interface(IJavaInstance)
['{699002BC-F53B-4868-88FA-884F26323132}']
  {Methods}
  procedure onAddGeofencesResult(paramInt: Integer; paramArrayOfString: TJavaObjectArray<JString>); cdecl;
end;
TJLocationClient_OnAddGeofencesResultListener = class(TJavaGenericImport<JLocationClient_OnAddGeofencesResultListenerClass, JLocationClient_OnAddGeofencesResultListener>) end;

JGameClass = interface(JFreezableClass)
['{5361CB92-B217-41D2-A914-E3E8F3A87693}']
end;

[JavaSignature('com/google/android/gms/games/Game')]
JGame = interface(JFreezable)
['{2A83D53C-79C0-44CC-93B8-ECD4B0F08230}']
  {Methods}
  function getAchievementTotalCount: Integer; cdecl;
  function getApplicationId: JString; cdecl;
  function getDescription: JString; cdecl; overload;
  procedure getDescription(paramCharArrayBuffer: Jdatabase_CharArrayBuffer); cdecl; overload;
  function getDeveloperName: JString; cdecl; overload;
  procedure getDeveloperName(paramCharArrayBuffer: Jdatabase_CharArrayBuffer); cdecl; overload;
  function getDisplayName: JString; cdecl; overload;
  procedure getDisplayName(paramCharArrayBuffer: Jdatabase_CharArrayBuffer); cdecl; overload;
  function getFeaturedImageUri: Jnet_Uri; cdecl;
  function getGameplayAclStatus: Integer; cdecl;
  function getHiResImageUri: Jnet_Uri; cdecl;
  function getIconImageUri: Jnet_Uri; cdecl;
  function getInstancePackageName: JString; cdecl;
  function getLeaderboardCount: Integer; cdecl;
  function getPrimaryCategory: JString; cdecl;
  function getSecondaryCategory: JString; cdecl;
  function isInstanceInstalled: Boolean; cdecl;
  function isPlayEnabledGame: Boolean; cdecl;
end;
TJGame = class(TJavaGenericImport<JGameClass, JGame>) end;

JILocationSourceDelegateClass = interface(JIInterfaceClass)
['{9E105F6A-9DDF-42A1-A1B9-05CA3D9A48C8}']
end;

[JavaSignature('com/google/android/gms/maps/internal/ILocationSourceDelegate')]
JILocationSourceDelegate = interface(JIInterface)
['{33B90AE3-00D3-4025-BC1F-D0D25CCAE092}']
  {Methods}
  procedure activate(paramcx: JObject); cdecl;
  procedure deactivate; cdecl;
end;
TJILocationSourceDelegate = class(TJavaGenericImport<JILocationSourceDelegateClass, JILocationSourceDelegate>) end;

JOnStateListLoadedListenerClass = interface(IJavaClass)
['{C4F3C39D-4858-4EBD-9E68-BEC22EA093CE}']
end;

[JavaSignature('com/google/android/gms/appstate/OnStateListLoadedListener')]
JOnStateListLoadedListener = interface(IJavaInstance)
['{03DFEAF5-58B0-40FE-BC1E-647D733CB3B0}']
  {Methods}
  procedure onStateListLoaded(paramInt: Integer; paramAppStateBuffer: JAppStateBuffer); cdecl;
end;
TJOnStateListLoadedListener = class(TJavaGenericImport<JOnStateListLoadedListenerClass, JOnStateListLoadedListener>) end;

JLocationSource_OnLocationChangedListenerClass = interface(IJavaClass)
['{C1885854-F25B-4E28-8AB9-2052BFBAA973}']
end;

[JavaSignature('com/google/android/gms/maps/LocationSource$OnLocationChangedListener')]
JLocationSource_OnLocationChangedListener = interface(IJavaInstance)
['{8A25D8FD-F6ED-4D09-A98C-BA569E82D7BB}']
  {Methods}
  procedure onLocationChanged(paramLocation: JLocation); cdecl;
end;
TJLocationSource_OnLocationChangedListener = class(TJavaGenericImport<JLocationSource_OnLocationChangedListenerClass, JLocationSource_OnLocationChangedListener>) end;

JParticipantClass = interface(JFreezableClass)
['{0BE0B958-105E-4602-B807-5599272EFA4E}']
  {Property Methods}
  function _GetSTATUS_DECLINED: Integer;
  function _GetSTATUS_INVITED: Integer;
  function _GetSTATUS_JOINED: Integer;
  function _GetSTATUS_LEFT: Integer;
  {Properties}
  property STATUS_DECLINED: Integer read _GetSTATUS_DECLINED;
  property STATUS_INVITED: Integer read _GetSTATUS_INVITED;
  property STATUS_JOINED: Integer read _GetSTATUS_JOINED;
  property STATUS_LEFT: Integer read _GetSTATUS_LEFT;
end;

[JavaSignature('com/google/android/gms/games/multiplayer/Participant')]
JParticipant = interface(JFreezable)
['{DC33E0EF-970D-4D19-97F5-7FAD48261851}']
  {Methods}
  function aK: JString; cdecl;
  function aL: Integer; cdecl;
  function getDisplayName: JString; cdecl; overload;
  procedure getDisplayName(paramCharArrayBuffer: Jdatabase_CharArrayBuffer); cdecl; overload;
  function getHiResImageUri: Jnet_Uri; cdecl;
  function getIconImageUri: Jnet_Uri; cdecl;
  function getParticipantId: JString; cdecl;
  function getPlayer: JPlayer; cdecl;
  function getStatus: Integer; cdecl;
  function isConnectedToRoom: Boolean; cdecl;
end;
TJParticipant = class(TJavaGenericImport<JParticipantClass, JParticipant>) end;

JUiSettingsClass = interface(JObjectClass)
['{E177867D-1013-41DC-8CC9-61E764F41FC2}']
end;

[JavaSignature('com/google/android/gms/maps/UiSettings')]
JUiSettings = interface(JObject)
['{C166F17B-4B44-4A09-8542-222C1668594A}']
  {Methods}
  function isCompassEnabled: Boolean; cdecl;
  function isMyLocationButtonEnabled: Boolean; cdecl;
  function isRotateGesturesEnabled: Boolean; cdecl;
  function isScrollGesturesEnabled: Boolean; cdecl;
  function isTiltGesturesEnabled: Boolean; cdecl;
  function isZoomControlsEnabled: Boolean; cdecl;
  function isZoomGesturesEnabled: Boolean; cdecl;
  procedure setAllGesturesEnabled(enabled: Boolean); cdecl;
  procedure setCompassEnabled(enabled: Boolean); cdecl;
  procedure setMyLocationButtonEnabled(enabled: Boolean); cdecl;
  procedure setRotateGesturesEnabled(enabled: Boolean); cdecl;
  procedure setScrollGesturesEnabled(enabled: Boolean); cdecl;
  procedure setTiltGesturesEnabled(enabled: Boolean); cdecl;
  procedure setZoomControlsEnabled(enabled: Boolean); cdecl;
  procedure setZoomGesturesEnabled(enabled: Boolean); cdecl;
end;
TJUiSettings = class(TJavaGenericImport<JUiSettingsClass, JUiSettings>) end;

JVisibleRegionCreatorClass = interface(JObjectClass)
['{F0ADE27E-6E2F-44CA-B534-65FA78D66A75}']
  {Property Methods}
  function _GetCONTENT_DESCRIPTION: Integer;
  {Methods}
  function init: JVisibleRegionCreator; cdecl;
  {Properties}
  property CONTENT_DESCRIPTION: Integer read _GetCONTENT_DESCRIPTION;
end;

[JavaSignature('com/google/android/gms/maps/model/VisibleRegionCreator')]
JVisibleRegionCreator = interface(JObject)
['{77DB84F4-D2D3-42A0-A53A-B50B3BC12993}']
  {Methods}
  function createFromParcel(parcel: JParcel): JVisibleRegion; cdecl;
  function newArray(size: Integer): TJavaObjectArray<JVisibleRegion>; cdecl;
end;
TJVisibleRegionCreator = class(TJavaGenericImport<JVisibleRegionCreatorClass, JVisibleRegionCreator>) end;

JOnLeaderboardScoresLoadedListenerClass = interface(IJavaClass)
['{B6DC33DD-7AC9-4EF7-84E5-831EF7BC0C83}']
end;

[JavaSignature('com/google/android/gms/games/leaderboard/OnLeaderboardScoresLoadedListener')]
JOnLeaderboardScoresLoadedListener = interface(IJavaInstance)
['{590A2D73-462B-4406-9684-160448A22313}']
  {Methods}
  procedure onLeaderboardScoresLoaded(paramInt: Integer; paramLeaderboardBuffer: JLeaderboardBuffer; paramLeaderboardScoreBuffer: JLeaderboardScoreBuffer); cdecl;
end;
TJOnLeaderboardScoresLoadedListener = class(TJavaGenericImport<JOnLeaderboardScoresLoadedListenerClass, JOnLeaderboardScoresLoadedListener>) end;

JCover_CoverInfoClass = interface(JFreezableClass)
['{5B6BC276-5F96-4282-98A4-9790D72CAF54}']
end;

[JavaSignature('com/google/android/gms/plus/model/people/Person$Cover$CoverInfo')]
JCover_CoverInfo = interface(JFreezable)
['{006DC380-FDF0-4FA3-9F8C-217EA45B4B5E}']
  {Methods}
  function getLeftImageOffset: Integer; cdecl;
  function getTopImageOffset: Integer; cdecl;
  function hasLeftImageOffset: Boolean; cdecl;
  function hasTopImageOffset: Boolean; cdecl;
end;
TJCover_CoverInfo = class(TJavaGenericImport<JCover_CoverInfoClass, JCover_CoverInfo>) end;

JLatLngCreatorClass = interface(JObjectClass)
['{099B3392-8621-4CD8-BEA3-6CC7E4A6A503}']
  {Property Methods}
  function _GetCONTENT_DESCRIPTION: Integer;
  {Methods}
  function init: JLatLngCreator; cdecl;
  {Properties}
  property CONTENT_DESCRIPTION: Integer read _GetCONTENT_DESCRIPTION;
end;

[JavaSignature('com/google/android/gms/maps/model/LatLngCreator')]
JLatLngCreator = interface(JObject)
['{5292361B-08AD-434C-9772-C69A062D115D}']
  {Methods}
  function createFromParcel(parcel: JParcel): JLatLng; cdecl;
  function newArray(size: Integer): TJavaObjectArray<JLatLng>; cdecl;
end;
TJLatLngCreator = class(TJavaGenericImport<JLatLngCreatorClass, JLatLngCreator>) end;

Jgames_OnSignOutCompleteListenerClass = interface(IJavaClass)
['{DC235B62-2E5D-411B-8379-33741321B7DD}']
end;

[JavaSignature('com/google/android/gms/games/OnSignOutCompleteListener')]
Jgames_OnSignOutCompleteListener = interface(IJavaInstance)
['{A94C0ACE-7D79-4077-B46A-85625EF9117B}']
  {Methods}
  procedure onSignOutComplete; cdecl;
end;
TJgames_OnSignOutCompleteListener = class(TJavaGenericImport<Jgames_OnSignOutCompleteListenerClass, Jgames_OnSignOutCompleteListener>) end;

JIGoogleMapDelegateClass = interface(JIInterfaceClass)
['{8F1C7FEB-7BD3-4EAA-B99B-59A74CC7B8C7}']
end;

[JavaSignature('com/google/android/gms/maps/internal/IGoogleMapDelegate')]
JIGoogleMapDelegate = interface(JIInterface)
['{7154878F-75B5-4D54-8126-D0FF7F396E90}']
  {Methods}
  function addCircle(paramCircleOptions: JCircleOptions): JObject; cdecl;
  function addGroundOverlay(paramGroundOverlayOptions: JGroundOverlayOptions): JObject; cdecl;
  function addMarker(paramMarkerOptions: JMarkerOptions): JObject; cdecl;
  function addPolygon(paramPolygonOptions: JPolygonOptions): JObject; cdecl;
  function addPolyline(paramPolylineOptions: JPolylineOptions): JIPolylineDelegate; cdecl;
  function addTileOverlay(paramTileOverlayOptions: JTileOverlayOptions): JObject; cdecl;
  procedure animateCamera(parambi: JObject); cdecl;
  procedure animateCameraWithCallback(parambi: JObject; paramcs: JObject); cdecl;
  procedure animateCameraWithDurationAndCallback(parambi: JObject; paramInt: Integer; paramcs: JObject); cdecl;
  procedure clear; cdecl;
  function getCameraPosition: JCameraPosition; cdecl;
  function getMapType: Integer; cdecl;
  function getMaxZoomLevel: Single; cdecl;
  function getMinZoomLevel: Single; cdecl;
  function getMyLocation: JLocation; cdecl;
  function getProjection: JIProjectionDelegate; cdecl;
  function getTestingHelper: JObject; cdecl;
  function getUiSettings: JIUiSettingsDelegate; cdecl;
  function isIndoorEnabled: Boolean; cdecl;
  function isMyLocationEnabled: Boolean; cdecl;
  function isTrafficEnabled: Boolean; cdecl;
  procedure moveCamera(parambi: JObject); cdecl;
  function setIndoorEnabled(paramBoolean: Boolean): Boolean; cdecl;
  procedure setInfoWindowAdapter(paramcu: JObject); cdecl;
  procedure setLocationSource(paramILocationSourceDelegate: JILocationSourceDelegate); cdecl;
  procedure setMapType(paramInt: Integer); cdecl;
  procedure setMyLocationEnabled(paramBoolean: Boolean); cdecl;
  procedure setOnCameraChangeListener(paramcv: JObject); cdecl;
  procedure setOnInfoWindowClickListener(paramcw: JObject); cdecl;
  procedure setOnMapClickListener(paramcy: JObject); cdecl;
  procedure setOnMapLongClickListener(paramcz: JObject); cdecl;
  procedure setOnMarkerClickListener(paramda: JObject); cdecl;
  procedure setOnMarkerDragListener(paramdb: JObject); cdecl;
  procedure setOnMyLocationButtonClickListener(paramdc: JObject); cdecl;
  procedure setOnMyLocationChangeListener(paramdd: JObject); cdecl;
  procedure setTrafficEnabled(paramBoolean: Boolean); cdecl;
  procedure snapshot(paramde: JObject; parambi: JObject); cdecl;
  procedure stopAnimation; cdecl;
end;
TJIGoogleMapDelegate = class(TJavaGenericImport<JIGoogleMapDelegateClass, JIGoogleMapDelegate>) end;

JPanoramaClient_aClass = interface(IJavaClass)
['{88DF00B0-2A0C-4F2C-AC8B-1FC3142601CA}']
end;

[JavaSignature('com/google/android/gms/panorama/PanoramaClient$a')]
JPanoramaClient_a = interface(IJavaInstance)
['{ECA517C4-3D6F-4891-A461-FC168217D136}']
  {Methods}
  procedure a(paramConnectionResult: JConnectionResult; paramInt: Integer; paramIntent: JIntent); cdecl;
end;
TJPanoramaClient_a = class(TJavaGenericImport<JPanoramaClient_aClass, JPanoramaClient_a>) end;

JTileOverlayOptionsCreatorClass = interface(JObjectClass)
['{E18C25A6-E13E-4A67-A7B3-73BD8A7DFF05}']
  {Property Methods}
  function _GetCONTENT_DESCRIPTION: Integer;
  {Methods}
  function init: JTileOverlayOptionsCreator; cdecl;
  {Properties}
  property CONTENT_DESCRIPTION: Integer read _GetCONTENT_DESCRIPTION;
end;

[JavaSignature('com/google/android/gms/maps/model/TileOverlayOptionsCreator')]
JTileOverlayOptionsCreator = interface(JObject)
['{269B57A3-0663-46A7-BD9D-976C9BE7D2C6}']
  {Methods}
  function createFromParcel(parcel: JParcel): JTileOverlayOptions; cdecl;
  function newArray(size: Integer): TJavaObjectArray<JTileOverlayOptions>; cdecl;
end;
TJTileOverlayOptionsCreator = class(TJavaGenericImport<JTileOverlayOptionsCreatorClass, JTileOverlayOptionsCreator>) end;

JPerson_GenderClass = interface(JObjectClass)
['{C5EFF326-3D1E-47C0-8380-08A20A233161}']
  {Property Methods}
  function _GetFEMALE: Integer;
  function _GetMALE: Integer;
  function _GetOTHER: Integer;
  {Methods}
  function init: JPerson_Gender; cdecl;
  {Properties}
  property FEMALE: Integer read _GetFEMALE;
  property MALE: Integer read _GetMALE;
  property OTHER: Integer read _GetOTHER;
end;

[JavaSignature('com/google/android/gms/plus/model/people/Person$Gender')]
JPerson_Gender = interface(JObject)
['{281D7398-5B5B-48D1-B017-BC092805F5AF}']
end;
TJPerson_Gender = class(TJavaGenericImport<JPerson_GenderClass, JPerson_Gender>) end;

JParticipantEntityClass = interface(JObjectClass)
['{021453EF-43A8-4460-980D-9283E20FF09A}']
  {Property Methods}
  function _GetCREATOR: JParticipantEntityCreator;
  {Methods}
  function init(participant: JParticipant): JParticipantEntity; cdecl;
  function a(paramParticipant: JParticipant): Integer; cdecl; overload;
  function a(paramParticipant: JParticipant; paramObject: JObject): Boolean; cdecl; overload;
  function b(paramParticipant: JParticipant): JString; cdecl;
  {Properties}
  property CREATOR: JParticipantEntityCreator read _GetCREATOR;
end;

[JavaSignature('com/google/android/gms/games/multiplayer/ParticipantEntity')]
JParticipantEntity = interface(JObject)
['{2D72952A-D241-4B27-BFFF-F066A55DC527}']
  {Methods}
  function F: Integer; cdecl;
  function aK: JString; cdecl;
  function aL: Integer; cdecl;
  function describeContents: Integer; cdecl;
  function equals(obj: JObject): Boolean; cdecl;
  function freeze: JParticipant; cdecl;
  function getDisplayName: JString; cdecl; overload;
  procedure getDisplayName(dataOut: Jdatabase_CharArrayBuffer); cdecl; overload;
  function getHiResImageUri: Jnet_Uri; cdecl;
  function getIconImageUri: Jnet_Uri; cdecl;
  function getParticipantId: JString; cdecl;
  function getPlayer: JPlayer; cdecl;
  function getStatus: Integer; cdecl;
  function hashCode: Integer; cdecl;
  function isConnectedToRoom: Boolean; cdecl;
  function isDataValid: Boolean; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
end;
TJParticipantEntity = class(TJavaGenericImport<JParticipantEntityClass, JParticipantEntity>) end;

JPerson_EmailsClass = interface(JFreezableClass)
['{A98965F2-5D52-4595-BA74-D6F294BD78E3}']
end;

[JavaSignature('com/google/android/gms/plus/model/people/Person$Emails')]
JPerson_Emails = interface(JFreezable)
['{F6ADFE5C-2957-43E0-A49D-81C988904C72}']
  {Methods}
  function getType: Integer; cdecl;
  function getValue: JString; cdecl;
  function hasPrimary: Boolean; cdecl;
  function hasType: Boolean; cdecl;
  function hasValue: Boolean; cdecl;
  function isPrimary: Boolean; cdecl;
end;
TJPerson_Emails = class(TJavaGenericImport<JPerson_EmailsClass, JPerson_Emails>) end;

JPlusOneButton_OnPlusOneClickListenerClass = interface(IJavaClass)
['{42763D8A-94F3-4096-9C01-EBDF8ED918EC}']
end;

[JavaSignature('com/google/android/gms/plus/PlusOneButton$OnPlusOneClickListener')]
JPlusOneButton_OnPlusOneClickListener = interface(IJavaInstance)
['{5F317FCE-AD9C-4B7F-BA9A-5E76573B8DC2}']
  {Methods}
  procedure onPlusOneClick(paramIntent: JIntent); cdecl;
end;
TJPlusOneButton_OnPlusOneClickListener = class(TJavaGenericImport<JPlusOneButton_OnPlusOneClickListenerClass, JPlusOneButton_OnPlusOneClickListener>) end;

JCameraUpdateFactoryClass = interface(JObjectClass)
['{67303D3C-CEA1-4D27-A15B-64C19309A076}']
  {Methods}
  function init: JCameraUpdateFactory; cdecl;
  function newCameraPosition(cameraPosition: JCameraPosition): JCameraUpdate; cdecl;
  function newLatLng(latLng: JLatLng): JCameraUpdate; cdecl;
  function newLatLngBounds(bounds: JLatLngBounds; padding: Integer): JCameraUpdate; cdecl; overload;
  function newLatLngBounds(bounds: JLatLngBounds; width: Integer; height: Integer; padding: Integer): JCameraUpdate; cdecl; overload;
  function newLatLngZoom(latLng: JLatLng; zoom: Single): JCameraUpdate; cdecl;
  function scrollBy(xPixel: Single; yPixel: Single): JCameraUpdate; cdecl;
  function zoomBy(amount: Single): JCameraUpdate; cdecl; overload;
  function zoomBy(amount: Single; focus: JPoint): JCameraUpdate; cdecl; overload;
  function zoomIn: JCameraUpdate; cdecl;
  function zoomOut: JCameraUpdate; cdecl;
  function zoomTo(zoom: Single): JCameraUpdate; cdecl;
end;

[JavaSignature('com/google/android/gms/maps/CameraUpdateFactory')]
JCameraUpdateFactory = interface(JObject)
['{4E9D6497-D772-4A4C-9E34-48B7B1EED438}']
end;
TJCameraUpdateFactory = class(TJavaGenericImport<JCameraUpdateFactoryClass, JCameraUpdateFactory>) end;

JPlusClient_OnAccessRevokedListenerClass = interface(IJavaClass)
['{37BBB8DE-48CE-4DDE-94D7-ECD2D634F555}']
end;

[JavaSignature('com/google/android/gms/plus/PlusClient$OnAccessRevokedListener')]
JPlusClient_OnAccessRevokedListener = interface(IJavaInstance)
['{339A35F4-5039-4539-84E1-A54BB83E8462}']
  {Methods}
  procedure onAccessRevoked(paramConnectionResult: JConnectionResult); cdecl;
end;
TJPlusClient_OnAccessRevokedListener = class(TJavaGenericImport<JPlusClient_OnAccessRevokedListenerClass, JPlusClient_OnAccessRevokedListener>) end;

JLocationStatusCodesClass = interface(JObjectClass)
['{0571787B-F88E-41B1-A5CC-3681EB3C4590}']
  {Property Methods}
  function _GetERROR: Integer;
  function _GetGEOFENCE_NOT_AVAILABLE: Integer;
  function _GetGEOFENCE_TOO_MANY_GEOFENCES: Integer;
  function _GetGEOFENCE_TOO_MANY_PENDING_INTENTS: Integer;
  function _GetSUCCESS: Integer;
  {Methods}
  function init: JLocationStatusCodes; cdecl;
  function J(paramInt: Integer): Integer; cdecl;
  {Properties}
  property ERROR: Integer read _GetERROR;
  property GEOFENCE_NOT_AVAILABLE: Integer read _GetGEOFENCE_NOT_AVAILABLE;
  property GEOFENCE_TOO_MANY_GEOFENCES: Integer read _GetGEOFENCE_TOO_MANY_GEOFENCES;
  property GEOFENCE_TOO_MANY_PENDING_INTENTS: Integer read _GetGEOFENCE_TOO_MANY_PENDING_INTENTS;
  property SUCCESS: Integer read _GetSUCCESS;
end;

[JavaSignature('com/google/android/gms/location/LocationStatusCodes')]
JLocationStatusCodes = interface(JObject)
['{47492F6A-FD16-40E4-945F-9F9979B4B620}']
end;
TJLocationStatusCodes = class(TJavaGenericImport<JLocationStatusCodesClass, JLocationStatusCodes>) end;

JGoogleMap_OnMarkerDragListenerClass = interface(IJavaClass)
['{6E1353C5-A89C-4DAD-9613-1029DA10D9E4}']
end;

[JavaSignature('com/google/android/gms/maps/GoogleMap$OnMarkerDragListener')]
JGoogleMap_OnMarkerDragListener = interface(IJavaInstance)
['{F4D0B32E-859B-47D6-8ECD-573590756CD9}']
  {Methods}
  procedure onMarkerDrag(paramMarker: JMarker); cdecl;
  procedure onMarkerDragEnd(paramMarker: JMarker); cdecl;
  procedure onMarkerDragStart(paramMarker: JMarker); cdecl;
end;
TJGoogleMap_OnMarkerDragListener = class(TJavaGenericImport<JGoogleMap_OnMarkerDragListenerClass, JGoogleMap_OnMarkerDragListener>) end;

JGoogleMapOptionsClass = interface(JObjectClass)
['{6858A5C7-54A7-40DE-9A0C-1DC32ED24A12}']
  {Property Methods}
  function _GetCREATOR: JGoogleMapOptionsCreator;
  {Methods}
  function init: JGoogleMapOptions; cdecl;
  function createFromAttributes(context: JContext; attrs: JAttributeSet): JGoogleMapOptions; cdecl;
  {Properties}
  property CREATOR: JGoogleMapOptionsCreator read _GetCREATOR;
end;

[JavaSignature('com/google/android/gms/maps/GoogleMapOptions')]
JGoogleMapOptions = interface(JObject)
['{DC638BBD-E6D4-4F74-B531-53B24E101B06}']
  {Methods}
  function F: Integer; cdecl;
  function aX: Byte; cdecl;
  function aY: Byte; cdecl;
  function aZ: Byte; cdecl;
  function ba: Byte; cdecl;
  function bb: Byte; cdecl;
  function bc: Byte; cdecl;
  function bd: Byte; cdecl;
  function be: Byte; cdecl;
  function camera(camera: JCameraPosition): JGoogleMapOptions; cdecl;
  function compassEnabled(enabled: Boolean): JGoogleMapOptions; cdecl;
  function describeContents: Integer; cdecl;
  function getCamera: JCameraPosition; cdecl;
  function getCompassEnabled: JBoolean; cdecl;
  function getMapType: Integer; cdecl;
  function getRotateGesturesEnabled: JBoolean; cdecl;
  function getScrollGesturesEnabled: JBoolean; cdecl;
  function getTiltGesturesEnabled: JBoolean; cdecl;
  function getUseViewLifecycleInFragment: JBoolean; cdecl;
  function getZOrderOnTop: JBoolean; cdecl;
  function getZoomControlsEnabled: JBoolean; cdecl;
  function getZoomGesturesEnabled: JBoolean; cdecl;
  function mapType(mapType: Integer): JGoogleMapOptions; cdecl;
  function rotateGesturesEnabled(enabled: Boolean): JGoogleMapOptions; cdecl;
  function scrollGesturesEnabled(enabled: Boolean): JGoogleMapOptions; cdecl;
  function tiltGesturesEnabled(enabled: Boolean): JGoogleMapOptions; cdecl;
  function useViewLifecycleInFragment(useViewLifecycleInFragment: Boolean): JGoogleMapOptions; cdecl;
  procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
  function zOrderOnTop(zOrderOnTop: Boolean): JGoogleMapOptions; cdecl;
  function zoomControlsEnabled(enabled: Boolean): JGoogleMapOptions; cdecl;
  function zoomGesturesEnabled(enabled: Boolean): JGoogleMapOptions; cdecl;
end;
TJGoogleMapOptions = class(TJavaGenericImport<JGoogleMapOptionsClass, JGoogleMapOptions>) end;

JLeaderboardBufferClass = interface(JObjectClass)
['{3057376F-EE45-4E71-92A3-D82D3CA565CF}']
  {Methods}
  function init(dataHolder: JObject): JLeaderboardBuffer; cdecl;
end;

[JavaSignature('com/google/android/gms/games/leaderboard/LeaderboardBuffer')]
JLeaderboardBuffer = interface(JObject)
['{AA406BEB-1C13-4FFD-86C7-195D13715416}']
end;
TJLeaderboardBuffer = class(TJavaGenericImport<JLeaderboardBufferClass, JLeaderboardBuffer>) end;

JMoment_BuilderClass = interface(JObjectClass)
['{02E48441-AAC0-4256-AB90-E655FE75E9C3}']
  {Methods}
  function init: JMoment_Builder; cdecl;
end;

[JavaSignature('com/google/android/gms/plus/model/moments/Moment$Builder')]
JMoment_Builder = interface(JObject)
['{7C265211-A570-404E-AE7E-D29FCD819EE9}']
  {Methods}
  function build: JMoment; cdecl;
  function setId(id: JString): JMoment_Builder; cdecl;
  function setResult(result: JItemScope): JMoment_Builder; cdecl;
  function setStartDate(startDate: JString): JMoment_Builder; cdecl;
  function setTarget(target: JItemScope): JMoment_Builder; cdecl;
  function setType(type_: JString): JMoment_Builder; cdecl;
end;
TJMoment_Builder = class(TJavaGenericImport<JMoment_BuilderClass, JMoment_Builder>) end;

JCircleClass = interface(JObjectClass)
['{EEB20939-CDBA-4C8C-BB4E-EA9F3FB16A14}']
  {Methods}
  function init(delegate: JObject): JCircle; cdecl;
end;

[JavaSignature('com/google/android/gms/maps/model/Circle')]
JCircle = interface(JObject)
['{D1408DBE-690F-45FD-A740-8D2FA4E10CED}']
  {Methods}
  function equals(other: JObject): Boolean; cdecl;
  function getCenter: JLatLng; cdecl;
  function getFillColor: Integer; cdecl;
  function getId: JString; cdecl;
  function getRadius: Double; cdecl;
  function getStrokeColor: Integer; cdecl;
  function getStrokeWidth: Single; cdecl;
  function getZIndex: Single; cdecl;
  function hashCode: Integer; cdecl;
  function isVisible: Boolean; cdecl;
  procedure remove; cdecl;
  procedure setCenter(center: JLatLng); cdecl;
  procedure setFillColor(color: Integer); cdecl;
  procedure setRadius(radius: Double); cdecl;
  procedure setStrokeColor(color: Integer); cdecl;
  procedure setStrokeWidth(width: Single); cdecl;
  procedure setVisible(visible: Boolean); cdecl;
  procedure setZIndex(zIndex: Single); cdecl;
end;
TJCircle = class(TJavaGenericImport<JCircleClass, JCircle>) end;

JPersonClass = interface(JFreezableClass)
['{73914393-03C0-47A6-8267-A900E6A8E1FE}']
end;

[JavaSignature('com/google/android/gms/plus/model/people/Person')]
JPerson = interface(JFreezable)
['{BA0A0480-FDFF-4DFC-BB62-5E4C7CA1693B}']
  {Methods}
  function getAboutMe: JString; cdecl;
  function getAgeRange: JPerson_AgeRange; cdecl;
  function getBirthday: JString; cdecl;
  function getBraggingRights: JString; cdecl;
  function getCircledByCount: Integer; cdecl;
  function getCover: JPerson_Cover; cdecl;
  function getCurrentLocation: JString; cdecl;
  function getDisplayName: JString; cdecl;
  function getEmails: JList; cdecl;
  function getGender: Integer; cdecl;
  function getId: JString; cdecl;
  function getImage: JPerson_Image; cdecl;
  function getLanguage: JString; cdecl;
  function getName: JPerson_Name; cdecl;
  function getNickname: JString; cdecl;
  function getObjectType: Integer; cdecl;
  function getOrganizations: JList; cdecl;
  function getPlacesLived: JList; cdecl;
  function getPlusOneCount: Integer; cdecl;
  function getRelationshipStatus: Integer; cdecl;
  function getTagline: JString; cdecl;
  function getUrl: JString; cdecl;
  function getUrls: JList; cdecl;
  function hasAboutMe: Boolean; cdecl;
  function hasAgeRange: Boolean; cdecl;
  function hasBirthday: Boolean; cdecl;
  function hasBraggingRights: Boolean; cdecl;
  function hasCircledByCount: Boolean; cdecl;
  function hasCover: Boolean; cdecl;
  function hasCurrentLocation: Boolean; cdecl;
  function hasDisplayName: Boolean; cdecl;
  function hasEmails: Boolean; cdecl;
  function hasGender: Boolean; cdecl;
  function hasHasApp: Boolean; cdecl;
  function hasId: Boolean; cdecl;
  function hasImage: Boolean; cdecl;
  function hasIsPlusUser: Boolean; cdecl;
  function hasLanguage: Boolean; cdecl;
  function hasName: Boolean; cdecl;
  function hasNickname: Boolean; cdecl;
  function hasObjectType: Boolean; cdecl;
  function hasOrganizations: Boolean; cdecl;
  function hasPlacesLived: Boolean; cdecl;
  function hasPlusOneCount: Boolean; cdecl;
  function hasRelationshipStatus: Boolean; cdecl;
  function hasTagline: Boolean; cdecl;
  function hasUrl: Boolean; cdecl;
  function hasUrls: Boolean; cdecl;
  function hasVerified: Boolean; cdecl;
  function isHasApp: Boolean; cdecl;
  function isPlusUser: Boolean; cdecl;
  function isVerified: Boolean; cdecl;
end;
TJPerson = class(TJavaGenericImport<JPersonClass, JPerson>) end;

JCircleOptionsClass = interface(JObjectClass)
['{D055A38E-CD1E-41B4-B6A0-F6EDF0869544}']
  {Property Methods}
  function _GetCREATOR: JCircleOptionsCreator;
  {Methods}
  function init: JCircleOptions; cdecl;
  {Properties}
  property CREATOR: JCircleOptionsCreator read _GetCREATOR;
end;

[JavaSignature('com/google/android/gms/maps/model/CircleOptions')]
JCircleOptions = interface(JObject)
['{5EB2D906-C70F-48BC-A34B-BA102162163A}']
  {Methods}
  function F: Integer; cdecl;
  function center(center: JLatLng): JCircleOptions; cdecl;
  function describeContents: Integer; cdecl;
  function fillColor(color: Integer): JCircleOptions; cdecl;
  function getCenter: JLatLng; cdecl;
  function getFillColor: Integer; cdecl;
  function getRadius: Double; cdecl;
  function getStrokeColor: Integer; cdecl;
  function getStrokeWidth: Single; cdecl;
  function getZIndex: Single; cdecl;
  function isVisible: Boolean; cdecl;
  function radius(radius: Double): JCircleOptions; cdecl;
  function strokeColor(color: Integer): JCircleOptions; cdecl;
  function strokeWidth(width: Single): JCircleOptions; cdecl;
  function visible(visible: Boolean): JCircleOptions; cdecl;
  procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
  function zIndex(zIndex: Single): JCircleOptions; cdecl;
end;
TJCircleOptions = class(TJavaGenericImport<JCircleOptionsClass, JCircleOptions>) end;

JPlayerClass = interface(JFreezableClass)
['{9DB5DC9B-3D32-4FBB-A863-6680D495678F}']
end;

[JavaSignature('com/google/android/gms/games/Player')]
JPlayer = interface(JFreezable)
['{C4D9E51B-AAC2-45A6-9C13-6C8CBBBDA3ED}']
  {Methods}
  function getDisplayName: JString; cdecl; overload;
  procedure getDisplayName(paramCharArrayBuffer: Jdatabase_CharArrayBuffer); cdecl; overload;
  function getHiResImageUri: Jnet_Uri; cdecl;
  function getIconImageUri: Jnet_Uri; cdecl;
  function getPlayerId: JString; cdecl;
  function getRetrievedTimestamp: Int64; cdecl;
  function hasHiResImage: Boolean; cdecl;
  function hasIconImage: Boolean; cdecl;
end;
TJPlayer = class(TJavaGenericImport<JPlayerClass, JPlayer>) end;

JGoogleMap_InfoWindowAdapterClass = interface(IJavaClass)
['{4767E38C-50FA-43D2-A353-6562739F9B82}']
end;

[JavaSignature('com/google/android/gms/maps/GoogleMap$InfoWindowAdapter')]
JGoogleMap_InfoWindowAdapter = interface(IJavaInstance)
['{DFF8DAEB-2C2B-44CC-9688-B88308CB1025}']
  {Methods}
  function getInfoContents(paramMarker: JMarker): JView; cdecl;
  function getInfoWindow(paramMarker: JMarker): JView; cdecl;
end;
TJGoogleMap_InfoWindowAdapter = class(TJavaGenericImport<JGoogleMap_InfoWindowAdapterClass, JGoogleMap_InfoWindowAdapter>) end;

JGoogleAuthExceptionClass = interface(JExceptionClass)
['{24885575-796B-42F6-AAB7-585DEF967D34}']
  {Methods}
  function init(err: JString): JGoogleAuthException; cdecl;
end;

[JavaSignature('com/google/android/gms/auth/GoogleAuthException')]
JGoogleAuthException = interface(JException)
['{CD1926C5-141D-497D-87E4-E7D7D978506B}']
end;
TJGoogleAuthException = class(TJavaGenericImport<JGoogleAuthExceptionClass, JGoogleAuthException>) end;

JAppStateClientClass = interface(JObjectClass)
['{79BEE472-BDE5-43D0-BF77-B6745765A451}']
  {Property Methods}
  function _GetSTATUS_CLIENT_RECONNECT_REQUIRED: Integer;
  function _GetSTATUS_DEVELOPER_ERROR: Integer;
  function _GetSTATUS_INTERNAL_ERROR: Integer;
  function _GetSTATUS_NETWORK_ERROR_NO_DATA: Integer;
  function _GetSTATUS_NETWORK_ERROR_OPERATION_DEFERRED: Integer;
  function _GetSTATUS_NETWORK_ERROR_OPERATION_FAILED: Integer;
  function _GetSTATUS_NETWORK_ERROR_STALE_DATA: Integer;
  function _GetSTATUS_OK: Integer;
  function _GetSTATUS_STATE_KEY_LIMIT_EXCEEDED: Integer;
  function _GetSTATUS_STATE_KEY_NOT_FOUND: Integer;
  function _GetSTATUS_WRITE_OUT_OF_DATE_VERSION: Integer;
  function _GetSTATUS_WRITE_SIZE_EXCEEDED: Integer;
  {Properties}
  property STATUS_CLIENT_RECONNECT_REQUIRED: Integer read _GetSTATUS_CLIENT_RECONNECT_REQUIRED;
  property STATUS_DEVELOPER_ERROR: Integer read _GetSTATUS_DEVELOPER_ERROR;
  property STATUS_INTERNAL_ERROR: Integer read _GetSTATUS_INTERNAL_ERROR;
  property STATUS_NETWORK_ERROR_NO_DATA: Integer read _GetSTATUS_NETWORK_ERROR_NO_DATA;
  property STATUS_NETWORK_ERROR_OPERATION_DEFERRED: Integer read _GetSTATUS_NETWORK_ERROR_OPERATION_DEFERRED;
  property STATUS_NETWORK_ERROR_OPERATION_FAILED: Integer read _GetSTATUS_NETWORK_ERROR_OPERATION_FAILED;
  property STATUS_NETWORK_ERROR_STALE_DATA: Integer read _GetSTATUS_NETWORK_ERROR_STALE_DATA;
  property STATUS_OK: Integer read _GetSTATUS_OK;
  property STATUS_STATE_KEY_LIMIT_EXCEEDED: Integer read _GetSTATUS_STATE_KEY_LIMIT_EXCEEDED;
  property STATUS_STATE_KEY_NOT_FOUND: Integer read _GetSTATUS_STATE_KEY_NOT_FOUND;
  property STATUS_WRITE_OUT_OF_DATE_VERSION: Integer read _GetSTATUS_WRITE_OUT_OF_DATE_VERSION;
  property STATUS_WRITE_SIZE_EXCEEDED: Integer read _GetSTATUS_WRITE_SIZE_EXCEEDED;
end;

[JavaSignature('com/google/android/gms/appstate/AppStateClient')]
JAppStateClient = interface(JObject)
['{273EF274-E1B7-490E-9D48-1D5F0E5FB2E9}']
  {Methods}
  procedure connect; cdecl;
  procedure deleteState(listener: JOnStateDeletedListener; stateKey: Integer); cdecl;
  procedure disconnect; cdecl;
  function getMaxNumKeys: Integer; cdecl;
  function getMaxStateSize: Integer; cdecl;
  function isConnected: Boolean; cdecl;
  function isConnecting: Boolean; cdecl;
  function isConnectionCallbacksRegistered(listener: JGooglePlayServicesClient_ConnectionCallbacks): Boolean; cdecl;
  function isConnectionFailedListenerRegistered(listener: JGooglePlayServicesClient_OnConnectionFailedListener): Boolean; cdecl;
  procedure listStates(listener: JOnStateListLoadedListener); cdecl;
  procedure loadState(listener: JOnStateLoadedListener; stateKey: Integer); cdecl;
  procedure reconnect; cdecl;
  procedure registerConnectionCallbacks(listener: JGooglePlayServicesClient_ConnectionCallbacks); cdecl;
  procedure registerConnectionFailedListener(listener: JGooglePlayServicesClient_OnConnectionFailedListener); cdecl;
  procedure resolveState(listener: JOnStateLoadedListener; stateKey: Integer; resolvedVersion: JString; resolvedData: TJavaArray<Byte>); cdecl;
  procedure signOut; cdecl; overload;
  procedure signOut(listener: JOnSignOutCompleteListener); cdecl; overload;
  procedure unregisterConnectionCallbacks(listener: JGooglePlayServicesClient_ConnectionCallbacks); cdecl;
  procedure unregisterConnectionFailedListener(listener: JGooglePlayServicesClient_OnConnectionFailedListener); cdecl;
  procedure updateState(stateKey: Integer; data: TJavaArray<Byte>); cdecl;
  procedure updateStateImmediate(listener: JOnStateLoadedListener; stateKey: Integer; data: TJavaArray<Byte>); cdecl;
end;
TJAppStateClient = class(TJavaGenericImport<JAppStateClientClass, JAppStateClient>) end;

JIMapViewDelegate_aClass = interface(JBinderClass)
['{9F7CE20D-58F1-48A6-9144-9E0E8D67963E}']
  {Methods}
  function init: JIMapViewDelegate_a; cdecl;
  function y(paramIBinder: JIBinder): JIMapViewDelegate; cdecl;
end;

[JavaSignature('com/google/android/gms/maps/internal/IMapViewDelegate$a')]
JIMapViewDelegate_a = interface(JBinder)
['{3E2BD12F-4171-479B-9586-2C1780B69EF7}']
  {Methods}
  function onTransact(code: Integer; data: JParcel; reply: JParcel; flags: Integer): Boolean; cdecl;
end;
TJIMapViewDelegate_a = class(TJavaGenericImport<JIMapViewDelegate_aClass, JIMapViewDelegate_a>) end;

JGoogleCloudMessagingClass = interface(JObjectClass)
['{452B4174-FF9C-48F5-9609-674C8206C28A}']
  {Property Methods}
  function _GetERROR_MAIN_THREAD: JString;
  function _GetERROR_SERVICE_NOT_AVAILABLE: JString;
  function _GetMESSAGE_TYPE_DELETED: JString;
  function _GetMESSAGE_TYPE_MESSAGE: JString;
  function _GetMESSAGE_TYPE_SEND_ERROR: JString;
  {Methods}
  function init: JGoogleCloudMessaging; cdecl;
  function getInstance(context: JContext): JGoogleCloudMessaging; cdecl;
  {Properties}
  property ERROR_MAIN_THREAD: JString read _GetERROR_MAIN_THREAD;
  property ERROR_SERVICE_NOT_AVAILABLE: JString read _GetERROR_SERVICE_NOT_AVAILABLE;
  property MESSAGE_TYPE_DELETED: JString read _GetMESSAGE_TYPE_DELETED;
  property MESSAGE_TYPE_MESSAGE: JString read _GetMESSAGE_TYPE_MESSAGE;
  property MESSAGE_TYPE_SEND_ERROR: JString read _GetMESSAGE_TYPE_SEND_ERROR;
end;

[JavaSignature('com/google/android/gms/gcm/GoogleCloudMessaging')]
JGoogleCloudMessaging = interface(JObject)
['{A4F0B5B0-FCB2-45F8-A3CB-D37481F2FBD8}']
  {Methods}
  procedure close; cdecl;
  function getMessageType(intent: JIntent): JString; cdecl;
  function register(senderIds: TJavaObjectArray<JString>): JString; cdecl;
  procedure send(to_: JString; msgId: JString; data: JBundle); cdecl; overload;
  procedure send(to_: JString; msgId: JString; timeToLive: Int64; data: JBundle); cdecl; overload;
  procedure unregister; cdecl;
end;
TJGoogleCloudMessaging = class(TJavaGenericImport<JGoogleCloudMessagingClass, JGoogleCloudMessaging>) end;

JVisibleRegionClass = interface(JObjectClass)
['{C83C8AE5-7C11-407C-A342-3DE928A8E39B}']
  {Property Methods}
  function _GetCREATOR: JVisibleRegionCreator;
  {Methods}
  function init(nearLeft: JLatLng; nearRight: JLatLng; farLeft: JLatLng; farRight: JLatLng; latLngBounds: JLatLngBounds): JVisibleRegion; cdecl;
  {Properties}
  property CREATOR: JVisibleRegionCreator read _GetCREATOR;
end;

[JavaSignature('com/google/android/gms/maps/model/VisibleRegion')]
JVisibleRegion = interface(JObject)
['{669FB7E8-207E-4C0A-8882-F056813AB68C}']
  {Property Methods}
  function _GetfarLeft: JLatLng;
  function _GetfarRight: JLatLng;
  function _GetlatLngBounds: JLatLngBounds;
  function _GetnearLeft: JLatLng;
  function _GetnearRight: JLatLng;
  {Methods}
  function F: Integer; cdecl;
  function describeContents: Integer; cdecl;
  function equals(o: JObject): Boolean; cdecl;
  function hashCode: Integer; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
  {Properties}
  property farLeft: JLatLng read _GetfarLeft;
  property farRight: JLatLng read _GetfarRight;
  property latLngBounds: JLatLngBounds read _GetlatLngBounds;
  property nearLeft: JLatLng read _GetnearLeft;
  property nearRight: JLatLng read _GetnearRight;
end;
TJVisibleRegion = class(TJavaGenericImport<JVisibleRegionClass, JVisibleRegion>) end;

JIProjectionDelegateClass = interface(JIInterfaceClass)
['{198CE567-66D1-47CB-A9F9-EF5753805C59}']
end;

[JavaSignature('com/google/android/gms/maps/internal/IProjectionDelegate')]
JIProjectionDelegate = interface(JIInterface)
['{93D6C3BF-8B87-48B0-BB99-0A216589089A}']
  {Methods}
  function fromScreenLocation(parambi: JObject): JLatLng; cdecl;
  function getVisibleRegion: JVisibleRegion; cdecl;
  function toScreenLocation(paramLatLng: JLatLng): JObject; cdecl;
end;
TJIProjectionDelegate = class(TJavaGenericImport<JIProjectionDelegateClass, JIProjectionDelegate>) end;

JOnAchievementsLoadedListenerClass = interface(IJavaClass)
['{A5BCEAA9-312B-4349-B35B-CE203BB9A05C}']
end;

[JavaSignature('com/google/android/gms/games/achievement/OnAchievementsLoadedListener')]
JOnAchievementsLoadedListener = interface(IJavaInstance)
['{0A5BB5D1-9353-4F7F-8E5D-B8BBA7BBED77}']
  {Methods}
  procedure onAchievementsLoaded(paramInt: Integer; paramAchievementBuffer: JAchievementBuffer); cdecl;
end;
TJOnAchievementsLoadedListener = class(TJavaGenericImport<JOnAchievementsLoadedListenerClass, JOnAchievementsLoadedListener>) end;

JBitmapDescriptorFactoryClass = interface(JObjectClass)
['{88545E48-00BE-4AEB-BF98-78308F5AB256}']
  {Property Methods}
  function _GetHUE_AZURE: Single;
  function _GetHUE_BLUE: Single;
  function _GetHUE_CYAN: Single;
  function _GetHUE_GREEN: Single;
  function _GetHUE_MAGENTA: Single;
  function _GetHUE_ORANGE: Single;
  function _GetHUE_RED: Single;
  function _GetHUE_ROSE: Single;
  function _GetHUE_VIOLET: Single;
  function _GetHUE_YELLOW: Single;
  {Methods}
  function init: JBitmapDescriptorFactory; cdecl;
  procedure a(paramdt: JObject); cdecl;
  function defaultMarker: JBitmapDescriptor; cdecl; overload;
  function defaultMarker(hue: Single): JBitmapDescriptor; cdecl; overload;
  function fromAsset(assetName: JString): JBitmapDescriptor; cdecl;
  function fromBitmap(image: JBitmap): JBitmapDescriptor; cdecl;
  function fromFile(fileName: JString): JBitmapDescriptor; cdecl;
  function fromPath(absolutePath: JString): JBitmapDescriptor; cdecl;
  function fromResource(resourceId: Integer): JBitmapDescriptor; cdecl;
  {Properties}
  property HUE_AZURE: Single read _GetHUE_AZURE;
  property HUE_BLUE: Single read _GetHUE_BLUE;
  property HUE_CYAN: Single read _GetHUE_CYAN;
  property HUE_GREEN: Single read _GetHUE_GREEN;
  property HUE_MAGENTA: Single read _GetHUE_MAGENTA;
  property HUE_ORANGE: Single read _GetHUE_ORANGE;
  property HUE_RED: Single read _GetHUE_RED;
  property HUE_ROSE: Single read _GetHUE_ROSE;
  property HUE_VIOLET: Single read _GetHUE_VIOLET;
  property HUE_YELLOW: Single read _GetHUE_YELLOW;
end;

[JavaSignature('com/google/android/gms/maps/model/BitmapDescriptorFactory')]
JBitmapDescriptorFactory = interface(JObject)
['{83007807-E8EE-43BD-9312-78943B089FD5}']
end;
TJBitmapDescriptorFactory = class(TJavaGenericImport<JBitmapDescriptorFactoryClass, JBitmapDescriptorFactory>) end;

JPolylineOptionsCreatorClass = interface(JObjectClass)
['{B9E9D567-F636-4201-B628-1311E77AE1E2}']
  {Property Methods}
  function _GetCONTENT_DESCRIPTION: Integer;
  {Methods}
  function init: JPolylineOptionsCreator; cdecl;
  {Properties}
  property CONTENT_DESCRIPTION: Integer read _GetCONTENT_DESCRIPTION;
end;

[JavaSignature('com/google/android/gms/maps/model/PolylineOptionsCreator')]
JPolylineOptionsCreator = interface(JObject)
['{70DBFF19-B3CA-4C01-9995-38FE447E483E}']
  {Methods}
  function createFromParcel(parcel: JParcel): JPolylineOptions; cdecl;
  function newArray(size: Integer): TJavaObjectArray<JPolylineOptions>; cdecl;
end;
TJPolylineOptionsCreator = class(TJavaGenericImport<JPolylineOptionsCreatorClass, JPolylineOptionsCreator>) end;

JSafeParcelableClass = interface(JParcelableClass)
['{865435AB-F2E1-4DEF-B775-17CC0CAC9AED}']
  {Property Methods}
  function _GetNULL: JString;
  {Properties}
  property NULL: JString read _GetNULL;
end;

[JavaSignature('com/google/android/gms/common/internal/safeparcel/SafeParcelable')]
JSafeParcelable = interface(JParcelable)
['{0CBA956D-6EA5-41D8-8AFC-42759ADFD37B}']
end;
TJSafeParcelable = class(TJavaGenericImport<JSafeParcelableClass, JSafeParcelable>) end;

JLeaderboardScoreClass = interface(JFreezableClass)
['{0326A2C8-D2D2-4F8D-BE0F-07703BB93916}']
end;

[JavaSignature('com/google/android/gms/games/leaderboard/LeaderboardScore')]
JLeaderboardScore = interface(JFreezable)
['{11C06C5D-3D5E-4A10-A455-36A0F06D3206}']
  {Methods}
  function getDisplayRank: JString; cdecl; overload;
  procedure getDisplayRank(paramCharArrayBuffer: Jdatabase_CharArrayBuffer); cdecl; overload;
  function getDisplayScore: JString; cdecl; overload;
  procedure getDisplayScore(paramCharArrayBuffer: Jdatabase_CharArrayBuffer); cdecl; overload;
  function getRank: Int64; cdecl;
  function getRawScore: Int64; cdecl;
  function getScoreHolder: JPlayer; cdecl;
  function getScoreHolderDisplayName: JString; cdecl; overload;
  procedure getScoreHolderDisplayName(paramCharArrayBuffer: Jdatabase_CharArrayBuffer); cdecl; overload;
  function getScoreHolderHiResImageUri: Jnet_Uri; cdecl;
  function getScoreHolderIconImageUri: Jnet_Uri; cdecl;
  function getTimestampMillis: Int64; cdecl;
end;
TJLeaderboardScore = class(TJavaGenericImport<JLeaderboardScoreClass, JLeaderboardScore>) end;

JItemScopeClass = interface(JFreezableClass)
['{236B5A11-B17F-4B73-AB0B-3CEE2FD976B6}']
end;

[JavaSignature('com/google/android/gms/plus/model/moments/ItemScope')]
JItemScope = interface(JFreezable)
['{B5E3B8DA-655E-4DE0-9E9E-C2E2EAD3A645}']
  {Methods}
  function getAbout: JItemScope; cdecl;
  function getAdditionalName: JList; cdecl;
  function getAddress: JItemScope; cdecl;
  function getAddressCountry: JString; cdecl;
  function getAddressLocality: JString; cdecl;
  function getAddressRegion: JString; cdecl;
  function getAssociated_media: JList; cdecl;
  function getAttendeeCount: Integer; cdecl;
  function getAttendees: JList; cdecl;
  function getAudio: JItemScope; cdecl;
  function getAuthor: JList; cdecl;
  function getBestRating: JString; cdecl;
  function getBirthDate: JString; cdecl;
  function getByArtist: JItemScope; cdecl;
  function getCaption: JString; cdecl;
  function getContentSize: JString; cdecl;
  function getContentUrl: JString; cdecl;
  function getContributor: JList; cdecl;
  function getDateCreated: JString; cdecl;
  function getDateModified: JString; cdecl;
  function getDatePublished: JString; cdecl;
  function getDescription: JString; cdecl;
  function getDuration: JString; cdecl;
  function getEmbedUrl: JString; cdecl;
  function getEndDate: JString; cdecl;
  function getFamilyName: JString; cdecl;
  function getGender: JString; cdecl;
  function getGeo: JItemScope; cdecl;
  function getGivenName: JString; cdecl;
  function getHeight: JString; cdecl;
  function getId: JString; cdecl;
  function getImage: JString; cdecl;
  function getInAlbum: JItemScope; cdecl;
  function getLatitude: Double; cdecl;
  function getLocation: JItemScope; cdecl;
  function getLongitude: Double; cdecl;
  function getName: JString; cdecl;
  function getPartOfTVSeries: JItemScope; cdecl;
  function getPerformers: JList; cdecl;
  function getPlayerType: JString; cdecl;
  function getPostOfficeBoxNumber: JString; cdecl;
  function getPostalCode: JString; cdecl;
  function getRatingValue: JString; cdecl;
  function getReviewRating: JItemScope; cdecl;
  function getStartDate: JString; cdecl;
  function getStreetAddress: JString; cdecl;
  function getText: JString; cdecl;
  function getThumbnail: JItemScope; cdecl;
  function getThumbnailUrl: JString; cdecl;
  function getTickerSymbol: JString; cdecl;
  function getType: JString; cdecl;
  function getUrl: JString; cdecl;
  function getWidth: JString; cdecl;
  function getWorstRating: JString; cdecl;
  function hasAbout: Boolean; cdecl;
  function hasAdditionalName: Boolean; cdecl;
  function hasAddress: Boolean; cdecl;
  function hasAddressCountry: Boolean; cdecl;
  function hasAddressLocality: Boolean; cdecl;
  function hasAddressRegion: Boolean; cdecl;
  function hasAssociated_media: Boolean; cdecl;
  function hasAttendeeCount: Boolean; cdecl;
  function hasAttendees: Boolean; cdecl;
  function hasAudio: Boolean; cdecl;
  function hasAuthor: Boolean; cdecl;
  function hasBestRating: Boolean; cdecl;
  function hasBirthDate: Boolean; cdecl;
  function hasByArtist: Boolean; cdecl;
  function hasCaption: Boolean; cdecl;
  function hasContentSize: Boolean; cdecl;
  function hasContentUrl: Boolean; cdecl;
  function hasContributor: Boolean; cdecl;
  function hasDateCreated: Boolean; cdecl;
  function hasDateModified: Boolean; cdecl;
  function hasDatePublished: Boolean; cdecl;
  function hasDescription: Boolean; cdecl;
  function hasDuration: Boolean; cdecl;
  function hasEmbedUrl: Boolean; cdecl;
  function hasEndDate: Boolean; cdecl;
  function hasFamilyName: Boolean; cdecl;
  function hasGender: Boolean; cdecl;
  function hasGeo: Boolean; cdecl;
  function hasGivenName: Boolean; cdecl;
  function hasHeight: Boolean; cdecl;
  function hasId: Boolean; cdecl;
  function hasImage: Boolean; cdecl;
  function hasInAlbum: Boolean; cdecl;
  function hasLatitude: Boolean; cdecl;
  function hasLocation: Boolean; cdecl;
  function hasLongitude: Boolean; cdecl;
  function hasName: Boolean; cdecl;
  function hasPartOfTVSeries: Boolean; cdecl;
  function hasPerformers: Boolean; cdecl;
  function hasPlayerType: Boolean; cdecl;
  function hasPostOfficeBoxNumber: Boolean; cdecl;
  function hasPostalCode: Boolean; cdecl;
  function hasRatingValue: Boolean; cdecl;
  function hasReviewRating: Boolean; cdecl;
  function hasStartDate: Boolean; cdecl;
  function hasStreetAddress: Boolean; cdecl;
  function hasText: Boolean; cdecl;
  function hasThumbnail: Boolean; cdecl;
  function hasThumbnailUrl: Boolean; cdecl;
  function hasTickerSymbol: Boolean; cdecl;
  function hasType: Boolean; cdecl;
  function hasUrl: Boolean; cdecl;
  function hasWidth: Boolean; cdecl;
  function hasWorstRating: Boolean; cdecl;
end;
TJItemScope = class(TJavaGenericImport<JItemScopeClass, JItemScope>) end;

JPlusClient_bClass = interface(IJavaClass)
['{EEE188FA-2A91-4525-A705-B91E8F297CFD}']
end;

[JavaSignature('com/google/android/gms/plus/PlusClient$b')]
JPlusClient_b = interface(IJavaInstance)
['{76320AE9-B47C-45F0-B70E-13398FEFFB5D}']
  {Methods}
  procedure a(paramConnectionResult: JConnectionResult; parameg: JObject); cdecl;
end;
TJPlusClient_b = class(TJavaGenericImport<JPlusClient_bClass, JPlusClient_b>) end;

JGooglePlayServicesClient_OnConnectionFailedListenerClass = interface(IJavaClass)
['{76F1132D-CE19-4A91-B88D-15CCD337CEA6}']
end;

[JavaSignature('com/google/android/gms/common/GooglePlayServicesClient$OnConnectionFailedListener')]
JGooglePlayServicesClient_OnConnectionFailedListener = interface(IJavaInstance)
['{00602635-A990-45A8-AE1E-037B00F8AD9B}']
  {Methods}
  procedure onConnectionFailed(paramConnectionResult: JConnectionResult); cdecl;
end;
TJGooglePlayServicesClient_OnConnectionFailedListener = class(TJavaGenericImport<JGooglePlayServicesClient_OnConnectionFailedListenerClass, JGooglePlayServicesClient_OnConnectionFailedListener>) end;

JRoomConfigClass = interface(JObjectClass)
['{F272FA9D-AE37-41CD-ABD1-62727128BCF8}']
  {Methods}
  function builder(listener: JRoomUpdateListener): JRoomConfig_Builder; cdecl;
  function createAutoMatchCriteria(minAutoMatchPlayers: Integer; maxAutoMatchPlayers: Integer; exclusiveBitMask: Int64): JBundle; cdecl;
end;

[JavaSignature('com/google/android/gms/games/multiplayer/realtime/RoomConfig')]
JRoomConfig = interface(JObject)
['{CD2AD528-203D-44D5-8DCF-8FAEA3584E38}']
  {Methods}
  function getAutoMatchCriteria: JBundle; cdecl;
  function getInvitationId: JString; cdecl;
  function getInvitedPlayerIds: TJavaObjectArray<JString>; cdecl;
  function getMessageReceivedListener: JRealTimeMessageReceivedListener; cdecl;
  function getRoomStatusUpdateListener: JRoomStatusUpdateListener; cdecl;
  function getRoomUpdateListener: JRoomUpdateListener; cdecl;
  function getVariant: Integer; cdecl;
  function isSocketEnabled: Boolean; cdecl;
end;
TJRoomConfig = class(TJavaGenericImport<JRoomConfigClass, JRoomConfig>) end;

JSignInButtonClass = interface(JFrameLayoutClass)
['{E2871683-3AFF-4060-8EA2-30A5CA64273E}']
  {Property Methods}
  function _GetCOLOR_DARK: Integer;
  function _GetCOLOR_LIGHT: Integer;
  function _GetSIZE_ICON_ONLY: Integer;
  function _GetSIZE_STANDARD: Integer;
  function _GetSIZE_WIDE: Integer;
  {Methods}
  function init(context: JContext): JSignInButton; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet): JSignInButton; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet; defStyle: Integer): JSignInButton; cdecl; overload;
  {Properties}
  property COLOR_DARK: Integer read _GetCOLOR_DARK;
  property COLOR_LIGHT: Integer read _GetCOLOR_LIGHT;
  property SIZE_ICON_ONLY: Integer read _GetSIZE_ICON_ONLY;
  property SIZE_STANDARD: Integer read _GetSIZE_STANDARD;
  property SIZE_WIDE: Integer read _GetSIZE_WIDE;
end;

[JavaSignature('com/google/android/gms/common/SignInButton')]
JSignInButton = interface(JFrameLayout)
['{3E380F90-B958-4997-B949-C77CDACDB789}']
  {Methods}
  procedure onClick(view: JView); cdecl;
  procedure setColorScheme(colorScheme: Integer); cdecl;
  procedure setEnabled(enabled: Boolean); cdecl;
  procedure setOnClickListener(listener: JView_OnClickListener); cdecl;
  procedure setSize(buttonSize: Integer); cdecl;
  procedure setStyle(buttonSize: Integer; colorScheme: Integer); cdecl;
end;
TJSignInButton = class(TJavaGenericImport<JSignInButtonClass, JSignInButton>) end;

JGamesClient_BuilderClass = interface(JObjectClass)
['{97489F11-15F3-46B5-A046-CEBBEF96AE38}']
  {Methods}
  function init(context: JContext; connectedListener: JGooglePlayServicesClient_ConnectionCallbacks; connectionFailedListener: JGooglePlayServicesClient_OnConnectionFailedListener): JGamesClient_Builder; cdecl;
end;

[JavaSignature('com/google/android/gms/games/GamesClient$Builder')]
JGamesClient_Builder = interface(JObject)
['{0B1428FD-2564-4CA3-BA90-590455D509D7}']
  {Methods}
  function create: JGamesClient; cdecl;
  function setAccountName(accountName: JString): JGamesClient_Builder; cdecl;
  function setGravityForPopups(gravity: Integer): JGamesClient_Builder; cdecl;
  function setScopes(scopes: TJavaObjectArray<JString>): JGamesClient_Builder; cdecl;
  function setViewForPopups(gamesContentView: JView): JGamesClient_Builder; cdecl;
end;
TJGamesClient_Builder = class(TJavaGenericImport<JGamesClient_BuilderClass, JGamesClient_Builder>) end;

JRealTimeReliableMessageSentListenerClass = interface(IJavaClass)
['{D1022E15-BA04-4582-9BD0-D591891E0CF2}']
end;

[JavaSignature('com/google/android/gms/games/multiplayer/realtime/RealTimeReliableMessageSentListener')]
JRealTimeReliableMessageSentListener = interface(IJavaInstance)
['{DB851520-3499-4A96-9DDB-6163C121CBCC}']
  {Methods}
  procedure onRealTimeMessageSent(paramInt1: Integer; paramInt2: Integer; paramString: JString); cdecl;
end;
TJRealTimeReliableMessageSentListener = class(TJavaGenericImport<JRealTimeReliableMessageSentListenerClass, JRealTimeReliableMessageSentListener>) end;

JActivityRecognitionClientClass = interface(JObjectClass)
['{95B0BA47-C41F-422C-BDB9-5E46A67A208E}']
  {Methods}
  function init(context: JContext; connectedListener: JGooglePlayServicesClient_ConnectionCallbacks; connectionFailedListener: JGooglePlayServicesClient_OnConnectionFailedListener): JActivityRecognitionClient; cdecl;
end;

[JavaSignature('com/google/android/gms/location/ActivityRecognitionClient')]
JActivityRecognitionClient = interface(JObject)
['{2F41C8A5-18B8-495C-B359-D9BE961E5AD3}']
  {Methods}
  procedure connect; cdecl;
  procedure disconnect; cdecl;
  function isConnected: Boolean; cdecl;
  function isConnecting: Boolean; cdecl;
  function isConnectionCallbacksRegistered(listener: JGooglePlayServicesClient_ConnectionCallbacks): Boolean; cdecl;
  function isConnectionFailedListenerRegistered(listener: JGooglePlayServicesClient_OnConnectionFailedListener): Boolean; cdecl;
  procedure registerConnectionCallbacks(listener: JGooglePlayServicesClient_ConnectionCallbacks); cdecl;
  procedure registerConnectionFailedListener(listener: JGooglePlayServicesClient_OnConnectionFailedListener); cdecl;
  procedure removeActivityUpdates(callbackIntent: JPendingIntent); cdecl;
  procedure requestActivityUpdates(detectionIntervalMillis: Int64; callbackIntent: JPendingIntent); cdecl;
  procedure unregisterConnectionCallbacks(listener: JGooglePlayServicesClient_ConnectionCallbacks); cdecl;
  procedure unregisterConnectionFailedListener(listener: JGooglePlayServicesClient_OnConnectionFailedListener); cdecl;
end;
TJActivityRecognitionClient = class(TJavaGenericImport<JActivityRecognitionClientClass, JActivityRecognitionClient>) end;

JRealTimeSocketClass = interface(IJavaClass)
['{E31FF8F0-8D9D-465F-B49A-07591DE337D0}']
end;

[JavaSignature('com/google/android/gms/games/RealTimeSocket')]
JRealTimeSocket = interface(IJavaInstance)
['{8BECC298-10B2-4E90-80F1-478E0CD67451}']
  {Methods}
  procedure close; cdecl;
  function getInputStream: JInputStream; cdecl;
  function getOutputStream: JOutputStream; cdecl;
  function getParcelFileDescriptor: JParcelFileDescriptor; cdecl;
  function isClosed: Boolean; cdecl;
end;
TJRealTimeSocket = class(TJavaGenericImport<JRealTimeSocketClass, JRealTimeSocket>) end;

JGoogleMap_OnMapClickListenerClass = interface(IJavaClass)
['{0964A15E-AF26-4F35-8AC8-BB937E0A68B5}']
end;

[JavaSignature('com/google/android/gms/maps/GoogleMap$OnMapClickListener')]
JGoogleMap_OnMapClickListener = interface(IJavaInstance)
['{B1E14548-02B7-4168-AEB6-26A64E23FC91}']
  {Methods}
  procedure onMapClick(paramLatLng: JLatLng); cdecl;
end;
TJGoogleMap_OnMapClickListener = class(TJavaGenericImport<JGoogleMap_OnMapClickListenerClass, JGoogleMap_OnMapClickListener>) end;

JInvitationEntityCreatorClass = interface(JObjectClass)
['{D000BA67-3D4A-4AEC-8C6E-EB53C059E8E7}']
  {Property Methods}
  function _GetCONTENT_DESCRIPTION: Integer;
  {Methods}
  function init: JInvitationEntityCreator; cdecl;
  {Properties}
  property CONTENT_DESCRIPTION: Integer read _GetCONTENT_DESCRIPTION;
end;

[JavaSignature('com/google/android/gms/games/multiplayer/InvitationEntityCreator')]
JInvitationEntityCreator = interface(JObject)
['{86C695D0-042A-43D2-A9F5-FA195D607F9A}']
  {Methods}
  function createFromParcel(parcel: JParcel): JInvitationEntity; cdecl;
  function newArray(size: Integer): TJavaObjectArray<JInvitationEntity>; cdecl;
end;
TJInvitationEntityCreator = class(TJavaGenericImport<JInvitationEntityCreatorClass, JInvitationEntityCreator>) end;

JGameEntityCreatorClass = interface(JObjectClass)
['{219E535A-45A6-4C5E-93F4-1B5BC9437AD2}']
  {Property Methods}
  function _GetCONTENT_DESCRIPTION: Integer;
  {Methods}
  function init: JGameEntityCreator; cdecl;
  {Properties}
  property CONTENT_DESCRIPTION: Integer read _GetCONTENT_DESCRIPTION;
end;

[JavaSignature('com/google/android/gms/games/GameEntityCreator')]
JGameEntityCreator = interface(JObject)
['{37BB8279-117C-4F51-AA66-360F8BEC9F3D}']
  {Methods}
  function createFromParcel(parcel: JParcel): JGameEntity; cdecl;
  function newArray(size: Integer): TJavaObjectArray<JGameEntity>; cdecl;
end;
TJGameEntityCreator = class(TJavaGenericImport<JGameEntityCreatorClass, JGameEntityCreator>) end;

JMarkerOptionsClass = interface(JObjectClass)
['{BDD0C13B-A5CE-4FD0-85E0-268A9170847F}']
  {Property Methods}
  function _GetCREATOR: JMarkerOptionsCreator;
  {Methods}
  function init: JMarkerOptions; cdecl;
  {Properties}
  property CREATOR: JMarkerOptionsCreator read _GetCREATOR;
end;

[JavaSignature('com/google/android/gms/maps/model/MarkerOptions')]
JMarkerOptions = interface(JObject)
['{60B30661-9D1C-4E6F-999B-0F7F13556E9B}']
  {Methods}
  function F: Integer; cdecl;
  function anchor(u: Single; v: Single): JMarkerOptions; cdecl;
  function bp: JIBinder; cdecl;
  function describeContents: Integer; cdecl;
  function draggable(draggable: Boolean): JMarkerOptions; cdecl;
  function getAnchorU: Single; cdecl;
  function getAnchorV: Single; cdecl;
  function getIcon: JBitmapDescriptor; cdecl;
  function getPosition: JLatLng; cdecl;
  function getSnippet: JString; cdecl;
  function getTitle: JString; cdecl;
  function icon(icon: JBitmapDescriptor): JMarkerOptions; cdecl;
  function isDraggable: Boolean; cdecl;
  function isVisible: Boolean; cdecl;
  function position(position: JLatLng): JMarkerOptions; cdecl;
  function snippet(snippet: JString): JMarkerOptions; cdecl;
  function title(title: JString): JMarkerOptions; cdecl;
  function visible(visible: Boolean): JMarkerOptions; cdecl;
  procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
end;
TJMarkerOptions = class(TJavaGenericImport<JMarkerOptionsClass, JMarkerOptions>) end;

JAchievementBufferClass = interface(JDataBufferClass)
['{14DB1E4E-61BF-4F67-9BC4-A300EB9D88A8}']
  {Methods}
  function init(dataHolder: JObject): JAchievementBuffer; cdecl;
end;

[JavaSignature('com/google/android/gms/games/achievement/AchievementBuffer')]
JAchievementBuffer = interface(JDataBuffer)
['{F91632A1-72D5-490F-9A8D-244A168006E1}']
  {Methods}
  function &get(position: Integer): JAchievement; cdecl;
end;
TJAchievementBuffer = class(TJavaGenericImport<JAchievementBufferClass, JAchievementBuffer>) end;

JLeaderboardVariantClass = interface(IJavaClass)
['{3680A21A-CA98-4D19-9A6F-BE9A3BBC67B3}']
  {Property Methods}
  function _GetCOLLECTION_PUBLIC: Integer;
  function _GetCOLLECTION_SOCIAL: Integer;
  function _GetNUM_SCORES_UNKNOWN: Integer;
  function _GetNUM_TIME_SPANS: Integer;
  function _GetPLAYER_RANK_UNKNOWN: Integer;
  function _GetPLAYER_SCORE_UNKNOWN: Integer;
  function _GetTIME_SPAN_ALL_TIME: Integer;
  function _GetTIME_SPAN_DAILY: Integer;
  function _GetTIME_SPAN_WEEKLY: Integer;
  {Properties}
  property COLLECTION_PUBLIC: Integer read _GetCOLLECTION_PUBLIC;
  property COLLECTION_SOCIAL: Integer read _GetCOLLECTION_SOCIAL;
  property NUM_SCORES_UNKNOWN: Integer read _GetNUM_SCORES_UNKNOWN;
  property NUM_TIME_SPANS: Integer read _GetNUM_TIME_SPANS;
  property PLAYER_RANK_UNKNOWN: Integer read _GetPLAYER_RANK_UNKNOWN;
  property PLAYER_SCORE_UNKNOWN: Integer read _GetPLAYER_SCORE_UNKNOWN;
  property TIME_SPAN_ALL_TIME: Integer read _GetTIME_SPAN_ALL_TIME;
  property TIME_SPAN_DAILY: Integer read _GetTIME_SPAN_DAILY;
  property TIME_SPAN_WEEKLY: Integer read _GetTIME_SPAN_WEEKLY;
end;

[JavaSignature('com/google/android/gms/games/leaderboard/LeaderboardVariant')]
JLeaderboardVariant = interface(IJavaInstance)
['{7A5DDBB0-946F-4D3D-BE8D-317984BA5E31}']
  {Methods}
  function getCollection: Integer; cdecl;
  function getDisplayPlayerRank: JString; cdecl;
  function getDisplayPlayerScore: JString; cdecl;
  function getNumScores: Int64; cdecl;
  function getPlayerRank: Int64; cdecl;
  function getRawPlayerScore: Int64; cdecl;
  function getTimeSpan: Integer; cdecl;
  function hasPlayerInfo: Boolean; cdecl;
end;
TJLeaderboardVariant = class(TJavaGenericImport<JLeaderboardVariantClass, JLeaderboardVariant>) end;

JUserRecoverableAuthExceptionClass = interface(JGoogleAuthExceptionClass)
['{B0EB85A3-956F-4683-96D6-6DE6F6269EC8}']
  {Methods}
  function init(msg: JString; intent: JIntent): JUserRecoverableAuthException; cdecl;
end;

[JavaSignature('com/google/android/gms/auth/UserRecoverableAuthException')]
JUserRecoverableAuthException = interface(JGoogleAuthException)
['{078C9325-3395-4A9E-A68B-06CC6812B83E}']
  {Methods}
  function getIntent: JIntent; cdecl;
end;
TJUserRecoverableAuthException = class(TJavaGenericImport<JUserRecoverableAuthExceptionClass, JUserRecoverableAuthException>) end;

JGooglePlayServicesAvailabilityExceptionClass = interface(JUserRecoverableAuthExceptionClass)
['{A620FBE5-CFF1-4458-9E30-C51FD3900597}']
end;

[JavaSignature('com/google/android/gms/auth/GooglePlayServicesAvailabilityException')]
JGooglePlayServicesAvailabilityException = interface(JUserRecoverableAuthException)
['{46FF4385-2E1E-4195-9018-ED62D4985AB5}']
  {Methods}
  function getConnectionStatusCode: Integer; cdecl;
end;
TJGooglePlayServicesAvailabilityException = class(TJavaGenericImport<JGooglePlayServicesAvailabilityExceptionClass, JGooglePlayServicesAvailabilityException>) end;

JAchievementClass = interface(IJavaClass)
['{6C078B6F-C3B1-4E2F-8581-0EE09D429939}']
  {Property Methods}
  function _GetSTATE_HIDDEN: Integer;
  function _GetSTATE_REVEALED: Integer;
  function _GetSTATE_UNLOCKED: Integer;
  function _GetTYPE_INCREMENTAL: Integer;
  function _GetTYPE_STANDARD: Integer;
  {Properties}
  property STATE_HIDDEN: Integer read _GetSTATE_HIDDEN;
  property STATE_REVEALED: Integer read _GetSTATE_REVEALED;
  property STATE_UNLOCKED: Integer read _GetSTATE_UNLOCKED;
  property TYPE_INCREMENTAL: Integer read _GetTYPE_INCREMENTAL;
  property TYPE_STANDARD: Integer read _GetTYPE_STANDARD;
end;

[JavaSignature('com/google/android/gms/games/achievement/Achievement')]
JAchievement = interface(IJavaInstance)
['{D35D3651-5E50-47C1-89DF-3081A179593F}']
  {Methods}
  function getAchievementId: JString; cdecl;
  function getCurrentSteps: Integer; cdecl;
  function getDescription: JString; cdecl; overload;
  procedure getDescription(paramCharArrayBuffer: Jdatabase_CharArrayBuffer); cdecl; overload;
  function getFormattedCurrentSteps: JString; cdecl; overload;
  procedure getFormattedCurrentSteps(paramCharArrayBuffer: Jdatabase_CharArrayBuffer); cdecl; overload;
  function getFormattedTotalSteps: JString; cdecl; overload;
  procedure getFormattedTotalSteps(paramCharArrayBuffer: Jdatabase_CharArrayBuffer); cdecl; overload;
  function getLastUpdatedTimestamp: Int64; cdecl;
  function getName: JString; cdecl; overload;
  procedure getName(paramCharArrayBuffer: Jdatabase_CharArrayBuffer); cdecl; overload;
  function getPlayer: JPlayer; cdecl;
  function getRevealedImageUri: Jnet_Uri; cdecl;
  function getState: Integer; cdecl;
  function getTotalSteps: Integer; cdecl;
  function getType: Integer; cdecl;
  function getUnlockedImageUri: Jnet_Uri; cdecl;
end;
TJAchievement = class(TJavaGenericImport<JAchievementClass, JAchievement>) end;

JPolygonOptionsCreatorClass = interface(JObjectClass)
['{451873CB-B4E7-4D10-AE9A-D8C2CDAF40BC}']
  {Property Methods}
  function _GetCONTENT_DESCRIPTION: Integer;
  {Methods}
  function init: JPolygonOptionsCreator; cdecl;
  {Properties}
  property CONTENT_DESCRIPTION: Integer read _GetCONTENT_DESCRIPTION;
end;

[JavaSignature('com/google/android/gms/maps/model/PolygonOptionsCreator')]
JPolygonOptionsCreator = interface(JObject)
['{FEF5C156-01D6-4A8B-96CF-A4330BE7FDB5}']
  {Methods}
  function createFromParcel(parcel: JParcel): JPolygonOptions; cdecl;
  function newArray(size: Integer): TJavaObjectArray<JPolygonOptions>; cdecl;
end;
TJPolygonOptionsCreator = class(TJavaGenericImport<JPolygonOptionsCreatorClass, JPolygonOptionsCreator>) end;

JPerson_OrganizationsClass = interface(JFreezableClass)
['{518FDD9A-EFE3-4253-8BE6-7EF263AD3213}']
end;

[JavaSignature('com/google/android/gms/plus/model/people/Person$Organizations')]
JPerson_Organizations = interface(JFreezable)
['{E6C4E8DE-9009-459D-956F-6C8440A3D70B}']
  {Methods}
  function getDepartment: JString; cdecl;
  function getDescription: JString; cdecl;
  function getEndDate: JString; cdecl;
  function getLocation: JString; cdecl;
  function getName: JString; cdecl;
  function getStartDate: JString; cdecl;
  function getTitle: JString; cdecl;
  function getType: Integer; cdecl;
  function hasDepartment: Boolean; cdecl;
  function hasDescription: Boolean; cdecl;
  function hasEndDate: Boolean; cdecl;
  function hasLocation: Boolean; cdecl;
  function hasName: Boolean; cdecl;
  function hasPrimary: Boolean; cdecl;
  function hasStartDate: Boolean; cdecl;
  function hasTitle: Boolean; cdecl;
  function hasType: Boolean; cdecl;
  function isPrimary: Boolean; cdecl;
end;
TJPerson_Organizations = class(TJavaGenericImport<JPerson_OrganizationsClass, JPerson_Organizations>) end;

JScopesClass = interface(JObjectClass)
['{1044C4C8-7758-4235-AECE-92D3CD0D433A}']
  {Property Methods}
  function _GetAPP_STATE: JString;
  function _GetGAMES: JString;
  function _GetPLUS_LOGIN: JString;
  function _GetPLUS_PROFILE: JString;
  {Methods}
  function init: JScopes; cdecl;
  {Properties}
  property APP_STATE: JString read _GetAPP_STATE;
  property GAMES: JString read _GetGAMES;
  property PLUS_LOGIN: JString read _GetPLUS_LOGIN;
  property PLUS_PROFILE: JString read _GetPLUS_PROFILE;
end;

[JavaSignature('com/google/android/gms/common/Scopes')]
JScopes = interface(JObject)
['{A9A538A4-79B6-4891-B22A-D5487A03C1CF}']
end;
TJScopes = class(TJavaGenericImport<JScopesClass, JScopes>) end;

JRoomConfig_BuilderClass = interface(JObjectClass)
['{A8118363-1188-4D23-B562-AA6C45827C8B}']
end;

[JavaSignature('com/google/android/gms/games/multiplayer/realtime/RoomConfig$Builder')]
JRoomConfig_Builder = interface(JObject)
['{41897BC0-FFF0-4634-90AC-27DDB46B3B48}']
  {Methods}
  function addPlayersToInvite(playerIds: TJavaObjectArray<JString>): JRoomConfig_Builder; cdecl; overload;
  function addPlayersToInvite(playerIds: JArrayList): JRoomConfig_Builder; cdecl; overload;
  function build: JRoomConfig; cdecl;
  function setAutoMatchCriteria(autoMatchCriteria: JBundle): JRoomConfig_Builder; cdecl;
  function setInvitationIdToAccept(invitationId: JString): JRoomConfig_Builder; cdecl;
  function setMessageReceivedListener(listener: JRealTimeMessageReceivedListener): JRoomConfig_Builder; cdecl;
  function setRoomStatusUpdateListener(listener: JRoomStatusUpdateListener): JRoomConfig_Builder; cdecl;
  function setSocketCommunicationEnabled(enableSockets: Boolean): JRoomConfig_Builder; cdecl;
  function setVariant(variant: Integer): JRoomConfig_Builder; cdecl;
end;
TJRoomConfig_Builder = class(TJavaGenericImport<JRoomConfig_BuilderClass, JRoomConfig_Builder>) end;

JTileCreatorClass = interface(JObjectClass)
['{D8F0B5B9-8A38-48EA-BE7E-C66EE65155DE}']
  {Property Methods}
  function _GetCONTENT_DESCRIPTION: Integer;
  {Methods}
  function init: JTileCreator; cdecl;
  {Properties}
  property CONTENT_DESCRIPTION: Integer read _GetCONTENT_DESCRIPTION;
end;

[JavaSignature('com/google/android/gms/maps/model/TileCreator')]
JTileCreator = interface(JObject)
['{D3FF7500-ABCA-4029-A885-7BCBC7411DE1}']
  {Methods}
  function createFromParcel(parcel: JParcel): JTile; cdecl;
  function newArray(size: Integer): TJavaObjectArray<JTile>; cdecl;
end;
TJTileCreator = class(TJavaGenericImport<JTileCreatorClass, JTileCreator>) end;

JPolylineOptionsClass = interface(JObjectClass)
['{73CA0FF0-3ECB-493C-822E-0D9E24EDD021}']
  {Property Methods}
  function _GetCREATOR: JPolylineOptionsCreator;
  {Methods}
  function init: JPolylineOptions; cdecl;
  {Properties}
  property CREATOR: JPolylineOptionsCreator read _GetCREATOR;
end;

[JavaSignature('com/google/android/gms/maps/model/PolylineOptions')]
JPolylineOptions = interface(JObject)
['{029E45C1-C7B8-4FDB-B575-B222EC3DE59F}']
  {Methods}
  function F: Integer; cdecl;
  function add(point: JLatLng): JPolylineOptions; cdecl; overload;
  function add(points: TJavaObjectArray<JLatLng>): JPolylineOptions; cdecl; overload;
  function addAll(points: JIterable): JPolylineOptions; cdecl;
  function color(color: Integer): JPolylineOptions; cdecl;
  function describeContents: Integer; cdecl;
  function geodesic(geodesic: Boolean): JPolylineOptions; cdecl;
  function getColor: Integer; cdecl;
  function getPoints: JList; cdecl;
  function getWidth: Single; cdecl;
  function getZIndex: Single; cdecl;
  function isGeodesic: Boolean; cdecl;
  function isVisible: Boolean; cdecl;
  function visible(visible: Boolean): JPolylineOptions; cdecl;
  function width(width: Single): JPolylineOptions; cdecl;
  procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
  function zIndex(zIndex: Single): JPolylineOptions; cdecl;
end;
TJPolylineOptions = class(TJavaGenericImport<JPolylineOptionsClass, JPolylineOptions>) end;

JActivityRecognitionResultClass = interface(JObjectClass)
['{454FB055-9DBD-42AB-898C-FEA2F58D210B}']
  {Property Methods}
  function _GetCREATOR: JActivityRecognitionResultCreator;
  function _GetEXTRA_ACTIVITY_RESULT: JString;
  {Methods}
  function init(probableActivities: JList; time: Int64; elapsedRealtimeMillis: Int64): JActivityRecognitionResult; cdecl; overload;
  function init(mostProbableActivity: JDetectedActivity; time: Int64; elapsedRealtimeMillis: Int64): JActivityRecognitionResult; cdecl; overload;
  function init: JActivityRecognitionResult; cdecl; overload;
  function extractResult(intent: JIntent): JActivityRecognitionResult; cdecl;
  function hasResult(intent: JIntent): Boolean; cdecl;
  {Properties}
  property CREATOR: JActivityRecognitionResultCreator read _GetCREATOR;
  property EXTRA_ACTIVITY_RESULT: JString read _GetEXTRA_ACTIVITY_RESULT;
end;

[JavaSignature('com/google/android/gms/location/ActivityRecognitionResult')]
JActivityRecognitionResult = interface(JObject)
['{D9E1E70E-2A71-4999-9A98-8A3FC0137BB3}']
  {Methods}
  function describeContents: Integer; cdecl;
  function getActivityConfidence(activityType: Integer): Integer; cdecl;
  function getElapsedRealtimeMillis: Int64; cdecl;
  function getMostProbableActivity: JDetectedActivity; cdecl;
  function getProbableActivities: JList; cdecl;
  function getTime: Int64; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
end;
TJActivityRecognitionResult = class(TJavaGenericImport<JActivityRecognitionResultClass, JActivityRecognitionResult>) end;

JGoogleMap_OnMyLocationButtonClickListenerClass = interface(IJavaClass)
['{69815418-ECFB-4F8B-8CCF-062E5BA9DCAD}']
end;

[JavaSignature('com/google/android/gms/maps/GoogleMap$OnMyLocationButtonClickListener')]
JGoogleMap_OnMyLocationButtonClickListener = interface(IJavaInstance)
['{BE4965DB-A640-42DA-9AD2-1F96425737F1}']
  {Methods}
  function onMyLocationButtonClick: Boolean; cdecl;
end;
TJGoogleMap_OnMyLocationButtonClickListener = class(TJavaGenericImport<JGoogleMap_OnMyLocationButtonClickListenerClass, JGoogleMap_OnMyLocationButtonClickListener>) end;

JGoogleMapOptionsCreatorClass = interface(JObjectClass)
['{68020886-D22F-40EC-AEBD-8AA7C4AC9F82}']
  {Property Methods}
  function _GetCONTENT_DESCRIPTION: Integer;
  {Methods}
  function init: JGoogleMapOptionsCreator; cdecl;
  {Properties}
  property CONTENT_DESCRIPTION: Integer read _GetCONTENT_DESCRIPTION;
end;

[JavaSignature('com/google/android/gms/maps/GoogleMapOptionsCreator')]
JGoogleMapOptionsCreator = interface(JObject)
['{D51A1302-2923-4015-9A0D-1D9AFE11B17B}']
  {Methods}
  function createFromParcel(parcel: JParcel): JGoogleMapOptions; cdecl;
  function newArray(size: Integer): TJavaObjectArray<JGoogleMapOptions>; cdecl;
end;
TJGoogleMapOptionsCreator = class(TJavaGenericImport<JGoogleMapOptionsCreatorClass, JGoogleMapOptionsCreator>) end;

JGoogleMap_SnapshotReadyCallbackClass = interface(IJavaClass)
['{61B0A354-3EDF-4C4C-AFA1-31C9ECD7E7CD}']
end;

[JavaSignature('com/google/android/gms/maps/GoogleMap$SnapshotReadyCallback')]
JGoogleMap_SnapshotReadyCallback = interface(IJavaInstance)
['{53C58A5E-B8D3-4999-924B-8D053025EA12}']
  {Methods}
  procedure onSnapshotReady(paramBitmap: JBitmap); cdecl;
end;
TJGoogleMap_SnapshotReadyCallback = class(TJavaGenericImport<JGoogleMap_SnapshotReadyCallbackClass, JGoogleMap_SnapshotReadyCallback>) end;

JGooglePlayServicesNotAvailableExceptionClass = interface(JExceptionClass)
['{B8297902-69BD-468A-BDB6-9E45162CD60B}']
  {Methods}
  function init(errorCode: Integer): JGooglePlayServicesNotAvailableException; cdecl;
end;

[JavaSignature('com/google/android/gms/common/GooglePlayServicesNotAvailableException')]
JGooglePlayServicesNotAvailableException = interface(JException)
['{9F44824B-11B1-42A6-B5B4-D0660A1EA463}']
  {Property Methods}
  function _GeterrorCode: Integer;
  {Properties}
  property errorCode: Integer read _GeterrorCode;
end;
TJGooglePlayServicesNotAvailableException = class(TJavaGenericImport<JGooglePlayServicesNotAvailableExceptionClass, JGooglePlayServicesNotAvailableException>) end;

JPerson_AgeRangeClass = interface(JFreezableClass)
['{755F2210-AB4C-4A53-B17E-3D9CC136167A}']
end;

[JavaSignature('com/google/android/gms/plus/model/people/Person$AgeRange')]
JPerson_AgeRange = interface(JFreezable)
['{F89766AD-8608-40F8-984E-2239413488D2}']
  {Methods}
  function getMax: Integer; cdecl;
  function getMin: Integer; cdecl;
  function hasMax: Boolean; cdecl;
  function hasMin: Boolean; cdecl;
end;
TJPerson_AgeRange = class(TJavaGenericImport<JPerson_AgeRangeClass, JPerson_AgeRange>) end;

JLeaderboardClass = interface(IJavaClass)
['{8CB29239-FC13-4DB2-966D-77D576C18DDC}']
  {Property Methods}
  function _GetSCORE_ORDER_LARGER_IS_BETTER: Integer;
  function _GetSCORE_ORDER_SMALLER_IS_BETTER: Integer;
  {Properties}
  property SCORE_ORDER_LARGER_IS_BETTER: Integer read _GetSCORE_ORDER_LARGER_IS_BETTER;
  property SCORE_ORDER_SMALLER_IS_BETTER: Integer read _GetSCORE_ORDER_SMALLER_IS_BETTER;
end;

[JavaSignature('com/google/android/gms/games/leaderboard/Leaderboard')]
JLeaderboard = interface(IJavaInstance)
['{353B79AC-19A2-42D4-AD92-A90808137FC3}']
  {Methods}
  function getDisplayName: JString; cdecl; overload;
  procedure getDisplayName(paramCharArrayBuffer: Jdatabase_CharArrayBuffer); cdecl; overload;
  function getIconImageUri: Jnet_Uri; cdecl;
  function getLeaderboardId: JString; cdecl;
  function getScoreOrder: Integer; cdecl;
  function getVariants: JArrayList; cdecl;
end;
TJLeaderboard = class(TJavaGenericImport<JLeaderboardClass, JLeaderboard>) end;

JInvitationEntityClass = interface(JObjectClass)
['{D3C0EA32-9E5E-4E8C-A24A-748EB93C1020}']
  {Property Methods}
  function _GetCREATOR: JInvitationEntityCreator;
  {Methods}
  function init(invitation: JInvitation): JInvitationEntity; cdecl;
  function a(paramInvitation: JInvitation): Integer; cdecl; overload;
  function a(paramInvitation: JInvitation; paramObject: JObject): Boolean; cdecl; overload;
  function b(paramInvitation: JInvitation): JString; cdecl;
  {Properties}
  property CREATOR: JInvitationEntityCreator read _GetCREATOR;
end;

[JavaSignature('com/google/android/gms/games/multiplayer/InvitationEntity')]
JInvitationEntity = interface(JObject)
['{1C8FCD28-9E08-43C5-B79A-2CDD2418338E}']
  {Methods}
  function F: Integer; cdecl;
  function aJ: Integer; cdecl;
  function describeContents: Integer; cdecl;
  function equals(obj: JObject): Boolean; cdecl;
  function freeze: JInvitation; cdecl;
  function getCreationTimestamp: Int64; cdecl;
  function getGame: JGame; cdecl;
  function getInvitationId: JString; cdecl;
  function getInviter: JParticipant; cdecl;
  function getParticipants: JArrayList; cdecl;
  function getVariant: Integer; cdecl;
  function hashCode: Integer; cdecl;
  function isDataValid: Boolean; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
end;
TJInvitationEntity = class(TJavaGenericImport<JInvitationEntityClass, JInvitationEntity>) end;

JRealTimeMessageClass = interface(JObjectClass)
['{E11642C1-4F49-4298-8B3F-DF46663B9BF2}']
  {Property Methods}
  function _GetCREATOR: JParcelable_Creator;
  function _GetRELIABLE: Integer;
  function _GetUNRELIABLE: Integer;
  {Methods}
  function init(senderParticipantId: JString; messageData: TJavaArray<Byte>; isReliable: Integer): JRealTimeMessage; cdecl;
  {Properties}
  property CREATOR: JParcelable_Creator read _GetCREATOR;
  property RELIABLE: Integer read _GetRELIABLE;
  property UNRELIABLE: Integer read _GetUNRELIABLE;
end;

[JavaSignature('com/google/android/gms/games/multiplayer/realtime/RealTimeMessage')]
JRealTimeMessage = interface(JObject)
['{0CF40854-AF00-4EC0-9922-62E2400A17B0}']
  {Methods}
  function describeContents: Integer; cdecl;
  function getMessageData: TJavaArray<Byte>; cdecl;
  function getSenderParticipantId: JString; cdecl;
  function isReliable: Boolean; cdecl;
  procedure writeToParcel(parcel: JParcel; flag: Integer); cdecl;
end;
TJRealTimeMessage = class(TJavaGenericImport<JRealTimeMessageClass, JRealTimeMessage>) end;

JInvitationBufferClass = interface(JObjectClass)
['{E4868447-B997-4C5B-A57D-D87CDF8EC8DC}']
  {Methods}
  function init(dataHolder: JObject): JInvitationBuffer; cdecl;
end;

[JavaSignature('com/google/android/gms/games/multiplayer/InvitationBuffer')]
JInvitationBuffer = interface(JObject)
['{F0384962-8D56-4FE8-9DD5-74DC54D69C48}']
end;
TJInvitationBuffer = class(TJavaGenericImport<JInvitationBufferClass, JInvitationBuffer>) end;

JLocationSourceClass = interface(IJavaClass)
['{DD46DE05-8B4F-47D2-8910-3B19D7FD9939}']
end;

[JavaSignature('com/google/android/gms/maps/LocationSource')]
JLocationSource = interface(IJavaInstance)
['{DE211F4A-D940-4E56-AAE3-21F7A885E2FE}']
  {Methods}
  procedure activate(paramOnLocationChangedListener: JLocationSource_OnLocationChangedListener); cdecl;
  procedure deactivate; cdecl;
end;
TJLocationSource = class(TJavaGenericImport<JLocationSourceClass, JLocationSource>) end;

JGoogleMapClass = interface(JObjectClass)
['{400A62AC-F9F5-403A-91B0-CCA7F0FCFFA2}']
  {Property Methods}
  function _GetMAP_TYPE_HYBRID: Integer;
  function _GetMAP_TYPE_NONE: Integer;
  function _GetMAP_TYPE_NORMAL: Integer;
  function _GetMAP_TYPE_SATELLITE: Integer;
  function _GetMAP_TYPE_TERRAIN: Integer;
  {Properties}
  property MAP_TYPE_HYBRID: Integer read _GetMAP_TYPE_HYBRID;
  property MAP_TYPE_NONE: Integer read _GetMAP_TYPE_NONE;
  property MAP_TYPE_NORMAL: Integer read _GetMAP_TYPE_NORMAL;
  property MAP_TYPE_SATELLITE: Integer read _GetMAP_TYPE_SATELLITE;
  property MAP_TYPE_TERRAIN: Integer read _GetMAP_TYPE_TERRAIN;
end;

[JavaSignature('com/google/android/gms/maps/GoogleMap')]
JGoogleMap = interface(JObject)
['{A525ED9B-BBC7-4848-9D2B-680E9D578195}']
  {Methods}
  function addCircle(options: JCircleOptions): JCircle; cdecl;
  function addGroundOverlay(options: JGroundOverlayOptions): JGroundOverlay; cdecl;
  function addMarker(options: JMarkerOptions): JMarker; cdecl;
  function addPolygon(options: JPolygonOptions): JPolygon; cdecl;
  function addPolyline(options: JPolylineOptions): JPolyline; cdecl;
  function addTileOverlay(options: JTileOverlayOptions): JTileOverlay; cdecl;
  procedure animateCamera(update: JCameraUpdate); cdecl; overload;
  procedure animateCamera(update: JCameraUpdate; callback: JGoogleMap_CancelableCallback); cdecl; overload;
  procedure animateCamera(update: JCameraUpdate; durationMs: Integer; callback: JGoogleMap_CancelableCallback); cdecl; overload;
  procedure clear; cdecl;
  function getCameraPosition: JCameraPosition; cdecl;
  function getMapType: Integer; cdecl;
  function getMaxZoomLevel: Single; cdecl;
  function getMinZoomLevel: Single; cdecl;
  function getMyLocation: JLocation; cdecl;
  function getProjection: JProjection; cdecl;
  function getUiSettings: JUiSettings; cdecl;
  function isIndoorEnabled: Boolean; cdecl;
  function isMyLocationEnabled: Boolean; cdecl;
  function isTrafficEnabled: Boolean; cdecl;
  procedure moveCamera(update: JCameraUpdate); cdecl;
  function setIndoorEnabled(enabled: Boolean): Boolean; cdecl;
  procedure setInfoWindowAdapter(adapter: JGoogleMap_InfoWindowAdapter); cdecl;
  procedure setLocationSource(source: JLocationSource); cdecl;
  procedure setMapType(type_: Integer); cdecl;
  procedure setMyLocationEnabled(enabled: Boolean); cdecl;
  procedure setOnCameraChangeListener(listener: JGoogleMap_OnCameraChangeListener); cdecl;
  procedure setOnInfoWindowClickListener(listener: JGoogleMap_OnInfoWindowClickListener); cdecl;
  procedure setOnMapClickListener(listener: JGoogleMap_OnMapClickListener); cdecl;
  procedure setOnMapLongClickListener(listener: JGoogleMap_OnMapLongClickListener); cdecl;
  procedure setOnMarkerClickListener(listener: JGoogleMap_OnMarkerClickListener); cdecl;
  procedure setOnMarkerDragListener(listener: JGoogleMap_OnMarkerDragListener); cdecl;
  procedure setOnMyLocationButtonClickListener(listener: JGoogleMap_OnMyLocationButtonClickListener); cdecl;
  procedure setOnMyLocationChangeListener(listener: JGoogleMap_OnMyLocationChangeListener); cdecl;
  procedure setTrafficEnabled(enabled: Boolean); cdecl;
  procedure snapshot(callback: JGoogleMap_SnapshotReadyCallback); cdecl; overload;
  procedure snapshot(callback: JGoogleMap_SnapshotReadyCallback; bitmap: JBitmap); cdecl; overload;
  procedure stopAnimation; cdecl;
end;
TJGoogleMap = class(TJavaGenericImport<JGoogleMapClass, JGoogleMap>) end;

JOnScoreSubmittedListenerClass = interface(IJavaClass)
['{1C3AE8C8-1250-4868-84AE-BF9F81CB8994}']
end;

[JavaSignature('com/google/android/gms/games/leaderboard/OnScoreSubmittedListener')]
JOnScoreSubmittedListener = interface(IJavaInstance)
['{96961BC1-89C4-446D-95A8-E6E94BB314E2}']
  {Methods}
  procedure onScoreSubmitted(paramInt: Integer; paramSubmitScoreResult: JSubmitScoreResult); cdecl;
end;
TJOnScoreSubmittedListener = class(TJavaGenericImport<JOnScoreSubmittedListenerClass, JOnScoreSubmittedListener>) end;

JPlusShareClass = interface(JObjectClass)
['{F5B23902-E272-45DE-B1D8-79F147BC1153}']
  {Property Methods}
  function _GetEXTRA_CALL_TO_ACTION: JString;
  function _GetEXTRA_CONTENT_DEEP_LINK_ID: JString;
  function _GetEXTRA_CONTENT_DEEP_LINK_METADATA: JString;
  function _GetEXTRA_CONTENT_URL: JString;
  function _GetEXTRA_IS_INTERACTIVE_POST: JString;
  function _GetEXTRA_SENDER_ID: JString;
  function _GetKEY_CALL_TO_ACTION_DEEP_LINK_ID: JString;
  function _GetKEY_CALL_TO_ACTION_LABEL: JString;
  function _GetKEY_CALL_TO_ACTION_URL: JString;
  function _GetKEY_CONTENT_DEEP_LINK_METADATA_DESCRIPTION: JString;
  function _GetKEY_CONTENT_DEEP_LINK_METADATA_THUMBNAIL_URL: JString;
  function _GetKEY_CONTENT_DEEP_LINK_METADATA_TITLE: JString;
  function _GetPARAM_CONTENT_DEEP_LINK_ID: JString;
  {Methods}
  function a(paramString1: JString; paramString2: JString; paramUri: Jnet_Uri): JBundle; cdecl;
  function createPerson(id: JString; displayName: JString): JPerson; cdecl;
  function getDeepLinkId(intent: JIntent): JString; cdecl;
  {Properties}
  property EXTRA_CALL_TO_ACTION: JString read _GetEXTRA_CALL_TO_ACTION;
  property EXTRA_CONTENT_DEEP_LINK_ID: JString read _GetEXTRA_CONTENT_DEEP_LINK_ID;
  property EXTRA_CONTENT_DEEP_LINK_METADATA: JString read _GetEXTRA_CONTENT_DEEP_LINK_METADATA;
  property EXTRA_CONTENT_URL: JString read _GetEXTRA_CONTENT_URL;
  property EXTRA_IS_INTERACTIVE_POST: JString read _GetEXTRA_IS_INTERACTIVE_POST;
  property EXTRA_SENDER_ID: JString read _GetEXTRA_SENDER_ID;
  property KEY_CALL_TO_ACTION_DEEP_LINK_ID: JString read _GetKEY_CALL_TO_ACTION_DEEP_LINK_ID;
  property KEY_CALL_TO_ACTION_LABEL: JString read _GetKEY_CALL_TO_ACTION_LABEL;
  property KEY_CALL_TO_ACTION_URL: JString read _GetKEY_CALL_TO_ACTION_URL;
  property KEY_CONTENT_DEEP_LINK_METADATA_DESCRIPTION: JString read _GetKEY_CONTENT_DEEP_LINK_METADATA_DESCRIPTION;
  property KEY_CONTENT_DEEP_LINK_METADATA_THUMBNAIL_URL: JString read _GetKEY_CONTENT_DEEP_LINK_METADATA_THUMBNAIL_URL;
  property KEY_CONTENT_DEEP_LINK_METADATA_TITLE: JString read _GetKEY_CONTENT_DEEP_LINK_METADATA_TITLE;
  property PARAM_CONTENT_DEEP_LINK_ID: JString read _GetPARAM_CONTENT_DEEP_LINK_ID;
end;

[JavaSignature('com/google/android/gms/plus/PlusShare')]
JPlusShare = interface(JObject)
['{B5154807-934B-4AEA-B761-7B3E99A5F5C7}']
end;
TJPlusShare = class(TJavaGenericImport<JPlusShareClass, JPlusShare>) end;

JParticipantUtilsClass = interface(JObjectClass)
['{72CD73A6-E7B1-46F7-9116-7394FDC35A03}']
  {Methods}
  function init: JParticipantUtils; cdecl;
  function getParticipantId(participants: JArrayList; playerId: JString): JString; cdecl;
  function z(paramString: JString): Boolean; cdecl;
end;

[JavaSignature('com/google/android/gms/games/multiplayer/ParticipantUtils')]
JParticipantUtils = interface(JObject)
['{245527FA-DD08-4626-BEDD-5D4976CFF950}']
end;
TJParticipantUtils = class(TJavaGenericImport<JParticipantUtilsClass, JParticipantUtils>) end;

JItemScope_BuilderClass = interface(JObjectClass)
['{D190EF3D-EEB6-4FDB-B3FC-67C90C5B9758}']
  {Methods}
  function init: JItemScope_Builder; cdecl;
end;

[JavaSignature('com/google/android/gms/plus/model/moments/ItemScope$Builder')]
JItemScope_Builder = interface(JObject)
['{56EBB360-26B6-419C-9094-9E4CCFD02E67}']
  {Methods}
  function build: JItemScope; cdecl;
  function setAbout(about: JItemScope): JItemScope_Builder; cdecl;
  function setAdditionalName(additionalName: JList): JItemScope_Builder; cdecl;
  function setAddress(address: JItemScope): JItemScope_Builder; cdecl;
  function setAddressCountry(addressCountry: JString): JItemScope_Builder; cdecl;
  function setAddressLocality(addressLocality: JString): JItemScope_Builder; cdecl;
  function setAddressRegion(addressRegion: JString): JItemScope_Builder; cdecl;
  function setAssociated_media(associated_media: JList): JItemScope_Builder; cdecl;
  function setAttendeeCount(attendeeCount: Integer): JItemScope_Builder; cdecl;
  function setAttendees(attendees: JList): JItemScope_Builder; cdecl;
  function setAudio(audio: JItemScope): JItemScope_Builder; cdecl;
  function setAuthor(author: JList): JItemScope_Builder; cdecl;
  function setBestRating(bestRating: JString): JItemScope_Builder; cdecl;
  function setBirthDate(birthDate: JString): JItemScope_Builder; cdecl;
  function setByArtist(byArtist: JItemScope): JItemScope_Builder; cdecl;
  function setCaption(caption: JString): JItemScope_Builder; cdecl;
  function setContentSize(contentSize: JString): JItemScope_Builder; cdecl;
  function setContentUrl(contentUrl: JString): JItemScope_Builder; cdecl;
  function setContributor(contributor: JList): JItemScope_Builder; cdecl;
  function setDateCreated(dateCreated: JString): JItemScope_Builder; cdecl;
  function setDateModified(dateModified: JString): JItemScope_Builder; cdecl;
  function setDatePublished(datePublished: JString): JItemScope_Builder; cdecl;
  function setDescription(description: JString): JItemScope_Builder; cdecl;
  function setDuration(duration: JString): JItemScope_Builder; cdecl;
  function setEmbedUrl(embedUrl: JString): JItemScope_Builder; cdecl;
  function setEndDate(endDate: JString): JItemScope_Builder; cdecl;
  function setFamilyName(familyName: JString): JItemScope_Builder; cdecl;
  function setGender(gender: JString): JItemScope_Builder; cdecl;
  function setGeo(geo: JItemScope): JItemScope_Builder; cdecl;
  function setGivenName(givenName: JString): JItemScope_Builder; cdecl;
  function setHeight(height: JString): JItemScope_Builder; cdecl;
  function setId(id: JString): JItemScope_Builder; cdecl;
  function setImage(image: JString): JItemScope_Builder; cdecl;
  function setInAlbum(inAlbum: JItemScope): JItemScope_Builder; cdecl;
  function setLatitude(latitude: Double): JItemScope_Builder; cdecl;
  function setLocation(location: JItemScope): JItemScope_Builder; cdecl;
  function setLongitude(longitude: Double): JItemScope_Builder; cdecl;
  function setName(name: JString): JItemScope_Builder; cdecl;
  function setPartOfTVSeries(partOfTVSeries: JItemScope): JItemScope_Builder; cdecl;
  function setPerformers(performers: JList): JItemScope_Builder; cdecl;
  function setPlayerType(playerType: JString): JItemScope_Builder; cdecl;
  function setPostOfficeBoxNumber(postOfficeBoxNumber: JString): JItemScope_Builder; cdecl;
  function setPostalCode(postalCode: JString): JItemScope_Builder; cdecl;
  function setRatingValue(ratingValue: JString): JItemScope_Builder; cdecl;
  function setReviewRating(reviewRating: JItemScope): JItemScope_Builder; cdecl;
  function setStartDate(startDate: JString): JItemScope_Builder; cdecl;
  function setStreetAddress(streetAddress: JString): JItemScope_Builder; cdecl;
  function setText(text: JString): JItemScope_Builder; cdecl;
  function setThumbnail(thumbnail: JItemScope): JItemScope_Builder; cdecl;
  function setThumbnailUrl(thumbnailUrl: JString): JItemScope_Builder; cdecl;
  function setTickerSymbol(tickerSymbol: JString): JItemScope_Builder; cdecl;
  function setType(type_: JString): JItemScope_Builder; cdecl;
  function setUrl(url: JString): JItemScope_Builder; cdecl;
  function setWidth(width: JString): JItemScope_Builder; cdecl;
  function setWorstRating(worstRating: JString): JItemScope_Builder; cdecl;
end;
TJItemScope_Builder = class(TJavaGenericImport<JItemScope_BuilderClass, JItemScope_Builder>) end;

JPerson_CoverClass = interface(JFreezableClass)
['{2A01AA50-3C30-46A4-A619-A5F534ECE9AF}']
end;

[JavaSignature('com/google/android/gms/plus/model/people/Person$Cover')]
JPerson_Cover = interface(JFreezable)
['{E13C8C39-D1B3-4E10-9548-2FEF8EF93128}']
  {Methods}
  function getCoverInfo: JCover_CoverInfo; cdecl;
  function getCoverPhoto: JCover_CoverPhoto; cdecl;
  function getLayout: Integer; cdecl;
  function hasCoverInfo: Boolean; cdecl;
  function hasCoverPhoto: Boolean; cdecl;
  function hasLayout: Boolean; cdecl;
end;
TJPerson_Cover = class(TJavaGenericImport<JPerson_CoverClass, JPerson_Cover>) end;

JOnSignOutCompleteListenerClass = interface(IJavaClass)
['{D1EB5BA9-422C-4227-9CF6-97A84CBB059B}']
end;

[JavaSignature('com/google/android/gms/appstate/OnSignOutCompleteListener')]
JOnSignOutCompleteListener = interface(IJavaInstance)
['{3D170166-50C8-4133-953D-38EA55020DA9}']
  {Methods}
  procedure onSignOutComplete; cdecl;
end;
TJOnSignOutCompleteListener = class(TJavaGenericImport<JOnSignOutCompleteListenerClass, JOnSignOutCompleteListener>) end;

JOnLeaderboardMetadataLoadedListenerClass = interface(IJavaClass)
['{304CF9F4-C10D-40D2-A74E-49AFFEE38380}']
end;

[JavaSignature('com/google/android/gms/games/leaderboard/OnLeaderboardMetadataLoadedListener')]
JOnLeaderboardMetadataLoadedListener = interface(IJavaInstance)
['{29C26A32-5AB7-43AC-AD52-0472DB0A9258}']
  {Methods}
  procedure onLeaderboardMetadataLoaded(paramInt: Integer; paramLeaderboardBuffer: JLeaderboardBuffer); cdecl;
end;
TJOnLeaderboardMetadataLoadedListener = class(TJavaGenericImport<JOnLeaderboardMetadataLoadedListenerClass, JOnLeaderboardMetadataLoadedListener>) end;

JLocationClient_OnRemoveGeofencesResultListenerClass = interface(IJavaClass)
['{592937ED-EA3B-43A9-9B13-BF89F0B46332}']
end;

[JavaSignature('com/google/android/gms/location/LocationClient$OnRemoveGeofencesResultListener')]
JLocationClient_OnRemoveGeofencesResultListener = interface(IJavaInstance)
['{B3640090-9A62-45D6-949E-6B8253D9785F}']
  {Methods}
  procedure onRemoveGeofencesByPendingIntentResult(paramInt: Integer; paramPendingIntent: JPendingIntent); cdecl;
  procedure onRemoveGeofencesByRequestIdsResult(paramInt: Integer; paramArrayOfString: TJavaObjectArray<JString>); cdecl;
end;
TJLocationClient_OnRemoveGeofencesResultListener = class(TJavaGenericImport<JLocationClient_OnRemoveGeofencesResultListenerClass, JLocationClient_OnRemoveGeofencesResultListener>) end;

JGeofence_BuilderClass = interface(JObjectClass)
['{1B065D39-AA46-4D6C-90DB-9E097BAE3C0A}']
  {Methods}
  function init: JGeofence_Builder; cdecl;
end;

[JavaSignature('com/google/android/gms/location/Geofence$Builder')]
JGeofence_Builder = interface(JObject)
['{64706B21-BE55-455E-8488-7A316C9184CE}']
  {Methods}
  function build: JGeofence; cdecl;
  function setCircularRegion(latitude: Double; longitude: Double; radius: Single): JGeofence_Builder; cdecl;
  function setExpirationDuration(durationMillis: Int64): JGeofence_Builder; cdecl;
  function setRequestId(requestId: JString): JGeofence_Builder; cdecl;
  function setTransitionTypes(transitionTypes: Integer): JGeofence_Builder; cdecl;
end;
TJGeofence_Builder = class(TJavaGenericImport<JGeofence_BuilderClass, JGeofence_Builder>) end;

JPanoramaClientClass = interface(JObjectClass)
['{688D7707-E775-4552-B421-F6037A124F89}']
  {Methods}
  function init(context: JContext; connectionCallbacks: JGooglePlayServicesClient_ConnectionCallbacks; connectionFailedListener: JGooglePlayServicesClient_OnConnectionFailedListener): JPanoramaClient; cdecl;
end;

[JavaSignature('com/google/android/gms/panorama/PanoramaClient')]
JPanoramaClient = interface(JObject)
['{142D7115-631D-45B3-A430-7602789E78F9}']
  {Methods}
  procedure connect; cdecl;
  procedure disconnect; cdecl;
  function isConnected: Boolean; cdecl;
  function isConnecting: Boolean; cdecl;
  function isConnectionCallbacksRegistered(listener: JGooglePlayServicesClient_ConnectionCallbacks): Boolean; cdecl;
  function isConnectionFailedListenerRegistered(listener: JGooglePlayServicesClient_OnConnectionFailedListener): Boolean; cdecl;
  procedure loadPanoramaInfo(listener: JPanoramaClient_OnPanoramaInfoLoadedListener; uri: Jnet_Uri); cdecl;
  procedure loadPanoramaInfoAndGrantAccess(listener: JPanoramaClient_OnPanoramaInfoLoadedListener; uri: Jnet_Uri); cdecl;
  procedure registerConnectionCallbacks(listener: JGooglePlayServicesClient_ConnectionCallbacks); cdecl;
  procedure registerConnectionFailedListener(listener: JGooglePlayServicesClient_OnConnectionFailedListener); cdecl;
  procedure unregisterConnectionCallbacks(listener: JGooglePlayServicesClient_ConnectionCallbacks); cdecl;
  procedure unregisterConnectionFailedListener(listener: JGooglePlayServicesClient_OnConnectionFailedListener); cdecl;
end;
TJPanoramaClient = class(TJavaGenericImport<JPanoramaClientClass, JPanoramaClient>) end;

JTileProviderClass = interface(IJavaClass)
['{D6128829-67DB-49E4-AB8A-D4ED01E19A11}']
  {Property Methods}
  function _GetNO_TILE: JTile;
  {Properties}
  property NO_TILE: JTile read _GetNO_TILE;
end;

[JavaSignature('com/google/android/gms/maps/model/TileProvider')]
JTileProvider = interface(IJavaInstance)
['{C1D85B3E-623F-4F8C-8CAA-8BFBFC944C61}']
  {Methods}
  function getTile(paramInt1: Integer; paramInt2: Integer; paramInt3: Integer): JTile; cdecl;
end;
TJTileProvider = class(TJavaGenericImport<JTileProviderClass, JTileProvider>) end;

JIGoogleMapDelegate_aClass = interface(JBinderClass)
['{2195B228-939C-4735-868F-7C9F249C1DE2}']
  {Methods}
  function init: JIGoogleMapDelegate_a; cdecl;
  function u(paramIBinder: JIBinder): JIGoogleMapDelegate; cdecl;
end;

[JavaSignature('com/google/android/gms/maps/internal/IGoogleMapDelegate$a')]
JIGoogleMapDelegate_a = interface(JBinder)
['{D530E481-0F85-4A04-850D-F25DE20B0586}']
  {Methods}
  function onTransact(code: Integer; data: JParcel; reply: JParcel; flags: Integer): Boolean; cdecl;
end;
TJIGoogleMapDelegate_a = class(TJavaGenericImport<JIGoogleMapDelegate_aClass, JIGoogleMapDelegate_a>) end;

JGooglePlayServicesClientClass = interface(IJavaClass)
['{9FF85676-EBAE-4D2E-913C-E8EF99D1A388}']
end;

[JavaSignature('com/google/android/gms/common/GooglePlayServicesClient')]
JGooglePlayServicesClient = interface(IJavaInstance)
['{70AFC0B8-8BC6-4BEA-88E4-CE8DB7F8F3CB}']
  {Methods}
  procedure connect; cdecl;
  procedure disconnect; cdecl;
  function isConnected: Boolean; cdecl;
  function isConnecting: Boolean; cdecl;
  function isConnectionCallbacksRegistered(paramConnectionCallbacks: JGooglePlayServicesClient_ConnectionCallbacks): Boolean; cdecl;
  function isConnectionFailedListenerRegistered(paramOnConnectionFailedListener: JGooglePlayServicesClient_OnConnectionFailedListener): Boolean; cdecl;
  procedure registerConnectionCallbacks(paramConnectionCallbacks: JGooglePlayServicesClient_ConnectionCallbacks); cdecl;
  procedure registerConnectionFailedListener(paramOnConnectionFailedListener: JGooglePlayServicesClient_OnConnectionFailedListener); cdecl;
  procedure unregisterConnectionCallbacks(paramConnectionCallbacks: JGooglePlayServicesClient_ConnectionCallbacks); cdecl;
  procedure unregisterConnectionFailedListener(paramOnConnectionFailedListener: JGooglePlayServicesClient_OnConnectionFailedListener); cdecl;
end;
TJGooglePlayServicesClient = class(TJavaGenericImport<JGooglePlayServicesClientClass, JGooglePlayServicesClient>) end;

JPlusShare_BuilderClass = interface(JObjectClass)
['{452E7438-523E-49EE-8D62-DC1F947CB0F0}']
  {Methods}
  function init(context: JContext): JPlusShare_Builder; cdecl; overload;
  function init(launchingActivity: JActivity): JPlusShare_Builder; cdecl; overload;
  function init(launchingActivity: JActivity; plusClient: JPlusClient): JPlusShare_Builder; cdecl; overload;
end;

[JavaSignature('com/google/android/gms/plus/PlusShare$Builder')]
JPlusShare_Builder = interface(JObject)
['{84FC9DA5-38FA-4A1F-AB92-0E3DCF29AD94}']
  {Methods}
  function addCallToAction(label_: JString; uri: Jnet_Uri; deepLinkId: JString): JPlusShare_Builder; cdecl;
  function addStream(streamUri: Jnet_Uri): JPlusShare_Builder; cdecl;
  function getIntent: JIntent; cdecl;
  function setContentDeepLinkId(deepLinkId: JString): JPlusShare_Builder; cdecl; overload;
  function setContentDeepLinkId(deepLinkId: JString; title: JString; description: JString; thumbnailUri: Jnet_Uri): JPlusShare_Builder; cdecl; overload;
  function setContentUrl(uri: Jnet_Uri): JPlusShare_Builder; cdecl;
  function setRecipients(recipientList: JList): JPlusShare_Builder; cdecl;
  function setStream(streamUri: Jnet_Uri): JPlusShare_Builder; cdecl;
  function setText(text: JCharSequence): JPlusShare_Builder; cdecl;
  function setType(mimeType: JString): JPlusShare_Builder; cdecl;
end;
TJPlusShare_Builder = class(TJavaGenericImport<JPlusShare_BuilderClass, JPlusShare_Builder>) end;

JMomentClass = interface(JFreezableClass)
['{4F99F47F-21F8-4492-98A3-1EAC01C6E87A}']
end;

[JavaSignature('com/google/android/gms/plus/model/moments/Moment')]
JMoment = interface(JFreezable)
['{313E023A-E21E-4C51-A33B-98FD3FE5BD5C}']
  {Methods}
  function getId: JString; cdecl;
  function getResult: JItemScope; cdecl;
  function getStartDate: JString; cdecl;
  function getTarget: JItemScope; cdecl;
  function getType: JString; cdecl;
  function hasId: Boolean; cdecl;
  function hasResult: Boolean; cdecl;
  function hasStartDate: Boolean; cdecl;
  function hasTarget: Boolean; cdecl;
  function hasType: Boolean; cdecl;
end;
TJMoment = class(TJavaGenericImport<JMomentClass, JMoment>) end;

JPersonBufferClass = interface(JDataBufferClass)
['{4AEB7532-3916-4C78-BD28-EDEF098AD60E}']
  {Methods}
  function init(dataHolder: JString): JPersonBuffer; cdecl;
end;

[JavaSignature('com/google/android/gms/plus/model/people/PersonBuffer')]
JPersonBuffer = interface(JDataBuffer)
['{E90B2976-B2D6-4A8E-979A-6A26C86DAC66}']
  {Methods}
  function &get(position: Integer): JPerson; cdecl;
end;
TJPersonBuffer = class(TJavaGenericImport<JPersonBufferClass, JPersonBuffer>) end;

JMarkerClass = interface(JObjectClass)
['{4A8555E9-4F3A-4C8A-A75E-17FB0015ABBB}']
  {Methods}
  function init(delegate: JObject): JMarker; cdecl;
end;

[JavaSignature('com/google/android/gms/maps/model/Marker')]
JMarker = interface(JObject)
['{16BF016C-85CC-4E27-A86A-A066BDDCA0B7}']
  {Methods}
  function equals(other: JObject): Boolean; cdecl;
  function getId: JString; cdecl;
  function getPosition: JLatLng; cdecl;
  function getSnippet: JString; cdecl;
  function getTitle: JString; cdecl;
  function hashCode: Integer; cdecl;
  procedure hideInfoWindow; cdecl;
  function isDraggable: Boolean; cdecl;
  function isInfoWindowShown: Boolean; cdecl;
  function isVisible: Boolean; cdecl;
  procedure remove; cdecl;
  procedure setAnchor(anchorU: Single; anchorV: Single); cdecl;
  procedure setDraggable(draggable: Boolean); cdecl;
  procedure setIcon(icon: JBitmapDescriptor); cdecl;
  procedure setPosition(latlng: JLatLng); cdecl;
  procedure setSnippet(snippet: JString); cdecl;
  procedure setTitle(title: JString); cdecl;
  procedure setVisible(visible: Boolean); cdecl;
  procedure showInfoWindow; cdecl;
end;
TJMarker = class(TJavaGenericImport<JMarkerClass, JMarker>) end;

JMapFragmentClass = interface(JFragmentClass)
['{F06BD952-32D9-492A-A0D3-40B2F488D619}']
  {Methods}
  function init: JMapFragment; cdecl;
  function newInstance: JMapFragment; cdecl; overload;
  function newInstance(options: JGoogleMapOptions): JMapFragment; cdecl; overload;
end;

[JavaSignature('com/google/android/gms/maps/MapFragment')]
JMapFragment = interface(JFragment)
['{DA91ED13-88F8-4FE5-B934-14853D60E7FD}']
  {Methods}
  function getMap: JGoogleMap; cdecl;
  procedure onActivityCreated(savedInstanceState: JBundle); cdecl;
  procedure onAttach(activity: JActivity); cdecl;
  procedure onCreate(savedInstanceState: JBundle); cdecl;
  function onCreateView(inflater: JLayoutInflater; container: JViewGroup; savedInstanceState: JBundle): JView; cdecl;
  procedure onDestroy; cdecl;
  procedure onDestroyView; cdecl;
  procedure onInflate(activity: JActivity; attrs: JAttributeSet; savedInstanceState: JBundle); cdecl;
  procedure onLowMemory; cdecl;
  procedure onPause; cdecl;
  procedure onResume; cdecl;
  procedure onSaveInstanceState(outState: JBundle); cdecl;
  procedure setArguments(args: JBundle); cdecl;
end;
TJMapFragment = class(TJavaGenericImport<JMapFragmentClass, JMapFragment>) end;

JBitmapDescriptorClass = interface(JObjectClass)
['{B3AC0084-AD2D-4B7C-A2E6-46E1F05DB7F6}']
  {Methods}
  function init(remoteObject: JObject): JBitmapDescriptor; cdecl;
end;

[JavaSignature('com/google/android/gms/maps/model/BitmapDescriptor')]
JBitmapDescriptor = interface(JObject)
['{650F87E1-F0BF-4270-8ADC-11941A7015AB}']
  {Methods}
  function aU: JObject; cdecl;
end;
TJBitmapDescriptor = class(TJavaGenericImport<JBitmapDescriptorClass, JBitmapDescriptor>) end;

JMarkerOptionsCreatorClass = interface(JObjectClass)
['{EF695BEB-416A-44B7-8CDB-597D8729AC3B}']
  {Property Methods}
  function _GetCONTENT_DESCRIPTION: Integer;
  {Methods}
  function init: JMarkerOptionsCreator; cdecl;
  {Properties}
  property CONTENT_DESCRIPTION: Integer read _GetCONTENT_DESCRIPTION;
end;

[JavaSignature('com/google/android/gms/maps/model/MarkerOptionsCreator')]
JMarkerOptionsCreator = interface(JObject)
['{15AE8D0F-3846-479E-AD98-4CF627C8C80C}']
  {Methods}
  function createFromParcel(parcel: JParcel): JMarkerOptions; cdecl;
  function newArray(size: Integer): TJavaObjectArray<JMarkerOptions>; cdecl;
end;
TJMarkerOptionsCreator = class(TJavaGenericImport<JMarkerOptionsCreatorClass, JMarkerOptionsCreator>) end;

JOnGamesLoadedListenerClass = interface(IJavaClass)
['{C6A85AD6-308E-41F8-AF51-B5F65BBA872E}']
end;

[JavaSignature('com/google/android/gms/games/OnGamesLoadedListener')]
JOnGamesLoadedListener = interface(IJavaInstance)
['{A7072B79-5C7C-4390-8DC2-BBEF7C12E08E}']
  {Methods}
  procedure onGamesLoaded(paramInt: Integer; paramGameBuffer: JGameBuffer); cdecl;
end;
TJOnGamesLoadedListener = class(TJavaGenericImport<JOnGamesLoadedListenerClass, JOnGamesLoadedListener>) end;

JAppStateClass = interface(JFreezableClass)
['{320912A0-C62F-4708-9F27-96135CE528C3}']
end;

[JavaSignature('com/google/android/gms/appstate/AppState')]
JAppState = interface(JFreezable)
['{62158A4D-D6E4-4068-84F6-13B5EFAF242C}']
  {Methods}
  function getConflictData: TJavaArray<Byte>; cdecl;
  function getConflictVersion: JString; cdecl;
  function getKey: Integer; cdecl;
  function getLocalData: TJavaArray<Byte>; cdecl;
  function getLocalVersion: JString; cdecl;
  function hasConflict: Boolean; cdecl;
end;
TJAppState = class(TJavaGenericImport<JAppStateClass, JAppState>) end;

JPolygonClass = interface(JObjectClass)
['{C32F88C3-3863-4316-AB9E-99F6501F709C}']
  {Methods}
  function init(delegate: JObject): JPolygon; cdecl;
end;

[JavaSignature('com/google/android/gms/maps/model/Polygon')]
JPolygon = interface(JObject)
['{BECE8208-AA23-48F1-AC07-849EBCABB32E}']
  {Methods}
  function equals(other: JObject): Boolean; cdecl;
  function getFillColor: Integer; cdecl;
  function getHoles: JList; cdecl;
  function getId: JString; cdecl;
  function getPoints: JList; cdecl;
  function getStrokeColor: Integer; cdecl;
  function getStrokeWidth: Single; cdecl;
  function getZIndex: Single; cdecl;
  function hashCode: Integer; cdecl;
  function isGeodesic: Boolean; cdecl;
  function isVisible: Boolean; cdecl;
  procedure remove; cdecl;
  procedure setFillColor(color: Integer); cdecl;
  procedure setGeodesic(geodesic: Boolean); cdecl;
  procedure setHoles(holes: JList); cdecl;
  procedure setPoints(points: JList); cdecl;
  procedure setStrokeColor(color: Integer); cdecl;
  procedure setStrokeWidth(width: Single); cdecl;
  procedure setVisible(visible: Boolean); cdecl;
  procedure setZIndex(zIndex: Single); cdecl;
end;
TJPolygon = class(TJavaGenericImport<JPolygonClass, JPolygon>) end;

JTileClass = interface(JObjectClass)
['{83027546-0EC6-4279-8EE9-9C7D5916EF94}']
  {Property Methods}
  function _GetCREATOR: JTileCreator;
  {Methods}
  function init(width: Integer; height: Integer; data: TJavaArray<Byte>): JTile; cdecl;
  {Properties}
  property CREATOR: JTileCreator read _GetCREATOR;
end;

[JavaSignature('com/google/android/gms/maps/model/Tile')]
JTile = interface(JObject)
['{B5DD3D4F-FD9D-431B-BD30-919E328BE40C}']
  {Property Methods}
  function _Getdata: TJavaArray<Byte>;
  function _Getheight: Integer;
  function _Getwidth: Integer;
  {Methods}
  function F: Integer; cdecl;
  function describeContents: Integer; cdecl;
  procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
  {Properties}
  property data: TJavaArray<Byte> read _Getdata;
  property height: Integer read _Getheight;
  property width: Integer read _Getwidth;
end;
TJTile = class(TJavaGenericImport<JTileClass, JTile>) end;

JDetectedActivityClass = interface(JObjectClass)
['{4BF1C57E-0AC8-498A-9EE6-506F8D632C07}']
  {Property Methods}
  function _GetCREATOR: JDetectedActivityCreator;
  function _GetIN_VEHICLE: Integer;
  function _GetON_BICYCLE: Integer;
  function _GetON_FOOT: Integer;
  function _GetSTILL: Integer;
  function _GetTILTING: Integer;
  function _GetUNKNOWN: Integer;
  {Methods}
  function init(activityType: Integer; confidence: Integer): JDetectedActivity; cdecl; overload;
  function init: JDetectedActivity; cdecl; overload;
  {Properties}
  property CREATOR: JDetectedActivityCreator read _GetCREATOR;
  property IN_VEHICLE: Integer read _GetIN_VEHICLE;
  property ON_BICYCLE: Integer read _GetON_BICYCLE;
  property ON_FOOT: Integer read _GetON_FOOT;
  property STILL: Integer read _GetSTILL;
  property TILTING: Integer read _GetTILTING;
  property UNKNOWN: Integer read _GetUNKNOWN;
end;

[JavaSignature('com/google/android/gms/location/DetectedActivity')]
JDetectedActivity = interface(JObject)
['{8A5265C3-15DA-4CEC-87AF-F46270E042F6}']
  {Methods}
  function describeContents: Integer; cdecl;
  function getConfidence: Integer; cdecl;
  function getType: Integer; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
end;
TJDetectedActivity = class(TJavaGenericImport<JDetectedActivityClass, JDetectedActivity>) end;

JRoomStatusUpdateListenerClass = interface(IJavaClass)
['{EAA3385A-F22C-44E6-B87C-A07AF86EFDF0}']
end;

[JavaSignature('com/google/android/gms/games/multiplayer/realtime/RoomStatusUpdateListener')]
JRoomStatusUpdateListener = interface(IJavaInstance)
['{4EEDB710-37CA-4B3B-937C-3F109DC26D4C}']
  {Methods}
  procedure onConnectedToRoom(paramRoom: JRoom); cdecl;
  procedure onDisconnectedFromRoom(paramRoom: JRoom); cdecl;
  procedure onP2PConnected(paramString: JString); cdecl;
  procedure onP2PDisconnected(paramString: JString); cdecl;
  procedure onPeerDeclined(paramRoom: JRoom; paramList: JList); cdecl;
  procedure onPeerInvitedToRoom(paramRoom: JRoom; paramList: JList); cdecl;
  procedure onPeerJoined(paramRoom: JRoom; paramList: JList); cdecl;
  procedure onPeerLeft(paramRoom: JRoom; paramList: JList); cdecl;
  procedure onPeersConnected(paramRoom: JRoom; paramList: JList); cdecl;
  procedure onPeersDisconnected(paramRoom: JRoom; paramList: JList); cdecl;
  procedure onRoomAutoMatching(paramRoom: JRoom); cdecl;
  procedure onRoomConnecting(paramRoom: JRoom); cdecl;
end;
TJRoomStatusUpdateListener = class(TJavaGenericImport<JRoomStatusUpdateListenerClass, JRoomStatusUpdateListener>) end;

JGoogleMap_CancelableCallbackClass = interface(IJavaClass)
['{9614115C-26C0-42A4-BC6B-8C5C07FFE24B}']
end;

[JavaSignature('com/google/android/gms/maps/GoogleMap$CancelableCallback')]
JGoogleMap_CancelableCallback = interface(IJavaInstance)
['{D1AC9707-AC15-4906-A687-B3A628DECC9D}']
  {Methods}
  procedure onCancel; cdecl;
  procedure onFinish; cdecl;
end;
TJGoogleMap_CancelableCallback = class(TJavaGenericImport<JGoogleMap_CancelableCallbackClass, JGoogleMap_CancelableCallback>) end;

JPlusClient_OnMomentsLoadedListenerClass = interface(IJavaClass)
['{3BAE124C-4C24-4365-B6CB-EC63C78CCD71}']
end;

[JavaSignature('com/google/android/gms/plus/PlusClient$OnMomentsLoadedListener')]
JPlusClient_OnMomentsLoadedListener = interface(IJavaInstance)
['{05069DAE-57BB-4F77-BAC6-3C57DFD827CF}']
  {Methods}
  procedure onMomentsLoaded(paramConnectionResult: JConnectionResult; paramMomentBuffer: JMomentBuffer; paramString1: JString; paramString2: JString); cdecl;
end;
TJPlusClient_OnMomentsLoadedListener = class(TJavaGenericImport<JPlusClient_OnMomentsLoadedListenerClass, JPlusClient_OnMomentsLoadedListener>) end;

JPlusClientClass = interface(JObjectClass)
['{2AA2BFD3-460D-4079-AD6A-957E42CC2FF4}']
  {Property Methods}
  function _GetKEY_REQUEST_VISIBLE_ACTIVITIES: JString;
  {Properties}
  property KEY_REQUEST_VISIBLE_ACTIVITIES: JString read _GetKEY_REQUEST_VISIBLE_ACTIVITIES;
end;

[JavaSignature('com/google/android/gms/plus/PlusClient')]
JPlusClient = interface(JObject)
['{EAB6DF55-278D-473C-BA80-D1B2C1C0F2BC}']
  {Methods}
  procedure a(paramb: JPlusClient_b; paramString: JString); cdecl; overload;
  procedure a(parama: JPlusClient_a; paramUri: Jnet_Uri; paramInt: Integer); cdecl; overload;
  procedure clearDefaultAccount; cdecl;
  procedure connect; cdecl;
  procedure disconnect; cdecl;
  function getAccountName: JString; cdecl;
  function getCurrentPerson: JPerson; cdecl;
  function isConnected: Boolean; cdecl;
  function isConnecting: Boolean; cdecl;
  function isConnectionCallbacksRegistered(listener: JGooglePlayServicesClient_ConnectionCallbacks): Boolean; cdecl;
  function isConnectionFailedListenerRegistered(listener: JGooglePlayServicesClient_OnConnectionFailedListener): Boolean; cdecl;
  procedure loadMoments(listener: JPlusClient_OnMomentsLoadedListener); cdecl; overload;
  procedure loadMoments(listener: JPlusClient_OnMomentsLoadedListener; maxResults: Integer; pageToken: JString; targetUrl: Jnet_Uri; type_: JString; userId: JString); cdecl; overload;
  procedure loadPeople(listener: JPlusClient_OnPeopleLoadedListener; collection: Integer); cdecl; overload;
  procedure loadPeople(listener: JPlusClient_OnPeopleLoadedListener; collection: Integer; orderBy: Integer; maxResults: Integer; pageToken: JString); cdecl; overload;
  procedure loadPeople(listener: JPlusClient_OnPeopleLoadedListener; personIds: JCollection); cdecl; overload;
  procedure loadPeople(listener: JPlusClient_OnPeopleLoadedListener; personIds: TJavaObjectArray<JString>); cdecl; overload;
  procedure loadPerson(listener: JPlusClient_OnPersonLoadedListener; userId: JString); cdecl;
  procedure loadVisiblePeople(listener: JPlusClient_OnPeopleLoadedListener; orderBy: Integer; pageToken: JString); cdecl; overload;
  procedure loadVisiblePeople(listener: JPlusClient_OnPeopleLoadedListener; pageToken: JString); cdecl; overload;
  procedure registerConnectionCallbacks(listener: JGooglePlayServicesClient_ConnectionCallbacks); cdecl;
  procedure registerConnectionFailedListener(listener: JGooglePlayServicesClient_OnConnectionFailedListener); cdecl;
  procedure removeMoment(momentId: JString); cdecl;
  procedure revokeAccessAndDisconnect(listener: JPlusClient_OnAccessRevokedListener); cdecl;
  procedure unregisterConnectionCallbacks(listener: JGooglePlayServicesClient_ConnectionCallbacks); cdecl;
  procedure unregisterConnectionFailedListener(listener: JGooglePlayServicesClient_OnConnectionFailedListener); cdecl;
  procedure writeMoment(moment: JMoment); cdecl;
end;
TJPlusClient = class(TJavaGenericImport<JPlusClientClass, JPlusClient>) end;

JLatLngBoundsClass = interface(JObjectClass)
['{E18E597F-AE80-4248-A4D1-CE1F3202668C}']
  {Property Methods}
  function _GetCREATOR: JLatLngBoundsCreator;
  {Methods}
  function init(southwest: JLatLng; northeast: JLatLng): JLatLngBounds; cdecl;
  function builder: JLatLngBounds_Builder; cdecl;
  {Properties}
  property CREATOR: JLatLngBoundsCreator read _GetCREATOR;
end;

[JavaSignature('com/google/android/gms/maps/model/LatLngBounds')]
JLatLngBounds = interface(JObject)
['{65EF16BB-A68A-43E0-BC7D-0FBFE1321A73}']
  {Property Methods}
  function _Getnortheast: JLatLng;
  function _Getsouthwest: JLatLng;
  {Methods}
  function F: Integer; cdecl;
  function &contains(point: JLatLng): Boolean; cdecl;
  function describeContents: Integer; cdecl;
  function equals(o: JObject): Boolean; cdecl;
  function hashCode: Integer; cdecl;
  function including(point: JLatLng): JLatLngBounds; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
  {Properties}
  property northeast: JLatLng read _Getnortheast;
  property southwest: JLatLng read _Getsouthwest;
end;
TJLatLngBounds = class(TJavaGenericImport<JLatLngBoundsClass, JLatLngBounds>) end;

JICameraUpdateFactoryDelegateClass = interface(JIInterfaceClass)
['{07D64056-52B1-4C96-8CFB-37F9B8858E1F}']
end;

[JavaSignature('com/google/android/gms/maps/internal/ICameraUpdateFactoryDelegate')]
JICameraUpdateFactoryDelegate = interface(JIInterface)
['{8304CE07-A2B8-4DE3-AEDA-741DA2821D85}']
  {Methods}
  function newCameraPosition(paramCameraPosition: JCameraPosition): JObject; cdecl;
  function newLatLng(paramLatLng: JLatLng): JObject; cdecl;
  function newLatLngBounds(paramLatLngBounds: JLatLngBounds; paramInt: Integer): JObject; cdecl;
  function newLatLngBoundsWithSize(paramLatLngBounds: JLatLngBounds; paramInt1: Integer; paramInt2: Integer; paramInt3: Integer): JObject; cdecl;
  function newLatLngZoom(paramLatLng: JLatLng; paramFloat: Single): JObject; cdecl;
  function scrollBy(paramFloat1: Single; paramFloat2: Single): JObject; cdecl;
  function zoomBy(paramFloat: Single): JObject; cdecl;
  function zoomByWithFocus(paramFloat: Single; paramInt1: Integer; paramInt2: Integer): JObject; cdecl;
  function zoomIn: JObject; cdecl;
  function zoomOut: JObject; cdecl;
  function zoomTo(paramFloat: Single): JObject; cdecl;
end;
TJICameraUpdateFactoryDelegate = class(TJavaGenericImport<JICameraUpdateFactoryDelegateClass, JICameraUpdateFactoryDelegate>) end;

Jlocation_LocationListenerClass = interface(IJavaClass)
['{FCF58DA8-B892-497A-A676-40D6D32A4DE9}']
end;

[JavaSignature('com/google/android/gms/location/LocationListener')]
Jlocation_LocationListener = interface(IJavaInstance)
['{01DB4B21-C92D-4BBC-8D21-D8270F4C0370}']
  {Methods}
  procedure onLocationChanged(paramLocation: JLocation); cdecl;
end;
TJlocation_LocationListener = class(TJavaGenericImport<Jlocation_LocationListenerClass, Jlocation_LocationListener>) end;

JIMapFragmentDelegateClass = interface(JIInterfaceClass)
['{2C6919E8-96C5-400E-A665-6CDC244D5A89}']
end;

[JavaSignature('com/google/android/gms/maps/internal/IMapFragmentDelegate')]
JIMapFragmentDelegate = interface(JIInterface)
['{1B884F8B-0474-4C3C-A3F1-D28FC74C8F1F}']
  {Methods}
  function getMap: JIGoogleMapDelegate; cdecl;
  function isReady: Boolean; cdecl;
  procedure onCreate(paramBundle: JBundle); cdecl;
  function onCreateView(parambi1: JObject; parambi2: JObject; paramBundle: JBundle): JObject; cdecl;
  procedure onDestroy; cdecl;
  procedure onDestroyView; cdecl;
  procedure onInflate(parambi: JObject; paramGoogleMapOptions: JGoogleMapOptions; paramBundle: JBundle); cdecl;
  procedure onLowMemory; cdecl;
  procedure onPause; cdecl;
  procedure onResume; cdecl;
  procedure onSaveInstanceState(paramBundle: JBundle); cdecl;
end;
TJIMapFragmentDelegate = class(TJavaGenericImport<JIMapFragmentDelegateClass, JIMapFragmentDelegate>) end;

JUserRecoverableNotifiedExceptionClass = interface(JGoogleAuthExceptionClass)
['{039B6304-A9DB-4C6D-AA9A-DC21D7FA51D7}']
  {Methods}
  function init(err: JString): JUserRecoverableNotifiedException; cdecl;
end;

[JavaSignature('com/google/android/gms/auth/UserRecoverableNotifiedException')]
JUserRecoverableNotifiedException = interface(JGoogleAuthException)
['{65AC8DD0-EEDF-41FE-86E0-459D38C0A468}']
end;
TJUserRecoverableNotifiedException = class(TJavaGenericImport<JUserRecoverableNotifiedExceptionClass, JUserRecoverableNotifiedException>) end;

JParticipatableClass = interface(IJavaClass)
['{E9864B4D-9A42-4F31-9DDF-328F7281408D}']
end;

[JavaSignature('com/google/android/gms/games/multiplayer/Participatable')]
JParticipatable = interface(IJavaInstance)
['{0DAC4927-D1B7-40F3-83DF-6C472854EF61}']
  {Methods}
  function getParticipants: JArrayList; cdecl;
end;
TJParticipatable = class(TJavaGenericImport<JParticipatableClass, JParticipatable>) end;

JGoogleMap_OnMarkerClickListenerClass = interface(IJavaClass)
['{0726010E-E833-4431-B205-F5D1978508F4}']
end;

[JavaSignature('com/google/android/gms/maps/GoogleMap$OnMarkerClickListener')]
JGoogleMap_OnMarkerClickListener = interface(IJavaInstance)
['{C3FD23CC-A959-4720-A91D-22074E303764}']
  {Methods}
  function onMarkerClick(paramMarker: JMarker): Boolean; cdecl;
end;
TJGoogleMap_OnMarkerClickListener = class(TJavaGenericImport<JGoogleMap_OnMarkerClickListenerClass, JGoogleMap_OnMarkerClickListener>) end;

JGroundOverlayOptionsCreatorClass = interface(JObjectClass)
['{C3F20BFE-B3FC-4137-9159-3574BC5BDEA6}']
  {Property Methods}
  function _GetCONTENT_DESCRIPTION: Integer;
  {Methods}
  function init: JGroundOverlayOptionsCreator; cdecl;
  {Properties}
  property CONTENT_DESCRIPTION: Integer read _GetCONTENT_DESCRIPTION;
end;

[JavaSignature('com/google/android/gms/maps/model/GroundOverlayOptionsCreator')]
JGroundOverlayOptionsCreator = interface(JObject)
['{07A591A3-7878-4DB7-BD5D-12BE6AFBEA9B}']
  {Methods}
  function createFromParcel(parcel: JParcel): JGroundOverlayOptions; cdecl;
  function newArray(size: Integer): TJavaObjectArray<JGroundOverlayOptions>; cdecl;
end;
TJGroundOverlayOptionsCreator = class(TJavaGenericImport<JGroundOverlayOptionsCreatorClass, JGroundOverlayOptionsCreator>) end;

JIPolylineDelegateClass = interface(JIInterfaceClass)
['{B7D05429-CC28-46F2-9BFD-3889D67A17D8}']
end;

[JavaSignature('com/google/android/gms/maps/model/internal/IPolylineDelegate')]
JIPolylineDelegate = interface(JIInterface)
['{E6343847-B189-40EB-8B0B-D37417218452}']
  {Methods}
  function equalsRemote(paramIPolylineDelegate: JIPolylineDelegate): Boolean; cdecl;
  function getColor: Integer; cdecl;
  function getId: JString; cdecl;
  function getPoints: JList; cdecl;
  function getWidth: Single; cdecl;
  function getZIndex: Single; cdecl;
  function hashCodeRemote: Integer; cdecl;
  function isGeodesic: Boolean; cdecl;
  function isVisible: Boolean; cdecl;
  procedure remove; cdecl;
  procedure setColor(paramInt: Integer); cdecl;
  procedure setGeodesic(paramBoolean: Boolean); cdecl;
  procedure setPoints(paramList: JList); cdecl;
  procedure setVisible(paramBoolean: Boolean); cdecl;
  procedure setWidth(paramFloat: Single); cdecl;
  procedure setZIndex(paramFloat: Single); cdecl;
end;
TJIPolylineDelegate = class(TJavaGenericImport<JIPolylineDelegateClass, JIPolylineDelegate>) end;

JPlusClient_OrderByClass = interface(IJavaClass)
['{318FADB7-DFAB-4ABF-8CD2-01240A277164}']
  {Property Methods}
  function _GetALPHABETICAL: Integer;
  function _GetBEST: Integer;
  {Properties}
  property ALPHABETICAL: Integer read _GetALPHABETICAL;
  property BEST: Integer read _GetBEST;
end;

[JavaSignature('com/google/android/gms/plus/PlusClient$OrderBy')]
JPlusClient_OrderBy = interface(IJavaInstance)
['{3335E9DA-CE30-42BB-B782-4AB6DBA68373}']
end;
TJPlusClient_OrderBy = class(TJavaGenericImport<JPlusClient_OrderByClass, JPlusClient_OrderBy>) end;

JOnAchievementUpdatedListenerClass = interface(IJavaClass)
['{D5A81729-A7D4-4FEF-ABC3-31E231409077}']
end;

[JavaSignature('com/google/android/gms/games/achievement/OnAchievementUpdatedListener')]
JOnAchievementUpdatedListener = interface(IJavaInstance)
['{2A05EB85-A7D1-49BB-A48A-58C5A39A459D}']
  {Methods}
  procedure onAchievementUpdated(paramInt: Integer; paramString: JString); cdecl;
end;
TJOnAchievementUpdatedListener = class(TJavaGenericImport<JOnAchievementUpdatedListenerClass, JOnAchievementUpdatedListener>) end;

JOnInvitationsLoadedListenerClass = interface(IJavaClass)
['{DAC00C24-F140-4A2B-A4EB-1F3052F63D7D}']
end;

[JavaSignature('com/google/android/gms/games/multiplayer/OnInvitationsLoadedListener')]
JOnInvitationsLoadedListener = interface(IJavaInstance)
['{00A2F816-AE8D-4D78-891A-252564309122}']
  {Methods}
  procedure onInvitationsLoaded(paramInt: Integer; paramInvitationBuffer: JInvitationBuffer); cdecl;
end;
TJOnInvitationsLoadedListener = class(TJavaGenericImport<JOnInvitationsLoadedListenerClass, JOnInvitationsLoadedListener>) end;

JIMapViewDelegateClass = interface(JIInterfaceClass)
['{24E152A6-AAD0-41CE-873A-CB4042EAB3DA}']
end;

[JavaSignature('com/google/android/gms/maps/internal/IMapViewDelegate')]
JIMapViewDelegate = interface(JIInterface)
['{92FC30C9-99E2-4EC7-A051-53499F82D965}']
  {Methods}
  function getMap: JIGoogleMapDelegate; cdecl;
  function getView: JObject; cdecl;
  procedure onCreate(paramBundle: JBundle); cdecl;
  procedure onDestroy; cdecl;
  procedure onLowMemory; cdecl;
  procedure onPause; cdecl;
  procedure onResume; cdecl;
  procedure onSaveInstanceState(paramBundle: JBundle); cdecl;
end;
TJIMapViewDelegate = class(TJavaGenericImport<JIMapViewDelegateClass, JIMapViewDelegate>) end;

JGoogleAuthUtilClass = interface(JObjectClass)
['{3EB073E2-78BC-422D-85BE-E79CE3D7F1A1}']
  {Property Methods}
  function _GetGOOGLE_ACCOUNT_TYPE: JString;
  function _GetKEY_ANDROID_PACKAGE_NAME: JString;
  function _GetKEY_CALLER_UID: JString;
  function _GetKEY_REQUEST_VISIBLE_ACTIVITIES: JString;
  function _GetKEY_SUPPRESS_PROGRESS_SCREEN: JString;
  {Methods}
  function init: JGoogleAuthUtil; cdecl;
  function getToken(context: JContext; accountName: JString; scope: JString): JString; cdecl; overload;
  function getToken(context: JContext; accountName: JString; scope: JString; extras: JBundle): JString; cdecl; overload;
  function getTokenWithNotification(context: JContext; accountName: JString; scope: JString; extras: JBundle): JString; cdecl; overload;
  function getTokenWithNotification(context: JContext; accountName: JString; scope: JString; extras: JBundle; callback: JIntent): JString; cdecl; overload;
  function getTokenWithNotification(context: JContext; accountName: JString; scope: JString; extras: JBundle; authority: JString; syncBundle: JBundle): JString; cdecl; overload;
  procedure invalidateToken(context: JContext; token: JString); cdecl;
  {Properties}
  property GOOGLE_ACCOUNT_TYPE: JString read _GetGOOGLE_ACCOUNT_TYPE;
  property KEY_ANDROID_PACKAGE_NAME: JString read _GetKEY_ANDROID_PACKAGE_NAME;
  property KEY_CALLER_UID: JString read _GetKEY_CALLER_UID;
  property KEY_REQUEST_VISIBLE_ACTIVITIES: JString read _GetKEY_REQUEST_VISIBLE_ACTIVITIES;
  property KEY_SUPPRESS_PROGRESS_SCREEN: JString read _GetKEY_SUPPRESS_PROGRESS_SCREEN;
end;

[JavaSignature('com/google/android/gms/auth/GoogleAuthUtil')]
JGoogleAuthUtil = interface(JObject)
['{B40125B3-21B6-4B54-893E-DAE1257A7986}']
end;
TJGoogleAuthUtil = class(TJavaGenericImport<JGoogleAuthUtilClass, JGoogleAuthUtil>) end;

JLatLngBoundsCreatorClass = interface(JObjectClass)
['{EEEFE6B4-19BB-4232-9E02-76E5ACE5D368}']
  {Property Methods}
  function _GetCONTENT_DESCRIPTION: Integer;
  {Methods}
  function init: JLatLngBoundsCreator; cdecl;
  {Properties}
  property CONTENT_DESCRIPTION: Integer read _GetCONTENT_DESCRIPTION;
end;

[JavaSignature('com/google/android/gms/maps/model/LatLngBoundsCreator')]
JLatLngBoundsCreator = interface(JObject)
['{F0D25814-494C-4A0E-85D8-5D3CD81E9808}']
  {Methods}
  function createFromParcel(parcel: JParcel): JLatLngBounds; cdecl;
  function newArray(size: Integer): TJavaObjectArray<JLatLngBounds>; cdecl;
end;
TJLatLngBoundsCreator = class(TJavaGenericImport<JLatLngBoundsCreatorClass, JLatLngBoundsCreator>) end;

JSupportMapFragmentClass = interface(JObjectClass)
['{4DCB1F0B-DEE3-47DE-BFB4-5A343A3F2A7C}']
  {Methods}
  function init: JSupportMapFragment; cdecl;
  function newInstance: JSupportMapFragment; cdecl; overload;
  function newInstance(options: JGoogleMapOptions): JSupportMapFragment; cdecl; overload;
end;

[JavaSignature('com/google/android/gms/maps/SupportMapFragment')]
JSupportMapFragment = interface(JObject)
['{4E4C9D9E-DDAA-4A2C-9C97-AA1C20C5DA3B}']
  {Methods}
  function getMap: JGoogleMap; cdecl;
  procedure onActivityCreated(savedInstanceState: JBundle); cdecl;
  procedure onAttach(activity: JActivity); cdecl;
  procedure onCreate(savedInstanceState: JBundle); cdecl;
  function onCreateView(inflater: JLayoutInflater; container: JViewGroup; savedInstanceState: JBundle): JView; cdecl;
  procedure onDestroy; cdecl;
  procedure onDestroyView; cdecl;
  procedure onInflate(activity: JActivity; attrs: JAttributeSet; savedInstanceState: JBundle); cdecl;
  procedure onLowMemory; cdecl;
  procedure onPause; cdecl;
  procedure onResume; cdecl;
  procedure onSaveInstanceState(outState: JBundle); cdecl;
  procedure setArguments(args: JBundle); cdecl;
end;
TJSupportMapFragment = class(TJavaGenericImport<JSupportMapFragmentClass, JSupportMapFragment>) end;

JPerson_OrderByClass = interface(IJavaClass)
['{873F91E7-0C9F-4333-AD45-4DAAFD7B7474}']
  {Property Methods}
  function _GetALPHABETICAL: Integer;
  function _GetBEST: Integer;
  {Properties}
  property ALPHABETICAL: Integer read _GetALPHABETICAL;
  property BEST: Integer read _GetBEST;
end;

[JavaSignature('com/google/android/gms/plus/model/people/Person$OrderBy')]
JPerson_OrderBy = interface(IJavaInstance)
['{2540F5B1-23BE-457C-8C86-F65AD8E64E3F}']
end;
TJPerson_OrderBy = class(TJavaGenericImport<JPerson_OrderByClass, JPerson_OrderBy>) end;

JGroundOverlayClass = interface(JObjectClass)
['{71A8228E-C56A-4DEE-B5DE-600E6FE8F24D}']
  {Methods}
  function init(delegate: JObject): JGroundOverlay; cdecl;
end;

[JavaSignature('com/google/android/gms/maps/model/GroundOverlay')]
JGroundOverlay = interface(JObject)
['{D9C7E7CE-26CA-4B65-B2B3-5A1C57E3AE3F}']
  {Methods}
  function equals(other: JObject): Boolean; cdecl;
  function getBearing: Single; cdecl;
  function getBounds: JLatLngBounds; cdecl;
  function getHeight: Single; cdecl;
  function getId: JString; cdecl;
  function getPosition: JLatLng; cdecl;
  function getTransparency: Single; cdecl;
  function getWidth: Single; cdecl;
  function getZIndex: Single; cdecl;
  function hashCode: Integer; cdecl;
  function isVisible: Boolean; cdecl;
  procedure remove; cdecl;
  procedure setBearing(bearing: Single); cdecl;
  procedure setDimensions(width: Single); cdecl; overload;
  procedure setDimensions(width: Single; height: Single); cdecl; overload;
  procedure setPosition(latLng: JLatLng); cdecl;
  procedure setPositionFromBounds(bounds: JLatLngBounds); cdecl;
  procedure setTransparency(transparency: Single); cdecl;
  procedure setVisible(visible: Boolean); cdecl;
  procedure setZIndex(zIndex: Single); cdecl;
end;
TJGroundOverlay = class(TJavaGenericImport<JGroundOverlayClass, JGroundOverlay>) end;

JPerson_RelationshipStatusClass = interface(JObjectClass)
['{16F8D0BC-2ECD-47A3-810F-425D0B22BC00}']
  {Property Methods}
  function _GetENGAGED: Integer;
  function _GetIN_A_RELATIONSHIP: Integer;
  function _GetIN_CIVIL_UNION: Integer;
  function _GetIN_DOMESTIC_PARTNERSHIP: Integer;
  function _GetITS_COMPLICATED: Integer;
  function _GetMARRIED: Integer;
  function _GetOPEN_RELATIONSHIP: Integer;
  function _GetSINGLE: Integer;
  function _GetWIDOWED: Integer;
  {Methods}
  function init: JPerson_RelationshipStatus; cdecl;
  {Properties}
  property ENGAGED: Integer read _GetENGAGED;
  property IN_A_RELATIONSHIP: Integer read _GetIN_A_RELATIONSHIP;
  property IN_CIVIL_UNION: Integer read _GetIN_CIVIL_UNION;
  property IN_DOMESTIC_PARTNERSHIP: Integer read _GetIN_DOMESTIC_PARTNERSHIP;
  property ITS_COMPLICATED: Integer read _GetITS_COMPLICATED;
  property MARRIED: Integer read _GetMARRIED;
  property OPEN_RELATIONSHIP: Integer read _GetOPEN_RELATIONSHIP;
  property SINGLE: Integer read _GetSINGLE;
  property WIDOWED: Integer read _GetWIDOWED;
end;

[JavaSignature('com/google/android/gms/plus/model/people/Person$RelationshipStatus')]
JPerson_RelationshipStatus = interface(JObject)
['{16665CEF-172C-493B-A0E3-A300E3569328}']
end;
TJPerson_RelationshipStatus = class(TJavaGenericImport<JPerson_RelationshipStatusClass, JPerson_RelationshipStatus>) end;

JDetectedActivityCreatorClass = interface(JObjectClass)
['{73440C81-DB55-48AC-983D-0B5D428937A2}']
  {Property Methods}
  function _GetCONTENT_DESCRIPTION: Integer;
  {Methods}
  function init: JDetectedActivityCreator; cdecl;
  {Properties}
  property CONTENT_DESCRIPTION: Integer read _GetCONTENT_DESCRIPTION;
end;

[JavaSignature('com/google/android/gms/location/DetectedActivityCreator')]
JDetectedActivityCreator = interface(JObject)
['{8FB1C7BF-D0DF-4BD7-B5FE-B9B5EA0C0A1B}']
  {Methods}
  function createFromParcel(parcel: JParcel): JDetectedActivity; cdecl;
  function newArray(size: Integer): TJavaObjectArray<JDetectedActivity>; cdecl;
end;
TJDetectedActivityCreator = class(TJavaGenericImport<JDetectedActivityCreatorClass, JDetectedActivityCreator>) end;

JCover_CoverPhotoClass = interface(JFreezableClass)
['{5E73FFC1-DF5D-41C2-A163-0673962BE9AE}']
end;

[JavaSignature('com/google/android/gms/plus/model/people/Person$Cover$CoverPhoto')]
JCover_CoverPhoto = interface(JFreezable)
['{33A3B764-B1B4-4E0F-B4C7-DAD6578FBCCD}']
  {Methods}
  function getHeight: Integer; cdecl;
  function getUrl: JString; cdecl;
  function getWidth: Integer; cdecl;
  function hasHeight: Boolean; cdecl;
  function hasUrl: Boolean; cdecl;
  function hasWidth: Boolean; cdecl;
end;
TJCover_CoverPhoto = class(TJavaGenericImport<JCover_CoverPhotoClass, JCover_CoverPhoto>) end;

JPlusClient_OnPersonLoadedListenerClass = interface(IJavaClass)
['{6715523C-7979-4560-87FB-E621D2F873DA}']
end;

[JavaSignature('com/google/android/gms/plus/PlusClient$OnPersonLoadedListener')]
JPlusClient_OnPersonLoadedListener = interface(IJavaInstance)
['{2D491C61-6BA2-4EB5-A89D-576EA844065F}']
  {Methods}
  procedure onPersonLoaded(paramConnectionResult: JConnectionResult; paramPerson: JPerson); cdecl;
end;
TJPlusClient_OnPersonLoadedListener = class(TJavaGenericImport<JPlusClient_OnPersonLoadedListenerClass, JPlusClient_OnPersonLoadedListener>) end;

JPlusClient_aClass = interface(IJavaClass)
['{442D3C23-B38C-46A6-838E-184FB0DEC58E}']
end;

[JavaSignature('com/google/android/gms/plus/PlusClient$a')]
JPlusClient_a = interface(IJavaInstance)
['{1379074C-02F0-41D4-BCD7-86F26EB22270}']
  {Methods}
  procedure a(paramConnectionResult: JConnectionResult; paramParcelFileDescriptor: JParcelFileDescriptor); cdecl;
end;
TJPlusClient_a = class(TJavaGenericImport<JPlusClient_aClass, JPlusClient_a>) end;

JGoogleMap_OnMyLocationChangeListenerClass = interface(IJavaClass)
['{A016AFD3-AE05-4306-80C4-DDED25000E54}']
end;

[JavaSignature('com/google/android/gms/maps/GoogleMap$OnMyLocationChangeListener')]
JGoogleMap_OnMyLocationChangeListener = interface(IJavaInstance)
['{44089694-6E57-4859-939D-97E59C7BAAAE}']
  {Methods}
  procedure onMyLocationChange(paramLocation: JLocation); cdecl;
end;
TJGoogleMap_OnMyLocationChangeListener = class(TJavaGenericImport<JGoogleMap_OnMyLocationChangeListenerClass, JGoogleMap_OnMyLocationChangeListener>) end;

JLocationRequestCreatorClass = interface(JObjectClass)
['{B08AC569-2E09-4BA7-A642-8E124C566B70}']
  {Property Methods}
  function _GetCONTENT_DESCRIPTION: Integer;
  {Methods}
  function init: JLocationRequestCreator; cdecl;
  {Properties}
  property CONTENT_DESCRIPTION: Integer read _GetCONTENT_DESCRIPTION;
end;

[JavaSignature('com/google/android/gms/location/LocationRequestCreator')]
JLocationRequestCreator = interface(JObject)
['{35AA2153-52CF-4BEE-82EC-6A666A3961C6}']
  {Methods}
  function createFromParcel(parcel: JParcel): JLocationRequest; cdecl;
  function newArray(size: Integer): TJavaObjectArray<JLocationRequest>; cdecl;
end;
TJLocationRequestCreator = class(TJavaGenericImport<JLocationRequestCreatorClass, JLocationRequestCreator>) end;

JParticipantBufferClass = interface(JDataBufferClass)
['{AABC49A4-2306-46B2-81E5-99EE05A6DDD5}']
  {Methods}
  function init: JParticipantBuffer; cdecl;
end;

[JavaSignature('com/google/android/gms/games/multiplayer/ParticipantBuffer')]
JParticipantBuffer = interface(JDataBuffer)
['{F756E11E-5E50-4947-BB3A-33A867A4FC43}']
  {Methods}
  function &get(position: Integer): JParticipant; cdecl;
end;
TJParticipantBuffer = class(TJavaGenericImport<JParticipantBufferClass, JParticipantBuffer>) end;

JMapViewClass = interface(JFrameLayoutClass)
['{1EE5FDF1-67E7-4FC9-AA41-D5B05E629B84}']
  {Methods}
  function init(context: JContext): JMapView; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet): JMapView; cdecl; overload;
  function init(context: JContext; attrs: JAttributeSet; defStyle: Integer): JMapView; cdecl; overload;
  function init(context: JContext; options: JGoogleMapOptions): JMapView; cdecl; overload;
end;

[JavaSignature('com/google/android/gms/maps/MapView')]
JMapView = interface(JFrameLayout)
['{FDC9DF68-5392-450C-A85C-E33754D8BFAD}']
  {Methods}
  function getMap: JGoogleMap; cdecl;
  procedure onCreate(savedInstanceState: JBundle); cdecl;
  procedure onDestroy; cdecl;
  procedure onLowMemory; cdecl;
  procedure onPause; cdecl;
  procedure onResume; cdecl;
  procedure onSaveInstanceState(outState: JBundle); cdecl;
end;
TJMapView = class(TJavaGenericImport<JMapViewClass, JMapView>) end;

JLocationClientClass = interface(JObjectClass)
['{19DD64B8-D7AB-46B0-8B0D-1BCE42BED9E8}']
  {Property Methods}
  function _GetKEY_LOCATION_CHANGED: JString;
  function _GetKEY_MOCK_LOCATION: JString;
  {Methods}
  function init(context: JContext; connectionCallbacks: JGooglePlayServicesClient_ConnectionCallbacks; connectionFailedListener: JGooglePlayServicesClient_OnConnectionFailedListener): JLocationClient; cdecl;
  function getErrorCode(intent: JIntent): Integer; cdecl;
  function getGeofenceTransition(intent: JIntent): Integer; cdecl;
  function getTriggeringGeofences(intent: JIntent): JList; cdecl;
  function hasError(intent: JIntent): Boolean; cdecl;
  {Properties}
  property KEY_LOCATION_CHANGED: JString read _GetKEY_LOCATION_CHANGED;
  property KEY_MOCK_LOCATION: JString read _GetKEY_MOCK_LOCATION;
end;

[JavaSignature('com/google/android/gms/location/LocationClient')]
JLocationClient = interface(JObject)
['{259FAD38-6C6B-4F7A-AB03-F40E8B41FF0E}']
  {Methods}
  procedure addGeofences(geofences: JList; pendingIntent: JPendingIntent; listener: JLocationClient_OnAddGeofencesResultListener); cdecl;
  procedure connect; cdecl;
  procedure disconnect; cdecl;
  function getLastLocation: JLocation; cdecl;
  function isConnected: Boolean; cdecl;
  function isConnecting: Boolean; cdecl;
  function isConnectionCallbacksRegistered(listener: JGooglePlayServicesClient_ConnectionCallbacks): Boolean; cdecl;
  function isConnectionFailedListenerRegistered(listener: JGooglePlayServicesClient_OnConnectionFailedListener): Boolean; cdecl;
  procedure registerConnectionCallbacks(listener: JGooglePlayServicesClient_ConnectionCallbacks); cdecl;
  procedure registerConnectionFailedListener(listener: JGooglePlayServicesClient_OnConnectionFailedListener); cdecl;
  procedure removeGeofences(pendingIntent: JPendingIntent; listener: JLocationClient_OnRemoveGeofencesResultListener); cdecl; overload;
  procedure removeGeofences(geofenceRequestIds: JList; listener: JLocationClient_OnRemoveGeofencesResultListener); cdecl; overload;
  procedure removeLocationUpdates(listener: Jlocation_LocationListener); cdecl; overload;
  procedure removeLocationUpdates(callbackIntent: JPendingIntent); cdecl; overload;
  procedure requestLocationUpdates(request: JLocationRequest; listener: Jlocation_LocationListener); cdecl; overload;
  procedure requestLocationUpdates(request: JLocationRequest; listener: Jlocation_LocationListener; looper: JLooper); cdecl; overload;
  procedure requestLocationUpdates(request: JLocationRequest; callbackIntent: JPendingIntent); cdecl; overload;
  procedure setMockLocation(mockLocation: JLocation); cdecl;
  procedure setMockMode(isMockMode: Boolean); cdecl;
  procedure unregisterConnectionCallbacks(listener: JGooglePlayServicesClient_ConnectionCallbacks); cdecl;
  procedure unregisterConnectionFailedListener(listener: JGooglePlayServicesClient_OnConnectionFailedListener); cdecl;
end;
TJLocationClient = class(TJavaGenericImport<JLocationClientClass, JLocationClient>) end;

JPerson_PlacesLivedClass = interface(JFreezableClass)
['{92550503-05E8-4307-AEC2-A3BFD35FEE3C}']
end;

[JavaSignature('com/google/android/gms/plus/model/people/Person$PlacesLived')]
JPerson_PlacesLived = interface(JFreezable)
['{087C2C27-6FA3-4A64-A848-530978D04BD9}']
  {Methods}
  function getValue: JString; cdecl;
  function hasPrimary: Boolean; cdecl;
  function hasValue: Boolean; cdecl;
  function isPrimary: Boolean; cdecl;
end;
TJPerson_PlacesLived = class(TJavaGenericImport<JPerson_PlacesLivedClass, JPerson_PlacesLived>) end;

JLifecycleDelegateClass = interface(IJavaClass)
['{93A34221-28BF-4077-B5D5-49BA16213C08}']
end;

[JavaSignature('com/google/android/gms/dynamic/LifecycleDelegate')]
JLifecycleDelegate = interface(IJavaInstance)
['{B7E98D3B-4DBE-4ED7-8BB5-0903FBE53E25}']
  {Methods}
  procedure onCreate(paramBundle: JBundle); cdecl;
  function onCreateView(paramLayoutInflater: JLayoutInflater; paramViewGroup: JViewGroup; paramBundle: JBundle): JView; cdecl;
  procedure onDestroy; cdecl;
  procedure onDestroyView; cdecl;
  procedure onInflate(paramActivity: JActivity; paramBundle1: JBundle; paramBundle2: JBundle); cdecl;
  procedure onLowMemory; cdecl;
  procedure onPause; cdecl;
  procedure onResume; cdecl;
  procedure onSaveInstanceState(paramBundle: JBundle); cdecl;
end;
TJLifecycleDelegate = class(TJavaGenericImport<JLifecycleDelegateClass, JLifecycleDelegate>) end;

JRuntimeRemoteExceptionClass = interface(JRuntimeExceptionClass)
['{75B6D1D4-4516-4CE8-9B0A-2BCCDA0A51B5}']
  {Methods}
  function init(e: JRemoteException): JRuntimeRemoteException; cdecl;
end;

[JavaSignature('com/google/android/gms/maps/model/RuntimeRemoteException')]
JRuntimeRemoteException = interface(JRuntimeException)
['{E6608F23-8EE1-4056-8FB7-CF5C8927EB08}']
end;
TJRuntimeRemoteException = class(TJavaGenericImport<JRuntimeRemoteExceptionClass, JRuntimeRemoteException>) end;

JSubmitScoreResult_ResultClass = interface(JObjectClass)
['{B59C98AF-D7A2-4253-9D0A-4600EFBC33BD}']
  {Methods}
  function init(rawScore: Int64; formattedScore: JString; newBest: Boolean): JSubmitScoreResult_Result; cdecl;
end;

[JavaSignature('com/google/android/gms/games/leaderboard/SubmitScoreResult$Result')]
JSubmitScoreResult_Result = interface(JObject)
['{081DA010-935B-468B-84FB-C5003495D743}']
  {Property Methods}
  function _GetformattedScore: JString;
  function _GetnewBest: Boolean;
  function _GetrawScore: Int64;
  {Methods}
  function toString: JString; cdecl;
  {Properties}
  property formattedScore: JString read _GetformattedScore;
  property newBest: Boolean read _GetnewBest;
  property rawScore: Int64 read _GetrawScore;
end;
TJSubmitScoreResult_Result = class(TJavaGenericImport<JSubmitScoreResult_ResultClass, JSubmitScoreResult_Result>) end;

JConnectionResultClass = interface(JObjectClass)
['{4A2A7E79-F9D7-4636-A3CF-C291BEF18896}']
  {Property Methods}
  function _GetB: JConnectionResult;
  function _GetDEVELOPER_ERROR: Integer;
  function _GetINTERNAL_ERROR: Integer;
  function _GetINVALID_ACCOUNT: Integer;
  function _GetLICENSE_CHECK_FAILED: Integer;
  function _GetNETWORK_ERROR: Integer;
  function _GetRESOLUTION_REQUIRED: Integer;
  function _GetSERVICE_DISABLED: Integer;
  function _GetSERVICE_INVALID: Integer;
  function _GetSERVICE_MISSING: Integer;
  function _GetSERVICE_VERSION_UPDATE_REQUIRED: Integer;
  function _GetSIGN_IN_REQUIRED: Integer;
  function _GetSUCCESS: Integer;
  {Methods}
  function init(statusCode: Integer; pendingIntent: JPendingIntent): JConnectionResult; cdecl;
  {Properties}
  property B: JConnectionResult read _GetB;
  property DEVELOPER_ERROR: Integer read _GetDEVELOPER_ERROR;
  property INTERNAL_ERROR: Integer read _GetINTERNAL_ERROR;
  property INVALID_ACCOUNT: Integer read _GetINVALID_ACCOUNT;
  property LICENSE_CHECK_FAILED: Integer read _GetLICENSE_CHECK_FAILED;
  property NETWORK_ERROR: Integer read _GetNETWORK_ERROR;
  property RESOLUTION_REQUIRED: Integer read _GetRESOLUTION_REQUIRED;
  property SERVICE_DISABLED: Integer read _GetSERVICE_DISABLED;
  property SERVICE_INVALID: Integer read _GetSERVICE_INVALID;
  property SERVICE_MISSING: Integer read _GetSERVICE_MISSING;
  property SERVICE_VERSION_UPDATE_REQUIRED: Integer read _GetSERVICE_VERSION_UPDATE_REQUIRED;
  property SIGN_IN_REQUIRED: Integer read _GetSIGN_IN_REQUIRED;
  property SUCCESS: Integer read _GetSUCCESS;
end;

[JavaSignature('com/google/android/gms/common/ConnectionResult')]
JConnectionResult = interface(JObject)
['{A6C69B94-8D31-4C7B-9001-1DDF4A76D784}']
  {Methods}
  function getErrorCode: Integer; cdecl;
  function getResolution: JPendingIntent; cdecl;
  function hasResolution: Boolean; cdecl;
  function isSuccess: Boolean; cdecl;
  procedure startResolutionForResult(activity: JActivity; requestCode: Integer); cdecl;
  function toString: JString; cdecl;
end;
TJConnectionResult = class(TJavaGenericImport<JConnectionResultClass, JConnectionResult>) end;

JSubmitScoreResultClass = interface(JObjectClass)
['{D3AA7BAB-2529-4CC9-B84A-3AC45B591E7E}']
  {Methods}
  function init(statusCode: Integer; leaderboardId: JString; playerId: JString; results: JHashMap): JSubmitScoreResult; cdecl; overload;
  function init(statusCode: Integer; leaderboardId: JString; playerId: JString): JSubmitScoreResult; cdecl; overload;
  function init(dataHolder: JObject): JSubmitScoreResult; cdecl; overload;
end;

[JavaSignature('com/google/android/gms/games/leaderboard/SubmitScoreResult')]
JSubmitScoreResult = interface(JObject)
['{DDA5A697-51BE-4B92-8C20-4D4DF728F634}']
  {Methods}
  function getLeaderboardId: JString; cdecl;
  function getPlayerId: JString; cdecl;
  function getScoreResult(timeSpan: Integer): JSubmitScoreResult_Result; cdecl;
  function getStatusCode: Integer; cdecl;
  function toString: JString; cdecl;
end;
TJSubmitScoreResult = class(TJavaGenericImport<JSubmitScoreResultClass, JSubmitScoreResult>) end;

JLeaderboardScoreBufferClass = interface(JDataBufferClass)
['{2170DADE-C803-4F52-9F02-F55510214415}']
  {Methods}
  function init(dataHolder: JObject): JLeaderboardScoreBuffer; cdecl;
end;

[JavaSignature('com/google/android/gms/games/leaderboard/LeaderboardScoreBuffer')]
JLeaderboardScoreBuffer = interface(JDataBuffer)
['{215A25BB-ACE6-43A5-9B5F-F1D1733A5F78}']
  {Methods}
  function aD: JObject; cdecl;
  function &get(position: Integer): JLeaderboardScore; cdecl;
end;
TJLeaderboardScoreBuffer = class(TJavaGenericImport<JLeaderboardScoreBufferClass, JLeaderboardScoreBuffer>) end;

JParticipantEntityCreatorClass = interface(JObjectClass)
['{9F633FD3-50A8-41B4-82CA-AB8F553B2B4A}']
  {Property Methods}
  function _GetCONTENT_DESCRIPTION: Integer;
  {Methods}
  function init: JParticipantEntityCreator; cdecl;
  {Properties}
  property CONTENT_DESCRIPTION: Integer read _GetCONTENT_DESCRIPTION;
end;

[JavaSignature('com/google/android/gms/games/multiplayer/ParticipantEntityCreator')]
JParticipantEntityCreator = interface(JObject)
['{B8A24E4A-3464-4112-A4E5-1F49E9B09454}']
  {Methods}
  function createFromParcel(parcel: JParcel): JParticipantEntity; cdecl;
  function newArray(size: Integer): TJavaObjectArray<JParticipantEntity>; cdecl;
end;
TJParticipantEntityCreator = class(TJavaGenericImport<JParticipantEntityCreatorClass, JParticipantEntityCreator>) end;

JILocationSourceDelegate_aClass = interface(JBinderClass)
['{FB4A99FA-8456-4B95-A382-022B5A079420}']
  {Methods}
  function init: JILocationSourceDelegate_a; cdecl;
  function w(paramIBinder: JIBinder): JILocationSourceDelegate; cdecl;
end;

[JavaSignature('com/google/android/gms/maps/internal/ILocationSourceDelegate$a')]
JILocationSourceDelegate_a = interface(JBinder)
['{4B9BAAA0-F6F0-40B7-9BAF-F5F9884ED9EF}']
  {Methods}
  function asBinder: JIBinder; cdecl;
  function onTransact(code: Integer; data: JParcel; reply: JParcel; flags: Integer): Boolean; cdecl;
end;
TJILocationSourceDelegate_a = class(TJavaGenericImport<JILocationSourceDelegate_aClass, JILocationSourceDelegate_a>) end;

JCircleOptionsCreatorClass = interface(JObjectClass)
['{0600AB5B-B6EA-4D7D-A8A0-3F70729792A8}']
  {Property Methods}
  function _GetCONTENT_DESCRIPTION: Integer;
  {Methods}
  function init: JCircleOptionsCreator; cdecl;
  {Properties}
  property CONTENT_DESCRIPTION: Integer read _GetCONTENT_DESCRIPTION;
end;

[JavaSignature('com/google/android/gms/maps/model/CircleOptionsCreator')]
JCircleOptionsCreator = interface(JObject)
['{B6976977-6EDF-45B0-9167-3AB2D82A3444}']
  {Methods}
  function createFromParcel(parcel: JParcel): JCircleOptions; cdecl;
  function newArray(size: Integer): TJavaObjectArray<JCircleOptions>; cdecl;
end;
TJCircleOptionsCreator = class(TJavaGenericImport<JCircleOptionsCreatorClass, JCircleOptionsCreator>) end;

JMapsInitializerClass = interface(JObjectClass)
['{589D1A92-1661-448C-9560-8F20ABA1157C}']
  {Methods}
  function init: JMapsInitializer; cdecl;
  procedure initialize(context: JContext); cdecl;
end;

[JavaSignature('com/google/android/gms/maps/MapsInitializer')]
JMapsInitializer = interface(JObject)
['{8708DF65-6068-4C6E-B345-603FC91F7C6A}']
end;
TJMapsInitializer = class(TJavaGenericImport<JMapsInitializerClass, JMapsInitializer>) end;

JGooglePlayServicesClient_ConnectionCallbacksClass = interface(IJavaClass)
['{2830C271-7F66-4DA7-AD93-026E416C3FCC}']
end;

[JavaSignature('com/google/android/gms/common/GooglePlayServicesClient$ConnectionCallbacks')]
JGooglePlayServicesClient_ConnectionCallbacks = interface(IJavaInstance)
['{4FB49886-F762-4DB3-A74C-75B5AF669744}']
  {Methods}
  procedure onConnected(paramBundle: JBundle); cdecl;
  procedure onDisconnected; cdecl;
end;
TJGooglePlayServicesClient_ConnectionCallbacks = class(TJavaGenericImport<JGooglePlayServicesClient_ConnectionCallbacksClass, JGooglePlayServicesClient_ConnectionCallbacks>) end;

JRoomEntityCreatorClass = interface(JObjectClass)
['{5ADBAE0F-4455-4462-8786-F9A569E6729B}']
  {Property Methods}
  function _GetCONTENT_DESCRIPTION: Integer;
  {Methods}
  function init: JRoomEntityCreator; cdecl;
  {Properties}
  property CONTENT_DESCRIPTION: Integer read _GetCONTENT_DESCRIPTION;
end;

[JavaSignature('com/google/android/gms/games/multiplayer/realtime/RoomEntityCreator')]
JRoomEntityCreator = interface(JObject)
['{E0A46DE1-C460-494F-A257-06E6D6F7932D}']
  {Methods}
  function createFromParcel(parcel: JParcel): JRoomEntity; cdecl;
  function newArray(size: Integer): TJavaObjectArray<JRoomEntity>; cdecl;
end;
TJRoomEntityCreator = class(TJavaGenericImport<JRoomEntityCreatorClass, JRoomEntityCreator>) end;

JOnPlayersLoadedListenerClass = interface(IJavaClass)
['{B841FD80-8993-4046-9A90-C5397B65E2BA}']
end;

[JavaSignature('com/google/android/gms/games/OnPlayersLoadedListener')]
JOnPlayersLoadedListener = interface(IJavaInstance)
['{E1D5AA80-9536-450F-9A8F-0798C3FA49E4}']
  {Methods}
  procedure onPlayersLoaded(paramInt: Integer; paramPlayerBuffer: JPlayerBuffer); cdecl;
end;
TJOnPlayersLoadedListener = class(TJavaGenericImport<JOnPlayersLoadedListenerClass, JOnPlayersLoadedListener>) end;

JPlusClient_OnPeopleLoadedListenerClass = interface(IJavaClass)
['{3ED45DFB-197C-41ED-8DF6-E73FA84D02E8}']
end;

[JavaSignature('com/google/android/gms/plus/PlusClient$OnPeopleLoadedListener')]
JPlusClient_OnPeopleLoadedListener = interface(IJavaInstance)
['{4ECCBDF1-0DB9-48F9-BF3C-15F7B09E12E7}']
  {Methods}
  procedure onPeopleLoaded(paramConnectionResult: JConnectionResult; paramPersonBuffer: JPersonBuffer; paramString: JString); cdecl;
end;
TJPlusClient_OnPeopleLoadedListener = class(TJavaGenericImport<JPlusClient_OnPeopleLoadedListenerClass, JPlusClient_OnPeopleLoadedListener>) end;

JGoogleMap_OnCameraChangeListenerClass = interface(IJavaClass)
['{DCCF80DA-1231-4C0C-9161-93BA6A899D9A}']
end;

[JavaSignature('com/google/android/gms/maps/GoogleMap$OnCameraChangeListener')]
JGoogleMap_OnCameraChangeListener = interface(IJavaInstance)
['{599287B7-6F74-4D70-822D-9C55486B2C89}']
  {Methods}
  procedure onCameraChange(paramCameraPosition: JCameraPosition); cdecl;
end;
TJGoogleMap_OnCameraChangeListener = class(TJavaGenericImport<JGoogleMap_OnCameraChangeListenerClass, JGoogleMap_OnCameraChangeListener>) end;

JGoogleMap_OnInfoWindowClickListenerClass = interface(IJavaClass)
['{020474F8-83E2-4E62-A37E-F07D54D4298A}']
end;

[JavaSignature('com/google/android/gms/maps/GoogleMap$OnInfoWindowClickListener')]
JGoogleMap_OnInfoWindowClickListener = interface(IJavaInstance)
['{FB213E06-A38F-4C91-998D-AB98AF202C29}']
  {Methods}
  procedure onInfoWindowClick(paramMarker: JMarker); cdecl;
end;
TJGoogleMap_OnInfoWindowClickListener = class(TJavaGenericImport<JGoogleMap_OnInfoWindowClickListenerClass, JGoogleMap_OnInfoWindowClickListener>) end;

JMomentBufferClass = interface(JDataBufferClass)
['{416E2747-6851-4A1B-8EBE-F614C92C251D}']
  {Methods}
  function init(dataHolder: JObject): JMomentBuffer; cdecl;
end;

[JavaSignature('com/google/android/gms/plus/model/moments/MomentBuffer')]
JMomentBuffer = interface(JDataBuffer)
['{B8DEF9D3-75C0-4C86-A439-6612B998A9EA}']
  {Methods}
  function &get(position: Integer): JMoment; cdecl;
end;
TJMomentBuffer = class(TJavaGenericImport<JMomentBufferClass, JMomentBuffer>) end;

JPolygonOptionsClass = interface(JObjectClass)
['{035017D5-E83D-4EBD-9159-872BB97687F8}']
  {Property Methods}
  function _GetCREATOR: JPolygonOptionsCreator;
  {Methods}
  function init: JPolygonOptions; cdecl;
  {Properties}
  property CREATOR: JPolygonOptionsCreator read _GetCREATOR;
end;

[JavaSignature('com/google/android/gms/maps/model/PolygonOptions')]
JPolygonOptions = interface(JObject)
['{D44C6C92-3D47-4E5F-8BA2-0AF69743B24E}']
  {Methods}
  function F: Integer; cdecl;
  function add(point: JLatLng): JPolygonOptions; cdecl; overload;
  function add(points: TJavaObjectArray<JLatLng>): JPolygonOptions; cdecl; overload;
  function addAll(points: JIterable): JPolygonOptions; cdecl;
  function addHole(points: JIterable): JPolygonOptions; cdecl;
  function bq: JList; cdecl;
  function describeContents: Integer; cdecl;
  function fillColor(color: Integer): JPolygonOptions; cdecl;
  function geodesic(geodesic: Boolean): JPolygonOptions; cdecl;
  function getFillColor: Integer; cdecl;
  function getHoles: JList; cdecl;
  function getPoints: JList; cdecl;
  function getStrokeColor: Integer; cdecl;
  function getStrokeWidth: Single; cdecl;
  function getZIndex: Single; cdecl;
  function isGeodesic: Boolean; cdecl;
  function isVisible: Boolean; cdecl;
  function strokeColor(color: Integer): JPolygonOptions; cdecl;
  function strokeWidth(width: Single): JPolygonOptions; cdecl;
  function visible(visible: Boolean): JPolygonOptions; cdecl;
  procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
  function zIndex(zIndex: Single): JPolygonOptions; cdecl;
end;
TJPolygonOptions = class(TJavaGenericImport<JPolygonOptionsClass, JPolygonOptions>) end;

JAppStateBufferClass = interface(JDataBufferClass)
['{C12CC577-CE18-4694-884A-1D2F9ECEF29A}']
  {Methods}
  function init(dataHolder: JObject): JAppStateBuffer; cdecl;
end;

[JavaSignature('com/google/android/gms/appstate/AppStateBuffer')]
JAppStateBuffer = interface(JDataBuffer)
['{1568FAAC-8015-41E6-9533-29837363DE96}']
  {Methods}
  function &get(position: Integer): JAppState; cdecl;
end;
TJAppStateBuffer = class(TJavaGenericImport<JAppStateBufferClass, JAppStateBuffer>) end;

JImageManagerClass = interface(JObjectClass)
['{A9B72249-08F9-4577-84D5-2D78D0799E1F}']
  {Methods}
  function a(paramContext: JContext; paramBoolean: Boolean): JImageManager; cdecl; overload;
  function create(context: JContext): JImageManager; cdecl;
end;

[JavaSignature('com/google/android/gms/common/images/ImageManager')]
JImageManager = interface(JObject)
['{87EDA32C-22FC-4A0A-B710-2DA3604AA1DD}']
  {Methods}
  procedure a(paramo: JObject); cdecl; overload;
  procedure loadImage(imageView: JImageView; uri: Jnet_Uri); cdecl; overload;
  procedure loadImage(imageView: JImageView; resId: Integer); cdecl; overload;
  procedure loadImage(imageView: JImageView; uri: Jnet_Uri; defaultResId: Integer); cdecl; overload;
  procedure loadImage(listener: JImageManager_OnImageLoadedListener; uri: Jnet_Uri); cdecl; overload;
  procedure loadImage(listener: JImageManager_OnImageLoadedListener; uri: Jnet_Uri; defaultResId: Integer); cdecl; overload;
end;
TJImageManager = class(TJavaGenericImport<JImageManagerClass, JImageManager>) end;

JAccountPickerClass = interface(JObjectClass)
['{911C15A8-C95E-451F-8179-C537DAEEF9D5}']
  {Methods}
  function init: JAccountPicker; cdecl;
end;

[JavaSignature('com/google/android/gms/common/AccountPicker')]
JAccountPicker = interface(JObject)
['{B0B75FAA-A0D7-45C1-A0BC-FF63522ED979}']
end;
TJAccountPicker = class(TJavaGenericImport<JAccountPickerClass, JAccountPicker>) end;

JGoogleMap_OnMapLongClickListenerClass = interface(IJavaClass)
['{17AD8C33-1410-4CB4-8DCA-9E4FF3A872BA}']
end;

[JavaSignature('com/google/android/gms/maps/GoogleMap$OnMapLongClickListener')]
JGoogleMap_OnMapLongClickListener = interface(IJavaInstance)
['{F0789EA1-C5E2-4269-A67A-5FF1B0482CA5}']
  {Methods}
  procedure onMapLongClick(paramLatLng: JLatLng); cdecl;
end;
TJGoogleMap_OnMapLongClickListener = class(TJavaGenericImport<JGoogleMap_OnMapLongClickListenerClass, JGoogleMap_OnMapLongClickListener>) end;

JCameraUpdateClass = interface(JObjectClass)
['{7A21197F-BCBA-493B-B258-0631CADCB0FF}']
end;

[JavaSignature('com/google/android/gms/maps/CameraUpdate')]
JCameraUpdate = interface(JObject)
['{11606994-4F45-4D5B-BA4E-22ABCAFE5A15}']
  {Methods}
  function aU: JObject; cdecl;
end;
TJCameraUpdate = class(TJavaGenericImport<JCameraUpdateClass, JCameraUpdate>) end;

JGooglePlayServicesUtilClass = interface(JObjectClass)
['{DEE7FB7C-B1A2-4098-9E42-67A2CF856C16}']
  {Property Methods}
  function _GetGOOGLE_PLAY_SERVICES_PACKAGE: JString;
  function _GetGOOGLE_PLAY_SERVICES_VERSION_CODE: Integer;
  function _GetGOOGLE_PLAY_STORE_PACKAGE: JString;
  function _GetH: Boolean;
  procedure _SetH(Value: Boolean);
  function _GetI: Boolean;
  procedure _SetI(Value: Boolean);
  {Methods}
  function init: JGooglePlayServicesUtil; cdecl;
  function a(paramContext: JContext; paramInt1: Integer; paramInt2: Integer): JIntent; cdecl; overload;
  function a(paramContext: JContext; paramInt: Integer): JString; cdecl; overload;
  function a(paramInt1: Integer; paramActivity: JActivity; paramInt2: Integer; paramOnCancelListener: JObject; paramInt3: Integer): JDialog; cdecl; overload;
  function a(paramResources: JResources): Boolean; cdecl; overload;
  function b(paramContext: JContext; paramInt1: Integer; paramInt2: Integer): JString; cdecl;
  function g: Boolean; cdecl;
  function getErrorDialog(errorCode: Integer; activity: JActivity; requestCode: Integer): JDialog; cdecl; overload;
  function getErrorDialog(errorCode: Integer; activity: JActivity; requestCode: Integer; cancelListener: JObject): JDialog; cdecl; overload;
  function getErrorPendingIntent(errorCode: Integer; context: JContext; requestCode: Integer): JPendingIntent; cdecl;
  function getErrorString(errorCode: Integer): JString; cdecl;
  function getOpenSourceSoftwareLicenseInfo(context: JContext): JString; cdecl;
  function getRemoteContext(context: JContext): JContext; cdecl;
  function getRemoteResource(context: JContext): JResources; cdecl;
  function isGooglePlayServicesAvailable(context: JContext): Integer; cdecl;
  function isUserRecoverableError(errorCode: Integer): Boolean; cdecl;
  {Properties}
  property GOOGLE_PLAY_SERVICES_PACKAGE: JString read _GetGOOGLE_PLAY_SERVICES_PACKAGE;
  property GOOGLE_PLAY_SERVICES_VERSION_CODE: Integer read _GetGOOGLE_PLAY_SERVICES_VERSION_CODE;
  property GOOGLE_PLAY_STORE_PACKAGE: JString read _GetGOOGLE_PLAY_STORE_PACKAGE;
  property H: Boolean read _GetH write _SetH;
  property I: Boolean read _GetI write _SetI;
end;

[JavaSignature('com/google/android/gms/common/GooglePlayServicesUtil')]
JGooglePlayServicesUtil = interface(JObject)
['{3549FA2E-E8D1-4F37-A21B-B5E10E6169D1}']
end;
TJGooglePlayServicesUtil = class(TJavaGenericImport<JGooglePlayServicesUtilClass, JGooglePlayServicesUtil>) end;

JPageDirectionClass = interface(JObjectClass)
['{79519738-99BE-4131-987E-B1C3E9648B3C}']
  {Property Methods}
  function _GetNEXT: Integer;
  function _GetNONE: Integer;
  function _GetPREV: Integer;
  {Properties}
  property NEXT: Integer read _GetNEXT;
  property NONE: Integer read _GetNONE;
  property PREV: Integer read _GetPREV;
end;

[JavaSignature('com/google/android/gms/games/PageDirection')]
JPageDirection = interface(JObject)
['{9773AAC5-7C05-4C27-9814-DC5143192F70}']
end;
TJPageDirection = class(TJavaGenericImport<JPageDirectionClass, JPageDirection>) end;

JOnStateLoadedListenerClass = interface(IJavaClass)
['{75A4429C-1EB7-40A1-BCDE-0EDD8B5A9287}']
end;

[JavaSignature('com/google/android/gms/appstate/OnStateLoadedListener')]
JOnStateLoadedListener = interface(IJavaInstance)
['{C3592DE9-23BC-411D-8318-26E62A716083}']
  {Methods}
  procedure onStateConflict(paramInt: Integer; paramString: JString; paramArrayOfByte1: TJavaArray<Byte>; paramArrayOfByte2: TJavaArray<Byte>); cdecl;
  procedure onStateLoaded(paramInt1: Integer; paramInt2: Integer; paramArrayOfByte: TJavaArray<Byte>); cdecl;
end;
TJOnStateLoadedListener = class(TJavaGenericImport<JOnStateLoadedListenerClass, JOnStateLoadedListener>) end;

JTileOverlayOptionsClass = interface(JObjectClass)
['{C3818656-1176-498F-A0E8-106091DEAE4B}']
  {Property Methods}
  function _GetCREATOR: JTileOverlayOptionsCreator;
  {Methods}
  function init: JTileOverlayOptions; cdecl;
  {Properties}
  property CREATOR: JTileOverlayOptionsCreator read _GetCREATOR;
end;

[JavaSignature('com/google/android/gms/maps/model/TileOverlayOptions')]
JTileOverlayOptions = interface(JObject)
['{514FFD3B-E0C2-4AD5-89EE-3EB07D1D03AF}']
  {Methods}
  function F: Integer; cdecl;
  function br: JIBinder; cdecl;
  function describeContents: Integer; cdecl;
  function getTileProvider: JTileProvider; cdecl;
  function getZIndex: Single; cdecl;
  function isVisible: Boolean; cdecl;
  function tileProvider(tileProvider: JTileProvider): JTileOverlayOptions; cdecl;
  function visible(visible: Boolean): JTileOverlayOptions; cdecl;
  procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
  function zIndex(zIndex: Single): JTileOverlayOptions; cdecl;
end;
TJTileOverlayOptions = class(TJavaGenericImport<JTileOverlayOptionsClass, JTileOverlayOptions>) end;

JGamesClientClass = interface(JObjectClass)
['{A550A093-5320-40D5-A9E8-8E8FDD23BE74}']
  {Property Methods}
  function _GetEXTRA_EXCLUSIVE_BIT_MASK: JString;
  function _GetEXTRA_INVITATION: JString;
  function _GetEXTRA_MAX_AUTOMATCH_PLAYERS: JString;
  function _GetEXTRA_MIN_AUTOMATCH_PLAYERS: JString;
  function _GetEXTRA_PLAYERS: JString;
  function _GetEXTRA_ROOM: JString;
  function _GetMAX_RELIABLE_MESSAGE_LEN: Integer;
  function _GetMAX_UNRELIABLE_MESSAGE_LEN: Integer;
  function _GetNOTIFICATION_TYPES_ALL: Integer;
  function _GetNOTIFICATION_TYPES_MULTIPLAYER: Integer;
  function _GetNOTIFICATION_TYPE_INVITATION: Integer;
  function _GetSTATUS_ACHIEVEMENT_NOT_INCREMENTAL: Integer;
  function _GetSTATUS_ACHIEVEMENT_UNKNOWN: Integer;
  function _GetSTATUS_ACHIEVEMENT_UNLOCKED: Integer;
  function _GetSTATUS_ACHIEVEMENT_UNLOCK_FAILURE: Integer;
  function _GetSTATUS_CLIENT_RECONNECT_REQUIRED: Integer;
  function _GetSTATUS_INTERNAL_ERROR: Integer;
  function _GetSTATUS_INVALID_REAL_TIME_ROOM_ID: Integer;
  function _GetSTATUS_LICENSE_CHECK_FAILED: Integer;
  function _GetSTATUS_MULTIPLAYER_ERROR_CREATION_NOT_ALLOWED: Integer;
  function _GetSTATUS_MULTIPLAYER_ERROR_NOT_TRUSTED_TESTER: Integer;
  function _GetSTATUS_NETWORK_ERROR_NO_DATA: Integer;
  function _GetSTATUS_NETWORK_ERROR_OPERATION_DEFERRED: Integer;
  function _GetSTATUS_NETWORK_ERROR_OPERATION_FAILED: Integer;
  function _GetSTATUS_NETWORK_ERROR_STALE_DATA: Integer;
  function _GetSTATUS_OK: Integer;
  function _GetSTATUS_PARTICIPANT_NOT_CONNECTED: Integer;
  function _GetSTATUS_REAL_TIME_CONNECTION_FAILED: Integer;
  function _GetSTATUS_REAL_TIME_INACTIVE_ROOM: Integer;
  function _GetSTATUS_REAL_TIME_MESSAGE_FAILED: Integer;
  function _GetSTATUS_REAL_TIME_MESSAGE_SEND_FAILED: Integer;
  function _GetSTATUS_REAL_TIME_ROOM_NOT_JOINED: Integer;
  {Properties}
  property EXTRA_EXCLUSIVE_BIT_MASK: JString read _GetEXTRA_EXCLUSIVE_BIT_MASK;
  property EXTRA_INVITATION: JString read _GetEXTRA_INVITATION;
  property EXTRA_MAX_AUTOMATCH_PLAYERS: JString read _GetEXTRA_MAX_AUTOMATCH_PLAYERS;
  property EXTRA_MIN_AUTOMATCH_PLAYERS: JString read _GetEXTRA_MIN_AUTOMATCH_PLAYERS;
  property EXTRA_PLAYERS: JString read _GetEXTRA_PLAYERS;
  property EXTRA_ROOM: JString read _GetEXTRA_ROOM;
  property MAX_RELIABLE_MESSAGE_LEN: Integer read _GetMAX_RELIABLE_MESSAGE_LEN;
  property MAX_UNRELIABLE_MESSAGE_LEN: Integer read _GetMAX_UNRELIABLE_MESSAGE_LEN;
  property NOTIFICATION_TYPES_ALL: Integer read _GetNOTIFICATION_TYPES_ALL;
  property NOTIFICATION_TYPES_MULTIPLAYER: Integer read _GetNOTIFICATION_TYPES_MULTIPLAYER;
  property NOTIFICATION_TYPE_INVITATION: Integer read _GetNOTIFICATION_TYPE_INVITATION;
  property STATUS_ACHIEVEMENT_NOT_INCREMENTAL: Integer read _GetSTATUS_ACHIEVEMENT_NOT_INCREMENTAL;
  property STATUS_ACHIEVEMENT_UNKNOWN: Integer read _GetSTATUS_ACHIEVEMENT_UNKNOWN;
  property STATUS_ACHIEVEMENT_UNLOCKED: Integer read _GetSTATUS_ACHIEVEMENT_UNLOCKED;
  property STATUS_ACHIEVEMENT_UNLOCK_FAILURE: Integer read _GetSTATUS_ACHIEVEMENT_UNLOCK_FAILURE;
  property STATUS_CLIENT_RECONNECT_REQUIRED: Integer read _GetSTATUS_CLIENT_RECONNECT_REQUIRED;
  property STATUS_INTERNAL_ERROR: Integer read _GetSTATUS_INTERNAL_ERROR;
  property STATUS_INVALID_REAL_TIME_ROOM_ID: Integer read _GetSTATUS_INVALID_REAL_TIME_ROOM_ID;
  property STATUS_LICENSE_CHECK_FAILED: Integer read _GetSTATUS_LICENSE_CHECK_FAILED;
  property STATUS_MULTIPLAYER_ERROR_CREATION_NOT_ALLOWED: Integer read _GetSTATUS_MULTIPLAYER_ERROR_CREATION_NOT_ALLOWED;
  property STATUS_MULTIPLAYER_ERROR_NOT_TRUSTED_TESTER: Integer read _GetSTATUS_MULTIPLAYER_ERROR_NOT_TRUSTED_TESTER;
  property STATUS_NETWORK_ERROR_NO_DATA: Integer read _GetSTATUS_NETWORK_ERROR_NO_DATA;
  property STATUS_NETWORK_ERROR_OPERATION_DEFERRED: Integer read _GetSTATUS_NETWORK_ERROR_OPERATION_DEFERRED;
  property STATUS_NETWORK_ERROR_OPERATION_FAILED: Integer read _GetSTATUS_NETWORK_ERROR_OPERATION_FAILED;
  property STATUS_NETWORK_ERROR_STALE_DATA: Integer read _GetSTATUS_NETWORK_ERROR_STALE_DATA;
  property STATUS_OK: Integer read _GetSTATUS_OK;
  property STATUS_PARTICIPANT_NOT_CONNECTED: Integer read _GetSTATUS_PARTICIPANT_NOT_CONNECTED;
  property STATUS_REAL_TIME_CONNECTION_FAILED: Integer read _GetSTATUS_REAL_TIME_CONNECTION_FAILED;
  property STATUS_REAL_TIME_INACTIVE_ROOM: Integer read _GetSTATUS_REAL_TIME_INACTIVE_ROOM;
  property STATUS_REAL_TIME_MESSAGE_FAILED: Integer read _GetSTATUS_REAL_TIME_MESSAGE_FAILED;
  property STATUS_REAL_TIME_MESSAGE_SEND_FAILED: Integer read _GetSTATUS_REAL_TIME_MESSAGE_SEND_FAILED;
  property STATUS_REAL_TIME_ROOM_NOT_JOINED: Integer read _GetSTATUS_REAL_TIME_ROOM_NOT_JOINED;
end;

[JavaSignature('com/google/android/gms/games/GamesClient')]
JGamesClient = interface(JObject)
['{52D3B726-68B2-4015-A47F-DD4E80F48282}']
  {Methods}
  procedure clearAllNotifications; cdecl;
  procedure clearNotifications(notificationTypes: Integer); cdecl;
  procedure connect; cdecl;
  procedure createRoom(config: JRoomConfig); cdecl;
  procedure declineRoomInvitation(invitationId: JString); cdecl;
  procedure disconnect; cdecl;
  procedure dismissRoomInvitation(invitationId: JString); cdecl;
  function getAchievementsIntent: JIntent; cdecl;
  function getAllLeaderboardsIntent: JIntent; cdecl;
  function getAppId: JString; cdecl;
  function getCurrentAccountName: JString; cdecl;
  function getCurrentGame: JGame; cdecl;
  function getCurrentPlayer: JPlayer; cdecl;
  function getCurrentPlayerId: JString; cdecl;
  function getInvitationInboxIntent: JIntent; cdecl;
  function getLeaderboardIntent(leaderboardId: JString): JIntent; cdecl;
  function getRealTimeSocketForParticipant(roomId: JString; participantId: JString): JRealTimeSocket; cdecl;
  function getRealTimeWaitingRoomIntent(room: JRoom; minParticipantsToStart: Integer): JIntent; cdecl;
  function getSelectPlayersIntent(minPlayers: Integer; maxPlayers: Integer): JIntent; cdecl;
  function getSettingsIntent: JIntent; cdecl;
  procedure incrementAchievement(id: JString; numSteps: Integer); cdecl;
  procedure incrementAchievementImmediate(listener: JOnAchievementUpdatedListener; id: JString; numSteps: Integer); cdecl;
  function isConnected: Boolean; cdecl;
  function isConnecting: Boolean; cdecl;
  function isConnectionCallbacksRegistered(listener: JGooglePlayServicesClient_ConnectionCallbacks): Boolean; cdecl;
  function isConnectionFailedListenerRegistered(listener: JGooglePlayServicesClient_OnConnectionFailedListener): Boolean; cdecl;
  procedure joinRoom(config: JRoomConfig); cdecl;
  procedure leaveRoom(listener: JRoomUpdateListener; roomId: JString); cdecl;
  procedure loadAchievements(listener: JOnAchievementsLoadedListener); cdecl; overload;
  procedure loadAchievements(listener: JOnAchievementsLoadedListener; forceReload: Boolean); cdecl; overload;
  procedure loadGame(listener: JOnGamesLoadedListener); cdecl;
  procedure loadInvitablePlayers(listener: JOnPlayersLoadedListener; pageSize: Integer; forceReload: Boolean); cdecl;
  procedure loadInvitations(listener: JOnInvitationsLoadedListener); cdecl;
  procedure loadLeaderboardMetadata(listener: JOnLeaderboardMetadataLoadedListener); cdecl; overload;
  procedure loadLeaderboardMetadata(listener: JOnLeaderboardMetadataLoadedListener; leaderboardId: JString); cdecl; overload;
  procedure loadMoreInvitablePlayers(listener: JOnPlayersLoadedListener; pageSize: Integer); cdecl;
  procedure loadMoreScores(listener: JOnLeaderboardScoresLoadedListener; buffer: JLeaderboardScoreBuffer; maxResults: Integer; pageDirection: Integer); cdecl;
  procedure loadPlayer(listener: JOnPlayersLoadedListener; playerId: JString); cdecl;
  procedure loadPlayerCenteredScores(listener: JOnLeaderboardScoresLoadedListener; leaderboardId: JString; span: Integer; leaderboardCollection: Integer; maxResults: Integer); cdecl; overload;
  procedure loadPlayerCenteredScores(listener: JOnLeaderboardScoresLoadedListener; leaderboardId: JString; span: Integer; leaderboardCollection: Integer; maxResults: Integer; forceReload: Boolean); cdecl; overload;
  procedure loadTopScores(listener: JOnLeaderboardScoresLoadedListener; leaderboardId: JString; span: Integer; leaderboardCollection: Integer; maxResults: Integer); cdecl; overload;
  procedure loadTopScores(listener: JOnLeaderboardScoresLoadedListener; leaderboardId: JString; span: Integer; leaderboardCollection: Integer; maxResults: Integer; forceReload: Boolean); cdecl; overload;
  procedure reconnect; cdecl;
  procedure registerConnectionCallbacks(listener: JGooglePlayServicesClient_ConnectionCallbacks); cdecl;
  procedure registerConnectionFailedListener(listener: JGooglePlayServicesClient_OnConnectionFailedListener); cdecl;
  procedure registerInvitationListener(listener: JOnInvitationReceivedListener); cdecl;
  procedure revealAchievement(id: JString); cdecl;
  procedure revealAchievementImmediate(listener: JOnAchievementUpdatedListener; id: JString); cdecl;
  function sendReliableRealTimeMessage(listener: JRealTimeReliableMessageSentListener; messageData: TJavaArray<Byte>; roomId: JString; recipientParticipantId: JString): Integer; cdecl;
  function sendUnreliableRealTimeMessage(messageData: TJavaArray<Byte>; roomId: JString; recipientParticipantId: JString): Integer; cdecl; overload;
  function sendUnreliableRealTimeMessage(messageData: TJavaArray<Byte>; roomId: JString; recipientParticipantIds: JList): Integer; cdecl; overload;
  function sendUnreliableRealTimeMessageToAll(messageData: TJavaArray<Byte>; roomId: JString): Integer; cdecl;
  procedure setGravityForPopups(gravity: Integer); cdecl;
  procedure setUseNewPlayerNotificationsFirstParty(newPlayerStyle: Boolean); cdecl;
  procedure setViewForPopups(gamesContentView: JView); cdecl;
  procedure signOut; cdecl; overload;
  procedure signOut(listener: Jgames_OnSignOutCompleteListener); cdecl; overload;
  procedure submitScore(leaderboardId: JString; score: Int64); cdecl;
  procedure submitScoreImmediate(listener: JOnScoreSubmittedListener; leaderboardId: JString; score: Int64); cdecl;
  procedure unlockAchievement(id: JString); cdecl;
  procedure unlockAchievementImmediate(listener: JOnAchievementUpdatedListener; id: JString); cdecl;
  procedure unregisterConnectionCallbacks(listener: JGooglePlayServicesClient_ConnectionCallbacks); cdecl;
  procedure unregisterConnectionFailedListener(listener: JGooglePlayServicesClient_OnConnectionFailedListener); cdecl;
  procedure unregisterInvitationListener; cdecl;
end;
TJGamesClient = class(TJavaGenericImport<JGamesClientClass, JGamesClient>) end;

JIProjectionDelegate_aClass = interface(JBinderClass)
['{A17C0831-FD0D-4BCB-9051-C9F2D9E337D2}']
  {Methods}
  function init: JIProjectionDelegate_a; cdecl;
  function I(paramIBinder: JIBinder): JIProjectionDelegate; cdecl;
end;

[JavaSignature('com/google/android/gms/maps/internal/IProjectionDelegate$a')]
JIProjectionDelegate_a = interface(JBinder)
['{3727BFDC-F79D-4758-A177-5C40C7DF358D}']
  {Methods}
  function onTransact(code: Integer; data: JParcel; reply: JParcel; flags: Integer): Boolean; cdecl;
end;
TJIProjectionDelegate_a = class(TJavaGenericImport<JIProjectionDelegate_aClass, JIProjectionDelegate_a>) end;

JTileOverlayClass = interface(JObjectClass)
['{048A54CE-D00A-4604-A37D-AD23D52380E8}']
  {Methods}
  function init(delegate: JObject): JTileOverlay; cdecl;
end;

[JavaSignature('com/google/android/gms/maps/model/TileOverlay')]
JTileOverlay = interface(JObject)
['{46B2D846-DF98-4A3A-9141-280BBB683586}']
  {Methods}
  procedure clearTileCache; cdecl;
  function equals(other: JObject): Boolean; cdecl;
  function getId: JString; cdecl;
  function getZIndex: Single; cdecl;
  function hashCode: Integer; cdecl;
  function isVisible: Boolean; cdecl;
  procedure remove; cdecl;
  procedure setVisible(visible: Boolean); cdecl;
  procedure setZIndex(zIndex: Single); cdecl;
end;
TJTileOverlay = class(TJavaGenericImport<JTileOverlayClass, JTileOverlay>) end;

JPlayerEntityClass = interface(JObjectClass)
['{E7FD712B-5000-4B16-92FB-B33AF573B225}']
  {Property Methods}
  function _GetCREATOR: JPlayerEntityCreator;
  {Methods}
  function init(player: JPlayer): JPlayerEntity; cdecl;
  function a(paramPlayer: JPlayer): Integer; cdecl; overload;
  function a(paramPlayer: JPlayer; paramObject: JObject): Boolean; cdecl; overload;
  function b(paramPlayer: JPlayer): JString; cdecl;
  {Properties}
  property CREATOR: JPlayerEntityCreator read _GetCREATOR;
end;

[JavaSignature('com/google/android/gms/games/PlayerEntity')]
JPlayerEntity = interface(JObject)
['{F8C59508-6EE7-474B-8F15-B641D16841FB}']
  {Methods}
  function F: Integer; cdecl;
  function describeContents: Integer; cdecl;
  function equals(obj: JObject): Boolean; cdecl;
  function freeze: JPlayer; cdecl;
  function getDisplayName: JString; cdecl; overload;
  procedure getDisplayName(dataOut: Jdatabase_CharArrayBuffer); cdecl; overload;
  function getHiResImageUri: Jnet_Uri; cdecl;
  function getIconImageUri: Jnet_Uri; cdecl;
  function getPlayerId: JString; cdecl;
  function getRetrievedTimestamp: Int64; cdecl;
  function hasHiResImage: Boolean; cdecl;
  function hasIconImage: Boolean; cdecl;
  function hashCode: Integer; cdecl;
  function isDataValid: Boolean; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
end;
TJPlayerEntity = class(TJavaGenericImport<JPlayerEntityClass, JPlayerEntity>) end;

JRealTimeMessageReceivedListenerClass = interface(IJavaClass)
['{41C291FA-37A2-4F78-B449-E48116C9DD87}']
end;

[JavaSignature('com/google/android/gms/games/multiplayer/realtime/RealTimeMessageReceivedListener')]
JRealTimeMessageReceivedListener = interface(IJavaInstance)
['{487EE70C-69D6-46F9-935A-8BC45DD124D5}']
  {Methods}
  procedure onRealTimeMessageReceived(paramRealTimeMessage: JRealTimeMessage); cdecl;
end;
TJRealTimeMessageReceivedListener = class(TJavaGenericImport<JRealTimeMessageReceivedListenerClass, JRealTimeMessageReceivedListener>) end;

JGamesActivityResultCodesClass = interface(JObjectClass)
['{418F14A8-CD7D-4B64-9DBC-83742127DE67}']
  {Property Methods}
  function _GetRESULT_APP_MISCONFIGURED: Integer;
  function _GetRESULT_LEFT_ROOM: Integer;
  function _GetRESULT_LICENSE_FAILED: Integer;
  function _GetRESULT_RECONNECT_REQUIRED: Integer;
  function _GetRESULT_SIGN_IN_FAILED: Integer;
  {Methods}
  function init: JGamesActivityResultCodes; cdecl;
  {Properties}
  property RESULT_APP_MISCONFIGURED: Integer read _GetRESULT_APP_MISCONFIGURED;
  property RESULT_LEFT_ROOM: Integer read _GetRESULT_LEFT_ROOM;
  property RESULT_LICENSE_FAILED: Integer read _GetRESULT_LICENSE_FAILED;
  property RESULT_RECONNECT_REQUIRED: Integer read _GetRESULT_RECONNECT_REQUIRED;
  property RESULT_SIGN_IN_FAILED: Integer read _GetRESULT_SIGN_IN_FAILED;
end;

[JavaSignature('com/google/android/gms/games/GamesActivityResultCodes')]
JGamesActivityResultCodes = interface(JObject)
['{1EB938AF-A0DC-4815-9D6A-E8A725450A25}']
end;
TJGamesActivityResultCodes = class(TJavaGenericImport<JGamesActivityResultCodesClass, JGamesActivityResultCodes>) end;

JPerson_ObjectTypeClass = interface(JObjectClass)
['{F7264D9D-1876-4E97-ACA0-A612669722C4}']
  {Property Methods}
  function _GetPAGE: Integer;
  function _GetPERSON: Integer;
  {Methods}
  function init: JPerson_ObjectType; cdecl;
  {Properties}
  property PAGE: Integer read _GetPAGE;
  property PERSON: Integer read _GetPERSON;
end;

[JavaSignature('com/google/android/gms/plus/model/people/Person$ObjectType')]
JPerson_ObjectType = interface(JObject)
['{5286AD08-0594-46C2-91CC-A7BF5AF29B32}']
end;
TJPerson_ObjectType = class(TJavaGenericImport<JPerson_ObjectTypeClass, JPerson_ObjectType>) end;

JLatLngClass = interface(JObjectClass)
['{009A2FD1-2E26-4E50-8D97-FF49B8D3E134}']
  {Property Methods}
  function _GetCREATOR: JLatLngCreator;
  {Methods}
  function init(latitude: Double; longitude: Double): JLatLng; cdecl;
  {Properties}
  property CREATOR: JLatLngCreator read _GetCREATOR;
end;

[JavaSignature('com/google/android/gms/maps/model/LatLng')]
JLatLng = interface(JObject)
['{1612BD16-050A-4E8E-BBFF-1447D11BED15}']
  {Property Methods}
  function _Getlatitude: Double;
  function _Getlongitude: Double;
  {Methods}
  function F: Integer; cdecl;
  function describeContents: Integer; cdecl;
  function equals(o: JObject): Boolean; cdecl;
  function hashCode: Integer; cdecl;
  function toString: JString; cdecl;
  procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
  {Properties}
  property latitude: Double read _Getlatitude;
  property longitude: Double read _Getlongitude;
end;
TJLatLng = class(TJavaGenericImport<JLatLngClass, JLatLng>) end;

JPlayerBufferClass = interface(JDataBufferClass)
['{210CFD2D-7964-4993-9337-FD7CBD357CD1}']
  {Methods}
  function init(dataHolder: JObject): JPlayerBuffer; cdecl;
end;

[JavaSignature('com/google/android/gms/games/PlayerBuffer')]
JPlayerBuffer = interface(JDataBuffer)
['{083864F7-7133-4CA0-B40A-8C8511571054}']
  {Methods}
  function &get(position: Integer): JPlayer; cdecl;
end;
TJPlayerBuffer = class(TJavaGenericImport<JPlayerBufferClass, JPlayerBuffer>) end;

JProjectionClass = interface(JObjectClass)
['{39569102-F14D-4DC9-9FC6-CEA9F4431A79}']
end;

[JavaSignature('com/google/android/gms/maps/Projection')]
JProjection = interface(JObject)
['{6B32610E-3FC7-4956-AD4B-3472F4023DC7}']
  {Methods}
  function fromScreenLocation(point: JPoint): JLatLng; cdecl;
  function getVisibleRegion: JVisibleRegion; cdecl;
  function toScreenLocation(location: JLatLng): JPoint; cdecl;
end;
TJProjection = class(TJavaGenericImport<JProjectionClass, JProjection>) end;

JCover_LayoutClass = interface(JObjectClass)
['{5E0959E7-198F-427F-A926-C97B63E79B11}']
  {Property Methods}
  function _GetBANNER: Integer;
  {Methods}
  function init: JCover_Layout; cdecl;
  {Properties}
  property BANNER: Integer read _GetBANNER;
end;

[JavaSignature('com/google/android/gms/plus/model/people/Person$Cover$Layout')]
JCover_Layout = interface(JObject)
['{E7631A78-F831-4040-ACA8-C44B23E692AC}']
end;
TJCover_Layout = class(TJavaGenericImport<JCover_LayoutClass, JCover_Layout>) end;

JPerson_ImageClass = interface(JFreezableClass)
['{6DB0FFAC-5A6D-4DAB-A867-62C95962B79A}']
end;

[JavaSignature('com/google/android/gms/plus/model/people/Person$Image')]
JPerson_Image = interface(JFreezable)
['{2EAA856D-748D-46BE-91E6-1B6ECF906959}']
  {Methods}
  function getUrl: JString; cdecl;
  function hasUrl: Boolean; cdecl;
end;
TJPerson_Image = class(TJavaGenericImport<JPerson_ImageClass, JPerson_Image>) end;

JIMapFragmentDelegate_aClass = interface(JBinderClass)
['{6313D950-39FD-49EA-BE9E-7103ADC801DD}']
  {Methods}
  function init: JIMapFragmentDelegate_a; cdecl;
  function x(paramIBinder: JIBinder): JIMapFragmentDelegate; cdecl;
end;

[JavaSignature('com/google/android/gms/maps/internal/IMapFragmentDelegate$a')]
JIMapFragmentDelegate_a = interface(JBinder)
['{AE865464-5DA2-47FA-BF15-7E3587F2B879}']
  {Methods}
  function onTransact(code: Integer; data: JParcel; reply: JParcel; flags: Integer): Boolean; cdecl;
end;
TJIMapFragmentDelegate_a = class(TJavaGenericImport<JIMapFragmentDelegate_aClass, JIMapFragmentDelegate_a>) end;

JUrlTileProviderClass = interface(JObjectClass)
['{34E3DD57-5F4F-4236-99BA-3341D5560F82}']
  {Methods}
  function init(width: Integer; height: Integer): JUrlTileProvider; cdecl;
end;

[JavaSignature('com/google/android/gms/maps/model/UrlTileProvider')]
JUrlTileProvider = interface(JObject)
['{34F7AB9C-B88F-46BB-BFD6-8EC61EB9E58B}']
  {Methods}
  function getTile(x: Integer; y: Integer; zoom: Integer): JTile; cdecl;
end;
TJUrlTileProvider = class(TJavaGenericImport<JUrlTileProviderClass, JUrlTileProvider>) end;

JRoomUpdateListenerClass = interface(IJavaClass)
['{E5B9F101-A738-4ABE-8733-ACA0A36DFD70}']
end;

[JavaSignature('com/google/android/gms/games/multiplayer/realtime/RoomUpdateListener')]
JRoomUpdateListener = interface(IJavaInstance)
['{F18EB4D8-5704-4A7D-AD45-1AD615E35C27}']
  {Methods}
  procedure onJoinedRoom(paramInt: Integer; paramRoom: JRoom); cdecl;
  procedure onLeftRoom(paramInt: Integer; paramString: JString); cdecl;
  procedure onRoomConnected(paramInt: Integer; paramRoom: JRoom); cdecl;
  procedure onRoomCreated(paramInt: Integer; paramRoom: JRoom); cdecl;
end;
TJRoomUpdateListener = class(TJavaGenericImport<JRoomUpdateListenerClass, JRoomUpdateListener>) end;




implementation

begin

end.


