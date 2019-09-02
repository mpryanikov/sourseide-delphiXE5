{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{ Copyright(c) 2013 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit FMX.KeyMapping;

interface
uses
  System.UITypes, FMX.Types;

implementation

{$IFDEF ANDROID}
uses
  Androidapi.KeyCodes;
{$ENDIF}

{$IF defined(MACOS) and NOT defined(IOS)}
uses
  Macapi.KeyCodes;
{$ENDIF}

{$IFDEF IOS}
{$ENDIF}

{$IFDEF MSWINDOWS}
{$ENDIF}

procedure AddArray(A: array of Word; Kind: TKeyKind);
var
  I: Integer;
begin
  for I := 0 to (Length(A) - 1) div 2 do
    RegisterKeyMapping(A[I * 2], A[I * 2 + 1], Kind);
end;

{$IFDEF ANDROID}
{$REGION 'Android key map'}
procedure RegisterVirtualKeys;
const
  TrivialKeys: array [0..71] of Word = (
    AKEYCODE_F1,                      vkF1,
    AKEYCODE_F2,                      vkF2,
    AKEYCODE_F3,                      vkF3,
    AKEYCODE_F4,                      vkF4,
    AKEYCODE_F5,                      vkF5,
    AKEYCODE_F6,                      vkF6,
    AKEYCODE_F7,                      vkF7,
    AKEYCODE_F8,                      vkF8,
    AKEYCODE_F9,                      vkF9,
    AKEYCODE_F10,                     vkF10,
    AKEYCODE_F11,                     vkF11,
    AKEYCODE_F12,                     vkF12,
    AKEYCODE_TAB,                     vkTab,
    AKEYCODE_INSERT,                  vkInsert,
    AKEYCODE_FORWARD_DEL,             vkDelete,
    AKEYCODE_MOVE_HOME,               vkHome,
    AKEYCODE_MOVE_END,                vkEnd,
    AKEYCODE_PAGE_UP,                 vkPrior,
    AKEYCODE_PAGE_DOWN,               vkNext,
    AKEYCODE_DPAD_UP,                 vkUp,
    AKEYCODE_DPAD_DOWN,               vkDown,
    AKEYCODE_DPAD_LEFT,               vkLeft,
    AKEYCODE_DPAD_RIGHT,              vkRight,
    AKEYCODE_NUM_LOCK,                vkNumLock,
    AKEYCODE_ENTER,                   vkReturn,
    AKEYCODE_DEL,                     vkBack,
    AKEYCODE_ESCAPE,                  vkEscape,

    AKEYCODE_SCROLL_LOCK,             vkScroll,
    AKEYCODE_CAPS_LOCK,               vkCapital,
    AKEYCODE_CTRL_LEFT,               vkLControl,
    AKEYCODE_CTRL_RIGHT,              vkRControl,
    AKEYCODE_MENU,                    vkMenu,
    AKEYCODE_ALT_LEFT,                vkLMenu,
    AKEYCODE_ALT_RIGHT,               vkRMenu,
    AKEYCODE_SHIFT_LEFT,              vkLShift,
    AKEYCODE_SHIFT_RIGHT,             vkRShift);

  OtherKeys: array [0..29] of Word = (
    AKEYCODE_HOME,                    vkBrowserHome,
    AKEYCODE_BACK,                    vkHardwareBack,
    AKEYCODE_CAMERA,                  vkCamera,
    AKEYCODE_CLEAR,                   vkOemClear,
    AKEYCODE_VOLUME_UP,               vkVolumeUp ,
    AKEYCODE_VOLUME_DOWN,             vkVolumeDown,
    AKEYCODE_MEDIA_PLAY_PAUSE,        vkMediaPlayPause,
    AKEYCODE_MEDIA_STOP,              vkMediaStop,
    AKEYCODE_MEDIA_NEXT,              vkMediaNextTrack,
    AKEYCODE_MEDIA_PREVIOUS,          vkMediaPrevTrack,
    AKEYCODE_CONTACTS,                vkLaunchMail,
    AKEYCODE_CALENDAR,                vkLaunchApp1,
    AKEYCODE_MUSIC,                   vkLaunchMediaSelect,
    AKEYCODE_CALCULATOR,              vkLaunchApp2,
    AKEYCODE_POWER,                   vkSleep);

  TextKeys: array [0..125] of Word = (
    AKEYCODE_0,                       vk0,
    AKEYCODE_1,                       vk1,
    AKEYCODE_2,                       vk2,
    AKEYCODE_3,                       vk3,
    AKEYCODE_4,                       vk4,
    AKEYCODE_5,                       vk5,
    AKEYCODE_6,                       vk6,
    AKEYCODE_7,                       vk7,
    AKEYCODE_8,                       vk8,
    AKEYCODE_9,                       vk9,
    AKEYCODE_A,                       vkA,
    AKEYCODE_B,                       vkB,
    AKEYCODE_C,                       vkC,
    AKEYCODE_D,                       vkD,
    AKEYCODE_E,                       vkE,
    AKEYCODE_F,                       vkF,
    AKEYCODE_G,                       vkG,
    AKEYCODE_H,                       vkH,
    AKEYCODE_I,                       vkI,
    AKEYCODE_J,                       vkJ,
    AKEYCODE_K,                       vkK,
    AKEYCODE_L,                       vkL,
    AKEYCODE_M,                       vkM,
    AKEYCODE_N,                       vkN,
    AKEYCODE_O,                       vkO,
    AKEYCODE_P,                       vkP,
    AKEYCODE_Q,                       vkQ,
    AKEYCODE_R,                       vkR,
    AKEYCODE_S,                       vkS,
    AKEYCODE_T,                       vkT,
    AKEYCODE_U,                       vkU,
    AKEYCODE_V,                       vkV,
    AKEYCODE_W,                       vkW,
    AKEYCODE_X,                       vkX,
    AKEYCODE_Y,                       vkY,
    AKEYCODE_Z,                       vkZ,
    AKEYCODE_APOSTROPHE,              vkQuote,
    AKEYCODE_BACKSLASH,               vkBackslash,
    AKEYCODE_COMMA,                   vkComma,
    AKEYCODE_EQUALS,                  vkEqual,
    AKEYCODE_GRAVE,                   vkTilde,
    AKEYCODE_LEFT_BRACKET,            vkLeftBracket,
    AKEYCODE_PERIOD,                  vkPeriod,
    AKEYCODE_MINUS,                   vkMinus,
    AKEYCODE_RIGHT_BRACKET,           vkRightBracket,
    AKEYCODE_SEMICOLON,               vkSemicolon,
    AKEYCODE_SLASH,                   vkSlash,
    AKEYCODE_NUMPAD_0,                vkNumpad0,
    AKEYCODE_NUMPAD_1,                vkNumpad1,
    AKEYCODE_NUMPAD_2,                vkNumpad2,
    AKEYCODE_NUMPAD_3,                vkNumpad3,
    AKEYCODE_NUMPAD_4,                vkNumpad4,
    AKEYCODE_NUMPAD_5,                vkNumpad5,
    AKEYCODE_NUMPAD_6,                vkNumpad6,
    AKEYCODE_NUMPAD_7,                vkNumpad7,
    AKEYCODE_NUMPAD_8,                vkNumpad8,
    AKEYCODE_NUMPAD_9,                vkNumpad9,
    AKEYCODE_NUMPAD_ADD,              vkAdd,
    AKEYCODE_NUMPAD_DIVIDE,           vkDivide,
    AKEYCODE_NUMPAD_DOT,              vkDecimal,
    AKEYCODE_NUMPAD_MULTIPLY,         vkMultiply,
    AKEYCODE_NUMPAD_SUBTRACT,         vkSubtract,
    AKEYCODE_SPACE,                   vkSpace);

begin
  AddArray(TrivialKeys, TKeyKind.kkFunctional);
  AddArray(OtherKeys, TKeyKind.kkUnknown);
  AddArray(TextKeys, TKeyKind.kkUsual);
end;
{$ENDREGION}
{$ENDIF}

{$IF defined(MACOS) and NOT defined(IOS)}
{$REGION 'OSX key map'}
procedure RegisterVirtualKeys;
const
  TrivialKeys: array [0..71] of Word = (
    KEY_F1        ,     vkF1,
    KEY_F2        ,     vkF2,
    KEY_F3        ,     vkF3,
    KEY_F4        ,     vkF4,
    KEY_F5        ,     vkF5,
    KEY_F6        ,     vkF6,
    KEY_F7        ,     vkF7,
    KEY_F8        ,     vkF8,
    KEY_F9        ,     vkF9,
    KEY_F10       ,     vkF10,
    KEY_F11       ,     vkF11,
    KEY_F12       ,     vkF12,
    KEY_F13       ,     vkF13,
    KEY_F14       ,     vkF14,
    KEY_F15       ,     vkF15,
    KEY_F16       ,     vkF16,
    KEY_F17       ,     vkF17,
    KEY_F18       ,     vkF18,
    KEY_F19       ,     vkF19,
    KEY_F20       ,     vkF20,
    KEY_TAB       ,     vkTab,
    KEY_INS       ,     vkInsert,
    KEY_DEL       ,     vkDelete,
    KEY_HOME      ,     vkHome,
    KEY_END       ,     vkEnd,
    KEY_PAGUP     ,     vkPrior,
    KEY_PAGDN     ,     vkNext,
    KEY_UP        ,     vkUp,
    KEY_DOWN      ,     vkDown,
    KEY_LEFT      ,     vkLeft,
    KEY_RIGHT     ,     vkRight,
    KEY_NUMLOCK   ,     vkNumLock,
    KEY_PADENTER  ,     vkReturn,
    KEY_BACKSPACE ,     vkBack,
    KEY_ENTER     ,     vkReturn,
    KEY_ESC       ,     vkEscape);

  TextKeys: array [0..125] of Word = (
    KEY_NUMPAD0      ,    vkNumpad0,
    KEY_NUMPAD1      ,    vkNumpad1,
    KEY_NUMPAD2      ,    vkNumpad2,
    KEY_NUMPAD3      ,    vkNumpad3,
    KEY_NUMPAD4      ,    vkNumpad4,
    KEY_NUMPAD5      ,    vkNumpad5,
    KEY_NUMPAD6      ,    vkNumpad6,
    KEY_NUMPAD7      ,    vkNumpad7,
    KEY_NUMPAD8      ,    vkNumpad8,
    KEY_NUMPAD9      ,    vkNumpad9,
    KEY_PADDIV       ,    vkDivide,
    KEY_PADMULT      ,    vkMultiply,
    KEY_PADSUB       ,    vkSubtract,
    KEY_PADADD       ,    vkAdd,
    KEY_PADDEC       ,    vkDecimal,
    KEY_SEMICOLON    ,    vkSemicolon,
    KEY_EQUAL        ,    vkEqual,
    KEY_COMMA        ,    vkComma,
    KEY_MINUS        ,    vkMinus,
    KEY_PERIOD       ,    vkPeriod,
    KEY_SLASH        ,    vkSlash,
    KEY_TILDE        ,    vkTilde,
    KEY_LEFTBRACKET  ,    vkLeftBracket,
    KEY_BACKSLASH    ,    vkBackslash,
    KEY_RIGHTBRACKET ,    vkRightBracket,
    KEY_QUOTE        ,    vkQuote,
    KEY_PARA         ,    vkPara,
    KEY_1            ,    vk1,
    KEY_2            ,    vk2,
    KEY_3            ,    vk3,
    KEY_4            ,    vk4,
    KEY_5            ,    vk5,
    KEY_6            ,    vk6,
    KEY_7            ,    vk7,
    KEY_8            ,    vk8,
    KEY_9            ,    vk9,
    KEY_0            ,    vk0,
    KEY_Q            ,    vkQ,
    KEY_W            ,    vkW,
    KEY_E            ,    vkE,
    KEY_R            ,    vkR,
    KEY_T            ,    vkT,
    KEY_Y            ,    vkY,
    KEY_U            ,    vkU,
    KEY_I            ,    vkI,
    KEY_O            ,    vkO,
    KEY_P            ,    vkP,
    KEY_A            ,    vkA,
    KEY_S            ,    vkS,
    KEY_D            ,    vkD,
    KEY_F            ,    vkF,
    KEY_G            ,    vkG,
    KEY_H            ,    vkH,
    KEY_J            ,    vkJ,
    KEY_K            ,    vkK,
    KEY_L            ,    vkL,
    KEY_Z            ,    vkZ,
    KEY_X            ,    vkX,
    KEY_C            ,    vkC,
    KEY_V            ,    vkV,
    KEY_B            ,    vkB,
    KEY_N            ,    vkN,
    KEY_M            ,    vkM);

begin
  AddArray(TrivialKeys, TKeyKind.kkFunctional);
  AddArray(TextKeys, TKeyKind.kkUsual);
end;
{$ENDREGION}
{$ENDIF}

{$IF defined(MACOS) and defined(IOS)}
{$REGION 'IOS key map'}
procedure RegisterVirtualKeys;
begin
  // Currently not used
end;
{$ENDREGION}
{$ENDIF}

{$IFDEF MSWINDOWS}
{$REGION 'Windows key map'}
procedure RegisterVirtualKeys;
begin
  // Currently not used
end;
{$ENDREGION}
{$ENDIF}

initialization
  RegisterVirtualKeys;

end.
