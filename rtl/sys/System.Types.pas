{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit System.Types;

interface

type
  PLongint = System.PLongint;
  {$EXTERNALSYM PLongint}
  PInteger = System.PInteger;
  {$EXTERNALSYM PInteger}
  PSmallInt = System.PSmallInt;
  {$EXTERNALSYM PSmallInt}
  PDouble = System.PDouble;
  {$EXTERNALSYM PDouble}
  PByte = System.PByte;
  {$EXTERNALSYM PByte}

  TIntegerDynArray      = array of Integer;
  {$EXTERNALSYM TIntegerDynArray 'System::TIntegerDynArray'}
  TCardinalDynArray     = array of Cardinal;
  {$EXTERNALSYM TCardinalDynArray 'System::TCardinalDynArray'}
  TWordDynArray         = array of Word;
  {$EXTERNALSYM TWordDynArray 'System::TWordDynArray'}
  TSmallIntDynArray     = array of SmallInt;
  {$EXTERNALSYM TSmallIntDynArray 'System::TSmallIntDynArray'}
  TByteDynArray         = array of Byte;
  {$EXTERNALSYM TByteDynArray 'System::TByteDynArray'}
  TShortIntDynArray     = array of ShortInt;
  {$EXTERNALSYM TShortIntDynArray 'System::TShortIntDynArray'}
  TInt64DynArray        = array of Int64;
  {$EXTERNALSYM TInt64DynArray 'System::TInt64DynArray'}
  TLongWordDynArray     = array of LongWord;
  {$EXTERNALSYM TLongWordDynArray 'System::TLongWordDynArray'}
  TSingleDynArray       = array of Single;
  {$EXTERNALSYM TSingleDynArray 'System::TSingleDynArray'}
  TDoubleDynArray       = array of Double;
  {$EXTERNALSYM TDoubleDynArray 'System::TDoubleDynArray'}
  TBooleanDynArray      = array of Boolean;
  {$EXTERNALSYM TBooleanDynArray 'System::TBooleanDynArray'}
  TStringDynArray       = array of string;
  {$EXTERNALSYM TStringDynArray 'System::TStringDynArray'}
{$IFNDEF NEXTGEN}
  TWideStringDynArray   = array of WideString;
  {$EXTERNALSYM TWideStringDynArray 'System::TWideStringDynArray'}
{$ENDIF !NEXTGEN}

{$IFNDEF NEXTGEN}
  OleStr = WideString;
{$ELSE NEXTGEN}
  OleStr = UnicodeString;
{$ENDIF NEXTGEN}

{ Duplicate management }

  TDuplicates = (dupIgnore, dupAccept, dupError);
  TDirection = (FromBeginning, FromEnd);

  {$NODEFINE TSize}
  {$NODEFINE PSize}

  PSize = ^TSize;
  TSize = record
    cx: Longint;
    cy: Longint;
  public
    constructor Create(P : TSize); overload;
    constructor Create(const X, Y : Integer); overload;
    // operator overloads
    class operator Equal(const Lhs, Rhs : TSize) : Boolean;
    class operator NotEqual(const Lhs, Rhs : TSize): Boolean;
    class operator Add(const Lhs, Rhs : TSize): TSize;
    class operator Subtract(const Lhs, Rhs : TSize): TSize;

    // methods
    function Add(const Point: TSize): TSize;
    function Distance(const P2 : TSize) : Double;
    function IsZero : Boolean;
    function Subtract(const Point: TSize): TSize;

    // properties
    property Width: Integer read cx write cx;
    property Height: Integer read cy write cy;
  end;

  TVectorArray = array [0..2] of Single;

  {$EXTERNALSYM tagSIZE}
  SIZE = TSize;
  tagSize = TSize;
  {$EXTERNALSYM SIZE}

  {$NODEFINE TSmallPoint}
  {$NODEFINE PSmallPoint}

  PSmallPoint = ^TSmallPoint;
  TSmallPoint = record
    x: SmallInt;
    y: SmallInt;
  public
    constructor Create(P : TSmallPoint); overload;
    constructor Create(const X, Y : Word); overload;
    constructor Create(const X, Y : SmallInt); overload;

    // operator overloads
    class operator Equal(const Lhs, Rhs : TSmallPoint) : Boolean;
    class operator NotEqual(const Lhs, Rhs : TSmallPoint): Boolean;
    class operator Add(const Lhs, Rhs : TSmallPoint): TSmallPoint;
    class operator Subtract(const Lhs, Rhs : TSmallPoint): TSmallPoint;

    // methods
    function Add(const Point: TSmallPoint): TSmallPoint;
    function Distance(const P2 : TSmallPoint) : Double;
    function IsZero : Boolean;
    function Subtract(const Point: TSmallPoint): TSmallPoint;
  end;

  PPoint = ^TPoint;
  TPoint = record
    X: Longint;
    Y: Longint;
  public
    constructor Create(P : TPoint); overload;
    constructor Create(const X, Y : Integer); overload;

    //operator overloads
    class operator Equal(const Lhs, Rhs : TPoint) : Boolean;
    class operator NotEqual(const Lhs, Rhs : TPoint): Boolean;
    class operator Add(const Lhs, Rhs : TPoint): TPoint;
    class operator Subtract(const Lhs, Rhs : TPoint): TPoint;
{$IFDEF FALSE}
    class operator Explicit(const Size: TSize): TPoint;
    class operator Explicit(const Point: TPoint): TSize;
    class operator Explicit(const SmallPoint: TSmallPoint): TPoint;
    class operator Explicit(const Point: TPoint): TSmallPoint;
    class operator Implicit(const Size: TSize): TPoint;
    class operator Implicit(const Point: TPoint): TSize;
{$ENDIF}

    class operator Implicit(Value: TSmallPoint): TPoint;
    class operator Explicit(Value: TPoint): TSmallPoint;

    function Distance(const P2 : TPoint) : Double;

    procedure SetLocation(const X, Y : Integer); overload;
    procedure SetLocation(const P : TPoint); overload;
    procedure Offset(const DX, DY : Integer); overload;
    procedure Offset(const Point: TPoint); overload;
    function Add(const Point: TPoint): TPoint;
    function Subtract(const Point: TPoint): TPoint;
    function IsZero : Boolean;
  end;

  {$NODEFINE TPoint}
  tagPOINT = TPoint;
  {$NODEFINE tagPOINT}

type
  TSplitRectType = (
    srLeft,
    srRight,
    srTop,
    srBottom
  );

  PRect = ^TRect;
  TRect = record
  private
    function GetWidth: Integer;
    procedure SetWidth(const Value: Integer);
    function GetHeight: Integer;
    procedure SetHeight(const Value: Integer);
    function GetSize: TSize;
    procedure SetSize(const Value: TSize);
    function GetLocation: TPoint;
  public
    constructor Create(const Origin: TPoint); overload;                              // empty rect at given origin
    constructor Create(const Origin: TPoint; Width, Height: Integer); overload;      // at TPoint of origin with width and height
    constructor Create(const Left, Top, Right, Bottom: Integer); overload;           // at x, y with width and height
    constructor Create(const P1, P2: TPoint; Normalize: Boolean = False); overload;  // with corners specified by p1 and p2
    constructor Create(const R: TRect; Normalize: Boolean = False); overload;

    // operator overloads
    class operator Equal(const Lhs, Rhs: TRect): Boolean;
    class operator NotEqual(const Lhs, Rhs: TRect): Boolean;

    // union of two rectangles
    class operator Add(const Lhs, Rhs: TRect): TRect;

    // intersection of two rectangles
    class operator Multiply(const Lhs, Rhs: TRect): TRect;

    class function Empty: TRect; inline; static;

    //utility methods
    //makes sure TopLeft is above and to the left of BottomRight
    procedure NormalizeRect;

    //returns true if left = right or top = bottom
    function IsEmpty: Boolean;

    //returns true if the point is inside the rect
    function Contains(const Pt: TPoint): Boolean; overload;

    // returns true if the rect encloses R completely
    function Contains(const R: TRect): Boolean; overload;

    // returns true if any part of the rect covers R
    function IntersectsWith(const R: TRect): Boolean;

    // computes an intersection of R1 and R2
    class function Intersect(const R1: TRect; const R2: TRect): TRect; overload; static;

    // replaces current rectangle with its intersection with R
    procedure Intersect(const R: TRect); overload;

    // computes a union of R1 and R2
    class function Union(const R1: TRect; const R2: TRect): TRect; overload; static;

    // replaces current rectangle with its union with R
    procedure Union(const R: TRect); overload;

    // creates a minimal rectangle that contains all points from array Points
    class function Union(const Points: Array of TPoint): TRect; overload; static;

    // offsets the rectangle origin relative to current position
    procedure Offset(const DX, DY: Integer); overload;
    procedure Offset(const Point: TPoint); overload;

    // sets new origin
    procedure SetLocation(const X, Y: Integer); overload;
    procedure SetLocation(const Point: TPoint); overload;

    // inflate by DX and DY
    procedure Inflate(const DX, DY: Integer); overload;

    // inflate in all directions
    procedure Inflate(const DL, DT, DR, DB: Integer); overload;

    //returns the center point of the rectangle;
    function CenterPoint: TPoint;

    function SplitRect(SplitType: TSplitRectType; Size: Integer): TRect; overload;
    function SplitRect(SplitType: TSplitRectType; Percent: Double): TRect; overload;

    // changing the width is always relative to Left;
    property Width: Integer read GetWidth write SetWidth;
    // changing the Height is always relative to Top
    property Height: Integer read GetHeight write SetHeight;

    property Size: TSize read GetSize write SetSize;

    property Location: TPoint read GetLocation write SetLocation;

  case Integer of
    0: (Left, Top, Right, Bottom: Longint);
    1: (TopLeft, BottomRight: TPoint);
  end;
  {$NODEFINE TRect}

  TPointFType = array [0..1] of Single;

  PPointF = ^TPointF;
  TPointF = record
    class function Create(const AX, AY: Single): TPointF; overload; static; inline;
    class function Create(const APoint: TPoint): TPointF; overload; static; inline;

    class operator Add(const APoint1, APoint2: TPointF): TPointF;
    class operator Subtract(const APoint1, APoint2: TPointF): TPointF;
    class operator Equal(const APoint1, APoint2: TPointF): Boolean;
    class operator NotEqual(const APoint1, APoint2: TPointF): Boolean;
    class operator Implicit(const APoint: TPoint): TPointF;
    class operator Negative(const APoint: TPointF): TPointF;
    class operator Multiply(const APoint1, APoint2: TPointF): TPointF;
    class operator Multiply(const APoint: TPointF; const AFactor: Single): TPointF;
    class operator Multiply(const AFactor: Single; const APoint: TPointF): TPointF;
    class operator Divide(const APoint: TPointF; const AFactor: Single): TPointF;

    function Distance(const APoint: TPointF): Single;
    // 3D cross-product with Z = 0
    function CrossProduct(const APoint: TPointF): Single;
    function DotProduct(const APoint: TPointF): Single; inline;

    procedure Offset(const APoint: TPointF); overload; inline;
    procedure Offset(const ADeltaX, ADeltaY: Single); overload; inline;
    procedure Offset(const APoint: TPoint); overload; inline;

    procedure SetLocation(const X, Y: Single); overload; deprecated 'Use ":=" assignment instead';
    procedure SetLocation(const P: TPointF); overload; deprecated 'Use ":=" assignment instead';
    procedure SetLocation(const P: TPoint); overload; deprecated 'Use ":=" assignment instead';
    function Subtract(const Point: TPointF): TPointF; overload; deprecated 'Use TPointF.Offset instead';
    function Subtract(const Point: TPoint): TPointF; overload; deprecated 'Use TPointF.Offset instead';
    function Add(const Point: TPointF): TPointF; overload; deprecated 'Use TPointF.Offset instead';
    function Add(const Point: TPoint): TPointF; overload; deprecated 'Use TPointF.Offset instead';
    function Scale(const AFactor: Single): TPointF; deprecated;

    function IsZero: Boolean;
    function Ceiling: TPoint;
    function Truncate: TPoint;
    function Round: TPoint;

    function Normalize: TPointF;
    function Length: Single;
    function Rotate(const AAngle: Single): TPointF; inline;
    function Reflect(const APoint: TPointF): TPointF; inline;
    function MidPoint(const APoint: TPointF): TPointF; inline;
    function AngleCosine(const APoint: TPointF): Single;

    case Integer of
      0: (V: TPointFType;);
      1: (X: Single;
          Y: Single;);
  end;

  TPolygon = array of TPointF;

  TCubicBezier = array [0..3] of TPointF;

  {$NODEFINE TPointF}
  tagPOINTF = TPointF;
  {$NODEFINE tagPOINTF}

  PSizeF = ^TSizeF;
  TSizeF = record
    cx: Single;
    cy: Single;
  public
    constructor Create(P: TSizeF); overload;
    constructor Create(const X, Y: Single); overload;
    // operator overloads
    class operator Equal(const Lhs, Rhs: TSizeF): Boolean;
    class operator NotEqual(const Lhs, Rhs: TSizeF): Boolean;
    class operator Add(const Lhs, Rhs: TSizeF): TSizeF;
    class operator Subtract(const Lhs, Rhs: TSizeF): TSizeF;

    class operator Implicit(const Size: TSizeF): TPointF;
    class operator Implicit(const Point: TPointF): TSizeF;
    class operator Implicit(const Size: TSize): TSizeF;

    function Ceiling: TSize;
    function Truncate: TSize;
    function Round: TSize;

    // metods
    function Add(const Point: TSizeF): TSizeF;
    function Subtract(const Point: TSizeF): TSizeF;
    function Distance(const P2: TSizeF): Double;
    function IsZero: Boolean;
    // properties
    property Width: Single read cx write cx;
    property Height: Single read cy write cy;
  end;

  PRectF = ^TRectF;
  TRectF = record
  private
    function GetWidth: Single;
    procedure SetWidth(const Value: Single);
    function GetHeight: Single;
    procedure SetHeight(const Value: Single);
    function GetSize: TSizeF;
    procedure SetSize(const Value: TSizeF);
    function GetLocation: TPointF;
  public
    constructor Create(const Origin: TPointF); overload;                               // empty rect at given origin
    constructor Create(const Origin: TPointF; const Width, Height: Single); overload; // at TPoint of origin with width and height
    constructor Create(const Left, Top, Right, Bottom: Single); overload;              // at x, y with width and height
    constructor Create(const P1, P2: TPointF; Normalize: Boolean = False); overload;  // with corners specified by p1 and p2
    constructor Create(const R: TRectF; Normalize: Boolean = False); overload;
    constructor Create(const R: TRect; Normalize: Boolean = False); overload;

    // operator overloads
    class operator Equal(const Lhs, Rhs: TRectF): Boolean;
    class operator NotEqual(const Lhs, Rhs: TRectF): Boolean;

    // union of two rectangles
    class operator Add(const Lhs, Rhs: TRectF): TRectF;

    // intersection of two rectangles
    class operator Multiply(const Lhs, Rhs: TRectF): TRectF;

    class function Empty: TRectF; inline; static;

    //utility methods

    // Current rect enters into the boundaries of the rectangle with keeping
    // aspect ratio and return ratio's value
    function Fit(BoundsRect: TRectF): Single;

    //makes sure TopLeft is above and to the left of BottomRight
    procedure NormalizeRect;

    //returns true if left = right or top = bottom
    function IsEmpty: Boolean;

    //returns true if the point is inside the rect
    function Contains(const Pt: TPointF): Boolean; overload;

    // returns true if the rect encloses R completely
    function Contains(const R: TRectF): Boolean; overload;

    // returns true if any part of the rect covers R
    function IntersectsWith(const R: TRectF): Boolean;

    // computes an intersection of R1 and R2
    class function Intersect(const R1: TRectF; const R2: TRectF): TRectF; overload; static;

    // replaces current rectangle with its intersection with R
    procedure Intersect(const R: TRectF); overload;

    // computes a union of R1 and R2
    class function Union(const R1: TRectF; const R2: TRectF): TRectF; overload; static;

    // replaces current rectangle with its union with R
    procedure Union(const R: TRectF); overload;

    // creates a minimal rectangle that contains all points from array Points
    class function Union(const Points: Array of TPointF): TRectF; overload; static;

    // offsets the rectangle origin relative to current position
    procedure Offset(const DX, DY: Single); overload;
    procedure Offset(const Point: TPointF); overload;

    // sets new origin
    procedure SetLocation(const X, Y: Single); overload;
    procedure SetLocation(const Point: TPointF); overload;

    // inflate by DX and DY
    procedure Inflate(const DX, DY: Single); overload;

    // inflate in all directions
    procedure Inflate(const DL, DT, DR, DB: Single); overload;

    //returns the center point of the rectangle;
    function CenterPoint: TPointF;

    function Ceiling: TRect;
    function Truncate: TRect;
    function Round: TRect;

    {
    function SplitRect(SplitType: TSplitRectType; Size: Integer): TRect; overload;
    function SplitRect(SplitType: TSplitRectType; Percent: Double): TRect; overload;
    }

    // changing the width is always relative to Left;
    property Width: Single read GetWidth write SetWidth;
    // changing the Height is always relative to Top
    property Height: Single read GetHeight write SetHeight;

    property Size: TSizeF read GetSize write SetSize;

    property Location: TPointF read GetLocation write SetLocation;

  case Integer of
    0: (Left, Top, Right, Bottom: Single);
    1: (TopLeft, BottomRight: TPointF);
  end;
  {$NODEFINE TPointF}
  {$NODEFINE TRectF}
  {$NODEFINE TSizeF}
  
  tagVECTOR = record
    case Integer of
      0: (V: TVectorArray;);
      1: (X: Single;
          Y: Single;
          W: Single;);
  end;
  
  TVector = record
    class function Create(const AX, AY, AW: Single): TVector; overload; static; inline;
    class function Create(const APoint: TPointF): TVector; overload; static; inline;

    class operator Add(const AVector1, AVector2: TVector): TVector;
    class operator Subtract(const AVector1, AVector2: TVector): TVector;
    class operator Equal(const AVector1, AVector2: TVector): Boolean;
    class operator NotEqual(const AVector1, AVector2: TVector): Boolean;
    class operator Implicit(const APoint: TPointF): TVector;
    class operator Implicit(const ASize: TSizeF): TVector;
    class operator Multiply(const AVector: TVector; const AFactor: Single): TVector;
    class operator Multiply(const AFactor: Single; const AVector: TVector): TVector;
    class operator Divide(const AVector: TVector; const AFactor: Single): TVector;

    procedure Offset(const ADelta: TPointF); overload; inline;
    procedure Offset(const ADeltaX, ADeltaY: Single); overload; inline;

    // 3D cross-product with Z = 0
    function CrossProduct(const AVector: TVector): Single;
    function DotProduct(const AVector: TVector): Single; inline;

    function Length: Single;
    function Normalize: TVector;
    function Rotate(const AAngle: Single): TVector; inline;
    function Reflect(const AVector: TVector): TVector; inline;
    function MidVector(const AVector: TVector): TVector; inline;
    function AngleCosine(const AVector: TVector): Single;

    function ToPointF: TPointF;

    case Integer of
      0: (V: TVectorArray;);
      1: (X: Single;
          Y: Single;
          W: Single;);
  end;

  TMatrixArray = array [0..2] of TVector;
  {$NODEFINE TMatrixArray 'System::Types::TMatrixArray' 'System::Types::TMaxtrixArrayBase'}
  (*$HPPEMIT END OPENNAMESPACE*)
  (*$HPPEMIT END 'typedef TVector TMatrixArray[3];'*)
  (*$HPPEMIT END CLOSENAMESPACE*)
  TMaxtrixArrayBase = array[0..2] of tagVECTOR;

  TMatrix = record
  private
    function Scale(const AFactor: Single): TMatrix;
  public
    class function CreateRotation(const AAngle: Single): TMatrix; static;
    class function CreateScaling(const AScaleX, AScaleY: Single): TMatrix; static;
    class function CreateTranslation(const ADeltaX, ADeltaY: Single): TMatrix; static;

    class operator Multiply(const AMatrix1, AMatrix2: TMatrix): TMatrix;
    class operator Multiply(const APoint: TPointF; const AMatrix: TMatrix): TPointF;
    class operator Multiply(const AVector: TVector; const AMatrix: TMatrix): TVector;

    function Determinant: Single;
    function Adjoint: TMatrix;
    function Inverse: TMatrix;

    case Integer of
      0: (M: TMatrixArray;);
      1: (m11, m12, m13: Single;
          m21, m22, m23: Single;
          m31, m32, m33: Single);
  end;

  TMatrixConstants = record helper for TMatrix
    const Identity: TMatrix = (m11: 1; m12: 0; m13: 0; m21: 0; m22: 1; m23: 0; m31: 0; m32: 0; m33: 1);
  end;

  TPoint3DType = array [0..2] of Single;
  TVector3DType = array [0..3] of Single;

  TPoint3D = record
    class function Create(const AX, AY, AZ: Single): TPoint3D; overload; static; inline;
    class function Create(const P: TVector3DType): TPoint3D; overload; static; inline;
    class function Create(const APoint: TPointF; const AZ: Single = 0.0): TPoint3D; overload; static; inline;

    class operator Add(const APoint1, APoint2: TPoint3D): TPoint3D;
    class operator Subtract(const APoint1, APoint2: TPoint3D): TPoint3D;
    class operator Equal(const APoint1, APoint2: TPoint3D): Boolean;
    class operator NotEqual(const APoint1, APoint2: TPoint3D): Boolean;
    class operator Negative(const APoint: TPoint3D): TPoint3D;
    class operator Multiply(const APoint1, APoint2: TPoint3D): TPoint3D;
    class operator Multiply(const APoint: TPoint3D; const AFactor: Single): TPoint3D;
    class operator Multiply(const AFactor: Single; const APoint: TPoint3D): TPoint3D;
    class operator Divide(const APoint: TPoint3D; const AFactor: Single): TPoint3D;

    procedure Offset(const ADelta: TPoint3D); overload; inline;
    procedure Offset(const ADeltaX, ADeltaY, ADeltaZ: Single); overload; inline;

    function CrossProduct(const APoint: TPoint3D): TPoint3D;
    function DotProduct(const APoint: TPoint3D): Single; inline;

    function Length: Single;
    function Normalize: TPoint3D;
    function Distance(const APoint: TPoint3D): Single;
    function Rotate(const AAxis: TPoint3D; const AAngle: Single): TPoint3D; inline;
    function Reflect(const APoint: TPoint3D): TPoint3D; inline;
    function MidPoint(const APoint: TPoint3D): TPoint3D; inline;
    function AngleCosine(const APoint: TPoint3D): Single;

    case Integer of
      0: (V: TPoint3DType;);
      1: (X: Single;
          Y: Single;
          Z: Single;);
  end;
  PPoint3D = ^TPoint3D;

  tagVECTOR3D = record
    case Integer of
      0: (V: TVector3DType;);
      1: (X: Single;
          Y: Single;
          Z: Single;
          W: Single;);
  end;

  TVector3D = record
    class function Create(const AX, AY, AZ: Single; const AW: Single = 1.0): TVector3D; overload; static; inline;
    class function Create(const APoint: TPoint3D; const AW: Single = 1.0): TVector3D; overload; static; inline;

    class operator Add(const AVector1, AVector2: TVector3D): TVector3D;
    class operator Subtract(const AVector1, AVector2: TVector3D): TVector3D;
    class operator Equal(const AVector1, AVector2: TVector3D): Boolean;
    class operator NotEqual(const AVector1, AVector2: TVector3D): Boolean;
    class operator Negative(const AVector: TVector3D): TVector3D;
    class operator Implicit(const APoint: TPoint3D): TVector3D;
    class operator Multiply(const AVector1, AVector2: TVector3D): TVector3D;
    class operator Multiply(const AVector: TVector3D; const AFactor: Single): TVector3D;
    class operator Multiply(const AFactor: Single; const AVector: TVector3D): TVector3D;
    class operator Divide(const AVector: TVector3D; const AFactor: Single): TVector3D;

    procedure Offset(const ADelta: TPoint3D); overload; inline;
    procedure Offset(const ADeltaX, ADeltaY, ADeltaZ: Single); overload; inline;

    function CrossProduct(const AVector: TVector3D): TVector3D;
    function DotProduct(const AVector: TVector3D): Single; inline;

    function Length: Single;
    function Normalize: TVector3D;
    function Distance(const AVector: TVector3D): Single;
    function Rotate(const AAxis: TPoint3D; const AAngle: Single): TVector3D; inline;
    function Reflect(const AVector: TVector3D): TVector3D; inline;
    function MidVector(const AVector: TVector3D): TVector3D; inline;
    function AngleCosine(const AVector: TVector3D): Single;

    // converts 4D (3D + W) vector into 3D vector (when ATransform is True, divides by W)
    function ToPoint3D(const ATransform: Boolean = False): TPoint3D;

    case Integer of
      0: (V: TVector3DType;);
      1: (X: Single;
          Y: Single;
          Z: Single;
          W: Single;);
  end;

  TVector3DArray = array [0..2] of TVector3D;
  {$NODEFINE TVector3DArray 'System::Types::TVector3DArray' 'System::Types::TVector3DArrayBase'}
  (*$HPPEMIT END OPENNAMESPACE*)
  (*$HPPEMIT END 'typedef TVector3D TVector3DArray[3];'*)
  (*$HPPEMIT END CLOSENAMESPACE*)
  TVector3DArrayBase = array[0..2] of tagVECTOR3D;

  TMatrix3DType = array [0..3] of TVector3D;
  {$NODEFINE TMatrix3DType 'System::Types::TMatrix3DType' 'System::Types::TMatrix3DTypeBase'}
  (*$HPPEMIT END OPENNAMESPACE*)
  (*$HPPEMIT END 'typedef TVector3D TMatrix3DType[3];'*)
  (*$HPPEMIT END CLOSENAMESPACE*)
  TMatrix3DTypeBase = array[0..3] of tagVECTOR3D;

  TMatrix3D = record
  private
    function DetInternal(const a1, a2, a3, b1, b2, b3, c1, c2, c3: Single): Single; inline;
    function Scale(const AFactor: Single): TMatrix3D;
  public
    // Creation of matrix by specifying individual values.
    constructor Create(const AM11, AM12, AM13, AM14, AM21, AM22, AM23, AM24, AM31, AM32, AM33,
      AM34, AM41, AM42, AM43, AM44: Single); overload;
    constructor Create(const AArray: TSingleDynArray); overload;

    // Creation of simple transformation matrices.
    class function CreateScaling(const AScale: TPoint3D): TMatrix3D; static;
    class function CreateTranslation(const ATranslation: TPoint3D): TMatrix3D; static;

    // Creation of rotation matrix around only one of the axes (work faster than composite variants)
    class function CreateRotationX(const AAngle: Single): TMatrix3D; static;
    class function CreateRotationY(const AAngle: Single): TMatrix3D; static;
    class function CreateRotationZ(const AAngle: Single): TMatrix3D; static;

    // Creation of a composite rotation matrix (slower but more flexible than simple variants)
    class function CreateRotation(const AAxis: TPoint3D; const AAngle: Single): TMatrix3D; static;
    class function CreateRotationYawPitchRoll(const AYaw, APitch, ARoll: Single): TMatrix3D; static;
    class function CreateRotationHeadingPitchBank(const AHeading, APitch, ABank: Single): TMatrix3D; static;

    // Creation of view/camera matrices.
    class function CreateLookAtRH(const ASource, ATarget, ACeiling: TPoint3D): TMatrix3D; static;
    class function CreateLookAtLH(const ASource, ATarget, ACeiling: TPoint3D): TMatrix3D; static;
    class function CreateLookAtDirRH(const ASource, ADirection, ACeiling: TPoint3D): TMatrix3D; static;
    class function CreateLookAtDirLH(const ASource, ADirection, ACeiling: TPoint3D): TMatrix3D; static;
    class function CreateOrthoLH(const AWidth, AHeight, AZNear, AZFar: Single): TMatrix3D; static;
    class function CreateOrthoRH(const AWidth, AHeight, AZNear, AZFar: Single): TMatrix3D; static;
    class function CreateOrthoOffCenterLH(const ALeft, ATop, ARight, ABottom, AZNear, AZFar: Single): TMatrix3D; static;
    class function CreateOrthoOffCenterRH(const ALeft, ATop, ARight, ABottom, AZNear, AZFar: Single): TMatrix3D; static;
    class function CreatePerspectiveFovLH(const AFOV, AAspect, AZNear, AZFar: Single;
     const AHorizontalFOV: Boolean = False): TMatrix3D; static;
    class function CreatePerspectiveFovRH(const AFOV, AAspect, AZNear, AZFar: Single;
     const AHorizontalFOV: Boolean = False): TMatrix3D; static;

    // multiplication of two 3D matrices
    class operator Multiply(const APoint1, APoint2: TMatrix3D): TMatrix3D;
    // multiplication of 3D vector and matrix
    class operator Multiply(const APoint: TPoint3D; const AMatrix: TMatrix3D): TPoint3D;
    // multiplication of 4D(3D + w) vector and 3D (4x4) matrix
    class operator Multiply(const AVector: TVector3D; const AMatrix: TMatrix3D): TVector3D;

    function Transpose: TMatrix3D;
    function Determinant: Single;
    function Adjoint: TMatrix3D;
    function Inverse: TMatrix3D;

    // calculates eye position from 3D matrix
    function EyePosition: TPoint3D;

    case Integer of
      0: (M: TMatrix3DType;);
      1: (m11, m12, m13, m14: Single;
          m21, m22, m23, m24: Single;
          m31, m32, m33, m34: Single;
          m41, m42, m43, m44: Single);
  end;

  TMatrix3DConstants = record helper for TMatrix3D
    const Identity: TMatrix3D = (m11: 1; m12: 0; m13: 0; m14: 0; m21: 0; m22: 1; m23: 0; m24: 0; m31: 0; m32: 0;
      m33: 1; m34: 0; m41: 0; m42: 0; m43: 0; m44: 1;);
  end;

  TQuaternion3D = record
    constructor Create(const AAxis: TPoint3D; const AAngle: Single); overload;
    constructor Create(const AYaw, APitch, ARoll: Single); overload;
    constructor Create(const AMatrix: TMatrix3D); overload;

    class operator Implicit(const AQuaternion: TQuaternion3D): TMatrix3D;
    class operator Multiply(const AQuaternion1, AQuaternion2: TQuaternion3D): TQuaternion3D;

    // calculates quaternion magnitude
    function Length: Single;
    function Normalize: TQuaternion3D;

    case Integer of
      0: (V: TVector3DType;);
      1: (ImagPart: TPoint3D;
          RealPart: Single;);
  end;

  TQuaternion3DConstants = record helper for TQuaternion3D
    const Identity: TQuaternion3D = (ImagPart: (X: 0; Y: 0; Z: 0); RealPart: 1);
  end;

  (*$HPPEMIT '#include <SystemTypes.h>'*)

  (*$HPPEMIT END OPENNAMESPACE*)
  (*$HPPEMIT END '  inline bool TRect::IntersectRect(const TRect &R1, const TRect &R2)'*)
  (*$HPPEMIT END '  {'*)
  (*$HPPEMIT END '    return Types::IntersectRect(*this, R1, R2) != 0;'*)
  (*$HPPEMIT END '  }'*)
  (*$HPPEMIT END ''*)
  (*$HPPEMIT END '  inline TRect TRect::Intersect(const TRect &r1, const TRect &r2) {'*)
  (*$HPPEMIT END '    TRect result;'*)
  (*$HPPEMIT END '    Types::IntersectRect(result, r1, r2);'*)
  (*$HPPEMIT END '    return result;'*)
  (*$HPPEMIT END '  }'*)
  (*$HPPEMIT END ''*)
  (*$HPPEMIT END '  inline void TRect::Intersect(const TRect &r) {'*)
  (*$HPPEMIT END '    Types::IntersectRect(*this, *this, r);'*)
  (*$HPPEMIT END '  }'*)
  (*$HPPEMIT END ''*)
  (*$HPPEMIT END '  inline bool TRect::UnionRect(const TRect &R1, const TRect &R2)'*)
  (*$HPPEMIT END '  {'*)
  (*$HPPEMIT END '    return Types::UnionRect(*this, R1, R2) != 0;'*)
  (*$HPPEMIT END '  }'*)
  (*$HPPEMIT END ''*)
  (*$HPPEMIT END '  inline TRect TRect::Union(const TRect &r1, const TRect& r2) {'*)
  (*$HPPEMIT END '    TRect result;'*)
  (*$HPPEMIT END '    Types::UnionRect(result, r1, r2);'*)
  (*$HPPEMIT END '    return result;'*)
  (*$HPPEMIT END '  }'*)
  (*$HPPEMIT END ''*)
  (*$HPPEMIT END '  inline void TRect::Union(const TRect &r) {'*)
  (*$HPPEMIT END '    Types::UnionRect(*this, *this, r);'*)
  (*$HPPEMIT END '  }'*)
  (*$HPPEMIT END ''*)
  (*$HPPEMIT END '  inline TRectF TRectF::Intersect(const TRectF &r1, const TRectF &r2) {'*)
  (*$HPPEMIT END '    TRectF result;'*)
  (*$HPPEMIT END '    Types::IntersectRectF(result, r1, r2);'*)
  (*$HPPEMIT END '    return result;'*)
  (*$HPPEMIT END '  }'*)
  (*$HPPEMIT END ''*)
  (*$HPPEMIT END '  inline void TRectF::Intersect(const TRectF &r) {'*)
  (*$HPPEMIT END '    Types::IntersectRectF(*this, *this, r);'*)
  (*$HPPEMIT END '  }'*)
  (*$HPPEMIT END ''*)
  (*$HPPEMIT END '  inline TRectF TRectF::Union(const TRectF &r1, const TRectF &r2) {'*)
  (*$HPPEMIT END '    TRectF result;'*)
  (*$HPPEMIT END '    Types::UnionRectF(result, r1, r2);'*)
  (*$HPPEMIT END '    return result;'*)
  (*$HPPEMIT END '  }'*)
  (*$HPPEMIT END ''*)
  (*$HPPEMIT END '  inline void TRectF::Union(const TRectF &r) {'*)
  (*$HPPEMIT END '    Types::UnionRectF(*this, *this, r);'*)
  (*$HPPEMIT END '  }'*)
  (*$HPPEMIT END ''*)
  (*$HPPEMIT END ''*)
  (*$HPPEMIT END CLOSENAMESPACE*)

  //NOTE: DWORD should really be CppULongInt
  DWORD = LongWord;
  {$EXTERNALSYM DWORD}
const
  RT_RCDATA       = PChar(10);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM RT_RCDATA}
{$ENDIF}  

  NullChar = #0;
  Tabulator = #9;
  Space = #32;
  CarriageReturn = #$D;
  LineFeed = #$A;
  VerticalTab = #$B;
  FormFeed = #$C;
  LineSeparator = #$2028;
  ParagraphSeparator = #$2029;

  BOM_LSB_FIRST = #$FEFF;
  BOM_MSB_FIRST = #$FFFE;

  GUID_NULL: TGUID = '{00000000-0000-0000-0000-000000000000}';

  cPI: Single = 3.141592654;
  {$EXTERNALSYM cPI}  // Moved out of HPP - PCH does not support float constants
  {$HPPEMIT 'extern const System::Single cPI /*= 3.141592654*/;'}
  cPIdiv180: Single = 0.017453292;
  {$EXTERNALSYM cPIdiv180}
  {$HPPEMIT 'extern const System::Single cPIdiv180 /*= 0.017453292*/;'}
  c180divPI: Single = 57.29577951;
  {$EXTERNALSYM c180divPI}
  {$HPPEMIT 'extern const System::Single c180divPI /*= 57.29577951*/;'}
  c2PI: Single = 6.283185307;
  {$EXTERNALSYM c2PI}
  {$HPPEMIT 'extern const System::Single c2PI /*= 6.283185307*/;'}
  cPIdiv2: Single = 1.570796326;
  {$EXTERNALSYM cPIdiv2}
  {$HPPEMIT 'extern const System::Single cPIdiv2 /*= 1.570796326*/;'}
  cPIdiv4: Single = 0.785398163;
  {$EXTERNALSYM cPIdiv4}
  {$HPPEMIT 'extern const System::Single cPIdiv4 /*= 0.785398163*/;'}
  c3PIdiv4: Single = 2.35619449;
  {$EXTERNALSYM c3PIdiv4}
  {$HPPEMIT 'extern const System::Single c3PIdiv4 /*= 2.35619449*/;'}
  cInv2PI: Single = 1 / 6.283185307;
  {$EXTERNALSYM cInv2PI}
  {$HPPEMIT 'extern const System::Single cInv2PI /*= 1 / 6.283185307*/;'}
  cInv360: Single = 1 / 360;
  {$EXTERNALSYM cInv360}
  {$HPPEMIT 'extern const System::Single cInv360 /*= 1 / 360*/;'}
  c180: Single = 180;
  {$EXTERNALSYM c180}
  {$HPPEMIT 'extern const System::Single c180 /*= 180*/;'}
  c360: Single = 360;
  {$EXTERNALSYM c360}
  {$HPPEMIT 'extern const System::Single c360 /*= 360*/;'}
  cOneHalf: Single = 0.5;
  {$EXTERNALSYM cOneHalf}
  {$HPPEMIT 'extern const System::Single cOneHalf /*= 0.5*/;'}

  CurveKappa = 0.5522847498;
  {$EXTERNALSYM CurveKappa}
  {$HPPEMIT 'extern const System::Extended CurveKappa /*= 0.5522847498*/;'}
  CurveKappaInv = 1 - CurveKappa;
  {$EXTERNALSYM CurveKappaInv}
  {$HPPEMIT 'extern const System::Extended CurveKappaInv /*= 1 - CurveKappa*/;'}

  Epsilon: Single = 1E-40;
  {$EXTERNALSYM Epsilon}
  {$HPPEMIT 'extern const System::Single Epsilon /*= 1E-40*/;'}
  Epsilon2: Single = 1E-30;
  {$EXTERNALSYM Epsilon2}
  {$HPPEMIT 'extern const System::Single Epsilon2 /*= 1E-40*/;'}

{$IFDEF MSWINDOWS}
  {$EXTERNALSYM GUID_NULL}
{$ENDIF}
{$IF not Defined(MSWINDOWS) or Defined(NEXTGEN)}
type
  PDisplay = Pointer;
  PEvent = Pointer;
  TXrmOptionDescRec = record end;
  XrmOptionDescRec = TXrmOptionDescRec;
  PXrmOptionDescRec = ^TXrmOptionDescRec;
  Widget = Pointer;
  WidgetClass = Pointer;
  ArgList = Pointer;
  Region = Pointer;
  
 {$IFDEF MACOS}
   EventHandlerCallRef = Pointer;
   EventRef = Pointer;
   CGImageRef = Pointer;
   RgnHandle = Pointer;
   HIShapeRef = Pointer;
   HIMutableShapeRef = Pointer;
   OSMenuRef = Pointer;
 {$ENDIF MACOS}

const
  STGTY_STORAGE   = 1;
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STGTY_STORAGE}
{$ENDIF}  
  STGTY_STREAM    = 2;
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STGTY_STREAM}
{$ENDIF}
  STGTY_LOCKBYTES = 3;
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STGTY_LOCKBYTES}
{$ENDIF}  
  STGTY_PROPERTY  = 4;
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STGTY_PROPERTY}
{$ENDIF}

  STREAM_SEEK_SET = 0;
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STREAM_SEEK_SET}
{$ENDIF}  
  STREAM_SEEK_CUR = 1;
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STREAM_SEEK_CUR}
{$ENDIF}
  STREAM_SEEK_END = 2;
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STREAM_SEEK_END}
{$ENDIF}  

// On Android fcntl.h file there is a group of LOCK_* macro definitions. One of
// them is also called LOCK_WRITE, which produces error with the defined here.
  LOCK_WRITE     = 1;
{$IF defined(MSWINDOWS) or defined(ANDROID)}
  {$EXTERNALSYM LOCK_WRITE}
{$ENDIF}  
  LOCK_EXCLUSIVE = 2;
{$IF defined(MSWINDOWS) or defined(ANDROID)}
  {$EXTERNALSYM LOCK_EXCLUSIVE}
{$ENDIF}  
  LOCK_ONLYONCE  = 4;
{$IF defined(MSWINDOWS) or defined(ANDROID)}
  {$EXTERNALSYM LOCK_ONLYONCE}
{$ENDIF}  

  { Unspecified error }
  E_FAIL                      = HRESULT($80004005);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM E_FAIL}
{$ENDIF}

  { Unable to perform requested operation. }
  STG_E_INVALIDFUNCTION       = HRESULT($80030001);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_INVALIDFUNCTION}
{$ENDIF}  

  { %l could not be found. }
  STG_E_FILENOTFOUND          = HRESULT($80030002);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_FILENOTFOUND}
{$ENDIF}  

  { The path %l could not be found. }
  STG_E_PATHNOTFOUND          = HRESULT($80030003);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_PATHNOTFOUND}
{$ENDIF}

  { There are insufficient resources to open another file. }
  STG_E_TOOMANYOPENFILES      = HRESULT($80030004);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_TOOMANYOPENFILES}
{$ENDIF}  

  { Access Denied. }
  STG_E_ACCESSDENIED          = HRESULT($80030005);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_ACCESSDENIED}
{$ENDIF}

  { Attempted an operation on an invalid object. }
  STG_E_INVALIDHANDLE         = HRESULT($80030006);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_INVALIDHANDLE}
{$ENDIF}  

  { There is insufficient memory available to complete operation. }
  STG_E_INSUFFICIENTMEMORY    = HRESULT($80030008);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_INSUFFICIENTMEMORY}
{$ENDIF}  

  { Invalid pointer error. }
  STG_E_INVALIDPOINTER        = HRESULT($80030009);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_INVALIDPOINTER}
{$ENDIF}  

  { There are no more entries to return. }
  STG_E_NOMOREFILES           = HRESULT($80030012);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_NOMOREFILES}
{$ENDIF}  

  { Disk is write-protected. }
  STG_E_DISKISWRITEPROTECTED  = HRESULT($80030013);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_DISKISWRITEPROTECTED}
{$ENDIF}

  { An error occurred during a seek operation. }
  STG_E_SEEKERROR             = HRESULT($80030019);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_SEEKERROR}
{$ENDIF}  

  { A disk error occurred during a write operation. }
  STG_E_WRITEFAULT            = HRESULT($8003001D);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_WRITEFAULT}
{$ENDIF}  

  { A disk error occurred during a read operation. }
  STG_E_READFAULT             = HRESULT($8003001E);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_READFAULT}
{$ENDIF}  

  { A share violation has occurred. }
  STG_E_SHAREVIOLATION        = HRESULT($80030020);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_SHAREVIOLATION}
{$ENDIF}  

  { A lock violation has occurred. }
  STG_E_LOCKVIOLATION         = HRESULT($80030021);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_LOCKVIOLATION}
{$ENDIF}  

  { %l already exists. }
  STG_E_FILEALREADYEXISTS     = HRESULT($80030050);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_FILEALREADYEXISTS}
{$ENDIF}

  { Invalid parameter error. }
  STG_E_INVALIDPARAMETER      = HRESULT($80030057);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_INVALIDPARAMETER}
{$ENDIF}

  { There is insufficient disk space to complete operation. }
  STG_E_MEDIUMFULL            = HRESULT($80030070);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_MEDIUMFULL}
{$ENDIF}  

  { Illegal write of non-simple property to simple property set. }
  STG_E_PROPSETMISMATCHED     = HRESULT($800300F0);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_PROPSETMISMATCHED}
{$ENDIF}  

  { An API call exited abnormally. }
  STG_E_ABNORMALAPIEXIT       = HRESULT($800300FA);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_ABNORMALAPIEXIT}
{$ENDIF}  

  { The file %l is not a valid compound file. }
  STG_E_INVALIDHEADER         = HRESULT($800300FB);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_INVALIDHEADER}
{$ENDIF}  

  { The name %l is not valid. }
  STG_E_INVALIDNAME           = HRESULT($800300FC);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_INVALIDNAME}
{$ENDIF}  

  { An unexpected error occurred. }
  STG_E_UNKNOWN               = HRESULT($800300FD);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_UNKNOWN}
{$ENDIF}  

  { That function is not implemented. }
  STG_E_UNIMPLEMENTEDFUNCTION = HRESULT($800300FE);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_UNIMPLEMENTEDFUNCTION}
{$ENDIF}  

  { Invalid flag error. }
  STG_E_INVALIDFLAG           = HRESULT($800300FF);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_INVALIDFLAG}
{$ENDIF}  

  { Attempted to use an object that is busy. }
  STG_E_INUSE                 = HRESULT($80030100);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_INUSE}
{$ENDIF}  

  { The storage has been changed since the last commit. }
  STG_E_NOTCURRENT            = HRESULT($80030101);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_NOTCURRENT}
{$ENDIF}  

  { Attempted to use an object that has ceased to exist. }
  STG_E_REVERTED              = HRESULT($80030102);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_REVERTED}
{$ENDIF}

  { Can't save. }
  STG_E_CANTSAVE              = HRESULT($80030103);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_CANTSAVE}
{$ENDIF}  

  { The compound file %l was produced with an incompatible version of storage. }
  STG_E_OLDFORMAT             = HRESULT($80030104);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_OLDFORMAT}
{$ENDIF}

  { The compound file %l was produced with a newer version of storage. }
  STG_E_OLDDLL                = HRESULT($80030105);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_OLDDLL}
{$ENDIF}  

  { Share.exe or equivalent is required for operation. }
  STG_E_SHAREREQUIRED         = HRESULT($80030106);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_SHAREREQUIRED}
{$ENDIF}

  { Illegal operation called on non-file based storage. }
  STG_E_NOTFILEBASEDSTORAGE   = HRESULT($80030107);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_NOTFILEBASEDSTORAGE}
{$ENDIF}  

  { Illegal operation called on object with extant marshallings. }
  STG_E_EXTANTMARSHALLINGS    = HRESULT($80030108);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_EXTANTMARSHALLINGS}
{$ENDIF}  

  { The docfile has been corrupted. }
  STG_E_DOCFILECORRUPT        = HRESULT($80030109);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_DOCFILECORRUPT}
{$ENDIF}  

  { OLE32.DLL has been loaded at the wrong address. }
  STG_E_BADBASEADDRESS        = HRESULT($80030110);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_BADBASEADDRESS}
{$ENDIF}  

  { The file download was aborted abnormally.  The file is incomplete. }
  STG_E_INCOMPLETE            = HRESULT($80030201);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_INCOMPLETE}
{$ENDIF}  

  { The file download has been terminated. }
  STG_E_TERMINATED            = HRESULT($80030202);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_E_TERMINATED}
{$ENDIF}  

  { The underlying file was converted to compound file format. }
  STG_S_CONVERTED             = HRESULT($00030200);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_S_CONVERTED}
{$ENDIF}  

  { The storage operation should block until more data is available. }
  STG_S_BLOCK                 = HRESULT($00030201);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_S_BLOCK}
{$ENDIF}  

  { The storage operation should retry immediately. }
  STG_S_RETRYNOW              = HRESULT($00030202);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_S_RETRYNOW}
{$ENDIF}  

  { The notified event sink will not influence the storage operation. }
  STG_S_MONITORING            = HRESULT($00030203);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM STG_S_MONITORING}
{$ENDIF}


type
  TOleChar = WideChar;
  POleStr = PWideChar;
  PPOleStr = ^POleStr;

  PCLSID = PGUID;
  TCLSID = TGUID;

{ 64-bit large integer }

  Largeint = Int64;
  {$EXTERNALSYM Largeint}

  PDWORD = ^DWORD;
  {$EXTERNALSYM PDWORD}

  { File System time stamps are represented with the following structure: }
  PFileTime = ^TFileTime;
  _FILETIME = record
    dwLowDateTime: DWORD;
    dwHighDateTime: DWORD;
  end;
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM _FILETIME}
{$ENDIF}  
  TFileTime = _FILETIME;
  FILETIME = _FILETIME;
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM FILETIME}
{$ENDIF}  

{ IStream interface }

  PStatStg = ^TStatStg;
  tagSTATSTG = record
    pwcsName: POleStr;
    dwType: Longint;
    cbSize: Largeint;
    mtime: TFileTime;
    ctime: TFileTime;
    atime: TFileTime;
    grfMode: Longint;
    grfLocksSupported: Longint;
    clsid: TCLSID;
    grfStateBits: Longint;
    reserved: Longint;
  end;
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM tagSTATSTG}
{$ENDIF}  
  TStatStg = tagSTATSTG;
  STATSTG = TStatStg;

  IClassFactory = interface(IUnknown)
    ['{00000001-0000-0000-C000-000000000046}']
    function CreateInstance(const unkOuter: IUnknown; const iid: TGUID;
      out obj): HResult; stdcall;
    function LockServer(fLock: LongBool): HResult; stdcall;
  end;
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM IClassFactory}
{$ENDIF}

  ISequentialStream = interface(IUnknown)
    ['{0c733a30-2a1c-11ce-ade5-00aa0044773d}']
    function Read(pv: Pointer; cb: Longint; pcbRead: PLongint): HResult;
      stdcall;
    function Write(pv: Pointer; cb: Longint; pcbWritten: PLongint): HResult;
      stdcall;
  end;
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM ISequentialStream}
{$ENDIF}  

  IStream = interface(ISequentialStream)
    ['{0000000C-0000-0000-C000-000000000046}']
    function Seek(dlibMove: Largeint; dwOrigin: Longint;
      out libNewPosition: Largeint): HResult; stdcall;
    function SetSize(libNewSize: Largeint): HResult; stdcall;
    function CopyTo(stm: IStream; cb: Largeint; out cbRead: Largeint;
      out cbWritten: Largeint): HResult; stdcall;
    function Commit(grfCommitFlags: Longint): HResult; stdcall;
    function Revert: HResult; stdcall;
    function LockRegion(libOffset: Largeint; cb: Largeint;
      dwLockType: Longint): HResult; stdcall;
    function UnlockRegion(libOffset: Largeint; cb: Largeint;
      dwLockType: Longint): HResult; stdcall;
    function Stat(out statstg: TStatStg; grfStatFlag: Longint): HResult;
      stdcall;
    function Clone(out stm: IStream): HResult; stdcall;
  end;
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM IStream}
{$ENDIF}  
{$ENDIF} { !MSWINDOWS }

function EqualRect(const R1, R2: TRect): Boolean; overload;
function EqualRect(const R1, R2: TRectF): Boolean; overload;
function Rect(Left, Top, Right, Bottom: Integer): TRect;
{$EXTERNALSYM Rect}
function RectF(Left, Top, Right, Bottom: Single): TRectF; inline; overload;
function Vector(const X, Y: Single; const W: Single = 1.0): TVector; overload;
function Vector(const P: TPointF; const W: Single = 1.0): TVector; overload;
function Vector3D(const X, Y, Z: Single; const W: Single = 1.0): TVector3D; overload;
function Vector3D(const P: TPoint3D; const W: Single = 1.0): TVector3D; overload;
function Point3D(const X, Y, Z: Single): TPoint3D; overload;
function Point3D(const AVector3D: TVector3D; const ATransform: Boolean = False): TPoint3D; overload;
function NormalizeRectF(const Pts: array of TPointF): TRectF; overload;
function NormalizeRect(const ARect: TRectF): TRectF; overload;
function RectWidth(const Rect: TRect): Integer; inline; overload;
function RectWidth(const Rect: TRectF): Single; inline; overload;
function RectHeight(const Rect: TRect): Integer; inline; overload;
function RectHeight(const Rect: TRectF): Single; inline; overload;
function RectCenter(var R: TRect; const Bounds: TRect): TRect; overload;
function RectCenter(var R: TRectF; const Bounds: TRectF): TRectF; overload;
function Bounds(ALeft, ATop, AWidth, AHeight: Integer): TRect;
{$EXTERNALSYM Bounds}
function Point(X, Y: Integer): TPoint; inline; overload;
{$EXTERNALSYM Point}
function PointF(X, Y: Single): TPointF; inline; overload;
function PointF(const V: TVector): TPointF; inline; overload;
function MinPoint(const P1, P2: TPointF): TPointF; overload;
function MinPoint(const P1, P2: TPoint): TPoint; overload;
function ScalePoint(const P: TPointF; dX, dY: Single): TPointF; overload;
function ScalePoint(const P: TPoint; dX, dY: Single): TPoint; overload;
function SmallPoint(X, Y: Integer): TSmallPoint; inline; overload;
function SmallPoint(XY: LongWord): TSmallPoint; overload;
function PtInRect(const Rect: TRect; const P: TPoint): Boolean; overload;
function PtInRect(const Rect: TRectF; const P: TPointF): Boolean; overload;
function PtInCircle(const Point, Center: TPoint; Radius: Integer): Boolean;
function IntersectRect(const Rect1, Rect2: TRect): Boolean; overload;
function IntersectRect(out Rect: TRect; const R1, R2: TRect): Boolean; overload;
function IntersectRect(const Rect1, Rect2: TRectF): Boolean; overload;
function IntersectRect(out Rect: TRectF; const R1, R2: TRectF): Boolean; overload;
function UnionRect(out Rect: TRect; const R1, R2: TRect): Boolean; overload;
function UnionRect(out Rect: TRectF; const R1, R2: TRectF): Boolean; overload;
function UnionRect(const ARect1, ARect2: TRect): TRect; inline; overload;
function UnionRect(const ARect1, ARect2: TRectF): TRectF; inline; overload;
function IsRectEmpty(const Rect: TRect): Boolean; overload;
function IsRectEmpty(const Rect: TRectF): Boolean; overload;
function OffsetRect(var R: TRect; DX, DY: Integer): Boolean; overload;
function OffsetRect(var R: TRectF; DX, DY: Single): Boolean; overload;
procedure MultiplyRect(var R: TRectF; const DX, DY: Single);
procedure InflateRect(var R: TRectF; const DX, DY: Single); overload;
procedure InflateRect(var R: TRect; const DX, DY: Integer); overload;
function CenterPoint(const Rect: TRect): TPoint;
function SplitRect(const Rect: TRect; SplitType: TSplitRectType; Size: Integer): TRect; overload;
function SplitRect(const Rect: TRect; SplitType: TSplitRectType; Percent: Double): TRect; overload;
function CenteredRect(const SourceRect: TRect; const CenteredRect: TRect): TRect;

function IntersectRectF(out Rect: TRectF; const R1, R2: TRectF): Boolean;
function UnionRectF(out Rect: TRectF; const R1, R2: TRectF): Boolean;

type
  TValueRelationship = -1..1;

const
  LessThanValue = Low(TValueRelationship);
  EqualsValue = 0;
  GreaterThanValue = High(TValueRelationship);

implementation

const
  // Single, 4bytes:    1-sign,  8-exp, 23-mantissa - 2^23 ~ 1E3*1E3*8 ~ 8E6 (really 8388608),
  //                          relative resolution = 1/(8E6) ~ 1.25E-7 (really 1.19E-7),
  //                          zero = 1/(2^(2^(8-1)) = 1/2^128 ~ 0.0625E-36 ~ 6.25E-38 (really 3E-39)
  FuzzFactorSingle = 10;
  SingleResolution: Single = 1.25E-7 * FuzzFactorSingle; // this is relative resolution of mantissa
  SingleZero: Single = 6.25E-37; // 6.25E-38 * FuzzFactorSingle;
  // Double, 8bytes:    1-sign, 11-exp, 52-mantissa - 2^52 ~ 1E3*1E3*1E3*1E3*1E3*4 = 4E15,
  //                             relative resolution = 2.5*E-16
  // FuzzFactorDouble = 10;
  // DoubleResolution: Double = 2.5E-16 * FuzzFactorDouble;
  // Extended, 10bytes: 1-sign, 15-exp, 64-mantissa - relative resolution = 0.0625*E-18
  // Real, 6bytes:      1-sign,  7-exp, 40-mantissa - 1.0*E-12 - deprecated

  // Example: for mantissa length=2 (0,xx - binary) resolution in mantissa is the last binary bite 0,01b = 1/4,
  //  for mantissa length=3 (0,xxx - binary) resolution in mantissa is the last binary bite 0,001b = 1/8, etc
  //  really the high digit is assumed to 1 (0,1xx or 0,1xxx) and the precision is 2 times higher

function Max(a, b: Integer): Integer; overload;
begin
  if a > b then Result := a else Result := b;
end;

function Max(a, b: Single): Single; overload;
begin
  if a > b then Result := a else Result := b;
end;

function Min(a, b: Integer): Integer; overload;
begin
  if a < b then Result := a else Result := b;
end;

function Min(a, b: Single): Single; overload;
begin
  if a < b then Result := a else Result := b;
end;

function SameValue(const A, B: Single; Epsilon: Single = 0): Boolean;
begin
{$EXCESSPRECISION OFF}
  if Epsilon = 0 then
    Epsilon := Max(Abs(A), Abs(B)) * SingleResolution;
  if Epsilon = 0 then
     Epsilon := SingleZero; // both A and B are very little, Epsilon was 0 because of normalization
  if A > B then
    Result := (A - B) <= Epsilon
  else
    Result := (B - A) <= Epsilon;
{$EXCESSPRECISION ON}
end;

procedure SinCosSingle(const Theta: Single; var Sin, Cos: Single);
var
{$IF SizeOf(Extended) = 10}
  S, C: Extended;
{$ELSE}
  S, C: Double;
{$ENDIF}
begin
  System.SineCosine(Theta, S, C);
  Sin := S;
  Cos := C;
end;

{ TVector }

class function TVector.Create(const APoint: TPointF): TVector;
begin
  Result.X := APoint.X;
  Result.Y := APoint.Y;
  Result.W := 1;
end;

class function TVector.Create(const AX, AY, AW: Single): TVector;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.W := AW;
end;

class operator TVector.Add(const AVector1, AVector2: TVector): TVector;
begin
{$EXCESSPRECISION OFF}
  Result.V[0] := AVector1.V[0] + AVector2.V[0];
  Result.V[1] := AVector1.V[1] + AVector2.V[1];
  Result.W := 1;
{$EXCESSPRECISION ON}
end;

class operator TVector.Subtract(const AVector1, AVector2: TVector): TVector;
begin
{$EXCESSPRECISION OFF}
  Result.V[0] := AVector1.V[0] - AVector2.V[0];
  Result.V[1] := AVector1.V[1] - AVector2.V[1];
  Result.W := 1;
{$EXCESSPRECISION ON}
end;

class operator TVector.Equal(const AVector1, AVector2: TVector): Boolean;
begin
  Result := SameValue(AVector1.X, AVector2.X) and SameValue(AVector1.Y, AVector2.Y);
end;

class operator TVector.NotEqual(const AVector1, AVector2: TVector): Boolean;
begin
  Result := not (AVector1 =  AVector2);
end;

class operator TVector.Implicit(const APoint: TPointF): TVector;
begin
  Result := Vector(APoint);
end;

class operator TVector.Implicit(const ASize: TSizeF): TVector;
begin
  Result := Vector(ASize.cx, ASize.cy);
end;

class operator TVector.Multiply(const AVector: TVector; const AFactor: Single): TVector;
begin
{$EXCESSPRECISION OFF}
  Result.X := AVector.X * AFactor;
  Result.Y := AVector.Y * AFactor;
  Result.W := AVector.W;
{$EXCESSPRECISION ON}
end;

class operator TVector.Multiply(const AFactor: Single; const AVector: TVector): TVector;
begin
{$EXCESSPRECISION OFF}
  Result.X := AFactor * AVector.X;
  Result.Y := AFactor * AVector.Y;
  Result.W := AVector.W;
{$EXCESSPRECISION ON}
end;

class operator TVector.Divide(const AVector: TVector; const AFactor: Single): TVector;
var
  InvFactor: Single;
begin
{$EXCESSPRECISION OFF}
  if AFactor <> 0 then
  begin
    InvFactor := 1 / AFactor;

    Result.X := AVector.X * InvFactor;
    Result.Y := AVector.Y * InvFactor;
    Result.W := AVector.W;
  end else
    Result := AVector;
{$EXCESSPRECISION ON}
end;

procedure TVector.Offset(const ADelta: TPointF);
begin
{$EXCESSPRECISION OFF}
  Self.X := Self.X + ADelta.X;
  Self.Y := Self.Y + ADelta.Y;
{$EXCESSPRECISION ON}
end;

procedure TVector.Offset(const ADeltaX, ADeltaY: Single);
begin
  Self.Offset(TPointF.Create(ADeltaX, ADeltaY));
end;

function TVector.CrossProduct(const AVector: TVector): Single;
begin
  Result := Self.X * AVector.Y - Self.Y * AVector.X;
end;

function TVector.DotProduct(const AVector: TVector): Single;
begin
  Result := Self.X * AVector.X + Self.Y * AVector.Y;
end;

function TVector.Length: Single;
begin
  Result := Sqrt(Self.X * Self.X + Self.Y * Self.Y);
end;

function TVector.Normalize: TVector;
var
  VecLen: Single;
begin
{$EXCESSPRECISION OFF}
  VecLen := Self.Length;

  if VecLen > 0.0 then
  begin
    Result.X := Self.X / VecLen;
    Result.Y := Self.Y / VecLen;
    Result.W := Self.W;
  end else
    Result := Self;
{$EXCESSPRECISION ON}
end;

function TVector.Rotate(const AAngle: Single): TVector;
begin
  Result := Self * TMatrix.CreateRotation(AAngle);
end;

function TVector.Reflect(const AVector: TVector): TVector;
begin
  Result := Self + AVector * (-2 * Self.DotProduct(AVector));
end;

function TVector.MidVector(const AVector: TVector): TVector;
begin
  Result.X := (Self.X + AVector.X) / 2;
  Result.Y := (Self.Y + AVector.Y) / 2;
  Result.W := 1;
end;

function TVector.AngleCosine(const AVector: TVector): Single;
begin
  Result := Self.Length * AVector.Length;

  if Abs(Result) > Epsilon then
    Result := Self.DotProduct(AVector) / Result
  else
    Result := Self.DotProduct(AVector) / Epsilon;

  Result := Max(Min(Result, 1), -1);
end;

function TVector.ToPointF: TPointF;
begin
  Result.X := Self.X;
  Result.Y := Self.Y;
end;

class function TMatrix.CreateRotation(const AAngle: Single): TMatrix;
var
  Sine, Cosine: Single;
begin
  SinCosSingle(AAngle, Sine, Cosine);

  Result := Identity;
  Result.m11 := Cosine;
  Result.m12 := Sine;
  Result.m21 := -Sine;
  Result.m22 := Cosine;
end;

class function TMatrix.CreateScaling(const AScaleX, AScaleY: Single): TMatrix;
begin
  Result := Identity;
  Result.m11 := AScaleX;
  Result.m22 := AScaleY;
end;

class function TMatrix.CreateTranslation(const ADeltaX, ADeltaY: Single): TMatrix;
begin
  Result := Identity;
  Result.m31 := ADeltaX;
  Result.m32 := ADeltaY;
end;

class operator TMatrix.Multiply(const AMatrix1, AMatrix2: TMatrix): TMatrix;
begin
  Result.m11 := AMatrix1.m11 * AMatrix2.m11 + AMatrix1.m12 * AMatrix2.m21 + AMatrix1.m13 * AMatrix2.m31;
  Result.m12 := AMatrix1.m11 * AMatrix2.m12 + AMatrix1.m12 * AMatrix2.m22 + AMatrix1.m13 * AMatrix2.m32;
  Result.m13 := AMatrix1.m11 * AMatrix2.m13 + AMatrix1.m12 * AMatrix2.m23 + AMatrix1.m13 * AMatrix2.m33;
  Result.m21 := AMatrix1.m21 * AMatrix2.m11 + AMatrix1.m22 * AMatrix2.m21 + AMatrix1.m23 * AMatrix2.m31;
  Result.m22 := AMatrix1.m21 * AMatrix2.m12 + AMatrix1.m22 * AMatrix2.m22 + AMatrix1.m23 * AMatrix2.m32;
  Result.m23 := AMatrix1.m21 * AMatrix2.m13 + AMatrix1.m22 * AMatrix2.m23 + AMatrix1.m23 * AMatrix2.m33;
  Result.m31 := AMatrix1.m31 * AMatrix2.m11 + AMatrix1.m32 * AMatrix2.m21 + AMatrix1.m33 * AMatrix2.m31;
  Result.m32 := AMatrix1.m31 * AMatrix2.m12 + AMatrix1.m32 * AMatrix2.m22 + AMatrix1.m33 * AMatrix2.m32;
  Result.m33 := AMatrix1.m31 * AMatrix2.m13 + AMatrix1.m32 * AMatrix2.m23 + AMatrix1.m33 * AMatrix2.m33;
end;

class operator TMatrix.Multiply(const APoint: TPointF; const AMatrix: TMatrix): TPointF;
begin
  Result.X := APoint.X * AMatrix.M[0].V[0] + APoint.Y * AMatrix.M[1].V[0]
    + AMatrix.M[2].V[0];
  Result.Y := APoint.X * AMatrix.M[0].V[1] + APoint.Y * AMatrix.M[1].V[1]
    + AMatrix.M[2].V[1];
end;

class operator TMatrix.Multiply(const AVector: TVector; const AMatrix: TMatrix): TVector;
begin
  Result.V[0] := AVector.V[0] * AMatrix.M[0].V[0] + AVector.V[1] * AMatrix.M[1].V[0]
    + AVector.V[2] * AMatrix.M[2].V[0];
  Result.V[1] := AVector.V[0] * AMatrix.M[0].V[1] + AVector.V[1] * AMatrix.M[1].V[1]
    + AVector.V[2] * AMatrix.M[2].V[1];
  Result.V[2] := AVector.V[0] * AMatrix.M[0].V[2] + AVector.V[1] * AMatrix.M[1].V[2]
    + AVector.V[2] * AMatrix.M[2].V[2];
end;

function TMatrix.Scale(const AFactor: Single): TMatrix;
var
  I: Integer;
begin
  for I := 0 to 2 do
  begin
    Result.M[I].V[0] := Self.M[I].V[0] * AFactor;
    Result.M[I].V[1] := Self.M[I].V[1] * AFactor;
    Result.M[I].V[2] := Self.M[I].V[2] * AFactor;
  end;
end;

function TMatrix.Determinant: Single;
begin
  Result := Self.M[0].V[0] * (Self.M[1].V[1] * Self.M[2].V[2] - Self.M[2].V[1] * Self.M[1].V[2]) - Self.M[0].V[1]
    * (Self.M[1].V[0] * Self.M[2].V[2] - Self.M[2].V[0] * Self.M[1].V[2]) + Self.M[0].V[2] * (Self.M[1].V[0]
    * Self.M[2].V[1] - Self.M[2].V[0] * Self.M[1].V[1]);
end;

function TMatrix.Adjoint: TMatrix;
var
  a1, a2, a3, b1, b2, b3, c1, c2, c3: Single;
begin
  a1 := Self.M[0].V[0];
  a2 := Self.M[0].V[1];
  a3 := Self.M[0].V[2];
  b1 := Self.M[1].V[0];
  b2 := Self.M[1].V[1];
  b3 := Self.M[1].V[2];
  c1 := Self.M[2].V[0];
  c2 := Self.M[2].V[1];
  c3 := Self.M[2].V[2];

  Result.M[0].V[0] := (b2 * c3 - c2 * b3);
  Result.M[1].V[0] := -(b1 * c3 - c1 * b3);
  Result.M[2].V[0] := (b1 * c2 - c1 * b2);

  Result.M[0].V[1] := -(a2 * c3 - c2 * a3);
  Result.M[1].V[1] := (a1 * c3 - c1 * a3);
  Result.M[2].V[1] := -(a1 * c2 - c1 * a2);

  Result.M[0].V[2] := (a2 * b3 - b2 * a3);
  Result.M[1].V[2] := -(a1 * b3 - b1 * a3);
  Result.M[2].V[2] := (a1 * b2 - b1 * a2);
end;

function TMatrix.Inverse: TMatrix;
const
  DefaultValue: TMatrix = (m11: 1.0; m12: 0.0; m13: 0.0; m21: 0.0; m22: 1.0; m23: 0.0; m31: 0.0; m32: 0.0; m33: 1.0);
var
  Det: Single;
begin
  Det := Self.Determinant;
  if Abs(Det) < Epsilon then
    Result := DefaultValue
  else
    Result:= Self.Adjoint.Scale(1 / Det);
end;

{ TPoint3D }

class function TPoint3D.Create(const AX, AY, AZ: Single): TPoint3D;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.Z := AZ;
end;

class function TPoint3D.Create(const P: TVector3DType): TPoint3D;
begin
  Result.X := P[0];
  Result.Y := P[1];
  Result.Z := P[2];
end;

class function TPoint3D.Create(const APoint: TPointF; const AZ: Single): TPoint3D;
begin
  Result.X := APoint.X;
  Result.Y := APoint.Y;
  Result.Z := AZ;
end;

class operator TPoint3D.Add(const APoint1, APoint2: TPoint3D): TPoint3D;
begin
{$EXCESSPRECISION OFF}
  Result.X := APoint1.X + APoint2.X;
  Result.Y := APoint1.Y + APoint2.Y;
  Result.Z := APoint1.Z + APoint2.Z;
{$EXCESSPRECISION ON}
end;

class operator TPoint3D.Subtract(const APoint1, APoint2: TPoint3D): TPoint3D;
begin
{$EXCESSPRECISION OFF}
  Result.X := APoint1.X - APoint2.X;
  Result.Y := APoint1.Y - APoint2.Y;
  Result.Z := APoint1.Z - APoint2.Z;
{$EXCESSPRECISION ON}
end;

class operator TPoint3D.Equal(const APoint1, APoint2: TPoint3D): Boolean;
begin
  Result := SameValue(APoint1.X, APoint2.X) and SameValue(APoint1.Y, APoint2.Y) and SameValue(APoint1.Z, APoint2.Z);
end;

class operator TPoint3D.NotEqual(const APoint1, APoint2: TPoint3D): Boolean;
begin
  Result := not (APoint1 = APoint2);
end;

class operator TPoint3D.Negative(const APoint: TPoint3D): TPoint3D;
begin
  Result.X := - APoint.X;
  Result.Y := - APoint.Y;
  Result.Z := - APoint.Z;
end;

class operator TPoint3D.Multiply(const APoint1, APoint2: TPoint3D): TPoint3D;
begin
{$EXCESSPRECISION OFF}
  Result.X := APoint1.X * APoint2.X;
  Result.Y := APoint1.Y * APoint2.Y;
  Result.Z := APoint1.Z * APoint2.Z;
{$EXCESSPRECISION ON}
end;

class operator TPoint3D.Multiply(const APoint: TPoint3D; const AFactor: Single): TPoint3D;
begin
{$EXCESSPRECISION OFF}
  Result.X := APoint.X * AFactor;
  Result.Y := APoint.Y * AFactor;
  Result.Z := APoint.Z * AFactor;
{$EXCESSPRECISION ON}
end;

class operator TPoint3D.Multiply(const AFactor: Single; const APoint: TPoint3D): TPoint3D;
begin
{$EXCESSPRECISION OFF}
  Result.X := AFactor * APoint.X;
  Result.Y := AFactor * APoint.Y;
  Result.Z := AFactor * APoint.Z;
{$EXCESSPRECISION ON}
end;

class operator TPoint3D.Divide(const APoint: TPoint3D; const AFactor: Single): TPoint3D;
var
  InvFactor: Single;
begin
{$EXCESSPRECISION OFF}
  if AFactor <> 0 then
  begin
    InvFactor := 1 / AFactor;

    Result.X := APoint.X * InvFactor;
    Result.Y := APoint.Y * InvFactor;
    Result.Z := APoint.Z * InvFactor;
  end else
    Result := APoint;
{$EXCESSPRECISION ON}
end;

procedure TPoint3D.Offset(const ADelta: TPoint3D);
begin
  Self := Self + ADelta;
end;

procedure TPoint3D.Offset(const ADeltaX, ADeltaY, ADeltaZ: Single);
begin
  Self.Offset(TPoint3D.Create(ADeltaX, ADeltaY, ADeltaZ));
end;

function TPoint3D.CrossProduct(const APoint: TPoint3D): TPoint3D;
begin
  Result.X := (Self.Y * APoint.Z) - (Self.Z * APoint.Y);
  Result.Y := (Self.Z * APoint.X) - (Self.X * APoint.Z);
  Result.Z := (Self.X * APoint.Y) - (Self.Y * APoint.X);
end;

function TPoint3D.DotProduct(const APoint: TPoint3D): Single;
begin
  Result := (Self.X * APoint.X) + (Self.Y * APoint.Y) + (Self.Z * APoint.Z);
end;

function TPoint3D.Length: Single;
begin
  Result := Sqrt(Self.X * Self.X + Self.Y * Self.Y + Self.Z * Self.Z);
end;

function TPoint3D.Normalize: TPoint3D;
var
  VecLen: Single;
begin
{$EXCESSPRECISION OFF}
  VecLen := Self.Length;

  if VecLen > 0.0 then
  begin
    Result.X := Self.X / VecLen;
    Result.Y := Self.Y / VecLen;
    Result.Z := Self.Z / VecLen;
  end else
    Result := Self;
{$EXCESSPRECISION ON}
end;

function TPoint3D.Distance(const APoint: TPoint3D): Single;
begin
{$EXCESSPRECISION OFF}
  Result := (Self - APoint).Length;
{$EXCESSPRECISION ON}
end;

function TPoint3D.Rotate(const AAxis: TPoint3D; const AAngle: Single): TPoint3D;
begin
  Result := Self * TMatrix3D.CreateRotation(AAxis, AAngle);
end;

function TPoint3D.Reflect(const APoint: TPoint3D): TPoint3D;
begin
  Result := Self + APoint * (-2 * Self.DotProduct(APoint));
end;

function TPoint3D.MidPoint(const APoint: TPoint3D): TPoint3D;
begin
  Result.X := (Self.X + APoint.X) / 2;
  Result.Y := (Self.Y + APoint.Y) / 2;
  Result.Z := (Self.Y + APoint.Y) / 2;
end;

function TPoint3D.AngleCosine(const APoint: TPoint3D): Single;
begin
  Result := Self.Length * APoint.Length;

  if Abs(Result) > Epsilon then
    Result := Self.DotProduct(APoint) / Result
  else
    Result := Self.DotProduct(APoint) / Epsilon;

  Result := Max(Min(Result, 1), -1);
end;

{ TVector3D }

class function TVector3D.Create(const AX, AY, AZ, AW: Single): TVector3D;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.Z := AZ;
  Result.W := AW;
end;

class function TVector3D.Create(const APoint: TPoint3D; const AW: Single): TVector3D;
begin
  Result.X := APoint.X;
  Result.Y := APoint.Y;
  Result.Z := APoint.Z;
  Result.W := AW;
end;

class operator TVector3D.Add(const AVector1, AVector2: TVector3D): TVector3D;
begin
{$EXCESSPRECISION OFF}
  Result.X := AVector1.X + AVector2.X;
  Result.Y := AVector1.Y + AVector2.Y;
  Result.Z := AVector1.Z + AVector2.Z;
  Result.W := 1.0;
{$EXCESSPRECISION ON}
end;

class operator TVector3D.Subtract(const AVector1, AVector2: TVector3D): TVector3D;
begin
{$EXCESSPRECISION OFF}
  Result.X := AVector1.X - AVector2.X;
  Result.Y := AVector1.Y - AVector2.Y;
  Result.Z := AVector1.Z - AVector2.Z;
  Result.W := 1.0;
{$EXCESSPRECISION ON}
end;

class operator TVector3D.Equal(const AVector1, AVector2: TVector3D): Boolean;
begin
  Result := SameValue(AVector1.X, AVector2.X)
    and SameValue(AVector1.Y, AVector2.Y)
    and SameValue(AVector1.Z, AVector2.Z);
end;

class operator TVector3D.NotEqual(const AVector1, AVector2: TVector3D): Boolean;
begin
  Result := not (AVector1 = AVector2);
end;

class operator TVector3D.Negative(const AVector: TVector3D): TVector3D;
begin
  Result.X := - AVector.X;
  Result.Y := - AVector.Y;
  Result.Z := - AVector.Z;
  Result.W := AVector.W;
end;

class operator TVector3D.Implicit(const APoint: TPoint3D): TVector3D;
begin
  Result := Vector3D(APoint);
end;

class operator TVector3D.Multiply(const AVector1, AVector2: TVector3D): TVector3D;
begin
{$EXCESSPRECISION OFF}
  Result.X := AVector1.X * AVector2.X;
  Result.Y := AVector1.Y * AVector2.Y;
  Result.Z := AVector1.Z * AVector2.Z;
  Result.W := AVector1.W * AVector2.W;
{$EXCESSPRECISION ON}
end;

class operator TVector3D.Multiply(const AVector: TVector3D; const AFactor: Single): TVector3D;
begin
{$EXCESSPRECISION OFF}
  Result.X := AVector.X * AFactor;
  Result.Y := AVector.Y * AFactor;
  Result.Z := AVector.Z * AFactor;
  Result.W := AVector.W;
{$EXCESSPRECISION ON}
end;

class operator TVector3D.Multiply(const AFactor: Single; const AVector: TVector3D): TVector3D;
begin
{$EXCESSPRECISION OFF}
  Result.X := AFactor * AVector.X;
  Result.Y := AFactor * AVector.Y;
  Result.Z := AFactor * AVector.Z;
  Result.W := AVector.W;
{$EXCESSPRECISION ON}
end;

class operator TVector3D.Divide(const AVector: TVector3D; const AFactor: Single): TVector3D;
var
  InvFactor: Single;
begin
{$EXCESSPRECISION OFF}
  if AFactor <> 0 then
  begin
    InvFactor := 1 / AFactor;

    Result.X := AVector.X * InvFactor;
    Result.Y := AVector.Y * InvFactor;
    Result.Z := AVector.Z * InvFactor;
    Result.W := AVector.W;
  end else
    Result := AVector;
{$EXCESSPRECISION ON}
end;

procedure TVector3D.Offset(const ADelta: TPoint3D);
begin
{$EXCESSPRECISION OFF}
  Self.X := Self.X + ADelta.X;
  Self.Y := Self.Y + ADelta.Y;
  Self.Z := Self.Z + ADelta.Z;
  Self.W := Self.W;
{$EXCESSPRECISION ON}
end;

procedure TVector3D.Offset(const ADeltaX, ADeltaY, ADeltaZ: Single);
begin
  Self.Offset(TPoint3D.Create(ADeltaX, ADeltaY, ADeltaZ));
end;

function TVector3D.CrossProduct(const AVector: TVector3D): TVector3D;
begin
  Result.X := Self.Y * AVector.Z - Self.Z * AVector.Y;
  Result.Y := Self.Z * AVector.X - Self.X * AVector.Z;
  Result.Z := Self.X * AVector.Y - Self.Y * AVector.X;
  Result.W := 1;
end;

function TVector3D.DotProduct(const AVector: TVector3D): Single;
begin
  Result := Self.X * AVector.X + Self.Y * AVector.Y + Self.Z * AVector.Z;
end;

function TVector3D.Length: Single;
begin
  Result := Sqrt(Self.X * Self.X + Self.Y * Self.Y + Self.Z * Self.Z);
end;

function TVector3D.Normalize: TVector3D;
var
  VecLen: Single;
begin
{$EXCESSPRECISION OFF}
  VecLen := Self.Length;

  if VecLen > 0 then
  begin
    Result.X := Self.X / VecLen;
    Result.Y := Self.Y / VecLen;
    Result.Z := Self.Z / VecLen;
    Result.W := Self.W;
  end else
    Result := Self;
{$EXCESSPRECISION ON}
end;

function TVector3D.Distance(const AVector: TVector3D): Single;
begin
{$EXCESSPRECISION OFF}
  Result := (AVector - Self).Length;
{$EXCESSPRECISION ON}
end;

function TVector3D.Rotate(const AAxis: TPoint3D; const AAngle: Single): TVector3D;
begin
  Result := Self * TMatrix3D.CreateRotation(AAxis, AAngle);
end;

function TVector3D.Reflect(const AVector: TVector3D): TVector3D;
begin
  Result := Self + AVector * (-2 * Self.DotProduct(AVector));
end;

function TVector3D.MidVector(const AVector: TVector3D): TVector3D;
begin
  Result.X := (Self.X + AVector.X) / 2;
  Result.Y := (Self.Y + AVector.Y) / 2;
  Result.Z := (Self.Z + AVector.Z) / 2;
  Result.W := 1;
end;

function TVector3D.AngleCosine(const AVector: TVector3D): Single;
begin
  Result := Self.Length * AVector.Length;

  if Abs(Result) > Epsilon then
    Result := Self.DotProduct(AVector) / Result
  else
    Result := Self.DotProduct(AVector) / Epsilon;

  Result := Max(Min(Result, 1), -1);
end;

function TVector3D.ToPoint3D(const ATransform: Boolean): TPoint3D;
begin
 if (not ATransform) or SameValue(Self.W, 0) then
 begin
   Result.X := Self.X;
   Result.Y := Self.Y;
   Result.Z := Self.Z;
 end else
 begin
   Result.X := Self.X / Self.W;
   Result.Y := Self.Y / Self.W;
   Result.Z := Self.Z / Self.W;
 end;
end;

{ TMatrix3D }

constructor TMatrix3D.Create(const AM11, AM12, AM13, AM14, AM21, AM22, AM23, AM24, AM31, AM32, AM33, AM34, AM41, AM42,
  AM43, AM44: Single);
begin
  Self.m11 := AM11;
  Self.m12 := AM12;
  Self.m13 := AM13;
  Self.m14 := AM14;
  Self.m21 := AM21;
  Self.m22 := AM22;
  Self.m23 := AM23;
  Self.m24 := AM24;
  Self.m31 := AM31;
  Self.m32 := AM32;
  Self.m33 := AM33;
  Self.m34 := AM34;
  Self.m41 := AM41;
  Self.m42 := AM42;
  Self.m43 := AM43;
  Self.m44 := AM44;
end;

constructor TMatrix3D.Create(const AArray: TSingleDynArray);
begin
  Self := TMatrix3D.Create(AArray[0], AArray[4], AArray[8], AArray[12], AArray[1], AArray[5], AArray[9], AArray[13],
    AArray[2], AArray[6], AArray[10], AArray[14], AArray[3], AArray[7], AArray[11], AArray[15]);
end;

class function TMatrix3D.CreateScaling(const AScale: TPoint3D): TMatrix3D;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.m11 := AScale.X;
  Result.m22 := AScale.Y;
  Result.m33 := AScale.Z;
  Result.m44 := 1;
end;

class function TMatrix3D.CreateTranslation(const ATranslation: TPoint3D): TMatrix3D;
begin
  Result := Identity;
  Result.m41 := ATranslation.X;
  Result.m42 := ATranslation.Y;
  Result.m43 := ATranslation.Z;
end;

class function TMatrix3D.CreateRotationX(const AAngle: Single): TMatrix3D;
var
  Sine, Cosine: Single;
begin
  SinCosSingle(AAngle, Sine, Cosine);

  Result := Identity;
  Result.m22 := Cosine;
  Result.m23 := Sine;
  Result.m32 := - Result.m23;
  Result.m33 := Result.m22;
end;

class function TMatrix3D.CreateRotationY(const AAngle: Single): TMatrix3D;
var
  Sine, Cosine: Single;
begin
  SinCosSingle(AAngle, Sine, Cosine);

  Result := Identity;
  Result.m11 := Cosine;
  Result.m13 := - Sine;
  Result.m31 := - Result.m13;
  Result.m33 := Result.m11;
end;

class function TMatrix3D.CreateRotationZ(const AAngle: Single): TMatrix3D;
var
  Sine, Cosine: Single;
begin
  SinCosSingle(AAngle, Sine, Cosine);

  Result := Identity;
  Result.m11 := Cosine;
  Result.m12 := Sine;
  Result.m21 := - Result.m12;
  Result.m22 := Result.m11;
end;

class function TMatrix3D.CreateRotation(const AAxis: TPoint3D; const AAngle: Single): TMatrix3D;
var
  NormAxis: TPoint3D;
  Cosine, Sine, OneMinusCos: Single;
begin
  SinCosSingle(AAngle, Sine, Cosine);
  OneMinusCos := 1 - Cosine;
  NormAxis := AAxis.Normalize;

  Result := Identity;
  Result.m11 := (OneMinusCos * NormAxis.X * NormAxis.X) + Cosine;
  Result.m12 := (OneMinusCos * NormAxis.X * NormAxis.Y) - (NormAxis.Z * Sine);
  Result.m13 := (OneMinusCos * NormAxis.Z * NormAxis.X) + (NormAxis.Y * Sine);
  Result.m21 := (OneMinusCos * NormAxis.X * NormAxis.Y) + (NormAxis.Z * Sine);
  Result.m22 := (OneMinusCos * NormAxis.Y * NormAxis.Y) + Cosine;
  Result.m23 := (OneMinusCos * NormAxis.Y * NormAxis.Z) - (NormAxis.X * Sine);
  Result.m31 := (OneMinusCos * NormAxis.Z * NormAxis.X) - (NormAxis.Y * Sine);
  Result.m32 := (OneMinusCos * NormAxis.Y * NormAxis.Z) + (NormAxis.X * Sine);
  Result.m33 := (OneMinusCos * NormAxis.Z * NormAxis.Z) + Cosine;
end;

class function TMatrix3D.CreateRotationYawPitchRoll(const AYaw, APitch, ARoll: Single): TMatrix3D;
var
  SineYaw, SinePitch, SineRoll: Single;
  CosineYaw, CosinePitch, CosineRoll: Single;
begin
  SinCosSingle(AYaw, SineYaw, CosineYaw);
  SinCosSingle(APitch, SinePitch, CosinePitch);
  SinCosSingle(ARoll, SineRoll, CosineRoll);

  Result := Identity;
  Result.m11 := CosineRoll * CosineYaw + SinePitch * SineRoll * SineYaw;
  Result.m12 := CosineYaw * SinePitch * SineRoll - CosineRoll * SineYaw;
  Result.m13 := - CosinePitch * SineRoll;
  Result.m21 := CosinePitch * SineYaw;
  Result.m22 := CosinePitch * CosineYaw;
  Result.m23 := SinePitch;
  Result.m31 := CosineYaw * SineRoll - CosineRoll * SinePitch * SineYaw;
  Result.m32 := - CosineRoll * CosineYaw * SinePitch - SineRoll * SineYaw;
  Result.m33 := CosinePitch * CosineRoll;
end;

class function TMatrix3D.CreateRotationHeadingPitchBank(const AHeading, APitch, ABank: Single): TMatrix3D;
var
  SineHeading, SinePitch, SineBank: Single;
  CosineHeading, CosinePitch, CosineBank: Single;
begin
  SinCosSingle(AHeading, SineHeading, CosineHeading);
  SinCosSingle(APitch, SinePitch, CosinePitch);
  SinCosSingle(ABank, SineBank, CosineBank);

  Result := Identity;
  Result.m11 := (CosineHeading * CosineBank) + (SineHeading * SinePitch * SineBank);
  Result.m12 := (- CosineHeading * SineBank) + (SineHeading * SinePitch * CosineBank);
  Result.m13 := SineHeading * CosinePitch;
  Result.m21 := SineBank * CosinePitch;
  Result.m22 := CosineBank * CosinePitch;
  Result.m23 := - SinePitch;
  Result.m31 := (- SineHeading * CosineBank) + (CosineHeading * SinePitch * SineBank);
  Result.m32 := (SineBank * SineHeading) + (CosineHeading * SinePitch * CosineBank);
  Result.m33 := CosineHeading * CosinePitch;
end;

class function TMatrix3D.CreateLookAtRH(const ASource, ATarget, ACeiling: TPoint3D): TMatrix3D;
var
  ZAxis, XAxis, YAxis: TPoint3D;
begin
  ZAxis := (ASource - ATarget).Normalize;
  XAxis := ACeiling.CrossProduct(ZAxis).Normalize;
  YAxis := ZAxis.CrossProduct(XAxis);

  Result := Identity;
  Result.m11 := XAxis.X;
  Result.m12 := YAxis.X;
  Result.m13 := ZAxis.X;
  Result.m21 := XAxis.Y;
  Result.m22 := YAxis.Y;
  Result.m23 := ZAxis.Y;
  Result.m31 := XAxis.Z;
  Result.m32 := YAxis.Z;
  Result.m33 := ZAxis.Z;
  Result.m41 := - XAxis.DotProduct(ASource);
  Result.m42 := - YAxis.DotProduct(ASource);
  Result.m43 := - ZAxis.DotProduct(ASource);
end;

class function TMatrix3D.CreateLookAtLH(const ASource, ATarget, ACeiling: TPoint3D): TMatrix3D;
var
  ZAxis, XAxis, YAxis: TPoint3D;
begin
  ZAxis := (ATarget - ASource).Normalize;
  XAxis := ACeiling.CrossProduct(ZAxis).Normalize;
  YAxis := ZAxis.CrossProduct(XAxis);

  Result := Identity;
  Result.m11 := XAxis.X;
  Result.m12 := YAxis.X;
  Result.m13 := ZAxis.X;
  Result.m21 := XAxis.Y;
  Result.m22 := YAxis.Y;
  Result.m23 := ZAxis.Y;
  Result.m31 := XAxis.Z;
  Result.m32 := YAxis.Z;
  Result.m33 := ZAxis.Z;
  Result.m41 := - XAxis.DotProduct(ASource);
  Result.m42 := - YAxis.DotProduct(ASource);
  Result.m43 := - ZAxis.DotProduct(ASource);
end;

class function TMatrix3D.CreateLookAtDirRH(const ASource, ADirection, ACeiling: TPoint3D): TMatrix3D;
var
  ZAxis, XAxis, YAxis: TPoint3D;
begin
  ZAxis := ADirection.Normalize;
  XAxis := ACeiling.CrossProduct(ZAxis).Normalize;
  YAxis := ZAxis.CrossProduct(XAxis);

  Result := Identity;
  Result.m11 := XAxis.X;
  Result.m12 := YAxis.X;
  Result.m13 := ZAxis.X;
  Result.m21 := XAxis.Y;
  Result.m22 := YAxis.Y;
  Result.m23 := ZAxis.Y;
  Result.m31 := XAxis.Z;
  Result.m32 := YAxis.Z;
  Result.m33 := ZAxis.Z;
  Result.m41 := - XAxis.DotProduct(ASource);
  Result.m42 := - YAxis.DotProduct(ASource);
  Result.m43 := - ZAxis.DotProduct(ASource);
end;

class function TMatrix3D.CreateLookAtDirLH(const ASource, ADirection, ACeiling: TPoint3D): TMatrix3D;
var
  ZAxis, XAxis, YAxis: TPoint3D;
begin
  ZAxis := - ADirection.Normalize;
  XAxis := ACeiling.CrossProduct(ZAxis).Normalize;
  YAxis := ZAxis.CrossProduct(XAxis);

  Result := Identity;
  Result.m11 := XAxis.X;
  Result.m12 := YAxis.X;
  Result.m13 := ZAxis.X;
  Result.m21 := XAxis.Y;
  Result.m22 := YAxis.Y;
  Result.m23 := ZAxis.Y;
  Result.m31 := XAxis.Z;
  Result.m32 := YAxis.Z;
  Result.m33 := ZAxis.Z;
  Result.m41 := - XAxis.DotProduct(ASource);
  Result.m42 := - YAxis.DotProduct(ASource);
  Result.m43 := - ZAxis.DotProduct(ASource);
end;

class function TMatrix3D.CreateOrthoLH(const AWidth, AHeight, AZNear, AZFar: Single): TMatrix3D;
begin
  Result := TMatrix3D.Identity;
  Result.m11 := 2 / AWidth;
  Result.m22 := 2 / AHeight;
  Result.m33 := 1 / (AZFar - AZNear);
  Result.m42 := AZNear / (AZNear - AZFar);
end;

class function TMatrix3D.CreateOrthoRH(const AWidth, AHeight, AZNear, AZFar: Single): TMatrix3D;
begin
  Result := TMatrix3D.Identity;
  Result.m11 := 2 / AWidth;
  Result.m22 := 2 / AHeight;
  Result.m33 := 1 / (AZNear - AZFar);
  Result.m42 := AZNear / (AZNear - AZFar);
end;

class function TMatrix3D.CreateOrthoOffCenterLH(const ALeft, ATop, ARight, ABottom, AZNear, AZFar: Single): TMatrix3D;
begin
  Result := TMatrix3D.Identity;
  Result.m11 := 2 / (ARight - ALeft);
  Result.m22 := 2 / (ATop - ABottom);
  Result.m33 := 1 / (AZFar - AZNear);
  Result.m41 := (ALeft + ARight) / (ALeft - ARight);
  Result.m42 := (ATop + ABottom) / (ABottom - ATop);
  Result.m43 := AZNear / (AZNear - AZFar);
end;

class function TMatrix3D.CreateOrthoOffCenterRH(const ALeft, ATop, ARight, ABottom, AZNear, AZFar: Single): TMatrix3D;
begin
  Result := TMatrix3D.Identity;
  Result.m11 := 2 / (ARight - ALeft);
  Result.m22 := 2 / (ATop - ABottom);
  Result.m33 := 1 / (AZNear - AZFar);
  Result.m41 := (ALeft + ARight) / (ALeft - ARight);
  Result.m42 := (ATop + ABottom) / (ABottom - ATop);
  Result.m43 := AZNear / (AZNear - AZFar);
end;

class function TMatrix3D.CreatePerspectiveFovLH(const AFOV, AAspect, AZNear, AZFar: Single;
  const AHorizontalFOV: Boolean = False): TMatrix3D;
var
  XScale, YScale: Single;
begin
  if AHorizontalFOV then
  begin
    XScale := 1 / Tangent(AFOV / 2);
    YScale := XScale / AAspect;
  end else
  begin
    YScale := 1 / Tangent(AFOV / 2);
    XScale := YScale / AAspect;
  end;

  Result := TMatrix3D.Identity;
  Result.m11 := XScale;
  Result.m22 := YScale;
  Result.m33 := AZFar / (AZFar - AZNear);
  Result.m34 := 1;
  Result.m43 := -AZNear * AZFar / (AZFar - AZNear);
  Result.m44 := 0;
end;

class function TMatrix3D.CreatePerspectiveFovRH(const AFOV, AAspect, AZNear, AZFar: Single;
  const AHorizontalFOV: Boolean = False): TMatrix3D;
var
  XScale, YScale: Single;
begin
  if AHorizontalFOV then
  begin
    XScale := 1 / Tangent(AFOV / 2);
    YScale := XScale / AAspect;
  end else
  begin
    YScale := 1 / Tangent(AFOV / 2);
    XScale := YScale / AAspect;
  end;

  Result := TMatrix3D.Identity;
  Result.m11 := XScale;
  Result.m22 := YScale;
  Result.m33 := AZFar / (AZNear - AZFar);
  Result.m34 := -1;
  Result.m43 := AZNear * AZFar / (AZNear - AZFar);
  Result.m44 := 0;
end;

class operator TMatrix3D.Multiply(const APoint1, APoint2: TMatrix3D): TMatrix3D;
begin
  Result.M[0].V[0] := APoint1.M[0].V[0] * APoint2.M[0].V[0] + APoint1.M[0].V[1] * APoint2.M[1].V[0]
    + APoint1.M[0].V[2] * APoint2.M[2].V[0] + APoint1.M[0].V[3] * APoint2.M[3].V[0];
  Result.M[0].V[1] := APoint1.M[0].V[0] * APoint2.M[0].V[1] + APoint1.M[0].V[1] * APoint2.M[1].V[1]
    + APoint1.M[0].V[2] * APoint2.M[2].V[1] + APoint1.M[0].V[3] * APoint2.M[3].V[1];
  Result.M[0].V[2] := APoint1.M[0].V[0] * APoint2.M[0].V[2] + APoint1.M[0].V[1] * APoint2.M[1].V[2]
    + APoint1.M[0].V[2] * APoint2.M[2].V[2] + APoint1.M[0].V[3] * APoint2.M[3].V[2];
  Result.M[0].V[3] := APoint1.M[0].V[0] * APoint2.M[0].V[3] + APoint1.M[0].V[1] * APoint2.M[1].V[3]
    + APoint1.M[0].V[2] * APoint2.M[2].V[3] + APoint1.M[0].V[3] * APoint2.M[3].V[3];
  Result.M[1].V[0] := APoint1.M[1].V[0] * APoint2.M[0].V[0] + APoint1.M[1].V[1] * APoint2.M[1].V[0]
    + APoint1.M[1].V[2] * APoint2.M[2].V[0] + APoint1.M[1].V[3] * APoint2.M[3].V[0];
  Result.M[1].V[1] := APoint1.M[1].V[0] * APoint2.M[0].V[1] + APoint1.M[1].V[1] * APoint2.M[1].V[1]
    + APoint1.M[1].V[2] * APoint2.M[2].V[1] + APoint1.M[1].V[3] * APoint2.M[3].V[1];
  Result.M[1].V[2] := APoint1.M[1].V[0] * APoint2.M[0].V[2] + APoint1.M[1].V[1] * APoint2.M[1].V[2]
    + APoint1.M[1].V[2] * APoint2.M[2].V[2] + APoint1.M[1].V[3] * APoint2.M[3].V[2];
  Result.M[1].V[3] := APoint1.M[1].V[0] * APoint2.M[0].V[3] + APoint1.M[1].V[1] * APoint2.M[1].V[3]
    + APoint1.M[1].V[2] * APoint2.M[2].V[3] + APoint1.M[1].V[3] * APoint2.M[3].V[3];
  Result.M[2].V[0] := APoint1.M[2].V[0] * APoint2.M[0].V[0] + APoint1.M[2].V[1] * APoint2.M[1].V[0]
    + APoint1.M[2].V[2] * APoint2.M[2].V[0] + APoint1.M[2].V[3] * APoint2.M[3].V[0];
  Result.M[2].V[1] := APoint1.M[2].V[0] * APoint2.M[0].V[1] + APoint1.M[2].V[1] * APoint2.M[1].V[1]
    + APoint1.M[2].V[2] * APoint2.M[2].V[1] + APoint1.M[2].V[3] * APoint2.M[3].V[1];
  Result.M[2].V[2] := APoint1.M[2].V[0] * APoint2.M[0].V[2] + APoint1.M[2].V[1] * APoint2.M[1].V[2]
    + APoint1.M[2].V[2] * APoint2.M[2].V[2] + APoint1.M[2].V[3] * APoint2.M[3].V[2];
  Result.M[2].V[3] := APoint1.M[2].V[0] * APoint2.M[0].V[3] + APoint1.M[2].V[1] * APoint2.M[1].V[3]
    + APoint1.M[2].V[2] * APoint2.M[2].V[3] + APoint1.M[2].V[3] * APoint2.M[3].V[3];
  Result.M[3].V[0] := APoint1.M[3].V[0] * APoint2.M[0].V[0] + APoint1.M[3].V[1] * APoint2.M[1].V[0]
    + APoint1.M[3].V[2] * APoint2.M[2].V[0] + APoint1.M[3].V[3] * APoint2.M[3].V[0];
  Result.M[3].V[1] := APoint1.M[3].V[0] * APoint2.M[0].V[1] + APoint1.M[3].V[1] * APoint2.M[1].V[1]
    + APoint1.M[3].V[2] * APoint2.M[2].V[1] + APoint1.M[3].V[3] * APoint2.M[3].V[1];
  Result.M[3].V[2] := APoint1.M[3].V[0] * APoint2.M[0].V[2] + APoint1.M[3].V[1] * APoint2.M[1].V[2]
    + APoint1.M[3].V[2] * APoint2.M[2].V[2] + APoint1.M[3].V[3] * APoint2.M[3].V[2];
  Result.M[3].V[3] := APoint1.M[3].V[0] * APoint2.M[0].V[3] + APoint1.M[3].V[1] * APoint2.M[1].V[3]
    + APoint1.M[3].V[2] * APoint2.M[2].V[3] + APoint1.M[3].V[3] * APoint2.M[3].V[3];
end;

class operator TMatrix3D.Multiply(const APoint: TPoint3D; const AMatrix: TMatrix3D): TPoint3D;
begin
  Result.X := (APoint.X * AMatrix.m11) + (APoint.Y * AMatrix.m21) + (APoint.Z * AMatrix.m31) + AMatrix.m41;
  Result.Y := (APoint.X * AMatrix.m12) + (APoint.Y * AMatrix.m22) + (APoint.Z * AMatrix.m32) + AMatrix.m42;
  Result.Z := (APoint.X * AMatrix.m13) + (APoint.Y * AMatrix.m23) + (APoint.Z * AMatrix.m33) + AMatrix.m43;
end;

class operator TMatrix3D.Multiply(const AVector: TVector3D;
 const AMatrix: TMatrix3D): TVector3D;
begin
  Result.X := (AVector.X * AMatrix.m11) + (AVector.Y * AMatrix.m21) + (AVector.Z * AMatrix.m31) + (AVector.W * AMatrix.m41);
  Result.Y := (AVector.X * AMatrix.m12) + (AVector.Y * AMatrix.m22) + (AVector.Z * AMatrix.m32) + (AVector.W * AMatrix.m42);
  Result.Z := (AVector.X * AMatrix.m13) + (AVector.Y * AMatrix.m23) + (AVector.Z * AMatrix.m33) + (AVector.W * AMatrix.m43);
  Result.W := (AVector.X * AMatrix.m14) + (AVector.Y * AMatrix.m24) + (AVector.Z * AMatrix.m34) + (AVector.W * AMatrix.m44);
end;

function TMatrix3D.Scale(const AFactor: single): TMatrix3D;
var
  i: Integer;
begin
{$EXCESSPRECISION OFF}
  for i := 0 to 3 do
  begin
    Result.M[i].V[0] := Self.M[i].V[0] * AFactor;
    Result.M[i].V[1] := Self.M[i].V[1] * AFactor;
    Result.M[i].V[2] := Self.M[i].V[2] * AFactor;
    Result.M[i].V[3] := Self.M[i].V[3] * AFactor;
  end;
{$EXCESSPRECISION ON}
end;

function TMatrix3D.Transpose: TMatrix3D;
begin
  Result.M[0].V[0] := Self.M[0].V[0];
  Result.M[0].V[1] := Self.M[1].V[0];
  Result.M[0].V[2] := Self.M[2].V[0];
  Result.M[0].V[3] := Self.M[3].V[0];
  Result.M[1].V[0] := Self.M[0].V[1];
  Result.M[1].V[1] := Self.M[1].V[1];
  Result.M[1].V[2] := Self.M[2].V[1];
  Result.M[1].V[3] := Self.M[3].V[1];
  Result.M[2].V[0] := Self.M[0].V[2];
  Result.M[2].V[1] := Self.M[1].V[2];
  Result.M[2].V[2] := Self.M[2].V[2];
  Result.M[2].V[3] := Self.M[3].V[2];
  Result.M[3].V[0] := Self.M[0].V[3];
  Result.M[3].V[1] := Self.M[1].V[3];
  Result.M[3].V[2] := Self.M[2].V[3];
  Result.M[3].V[3] := Self.M[3].V[3];
end;

function TMatrix3D.EyePosition: TPoint3D;
type
  TMatrix3DArray = array [0 .. 15] of Single;
begin
  Result.X := - TMatrix3DArray(Self)[0] * TMatrix3DArray(Self)[12] - TMatrix3DArray(Self)[1] * TMatrix3DArray(Self)[13]
    - TMatrix3DArray(Self)[2] * TMatrix3DArray(Self)[14];
  Result.Y := - TMatrix3DArray(Self)[4] * TMatrix3DArray(Self)[12] - TMatrix3DArray(Self)[5] * TMatrix3DArray(Self)[13]
    - TMatrix3DArray(Self)[6] * TMatrix3DArray(Self)[14];
  Result.Z := - TMatrix3DArray(Self)[8] * TMatrix3DArray(Self)[12] - TMatrix3DArray(Self)[9] * TMatrix3DArray(Self)[13]
    - TMatrix3DArray(Self)[10] * TMatrix3DArray(Self)[14];
end;

function TMatrix3D.DetInternal(const a1, a2, a3, b1, b2, b3, c1, c2, c3: Single): Single;
begin
  Result := a1 * (b2 * c3 - b3 * c2) - b1 * (a2 * c3 - a3 * c2) + c1 * (a2 * b3 - a3 * b2);
end;

function TMatrix3D.Determinant: Single;
begin
  Result :=
    Self.M[0].V[0] * DetInternal(Self.M[1].V[1], Self.M[2].V[1], Self.M[3].V[1], Self.M[1].V[2],
    Self.M[2].V[2], Self.M[3].V[2], Self.M[1].V[3], Self.M[2].V[3], Self.M[3].V[3])
    - Self.M[0].V[1] * DetInternal(Self.M[1].V[0], Self.M[2].V[0], Self.M[3].V[0], Self.M[1].V[2], Self.M[2].V[2],
    Self.M[3].V[2], Self.M[1].V[3], Self.M[2].V[3], Self.M[3].V[3])
    + Self.M[0].V[2] * DetInternal(Self.M[1].V[0], Self.M[2].V[0], Self.M[3].V[0], Self.M[1].V[1], Self.M[2].V[1],
    Self.M[3].V[1], Self.M[1].V[3], Self.M[2].V[3], Self.M[3].V[3])
    - Self.M[0].V[3] * DetInternal(Self.M[1].V[0], Self.M[2].V[0], Self.M[3].V[0], Self.M[1].V[1], Self.M[2].V[1],
    Self.M[3].V[1], Self.M[1].V[2], Self.M[2].V[2], Self.M[3].V[2]);
end;

function TMatrix3D.Adjoint: TMatrix3D;
var
  a1, a2, a3, a4, b1, b2, b3, b4, c1, c2, c3, c4, d1, d2, d3, d4: Single;
begin
  a1 := Self.M[0].V[0];
  b1 := Self.M[0].V[1];
  c1 := Self.M[0].V[2];
  d1 := Self.M[0].V[3];
  a2 := Self.M[1].V[0];
  b2 := Self.M[1].V[1];
  c2 := Self.M[1].V[2];
  d2 := Self.M[1].V[3];
  a3 := Self.M[2].V[0];
  b3 := Self.M[2].V[1];
  c3 := Self.M[2].V[2];
  d3 := Self.M[2].V[3];
  a4 := Self.M[3].V[0];
  b4 := Self.M[3].V[1];
  c4 := Self.M[3].V[2];
  d4 := Self.M[3].V[3];

  Result.M[0].V[0] := DetInternal(b2, b3, b4, c2, c3, c4, d2, d3, d4);
  Result.M[1].V[0] := -DetInternal(a2, a3, a4, c2, c3, c4, d2, d3, d4);
  Result.M[2].V[0] := DetInternal(a2, a3, a4, b2, b3, b4, d2, d3, d4);
  Result.M[3].V[0] := -DetInternal(a2, a3, a4, b2, b3, b4, c2, c3, c4);

  Result.M[0].V[1] := -DetInternal(b1, b3, b4, c1, c3, c4, d1, d3, d4);
  Result.M[1].V[1] := DetInternal(a1, a3, a4, c1, c3, c4, d1, d3, d4);
  Result.M[2].V[1] := -DetInternal(a1, a3, a4, b1, b3, b4, d1, d3, d4);
  Result.M[3].V[1] := DetInternal(a1, a3, a4, b1, b3, b4, c1, c3, c4);

  Result.M[0].V[2] := DetInternal(b1, b2, b4, c1, c2, c4, d1, d2, d4);
  Result.M[1].V[2] := -DetInternal(a1, a2, a4, c1, c2, c4, d1, d2, d4);
  Result.M[2].V[2] := DetInternal(a1, a2, a4, b1, b2, b4, d1, d2, d4);
  Result.M[3].V[2] := -DetInternal(a1, a2, a4, b1, b2, b4, c1, c2, c4);

  Result.M[0].V[3] := -DetInternal(b1, b2, b3, c1, c2, c3, d1, d2, d3);
  Result.M[1].V[3] := DetInternal(a1, a2, a3, c1, c2, c3, d1, d2, d3);
  Result.M[2].V[3] := -DetInternal(a1, a2, a3, b1, b2, b3, d1, d2, d3);
  Result.M[3].V[3] := DetInternal(a1, a2, a3, b1, b2, b3, c1, c2, c3);
end;

function TMatrix3D.Inverse: TMatrix3D;
const
  DefaultValue: TMatrix3D = (m11: 1.0; m12: 0.0; m13: 0.0; m14: 0.0; m21: 0.0; m22: 1.0; m23: 0.0; m24: 0.0; m31: 0.0;
    m32: 0.0; m33: 1.0; m34: 0.0; m41: 0.0; m42: 0.0; m43: 0.0; m44: 1.0;);
var
  Det: Single;
begin
  Det := Self.Determinant;
  if Abs(Det) < Epsilon then
    Result := DefaultValue
  else
    Result := Self.Adjoint.Scale(1 / Det);
end;

{ TQuaternion3D }

constructor TQuaternion3D.Create(const AAxis: TPoint3D; const AAngle: Single);
var
  AxisLen, Sine, Cosine: Single;
begin
  AxisLen := AAxis.Length;

  if AxisLen > 0 then
  begin
    SinCosSingle(AAngle / 2, Sine, Cosine);

    Self.RealPart := Cosine;
    Self.ImagPart := AAxis * (Sine / AxisLen);
  end else Self := Identity;
end;

constructor TQuaternion3D.Create(const AYaw, APitch, ARoll: Single);
begin
  Self := TQuaternion3D.Create(Point3D(0, 1, 0), AYaw) * TQuaternion3D.Create(Point3D(1, 0, 0), APitch)
    * TQuaternion3D.Create(Point3D(0, 0, 1), ARoll);
end;

constructor TQuaternion3D.Create(const AMatrix: TMatrix3D);
var
  Trace, S: double;
  NewQuat: TQuaternion3D;
begin
  Trace := AMatrix.m11 + AMatrix.m22 + AMatrix.m33;
  if Trace > EPSILON then
  begin
    S := 0.5 / Sqrt(Trace + 1.0);
    NewQuat.ImagPart.X := (AMatrix.M23 - AMatrix.M32) * S;
    NewQuat.ImagPart.Y := (AMatrix.M31 - AMatrix.M13) * S;
    NewQuat.ImagPart.Z := (AMatrix.M12 - AMatrix.M21) * S;
    NewQuat.RealPart := 0.25 / S;
  end
  else if (AMatrix.M11 > AMatrix.M22) and (AMatrix.M11 > AMatrix.M33) then
  begin
    S := Sqrt(Max(EPSILON, 1 + AMatrix.M11 - AMatrix.M22 - AMatrix.M33)) * 2.0;
    NewQuat.ImagPart.X := 0.25 * S;
    NewQuat.ImagPart.Y := (AMatrix.M12 + AMatrix.M21) / S;
    NewQuat.ImagPart.Z := (AMatrix.M31 + AMatrix.M13) / S;
    NewQuat.RealPart := (AMatrix.M23 - AMatrix.M32) / S;
  end
  else if (AMatrix.M22 > AMatrix.M33) then
  begin
    S := Sqrt(Max(EPSILON, 1 + AMatrix.M22 - AMatrix.M11 - AMatrix.M33)) * 2.0;
    NewQuat.ImagPart.X := (AMatrix.M12 + AMatrix.M21) / S;
    NewQuat.ImagPart.Y := 0.25 * S;
    NewQuat.ImagPart.X := (AMatrix.M23 + AMatrix.M32) / S;
    NewQuat.RealPart := (AMatrix.M31 - AMatrix.M13) / S;
  end else
  begin
    S := Sqrt(Max(EPSILON, 1 + AMatrix.M33 - AMatrix.M11 - AMatrix.M22)) * 2.0;
    NewQuat.ImagPart.X := (AMatrix.M31 + AMatrix.M13) / S;
    NewQuat.ImagPart.Y := (AMatrix.M23 + AMatrix.M32) / S;
    NewQuat.ImagPart.Z := 0.25 * S;
    NewQuat.RealPart := (AMatrix.M12 - AMatrix.M21) / S;
  end;
  Self := NewQuat.Normalize;
end;

class operator TQuaternion3D.Implicit(const AQuaternion: TQuaternion3D): TMatrix3D;
var
  NormQuat: TQuaternion3D;
  xx, xy, xz, xw, yy, yz, yw, zz, zw: Single;
begin
  NormQuat := AQuaternion.Normalize;

{$EXCESSPRECISION OFF}
  xx := NormQuat.ImagPart.X * NormQuat.ImagPart.X;
  xy := NormQuat.ImagPart.X * NormQuat.ImagPart.Y;
  xz := NormQuat.ImagPart.X * NormQuat.ImagPart.Z;
  xw := NormQuat.ImagPart.X * NormQuat.RealPart;
  yy := NormQuat.ImagPart.Y * NormQuat.ImagPart.Y;
  yz := NormQuat.ImagPart.Y * NormQuat.ImagPart.Z;
  yw := NormQuat.ImagPart.Y * NormQuat.RealPart;
  zz := NormQuat.ImagPart.Z * NormQuat.ImagPart.Z;
  zw := NormQuat.ImagPart.Z * NormQuat.RealPart;
{$EXCESSPRECISION ON}

  FillChar(Result, Sizeof(Result), 0);
  Result.M11 := 1 - 2 * (yy + zz);
  Result.M21 := 2 * (xy - zw);
  Result.M31 := 2 * (xz + yw);
  Result.M12 := 2 * (xy + zw);
  Result.M22 := 1 - 2 * (xx + zz);
  Result.M32 := 2 * (yz - xw);
  Result.M13 := 2 * (xz - yw);
  Result.M23 := 2 * (yz + xw);
  Result.M33 := 1 - 2 * (xx + yy);
  Result.M44 := 1;
end;

class operator TQuaternion3D.Multiply(const AQuaternion1, AQuaternion2: TQuaternion3D): TQuaternion3D;
begin
  Result.RealPart := AQuaternion1.RealPart * AQuaternion2.RealPart - AQuaternion1.ImagPart.X * AQuaternion2.ImagPart.X
    - AQuaternion1.ImagPart.Y * AQuaternion2.ImagPart.Y - AQuaternion1.ImagPart.Z * AQuaternion2.ImagPart.Z;
  Result.ImagPart.X := AQuaternion1.RealPart * AQuaternion2.ImagPart.X + AQuaternion2.RealPart * AQuaternion1.ImagPart.X
    + AQuaternion1.ImagPart.Y * AQuaternion2.ImagPart.Z - AQuaternion1.ImagPart.Z * AQuaternion2.ImagPart.Y;
  Result.ImagPart.Y := AQuaternion1.RealPart * AQuaternion2.ImagPart.Y + AQuaternion2.RealPart * AQuaternion1.ImagPart.Y
    + AQuaternion1.ImagPart.Z * AQuaternion2.ImagPart.X - AQuaternion1.ImagPart.X * AQuaternion2.ImagPart.Z;
  Result.ImagPart.Z := AQuaternion1.RealPart * AQuaternion2.ImagPart.Z + AQuaternion2.RealPart * AQuaternion1.ImagPart.Z
    + AQuaternion1.ImagPart.X * AQuaternion2.ImagPart.Y - AQuaternion1.ImagPart.Y * AQuaternion2.ImagPart.X;
end;

function TQuaternion3D.Length: Single;
begin
  Result := Sqrt(Self.ImagPart.DotProduct(Self.ImagPart) + Self.RealPart * Self.RealPart);
end;

function TQuaternion3D.Normalize: TQuaternion3D;
var
  QuatLen, InvLen: Single;
begin
  QuatLen := Self.Length;
  if QuatLen > EPSILON2 then
  begin
{$EXCESSPRECISION OFF}
    InvLen := 1 / QuatLen;
    Result.ImagPart := Self.ImagPart * InvLen;
    Result.RealPart := Self.RealPart * InvLen;
{$EXCESSPRECISION ON}
  end
  else
    Result := Identity;
end;

function Ceil(const X: Single): Integer;
begin
  Result := Integer(Trunc(X));
  if Frac(X) > 0 then
    Inc(Result);
end;

{ TPoint }
class operator TPoint.Equal(const Lhs, Rhs: TPoint): Boolean;
begin
  Result := (Lhs.X = Rhs.X) and (Lhs.Y = Rhs.Y);
end;

class operator TPoint.NotEqual(const Lhs, Rhs: TPoint): Boolean;
begin
  Result := (Lhs.X <> Rhs.X) or (Lhs.Y <> Rhs.Y);
end;

class operator TPoint.Add(const Lhs, Rhs: TPoint): TPoint;
begin
  Result.X := Lhs.X + Rhs.X;
  Result.Y := Lhs.Y + Rhs.Y;
end;

class operator TPoint.Subtract(const Lhs, Rhs: TPoint): TPoint;
begin
  Result.X := Lhs.X - Rhs.X;
  Result.Y := Lhs.Y - Rhs.Y;
end;

{$IFDEF FALSE}
class operator TPoint.Explicit(const Size: TSize): TPoint;
begin
  Result.X := Size.cx;
  Result.Y := Size.cy;
end;

class operator TPoint.Explicit(const Point: TPoint): TSize;
begin
  Result.cx := Point.X;
  Result.cy := Point.Y;
end;

class operator TPoint.Explicit(const SmallPoint: TSmallPoint): TPoint;
begin
  Result.X := SmallPoint.x;
  Result.Y := SmallPoint.y;
end;

class operator TPoint.Explicit(const Point: TPoint): TSmallPoint;
begin
  Result.x := Point.X;
  Result.y := Point.Y;
end;

class operator TPoint.Implicit(const Size: TSize): TPoint;
begin
  Result.X := Size.cx;
  Result.Y := Size.cy;
end;

class operator TPoint.Implicit(const Point: TPoint): TSize;
begin
  Result.cx := Point.X;
  Result.cy := Point.Y;
end;

{$ENDIF}

class operator TPoint.Implicit(Value: TSmallPoint): TPoint;
begin
  Result.x := Value.x;
  Result.y := Value.y;
end;

class operator TPoint.Explicit(Value: TPoint): TSmallPoint;
begin
  if Value.x < Low(SmallInt) then
    Result.x := Low(SmallInt)
  else if Value.x > High(SmallInt) then
    Result.x := High(SmallInt)
  else
    Result.x := SmallInt(Result.x);

  if Value.y < Low(SmallInt) then
    Result.y := Low(SmallInt)
  else if Value.y > High(SmallInt) then
    Result.y := High(SmallInt)
  else
    Result.y := SmallInt(Result.y);
end;



constructor TPoint.Create(P: TPoint);
begin
  Self.X := p.X;
  Self.Y := p.Y;
end;

constructor TPoint.Create(const X, Y: Integer);
begin
  Self.X := X;
  Self.Y := Y;
end;

function TPoint.Distance(const P2: TPoint): Double;
begin
  // We Add 0.0 int the expression to cast all operations to floating point.
  // This avoids overflow in signed integer operations.
  Result := Sqrt(Sqr(0.0 + Self.X - P2.X) + Sqr(0.0 + Self.Y - P2.Y));
end;

procedure TPoint.SetLocation(const X, Y: Integer);
begin
  Self.X := X;
  Self.Y := Y;
end;

procedure TPoint.SetLocation(const P: TPoint);
begin
  Self := P;
end;

procedure TPoint.Offset(const DX, DY: Integer);
begin
  Inc(Self.X, DX);
  Inc(Self.Y, DY);
end;

procedure TPoint.Offset(const Point: TPoint);
begin
  Self.Offset(Point.X, Point.Y);
end;

function TPoint.Add(const Point: TPoint): TPoint;
begin
  Result.SetLocation(Self.X + Point.X, Self.Y + Point.Y);
end;

function TPoint.Subtract(const Point: TPoint): TPoint;
begin
  Result.SetLocation(Self.X - Point.X, Self.Y - Point.Y);
end;

function TPoint.IsZero: Boolean;
begin
  Result := (X = 0) and (Y = 0);
end;

{ TRect }

class function TRect.Empty: TRect;
begin
  Result := TRect.Create(0,0,0,0);
end;

constructor TRect.Create(const Origin: TPoint);
begin
  Create(Origin.X, Origin.Y, Origin.X, Origin.Y);
end;

constructor TRect.Create(const Origin: TPoint; Width, Height: Integer);
begin
  Create(Origin.X, Origin.Y, Origin.X + Width, Origin.Y + Height);
end;

constructor TRect.Create(const Left, Top, Right, Bottom: Integer);
begin
  Self.Left  := Left;
  Self.Top   := Top;
  Self.Right := Right;
  Self.Bottom:= Bottom;
end;

constructor TRect.Create(const P1, P2: TPoint; Normalize: Boolean);
begin
  Self.TopLeft := P1;
  Self.BottomRight := P2;
  if Normalize then Self.NormalizeRect;
end;

constructor TRect.Create(const R: TRect; Normalize: Boolean);
begin
  Self.TopLeft := R.TopLeft;
  Self.BottomRight := R.BottomRight;
  if Normalize then Self.NormalizeRect;
end;

function TRect.GetLocation: TPoint;
begin
  Result := TopLeft;
end;

procedure TRect.SetWidth(const Value: Integer);
begin
  Self.Right := Self.Left + Value;
end;

function TRect.GetWidth: Integer;
begin
  Result := Self.Right - Self.Left;
end;

procedure TRect.SetHeight(const Value: Integer);
begin
  Self.Bottom := Self.Top + Value;
end;

function TRect.GetHeight: Integer;
begin
  Result := Self.Bottom - Self.Top;
end;

function TRect.GetSize: TSize;
begin
  Result.cx := Width;
  Result.cy := Height;
end;

procedure TRect.SetSize(const Value: TSize);
begin
  Width := Value.cx;
  Height := Value.cy;
end;

class operator TRect.Equal(const Lhs, Rhs: TRect): Boolean;
begin
  Result := (Lhs.Left = Rhs.Left) and (Lhs.Right = Rhs.Right) and
            (Lhs.Top = Rhs.Top) and (Lhs.Bottom = Rhs.Bottom);
end;

class operator TRect.NotEqual(const Lhs, Rhs: TRect): Boolean;
begin
  Result := not(Lhs = Rhs);
end;

class operator TRect.Add(const Lhs, Rhs: TRect): TRect;
begin
  Result := TRect.Union(Lhs, Rhs);
end;

class operator TRect.Multiply(const Lhs, Rhs: TRect): TRect;
begin
  Result := TRect.Intersect(Lhs, Rhs);
end;

procedure TRect.NormalizeRect;
begin
  if Top > Bottom then begin
    Top := Top xor Bottom;
    Bottom := Top xor Bottom;
    Top := Top xor Bottom;
  end;
  if Left > Right then begin
    Left := Left xor Right;
    Right:= Left xor Right;
    Left := Left xor Right;
  end
end;

function TRect.IsEmpty: Boolean;
begin
  Result := IsRectEmpty(Self);
end;

function PtInRect(const Rect: TRect; const P: TPoint): Boolean;
begin
  Result := (P.X >= Rect.Left) and (P.X < Rect.Right) and (P.Y >= Rect.Top)
    and (P.Y < Rect.Bottom);
end;

function TRect.Contains(const PT: TPoint): Boolean;
begin
  Result := PtInRect(self, PT);
end;

function TRect.Contains(const R: TRect): Boolean;
begin
  Result := Contains(R.TopLeft) and Contains(R.BottomRight);
end;

function TRect.IntersectsWith(const R: TRect): Boolean;
begin
  Result := not ( (Self.BottomRight.X < R.TopLeft.X) or
                  (Self.BottomRight.Y < R.TopLeft.Y) or
                  (R.BottomRight.X < Self.TopLeft.X) or
                  (R.BottomRight.Y < Self.TopLeft.Y) );
end;
    
class function TRect.Intersect(const R1: TRect; const R2: TRect): TRect;
begin
  IntersectRect(Result, R1, R2);
end;

procedure TRect.Intersect(const R: TRect);
begin
  Self := Intersect(Self, R);
end;

class function TRect.Union(const R1: TRect; const R2: TRect): TRect;
begin
  UnionRect(Result, R1, R2);
end;

procedure TRect.Union(const R: TRect);
begin
  Self := Union(Self, R);
end;

class function TRect.Union(const Points: Array of TPoint): TRect;
var
  i: Integer;
  TLCorner, BRCorner: TPoint;
begin
  if Length(Points) > 0 then
  begin
    TLCorner := Points[Low(Points)];
    BRCorner := Points[Low(Points)];

    if Length(Points) > 1 then
    begin
      for i := Low(Points) + 1 to High(Points) do
      begin
        if Points[i].X < TLCorner.X then TLCorner.X := Points[i].X;
        if Points[i].X > BRCorner.X then BRCorner.X := Points[i].X;
        if Points[i].Y < TLCorner.Y then TLCorner.Y := Points[i].Y;
        if Points[i].Y > BRCorner.Y then BRCorner.Y := Points[i].Y;
      end;
    end;

    Result := TRect.Create(TLCorner, BRCorner);

  end
  else begin
    Result := TRect.Empty;
  end;
end;


procedure TRect.Offset(const DX, DY: Integer);
begin
  TopLeft.Offset(DX, DY);
  BottomRight.Offset(DX, DY);
end;

procedure TRect.Offset(const Point: TPoint);
begin
  TopLeft.Offset(Point);
  BottomRight.Offset(Point);
end;

// sets new origin
procedure TRect.SetLocation(const X, Y: Integer);
begin
  Offset(X - Left, Y - Top);
end;

procedure TRect.SetLocation(const Point: TPoint);
begin
  Offset(Point.X - Left, Point.Y - Top);
end;

procedure TRect.Inflate(const DX, DY: Integer);
begin
  TopLeft.Offset(-DX, -DY);
  BottomRight.Offset(DX, DY);
end;

procedure TRect.Inflate(const DL, DT, DR, DB: Integer);
begin
  TopLeft.Offset(-DL, -DT);
  BottomRight.Offset(DR, DB);
end;

function TRect.CenterPoint: TPoint;
begin
  Result.X := (Right - Left) div 2 + Left;
  Result.Y := (Bottom - Top) div 2 + Top;
end;

function TRect.SplitRect(SplitType: TSplitRectType; Size: Integer): TRect;
begin
  Result := Self;

  case SplitType of
    srLeft:
      Result.Right := Self.Left + Size;
    srRight:
      Result.Left := Self.Right - Size;
    srTop:
      Result.Bottom := Self.Top + Size;
    srBottom:
      Result.Top := Self.Bottom - Size;
  end;
end;

function TRect.SplitRect(SplitType: TSplitRectType; Percent: Double): TRect;
begin
  Result := Self;
  case SplitType of
    srLeft:
      Result.Right := Self.Left + Trunc(Percent * Self.Width);
    srRight:
      Result.Left := Self.Right - Trunc(Percent * Self.Width);
    srTop:
      Result.Bottom := Self.Top + Trunc(Percent * Self.Height);
    srBottom:
      Result.Top := Self.Bottom - Trunc(Percent * Self.Height);
  end;
end;

{}

function SplitRect(const Rect: TRect; SplitType: TSplitRectType; Size: Integer): TRect;
begin
  Result := Rect;
  case SplitType of
    srLeft:
      Result.Right := Rect.Left + Size;
    srRight:
      Result.Left := Rect.Right - Size;
    srTop:
      Result.Bottom := Rect.Top + Size;
    srBottom:
      Result.Top := Rect.Bottom - Size;
  end;
end;

function SplitRect(const Rect: TRect; SplitType: TSplitRectType; Percent: Double): TRect;
begin
  Result := Rect;
  case SplitType of
    srLeft:
      Result.Right := Rect.Left + Trunc(Percent * RectWidth(Rect));
    srRight:
      Result.Left := Rect.Right - Trunc(Percent * RectWidth(Rect));
    srTop:
      Result.Bottom := Rect.Top + Trunc(Percent * RectHeight(Rect));
    srBottom:
      Result.Top := Rect.Bottom - Trunc(Percent * RectHeight(Rect));
  end;
end;

function CenteredRect(const SourceRect: TRect; const CenteredRect: TRect): TRect;
var
  Width, Height: Integer;
  X, Y: Integer;
begin
  Width := RectWidth(CenteredRect);
  Height := RectHeight(CenteredRect);
  X := (SourceRect.Right + SourceRect.Left) div 2;
  Y := (SourceRect.Top + SourceRect.Bottom) div 2;
  Result := Rect(X - Width div 2, Y - Height div 2, X + (Width + 1) div 2, Y + (Height + 1) div 2);
end;

function MinPoint(const P1, P2: TPointF): TPointF;
begin
  Result := P1;
  if (P2.Y < P1.Y) or ((P2.Y = P1.Y) and (P2.X < P1.X)) then
    Result := P2;
end;

function MinPoint(const P1, P2: TPoint): TPoint;
begin
  Result := P1;
  if (P2.Y < P1.Y) or ((P2.Y = P1.Y) and (P2.X < P1.X)) then
    Result := P2;
end;

procedure MultiplyRect(var R: TRectF; const DX, DY: Single);
begin
  R.Left := R.Left * dX;
  R.Right := R.Right * dX;
  R.Top := R.Top * dY;
  R.Bottom := R.Bottom * dY;
end;

function NormalizeRectF(const Pts: array of TPointF): TRectF;
var
  Pt: TPointF;
begin
  Result.Left := $F000;
  Result.Top := $F000;
  Result.Right := -$F000;
  Result.Bottom := -$F000;
  for Pt in Pts do
  begin
    if Pt.X < Result.Left then
      Result.Left := Pt.X;
    if Pt.Y < Result.Top then
      Result.Top := Pt.Y;
    if Pt.X > Result.Right then
      Result.Right := Pt.X;
    if Pt.Y > Result.Bottom then
      Result.Bottom := Pt.Y;
  end;
end;

function NormalizeRect(const ARect: TRectF): TRectF;
begin
  Result := NormalizeRectF([TPointF.Create(ARect.Left, ARect.Top), TPointF.Create(ARect.Right, ARect.Top),
    TPointF.Create(ARect.Right, ARect.Bottom), TPointF.Create(ARect.Left, ARect.Bottom)]);
end;

function ScalePoint(const P: TPointF; dX, dY: Single): TPointF;
begin
{$EXCESSPRECISION OFF}
  Result.X := P.X * dX;
  Result.Y := P.Y * dY;
{$EXCESSPRECISION ON}
end;

function ScalePoint(const P: TPoint; dX, dY: Single): TPoint;
begin
  Result.X := Round(P.X * dX);
  Result.Y := Round(P.Y * dY);
end;

function EqualRect(const R1, R2: TRect): Boolean;
begin
  Result := (R1.Left = R2.Left) and (R1.Right = R2.Right) and
    (R1.Top = R2.Top) and (R1.Bottom = R2.Bottom);
end;

function EqualRect(const R1, R2: TRectF): Boolean;
begin
  Result := (R1.Left = R2.Left) and (R1.Right = R2.Right) and
    (R1.Top = R2.Top) and (R1.Bottom = R2.Bottom);
end;

function Rect(Left, Top, Right, Bottom: Integer): TRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Bottom := Bottom;
  Result.Right := Right;
end;

function RectF(Left, Top, Right, Bottom: Single): TRectF;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Bottom := Bottom;
  Result.Right := Right;
end;

function Vector(const X, Y: Single; const W: Single = 1.0): TVector;
begin
  Result.X := X;
  Result.Y := Y;
  Result.W := W;
end;

function Vector(const P: TPointF; const W: Single = 1.0): TVector;
begin
  Result.X := P.X;
  Result.Y := P.Y;
  Result.W := W;
end;

function Vector3D(const X, Y, Z: Single; const W: Single = 1.0): TVector3D; overload;
begin
  Result := TVector3D.Create(X,Y,Z,W);
end;

function Vector3D(const P: TPoint3D; const W: Single = 1.0): TVector3D; overload;
begin
  Result := TVector3D.Create(P,W);
end;

function Point3D(const X, Y, Z: Single): TPoint3D; overload;
begin
  Result := TPoint3D.Create(X,Y,Z);
end;

function Point3D(const AVector3D: TVector3D; const ATransform: Boolean): TPoint3D; overload;
begin
  Result := AVector3D.ToPoint3D(ATransform);
end;

function RectWidth(const Rect: TRect): Integer;
begin
  Result := Rect.Right - Rect.Left;
end;

function RectHeight(const Rect: TRect): Integer;
begin
  Result := Rect.Bottom - Rect.Top;
end;

function RectWidth(const Rect: TRectF): Single;
begin
  Result := Rect.Right - Rect.Left;
end;

function RectHeight(const Rect: TRectF): Single;
begin
  Result := Rect.Bottom - Rect.Top;
end;

function RectCenter(var R: TRect; const Bounds: TRect): TRect;
begin
  OffsetRect(R, -R.Left, -R.Top);
  OffsetRect(R, (RectWidth(Bounds) - RectWidth(R)) div 2, (RectHeight(Bounds) - RectHeight(R)) div 2);
  OffsetRect(R, Bounds.Left, Bounds.Top);
  Result := R;
end;

function RectCenter(var R: TRectF; const Bounds: TRectF): TRectF;
begin
  OffsetRect(R, -R.Left, -R.Top);
  OffsetRect(R, Round((RectWidth(Bounds) - RectWidth(R)) / 2), Round((RectHeight(Bounds) - RectHeight(R)) / 2));
  OffsetRect(R, Bounds.Left, Bounds.Top);
  Result := R;
end;

function Point(X, Y: Integer): TPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function PointF(X, Y: Single): TPointF;
begin
  Result.X := X;
  Result.Y := Y;
end;

function PointF(const V: TVector): TPointF;
begin
  Result.X := V.X;
  Result.Y := V.Y;
end;

function SmallPoint(X, Y: Integer): TSmallPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function SmallPoint(XY: LongWord): TSmallPoint;
begin
  Result.X := SmallInt(XY and $0000FFFF);
  Result.Y := SmallInt(XY shr 16);
end;

function PtInCircle(const Point, Center: TPoint; Radius: Integer): Boolean;
begin
  if Radius > 0 then
  begin
    Result := Sqr((Point.X - Center.X) / Radius) +
      Sqr((Point.Y - Center.Y) / Radius) <= 1;
  end
  else
  begin
    Result := False;
  end;
end;

function IntersectRect(const Rect1, Rect2: TRect): Boolean;
begin
  Result := (Rect1.Left <= Rect2.Right) and (Rect1.Right >= Rect2.Left) and (Rect1.Top <= Rect2.Bottom) and
    (Rect1.Bottom >= Rect2.Top);
end;

function IntersectRect(out Rect: TRect; const R1, R2: TRect): Boolean;
var
  tmpRect: TRect;
begin
  tmpRect := R1;
  if R2.Left > R1.Left then tmpRect.Left := R2.Left;
  if R2.Top > R1.Top then tmpRect.Top := R2.Top;
  if R2.Right < R1.Right then tmpRect.Right := R2.Right;
  if R2.Bottom < R1.Bottom then tmpRect.Bottom := R2.Bottom;
  Result := not IsRectEmpty(tmpRect);
  if not Result then FillChar(tmpRect, SizeOf(Rect), 0);
  Rect := tmpRect;
end;

function IntersectRect(const Rect1, Rect2: TRectF): Boolean;
begin
  Result := (Rect1.Left <= Rect2.Right) and (Rect1.Right >= Rect2.Left) and (Rect1.Top <= Rect2.Bottom) and
    (Rect1.Bottom >= Rect2.Top);
end;

function IntersectRect(out Rect: TRectF; const R1, R2: TRectF): Boolean;
var
  tmpRect: TRectF;
begin
  tmpRect := R1;
  if R2.Left > R1.Left then tmpRect.Left := R2.Left;
  if R2.Top > R1.Top then tmpRect.Top := R2.Top;
  if R2.Right < R1.Right then tmpRect.Right := R2.Right;
  if R2.Bottom < R1.Bottom then tmpRect.Bottom := R2.Bottom;
  Result := not IsRectEmpty(tmpRect);
  if not Result then FillChar(tmpRect, SizeOf(Rect), 0);
  Rect := tmpRect;
end;

function UnionRect(out Rect: TRect; const R1, R2: TRect): Boolean;
var
  tmpRect: TRect;
begin
  tmpRect := R1;
  if not IsRectEmpty(R2) then
  begin
    if R2.Left < R1.Left then tmpRect.Left := R2.Left;
    if R2.Top < R1.Top then tmpRect.Top := R2.Top;
    if R2.Right > R1.Right then tmpRect.Right := R2.Right;
    if R2.Bottom > R1.Bottom then tmpRect.Bottom := R2.Bottom;
  end;
  Result := not IsRectEmpty(tmpRect);
  if not Result then FillChar(tmpRect, SizeOf(Rect), 0);
  Rect := tmpRect;
end;

function UnionRect(out Rect: TRectF; const R1, R2: TRectF): Boolean;
var
  tmpRect: TRectF;
begin
  tmpRect := R1;
  if not IsRectEmpty(R2) then
  begin
    if R2.Left < R1.Left then tmpRect.Left := R2.Left;
    if R2.Top < R1.Top then tmpRect.Top := R2.Top;
    if R2.Right > R1.Right then tmpRect.Right := R2.Right;
    if R2.Bottom > R1.Bottom then tmpRect.Bottom := R2.Bottom;
  end;
  Result := not IsRectEmpty(tmpRect);
  if not Result then FillChar(tmpRect, SizeOf(Rect), 0);
  Rect := tmpRect;
end;

function UnionRect(const ARect1, ARect2: TRect): TRect;
begin
  UnionRect(Result, ARect1, ARect2);
end;

function UnionRect(const ARect1, ARect2: TRectF): TRectF;
begin
  UnionRect(Result, ARect1, ARect2);
end;

procedure InflateRect(var R: TRectF; const DX, DY: Single);
begin
{$EXCESSPRECISION OFF}
  R.Left := R.Left - DX;
  R.Right := R.Right + DX;
  R.Top := R.Top - DY;
  R.Bottom := R.Bottom + DY;
{$EXCESSPRECISION ON}
end;

procedure InflateRect(var R: TRect; const DX, DY: Integer);
begin
  R.Left := R.Left - DX;
  R.Right := R.Right + DX;
  R.Top := R.Top - DY;
  R.Bottom := R.Bottom + DY;
end;

function IsRectEmpty(const Rect: TRect): Boolean;
begin
  Result := (Rect.Right <= Rect.Left) or (Rect.Bottom <= Rect.Top);
end;

function IsRectEmpty(const Rect: TRectF): Boolean;
begin
  Result := (Rect.Right <= Rect.Left) or (Rect.Bottom <= Rect.Top);
end;

function OffsetRect(var R: TRect; DX, DY: Integer): Boolean;
begin
  if @R <> nil then // Test to increase compatiblity with Windows
  begin
    Inc(R.Left, DX);
    Inc(R.Right, DX);
    Inc(R.Top, DY);
    Inc(R.Bottom, DY);
    Result := True;
  end
  else
    Result := False;
end;

function OffsetRect(var R: TRectF; DX, DY: Single): Boolean;
begin
{$EXCESSPRECISION OFF}
  if @R <> nil then // Test to increase compatiblity with Windows
  begin
    R.Left := R.Left + DX;
    R.Right := R.Right + DX;
    R.Top := R.Top + DY;
    R.Bottom := R.Bottom + DY;
    Result := True;
  end
  else
    Result := False;
{$EXCESSPRECISION ON}
end;

function Bounds(ALeft, ATop, AWidth, AHeight: Integer): TRect;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Right := ALeft + AWidth;
  Result.Bottom :=  ATop + AHeight;
end;

function CenterPoint(const Rect: TRect): TPoint;
begin
  Result.X := (Rect.Right - Rect.Left) div 2 + Rect.Left;
  Result.Y := (Rect.Bottom - Rect.Top) div 2 + Rect.Top;
end;

function IntersectRectF(out Rect: TRectF; const R1, R2: TRectF): Boolean;
var
  tmpRect: TRectF;
begin
  tmpRect := R1;
  if R2.Left > R1.Left then tmpRect.Left := R2.Left;
  if R2.Top > R1.Top then tmpRect.Top := R2.Top;
  if R2.Right < R1.Right then tmpRect.Right := R2.Right;
  if R2.Bottom < R1.Bottom then tmpRect.Bottom := R2.Bottom;
  Result := not tmpRect.IsEmpty;
  if not Result then begin
    tmpRect.Top := 0.0;
    tmpRect.Bottom := 0.0;
    tmpRect.Left := 0.0;
    tmpRect.Right := 0.0;
  end;
  Rect := tmpRect;
end;

function UnionRectF(out Rect: TRectF; const R1, R2: TRectF): Boolean;
var
  tmpRect: TRectF;
begin
  tmpRect := R1;
  if not R2.IsEmpty then
  begin
    if R2.Left < R1.Left then tmpRect.Left := R2.Left;
    if R2.Top < R1.Top then tmpRect.Top := R2.Top;
    if R2.Right > R1.Right then tmpRect.Right := R2.Right;
    if R2.Bottom > R1.Bottom then tmpRect.Bottom := R2.Bottom;
  end;
  Result := not tmpRect.IsEmpty;
  if not Result then begin
    tmpRect.Top :=0.0;
    tmpRect.Bottom := 0.0;
    tmpRect.Left := 0.0;
    tmpRect.Right := 0.0;
  end;
  Rect := tmpRect;
end;

{ TPointF }

class function TPointF.Create(const AX, AY: Single): TPointF;
begin
  Result.X := AX;
  Result.Y := AY;
end;

class function TPointF.Create(const APoint: TPoint): TPointF;
begin
  Result.X := APoint.X;
  Result.Y := APoint.Y;
end;

class operator TPointF.Add(const APoint1, APoint2: TPointF): TPointF;
begin
{$EXCESSPRECISION OFF}
  Result.X := APoint1.X + APoint2.X;
  Result.Y := APoint1.Y + APoint2.Y;
{$EXCESSPRECISION ON}
end;

class operator TPointF.Subtract(const APoint1, APoint2: TPointF): TPointF;
begin
{$EXCESSPRECISION OFF}
  Result.X := APoint1.X - APoint2.X;
  Result.Y := APoint1.Y - APoint2.Y;
{$EXCESSPRECISION ON}
end;

class operator TPointF.Equal(const APoint1, APoint2: TPointF): Boolean;
begin
  Result := SameValue(APoint1.X, APoint2.X) and SameValue(APoint1.Y, APoint2.Y);
end;

class operator TPointF.NotEqual(const APoint1, APoint2: TPointF): Boolean;
begin
  Result := not (APoint1 = APoint2);
end;

class operator TPointF.Implicit(const APoint: TPoint): TPointF;
begin
  Result.X := APoint.X;
  Result.Y := APoint.Y;
end;

class operator TPointF.Negative(const APoint: TPointF): TPointF;
begin
  Result.X := - APoint.X;
  Result.Y := - APoint.Y;
end;

class operator TPointF.Multiply(const APoint1, APoint2: TPointF): TPointF;
begin
{$EXCESSPRECISION OFF}
  Result.X := APoint1.X * APoint2.X;
  Result.Y := APoint1.Y * APoint2.Y;
{$EXCESSPRECISION ON}
end;

class operator TPointF.Multiply(const APoint: TPointF; const AFactor: Single): TPointF;
begin
{$EXCESSPRECISION OFF}
  Result.X := APoint.X * AFactor;
  Result.Y := APoint.Y * AFactor;
{$EXCESSPRECISION ON}
end;

class operator TPointF.Multiply(const AFactor: Single; const APoint: TPointF): TPointF;
begin
{$EXCESSPRECISION OFF}
  Result.X := AFactor * APoint.X;
  Result.Y := AFactor * APoint.Y;
{$EXCESSPRECISION ON}
end;

class operator TPointF.Divide(const APoint: TPointF; const AFactor: Single): TPointF;
var
  InvFactor: Single;
begin
{$EXCESSPRECISION OFF}
  if AFactor <> 0 then
  begin
    InvFactor := 1 / AFactor;

    Result.X := APoint.X * InvFactor;
    Result.Y := APoint.Y * InvFactor;
  end else
    Result := APoint;
{$EXCESSPRECISION ON}
end;

function TPointF.Add(const Point: TPointF): TPointF;
begin
{$EXCESSPRECISION OFF}
  Result.X := Self.X + Point.X;
  Result.Y := Self.Y + Point.Y;
{$EXCESSPRECISION ON}
end;

function TPointF.Add(const Point: TPoint): TPointF;
begin
{$EXCESSPRECISION OFF}
  Result.X := Self.X + Point.X;
  Result.Y := Self.Y + Point.Y;
{$EXCESSPRECISION ON}
end;

function TPointF.Distance(const APoint: TPointF): Single;
begin
{$EXCESSPRECISION OFF}
  Result := (APoint - Self).Length;
{$EXCESSPRECISION ON}
end;

function TPointF.CrossProduct(const APoint: TPointF): Single;
begin
  Result := Self.X * APoint.Y - Self.Y * APoint.X;
end;

function TPointF.DotProduct(const APoint: TPointF): Single;
begin
  Result := (Self.X * APoint.X) + (Self.Y * APoint.Y);
end;

procedure TPointF.Offset(const APoint: TPointF);
begin
  Self := Self + APoint;
end;

procedure TPointF.Offset(const ADeltaX, ADeltaY: Single);
begin
  Self.Offset(TPointF.Create(ADeltaX, ADeltaY));
end;

procedure TPointF.Offset(const APoint: TPoint);
begin
  Self.Offset(TPointF.Create(APoint));
end;

function TPointF.IsZero: Boolean;
begin
  Result := SameValue(X, 0.0) and SameValue(Y, 0.0);
end;

function TPointF.Ceiling: TPoint;
begin
  Result.X := Ceil(X);
  Result.Y := Ceil(Y);
end;

function TPointF.Truncate: TPoint;
begin
  Result.X := Trunc(X);
  Result.Y := Trunc(Y);
end;

function TPointF.Round: TPoint;
begin
  Result.X := System.Round(X);
  Result.Y := System.Round(Y);
end;

function TPointF.Scale(const AFactor: Single): TPointF;
begin
  Result := Self * AFactor;
end;

function TPointF.Normalize: TPointF;
var
  Len: Single;
begin
  Len := Sqrt(Sqr(X) + Sqr(Y));

  if (Len <> 0.0) then
  begin
    Result.X := X / Len;
    Result.Y := Y / Len;
  end
  else
    Result := Self;
end;

function TPointF.Length: Single;
begin
  Result := Sqrt(Sqr(X) + Sqr(Y));
end;

procedure TPointF.SetLocation(const P: TPoint);
begin
  Self.X := P.X;
  Self.Y := P.Y;
end;

procedure TPointF.SetLocation(const P: TPointF);
begin
  Self := P;
end;

procedure TPointF.SetLocation(const X, Y: Single);
begin
  Self.X := X;
  Self.Y := Y;
end;

function TPointF.Subtract(const Point: TPointF): TPointF;
begin
{$EXCESSPRECISION OFF}
  Result.X := Self.X - Point.X;
  Result.Y := Self.Y - Point.Y;
{$EXCESSPRECISION ON}
end;

function TPointF.Subtract(const Point: TPoint): TPointF;
begin
{$EXCESSPRECISION OFF}
  Result.X := Self.X - Point.X;
  Result.Y := Self.Y - Point.Y;
{$EXCESSPRECISION ON}
end;

function TPointF.Rotate(const AAngle: Single): TPointF;
begin
  Result := Self * TMatrix.CreateRotation(AAngle);
end;

function TPointF.Reflect(const APoint: TPointF): TPointF;
begin
  Result := Self + APoint * (-2 * Self.DotProduct(APoint));
end;

function TPointF.MidPoint(const APoint: TPointF): TPointF;
begin
  Result.X := (Self.X + APoint.X) / 2;
  Result.Y := (Self.Y + APoint.Y) / 2;
end;

function TPointF.AngleCosine(const APoint: TPointF): Single;
begin
  Result := Self.Length * APoint.Length;

  if Abs(Result) > Epsilon then
    Result := Self.DotProduct(APoint) / Result
  else
    Result := Self.DotProduct(APoint) / Epsilon;

  Result := Max(Min(Result, 1), -1);
end;

{ TRectF }
constructor TRectF.Create(const R: TRectF; Normalize: Boolean);
begin
  Self := R;
  if Normalize then NormalizeRect;
end;

constructor TRectF.Create(const R: TRect; Normalize: Boolean);
begin
  Self.Left := R.Left;
  Self.Top  := R.Top;
  Self.Right := R.Right;
  Self.Bottom := R.Bottom;
  if Normalize then NormalizeRect;
end;


constructor TRectF.Create(const Origin: TPointF);
begin
  TopLeft := Origin;
  BottomRight := Origin;
end;

constructor TRectF.Create(const Left, Top, Right, Bottom: Single);
begin
  Self.Left := Left; Self.Top := Top;
  Self.Right := Right; Self.Bottom := Bottom;
end;

constructor TRectF.Create(const P1, P2: TPointF; Normalize: Boolean);
begin
  Self.TopLeft := P1;
  Self.BottomRight := P2;
  if Normalize then NormalizeRect;
end;

constructor TRectF.Create(const Origin: TPointF; const Width, Height: Single);
begin
  Self.TopLeft := Origin;
  Self.Width := Width;
  Self.Height := Height;
end;

class operator TRectF.Equal(const Lhs, Rhs: TRectF): Boolean;
begin
  Result := (Lhs.TopLeft = Rhs.TopLeft) and
            (Lhs.BottomRight = Rhs.BottomRight);
end;

function TRectF.Fit(BoundsRect: TRectF): Single;
var
  ratio: Single;
begin
  Result := 1;
  if BoundsRect.Width * BoundsRect.Height = 0 then
    Exit;

  if (Self.Width / BoundsRect.Width) > (Self.Height / BoundsRect.Height) then
    ratio := Self.Width / BoundsRect.Width
  else
    ratio := Self.Height / BoundsRect.Height;

  if ratio < 1 then
  begin
    Self := RectF(0, 0, Self.Width, Self.Height);
  end
  else
  begin
    Self := RectF(0, 0, Self.Width / ratio, Self.Height / ratio);
  end;

  Result := ratio;
  RectCenter(Self, BoundsRect);
end;

class operator TRectF.NotEqual(const Lhs, Rhs: TRectF): Boolean;
begin
  Result := not (Lhs = Rhs);
end;

class operator TRectF.Add(const Lhs, Rhs: TRectF): TRectF;
begin
  Result := TRectF.Union(Lhs, Rhs);
end;

class operator TRectF.Multiply(const Lhs, Rhs: TRectF): TRectF;
begin
  Result := TRectF.Intersect(Lhs, Rhs);
end;

function TRectF.CenterPoint: TPointF;
begin
{$EXCESSPRECISION OFF}
  Result.X := (Right - Left)/2.0 + Left;
  Result.Y := (Bottom - Top)/2.0 + Top;
{$EXCESSPRECISION ON}
end;

function TRectF.Contains(const R: TRectF): Boolean;
begin
  Result := Contains(R.TopLeft) and Contains(R.BottomRight);
end;

function PtInRect(const Rect: TRectF; const P: TPointF): Boolean;
begin
  Result := (P.X >= Rect.Left) and (P.X < Rect.Right) and (P.Y >= Rect.Top)
    and (P.Y < Rect.Bottom);
end;

function TRectF.Contains(const Pt: TPointF): Boolean;
begin
  Result := ((Pt.X > Self.Left) or SameValue(Pt.X, Self.Left)) and
            (Pt.X < Self.Right) and
            ((Pt.Y > Self.Top) or SameValue(Pt.Y, Self.Top)) and
            (Pt.Y < Self.Bottom);
end;

class function TRectF.Empty: TRectF;
begin
  Result := TRectF.Create(0,0,0,0);
end;

function TRectF.GetHeight: Single;
begin
{$EXCESSPRECISION OFF}
  Result := Self.Bottom - Self.Top;
{$EXCESSPRECISION ON}
end;

procedure TRectF.SetHeight(const Value: Single);
begin
{$EXCESSPRECISION OFF}
  Self.Bottom := Self.Top + Value;
{$EXCESSPRECISION ON}
end;

function TRectF.GetWidth: Single;
begin
{$EXCESSPRECISION OFF}
  Result := Self.Right - Self.Left;
{$EXCESSPRECISION ON}
end;

procedure TRectF.SetWidth(const Value: Single);
begin
{$EXCESSPRECISION OFF}
  Self.Right := Self.Left + Value;
{$EXCESSPRECISION ON}
end;

function TRectF.GetSize: TSizeF;
begin
  Result.cx := Width;
  Result.cy := Height;
end;

procedure TRectF.SetSize(const Value: TSizeF);
begin
  Width := Value.cx;
  Height := Value.cy;
end;

procedure TRectF.Inflate(const DX, DY: Single);
begin
{$EXCESSPRECISION OFF}
  TopLeft.Offset(-DX, -DY);
  BottomRight.Offset(DX, DY);
{$EXCESSPRECISION ON}
end;

procedure TRectF.Inflate(const DL, DT, DR, DB: Single);
begin
{$EXCESSPRECISION OFF}
  TopLeft.Offset(-DL, -DT);
  BottomRight.Offset(DR, DB);
{$EXCESSPRECISION ON}
end;

procedure TRectF.Offset(const Point: TPointF);
begin
  TopLeft.Offset(Point);
  BottomRight.Offset(Point);
end;

procedure TRectF.Offset(const DX, DY: Single);
begin
  TopLeft.Offset(DX, DY);
  BottomRight.Offset(DX, DY);
end;

function TRectF.GetLocation: TPointF;
begin
  Result := TopLeft;
end;

procedure TRectF.SetLocation(const Point: TPointF);
begin
{$EXCESSPRECISION OFF}
  Offset(Point.X - Left, Point.Y - Top);
{$EXCESSPRECISION ON}
end;

procedure TRectF.SetLocation(const X, Y: Single);
begin
{$EXCESSPRECISION OFF}
  Offset(X - Left, Y - Top);
{$EXCESSPRECISION ON}
end;

function TRectF.IntersectsWith(const R: TRectF): Boolean;
begin
  Result := not ( (Self.BottomRight.X < R.TopLeft.X) or
                  (Self.BottomRight.Y < R.TopLeft.Y) or
                  (R.BottomRight.X < Self.TopLeft.X) or
                  (R.BottomRight.Y < Self.TopLeft.Y) );
end;

function TRectF.IsEmpty: Boolean;
begin
  Result := (Right < Left) or SameValue(Right, Left)
         or (Bottom < Top) or SameValue(Bottom, Top);
end;

procedure TRectF.NormalizeRect;
var
  temp: Single;
begin
  if Top > Bottom then begin
    temp := Top;
    Top := Bottom;
    Bottom := temp;
  end;
  if Left > Right then begin
    temp := Left;
    Left := Right;
    Right := temp;
  end
end;

function TRectF.Ceiling: TRect;
begin
  Result.TopLeft := TopLeft.Ceiling;
  Result.BottomRight := BottomRight.Ceiling;
end;

function TRectF.Truncate: TRect;
begin
  Result.TopLeft := TopLeft.Truncate;
  Result.BottomRight := BottomRight.Truncate;
end;

function TRectF.Round: TRect;
begin
  Result.TopLeft := TopLeft.Round;
  Result.BottomRight := BottomRight.Round;
end;

class function TRectF.Intersect(const R1, R2: TRectF): TRectF;
begin
  IntersectRectF(Result, R1, R2);
end;

procedure TRectF.Intersect(const R: TRectF);
begin
  Self := Intersect(Self, R);
end;

class function TRectF.Union(const R1, R2: TRectF): TRectF;
begin
  UnionRectF(Result, R1, R2);
end;

procedure TRectF.Union(const R: TRectF);
begin
  Self := TRectF.Union(Self, R);
end;

class function TRectF.Union(const Points: Array of TPointF): TRectF;
var
  I: Integer;
  TLCorner, BRCorner: TPointF;
begin
  if Length(Points) > 0 then
  begin
    TLCorner := Points[Low(Points)];
    BRCorner := Points[Low(Points)];

    if Length(Points) > 1 then
    begin 
      for I := Low(Points) + 1 to High(Points) do
      begin
        if Points[I].X < TLCorner.X then TLCorner.X := Points[I].X;
        if Points[I].X > BRCorner.X then BRCorner.X := Points[I].X;
        if Points[I].Y < TLCorner.Y then TLCorner.Y := Points[I].Y;
        if Points[I].Y > BRCorner.Y then BRCorner.Y := Points[I].Y;
      end;
    end;

    Result := TRectF.Create(TLCorner, BRCorner);
  end
  else begin
    Result := TRectF.Empty;
  end;
end;

{ tagSIZE, TSize }
constructor TSize.Create(P: TSize);
begin
  cx := P.cx;
  cy := P.cy;
end;

constructor TSize.Create(const X, Y: Integer);
begin
  cx := X;
  cy := Y;
end;

class operator TSize.Add(const Lhs, Rhs: TSize): TSize;
begin
  Result.cx := Lhs.cx + Rhs.cx;
  Result.cy := Lhs.cy + Rhs.cy;
end;

function TSize.Add(const Point: TSize): TSize;
begin
  Result.cx := cx + Point.cx;
  Result.cy := cy + Point.cy;
end;

function TSize.Distance(const P2: TSize): Double;
begin
  // We Add 0.0 int the expression to cast all operations to floating point.
  // This avoids overflow in signed integer operations.
  Result := Sqrt(Sqr(0.0+ Self.cx - P2.cx) + Sqr(0.0 + Self.cy - P2.cy));
end;

function TSize.IsZero: Boolean;
begin
  Result := (cx = 0) and (cy = 0);
end;

class operator TSize.Equal(const Lhs, Rhs: TSize): Boolean;
begin
  Result := (Lhs.cx = Rhs.cx) and (Lhs.cy = Rhs.cy);
end;

class operator TSize.NotEqual(const Lhs, Rhs: TSize): Boolean;
begin
  Result := not (Lhs = Rhs);
end;

class operator TSize.Subtract(const Lhs, Rhs: TSize): TSize;
begin
  Result.cx := Lhs.cx - Rhs.cx;
  Result.cy := Lhs.cy - Rhs.cy;
end;

function TSize.Subtract(const Point: TSize): TSize;
begin
  Result.cx := cx - Point.cx;
  Result.cy := cy - Point.cy;
end;

{ TSizeF }
function TSizeF.Add(const Point: TSizeF): TSizeF;
begin
{$EXCESSPRECISION OFF}
  Result.cx := cx + Point.cx;
  Result.cy := cy + Point.cy;
{$EXCESSPRECISION ON}
end;

class operator TSizeF.Add(const Lhs, Rhs: TSizeF): TSizeF;
begin
{$EXCESSPRECISION OFF}
  Result.cx := Lhs.cx + Rhs.cx;
  Result.cy := Lhs.cy + Rhs.cy;
{$EXCESSPRECISION ON}
end;

constructor TSizeF.Create(const X, Y: Single);
begin
  cx := X;
  cy := Y;
end;

constructor TSizeF.Create(P: TSizeF);
begin
  cx := P.cx;
  cy := P.cy;
end;

function TSizeF.Distance(const P2: TSizeF): Double;
begin
  Result := Sqrt(Sqr(Self.cx - P2.cx) + Sqr(Self.cy - P2.cy));
end;

class operator TSizeF.Implicit(const Point: TPointF): TSizeF;
begin
  Result.cx := Point.X;
  Result.cy := Point.Y;
end;

class operator TSizeF.Implicit(const Size: TSizeF): TPointF;
begin
  Result.X := Size.cx;
  Result.Y := Size.cy;
end;

function TSizeF.IsZero: Boolean;
begin
  Result := SameValue(cx, 0.0) and SameValue(cy, 0.0);
end;

class operator TSizeF.Equal(const Lhs, Rhs: TSizeF): Boolean;
begin
  Result := SameValue(Lhs.cx, Rhs.cx) and SameValue(Lhs.cy, Rhs.cy);
end;

class operator TSizeF.NotEqual(const Lhs, Rhs: TSizeF): Boolean;
begin
  Result := not (Lhs = Rhs);
end;

function TSizeF.Subtract(const Point: TSizeF): TSizeF;
begin
{$EXCESSPRECISION OFF}
  Result.cx := cx - Point.cx;
  Result.cy := cy - Point.cy;
{$EXCESSPRECISION ON}
end;

class operator TSizeF.Subtract(const Lhs, Rhs: TSizeF): TSizeF;
begin
{$EXCESSPRECISION OFF}
  Result.cx := Lhs.cx - Rhs.cx;
  Result.cy := Lhs.cy - Rhs.cy;
{$EXCESSPRECISION ON}
end;

function TSizeF.Ceiling: TSize;
begin
  Result.cx := Ceil(cx);
  Result.cy := Ceil(cy);
end;

function TSizeF.Round: TSize;
begin
  Result.cx := Trunc(cx + 0.5);
  Result.cy := Trunc(cy + 0.5);
end;

function TSizeF.Truncate: TSize;
begin
  Result.cx := Trunc(cx);
  Result.cy := Trunc(cy);
end;

class operator TSizeF.Implicit(const Size: TSize): TSizeF;
begin
  Result.cx := Size.cx;
  Result.cy := Size.cy;
end;



{ TSmallPoint }
constructor TSmallPoint.Create(P: TSmallPoint);
begin
  x := P.x;
  y := P.y;
end;

constructor TSmallPoint.Create(const X, Y: Word);
begin
  Self.x := X;
  Self.y := Y;
end;

constructor TSmallPoint.Create(const X, Y: SmallInt);
begin
  Self.x := X;
  Self.y := Y;
end;

function TSmallPoint.Add(const Point: TSmallPoint): TSmallPoint;
begin
  Result.x := x + Point.x;
  Result.y := y + Point.y;
end;

class operator TSmallPoint.Add(const Lhs, Rhs: TSmallPoint): TSmallPoint;
begin
  Result.x := Lhs.x + Rhs.x;
  Result.y := Lhs.y + Rhs.y;
end;

function TSmallPoint.Subtract(const Point: TSmallPoint): TSmallPoint;
begin
  Result.x := x - Point.x;
  Result.y := y - Point.y;
end;

class operator TSmallPoint.Subtract(const Lhs, Rhs: TSmallPoint): TSmallPoint;
begin
  Result.x := Lhs.x - Rhs.x;
  Result.y := Lhs.y - Rhs.y;
end;

function TSmallPoint.Distance(const P2: TSmallPoint): Double;
begin
  // We Add 0.0 int the expression to cast all operations to floating point.
  // This avoids overflow in signed integer operations.
  Result := Sqrt(Sqr(0.0+ Self.X - P2.X) + Sqr(0.0 + Self.Y - P2.Y));
end;

class operator TSmallPoint.Equal(const Lhs, Rhs: TSmallPoint): Boolean;
begin
  Result := (Lhs.x = Rhs.x) and (Lhs.y = Rhs.y);
end;

class operator TSmallPoint.NotEqual(const Lhs, Rhs: TSmallPoint): Boolean;
begin
  Result := not (Lhs = Rhs);
end;

//class operator TSmallPoint.Implicit(Value: TSmallPoint): Cardinal;
//begin
//  Result := (Value.x shl 16) or Value.y;
//end;

function TSmallPoint.IsZero: Boolean;
begin
  Result := (x = 0) and (y = 0);
end;

end.