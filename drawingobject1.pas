unit DrawingObject1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Persistent1, DrawingCommon1, Graphics, Preferences1,
  ExtCtrls, ThreePoint1, Forms;

type
  TDrawingObjects = class;

  { TDrawingObject }

  TDrawingObject = class(TPersistentz)
  private
    fDrawingObjects : TDrawingObjects;
    fOrigin         : T3Point;
    fRho, fTheta    : Double;
    fSelect         : Boolean;
    fMoveStart      : T3Point;

    function GetX: Double;
    function GetY: Double;
    function GetZ: Double;
    procedure SetOrigin(const AValue: T3Point);
    procedure SetRho(const AValue: Double);
    procedure SetTheta(const AValue: Double);
    procedure SetX(const AValue: Double);
    procedure SetY(const AValue: Double);
    procedure SetZ(const AValue: Double);
  protected
    procedure LoadCommon( var F : TextFile ); virtual; abstract;
  public
    constructor Create( aParent : TPersistentZ = nil); override;
    constructor Create( var F    : TextFile;
                        aParent : TPersistentZ = nil ); virtual; //abstract;
    destructor Destroy; override;
    procedure MakeNew; override;
    procedure Save( var F : TextFile ); override;
    procedure Load( var F : TextFile ); override;
    procedure Assign( Source : TPersistentZ ); override;

    function PixelsX( Value       : T3Point;
                      Box         : TDrawingBox;
                      Preferences : TPreferences;
                      PaintBox    : TPaintBox) : Integer;
    function PixelsY( Value       : T3Point;
                      Box         : TDrawingBox;
                      Preferences : TPreferences;
                      PaintBox    : TPaintBox) : Integer;

    function MicronsX( Value : T3Point;
                       Box   : TDrawingBox ) : Double;
    function MicronsY( Value : T3Point;
                       Box   : TDrawingBox ) : Double;

    procedure Draw( Frame       : TFrame;
                    Preferences : TPreferences;
                    ActiveLayer : Boolean ); virtual; abstract;

    function Drawing : TObject;

    procedure DrawHandle( Frame : TFrame;
                          X, Y  : Integer; // Pixel coordinates on Canvas
                          Ref   : Integer );

    procedure Move( Position : T3Point );
    procedure MoveStart( Start : T3Point );

    procedure ToggleSelect;
    procedure Select;
    procedure Deselect;

    property X : Double read GetX write SetX;
    property Y : Double read GetY write SetY;
    property Z : Double read GetZ write SetZ;

    property Origin : T3Point read fOrigin write SetOrigin;
    property Rho : Double read fRho write SetRho;       // rotation in Z
    property Theta : Double read fTheta write SetTheta; // rotation in XY

    property Selected : Boolean read fSelect;

  end;

  { TDrawingObjects }

  TDrawingObjects = class(TPersistentList)
  public
    procedure MakeNew; override;
    procedure Save( var F : TextFile ); override;
    procedure Load( var F : TextFile ); override;
    procedure Assign( Source : TPersistentZ ); override;

    procedure Draw( PaintBox : TPaintBox;
                    Box : TDrawingBox ); virtual; abstract;

    procedure DeselectAll;
  end;

  { TDrawingObjectRaster }

  TDrawingObjectReference = record
    Obj : TDrawingObject;
    Ref : Integer;
  end;

  TDrawingObjectRaster = class
  private
    vRaster : array of TDrawingObjectReference;
    fHeight : Integer;
    fWidth  : Integer;

    function  CoordinatesAreValid( var X, Y : Integer ) : Boolean;
  public
    constructor Create;
    destructor  Destroy;

    procedure Resize( aWidth, aHeight : Integer );

    function DrawingObject( X, Y : Integer) : TDrawingObjectReference;
    procedure DrawingObject( X, Y : Integer; Obj : TDrawingObject; Reference : Integer);

    property Height : Integer read fHeight;
    property Width  : Integer read fWidth;

  end;

implementation

uses
  Main1, Internals1, Drawing1, UnitConversion1, RectangularSolid1, Sphere1,
  StraightLine1, CanvasStack1, DrawingFrame1;

const
  CurrentVersion = 1;
  CurrentListVersion = 1;

{ TDrawingObjectRaster }

const
  RasterDiv = 5;

function TDrawingObjectRaster.CoordinatesAreValid(var X, Y: Integer): Boolean;
begin
  Result := False;
  Dec(X);
  Dec(Y);
  if (X < 0) or (Y < 0) then exit;
  if (X >= fWidth) or (Y >= fHeight) then exit;

  Result := True;
end;

constructor TDrawingObjectRaster.Create;
begin
  SetLength(vRaster,0);
  fHeight := 0;
  fWidth  := 0;
end;

destructor TDrawingObjectRaster.Destroy;
begin
  SetLength(vRaster,0);
end;

function TDrawingObjectRaster.DrawingObject(X, Y: Integer): TDrawingObjectReference;
var
  Index : Integer;
  Q : Integer;
begin
  X := X div RasterDiv; Y := Y div RasterDiv;
  if CoordinatesAreValid( X, Y ) then
    begin
      Index := Y * fWidth + X;
      Result := vRaster[Index]
    end
  else
    begin
      Result.Obj := nil;
      Result.Ref := 0;
    end;
end;

procedure TDrawingObjectRaster.DrawingObject(X, Y: Integer; Obj: TDrawingObject;
  Reference : Integer );
var
  Index : Integer;
begin
  X := X div RasterDiv; Y := Y div RasterDiv;
  if CoordinatesAreValid( X, Y ) then
    begin
      Index := Y * fWidth + X;
      vRaster[Index].Obj := Obj;
      vRaster[Index].Ref := Reference;
    end;
end;

procedure TDrawingObjectRaster.Resize(aWidth, aHeight: Integer);
begin
  SetLength(vRaster,0);
  fWidth := aWidth div RasterDiv;
  fHeight := aHeight div RasterDiv;
  SetLength(vRaster, fWidth * fHeight );
end;

{ TDrawingObjects }

procedure TDrawingObjects.Assign(Source: TPersistentZ);
begin
  inherited Assign(Source);
end;

procedure TDrawingObjects.DeselectAll;
var
  I : Integer;
begin
  for I := 0 to pred(Count) do
    TDrawingObject(Items[I]).Deselect;
end;

procedure TDrawingObjects.Load(var F: TextFile);
var
  V : Integer;
  S : String;
  C : Integer;
  I : Integer;
  O : TDrawingObject;
begin
  Readln(F,S);
  if S <> '<DrawingObjects>' then
    raise Exception.Create('Start of Drawing Object List');
  Readln(F, V );
// Read based on version;
  if V >= 1 then
    begin
      Readln(F,C);
      for I := 0 to pred(C) do
        begin
          Readln(F,S);
          if S = '<Rectangular Solid>' then
            O := TRectangularSolid.Create(F,self)
          else if S = '<Sphere>' then
            O := TSphere.Create(F,Self)
          else if S = '<Straight Line>' then
            O := TStraightLine.Create(F,self)
          else
            raise Exception.Create('Drawing Object List Error or unknown Drawing Object: ' + S);
          Add(O);
        end;
    end;
  Readln(F, S);
  if S <> '</DrawingObjects>' then
    raise Exception.Create('End of Drawing Object List');
  inherited Load(F);
end;

procedure TDrawingObjects.MakeNew;
begin
  inherited MakeNew;
end;

procedure TDrawingObjects.Save(var F: TextFile);
var
  I : Integer;
begin
  inherited Save(F);
  Writeln(F,'<DrawingObjects>');
  Writeln(F,CurrentListVersion);
  Writeln(F,Count);
  for I := 0 to pred(Count) do
    TDrawingObject(Items[I]).Save(F);
  Writeln(F,'</DrawingObjects>');
end;

{ TDrawingObject }

procedure TDrawingObject.Assign(Source: TPersistentZ);
begin
  inherited Assign(Source);
end;

constructor TDrawingObject.Create(var F: TextFile; aParent: TPersistentZ);
begin
  Origin := T3Point.Create;;
  fMoveStart := T3Point.Create;
end;

constructor TDrawingObject.Create(aParent: TPersistentZ);
begin
  inherited Create(aParent);
  Origin := T3Point.Create;
  fMoveStart := T3Point.Create;
end;

destructor TDrawingObject.Destroy;
begin
  Origin.Free;
  fDrawingObjects.Free;
  fMoveStart.Free;
  inherited Destroy;
end;

procedure TDrawingObject.DrawHandle(Frame: TFrame; X, Y: Integer; Ref : Integer);
var
  DF : TDrawingFrame;
  X0, Y0, X1, Y1 : Integer;
  Size : Integer;
begin
  DF := Frame as TDrawingFrame;
  if Ref = 1 then
    begin
      Size := 5;
    end
  else
    begin
      Size := 4;
    end;
  X0 := X - Size;
  X1 := X + Size;
  Y0 := Y - Size;
  Y1 := Y + Size;
  DF.DrawingObject(X0, Y0, Self,Ref);
  DF.DrawingObject(X0, Y1, Self,Ref);
  DF.DrawingObject(X1, Y0, Self,Ref);
  DF.DrawingObject(X1, Y1, Self,Ref);
  DF.DrawingObject( X, Y,  Self, Ref );
  CanvasStack.Push( DF.PaintBox1.Canvas );
  DF.PaintBox1.Canvas.Pen.Width := 1;
  DF.PaintBox1.Canvas.Pen.Style := psSolid;
  DF.PaintBox1.Canvas.Pen.Color := clBlack;
  DF.PaintBox1.Canvas.Brush.Color := $cccc00;
  DF.PaintBox1.Canvas.Rectangle( X0, Y0, X1, Y1 );
  X0 := X - Size + 3;
  Y0 := Y + Size - 2;
  X1 := X + Size - 2;
  Y1 := Y - Size + 3;

  DF.PaintBox1.Canvas.Pen.Width := 1;
  DF.PaintBox1.Canvas.Pen.Color := clBlack;
  DF.PaintBox1.Canvas.MoveTo( X0,Y0);
  DF.PaintBox1.Canvas.LineTo( X1,Y0);
  DF.PaintBox1.Canvas.LineTo( X1, Y1);
  CanvasStack.Pop( DF.PaintBox1.Canvas );
end;

function TDrawingObject.Drawing: TObject;
begin
  Result := MainForm.ActiveDrawing;
end;

procedure TDrawingObject.ToggleSelect;
begin
  fSelect := not fSelect;
end;

procedure TDrawingObject.Select;
begin
  fSelect := true;
end;

procedure TDrawingObject.Deselect;
begin
  fSelect := False;
end;

function TDrawingObject.GetX: Double;
begin
  Result := fOrigin.fX;
end;

function TDrawingObject.GetY: Double;
begin
  Result := fOrigin.fY;
end;

function TDrawingObject.GetZ: Double;
begin
  Result := fOrigin.fZ;
end;

procedure TDrawingObject.Load(var F: TextFile);
var
  S : String;
  V : Integer;
begin
  Readln(F,S);
  if S <> '<Drawing Object>' then
    raise Exception.Create('Start of Drawing Object');
  Readln(F,V);
  if V >= 1 then
    fOrigin.Load(F);
  Readln(F,fRho);
  Readln(F,fTheta);

  inherited Load(F);
  Readln(F,S);
  if S <> '</Drawing Object>' then
    raise Exception.Create('End of Drawing Object');
end;

procedure TDrawingObject.MakeNew;
begin
  inherited MakeNew;
  fDrawingObjects := TDrawingObjects.Create( Self );
  fOrigin.X := 0.0;
  fOrigin.Y := 0.0;
  fOrigin.Z := 0.0;
  fRho := 0;
  fTheta := 0;
end;

function TDrawingObject.MicronsX(Value: T3Point; Box: TDrawingBox): Double;
begin
  case Box of
    XY, XZ :  Result := Value.X;
    YZ     :  Result := Value.Y;
  end;
end;

function TDrawingObject.MicronsY(Value: T3Point; Box: TDrawingBox): Double;
begin
  case Box of
    XY     :  Result := Value.Y;
    XZ, YZ :  Result := Value.Z;
  end;
end;

procedure TDrawingObject.Move(Position: T3Point);
var
  T : T3Point;
begin
  InternalsForm1.PutEvent( 'Move', 'Pos:    ' + Position.Show );
  InternalsForm1.PutEvent( 'Move', 'Start:  ' + fMoveStart.Show );
  T := T3Point.Create(Position);
  T.Sub(fMoveStart);
  InternalsForm1.PutEvent( 'Move', 'T:      ' + T.Show );
  Origin.Sub( T );
  InternalsForm1.PutEvent( 'Move', 'Origin: ' + Origin.Show );
  T.Free;
end;

procedure TDrawingObject.MoveStart(Start: T3Point);
begin
  fMoveStart.Assign(Start);
  InternalsForm1.PutEvent( 'MoveStart:  ', fMoveStart.Show );
end;

function TDrawingObject.PixelsX(Value: T3Point; Box: TDrawingBox;
  Preferences: TPreferences; PaintBox : TPaintBox): Integer;
var
  D : TDrawing;
  V : Double;
begin
  case Box of
    XY, XZ :  V := Value.X;
    YZ     :  V := Value.Y;
  end;
  D := TDrawing(Drawing);
  { DONE 1 -oDon Ziesig -cPrimary Code Functionality : Get reference to Drawing at this location. }
  Result := MicronsToPixels( V - D.MinX[Box], D.Preferences );
end;

function TDrawingObject.PixelsY(Value: T3Point; Box: TDrawingBox;
  Preferences: TPreferences; PaintBox : TPaintBox): Integer;
var
  D : TDrawing;
  H : Integer;
  Y0 : Integer;
  V : Double;
begin
  case Box of
    XY     :  V := Value.Y;
    XZ, YZ :  V := Value.Z;
  end;
  D := TDrawing(Drawing);
  { DONE 1 -oDon Ziesig -cPrimary Code Functionality : Get reference to Drawing at this location. }
  H := PaintBox.Height;
  Y0 := MicronsToPixels( V - D.MinY[Box], D.Preferences );
  Result := H - Y0;
  //InternalsForm1.PutEvent('Value',Value*MicronsToFt);
  //InternalsForm1.PutEvent('Canvas.Height',H);
  //InternalsForm1.PutEvent('Y0',Y0);
  //InternalsForm1.PutEvent('Result',Result);
  //InternalsForm1.PutEvent('========','=====================================');
end;

procedure TDrawingObject.Save(var F: TextFile);
begin
  Writeln(F,'<Drawing Object>');
  Writeln(F,CurrentVersion);
  fOrigin.Save(F);

  Writeln(F,fRho);
  Writeln(F,fTheta);

  inherited Save(F);
  Writeln(F,'</Drawing Object>');
end;

procedure TDrawingObject.SetOrigin(const AValue: T3Point);
begin
  if fOrigin=AValue then exit;
  fOrigin:=AValue;
end;

procedure TDrawingObject.SetRho(const AValue: Double);
begin
  Update( fRho, AValue );
end;

procedure TDrawingObject.SetTheta(const AValue: Double);
begin
  Update( fTheta, AValue );
end;

procedure TDrawingObject.SetX(const AValue: Double);
begin
  Origin.X := AValue;
//  Update( fX, AValue );
end;

procedure TDrawingObject.SetY(const AValue: Double);
begin
  Origin.Y := AValue;
//  Update( fY, AValue );
end;

procedure TDrawingObject.SetZ(const AValue: Double);
begin
  Origin.Z := AValue;
//  Update( fZ, AValue );
end;

end.

