unit DrawingObject1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Persistent1, DrawingCommon1, Graphics, Preferences1,
  ExtCtrls, ThreePoint1;

type
  TDrawingObjects = class;

  { TDrawingObject }

  TDrawingObject = class(TPersistentz)
  private
    fDrawingObjects : TDrawingObjects;
    fOrigin         : T3Point;
    fRho, fTheta    : Double;

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

    procedure Draw( PaintBox : TPaintBox;
                    Box : TDrawingBox;
                    Preferences : TPreferences); virtual; abstract;

    function Drawing : TObject;

    property X : Double read GetX write SetX;
    property Y : Double read GetY write SetY;
    property Z : Double read GetZ write SetZ;

    property Origin : T3Point read fOrigin write SetOrigin;
    property Rho : Double read fRho write SetRho;       // rotation in Z
    property Theta : Double read fTheta write SetTheta; // rotation in XY

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
  end;

implementation

uses
  Main1, Internals1, Drawing1, UnitConversion1, RectangularSolid1, Sphere1;

const
  CurrentVersion = 1;
  CurrentListVersion = 1;

{ TDrawingObjects }

procedure TDrawingObjects.Assign(Source: TPersistentZ);
begin
  inherited Assign(Source);
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
end;

constructor TDrawingObject.Create(aParent: TPersistentZ);
begin
  inherited Create(aParent);
  Origin := T3Point.Create;
end;

destructor TDrawingObject.Destroy;
begin
  Origin.Free;
  fDrawingObjects.Free;
  inherited Destroy;
end;

function TDrawingObject.Drawing: TObject;
begin
  Result := MainForm.ActiveDrawing;
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

