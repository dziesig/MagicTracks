unit ThreePoint1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Persistent1;

type

  { T3Point }

  T3Point = class( TPersistentZ )
  private
    procedure SetX(const AValue: Double);
    procedure SetY(const AValue: Double);
    procedure SetZ(const AValue: Double);
  public
    fX, fY, fZ : Double;

    constructor Create( aParent : TPersistentZ = nil ); virtual;
    constructor Create( vX, vY, vZ : Double; aParent : TPersistentZ = nil ); virtual;
    constructor Create( From : T3Point; aParent : TPersistentZ = nil ); virtual;

    procedure Save(var F : TextFile ); override;
    procedure Load(var F : TextFile ); override;

    procedure MakeNew;

    procedure Assign( From : T3Point );

    property X : Double read fX write SetX;
    property Y : Double read fY write SetY;
    property Z : Double read fZ write SetZ;
  end;

implementation

uses
  UnitConversion1, DrawingCommon1, Drawing1;

{ T3Point }

const
  CurrentVersion = 1;

procedure T3Point.Save(var F: TextFile);
begin
  inherited Save(F);
  Writeln(F,'<3 Point>');
  Writeln(F,CurrentVersion);
  Writeln(F,fX);
  Writeln(F,fY);
  Writeln(F,fZ);
  Writeln(F,'</3 Point>');
end;

procedure T3Point.SetX(const AValue: Double);
begin
  Update(fX,AValue);
end;

procedure T3Point.SetY(const AValue: Double);
begin
  Update(fY,AValue);
end;

procedure T3Point.SetZ(const AValue: Double);
begin
  Update(fZ,AValue);
end;

procedure T3Point.Assign(From: T3Point);
begin
  fX := From.fX;
  fY := From.fY;
  fZ := From.fZ;
end;

constructor T3Point.Create(aParent: TPersistentZ);
begin
  if aParent <> nil then inherited Create(aParent);
end;

constructor T3Point.Create( vX, vY, vZ: Double; aParent: TPersistentZ);
begin
  if aParent <> nil then inherited Create(aParent);
  fX := vX;
  fY := vY;
  fZ := vZ;
end;

constructor T3Point.Create( From: T3Point; aParent: TPersistentZ);
begin
  if aParent <> nil then inherited Create(aParent);
  Assign( From );
end;

procedure T3Point.Load(var F: TextFile);
var
  V : Integer;
  S : String;
begin
  Readln(F,S);
  if S <> '<3 Point>' then;
  Readln(F,V);
  if V >= 1 then
    begin
      Readln(F,fx);
      Readln(F,fY);
      Readln(F,fZ);
    end;
  Readln(F,S);
  if S <> '</3 Point>' then ;
  inherited Load(F);
end;

procedure T3Point.MakeNew;
begin
  fX := 0.0;
  fY := 0.0;
  fZ := 0.0;
end;

end.

