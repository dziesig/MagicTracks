//Copyright (c) 2012 by Donald R. Ziesig
//
//Donald.at.Ziesig.org
//
//This file is part of MagicTracks.
//
//MagicTracks is free software: you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation, either version 3 of the License, or
//(at your option) any later version.
//
//MagicTracks is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.
//
//You should have received a copy of the GNU General Public License
//along with MagicTracks.  If not, see <http://www.gnu.org/licenses/>.

unit ThreePoint1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  Persists1, TextIO1;

type

  { T3Point }

  T3Point = class( TPersists )
  private
    procedure SetX(const AValue: Double);
    procedure SetY(const AValue: Double);
    procedure SetZ(const AValue: Double);
  public
    fX, fY, fZ : Double;

    constructor Create( aParent : TPersists = nil ); override;
    constructor Create( vX, vY, vZ : Double; aParent : TPersists = nil ); virtual;
    constructor Create( From : T3Point; aParent : TPersists = nil ); virtual;

    procedure Add( const Value : T3Point );

    procedure Save( TextIO : TTextIO ); override;
    procedure Load( TextIO : TTextIO ); override;

    procedure MakeNew;

    procedure Assign( From : T3Point );

    procedure Sub( const Value : T3Point );

    function Show : String;

    property X : Double read fX write SetX;
    property Y : Double read fY write SetY;
    property Z : Double read fZ write SetZ;

  end;

implementation

uses
  UnitConversion1; // , DrawingCommon1, Drawing1;

{ T3Point }

const
  CurrentVersion = 1;

procedure T3Point.Save( TextIO : TTextIO );
var
  S : String;
begin
  S := self.ClassName;          // Get our class name
  TextIO.Writeln('<'+S+'>');    // Write the start of class

  TextIO.Writeln(CurrentVersion);
  TextIO.Writeln(fX);
  TextIO.Writeln(fY);
  TextIO.Writeln(fZ);
  TextIO.Writeln('</'+S+'>');   // Write the end of class
  fModified := false;           // if it were modified, it isn't any more.
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

function T3Point.Show: String;
begin
  Result := '(' + FloatToStr( X * MicronstoIn ) + ', ' +
                  FloatToStr( Y * MicronstoIn ) + ', ' +
                  FloatToStr( Z * MicronstoIn ) + ')';
end;

procedure T3Point.Sub(const Value: T3Point);
begin
  Update( fX, fX - Value.X );
  Update( fY, fY - Value.Y );
  Update( fZ, fZ - Value.Z );

end;

procedure T3Point.Add(const Value: T3Point);
begin
  Update( fX, fX + Value.X );
  Update( fY, fY + Value.Y );
  Update( fZ, fZ + Value.Z );
end;

procedure T3Point.Assign(From: T3Point);
begin
  fX := From.fX;
  fY := From.fY;
  fZ := From.fZ;
end;

constructor T3Point.Create(aParent: TPersists);
begin
  if aParent <> nil then inherited Create(aParent);
end;

constructor T3Point.Create( vX, vY, vZ: Double; aParent: TPersists);
begin
  if aParent <> nil then inherited Create(aParent);
  fX := vX;
  fY := vY;
  fZ := vZ;
end;

constructor T3Point.Create( From: T3Point; aParent: TPersists);
begin
  if aParent <> nil then inherited Create(aParent);
  Assign( From );
end;

procedure T3Point.Load( TextIO : TTextIO );
var
  V : Integer;
  ClsName : String;
  S       : String;
begin
  ClsName := self.ClassName;    // Get the expected class name
  TextIO.ReadLn(S);             // Read the start of class
  CheckStartClass(S,ClsName);   // Assert they are correct and of correct format
  TextIO.Readln(V);
  if V >= 1 then
    begin
      TextIO.Readln(fx);
      TextIO.Readln(fY);
      TextIO.Readln(fZ);
    end;
  TextIO.Readln(S);             // Read the end of class
  CheckEndClass(S,ClsName);     // Assert end of class is correct and of correct format
  fModified := false;           // make sure this was NOT modified by the load.
end;

procedure T3Point.MakeNew;
begin
  fX := 0.0;
  fY := 0.0;
  fZ := 0.0;
end;

end.

