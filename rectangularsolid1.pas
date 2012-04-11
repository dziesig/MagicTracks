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

unit RectangularSolid1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Persistent1, DrawingCommon1, DrawingObject1,
  Preferences1, ExtCtrls, ThreePoint1;

type

  { RectangularSolid }

  { TRectangularSolid }

  TRectangularSolid = class (TDrawingObject)
  private
    fSize : T3Point;
    function GetHeight: Double;
    function GetLength: Double;
    function GetWidth: Double;
    procedure SetHeight(const AValue: Double);
    procedure SetLength(const AValue: Double);
    procedure SetWidth(const AValue: Double);
  protected
    procedure LoadCommon( var F : TextFile ); override;
  public
    constructor Create( aParent : TPersistentZ = nil); override;
    constructor Create( var F    : TextFile;
                        aParent : TPersistentZ = nil ); override;
    procedure MakeNew; override;
    procedure Save( var F : TextFile ); override;
    procedure Load( var F : TextFile ); override;
    procedure Assign( Source : TPersistentZ ); override;

    procedure Draw( PaintBox : TPaintBox; Box : TDrawingBox; Preferences : TPreferences ); override;

    property Length : Double read GetLength write SetLength;
    property Width  : Double read GetWidth  write SetWidth;
    property Height : Double read GetHeight write SetHeight;
  end;

implementation

uses
  Internals1;

{ RectangularSolid }

const
  CurrentVersion = 1;

procedure TRectangularSolid.Assign(Source: TPersistentZ);
begin
  inherited Assign(Source);
end;

constructor TRectangularSolid.Create(var F: TextFile; aParent: TPersistentZ);
begin
  inherited Create(F, aParent);
  MakeNew;
  LoadCommon( F );
end;

constructor TRectangularSolid.Create(aParent: TPersistentZ);
begin
  inherited Create(aParent);
  MakeNew;
end;

procedure TRectangularSolid.Draw( PaintBox: TPaintBox;
                                  Box: TDrawingBox;
                                  Preferences : TPreferences );
var
  Offset : T3Point;
  XX0, XX1 : Integer;
  YY0, YY1 : Integer;
  OldWidth : Integer;
  OldStyle : TPenStyle;
begin
  Offset := T3Point.Create;
  Offset.X := Origin.X + Length;
  Offset.Y := Origin.Y + Width;
  Offset.Z := Origin.Z + Height;

  //InternalsForm1.PutRectOrigin( Origin, Box, Preferences );
  //InternalsForm1.PutRectOffset( Offset, Box, Preferences );
  //InternalsForm1.PutRectLength( fSize, Box,Preferences );

  XX0 := PixelsX( Origin, Box, Preferences, PaintBox);
  XX1 := PixelsX( Offset, Box, Preferences, PaintBox);
  YY0 := PixelsY( Origin, Box, Preferences, PaintBox);
  YY1 := PixelsY( Offset, Box, Preferences, PaintBox);

//  InternalsForm1.PutRectPixels( XX0, YY0, XX1, YY1, Box );

  with PaintBox.Canvas do
    begin
      OldWidth := Pen.Width;
      OldStyle := Pen.Style;
      try
        Pen.Width := 3;
        Pen.Style := psSolid;
        MoveTo(XX0,YY0);
        LineTo(XX0,YY1);
        LineTo(XX1,YY1);
        LineTo(XX1,YY0);
        LineTo(XX0,YY0);
        //InternalsForm1.PutEvent('XX0',XX0);
        //InternalsForm1.PutEvent('XX1',XX1);
        //InternalsForm1.PutEvent('YY1',YY0);
        //InternalsForm1.PutEvent('yy1',YY1);
      finally
        Pen.Width := OldWidth;
        Pen.Style := OldStyle;
      end;
    end;
    Offset.free;
end;

function TRectangularSolid.GetHeight: Double;
begin
  Result := fSize.Z;
end;

function TRectangularSolid.GetLength: Double;
begin
  Result := fSize.X;
end;

function TRectangularSolid.GetWidth: Double;
begin
  Result := fSize.Y;
end;

procedure TRectangularSolid.Load(var F: TextFile);
var
  S : String;
  V : Integer;
begin
  Readln(F,S);
  if S <> '<Rectangular Solid>' then
    raise Exception.Create('Start of Rectangular Solid.');
  LoadCommon( F );
end;

procedure TRectangularSolid.LoadCommon(var F: TextFile);
var
  S : String;
  V : Integer;
begin
  Readln(F,V);
  if V >= 1 then
    begin
      fSize.Load(F);
    end;
  inherited Load(F);  { TODO : Make sure that this inherited Load(F) is really needed/valid; }
  Readln(F,S);
  if S <> '</Rectangular Solid>' then
    raise Exception.Create('End of Rectangular Solid.');
end;

procedure TRectangularSolid.MakeNew;
begin
  inherited MakeNew;
  fSize := T3Point.Create;
end;

procedure TRectangularSolid.Save(var F: TextFile);
begin
  Writeln(F,'<Rectangular Solid>');
  Writeln(F,CurrentVersion);
  fSize.Save(F);
  inherited Save(F);
  Writeln(F,'</Rectangular Solid>');
end;

procedure TRectangularSolid.SetHeight(const AValue: Double);
begin
  fSize.Z := AValue;
end;

procedure TRectangularSolid.SetLength(const AValue: Double);
begin
  fSize.X := AValue;
end;

procedure TRectangularSolid.SetWidth(const AValue: Double);
begin
  fSize.Y := AValue;
end;

end.

