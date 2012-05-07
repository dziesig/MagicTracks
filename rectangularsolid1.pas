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
  Classes, SysUtils, Graphics,

  Persists1, TextIO1, DrawingCommon1, DrawingObject1,
  Preferences1, ExtCtrls, ThreePoint1, Forms;

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
  //protected
  //  procedure LoadCommon( var F : TextFile ); override;
  public
    constructor Create( aParent : TPersists = nil); override;
    //constructor Create( var F    : TextFile;
    //                    aParent : TPersists = nil ); override;
    procedure MakeNew; override;
    procedure Save( TextIO : TTextIO ); override;
    procedure Load( TextIO : TTextIO ); override;
    procedure Assign( Source : TPersists ); override;

    procedure Draw( Frame       : TFrame;
                    Preferences : TPreferences;
                    ActiveLayer : Boolean ); override;

    property Length : Double read GetLength write SetLength;
    property Width  : Double read GetWidth  write SetWidth;
    property Height : Double read GetHeight write SetHeight;
  end;

implementation

uses
  Internals1, DrawingFrame1;

{ RectangularSolid }

const
  CurrentVersion = 1;

procedure TRectangularSolid.Assign(Source: TPersists);
begin
  inherited Assign(Source);
end;

//constructor TRectangularSolid.Create(var F: TextFile; aParent: TPersistentZ);
//begin
//  inherited Create(F, aParent);
//  MakeNew;
//  LoadCommon( F );
//end;

constructor TRectangularSolid.Create(aParent: TPersists);
begin
  inherited Create(aParent);
  MakeNew;
end;

procedure TRectangularSolid.Draw( Frame       : TFrame;
                                  Preferences : TPreferences;
                                  ActiveLayer : Boolean );
var
  DF : TDrawingFrame;
  Offset : T3Point;
  XX0, XX1 : Integer;
  YY0, YY1 : Integer;
  OldWidth : Integer;
  OldStyle : TPenStyle;
begin
  DF := Frame as TDrawingFrame;
  Offset := T3Point.Create;
  Offset.X := Origin.X + Length;
  Offset.Y := Origin.Y + Width;
  Offset.Z := Origin.Z + Height;

  //InternalsForm1.PutRectOrigin( Origin, Box, Preferences );
  //InternalsForm1.PutRectOffset( Offset, Box, Preferences );
  //InternalsForm1.PutRectLength( fSize, Box,Preferences );

  XX0 := PixelsX( Origin, DF.BoxType, Preferences, DF.PaintBox1);
  XX1 := PixelsX( Offset, DF.BoxType, Preferences, DF.PaintBox1);
  YY0 := PixelsY( Origin, DF.BoxType, Preferences, DF.PaintBox1);
  YY1 := PixelsY( Offset, DF.BoxType, Preferences, DF.PaintBox1);

//  InternalsForm1.PutRectPixels( XX0, YY0, XX1, YY1, Box );

  with DF.PaintBox1.Canvas do
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

//procedure TRectangularSolid.Draw(Frame: TFrame; Preferences: TPreferences);
//var
//  DF : TDrawingFrame;
//begin
////  inherited Draw(Frame, Preferences);
//end;
//
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

procedure TRectangularSolid.Load( TextIO : TTextIO );
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
      fSize.Load(TextIO);
    end;
  inherited Load(TextIO);  { TODO : Make sure that this inherited Load(F) is really needed/valid; }
  TextIO.Readln(S);             // Read the end of class
  CheckEndClass(S,ClsName);     // Assert end of class is correct and of correct format
  fModified := false;           // make sure this was NOT modified by the load.
end;

//procedure TRectangularSolid.LoadCommon(var F: TextFile);
//var
//  S : String;
//  V : Integer;
//begin
//  Readln(F,V);
//  if V >= 1 then
//    begin
//      fSize.Load(F);
//    end;
//  inherited Load(F);  { TODO : Make sure that this inherited Load(F) is really needed/valid; }
//  Readln(F,S);
//  if S <> '</Rectangular Solid>' then
//    raise Exception.Create('End of Rectangular Solid.');
//end;

procedure TRectangularSolid.MakeNew;
begin
  inherited MakeNew;
  fSize := T3Point.Create;
end;

procedure TRectangularSolid.Save( TextIO : TTextIO );
var
  S : String;
begin
  S := self.ClassName;          // Get our class name
  TextIO.Writeln('<'+S+'>');    // Write the start of class

  TextIO.Writeln(CurrentVersion);
  fSize.Save(TextIO);
  inherited Save(TextIO);
  TextIO.Writeln('</'+S+'>');   // Write the end of class
  fModified := false;           // if it were modified, it isn't any more.
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

