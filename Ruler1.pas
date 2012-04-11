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

unit Ruler1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls, DrawingCommon1, Drawing1;

type

  { TRuler }

  TRuler = class
  private
    fPB        : TPaintBox;
    fScrollBar : TScrollBar;
    fBoxType   : TDrawingBox;

    function  SBPosition : Double; { returns 0 .. 1.0 }
  public
    constructor Create( PB        : TPaintBox;
                        ScrollBar : TScrollBar;
                        BoxType   : TDrawingBox );

    destructor Destroy; override;

    procedure Draw( Drawing : TDrawing );

    procedure OriginToLowerLeft( Drawing : TDrawing );

    procedure Center( ScrollBar : TScrollBar; Pos : Double; Drawing : TDrawing );

  end;

implementation

uses
  Graphics, UnitConversion1, Preferences1, Common1;

type
  TInchesDraw = ( idInchesPlusFractions,
                  idInchesOnly,
                  idSixInches,
                  idFeet,
                  idTenFeet );

const
  InchesDraw : array [0..MaxZoom] of TInchesDraw =
    ( idInchesPlusFractions, idInchesPlusFractions, idInchesPlusFractions, idInchesPlusFractions,
      idInchesPlusFractions, idInchesPlusFractions, idInchesOnly,          idInchesOnly,
      idInchesOnly,          idInchesOnly,          idSixInches,           idSixInches,
      idSixInches,           idSixInches,           idSixInches,           idSixInches,
      idSixInches,           idFeet,                idTenFeet,             idTenFeet );

{ TRuler }

procedure TRuler.Center(ScrollBar : TScrollBar; Pos: Double; Drawing: TDrawing);
begin
  { TODO 1 -oDon Ziesig -cUser Interface : Implement Center Ruler on Coordinate }
end;

constructor TRuler.Create( PB: TPaintBox;
                           ScrollBar : TScrollBar;
                           BoxType   : TDrawingBox );
begin
  fPB := PB;
  fScrollBar := ScrollBar;
  fBoxType   := BoxType;
end;

destructor TRuler.Destroy;
begin

end;

procedure TRuler.Draw( Drawing : TDrawing );
var
  RulerRect : TRect;
  PosSB : Double;
  Horiz : Boolean;

  DisplayWidth  : Integer;
  DisplayHeight : Integer;

  procedure Horizontal;
  var
    WidthMicrons  : Double;
    XMin, XMax : Double;
    LeftPixels : Integer;
    I          : Integer;
    X, OldX    : Integer;
  begin
    WidthMicrons := PixelsToMicrons( DisplayWidth, Drawing.Preferences );
    case fBoxType of
      XY, XZ:
        if Drawing.Preferences.EnglishUnits then
          begin
            XMin := Drawing.XMin - 10 * FtToMicrons;
            XMax := Drawing.XMax + 10 * FtToMicrons;
          end;
      YZ:
        if Drawing.Preferences.EnglishUnits then
          begin
            XMin := Drawing.YMin - 10 * FtToMicrons;
            XMax := Drawing.YMax + 10 * FtToMicrons;
          end;
    end;

    LeftPixels := MicronsToPixels( PosSB * ( XMax - WidthMicrons - XMin) + XMin, Drawing.Preferences );

    case InchesDraw[Drawing.Preferences.ZoomIndex] of
      idInchesPlusFractions:
        begin
          OldX := Round(PixelsToValue(LeftPixels,Drawing.Preferences)*48);
          for I := 0 to DisplayWidth do
            begin
              X := Round(PixelsToValue(I+LeftPixels,Drawing.Preferences)*48);
              if (X <> OldX) then
                begin
                  OldX := X;
                  fPB.Canvas.MoveTo(I,0);
                  fPB.Canvas.LineTo(I,Trunc(DisplayHeight * 0.2));
                  if (X mod 48) = 0 then
                    begin
                      fPB.Canvas.LineTo(I,Trunc(DisplayHeight * 0.67));
                      fPB.Canvas.TextOut(I+3,Trunc(DisplayHeight * 0.45),IntToStr(X div 48)+'''');
                    end
                  else if (X mod 24) = 0 then
                    begin
                      fPB.Canvas.LineTo(I,Trunc(DisplayHeight * 0.67));
                      fPB.Canvas.TextOut( I+3,Trunc(DisplayHeight * 0.45),
                                          IntToStr(X div 48)+'''' + '6"');
                    end
                  else if (X mod 4) = 0 then
                    fPB.Canvas.LineTo(I,Trunc(DisplayHeight * 0.5));

                end;
            end;
        end;
      idInchesOnly:
        begin
          OldX := Round(PixelsToValue(LeftPixels,Drawing.Preferences)*12);
          for I := 0 to DisplayWidth do
            begin
              X := Round(PixelsToValue(I+LeftPixels,Drawing.Preferences)*12);
              if (X <> OldX) then
                begin
                  OldX := X;
                  fPB.Canvas.MoveTo(I,0);
                  fPB.Canvas.LineTo(I,Trunc(DisplayHeight * 0.3));
                  if (X mod 6) = 0 then
                    begin
                      fPB.Canvas.LineTo(I,Trunc(DisplayHeight * 0.5));
                    end;
                  if (X mod 12) = 0 then
                    begin
                      fPB.Canvas.LineTo(I,Trunc(DisplayHeight * 0.67));
                      fPB.Canvas.TextOut(I+3,Trunc(DisplayHeight * 0.45),IntToStr(X div 12)+'''');
                    end;
                end;
            end;
        end;
      idSixInches:
        begin
          OldX := Round(PixelsToValue(LeftPixels,Drawing.Preferences)*6);
          for I := 0 to DisplayWidth do
            begin
              X := Round(PixelsToValue(I+LeftPixels,Drawing.Preferences)*6);
              if (X <> OldX) then
                begin
                  OldX := X;
                  fPB.Canvas.MoveTo(I,0);
                  fPB.Canvas.LineTo(I,Trunc(DisplayHeight * 0.3));
                  if (X mod 3) = 0 then
                    begin
                      fPB.Canvas.LineTo(I,Trunc(DisplayHeight * 0.5));
                    end;
                  if (X mod 6) = 0 then
                    begin
                      fPB.Canvas.LineTo(I,Trunc(DisplayHeight * 0.67));
                      fPB.Canvas.TextOut(I+3,Trunc(DisplayHeight * 0.45),IntToStr(X div 6)+'''');
                    end;
                end;
            end;
        end;
      idFeet:
        begin
          OldX := Round(PixelsToValue(LeftPixels,Drawing.Preferences)*2);
          for I := 0 to DisplayWidth do
            begin
              X := Round(PixelsToValue(I+LeftPixels,Drawing.Preferences)*2);
              if (X <> OldX) then
                begin
                  OldX := X;
                  fPB.Canvas.MoveTo(I,0);
                  fPB.Canvas.LineTo(I,Trunc(DisplayHeight * 0.3));
                 if (X mod 2) = 0 then
                    begin
                      fPB.Canvas.LineTo(I,Trunc(DisplayHeight * 0.67));
                      fPB.Canvas.TextOut(I+3,Trunc(DisplayHeight * 0.45),IntToStr(X div 2)+'''');
                    end;
                end;
            end;
        end;
      idTenFeet:
        begin
          OldX := Round(PixelsToValue(LeftPixels,Drawing.Preferences));
          for I := 0 to DisplayWidth do
            begin
              X := Round(PixelsToValue(I+LeftPixels,Drawing.Preferences));
              if (X <> OldX) then
                begin
                  OldX := X;
                  fPB.Canvas.MoveTo(I,0);
                  fPB.Canvas.LineTo(I,Trunc(DisplayHeight * 0.3));
                 if (X mod 10) = 0 then
                    begin
                      fPB.Canvas.LineTo(I,Trunc(DisplayHeight * 0.67));
                      fPB.Canvas.TextOut(I+3,Trunc(DisplayHeight * 0.45),IntToStr(X)+'''');
                    end;
                end;
            end;
        end;
    end;
  end;

{ NOTE:  The vertical rulers invert the coordinates.  The paint box and scroll bar
         have their zero coordinate at the top.  The drawing uses standard +Y is
         UP! }
  procedure Vertical;
  var
    HeightMicrons : Double;
    YMin, YMax : Double;
    TopPixels : Integer;
    Y, OldY   : Integer;
    I         : Integer;
    PosSBPrime : Double;
  begin
    PosSBPrime := 1.0 - PosSB;
    HeightMicrons := PixelsToMicrons( DisplayHeight, Drawing.Preferences );

    case fBoxType of
      XY:
        if Drawing.Preferences.EnglishUnits then
          begin
            YMin := Drawing.YMin - 10 * FtToMicrons;
            YMax := Drawing.YMax + 10 * FtToMicrons;
          end;
      XZ, YZ:
        if Drawing.Preferences.EnglishUnits then
          begin
            YMin := Drawing.ZMin - 10 * FtToMicrons;
            YMax := Drawing.ZMax + 10 * FtToMicrons;
          end;
    end;

    TopPixels := MicronsToPixels( PosSBPrime * ( YMax - HeightMicrons - YMin) + YMin, Drawing.Preferences );

    case InchesDraw[Drawing.Preferences.ZoomIndex] of
      idInchesPlusFractions:
        begin
          OldY := Round(PixelsToValue(TopPixels,Drawing.Preferences)*48);
          for I := 0 to DisplayHeight do
            begin
              Y := Round(PixelsToValue(I+TopPixels,Drawing.Preferences)*48);
              if (Y <> OldY) then
                begin
                  OldY := Y;
                  fPB.Canvas.MoveTo(DisplayWidth,DisplayHeight - I);
                  fPB.Canvas.LineTo(Trunc(DisplayWidth * 0.7),DisplayHeight - I);
                  if (Y mod 48) = 0 then
                    begin
                      fPB.Canvas.LineTo(Trunc(DisplayWidth * 0.33),DisplayHeight - I);
                      AngleTextOut( fPB.Canvas, 270, Trunc(DisplayWidth * 0.45) +1,
                                    DisplayHeight - I+3,IntToStr(Y div 96)+'''');
                    end
                  else if (Y mod 24) = 0 then
                    begin
                      fPB.Canvas.LineTo(Trunc(DisplayWidth * 0.5),DisplayHeight - I);
                      AngleTextOut( fPB.Canvas, 270, Trunc(DisplayWidth * 0.45) +1,
                                    DisplayHeight - I+3,IntToStr(Y div 96)+'''' + ' 6"');
                    end
                  else if (Y mod 4) = 0 then
                    fPB.Canvas.LineTo(Trunc(DisplayWidth * 0.5),DisplayHeight - I);

                end;
            end;
        end;
      idInchesOnly:
        begin
          OldY := Round(PixelsToValue(TopPixels,Drawing.Preferences)*12);
          for I := 0 to DisplayHeight do
            begin
              Y := Round(PixelsToValue(I+TopPixels,Drawing.Preferences)*12);
              if (Y <> OldY) then
                begin
                  OldY := Y;
                  fPB.Canvas.MoveTo(DisplayWidth,DisplayHeight - I);
                  fPB.Canvas.LineTo(Trunc(DisplayWidth * 0.7),DisplayHeight - I);
                  if (Y mod 6) = 0 then
                    begin
                      fPB.Canvas.LineTo(Trunc(DisplayWidth * 0.5),DisplayHeight - I);
                    end;
                  if (Y mod 12) = 0 then
                    begin
                      fPB.Canvas.LineTo(Trunc(DisplayWidth * 0.33),DisplayHeight - I);
                      AngleTextOut( fPB.Canvas, 270, Trunc(DisplayWidth * 0.45) +1,
                                    DisplayHeight - I+3,IntToStr(Y div 12)+'''');
                    end;
                end;
            end;
        end;
      idSixInches:
        begin
          OldY := Round(PixelsToValue(TopPixels,Drawing.Preferences)*6);
          for I := 0 to DisplayHeight do
            begin
              Y := Round(PixelsToValue(I+TopPixels,Drawing.Preferences)*6);
              if (Y <> OldY) then
                begin
                  OldY := Y;
                  fPB.Canvas.MoveTo(DisplayWidth,DisplayHeight - I);
                  fPB.Canvas.LineTo(Trunc(DisplayWidth * 0.7),DisplayHeight - I);
                  if (Y mod 3) = 0 then
                    begin
                      fPB.Canvas.LineTo(Trunc(DisplayWidth * 0.5),DisplayHeight - I);
                    end;
                  if (Y mod 6) = 0 then
                    begin
                      fPB.Canvas.LineTo(Trunc(DisplayWidth * 0.33),DisplayHeight - I);
                      AngleTextOut( fPB.Canvas, 270, Trunc(DisplayWidth * 0.45) +1,
                                    DisplayHeight - I+3,IntToStr(Y div 6)+'''');
                    end;
                end;
            end;
        end;
      idFeet:
        begin
          OldY := Round(PixelsToValue(TopPixels,Drawing.Preferences)*2);
          for I := 0 to DisplayHeight do
            begin
              Y := Round(PixelsToValue(I+TopPixels,Drawing.Preferences)*2);
              if (Y <> OldY) then
                begin
                  OldY := Y;
                  fPB.Canvas.MoveTo(DisplayWidth,DisplayHeight - I);
                  fPB.Canvas.LineTo(Trunc(DisplayWidth * 0.7),DisplayHeight - I);
                  if (Y mod 2) = 0 then
                    begin
                      fPB.Canvas.LineTo(Trunc(DisplayWidth * 0.33),DisplayHeight - I);
                      AngleTextOut( fPB.Canvas, 270, Trunc(DisplayWidth * 0.45) +1,
                                    DisplayHeight - I+3,IntToStr(Y div 2)+'''');
                    end;
                end;
            end;
        end;
      idTenFeet:
        begin
          OldY := Round(PixelsToValue(TopPixels,Drawing.Preferences));
          for I := 0 to DisplayHeight do
            begin
              Y := Round(PixelsToValue(I+TopPixels,Drawing.Preferences));
              if (Y <> OldY) then
                begin
                  OldY := Y;
                  fPB.Canvas.MoveTo(DisplayWidth,DisplayHeight - I);
                  fPB.Canvas.LineTo(Trunc(DisplayWidth * 0.7),DisplayHeight - I);
                  if (Y mod 10) = 0 then
                    begin
                      fPB.Canvas.LineTo(Trunc(DisplayWidth * 0.33),DisplayHeight - I);
                      AngleTextOut( fPB.Canvas, 270, Trunc(DisplayWidth * 0.45) +1,
                                    DisplayHeight - I+3,IntToStr(Y)+'''');
                    end;
                end;
            end;
        end;
    end;
  end;
begin
  if Drawing = nil then exit;

  RulerRect := RectToOrigin(fPB.BoundsRect );
  PosSB := SBPosition;

{ Setup and draw the ruler background }
  fPB.Canvas.Brush.Color := clYellow;
  fPB.Canvas.Brush.Style := bsSolid;
  fPb.Canvas.Pen.Width := 1;
  fPB.Canvas.Pen.Style := psSolid;
  fPB.Canvas.Pen.Color := clBlack;

  fPB.Canvas.FillRect( RulerRect );

{ Determine the orientation of the ruler }

  DisplayWidth  := RulerRect.Right;
  DisplayHeight := RulerRect.Bottom;

  Horiz := DisplayHeight < DisplayWidth;

{ Outline the ruler in black }

fPB.Canvas.Rectangle( RulerRect );

{ Draw the appropriate ruler }

  if Horiz then
    Horizontal
  else
    Vertical;

end;

procedure TRuler.OriginToLowerLeft(Drawing: TDrawing);
begin
//    LeftPixels := MicronsToPixels( PosSB * ( XMax - WidthMicrons - XMin) + XMin, Drawing.Preferences );
//  LeftMicrons = PosSB * (XMax - WidthMicrons - XMin) + XMin;
// 0 = PosSb * (XMax - WidthMicrons - XMin ) + XMin - LeftMicrons

//  PosSb * (XMax - WidthMicrons - XMin ) =  - XMin + LeftMicrons
//  PosSb =  (LeftMicrons - XMin) / (XMax - WidthMicrons - XMin )

end;

function TRuler.SBPosition: Double;
begin
  Result := fScrollBar.Position / fScrollBar.Max;
end;

end.

