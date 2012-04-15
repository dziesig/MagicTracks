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

// This unit implements Breshenham's Line Drawing algorithm (for straight lines)
// and derivatives for other figures (Circle, Ellipse).

unit Breshenham1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TPlotPoint = record
    X, Y : Integer;
  end;

  TPlotPoints = array of TPlotPoint;

procedure BreshenhamLine( var Line : TPlotPoints; X0, Y0, X1, Y1 : Integer );

implementation

procedure BreshenhamLine(var Line : TPlotPoints; X0, Y0, X1, Y1: Integer);
var
  DX, DY, SX, SY : Integer;
  Err : Integer;
  E2  : Integer;
  Len : Integer;

  procedure SetPixel( X, Y : Integer );
  begin
    Line[Len].X := X;
    Line[Len].Y := Y;
    Inc(Len);
    if Len >= Length(Line) then
      SetLength(Line,2*Length(Line));
  end;

begin
  Len := 0;
  SetLength(Line,1);
  dx := abs(x1-x0);
  dy := abs(y1-y0);
  if x0 < x1 then sx := 1 else sx := -1;
  if y0 < y1 then sy := 1 else sy := -1;
  err := dx-dy;

  while true do
    begin
      setPixel(x0,y0);
      if (x0 = x1) and (y0 = y1) then  break; // loop
      e2 := 2*err;
      if e2 > -dy then
        begin
          err := err - dy;
          x0 := x0 + sx;
        end;
      if e2 <  dx then
        begin
          err := err + dx;
          y0 := y0 + sy;
        end;
    end;
  SetLength(Line,Len);
end;

end.

