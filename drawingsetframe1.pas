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

unit DrawingSetFrame1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls,
  DrawingCommon1, Drawing1, DrawingFrame1;

type

  { TDrawingSetFrame }

  TDrawingSetFrame = class(TFrame)
    DrawingFrameYZ: TDrawingFrame;
    DrawingFrameXZ: TDrawingFrame;
    DrawingFrameXY: TDrawingFrame;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure FrameResize(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
  private
    { private declarations }
    fDrawing : TDrawing;

    vVSplit, vHSplit : Integer;
    procedure SetDrawing(const AValue: TDrawing);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;

    procedure MoveTo( X, Y, Z : Double ); // Units in Microns
    procedure LineTo( X, Y, Z : Double );

    procedure PositionLowerLeft;

    procedure Invalidate; override;

    property Drawing : TDrawing read fDrawing write SetDrawing;
  end; 

implementation

{$R *.lfm}

uses
  UnitConversion1;

{ TDrawingSetFrame }


procedure TDrawingSetFrame.Splitter1Moved(Sender: TObject);
begin
  vVSplit := (Panel1.Width *100) div Width;
  vHSplit := (Panel3.Height * 100) div Height;
end;

procedure TDrawingSetFrame.FrameResize(Sender: TObject);
begin
  Panel1.Width := (Width * vVsplit) div 100;
  Panel3.Height := (Height * vHsplit) div 100;
end;

procedure TDrawingSetFrame.Invalidate;
begin
  DrawingFrameXY.Invalidate;
  DrawingFrameXZ.Invalidate;
  DrawingFrameYZ.Invalidate;
end;

procedure TDrawingSetFrame.SetDrawing(const AValue: TDrawing);
begin
  fDrawing:=AValue;
  DrawingFrameXY.Drawing := fDrawing;
  DrawingFrameXZ.Drawing := fDrawing;
  DrawingFrameYZ.Drawing := fDrawing;

  DrawingFrameXY.BoxType := XY;
  DrawingFrameXZ.BoxType := XZ;
  DrawingFrameYZ.BoxType := YZ;

end;

constructor TDrawingSetFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  vVSplit := 50;
  vHSplit := 50;
  Panel1.Width := (Width * vVsplit) div 100;
  Panel3.Height := (Height * vHsplit) div 100;
end;

procedure TDrawingSetFrame.MoveTo(X, Y, Z: Double);
var
  XX, YY : Integer;
begin
  XX := MicronsToPixels( X - DrawingFrameXY.Drawing.MinX[XY], DrawingFrameXY.Drawing.Preferences );
  YY := MicronsToPixels( Y - DrawingFrameXY.Drawing.MinY[XY], DrawingFrameXY.Drawing.Preferences );
  XX := 0; YY := 0;
  DrawingFrameXY.PaintBox1.Canvas.MoveTo( XX, YY );
end;

procedure TDrawingSetFrame.LineTo(X, Y, Z: Double);
var
  XX, YY : Integer;
begin
  XX := MicronsToPixels( X - DrawingFrameXY.Drawing.MinX[XY], DrawingFrameXY.Drawing.Preferences );
  YY := MicronsToPixels( Y - DrawingFrameXY.Drawing.MinY[XY], DrawingFrameXY.Drawing.Preferences );
  XX := DrawingFrameXY.Canvas.Width;
  yy := drawingframexy.canvas.height;
  DrawingFrameXY.PaintBox1.Canvas.LineTo( XX, YY );
end;

procedure TDrawingSetFrame.PositionLowerLeft;
begin
  DrawingFrameXY.PositionLowerLeft;
  DrawingFrameXZ.PositionLowerLeft;
  DrawingFrameYZ.PositionLowerLeft;
end;

end.

