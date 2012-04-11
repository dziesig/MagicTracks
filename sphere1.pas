unit Sphere1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, DrawingCommon1, DrawingObject1, Persistent1,
  Preferences1, ExtCtrls, ThreePoint1;

type

  { TSphere }

  TSphere = class(TDrawingObject)
  private
//    fRX, fRY, fRZ : Double;
    fRadii : T3Point;
    //procedure SetRX(const AValue: Double);
    //procedure SetRY(const AValue: Double);
    //procedure SetRZ(const AValue: Double);
    procedure SetRadii(const AValue: T3Point);
  protected
    procedure LoadCommon( var F : TextFile ); override;
  public
    constructor Create( aParent : TPersistentZ = nil); override;
    constructor Create( var F   : TextFile;
                        aParent : TPersistentZ = nil ); virtual;

    destructor Destroy; override;

    procedure MakeNew; override;
    procedure Save( var F : TextFile ); override;
    procedure Load( var F : TextFile ); override;
    procedure Assign( Source : TPersistentZ ); override;

    procedure Draw( PaintBox : TPaintBox; Box : TDrawingBox; Preferences : TPreferences ); override;

    property Radii : T3Point read fRadii write SetRadii;
    //property RX : Double read fRX write SetRX;
    //property RY : Double read fRY write SetRY;
    //property RZ : Double read fRZ write SetRZ;

  end;


implementation

uses
  Math;

////////////////////////////////////////////////////////////////////////////////
// Code to draw rotated elipse http://www.angusj.com/delphitips/ellipses.php
////////////////////////////////////////////////////////////////////////////////
procedure RotatePts(var pts: array of TPoint;
  origin: TPoint; radians: single);
var
  i,x,y: integer;
  cosAng, sinAng: single;
begin
  cosAng := cos(radians);
  sinAng := sin(radians);
  for i := low(pts) to high(pts) do
  begin
    x := pts[i].X - origin.X;
    y := pts[i].Y - origin.Y;
    pts[i].X := round((x * cosAng) - (y * sinAng)) + origin.X;
    pts[i].Y := round((x * sinAng) + (y * cosAng)) + origin.Y;
  end;
end;

//see - http://www.codeguru.com/Cpp/G-M/gdi/article.php/c131

procedure DrawRotatedEllipse(canvas: TCanvas;
  rec: TRect; degrees: integer);
const
  //Magic constant = 2/3*(sqrt(2)-1)
  offset: single = 0.27614237;
var
  midx, midy, offx, offy: integer;
  pts: array [0..12] of TPoint;
  radians: single;
begin
  degrees := degrees mod 360;
  if degrees < 0 then inc(degrees, 360);
  radians := degrees *pi / 180;

  //if there's no rotation, use the standard Windows function
  if radians = 0 then
    canvas.Ellipse(rec)
  else
    begin
      with rec do
        begin
          dec(right); dec(bottom); //empirically this seems better
          midx := (right + left) div 2;
          midy := (bottom + top) div 2;
          offx := round((right - left) * offset);
          offy := round((bottom - top) * offset);
          pts[0]  := Point(left, midy);
          pts[1]  := Point(left, midy - offy);
          pts[2]  := Point(midx - offx, top);
          pts[3]  := Point(midx, top);
          pts[4]  := Point(midx + offx, top);
          pts[5]  := Point(right, midy - offy);
          pts[6]  := Point(right, midy);
          pts[7]  := Point(right, midy + offy);
          pts[8]  := Point(midx + offx, bottom);
          pts[9]  := Point(midx, bottom);
          pts[10] := Point(midx - offx, bottom);
          pts[11] := Point(left, midy + offy);
          pts[12] := pts[0];
          //rotate all points about the ellipse center ...
          RotatePts(pts, Point(midx,midy), radians);
        end;
      //with canvas do
      //  begin
      //    beginPath(Handle);
      //    canvas.PolyBezier(pts);
      //    EndPath(Handle);
      //    if canvas.Brush.Style = bsClear then
      //      StrokePath(Handle)
      //    else
      //      StrokeAndFillPath(Handle);
      //  end;
    end;
end;////////////////////////////////////////////////////////////////////////////////

{ TSphere }

const
  CurrentVersion = 1;

procedure TSphere.Assign(Source: TPersistentZ);
begin
  inherited Assign(Source);
end;

constructor TSphere.Create(var F: TextFile; aParent: TPersistentZ);
begin
  inherited Create(aParent);
  MakeNew;
  LoadCommon( F );
end;

constructor TSphere.Create(aParent: TPersistentZ);
begin
  inherited Create(aParent);
  MakeNew;
end;

destructor TSphere.Destroy;
begin
  fRadii.Free;
  inherited Destroy;
end;

procedure TSphere.Draw(PaintBox: TPaintBox; Box: TDrawingBox;
  Preferences: TPreferences);
var
  XX, YY, xRX, xRY : Integer;
  OldSIze : Integer;
begin
  XX := PixelsX( Origin, Box, Preferences, PaintBox );
  YY := PixelsY( Origin, Box, Preferences, PaintBox );
  OldSize := PaintBox.Canvas.Pen.Width;
  try
    PaintBox.Canvas.Pen.Width := 1;
    PaintBox.Canvas.MoveTo( XX-1,YY-1 );
    PaintBox.Canvas.LineTo( XX+1,YY+1 );
  finally
    PaintBox.Canvas.Pen.Width := OldSize;
  end;
end;

procedure TSphere.Load(var F: TextFile);
var
  S : String;
  V : Integer;
begin
  Readln(F,S);
  if S <> '<Sphere>' then
    raise Exception.Create('Start of Sphere.');
  LoadCommon( F );
end;

procedure TSphere.LoadCommon(var F: TextFile);
var
  S : String;
  V : Integer;
begin
  Readln(F,V);
  if V >= 1 then
    begin
      fRadii.Load(F);
      //Readln(F,fRX);
      //Readln(F,fRY);
      //Readln(F,fRZ);
    end;
  inherited Load(F);
  Readln(F,S);
  if S <> '</Sphere>' then
    raise Exception.Create('End of Sphere.');
end;

procedure TSphere.MakeNew;
begin
  inherited MakeNew;
  Radii :=T3Point.Create( Self );
  //fRX := 0;
  //fRY := 0;
  //fRZ := 0;
end;

procedure TSphere.Save(var F: TextFile);
begin
  Writeln(F,'<Sphere>');
  Writeln(F,CurrentVersion);
  fRadii.Save( F );
  //Writeln(F,fRX);
  //Writeln(F,fRY);
  //Writeln(F,fRZ);
  inherited Save(F);
  Writeln(F,'</Sphere>');
end;

procedure TSphere.SetRadii(const AValue: T3Point);
begin
  if fRadii=AValue then exit;
  fRadii:=AValue;
end;

//procedure TSphere.SetRX(const AValue: Double);
//begin
//  Update(fRX,AValue);
//end;
//
//procedure TSphere.SetRY(const AValue: Double);
//begin
//  Update(fRY,AValue);
//end;
//
//procedure TSphere.SetRZ(const AValue: Double);
//begin
//  Update(fRZ,AValue);
//end;

end.

