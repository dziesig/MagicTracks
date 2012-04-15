unit StraightLine1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Persistent1, DrawingCommon1, DrawingObject1, ExtCtrls,
  Preferences1, ThreePoint1, Breshenham1;

type

  { TStraightLine }

  TStraightLine = class(TDrawingObject)
  protected
  private
    fLineEnd : T3Point;
    procedure LoadCommon( var F : TextFile ); override;
  public
    constructor Create( aParent : TPersistentZ = nil); override;
    constructor Create( var F    : TextFile;
                        aParent : TPersistentZ = nil ); override;
    procedure MakeNew; override;
    procedure Save( var F : TextFile ); override;
    procedure Load( var F : TextFile ); override;
    procedure Assign( Source : TPersistentZ ); override;

    procedure Draw( PaintBox    : TPaintBox;
                    Box         : TDrawingBox;
                    Preferences : TPreferences ); override;

    procedure SetLineEnd( XX, YY, ZZ : Double );
    property LineEnd : T3Point read fLineEnd;
  end;

implementation

uses
  Graphics;

{ TStraightLine }

const
  CurrentVersion = 1;

procedure TStraightLine.Assign(Source: TPersistentZ);
begin
  inherited Assign(Source);
end;

constructor TStraightLine.Create(var F: TextFile; aParent: TPersistentZ);
begin
  inherited Create(F, aParent);
  MakeNew;
  LoadCommon( F );
end;

constructor TStraightLine.Create(aParent: TPersistentZ);
begin
  inherited Create(aParent);
  MakeNew;
end;

procedure TStraightLine.Draw(PaintBox: TPaintBox; Box: TDrawingBox;
  Preferences: TPreferences);
var
  Offset : T3Point;
  XX0, XX1 : Integer;
  YY0, YY1 : Integer;
  OldWidth : Integer;
  OldStyle : TPenStyle;
  BLine    : TPlotPoints;
  Len   : Integer;
  I : Integer;
begin
  Offset := T3Point.Create;
  Offset.Add(Origin);
  Offset.Add(fLineEnd);

  XX0 := PixelsX( Origin, Box, Preferences, PaintBox);
  XX1 := PixelsX( Offset, Box, Preferences, PaintBox);
  YY0 := PixelsY( Origin, Box, Preferences, PaintBox);
  YY1 := PixelsY( Offset, Box, Preferences, PaintBox);

  with PaintBox.Canvas do
    begin
      OldWidth := Pen.Width;
      OldStyle := Pen.Style;
      try
        Pen.Width := 3;
        Pen.Style := psSolid;
{$ifdef Canvas}
        MoveTo(XX0,YY0);
        LineTo(XX1,YY1);
{$else}
 //       SetLength( BLine, 1);
        BreshenhamLine( BLine, XX0, YY0, XX1, YY1 );
        Len := Length( BLine );
        for I := 0 to pred(Len) do
          PaintBox.Canvas.Pixels[BLine[I].X, BLine[I].Y] := clRed;
{$endif}
      finally
        Pen.Width := OldWidth;
        Pen.Style := OldStyle;
      end;
    end;
  Offset.free;
end;

procedure TStraightLine.Load(var F: TextFile);
var
  S : String;
  V : Integer;
begin
  Readln(F,S);
  if S <> '<Straight Line>' then
    raise Exception.Create('Start of Straight Line.');
  LoadCommon( F );
end;

procedure TStraightLine.LoadCommon(var F: TextFile);
var
  S : String;
  V : Integer;
begin
  Readln(F,V);
  if V >= 1 then
    begin
      fLineEnd.Load(F);
    end;
  inherited Load(F);  { TODO : Make sure that this inherited Load(F) is really needed/valid; }
  Readln(F,S);
  if S <> '</Straight Line>' then
    raise Exception.Create('End of Rectangular Solid.');
end;

procedure TStraightLine.MakeNew;
begin
  inherited MakeNew;
  fLineEnd := T3Point.Create;
end;

procedure TStraightLine.Save(var F: TextFile);
begin
  Writeln(F,'<Straight Line>');
  Writeln(F,CurrentVersion);
  fLineEnd.Save(F);
  inherited Save(F);
  Writeln(F,'</Straight Line>');
end;

procedure TStraightLine.SetLineEnd(XX, YY, ZZ: Double);
begin
  fLineEnd.X := XX;
  fLineEnd.Y := YY;
  fLineEnd.Z := ZZ;
end;

end.

