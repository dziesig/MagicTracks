unit StraightLine1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Persistent1, DrawingCommon1, DrawingObject1, ExtCtrls,
  Preferences1, ThreePoint1, Breshenham1, Forms;

type

  { TStraightLine }

  TStraightLine = class(TDrawingObject)
  protected
    procedure LoadCommon( var F : TextFile ); override;
  private
    fLineEnd : T3Point;
  public
    constructor Create( aParent : TPersistentZ = nil); override;
    constructor Create( var F    : TextFile;
                        aParent : TPersistentZ = nil ); override;
    procedure MakeNew; override;
    procedure Save( var F : TextFile ); override;
    procedure Load( var F : TextFile ); override;
    procedure Assign( Source : TPersistentZ ); override;

    //procedure Draw( PaintBox    : TPaintBox;
    //                Box         : TDrawingBox;
    //                Preferences : TPreferences ); override;

    procedure Draw( Frame       : TFrame;
                    Preferences : TPreferences;
                    ActiveLayer : Boolean ); override;

    procedure SetLineEnd( XX, YY, ZZ : Double );
    property LineEnd : T3Point read fLineEnd;
  end;

implementation

uses
  Graphics, DrawingFrame1;

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

procedure TStraightLine.Draw( Frame       : TFrame;
                              Preferences : TPreferences;
                              ActiveLayer : Boolean );
var
  DF : TDrawingFrame;
  Offset : T3Point;
  XX0, XX1 : Integer;
  YY0, YY1 : Integer;
  BLine    : TPlotPoints;
  Len   : Integer;
  I : Integer;
  TempColor : TColor;
  PenSize : Integer;
begin
  DF := Frame as TDrawingFrame;
  Offset := T3Point.Create;
  Offset.Add(Origin);
  Offset.Add(fLineEnd);

  XX0 := PixelsX( Origin, DF.BoxType, Preferences, DF.PaintBox1);
  XX1 := PixelsX( Offset, DF.BoxType, Preferences, DF.PaintBox1);
  YY0 := PixelsY( Origin, DF.BoxType, Preferences, DF.PaintBox1);
  YY1 := PixelsY( Offset, DF.BoxType, Preferences, DF.PaintBox1);

  // Code to help test drawing of active vs. inactive layers;

  BreshenhamLine( BLine, XX0, YY0, XX1, YY1 );
  Len := Length( BLine );
  for I := 0 to pred(Len) do
    DF.PaintBox1.Canvas.Pixels[BLine[I].X, BLine[I].Y] := clBlack;
  if Selected then
    begin
      PenSize := DF.PaintBox1.Canvas.Pen.Width;
      try
        DF.PaintBox1.Canvas.Pen.Width := 6;
        DF.PaintBox1.Canvas.MoveTo(BLine[0].X,BLine[0].Y);
        DF.PaintBox1.Canvas.LineTo(BLine[0].X+1, BLine[0].Y+1);
        DF.PaintBox1.Canvas.MoveTo(BLine[pred(Len)].X,BLine[pred(Len)].Y);
        DF.PaintBox1.Canvas.LineTo(BLine[pred(Len)].X+1, BLine[pred(Len)].Y+1);
      finally
        DF.PaintBox1.Canvas.Pen.Width := PenSize;
      end;

    end;
  for I := 0 to pred(Len) do
    DF.DrawingObject(BLine[I].X, BLine[I].Y, Self);

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

