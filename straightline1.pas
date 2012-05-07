unit StraightLine1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  Persists1, TextIO1, DrawingCommon1, DrawingObject1, ExtCtrls,
  Preferences1, ThreePoint1, Breshenham1, Forms;

type

  { TStraightLine }

  TStraightLine = class(TDrawingObject)
  //protected
  //  procedure LoadCommon( var F : TextFile ); override;
  private
    fLineEnd : T3Point;
  public
    constructor Create( aParent : TPersists = nil); override;
    //constructor Create( var F    : TextFile;
    //                    aParent : TPersistentZ = nil ); override;
    procedure MakeNew; override;
    procedure Save( TextIO : TTextIO ); override;
    procedure Load( TextIO : TTextIO ); override;
    procedure Assign( Source : TPersists ); override;

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

procedure TStraightLine.Assign(Source: TPersists);
begin
  inherited Assign(Source);
end;

//constructor TStraightLine.Create(var F: TextFile; aParent: TPersistentZ);
//begin
//  inherited Create(F, aParent);
//  MakeNew;
//  LoadCommon( F );
//end;

constructor TStraightLine.Create(aParent: TPersists);
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
    begin
      DF.PaintBox1.Canvas.Pixels[BLine[I].X, BLine[I].Y] := clBlack;
      DF.DrawingObject(BLine[I].X, BLine[I].Y, Self,0);
    end;
  if Selected then
    begin
      DrawHandle(DF,BLine[0].X,BLine[0].Y,1);
      DrawHandle(DF,BLine[pred(Len)].X,BLine[pred(Len)].Y,2);
    end;

  Offset.free;
end;

procedure TStraightLine.Load( TextIO : TTextIO );
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
      fLineEnd.Load(TextIO);
    end;
  inherited Load(TextIO);  { TODO : Make sure that this inherited Load(F) is really needed/valid; }

  TextIO.Readln(S);             // Read the end of class
  CheckEndClass(S,ClsName);     // Assert end of class is correct and of correct format
  fModified := false;           // make sure this was NOT modified by the load.
 end;

//procedure TStraightLine.LoadCommon(var F: TextFile);
//var
//  S : String;
//  V : Integer;
//begin
//  Readln(F,V);
//  if V >= 1 then
//    begin
//      fLineEnd.Load(F);
//    end;
//  inherited Load(F);  { TODO : Make sure that this inherited Load(F) is really needed/valid; }
//  Readln(F,S);
//  if S <> '</Straight Line>' then
//    raise Exception.Create('End of Rectangular Solid.');
//end;

procedure TStraightLine.MakeNew;
begin
  inherited MakeNew;
  fLineEnd := T3Point.Create;
end;

procedure TStraightLine.Save( TextIO : TTextIO );
var
  S : String;
begin
  S := self.ClassName;          // Get our class name
  TextIO.Writeln('<'+S+'>');    // Write the start of class

  TextIO.Writeln(CurrentVersion);
  fLineEnd.Save(TextIO);
  inherited Save(TextIO);
  TextIO.Writeln('</'+S+'>');   // Write the end of class
  fModified := false;           // if it were modified, it isn't any more.
end;

procedure TStraightLine.SetLineEnd(XX, YY, ZZ: Double);
begin
  fLineEnd.X := XX;
  fLineEnd.Y := YY;
  fLineEnd.Z := ZZ;
end;

end.

