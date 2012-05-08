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

unit Layers1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,

  DrawingObject1, DrawingCommon1, Generics1,
  Preferences1, ExtCtrls, Forms, Persists1, TextIO1;

type

  TLayerKinds = ( lkBuilding,
                  lkFramework,
                  lkElectrical,
                  lkTrack,
                  lkScenery,
                  lkStructures );

  TViewLayers = set of TLayerKinds;

  { TLayer }

  TLayer = class(TPersists)
    private
      fLineColor    : Integer;
      fSurfaceColor : Integer;
      fLineStyle    : Integer;
      fLineSize     : Integer;
      fRailSize     : Integer;
      fLineStart    : Integer;
      fLineEnd      : Integer;
      fTrackType    : Integer;

      fDrawingObjects : TDrawingObjects;

      procedure SetLineColor(const AValue: Integer);
      procedure SetLineEnd(const AValue: Integer);
      procedure setLineSize(const AValue: Integer);
      procedure setLineStart(const AValue: Integer);
      procedure setLineStyle(const AValue: Integer);
      procedure setRailSize(const AValue: Integer);
      procedure SetSurfaceColor(const AValue: Integer);
      procedure SetTrackType(const AValue: Integer);
    public

      constructor Create( AParent : TPersists = nil ); override;
      procedure PutToConfig( ConfigFileName : String );
      procedure GetFromConfig( ConfigFile : String );

      procedure Assign( Source : TPersists ); override;

      procedure Load( TextIO : TTextIO ); override;
      procedure Save( TextIO : TTextIO ); override;

      procedure Draw( Frame       : TFrame;
                      Preferences : TPreferences;
                      ActiveLayer : Boolean );

      function Drawing : TObject;

      procedure Deselect;

      property LineColor    : Integer read fLineColor    write SetLineColor;
      property SurfaceColor : Integer read fSurfaceColor write SetSurfaceColor;
      property LineStyle    : Integer read fLineStyle    write setLineStyle;
      property LineSize     : Integer read fLineSize     write setLineSize;
      property RailSize     : Integer read fRailSize     write setRailSize;
      property LineStart    : Integer read fLineStart    write setLineStart;
      property LineEnd      : Integer read fLineEnd      write SetLineEnd;
      property TrackType    : Integer read fTrackType    write SetTrackType;

      property DrawingObjects : TDrawingObjects read fDrawingObjects;

  end;

  { TLayers }

    TDrawingObjectList = specialize TMagicList<TLayer>;

  TLayers = class(TDrawingObjectList)
    private
      fActiveLayer  : TLayerKinds;
      fViewLayers   : TViewLayers;
      procedure SetActiveLayer(const AValue: TLayerKinds);
      procedure SetViewLayers(const AValue: TViewLayers);

      procedure ReadViewLayers(  TextIO : TTextIO; var Value : TViewLayers);
      procedure WriteViewLayers(  TextIO : TTextIO; Value : TViewLayers);
    public
      constructor Create( aParent : TPersists = nil ); override;
      destructor  Destroy; override;

      procedure PutToConfig( ConfigFileName : String );
      procedure GetFromConfig( ConfigFileName : String );
      procedure Update( var Data : TLayerKinds; NewValue : TLayerKinds );  overload;
      procedure Update( var Data : TViewLayers; NewValue : TViewLayers );  overload;

      procedure Assign( Source : TPersists ); override;

      procedure SetViewLayer( Layer : TLayerKinds );
      procedure ClearViewLayer( Layer : TLayerKinds );

      procedure Load( TextIO : TTextIO ); override;
      procedure Save( TextIO : TTextIO ); override;

      function Drawing : TObject;

      procedure Deselect;

      property ActiveLayerKind : TLayerKinds read fActiveLayer write SetActiveLayer;
      property ViewLayers  : TViewLayers read fViewLayers write SetViewLayers;
  end;

const
  LayerNames : array[TLayerKinds] of String =
    ( 'Building', 'Framework', 'Electrical', 'Track', 'Scenery', 'Structures' );

implementation

uses
  IniFiles, Main1, DrawingFrame1;

{ TLayers }

const

  CurrentVersion       = 2;    // Individual Layer
  CurrentLayersVersion = 1; // List of Layers

{ TLayer }

procedure TLayer.Assign(Source: TPersists);
var
  S : TLayer;
begin
  inherited Assign(Source);
  S := TLayer(Source);
  fLineColor    := S.fLineColor;
  fSurfaceColor := S.fSurfaceColor;
  fLineStyle    := S.fLineStyle;
  fLineSize     := S.fLineSize;
  fRailSize     := S.fRailSize;
  fLineStart    := S.fLineStart;
  fLineEnd      := S.fLineEnd;
  fTrackType    := S.fTrackType;
end;

constructor TLayer.Create(AParent: TPersists);
begin
  fDrawingObjects := TDrawingObjects.Create( AParent );
end;

procedure TLayer.Deselect;
var
  I : Integer;
  O : TDrawingObject;
begin
  for I := 0 to pred(fDrawingObjects.Count) do
    begin
      O := TDrawingObject(fDrawingObjects.Items[I]);
      O.Deselect;
    end;
end;

procedure TLayer.Draw(  Frame       : TFrame;
                        Preferences : TPreferences;
                        ActiveLayer : Boolean );
var
  DF : TDrawingFrame;
  I : Integer;
  O : TDrawingObject;
begin
  DF := Frame as TDrawingFrame;
  for I := 0 to pred(fDrawingObjects.Count) do
    begin
      O := TDrawingObject(fDrawingObjects.Items[I]);
      O.Draw( DF, Preferences, ActiveLayer );
    end;
end;

function TLayer.Drawing: TObject;
begin
  Result := MainForm.ActiveDrawing;
end;

procedure TLayer.GetFromConfig(ConfigFile: String);
var
  IniFile : TIniFile;
  Section : String;
begin
  Section := Name + ' Layer';
  IniFile := TIniFile.Create( ConfigFile );
  fLineColor    := IniFile.ReadInteger( Section,'Line Color',0 );
  fSurfaceColor := IniFile.ReadInteger( Section,'Surface Color', 0 );
  fLineStyle    := IniFile.ReadInteger( Section,'Line Style',0 );
  fLineSize     := IniFile.ReadInteger( Section,'Line Size',0 );
  fRailSize     := IniFile.ReadInteger( Section,'Rail Size',0 );
  fLineStart    := IniFile.ReadInteger( Section,'Line Start',0 );
  fLineEnd      := IniFile.ReadInteger( Section,'Line End',0 );
  fTrackType    := IniFile.ReadInteger( Section,'Track Type',0 );
  IniFile.Free;
end;

procedure TLayer.Load( TextIO : TTextIO );
var
  V : Integer;
  ClsName : String;
  S       : String;
  I : Integer;
  C : Integer;
  D : TDrawingObject;
begin
  MakeNew;
  ClsName := self.ClassName;    // Get the expected class name
  TextIO.ReadLn(S);             // Read the start of class
  CheckStartClass(S,ClsName);   // Assert they are correct and of correct format
  TextIO.Readln(V);
// Read based on version;
  if V >= 1 then
    begin
      TextIO.ReadLn( fName );
      TextIO.ReadLn( fLineColor);
      TextIO.ReadLn( fSurfaceColor);
      TextIO.ReadLn( fLineStyle);
      TextIO.ReadLn( fLineSize);
      TextIO.ReadLn( fRailSize);
      TextIO.ReadLn( fLineStart);
      TextIO.ReadLn( fLineEnd);
      TextIO.ReadLn( fTrackType);
    end;
  if V >= 2 then
    begin
      fDrawingObjects.Load(TextIO);
    end;
  TextIO.Readln(S);             // Read the end of class
  CheckEndClass(S,ClsName);     // Assert end of class is correct and of correct format
  fModified := false;           // make sure this was NOT modified by the load.
//  inherited Load(F);
end;

procedure TLayer.PutToConfig(ConfigFileName: String);
var
  IniFile : TIniFile;
  Section : String;
begin
  Section := Name + ' Layer';
  IniFile := TIniFile.Create( ConfigFileName );
  IniFile.WriteInteger( Section,'Line Color', fLineColor );
  IniFile.WriteInteger( Section,'Surface Color', fSurfaceColor );
  IniFile.WriteInteger( Section,'Line Style',fLineStyle );
  IniFile.WriteInteger( Section,'Line Size',fLineSize );
  IniFile.WriteInteger( Section,'Rail Size',fRailSize );
  IniFile.WriteInteger( Section,'Line Start',fLineStart );
  IniFile.WriteInteger( Section,'Line End',fLineEnd );
  IniFile.WriteInteger( Section,'Track Type',fTrackType );
  IniFile.Free;
end;

procedure TLayer.Save( TextIO : TTextIO );
var
  S : String;
begin
  S := self.ClassName;          // Get our class name
  TextIO.Writeln('<'+S+'>');    // Write the start of class

  TextIO.Writeln(CurrentVersion);
  // Version 1
  TextIO.Writeln( Name );
  TextIO.Writeln( fLineColor);
  TextIO.Writeln( fSurfaceColor);
  TextIO.Writeln( fLineStyle);
  TextIO.Writeln( fLineSize);
  TextIO.Writeln( fRailSize);
  TextIO.Writeln( fLineStart);
  TextIO.Writeln( fLineEnd);
  TextIO.Writeln( fTrackType);
  // Version 2
  fDrawingObjects.Save( TextIO );
  TextIO.Writeln('</'+S+'>');   // Write the end of class
  fModified := false;           // if it were modified, it isn't any more.
end;

procedure TLayer.SetLineColor(const AValue: Integer);
begin
  if fLineColor=AValue then exit;
  fLineColor:=AValue;
end;

procedure TLayer.SetLineEnd(const AValue: Integer);
begin
  if fLineEnd=AValue then exit;
  fLineEnd:=AValue;
end;

procedure TLayer.setLineSize(const AValue: Integer);
begin
  if fLineSize=AValue then exit;
  fLineSize:=AValue;
end;

procedure TLayer.setLineStart(const AValue: Integer);
begin
  if fLineStart=AValue then exit;
  fLineStart:=AValue;
end;

procedure TLayer.setLineStyle(const AValue: Integer);
begin
  if fLineStyle=AValue then exit;
  fLineStyle:=AValue;
end;

procedure TLayer.setRailSize(const AValue: Integer);
begin
  if fRailSize=AValue then exit;
  fRailSize:=AValue;
end;

procedure TLayer.SetSurfaceColor(const AValue: Integer);
begin
  if fSurfaceColor=AValue then exit;
  fSurfaceColor:=AValue;
end;

procedure TLayer.SetTrackType(const AValue: Integer);
begin
  if fTrackType=AValue then exit;
  fTrackType:=AValue;
end;

procedure TLayers.Assign(Source: TPersists);
var
  S : TLayers;
  L : TLayer;
  I : Integer;
begin
//  inherited Assign(Source);
  S := TLayers( Source );
  fViewLayers := S.fViewLayers;
  fActiveLayer := S.fActiveLayer;
  for I := 0 to 3 do
    begin
      L := TLayer(Items[I]);
      L .Assign( TLayer(S.Items[I]));
    end;
end;

procedure TLayers.ClearViewLayer(Layer: TLayerKinds);
begin
  if Layer in ViewLayers then
    begin
      ViewLayers := ViewLayers - [Layer];
      fModified := true;
    end;
end;

constructor TLayers.Create( aParent : TPersists = nil );
var
  Kind      : TLayerKinds;
  Layer     : TLayer;
begin
  inherited Create(aParent);
  for Kind := lkBuilding to lkStructures do
    begin
      Layer := TLayer.Create(aParent);
      Layer.Name := LayerNames[Kind];
      Add(Layer);
    end;

end;

procedure TLayers.Deselect;
begin
  TLayer(Items[ord(fActiveLayer)]).Deselect;
//  fActiveLayer.Deselect;
end;

destructor TLayers.Destroy;
begin
  inherited Destroy;
end;

function TLayers.Drawing: TObject;
begin
  Result := Parent;
end;

procedure TLayers.GetFromConfig(ConfigFileName: String);
var
  Kind      : TLayerKinds;
  IniFile   : TIniFile;
  View      : Integer;
begin
  IniFile := TIniFile.Create( ConfigFileName );
  fActiveLayer := TLayerKinds(IniFile.ReadInteger('Layers','ActiveLayer',0));
  fViewLayers := [];
  for Kind in TLayerKinds do
    begin
      View := IniFile.ReadInteger('View Layer', LayerNames[Kind], 0);
      if View > 0 then fviewLayers := fViewLayers + [Kind];
    end;
  IniFile.Free;
  for Kind in TLayerKinds do
    TLayer(Items[ord(Kind)]).GetFromConfig(ConfigFileName);
end;

procedure TLayers.Load( TextIO : TTextIO );
var
  V : Integer;
  ClsName : String;
  S       : String;
  I       : TLayerKinds;
  Temp    : Integer;
begin
  ClsName := self.ClassName;    // Get the expected class name
  TextIO.ReadLn(S);             // Read the start of class
  CheckStartClass(S,ClsName);   // Assert they are correct and of correct format
  TextIO.Readln(V);
  MakeNew;
// Read based on version;
  if V >= 1 then
    begin
      TextIO.Readln( Temp);
      fActiveLayer := TLayerKinds( Temp );
      ReadViewLayers( TextIO, fViewLayers );
      for I in TLayerKinds do
        TLayer(Items[ord(I)]).Load(TextIO);
    end;

  TextIO.Readln(S);             // Read the end of class
  CheckEndClass(S,ClsName);     // Assert end of class is correct and of correct format
  fModified := false;           // make sure this was NOT modified by the load.
end;

procedure TLayers.PutToConfig(ConfigFileName: String);
var
  Kind      : TLayerKinds;
  IniFile   : TIniFile;
  View      : Integer;
begin
  IniFile := TIniFile.Create( ConfigFileName );
  IniFile.WriteInteger('Layers','ActiveLayer',ord(fActiveLayer));
  for Kind in TLayerKinds do
    begin
      if Kind in fViewLayers then
        View := 1
      else
        View := 0;
      IniFile.WriteInteger('View Layer', LayerNames[Kind], View);
    end;
  IniFile.Free;
  for Kind in TLayerKinds do
    begin
      TLayer(Items[ord(Kind)]).PutToConfig(ConfigFileName);;
    end;
end;

procedure TLayers.ReadViewLayers( TextIO : TTextIO; var Value: TViewLayers);
var
  I : TLayerKinds;
  V : Integer;
begin
  Value := [];
  for I in TLayerKinds do
    begin
      TextIO.Readln(V);
      if V = 1 then
        Value := Value + [I];
    end;
end;

procedure TLayers.Save( TextIO : TTextIO );
var
  I : TLayerKinds;
  S : String;
begin
  S := self.ClassName;          // Get our class name
  TextIO.Writeln('<'+S+'>');    // Write the start of class

  TextIO.Writeln(CurrentVersion);
  // Write based on version;
  TextIO.Writeln(ord(fActiveLayer));
  WriteViewLayers( TextIO, fViewLayers );
  for I in TLayerKinds do
    TLayer(Items[ord(I)]).Save(TextIO);
  TextIO.Writeln('</'+S+'>');   // Write the end of class
  fModified := false;           // if it were modified, it isn't any more.
end;

procedure TLayers.SetActiveLayer(const AValue: TLayerKinds);
begin
  Update(fActiveLayer,AValue);
end;

procedure TLayers.SetViewLayer(Layer: TLayerKinds);
begin
  if not (Layer in ViewLayers) then
    begin
      ViewLayers := ViewLayers + [Layer];
      fModified := true;
    end;

end;

procedure TLayers.SetViewLayers(const AValue: TViewLayers);
begin
  Update(fViewLayers,AValue);
end;

procedure TLayers.Update(var Data: TLayerKinds; NewValue: TLayerKinds);
begin
  fModified := fModified or (Data <> NewValue);
  Data := NewValue;
end;

procedure TLayers.Update(var Data: TViewLayers; NewValue: TViewLayers);
begin
  fModified := fModified or (Data <> NewValue);
  Data := NewValue;
end;

procedure TLayers.WriteViewLayers( TextIO : TTextIO; Value: TViewLayers);
var
  I : TLayerKinds;
begin
  for I in TLayerKinds do
    if I in Value then
      TextIO.Writeln(1)
    else
      TextIO.Writeln(0);
end;

end.

