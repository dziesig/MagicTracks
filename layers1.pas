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
  Classes, SysUtils, Graphics, Persistent1, DrawingObject1, DrawingCommon1,
  Preferences1, ExtCtrls;

type

  TLayerKinds = ( lkBuilding,
                  lkFramework,
                  lkElectrical,
                  lkTrack,
                  lkScenery,
                  lkStructures );

  TViewLayers = set of TLayerKinds;

  { TLayer }

  TLayer = class(TPersistentz)
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

      constructor Create( AParent : TPersistentz = nil ); virtual;
      procedure PutToConfig( ConfigFileName : String );
      procedure GetFromConfig( ConfigFile : String );

      procedure Assign( Source : TPersistentz ); override;

      procedure Load( var F : TextFile ); override;
      procedure Save( var F : TextFile ); override;

      procedure Draw(Paintbox :  TPaintBox; Box : TDrawingBox; Preferences : TPreferences );

      function Drawing : TObject;

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

  TLayers = class(TPersistentList)
    private
      fActiveLayer  : TLayerKinds;
      fViewLayers   : TViewLayers;
      procedure SetActiveLayer(const AValue: TLayerKinds);
      procedure SetViewLayers(const AValue: TViewLayers);

      procedure ReadViewLayers( var F : TextFile; var Value : TViewLayers);
      procedure WriteViewLayers( var F : TextFile; Value : TViewLayers);
    public
      constructor Create( aParent : TPersistentZ = nil ); override;
      destructor  Destroy; override;

      procedure PutToConfig( ConfigFileName : String );
      procedure GetFromConfig( ConfigFileName : String );
      procedure Update( var Data : TLayerKinds; NewValue : TLayerKinds );  overload;
      procedure Update( var Data : TViewLayers; NewValue : TViewLayers );  overload;

      procedure Assign( Source : TPersistentz ); override;

      procedure SetViewLayer( Layer : TLayerKinds );
      procedure ClearViewLayer( Layer : TLayerKinds );

      procedure Load( var F : TextFile ); override;
      procedure Save( var F : TextFile ); override;

      function Drawing : TObject;

      property ActiveLayerKind : TLayerKinds read fActiveLayer write SetActiveLayer;
      property ViewLayers  : TViewLayers read fViewLayers write SetViewLayers;
  end;

const
  LayerNames : array[TLayerKinds] of String =
    ( 'Building', 'Framework', 'Electrical', 'Track', 'Scenery', 'Structures' );

implementation

uses
  IniFiles, Main1;

{ TLayers }

const

  CurrentVersion       = 2;    // Individual Layer
  CurrentLayersVersion = 1; // List of Layers

{ TLayer }

procedure TLayer.Assign(Source: TPersistentz);
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

constructor TLayer.Create(AParent: TPersistentz);
begin
  fDrawingObjects := TDrawingObjects.Create( AParent );
end;

procedure TLayer.Draw(PaintBox: TPaintBox; Box: TDrawingBox; Preferences : TPreferences);
var
  I : Integer;
  O : TDrawingObject;
begin
  for I := 0 to pred(fDrawingObjects.Count) do
    begin
      O := TDrawingObject(fDrawingObjects.Items[I]);
      O.Draw( PaintBox, Box, Preferences );
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

procedure TLayer.Load(var F: TextFile);
var
  S : String;
  V : Integer;
  I : Integer;
  C : Integer;
  D : TDrawingObject;
begin
  MakeNew;
  Readln(F, S);
  if S <> '<Magic Track Layer>' then
    raise Exception.Create('Start of Layer');
  Readln(F, V );
// Read based on version;
  if V >= 1 then
    begin
      Readln(F, fName );
      Readln(F, fLineColor);
      Readln(F, fSurfaceColor);
      Readln(F, fLineStyle);
      Readln(F, fLineSize);
      Readln(F, fRailSize);
      Readln(F, fLineStart);
      Readln(F, fLineEnd);
      Readln(F, fTrackType);
    end;
  if V >= 2 then
    begin
      fDrawingObjects.Load(F);
    end;
  Readln(F, S);
  if S <> '</Magic Track Layer>' then
    raise Exception.Create('End of Layer');
  inherited Load(F);
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

procedure TLayer.Save(var F: TextFile);
var
  I : Integer;
begin
  inherited Save(F);
  Writeln(F,'<Magic Track Layer>' );
  Writeln(F, CurrentVersion );
  // Version 1
  Writeln(F, Name );
  Writeln(F, fLineColor);
  Writeln(F, fSurfaceColor);
  Writeln(F, fLineStyle);
  Writeln(F, fLineSize);
  Writeln(F, fRailSize);
  Writeln(F, fLineStart);
  Writeln(F, fLineEnd);
  Writeln(F, fTrackType);
  // Version 2
  fDrawingObjects.Save( F );
  Writeln(F,'</Magic Track Layer>' );
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

procedure TLayers.Assign(Source: TPersistentz);
var
  S : TLayers;
  L : TLayer;
  I : Integer;
begin
  inherited Assign(Source);
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

constructor TLayers.Create( aParent : TPersistentZ = nil );
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

procedure TLayers.Load(var F: TextFile);
var
  S : String;
  V : Integer;
  I : TLayerKinds;
begin
  MakeNew;
  Readln(F, S);
  if S <> '<Magic Track Layers>' then
    raise Exception.Create('Start of Layers');
  Readln(F, V );
// Read based on version;
  if V >= 1 then
    begin
      Readln(F, fActiveLayer);
      ReadViewLayers(F, fViewLayers);
      for I in TLayerKinds do
        TLayer(Items[ord(I)]).Load(F);
    end;

  Readln(F, S);
  if S <> '</Magic Track Layers>' then
    raise Exception.Create('End of Layers');
  inherited Load(F);
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

procedure TLayers.ReadViewLayers(var F: TextFile; var Value: TViewLayers);
var
  I : TLayerKinds;
  V : Integer;
begin
  Value := [];
  for I in TLayerKinds do
    begin
      Readln(F,V);
      if V = 1 then
        Value := Value + [I];
    end;
end;

procedure TLayers.Save(var F: TextFile);
var
  I : TLayerKinds;
begin
  inherited;
  Writeln(F,'<Magic Track Layers>' );
  Writeln(F, CurrentLayersVersion );
  // Write based on version;
  Writeln(F, fActiveLayer);
  WriteViewLayers( F, fViewLayers );
  for I in TLayerKinds do
    TLayer(Items[ord(I)]).Save(F);

  Writeln(F,'</Magic Track Layers>' );
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

procedure TLayers.WriteViewLayers(var F: TextFile; Value: TViewLayers);
var
  I : TLayerKinds;
begin
  for I in TLayerKinds do
    if I in Value then
      Writeln(F, 1)
    else
      Writeln(F, 0);
end;

end.

