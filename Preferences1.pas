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

unit Preferences1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  Persists1, TextIO1;

type

  { TPreferences }

  TPreferences = class(TPersists)
  private
    fEnglishUnits       : Boolean;   { Drawing is displayed in Feet, etc. }
    fInchesAndFractions : Boolean;   { If English, use e.g. 5'3 1/8", else 5.26042 }
    fCentimeters        : Boolean;   { If not English use CM, else Meters }
    fSnapToGrid         : Boolean;
    fShowGrid           : Boolean;
    fGridSpacingEnglish : Double;
    fGridSpacingMetric  : Double;
    fZoomIndex          : Integer;
    fScaleIndex         : Integer;

    function GetGridSpacingMicrons: Double;
    function GetScale: Double;
    function GetZoom: Double;
    procedure SetCentimeters(const AValue: Boolean);
    procedure SetEnglishUnits(const AValue: Boolean);
    procedure SetGridSpacingEnglish(const AValue: Double);
    procedure SetGridSpacingMetric(const AValue: Double);
    procedure SetInchesAndFractions(const AValue: Boolean);
    procedure SetScaleIndex(const AValue: Integer);
    procedure SetShowGrid(const AValue: Boolean);
    procedure SetSnapToGrid(const AValue: Boolean);
    procedure SetZoomIndex(const AValue: Integer);
  public
    constructor Create( aParent : TPersists = nil); override;

    procedure MakeNew; override;

    //procedure Save( var F : TextFile ); override;
    //procedure Load( var F : TextFile ); override;
    procedure Save( TextIO : TTextIO ); override;
    procedure Load( TextIO : TTextIO ); override;

    procedure Assign( Source : TPersists ); override;

    procedure PutToConfig( ConfigFileName : String );
    procedure GetFromConfig( ConfigFilename : String );

    property EnglishUnits : Boolean read fEnglishUnits write SetEnglishUnits;
    property InchesAndFractions : Boolean read fInchesAndFractions write SetInchesAndFractions;
    property Centimeters        : Boolean read fCentimeters        write SetCentimeters;
    property SnapToGrid         : Boolean read fSnapToGrid         write SetSnapToGrid;
    property ShowGrid           : Boolean read fShowGrid           write SetShowGrid;
    property GridSpacingEnglish : Double  read fGridSpacingEnglish write SetGridSpacingEnglish;
    property GridSpacingMetric  : Double  read fGridSpacingMetric  write SetGridSpacingMetric;
    property GridSpacingMicrons : Double  read GetGridSpacingMicrons;
    property Zoom               : Double  read GetZoom;
    property Scale              : Double  read GetScale;

    property ZoomIndex          : Integer read fZoomIndex          write SetZoomIndex;
    property ScaleIndex         : Integer read fScaleIndex         write SetScaleIndex;
  end;

const
  MaxZoom = 19;
  Zooms : array [0..MaxZoom] of Double =
    (  1,  2,  3,  4,  5,  6,  7,  8,  9, 10,   { 0 .. 9 }
      12, 15, 20, 25, 30, 40, 50, 60, 70, 80);  { 10 .. 19 }

  MaxScales = 7;
  Scales : array [0..MaxScales-1] of Double =
    (
      22.5,  48.0,  64.0, 87.5, 120.0, 160.0, 220.0
    );
  ScaleNames : array [0..MaxScales-1] of String =
    (
      'G', 'O', 'S', 'HO', 'TT', 'N', 'Z'
    );
implementation

uses
  IniFiles, Common1;

{ TPreferences }

const
  CurrentVersion = 3;

procedure TPreferences.Assign(Source: TPersists);
var
  S : TPreferences;
begin
  { TODO 2 -oMe -cModified : Review the status of MODIFIED when Assigned is called.  It may need to be copied }
  inherited Assign(Source);
  S := TPreferences(Source);
  EnglishUnits       := S.EnglishUnits;
  InchesAndFractions := S.InchesAndFractions;
  Centimeters        := S.Centimeters;

  SnapToGrid         := S.SnapToGrid;
  ShowGrid           := S.ShowGrid;
  GridSpacingEnglish := S.GridSpacingEnglish;
  GridSpacingMetric  := S.GridSpacingMetric;
  ZoomIndex          := S.ZoomIndex;
  ScaleIndex         := S.ScaleIndex;
end;

constructor TPreferences.Create(aParent: TPersists);
begin
  inherited Create(aParent);
end;

procedure TPreferences.GetFromConfig(ConfigFilename: String);
var
  IniFile : TIniFile;
begin
  IniFile := TIniFile.Create(ConfigFileName);

  EnglishUnits       := IniFile.ReadBool('Units','English',true);
  InchesAndFractions := IniFile.ReadBool('Units','InchesAndFractions',false);
  Centimeters        := IniFile.ReadBool('Units','Centimeters',false);

  SnapToGrid         := IniFile.ReadBool('Grid','SnapTo',true);
  ShowGrid           := IniFile.ReadBool('Grid','Show',true);
{ Note: Grid Spacing English defaults to 1/4" but is stored in microns (as are all other coordinates)}
  GridSpacingEnglish := IniFile.ReadFloat('Grid','SpacingEnglish',76200);
  GridSpacingMetric  := IniFile.ReadFloat('Grid','SpacingMetric',10000.0);

  ZoomIndex          := IniFile.ReadInteger('View','Zoom',9);
  ScaleIndex         := IniFile.ReadInteger('Scale','Scale',4);

  IniFile.Free;

  UNMODIFY;
end;

function TPreferences.GetZoom: Double;
begin
  Result := Zooms[fZoomIndex];
end;

function TPreferences.GetGridSpacingMicrons: Double;
begin
  if fEnglishUnits then
    Result := fGridSpacingEnglish
  else
    Result := fGridSpacingMetric;
end;

function TPreferences.GetScale: Double;
begin
  Result := Scales[fScaleIndex];
end;

procedure TPreferences.Load(TextIO : TTextIO );
var
  S : String;
  V : Integer;
  ClsName : String;
begin
  ClsName := self.ClassName;    // Get the expected class name
  TextIO.ReadLn(S);             // Read the start of class
  CheckStartClass(S,ClsName);   // Assert they are correct and of correct format
//  TextIO.Readln(S);             // Read the Object's Name
//  Name := S;
  //TextIO.Readln(S);             // Read the end of class
  //CheckEndClass(S,ClsName);     // Assert end of class is correct and of correct format
  //fModified := false;           // make sure this was NOT modified by the load.

  //Readln(F,S);
  //if S <> '<Preferences>' then
  //  raise Exception.Create('Sync Error:  Preferences Start');
  MakeNew;
  TextIO.Readln(V);
  if V >= 1 then
    begin
      TextIO.Readln(fEnglishUnits);
      //ReadBool(F,fEnglishUnits);
      TextIO.Readln(fInchesAndFractions);
      TextIO.Readln(fCentimeters);
      TextIO.Readln(fSnapToGrid);
      TextIO.Readln(fShowGrid);
      TextIO.Readln(fGridSpacingEnglish);
      TextIO.Readln(fGridSpacingMetric);
    end;
  if V >= 2 then
    begin
      TextIO.Readln(fZoomIndex);
    end;
  if V >= 3 then
    begin
      TextIO.Readln(fScaleIndex);
    end;
//  Readln(F,S);

  TextIO.Readln(S);             // Read the end of class
  CheckEndClass(S,ClsName);     // Assert end of class is correct and of correct format
//  inherited Load(TextIO);
end;

procedure TPreferences.MakeNew;
begin
  fZoomIndex := 9;
  fScaleIndex := 4;
  inherited MakeNew;
end;

procedure TPreferences.PutToConfig(ConfigFileName: String);
var
  IniFile : TIniFile;
begin
  IniFile := TIniFile.Create(ConfigFileName);

  IniFile.WriteBool('Units','English',EnglishUnits);
  IniFile.WriteBool('Units','InchesAndFractions',InchesAndFractions);
  IniFile.WriteBool('Units','Centimeters',Centimeters);

  IniFile.WriteBool('Grid','SnapTo',SnapToGrid);
  IniFile.WriteBool('Grid','Show',ShowGrid);

  IniFile.WriteFloat('Grid','SpacingEnglish',GridSpacingEnglish);
  IniFile.WriteFloat('Grid','SpacingMetric',GridSpacingMetric);

  IniFile.WriteFloat('View','Zoom',Zoom);

  IniFile.WriteInteger('Scale','Scale',fScaleIndex);

  IniFile.Free;
end;

procedure TPreferences.Save( TextIO : TTextIO );
var
  S : String;
begin
  S := self.ClassName;          // Get our class name
  TextIO.Writeln('<'+S+'>');    // Write the start of class
  TextIO.Writeln(CurrentVersion);
  //Writeln(F,'<Preferences>');
  //Writeln(F,CurrentVersion);
  TextIO.Writeln(EnglishUnits);
  TextIO.Writeln(InchesAndFractions);
  TextIO.Writeln(Centimeters);
  TextIO.Writeln(SnapToGrid);
  TextIO.Writeln(ShowGrid);
  TextIO.Writeln(GridSpacingEnglish);
  TextIO.Writeln(GridSpacingMetric);
  TextIO.Writeln(fZoomIndex);
  TextIO.Writeln(fScaleIndex);
  TextIO.Writeln('</'+S+'>');    // Write the end of class
  fModified := false;
end;

procedure TPreferences.SetCentimeters(const AValue: Boolean);
begin
  Update( fCentimeters, AValue );
end;

procedure TPreferences.SetEnglishUnits(const AValue: Boolean);
begin
  Update( fEnglishUnits, AValue );
end;

procedure TPreferences.SetGridSpacingEnglish(const AValue: Double);
begin
  Update( fGridSpacingEnglish, AValue );
end;

procedure TPreferences.SetGridSpacingMetric(const AValue: Double);
begin
  Update( fGridSpacingMetric, AValue );
end;

procedure TPreferences.SetInchesAndFractions(const AValue: Boolean);
begin
  Update( fInchesAndFractions, AValue );
end;

procedure TPreferences.SetScaleIndex(const AValue: Integer);
begin
  if fScaleIndex=AValue then exit;
  fScaleIndex:=AValue;
end;

procedure TPreferences.SetShowGrid(const AValue: Boolean);
begin
  fShowGrid := AValue ;
end;

procedure TPreferences.SetSnapToGrid(const AValue: Boolean);
begin
  fSnapToGrid := AValue;
end;

procedure TPreferences.SetZoomIndex(const AValue: Integer);
begin
  fZoomIndex := AValue;
end;

end.

