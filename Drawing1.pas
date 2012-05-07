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

unit Drawing1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  Persists1, Preferences1, ExtCtrls, DrawingCommon1, TextIO1,
  Layers1, Graphics, Forms, ThreePoint1;

type
  TDrawingMins = array [TDrawingBox] of Double;

  { TDrawing }

  TDrawing = class(TPersists)
    private
      fActiveLayer: TLayer;
      fPath : String;
      fPreferences: TPreferences;
      fReadOnly : Boolean;

      fGuide1X, fGuide1Y, fGuide1Z : Double;
      fGuide2X, fGuide2Y, fGuide2Z : Double;

      fLayers : TLayers;

{
  The Lower Left (on Screen) coordinates for the three windows.  Coordinates
  are in MICRONS.
}
      fMinX : TDrawingMins;
      fMinY : TDrawingMins;
      fMinZ : TDrawingMins;

      function GetActiveLayer: TLayer;
      function GetMinX(Index : TDrawingBox): Double;
      function GetMinY(Index : TDrawingBox): Double;

      function GetZoom: Double;
      function GetZoomIndex: Integer;
      procedure SetActiveLayer(const AValue: TLayer);
      procedure SetGuide1X(const AValue: Double);
      procedure SetGuide1Y(const AValue: Double);
      procedure SetGuide1Z(const AValue: Double);
      procedure SetGuide2X(const AValue: Double);
      procedure SetGuide2Y(const AValue: Double);
      procedure SetGuide2Z(const AValue: Double);

      procedure SetMinX(Index : TDrawingBox; const AValue: Double);
      procedure SetMinY(Index : TDrawingBox; const AValue: Double);

      procedure SetPreferences(const AValue: TPreferences);

      function GetXMax: Double;
      function GetXMin: Double;
      function GetYMax: Double;
      function GetYMin: Double;
      function GetZMax: Double;
      function GetZMin: Double;

    protected

      procedure ExtractFileAndPath( Value : String );
      function GetFullPath: string;
      function GetNameOnly: string;

      function IsModified : Boolean; override;

    public
      constructor Create( AParent : TPersists = nil ); override;
      destructor  Destroy; override;

      procedure MakeNew; override;

      procedure SetFullPath( Value : String );

      procedure Save( TextIO : TTextIO ); override;
      procedure Load( TextIO : TTextIO ); override;

      procedure PutToFile;
      procedure GetFromFile( IsReadOnly : Boolean = false);

      function  CanLoadOrSave : Boolean;

      procedure IncZoom;
      procedure DecZoom;
      procedure SetZoom( Index : Integer );

      procedure Draw( Frame : TFrame );

      procedure MoveSelected( Position : T3Point );
      function  SelectedCount : Integer;

      procedure Update( var Data : TLayer; NewValue : TLayer );  overload;

      property Path : String read fPath;
      property FullPath : string read GetFullPath;
      property NameOnly : string read GetNameOnly;
      property ReadOnly : boolean read fReadOnly;

      property Preferences : TPreferences read fPreferences write SetPreferences;

{ MinX... determines the origin of the specified drawing box's visible area }
{ DONE 1 -oDon Ziesig -cDrawing to GUI : Change this to map X Y Z by box type into X and Y only. }
      property MinX[Index : TDrawingBox] : Double read GetMinX write SetMinX;
      property MinY[Index : TDrawingBox] : Double read GetMinY write SetMinY;

{ XMin... and XMax... return the extent of the drawing itself (in microns) }

      property XMin : Double read GetXMin;
      property YMin : Double read GetYMin;
      property ZMin : Double read GetZMin;

      property XMax : Double read GetXMax;
      property YMax : Double read GetYMax;
      property ZMax : Double read GetZMax;

{ Zoom is the scale factof from the display to actual units }
{ ZoomIndex is selects the scale factor from the array of Zooms }

      property Zoom : Double read GetZoom;

      property ZoomIndex : Integer read GetZoomIndex;

{ Guides provide visual alignment tools with the ability to
    1: Snap the guide itself to the grid (not shown on display), and
    2: Snap a drawing object to the guide (whether or not the guide is
       snapped to the grid.

  Note that two of each guide is drawn (e.g., the X guides for XY and XZ )
  and both track the other.
}
      property Guide1X : Double read fGuide1X write SetGuide1X;
      property Guide1Y : Double read fGuide1Y write SetGuide1Y;
      property Guide1Z : Double read fGuide1Z write SetGuide1Z;

      property Guide2X : Double read fGuide2X write SetGuide2X;
      property Guide2Y : Double read fGuide2Y write SetGuide2Y;
      property Guide2Z : Double read fGuide2Z write SetGuide2Z;

      property Layers : TLayers read fLayers;
      property ActiveLayer : TLayer read GetActiveLayer write SetActiveLayer;


  end;

implementation

uses
  UnitConversion1, Internals1, DrawingObject1;

{ TDrawing }

const
  CurrentVersion = 5;

function TDrawing.CanLoadOrSave: Boolean;
begin
  Result := (fPath <> '') and (fName <> '') and (Pos('UNTITLED',fName) = 0);
end;

constructor TDrawing.Create( AParent : TPersists );
begin
  inherited;
  fPreferences := TPreferences.Create;
  fLayers      := TLayers.Create;
end;

procedure TDrawing.DecZoom;
begin
  if Preferences.ZoomIndex > 0 then
    Preferences.ZoomIndex := Preferences.ZoomIndex-1;
end;

destructor TDrawing.Destroy;
begin
  inherited;
end;

procedure TDrawing.ExtractFileAndPath( Value : String);
begin
  fPath := ExtractFilePath( Value );
  fName := ExtractFileName( Value );
end;

function TDrawing.GetActiveLayer: TLayer;
begin
  Result := TLayer(Layers.Items[ord(Layers.ActiveLayerKind)]);
end;

procedure TDrawing.GetFromFile( IsReadOnly : Boolean);
var
//  F : TextFile;
  TextIo : TTextIO;
begin
  fReadOnly := IsReadOnly;
  TextIO := TTextIO.Create( GetFullPath, false );
  Load( TextIO );
  TextIO.Destroy;
  //AssignFile(F, GetFullPath);
  //Reset(F);
  //Load(F);
  //Closefile(F);
end;

function TDrawing.GetFullPath: string;
begin
  Result := fPath + fName;
end;

function TDrawing.GetMinX(Index : TDrawingBox): Double;
begin
  case Index of
    XY, XZ: Result := fMinX[Index];
    YZ:     Result := fMinY[Index];
  end;
end;

function TDrawing.GetMinY(Index : TDrawingBox): Double;
begin
  case Index of
    XY:     Result := fMinY[Index];
    XZ, YZ: Result := fMinZ[Index];
  end;
end;

function TDrawing.GetNameOnly: string;
begin
  Result := ChangeFileExt(fName,'');
end;

function TDrawing.GetXMax: Double;
begin
  Result := 30.0 * FtToMicrons; { TODO 1 -oMe -cDevelopment : Update to return actual value when drawing is fully implemented. }
end;

function TDrawing.GetXMin: Double;
begin
  Result := -10.0 * FtToMicrons; { TODO 1 -oMe -cDevelopment : Update to return actual value when drawing is fully implemented. }
end;

function TDrawing.GetYMax: Double;
begin
  Result := 25.0 * FtToMicrons; { TODO 1 -oMe -cDevelopment : Update to return actual value when drawing is fully implemented. }
end;

function TDrawing.GetYMin: Double;
begin
  Result := 0.0; { TODO 1 -oMe -cDevelopment : Update to return actual value when drawing is fully implemented. }
end;

function TDrawing.GetZMax: Double;
begin
  Result := 0.0; { TODO 1 -oMe -cDevelopment : Update to return actual value when drawing is fully implemented. }
end;

function TDrawing.GetZMin: Double;
begin
  Result := 0.0; { TODO 1 -oMe -cDevelopment : Update to return actual value when drawing is fully implemented. }
end;

function TDrawing.GetZoom: Double;
begin
  Result := Preferences.Zoom;
end;

function TDrawing.GetZoomIndex: Integer;
begin
  Result := Preferences.ZoomIndex;
end;

procedure TDrawing.SetGuide1X(const AValue: Double);
begin
  Update( fGuide1X, AValue );
  InternalsForm1.PutGuides( fGuide1X, fGuide1Y, fGuide1Z, fGuide2X, fGuide2Y,fGuide2Z );
end;

procedure TDrawing.SetGuide1Y(const AValue: Double);
begin
  Update( fGuide1Y, AValue );
  InternalsForm1.PutGuides( fGuide1X, fGuide1Y, fGuide1Z, fGuide2X, fGuide2Y,fGuide2Z );
end;

procedure TDrawing.SetGuide1Z(const AValue: Double);
begin
  Update( fGuide1Z, AValue );
  InternalsForm1.PutGuides( fGuide1X, fGuide1Y, fGuide1Z, fGuide2X, fGuide2Y,fGuide2Z );
end;

procedure TDrawing.SetGuide2X(const AValue: Double);
begin
  Update( fGuide2X, AValue );
  InternalsForm1.PutGuides( fGuide1X, fGuide1Y, fGuide1Z, fGuide2X, fGuide2Y,fGuide2Z );
end;

procedure TDrawing.SetGuide2Y(const AValue: Double);
begin
  Update( fGuide2Y, AValue );
  InternalsForm1.PutGuides( fGuide1X, fGuide1Y, fGuide1Z, fGuide2X, fGuide2Y,fGuide2Z );
end;

procedure TDrawing.SetGuide2Z(const AValue: Double);
begin
  Update( fGuide2Z, AValue );
  InternalsForm1.PutGuides( fGuide1X, fGuide1Y, fGuide1Z, fGuide2X, fGuide2Y,fGuide2Z );
end;

procedure TDrawing.IncZoom;
begin
  if Preferences.ZoomIndex < MaxZoom then
    Preferences.ZoomIndex := Preferences.ZoomIndex+1;
end;

function TDrawing.IsModified: Boolean;
begin
  { This causes simple operations such as scrolling a drawing to cause the }
  { drawing to be considered MODIFIED.  If that behavior is not wanted,    }
  { use:     Result:=inherited IsModified;   instead.                      }
  Result:=inherited IsModified or fPreferences.Modified;
end;

procedure TDrawing.Load( TextIO : TTextIO );
var
  V : Integer;
  ClsName : String;
  S       : String;
begin
  MakeNew;
  ClsName := self.ClassName;    // Get the expected class name
  TextIO.ReadLn(S);             // Read the start of class
  CheckStartClass(S,ClsName);   // Assert they are correct and of correct format
  TextIO.Readln(V);
// Read based on version;
  if V >= 2 then
    Preferences.Load(TextIO);
  if V >= 3 then
    begin
      TextIO.Readln(fMinX[XY]);
      TextIO.Readln(fMinY[XY]);
      TextIO.Readln(fMinX[XZ]);
      TextIO.Readln(fMinZ[XZ]);
      TextIO.Readln(fMinY[YZ]);
      TextIO.Readln(fMinZ[XZ]);
    end;
  if V >= 4 then
    begin
      Layers.Load(TextIO);
    end;
  if V >= 5 then
    begin
      TextIO.Readln(fGuide1X);
      TextIO.Readln(fGuide1Y);
      TextIO.Readln(fGuide1Z);
      TextIO.Readln(fGuide2X);
      TextIO.Readln(fGuide2Y);
      TextIO.Readln(fGuide2Z);
    end;

  TextIO.Readln(S);             // Read the end of class
  CheckEndClass(S,ClsName);     // Assert end of class is correct and of correct format
  fModified := false;           // make sure this was NOT modified by the load.
end;

procedure TDrawing.MakeNew;
begin
  inherited MakeNew;
  fMinX[XY] := 0;
  fMinY[XY] := 0;
  fMinX[XZ] := 0;
  fMinZ[XZ] := 0;
  fMinY[YZ] := 0;
  fMinZ[YZ] := 0;
end;

procedure TDrawing.MoveSelected(Position: T3Point);
var
  DrawingObject : TDrawingObject;
  I             : Integer;
begin
  for I := 0 to pred(ActiveLayer.DrawingObjects.Count) do
    begin
      DrawingObject := TDrawingObject(ActiveLayer.DrawingObjects[I]);
      if DrawingObject.Selected then
        DrawingObject.Move( Position );
    end;
end;

procedure TDrawing.PutToFile;
var
  TextIO : TTextIO;
begin
  TextIO := TTextIO.Create( GetFullPath, true );
  Save( TextIO );
  TextIO.Destroy;
end;

procedure TDrawing.Save( TextIO : TTextIO );
var
  S : String;
begin
  S := self.ClassName;          // Get our class name
  TextIO.Writeln('<'+S+'>');    // Write the start of class
  TextIO.Writeln(CurrentVersion);
  // Write based on version;
  // Version 2
  Preferences.Save(TextIO);
  // Version 3
  TextIO.Writeln(fMinX[XY]);
  TextIO.Writeln(fMinY[XY]);
  TextIO.Writeln(fMinX[XZ]);
  TextIO.Writeln(fMinZ[XZ]);
  TextIO.Writeln(fMinY[YZ]);
  TextIO.Writeln(fMinZ[XZ]);
  // Version 4
  Layers.Save(TextIO);
  //Version 5
  TextIO.Writeln(fGuide1X);
  TextIO.Writeln(fGuide1Y);
  TextIO.Writeln(fGuide1Z);
  TextIO.Writeln(fGuide2X);
  TextIO.Writeln(fGuide2Y);
  TextIO.Writeln(fGuide2Z);

  TextIO.Writeln('</'+S+'>');   // Write the end of class
  fModified := false;           // if it were modified, it isn't any more.
end;

function TDrawing.SelectedCount: Integer;
var
  DrawingObject : TDrawingObject;
  I             : Integer;
begin
  Result := 0;
  for I := 0 to pred(ActiveLayer.DrawingObjects.Count) do
    begin
      DrawingObject := TDrawingObject(ActiveLayer.DrawingObjects[I]);
      if DrawingObject.Selected then
        Inc(Result);
    end;
end;

procedure TDrawing.SetActiveLayer(const AValue: TLayer);
begin
  Update( fActiveLayer, AValue );
end;

procedure TDrawing.SetFullPath(Value: String);
begin
  ExtractFileAndPath( Value );
end;

procedure TDrawing.SetMinX(Index : TDrawingBox; const AValue: Double);
begin
  case Index of
    XY, XZ: fMinX[Index] := AValue;
    YZ:     fMinY[Index] := AValue;
  end;
  InternalsForm1.PutMins( fMinX, fMinY, fMinZ );
end;

procedure TDrawing.SetMinY(Index : TDrawingBox; const AValue: Double);
begin
  case Index of
    XY:     fMinY[Index] := AValue;
    XZ, YZ: fMinZ[Index] := AValue;
  end;
  InternalsForm1.PutMins( fMinX, fMinY, fMinZ );
end;

procedure TDrawing.SetPreferences(const AValue: TPreferences);
begin
  fPreferences.Assign(AValue);
end;


procedure TDrawing.SetZoom(Index: Integer);
begin
  Preferences.ZoomIndex := Index;
end;

procedure TDrawing.Update(var Data: TLayer; NewValue: TLayer);
begin
  fModified := fModified or (Data <> NewValue);
  Data := NewValue;
end;

procedure TDrawing.Draw(Frame: TFrame);
var
  I : TLayerKinds;
begin
//  InternalsForm1.PutEvent('Draw','Drawing Box:  ' + IntToStr(ord(Box)));
  for I in TLayerKinds do
    if I in Layers.ViewLayers then
      TLayer(Layers[ord(I)]).Draw( Frame, Preferences, I = Layers.ActiveLayerKind );
end;


end.

