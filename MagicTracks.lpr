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

program MagicTracks;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, printer4lazarus, Main1, Drawing1, About1, Preferences1,
  PreferencesForm1, UnitConversion1, Ruler1, DrawingFrame1, DrawingSetFrame1,
  ThreePoint1, LayerForm1, Layers1, Internals1, DrawingObject1, DrawingCommon1,
  RectangularSolid1, Sphere1, Common1, Persistent1, StraightLine1, Breshenham1,
  CanvasStack1;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TAboutForm1, AboutForm1);
  Application.CreateForm(TPreferencesForm, PreferencesForm);
  Application.CreateForm(TLayerForm, LayerForm);
  Application.CreateForm(TInternalsForm1, InternalsForm1);
  Application.Run;
end.

