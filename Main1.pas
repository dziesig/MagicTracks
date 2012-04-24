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

unit Main1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ActnList, ComCtrls, ExtCtrls, StdCtrls, Printers, types, Drawing1,
  Preferences1, DrawingSetFrame1, Layers1;

type

  { TMainForm }

  TMainForm = class(TForm)
    GuidesToLine: TAction;
    GuidesToRectangularSolid: TAction;
    GuidesToSphere: TAction;
    FileSaveAll: TAction;
    HelpShowInternals: TAction;
    IdleTimer1: TIdleTimer;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    GuidesStraightLine: TMenuItem;
    MenuItem38: TMenuItem;
    VisibleLayerPanel: TPanel;
    ActiveLayerPanel: TPanel;
    ToolButton19: TToolButton;
    ToolsLayerDefaults: TAction;
    LayersEdit: TAction;
    DrawingSetFrame1: TDrawingSetFrame;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    PositionLowerLeft: TAction;
    MenuItem23: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    ToolButton14: TToolButton;
    ToolsShowGrid: TAction;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MIZoom1: TMenuItem;
    MIZoom2: TMenuItem;
    MIzoom3: TMenuItem;
    MIZoom4: TMenuItem;
    MIZoom5: TMenuItem;
    MIZoom6: TMenuItem;
    MIZoom7: TMenuItem;
    MIZoom8: TMenuItem;
    MIZoom9: TMenuItem;
    MIZoom10: TMenuItem;
    MIZoom12: TMenuItem;
    MIZoom15: TMenuItem;
    MIZoom20: TMenuItem;
    MIZoom25: TMenuItem;
    MIZoom30: TMenuItem;
    MIZoom40: TMenuItem;
    MIZoom50: TMenuItem;
    MIZoom60: TMenuItem;
    MIZoom70: TMenuItem;
    MIZoom80: TMenuItem;
    StatusBar2: TStatusBar;
    ZoomOut1: TMenuItem;
    ZoomIn1: TMenuItem;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ViewZoomOut: TAction;
    ViewZoomIn: TAction;
    MenuItem24: TMenuItem;
    ToolsSnapToGrid: TAction;
    ToolButton13: TToolButton;
    ToolButton15: TToolButton;
    ToolsPreferencesCurrentDrawing: TAction;
    ToolsPreferencesNewDrawing: TAction;
    HelpAbout: TAction;
    FileClose: TAction;
    FilePrint: TAction;
    FilePrintSetup: TAction;
    EditPaste: TAction;
    EditCut: TAction;
    EditCopy: TAction;
    EditUndo: TAction;
    FileExit: TAction;
    FileSaveAs: TAction;
    FileSave: TAction;
    FileOpen: TAction;
    FileNew: TAction;
    ActionList1: TActionList;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;
    TabControl1: TTabControl;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure FileCloseExecute(Sender: TObject);
    procedure FileExitExecute(Sender: TObject);
    procedure FileNewExecute(Sender: TObject);
    procedure FileOpenExecute(Sender: TObject);
    procedure FilePrintExecute(Sender: TObject);
    procedure FilePrintSetupExecute(Sender: TObject);
    procedure FileSaveAllExecute(Sender: TObject);
    procedure FileSaveAsExecute(Sender: TObject);
    procedure FileSaveExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure GuidesToLineExecute(Sender: TObject);
    procedure GuidesToRectangularSolidExecute(Sender: TObject);
    procedure GuidesToSphereExecute(Sender: TObject);
    procedure HelpAboutExecute(Sender: TObject);
    procedure HelpShowInternalsExecute(Sender: TObject);
    procedure IdleTimer1Timer(Sender: TObject);
    procedure LayersEditExecute(Sender: TObject);
    procedure MIZoom1Click(Sender: TObject);
    procedure PositionLowerLeftExecute(Sender: TObject);
    procedure ToolsLayerDefaultsExecute(Sender: TObject);
    procedure ToolsObjectsSnapToGuidesExecute(Sender: TObject);
    procedure ToolsShowGridExecute(Sender: TObject);
    procedure ViewZoomInExecute(Sender: TObject);
    procedure ViewZoomOutExecute(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure ToolsPreferencesCurrentDrawingExecute(Sender: TObject);
    procedure ToolsPreferencesNewDrawingExecute(Sender: TObject);
    procedure ToolsSnapToGridExecute(Sender: TObject);
  private
    { private declarations }
    NewCount : Integer;
    {
      Note:  Drawings as TList is a work-around for the incomplete
      implementation of Object access in the Lazarus TTabControl
    }
    Drawings : TList;
    fActiveDrawing : TDrawing;
    fConfigFileName : String;

    fDefaultPreferences : TPreferences;
    fDefaultLayers : TLayers;

    MIZooms : array[0..MaxZoom] of TMenuItem;

    procedure SetActiveDrawing( Value : TDrawing );

  public
    { public declarations }

    property ConfigFileName : String read fConfigFileName;
    property ActiveDrawing : TDrawing read fActiveDrawing write SetActiveDrawing;
    property DefaultPreferences : TPreferences read fDefaultPreferences;
    property DefaultLayers : TLayers read fDefaultLayers;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  About1, PreferencesForm1, LayerForm1, Internals1, Sphere1, DrawingObject1,
  RectangularSolid1, ThreePoint1, StraightLine1;

{ TMainForm }


procedure TMainForm.FileCloseExecute(Sender: TObject);
var
  Index   : Integer;
  Drawing : TDrawing;
  Ans     : Integer;
  CloseIt : Boolean;
begin
  Index := TabControl1.TabIndex;
  if Index >= 0 then
    begin
      Drawing := TDrawing(Drawings[Index]);
      CloseIt := true;
      { Check for modified Drawing, get user's permission to close if changed. }
      if Drawing.Modified then
        begin
          Ans := MessageDlg('Drawing ' +  Drawing.NameOnly + ' has been modified.'#10#13+
                            'Close without saving (all changes will be lost)?',
                            mtConfirmation,[mbNo, mbYes], 0);
          CloseIt := Ans = mrYes;
        end;
      if CloseIt then
        begin
          TDrawing(Drawings[Index]).Free;
          Drawings.Delete(Index);
          TabControl1.Tabs.Delete(Index);
          Index := TabControl1.TabIndex;
          if Index >= 0 then
            ActiveDrawing := TDrawing(Drawings[Index])
          else
            ActiveDrawing := nil;
        end;
    end;
end;

procedure TMainForm.FileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FileNewExecute(Sender: TObject);
var
  Drawing : TDrawing;
  Index : Integer;
  P : TPreferences;
begin
  Inc(NewCount);
  Drawing := TDrawing.Create;
  Drawing.SetFullPath('UNTITLED' + '-'+IntToStr(NewCount));
  P := Drawing.Preferences;
  P.Assign(fDefaultPreferences);
  Drawing.Preferences.Assign(fDefaultPreferences);
  { DONE 1 -oDon Ziesig -cDrawing : Add Layers to from Default to new Drawing }
  Drawing.Layers.Assign(fDefaultLayers);
  Index := TabControl1.Tabs.Add(Drawing.Name);
  Drawings.Add(TObject(Drawing));
  TabControl1.TabIndex := Index;
  ActiveDrawing := Drawing; // Needed for first drawing because tab NOT changed.
  Drawing.Preferences.UNMODIFY;
end;

procedure TMainForm.FileOpenExecute(Sender: TObject);
var
  Drawing : TDrawing;
  Index   : Integer;
  I, C    : Integer;
begin
  if OpenDialog1.Execute then
    begin
      C := 0;
      for I := 0 to pred(Drawings.Count) do
        if TDrawing(Drawings[I]).FullPath = OpenDialog1.FileName then
          Inc(C);
      if C > 0 then
        MessageDlg(OpenDialog1.FileName + #13#10 +
                   'is already open in one of the tabs.' + #13#10 +
                   'This one will be opened "READ ONLY"',mtWarning,[mbOK],0);
      Drawing := TDrawing.Create;
      Drawing.SetFullPath(OpenDialog1.FileName);
      Drawing.GetFromFile( C > 0 );
      if C > 0 then
        Index := TabControl1.Tabs.Add(Drawing.NameOnly + '(RO)')
      else
        Index := TabControl1.Tabs.Add(Drawing.NameOnly);
      Drawings.Add(TObject(Drawing));
      ActiveDrawing := Drawing;
      TabControl1.TabIndex := Index;
    end;
end;

procedure TMainForm.FilePrintExecute(Sender: TObject);
begin
  //if PrintDialog1.Execute then
  //  begin
  //
  //  end;
end;

procedure TMainForm.FilePrintSetupExecute(Sender: TObject);
begin
  //if PrinterSetupDialog1.Execute then
  //  begin
  //    ;
  //  end;
end;

procedure TMainForm.FileSaveAllExecute(Sender: TObject);
var
  I : Integer;
  S : String;
begin
  for I := 0 to pred(Drawings.Count) do
    if TDrawing(Drawings[I]).Modified then
      if TDrawing(Drawings[I]).CanLoadOrSave then
        TDrawing(Drawings[I]).PutToFile
      else
        try
          S := SaveDialog1.Title;
          SaveDialog1.Title := TabControl1.Tabs[I];
          if SaveDialog1.Execute then
            begin
              TDrawing(Drawings[I]).SetFullPath(SaveDialog1.FileName);
              TDrawing(Drawings[I]).PutToFile;
              TabControl1.Tabs[I] := TDrawing(Drawings[I]).NameOnly;
            end;
        finally
          SaveDialog1.Title := S;
        end;

end;

procedure TMainForm.FileSaveAsExecute(Sender: TObject);
var
  Index : Integer;
begin
  if SaveDialog1.Execute then
    begin
      fActiveDrawing.SetFullPath(SaveDialog1.FileName);
      fActiveDrawing.PutToFile;
      ActiveDrawing := fActiveDrawing;
      Index := TabControl1.TabIndex;
      TabControl1.Tabs[Index] := ActiveDrawing.NameOnly;
    end;
end;

procedure TMainForm.FileSaveExecute(Sender: TObject);
begin
  { DONE 3 -oMe -cControls : Add menu item and button for "Save All" }
  if ActiveDrawing.CanLoadOrSave then
    ActiveDrawing.PutToFile
  else
    FileSaveAsExecute(Sender);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  I : Integer;
  C : Integer;
begin
  C := 0;
  { Check all drawings for modification, get user's permission to exit if any changed. }
  for I := 0 to pred(Drawings.Count) do
    if TDrawing(Drawings[I]).Modified then
      Inc(C);
  if C > 0 then
    CanClose := MessageDlg(IntToStr(C) + ' Drawing(s) have been modified.'#10#13 +
                           'Continue exiting Magic Tracks (all changes will be lost)?' ,
                           mtConfirmation,[mbYes, mbNo],0) = mrYes;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  fConfigFileName := GetAppConfigFile(False);
  { Create the configuration file directory if it doesn't exist }
  ForceDirectory(ExtractFilePath(fConfigFileName));
  fDefaultPreferences := TPreferences.Create;
  fDefaultPreferences.GetFromConfig( fConfigFileName );
  fDefaultLayers := TLayers.Create;
  fDefaultLayers.GetFromConfig( fConfigFileName );
  NewCount := 0;
  Drawings := TList.Create;

  { Setup the Zoom-index MenuItems }

  MIZooms[ 0] := MIZoom1;
  MIZooms[ 1] := MIZoom2;
  MIZooms[ 2] := MIZoom3;
  MIZooms[ 3] := MIZoom4;
  MIZooms[ 4] := MIZoom5;
  MIZooms[ 5] := MIZoom6;
  MIZooms[ 6] := MIZoom7;
  MIZooms[ 7] := MIZoom8;
  MIZooms[ 8] := MIZoom9;
  MIZooms[ 9] := MIZoom10;
  MIZooms[10] := MIZoom12;
  MIZooms[11] := MIZoom15;
  MIZooms[12] := MIZoom20;
  MIZooms[13] := MIZoom25;
  MIZooms[14] := MIZoom30;
  MIZooms[15] := MIZoom40;
  MIZooms[16] := MIZoom50;
  MIZooms[17] := MIZoom60;
  MIZooms[18] := MIZoom70;
  MIZooms[19] := MIZoom80;

{ NO ActiveDrawing to start }

  ActiveDrawing := nil;

end;

procedure TMainForm.FormResize(Sender: TObject);
begin
end;

procedure TMainForm.GuidesToLineExecute(Sender: TObject);
var
  S : TStraightLine;
  O : TDrawingObjects;
  D : TDrawing;
  L : TLayer;
begin
  S := TStraightLine.Create( ActiveDrawing.ActiveLayer );
  S.X := ActiveDrawing.Guide2X;
  S.Y := ActiveDrawing.Guide2Y;
  S.Z := ActiveDrawing.Guide2Z;
  S.SetLineEnd( ActiveDrawing.Guide1X - ActiveDrawing.Guide2X,
                ActiveDrawing.Guide1Y - ActiveDrawing.Guide2Y,
                ActiveDrawing.Guide1Z - ActiveDrawing.Guide2Z );
  D := ActiveDrawing;
  L := D.ActiveLayer;
  O := L.DrawingObjects;
  O.Add( S );
  DrawingSetFrame1.Invalidate;
end;

procedure TMainForm.GuidesToRectangularSolidExecute(Sender: TObject);
var
  S : TRectangularSolid;
  O : TDrawingObjects;
  D : TDrawing;
  L : TLayer;
begin
  S := TRectangularSolid.Create( ActiveDrawing.ActiveLayer );
  S.X := ActiveDrawing.Guide2X;
  S.Y := ActiveDrawing.Guide2Y;
  S.Z := ActiveDrawing.Guide2Z;
  S.Length := ActiveDrawing.Guide1X - ActiveDrawing.Guide2X;
  S.Width  := ActiveDrawing.Guide1Y - ActiveDrawing.Guide2Y;
  S.Height := ActiveDrawing.Guide1Z - ActiveDrawing.Guide2Z;
  D := ActiveDrawing;
  L := D.ActiveLayer;
  O := L.DrawingObjects;
  O.Add( S );
  DrawingSetFrame1.Invalidate;
end;

procedure TMainForm.GuidesToSphereExecute(Sender: TObject);
var
  S : TSphere;
  DX, DY, DZ : Double;
  O : TDrawingObjects;
  D : TDrawing;
  L : TLayer;
  R : T3Point;
begin
  S := TSphere.Create(ActiveDrawing.ActiveLayer);
  DX := ActiveDrawing.Guide2X - ActiveDrawing.Guide1X;
  S.X := (ActiveDrawing.Guide1X + ActiveDrawing.Guide2X) / 2;
  DY := ActiveDrawing.Guide2Y - ActiveDrawing.Guide1Y;
  S.Y := (ActiveDrawing.Guide1Y + ActiveDrawing.Guide2Y) / 2;
  DZ := ActiveDrawing.Guide2Z - ActiveDrawing.Guide1Z;
  S.Z := (ActiveDrawing.Guide1Z + ActiveDrawing.Guide2Z) / 2;
  R := T3Point.Create( DX, DY, DZ );
  S.Radii := R;
  R.Free;
  D := ActiveDrawing;
  L := D.ActiveLayer;
  O := L.DrawingObjects;
  O.Add( S );
  DrawingSetFrame1.Invalidate;
end;


procedure TMainForm.HelpAboutExecute(Sender: TObject);
begin
  AboutForm1.ShowModal;//Modal;
end;

procedure TMainForm.HelpShowInternalsExecute(Sender: TObject);
begin
  InternalsForm1.Show;
end;

procedure TMainForm.IdleTimer1Timer(Sender: TObject);
var
  I : Integer;
  Count : Integer;
begin
  Count := 0;
  for I := 0 to pred(Drawings.Count) do
    if TDrawing(Drawings[I]).Modified then
      Inc(Count);
  FileSaveAll.Enabled := Count > 0;
end;

procedure TMainForm.LayersEditExecute(Sender: TObject);
begin
  LayerForm.Assign( ActiveDrawing.Layers);
  if LayerForm.ShowModal = mrOk then
    begin
      ActiveDrawing.Layers.Assign(LayerForm.Layers);
      ActiveDrawing.ActiveLayer := LayerForm.ActiveLayer;
    end;
end;

procedure TMainForm.MIZoom1Click(Sender: TObject);
begin
  ActiveDrawing.SetZoom((Sender as TMenuItem).Tag);
  ActiveDrawing := ActiveDrawing;
end;

procedure TMainForm.PositionLowerLeftExecute(Sender: TObject);
begin
  if ActiveDrawing <> nil then
    begin
      DrawingSetFrame1.PositionLowerLeft;
    end;

end;


procedure TMainForm.SetActiveDrawing(Value: TDrawing);
var
  IsActive : Boolean;
  I        : Integer;
  V        : TLayerKinds;
begin
  fActiveDrawing := Value;
  IsActive := Value <> nil;
  if IsActive then
    begin
      StatusBar1.Panels[1].Text := Value.FullPath;
      ToolsShowGrid.Checked := fActiveDrawing.Preferences.ShowGrid;
      ToolsSnapToGrid.Checked := fActiveDrawing.Preferences.SnapToGrid;
      VisibleLayerPanel.Caption := '';
      for V in TLayerKinds do
        if V in ActiveDrawing.Layers.ViewLayers then
          VisibleLayerPanel.Caption := VisibleLayerPanel.Caption + LayerNames[V] + ' ';
      ActiveLayerPanel.Caption := LayerNames[ActiveDrawing.Layers.ActiveLayerKind];
    end
  else
    begin
      StatusBar1.Panels[1].Text := 'NIL';
      VisibleLayerPanel.Caption := '';
      ActiveLayerPanel.Caption := '';
    end;
  FileSave.Enabled := IsActive;
  FileSaveAs.Enabled := IsActive;
  FileClose.Enabled := IsActive;
  FilePrint.Enabled := IsActive;
  ToolsPreferencesCurrentDrawing.Enabled := IsActive;
  ToolsShowGrid.Enabled := IsActive;
  ToolsSnapToGrid.Enabled := IsActive;
  ViewZoomIn.Enabled := IsActive;
  ViewZoomOut.Enabled := IsActive;
  LayersEdit.Enabled := IsActive;

  for I := 0 to MaxZoom do
    begin
      MiZooms[I].Enabled := IsActive;
      MiZooms[I].Checked := False;
    end;
  if IsActive then
    begin
      MIZooms[ActiveDrawing.ZoomIndex].Checked := true;

    end;

  DrawingSetFrame1.Drawing := ActiveDrawing;

end;


procedure TMainForm.TabControl1Change(Sender: TObject);
var
  Index : Integer;
begin
  Index := TabControl1.TabIndex;
  if Index >= 0 then
    ActiveDrawing := TDrawing(Drawings[Index])
  else
    ActiveDrawing := nil;
end;

procedure TMainForm.ToolsLayerDefaultsExecute(Sender: TObject);
begin
  LayerForm.Assign( fDefaultLayers );
  if LayerForm.ShowModal = mrOk then
    begin
      LayerForm.Layers.PutToConfig( fConfigFilename );
      fDefaultLayers.Assign( LayerForm.Layers );
    end;
end;

procedure TMainForm.ToolsObjectsSnapToGuidesExecute(Sender: TObject);
begin
  { TODO 1 -oDon Ziesig -cUser Interface : Implement Objects Snap to Guides }
end;

procedure TMainForm.ToolsPreferencesCurrentDrawingExecute(Sender: TObject);
begin
  PreferencesForm.Preferences.Assign( ActiveDrawing.Preferences );
  if PreferencesForm.ShowModal = mrOk then
    ActiveDrawing.Preferences.Assign( PreferencesForm.Preferences );
end;

procedure TMainForm.ToolsPreferencesNewDrawingExecute(Sender: TObject);
var
  Ans : Integer;
begin
  PreferencesForm.Preferences.Assign(fDefaultPreferences);
  Ans := PreferencesForm.ShowModal;
  if Ans = mrOk then
    begin
      PreferencesForm.Preferences.PutToConfig(fConfigFileName);
      fDefaultPreferences.Assign(PreferencesForm.Preferences);
    end;
end;


procedure TMainForm.ToolsShowGridExecute(Sender: TObject);
begin
  if ActiveDrawing = nil then exit;
  ActiveDrawing.Preferences.ShowGrid := ToolsShowGrid.Checked;
  ActiveDrawing := ActiveDrawing;
end;

procedure TMainForm.ToolsSnapToGridExecute(Sender: TObject);
begin
  ActiveDrawing.Preferences.SnapToGrid := ToolsSnapToGrid.Checked;
end;

procedure TMainForm.ViewZoomInExecute(Sender: TObject);
begin
  if ActiveDrawing = nil then exit;
  ActiveDrawing.DecZoom;
  ActiveDrawing := ActiveDrawing;
end;

procedure TMainForm.ViewZoomOutExecute(Sender: TObject);
begin
  if ActiveDrawing = nil then exit;
  ActiveDrawing.IncZoom;
  ActiveDrawing := ActiveDrawing;
end;

end.

