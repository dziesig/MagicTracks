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

unit PreferencesForm1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, ExtCtrls, Preferences1;

type

  { TPreferencesForm }

  TPreferencesForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    ZoomCB: TComboBox;
    EngSignCB: TComboBox;
    ScaleCB: TComboBox;
    ZoomGB: TGroupBox;
    MetricSignCB: TComboBox;
    DivisorCB: TComboBox;
    FractLabel: TLabel;
    ShowGuidesCB: TCheckBox;
    SnapToGridCB: TCheckBox;
    GroupBox1: TGroupBox;
    FeetEdit: TLabeledEdit;
    InchesEdit: TLabeledEdit;
    FractionEdit: TLabeledEdit;
    CentimeterEdit: TLabeledEdit;
    UnitsRG: TRadioGroup;
    EnglishRG: TRadioGroup;
    MetricRG: TRadioGroup;
    ZoomGB1: TGroupBox;
    procedure BitBtn1Click(Sender: TObject);
    procedure EnglishRGClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure UnitsRGClick(Sender: TObject);
    procedure ZoomCBChange(Sender: TObject);
    procedure ZoomGB1Click(Sender: TObject);
  private
    { private declarations }
    Populating : Boolean;
    procedure Populate;
    procedure Extract;
  public
    { public declarations }
    Preferences : TPreferences;
  end; 

var
  PreferencesForm: TPreferencesForm;

implementation

{$R *.lfm}

uses
  UnitConversion1;

{ TPreferencesForm }

procedure TPreferencesForm.BitBtn1Click(Sender: TObject);
begin
  Extract;
end;

procedure TPreferencesForm.EnglishRGClick(Sender: TObject);
begin
  Extract;
  Populate;
end;

procedure TPreferencesForm.Extract;
begin
  if Populating then exit;
  Preferences.EnglishUnits := UnitsRG.ItemIndex = 0;
  Preferences.InchesAndFractions := EnglishRG.ItemIndex = 1;
  Preferences.Centimeters := MetricRG.ItemIndex = 1;
  Preferences.ShowGrid := ShowGuidesCB.Checked;
  Preferences.SnapToGrid := SnapToGridCB.Checked;
  Preferences.GridSpacingEnglish := FromUser( EngSignCB, FeetEdit, InchesEdit,
                                              FractionEdit, DivisorCB,
                                              Preferences );
  Preferences.GridSpacingMetric  := FromUser( MetricSignCB, CentimeterEdit,
                                              Preferences );
  Preferences.ZoomIndex := ZoomCB.ItemIndex;
  Preferences.ScaleIndex := ScaleCB.ItemIndex;
end;

procedure TPreferencesForm.FormCreate(Sender: TObject);
var
  I : Integer;
begin
  Preferences := TPreferences.Create(nil);
  Populating := False;
  for I := 0 to MaxZoom do
    ZoomCB.Items.Add(IntToStr(Round(Zooms[I])));
  for I := 0 to pred(MaxScales) do
    ScaleCB.Items.Add(ScaleNames[I]);
end;

procedure TPreferencesForm.FormShow(Sender: TObject);
var
  I : Integer;
begin
  ScaleCB.Items.Clear;
  for I := 0 to pred(MaxScales) do
    ScaleCB.Items.Add(ScaleNames[I]);
  Populate;
end;

procedure TPreferencesForm.Populate;
begin
  Populating := True;
  if Preferences.EnglishUnits then
    UnitsRG.ItemIndex := 0
  else
    UnitsRG.ItemIndex := 1;
  EnglishRG.Enabled := Preferences.EnglishUnits;
  MetricRG.Enabled := not Preferences.EnglishUnits;

  if Preferences.InchesAndFractions then
    EnglishRG.ItemIndex := 1
  else
    EnglishRG.ItemIndex := 0;

  if Preferences.Centimeters then
    begin
      MetricRG.ItemIndex := 1;
      CentimeterEdit.EditLabel.Caption := 'Centimeters';
    end
  else
    begin
      MetricRG.ItemIndex := 0;
      CentimeterEdit.EditLabel.Caption := 'Meters';
    end;

  ShowGuidesCB.Checked := Preferences.ShowGrid;
  SnapToGridCB.Checked := Preferences.SnapToGrid;

  InchesEdit.Visible := Preferences.InchesAndFractions;
  FractionEdit.Visible := Preferences.InchesAndFractions;
  FractLabel.Visible := Preferences.InchesAndFractions;
  DivisorCB.Visible := Preferences.InchesAndFractions;

  ToUser( EngSignCB, FeetEdit, InchesEdit, FractionEdit, DivisorCB,
          Preferences.GridSpacingEnglish, Preferences );
  ToUser( MetricSignCB, CentimeterEdit, Preferences.GridSpacingMetric, Preferences );

  ZoomCB.ItemIndex := Preferences.ZoomIndex;
  ScaleCB.ItemIndex := Preferences.ScaleIndex;
  Populating := False;
end;

procedure TPreferencesForm.UnitsRGClick(Sender: TObject);
begin
  Extract;
  Populate;
end;

procedure TPreferencesForm.ZoomCBChange(Sender: TObject);
begin
   ;
end;

procedure TPreferencesForm.ZoomGB1Click(Sender: TObject);
begin

end;

end.

