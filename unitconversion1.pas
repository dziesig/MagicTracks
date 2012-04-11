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

unit UnitConversion1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Preferences1, ExtCtrls, StdCtrls;

{ This unit provides conversion from English/Metric units in strings to internal
  units in microns (10^-6 Meters). Conversions are based on the preferences specified. }

{ ToUser and FromUser are provided User interface only }
  procedure ToUser( EngSign : TComboBox;
                    FootEdit, InchEdit, FracEdit : TLabeledEdit;
                    FracCombo : TComboBox;
                    MetSign : TComboBox;
                    Meters  : TLabeledEdit;
                    Value : Double;
                    Preferences : TPreferences ); overload;

  procedure ToUser( EngSign : TComboBox;
                    FootEdit, InchEdit, FracEdit : TLabeledEdit;
                    FracCombo : TComboBox;
                    Value : Double;
                    Preferences : TPreferences ); overload;

  procedure ToUser( MetSign : TComboBox;
                    Meters  : TLabeledEdit;
                    Value : Double;
                    Preferences : TPreferences ); overload;


  function FromUser( EngSign : TComboBox;
                     FootEdit, InchEdit, FracEdit : TLabeledEdit;
                     FracCombo : TComboBox;
                     MetSign : TComboBox;
                     Meters  : TLabeledEdit;
                     Preferences : TPreferences ) : Double;

  function FromUser( EngSign : TComboBox;
                     FootEdit, InchEdit, FracEdit : TLabeledEdit;
                     FracCombo : TComboBox;
                     Preferences : TPreferences ) : Double;

  function FromUser( MetSign : TComboBox;
                     Meters  : TLabeledEdit;
                     Preferences : TPreferences ) : Double;

  function ToString( Value : Double; Preferences : TPreferences ) : String; overload;
  function ToString( Pixels : Integer; Preferences : TPreferences ) : String; overload;

  function MicronsToPixels( Value : Double; Preferences : TPreferences ) : Integer;
  function PixelsToMicrons( Value : Integer; Preferences : TPreferences ) : Double;

  function PixelsToValue( Value : Integer; Preferences : TPreferences ) : Double;

const
  FtToMicrons = 304800.0;
  InToMicrons = 25400.0;

  MicronsToFt = 3.2808398950131233595800524934383e-6;
  MicronstoIn = 3.937007874015748031496062992126e-5;

  MicronsToCm = 0.0001;
  MicronsToM  = 0.000001;

  CmToMicrons = 10000.0;
  MToMicrons  = 1000000.0;



implementation

uses
  Common1;

const
  Denominator : array [0..5] of Integer = (2,4,8,16,32,64);

procedure ToUser(EngSign: TComboBox; FootEdit, InchEdit, FracEdit: TLabeledEdit;
  FracCombo: TComboBox; MetSign: TComboBox; Meters: TLabeledEdit; Value: Double;
  Preferences: TPreferences);
var
  Feet : Integer;
  Foot : Double;
  Inch : Integer;
  Fr   : Double;
  Neg : Boolean;
begin
  Neg := Value < 0.0;
  if Neg then
    begin
      EngSign.ItemIndex := 1;
      MetSign.ItemIndex := 1;
      Value := - Value;
    end
  else
    begin
      EngSign.ItemIndex := 0;
      MetSign.ItemIndex := 0;
    end;

  if Preferences.Centimeters then
    Meters.Text := FloatToStr(Value * MicronsToCm)
  else
    Meters.Text := FloatToStr(Value * MicronsToM);

  if Preferences.InchesAndFractions then
    begin
      Feet := Trunc( Value * MicronsToFt);
      Inch := Trunc( Value * MicronsToIn);
      Fr := Round((Value * MicronsToIn - Inch)*64);
      Inch := Inch - (Feet * 12);
      FootEdit.Text := IntToStr(Feet);
      InchEdit.Text := IntToStr(Inch);
      FracEdit.Text := FloatToStr(Fr); // LOOK HERE  -  Temporary!!!!
    end
  else
    begin
      Foot := Value * MicronsToFt;
      FootEdit.Text := FloatToStr(Foot);
      InchEdit.Text := '';
      FracEdit.Text := '';
    end;
end;

procedure ToUser(EngSign: TComboBox; FootEdit, InchEdit, FracEdit: TLabeledEdit;
  FracCombo: TComboBox; Value: Double; Preferences: TPreferences);
var
  Feet : Integer;
  Foot : Double;
  Inch : Integer;
  Fr   : Double;
  Neg : Boolean;
begin
  Neg := Value < 0.0;
  if Neg then
    begin
      EngSign.ItemIndex := 1;
      Value := - Value;
    end
  else
    begin
      EngSign.ItemIndex := 0;
    end;

  if Preferences.InchesAndFractions then
    begin
      Feet := Trunc( Value * MicronsToFt);
      Inch := Trunc( Value * MicronsToIn);
      Fr := Round((Value * MicronsToIn - Inch)*64);
      Inch := Inch - (Feet * 12);
      FootEdit.Text := IntToStr(Feet);
      InchEdit.Text := IntToStr(Inch);
      FracEdit.Text := FloatToStr(Fr); // LOOK HERE  -  Temporary!!!!
    end
  else
    begin
      Foot := Value * MicronsToFt;
      FootEdit.Text := FloatToStr(Foot);
      InchEdit.Text := '0';
      FracEdit.Text := '0';
    end;

end;

procedure ToUser(MetSign: TComboBox; Meters: TLabeledEdit; Value: Double;
  Preferences: TPreferences);
var
  Feet : Integer;
  Inch : Integer;
  Fr   : Double;
  Neg : Boolean;
begin
  Neg := Value < 0.0;
  if Neg then
    begin
      MetSign.ItemIndex := 1;
      Value := - Value;
    end
  else
    begin
      MetSign.ItemIndex := 0;
    end;

  if Preferences.Centimeters then
    Meters.Text := FloatToStr(Value * MicronsToCm)
  else
    Meters.Text := FloatToStr(Value * MicronsToM);

end;

function FromUser(EngSign: TComboBox; FootEdit, InchEdit, FracEdit: TLabeledEdit;
  FracCombo: TComboBox; MetSign: TComboBox; Meters: TLabeledEdit;
  Preferences: TPreferences): Double;
begin
  if Preferences.EnglishUnits then
    Result := FromUser(EngSign,FootEdit,InchEdit,FracEdit,FracCombo,Preferences)
  else
    Result := FromUser(MetSign,Meters,Preferences);
end;

function FromUser(EngSign: TComboBox; FootEdit, InchEdit,
  FracEdit: TLabeledEdit; FracCombo: TComboBox; Preferences: TPreferences
  ): Double;
var
  F : Double;
begin
  Result := StringToFloat(FootEdit.Text) * FtToMicrons + StringToInt(InchEdit.Text) * InToMicrons;
  F := StringToInt(FracEdit.Text) / Denominator[FracCombo.ItemIndex];
  Result := Result + F * InToMicrons;
  if EngSign.ItemIndex = 1 then
    Result := -Result;
end;

function FromUser(MetSign: TComboBox; Meters: TLabeledEdit;
  Preferences: TPreferences): Double;
begin
  Result := StringToFloat(Meters.Text);
  if Preferences.Centimeters then
    Result := Result * CmToMicrons
  else
    Result := Result * MToMicrons;
  if MetSign.ItemIndex = 1 then
    Result := -Result;
end;

function ToString(Value: Double; Preferences: TPreferences): String;
begin
  if Preferences.EnglishUnits then
    begin
      if Preferences.InchesAndFractions then
        begin
          Result := FloatToStr( Value );  { TODO 2 -oDonz -cUser Interface : Change this to Ft, In, Fraction }
        end
      else
        begin
          Result := FloatToStr( Value );  { TODO 3 -oDon Z -cUser Interface : Check the fractional significance }
        end;
    end
  else
    begin

    end;
end;

function ToString(Pixels: Integer; Preferences: TPreferences): String;
begin
  Result := ToString( PixelsToValue( Pixels, Preferences ), Preferences );
end;

function MicronsToPixels(Value: Double; Preferences: TPreferences): Integer;
var
  Inches : Double;
begin
  Inches := Value * MicronsToIn / Preferences.Zoom;
  Result := Round( Inches * 96.0 );
end;

function PixelsToMicrons(Value: Integer; Preferences: TPreferences): Double;
var
  Inches : Double;
begin
  Inches := Value / 96.0;
  Result := Inches * InToMicrons * Preferences.Zoom;
end;

function PixelsToValue(Value: Integer; Preferences: TPreferences): Double;
begin
  if Preferences.EnglishUnits then
    Result := PixelsToMicrons( Value, Preferences ) * MicronsToFt
  else
    Result := PixelsToMicrons( Value, Preferences ) * MicronsToM;
end;

end.

