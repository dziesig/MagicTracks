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

unit DrawingFrame1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, Menus,
  DrawingCommon1, Drawing1, DrawingObject1;

type

  { TDrawingFrame }

  TDrawingFrame = class(TFrame)
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    PopupMenu1: TPopupMenu;
    Ruler_XPB: TPaintBox;
    Ruler_YPB: TPaintBox;
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseEnter(Sender: TObject);
    procedure PaintBox1MouseLeave(Sender: TObject);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox1Resize(Sender: TObject);
    procedure Ruler_XPBMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Ruler_XPBMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Ruler_XPBMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Ruler_YPBMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Ruler_YPBMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Ruler_YPBMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Ruler_YPBPaint(Sender: TObject);
    procedure Ruler_XPBPaint(Sender: TObject);
  private
    { private declarations }
    fBoxType : TDrawingBox;
    fDrawing : TDrawing;

    // Ruler Mouse ops;
    vXMouseDown : Boolean;
    vXStart : Double;
    vYMouseDown : Boolean;
    vYStart : Double;

    vMouseJustEntered : Boolean;
    vMouseLastX : Double;
    vMouseXPixels : Integer;
    vMouseLastY : Double;
    vMouseYPixels : Integer;
    vMouseOverGuide1X,
    vMouseOverGuide1Y,
    vMouseOverGuide2X,
    vMouseOverGuide2Y : Boolean;

    vMouseGuide1XStart,
    vMouseGuide1YStart,
    vMouseGuide2XStart,
    vMouseGuide2YStart : Double;

    vMouseGuide1XTracking,
    vMouseGuide1YTracking,
    vMouseGuide2XTracking,
    vMouseGuide2YTracking : Boolean;

    vMouseDownX,
    vMouseDownY : Integer;

    vDrawingObjects : TDrawingObjectRaster;

    vMouseInPaintBox : Boolean;
    vRecursionDepth : Integer;

    function GetGuide1X: Double;
    function GetGuide1Y: Double;
    function GetGuide2X: Double;
    function GetGuide2Y: Double;
    procedure PaintRulerBackground( RulerPB : TPaintBox );
    procedure PaintDrawingBackground;
    procedure PaintDrawing;
    procedure SetBoxType(const AValue: TDrawingBox);
    procedure SetDrawing(const AValue: TDrawing);

    function MouseX( X : Integer ) : Double; // Returns Mouse X in DRAWING coords.
    function MouseY( Y : Integer ) : Double;

    function PixelsX( X : Double ) : Integer;
    function PixelsY( Y : Double ) : Integer;
    procedure SetGuide1X(const AValue: Double);
    procedure SetGuide1Y(const AValue: Double);
    procedure SetGuide2X(const AValue: Double);
    procedure SetGuide2Y(const AValue: Double);

  public
    { public declarations }

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure PositionLowerLeft;
    procedure Invalidate; override;

    function  DrawingObject( X, Y : Integer) : TDrawingObjectReference; overload;
    procedure DrawingObject( X, Y : Integer; Obj : TDrawingObject; Reference : Integer ); overload;

    property BoxType : TDrawingBox read fBoxType write SetBoxType;
    property Drawing : TDrawing read fDrawing write SetDrawing;

    property Guide1X : Double read GetGuide1X write SetGuide1X;
    property Guide1Y : Double read GetGuide1Y write SetGuide1Y;
    property Guide2X : Double read GetGuide2X write SetGuide2X;
    property Guide2Y : Double read GetGuide2Y write SetGuide2Y;

  end; 

implementation

{$R *.lfm}

uses
  Common1, Graphics, UnitConversion1, Preferences1, Math,
  DrawingSetFrame1, Internals1;

const
  RulerHackColor = clGreen;

type
  TInchesDraw = ( idInchesPlusFractions,
                  idInchesOnly,
                  idSixInches,
                  idFeet,
                  idTenFeet );

const
  InchesDraw : array [0..MaxZoom] of TInchesDraw =
    ( idInchesPlusFractions, idInchesPlusFractions, idInchesPlusFractions, idInchesPlusFractions,
      idInchesPlusFractions, idInchesPlusFractions, idInchesOnly,          idInchesOnly,
      idInchesOnly,          idInchesOnly,          idSixInches,           idSixInches,
      idSixInches,           idSixInches,           idSixInches,           idSixInches,
      idFeet,                idFeet,                idTenFeet,             idTenFeet );

{ TDrawingFrame }

procedure TDrawingFrame.PaintRulerBackground(RulerPB: TPaintBox);
var
  RulerRect : TRect;
begin
  { Setup and draw the ruler background }
  if fDrawing = nil then exit;
  RulerRect := RectToOrigin(RulerPB.BoundsRect );

  RulerPB.Canvas.Brush.Color := clYellow;
  RulerPB.Canvas.Brush.Style := bsSolid;
  RulerPB.Canvas.Pen.Width := 1;
  RulerPB.Canvas.Pen.Style := psSolid;
  RulerPB.Canvas.Pen.Color := clBlack;

  RulerPB.Canvas.Rectangle(RulerRect);

end;

function TDrawingFrame.PixelsX(X: Double): Integer;
begin
  Result := MicronsToPixels( X - fDrawing.MinX[BoxType], fDrawing.Preferences );
end;

function TDrawingFrame.PixelsY(Y: Double): Integer;
begin
  Result := MicronsToPixels( Y - fDrawing.MinY[BoxType], fDrawing.Preferences );
end;

procedure TDrawingFrame.PaintDrawingBackground;
var
  DrawingRect : TRect;
  ApproxGridPixels : Integer;
  I, J, JJ : Integer;
  X, OldX : Integer;
  Y, OldY : Integer;
  XX, YY : Double;
  oldColor : TColor;
  oldPenStyle : TPenStyle;
begin
  { Setup and draw the drawing background }

  if fDrawing = nil then exit;
  DrawingRect := RectToOrigin(PaintBox1.BoundsRect );
  PaintBox1.Canvas.Brush.Color := clWhite;
  PaintBox1.Canvas.Brush.Style := bsSolid;
  PaintBox1.Canvas.Pen.Width := 1;
  PaintBox1.Canvas.Pen.Style := psSolid;
  PaintBox1.Canvas.Pen.Color := clBlack;
  PaintBox1.Canvas.Rectangle( DrawingRect );

  if fDrawing.Preferences.ShowGrid then
    begin

  { See if the grid is worthwile }

      ApproxGridPixels := MicronsToPixels( fDrawing.Preferences.GridSpacingMicrons, fDrawing.Preferences );

      if ApproxGridPixels >= 10 then  { can't tell them apart }
        begin
          oldColor := PaintBox1.Canvas.Pen.Color;
          PaintBox1.Canvas.Pen.Color := clBlack;
          XX := PixelsToMicrons(-1,Drawing.Preferences);
          OldX := Floor( XX /  fDrawing.Preferences.GridSpacingMicrons);
          for I := 0 to Pred(Ruler_XPB.Width) do
            begin
              XX := PixelsToMicrons(I,Drawing.Preferences);
              X := Floor( XX / fDrawing.Preferences.GridSpacingMicrons);
              if (X <> OldX) then
                begin
                  OldX := X;
                  YY := PixelsToMicrons(PaintBox1.Height,Drawing.Preferences);
                  OldY := Floor( YY / fDrawing.Preferences.GridSpacingMicrons );
                  for J := Pred(Ruler_YPB.Height) downto 0 do
                    begin
                      JJ := pred(PaintBox1.Height) - J;
                      YY := PixelsToMicrons(JJ,Drawing.Preferences);
                      Y := Floor( YY / fDrawing.Preferences.GridSpacingMicrons );
                      if (Y <> OldY) then
                        begin
                          OldY := Y;
                          PaintBox1.Canvas.MoveTo( I, J );
                          PaintBox1.Canvas.LIneTo( I+1, J+1 );
                        end;
                    end;
                end;
            end;
          PaintBox1.Canvas.Pen.Color := oldColor;
        end;
    end;

  // Draw the guides
  oldPenStyle := PaintBox1.Canvas.Pen.Style;
  X := PaintBox1.Width;
  Y := PaintBox1.Height;
  PaintBox1.Canvas.Pen.Style := psDashDot;
  I := PixelsX( Guide2X );
  J := PixelsY( Guide2Y );
  PaintBox1.Canvas.MoveTo( I, 0);
  PaintBox1.Canvas.LineTo( I, Y);
  PaintBox1.Canvas.MoveTo( 0, Y-J);
  PaintBox1.Canvas.LineTo( X, Y-J);
  PaintBox1.Canvas.Pen.Style := oldPenStyle;
  PaintBox1.Canvas.Pen.Style := psDash;
  I := PixelsX( Guide1X );
  J := PixelsY( Guide1Y );
  PaintBox1.Canvas.MoveTo( I, 0);
  PaintBox1.Canvas.LineTo( I, Y);
  PaintBox1.Canvas.MoveTo( 0, Y-J);
  PaintBox1.Canvas.LineTo( X, Y-J);

end;

procedure TDrawingFrame.PaintBox1Paint(Sender: TObject);
begin
  if Drawing = nil then exit;
  PaintDrawingBackground;
  PaintDrawing;
end;

procedure TDrawingFrame.PaintBox1Resize(Sender: TObject);
var
  W, H : Integer;
  L : Integer;
begin
  W := PaintBox1.Width;
  H := PaintBox1.Height;
  InternalsForm1.PutEvent('Resize ' + Name, 'W x H:  ' + IntToStr(W) + ' x ' + IntToStr(H));
  L := H * W;
  InternalsForm1.PutEvent('Paintbox size ' + Name, IntToStr(L));
  vDrawingObjects.Resize( W, H );
end;

procedure TDrawingFrame.PaintDrawing;
var
  X, Y : Integer;
begin
  // Draw the Active Layers of the Drawing;

  Drawing.Draw( Self );
end;

constructor TDrawingFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  vDrawingObjects := TDrawingObjectRaster.Create;
  vMouseInPaintBox := False;
  vRecursionDepth := 0;
end;

destructor TDrawingFrame.Destroy;
begin
  vDrawingObjects.Free;
  inherited Destroy;
end;

function TDrawingFrame.DrawingObject(X, Y: Integer): TDrawingObjectReference;
begin
  Result := vDrawingObjects.DrawingObject( X, Y );
end;

procedure TDrawingFrame.DrawingObject(X, Y: Integer;  Obj : TDrawingObject; Reference : Integer );
begin
  vDrawingObjects.DrawingObject( X, Y, Obj, Reference );
end;

function TDrawingFrame.GetGuide1X: Double;
begin
  case BoxType of
    XY : Result := Drawing.Guide1X;
    XZ : Result := Drawing.Guide1X;
    YZ : Result := Drawing.Guide1Y;
  end;
end;

function TDrawingFrame.GetGuide1Y: Double;
begin
  case BoxType of
    XY : Result := Drawing.Guide1Y;
    XZ : Result := Drawing.Guide1Z;
    YZ : Result := Drawing.Guide1Z;
  end;
end;

function TDrawingFrame.GetGuide2X: Double;
begin
  case BoxType of
    XY : Result := Drawing.Guide2X;
    XZ : Result := Drawing.Guide2X;
    YZ : Result := Drawing.Guide2Y;
  end;
end;

function TDrawingFrame.GetGuide2Y: Double;
begin
  case BoxType of
    XY : Result := Drawing.Guide2Y;
    XZ : Result := Drawing.Guide2Z;
    YZ : Result := Drawing.Guide2Z;
  end;
end;

procedure TDrawingFrame.Invalidate;
begin
  PaintBox1.Invalidate;
end;

procedure TDrawingFrame.MenuItem1Click(Sender: TObject);
begin
  Guide1X := MouseX( vMouseDownX );
  Guide1Y := MouseY( vMouseDownY );
  TDrawingSetFrame( Owner ).Invalidate;
end;

procedure TDrawingFrame.MenuItem2Click(Sender: TObject);
begin
  Guide2X := MouseX( vMouseDownX );
  Guide2Y := MouseY( vMouseDownY );
  TDrawingSetFrame( Owner ).Invalidate;
end;

function TDrawingFrame.MouseX(X: Integer): Double;
var
  XX, Spacing : Double;
  I  : Integer;
begin
  Result := PixelsToMicrons( X, fDrawing.Preferences) + fDrawing.MinX[BoxType];
  if fDrawing.Preferences.SnapToGrid then
    begin
      Spacing := fDrawing.Preferences.GridSpacingMicrons;
      XX := Result / Spacing;
      I := Round(XX);
      Result := (I * Spacing);
    end;
end;

function TDrawingFrame.MouseY(Y: Integer): Double;
var
  XX, Spacing : Double;
  I  : Integer;
begin
  Result := PixelsToMicrons( Y, fDrawing.Preferences) + fDrawing.MinY[BoxType];
  if fDrawing.Preferences.SnapToGrid then
    begin
      Spacing := fDrawing.Preferences.GridSpacingMicrons;
      XX := Result / Spacing;
      I := Round(XX);
      Result := (I * Spacing);
    end;
end;

procedure TDrawingFrame.PaintBox1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Obj : TDrawingObjectReference;
begin
//  InternalsForm1.PutEvent('PaintBox1MouseDown ' + Name + ' X, Y:  ', IntToStr(X) + ', ' + IntToStr(Y));
  if Button = mbRight then
    begin
      // Set Guide to current X Y
      vMouseDownX := X;
      Y := pred(PaintBox1.Height) - Y;
      vMouseDownY := Y;
      PopupMenu1.PopUp;
      exit;
    end;

  if Button = mbLeft then
    begin
      Obj := DrawingObject(X, Y);
      if Obj.Obj = nil then
        begin
          fDrawing.Layers.Deselect;
        end
      else
        begin
          if not (ssCtrl in Shift) then
            begin
              fDrawing.Layers.Deselect;
              Obj.Obj.Select;
            end
          else
            Obj.Obj.ToggleSelect;
        end;
      TDrawingSetFrame( Owner ).Invalidate;
    end;
  if vMouseOverGuide1X then
    begin
      vMouseGuide1XStart := MouseX( X );
      vMouseGuide1XTracking := True;
    end
  else if vMouseOverGuide1Y then
    begin
      vMouseGuide1YStart := MouseY( Y );
      vMouseGuide1YTracking := True;
    end
  else if vMouseOverGuide2X then
    begin
      vMouseGuide2XStart := MouseX( X );
      vMouseGuide2XTracking := True;
    end
  else if vMouseOverGuide2Y then
    begin
      vMouseGuide2YStart := MouseY( Y );
      vMouseGuide2YTracking := True;
    end;
end;

procedure TDrawingFrame.PaintBox1MouseEnter(Sender: TObject);
begin
  if fDrawing = nil then exit;
  vMouseJustEntered := True;
  vMouseInPaintBox := True;
//  InternalsForm1.PutEvent('PaintBox1MouseEnter ' + Name ,'');
end;

procedure TDrawingFrame.PaintBox1MouseLeave(Sender: TObject);
var
  PM : TPenMode;
  OldColor : TColor;
begin
  if fDrawing = nil then exit;
  vMouseInPaintBox := False;
//  InternalsForm1.PutEvent('PaintBox1MouseLeave ' + Name ,'');
  PM := Ruler_XPB.Canvas.Pen.Mode;
  OldColor := Ruler_XPB.Canvas.Pen.Color;
  Ruler_XPB.Canvas.Pen.Color := RulerHackColor;
  Ruler_XPB.Canvas.Pen.Mode := pmNotXOR;
  Ruler_XPB.Canvas.MoveTo(vMouseXPixels,0);
  Ruler_XPB.Canvas.LineTo(vMouseXPixels,Ruler_XPB.Height);
  Ruler_XPB.Canvas.Pen.Mode := PM;
  Ruler_XPB.Canvas.Pen.Color := OldColor;

  PM := Ruler_YPB.Canvas.Pen.Mode;
  OldColor := Ruler_YPB.Canvas.Pen.Color;
  Ruler_YPB.Canvas.Pen.Color := RulerHackColor;
  Ruler_YPB.Canvas.Pen.Mode := pmNotXOR;
  Ruler_YPB.Canvas.MoveTo(0,pred(PaintBox1.Height) - vMouseYPixels);
  Ruler_YPB.Canvas.LineTo(Ruler_YPB.Height,pred(PaintBox1.Height) - vMouseYPixels);
  Ruler_YPB.Canvas.Pen.Mode := PM;
  Ruler_YPB.Canvas.Pen.Color := OldColor;
end;

procedure TDrawingFrame.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
const
  Thresh = 1; // Temp for development
var
  PM : TPenMode;

  OldColor : TColor;
  DG1X, DG1Y : Double;
  DG2X, DG2Y : Double;

  DX1, DX2, DY1, DY2 : Double;

  YPrime : Integer;

  Obj : TDrawingObjectReference;

begin
  if fDrawing = nil then exit;
  if not vMouseInPaintBox then exit;
  if VRecursionDepth > 0 then exit;
  Inc(vRecursionDepth);
  OldColor := Ruler_XPB.Canvas.Pen.Color;
  Ruler_XPB.Canvas.Pen.Color := RulerHackColor;
  Ruler_YPB.Canvas.Pen.Color := RulerHackColor;
  if not vMouseJustEntered then
    begin
      PM := Ruler_XPB.Canvas.Pen.Mode;
      Ruler_XPB.Canvas.Pen.Mode := pmNotXOR;
      Ruler_XPB.Canvas.MoveTo(vMouseXPixels,0);
      Ruler_XPB.Canvas.LineTo(vMouseXPixels,Ruler_XPB.Height);
      Ruler_XPB.Canvas.Pen.Mode := PM;

      PM := Ruler_YPB.Canvas.Pen.Mode;
      Ruler_YPB.Canvas.Pen.Mode := pmNotXOR;
      Ruler_YPB.Canvas.MoveTo(0,pred(PaintBox1.Height) - vMouseYPixels);
      Ruler_YPB.Canvas.LineTo(Ruler_YPB.Height,pred(PaintBox1.Height) - vMouseYPixels);
      Ruler_YPB.Canvas.Pen.Mode := PM;
    end;

  vMouseJustEntered := False;
  vMouseLastX := MouseX( X );

  vMouseXPixels := MicronsToPixels( vMouseLastX - fDrawing.MinX[fBoxType], fDrawing.Preferences );
  PM := Ruler_XPB.Canvas.Pen.Mode;
  Ruler_XPB.Canvas.Pen.Mode := pmNotXOR;
  Ruler_XPB.Canvas.MoveTo(vMouseXPixels,0);
  Ruler_XPB.Canvas.LineTo(vMouseXPixels,Ruler_XPB.Height);
  Ruler_XPB.Canvas.Pen.Mode := PM;

  YPrime := pred(PaintBox1.Height) - Y;
  vMouseLastY := MouseY( YPrime );

  vMouseYPixels := MicronsToPixels( vMouseLastY - fDrawing.MinY[fBoxType], fDrawing.Preferences );
  PM := Ruler_YPB.Canvas.Pen.Mode;
  Ruler_YPB.Canvas.Pen.Mode := pmNotXOR;
  Ruler_YPB.Canvas.MoveTo(0,pred(PaintBox1.Height) - vMouseYPixels);
  Ruler_YPB.Canvas.LineTo(Ruler_YPB.Height,pred(PaintBox1.Height) - vMouseYPixels);
  Ruler_YPB.Canvas.Pen.Mode := PM;
  Ruler_XPB.Canvas.Pen.Color := OldColor;
  Ruler_YPB.Canvas.Pen.Color := OldColor;

// Check if Mouse is over one of the guides (starting with 1)

  vMouseOverGuide1X := false;
  vMouseOverGuide1y := false;
  vMouseOverGuide2X := false;
  vMouseOverGuide2Y := false;

  DG1X := Guide1X;
  DG1Y := Guide1Y;
  DG2X := Guide2X;
  DG2Y := Guide2Y;

  DX1 := MicronsToPixels(vMouseLastX - DG1X, fDrawing.Preferences );
  DY1 := MicronsToPixels(vMouseLastY - DG1Y, fDrawing.Preferences );
  DX2 := MicronsToPixels(vMouseLastX - DG2X, fDrawing.Preferences );
  DY2 := MicronsToPixels(vMouseLastY - DG2Y, fDrawing.Preferences );

  if abs(DX1) < Thresh then
    vMouseOverGuide1X := true
  else if abs(DY1) < Thresh then
    vMouseOverGuide1Y := true
  else if abs(DX2) < Thresh then
    vMouseOverGuide2X := true
  else if abs(DY2) < Thresh then
    vMouseOverGuide2Y := true;

  if vMouseOverGuide1X or vMouseOverGuide2X then
    PaintBox1.Cursor := crHSplit
  else if vMouseOverGuide1Y or vMouseOverGuide2Y then
    PaintBox1.Cursor := crVSplit
  else
    PaintBox1.Cursor := crDefault;

  if ssLeft in Shift then
    begin
      if vMouseGuide1XTracking then
        Guide1X := vMouseLastX
      else if vMouseGuide1YTracking then
        Guide1Y := vMouseLastY
      else if vMouseGuide2XTracking then
        Guide2X := vMouseLastX
      else if vMouseGuide2YTracking then
        Guide2Y := vMouseLastY;
      TDrawingSetFrame(Owner).Invalidate;

      Application.ProcessMessages;
    end;

  try
    Obj := DrawingObject(X, Y);
    if Obj.Obj <> nil then
      begin
        if Obj.Obj.Selected then
          if Obj.Ref = 0 then
            PaintBox1.Cursor := crSize // For moving the object.
          else
            PaintBox1.Cursor := crCross // For moving handle
        else
          PaintBox1.Cursor := crHandPoint;
        Application.ProcessMessages;
      end;
  except
    InternalsForm1.PutEvent('Exception:  ',(Sender as TComponent).Name);
    InternalsForm1.PutEvent('Exception:  ',Name);
    InternalsForm1.PutEvent('Depth:  ',IntToStr(vRecursionDepth));
    InternalsForm1.PutEvent('PB W x H:  ',IntToStr(PaintBox1.Width) + ' x ' + IntToStr(PaintBox1.Height) );
    InternalsForm1.PutEvent('Exception OBJ:  X, Y = ', IntToStr( X ) +  ',' +
                                                        IntToStr( Y ) );
    raise;
  end;
  Dec(vRecursionDepth);
//  InternalsForm1.PutEvent('PaintBox1MouseMove ' + Name + '('+IntToStr(vRecursionDepth) + ') Done','');

end;

procedure TDrawingFrame.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
//  InternalsForm1.PutEvent('PaintBox1MouseUp ' + Name + ' X, Y:  ', IntToStr(X) + ', ' + IntToStr(Y));
  vMouseGuide1XTracking := false;
  vMouseGuide1YTracking := false;
  vMouseGuide2XTracking := false;
  vMouseGuide2YTracking := false;
end;

procedure TDrawingFrame.Ruler_XPBMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if fDrawing = nil then exit;
  vXMouseDown := Button = mbLeft;
  if vXMouseDown then
    begin
      vXStart := Drawing.MinX[fBoxType] + PixelsToMicrons(X,Drawing.Preferences);
     Ruler_XPB.Invalidate;
     Invalidate;
    end;
end;

procedure TDrawingFrame.Ruler_XPBMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  MU : Double;
begin
  if vXMouseDown then
    begin
      MU := PixelsToMicrons(X,Drawing.Preferences);
      Drawing.MinX[BoxType] := vXStart - MU;

      Ruler_XPB.Invalidate;
      Invalidate;
    end;
end;

procedure TDrawingFrame.Ruler_XPBMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  vXMouseDown := False;
end;

procedure TDrawingFrame.Ruler_YPBMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  YPrime : Integer;
begin
  if fDrawing = nil then exit;
  vYMouseDown := Button = mbLeft;
  if vYMouseDown then
    begin
      YPrime := pred(Ruler_YPB.Height) - Y;
      vYStart := Drawing.MinY[fBoxType] + PixelsToMicrons(YPrime,Drawing.Preferences);
      Ruler_YPB.Invalidate;
      Invalidate;
    end;
end;

procedure TDrawingFrame.Ruler_YPBMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  MU : Double;
  YPrime : Integer;
  V : Double;
begin
  if not vYMouseDown then exit;
  YPrime := pred(Ruler_YPB.Height) - Y;
  MU := PixelsToMicrons(YPrime,Drawing.Preferences);
  V := vYStart - MU;
  Drawing.MinY[BoxType] := V;
  Ruler_YPB.Invalidate;
  Invalidate;
end;

procedure TDrawingFrame.Ruler_YPBMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  vYMouseDown := False;
end;

procedure TDrawingFrame.Ruler_XPBPaint(Sender: TObject);
var
  I : Integer;
  X, OldX : Integer;
  XX : Double;
  MX : Double;
  DisplayHeight : Integer;
begin
  if Drawing = nil then exit;
  DisplayHeight := Ruler_XPB.Height;
  PaintRulerBackground( Ruler_XPB );
  MX := Drawing.MinX[fBoxType];
  case InchesDraw[Drawing.Preferences.ZoomIndex] of
    idInchesPlusFractions:
      begin
        XX := PixelsToMicrons(-1,Drawing.Preferences) + MX;
        OldX := Floor( XX * MicronsToFt * 48);
        for I := 0 to Pred(Ruler_XPB.Width) do
          begin
            XX := PixelsToMicrons(I,Drawing.Preferences) + MX;
            X := Floor( XX * MicronsToFt * 48);
            if (X <> OldX) then
              begin
                OldX := X;
                Ruler_XPB.Canvas.MoveTo(I,0);

                if (X mod 48) = 0 then
                  begin
                    Ruler_XPB.Canvas.Pen.Width := 2;                    Ruler_XPB.Canvas.MoveTo(I,0);
                    Ruler_XPB.Canvas.LineTo(I,Trunc(DisplayHeight * 0.67));
                    Ruler_XPB.Canvas.Pen.Width := 1;
                    Ruler_XPB.Canvas.TextOut(I+3,Trunc(DisplayHeight * 0.45),IntToStr(X div 48)+'''');
                  end
                else if (X mod 24) = 0 then
                  begin
                    Ruler_XPB.Canvas.LineTo(I,Trunc(DisplayHeight * 0.67));
                    Ruler_XPB.Canvas.TextOut( I+3,Trunc(DisplayHeight * 0.45),
                                        IntToStr(X div 48)+'''' + '6"');
                  end
                else if (X mod 4) = 0 then
                  Ruler_XPB.Canvas.LineTo(I,Trunc(DisplayHeight * 0.5))
                else
                  Ruler_XPB.Canvas.LineTo(I,Trunc(DisplayHeight * 0.2));
              end;

          end;
      end;
    idInchesOnly:
      begin
        XX := PixelsToMicrons(-1,Drawing.Preferences) + MX;
        OldX := Floor( XX * MicronsToFt * 12);
        for I := 0 to Pred(Ruler_XPB.Width) do
          begin
            XX := PixelsToMicrons(I,Drawing.Preferences) + MX;
            X := Floor( XX * MicronsToFt * 12);
            if (X <> OldX) then
              begin
                OldX := X;
                Ruler_XPB.Canvas.MoveTo(I,0);
                if (X mod 6) = 0 then
                  begin
                    Ruler_XPB.Canvas.LineTo(I,Trunc(DisplayHeight * 0.5));
                  end;
                if (X mod 12) = 0 then
                  begin
                    Ruler_XPB.Canvas.Pen.Width := 2;                    Ruler_XPB.Canvas.MoveTo(I,0);
                    Ruler_XPB.Canvas.LineTo(I,Trunc(DisplayHeight * 0.67));
                    Ruler_XPB.Canvas.Pen.Width := 1;                    Ruler_XPB.Canvas.MoveTo(I,0);
                    Ruler_XPB.Canvas.TextOut(I+3,Trunc(DisplayHeight * 0.45),IntToStr(X div 12)+'''');
                  end
                else
                  Ruler_XPB.Canvas.LineTo(I,Trunc(DisplayHeight * 0.3));
              end;
          end;
      end;
    idSixInches:
      begin
        XX := PixelsToMicrons(-1,Drawing.Preferences) + MX;
        OldX := Floor( XX * MicronsToFt * 6);
        for I := 0 to Pred(Ruler_XPB.Width) do
          begin
            XX := PixelsToMicrons(I,Drawing.Preferences) + MX;
            X := Floor( XX * MicronsToFt * 6);
            if (X <> OldX) then
              begin
                OldX := X;
                Ruler_XPB.Canvas.MoveTo(I,0);
                if (X mod 3) = 0 then
                  begin
                    Ruler_XPB.Canvas.LineTo(I,Trunc(DisplayHeight * 0.5));
                  end;
                if (X mod 6) = 0 then
                  begin
                    Ruler_XPB.Canvas.Pen.Width := 2;                    Ruler_XPB.Canvas.MoveTo(I,0);
                    Ruler_XPB.Canvas.LineTo(I,Trunc(DisplayHeight * 0.67));
                    Ruler_XPB.Canvas.Pen.Width := 1;                    Ruler_XPB.Canvas.MoveTo(I,0);
                    Ruler_XPB.Canvas.TextOut(I+3,Trunc(DisplayHeight * 0.45),IntToStr(X div 6)+'''');
                  end
                else
                  Ruler_XPB.Canvas.LineTo(I,Trunc(DisplayHeight * 0.3));
              end;
          end;
      end;
    idFeet:
      begin
        XX := PixelsToMicrons(-1,Drawing.Preferences) + MX;
        OldX := Floor( XX * MicronsToFt * 2);
        for I := 0 to Pred(Ruler_XPB.Width) do
          begin
            XX := PixelsToMicrons(I,Drawing.Preferences) + MX;
            X := Floor( XX * MicronsToFt * 2);
            if (X <> OldX) then
              begin
                OldX := X;
                Ruler_XPB.Canvas.MoveTo(I,0);

                if (X mod 2) = 0 then
                  begin
                    Ruler_XPB.Canvas.Pen.Width := 2;                    Ruler_XPB.Canvas.MoveTo(I,0);
                    Ruler_XPB.Canvas.LineTo(I,Trunc(DisplayHeight * 0.67));
                    Ruler_XPB.Canvas.Pen.Width := 1;                    Ruler_XPB.Canvas.MoveTo(I,0);
                    Ruler_XPB.Canvas.TextOut(I+3,Trunc(DisplayHeight * 0.45),IntToStr(X div 2)+'''');
                  end
                else
                  Ruler_XPB.Canvas.LineTo(I,Trunc(DisplayHeight * 0.3));
               end;
          end;
      end;
    idTenFeet:
      begin
        XX := PixelsToMicrons(-1,Drawing.Preferences) + MX;
        OldX := Floor( XX * MicronsToFt);
        for I := 0 to Pred(Ruler_XPB.Width) do
          begin
            XX := PixelsToMicrons(I,Drawing.Preferences) + MX;
            X := Floor( XX * MicronsToFt);
            if (X <> OldX) then
              begin
                OldX := X;
                Ruler_XPB.Canvas.MoveTo(I,0);
               if (X mod 10) = 0 then
                  begin
                    Ruler_XPB.Canvas.Pen.Width := 2;                    Ruler_XPB.Canvas.MoveTo(I,0);
                    Ruler_XPB.Canvas.LineTo(I,Trunc(DisplayHeight * 0.67));
                    Ruler_XPB.Canvas.Pen.Width := 1;                    Ruler_XPB.Canvas.MoveTo(I,0);
                     Ruler_XPB.Canvas.TextOut(I+3,Trunc(DisplayHeight * 0.45),IntToStr(X)+'''');
                  end
               else
                 Ruler_XPB.Canvas.LineTo(I,Trunc(DisplayHeight * 0.3));

              end;
          end;
      end;
  end;
end;

procedure TDrawingFrame.Ruler_YPBPaint(Sender: TObject);
var
  I, J : Integer;
  X, OldX : Integer;
  XX : Double;
  MX : Double;
  DisplayHeight : Integer;
  DisplayWidth : Integer;
begin
  if Drawing = nil then exit;
  DisplayHeight := Ruler_YPB.Width;
  DisplayWidth  := Ruler_YPB.Height;
  PaintRulerBackground( Ruler_YPB );
  MX := Drawing.MinY[fBoxType];

  {Note that the ruler's pixel coordinates are inverted vs. the drawing vertical }
  {dimension.  To make the majority of the code consistent, the index J is       }
  {is started at the bottom (Height) and decremented to 0.  The index I is mapped}
  {to be the logical (0 at bottom) coordinates and translated into dimensions    }
  { consistent with the user's preferences.  Drawing is performed at J.          }

  case InchesDraw[Drawing.Preferences.ZoomIndex] of
    idInchesPlusFractions:
      begin
        XX := PixelsToMicrons(-1,Drawing.Preferences) + MX;
        OldX := Floor( XX * MicronsToFt * 48); { 1/4" tics }
        for J := Pred(Ruler_YPB.Height) downto 0 do
          begin
            I := DisplayWidth - J;
            XX := PixelsToMicrons(I,Drawing.Preferences) + MX;
            X := Floor( XX * MicronsToFt * 48);
            if (X <> OldX) then
              begin
                OldX := X;
                Ruler_YPB.Canvas.MoveTo(DisplayHeight,J);
                if (X mod 48) = 0 then
                  begin
                    Ruler_YPB.Canvas.Pen.Width := 2;
                    Ruler_YPB.Canvas.LineTo(Trunc(DisplayHeight * 0.33), J);
                    Ruler_YPB.Canvas.Pen.Width := 1;
                    AngleTextOut( Ruler_YPB.Canvas, 270, Trunc(DisplayHeight * 0.45) +1,
                                  J-3,IntToStr(X div 48)+'''');
                  end
                else if (X mod 24) = 0 then
                  begin
                    Ruler_YPB.Canvas.LineTo(Trunc(DisplayHeight * 0.33),J);
                    AngleTextOut( Ruler_YPB.Canvas, 270, Trunc(DisplayHeight * 0.45) +1,
                                  J-3,IntToStr(X div 48)+'''' + '6"');
                  end
                else if (X mod 4) = 0 then
                  Ruler_YPB.Canvas.LineTo(Trunc(DisplayHeight * 0.5),J)
                else
                  Ruler_YPB.Canvas.LineTo(Trunc(DisplayHeight * 0.8),J);
              end;
          end;
      end;
    idInchesOnly:
      begin
        XX := PixelsToMicrons(-1,Drawing.Preferences) + MX;
        OldX := Floor( XX * MicronsToFt * 12); { 1" tics }
        for J := Pred(Ruler_YPB.Height) downto 0 do
          begin
            I := DisplayWidth - J;
            XX := PixelsToMicrons(I,Drawing.Preferences) + MX;
            X := Floor( XX * MicronsToFt * 12);
            if (X <> OldX) then
              begin
                OldX := X;
                Ruler_YPB.Canvas.MoveTo(DisplayHeight,J);
                if (X mod 12) = 0 then
                  begin
                    Ruler_YPB.Canvas.Pen.Width := 2;
                    Ruler_YPB.Canvas.LineTo(Trunc(DisplayHeight * 0.33), J);
                    Ruler_YPB.Canvas.Pen.Width := 1;
                    AngleTextOut( Ruler_YPB.Canvas, 270, Trunc(DisplayHeight * 0.45) +1,
                                  J-3,IntToStr(X div 12)+'''');
                  end
                else if (X mod 6) = 0 then
                  Ruler_YPB.Canvas.LineTo(Trunc(DisplayHeight * 0.5),J)
                else
                  Ruler_YPB.Canvas.LineTo(Trunc(DisplayHeight * 0.8),J);
              end;
          end;
      end;
    idSixInches:
      begin
        XX := PixelsToMicrons(-1,Drawing.Preferences) + MX;
        OldX := Floor( XX * MicronsToFt * 6); { 2" tics }
        for J := Pred(Ruler_YPB.Height) downto 0 do
          begin
            I := DisplayWidth - J;
            XX := PixelsToMicrons(I,Drawing.Preferences) + MX;
            X := Floor( XX * MicronsToFt * 6);
            if (X <> OldX) then
              begin
                OldX := X;
                Ruler_YPB.Canvas.MoveTo(DisplayHeight,J);
                if (X mod 6) = 0 then
                  begin
                    Ruler_YPB.Canvas.Pen.Width := 2;
                    Ruler_YPB.Canvas.LineTo(Trunc(DisplayHeight * 0.33), J);
                    Ruler_YPB.Canvas.Pen.Width := 1;
                    AngleTextOut( Ruler_YPB.Canvas, 270, Trunc(DisplayHeight * 0.45) +1,
                                  J-3,IntToStr(X div 6)+'''');
                  end
                else if (X mod 3) = 0 then
                  Ruler_YPB.Canvas.LineTo(Trunc(DisplayHeight * 0.5),J)
                else
                  Ruler_YPB.Canvas.LineTo(Trunc(DisplayHeight * 0.8),J);
              end;
          end;
      end;
    idFeet:
      begin
        XX := PixelsToMicrons(-1,Drawing.Preferences) + MX;
        OldX := Floor( XX * MicronsToFt * 2);
        for J := Pred(Ruler_YPB.Height) downto 0 do
          begin
            I := DisplayWidth - J;
            XX := PixelsToMicrons(I,Drawing.Preferences) + MX;
            X := Floor( XX * MicronsToFt * 2);
            if (X <> OldX) then
              begin
                OldX := X;
                Ruler_YPB.Canvas.MoveTo(DisplayHeight,J);
                if (X mod 2) = 0 then
                  begin
                    Ruler_YPB.Canvas.Pen.Width := 2;
                    Ruler_YPB.Canvas.LineTo(Trunc(DisplayHeight * 0.33), J);
                    Ruler_YPB.Canvas.Pen.Width := 1;
                    AngleTextOut( Ruler_YPB.Canvas, 270, Trunc(DisplayHeight * 0.45) +1,
                                  J-3,IntToStr(X div 2)+'''');
                  end
                else
                  Ruler_YPB.Canvas.LineTo(Trunc(DisplayHeight * 0.8),J);
              end;
          end;
      end;
    idTenFeet:
      begin
        XX := PixelsToMicrons(-1,Drawing.Preferences) + MX;
        OldX := Floor( XX * MicronsToFt);
        for J := Pred(Ruler_YPB.Height) downto 0 do
          begin
            I := DisplayWidth - J;
            XX := PixelsToMicrons(I,Drawing.Preferences) + MX;
            X := Floor( XX * MicronsToFt);
            if (X <> OldX) then
              begin
                OldX := X;
                Ruler_YPB.Canvas.MoveTo(DisplayHeight,J);
                if (X mod 10) = 0 then
                  begin
                    Ruler_YPB.Canvas.Pen.Width := 2;
                    Ruler_YPB.Canvas.LineTo(Trunc(DisplayHeight * 0.33), J);
                    Ruler_YPB.Canvas.Pen.Width := 1;
                    AngleTextOut( Ruler_YPB.Canvas, 270, Trunc(DisplayHeight * 0.45) +1,
                                  J-3,IntToStr(X)+'''');
                  end
                else
                  Ruler_YPB.Canvas.LineTo(Trunc(DisplayHeight * 0.8),J);
              end;
          end;
      end;
  end;
end;


procedure TDrawingFrame.SetBoxType(const AValue: TDrawingBox);
begin
  if fBoxType=AValue then exit;
  fBoxType:=AValue;
  vMouseJustEntered := False;
end;

procedure TDrawingFrame.SetDrawing(const AValue: TDrawing);
begin
  fDrawing:=AValue;
  Ruler_YPB.Invalidate;
  Ruler_XPB.Invalidate;
  PaintBox1.Invalidate;
end;

procedure TDrawingFrame.SetGuide1X(const AValue: Double);
begin
  case BoxType of
    XY : Drawing.Guide1X := AValue;
    XZ : Drawing.Guide1X := AValue;
    YZ : Drawing.Guide1Y := AValue;
  end;
end;

procedure TDrawingFrame.SetGuide1Y(const AValue: Double);
begin
  case BoxType of
    XY : Drawing.Guide1Y := AValue;
    XZ : Drawing.Guide1Z := AValue;
    YZ : Drawing.Guide1Z := AValue;
  end;
end;

procedure TDrawingFrame.SetGuide2X(const AValue: Double);
begin
  case BoxType of
    XY : Drawing.Guide2X := AValue;
    XZ : Drawing.Guide2X := AValue;
    YZ : Drawing.Guide2Y := AValue;
  end;
end;

procedure TDrawingFrame.SetGuide2Y(const AValue: Double);
begin
  case BoxType of
    XY : Drawing.Guide2Y := AValue;
    XZ : Drawing.Guide2Z := AValue;
    YZ : Drawing.Guide2Z := AValue;
  end;
end;

procedure TDrawingFrame.PositionLowerLeft;
begin
  Drawing.MinX[fBoxType] := 0;
  Drawing.MinY[fBoxType] := 0;

  Ruler_YPB.Invalidate;
  Ruler_XPB.Invalidate;
  PaintBox1.Invalidate;

end;


end.

