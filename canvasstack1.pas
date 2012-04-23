unit CanvasStack1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
  TCanvasProperties = record
    PenWidth : Integer;
    PenStyle : TPenStyle;
    PenColor : TColor;
    BrushStyle : TBrushStyle;
    BrushColor : TColor;
  end;

  { TCanvasStack }

  TCanvasStack = class
    SP : Integer;
    Size : Integer;
    Data : array of TCanvasProperties;
    constructor Create;
    destructor Destroy;
    procedure Push( Canvas : TCanvas );
    procedure Pop( Canvas : TCanvas );
  end;

var

  CanvasStack : TCanvasStack;

implementation

{ TCanvasStack }

constructor TCanvasStack.Create;
begin
  SP := 0;
  SetLength(Data, 0);
end;

destructor TCanvasStack.Destroy;
begin
  SetLength(Data, 0);
end;

procedure TCanvasStack.Pop(Canvas: TCanvas);
var
  Properties : TCanvasProperties;
begin
  Dec(SP);
  if SP < 0 then raise Exception.Create('Canvas Stack underflow');
  Properties := Data[SP];
  Canvas.Pen.Width := Properties.PenWidth;
  Canvas.Pen.Style := Properties.PenStyle;
  Canvas.Pen.Color := Properties.PenColor;
  Canvas.Brush.Style := Properties.BrushStyle;
  Canvas.Brush.Color := Properties.BrushColor;
end;

procedure TCanvasStack.Push(Canvas: TCanvas);
var
  Properties : TCanvasProperties;
begin
  Properties.PenWidth := Canvas.Pen.Width;
  Properties.PenStyle := Canvas.Pen.Style;
  Properties.PenColor := Canvas.Pen.Color;
  Properties.BrushStyle := Canvas.Brush.Style;
  Properties.BrushColor := Canvas.Brush.Color;
  if SP >= Length(Data) then
    SetLength(Data,(Length(Data) + 1)*2);
  Data[SP] := Properties;
  Inc(SP);
end;

initialization

  CanvasStack := TCanvasStack.Create;

end.

