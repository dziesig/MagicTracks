unit LayerForm1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ColorBox, Buttons, types, Layers1;

type

  { TLayerForm }

  TLayerForm = class(TForm)
    OkBitBtn: TBitBtn;
    CancelBitBtn: TBitBtn;
    SurfaceColorListBox: TColorListBox;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    ViewAllLayersButton: TButton;
    ViewLayersCG: TCheckGroup;
    LineColorListBox: TColorListBox;
    LineStyleCB: TComboBox;
    TrackTypeCB: TComboBox;
    TrackTypeLabel: TLabel;
    RailLabel: TLabel;
    LineSizeCB: TComboBox;
    LineStartCB: TComboBox;
    LineEndingCB: TComboBox;
    RailSizeCB: TComboBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    SizeLabel: TLabel;
    LineStartLabel: TLabel;
    LineEndingLabel: TLabel;
    PaintBox1: TPaintBox;
    EditLayerRG: TRadioGroup;
    procedure OkBitBtnClick(Sender: TObject);
    procedure EditLayerRGClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ViewAllLayersButtonClick(Sender: TObject);
    procedure LineStyleCBChange(Sender: TObject);
    procedure LineStyleCBDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure LineSizeCBChange(Sender: TObject);
    procedure LineSizeCBDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure PaintBox1Paint(Sender: TObject);
  private
    { private declarations }
    fLayers : TLayers;
    fActiveLayer : TLayer;
    fViewLayers : TViewLayers;
    procedure SetActiveLayer(const AValue: TLayer);
    procedure ActiveLayerToControls;
    procedure ControlsToActiveLayer;
    procedure SetViewLayers( const AValue : TViewLayers );
    procedure GetViewLayers( var   AValue : TViewLayers );
  public
    { public declarations }
    property  Layers : TLayers read fLayers;
    procedure Assign( Source : TLayers);
    property  Activelayer : TLayer read fActiveLayer write SetActiveLayer;
  end; 

var
  LayerForm: TLayerForm;

implementation

{$R *.lfm}

uses
  Common1, LCLType;

{ TLayerForm }

procedure TLayerForm.ViewAllLayersButtonClick(Sender: TObject);
var
  Kind : TLayerKinds;
begin
  for Kind in TLayerKinds do
    ViewLayersCG.Checked[ord(Kind)] := True;
end;

procedure TLayerForm.ActiveLayerToControls;
var
  Lines : Boolean;
begin
  LineColorListBox.ItemIndex    := ActiveLayer.LineColor;
  SurfaceColorListBox.ItemIndex := ActiveLayer.SurfaceColor;
  LineStyleCB.ItemIndex         := ActiveLayer.LineStyle;
  LineSizeCB.ItemIndex          := ActiveLayer.LineSize;
  RailSizeCB.ItemIndex          := ActiveLayer.RailSize;
  LineStartCB.ItemIndex         := ActiveLayer.LineStart;
  LineEndingCB.ItemIndex        := ActiveLayer.LineEnd;
  TrackTypeCB.ItemIndex         := ActiveLayer.TrackType;
  Lines := ActiveLayer.LineStyle <= 4;
  LineSizeCB.Visible      := Lines;
  SizeLabel.Visible       := Lines;
  RailSizeCB.Visible      := not Lines;
  RailLabel.Visible       := not Lines;
  LineEndingLabel.Visible := Lines;
  LineEndingCB.Visible    := Lines;
  LineStartLabel.Visible  := Lines;
  LineStartCB.Visible     := Lines;
  TrackTypeLabel.visible  := not Lines;
  TrackTypeCB.Visible     := not Lines;

end;

procedure TLayerForm.Assign(Source: TLayers);
var
  Index : Integer;
begin
  Layers.Assign( Source );
  Index := ord(Layers.ActiveLayerKind);
  fActiveLayer := nil;
  Application.ProcessMessages; // Fix timing bug in Lazarus
  ActiveLayer := TLayer(Layers.Items[Index]);
  Application.ProcessMessages; // Fix timing bug in Lazarus
  ActiveLayerToControls;
  SetViewLayers(Layers.ViewLayers);
end;

procedure TLayerForm.OkBitBtnClick(Sender: TObject);
var
  I          : Integer;
begin
  I := EditLayerRG.ItemIndex;
  fActiveLayer := TLayer(Layers.Items[I]);
  GetViewLayers(fViewLayers);
  Layers.ViewLayers := fViewLayers;
  ControlsToActiveLayer;
end;

procedure TLayerForm.ControlsToActiveLayer;
begin
  if fActiveLayer = nil then raise Exception.Create('Active Layer = nil');
  ActiveLayer.LineColor    := LineColorListBox.ItemIndex;
  ActiveLayer.SurfaceColor := SurfaceColorListBox.ItemIndex;
  ActiveLayer.LineStyle    := LineStyleCB.ItemIndex;
  ActiveLayer.LineSize     := LineSizeCB.ItemIndex;
  ActiveLayer.RailSize     := RailSizeCB.ItemIndex;
  ActiveLayer.LineStart    := LineStartCB.ItemIndex;
  ActiveLayer.LineEnd      := LineEndingCB.ItemIndex;
  ActiveLayer.TrackType    := TrackTypeCB.ItemIndex;
end;

procedure TLayerForm.LineStyleCBChange(Sender: TObject);
var
  Lines : Boolean;
begin
  Lines := LineStyleCB.ItemIndex <= 4;
  RailLabel.Visible := not Lines;
  RailSizeCB.Visible    := not Lines;
  SizeLabel.Visible := Lines;
  LineSizeCB.Visible    := Lines;
  LineEndingLabel.Visible := Lines;
  LineEndingCB.Visible    := Lines;
  LineStartLabel.Visible  := Lines;
  LineStartCB.Visible     := Lines;
  TrackTypeLabel.visible  := not Lines;
  TrackTypeCB.Visible     := not Lines;
end;

procedure TLayerForm.LineStyleCBDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  ComboBox : TComboBox;
  H, W : Integer;
  BG, FG : TColor;
begin
  BG := clGray;
  FG := clGreen;
  ComboBox := Control as TComboBox;
  if State = [odNoAccel,odNoFocusRect] then
    begin
      BG := clWhite;
      FG := clBlack;
    end;
  if odSelected in State then
      begin
        BG := clWhite;
        FG := clBlack;
      end;
  if odFocused in State then
      begin
        BG := clBlack;
        FG := clWhite;
      end;
  if odComboBoxEdit in State then
      begin
        BG := clWhite;
        FG := clBlack;
        ComboBox.Canvas.Font.Color := clBlack;
      end;
  ComboBox.Canvas.Brush.Color := BG;
  ComboBox.Canvas.Pen.Color   := FG;

  ComboBox.Canvas.FillRect(ARect);
  H := ARect.Top + ((ARect.Bottom - ARect.Top) div 2);
  W  := ARect.Right - ARect.Left;
  if Index <= 4 then
    begin
      ComboBox.Canvas.Pen.Style := TPenStyle(Index);
      ComboBox.Canvas.MoveTo(0,H);
      ComboBox.Canvas.LineTo(W,H);
    end
  else
    begin
      ComboBox.Canvas.FillRect(ARect);
      ComboBox.Canvas.TextOut(6,ARect.Top,ComboBox.Items[Index]);
    end;
end;

procedure TLayerForm.EditLayerRGClick(Sender: TObject);
var
  I : Integer;
begin
  I := EditLayerRG.ItemIndex;
  ActiveLayer := TLayer(Layers[I]);
  Layers.ActiveLayerKind := TLayerKinds(I);
end;

procedure TLayerForm.FormCreate(Sender: TObject);
begin
  fLayers := TLayers.Create;
end;

procedure TLayerForm.FormShow(Sender: TObject);
var
  I : Integer;
begin
  fActiveLayer := nil;
  I := ord(Layers.ActiveLayerKind);
  EditlayerRG.ItemIndex := I;
  fActiveLayer := TLayer(Layers[I]);
  ActiveLayerToControls;
end;

procedure TLayerForm.GetViewLayers(var AValue: TViewLayers);
var
  I : TLayerKinds;
begin
  AValue := [];
  for I in TLayerKinds do
    begin
      if ViewLayersCG.Checked[ord(i)] then
        AValue := AValue + [I];
    end;
end;

procedure TLayerForm.LineSizeCBChange(Sender: TObject);
begin
end;

procedure TLayerForm.LineSizeCBDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
const
  LineWidths : array[0..3] of Integer = (1,3,5,7);
var
  ComboBox : TComboBox;
  H, W : Integer;
begin
  ComboBox := Control as TComboBox;
  ComboBox.Canvas.Brush.Color := clWhite;
  ComboBox.Canvas.FillRect(ARect);
  H := ARect.Top + ((ARect.Bottom - ARect.Top) div 2);
  W  := ARect.Right - ARect.Left;
  ComboBox.Canvas.Pen.Style := psSolid;
  ComboBox.Canvas.Pen.Width := LineWidths[Index];
  ComboBox.Canvas.Pen.Color := clBlack;
  ComboBox.Canvas.MoveTo(0,H);
  ComboBox.Canvas.LineTo(W,H);
end;

procedure TLayerForm.PaintBox1Paint(Sender: TObject);
var
  DrawingRect : TRect;
begin
  DrawingRect := RectToOrigin(PaintBox1.BoundsRect );
  PaintBox1.Canvas.Brush.Style := bsSolid;
  PaintBox1.Canvas.Brush.Color := clWhite;
  PaintBox1.Canvas.Pen.Style := psSolid;
  PaintBox1.Canvas.Pen.Color := clBlack;
  PaintBox1.Canvas.Pen.Width := 1;
  PaintBox1.Canvas.Rectangle( DrawingRect )
end;

procedure TLayerForm.SetActiveLayer(const AValue: TLayer);
begin
  if fActiveLayer <> nil then
    begin
      ControlsToActiveLayer;
    end;

  fActiveLayer:=AValue;

  ActiveLayerToControls;

end;

procedure TLayerForm.SetViewLayers(const AValue: TViewLayers);
var
  I : TLayerKinds;
begin
  for I in TLayerKinds do
    ViewLayersCG.Checked[ord(I)] := I in AValue;
end;

end.

