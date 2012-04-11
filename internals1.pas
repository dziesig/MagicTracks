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

unit Internals1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Grids, Menus, ExtCtrls, StdCtrls, DrawingCommon1, Drawing1, ThreePoint1,
  Preferences1;

type

  { TInternalsForm1 }

  TInternalsForm1 = class(TForm)
    Button1: TButton;
    GroupBox16: TGroupBox;
    GroupBox17: TGroupBox;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    X0Edit: TEdit;
    X0Edit1: TEdit;
    X0Edit2: TEdit;
    X1Edit1: TEdit;
    X1Edit2: TEdit;
    Y0Edit: TEdit;
    X1Edit: TEdit;
    Y0Edit1: TEdit;
    Y0Edit2: TEdit;
    Y1Edit: TEdit;
    GroupBox15: TGroupBox;
    GroupBox7: TGroupBox;
    GroupBox8: TGroupBox;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    ObjectXEdit: TEdit;
    ObjectYEdit: TEdit;
    ObjectZEdit: TEdit;
    OriginXEdit: TEdit;
    OffsetXEdit: TEdit;
    OriginYEdit: TEdit;
    OffsetYEdit: TEdit;
    OriginZEdit: TEdit;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    GuideX2Edit: TEdit;
    GuideY2Edit: TEdit;
    GuideZ2Edit: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    OffsetZEdit: TEdit;
    TabSheet3: TTabSheet;
    XYMinXEdit: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PageControl1: TPageControl;
    Panel1: TPanel;
    StringGrid1: TStringGrid;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    GuideX1Edit: TEdit;
    GuideY1Edit: TEdit;
    GuideZ1Edit: TEdit;
    XZMinXEdit: TEdit;
    Y1Edit1: TEdit;
    Y1Edit2: TEdit;
    YZMinXEdit: TEdit;
    XYMinYEdit: TEdit;
    XzMinYEdit: TEdit;
    YZMinYEdit: TEdit;
    XYMinZEdit: TEdit;
    XZMinZEdit: TEdit;
    YZMinZEdit: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Label45Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure PutEvent( const Event : String; Value : String ); overload;
    procedure PutEvent( const Event : String; Value : Integer ); overload;
    procedure PutEvent( const Event : String; Value : Double ); overload;

    procedure PutMins( const MinX, MinY, MinZ : TDrawingMins );
    procedure PutGuides( const Guide1X, Guide1Y, Guide1Z, Guide2X, Guide2Y, Guide2Z : Double );

    procedure PutRectOrigin( O : T3Point; Box : TDrawingBox; Pref : TPreferences );
    procedure PutRectOffset( O : T3Point; Box : TDrawingBox; Pref : TPreferences );
    procedure PutRectLength( O : T3Point; Box : TDrawingBox; Pref : TPreferences );

    procedure PutRectPixels( X0, Y0, X1, Y1 : Integer; Box : TDrawingBox );
  end; 

var
  InternalsForm1: TInternalsForm1;

implementation

{$R *.lfm}

uses
  UnitConversion1;

{ TInternalsForm1 }

procedure TInternalsForm1.Button1Click(Sender: TObject);
begin
  StringGrid1.RowCount := 1;
end;

procedure TInternalsForm1.FormCreate(Sender: TObject);
begin
  Application.ProcessMessages;
  StringGrid1.Cells[0,0] := 'Events';
  StringGrid1.Cells[1,0] := 'Value';
  StringGrid1.ColWidths[0] := 120;
  StringGrid1.ColWidths[1] := StringGrid1.Width - 141;
  Application.ProcessMessages;
  StringGrid1.RowCount := 1;
end;

procedure TInternalsForm1.FormResize(Sender: TObject);
begin
  StringGrid1.ColWidths[0] := 120;
  StringGrid1.ColWidths[1] := StringGrid1.Width - 141;
end;

procedure TInternalsForm1.Label45Click(Sender: TObject);
begin

end;

procedure TInternalsForm1.MenuItem2Click(Sender: TObject);
begin
  if MenuItem2.Checked then
    FormStyle := fsStayOnTop
  else
    FormStyle := fsNormal;
end;

procedure TInternalsForm1.PageControl1Change(Sender: TObject);
begin

end;

procedure TInternalsForm1.PutEvent(const Event: String; Value: Double);
begin
  StringGrid1.RowCount := StringGrid1.RowCount + 1;
  Application.ProcessMessages;
  StringGrid1.Cells[0,StringGrid1.RowCount-1] := Event;
  StringGrid1.Cells[1,StringGrid1.RowCount-1] := FloatToStr(Value);
end;

procedure TInternalsForm1.PutEvent(const Event: String; Value: Integer);
begin
  StringGrid1.RowCount := StringGrid1.RowCount + 1;
  Application.ProcessMessages;
  StringGrid1.Cells[0,StringGrid1.RowCount-1] := Event;
  StringGrid1.Cells[1,StringGrid1.RowCount-1] := IntToStr(Value);
end;

procedure TInternalsForm1.PutEvent(const Event: String; Value: String);
begin
  StringGrid1.RowCount := StringGrid1.RowCount + 1;
  Application.ProcessMessages;
  StringGrid1.Cells[0,StringGrid1.RowCount-1] := Event;
  StringGrid1.Cells[1,StringGrid1.RowCount-1] := Value;
end;

procedure TInternalsForm1.PutGuides(const Guide1X, Guide1Y, Guide1Z, Guide2X,
  Guide2Y, Guide2Z: Double);
begin
  GuideX1Edit.Text := FloatToStr( Guide1X );
  GuideY1Edit.Text := FloatToStr( Guide1Y );
  GuideZ1Edit.Text := FloatToStr( Guide1Z );

  GuideX2Edit.Text := FloatToStr( Guide2X );
  GuideY2Edit.Text := FloatToStr( Guide2Y );
  GuideZ2Edit.Text := FloatToStr( Guide2Z );


end;

procedure TInternalsForm1.PutMins(const MinX, MinY, MinZ: TDrawingMins);
begin
  XYMinXEdit.Text := FloatToStr(MinX[XY]);
  XYMinYEdit.Text := FloatToStr(MinY[XY]);
  XYMinZEdit.Text := FloatToStr(MinZ[XY]);

  XZMinXEdit.Text := FloatToStr(MinX[XZ]);
  XZMinYEdit.Text := FloatToStr(MinY[XZ]);
  XZMinZEdit.Text := FloatToStr(MinZ[XZ]);

  YZMinXEdit.Text := FloatToStr(MinX[YZ]);
  YZMinYEdit.Text := FloatToStr(MinY[YZ]);
  YZMinZEdit.Text := FloatToStr(MinZ[YZ]);
end;

procedure TInternalsForm1.PutRectLength(O: T3Point; Box : TDrawingBox; Pref : TPreferences );
begin
  ObjectXEdit.Text := FloatToStr( O.X * MicronsToFt );
  ObjectYEdit.Text := FloatToStr( O.Y * MicronsToFt );
  ObjectZEdit.Text := FloatToStr( O.Z * MicronsToFt );
end;

procedure TInternalsForm1.PutRectOffset(O: T3Point; Box : TDrawingBox; Pref : TPreferences );
begin
  OffsetXEdit.Text := FloatToStr( O.X * MicronsToFt );
  OffsetYEdit.Text := FloatToStr( O.Y * MicronsToFt );
  OffsetZEdit.Text := FloatToStr( O.Z * MicronsToFt );
end;

procedure TInternalsForm1.PutRectOrigin(O: T3Point; Box : TDrawingBox; Pref : TPreferences );
begin
  OriginXEdit.Text := FloatToStr( O.X * MicronsToFt  );
  OriginYEdit.Text := FloatToStr( O.Y * MicronsToFt  );
  OriginZEdit.Text := FloatToStr( O.Z * MicronsToFt  );
end;

procedure TInternalsForm1.PutRectPixels(X0, Y0, X1, Y1: Integer;
  Box: TDrawingBox);
begin
  case Box of
    XY :
      begin
        X0Edit.Text := IntToStr( X0  );
        X1Edit.Text := IntToStr( X1  );
        Y0Edit.Text := IntToStr( Y0  );
        Y1Edit.Text := IntToStr( Y1  );
      end;
    XZ :
      begin
        X0Edit1.Text := IntToStr( X0  );
        X1Edit1.Text := IntToStr( X1  );
        Y0Edit1.Text := IntToStr( Y0  );
        Y1Edit1.Text := IntToStr( Y1  );
      end;
    YZ :
      begin
        X0Edit2.Text := IntToStr( X0  );
        X1Edit2.Text := IntToStr( X1  );
        Y0Edit2.Text := IntToStr( Y0  );
        Y1Edit2.Text := IntToStr( Y1  );
      end;
  end;
end;

end.

