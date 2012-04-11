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

unit About1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

type

  { TAboutForm1 }

  TAboutForm1 = class(TForm)
    BitBtn1: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ConfigFileName: TLabel;
    BuildLabel: TLabel;
    Memo1: TMemo;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  AboutForm1: TAboutForm1;

implementation

{$R *.lfm}

uses
  Main1;

{ TAboutForm1 }

procedure TAboutForm1.BitBtn1Click(Sender: TObject);
begin
  Close;
end;

procedure TAboutForm1.FormCreate(Sender: TObject);
var
  Age : Integer;
  Exe : string;
  TS  : TDateTime;
begin
  Exe := ParamStr(0);
  Age := FileAge(Exe);
  TS  := FileDateToDateTime( Age );
  BuildLabel.Caption := 'This Build:  ' + DateTimeToStr( TS );
  ConfigFileName.Caption := MainForm.ConfigFileName;
end;


end.

