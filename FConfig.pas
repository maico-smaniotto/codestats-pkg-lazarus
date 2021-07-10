{
    This file is part of the Code::Stats Package

    Config form
    Copyright (c) 2021 by Maico Smaniotto maicosmaniotto@yahoo.com.br

    See the file LICENSE.txt, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit FConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Windows;

type
  TFormConfig = class(TForm)
    BtOK: TButton;
    BtCancel: TButton;
    CkEnable: TCheckBox;
    GbFields: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    EdApiToken: TEdit;
    EdApiUrl: TEdit;
    CkShowMessages: TCheckBox;
    procedure CkEnableClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtOKClick(Sender: TObject);
  private

  public

  end;

implementation

{$R *.lfm}

{ TFormConfig }

procedure TFormConfig.CkEnableClick(Sender: TObject);
begin
  GbFields.Enabled := CkEnable.Checked;
end;

procedure TFormConfig.FormShow(Sender: TObject);
begin
  CkEnableClick(CkEnable);
end;

procedure TFormConfig.BtOKClick(Sender: TObject);
begin
  if CkEnable.Checked then
  begin
    if EdApiToken.Text = '' then
    begin
      Application.MessageBox('Error', 'API token must be informed.', MB_ICONERROR + MB_OK);
      EdApiToken.SetFocus;
      Exit;
    end;
    if EdApiUrl.Text = '' then
    begin
      Application.MessageBox('Error', 'API URL must be informed.', MB_ICONERROR + MB_OK);
      EdApiUrl.SetFocus;
      Exit;
    end;
  end;
  ModalResult := mrOk;
end;

end.

