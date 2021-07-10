{
    This file is part of the Code::Stats Package

    Thread for sending pulses to Code::Stats
    Copyright (c) 2021 by Maico Smaniotto maicosmaniotto@yahoo.com.br

    See the file LICENSE.txt, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit uThreadPulse;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, fpjson;

type
  TShowMessageCallback = procedure (const AMessage: String) of object;
  TAddXPCallback = procedure (const ALanguage: String; const AXP: Integer) of object;

  TThreadPulse = class(TThread)
  private
    FApiUrl: String;
    FApiToken: String;
    FContentJsonStr: String;

    FAddXPCallback: TAddXPCallback;
    FArgLanguage: String;
    FArgXP: Integer;

    FShowMessageCallback: TShowMessageCallback;
    FArgMessage: String;

    procedure CallAddXP;
    procedure CallShowMessage;
  protected
    procedure Execute; override;
  public
    property AddXPCallback: TAddXPCallback read FAddXPCallback write FAddXPCallback;
    property ShowMessageCallback: TShowMessageCallback read FShowMessageCallback write FShowMessageCallback;

    constructor Create(CreateSuspended: Boolean; const AApiUrl: String; const AApiToken: String; const AContentJsonStr: String);
  end;

implementation

{ TThreadPulse }

procedure TThreadPulse.CallAddXP;
begin
  if FAddXPCallback <> nil then
    FAddXPCallback(FArgLanguage, FArgXP);
end;

procedure TThreadPulse.CallShowMessage;
begin
  if FShowMessageCallback <> nil then
      FShowMessageCallback(FArgMessage);
end;

procedure TThreadPulse.Execute;
var
  I            : Integer;
  Client       : TFPHttpClient;
  Response     : RawByteString;
  ResponseJSON : TJSONObject;

  ContentJSON  : TJSONObject;
  XPs          : TJSONArray;
begin
  Client   := TFPHttpClient.Create(nil);
  try
    Client.AddHeader('X-API-Token', FApiToken);
    Client.AddHeader('Content-Type', 'application/json; charset=UTF-8');
    Client.RequestBody := TRawByteStringStream.Create(FContentJsonStr);
    try
      Response := Client.Post(FApiUrl);

      if Response <> '' then
      begin
        ResponseJSON := GetJSON(Response) as TJSONObject;
        try
          if ResponseJSON.Names[0] = 'ok' then
          begin
            // received
            ContentJSON := GetJSON(FContentJsonStr) as TJSONObject;
            try
              // retrieve the list of languages and XP
              XPs := ContentJSON.FindPath('xps') as TJsonArray;
              for I := 0 to XPs.Count - 1 do
              begin
                // decreases XP counter for each language
                FArgLanguage := TJSONObject(XPs.Items[I]).Items[0].AsString;
                FArgXP       := -TJSONObject(XPs.Items[I]).Items[1].AsInteger;
                Synchronize(@CallAddXP);
              end;
            finally
              ContentJSON.Free;
            end;
          end;

          FArgMessage := ResponseJSON.Names[0] + ', ' + ResponseJSON.Items[0].AsString;
          Synchronize(@CallShowMessage);
        finally
          ResponseJSON.Free;
        end;
      end;
    except
      on E: Exception do
      begin
        FArgMessage := E.Message;
        Synchronize(@CallShowMessage);
      end;
    end;
  finally
    Client.RequestBody.Free;
    Client.Free;
  end;
end;

constructor TThreadPulse.Create(CreateSuspended: Boolean; const AApiUrl: String;
  const AApiToken: String; const AContentJsonStr: String);
begin
  inherited Create(CreateSuspended);
  FApiUrl         := AApiUrl;
  FApiToken       := AApiToken;
  FContentJsonStr := AContentJsonStr;

  FreeOnTerminate := True;
end;

end.

