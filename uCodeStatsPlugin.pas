{
    This file is part of the Code::Stats Package

    Package main unit
    Copyright (c) 2021 by Maico Smaniotto maicosmaniotto@yahoo.com.br

    See the file LICENSE.txt, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit uCodeStatsPlugin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, ComCtrls, Dialogs, SrcEditorIntf, MenuIntf, LazIDEIntf, IDEMsgIntf, IDEExternToolIntf, BaseIDEIntf, LazConfigStorage,
  SynEdit, Generics.Collections, fpjson, fphttpclient, opensslsockets, uThreadPulse;

type
  TEditorChangeRec = record
    Editor: TSynEdit;
    ChangeEvent: TNotifyEvent;
  end;

  TCodeStatsPlugin = class
  private
    FThreadPulse: TThreadPulse;

    FEnabled: Boolean;
    FShowMessages: Boolean;
    FApiUrl: String;
    FApiToken: String;
    FEditorChangeList: array of TEditorChangeRec;
    FXPList: specialize TDictionary<String, Integer>;
    FTimerPulse: TTimer;
    FTimerInit: TTimer;
    FStatusBar: TStatusBar;
    procedure AddXP(const ALanguage: String; const AXP: Integer);
    procedure SendPulse;
    procedure ResetTimer;
    procedure LoadConfig;
    procedure SaveConfig;
    procedure ShowCodeStatsMessage(const AMessage: String);

    procedure OnSrcEditActivate(Sender: TObject);
    procedure OnSrcEditStatusChange(Sender: TObject);
    procedure OnSrcEditChange(Sender: TObject);

    procedure UpdateStatusBarXP(AValue: Integer);
    procedure ShowConfig(Sender: TObject);
    procedure SendCodeStatsData(Sender: TObject);
    procedure TimerPulseTimer(Sender: TObject);
    procedure InitCodeStats(Sender: TObject);

    procedure ThreadPulseTerminate(Sender: TObject);

    function GetLanguageName(const AFileName: String): String;
    function GetXP(const ALanguage: String): Integer;
  public
    constructor Create; reintroduce; overload;
    destructor Destroy; override;
  end;

const
  CONFIG_FILENAME   = 'codestats.xml';
  API_URL_DEF       = 'https://codestats.net/api/my/pulses';
  ENABLED_DEF       = False;
  SHOW_MESSAGES_DEF = True;

var
  CodeStatsPlugin: TCodeStatsPlugin;

procedure Register;

implementation

uses FConfig;

procedure Register;
begin
  RegisterIDEMenuCommand(itmSecondaryTools, 'Code::Stats', 'Configure Code::Stats', @CodeStatsPlugin.ShowConfig);

  SourceEditorManagerIntf.RegisterChangeEvent(semEditorActivate, @CodeStatsPlugin.OnSrcEditActivate);
  SourceEditorManagerIntf.RegisterChangeEvent(semEditorStatus, @CodeStatsPlugin.OnSrcEditStatusChange);

  LazarusIDE.AddHandlerOnIDEClose(@CodeStatsPlugin.SendCodeStatsData);
  //LazarusIDE.AddHandlerOnSaveEditorFile(@CodeStatsPlugin.SendCodeStatsData);
end;

{ TCodeStatsPlugin }

function TCodeStatsPlugin.GetXP(const ALanguage: String): Integer;
var
  Value: Integer;
begin
  Value := 0;
  if FXPList.ContainsKey(ALanguage) then
    FXPList.TryGetValue(ALanguage, Value);

  Result := Value;
end;

procedure TCodeStatsPlugin.AddXP(const ALanguage: String; const AXP: Integer);
var
  Value: Integer;
begin
  Value := GetXP(ALanguage) + AXP;
  FXPList.AddOrSetValue(ALanguage, Value);

  UpdateStatusBarXP(Value);
  ResetTimer;
end;

procedure TCodeStatsPlugin.SendPulse;
var
  Key          : String;
  TimeOffset   : Integer;
  TimeStamp    : String;
  ContentJSON  : TJSONObject;
  XPs          : TJSONArray;
  Language     : TJSONObject;
begin
  if not FEnabled then Exit;

  if FXPList = nil then Exit;

  if FThreadPulse <> nil then Exit; // Thread pulse is running

  XPs := TJSONArray.Create;

  for Key in FXPList.Keys do
  begin
    if FXPList.Items[Key] = 0 then
      Continue;

    Language := TJSONObject.Create;
    Language.Add('language', Key);          // add language name
    Language.Add('xp', FXPList.Items[Key]); // add xp value

    XPs.Add(Language);
  end;

  if XPs.Count = 0 then
  begin
    XPs.Free;
    Exit;
  end;

  TimeOffset := -GetLocalTimeOffset;
  TimeStamp  := FormatDateTime('yyyy-MM-dd''T''HH:mm:ss', Now) + specialize IfThen<String>(TimeOffset < 0, '-', '+') +
                Format('%.2d', [Abs(TimeOffset) div 60]) + ':' + Format('%.2d' , [Abs(TimeOffset) mod 60]);

  ContentJSON := TJSONObject.Create;
  try
    ContentJSON.Add('coded_at', TimeStamp);
    ContentJSON.Add('xps', XPs);

    FThreadPulse                     := TThreadPulse.Create(True, FApiUrl, FApiToken, ContentJSON.AsJSON);
    FThreadPulse.AddXPCallback       := @AddXP;
    FThreadPulse.ShowMessageCallback := @ShowCodeStatsMessage;
    FThreadPulse.OnTerminate         := @ThreadPulseTerminate;
    FThreadPulse.Start;
  finally
    ContentJSON.Free;
  end;
end;

procedure TCodeStatsPlugin.ResetTimer;
begin
  FTimerPulse.Enabled := False;
  FTimerPulse.Enabled := True;
end;

procedure TCodeStatsPlugin.LoadConfig;
var
  Config: TConfigStorage;
begin
  Config := GetIDEConfigStorage(CONFIG_FILENAME, True);
  try
    FApiToken     := Config.GetValue('ApiToken/Value', '');
    FApiUrl       := Config.GetValue('ApiUrl/Value', API_URL_DEF);
    FShowMessages := Config.GetValue('ShowMessages/Value', SHOW_MESSAGES_DEF);
    FEnabled      := Config.GetValue('Enabled/Value', ENABLED_DEF);
  finally
    Config.Free;
  end;
end;

procedure TCodeStatsPlugin.SaveConfig;
const
  Version = 1;
var
  Config: TConfigStorage;
begin
  Config := GetIDEConfigStorage(CONFIG_FILENAME, False);
  try
    Config.SetDeleteValue('Version/Value', Version, 0);
    Config.SetDeleteValue('ApiToken/Value', FApiToken, '');
    Config.SetDeleteValue('ApiUrl/Value', FApiUrl, API_URL_DEF);
    Config.SetDeleteValue('ShowMessages/Value', FShowMessages, SHOW_MESSAGES_DEF);
    Config.SetDeleteValue('Enabled/Value', FEnabled, ENABLED_DEF);
  finally
    Config.Free;
  end;
end;

procedure TCodeStatsPlugin.ShowCodeStatsMessage(const AMessage: String);
begin
  if FShowMessages then
    IDEMessagesWindow.AddCustomMessage(mluImportant, 'Code::Stats - ' + AMessage);
end;

procedure TCodeStatsPlugin.OnSrcEditActivate(Sender: TObject);
var
  I: Integer;
  Found : Boolean;
  Editor: TSynEdit;
  Component: TComponent;
begin
  FStatusBar := nil;

  if not FEnabled then Exit; // Code::Stats is disabled

  if SourceEditorManagerIntf.ActiveEditor = nil then Exit; // No editor active

  if SourceEditorManagerIntf.ActiveEditor.EditorControl = nil then Exit; // No editor loaded

  // Get current editor
  Editor := TSynEdit(SourceEditorManagerIntf.ActiveEditor.EditorControl);

  // Checks if editor already has a custom change event
  Found := False;
  for I := 0 to Length(FEditorChangeList) - 1 do
  begin
    if Editor = FEditorChangeList[I].Editor then
    begin
      Found := True;
      Break;
    end;
  end;
  if not Found then
  begin
    // Editor doesn't have a custom change event yet
    // Saves the current editor change event and assigns a new custom change event
    SetLength(FEditorChangeList, Length(FEditorChangeList) + 1);
    FEditorChangeList[High(FEditorChangeList)].Editor      := Editor;
    FEditorChangeList[High(FEditorChangeList)].ChangeEvent := Editor.OnChange;
    Editor.OnChange := @OnSrcEditChange;
  end;

  // Get IDE StatusBar
  Component := Editor;
  repeat
    Component := Component.GetParentComponent;
    if Component <> nil then
    begin
      if Component is TSourceEditorWindowInterface then
      begin
        Component := Component.FindComponent('StatusBar');
        if Component <> nil then
          FStatusBar := TStatusBar(Component);
        Break;
      end;
    end;
  until Component = nil;

  UpdateStatusBarXP(GetXP(GetLanguageName(SourceEditorManagerIntf.ActiveEditor.FileName)));
end;

procedure TCodeStatsPlugin.OnSrcEditStatusChange(Sender: TObject);
begin
  UpdateStatusBarXP(GetXP(GetLanguageName(SourceEditorManagerIntf.ActiveEditor.FileName)));
end;

procedure TCodeStatsPlugin.OnSrcEditChange(Sender: TObject);
var
  Editor: TSynEdit;
  I: Integer;
begin
  Editor := TSynEdit(Sender);

  for I := 0 to Length(FEditorChangeList) - 1 do
  begin
    if Editor = FEditorChangeList[I].Editor then
    begin
      // Runs the original editor change event
      if FEditorChangeList[I].ChangeEvent <> nil then
        FEditorChangeList[I].ChangeEvent(Sender);

      Break;
    end;
  end;

  // Runs new code to update XP
  if FEnabled then
    AddXP(GetLanguageName(SourceEditorManagerIntf.ActiveEditor.FileName), 1);
end;

procedure TCodeStatsPlugin.UpdateStatusBarXP(AValue: Integer);
const
  CS_STR = '        ✎ C::S ';
var
  Content: String;
  CSPos: Integer;
  StatusPanel: TStatusPanel;
begin
  if not FEnabled then Exit;

  if FStatusBar = nil then Exit;

  StatusPanel := FStatusBar.Panels[FStatusBar.Panels.Count - 1];

  CSPos := Pos(CS_STR, StatusPanel.Text);
  if CSPos > 0 then
    Content := Copy(StatusPanel.Text, 1, CSPos - 1)
  else
    Content := StatusPanel.Text;

  if AValue >= 0 then
    StatusPanel.Text := Content + CS_STR + IntToStr(AValue)
  else
    StatusPanel.Text := Content;
end;

procedure TCodeStatsPlugin.ShowConfig(Sender: TObject);
var
  FormConfig: TFormConfig;
begin
  FormConfig := TFormConfig.Create(nil);
  try
    FormConfig.CkEnable.Checked       := FEnabled;
    FormConfig.EdApiToken.Text        := FApiToken;
    FormConfig.EdApiUrl.Text          := FApiUrl;
    FormConfig.CkShowMessages.Checked := FShowMessages;

    if FormConfig.ShowModal = mrOk then
    begin
      FEnabled      := FormConfig.CkEnable.Checked;
      FApiToken     := FormConfig.EdApiToken.Text;
      FApiUrl       := FormConfig.EdApiUrl.Text;
      FShowMessages := FormConfig.CkShowMessages.Checked;

      SaveConfig;

      // Force EditActivate routine to locate current Editor and StatusBar
      OnSrcEditActivate(nil);
      FTimerPulse.Enabled := FEnabled;
    end;
  finally
    FormConfig.Free;
  end;
end;

procedure TCodeStatsPlugin.SendCodeStatsData(Sender: TObject);
begin
  SendPulse;
end;

procedure TCodeStatsPlugin.TimerPulseTimer(Sender: TObject);
begin
  SendPulse;
end;

procedure TCodeStatsPlugin.InitCodeStats(Sender: TObject);
begin
  // Waint until IDE starts
  if LazarusIDE <> nil then
  begin
    FTimerInit.Enabled := False;

    // Load configuration and start code::stats
    LoadConfig;
    FTimerPulse.Enabled := FEnabled;

    OnSrcEditActivate(nil);
  end;
end;

procedure TCodeStatsPlugin.ThreadPulseTerminate(Sender: TObject);
begin
  FThreadPulse := nil;
end;

function TCodeStatsPlugin.GetLanguageName(const AFileName: String): String;
var
  Ext: String;
begin
  Ext := LowerCase(ExtractFileExt(AFileName));
  Ext := Copy(Ext, 2, Length(Ext) - 1);

  if (Ext = 'pas') or (Ext = 'pp') or (Ext = 'inc') or (Ext = 'lpr') or (Ext = 'lrs') or (Ext = 'dpr') or (Ext = 'dpk') or (Ext = 'fpd') then
    Result := 'Object Pascal'
  else if (Ext = 'lfm') or (Ext = 'dfm') or (Ext = 'xfm') then
    Result := 'Lazarus Form Definition'
  else if (Ext = 'xml') or (Ext = 'xsd') or (Ext = 'xsl') or (Ext = 'xslt') or (Ext = 'dtd') or
          (Ext = 'lpi') or (Ext = 'lps') or (Ext = 'lpk') or (Ext = 'wsdl') or (Ext = 'svg') then
    Result := 'XML'
  else if Ext = 'ini' then
    Result := 'INI File'
  else if Ext = 'json' then
    Result := 'JSON'
  else if Ext = 'sql' then
    Result := 'SQL'
  else if Ext = 'asm' then
    Result := 'Assembly'
  else if (Ext = 'c') or (Ext = 'h') then
    Result := 'C'
  else if (Ext = 'cpp') or (Ext = 'hpp') or (Ext = 'cc') or (Ext = 'hh') then
    Result := 'C++'
  else if Ext = 'cs' then
    Result := 'C#'
  else if Ext = 'bat' then
    Result := 'MS-DOS Batch Language'
  else if Ext = 'sh' then
    Result := 'UNIX Shell Script'
  else if Ext = 'diff' then
    Result := 'Diff File'
  else if (Ext = 'htm') or (Ext = 'html') or (Ext = 'xhtml') then
    Result := 'HTML'
  else if Ext = 'css' then
    Result := 'CSS'
  else if Ext = 'js' then
    Result := 'Javascript'
  else if (Ext = 'py') or (Ext = 'pyw') then
    Result := 'Python'
  else if (Ext = 'pl') or (Ext = 'pm') or (Ext = 'cgi') then
    Result := 'Perl'
  else if (Ext = 'php') or (Ext = 'php3') or (Ext = 'php4') then
    Result := 'PHP'
  else if Ext = 'po' then
    Result := 'PO Language File'
  else if (Ext = 'pike') or (Ext = 'pmod') then
    Result := 'Pike'
  else if Ext = 'java' then
    Result := 'Java'
  else if Ext = 'txt' then
    Result := 'Text File'
  else if Ext <> '' then
    Result := Uppercase(Ext) + ' File'
  else
    Result := '';
end;

constructor TCodeStatsPlugin.Create;
begin
  inherited Create;

  FThreadPulse := nil;

  FStatusBar := nil;
  SetLength(FEditorChangeList, 0);
  FXPList := specialize TDictionary<String, Integer>.Create;

  FTimerPulse          := TTimer.Create(nil);
  FTimerPulse.Enabled  := False;
  FTimerPulse.Interval := 10000;
  FTimerPulse.OnTimer  := @TimerPulseTimer;

  // Use a timer to wait until IDE starts
  FTimerInit          := TTimer.Create(nil);
  FTimerInit.Enabled  := False;
  FTimerInit.Interval := 500;
  FTimerInit.OnTimer  := @InitCodeStats;
  FTimerInit.Enabled  := True;
end;

destructor TCodeStatsPlugin.Destroy;
begin
  FTimerPulse.Enabled := False;
  FTimerPulse.Free;
  FTimerInit.Free;

  FEditorChangeList := nil;
  FreeAndNil(FXPList);

  if FThreadPulse <> nil then
    FThreadPulse.Terminate;

  inherited Destroy;
end;

initialization
  CodeStatsPlugin := TCodeStatsPlugin.Create;

finalization
  CodeStatsPlugin.Free;

end.

