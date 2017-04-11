unit RemoteConfigFacade;

interface

uses Firebase.Interfaces, System.SysUtils,
  Generics.Collections, System.Threading, System.JSON, Firebase.Database;

const
  CONFIG_FILENAME = 'config.json';

type

  TSettings = class(TObject)
  private
    FStyleFilename: string;
    FWelcomeMessage: string;
    procedure SetStyleFilename(const Value: string);
    procedure SetWelcomeMessage(const Value: string);
  public
    property StyleFilename: string read FStyleFilename write SetStyleFilename;
    property WelcomeMessage: string read FWelcomeMessage
      write SetWelcomeMessage;
  end;

  TConfigurationHandler = class(TObject)
  public
    class function LoadFromFile: TSettings;
    class procedure SaveToFile(Settings: TSettings); overload;
    class procedure SaveToFile(const ASettingsJSONStr: string); overload;
  end;

  TRemoteConfigFacade = class(TObject)
  public
    class procedure CheckConfiguration(const aBaseURI: string);
  end;

function GetOSString: string;
function GetConfigFilePath: string;

procedure ApplyStyle(AStyleFilename: string);
procedure ApplySettings;

implementation

uses
  System.Classes, FMX.Styles, IOUtils, REST.JSON;

function GetOSString: string;
begin
  case TOSVersion.Platform of
    TOSVersion.TPlatform.pfWindows:
      exit('windows');
    TOSVersion.TPlatform.pfAndroid:
      exit('android');
  end;
end;

function GetConfigFilePath: string;
begin
  case TOSVersion.Platform of
    TOSVersion.TPlatform.pfWindows:
      exit(CONFIG_FILENAME);
    TOSVersion.TPlatform.pfAndroid:
      exit(TPath.Combine(TPath.GetDocumentsPath, CONFIG_FILENAME));
  end;
end;

procedure ApplySettings;
var
  LSettings: TSettings;
begin
  LSettings := TConfigurationHandler.LoadFromFile;
  try
    if not LSettings.StyleFilename.IsEmpty then
      ApplyStyle(LSettings.StyleFilename);
  finally
    LSettings.Free;
  end;
end;

{ TRemoteConfigFacade }

class procedure TRemoteConfigFacade.CheckConfiguration(const aBaseURI: string);
begin
  TTask.Run(
    procedure
    var
      FFC: IFirebaseDatabase;
      Resp: IFirebaseResponse;
      JSONResp: TJSONValue;
    begin
      FFC := TFirebaseDatabase.Create;
      FFC.SetBaseURI(aBaseURI);
      Resp := FFC.Get([GetOSString, '.json']);
      JSONResp := TJSONObject.ParseJSONValue(Resp.ContentAsString);
      try
        if (not Assigned(JSONResp)) or (not(JSONResp is TJSONObject)) then
          exit;
        TConfigurationHandler.SaveToFile(Resp.ContentAsString());
        TThread.Queue(nil,
          procedure
          begin
            ApplySettings;
          end);
      finally
        JSONResp.Free;
      end;
    end);
end;

procedure ApplyStyle(AStyleFilename: string);
begin
  case TOSVersion.Platform of
    TOSVersion.TPlatform.pfAndroid:
      AStyleFilename := TPath.Combine(TPath.GetDocumentsPath, AStyleFilename);
  end;
  TStyleManager.SetStyleFromFile(AStyleFilename);
end;

{ TSettings }

procedure TSettings.SetStyleFilename(const Value: string);
begin
  FStyleFilename := Value;
end;

procedure TSettings.SetWelcomeMessage(const Value: string);
begin
  FWelcomeMessage := Value;
end;

{ TConfigurationHandler }

class function TConfigurationHandler.LoadFromFile: TSettings;
begin
  if FileExists(GetConfigFilePath) then
  begin
    Result := TJSON.JsonToObject<TSettings>
      (TFile.ReadAllText(GetConfigFilePath));
  end
  else
    Result := TSettings.Create();
end;

class procedure TConfigurationHandler.SaveToFile(Settings: TSettings);
begin
  TConfigurationHandler.SaveToFile(TJSON.ObjectToJsonString(Settings));
end;

class procedure TConfigurationHandler.SaveToFile(const ASettingsJSONStr
  : string);
begin
  TFile.WriteAllText(GetConfigFilePath, ASettingsJSONStr);
end;

end.
