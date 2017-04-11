program RemoteConfigClient;

uses
  System.SysUtils,
  System.StartUpCopy,
  FMX.Forms,
  MainFMX in 'MainFMX.pas' {RemoteConfigClientFrm} ,
  RemoteConfigFacade in 'RemoteConfigFacade.pas',
  FMX.Styles;

{$R *.res}

begin

  ReportMemoryLeaksOnShutdown := true;

  // check the Remote Configuration before start
  TRemoteConfigFacade.CheckConfiguration
    ('https://fiery-torch-9659.firebaseio.com/remoteconfigdemo');

  Application.Initialize;
  Application.CreateForm(TRemoteConfigClientFrm, RemoteConfigClientFrm);
  Application.Run;

end.
