program Firebase4DelphiAuthChatDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  ChatDemoFMX in 'ChatDemoFMX.pas' {MainForm},
  ChatFacade in 'ChatFacade.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := true;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
