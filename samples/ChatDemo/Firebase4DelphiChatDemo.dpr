program Firebase4DelphiChatDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  ChatDemoFMX in 'ChatDemoFMX.pas' {TabbedForm},
  ChatFacade in 'ChatFacade.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := true;
  Application.Initialize;
  Application.CreateForm(TTabbedForm, TabbedForm);
  Application.Run;
end.
