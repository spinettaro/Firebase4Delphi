unit MainFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, FMX.Layouts;

type
  TRemoteConfigClientFrm = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    GridPanelLayout1: TGridPanelLayout;
    Edit1: TEdit;
    Timer1: TTimer;
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  RemoteConfigClientFrm: TRemoteConfigClientFrm;

implementation

uses
  FMX.Styles, RemoteConfigFacade;

{$R *.fmx}

procedure TRemoteConfigClientFrm.FormShow(Sender: TObject);
begin
  // spinettaro - workaround, in according to documentation
  // it should be on initialization section,
  // but doesn't work http://docwiki.embarcadero.com/RADStudio/Seattle/en/Customizing_FireMonkey_Applications_with_Styles (FormStyle chapter)
  ApplySettings;
end;

procedure TRemoteConfigClientFrm.Timer1Timer(Sender: TObject);
begin
  // check the Remote Configuration before start
  TRemoteConfigFacade.CheckConfiguration
    ('https://fiery-torch-9659.firebaseio.com/remoteconfigdemo');
end;

end.
