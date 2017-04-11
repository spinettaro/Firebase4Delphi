unit SplashFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects;

type
  TSplashForm = class(TForm)
    Text1: TText;
    SplashTimer: TTimer;
    procedure SplashTimerTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FSplashTime: integer;
    procedure SetSplashTime(const Value: integer);
    { Private declarations }
  public
    { Public declarations }
    property SplashTime: integer read FSplashTime write SetSplashTime;

  end;

var
  SplashForm: TSplashForm;

implementation

{$R *.fmx}

procedure TSplashForm.FormShow(Sender: TObject);
begin
   SplashTimer.Enabled := true;
   SplashTimer.Interval := SplashTime;
end;

procedure TSplashForm.SetSplashTime(const Value: integer);
begin
  FSplashTime := Value;
end;

procedure TSplashForm.SplashTimerTimer(Sender: TObject);
begin
  SplashTimer.Enabled := false;
  Close;
end;

end.
