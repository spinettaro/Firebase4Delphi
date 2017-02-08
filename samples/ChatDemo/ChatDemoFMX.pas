unit ChatDemoFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms,
  FMX.Dialogs, FMX.TabControl,
  FMX.StdCtrls, FMX.Gestures, FMX.ListView.Types, FMX.ListView,
  FMX.Controls.Presentation, FMX.Edit, System.Actions, FMX.ActnList,
  FMX.Layouts,
//  {$IF CompilerVersion >= 30} // For Seattle and major
//  FMX.ListView.Adapters.Base,
//  FMX.ListView.Appearances,
//  {$ENDIF}
  ChatFacade, FMX.ListView.Appearances, FMX.ListView.Adapters.Base;

type
  TMainForm = class(TForm)
    HeaderToolBar: TToolBar;
    ToolBarLabel: TLabel;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    ListView1: TListView;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Edit2: TEdit;
    Button1: TButton;
    ActionList1: TActionList;
    actStartChat: TAction;
    Layout1: TLayout;
    Edit3: TEdit;
    Button2: TButton;
    actSendMessage: TAction;
    actChatMain: TChangeTabAction;
    GridPanelLayout1: TGridPanelLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    procedure FormCreate(Sender: TObject);
    procedure actStartChatExecute(Sender: TObject);
    procedure actSendMessageExecute(Sender: TObject);
  private
    LFC: IFirebaseChatFacade;
    { Private declarations }
    procedure OnNewMessage(AChatMsg: TChatMessage);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.actSendMessageExecute(Sender: TObject);
begin
  LFC.SendMessage(Edit3.Text);
end;

procedure TMainForm.actStartChatExecute(Sender: TObject);
begin
  LFC := TFirebaseChatFacade.Create;
  LFC.SetBaseURI(Edit1.Text);
  LFC.SetUsername(Edit2.Text);
  LFC.SetOnNewMessage(OnNewMessage);
  LFC.StartListenChat;
  actChatMain.ExecuteTarget(Sender);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  { This defines the default active tab at runtime }
  TabControl1.ActiveTab := TabItem1;
end;

procedure TMainForm.OnNewMessage(AChatMsg: TChatMessage);
var
  Item: TListViewItem;
begin
  try
    ListView1.BeginUpdate;
    try
      Item := ListView1.Items.Add;
      Item.Text := AChatMsg.Msg;
      Item.Detail := AChatMsg.Username;
    finally
      ListView1.EndUpdate;
    end;

  finally
    AChatMsg.Free;
  end;
end;

end.
