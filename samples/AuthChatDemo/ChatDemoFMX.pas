unit ChatDemoFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms,
  FMX.Dialogs, FMX.TabControl,
  FMX.StdCtrls, FMX.Gestures, FMX.ListView.Types, FMX.ListView,
  FMX.Controls.Presentation, FMX.Edit, System.Actions, FMX.ActnList,
  FMX.Layouts,
  // {$IF CompilerVersion >= 30} // For Seattle and major
  // FMX.ListView.Adapters.Base,
  // FMX.ListView.Appearances,
  // {$ENDIF}
  ChatFacade, FMX.ListView.Appearances, FMX.ListView.Adapters.Base;

const
  WEB_API_KEY = 'AIzaSyCuTQHMv8bj2_PXKOtQh0HrDcj-54Hd30E';

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
    Layout4: TLayout;
    Label3: TLabel;
    Edit4: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure actStartChatExecute(Sender: TObject);
    procedure actSendMessageExecute(Sender: TObject);
  private
    LFC: IFirebaseChatFacade;
    FToken: string;
    { Private declarations }
    procedure OnNewMessage(AChatMsg: TChatMessage);
    function DoLogin: Boolean;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  System.JSON, Firebase.Interfaces,
  Firebase.Auth,
  Firebase.Database;

{$R *.fmx}

procedure TMainForm.actSendMessageExecute(Sender: TObject);
begin
  LFC.SendMessage(Edit3.Text);
end;

procedure TMainForm.actStartChatExecute(Sender: TObject);
begin
  if not DoLogin then
  begin
    ShowMessage('Login failed');
    exit;
  end;
  LFC := TFirebaseChatFacade.Create;
  LFC.SetBaseURI(Edit1.Text);
  LFC.SetToken(FToken);
  LFC.SetUsername(Edit2.Text);
  LFC.SetOnNewMessage(OnNewMessage);
  LFC.StartListenChat;
  actChatMain.ExecuteTarget(Sender);
end;

function TMainForm.DoLogin: Boolean;
var
  Auth: IFirebaseAuth;
  AResponse: IFirebaseResponse;
  JSONResp: TJSONValue;
  Obj: TJSONObject;
begin
  Auth := TFirebaseAuth.Create;
  Auth.SetApiKey(WEB_API_KEY);
  AResponse := Auth.SignInWithEmailAndPassword(Edit2.Text, Edit4.Text);
  JSONResp := TJSONObject.ParseJSONValue(AResponse.ContentAsString);
  try
    if (not Assigned(JSONResp)) or (not(JSONResp is TJSONObject)) then
      exit(false);
    Obj := JSONResp as TJSONObject;
    FToken := Obj.Values['idToken'].Value;
    Result := true;
  finally
    JSONResp.Free;
  end;
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
