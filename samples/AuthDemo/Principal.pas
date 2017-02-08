unit Principal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    edtEmail: TEdit;
    edtPassword: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    memoToken: TMemo;
    Button1: TButton;
    Label3: TLabel;
    Button2: TButton;
    memoResp: TMemo;
    Label4: TLabel;
    Label5: TLabel;
    edtNode: TEdit;
    Label6: TLabel;
    edtKey: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  Firebase.Interfaces,
  Firebase.Auth,
  Firebase.Database,
  System.JSON,
  System.Net.HttpClient,
  System.Generics.Collections;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  Auth: TFirebaseAuth;
  AResponse: IFirebaseResponse;
  JSONResp: TJSONValue;
  Obj: TJSONObject;
  I: Integer;
  Key: string;
begin
  Auth := TFirebaseAuth.Create;
  try
    Auth.SetApiKey(edtKey.Text);
    AResponse := Auth.SignInWithEmailAndPassword(edtEmail.Text, edtPassword.Text);
    JSONResp := TJSONObject.ParseJSONValue(AResponse.ContentAsString);
    if (not Assigned(JSONResp)) or (not(JSONResp is TJSONObject)) then
    begin
      if Assigned(JSONResp) then
      begin
        JSONResp.Free;
      end;
      Exit;
    end;
    Obj := JSONResp as TJSONObject;
    Obj.Values['idToken'].Value;
    memoToken.Lines.Add(Obj.Values['idToken'].Value);
  finally
    Auth.Free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  ADatabase: TFirebaseDatabase;
  AResponse: IFirebaseResponse;
  AParams: TDictionary<string, string>;
  JSONResp: TJSONValue;
begin
  ADatabase := TFirebaseDatabase.Create;
  ADatabase.SetBaseURI('https://cloudclass-156620.firebaseio.com');
  ADatabase.SetToken(memoToken.Text);
  memoResp.Lines.Clear;
  AParams := TDictionary<string, string>.Create;
  try
    AParams.Add('orderBy', '"$key"');
    AParams.Add('limitToLast', '2');
    AResponse := ADatabase.Get([edtNode.Text+'.json'], AParams);
    JSONResp := TJSONObject.ParseJSONValue(AResponse.ContentAsString);
    if (not Assigned(JSONResp)) or (not(JSONResp is TJSONObject)) then
    begin
      if Assigned(JSONResp) then
      begin
        JSONResp.Free;
      end;
      Exit;
    end;
    memoResp.Lines.Add(JSONResp.ToString);
  finally
    AParams.Free;
    ADatabase.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

  //edtEmail.Text := '';
  //edtPassword.Text := '';
  memoResp.Lines.Clear;

end;

end.
