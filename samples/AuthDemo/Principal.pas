unit Principal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
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
    Button3: TButton;
    Label7: TLabel;
    edtDomain: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
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
  System.Generics.Collections,
  System.JSON.Types,
  System.JSON.Writers;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  Auth: IFirebaseAuth;
  AResponse: IFirebaseResponse;
  JSONResp: TJSONValue;
  Obj: TJSONObject;
begin
  Auth := TFirebaseAuth.Create;
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
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  ADatabase: TFirebaseDatabase;
  AResponse: IFirebaseResponse;
  AParams: TDictionary<string, string>;
  JSONResp: TJSONValue;
begin
  ADatabase := TFirebaseDatabase.Create;
  ADatabase.SetBaseURI(edtDomain.Text);
  ADatabase.SetToken(memoToken.Text);
  memoResp.Lines.Clear;
  AParams := TDictionary<string, string>.Create;
  try
    AParams.Add('orderBy', '"$key"');
    AParams.Add('limitToLast', '2');
    AResponse := ADatabase.Get([edtNode.Text + '.json'], AParams);
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

procedure TForm1.Button3Click(Sender: TObject);
var
  ADatabase: TFirebaseDatabase;
  AResponse: IFirebaseResponse;
  JSONReq: TJSONObject;
  JSONResp: TJSONValue;
  Writer: TJsonTextWriter;
  StringWriter: TStringWriter;
begin

  StringWriter := TStringWriter.Create();
  Writer := TJsonTextWriter.Create(StringWriter);
  Writer.Formatting := TJsonFormatting.None;

  // Start
  Writer.WriteStartObject;

  // Guid
  Writer.WritePropertyName('razao');
  Writer.WriteValue('softclass software ltda');

  // Data
  Writer.WritePropertyName('data');
  Writer.WriteValue('08/02/2017');

  // Hora
  Writer.WritePropertyName('hora');
  Writer.WriteValue('11:03');

  // Produtos
  Writer.WritePropertyName('Produtos');
  Writer.WriteStartArray;

  // Produto 01
  Writer.WriteStartObject;
  Writer.WritePropertyName('codigo');
  Writer.WriteValue('A502C9DA');
  Writer.WritePropertyName('descricao');
  Writer.WriteValue('AGUA MINERAL');
  Writer.WriteEndObject;

  // Produto 02
  Writer.WriteStartObject;
  Writer.WritePropertyName('codigo');
  Writer.WriteValue('A502C9DA');
  Writer.WritePropertyName('descricao');
  Writer.WriteValue('AGUA MINERAL');
  Writer.WriteEndObject;

  Writer.WriteEndArray;

  // Pagamentos
  Writer.WritePropertyName('Pagamentos');
  Writer.WriteStartArray;

  // Dinheiro
  Writer.WriteStartObject;
  Writer.WritePropertyName('dinheiro');
  Writer.WriteValue('100.00');
  Writer.WriteEndObject;

  Writer.WriteEndArray;

  // End
  Writer.WriteEndObject;

  JSONReq := TJSONObject.ParseJSONValue(StringWriter.ToString) as TJSONObject;

  ADatabase := TFirebaseDatabase.Create;
  ADatabase.SetBaseURI(edtDomain.Text);
  ADatabase.SetToken(memoToken.Text);
  try
    AResponse := ADatabase.Post([edtNode.Text + '.json'], JSONReq);
    JSONResp := TJSONObject.ParseJSONValue(AResponse.ContentAsString);
    if (not Assigned(JSONResp)) or (not(JSONResp is TJSONObject)) then
    begin
      if Assigned(JSONResp) then
      begin
        JSONResp.Free;
      end;
      Exit;
    end;
    memoResp.Text := JSONResp.ToString;
  finally
    ADatabase.Free;
  end;

end;

end.
