{ *******************************************************************************
  Copyright 2015 Daniele Spinetti

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
  ******************************************************************************** }

unit Firebase.Client;

interface

uses Firebase.Interfaces, System.Net.HttpClient, System.JSON, System.SysUtils,
  System.Generics.Collections;

type

  TFirebaseClient = class(TInterfacedObject, IFirebaseClient)
  private
    FBaseUri: string;
    FTimeOut: integer;
    function EncodeResourceParams(AResourceParams: array of string): string;
    function EncodeQueryParams(AQueryParams
      : TDictionary<string, string>): string;
    function SendData(const AResourceParams: array of string;
      const ACommand: TFirebaseCommand; AData: TJSONValue = nil;
      AQueryParams: TDictionary < string, string >= nil;
      ADataOwner: boolean = true): IFirebaseResponse;
  public
    procedure SetBaseURI(const AbaseURI: string);
    procedure SetTimeOut(const ASeconds: integer);
    function &Set(const AParams: array of string; AData: TJSONValue = nil;
      AQueryParams: TDictionary<string, string> = nil; ADataOwner: boolean = true): IFirebaseResponse;
    function Push(const AParams: array of string; AData: TJSONValue = nil;
      AQueryParams: TDictionary<string, string> = nil; ADataOwner: boolean = true): IFirebaseResponse;
    // PATCH - Updating Data
    function Update(const AParams: array of string; AData: TJSONValue = nil;
      AQueryParams: TDictionary<string, string> = nil; ADataOwner: boolean = true): IFirebaseResponse;
    function Get(const AParams: array of string;
      AQueryParams: TDictionary<string, string> = nil): IFirebaseResponse;
    function Delete(const AParams: array of string;
      AQueryParams: TDictionary<string, string> = nil): IFirebaseResponse;
    property BaseUri: string read FBaseUri write SetBaseURI;
    property TimeOut: integer read FTimeOut write SetTimeOut;
  end;

  TFirebaseResponse = class(TInterfacedObject, IFirebaseResponse)
  private
    FHttpResponse: IHTTPResponse;
  public
    constructor Create(AHTTPResponse: IHTTPResponse);
    function ContentAsString(const AEncoding: TEncoding = nil): string;
  end;

implementation

uses
  System.Net.URLClient, System.Classes;

{ TFirebaseClient }

function TFirebaseClient.Delete(const AParams: array of string;
  AQueryParams: TDictionary<string, string> = nil): IFirebaseResponse;
begin
  Result := SendData(AParams, TFirebaseCommand.fcRemove, nil, AQueryParams);
end;

function TFirebaseClient.EncodeQueryParams(AQueryParams
  : TDictionary<string, string>): string;
var
  Param: TPair<string, string>;
begin
  if (not Assigned(AQueryParams)) or not(AQueryParams.Count > 0) then
    exit('');
  Result := '?';
  for Param in AQueryParams do
  begin
    if Result <> '?' then
      Result := Result + '&';
    Result := Result + TURI.URLDecode(Param.Key) + '=' +
      TURI.URLDecode(Param.Value)
  end;
end;

function TFirebaseClient.EncodeResourceParams(AResourceParams
  : array of string): string;
var
  i: integer;
begin
  Result := '';
  for i := low(AResourceParams) to high(AResourceParams) do
    Result := Result + '/' + TURI.URLEncode(AResourceParams[i]);
end;

function TFirebaseClient.Get(const AParams: array of string;
  AQueryParams: TDictionary<string, string> = nil): IFirebaseResponse;
begin
  Result := SendData(AParams, TFirebaseCommand.fcGet, nil, AQueryParams);
end;

function TFirebaseClient.Push(const AParams: array of string;
  AData: TJSONValue = nil; AQueryParams: TDictionary<string, string> = nil; ADataOwner: boolean = true)
  : IFirebaseResponse;
begin
  Result := SendData(AParams, TFirebaseCommand.fcPush, AData, AQueryParams, ADataOwner);
end;

function TFirebaseClient.&Set(const AParams: array of string;
  AData: TJSONValue = nil; AQueryParams: TDictionary<string, string> = nil; ADataOwner: boolean = true)
  : IFirebaseResponse;
begin
  Result := SendData(AParams, TFirebaseCommand.fcSet, AData, AQueryParams, ADataOwner);
end;

function TFirebaseClient.SendData(const AResourceParams: array of string;
  const ACommand: TFirebaseCommand; AData: TJSONValue = nil;
  AQueryParams: TDictionary<string, string> = nil; ADataOwner: boolean = true)
  : IFirebaseResponse;
var
  LClient: THTTPClient;
  LResp: IHTTPResponse;
  LURL: string;
  LSource: TStringStream;
begin
  try
    LClient := THTTPClient.Create;
    try
      LSource := nil;
      if AData <> nil then
        LSource := TStringStream.Create(AData.ToJSON);
      try
        LURL := BaseUri + EncodeResourceParams(AResourceParams) +
          EncodeQueryParams(AQueryParams);
        case ACommand of
          fcSet:
            LResp := LClient.Put(LURL, LSource);
          fcPush:
            LResp := LClient.Post(LURL, LSource);
          fcUpdate:
            LResp := LClient.Patch(LURL, LSource);
          fcGet:
            LResp := LClient.Get(LURL);
          fcRemove:
            LResp := LClient.Delete(LURL);
        end;
        Result := TFirebaseResponse.Create(LResp);
      finally
        if Assigned(LSource) then
          LSource.Free;
      end;
    finally
      LClient.Free;
    end;
  finally
    if ADataOwner then
    begin
      if Assigned(AData) then
        AData.Free;
    end;
  end;
end;

procedure TFirebaseClient.SetBaseURI(const AbaseURI: string);
begin
  FBaseUri := AbaseURI;
end;

procedure TFirebaseClient.SetTimeOut(const ASeconds: integer);
begin
  FTimeOut := ASeconds;
end;

function TFirebaseClient.Update(const AParams: array of string;
  AData: TJSONValue = nil; AQueryParams: TDictionary<string, string> = nil; ADataOwner: boolean = true)
  : IFirebaseResponse;
begin
  Result := SendData(AParams, TFirebaseCommand.fcUpdate, AData, AQueryParams, ADataOwner);
end;

{ TFirebaseResponse }

function TFirebaseResponse.ContentAsString(const AEncoding
  : TEncoding = nil): string;
begin
  Result := FHttpResponse.ContentAsString(AEncoding);
end;

constructor TFirebaseResponse.Create(AHTTPResponse: IHTTPResponse);
begin
  inherited Create;
  FHttpResponse := AHTTPResponse;
end;

end.
