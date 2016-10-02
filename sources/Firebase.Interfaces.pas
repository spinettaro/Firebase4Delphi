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

unit Firebase.Interfaces;

interface

uses System.JSON, System.SysUtils, System.Generics.Collections;

type

  TFirebaseCommand = (fcPut, fcPatch, fcPost, fcGet, fcRemove);

  IFirebaseResponse = interface(IInterface)
    ['{28CE1C37-DE9E-47C2-8764-2FB073B93FB8}']
    function ContentAsString(const AEncoding: TEncoding = nil): string;
  end;

  IFirebaseClient = interface(IInterface)
    ['{43135DC9-C04F-42A3-AB5B-3E15AE207322}']
    procedure SetBaseURI(const AbaseURI: string);
    procedure SetTimeOut(const ASeconds: Integer);
    // It's a put
    function Put(const AParams: array of string; AData: TJSONValue = nil;
      AQueryParams: TDictionary<string, string> = nil;
      ADataOwner: boolean = true): IFirebaseResponse;
    // It's a post
    function Post(const AParams: array of string; AData: TJSONValue = nil;
      AQueryParams: TDictionary<string, string> = nil;
      ADataOwner: boolean = true): IFirebaseResponse;
    // PATCH - Updating Data
    function Patch(const AParams: array of string; AData: TJSONValue = nil;
      AQueryParams: TDictionary<string, string> = nil;
      ADataOwner: boolean = true): IFirebaseResponse;
    function Get(const AParams: array of string;
      AQueryParams: TDictionary<string, string> = nil): IFirebaseResponse;
    function Delete(const AParams: array of string;
      AQueryParams: TDictionary<string, string> = nil): IFirebaseResponse;
  end;

implementation

end.
