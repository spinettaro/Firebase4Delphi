object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Firebase Authentication Demo'
  ClientHeight = 505
  ClientWidth = 472
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 9
    Top = 7
    Width = 24
    Height = 13
    Caption = 'Email'
  end
  object Label2: TLabel
    Left = 9
    Top = 47
    Width = 46
    Height = 13
    Caption = 'Password'
  end
  object Label3: TLabel
    Left = 9
    Top = 219
    Width = 29
    Height = 13
    Caption = 'Token'
  end
  object Label4: TLabel
    Left = 8
    Top = 349
    Width = 47
    Height = 13
    Caption = 'Response'
  end
  object Label5: TLabel
    Left = 8
    Top = 87
    Width = 25
    Height = 13
    Caption = 'Node'
  end
  object Label6: TLabel
    Left = 8
    Top = 131
    Width = 62
    Height = 13
    Caption = 'Firebase Key'
  end
  object Label7: TLabel
    Left = 8
    Top = 173
    Width = 79
    Height = 13
    Caption = 'Firebase Domain'
  end
  object edtEmail: TEdit
    Left = 9
    Top = 23
    Width = 289
    Height = 21
    TabOrder = 1
    Text = 'denernun@gmail.com'
  end
  object edtPassword: TEdit
    Left = 9
    Top = 63
    Width = 289
    Height = 21
    TabOrder = 3
    Text = '030372'
  end
  object memoToken: TMemo
    Left = 9
    Top = 235
    Width = 454
    Height = 111
    TabOrder = 8
  end
  object Button1: TButton
    Left = 320
    Top = 21
    Width = 143
    Height = 25
    Caption = 'SignIn'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 320
    Top = 61
    Width = 143
    Height = 25
    Caption = 'Get'
    TabOrder = 2
    OnClick = Button2Click
  end
  object memoResp: TMemo
    Left = 8
    Top = 365
    Width = 454
    Height = 132
    Lines.Strings = (
      '')
    TabOrder = 9
  end
  object edtNode: TEdit
    Left = 8
    Top = 103
    Width = 289
    Height = 21
    TabOrder = 5
    Text = '/queue/output'
  end
  object edtKey: TEdit
    Left = 8
    Top = 147
    Width = 289
    Height = 21
    TabOrder = 6
    Text = 'AIzaSyBPQVbvyNJkyKbc4kr8ig8xr57EraW9pNQ'
  end
  object Button3: TButton
    Left = 320
    Top = 101
    Width = 143
    Height = 25
    Caption = 'Post'
    TabOrder = 4
    OnClick = Button3Click
  end
  object edtDomain: TEdit
    Left = 8
    Top = 189
    Width = 289
    Height = 21
    TabOrder = 7
    Text = 'https://cloudclass-156620.firebaseio.com'
  end
end
