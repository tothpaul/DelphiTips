unit Execute.RecordProperty;

{
   Record property concept (c)2018 Execute SARL
   http://www.execute.fr
   https://github.com/tothpaul

   the goal of this code is to avoid the creation of a sub component to store
   published components properties (like Paddings, Margin, etc) and to allow
   structured properties like Color.Focus, Color.Active etc.

}

interface

uses
  System.Classes;

type
// the structured property
  TRecordProperty = record
    Str: string;
    Int: Integer;
  end;

  TRecordPropertyObject = class;

// properties "helper"
// this record is almost empty, just a reference to the component
// all the properties are "virtual" and lead to the owner
  TRecordPropertyHelper = record
  private
    FOwner: TRecordPropertyObject;
    function GetStr: string; inline;
    procedure SetStr(Value: string); inline;
    function GetInt: Integer; inline;
    procedure SetInt(Value: Integer); inline;
  public
    property Str: string read GetStr write SetStr;
    property Int: Integer read GetInt write SetInt;
  end;

// the component
  TRecordPropertyObject = class(TComponent)
  private
  // it's properties
    FValues    : TRecordProperty;
  // properties "helper"
    FValuesProp: TRecordPropertyHelper;
  // we need to be notified when FValues is modified
    FOnChange  : TNotifyEvent;
    procedure DoChange;
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure ReadStr(Reader: TReader);
    procedure ReadInt(Reader: TReader);
    procedure WriteStr(Writer: TWriter);
    procedure WriteInt(Writer: TWriter);
    constructor Create(AOwner: TComponent); override;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  // Delphi (Tokyo) do not support published Record members, we have to add some property editors...
    property Values: TRecordPropertyHelper read FValuesProp write FValuesProp;
  end;

implementation

{ TRecordPropertyHelper }

function TRecordPropertyHelper.GetInt: Integer;
begin
  Result := FOwner.FValues.Int;
end;

function TRecordPropertyHelper.GetStr: string;
begin
  Result := FOwner.FValues.Str;
end;

procedure TRecordPropertyHelper.SetInt(Value: Integer);
begin
  if Value <> FOwner.FValues.Int then
  begin
    FOwner.FValues.Int := Value;
    FOwner.DoChange;
  end;
end;

procedure TRecordPropertyHelper.SetStr(Value: string);
begin
  if Value <> FOwner.FValues.Str then
  begin
    FOwner.FValues.Str := Value;
    FOwner.DoChange;
  end;
end;

{ TRecordPropertyObject }

constructor TRecordPropertyObject.Create(AOwner: TComponent);
begin
  inherited;
// the "helper" needs to acces the object instance
  FValuesProp.FOwner := Self;
end;

procedure TRecordPropertyObject.DefineProperties(Filer: TFiler);
begin
  inherited;
// can not use "Values.Str":
// the loader will find the "Values" published property but don't know how
// to handle a tkRecord property
  Filer.DefineProperty('FValues.Str', ReadStr, WriteStr, FValues.Str <> '');
  Filer.DefineProperty('FValues.Int', ReadInt, WriteInt, FValues.Int <> 0);
end;

procedure TRecordPropertyObject.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TRecordPropertyObject.ReadInt(Reader: TReader);
begin
  FValues.Int := Reader.ReadInteger;
end;

procedure TRecordPropertyObject.ReadStr(Reader: TReader);
begin
  FValues.Str := Reader.ReadString;
end;

procedure TRecordPropertyObject.WriteInt(Writer: TWriter);
begin
  Writer.WriteInteger(FValues.Int);
end;

procedure TRecordPropertyObject.WriteStr(Writer: TWriter);
begin
  Writer.WriteString(FValues.Str);
end;

end.
