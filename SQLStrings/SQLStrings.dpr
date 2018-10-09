program SQLStrings;

(*
   Demonstration of how to implement a feature I had in mind
   (c)2018 Paul TOTH <contact@execute.fr>

   for SQL I have to write things like that:

     Query.SQL := 'SELECT Field1, Field2... WHERE ID = :ID';
     Query.ParamByName('ID').AsInteger := id;
     Query.Open;
     if Query.FieldByName('Field1').AsInteger = 1 then
       ShowMessage(Query.FieldByName('Field2').AsString;

   you can also write things like that to avoid field name lookups

     Query.SQL := 'SELECT Field1, Field2... WHERE ID = :ID';
     Query.Params[0].AsInteger := id;
     Query.Open;
     if Query.Fields[0].AsInteger = 1 then
       ShowMessage(Query.Fields[1].AsString;

   but add a field in the SQL and all the index can change

   so I would like to write something like that

     s := 'SELECT {field1}, {field2} ... WHERE ID = {:ID}'

   having

     s.field1 = 0
     s.field2 = 1
     s.ID = 0

   the code bellow use a custom Variant to make that, but what I had in mind
   is a compile time resolution of the string, not a runtime info.

   note also that Firebird for instance do not use named parameters, but only question marks,
   so the syntax needs some options to change the way values are computed

     s := 'SELECT {field1}, {field2} ... WHERE ID = {?ID}'

*)

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Variants;

type
  TSQLString = class(TInvokeableVariantType)
  private const
    VTYPE = $0113;
  private type
    TStringInfo = class
      RefCount: Integer;
      Value   : string;
      Fields  : TArray<string>;
      Params  : TArray<string>;
      constructor Create(const AValue: string);
      function AddName(Start, Stop: Integer): Boolean;
      procedure Release;
      function IndexOf(const AName: string): Integer;
    end;
  class var
    instance: TSQLString;
  public
    class procedure Init(var V: Variant; const Value: string);
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: Boolean); override;
    procedure CastTo(var Dest: TVarData; const Source: TVarData;
      const AVarType: TVarType); override;
    function GetProperty(var Dest: TVarData; const V: TVarData;
      const Name: string): Boolean; override;
  end;

function AddStrings(var Strings: TArray<string>; const AValue: string): Integer;
begin
  Result := High(Strings);
  while Result >= 0 do
  begin
    if Strings[Result] = AValue then
      Exit;
    Dec(Result);
  end;
  Result := Length(Strings);
  SetLength(Strings, Result + 1);
  Strings[Result] := AValue;
end;

function StringsIndex(const Strings: TArray<string>; const AValue: string): Integer;
begin
  Result := High(Strings);
  while Result >= 0 do
  begin
    if Strings[Result] = AValue then
      Exit;
    Dec(Result);
  end;
end;

constructor TSQLString.TStringInfo.Create(const AValue: string);
var
  Start: Integer;
  Stop : Integer;
  Index: Integer;
begin
  inherited Create;
  Value := AValue;
  Start := Pos('{', Value);
  while Start > 0 do
  begin
    Stop := Pos('}', Value, Start);
    if Stop < Start then
      Break;
    if not AddName(Start, Stop) then
      Inc(Start);
    Start := Pos('{', Value, Start);
  end;
end;

function TSQLString.TStringInfo.AddName(Start, Stop: Integer): Boolean;
var
  QMark: Boolean;
  Param: Boolean;
  Index: Integer;
  Name : string;
begin
  Index := Start + 1;
  QMark := Value[Index] = '?';
  Param := QMark or (Value[Index] = ':');
  if Param then
    Inc(Index);
  if Value[Index] in ['0'..'9'] then
    Exit(False);
  while Index < Stop do
  begin
    if not (Value[Index] in ['_', 'a'..'z', 'A'..'Z', '0'..'9']) then
      Break;
    Inc(Index);
  end;
  if Index <> Stop then
    Exit(False);
  Delete(Value, Stop, 1);
  Delete(Value, Start, 1);
  if Param then
    Inc(Start)
  else
    Dec(Stop);
  Dec(Stop, Start);
  Name := UpperCase(System.Copy(Value, Start, Stop));
  if QMark then
    Delete(Value, Start, Stop);
  if Param then
    AddStrings(Params, Name)
  else
    AddStrings(Fields, Name);
  Result := True;
end;

procedure TSQLString.TStringInfo.Release;
begin
  if Self = nil then
    Exit;
  Dec(RefCount);
  if RefCount < 0 then
    Destroy;
end;

function TSQLString.TStringInfo.IndexOf(const AName: string): Integer;
begin
  Result := -1;
  if Self = nil then
    Exit;
  Result := StringsIndex(Fields, AName);
  if Result < 0 then
    Result := StringsIndex(Params, AName);
end;

class procedure TSQLString.Init(var V: Variant; const Value: string);
begin
  if Instance = nil then
    Instance := TSQLString.Create(VTYPE);
  VarClear(V);
  TVarData(V).VType := VTYPE;
  if Value <> '' then
    TVarData(V).VPointer := TStringInfo.Create(Value);
end;

procedure TSQLString.CastTo(var Dest: TVarData; const Source: TVarData;
  const AVarType: TVarType);
begin
  if (Source.VType = VTYPE) and (AVarType = varUString) then
  begin
    if Source.VPointer = nil then
      Variant(Dest) := ''
    else
      Variant(Dest) := TStringInfo(Source.VPointer).Value;
  end else begin
    inherited;
  end;
end;

procedure TSQLString.Clear(var V: TVarData);
begin
  if V.VType = VTYPE then
    TStringInfo(V.VPointer).Release;
end;

procedure TSQLString.Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: Boolean);
begin
  Init(Variant(Dest), '');
  if Source.VType = VTYPE then
  begin
    if Source.VPointer <> nil then
    begin
      Dest.VPointer := Source.VPointer;
      if not Indirect then
      begin
        Inc(TStringInfo(Source.VPointer).RefCount);
      end;
    end;
  end;
end;

function TSQLString.GetProperty(var Dest: TVarData; const V: TVarData;
      const Name: string): Boolean;
var
  Index: Integer;
begin
  Result := V.VType = VTYPE;
  if Result then
  begin
    Variant(Dest) := TStringInfo(V.VPointer).IndexOf(Name);
  end;
end;

var
  s: string;
  v: Variant;
begin
  WriteLn('SQLStrings demo:');
  WriteLn;
  s := 'SELECT {name}, {age} FROM customers WHERE id = {:id}';
  TSQLString.Init(v, s);
  WriteLn(s);
  WriteLn(v);  // SELECT name, age FROM customers WHERE id = :id
  WriteLn('name = ', v.name); // 0
  WriteLn('age = ', v.age); // 1
  WriteLn('id = ', v.id); // 0
  WriteLn('unknow = ', v.unknow); // -1

  WriteLn;
  s := 'SELECT {name}, {age} FROM customers WHERE id = {?id}';
  TSQLString.Init(v, s);
  WriteLn(s);
  WriteLn(v);  // SELECT name, age FROM customers WHERE id = ?
  WriteLn('name = ', v.name); // 0
  WriteLn('age = ', v.age); // 1
  WriteLn('id = ', v.id); // 0
  WriteLn('unknow = ', v.unknow); // -1

  ReadLn;
end.
