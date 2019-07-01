Quick unit to use Tuple under Delphi Tokyo

```
procedure ShowMyTuple(const ATuple: Variant);
begin
  ShowMessage(string(ATuple.FirstName) + ' ' + string(ATuple.LastName) + ' ' + string(ATuple.Age) + ' ' + string(ATuple.Size));
end;

var
  v: Variant;
begin
  TTuple.Init(v);
  v.LastName := 'Durand';
  v.FirstName := 'Pierre';
  v.Age := 45;
  v.Size := 1.80;
  ShowMyTuple(v);
end;
```

based on an article by Vicente Romero Zaldivar
https://community.embarcadero.com/article/technical-articles/162-programming/6064-programming-in-delphi-7-in-a-script-like-way
