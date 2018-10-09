# SQLStrings

Demonstration of how to implement a feature I had in mind

(c)2018 Paul TOTH <contact@execute.fr>

for SQL I have to write things like that:
```
 Query.SQL := 'SELECT Field1, Field2... WHERE ID = :ID';
 Query.ParamByName('ID').AsInteger := id;
 Query.Open;
 if Query.FieldByName('Field1').AsInteger = 1 then
	 ShowMessage(Query.FieldByName('Field2').AsString;
```
you can also write things like that to avoid field name lookups
```
	 Query.SQL := 'SELECT Field1, Field2... WHERE ID = :ID';
	 Query.Params[0].AsInteger := id;
	 Query.Open;
	 if Query.Fields[0].AsInteger = 1 then
		 ShowMessage(Query.Fields[1].AsString;
```
but add a field in the SQL and all the index can change

so I would like to write something like that
```
 s := 'SELECT {field1}, {field2} ... WHERE ID = {:ID}'
```
having
```
 s.field1 = 0
 s.field2 = 1
 s.ID = 0
```
the code bellow use a custom Variant to make that, but what I had in mind
is a compile time resolution of the string, not a runtime info.

note also that Firebird for instance do not use named parameters, but only question marks,
so the syntax needs some options to change the way values are computed
```
 s := 'SELECT {field1}, {field2} ... WHERE ID = {?ID}'
```