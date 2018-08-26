unit Execute.RecordProperty.Register;

interface

uses
  DesignIntf, // need a reference to "C:\Program Files (x86)\Embarcadero\Studio\19.0\lib\win32\release\designide.dcp" !
  DesignEditors,
  System.Classes,
  System.TypInfo,
  Execute.RecordProperty;

type
// Delphi do not support Record properties, let's define a proxy class
  TRecordPropertyProxy = class(TPersistent)
  private
    FRecord: ^TRecordPropertyHelper;
    function GetStr: string;
    procedure SetStr(const Value: string);
    function GetInt: Integer;
    procedure SetInt(Value: Integer);
  published
    property Str: string read GetStr write SetStr;
    property Int: Integer read GetInt write SetInt;
  end;

// Delphi do not support Record properties, let's define a property Editor for TRecordProperty(Helper)
  TRecordPropertyEditor = class(TPropertyEditor)
  private
    FProxy: TRecordPropertyProxy;
  protected
    procedure SetPropEntry(Index: Integer; AInstance: TPersistent;
      APropInfo: PPropInfo); override;
  public
    constructor Create(const ADesigner: IDesigner; APropCount: Integer); override;
    destructor Destroy; override;
    function GetValue: string; override;
    function GetAttributes: TPropertyAttributes; override;
    procedure GetProperties(Proc: TGetPropProc); override;
  end;

procedure Register;

implementation

procedure Register;
begin
// First, we need to register the component (Values record property is not visible yet)
  RegisterComponents('Execute', [TRecordPropertyObject]);
// Delphi do not support Record properties, let's define a property Editor for TRecordProperty(Helper)
  RegisterPropertyEditor(TypeInfo(TRecordPropertyHelper), nil, '', TRecordPropertyEditor);
end;

{ TRecordPropertyEditor }

constructor TRecordPropertyEditor.Create(const ADesigner: IDesigner;
  APropCount: Integer);
begin
  inherited;
  FProxy := TRecordPropertyProxy.Create;
end;

destructor TRecordPropertyEditor.Destroy;
begin
  FProxy.Free;
  inherited;
end;

function TRecordPropertyEditor.GetValue: string;
begin
  Result := '(TRecordProperty)';
end;

procedure TRecordPropertyEditor.SetPropEntry(Index: Integer;
  AInstance: TPersistent; APropInfo: PPropInfo);
begin
  inherited;
  FProxy.FRecord := Pointer(PByte(AInstance) + Cardinal(APropInfo^.GetProc) and $FFFFFF);
end;

function TRecordPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [
//    paValueList,
    paSubProperties,   // -> GetProperties
//    paDialog,
//    paMultiSelect,
//    paAutoUpdate,
//    paSortList,
    paReadOnly,
//    paRevertable,
//    paFullWidthName,
//    paVolatileSubProperties,
    paVCL,
//    paNotNestable,
    paDisplayReadOnly
//    paCustomDropDown,
//    paValueEditable
  ];
end;

procedure TRecordPropertyEditor.GetProperties(Proc: TGetPropProc);
var
  Components: IDesignerSelections;
begin
  Components := TDesignerSelections.Create;
  Components.Add(FProxy);
  GetComponentProperties(Components, tkProperties, Designer, Proc);
end;



{ TRecordPropertyProxy }

function TRecordPropertyProxy.GetInt: Integer;
begin
  Result := FRecord.Int;
end;

function TRecordPropertyProxy.GetStr: string;
begin
  Result := FRecord.Str;
end;

procedure TRecordPropertyProxy.SetInt(Value: Integer);
begin
  FRecord.Int := Value;
end;

procedure TRecordPropertyProxy.SetStr(const Value: string);
begin
  FRecord.Str := Value;
end;

end.
