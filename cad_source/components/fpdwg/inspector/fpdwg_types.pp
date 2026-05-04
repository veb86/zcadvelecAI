unit fpdwg_types;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  SysUtils,
  dwg;

type
  TDWGHandle = UInt64;

  TDWGHandleSource = (
    hsNull,
    hsAbsoluteRef,
    hsHandleref
  );

  TDWGHandleRef = record
    Value: TDWGHandle;
    Source: TDWGHandleSource;
    function IsNull: Boolean;
    function ToString: string;
    class function Null: TDWGHandleRef; static;
  end;

  TDWGObjectStatus = (
    osRaw,
    osResolved,
    osPartial,
    osBroken,
    osFailed
  );

  TDWGLoadMode = (
    lmStrict,
    lmTolerant
  );

  TDWGVersion = (
    dvInvalid,
    dvR13,
    dvR14,
    dvR2000,
    dvR2004,
    dvR2007,
    dvR2010,
    dvR2013,
    dvR2018,
    dvAfter
  );

  TDWGDomainObjectType = (
    dotHeader,
    dotLayer,
    dotLinetype,
    dotStyle,
    dotBlockHeader,
    dotBlock,
    dotLine,
    dotArc,
    dotCircle,
    dotLWPolyline,
    dotText,
    dotSyntheticTable,
    dotUnknown
  );

  TDWGErrorSeverity = (
    desInfo,
    desWarning,
    desError,
    desFatal
  );

  TDWGError = record
    Code: Integer;
    Severity: TDWGErrorSeverity;
    Handle: TDWGHandle;
    ObjectType: DWG_OBJECT_TYPE;
    Message: string;
  end;

implementation

function TDWGHandleRef.IsNull: Boolean;
begin
  Result := (Source = hsNull) or (Value = 0);
end;

function TDWGHandleRef.ToString: string;
begin
  if IsNull then
    Result := '0'
  else
    Result := IntToHex(Value, 1);
end;

class function TDWGHandleRef.Null: TDWGHandleRef;
begin
  Result.Value := 0;
  Result.Source := hsNull;
end;

end.
