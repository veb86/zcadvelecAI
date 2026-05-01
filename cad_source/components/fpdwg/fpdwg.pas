{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fpdwg;

{$warn 5023 off : no warning about unused units}
interface

uses
  dwg, dwgproc, fpdwg_types, fpdwg_logger, fpdwg_libredwg_utils,
  fpdwg_reader, fpdwg_filter, fpdwg_factory, fpdwg_model_base,
  fpdwg_model_tables, fpdwg_model_blocks, fpdwg_model_entities,
  fpdwg_model_unknown, fpdwg_map_unknown, fpdwg_map_layer,
  fpdwg_map_linetype, fpdwg_map_block, fpdwg_map_line, fpdwg_registry,
  fpdwg_document, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('fpdwg', @Register);
end.
