{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit DKVTUtils;

{$warn 5023 off : no warning about unused units}
interface

uses
  DK_VSTUtils, DK_VSTTables, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('DKVTUtils', @Register);
end.
