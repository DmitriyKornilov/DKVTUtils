{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit DKVTUtils;

{$warn 5023 off : no warning about unused units}
interface

uses
  DK_VSTTypes, DK_VSTTables, DK_VSTTableTools, DK_DropDown, DK_DropDownConst, 
  DK_DropDownForm, DK_VSTCore, DK_VSTEdit, DK_TVSTEditTools, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('DKVTUtils', @Register);
end.
