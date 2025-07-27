{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit DKVTUtils;

{$warn 5023 off : no warning about unused units}
interface

uses
  DK_VSTTypes, DK_VSTTables, DK_VSTTableTools, DK_VSTDropDown, 
  DK_VSTDropDownConst, DK_VSTDropDownForm, DK_VSTCore, DK_VSTEdit, 
  DK_VSTEditTools, DK_VSTParamList, DK_VSTCategoryTables, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('DKVTUtils', @Register);
end.
