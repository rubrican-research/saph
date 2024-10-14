{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit saph2_12;

{$warn 5023 off : no warning about unused units}
interface

uses
    Controls.Listener, saph.lists, Obj.Listener, saph.reactive, saph.winman, 
    LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('saph2_12', @Register);
end.
