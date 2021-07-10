{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CodeStats;

{$warn 5023 off : no warning about unused units}
interface

uses
  uCodeStatsPlugin, FConfig, uThreadPulse, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('uCodeStatsPlugin', @uCodeStatsPlugin.Register);
end;

initialization
  RegisterPackage('CodeStats', @Register);
end.
