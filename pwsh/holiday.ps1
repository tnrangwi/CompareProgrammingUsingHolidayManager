#!/usr/local/bin/pwsh
#Terminate on powershell error in function or cmd-let. External
#commands still need to be checked with $LastExitCode, $? checks
#both, $Error contains a full set of last errors and can be cleared.
#This preference can be reset on command basis.
$ErrorActionPreference="Stop"
$Env:PSModulePath="${Env:PSModulePath}:$PSScriptRoot/lib"
Import-Module ConfigReader

$cfgFile = "${PSScriptRoot}/etc/holiday.conf"
if (Test-Path $cfgFile) {
    $cfg = ConfigReader\Read-ConfigFile $cfgFile
} else {
    $cfg = @{ "global" = @( @{ "port"=@(1970); "server"=@("localhost") } ) }
}
#$cfg.global[0].port[0]
#$cfg.global[0].privileged[0]
#$cfg.global[0].privileged[1]
