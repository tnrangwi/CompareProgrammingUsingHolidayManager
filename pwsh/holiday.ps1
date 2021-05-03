#!/usr/local/bin/pwsh
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
