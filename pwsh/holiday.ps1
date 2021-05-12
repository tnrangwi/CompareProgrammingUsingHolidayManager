#!/usr/local/bin/pwsh
#Terminate on powershell error in function or cmd-let. External
#commands still need to be checked with $LastExitCode, $? checks
#both, $Error contains a full set of last errors and can be cleared.
#This preference can be reset on command basis.
$ErrorActionPreference="Stop"
$Env:PSModulePath="${Env:PSModulePath}:$PSScriptRoot/lib"
Import-Module ConfigReader

function Load-UsrFile ([String]$fn) {
    $userName = Split-Path -LeafBase $fn
    $lines = Get-Content -Path $fn
    #if ($lines.Count -lt 1)
    $usr = @{ group = $lines[0]; table = @() }
    for ($i = 1; $i -le $lines.Count; $i++) {
        Write-Host("Holiday line:" + $lines[$i])
        #FIXME: Split and check
        if ($lines[$i] -gt 0) {
            $usr.table += $lines[$i]
        }
    }
    $userDicts.Add($userName, $usr)
}

$cfgFile = "${PSScriptRoot}/etc/holiday.conf"
$userDicts = @{}
if (Test-Path $cfgFile) {
    $cfg = ConfigReader\Read-ConfigFile $cfgFile
} else {
    $cfg = @{ "global" = @( @{ "port"=@(1970); "server"=@("localhost") } ) }
}
Get-ChildItem  -Filter *.usr | ForEach-Object { Load-UsrFile $_.FullName }
#Then startup server

#$cfg.global[0].port[0]
#$cfg.global[0].privileged[0]
#$cfg.global[0].privileged[1]
