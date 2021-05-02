#!/usr/local/bin/pwsh

function Read-ConfigFile ([String]$fn) {
    #FIXME: Error handling not there at all.
    $cfg = @{}
    $section = $null
    $secNr = 0
    #switch -f <file> would do as well
    Get-Content $fn | ForEach-Object {
        Switch -regex  ($_) {
            "^#" {
                #ignore comments
                Break
            } "^\[([a-zA-Z]+)\]" {
                #new section
                $section = $matches[1]
                if ($cfg.ContainsKey($section)) {
                    $cfg[$section] += @{}
                    $secNr = $cfg[$section].Length - 1
                } else {
                    $cfg.Add($section, @(@{}))
                    $secNr = 0
                }
                Break
            } '^([a-zA-Z][a-zA-Z0-9_]*)=(.*)$' {
                $key = $matches[1]
                $valstr = $matches[2]
                if ($section -eq $null) {
                    $section = "default"
                    $cfg.Add($section, @(@{}))
                }
                if ($valstr -match '^-?[0-9]{1,9}$') {
                    $val = $valstr -as [int]
                } elseif ($valstr -match '^+?[0-9]{1,9}[.]?[0-9]{0,9}' -or $valstr -match '^-?[0-9]{1,9}[.]?[0-9]{0,9}E-?[0-9]{1,3}') {
                    $val = $valstr -as [float]
                } elseif ($valstr -match '^"(.*)"$') {
                    $val = $matches[1]
                } elseif ($valstr.Length -eq 0) {
                    $val = $null
                } else {
                    Write-Host "Parser error - unsupported value type"
                    return $null
                }
                if ($cfg.$section[$secNr].ContainsKey($key)) {
                    $cfg.$section[$secNr].$key += $val
                } else {
                    $cfg.$section[$secNr].Add($key, @($val))
                }
                Break
            } default {
                Write-Host "Parser error - unsupported line in config file"
                return $null
            }
        }
    }
    $cfg
}

$cfgFile = "${PSScriptRoot}/etc/holiday.conf"
if (Test-Path $cfgFile) {
    $cfg = Read-ConfigFile $cfgFile
} else {
    $cfg = @{ "global" = @( @{ "port"=@(1970); "server"=@("localhost") } ) }
}
#$cfg.global[0].port[0]
#$cfg.global[0].privileged[0]
#$cfg.global[0].privileged[1]
