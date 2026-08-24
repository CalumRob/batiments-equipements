# 22-memory-sampler.ps1 — background memory profiler for the #22 gate.
# Samples every Rscript/java process every $SampleSeconds, tagging each by
# command line (orchestrator | worker child | build | other), appending to a
# CSV. Stops when the stop-file appears.

param(
    [string]$OutCsv = "E:\Temp\opencode\pocock-workers\batiments-equipements\issue-22\.scratch\tracer-matrice\research\outputs\22-memory-profile.csv",
    [string]$StopFile = "E:\Temp\opencode\pocock-workers\batiments-equipements\issue-22\.scratch\tracer-matrice\research\outputs\22-sampler.stop",
    [int]$SampleSeconds = 10
)

if (-not (Test-Path -LiteralPath $OutCsv)) {
    "timestamp_utc,pid,name,role,working_set_mb,private_mb" | Set-Content -LiteralPath $OutCsv
}

function Get-Role([string]$cl) {
    if ($cl -match 'worker_bootstrap') { "worker-child" }
    elseif ($cl -match '22b-build-network') { "network-build" }
    elseif ($cl -match '22e-run-probe') { "probe-orchestrator" }
    elseif ($cl -match 'osmosis') { "osmosis" }
    else { "other" }
}

while ($true) {
    if (Test-Path -LiteralPath $StopFile) { break }
    $stamp = (Get-Date).ToUniversalTime().ToString("yyyy-MM-ddTHH:mm:ssZ")
    $procs = Get-CimInstance Win32_Process -Filter "Name='Rscript.exe' OR Name='Rgui.exe' OR Name='java.exe'" -ErrorAction SilentlyContinue
    foreach ($p in $procs) {
        $ws = [math]::Round(($p.WorkingSetSize / 1MB), 1)
        $privRaw = $p.PrivateMemorySize
        if ($null -eq $privRaw) { $privRaw = 0 }
        $priv = [math]::Round(($privRaw / 1MB), 1)
        $role = Get-Role ([string]$p.CommandLine)
        "$stamp,$($p.ProcessId),$($p.Name),$role,$ws,$priv" |
            Add-Content -LiteralPath $OutCsv
    }
    Start-Sleep -Seconds $SampleSeconds
}
