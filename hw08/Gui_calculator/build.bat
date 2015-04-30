@echo off

cls
powershell -Command "(New-Object Net.WebClient).DownloadFile('http://nuget.org/nuget.exe', 'NuGet.exe')"
"NuGet.exe" "Install" "FAKE" "-OutputDirectory" "packages" "-ExcludeVersion"
"NuGet.exe" "Install" "NUnit.Runners" "-OutputDirectory" "packages" "-ExcludeVersion"
pause