@echo off
for /f "tokens=2 delims==" %%G in ('wmic os get localdatetime /value') do set datetime=%%G

set year=%datetime:~0,4%
set month=%datetime:~4,2%
set day=%datetime:~6,2%
set hour=%datetime:~8,2%
set minute=%datetime:~10,2%

@echo on

SET rScript=[PathToR]/bin/Rscript.exe

%rScript% --verbose [pathToAutoDistribution]\AutoDistributionScript.R > [pathToAutoDistribution]\outputFile-%year%-%month%-%day%-%hour%%minute%.Rout 2>&1