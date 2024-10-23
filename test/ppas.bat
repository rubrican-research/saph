@echo off
SET THEFILE=R:\Data\Dev\Libraries\saph\test\multi.exe
echo Linking %THEFILE%
R:\Apps\Lazarus\3.4\fpc\3.2.2\bin\x86_64-win64\ld.exe -b pei-x86-64  --gc-sections  -s --subsystem windows --entry=_WinMainCRTStartup   --base-file base.$$$ -o R:\Data\Dev\Libraries\saph\test\multi.exe R:\Data\Dev\Libraries\saph\test\link140080.res
if errorlevel 1 goto linkend
dlltool.exe -S R:\Apps\Lazarus\3.4\fpc\3.2.2\bin\x86_64-win64\as.exe -D R:\Data\Dev\Libraries\saph\test\multi.exe -e exp.$$$ --base-file base.$$$ 
if errorlevel 1 goto linkend
R:\Apps\Lazarus\3.4\fpc\3.2.2\bin\x86_64-win64\ld.exe -b pei-x86-64  -s --subsystem windows --entry=_WinMainCRTStartup  -o R:\Data\Dev\Libraries\saph\test\multi.exe R:\Data\Dev\Libraries\saph\test\link140080.res exp.$$$
if errorlevel 1 goto linkend
goto end
:asmend
echo An error occurred while assembling %THEFILE%
goto end
:linkend
echo An error occurred while linking %THEFILE%
:end
