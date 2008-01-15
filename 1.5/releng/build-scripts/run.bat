@echo off
setlocal

REM The location where the headless build scripts & resource files live.
set BUILDHOME=%~dp0

REM ******************************************
REM *          CHANGE THESE SETTINGS         *
REM ******************************************

REM The JRE java.exe to be used
set JAVAEXE=C:\bin\jdk\sun\bin\java.exe

REM the Eclipse to use
set ECLIPSEHOME=C:\bin\sdp\3.2.2\eclipse

REM CVS login info - provide CVS user name and password
REM :extssh:<username>:<password>
REM put "" around the value if it contains non-alphanumeric chars
set CVSINFO=:pserver:anonymous

REM The directory containing tag.current.properties
SET BUILDSTATEDIR=S:\projects\epf\1.2.0\state

REM ******************************************
REM *     DO NOT CHANGE THESE SETTINGS       *
REM ******************************************

REM The Eclipse startup.jar
set STARTUPJAR="%ECLIPSEHOME%\startup.jar"

REM The scripts directory in the org.eclipse.pde.build Eclipse plugin.
set SCRIPTSHOME="%ECLIPSEHOME%\plugins\org.eclipse.pde.build_3.2.1.r321_v20060823\scripts"

REM The location where the headless build scripts & resource files live.
REM set BUILDHOME=C:\builds\epf_110\headless
set BUILDHOME=%~dp0

REM The location of the temporary build workspace
set WORKSPACE=%BUILDHOME%\workspace

REM The buildfile to use for the build
set BUILDFILE=build.xml

REM The builder directory containing build.properties
REM build.properties, allElements.xml and customTargets.xml needs to be in the same place
set BUILDERDIR=%BUILDHOME%

set HOUR=%time:~0,2%
if "%HOUR:~0,1%"==" " set HOUR=0%time:~1,1%
set BUILDID=%date:~-4%%date:~4,2%%date:~7,2%-%HOUR%%time:~3,2%

REM ****************************************************


if not exist %JAVAEXE% echo ERROR: incorrect java.exe=%JAVAEXE%, edit this file and correct the JAVAEXE envar
if not exist %JAVAEXE% goto done

if not exist %STARTUPJAR% echo ERROR: incorrect startup.jar=%STARTUPJAR%, edit this file and correct the STARTUPJAR envar
if not exist %STARTUPJAR% goto done

if not exist %BUILDERDIR% echo ERROR: incorrect builderdir=%BUILDERDIR%, edit this file and correct the BUILDERDIR envar
if not exist %BUILDERDIR% goto done

if not exist %BUILDSTATEDIR% echo ERROR: incorrect buildStateDir=%BUILDSTATEDIR%, edit this file and correct the BUILDSTATEDIR envar
if not exist %BUILDSTATEDIR% goto done


:run

pushd %SCRIPTSHOME%

if not exist %BUILDFILE% echo ERROR: incorrect buildfile=%BUILDFILE%, edit this file and correct the BUILDFILE envar
if not exist %BUILDFILE% goto done

@echo on

rd /s /q %WORKSPACE%

%JAVAEXE% -cp %STARTUPJAR% org.eclipse.core.launcher.Main -noupdate -application org.eclipse.ant.core.antRunner -data %WORKSPACE% -buildfile %BUILDFILE% -Dbuilder=%BUILDERDIR% -DbuildId=%BUILDID% -DbuildStateDir=%BUILDSTATEDIR% -DcvsInfo=%CVSINFO% -DbaseLocation=%ECLIPSEHOME% %1 %2 %3 %4 %5 %6 %7 %8 %9

@echo off
popd

:done
REM pause
