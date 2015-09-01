@echo off

setlocal enableextensions enabledelayedexpansion
set progName=%~n0

pushd %~d0%~p0\..
if errorlevel 1 (
    echo Base directory does not exist, cannot install >&2
    goto :EOF
)

set rexec=R
set libpath=

rem Windows CMD shell parameter processing, rather complicated
rem unfortunately when suitable for parameters with spaces and
rem parantheses -- especially if we try to do it in a generic way.
rem The following does it.
rem There must not be any singly parenthesis within all that or
rem path processing of paths like "C:\Prgram Files (x86)" will fail.
rem That's why all these if's are there on several lines because
rem I cannot use if <cond> ( do-this-and-that ) when this variables
rem are affected.
:parms
  if "%1"=="--" goto eofparm
  if "%1"=="" goto eofparm
  set chk=%1
  set chk=%chk:~0,1%
  if not %chk%==- goto eofparm
  if "%1"=="-R" call :setglob rexec %2
  if "%1"=="-R" (
    shift
    shift
    goto parms
  )
  if "%1"=="-l" call :setglob libpath %2
  if "%1"=="-l" (
    shift
    shift
    goto parms
  )
  if "%1"=="-h" (
    echo Usage:%progName% [-l libpath] [-R path\to\R\R]
    goto :error
  )
  echo Ignored option:%1
  shift
  goto parms
:eofparm

echo Check setup >&2

call :chksetup
if errorlevel 1 goto :error

echo Now install!
set failed=0
for /D %%p in (.\packages\*) do call :instpkg %%~np
if errorlevel 1 goto :error
goto :success

rem small subroutines
rem -----------------

rem Install a single package. Set stop indicator as we cannot
rem break the loop.
:instpkg
if %failed%==1 goto :EOF
echo Install package "%1"
rem Yes R likes to have a slash here instead of a backslash.
if "%libpathSet%"=="1" "%rexec%" CMD INSTALL -l "%libpath%" ./packages/"%1"
rem no if / else as libpath may contain parenthesis
if not "%libpathSet%"=="1" "%rexec%" CMD INSTALL ./packages/"%1"
if errorlevel 1 (
  set failed=1
  ver /invalid 2>NUL
)
goto :EOF

:chksetup
set chk=0
rem for won't look into file system without a single wildcard
for /D %%p in (packag?s) do set chk=1
if %chk%==0 (
  echo Directory "packages" not found >&2
  ver /invalid 2>NUL
  goto :EOF
)
set chk=0
for /D %%p in (.\packages\*) do set chk=1
if %chk%==0 (
  echo Not a single package found for installation >&2
  ver /invalid 2>NUL
  goto :EOF
)
"%rexec%" --slave -e "1" >NUL 2>&1
if errorlevel 1 (
  echo Cannot start a simple R program >&2
  ver /invalid 2>NUL
  goto :EOF
)
goto :EOF

:setglob
rem Copy a quoted value with parantheses into a variable.
rem First line removes this quotes, therefore
rem the for is needed. The ampersand is invalid
rem in paths and therefore used as delimiter that
rem never occurs.
for /F "usebackq delims=&" %%i in ('%2') do set %1=%%~i
set %1Set=1
goto :EOF

:cpglob
rem Copy the contents of a global variable into another
rem when the source contains a paranthesis and the
rem set shall happen within a paranthesis.
rem Variable contents never contain the enclosing
rem quotes, so no for hack here.
set %1=!%2!
set %1Set=
goto :EOF

rem end of subroutines
rem ------------------


:success
echo Installation successful
popd
ver >nul
goto :EOF

:error
popd
echo Installation failed
rem Generate error code.
ver /InvalidOption >nul 2>&1
