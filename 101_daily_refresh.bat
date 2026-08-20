@echo off
setlocal

cd /d "%~dp0"

"C:\Program Files\R\R-4.6.0\bin\Rscript.exe" "%~dp099_refresh_database.R"
if errorlevel 1 exit /b %errorlevel%

call "%~dp0100_push_to_github.bat"
exit /b %errorlevel%
