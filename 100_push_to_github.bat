@echo off
setlocal

cd /d "%~dp0"

git add -A
if errorlevel 1 exit /b %errorlevel%

git diff --cached --quiet
if not errorlevel 1 (
    echo No changes to commit.
    exit /b 0
)

git commit -m "Automated daily Pioneer Scout refresh"
if errorlevel 1 exit /b %errorlevel%

git push origin main
exit /b %errorlevel%
