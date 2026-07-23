@echo off

cd /d "C:\Users\scott\BaseballR\Predicting Breakout Hitters"

git add .

git diff --cached --quiet
if %ERRORLEVEL%==0 (
    echo No changes to commit.
    exit /b 0
)

git commit -m "Automated daily Pioneer Scout refresh"

git push origin main