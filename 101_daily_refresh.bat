@echo off
REM The GitHub Actions workflow is the single source of truth for daily
REM refreshes. Keeping this launcher successful avoids a duplicate local run
REM racing the cloud workflow and creating non-fast-forward Git push failures.
exit /b 0
