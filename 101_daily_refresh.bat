@echo off

"C:\Program Files\R\R-4.6.0\bin\Rscript.exe" "C:\Users\scott\BaseballR\Predicting Breakout Hitters\99_refresh_database.R"

call "C:\Users\scott\BaseballR\Predicting Breakout Hitters\100_push_to_github.bat"