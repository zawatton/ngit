@echo off
rem Python preserves each argument while adding the runtime-safe framing.
if defined NGIT_PYTHON goto custom_python
py -3 "%~dp0..\tools\ngit-windows.py" %*
exit /b %errorlevel%
:custom_python
"%NGIT_PYTHON%" "%~dp0..\tools\ngit-windows.py" %*
exit /b %errorlevel%
