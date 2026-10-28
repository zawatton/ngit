@echo off
if defined NGIT_PYTHON goto custom_python
py -3 "%~dp0ngit-git" %*
exit /b %errorlevel%
:custom_python
"%NGIT_PYTHON%" "%~dp0ngit-git" %*
exit /b %errorlevel%
