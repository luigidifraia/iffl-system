@ECHO off
REM
REM This file is the script to set PATH for the IFFL build tool chain.
REM
SET TL_PATH=%~dp0
SET PATH=%TL_PATH%;%PATH%
CMD /K cd %CD%\..\src\examples
