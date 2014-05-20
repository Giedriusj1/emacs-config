@echo off
::set PATH=C:\Users\Giedrius\Desktop\UnxUpdates;%PATH%
set HOME=C:\Users\Giedrius\Desktop\emacs-24.3\bin
set PATH=%HOME%\UnxUpdates;%PATH%
::set HOME=%CD%\..\

bitsadmin /transfer mydownloadjob  /download /priority normal https://raw.githubusercontent.com/Giedriusj1/emacs-config/master/emacs.org %HOME%/.emacs.d/emacs.org
"%~dp0emacsclientw.exe" -na  "%~dp0runemacs.exe " "%1"
