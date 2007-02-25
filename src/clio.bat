REM $Id: clio.bat,v 1.2 2006/05/16 10:52:42 Joaquim Exp $
REM $Author: Joaquim $
SET CLIOPATH=C:\users\joaquim\develop\rch\clio\src
CD %CLIOPATH%
"C:\Program files\pl\bin\plcon.exe" -f clioStart.pl -- -sf gacto.str -df %1 -echo yes

