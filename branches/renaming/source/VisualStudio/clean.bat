@echo off
echo Deleting all temporary files...
del *_force.txt /Q

rem Delete compiler temporary files.
del ..\compiler\*.obj /Q
del ..\compiler\*.cm* /Q


rem Delete disassembler temporary files.
del ..\disassembler\*.obj /Q
del ..\disassembler\*.cm* /Q

rem Delete front-end temporary files.
del ..\front\*.obj /Q
del ..\front\*.cm* /Q

rem Delete loader temporary files.
del ..\loader\*.obj /Q
del ..\loader\*.cm* /Q

echo Done.
