!include "MUI2.nsh"
!include "path.nsh"

!system 'cd ..\.. && svn up' = 0
!system 'cd .. && omake distclean' = 0
!system 'cd .. && omake all' = 0
!system '..\tjcc -v | perl -pe "s/Teyjus version/!define VERSION/" > version.nsh'
!include version.nsh

; The name of the installer
Name "Teyjus"

; The file to write
OutFile "teyjus-installer-${VERSION}.exe"

; The default installation directory
InstallDir $PROGRAMFILES\Teyjus

; Registry key to check for directory (so if you install again, it will
; overwrite the old one automatically)
InstallDirRegKey HKLM "Software\Teyjus" "Install_Dir"

; Request application privileges for Windows Vista
RequestExecutionLevel admin

;--------------------------------
; Pages

!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!define MUI_FINISHPAGE_TEXT "Teyjus has been installed successfully. The directory '$INSTDIR\bin' has been added to your path."
!insertmacro MUI_PAGE_FINISH


!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES


;--------------------------------
;Languages

!insertmacro MUI_LANGUAGE "English"


;--------------------------------
; The stuff to install

Section "Teyjus (required)"

  SetOutPath $INSTDIR\bin
  File "..\tjcc.exe"
  File "..\tjsim.exe"
  File "..\tjlink.exe"
  File "..\tjdis.exe"
  File "..\tjdepend.exe"

  SetOutPath $INSTDIR
  File /r /x .svn "..\..\emacs"

  File /r /x .svn /x depend /x *.lp /x *.lpo "..\..\examples"

  ; Add to user's $PATH
  Push $INSTDIR\bin
  Call AddToPath

  ; Write the installation path into the registry
  WriteRegStr HKLM SOFTWARE\Teyjus "Install_Dir" "$INSTDIR"

  ; Write the uninstall keys for Windows
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Teyjus" "DisplayName" "Teyjus"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Teyjus" "UninstallString" '"$INSTDIR\uninstall.exe"'
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Teyjus" "NoModify" 1
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Teyjus" "NoRepair" 1
  WriteUninstaller "uninstall.exe"

SectionEnd

;--------------------------------

; Uninstaller

Section "Uninstall"

  ; Remove registry keys
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Teyjus"
  DeleteRegKey HKLM SOFTWARE\Teyjus

  ; Remove files and uninstaller
  RMDir /r $INSTDIR\bin
  RMDir /r $INSTDIR\emacs
  RMDir /r $INSTDIR\examples
  Delete $INSTDIR\uninstall.exe

  ; Remove from user's $PATH
  Push $INSTDIR
  Call un.RemoveFromPath

  ; Remove directories used
  RMDir "$INSTDIR"

SectionEnd
