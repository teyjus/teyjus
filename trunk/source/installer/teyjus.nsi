!include "path.nsh"

; The name of the installer
Name "Teyjus"

; The file to write
OutFile "teyjus-installer.exe"

; The default installation directory
InstallDir $PROGRAMFILES\Teyjus

; Registry key to check for directory (so if you install again, it will 
; overwrite the old one automatically)
InstallDirRegKey HKLM "Software\Teyjus" "Install_Dir"

; Request application privileges for Windows Vista
RequestExecutionLevel admin

;--------------------------------

; Pages

;Page components
Page directory
Page instfiles

UninstPage uninstConfirm
UninstPage instfiles

;--------------------------------

; The stuff to install
Section "Teyjus (required)"

  ; Set output path to the installation directory.
  SetOutPath $INSTDIR
  
  ; Files to install
  File "..\tjcc.exe"
  File "..\tjsim.exe"
  File "..\tjlink.exe"
  File "..\tjdis.exe"
  File "..\tjdepend.exe"
  
  File "..\..\emacs\teyjus.el"
  
  ; Add to user's $PATH
  Push $INSTDIR
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
  Delete $INSTDIR\tjcc.exe
  Delete $INSTDIR\tjsim.exe
  Delete $INSTDIR\tjlink.exe
  Delete $INSTDIR\tjdis.exe
  Delete $INSTDIR\tjdepend.exe
  Delete $INSTDIR\teyjus.el
  Delete $INSTDIR\uninstall.exe

  ; Remove from user's $PATH
  Push $INSTDIR
  Call un.RemoveFromPath

  ; Remove directories used
  RMDir "$SMPROGRAMS\Teyjus"
  RMDir "$INSTDIR"

SectionEnd
