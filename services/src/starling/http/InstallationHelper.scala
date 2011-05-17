package starling.http

object InstallationHelper {
  def generateL4JXML(serverURL:String, serverName:String, booterJarPath:String, iconPath:String, splashPath:String, starlingTimestamp:Long) = {
    <launch4jConfig>
      <dontWrapJar>false</dontWrapJar>
      <headerType>gui</headerType>
      <jar>{booterJarPath}</jar>
      <outfile>Starling-{serverName}_{starlingTimestamp}.exe</outfile>
      <errTitle></errTitle>
      <cmdLine>{serverURL} {serverName}</cmdLine>
      <chdir></chdir>
      <priority>normal</priority>
      <downloadUrl>http://java.com/download</downloadUrl>
      <supportUrl></supportUrl>
      <customProcName>true</customProcName>
      <stayAlive>false</stayAlive>
      <manifest></manifest>
      <icon>{iconPath}</icon>
      <jre>
        <path>java6_update_21</path>
        <minVersion>1.6.0</minVersion>
        <maxVersion></maxVersion>
        <jdkPreference>preferJre</jdkPreference>
        <maxHeapSize>500</maxHeapSize>
        <opt>-Dsun.java2d.d3d=false</opt>
      </jre>
      <splash>
        <file>{splashPath}</file>
        <waitForWindow>true</waitForWindow>
        <timeout>60</timeout>
        <timeoutErr>true</timeoutErr>
      </splash>
      <versionInfo>
        <fileVersion>0.0.0.1</fileVersion>
        <txtFileVersion>Some more</txtFileVersion>
        <fileDescription>Starling</fileDescription>
        <copyright>Trafigura</copyright>
        <productVersion>0.0.0.1</productVersion>
        <txtProductVersion>fds</txtProductVersion>
        <productName>Starling</productName>
        <companyName>Trafigura</companyName>
        <internalName>Starling</internalName>
        <originalFilename>Starling-{serverName}.exe</originalFilename>
      </versionInfo>
    </launch4jConfig>
  }

  def generateNSISText(serverName:String) = {
""";!include "MUI2.nsh"

!define product "Starling-""" + serverName + """"
!define exe "${product}.exe"
!define excel_ini "excel_plugin-""" + serverName + """.ini"
!define excel_xll "excel_plugin-""" + serverName + """.xll"
!define uninstall_exe "uninstall_${product}.exe"
!define company "Trafigura"
!define add_remove_key "Software\Microsoft\Windows\CurrentVersion\Uninstall\${product}"
!define java_version_str "SOFTWARE\JavaSoft\Java Runtime Environment"

Name ${product}
OutFile "install_${product}.exe"
InstallDir $PROGRAMFILES\${company}\${product}

Section

; Lets check that we are admin
UserInfo::GetAccountType
Pop $0
StrCmp $0 "Admin" +3
MessageBox MB_OK "You have to be admin to install ${product}"
Quit

; Lets check the java version
ReadRegStr $1 HKLM "${java_version_str}" CurrentVersion
StrCmp $1 "1.6" +3
MessageBox MB_OK "You must have Java 6 to install ${product}"
Quit

SetShellVarContext all

SetOutPath $INSTDIR

; Add a registry key so that we can use kerberos
WriteRegDWORD HKLM "SYSTEM\CurrentControlSet\Control\Lsa\Kerberos" "allowtgtsessionkey" 1
; Add in the place Win 7 would expect it. Doesn't hurt XP to have it there too.
WriteRegDWORD HKLM "SYSTEM\CurrentControlSet\Control\Lsa\Kerberos\Parameters" "allowtgtsessionkey" 1

File ${exe}
File ${excel_ini}
File ${excel_xll}

; Shortcut on the start menu
CreateDirectory "$SMPROGRAMS\${company}"
CreateShortCut "$SMPROGRAMS\${company}\${product}.lnk" "$INSTDIR\${exe}"
; Uninstall shortcut on the start menu
CreateShortCut "$SMPROGRAMS\${company}\Uninstall ${product}.lnk" "$INSTDIR\${uninstall_exe}"
; Shortcut on the desktop
CreateShortCut "$DESKTOP\${product}.lnk" "$INSTDIR\${exe}"

; Write uninstall information to the add/remove programs bit in control panel
WriteRegStr HKLM "${add_remove_key}" "DisplayName" "${product} (remove only)"
WriteRegStr HKLM "${add_remove_key}" "UninstallString" "$INSTDIR\${uninstall_exe}"

WriteUninstaller $INSTDIR\${uninstall_exe}

SectionEnd

Section "Uninstall"

SetShellVarContext all

Delete $INSTDIR\${uninstall_exe}

Delete $INSTDIR\${exe}
Delete $INSTDIR\${excel_ini}
Delete $INSTDIR\${excel_xll}

RMDir /r $INSTDIR\

; This will only delete the company directory if it is empty.
RMDir $PROGRAMFILES\${company}

; Remove the start menu items
Delete "$SMPROGRAMS\${company}\${product}.lnk"
Delete "$SMPROGRAMS\${company}\Uninstall ${product}.lnk"
; Remove company start menu if required.
RMDir "$SMPROGRAMS\${company}"
; Remove desktop shortcut
Delete "$DESKTOP\${product}.lnk"
; Remove the key in the add/remove programs bit in control panel
DeleteRegKey HKLM "${add_remove_key}"

SectionEnd"""
  }
}