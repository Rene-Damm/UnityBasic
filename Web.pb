XIncludeFile "Code.pb"
XIncludeFile "Machine.pb"

DeclareModule Web
  
  Structure WebHost
  EndStructure
  
  Declare   ConnectToWebHost( *Host.WebHost )
  Declare   LaunchLocalWebHost( *Host.WebHost )
  
EndDeclareModule

Module Web
EndModule

; IDE Options = PureBasic 5.73 LTS (Windows - x64)
; CursorPosition = 8
; Folding = -
; EnableXP