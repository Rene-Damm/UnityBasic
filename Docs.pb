XIncludeFile "Code.pb"

DeclareModule Docs
  
  UseModule Code
  
  Declare.s GenerateDocs( Path.s, *Code.Code )
  
EndDeclareModule

Module Docs
EndModule

; IDE Options = PureBasic 5.73 LTS (Windows - x64)
; CursorPosition = 4
; Folding = -
; EnableXP