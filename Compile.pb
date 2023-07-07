; Compiles a Program into a MachineProgram.

XIncludeFile "Code.pb"
XIncludeFile "Machine.pb"

DeclareModule Compile
  
  UseModule Code
  UseModule Machine
  
  Declare   Compile( *Code.Code, ProgramIndex.l, *Program.MachineProgram )
  
EndDeclareModule

Module Compile
  
  ; ------------------------------------------------------------------------
  
  Procedure Compile( *Code.Code, ProgramIndex.l, *Program.MachineProgram )
  EndProcedure
  
EndModule

; IDE Options = PureBasic 5.73 LTS (Windows - x64)
; CursorPosition = 1
; Folding = -
; EnableXP