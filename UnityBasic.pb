
EnableExplicit

Define.s ProjectPath = "C:\Dropbox\Workspaces\UnityBasic_PB\TestProject"
Define.s TextFilePath = ProjectPath + "\TestFile.code"

;==============================================================================
; Initialize.


InitScintilla()
ExamineDesktops()

Define.i WindowWidth = DesktopWidth( 0 )
Define.i WindowHeight = DesktopHeight( 0 )

Define Window.i = OpenWindow( #PB_Any, 0, 0, WindowWidth, WindowHeight, "Unity Basic", #PB_Window_BorderLess )
Global Scintilla.i = ScintillaGadget( #PB_Any, 0, 0, WindowWidth, WindowHeight, 0 )

#WINDOW_SAVE_TIMER = 0

AddWindowTimer( Window, #WINDOW_SAVE_TIMER, 2000 )

Global.i TextFile
Global.i TextLength = FileSize( TextFilePath )
If TextLength = -1
  TextFile = CreateFile( #PB_Any, TextFilePath, #PB_UTF8 | #PB_File_SharedRead )
  TextLength = 0
Else
  TextFile = OpenFile( #PB_Any, TextFilePath, #PB_UTF8 | #PB_File_SharedRead )
EndIf

Global *Text = AllocateMemory( TextLength + 1024 )
ReadData( TextFile, *Text, TextLength )
PokeB( *Text + TextLength, 0 )
ScintillaSendMessage( Scintilla, #SCI_SETTEXT, 0, *Text )
ScintillaSendMessage( Scintilla, #SCI_SETREADONLY, 0 )

;==============================================================================

Procedure FlushText()
  
  If ScintillaSendMessage( Scintilla, #SCI_GETMODIFY ) = 0
    ProcedureReturn
  EndIf  
  
  TextLength = ScintillaSendMessage( Scintilla, #SCI_GETLENGTH )
  If MemorySize( *Text ) < ( TextLength + 1 )
    *Text = ReAllocateMemory( *Text, TextLength + 1024 )
  EndIf
  
  ScintillaSendMessage( Scintilla, #SCI_GETTEXT, TextLength + 1, *Text )
  FileSeek( TextFile, 0 )
  WriteData( TextFile, *Text, TextLength )
  TruncateFile( TextFile )
  ScintillaSendMessage( Scintilla, #SCI_SETSAVEPOINT )
  
EndProcedure

;==============================================================================
; Main loop.

Repeat
  
  Define Event = WaitWindowEvent()
  
  If Event = #PB_Event_Timer
    If EventTimer() = #WINDOW_SAVE_TIMER
      FlushText()
    EndIf
  ElseIf Event = #PB_Event_Gadget
  EndIf
  
Until Event = #PB_Event_CloseWindow

FlushText()

;[X] Full-screen text view
;[X] Text is saved and loaded
;[ ] Type definition is parsed

; IDE Options = PureBasic 5.73 LTS (Windows - x64)
; CursorPosition = 78
; FirstLine = 25
; Folding = -
; EnableXP