
EnableExplicit

;==============================================================================
; Initialize.

InitScintilla()
ExamineDesktops()

Define.i WindowWidth = DesktopWidth( 0 )
Define.i WindowHeight = DesktopHeight( 0 )

Define Window.i = OpenWindow( #PB_Any, 0, 0, WindowWidth, WindowHeight, "Unity Basic", #PB_Window_BorderLess )
Define Scintilla.i = ScintillaGadget( #PB_Any, 0, 0, WindowWidth, WindowHeight, 0 )

Define *Text = UTF8( "Test" )
ScintillaSendMessage( Scintilla, #SCI_SETTEXT, 0, *Text )

;==============================================================================
; Main loop.

Repeat
  
  Define Event = WaitWindowEvent()
  
  If Event = #PB_Event_Gadget
  EndIf
  
Until Event = #PB_Event_CloseWindow

;[X] Full-screen text view
;[ ] Text is saved and loaded
;[ ] Type definition is parsed

; IDE Options = PureBasic 5.73 LTS (Windows - x64)
; CursorPosition = 30
; EnableXP