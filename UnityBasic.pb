
EnableExplicit

Define.s ProjectPath = "C:\Dropbox\Workspaces\UnityBasic_PB\TestProject"
Define.s TextFilePath = ProjectPath + "\TestFile.code"

#SPACE = 32
#NEWLINE = 10
#RETURN = 13
#EQUALS = 61
  
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

SetActiveGadget( Scintilla )

;==============================================================================

Enumeration DefinitionType
  #TypeDefinition
  #MethodDefinition
  #FieldDefinition
  #ModuleDefinition
  #LibraryDefinition
EndEnumeration

EnumerationBinary DefinitionFlags
  #IsImport
  #IsAbstract
  #IsBefore
  #IsAfter
  #IsAround
EndEnumeration

Structure Definition
  Name.s
  Scope.i
  Type.i
  Flags.i
EndStructure

Structure Scope
  Parent.i ; -1 is global scope.
  Definition.i ; -1 is global scope.
  Map Definitions.i()
EndStructure

Structure Code
  Array Scopes.Scope( 1 ) ; First one is always the global scope
  Array Definitions.Definition( 1 )
EndStructure

Global Code.Code

;==============================================================================
; Parser.
;;;;TODO: put this on a thread

Structure Parser
  *Position
  *EndPosition
EndStructure

Procedure.c ToLower( Character.c )
  ;;;;FIXME: Not Unicode...
  If Character >= 65 And Character <= 90
    ProcedureReturn 97 + ( Character - 65 )
  EndIf
  ProcedureReturn Character
EndProcedure

Procedure.i IsWhitespace( Character.c )
  Select Character
    Case #TAB, #SPACE, #NEWLINE, #RETURN
      ProcedureReturn #True
  EndSelect
  ProcedureReturn #False
EndProcedure

Procedure.i IsAlphanumeric( Character.c )
  ;;;;FIXME: Not Unicode...
  If Character >= 65 And Character <= 90
    ProcedureReturn #True
  EndIf
  If Character >= 97 And Character <= 122
    ProcedureReturn #True
  EndIf
  If Character >= 48 And Character <= 57
    ProcedureReturn #True
  EndIf
  ProcedureReturn #False
EndProcedure

Procedure SkipWhitespace( *Parser.Parser )
  While *Parser\Position < *Parser\EndPosition
    Define.b Char = PeekB( *Parser\Position )
    If Not IsWhitespace( Char )
      Break
    EndIf
    *Parser\Position + 1
  Wend
EndProcedure

Procedure.i MatchToken( *Parser.Parser, Token.s, Length.i )
  If *Parser\EndPosition - *Parser\Position < Length
    ProcedureReturn #False
  EndIf
  Define.i Index
  For Index = 0 To Length - 1
    Define.c Actual = PeekB( *Parser\Position + Index )
    Define.c Expected = PeekB( @Token + Index * SizeOf( Character ) )
    If ToLower( Actual ) <> Expected
      ProcedureReturn #False
    EndIf
  Next
  If *Parser\Position + Index < *Parser\EndPosition
    Define.c NextChar = PeekB( *Parser\Position + Index )
    If IsAlphanumeric( NextChar )
      ProcedureReturn #False
    EndIf
  EndIf
  *Parser\Position + Index
  ProcedureReturn #True
EndProcedure

; Returns a DefinitionFlag.
Procedure.i ParseModifier( *Parser.Parser )
  SkipWhitespace( *Parser )
  If MatchToken( *Parser, "abstract", 8 )
    ProcedureReturn #IsAbstract
  EndIf
  ProcedureReturn 0
EndProcedure

Procedure.i ParseDefinition( *Parser.Parser )
  ProcedureReturn #False
EndProcedure

Procedure.i ParseText()
  ProcedureReturn #False
EndProcedure

Prototype.i ParseFunction( *Parser.Parser )

Procedure TestParseText( Fn.ParseFunction, Text.s, Expected.i = #True )
  Define.Parser Parser
  Define *Buffer = UTF8( Text )
  Define.i Length = MemoryStringLength( *Buffer, #PB_UTF8 )
  Parser\Position = *Buffer
  Parser\EndPosition = *Buffer + Length
  Assert( Fn( @Parser ) = Expected )
  FreeMemory( *Buffer )
EndProcedure

ProcedureUnit CanParseModifier()
  TestParseText( @ParseModifier(), "abstract", #IsAbstract )
  TestParseText( @ParseModifier(), "ABSTRACT", #IsAbstract )
EndProcedureUnit

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
  
  ParseText()
  
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
; CursorPosition = 184
; FirstLine = 137
; Folding = --
; EnableXP