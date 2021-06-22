
EnableExplicit

Define.s UnityExecutablePath = "C:\Program Files\Unity\Hub\Editor\2020.3.5f1\Editor\Unity.exe"
Define.s GeneratedPathProject = "C:\Dropbox\Workspaces\UnityBasic_PB\UnityProject"
Define.s SourceProjectPath = "C:\Dropbox\Workspaces\UnityBasic_PB\TestProject"
Define.s TextFilePath = SourceProjectPath + "\TestFile.code"

#SPACE = 32
#NEWLINE = 10
#RETURN = 13
#EQUALS = 61
  
;==============================================================================
; Initialize.

InitScintilla()
InitNetwork()
ExamineDesktops()

Define.i Server = CreateNetworkServer( #PB_Any, 10978 )
If Server = 0
  Debug( "Cannot create server!!" )
  ;;;;TODO: handle error
EndIf

Define.i WindowWidth = DesktopWidth( 0 )
Define.i WindowHeight = DesktopHeight( 0 )

Define Window.i = OpenWindow( #PB_Any, 0, 0, WindowWidth, WindowHeight, "Unity Basic", #PB_Window_BorderLess )
Global Scintilla.i = ScintillaGadget( #PB_Any, 0, 0, WindowWidth, WindowHeight, 0 )

#WINDOW_SAVE_TIMER = 0
#WINDOW_NETWORK_TIMER = 1

AddWindowTimer( Window, #WINDOW_SAVE_TIMER, 2000 )
AddWindowTimer( Window, #WINDOW_NETWORK_TIMER, 500 )

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

Enumeration ClientStatus
  #WaitingForClientToConnect
  #WaitingForClientToBuild
  #WaitingForClientToRun
EndEnumeration

Global.i Unity = RunProgram( UnityExecutablePath, ~"-batchmode -projectPath \"" + GeneratedPathProject + ~"\" -executeMethod EditorTooling.Run", "", #PB_Program_Open | #PB_Program_Read )
Global.i UnityClient
Global.i UnityClientStatus = #WaitingForClientToConnect
Global *UnityClientNetworkBuffer = AllocateMemory( 65536 ) ; Max length of TCP message.

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
  Name.i
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
  IdentifierCount.i
  ScopeCount.i
  DefinitionCount.i
  Map IdentifierTable.i()
  Array Identifiers.s( 1 )
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
  NameBuffer.s
  NameBufferSize.i
  ;store failure state here; func to reset
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

Procedure.i IsAlpha( Character.c )
  ;;;;FIXME: Not Unicode...
  If Character >= 65 And Character <= 90
    ProcedureReturn #True
  EndIf
  If Character >= 97 And Character <= 122
    ProcedureReturn #True
  EndIf
  ProcedureReturn #False
EndProcedure

Procedure.i IsAlphanumeric( Character.c )
  ;;;;FIXME: Not Unicode...
  If Character >= 48 And Character <= 57
    ProcedureReturn #True
  EndIf
  ProcedureReturn IsAlpha( Character )
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

Procedure.i MatchToken( *Parser.Parser, Token.s, Length.i, AnyFollowing = #False )
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
  If Not AnyFollowing And *Parser\Position + Index < *Parser\EndPosition
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

Procedure.i ParseIdentifier( *Parser.Parser )
  SkipWhitespace( *Parser )
  
  ;;;;TODO: escaped identifiers
  ;;;;TODO: identifiers ending in !
  
  Define *StartPosition = *Parser\Position
  While *Parser\Position < *Parser\EndPosition
    Define.c Char = PeekB( *Parser\Position )
    If ( *StartPosition = *Parser\Position And Not IsAlpha( Char ) ) Or Not IsAlphanumeric( Char )
      Break
    EndIf
    *Parser\Position + 1
  Wend
  
  If *Parser\Position = *StartPosition
    ProcedureReturn -1
  EndIf
  
  Define.i Length = *Parser\Position - *StartPosition
  If *Parser\NameBufferSize < ( Length - 1 )
    *Parser\NameBufferSize = Length + 64
    *Parser\NameBuffer = Space( *Parser\NameBufferSize )
  EndIf
  
  Define.i Index
  Define *Buffer = @*Parser\NameBuffer
  For Index = 0 To Length - 1
    Define.c Char = ToLower( PeekB( *StartPosition + Index ) )
    PokeC( *Buffer + Index * SizeOf( Character ), Char )
  Next
  PokeC( *Buffer + Index * SizeOf( Character ), #NUL )
  
  Define *Element = FindMapElement( Code\IdentifierTable(), *Parser\NameBuffer )
  If *Element = #Null
    Define.s Name = Left( *Parser\NameBuffer, Length )
    If ArraySize( Code\Identifiers() ) = Code\IdentifierCount
      ReDim Code\Identifiers.s( Code\IdentifierCount + 512 )
    EndIf
    Define.i IdIndex = Code\IdentifierCount
    Code\Identifiers( IdIndex ) = Name
    Code\IdentifierCount + 1
    *Element = AddMapElement( Code\IdentifierTable(), *Parser\NameBuffer )
    PokeI( *Element, IdIndex )
  EndIf
  
  ProcedureReturn PeekI( *Element )
  
EndProcedure

; Returns index of definition or -1 on failure.
Procedure.i ParseDefinition( *Parser.Parser )
  
  ; Parse modifiers.
  Define.i Flags = 0
  
  ; Parse type.
  Define.i Type = 0
  SkipWhitespace( *Parser )
  If MatchToken( *Parser, "type", 4 )
    Type = #TypeDefinition
  Else
    ;;;;TODO
    ProcedureReturn -1
  EndIf
  
  ; Parse name.
  Define.i Name = ParseIdentifier( *Parser )
  If Name = -1
    ;;;;TODO
    ProcedureReturn -1
  EndIf
  
  ; Add definition.
  If ArraySize( Code\Definitions() ) = Code\DefinitionCount
    ReDim Code\Definitions.Definition( Code\DefinitionCount + 256 )
  EndIf
  Define.i DefinitionIndex = Code\DefinitionCount
  Code\Definitions( DefinitionIndex )\Flags = Flags
  Code\Definitions( DefinitionIndex )\Name = Name
  Code\Definitions( DefinitionIndex )\Type = Type
  Code\DefinitionCount + 1
  
  ; Parse parameters.
  
  ; Parse clauses.
  
  ; Parse body.
  SkipWhitespace( *Parser )
  If MatchToken( *Parser, ";", 1, #True )
  ElseIf MatchToken( *Parser, "{", 1, #True )
    ;;;;TODO
  Else
    ProcedureReturn -1
  EndIf  
  
  ProcedureReturn DefinitionIndex
  
EndProcedure

Procedure ParseText()
  
  ResetStructure( @Code, Code )
  
  Define.Parser Parser
  Parser\Position = *Text
  Parser\EndPosition = *Text + TextLength
  
  While Parser\Position < Parser\EndPosition
    If ParseDefinition( @Parser ) = -1
      ;;;;TODO: error handling (diagnose and keep going)
      Break
    EndIf
  Wend
  
  Debug( "Definitions: " + Str( Code\DefinitionCount ) )
  
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
  ResetStructure( @Code, Code )
  TestParseText( @ParseModifier(), "abstract", #IsAbstract )
  TestParseText( @ParseModifier(), "ABSTRACT", #IsAbstract )
EndProcedureUnit

ProcedureUnit CanParseIdentifier()
  ResetStructure( @Code, Code )
  TestParseText( @ParseIdentifier(), "Foobar", 0 )
  Assert( Code\IdentifierCount = 1 )
  Assert( FindMapElement( Code\IdentifierTable(), "foobar" ) <> #Null )
  Assert( PeekI( FindMapElement( Code\IdentifierTable(), "foobar" ) ) = 0 )
  TestParseText( @ParseIdentifier(), "FOOBAR", 0 )
  Assert( Code\IdentifierCount = 1 )
  Assert( FindMapElement( Code\IdentifierTable(), "foobar" ) <> #Null )
  Assert( PeekI( FindMapElement( Code\IdentifierTable(), "foobar" ) ) = 0 )
EndProcedureUnit

ProcedureUnit CanParseSimpleTypeDefinition()
  ResetStructure( @Code, Code )
  TestParseText( @ParseDefinition(), "type First;", 0 )
  Assert( Code\DefinitionCount = 1 )
  Assert( Code\IdentifierCount = 1 )
  Assert( Code\Identifiers( 0 ) = "first" )
  Assert( Code\Definitions( 0 )\Name = 0 )
  Assert( Code\Definitions( 0 )\Type = #TypeDefinition )
  Assert( Code\Definitions( 0 )\Flags = 0 )
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

; Sends some text to the Unity subprocess.
Procedure SendString( String.s )
  
  CompilerIf #False
  Define.i Length = StringByteLength( String, #PB_UTF8 )
  Define *Buffer = AllocateMemory( Length + SizeOf( Word ) )
  
  PokeW( *Buffer, Length )
  PokeS( *Buffer + SizeOf( Word ), String, Length, #PB_UTF8 | #PB_String_NoZero )
  
  SendNetworkData( UnityClient, *Buffer, Length + SizeOf( Word ) )
  CompilerEndIf
  
  Define.i Length = StringByteLength( String, #PB_UTF8 )
  Define *Buffer = AllocateMemory( Length )
  
  PokeS( *Buffer, String, Length, #PB_UTF8 | #PB_String_NoZero )
  
  SendNetworkData( UnityClient, *Buffer, Length )
  
  FreeMemory( *Buffer )
  
EndProcedure

;==============================================================================
; Main loop.

ParseText()

Repeat
  
  Define Event = WaitWindowEvent()
  
  If Event = #PB_Event_Timer
    Select EventTimer()
        Case #WINDOW_SAVE_TIMER
          FlushText()
          
        Case #WINDOW_NETWORK_TIMER
          Select NetworkServerEvent( Server )
            Case #PB_NetworkEvent_Connect
              Debug "Unity connected"
              UnityClient = EventClient()
              SendString( "build" )
              UnityClientStatus = #WaitingForClientToBuild
              
            Case #PB_NetworkEvent_Disconnect
              Debug "Unity disconnected"
              UnityClient = 0
              
            Case #PB_NetworkEvent_Data
              Debug "Unity data received"
              Define.i ReadResult = ReceiveNetworkData( EventClient(), *UnityClientNetworkBuffer, 65536 )
              If ReadResult <= 0
                Debug "Read failure!!"
              Else
                Define.s Text = PeekS( *UnityClientNetworkBuffer, ReadResult, #PB_UTF8 | #PB_ByteLength )
                Debug "Data " + Text
              EndIf
          EndSelect
      EndSelect
  EndIf
  
Until Event = #PB_Event_CloseWindow

FlushText()
If Unity <> 0
  KillProgram( Unity ) ;;;;TODO: shut down Unity more elegantly...
  CloseProgram( Unity )
EndIf

;[X] Full-screen text view
;[X] Text is saved and loaded
;[X] Type definition is parsed
;[X] Unity is launched in background
;[X] Unity connects over network
;[X] Unity player is built
;[ ] Unity player is executed and window embedded
;[ ] Output code is generated


; What I want
; - All tests are being run continuously on all connected players (smart execution to narrow down run sets)
; - Everything is saved automatically
; - Changes are picked up automatically and reload the application automatically to a known state and from there, continue with the changes applied
; - The toolchain is configured entirely from within annotations in the code

; IDE Options = PureBasic 5.73 LTS (Windows - x64)
; CursorPosition = 454
; FirstLine = 406
; Folding = ---
; EnableXP