
EnableExplicit

Define.s UnityEditorExecutablePath = "C:\Program Files\Unity\Hub\Editor\2020.3.5f1\Editor\Unity.exe"
Define.s UnityPlayerExecutablePath = "C:\Dropbox\Workspaces\UnityBasic_PB\UnityProject\Builds\UnityBasic64.exe"
Define.s GeneratedProjectPath = "C:\Dropbox\Workspaces\UnityBasic_PB\UnityProject"
Define.s GeneratedDocsPath = "C:\Dropbox\Workspaces\UnityBasic_PB\Docs"
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

Define.i WindowWidth = DesktopUnscaledX( DesktopWidth( 0 ) )
Define.i WindowHeight = DesktopUnscaledY( DesktopHeight( 0 ) )

Define.i Window = OpenWindow( #PB_Any, 0, 0, WindowWidth, WindowHeight, "Unity Basic", #PB_Window_BorderLess | #PB_Window_ScreenCentered )

SetWindowTitle( Window, "UnityBasic" )
Global.i StatusBar = CreateStatusBar( #PB_Any, WindowID( Window ) )
AddStatusBarField( WindowWidth * 0.05 )
AddStatusBarField( WindowWidth * 0.70 )
AddStatusBarField( WindowWidth * 0.25 )

Define.i ContentWidth = WindowWidth
Define.i ContentHeight = WindowHeight - StatusBarHeight( StatusBar )

Global.i Scintilla = ScintillaGadget( #PB_Any, 0, 0, ContentWidth / 2, ContentHeight, 0 )
Define.i DocViewer = WebGadget( #PB_Any, ContentWidth / 2, 0, ContentWidth / 2, ContentHeight / 2, "file:///" + ReplaceString( GeneratedDocsPath, "\", "/" ) + "/index.html" )
Define.i PlayerContainer = ContainerGadget( #PB_Any, ContentWidth / 2, ContentHeight / 2, ContentWidth / 2 , ContentHeight / 2 )
;Global Scintilla.i = ScintillaGadget( #PB_Any, 0, 0, 0, 0, 0 )
;Define.i DocViewer = WebGadget( #PB_Any, 0, 0, 0, 0, "https://unity3d.com" )
;Define.i PlayerContainer = ContainerGadget( #PB_Any, 0, 0, 0, 0 )
;Define.i HorizontalSplitter = SplitterGadget( #PB_Any, 0, 0, 0, 0, DocViewer, PlayerContainer )
;Define.i VerticalSplitter = SplitterGadget( #PB_Any, 0, 0, WindowWidth, WindowHeight, Scintilla, HorizontalSplitter, #PB_Splitter_Vertical )


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
  #WaitingForClientIdle
EndEnumeration

Global.i UnityEditor = RunProgram( UnityEditorExecutablePath, ~"-batchmode -projectPath \"" + GeneratedProjectPath + ~"\" -executeMethod EditorTooling.Run", "", #PB_Program_Open | #PB_Program_Read )
Global.i UnityPlayer
Global.i UnityClient
Global.i UnityClientStatus = #WaitingForClientToConnect
Global *UnityClientNetworkBuffer = AllocateMemory( 65536 ) ; Max length of TCP message.

;==============================================================================

Structure TextRegion
  LeftPos.i
  RightPos.i
EndStructure

Enumeration AnnotationKind
  #DescriptionAnnotation
  #AssetAnnotation
EndEnumeration

; Annotations are free-form text blobs that can be attached to definitions.
; They are used as instructions for the tooling.
Structure Annotation
  AnnotationKind.i
  AnnotationText.s
  NextAnnotation.i
EndStructure

Enumeration ClauseKind
  #PreconditionClause
  #PostconditionClause
EndEnumeration

; Clauses are optional forms that can be tagged onto definitions.
; They are used for things such as pre- and postconditions.
Structure Clause
  NextClause.i
EndStructure

CompilerIf #False
Enumeration TypeKind
  #NamedType  
  #PrimitiveType
  #DerivedType
  #UnionType
  #IntersectionType
  #InstancedType
  #TupleType
EndEnumeration
CompilerEndIf

Enumeration Operator
  #LiteralExpression
  #LogicalAndExpression
  #LogicalOrExpression
  #LogicalNotExpression
  #BitwiseAndExpression
  #BitwiseOrExpression
  #CallExpression
  #TupleExpression
EndEnumeration

; Type and value expressions use the same data format.
Structure Expression
  Operator.i
  Type.i
  Region.TextRegion
  FirstOperand.i
  SecondOperand.i
EndStructure

Enumeration StatementKind
  #ExpressionStatement
  #ReturnStatement
  #YieldStatement
  #LoopStatement
  #BreakStatement
  #ContinueStatement
  #IfStatement
  #SwitchStatement
EndEnumeration

Structure Statement
  StatementKind.i
  NextStatement.i
EndStructure

Enumeration DefinitionType
  #TypeDefinition
  #MethodDefinition
  #FieldDefinition
  #ModuleDefinition
  #LibraryDefinition
  #FeatureDefinition
  #ProgramDefinition
EndEnumeration

EnumerationBinary DefinitionFlags
  #IsImport
  #IsAbstract
  #IsBefore
  #IsAfter
  #IsAround
  #IsImmutable
  #IsMutable
  #IsSingleton ; 'object' in code
  #IsExtend
EndEnumeration

Structure Definition
  Name.i
  Scope.i
  Type.i
  Flags.i
  InnerScope.i
  Region.TextRegion
  NextDefinitionInScope.i
  FirstAnnotation.i ; -1 if none.
  FirstClause.i ; -1 if none.
EndStructure

Structure Scope
  Parent.i ; -1 is global scope.
  Definition.i ; -1 is global scope.
  FirstDefinition.i
EndStructure

Structure Diagnostic
EndStructure

; Just a bunch of arrays that contain a completely flattened representation of source code.
; No explicit tree structure.
Structure Code
  IdentifierCount.i
  ScopeCount.i
  DefinitionCount.i
  StatementCount.i
  ExpressionCount.i
  AnnotationCount.i
  ClauseCount.i
  DiagnosticCount.i
  ErrorCount.i
  WarningCount.i
  Map IdentifierTable.i()
  Array Identifiers.s( 0 )
  Array Scopes.Scope( 1 ) ; First one is always the global scope
  Array Definitions.Definition( 0 )
  Array Statements.Statement( 0 )
  Array Expressions.Expression( 0 )
  Array Annotations.Annotation( 0 )
  Array Clauses.Clause( 0 )
  Array Diagnostics.Diagnostic( 0 )
EndStructure

Global Code.Code

Procedure ResetCode()
  If Scintilla <> 0
    ScintillaSendMessage( Scintilla, #SCI_ANNOTATIONCLEARALL )
  EndIf
  ResetStructure( @Code, Code )
  Code\ScopeCount = 1
  Code\Scopes( 1 )\Parent = -1
  Code\Scopes( 1 )\Definition = -1
EndProcedure

;==============================================================================
; Parser.
;;;;TODO: put this on a thread

Structure Parser
  *Position
  *EndPosition
  NameBuffer.s
  NameBufferSize.i
  CurrentScope.i
  CurrentDefinitionInScope.i
  ;store failure state here; func to reset
EndStructure

Macro PushScope( Parser )
  Define.i PreviousScope = Parser\CurrentScope
  Define.i PreviousDefinitionInScope = Parser\CurrentDefinitionInScope
  ;;TODO
EndMacro

Macro PopScope( Parser )
  Parser\CurrentScope = PreviousScope
  Parser\CurrentDefinitionInScope = PreviousDefinionInScope
EndMacro

Procedure.c ToLower( Character.c )
  ;;;;FIXME: Not Unicode...
  If Character >= 65 And Character <= 90
    ProcedureReturn 97 + ( Character - 65 )
  EndIf
  ProcedureReturn Character
EndProcedure

Procedure.b IsUpper( Character.c )
  ;;;;FIXME: Not Unicode...
  If Character >= 65 And Character <= 90
    ProcedureReturn #True
  EndIf
  ProcedureReturn #False
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
    If ( *StartPosition = *Parser\Position And Not IsAlpha( Char ) And Char <> '_' ) Or ( Not IsAlphanumeric( Char ) And Char <> '_' )
      Break
    EndIf
    *Parser\Position + 1
  Wend
  
  If *Parser\Position = *StartPosition
    ProcedureReturn -1
  EndIf
  
  Define.i Length = *Parser\Position - *StartPosition
  If *Parser\NameBufferSize < Length * 2
    *Parser\NameBufferSize = Length * 2
    *Parser\NameBuffer = Space( *Parser\NameBufferSize )
  EndIf
  
  ;;;;TODO: support stripping prefixes and suffixes in canonicalization (problem: probably wants to be specific to a definition kind)
  ; Collect the identifier and canonicalize it at the same time (i.e. do away with
  ; naming conventions such that we can later print the identifier in whatever naming
  ; convention the user prefers; forestalls any but_I_like_my_identifiers_like_this).
  Define.i ReadIndex = 0
  Define.i WriteIndex = 0
  Define.i WordLength = 0
  Define.b LastWasUpperOrUnderscore = #False
  Define *Buffer = @*Parser\NameBuffer
  While ReadIndex < Length
    
    Define.c Char = PeekB( *StartPosition + ReadIndex )
    
    ; Figure out whether it's a separator.
    If IsUpper( Char ) Or Char = '_'
      If WordLength > 0 And Not LastWasUpperOrUnderscore
        PokeC( *Buffer + WriteIndex * SizeOf( Character ), '_' )
        WriteIndex + 1
      EndIf
      Char = ToLower( Char )
      WordLength = 0
      LastWasUpperOrUnderscore = #True
    Else
      LastWasUpperOrUnderscore = #False
    EndIf
    
    PokeC( *Buffer + WriteIndex * SizeOf( Character ), Char )
    WordLength + 1
    WriteIndex + 1
    ReadIndex + 1
    
  Wend
  PokeC( *Buffer + WriteIndex * SizeOf( Character ), #NUL )
  
  Define *Element = FindMapElement( Code\IdentifierTable(), *Parser\NameBuffer )
  If *Element = #Null
    Define.s Name = Left( *Parser\NameBuffer, WriteIndex )
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
  
  ; Parse annotations.
  
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
  Define *Definition.Definition = @Code\Definitions( DefinitionIndex )
  *Definition\Flags = Flags
  *Definition\Name = Name
  *Definition\Type = Type
  *Definition\NextDefinitionInScope = -1
  *Definition\InnerScope = -1
  *Definition\FirstAnnotation = -1
  *Definition\FirstClause = -1
  Code\DefinitionCount + 1
  
  ; Add to scope.
  If *Parser\CurrentDefinitionInScope <> -1
    Code\Definitions( *Parser\CurrentDefinitionInScope )\NextDefinitionInScope = DefinitionIndex
  Else
    Code\Scopes( *Parser\CurrentScope )\FirstDefinition = DefinitionIndex
  EndIf
  *Parser\CurrentDefinitionInScope = DefinitionIndex
  
  ; Parse type parameters.
  
  ; Parse value parameters.
  
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
  
  ResetCode()
  
  Define.Parser Parser
  Parser\Position = *Text
  Parser\EndPosition = *Text + TextLength
  Parser\CurrentDefinitionInScope = -1
  
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
  Parser\CurrentDefinitionInScope = -1
  Assert( Fn( @Parser ) = Expected )
  FreeMemory( *Buffer )
EndProcedure

ProcedureUnit CanParseModifier()
  ResetCode()
  TestParseText( @ParseModifier(), "abstract", #IsAbstract )
  TestParseText( @ParseModifier(), "ABSTRACT", #IsAbstract )
EndProcedureUnit

ProcedureUnit CanParseIdentifier()
  ResetCode()
  TestParseText( @ParseIdentifier(), "Foobar", 0 )
  Assert( Code\IdentifierCount = 1 )
  Assert( FindMapElement( Code\IdentifierTable(), "foobar" ) <> #Null )
  Assert( PeekI( FindMapElement( Code\IdentifierTable(), "foobar" ) ) = 0 )
  ResetCode()
  TestParseText( @ParseIdentifier(), "FOOBAR", 0 )
  Assert( Code\IdentifierCount = 1 )
  Assert( FindMapElement( Code\IdentifierTable(), "foobar" ) <> #Null )
  Assert( PeekI( FindMapElement( Code\IdentifierTable(), "foobar" ) ) = 0 )
  ResetCode()
  TestParseText( @ParseIdentifier(), "FooBar", 0 )
  Assert( Code\IdentifierCount = 1 )
  Assert( FindMapElement( Code\IdentifierTable(), "foo_bar" ) <> #Null )
  Assert( PeekI( FindMapElement( Code\IdentifierTable(), "foo_bar" ) ) = 0 )
EndProcedureUnit

ProcedureUnit CanParseSimpleTypeDefinition()
  ResetCode()
  TestParseText( @ParseDefinition(), "type First;", 0 )
  Assert( Code\DefinitionCount = 1 )
  Assert( Code\IdentifierCount = 1 )
  Assert( Code\Identifiers( 0 ) = "first" )
  Assert( Code\Definitions( 0 )\Name = 0 )
  Assert( Code\Definitions( 0 )\Type = #TypeDefinition )
  Assert( Code\Definitions( 0 )\Flags = 0 )
EndProcedureUnit

;==============================================================================
; Code generation.
;;;;TODO: put this stuff on a thread

Structure Asset
EndStructure

Structure Field
  Name.s
EndStructure

; Type codes are type indices offset by the number of built-in types.
Enumeration BuiltinType
  #Int32Type
  #Int64Type
  #Float32Type
  #Float64Type
EndEnumeration

EnumerationBinary TypeFlags
  #TypeIsArray
EndEnumeration

; No inheritance/derivation.
Structure Type
  Name.s
  SizeInBytes.i
  Flags.i
EndStructure

Enumeration InstructionCode
  #CallInsn
  #BranchInsn
  #JumpInsn
  #AddInsn
  #SubtractInsn
  #MultiplyInsn
  #DivideInsn
  #LoadInsn
  #StoreInsn ; Void value instruction.
EndEnumeration

EnumerationBinary InstructionFlags
  #InsnIsInteger
  #InsnIsFloat
  #InsnIsWide ; Makes int or float 64bit.
  #InsnIsVectored
EndEnumeration

; Instructions are fixed-width SSA.
Structure Instruction
EndStructure

; All functions are fully symmetric, i.e. 1 argument value, 1 result value.
Structure Function
  Name.s
  ArgumentType.i
  ResultType.i
  FirstInstruction.i
  InstructionCount.i
EndStructure

Structure Object
  Type.i
  ConstructorFunction.i
EndStructure

Structure Program
  AssetCount.i
  FunctionCount.i
  TypeCount.i
  ObjectCount.i
  InstructionCount.i
  Array Assets.Asset( 1 )
  Array Functions.Function( 1 )
  Array Types.Type( 1 )
  Array Objects.Object( 1 )
  Array Instructions.Instruction( 1 )
EndStructure

; For now, we only support a single program in source.
Global Program.Program

Structure GenType
EndStructure

Structure GenField
EndStructure

Structure GenFunction
EndStructure

Structure GenObject
EndStructure

;;;;TODO: would be more efficient to fold this into the parsing pass
Procedure Collect( Map Types.GenType(), Map Fields.GenField(), Map Functions.GenFunction(), Map Objects.GenObject() )
  
  Define.i DefinitionIndex
  For DefinitionIndex = 0 To Code\DefinitionCount - 1
    
    Define *Definition.Definition = @Code\Definitions( DefinitionIndex )
    
    ;;;;TODO: probably need to ultimately mangle names here
    Define.s Name = Code\Identifiers( *Definition\Name )
    
    Select *Definition\Type
        
      Case #TypeDefinition
        
        If FindMapElement( Types(), Name ) <> #Null
          ;;;;TODO: add diagnostic
          Debug "Type already defined!"
          Continue
        EndIf
        
        Define *GenType.GenType = AddMapElement( Types(), Name )
        
    EndSelect
    
  Next
  
EndProcedure

Procedure GenTypes( Map Types.GenType(), Map Fields.GenField() )
  
  ;;;;TODO: typecheck....
  
  ForEach Types()
    
    Define.GenType *GenType = Types()
    
    If ArraySize( Program\Types() ) = Program\TypeCount
      ReDim Program\Types( Program\TypeCount + 512 )
    EndIf
    Define.i TypeIndex = Program\TypeCount
    Define.Type *Type = @Program\Types( TypeIndex )
    Program\TypeCount + 1
    
    *Type\Name = MapKey( Types() )
    
  Next
  
EndProcedure

Procedure GenFunctions()
EndProcedure

Procedure GenObjects()
EndProcedure

Procedure GenAssets()
EndProcedure

; Translate `Code` into `Program`.
Procedure TranslateProgram()
  
  ResetStructure( @Program, Program )
  
  NewMap Types.GenType()
  NewMap Fields.GenField()
  NewMap Functions.GenFunction()
  NewMap Objects.GenObject()
  
  Collect( Types(), Fields(), Functions(), Objects() )
  
  GenTypes( Types(), Fields() )
  GenFunctions()
  GenObjects()
  GenAssets()
  
EndProcedure

Procedure SendProgram()
EndProcedure

Procedure UpdateProgram()
  ParseText()
  ;;;;TODO: update annotations
  If Code\ErrorCount > 0
    ProcedureReturn
  EndIf
  TranslateProgram()
  If Code\ErrorCount > 0
    ProcedureReturn
  EndIf
  SendProgram()
EndProcedure

ProcedureUnit CanCompileSimpleProgram()
  Define.s Text = ~"type FirstType;\n" +
                  ~"type SecondType;\n"
  *Text = UTF8( Text )
  TextLength = Len( Text )
  UpdateProgram()
  Assert( Program\TypeCount = 2 )
  Assert( Program\Types( 0 )\Name = "first_type" )
  Assert( Program\Types( 1 )\Name = "second_type" )
  FreeMemory( *Text )
EndProcedureUnit

;==============================================================================

Procedure Status( Message.s )
  StatusBarText( StatusBar, 1, Message )  
EndProcedure

Procedure FlushText( Recompile.b = #True )
  
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
  
  If Recompile
    UpdateProgram()
  EndIf
  
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

UpdateProgram()
Status( "Waiting for Unity to connect..." )

Repeat
  
  Define Event = WaitWindowEvent()
  
  If Event = #PB_Event_Timer
    Select EventTimer()
        Case #WINDOW_SAVE_TIMER
          FlushText()
          
        Case #WINDOW_NETWORK_TIMER
          Select NetworkServerEvent( Server )
            Case #PB_NetworkEvent_Connect
              Status( "Waiting for Unity to build player..." )
              UnityClient = EventClient()
              SendString( "build" )
              UnityClientStatus = #WaitingForClientToBuild
              
            Case #PB_NetworkEvent_Disconnect
              Status( "Unity disconnected." )
              UnityClient = 0
              
            Case #PB_NetworkEvent_Data
              Define.i ReadResult = ReceiveNetworkData( EventClient(), *UnityClientNetworkBuffer, 65536 )
              If ReadResult <= 0
                Debug "Read failure!!"
              Else
                Define.s Text = PeekS( *UnityClientNetworkBuffer, ReadResult, #PB_UTF8 | #PB_ByteLength )
                Debug "Data " + Text
                Select UnityClientStatus
                    
                  Case #WaitingForClientToBuild
                    UnityClientStatus = #WaitingForClientIdle
                    If Text = "build failure"
                      ;;;;TODO: handle failure
                      Status( "Build failed!" )
                    Else
                      Status( "Starting player..." )
                      Define.i HWND = GadgetID( PlayerContainer )
                      UnityPlayer = RunProgram( UnityPlayerExecutablePath, "-parentHWND " + Str( HWND ), "", #PB_Program_Open | #PB_Program_Read )
                    EndIf
                    
                EndSelect
              EndIf
          EndSelect
      EndSelect
  EndIf
  
Until Event = #PB_Event_CloseWindow

Status( "Exiting." )
FlushText( #False )
;;;;TODO: shut down Unity more elegantly...
If UnityEditor <> 0
  KillProgram( UnityEditor )
  CloseProgram( UnityEditor )
EndIf
If UnityPlayer <> 0
  KillProgram( UnityPlayer )
  CloseProgram( UnityPlayer )
EndIf

;[X] Full-screen text view
;[X] Text is saved and loaded
;[X] Type definition is parsed
;[X] Unity is launched in backgroundz
;[X] Unity connects over network
;[X] Unity player is built
;[X] Unity player is executed
;[X] Unity player window is embedded into IDE window
;[ ] Program is generated
;[ ] Program is transferred to player
;[ ] Program is being run
;[ ] Program is migrated from one run to the next
;[ ] Can add asset to project
;[ ] Assets are being built and rebuilt into asset bundles
;[ ] Player can load asset bundles
;[ ] Can add&render display element (sprite or model)
;[ ] Can animate display element in code

; ....

;[ ] Diagnostics are shown as annotations on code
;[ ] Can generate docs from code
;[ ] Can run tests from code in player
;[ ] Syntax highlighting in text editor
;[ ] Auto-completion in text editor

;[ ] Vim mode

; What I want
; - All tests are being run continuously on all connected players (smart execution to narrow down run sets)
; - Everything is saved automatically
; - Changes are picked up automatically and reload the application automatically to a known state and from there, continue with the changes applied
; - The toolchain is configured entirely from within annotations in the code


; Qs:
; - How do I not make it depend on total program size but rather on change size?
; - How would scenes be created in a graphical way?
; - Where do we display log and debug output?
; IDE Options = PureBasic 5.73 LTS (Windows - x64)
; CursorPosition = 547
; FirstLine = 512
; Folding = -----
; EnableXP