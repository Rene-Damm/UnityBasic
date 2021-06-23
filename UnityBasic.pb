
EnableExplicit

IncludePath "GoScintilla/"
XIncludeFile "GoScintilla.pbi"

#SOURCE_FILE_EXTENSION = ".ubasic"

Global.s UnityEditorExecutablePath = "C:\Program Files\Unity\Hub\Editor\2020.3.5f1\Editor\Unity.exe"
Global.s UnityPlayerExecutablePath = "C:\Dropbox\Workspaces\UnityBasic_PB\UnityProject\Builds\UnityBasic64.exe"
Global.s GeneratedProjectPath = "C:\Dropbox\Workspaces\UnityBasic_PB\UnityProject"
Global.s GeneratedDocsPath = "C:\Dropbox\Workspaces\UnityBasic_PB\Docs"
Global.s SourceProjectPath = "C:\Dropbox\Workspaces\UnityBasic_PB\TestProject"
Global.s LibrariesPath = "C:\Dropbox\Workspaces\UnityBasic_PB\Libs"
Global.s TextFilePath = SourceProjectPath + "\TestFile" + #SOURCE_FILE_EXTENSION

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

Global.i Scintilla = GOSCI_Create( #PB_Any, 0, 0, ContentWidth / 2, ContentHeight, 0, #GOSCI_AUTOSIZELINENUMBERSMARGIN )
;;;;TODO: use navigation callback or popup blocker to navigate from docs to code (put links in generated doc)
Global.i DocViewer = WebGadget( #PB_Any, ContentWidth / 2, 0, ContentWidth / 2, ContentHeight / 2, "file:///" + ReplaceString( GeneratedDocsPath, "\", "/" ) + "/index.html" )
Global.i PlayerContainer = ContainerGadget( #PB_Any, ContentWidth / 2, ContentHeight / 2, ContentWidth / 2 , ContentHeight / 2 )

;;;;TODO: layout using splitters instead of fixed proportions
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

GOSCI_SetAttribute( Scintilla, #GOSCI_LINENUMBERAUTOSIZEPADDING, 10 )
GOSCI_SetMarginWidth( Scintilla, #GOSCI_MARGINFOLDINGSYMBOLS, 24 )
GOSCI_SetColor( Scintilla, #GOSCI_CARETLINEBACKCOLOR, $B4FFFF )
GOSCI_SetFont( Scintilla, "Consolas", 16 )
GOSCI_SetTabs( Scintilla, 4, 1 )

Enumeration Styles
  #StyleKeyword = 1
  #StyleComment
  #StyleString
  #StyleNumber
  #StyleFunction
  #StyleType
  #StyleAnnotation
EndEnumeration

GOSCI_SetStyleFont( Scintilla, #StyleKeyword, "", -1, #PB_Font_Bold )
GOSCI_SetStyleColors( Scintilla, #StyleKeyword, $800000 )

GOSCI_SetStyleFont( Scintilla, #StyleAnnotation, "", -1, #PB_Font_Italic )
GOSCI_SetStyleColors( Scintilla, #StyleAnnotation, $006400 )

GOSCI_SetStyleFont( Scintilla, #StyleComment, "", -1, #PB_Font_Italic )
GOSCI_SetStyleColors( Scintilla, #StyleComment, $006400 )

GOSCI_SetStyleColors( Scintilla, #StyleString, #Gray )

GOSCI_SetStyleColors( Scintilla, #StyleFunction, #Blue )

GOSCI_AddDelimiter( Scintilla, "//", "", #GOSCI_DELIMITTOENDOFLINE, #StyleComment )
GOSCI_AddDelimiter( Scintilla, "/*", "*/", #GOSCI_DELIMITTOENDOFLINE, #StyleComment )
GOSCI_AddDelimiter( Scintilla, ~"\"", ~"\"", #GOSCI_DELIMITBETWEEN, #StyleString )

GOSCI_AddKeywords( Scintilla, "TYPE METHOD FIELD OBJECT PROGRAM BEGIN END RETURN ABSTRACT IMMUTABLE MUTABLE", #StyleKeyword )
GOSCI_AddKeywords( Scintilla, "|DESCRIPTION |DETAILS |COMPANY |PRODUCT |CATEGORY", #StyleAnnotation )

GOSCI_SetLexerOption( Scintilla, #GOSCI_LEXEROPTION_SEPARATORSYMBOLS, @"=+-*/%()[],.;" )

SetActiveGadget( Scintilla )

;==============================================================================
; Networking and child processes.

#MAX_MESSAGE_LENGTH = ( 32 * 1024 )

Enumeration Status
  #WaitingForEditorToConnect
  #WaitingForEditorToBuildPlayer
  #WaitingForEditorToBuildAssetBundles
  #WaitingForPlayerToConnect
  #ReadyState
  #BadState ; States we can't yet recover from.
EndEnumeration

Structure UnityProjectSettings
  ProductName.s
  CompanyName.s
EndStructure

Global.i UnityEditor = RunProgram( UnityEditorExecutablePath, ~"-batchmode -projectPath \"" + GeneratedProjectPath + ~"\" -executeMethod EditorTooling.Run", "", #PB_Program_Open | #PB_Program_Read )
Global.i UnityPlayer
Global.i UnityEditorClient
Global.i UnityPlayerClient
Global.i UnityStatus = #WaitingForEditorToConnect
Global *UnityNetworkBuffer = AllocateMemory( #MAX_MESSAGE_LENGTH )
Global *UnityNetworkBufferPos
Global.i UnityBatchSendClient
Global.UnityProjectSettings UnityProjectSettings

; Sends some text to a Unity subprocess.
Procedure SendString( Client.i, String.s )
    
  Define.i Length = StringByteLength( String, #PB_UTF8 )
  Define *Buffer = AllocateMemory( Length + 1 )
  
  PokeS( *Buffer, String, Length, #PB_UTF8 )
  
  SendNetworkData( Client, *Buffer, Length + 1 )
  
  FreeMemory( *Buffer )
  
EndProcedure

;;;;REVIEW: seems like we may not even need this and PB is doing this under the hood for us
Procedure StartBatchSend( Client.i )
  *UnityNetworkBufferPos = *UnityNetworkBuffer
  UnityBatchSendClient = Client
EndProcedure

Procedure FlushBatch()
  
  Define.i Length = *UnityNetworkBufferPos - *UnityNetworkBuffer
  If Length > 0
    SendNetworkData( UnityBatchSendClient, *UnityNetworkBuffer, Length )
  EndIf
  
  *UnityNetworkBufferPos = *UnityNetworkBuffer
  
EndProcedure

Procedure BatchSendString( String.s )
  
  Define.i Length = StringByteLength( String, #PB_UTF8 )
  Define.i Available = *UnityNetworkBufferPos - *UnityNetworkBuffer
  If Available < Length + 1 Or Available + Length + 1 > #MAX_MESSAGE_LENGTH
    FlushBatch()
  EndIf
  
  PokeS( *UnityNetworkBufferPos, String, Length, #PB_UTF8 )
  *UnityNetworkBufferPos + Length + 1
  
EndProcedure

Procedure FinishBatchSend()
  FlushBatch()
  UnityBatchSendClient = 0
EndProcedure

;==============================================================================

Structure TextRegion
  LeftPos.i
  RightPos.i
EndStructure

;;;;REVIEW: allow specifying annotations that only relate to a certain build platform?
Enumeration AnnotationKind
  
  ;;;;REVIEW: rename "DESCRIPTION" to "SUMMARY"??
  
  ; Doc annotations.
  #DescriptionAnnotation
  #DetailsAnnotation
  #ExampleAnnotation
  #CategoryAnnotation
  #ReturnsAnnotation
  #ArgumentAnnotation
  #TypeArgumentAnnotation
  
  ; Test annotations.
  #TestAnnotation
  
  ; Asset annotations.
  #AssetAnnotation
  
  ; Program annotations.
  #CompanyAnnotation
  #ProductAnnotation
  #AssetServerAnnotation
  
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
  #InvariantClause
  #WhereClause
EndEnumeration

; Clauses are optional forms that can be tagged onto definitions.
; They are used for things such as pre- and postconditions.
Structure Clause
  Expression.i
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
  #DependentType
EndEnumeration
CompilerEndIf

; -1 is invalid index. Further negative values are "built-in" types, i.e. types for
; which literal values exist. Note that Boolean isn't among them -- True and False are singletons.
Enumeration BuiltinType
  #IntegerType = -2
  #FloatType = -3
  #ImmutableStringType = -4
  #MutableStringType = -5
EndEnumeration

Enumeration Operator
  #LiteralExpression
  #NameExpression
  #LogicalAndExpression
  #LogicalOrExpression
  #LogicalNotExpression
  #BitwiseAndExpression
  #BitwiseOrExpression
  #ApplyExpression ; For types, this instantiates the given named type.
  #TupleExpression
EndEnumeration

; Type and value expressions use the same data format.
; All expressions are either unary or binary.
Structure Expression
  Operator.i
  Type.i
  Region.TextRegion
  StructureUnion
    FirstOperandI.i
    FirstOperandD.d
  EndStructureUnion
  SecondOperandI.i
  NextExpression.i
EndStructure

Enumeration StatementKind
  #ExpressionStatement
  #ReturnStatement
  #YieldStatement
  #LoopStatement
  #BreakStatement
  #ContinueStatement
  #IfStatement
  #ElseIfStatement
  #ElseStatement
  #SwitchStatement
  #DefinitionStatement
EndEnumeration

Structure Statement
  StatementKind.i
  ReferencedIndex.i ; Depends on the kind of statement which array this refers to.
  InnerScope.i
  NextStatement.i
EndStructure

Enumeration DefinitionKind
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
  #IsReplace
  #IsIterator
EndEnumeration

Structure Parameter
  Name.i
  Type.i
  DefaultExpression.i ; -1 if none.
  NextParameter.i
EndStructure

Structure Definition
  Name.i
  Scope.i
  DefinitionKind.i
  Flags.i
  Type.i
  InnerScope.i
  Region.TextRegion
  NextDefinition.i
  FirstAnnotation.i ; -1 if none.
  FirstClause.i     ; -1 if none.
  FirstTypeParameter.i ; -1 if none.
  FirstValueParameter.i ; -1 if none.
EndStructure

Structure Scope
  Parent.i ; -1 is global scope.
  Definition.i ; -1 is global scope.
  FirstDefinitionOrStatement.i ; Whether definition or statement depends on the type of scope.
EndStructure

Enumeration DiagnosticCode
  
  ; Syntax errors.
  
EndEnumeration

Structure Diagnostic
  Code.i
  Index.i ; Array is determined automatically from what type of diagnostic it is (based on `Code`).
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
  ParameterCount.i
  DiagnosticCount.i
  ErrorCount.i
  WarningCount.i
  Map IdentifierTable.i()
  Map StringLiterals.i()
  Array Identifiers.s( 0 )
  Array Scopes.Scope( 1 ) ; First one is always the global scope
  Array Definitions.Definition( 0 )
  Array Statements.Statement( 0 )
  Array Expressions.Expression( 0 )
  Array Annotations.Annotation( 0 )
  Array Clauses.Clause( 0 )
  Array Parameters.Parameter( 0 )
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

Procedure.s FormatDiagnostic( Index.i )
EndProcedure

Procedure Diagnose( DiagnosticCode.i, Index.i )
  
  Define.i DiagnosticIndex = Code\DiagnosticCount
  If ArraySize( Code\Diagnostics() ) = DiagnosticIndex
    ReDim Code\Diagnostics( DiagnosticIndex + 32 )
  EndIf
  Code\DiagnosticCount + 1
  
  Define.Diagnostic *Diagnostic = @Code\Diagnostics( DiagnosticIndex )
  
  *Diagnostic\Code = DiagnosticCode
  *Diagnostic\Index = Index
  
  Debug FormatDiagnostic( DiagnosticIndex )
  
EndProcedure

;==============================================================================
; Parser.
;;;;TODO: put this on a thread

Enumeration ParserContext
  #ValueContext
  #TypeContext
EndEnumeration

Structure Parser
  *Position
  *EndPosition
  *StartPosition
  LastRegion.TextRegion
  FileName.s
  NameBuffer.s
  NameBufferSize.i
  CurrentLine.i
  CurrentColumn.i
  CurrentScope.i
  CurrentDefinitionInScope.i
  CurrentStatement.i
  CurrentContext.i
  ;store failure state here; func to reset
EndStructure

Macro PushContext( Parser, Context )
  Define.i PreviousContext = Parser\CurrentContext
  Parser\CurrentContext = Context
EndMacro

Macro PopContext( Parser )
  Parser\CurrentContext = PreviousContext
EndMacro

Macro PushScope( Parser )
  
  Define.i PreviousScope = Parser\CurrentScope
  Define.i PreviousDefinitionInScope = Parser\CurrentDefinitionInScope
  Define.i PreviousStatementInScope = Parser\CurrentStatement
  Define.i CurrentScope = Code\ScopeCount
  If ArraySize( Code\Scopes() ) = CurrentScope
    ReDim Code\Scopes( CurrentScope + 256 )
  EndIf
  Code\ScopeCount + 1
  Code\Scopes( CurrentScope )\FirstDefinitionOrStatement = -1
  Parser\CurrentScope = CurrentScope
  Parser\CurrentDefinitionInScope = -1
  Parser\CurrentStatement = -1
  
EndMacro

Macro PopScope( Parser )
  
  Parser\CurrentScope = PreviousScope
  Parser\CurrentDefinitionInScope = PreviousDefinionInScope
  Parser\CurrentStatement = PreviousStatementInScope
  CurrentScope = PreviousScope
  
EndMacro

Macro MakeExpressionOpI( IndexVariable, Op, Tp, Operand, StartPos )
  
  Define.i IndexVariable = Code\ExpressionCount
  If ArraySize( Code\Expressions() ) = IndexVariable
    ReDim Code\Expressions( IndexVariable + 1024 )
  EndIf
  Code\ExpressionCount + 1
  
  Define.Expression *Expression = @Code\Expressions( IndexVariable )
  *Expression\Operator = Op
  *Expression\Type = Tp
  *Expression\FirstOperandI = Operand
  *Expression\Region\LeftPos = StartPos
  *Expression\Region\RightPos = *Parser\Position - *Parser\StartPosition
  *Expression\NextExpression = -1
  
EndMacro

Macro MakeExpressionOp2I( IndexVariable, Op, Tp, Operand1, Operand2, StartPos )
  
  Define.i IndexVariable = Code\ExpressionCount
  If ArraySize( Code\Expressions() ) = IndexVariable
    ReDim Code\Expressions( IndexVariable + 1024 )
  EndIf
  Code\ExpressionCount + 1
  
  Define.Expression *Expression = @Code\Expressions( IndexVariable )
  *Expression\Operator = Op
  *Expression\Type = Tp
  *Expression\FirstOperandI = Operand1
  *Expression\SecondOperandI = Operand2
  *Expression\Region\LeftPos = StartPos
  *Expression\Region\RightPos = *Parser\Position - *Parser\StartPosition
  *Expression\NextExpression = -1
  
EndMacro

Macro MakeStatement( IndexVariable, Kind, Reference, Scope = -1 )
  
  Define.i IndexVariable = Code\StatementCount
  If ArraySize( Code\Statements() ) = IndexVariable
    ReDim Code\Statements( IndexVariable + 1024 )
  EndIf
  Define.Statement *Statement = @Code\Statements( IndexVariable )
  Code\StatementCount + 1
  
  *Statement\StatementKind = Kind
  *Statement\ReferencedIndex = Reference
  *Statement\InnerScope = Scope
  *Statement\NextStatement = -1
  
  If *Parser\CurrentStatement <> -1
    Code\Statements( *Parser\CurrentStatement )\NextStatement = IndexVariable
  EndIf
  *Parser\CurrentStatement = IndexVariable
  
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

Procedure.i IsDigit( Character.c )
  ;;;;FIXME: Not Unicode...
  If Character >= 48 And Character <= 57
    ProcedureReturn #True
  EndIf
  ProcedureReturn #False
EndProcedure

Procedure.i IsAlphanumeric( Character.c )
  If IsAlpha( Character ) Or IsDigit( Character )
    ProcedureReturn #True
  EndIf
  ProcedureReturn #False
EndProcedure

; Skips over whitespace characters and comments.
Procedure SkipWhitespace( *Parser.Parser, AllowNewline.b = #True )
  
  Define.i CommentNestingDepth = 0
  
  While *Parser\Position < *Parser\EndPosition
    Define.b Char = PeekB( *Parser\Position )
    
    ; Comments.
    ; We don't allow comments in 'Not AllowNewline' sectiosn
    If AllowNewline And Char = '/' And *Parser\EndPosition - *Position >= 2
      Define.b NextChar = PeekB( *Parser\Position + 1 )
      If NextChar = '/' And CommentNestingDepth = 0
        *Parser\Position + 2
        *Parser\CurrentColumn + 2
        While *Parser\Position < *Parser\EndPosition
          Char = PeekB( *Parser\Position )
          If Char = #NEWLINE
            Break
          EndIf
          *Parser\Position + 1
          *Parser\CurrentColumn + 1
        Wend
      ElseIf NextChar = '*'
        *Parser\Position + 2
        *Parser\CurrentColumn + 2
        CommentNestingDepth + 1
        If *Parser\Position < *Parser\EndPosition
          Char = PeekB( *Parser\Position )
        Else
          Break
        EndIf
      EndIf
    ElseIf CommentNestingDepth > 0 And Char = '*' And *Parser\EndPosition - *Position >= 2
      Define.b NextChar = PeekB( *Parser\Position + 1 )
      If NextChar = '/'
        CommentNestingDepth - 1
        *Parser\Position + 2
        *Parser\CurrentColumn + 2
        If *Parser\Position < *Parser\EndPosition
          Char = PeekB( *Parser\Position )
        Else
          Break
        EndIf
      EndIf
    EndIf
    
    If Not AllowNewline And Char = #NEWLINE
      Break
    EndIf
    
    If Not IsWhitespace( Char ) And CommentNestingDepth = 0
      Break
    EndIf
    
    If Char = #NEWLINE
      *Parser\CurrentLine + 1
      *Parser\CurrentColumn = 0
    EndIf
    
    *Parser\Position + 1
    *Parser\CurrentColumn + 1
    
  Wend
  
EndProcedure

Procedure.b NextCharIsListBegin( *Parser.Parser )
  
  SkipWhitespace( *Parser )
  If *Parser\Position < *Parser\EndPosition
    Define.c Char = PeekB( *Parser\Position ) 
    If *Parser\CurrentContext = #ValueContext
      ProcedureReturn Bool( Char = '(' )
    Else
      ProcedureReturn Bool( Char = '<' )
    EndIf
  EndIf
  
  ProcedureReturn #False
  
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
  *Parser\CurrentColumn + Index
  ProcedureReturn #True
EndProcedure

Procedure ExpectSymbol( *Parser.Parser, Symbol.s, Length.i )
  SkipWhitespace( *Parser )
  If Not MatchToken( *Parser, Symbol, Length, #True )
    ;;;;TODO: diagnose
    Debug "Expecting " + Symbol + " but got " + Chr( PeekB( *Parser\Position ) )
  EndIf
EndProcedure

; Returns a DefinitionFlag.
Procedure.i ParseModifier( *Parser.Parser )
  SkipWhitespace( *Parser )
  If MatchToken( *Parser, "abstract", 8 )
    ProcedureReturn #IsAbstract
  ElseIf MatchToken( *Parser, "mutable", 7 )
    ProcedureReturn #IsMutable
  ElseIf MatchToken( *Parser, "immutable", 9 )
    ProcedureReturn #IsImmutable
  ElseIf MatchToken( *Parser, "iterator", 8 )
    ProcedureReturn #IsIterator
  ElseIf MatchToken( *Parser, "before", 6 )
    ProcedureReturn #IsBefore
  ElseIf MatchToken( *Parser, "after", 5 )
    ProcedureReturn #IsAfter
  ElseIf MatchToken( *Parser, "around", 6 )
    ProcedureReturn #IsAround
  ElseIf MatchToken( *Parser, "import", 6 )
    ProcedureReturn #IsImport
  ElseIf MatchToken( *Parser, "extend", 6 )
    ProcedureReturn #IsExtend
  ElseIf MatchToken( *Parser, "replace", 7 ) ;;;;REVIEW: 'override'?
    ProcedureReturn #IsReplace
  EndIf
  ProcedureReturn 0
EndProcedure

; Parses a name into NameBuffer and canonicalizes it.
; Returns length of name on success. Updates LastRegion.
; Returns -1 if there's no name at the current position.
Procedure.i ReadName( *Parser.Parser )
  
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
  
  *Parser\LastRegion\LeftPos = *StartPosition - *Parser\StartPosition
  *Parser\LastRegion\RightPos = *Parser\LastRegion\LeftPos + Length
  
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
  
  ProcedureReturn WriteIndex
  
EndProcedure

Procedure.i ParseIdentifier( *Parser.Parser )
  
  SkipWhitespace( *Parser )
  
  ;;;;TODO: escaped identifiers
  ;;;;TODO: identifiers ending in !
  
  Define.i Length = ReadName( *Parser )
  If Length = -1
    ProcedureReturn -1
  EndIf
  
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

Procedure.i ParseAnnotation( *Parser.Parser )
  
  Define.i LineBefore = *Parser\CurrentLine
  SkipWhitespace( *Parser )
  If *Parser\CurrentLine = LineBefore And *Parser\CurrentLine <> 1
    ProcedureReturn -1
  EndIf
  If Not MatchToken( *Parser, "|", 1, #True )
    ProcedureReturn -1
  EndIf
  
  Define.i AnnotationKind
  If MatchToken( *Parser, "description", 11 )
    AnnotationKind = #DescriptionAnnotation
  ElseIf MatchToken( *Parser, "details", 7 )
    AnnotationKind = #DetailsAnnotation
  ElseIf MatchToken( *Parser, "category", 8 )
    AnnotationKind = #CategoryAnnotation
  ElseIf MatchToken( *Parser, "product", 7 )
    AnnotationKind = #ProductAnnotation
  ElseIf MatchToken( *Parser, "company", 7 )
    AnnotationKind = #CompanyAnnotation
  Else
    ;;;;TODO: diagnose
    Debug "Unknown annotation!"
  EndIf
  
  SkipWhitespace( *Parser, #False )
  Define *StartPos = *Parser\Position
  While *Parser\Position < *Parser\EndPosition
    Define.c Char = PeekB( *Parser\Position )
    If Char = #NEWLINE
      Break
    EndIf
    *Parser\Position + 1
  Wend
  Define.s Text
  If *Parser\Position > *StartPos
    Text = PeekS( *StartPos, *Parser\Position - *StartPos, #PB_UTF8 | #PB_ByteLength )
  EndIf
  
  Define.i AnnotationIndex = Code\AnnotationCount
  If ArraySize( Code\Annotations() ) = AnnotationIndex
    ReDim Code\Annotations( AnnotationIndex + 512 )
  EndIf
  Define.Annotation *Annotation = @Code\Annotations( AnnotationIndex )
  *Annotation\AnnotationKind = AnnotationKind
  *Annotation\AnnotationText = Text
  *Annotation\NextAnnotation = -1
  Code\AnnotationCount + 1
  
  ProcedureReturn AnnotationIndex
  
EndProcedure

; Expression parsing is recursive.
Declare.i ParseExpression( *Parser.Parser )

Procedure.i ParseBasicExpression( *Parser.Parser )
  
  SkipWhitespace( *Parser )
  If *Parser\Position = *Parser\EndPosition
    ProcedureReturn -1
  EndIf
  
  Define.i LeftPos = *Parser\Position - *Parser\StartPosition
  
  Define.i Operator = -1
  Define.i Type = -1
  Define.i FirstOperandI
  
  Define.c Char = PeekB( *Parser\Position )
  If IsDigit( Char )
    
    Operator = #LiteralExpression
    
    ;;;;TODO: support hex literals
    
    ; Numbers.
    
    Define.i IntegerPart = Char - 48
    *Parser\Position + 1
    
    ; Advance to next non-digit.
    While *Parser\Position < *Parser\EndPosition
      Char = PeekB( *Parser\Position )
      If Not IsDigit( Char )
        Break
      EndIf
      IntegerPart * 10
      IntegerPart + ( Char - 48 )
      *Parser\Position + 1
    Wend
    
    ;;;;TODO: float literals
    ;;;;TODO: integer size suffixes
    
    FirstOperandI = IntegerPart
    
  ElseIf IsAlpha( Char ) Or Char = '_'
    
    Operator = #NameExpression
    FirstOperandI = ParseIdentifier( *Parser )
    
  ElseIf Char = '('
    
    ; Either a simple parenthesized expression or a tuple expression.
    ; Depends on the number of elements.
    
    *Parser\Position + 1
    
    Define.i FirstExpression = ParseExpression( *Parser )
    If FirstExpression = -1
      ;;;;TODO: diagnose
      Debug "Expecting expression!"
    EndIf
    
    SkipWhitespace( *Parser )
    If *Parser\Position >= *Parser\EndPosition Or PeekB( *Parser\Position ) <> ','
      ; It's a simple parenthesized expression. The parenthesis simply
      ; disappear in our internal representation.
      ExpectSymbol( *Parser, ")", 1 )
      ProcedureReturn FirstExpression
    EndIf
    
    Define.i LastExpression = FirstExpression
    
    While *Parser\Position < *Parser\EndPosition
      
      SkipWhitespace( *Parser )
      If MatchToken( *Parser, ")", 1, #True )
        Break
      ElseIf Not MatchToken( *Parser, ",", 1, #True )
        ;;;;TODO: diagnose
        Debug "Expecting ','!!"
      EndIf
      
      Define.i Expression = ParseExpression( *Parser )
      If Expression = -1
        Break
      EndIf
      
      Code\Expressions( LastExpression )\NextExpression = Expression
      LastExpression = Expression
      
    Wend
    
    Operator = #TupleExpression
    FirstOperandI = FirstExpression
  
  Else
    ProcedureReturn -1
  EndIf
  
  MakeExpressionOpI( Expression, Operator, Type, FirstOperandI, LeftPos )
  
  ProcedureReturn Expression
  
EndProcedure

Procedure.i ParseUnaryPostfixExpression( *Parser.Parser )
  
  ;;;;TODO: first one needs to be allowed to be a full expression
  Define.i FirstExpression = ParseBasicExpression( *Parser )
  If FirstExpression = -1
    ProcedureReturn -1
  EndIf
  
  ; ATM we allow only parenthesized and tuple expressions to form apply expressions.
  ; With a more powerful parser, that could be changed but there's a lot of ambiguity
  ; that needs to be figured out on that path.
  If NextCharIsListBegin( *Parser )
      
    Define.i SecondExpression = ParseBasicExpression( *Parser )
    If SecondExpression <> -1
      MakeExpressionOp2I( ApplyExpr, #ApplyExpression, -1, FirstExpression, SecondExpression, Code\Expressions( FirstExpression )\Region\LeftPos )
      ProcedureReturn ApplyExpr
    EndIf
    
  EndIf
  
  ProcedureReturn FirstExpression
  
EndProcedure

Procedure.i ParseUnaryPrefixExpression( *Parser.Parser )
  
  SkipWhitespace( *Parser )
  
  Define.i LeftPos = *Parser\Position - *Parser\StartPosition
  Define.i PrefixOperator = -1
  If MatchToken( *Parser, "!", 1, #True )
    PrefixOperator = #LogicalNotExpression
  EndIf
  
  Define.i Expression = ParseUnaryPostfixExpression( *Parser )
  
  If PrefixOperator <> -1
    MakeExpressionOpI( UnaryExpression, PrefixOperator, -1, Expression, LeftPos )
    Expression = UnaryExpression
  EndIf
  
  ProcedureReturn Expression
  
EndProcedure

Procedure.i ParseBinaryExpression( *Parser.Parser )
  Define.i Left = ParseUnaryPrefixExpression( *Parser )
  ;;;;TODO
  ProcedureReturn Left
EndProcedure

Procedure.i ParseExpression( *Parser.Parser )
  ProcedureReturn ParseBinaryExpression( *Parser )
EndProcedure

; Statements may themselves open scope (e.g. if statement) and thus
; lead to recursion.
Declare.i ParseStatementList( *Parser.Parser, Definition.i = -1 )

Procedure.i ParseStatement( *Parser.Parser )
  
  SkipWhitespace( *Parser )
  
  Define.i Statement = -1
  Define.i ReferencedIndex = -1
  Define.i InnerScope = -1
  
  If MatchToken( *Parser, "if", 2 )
    
    Statement = #IfStatement
    ReferencedIndex = ParseExpression( *Parser )
    InnerScope = ParseStatementList( *Parser )
    
    CompilerIf #False
    SkipWhitespace( *Parser )
    If MatchToken( *Parser, "elseif", 6 )
    ElseIf MatchToken( *Parser, "else", 4 )
    EndIf
    CompilerEndIf
    
  ElseIf MatchToken( *Parser, "return", 6 )
    
    SkipWhitespace( *Parser )
    If Not MatchToken( *Parser, ";", 1, #True )
      
      ReferencedIndex = ParseExpression( *Parser )
      ExpectSymbol( *Parser, ";", 1 )
      
    EndIf
    
    Statement = #ReturnStatement
    
  Else
    ProcedureReturn -1
  EndIf
  
  MakeStatement( StatementIndex, Statement, ReferencedIndex, InnerScope )
  
  ProcedureReturn StatementIndex
  
EndProcedure

; Opens a new scope and parses a list of statements into it.
; Returns the index of the scope.
Procedure.i ParseStatementList( *Parser.Parser, Definition.i = -1 )
  
  PushScope( *Parser )
  Define.i Scope = CurrentScope
  Code\Scopes( CurrentScope )\Definition = Definition
    
  Define.i FirstStatement = -1
  Define.i LastStatement = -1
  
  While *Parser\Position < *Parser\EndPosition
    
    SkipWhitespace( *Parser )
    If MatchToken( *Parser, "end", 3 )
      Break
    EndIf
    
    Define.i Statement = ParseStatement( *Parser )
    If Statement = -1
      Break
    EndIf
    
    If FirstStatement = -1
      FirstStatement = Statement
      ; The list of statements is linked through the scope.
      Code\Scopes( CurrentScope )\FirstDefinitionOrStatement = Statement      
    Else
      Code\Statements( LastStatement )\NextStatement = Statement
    EndIf
    
    ; Statement may be a sequence (e.g. if-else). Iterate to last one.
    While Code\Statements( Statement )\NextStatement <> -1
      Statement = Code\Statements( Statement )\NextStatement
    Wend
    
    LastStatement = Statement
    
  Wend
  
  PopScope( *Parser )
  ExpectSymbol( *Parser, ";", 1 )
  
  ProcedureReturn Scope
  
EndProcedure

Procedure.i ParseParameterList( *Parser.Parser )
  
  Define.i FirstParameter = -1
  Define.i LastParameter = -1
  
  While *Parser\Position < *Parser\EndPosition
    
    If FirstParameter <> -1
      SkipWhitespace( *Parser )
      If Not MatchToken( *Parser, ",", 1, #True )
        Break
      EndIf
    EndIf
    
    Define.i Name = ParseIdentifier( *Parser )
    Define.i Type = -1
    
    If Name = -1
      Break
    EndIf
    
    SkipWhitespace( *Parser )
    If MatchToken( *Parser, ":", 1, #True )
      Type = ParseExpression( *Parser )
      If Type = -1
        ;;;;TODO: add diagnostic
        Debug "Expecting type!!"
      EndIf
    EndIf
    
    Define.i ParameterIndex = Code\ParameterCount
    If ArraySize( Code\Parameters() ) = ParameterIndex
      ReDim Code\Parameters( ParameterIndex + 512 )
    EndIf
    Code\ParameterCount + 1
    
    Define.Parameter *Parameter = @Code\Parameters( ParameterIndex )
    
    *Parameter\Name = Name
    *Parameter\Type = Type
    *Parameter\NextParameter = -1
    
    If FirstParameter = -1
      FirstParameter = ParameterIndex
    Else
      Code\Parameters( LastParameter )\NextParameter = ParameterIndex
    EndIf
    
    LastParameter = ParameterIndex
    
  Wend
  
  ProcedureReturn FirstParameter
  
EndProcedure

; Returns index of definition or -1 on failure.
Procedure.i ParseDefinition( *Parser.Parser )
  
  ; Parse annotations.
  Define.i FirstAnnotation = -1
  Define.i LastAnnotation = -1
  While *Parser\Position < *Parser\EndPosition
    Define.i Annotation = ParseAnnotation( *Parser )
    If Annotation = -1
      Break
    EndIf
    If FirstAnnotation = -1
      FirstAnnotation = Annotation
    Else
      Code\Annotations( LastAnnotation )\NextAnnotation = Annotation
    EndIf
    LastAnnotation = Annotation
  Wend
  
  ; Parse modifiers.
  Define.i Flags = 0
  While *Parser\Position < *Parser\EndPosition
    Define.i Modifier = ParseModifier( *Parser )
    If Modifier = 0
      Break
    EndIf
    If Flags & Modifier
      ;;;;TODO: diagnose duplicate modifier
      Debug "Duplicate modifier!!"
    EndIf
    Flags | Modifier
  Wend
  
  ; Parse definition kind.
  Define.i Kind = -1
  SkipWhitespace( *Parser )
  If MatchToken( *Parser, "type", 4 )
    Kind = #TypeDefinition
  ElseIf MatchToken( *Parser, "object", 6 )
    Kind = #TypeDefinition
    Flags | #IsSingleton
  ElseIf MatchToken( *Parser, "method", 6 )
    Kind = #MethodDefinition
  ElseIf MatchToken( *Parser, "field", 5 )
    Kind = #FieldDefinition
  ElseIf MatchToken( *Parser, "features", 8 )
    Kind = #FeatureDefinition
  ElseIf MatchToken( *Parser, "program", 7 )
    Kind = #ProgramDefinition
  ElseIf MatchToken( *Parser, "library", 7 )
    Kind = #LibraryDefinition
  ElseIf MatchToken( *Parser, "module", 6 )
    Kind = #ModuleDefinition
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
  *Definition\DefinitionKind = Kind
  *Definition\NextDefinition = -1
  *Definition\InnerScope = -1
  *Definition\FirstAnnotation = FirstAnnotation
  *Definition\FirstClause = -1
  *Definition\FirstValueParameter = -1
  *Definition\FirstTypeParameter = -1
  *Definition\Region = *Parser\LastRegion
  *Definition\Scope = *Parser\CurrentScope
  Code\DefinitionCount + 1
  
  ; Add to scope.
  If *Parser\CurrentDefinitionInScope <> -1
    Code\Definitions( *Parser\CurrentDefinitionInScope )\NextDefinition = DefinitionIndex
  Else
    Code\Scopes( *Parser\CurrentScope )\FirstDefinitionOrStatement = DefinitionIndex
  EndIf
  *Parser\CurrentDefinitionInScope = DefinitionIndex
  
  ; Parse type parameters.
  SkipWhitespace( *Parser )
  If MatchToken( *Parser, "<", 1, #True )
    
    *Definition\FirstTypeParameter = ParseParameterList( *Parser )
    ExpectSymbol( *Parser, ">", 1 )
    
  EndIf
  
  ; Parse value parameters.
  SkipWhitespace( *Parser )
  If MatchToken( *Parser, "(", 1, #True )
    
    *Definition\FirstValueParameter = ParseParameterList( *Parser )
    ExpectSymbol( *Parser, ")", 1 )
    
  EndIf
  
  ; Parse type.
  SkipWhitespace( *Parser )
  If MatchToken( *Parser, ":", 1, #True )
    *Definition\Type = ParseExpression( *Parser )
  EndIf
  
  ; Parse clauses.
  
  ; Parse body.
  SkipWhitespace( *Parser )
  If Not MatchToken( *Parser, ";", 1, #True )
    *Definition\InnerScope = ParseStatementList( *Parser, DefinitionIndex )
  EndIf
  
  ProcedureReturn DefinitionIndex
  
EndProcedure

Procedure ParseText()
  
  ;;;;TODO: preparse libraries and reset to the preparsed state here
  
  Define.Parser Parser
  Parser\Position = *Text
  Parser\EndPosition = *Text + TextLength
  Parser\StartPosition = *Text
  Parser\CurrentDefinitionInScope = -1
  Parser\CurrentStatement = -1
  Parser\CurrentLine = 1
  Parser\CurrentColumn = 1
  Parser\CurrentContext = #ValueContext
  
  While Parser\Position < Parser\EndPosition
    If ParseDefinition( @Parser ) = -1
      ;;;;TODO: error handling (diagnose and keep going)
      Break
    EndIf
  Wend
  
  Debug( "Definitions: " + Str( Code\DefinitionCount ) + ", Expressions: " +
         Str( Code\ExpressionCount ) + ", Statements: " + Str( Code\StatementCount ) +
         ", Parameters: " + Str( Code\ParameterCount ) )
  
EndProcedure

Prototype.i ParseFunction( *Parser.Parser )

Procedure TestParseText( Fn.ParseFunction, Text.s, Expected.i = #True )
  Define.Parser Parser
  Define *Buffer = UTF8( Text )
  Define.i Length = MemoryStringLength( *Buffer, #PB_UTF8 )
  Parser\Position = *Buffer
  Parser\EndPosition = *Buffer + Length
  Parser\StartPosition = *Buffer
  Parser\CurrentDefinitionInScope = -1
  Parser\CurrentStatement = -1
  Parser\CurrentLine = 1
  Parser\CurrentColumn = 1
  Parser\CurrentContext = #ValueContext
  Assert( Fn( @Parser ) = Expected )
  FreeMemory( *Buffer )
EndProcedure

ProcedureUnit CanSkipComments()
  ResetCode()
  TestParseText( @ParseModifier(), "/* foo */ abstract", #IsAbstract )
  TestParseText( @ParseModifier(), ~"// foo\n abstract", #IsAbstract )
  TestParseText( @ParseModifier(), "/* /* foo */ */ abstract", #IsAbstract )
EndProcedureUnit

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

ProcedureUnit CanParseLiteralExpression()
  ResetCode()
  TestParseText( @ParseExpression(), "01234", 0 )
  Assert( Code\ExpressionCount = 1 )
  Assert( Code\Expressions( 0 )\Operator = #LiteralExpression )
  ;;;;TODO: check type
  Assert( Code\Expressions( 0 )\FirstOperandI = 1234 )
EndProcedureUnit

ProcedureUnit CanParseNameExpression()
  ResetCode()
  TestParseText( @ParseExpression(), "test", 0 )
  Assert( Code\ExpressionCount = 1 )
  Assert( Code\IdentifierCount = 1 )
  Assert( Code\Identifiers( 0 ) = "test" )
  Assert( Code\Expressions( 0 )\Operator = #NameExpression )
  ;;;;TODO: check type
  Assert( Code\Expressions( 0 )\FirstOperandI = 0 )
EndProcedureUnit

ProcedureUnit CanParseParenthesizedExpression()
  ResetCode()
  ; This is *not* a tuple expression. Need at least two elements.
  TestParseText( @ParseExpression(), "( a )", 0 )
  Assert( Code\ExpressionCount = 1 )
  Assert( Code\IdentifierCount = 1 )
  Assert( Code\Identifiers( 0 ) = "a" )
  Assert( Code\Expressions( 0 )\Operator = #NameExpression )
  Assert( Code\Expressions( 0 )\FirstOperandI = 0 )
  Assert( Code\Expressions( 0 )\NextExpression = -1 )
EndProcedureUnit

ProcedureUnit CanParseTupleExpression()
  ResetCode()
  TestParseText( @ParseExpression(), "( a, b, c )", 3 )
  Assert( Code\ExpressionCount = 4 )
  Assert( Code\IdentifierCount = 3 )
  Assert( Code\Identifiers( 0 ) = "a" )
  Assert( Code\Identifiers( 1 ) = "b" )
  Assert( Code\Identifiers( 2 ) = "c" )
  Assert( Code\Expressions( 0 )\Operator = #NameExpression )
  Assert( Code\Expressions( 0 )\FirstOperandI = 0 )
  Assert( Code\Expressions( 0 )\NextExpression = 1 )
  Assert( Code\Expressions( 1 )\Operator = #NameExpression )
  Assert( Code\Expressions( 1 )\FirstOperandI = 1 )
  Assert( Code\Expressions( 1 )\NextExpression = 2 )
  Assert( Code\Expressions( 2 )\Operator = #NameExpression )
  Assert( Code\Expressions( 2 )\FirstOperandI = 2 )
  Assert( Code\Expressions( 2 )\NextExpression = -1 )
  Assert( Code\Expressions( 3 )\Operator = #TupleExpression )
  Assert( Code\Expressions( 3 )\FirstOperandI = 0 )
  Assert( Code\Expressions( 3 )\NextExpression = -1 )
EndProcedureUnit

ProcedureUnit CanParseApplyExpression()
  ResetCode()
  ; One expression followed by another without an operator in-between is an application.
  ; NOTE: ATM, parenthesis are needed.
  TestParseText( @ParseExpression(), "a( b )", 2 )
  Assert( Code\ExpressionCount = 3 )
  Assert( Code\IdentifierCount = 2 )
  Assert( Code\Identifiers( 0 ) = "a" )
  Assert( Code\Identifiers( 1 ) = "b" )
  Assert( Code\Expressions( 0 )\Operator = #NameExpression )
  Assert( Code\Expressions( 0 )\FirstOperandI = 0 )
  Assert( Code\Expressions( 0 )\NextExpression = -1 ) ; These are not linked.
  Assert( Code\Expressions( 1 )\Operator = #NameExpression )
  Assert( Code\Expressions( 1 )\FirstOperandI = 1 )
  Assert( Code\Expressions( 1 )\NextExpression = -1 )
  Assert( Code\Expressions( 2 )\Operator = #ApplyExpression )
  Assert( Code\Expressions( 2 )\FirstOperandI = 0 )
  Assert( Code\Expressions( 2 )\SecondOperandI = 1 )
  Assert( Code\Expressions( 2 )\NextExpression = -1 )
EndProcedureUnit
  
ProcedureUnit CanParseIfStatementWithoutElse()
  ResetCode()
  TestParseText( @ParseStatement(), "if ( a ) return 123; end;", 1 )
  Assert( Code\StatementCount = 2 )
  Assert( Code\ExpressionCount = 2 )
  Assert( Code\IdentifierCount = 1 )
  Assert( Code\ScopeCount = 2 )
  Assert( Code\Identifiers( 0 ) = "a" )
  Assert( Code\Scopes( 1 )\Definition = -1 )
  Assert( Code\Scopes( 1 )\Parent = 0 )
  Assert( Code\Scopes( 1 )\FirstDefinitionOrStatement = 0 )
  Assert( Code\Statements( 0 )\StatementKind = #ReturnStatement )
  Assert( Code\Statements( 0 )\ReferencedIndex = 1 )
  Assert( Code\Statements( 0 )\NextStatement = -1 )
  Assert( Code\Statements( 0 )\InnerScope = -1 )
  Assert( Code\Statements( 1 )\StatementKind = #IfStatement )
  Assert( Code\Statements( 1 )\ReferencedIndex = 0 )
  Assert( Code\Statements( 1 )\NextStatement = -1 )
  Assert( Code\Statements( 1 )\InnerScope = 1 )
  Assert( Code\Expressions( 0 )\Operator = #NameExpression )
  Assert( Code\Expressions( 0 )\FirstOperandI = 0 )
  Assert( Code\Expressions( 0 )\NextExpression = -1 )
  Assert( Code\Expressions( 1 )\Operator = #LiteralExpression )
  Assert( Code\Expressions( 1 )\FirstOperandI = 123 )
  Assert( Code\Expressions( 1 )\NextExpression = -1 )
EndProcedureUnit  

ProcedureUnit CanParseIfStatementWithElse()
  ResetCode()
  TestParseText( @ParseStatement(), "if ( a ) return 123; else return 321; end;", 1 )
  Assert( Code\ScopeCount = 3 )
  Assert( Code\StatementCount = 4 )
  Assert( Code\ExpressionCount = 3 )
  Assert( Code\IdentifierCount = 1 )
  Assert( Code\Identifiers( 0 ) = "a" )
  Assert( Code\Scopes( 1 )\Parent = 0 )
  Assert( Code\Scopes( 1 )\FirstDefinitionOrStatement = 0 )
  Assert( Code\Scopes( 1 )\Definition = -1 )
  Assert( Code\Scopes( 2 )\Parent = 0 )
  Assert( Code\Scopes( 2 )\FirstDefinitionOrStatement = 2 )
  Assert( Code\Scopes( 2 )\Definition = -1 )
  Assert( Code\Statements( 0 )\StatementKind = #ReturnStatement )
  Assert( Code\Statements( 0 )\ReferencedIndex = 1 )
  Assert( Code\Statements( 0 )\NextStatement = -1 )
  Assert( Code\Statements( 0 )\InnerScope = -1 )
  Assert( Code\Statements( 1 )\StatementKind = #IfStatement )
  Assert( Code\Statements( 1 )\ReferencedIndex = 0 )
  Assert( Code\Statements( 1 )\NextStatement = 3 )
  Assert( Code\Statements( 1 )\InnerScope = 1 )
  Assert( Code\Statements( 2 )\StatementKind = #ReturnStatement )
  Assert( Code\Statements( 2 )\ReferencedIndex = 2 )
  Assert( Code\Statements( 2 )\NextStatement = -1 )
  Assert( Code\Statements( 2 )\InnerScope = -1 )
  Assert( Code\Statements( 3 )\StatementKind = #ElseStatement )
  Assert( Code\Statements( 3 )\ReferencedIndex = -1 )
  Assert( Code\Statements( 3 )\NextStatement = -1 )
  Assert( Code\Statements( 3 )\InnerScope = 2 )
  Assert( Code\Expressions( 0 )\Operator = #NameExpression )
  Assert( Code\Expressions( 0 )\FirstOperandI = 0 )
  Assert( Code\Expressions( 0 )\NextExpression = -1 )
  Assert( Code\Expressions( 1 )\Operator = #LiteralExpression )
  Assert( Code\Expressions( 1 )\FirstOperandI = 123 )
  Assert( Code\Expressions( 1 )\NextExpression = -1 )
  Assert( Code\Expressions( 2 )\Operator = #LiteralExpression )
  Assert( Code\Expressions( 2 )\FirstOperandI = 321 )
  Assert( Code\Expressions( 2 )\NextExpression = -1 )
EndProcedureUnit  

ProcedureUnit CanParseAnnotation()
  ResetCode()
  TestParseText( @ParseAnnotation(), "|DESCRIPTION Foo", 0 )
  Assert( Code\AnnotationCount = 1 )
  Assert( Code\Annotations( 0 )\AnnotationKind = #DescriptionAnnotation )
  Assert( Code\Annotations( 0 )\AnnotationText = "Foo" )
EndProcedureUnit

ProcedureUnit CanParseSimpleTypeDefinition()
  ResetCode()
  TestParseText( @ParseDefinition(), "type First;", 0 )
  Assert( Code\DefinitionCount = 1 )
  Assert( Code\IdentifierCount = 1 )
  Assert( Code\Identifiers( 0 ) = "first" )
  Assert( Code\Definitions( 0 )\Name = 0 )
  Assert( Code\Definitions( 0 )\DefinitionKind = #TypeDefinition )
  Assert( Code\Definitions( 0 )\Flags = 0 )
  Assert( Code\Definitions( 0 )\FirstAnnotation = -1 )
  Assert( Code\Definitions( 0 )\FirstValueParameter = -1 )
  Assert( Code\Definitions( 0 )\FirstTypeParameter = -1 )
EndProcedureUnit

ProcedureUnit CanParseDerivedTypeDefinition()
  ResetCode()
  TestParseText( @ParseDefinition(), "type First : Second;", 0 )
  Assert( Code\DefinitionCount = 1 )
  Assert( Code\IdentifierCount = 2 )
  Assert( Code\ExpressionCount = 1 )
  Assert( Code\Identifiers( 0 ) = "first" )
  Assert( Code\Identifiers( 1 ) = "second" )
  Assert( Code\Definitions( 0 )\Name = 0 )
  Assert( Code\Definitions( 0 )\DefinitionKind = #TypeDefinition )
  Assert( Code\Definitions( 0 )\Flags = 0 )
  Assert( Code\Definitions( 0 )\FirstAnnotation = -1 )
  Assert( Code\Definitions( 0 )\FirstValueParameter = -1 )
  Assert( Code\Definitions( 0 )\FirstTypeParameter = -1 )
  Assert( Code\Definitions( 0 )\Type = 0 )
  Assert( Code\Expressions( 0 )\Operator = #NameExpression )
  Assert( Code\Expressions( 0 )\FirstOperandI = 1 )
EndProcedureUnit

ProcedureUnit CanParseEmptyMethodDefinition()
  ResetCode()
  TestParseText( @ParseDefinition(), "method First;", 0 )
  Assert( Code\DefinitionCount = 1 )
  Assert( Code\IdentifierCount = 1 )
  Assert( Code\Identifiers( 0 ) = "first" )
  Assert( Code\Definitions( 0 )\Name = 0 )
  Assert( Code\Definitions( 0 )\DefinitionKind = #MethodDefinition )
  Assert( Code\Definitions( 0 )\Flags = 0 )
  Assert( Code\Definitions( 0 )\FirstAnnotation = -1 )
  Assert( Code\Definitions( 0 )\InnerScope = -1 )
  Assert( Code\Definitions( 0 )\FirstValueParameter = -1 )
  Assert( Code\Definitions( 0 )\FirstTypeParameter = -1 )
EndProcedureUnit

ProcedureUnit CanParseSimpleMethodDefinition()
  ResetCode()
  TestParseText( @ParseDefinition(), "method First() return; end;", 0 )
  Assert( Code\DefinitionCount = 1 )
  Assert( Code\IdentifierCount = 1 )
  Assert( Code\ScopeCount = 2 )
  Assert( Code\StatementCount = 1 )
  Assert( Code\Identifiers( 0 ) = "first" )
  Assert( Code\Definitions( 0 )\Name = 0 )
  Assert( Code\Definitions( 0 )\DefinitionKind = #MethodDefinition )
  Assert( Code\Definitions( 0 )\Flags = 0 )
  Assert( Code\Definitions( 0 )\FirstAnnotation = -1 )
  Assert( Code\Definitions( 0 )\InnerScope = 1 )
  Assert( Code\Definitions( 0 )\FirstValueParameter = -1 )
  Assert( Code\Definitions( 0 )\FirstTypeParameter = -1 )
  Assert( Code\Scopes( 1 )\Definition = 0 )
  Assert( Code\Scopes( 1 )\FirstDefinitionOrStatement = 0 )
  Assert( Code\Statements( 0 )\StatementKind = #ReturnStatement )
  Assert( Code\Statements( 0 )\NextStatement = -1 )
EndProcedureUnit

ProcedureUnit CanParseMethodDefinitionWithValueParameters()
  ResetCode()
  TestParseText( @ParseDefinition(), "method First( A, B : C ) end;", 0 )
  Assert( Code\DefinitionCount = 1 )
  Assert( Code\IdentifierCount = 4 )
  Assert( Code\ScopeCount = 2 )
  Assert( Code\StatementCount = 0 )
  Assert( Code\ParameterCount = 2 )
  Assert( Code\ExpressionCount = 1 )
  Assert( Code\Identifiers( 0 ) = "first" )
  Assert( Code\Identifiers( 1 ) = "a" )
  Assert( Code\Identifiers( 2 ) = "b" )
  Assert( Code\Identifiers( 3 ) = "c" )
  Assert( Code\Definitions( 0 )\Name = 0 )
  Assert( Code\Definitions( 0 )\DefinitionKind = #MethodDefinition )
  Assert( Code\Definitions( 0 )\Flags = 0 )
  Assert( Code\Definitions( 0 )\FirstAnnotation = -1 )
  Assert( Code\Definitions( 0 )\InnerScope = 1 )
  Assert( Code\Definitions( 0 )\FirstValueParameter = 0 )
  Assert( Code\Definitions( 0 )\FirstTypeParameter = -1 )
  Assert( Code\Scopes( 1 )\Definition = 0 )
  Assert( Code\Scopes( 1 )\FirstDefinitionOrStatement = -1 )
  Assert( Code\Parameters( 0 )\Name = 1 )
  Assert( Code\Parameters( 0 )\NextParameter = 1 )
  Assert( Code\Parameters( 0 )\Type = -1 )
  Assert( Code\Parameters( 1 )\Name = 2 )
  Assert( Code\Parameters( 1 )\NextParameter = -1 )
  Assert( Code\Parameters( 1 )\Type = 0 )
  Assert( Code\Expressions( 0 )\Operator = #NameExpression )
  Assert( Code\Expressions( 0 )\FirstOperandI = 3 )
EndProcedureUnit

ProcedureUnit CanParseTypeDefinitionWithAnnotation()
  ResetCode()
  TestParseText( @ParseDefinition(), ~"|DESCRIPTION Something\n|DETAILS Foo\ntype First;", 0 )
  Assert( Code\DefinitionCount = 1 )
  Assert( Code\IdentifierCount = 1 )
  Assert( Code\Identifiers( 0 ) = "first" )
  Assert( Code\Definitions( 0 )\Name = 0 )
  Assert( Code\Definitions( 0 )\DefinitionKind = #TypeDefinition )
  Assert( Code\Definitions( 0 )\Flags = 0 )
  Assert( Code\Definitions( 0 )\FirstAnnotation = 0 )
  Assert( Code\AnnotationCount = 2 )
  Assert( Code\Annotations( 0 )\AnnotationKind = #DescriptionAnnotation )
  Assert( Code\Annotations( 0 )\AnnotationText = "Something" )
  Assert( Code\Annotations( 0 )\NextAnnotation = 1 )
  Assert( Code\Annotations( 1 )\AnnotationKind = #DetailsAnnotation )
  Assert( Code\Annotations( 1 )\AnnotationText = "Foo" )
  Assert( Code\Annotations( 1 )\NextAnnotation = -1 )
EndProcedureUnit
  
ProcedureUnit CanParseSimpleProgram()
  ResetCode()
  TestParseText( @ParseDefinition(), ~"|PRODUCT MyProduct\n|COMPANY MyCompany\nprogram First;", 0 )
  Assert( Code\DefinitionCount = 1 )
  Assert( Code\IdentifierCount = 1 )
  Assert( Code\Identifiers( 0 ) = "first" )
  Assert( Code\Definitions( 0 )\Name = 0 )
  Assert( Code\Definitions( 0 )\DefinitionKind = #ProgramDefinition )
  Assert( Code\Definitions( 0 )\Flags = 0 )
  Assert( Code\Definitions( 0 )\FirstAnnotation = 0 )
  Assert( Code\AnnotationCount = 2 )
  Assert( Code\Annotations( 0 )\AnnotationKind = #ProductAnnotation )
  Assert( Code\Annotations( 0 )\AnnotationText = "MyProduct" )
  Assert( Code\Annotations( 0 )\NextAnnotation = 1 )
  Assert( Code\Annotations( 1 )\AnnotationKind = #CompanyAnnotation )
  Assert( Code\Annotations( 1 )\AnnotationText = "MyCompany" )
  Assert( Code\Annotations( 1 )\NextAnnotation = -1 )
EndProcedureUnit

;==============================================================================
; Documentation.

;;;;TODO: support docs for language elements (just have manual tab with handwritten content?)

Procedure GenerateDocs()
  
  ;;no... instead, host a simple webserver that dynamically generates content and have the doc viewer go there
  
  Define.s TypesFolder = GeneratedDocsPath + "\Types"
  Define.s FunctionsFolder = GeneratedDocsPath + "\Functions"
  
  Define.s TypelistBeginMarker = "<!--TYPELIST:BEGIN-->"
  Define.s TypelistEndMarker = "<!--TYPELIST:END-->"
  Define.s FunclistBeginMarker = "<!--FUNCLIST:BEGIN-->"
  Define.s FunclistEndMarker = "<!--FUNCLIST:END-->"
  
  ; Remove existing generated doc files.
  DeleteDirectory( TypesFolder, "*.html" )
  DeleteDirectory( FunctionsFolder, "*.html" )
  CreateDirectory( TypesFolder )
  CreateDirectory( FunctionsFolder )
  
  ;;;;TODO: sort alphabetically
  ;;;;TODO: put types in inheritance hierarchy (okay if a type is mentioned more than once; or introduce groups for stuff like "A & B"?)
  ;;;;TODO: also have a tab for assets; in fact, the browser should basically be a browsable HTML version of the project
  ;;;;TODO: also have a tab for tests? or display the results inline as blobs on the test methods? but maybe both with the former for details?
  
  ; Populate TOC.
  Define.i TOCFile = OpenFile( #PB_Any, GeneratedDocsPath + "\toc.html" )
  Define.s TOC = ReadString( TOCFile, #PB_UTF8 | #PB_File_IgnoreEOL )
  
  Define.i TypelistBegin = FindString( TOC, TypelistBeginMarker )
  Define.i TypelistEnd = FindString( TOC, TypelistEndMarker )
  Define.i FunclistBegin = FindString( TOC, FunclistBeginMarker )
  Define.i FunclistEnd = FindString( TOC, FunclistEndMarker )
  
  If TypelistBegin = 0
    Debug "Could not find TYPELIST:BEGIN in TOC"
    ProcedureReturn
  EndIf
  If TypelistEnd = 0
    Debug "Could not find TYPELIST:END in TOC"
    ProcedureReturn
  EndIf
  If FunclistBegin = 0
    Debug "Could not find FUNCLIST:BEGIN in TOC"
    ProcedureReturn
  EndIf
  If FunclistEnd = 0
    Debug "Could not find FUNCLIST:END in TOC"
    ProcedureReturn
  EndIf
  
  TypelistEnd + Len( TypelistBeginMarker )
  FunclistEnd + Len( FunclistEndMarker )
  
  Define.s Prefix = Left( TOC, TypelistBegin - 1 )
  Define.s Midsection = Mid( TOC, TypelistEnd, FunclistBegin - TypelistEnd )
  Define.s Suffix = Right( TOC, Len( TOC ) - FunclistEnd - 1 )
  
  Define.i Index
  
  FileSeek( TOCFile, 0 )
  TruncateFile( TOCFile )
  WriteString( TOCFile, Prefix, #PB_UTF8 )
  WriteString( TOCFile, TypelistBeginMarker, #PB_UTF8 )
  WriteString( TOCFile, ~"\n", #PB_UTF8 )
  For Index = 0 To Code\DefinitionCount - 1
    Define.Definition *Definition = @Code\Definitions( Index )
    If *Definition\DefinitionKind <> #TypeDefinition
      Continue
    EndIf
    Define.s Name = Code\Identifiers( *Definition\Name )
    WriteString( TOCFile, ~"<li><a href=\"Types\\" + Name + ~".html\">" + Name + ~"</a></li>\n" )
  Next
  WriteString( TOCFile, TypelistEndMarker, #PB_UTF8 )
  WriteString( TOCFile, ~"\n", #PB_UTF8 )
  WriteString( TOCFile, Midsection, #PB_UTF8 )
  WriteString( TOCFile, FunclistBeginMarker, #PB_UTF8 )
  WriteString( TOCFile, ~"\n", #PB_UTF8 )
  For Index = 0 To Code\DefinitionCount - 1
    Define.Definition *Definition = @Code\Definitions( Index )
    If *Definition\DefinitionKind <> #MethodDefinition
      Continue
    EndIf
  Next
  WriteString( TOCFile, FunclistEndMarker, #PB_UTF8 )
  WriteString( TOCFile, ~"\n", #PB_UTF8 )
  WriteString( TOCFile, Suffix, #PB_UTF8 )
  
  CloseFile( TOCFile )
  
  ; Reload browser.
  ;;;;FIXME: blanks out the browser...
  ;SetGadgetState( DocViewer, #PB_Web_Refresh )
  
EndProcedure

Procedure OpenDocsForDefinition( DefinitionType.i, Name.s )
EndProcedure

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
  Name.s
  Company.s
  Product.s
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
  FieldCount.i
  FirstField.i
EndStructure

; These are de-duplicated. One instance of the same field is applied to every
; single type it applies to.
Structure GenField
  NextField.i
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
    
    Select *Definition\DefinitionKind
        
      Case #TypeDefinition
        
        If FindMapElement( Types(), Name ) <> #Null
          ;;;;TODO: add diagnostic
          Debug "Type already defined!"
          Continue
        EndIf
        
        Define *GenType.GenType = AddMapElement( Types(), Name )
        
      Case #ProgramDefinition
        
        ;;;;TODO: for now, ensure there is only one of these
        
        Program\Name = Code\Identifiers( *Definition\Name )
        
        Define.i AnnotationIndex = *Definition\FirstAnnotation
        While AnnotationIndex <> -1
          Define.Annotation *Annotation = @Code\Annotations( AnnotationIndex )
          Select *Annotation\AnnotationKind
            Case #ProductAnnotation
              Program\Product = *Annotation\AnnotationText
              
            Case #CompanyAnnotation
              Program\Company = *Annotation\AnnotationText
          EndSelect
          AnnotationIndex = *Annotation\NextAnnotation
        Wend
        
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
  
  ;;;;FIXME: somehow the custom product and company strings we relate aren't coming through in the player builds...
  Program\Product = "DefaultProduct"
  Program\Company = "DefaultCompany"
  
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

Procedure.s ToTypeMessage( *Type.Type )
  ProcedureReturn "t|" + *Type\Name
EndProcedure

Procedure SendProgram()
  
  If UnityPlayerClient = 0
    ProcedureReturn
  EndIf
  
  StartBatchSend( UnityPlayerClient )
  ;;;;TODO: send program delta for incremental changes instead of resetting all the time
  BatchSendString( "reset" )
  
  Define.i Index
  
  ; Send types.
  For Index = 0 To Program\TypeCount - 1
    Define.Type *Type = @Program\Types( Index )
    Define.s Message = ToTypeMessage( *Type )
    BatchSendString( Message )
  Next
  
  ; Send functions.
  
  ; Send objects.
  
  ; Send assets.
  
  BatchSendString( "commit" )
  FinishBatchSend()
  
EndProcedure

Procedure SendProjectSettings()
  
  If UnityEditorClient = 0
    ProcedureReturn
  EndIf
  
  ;;;;TODO: relay name and change name of output executable accordingly
  
  If UnityProjectSettings\ProductName <> Program\Product
    SendString( UnityEditorClient, "set:productName=" + Program\Product )
    UnityProjectSettings\ProductName = Program\Product
  EndIf
  
  If UnityProjectSettings\CompanyName <> Program\Company
    SendString( UnityEditorClient, "set:companyName=" + Program\Company )
    UnityProjectSettings\CompanyName = Program\Company
  EndIf
  
EndProcedure

Global.b LibrariesLoaded = #False
Global.Code LibraryCode

Procedure LoadLibraries()
  
  ResetCode()
  
  If Not LibrariesLoaded
    
    Define.i Directory = ExamineDirectory( #PB_Any, LibrariesPath , "*" + #SOURCE_FILE_EXTENSION )
    If Directory
      
      Define *TextBefore = *Text
      Define TextLengthBefore = TextLength
      
      While NextDirectoryEntry( Directory )
        
        Define.s Path = LibrariesPath + #PS$ + DirectoryEntryName( Directory )
        Debug "Loading library " + Path
        Define.i File = ReadFile( #PB_Any, Path, #PB_UTF8 | #PB_File_SharedRead )
        Define.i Size = DirectoryEntrySize( Directory )
        Define *Buffer = AllocateMemory( Size )
        
        ReadData( File, *Buffer, Size )
        CloseFile( File )
        *Text = *Buffer
        TextLength = Size
        ParseText()
        *Text = #Null
        TextLength = 0
        FreeMemory( *Buffer )
        
      Wend
      
      FinishDirectory( Directory )
      
      *Text = *TextBefore
      TextLength = TextLengthBefore
      
    EndIf
    
    LibrariesLoaded = #True
    CopyStructure( @Code, @LibraryCode, Code )
    
  Else
    
    CopyStructure( @LibraryCode, @Code, Code )
    
  EndIf
  
EndProcedure

Procedure UpdateProgram()
    
  ResetCode()
  LoadLibraries()
  ParseText()
  
  If Code\ErrorCount > 0
    ProcedureReturn
  EndIf
  
  TranslateProgram()
  SendProjectSettings()
  
  If Code\ErrorCount > 0
    ProcedureReturn
  EndIf
  
  SendProgram()
  GenerateDocs()
  
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

;==============================================================================
; Main loop.

UpdateProgram()
Status( "Waiting for Unity editor to connect..." )

Repeat
  
  Define Event = WaitWindowEvent()
  
  ;;;;TODO: If F1 is hit with focus on text editor, look up word under cursor in docs and point docs browser to that
  
  If Event = #PB_Event_Timer
    Select EventTimer()
        Case #WINDOW_SAVE_TIMER
          FlushText()
          
        Case #WINDOW_NETWORK_TIMER
          Select NetworkServerEvent( Server )
            Case #PB_NetworkEvent_Connect
              Select UnityStatus
                  
                Case #WaitingForEditorToConnect
                  UnityEditorClient = EventClient()
                  Status( "Waiting for Unity editor to build player..." )
                  SendProjectSettings()
                  SendString( UnityEditorClient, "build" )
                  UnityStatus = #WaitingForEditorToBuildPlayer
                  
                Case #WaitingForPlayerToConnect
                  UnityPlayerClient = EventClient()
                  Status( "Unity player connected." )
                  SendProgram()
                  UnityStatus = #ReadyState
                  
              EndSelect
              
            Case #PB_NetworkEvent_Disconnect
              If EventClient() = UnityEditorClient
                Status( "Unity editor disconnected." )
                UnityEditorClient = 0
                UnityStatus = #BadState
              ElseIf EventClient() = UnityPlayerClient
                Status( "Unity player disconnected." )
                UnityPlayerClient = 0
                UnityStatus = #BadState
              EndIf
              
            Case #PB_NetworkEvent_Data
              Define.i ReadResult = ReceiveNetworkData( EventClient(), *UnityNetworkBuffer, #MAX_MESSAGE_LENGTH )
              If ReadResult <= 0
                Debug "Read failure!!"
              Else
                Define.s Text = PeekS( *UnityNetworkBuffer, ReadResult, #PB_UTF8 | #PB_ByteLength )
                Debug "Data " + Text
                Select UnityStatus
                    
                  Case #WaitingForEditorToBuildPlayer
                    If EventClient() = UnityEditorClient
                      If Text = "build failure"
                        ;;;;TODO: handle failure
                        Status( "Build failed!" )
                        UnityStatus = #BadState
                      Else
                        UnityStatus = #WaitingForPlayerToConnect
                        Status( "Waiting for Unity player to connect..." )
                        Define.i HWND = GadgetID( PlayerContainer )
                        UnityPlayer = RunProgram( UnityPlayerExecutablePath, "-parentHWND " + Str( HWND ), "", #PB_Program_Open | #PB_Program_Read )
                      EndIf
                    EndIf
                    
                EndSelect
              EndIf
          EndSelect
      EndSelect
  EndIf
  
Until Event = #PB_Event_CloseWindow

Status( "Exiting." )
FlushText( #False )
GOSCI_Free( Scintilla )
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
;[X] Program is generated
;[X] Unity player connects to IDE
;[X] Program is transferred To player
;[X] Can parse functions
;[X] Can parse return statement
;[X] "Standard" library is automatically injected
;[X] Can parse expressions
;[X] Add DOTS to Unity project
;[X] Can add comments
;[X] Methods can have value parameters
;[ ] Can invoke methods
;[ ] Can have conditional branches
;[ ] Can have loops
;[ ] Methods are translated
;[ ] Program is being run
;[ ] Can add asset to project
;[ ] Assets are being built and rebuilt into asset bundles
;[ ] Player can load asset bundles
;[ ] Can add&render display element (sprite or model)
;[ ] Can animate display element in code
;[ ] Program is migrated from one run to the next

; ....

;[ ] Can write parameterized types
;[ ] Can instantiate parameterized types
;[ ] Can write parameterized methods
;[ ] Can invoke parameterized methods

; ...

;[ ] Dark theme for text editor
;[ ] Diagnostics are shown as annotations on code
;[ ] Can generate docs from code
;[ ] Can jump to docs by pressing F1
;[ ] Can jump to definition by pressing F2
;[ ] Can jump to definition by opening goto popup
;[ ] Can run tests from code in player
;[ ] Syntax highlighting in text editor
;[ ] Auto-completion in text editor
;[ ] Make parser predictive and the syntax more liberal

; ....

;[ ] Vim mode

; Optimizations
; - Make a pass over the structs and make them more compact (indices can be 32bit, for example)

; What I want
; - All tests are being run continuously on all connected players (smart execution to narrow down run sets)
; - Everything is saved automatically
; - Changes are picked up automatically and reload the application automatically to a known state and from there, continue with the changes applied
; - The toolchain is configured entirely from within annotations in the code

; Plan
; Wednesday: Program generation and *execution*
; Thursday: Asset import and rendering (engine stuff)
; Friday: Polish (cosmetics and bells&whistles)


; Qs:
; - How do I not make it depend on total program size but rather on change size?
; - How would scenes be created in a graphical way?
; - Where do we display log and debug output?
; IDE Options = PureBasic 5.73 LTS (Windows - x64)
; CursorPosition = 1089
; FirstLine = 1058
; Folding = ----------
; EnableXP