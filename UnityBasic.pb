
EnableExplicit

IncludePath "GoScintilla/"
XIncludeFile "GoScintilla.pbi"

#SOURCE_FILE_EXTENSION = ".ubasic"
#UNITY_SERVER_PORT = 10978
#DOC_SERVER_PORT = 17790

Global.s UnityEditorExecutablePath = "C:\Program Files\Unity\Hub\Editor\2020.3.5f1\Editor\Unity.exe"
Global.s UnityPlayerExecutablePath = "C:\Dropbox\Workspaces\UnityBasic_PB\UnityProject\Builds\UnityBasic64.exe"
Global.s GeneratedProjectPath = "C:\Dropbox\Workspaces\UnityBasic_PB\UnityProject"
Global.s GeneratedDocsPath = "C:\Dropbox\Workspaces\UnityBasic_PB\Docs";;;;TODO: kill this
Global.s DocsTemplatePath = "C:\Dropbox\Workspaces\UnityBasic_PB\Docs"
Global.s SourceProjectPath = "C:\Dropbox\Workspaces\UnityBasic_PB\TestProject"
Global.s LibrariesPath = "C:\Dropbox\Workspaces\UnityBasic_PB\Libs"
Global.s TextFilePath = SourceProjectPath + "\TestFile" + #SOURCE_FILE_EXTENSION
Global.s DocURL = "http://127.0.0.1:" + Str( #DOC_SERVER_PORT )

#SPACE = 32
#NEWLINE = 10
#RETURN = 13
#EQUALS = 61

;==============================================================================
;-== UI Setup.

InitScintilla()
InitNetwork()
ExamineDesktops()

; Create a server for communicating with Unity processes (both editor and player).
Define.i UnityServer = CreateNetworkServer( #PB_Any, #UNITY_SERVER_PORT )
If UnityServer = 0
  Debug "Cannot create Unity server!!"
  ;;;;TODO: error
EndIf

; Create a server for serving content to the embedded web browser (though it can be
; accessed by any other browser just the same).
Define.i DocServer = CreateNetworkServer( #PB_Any, #DOC_SERVER_PORT )
If DocServer = 0
  Debug "Cannot create doc server!!"
  ;;;;TODO: error
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
Global.i DocViewer = WebGadget( #PB_Any, ContentWidth / 2, 0, ContentWidth / 2, ContentHeight / 2, DocURL )
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

GOSCI_AddKeywords( Scintilla, "TYPE METHOD FIELD OBJECT PROGRAM BEGIN END RETURN ABSTRACT IMMUTABLE MUTABLE IF LOOP", #StyleKeyword )
GOSCI_AddKeywords( Scintilla, "|DESCRIPTION |DETAILS |COMPANY |PRODUCT |CATEGORY |ICALL |PRAGMA", #StyleAnnotation )

GOSCI_SetLexerOption( Scintilla, #GOSCI_LEXEROPTION_SEPARATORSYMBOLS, @"=+-*/%()[],.;" )

Global.i TabWidth = 4
Global.b UseSoftTabs = #True

GOSCI_SetTabs( Scintilla, TabWidth, UseSoftTabs )
ScintillaSendMessage( Scintilla, #SCI_SETINDENTATIONGUIDES, #SC_IV_REAL )

SetActiveGadget( Scintilla )

;==============================================================================
;-== Networking And child processes.

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

Procedure.s ReceiveString( Client.i )
  
  Define.i ReadResult = ReceiveNetworkData( EventClient(), *UnityNetworkBuffer, #MAX_MESSAGE_LENGTH )
  If ReadResult <= 0
    Debug "Read failure!!"
    ProcedureReturn ""
  EndIf
  
  Define.s Text = PeekS( *UnityNetworkBuffer, ReadResult, #PB_UTF8 | #PB_ByteLength )
  ProcedureReturn Text
                
EndProcedure

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
;-== Utilities.

Procedure.i SplitString( Array Split.s( 1 ), String.s, Delimiter.s )
  
  Define.i DelimiterLen = Len( Delimiter )
  Define.i StringLen = Len( String )
  
  Define.i Count = CountString( String, Delimiter )
  If StringLen > DelimiterLen And FindString( String, Delimiter, StringLen + 1 - DelimiterLen ) = 0
    Count + 1
  EndIf
  ReDim Split( Count )
  
  Define.i Index
  Define.i Position = 1
  For Index = 0 To Count - 1
    Define.i EndPos = FindString( String, Delimiter, Position )
    If EndPos = 0
      EndPos = Len( String ) + 1
    EndIf
    Define.s Fragment = Mid( String, Position, EndPos - Position )
    Split( Index ) = Fragment
    Position = EndPos + DelimiterLen
  Next
  
  ProcedureReturn Count
  
EndProcedure

Structure StringBuilder
  *Buffer
  Capacity.i
  Length.i
EndStructure

Procedure.i AppendString( *Builder.StringBuilder, String.s )
  
  Define.i ByteLength = StringByteLength( String, #PB_UTF8 )
  If *Builder\Length + ByteLength > *Builder\Capacity
    *Builder\Capacity + ByteLength + 1024
    *Builder\Buffer = ReAllocateMemory( *Builder\Buffer, *Builder\Capacity )
  EndIf
  
  Define *Ptr = *Builder\Buffer + *Builder\Length
  PokeS( *Ptr, String, Len( String ), #PB_UTF8 | #PB_String_NoZero )
  *Builder\Length + ByteLength
  
  ProcedureReturn *Ptr
  
EndProcedure

Procedure FreeStringBuilder( *Builder.StringBuilder )
  If *Builder\Buffer <> #Null
    FreeMemory( *Builder\Buffer )
  EndIf
  *Builder\Buffer = #Null
  *Builder\Length = 0
  *Builder\Capacity = 0
EndProcedure

Procedure ResetStringBuilder( *Builder.StringBuilder )
  *Builder\Length = 0
EndProcedure

Procedure.s GetString( *Builder.StringBuilder, FreeBuilder.b = #True )
  Define.s String
  If *Builder\Length <> 0
    String = PeekS( *Builder\Buffer, *Builder\Length, #PB_UTF8 | #PB_ByteLength )
  Else
    String = ""
  EndIf
  If FreeBuilder
    FreeStringBuilder( *Builder )
  EndIf
  ProcedureReturn String
EndProcedure


;==============================================================================
; HTTP.

Enumeration HTTPMethod
  #HTTPGet
  #HTTPPost
EndEnumeration

Structure HTTPRequest
  Method.i
  Path.s
  Map Headers.s()
EndStructure

Structure HTTPResponse
  StatusCode.i
  Body.s
  Map Headers.s()
EndStructure

Procedure.b ParseHTTPRequest( Text.s, *Request.HTTPRequest )
  
  ResetStructure( *Request, HTTPRequest )
  
  Dim Lines.s( 0 )
  SplitString( Lines(), Text, ~"\n" )
  If ArraySize( Lines() ) < 1
    ProcedureReturn #False
  EndIf
  
  Dim Fragments.s( 0 )
  If SplitString( Fragments(), Lines( 0 ), " " ) <> 3
    ProcedureReturn #False
  EndIf
  
  Select Fragments( 0 )
    Case "GET"
      *Request\Method = #HTTPGet
    Case "POST"
      *Request\Method = #HTTPPost
    Default
      *Request\Method = -1
  EndSelect
  
  *Request\Path = Fragments( 1 )
      
  ProcedureReturn #True
      
EndProcedure

Procedure.s HTTPStatusCodeToString( StatusCode.i )
  Select StatusCode
    Case 200
      ProcedureReturn "OK"
    Default
      ProcedureReturn "Hmpf..."
  EndSelect
EndProcedure

Procedure.s FormatHTTPResponse( *Response.HTTPResponse )
  
  Define.s Status = "HTTP/1.1 " + Str( *Response\StatusCode ) + " " + HTTPStatusCodeToString( *Response\StatusCode ) + ~"\n"
  
  Define.s Headers = "Content-Length: " + Len( *Response\Body ) + ~"\n"
  ForEach *Response\Headers()
    Define.s Value = *Response\Headers()
    Headers + MapKey( *Response\Headers() ) + ": " + Value + ~"\n"
  Next
  
  ProcedureReturn Status + Headers + ~"\n" + *Response\Body
  
EndProcedure

ProcedureUnit CanParseSimpleHTTPRequest()
  Define.s Text = ~"GET / HTTP/1.1\n" +
                  ~"Host: developer.mozilla.org\n" +
                  ~"Accept-Language: fr"
  Define.HTTPRequest Request
  Assert( ParseHTTPRequest( Text, @Request ) = #True )
  Assert( Request\Method = #HTTPGet )
  Assert( Request\Path = "/" )
  ;;;;TODO: headers
EndProcedureUnit

ProcedureUnit CanFormatSimpleHTTPResponse()
  Define.HTTPResponse Response
  Response\StatusCode = 200
  AddMapElement( Response\Headers(), "Content-Type" )
  Response\Headers() = "text/html"
  Response\Body = "Great!"
  ;;;;TODO: headers
  Define.s Expected = ~"HTTP/1.1 200 OK\n" +
                      ~"Content-Length: 6\n" +
                      ~"Content-Type: text/html\n" +
                      ~"\n" +
                      ~"Great!"
  Assert( FormatHTTPResponse( @Response ) = Expected )
EndProcedureUnit

;==============================================================================
;-== Abstract syntax.

;;;;TODO: figure out how to deal with separate files feeding into inputs
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
  
  ; Misc annotations.
  #IcallAnnotation
  #PragmaAnnotation
  
EndEnumeration

; Annotations are free-form text blobs that can be attached to definitions.
; They are used as instructions for the tooling.
Structure Annotation
  AnnotationKind.i
  AnnotationText.s
  NextAnnotation.i
EndStructure

Enumeration ClauseKind
  #PreconditionClause   ; 'requires'
  #PostconditionClause  ; 'ensures'
  #InvariantClause
  #WhereClause
EndEnumeration

; Clauses are optional forms that can be tagged onto definitions.
; They are used for things such as pre- and postconditions.
Structure Clause
  Expression.i
  NextClause.i
EndStructure

#INVALID_TYPE_ID = 0

; During parsing, we assign negative type IDs as stand-ins for types of literals.
; During compilation, these are replaced with actual type IDs.
Enumeration LiteralType
  #IntegerLiteralType = -1
  #FloatLiteralType = -2
  #StringLiteralType = -3
  #NothingLiteralType = -4 ; Empty tuple expression '()'. Not translated to 'Nothing' as that would be affected by name lookup rules.
EndEnumeration

Enumeration Operator
  #LiteralExpression = 1
  #NameExpression
  #AndExpression
  #OrExpression
  #NotExpression
  #BitwiseAndExpression
  #BitwiseOrExpression
  #ApplyExpression ; For types, this instantiates the given named type.
  #TupleExpression
  #ArrowExpression ; A -> B (type context: function type, value context: anonymous function)
EndEnumeration

Enumeration ExpressionContext
  #ValueContext = 1
  #TypeContext
EndEnumeration

; Type and value expressions use the same data format.
; All expressions are either unary or binary.
Structure Expression
  Operator.b
  Context.b
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
  #ExpressionStatement = 1
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
  #TypeDefinition = 1
  #MethodDefinition
  #FieldDefinition
  #FeatureDefinition
  #ModuleDefinition
  #LibraryDefinition
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
  #IsAlias ; for '=' type definitions.
  #IsPrivate
EndEnumeration

Structure Parameter
  Name.i
  TypeExpression.i
  DefaultExpression.i ; -1 if none.
  NextParameter.i
EndStructure

Structure Definition
  Name.i
  Scope.i
  DefinitionKind.i
  Flags.i
  TypeExpression.i
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

Enumeration NamingConvention
  #GnuCase ; foo_bar
  #JavaCase ; fooBar
  #PascalCase ; FooBar
EndEnumeration

Global.i DefaultNamingConvention = #PascalCase

Procedure.s FormatIdentifier( Identifier.s, NamingConvention.i = -1 )
  
  If NamingConvention = -1
    NamingConvention = DefaultNamingConvention
  EndIf
  
  If NamingConvention = #GnuCase
    ProcedureReturn Identifier
  EndIf
  
  Dim Parts.s( 0 )
  SplitString( Parts(), Identifier, "_" )
  Define.i NumParts = ArraySize( Parts() )
  
  Define.StringBuilder Builder
  Define.i Index
  For Index = 0 To NumParts - 1
    Define *Ptr = AppendString( @Builder, Parts( Index ) )
    ; Uppercase first letter.
    If Index <> 0 Or NamingConvention <> #JavaCase
      PokeB( *Ptr, PeekB( *Ptr ) - ( 97 - 65 ) )
    EndIf
  Next
  
  Define.s Result = GetString( @Builder )
  ProcedureReturn Result
  
EndProcedure

ProcedureUnit CanFormatIdentifier()
  Assert( FormatIdentifier( "foo_bar", #GnuCase ) = "foo_bar" )
  Assert( FormatIdentifier( "foo_bar", #JavaCase ) = "fooBar" )
  Assert( FormatIdentifier( "foo_bar", #PascalCase ) = "FooBar" )
EndProcedureUnit
  
Procedure.s ExpressionKindToString( ExpressionKind.i )
  Select ExpressionKind
    Case #ApplyExpression
      ProcedureReturn "Apply"
    Case #TupleExpression
      ProcedureReturn "Tuple"
    Case #LiteralExpression
      ProcedureReturn "Literal"
    Case #NameExpression
      ProcedureReturn "Name"
    Case #AndExpression
      ProcedureReturn "And"
    Case #OrExpression
      ProcedureReturn "Or"
    Case #NotExpression
      ProcedureReturn "Not"
    Case #ArrowExpression
      ProcedureReturn "Arrow"
    Default
      ProcedureReturn "<Unknown>"
  EndSelect
EndProcedure
  
Procedure.s DefinitionKindToString( DefinitionKind.i )
  Select DefinitionKind
    Case #TypeDefinition
      ProcedureReturn "Type"
    Case #MethodDefinition
      ProcedureReturn "Method"
    Case #ProgramDefinition
      ProcedureReturn "Program"
    Default
      ProcedureReturn "???"
  EndSelect
EndProcedure

Procedure.i FindAnnotation( *Definition.Definition, AnnotationKind.i )
  
  Define.i AnnotationIndex = *Definition\FirstAnnotation
  While AnnotationIndex <> -1
    
    If Code\Annotations( AnnotationIndex )\AnnotationKind = AnnotationKind
      ProcedureReturn AnnotationIndex
    EndIf
    
    AnnotationIndex = Code\Annotations( AnnotationIndex )\NextAnnotation
    
  Wend
  
  ProcedureReturn -1
  
EndProcedure

Procedure HasAnnotation( *Definition.Definition, AnnotationKind.i )
  ProcedureReturn Bool( FindAnnotation( *Definition, AnnotationKind ) <> -1 )
EndProcedure

;==============================================================================
;-== Parsing.
;;;;TODO: put this on a thread

Structure ParserLocationState
  *Position
  LastRegion.TextRegion
  CurrentLine.i
  CurrentColumn.i
EndStructure

Structure Parser Extends ParserLocationState
  *EndPosition
  *StartPosition
  CurrentScope.i
  CurrentDefinitionInScope.i
  CurrentStatement.i
  CurrentExpressionContext.i
  FileName.s
  NameBufferSize.l
  NameBuffer.s
  StringBufferSize.l
  StringBuffer.s
EndStructure

Macro SaveParserLocation( Parser, LocationVariable )
  Define.ParserLocationState LocationVariable
  LocationVariable\Position = Parser\Position
  LocationVariable\LastRegion = Parser\LastRegion
  LocationVariable\CurrentLine = Parser\CurrentLine
  LocationVariable\CurrentColumn = Parser\CurrentColumn
EndMacro

Macro RestoreParserLocation( Parser, LocationVariable )
  Parser\Position = LocationVariable\Position
  Parser\LastRegion = LocationVariable\LastRegion
  Parser\CurrentLine = LocationVariable\CurrentLine
  Parser\CurrentColumn = LocationVariable\CurrentColumn
EndMacro

Macro PushExpressionContext( Parser, Context )
  Define.i PreviousExpressionContext = Parser\CurrentExpressionContext
  Parser\CurrentExpressionContext = Context
EndMacro

Macro PopExpressionContext( Parser )
  Parser\CurrentExpressionContext = PreviousExpressionContext
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
  Code\Scopes( CurrentScope )\Parent = PreviousScope
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
  *Expression\Context = *Parser\CurrentExpressionContext
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
  *Expression\Context = *Parser\CurrentExpressionContext
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
    Debug Str( *Parser\CurrentLine ) + ": Expecting " + Symbol + " but got " + Chr( PeekB( *Parser\Position ) )
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
  If *Parser\NameBufferSize < ( Length + 1 ) * 2
    *Parser\NameBufferSize = ( Length + 1 ) * 2
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
  ElseIf MatchToken( *Parser, "pragma", 6 )
    AnnotationKind = #PragmaAnnotation
  ElseIf MatchToken( *Parser, "icall", 5 )
    AnnotationKind = #IcallAnnotation
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
    Text = Trim( Text )
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
  Define.i Type = #INVALID_TYPE_ID
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
    
    Type = #IntegerLiteralType
    FirstOperandI = IntegerPart
    
  ElseIf Char = '"'
        
    ; Strings.
    Define.l StringLength = 0
    
    Macro PushChar( Char )
      If StringLength = *Parser\StringBufferSize
        *Parser\StringBufferSize + 1024
        Define.s NewBuffer = Space( *Parser\StringBufferSize )
        If *Parser\StringBuffer
          CopyMemory( @*Parser\StringBuffer, @NewBuffer, StringLength * SizeOf( Character ) )
        EndIf
        *Parser\StringBuffer = NewBuffer
      EndIf
      PokeC( @*Parser\StringBuffer + StringLength * SizeOf( Character ), Char )
      StringLength + 1
    EndMacro
    
    *Parser\Position + 1
    While *Parser\Position < *Parser\EndPosition
      Char = PeekB( *Parser\Position )
      If Char = '"'
        Break
      EndIf
      ;;;;TODO: escape sequences (including unicode chars)
      PushChar( Char )
      *Parser\Position + 1
    Wend
    
    If *Parser\Position = *Parser\EndPosition Or PeekB( *Parser\Position ) <> '"'
      ;;;;TODO: diagnose
      Debug "Missing doublequote in string literal"
    Else
      *Parser\Position + 1
    EndIf
    
    PushChar( #NUL )
    
    If Not FindMapElement( Code\StringLiterals(), *Parser\StringBuffer )
      FirstOperandI = MapSize( Code\StringLiterals() )
      AddMapElement( Code\StringLiterals(), *Parser\StringBuffer )
      Code\StringLiterals() = FirstOperandI
    Else
      FirstOperandI = Code\StringLiterals()
    EndIf
    
    Type = #StringLiteralType
    Operator = #LiteralExpression
    
    UndefineMacro PushChar
    
  ElseIf IsAlpha( Char ) Or Char = '_'
    
    Operator = #NameExpression
    FirstOperandI = ParseIdentifier( *Parser )
    
  ElseIf Char = '('
    
    ; Either a simple parenthesized expression or a tuple expression.
    ; Depends on the number of elements.
    
    *Parser\Position + 1
    
    SkipWhitespace( *Parser )
    If MatchToken( *Parser, ")", 1, #True )
      ; Empty tuple expression '()'. Semantically identical to 'Nothing' object.
      Operator = #LiteralExpression
      Type = #NothingLiteralType
    Else
      
      Define.i FirstExpression = ParseExpression( *Parser )
      If FirstExpression = -1
        ;;;;TODO: diagnose
        Debug "Expecting expression!"
      EndIf
      
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
      
    EndIf
  
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
  
  ; Check for dot expressions.
  Define.b IsDotExpression = #False
  SkipWhitespace( *Parser )
  If MatchToken( *Parser, ".", 1, #True )
    
    Define.i SecondExpression = ParseBasicExpression( *Parser )
    If SecondExpression <> -1
      ; Note the inversion of the operands. "A.B" -> "B( A )"
      MakeExpressionOp2I( DotExpr, #ApplyExpression, #INVALID_TYPE_ID, SecondExpression, FirstExpression, Code\Expressions( FirstExpression )\Region\LeftPos )
      IsDotExpression = #True
      FirstExpression = DotExpr
    EndIf
    
    SkipWhitespace( *Parser )
    
  EndIf
  
  ;;;;REVIEW: should this be considered a binary expression instead?
  ; ATM we allow only parenthesized and tuple expressions to form apply expressions.
  ; With a more powerful parser, that could be changed but there's a lot of ambiguity
  ; that needs to be figured out on that path.
  If *Parser\Position < *Parser\EndPosition
    
    Define.i SecondExpression = -1
    
    Define.c Char = PeekB( *Parser\Position ) 
    If *Parser\CurrentExpressionContext = #ValueContext
      If Char = '('
        SecondExpression = ParseBasicExpression( *Parser )
      ElseIf Char = '<'
        ; Angle bracket in value context switches us into type context.
        PushExpressionContext( *Parser, #TypeContext )
        SecondExpression = ParseBasicExpression( *Parser )
        PopExpressionContext( *Parser )
      EndIf
    Else
      ; In type context, '<' is nesting and '(' is grouping.
      If Char = '<' Or Char = '('
        SecondExpression = ParseBasicExpression( *Parser )
      EndIf
    EndIf
    
    If SecondExpression <> -1
      ; If the left side is a dot expression, create a tuple expression. "A.B( C )" -> "B( A, C )"
      If IsDotExpression
        MakeExpressionOp2I( TupleExpression, #TupleExpression, #INVALID_TYPE_ID, Code\Expressions( FirstExpression )\SecondOperandI, SecondExpression, Code\Expressions( FirstExpression )\Region\LeftPos )
        Code\Expressions( FirstExpression )\SecondOperandI = TupleExpression
        ProcedureReturn FirstExpression
      Else
        MakeExpressionOp2I( ApplyExpr, #ApplyExpression, #INVALID_TYPE_ID, FirstExpression, SecondExpression, Code\Expressions( FirstExpression )\Region\LeftPos )
        ProcedureReturn ApplyExpr
      EndIf
    EndIf
  EndIf
  
  ProcedureReturn FirstExpression
  
EndProcedure

Procedure.i ParseUnaryPrefixExpression( *Parser.Parser )
  
  SkipWhitespace( *Parser )
  
  Define.i LeftPos = *Parser\Position - *Parser\StartPosition
  Define.i PrefixOperator = -1
  If MatchToken( *Parser, "!", 1, #True )
    PrefixOperator = #NotExpression
  EndIf
  
  Define.i Expression = ParseUnaryPostfixExpression( *Parser )
  
  If PrefixOperator <> -1
    MakeExpressionOpI( UnaryExpression, PrefixOperator, #INVALID_TYPE_ID, Expression, LeftPos )
    Expression = UnaryExpression
  EndIf
  
  ProcedureReturn Expression
  
EndProcedure

Procedure.i ParseBinaryExpression( *Parser.Parser )
  
  Define.i Expression = ParseUnaryPrefixExpression( *Parser )
  
  SkipWhitespace( *Parser )
  
  Define.i Operator = -1
  
  If *Parser\CurrentExpressionContext = #ValueContext
    If MatchToken( *Parser, "&&", 2, #True )
      Operator = #AndExpression
    ElseIf MatchToken( *Parser, "||", 2, #True )
      Operator = #OrExpression
    EndIf
  EndIf
  
  If Operator = -1
    
    If MatchToken( *Parser, "&", 1, #True )
      
      If *Parser\CurrentExpressionContext = #TypeContext
        Operator = #AndExpression
      Else
        Operator = #BitwiseAndExpression
      EndIf
      
    ElseIf MatchToken( *Parser, "|", 1, #True )
      
      If *Parser\CurrentExpressionContext = #TypeContext
        Operator = #OrExpression
      Else
        Operator = #BitwiseOrExpression
      EndIf
      
    EndIf
    
  EndIf
  
  ;;;;TODO: deal with left/right association properly
  
  If Operator <> -1
    
    Define.i RightExpression = ParseExpression( *Parser )
    
    MakeExpressionOp2I( BinaryExpression, Operator, #INVALID_TYPE_ID, Expression, RightExpression, Code\Expressions( Expression )\Region\LeftPos )
    Expression = BinaryExpression
    
  EndIf

  ProcedureReturn Expression
  
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
    
    ReferencedIndex = ParseExpression( *Parser )
    If ReferencedIndex <> -1
      
      Statement = #ExpressionStatement
      ExpectSymbol( *Parser, ";", 1 )
      
    Else
      ProcedureReturn -1
    EndIf
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
    
    ; Parameters can take two forms: 'A' or 'A : B'. We don't know which it is until we've seen or not
    ; seen the ':'. We store the current parser location and try to parse 'A : B'. If that works, that's it.
    ; If we don't see the ':', we back up to where we started and parse just a type expression instead.
    
    SaveParserLocation( *Parser, StartLocation )
    
    Define.i Name = ParseIdentifier( *Parser )
    Define.i TypeExpression = -1
    
    If Name = -1
      Break
    EndIf
    
    SkipWhitespace( *Parser )
    If Not MatchToken( *Parser, ":", 1, #True )
      ; Back up to where we started. We know that we at least have a name
      ; expression found at the location.
      Name = -1
      RestoreParserLocation( *Parser, StartLocation )
    EndIf
    
    PushExpressionContext( *Parser, #TypeContext )
    TypeExpression = ParseExpression( *Parser )
    If TypeExpression = -1
      ;;;;TODO: add diagnostic
      Debug "Expecting type!!"
    EndIf
    PopExpressionContext( *Parser )
    
    Define.i ParameterIndex = Code\ParameterCount
    If ArraySize( Code\Parameters() ) = ParameterIndex
      ReDim Code\Parameters( ParameterIndex + 512 )
    EndIf
    Code\ParameterCount + 1
    
    Define.Parameter *Parameter = @Code\Parameters( ParameterIndex )
    
    *Parameter\Name = Name
    *Parameter\TypeExpression = TypeExpression
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
  *Definition\TypeExpression = -1
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
    
    PushExpressionContext( *Parser, #TypeContext )
    *Definition\FirstTypeParameter = ParseParameterList( *Parser )
    PopExpressionContext( *Parser )
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
    PushExpressionContext( *Parser, #TypeContext )
    *Definition\TypeExpression = ParseExpression( *Parser )
    PopExpressionContext( *Parser )
  ElseIf MatchToken( *Parser, "=", 1, #True )
    PushExpressionContext( *Parser, #TypeContext )
    *Definition\TypeExpression = ParseExpression( *Parser )
    *Definition\Flags | #IsAlias
    PopExpressionContext( *Parser )
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
  Parser\CurrentExpressionContext = #ValueContext
  
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

;{ TESTS

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
  Parser\CurrentExpressionContext = #ValueContext
  Define.i ParserResult = Fn( @Parser )
  Assert( ParserResult = Expected )
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

ProcedureUnit CanParseIntegerLiteral()
  ResetCode()
  TestParseText( @ParseExpression(), "01234", 0 )
  Assert( Code\ExpressionCount = 1 )
  Assert( Code\Expressions( 0 )\Operator = #LiteralExpression )
  Assert( Code\Expressions( 0 )\Context = #ValueContext )
  Assert( Code\Expressions( 0 )\Type = #IntegerLiteralType )
  Assert( Code\Expressions( 0 )\FirstOperandI = 1234 )
EndProcedureUnit

ProcedureUnit CanParseStringLiteral()
  ResetCode()
  TestParseText( @ParseExpression(), ~"  \"abc\" ", 0 )
  Assert( Code\ExpressionCount = 1 )
  Assert( Code\Expressions( 0 )\Operator = #LiteralExpression )
  Assert( Code\Expressions( 0 )\Context = #ValueContext )
  Assert( Code\Expressions( 0 )\Type = #StringLiteralType )
  Assert( Code\Expressions( 0 )\FirstOperandI = 0 )
  Assert( MapSize( Code\StringLiterals() ) = 1 )
  Assert( FindMapElement( Code\StringLiterals(), "abc" ) <> #Null )
  Assert( Code\StringLiterals() = 0 )
EndProcedureUnit

ProcedureUnit CanParseNameExpression()
  ResetCode()
  TestParseText( @ParseExpression(), "test", 0 )
  Assert( Code\ExpressionCount = 1 )
  Assert( Code\IdentifierCount = 1 )
  Assert( Code\Identifiers( 0 ) = "test" )
  Assert( Code\Expressions( 0 )\Operator = #NameExpression )
  Assert( Code\Expressions( 0 )\Context = #ValueContext )
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
  Assert( Code\Expressions( 0 )\Context = #ValueContext )
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

; A.B is equivalent to B( A )
ProcedureUnit CanParseDotExpression()
  ResetCode()
  TestParseText( @ParseExpression(), "a.b", 2 )
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
  Assert( Code\Expressions( 2 )\FirstOperandI = 1 ) ; Note the inversion here.
  Assert( Code\Expressions( 2 )\SecondOperandI = 0 )
  Assert( Code\Expressions( 2 )\NextExpression = -1 )
EndProcedureUnit

; A.B( C ) is equivalent to B( A, C )
ProcedureUnit CanParseDotExpressionWithArguments()
  ResetCode()
  TestParseText( @ParseExpression(), "a.b( c )", 2 )
  Assert( Code\ExpressionCount = 5 )
  Assert( Code\IdentifierCount = 3 )
  Assert( Code\Identifiers( 0 ) = "a" )
  Assert( Code\Identifiers( 1 ) = "b" )
  Assert( Code\Identifiers( 2 ) = "c" )
  Assert( Code\Expressions( 0 )\Operator = #NameExpression )
  Assert( Code\Expressions( 0 )\FirstOperandI = 0 )
  Assert( Code\Expressions( 0 )\NextExpression = -1 )
  Assert( Code\Expressions( 1 )\Operator = #NameExpression )
  Assert( Code\Expressions( 1 )\FirstOperandI = 1 )
  Assert( Code\Expressions( 1 )\NextExpression = -1 )
  ; The reverse order of expressions here is because
  ; the parser first sees the 'a.b' and then the '( c )'. It then modifies
  ; the existing apply expression it created for 'a.b' after creating a new
  ; tuple expression for '( a, c )'.
  Assert( Code\Expressions( 2 )\Operator = #ApplyExpression )
  Assert( Code\Expressions( 2 )\FirstOperandI = 1 )
  Assert( Code\Expressions( 2 )\SecondOperandI = 4 )
  Assert( Code\Expressions( 2 )\NextExpression = -1 )
  Assert( Code\Expressions( 3 )\Operator = #NameExpression )
  Assert( Code\Expressions( 3 )\FirstOperandI = 2 )
  Assert( Code\Expressions( 3 )\NextExpression = -1 )
  Assert( Code\Expressions( 4 )\Operator = #TupleExpression )
  Assert( Code\Expressions( 4 )\FirstOperandI = 0 )
  Assert( Code\Expressions( 4 )\SecondOperandI = 3 )
  Assert( Code\Expressions( 4 )\NextExpression = -1 )
EndProcedureUnit

ProcedureUnit CanParseApplyNothingExpression()
  ResetCode()
  TestParseText( @ParseExpression(), "a()", 2 )
  Assert( Code\ExpressionCount = 3 )
  Assert( Code\IdentifierCount = 1 )
  Assert( Code\Identifiers( 0 ) = "a" )
  Assert( Code\Expressions( 0 )\Operator = #NameExpression )
  Assert( Code\Expressions( 0 )\Context = #ValueContext )
  Assert( Code\Expressions( 0 )\FirstOperandI = 0 )
  Assert( Code\Expressions( 0 )\NextExpression = -1 )
  Assert( Code\Expressions( 1 )\Operator = #LiteralExpression )
  Assert( Code\Expressions( 1 )\Context = #ValueContext )
  Assert( Code\Expressions( 1 )\Type = #NothingLiteralType )
  Assert( Code\Expressions( 1 )\NextExpression = -1 )
  Assert( Code\Expressions( 2 )\Operator = #ApplyExpression )
  Assert( Code\Expressions( 2 )\Context = #ValueContext )
  Assert( Code\Expressions( 2 )\FirstOperandI = 0 )
  Assert( Code\Expressions( 2 )\SecondOperandI = 1 )
  Assert( Code\Expressions( 2 )\NextExpression = -1 )
EndProcedureUnit

CompilerIf #False
ProcedureUnit CanParseComplexApplyExpression()
  ResetCode()
  TestParseText( @ParseExpression(), "a.b< c, d >( 1, ( 2, 3 ), 4 )", 2 )
  Assert( Code\ExpressionCount = 5 )
  Assert( Code\IdentifierCount = 4 )
  Assert( Code\Identifiers( 0 ) = "a" )
  Assert( Code\Identifiers( 1 ) = "b" )
  Assert( Code\Identifiers( 2 ) = "c" )
  Assert( Code\Identifiers( 3 ) = "d" )
  Assert( Code\Expressions( 0 )\Operator = #NameExpression ) ; a
  Assert( Code\Expressions( 0 )\Context = #ValueContext )
  Assert( Code\Expressions( 0 )\FirstOperandI = 0 )
  Assert( Code\Expressions( 0 )\NextExpression = -1 )
  Assert( Code\Expressions( 1 )\Operator = #NameExpression ) ; b
  Assert( Code\Expressions( 1 )\Context = #ValueContext )
  Assert( Code\Expressions( 1 )\FirstOperandI = 1 )
  Assert( Code\Expressions( 1 )\NextExpression = -1 )
  Assert( Code\Expressions( 2 )\Operator = #ApplyExpression ) ; b< ... >( a, 1, ... )
  Assert( Code\Expressions( 2 )\Context = #ValueContext )
  Assert( Code\Expressions( 2 )\FirstOperandI = 3 )
  Assert( Code\Expressions( 2 )\SecondOperandI = 123456 )
  Assert( Code\Expressions( 2 )\NextExpression = -1 )
  Assert( Code\Expressions( 3 )\Operator = #ApplyExpression ) ; b< c, d >
  Assert( Code\Expressions( 3 )\Context = #TypeContext )
  Assert( Code\Expressions( 3 )\FirstOperandI = 1 )
  Assert( Code\Expressions( 3 )\SecondOperandI = 6 )
  Assert( Code\Expressions( 3 )\NextExpression = -1 )
  Assert( Code\Expressions( 4 )\Operator = #NameExpression ) ; c
  Assert( Code\Expressions( 4 )\Context = #TypeContext )
  Assert( Code\Expressions( 4 )\FirstOperandI = 2 )
  Assert( Code\Expressions( 4 )\NextExpression = -1 )
  Assert( Code\Expressions( 5 )\Operator = #NameExpression ) ; d
  Assert( Code\Expressions( 5 )\Context = #TypeContext )
  Assert( Code\Expressions( 5 )\FirstOperandI = 3 )
  Assert( Code\Expressions( 5 )\NextExpression = -1 )
  Assert( Code\Expressions( 6 )\Operator = #TupleExpression )
  Assert( Code\Expressions( 6 )\FirstOperandI = 3 )
  Assert( Code\Expressions( 6 )\SecondOperandI = 4 )
  Assert( Code\Expressions( 6 )\NextExpression = -1 )
  Assert( Code\Expressions( 7 )\Operator = #LiteralExpression ) ; 1
  Assert( Code\Expressions( 7 )\Type = #IntegerLiteralType )
  Assert( Code\Expressions( 7 )\Context = #ValueContext )
  Assert( Code\Expressions( 7 )\FirstOperandI = 1 )
  Assert( Code\Expressions( 7 )\NextExpression = -1 )
  Assert( Code\Expressions( 8 )\Operator = #LiteralExpression ) ; 2
  Assert( Code\Expressions( 8 )\Type = #IntegerLiteralType )
  Assert( Code\Expressions( 8 )\Context = #ValueContext )
  Assert( Code\Expressions( 8 )\FirstOperandI = 2 )
  Assert( Code\Expressions( 8 )\NextExpression = -1 )
  Assert( Code\Expressions( 9 )\Operator = #LiteralExpression ) ; 3
  Assert( Code\Expressions( 9 )\Type = #IntegerLiteralType )
  Assert( Code\Expressions( 9 )\Context = #ValueContext )
  Assert( Code\Expressions( 9 )\FirstOperandI = 3 )
  Assert( Code\Expressions( 9 )\NextExpression = -1 )
  Assert( Code\Expressions( 10 )\Operator = #LiteralExpression ) ; 4
  Assert( Code\Expressions( 10 )\Type = #IntegerLiteralType )
  Assert( Code\Expressions( 10 )\Context = #ValueContext )
  Assert( Code\Expressions( 10 )\FirstOperandI = 4 )
  Assert( Code\Expressions( 10 )\NextExpression = -1 )

EndProcedureUnit
CompilerEndIf  
  
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

;;;;TODO: =============================================================
CompilerIf #False
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
CompilerEndIf

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
  Assert( Code\Definitions( 0 )\TypeExpression = 0 )
  Assert( Code\Expressions( 0 )\Operator = #NameExpression )
  Assert( Code\Expressions( 0 )\Context = #TypeContext )
  Assert( Code\Expressions( 0 )\FirstOperandI = 1 )
EndProcedureUnit

ProcedureUnit CanParseTypeAliasDefinition()
  ResetCode()
  TestParseText( @ParseDefinition(), "type First = A | B;", 0 )
  Assert( Code\DefinitionCount = 1 )
  Assert( Code\IdentifierCount = 3 )
  Assert( Code\ExpressionCount = 3 )
  Assert( Code\Identifiers( 0 ) = "first" )
  Assert( Code\Identifiers( 1 ) = "a" )
  Assert( Code\Identifiers( 2 ) = "b" )
  Assert( Code\Definitions( 0 )\Name = 0 )
  Assert( Code\Definitions( 0 )\DefinitionKind = #TypeDefinition )
  Assert( Code\Definitions( 0 )\Flags = #IsAlias )
  Assert( Code\Definitions( 0 )\FirstAnnotation = -1 )
  Assert( Code\Definitions( 0 )\FirstValueParameter = -1 )
  Assert( Code\Definitions( 0 )\FirstTypeParameter = -1 )
  Assert( Code\Definitions( 0 )\TypeExpression = 2 )
  Assert( Code\Expressions( 0 )\Operator = #NameExpression )
  Assert( Code\Expressions( 0 )\Context = #TypeContext )
  Assert( Code\Expressions( 0 )\FirstOperandI = 1 )
  Assert( Code\Expressions( 1 )\Operator = #NameExpression )
  Assert( Code\Expressions( 1 )\Context = #TypeContext )
  Assert( Code\Expressions( 1 )\FirstOperandI = 2 )
  Assert( Code\Expressions( 2 )\Operator = #OrExpression )
  Assert( Code\Expressions( 2 )\Context = #TypeContext )
  Assert( Code\Expressions( 2 )\FirstOperandI = 0 )
  Assert( Code\Expressions( 2 )\SecondOperandI = 1 )
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
  TestParseText( @ParseDefinition(), "method First() Foo(); end;", 0 )
  Assert( Code\DefinitionCount = 1 )
  Assert( Code\IdentifierCount = 2 )
  Assert( Code\ScopeCount = 2 )
  Assert( Code\StatementCount = 1 )
  Assert( Code\ExpressionCount = 3 )
  Assert( Code\Identifiers( 0 ) = "first" )
  Assert( Code\Identifiers( 1 ) = "foo" )
  Assert( Code\Definitions( 0 )\Name = 0 )
  Assert( Code\Definitions( 0 )\DefinitionKind = #MethodDefinition )
  Assert( Code\Definitions( 0 )\Flags = 0 )
  Assert( Code\Definitions( 0 )\FirstAnnotation = -1 )
  Assert( Code\Definitions( 0 )\InnerScope = 1 )
  Assert( Code\Definitions( 0 )\FirstValueParameter = -1 )
  Assert( Code\Definitions( 0 )\FirstTypeParameter = -1 )
  Assert( Code\Scopes( 1 )\Definition = 0 )
  Assert( Code\Scopes( 1 )\FirstDefinitionOrStatement = 0 )
  Assert( Code\Statements( 0 )\StatementKind = #ExpressionStatement )
  Assert( Code\Statements( 0 )\ReferencedIndex = 2 )
  Assert( Code\Statements( 0 )\NextStatement = -1 )
  Assert( Code\Expressions( 0 )\Operator = #NameExpression )
  Assert( Code\Expressions( 0 )\Context = #ValueContext )
  Assert( Code\Expressions( 0 )\FirstOperandI = 1 )
  Assert( Code\Expressions( 1 )\Operator = #LiteralExpression )
  Assert( Code\Expressions( 1 )\Context = #ValueContext )
  Assert( Code\Expressions( 1 )\Type = #NothingLiteralType )
  Assert( Code\Expressions( 2 )\Operator = #ApplyExpression )
  Assert( Code\Expressions( 2 )\Context = #ValueContext )
  Assert( Code\Expressions( 2 )\FirstOperandI = 0 )
  Assert( Code\Expressions( 2 )\SecondOperandI = 1 )
EndProcedureUnit

ProcedureUnit CanParseMethodDefinitionWithValueParameters()
  ResetCode()
  TestParseText( @ParseDefinition(), "method First( A, B : C ) end;", 0 )
  Assert( Code\DefinitionCount = 1 )
  Assert( Code\IdentifierCount = 4 )
  Assert( Code\ScopeCount = 2 )
  Assert( Code\StatementCount = 0 )
  Assert( Code\ParameterCount = 2 )
  Assert( Code\ExpressionCount = 2 )
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
  Assert( Code\Parameters( 0 )\Name = -1 )
  Assert( Code\Parameters( 0 )\NextParameter = 1 )
  Assert( Code\Parameters( 0 )\TypeExpression = 0 )
  Assert( Code\Parameters( 1 )\Name = 2 )
  Assert( Code\Parameters( 1 )\NextParameter = -1 )
  Assert( Code\Parameters( 1 )\TypeExpression = 1 )
  Assert( Code\Expressions( 0 )\Operator = #NameExpression )
  Assert( Code\Expressions( 0 )\FirstOperandI = 1 )
  Assert( Code\Expressions( 0 )\Context = #TypeContext )
  Assert( Code\Expressions( 1 )\Operator = #NameExpression )
  Assert( Code\Expressions( 1 )\FirstOperandI = 3 )
  Assert( Code\Expressions( 1 )\Context = #TypeContext )
EndProcedureUnit

; A parameter can drop the *name*. I.e. instead of "A : B", you write "B".
; This does *NOT* drop the type part, it drops the name part.
; So "B" is equivalent to "B : B", i.e. it generates an implicit name.
; The interesting thing is that it can be a full type expression, not just a simple named type.
; So, "A | B" is just as valid as "A< B >", for example.
; "A | B" gets a name "a_or_b".
; "A & B" gets a name "a_and_b".
ProcedureUnit CanParseMethodDefinitionUsingImplicitlyNamedParameter()
  ResetCode()
  TestParseText( @ParseDefinition(), "method First( A | B ) end;", 0 )
  Assert( Code\DefinitionCount = 1 )
  Assert( Code\IdentifierCount = 3 )
  Assert( Code\ExpressionCount = 3 )
  Assert( Code\ParameterCount = 1 )
  Assert( Code\Identifiers( 0 ) = "first" )
  Assert( Code\Identifiers( 1 ) = "a" )
  Assert( Code\Identifiers( 2 ) = "b" )
  Assert( Code\Definitions( 0 )\Name = 0 )
  Assert( Code\Definitions( 0 )\DefinitionKind = #MethodDefinition )
  Assert( Code\Definitions( 0 )\Flags = 0 )
  Assert( Code\Definitions( 0 )\FirstAnnotation = -1 )
  Assert( Code\Definitions( 0 )\FirstValueParameter = 0 )
  Assert( Code\Definitions( 0 )\FirstTypeParameter = -1 )
  Assert( Code\Parameters( 0 )\Name = -1 ) ; No name!
  Assert( Code\Parameters( 0 )\NextParameter = -1 )
  Assert( Code\Parameters( 0 )\TypeExpression = 2 )
  Assert( Code\Expressions( 0 )\Operator = #NameExpression )
  Assert( Code\Expressions( 0 )\FirstOperandI = 1 )
  Assert( Code\Expressions( 0 )\Context = #TypeContext )
  Assert( Code\Expressions( 1 )\Operator = #NameExpression )
  Assert( Code\Expressions( 1 )\FirstOperandI = 2 )
  Assert( Code\Expressions( 1 )\Context = #TypeContext )
  Assert( Code\Expressions( 2 )\Operator = #OrExpression )
  Assert( Code\Expressions( 2 )\FirstOperandI = 0 )
  Assert( Code\Expressions( 2 )\SecondOperandI = 1 )
  Assert( Code\Expressions( 2 )\Context = #TypeContext )
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
  
;}

;==============================================================================
;-== Codegen and semantic analysis.
;;;;TODO: put this stuff on a thread

Structure Asset
EndStructure

Structure Field
  Name.s
  Offset.i
  Type.i
EndStructure

; Type codes are type indices offset by the number of built-in types.
Enumeration BuiltinType
  #Int32Type
  #Int64Type
  #Float32Type
  #Float64Type
EndEnumeration

EnumerationBinary TypeFlags
  #TypeIsArray ; Used for "normal" arrays as well as strings.
  #TypeIsTuple
EndEnumeration

; No inheritance/derivation at runtime.
; Each type is basically two structs:
;   (1) An instance struct (with sizeof() >= 0)
;   (2) One static struct (with sizeof() >= 0)
Structure Type
  Name.s
  SizeInBytes.i
  Flags.i
  FirstField.i
  FieldCount.i
EndStructure

Enumeration InstructionCode
  
  ; Invocation insns.
  #CallInsn = 1 ; Call function. Operand1 is function index. Operand2 is argument. Value is result value.
  #CCallInsn    ; Call function with closure. Operand1 is function index. Operand2 is argument. Operand3 is type index of closure.
  #ICallInsn    ; Call intrinsic. Operand1 is Intrinsic enum value. Operand2 is argument. Value is result value.
  #YieldInsn
  #ReturnInsn
  
  ; Memory insns.
  ; Store is the only way of changing state.
  #LoadInsn     ; Load field/value. Operand1 is object. Operand2 is field index.
  #StoreInsn    ; Store field. Operand1 is object. Operand2 is field index. Operand3 is value.
  #ConsInsn     ; LISP-ish cons for "quasi"-tuples. Operand1 is head. Operand2 is tail.
  
  ; Block insns.
  #BlockInsn    ; Operand1 is count of instructions comprising the block.
  #ExitInsn     ; Operand1 is count of blocks to exit.
  
  ; Guard insns.
  ; These can only appear right after a BlockInsn.
  ; A sequence of these will stack. For any one block, must all be the same type of guards, though.
  #ConditionalInsn ; Compares first arg to True or False singleton. Second arg is flags.
  #CatchInsn
  #TypecaseInsn ; No subtyping. Checks for exact type ID match on given object. Operand1 is value, Operand2 is type ID.
  
EndEnumeration

EnumerationBinary ConditionalInsnFlags
  #IsNotCompare
  #IsAndCompare
  #IsOrCompare
EndEnumeration

EnumerationBinary LoadInsnFlags
  #IsSlotLoad ; Load current value of slot in object. First arg is object. Second arg is slot index (if array object, -1 is length slot, rest is element slots).
  #IsIntConstantLoad ; First arg is constant.
  #IsFloatConstantLoad ; First arg is constant.
  #IsObjectLoad ; Load reference to object from globals. First arg is index.
EndEnumeration

EnumerationBinary StoreInsnFlags
  #IsSlotStore
EndEnumeration

;;;;how do I get integer/float operations cheap? (cheap calls / math ops, cheap representation)

;;;;it makes sense for as much optimization as possible to happen on the IDE side instead of the player side

Enumeration Intrinsic
  
  #IntrDebugLog = 1
  #IntrDebugBreak
  
  ;;;;REVIEW: should the types here be implicit??
  #IntrIntAdd
  #IntrIntSubtract
  #IntrIntMultiply
  #IntrIntDivide
  #IntrIntModulo
  
  #IntrFloatAdd
  #IntrFloatSubtract
  #IntrFloatMultiply
  #IntrFloatDivide
  #IntrFloatModulo
  
EndEnumeration

; Instructions are fixed-width SSA for a very simple, stack-less VM somewhat inspired by SmallTalk.
Structure Instruction
  Opcode.b
  Operand1.b
  Operand2.b
  Operand3.b
EndStructure

; All functions are fully symmetric, i.e. 1 argument value, 1 result value.
; Unlike methods, there's no overloading and no dispatching. Basically, a function is a "method group".
; Any dispatching (where necessary) happens internally to a function.
Structure Function
  Name.s
  ArgumentType.i
  ResultType.i
  FirstInstruction.i
  InstructionCount.i
EndStructure

Structure Object
  Type.i
  FirstByte.i ; Memory state does not necessary correspond to layout runtime uses. Contains no type tags.
EndStructure

;;;;REVIEW: transmit instructions and initial state both as one big memory blob?

Structure Program
  Name.s
  Company.s
  Product.s
  AssetCount.i
  FunctionCount.i
  TypeCount.i
  ObjectCount.i
  InstructionCount.i
  InitialMemorySize.i
  Array Assets.Asset( 0 )
  Array Functions.Function( 0 )
  Array Types.Type( 0 )
  Array Fields.Field( 0 )
  Array Objects.Object( 0 )
  Array Instructions.Instruction( 0 )
  Array InitialMemory.b( 0 )
EndStructure

; This is the compiled program.
; For now, we only support a single program in source.
Global Program.Program

Global.b WriteGeneratedProgramToDisk = #False

#INVALID_NODE_INDEX = -1

; These are de-duplicated. One instance of the same field is applied to every
; single type it applies to.
Structure GenField
  Name.s
  Offset.i
  Type.i
  StructureUnion
    DefaultValueI.i
    DefaultValueF.d
    DefaultValueS.i
    DefaultValueO.i
  EndStructureUnion
EndStructure

Enumeration GenTypeKind
  #NamedType = 1
  #UnionType
  #IntersectionType
  #InstancedType
  #TupleType
  #FunctionType
  #DependentType
EndEnumeration

EnumerationBinary GenTypeFlags
  #IsSingletonType
  #IsAbstractType
  #IsImmutableType
  #IsMutableType
EndEnumeration

; Every type used in the program gets its own type instance.
Structure GenType
  Id.l                  ; Unique consecutive identifier.
  TypeKind.b
  TypeFlags.b
  DefinitionIndex.l     ; If coming from definition (named types). Also gives the name.
  FieldCount.i
  Array Fields.GenField( 0 )
  ; If it's a type combinator, this is the IDs of the type being combined.
  ; For instanced types, the first is the generic type being instanced and the second is the tuple type applied to it.
  LeftOperandTypeId.l
  RightOperandTypeId.l
  ; IDs of types that are combined with this one.
  ; First 0 entry in array is end. If all entries taken, no 0 entry.
  ; All binary combinations are formed from type with *lower* ID (the operator is transitive).
  Array Combinations.l( 0 )
EndStructure

Structure GenTypeTable
  TypeCount.l
  ObjectTypeId.l
  IntegerTypeId.l
  FloatTypeId.l
  ImmutableStringTypeId.l
  NothingTypeId.l
  TrueTypeId.l
  FalseTypeId.l
  *SubtypeMatrix ; 2-dimensional typecount*typecount 2-bitfield matrix with subtype flags (0=not initialized, 1=is not subtype, 2=is subtype).
  Array Types.GenType( 0 )
  Map NamedTypes.l() ; Maps a name to a type ID. These are the "primitive" types all other types are built from. Also contains aliases.
  Map AliasToTypeExpression.i() ; Maps a name to an expression index.
EndStructure

EnumerationBinary GenMethodFlags
  #IsReadMethod
  #IsWriteMethod
  #IsDefaultMethod
  #IsAfterMethod
  #IsBeforeMethod
  #IsAroundMethod
  #IsAbstractMethod
  #IsIntrinsicMethod
EndEnumeration

; A method implements a function for one specific argument type.
; There can be only one method non-before/after/around method for any given argument type.
; During code generation, the code for each method is inlined into the function it belongs to.
Structure GenMethod
  DefinitionIndex.i
  MethodFlags.i
  ArgumentTypeId.i
  ResultTypeId.i
EndStructure

; Each node in the dispatch tree has an associated function type.
; Each such function type dictates the argument and result types for the entire branch of the tree.
; Each child node may expect a more specific type of argument and may in turn return a more specific type of result.
; Both these types act covariantly as we go down the tree.
; This means that a node's function type is *not* a subtype of its parent node's function type.
Structure GenDispatchTreeNode
  *Method.GenMethod
  ArgumentTypeId.i
  ResultTypeId.i
  Parent.l
  FirstChild.l
  NextSibling.l
  FirstBefore.l
  FirstAfter.l
  FirstAround.l
  CallWeightScore.l ; Sum of # of call sites that enter the function through this branch, each times weight for site (x10 in loop).
EndStructure

Structure GenDispatchTree
  NodeCount.i
  Array Nodes.GenDispatchTreeNode( 0 )
EndStructure

; A collection of methods that all have the same name.
; Essentially, each function is an 'Object -> Object' mapping.
Structure GenFunction
  Name.s
  InvocationCount.i
  BodyScopeIndex.i
  DispatchTree.GenDispatchTree
  MethodCount.i
  Array Methods.GenMethod( 0 )
EndStructure

Structure GenCallSite
  *Function.GenFunction
  DispatchNodeIndex.l
  InstructionIndex.l
EndStructure

Structure GenFunctionTable
  Array CallSites.GenCallSite( 0 ) ; Mirrors expression array.
  Map Functions.GenFunction()
EndStructure

; A symbol table entry.
Structure GenSymbol
  IdentifierIndex.l
  ScopeIndex.l        ; The scope that *owns* the node, not necessarily the scope the symbol was defined in.
  ValueIndex.l        ; Index into GenValueTable\Values(). -1 if no value entry.
  TypeIndex.l         ; Index into GenTypeTable\Types(). -1 if no type entry.
  NextSymbol.l
EndStructure

; For now, hash-table size for scopes is fixed.
#GENSCOPE_HASHTABLE_SIZE = 16

; 
Structure GenScope
  DefinitionOrStatementIndex.l  ; Positive if statement, negative if definition.
  ParentScope.l                 ; -1 for root.
  FirstChildScope.l
  NextSiblingScope.l
  SymbolTable.l[ #GENSCOPE_HASHTABLE_SIZE ]
EndStructure

Structure GenSymbolTable
  ScopeCount.l
  SymbolCount.l
  Array Scopes.GenScope( 0 ) ; First scope is global scope.
  Array Symbols.GenSymbol( 0 )
EndStructure

Structure GenValue
  TypeId.l
EndStructure

Structure GenValueTable
  ValueCount.l
  Array Values.GenValue( 0 )
EndStructure

Structure GenProgram
  Name.s
  Company.s
  Product.s
  SymbolTable.GenSymbolTable
  ValueTable.GenValueTable
  TypeTable.GenTypeTable
  FunctionTable.GenFunctionTable
EndStructure

; This is the intermediate representation of the program.
; For semantic analysis.
Global.GenProgram GenProgram

Structure GenBlock
EndStructure

Structure GenEmitter
  BlockStackDepth.i
  Array BlockStack.GenBlock( 0 )
  InstructionCount.l
  Array Instructions.Instruction( 0 )
EndStructure

Procedure.i EmitInsn( *Emitter.GenEmitter, Opcode.b )
  
  Define.i InstructionIndex = *Emitter\InstructionCount
  If ArraySize( *Emitter\Instructions() ) = InstructionIndex
    ReDim *Emitter\Instructions( InstructionIndex + 4096 )
  EndIf
  *Emitter\InstructionCount + 1
  
  Define.Instruction *Insn = @*Emitter\Instructions( InstructionIndex )
  *Insn\Opcode = Opcode
  
  ProcedureReturn InstructionIndex
  
EndProcedure

Procedure.i EmitBeginBlock( *Emitter.GenEmitter )
EndProcedure

Procedure EmitEndBlock( *Emitter.GenEmitter )
EndProcedure

Macro GenTypePtr( TypeId )
  @GenProgram\TypeTable\Types( TypeId - 1 )
EndMacro

#SUBTYPE_NOT_INITIALIZED = 0
#SUBTYPE_FALSE = 1
#SUBTYPE_TRUE = 2

Macro SetupSubtypeMatrixIndex( FirstTypeId, SecondTypeId )
  CompilerIf #PB_Compiler_Debugger
    If FirstTypeId = #INVALID_TYPE_ID
      DebuggerError( "FirstTypeId is invalid" )
    EndIf
    If SecondTypeId = #INVALID_TYPE_ID
      DebuggerError( "SecondTypeId is invalid" )
    EndIf
  CompilerEndIf
  Define.i FirstTypeId#Index = FirstTypeId - 1
  Define.i SecondTypeId#Index = SecondTypeId - 1
  Define.i SubtypeMatrixIndex = FirstTypeId#Index * GenProgram\TypeTable\TypeCount + SecondTypeId#Index
  ; Two bits per entry.
  Define.i SubtypeMatrixByteOffset = SubtypeMatrixIndex / 4
  Define.i SubtypeMatrixBitOffset = ( SubtypeMatrixIndex % 4 ) * 2
  Define.i SubtypeMatrixMask = 3 << SubtypeMatrixBitOffset
  Define *SubtypeMatrixPtr = GenProgram\TypeTable\SubtypeMatrix + SubtypeMatrixByteOffset
  CompilerIf #PB_Compiler_Debugger
    If SubtypeMatrixIndex < 0
      DebuggerError( "Negative subtype matrix index!" )
    EndIf
    If SubtypeMatrixByteOffset >= MemorySize( GenProgram\TypeTable\SubtypeMatrix )
      DebuggerError( "Subtype byte offset out of range!" )
    EndIf
  CompilerEndIf
EndMacro

Declare.b ComputeIsFirstSubtypeOfSecond( FirstTypeId.i, SecondTypeId.i )

Procedure.b FirstIsSubtypeOfSecond( FirstTypeId.i, SecondTypeId.i )
  
  CompilerIf #PB_Compiler_Debugger
    If FirstTypeId <= 0
      DebuggerError( "First type ID is invalid!" )
    EndIf
    If SecondTypeId <= 0
      DebuggerError( "Second type ID is invalid!" )
    EndIf
  CompilerEndIf
  
  SetupSubtypeMatrixIndex( FirstTypeId, SecondTypeId )
  
  ; In order to avoid having to compute all NxN relationships up front, we make the process
  ; lazy and only compute that part of the matrix that we actually need to understand the program.
  
  Define.b Value = ( PeekB( *SubtypeMatrixPtr ) & SubtypeMatrixMask ) >> SubtypeMatrixBitOffset
  Select Value
      
    Case #SUBTYPE_NOT_INITIALIZED
      If ComputeIsFirstSubtypeOfSecond( FirstTypeId, SecondTypeId )
        PokeB( *SubtypeMatrixPtr, PeekB( *SubtypeMatrixPtr ) | ( #SUBTYPE_TRUE << SubtypeMatrixBitOffset ) )
        ProcedureReturn #True
      Else
        PokeB( *SubtypeMatrixPtr, PeekB( *SubtypeMatrixPtr ) | ( #SUBTYPE_FALSE << SubtypeMatrixBitOffset ) )
        ProcedureReturn #False
      EndIf
      
    Case #SUBTYPE_TRUE
      ProcedureReturn #True
      
    Case #SUBTYPE_FALSE
      ProcedureReturn #False
      
  EndSelect
  
EndProcedure

Procedure.b SetFirstIsSubtypeOfSecond( FirstTypeId.i, SecondTypeId.i )
  SetupSubtypeMatrixIndex( FirstTypeId, SecondTypeId )
  PokeB( *SubtypeMatrixPtr, PeekB( *SubtypeMatrixPtr ) | ( #SUBTYPE_TRUE << SubtypeMatrixBitOffset ) )
EndProcedure

; Determine if 'FirstTypeId <: SecondTypeId' for any type relationship other than identity (FirstTypeId = SecondTypeId)
; and direct derivation (FirstTypeId : SecondTypeId). The latter two relationships we initialize directly in GenTypes().
Procedure.b ComputeIsFirstSubtypeOfSecond( FirstTypeId.i, SecondTypeId.i )
  
  ; Everything is a subtype of Object.
  If SecondTypeId = GenProgram\TypeTable\ObjectTypeId
    ProcedureReturn #True
  EndIf
  
  Define.GenType *FirstGenType = GenTypePtr( FirstTypeId )
  Define.GenType *SecondGenType = GenTypePtr( SecondTypeId )
  
  Select *SecondGenType\TypeKind
      
    Case #IntersectionType
      ProcedureReturn Bool( FirstIsSubtypeOfSecond( FirstTypeId, *SecondGenType\LeftOperandTypeId ) Or FirstIsSubtypeOfSecond( FirstTypeId, *SecondGenType\RightOperandTypeId ) )
      
    Case #UnionType
      ProcedureReturn Bool( FirstIsSubtypeOfSecond( FirstTypeId, *SecondGenType\LeftOperandTypeId ) And FirstIsSubtypeOfSecond( FirstTypeId, *SecondGenType\RightOperandTypeId ) )
      
    Case #FunctionType
      ; We only test for relationships between function types here. Other type relationships
      ; from function types to other kinds of types can be established explicitly in code.
      If *FirstGenType\TypeKind = #FunctionType
        ; Contravariant argument, covariant result.
        ProcedureReturn Bool( FirstIsSubtypeOfSecond( *SecondGenType\LeftOperandTypeId, *FirstGenType\LeftOperandTypeId ) And FirstIsSubtypeOfSecond( *FirstGenType\RightOperandTypeId, *SecondGenType\RightOperandTypeId ) )
      EndIf
      
    Case #TupleType
      
      If *FirstGenType\TypeKind = #TupleType
        ProcedureReturn Bool( FirstIsSubtypeOfSecond( *FirstGenType\LeftOperandTypeId, *SecondGenType\LeftOperandTypeId ) And FirstIsSubtypeOfSecond( *FirstGenType\RightOperandTypeId, *SecondGenType\RightOperandTypeId ) )
      EndIf
      
  EndSelect
  
  Select *FirstGenType\TypeKind
      
    Case #NamedType
      If FirstTypeId <> GenProgram\TypeTable\ObjectTypeId
        ProcedureReturn FirstIsSubtypeOfSecond( *FirstGenType\LeftOperandTypeId, SecondTypeId )
      EndIf
      
  EndSelect

  ProcedureReturn #False
  
EndProcedure

Procedure.i NewGenScope( ParentScopeIndex.i )
  
  Define.i ScopeIndex = GenProgram\SymbolTable\ScopeCount
  If ArraySize( GenProgram\SymbolTable\Scopes() ) = ScopeIndex
    ReDim GenProgram\SymbolTable\Scopes( ScopeIndex + 512 )
  EndIf
  GenProgram\SymbolTable\ScopeCount + 1
  
  Define.GenScope *GenScope = @GenProgram\SymbolTable\Scopes( ScopeIndex )
  *GenScope\ParentScope = ParentScopeIndex
  
  Define.i HashTableIndex
  For HashTableIndex = 0 To #GENSCOPE_HASHTABLE_SIZE - 1
    *GenScope\SymbolTable[ HashTableIndex ] = -1
  Next
    
  ProcedureReturn ScopeIndex
  
EndProcedure

Procedure.i NewGenSymbol( ScopeIndex.i, IdentifierIndex.i )
  
  Define.i SymbolIndex = GenProgram\SymbolTable\SymbolCount
  If ArraySize( GenProgram\SymbolTable\Symbols() ) = SymbolIndex
    ReDim GenProgram\SymbolTable\Symbols( SymbolIndex + 2048 )
  EndIf
  GenProgram\SymbolTable\SymbolCount + 1
  
  Define.GenSymbol *Symbol = @GenProgram\SymbolTable\Symbols( SymbolIndex )
  *Symbol\IdentifierIndex = IdentifierIndex
  *Symbol\ScopeIndex = ScopeIndex
    
  Define.GenScope *Scope = GenProgram\SymbolTable\Scopes( ScopeIndex )
  
  Define.i HashTableIndex = IdentifierIndex % #GENSCOPE_HASHTABLE_SIZE
  
  *Symbol\NextSymbol = *Scope\SymbolTable[ HashTableIndex ]
  *Scope\SymbolTable[ HashTableIndex ] = SymbolIndex
  
  ProcedureReturn SymbolIndex
  
EndProcedure

; Adds a new GenValue to GenProgram\ValueTable\Values() and returns the *index* for it.
Procedure.i NewGenValue( TypeId.i )
  
  Define ValueIndex = GenProgram\ValueTable\ValueCount
  If ArraySize( GenProgram\ValueTable\Values() ) = ValueIndex
    ReDim GenProgram\ValueTable\Values( ValueIndex + 1024 )
  EndIf
  GenProgram\ValueTable\ValueCount + 1
  
  Define.GenValue *GenValue = @GenProgram\ValueTable\Values( ValueIndex )
  *GenValue\TypeId = TypeId
  
  ProcedureReturn ValueIndex
  
EndProcedure

; Adds a new GenType to GenProgram\TypeTable\Types() and returns a *pointer* to it.
Procedure.i NewGenTypePtr( TypeKind.i )
  
  Define.i TypeIndex = GenProgram\TypeTable\TypeCount
  If ArraySize( GenProgram\TypeTable\Types() ) = TypeIndex
    ReDim GenProgram\TypeTable\Types( TypeIndex + 512 )
  EndIf
  GenProgram\TypeTable\TypeCount + 1
  Define.i TypeId = TypeIndex + 1
  
  Define.GenType *GenType = @GenProgram\TypeTable\Types( TypeIndex )
  *GenType\TypeKind = TypeKind
  *GenType\Id = TypeId
  
  ProcedureReturn *GenType

EndProcedure

Procedure.i MakeCombinedGenType( CombinedTypeKind.i, LeftTypeId.i, RightTypeId.i )
  
  ; NOTE: We allow combining a type with itself.
  
  ; For transitive operators, we always combine the type with the *higher* ID
  ; *into* the type With the *lower* one.
  If ( CombinedTypeKind = #UnionType Or CombinedTypeKind = #IntersectionType ) And LeftTypeId > RightTypeId
    Swap LeftTypeId, RightTypeId
  EndIf
  
  Define.i LeftTypeIndex = LeftTypeId - 1
  Define.i RightTypeIndex = RightTypeId - 1
  
  ; NOTE: As soon as we add a type, this pointer must be considered invalid!
  Define.GenType *LeftType = @GenProgram\TypeTable\Types( LeftTypeIndex )
  
  ; Search the list of existing combinations for this particular one.
  Define.i CombinationIndex
  For CombinationIndex = 0 To ArraySize( *LeftType\Combinations() ) - 1
    
    Define.i CombinedTypeId = *LeftType\Combinations( CombinationIndex )
    If CombinedTypeId = #INVALID_TYPE_ID
      Break
    EndIf
    
    Define.i CombinedTypeIndex = CombinedTypeId - 1
    Define.GenType *CombinedType = @GenProgram\TypeTable\Types( CombinedTypeIndex )
    If *CombinedType\TypeKind = CombinedTypeKind And *CombinedType\RightOperandTypeId = RightTypeId
      ; Found it. This is the right combination type and the right operand.
      ProcedureReturn *CombinedType
    EndIf
    
  Next
  
  ; We didn't find an existing combination, so add a new one.
  Define.GenType *GenType = NewGenTypePtr( CombinedTypeKind )
  *GenType\LeftOperandTypeId = LeftTypeId
  *GenType\RightOperandTypeId = RightTypeId
  
  ; If needed, make space in array.
  *LeftType = @GenProgram\TypeTable\Types( LeftTypeIndex ) ; NewGenTypePtr() may have reallocated the array.
  If CombinationIndex = ArraySize( *LeftType\Combinations() )
    ReDim *LeftType\Combinations( CombinationIndex + 8 )
  EndIf
  *LeftType\Combinations( CombinationIndex ) = *GenType\Id
  
  ProcedureReturn *GenType
          
EndProcedure

Procedure.i LookupSymbol( ScopeIndex.i, IdentifierIndex.i )
  
  Define.i HashTableIndex = IdentifierIndex % #GENSCOPE_HASHTABLE_SIZE
  
  Define.i CurrentScope = ScopeIndex
  While CurrentScope <> -1
    
    Define.i CurrentSymbol = GenProgram\SymbolTable\Scopes( CurrentScope )\SymbolTable[ HashTableIndex ]
    While CurrentSymbol <> -1
      
      Define.GenSymbol *Symbol = @GenProgram\SymbolTable\Symbols( CurrentSymbol )
      If *Symbol\IdentifierIndex = IdentifierIndex
        ProcedureReturn CurrentSymbol
      EndIf
      
      CurrentSymbol = *Symbol\NextSymbol
      
    Wend
    
    CurrentScope = GenProgram\SymbolTable\Scopes( CurrentScope )\ParentScope
    
  Wend
  
  ProcedureReturn -1
  
EndProcedure

; Assinging types is recursive.
Declare.i AssignTypeIdIfTypeExpression( ExpressionIndex.i )

;....... problem... this already depends on symbol table structures
Procedure.i LookupNamedTypeFromString( Name.s )
  
  Define.i TypeId = #INVALID_TYPE_ID
  
  If Not FindMapElement( GenProgram\TypeTable\NamedTypes(), Name )
    ;;;;TODO: Diagnostic
    Debug "Cannot find type " + Name
  Else
    TypeId = GenProgram\TypeTable\NamedTypes()
    If TypeId = #INVALID_TYPE_ID
      
      ; It's a type alias.
      If Not FindMapElement( GenProgram\TypeTable\AliasToTypeExpression(), Name )
        ; This should not happend.
        Debug "Cannot find aliased type expression for " + Name
      Else
        Define.i AliasedTypeExpression = GenProgram\TypeTable\AliasToTypeExpression()
        TypeId = AssignTypeIdIfTypeExpression( AliasedTypeExpression )
        GenProgram\TypeTable\NamedTypes( Name ) = TypeId
      EndIf
      
    EndIf
  EndIf
  
  ProcedureReturn TypeId
  
EndProcedure

Procedure.i LookupNamedType( NameIndex.i )
  
  Define.s Name = Code\Identifiers( NameIndex )
  ProcedureReturn LookupNamedTypeFromString( Name )
  
EndProcedure

;;;;TODO: catch cycles!
; Determine the type of the given type expression.
; May lead to other type expressions being assign types as a side effect.
; Will generate new types as needed (for every new unique type combination found).
; Returns the type ID assigned to the expression.
Procedure.i AssignTypeIdIfTypeExpression( ExpressionIndex.i )
  
  Define.Expression *Expression = @Code\Expressions( ExpressionIndex )
  If *Expression\Context <> #TypeContext
    ProcedureReturn #INVALID_TYPE_ID
  EndIf
  
  ; Early out if expression has already been assigned a type.
  Define.i TypeId = *Expression\Type
  If TypeId <> 0
    ProcedureReturn TypeId
  EndIf
    
  Select *Expression\Operator
      
    Case #NameExpression
      TypeId = LookupNamedType( *Expression\FirstOperandI )
      
    Case #AndExpression, #OrExpression, #ApplyExpression, #TupleExpression, #ArrowExpression
      
      Define.i CombinedTypeKind = -1
      Select *Expression\Operator
        Case #AndExpression
          CombinedTypeKind = #UnionType
        Case #OrExpression
          CombinedTypeKind = #IntersectionType
        Case #ApplyExpression
          CombinedTypeKind = #InstancedType
        Case #TupleExpression
          CombinedTypeKind = #TupleType
        Case #ArrowExpression
          CombinedTypeKind = #FunctionType
      EndSelect
      
      Define.i LeftTypeId = AssignTypeIdIfTypeExpression( *Expression\FirstOperandI )
      Define.i RightTypeId = AssignTypeIdIfTypeExpression( *Expression\SecondOperandI )
      
      Define.GenType *CombinedType = MakeCombinedGenType( CombinedTypeKind, LeftTypeId, RightTypeId )
      TypeId = *CombinedType\Id
      
  EndSelect
  
  *Expression\Type = TypeId  
  ProcedureReturn TypeId
  
EndProcedure

Declare.i FindDispatchTreeNode( *DispatchTree.GenDispatchTree, ArgumentTypeId.i, StartingNode.i = 0 )

Procedure.i AssignTypeIdIfValueExpression( ExpressionIndex.i )
  
  Define.Expression *Expression = @Code\Expressions( ExpressionIndex )
  If *Expression\Context <> #ValueContext
    ProcedureReturn #INVALID_TYPE_ID
  EndIf
  
  ; Early out if expression has already been assigned a type.
  Define.i TypeId = *Expression\Type
  If TypeId > #INVALID_TYPE_ID
    ProcedureReturn TypeId
  ElseIf TypeId < #INVALID_TYPE_ID
    ; If it's a literal type, replace it with the type ID for the respective
    ; named type that we found in the program.
    Select TypeId
        
      Case #IntegerLiteralType
        TypeId = GenProgram\TypeTable\IntegerTypeId
        
      Case #FloatLiteralType
        TypeId = GenProgram\TypeTable\FloatTypeId
        
      Case #StringLiteralType
        TypeId = GenProgram\TypeTable\ImmutableStringTypeId
        
      Case #NothingLiteralType
        TypeId = GenProgram\TypeTable\NothingTypeId
        
      Default
        Debug "Unknown literal type!"
        
    EndSelect
      
    *Expression\Type = TypeId
    ProcedureReturn TypeId
  EndIf
  
  ;.... for name lookups, we need proper scoping, so this actually has to go hierarchically
  ; (however, in this language, only local variables can mess with the dispatching)
  
  Select *Expression\Operator
      
    Case #ApplyExpression
      
      Define.GenCallSite *CallSite = @GenProgram\FunctionTable\CallSites( ExpressionIndex )
    
      ; Look up function.
      Define.Expression *LeftHandExpr = @Code\Expressions( *Expression\FirstOperandI )
      Select *LeftHandExpr\Operator
          
        Case #NameExpression
          
          Define.s Name = Code\Identifiers( *LeftHandExpr\FirstOperandI )
          Define.GenFunction *CalledFunction = FindMapElement( GenProgram\FunctionTable\Functions(), Name )
          If *CalledFunction = #Null
            ;;;;TODO: diagnose
            Debug "Can't find function " + Name
            ProcedureReturn #INVALID_TYPE_ID
          Else
            *CallSite\Function = *CalledFunction
          EndIf
          
        Case #ApplyExpression
          ;;;;TODO
          Debug "TODO...."
          ;need to take type applications into account here
          
        Default
          Debug "Not implemented... ApplyExpression on expression of type " + ExpressionKindToString( *LeftHandExpr\Operator )
          
      EndSelect
      
      ; Determine argument type.
      Define.i ArgumentTypeId = AssignTypeIdIfValueExpression( *Expression\SecondOperandI )
    
      ; Find dispatch node in dispatch tree.
      Define.i DispatchNodeIndex = FindDispatchTreeNode( @*CallSite\Function\DispatchTree, ArgumentTypeId )
      If DispatchNodeIndex = -1
        ;;;;TODO: diagnose and handle
        Debug "Function doesn't apply to this argument"
        ProcedureReturn #INVALID_TYPE_ID
      EndIf
      *CallSite\DispatchNodeIndex = DispatchNodeIndex
    
      ;;;TODO: If node corresponds to a specialization already, switch to the specialization.
      
      ; Count invocation.
      *CallSite\Function\InvocationCount + 1
      *CallSite\Function\DispatchTree\Nodes( DispatchNodeIndex )\CallWeightScore + 1
      
      ; Return type becomes type of expression.
      TypeId = *CallSite\Function\DispatchTree\Nodes( DispatchNodeIndex )\ResultTypeId
      
  EndSelect
  
  *Expression\Type = TypeId  
  ProcedureReturn TypeId
  
EndProcedure

;;;;TODO: would be more efficient to fold this into the parsing pass
Procedure GenCollect()
  
  ; Add global scope.
  NewGenScope( -1 )
  
  ;{ Collect definitions.
  ; As a side-effect, we assign a unique type ID to every named type definition that isn't an alias. This
  ; is the first pass of type generation which essentially puts the starting set of "primitive" type IDs
  ; in place. All other type IDs are combinations synthesized from this set of primitives.
  Define.i DefinitionIndex
  For DefinitionIndex = 0 To Code\DefinitionCount - 1
    
    Define *Definition.Definition = @Code\Definitions( DefinitionIndex )
    
    ;;;;TODO: probably need to ultimately mangle names here
    Define.s Name = Code\Identifiers( *Definition\Name )
    
    Select *Definition\DefinitionKind
        
      Case #TypeDefinition
        
        ;........ the NamedTypes() thing should probably go in the symbol tables instead of into a separate map
        
        If FindMapElement( GenProgram\TypeTable\NamedTypes(), Name )
          ;;;;TODO: add diagnostic
          Debug "Type already defined: " + Name
          Continue
        EndIf
        
        If *Definition\Flags & #IsAlias
          ;;;;TODO: ensure that the alias doesn't attempt to add new modifiers (like 'abstract')
          AddMapElement( GenProgram\TypeTable\AliasToTypeExpression(), Name )
          GenProgram\TypeTable\AliasToTypeExpression() = *Definition\TypeExpression
          ; Put placeholder in table.
          ; NOTE: type aliases that aren't actually used will remain unresolved in the
          ;       name table.
          AddMapElement( GenProgram\TypeTable\NamedTypes(), Name )
          GenProgram\TypeTable\NamedTypes() = #INVALID_TYPE_ID
          Continue
        EndIf
        
        Define.GenType *GenType = NewGenTypePtr( #NamedType )
        *GenType\DefinitionIndex = DefinitionIndex
        
        AddMapElement( GenProgram\TypeTable\NamedTypes(), Name )
        GenProgram\TypeTable\NamedTypes() = *GenType\Id
        
        ;;;;REVIEW: should this be an annotation rather than just hardcoded names?
        Select Name
            
          Case "object"
            GenProgram\TypeTable\ObjectTypeId = *GenType\Id
            
          Case "integer"
            GenProgram\TypeTable\IntegerTypeId = *GenType\Id
            
          Case "float"
            GenProgram\TypeTable\FloatTypeId = *GenType\Id
            
          Case "immutable_string"
            GenProgram\TypeTable\ImmutableStringTypeId = *GenType\Id
            
          Case "nothing"
            GenProgram\TypeTable\NothingTypeId = *GenType\Id
            
          Case "true"
            GenProgram\TypeTable\TrueTypeId = *GenType\Id
                        
          Case "false"
            GenProgram\TypeTable\FalseTypeId = *GenType\Id
            
        EndSelect
        
        ; If it's a singleton, also add a value.
        If *Definition\Flags & #IsSingleton
          *GenType\TypeFlags & #IsSingletonType
          NewGenValue( *GenType\Id )
        EndIf
        
      Case #MethodDefinition
        
        ; Make sure the function is defined.
        Define.GenFunction *GenFunction = FindMapElement( GenProgram\FunctionTable\Functions(), Name )
        If *GenFunction = #Null
          *GenFunction = AddMapElement( GenProgram\FunctionTable\Functions(), Name )
          *GenFunction\Name = Name
        EndIf
        
        ; Add the method to the function.
        Define.i MethodIndex = *GenFunction\MethodCount
        If ArraySize( *GenFunction\Methods() ) = MethodIndex
          ReDim *GenFunction\Methods( MethodIndex + 32 )
        EndIf
        *GenFunction\MethodCount + 1
        
        Define.GenMethod *GenMethod = @*GenFunction\Methods( MethodIndex )
        *GenMethod\DefinitionIndex = DefinitionIndex
        
        ; Set flags.
        If *Definition\Flags & #IsAbstract
          *GenMethod\MethodFlags | #IsAbstractMethod
        EndIf
        If *Definition\Flags & #IsBefore
          *GenMethod\MethodFlags | #IsBeforeMethod
        EndIf
        If *Definition\Flags & #IsAfter
          *GenMethod\MethodFlags | #IsAfterMethod
        EndIf
        If *Definition\Flags & #IsAround
          *GenMethod\MethodFlags | #IsAroundMethod
        EndIf
        
      Case #ProgramDefinition
        
        ;;;;TODO: for now, ensure there is only one of these
        
        GenProgram\Name = Code\Identifiers( *Definition\Name )
        
        Define.i AnnotationIndex = *Definition\FirstAnnotation
        While AnnotationIndex <> -1
          Define.Annotation *Annotation = @Code\Annotations( AnnotationIndex )
          Select *Annotation\AnnotationKind
              
            Case #ProductAnnotation
              GenProgram\Product = *Annotation\AnnotationText
              
            Case #CompanyAnnotation
              GenProgram\Company = *Annotation\AnnotationText
              
            Case #PragmaAnnotation
              
              Define.s Pragma = ""
              Define.s Argument = ""
              
              Define.i IndexOfOpenParen = FindString( *Annotation\AnnotationText, "(" )
              If IndexOfOpenParen = 0
                Pragma = *Annotation\AnnotationText
              Else
                Pragma = Left( *Annotation\AnnotationText, IndexOfOpenParen - 1 )
                Define.i IndexOfCloseParen = FindString( *Annotation\AnnotationText, ")" )
                If IndexOfCloseParen = 0
                  Argument = Mid( *Annotation\AnnotationText, IndexOfOpenParen + 1 )
                Else
                  Argument = Mid( *Annotation\AnnotationText, IndexOfOpenParen + 1, IndexOfCloseParen - IndexOfOpenParen - 1 )
                EndIf
                
                Pragma = Trim( Pragma )
                Argument = Trim( Argument )
              EndIf
              
              Select LCase( Pragma )
                  
                ;;;;TODO: allow naming convention to be specific by example (e.g. "LikeThis" or "likeThis")
                  
                Case "namingconvention"
                  Select LCase( Argument )
                    Case "gnu"
                      DefaultNamingConvention = #GnuCase
                      
                    Case "pascal"
                      DefaultNamingConvention = #PascalCase
                      
                    Case "java"
                      DefaultNamingConvention = #JavaCase
                  EndSelect
                  
                Case "indentation"
                  
                  Dim Arguments.s( 0 )
                  SplitString( Arguments(), Argument, "," )
                  If ArraySize( Arguments() ) >= 1
                    TabWidth = Val( Trim( Arguments( 0 ) ) )
                  EndIf
                  If ArraySize( Arguments() ) >= 2
                    Select LCase( Trim( Arguments( 1 ) ) )
                      Case "spaces"
                        UseSoftTabs = #True
                      Case "tabs"
                        UseSoftTabs = #False
                      Default
                        Debug "Unknown indentation setting: " + Arguments( 1 )
                    EndSelect
                  EndIf
                  
                  GOSCI_SetTabs( Scintilla, TabWidth, UseSoftTabs )
                  
                Case "debug"
                  
                  Dim Arguments.s( 0 )
                  SplitString( Arguments(), Argument, "," )
                  If ArraySize( Arguments() )
                    Define.i Index
                    For Index = 0 To ArraySize( Arguments() ) -1
                      Select LCase( Arguments( Index ) )
                        Case "writecode"
                          WriteGeneratedProgramToDisk = #True
                      EndSelect
                    Next
                  EndIf
                  
              EndSelect
              
          EndSelect
          AnnotationIndex = *Annotation\NextAnnotation
        Wend
        
    EndSelect
    
  Next
  ;}
  
EndProcedure

; Come up with a name for the type that can be used for debugging purposes.
Procedure.s GenTypeName( *GenType.GenType )
  
  Select *GenType\TypeKind
      
    Case #NamedType
      ProcedureReturn Code\Identifiers( Code\Definitions( *GenType\DefinitionIndex )\Name )
      
    Case #UnionType
      ProcedureReturn "and(" + GenTypeName( GenTypePtr( *GenType\LeftOperandTypeId ) ) + "," + GenTypeName( GenTypePtr( *GenType\RightOperandTypeId ) ) + ")"
      
    Case #IntersectionType
      ProcedureReturn "or(" + GenTypeName( GenTypePtr( *GenType\LeftOperandTypeId ) ) + "," + GenTypeName( GenTypePtr( *GenType\RightOperandTypeId ) ) + ")"
      
    Case #FunctionType
      ProcedureReturn "fun(" + GenTypeName( GenTypePtr( *GenType\LeftOperandTypeId ) ) + "," + GenTypeName( GenTypePtr( *GenType\RightOperandTypeId ) ) + ")"
      
    Case #TupleType
      ProcedureReturn "tpl(" + GenTypeName( GenTypePtr( *GenType\LeftOperandTypeId ) ) + "," + GenTypeName( GenTypePtr( *GenType\RightOperandTypeId ) ) + ")"
      
    Default
      ProcedureReturn "<unknown>?!?!?!?"
      
  EndSelect
  
EndProcedure

Procedure.s GenTypeNameById( TypeId.i )
  ProcedureReturn GenTypeName( GenTypePtr( TypeId ) )
EndProcedure

Procedure.i GenTypeForParameters( ParameterIndex.i )
  
  Define.Parameter *Parameter = @Code\Parameters( ParameterIndex )
  
  Define.i TypeId = #INVALID_TYPE_ID
  
  ; If there's an explicit type expression on the parameter, that's our type.
  If *Parameter\TypeExpression <> -1
    TypeId = Code\Expressions( *Parameter\TypeExpression )\Type
  Else
    ; Otherwise, type has to match name of parameter.
    TypeId = LookupNamedType( *Parameter\Name )
  EndIf
  
  ; If there's more parameters following, create a tuple type.
  If *Parameter\NextParameter <> -1
    Define.i RightTypeId = GenTypeForParameters( *Parameter\NextParameter )
    Define.GenType *TupleType = MakeCombinedGenType( #TupleType, TypeId, RightTypeId )
    TypeId = *TupleType\Id
  EndIf
  
  ProcedureReturn TypeId
  
EndProcedure

; Finalizes the set of type IDs in the program and establishes
; their subtype relationships.
Procedure GenTypes()
  
  ; First, assign a type to every type expression in the program.
  Define ExpressionIndex.i
  For ExpressionIndex = 0 To Code\ExpressionCount - 1
    AssignTypeIdIfTypeExpression( ExpressionIndex )
  Next
  
  ; Next, assign a type to every method in the program. This may add
  ; additional types to the program.
  ForEach GenProgram\FunctionTable\Functions()
    
    Define.GenFunction *GenFunction = GenProgram\FunctionTable\Functions()
    
    Define.i MethodIndex
    For MethodIndex = 0 To *GenFunction\MethodCount - 1
      
      Define.GenMethod *GenMethod = @*GenFunction\Methods( MethodIndex )
      
      ; Default types for both argument and result is 'Nothing'.
      Define.i ResultTypeId = GenProgram\TypeTable\NothingTypeId
      Define.i ArgumentTypeId = GenProgram\TypeTable\NothingTypeId
      
      Define.i DefinitionIndex = *GenMethod\DefinitionIndex
      Define.Definition *Definition = @Code\Definitions( DefinitionIndex )
      
      ; Result type.
      Define.i ResultTypeExpressionIndex = *Definition\TypeExpression
      If ResultTypeExpressionIndex <> -1
        ResultTypeId = Code\Expressions( ResultTypeExpressionIndex )\Type
      EndIf
      
      ; Argument type.
      Define FirstValueParameterIndex = *Definition\FirstValueParameter
      If FirstValueParameterIndex <> -1
        ArgumentTypeId = GenTypeForParameters( FirstValueParameterIndex )
      EndIf
      
      *GenMethod\ArgumentTypeId = ArgumentTypeId
      *GenMethod\ResultTypeId = ResultTypeId
      
    Next
    
  Next
  
  ; Finally, add type instances to the output program and at the same time build the subtype matrix.
  Define.i TypeCount = GenProgram\TypeTable\TypeCount
  ReDim Program\Types( TypeCount )
  Program\TypeCount = TypeCount
  GenProgram\TypeTable\SubtypeMatrix = AllocateMemory( ( TypeCount * TypeCount + 3 ) / 4 )
  Define.i ObjectTypeId = GenProgram\TypeTable\ObjectTypeId
  Define.i TypeIndex
  For TypeIndex = 0 To TypeCount - 1
    
    ;;;;REVIEW: only put certain types in the final program? (like e.g. ones that are actually instantiated)
    ; Add to program.
    Define.GenType *GenType = @GenProgram\TypeTable\Types( TypeIndex )
    Define.Type *Type = @Program\Types( TypeIndex )
    *Type\Name = GenTypeName( *GenType )
        
    ; Establish trivial subtype relationships for named types.
    ; These are the starting relationships based on which we later lazily compute
    ; all other subtype relationships.
    SetFirstIsSubtypeOfSecond( *GenType\Id, *GenType\Id ) ; Every type is a subtype of itself.
    If *GenType\TypeKind = #NamedType
      Define.Definition *Definition = @Code\Definitions( *GenType\DefinitionIndex )
      Define.i TypeExpressionIndex = *Definition\TypeExpression
      
      Define.i IdOfTypeDerivedFrom = ObjectTypeId
      If TypeExpressionIndex <> -1
        IdOfTypeDerivedFrom = Code\Expressions( TypeExpressionIndex )\Type
      EndIf
          
      If IdOfTypeDerivedFrom <> #INVALID_TYPE_ID
        SetFirstIsSubtypeOfSecond( *GenType\Id, IdOfTypeDerivedFrom )
      EndIf
      
      ; Store ID of type derived from in #NamedType GenType.
      *GenType\LeftOperandTypeId = IdOfTypeDerivedFrom
      
    EndIf
    
  Next
  
EndProcedure

Procedure GenFields()
EndProcedure

Procedure.i AddDispatchTreeNode( *DispatchTree.GenDispatchTree, Parent.i, ArgumentTypeId.i, ResultTypeId.i, *GenMethod.GenMethod = #Null )
  
  If ArraySize( *DispatchTree\Nodes() ) = *DispatchTree\NodeCount
    ReDim *DispatchTree\Nodes( *DispatchTree\NodeCount + 128 )
  EndIf
  
  Define.i Index = *DispatchTree\NodeCount
  *DispatchTree\NodeCount + 1
  
  Define.GenDispatchTreeNode *Ptr = @*DispatchTree\Nodes( Index )
  
  *Ptr\ArgumentTypeId = ArgumentTypeId
  *Ptr\ResultTypeId = ResultTypeId
  *Ptr\Parent = Parent
  *Ptr\FirstChild = #INVALID_NODE_INDEX
  *Ptr\FirstAfter = #INVALID_NODE_INDEX
  *Ptr\FirstBefore = #INVALID_NODE_INDEX
  *Ptr\FirstAround = #INVALID_NODE_INDEX
  
  If Parent <> #INVALID_NODE_INDEX
    *Ptr\NextSibling = *DispatchTree\Nodes( Parent )\FirstChild
    *DispatchTree\Nodes( Parent )\FirstChild = Index
  Else
    *Ptr\NextSibling = #INVALID_NODE_INDEX
  EndIf
  
  ProcedureReturn Index
  
EndProcedure

Procedure.i FindDispatchTreeNode( *DispatchTree.GenDispatchTree, ArgumentTypeId.i, StartingNode.i = 0 )
  
  Define.i NodeIndex = StartingNode
  
  If Not FirstIsSubtypeOfSecond( ArgumentTypeId, *DispatchTree\Nodes( NodeIndex )\ArgumentTypeId )
    ; Entire dispatch subtree doesn't apply to given argument type.
    ProcedureReturn #INVALID_NODE_INDEX
  EndIf
  
  While #True
    
    ; See if any of the children are more specialized.
    Define.i ChildIndex = *DispatchTree\Nodes( NodeIndex )\FirstChild
    Define.b FoundChild = #False
    While ChildIndex <> #INVALID_NODE_INDEX
      
      If FirstIsSubtypeOfSecond( ArgumentTypeId, *DispatchTree\Nodes( ChildIndex )\ArgumentTypeId )
        FoundChild = #True
        Break
      EndIf
      
      ChildIndex = *DispatchTree\Nodes( ChildIndex )\NextSibling
      
    Wend
    
    If FoundChild
      NodeIndex = ChildIndex
    Else
      Break
    EndIf
    
  Wend
  
  ProcedureReturn NodeIndex
  
EndProcedure

; A dispatch tree puts all methods contained in a given function into a total ordering.
Procedure GenDispatchTree( *GenFunction.GenFunction )
  
  Define.GenDispatchTree *DispatchTree = @*GenFunction\DispatchTree
  
  ; Add root node representing Object->Object.
  AddDispatchTreeNode( *DispatchTree, #INVALID_NODE_INDEX, GenProgram\TypeTable\ObjectTypeId, GenProgram\TypeTable\ObjectTypeId )
  
  ; Go through each method in the function and find or create the node for it.
  Define.i MethodIndex
  For MethodIndex = 0 To *GenFunction\MethodCount - 1
    
    Define.GenMethod *GenMethod = @*GenFunction\Methods( MethodIndex )
    
    ; Walk down the tree looking for where the node for this method goes.
    Define.i CurrentNodeIndex = 0 ; Start with Object->Object.
    While #True
      
      Define.GenDispatchTreeNode *Node = @*DispatchTree\Nodes( CurrentNodeIndex )
      Define.b IsBeforeAfterAroundMethodToAddToNode = #False
      
      If *GenMethod\ArgumentTypeId = *Node\ArgumentTypeId
        
        ; The node has *exactly* the type we're looking for. Might be a node we inserted
        ; without having an actual method yet (happens for the Object->Object root node that we create
        ; automatically but also for after/before/around methods). If Not, this Case is only okay If the
        ; current method is after/before/around. Otherwise, we have a duplicate definition.
        
        If ( *GenMethod\MethodFlags & ( #IsAfter | #IsBefore | #IsAround ) ) = 0
          ;;;;TODO: check result type
          ; It's not an after/before/around method.
          If *Node\Method <> #Null
            ;;;;TODO: diagnose duplicate method implementation
            Debug "Duplicate method definition for function " + *GenFunction\Name
          Else
            ; "Claim" this node.
            *Node\Method = *GenMethod
            *Node\ResultTypeId = *GenMethod\ResultTypeId
          EndIf
          Break
        Else
          IsBeforeAfterAroundMethodToAddToNode = #True
        EndIf
        
      Else
        
        If FirstIsSubtypeOfSecond( *GenMethod\ArgumentTypeId, *Node\ArgumentTypeId )
          
          Define.b HaveFoundNode = #False
          
          ; If we have children, can be that we simply have to descend into them. However,
          ; can also be that we have to insert us as an intermediate node in-between the
          ; current parent and one or more of the children.
          
          Define.i ChildIndex = *Node\FirstChild
          While ChildIndex <> #INVALID_NODE_INDEX
            
            Define.GenDispatchTreeNode *Child = @*DispatchTree\Nodes( ChildIndex )
            
            If FirstIsSubtypeOfSecond( *GenMethod\ArgumentTypeId, *Child\ArgumentTypeId )
              
              ; Descend into child node.
              ; NOTE: This branch also takes care of looking at two identical types.
              HaveFoundNode = #True
              CurrentNodeIndex = ChildIndex
              Break
              
            ElseIf FirstIsSubtypeOfSecond( *Child\ArgumentTypeId, *GenMethod\ArgumentTypeId )
              
              ; We're a supertype of the child's type. We need to insert ourselves
              ; in-between. This may apply to other children of the current parent as well.
              Define.i NewNodeIndex = AddDispatchTreeNode( *DispatchTree, CurrentNodeIndex, *GenMethod\ArgumentTypeId, *GenMethod\ResultTypeId )
              Define.GenDispatchTreeNode *NewNode = @*DispatchTree\Nodes( NewNodeIndex )
              HaveFoundNode = #True
              
              ; Go through all the children of the parent and move all of the ones that
              ; are subtypes to below our new node.
              ChildIndex = *NewNode\NextSibling ; *Node\FirstChild is == NewNodeIndex and NextSibling is what used to be FirstChild.
              *Node\FirstChild = #INVALID_NODE_INDEX ; We'll rebuild the child list on the parent.
              *NewNode\NextSibling = #INVALID_NODE_INDEX
              While ChildIndex <> #INVALID_NODE_INDEX
                
                *Child = @*DispatchTree\Nodes( ChildIndex )
                Define.i NextChildIndex = *Child\NextSibling
                
                If FirstIsSubtypeOfSecond( *Child\ArgumentTypeId, *GenMethod\ArgumentTypeId )
                  
                  ; Add it to our new node's children.
                  
                  *Child\Parent = NewNodeIndex
                  *Child\NextSibling = *NewNode\FirstChild
                  *NewNode\FirstChild = ChildIndex
                  
                Else
                  
                  ; Add it to the parent's children.
                  *Child\NextSibling = *Node\FirstChild
                  *Node\FirstChild = ChildIndex
                  
                EndIf
                
                ChildIndex = NextChildIndex
                
              Wend
              
              Break
              
            EndIf
            
            ChildIndex = *Child\NextSibling
            
          Wend
          
          ; If at this point, we have found a node arrangement, just create a new node
          ; for the current method and insert us.
          If Not HaveFoundNode
            Define.i NewNodeIndex = AddDispatchTreeNode( *DispatchTree, CurrentNodeIndex, *GenMethod\ArgumentTypeId, *GenMethod\ResultTypeId )
            *DispatchTree\Nodes( NewNodeIndex )\Method = *GenMethod
            Break
          Else
            
          EndIf
          
        Else
          DebuggerError( "Error... we're in a branch of the dispatch tree where the current argument type is no longer a subtype! " + GenTypeNameById( *GenMethod\ArgumentTypeId ) + " <! " + GenTypeNameById( *Node\ArgumentTypeId ) )
          Break
        EndIf
        
      EndIf
      
      If IsBeforeAfterAroundMethodToAddToNode
        ;;;;TODO: actually add to node
        Break
      EndIf
      
    Wend
    
  Next
  
EndProcedure

; Generates code for each function and as a side-effect, typechecks each
; value expression used in methods.
Procedure GenFunctions()
  
  ;{ Generate dispatch trees for all functions.
  
  ForEach GenProgram\FunctionTable\Functions()
    
    Define.GenFunction *GenFunction = GenProgram\FunctionTable\Functions()
    GenDispatchTree( *GenFunction )
    
  Next
  
  ;}
  
  ;{ Generate instructions for all functions.
  
  ReDim GenProgram\FunctionTable\CallSites( Code\ExpressionCount )
  FillMemory( @GenProgram\FunctionTable\CallSites( 0 ), ArraySize( GenProgram\FunctionTable\CallSites() ) * SizeOf( GenCallSite ), 0, #PB_Long )
  
  ForEach GenProgram\FunctionTable\Functions()
    
    Define.GenFunction *GenFunction = GenProgram\FunctionTable\Functions()
    
  Next
  
  ;}
  
  ;;;;TODO: generate specializations
  
  ;{ Emit functions.
  
  Define.i FunctionCount = MapSize( GenProgram\FunctionTable\Functions() )
  ReDim Program\Functions( FunctionCount )
  Program\FunctionCount = FunctionCount
  
  Define.i FunctionIndex = 0
  ForEach GenProgram\FunctionTable\Functions()
    
    Define.GenFunction *GenFunction = GenProgram\FunctionTable\Functions()
    Define.Function *Function = @Program\Functions( FunctionIndex )
    
    *Function\Name = *GenFunction\Name
    
    FunctionIndex + 1
    
  Next
  
  ;}
  
EndProcedure

Procedure GenObjects()
EndProcedure

Procedure GenAssets()
EndProcedure

Procedure GenProgram()
  
  Program\Name = GenProgram\Name
  Program\Company = GenProgram\Company
  Program\Product = GenProgram\Product
  
EndProcedure

; Translate `Code` into `Program`.
Procedure TranslateProgram( WithLibraries.b )
  
  If GenProgram\TypeTable\SubtypeMatrix <> #Null
    FreeMemory( GenProgram\TypeTable\SubtypeMatrix )
    GenProgram\TypeTable\SubtypeMatrix = #Null
  EndIf
  
  ResetStructure( @Program, Program )
  ResetStructure( @GenProgram, GenProgram )
  
  ;;;;FIXME: somehow the custom product and company strings we relate aren't coming through in the player builds...
  Program\Product = "DefaultProduct"
  Program\Company = "DefaultCompany"
  
  GenCollect()
  
  If WithLibraries
    If GenProgram\TypeTable\ObjectTypeId = #INVALID_TYPE_ID
      Debug "Object type missing"
      ProcedureReturn
    EndIf
    If GenProgram\TypeTable\IntegerTypeId = #INVALID_TYPE_ID
      Debug "Integer type missing"
      ProcedureReturn
    EndIf
    If GenProgram\TypeTable\FloatTypeId = #INVALID_TYPE_ID
      Debug "Float type missing"
      ProcedureReturn
    EndIf
    If GenProgram\TypeTable\ImmutableStringTypeId = #INVALID_TYPE_ID
      Debug "ImmutableString type missing"
      ProcedureReturn
    EndIf
    If GenProgram\TypeTable\NothingTypeId = #INVALID_TYPE_ID
      Debug "Nothing type missing"
      ProcedureReturn
    EndIf
    If GenProgram\TypeTable\TrueTypeId = #INVALID_TYPE_ID
      Debug "True type missing"
      ProcedureReturn
    EndIf
    If GenProgram\TypeTable\FalseTypeId = #INVALID_TYPE_ID
      Debug "False type missing"
      ProcedureReturn
    EndIf
  EndIf
  
  GenTypes()
  GenFields()
  GenFunctions()
  GenObjects()
  GenAssets()
  GenProgram()
  
  Debug "GenFunctions: " + Str( MapSize( GenProgram\FunctionTable\Functions() ) ) +
        ", GenTypes: " + Str( GenProgram\TypeTable\TypeCount ) +
        ", GenValues: " + Str( GenProgram\ValueTable\ValueCount ) +
        ", GenScopes: " + Str( GenProgram\SymbolTable\ScopeCount ) +
        ", GenSymbols: " + Str( GenProgram\SymbolTable\SymbolCount )
  
EndProcedure

Procedure.s ToTypeMessage( *Type.Type )
  ProcedureReturn "t|" + *Type\Name
EndProcedure

Procedure.s ToFunctionMessage( *Function.Function )
  ProcedureReturn "f|" + *Function\Name
EndProcedure

Procedure.s ToInstructionMessage( *Instruction.Instruction )
  
  Define.s Insn
  Select *Instruction\Opcode
      
    Case #ICallInsn
      Insn = "icall|" + Str( *Instruction\Operand1 )
      
    Case #BlockInsn
      Insn = ".block|" + Str( *Instruction\Operand1 )
      
    Case #ConditionalInsn
      ;;;;TODO
      Insn = ".if|" + Str( *Instruction\Operand1 )
      
    Case #TypecaseInsn
      Insn = ".typecase|" + Str( *Instruction\Operand1 ) + "|" + Str( *Instruction\Operand2 )
      
  EndSelect
  
  ProcedureReturn "i|" + insn
  
EndProcedure

Procedure SendProgram()
  
  If Not UnityPlayerClient And Not WriteGeneratedProgramToDisk
    ProcedureReturn
  EndIf
  
  Define.i File = 0
  If WriteGeneratedProgramToDisk
    File = OpenFile( #PB_Any, TextFilePath + ".gen", #PB_UTF8 )
  EndIf
  
  If UnityPlayerClient
    StartBatchSend( UnityPlayerClient )
    ;;;;TODO: send program delta for incremental changes instead of resetting all the time
    BatchSendString( "reset" )
  EndIf
  
  Define.i Index
  
  ; Send types.
  If Program\TypeCount > 0
    For Index = 0 To Program\TypeCount - 1
      Define.Type *Type = @Program\Types( Index )
      Define.s Message = ToTypeMessage( *Type )
      If File
        WriteStringN( File, Message, #PB_UTF8 )
      EndIf
      If UnityPlayerClient
        BatchSendString( Message )
      EndIf
    Next
  EndIf
  
  ; Send functions.
  If Program\FunctionCount > 0
    For Index = 0 To Program\FunctionCount - 1
      Define.Function *Function = @Program\Functions( Index )
      Define.s Message = ToFunctionMessage( *Function )
      If File
        WriteStringN( File, Message, #PB_UTF8 )
      EndIf
      If UnityPlayerClient
        BatchSendString( Message )
      EndIf
      
      ; Send instructions.
      Define.i InstructionIndex
      For InstructionIndex = 0 To *Function\InstructionCount - 1
        Define.Instruction *Instruction = @Program\Instructions( *Function\FirstInstruction + InstructionIndex )
        Message = ToInstructionMessage( Instruction )
        If File
          ; For debugging, write out instruction index explicitly.
          WriteString( File, Str( InstructionIndex ), #PB_UTF8 )
          WriteString( File, ": ", #PB_UTF8 )
          WriteStringN( File, Message, #PB_UTF8 )
        EndIf
        If UnityPlayerClient
          BatchSendString( Message )
        EndIf
      Next
    Next
  EndIf
  
  ; Send objects.
  
  ; Send assets.
  
  If File
    CloseFile( File )
  EndIf
  If UnityPlayerClient
    BatchSendString( "commit" )
    FinishBatchSend()
  EndIf
    
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

Declare RefreshDocs()

Procedure UpdateProgram( WithLibraries.b = #True )
  
  Debug "Compiling..."
    
  ResetCode()
  If WithLibraries
    LoadLibraries()
  EndIf
  ParseText()
  
  If Code\ErrorCount > 0
    ProcedureReturn
  EndIf
  
  TranslateProgram( WithLibraries )
  SendProjectSettings()
  
  If Code\ErrorCount > 0
    ProcedureReturn
  EndIf
  
  SendProgram()
  RefreshDocs()

EndProcedure

;{ TESTS

ProcedureUnit CanCompileSimpleProgram()

  Define.s Text = ~"type FirstType;\n" +
                  ~"type SecondType : FirstType;\n" +
                  ~"type ThirdType;\n" +
                  ~"type FourthType = FirstType | ThirdType;\n" +
                  ~"type FifthType = ThirdType;\n" +
                  ~"type SixthType = FourthType | SecondType;\n" +
                  ~"type SeventhType : SecondType;\n"
  
  *Text = UTF8( Text )
  TextLength = Len( Text )
  
  UpdateProgram( #False )
  
  Assert( Program\TypeCount = 6 )
  Assert( ArraySize( Program\Types() ) = 6 )
  Assert( Program\Types( 0 )\Name = "first_type" )
  Assert( Program\Types( 1 )\Name = "second_type" )
  Assert( Program\Types( 2 )\Name = "third_type" )
  Assert( Program\Types( 3 )\Name = "seventh_type" )
  Assert( Program\Types( 4 )\Name = "or(first_type,third_type)" )
  Assert( Program\Types( 5 )\Name = "or(second_type,or(first_type,third_type))" )
  
  Assert( GenProgram\TypeTable\TypeCount = 6 )
  Assert( ArraySize( GenProgram\TypeTable\Types() ) >= 6 )
  
  Assert( MapSize( GenProgram\TypeTable\NamedTypes() ) = 7 )
  Assert( GenProgram\TypeTable\NamedTypes( "first_type" ) = 1 )
  Assert( GenProgram\TypeTable\NamedTypes( "second_type" ) = 2 )
  Assert( GenProgram\TypeTable\NamedTypes( "third_type" ) = 3 )
  Assert( GenProgram\TypeTable\NamedTypes( "fourth_type" ) = 5 )
  Assert( GenProgram\TypeTable\NamedTypes( "fifth_type" ) = #INVALID_TYPE_ID ) ; Unused alias.
  Assert( GenProgram\TypeTable\NamedTypes( "sixth_type" ) = #INVALID_TYPE_ID ) ; Unused alias.
  Assert( GenProgram\TypeTable\NamedTypes( "seventh_type" ) = 4 ) ; Unused alias.
  
  Assert( FirstIsSubtypeOfSecond( 1, 2 ) = #False ) ; FirstType   !: SecondType
  Assert( FirstIsSubtypeOfSecond( 2, 1 ) = #True )  ; SecondType  <: FirstType
  Assert( FirstIsSubtypeOfSecond( 1, 1 ) = #True )  ; FirstType   <: FirstType
  Assert( FirstIsSubtypeOfSecond( 2, 2 ) = #True )  ; SecondType  <: SecondType
  Assert( FirstIsSubtypeOfSecond( 1, 5 ) = #True )  ; FirstType   <: ( FirstType | ThirdType )
  Assert( FirstIsSubtypeOfSecond( 5, 1 ) = #False ) ; ( FirstType | ThirdType ) !: FirstType
  Assert( FirstIsSubtypeOfSecond( 1, 4 ) = #False ) ; FirstType   !: SeventhType
  Assert( FirstIsSubtypeOfSecond( 4, 1 ) = #True )  ; SeventhType <: FirstType
  Assert( FirstIsSubtypeOfSecond( 4, 2 ) = #True )  ; SeventhType <: SecondType
  
  FreeMemory( *Text )
  
EndProcedureUnit

ProcedureUnit CanGenerateDispatchTree()

  Define.s Text = ~"type Object;\n" +
                  ~"type String : Object;\n" +
                  ~"type Boolean = True | False;\n" +
                  ~"object True;\n" +
                  ~"object False;\n" +
                  ~"method ToString( Object ) : String return 1; end;\n" +
                  ~"method ToString( Boolean ) : String return 2; end;\n" +
                  ~"method ToString( True ) : String return 3; end;\n"
  
  *Text = UTF8( Text )
  TextLength = Len( Text )
  
  UpdateProgram( #False )
  
  Assert( MapSize( GenProgram\FunctionTable\Functions() ) = 1 )
  Assert( FindMapElement( GenProgram\FunctionTable\Functions(), "to_string" ) <> #Null )
  
  Define.GenFunction *GenFunction = GenProgram\FunctionTable\Functions()
  
  Assert( *GenFunction\MethodCount = 3 )
  Assert( *GenFunction\DispatchTree\NodeCount = 3 )
  Assert( *GenFunction\DispatchTree\Nodes( 0 )\ArgumentTypeId = GenProgram\TypeTable\ObjectTypeId )
  Assert( *GenFunction\DispatchTree\Nodes( 0 )\ResultTypeId = GenProgram\TypeTable\NamedTypes( "string" ) )
  Assert( *GenFunction\DispatchTree\Nodes( 0 )\Method = @GenProgram\FunctionTable\Functions( "to_string" )\Methods( 0 ) )
  Assert( *GenFunction\DispatchTree\Nodes( 0 )\Parent = -1 )
  Assert( *GenFunction\DispatchTree\Nodes( 1 )\ArgumentTypeId = GenProgram\TypeTable\NamedTypes( "boolean" ) )
  Assert( *GenFunction\DispatchTree\Nodes( 1 )\ResultTypeId = GenProgram\TypeTable\NamedTypes( "string" ) )
  Assert( *GenFunction\DispatchTree\Nodes( 1 )\Parent = 0 )
  Assert( *GenFunction\DispatchTree\Nodes( 1 )\Method = @GenProgram\FunctionTable\Functions( "to_string" )\Methods( 1 ) )
  Assert( *GenFunction\DispatchTree\Nodes( 2 )\ArgumentTypeId = GenProgram\TypeTable\NamedTypes( "true" ) )
  Assert( *GenFunction\DispatchTree\Nodes( 2 )\ResultTypeId = GenProgram\TypeTable\NamedTypes( "string" ) )
  Assert( *GenFunction\DispatchTree\Nodes( 2 )\Method = @GenProgram\FunctionTable\Functions( "to_string" )\Methods( 2 ) )
  Assert( *GenFunction\DispatchTree\Nodes( 2 )\Parent = 1 )
  
EndProcedureUnit
  
ProcedureUnit CanGenerateInstructionsForSimpleFunction()
  
  Define.s Text = ~"type Object;\n" +
                  ~"type A : Object;\n" +
                  ~"type B : Object;\n" +
                  ~"object C : Object;\n" +
                  ~"type String : Object;\n" +
                  ~"method ToString( Object ) : String return \"Object\"; end;\n" +
                  ~"method ToString( A ) : String return \"A\"; end;\n" +
                  ~"method ToString( B | C ) : String return \"B | C\"; end;\n"
  
  *Text = UTF8( Text )
  TextLength = Len( Text )
  
  UpdateProgram( #False )
  
  Assert( MapSize( GenProgram\FunctionTable\Functions() ) = 1 )
  Assert( FindMapElement( GenProgram\FunctionTable\Functions(), "to_string" ) <> #Null )
  Assert( MapSize( Code\StringLiterals() ) = 3 )
  Assert( FindMapElement( Code\StringLiterals(), "Object" ) <> #Null )
  Assert( FindMapElement( Code\StringLiterals(), "A" ) <> #Null )
  Assert( FindMapElement( Code\StringLiterals(), "B | C" ) <> #Null )
  
  Define.GenFunction *GenFunction = GenProgram\FunctionTable\Functions()
  
  Assert( *GenFunction\MethodCount = 3 )
  
  ;......what does the instruction sequence look like
  
EndProcedureUnit

CompilerIf #False
ProcedureUnit CanDetectCallToMissingFunction()

  Define.s Text = ~"type Object;\n" +
                  ~"object Nothing;\n" +
                  ~"method Test() Flub(); end;\n";
  
  *Text = UTF8( Text )
  TextLength = Len( Text )
  
  UpdateProgram( #False )
  
  ;
  
EndProcedureUnit
CompilerEndIf

ProcedureUnit CanCreatedNestedSymbolScopes()

  ResetStructure( @GenProgram, GenProgram )

  Define OuterScope = NewGenScope( -1 )
  
  Define FirstSymbol = NewGenSymbol( OuterScope, 0 )
  Define SecondSymbol = NewGenSymbol( OuterScope, 1 )
  
  Define InnerScope = NewGenScope( OuterScope )
  
  Define ThirdSymbol = NewGenSymbol( InnerScope, 2 )
  Define FourthSymbol = NewGenSymbol( OuterScope, 3 ) ; Add one to outer scope after inner scope already exists.
  
  Assert( GenProgram\SymbolTable\ScopeCount = 2 )
  Assert( GenProgram\SymbolTable\SymbolCount = 4 )
  
  Assert( LookupSymbol( InnerScope, 4 ) = -1 )
  Assert( LookupSymbol( InnerScope, 3 ) = FourthSymbol )
  Assert( LookupSymbol( InnerScope, 2 ) = ThirdSymbol )
  Assert( LookupSymbol( InnerScope, 1 ) = SecondSymbol )
  Assert( LookupSymbol( InnerScope, 0 ) = FirstSymbol )

  Assert( LookupSymbol( OuterScope, 4 ) = -1 )
  Assert( LookupSymbol( OuterScope, 3 ) = FourthSymbol )
  Assert( LookupSymbol( OuterScope, 2 ) = -1 )
  Assert( LookupSymbol( OuterScope, 1 ) = SecondSymbol )
  Assert( LookupSymbol( OuterScope, 0 ) = FirstSymbol )
  
EndProcedureUnit

;}

;==============================================================================
;-== Documentation.

;;;;TODO: put this on a separate thread
;;;;TODO: support docs for language elements (just have manual tab with handwritten content?)
;;;;TODO: add tab for assets
;;;;TODO: generate from GenProgram instead of Code

Global.b TemplatesLoaded = #False
Global.s TOCTemplate
Global.b TOCNeedsToBeRegenerated = #True
Global.s TOC

#TOC_TYPE_LIST_MARKER = "<!--TYPES-->"
#TOC_METHOD_LIST_MARKER = "<!--METHODS-->"

#UNCATEGORIZED = "<Uncategorized>"

Procedure LoadDocTemplates()
  
  If TemplatesLoaded
    ProcedureReturn
  EndIf
  
  Define.i TOCFile = ReadFile( #PB_Any, GeneratedDocsPath + "\toc.html" )
  TOCTemplate = ReadString( TOCFile, #PB_UTF8 | #PB_File_IgnoreEOL )
  CloseFile( TOCFile )
  
  TemplatesLoaded = #True
  
EndProcedure

Structure Category
  List Entries.s()
EndStructure

;;;;FIXME: using characters in category names that need to be HTML entities will lead to HTML errors

Procedure AddDefinitionToCategory( *Definition.Definition, Category.s, Map TypeCategories.Category(), Map MethodCategories.Category() )
  
  Define *Category.Category
  ;;;;REVIEW: how should we handle fields here? add them to methods?
  Select *Definition\DefinitionKind
      
    Case #TypeDefinition
      *Category = FindMapElement( TypeCategories(), Category )
      If *Category = #Null
        *Category = AddMapElement( TypeCategories(), Category )
      EndIf
      
    Case #MethodDefinition
      *Category = FindMapElement( MethodCategories(), Category )
      If *Category = #Null
        *Category = AddMapElement( MethodCategories(), Category )
      EndIf
      
    Default
      ProcedureReturn
      
  EndSelect
  
  AddElement( *Category\Entries() )
  *Category\Entries() = Code\Identifiers( *Definition\Name )
  
EndProcedure

Procedure CollectCategories( Map TypeCategories.Category(), Map MethodCategories.Category() )
  
  ;;;;TODO: fold this into the parsing or gen pass
  
  If Code\DefinitionCount = 0
    ProcedureReturn
  EndIf
  
  Define.i DefinitionIndex
  For DefinitionIndex = 0 To Code\DefinitionCount - 1
    
    Define *Definition.Definition = @Code\Definitions( DefinitionIndex )
    
    ;;;;TODO: take inheritance into account
    
    Define.b IsCategorized = #False
    Define.i AnnotationIndex = *Definition\FirstAnnotation
    While AnnotationIndex <> -1
      ;;;;TODO: look for asset annotations
      Define *Annotation.Annotation = @Code\Annotations( AnnotationIndex )
      If *Annotation\AnnotationKind = #CategoryAnnotation
        IsCategorized = #True
        Define.s CategoryName = Trim( *Annotation\AnnotationText )
        AddDefinitionToCategory( *Definition, CategoryName, TypeCategories(), MethodCategories() )
      EndIf
      AnnotationIndex = *Annotation\NextAnnotation
    Wend
    
    If Not IsCategorized
      AddDefinitionToCategory( *Definition, #UNCATEGORIZED, TypeCategories(), MethodCategories() )
    EndIf
    
  Next
  
  ForEach TypeCategories()
    SortList( TypeCategories()\Entries(), #PB_Sort_Ascending | #PB_Sort_NoCase )
  Next
  ForEach MethodCategories()
    SortList( MethodCategories()\Entries(), #PB_Sort_Ascending | #PB_Sort_NoCase )
  Next

EndProcedure

Procedure AddCategory( Path.s, *Builder.StringBuilder, Map Categories.Category() )
  
  ;;;;TODO: automatically indent types based on their derivation
  
  Define.s Category = MapKey( Categories() )
  
  If Category <> #UNCATEGORIZED
    AppendString( *Builder, "<li><p>" )
    AppendString( *Builder, MapKey( Categories() ) )
    AppendString( *Builder, "<p><ul>" )
  EndIf
  
  ForEach Categories()\Entries()
    AppendString( *Builder, ~"<li><a href=\"" )
    AppendString( *Builder, DocURL )
    AppendString( *Builder, Path )
    AppendString( *Builder, Categories()\Entries() )
    AppendString( *Builder, ~"\">" )
    AppendString( *Builder, FormatIdentifier( Categories()\Entries() ) )
    AppendString( *Builder, "</a></li>" )
  Next
  
  If Category <> ""
    AppendString( *Builder, "</ul></li>" )
  EndIf
  
EndProcedure

Procedure.s GenerateTOCList( Path.s, Map Categories.Category() )
  
  Define.StringBuilder Builder
  
  ; Add uncategorized first.
  If FindMapElement( Categories(), #UNCATEGORIZED )
    AddCategory( Path, @Builder, Categories() )
  EndIf
  
  ; Add categorized.
  ForEach Categories()
    If MapKey( Categories() ) = #UNCATEGORIZED
      Continue
    EndIf
    AddCategory( Path, @Builder, Categories() )
  Next
  
  ProcedureReturn GetString( @Builder )
  
EndProcedure

Procedure GenerateTableOfContents()
  
  NewMap TypeCategories.Category()
  NewMap MethodCategories.Category()
  
  CollectCategories( TypeCategories(), MethodCategories() )
  
  Define.s TypeList = GenerateTOCList( "/types/", TypeCategories() )
  Define.s MethodList = GenerateTOCList( "/methods/", MethodCategories() )
  
  TOC = ReplaceString( TOCTemplate, #TOC_TYPE_LIST_MARKER, TypeList )
  TOC = ReplaceString( TOC, #TOC_METHOD_LIST_MARKER, MethodList )
  
  TOCNeedsToBeRegenerated = #False
  
EndProcedure

Procedure CollectDocAnnotations( *StringBuilder.StringBuilder, AnnotationKind.i, *Definition.Definition )
  
  Define.b IsFirst = #True
  
  Define.i AnnotationIndex = *Definition\FirstAnnotation
  While AnnotationIndex <> -1
    
    Define.Annotation *Annotation = @Code\Annotations( AnnotationIndex )
    AnnotationIndex = *Annotation\NextAnnotation
    
    If *Annotation\AnnotationKind <> AnnotationKind
      Continue
    EndIf
    
    If IsFirst
      AppendString( *StringBuilder, "<p>" )
      IsFirst = #False
    ElseIf *Annotation\AnnotationText = ""
      ; Empty line starts new paragraph.
      AppendString( *StringBuilder, "</p><p>" )
    EndIf
    
    AppendString( *StringBuilder, *Annotation\AnnotationText )
    AppendString( *StringBuilder, " " )
    
  Wend
  
  If Not IsFirst
    AppendString( *StringBuilder, "</p>" )
  EndIf
  
EndProcedure

Procedure AddDocAnnotations( *StringBuilder.StringBuilder, *Definition.Definition )
  
  ;;;;TODO: collect inherited docs
  
  ; Add description.
  AppendString( *StringBuilder, "<h3>Description</h3>" )
  CollectDocAnnotations( *StringBuilder, #DescriptionAnnotation, *Definition )
  
  ; Add details.
  AppendString( *StringBuilder, "<h3>Details</h3>" )
  CollectDocAnnotations( *StringBuilder, #DetailsAnnotation, *Definition )
  
EndProcedure

Procedure GenerateDocsForDefinition( DefinitionKind.i, Name.s, *Builder.StringBuilder )
  
  ;;;;TODO: parse x: markup
  
  ; Add title.
  AppendString( *Builder, "<h2>" )
  AppendString( *Builder, DefinitionKindToString( DefinitionKind ) )
  AppendString( *Builder, ": <i>" )
  AppendString( *Builder, FormatIdentifier( Name ) )
  AppendString( *Builder, "</i></h2>" )
  
  ;;;;TODO: for types, add supertype information (with link); or better yet; type hierarchy
  ;;;;TODO: for functions, print prototypes (with links)
  
  Select DefinitionKind
      
    Case #TypeDefinition
      Define.i TypeId = LookupNamedTypeFromString( Name )
      If TypeId <> #INVALID_TYPE_ID
        Define.GenType *GenType = GenTypePtr( TypeId )
        Define.i DefinitionIndex = *GenType\DefinitionIndex
        AddDocAnnotations( *Builder, @Code\Definitions( DefinitionIndex ) )
      EndIf
      
  EndSelect
  
EndProcedure

Procedure.s GenerateDocsAt( Path.s )
  
  LoadDocTemplates()
  
  Dim Parts.s( 0 )
  SplitString( Parts(), Path, "/" )
  
  If TOCNeedsToBeRegenerated
    GenerateTableOfContents()
  EndIf
  
  Define.StringBuilder Builder
  
  AppendString( @Builder, TOC )
  AppendString( @Builder, ~"<td style=\"width: 80%; vertical-align: top;\"><div class=\"main\">" )
  
  Select ArraySize( Parts() )
      
      ; Index #0 should always be the root '/'.
    Case 0
    Case 1
      AppendString( @Builder, "<h1>dotBASIC</h1>" )
      AppendString( @Builder, "<p>Welcome to <b>dotBASIC</b> &ndash; an advanced, game-oriented BASIC programming environment " +
                              "built on <i>Unity DOTS</i> (Data-Oriented Tech Stack) technology.</p>" )
      AppendString( @Builder, "<h2>Overview</h2>" )
      AppendString( @Builder, "<p>This is a live programming environment. As you type and modify code on the left, the Unity player " +
                              "running below this pane as well as the documentation in this pane are automatically updated.</p>" )
      
    Case 2
      
    Default
      Select Parts( 1 )
        Case "types"
          GenerateDocsForDefinition( #TypeDefinition, Parts( 2 ), @Builder )
        Case "methods"
          GenerateDocsForDefinition( #MethodDefinition, Parts( 2 ), @Builder )
      EndSelect
      
  EndSelect
  
  AppendString( @Builder, "</div></td></tr></table></body></html>" )
  
  ProcedureReturn GetString( @Builder )
  
EndProcedure

Procedure RefreshDocs()
  
  ; Refresh doc view.
  TOCNeedsToBeRegenerated = #True
  If DocViewer
    Define.s URL = GetGadgetText( DocViewer )
    SetGadgetText( DocViewer, URL )
    ;SetGadgetState( DocViewer, #PB_Web_Refresh )
  EndIf
  
EndProcedure

Procedure OpenDocsForDefinition( DefinitionType.i, Name.s )
EndProcedure

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
;-== Main loop.

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
          Select NetworkServerEvent( UnityServer )
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
              Define.s Text = ReceiveString( EventClient() )
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
          EndSelect
          
          Select NetworkServerEvent( DocServer )
              
            Case #PB_NetworkEvent_Connect
              Debug "Doc client connected"
              
            Case #PB_NetworkEvent_Disconnect
              Debug "Doc client disconnected"
              
            Case #PB_NetworkEvent_Data
              Debug "Doc client asking for stuff"
              
              ; Parse request.
              Define.s Text = ReceiveString( EventClient() )
              Define.HTTPRequest Request
              ParseHTTPRequest( Text, @Request )
              
              ; Generate content.
              Define.s Content = GenerateDocsAt( Request\Path )
              
              ; Build response.
              Define.HTTPResponse Response
              Response\StatusCode = 200
              Response\Body = Content
              AddMapElement( Response\Headers(), "Content-Type" )
              Response\Headers() = "text/html"
              Text = FormatHTTPResponse( @Response )
              SendNetworkString( EventClient(), Text, #PB_UTF8 )
              
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
;[X] Can generate subtype matrix
;[X] Can assign types to methods
;[X] Generate dispatch trees
;[X] Scopes have symbols tables attached to them (both types and values)
;[ ] Can codegen dispatch trees
;[X] Can have string literals
;[X] Can write apply expressions in dot form
;[ ] Can generate a "match argument" sequence for one method
;[ ] Can translate a simple if statement followed by a return statement
;[ ] Can generate and populate functions with methods
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

;[X] Can have type aliases
;[ ] Can have method aliases
;[X] Can have implicitly named parameters
;[ ] Can write parameterized types
;[ ] Can instantiate parameterized types
;[ ] Can write parameterized methods
;[ ] Can invoke parameterized methods
;[ ] Can generate specialized versions of functions

; ...

;[ ] Make most semicolons optional
;[ ] Split up source code for IDE into modules
;[ ] Dark theme for text editor
;[ ] Diagnostics are shown as annotations on code
;[X] Can generate docs from code
;[ ] Can jump to docs by pressing F1
;[ ] Can jump to definition by pressing F2
;[ ] Can jump to definition by opening goto popup
;[ ] Can run tests from code in player
;[ ] Syntax highlighting in text editor
;[ ] Auto-completion in text editor
;[ ] Make parser predictive and the syntax more liberal
;[ ] Show indentation guides

; ....

;[ ] Vim mode

; Optimizations
; - Make a pass over the structs and make them more compact (indices can be 32bit, for example)
; - Instead of just plain arrays, have chunked/segmented arrays where the whole array does not need to be re-allocated and segments can be allocated in pages
; - Instead of having `NextXXX` fields, switch to an approach where lists are added in bulk and the owner instead has `FirstXXX` and `XXXCount` fields

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
; CursorPosition = 4858
; FirstLine = 4842
; Folding = --------------------
; Markers = 2768,2999,4352
; EnableXP