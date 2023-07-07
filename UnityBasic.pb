
; what would this look like in a data-oriented architecture? what if all this ran on the Unity job system?

EnableExplicit

XIncludeFile "Utils.pb"
XIncludeFile "Network.pb"
XIncludeFile "HTTP.pb"
XIncludeFile "Text.pb"
XIncludeFile "Code.pb"
CompilerIf #False
XIncludeFile "Machine.pb"
XIncludeFile "Docs.pb"
XIncludeFile "Unity.pb"
CompilerEndIf

XIncludeFile "Parse.pb"
XIncludeFile "Analyze.pb"
CompilerIf #False
XIncludeFile "Compile.pb"

UseModule Utils
UseModule HTTP
UseModule Text
UseModule Code
UseModule Machine
UseModule Docs
UseModule Unity
UseModule Parse
UseModule Translate
UseModule Generate

IncludePath "GoScintilla/"
XIncludeFile "GoScintilla.pbi"

#SOURCE_FILE_EXTENSION = ".ubasic"
#UNITY_SERVER_PORT = 10978
#DOC_SERVER_PORT = 17790

Global.s GeneratedDocsPath = "C:\Dropbox\Workspaces\UnityBasic_PB\Docs";;;;TODO: kill this
Global.s DocsTemplatePath = "C:\Dropbox\Workspaces\UnityBasic_PB\Docs"
Global.s SourceProjectPath = "C:\Dropbox\Workspaces\UnityBasic_PB\TestProject"
Global.s LibrariesPath = "C:\Dropbox\Workspaces\UnityBasic_PB\Libs"
Global.s TextFilePath = SourceProjectPath + "\TestFile" + #SOURCE_FILE_EXTENSION
Global.s DocURL = "http://127.0.0.1:" + Str( #DOC_SERVER_PORT )


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

GOSCI_AddKeywords( Scintilla, "TYPE METHOD FIELD OBJECT PROGRAM BEGIN END RETURN RESEND ABSTRACT IMMUTABLE MUTABLE IF LOOP", #StyleKeyword )
GOSCI_AddKeywords( Scintilla, "|DESCRIPTION |DETAILS |COMPANY |PRODUCT |CATEGORY |ICALL |PRAGMA", #StyleAnnotation )

GOSCI_SetLexerOption( Scintilla, #GOSCI_LEXEROPTION_SEPARATORSYMBOLS, @"=+-*/%()[],.;" )

Global.i TabWidth = 4
Global.b UseSoftTabs = #True

GOSCI_SetTabs( Scintilla, TabWidth, UseSoftTabs )
ScintillaSendMessage( Scintilla, #SCI_SETINDENTATIONGUIDES, #SC_IV_REAL )

SetActiveGadget( Scintilla )

;;;;TODO: set tabs from setting in code
;GOSCI_SetTabs( Scintilla, TabWidth, UseSoftTabs )

;==============================================================================
;-== Abstract syntax.

Global Code.Code

; -----------------------------------------------------------------------------

Procedure ResetCode()
  If Scintilla <> 0
    ScintillaSendMessage( Scintilla, #SCI_ANNOTATIONCLEARALL )
  EndIf
  ResetStructure( @Code, Code )
  Code\ScopeCount = 1
  Code\Scopes( 1 )\Parent = -1
  Code\Scopes( 1 )\Definition = -1
EndProcedure

; -----------------------------------------------------------------------------

Procedure.s FormatDiagnostic( Index.i )
EndProcedure

; -----------------------------------------------------------------------------

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

; -----------------------------------------------------------------------------

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

; -----------------------------------------------------------------------------

Procedure HasAnnotation( *Definition.Definition, AnnotationKind.i )
  ProcedureReturn Bool( FindAnnotation( *Definition, AnnotationKind ) <> -1 )
EndProcedure

;==============================================================================
;-== Codegen and semantic analysis.

; This is the compiled program.
; For now, we only support a single program in source.
Global Program.Program

Global.b WriteGeneratedProgramToDisk = #False

  
; This is the intermediate representation of the program.
; For semantic analysis.
Global.GenProgram GenProgram

; -----------------------------------------------------------------------------

Procedure.s ToTypeMessage( *Type.Type )
  ProcedureReturn "t|" + *Type\Name
EndProcedure

; -----------------------------------------------------------------------------

Procedure.s ToFunctionMessage( *Function.Function )
  ProcedureReturn "f|" + *Function\Name
EndProcedure

; -----------------------------------------------------------------------------

Procedure.s ToInstructionMessage( *Instruction.Instruction )
  
  Define.s Insn
  Select *Instruction\Opcode
      
    Case #ICallInsn
      Insn = "icall|" + Str( *Instruction\ShortOperands[ 0 ] )
      
    Case #BlockInsn
      Insn = ".block|" + Str( *Instruction\LongOperand )
      
    Case #ConditionalInsn
      ;;;;TODO
      Insn = ".if|" + Str( *Instruction\LongOperand )
      
    Case #TypecaseInsn
      Insn = ".typecase|" + Str( *Instruction\LongOperand )
      
  EndSelect
  
  ProcedureReturn "i|" + insn
  
EndProcedure

; -----------------------------------------------------------------------------

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

; -----------------------------------------------------------------------------

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

; -----------------------------------------------------------------------------

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

; -----------------------------------------------------------------------------

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

; -----------------------------------------------------------------------------

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

; -----------------------------------------------------------------------------

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
  
  Assert( Program\InstructionCount = 3 + 2 + 3 + 3 ) ; 3 blocks, 2 typecases, 3 load, 3 returns.
    
EndProcedureUnit

; -----------------------------------------------------------------------------

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

; -----------------------------------------------------------------------------

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

; -----------------------------------------------------------------------------

Procedure LoadDocTemplates()
  
  If TemplatesLoaded
    ProcedureReturn
  EndIf
  
  Define.i TOCFile = ReadFile( #PB_Any, GeneratedDocsPath + "\toc.html" )
  TOCTemplate = ReadString( TOCFile, #PB_UTF8 | #PB_File_IgnoreEOL )
  CloseFile( TOCFile )
  
  TemplatesLoaded = #True
  
EndProcedure

; -----------------------------------------------------------------------------

Structure Category
  List Entries.s()
EndStructure

; -----------------------------------------------------------------------------

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

; -----------------------------------------------------------------------------

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

; -----------------------------------------------------------------------------

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

; -----------------------------------------------------------------------------

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

; -----------------------------------------------------------------------------

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

; -----------------------------------------------------------------------------

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

; -----------------------------------------------------------------------------

Procedure AddDocAnnotations( *StringBuilder.StringBuilder, *Definition.Definition )
  
  ;;;;TODO: collect inherited docs
  
  ; Add description.
  AppendString( *StringBuilder, "<h3>Description</h3>" )
  CollectDocAnnotations( *StringBuilder, #DescriptionAnnotation, *Definition )
  
  ; Add details.
  AppendString( *StringBuilder, "<h3>Details</h3>" )
  CollectDocAnnotations( *StringBuilder, #DetailsAnnotation, *Definition )
  
EndProcedure

; -----------------------------------------------------------------------------

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

; -----------------------------------------------------------------------------

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

; -----------------------------------------------------------------------------

Procedure RefreshDocs()
  
  ; Refresh doc view.
  TOCNeedsToBeRegenerated = #True
  If DocViewer
    Define.s URL = GetGadgetText( DocViewer )
    SetGadgetText( DocViewer, URL )
    ;SetGadgetState( DocViewer, #PB_Web_Refresh )
  EndIf
  
EndProcedure

; -----------------------------------------------------------------------------

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
;[ ] Break up code into modules  <-------------------------------------------------
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

CompilerEndIf

; IDE Options = PureBasic 5.73 LTS (Windows - x64)
; CursorPosition = 163
; FirstLine = 124
; Folding = -----
; Markers = 235,550
; EnableXP