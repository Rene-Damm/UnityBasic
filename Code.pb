; Parsed source code.

XIncludeFile "Text.pb"
XIncludeFile "Utils.pb"

DeclareModule Code
  
  UseModule Text
  
  Enumeration DiagnosticCode
    
    ; Syntax errors.
    
  EndEnumeration

  Structure Diagnostic
    Code.i
    Index.i ; Array is determined automatically from what type of diagnostic it is (based on `Code`).
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
    
    ; Settings annotations.
    #NamingConventionAnnotation
    #IndentationAnnotation
    #BuildOptionAnnotation
    
    ; Misc annotations.
    #IcallAnnotation
    
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
  
  ; Expression language is the same between types and values. Context
  ; determines which of the two the expression relates to.
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
    #ResendStatement;;doesn't this need to be an expression??
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
    #TypeDefinition = 1 ; Object definition is a type definition with a singleton modifier.
    #MethodDefinition
    #FieldDefinition
    #FeatureDefinition
    #InterfaceDefinition
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
  
  ; These are de-duplicated. One instance of the same field is applied to every
  ; single type it applies to.
  Structure Field
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
  
  Enumeration TypeKind
    #NamedType = 1
    #UnionType
    #IntersectionType
    #InstancedType
    #TupleType
    #FunctionType
    #DependentType
  EndEnumeration
  
  EnumerationBinary TypeFlags
    #IsSingletonType
    #IsAbstractType
    #IsImmutableType
    #IsMutableType
  EndEnumeration
  
  #INVALID_TYPE_ID = 0
  
  ; Every type used in the program gets its own type instance.
  Structure Type
    Id.l                  ; Unique consecutive identifier.
    TypeKind.b
    TypeFlags.b
    DefinitionIndex.l     ; If coming from definition (named types). Also gives the name.
    SingletonIndex.l      ; Index into ObjectTable if the type is a singleton.
    FieldCount.i
    Array Fields.Field( 0 )
    ; If it's a type combinator, this is the IDs of the type being combined.
    ; For instanced types, the first is the generic type being instanced and the second is the tuple type applied to it.
    LeftOperandTypeId.l
    RightOperandTypeId.l
    ; IDs of types that are combined with this one.
    ; First 0 entry in array is end. If all entries taken, no 0 entry.
    ; All binary combinations are formed from type with *lower* ID (the operator is transitive).
    Array Combinations.l( 0 )
  EndStructure
  
  Structure TypeTable
    TypeCount.l
    ObjectTypeId.l
    IntegerTypeId.l
    FloatTypeId.l
    ImmutableStringTypeId.l
    NothingTypeId.l
    TrueTypeId.l
    FalseTypeId.l
    ComponentTypeId.l
    EntityTypeId.l
    SystemTypeId.l
    *SubtypeMatrix ; 2-dimensional typecount*typecount 2-bitfield matrix with subtype flags (0=not initialized, 1=is not subtype, 2=is subtype).
    Array Types.Type( 0 )
    Map NamedTypes.l() ; Maps a name to a type ID. These are the "primitive" types all other types are built from. Also contains aliases.
    Map AliasToTypeExpression.i() ; Maps a name to an expression index.
  EndStructure
  
  EnumerationBinary MethodFlags
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
  Structure Method
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
  Structure DispatchTreeNode
    *Method.Method
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
  
  Structure DispatchTree
    NodeCount.i
    Array Nodes.DispatchTreeNode( 0 )
  EndStructure
  
  ; A collection of methods that all have the same name.
  ; Essentially, each function is an 'Object -> Object' mapping.
  Structure Function
    Name.s
    InvocationCount.i
    BodyScopeIndex.i
    DispatchTree.DispatchTree
    MethodCount.i
    Array Methods.Method( 0 )
  EndStructure
  
  Structure CallSite
    *Function.Function
    DispatchNodeIndex.l
    InstructionIndex.l
  EndStructure
  
  Structure FunctionTable
    Array CallSites.CallSite( 0 ) ; Mirrors expression array.
    Map Functions.Function()
  EndStructure
  
  ; A symbol table entry.
  Structure Symbol
    IdentifierIndex.l
    ScopeIndex.l        ; The scope that *owns* the node, not necessarily the scope the symbol was defined in.
    ObjectIndex.l       ; Index into ObjectTable\Objects(). -1 if no object entry.
    TypeIndex.l         ; Index into TypeTable\Types(). -1 if no type entry.
    NextSymbol.l
  EndStructure
  
  Enumeration NamingConvention
    #GnuCase ; foo_bar
    #JavaCase ; fooBar
    #PascalCase ; FooBar
  EndEnumeration
    
  Structure Settings
    ProductName.s
    CompanyName.s
    NamingConvention.i
    TabWidth.i
    UseSoftTabs.b
    BuildOptWriteCode.b
  EndStructure
  
  ; For now, hash-table size for scopes is fixed.
  #SCOPE_HASHTABLE_SIZE = 16
  
  Structure Scope
    Parent.i ; -1 is global scope.
    Definition.i ; -1 is global scope.
    FirstDefinitionOrStatement.i ; Whether definition or statement depends on the type of scope.
    FirstChildScope.l
    NextSiblingScope.l
    SettingsIndex.l ; -1 means none.
    SymbolTable.l[ #SCOPE_HASHTABLE_SIZE ]
  EndStructure
  
  Structure Object
    TypeId.l
  EndStructure
  
  Structure ObjectTable
    ObjectCount.l
    Array Objects.Object( 0 )
  EndStructure
  
  Structure Program
    DefinitionIndex.l
    ObjectTable.ObjectTable
    TypeTable.TypeTable
    FunctionTable.FunctionTable
  EndStructure

  ; Just a bunch of arrays that contain a completely flattened representation of source code.
  ; No explicit tree structure (thus differs a lot from traditional AST).
  ; Each piece of code is a collection of definitions (programs, libraries, modules, etc).
  Structure Code
    
    ProgramCount.i
    SettingsCount.i
    IdentifierCount.i
    ScopeCount.i
    SymbolCount.i
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
    
    Array Programs.Program( 0 )
    Array Settings.Settings( 1 )
    Array Identifiers.s( 0 )
    Array Scopes.Scope( 1 ) ; First one is always the global scope
    Array Symbols.Symbol( 0 )
    Array Definitions.Definition( 0 )
    Array Statements.Statement( 0 )
    Array Expressions.Expression( 0 )
    Array Annotations.Annotation( 0 )
    Array Clauses.Clause( 0 )
    Array Parameters.Parameter( 0 )
    Array Diagnostics.Diagnostic( 0 )
    
  EndStructure
  
  Declare   ResetCode( *Code.Code )
  Declare.s FormatIdentifier( Identifier.s, NamingConvention.i )
  Declare.s ExpressionKindToString( ExpressionKind.i )
  Declare.s DefinitionKindToString( DefinitionKind.i )
  
  Declare.l MakeScopeAndReturnIndex( *Code.Code, ParentScope.l, DefinitionIndex.l = -1 )
  Declare.i MakeTypeAndReturnPointer( *Code.Code, TypeKind.i )
  Declare.i MakeCombinedTypeAndReturnPointer( *Code.Code, CombinedTypeKind.i, LeftTypeId.l, RightTypeId.l )
  Declare.i MakeObjectAndReturnIndex( *Code.Code, TypeId.l )
  Declare.i MakeProgramAndReturnPointer( *Code.Code, DefinitionIndex.l )
  Declare.i MakeSettingsAndReturnIndex( *Code.Code, CopySettingsFromIndex.l = -1 )
  
  Declare.i LookupSymbolAndReturnIndex( *Code.Code, Scope.i, Identifier.i, Context.b )
  Declare.i EnterSymbolAndReturnIndex( *Code.Code, Scope.i, Identifier.i, Context.b )
  
  Declare.l FindSettingsForScopeAndReturnIndex( *Code.Code, ScopeIndex.l )
  Declare.i InitSettingsForScopeAndReturnPointer( *Code.Code, ScopeIndex.l )
  Declare.i GetOrMakeSettingsForInnerScopeAndReturnPointer( *Code.Code, DefinitionIndex.l )
  
EndDeclareModule

Module Code
  
  UseModule Utils
      
  ; ---------------------------------------------------------------------------
  
  Procedure ResetCode( *Code.Code )
    
    ResetStructure( *Code, Code )
    
    *Code\ScopeCount = 1
    *Code\Scopes( 0 )\Parent = -1
    *Code\Scopes( 0 )\Definition = -1
    *Code\Scopes( 0 )\SettingsIndex = MakeSettingsAndReturnIndex( *Code )
    
  EndProcedure
  
  ; ---------------------------------------------------------------------------
  
  Procedure.s FormatIdentifier( Identifier.s, NamingConvention.i )
        
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
  
  ; -----------------------------------------------------------------------------
  
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
  
  ; -----------------------------------------------------------------------------
  
  Procedure.s DefinitionKindToString( DefinitionKind.i )
    Select DefinitionKind
      Case #TypeDefinition
        ProcedureReturn "Type"
      Case #MethodDefinition
        ProcedureReturn "Method"
      Case #ProgramDefinition
        ProcedureReturn "Program"
      Case #LibraryDefinition
        ProcedureReturn "Library"
      Case #ModuleDefinition
        ProcedureReturn "Module"
      Default
        ProcedureReturn "???"
    EndSelect
  EndProcedure
  
  ; -----------------------------------------------------------------------------
  
  Procedure.l MakeScopeAndReturnIndex( *Code.Code, ParentScope.l, DefinitionIndex.l = -1 )
    
    Define.l ScopeIndex = *Code\ScopeCount
    If ArraySize( *Code\Scopes() ) = ScopeIndex
      ReDim *Code\Scopes( ScopeIndex + 256 )
    EndIf
    *Code\ScopeCount + 1
    
    Define.Scope *Scope = @*Code\Scopes( ScopeIndex )
    
    *Scope\Definition = DefinitionIndex
    *Scope\Parent = ParentScope
    *Scope\FirstChildScope = -1
    *Scope\FirstDefinitionOrStatement = -1
    
    ProcedureReturn ScopeIndex
    
  EndProcedure
  
  ; -----------------------------------------------------------------------------
  ; Adds a new Type to *Code\TypeTable\Types() and returns a *pointer* to it.
  
  Procedure.i MakeTypeAndReturnPointer( *Code.Code, TypeKind.i )
    
    Define.i TypeIndex = *Code\TypeTable\TypeCount
    If ArraySize( *Code\TypeTable\Types() ) = TypeIndex
      ReDim *Code\TypeTable\Types( TypeIndex + 512 )
    EndIf
    *Code\TypeTable\TypeCount + 1
    Define.i TypeId = TypeIndex + 1
    
    Define.Type *Type = @*Code\TypeTable\Types( TypeIndex )
    *Type\TypeKind = TypeKind
    *Type\Id = TypeId
    
    ProcedureReturn *Type
  
  EndProcedure
  
  ; -----------------------------------------------------------------------------
  
  Procedure.i MakeCombinedTypeAndReturnPointer( *Code.Code, CombinedTypeKind.i, LeftTypeId.l, RightTypeId.l )
    
    ; NOTE: We allow combining a type with itself.
    
    ; For transitive operators, we always combine the type with the *higher* ID
    ; *into* the type With the *lower* one.
    If ( CombinedTypeKind = #UnionType Or CombinedTypeKind = #IntersectionType ) And LeftTypeId > RightTypeId
      Swap LeftTypeId, RightTypeId
    EndIf
    
    Define.i LeftTypeIndex = LeftTypeId - 1
    Define.i RightTypeIndex = RightTypeId - 1
    
    ; NOTE: As soon as we add a type, this pointer must be considered invalid!
    Define.Type *LeftType = @*Code\TypeTable\Types( LeftTypeIndex )
    
    ; Search the list of existing combinations for this particular one.
    Define.i CombinationIndex
    For CombinationIndex = 0 To ArraySize( *LeftType\Combinations() ) - 1
      
      Define.i CombinedTypeId = *LeftType\Combinations( CombinationIndex )
      If CombinedTypeId = #INVALID_TYPE_ID
        Break
      EndIf
      
      Define.i CombinedTypeIndex = CombinedTypeId - 1
      Define.Type *CombinedType = @*Code\TypeTable\Types( CombinedTypeIndex )
      If *CombinedType\TypeKind = CombinedTypeKind And *CombinedType\RightOperandTypeId = RightTypeId
        ; Found it. This is the right combination type and the right operand.
        ProcedureReturn *CombinedType
      EndIf
      
    Next
    
    ; We didn't find an existing combination, so add a new one.
    Define.Type *Type = MakeTypeAndReturnPointer( *Code, CombinedTypeKind )
    *Type\LeftOperandTypeId = LeftTypeId
    *Type\RightOperandTypeId = RightTypeId
    
    ; If needed, make space in array.
    *LeftType = @*Code\TypeTable\Types( LeftTypeIndex ) ; NewGenTypePtr() may have reallocated the array.
    If CombinationIndex = ArraySize( *LeftType\Combinations() )
      ReDim *LeftType\Combinations( CombinationIndex + 8 )
    EndIf
    *LeftType\Combinations( CombinationIndex ) = *Type\Id
    
    ProcedureReturn *Type
            
  EndProcedure
  
  ; -----------------------------------------------------------------------------
  ; Adds a new Object to *Code\ObjectTable\Objects() and returns the *index* for it.
  
  Procedure.i MakeObjectAndReturnIndex( *Code.Code, TypeId.l )
    
    Define.l ObjectIndex = *Code\ObjectTable\ObjectCount
    If ArraySize( *Code\ObjectTable\Objects() ) = ObjectIndex
      ReDim *Code\ObjectTable\Objects( ObjectIndex + 1024 )
    EndIf
    *Code\ObjectTable\ObjectCount + 1
    
    Define.Object *Object = @*Code\ObjectTable\Objects( ObjectIndex )
    *Object\TypeId = TypeId
    
    ProcedureReturn ObjectIndex
    
  EndProcedure
  
  ; -----------------------------------------------------------------------------
  ; Adds a new Program to *Code\Programs() and returns the *index* for it.
  
  Procedure.i MakeProgramAndReturnPointer( *Code.Code, DefinitionIndex.l )
    
    Define.l ProgramIndex = *Code\ProgramCount
    If ArraySize( *Code\Programs() ) = ProgramIndex
      ReDim *Code\Programs( ProgramIndex + 4 )
    EndIf
    *Code\ProgramCount + 1
    
    Define.Program *Program = @*Code\Programs( ProgramIndex )
    
    *Program\DefinitionIndex = DefinitionIndex
    
    ProcedureReturn *Program
    
  EndProcedure
  
  ; -----------------------------------------------------------------------------
  
  Procedure.i LookupSymbolAndReturnIndex( *Code.Code, Scope.i, Identifier.i, Context.b )
  EndProcedure
  
  ; -----------------------------------------------------------------------------
  
  Procedure.i EnterSymbolAndReturnIndex( *Code.Code, Scope.i, Identifier.i, Context.b )
  EndProcedure
  
  ; -----------------------------------------------------------------------------
  ; Adds a new Settings block to *Code\Settings() and returns the *index* for it.
  
  Procedure.i MakeSettingsAndReturnIndex( *Code.Code, CopySettingsFromIndex.l = -1 )
    
    Define.l SettingsIndex = *Code\SettingsCount
    If ArraySize( *Code\Settings() ) = SettingsIndex
      ReDim *Code\Settings( SettingsIndex + 5 )
    EndIf
    *Code\SettingsCount + 1
    
    Define.Settings *Settings = @*Code\Settings( SettingsIndex )
    
    If CopySettingsFromIndex >= 0
      Define.Settings *OtherSettings = @*Code\Settings( CopySettingsFromIndex )
      CopyStructure( *OtherSettings, *Settings, Settings )
    Else
      *Settings\CompanyName = "DefaultCompany"
      *Settings\ProductName = "DefaultProduct"
      *Settings\NamingConvention = #PascalCase
      *Settings\TabWidth = 4
      *Settings\UseSoftTabs = #True
    EndIf
    
    ProcedureReturn SettingsIndex
    
  EndProcedure
  
  ; -----------------------------------------------------------------------------
  
  Procedure.l FindSettingsForScopeAndReturnIndex( *Code.Code, ScopeIndex.l )
    
    While *Code\Scopes( ScopeIndex )\SettingsIndex < 0
      ScopeIndex = *Code\Scopes( ScopeIndex )\Parent
      If ScopeIndex < 0
        Debug "Root scope has no settings!"
        ProcedureReturn -1
      EndIf
    Wend
    
    ProcedureReturn ScopeIndex
    
  EndProcedure
  
  ; -----------------------------------------------------------------------------
  
  Procedure.i InitSettingsForScopeAndReturnPointer( *Code.Code, ScopeIndex.l )
    
    Define.l SettingsIndex
    
    Define.Scope *Scope = @*Code\Scopes( ScopeIndex )
    If *Scope\SettingsIndex >= 0
      SettingsIndex = *Scope\SettingsIndex
    Else
      
      Define.l CopySettingsFromIndex = FindSettingsForScopeAndReturnIndex( *Code, ScopeIndex )
      SettingsIndex = MakeSettingsAndReturnIndex( *Code, CopySettingsFromIndex )
      *Scope\SettingsIndex = SettingsIndex
      
    EndIf
    
    ProcedureReturn @*Code\Settings( SettingsIndex )
    
  EndProcedure
  
  ; -----------------------------------------------------------------------------
  
  Procedure.i GetOrMakeSettingsForInnerScopeAndReturnPointer( *Code.Code, DefinitionIndex.l )
    
    Define.Definition *Definition = @*Code\Definitions( DefinitionIndex )
    
    ; Add inner scope on definition, if there isn't one already.
    If *Definition\InnerScope = -1
      *Definition\InnerScope = MakeScopeAndReturnIndex( *Code, *Definition\Scope, DefinitionIndex )
    EndIf
    
    ProcedureReturn InitSettingsForScopeAndReturnPointer( *Code, *Definition\InnerScope )
    
  EndProcedure
  
EndModule

; IDE Options = PureBasic 5.73 LTS (Windows - x64)
; CursorPosition = 129
; FirstLine = 82
; Folding = ---
; EnableXP