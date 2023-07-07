; Analyzes Code and collects all its definitions.
; Generally needs to run after Parse and before anything that processes Code.

XIncludeFile "Code.pb"

; For tests.
XIncludeFile "Parse.pb"
XIncludeFile "Text.pb"

DeclareModule Analyze
  
  UseModule Code
  
  Declare   Analyze( *Code.Code )
  
EndDeclareModule

Module Analyze
  
  UseModule Utils
    
  ; -----------------------------------------------------------------------------
  
  ;;;;TODO: would be more efficient to fold this into the parsing pass
  Procedure CollectDefinitions( *Code.Code )
    
    ;{ Collect definitions.
    ; As a side-effect, we assign a unique type ID to every named type definition that isn't an alias. This
    ; is the first pass of type generation which essentially puts the starting set of "primitive" type IDs
    ; in place. All other type IDs are combinations synthesized from this set of primitives.
    Define.i DefinitionIndex
    For DefinitionIndex = 0 To *Code\DefinitionCount - 1
      
      Define *Definition.Definition = *Code\Definitions( DefinitionIndex )
      
      ;;;;TODO: probably need to ultimately mangle names here
      Define.s Name = *Code\Identifiers( *Definition\Name )
      
      Select *Definition\DefinitionKind
          
        Case #TypeDefinition
          
          ;--------------------------2-3021-4032-4023-=043=-204-32=04=3204=-3204$#@$#@$#@$#@@@@@@#@$
          
          ;........ the NamedTypes() thing should probably go in the symbol tables instead of into a separate map
          
          If FindMapElement( *Code\TypeTable\NamedTypes(), Name )
            ;;;;TODO: add diagnostic
            Debug "Type already defined: " + Name
            Continue
          EndIf
          
          If *Definition\Flags & #IsAlias
            ;;;;TODO: ensure that the alias doesn't attempt to add new modifiers (like 'abstract')
            AddMapElement( *Code\TypeTable\AliasToTypeExpression(), Name )
            *Code\TypeTable\AliasToTypeExpression() = *Definition\TypeExpression
            ; Put placeholder in table.
            ; NOTE: type aliases that aren't actually used will remain unresolved in the
            ;       name table.
            AddMapElement( *Code\TypeTable\NamedTypes(), Name )
            *Code\TypeTable\NamedTypes() = #INVALID_TYPE_ID
            Continue
          EndIf
          
          Define.Type *Type = MakeTypeAndReturnPointer( *Code, #NamedType )
          *Type\DefinitionIndex = DefinitionIndex
          
          AddMapElement( *Code\TypeTable\NamedTypes(), Name )
          *Code\TypeTable\NamedTypes() = *Type\Id
          
          ;;;;REVIEW: should this be an annotation rather than just hardcoded names?
          Select Name
              
            Case "object"
              *Code\TypeTable\ObjectTypeId = *Type\Id
              
            Case "integer"
              *Code\TypeTable\IntegerTypeId = *Type\Id
              
            Case "float"
              *Code\TypeTable\FloatTypeId = *Type\Id
              
            Case "immutable_string"
              *Code\TypeTable\ImmutableStringTypeId = *Type\Id
              
            Case "nothing"
              *Code\TypeTable\NothingTypeId = *Type\Id
              
            Case "true"
              *Code\TypeTable\TrueTypeId = *Type\Id
                          
            Case "false"
              *Code\TypeTable\FalseTypeId = *Type\Id
              
            Case "component"
              *Code\TypeTable\ComponentTypeId = *Type\Id
              
            Case "entity"
              *Code\TypeTable\EntityTypeId = *Type\Id
              
            Case "system"
              *Code\TypeTable\SystemTypeId = *Type\Id
              
          EndSelect
          
          ; If it's a singleton, also add an instance.
          If *Definition\Flags & #IsSingleton
            *Type\TypeFlags & #IsSingletonType
            *Type\SingletonIndex = MakeObjectAndReturnIndex( *Code, *Type\Id )
          EndIf
          
        Case #MethodDefinition
          
          ; Make sure the function is defined.
          Define.Function *Function = FindMapElement( *Code\FunctionTable\Functions(), Name )
          If *Function = #Null
            *Function = AddMapElement( *Code\FunctionTable\Functions(), Name )
            *Function\Name = Name
          EndIf
          
          ; Add the method to the function.
          Define.i MethodIndex = *Function\MethodCount
          If ArraySize( *Function\Methods() ) = MethodIndex
            ReDim *Function\Methods( MethodIndex + 32 )
          EndIf
          *Function\MethodCount + 1
          
          Define.Method *Method = @*Function\Methods( MethodIndex )
          *Method\DefinitionIndex = DefinitionIndex
          
          ; Set flags.
          If *Definition\Flags & #IsAbstract
            *Method\MethodFlags | #IsAbstractMethod
          EndIf
          If *Definition\Flags & #IsBefore
            *Method\MethodFlags | #IsBeforeMethod
          EndIf
          If *Definition\Flags & #IsAfter
            *Method\MethodFlags | #IsAfterMethod
          EndIf
          If *Definition\Flags & #IsAround
            *Method\MethodFlags | #IsAroundMethod
          EndIf
          
        Case #ProgramDefinition
          
          Define.Program *Program = MakeProgramAndReturnPointer( *Code, DefinitionIndex )
                    
      EndSelect
      
      ; Collect annotations.
      Define.i AnnotationIndex = *Definition\FirstAnnotation
      While AnnotationIndex <> -1
        Define.Annotation *Annotation = *Code\Annotations( AnnotationIndex )
        Select *Annotation\AnnotationKind
            
          Case #ProductAnnotation
            
            Define.Settings *Settings = GetOrMakeSettingsForInnerScopeAndReturnPointer( *Code, DefinitionIndex )
            *Settings\ProductName = *Annotation\AnnotationText
            
          Case #CompanyAnnotation
            
            Define.Settings *Settings = GetOrMakeSettingsForInnerScopeAndReturnPointer( *Code, DefinitionIndex )
            *Settings\CompanyName = *Annotation\AnnotationText
            
          ;;;;TODO: allow naming convention to be specific by example (e.g. "LikeThis" or "likeThis")
          Case #NamingConventionAnnotation
            
            Define.Settings *Settings = GetOrMakeSettingsForInnerScopeAndReturnPointer( *Code, DefinitionIndex )
            Select LCase( *Annotation\AnnotationText )
              Case "gnu"
                *Settings\NamingConvention = #GnuCase
                
              Case "pascal"
                *Settings\NamingConvention = #PascalCase
                
              Case "java"
                *Settings\NamingConvention = #JavaCase
                
              Default
                ;;;;TODO: Diagnose
                Debug "Unknown naming convention: " + *Annotation\AnnotationText
            EndSelect            
            
                
          Case #IndentationAnnotation
            
            Define.Settings *Settings = GetOrMakeSettingsForInnerScopeAndReturnPointer( *Code, DefinitionIndex )
            Dim Arguments.s( 0 )
            SplitString( Arguments(), *Annotation\AnnotationText, "," )
            If ArraySize( Arguments() ) >= 1
              *Settings\TabWidth = Val( Trim( Arguments( 0 ) ) )
            EndIf
            ;;;;TODO: handle case where there are less than 2 arguments
            If ArraySize( Arguments() ) >= 2
              Select LCase( Trim( Arguments( 1 ) ) )
                Case "spaces"
                  *Settings\UseSoftTabs = #True
                Case "tabs"
                  *Settings\UseSoftTabs = #False
                Default
                  Debug "Unknown indentation setting: " + Arguments( 1 )
              EndSelect
            EndIf
            
          Case #BuildOptionAnnotation
            
            Dim Arguments.s( 0 )
            SplitString( Arguments(), *Annotation\AnnotationText, "," )
            If ArraySize( Arguments() )
              Define.i Index
              For Index = 0 To ArraySize( Arguments() ) -1
                Select LCase( Arguments( Index ) )
                  Case "writecode"
                    *Settings\BuildOptWriteCode = #True
                EndSelect
              Next
            EndIf
                
        EndSelect
        AnnotationIndex = *Annotation\NextAnnotation
      Wend
      
    Next
    ;}
    
  EndProcedure
  
  ; -----------------------------------------------------------------------------
  
  ;..... this can't be right... lookup needs to be contextual
  Procedure.i LookupNamedType( *Code.Code, NameIndex.i )
    
    Define.s Name = *Code\Identifiers( NameIndex )
    ProcedureReturn LookupNamedTypeFromString( *Code, Name )
    
  EndProcedure
  
  ; -----------------------------------------------------------------------------
  ;;;;TODO: catch cycles!
  ; Determine the type of the given type expression.
  ; May lead to other type expressions being assign types as a side effect.
  ; Will generate new types as needed (for every new unique type combination found).
  ; Returns the type ID assigned to the expression.
  
  Procedure.i AssignTypeIdIfTypeExpression( *Code.Code, ExpressionIndex.i )
    
    Define.Expression *Expression = @*Code\Expressions( ExpressionIndex )
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
        
        Define.i LeftTypeId = AssignTypeIdIfTypeExpression( *Code, *Expression\FirstOperandI )
        Define.i RightTypeId = AssignTypeIdIfTypeExpression( *Code, *Expression\SecondOperandI )
        
        Define.GenType *CombinedType = MakeCombinedGenType( *Code, CombinedTypeKind, LeftTypeId, RightTypeId )
        TypeId = *CombinedType\Id
        
    EndSelect
    
    *Expression\Type = TypeId  
    ProcedureReturn TypeId
    
  EndProcedure
  
  ; -----------------------------------------------------------------------------
  ; Finalizes the set of type IDs in the program and establishes
  ; their subtype relationships.
  
  Procedure GenenerateTypes( *Code.Code )
    
    Define.IRProgram *IRProgram = *Emitter\IRProgram
    
    ; First, assign a type to every type expression in the program.
    Define ExpressionIndex.i
    For ExpressionIndex = 0 To *Code\ExpressionCount - 1
      AssignTypeIdIfTypeExpression( *IRProgram, *Code, ExpressionIndex )
    Next
    
    ; Next, assign a type to every method in the program. This may add
    ; additional types to the program.
    ForEach *IRProgram\FunctionTable\Functions()
      
      Define.GenFunction *GenFunction = *IRProgram\FunctionTable\Functions()
      
      Define.i MethodIndex
      For MethodIndex = 0 To *GenFunction\MethodCount - 1
        
        Define.GenMethod *GenMethod = @*GenFunction\Methods( MethodIndex )
        
        ; Default types for both argument and result is 'Nothing'.
        Define.i ResultTypeId = *IRProgram\TypeTable\NothingTypeId
        Define.i ArgumentTypeId = *IRProgram\TypeTable\NothingTypeId
        
        Define.i DefinitionIndex = *GenMethod\DefinitionIndex
        Define.Definition *Definition = @*Code\Definitions( DefinitionIndex )
        
        ; Result type.
        Define.i ResultTypeExpressionIndex = *Definition\TypeExpression
        If ResultTypeExpressionIndex <> -1
          ResultTypeId = *Code\Expressions( ResultTypeExpressionIndex )\Type
        EndIf
        
        ; Argument type.
        Define FirstValueParameterIndex = *Definition\FirstValueParameter
        If FirstValueParameterIndex <> -1
          ArgumentTypeId = GenTypeForParameters( *IRProgram, *Code, FirstValueParameterIndex )
        EndIf
        
        *GenMethod\ArgumentTypeId = ArgumentTypeId
        *GenMethod\ResultTypeId = ResultTypeId
        
      Next
      
    Next
    
    ; Finally, add type instances to the output program and at the same time build the subtype matrix.
    Define.i TypeCount = IRProgram\TypeTable\TypeCount
    ReDim Program\Types( TypeCount )
    Program\TypeCount = TypeCount
    IRProgram\TypeTable\SubtypeMatrix = AllocateMemory( ( TypeCount * TypeCount + 3 ) / 4 )
    Define.i ObjectTypeId = IRProgram\TypeTable\ObjectTypeId
    Define.i TypeIndex
    For TypeIndex = 0 To TypeCount - 1
      
      ;;;;REVIEW: only put certain types in the final program? (like e.g. ones that are actually instantiated)
      ; Add to program.
      Define.GenType *GenType = @IRProgram\TypeTable\Types( TypeIndex )
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
  
  ; ------------------------------------------------------------------------
  
  Procedure Analyze( *Code.Code )
    
    CollectDefinitions( *Code )
    
    GenerateTypes( *Code )
    GenerateFields( *Code )
    GenerateFunctions( *Code )
    
  EndProcedure
  
EndModule

;{ TESTS

; -----------------------------------------------------------------------------

Macro TestFixture( SourceCode )
  UseModule Code
  UseModule Analyze
  UseModule Text
  Define.Text Text
  Define.Code Code
  MakeText( @Text, SourceCode )
  Parse( @Text, @Code )
  Assert( Code\DiagnosticCount = 0 )
  Analyze( @Code )
EndMacro

; -----------------------------------------------------------------------------

ProcedureUnit CanGenerateDispatchTree()

  TestFixture( ~"type Object;\n" +
               ~"type String : Object;\n" +
               ~"type Boolean = True | False;\n" +
               ~"object True;\n" +
               ~"object False;\n" +
               ~"method ToString( Object ) : String return 1; end;\n" +
               ~"method ToString( Boolean ) : String return 2; end;\n" +
               ~"method ToString( True ) : String return 3; end;\n" )
  
  Assert( MapSize( Code\FunctionTable\Functions() ) = 1 )
  Assert( FindMapElement( Code\FunctionTable\Functions(), "to_string" ) <> #Null )
  
  Define.Function *Function = Code\FunctionTable\Functions()
  
  Assert( *Function\MethodCount = 3 )
  Assert( *Function\DispatchTree\NodeCount = 3 )
  Assert( *Function\DispatchTree\Nodes( 0 )\ArgumentTypeId = GenProgram\TypeTable\ObjectTypeId )
  Assert( *Function\DispatchTree\Nodes( 0 )\ResultTypeId = GenProgram\TypeTable\NamedTypes( "string" ) )
  Assert( *Function\DispatchTree\Nodes( 0 )\Method = @Code\FunctionTable\Functions( "to_string" )\Methods( 0 ) )
  Assert( *Function\DispatchTree\Nodes( 0 )\Parent = -1 )
  Assert( *Function\DispatchTree\Nodes( 1 )\ArgumentTypeId = GenProgram\TypeTable\NamedTypes( "boolean" ) )
  Assert( *Function\DispatchTree\Nodes( 1 )\ResultTypeId = GenProgram\TypeTable\NamedTypes( "string" ) )
  Assert( *Function\DispatchTree\Nodes( 1 )\Parent = 0 )
  Assert( *Function\DispatchTree\Nodes( 1 )\Method = @Code\FunctionTable\Functions( "to_string" )\Methods( 1 ) )
  Assert( *Function\DispatchTree\Nodes( 2 )\ArgumentTypeId = GenProgram\TypeTable\NamedTypes( "true" ) )
  Assert( *Function\DispatchTree\Nodes( 2 )\ResultTypeId = GenProgram\TypeTable\NamedTypes( "string" ) )
  Assert( *Function\DispatchTree\Nodes( 2 )\Method = @Code\FunctionTable\Functions( "to_string" )\Methods( 2 ) )
  Assert( *Function\DispatchTree\Nodes( 2 )\Parent = 1 )
  
EndProcedureUnit

UndefineMacro TestFixture

;}

; IDE Options = PureBasic 5.73 LTS (Windows - x64)
; CursorPosition = 42
; FirstLine = 25
; Folding = --
; EnableXP