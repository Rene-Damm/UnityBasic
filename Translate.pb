; Creates IR from Code.

XIncludeFile "Code.pb"
XIncludeFile "Program.pb"

DeclareModule Translate
  
  UseModule Code
  UseModule Program
  
  Declare   Translate( *Code.Code, *Program.Program, WithLibraries.b = #True )
  
EndDeclareModule

Module Translate
  
  ; ---------------------------------------------------------------------------
  
  Structure GenBlock
    FirstInsn.l
  EndStructure
  
  ; ---------------------------------------------------------------------------
  
  Structure GenEmitter
    *Program.Program
    *IRProgram.IRProgram
    BlockStackDepth.l
    Array BlockStack.GenBlock( 0 )
  EndStructure
  
  ; ---------------------------------------------------------------------------
  
  Procedure.i EmitInsn( *Emitter.GenEmitter, Opcode.b )
    
    Define *Program = *Emitter\Program
    Define.i InstructionIndex = *Program\InstructionCount
    If ArraySize( *Program\Instructions() ) = InstructionIndex
      ReDim *Program\Instructions( InstructionIndex + 8192 )
    EndIf
    *Program\InstructionCount + 1
    
    Define.Instruction *Insn = @*Program\Instructions( InstructionIndex )
    *Insn\Opcode = Opcode
    
    ProcedureReturn InstructionIndex
    
  EndProcedure
  
  ; -----------------------------------------------------------------------------
  
  Procedure.i EmitBeginBlock( *Emitter.GenEmitter )
    
    Define.i BlockInsn = EmitInsn( *Emitter, #BlockInsn )
                                   
    Define.i BlockIndex = *Emitter\BlockStackDepth
    If ArraySize( *Emitter\BlockStack() ) = BlockIndex
      ReDim *Emitter\BlockStack( BlockIndex + 32 )
    EndIf
    *Emitter\BlockStackDepth + 1
    
    Define.GenBlock *Block = @*Emitter\BlockStack( BlockIndex )
    *Block\FirstInsn = BlockInsn
    
    ProcedureReturn BlockInsn
  
  EndProcedure
  
  ; -----------------------------------------------------------------------------
  
  Procedure EmitEndBlock( *Emitter.GenEmitter )
    
    If *Emitter\BlockStackDepth <= 0
      Debug "Block stack underflow!"
      ProcedureReturn
    EndIf
    
    Define.i BlockIndex = *Emitter\BlockStackDepth
    Define.i FirstInsn = *Emitter\BlockStack( BlockIndex )\FirstInsn
    Define.Instruction *BlockInsn = @*Emitter\Program\Instructions( FirstInsn )
    Define.i InsnCount = *Emitter\Program\InstructionCount - FirstInsn
    *BlockInsn\LongOperand = InsnCount
    
    *Emitter\BlockStackDepth - 1
    
  EndProcedure
  
  ; -----------------------------------------------------------------------------
  
  Macro GenTypePtr( *TypeTable.IRTypeTable, TypeId )
    @*TypeTable\Types( TypeId - 1 )
  EndMacro
  
  ; -----------------------------------------------------------------------------
  
  #SUBTYPE_NOT_INITIALIZED = 0
  #SUBTYPE_FALSE = 1
  #SUBTYPE_TRUE = 2
  
  Macro SetupSubtypeMatrixIndex( *TypeTable.IRTypeTable, FirstTypeId, SecondTypeId )
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
    Define.i SubtypeMatrixIndex = FirstTypeId#Index * *TypeTable\TypeCount + SecondTypeId#Index
    ; Two bits per entry.
    Define.i SubtypeMatrixByteOffset = SubtypeMatrixIndex / 4
    Define.i SubtypeMatrixBitOffset = ( SubtypeMatrixIndex % 4 ) * 2
    Define.i SubtypeMatrixMask = 3 << SubtypeMatrixBitOffset
    Define *SubtypeMatrixPtr = *TypeTable\SubtypeMatrix + SubtypeMatrixByteOffset
    CompilerIf #PB_Compiler_Debugger
      If SubtypeMatrixIndex < 0
        DebuggerError( "Negative subtype matrix index!" )
      EndIf
      If SubtypeMatrixByteOffset >= MemorySize( *TypeTable\SubtypeMatrix )
        DebuggerError( "Subtype byte offset out of range!" )
      EndIf
    CompilerEndIf
  EndMacro
  
  ; -----------------------------------------------------------------------------
  
  Declare.b ComputeIsFirstSubtypeOfSecond( *TypeTable.IRTypeTable, FirstTypeId.i, SecondTypeId.i )
  
  Procedure.b FirstIsSubtypeOfSecond( *TypeTable.IRTypeTable, FirstTypeId.i, SecondTypeId.i )
    
    CompilerIf #PB_Compiler_Debugger
      If FirstTypeId <= 0
        DebuggerError( "First type ID is invalid!" )
      EndIf
      If SecondTypeId <= 0
        DebuggerError( "Second type ID is invalid!" )
      EndIf
    CompilerEndIf
    
    SetupSubtypeMatrixIndex( *TypeTable, FirstTypeId, SecondTypeId )
    
    ; In order to avoid having to compute all NxN relationships up front, we make the process
    ; lazy and only compute that part of the matrix that we actually need to understand the program.
    
    Define.b Value = ( PeekB( *SubtypeMatrixPtr ) & SubtypeMatrixMask ) >> SubtypeMatrixBitOffset
    Select Value
        
      Case #SUBTYPE_NOT_INITIALIZED
        If ComputeIsFirstSubtypeOfSecond( *TypeTable, FirstTypeId, SecondTypeId )
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
  
  ; -----------------------------------------------------------------------------
  
  Procedure.b SetFirstIsSubtypeOfSecond( *TypeTable.IRTypeTable, FirstTypeId.i, SecondTypeId.i )
    SetupSubtypeMatrixIndex( *TypeTable, FirstTypeId, SecondTypeId )
    PokeB( *SubtypeMatrixPtr, PeekB( *SubtypeMatrixPtr ) | ( #SUBTYPE_TRUE << SubtypeMatrixBitOffset ) )
  EndProcedure
  
  ; -----------------------------------------------------------------------------
  
  ; Determine if 'FirstTypeId <: SecondTypeId' for any type relationship other than identity (FirstTypeId = SecondTypeId)
  ; and direct derivation (FirstTypeId : SecondTypeId). The latter two relationships we initialize directly in GenTypes().
  Procedure.b ComputeIsFirstSubtypeOfSecond( *TypeTable.IRTypeTable, FirstTypeId.i, SecondTypeId.i )
    
    ; Everything is a subtype of Object.
    If SecondTypeId = *TypeTable\ObjectTypeId
      ProcedureReturn #True
    EndIf
    
    Define.GenType *FirstGenType = GenTypePtr( *TypeTable, FirstTypeId )
    Define.GenType *SecondGenType = GenTypePtr( *TypeTable, SecondTypeId )
    
    Select *SecondGenType\TypeKind
        
      Case #IntersectionType
        ProcedureReturn Bool( FirstIsSubtypeOfSecond( *TypeTable, FirstTypeId, *SecondGenType\LeftOperandTypeId ) Or FirstIsSubtypeOfSecond( *TypeTable, FirstTypeId, *SecondGenType\RightOperandTypeId ) )
        
      Case #UnionType
        ProcedureReturn Bool( FirstIsSubtypeOfSecond( *TypeTable, FirstTypeId, *SecondGenType\LeftOperandTypeId ) And FirstIsSubtypeOfSecond( *TypeTable, FirstTypeId, *SecondGenType\RightOperandTypeId ) )
        
      Case #FunctionType
        ; We only test for relationships between function types here. Other type relationships
        ; from function types to other kinds of types can be established explicitly in code.
        If *FirstGenType\TypeKind = #FunctionType
          ; Contravariant argument, covariant result.
          ProcedureReturn Bool( FirstIsSubtypeOfSecond( *TypeTable, *SecondGenType\LeftOperandTypeId, *FirstGenType\LeftOperandTypeId ) And FirstIsSubtypeOfSecond( *TypeTable, *FirstGenType\RightOperandTypeId, *SecondGenType\RightOperandTypeId ) )
        EndIf
        
      Case #TupleType
        
        If *FirstGenType\TypeKind = #TupleType
          ProcedureReturn Bool( FirstIsSubtypeOfSecond( *TypeTable, *FirstGenType\LeftOperandTypeId, *SecondGenType\LeftOperandTypeId ) And FirstIsSubtypeOfSecond( *TypeTable, *FirstGenType\RightOperandTypeId, *SecondGenType\RightOperandTypeId ) )
        EndIf
        
    EndSelect
    
    Select *FirstGenType\TypeKind
        
      Case #NamedType
        If FirstTypeId <> IRProgram\TypeTable\ObjectTypeId
          ProcedureReturn FirstIsSubtypeOfSecond( *TypeTable, *FirstGenType\LeftOperandTypeId, SecondTypeId )
        EndIf
        
    EndSelect
  
    ProcedureReturn #False
    
  EndProcedure
  
  ; -----------------------------------------------------------------------------
  
  Procedure.i NewGenScope( *IRProgram.IRProgram, ParentScopeIndex.i )
    
    Define.i ScopeIndex = *IRProgram\SymbolTable\ScopeCount
    If ArraySize( *IRProgram\SymbolTable\Scopes() ) = ScopeIndex
      ReDim *IRProgram\SymbolTable\Scopes( ScopeIndex + 512 )
    EndIf
    *IRProgram\SymbolTable\ScopeCount + 1
    
    Define.GenScope *GenScope = @*IRProgram\SymbolTable\Scopes( ScopeIndex )
    *GenScope\ParentScope = ParentScopeIndex
    
    Define.i HashTableIndex
    For HashTableIndex = 0 To #GENSCOPE_HASHTABLE_SIZE - 1
      *GenScope\SymbolTable[ HashTableIndex ] = -1
    Next
      
    ProcedureReturn ScopeIndex
    
  EndProcedure
  
  ; -----------------------------------------------------------------------------
  
  Procedure.i NewGenSymbol( *IRProgram.IRProgram, ScopeIndex.i, IdentifierIndex.i )
    
    Define.i SymbolIndex = *IRProgram\SymbolTable\SymbolCount
    If ArraySize( *IRProgram\SymbolTable\Symbols() ) = SymbolIndex
      ReDim *IRProgram\SymbolTable\Symbols( SymbolIndex + 2048 )
    EndIf
    *IRProgram\SymbolTable\SymbolCount + 1
    
    Define.GenSymbol *Symbol = @*IRProgram\SymbolTable\Symbols( SymbolIndex )
    *Symbol\IdentifierIndex = IdentifierIndex
    *Symbol\ScopeIndex = ScopeIndex
      
    Define.GenScope *Scope = @*IRProgram\SymbolTable\Scopes( ScopeIndex )
    
    Define.i HashTableIndex = IdentifierIndex % #GENSCOPE_HASHTABLE_SIZE
    
    *Symbol\NextSymbol = *Scope\SymbolTable[ HashTableIndex ]
    *Scope\SymbolTable[ HashTableIndex ] = SymbolIndex
    
    ProcedureReturn SymbolIndex
    
  EndProcedure
  
  ; -----------------------------------------------------------------------------
  
  Procedure.i LookupSymbol( *IRProgram.IRProgram, ScopeIndex.i, IdentifierIndex.i )
    
    Define.i HashTableIndex = IdentifierIndex % #GENSCOPE_HASHTABLE_SIZE
    
    Define.i CurrentScope = ScopeIndex
    While CurrentScope <> -1
      
      Define.i CurrentSymbol = *IRProgram\SymbolTable\Scopes( CurrentScope )\SymbolTable[ HashTableIndex ]
      While CurrentSymbol <> -1
        
        Define.GenSymbol *Symbol = @*IRProgram\SymbolTable\Symbols( CurrentSymbol )
        If *Symbol\IdentifierIndex = IdentifierIndex
          ProcedureReturn CurrentSymbol
        EndIf
        
        CurrentSymbol = *Symbol\NextSymbol
        
      Wend
      
      CurrentScope = *IRProgram\SymbolTable\Scopes( CurrentScope )\ParentScope
      
    Wend
    
    ;;;;TODO: create a separate lookup path for global symbols so that we don't have to put them into a GenScope
    
    ProcedureReturn -1
    
  EndProcedure
  
  ; -----------------------------------------------------------------------------
  
  ; Assinging types is recursive.
  Declare.i AssignTypeIdIfTypeExpression( *IRProgram.IRProgram, *Code.Code, ExpressionIndex.i )
  
  ;....... problem... this already depends on symbol table structures
  Procedure.i LookupNamedTypeFromString( *IRProgram.IRProgram, *Code.Code, Name.s )
    
    Define.i TypeId = #INVALID_TYPE_ID
    
    If Not FindMapElement( *IRProgram\TypeTable\NamedTypes(), Name )
      ;;;;TODO: Diagnostic
      Debug "Cannot find type " + Name
    Else
      TypeId = *IRProgram\TypeTable\NamedTypes()
      If TypeId = #INVALID_TYPE_ID
        
        ; It's a type alias.
        If Not FindMapElement( *IRProgram\TypeTable\AliasToTypeExpression(), Name )
          ; This should not happend.
          Debug "Cannot find aliased type expression for " + Name
        Else
          Define.i AliasedTypeExpression = *IRProgram\TypeTable\AliasToTypeExpression()
          TypeId = AssignTypeIdIfTypeExpression( *IRProgram, *Code, AliasedTypeExpression )
          *IRProgram\TypeTable\NamedTypes( Name ) = TypeId
        EndIf
        
      EndIf
    EndIf
    
    ProcedureReturn TypeId
    
  EndProcedure
  
  ; -----------------------------------------------------------------------------
  
  Declare.i FindDispatchTreeNode( *DispatchTree.GenDispatchTree, ArgumentTypeId.i, StartingNode.i = 0 )
  
  Procedure.i AssignTypeIdIfValueExpression( *IRProgram.IRProgram, *Code.Code, ExpressionIndex.i )
    
    Define.Expression *Expression = @*Code\Expressions( ExpressionIndex )
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
          TypeId = *IRProgram\TypeTable\IntegerTypeId
          
        Case #FloatLiteralType
          TypeId = *IRProgram\TypeTable\FloatTypeId
          
        Case #StringLiteralType
          TypeId = *IRProgram\TypeTable\ImmutableStringTypeId
          
        Case #NothingLiteralType
          TypeId = *IRProgram\TypeTable\NothingTypeId
          
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
        
        Define.GenCallSite *CallSite = @*IRProgram\FunctionTable\CallSites( ExpressionIndex )
      
        ; Look up function.
        Define.Expression *LeftHandExpr = @*Code\Expressions( *Expression\FirstOperandI )
        Select *LeftHandExpr\Operator
            
          Case #NameExpression
            
            Define.s Name = *Code\Identifiers( *LeftHandExpr\FirstOperandI )
            Define.GenFunction *CalledFunction = FindMapElement( *IRProgram\FunctionTable\Functions(), Name )
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
        Define.i ArgumentTypeId = AssignTypeIdIfValueExpression( *IRProgram, *Code, *Expression\SecondOperandI )
      
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
  
  
  ; -----------------------------------------------------------------------------
  
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
  
  ; -----------------------------------------------------------------------------
  
  Procedure.s GenTypeNameById( TypeId.i )
    ProcedureReturn GenTypeName( GenTypePtr( TypeId ) )
  EndProcedure
  
  ; -----------------------------------------------------------------------------
  
  Procedure.i GenTypeForParameters( *IRProgram.IRProgram, *Code.Code, ParameterIndex.i )
    
    Define.Parameter *Parameter = @*Code\Parameters( ParameterIndex )
    
    Define.i TypeId = #INVALID_TYPE_ID
    
    ; If there's an explicit type expression on the parameter, that's our type.
    If *Parameter\TypeExpression <> -1
      TypeId = *Code\Expressions( *Parameter\TypeExpression )\Type
    Else
      ; Otherwise, type has to match name of parameter.
      TypeId = LookupNamedType( *Parameter\Name )
    EndIf
    
    ; If there's more parameters following, create a tuple type.
    If *Parameter\NextParameter <> -1
      Define.i RightTypeId = GenTypeForParameters( *IRProgram, *Code, *Parameter\NextParameter )
      Define.GenType *TupleType = MakeCombinedGenType( *IRProgram, #TupleType, TypeId, RightTypeId )
      TypeId = *TupleType\Id
    EndIf
    
    ProcedureReturn TypeId
    
  EndProcedure
  
  ; -----------------------------------------------------------------------------
  
  Procedure GenFields( *Emitter.GenEmitter )
  EndProcedure
  
  ; -----------------------------------------------------------------------------
  
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
  
  ; -----------------------------------------------------------------------------
  
  Procedure.i FindDispatchTreeNode( *DispatchTree.GenDispatchTree, *TypeTable.IRTypeTable, ArgumentTypeId.i, StartingNode.i = 0 )
    
    Define.i NodeIndex = StartingNode
    
    If Not FirstIsSubtypeOfSecond( *TypeTable, ArgumentTypeId, *DispatchTree\Nodes( NodeIndex )\ArgumentTypeId )
      ; Entire dispatch subtree doesn't apply to given argument type.
      ProcedureReturn #INVALID_NODE_INDEX
    EndIf
    
    While #True
      
      ; See if any of the children are more specialized.
      Define.i ChildIndex = *DispatchTree\Nodes( NodeIndex )\FirstChild
      Define.b FoundChild = #False
      While ChildIndex <> #INVALID_NODE_INDEX
        
        If FirstIsSubtypeOfSecond( *TypeTable, ArgumentTypeId, *DispatchTree\Nodes( ChildIndex )\ArgumentTypeId )
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
  
  ; -----------------------------------------------------------------------------
  ; A dispatch tree puts all methods contained in a given function into a total ordering.
  
  Procedure GenDispatchTree( *IRProgram.IRProgram, *GenFunction.GenFunction )
    
    Define.GenDispatchTree *DispatchTree = @*GenFunction\DispatchTree
    Define.IRTypeTable *TypeTable = @*IRProgram\TypeTable
    
    ; Add root node representing Object->Object.
    AddDispatchTreeNode( *DispatchTree, #INVALID_NODE_INDEX, *TypeTable\ObjectTypeId, *Emitter\IRProgram\TypeTable\ObjectTypeId )
    
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
          
          If FirstIsSubtypeOfSecond( *TypeTable, *GenMethod\ArgumentTypeId, *Node\ArgumentTypeId )
            
            Define.b HaveFoundNode = #False
            
            ; If we have children, can be that we simply have to descend into them. However,
            ; can also be that we have to insert us as an intermediate node in-between the
            ; current parent and one or more of the children.
            
            Define.i ChildIndex = *Node\FirstChild
            While ChildIndex <> #INVALID_NODE_INDEX
              
              Define.GenDispatchTreeNode *Child = @*DispatchTree\Nodes( ChildIndex )
              
              If FirstIsSubtypeOfSecond( *TypeTable, *GenMethod\ArgumentTypeId, *Child\ArgumentTypeId )
                
                ; Descend into child node.
                ; NOTE: This branch also takes care of looking at two identical types.
                HaveFoundNode = #True
                CurrentNodeIndex = ChildIndex
                Break
                
              ElseIf FirstIsSubtypeOfSecond( *TypeTable, *Child\ArgumentTypeId, *GenMethod\ArgumentTypeId )
                
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
                  
                  If FirstIsSubtypeOfSecond( *TypeTable, *Child\ArgumentTypeId, *GenMethod\ArgumentTypeId )
                    
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
  
  ; -----------------------------------------------------------------------------
  
  Procedure GenMethod( *Emitter.GenEmitter, *Function.GenFunction, *Node.GenDispatchTreeNode )
    
    ;;;;TODO: before, after, around methods
    
    ; Emit children first.
    Define.i ChildIndex = *Node\FirstChild
    While ChildIndex <> -1
      
      Define.GenDispatchTreeNode *Child = @*Function\DispatchTree\Nodes( ChildIndex )
      
      GenMethod( *Emitter, *Function, *Child )
      
      ChildIndex = *Child\NextSibling
      
    Wend
    
    ;how does resend work??
    
    ; If the node has an associated method, emit code for it. 
    If *Node\Method <> #Null
    EndIf
    
  EndProcedure
  
  ; -----------------------------------------------------------------------------
  
  ; Generates code for each function and as a side-effect, typechecks each
  ; value expression used in methods.
  Procedure GenFunctions( *Emitter.GenEmitter, *Code.Code )
    
    Define.IRProgram *IRProgram = *Emitter\IRProgram
    Define.Program *Program = *Emitter\Program
    
    ;{ Generate dispatch trees for all functions.
    
    ForEach *IRProgram\FunctionTable\Functions()
      
      Define.GenFunction *GenFunction = *IRProgram\FunctionTable\Functions()
      GenDispatchTree( *Emitter, *GenFunction )
      
    Next
    
    ;}
    
    ;;;;TODO: generate specializations
    
    ;{ Generate instructions for all functions.
    
    Define.i FunctionCount = MapSize( *IRProgram\FunctionTable\Functions() )
    ReDim *Program\Functions( FunctionCount )
    *Program\FunctionCount = FunctionCount
    
    ReDim *IRProgram\FunctionTable\CallSites( Code\ExpressionCount )
    FillMemory( *IRProgram\FunctionTable\CallSites( 0 ), ArraySize( IRProgram\FunctionTable\CallSites() ) * SizeOf( GenCallSite ), 0, #PB_Long )
    
    Define.i FunctionIndex = 0
    ForEach *IRProgram\FunctionTable\Functions()
      
      ;;;;TODO: enter value arguments into symbol table... somewhere...
      
      Define.GenFunction *GenFunction = *IRProgram\FunctionTable\Functions()
      Define.Function *Function = *Program\Functions( FunctionIndex )
      
      *Function\Name = *GenFunction\Name
      
      ; We don't really need a BlockInsn for the function as a whole. Just keep track
      ; of the instruction index and count.
      *Function\FirstInstruction = Program\InstructionCount
      
      ; Emit instructions blocks.
      GenMethod( *Emitter, *GenFunction, @*GenFunction\DispatchTree\Nodes( 0 ) )
      
      *Function\InstructionCount = Program\InstructionCount - *Function\FirstInstruction
      FunctionIndex + 1    
      
    Next
    
    ;}
    
  EndProcedure
  
  ; -----------------------------------------------------------------------------
  
  Procedure TranslateObjects( *Emitter.GenEmitter, *Code.Code )
  EndProcedure
  
  ; -----------------------------------------------------------------------------
  
  Procedure TranslateAssets( *Emitter.GenEmitter, *Code.Code )
  EndProcedure
  
  ; -----------------------------------------------------------------------------
  
  Procedure TranslateProgram( *Emitter.GenEmitter, *Code.Code )
    
    Program\Name = IRProgram\Name
    Program\Company = IRProgram\Company
    Program\Product = IRProgram\Product
    
  EndProcedure
  
  ; -----------------------------------------------------------------------------
  
  Procedure Translate( *Code.Code, *Program.Program, WithLibraries.b = #True )
    
    Define.IRProgram IRProgram
    Define.GenEmitter Emitter
    Emitter\Program = *Program
    Emitter\IRProgram = @IRProgram
        
    ;;;;FIXME: somehow the custom product and company strings we relate aren't coming through in the player builds...
    Program\Product = "DefaultProduct"
    Program\Company = "DefaultCompany"
    
    GenCollect( @Emitter, *Code )
    
    If WithLibraries
      If IRProgram\TypeTable\ObjectTypeId = #INVALID_TYPE_ID
        Debug "Object type missing"
        ProcedureReturn
      EndIf
      If IRProgram\TypeTable\IntegerTypeId = #INVALID_TYPE_ID
        Debug "Integer type missing"
        ProcedureReturn
      EndIf
      If IRProgram\TypeTable\FloatTypeId = #INVALID_TYPE_ID
        Debug "Float type missing"
        ProcedureReturn
      EndIf
      If IRProgram\TypeTable\ImmutableStringTypeId = #INVALID_TYPE_ID
        Debug "ImmutableString type missing"
        ProcedureReturn
      EndIf
      If IRProgram\TypeTable\NothingTypeId = #INVALID_TYPE_ID
        Debug "Nothing type missing"
        ProcedureReturn
      EndIf
      If IRProgram\TypeTable\TrueTypeId = #INVALID_TYPE_ID
        Debug "True type missing"
        ProcedureReturn
      EndIf
      If IRProgram\TypeTable\FalseTypeId = #INVALID_TYPE_ID
        Debug "False type missing"
        ProcedureReturn
      EndIf
      If IRProgram\TypeTable\ComponentTypeId = #INVALID_TYPE_ID
        Debug "Component type missing"
        ProcedureReturn
      EndIf
      If IRProgram\TypeTable\EntityTypeId = #INVALID_TYPE_ID
        Debug "Entity type missing"
        ProcedureReturn
      EndIf
      If IRProgram\TypeTable\SystemTypeId = #INVALID_TYPE_ID
        Debug "System type missing"
        ProcedureReturn
      EndIf
    EndIf
    
    TranslateTypes( @Emitter, *Code )
    TranslateFields( @Emitter, *Code )
    TranslateFunctions( @Emitter, *Code )
    TranslateObjects( @Emitter, *Code )
    TranslateAssets( @Emitter, *Code )
    TranslateProgram( @Emitter, *Code )
    
    Debug "Functions: " + Str( MapSize( IRProgram\FunctionTable\Functions() ) ) +
          ", Types: " + Str( IRProgram\TypeTable\TypeCount ) +
          ", Values: " + Str( IRProgram\ValueTable\ValueCount ) +
          ", Scopes: " + Str( IRProgram\SymbolTable\ScopeCount ) +
          ", Symbols: " + Str( IRProgram\SymbolTable\SymbolCount )
    
  EndProcedure

EndModule

; IDE Options = PureBasic 5.73 LTS (Windows - x64)
; CursorPosition = 336
; FirstLine = 324
; Folding = ------
; EnableXP