; Compiled code.
; Uses a simple machine model that is meant to be portable across a wide variety of runtimes.
; Essentially just a bunch of structs and functions.
; Can leverage host-specific functionality in three ways:
;  (1) Through instrinsic code (ICalls)
;  (2) Through assets deployed to the host (provide data to programs)
;  (3)

DeclareModule Machine
  
  Structure MachineAsset
  EndStructure
  
  ;should this just be debug info?
  Structure MachineDataField
    Name.s
    Offset.i
    Type.i
  EndStructure
  
  Structure MachineDataObject
    Type.i
    FirstByte.i ; Memory state does not necessarily correspond to layout runtime uses. Contains no type tags. (really??)
  EndStructure
  
  ; Type codes are type indices offset by the number of built-in types.
  Enumeration MachineBuiltinDataType
    
    ; Integer scalars.
    #Int1
    #Int8
    #Int16
    #Int32Type
    #Int64Type
    
    ; Float scalars.
    #Float32Type
    #Float64Type
    
    ; Integer vectors.
    #Int32x2Type
    #Int32x3Type
    
    ; Float vectors.
    #Float32x2Type
    #Float32x3Type
    #Float64x2Type
    #Float64x3Type
    
    ; Misc.
    #CharType     ; UTF-16 character.
    
  EndEnumeration
  
  EnumerationBinary MachineDataTypeFlags
  EndEnumeration  
  
  Enumeration MachineDataTypeKind
    #TypeIsArray            ; Used for "normal" arrays as well as strings.
    #TypeIsTuple
    #TypeIsComponent ;?? should these be communicated through some kind of annotations instead?
    #TypeIsSharedComponent ;??
    #TypeIsSystem ;??
  EndEnumeration
    
  ; No inheritance/derivation at runtime.
  ; Each type is basically two structs:
  ;   (1) An instance struct (with sizeof() >= 0)
  ;   (2) One static struct (with sizeof() >= 0)
  Structure MachineDataType
    Name.s
    SizeInBytes.l
    Flags.b
    Kind.b
    ;;;;REVIEW: why do we even need field information in the player?
    FirstField.l
    FieldCount.l
  EndStructure
  
  Enumeration MachineInstructionCode
    
    ;;;;how do you call something dynamically?
    ; Invocation insns.
    #LookupInsn = 1 ; Find ...
    #CallInsn       ; Call function. Operand1 is function index. Operand2 is entry node index. Operand3 is argument insn (relative). Value is result value.
    #CCallInsn      ; Call function with closure. Operand1 is function index. Operand2 is entry node index. Operand3 is argument insn (relative). Operand3 is type index of closure.
    #ICallInsn      ; Call intrinsic. Operand1 is Intrinsic enum value. Operand2 is argument. Value is result value.
    #YieldInsn
    #ReturnInsn
    
    ; Memory insns.
    ; Store is the only way of changing state.
    ;;;;TODO:
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
    #TypecaseInsn ; No subtyping. Checks for exact type ID match on given object. Operand1 is value. Operand2 is type ID *or* index of typeset.
    
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
  
  Enumeration MachineIntrinsic
    
    #IntrDebugLog = 1
    #IntrDebugBreak
    
    ;;;;REVIEW: should the types here be implicit?? also the bitwidths?
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
  
  ; Instructions are fixed-width SSA for a very simple, stack-less VM partially inspired by SmallTalk.
  ; Each instruction is 64bits wide.
  ; Constant data (any object) needs to be loaded from the constant store.
  Structure MachineInstruction
    Opcode.b
    Flags.b
    StructureUnion
      ShortOperands.w[ 3 ]
      LongOperand.l
    EndStructureUnion
  EndStructure
  
  ;why not just instruction blocks? is the function and node structure important at runtime?
  
  ; All functions are fully symmetric, i.e. 1 argument value, 1 result value.
  ; Unlike methods, there's no overloading and no dispatching. Basically, a function is a "method group".
  ; Any dispatching (where necessary) happens internally to a function.
  Structure MachineFunction
    Name.s
    ArgumentType.i
    ResultType.i
    FirstInstruction.i
    InstructionCount.i
  EndStructure

  ;;;;REVIEW: transmit instructions and initial state both as one big memory blob?
  
  Structure MachineProgram
    Name.s
    Company.s
    Product.s
    AssetCount.i
    FunctionCount.i
    TypeCount.i
    TypeSetCount.i
    ObjectCount.i
    InstructionCount.i
    InitialMemorySize.i
    Array Assets.Asset( 0 )
    Array Functions.Function( 0 )
    Array Types.Type( 0 )
    Array TypeSets.l( 0 ) ; Each TypeSet is TypeCount bits long (rounded up to next byte boundary).
    Array Fields.Field( 0 )
    Array Objects.Object( 0 )
    Array Instructions.Instruction( 0 )
    Array InitialMemory.b( 0 )
  EndStructure  
  
EndDeclareModule

Module Machine
EndModule

; IDE Options = PureBasic 5.73 LTS (Windows - x64)
; CursorPosition = 13
; Folding = -
; EnableXP