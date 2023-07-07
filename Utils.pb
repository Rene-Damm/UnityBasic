DeclareModule Utils
  
  Macro DoubleQuote
    "
  EndMacro
  
  Macro Stringify( Text )
    DoubleQuote#Text#DoubleQuote
  EndMacro
  
  ;;stringify
  
  Structure StringBuilder
    *Buffer
    Capacity.i
    Length.i
  EndStructure
  
  Declare.i AppendString( *Builder.StringBuilder, String.s )
  Declare   FreeStringBuilder( *Builder.StringBuilder )
  Declare   ResetStringBuilder( *Builder.StringBuilder )
  Declare.s GetString( *Builder.StringBuilder, FreeBuilder.b = #True )
  
  Declare.i SplitString( Array Split.s( 1 ), String.s, Delimiter.s )
  
EndDeclareModule

Module Utils
  
  ; ---------------------------------------------------------------------------
  
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

  ; ---------------------------------------------------------------------------
  
  Procedure FreeStringBuilder( *Builder.StringBuilder )
    If *Builder\Buffer <> #Null
      FreeMemory( *Builder\Buffer )
    EndIf
    *Builder\Buffer = #Null
    *Builder\Length = 0
    *Builder\Capacity = 0
  EndProcedure

  ; ---------------------------------------------------------------------------
  
  Procedure ResetStringBuilder( *Builder.StringBuilder )
    *Builder\Length = 0
  EndProcedure
  
  ; ---------------------------------------------------------------------------
  
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
  
  ; ---------------------------------------------------------------------------
  
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
  
EndModule

; IDE Options = PureBasic 5.73 LTS (Windows - x64)
; CursorPosition = 9
; Folding = --
; EnableXP