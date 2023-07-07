; Parsing of Text into Code.

;;;;TODO: put this on a thread

XIncludeFile "Text.pb"
XIncludeFile "Code.pb"

DeclareModule Parse
  
  UseModule Text
  UseModule Code
  
  Declare Parse( *Text.Text, *Code.Code )
  
EndDeclareModule

Module Parse
  
  ; ---------------------------------------------------------------------------
  
  Structure ParserLocationState
    *Position
    LastRegion.TextRegion
    CurrentLine.i
    CurrentColumn.i
  EndStructure
  
  ; ---------------------------------------------------------------------------
  
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
  
  ; ---------------------------------------------------------------------------
  
  Macro SaveParserLocation( Parser, LocationVariable )
    Define.ParserLocationState LocationVariable
    LocationVariable\Position = Parser\Position
    LocationVariable\LastRegion = Parser\LastRegion
    LocationVariable\CurrentLine = Parser\CurrentLine
    LocationVariable\CurrentColumn = Parser\CurrentColumn
  EndMacro

  ; ---------------------------------------------------------------------------
  
  Macro RestoreParserLocation( Parser, LocationVariable )
    Parser\Position = LocationVariable\Position
    Parser\LastRegion = LocationVariable\LastRegion
    Parser\CurrentLine = LocationVariable\CurrentLine
    Parser\CurrentColumn = LocationVariable\CurrentColumn
  EndMacro

  ; ---------------------------------------------------------------------------
  
  Macro PushExpressionContext( Parser, Context )
    Define.i PreviousExpressionContext = Parser\CurrentExpressionContext
    Parser\CurrentExpressionContext = Context
  EndMacro

  ; ---------------------------------------------------------------------------
  
  Macro PopExpressionContext( Parser )
    Parser\CurrentExpressionContext = PreviousExpressionContext
  EndMacro
  
  ; ---------------------------------------------------------------------------
  
  Macro PushScope( Parser, Code, Definition )
    
    Define.i PreviousScope = Parser\CurrentScope
    Define.i PreviousDefinitionInScope = Parser\CurrentDefinitionInScope
    Define.i PreviousStatementInScope = Parser\CurrentStatement
    Define.i CurrentScope = MakeScopeAndReturnIndex( Code, PreviousScope, Definition )
    Parser\CurrentScope = CurrentScope
    Parser\CurrentDefinitionInScope = -1
    Parser\CurrentStatement = -1
    
  EndMacro

  ; ---------------------------------------------------------------------------
  
  Macro PopScope( Parser )
    
    Parser\CurrentScope = PreviousScope
    Parser\CurrentDefinitionInScope = PreviousDefinionInScope
    Parser\CurrentStatement = PreviousStatementInScope
    CurrentScope = PreviousScope
    
  EndMacro

  ; ---------------------------------------------------------------------------
  
  Macro MakeExpressionOpI( IndexVariable, Op, Tp, Operand, StartPos )
    
    Define.i IndexVariable = *Code\ExpressionCount
    If ArraySize( *Code\Expressions() ) = IndexVariable
      ReDim *Code\Expressions( IndexVariable + 1024 )
    EndIf
    *Code\ExpressionCount + 1
    
    Define.Expression *Expression = @*Code\Expressions( IndexVariable )
    *Expression\Operator = Op
    *Expression\Context = *Parser\CurrentExpressionContext
    *Expression\Type = Tp
    *Expression\FirstOperandI = Operand
    *Expression\Region\LeftPos = StartPos
    *Expression\Region\RightPos = *Parser\Position - *Parser\StartPosition
    *Expression\NextExpression = -1
    
  EndMacro

  ; ---------------------------------------------------------------------------
  
  Macro MakeExpressionOp2I( IndexVariable, Op, Tp, Operand1, Operand2, StartPos )
    
    Define.i IndexVariable = *Code\ExpressionCount
    If ArraySize( *Code\Expressions() ) = IndexVariable
      ReDim *Code\Expressions( IndexVariable + 1024 )
    EndIf
    *Code\ExpressionCount + 1
    
    Define.Expression *Expression = @*Code\Expressions( IndexVariable )
    *Expression\Operator = Op
    *Expression\Context = *Parser\CurrentExpressionContext
    *Expression\Type = Tp
    *Expression\FirstOperandI = Operand1
    *Expression\SecondOperandI = Operand2
    *Expression\Region\LeftPos = StartPos
    *Expression\Region\RightPos = *Parser\Position - *Parser\StartPosition
    *Expression\NextExpression = -1
    
  EndMacro

  ; ---------------------------------------------------------------------------
  
  Macro MakeStatement( IndexVariable, Kind, Reference, Scope = -1 )
    
    Define.i IndexVariable = *Code\StatementCount
    If ArraySize( *Code\Statements() ) = IndexVariable
      ReDim *Code\Statements( IndexVariable + 1024 )
    EndIf
    Define.Statement *Statement = @*Code\Statements( IndexVariable )
    *Code\StatementCount + 1
    
    *Statement\StatementKind = Kind
    *Statement\ReferencedIndex = Reference
    *Statement\InnerScope = Scope
    *Statement\NextStatement = -1
    
    If *Parser\CurrentStatement <> -1
      *Code\Statements( *Parser\CurrentStatement )\NextStatement = IndexVariable
    EndIf
    *Parser\CurrentStatement = IndexVariable
    
  EndMacro

  ; ---------------------------------------------------------------------------
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

  ; ---------------------------------------------------------------------------
  
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

  ; ---------------------------------------------------------------------------
  
  Procedure ExpectSymbol( *Parser.Parser, Symbol.s, Length.i )
    SkipWhitespace( *Parser )
    If Not MatchToken( *Parser, Symbol, Length, #True )
      ;;;;TODO: diagnose
      Debug Str( *Parser\CurrentLine ) + ": Expecting " + Symbol + " but got " + Chr( PeekB( *Parser\Position ) )
    EndIf
  EndProcedure

  ; ---------------------------------------------------------------------------
  ; Returns a DefinitionFlag.
  
  Procedure.i ParseModifier( *Parser.Parser, *Code.Code )
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

  ; ---------------------------------------------------------------------------
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

  ; ---------------------------------------------------------------------------
  
  Procedure.i ParseIdentifier( *Parser.Parser, *Code.Code )
    
    SkipWhitespace( *Parser )
    
    ;;;;TODO: escaped identifiers
    ;;;;TODO: identifiers ending in !
    
    Define.i Length = ReadName( *Parser )
    If Length = -1
      ProcedureReturn -1
    EndIf
    
    Define *Element = FindMapElement( *Code\IdentifierTable(), *Parser\NameBuffer )
    If *Element = #Null
      Define.s Name = Left( *Parser\NameBuffer, Length )
      If ArraySize( *Code\Identifiers() ) = *Code\IdentifierCount
        ReDim *Code\Identifiers.s( *Code\IdentifierCount + 512 )
      EndIf
      Define.i IdIndex = *Code\IdentifierCount
      *Code\Identifiers( IdIndex ) = Name
      *Code\IdentifierCount + 1
      *Element = AddMapElement( *Code\IdentifierTable(), *Parser\NameBuffer )
      PokeI( *Element, IdIndex )
    EndIf
    
    ProcedureReturn PeekI( *Element )
    
  EndProcedure

  ; ---------------------------------------------------------------------------
  
  Procedure.i ParseAnnotation( *Parser.Parser, *Code.Code )
    
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
    ElseIf MatchToken( *Parser, "icall", 5 )
      AnnotationKind = #IcallAnnotation
    ElseIf MatchToken( *Parser, "product", 7 )
      AnnotationKind = #ProductAnnotation
    ElseIf MatchToken( *Parser, "company", 7 )
      AnnotationKind = #CompanyAnnotation
    ElseIf MatchToken( *Parser, "naming", 6 )
      AnnotationKind = #NamingConventionAnnotation
    ElseIf MatchToken( *Parser, "indentation", 11 )
      AnnotationKind = #IndentationAnnotation
    ElseIf MatchToken( *Parser, "buildoption", 11 )
      AnnotationKind = #BuildOptionAnnotation
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
    
    Define.i AnnotationIndex = *Code\AnnotationCount
    If ArraySize( *Code\Annotations() ) = AnnotationIndex
      ReDim *Code\Annotations( AnnotationIndex + 512 )
    EndIf
    Define.Annotation *Annotation = @*Code\Annotations( AnnotationIndex )
    *Annotation\AnnotationKind = AnnotationKind
    *Annotation\AnnotationText = Text
    *Annotation\NextAnnotation = -1
    *Code\AnnotationCount + 1
    
    ProcedureReturn AnnotationIndex
    
  EndProcedure

  ; ---------------------------------------------------------------------------
  
  ; Expression parsing is recursive.
  Declare.i ParseExpression( *Parser.Parser, *Code.Code )
  
  Procedure.i ParseBasicExpression( *Parser.Parser, *Code.Code )
    
    SkipWhitespace( *Parser )
    If *Parser\Position = *Parser\EndPosition
      ProcedureReturn -1
    EndIf
    
    Define.i LeftPos = *Parser\Position - *Parser\StartPosition
    
    Define.i Operator = -1
    Define.i Type = #INVALID_TYPE_ID
    Define.i FirstOperandI
    Define.i SecondOperandI = -1
    
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
      
      If Not FindMapElement( *Code\StringLiterals(), *Parser\StringBuffer )
        FirstOperandI = MapSize( *Code\StringLiterals() )
        AddMapElement( *Code\StringLiterals(), *Parser\StringBuffer )
        *Code\StringLiterals() = FirstOperandI
      Else
        FirstOperandI = *Code\StringLiterals()
      EndIf
      
      Type = #StringLiteralType
      Operator = #LiteralExpression
      
      UndefineMacro PushChar
      
    ElseIf IsAlpha( Char ) Or Char = '_'
      
      Operator = #NameExpression
      FirstOperandI = ParseIdentifier( *Parser, *Code )
      SecondOperandI = *Parser\CurrentScope
      
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
        
        Define.i FirstExpression = ParseExpression( *Parser, *Code )
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
          
          Define.i Expression = ParseExpression( *Parser, *Code )
          If Expression = -1
            Break
          EndIf
          
          *Code\Expressions( LastExpression )\NextExpression = Expression
          LastExpression = Expression
          
        Wend
        
        Operator = #TupleExpression
        FirstOperandI = FirstExpression
        
      EndIf
    
    Else
      ProcedureReturn -1
    EndIf
    
    If SecondOperandI <> -1
      MakeExpressionOp2I( Expression, Operator, Type, FirstOperandI, SecondOperandI, LeftPos )
    Else
      MakeExpressionOpI( Expression, Operator, Type, FirstOperandI, LeftPos )
    EndIf
    
    ProcedureReturn Expression
    
  EndProcedure
  
  ; ---------------------------------------------------------------------------
  
  Procedure.i ParseUnaryPostfixExpression( *Parser.Parser, *Code.Code )
    
    ;;;;TODO: first one needs to be allowed to be a full expression
    Define.i FirstExpression = ParseBasicExpression( *Parser, *Code )
    If FirstExpression = -1
      ProcedureReturn -1
    EndIf
    
    ; Check for dot expressions.
    Define.b IsDotExpression = #False
    SkipWhitespace( *Parser )
    If MatchToken( *Parser, ".", 1, #True )
      
      Define.i SecondExpression = ParseBasicExpression( *Parser, *Code )
      If SecondExpression <> -1
        ; Note the inversion of the operands. "A.B" -> "B( A )"
        MakeExpressionOp2I( DotExpr, #ApplyExpression, #INVALID_TYPE_ID, SecondExpression, FirstExpression, *Code\Expressions( FirstExpression )\Region\LeftPos )
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
          SecondExpression = ParseBasicExpression( *Parser, *Code )
        ElseIf Char = '<'
          ; Angle bracket in value context switches us into type context.
          PushExpressionContext( *Parser, #TypeContext )
          SecondExpression = ParseBasicExpression( *Parser, *Code )
          PopExpressionContext( *Parser )
        EndIf
      Else
        ; In type context, '<' is nesting and '(' is grouping.
        If Char = '<' Or Char = '('
          SecondExpression = ParseBasicExpression( *Parser, *Code )
        EndIf
      EndIf
      
      If SecondExpression <> -1
        ; If the left side is a dot expression, create a tuple expression. "A.B( C )" -> "B( A, C )"
        If IsDotExpression
          MakeExpressionOp2I( TupleExpression, #TupleExpression, #INVALID_TYPE_ID, *Code\Expressions( FirstExpression )\SecondOperandI, SecondExpression, *Code\Expressions( FirstExpression )\Region\LeftPos )
          *Code\Expressions( FirstExpression )\SecondOperandI = TupleExpression
          ProcedureReturn FirstExpression
        Else
          MakeExpressionOp2I( ApplyExpr, #ApplyExpression, #INVALID_TYPE_ID, FirstExpression, SecondExpression, *Code\Expressions( FirstExpression )\Region\LeftPos )
          ProcedureReturn ApplyExpr
        EndIf
      EndIf
    EndIf
    
    ProcedureReturn FirstExpression
    
  EndProcedure

  ; ---------------------------------------------------------------------------

  Procedure.i ParseUnaryPrefixExpression( *Parser.Parser, *Code.Code )
    
    SkipWhitespace( *Parser )
    
    Define.i LeftPos = *Parser\Position - *Parser\StartPosition
    Define.i PrefixOperator = -1
    If MatchToken( *Parser, "!", 1, #True )
      PrefixOperator = #NotExpression
    EndIf
    
    Define.i Expression = ParseUnaryPostfixExpression( *Parser, *Code )
    
    If PrefixOperator <> -1
      MakeExpressionOpI( UnaryExpression, PrefixOperator, #INVALID_TYPE_ID, Expression, LeftPos )
      Expression = UnaryExpression
    EndIf
    
    ProcedureReturn Expression
    
  EndProcedure
  
  ; ---------------------------------------------------------------------------
  
  Procedure.i ParseBinaryExpression( *Parser.Parser, *Code.Code )
    
    Define.i Expression = ParseUnaryPrefixExpression( *Parser, *Code )
    
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
      
      Define.i RightExpression = ParseExpression( *Parser, *Code )
      
      MakeExpressionOp2I( BinaryExpression, Operator, #INVALID_TYPE_ID, Expression, RightExpression, *Code\Expressions( Expression )\Region\LeftPos )
      Expression = BinaryExpression
      
    EndIf
  
    ProcedureReturn Expression
    
  EndProcedure
  
  ; ---------------------------------------------------------------------------
  
  Procedure.i ParseExpression( *Parser.Parser, *Code.Code )
    ProcedureReturn ParseBinaryExpression( *Parser, *Code )
  EndProcedure
  
  ; ---------------------------------------------------------------------------
  
  ; Statements may themselves open scope (e.g. if statement) and thus
  ; lead to recursion.
  Declare.i ParseStatementList( *Parser.Parser, *Code.Code, Definition.i = -1 )
  
  Procedure.i ParseStatement( *Parser.Parser, *Code.Code )
    
    SkipWhitespace( *Parser )
    
    Define.i Statement = -1
    Define.i ReferencedIndex = -1
    Define.i InnerScope = -1
    
    If MatchToken( *Parser, "if", 2 )
      
      Statement = #IfStatement
      ReferencedIndex = ParseExpression( *Parser, *Code )
      InnerScope = ParseStatementList( *Parser, *Code )
      
      CompilerIf #False
      SkipWhitespace( *Parser )
      If MatchToken( *Parser, "elseif", 6 )
      ElseIf MatchToken( *Parser, "else", 4 )
      EndIf
      CompilerEndIf
      
    ElseIf MatchToken( *Parser, "return", 6 )
      
      SkipWhitespace( *Parser )
      If Not MatchToken( *Parser, ";", 1, #True )
        
        ReferencedIndex = ParseExpression( *Parser, *Code )
        ExpectSymbol( *Parser, ";", 1 )
        
      EndIf
      
      Statement = #ReturnStatement
      
    ElseIf MatchToken( *Parser, "resend", 6 )
      
      SkipWhitespace( *Parser )
      If Not MatchToken( *Parser, ";", 1, #True )
        
        ReferencedIndex = ParseExpression( *Parser, *Code )
        ExpectSymbol( *Parser, ";", 1 )
        
      EndIf
      
      Statement = #ResendStatement
      
    Else
      
      ReferencedIndex = ParseExpression( *Parser, *Code )
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
  
  ; ---------------------------------------------------------------------------  
  ; Opens a new scope and parses a list of statements into it.
  ; Returns the index of the scope.
  
  Procedure.i ParseStatementList( *Parser.Parser, *Code.Code, Definition.i = -1 )
    
    PushScope( *Parser, *Code, Definition )
    Define.i Scope = CurrentScope
    *Code\Scopes( CurrentScope )\Definition = Definition
      
    Define.i FirstStatement = -1
    Define.i LastStatement = -1
    
    While *Parser\Position < *Parser\EndPosition
      
      SkipWhitespace( *Parser )
      If MatchToken( *Parser, "end", 3 )
        Break
      EndIf
      
      Define.i Statement = ParseStatement( *Parser, *Code )
      If Statement = -1
        Break
      EndIf
      
      If FirstStatement = -1
        FirstStatement = Statement
        ; The list of statements is linked through the scope.
        *Code\Scopes( CurrentScope )\FirstDefinitionOrStatement = Statement      
      Else
        *Code\Statements( LastStatement )\NextStatement = Statement
      EndIf
      
      ; Statement may be a sequence (e.g. if-else). Iterate to last one.
      While *Code\Statements( Statement )\NextStatement <> -1
        Statement = *Code\Statements( Statement )\NextStatement
      Wend
      
      LastStatement = Statement
      
    Wend
    
    PopScope( *Parser )
    ExpectSymbol( *Parser, ";", 1 )
    
    ProcedureReturn Scope
    
  EndProcedure
  
  ; ---------------------------------------------------------------------------
  
  Procedure.i ParseParameterList( *Parser.Parser, *Code.Code )
    
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
      
      Define.i Name = ParseIdentifier( *Parser, *Code )
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
      TypeExpression = ParseExpression( *Parser, *Code )
      If TypeExpression = -1
        ;;;;TODO: add diagnostic
        Debug "Expecting type!!"
      EndIf
      PopExpressionContext( *Parser )
      
      Define.i ParameterIndex = *Code\ParameterCount
      If ArraySize( *Code\Parameters() ) = ParameterIndex
        ReDim *Code\Parameters( ParameterIndex + 512 )
      EndIf
      *Code\ParameterCount + 1
      
      Define.Parameter *Parameter = @*Code\Parameters( ParameterIndex )
      
      *Parameter\Name = Name
      *Parameter\TypeExpression = TypeExpression
      *Parameter\NextParameter = -1
      
      If FirstParameter = -1
        FirstParameter = ParameterIndex
      Else
        *Code\Parameters( LastParameter )\NextParameter = ParameterIndex
      EndIf
      
      LastParameter = ParameterIndex
      
    Wend
    
    ProcedureReturn FirstParameter
    
  EndProcedure
  
  ; ---------------------------------------------------------------------------  
  ; Returns index of definition or -1 on failure.
  
  Procedure.i ParseDefinition( *Parser.Parser, *Code.Code )
    
    ; Parse annotations.
    Define.i FirstAnnotation = -1
    Define.i LastAnnotation = -1
    While *Parser\Position < *Parser\EndPosition
      Define.i Annotation = ParseAnnotation( *Parser, *Code )
      If Annotation = -1
        Break
      EndIf
      If FirstAnnotation = -1
        FirstAnnotation = Annotation
      Else
        *Code\Annotations( LastAnnotation )\NextAnnotation = Annotation
      EndIf
      LastAnnotation = Annotation
    Wend
    
    ; Parse modifiers.
    Define.i Flags = 0
    While *Parser\Position < *Parser\EndPosition
      Define.i Modifier = ParseModifier( *Parser, *Code )
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
    Define.i Name = ParseIdentifier( *Parser, *Code )
    If Name = -1
      ;;;;TODO
      ProcedureReturn -1
    EndIf
    
    ; Add definition.
    If ArraySize( *Code\Definitions() ) = *Code\DefinitionCount
      ReDim *Code\Definitions.Definition( *Code\DefinitionCount + 256 )
    EndIf
    Define.i DefinitionIndex = *Code\DefinitionCount
    Define *Definition.Definition = @*Code\Definitions( DefinitionIndex )
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
    *Code\DefinitionCount + 1
    
    ; Add to scope.
    If *Parser\CurrentDefinitionInScope <> -1
      *Code\Definitions( *Parser\CurrentDefinitionInScope )\NextDefinition = DefinitionIndex
    Else
      *Code\Scopes( *Parser\CurrentScope )\FirstDefinitionOrStatement = DefinitionIndex
    EndIf
    *Parser\CurrentDefinitionInScope = DefinitionIndex
    
    ; Parse type parameters.
    SkipWhitespace( *Parser )
    If MatchToken( *Parser, "<", 1, #True )
      
      PushExpressionContext( *Parser, #TypeContext )
      *Definition\FirstTypeParameter = ParseParameterList( *Parser, *Code )
      PopExpressionContext( *Parser )
      ExpectSymbol( *Parser, ">", 1 )
      
    EndIf
    
    ; Parse value parameters.
    SkipWhitespace( *Parser )
    If MatchToken( *Parser, "(", 1, #True )
      
      *Definition\FirstValueParameter = ParseParameterList( *Parser, *Code )
      ExpectSymbol( *Parser, ")", 1 )
      
    EndIf
    
    ; Parse type.
    SkipWhitespace( *Parser )
    If MatchToken( *Parser, ":", 1, #True )
      PushExpressionContext( *Parser, #TypeContext )
      *Definition\TypeExpression = ParseExpression( *Parser, *Code )
      PopExpressionContext( *Parser )
    ElseIf MatchToken( *Parser, "=", 1, #True )
      PushExpressionContext( *Parser, #TypeContext )
      *Definition\TypeExpression = ParseExpression( *Parser, *Code )
      *Definition\Flags | #IsAlias
      PopExpressionContext( *Parser )
    EndIf
    
    ; Parse clauses.
    
    ; Parse body.
    SkipWhitespace( *Parser )
    If Not MatchToken( *Parser, ";", 1, #True )
      *Definition\InnerScope = ParseStatementList( *Parser, *Code, DefinitionIndex )
    EndIf
    
    ProcedureReturn DefinitionIndex
    
  EndProcedure
  
  ; ---------------------------------------------------------------------------
  
  Procedure Parse( *Text.Text, *Code.Code )
    
    ;;;;TODO: preparse libraries and reset to the preparsed state here
    
    Define.Parser Parser
    Parser\Position = *Text\Contents
    Parser\EndPosition = *Text\Contents + *Text\ByteLength
    Parser\StartPosition = *Text\Contents
    Parser\CurrentDefinitionInScope = -1
    Parser\CurrentStatement = -1
    Parser\CurrentLine = 1
    Parser\CurrentColumn = 1
    Parser\CurrentExpressionContext = #ValueContext
    
    While Parser\Position < Parser\EndPosition
      If ParseDefinition( @Parser, *Code ) = -1
        ;;;;TODO: error handling (diagnose and keep going)
        Break
      EndIf
    Wend
    
    Debug( "Definitions: " + Str( *Code\DefinitionCount ) + ", Expressions: " +
           Str( *Code\ExpressionCount ) + ", Statements: " + Str( *Code\StatementCount ) +
           ", Parameters: " + Str( *Code\ParameterCount ) + ", Identifiers: " + Str( *Code\IdentifierCount ) )
    
  EndProcedure
  
  
EndModule

;{ TESTS

; -----------------------------------------------------------------------------

; NOTE: Cannot wrap ProcedureUnit/EndProcedureUnit using macros; PureUnitGui won't see the tests.

Macro TestFixture()
  UseModule Utils
  UseModule Code
  UseModule Parse
  UseModule Text
  Define.Code Code
EndMacro

Macro TestParse( String, Name = Test, NoDiagnostics = #True )
  Define.Text Name#Text
  MakeText( @Name#Text, String, Stringify( Name ) )
  ResetCode( @Code )
  Parse( @Name#Text, @Code )
  If NoDiagnostics
    Assert( Code\DiagnosticCount = 0 )
  EndIf
  FreeText( @Name#Text )
EndMacro

Macro TestParseExpression( String, Name = Test, NoDiagnostics = #True )
  TestParse( "method foo() return " + String + "; end;", Name, NoDiagnostics )
EndMacro

Macro TestParseStatement( String, Name = Test, NoDiagnostics = #True )
  TestParse( "method foo() " + String + "; end;", Name, NoDiagnostics )
EndMacro

; -----------------------------------------------------------------------------

ProcedureUnit CanSkipComments()

  TestFixture()
  TestParse( "/* type */  program Foo;", MultiLineComment )
  TestParse( "  // type \nprogram Foo;", SingleLineComment )
  TestParse( "/* /* type /* */ program Bar;", NestedComment )
  
EndProcedureUnit

; ---------------------------------------------------------------------------

ProcedureUnit CanParseModifier()

  TestFixture()

  Macro TestModifier( Modifier )
    TestParse( LCase( Stringify( Modifier ) ) + " type foo;", Modifier#LowerCase )
    Assert( Code\DefinitionCount = 1 )
    Assert( Code\Definitions( 0 )\Flags & #Is#Modifier )
    TestParse( UCase( Stringify( Modifier ) ) + " type foo;", Modifier#UpperCase )
    Assert( Code\DefinitionCount = 1 )
    Assert( Code\Definitions( 0 )\Flags & #Is#Modifier )
  EndMacro
  
  TestModifier( Abstract )
  TestModifier( Mutable )
  TestModifier( Immutable )
  
  UndefineMacro TestModifier
  
EndProcedureUnit

; ---------------------------------------------------------------------------

ProcedureUnit CanParseIdentifier()

  TestFixture()

  TestParse( "type foobar;", LowerCase )
  Assert( Code\IdentifierCount = 1 )
  Assert( FindMapElement( Code\IdentifierTable(), "foobar" ) <> #Null )
  Assert( PeekI( FindMapElement( Code\IdentifierTable(), "foobar" ) ) = 0 )
  
  TestParse( "type FOOBAR;", UpperCase )
  Assert( Code\IdentifierCount = 1 )
  Assert( FindMapElement( Code\IdentifierTable(), "foobar" ) <> #Null )
  Assert( PeekI( FindMapElement( Code\IdentifierTable(), "foobar" ) ) = 0 )
  
  TestParse( "type FooBar;", MixedCase )
  Assert( Code\IdentifierCount = 1 )
  Assert( FindMapElement( Code\IdentifierTable(), "foo_bar" ) <> #Null )
  Assert( PeekI( FindMapElement( Code\IdentifierTable(), "foo_bar" ) ) = 0 )
  
EndProcedureUnit

; ---------------------------------------------------------------------------

ProcedureUnit CanParseIntegerLiteral()

  TestFixture()
  TestParseExpression( "01234" )

  Assert( Code\ExpressionCount = 1 )
  Assert( Code\Expressions( 0 )\Operator = #LiteralExpression )
  Assert( Code\Expressions( 0 )\Context = #ValueContext )
  Assert( Code\Expressions( 0 )\Type = #IntegerLiteralType )
  Assert( Code\Expressions( 0 )\FirstOperandI = 1234 )
  
EndProcedureUnit

; ---------------------------------------------------------------------------

ProcedureUnit CanParseStringLiteral()

  TestFixture()
  TestParseExpression( ~" \"abc\"" )

  Assert( Code\ExpressionCount = 1 )
  Assert( Code\Expressions( 0 )\Operator = #LiteralExpression )
  Assert( Code\Expressions( 0 )\Context = #ValueContext )
  Assert( Code\Expressions( 0 )\Type = #StringLiteralType )
  Assert( Code\Expressions( 0 )\FirstOperandI = 0 )
  Assert( MapSize( Code\StringLiterals() ) = 1 )
  Assert( FindMapElement( Code\StringLiterals(), "abc" ) <> #Null )
  Assert( Code\StringLiterals() = 0 )
  
EndProcedureUnit

; ---------------------------------------------------------------------------

ProcedureUnit CanParseNameExpression()

  TestFixture()
  TestParseExpression( "test" )
  
  Assert( Code\ExpressionCount = 1 )
  Assert( Code\IdentifierCount = 2 ) ; Expression is wrapped in method definition.
  Assert( Code\Identifiers( 1 ) = "test" )
  Assert( Code\Expressions( 0 )\Operator = #NameExpression )
  Assert( Code\Expressions( 0 )\Context = #ValueContext )
  ;;;;TODO: check type
  Assert( Code\Expressions( 0 )\FirstOperandI = 1 )
  
EndProcedureUnit

; ---------------------------------------------------------------------------

ProcedureUnit CanParseParenthesizedExpression()

  TestFixture()

  ; This is *not* a tuple expression. Need at least two elements.
  TestParseExpression( "( a )" )

  Assert( Code\ExpressionCount = 1 )
  Assert( Code\IdentifierCount = 2 ) ; Expression is wrapped in method definition.
  Assert( Code\Identifiers( 1 ) = "a" )
  Assert( Code\Expressions( 0 )\Operator = #NameExpression )
  Assert( Code\Expressions( 0 )\Context = #ValueContext )
  Assert( Code\Expressions( 0 )\FirstOperandI = 1 )
  Assert( Code\Expressions( 0 )\NextExpression = -1 )
  
EndProcedureUnit

; ---------------------------------------------------------------------------

ProcedureUnit CanParseTupleExpression()

  TestFixture()
  TestParseExpression( "( a, b, c )" )

  Assert( Code\ExpressionCount = 4 )
  Assert( Code\IdentifierCount = 4 ) ; Expression is wrapped in method definition.
  Assert( Code\Identifiers( 1 ) = "a" )
  Assert( Code\Identifiers( 2 ) = "b" )
  Assert( Code\Identifiers( 3 ) = "c" )
  Assert( Code\Expressions( 0 )\Operator = #NameExpression )
  Assert( Code\Expressions( 0 )\FirstOperandI = 1 )
  Assert( Code\Expressions( 0 )\NextExpression = 1 )
  Assert( Code\Expressions( 1 )\Operator = #NameExpression )
  Assert( Code\Expressions( 1 )\FirstOperandI = 2 )
  Assert( Code\Expressions( 1 )\NextExpression = 2 )
  Assert( Code\Expressions( 2 )\Operator = #NameExpression )
  Assert( Code\Expressions( 2 )\FirstOperandI = 3 )
  Assert( Code\Expressions( 2 )\NextExpression = -1 )
  Assert( Code\Expressions( 3 )\Operator = #TupleExpression )
  Assert( Code\Expressions( 3 )\FirstOperandI = 0 )
  Assert( Code\Expressions( 3 )\NextExpression = -1 )
  
EndProcedureUnit

; ---------------------------------------------------------------------------

ProcedureUnit CanParseApplyExpression()

  TestFixture()

  ; One expression followed by another without an operator in-between is an application.
  ; NOTE: ATM, parenthesis are needed.
  TestParseExpression( "a( b )" )

  Assert( Code\ExpressionCount = 3 )
  Assert( Code\IdentifierCount = 3 ) ; Expression is wrapped in method definition.
  Assert( Code\Identifiers( 1 ) = "a" )
  Assert( Code\Identifiers( 2 ) = "b" )
  Assert( Code\Expressions( 0 )\Operator = #NameExpression )
  Assert( Code\Expressions( 0 )\FirstOperandI = 1 )
  Assert( Code\Expressions( 0 )\NextExpression = -1 ) ; These are not linked.
  Assert( Code\Expressions( 1 )\Operator = #NameExpression )
  Assert( Code\Expressions( 1 )\FirstOperandI = 2 )
  Assert( Code\Expressions( 1 )\NextExpression = -1 )
  Assert( Code\Expressions( 2 )\Operator = #ApplyExpression )
  Assert( Code\Expressions( 2 )\FirstOperandI = 0 )
  Assert( Code\Expressions( 2 )\SecondOperandI = 1 )
  Assert( Code\Expressions( 2 )\NextExpression = -1 )
  
EndProcedureUnit

; ---------------------------------------------------------------------------

ProcedureUnit CanParseApplyNothingExpression()

  TestFixture()
  TestParseExpression( "a()" )

  Assert( Code\ExpressionCount = 3 )
  Assert( Code\IdentifierCount = 2 ) ; Expression is wrapped in method definition.
  Assert( Code\Identifiers( 1 ) = "a" )
  Assert( Code\Expressions( 0 )\Operator = #NameExpression )
  Assert( Code\Expressions( 0 )\Context = #ValueContext )
  Assert( Code\Expressions( 0 )\FirstOperandI = 1 )
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
  
; ---------------------------------------------------------------------------

; A.B is equivalent to B( A )
ProcedureUnit CanParseDotExpression()

  TestFixture()
  TestParseExpression( "a.b" )

  Assert( Code\ExpressionCount = 3 )
  Assert( Code\IdentifierCount = 3 ) ; Expression is wrapped in method definition.
  Assert( Code\Identifiers( 1 ) = "a" )
  Assert( Code\Identifiers( 2 ) = "b" )
  Assert( Code\Expressions( 0 )\Operator = #NameExpression )
  Assert( Code\Expressions( 0 )\FirstOperandI = 1 )
  Assert( Code\Expressions( 0 )\NextExpression = -1 ) ; These are not linked.
  Assert( Code\Expressions( 1 )\Operator = #NameExpression )
  Assert( Code\Expressions( 1 )\FirstOperandI = 2 )
  Assert( Code\Expressions( 1 )\NextExpression = -1 )
  Assert( Code\Expressions( 2 )\Operator = #ApplyExpression )
  Assert( Code\Expressions( 2 )\FirstOperandI = 1 ) ; Note the inversion here.
  Assert( Code\Expressions( 2 )\SecondOperandI = 0 )
  Assert( Code\Expressions( 2 )\NextExpression = -1 )
  
EndProcedureUnit

; ---------------------------------------------------------------------------

; A.B( C ) is equivalent to B( A, C )
ProcedureUnit CanParseDotExpressionWithArguments()

  TestFixture()
  TestParseExpression( "a.b( c )" )

  Assert( Code\ExpressionCount = 5 )
  Assert( Code\IdentifierCount = 4 ) ; Expression is wrapped in method definition.
  Assert( Code\Identifiers( 1 ) = "a" )
  Assert( Code\Identifiers( 2 ) = "b" )
  Assert( Code\Identifiers( 3 ) = "c" )
  Assert( Code\Expressions( 0 )\Operator = #NameExpression )
  Assert( Code\Expressions( 0 )\FirstOperandI = 1 )
  Assert( Code\Expressions( 0 )\NextExpression = -1 )
  Assert( Code\Expressions( 1 )\Operator = #NameExpression )
  Assert( Code\Expressions( 1 )\FirstOperandI = 2 )
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
  Assert( Code\Expressions( 3 )\FirstOperandI = 3 )
  Assert( Code\Expressions( 3 )\NextExpression = -1 )
  Assert( Code\Expressions( 4 )\Operator = #TupleExpression )
  Assert( Code\Expressions( 4 )\FirstOperandI = 0 )
  Assert( Code\Expressions( 4 )\SecondOperandI = 3 )
  Assert( Code\Expressions( 4 )\NextExpression = -1 )
  
EndProcedureUnit

; ---------------------------------------------------------------------------

CompilerIf #False
ProcedureUnit CanParseComplexApplyExpression()
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

; ---------------------------------------------------------------------------

ProcedureUnit CanParseIfStatementWithoutElse()

  TestFixture()
  TestParseStatement( "if ( a ) return 123" )
  
  ;;;;REVIEW: should this even open a new scope?

  Assert( Code\StatementCount = 2 )
  Assert( Code\ExpressionCount = 2 )
  Assert( Code\IdentifierCount = 2 ) ; Expression is wrapped in method definition.
  Assert( Code\ScopeCount = 3 )
  Assert( Code\Identifiers( 1 ) = "a" )
  Assert( Code\Scopes( 2 )\Definition = -1 )
  Assert( Code\Scopes( 2 )\Parent = 1 )
  Assert( Code\Scopes( 2 )\FirstDefinitionOrStatement = 0 )
  Assert( Code\Statements( 0 )\StatementKind = #ReturnStatement )
  Assert( Code\Statements( 0 )\ReferencedIndex = 1 )
  Assert( Code\Statements( 0 )\NextStatement = -1 )
  Assert( Code\Statements( 0 )\InnerScope = -1 )
  Assert( Code\Statements( 1 )\StatementKind = #IfStatement )
  Assert( Code\Statements( 1 )\ReferencedIndex = 0 )
  Assert( Code\Statements( 1 )\NextStatement = -1 )
  Assert( Code\Statements( 1 )\InnerScope = 2 )
  Assert( Code\Expressions( 0 )\Operator = #NameExpression )
  Assert( Code\Expressions( 0 )\FirstOperandI = 1 )
  Assert( Code\Expressions( 0 )\NextExpression = -1 )
  Assert( Code\Expressions( 1 )\Operator = #LiteralExpression )
  Assert( Code\Expressions( 1 )\FirstOperandI = 123 )
  Assert( Code\Expressions( 1 )\NextExpression = -1 )
  
EndProcedureUnit

; ---------------------------------------------------------------------------

;;;;TODO: =============================================================
CompilerIf #False
ProcedureUnit CanParseIfStatementWithElse()
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

; ---------------------------------------------------------------------------

ProcedureUnit CanParseAnnotation()

  TestFixture()
  TestParse( ~"|DESCRIPTION Foo\ntype Bar;" )

  Assert( Code\AnnotationCount = 1 )
  Assert( Code\Annotations( 0 )\AnnotationKind = #DescriptionAnnotation )
  Assert( Code\Annotations( 0 )\AnnotationText = "Foo" )
  
EndProcedureUnit

; ---------------------------------------------------------------------------

ProcedureUnit CanParseSimpleTypeDefinition()

  TestFixture()
  TestParse( "type First;" )

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

; ---------------------------------------------------------------------------

ProcedureUnit CanParseDerivedTypeDefinition()

  TestFixture()
  TestParse( "type First : Second;" )

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

; ---------------------------------------------------------------------------

ProcedureUnit CanParseTypeAliasDefinition()

  TestFixture()
  TestParse( "type First = A | B;" )

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

; ---------------------------------------------------------------------------

ProcedureUnit CanParseEmptyMethodDefinition()

  TestFixture()
  TestParse( "method First;" )

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

; ---------------------------------------------------------------------------

ProcedureUnit CanParseSimpleMethodDefinition()

  TestFixture()
  TestParse( "method First() Foo(); end;" )

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

; ---------------------------------------------------------------------------

ProcedureUnit CanParseMethodDefinitionWithValueParameters()

  TestFixture()
  TestParse( "method First( A, B : C ) end;" )

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

; ---------------------------------------------------------------------------

; A parameter can drop the *name*. I.e. instead of "A : B", you write "B".
; This does *NOT* drop the type part, it drops the name part.
; So "B" is equivalent to "B : B", i.e. it generates an implicit name.
; The interesting thing is that it can be a full type expression, not just a simple named type.
; So, "A | B" is just as valid as "A< B >", for example.
; "A | B" gets a name "a_or_b".
; "A & B" gets a name "a_and_b".
ProcedureUnit CanParseMethodDefinitionUsingImplicitlyNamedParameter()

  TestFixture()
  TestParse( "method First( A | B ) end;" )

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

; ---------------------------------------------------------------------------

ProcedureUnit CanParseTypeDefinitionWithAnnotation()

  TestFixture()
  TestParse( ~"|DESCRIPTION Something\n|DETAILS Foo\ntype First;" )

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

; ---------------------------------------------------------------------------

ProcedureUnit CanParseSimpleProgram()

  TestFixture()
  TestParse( ~"|PRODUCT MyProduct\n|COMPANY MyCompany\nprogram First;" )

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

UndefineMacro TestFixture
UndefineMacro TestParse
UndefineMacro TestParseExpression
UndefineMacro TestParseStatement

;}

; IDE Options = PureBasic 5.73 LTS (Windows - x64)
; CursorPosition = 566
; FirstLine = 552
; Folding = ------
; EnableXP