DeclareModule Text
  
  #SPACE = 32
  #NEWLINE = 10
  #RETURN = 13
  #EQUALS = 61
  
  ;;;;TODO: figure out how to deal with separate files feeding into inputs
  Structure TextRegion
    LeftPos.i
    RightPos.i
  EndStructure
  
  Structure Text
    Name.s
    CharacterLength.i ; Number of characters *excluding* trailing NUL.
    ByteLength.i      ; Number of bytes *including* trailing NUL.
    *Contents         ; NUL-terminated UTF-8 string.
  EndStructure
  
  Declare   MakeText( *Text.Text, Content.s, Name.s = "" )
  Declare   FreeText( *Text.Text )
  
  Declare.c ToLower( Character.c )
  Declare.b IsUpper( Character.c )
  Declare.b IsWhitespace( Character.c )
  Declare.b IsAlpha( Character.c )
  Declare.b IsAlphanumeric( Character.c )
  Declare.b IsDigit( Character.c )
  
EndDeclareModule

Module Text
  
  ; ---------------------------------------------------------------------------
  
  Procedure MakeText( *Text.Text, Content.s, Name.s = "" )
    If *Text\Contents <> #Null
      FreeText( *Text )
    EndIf
    Define.i CharacterLength = Len( Content )
    *Text\Name = Name
    *Text\CharacterLength = CharacterLength
    *Text\Contents = UTF8( Content )
    *Text\ByteLength = MemorySize( *Text\Contents )
  EndProcedure
  
  ; ---------------------------------------------------------------------------
  
  Procedure FreeText( *Text.Text )
    If *Text\Contents <> #Null
      FreeMemory( *Text\Contents )
    EndIf
    ClearStructure( *Text, Text )
  EndProcedure
  
  ; ---------------------------------------------------------------------------
  
  Procedure.c ToLower( Character.c )
    ;;;;FIXME: Not Unicode...
    If Character >= 65 And Character <= 90
      ProcedureReturn 97 + ( Character - 65 )
    EndIf
    ProcedureReturn Character
  EndProcedure

  ; ---------------------------------------------------------------------------

  Procedure.b IsUpper( Character.c )
    ;;;;FIXME: Not Unicode...
    If Character >= 65 And Character <= 90
      ProcedureReturn #True
    EndIf
    ProcedureReturn #False
  EndProcedure

  ; ---------------------------------------------------------------------------

  Procedure.b IsWhitespace( Character.c )
    Select Character
      Case #TAB, #SPACE, #NEWLINE, #RETURN
        ProcedureReturn #True
    EndSelect
    ProcedureReturn #False
  EndProcedure

  ; ---------------------------------------------------------------------------

  Procedure.b IsAlpha( Character.c )
    ;;;;FIXME: Not Unicode...
    If Character >= 65 And Character <= 90
      ProcedureReturn #True
    EndIf
    If Character >= 97 And Character <= 122
      ProcedureReturn #True
    EndIf
    ProcedureReturn #False
  EndProcedure

  ; ---------------------------------------------------------------------------

  Procedure.b IsDigit( Character.c )
    ;;;;FIXME: Not Unicode...
    If Character >= 48 And Character <= 57
      ProcedureReturn #True
    EndIf
    ProcedureReturn #False
  EndProcedure

  ; ---------------------------------------------------------------------------

  Procedure.b IsAlphanumeric( Character.c )
    If IsAlpha( Character ) Or IsDigit( Character )
      ProcedureReturn #True
    EndIf
    ProcedureReturn #False
  EndProcedure

EndModule

; IDE Options = PureBasic 5.73 LTS (Windows - x64)
; CursorPosition = 17
; FirstLine = 6
; Folding = --
; EnableXP