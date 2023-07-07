XIncludeFile "Utils.pb"

DeclareModule HTTP
    
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
  
  Declare.b ParseHTTPRequest( Text.s, *Request.HTTPRequest )
  Declare.s HTTPStatusCodeToString( StatusCode.i )
  Declare.s FormatHTTPResponse( *Response.HTTPResponse )

EndDeclareModule

Module HTTP
  
  UseModule Utils
  
  ; ---------------------------------------------------------------------------
  
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

  ; ---------------------------------------------------------------------------
  
  Procedure.s HTTPStatusCodeToString( StatusCode.i )
    Select StatusCode
      Case 200
        ProcedureReturn "OK"
      Default
        ProcedureReturn "Hmpf..."
    EndSelect
  EndProcedure

  ; ---------------------------------------------------------------------------
  
  Procedure.s FormatHTTPResponse( *Response.HTTPResponse )
    
    Define.s Status = "HTTP/1.1 " + Str( *Response\StatusCode ) + " " + HTTPStatusCodeToString( *Response\StatusCode ) + ~"\n"
    
    Define.s Headers = "Content-Length: " + Len( *Response\Body ) + ~"\n"
    ForEach *Response\Headers()
      Define.s Value = *Response\Headers()
      Headers + MapKey( *Response\Headers() ) + ": " + Value + ~"\n"
    Next
    
    ProcedureReturn Status + Headers + ~"\n" + *Response\Body
    
  EndProcedure

  ; ---------------------------------------------------------------------------
  
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

  ; ---------------------------------------------------------------------------
  
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

EndModule

; IDE Options = PureBasic 5.73 LTS (Windows - x64)
; CursorPosition = 65
; FirstLine = 65
; Folding = -
; EnableXP