DeclareModule Network
  
  Structure NetworkServer
    Handle.i
    *Buffer
    *BufferPos
  EndStructure
  
  Declare.b StartNetworkServer( *Server.NetworkServer, Port.i, LocalConnectionsOnly.b = #True )
  Declare   StopNetworkServer( *Server.NetworkServer )
  Declare   SendStringToClient( *Server.NetworkServer, Client.i, String.s )
  Declare.s ReceiveStringFromClient( *Server.NetworkServer, Client.i )
  
EndDeclareModule

Module Network
  
  Global.b NetworkInitialized = #False
    
  ; ---------------------------------------------------------------------------
  
  Procedure.b StartNetworkServer( *Server.NetworkServer, Port.i, LocalConnectionsOnly.b = #True )
    
    If Not NetworkInitialized
      InitNetwork()
      NetworkInitialized = #True
    EndIf
    
    Define Result
    If LocalConnectionsOnly
      Result = CreateNetworkServer( #PB_Any, Port, #PB_Network_TCP | #PB_Network_IPv4, "127.0.0.1" )
    Else
      Result = CreateNetworkServer( #PB_Any, Port, #PB_Network_TCP | #PB_Network_IPv4 )
    EndIf
      
    If Result = 0
      ProcedureReturn #False
    EndIf
    
    *Server\Buffer = AllocateMemory( 65536 ) ; 10kb memory buffer by default.
    *Server\BufferPos = *Server\Buffer
    
    ProcedureReturn #True
    
  EndProcedure
  
  ; ---------------------------------------------------------------------------
  
  Procedure StopNetworkServer( *Server.NetworkServer )
    
    CloseNetworkServer( *Server\Handle )
    FreeMemory( *Server\Buffer )
    
    ResetStructure( *Server, NetworkServer )
    
  EndProcedure
  
  ; ---------------------------------------------------------------------------
  
  Procedure SendStringToClient( *Server.NetworkServer, Client.i, String.s )
    
    ;;REVIEW: why not SendNetworkString?
    
    Define.i NumBytes = StringByteLength( String, #PB_UTF8 )
    If NumBytes + 1 > MemorySize( *Server\Buffer )
      ;;;;REVIEW: automatically chop up strings that are too long?
      Debug "String length exceeds maximum allowed message length of 64k"
      ProcedureReturn
    EndIf
    
    PokeS( *Server\Buffer, String, NumBytes, #PB_UTF8 )
    SendNetworkData( Client, *Server\Buffer, NumBytes + 1 )
  
  EndProcedure
  
  ; ---------------------------------------------------------------------------
  
  Procedure.s ReceiveStringFromClient( *Server.NetworkServer, Client.i )
    
    Define.i ReadResult = ReceiveNetworkData( Client, *Server\Buffer, MemorySize( *Server\Buffer ) )
    If ReadResult <= 0
      Debug "Read failure!!"
      ProcedureReturn ""
    EndIf
    
    Define.s Text = PeekS( *Server\Buffer, ReadResult, #PB_UTF8 | #PB_ByteLength )
    ProcedureReturn Text
    
  EndProcedure
  
EndModule

; IDE Options = PureBasic 5.73 LTS (Windows - x64)
; CursorPosition = 61
; FirstLine = 35
; Folding = --
; EnableXP