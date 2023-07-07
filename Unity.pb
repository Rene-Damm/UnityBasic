XIncludeFile "Code.pb"
XIncludeFile "Machine.pb"

DeclareModule Unity
  
  UseModule Code
  UseModule Machine
  
  Enumeration UnityStatus
    #UnityNotInitialized
    #WaitingForUnityEditorToConnect
    #WaitingForUnityEditorToBuildPlayer
    #WaitingForUnityEditorToBuildAssetBundles
    #WaitingForUnityPlayerToConnect
    #UnityReadyState
    #UnityBadState ; States we can't yet recover from.
  EndEnumeration
  
  Structure UnityInstance
    Status.i
    EditorProcess.i
    PlayerProcess.i
  EndStructure
  
  Declare   LaunchUnity( *Unity.UnityInstance )
  Declare   KillUnity( *Unity.UnityInstance )
  
  Declare   SendProjectSettingsToUnityEditor( *Unity.UnityInstance, *Code.Code, ProgramIndex.l )
  Declare   SendCompiledProgramToUnityPlayer( *Unity.UnityInstance, *Program.MachineProgram )
  
EndDeclareModule

Module Unity
  
  ;;;;TODO: do away with this hardcoded limit
  #MAX_MESSAGE_LENGTH = ( 32 * 1024 )

  ; -----------------------------------------------------------------------------

  Structure UnityProjectSettings
    ProductName.s
    CompanyName.s
  EndStructure

  ; -----------------------------------------------------------------------------
  
  Global.s UnityEditorExecutablePath = "C:\Program Files\Unity\Hub\Editor\2020.3.22f1\Editor\Unity.exe"
  Global.s UnityPlayerExecutablePath = "C:\Dropbox\Workspaces\UnityBasic_PB\UnityProject\Builds\UnityBasic64.exe"
  Global.s UnityProjectPath = "C:\Dropbox\Workspaces\UnityBasic_PB\UnityProject"
  
  
  Global.i UnityEditor = RunProgram( UnityEditorExecutablePath, ~"-batchmode -projectPath \"" + GeneratedProjectPath + ~"\" -executeMethod EditorTooling.Run", "", #PB_Program_Open | #PB_Program_Read )
  Global.i UnityPlayer
  Global.i UnityEditorClient
  Global.i UnityPlayerClient
  Global.i UnityStatus
  Global *UnityNetworkBuffer = AllocateMemory( #MAX_MESSAGE_LENGTH )
  Global *UnityNetworkBufferPos
  Global.i UnityBatchSendClient
  Global.UnityProjectSettings UnityProjectSettings

  ; -----------------------------------------------------------------------------
  
  Procedure LaunchUnityEditor()
    
    UnityEditor = RunProgram( UnityEditorExecutablePath, ~"-batchmode -projectPath \"" + UnityProjectPath + ~"\" -executeMethod EditorTooling.Run", "", #PB_Program_Open | #PB_Program_Read )
    UnityStatus = #WaitingForUnityEditorToConnect
    
  EndProcedure
  
EndModule

; IDE Options = PureBasic 5.73 LTS (Windows - x64)
; CursorPosition = 27
; Folding = -
; EnableXP