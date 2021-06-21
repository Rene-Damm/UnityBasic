using System.Net.Sockets;
using UnityEditor;
using UnityEditor.Build.Reporting;
using UnityEngine;

public static class EditorTooling
{
    public static void BuildPlayer()
    {
        var options = new BuildPlayerOptions
        {
            target = BuildTarget.StandaloneWindows64, locationPathName = "Builds"
        };

        var report = BuildPipeline.BuildPlayer(options);
        switch (report.summary.result)
        {
            case BuildResult.Succeeded:
                Debug.Log("EditorTooling.BuildPlayer success");
                break;
            
            default:
                Debug.Log("EditorTooling.BuildPlayer failure");
                break;
        }
    }

    private static TcpClient s_Client;

    public static void ConnectClient()
    {
        s_Client = new TcpClient("127.0.0.1", 10978);
        var stream = s_Client.GetStream();
    }
}