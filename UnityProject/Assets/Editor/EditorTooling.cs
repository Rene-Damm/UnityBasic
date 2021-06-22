using System;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Threading.Tasks;
using UnityEditor;
using UnityEditor.Build.Reporting;
using UnityEngine;

public static class EditorTooling
{
    private static TcpClient s_Client;
    private static HttpListener s_AssetServer;

    public static async void Run()
    {
        // Connect to IDE.
        s_Client = new TcpClient();
        await s_Client.ConnectAsync("127.0.0.1", 10978);

        var exit = false;
        while (!exit)
        {
            var message = await Receive();
            switch (message)
            {
                case "exit":
                    exit = true;
                    break;
                
                case "build":
                    // Building directly from here results in "PlayerLoop internal function has been
                    // called recursively error" so put the call on the editor update loop instead.
                    EditorApplication.delayCall += () =>
                    {
                        if (!BuildPlayer())
                            Send("build failure");
                        else
                            Send("Builds/UnityBasic64.exe");
                    };
                    break;
            }
        }
        
        EditorApplication.Exit(0);
    }

    private static async void Send(string message)
    {
        var stream = s_Client.GetStream();
        var buffer = Encoding.UTF8.GetBytes(message);
        await stream.WriteAsync(buffer, 0, buffer.Length);
    }

    private static async Task<string> Receive()
    {
        var stream = s_Client.GetStream();

        var buffer = new byte[4 * 1024];
        var numRead = await stream.ReadAsync(buffer, 0, 4 * 1024);

        return Encoding.UTF8.GetString(buffer, 0, numRead);
    }

    private static bool BuildPlayer()
    {
        ////TODO: set company and product name from data transmitted by IDE
        
        var options = new BuildPlayerOptions
        {
            target = BuildTarget.StandaloneWindows64,
            locationPathName = "Builds/UnityBasic64.exe"
        };

        var report = BuildPipeline.BuildPlayer(options);
        switch (report.summary.result)
        {
            case BuildResult.Succeeded:
                Debug.Log("EditorTooling.BuildPlayer success");
                return true;
            
            default:
                Debug.Log("EditorTooling.BuildPlayer failure");
                return false;
        }
    }
}