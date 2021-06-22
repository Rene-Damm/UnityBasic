// Connects to the IDE and gets new content into the player.

using System.Net.Sockets;
using System.Text;
using System.Threading.Tasks;
using UnityEngine;

public static class Bridge
{
    private static TcpClient s_Client;
    
    [RuntimeInitializeOnLoadMethod]
    public static async void Run()
    {
        // Connect to IDE.
        s_Client = new TcpClient();
        await s_Client.ConnectAsync("127.0.0.1", 10978);

        var exit = false;
        while (!exit)
        {
            var message = await Receive(); 
            var split = message.Split('\0');
            foreach (var entry in split)
                if (!ProcessMessage(entry))
                {
                    exit = true;
                    break;
                }
        }
    }

    private static bool ProcessMessage(string message)
    {
        var split = message.Split('|');
        if (split.Length < 1)
            return true;

        switch (split[0])
        {
            case "exit":
                return false;
            
            case "commit":
                Debug.Log("commit");
                break;
            
            case "reset":
                Debug.Log("reset");
                break;
            
            case "patch":
                break;
            
            case "f":
                break;
            
            case "t":
                Debug.Log("type " + split[1]);
                break;
        }

        return true;
    }

    private static async Task<string> Receive()
    {
        var stream = s_Client.GetStream();

        var buffer = new byte[32 * 1024];
        var numRead = await stream.ReadAsync(buffer, 0, buffer.Length);

        // May have embedded NUL characters.
        return Encoding.UTF8.GetString(buffer, 0, numRead);
    }
}
