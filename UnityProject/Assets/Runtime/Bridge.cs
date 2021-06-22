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

        while (true)
        {
            var message = await Receive();
            if (message == "exit")
                break;
            switch (message)
            {
                case "program":
                    break;
            }
        }
    }
    
    // Receive an initial program and start running it.
    private static void StartProgram()
    {
    }
    
    // Receive a program path to update the running program.
    private static void PatchProgram()
    {
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
}
