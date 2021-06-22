public static class Runtime
{
    public static Program program;
    public static Function[] functions;
    public static Type[] types;
    
    public struct Program
    {
        public int functionCount;
        public int typeCount;
        public byte[] globals;
    }

    public struct Function
    {
    }

    public struct Type
    {
    }
}