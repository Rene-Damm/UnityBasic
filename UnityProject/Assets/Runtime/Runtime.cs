// All handles are 64bit. Upper 32bits are type, lower 32bits are index into array of given type.
// Instances are stored in arrays. Each array maintains a free list.
// Values are *not* tagged. Means literal values are indistinguishable from references. Literals need boxing to turn into references.
// Strings are reference-counted.

public static class Runtime
{
    public static Program program;
    public static Function[] functions;
    public static Type[] types;

    public struct Instances
    {
    }
    
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