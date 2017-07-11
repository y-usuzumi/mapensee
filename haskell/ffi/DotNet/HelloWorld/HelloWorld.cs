using System;
using System.Runtime.InteropServices;

namespace HelloWorld
{
    class HelloWorld
    {
        [DllImport("HelloWorld.so", CallingConvention=CallingConvention.Cdecl)]
        private static extern void hs_init(IntPtr argc, IntPtr arv);

        [DllImport("HelloWorld.so", CallingConvention=CallingConvention.Cdecl)]
        private static extern void hs_exit();

        [DllImport("HelloWorld.so", CallingConvention=CallingConvention.Cdecl)]
        private static extern int hello(string str);

        public static void Main(string[] args)
        {
            Console.WriteLine("Initializing runtime...");
            hs_init(IntPtr.Zero, IntPtr.Zero);

            try
            {
                Console.WriteLine("Calling to Haskell...");
                int result = hello("Savor");
                Console.WriteLine("Got result: {0}", result);
            }
            finally
            {
                Console.WriteLine("Exiting runtime...");
                hs_exit();
            }
        }
    }
}
