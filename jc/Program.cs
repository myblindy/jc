using System;

namespace jc
{
    class Program
    {
        static void Main(string[] args)
        {
            if (args.Length < 1)
            {
                Console.WriteLine("Usage: jc source.jc");
                return;
            }

            Parser parser;
            try { parser = new Parser(args[0]); }
            catch
            {
                try { parser = new Parser(args[0] + ".jc"); }
                catch { Console.WriteLine("Problem reading {0}.", args[0]); return; }
            }
            parser.Parse();

            Console.WriteLine("\nBuild {0}, {1} error{2} and {3} warning{4} encountered.", 
                (parser.Errors.Count+parser.Warnings.Count)>0?"failed":"succeeded", 
                parser.Errors.Count, parser.Errors.Count==1?"":"s", 
                parser.Warnings.Count, parser.Warnings.Count==1?"":"s");
        }
    }
}
