using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

namespace jc
{
    class Parser
    {
        protected Lexer lexer;
        protected Lexer.Token lookahead;
        public SortedDictionary<LogType, TextWriter> Output = new SortedDictionary<LogType, TextWriter>();

        public List<string> Errors { get; set; }
        public List<string> Warnings { get; set; }

        /// <summary>
        /// The class holding a symbol table
        /// </summary>
        public class SymbolTable
        {
            /// <summary>
            /// The name of the symbol table, "Global" for the global table
            /// </summary>
            public string ID { get; set; }

            /// <summary>
            /// The constructor of the symbol table class
            /// </summary>
            /// <param name="id">The ID given to it</param>
            public SymbolTable(string id)
            {
                Data = new SortedDictionary<string, Information>();
                Parent = null;
                ID = id;
            }

            /// <summary>
            /// The class that holds information about the object being tracked by the symbol table
            /// </summary>
            public class Information
            {
                public Information()
                {
                    Properties = new SortedDictionary<string, object>();
                }

                public enum EKind
                {
                    Class,
                    Function,
                    Variable,
                }

                /// <summary>
                /// The kind of the object, function, class or variable
                /// </summary>
                public EKind Kind { get; set; }

                /// <summary>
                /// The properties of the object. Possible values for the key are
                /// "variable_type", "variable_arrayindices", "class_symtable",
                /// "function_type", "function_parameters", "function_symtable"
                /// </summary>
                public SortedDictionary<string, object> Properties { get; set; }
            }

            /// <summary>
            /// The data being tracked by the symbol table, grouped by the key (the name)
            /// </summary>
            public SortedDictionary<string, Information> Data { get; set; }

            /// <summary>
            /// The parent of the symbol table, null for the global table
            /// </summary>
            public SymbolTable Parent { get; set; }

            public Information Find(string id, bool recurse)
            {
                SymbolTable st;
                return Find(id, recurse, out st);
            }

            /// <summary>
            /// Finds a symbol name, optionally looking up in the table hierarchy
            /// </summary>
            /// <param name="id">The name of the symbol to look up</param>
            /// <param name="recurse">Whether or not to recurse up the tree</param>
            /// <param name="st">The symbol table in which the symbol was found</param>
            /// <returns>The structure holding all the information about the item, or null if not found</returns>
            public Information Find(string id, bool recurse, out SymbolTable st)
            {
                Information info;
                if (Data.TryGetValue(id, out info))
                {
                    st = this;
                    return info;
                }
                else
                    if (recurse && Parent != null)
                        return Parent.Find(id, recurse, out st);

                st = null; 
                return null;
            }

            /// <summary>
            /// Adds a symbol to the symbol table
            /// </summary>
            /// <param name="id">The name of the symbol</param>
            /// <returns>The Information structure to be filled</returns>
            public Information Add(string id)
            {
                if (Data.ContainsKey(id))
                    return Data[id];

                Data.Add(id, new Information());

                return Data[id];
            }

            /// <summary>
            /// Creates a child symbol table for a class or a function,
            /// setting its parent pointer to this table
            /// </summary>
            /// <param name="id">The name of the child table</param>
            /// <returns>The newly created symbol table</returns>
            public SymbolTable CreateChild(string id)
            {
                SymbolTable child = new SymbolTable(id);
                child.Parent = this;

                return child;
            }

            /// <summary>
            /// Represents the symbol table as a string for easy debugging
            /// </summary>
            /// <param name="level">The indentation level</param>
            /// <returns></returns>
            public string ToString(int level)
            {
                StringBuilder output = new StringBuilder();
                string pad = "".PadRight(level * 2, ' ');

                output.AppendFormat("{0}{1}\r\n", pad, ID);
                output.AppendFormat("{0}{1}\r\n", pad, "".PadRight(ID.Length, '='));
                output.AppendFormat("{0}{1}\r\n", pad, "");

                foreach (KeyValuePair<string, Information> kvp in Data)
                {
                    output.AppendFormat("{0}- {1} {2}\r\n", pad, kvp.Value.Kind, kvp.Key);
                    switch (kvp.Value.Kind)
                    {
                        case Information.EKind.Class:
                            output.AppendFormat("{0}  Class Size: {1}\r\n", pad, kvp.Value.Properties["class_size"]);
                            output.AppendFormat("{0}  Symbol Table:\r\n", pad);
                            output.Append((kvp.Value.Properties["class_symtable"] as SymbolTable).ToString(level + 1));
                            break;
                        case Information.EKind.Function:
                            output.AppendFormat("{0}  Type: {1}\r\n", pad, kvp.Value.Properties["function_type"]);
                            foreach (SemanticInfo.ParameterInfo pi in (List<SemanticInfo.ParameterInfo>)kvp.Value.Properties["function_parameters"])
                            {
                                string sizes = "";
                                foreach (string size in pi.ArrayIndices)
                                    sizes += "[" + size + "]";
                                output.AppendFormat("{0}  Parameter: {1} of type {2}, size {3} and array size {4}\r\n",
                                    pad, pi.Name, pi.Type, pi.Size, sizes == "" ? "<single item>" : sizes);
                            }
                            output.AppendFormat("{0}  Symbol Table:\r\n", pad);
                            output.Append((kvp.Value.Properties["function_symtable"] as SymbolTable).ToString(level + 1));
                            break;
                        case Information.EKind.Variable:
                            {
                                string sizes = "";
                                foreach (string size in kvp.Value.Properties["variable_arrayindices"] as string[])
                                    sizes += "[" + size + "]";

                                output.AppendFormat("{0}  Type: {1}\r\n", pad, kvp.Value.Properties["variable_type"]);
                                output.AppendFormat("{0}  Array Size: {1}\r\n", pad, sizes == "" ? "<single item>" : sizes);
                                output.AppendFormat("{0}  Size: {1}\r\n", pad, kvp.Value.Properties["variable_size"]);
                                output.AppendFormat("{0}  Offset: {1}\r\n", pad, kvp.Value.Properties["variable_offset"]);
                            }
                            break;
                        default:
                            output.AppendFormat("{0}  Cannot interpret {1}\r\n", pad, kvp.Value.Kind);
                            break;
                    }
                }

                return output.ToString();
            }

            /// <summary>
            /// Represents the symbol table as a string for easy debugging
            /// </summary>
            /// <returns></returns>
            public override string ToString()
            {
                return ToString(0);
            }
        }

        protected class StackFrame
        {
            protected Parser parser;
            protected SortedDictionary<int, bool> temp = new SortedDictionary<int, bool>();

            public StackFrame(Parser parser)
                : this(parser, false)
            {
            }

            public StackFrame(Parser parser, bool simfn)
            {
                this.parser = parser;
                if (simfn)
                    GenerateBackupRegisters(new StringBuilder());
            }

            public int GetTotalRealOffset()
            {
                int maxoffset = 0;
                foreach (KeyValuePair<string, SymbolTable.Information> kvp in parser.CurrentScope.Data)
                    if (kvp.Value.Kind == SymbolTable.Information.EKind.Variable)
                        if (kvp.Value.Properties.ContainsKey("variable_offset"))
                        {
                            int offset = (int)kvp.Value.Properties["variable_offset"]
                                + (kvp.Value.Properties.ContainsKey("variable_size") ?
                                (int)kvp.Value.Properties["variable_size"] : 0);
                            if (offset > maxoffset)
                                maxoffset = offset;
                        }

                return maxoffset;
            }

            public int GetTotalTempOffset()
            {
                int i = 0;
                while (true)
                {
                    if (!temp.ContainsKey(i) || !temp[i])
                        return i;
                    i += 4;
                }
            }

            public int RequestTemp()
            {
                return RequestTemp(4);
            }

            public int RequestTemp(int n)
            {
                int i = 0;
                while (true)
                {
                    if (!temp.ContainsKey(i))
                    {
                        temp.Add(i, true);
                        return i;
                    }
                    else if (!temp[i])
                    {
                        temp[i] = true;
                        return i;
                    }
                    i += 4;
                }
            }

            public void SafeFreeTemp(int i)
            {
                if (i >= GetTotalRealOffset())
                    FreeTemp(i - GetTotalRealOffset());
            }

            public void FreeTemp(int i)
            {
                temp[i] = false;
            }

            public int[] GenerateBackupRegisters(StringBuilder sb)
            {
                int[] regs = new int[14];

                sb.Append("%pusha\n");
                for (int i = 2; i <= 15; ++i)
                {
                    regs[i - 2] = RequestTemp();
                    sb.Append("addi r1,r14," + (GetTotalRealOffset() + regs[i - 2]) + "\nsw stack(r1),r" + i + " \n");
                }

                return regs;
            }

            public void GenerateRestoreRegisters(StringBuilder sb, int[] regs)
            {
                sb.Append("%popa\n");
                for (int i = 2; i <= 15; ++i)
                {
                    sb.Append("addi r1,r14," + (GetTotalRealOffset() + regs[i - 2]) + "\nlw r" + i + ",stack(r1)\n");
                    FreeTemp(regs[i - 2]);
                }
            }
        }

        protected Stack<StackFrame> CompilerStack = new Stack<StackFrame>();

        /// <summary>
        /// Holds the semantic information of the parsing nodes
        /// </summary>
        protected class SemanticInfo
        {
            /// <summary>
            /// Holds the parameter info for function processing
            /// </summary>
            public class ParameterInfo
            {
                /// <summary>
                /// The type of the parameter
                /// </summary>
                public string Type { get; set; }

                /// <summary>
                /// The name of the parameter
                /// </summary>
                public string Name { get; set; }

                /// <summary>
                /// Any array indices of the parameter
                /// </summary>
                public List<string> ArrayIndices { get; set; }

                public StringBuilder Code { get; set; }

                public int Size { get; set; }

                public int OldFrameOffset { get; set; }

                /// <summary>
                /// Initializes the structure
                /// </summary>
                public ParameterInfo() { ArrayIndices = new List<string>(); Code = new StringBuilder(); }
            }

            /// <summary>
            /// Holds the parameters as a dictionary
            /// </summary>
            public List<ParameterInfo> Parameters { get; set; }

            /// <summary>
            /// The name of the structure being parsed
            /// </summary>
            public string Name { get; set; }

            /// <summary>
            /// The type of the structure being parsed
            /// </summary>
            public string Type { get; set; }

            public int Offset { get; set; }
            public bool ThisOffset { get; set; }

            /// <summary>
            /// The indices of the structure being parsed
            /// </summary>
            public List<string> ArrayIndices { get; set; }

            /// <summary>
            /// Initializes the structure
            /// </summary>
            public SemanticInfo()
            {
                Parameters = new List<ParameterInfo>();
                ArrayIndices = new List<string>();
                Code = new StringBuilder();
            }

            public StringBuilder Code { get; set; }
        }

        /// <summary>
        /// The global symbol table
        /// </summary>
        public SymbolTable Global = new SymbolTable("Global");

        /// <summary>
        /// The current symbol table (scope)
        /// </summary>
        protected SymbolTable CurrentScope;

        protected int LastJumpLabel = 0;

        protected string GetNewJumpLabel()
        {
            return "j" + LastJumpLabel++;
        }

        /// <summary>
        /// Constructs the parser
        /// </summary>
        /// <param name="file">The file name to parse</param>
        public Parser(string file)
        {
            lexer = new Lexer(File.ReadAllText(file), this);
            Errors = new List<string>();
            Warnings = new List<string>();
            CurrentScope = Global;

            int pos = file.LastIndexOf('.');
            string realname = pos >= 0 ? file.Substring(0, pos) : file;

            Output.Add(LogType.Parse, File.CreateText(realname + "_parseinfo.txt"));
            Output.Add(LogType.SymbolTable, File.CreateText(realname + "_symboltable.txt"));
            Output.Add(LogType.Error, System.Console.Error);
            Output.Add(LogType.Warning, System.Console.Error);
            Output.Add(LogType.Code, File.CreateText(realname + ".m"));
        }

        /// <summary>
        /// The type of logging to do
        /// </summary>
        public enum LogType
        {
            Error,
            Warning,
            Parse,
            SymbolTable,
            Code
        }

        /// <summary>
        /// Logs a line of text
        /// </summary>
        /// <param name="type">The type of logging to do</param>
        /// <param name="line">The line of text to be logged</param>
        protected void Log(LogType type, string line)
        {
            Output[type].WriteLine(line);
        }

        /// <summary>
        /// Logs a syntax error
        /// </summary>
        /// <param name="error">The syntax error message</param>
        protected void SyntaxError(string error)
        {
            Errors.Add(string.Format("** Syntax error:{0}", error));
        }

        /// <summary>
        /// Logs a semantic error
        /// </summary>
        /// <param name="error">The semantic error message</param>
        protected void SemanticError(string error)
        {
            Errors.Add(string.Format("** Semantic error:{0}", error));
        }

        /// <summary>
        /// Logs a "Symbol already defined" semantic error
        /// </summary>
        /// <param name="symbol">The name of the symbol</param>
        protected void SemanticErrorSymbolAlreadyDefined(string symbol)
        {
            SemanticError(string.Format("{0}:Symbol {1} already defined",
                lexer.Line, symbol));
        }

        protected void SemanticErrorWrongType(string symbol)
        {
            SemanticError(string.Format("{0}:Symbol {1} is of a wrong type",
                lexer.Line, symbol));
        }

        protected void SemanticErrorFunctionNotFound(string symbol)
        {
            SemanticError(string.Format("{0}:Function {1} is not found",
                lexer.Line, symbol));
        }

        protected void SemanticErrorWrongTypeExpected(string symbol, string expected)
        {
            SemanticError(string.Format("{0}:Symbol {1} is of a wrong type, expected {2}",
                lexer.Line, symbol, expected ?? "unknown"));
        }

        protected void SemanticErrorVariableNotFound(string symbol)
        {
            SemanticError(string.Format("{0}:Symbol {1} is not found",
                lexer.Line, symbol));
        }

        protected void SemanticErrorWrongParameterCountExpected(string name, int count, int expected)
        {
            SemanticError(string.Format("{0}:Function {1} has {2} parameters, expected {3}",
                lexer.Line, name, count, expected));
        }

        protected void SemanticErrorWrongParamterTypeExpected(string name, int i, string type, string expected)
        {
            SemanticError(string.Format("{0}:Function {1} has a wrong parameter type. Parameter {2} is of type"
                + "{3}, expected {4}",
                lexer.Line, name, i, type, expected));
        }

        protected void SemanticErrorWrongReturnTypeExpected(string name, string type, string expected)
        {
            SemanticError(string.Format("{0}:In function {1} you tried to return {2}, expected {3}",
                lexer.Line, name, type, expected));
        }

        /// <summary>
        /// Logs a warning
        /// </summary>
        /// <param name="warning">The warning messge</param>
        protected void Warning(string warning)
        {
            Warnings.Add(string.Format("*  Warning:{0}", warning));
        }

        /// <summary>
        /// The main function. Runs the parser over the stream and 
        /// returns whether or not the stream is valid.
        /// </summary>
        /// <returns>A bool indicating whether or not the stream is valid syntactically</returns>
        public bool Parse()
        {
            // first parse the program
            // phase 1
            CompilerStack.Clear();
            CompilerStack.Push(new StackFrame(this));
            lexer.Reset();
            lookahead = lexer.GetNextToken();
            SemanticInfo si = new SemanticInfo();
            bool ok1 = NTF_Prog(1, si) && Match(1, Lexer.Token.TokenType.EndOfFile);

            ComputeClassSizes();

            // phase 2
            CompilerStack.Clear();
            CompilerStack.Push(new StackFrame(this));
            si = new SemanticInfo();
            lexer.Reset();
            lookahead = lexer.GetNextToken();
            bool ok2 = NTF_Prog(2, si) && Match(2, Lexer.Token.TokenType.EndOfFile);

            si.Code.Insert(0, "stack res 5000\nalign\n");
            si.Code.Append(
@"
putint	align
	add	r2,r0,r0		% Initialize buffer's index i
	cge	r3,r1,r0		% True if N >= 0
	bnz	r3,putint1		% Branch if True (N >= 0)
	sub	r1,r0,r1		% N = -N
putint1	modi	r4,r1,10		% Rightmost digit
	addi	r4,r4,48		% Convert to ch
	divi	r1,r1,10		% Truncate rightmost digit
	sb	putint9(r2),r4		% Store ch in buffer
	addi	r2,r2,1			% i++
	bnz	r1,putint1		% Loop if not finished
	bnz	r3,putint2		% Branch if True (N >= 0)
	addi	r3,r0,45
	sb	putint9(r2),r3		% Store '-' in buffer
	addi	r2,r2,1			% i++
	add	r1,r0,r0		% Initialize output register (r1 = 0)
putint2	subi	r2,r2,1			% i--
	lb	r1,putint9(r2)		% Load ch from buffer
	putc	r1			% Output ch
	bnz	r2,putint2		% Loop if not finished
    addi r1,r0,13       % load new line
    putc r1             % print it
    addi r1,r0,10       % new feed thingie
    putc r1
	jr	r15			% return to the caller
putint9	res	12			% loacl buffer (12 bytes)
	align

getint   add    r1,r0,r0         % n := 0 (result)
         add    r2,r0,r0         % c := 0 (character)
         add    r3,r0,r0         % s := 0 (sign)
getint1  getc   r2               % read c
         ceqi   r4,r2,32
         bnz    r4,getint1       % skip blanks
         ceqi   r4,r2,43
         bnz    r4,getint2       % branch if c is '+'
         ceqi   r4,r2,45
         bz     r4,getint3       % branch if c is not '-'
         addi   r3,r0,1          % s := 1 (number is negative)
getint2  getc   r2               % read c
getint3  ceqi   r4,r2,10
         bnz    r4,getint5       % branch if c is \n
         cgei   r4,r2,48
         bz     r4,getint4       % c < 0
         clei   r4,r2,57
         bz     r4,getint4       % c > 9
         muli   r1,r1,10         % n := 10 * n
         add    r1,r1,r2         % n := n + c
         subi   r1,r1,48         % n := n - '0'
         j      getint2
getint4  addi   r2,r0,63         % c := '?'
         putc   r2               % write c
         j      getint           % Try again
getint5  bz     r3,getint6       % branch if s = 0 (number is positive)
         sub    r1,r0,r1         % n := -n
getint6  jr     r15              % return
");

            Log(LogType.Code, si.Code.ToString());

            // then write out the debug information
            Log(LogType.SymbolTable, Global.ToString());

            foreach (string error in Errors)
                Log(LogType.Error, error);

            foreach (string warning in Warnings)
                Log(LogType.Warning, warning);


            // and flush the streams
            foreach (TextWriter tw in Output.Values)
                tw.Flush();

            return ok1 && ok2;
        }

        protected void ComputeClassSizes()
        {
            bool keepgoing;

            do
            {
                keepgoing = false;

                foreach (KeyValuePair<string, SymbolTable.Information> kvp in Global.Data)
                    if (kvp.Value.Kind == SymbolTable.Information.EKind.Class
                        && !kvp.Value.Properties.ContainsKey("class_size"))
                    {
                        SymbolTable classst = kvp.Value.Properties["class_symtable"] as SymbolTable;
                        int size = 0;
                        bool skipthis = false;
                        foreach (KeyValuePair<string, SymbolTable.Information> kvpi in classst.Data)
                            if (kvpi.Value.Kind == SymbolTable.Information.EKind.Variable)
                                if (kvpi.Value.Properties.ContainsKey("variable_size"))
                                    size += (int)kvpi.Value.Properties["variable_size"];
                                else
                                {
                                    string type = kvpi.Value.Properties["variable_type"] as string;
                                    switch (type)
                                    {
                                        case "int":
                                            kvpi.Value.Properties.Add("variable_size", 4);
                                            size += 4;
                                            break;
                                        case "float":
                                            kvpi.Value.Properties.Add("variable_size", 8);
                                            size += 8;
                                            break;
                                        default:
                                            // this is a class, attempt to get the class' size and
                                            // if we fail, flag to try again
                                            SymbolTable.Information info = Global.Find(type, false);
                                            if (info == null)
                                            {
                                                Warning(string.Format(
                                                    "Couldn't find class {0}, assuming its size is 0",
                                                    type));
                                                kvpi.Value.Properties.Add("variable_size", 0);
                                            }
                                            else
                                                if (info.Properties.ContainsKey("class_size"))
                                                {
                                                    kvpi.Value.Properties.Add("variable_size",
                                                        (int)info.Properties["class_size"]);
                                                    size += (int)info.Properties["class_size"];
                                                }
                                                else
                                                {
                                                    keepgoing = true;
                                                    skipthis = true;
                                                }
                                            break;
                                    }
                                }
                        if (!skipthis)
                            kvp.Value.Properties.Add("class_size", size);
                    }
            } while (keepgoing);

            ComputeClassVariableSizes(Global);
            ComputeVariableOffsets(Global);
        }

        protected void ComputeVariableOffsets(SymbolTable st)
        {
            int free = 0;
            foreach (KeyValuePair<string, SymbolTable.Information> kvp in st.Data)
                switch (kvp.Value.Kind)
                {
                    case SymbolTable.Information.EKind.Class:
                        ComputeVariableOffsets(kvp.Value.Properties["class_symtable"] as SymbolTable);
                        break;
                    case SymbolTable.Information.EKind.Function:
                        ComputeVariableOffsets(kvp.Value.Properties["function_symtable"] as SymbolTable);
                        break;
                    case SymbolTable.Information.EKind.Variable:
                        kvp.Value.Properties["variable_offset"] = free;
                        free += (int)kvp.Value.Properties["variable_size"];
                        break;
                }
        }

        protected void ComputeClassVariableSizes(SymbolTable st)
        {
            foreach (KeyValuePair<string, SymbolTable.Information> kvp in st.Data)
                switch (kvp.Value.Kind)
                {
                    case SymbolTable.Information.EKind.Class:
                        ComputeClassVariableSizes(kvp.Value.Properties["class_symtable"] as SymbolTable);
                        break;
                    case SymbolTable.Information.EKind.Function:
                        ComputeClassVariableSizes(kvp.Value.Properties["function_symtable"] as SymbolTable);
                        foreach (SemanticInfo.ParameterInfo pi in (List<SemanticInfo.ParameterInfo>)kvp.Value.Properties["function_parameters"])
                            switch (pi.Type)
                            {
                                case "int":
                                    pi.Size = 4;
                                    break;
                                case "float":
                                    pi.Size = 8;
                                    break;
                                default:
                                    // look up the class and get its size
                                    SymbolTable.Information info = Global.Find(pi.Type, false);
                                    if (info == null)
                                    {
                                        Warning("Could not find the class " + pi.Type + ", assuming its size is 0");
                                        pi.Size = 0;
                                    }
                                    else
                                        pi.Size = (int)info.Properties["class_size"];
                                    break;
                            }
                        break;
                    case SymbolTable.Information.EKind.Variable:
                        if (!kvp.Value.Properties.ContainsKey("variable_size"))
                        {
                            string type = (string)kvp.Value.Properties["variable_type"];
                            switch (type)
                            {
                                case "int":
                                    kvp.Value.Properties.Add("variable_size", 4);
                                    break;
                                case "float":
                                    kvp.Value.Properties.Add("variable_size", 8);
                                    break;
                                default:
                                    // look up the class and get its size
                                    SymbolTable.Information info = Global.Find(type, false);
                                    if (info == null)
                                    {
                                        Warning("Could not find the class " + type + ", assuming its size is 0");
                                        kvp.Value.Properties.Add("variable_size", 0);
                                    }
                                    else
                                        kvp.Value.Properties.Add("variable_size", (int)info.Properties["class_size"]);
                                    break;
                            }
                        }
                        break;
                }
        }

        /// <summary>w
        /// Tests whether the lookahead is in the token list
        /// </summary>
        /// <param name="tokens">The list of tokens given as either string, Lexer.Token.TokenType or Lexer.Token</param>
        /// <returns>A bool representing whether or not the lookahead is in the list</returns>
        protected bool ValidLookAhead(object[] tokens)
        {
            foreach (object o in tokens)
                if (o is string)
                {
                    if (lookahead.Value == (string)o)
                        return true;
                }
                else if (o is Lexer.Token.TokenType)
                {
                    if (lookahead.Type == (Lexer.Token.TokenType)o)
                        return true;
                }
                else if (o is Lexer.Token)
                {
                    if (lookahead.Type == ((Lexer.Token)o).Type && lookahead.Value == ((Lexer.Token)o).Value)
                        return true;
                }
                else
                    throw new InvalidDataException(
                        "Encountered an invalid token type in the first/follow token list");

            return false;
        }

        /// <summary>
        /// Skips all the errors until the next (maybe valid) token
        /// </summary>
        /// <param name="tokens">The list of "valid" tokens</param>
        /// <returns>true if there has been an error that was skipped, false otherwise</returns>
        protected bool SkipErrors(int p, params object[] tokens)
        {
            if (ValidLookAhead(tokens))
                return false;

            if (p == 1)
                SyntaxError(string.Format("{0}:unexpected token '{1}' of type '{2}'",
                    lookahead.Line, lookahead.Value, lookahead.Type));

            do
            {
                if (lookahead.Type == Lexer.Token.TokenType.EndOfFile)
                    break;
                lookahead = lexer.GetNextToken();
            } while (!ValidLookAhead(tokens));

            if (p == 1)
                Warning(string.Format("{0}:Resuming parsing near the token '{1}' of type '{2}'",
                    lookahead.Line, lookahead.Value, lookahead.Type));

            return true;
        }

        // <prog> ::= <classdeclstar><progbody>
        protected bool NTF_Prog(int p, SemanticInfo si)
        {
            bool error = SkipErrors(p, "class", "program", Lexer.Token.TokenType.EndOfFile);

            error |= !NTF_ClassDeclStar(p, si);
            error |= !NTF_ProgBody(p, si);

            if (!error && p == 1)
                Log(LogType.Parse, "<prog> ::= <classdeclstar><progbody>");

            return !error;
        }

        // <progbody> ::= program <funcbody>;<functionstar>
        // MODIFIED
        // <progbody> ::= program <funcbody>;<definitionstar>
        protected bool NTF_ProgBody(int p, SemanticInfo si)
        {
            bool error = SkipErrors(p, "program", Lexer.Token.TokenType.EndOfFile);

            if (p == 1)
            {
                SymbolTable.Information info;
                if ((info = CurrentScope.Find("$program", false)) != null)
                {
                    SemanticErrorSymbolAlreadyDefined("$program");
                    error = true;
                    if (info.Properties.ContainsKey("function_symtable"))
                        if (info.Properties["function_symtable"] != null)
                            CurrentScope = info.Properties["function_symtable"] as SymbolTable;
                        else
                            info.Properties["function_symtable"] = CurrentScope = CurrentScope.CreateChild("$program");
                    else
                    {
                        info.Properties.Add("function_symtable", CurrentScope.CreateChild("$program"));
                        CurrentScope = info.Properties["function_symtable"] as SymbolTable;
                    }
                }
                else
                {
                    info = CurrentScope.Add("$program");
                    info.Kind = SymbolTable.Information.EKind.Function;
                    info.Properties.Add("function_symtable", CurrentScope.CreateChild("$program"));
                    info.Properties.Add("function_parameters", new List<SemanticInfo.ParameterInfo>());
                    info.Properties.Add("function_type", "<no type>");
                    CurrentScope = info.Properties["function_symtable"] as SymbolTable;
                }
            }
            else
                CurrentScope = CurrentScope.Find("$program", false).Properties["function_symtable"] as SymbolTable;

            if (p == 2)
                si.Code.Append("entry\n\nalign\naddi r14,r0,0\n");

            error |= !Match(p, "program");
            error |= !NTF_FuncBody(p, si);
            error |= !Match(p, Lexer.Token.TokenType.Semicolon);

            si.Code.Append("hlt\n");

            CurrentScope = CurrentScope.Parent;

            error |= !NTF_DefinitionStar(p, si);

            if (!error && p == 1)
                Log(LogType.Parse, "<progbody> ::= program <funcbody> ; <definitionstar>");

            return !error;
        }

        /*
                // <functionstar> ::= e | <function><functionstar>
                protected bool NTF_FunctionStar()
                {
                    // first(<functionstar>)=first(<function>)=first(<funchead>)=first(<type>)
                    if (lookahead.Value == "int" || lookahead.Value == "float" || lookahead.Type == Lexer.Token.TokenType.Identifier)
                        if (NTF_Function() && NTF_FunctionStar())
                        {
                            Log("<functionstar> ::= <function><functionstar>");
                            return true;
                        }
                        else
                            return false;
                    // follow(<functionstar>)=}+$
                    else if (lookahead.Type == Lexer.Token.TokenType.CloseBrace || lookahead.Type == Lexer.Token.TokenType.EndOfFile)
                    {
                        Log("<functionstar> ::= e");
                        return true;
                    }
                    else
                        return false;
                }

                // <function> ::= <funchead><funcbody>;
                protected bool NTF_Function()
                {
                    if (NTF_FuncHead() && NTF_FuncBody() && Match(Lexer.Token.TokenType.Semicolon))
                    {
                        Log("<function> ::= <funchead><funcbody>;");
                        return true;
                    }
                    else
                        return false;
                }
        */

        // <funcbody> ::= {<typedeclstar><statementstar>}
        protected bool NTF_FuncBody(int p, SemanticInfo si)
        {
            bool error = SkipErrors(p, Lexer.Token.TokenType.OpenBrace, Lexer.Token.TokenType.Semicolon);

            error |= !Match(p, Lexer.Token.TokenType.OpenBrace);
            error |= !NTF_TypeDeclStar(p);
            error |= !NTF_StatementStar(p, si);
            error |= !Match(p, Lexer.Token.TokenType.CloseBrace);

            if (!error && p == 1)
                Log(LogType.Parse, "<funcbody> ::= {<typedeclstar><statementstar>}");

            return !error;
        }

        // <statementstar> ::= e | <statement><statementstar>
        protected bool NTF_StatementStar(int p, SemanticInfo si)
        {
            bool error = SkipErrors(p, "if", "for", "cin", "cout", "return", Lexer.Token.TokenType.Identifier,
                Lexer.Token.TokenType.CloseBrace);

            // first(<statement>)=first(<variable>)+if+for+cin+cout+return
            //                   =first(<idnest>)+id+if+for+cin+cout+return
            //                   =id+if+for+cin+cout+return
            if (lookahead.Value == "if" || lookahead.Value == "for" || lookahead.Value == "cin" || lookahead.Value == "cout"
                || lookahead.Value == "return" || lookahead.Type == Lexer.Token.TokenType.Identifier)
            {
                error |= !NTF_Statement(p, si);
                error |= !NTF_StatementStar(p, si);

                if (!error && p == 1)
                    Log(LogType.Parse, "<statementstar> ::= <statement><statementstar>");
            }
            // follow(<statementstar>)=}
            else if (lookahead.Type == Lexer.Token.TokenType.CloseBrace)
            {
                if (p == 1)
                    Log(LogType.Parse, "<statementstar> ::= e");
            }
            else
                error = true;

            return !error;
        }

        // <statement> ::= <variable> := <expr>
        //               | if ( <expr> ) then <statblock> else <statblock> fi ;
        //               | for ( <statement> ; <expr> ; <statement> ) <statblock> ;
        //               | cin ( <variable> ) ;
        //               | cout ( <expr> ) ;
        //               | return ( <expr> ) ;
        protected bool NTF_Statement(int p, SemanticInfo si)
        {
            bool error = SkipErrors(p, Lexer.Token.TokenType.Identifier, "if", "for", "cin", "cout", "return",
                Lexer.Token.TokenType.CloseBrace, Lexer.Token.TokenType.Semicolon, "else", "fi");

            SemanticInfo si2, si3, si4, si5;

            // first(<variable>)
            if (lookahead.Type == Lexer.Token.TokenType.Identifier)
            {
                si2 = new SemanticInfo();
                error |= !NTF_Variable(p, si2);
                error |= !Match(p, Lexer.Token.TokenType.Assignement);
                si3 = new SemanticInfo();
                error |= !NTF_Expr(p, si3);
                error |= !Match(p, Lexer.Token.TokenType.Semicolon);

                if (p == 2)
                {
                    // check to see if the types match
                    if (si2.Type != si3.Type)
                        SemanticErrorWrongTypeExpected(si3.Type, si2.Type);

                    // paste si3.code and store the offset, after which you free the temp var if necessary
                    si.Code.Append(si3.Code);
                    si.Code.Append("addi r1,r14," + si3.Offset + "\nlw r2,stack(r1)\naddi r1,r14," + si2.Offset +
                        "\nsw stack(r1), r2\n");
                }

                if (!error && p == 1)
                    Log(LogType.Parse, "<statement> ::= <variable> := <expr>");
            }
            else if (lookahead.Value == "if")
            {
                error |= !Match(p, "if");
                error |= !Match(p, Lexer.Token.TokenType.OpenBracket);
                si2 = new SemanticInfo();
                error |= !NTF_Expr(p, si2);
                error |= !Match(p, Lexer.Token.TokenType.CloseBracket);
                error |= !Match(p, "then");
                si3 = new SemanticInfo();
                error |= !NTF_StatBlock(p, si3);
                error |= !Match(p, "else");
                si4 = new SemanticInfo();
                error |= !NTF_StatBlock(p, si4);
                error |= !Match(p, "fi");
                error |= !Match(p, Lexer.Token.TokenType.Semicolon);

                if (p == 2)
                {
                    // test expression
                    si.Code.Append(si2.Code);

                    // jump
                    string j1 = GetNewJumpLabel();
                    si.Code.Append("addi r2,r14," + si2.Offset + "\nlw r1,stack(r2)\nbz r1, " + j1 + "\n");

                    // then code
                    si.Code.Append(si3.Code);

                    // exit if
                    string j2 = GetNewJumpLabel();
                    si.Code.Append("j " + j2 + "\n");

                    // else code
                    si.Code.Append(j1 + " nop\n");
                    si.Code.Append(si4.Code);

                    // finish up
                    si.Code.Append(j2 + " nop\n");
                }

                if (!error && p == 1)
                    Log(LogType.Parse, "<statement> ::= if ( <expr> ) then <statblock> else <statblock> fi ;");
            }
            else if (lookahead.Value == "for")
            {
                error |= !Match(p, "for");
                error |= !Match(p, Lexer.Token.TokenType.OpenBracket);
                si2 = new SemanticInfo();
                error |= !NTF_Statement(p, si2);
                error |= !Match(p, Lexer.Token.TokenType.Semicolon);
                si3 = new SemanticInfo();
                error |= !NTF_Expr(p, si3);
                error |= !Match(p, Lexer.Token.TokenType.Semicolon);
                si4 = new SemanticInfo();
                error |= !NTF_Statement(p, si4);
                error |= !Match(p, Lexer.Token.TokenType.CloseBracket);
                si5 = new SemanticInfo();
                error |= !NTF_StatBlock(p, si5);
                error |= !Match(p, Lexer.Token.TokenType.Semicolon);

                if (p == 2)
                {
                    // init
                    si.Code.Append(si2.Code);

                    // add the label
                    string j1 = GetNewJumpLabel();
                    si.Code.AppendLine(j1 + " nop");

                    // test
                    si.Code.Append(si3.Code);
                    string j2 = GetNewJumpLabel();
                    si.Code.AppendLine("addi r2,r14," + si3.Offset + "\nlw r1,stack(r2)\nbz r1," + j2);

                    // statements and increment
                    si.Code.Append(si5.Code);
                    si.Code.Append(si4.Code);

                    // jump back to the test
                    si.Code.AppendLine("j " + j1);

                    // exit label
                    si.Code.AppendLine(j2 + " nop");
                }

                if (!error && p == 1)
                    Log(LogType.Parse, "<statement> ::= for ( <statement> ; <expr> ; <statement> ) <statblock> ;");
            }
            else if (lookahead.Value == "cin")
            {
                error |= !Match(p, "cin");
                error |= !Match(p, Lexer.Token.TokenType.OpenBracket);
                si2 = new SemanticInfo();
                error |= !NTF_Variable(p, si2);
                error |= !Match(p, Lexer.Token.TokenType.CloseBracket);
                error |= !Match(p, Lexer.Token.TokenType.Semicolon);

                if (p == 2)
                {
                    int[] tmp = CompilerStack.Peek().GenerateBackupRegisters(si.Code);
                    si.Code.Append("jl r15,getint\naddi r2,r14," + si2.Offset + "\nsw stack(r2),r1\n");
                    CompilerStack.Peek().GenerateRestoreRegisters(si.Code, tmp);
                }

                if (!error && p == 1)
                    Log(LogType.Parse, "<statement> ::= cin ( <variable> ) ;");
            }
            else if (lookahead.Value == "cout")
            {
                error |= !Match(p, "cout");
                error |= !Match(p, Lexer.Token.TokenType.OpenBracket);
                si2 = new SemanticInfo();
                error |= !NTF_Expr(p, si2);
                error |= !Match(p, Lexer.Token.TokenType.CloseBracket);
                error |= !Match(p, Lexer.Token.TokenType.Semicolon);

                if (p == 2)
                {
                    // append si2 and load the temp register into r1 before calling cout
                    si.Code.Append(si2.Code);
                    int[] tmp = CompilerStack.Peek().GenerateBackupRegisters(si.Code);
                    si.Code.Append("addi r2,r14," + si2.Offset + "\nlw r1,stack(r2)\n");
                    si.Code.Append("jl r15,putint\n");
                    CompilerStack.Peek().GenerateRestoreRegisters(si.Code, tmp);
                }

                if (!error && p == 1)
                    Log(LogType.Parse, "<statement> ::= cout ( <expr> ) ;");
            }
            else if (lookahead.Value == "return")
            {
                error |= !Match(p, "return");
                error |= !Match(p, Lexer.Token.TokenType.OpenBracket);
                si2 = new SemanticInfo();
                error |= !NTF_Expr(p, si2);
                error |= !Match(p, Lexer.Token.TokenType.CloseBracket);
                error |= !Match(p, Lexer.Token.TokenType.Semicolon);

                if (p == 2)
                {
                    si.Code.Append(si2.Code);
                    // store the return in r1
                    // make sure the return type matches
                    SymbolTable.Information info = CurrentScope.Parent.Find(CurrentScope.ID, false);
                    if (info.Properties["function_type"] as string != si2.Type)
                    {
                        error = true;
                        SemanticErrorWrongReturnTypeExpected
                            (si.Name, si2.Type, info.Properties["function_type"] as string);
                    }
                    else
                    {
                        // copy it in r1
                        si.Code.Append("addi r2,r14," + si2.Offset + "\nlw r1,stack(r2)\njr r15\n");
                    }
                }

                if (!error && p == 1)
                    Log(LogType.Parse, "<statement> ::= return ( <expr> ) ;");
            }
            else
                error = true;

            return !error;
        }

        // <statblock> ::= { <statementstar> } | <statement> | e
        protected bool NTF_StatBlock(int p, SemanticInfo si)
        {
            bool error = SkipErrors(p, Lexer.Token.TokenType.OpenBrace, "if", "for", "cin", "cout", "return", Lexer.Token.TokenType.Identifier,
                "else", "fi", Lexer.Token.TokenType.Semicolon);

            if (lookahead.Type == Lexer.Token.TokenType.OpenBrace)
            {
                error |= !Match(p, Lexer.Token.TokenType.OpenBrace);
                error |= !NTF_StatementStar(p, si);
                error |= !Match(p, Lexer.Token.TokenType.CloseBrace);

                if (!error && p == 1)
                    Log(LogType.Parse, "<statblock> ::= { <statementstar> }");
            }
            // first(<statementstar>)
            else if (lookahead.Value == "if" || lookahead.Value == "for" || lookahead.Value == "cin" || lookahead.Value == "cout"
                || lookahead.Value == "return" || lookahead.Type == Lexer.Token.TokenType.Identifier)
            {
                error |= !NTF_Statement(p, si);

                if (!error && p == 1)
                    Log(LogType.Parse, "<statblock> ::= <statement>");
            }
            // follow(<statblock>)=else+fi+;
            else if (lookahead.Value == "else" || lookahead.Value == "fi" || lookahead.Type == Lexer.Token.TokenType.Semicolon)
            {
                if (p == 1)
                    Log(LogType.Parse, "<statblock> ::= e");
            }
            else
                error = true;

            return !error;
        }

        // <expr> ::= <arithexpr><expr2>
        protected bool NTF_Expr(int p, SemanticInfo si)
        {
            bool error = SkipErrors(p, "+", "-", Lexer.Token.TokenType.Identifier, Lexer.Token.TokenType.Number,
                Lexer.Token.TokenType.OpenBracket, Lexer.Token.TokenType.Not,
                Lexer.Token.TokenType.Semicolon, Lexer.Token.TokenType.CloseBracket);

            error |= !NTF_ArithExpr(p, si);
            error |= !NTF_Expr2(p, si);

            if (!error && p == 1)
                Log(LogType.Parse, "<expr> ::= <arithexpr> <expr2>");

            return !error;
        }

        // <arithexpr> ::= <term> <arithexpr2> | <sign> <term> <arithexpr2>
        protected bool NTF_ArithExpr(int p, SemanticInfo si)
        {
            bool error = SkipErrors(p, "+", "-", Lexer.Token.TokenType.Identifier, Lexer.Token.TokenType.Number,
                Lexer.Token.TokenType.OpenBracket, Lexer.Token.TokenType.Not,
                Lexer.Token.TokenType.Comparison, Lexer.Token.TokenType.Additive, Lexer.Token.TokenType.CloseSquareBracket,
                Lexer.Token.TokenType.Comma, Lexer.Token.TokenType.CloseBracket, Lexer.Token.TokenType.Semicolon);

            // first(<term>)=first(<variable>)+num+(+!
            if (lookahead.Type == Lexer.Token.TokenType.Identifier || lookahead.Type == Lexer.Token.TokenType.Number
                || lookahead.Type == Lexer.Token.TokenType.OpenBracket || lookahead.Type == Lexer.Token.TokenType.Not)
            {
                error |= !NTF_Term(p, si);
                error |= !NTF_ArithExpr2(p, si);

                if (!error && p == 1)
                    Log(LogType.Parse, "<arithexpr> ::= <term> <arithexpr2>");
            }
            else if (lookahead.Value == "+" || lookahead.Value == "-")
            {
                SemanticInfo si2 = new SemanticInfo();
                error |= !NTF_Sign(p, si2);
                error |= !NTF_Term(p, si);
                si.Code.Append(si2.Code);
                error |= !NTF_ArithExpr2(p, si);

                if (!error && p == 1)
                    Log(LogType.Parse, "<arithexpr> ::= <sign> <term> <arithexpr2>");
            }
            else
                error = true;

            return !error;
        }

        // <sign> ::= + | -
        protected bool NTF_Sign(int p, SemanticInfo si)
        {
            bool error = SkipErrors(p, "+", "-",
                Lexer.Token.TokenType.Identifier, Lexer.Token.TokenType.Number, Lexer.Token.TokenType.OpenBracket,
                Lexer.Token.TokenType.Not);

            if (lookahead.Value == "+")
            {
                error |= !Match(p, "+");

                if (!error && p == 1)
                    Log(LogType.Parse, "<sign> ::= +");
            }
            else
            {
                error |= !Match(p, "-");

                // TODO: fix unary -
                if (p == 2)
                    SemanticError("unary - not working yet");

                if (!error && p == 1)
                    Log(LogType.Parse, "<sign> ::= -");
            }

            return false;
        }

        // <arithexpr2> ::= e | addop <term><arithexpr2>
        protected bool NTF_ArithExpr2(int p, SemanticInfo si)
        {
            bool error = SkipErrors(p, Lexer.Token.TokenType.Additive,
                Lexer.Token.TokenType.Semicolon, Lexer.Token.TokenType.CloseBracket, Lexer.Token.TokenType.Comparison,
                Lexer.Token.TokenType.CloseSquareBracket, Lexer.Token.TokenType.Comma);

            // first(<arithexpr2>)=addop
            if (lookahead.Type == Lexer.Token.TokenType.Additive)
            {
                string op = lookahead.Value;
                if (op == "+") op = "add";
                else if (op == "-") op = "sub";
                else if (op == "||") op = "or";
                error |= !Match(p, Lexer.Token.TokenType.Additive);
                SemanticInfo si2 = new SemanticInfo();
                error |= !NTF_Term(p, si2);
                if (p == 2)
                {
                    if (si.Type == "int" && si2.Type == "int")
                    {
                        // TODO: handle logical or (and and)
                        // paste si2 then add the 2 together freeing up the temp vars if needed
                        si.Code.Append(si2.Code);
                        si.Code.Append("addi r3,r14," + si.Offset + "\nlw r1,stack(r3)\n" +
                            "addi r3,r14," + si2.Offset + "\nlw r2,stack(r3)\n" + op + " r1,r1,r2\n");

                        CompilerStack.Peek().SafeFreeTemp(si.Offset);
                        CompilerStack.Peek().SafeFreeTemp(si2.Offset);

                        int tmp = CompilerStack.Peek().RequestTemp() + CompilerStack.Peek().GetTotalRealOffset();
                        si.Code.Append("addi r2,r14," + tmp + "\nsw stack(r2),r1\n");
                        si.Offset = tmp;
                    }
                }
                error |= !NTF_ArithExpr2(p, si);

                if (!error && p == 1)
                    Log(LogType.Parse, "<arithexpr2> ::= addop <term> <arithexpr2>");
            }
            // follow(<arithexpr2>)=follow(<arithexpr>)=follow(<expr>)+relop+addop+]+,+follow(<aparams>)
            //                     =;+)+relop+addop+]+,
            else if (lookahead.Type == Lexer.Token.TokenType.Semicolon || lookahead.Type == Lexer.Token.TokenType.CloseBracket
                || lookahead.Type == Lexer.Token.TokenType.Additive || lookahead.Type == Lexer.Token.TokenType.Comparison
                || lookahead.Type == Lexer.Token.TokenType.CloseSquareBracket || lookahead.Type == Lexer.Token.TokenType.Comma)
            {
                if (p == 1)
                    Log(LogType.Parse, "<arithexpr2> ::= e");
            }
            else
                error = true;

            return !error;
        }

        // <term> ::= <factor> <term2>
        protected bool NTF_Term(int p, SemanticInfo si)
        {
            bool error = SkipErrors(p, Lexer.Token.TokenType.Identifier, Lexer.Token.TokenType.OpenBracket, Lexer.Token.TokenType.Not,
                Lexer.Token.TokenType.Number,
                Lexer.Token.TokenType.Multiplicative, Lexer.Token.TokenType.Comma, Lexer.Token.TokenType.CloseBracket,
                Lexer.Token.TokenType.Comparison, Lexer.Token.TokenType.Additive, Lexer.Token.TokenType.Semicolon);

            error |= !NTF_Factor(p, si);
            error |= !NTF_Term2(p, si);

            if (!error && p == 1)
                Log(LogType.Parse, "<term> ::= <factor> <term2>");

            return !error;
        }

        // <term2> ::= e | multop <factor> <term2>
        protected bool NTF_Term2(int p, SemanticInfo si)
        {
            bool error = SkipErrors(p, Lexer.Token.TokenType.Multiplicative,
                Lexer.Token.TokenType.Semicolon, Lexer.Token.TokenType.CloseBracket, Lexer.Token.TokenType.Additive,
                Lexer.Token.TokenType.Comparison, Lexer.Token.TokenType.CloseSquareBracket, Lexer.Token.TokenType.Comma);

            if (lookahead.Type == Lexer.Token.TokenType.Multiplicative)
            {
                string op = lookahead.Value;
                if (op == "*") op = "mul";
                else if (op == "/") op = "div";
                else if (op == "&&") op = "and";
                error |= !Match(p, Lexer.Token.TokenType.Multiplicative);

                SemanticInfo si2 = new SemanticInfo();
                error |= !NTF_Factor(p, si2);

                if (p == 2)
                {
                    // TODO: handle more types
                    if (si.Type == "int" && si2.Type == "int")
                    {
                        // TODO: handle logical or (and and)
                        // paste si2 then add the 2 together freeing up the temp vars if needed
                        si.Code.Append(si2.Code);
                        si.Code.Append("addi r3,r14," + si.Offset + "\nlw r1,stack(r3)\n" +
                            "addi r3,r14," + si2.Offset + "\nlw r2,stack(r3)\n" + op + " r1,r1,r2\n");

                        CompilerStack.Peek().SafeFreeTemp(si.Offset);
                        CompilerStack.Peek().SafeFreeTemp(si2.Offset);

                        int tmp = CompilerStack.Peek().RequestTemp() + CompilerStack.Peek().GetTotalRealOffset();
                        si.Code.Append("addi r2,r14," + tmp + "\nsw stack(r2),r1\n");
                        si.Offset = tmp;
                    }
                    else
                    {
                        if (si.Type != "int")
                            SemanticErrorWrongType(si.Name);
                        if (si2.Type != "int")
                            SemanticErrorWrongType(si2.Name);

                        error = true;
                    }
                }


                error |= !NTF_Term2(p, si);

                if (!error && p == 1)
                    Log(LogType.Parse, "<term2> ::= multop <factor> <term2>");
            }
            // follow(<term2>)=follow(<term>)=follow(<arithexpr>)=;+)+relop+addop+]+,
            else if (lookahead.Type == Lexer.Token.TokenType.Semicolon || lookahead.Type == Lexer.Token.TokenType.CloseBracket
                || lookahead.Type == Lexer.Token.TokenType.Additive || lookahead.Type == Lexer.Token.TokenType.Comparison
                || lookahead.Type == Lexer.Token.TokenType.CloseSquareBracket || lookahead.Type == Lexer.Token.TokenType.Comma)
            {
                if (p == 1)
                    Log(LogType.Parse, "<term2> ::= e");
            }
            else
                error = true;

            return !error;
        }

        // <factor> ::= <idnest> <factor2>
        //            | num
        //            | ( <expr> )
        //            | ! <factor>
        protected bool NTF_Factor(int p, SemanticInfo si)
        {
            bool error = SkipErrors(p, Lexer.Token.TokenType.Identifier, Lexer.Token.TokenType.Number, Lexer.Token.TokenType.OpenBracket,
                Lexer.Token.TokenType.Not, Lexer.Token.TokenType.Multiplicative, Lexer.Token.TokenType.Comparison,
                Lexer.Token.TokenType.Additive, Lexer.Token.TokenType.Comma, Lexer.Token.TokenType.CloseBracket,
                Lexer.Token.TokenType.Semicolon);

            // first(<idneststar>)=id
            if (lookahead.Type == Lexer.Token.TokenType.Identifier)
            {
                error |= !NTF_IdNest(p, si);
                error |= !NTF_Factor2(p, si);

                if (!error && p == 1)
                    Log(LogType.Parse, "<factor> ::= <idneststar> id <factor2>");
            }
            else if (lookahead.Type == Lexer.Token.TokenType.Number)
            {
                if (p == 2)
                {
                    // load the number
                    si.Name = lookahead.Value;
                    si.Type = lookahead.Value.IndexOf('.') >= 0 ? "float" : "int";
                    // TODO: make it work with floats
                    si.Offset = CompilerStack.Peek().GetTotalRealOffset() + CompilerStack.Peek().RequestTemp();
                    si.Code.Append("addi r1,r0," + lookahead.Value + "\naddi r2,r14," + si.Offset + "\nsw stack(r2),r1\n");
                }
                error |= !Match(p, Lexer.Token.TokenType.Number);

                if (!error && p == 1)
                    Log(LogType.Parse, "<factor> ::= num");
            }
            else if (lookahead.Type == Lexer.Token.TokenType.OpenBracket)
            {
                error |= !Match(p, Lexer.Token.TokenType.OpenBracket);
                error |= !NTF_Expr(p, si);
                error |= !Match(p, Lexer.Token.TokenType.CloseBracket);

                if (!error && p == 1)
                    Log(LogType.Parse, "<factor> ::= ( <expr> )");
            }
            else if (lookahead.Type == Lexer.Token.TokenType.Not)
            {
                error |= !Match(p, Lexer.Token.TokenType.Not);

                SemanticInfo si2 = new SemanticInfo();
                error |= !NTF_Factor(p, si2);

                if (p == 2)
                {
                    si.Code.Append(si2.Code);
                    si.Code.Append("addi r2,r14," + si2.Offset + "\nlw r1,stack(r2)\nnot r1,r1\n");
                    CompilerStack.Peek().SafeFreeTemp(si2.Offset);

                    string jmp_zero = GetNewJumpLabel();
                    si.Code.Append("bz r1," + jmp_zero + "\naddi r1,r0,1\n" + jmp_zero + " nop\n");

                    si.Offset = CompilerStack.Peek().RequestTemp();
                    si.Code.Append("addi r2,r14," + si.Offset + "\nsw stack(r2),r1\n");
                }

                if (!error && p == 1)
                    Log(LogType.Parse, "<factor> ::= ! <factor>");
            }
            else
                error = true;

            return !error;
        }

        // <factor2> ::= <indicestar>
        //             | ( <aparams> )
        protected bool NTF_Factor2(int p, SemanticInfo si)
        {
            bool error = SkipErrors(p, Lexer.Token.TokenType.OpenSquareBracket, Lexer.Token.TokenType.Multiplicative,
                Lexer.Token.TokenType.Additive, Lexer.Token.TokenType.Comma, Lexer.Token.TokenType.CloseBracket,
                Lexer.Token.TokenType.Comparison, Lexer.Token.TokenType.Semicolon, Lexer.Token.TokenType.OpenBracket);

            // first(<factor2>.1)=first(<indicestar>)+follow(<factor2>)
            // =[+multop+addop+,+)+;
            if (lookahead.Type == Lexer.Token.TokenType.OpenSquareBracket || lookahead.Type == Lexer.Token.TokenType.Multiplicative
                || lookahead.Type == Lexer.Token.TokenType.Additive || lookahead.Type == Lexer.Token.TokenType.Comma
                || lookahead.Type == Lexer.Token.TokenType.CloseBracket || lookahead.Type == Lexer.Token.TokenType.Comparison
                || lookahead.Type == Lexer.Token.TokenType.Semicolon)
            {
                error |= !NTF_IndiceStar(p, si);

                if (p == 2)
                {
                    SymbolTable st = CurrentScope;
                    SymbolTable.Information info;

                    si.Offset = 0;

                    foreach (SemanticInfo.ParameterInfo pi in si.Parameters)
                    {
                        info = st.Find(pi.Name, true);
                        if (info == null || info.Kind != SymbolTable.Information.EKind.Variable)
                        {
                            SemanticErrorVariableNotFound(pi.Name);
                            return false;
                        }

                        si.Offset += (int)info.Properties["variable_offset"];

                        info = Global.Find(info.Properties["variable_type"] as string, false);
                        if (info == null || info.Kind != SymbolTable.Information.EKind.Class)
                        {
                            SemanticErrorWrongType(pi.Name);
                            return false;
                        }

                        st = info.Properties["class_symtable"] as SymbolTable;
                    }

                    info = st.Find(si.Name, false);
                    if (info == null || info.Kind != SymbolTable.Information.EKind.Variable)
                    {
                        // fall through if it's not qualified and i'm in a class
                        if (si.Parameters.Count == 0 && CurrentScope != Global)
                        {
                            info = CurrentScope.Parent.Find(si.Name, false);
                            if (info == null || info.Kind != SymbolTable.Information.EKind.Variable)
                            {
                                SemanticErrorFunctionNotFound(si.Name);
                                return false;
                            }
                            else
                            {
                                // copy from r12+info.offset into temp variables
                                int tmp = CompilerStack.Peek().RequestTemp((int)info.Properties["variable_size"]);
                                for (int k = 0; k < (int)info.Properties["variable_size"]; k += 4)
                                {
                                    si.Code.Append("addi r2,r12," + 
                                        (si.Offset + (int)info.Properties["variable_offset"] + k) + "\nlw r1,stack(r2)\n" +
                                        "addi r2,r14," + (tmp + k) + "\nsw stack(r2),r1\n");
                                }

                                si.Offset = tmp;
                                si.Type = info.Properties["variable_type"] as string;

                                return error;
                            }
                        }
                        else
                        {
                            SemanticErrorFunctionNotFound(si.Name);
                            return false;
                        }
                    }

                    si.Offset += (int)info.Properties["variable_offset"];
                    si.Type = (string)info.Properties["variable_type"];
                }

                if (!error && p == 1)
                    Log(LogType.Parse, "<factor2> ::= <indicestar>");
            }
            else if (lookahead.Type == Lexer.Token.TokenType.OpenBracket)
            {
                error |= !Match(p, Lexer.Token.TokenType.OpenBracket);
                SemanticInfo si2 = new SemanticInfo();
                error |= !NTF_AParams(p, si2);
                if (p == 2)
                {
                    // this is a function call. check the symbol table for the parameters
                    // and see if they are all ok for starters
                    // resolve the function call
                    SymbolTable st = CurrentScope;
                    SymbolTable.Information info;

                    int offset = 0;

                    foreach (SemanticInfo.ParameterInfo pi in si.Parameters)
                    {
                        info = st.Find(pi.Name, true);
                        if (info == null || info.Kind != SymbolTable.Information.EKind.Variable)
                        {
                            SemanticErrorVariableNotFound(pi.Name);
                            return false;
                        }

                        offset += (int)info.Properties["variable_offset"];

                        info = Global.Find(info.Properties["variable_type"] as string, false);
                        if (info == null || info.Kind != SymbolTable.Information.EKind.Class)
                        {
                            SemanticErrorWrongType(pi.Name);
                            return false;
                        }

                        st = info.Properties["class_symtable"] as SymbolTable;
                    }

                    info = st.Find(si.Name, si.Parameters.Count == 0, out st);
                    if (info == null || info.Kind != SymbolTable.Information.EKind.Function)
                    {
                        SemanticErrorFunctionNotFound(si.Name);
                        return false;
                    }

                    string fnname = GetFunctionLabelName(st, si.Name);

                    if (info.Kind != SymbolTable.Information.EKind.Function)
                    {
                        SemanticErrorWrongType(si.Name);
                        error = true;
                    }
                    else
                    {
                        // look up the parameters in the symbol table
                        List<SemanticInfo.ParameterInfo> realparams = (List<SemanticInfo.ParameterInfo>)
                            info.Properties["function_parameters"];
                        if (realparams.Count != si2.Parameters.Count)
                        {
                            SemanticErrorWrongParameterCountExpected
                                (si.Name, si2.Parameters.Count, realparams.Count);
                            error = true;
                        }
                        else
                            for (int i = 0; i < realparams.Count; ++i)
                                if (realparams[i].Type != si2.Parameters[i].Type)
                                {
                                    SemanticErrorWrongParamterTypeExpected
                                        (si.Name, i, si2.Parameters[i].Type, realparams[i].Type);
                                    error = true;
                                    break;
                                }

                        // if everything is still ok, make the necessary calls
                        if (!error)
                        {
                            // allocate a temp var for the return value
                            int ret = CompilerStack.Peek().RequestTemp();
                            // paste the parameter code
                            foreach (SemanticInfo.ParameterInfo pi in si2.Parameters)
                                si.Code.Append(pi.Code);
                            // get the stack size
                            int stackframesize = CompilerStack.Peek().GetTotalRealOffset() + CompilerStack.Peek().GetTotalTempOffset();
                            // move the frame pointer forward (saving in r13 the old frame)
                            si.Code.Append("add r13,r0,r14\naddi r14,r14," + stackframesize + "\n");
                            // create a new stack frame
                            CompilerStack.Push(new StackFrame(this));
                            // chance the current scope
                            SymbolTable oldscope = CurrentScope;
                            CurrentScope = info.Properties["function_symtable"] as SymbolTable;
                            // save all the registers
                            int[] regs = CompilerStack.Peek().GenerateBackupRegisters(si.Code);
                            // fill the parameters
                            for (int i = 0; i < si2.Parameters.Count; ++i)
                            {
                                // find the parameter in the current scope
                                SymbolTable.Information paraminfo =
                                    CurrentScope.Find(realparams[i].Name, false);
                                // and fill in the code to copy it
                                for (int k = 0; k < realparams[i].Size; k += 4)
                                    si.Code.Append("addi r2,r13," + (si2.Parameters[i].OldFrameOffset + k) +
                                        "\nlw r1,stack(r2)\naddi r2,r14," +
                                        ((int)paraminfo.Properties["variable_offset"] + k) +
                                        "\nsw stack(r2),r1\n");
                            }
                            // the "this" pointer
                            si.Code.Append("addi r12,r13," + offset + "\n");
                            // generate the call
                            si.Code.Append("jl r15," + fnname + "\n");
                            // copy the return from r1 to stack(r14+0), ie stack(oldr14+stackframesize+0)
                            si.Code.Append("sw stack(r14),r1\n");
                            // restore the registers
                            CompilerStack.Peek().GenerateRestoreRegisters(si.Code, regs);
                            // restore the scope and the stack frame
                            CurrentScope = oldscope;
                            CompilerStack.Pop();
                            // rewind the stack pointer
                            si.Code.Append("subi r14,r14," + stackframesize + "\n");
                            // copy the return variable from stack(oldr14+stackframesize+0) to the temp var in our old stack
                            si.Code.Append("addi r2,r14," + stackframesize + "\nlw r1,stack(r2)\n" +
                                "addi r2,r14," + (ret + CompilerStack.Peek().GetTotalRealOffset()) + "\n"
                                + "sw stack(r2),r1\n");
                            // free up the parameter temp variables
                            foreach (SemanticInfo.ParameterInfo pi in si2.Parameters)
                                CompilerStack.Peek().SafeFreeTemp(pi.OldFrameOffset);
                            // and set the return type of this expression
                            si.Type = info.Properties["function_type"] as string;
                            si.Offset = ret + CompilerStack.Peek().GetTotalRealOffset();
                        }
                    }
                }

                error |= !Match(p, Lexer.Token.TokenType.CloseBracket);

                if (!error && p == 1)
                    Log(LogType.Parse, "<factor2> ::= ( <aparams> )");
            }

            return !error;
        }

        protected string GetFunctionLabelName(SymbolTable st, string name)
        {
            string o = "fn__";

            if (st.Parent != null)
                o += st.ID + "_";

            return o + name + "_";
        }

        // <aparams> ::= <arithexpr> <aparamstailstar> | e
        protected bool NTF_AParams(int p, SemanticInfo si)
        {
            bool error = SkipErrors(p, "+", "-", Lexer.Token.TokenType.Identifier, Lexer.Token.TokenType.Number,
                Lexer.Token.TokenType.OpenBracket, Lexer.Token.TokenType.Not, Lexer.Token.TokenType.CloseBracket);

            // first(<arithexpr>)=++-+first(<term>)=++-+id+num+(+!
            if (lookahead.Value == "+" || lookahead.Value == "-" || lookahead.Type == Lexer.Token.TokenType.Identifier
                || lookahead.Type == Lexer.Token.TokenType.Number || lookahead.Type == Lexer.Token.TokenType.OpenBracket
                || lookahead.Type == Lexer.Token.TokenType.Not)
            {
                SemanticInfo si2 = new SemanticInfo();
                error |= !NTF_ArithExpr(p, si2);
                if (p == 2)
                    si.Parameters.Add(new SemanticInfo.ParameterInfo
                    {
                        Code = si2.Code,
                        OldFrameOffset = si2.Offset,
                        Type = si2.Type,
                    });
                error |= !NTF_AParamsTailStar(p, si2);

                if (!error && p == 1)
                    Log(LogType.Parse, "<aparams> ::= <arithexpr> <aparamstail>");
            }
            // follow = )
            else if (lookahead.Type == Lexer.Token.TokenType.CloseBracket)
            {
                if (p == 1)
                    Log(LogType.Parse, "<aparams> ::= e");
            }
            else
                error = true;

            return !error;
        }

        // <aparamstailstar> ::= e | <aparamstail> <aparamstailstar>
        protected bool NTF_AParamsTailStar(int p, SemanticInfo si)
        {
            bool error = SkipErrors(p, Lexer.Token.TokenType.Comma, Lexer.Token.TokenType.CloseBracket);

            if (lookahead.Type == Lexer.Token.TokenType.Comma)
            {
                // TODO: handle function params
                error |= !NTF_AParamsTail(p, si);
                error |= !NTF_AParamsTailStar(p, si);

                if (!error && p == 1)
                    Log(LogType.Parse, "<aparamstailstar> ::= <apramstail> <aparamstailstar>");
            }
            // follow=)
            else if (lookahead.Type == Lexer.Token.TokenType.CloseBracket)
            {
                if (p == 1)
                    Log(LogType.Parse, "<aparamstailstar> ::= e");
            }
            else
                error = true;

            return !error;
        }

        // <apramstail> ::= , <arithexpr> | e
        protected bool NTF_AParamsTail(int p, SemanticInfo si)
        {
            bool error = SkipErrors(p, Lexer.Token.TokenType.Comma, Lexer.Token.TokenType.CloseBracket);

            if (lookahead.Type == Lexer.Token.TokenType.Comma)
            {
                SemanticInfo si2 = new SemanticInfo();
                error |= !Match(p, Lexer.Token.TokenType.Comma);
                error |= !NTF_ArithExpr(p, si2);
                if (p == 2)
                    si.Parameters.Add(new SemanticInfo.ParameterInfo
                    {
                        Code = si2.Code,
                        OldFrameOffset = si2.Offset,
                        Type = si2.Type,
                    });

                if (!error && p == 1)
                    Log(LogType.Parse, "<aparamstail> ::= , <arithexpr>");
            }
            else if (lookahead.Type == Lexer.Token.TokenType.CloseBracket)
            {
                if (p == 1)
                    Log(LogType.Parse, "<araparmstail> ::= e");
            }
            else
                error = true;

            return !error;
        }

        // <indicestar> ::= e | <indice> <indicestar>
        // TODO: care about arrays
        protected bool NTF_IndiceStar(int p, SemanticInfo si)
        {
            bool error = SkipErrors(p, Lexer.Token.TokenType.OpenSquareBracket,
                Lexer.Token.TokenType.Dot, Lexer.Token.TokenType.Assignement, Lexer.Token.TokenType.CloseBracket,
                Lexer.Token.TokenType.Multiplicative, Lexer.Token.TokenType.Additive, Lexer.Token.TokenType.Comma,
                Lexer.Token.TokenType.Comparison, Lexer.Token.TokenType.Semicolon, Lexer.Token.TokenType.OpenBracket,
                Lexer.Token.TokenType.Identifier);

            if (lookahead.Type == Lexer.Token.TokenType.OpenSquareBracket)
            {
                error |= !NTF_Indice(p, si);
                error |= !NTF_IndiceStar(p, si);

                if (!error && p == 1)
                    Log(LogType.Parse, "<indicestar> ::= <indice> <indicestar>");
            }
            // follow(indicestar)=.+:=+)+multop+addop+,+relop+;+(
            else if (lookahead.Type == Lexer.Token.TokenType.Dot || lookahead.Type == Lexer.Token.TokenType.Assignement
                || lookahead.Type == Lexer.Token.TokenType.CloseBracket || lookahead.Type == Lexer.Token.TokenType.Multiplicative
                || lookahead.Type == Lexer.Token.TokenType.Additive || lookahead.Type == Lexer.Token.TokenType.Comma
                || lookahead.Type == Lexer.Token.TokenType.Comparison || lookahead.Type == Lexer.Token.TokenType.Semicolon
                || lookahead.Type == Lexer.Token.TokenType.OpenBracket || lookahead.Type == Lexer.Token.TokenType.Identifier)
            {
                if (p == 1)
                    Log(LogType.Parse, "<indicestar> ::= e");
            }
            else
                error = true;

            return !error;
        }

        // <indice> ::= [ <arithexpr> ] | e
        protected bool NTF_Indice(int p, SemanticInfo si)
        {
            bool error = SkipErrors(p, Lexer.Token.TokenType.OpenSquareBracket,
                Lexer.Token.TokenType.Identifier, Lexer.Token.TokenType.Dot, Lexer.Token.TokenType.Assignement,
                Lexer.Token.TokenType.CloseBracket, Lexer.Token.TokenType.Multiplicative, Lexer.Token.TokenType.Additive,
                Lexer.Token.TokenType.Comma);

            if (lookahead.Type == Lexer.Token.TokenType.OpenSquareBracket)
            {
                error |= !Match(p, Lexer.Token.TokenType.OpenSquareBracket);
                // TODO: care about indices
                error |= !NTF_ArithExpr(p, si);
                error |= !Match(p, Lexer.Token.TokenType.CloseSquareBracket);

                if (!error && p == 1)
                    Log(LogType.Parse, "<indice> ::= [ <arithexpr> ]");
            }
            // follow=.+follow(variable)=.+:=+)+multop+addop+,+id
            else if (lookahead.Type == Lexer.Token.TokenType.Identifier || lookahead.Type == Lexer.Token.TokenType.Dot
                || lookahead.Type == Lexer.Token.TokenType.Assignement || lookahead.Type == Lexer.Token.TokenType.CloseBracket
                || lookahead.Type == Lexer.Token.TokenType.Multiplicative || lookahead.Type == Lexer.Token.TokenType.Additive
                || lookahead.Type == Lexer.Token.TokenType.Comma)
            {
                if (p == 1)
                    Log(LogType.Parse, "<indice> ::= e");
            }
            else
                error = true;

            return !error;
        }

        // <idnest> ::= id <idnest2>
        // made it to read the name of the method/class im calling too
        // MODIFIED 
        // <idnest> ::= id <indicestar> <idnest2>
        protected bool NTF_IdNest(int p, SemanticInfo si)
        {
            bool error = SkipErrors(p, Lexer.Token.TokenType.Identifier,
                Lexer.Token.TokenType.OpenSquareBracket, Lexer.Token.TokenType.OpenBracket, Lexer.Token.TokenType.Dot,
                Lexer.Token.TokenType.Semicolon, Lexer.Token.TokenType.CloseBracket, Lexer.Token.TokenType.Comparison,
                Lexer.Token.TokenType.Additive, Lexer.Token.TokenType.CloseSquareBracket, Lexer.Token.TokenType.Comma,
                Lexer.Token.TokenType.Assignement, Lexer.Token.TokenType.Multiplicative);

            if (p == 2)
                si.Parameters.Add(new SemanticInfo.ParameterInfo { Name = lookahead.Value });
            error |= !Match(p, Lexer.Token.TokenType.Identifier);

            error |= !NTF_IndiceStar(p, si);

            error |= !NTF_IdNest2(p, si);

            if (p == 2)
            {
                si.Name = si.Parameters.Last().Name;
                si.Parameters.RemoveAt(si.Parameters.Count - 1);
            }

            if (!error && p == 1)
                Log(LogType.Parse, "<idnest> ::= id <indicestar> <idnest2>");

            return !error;
        }

        // <idnest2> ::= <indicestar> . <idnest> | e
        // MODIFIED
        // <idnest2> ::= . <idnest> | e
        protected bool NTF_IdNest2(int p, SemanticInfo si)
        {
            bool error = SkipErrors(p, Lexer.Token.TokenType.Dot,
                Lexer.Token.TokenType.OpenSquareBracket, Lexer.Token.TokenType.OpenBracket, Lexer.Token.TokenType.Dot,
                Lexer.Token.TokenType.Semicolon, Lexer.Token.TokenType.CloseBracket, Lexer.Token.TokenType.Comparison,
                Lexer.Token.TokenType.Additive, Lexer.Token.TokenType.CloseSquareBracket, Lexer.Token.TokenType.Comma,
                Lexer.Token.TokenType.Assignement, Lexer.Token.TokenType.Multiplicative);

            // first(<idnest2>.1)=.
            if (lookahead.Type == Lexer.Token.TokenType.Dot)
            {
                error |= !Match(p, Lexer.Token.TokenType.Dot);

                SemanticInfo si2 = new SemanticInfo();

                error |= !NTF_IdNest(p, si2);

                if (p == 2)
                {
                    foreach (SemanticInfo.ParameterInfo pi in si2.Parameters)
                        si.Parameters.Add(pi);
                    si.Parameters.Add(new SemanticInfo.ParameterInfo { Name = si2.Name });
                }

                if (!error && p == 1)
                    Log(LogType.Parse, "<idnest2> ::= . <idnest>");
            }
            // follow(idnest2)=follow(idnest)=[+follow(variable)+(=[+(+:=+)+follow(factor)
            //                =[+(+:=+)+follow(term)=[+(+:=+)+addop+multop+follow(arithexpr)
            //                =;+)+relop+addop+]+,+[+(+:=+multop
            else if (lookahead.Type == Lexer.Token.TokenType.Semicolon || lookahead.Type == Lexer.Token.TokenType.CloseBracket ||
                lookahead.Type == Lexer.Token.TokenType.Comparison || lookahead.Type == Lexer.Token.TokenType.Additive ||
                lookahead.Type == Lexer.Token.TokenType.CloseSquareBracket || lookahead.Type == Lexer.Token.TokenType.Comma ||
                lookahead.Type == Lexer.Token.TokenType.OpenSquareBracket || lookahead.Type == Lexer.Token.TokenType.OpenBracket ||
                lookahead.Type == Lexer.Token.TokenType.OpenSquareBracket || lookahead.Type == Lexer.Token.TokenType.Assignement ||
                lookahead.Type == Lexer.Token.TokenType.Multiplicative)
            {
                if (p == 1)
                    Log(LogType.Parse, "<idnest2> ::= e");
            }
            else
                error = true;

            return !error;
        }

        // <expr2> ::= e | relop <arithexpr>
        protected bool NTF_Expr2(int p, SemanticInfo si)
        {
            bool error = SkipErrors(p, Lexer.Token.TokenType.Comparison,
                Lexer.Token.TokenType.Semicolon, Lexer.Token.TokenType.CloseBracket);

            // first(<expr2>)=relop
            if (lookahead.Type == Lexer.Token.TokenType.Comparison)
            {
                string op = lookahead.Value;
                if (op == "<") op = "clt";
                else if (op == "<=") op = "cle";
                else if (op == ">") op = "cgt";
                else if (op == ">=") op = "cge";
                else if (op == "==") op = "ceq";
                else if (op == "<>") op = "cne";
                error |= !Match(p, Lexer.Token.TokenType.Comparison);
                SemanticInfo si2 = new SemanticInfo();
                error |= !NTF_ArithExpr(p, si2);

                if (p == 2)
                {
                    si.Code.Append(si2.Code);
                    si.Code.Append("addi r3,r14," + si.Offset + "\nlw r1,stack(r3)\n" +
                        "addi r3,r14," + si2.Offset + "\nlw r2,stack(r3)\n" + op + " r1,r1,r2\n");

                    CompilerStack.Peek().SafeFreeTemp(si.Offset);
                    CompilerStack.Peek().SafeFreeTemp(si2.Offset);

                    int tmp = CompilerStack.Peek().RequestTemp() + CompilerStack.Peek().GetTotalRealOffset();
                    si.Code.Append("addi r2,r14," + tmp + "\nsw stack(r2),r1\n");
                    si.Offset = tmp;
                }

                if (!error && p == 1)
                    Log(LogType.Parse, "<expr2> ::= relop <arithexpr>");
            }
            // follow(<expr2>)=follow(<expr>)=;+)
            else if (lookahead.Type == Lexer.Token.TokenType.Semicolon || lookahead.Type == Lexer.Token.TokenType.CloseBracket)
            {
                if (p == 1)
                    Log(LogType.Parse, "<expr2> ::= e");
            }
            else
                error = true;

            return !error;
        }

        // <variable> ::= <idnest> <indicestar>
        protected bool NTF_Variable(int p, SemanticInfo si)
        {
            bool error = SkipErrors(p, Lexer.Token.TokenType.Identifier,
                Lexer.Token.TokenType.Assignement, Lexer.Token.TokenType.Multiplicative, Lexer.Token.TokenType.Additive,
                Lexer.Token.TokenType.Comparison, Lexer.Token.TokenType.Comma, Lexer.Token.TokenType.CloseBracket,
                Lexer.Token.TokenType.Semicolon);

            SemanticInfo si2 = new SemanticInfo();
            error |= !NTF_IdNest(p, si2);
            error |= !NTF_IndiceStar(p, si2);

            if (p == 2)
            {
                // find the type of this variable
                SymbolTable st = CurrentScope;
                SymbolTable.Information info;

                si.Offset = 0;

                foreach (SemanticInfo.ParameterInfo pi in si2.Parameters)
                {
                    info = st.Find(pi.Name, true);
                    if (info == null || info.Kind != SymbolTable.Information.EKind.Variable)
                    {
                        SemanticErrorVariableNotFound(pi.Name);
                        return false;
                    }

                    si.Offset += (int)info.Properties["variable_offset"];
                    info = Global.Find(info.Properties["variable_type"] as string, false);
                    if (info == null || info.Kind != SymbolTable.Information.EKind.Class)
                    {
                        SemanticErrorWrongType(pi.Name);
                        return false;
                    }

                    st = info.Properties["class_symtable"] as SymbolTable;
                }

                info = st.Find(si2.Name, si2.Parameters.Count == 0);
                if (info == null || info.Kind != SymbolTable.Information.EKind.Variable)
                {
                    SemanticErrorVariableNotFound(si.Name);
                    return false;
                }

                si.Type = (string)info.Properties["variable_type"];
                si.Offset += (int)info.Properties["variable_offset"];
            }

            if (!error && p == 1)
                Log(LogType.Parse, "<variable> ::= <idnest> <indicestar>");

            return !error;
        }

        /*
        // <funchead> ::= <type> id ( <fparams> )
        protected bool NTF_FuncHead()
        {
            if (NTF_Type() && Match(Lexer.Token.TokenType.Identifier) && Match(Lexer.Token.TokenType.OpenBracket)
                && NTF_FParams() && Match(Lexer.Token.TokenType.CloseBracket))
            {
                Log("<funchead> ::= <type> id ( <fparams> )");
                return true;
            }
            else
                return false;
        }
        */

        // <fparams> ::= <type> id <arraystar> <fparamstailstar> | e
        protected bool NTF_FParams(int p, SemanticInfo si)
        {
            bool error = SkipErrors(p, "int", "float", Lexer.Token.TokenType.Identifier,
                Lexer.Token.TokenType.CloseBracket);

            if (lookahead.Value == "int" || lookahead.Value == "float" || lookahead.Type == Lexer.Token.TokenType.Identifier)
            {
                SemanticInfo paramsi = new SemanticInfo();

                error |= !NTF_Type(p, paramsi);
                paramsi.Name = lookahead.Value;
                error |= !Match(p, Lexer.Token.TokenType.Identifier);
                error |= !NTF_ArrayStar(p, paramsi);

                // TODO: parameter redefinition
                si.Parameters.Add(new SemanticInfo.ParameterInfo
                {
                    Name = paramsi.Name,
                    Type = paramsi.Type,
                    ArrayIndices = paramsi.ArrayIndices
                });

                error |= !NTF_FParamsTailStar(p, si);

                if (!error && p == 1)
                    Log(LogType.Parse, "<fparams> ::= <type> id <arraystar> <fparamstailstar>");
            }
            else if (lookahead.Type == Lexer.Token.TokenType.CloseBracket)
            {
                if (p == 1)
                    Log(LogType.Parse, "fparams> ::= e");
            }
            else
                error = true;

            return !error;
        }

        // <fparamstailstar> ::= e | <fparamstail><fparamstairlstar>
        protected bool NTF_FParamsTailStar(int p, SemanticInfo si)
        {
            bool error = SkipErrors(p, Lexer.Token.TokenType.Comma,
                Lexer.Token.TokenType.CloseBracket);

            if (lookahead.Type == Lexer.Token.TokenType.Comma)
            {
                error |= !NTF_FParamsTail(p, si);
                error |= !NTF_FParamsTailStar(p, si);

                if (!error && p == 1)
                    Log(LogType.Parse, "<fparamstailstar> ::= <fparamstail> <fparamstailstar>");
            }
            else if (lookahead.Type == Lexer.Token.TokenType.CloseBracket)
            {
                if (p == 1)
                    Log(LogType.Parse, "<fparamstailstar> ::= e");
            }
            else
                error = true;

            return !error;
        }

        // <fparamstail> ::= e | , <type> id <arraystar>
        protected bool NTF_FParamsTail(int p, SemanticInfo si)
        {
            bool error = SkipErrors(p, Lexer.Token.TokenType.Comma,
                Lexer.Token.TokenType.CloseBracket);

            if (lookahead.Type == Lexer.Token.TokenType.Comma)
            {
                SemanticInfo paramsi = new SemanticInfo();

                error |= !Match(p, Lexer.Token.TokenType.Comma);
                error |= !NTF_Type(p, paramsi);
                paramsi.Name = lookahead.Value;
                error |= !Match(p, Lexer.Token.TokenType.Identifier);
                error |= !NTF_ArrayStar(p, paramsi);

                // TODO: parameter redefinition?
                si.Parameters.Add(new SemanticInfo.ParameterInfo
                {
                    Name = paramsi.Name,
                    Type = paramsi.Type,
                    ArrayIndices = paramsi.ArrayIndices
                });

                if (!error && p == 1)
                    Log(LogType.Parse, "<fparamstail> ::= , <type> id <arraystar>");
            }
            else if (lookahead.Type == Lexer.Token.TokenType.CloseBracket)
            {
                if (p == 1)
                    Log(LogType.Parse, "<fparamstail> ::= e");
            }
            else
                error = true;

            return !error;
        }

        // <arraystar> ::= e | <array> <arraystar>
        protected bool NTF_ArrayStar(int p, SemanticInfo si)
        {
            bool error = SkipErrors(p, Lexer.Token.TokenType.OpenSquareBracket,
                Lexer.Token.TokenType.Semicolon, Lexer.Token.TokenType.Comma, Lexer.Token.TokenType.CloseBracket);

            if (lookahead.Type == Lexer.Token.TokenType.OpenSquareBracket)
            {
                error |= !NTF_Array(p, si);
                error |= !NTF_ArrayStar(p, si);

                if (!error && p == 1)
                    Log(LogType.Parse, "<arraystar> ::= <array> <arraystar>");
            }
            // follow=;+,+)
            else
                if (lookahead.Type == Lexer.Token.TokenType.Semicolon || lookahead.Type == Lexer.Token.TokenType.Comma
                    || lookahead.Type == Lexer.Token.TokenType.CloseBracket)
                {
                    if (p == 1)
                        Log(LogType.Parse, "<arraystar> ::= e");
                }
                else
                    error = true;

            return !error;
        }

        // <array> ::= [ <int> ] | e
        protected bool NTF_Array(int p, SemanticInfo si)
        {
            bool error = SkipErrors(p, Lexer.Token.TokenType.OpenSquareBracket,
                Lexer.Token.TokenType.Comma, Lexer.Token.TokenType.CloseBracket, Lexer.Token.TokenType.Semicolon);

            if (lookahead.Type == Lexer.Token.TokenType.OpenSquareBracket)
            {
                SemanticInfo inti = new SemanticInfo();

                error |= !Match(p, Lexer.Token.TokenType.OpenSquareBracket);
                error |= !NTF_Int(p, inti);
                error |= !Match(p, Lexer.Token.TokenType.CloseSquareBracket);

                si.ArrayIndices.Add(inti.Name);

                if (!error && p == 1)
                    Log(LogType.Parse, "<array> ::= [ <int> ]");
            }
            // follow=,+)+;
            else if (lookahead.Type == Lexer.Token.TokenType.Comma || lookahead.Type == Lexer.Token.TokenType.CloseBracket
                || lookahead.Type == Lexer.Token.TokenType.Semicolon)
            {
                if (p == 1)
                    Log(LogType.Parse, "<array> ::= e");
            }
            else
                error = true;

            return !error;
        }

        protected bool NTF_Int(int p, SemanticInfo si)
        {
            bool error = SkipErrors(p, Lexer.Token.TokenType.Number,
                Lexer.Token.TokenType.CloseSquareBracket);

            if (lookahead.Type == Lexer.Token.TokenType.Number && lookahead.Value.IndexOf(".") < 0)
            {
                si.Name = lookahead.Value;
                error |= !Match(p, Lexer.Token.TokenType.Number);

                if (!error && p == 1)
                    Log(LogType.Parse, "<int> ::= int");
            }
            else
                error = true;

            return !error;
        }

        // <typedeclstar> ::= e | <typedecl><typedeclstar>
        protected bool NTF_TypeDeclStar(int p)
        {
            bool error = SkipErrors(p, "int", "float", Lexer.Token.TokenType.Identifier,
                "if", "for", "cin", "cout", "return", Lexer.Token.TokenType.CloseBrace);

            // first(<typedecl>)
            if (lookahead.Type == Lexer.Token.TokenType.Identifier || lookahead.Value == "int" || lookahead.Value == "float")
            {
                // if the next token after the lookahead isnt id, skip to ::=e
                if (lexer.PeekNextToken().Type != Lexer.Token.TokenType.Identifier)
                {
                    if (p == 1)
                        Log(LogType.Parse, "<typedeclstar> ::= e (FORCED)");
                }
                else
                {
                    error |= !NTF_TypeDecl(p);
                    error |= !NTF_TypeDeclStar(p);

                    if (!error && p == 1)
                        Log(LogType.Parse, "<typedeclstar> ::= <typedecl><typedeclstar>");
                }
            }
            // follow(<typedeclstar>)=first(<statementstar)+}+first(functionstar)
            //                       =first(<statement>)+}+first(function)
            //                       =if|for|cin|cout|return|first(<variable>)|}|first(funchead)
            //                       =if+for+cin+cout+return+id+}+int+float
            else if (lookahead.Value == "if" || lookahead.Value == "for" || lookahead.Value == "cin"
                || lookahead.Value == "cout" || lookahead.Value == "return" || lookahead.Type == Lexer.Token.TokenType.CloseBrace
                || lookahead.Type == Lexer.Token.TokenType.Identifier || lookahead.Value == "int"
                || lookahead.Value == "float")
            {
                if (p == 1)
                    Log(LogType.Parse, "<typedeclstar> ::= e");
            }
            else
                error = true;

            return !error;
        }

        // <typedecl> ::= <type> id <arraystar>;
        protected bool NTF_TypeDecl(int p)
        {
            bool error = SkipErrors(p, "int", "float", Lexer.Token.TokenType.Identifier,
                "if", "cin", "cout", "return", "for", Lexer.Token.TokenType.CloseBrace);

            SemanticInfo si = new SemanticInfo();

            error |= !NTF_Type(p, si);
            si.Name = lookahead.Value;
            error |= !Match(p, Lexer.Token.TokenType.Identifier);
            error |= !NTF_ArrayStar(p, si);
            error |= !Match(p, Lexer.Token.TokenType.Semicolon);

            if (p == 1)
            {
                SymbolTable.Information info;
                if ((info = CurrentScope.Find(si.Name, false)) != null)
                    SemanticErrorSymbolAlreadyDefined(si.Name);
                else
                {
                    info = CurrentScope.Add(si.Name);
                    info.Kind = SymbolTable.Information.EKind.Variable;
                    info.Properties.Add("variable_type", si.Type);
                    info.Properties.Add("variable_arrayindices", si.ArrayIndices.ToArray());

                    // TODO: account for objects bigger than 4 (word)
                    //info.Properties.Add("variable_offset", CompilerStack.Peek().GetTotalRealOffset());
                }
            }

            if (!error && p == 1)
                Log(LogType.Parse, "<typedecl> ::= <type> id <arraystar>;");

            return !error;
        }

        // <type> ::= int | float | id
        protected bool NTF_Type(int p, SemanticInfo si)
        {
            bool error = SkipErrors(p, "int", "float", Lexer.Token.TokenType.Identifier);

            if (lookahead.Value == "int")
            {
                si.Type = "int";
                error |= !Match(p, "int");

                if (!error && p == 1)
                    Log(LogType.Parse, "<type> ::= int");
            }
            else if (lookahead.Value == "float")
            {
                si.Type = "float";
                error |= !Match(p, "float");

                if (!error && p == 1)
                    Log(LogType.Parse, "<type> ::= float");
            }
            else if (lookahead.Type == Lexer.Token.TokenType.Identifier)
            {
                si.Type = lookahead.Value;
                error |= !Match(p, Lexer.Token.TokenType.Identifier);

                if (!error && p == 1)
                    Log(LogType.Parse, "<type> ::= id");
            }

            return !error;
        }

        // <classdeclstar> ::= e | <classdecl><classdeclstar>
        protected bool NTF_ClassDeclStar(int p, SemanticInfo si)
        {
            bool error = SkipErrors(p, "class", "program");

            if (lookahead.Value == "class")                                    // first(<classdecl>)
            {
                error |= !NTF_ClassDecl(p, si);
                error |= !NTF_ClassDeclStar(p, si);

                if (!error && p == 1)
                    Log(LogType.Parse, "<classdeclstar> ::= <classdecl><classdeclstar>");
            }
            else
                if (lookahead.Value == "program")                              // follow(<classdecl>)
                {
                    if (p == 1)
                        Log(LogType.Parse, "<classdeclstar> ::= e");
                }
                else
                    error = true;

            return !error;
        }

        // <classdecl> ::= class id { <typedeclstar> <functionstar> } ;
        // MODIFIED
        // <classdecl> ::= class id { <definitionstar> } ;
        protected bool NTF_ClassDecl(int p, SemanticInfo si)
        {
            /*
                        if (Match("class") && Match(Lexer.Token.TokenType.Identifier) && Match(Lexer.Token.TokenType.OpenBrace)
                            && NTF_TypeDeclStar() && NTF_FunctionStar() && Match(Lexer.Token.TokenType.CloseBrace)
                            && Match(Lexer.Token.TokenType.Semicolon))
                        {
                            Log("<classdecl> ::= class id {<typedeclstar><functionstar>};");
                            return true;
                        }
                        else
                            return false;
            */
            bool error = SkipErrors(p, "class", "program");

            error |= !Match(p, "class");

            if (p == 1)
            {
                SymbolTable child;
                SymbolTable.Information info;
                if ((info = CurrentScope.Find(lookahead.Value, false)) != null)
                {
                    SemanticErrorSymbolAlreadyDefined(lookahead.Value);
                    error = true;
                    if (info.Properties.ContainsKey("class_symtable"))
                        if (info.Properties["class_symtable"] != null)
                            child = info.Properties["class_symtable"] as SymbolTable;
                        else
                            info.Properties["class_symtable"] = child = CurrentScope.CreateChild(lookahead.Value);
                    else
                    {
                        info.Properties.Add("class_symtable", CurrentScope.CreateChild(lookahead.Value));
                        child = info.Properties["class_symtable"] as SymbolTable;
                    }
                }
                else
                {
                    info = CurrentScope.Add(lookahead.Value);
                    child = CurrentScope.CreateChild(lookahead.Value);

                    info.Kind = SymbolTable.Information.EKind.Class;
                    info.Properties.Add("class_symtable", child);
                }

                CurrentScope = child;
            }
            else
            {
                CurrentScope = CurrentScope.Find(lookahead.Value, false).Properties["class_symtable"] as SymbolTable;
            }

            error |= !Match(p, Lexer.Token.TokenType.Identifier);
            error |= !Match(p, Lexer.Token.TokenType.OpenBrace);
            error |= !NTF_DefinitionStar(p, si);
            error |= !Match(p, Lexer.Token.TokenType.CloseBrace);
            error |= !Match(p, Lexer.Token.TokenType.Semicolon);

            CurrentScope = CurrentScope.Parent;

            if (!error && p == 1)
                Log(LogType.Parse, "<classdecl> := class id { <definitionstar> } ;");

            return !error;
        }

        // <definitionstar> ::= e | <definition> <definitionstar>
        protected bool NTF_DefinitionStar(int p, SemanticInfo si)
        {
            bool error = SkipErrors(p, "int", "float", Lexer.Token.TokenType.Identifier, Lexer.Token.TokenType.CloseBrace,
                Lexer.Token.TokenType.EndOfFile);

            if (lookahead.Value == "int" || lookahead.Value == "float" || lookahead.Type == Lexer.Token.TokenType.Identifier)
            {
                error |= !NTF_Definition(p, si);
                error |= !NTF_DefinitionStar(p, si);

                if (!error && p == 1)
                    Log(LogType.Parse, "<definitionstar> ::= <definition> <definitionstar>");
            }
            // follow=}+$
            else if (lookahead.Type == Lexer.Token.TokenType.EndOfFile || lookahead.Type == Lexer.Token.TokenType.CloseBrace)
            {
                if (p == 1)
                    Log(LogType.Parse, "<definitionstar> ::= e");
            }
            else
                error = true;

            return !error;
        }

        // <definition> ::= <type> id <definition2>
        protected bool NTF_Definition(int p, SemanticInfo si)
        {
            bool error = SkipErrors(p, "int", "float", Lexer.Token.TokenType.Identifier, Lexer.Token.TokenType.CloseBrace,
                Lexer.Token.TokenType.EndOfFile);

            error |= !NTF_Type(p, si);

            si.Name = lookahead.Value;
            error |= !Match(p, Lexer.Token.TokenType.Identifier);

            error |= !NTF_Definition2(p, si);

            if (!error && p == 1)
                Log(LogType.Parse, "<definition> ::= <type> id <definition2>");

            return !error;
        }

        // <definition2> ::= ( <fparams> ) <funcbody> ;
        //                 | <arraystar> ;
        protected bool NTF_Definition2(int p, SemanticInfo si)
        {
            bool error = SkipErrors(p, Lexer.Token.TokenType.OpenBracket, Lexer.Token.TokenType.OpenSquareBracket,
                Lexer.Token.TokenType.Semicolon,
                Lexer.Token.TokenType.CloseBrace, Lexer.Token.TokenType.EndOfFile);

            if (lookahead.Type == Lexer.Token.TokenType.OpenBracket)
            {
                error |= !Match(p, Lexer.Token.TokenType.OpenBracket);
                SemanticInfo si2 = new SemanticInfo();
                error |= !NTF_FParams(p, si2);
                error |= !Match(p, Lexer.Token.TokenType.CloseBracket);

                if (p == 1)
                {
                    SymbolTable.Information info;
                    if ((info = CurrentScope.Find(si.Name, false)) != null)
                    {
                        SemanticErrorSymbolAlreadyDefined(si.Name);
                        error = true;

                        if (info.Properties.ContainsKey("function_symtable"))
                            if (info.Properties["function_symtable"] != null)
                                CurrentScope = info.Properties["function_symtable"] as SymbolTable;
                            else
                                info.Properties["function_symtable"] = CurrentScope = CurrentScope.CreateChild(si.Name);
                        else
                        {
                            info.Properties.Add(si.Name, CurrentScope.CreateChild(si.Name));
                            CurrentScope = info.Properties[si.Name] as SymbolTable;
                        }
                    }
                    else
                    {
                        info = CurrentScope.Add(si.Name);
                        info.Kind = SymbolTable.Information.EKind.Function;
                        info.Properties.Add("function_parameters", si2.Parameters);
                        info.Properties.Add("function_type", si.Type);
                        info.Properties.Add("function_symtable", CurrentScope.CreateChild(si.Name));
                        CurrentScope = info.Properties["function_symtable"] as SymbolTable;

                        // also add the parameters as member variables
                        foreach (SemanticInfo.ParameterInfo pi in si2.Parameters)
                        {
                            SymbolTable.Information inf = (info.Properties["function_symtable"] as SymbolTable)
                                .Add(pi.Name);
                            inf.Kind = SymbolTable.Information.EKind.Variable;
                            inf.Properties.Add("variable_type", pi.Type);
                            inf.Properties.Add("variable_arrayindices", pi.ArrayIndices.ToArray());
                            //inf.Properties.Add("variable_offset", CompilerStack.Peek().GetTotalRealOffset());
                        }
                    }
                }
                else
                {
                    si.Code.Append(si2.Code);
                    CurrentScope = CurrentScope.Find(si.Name, false).Properties["function_symtable"] as SymbolTable;
                }

                CompilerStack.Push(new StackFrame(this, true));

                // the function label
                if (p == 2)
                    si.Code.Append(GetFunctionLabelName(CurrentScope.Parent, si.Name) + " nop\n");
                error |= !NTF_FuncBody(p, si);
                error |= !Match(p, Lexer.Token.TokenType.Semicolon);
                if (p == 2)
                    si.Code.Append("% append default return\naddi r1,r0,0\njr r15\n");

                CompilerStack.Pop();

                CurrentScope = CurrentScope.Parent;

                if (!error && p == 1)
                    Log(LogType.Parse, "<definition2> ::= ( <fparams> ) <funcbody> ;");
            }
            else if (lookahead.Type == Lexer.Token.TokenType.OpenSquareBracket || lookahead.Type == Lexer.Token.TokenType.Semicolon)
            {
                error |= !NTF_ArrayStar(p, si);
                error |= !Match(p, Lexer.Token.TokenType.Semicolon);

                if (p == 1)
                {
                    SymbolTable.Information info;
                    if ((info = CurrentScope.Find(si.Name, false)) != null)
                    {
                        SemanticErrorSymbolAlreadyDefined(si.Name);
                        error = true;
                    }
                    else
                    {
                        info = CurrentScope.Add(si.Name);
                        info.Kind = SymbolTable.Information.EKind.Variable;
                        info.Properties["variable_arrayindices"] = new string[si.ArrayIndices.Count];
                        for (int i = 0; i < si.ArrayIndices.Count; ++i)
                            (info.Properties["variable_arrayindices"] as string[])[i] = si.ArrayIndices[i];
                        info.Properties["variable_type"] = si.Type;

                        // TODO: account for objects bigger than 4
                        //info.Properties.Add("variable_offset", CompilerStack.Peek().GetTotalRealOffset());
                    }
                }

                if (!error && p == 1)
                    Log(LogType.Parse, "<definition2> ::= <arraystar> ;");
            }

            return !error;
        }

        /// <summary>
        /// Tries to match a token from a type
        /// </summary>
        /// <param name="type">The type of the token to match against</param>
        /// <returns></returns>
        protected bool Match(int p, Lexer.Token.TokenType type)
        {
            return Match(p, null, type);
        }

        /// <summary>
        /// Tries to match a token from a value
        /// </summary>
        /// <param name="val">The value to match it against</param>
        /// <returns></returns>
        protected bool Match(int p, string val)
        {
            return Match(p, val, null);
        }

        /// <summary>
        /// Tries to consume a token of the given type
        /// </summary>
        /// <param name="val">The value of the token, can be null to ignore</param>
        /// <param name="type">The type of the token, can be null to ignore</param>
        /// <returns>A bool signifying if the symbol could be matched or not</returns>
        protected bool Match(int p, string val, Lexer.Token.TokenType? type)
        {
            bool ok = true;

            if (val != null && lookahead.Value != val)
                ok = false;
            else if (type.HasValue && lookahead.Type != type.Value)
                ok = false;

            if (!ok && p == 1)
                SyntaxError(string.Format("{0}: expected '{1}', got instead '{2}', a {3}",
                    lookahead.Line, val ?? type.Value.ToString(), lookahead.Value, lookahead.Type.ToString()));

            if (lookahead.Type != Lexer.Token.TokenType.EndOfFile)
                lookahead = lexer.GetNextToken();

            return ok;
        }
    }
}
