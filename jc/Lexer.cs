using System;
using System.Linq;
using System.Text;

namespace jc
{
    class Lexer
    {
        /// <summary>
        /// The buffer containing the data to analyze. The end-of-file character is simulated by
        /// positioning the cursor 1 character past the length of the string
        /// </summary>
        /// <seealso cref="Cursor"/>
        protected string Buffer { get; set; }

        /// <summary>
        /// The cursor pointing to the currect character being analyzed in the buffer
        /// </summary>
        /// <seealso cref="Buffer"/>
        public int Cursor { get; protected set; }

        /// <summary>
        /// The token last peeked at. Can be null for no token peeked.
        /// </summary>
        protected Token PeekedToken { get; set; }

        /// <summary>
        /// The current line
        /// </summary>
        public int Line { get; protected set; }

        protected Parser parser;

        /// <summary>
        /// The token class we use to return information to the caller
        /// </summary>
        public class Token
        {
            public enum TokenType
            {
                Identifier,
                Number,
                Comparison,
                Assignement,
                Additive,
                Multiplicative,
                Colon,
                Comment,
                Semicolon,
                Dot,
                Comma,
                Not,
                OpenBracket,
                CloseBracket,
                OpenBrace,
                CloseBrace,
                OpenSquareBracket,
                CloseSquareBracket,
                Reserved,
                EndOfFile,
                Nothing,
                Error
            }

            public TokenType Type;
            public string Value;
            public int Line;
        }

        public Lexer(string stream, Parser parser)
        {
            Buffer = stream;
            Reset();
            this.parser = parser;
        }

        public void Reset()
        {
            Cursor = -1;
            Line = 1;
            PeekedToken = null;
        }

        /// <summary>
        /// Advances the cursor up until we reach the end-of-file virtual marker
        /// </summary>
        protected void Advance()
        {
            if (Cursor >= Buffer.Length + 1)
                parser.Errors.Add(string.Format(
                    "Lexer error:{0}:Tried to read past the end of file. This is an internal error",
                    Line));
            else if (Cursor > 0 && Cursor < Buffer.Length)
                if (Buffer[Cursor] == '\r')
                    ++Line;

            ++Cursor;
        }

        /// <summary>
        /// Back up the cursor
        /// </summary>
        protected void Backup()
        {
            if (Cursor > 0 && Buffer[Cursor - 1] == '\r')
                --Line;

            Cursor = Math.Max(Cursor - 1, 0);
        }

        /// <summary>
        /// Retrieves the next character to analyze
        /// </summary>
        /// <returns>The character at the current position (after the advancement of the cursor)</returns>
        protected char? GetNextChar()
        {
            Advance();
            return Cursor < Buffer.Length ? (char?)Buffer[Cursor] : (char?)null;
        }

        protected class TransitionTableType
        {
            public int equals = -1, lessthan = -1, greaterthan = -1, divideby = -1, semicolon = -1, dot = -1,
                comma = -1, colon = -1, plus = -1, minus = -1, star = -1, and = -1, exclamation = -1, pipe = -1,
                openbracket = -1, closebracket = -1, openbrace = -1, closebrace = -1, opensquarebracket = -1,
                closesquarebracket = -1, letter = -1, nonzeronumber = -1, zero = -1, eoln = -1, white = -1;
            public Token.TokenType final = Token.TokenType.Nothing;
            public bool backup = false;
        }

        /// <summary>
        /// The language's state machine
        /// </summary>
        protected static TransitionTableType[] TransitionTable =
        {
            /* 0*/ new TransitionTableType {equals=1, lessthan=2, greaterthan=6, divideby=9, colon=16, letter=19, nonzeronumber=21, zero=27, semicolon=36, dot=37, comma=38, plus=39, minus=40, star=41, and=42, exclamation=44, pipe=45, openbracket=47, closebracket=48, openbrace=49, closebrace=50, opensquarebracket=51, closesquarebracket=52},
            /* 1*/ new TransitionTableType {equals=35},
            /* 2*/ new TransitionTableType {greaterthan=3, equals=4, and=5, closebrace=5, closebracket=5, closesquarebracket=5, colon=5, comma=5, divideby=5, dot=5, exclamation=5, lessthan=5, letter=5, minus=5, nonzeronumber=5, openbrace=5, openbracket=5, opensquarebracket=5, pipe=5, plus=5, semicolon=5, star=5, zero=5, eoln=5, white=5},
            /* 3*/ new TransitionTableType {final=Token.TokenType.Comparison},
            /* 4*/ new TransitionTableType {final=Token.TokenType.Comparison},
            /* 5*/ new TransitionTableType {final=Token.TokenType.Comparison, backup=true},
            /* 6*/ new TransitionTableType {greaterthan=8, equals=7, and=8, closebrace=8, closebracket=8, closesquarebracket=8, colon=8, comma=8, divideby=8, dot=8, exclamation=8, lessthan=8, letter=8, minus=8, nonzeronumber=8, openbrace=8, openbracket=8, opensquarebracket=8, pipe=8, plus=8, semicolon=8, star=8, zero=8, eoln=8, white=8},
            /* 7*/ new TransitionTableType {final=Token.TokenType.Comparison},
            /* 8*/ new TransitionTableType {final=Token.TokenType.Comparison, backup=true},
            /* 9*/ new TransitionTableType {greaterthan=15, equals=15, and=15, closebrace=15, closebracket=15, closesquarebracket=15, colon=15, comma=15, divideby=10, dot=15, exclamation=15, lessthan=15, letter=15, minus=15, nonzeronumber=15, openbrace=15, openbracket=15, opensquarebracket=15, pipe=15, plus=15, semicolon=15, star=12, zero=15, eoln=15, white=15},
            /*10*/ new TransitionTableType {greaterthan=10, equals=10, and=10, closebrace=10, closebracket=10, closesquarebracket=10, colon=10, comma=10, divideby=10, dot=10, exclamation=10, lessthan=10, letter=10, minus=10, nonzeronumber=10, openbrace=10, openbracket=10, opensquarebracket=10, pipe=10, plus=10, semicolon=10, star=10, zero=10, eoln=11, white=10},
            /*11*/ new TransitionTableType {final=Token.TokenType.Comment},
            /*12*/ new TransitionTableType {greaterthan=12, equals=12, and=12, closebrace=12, closebracket=12, closesquarebracket=12, colon=12, comma=12, divideby=12, dot=12, exclamation=12, lessthan=12, letter=12, minus=12, nonzeronumber=12, openbrace=12, openbracket=12, opensquarebracket=12, pipe=12, plus=12, semicolon=12, star=13, zero=12, eoln=12, white=12},
            /*13*/ new TransitionTableType {greaterthan=12, equals=12, and=12, closebrace=12, closebracket=12, closesquarebracket=12, colon=12, comma=12, divideby=14, dot=12, exclamation=12, lessthan=12, letter=12, minus=12, nonzeronumber=12, openbrace=12, openbracket=12, opensquarebracket=12, pipe=12, plus=12, semicolon=12, star=12, zero=12, eoln=12, white=12},
            /*14*/ new TransitionTableType {final=Token.TokenType.Comment},
            /*15*/ new TransitionTableType {final=Token.TokenType.Multiplicative, backup=true},
            /*16*/ new TransitionTableType {greaterthan=18, equals=17, and=18, closebrace=18, closebracket=18, closesquarebracket=18, colon=18, comma=18, divideby=18, dot=18, exclamation=18, lessthan=18, letter=18, minus=18, nonzeronumber=18, openbrace=18, openbracket=18, opensquarebracket=18, pipe=18, plus=18, semicolon=18, star=18, zero=18, eoln=18, white=18},
            /*17*/ new TransitionTableType {final=Token.TokenType.Assignement},
            /*18*/ new TransitionTableType {final=Token.TokenType.Colon, backup=true},
            /*19*/ new TransitionTableType {greaterthan=20, equals=20, and=20, closebrace=20, closebracket=20, closesquarebracket=20, colon=20, comma=20, divideby=20, dot=20, exclamation=20, lessthan=20, letter=19, minus=20, nonzeronumber=19, openbrace=20, openbracket=20, opensquarebracket=20, pipe=20, plus=20, semicolon=20, star=20, zero=19, eoln=20, white=20},
            /*20*/ new TransitionTableType {final=Token.TokenType.Identifier, backup=true},
            /*21*/ new TransitionTableType {greaterthan=26, equals=26, and=26, closebrace=26, closebracket=26, closesquarebracket=26, colon=26, comma=26, divideby=26, dot=22, exclamation=26, lessthan=26, letter=26, minus=26, nonzeronumber=21, openbrace=26, openbracket=26, opensquarebracket=26, pipe=26, plus=26, semicolon=26, star=26, zero=21, eoln=26, white=26},
            /*22*/ new TransitionTableType {zero=23, nonzeronumber=24},
            /*23*/ new TransitionTableType {final=Token.TokenType.Number},
            /*24*/ new TransitionTableType {greaterthan=32, equals=32, and=32, closebrace=32, closebracket=32, closesquarebracket=32, colon=32, comma=32, divideby=32, dot=32, exclamation=32, lessthan=32, letter=32, minus=32, nonzeronumber=24, openbrace=32, openbracket=32, opensquarebracket=32, pipe=32, plus=32, semicolon=32, star=32, zero=25, eoln=32, white=32},
            /*25*/ new TransitionTableType {nonzeronumber=24, zero=25},
            /*26*/ new TransitionTableType {final=Token.TokenType.Number, backup=true},
            /*27*/ new TransitionTableType {greaterthan=28, equals=28, and=28, closebrace=28, closebracket=28, closesquarebracket=28, colon=28, comma=28, divideby=28, dot=33, exclamation=28, lessthan=28, letter=28, minus=28, nonzeronumber=28, openbrace=28, openbracket=28, opensquarebracket=28, pipe=28, plus=28, semicolon=28, star=28, zero=28, eoln=28, white=28},
            /*28*/ new TransitionTableType {final=Token.TokenType.Number, backup=true},
            /*29*/ new TransitionTableType {final=Token.TokenType.Number},
            /*30*/ new TransitionTableType {greaterthan=34, equals=34, and=34, closebrace=34, closebracket=34, closesquarebracket=34, colon=34, comma=34, divideby=34, dot=34, exclamation=34, lessthan=34, letter=34, minus=34, nonzeronumber=30, openbrace=34, openbracket=34, opensquarebracket=34, pipe=34, plus=34, semicolon=34, star=34, zero=34, eoln=34, white=31},
            /*31*/ new TransitionTableType {zero=31, nonzeronumber=30},
            /*32*/ new TransitionTableType {final=Token.TokenType.Number, backup=true},
            /*33*/ new TransitionTableType {zero=29, nonzeronumber=30},
            /*34*/ new TransitionTableType {final=Token.TokenType.Number, backup=true},
            /*35*/ new TransitionTableType {final=Token.TokenType.Comparison},
            /*36*/ new TransitionTableType {final=Token.TokenType.Semicolon},
            /*37*/ new TransitionTableType {final=Token.TokenType.Dot},
            /*38*/ new TransitionTableType {final=Token.TokenType.Comma},
            /*39*/ new TransitionTableType {final=Token.TokenType.Additive},
            /*40*/ new TransitionTableType {final=Token.TokenType.Additive},
            /*41*/ new TransitionTableType {final=Token.TokenType.Multiplicative},
            /*42*/ new TransitionTableType {and=43},
            /*43*/ new TransitionTableType {final=Token.TokenType.Multiplicative},
            /*44*/ new TransitionTableType {final=Token.TokenType.Not},
            /*45*/ new TransitionTableType {pipe=46},
            /*46*/ new TransitionTableType {final=Token.TokenType.Additive},
            /*47*/ new TransitionTableType {final=Token.TokenType.OpenBracket},
            /*48*/ new TransitionTableType {final=Token.TokenType.CloseBracket},
            /*49*/ new TransitionTableType {final=Token.TokenType.OpenBrace},
            /*50*/ new TransitionTableType {final=Token.TokenType.CloseBrace},
            /*51*/ new TransitionTableType {final=Token.TokenType.OpenSquareBracket},
            /*52*/ new TransitionTableType {final=Token.TokenType.CloseSquareBracket}
        };

        /// <summary>
        /// Known reserved keywords 
        /// </summary>
        protected static string[] Identifiers =
        {
            "fi", "cout", "for", "else", "if", "int", "program", "cin", "float", "return", "then", "class"
        };

        /// <summary>
        /// Retrieves the next token by executing the state machine
        /// </summary>
        /// <returns>The next token as a Token class</returns>
        public Token GetNextToken()
        {
            Token token;

            do
            {
                token = InternalGetNextToken();
            } while (token.Type == Token.TokenType.Comment);

            return token;
        }

        /// <summary>
        /// Peeks the next token without moving the cursor
        /// </summary>
        /// <returns>The next token as a Token class</returns>
        public Token PeekNextToken()
        {
            return PeekedToken = GetNextToken();
        }

        /// <summary>
        /// Returns the next token, including comments, which must 
        /// be removed before being used by the parser. This is why this
        /// method is marked as internal and is wrapped by GetNextToken().
        /// </summary>
        /// <returns>The next token as a Token class</returns>
        protected Token InternalGetNextToken()
        {
            // handle peeking first
            if (PeekedToken != null)
            {
                Token token = PeekedToken;
                PeekedToken = null;
                return token;
            }

            int state = 0;
            char? next = GetNextChar();
            StringBuilder output = new StringBuilder();

            // eat the white space
            while (next.HasValue && char.IsWhiteSpace(next.Value))
                next = GetNextChar();

            if (!next.HasValue)
                return new Token { Line = Line, Value = "", Type = Token.TokenType.EndOfFile };

            while (true)
            {
                output.Append(next ?? ' ');
                state = GetNewState(state, next ?? '\r');

                if (state == -1)
                {
                    // error occured, skip to end of line and spit the error out
                    if (!char.IsWhiteSpace(next ?? ' '))
                    {
                        next = GetNextChar();
                        while (!char.IsWhiteSpace((next ?? ' ')))
                        {
                            output.Append(next);
                            next = GetNextChar();
                        }
                    }

                    return new Token
                        {
                            Type = Token.TokenType.Error,
                            Value = output.Remove(output.Length - 1, 1).ToString(),
                            Line = Line
                        };
                }

                if (TransitionTable[state].final != Token.TokenType.Nothing)
                {
                    // reached a final state, make the token and back up the cursor if needed
                    if (TransitionTable[state].backup)
                    {
                        Backup();
                        output.Remove(output.Length - 1, 1);
                    }

                    if (TransitionTable[state].final == Token.TokenType.Identifier && Identifiers.Contains(output.ToString()))
                        return new Token
                            {
                                Type = Token.TokenType.Reserved,
                                Value = output.ToString(),
                                Line = Line
                            };
                    else
                        return new Token
                            {
                                Type = TransitionTable[state].final,
                                Value = output.ToString(),
                                Line = Line
                            };
                }

                if (!next.HasValue)
                    break;
                else
                    next = GetNextChar();
            }

            // error, we reached the end of file
            // if we are in the middle of processing a number, its an error, otherwise its just eof
            return new Token
                {
                    Type = output.Length == 0 ? Token.TokenType.EndOfFile : Token.TokenType.Error,
                    Value = output.ToString(),
                    Line = Line
                };
        }

        /// <summary>
        /// Determines what state the automaton goes into knowing the current state and the input character
        /// </summary>
        /// <param name="state">The current state</param>
        /// <param name="p">The input character</param>
        /// <returns>The new state</returns>
        protected int GetNewState(int state, char p)
        {
            switch (p)
            {
                case '=':
                    return TransitionTable[state].equals;
                case '<':
                    return TransitionTable[state].lessthan;
                case '>':
                    return TransitionTable[state].greaterthan;
                case '/':
                    return TransitionTable[state].divideby;
                case ';':
                    return TransitionTable[state].semicolon;
                case '.':
                    return TransitionTable[state].dot;
                case ',':
                    return TransitionTable[state].comma;
                case ':':
                    return TransitionTable[state].colon;
                case '+':
                    return TransitionTable[state].plus;
                case '-':
                    return TransitionTable[state].minus;
                case '*':
                    return TransitionTable[state].star;
                case '&':
                    return TransitionTable[state].and;
                case '!':
                    return TransitionTable[state].exclamation;
                case '|':
                    return TransitionTable[state].pipe;
                case '(':
                    return TransitionTable[state].openbracket;
                case ')':
                    return TransitionTable[state].closebracket;
                case '{':
                    return TransitionTable[state].openbrace;
                case '}':
                    return TransitionTable[state].closebrace;
                case '[':
                    return TransitionTable[state].opensquarebracket;
                case ']':
                    return TransitionTable[state].closesquarebracket;
                case '0':
                    return TransitionTable[state].zero;
                default:
                    if (char.IsDigit(p))
                        return TransitionTable[state].nonzeronumber;
                    else if (char.IsLetter(p))
                        return TransitionTable[state].letter;
                    else if (p == '\n' || p == '\r')
                        return TransitionTable[state].eoln;
                    else if (char.IsWhiteSpace(p))
                        return TransitionTable[state].white;
                    else
                        parser.Errors.Add(string.Format(
                            "Lexer error:{0}:Wrong state/transition type. This is an internal error.",
                            Line));
                    return -1;
            }
        }
    }
}
