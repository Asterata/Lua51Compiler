
using Lua51Compiler;

using Loretta.CodeAnalysis;
using Loretta.CodeAnalysis.Lua;
using Loretta.CodeAnalysis.Lua.Experimental;
using Loretta.CodeAnalysis.Lua.Syntax;
using Lua51Compiler.IR;

/*
local a,b,c,g = 1,2,3,31
local d,e,f = 4,5,6
a = a - b + d + c / d
a = 123
local a,b,c = 1,2,3
local d,e,f = 4,5,6
a = a and g
K = 3131
b = print(a)
c = a or b or a
local a,b,c = 1,2,3
if a then
    print(a)
end
if a==b then
    print("EQ")
elseif a~=b then
    print("NEQ")
elseif a<b then
    print("LT")
elseif a>b then
    print("GT")
elseif a<=b then
    print("LE")
elseif a>=b then
    print("GE")
else
    print("ELSE")
end
b = print(a)
c = a or b or a
local a,b,c = 1,2,3
while a < 10 do
    print(a)
    break
end
while a < 10 do
    print(a)
    a = a + 1
    continue
end
local a = 1
repeat
    print(a)
    break
until a > 10
repeat
    print(a)
    a = a + 1
    continue
until a > 10
for i = 1, 10 do
    print(i)
    continue
end
for i = 1, 10 do
    print(i)
    break
end
local settings = {
    ["a"] = 1,
    ["b"] = 2,
    ["c"] = 3
}
local a,b,c = 1,2,3
setmetatable(settings, {
    __index = function(self, key)
        return self[key]
    end,
    __newindex = function(self, key, value)
        self[key] = value
    end
})
local a,b,c = 1,2,3
 */


var source = @"function imAFunction (...)
    return 1,2,3
end
local a,b,c = imAFunction()";

var CLIMode = source == "";
    
// check if it gets a .lua file in run parameters
if (CLIMode)
{
    if (args.Length == 1)
    {
        source = File.ReadAllText(args[0]);
    }
    else
    {
        Console.WriteLine("[ERROR] No file specified!");
        Console.WriteLine("To specify a file, run the program with the file path as a parameter");
        Console.WriteLine("Or write the code in the console!");
        source = Console.ReadLine();
    }
}


var tree = SyntaxFactory.ParseSyntaxTree(source, new LuaParseOptions(LuaSyntaxOptions.Lua51));
var root = (CompilationUnitSyntax) tree.GetRoot();
var folded = root.ConstantFold(ConstantFoldingOptions.All);
var newSource = folded.NormalizeWhitespace().ToFullString();

var compilerResult = new LuaPrototypeCompiler().CompilePrototype(newSource);

Console.WriteLine("Done!");

string CreateListing(Lua51Prototype proto)
{
    var str = "";
    str += proto.ToString();
    foreach (var p in proto.Prototypes)
    {
        str += CreateListing(p);
    }
    return str;
}
var luaListing = CreateListing(compilerResult);
Console.WriteLine(luaListing);

if (CLIMode)
{
    // stop the program from closing
    Console.WriteLine("\n\nPress any key to exit...");
    Console.ReadLine();
}