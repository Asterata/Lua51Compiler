
# Lua51Compiler
**A [Lua](https://www.lua.org/) 5.1 Compiler with extra features.**

**The current state of the compiler is not stable and its still in development, but most of the parts works as expected.**

## Extra Features
- [x] `goto` support
- [x] `continue` support
- [ ] `+= -= -- ++ etc.` support
- [ ] `& | ~ >> <<` support

## Current Known Compiler Resictictions
### **_These will be fixed in the future._**
-  `goto` will jump to the first label with the same name.
- ### Example
    ```lua
    local a = true
    t = 'pass'
    goto passEqCheck
    if a then
        goto passEqCheck
        local t = 1
        ::passEqCheck::
    end
    t = 'dont pass'
    ::passEqCheck::
    print(t) -- Result: dont pass
    ```
  The compiler will always jump to the first label, not the second one.

## How to use?
- [Loretta](https://github.com/LorettaDevs/Loretta) is required to use the compiler.
#### Normal usage
```csharp
using Lua51Compiler;

// The compiler will return a Lua51Prototype.
// The Parameter is a string.
var compilerResult = new LuaPrototypeCompiler().CompilePrototype("YOUR SOURCE HERE");

```
#### If you want to fold the constants
- In this part you can rely on [Loretta](https://github.com/LorettaDevs/Loretta) for folding constants.
```csharp
using Lua51Compiler;
using Loretta.CodeAnalysis;
using Loretta.CodeAnalysis.Lua;
using Loretta.CodeAnalysis.Lua.Experimental; // Use this if you want to fold constants.
using Loretta.CodeAnalysis.Lua.Syntax;

// parsing and folding the constants
var source = "YOUR SOURCE HERE";
var tree = SyntaxFactory.ParseSyntaxTree(source, new LuaParseOptions(LuaSyntaxOptions.Lua51));
var root = (CompilationUnitSyntax) tree.GetRoot();
var folded = root.ConstantFold(ConstantFoldingOptions.All);
var newSource = folded.NormalizeWhitespace().ToFullString();

// The compiler will return a Lua51Prototype.
var compilerResult = new LuaPrototypeCompiler().CompilePrototype(newSource);

```


## Compiler Examples
#### Input 1
```lua
local a = 1
local b = 2
local c = a + b
```
#### Output 1
```lua
function [function_A36B435A2A] ()                                          
        1 | Double                                                         
        2 | Double                                                         
                                                                           
        [00] Opcode: LOADK                A: 0    B: 0    C: 0          ; 1
        [01] Opcode: LOADK                A: 1    B: 1    C: 0          ; 2
        [02] Opcode: ADD                  A: 2    B: 0    C: 1             
        [03] Opcode: RETURN               A: 0    B: 1    C: 0             
end 
```

#### Input 2
```lua
local a,b,c = 1,2,3
while a < 10 do
  print(a)
  if a == 9 then
    break
  end
end
```
#### Output 2
```lua
function [function_758B32014F] ()
    1 | Double
    2 | Double
    3 | Double
    10 | Double
    print | String
    9 | Double
    
    [000] Opcode: LOADK                A: 0    B: 0    C: 0         ; 1
    [001] Opcode: LOADK                A: 1    B: 1    C: 0         ; 2
    [002] Opcode: LOADK                A: 2    B: 2    C: 0         ; 3
    [003] Opcode: LT                   A: 0    B: 0    C: 259
    [004] Opcode: JMP                  A: 0    B: 7    C: 0         ; goto 12
    [005] Opcode: GETGLOBAL            A: 3    B: 4    C: 0         ; "print"
    [006] Opcode: MOVE                 A: 4    B: 0    C: 0
    [007] Opcode: CALL                 A: 3    B: 2    C: 1
    [008] Opcode: EQ                   A: 0    B: 0    C: 261
    [009] Opcode: JMP                  A: 0    B: 1    C: 0         ; goto 11
    [010] Opcode: JMP                  A: 0    B: 1    C: 0         ; goto 12
    [011] Opcode: JMP                  A: 0    B: -9   C: 0         ; goto 3
    [012] Opcode: RETURN               A: 0    B: 1    C: 0
end
```

#### Input 3
```lua
function imAFunction (...)
  return 1,2,3
end
local a,b,c = imAFunction()
```
#### Output 3
```lua
function [function_998133888D] (...)
imAFunction | String

[00] Opcode: CLOSURE              A: 0    B: 0    C: 0          ; Function[function_829DF0BE40]
[01] Opcode: SETGLOBAL            A: 0    B: 0    C: 0          ; "imAFunction"
[02] Opcode: GETGLOBAL            A: 0    B: 0    C: 0          ; "imAFunction"
[03] Opcode: CALL                 A: 0    B: 1    C: 4
[04] Opcode: RETURN               A: 0    B: 1    C: 0
end

function [function_829DF0BE40] (...)
1 | Double
2 | Double
3 | Double

[00] Opcode: LOADK                A: 0    B: 0    C: 0          ; 1
[01] Opcode: LOADK                A: 1    B: 1    C: 0          ; 2
[02] Opcode: LOADK                A: 2    B: 2    C: 0          ; 3
[03] Opcode: RETURN               A: 0    B: 4    C: 0
[04] Opcode: RETURN               A: 0    B: 1    C: 0
end
```

## Libraries used:
- [Loretta](https://github.com/LorettaDevs/Loretta) A C# Lua lexer, parser, code analysis, transformation and code generation toolkit.