

using System.Reflection.Metadata;

namespace Lua51Compiler.IR;

public class Lua51Prototype 
{
	
	public string FunctionId = "function_" + Guid.NewGuid().ToString().Replace("-", "")[..10].ToUpper();


	public List<Lua51Instruction> Instructions = new();
	public List<Lua51Prototype> Prototypes = new();

	public  List<Lua51Constant> Constants = new();
	public readonly Dictionary<object, Lua51Constant> ConstantMap = new();
	
	public readonly Lua51Constant NilConstant = new(null!, ConstantType.Nil);
	
	public List<string> Upvalues = new();
	public int ParameterCount;
	public int UpvalueCount;
	public bool IsVararg;

	
	public Lua51Prototype() { }

	public int EmitK(object value) 
	{
		
		value ??= NilConstant;
		
		if (ConstantMap.TryGetValue(value, out var constant))
			return Constants.IndexOf(constant);
		
		
		var type = value switch {
			bool _ => ConstantType.Boolean,
			string _ => ConstantType.String,
			double _ => ConstantType.Double,
			long _ => ConstantType.Double,
			int _ => ConstantType.Double,
			null => ConstantType.Nil,
			_ => ConstantType.Unknown
		};

		switch (type)
		{
			case ConstantType.Unknown:
				return -1; // just in case
			case ConstantType.Nil:
				var idx = Constants.IndexOf(NilConstant);
				if (idx != -1)
					return idx;
				Constants.Add(NilConstant);
				return Constants.IndexOf(NilConstant);
			default:
				Constants.Add(new Lua51Constant(value, type));
				ConstantMap.Add(value, Constants[^1]);
				break;
		}

		return Constants.Count - 1;
	}

	public int Emit(Lua51Opcode opcode, int regA = 0, int regB = 0, int regC = 0) 
	{
		var instr = new Lua51Instruction {
			A = regA, B = regB, C = regC, 
			Opcode = opcode
		};
		Instructions.Add(instr);
		instr.Update(this);
		
		return Instructions.Count - 1;
	}
	
	public int Emit(Lua51Instruction instr) 
	{
		
		Instructions.Add(instr);
		instr.Update(this);
		
		return Instructions.Count - 1;
	}
	
	public int EmitLabel() => Instructions.Count - 1;
	
	
	// ToString
	public override string ToString()
	{ 
		var functionName = FunctionId;
		
		var parameters = "(";

		for (var i = 0; i < ParameterCount; i++)
		{
			parameters += $"p_{i}, ";
		}
		
		parameters += "...)";

		var constants = "";
		foreach (var c in Constants)
		{
			constants += $"\t\r\t{c.Data} | {c.Type}\n";
		}

		var instructions = "";
		for (var index = 0; index < Instructions!.Count; index++)
		{
			
			
			var instr = Instructions[index];
			
			var references = "";

			switch (instr.Opcode)
			{
				case Lua51Opcode.JMP:
				case Lua51Opcode.FORPREP:
				case Lua51Opcode.FORLOOP:
				{
					references = $"; goto {Instructions[index+1].PC + instr.B}";
					break;
				}
				case Lua51Opcode.LOADK:
				case Lua51Opcode.GETGLOBAL:
				case Lua51Opcode.SETGLOBAL:
				{
					references = Constants[instr.B].Type switch
					{
						ConstantType.String => $"; {Constants[instr.B]}",
						_ => $"; {Constants[instr.B]}"
					};
					break;
				}
				case Lua51Opcode.CLOSURE:
				{
					references = $"; Function[{Prototypes[instr.B].FunctionId}]";
					break;
				}
		            
			}
            
			var idxStr = index.ToString().PadLeft(Instructions.Count.ToString().Length+1, '0');
			
			var opcoName = instr.Opcode.ToString();
			opcoName = opcoName.PadRight(20, ' ');
			
			instructions += $"\t\r\t[{idxStr}] Opcode: {opcoName} A: {instr.A,-3}  B: {instr.B,-3}  C: {instr.C,-3}\t{references}\n";
			
		}

		// Idk why but when I try to edit this section, my GitHub Copilot suggests changes that cause my Rider IDE to freak out.
		return @$"
function [{functionName}] {parameters}
{constants}
{instructions}end
";
			
		}
}
	
	
