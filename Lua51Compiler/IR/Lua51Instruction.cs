
namespace Lua51Compiler.IR;

public enum Lua51Opcode {
	MOVE,
	LOADK,
	LOADBOOL,
	LOADNIL,
	GETUPVAL,
	GETGLOBAL,
	GETTABLE,
	SETGLOBAL,
	SETUPVAL, // TODO: ADD
	SETTABLE,
	NEWTABLE,
	SELF,
	ADD,
	SUB,
	MUL,
	DIV,
	MOD,
	POW,
	UNM,
	NOT,
	LEN,
	CONCAT,
	JMP,
	EQ,
	LT,
	LE,
	TEST,
	TESTSET,
	CALL,
	TAILCALL,
	RETURN,
	FORLOOP,
	FORPREP,
	TFORLOOP,
	SETLIST,
	CLOSE, // TODO: ADD
	CLOSURE,
	VARARG,
	NOP
}

public enum InstructionType {
	ABC,
	ABx,
	AsBx
}

public enum JumpType {
	Goto,
	Break,
	Continue,
	Normal
}

public class Lua51Instruction {
	public int PC = 0;

	public Lua51Opcode Opcode;
	public InstructionType Type;
	
	public JumpType JumpType = JumpType.Normal;
	public string? GotoLabel;

	public int A = 0;
	public int B = 0;
	public int C = 0;

	public void UpdatePC(Lua51Prototype proto) => PC = proto.Instructions.IndexOf(this);

	public void Update(Lua51Prototype proto)
	{
		PC = proto.Instructions.IndexOf(this);
		switch (Opcode)
		{
			case Lua51Opcode.LOADK:
			case Lua51Opcode.GETGLOBAL:
			case Lua51Opcode.SETGLOBAL:
			case Lua51Opcode.CLOSURE:
				Type = InstructionType.ABx;
				break;
			case Lua51Opcode.JMP:
			case Lua51Opcode.FORLOOP:
			case Lua51Opcode.FORPREP:
				Type = InstructionType.AsBx;
				break;
			default:
				Type = InstructionType.ABC;
				break;
		}
	}

	public override string ToString()
	{
		return $"{Opcode} {A} {B} {C}";
	}
}
