using System.Data;
using System.Globalization;
using System.Runtime.CompilerServices;
using Loretta.CodeAnalysis;
using Loretta.CodeAnalysis.Lua;
using Loretta.CodeAnalysis.Lua.Syntax;
using LuaCompiler.Extentions;
using static Loretta.CodeAnalysis.Lua.SyntaxFactory;
using Lua51Compiler.IR;
using static Lua51Compiler.Utils.Utils;

namespace Lua51Compiler;


/*
 * TODO:
 * - [?] Fix binary expression register allocation https://github.com/monkey0506/lua51src/blob/dbe6235db65cbfca10aebc2b3634a36c1616e462/lcode.c#L662-L663C21
 * - [X] Add statements like repeat until, if, while, for
 * - [X] Globals and Upvalues
 * - [X] break, continue
 * - [X] goto
 * - [?] closures
 */



public class Scope {
	public Lua51Compiler Compiler { get; set; }

	public int InitialStackTop { get; set; }
	public Dictionary<string, int> VariableMap = new();
	public Dictionary<string, int> TempVariableMap = new();
	public Dictionary<string, int> UpvalueMap = new();
	public Dictionary<string, int> LabelMap = new();
	public Dictionary<string, int> TempLabelMap = new();
	public StatementListSyntax StatementList { get; set; }
}

public class Lua51Compiler {
	public Lua51Prototype Root { get; set; } = new();
	public int StackTop = 0;
	public int CurrentRegister = 0;
	public Dictionary<string, int> GlobalMap = new();
	
	private Dictionary<SyntaxKind, Lua51Opcode> MathDictionary = new() {
		{SyntaxKind.AddExpression, Lua51Opcode.ADD},
		{SyntaxKind.SubtractExpression, Lua51Opcode.SUB},
		{SyntaxKind.MultiplyExpression, Lua51Opcode.MUL},
		{SyntaxKind.DivideExpression, Lua51Opcode.DIV},
		{SyntaxKind.ModuloExpression, Lua51Opcode.MOD},
		{SyntaxKind.ExponentiateExpression, Lua51Opcode.POW},
	};
	

	[MethodImpl(MethodImplOptions.AggressiveInlining)]
	protected virtual void AllocReg(int count = 1) {
		CurrentRegister += count;
		if (CurrentRegister > StackTop)
			StackTop = CurrentRegister;
	}
	
	
	////////// UTILITIES //////////
	
	private void MoveIfFalse(int newRegister, int source, bool increase = false)
	{
		if (newRegister != source)
			Root.Emit(Lua51Opcode.MOVE, newRegister, source);

		if (!increase)
			return;
		
		AllocReg();
	}

	private static bool IsCall(SyntaxNode node) => node.IsKind(SyntaxKind.FunctionCallExpression) || node.IsKind(SyntaxKind.MethodCallExpression);
	private bool IsJump(Lua51Opcode opcode)
	{
		// if opcode is JUMP, FORPREP, FORLOOP return true
		return opcode is (Lua51Opcode.JMP or Lua51Opcode.FORPREP or Lua51Opcode.FORLOOP);
	}
	
	private void PatchJump(int from, int to)
	{
		var instruction = Root.Instructions[from];
		var opcode = instruction.Opcode;
		if (IsJump(opcode))
		{
			instruction.B = to - from;
		}
	}
	
	private List<SyntaxNode> GetFunctionArgument(FunctionArgumentSyntax functionArgument)
	{
		return functionArgument.Kind() switch
		{
			SyntaxKind.StringFunctionArgument => new List<SyntaxNode>
			{
				((StringFunctionArgumentSyntax)functionArgument).Expression
			},
			SyntaxKind.ExpressionListFunctionArgument => ((IEnumerable<SyntaxNode>) ((ExpressionListFunctionArgumentSyntax) functionArgument).Expressions).ToList(),
			SyntaxKind.TableConstructorFunctionArgument => new List<SyntaxNode>
			{
				((TableConstructorFunctionArgumentSyntax)functionArgument).TableConstructor
			},
			_ => throw new Exception("Invalid function argument: " + functionArgument.Kind())
		};
	}

	
	protected int CompileExpressionAlloc(ExpressionSyntax expression, Scope scope, bool @return = false)
	{
		CompileExpression(expression, scope, @return);
		AllocReg();
		return CurrentRegister;
	}

	private (int, int) TryOptimizeExpressionConstant(ExpressionSyntax first, ExpressionSyntax second, Scope scope, bool skipFirst = false)
	{
		var firstRegister = -1;
		var secondRegister = -1;
		
		if (first is BinaryExpressionSyntax binFirst)
		{
			firstRegister = CompileBinaryExpression(binFirst, scope);
		}
		
		if (second is BinaryExpressionSyntax binSecond)
		{
			AllocReg();
			secondRegister = CompileBinaryExpression(binSecond, scope);
		}
		
		if (first is LiteralExpressionSyntax leftLiteral)
		{
			firstRegister = Root.EmitK(leftLiteral.Token.Value!) + 256;
		}
		if (second is LiteralExpressionSyntax rightLiteral)
		{
			secondRegister = Root.EmitK(rightLiteral.Token.Value!) + 256;
		}
		
		if (skipFirst)
			goto skipFirstRegister;
		if (firstRegister == -1)
		{
			if (scope.VariableMap.TryGetValue(((IdentifierNameSyntax) first).Name, out var localRegister))
			{
				firstRegister = localRegister;
			}
			else if (scope.UpvalueMap.TryGetValue(((IdentifierNameSyntax) first).Name, out var localRegister2))
			{
				firstRegister = localRegister2;
			}
			else if (GlobalMap.TryGetValue(((IdentifierNameSyntax) first).Name, out var localRegister3))
			{
				firstRegister = localRegister3;
			}
			else
			{
				CompileExpression(first, scope);
				firstRegister = CurrentRegister;
				AllocReg();
			}
		}
		
		skipFirstRegister: ;

		if (secondRegister == -1)
		{
			
			if (second is IdentifierNameSyntax syntax && scope.VariableMap.TryGetValue(syntax.Name, out var localRegister))
			{
				secondRegister = localRegister;
			}
			else if (second is IdentifierNameSyntax syntax2 && scope.UpvalueMap.TryGetValue(syntax2.Name, out var localRegister2))
			{
				secondRegister = localRegister2;
			}
			else if (second is IdentifierNameSyntax syntax3 && GlobalMap.TryGetValue(syntax3.Name, out var localRegister3))
			{
				secondRegister = localRegister3;
			}
			else
			{
				CompileExpression(second, scope);
				secondRegister = CurrentRegister;
				AllocReg();
			}
		}
		
		return (firstRegister, secondRegister);
	}
	
	
	////////// EXPRESSIONS //////////

	protected virtual int CompileExpression(ExpressionSyntax expression, Scope scope, bool @return = false) {
		switch (expression) {
			case LiteralExpressionSyntax literalExpression: {
				CompileLiteralExpression(literalExpression, scope);
				break;
			}
			case PrefixExpressionSyntax prefixExpression: {
				CompilePrefixExpression(prefixExpression, scope);
				break;
			}
			case UnaryExpressionSyntax unaryExpression: {
				CompileUnaryExpression(unaryExpression, scope);
				break;
			}
			case BinaryExpressionSyntax binaryExpression: {
				var oldReg = CurrentRegister;
				CompileBinaryExpression(binaryExpression, scope);
				break;
			}
			case VarArgExpressionSyntax varargExpression: {
				CompileVarArgExpression(varargExpression, scope, 1, @return);
				break;
			}
			case TableConstructorExpressionSyntax tableConstructorExpression: {
				CompileTableConstructorExpression(tableConstructorExpression, scope);
				break;
			}

			case AnonymousFunctionExpressionSyntax anonymousFunctionExpression: {
				CompileAnonymousFunctionExpression(anonymousFunctionExpression, scope);
				break;
			}
			default: {
				throw new SyntaxErrorException($"Unsupported expression syntax: {expression.Kind()}");
			}
		}
		
		return CurrentRegister;
	}

	protected virtual void CompileLiteralExpression(LiteralExpressionSyntax literal, Scope scope) {
		switch (literal.Kind()) {
			case SyntaxKind.StringLiteralExpression:
			case SyntaxKind.NumericalLiteralExpression: {
				Root.Emit(Lua51Opcode.LOADK, CurrentRegister, Root.EmitK(literal.Token.Value!));
				break;
			}
			case SyntaxKind.TrueLiteralExpression:
			case SyntaxKind.FalseLiteralExpression: {
				Root.Emit(Lua51Opcode.LOADK, CurrentRegister, Root.EmitK(literal.Kind() == SyntaxKind.TrueLiteralExpression));
				break;
			}
			case SyntaxKind.NilLiteralExpression: {
				Root.Emit(Lua51Opcode.LOADNIL, CurrentRegister, CurrentRegister);
				break;
			}
			default: {
				throw new SyntaxErrorException($"Unsupported literal expression syntax: {literal.Kind()}");
			}
		}
		//AllocReg();
	}
	
	private int CompileFunctionCallExpression(SyntaxNode call, List<SyntaxNode> arguments, List<SyntaxNode> nonChangedArguments, int function, Scope scope)
	{
		if (call is FunctionCallExpressionSyntax syntax)
		{
			CompileExpression(syntax.Expression, scope);
			AllocReg();
		}
		// DO NOT COMPILE THE METHODCALL EXPRESSION HERE
		
		var startRegister = CurrentRegister;
		var argumentCount = arguments.Count;
		
		
		//AllocReg(); // can be removed, will not error
		if (CurrentRegister-1 != function)
		{
			MoveIfFalse(CurrentRegister, function);
			function = CurrentRegister - 1;
		}

		foreach (var args in arguments)
		{
			var luaValue = CompileExpression((ExpressionSyntax)args, scope);
			if (luaValue != CurrentRegister)
			{
				MoveIfFalse(CurrentRegister, luaValue);
			}
			AllocReg();
		}

		var last = Root.Instructions.Last();
		if (last.Opcode is Lua51Opcode.VARARG)
		{
			argumentCount += last.B - 1;
		}

		var callC = call.Parent?.Parent switch
		{
			// get if the call is assigned to something
			LocalVariableDeclarationStatementSyntax ass => ass.Names.Count,
			AssignmentStatementSyntax ass2 => ass2.Variables.Count,
			_ => 0
		};

		var tailcallFlag = call.Parent is ReturnStatementSyntax parent && parent.Expressions.Last().Equals(call);
		if (tailcallFlag)
			Root.Emit(Lua51Opcode.TAILCALL, function, argumentCount+1);
		else
			Root.Emit(Lua51Opcode.CALL, function, argumentCount+1, callC+1);

		CurrentRegister = function;
		return CurrentRegister;
	}
	
	private int CompileMethodCallExpression(MethodCallExpressionSyntax self, Scope scope)
	{
		var luaValue = CompileExpression(self.Expression, scope);
		var identifier = self.Identifier;
		
		var constant = Root.EmitK(identifier.Value); // FIX IF ERRORS
		Root.Emit(Lua51Opcode.SELF, CurrentRegister, luaValue, constant+256);
		var source = GetFunctionArgument(self.Argument);
		var nonChangedArguments = source;
		var list = source.ToList();
		return CompileFunctionCallExpression(self, list, nonChangedArguments, CurrentRegister, scope);
	}

	protected virtual int CompilePrefixExpression(PrefixExpressionSyntax prefix, Scope scope)
	{
		switch (prefix)
		{
			case IdentifierNameSyntax identifierName:
			{
				CompileIdentifierName(identifierName, scope);
				break;
			}
			case ParenthesizedExpressionSyntax parenthesizedExpression: 
			{
				CompileExpression(parenthesizedExpression.Expression, scope);
				break;
			}
			case FunctionCallExpressionSyntax functionCallExpression:
			{
				var syntaxNodeList = GetFunctionArgument(functionCallExpression.Argument);
				CompileFunctionCallExpression(functionCallExpression, syntaxNodeList, syntaxNodeList, CurrentRegister, scope);
				break;
			}
			case MethodCallExpressionSyntax methodCallExpression:
			{
				CompileMethodCallExpression(methodCallExpression, scope);
				break;
			}
			case MemberAccessExpressionSyntax memberAccessExpression:
			{
				CompileMemberAccessExpression(memberAccessExpression, scope);
				break;
			}
			case ElementAccessExpressionSyntax elementAccessExpression:
			{
				CompileElementAccessExpression(elementAccessExpression, scope);
				break;
			}
		}

		return CurrentRegister;
	}
	
	private void CompileIdentifierName(IdentifierNameSyntax identifierName, Scope scope)
	{
		var name = identifierName.Name;
        
		if (scope.VariableMap.TryGetValue(name, out var localRegister))
		{
			MoveIfFalse(CurrentRegister, localRegister, false);
			return;
		}

		if (scope.UpvalueMap.TryGetValue(name, out var upvalIndex))
		{
			Root.Emit(Lua51Opcode.GETUPVAL, CurrentRegister, upvalIndex);
			AllocReg();
			return;
		}
		
		Root.Emit(Lua51Opcode.GETGLOBAL, CurrentRegister, Root.EmitK(name));
		
	}

	protected virtual void CompileUnaryExpression(UnaryExpressionSyntax unaryExpression, Scope scope) {
		CompileExpression(unaryExpression.Operand, scope);
		AllocReg();

		switch (unaryExpression.Kind())
		{
			case SyntaxKind.UnaryMinusExpression:
				Root.Emit(Lua51Opcode.UNM, CurrentRegister, CurrentRegister-1); // -1 because we allocated a register
				break;
			case SyntaxKind.LengthExpression:
				Root.Emit(Lua51Opcode.LEN, CurrentRegister, CurrentRegister-1);
				break;
			case SyntaxKind.LogicalNotExpression:
				Root.Emit(Lua51Opcode.NOT, CurrentRegister, CurrentRegister-1);
				break;
			default:
				throw new ArgumentOutOfRangeException();
		}
	}
	
	

	private int CompileCompareOpcode(SyntaxNode node, SyntaxNode left, SyntaxNode right, Scope scope, bool ifStatement = false)
	{
		if (!ifStatement) // for LOADBOOL a => b
		{
			Dictionary<SyntaxKind, Lua51Opcode> compareDict = new() {
				{SyntaxKind.EqualsExpression, Lua51Opcode.EQ}, // 0
				{SyntaxKind.NotEqualsExpression, Lua51Opcode.EQ}, // 1
			
				{SyntaxKind.LessThanExpression, Lua51Opcode.LT}, // 0
				{SyntaxKind.GreaterThanOrEqualExpression, Lua51Opcode.LT}, // 1
			
				{SyntaxKind.LessThanOrEqualExpression, Lua51Opcode.LE}, // 0
				{SyntaxKind.GreaterThanExpression, Lua51Opcode.LE}, // 1
			};
		
			var compareOpcode = compareDict[node.Kind()];
			
			// if its NotEqualsExpression, GreaterThanOrEqualExpression, GreaterThanExpression A register has to be 1
			if (node.IsKind(SyntaxKind.GreaterThanExpression) || node.IsKind(SyntaxKind.GreaterThanOrEqualExpression))
			{
				var (leftRegister, rightRegister) =
					TryOptimizeExpressionConstant((ExpressionSyntax)left, (ExpressionSyntax)right, scope);
				
				// A = 1
				Root.Emit(compareOpcode, 1, rightRegister, leftRegister);
			}
			else if (node.IsKind(SyntaxKind.NotEqualsExpression))
			{
				var (leftRegister, rightRegister) =
					TryOptimizeExpressionConstant((ExpressionSyntax)left, (ExpressionSyntax)right, scope);
				
				// A = 0
				Root.Emit(compareOpcode, 0, rightRegister, leftRegister);
			}
			else
			{
				var (leftRegister, rightRegister) =
					TryOptimizeExpressionConstant((ExpressionSyntax)left, (ExpressionSyntax)right, scope);

				Root.Emit(compareOpcode, 0, leftRegister, rightRegister);
			}
		}
		
		else if (ifStatement)  // for EQ LT LE if a => b then
		{
			Dictionary<SyntaxKind, Lua51Opcode> compareDict = new() {
				{SyntaxKind.EqualsExpression, Lua51Opcode.EQ}, // 0
				{SyntaxKind.NotEqualsExpression, Lua51Opcode.EQ}, // 1
			
				{SyntaxKind.LessThanExpression, Lua51Opcode.LT}, // 0
				{SyntaxKind.GreaterThanOrEqualExpression, Lua51Opcode.LE}, // 0
			
				{SyntaxKind.LessThanOrEqualExpression, Lua51Opcode.LE}, // 0
				{SyntaxKind.GreaterThanExpression, Lua51Opcode.LT}, // 0
			};
		
			var compareOpcode = compareDict[node.Kind()];
			
			// if its NotEqualsExpression, GreaterThanOrEqualExpression, GreaterThanExpression A register has to be 1
			if (node.IsKind(SyntaxKind.GreaterThanExpression) || node.IsKind(SyntaxKind.GreaterThanOrEqualExpression))
			{
				var (leftRegister, rightRegister) =
					TryOptimizeExpressionConstant((ExpressionSyntax)left, (ExpressionSyntax)right, scope);
				
				// A = 0
				Root.Emit(compareOpcode, 0, rightRegister, leftRegister);
			}
			else if (node.IsKind(SyntaxKind.NotEqualsExpression))
			{
				var (leftRegister, rightRegister) =
					TryOptimizeExpressionConstant((ExpressionSyntax)left, (ExpressionSyntax)right, scope);
				
				// A = 1
				Root.Emit(compareOpcode, 1, rightRegister, leftRegister);
			}
			else
			{
				var (leftRegister, rightRegister) =
					TryOptimizeExpressionConstant((ExpressionSyntax)left, (ExpressionSyntax)right, scope);

				Root.Emit(compareOpcode, 0, leftRegister, rightRegister);
			}
		}

		return Root.EmitLabel();
	}

	protected virtual int CompileBinaryExpression(BinaryExpressionSyntax binaryExpression, Scope scope, bool fromOptimizer = false) 
	{
		var startRegister = CurrentRegister;
		
		var binLeft = binaryExpression.Left;
		var binRight = binaryExpression.Right;

		switch (binaryExpression.Kind())
		{
			case SyntaxKind.AddExpression:
			case SyntaxKind.SubtractExpression:
			case SyntaxKind.MultiplyExpression:
			case SyntaxKind.DivideExpression:
			case SyntaxKind.ModuloExpression:
			case SyntaxKind.ExponentiateExpression:
			{
				var (leftRegister, rightRegister) = TryOptimizeExpressionConstant(binLeft, binRight, scope);
				Root.Emit(MathDictionary[binaryExpression.Kind()], CurrentRegister, leftRegister, rightRegister);
				break;
			}
			case SyntaxKind.ConcatExpression:
			{
				var concatExpressions = GetConcatExpressions(binaryExpression); // Utils.cs
				var concatReg = CurrentRegister;
				
				foreach (var exp in concatExpressions)
				{
					var exprReg = CompileExpression(exp, scope);
					MoveIfFalse(CurrentRegister, exprReg);
					AllocReg();
				}
				
				CurrentRegister = startRegister;
				Root.Emit(Lua51Opcode.CONCAT, concatReg, CurrentRegister, CurrentRegister + concatExpressions.Count - 1);
				break;
			}
			case SyntaxKind.EqualsExpression:
			case SyntaxKind.NotEqualsExpression:
			case SyntaxKind.LessThanExpression:
			case SyntaxKind.LessThanOrEqualExpression:
			case SyntaxKind.GreaterThanExpression:
			case SyntaxKind.GreaterThanOrEqualExpression:
			{
				CompileCompareOpcode(binaryExpression, binRight, binLeft, scope);
				var from = Root.Emit(Lua51Opcode.JMP, 0, 31);
				AllocReg();
				Root.Emit(Lua51Opcode.LOADBOOL, startRegister, 0, 1);

				var to = Root.EmitLabel();
				Root.Emit(Lua51Opcode.LOADBOOL, startRegister, 1, 0);
				PatchJump(from, to);
				CurrentRegister--;
				return CurrentRegister;
			}
			case SyntaxKind.LogicalAndExpression:
			case SyntaxKind.LogicalOrExpression:
			{
				var dontGenerateMove = binaryExpression.Parent is BinaryExpressionSyntax;
				
				if (binLeft is BinaryExpressionSyntax bb)
				{
					//AllocReg();
					//CompileBinaryExpression(bb, scope);
				}
				
				var regC = 0;
				if (binaryExpression.Kind() is SyntaxKind.LogicalOrExpression)
					regC = 1;
				
				
				var selectedOpcode = Lua51Opcode.TESTSET;
				switch (binaryExpression.Parent?.Parent)
				{
					case LocalVariableDeclarationStatementSyntax { Names.Count: 1 } ass:
					{
						if (ass.Names[0].Name == ((IdentifierNameSyntax)binLeft).Name)
						{
							selectedOpcode = Lua51Opcode.TEST;
						}

						break;
					}
					case AssignmentStatementSyntax { Variables.Count: 1 } ass2:
					{
						IdentifierNameSyntax identifierName = null;
						
						if (binLeft is not IdentifierNameSyntax syntax) break;
						if (((IdentifierNameSyntax)ass2.Variables[0]).Name == syntax.Name)
						{
							selectedOpcode = Lua51Opcode.TEST;
						}

						break;
					}
				}
				
				
				var	(regA,regB) = TryOptimizeExpressionConstant(binaryExpression.Left, binaryExpression.Right, scope);

				var testsetA = startRegister;
				switch (selectedOpcode)
				{
					case Lua51Opcode.TEST:
						Root.Emit(Lua51Opcode.TEST, regA, 0, regC);
						break;
					case Lua51Opcode.TESTSET:
						Root.Emit(Lua51Opcode.TESTSET, testsetA, regA, regC);
						break;
				}
				// Commented out for readability
				// Root.Emit(selectedOpcode, regA, CurrentRegister = (selectedOpcode == Lua51Opcode.Test) ? 0 : CurrentRegister, regC);
				
				var from = Root.Emit(Lua51Opcode.JMP,0,31); // Testing PatchJump() again...
				
				switch (selectedOpcode)
				{
					case Lua51Opcode.TEST:
					{
						if (dontGenerateMove) break;
						Root.Emit(Lua51Opcode.MOVE, regA, regB);
					}
						break;
					case Lua51Opcode.TESTSET:
					{
						if (dontGenerateMove) break;
						Root.Emit(Lua51Opcode.MOVE, testsetA, regB);
					}
						break;
				}
				
				if (binRight is BinaryExpressionSyntax aa)
				{
					CompileBinaryExpression(aa, scope);
					AllocReg(); // FIX IF ERRORS
				}
				
				PatchJump(from, Root.EmitLabel()); // TODO: MUST FIX
				
				if (binRight is IdentifierNameSyntax syntax2 &&
				    scope.VariableMap.TryGetValue(syntax2.Name, out var localRegister))
				{
					return localRegister;
				}

				break;
			}
		}

		return CurrentRegister;
	}

	protected virtual void CompileVarArgExpression(VarArgExpressionSyntax varargExpression, Scope scope, int targets = 1, bool multRet = false) {
		Root.IsVararg = true;
		if (!multRet) {
			Root.Emit(Lua51Opcode.VARARG, CurrentRegister, targets);
		} else {
			Root.Emit(Lua51Opcode.VARARG, CurrentRegister, 0);
		}
		AllocReg();
	}

	protected virtual void CompileTableConstructorExpression(TableConstructorExpressionSyntax tableConstructorExpression, Scope scope)
	{
		
		if (tableConstructorExpression.Fields.Count == 0) {
			Root.Emit(Lua51Opcode.NEWTABLE, CurrentRegister, 0, 0);
			return;
		}

		var B = 0;
		var C = 0;
		var tabFields = tableConstructorExpression.Fields;
		foreach (var tFieldSyntax in tabFields)
		{
			if (tFieldSyntax.IsKind(SyntaxKind.UnkeyedTableField))
				B++;
			else if (tFieldSyntax.IsKind(SyntaxKind.ExpressionKeyedTableField) || tFieldSyntax.IsKind(SyntaxKind.IdentifierKeyedTableField))
				C++;
		}
		
		var tableRegister = CurrentRegister; // - 1??
		Root.Emit(Lua51Opcode.NEWTABLE, tableRegister, B, C);
		AllocReg();
		
		var setListB = 0;
		if (B > 0)
		{
			foreach (var tFieldSyntax in tabFields.Where(tFieldSyntax => tFieldSyntax.IsKind(SyntaxKind.UnkeyedTableField)))
			{
				CompileExpression(((UnkeyedTableFieldSyntax) tFieldSyntax).Value, scope);
				AllocReg();
				setListB++;
			}
		}
		
		if (C > 0)
		{
			foreach (var tFieldSyntax in tabFields)
			{
				switch (tFieldSyntax.Kind())
				{
					case SyntaxKind.ExpressionKeyedTableField:
					{
						var exprKeyedTableFieldSyntax = (ExpressionKeyedTableFieldSyntax) tFieldSyntax;
						
						var (tableKeyRegister, tableValueRegister) = TryOptimizeExpressionConstant(exprKeyedTableFieldSyntax.Key, exprKeyedTableFieldSyntax.Value, scope);
						
						Root.Emit(Lua51Opcode.SETTABLE, tableRegister, tableKeyRegister, tableValueRegister);
						
						break;
					}
					case SyntaxKind.IdentifierKeyedTableField:
					{
						var exprKeyedTableFieldSyntax = (IdentifierKeyedTableFieldSyntax) tFieldSyntax;
						var idenName = IdentifierName(exprKeyedTableFieldSyntax.Identifier).Name;
						
						var (tableKeyRegister, tableValueRegister) = TryOptimizeExpressionConstant(exprKeyedTableFieldSyntax.Value, exprKeyedTableFieldSyntax.Value, scope, true);
						
						Root.Emit(Lua51Opcode.SETTABLE, tableRegister, Root.EmitK(idenName)+256, tableValueRegister);
						
						break;
					}
				}
			}
		}
		
		if (setListB > 0)
		{
			Root.Emit(Lua51Opcode.SETLIST, tableRegister, setListB, 1);
			CurrentRegister = tableRegister;
		}
	}
	
	private void CompileMemberAccessExpression(MemberAccessExpressionSyntax memberAccess, Scope scope)
	{
		var member = CompileExpressionAlloc(memberAccess.Expression, scope);
        
		Root.Emit(Lua51Opcode.GETTABLE, CurrentRegister, member, Root.EmitK(memberAccess.MemberName.Text));
	}
	
	private void CompileElementAccessExpression(ElementAccessExpressionSyntax elementAccess, Scope scope)
	{
		var startReg = CompileExpressionAlloc(elementAccess.Expression, scope);
        
		if (elementAccess.KeyExpression is LiteralExpressionSyntax literalExpression)
		{
			switch (literalExpression.Token.Value)
			{
				case string s:
				{
					Root.Emit(Lua51Opcode.GETTABLE, CurrentRegister, startReg, Root.EmitK(s));
					break;
				}
				case double d:
				{
					Root.Emit(Lua51Opcode.GETTABLE, CurrentRegister, startReg, Root.EmitK(d));
					break;
				}
				case long l:
				{
					Root.Emit(Lua51Opcode.GETTABLE, CurrentRegister, startReg, Root.EmitK(l));
					break;
				}
				case bool b:
				{
					Root.Emit(Lua51Opcode.GETTABLE, CurrentRegister, startReg, Root.EmitK(b));
					break;
				}
				default:
				{
					throw new NotImplementedException();
				}
			}
            
			return;
		}

		var startRegister = CurrentRegister;

		var keyReg = CompileExpressionAlloc(elementAccess.KeyExpression, scope);

		CurrentRegister = startRegister;

		Root.Emit(Lua51Opcode.GETTABLE, CurrentRegister, startReg, keyReg);
	}

	protected virtual void CompileAnonymousFunctionExpression(
		AnonymousFunctionExpressionSyntax anonymousFunctionExpression,
		Scope scope
	) {
		throw new NotImplementedException();
	}
	
	
	
	////////// STATMENTS //////////
	
	protected virtual void CompileReturnStatement(ReturnStatementSyntax @return, Scope scope) {
		var oldRegister = CurrentRegister;
		if (@return.Expressions.Count == 0) {
			oldRegister = 0;
		}
		foreach (var returnExpression in @return.Expressions) {
			CompileExpression(returnExpression, scope, true);
			AllocReg();
		}
		Root.Emit(Lua51Opcode.RETURN, oldRegister, @return.Expressions.Count + 1);
		// check if return expressions contains a VarArgExpressionSyntax:
		if (@return.Expressions.Any(x => x is VarArgExpressionSyntax)) {
			Root.Instructions[^1].B = 0;
		}
		CurrentRegister = oldRegister;
	}
	
	protected virtual void CompileDoStatement(DoStatementSyntax @do, Scope scope) {
		foreach (var statement in @do.Body.Statements) {
			CompileStatement(statement, scope);
		}
	}
	
	protected virtual void CompileLocalVariableDeclarationStatement(LocalVariableDeclarationStatementSyntax localVariableDeclarationStatement, Scope scope)
	{
		if (localVariableDeclarationStatement.EqualsValues is null) {
			foreach (var name in localVariableDeclarationStatement.Names) {
				if (scope.VariableMap.Any())
				{
					AllocReg();
					scope.VariableMap[name.Name] = scope.VariableMap.Values.DefaultIfEmpty().Max() + 1;
				}
				else
				{
					scope.VariableMap.Add(name.Name, CurrentRegister);
				}
				
			}
			return;
		}

		var valueCount = localVariableDeclarationStatement.EqualsValues.Values.Count;
		var targetCount = localVariableDeclarationStatement.Names.Count;
		var addLoadNil = true;
		for (var i = 0; i < targetCount; i++) {
			// get previous expression unless we're on the first iteration
			var name = localVariableDeclarationStatement.Names[i];
			
			if (scope.VariableMap.Any())
				scope.VariableMap[name.Name] = scope.VariableMap.Values.DefaultIfEmpty().Max() + 1;
			else
				scope.VariableMap.Add(name.Name, CurrentRegister);
			
			if (i < valueCount) {
				CompileExpression(localVariableDeclarationStatement.EqualsValues.Values[i], scope);
				if (localVariableDeclarationStatement.EqualsValues.Values[i] is FunctionCallExpressionSyntax)
					addLoadNil = false;
				AllocReg();
			}
		}

		if (!addLoadNil)
			return;
		
		if (valueCount < targetCount)
		{
			Root.Emit(Lua51Opcode.LOADNIL, CurrentRegister, targetCount-1);
			for (var i = CurrentRegister; i < targetCount; i++)
				AllocReg();
		}
		
	}
	
	private void CompileAssignmentStatement(AssignmentStatementSyntax assignment, Scope scope)
	{
		//// Will Change ////
		var valueCount = assignment.EqualsValues.Values.Count;
		var variableCount = assignment.Variables.Count;
        var addLoadNil = true;
        
		for (var index = 0; index < variableCount; index++)
		{
			var variable = assignment.Variables[index];
            
			if (variableCount + 1 == index)
			{
				CompileExpression(variable, scope);
				AllocReg();
			}
			else
			{
				var value = assignment.EqualsValues.Values[index];
				if (value is FunctionCallExpressionSyntax)
					addLoadNil = false;
				switch (variable)
				{
					case IdentifierNameSyntax identifierName:
					{
						if (!scope.VariableMap.ContainsKey(identifierName.Name))
						{
							if (!scope.VariableMap.ContainsKey(identifierName.Name))
							{
								Root.Emit(Lua51Opcode.SETGLOBAL,CurrentRegister , Root.EmitK(identifierName.Name));
								AllocReg();
							}
							else
							{
								CompileExpression(variable, scope);
								AllocReg();
							}
						}
						else
						{
							CompileExpression(variable, scope);
							AllocReg();
						}

						CompileExpression(value, scope);
						AllocReg();
						
						break;
					}
					default:
					{
						CompileExpression(variable, scope);
						AllocReg();
						CompileExpression(value, scope);
						AllocReg();
						break;
					}
				}
			}
		}
		
		if (!addLoadNil)
			return;
		
		if (variableCount > valueCount)
		{
			Root.Emit(Lua51Opcode.LOADNIL, CurrentRegister+variableCount, CurrentRegister + valueCount);
		}
	}
	
	private int CompileIfStatement(IfStatementSyntax ifStatement, Scope scope)
	{
		var startRegister = CurrentRegister;
		var elseJmp = new List<int>();
		
		void CompileIfCondition()
		{
			switch (ifStatement.Condition)
			{
				case IdentifierNameSyntax identifier:
				{
					// compile test
					Root.Emit(Lua51Opcode.TEST, scope.VariableMap[identifier.Name], 0, 0);
					break;
				}
				case LiteralExpressionSyntax literal:
				{
					// compile literal
					CompileLiteralExpression(literal, scope);
					break;
				}
				case BinaryExpressionSyntax binary:
				{
					// i know this is kind of a retarded way to do this but i dont care
					if (binary.Kind() is (SyntaxKind.EqualsExpression or SyntaxKind.NotEqualsExpression or SyntaxKind.LessThanExpression or SyntaxKind.LessThanOrEqualExpression or SyntaxKind.GreaterThanExpression or SyntaxKind.GreaterThanOrEqualExpression))
						CompileCompareOpcode(binary, binary.Left, binary.Right, scope, true);
					else
						CompileBinaryExpression(binary, scope);
					break;
				}
				default:
					throw new ArgumentOutOfRangeException();
			}
		}
		int CompileBlock()
		{
			scope.TempVariableMap = scope.VariableMap.DeepClone();
			scope.TempLabelMap = scope.LabelMap.DeepClone();
			foreach (var statement in ifStatement.Body.Statements)
			{
				CompileStatement(statement, scope);
			}
			// Local Releasing
			scope.VariableMap = scope.TempVariableMap;
			scope.LabelMap = scope.TempLabelMap;
			
			if (ifStatement.ElseIfClauses.Any())
				elseJmp.Add(Root.Emit(Lua51Opcode.JMP, 0, 31));

			return CurrentRegister;
		}
		void CompileElseIfCondition(ElseIfClauseSyntax elseIfClause)
		{
			switch (elseIfClause.Condition)
			{
				case IdentifierNameSyntax identifier:
				{
					// compile test
					Root.Emit(Lua51Opcode.TEST, scope.VariableMap[identifier.Name], 0, 0);
					break;
				}
				case LiteralExpressionSyntax literal:
				{
					// compile literal
					CompileLiteralExpression(literal, scope);
					break;
				}
				case BinaryExpressionSyntax binary:
				{
					// i know this is kind of a retarded way to do this but i dont care
					if (binary.Kind() is (SyntaxKind.EqualsExpression or SyntaxKind.NotEqualsExpression or SyntaxKind.LessThanExpression or SyntaxKind.LessThanOrEqualExpression or SyntaxKind.GreaterThanExpression or SyntaxKind.GreaterThanOrEqualExpression))
						CompileCompareOpcode(binary, binary.Left, binary.Right, scope, true);
					else
						CompileBinaryExpression(binary, scope);
					break;
				}
				default:
					throw new ArgumentOutOfRangeException();
			}
		}
		void CompileElseIf()
		{
			foreach (var elseIfClause in ifStatement.ElseIfClauses)
			{
				CompileElseIfCondition(elseIfClause);
				var from = Root.Emit(Lua51Opcode.JMP, 0, 31);
				foreach (var statement in elseIfClause.Body.Statements)
				{
					CompileStatement(statement, scope);
				}
				
				elseJmp.Add( Root.Emit(Lua51Opcode.JMP, 0, 31) );
				
				PatchJump(from, Root.EmitLabel());
			}
		}
		void CompileElse()
		{
			if (ifStatement.ElseClause is null)
				return;
			
			foreach (var statement in ifStatement.ElseClause!.ElseBody.Statements)
			{
				CompileStatement(statement, scope);
			}
		}
		
		CompileIfCondition();
		var from = Root.Emit(Lua51Opcode.JMP, 0, 31);
		var lastReg = CompileBlock();
		PatchJump(from, Root.EmitLabel());

		CompileElseIf();
		CompileElse();
		
		// Release locals
		CurrentRegister = startRegister;
		
		foreach (var elseifJump in elseJmp)
		{
			PatchJump(elseifJump, Root.EmitLabel());
		}
		
		return CurrentRegister;
	}
	
	private int CompileWhileStatement(WhileStatementSyntax whileStatement, Scope scope)
	{
		void CompileWhileCondition()
		{
			switch (whileStatement.Condition)
			{
				case IdentifierNameSyntax identifier:
				{
					// compile test
					Root.Emit(Lua51Opcode.TEST, scope.VariableMap[identifier.Name], 0, 0);
					break;
				}
				case LiteralExpressionSyntax literal:
				{
					// compile literal
					CompileLiteralExpression(literal, scope);
					break;
				}
				case BinaryExpressionSyntax binary:
				{
					// i know this is kind of a retarded way to do this but i dont care
					if (binary.Kind() is (SyntaxKind.EqualsExpression or SyntaxKind.NotEqualsExpression or SyntaxKind.LessThanExpression or SyntaxKind.LessThanOrEqualExpression or SyntaxKind.GreaterThanExpression or SyntaxKind.GreaterThanOrEqualExpression))
						CompileCompareOpcode(binary, binary.Left, binary.Right, scope, true);
					else
						CompileBinaryExpression(binary, scope);
					break;
				}
				default:
					throw new ArgumentOutOfRangeException();
			}
		}
		int CompileBlock()
		{
			scope.TempVariableMap = scope.VariableMap.DeepClone();
			scope.TempLabelMap = scope.LabelMap.DeepClone();
			foreach (var statement in whileStatement.Body.Statements)
			{
				CompileStatement(statement, scope);
			}
			
			scope.VariableMap = scope.TempVariableMap;
			scope.LabelMap = scope.TempLabelMap;

			return CurrentRegister;
		}
		void CompileBreakContinue(int loopStart, int loopEnd)
		{
			// Get the instructions from the loopStart to loopEnd as a list
			var instructions = Root.Instructions.GetRange(loopStart, loopEnd - loopStart);
			

			var jmpInstructions = instructions.Where(x => x is { Opcode: Lua51Opcode.JMP, JumpType: not JumpType.Normal }).ToList();

			foreach (var jmp in jmpInstructions)
			{
				switch (jmp.JumpType)
				{
					case JumpType.Continue:
						PatchJump(jmp.PC, loopStart);
						break;
					
					case JumpType.Break:
						PatchJump(jmp.PC, loopEnd);
						break;
				}
			}
		}
		
		var startRegister = CurrentRegister;
		
		CompileWhileCondition();
		var whileCondition = Root.EmitLabel()-1;
		
		var from = Root.Emit(Lua51Opcode.JMP, 0, 31);
		var lastReg = CompileBlock();
		
		var whileJmp = Root.Emit(Lua51Opcode.JMP, 0, 31);
		PatchJump(from, Root.EmitLabel());
		PatchJump(whileJmp, whileCondition);
		
		CompileBreakContinue(whileCondition, whileJmp);
		
		// Release locals
		CurrentRegister = startRegister;
		
		return CurrentRegister;
	}
	
	private int CompileNumericForStatement(NumericForStatementSyntax numericForStatement, Scope scope)
	{
		int CompileBlock()
		{
			var identifierRegister = CurrentRegister;
			AllocReg(); // +1 because we need the start register
			scope.TempVariableMap = scope.VariableMap.DeepClone();
			scope.TempLabelMap = scope.LabelMap.DeepClone();

			scope.VariableMap[numericForStatement.Identifier.Name] = identifierRegister;

			foreach (var statement in numericForStatement.Body.Statements)
			{
				CompileStatement(statement, scope);
			}
			scope.VariableMap = scope.TempVariableMap;
			scope.LabelMap = scope.TempLabelMap;

			return CurrentRegister;
		}
		void CompileBreakContinue(int loopStart, int loopEnd)
		{
			// Get the instructions from the loopStart to loopEnd as a list
			var instructions = Root.Instructions.GetRange(loopStart, loopEnd - loopStart);
			
			var jmpInstructions = instructions.Where(x => x is { Opcode: Lua51Opcode.JMP, JumpType: not JumpType.Normal }).ToList();

			foreach (var jmp in jmpInstructions)
			{
				switch (jmp.JumpType)
				{
					case JumpType.Continue:
						PatchJump(jmp.PC, loopEnd-1);
						break;
					
					case JumpType.Break:
						PatchJump(jmp.PC, loopEnd);
						break;
				}
			}
		}
		
		var startRegister = CurrentRegister;
		
		// Compile Start,Step,End values
		var start = numericForStatement.InitialValue;
		var end = numericForStatement.FinalValue;
		var step = (ExpressionSyntax)LiteralExpression(SyntaxKind.NumericalLiteralExpression, Literal(1)); // idk why i did this but it looks cool
		if (numericForStatement.StepValue is not null)
			step = numericForStatement.StepValue;
		
		CompileExpressionAlloc(start, scope);
		CompileExpressionAlloc(end, scope);
		CompileExpressionAlloc(step, scope);
		
		var registerA = CurrentRegister - 3; // -2 because we allocated 2 registers for the for loop
		var from = Root.Emit(Lua51Opcode.FORPREP, registerA, 31);
		var forLoopLabel = Root.EmitLabel();
		
		CompileBlock();
		PatchJump(from, Root.EmitLabel());
		
		var forLoop = Root.Emit(Lua51Opcode.FORLOOP, registerA, 31);
		PatchJump(forLoop, forLoopLabel);
		
		CompileBreakContinue(from, forLoop);
		
		// Release locals
		CurrentRegister = startRegister;
		
		return CurrentRegister;
	}
	
	private int CompileGenericForStatement(GenericForStatementSyntax genericForStatement, Scope scope)
	{
		int CompileBlock()
		{
			var firstIdentifierRegister = CurrentRegister;
			AllocReg(); // +1 because we need the start register
			var secondIdentifierRegister = CurrentRegister;
			AllocReg(); // +1 because we need the start+1 register
			scope.TempVariableMap = scope.VariableMap.DeepClone();
			scope.TempLabelMap = scope.LabelMap.DeepClone();
			
			var names = genericForStatement.Identifiers;
			var firstIdentifier = names[0];
			var secondIdentifier = names[1];

			scope.VariableMap[firstIdentifier.Name] = firstIdentifierRegister;
			scope.VariableMap[secondIdentifier.Name] = secondIdentifierRegister;

			foreach (var statement in genericForStatement.Body.Statements)
			{
				CompileStatement(statement, scope);
			}
			
			scope.VariableMap = scope.TempVariableMap;
			scope.LabelMap = scope.TempLabelMap;

			return CurrentRegister;
		}
		void CompileBreakContinue(int loopStart, int loopEnd)
		{
			// Get the instructions from the loopStart to loopEnd as a list
			var instructions = Root.Instructions.GetRange(loopStart, loopEnd - loopStart);
			
			var jmpInstructions = instructions.Where(x => x is { Opcode: Lua51Opcode.JMP, JumpType: not JumpType.Normal }).ToList();

			foreach (var jmp in jmpInstructions)
			{
				switch (jmp.JumpType)
				{
					case JumpType.Continue:
						PatchJump(jmp.PC, loopEnd-2);
						break;
					
					case JumpType.Break:
						PatchJump(jmp.PC, loopEnd);
						break;
				}
			}
		}
		
		var startRegister = CurrentRegister;
		

		var exprs = genericForStatement.Expressions;

		foreach (var expr in exprs)
		{
			CompileExpression(expr, scope);
		}
		var tForLoopJump = Root.Emit(Lua51Opcode.JMP , 0, 31);
		var from = Root.EmitLabel();
		CompileBlock();
		
		var tForLoop = Root.Emit(Lua51Opcode.TFORLOOP, startRegister, 2);
		PatchJump(tForLoopJump, tForLoop-1);
		var tForLoopNextJump = Root.Emit(Lua51Opcode.JMP, 0, 31);
		PatchJump(tForLoopNextJump, from);

		CompileBreakContinue(tForLoopJump, tForLoopNextJump);
		
		// Release locals
		CurrentRegister = startRegister;
		
		return CurrentRegister;
	}
	
	private int CompileRepeatUntilStatement(RepeatUntilStatementSyntax repeatUntilStatement, Scope scope)
	{
		void CompileRepeatUntilCondition()
		{
			switch (repeatUntilStatement.Condition)
			{
				case IdentifierNameSyntax identifier:
				{
					// compile test
					Root.Emit(Lua51Opcode.TEST, scope.VariableMap[identifier.Name], 0, 0);
					break;
				}
				case LiteralExpressionSyntax literal:
				{
					// compile literal
					CompileLiteralExpression(literal, scope);
					break;
				}
				case BinaryExpressionSyntax binary:
				{
					// i know this is kind of a retarded way to do this but i dont care
					if (binary.Kind() is (SyntaxKind.EqualsExpression or SyntaxKind.NotEqualsExpression or SyntaxKind.LessThanExpression or SyntaxKind.LessThanOrEqualExpression or SyntaxKind.GreaterThanExpression or SyntaxKind.GreaterThanOrEqualExpression))
						CompileCompareOpcode(binary, binary.Left, binary.Right, scope, true);
					else
						CompileBinaryExpression(binary, scope);
					break;
				}
				default:
					throw new ArgumentOutOfRangeException();
			}
		}
		int CompileBlock()
		{
			scope.TempVariableMap = scope.VariableMap.DeepClone();
			scope.TempLabelMap = scope.LabelMap.DeepClone();
			foreach (var statement in repeatUntilStatement.Body.Statements)
			{
				CompileStatement(statement, scope);
			}
			scope.VariableMap = scope.TempVariableMap;
			scope.LabelMap = scope.TempLabelMap;

			return CurrentRegister;
		}
		void CompileBreakContinue(int loopStart, int loopEnd)
		{
			if (loopStart < 0)
				loopStart = 0;
			// Get the instructions from the loopStart to loopEnd as a list
			var instructions = Root.Instructions.GetRange(loopStart, loopEnd - loopStart);
			
			var jmpInstructions = instructions.Where(x => x is { Opcode: Lua51Opcode.JMP, JumpType: not JumpType.Normal }).ToList();

			foreach (var jmp in jmpInstructions)
			{
				switch (jmp.JumpType)
				{
					case JumpType.Continue:
						PatchJump(jmp.PC, loopEnd-2);
						break;
					
					case JumpType.Break:
						PatchJump(jmp.PC, loopEnd);
						break;
				}
			}
		}
		
		var startRegister = CurrentRegister;
		
		var repeatUntilStart = Root.EmitLabel();
		CompileBlock();
		CompileRepeatUntilCondition();
		var from = Root.Emit(Lua51Opcode.JMP, 0, 31);
		PatchJump(from, repeatUntilStart);
		
		CompileBreakContinue(repeatUntilStart, from);
		
		// Release locals
		CurrentRegister = startRegister;
		
		return CurrentRegister;
	}
	
	private int CompileContinueStatement(ContinueStatementSyntax continueStatement, Scope scope)
	{
		Root.Emit(new Lua51Instruction
		{
			Opcode = Lua51Opcode.JMP,
			A = 0,
			B = 31,
			JumpType = JumpType.Continue
		});
		return CurrentRegister;
	}
	
	private int CompileBreakStatement(BreakStatementSyntax breakStatement, Scope scope)
	{
		Root.Emit(new Lua51Instruction
		{
			Opcode = Lua51Opcode.JMP,
			A = 0,
			B = 31,
			JumpType = JumpType.Break
		});
		return CurrentRegister;
	}
	
	private int CompileFunctionDeclarationStatement(FunctionDeclarationStatementSyntax functionDeclarationStatement, Scope scope)
	{
		
		var startRegister = CurrentRegister;
		var compiledPrototype = new LuaPrototypeCompiler().CompilePrototype(functionDeclarationStatement.Body.NormalizeWhitespace().ToFullString(), scope);
		
		var parameterCount = functionDeclarationStatement.Parameters.Parameters.Count;
		if (functionDeclarationStatement.Parameters.Parameters.Any(x => x is VarArgParameterSyntax))
			parameterCount--;
		
		var proto = new Lua51Prototype
		{
			Instructions = compiledPrototype.Instructions,
			Constants = compiledPrototype.Constants,
			Prototypes = compiledPrototype.Prototypes,
			UpvalueCount = compiledPrototype.UpvalueCount,
			ParameterCount = parameterCount,
			IsVararg = compiledPrototype.IsVararg
		};
		Root.Prototypes.Add(proto);
		Root.Emit(Lua51Opcode.CLOSURE, CurrentRegister, Root.Prototypes.Count-1);
		Root.Emit(Lua51Opcode.SETGLOBAL, CurrentRegister, Root.EmitK(IdentifierName(functionDeclarationStatement.Name.Name).Name));

		foreach (var upval in proto.Upvalues)
		{
			if (scope.VariableMap.TryGetValue(upval, out var value2))
			{
				Root.Emit(Lua51Opcode.MOVE, CurrentRegister, value2);
			}
			else if (scope.UpvalueMap.TryGetValue(upval, out var value1))
			{
				Root.Emit(Lua51Opcode.GETUPVAL, CurrentRegister, value1);
			}
			else
			{
				throw new Exception("Upvalue not found!");
			}
		}
		
		
		
		
		return CurrentRegister;
	}
	
	private int CompileLocalFunctionDeclarationStatement(LocalFunctionDeclarationStatementSyntax localFunctionDeclarationStatement, Scope scope)
	{
		
		throw new NotImplementedException();
		
		return CurrentRegister;
	}
	
	private void SearchGotoLabels(StatementListSyntax statementList, Scope scope)
	{
		// Search for goto labels
		foreach (var gotoLabelStatement in statementList.Statements.Where(statement => statement.Kind() is SyntaxKind.GotoLabelStatement).Cast<GotoLabelStatementSyntax?>())
		{
			if (scope.LabelMap.ContainsKey(gotoLabelStatement!.Identifier.Text))
				scope.LabelMap[gotoLabelStatement!.Identifier.Text] = Root.EmitLabel()-1;
			else
				scope.LabelMap.Add(gotoLabelStatement!.Identifier.Text, Root.EmitLabel()-1);
		}
	}
	
	private void FixGotoLabels(Lua51Prototype proto, string gotoLabel, Scope scope)
	{
		var foundGotoJumps = proto.Instructions.Where(x => x is { JumpType: JumpType.Goto }).ToList();
		foreach (var jmp in foundGotoJumps.Where(jmp => jmp.GotoLabel == gotoLabel))
		{
			jmp.B = scope.LabelMap[jmp.GotoLabel!] - jmp.PC + 1;
			jmp.GotoLabel = null;
		}
	}
	
	protected virtual void CompileStatement(StatementSyntax statement, Scope scope) {
		switch (statement) {
			case ReturnStatementSyntax @return: {
				CompileReturnStatement(@return, scope);
				break;
			}
			case DoStatementSyntax @do: {
				CompileDoStatement(@do, scope);
				break;
			}
			case LocalVariableDeclarationStatementSyntax localVariableDeclarationStatement: {
				CompileLocalVariableDeclarationStatement(localVariableDeclarationStatement, scope);
				break;
			}
			case AssignmentStatementSyntax assignmentStatement: {
				CompileAssignmentStatement(assignmentStatement, scope);
				break;
			}
			case ExpressionStatementSyntax expressionStatementSyntax: {
				CompileExpression(expressionStatementSyntax.Expression, scope);
				break;
			}
			case IfStatementSyntax ifStatementSyntax: {
				CompileIfStatement(ifStatementSyntax, scope);
				break;
			}
			case WhileStatementSyntax whileStatementSyntax: {
				CompileWhileStatement(whileStatementSyntax, scope);
				break;
			}
			case NumericForStatementSyntax numericForStatementSyntax: {
				CompileNumericForStatement(numericForStatementSyntax, scope);
				break;
			}
			case GenericForStatementSyntax genericForStatementSyntax: {
				CompileGenericForStatement(genericForStatementSyntax, scope);
				break;
			}
			case RepeatUntilStatementSyntax repeatUntilStatement:
			{
				CompileRepeatUntilStatement(repeatUntilStatement, scope);
				break;
			}
			case ContinueStatementSyntax continueStatement:
			{
				CompileContinueStatement(continueStatement, scope);
				break;
			}
			case BreakStatementSyntax breakStatement:
			{
				CompileBreakStatement(breakStatement, scope);
				break;
			}
			case GotoLabelStatementSyntax gotoLabelStatement:
			{
				scope.LabelMap.TryAdd(gotoLabelStatement.Identifier.Text, Root.EmitLabel()-1);
				FixGotoLabels(Root, gotoLabelStatement.Identifier.Text, scope);
				break;
			}
			case GotoStatementSyntax gotoStatement:
			{
				Root.Emit(new Lua51Instruction
				{
					Opcode = Lua51Opcode.JMP,
					B = 31,
					GotoLabel = gotoStatement.LabelName.Text,
					JumpType = JumpType.Goto
				});
				break;
			}
			case FunctionDeclarationStatementSyntax functionDeclarationStatement:
			{
				CompileFunctionDeclarationStatement(functionDeclarationStatement, scope);
				break;
			}
			case LocalFunctionDeclarationStatementSyntax localFunctionDeclarationStatement:
			{
				CompileLocalFunctionDeclarationStatement(localFunctionDeclarationStatement, scope);
				break;
			}
			
		}

	}

	public virtual Lua51Prototype CompilePrototype(string source, Scope parentScope = null) {
		var sourceTree = LuaSyntaxTree.ParseText(source, new LuaParseOptions(LuaSyntaxOptions.Lua51));
		var root = sourceTree.GetRoot() as CompilationUnitSyntax;
		var hasErrors = sourceTree.GetDiagnostics().OrderByDescending(diag => diag.Severity)
			.Aggregate(false, (current, diagnostic) => current | diagnostic.Severity == DiagnosticSeverity.Error);
		if (hasErrors)
		{
			var diagnostic = sourceTree.GetDiagnostics().First();
			var lineSpan = diagnostic.Location.GetLineSpan().StartLinePosition.Line + 1;
			var errorCode = diagnostic.ToString();
			var index = errorCode.IndexOf(" LUA", StringComparison.Ordinal);
			errorCode = errorCode[(index + 10)..];
			throw new Exception("Code error at line "+ lineSpan.ToString(CultureInfo.InvariantCulture) + " | Error -> " + errorCode);
		}
		
		
		var scope = new Scope
		{
			StatementList = root!.Statements
		};
		if (parentScope != null)
		{
			scope = parentScope;
		}
		
		
		foreach (var statement in root!.Statements.Statements) {
			CompileStatement(statement, scope);
		}
		
		
		Root.UpvalueCount = scope.UpvalueMap.Count;
		Root.Upvalues = scope.UpvalueMap.Keys.ToList();

		Root.Emit(Lua51Opcode.RETURN, 0, 1);
		return Root;
	}
}

public class LuaPrototypeCompiler : Lua51Compiler { }