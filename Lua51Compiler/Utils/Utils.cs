using Loretta.CodeAnalysis;
using Loretta.CodeAnalysis.Lua;
using Loretta.CodeAnalysis.Lua.Syntax;

namespace Lua51Compiler.Utils;

public class Utils
{
    public static List<ExpressionSyntax> GetConcatExpressions(BinaryExpressionSyntax expr)
    {
        var concatExpressions = new List<ExpressionSyntax>();
        var left = expr.Left;
        var right = expr.Right;
        if (!left.IsKind(SyntaxKind.ConcatExpression))
        {
            concatExpressions.Add(left);
        }
        else
        {
            var innerConcatExpressions = GetConcatExpressions((BinaryExpressionSyntax) left);
            concatExpressions.AddRange(innerConcatExpressions);
        }
        concatExpressions.Add(right);
        return concatExpressions;
    }
    
    
    
}