using System.Text.RegularExpressions;

namespace Lua51Compiler.IR;

public enum ConstantType {
    Boolean,
    Nil,
    Double,
    String,
    Unknown
}

public class Lua51Constant {
    
    public List<Lua51Instruction> References = new();

    public object? Data;
    public ConstantType Type;
    
    public string EscapeData() {

        var dataString = (string)Data!;
        dataString = dataString.Replace("\\", "\\\\");
        dataString = dataString.Replace("\n", "\\n");
        dataString = dataString.Replace("\r", "\\r");
        dataString = dataString.Replace("\t", "\\t");
        dataString = dataString.Replace("\v", "\\v");
        dataString = dataString.Replace("\a", "\\a");
        dataString = dataString.Replace("\b", "\\b");
        dataString = dataString.Replace("\f", "\\f");
        dataString = dataString.Replace("\"", "\\\"");
        dataString = dataString.Replace("'", "\\'");

        return dataString;
    }

    public override string ToString() {
        switch (Type) {
            case ConstantType.Boolean:
                return (bool)Data ? "true" : "false";

            case ConstantType.Nil:
                return "nil";

            case ConstantType.Double:
                return Data.ToString();

            case ConstantType.String:
                return "\"" + EscapeData() + "\"";

            case ConstantType.Unknown:
                return Data.ToString();
        }

        throw new Exception("Undefined constant type");
    }

    public Lua51Constant(object data, ConstantType type) {
        Data = data;
        Type = type;
    }

    public Lua51Constant Clone() {
        return (Lua51Constant)MemberwiseClone();
    }
}