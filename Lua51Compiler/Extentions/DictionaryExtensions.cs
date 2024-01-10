namespace LuaCompiler.Extentions;

public static class DictionaryExtensions
{
    public static Dictionary<TKey, TValue> DeepClone<TKey, TValue>(this Dictionary<TKey, TValue> original) where TKey : notnull
    {
        return original.ToDictionary(kvp => kvp.Key, kvp => kvp.Value);
    }
}