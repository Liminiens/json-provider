using System;
using JsonProvider.Provider;
using Newtonsoft.Json.Linq;

namespace JsonProvider.Example
{
    class Program
    {
        static void Main(string[] args)
        {
            var data = ProviderExampleType.Value.Data;
            foreach (var value in data)
            {
                Console.WriteLine(value);
            }
        }
    }
}
