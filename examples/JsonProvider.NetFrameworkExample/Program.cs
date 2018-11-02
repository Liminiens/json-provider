using System;
using JsonProvider.NetFrameworkProvider;

namespace JsonProvider.NetFrameworkExample
{
    class Program
    {
        static void Main(string[] args)
        {
            var sample = ProviderExampleType.GetSample();
            Print(sample);
            var parsed = ProviderExampleType.Parse("{ \"Data\": [{ \"Test\": 1, \"Array\": [2, 3] }] }");
            Print(parsed);
        }

        static void Print(ProviderExampleType.Root data)
        {
            foreach (var el in data.Data)
            {
                foreach (var value in el.Array)
                {
                    Console.WriteLine(value);
                }
            }
        }
    }
}
