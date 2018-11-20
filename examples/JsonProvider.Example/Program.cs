using JsonProvider.Provider;
using System;
using System.Linq;

namespace JsonProvider.Example
{
    class Program
    {
        static void Main(string[] args)
        {
            var sample = JsonStringType.GetSample();
            Print(sample);
            var parsed = JsonStringType.Parse("{ \"Data\": [{ \"Test\": 1, \"Array\": [2, 3] }] }");
            Print(parsed);
            Console.WriteLine();

            var fileSample = JsonFromFile.GetSample();
            fileSample.WebApp.Servlet
                .Select(x => x.ServletName)
                .ToList()
                .ForEach(Console.WriteLine);
            Console.WriteLine();

            foreach (var comment in JsonFromWeb.GetSample())
            {
                Console.WriteLine($"Name: {comment.Name}; Body: {comment.Body}\n");
            }
        }

        static void Print(JsonStringType.SuperRoot data)
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
