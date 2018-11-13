[![NuGet](https://img.shields.io/nuget/v/FSharp.Data.JsonProvider.svg)](https://www.nuget.org/packages/FSharp.Data.JsonProvider)

# Json generative type provider

This is a F# type provider which allows you to generate types from string json sample and then use them in F# or C# project.

![Screenshot](docs/preview.png)

## Status

| OS      | Build & Test |
|---------|--------------|
| Mac OS  | [![Build Status](https://dev.azure.com/JsonProvider/JsonProvider/_apis/build/status/Liminiens.json-provider)](https://dev.azure.com/JsonProvider/JsonProvider/_build/latest?definitionId=1&branchName=master&jobname=macOS_10_13) |
| Linux   | [![Build Status](https://dev.azure.com/JsonProvider/JsonProvider/_apis/build/status/Liminiens.json-provider)](https://dev.azure.com/JsonProvider/JsonProvider/_build/latest?definitionId=1&branchName=master&jobname=ubuntu_16_04) |
| Windows | [![Build Status](https://dev.azure.com/JsonProvider/JsonProvider/_apis/build/status/Liminiens.json-provider)](https://dev.azure.com/JsonProvider/JsonProvider/_build/latest?definitionId=1&branchName=master&jobname=vs2017_win2016) |

Paket is used to acquire the type provider SDK and build the nuget package.

Building:

    .paket\paket.exe update

    dotnet build -c release

    .paket\paket.exe pack src\JsonProvider.Runtime\paket.template --version 0.0.1
