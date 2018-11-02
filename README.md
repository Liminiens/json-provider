# Json generative type provider



## Status

| OS      | Build & Test |
|---------|--------------|
| Mac OS  | [![Build Status](https://dev.azure.com/JsonProvider/JsonProvider/_apis/build/status/Liminiens.json-provider)](https://dev.azure.com/JsonProvider/JsonProvider/_build/latest?definitionId=1&branchName=master&jobname=macOS_10_13) |
| Linux   | [![Build Status](https://dev.azure.com/JsonProvider/JsonProvider/_apis/build/status/Liminiens.json-provider)](https://dev.azure.com/JsonProvider/JsonProvider/_build/latest?definitionId=1&branchName=master&jobname=ubuntu_16_04) |
| Windows | [![Build Status](https://dev.azure.com/JsonProvider/JsonProvider/_apis/build/status/Liminiens.json-provider)](https://dev.azure.com/JsonProvider/JsonProvider/_build/latest?definitionId=1&branchName=master&jobname=vs2017_win2016) |

This is a simple F# type provider.  It has separate design-time and runtime assemblies.

Paket is used to acquire the type provider SDK and build the nuget package (you can remove this use of paket if you like)

Building:

    .paket\paket.exe update

    dotnet build -c release

    .paket\paket.exe pack src\JsonProvider.Runtime\paket.template --version 0.0.1
    
    
[![Built with NUKE](http://nuke.build/squared)](https://nuke.build)
