[![NuGet](https://img.shields.io/nuget/v/FSharp.Data.JsonProvider.svg)](https://www.nuget.org/packages/FSharp.Data.JsonProvider)

# Json generative type provider

This is a F# type provider which allows you to generate types from string json sample and then use them in F# or C# project.

![Screenshot](docs/preview.png)

## Usage

You can use it to generate types from json samples (files, web resources, string literals) at design time and then use them in your F#\C# projects.

## Examples

In F#:

    open FSharp.Data.JsonProvider

    type FileType = JsonProvider<"file.txt">

    type RelativeFileType = JsonProvider<"../file.txt">

    type WebType = JsonProvider<"https://jsonplaceholder.typicode.com/todos/1">

    type StringType = JsonProvider<""" { "Data": 1 } """>

## Status

| OS      | Build & Test |
|---------|--------------|
| Mac OS  | [![Build Status](https://dev.azure.com/GithubProjects/JsonProvider/_apis/build/status/Liminiens.json-provider)](https://dev.azure.com/GithubProjects/JsonProvider/_build/latest?definitionId=1&branchName=master&jobname=macOS_10_13) |
| Linux   | [![Build Status](https://dev.azure.com/GithubProjects/JsonProvider/_apis/build/status/Liminiens.json-provider)](https://dev.azure.com/GithubProjects/JsonProvider/_build/latest?definitionId=1&branchName=master&jobname=ubuntu_16_04) |
| Windows | [![Build Status](https://dev.azure.com/GithubProjects/JsonProvider/_apis/build/status/Liminiens.json-provider)](https://dev.azure.com/GithubProjects/JsonProvider/_build/latest?definitionId=1&branchName=master&jobname=vs2017_win2016) |

Paket is used to acquire the type provider SDK and build the nuget package.

Build:

    .\build.ps1 --target Build --Configuration Release

Pack:

    .\build.ps1 --target Pack --Configuration Release

    
[![Built with NUKE](http://nuke.build/squared)](https://nuke.build)

