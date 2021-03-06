using System;
using Nuke.Common;
using Nuke.Common.Git;
using Nuke.Common.ProjectModel;
using Nuke.Common.Tooling;
using Nuke.Common.Tools.DotNet;
using Nuke.Common.Tools.Git;
using Nuke.Common.Tools.GitVersion;
using Nuke.Common.Tools.Paket;
using static Nuke.Common.IO.FileSystemTasks;
using static Nuke.Common.IO.PathConstruction;
using static Nuke.Common.Tools.DotNet.DotNetTasks;

class Build : NukeBuild
{
    public static int Main() => Execute<Build>(x => x.Compile);
    static Build()
    {
        Environment.SetEnvironmentVariable("GITVERSION_EXE", RootDirectory / "packages" / "nukebuild" / "GitVersion.CommandLine.DotNetCore" / "tools" / "GitVersion.dll");
        Environment.SetEnvironmentVariable("PAKET_EXE", RootDirectory / ".paket" / "paket.exe");
    }

    [Parameter("Configuration to build - Default is 'Debug' (local) or 'Release' (server)")]
    readonly string Configuration = IsLocalBuild ? "Debug" : "Release";

    [Solution("JsonProvider.sln")] readonly Solution Solution;
    [GitRepository] readonly GitRepository GitRepository;
    [GitVersion] readonly GitVersion GitVersion;

    AbsolutePath SourceDirectory => RootDirectory / "src";
    AbsolutePath TestsDirectory => RootDirectory / "tests";
    AbsolutePath OutputDirectory => RootDirectory / "output";

    Target Clean => _ => _
        .Executes(() =>
        {
            DeleteDirectories(GlobDirectories(SourceDirectory, "**/bin", "**/obj"));
            DeleteDirectories(GlobDirectories(TestsDirectory, "**/bin", "**/obj"));
            EnsureCleanDirectory(OutputDirectory);
        });

    Target Restore => _ => _
        .DependsOn(Clean)
        .Executes(() =>
        {
            DotNetRestore();
        });

    Target Compile => _ => _
        .DependsOn(Restore)
        .Executes(() =>
        {
            DotNetBuild(s => s
                .SetProjectFile(Solution)
                .EnableNoRestore()
                .SetConfiguration(Configuration)
                .SetAssemblyVersion(GitVersion.GetNormalizedAssemblyVersion())
                .SetFileVersion(GitVersion.GetNormalizedFileVersion())
                .SetInformationalVersion(GitVersion.InformationalVersion));
        });

    Target Test => _ => _
        .DependsOn(Compile)
        .Executes(() =>
        {
            DotNetTest(s => s
                .SetConfiguration(Configuration)
                .EnableNoBuild()
                .SetResultsDirectory(OutputDirectory / "tests"));
        });

    Target Pack => _ => _
        .DependsOn(Compile)
        .Executes(() =>
        {
            PaketTasks.PaketPack(s => s
                .SetTemplateFile(RootDirectory / "src" / "JsonProvider.Runtime" / "paket.template")
                .SetBuildConfiguration(Configuration)
                .EnableSymbols()
                .SetOutputDirectory(OutputDirectory)
                .SetPackageVersion(GitVersion.NuGetVersionV2));
        });
}
