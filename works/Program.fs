#if DEBUG
open Rapat
open System.IO
stdout.WriteLine (sprintf "Result:\n%s" (Compiler.Compile(File.ReadAllText @"C:\Tests\test.rpt")))
#else
open System.IO
open Rapat
open System
open System.Diagnostics

let help _ =
    stdout.WriteLine "Usage:"
    stdout.WriteLine "rptc build <source> [<dest>] [<format>] [<architecture>]"
    stdout.WriteLine "\tdest:         output file (default: result.o)"
    stdout.WriteLine "\tformat:       32elf, 64elf, macho, 32win, 64win (default: 32elf)"
    stdout.WriteLine "\tarchitecture: x86, x86-64 (default: x86)"
    stdout.WriteLine "rptc version"
    stdout.WriteLine "rptc help"
    0

let gt fmt arch =
    match fmt, arch with
    | "32elf", "x86" -> "i386-pc-linux-gnu"
    | "64elf", "x86-64" -> "x86_64-pc-linux-gnu"
    | "macho", "x86-64" -> "x86_64-apple-darwin"
    | "32win", "x86" -> "i386-pc-windows-msvc"
    | "64win", "x86-64" -> "x86_64-pc-windows-msvc"
    | "32elf", _ -> "i386-pc-linux-gnu"
    | "64elf", _ -> "x86_64-pc-linux-gnu"
    | "macho", _ -> "x86_64-apple-darwin"
    | "32win", _ -> "i386-pc-windows-msvc"
    | "64win", _ -> "x86_64-pc-windows-msvc"
    | _, "x86" -> "i386-pc-linux-gnu"
    | _, "x86-64" -> "x86_64-pc-linux-gnu"
    | _ -> "i386-pc-linux-gnu"

[<EntryPoint>]
let rec main argd =
    let sd = Path.GetDirectoryName(Environment.GetCommandLineArgs()[0])
    let asm = Path.Combine(sd, "temp.ll")
    if argd.Length = 0 then help()
    else
        if argd.[0].StartsWith '-' then argd.[0] <- argd.[0].TrimStart('-')

        if argd.[0] = "version" then
            stdout.WriteLine "Rapat Compiler Snapshot 20260716-00"
            stdout.WriteLine "Copyright (C) Nam_exx00"
            0
        elif argd.[0] = "help" then help()
        elif argd.[0] = "build" then
            let src = if argd.Length > 1 then argd.[1] else String.Empty
            let dst = if argd.Length > 2 then argd.[2] else "result.o"
            let fmt = if argd.Length > 3 then argd.[3] else "32elf"
            let arch = if argd.Length > 4 then argd.[4] else "x86"
            if src = String.Empty then
                stderr.WriteLine "Fatal: Invalid File."
                -1
            else
                let mutable file = String.Empty
                try
                    file <- File.ReadAllText src
                with
                | _ ->
                    stderr.WriteLine "Fatal: Invalid File."
                try
                    File.WriteAllText (Path.Combine(sd, "temp.ll"), Compiler.Compile file)
                    let clang = ProcessStartInfo()
                    clang.FileName <- Path.Combine(sd, "clang.exe")
                    let triple = gt fmt arch
                    clang.Arguments <- $"-target {triple} -c {asm} -o {dst}"
                    clang.UseShellExecute <- false
                    clang.CreateNoWindow <- true
                    clang.RedirectStandardOutput <- false
                    clang.RedirectStandardError <- false
                    Process.Start(clang).WaitForExit() |> ignore
                    File.Delete(Path.Combine(sd, "temp.ll"))
                    0
                with
                | error ->
                    stderr.WriteLine error.Message
                    -1
        else 
            stderr.WriteLine "Fatal: Invalid Option."
            -1
#endif