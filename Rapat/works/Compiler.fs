namespace Rapat

open System
open System.IO
open System.Text
open System.Collections.Generic

type Compiler =
    static let cl (s: string, c: char) =
        let mutable count = 0
        let len = s.Length
        while count < len && s.[count] = c do
            count <- count + 1
        count

    static let ex = List<string>()
    static let txt = StringBuilder ""
    static let dat = StringBuilder ";gb\n"
    static let main = StringBuilder "define i32 @main() {\n"

    static let vn = List<string>()
    static let vt = List<int>()
    static let fn = List<string>()
    static let ft = List<int>()
    static let fpb = List<string[]>()
    static let fpv = List<bool>()
    static let mutable pms = 0
    static let mutable lc = 0
    static let mutable vc = 0
    static let mutable cb = main
    static let mutable mla = false

    static let rec Run(code: string) =
        let mutable i = 0
        let mutable gb = false
        let mutable gbb = StringBuilder()
        let mutable gbd = 0
        let mutable r = false
        let mutable rn = true
        let mutable rn2 = 0
        let mutable fc = 0
        let mutable note = false
        let mutable nd = 0
        let pb = Stack<string>()
        let mutable raw = false
        let mutable sb = cb

        while i < code.Length do
            let mutable op = code.[i]
            if note then
                if op = ')' then
                    if nd = 0 then
                        note <- false
                    else
                        nd <- nd - 1
                elif op = '(' then
                    nd <- nd + 1
            else
                let fatal (s: string) =
                    ex.Add(sprintf "Fatal: %s At '%c' (%d)." s op i)

                let tfat (t: string) =
                    fatal (sprintf "Type unmatched. Should be %s." t)

                let pfat (i: int) =
                    fatal (sprintf "Parameters size unmatched. Should be %d parameters." i)

                if gb then
                    if raw then
                        gbb.Append op |> ignore
                        raw <- false
                    elif r && (not rn) && gbb.Length < rn2 then
                        gbb.Append op |> ignore
                    else
                        match op with
                        | '(' -> note <- true
                        | '\\' -> raw <- true
                        | '[' ->
                            gbd <- gbd + 1
                            gbb.Append '[' |> ignore
                        | ',' ->
                            if gbd = 0 then
                                pb.Push(gbb.ToString())
                                gbb.Clear() |> ignore
                            else
                                gbb.Append ',' |> ignore
                        | ']' ->
                            if gbd = 0 then
                                if gbb.Length <> 0 then
                                    pb.Push(gbb.ToString())
                                    gbb.Clear() |> ignore
                                gb <- false
                            else
                                gbd <- gbd - 1
                                gbb.Append ']' |> ignore
                        | '.' ->
                            if gbd = 0 then
                                if r && rn then
                                    rn <- false
                                    try
                                        rn2 <- Int32.Parse(gbb.ToString())
                                        gbb.Clear() |> ignore
                                    with _ ->
                                        fatal "Invalid number."
                            else
                                gbb.Append '.' |> ignore
                        | '`' ->
                            if gbd = 0 then
                                r <- true
                            else
                                gbb.Append '`' |> ignore
                        | _ -> gbb.Append op |> ignore
                else
                    match op with
                    | '\t'
                    | '\r'
                    | '\n'
                    | ' ' -> ()
                    | '(' -> note <- true
                    | '[' -> gb <- true
                    | '*' ->
                        if pb.Count = 2 then
                            let mutable a1 = pb.Pop()
                            let mutable a0 = pb.Pop()
                            let mutable iv = false
                            let mutable vi = -1

                            if not (a0.StartsWith("n:") || a0.StartsWith("x:") || a0.StartsWith("0x") || a0.StartsWith("8b") || a0.StartsWith("16b") || a0.StartsWith("64b") || a0.StartsWith("c:")) then
                                for j = 0 to vn.Count - 1 do
                                    if vn.[j] = a0 then
                                        iv <- true
                                        vi <- j

                            let mutable inum = false
                            let mutable nv = ""
                            if a0.StartsWith("n:") then
                                inum <- true
                                nv <- a0.[2..]
                            elif a0.StartsWith("x:") then
                                inum <- true
                                nv <- Convert.ToInt64(a0.[2..], 16).ToString()

                            if cb = main && not mla then
                                main.Append "main:\n" |> ignore
                                mla <- true

                            if iv then
                                match vt.[vi] with
                                | 0 | 1 ->
                                    let t = lc
                                    lc <- lc + 1
                                    cb.Append(sprintf "  %%%d = load i8, i8* @v%d\n" t vi) |> ignore
                                    if a1.StartsWith "n:" then
                                        let p = lc
                                        lc <- lc + 1
                                        cb.Append(sprintf "  %%%d = inttoptr i32 %s to i8*\n" p (a1.[2..])) |> ignore
                                        cb.Append(sprintf "  store i8 %%%d, i8* %%%d\n" t p) |> ignore
                                    elif a1.StartsWith "x:" then
                                        let p = lc
                                        lc <- lc + 1
                                        cb.Append(sprintf "  %%%d = inttoptr i32 %s to i8*\n" p (Convert.ToInt64(a1.[2..], 16).ToString())) |> ignore
                                        cb.Append(sprintf "  store i8 %%%d, i8* %%%d\n" t p) |> ignore
                                    else
                                        tfat "number or hexadecimal"
                                | 2 | 3 ->
                                    let t = lc
                                    lc <- lc + 1
                                    cb.Append(sprintf "  %%%d = load i16, i16* @v%d\n" t vi) |> ignore
                                    if a1.StartsWith "n:" then
                                        let p = lc
                                        lc <- lc + 1
                                        cb.Append(sprintf "  %%%d = inttoptr i32 %s to i16*\n" p (a1.[2..])) |> ignore
                                        cb.Append(sprintf "  store i16 %%%d, i16* %%%d\n" t p) |> ignore
                                    elif a1.StartsWith "x:" then
                                        let p = lc
                                        lc <- lc + 1
                                        cb.Append(sprintf "  %%%d = inttoptr i32 %s to i16*\n" p (Convert.ToInt64(a1.[2..], 16).ToString())) |> ignore
                                        cb.Append(sprintf "  store i16 %%%d, i16* %%%d\n" t p) |> ignore
                                    else
                                        tfat "number or hexadecimal"
                                | 4 | 5 ->
                                    let t = lc
                                    lc <- lc + 1
                                    cb.Append(sprintf "  %%%d = load i64, i64* @v%d\n" t vi) |> ignore
                                    if a1.StartsWith "n:" then
                                        let p = lc
                                        lc <- lc + 1
                                        cb.Append(sprintf "  %%%d = inttoptr i32 %s to i64*\n" p (a1.[2..])) |> ignore
                                        cb.Append(sprintf "  store i64 %%%d, i64* %%%d\n" t p) |> ignore
                                    elif a1.StartsWith "x:" then
                                        let p = lc
                                        lc <- lc + 1
                                        cb.Append(sprintf "  %%%d = inttoptr i32 %s to i64*\n" p (Convert.ToInt64(a1.[2..], 16).ToString())) |> ignore
                                        cb.Append(sprintf "  store i64 %%%d, i64* %%%d\n" t p) |> ignore
                                    else
                                        tfat "number or hexadecimal"
                                | 6 | 7 ->
                                    let t = lc
                                    lc <- lc + 1
                                    cb.Append(sprintf "  %%%d = load i32, i32* @v%d\n" t vi) |> ignore
                                    if a1.StartsWith "n:" then
                                        let p = lc
                                        lc <- lc + 1
                                        cb.Append(sprintf "  %%%d = inttoptr i32 %s to i32*\n" p (a1.[2..])) |> ignore
                                        cb.Append(sprintf "  store i32 %%%d, i32* %%%d\n" t p) |> ignore
                                    elif a1.StartsWith "x:" then
                                        let p = lc
                                        lc <- lc + 1
                                        cb.Append(sprintf "  %%%d = inttoptr i32 %s to i32*\n" p (Convert.ToInt64(a1.[2..], 16).ToString())) |> ignore
                                        cb.Append(sprintf "  store i32 %%%d, i32* %%%d\n" t p) |> ignore
                                    else
                                        tfat "number or hexadecimal"
                                | _ -> tfat "unsupported variable type"
                            elif inum then
                                if a1.StartsWith "n:" then
                                    let p = lc
                                    lc <- lc + 1
                                    cb.Append(sprintf "  %%%d = inttoptr i32 %s to i32*\n" p (a1.[2..])) |> ignore
                                    cb.Append(sprintf "  store i32 %s, i32* %%%d\n" nv p) |> ignore
                                elif a1.StartsWith "x:" then
                                    let p = lc
                                    lc <- lc + 1
                                    cb.Append(sprintf "  %%%d = inttoptr i32 %s to i32*\n" p (Convert.ToInt64(a1.[2..], 16).ToString())) |> ignore
                                    cb.Append(sprintf "  store i32 %s, i32* %%%d\n" nv p) |> ignore
                                else
                                    tfat "number or hexadecimal"
                            else
                                if a0.StartsWith "8bx:" then
                                    let v = Convert.ToInt64(a0.[4..], 16).ToString()
                                    if a1.StartsWith "n:" then
                                        let p = lc
                                        lc <- lc + 1
                                        cb.Append(sprintf "  %%%d = inttoptr i32 %s to i8*\n" p (a1.[2..])) |> ignore
                                        cb.Append(sprintf "  store i8 %s, i8* %%%d\n" v p) |> ignore
                                    elif a1.StartsWith "x:" then
                                        let p = lc
                                        lc <- lc + 1
                                        cb.Append(sprintf "  %%%d = inttoptr i32 %s to i8*\n" p (Convert.ToInt64(a1.[2..], 16).ToString())) |> ignore
                                        cb.Append(sprintf "  store i8 %s, i8* %%%d\n" v p) |> ignore
                                    else
                                        tfat "number or hexadecimal"
                                elif a0.StartsWith "8b:" then
                                    if a1.StartsWith "n:" then
                                        let p = lc
                                        lc <- lc + 1
                                        cb.Append(sprintf "  %%%d = inttoptr i32 %s to i8*\n" p (a1.[2..])) |> ignore
                                        cb.Append(sprintf "  store i8 %s, i8* %%%d\n" (a0.[3..]) p) |> ignore
                                    elif a1.StartsWith "x:" then
                                        let p = lc
                                        lc <- lc + 1
                                        cb.Append(sprintf "  %%%d = inttoptr i32 %s to i8*\n" p (Convert.ToInt64(a1.[2..], 16).ToString())) |> ignore
                                        cb.Append(sprintf "  store i8 %s, i8* %%%d\n" (a0.[3..]) p) |> ignore
                                    else
                                        tfat "number or hexadecimal"
                                elif a0.StartsWith "16bx:" then
                                    let v = Convert.ToInt64(a0.[5..], 16).ToString()
                                    if a1.StartsWith "n:" then
                                        let p = lc
                                        lc <- lc + 1
                                        cb.Append(sprintf "  %%%d = inttoptr i32 %s to i16*\n" p (a1.[2..])) |> ignore
                                        cb.Append(sprintf "  store i16 %s, i16* %%%d\n" v p) |> ignore
                                    elif a1.StartsWith "x:" then
                                        let p = lc
                                        lc <- lc + 1
                                        cb.Append(sprintf "  %%%d = inttoptr i32 %s to i16*\n" p (Convert.ToInt64(a1.[2..], 16).ToString())) |> ignore
                                        cb.Append(sprintf "  store i16 %s, i16* %%%d\n" v p) |> ignore
                                    else
                                        tfat "number or hexadecimal"
                                elif a0.StartsWith "16b:" then
                                    if a1.StartsWith "n:" then
                                        let p = lc
                                        lc <- lc + 1
                                        cb.Append(sprintf "  %%%d = inttoptr i32 %s to i16*\n" p (a1.[2..])) |> ignore
                                        cb.Append(sprintf "  store i16 %s, i16* %%%d\n" (a0.[4..]) p) |> ignore
                                    elif a1.StartsWith "x:" then
                                        let p = lc
                                        lc <- lc + 1
                                        cb.Append(sprintf "  %%%d = inttoptr i32 %s to i16*\n" p (Convert.ToInt64(a1.[2..], 16).ToString())) |> ignore
                                        cb.Append(sprintf "  store i16 %s, i16* %%%d\n" (a0.[4..]) p) |> ignore
                                    else
                                        tfat "number or hexadecimal"
                                elif a0.StartsWith "64bx:" then
                                    let v = Convert.ToInt64(a0.[5..], 16).ToString()
                                    if a1.StartsWith "n:" then
                                        let p = lc
                                        lc <- lc + 1
                                        cb.Append(sprintf "  %%%d = inttoptr i32 %s to i64*\n" p (a1.[2..])) |> ignore
                                        cb.Append(sprintf "  store i64 %s, i64* %%%d\n" v p) |> ignore
                                    elif a1.StartsWith "x:" then
                                        let p = lc
                                        lc <- lc + 1
                                        cb.Append(sprintf "  %%%d = inttoptr i32 %s to i64*\n" p (Convert.ToInt64(a1.[2..], 16).ToString())) |> ignore
                                        cb.Append(sprintf "  store i64 %s, i64* %%%d\n" v p) |> ignore
                                    else
                                        tfat "number or hexadecimal"
                                elif a0.StartsWith "64b:" then
                                    if a1.StartsWith "n:" then
                                        let p = lc
                                        lc <- lc + 1
                                        cb.Append(sprintf "  %%%d = inttoptr i32 %s to i64*\n" p (a1.[2..])) |> ignore
                                        cb.Append(sprintf "  store i64 %s, i64* %%%d\n" (a0.[4..]) p) |> ignore
                                    elif a1.StartsWith "x:" then
                                        let p = lc
                                        lc <- lc + 1
                                        cb.Append(sprintf "  %%%d = inttoptr i32 %s to i64*\n" p (Convert.ToInt64(a1.[2..], 16).ToString())) |> ignore
                                        cb.Append(sprintf "  store i64 %s, i64* %%%d\n" (a0.[4..]) p) |> ignore
                                    else
                                        tfat "number or hexadecimal"
                                elif a0.StartsWith "c:" then
                                    let c = a0.[2..]
                                    if c.Length = 1 then
                                        if a1.StartsWith "n:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(sprintf "  %%%d = inttoptr i32 %s to i8*\n" p (a1.[2..])) |> ignore
                                            cb.Append(sprintf "  store i8 %d, i8* %%%d\n" (int c.[0]) p) |> ignore
                                        elif a1.StartsWith "x:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(sprintf "  %%%d = inttoptr i32 %s to i8*\n" p (Convert.ToInt64(a1.[2..], 16).ToString())) |> ignore
                                            cb.Append(sprintf "  store i8 %d, i8* %%%d\n" (int c.[0]) p) |> ignore
                                        else
                                            tfat "number or hexadecimal"
                                    else
                                        fatal "Character size unmatched. Should be 1 character."
                                else
                                    tfat "8/16/64 bits number or character"
                        else
                            pfat 2
                    | '#' ->
                        let count = pb.Count - 1
                        for i = 0 to count do
                            let c = pb.Pop()
                            if c.StartsWith "ist:" then
                                if cb = main && not mla then
                                    main.Append "main:\n" |> ignore
                                    mla <- true
                                cb.Append(sprintf "  %s\n" (c.[4..].Trim())) |> ignore
                            else
                                tfat "Insert"
                    | '@' ->
                        if pb.Count = 0 then
                            if cb = main && not mla then
                                main.Append "main:\n" |> ignore
                                mla <- true
                            let ll = lc
                            lc <- lc + 1
                            cb.Append(sprintf "  br label %%F%d\nF%d:\n" ll ll) |> ignore
                            i <- code.Length
                        elif pb.Count = 1 then
                            if cb = main && not mla then
                                main.Append "main:\n" |> ignore
                                mla <- true
                            let ll = lc
                            lc <- lc + 1
                            cb.Append(sprintf "F%d:\n" ll) |> ignore
                            let ob = cb
                            let tb = StringBuilder()
                            cb <- tb
                            Run(pb.Pop())
                            cb <- ob
                            ob.Append(tb.ToString()) |> ignore
                            cb.Append(sprintf "  br label %%F%d\n" ll) |> ignore
                        else
                            pfat 1
                    | ':' ->
                        if pb.Count = 2 then
                            let mutable name = pb.Pop().Trim()
                            let mutable d = pb.Pop()
                            let mutable tpe = -1

                            if d.StartsWith "8bx:" then
                                tpe <- 0
                                d <- Convert.ToInt64(d.[4..], 16).ToString()
                            elif d.StartsWith "8b:" then
                                tpe <- 1
                                d <- d.[3..]
                            elif d.StartsWith "16bx:" then
                                tpe <- 2
                                d <- Convert.ToInt64(d.[5..], 16).ToString()
                            elif d.StartsWith "16b:" then
                                tpe <- 3
                                d <- d.[4..]
                            elif d.StartsWith "64bx:" then
                                tpe <- 4
                                d <- Convert.ToInt64(d.[5..], 16).ToString()
                            elif d.StartsWith "64b:" then
                                tpe <- 5
                                d <- d.[4..]
                            elif d.StartsWith "c:" then
                                tpe <- 0
                                d <- d.[2..]
                                if d.Length = 1 then
                                    d <- string (int d.[0])
                                else
                                    fatal "Character size unmatched. Should be 1 character."
                            elif d.StartsWith "n:" then
                                tpe <- 6
                                d <- d.[2..]
                            elif d.StartsWith "x:" then
                                tpe <- 7
                                d <- Convert.ToInt64(d.[2..], 16).ToString()
                            else
                                tfat "supported types"

                            let mutable found = false
                            for j = vn.Count - 1 downto 0 do
                                if vn.[j] = name && vt.[j] = tpe then
                                    match tpe with
                                    | 0 | 1 ->
                                        if cb = main && not mla then
                                            main.Append "main:\n" |> ignore
                                            mla <- true
                                        cb.Append(sprintf "  store i8 %s, i8* @v%d\n" d j) |> ignore
                                    | 2 | 3 ->
                                        if cb = main && not mla then
                                            main.Append "main:\n" |> ignore
                                            mla <- true
                                        cb.Append(sprintf "  store i16 %s, i16* @v%d\n" d j) |> ignore
                                    | 4 | 5 ->
                                        if cb = main && not mla then
                                            main.Append "main:\n" |> ignore
                                            mla <- true
                                        cb.Append(sprintf "  store i64 %s, i64* @v%d\n" d j) |> ignore
                                    | 6 | 7 ->
                                        if cb = main && not mla then
                                            main.Append "main:\n" |> ignore
                                            mla <- true
                                        cb.Append(sprintf "  store i32 %s, i32* @v%d\n" d j) |> ignore
                                    | _ -> tfat "supported types"
                                    found <- true

                            if not found then
                                match tpe with
                                | 0 | 1 -> dat.Append(sprintf "@v%d = global i8 %s\n" vc d) |> ignore
                                | 2 | 3 -> dat.Append(sprintf "@v%d = global i16 %s\n" vc d) |> ignore
                                | 4 | 5 -> dat.Append(sprintf "@v%d = global i64 %s\n" vc d) |> ignore
                                | 6 | 7 -> dat.Append(sprintf "@v%d = global i32 %s\n" vc d) |> ignore
                                | _ -> tfat "supported types"
                                vn.Add name
                                vt.Add tpe
                                vc <- vc + 1
                        else
                            pfat 2
                    | 'f' ->
                        if pb.Count = 0 || pb.Count = 1 then
                            pfat 2
                        elif pb.Count = 2 then
                            let mutable name = pb.Pop().Trim()
                            let mutable c = pb.Pop()
                            let n = fn.Count
                            let fb = StringBuilder()
                            let ob = cb
                            cb <- fb

                            if name.StartsWith "private:" then
                                name <- name.[8..].Trim()
                                fpv.Add true
                            else
                                fpv.Add false

                            fn.Add name
                            Run c

                            let result = gbb.ToString().Trim()
                            if String.IsNullOrEmpty result then
                                ft.Add 0
                            elif result.StartsWith "8b:" then
                                fb.Append(sprintf "  ret i32 %s\n" (result.[3..])) |> ignore
                                ft.Add 1
                            elif result.StartsWith "16b:" then
                                fb.Append(sprintf "  ret i32 %s\n" (result.[4..])) |> ignore
                                ft.Add 1
                            elif result.StartsWith "64b:" then
                                fb.Append(sprintf "  ret i32 %s\n" (result.[4..])) |> ignore
                                ft.Add 1
                            elif result.StartsWith "n:" then
                                fb.Append(sprintf "  ret i32 %s\n" (result.[2..])) |> ignore
                                ft.Add 1
                            elif result.StartsWith "x:" then
                                fb.Append(sprintf "  ret i32 %s\n" (Convert.ToInt64(result.[2..], 16).ToString())) |> ignore
                                ft.Add 1
                            else
                                tfat "supported types"

                            cb <- ob
                            if fpv.[n] then
                                txt.Append(sprintf "define private i32 @f%d() {\n" n) |> ignore
                            else
                                txt.Append(sprintf "define i32 @f%d() {\n" n) |> ignore
                            txt.Append(fb.ToString()) |> ignore
                            txt.Append "}\n" |> ignore
                    | '$' ->
                        if pb.Count = 1 then
                            let lib = pb.Pop()
                            try
                                Run(File.ReadAllText(Path.Combine(AppContext.BaseDirectory, ".lib/", lib)))
                            with _ ->
                                try
                                    Run(File.ReadAllText lib)
                                with _ ->
                                    fatal "Invalid Library."
                        else
                            pfat 1
                    | _ ->
                        let mutable found = false
                        let mutable j = fn.Count - 1
                        while j >= 0 && not found do
                            let name = fn.[j]
                            let mutable k = 0
                            let mutable matched = true
                            try
                                while k < name.Length && matched do
                                    if code.[i + k] <> name.[k] then
                                        matched <- false
                                    k <- k + 1
                                if matched then
                                    i <- i + k - 1
                            with _ -> ()

                            if matched then
                                found <- true
                                let count = pb.Count
                                if count <> 0 then
                                    let fpms = fpb.[j]
                                    if count = fpms.Length then
                                        for idx = 0 to count - 1 do
                                            let fpm = fpms.[idx]
                                            let pm = pb.Pop()
                                            if pm.StartsWith "8b:" then
                                                if fpm = "8 bits number" then
                                                    if cb = main && not mla then
                                                        main.Append "main:\n" |> ignore
                                                        mla <- true
                                                    cb.Append(sprintf "  %%a%d = add i32 %s, 0\n" idx (pm.[3..])) |> ignore
                                                else
                                                    tfat fpm
                                            elif pm.StartsWith "8bx:" then
                                                if fpm = "8 bits hexadecimal" then
                                                    if cb = main && not mla then
                                                        main.Append "main:\n" |> ignore
                                                        mla <- true
                                                    cb.Append(sprintf "  %%a%d = add i32 %s, 0\n" idx (Convert.ToInt64(pm.[4..], 16).ToString())) |> ignore
                                                else
                                                    tfat fpm
                                            elif pm.StartsWith "16b:" then
                                                if fpm = "16 bits number" then
                                                    if cb = main && not mla then
                                                        main.Append "main:\n" |> ignore
                                                        mla <- true
                                                    cb.Append(sprintf "  %%a%d = add i32 %s, 0\n" idx (pm.[4..])) |> ignore
                                                else
                                                    tfat fpm
                                            elif pm.StartsWith "16bx:" then
                                                if fpm = "16 bits hexadecimal" then
                                                    if cb = main && not mla then
                                                        main.Append "main:\n" |> ignore
                                                        mla <- true
                                                    cb.Append(sprintf "  %%a%d = add i32 %s, 0\n" idx (Convert.ToInt64(pm.[5..], 16).ToString())) |> ignore
                                                else
                                                    tfat fpm
                                            elif pm.StartsWith "64b:" then
                                                if fpm = "64 bits number" then
                                                    if cb = main && not mla then
                                                        main.Append "main:\n" |> ignore
                                                        mla <- true
                                                    cb.Append(sprintf "  %%a%d = add i32 %s, 0\n" idx (pm.[4..])) |> ignore
                                                else
                                                    tfat fpm
                                            elif pm.StartsWith "64bx:" then
                                                if fpm = "64 bits hexadecimal" then
                                                    if cb = main && not mla then
                                                        main.Append "main:\n" |> ignore
                                                        mla <- true
                                                    cb.Append(sprintf "  %%a%d = add i32 %s, 0\n" idx (Convert.ToInt64(pm.[5..], 16).ToString())) |> ignore
                                                else
                                                    tfat fpm
                                            elif pm.StartsWith "x:" then
                                                if fpm = "hexadecimal" then
                                                    if cb = main && not mla then
                                                        main.Append "main:\n" |> ignore
                                                        mla <- true
                                                    cb.Append(sprintf "  %%a%d = add i32 %s, 0\n" idx (Convert.ToInt64(pm.[2..], 16).ToString())) |> ignore
                                                else
                                                    tfat fpm
                                            elif pm.StartsWith "n:" then
                                                if fpm = "number" then
                                                    if cb = main && not mla then
                                                        main.Append "main:\n" |> ignore
                                                        mla <- true
                                                    cb.Append(sprintf "  %%a%d = add i32 %s, 0\n" idx (pm.[2..])) |> ignore
                                                else
                                                    tfat fpm
                                            else
                                                tfat "supported types"
                                    else
                                        pfat fpms.Length

                                if cb = main && not mla then
                                    main.Append "main:\n" |> ignore
                                    mla <- true

                                if fpv.[j] then
                                    cb.Append(sprintf "  call i32 @f%d()\n" j) |> ignore
                                else
                                    cb.Append(sprintf "  call i32 @f%d()\n" j) |> ignore
                            j <- j - 1

                        if not found then
                            fatal "Invalid Character."
            i <- i + 1

        if pb.Count = 1 then
            let data = pb.Pop()
            if data.StartsWith "8bx:" then
                if cb = main && not mla then
                    main.Append "main:\n" |> ignore
                    mla <- true
                cb.Append(sprintf "  ret i32 %s\n" (Convert.ToInt64(data.[4..], 16).ToString())) |> ignore
            elif data.StartsWith "8b:" then
                if cb = main && not mla then
                    main.Append "main:\n" |> ignore
                    mla <- true
                cb.Append(sprintf "  ret i32 %s\n" (data.[3..])) |> ignore
            elif data.StartsWith "16bx:" then
                if cb = main && not mla then
                    main.Append "main:\n" |> ignore
                    mla <- true
                cb.Append(sprintf "  ret i32 %s\n" (Convert.ToInt64(data.[5..], 16).ToString())) |> ignore
            elif data.StartsWith "16b:" then
                if cb = main && not mla then
                    main.Append "main:\n" |> ignore
                    mla <- true
                cb.Append(sprintf "  ret i32 %s\n" (data.[4..])) |> ignore
            elif data.StartsWith "64bx:" then
                if cb = main && not mla then
                    main.Append "main:\n" |> ignore
                    mla <- true
                cb.Append(sprintf "  ret i32 %s\n" (Convert.ToInt64(data.[5..], 16).ToString())) |> ignore
            elif data.StartsWith "64b:" then
                if cb = main && not mla then
                    main.Append "main:\n" |> ignore
                    mla <- true
                cb.Append(sprintf "  ret i32 %s\n" (data.[4..])) |> ignore
            elif data.StartsWith "n:" then
                if cb = main && not mla then
                    main.Append "main:\n" |> ignore
                    mla <- true
                cb.Append(sprintf "  ret i32 %s\n" (data.[2..])) |> ignore
            elif data.StartsWith "x:" then
                if cb = main && not mla then
                    main.Append "main:\n" |> ignore
                    mla <- true
                cb.Append(sprintf "  ret i32 %s\n" (Convert.ToInt64(data.[2..], 16).ToString())) |> ignore

        cb <- sb

    static member Compile(code: string) =
        let out = StringBuilder()
        mla <- false
        Run code

        if ex.Count = 0 then
            if main.ToString() <> "define i32 @main() {\n" then
                out.Append main |> ignore
                if not (main.ToString().Contains("ret")) then
                    out.Append "  ret i32 0\n" |> ignore
                out.Append "}\n" |> ignore
            if txt.Length > 0 then
                out.Append txt |> ignore
            if dat.ToString() <> ";gb\n" then
                out.Append dat |> ignore
        else
            failwithf "%s" ("Build Unsuccessful.\n\t" + String.Join("\n\t", ex))

        out.ToString()