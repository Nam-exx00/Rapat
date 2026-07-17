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
    static let main = StringBuilder "define i32 @main() {\nmain:\n"

    static let vn = List<string>()
    static let vt = List<int>()
    static let fn = List<string>()
    static let ft = List<int>()
    static let fpb = List<string[]>()
    static let fpv = List<bool>()
    static let mutable pms = 0
    static let mutable lc = 1
    static let mutable vc = 0
    static let mutable cb = main
    static let mutable mla = true
    static let mutable pn = List<string>()
    static let mutable pt = List<string>()
    static let mutable pid = List<int>()
    static let mutable bare = false 
    static let mutable rc = false
    static let lvn = List<string>()
    static let lvt = List<int>()
    static let lvp = List<int>()

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
                    ex.Add(String.Format("Fatal: {0} At '{1}' ({2}).", s, op, i))

                let tfat (t: string) =
                    fatal (String.Format("Type unmatched. Should be {0}.", t))

                let pfat (i: int) =
                    fatal (String.Format("Parameters size unmatched. Should be {0} parameters.", i))

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
                                    let expr = gbb.ToString()
                                    let ho = expr.Contains("+") || expr.Contains("-") || expr.Contains("*") || expr.Contains("/")
                                    if ho then
                                        let parts = expr.Split(',')
                                        let v = parts.[0]
                                        let n = if parts.Length > 1 then parts.[1].Trim() else ""
                                        let mutable ns = List<int>()
                                        let mutable os = List<char>()
                                        let mutable j = 0
                                        while j < v.Length do
                                            let c = v.[j]
                                            if c = 'n' && j + 1 < v.Length && v.[j+1] = ':' then
                                                let mutable num = 0
                                                let mutable k = j + 2
                                                while k < v.Length && System.Char.IsDigit(v.[k]) do
                                                    num <- num * 10 + (int v.[k] - 48)
                                                    k <- k + 1
                                                ns.Add(num)
                                                j <- k
                                            elif System.Char.IsDigit(c) then
                                                let mutable num = 0
                                                let mutable k = j
                                                while k < v.Length && System.Char.IsDigit(v.[k]) do
                                                    num <- num * 10 + (int v.[k] - 48)
                                                    k <- k + 1
                                                ns.Add(num)
                                                j <- k
                                            elif "+-*/".Contains(c) then
                                                os.Add(c)
                                                j <- j + 1
                                            else
                                                j <- j + 1
                                        if ns.Count > 0 then
                                            let mutable r = -1
                                            for i = 0 to ns.Count - 1 do
                                                let r2 = lc
                                                lc <- lc + 1
                                                cb.Append(String.Format("  %{0} = add i32 {1}, 0\n", r2, ns.[i])) |> ignore
                                                if r = -1 then
                                                    r <- r2
                                                else
                                                    let nr = lc
                                                    lc <- lc + 1
                                                    match os.[i-1] with
                                                    | '+' -> cb.Append(String.Format("  %{0} = add i32 %{1}, %{2}\n", nr, r, r2)) |> ignore
                                                    | '-' -> cb.Append(String.Format("  %{0} = sub i32 %{1}, %{2}\n", nr, r, r2)) |> ignore
                                                    | '*' -> cb.Append(String.Format("  %{0} = mul i32 %{1}, %{2}\n", nr, r, r2)) |> ignore
                                                    | '/' -> 
                                                        if ns.[i] = 0 then
                                                            fatal "Division by zero"
                                                        cb.Append(String.Format("  %{0} = sdiv i32 %{1}, %{2}\n", nr, r, r2)) |> ignore
                                                    | _ -> ()
                                                    r <- nr
                                            if n <> "" then
                                                pb.Push(String.Format("n:{0}", r))
                                                pb.Push(n)
                                            else
                                                pb.Push(String.Format("n:{0}", r))
                                            gbb.Clear() |> ignore
                                            gb <- false
                                        else
                                            pb.Push(gbb.ToString())
                                            gbb.Clear() |> ignore
                                            gb <- false
                                    else
                                        pb.Push(gbb.ToString())
                                        gbb.Clear() |> ignore
                                        gb <- false
                                else
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
                        if bare then
                            if pb.Count = 2 then
                                let mutable a1 = pb.Pop()
                                let mutable a0 = pb.Pop()
                                let mutable iv = false
                                let mutable vi = -1
                                let mutable ip2 = false
                                let mutable pi2 = -1
                                let mutable il = false
                                let mutable li = -1

                                if not (a0.StartsWith("n:") || a0.StartsWith("x:") || a0.StartsWith("0x") || a0.StartsWith("8b") || a0.StartsWith("16b") || a0.StartsWith("64b") || a0.StartsWith("c:")) then
                                    for j = 0 to lvn.Count - 1 do
                                        if lvn.[j] = a0 then
                                            il <- true
                                            li <- j
                                    if not il then
                                        for j = 0 to pn.Count - 1 do
                                            if pn.[j] = a0 then
                                                ip2 <- true
                                                pi2 <- j
                                    if not il && not ip2 then
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

                                if il then
                                    let ptr = lvp.[li]
                                    match lvt.[li] with
                                    | 0 | 1 ->
                                        let t = lc
                                        lc <- lc + 1
                                        cb.Append(String.Format("  %{0} = load i8, i8* %{1}\n", t, ptr)) |> ignore
                                        if a1.StartsWith "n:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i8*\n", p, a1.[2..])) |> ignore
                                            cb.Append(String.Format("  store i8 %{0}, i8* %{1}\n", t, p)) |> ignore
                                        elif a1.StartsWith "x:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i8*\n", p, Convert.ToInt64(a1.[2..], 16).ToString())) |> ignore
                                            cb.Append(String.Format("  store i8 %{0}, i8* %{1}\n", t, p)) |> ignore
                                        else
                                            tfat "number or hexadecimal"
                                    | 2 | 3 ->
                                        let t = lc
                                        lc <- lc + 1
                                        cb.Append(String.Format("  %{0} = load i16, i16* %{1}\n", t, ptr)) |> ignore
                                        if a1.StartsWith "n:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i16*\n", p, a1.[2..])) |> ignore
                                            cb.Append(String.Format("  store i16 %{0}, i16* %{1}\n", t, p)) |> ignore
                                        elif a1.StartsWith "x:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i16*\n", p, Convert.ToInt64(a1.[2..], 16).ToString())) |> ignore
                                            cb.Append(String.Format("  store i16 %{0}, i16* %{1}\n", t, p)) |> ignore
                                        else
                                            tfat "number or hexadecimal"
                                    | 4 | 5 ->
                                        let t = lc
                                        lc <- lc + 1
                                        cb.Append(String.Format("  %{0} = load i64, i64* %{1}\n", t, ptr)) |> ignore
                                        if a1.StartsWith "n:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i64*\n", p, a1.[2..])) |> ignore
                                            cb.Append(String.Format("  store i64 %{0}, i64* %{1}\n", t, p)) |> ignore
                                        elif a1.StartsWith "x:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i64*\n", p, Convert.ToInt64(a1.[2..], 16).ToString())) |> ignore
                                            cb.Append(String.Format("  store i64 %{0}, i64* %{1}\n", t, p)) |> ignore
                                        else
                                            tfat "number or hexadecimal"
                                    | 6 | 7 ->
                                        let t = lc
                                        lc <- lc + 1
                                        cb.Append(String.Format("  %{0} = load i32, i32* %{1}\n", t, ptr)) |> ignore
                                        if a1.StartsWith "n:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i32*\n", p, a1.[2..])) |> ignore
                                            cb.Append(String.Format("  store i32 %{0}, i32* %{1}\n", t, p)) |> ignore
                                        elif a1.StartsWith "x:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i32*\n", p, Convert.ToInt64(a1.[2..], 16).ToString())) |> ignore
                                            cb.Append(String.Format("  store i32 %{0}, i32* %{1}\n", t, p)) |> ignore
                                        else
                                            tfat "number or hexadecimal"
                                    | _ -> tfat "unsupported variable type"
                                elif ip2 then
                                    let pi = pid.[pi2]
                                    match pt.[pi2] with
                                    | "i8" ->
                                        if a1.StartsWith "n:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i8*\n", p, a1.[2..])) |> ignore
                                            cb.Append(String.Format("  store i8 %p{0}, i8* %{1}\n", pi, p)) |> ignore
                                        elif a1.StartsWith "x:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i8*\n", p, Convert.ToInt64(a1.[2..], 16).ToString())) |> ignore
                                            cb.Append(String.Format("  store i8 %p{0}, i8* %{1}\n", pi, p)) |> ignore
                                        else
                                            tfat "number or hexadecimal"
                                    | "i16" ->
                                        if a1.StartsWith "n:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i16*\n", p, a1.[2..])) |> ignore
                                            cb.Append(String.Format("  store i16 %p{0}, i16* %{1}\n", pi, p)) |> ignore
                                        elif a1.StartsWith "x:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i16*\n", p, Convert.ToInt64(a1.[2..], 16).ToString())) |> ignore
                                            cb.Append(String.Format("  store i16 %p{0}, i16* %{1}\n", pi, p)) |> ignore
                                        else
                                            tfat "number or hexadecimal"
                                    | "i32" ->
                                        if a1.StartsWith "n:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i32*\n", p, a1.[2..])) |> ignore
                                            cb.Append(String.Format("  store i32 %p{0}, i32* %{1}\n", pi, p)) |> ignore
                                        elif a1.StartsWith "x:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i32*\n", p, Convert.ToInt64(a1.[2..], 16).ToString())) |> ignore
                                            cb.Append(String.Format("  store i32 %p{0}, i32* %{1}\n", pi, p)) |> ignore
                                        else
                                            tfat "number or hexadecimal"
                                    | "i64" ->
                                        if a1.StartsWith "n:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i64*\n", p, a1.[2..])) |> ignore
                                            cb.Append(String.Format("  store i64 %p{0}, i64* %{1}\n", pi, p)) |> ignore
                                        elif a1.StartsWith "x:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i64*\n", p, Convert.ToInt64(a1.[2..], 16).ToString())) |> ignore
                                            cb.Append(String.Format("  store i64 %p{0}, i64* %{1}\n", pi, p)) |> ignore
                                        else
                                            tfat "number or hexadecimal"
                                    | "i8*" ->
                                        if a1.StartsWith "n:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i8*\n", p, a1.[2..])) |> ignore
                                            cb.Append(String.Format("  store i8* %p{0}, i8* %{1}\n", pi, p)) |> ignore
                                        elif a1.StartsWith "x:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i8*\n", p, Convert.ToInt64(a1.[2..], 16).ToString())) |> ignore
                                            cb.Append(String.Format("  store i8* %p{0}, i8* %{1}\n", pi, p)) |> ignore
                                        else
                                            tfat "number or hexadecimal"
                                    | _ -> tfat "unsupported parameter type"
                                elif iv then
                                    match vt.[vi] with
                                    | 0 | 1 ->
                                        let t = lc
                                        lc <- lc + 1
                                        cb.Append(String.Format("  %{0} = load i8, i8* @v{1}\n", t, vi)) |> ignore
                                        if a1.StartsWith "n:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i8*\n", p, a1.[2..])) |> ignore
                                            cb.Append(String.Format("  store i8 %{0}, i8* %{1}\n", t, p)) |> ignore
                                        elif a1.StartsWith "x:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i8*\n", p, Convert.ToInt64(a1.[2..], 16).ToString())) |> ignore
                                            cb.Append(String.Format("  store i8 %{0}, i8* %{1}\n", t, p)) |> ignore
                                        else
                                            tfat "number or hexadecimal"
                                    | 2 | 3 ->
                                        let t = lc
                                        lc <- lc + 1
                                        cb.Append(String.Format("  %{0} = load i16, i16* @v{1}\n", t, vi)) |> ignore
                                        if a1.StartsWith "n:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i16*\n", p, a1.[2..])) |> ignore
                                            cb.Append(String.Format("  store i16 %{0}, i16* %{1}\n", t, p)) |> ignore
                                        elif a1.StartsWith "x:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i16*\n", p, Convert.ToInt64(a1.[2..], 16).ToString())) |> ignore
                                            cb.Append(String.Format("  store i16 %{0}, i16* %{1}\n", t, p)) |> ignore
                                        else
                                            tfat "number or hexadecimal"
                                    | 4 | 5 ->
                                        let t = lc
                                        lc <- lc + 1
                                        cb.Append(String.Format("  %{0} = load i64, i64* @v{1}\n", t, vi)) |> ignore
                                        if a1.StartsWith "n:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i64*\n", p, a1.[2..])) |> ignore
                                            cb.Append(String.Format("  store i64 %{0}, i64* %{1}\n", t, p)) |> ignore
                                        elif a1.StartsWith "x:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i64*\n", p, Convert.ToInt64(a1.[2..], 16).ToString())) |> ignore
                                            cb.Append(String.Format("  store i64 %{0}, i64* %{1}\n", t, p)) |> ignore
                                        else
                                            tfat "number or hexadecimal"
                                    | 6 | 7 ->
                                        let t = lc
                                        lc <- lc + 1
                                        cb.Append(String.Format("  %{0} = load i32, i32* @v{1}\n", t, vi)) |> ignore
                                        if a1.StartsWith "n:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i32*\n", p, a1.[2..])) |> ignore
                                            cb.Append(String.Format("  store i32 %{0}, i32* %{1}\n", t, p)) |> ignore
                                        elif a1.StartsWith "x:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i32*\n", p, Convert.ToInt64(a1.[2..], 16).ToString())) |> ignore
                                            cb.Append(String.Format("  store i32 %{0}, i32* %{1}\n", t, p)) |> ignore
                                        else
                                            tfat "number or hexadecimal"
                                    | _ -> tfat "unsupported variable type"
                                elif inum then
                                    if a1.StartsWith "n:" then
                                        let p = lc
                                        lc <- lc + 1
                                        cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i32*\n", p, a1.[2..])) |> ignore
                                        cb.Append(String.Format("  store i32 {0}, i32* %{1}\n", nv, p)) |> ignore
                                    elif a1.StartsWith "x:" then
                                        let p = lc
                                        lc <- lc + 1
                                        cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i32*\n", p, Convert.ToInt64(a1.[2..], 16).ToString())) |> ignore
                                        cb.Append(String.Format("  store i32 {0}, i32* %{1}\n", nv, p)) |> ignore
                                    else
                                        tfat "number or hexadecimal"
                                else
                                    if a0.StartsWith "8bx:" then
                                        let v = Convert.ToInt64(a0.[4..], 16).ToString()
                                        if a1.StartsWith "n:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i8*\n", p, a1.[2..])) |> ignore
                                            cb.Append(String.Format("  store i8 {0}, i8* %{1}\n", v, p)) |> ignore
                                        elif a1.StartsWith "x:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i8*\n", p, Convert.ToInt64(a1.[2..], 16).ToString())) |> ignore
                                            cb.Append(String.Format("  store i8 {0}, i8* %{1}\n", v, p)) |> ignore
                                        else
                                            tfat "number or hexadecimal"
                                    elif a0.StartsWith "8b:" then
                                        if a1.StartsWith "n:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i8*\n", p, a1.[2..])) |> ignore
                                            cb.Append(String.Format("  store i8 {0}, i8* %{1}\n", a0.[3..], p)) |> ignore
                                        elif a1.StartsWith "x:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i8*\n", p, Convert.ToInt64(a1.[2..], 16).ToString())) |> ignore
                                            cb.Append(String.Format("  store i8 {0}, i8* %{1}\n", a0.[3..], p)) |> ignore
                                        else
                                            tfat "number or hexadecimal"
                                    elif a0.StartsWith "16bx:" then
                                        let v = Convert.ToInt64(a0.[5..], 16).ToString()
                                        if a1.StartsWith "n:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i16*\n", p, a1.[2..])) |> ignore
                                            cb.Append(String.Format("  store i16 {0}, i16* %{1}\n", v, p)) |> ignore
                                        elif a1.StartsWith "x:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i16*\n", p, Convert.ToInt64(a1.[2..], 16).ToString())) |> ignore
                                            cb.Append(String.Format("  store i16 {0}, i16* %{1}\n", v, p)) |> ignore
                                        else
                                            tfat "number or hexadecimal"
                                    elif a0.StartsWith "16b:" then
                                        if a1.StartsWith "n:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i16*\n", p, a1.[2..])) |> ignore
                                            cb.Append(String.Format("  store i16 {0}, i16* %{1}\n", a0.[4..], p)) |> ignore
                                        elif a1.StartsWith "x:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i16*\n", p, Convert.ToInt64(a1.[2..], 16).ToString())) |> ignore
                                            cb.Append(String.Format("  store i16 {0}, i16* %{1}\n", a0.[4..], p)) |> ignore
                                        else
                                            tfat "number or hexadecimal"
                                    elif a0.StartsWith "64bx:" then
                                        let v = Convert.ToInt64(a0.[5..], 16).ToString()
                                        if a1.StartsWith "n:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i64*\n", p, a1.[2..])) |> ignore
                                            cb.Append(String.Format("  store i64 {0}, i64* %{1}\n", v, p)) |> ignore
                                        elif a1.StartsWith "x:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i64*\n", p, Convert.ToInt64(a1.[2..], 16).ToString())) |> ignore
                                            cb.Append(String.Format("  store i64 {0}, i64* %{1}\n", v, p)) |> ignore
                                        else
                                            tfat "number or hexadecimal"
                                    elif a0.StartsWith "64b:" then
                                        if a1.StartsWith "n:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i64*\n", p, a1.[2..])) |> ignore
                                            cb.Append(String.Format("  store i64 {0}, i64* %{1}\n", a0.[4..], p)) |> ignore
                                        elif a1.StartsWith "x:" then
                                            let p = lc
                                            lc <- lc + 1
                                            cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i64*\n", p, Convert.ToInt64(a1.[2..], 16).ToString())) |> ignore
                                            cb.Append(String.Format("  store i64 {0}, i64* %{1}\n", a0.[4..], p)) |> ignore
                                        else
                                            tfat "number or hexadecimal"
                                    elif a0.StartsWith "c:" then
                                        let c = a0.[2..]
                                        if c.Length = 1 then
                                            if a1.StartsWith "n:" then
                                                let p = lc
                                                lc <- lc + 1
                                                cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i8*\n", p, a1.[2..])) |> ignore
                                                cb.Append(String.Format("  store i8 {0}, i8* %{1}\n", int c.[0], p)) |> ignore
                                            elif a1.StartsWith "x:" then
                                                let p = lc
                                                lc <- lc + 1
                                                cb.Append(String.Format("  %{0} = inttoptr i32 {1} to i8*\n", p, Convert.ToInt64(a1.[2..], 16).ToString())) |> ignore
                                                cb.Append(String.Format("  store i8 {0}, i8* %{1}\n", int c.[0], p)) |> ignore
                                            else
                                                tfat "number or hexadecimal"
                                        else
                                            fatal "Character size unmatched. Should be 1 character."
                                    else
                                        tfat "8/16/64 bits number or character"
                            else
                                pfat 2
                        else fatal "The action is dangerous. But dangerous option disabled."
                    | '#' ->
                        if bare then
                            let count = pb.Count - 1
                            for i = 0 to count do
                                let c = pb.Pop()
                                if c.StartsWith "ist:" then
                                    if cb = main && not mla then
                                        main.Append "main:\n" |> ignore
                                        mla <- true
                                    cb.Append(String.Format("  {0}\n", c.[4..].Trim())) |> ignore
                                else
                                    tfat "Insert"
                        else fatal "The action is dangerous. But dangerous option disabled."
                    | '@' ->
                        if pb.Count = 0 then
                            if cb = main && not mla then
                                main.Append "main:\n" |> ignore
                                mla <- true
                            let ll = lc
                            lc <- lc + 1
                            cb.Append(String.Format("  br label %F{0}\nF{0}:\n", ll)) |> ignore
                            i <- code.Length
                        elif pb.Count = 1 then
                            if cb = main && not mla then
                                main.Append "main:\n" |> ignore
                                mla <- true
                            let ll = lc
                            lc <- lc + 1
                            cb.Append(String.Format("F{0}:\n", ll)) |> ignore
                            let ob = cb
                            let tb = StringBuilder()
                            cb <- tb
                            Run(pb.Pop())
                            cb <- ob
                            ob.Append(tb.ToString()) |> ignore
                            cb.Append(String.Format("  br label %F{0}\n", ll)) |> ignore
                        else
                            pfat 1
                    | ':' ->
                        if pb.Count = 2 then
                            let mutable name = pb.Pop().Trim()
                            let mutable d = pb.Pop()
                            let mutable tpe = -1
                            let mutable loc = false

                            if name.StartsWith "private:" then
                                name <- name.[8..].Trim()
                                loc <- true

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
                            if loc then
                                let ptr = lc
                                lc <- lc + 1
                                match tpe with
                                | 0 | 1 ->
                                    cb.Append(String.Format("  %{0} = alloca i8\n", ptr)) |> ignore
                                    cb.Append(String.Format("  store i8 {0}, i8* %{1}\n", d, ptr)) |> ignore
                                | 2 | 3 ->
                                    cb.Append(String.Format("  %{0} = alloca i16\n", ptr)) |> ignore
                                    cb.Append(String.Format("  store i16 {0}, i16* %{1}\n", d, ptr)) |> ignore
                                | 4 | 5 ->
                                    cb.Append(String.Format("  %{0} = alloca i64\n", ptr)) |> ignore
                                    cb.Append(String.Format("  store i64 {0}, i64* %{1}\n", d, ptr)) |> ignore
                                | 6 | 7 ->
                                    cb.Append(String.Format("  %{0} = alloca i32\n", ptr)) |> ignore
                                    cb.Append(String.Format("  store i32 {0}, i32* %{1}\n", d, ptr)) |> ignore
                                | _ -> tfat "supported types"
                                lvn.Add(name)
                                lvt.Add(tpe)
                                lvp.Add(ptr)
                            else
                                for j = vn.Count - 1 downto 0 do
                                    if vn.[j] = name && vt.[j] = tpe then
                                        match tpe with
                                        | 0 | 1 ->
                                            if cb = main && not mla then
                                                main.Append "main:\n" |> ignore
                                                mla <- true
                                            cb.Append(String.Format("  store i8 {0}, i8* @v{1}\n", d, j)) |> ignore
                                        | 2 | 3 ->
                                            if cb = main && not mla then
                                                main.Append "main:\n" |> ignore
                                                mla <- true
                                            cb.Append(String.Format("  store i16 {0}, i16* @v{1}\n", d, j)) |> ignore
                                        | 4 | 5 ->
                                            if cb = main && not mla then
                                                main.Append "main:\n" |> ignore
                                                mla <- true
                                            cb.Append(String.Format("  store i64 {0}, i64* @v{1}\n", d, j)) |> ignore
                                        | 6 | 7 ->
                                            if cb = main && not mla then
                                                main.Append "main:\n" |> ignore
                                                mla <- true
                                            cb.Append(String.Format("  store i32 {0}, i32* @v{1}\n", d, j)) |> ignore
                                        | _ -> tfat "supported types"
                                        found <- true

                                if not found then
                                    match tpe with
                                    | 0 | 1 -> dat.Append(String.Format("@v{0} = global i8 {1}\n", vc, d)) |> ignore
                                    | 2 | 3 -> dat.Append(String.Format("@v{0} = global i16 {1}\n", vc, d)) |> ignore
                                    | 4 | 5 -> dat.Append(String.Format("@v{0} = global i64 {1}\n", vc, d)) |> ignore
                                    | 6 | 7 -> dat.Append(String.Format("@v{0} = global i32 {1}\n", vc, d)) |> ignore
                                    | _ -> tfat "supported types"
                                    vn.Add name
                                    vt.Add tpe
                                    vc <- vc + 1
                        else
                            pfat 2
                    | 'f' ->
                        if pb.Count = 2 then
                            let mutable name = pb.Pop().Trim()
                            let mutable c = pb.Pop()
                            let n = fn.Count
                            let fb = StringBuilder()
                            let ob = cb
                            cb <- fb

                            lvn.Clear()
                            lvt.Clear()
                            lvp.Clear()

                            if name.StartsWith "private:" then
                                name <- name.[8..].Trim()
                                fpv.Add true
                            else
                                fpv.Add false

                            pn.Clear()
                            pt.Clear()
                            pid.Clear()
                            if rc then
                                fn.Add name
                            Run c
                            if not rc then
                                fn.Add name

                            let result = gbb.ToString().Trim()
                            if String.IsNullOrEmpty result then
                                ft.Add 0
                            elif result.StartsWith "8b:" then
                                fb.Append(String.Format("  ret i32 {0}\n", result.[3..])) |> ignore
                                ft.Add 1
                            elif result.StartsWith "16b:" then
                                fb.Append(String.Format("  ret i32 {0}\n", result.[4..])) |> ignore
                                ft.Add 1
                            elif result.StartsWith "64b:" then
                                fb.Append(String.Format("  ret i32 {0}\n", result.[4..])) |> ignore
                                ft.Add 1
                            elif result.StartsWith "n:" then
                                fb.Append(String.Format("  ret i32 {0}\n", result.[2..])) |> ignore
                                ft.Add 1
                            elif result.StartsWith "x:" then
                                fb.Append(String.Format("  ret i32 {0}\n", Convert.ToInt64(result.[2..], 16).ToString())) |> ignore
                                ft.Add 1
                            else
                                tfat "supported types"

                            cb <- ob
                            if fpv.[n] then
                                txt.Append(String.Format("define private i32 @f{0}() {{\n", n)) |> ignore
                            else
                                txt.Append(String.Format("define i32 @f{0}() {{\n", n)) |> ignore
                            txt.Append(fb.ToString()) |> ignore
                            if not (fb.ToString().Contains "ret") then
                                txt.Append "  ret i32 0\n" |> ignore
                            txt.Append "}\n" |> ignore
                            pn.Clear()
                            pt.Clear()
                            pid.Clear()
                        elif pb.Count = 3 then
                            let mutable p = pb.Pop()
                            let mutable name = pb.Pop().Trim()
                            let mutable c = pb.Pop()
                            
                            let pstr = p.Replace("[", "").Replace("]", "").Trim()
                            
                            let n = fn.Count
                            let fb = StringBuilder()
                            let ob = cb
                            cb <- fb

                            lvn.Clear()
                            lvt.Clear()
                            lvp.Clear()

                            if name.StartsWith "private:" then
                                name <- name.[8..].Trim()
                                fpv.Add true
                            else
                                fpv.Add false

                            let mutable pt2 = List<string>()
                            pn.Clear()
                            pt.Clear()
                            pid.Clear()
                            if pstr <> "" && pstr <> "[]" then
                                let parts = pstr.Split(',')
                                let mutable i = 0
                                for part in parts do
                                    let trimmed = part.Trim()
                                    if trimmed <> "" then
                                        let coloni = trimmed.IndexOf(':')
                                        if coloni > 0 then
                                            let pname = trimmed.Substring(0, coloni)
                                            let ptype = trimmed.Substring(coloni + 1)
                                            let tm =
                                                match ptype with
                                                | "i8" | "char" | "byte" | "c" -> "i8"
                                                | "i16" | "short" -> "i16"
                                                | "i32" | "int" -> "i32"
                                                | "i64" | "long" -> "i64"
                                                | "ptr" | "i8*" -> "i8*"
                                                | "i16*" -> "i16*"
                                                | "i32*" -> "i32*"
                                                | "i64*" -> "i64*"
                                                | _ -> 
                                                    if ptype.StartsWith("8b") then "i8"
                                                    elif ptype.StartsWith("16b") then "i16"
                                                    elif ptype.StartsWith("32b") then "i32"
                                                    elif ptype.StartsWith("64b") then "i64"
                                                    else ""
                                            pn.Add(pname)
                                            pt.Add(tm)
                                            pt2.Add(tm)
                                            pid.Add(i)
                                            i <- i + 1
                                        else
                                            tfat "supported types"

                            fpb.Add(pt2.ToArray())
                            fn.Add name
                            Run c

                            let result = gbb.ToString().Trim()
                            if String.IsNullOrEmpty result then
                                ft.Add 0
                            elif result.StartsWith "8b:" then
                                fb.Append(String.Format("  ret i32 {0}\n", result.[3..])) |> ignore
                                ft.Add 1
                            elif result.StartsWith "16b:" then
                                fb.Append(String.Format("  ret i32 {0}\n", result.[4..])) |> ignore
                                ft.Add 1
                            elif result.StartsWith "64b:" then
                                fb.Append(String.Format("  ret i32 {0}\n", result.[4..])) |> ignore
                                ft.Add 1
                            elif result.StartsWith "n:" then
                                fb.Append(String.Format("  ret i32 {0}\n", result.[2..])) |> ignore
                                ft.Add 1
                            elif result.StartsWith "x:" then
                                fb.Append(String.Format("  ret i32 {0}\n", Convert.ToInt64(result.[2..], 16).ToString())) |> ignore
                                ft.Add 1
                            else
                                tfat "supported types"

                            cb <- ob
                            let paramStr = String.Join(", ", pt2 |> Seq.mapi (fun i t -> String.Format("i32 %p{0}", i)))
                            if fpv.[n] then
                                txt.Append(String.Format("define private i32 @f{0}({1}) {{\n", n, paramStr)) |> ignore
                            else
                                txt.Append(String.Format("define i32 @f{0}({1}) {{\n", n, paramStr)) |> ignore
                            txt.Append(fb.ToString()) |> ignore
                            if not (fb.ToString().Contains "ret") then
                                txt.Append "  ret i32 0\n" |> ignore
                            txt.Append "}\n" |> ignore
                            pn.Clear()
                            pt.Clear()
                            pid.Clear()
                        else
                            pfat 2
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
                    | '!' ->
                        if pb.Count = 2 then
                            match pb.Pop().Trim() with
                            | "bare" ->
                                bare <- true
                                Run(pb.Pop())
                                bare <- false
                            | "rec" ->
                                rc <- true
                                Run(pb.Pop())
                                rc <- false
                            | _ -> fatal "Invalid Option."
                        else pfat 2
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
                                        let mutable ps = List<string>()
                                        for i = 0 to count - 1 do
                                            let fpm = fpms.[i]
                                            let pm = pb.Pop()
                                            if pm.StartsWith "c:" then
                                                let c = pm.[2..]
                                                if c.Length = 1 then
                                                    cb.Append(String.Format("  %p{0} = add i32 {1}, 0\n", i, int c.[0])) |> ignore
                                                    ps.Add(String.Format("i32 %p{0}", i))
                                                else
                                                    fatal "Character size unmatched. Should be 1 character."
                                            elif pm.StartsWith "8b:" then
                                                cb.Append(String.Format("  %p{0} = add i32 {1}, 0\n", i, pm.[3..])) |> ignore
                                                ps.Add(String.Format("i32 %p{0}", i))
                                            elif pm.StartsWith "8bx:" then
                                                cb.Append(String.Format("  %p{0} = add i32 {1}, 0\n", i, Convert.ToInt64(pm.[4..], 16).ToString())) |> ignore
                                                ps.Add(String.Format("i32 %p{0}", i))
                                            elif pm.StartsWith "16b:" then
                                                cb.Append(String.Format("  %p{0} = add i32 {1}, 0\n", i, pm.[4..])) |> ignore
                                                ps.Add(String.Format("i32 %p{0}", i))
                                            elif pm.StartsWith "16bx:" then
                                                cb.Append(String.Format("  %p{0} = add i32 {1}, 0\n", i, Convert.ToInt64(pm.[5..], 16).ToString())) |> ignore
                                                ps.Add(String.Format("i32 %p{0}", i))
                                            elif pm.StartsWith "32b:" then
                                                cb.Append(String.Format("  %p{0} = add i32 {1}, 0\n", i, pm.[4..])) |> ignore
                                                ps.Add(String.Format("i32 %p{0}", i))
                                            elif pm.StartsWith "32bx:" then
                                                cb.Append(String.Format("  %p{0} = add i32 {1}, 0\n", i, Convert.ToInt64(pm.[5..], 16).ToString())) |> ignore
                                                ps.Add(String.Format("i32 %p{0}", i))
                                            elif pm.StartsWith "64b:" then
                                                cb.Append(String.Format("  %p{0} = add i32 {1}, 0\n", i, pm.[4..])) |> ignore
                                                ps.Add(String.Format("i32 %p{0}", i))
                                            elif pm.StartsWith "64bx:" then
                                                cb.Append(String.Format("  %p{0} = add i32 {1}, 0\n", i, Convert.ToInt64(pm.[5..], 16).ToString())) |> ignore
                                                ps.Add(String.Format("i32 %p{0}", i))
                                            elif pm.StartsWith "x:" then
                                                cb.Append(String.Format("  %p{0} = add i32 {1}, 0\n", i, Convert.ToInt64(pm.[2..], 16).ToString())) |> ignore
                                                ps.Add(String.Format("i32 %p{0}", i))
                                            elif pm.StartsWith "n:" then
                                                cb.Append(String.Format("  %p{0} = add i32 {1}, 0\n", i, pm.[2..])) |> ignore
                                                ps.Add(String.Format("i32 %p{0}", i))
                                            else
                                                tfat "supported types"
                                        if fpv.[j] then
                                            cb.Append(String.Format("  call i32 @f{0}({1})\n", j, String.Join(", ", ps))) |> ignore
                                        else
                                            cb.Append(String.Format("  call i32 @f{0}({1})\n", j, String.Join(", ", ps))) |> ignore
                                    else
                                        pfat fpms.Length
                                else
                                    if cb = main && not mla then
                                        main.Append "main:\n" |> ignore
                                        mla <- true
                                    if fpv.[j] then
                                        cb.Append(String.Format("  call i32 @f{0}()\n", j)) |> ignore
                                    else
                                        cb.Append(String.Format("  call i32 @f{0}()\n", j)) |> ignore
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
                cb.Append(String.Format("  ret i32 {0}\n", Convert.ToInt64(data.[4..], 16).ToString())) |> ignore
            elif data.StartsWith "8b:" then
                if cb = main && not mla then
                    main.Append "main:\n" |> ignore
                    mla <- true
                cb.Append(String.Format("  ret i32 {0}\n", data.[3..])) |> ignore
            elif data.StartsWith "16bx:" then
                if cb = main && not mla then
                    main.Append "main:\n" |> ignore
                    mla <- true
                cb.Append(String.Format("  ret i32 {0}\n", Convert.ToInt64(data.[5..], 16).ToString())) |> ignore
            elif data.StartsWith "16b:" then
                if cb = main && not mla then
                    main.Append "main:\n" |> ignore
                    mla <- true
                cb.Append(String.Format("  ret i32 {0}\n", data.[4..])) |> ignore
            elif data.StartsWith "64bx:" then
                if cb = main && not mla then
                    main.Append "main:\n" |> ignore
                    mla <- true
                cb.Append(String.Format("  ret i32 {0}\n", Convert.ToInt64(data.[5..], 16).ToString())) |> ignore
            elif data.StartsWith "64b:" then
                if cb = main && not mla then
                    main.Append "main:\n" |> ignore
                    mla <- true
                cb.Append(String.Format("  ret i32 {0}\n", data.[4..])) |> ignore
            elif data.StartsWith "n:" then
                if cb = main && not mla then
                    main.Append "main:\n" |> ignore
                    mla <- true
                cb.Append(String.Format("  ret i32 {0}\n", data.[2..])) |> ignore
            elif data.StartsWith "x:" then
                if cb = main && not mla then
                    main.Append "main:\n" |> ignore
                    mla <- true
                cb.Append(String.Format("  ret i32 {0}\n", Convert.ToInt64(data.[2..], 16).ToString())) |> ignore

        cb <- sb

    static member Compile(code: string) =
        let out = StringBuilder()
        mla <- true
        Run code

        if ex.Count = 0 then
            if main.ToString() <> "define i32 @main() {\nmain:\n" then
                out.Append main |> ignore
                if not (main.ToString().Contains "ret") then
                    out.Append "  ret i32 0\n" |> ignore
                out.Append "}\n" |> ignore
            if txt.Length > 0 then
                out.Append txt |> ignore
            if dat.ToString() <> ";gb\n" then
                out.Append dat |> ignore
        else
            failwith (String.Format("Build Unsuccessful.\n\t{0}", String.Join("\n\t", ex)))

        out.ToString()