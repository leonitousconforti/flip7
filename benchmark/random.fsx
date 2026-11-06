// ------------------------------------------------------
// https://github.com/microsoft/referencesource/blob/ec9fa9ae770d522a5b5f0607898044b7478574a3/mscorlib/system/random.cs
// ------------------------------------------------------
type private MyRandom(seed: int) =
    let mutable inext = 0
    let mutable inextp = 21
    let seedArray = Array.zeroCreate 56

    do
        let subtraction =
            if seed = System.Int32.MinValue then
                System.Int32.MaxValue
            else
                abs seed

        let mutable mj = MyRandom.MSEED - subtraction
        seedArray.[55] <- mj

        let mutable mk = 1
        for i in 1..54 do
            let ii = (21 * i) % 55
            seedArray.[ii] <- mk
            mk <- mj - mk
            if mk < 0 then
                mk <- mk + MyRandom.P
            mj <- seedArray.[ii]

        for _k in 1..4 do
            for i in 1..55 do
                seedArray.[i] <- seedArray.[i] - seedArray.[1 + (i + 30) % 55]
                if seedArray.[i] < 0 then
                    seedArray.[i] <- seedArray.[i] + MyRandom.P

    static member private P = System.Int32.MaxValue
    static member private MSEED = 161803398

    member _self.Next() =
        let locINext = inext % 55 + 1
        let locINextp = inextp % 55 + 1

        let mutable retVal = seedArray.[locINext] - seedArray.[locINextp]
        if retVal = MyRandom.P then
            retVal <- retVal - 1
        if retVal < 0 then
            retVal <- retVal + MyRandom.P

        seedArray.[locINext] <- retVal
        inext <- locINext
        inextp <- locINextp
        retVal

// ------------------------------------------------------
// Test the MyRandom implementation against System.Random
// ------------------------------------------------------

do
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    let picker = System.Random(1)

    let seeds = [
        System.Int32.MinValue
        yield! [ -1000000 .. 1000000 ]
        for _ in 1..1000000 do
            picker.Next(System.Int32.MinValue, System.Int32.MaxValue)
        System.Int32.MaxValue
    ]

    stopwatch.Restart()
    for seed in seeds do
        let random = System.Random(seed)
        let myRandom = MyRandom(seed)

        for i in 1..55 do
            let myValue = myRandom.Next()
            let value = random.Next()
            if myValue <> value then
                failwithf "Mismatch on seed %d @ iteration %d: %d vs %d" seed i myValue value

    stopwatch.Stop()
    printfn "All tests passed in %f seconds" stopwatch.Elapsed.TotalSeconds
