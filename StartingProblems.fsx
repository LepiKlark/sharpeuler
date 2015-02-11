module Utility =
    let primeFactors n =
        Seq.unfold (
            fun (lp, rem) ->
                if rem = 1L then None
                else
                    let upperBound = rem |> float |> sqrt |> int64
                    let factor = seq { lp..upperBound } |> Seq.tryFind (fun div -> rem % div = 0L)
                    match factor with
                    | Some(factor) -> Some(factor,((factor, (rem/factor))))
                    | None -> Some(rem, (1L,1L) )) (2L,n) // this is a prime. Return it self and break.

    let isPrime (n : int64) = primeFactors n |> Seq.length = 1
    let rec gcd a b = if b = 0 then a else gcd b (a % b)
    let rec fact = function | 0L | 1L -> 1L | n -> n * fact(n-1L)
    let rec factbig (n : bigint) = 
        if n.IsOne || n.IsZero then
            bigint.One
        else
            n * factbig(n-bigint.One)
    let combination n m = fact(n) / (fact(n - m) * fact(m))
    let rec comb n l =
        match (n,l) with
        | (0,_) -> [[]]
        | (_,[]) -> []
        | (n,x::xs) ->
            let useX = List.map (fun l -> x::l) (comb (n-1) xs)
            let noX = comb n xs
            useX @ noX
    open System.Collections.Generic
    let memoize f = 
        let cache = System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural)
        fun x ->
            let ok, res = cache.TryGetValue(x)
            if ok then 
                res
            else 
                let res = f x
                cache.[x] <- res
                res
    let memo () = 
        let cache = System.Collections.Generic.Dictionary<'b,'d>(HashIdentity.Structural)
        fun (mapper : 'a -> 'b) (f : 'a -> 'd) (x : 'a) ->
            let key = mapper x
            if cache.ContainsKey key then
                cache.[key]
            else
                let ret = f x
                cache.[key] <- ret
                ret

    let divisors n =
        let upper = n |> float |> sqrt |> int
        if upper * upper = n then
            seq { 
                yield 1; 
                if upper <> 1 then yield upper; 
                for x in 2..(upper-1) do if n % x = 0 then yield x; yield (n/x) }
        else
            seq { yield 1; for x in 2..upper do if n % x = 0 then yield x; yield (n/x) }
        
module ``problem 1`` =
    [1..999] |> List.filter (fun n -> n % 3 = 0 || n % 5 = 0) |> List.sum

module ``problem 2`` =
    let fibonacci = Seq.unfold (fun (a,b) -> Some((a + b, (b, a + b)))) (0,1)
    let isEven n = n % 2 = 0
    fibonacci |> Seq.takeWhile (fun n -> n < 4000000) |> Seq.where isEven |> Seq.sum

module ``problem 3`` =    
    open Utility
    primeFactors 600851475143L |> Seq.toArray

module ``problem 4`` =
    let isPalindrome n =
        let str = string n |> Seq.toArray
        str = Array.rev str

    [ for x in 100..999 do for y in 100..999 do if isPalindrome (x * y) then yield (x * y)]
    |> List.max

module ``problem 5`` =
    let rec gcd a b = if b = 0 then a else gcd b (a % b)
    [1..20] |> List.fold (fun total curr -> total * (curr / (gcd total curr))) 1

module ``problem 6`` =
    let rangeSum range = range * (range + 1) / 2 
    let rangeSquareSum range = range * (range + 1) * (2 * range + 1) / 6
    pown (rangeSum 100) 2 - rangeSquareSum 100 |> abs

module ``problem 7`` =
    open Utility
    open System
    seq { 2L..(Int64.MaxValue) } |> Seq.where isPrime |> Seq.nth 10000

module ``problem 8`` =
    let num = "73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450".Replace("\n", "") |> Seq.map (fun l -> int64 l - int64 '0')
    num |> Seq.windowed 13 |> Seq.map (Array.reduce (*)) |> Seq.max

module ``problem 9`` =
    [ for x in 1..500 do 
        for y in 1..500 do 
            yield sqrt ((float x) ** 2. + (float y) ** 2.), float x, float y ]
    |> List.find (fun (a,b,c) -> a + b + c = 1000.) |> fun (a,b,c) -> a * b * c
    
module ``problem 10`` =
    open Utility
    [2L..2000000L] |> List.filter isPrime |> List.sum

module ``problem 11`` =
    let grid =
     """08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08 
49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00 
81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65 
52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91 
22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80 
24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50 
32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70 
67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21 
24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72 
21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95 
78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92 
16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57 
86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58 
19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40 
04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66 
88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69 
04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36 
20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16 
20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54 
01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"""
    let parsed = grid.Split(' ') |> Array.map System.Int32.Parse
    let matrix = Array2D.init 20 20 (fun x y -> parsed.[x * 20 + y])

    let indexes = seq {
        for x in 0..19 do
            for y in 0..19 do
                yield Seq.init 4 (fun i -> x + i,y)
                yield Seq.init 4 (fun i -> x,y+i)
                yield Seq.init 4 (fun i -> x+i,y+i)
                yield Seq.init 4 (fun i -> x-i,y+i) }

    indexes
    |> Seq.map (fun comb -> 
        try comb |> Seq.map (fun (x,y) -> matrix.[x, y]) |> Seq.reduce (*) |> Some
        with | ex -> None) |> Seq.choose id |> Seq.max

module ``problem 12`` =
    open Utility

    let countDivisors n =
        primeFactors n |> Seq.countBy id |> Seq.map snd
        |> Seq.map ((+) 1) |> Seq.reduce (*)

    Seq.initInfinite (fun i -> i * (i + 1) / 2)
    |> Seq.skip 2
    |> Seq.find (fun x -> countDivisors (int64 x) > 500)

module ``problem 13`` =
    let filePath = @"C:\Users\Aleksandar\Desktop\num.txt"
    System.IO.File.ReadAllLines filePath |> Seq.map (bigint.Parse) |> Seq.reduce (+)
    |> string |> Seq.take 10 |> Seq.map string |> Seq.reduce (+)

module ``problem 14`` =
    open Utility

    let rec collatz = function 
        | 0L | 1L -> 1L
        | n when n % 2L = 0L -> 1L + collatz (n / 2L) 
        | n when n % 2L <> 0L -> 1L + collatz (3L * n + 1L)

    let mcollatz = memoize collatz
    [1L..1000000L] |> List.maxBy mcollatz

module ``problem 15`` =
    open Utility
    let walk n = factbig (bigint (n * 2)) / bigint.Pow((factbig (bigint n)),2)
    walk 20

module ``problem 16`` =
    bigint.Pow(bigint 2, 1000) |> string |> Seq.map (string >> System.Int32.Parse) |> Seq.sum

module ``problem 17`` =
    let digitToWord n =
        [| "zero"; "one"; "two"; "three"; "four"; "five"; "six"; "seven";
           "eight"; "nine"; "ten"; "eleven"; "twelve"; "thirteen"; "fourteen";
           "fifteen"; "sixteen"; "seventeen"; "eighteen"; "nineteen"; |].[n]
        
    let decadeToWord n =
        [| ""; ""; "twenty"; "thirty"; "forty"; "fifty"; "sixty"; "seventy"; "eighty"; "ninety" |].[n]
    let rec toWords n =
        match n with
        | n when n >= 100 && n % 100 <> 0 -> 
            digitToWord (n/100) + "hundredand" + (toWords (n % 100))
        | n when n >= 100 -> digitToWord (n%100) + "hundred"
        | n when n >= 20 && n % 10 <> 0 ->
            decadeToWord (n/10) +  toWords(n % 10)
        | n when n >= 20 -> decadeToWord (n/10)
        | n -> digitToWord n

    "onethousand".Length + ([1..999] |> Seq.sumBy (toWords >> Seq.length))

module ``problem 18`` =
    open Utility
    let triangles = 
        """75
        95 64
        17 47 82
        18 35 87 10
        20 04 82 47 65
        19 01 23 75 03 34
        88 02 77 73 07 63 67
        99 65 04 28 06 16 70 92
        41 41 26 56 83 40 80 70 33
        41 48 72 33 47 32 37 16 94 29
        53 71 44 65 25 43 91 52 97 51 14
        70 11 33 28 77 73 17 78 39 68 17 57
        91 71 52 38 17 14 91 43 58 50 27 29 48
        63 66 04 68 89 53 67 30 73 16 69 87 40 31
        04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"""
    let triangle = triangles.Split('\n') |> Array.map (fun l -> l.Trim().Split(' ') |> Array.map System.Int32.Parse)

    let mtraverse (triangle : int [] []) =
        let memo = (memo ()) id
        let rec traverse pos =
            match pos with
            | (x, _) when x = (triangle.Length - 1) -> triangle.[fst pos].[snd pos]
            | (x, y) ->
                let left = memo traverse (x + 1, y)
                let right = memo traverse (x + 1, y + 1)
                triangle.[x].[y] + (max left right)
        traverse (0,0)
    
    mtraverse triangle

module ``problem 67`` =
    open ``problem 18``
    let path = "C:\Users\Aleksandar\Desktop\p067_triangle.txt"
    let triangle = System.IO.File.ReadAllLines(path) |> Array.map (fun l -> l.Trim().Split(' ') |> Array.map System.Int32.Parse)
    mtraverse triangle

module ``problem 19`` =
    open System
    let start = DateTime(1901, 1, 1)
    let ``end`` = DateTime(2000, 12, 31)
    let cal = Seq.unfold (fun date -> if date > ``end`` then None else Some(date, (date.AddDays(1.)))) start
    cal
    |> Seq.where (fun d -> d.Day = 1 && d.DayOfWeek = DayOfWeek.Sunday)
    |> Seq.length

module ``problem 20`` =
    open Utility
    factbig (bigint 100) |> string |> Seq.map string |> Seq.map System.Int32.Parse |> Seq.sum

module ``problem 21`` =
    open Utility
    let d x = divisors x |> Seq.sum
    let isAmicable x = d(d x) = x && d x <> x
    
    [1..9999] |> List.filter isAmicable |> List.sum

module ``problem 22`` =
    let fp = @"C:\Users\Aleksandar\Desktop\p022_names.txt"
    let input = 
        System.IO.File.ReadAllText(fp).Split(',') 
        |> Array.map (fun s -> s.Trim('"'))
        |> Array.sort
    let worth (s : string) = s |> Seq.map (fun c -> int c - int 'A' + 1) |> Seq.sum
    input |> Array.mapi (fun i s -> (i + 1) * worth s) |> Array.sum

module ``problem 23`` =
    open Utility
    let isAbundant x = (divisors x |> Seq.sum) > x
    let limit = 28123
    let abundant = [1..limit] |> List.filter isAbundant |> List.toArray
    let sums = [|for a in abundant do for b in abundant do yield a + b|]
    ([1..limit] |> Set.ofList) - (sums |> Set.ofArray)
    |> Set.toArray |> Array.sum