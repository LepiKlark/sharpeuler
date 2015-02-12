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
    let num = "...".Replace("\n", "") |> Seq.map (fun l -> int64 l - int64 '0')
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
     """..."""
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
        """..."""
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
    let abundant = [|1..limit|] |> Array.filter isAbundant
    let sums = seq { for a in abundant do for b in abundant do if a + b <= limit then yield a + b } |> Seq.distinct
    (limit * (limit + 1) / 2) - (sums|> Seq.sum)

module ``problem 25`` =
    let fibonacci = Seq.unfold (fun (a,b) -> Some((a + b, (b, a + b)))) (bigint 0, bigint 1)
    let cntdigits (n : bigint) = n |> string |> Seq.length
    (fibonacci |> Seq.findIndex (fun n -> cntdigits n = 1000)) + 2
    
module ``problem 26`` =
    let rec div nominator denominator rs =
        let r = nominator % denominator
        if r = 0 then 0
        elif List.exists ((=) r) rs then 1 + List.findIndex ((=) r) (rs)
        else div (r * 10) denominator (r::rs)
    [1..999] |> List.maxBy (fun n -> div 1 n [])

module ``problem 27`` =
    open Utility
    let exists n1 n2 p1 p2 = (p2 - n2 * n2 - p1 + n1 * n1) % (n2 - n1)
    let primes = [1L..100000L] |> List.filter isPrime
    primes |> Seq.length