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

    let isPrime n =
        match n with
        | _ when n > 3L && (n % 2L = 0L || n % 3L = 0L) -> false
        | _ ->
            let maxDiv = int64(System.Math.Sqrt(float n)) + 1L
            let rec f d i = 
                if d > maxDiv then 
                    true
                else
                    if n % d = 0L then 
                        false
                    else
                        f (d + i) (6L - i)     
            f 5L 2L

    let rec gcd a b = if b = 0 then a else gcd b (a % b)
    let rec fact = function | 0L | 1L -> 1L | n -> n * fact(n-1L)
    let rec factbig (n : bigint) = 
        if n.IsOne || n.IsZero then
            bigint.One
        else
            n * factbig(n-bigint.One)
    let rec factgen n = 
        if n = LanguagePrimitives.GenericOne || n = LanguagePrimitives.GenericZero then LanguagePrimitives.GenericOne
        else n * factgen (n - LanguagePrimitives.GenericOne)
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
        
    let rec distribute e = function
    | [] -> [[e]]
    | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

    let rec permute = function
    | [] -> [[]]
    | e::xs -> List.collect (distribute e) (permute xs)

    let digits x = x |> string |> Seq.map (fun c -> int c - int '0')

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
    let grid = """..."""
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

module ``problem 24`` =
    open Utility

    let remove pos lst =
        let rec removei curr = function
        | [] -> []
        | e::tail when curr = pos -> removei (curr + 1) tail
        | e::tail -> e::removei (curr + 1) tail
        removei 0 lst

    let rec get_perm n (collection : list<'a>) =
        let len = collection |> Seq.length |> int64
        let f = fact(len - 1L)
        if n <> 0L then
            let pos = n / f |> int
            let elem = collection.[pos]
            let rest = remove pos collection 
            elem::(get_perm (n - (f * int64 pos)) rest)
        else collection

    get_perm 999999L [0..9] |> List.iter (printf  "%A")

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

    let f a b n = 
        let r = n * n + a * n + b 
        r > 0L && isPrime r
    let bcands = [1L..1000L] |> List.filter isPrime
    [ for a in [-1000L..1000L] do
        for b in bcands do
            yield a, b, (seq { 0L.. 100L } |> Seq.takeWhile (fun n -> f a b n) |> Seq.length) ] 
    |> List.maxBy (fun (_, _, v) -> v) |> fun (a, b, _) -> a * b

module ``problem 28`` =
    let sumdiag n = -3 + (Seq.init 4 (fun j -> Seq.init n (fun i -> pown (1 + i * 2) 2 - i * 2 * j)) |> Seq.collect id |> Seq.sum)
    sumdiag 501

module ``problem 29`` =
    [for a in 2..100 do for b in 2..100 do yield bigint.Pow ((bigint a),b) ] |> Seq.distinct |> Seq.length

module ``problem 30`` =
    let f n = n |> string |> Seq.map (fun c -> int c - int '0') |> Seq.map (fun i -> pown i 5) |> Seq.sum
    ([1..500000] |> List.filter (fun n -> f n = n) |> List.sum) - 1

module ``problem 31`` =
    open Utility
    let rec split lst = [
        match lst with
        | [] -> ()
        | h::t as xs -> yield h,xs; yield! split t ]
    
    let memo = (memo ()) id
    let rec count (sum, coins) =
        if sum = 200 then 1
        elif sum > 200 then 0
        else split coins |> List.map (fun (c, rest) -> memo count (sum + c, rest)) |> List.sum
    count (0,[1; 2; 5; 10; 20; 50; 100; 200])

module ``problem 32`` =
    let check x y =
        let res = string x + string y + string (x * y)
        Seq.length res = 9 && Seq.distinct res |> Seq.length = 9 && (Seq.min res) > '0'

    [ for x in 1..3000 do
        for y in x..3000 do
            if check x y then yield x * y ] |> Seq.distinct |> Seq.sum

module ``problem 33`` =
    let check a b =
        let an, ad = a / 10, a % 10
        let bn, bd = b / 10, b % 10
        let f a b = float a / float b
        if an = 0 || ad = 0 || bn = 0 || bd = 0 then false
        elif an=bn then f ad bd=f a b
        elif an=bd then f ad bn=f a b
        elif ad=bn then f an bd=f a b
        elif ad=bd then f an bn=f a b
        else false

    [ for a in 10..99 do for b in (a+1)..99 do if check a b then yield a, b ]

module ``problem 34`` =
    open Utility

    let sumdigitfact n = n |> string |> Seq.map (fun c -> int64 c - int64 '0') |> Seq.map fact |> Seq.sum
    [3L..50000L] |> List.filter (fun c -> c = sumdigitfact c) |> List.sum

module ``problem 35`` =
    open Utility
    let primes = [1L..1000000L] |> List.filter isPrime |> List.map int
    let primeSet = primes |> Set.ofList

    let rec listRotations list =
        seq {
            match list with
            | [] -> ()
            | h::t -> yield list; yield! listRotations (t@[h])
        } // List.permute is better choice here.

    let num_len n = (int << log10 << float <| n) + 1
    let numRotations n =
        let t = n |> digits |> Seq.toList |> listRotations |> Seq.take (num_len n)
        t |> Seq.map (fun cs -> cs |> Seq.map string |> Seq.reduce (+) |> System.Int32.Parse)
    
    let ForAll f s = Seq.exists (f >> not) s |> not

    [ for p in primes do
        if numRotations p |> ForAll (fun e -> Set.contains e primeSet) then yield p ] |> List.length

module ``problem 36`` =
    let rec toBinary = function
    | 0 -> []
    | x -> (x &&& 1) :: toBinary (x >>> 1)

    let rec toDec = function
    | 0 -> []
    | x -> (x % 10) :: toDec (x / 10)

    let check x = toDec x = List.rev (toDec x) && toBinary x = List.rev (toBinary x)     
    [1..999999] |> List.filter check |> List.sum

module ``problem 37`` =
    open Utility

    let truncL (x : int) = x % (pown 10 (int(log10 (float x))))
    let truncLSeq = Seq.unfold(fun s -> if s <> 0 then Some (s, truncL s) else None)
    let isTruncLPrime (n : int64) = n |> int |> truncLSeq |> Seq.map int64 |> Seq.forall isPrime

    let rec buildPrimes (curr : int64) =
        let cands = [1L..2L..9L] |> List.map (fun i -> curr * 10L + i) |> List.filter isPrime
        let truncPrimes = cands |> List.filter isTruncLPrime
        match cands with
        | [] -> []
        | lst -> truncPrimes @ (cands |> List.map buildPrimes |> List.collect id)

    [2L;3L;5L;7L] |> List.map buildPrimes |> List.collect id |> List.sum

module ``problem 38`` =
    let digitCount (n : int64) = int (log10 (float n)) + 1
    [ for x in 1L..99999L do
         for y in 1L..((10L / int64 (digitCount x)) + 1L) do
             yield [1L..y] |> List.map ((*) x) |> List.map string |> List.reduce (+) ]
    |> List.filter (fun s -> s.Length = 9)
    |> List.filter (fun s -> s.Contains("0") |> not)
    |> List.filter (fun s -> s |> Seq.distinct |> Seq.length = 9)
    |> List.max

module ``problem 39`` =
    let c x y = (pown x 2 + pown y 2) |> float |> sqrt
    let cnt goal =
        [ for x in 1. .. (goal / 3.) do
            for y in x .. ((goal - x) / 2. + 1.) do
                if (x + y + sqrt(x ** 2. + y ** 2.)) = goal then yield 1
        ] |> List.length
    [1. .. 1000. ] |> List.mapi (fun i c -> float i + 1., cnt c)
    |> List.sortBy ((~-) << snd) |> List.head

module ``problem 40`` =
    let sb = System.Text.StringBuilder()
    seq { 1 .. 200000 } |> Seq.iter (sb.Append >> ignore)
    let str = sb.ToString()
    [1;10;100;1000;10000;100000;1000000] |> List.map (fun i -> int str.[i - 1] - int '0') |> List.reduce (*)

module ``problem 41`` =
    open Utility
    let lstToInt64 (xs : list<int64>) = xs |> List.fold (fun s c -> s * 10L + c) 0L
    
    let rec search upper =
        let r = [1L..upper] |> permute |> List.map lstToInt64 |> List.sortBy (~-) |> List.tryFind isPrime
        match r with
        | Some x -> x
        | None   -> search (upper - 1L)

    search 9L

module ``problem 42`` =
    let value (c : char) = int c - int 'A' + 1
    let path = @"C:\Users\Aleksandar\Desktop\p042_words.txt"
    let words = System.IO.File.ReadAllText path |> fun s -> s.Replace("\"", "").Replace("\n", "").Split(',')
    let isTriangle n = ((-1. + (sqrt (1. + 8. * n))) / 2.) % 1. = 0.
    words |> Seq.map (Seq.map value >> Seq.sum >> float) |> Seq.filter isTriangle |> Seq.length

module ``problem 43`` =
    let divs = [2;3;5;7;11;13;17]
    let split (s : Set<int>) = [ for x in s -> x, (Set.remove x s) ]

    let rec build curr divs numoptions : (list<list<int>>)=
        match curr with
        | [] | [_] | [_;_]  ->
            [ for x, rem in (split numoptions) ->
                build (x::curr) divs rem ] |> List.collect id
        | a::b::lst -> 
            match divs with
            | [] -> [curr |> List.rev]
            | div::divs -> 
                [ for x, rem in (split numoptions |> List.filter (fun (x, _) -> (a * 10 + b * 100 + x) % div = 0)) ->
                    build (x::curr) divs rem ] |> List.collect id
    build [] divs ([0..9] |> Set.ofList)
    |> List.map (List.map string) |> List.map (List.reduce (+))
    |> List.map (System.Int64.Parse) |> List.sum

module ``problem 44`` =
    let s = Seq.initInfinite (fun i -> i * (3 * i - 1) / 2) |> Seq.skip 1
    let isPand x = (0.5 + sqrt (0.25 + 6. * (float x))) / 3. % 1. = 0.

    let search = s |> Seq.take 3000 |> Seq.toArray

    [ for x in search do
        for y in search do
            if isPand (x + y) && isPand (x - y) then yield x,y ]
       
module ``problem 45`` =
    let isPand x = (0.5 + sqrt (0.25 + 6. * (float x))) / 3. % 1. = 0.
    let isTriangle x = (-0.5 + sqrt(0.25 + 2. * (float x))) % 1. = 0.
    Seq.initInfinite (fun i -> i * (2 * i - 1)) |> Seq.skip 1
    |> Seq.where (fun i -> isTriangle i && isPand i)
    |> Seq.take 3

module ``problem 46`` =
    open Utility
    let primes = [1L..1000000L] |> List.filter isPrime
    let isGold x =
        primes |> Seq.takeWhile (fun p -> p < x)
        |> Seq.tryFind (fun p -> sqrt (float (x - p) / 2.) % 1. = 0.) |> Option.isSome
    [1L..1000000L] |> List.filter (isPrime >> not) |> List.filter (fun n -> n % 2L = 1L)
    |> Seq.tryFind (isGold >> not)

module ``problem 47`` =
    open Utility
    Seq.initInfinite (fun i -> int64 i + 1L) 
    |> Seq.windowed 4 
    |> Seq.find (fun arr -> arr |> Array.forall (primeFactors >> Seq.distinct >> Seq.length >> ((=) 4)))

module ``problem 48`` =
    [1..1000] |> List.map (fun i -> bigint.Pow ((bigint i), i)) |> List.sum
    |> string |> Seq.toList |> List.rev |> Seq.take 10
    |> Seq.toList |> List.rev |> List.map string |> Seq.reduce (+)

module ``problem 49`` =
    open Utility
    let isPermutation a b =
        (digits a |> Seq.sort |> Seq.toList) = (digits b |> Seq.sort |> Seq.toList)
    let primes = [1000L..9999L] |> List.filter isPrime |> List.map int

    [for x in primes do
        for y in primes do
            for z in primes do
                if (x < y && y < z) then
                    if (y - x = z - y) then
                        if isPermutation x y && isPermutation y z then
                            yield x, y, z]

module ``problem 50`` =
    open Utility

    let primes = [1L..100000L] |> List.filter isPrime
    let chain l = primes |> Seq.windowed l |> Seq.map (Array.sum) |> Seq.tryFind isPrime

    [|1..1000|] |> Array.Parallel.map (fun l -> chain l)
    |> Array.choose id |> Array.filter (fun c -> c < 1000000L) |> Seq.last

module ``problem 51`` =
    open Utility

    let primes = [99999L..999999L] |> List.filter isPrime |> List.map int |> List.map (digits >> Seq.toArray)

    [for perm in (comb 3 [0..5]) do
        let max = 
            [for digitArray in primes do
              if perm |> List.map (fun i -> digitArray.[i]) |> Seq.distinct |> Seq.length = 1 then // all fixed digits are same
                  yield digitArray |> Array.mapi (fun i c -> i,c) |> Array.filter (fun (p,c) -> List.exists ((=) p) perm |> not) |> Array.map snd]
            |> Seq.countBy id |> Seq.maxBy snd
        yield (perm,max)] |> List.maxBy (fun (_, (_, c)) -> c)

    //val it : int list * (int [] * int) = ([0; 2; 4], ([|2; 3; 3|], 8)) -> 

module ``problem 52`` =
    open Utility
    
    let sortedDigits x = x |> digits |> Seq.sort |> Seq.toArray

    [123456..(999999/6)] |> List.tryFind (
        fun x -> 
            let x1 = sortedDigits x
            let x2 = sortedDigits (2*x)
            let x3 = sortedDigits (3*x)
            let x4 = sortedDigits (4*x)
            let x5 = sortedDigits (5*x)
            let x6 = sortedDigits (6*x)

            x1 = x2 && x3 = x4 && x5 = x6 && x2 = x3 && x4 = x5
        )

module ``problem 53`` =
    open Utility
    let comb n r = (factbig n) / ((factbig r) * (factbig (n-r)))
    [for n in 1..100 do for r in 1..n -> comb (bigint n) (bigint r)] 
    |> List.filter (fun n -> n > bigint 1000000) |> Seq.length

module ``problem 54`` =
    type Suit = Club | Diamond | Spade | Heart
    type Card = int * Suit
    type Hand = Card list
    type HandScoreWithSortedCards = int * list<int> * list<int>

    exception InvalidCardParsing

    let text = System.IO.File.ReadAllLines @"C:\Users\atomic\Desktop\p054_poker.txt"
    let parseLine (line : string) : Hand * Hand =
        let cards = line.Split(' ') |> Array.map (fun s -> 
                        let suit = match s.[1] with
                                   | 'C' -> Club | 'S' -> Spade | 'D' -> Diamond | 'H' -> Heart
                                   | _ -> raise InvalidCardParsing                    
                        let rank = match s.[0] with
                                   | '2' -> 2 | '3' -> 3 | '4' -> 4 | '5' -> 5 | '6' -> 6
                                   | '7' -> 7 | '8' -> 8 | '9' -> 9 | 'T' -> 10 | 'J' -> 11 | 'Q' -> 12
                                   | 'K' -> 13 | 'A' -> 14 | _ -> raise InvalidCardParsing
                        rank, suit)
        (cards |> Seq.take 5 |> Seq.toList), (cards |> Seq.skip 5 |> Seq.toList)
    
    let rec isConsecutive = function 
    | a::b::lst when a <> b - 1 -> false
    | a::b::lst when a = b - 1 -> true
    | [_] | []  -> true

    let parseHand (h : Hand) : HandScoreWithSortedCards =
        let sorted = h |> List.sortBy (fst >> (~-)) |> List.map fst
        let areSuiteSame = h |> List.map snd |> Seq.distinct |> Seq.length = 1
        let areRanksConsecutive = isConsecutive << List.rev <| sorted
        let groups = h |> Seq.countBy fst |> Seq.sortBy (snd >> (~-)) |> Seq.toList
        let smallest = sorted |> Seq.last
        (match groups with
        | _ when areSuiteSame && areRanksConsecutive && smallest = 10 -> 8,[]
        | _ when areSuiteSame && areRanksConsecutive -> 7,[]
        | (x, 4)::(y,1)::[] -> 6,[x]
        | (x, 3)::(y,2)::[] -> 5,[x;y]
        | _ when areSuiteSame -> 4,[]
        | _ when areRanksConsecutive -> 3,[]
        | (x,3)::_ -> 2,[x]
        | (x,2)::(y,2)::_::[] -> 1,([x;y] |> List.sortBy (~-))
        | (x,2)::_ -> 0,[x]
        | _ -> -1,[]) |> fun (a,b) -> a,b,sorted

    text |> Array.map parseLine |> Array.filter (fun (h1, h2) -> parseHand h1 > parseHand h2) |> Array.length

module ``problem 55`` =
    let rev n = string n |> Seq.toArray |> Array.rev |> Array.map string |> Array.reduce (+) |> bigint.Parse
    let isPalindrome n = n = rev n
    let isLychrel n =
        let rec isLychrelI n i =
            if i > 50 then true
            elif isPalindrome n then false
            else isLychrelI (n + rev n) (i + 1)
        isLychrelI (n + rev n) 0

    [1..10000] |> List.map (fun c -> bigint c) |> List.filter isLychrel |> List.length

module ``problem 56`` =
    [for a in 80..99 do for b in 80..99 -> bigint.Pow ((bigint a),b) ] |> List.map (fun n -> n |> string |> Seq.map (fun c -> int c - int '0') |> Seq.sum) |> Seq.max


module ``Problem 57`` =
    let rec calc iter nom denom =                
        if iter = 0 then nom + denom, denom
        else calc (iter - 1) denom (nom + (bigint 2) * denom)

    [for x in 2..998 do yield calc x (bigint 2) (bigint 5)]
    |> List.filter(fun (n,d) -> n.ToString().Length > d.ToString().Length)
    |> List.length

module ``Problem 58`` =
    open Utility
    let gendiag n = (Seq.init 4 (fun j -> Seq.init n (fun i -> pown (1 + i * 2) 2 - i * 2 * j)) |> Seq.collect id)
    let gendiagside sidel = gendiag (sidel / 2 + 1) |> Seq.toArray |> Array.filter ((<>) 1)
    
    let ratio diag =
        let s = gendiagside diag
        let p = s |> Array.filter (int64 >> isPrime)
        float (Array.length p) / float (Array.length s + 1)
    
    [ for x in 7..10000 do if x % 2 = 1 then yield x]
    |> Seq.takeWhile (fun i -> ratio i > 0.1)
    |> Seq.last

    [ for x in 30000..30002 do if x % 2 = 1 then yield x]
    |> Seq.map (fun i -> ratio i) |> Seq.last

    let rec binary s e f l (lookup : int array) =
        let d = (s + e) / 2
        if s = e then 
            s, e, (f d)
        elif f lookup.[d] > l then 
            printfn "found %f for %i" (f lookup.[d]) lookup.[d]
            binary d e f l lookup 
        else 
            printfn "found %f for %i" (f lookup.[d]) lookup.[d]
            binary s d f l lookup

    let lookup = [| for x in 20000..30002 do if x % 2 = 1 then yield x|]
    binary 0 (Array.length lookup - 1) ratio 0.1 lookup 
    // 26241

module ``problem 59`` =
    let path = @"C:\Users\Aleksandar\Desktop\p059_cipher.txt"
    let content = 
        System.IO.File.ReadAllText(path).Split(',')
        |> Array.map (System.Int32.Parse >> char)

    let decode (pass : char array) =
        let passLength = Seq.length pass
        content 
        |> Array.mapi (fun i l -> int l ^^^ (int pass.[i % passLength]) |> char)

    let passCombinations =
        [for x in 'a'..'z' do for y in 'a'..'z' do for z in 'a'..'z' do yield [|x;y;z|]]

    let decodes = [for pass in passCombinations do yield decode pass]

    let code = 
        decodes
        |> List.map (fun decode -> decode, decode |> Seq.where ((=) 'e') |> Seq.length)
        |> Seq.sortBy (snd >> (~-) )// number of 'and'
        |> Seq.nth 1

    code |> fst |> Seq.map int |> Seq.sum
    // seconds is the answer.

module ``problem 60`` =
    open Utility
    let fastConcat x y = x * (int64 <| pown 10 (int (log10 (float y)) + 1)) + y
    let isConcatPrime a b = isPrime (fastConcat a b) && isPrime (fastConcat b a)    

    let primes = [3L..10000L] |> List.filter isPrime

    let rec calc combs l =
        if l = 5 then combs
        else
            calc [for c in combs do 
                    for p in primes do
                        if p > List.head c &&
                           c |> List.forall (isConcatPrime p) then yield p::c] (l + 1)
    calc (primes |> List.map (fun e -> [e])) 1
    // [[8389L; 6733L; 5701L; 5197L; 13L]] (50s....)

module ``problem 61`` =
    open Utility
    let numbers = 
        [   fun n -> n * (n + 1) / 2
            fun n -> n * n
            fun n -> n * (3 * n - 1) / 2
            fun n -> n * (2 * n - 1)
            fun n -> n * (5 * n - 3) / 2
            fun n -> n * (3 * n - 2) 
        ] |> List.map (fun fn -> [10..200] |> List.map fn |> List.filter (fun n -> n >= 1000 && n <= 9999))
    
    let isPossible prev next = prev % 100 = next / 100
    let rec build families hist =
        match families, hist with
        | [], _ -> [hist |> List.rev]
        | curr::tail, [] -> 
            [for e in curr -> build tail (e::hist)] |> List.collect id
        | curr::tail, hist ->
            let last = List.head hist
            [for e in curr |> List.filter (fun e -> isPossible e last) -> build tail (e::hist)] 
            |> List.collect id
            
    (List.tail numbers |> permute |> List.map (fun t -> (List.head numbers)::t)) |> Array.ofList
    |> Array.Parallel.map (fun f -> build f [])
    |> List.ofArray
    |> List.collect id
    |> List.filter (fun ls ->
        isPossible (List.head ls) (List.head (List.rev ls))) |> List.map (List.sum)          

module ``problem 62`` =
    let hash (n : int64) =
        let rec hashInternal = function | 0L -> [] | n -> (n % 10L)::(hashInternal (n/10L))
        let list = hashInternal n |> List.sort
        List.foldBack (fun c s -> s * 10L + c) list 0L
    let cube (n : int64) = n * n * n
    [100L..30000L] |> List.map (fun n -> n, n |> cube |> hash)
    |> Seq.groupBy snd |> Seq.map (fun (n, g) -> n, g |> Seq.map fst, g |> Seq.length)
    |> Seq.where (fun (_, _, l) -> l = 5)
    |> Seq.head |> fun (_, n, _) -> n |> Seq.head

module ``problem 63`` =

    [for b in 1 .. 9 do
        for p in 1 .. 30 do
            let t = log10(float(b))
            if System.Math.Floor((float p) * t + 1.) |> int = p then
                yield b,p ] |> List.length

    let genFractions (num : int)=
        let sqrtW = int << floor << sqrt << float
        let a0 = sqrtW num
        let c = 1
        let b = a0

        let rec genSeq b c history =
            let alfaB = num - b * b
            let gcd = Utility.gcd alfaB c
            let alfaBGcd = alfaB / gcd
            let cgcd = c / gcd
            let a = (a0 + b) / alfaBGcd
            let c = alfaBGcd
            let b = a * alfaBGcd - b
            if List.exists ((=) (b,c)) history then 0
            else 1 + genSeq b c ((b,c)::history)

        genSeq b c []

    let rationalRoots = [1..100] |> List.map (fun i -> i * i)
    let irrationalRoots = [1..10000] |> List.filter (fun i -> rationalRoots |> List.exists (fun r -> i = r) |> not)

    irrationalRoots |> List.map genFractions |> List.filter (fun i -> i % 2 = 1) |> List.length

module ``problem 64`` =
    let fn c (p1,p2) = c * p2 + p1, p2

    let rec conv (a : int list) (p1 : bigint) (p2 : bigint) =
        match a with
        | ah::at -> 
            let p11, p21 = fn (bigint ah) (p1, p2)
            conv at p21 p11
        | [] -> p2 + p1, p2

    let lst = 1::2::(List.init 97 (fun i -> match (i % 3) with | 0 | 1 -> 1 | 2 -> (i + 1) / 3 * 2 + 2))
    let _, b = conv lst (bigint 1) (bigint 2)

    b |> string |> Seq.map (fun n -> int n - int '0') |> Seq.sum

module ``problem 65`` =
    let genFractions (num : int)=
        let sqrtW = int << floor << sqrt << float
        let a0 = sqrtW num
        let c = 1
        let b = a0

        let rec genSeq b c history =
            let alfaB = num - b * b
            let gcd = Utility.gcd alfaB c
            let alfaBGcd = alfaB / gcd
            let cgcd = c / gcd
            let a = (a0 + b) / alfaBGcd
            let c = alfaBGcd
            let b = a * alfaBGcd - b
            if List.exists ((=) (b,c)) history then []
            else (a,b,c)::genSeq b c ((b,c)::history)

        genSeq b c []
    
    let rec pn (cf:bigint list) i ``p(i-1)`` ``p(i-2)`` =
        match i, cf with
        | 0L, a0::cf -> a0::(pn cf (i+1L) a0 (bigint(0L)))
        | 1L, a1::cf -> 
            let succ = ``p(i-1)`` * a1 + bigint(1L)
            succ::(pn cf (i+1L) succ ``p(i-1)``)
        | n, h::cf -> 
            let succ = ``p(i-1)`` * h + ``p(i-2)``
            succ::(pn cf (i+1L) succ ``p(i-1)``)
        | n, [] -> []

    let rec qn cf i ``p(i-1)`` ``p(i-2)`` =
        match i, cf with
        | n, [] -> []
        | 0L, _ -> bigint.One::(qn cf (i+1L) bigint.One bigint.Zero)
        | 1L, a0::a1::cf -> 
            a1::(qn cf (i+1L) a1 ``p(i-1)``)
        | n, an::cf -> 
            let succ = ``p(i-1)`` * an + ``p(i-2)``
            succ::(qn cf (i+1L) succ ``p(i-1)``)

    let solve D =
        let fractions = genFractions D |> List.map (fun (x,_,_) -> bigint x)
        let a0 = D |> float |> sqrt |> int
        if a0 % 2 = 1 then
            let ps = pn ((bigint a0)::(fractions@fractions)) 0L bigint.Zero bigint.Zero
            let qs = qn ((bigint a0)::(fractions@fractions)) 0L bigint.Zero bigint.Zero
            ps |> List.rev |> Seq.nth 1, qs |> List.rev |> Seq.nth 1
        else
            let ps = pn ((bigint a0)::fractions) 0L bigint.Zero bigint.Zero            
            let qs =  qn ((bigint a0)::fractions) 0L bigint.Zero bigint.Zero
            ps |> List.rev |> Seq.nth 1, qs |> List.rev |> Seq.nth 1

    let D = 13

    let squares = [1..40] |> List.map (fun i -> i * i) |> Set.ofList
    (([990..1000] |> Set.ofList) - squares) |> Set.toList |> List.map (fun D -> solve D, D) |> List.sortBy (fst >> fst >> (~-)) |> Seq.take 5