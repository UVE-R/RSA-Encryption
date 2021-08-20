import System.Random ( newStdGen, mkStdGen, Random(randomR), RandomGen )
import Data.Char ( ord, chr )


--rabin miller loop
rabinMillerLoop :: Integer -> Integer -> Integer  -> Bool
rabinMillerLoop n d x
    | d == n - 1 = False
    | x == 1 = False
    | x == n-1 = True
    | otherwise =
        rabinMillerLoop n (d * 2) $ modexp x 2 n


--main rabin miller function
rabinMiller :: Integer -> Integer  -> Bool
rabinMiller n d = do

    let (a,gen) = randomR (2, n-2) (mkStdGen 100)

    let x = modexp a d n

    if (x == 1) || (x == n-1) then True
    else rabinMillerLoop n (d * 2) $ modexp x 2 n


--runs rabinMiller cnt number of times, returns an array of size cnt with the result of each test
runRabinMiller :: Integer -> Integer -> Integer -> [Bool] -> [Bool]
runRabinMiller n c cnt xs
    | cnt == 0 = xs
    | otherwise = runRabinMiller n c (cnt-1) (rabinMiller n c : xs)


--helper function for rabin miller
getC :: Integer -> Integer
getC n
    | even n = getC $ n `div` 2
    | otherwise = n


--small prime numbers used to check for primality and divisibility
lowPrimes :: [Integer]
lowPrimes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607, 613, 617, 619, 631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739, 743, 751, 757, 761, 769, 773, 787, 797, 809, 811, 821, 823, 827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997]


isPrime :: Integer  -> Bool
isPrime n
    | n<2 = False
    | elem n lowPrimes = True --n is a small prime
    | any (==True) $ map (\x-> (==0) $ mod n x) lowPrimes  = False --if n is divisible by a prime    
    | otherwise = all (==True) $ runRabinMiller n (getC n-1) 40 [] --run 40 times, if in all 40 tests it is true then n is probably prime


generateLargePrime :: (RandomGen t1)=> Integer -> t1 -> Integer -> Integer
generateLargePrime num gen keysize
    | isPrime num = num
    | otherwise = generateLargePrime randNum newGen keysize
    where (randNum,newGen) = randomR' keysize gen


modexp :: Integer -> Integer -> Integer -> Integer
modexp _ 0 _ = 1
modexp b e m
  | even e    = (r*r) `mod` m
  | otherwise = (b*r*r) `mod` m
  where
    r = modexp b (e `div` 2) m


--helper function to get a random number using keysize
randomR' :: (RandomGen t1) => Integer -> t1 -> (Integer, t1)
randomR' keysize gen = randomR ( 2^(keysize -1) , (2^keysize) - 1 ) gen


--returns the encryption and decryption key as a pair
generateKeys :: RandomGen t => Integer -> Integer -> Integer -> t -> (Integer, Integer)
generateKeys phiN e keysize state
    | isCoPrime e phiN = (e, modularInverse e phiN)
    | otherwise = generateKeys phiN e keysize gen
    where (e,gen) = randomR' keysize state


isCoPrime :: Integer -> Integer -> Bool
isCoPrime a b
    | gcd' a b ==1 = True
    | otherwise = False


--euclidean algorithm
gcd' :: Integral t => t -> t -> t
gcd' a b
    | b == 0 = a
    | otherwise = gcd' b $ mod a b


modularInverse :: Integral a => a -> a -> a
modularInverse a b
        | x<0 = x+b --make x positive
        | otherwise = x
        where x = egcd a b 1 0 0 1


--extended euclidean algorithm
egcd :: Integral t => t -> t -> t -> t -> t -> t -> t
egcd old_r r old_s s old_t t
    | r == 0 = old_s
    | otherwise = egcd r new_r s new_s t new_t
    where quotient = div old_r r
          new_r = old_r - (quotient * r)
          new_s = old_s - (quotient * s)
          new_t = old_t - (quotient * t)


encrypt :: Integer -> Integer -> [Char] -> [Integer]
encrypt e n msg = map ((\x -> (modexp x e n)) . toInteger . ord) msg


decrypt :: Integer -> Integer -> [Integer] -> String
decrypt d n msg = map ((chr . fromInteger) . (\x -> modexp x d n)) msg


main:: IO()
main = do

    let keysize = 20

    --get 2 primes
    g<- newStdGen
    let p = generateLargePrime 1 g keysize
    g<- newStdGen
    let q = generateLargePrime 1 g keysize

    let n = p * q
        phiN = (p-1) * (q-1)

    --get keys
    g<- newStdGen
    let (init,gen) = randomR' keysize g
        (e,d) = generateKeys phiN init keysize gen


    putStrLn "Enter message: "
    msg <- getLine

    --let msg = "Hello World!"
    let enc = encrypt e n msg
        dec = decrypt d n enc

    --IO
    putStrLn $ "Message: " ++ msg
    putStr "Encrypted: "
    print(enc)
    putStrLn $ "Decrypted: " ++ dec

    {-
    print(e)
    print(d)
    print(n)
    print(p)
    print(q)
    -}

