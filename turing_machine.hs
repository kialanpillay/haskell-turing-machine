type Symbol = Char
type State = Int

data Tape = Tape [Symbol] Symbol [Symbol]

type TransitionFunction = State -> Tape -> Maybe (State, Tape)

data TuringMachine = TuringMachine [State] [Symbol] TransitionFunction State Tape 

left :: Tape -> Tape
left (Tape (x:xs) y zs) = Tape xs x (y:zs)

right :: Tape -> Tape
right (Tape xs y (z:zs)) = Tape (y:xs) z zs

write :: Tape -> Symbol -> Tape
write (Tape xs y zs) a = Tape xs a zs

inputLength :: Tape -> Int
inputLength (Tape xs y zs) | y == 'a'   = 1 + inputLength (right (Tape xs y zs))
                           | y == 'b'   = 1 + inputLength (right (Tape xs y zs))
                           | otherwise  = 0

tape :: [Char] -> Tape
tape [] = Tape blanks ' ' blanks
tape (x:xs ) = Tape blanks x (xs ++ blanks)

simulate :: TuringMachine -> Tape
simulate (TuringMachine qs as delta q0 t0) =
    case delta q0 t0 of    
        Just (q1, t1) -> simulate (TuringMachine qs as delta q1 t1)
        Nothing       -> t0


blanks :: [Symbol]
blanks = repeat ' '

output :: Tape -> Symbol
output (Tape xs a zs) = a

delta :: TransitionFunction
delta q (Tape xs a ys) =
  case (q, a) of
    (0,'a')  -> Just (1, right(write (Tape xs a ys) ' '))
    (0,'b')  -> Just (4, right(write (Tape xs a ys) ' '))
    (0,' ')  -> Just (6, write (Tape xs a ys) 'a')
    (1,'a')  -> Just (1, right(write (Tape xs a ys) 'a'))
    (1,'b')  -> Just (1, right(write (Tape xs a ys) 'b'))
    (1,' ')  -> Just (2, left(write (Tape xs a ys) ' '))
    (2,'a')  -> Just (3, left(write (Tape xs a ys) ' '))
    (2,'b')  -> Just (7, write (Tape xs a ys) 'b')
    (2,' ')  -> Just (6, write (Tape xs a ys) 'a')
    (3,'a')  -> Just (3, left(write (Tape xs a ys) 'a'))
    (3,'b')  -> Just (3, left(write (Tape xs a ys) 'b'))
    (3,' ')  -> Just (0, right(write (Tape xs a ys) ' '))
    (4,'a')  -> Just (4, right(write (Tape xs a ys) 'a'))
    (4,'b')  -> Just (4, right(write (Tape xs a ys) 'b'))
    (4,' ')  -> Just (5, left(write (Tape xs a ys) ' '))
    (5,'a')  -> Just (7, write (Tape xs a ys) 'b')
    (5,'b')  -> Just (3, left(write (Tape xs a ys) ' '))
    (5,' ')  -> Just (6, write (Tape xs a ys) 'a')
    (6,_) ->  Nothing
    (7,_) ->  Nothing
    (_,_) ->  error "The input string x must only contain the symbols {a,b}"

-- (Tape blanks 'b' ("ba" ++ blanks))
-- (Tape blanks 'b' ("aab" ++ blanks))
-- (Tape blanks 'a' ("bab" ++ blanks))
-- (Tape blanks 'a' ("bab" ++ blanks))
-- (Tape blanks 'a' ("ba" ++ blanks))
-- (Tape blanks 'a' ("aa" ++ blanks))
-- (Tape blanks 'b' ("abab" ++ blanks))
-- (Tape blanks ' ' blanks)
-- f "aba"

f :: [Char] -> Symbol
f x | inputLength (tape x) < 5 =  output (simulate (TuringMachine [0,1,2,3,4,5,6,7] ['a','b', ' '] delta 0 (tape x)))
    | otherwise         =  error "The input string x can be up to length N = 4"