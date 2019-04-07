-- Reverse a sequence.
revSeq :: [Char] -> [Char]
revSeq [] = []
revSeq (x:xs) = revSeq xs ++ [x]

-- Complement a base.
compBase 'A' = 'T'
compBase 'a' = 't'
compBase 'T' = 'A'
compBase 't' = 'a'
compBase 'C' = 'G'
compBase 'c' = 'g'
compBase 'G' = 'C'
compBase 'g' = 'c'
compBase 'Y' = 'R'
compBase 'y' = 'r'
compBase 'R' = 'Y'
compBase 'r' = 'y'
compBase 'S' = 'S'
compBase 's' = 's'
compBase 'W' = 'W'
compBase 'w' = 'w'
compBase 'K' = 'M'
compBase 'M' = 'K'
compBase 'k' = 'm'
compBase 'm' = 'k'
compBase 'B' = 'V'
compBase 'V' = 'B'
compBase 'b' = 'v'
compBase 'v' = 'b'
compBase 'D' = 'H'
compBase 'H' = 'D'
compBase 'd' = 'h'
compBase 'h' = 'd'
compBase 'N' = 'N'
compBase 'n' = 'n'
compBase _ = error "Unknown base code"

-- Complement a sequence.
compSeq :: [Char] -> [Char]
compSeq [] = []
compSeq (x:xs) = [compBase x] ++ (compSeq xs)

-- Reverse complement a sequence.
revCompSeq :: [Char] -> [Char]
revCompSeq [] = []
revCompSeq s = revSeq (compSeq s)

-- dna.hs ends here
