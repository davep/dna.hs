-- Reverse a sequence.
revSeq :: [Char] -> [Char]
revSeq [] = []
revSeq (x:xs) = revSeq xs ++ [x]

-- Complement a base.
compBase 'A' = 'T'
compBase 'T' = 'A'
compBase 'C' = 'G'
compBase 'G' = 'C'

-- Complement a sequence.
compSeq :: [Char] -> [Char]
compSeq [] = []
compSeq (x:xs) = [compBase x] ++ (compSeq xs)

-- Reverse complement a sequence.
revCompSeq :: [Char] -> [Char]
revCompSeq [] = []
revCompSeq s = revSeq (compSeq s)

-- dna.hs ends here
