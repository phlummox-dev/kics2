import CLPFD2

alpha :: [[Int]]
alpha =
  let alphabet    = take 26 (domain 1 26)
      word as     = foldr (\x y -> as !# (fdc (ord x - ord 'a')) +# y) (fdc 0)
      constraints = allDifferent alphabet                /\
                    word alphabet "ballet"    =# fdc 45  /\
                    word alphabet "cello"     =# fdc 43  /\
                    word alphabet "concert"   =# fdc 74  /\
                    word alphabet "flute"     =# fdc 30  /\
                    word alphabet "fugue"     =# fdc 50  /\
                    word alphabet "glee"      =# fdc 66  /\
                    word alphabet "jazz"      =# fdc 58  /\
                    word alphabet "lyre"      =# fdc 47  /\
                    word alphabet "oboe"      =# fdc 53  /\
                    word alphabet "opera"     =# fdc 65  /\
                    word alphabet "polka"     =# fdc 59  /\
                    word alphabet "quartet"   =# fdc 50  /\
                    word alphabet "saxophone" =# fdc 134 /\
                    word alphabet "scale"     =# fdc 51  /\
                    word alphabet "solo"      =# fdc 37  /\
                    word alphabet "song"      =# fdc 61  /\
                    word alphabet "soprano"   =# fdc 82  /\
                    word alphabet "theme"     =# fdc 72  /\
                    word alphabet "violin"    =# fdc 100 /\
                    word alphabet "waltz"     =# fdc 34
  in solveFD [GecodeSearch] constraints
