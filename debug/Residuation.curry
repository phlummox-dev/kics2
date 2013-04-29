--- Suspending not
snot b = case b of
  True  -> False
  False -> True

-- OK

-- ()
test0 = ensureNotFree ()
-- suspend
test1 = ensureNotFree x where x free
-- !
test2 = ensureNotFree failed
-- suspends
test3 = ensureNotFree x =:= () where x free
-- Success
test4 = ensureNotFree x =:= y & x =:= () where y, x free
-- Success
test5 = snot x =:= y & x =:= True  where x, y free
-- Success, Success
test6 = ensureNotFree x =:= y & x =:= (True ? False) where x,y free
-- suspends
test7 = snot x =:= y & y =:= True  where x, y free
-- suspends
test8 = snot x =:= y  & snot y =:= x                where x, y free
-- {y = True} Success
-- {y = False} Success
test9 y = ensureNotFree x =:= y & x =:= (True ? False) where x free
-- Success
test10 = snot x =:= y  & snot y =:= x  & x =:= True  where x, y free
-- Success
test11 = snot x =:= y  & (snot y =:= x & y =:= True) where x, y free
-- Success
test12 = (snot x =:= y & snot y =:= x) & y =:= True  where x, y free
test13 ys = ensureSpine xs =:= ys & xs =:= [True, False, True] where xs free


data Nat = Z | S Nat

type Value = Nat

zero  = Z
one   = S Z
two   = S one
three = S two
four  = S three

Z   $+$ n = n
S n $+$ m = S (n $+$ m)

n   $-$ Z   = n
S n $-$ S m = n $-$ m

data Message = Deposit Value | Withdraw Value | Balance Value

account :: Value -> [Message] -> Success
account _ []                 =  success
account n (Deposit  a : ms)  =  account (n $+$ a) ms
account n (Withdraw a : ms)  =  account (n $-$ a) ms
account n (Balance  b : ms)  =  b =:= n & account n ms

make_account s = account zero (ensureSpine s) -- create bank account

-- goals:
goal1 b = let s free in
          make_account s & s=:=[Deposit one, Deposit one, Balance b]
goal2 b = let s free in
          make_account s &
            s=:=[Deposit four, Withdraw two, Deposit one, Balance b]

 -- send a message:
sendMsg msg obj | obj =:= msg:obj1  = obj1  where obj1 free -- send a message

-- client process for bank account:
client s | s1 =:= sendMsg (Balance b) s =
  if b == one
    then s1 =:= []   -- stop process
    else if b > one then client (sendMsg (Withdraw two  ) s1)  -- buy
                    else client (sendMsg (Deposit  three) s1)  -- work
  where s1,b free

goal3 s = make_account s & [] =:= (sendMsg (Deposit one) s) -- simulation
goal4 s = make_account s & s1 =:= sendMsg (Balance     b) s
                         & (if b == zero then s1 =:= [] else failed)
                         where s1,b free

goal5 s = make_account s & client (sendMsg (Deposit two) s) -- simulation
