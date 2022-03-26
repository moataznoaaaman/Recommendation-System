
import DataFile
--import System.Random 
--import System.IO.Unsafe
--randomZeroToX :: Int -> Int 
--randomZeroToX x = unsafePerformIO (getStdRandom (randomR (0, x)))


--is the recommend function correct? recommend empty cart gives empty string?
--dataset format? ZIP
--how to test recommend?
--p10 of tests for user1, i got item 2 and item 5 too?
--p13 recommend "user2" ["item1", "item2"] i got other than 4
--recommend "user2" ["item1", "item5"] 
-- recommendEmptyCart "user4" ? empty string not list?

----------------------------------------------DataSet------------------------------------------------------


--users = ["user1", "user2", "user3", "user4 "] 
--items = ["item1", "item2", "item3", "item4", "item5", "item6"] 
--purchasesHistory = [ ("user1", [["item1", "item2", "item3"] , ["item1", "item2", "item4"]]) , ("user2", [["item2", "item5"] , ["item4", "item5"]]) , ("user3", [["item3", "item2"]]) , ("user4", []) ]
 
 
-------------------------------------createEmptyFreqList---------------------------------------------------

createEmptyFreqList [x] = [(x,[])]
createEmptyFreqList (x:xs) = [(x,[])] ++ createEmptyFreqList xs  

----------------------------------------getAllUsersStats---------------------------------------------------

--takes purchases history 
--expected output in description.

getAllUsersStats [x] = [perUser x]
getAllUsersStats (x:xs) = [perUser x]  ++ getAllUsersStats xs

--takes ("user1", [[" item1", "item2", "item3"] , ["item1", " item2", "item4 "]]) 
--expected to return : (for user1 in this case)
--("user1 " ,[("item1" ,[("item2",2) ,("item3",1) ,("item4",1) ]) ,
--("item2" ,[(" item1",2) ,("item3",1) ,("item4",1) ]) ,
--("item3" ,[("item1",1) ,("item2 ",1) ]) ,
--("item4" ,[("item1",1) ,("item2",1) ]) ,
--("item5 " ,[]) ,("item6 " ,[]) ])
perUser (person, []) = (person, createEmptyFreqList items)
perUser  (person, carts) = (person, getStats items carts )

--give it a list of items and the carts of a user
--[[" item1", "item2", "item3"] , ["item1", " item2", "item4 "]]
--expected to return: 
--[("item1" ,[("item2",2) ,("item3",1) ,("item4",1) ]) ,
--("item2" ,[(" item1",2) ,("item3",1) ,("item4",1) ]) ,
--("item3" ,[("item1",1) ,("item2 ",1) ]) ,
--("item4" ,[("item1",1) ,("item2",1) ]) ,
--("item5 " ,[]) ,("item6 " ,[]) ]

getStats [it] carts = [perItemDec it carts]
getStats (it:itx) carts = [perItemDec it carts] ++ getStats itx carts

--give it  the specific item carts, expect it to return (item, count list)

perItemDec itN carts = (itN, clean itN (addItems itN carts (createEmptyCounter items)))

--operate on each cart
--"item1"
--[["item1", "item2", "item3"] , ["item1", "item2", "item4"]]
--to output:[("item2",2) ,("item3",1) ,("item4",1) ]

addItems itN [x] list = if (elem itN x) then incrItems itN x list else list

addItems itN (x:xs) list = if (elem itN x) then addItems itN xs (addItems itN [x] list) else addItems itN xs list


incrItems itN [x] list = increment list x
incrItems itN (x:xs) list = incrItems itN xs (increment list x)

createEmptyCounter  [x] = [(x,0)]
createEmptyCounter  (x:xs) = [(x,0)] ++ createEmptyCounter  xs  

--give it the old counter list to give an updated list

increment [o] it = [updatePair o]
increment (o:os) it = if check o it then (updatePair o):os else o:(increment os it) 

updatePair (itN, c) = (itN, c+1)

check (itN, c) it = (itN==it)

--clean: removes unwanted instances from an item frequency list: 
--[("item1",2),("item2",2),("item3",1),("item4",1),("item5",0),("item6",0)]
--clean itN [] = []
clean itN [x] = if (checkTrash itN x) then [] else [x]
clean itN (x:xs) = if (checkTrash itN x) then clean itN xs else x:(clean itN xs)

checkTrash itN (n,c) = if ((itN == n) || c==0) then True else False

---------------------------------------recommendBasedOnUsers---------------------------------------------------

--UNCOMMENT:

--recommendBasedOnUsers :: String -> String

recommendBasedOnUsers toUser = if (length(freqListUsers toUser)==0) then "" 
else (flattenn (freqListUsers toUser)) !! (randomZeroToX (length (flattenn (freqListUsers toUser))-1))

--input: [("item3",3),("item1",6),("item4",2),("item2",3), ("item5", 2)]

flattenn [x] = rep x
flattenn (x:xs) = (rep x) ++ flattenn xs

rep (itN, 1) = [itN]
rep (itN, c) = [itN] ++ rep(itN,c-1)

---------------------------------------recommendEmptyCart----------------------------------------------------


--UNCOMMENT: 
recommendEmptyCart toUser = if (length(freqListItems toUser))==0 then [] else (flattenn (freqListItems toUser)) !! (randomZeroToX (length (flattenn (freqListItems toUser))-1))

---------------------------------------recommendBasedOnItemsInCart-------------------------------------------

--UNCOMMENT:
recommendBasedOnItemsInCart toUser cart = if (length(freqListItems toUser))==0 then [] else (flattenn (freqListCartAndItems toUser cart)) !! (randomZeroToX (length (flattenn (freqListCartAndItems toUser cart))-1))

--------------------------------------------recommend---------------------------------------------------------

recommend toUser cart   | (length(recommendBasedOnUsers toUser)==0 && length(recommendEmptyCart toUser)==0) = items !! (randomZeroToX ((length items)-1))
                        | (length(recommendBasedOnUsers toUser)==0 && cart == []) = recommendEmptyCart toUser
						| (length(recommendBasedOnUsers toUser)==0 && cart /= []) = recommendBasedOnItemsInCart toUser cart
						| (length(recommendEmptyCart toUser)==0 && length(recommendBasedOnUsers toUser)/=0) = recommendBasedOnUsers toUser
						| otherwise = if (randomZeroToX 1) == 0 then recommendBasedOnUsers toUser else if (cart ==[]) then recommendEmptyCart toUser else recommendBasedOnItemsInCart toUser cart 
						
--------------------------------------------purchasesIntersection------------------------------------------

----------------------------------------------freqListUsers------------------------------------------------

----------------------------------------------freqListItems----------------------------------------------
freqListItems:: String -> [(String, Int)]
freqListItems x = addTuples (returnTuples (snd (returnListOfUser x (getAllUsersStats purchasesHistory))))

--------------------------------------------freqListCartAndItems------------------------------------------

freqListCartAndItems:: String -> [String] -> [(String, Int)]
freqListCartAndItems s l = combine (freqListItems s) (freqListCart s l)

----------------------------------------------freqListCart------------------------------------------------

freqListCart:: String ->[String] -> [(String, Int)]
freqListCart s [] = []
freqListCart s c = countInstances (freqListCart2 c (returnCartOfUser s))

--puts all the "accessories" of the items in the cart in one giant list
freqListCart2 [] p = []
freqListCart2 (c:cs) p = countAccessories c p ++ freqListCart2 cs p

-----------------------------------------------helpers----------------------------------------------------

--returns the tuple containing user s & their previous carts
returnListOfUser s (x:xs)
	| (fst x) == s = x
	|otherwise = returnListOfUser s xs


--returns the list of previous carts of user s
returnCartOfUser:: String -> [[String]]
returnCartOfUser  s = snd (returnListOfUser s purchasesHistory)


--counts instances of item s in list
count:: String->[String]->Int
count s [] = 0
count s (x:xs) 
	| s==x = 1+ count s xs
	| otherwise = count s xs


--checks to see if x is cart c and if it is to puts its "accessories" in a list
countAccessories x [] = []
countAccessories x (c:cs)
	|(elem x c) = (addExcept x c) ++ countAccessories x cs
	|otherwise = countAccessories x cs
	
	
--puts all elements besides x in a list
addExcept x [] = []	
addExcept x (c:cs)
	|c /= x = c:addExcept x cs
	|otherwise = addExcept x cs
	
--flattens nested list
flat [] = []	
flat (x:xs) = x ++ flat xs

--removes all instances of x in list	
remove x [] = []
remove x (y:ys) 
	| x/=y = y:remove x ys
	| otherwise = remove x ys
	

--takes a list of repeated elements, puts the element and its count in a tuple
countInstances [] = []	
countInstances (x:xs) = [(x,count x (x:xs))] ++ countInstances (remove x xs)


--returns a list of tuples (item, #) men el 3ak bta3 getUsersStats 
returnTuples [] = []
returnTuples (x:xs) = snd x ++ returnTuples xs

--returns the numbers in tuples containing the same item
addNumFromTuples x [] = 0
addNumFromTuples x (y:ys)
	|fst y == x = snd y + addNumFromTuples x ys
	|otherwise = addNumFromTuples x ys

--removes tuples from list that contain element x
removeTuples x [] = []
removeTuples x (y:ys)
	|fst y /= x = y:(removeTuples x ys)
	|otherwise = removeTuples x ys

--tuples carrying the same item will have their numbers summed up
addTuples [] = []
addTuples (x:xs) = [(fst x, snd x + (addNumFromTuples (fst x) xs))] ++ addTuples (removeTuples (fst x) xs)


--adds the numbers of tuples containing the same item
combine [] l = l
combine (x:xs) l = [(fst x , snd x + (addNumFromTuples (fst x) l))] ++ combine xs (removeTuples (fst x) l)




--                     basic   HELPERS
--this function gets all the purchases items from purchasesHistory
get_p_i [] =[]
get_p_i ((a,l):(xs))=concat l++(get_p_i xs)


--this function returns the items bought by a user
i_for_u user []=[]
i_for_u user ((a,l):(xs)) |user==a = (concat l)
                          |otherwise = i_for_u user xs
						  
--this is responsible for calculating the frequencies 						  
freq i []=0
freq i (x:xs)|i==x=1+(freq i xs)
             |otherwise =	freq i xs	
--removing the dublicts from a list
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs)   | x `elem` xs   = rmdups xs
                | otherwise     = x : rmdups xs			 
			 
			 
--------functions
--this returns all the main items
getitems []=[]
getitems ((i,l):xs)	= [i]++(getitems xs)		

--------------------purchasesIntersection
handle [] a =[]
handle ((a,l):xs) ((b,k):xd) |l==[] ||k==[]= handle xs xd
                             |otherwise = [(a,(sumup1(l++k)))]++(handle xs xd)
purchasesIntersection a []=[]     	
purchasesIntersection a ((b,l):xs) =[(handle a l)]++ (purchasesIntersection a xs)		

sumup1 []=[]
sumup1 l= sumup2 (rmdups(getitems l)) l

sumup2 [] l=[]
sumup2 (x:xs) l=[(x,(sumup3 x l))]++(sumup2 xs l)

sumup3 x []=0
sumup3 x ((a,n):xs)|x==a=n+(sumup3 x xs)
                   |otherwise = sumup3 x xs  

----------------------freqlistusers
getto []=[]
getto (x:xs)=(getto1 x)++(getto xs)

getto1 []=[]
getto1 ((a,l):xs)=l++(getto1 xs)

---input1 is a function that takes the user input of freqListUsers and generate the first input of purchasesIntersection
---input2 is a function that takes the user input of freqListUsers and generate the second input of purchasesIntersection
-------------------------------
---------------------------------
---------------------------------
freqListUsers user=sumup1(getto(purchasesIntersection (getspecificuser1 user) (getotherusers1 user) ))
---------------the new part that takes the output of get all users stats and modifies it to be the input of purchasesIntersection
--first input of purchasesIntersection
getspecificuser1 user =  getspecificuser2 user (getAllUsersStats purchasesHistory)

getspecificuser2 user []=[]
getspecificuser2 user ((a,l):xs) |user==a = l
                                 | otherwise =getspecificuser2 user xs		
								 
								 
--second of purchasesIntersection								 
getotherusers1 user =getotherusers2	user (getAllUsersStats purchasesHistory)

getotherusers2 user []=[]
getotherusers2 user ((a,l):xs)		|user==a =getotherusers2 user xs
                                    |otherwise =[(a,l)]++(getotherusers2 user xs)
						







