https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
import Debug.Trace

data Game = Game {stacks :: [Stack]}
    deriving (Eq)
type Stack = [Disk]
-- the list of discs starts from top to bottom
type Disk = Int -- 1 ..5

-- 1. Check Valid
-- from top to bottom, the Disk size must be increasing
is_valid_stack :: [Disk] -> Bool
is_valid_stack col = case col of
    []      -> True
    [d]     -> True
    d1:d2:ds-> (d1<d2) && is_valid_stack (d2:ds)
      
is_valid_stacks ::[Stack] -> Bool
is_valid_stacks stacks = case stacks of
    []      -> True
    p:ps    -> is_valid_stack p && is_valid_stacks ps
      
is_valid_game ::Game -> Bool
is_valid_game (Game stacks) = is_valid_stacks stacks

-- 2. Print game
print_game :: Game -> String
print_game (Game [p1,p2, p3]) = "\n" ++ transpose_and_combine p1 p2 p3 
                                ("stack_1  " ++ "stack_2  " ++ "stack_3\n\n")  
    
transpose_and_combine :: [Disk] -> [Disk] -> [Disk] -> String -> String
transpose_and_combine col1 col2 col3 acc = case (col1, col2, col3) of
    ([],[],[])  -> acc
    (_,_,_)     -> case (extract_last col1,
                         extract_last col2, 
                         extract_last col3) of
        ((lst1, rem1),(lst2, rem2), (lst3, rem3)) -> 
                        transpose_and_combine rem1 rem2 rem3 
                        (lst1 ++ "      " ++ lst2 
                        ++ "      " ++ lst3 ++ "\n" ++ acc)
            
extract_last :: [Disk] -> (String, [Disk])
extract_last column = case column of
    []      -> (" ",[])
    [d]     -> (show d,[])
    d:ds    -> (show last_d, [i|i<-column, i/=last_d])
    where
        last_d = last column
-- 3. Move disc

-- 4. Solve the game

-- Test the program
test = do 
    let initial_game = Game [[1,2,3,4,5],[],[]]
    print $ is_valid_game initial_game    
    putStr $ print_game initial_game