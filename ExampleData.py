#Mock values
EX_0_DESCRIPTION =  """
    Vipul is a hardworking super-hero who maintains the bracket ratio of all the strings in the world. 
    Recently he indulged himself in saving the string population so much that he lost his ability for 
    checking brackets (luckily, not permanently ).Being his super-hero friend help him in his time of hardship.

    Input

    The first line of the input contains an integer T denoting the number of test cases. The description of T test cases follows.
    The first line of each test case contains a single string S denoting the string to be checked.

    Output

    For each test case, output a single line printing "YES" or "NO" (without " " and in uppercase only) , denoting if the brackets in the given string is balanced or not .

    Constraints

    1 ≤ T ≤ 10
    1 ≤ length of S ≤ 60

    Example
    Input:
    3
    ((()))
    (())()
    ()(()

    Output:
    YES
    YES
    NO

    Explanation
    Example is self-explanatory."""

EX_0_PUBLIC_TESTS =   """
    Public test 1:
    Input:
    3
    ((()))
    (())()
    ()(()
    Output:
    YES
    YES
    NO

    """


EX_0_SOLUTION = r"""
    main :: IO ()
    main = do
      t <- readLn :: IO Int
      loop t
      where
        loop 0 = return ()
        loop n = do
          s <- getLine
          putStrLn (if balanced s then "YES" else "NO")
          loop (n - 1)

    balanced :: String -> Bool
    balanced = go 0
      where
        go 0 [] = True
        go _ [] = False
        go n (x:xs) = case x of
          '(' -> go (n + 1) xs
          ')' -> let n' = n - 1 in n' >= 0 && go n' xs
          _   -> False
      
  """


