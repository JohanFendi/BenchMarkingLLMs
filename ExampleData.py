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
    ```Haskell
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
      ```
  """

EX_0_SOLUTION2 = """
'To solve this problem, we need to determine if the brackets in a given string are balanced. The solution involves checking each character in the string to ensure that every opening bracket \'(\' has a corresponding closing bracket \')\', and that they are properly nested. \n\n### Approach\n1. **Problem Analysis**: The problem requires checking if the brackets in a string are balanced. The string may contain non-bracket characters, which should be ignored. The solution must efficiently process each string to verify balance.\n2. **Intuition**: We use a counter to track the balance of brackets. For each opening bracket \'(\', we increment the counter, and for each closing bracket \')\', we decrement it. If at any point the counter becomes negative, it indicates an unmatched closing bracket, making the string unbalanced. After processing all characters, the counter must be zero for the string to be balanced.\n3. **Algorithm Selection**: The algorithm processes each character in the string:\n   - For \'(\', increment the counter.\n   - For \')\', decrement the counter. If the counter becomes negative, return unbalanced.\n   - For non-bracket characters, leave the counter unchanged.\n   - After processing all characters, if the counter is zero, the string is balanced; otherwise, it is not.\n4. **Complexity Analysis**: The algorithm processes each character in the string exactly once, leading to a time complexity of O(n) per test case, where n is the length of the string. The space complexity is O(1) as we only use a counter.\n\n### Solution Code\n```Haskell\nimport Control.Monad (replicateM, forM_)\n\nmain :: IO ()\nmain = do\n  t <- readLn\n  strings <- replicateM t getLine\n  forM_ strings $ \\s -> \n      if valid s then putStrLn "YES" else putStrLn "NO"\n  where\n    valid s = go 0 s\n    go count [] = count == 0\n    go count (c:cs) = \n        let new = case c of\n               \'(\' -> count + 1\n               \')\' -> count - 1\n               _ -> count\n        in if new < 0 then False else go new cs\n```\n\n### Explanation\n1. **Reading Input**: The main function reads the number of test cases `T` and then reads `T` strings.\n2. **Processing Each String**: For each string, the function `valid` checks if the brackets are balanced.\n3. **Validation Function**: The `valid` function uses a helper function `go` that:\n   - Takes a counter and the string.\n   - For each character in the string:\n     - If it\'s \'(\', increments the counter.\n     - If it\'s \')\', decrements the counter. If the counter becomes negative, returns `False` immediately.\n     - Ignores non-bracket characters.\n   - After processing all characters, checks if the counter is zero (indicating balanced brackets).\n4. **Output**: For each string, "YES" is printed if the brackets are balanced, otherwise "NO" is printed.\n\nThis approach efficiently checks the balance of brackets by processing each character once and using a counter to track the nesting depth, ensuring optimal performance.'"""

