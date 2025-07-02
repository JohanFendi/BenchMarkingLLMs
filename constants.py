SYSTEM_PROMPT = "You are a Haskell expert."

TASK_DESCRIPTION = """
    Your task is to solve the following problem by writing Haskell code.

    Requirements:
    1. Implement a `main :: IO ()` that reads all input from stdin and writes the answer to stdout.
    2. Do not wrap your code in ```Haskell```.
    3. Do not include comments, explanations, or markdown — only raw code.
    4. You may **only** import these modules if needed: Data.List, Data.Char, Data.Map.Strict, Data.Set, Data.Array and Control.Monad. 
    5. When importing a module **you must only** list the specific functions you need in parentheses. For example: import Data.List (sort, nub). 
    6. Use only these Prelude I/O/parsing functions (no imports needed): getChar, getLine, getContents, interact, readLn, read, words and unwords. 
    7. Add explicit type signatures. 
"""

#Status strings

COMPILATION_ERROR_STRING = "COMPILATION_ERROR"
ERROR_STRING = "ERROR"
FAILED_STRING = "FAILED"
PASSED_STRING = "PASSED"

NO_FAILED_TESTCASES_INDEX = -1
NO_FAILED_TESTCASES_OUTPUT = ""

AVAILABLE_COLUMNS = [
    "status",
    "problem_solution",
    "failed_testcase_index",
    "failed_output",
    "stderr",
    "return_code",
]

#Mock values

PROBLEM_ONE_DESCRIPTION =  """
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

PROBLEM_ONE_PUBLIC_TESTS =   """
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

PROBLEM_ONE_SOLUTION = r"""
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


