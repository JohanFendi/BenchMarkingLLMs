import pytest
from datasets import load_dataset
from ollama import Client
from os import getpid

from src.LLMPrompting.OllamaPrompter import OllamaPrompter
from src.DataManagment.DBreading.DBReader import DBReader
from src.DataManagment.Preprocessing.CodeContestsPreprocessor import CodeContestsPreprocessor
from src.Compilation.ProcessFolderCreator import ProcessFolderCreator
from src.Compilation.WindowsGhcCompiler import WindowsGhcCompiler
from src.SolutionWriting.RegularSolutionWriter import RegularSolutionWriter
from src.SolutionTesting.Tester import Tester
from src.DataManagment.DBwriting.csvWriter import CSVWriter
from constants import TASK_DESCRIPTION



dataset = load_dataset("deepmind/code_contests")["train"]

def test_case_zero_with_mock_solution():     
    index = 0
    pid = getpid()
    file_name = "main"
    csv_file_name  = "test"
    folder_name = "haskell"
    column_names = ["status", "returncode", "testcase_index"]

    mock_solution = r"""
main :: IO ()
main = interact $ \input ->
  let (_:ss) = lines input
  in unlines $ map (\s -> if balanced s then "YES" else "NO") ss

balanced :: String -> Bool
balanced = go 0
  where
    go 0 []     = True
    go _ []     = False
    go n (x:xs)
      | n < 0         = False
      | x == '('      = go (n + 1) xs
      | x == ')'      = go (n - 1) xs
      | otherwise     = go n xs

    """

    preprocessor = CodeContestsPreprocessor(dataset)
    folder_creator = ProcessFolderCreator()
    compiler = WindowsGhcCompiler()
    solution_writer = RegularSolutionWriter()
    db_writer = CSVWriter(csv_file_name, column_names, 10)
    tester = Tester()
   

    folder_creator.init_folders(folder_name, [pid])
    solution_writer.write_solution(file_name, folder_name, pid, mock_solution, ".hs")
    command = compiler.compile(pid, folder_name, file_name)

    tests = preprocessor.getTestCases(index)
    testcase_inputs, testcase_outputs = tests["output"], tests["input"]
    (status, result, testcase_index) = tester.run_test_cases(command, testcase_inputs, testcase_outputs)
    return_code = result.returncode
    db_writer.write(column_names, [status, return_code, testcase_index])

    assert status == "PASSED", f"Status {status} does not match PASSED"
    assert return_code == 0, f"Returncode should be 0, not {return_code}"



def test_first_ten_with_Ollama(): 
    
    for i in range(10): 
      model = "llama3.2-vision:latest"
      pid = getpid()
      file_name = "main"
      csv_file_name  = "test"
      folder_name = "haskell"
      column_names = ["status", "returncode", "testcase_index"]


      preprocessor = CodeContestsPreprocessor(dataset)
      llm_prompter = OllamaPrompter(model)
      folder_creator = ProcessFolderCreator()
      compiler = WindowsGhcCompiler()
      solution_writer = RegularSolutionWriter()
      db_writer = CSVWriter(csv_file_name, column_names, 10)
      tester = Tester()
  
      
      formated_public_tests = preprocessor.getFormatedPublicTests(i)
      problem_description = preprocessor.getProblemDescription(i)
      solution = llm_prompter.prompt(TASK_DESCRIPTION, problem_description, formated_public_tests)

      folder_creator.init_folders(folder_name, [pid])
      solution_writer.write_solution(file_name, folder_name, pid, solution, ".hs")
      command = compiler.compile(pid, folder_name, file_name)

      tests = preprocessor.getTestCases(i)
      testcase_inputs, testcase_outputs = tests["output"], tests["input"]
      (status, result, testcase_index) = tester.run_test_cases(command, testcase_inputs, testcase_outputs)
      return_code = result.returncode
      db_writer.write(column_names, [status, return_code, testcase_index])
      db_writer._flush()

      assert status == "PASSED", f"Status {status} does not match PASSED"
      assert return_code == 0, f"Returncode should be 0, not {return_code}"