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
from src.LLMPrompting.GPTPrompter import GPTPrompter
import constants 
import LLMTaskDescriptions
import ExampleData


dataset = load_dataset("deepmind/code_contests")["train"]
model = "o4-mini-2025-04-16"
pid = getpid()
file_name = "main"
csv_file_name  = "test"
folder_name = "haskell"
column_names = ["status", "problem_solution", "failed_testcase_index", "failed_output", "stderr", "return_code"]

preprocessor = CodeContestsPreprocessor(dataset)
llm_prompter = GPTPrompter(model)
compiler = WindowsGhcCompiler()
solution_writer = RegularSolutionWriter()
db_writer = CSVWriter(csv_file_name, column_names, 10)
tester = Tester()
folder_creator = ProcessFolderCreator()
folder_creator.init_folders(folder_name, [pid])

verbose = True

#Tests total pipeline health 
def test_case_zero_with_mock_solution():     
    index = 0
    mock_solution = ExampleData.EX_0_SOLUTION
   
    folder_creator.init_folders(folder_name, [pid])
    solution_writer.write_solution(file_name, folder_name, pid, mock_solution, ".hs")
    command, return_code, stderr = compiler.compile(pid, folder_name, file_name)

    assert return_code == 0, f"Returncode should be 0, not {return_code}"

    tests = preprocessor.getTestCases(index)
    testcase_inputs, testcase_outputs = tests["input"], tests["output"]
    status, result, testcase_index, failed_output= tester.run_test_cases(command, testcase_inputs, testcase_outputs)

    db_writer.write(column_names, [status, mock_solution, testcase_index, failed_output, result.stderr, result.returncode])
    db_writer.flush()

    assert status == "PASSED", f"Status {status} does not match PASSED"
    assert result.returncode == 0, f"Returncode should be 0, not {result.returncode}"
    


def test_first_ten_with_OpenAI(): 
    test_results = []

    for i in range(10): 
      formated_public_tests = preprocessor.getFormatedPublicTests(i)
      problem_description = preprocessor.getProblemDescription(i)
      solution = llm_prompter.prompt(constants.SYSTEM_PROMPT, LLMTaskDescriptions.HASKELL_TASK_DESCRIPTION, 
                                     problem_description, formated_public_tests)
      solution_writer.write_solution(file_name, folder_name, pid, solution, ".hs")
      command, return_code, stderr = compiler.compile(pid, folder_name, file_name)

      if verbose: 
        print(f"\n===Test Case {i}===")
 
      if return_code == 0: 
        tests = preprocessor.getTestCases(i)
        testcase_inputs, testcase_outputs = tests["input"], tests["output"]
        status, result, testcase_index, failed_output = tester.run_test_cases(command, testcase_inputs, testcase_outputs)

        return_code = result.returncode
        stderr = result.stderr
        db_writer.write(column_names, [status, solution, testcase_index, failed_output, stderr, return_code])
        test_results.append(status)

      else: 
        status = constants.COMPILATION_ERROR_STRING
        db_writer.write(column_names, [status, solution, -1, "" , stderr, return_code])
        test_results.append(status)
         
      if verbose: 
        print(f"\nStatus:{status}")
        print(f"\nError:{stderr}")
        print(f"\nSolution:{solution}")
        print(f"\nReturn code:{return_code}")
      
      
    if verbose: 
      print(test_results)

    db_writer.flush()
    assert all(res == "PASSED" for res in test_results), f"Status does not match PASSED"

    

