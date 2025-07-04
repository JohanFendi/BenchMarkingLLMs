from typing import Dict, Any
from os import getpid
from time import time

from LLMPrompting.LLMPrompter import LLMPrompter
from Compilation.FolderCreator import FolderCreator
from DataManagment.DBreading.DBReader import DBReader
from DataManagment.DBwriting.DBWriter import DBWriter
from DataManagment.Preprocessing.PromptPreprocessor import PromptPreprocessor
from DataManagment.Preprocessing.TestPreprocessor import TestPreprocessor
from SolutionTesting.SolutionTester import SolutionTester
from Compilation.Compiler import Compiler
from SolutionWriting.SolutionWriter import SolutionWriter
from constants import COMPILATION_ERROR_STRING, NO_FAILED_TESTCASES_INDEX, NO_FAILED_TESTCASES_OUTPUT, AVAILABLE_COLUMNS


class App: 

    def __init__(self, test_preprocessor:TestPreprocessor, prompt_preprocessor:PromptPreprocessor,
                 llm_prompter:LLMPrompter, folder_creator:FolderCreator, compiler:Compiler, 
                 solution_writer:SolutionWriter, db_writer:DBWriter, 
                 solution_tester:SolutionTester, start_index:int, end_index:int) -> None:
        
        if start_index > end_index: 
            raise IndexError("start_index must be less than or equal to end_index.")
            
        if end_index >= prompt_preprocessor.getDBSize() or end_index >= test_preprocessor.getDBSize(): 
            raise IndexError(f"end_index greater than or equal to database size.")
        
        for col in db_writer.get_column_names(): 
            if col not in AVAILABLE_COLUMNS: 
                raise ValueError(f"Column {col} not available.")

            
        self._prompt_preprocessor = prompt_preprocessor
        self._test_preprocessor = test_preprocessor
        self._llm_prompter = llm_prompter
        self._folder_creator = folder_creator
        self._compiler = compiler
        self._solution_writer = solution_writer
        self._db_writer = db_writer
        self._start_index = start_index
        self._end_index = end_index
        self._solution_tester = solution_tester


    def run(self, llm_task_description:str, 
            folder_name:str, file_name:str, 
            file_postfix:str, epoch_size:int) -> None:
        
        column_names = self._db_writer.get_column_names()
        process_id = str(getpid())
        self._folder_creator.init_folders(folder_name,[process_id])

        for i in range(self._start_index, self._end_index): 

            #Get data for prompt
            try: 
              status, problem_solution, failed_testcase_index, failed_output, stderr, return_code = self.run_iteration(i, llm_task_description, 
                                                                                            file_name, folder_name, process_id, file_postfix)
            except ValueError as e: 
                print("Failed") #TODO Logging
                continue

            ################# REED FLAAGG
            except FileNotFoundError as e: 
                print("Failed") #TODO logging
                return #File does not exist, so all other cases will also fail
            
            except FileNotFoundError as e: 
                print("Failed") #TODO logging
                return #File does not exist, so all other cases will also fail
            
            ###################################
            
            except OSError as e: 
                print("Failed") #TODO logging
                return #Wrong OS
            
            except TypeError as e: 
                print("Wrong structure of datapoint") #TODO logging
    
            try: 
                self.store_result(status, problem_solution, failed_testcase_index, failed_output, stderr, return_code, column_names)
            except ValueError as e: 
                print("Failed") #TODO logging
                return #Because columns are bad
        
        self._db_writer.flush()
        
  
        
    def run_iteration(self, index:int, llm_task_description:str,
                    file_name:str, folder_name:str, 
                    process_id:int, file_postfix:str): 
        
        formated_public_tests = self._prompt_preprocessor.getFormatedPublicTests(index)
        problem_description = self._prompt_preprocessor.getProblemDescription(index)

        problem_solution = self._llm_prompter.prompt(llm_task_description, problem_description, formated_public_tests)
        self._solution_writer.write_solution(file_name, folder_name, process_id, problem_solution, file_postfix)
        command, return_code, stderr = self._compiler.compile(process_id, folder_name, file_name)

        failed_output = NO_FAILED_TESTCASES_OUTPUT # actual output of failed test case
        failed_testcase_index = NO_FAILED_TESTCASES_INDEX 
        status = "" #ERROR, PASSED, FAILED or COMPILATION_ERROR

        if return_code == 0:       
            tests = self._test_preprocessor.getTestCases(i)
            testcase_inputs, testcase_outputs = tests["input"], tests["output"]
            status, result, failed_testcase_index, failed_output = self._solution_tester.run_test_cases(command, testcase_inputs, testcase_outputs)
            return_code = result.returncode
            stderr = result.stderr

        #The compilation resulted in an error
        else: 
            status = COMPILATION_ERROR_STRING

        return status, problem_solution, failed_testcase_index, failed_output, stderr, return_code
    

    def store_result(self, status:str, problem_solution:str, 
                     failed_testcase_index:int, failed_output:str, 
                     stderr:str, return_code:int, column_names:str) -> None: 
        
        col_map = {
                "status":                    status,
                "problem_solution":          problem_solution,
                "failed_testcase_index":     failed_testcase_index,
                "failed_output":             failed_output,
                "stderr":                    stderr,
                "return_code":               return_code
            }
        
        row = [col_map[col] for col in column_names]
        self._db_writer.write(column_names, row)

