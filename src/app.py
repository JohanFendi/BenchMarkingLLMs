from typing import Dict, Any
from os import getpid

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


    #Relative order of column names should be: status, problem_solution, failed_testcase_index, failed_output, stderr, return_code. 
    def run(self, llm_task_description:str, 
            folder_name:str, file_name:str, 
            file_postfix:str, column_names:list[str]) -> None:
        
        valid, error_msg = self.valid_columns(column_names)
        if not valid: 
            raise RuntimeError(error_msg)        
        
        process_id = str(getpid())
        self._folder_creator.init_folders(folder_name,[process_id])
   
        for i in range(self._start_index, self._end_index): 
        
            #Get data for prompt
            problem_description = self._prompt_preprocessor.getProblemDescription(i)
            formated_public_tests = self._prompt_preprocessor.getFormatedPublicTests(i)

            #Get Ai-generated solution, write it to file and compile.
            problem_solution = self._llm_prompter.prompt(llm_task_description, problem_description, formated_public_tests)
            self._solution_writer.write_solution(file_name, folder_name, process_id, problem_solution, file_postfix)
            command, return_code, stderr = self._compiler.compile(process_id, folder_name, file_name)
            
            failed_output = "" # actual output of failed test case
            failed_testcase_index = NO_FAILED_TESTCASES_INDEX 
            status = NO_FAILED_TESTCASES_OUTPUT #ERROR, PASSED, FAILED or COMPILATION_ERROR

            #Run tests
            if return_code == 0:                 
                tests = self._test_preprocessor.getTestCases(i)
                testcase_inputs, testcase_outputs = tests["input"], tests["output"]
                status, result, failed_testcase_index, failed_output = self._solution_tester.run_test_cases(command, testcase_inputs, testcase_outputs)
                return_code = result.returncode
                stderr = result.stderr
            
            #The compilation resulted in an error
            else: 
                status = COMPILATION_ERROR_STRING

            #Store results
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

        self._db_writer.flush()

  
    #Valid or not and eventuall error msg. 
    def valid_columns(self, column_names:list[str])->tuple[bool, str]: 
        if column_names != self._db_writer.get_column_names(): 
            return (False, f"column_names not equal to column names of data base writer.")
        
        for col in column_names: 
            if col not in AVAILABLE_COLUMNS: 
                return (False, f"Feature {col} cannot be stored. Available features are: {AVAILABLE_COLUMNS}")
            
        return (True, "")
            
