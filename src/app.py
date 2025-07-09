from typing import Dict, Any, TypeVar
from os import getpid
from time import time
from logging import Logger

from LLMPrompting.LLMPrompter import LLMPrompter
from Compilation.FolderCreator import FolderCreator
from DataManagment.DBreading.DBReader import DBReader
from DataManagment.DBwriting.DBWriter import DBWriter
from DataManagment.Preprocessing.PromptPreprocessor import PromptPreprocessor
from DataManagment.Preprocessing.TestPreprocessor import TestPreprocessor
from SolutionTesting.SolutionTester import SolutionTester
from Compilation.Compiler import Compiler
from SolutionWriting.SolutionWriter import SolutionWriter
import constants
from exceptions import *
from CustomLogging import get_logger
from EpochLogger import EpochLogger



class App: 

    def __init__(self, test_preprocessor:TestPreprocessor, prompt_preprocessor:PromptPreprocessor,
                 llm_prompter:LLMPrompter, folder_creator:FolderCreator, compiler:Compiler, 
                 solution_writer:SolutionWriter, db_writer:DBWriter, 
                 solution_tester:SolutionTester, start_index:int, end_index:int, epoch_size:int) -> None:
        
        if start_index > end_index: 
            raise IndexError("start_index must be less than or equal to end_index.")
            
        if end_index >= prompt_preprocessor.getDBSize() or end_index >= test_preprocessor.getDBSize(): 
            raise IndexError(f"end_index greater than or equal to database size.")
        
        for col in db_writer.get_column_names(): 
            if col not in constants.AVAILABLE_COLUMNS: 
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
        self._epoch_logger = EpochLogger(epoch_size)
        self._error_logger = get_logger(constants.LOGGER_FORMAT, 
                                        constants.ERROR_LOGGER_NAME, 
                                        constants.ERROR_LOG_FILE_NAME)


    def run(self, 
            llm_task_description:str, 
            folder_name:str, 
            file_name:str, 
            file_postfix:str) -> None:    
        
        #init folders and get data cols. 
        process_id = str(getpid())
        column_names = self._db_writer.get_column_names()
        self._folder_creator.init_folders(folder_name,[process_id])

        for i in range(self._start_index, self._end_index): 

            #Get solution, compile and test it. 
            try: 
              (status, problem_solution, failed_testcase_index, 
               failed_output, stderr, return_code, pipeline_time, 
               ai_time) = self._run_iteration(i, llm_task_description, 
                        file_name, folder_name, process_id, file_postfix)
              
            except DataLengthMismatchError as e: 
                self._error_logger.exception("DataLengthMismatchError")
                continue

            except DirectoryNotFoundError as e: 
                self._error_logger.exception("DirectoryNotFoundError")
                return 
            
            except FileNotFoundError as e: 
                self._error_logger.exception("FileNotFoundError")
                return 
            
            except WrongOSError as e: 
                self._error_logger.exception("WrongOSError")
                return
            
            except NestedTypeMismatchError as e: 
                self._error_logger.exception("NestedTypeMismatchError")
                continue

            #Store result. 
            try: 
                storage_time = self._store_result(status, problem_solution, failed_testcase_index, failed_output, stderr, return_code, column_names)

            except DataLengthMismatchError as e: 
                self._error_logger.exception("DataLengthMismatchError")
                continue

            except ColumnMismatchError as e: 
                self._error_logger.exception("ColumnMismatchError")
                return
            
            self._epoch_logger.update_epoch(ai_time, pipeline_time, storage_time, status)

        self._epoch_logger.log_epoch()    
        self._db_writer.flush()
        
  
    def _run_iteration(self, index:int, llm_task_description:str,
                    file_name:str, folder_name:str, 
                    process_id:int, file_postfix:str) -> tuple[str, str, int, str, str, str, int, float, float]: 
        pipeline_start_time = time()

        #Get inputs for model
        formated_public_tests = self._prompt_preprocessor.getFormatedPublicTests(index)
        problem_description = self._prompt_preprocessor.getProblemDescription(index)

        #Get and compile solution
        ai_start_time = time()
        problem_solution = self._llm_prompter.prompt(llm_task_description, problem_description, formated_public_tests)
        ai_end_time = time()
        self._solution_writer.write_solution(file_name, folder_name, process_id, problem_solution, file_postfix)
        command, return_code, stderr = self._compiler.compile(process_id, folder_name, file_name)

        failed_output = constants.NO_FAILED_TESTCASES_OUTPUT # actual output of failed test case
        failed_testcase_index = constants.NO_FAILED_TESTCASES_INDEX 
        status = "" #ERROR, PASSED, FAILED or COMPILATION_ERROR

        #Run solution
        if return_code == 0:       
            tests = self._test_preprocessor.getTestCases(index)
            testcase_inputs, testcase_outputs = tests["input"], tests["output"]
            status, result, failed_testcase_index, failed_output = self._solution_tester.run_test_cases(command, testcase_inputs, testcase_outputs)
            return_code = result.returncode
            stderr = result.stderr

        #The compilation resulted in an error
        else: 
            status = constants.COMPILATION_ERROR_STRING

        pipeline_end_time = time()
        ai_time = ai_end_time - ai_start_time
        pipeline_time = pipeline_end_time - pipeline_start_time - ai_time
        return status, problem_solution, failed_testcase_index, failed_output, stderr, return_code, pipeline_time, ai_time
    

    def _store_result(self, 
                     status:str, 
                     problem_solution:str, 
                     failed_testcase_index:int, 
                     failed_output:str, 
                     stderr:str, return_code:int, 
                     column_names:str) -> float: 
        
        start_time = time()
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
        end_time = time()
        return end_time - start_time
    



   

        




