from typing import Dict, Any, TypeVar
from os import getpid
from time import time
from logging import Logger
from dataclasses import dataclass


from src.DataManagment.Preprocessing.FeatureProcessing import remove_code_fence
from src.LLMPrompting.LLMPrompter import LLMPrompter
from src.Compilation.FolderCreator import FolderCreator
from src.DataManagment.DBreading.DBReader import DBReader
from src.DataManagment.DBwriting.DBWriter import DBWriter
from src.DataManagment.Preprocessing.PromptPreprocessor import PromptPreprocessor
from src.DataManagment.Preprocessing.TestPreprocessor import TestPreprocessor
from src.SolutionTesting.SolutionTester import SolutionTester
from src.Compilation.Compiler import Compiler
from src.SolutionWriting.SolutionWriter import SolutionWriter
import constants
from src.Exceptions import *
from src.Logging.EpochLogger import EpochLogger


@dataclass
class IterationData: 
    status: str
    problem_solution: str 
    failed_output:str
    stderr:str 
    failed_testcase_index:int 
    return_code:int 
    pipeline_time:float 
    ai_time:float



class App: 

    def __init__(self, test_preprocessor:TestPreprocessor, 
                 prompt_preprocessor:PromptPreprocessor,
                 llm_prompter:LLMPrompter, 
                 folder_creator:FolderCreator, 
                 compiler:Compiler, 
                 solution_writer:SolutionWriter, 
                 db_writer:DBWriter, 
                 solution_tester:SolutionTester, 
                 epoch_logger:EpochLogger,
                 error_logger:Logger, 
                 start_index:int, 
                 end_index:int) -> None:
        
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
        self._epoch_logger = epoch_logger
        self._error_logger = error_logger


    def run(self, 
            llm_system_prompt:str, 
            llm_task_description:str, 
            programming_language:str, 
            file_name:str, 
            file_postfix:str) -> None:    
        
        #init folders and get data cols. 
        process_id = str(getpid())
        self._folder_creator.init_folders(programming_language,[process_id])

        for i in range(self._start_index, self._end_index): 
            #Get solution, compile and test it. 
            try: 
              data = self._run_iteration(llm_system_prompt, i, llm_task_description, 
                        file_name, programming_language, process_id, file_postfix)
              
            except DataLengthMismatchError as e: 
                self._error_logger.exception(f"Iteration {i} of App.run(): DataLengthMismatchError:")
                continue

            except CodeFenceNotFoundError as e: 
                self._error_logger.exception(f"Iteration {i} of App.run(): CodeFenceNotFoundError:")
                continue

            except DirectoryNotFoundError as e: 
                self._error_logger.exception(f"Iteration {i} of App.run(): DirectoryNotFoundError: ")
                return 
            
            except FileNotFoundError as e: 
                self._error_logger.exception(f"Iteration {i} of App.run(): FileNotFoundError:")
                return 
            
            except WrongOSError as e: 
                self._error_logger.exception(f"Iteration {i} of App.run():WrongOSError:")
                return
            
            except NestedTypeMismatchError as e: 
                self._error_logger.exception(f"Iteration {i} of App.run(): NestedTypeMismatchError:")
                continue

            #Store result. 
            try: 
                storage_time = self._store_result(data)

            except DataLengthMismatchError as e: 
                self._error_logger.exception(f"Iteration {i} of App.run(): DataLengthMismatchError:")
                continue

            except ColumnMismatchError as e: 
                self._error_logger.exception(f"Iteration {i} of App.run(): ColumnMismatchError:")
                return
            
            self._epoch_logger.update_epoch(data.ai_time, data.pipeline_time, storage_time, data.status)
        
        #Clear system of remaning data
        self._epoch_logger.log_epoch()    
        self._db_writer.flush()


    def _prompt_llm(self, index: int, llm_system_prompt:str, llm_task_description:str) -> tuple[float, float, str]: 
        pipeline_start_time = time()

        #Get inputs for model
        formated_public_tests = self._prompt_preprocessor.getFormatedPublicTests(index)
        problem_description = self._prompt_preprocessor.getProblemDescription(index)

        #Get and compile solution
        ai_start_time = time()
        problem_solution = self._llm_prompter.prompt(llm_system_prompt, llm_task_description, problem_description, formated_public_tests)
        ai_end_time = time()

        ai_time = ai_end_time - ai_start_time

        return ai_time, pipeline_start_time, problem_solution, 
        
  
    def _run_iteration(self, 
                       llm_system_prompt:str, 
                       index:int, 
                       llm_task_description:str,
                       file_name:str, 
                       programming_language:str, 
                       process_id:int, 
                       file_postfix:str) -> IterationData: 
        
        ai_time, pipeline_start_time, problem_solution = self._prompt_llm(index, llm_system_prompt, llm_task_description)
        
        problem_solution = remove_code_fence(programming_language, problem_solution)
        self._solution_writer.write_solution(file_name, programming_language, process_id, problem_solution, file_postfix)
        command, return_code, stderr = self._compiler.compile(process_id, programming_language, file_name)

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
        return IterationData(status, problem_solution, failed_output, stderr, failed_testcase_index, return_code, pipeline_time, ai_time)
    

    def _store_result(self, data:IterationData) -> float: 
        column_names = self._db_writer.get_column_names()
        start_time = time()
        col_map = {
                "status":                    data.status,
                "problem_solution":          data.problem_solution,
                "failed_testcase_index":     data.failed_testcase_index,
                "failed_output":             data.failed_output,
                "stderr":                    data.stderr,
                "return_code":               data.return_code
            }
        
        row = [col_map[col] for col in column_names]
        self._db_writer.write(column_names, row)
        end_time = time()
        return end_time - start_time
    



   

        




