from time import time


from src.DataManagment.Preprocessing.FeatureProcessing import remove_code_fence
from src.LLMPrompting.LLMPrompter import LLMPrompter
from src.Compilation.FolderCreator import FolderCreator
from src.DataManagment.DBwriting.DBWriter import DBWriter
from src.DataManagment.Preprocessing.PromptPreprocessor import PromptPreprocessor
from src.DataManagment.Preprocessing.TestPreprocessor import TestPreprocessor
from src.SolutionTesting.SolutionTester import SolutionTester
from src.Compilation.Compiler import Compiler
from src.SolutionWriting.SolutionWriter import SolutionWriter
from src.Types import IterationData, LLMPromptData
import constants


class Pipeline(): 

    def __init__(self, 
                 test_preprocessor:TestPreprocessor, 
                 prompt_preprocessor:PromptPreprocessor,
                 llm_prompter:LLMPrompter, 
                 folder_creator:FolderCreator, 
                 compiler:Compiler, 
                 solution_writer:SolutionWriter, 
                 db_writer:DBWriter, 
                 solution_tester:SolutionTester, 
                 programming_language:str, 
                 process_ids:list[str], 
                 file_name:str, 
                 file_postfix:str) -> None: 
        
        if prompt_preprocessor.getDBSize() != test_preprocessor.getDBSize(): 
            raise ValueError(f"""Prompt preprocessors database size: {prompt_preprocessor.getDBSize()}, not equal to 
                             test preprocessor database  size: {test_preprocessor}.""")
        
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
        self._solution_tester = solution_tester
        self._max_index = self._prompt_preprocessor.getDBSize()-1
        self._folder_creator.init_folders(programming_language, process_ids)

        #Directory/file variables
        self._file_name = file_name
        self._file_postfix = file_postfix
        self._programming_language = programming_language


    async def prompt_llm(self, 
                        index:int, 
                        llm_system_prompt:str, 
                        llm_task_description:str) -> LLMPromptData: 
        
        if not (0 <= index < self._max_index): 
            raise IndexError(f"end_index has to be between 0 and {self._max_index}, not {index}.")
        
        #Get inputs for model
        formated_public_tests = self._prompt_preprocessor.getFormatedPublicTests(index)
        problem_description = self._prompt_preprocessor.getProblemDescription(index)

        #Get and compile solution
        print(f"Prompting AI with datapoint with index {index}")
        ai_start_time = time()
        problem_solution = await self._llm_prompter.prompt(llm_system_prompt, llm_task_description, problem_description, formated_public_tests)
        ai_end_time = time()
        print(f"Finished Prompting AI with datapoint with index {index}")

        ai_time = ai_end_time - ai_start_time
        return LLMPromptData(ai_time, problem_solution, index)
        
  
    def run_evaluation(self, 
                       llm_prompt_data:LLMPromptData, 
                       process_id:str) -> tuple[float, float, str]:  #Eval time, Db write time, status
        
        print(f"Starting eval pipeline for index {llm_prompt_data.index}")
        eval_start_time = time()

        problem_solution = remove_code_fence(self._programming_language, llm_prompt_data.problem_solution)
        self._solution_writer.write_solution(self._file_name, self._programming_language, process_id, 
                                             problem_solution, self._file_postfix)
        command, return_code, stderr = self._compiler.compile(process_id, self._programming_language, self._file_name)

        failed_output = constants.NO_FAILED_TESTCASES_OUTPUT # actual output of failed test case
        failed_testcase_index = constants.NO_FAILED_TESTCASES_INDEX 
        status = "" #ERROR, PASSED, FAILED or COMPILATION_ERROR

        #Run solution
        if return_code == 0:       
            tests = self._test_preprocessor.getTestCases(llm_prompt_data.index)
            testcase_inputs, testcase_outputs = tests["input"], tests["output"]
            status, result, failed_testcase_index, failed_output = self._solution_tester.run_test_cases(command, testcase_inputs, testcase_outputs)
            return_code = result.returncode
            stderr = result.stderr

        #The compilation resulted in an error
        else: 
            status = constants.COMPILATION_ERROR_STRING

        eval_end_time = time()    
        eval_time = eval_end_time - eval_start_time

        #Store result
        iteration_data = IterationData(problem_solution, status, failed_output, stderr, failed_testcase_index, return_code)
        db_write_time = self._store_result(iteration_data)

        print(f"Finished eval pipeline for index {llm_prompt_data.index}")
        return eval_time, db_write_time, status
    

    def _store_result(self, 
                     data:IterationData) -> float: 
        
        start_time = time()
        data = {
                "status":                    data.status,
                "problem_solution":          data.problem_solution,
                "failed_testcase_index":     data.failed_testcase_index,
                "failed_output":             data.failed_output,
                "stderr":                    data.stderr,
                "return_code":               data.return_code
            }
        self._db_writer.write(data)
        end_time = time()
        write_time = end_time - start_time
        return write_time
    

    def clean_up(self) -> None: 
        self._db_writer.flush()