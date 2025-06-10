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

    def run(self, llm_task_description:str, folder_name:str, file_name:str) -> None:
        process_id = str(getpid())
        self._folder_creator.init_folders(folder_name,[process_id])
       
        for i in range(self._start_index, self._end_index): 
        
            #Get data for prompt
            problem_description = self._prompt_preprocessor.getProblemDescription(i)
            formated_public_tests = self._prompt_preprocessor.getFormatedPublicTests(i)

            #Get Ai-generated solution write it to file, create executable and get command to be able to execute code
            problem_solution = self._llm_prompter.prompt(llm_task_description, problem_description, formated_public_tests)
            self._solution_writer.write_solution(file_name, folder_name, process_id, problem_solution)
            command = self._compiler.compile(process_id, folder_name, file_name)         

            #Run tests
            testcase_inputs, testcase_outputs = self._test_preprocessor.getTestCases(i)
            (status, returncode, testcase_index) = self._solution_tester.run_test_cases(command, testcase_inputs, testcase_outputs)

            #Store results
            self._db_writer.write(problem_solution, status)
