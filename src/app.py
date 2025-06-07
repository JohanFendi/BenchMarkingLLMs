from typing import Dict, Any
from os import getpid


from LLMPrompting.LLMPrompter import LLMPrompter
from Compilation.FolderCreator import FolderCreator
from DataManagment.DBreading.DBReader import DBReader
from DataManagment.DBwriting.DBWriter import DBWriter
from DataManagment.Preprocessing.PromptPreprocessor import PromptPreprocessor
from DataManagment.Preprocessing.TestPreprocessor import TestPreprocessor
from SolutionTesting.SolutionTester import SolutionTester


class App: 
    def __init__(self, test_preprocessor:TestPreprocessor, prompt_preprocessor:PromptPreprocessor,
                 llm_prompter:LLMPrompter, folder_creator:FolderCreator, db_writer:DBWriter, 
                 solution_tester:SolutionTester, start_index:int, end_index:int) -> None:
        
        if start_index > end_index: 
            raise IndexError("start_index must be less than or equal to end_index.")
        
        arguments_info = [
                        ("prompt_preprocessor",prompt_preprocessor,PromptPreprocessor), 
                        ("test_preprocessor",test_preprocessor,TestPreprocessor), 
                        ("llmPrompter", llm_prompter, LLMPrompter), 
                        ("folderCreator", folder_creator, FolderCreator), 
                        ("dbWriter", db_writer, DBWriter), 
                        ("solution_tester", solution_tester, SolutionTester)
                        ]
        
        for variable_name, argument, argument_type in arguments_info: 
            if not isinstance(argument, argument_type): 
                raise TypeError(f"{variable_name} must be of type {argument_type}, not {type(argument)}.")
            
        if not prompt_preprocessor is test_preprocessor: 
            raise ValueError("prompt_preprocessor must be same object as test_preprocessor")
            
        if end_index >= prompt_preprocessor.getDBSize() or end_index >= test_preprocessor.getDBSize(): 
            raise IndexError(f"end_index greater than or equal to database size.")
            
        self._prompt_preprocessor = prompt_preprocessor
        self._test_preprocessor = test_preprocessor
        self._llm_prompter = llm_prompter
        self._folder_creator = folder_creator
        self._db_writer = db_writer
        self._start_index = start_index
        self._end_index = end_index
        self._solution_tester = solution_tester

    def run(self):
        for i in range(self._start_index, self._end_index): 
        
            #Get data for prompt
            problem_description = self._prompt_preprocessor.getProblemDescription(i)
            formated_public_tests : str = self._prompt_preprocessor.getFormatedPublicTests(i)

            #Get Ai-generated solution, write it to file, create executable and get command to be able to execute code
            problem_solution : str = self._llm_prompter.prompt("", problem_description, formated_public_tests)
            process_id : str = str(getpid())
            command : str = self._folder_creator.create(problem_solution, process_id, "Haskell", "main" )

            #Run tests
            testcase_inputs, testcase_outputs = self._test_preprocessor.getTestCases(i)
            result : str = self._solution_tester.runTestCases(command, testcase_inputs, testcase_outputs)

            #Store results
            self._db_writer.write(problem_solution, result)
