from LLMPrompting.LLMPrompter import LLMPrompter
from FolderCreation.FolderCreator import FolderCreator
from DataManagment.DBreading.DBReader import DBReader
from DataManagment.DBwriting.DBWriter import DBWriter
from DataManagment.Preprocessing.FeatureProcessing import mergeFeatures, formatPublicTests
from SolutionTesting.SolutionTester import SolutionTester

from typing import Dict, Any
from os import getpid


class App: 
    def __init__(self, db_reader:DBReader, llm_prompter:LLMPrompter, folder_creator:FolderCreator, db_writer:DBWriter, solution_tester:SolutionTester, start_index:int, end_index:int) -> None:
        if start_index > end_index: 
            raise IndexError("start_index must be less than or equal to end_index.")
        
        arguments_info = [
                        ("dbReader",db_reader, DBReader), 
                        ("llmPrompter", llm_prompter, LLMPrompter), 
                        ("folderCreator", folder_creator, FolderCreator), 
                        ("dbWriter", db_reader, DBReader), 
                        ("solution_tester", solution_tester, SolutionTester)
                        ]
        
        for variable_name, argument, argument_type in arguments_info: 
            if not isinstance(argument, argument_type): 
                raise TypeError(f"{variable_name} must be of type {argument_type}, not {type(argument)}.")
            
        if end_index >= db_reader.getDBSize(): 
            raise IndexError(f"end_index greater than or equal to database size.")
            
        self._db_reader = db_reader
        self._llm_prompter = llm_prompter
        self._folder_creator = folder_creator
        self._db_writer = db_writer
        self._start_index = start_index
        self._end_index = end_index
        self._solution_tester = solution_tester

    def run(self):
        for i in range(self._start_index, self._end_index): 
            data_point : Dict[str, Any] = self._db_reader.getPoint(i) 
            problem_description = data_point["description"]
            formated_public_tests : str = formatPublicTests(data_point["input"], data_point["output"])

            #Merge public_tests, private_tests, generated_tests into tests 
            #important that this happens after the formating of the public tests
            features_to_merge = ["public_tests", "private_tests", "generated_tests"]
            keys_of_features =  ["input", "output"]
            new_feature_name = "tests"
            mergeFeatures(data_point, features_to_merge , keys_of_features, new_feature_name)

            #Get Ai-generated solution, write it to file, create executable and get command to be able to execute code
            problem_solution : str = self._llm_prompter.prompt("", problem_description, formated_public_tests)
            process_id : str = str(getpid())
            command : str = self._folder_creator.create(problem_solution, process_id, "Haskell", "main" )

            result : str = self._solution_tester.runTestCases(command, data_point["tests"]["input"], data_point["tests"]["output"])
            self._db_writer.write(problem_solution, result)
