from datasets import load_dataset, Dataset
from typing import override

from ..DBreading.DatasetReader import DatasetReader
from .FeatureProcessing import buildMergedFeature
from .PromptPreprocessor import PromptPreprocessor
from .TestPreprocessor import TestPreprocessor


class CodeContestsPreprocessor(TestPreprocessor, PromptPreprocessor): 

    #dataset should be a slice of the following datastructure: load_dataset("deepmind/code_contests")["train"], 
    #and thus with type Dataset. 
    def __init__(self, dataset:Dataset): 
        self._dataset_reader = DatasetReader(dataset)
        self._features = ["private_tests", "public_tests", "generated_tests", "description"]
        self._current_index = -1
        self._cached_point = None


    @override
    def getTestCases(self, index:int) -> dict[str, list[str]]:
        self._checkIndexValid(index)
        self._cache_Point(index)
        features = ["private_tests", "public_tests", "generated_tests"]
        keys = ["input", "output"]
        tests_dict = buildMergedFeature(self._cached_point, features, keys)
        return tests_dict


    @override
    def getProblemDescription(self, index:int) -> str:
        self._checkIndexValid(index)
        self._cache_Point(index)
        return self._cached_point["description"]


    @override
    def getFormatedPublicTests(self, index:int) -> str:
        self._checkIndexValid(index)
        self._cache_Point(index)
        public_tests = self._cached_point["public_tests"]
        test_input, test_output = public_tests["input"], public_tests["output"]

        if len(test_input) != len(test_output): 
            raise ValueError(f"Mismatched lengths: input has {len(input)}, output has {len(test_output)}.")
        
        strings = []
        for i, (input_str, output_str) in enumerate(zip(input, test_output)): 
            strings.append(f"Public test {i+1}:\nInput:\n{input_str}\nOutput:\n{output_str}")

        return "\n".join(strings)


    @override
    def getDBSize(self) -> int:
        return self._dataset_reader.getDBSize()
    

    def _checkIndexValid(self, index:int):
        datasetSize = self._dataset_reader.getDBSize()
        if index < 0 or index > datasetSize - 1:
            raise IndexError(f"""Index of {index} invalid. Index has to be a non negaitve
                                 integer smaller than database size of {datasetSize}-1.""") 
    
    
    def _cache_Point(self, index:int): 
        if index != self._current_index: 
            self._current_index = index
            self._cached_point = self._dataset_reader.getPoint(self._current_index, self._features)