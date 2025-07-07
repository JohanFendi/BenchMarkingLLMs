from datasets import Dataset
from typing import override

from ..DBreading.DatasetReader import DatasetReader
from .FeatureProcessing import buildMergedFeature
from .PromptPreprocessor import PromptPreprocessor
from .TestPreprocessor import TestPreprocessor
from exceptions import DataLengthMismatchError


class CodeContestsPreprocessor(TestPreprocessor, PromptPreprocessor): 

    #dataset should be a slice of the following datastructure: 
    #load_dataset("deepmind/code_contests")["train"] and thus of type Dataset. 
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
        tests_dict = buildMergedFeature(self._cached_point, features, keys) #Throws NestedTypeMIsmatchError
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
        test_inputs, expected_outputs = public_tests["input"], public_tests["output"]

        if len(test_inputs) != len(expected_outputs): 
            raise DataLengthMismatchError(f"Mismatched lengths: public test {index} input has {len(test_inputs)}, output has {len(expected_outputs)}.")
        
        strings = []
        for i, (input_str, output_str) in enumerate(zip(test_inputs, expected_outputs)): 
            strings.append(f"Public test {i+1}:\nInput:\n{input_str}\nExpected Output:\n{output_str}")

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