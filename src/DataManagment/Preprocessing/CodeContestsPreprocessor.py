from datasets import load_dataset
from typing import override

from ..DBreading.DatasetReader import DatasetReader
from .FeatureProcessing import buildMergedFeature
from .PromptPreprocessor import PromptPreprocessor
from .TestPreprocessor import TestPreprocessor


class CodeContestsPreprocessor(TestPreprocessor, PromptPreprocessor): 

    #dataset should be a slice of the following datastructure: load_dataset("deepmind/code_contests")["train"]
    def __init__(self, dataset): 
        self._dataset_reader = DatasetReader(dataset)


    @override
    def getTestCases(self, index:int) -> dict[str, list[str]]:
        features = ["private_tests", "public_tests", "generated_tests"]
        keys = ["input", "output"]
        point = self._dataset_reader.getPoint(index, features)
        tests_dict = buildMergedFeature(point, features, keys, "tests")
        return tests_dict


    @override
    def getProblemDescription(self, index:int) -> str:
        point = self._dataset_reader.getPoint(index, ["description"])
        return point["description"]


    @override
    def getFormatedPublicTests(self, index:int) -> str:
        point = self._dataset_reader.getPoint(index, ["public_tests"])
        input, output = point["input"], point["output"]

        if len(input) != len(output): 
            raise ValueError("Length of Input and output not equal")
        
        strings = []
        for i, (input_str, output_str) in enumerate(zip(input, output)): 
            strings.append(f"Public test {i+1}:\nInput:\n{input_str}\nOutput:\n{output_str}")

        return "\n".join(strings)


    @override
    def getDBSize(self) -> int:
        return self._dataset_reader.getDBSize()