from datasets import load_dataset
from typing import override

from ..DBreading.datasetReaders import DatasetReader
from .FeatureProcessing import mergeFeatures
from .PromptPreprocessor import PromptPreprocessor
from .TestPreprocessor import TestPreprocessor


class CodeContestsPreprocessor(TestPreprocessor, PromptPreprocessor): 

    def __init__(self): 
        self._dataset = load_dataset("deepmind/code_contests")
        self._dataset_reader = DatasetReader(self._dataset)


    @override
    def getTestCases(self, index:int) -> tuple[list[str], list[str]]:
        point = self._dataset_reader.getPoint(index)
        features = ["private_tests", "public_tests", "generated_tests"]
        keys = ["input", "output"]
        mergeFeatures(point, features, keys, "tests")
        return (point["tests"]["input"],point["tests"]["output"])


    @override
    def getProblemDescription(self, index:int) -> str:
        point = self._dataset_reader.getPoint(index, ["description"])
        return point["description"]


    @override
    def getFormatedPublicTests(self, index:int) -> str:
        input, output = self.getTestCases(index)

        if len(input) != len(output): 
            raise ValueError("Length of Input and output not equal")
        
        strings = []
        for i, (input_str, output_str) in enumerate(zip(input, output)): 
            strings.append(f"Public test {i+1}:\nInput:\n{input_str}\nOutput:\n{output_str}")

        return "\n".join(strings)

    @override
    def getDBSize(self) -> int:
        return self._dataset_reader.getDBSize()