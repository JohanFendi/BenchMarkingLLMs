from abc import ABC, abstractmethod
from subprocess import run


class SolutionTester(ABC):


    """
    Returns the resulting datapoint as a list of strings.
    """

    @abstractmethod
    def runTestCases(self, command:str, input_strings : list[str], output_strings : list[str]) -> str: 
        pass