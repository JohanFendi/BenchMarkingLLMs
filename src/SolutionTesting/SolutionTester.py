from abc import ABC, abstractmethod
from subprocess import run


class SolutionTester(ABC):

    @abstractmethod
    def run_test_cases(self, command:str, input_strings:list[str], output_strings:list[str]) -> str: 
        """
        Returns the resulting datapoint as a list of strings.
        """
        pass