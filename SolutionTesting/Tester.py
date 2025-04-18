from abc import ABC, abstractmethod
from typing import Any, TypeVar

T = TypeVar("T")

class Tester(ABC):


    """
    Returns the resulting datapoint as a list of strings.
    """

    @abstractmethod
    def runTestCases(executable:str, input : list[T], output : list[T]) -> list[str]: 
        pass