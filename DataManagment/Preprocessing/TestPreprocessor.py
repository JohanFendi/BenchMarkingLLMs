from abc import ABC, abstractmethod


class TestPreprocessor(ABC): 

    @abstractmethod
    def getTestCases(self, index:int) -> tuple[list[str], list[str]]: 
        pass

    
    