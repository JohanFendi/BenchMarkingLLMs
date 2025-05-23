from abc import ABC, abstractmethod


class TestPreprocessor(ABC): 


    @abstractmethod
    def getTestCases(self, index:int) -> dict[str, list[str]]: 
        pass

    
    @abstractmethod
    def getDBSize(self) -> int:
        pass