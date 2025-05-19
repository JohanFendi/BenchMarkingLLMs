from abc import ABC, abstractmethod


class PromptPreprocessor(ABC): 


    @abstractmethod
    def getFormatedPublicTests(self, index:int) -> str:
        pass


    @abstractmethod
    def getProblemDescription(self, index:int) -> str: 
        pass


    @abstractmethod
    def getDBSize(self) -> int:
        pass
