from abc import ABC, abstractmethod

class DBWriter(ABC): 

    @abstractmethod
    def write(solution:str, result:str) -> None: 
        pass