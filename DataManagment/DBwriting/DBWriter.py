from abc import ABC, abstractmethod

class DBWriter(ABC): 

    @abstractmethod
    def write(Point : list[str]) -> None: 
        pass