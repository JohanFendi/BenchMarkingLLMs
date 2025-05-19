from abc import ABC, abstractmethod
from typing import Dict, Any

class DBReader(ABC): 
    

    @abstractmethod
    def getPoint(self, index:int,  keys : list[str]) -> Dict[str, Any]: 

        """
        Retrieve specified key-value pairs from a data point at the given index.
        """
        
        pass


    @abstractmethod
    def getDBSize(self) -> int: 
        pass