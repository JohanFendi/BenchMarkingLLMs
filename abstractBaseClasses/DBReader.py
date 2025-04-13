from abc import ABC, abstractmethod
from typing import Dict, Any

class DBReader(ABC): 

    """
    Retrieve specified key-value pairs from a data point at the given index.
    """
    @abstractmethod
    def getPoint(index:int,  keys : list[str]) -> Dict[str, Any]: 
        pass