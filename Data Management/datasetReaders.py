import DBReader
from datasets import DatasetDict
from typing import Dict, Any


class DatasetReader(DBReader):

    def __init__(self, dataset : DatasetDict) -> None: 
        self._dataset = dataset
      
    def getPoint(self, index:int, keys: list[str])->Dict[str, Any]: 
        if index >= len(self._dataset): 
            raise IndexError(f"IndexError: Index {index} out of range for dataset with length {len(self._dataset)}.")
        
        point = self._dataset[index]
        result = {key:point[key] for key in keys}
        return result        
        



        
    
    
