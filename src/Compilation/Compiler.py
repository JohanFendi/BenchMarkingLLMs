from abc import abstractmethod, ABC
from pathlib import Path

class Compiler(): 
    
    
    @abstractmethod
    def compile(self,process_name:str, folder_name:str, file_name:str) -> str: 
        """
        Returns the command to create the executable. 
        """
        pass

    
    