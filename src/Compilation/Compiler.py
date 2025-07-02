from abc import abstractmethod, ABC
from pathlib import Path

class Compiler(): 
    
    
    @abstractmethod
    def compile(self,process_name:str, folder_name:str, file_name:str) -> tuple[str, int, str]: 
        """
        Returns the command, returncode, and stderr to create the executable. 
        """
        pass

    
    