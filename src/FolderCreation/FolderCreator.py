from abc import ABC, abstractmethod

class FolderCreator(ABC): 

    

    @abstractmethod
    def create(self, solution:str, process_name:str, folder_name:str, file_name:str) -> str: 
        """
        Creates an folder with the name process_name, 
        in which it places an executable, along with other files. 
        Returns the command to execute the executable. 
        Should be called from main file. 
        """
        pass

    