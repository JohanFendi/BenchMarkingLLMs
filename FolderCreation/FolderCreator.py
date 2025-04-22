from abc import ABC, abstractmethod

class FolderCreator(ABC): 

    """
    Creates an folder with the name process_name, 
    in which it places an executable, along with other files. 
    Returns the path to the executable. 
    """

    @abstractmethod
    def create(solution:str, process_name:str) -> str: 
        pass

    