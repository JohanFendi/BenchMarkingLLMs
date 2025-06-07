from abc import ABC, abstractmethod


class FolderCreator(ABC): 
    

    @abstractmethod
    def setUpFolders(self, folder_name:str, process_ids:list[str]) -> None: 
        """
        Takes in a folder name and a list of process ids. 
        Creates folder named folder_name and creates subfolders
        with names of the process_ids.  
        """
        pass        

    