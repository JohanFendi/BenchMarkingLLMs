from pathlib import Path
from typing import override

from Compilation.FolderCreator import FolderCreator


class ProcessFolderCreator(FolderCreator): 

    #Creates a folder for each process id inside a parent folder named folder_name
    #Meant to be used only once to set up folders. 
    @override 
    def setUpFolders(self, folder_name:str, process_ids:list[str]) -> None: 
        if not Path(folder_name).exists(): 
            Path(folder_name).mkdir()

        for process_id in process_ids: 
            if not Path(f"{folder_name}/{process_id}").exists(): 
                Path(f"{folder_name}/{process_id}").mkdir()