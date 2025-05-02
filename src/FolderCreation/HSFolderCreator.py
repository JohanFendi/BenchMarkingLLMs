from pathlib import Path
from FolderCreation.FolderCreator import FolderCreator
from subprocess import run

class HSFolderCreator(FolderCreator): 

    # file_name must not have postfix. 
    def create(self, solution:str, process_id:str, folder_name:str, file_name:str) -> str: 
        if not Path(folder_name).exists(): 
            Path(folder_name).mkdir()

        if not Path(f"{folder_name}/{process_id}").exists(): 
            Path(f"{folder_name}/{process_id}").mkdir()

        if ".hs" in file_name.strip(): 
            file_name = file_name.strip()[0:-3]

        with open(f"{folder_name}/{process_id}/{file_name}.hs", "w") as haskell_file: 
            haskell_file.write(solution)

        file_path = f"{folder_name}/{process_id}/{file_name}.hs"
        run(["ghc", file_path])

        return f"{folder_name}/{process_id}/{file_name}.exe"