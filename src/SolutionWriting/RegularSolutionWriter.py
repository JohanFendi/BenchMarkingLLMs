from pathlib import Path
from typing import override
from exceptions import DirectoryNotFoundError

from .SolutionWriter import SolutionWriter


class RegularSolutionWriter(SolutionWriter): 

    @override
    def write_solution(self, file_name:str, folder_name:str, process_id:str, solution:str, postfix:str) -> None: 
        #Check that folder folder_name/process_id exists
        if not Path(f"{folder_name}/{process_id}").exists(): 
            raise DirectoryNotFoundError(f"Directory {folder_name}/{process_id} not found.")

        #Remove postfix
        if postfix in file_name and file_name[len(file_name)-len(postfix):] == postfix: 
            file_name = file_name[:len(file_name)-3] 
            
        #Create file and write solution
        with open(f"{folder_name}/{process_id}/{file_name}{postfix}", "w") as haskell_file: 
            haskell_file.write(solution)