from typing import override
from pathlib import Path
from subprocess import run
from sys import platform

from ..Compilation.Compiler import Compiler


class WindowsGhcCompiler(Compiler): 


    @override
    def compile(self, process_id:str, folder_name:str, file_name:str) -> str: 
        """
        Compiles file and returns executable command.
        """ 
       
        #Check flie exists
        file_path = f"{folder_name}/{process_id}/{file_name}.hs"
        if not Path(file_path).exists(): 
            raise FileNotFoundError(f"File {file_path} not found.")
        
        #Check OS is Windows
        if not platform.startswith("win"): 
            raise NotImplementedError(f"WindowsGhcCompiler does not support following OS:{platform}")

        #Compile file, this part varies a lot from os to os
        
        command = ["ghc", file_path]
        result = run(command, capture_output=True, text=True)
        
        #Raise error 
        if result.returncode != 0: 
            raise RuntimeError(f"ghc compile failed:\n{result.stderr}")

        return f"{file_path}.exe" # .exe is windows specific
