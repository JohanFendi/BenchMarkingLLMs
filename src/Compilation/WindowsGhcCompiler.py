from typing import override
from pathlib import Path
from subprocess import run
from sys import platform

from .Compiler import Compiler
from src.Exceptions import WrongOSError


class WindowsGhcCompiler(Compiler): 

    HIDDEN_PACKAGES = ["array", "containers"]

    @override
    def compile(self, process_id:str, folder_name:str, file_name:str) -> tuple[str, int, str]: 
        """
        Compiles file and returns executable command.
        """ 
        #Remove postfix
        if ".hs" in file_name and file_name[len(file_name)-3:] == ".hs": 
            file_name = file_name[:len(file_name)-3] 
       
        #Check flie exists
        file_path = f"{folder_name}/{process_id}/{file_name}"
        if not Path(f"{file_path}.hs").exists(): 
            raise FileNotFoundError(f"File {file_path}.hs not found.")
        
        #Check OS is Windows
        if not platform.startswith("win"): 
            raise WrongOSError(f"WindowsGhcCompiler does not support following OS:{platform}")

        #Compile file, this part varies a lot from os to os
        command = self._get_command(file_path)
        result = run(command, capture_output=True, text=True)

        return (f"{file_path}.exe", result.returncode, result.stderr) # .exe is windows specific


    def _get_command(self, file_path:str) -> list[str]: 
        command = ["ghc"]

        #Expose hidden packages
        for pkg in WindowsGhcCompiler.HIDDEN_PACKAGES: 
            command.append("-package")
            command.append(pkg)

        command.append(file_path)

        return command

        