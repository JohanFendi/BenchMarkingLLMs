from abc import ABC, abstractmethod


class SolutionWriter(ABC): 

    @abstractmethod
    def write_solution(self, file_name:str, folder_name:str, process_id:str, solution:str) -> None: 
        pass
