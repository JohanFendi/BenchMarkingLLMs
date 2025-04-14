from abc import ABC, abstractmethod

class CodeRunner(ABC): 

    """
    Runs the AI generated solution and returns three possible values: 
    "PASSED", "FAILED", "ERROR".
    """

    @abstractmethod
    def run(solution:str) -> str: 
        pass

    