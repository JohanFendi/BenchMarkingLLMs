from abc import ABC, abstractmethod


class LLMPrompter(ABC): 

    """
    Prompts an LLM and returns the answer as a string
    """

    @abstractmethod
    def prompt(description:str, problem_description:str, public_tests:str) -> str: 
        pass