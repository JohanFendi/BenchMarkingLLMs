from abc import ABC, abstractmethod


class LLMPrompter(ABC): 

    """
    Prompts an LLM and returns the answer as a string
    """

    @abstractmethod
    def prompt(self, system_prompt:str, task_description:str, problem_description:str, public_tests:str) -> str: 
        pass