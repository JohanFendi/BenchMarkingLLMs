import ollama
from AIPrompting.LLMPrompter import LLMPrompter 


class OllamaPrompter(LLMPrompter):

    def __init__(self, client : ollama.Client, model : str ) -> None:
        self._client = client
        self._model = model


    def prompt(task_description:str, problem_description:str, public_tests:str) -> str: 
        prompt = f"""Task description: {task_description} \n Coding problem: {problem_description}"""
        response = self._client.generate(model=self._model, prompt = prompt)
        return response.response