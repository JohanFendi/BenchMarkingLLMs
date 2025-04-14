import ollama
from AIprompting.LLMPrompter import LLMPrompter


class OllamaPrompter(LLMPrompter):

    def __init__(self, client : ollama.Client, model : str ) -> None:
        self._client = client
        self._model = model


    def prompt(self, task_description:str, coding_problem:str) -> str:
        prompt = f"""Task description: {task_description} \n Coding problem: {coding_problem}"""
        response = self._client.generate(model=self._model, prompt = prompt)
        return response.response