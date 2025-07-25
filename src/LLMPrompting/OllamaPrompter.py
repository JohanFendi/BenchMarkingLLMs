from ollama import chat
from src.LLMPrompting.LLMPrompter import LLMPrompter 


class OllamaPrompter(LLMPrompter):

    def __init__(self, model:str) -> None:
        self._model = model


    def prompt(self, system_prompt:str, task_description:str, problem_description:str, formated_public_tests:str) -> str: 
        log = [
                {"role":"system", "content": system_prompt}, 
                {"role": "user", "content": f"**Task description**: {task_description}"},
                {"role": "user", "content": f"**Problem description**: {problem_description}"}, 
                {"role": "user", "content": f"**Formated Public Tests**: {formated_public_tests}"}
              ]
        
        response = chat(self._model, messages=log, stream=False)
        return response.message.content