from openai import OpenAI
from typing import override
from dotenv import load_dotenv
import os


from src.LLMPrompting.LLMPrompter import LLMPrompter


class GPTPrompter(LLMPrompter): 

    def __init__(self, model:str) -> None: 
        self._model = model

        load_dotenv()
        key = os.getenv("OPENAI_API_KEY")
        if not key: 
            raise RuntimeError("OPENAI_API_KEY is missing from environment.")
        
        self._client = OpenAI(api_key=key)

    
    @override
    def prompt(self, system_prompt:str, task_description:str, problem_description:str, public_tests:str) -> str:
        response = self._client.responses.create(
            model=self._model,
            input=[
                {"role":"user", "content":f"**Task Description**{task_description}"}, 
                {"role":"user", "content":f"**Problem Description**{problem_description}"},
                {"role":"user", "content":f"**Public Tests**{public_tests}"}
            ]
        )   

        return response.output_text
