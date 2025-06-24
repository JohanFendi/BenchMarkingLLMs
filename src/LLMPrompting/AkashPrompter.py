import os
import textwrap
from dotenv import load_dotenv
from openai import OpenAI
from typing import override

from src.LLMPrompting.LLMPrompter import LLMPrompter


class AkashPrompter(LLMPrompter): 
    
    def __init__(self, model:str) -> None: 
        self._model = model
        load_dotenv()
        key = os.getenv("AKASH_API_KEY")
        if not key: 
            raise RuntimeError("AKASH_API_KEY is missing from environment")
        self._client = OpenAI(
            api_key=key, 
            base_url= "https://chatapi.akash.network/api/v1"
        )
        
    @override
    def prompt(self, system_prompt:str, task_description:str, problem_description:str, public_tests:str) -> str: 
        response = self._client.chat.completions.create(
            model=self._model,
            messages=[
                {"role":"system", "content" : system_prompt},
                {"role": "user", "content": f"**Task description**:{task_description}"},
                {"role": "user", "content": f"**Problem description**:{problem_description}"}, 
                {"role": "user", "content": f"**Public tests**:{public_tests}"}]
        )
        return response.choices[0].message.content