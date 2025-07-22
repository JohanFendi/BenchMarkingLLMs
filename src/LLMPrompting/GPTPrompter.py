from openai import AsyncOpenAI
from typing import override


from src.ApiKey import load_api_key
from src.LLMPrompting.LLMPrompter import LLMPrompter


class GPTPrompter(LLMPrompter): 

    def __init__(self, model:str) -> None: 
        self._model = model
        key = load_api_key("OPENAI_API_KEY")
        self._client = AsyncOpenAI(api_key=key)

    
    @override
    async def prompt(self, 
               system_prompt:str, 
               task_description:str, 
               problem_description:str, 
               public_tests:str) -> str:
        
        response = await self._client.responses.create(
            model=self._model,
            input=[
                {"role":"user", "content":f"**Task Description**{task_description}"}, 
                {"role":"user", "content":f"**Problem Description**{problem_description}"},
                {"role":"user", "content":f"**Public Tests**{public_tests}"}
            ], 
            stream=False
        )
       
        return response.output_text
    
    