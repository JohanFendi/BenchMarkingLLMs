from openai import AsyncOpenAI


class OpenAIPrompter(): 
    """Compatible with Akash chat and Deepseek api"""
    
    def __init__(self, model:str, api_key:str, url:str) -> None: 
        self._model = model
        self._client = AsyncOpenAI(
            api_key=api_key, 
            base_url=url
        )
    
    
    async def prompt(self, system_prompt:str, task_description:str, problem_description:str, public_tests:str) -> str: 
        response = await self._client.chat.completions.create(
            model=self._model,
            messages=[
                {"role": "user", "content": f"**Task description**:{task_description}"},
                {"role": "user", "content": f"**Problem description**:{problem_description}"}, 
                {"role": "user", "content": f"**Public tests**:{public_tests}"}
            ], 
            stream = False
        )
        return response.choices[0].message.content