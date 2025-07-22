from typing import override


from src.apiKey import load_api_key
from src.LLMPrompting.LLMPrompter import LLMPrompter
from src.LLMPrompting.OpenAIPrompter import OpenAIPrompter


class DeepseekPrompter(LLMPrompter): 

    def __init__(self, model:str) -> None:  
        key = load_api_key("DEEPSEEK_API_KEY")
        url = "https://api.deepseek.com"
        self._open_ai_prompter = OpenAIPrompter(model, key, url)


    @override
    async def prompt(self, system_prompt:str, task_description:str, problem_description:str, public_tests:str) -> str:
        return await self._open_ai_prompter.prompt(system_prompt, task_description, problem_description, public_tests)