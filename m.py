from datasets import load_dataset
from src.DataManagment.Preprocessing.CodeContestsPreprocessor import CodeContestsPreprocessor
from src.LLMPrompting.AkashPrompter import AkashPrompter
from src.LLMPrompting.GPTPrompter import GPTPrompter

import constants


model1= "gpt-4.1-2025-04-14"
model2 = "o4-mini-2025-04-16"
ap = GPTPrompter(model1)

print("===Prompting AI===")
response = ap.prompt(constants.SYSTEM_PROMPT, constants.TASK_DESCRIPTION, constants.PROBLEM_ONE_DESCRIPTION, constants.PROBLEM_ONE_PUBLIC_TESTS)
print("===Finished Prompt===")
print(response)


