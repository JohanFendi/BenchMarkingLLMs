import ollama
from OllamaPrompters import OllamaPrompter
from datasets import load_dataset

model_name =  "llama3.2-vision:latest"
prompter = OllamaPrompter(ollama.Client(), model_name)
task_description = """Your task is to answer the following coding 
                        question in haskell. You should not write any comments,
                        not include any natural language in your respons and not use 
                        any imports.
                    """
coding_problem = "Write a method that takes in a integer and returns its square root. "


dataset_name = "deepmind/code_contests"
dataset = load_dataset(dataset_name)

print(dataset["train"][13])
