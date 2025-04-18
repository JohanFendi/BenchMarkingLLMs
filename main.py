import ollama
from datasets import load_dataset

from AIPrompting.LLMPrompter import LLMPrompter
from AIPrompting.OllamaPrompters import OllamaPrompter
from DataManagment.datasetReaders import DatasetReader


dataset_name = "deepmind/code_contests"
dataset = load_dataset(dataset_name)

keys = ["description", "public_tests", "private_tests", "generated_tests"]

reader = DatasetReader(dataset["train"])
point = reader.getPoint(9000, keys)


def printProblem(point, keys): 
    for key in keys: 
        print(point[key])

printProblem(point, keys)