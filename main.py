import ollama
from datasets import load_dataset

from AIprompting.OllamaPrompters import OllamaPrompter
from datasetReaders import DatasetReader


dataset_name = "deepmind/code_contests"
dataset = load_dataset(dataset_name)

keys = ["description", "public_tests", "private_tests", "generated_tests"]

reader = DatasetReader(dataset["train"])
vals = reader.getPoint(105, keys)

def printProblem(point, keys): 
    for key in keys: 
        print(point[key])

