import ollama
from datasets import load_dataset

from OllamaPrompters import OllamaPrompter
from datasetReaders import DatasetReader


dataset_name = "deepmind/code_contests"
dataset = load_dataset(dataset_name)

keys = ["description", "public_tests", "private_tests", "generated_tests"]

reader = DatasetReader(dataset["train"])
vals = reader.getPoint(105, keys)

def printProblem(point, keys): 
    for key in keys: 
        print(point[key])

"""
num_null = 0
for i in range(len(dataset["train"])): 
    point = dataset["train"][i]
    if len(point["private_tests"])==0 and len(point["generated_tests"])==0: 
        num_null += 1

print(num_null)
"""

l = 0
for i in range(len(dataset["train"])): 
    point = dataset["train"][i]

    if (not point["private_tests"]["input"] and not point["private_tests"]["output"]) and (not point["generated_tests"]["input"] and not point["generated_tests"]["output"]): 
        l += 1

print(l)