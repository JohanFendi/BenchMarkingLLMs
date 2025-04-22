import ollama
from datasets import load_dataset

from LLMPrompting.LLMPrompter import LLMPrompter
from LLMPrompting.OllamaPrompters import OllamaPrompter
from DataManagment.DBreading.datasetReaders import DatasetReader
from DataManagment.FeatureProcessing import compareTypes, mergeFeatures

d1 = {"k":5, "z": 10, "yy": 44,  "Höh":[444]}
d2 = {"yy": 4, "z": 1, "Höh":[],  "k":44}

dataset_name = "deepmind/code_contests"
dataset = load_dataset(dataset_name)

features = ["public_tests", "private_tests", "generated_tests"]
keys = ["input", "output"]

reader = DatasetReader(dataset["train"])
p1 = reader.getPoint(100, features)


x = {feature:{key:[] for key in keys}  for feature in features}



print("===PreMerged===")
print(p1)
print("===PostMerged===")
mergeFeatures(p1, features, keys, "tests")
print(p1)

