import subprocess
import os

import ollama
from datasets import load_dataset

from LLMPrompting.LLMPrompter import LLMPrompter
from LLMPrompting.OllamaPrompters import OllamaPrompter
from DataManagment.DBreading.datasetReaders import DatasetReader

dir_path = "Haskell/"
file_name = "main.hs"
executable = "main.exe"
command = f"{dir_path}{executable}"
file_path = f"{dir_path}{file_name}"

#compilation = subprocess.run(["ghc",file_path])
#running = subprocess.run([command], input="4\n5", text=True, capture_output=True, timeout=1)
#print(running.stdout)
   

dataset_name = "deepmind/code_contests"
dataset = load_dataset(dataset_name)

keys = ["public_tests", "private_tests", "generated_tests"]

reader = DatasetReader(dataset["train"])
point = reader.getPoint(9500, keys)


t= type(point["private_tests"]["output"])
print(t)


def printProblem(point, keys): 
    for key in keys: 
        print(point[key])

