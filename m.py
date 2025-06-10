from datasets import load_dataset
from src.DataManagment.Preprocessing.CodeContestsPreprocessor import CodeContestsPreprocessor


dataset = load_dataset("deepmind/code_contests")["train"]
pp = CodeContestsPreprocessor(dataset)


index = 0
tests = pp.getTestCases(index)
inputs = tests["input"]
outputs = tests["output"]

print(pp.getFormatedPublicTests(index))
print(inputs)
print(outputs)
