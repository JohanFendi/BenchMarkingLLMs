from datasets import load_dataset
from src.DataManagment.Preprocessing.CodeContestsPreprocessor import CodeContestsPreprocessor


dataset = load_dataset("deepmind/code_contests")["train"]
pp = CodeContestsPreprocessor()


index = 9000

good = True
for i in range(0,len(dataset)): 
    try: 
        tests = dataset[index]["public_tests"]
        inp, out = tests["input"], tests["output"]
        tests2 = pp._dataset_reader.getPoint(index, ["public_tests"])["public_tests"]
        inp2, out2 = tests2["input"], tests2["output"]
        good = good and inp == inp2 and out == out2
    except KeyError: 
        print(i)
        print(tests)
        print(tests2)
print(good)