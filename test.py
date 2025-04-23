from os import getpid
from pathlib import Path
from FolderCreation.HSFolderCreator import HSFolderCreator
from SolutionTesting.Tester import Tester
from subprocess import run

process_id = getpid()
folderCreator = HSFolderCreator()


command = folderCreator.create(f"main::IO()\nmain = do\n    var <- getLine\n    print var", 6648, "Haskell", "main.hs     ")
inputs = outputs = ["555", "667", "1400"]
tester = Tester()
result = tester.runTestCases(command, inputs, outputs)
print(result)
