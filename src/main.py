from DataManagment.Preprocessing import CodeContestsPreprocessor 
from DataManagment.DBwriting import CSVWriter
from LLMPrompting import OllamaPrompter, LLMPrompter
from SolutionTesting import SolutionTester, Tester
from FolderCreation import FolderCreator, HSFolderCreator

import os
from pathlib import Path

root = os.path.abspath(os.curdir)
print(root)






