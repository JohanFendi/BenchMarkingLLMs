import pytest
from datasets import load_dataset
from ollama import Client

from src.LLMPrompting.OllamaPrompter import OllamaPrompter
from src.DataManagment.DBreading.DBReader import DBReader
from src.DataManagment.Preprocessing.CodeContestsPreprocessor import CodeContestsPreprocessor
from src.Compilation.FolderCreator import FolderCreator
from src.Compilation.Compiler import Compiler
from src.SolutionWriting.SolutionWriter import SolutionWriter
from src.SolutionTesting.SolutionTester import SolutionTester
from src.DataManagment.DBwriting.DBWriter import DBWriter



def test_test_case_zero(): 
    task_description = """You are a Haskell expert. Your task is to solve the following coding problem by writing a complete Haskell program.
    
    Requirements:
    - Implement a `main :: IO ()` that reads all input from stdin and writes the answer to stdout.
    - You may define as many helper functions as you need.
    - Only use Prelude functions—**do not** import any modules.
    - The code must compile under GHC and satisfy the problem specification.

    Respond with **only** valid Haskell code. **Do not** include comments, explanations, or markdown—just the standalone code."""

    dataset = load_dataset("deepmind/code_contests")["train"]
    preprocessor = CodeContestsPreprocessor(dataset)
    llm_prompter = OllamaPrompter(Client())


    formated_public_tests = preprocessor.getFormatedPublicTests(index)
    problem_description = preprocessor.getProblemDescription(index)


    log = [{"role":"system", "content": "You are a Haskell Expert"}, 
           {"role": "user", "content":}]
    index = 0


