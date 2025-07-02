from .SolutionTester import SolutionTester
from subprocess import run, CompletedProcess
from typing import override

from constants import ERROR_STRING, PASSED_STRING, FAILED_STRING


class Tester(SolutionTester):

    @override                                                                                #(Status, returncode, index)
    def run_test_cases(self, command:str, input_strings:list[str], output_strings:list[str]) -> tuple[str, CompletedProcess, int, str]: 
        
        for i, (test_input, expected_output) in enumerate(zip(input_strings, output_strings)):
            result = run([command], input=test_input, capture_output=True, text=True)

            if result.returncode != 0: 
                return (ERROR_STRING, result, i, "")
            
            output = result.stdout.strip().strip('"')
            expected_output = expected_output.strip()

            if output != expected_output: 
                return (FAILED_STRING, result, i, output) 
        
        return (PASSED_STRING, result, -1, "")  