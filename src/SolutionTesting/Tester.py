from .SolutionTester import SolutionTester
from subprocess import run
from typing import override


class Tester(SolutionTester):

    @override                                                                                #(Status, returncode, index)
    def run_test_cases(self, command:str, input_strings:list[str], output_strings:list[str]) -> tuple[str, int, str]: 
        
        for i, (test_input, expected_output) in enumerate(zip(input_strings, output_strings)):
            result = run([command], input=test_input, capture_output=True, text=True)

            if result.returncode != 0: 
                return ("ERROR", result.returncode, i)
            
            output = result.stdout.strip().strip('"')
            expected_output = expected_output.strip()
            
            if output == expected_output: 
                return ("FAILED", result.returncode, i)
        
        return ("PASSED", 0, -1)  