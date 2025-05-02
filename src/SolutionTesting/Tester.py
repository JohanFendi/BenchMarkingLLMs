from .SolutionTester import SolutionTester
from subprocess import run
from typing import override


class Tester(SolutionTester):

    @override
    def runTestCases(self, command:str, input_strings:list[str], output_strings:list[str]) -> str: 
        passed = True
        for i, input_str in enumerate(input_strings):
            result = run([command], input=input_str, capture_output=True, text=True)

            if not result.stderr == "": 
                return "ERROR"
            
            output = result.stdout.strip().strip('"')
            passed = passed and output == output_strings[i].strip()

            if not passed: 
                return f"FAILED,{i}"
        
        return "PASSED" 