from SolutionTesting.SolutionTester import SolutionTester
from subprocess import run


class Tester(SolutionTester):

    def runTestCases(self, command:str, inputs : list[str], expected_outputs : list[str]) -> str: 
        passed = True
        for i, input_str in enumerate(inputs):
            result = run([command], input=input_str, capture_output=True, text=True)

            if not result.stderr == "": 
                return "ERROR"
            
            output = result.stdout.strip().strip('"')
            passed = passed and output == expected_outputs[i].strip()

            if not passed: 
                return f"FAILED,{i}"
        
        return "PASSED" 