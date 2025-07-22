from dataclasses import dataclass


@dataclass
class IterationData: 
    problem_solution:str
    status:str
    failed_output:str
    stderr:str
    failed_testcase_index:int
    return_code:int


@dataclass
class LLMPromptData: 
    ai_time:float
    pipeline_start_time:float
    problem_solution:str
    index:int