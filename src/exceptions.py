

#Thrown by SolutionWriters 
class DirectoryNotFoundError(Exception):
    pass


#Thrown by buildMergedFeature
class NestedTypeMismatchError(Exception): 
    pass


#Thrown by CodeContestsPreprocessor
class DataLengthMismatchError(Exception):
    pass


#Thrown by WindowsGHCCompiler, used to signal
#that os did not match excpected one. 
class WrongOSError(OSError): 
    pass


#Thrown by DBWriters
class ColumnMismatchError(Exception): 
    pass


#Thrown by LLMPrompters
class APIKeyNotFoundError(Exception): 
    pass


#Thrown by remove_code_fence
class CodeFenceNotFoundError(Exception): 
    pass


class SchedulerError(Exception): 
    pass


