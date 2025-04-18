from typing import TypeVar, Dict

T = TypeVar("T")

def formatPublicTests(input : list[T], output: list[T]) -> str: 
    raise NotImplementedError("formatPublicTests not implemented") 


"""
Returns a new ponint with the features in featuresToMerge merged into one feature. 
Requires all features in featuresToMerge to be of the same type. 
"""
def mergeFeatures(dataPoint : Dict[str, any], features : list[str], featuresToMerge: list[str]) -> Dict[str, any]: 
    raise NotImplementedError("mergeFeatures not implemented. ")