from typing import TypeVar

from src.Exceptions import NestedTypeMismatchError, CodeFenceNotFoundError


T = TypeVar("T")


def buildMergedFeature(point:dict[str, any], features:list[str], keys:list[str]) -> dict[str, list[str]]: 
    correctly_typed_point = {feature:{key:[] for key in keys} for feature in features}

    if not compareTypes(correctly_typed_point, point): 
        raise NestedTypeMismatchError("Type of point not equal to required type.")

    merged_feature = {key:[] for key in keys}

    for key in keys:
        resultingList = merged_feature[key]
        for feature in features: 
            listOfFeatureToBeMerged = point[feature][key]
            resultingList.extend(listOfFeatureToBeMerged)
            
    return merged_feature


def compareTypes(correctly_typed_dict:T, a:T) -> bool:

    aIsDict = isinstance(a, dict)
    bIsDict = isinstance(correctly_typed_dict, dict)

    if not aIsDict and not bIsDict: 
        return type(a) == type(correctly_typed_dict)
    
    if not aIsDict or not bIsDict: 
        return False
    
    #Now we only have dictionaries
    a_keys = a.keys()
    if not all(key in a_keys for key in correctly_typed_dict.keys()):
        return False

    return all(compareTypes(correctly_typed_dict[key], a[key]) for key in correctly_typed_dict.keys())


def remove_code_fence(identifier:str, solution_string:str) -> str:
    string_lower = solution_string.lower()
    identifier = identifier.lower()
    start = f"```{identifier}"
    start_idx = string_lower.rfind(start)

    if start_idx == -1: 
        raise CodeFenceNotFoundError(f"Excpected start of code fence, '{start}', not found in solution string :{solution_string}")
    
    solution_string = solution_string[start_idx+len(start):]

    end = "```"
    end_idx = solution_string.find(end)
    if end_idx == -1: 
        raise CodeFenceNotFoundError(f"Excpected end of code fence, '{end}', not found in solution string :{solution_string}")

    return solution_string[:end_idx]
