from typing import TypeVar, Dict
from copy import deepcopy

T = TypeVar("T")


def mergeFeatures(point:Dict[str, any], features:list[str], keys:list[str], newFeatureName:str) -> None: 
    point = deepcopy(point)
    correctly_typed_point = {feature:{key:[] for key in keys} for feature in features}

    if not compareTypes(correctly_typed_point, point): 
        raise TypeError("Type of point not equal to required type.")
    
    if newFeatureName in point:
        raise ValueError(f"Key '{newFeatureName}' already exists in the dictionary.")

    point[newFeatureName] = {key:[] for key in keys}

    for key in keys:
        resultingList = point[newFeatureName][key]
        for feature in features: 
            listOfFeatureToBeMerged = point[feature][key]
            resultingList.extend(listOfFeatureToBeMerged)

    for feature in features: 
        point.pop(feature)

    return point


def compareTypes(correctly_typed_dict:T, a:T) -> bool:

    aIsDict = isinstance(a, dict)
    bIsDict = isinstance(correctly_typed_dict, dict)

    if not aIsDict and not bIsDict: 
        return type(a) == type(correctly_typed_dict)
    
    if not aIsDict or not bIsDict: 
        return False
    
    #Now we only have dictionaries
    if not all([key in a.keys() for key in correctly_typed_dict.keys()]):
        return False
    
    typeIsValid = True
    for key in correctly_typed_dict.keys():
        typeIsValid = typeIsValid and compareTypes(correctly_typed_dict[key], a[key])

    return typeIsValid