from typing import TypeVar, Dict


T = TypeVar("T")


def buildMergedFeature(point:Dict[str, any], features:list[str], keys:list[str], newFeatureName:str) -> Dict[str, list[str]]: 
    correctly_typed_point = {feature:{key:[] for key in keys} for feature in features}

    if not compareTypes(correctly_typed_point, point): 
        raise TypeError("Type of point not equal to required type.")
    
    if newFeatureName in point:
        raise ValueError(f"Key '{newFeatureName}' already exists in the dictionary.")

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