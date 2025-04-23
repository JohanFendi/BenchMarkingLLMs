from typing import TypeVar, Dict

T = TypeVar("T")


def formatPublicTests(input : list[str], output: list[str]) -> str: 
    if len(input) != len(output): 
        raise ValueError("Input and output must be same")
    
    strings = []
    for i, (input_str, output) in enumerate(zip(input, output)): 
        strings.append(f"Public test {i+1}:\nInput:\n{input_str}\nOutput:\n{output}")

    return "\n".join(strings)


def mergeFeatures(point:Dict[str, any], features:list[str], keys:list[str], newFeatureName:str) -> Dict[str, any]: 
    correctly_typed_feature = {feature:{key:[] for key in keys } for feature in features}
    if not compareTypes(correctly_typed_feature, point): 
        raise TypeError("Type of point not equal to required type.")

    point[newFeatureName] = {key:[] for key in keys}
    for feature in features: 
        for key in keys: 
            resultingList = point[newFeatureName][key]
            listOfFeatureToRemove = point[feature][key]
            resultingList.extend(listOfFeatureToRemove)

    for feature in features: 
        point.pop(feature)


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
    for key in a.keys():
        typeIsValid = typeIsValid and compareTypes(correctly_typed_dict[key], a[key])

    return typeIsValid