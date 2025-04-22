from typing import TypeVar, Dict

T = TypeVar("T")


def formatPublicTests(input : list[T], output: list[T]) -> str: 
    raise NotImplementedError("formatPublicTests not implemented") 


def mergeFeatures(point:Dict[str, any], features:list[str], keys:list[str], newFeatureName:str) -> Dict[str, any]: 
    """
    Mutates the input point with the features in features merged into one feature. 
    The point needs to be a dictionary type. The features of the point in turn also dictionaries. 
    These dictionaries have the specified keys. Their values are lists of the same type. 
    Ex: keys = ["input", "output"], features = ["private_tests", "generated_tests"], newFeatureName = "tests"
    point = {"private_tests":{"input":[2], "output":[5]}, "generated_tests":{"input":[6], "output":[7]}}
    Output is then: {"private_tests":{"input":[2, 6], "output":[5, 7]}
    """

    correct_typed_feature = {feature:{key:[] for key in keys } for feature in features}
    if not compareTypes(correct_typed_feature, point): 
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