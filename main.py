from DataManagment.DBreading.DBReader import DBReader
from DataManagment.DBreading.datasetReaders import DatasetReader


w = DatasetReader([333])

if isinstance(w, DBReader): 
    print("Hey")