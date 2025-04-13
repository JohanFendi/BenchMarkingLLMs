from abc import ABC, abstractmethod

class DBReader(ABC): 

    """Returns points from database"""
    @abstractmethod
    def getPoint(index:int): 
        pass