from abc import ABC, abstractmethod
from typing import Coroutine, Any
from asyncio import Task



class Scheduler(ABC): 


    @abstractmethod
    async def schedule(self, coro:Coroutine[Any, Any, Any]) -> bool:
        """Makes corutine into task and schedules it. Retuns true if succesful else false"""
        pass


    @abstractmethod
    async def get(self) -> list[Any]: 
        """Get results of some tasks in a set. """
        pass


    @abstractmethod
    def is_empty(self) -> bool: 
        pass


    @abstractmethod
    def is_full(self) -> bool: 
        pass