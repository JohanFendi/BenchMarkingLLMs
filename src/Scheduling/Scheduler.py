from abc import ABC, abstractmethod
from typing import Coroutine
from asyncio import Task



class Scheduler(ABC): 


    @abstractmethod
    async def schedule(self, coro:Coroutine[any, any, any], id: int) -> bool:
        """Schedules LLM prompting task. Retuns true if succesful else false"""
        pass


    @abstractmethod
    async def get(self) -> tuple[Task[any], int]: 
        """Get LLM prompting task with heighest priority. """
        pass


    @abstractmethod
    def is_empty(self) -> bool: 
        pass


    @abstractmethod
    def is_full(self) -> bool: 
        pass