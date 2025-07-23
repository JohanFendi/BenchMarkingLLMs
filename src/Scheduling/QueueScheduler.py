from src.Scheduling.Scheduler import Scheduler
from typing import override, Coroutine, Any
from asyncio import Task, Queue, create_task


class QueueScheduler(Scheduler):

    def __init__(self, max_parallel_tasks:int) -> None: 
        self._max_parallel_tasks = max_parallel_tasks
        self._queue : Queue[Task[Any]] = Queue()


    @override
    async def schedule(self, coro:Coroutine[Any, Any, Any]) -> bool:
        if self.is_full(): 
            return False #Not enqueued
        
        task = create_task(coro)
        await self._queue.put(task)
        return True #Succesfully enqueued
    

    @override
    async def get(self) -> list[Any]: 
        if self.is_empty(): 
            return []
            
        task = await self._queue.get()
        result = await task
        return [result]
        
        
    @override
    def is_empty(self) -> bool:
        return self._queue.qsize() == 0
    

    @override
    def is_full(self) -> bool: 
        return self._queue.qsize() == self._max_parallel_tasks

