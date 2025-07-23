from asyncio import Task, create_task, wait, FIRST_COMPLETED
from typing import override, Coroutine, Any


from src.Scheduling.Scheduler import Scheduler


class UnorderedScheduler(Scheduler): 


    def __init__(self, max_parallel_tasks:int) -> None: 
        self._max_parallel_tasks = max_parallel_tasks
        self._tasks : set[Task[Any]] = set()


    @override
    async def schedule(self, coro:Coroutine) -> bool:
        if self.is_full(): 
            return False
        
        task = create_task(coro)
        self._tasks.add(task)
        return True
    

    @override
    async def get(self) -> list[Any]: 
        if self.is_empty(): 
            return []
        
        done, pending = await wait(self._tasks, return_when=FIRST_COMPLETED) 
        self._tasks = pending
        results = []

        for done_task in done: 
            results.append(done_task.result())

        return results

    
    @override
    def is_full(self) -> bool: 
        return len(self._tasks) == self._max_parallel_tasks
    

    @override 
    def is_empty(self):
        return len(self._tasks) == 0