from logging import Logger
from time import time


from src.Exceptions import *
from src.Logging.EpochLogger import EpochLogger
from src.Pipeline import Pipeline
from src.Scheduling.Scheduler import Scheduler
    

class App: 

    def __init__(self, 
                 pipeline:Pipeline, 
                 epoch_logger:EpochLogger,
                 error_logger:Logger, 
                 llm_schedular:Scheduler, 
                 llm_system_prompt:str, 
                 llm_task_description:str, 
                 programming_language:str, 
                 file_name:str, 
                 file_postfix:str) -> None:
    
        self._pipeline = pipeline
        self._llm_schedular = llm_schedular
        self._epoch_logger = epoch_logger
        self._error_logger = error_logger
        self._llm_system_prompt = llm_system_prompt
        self._llm_task_description = llm_task_description

        #Dir and file variables 
        self._programming_language = programming_language
        self._file_name = file_name
        self._file_postfix = file_postfix


    async def run(self, 
            process_id: str,
            start_index:int, 
            end_index:int) -> None:    
        
        if start_index > end_index: 
            raise IndexError("start_index must be less than or equal to end_index.")
         
        current_index = start_index
        try: 
            print("In loop")

            epoch_start_time = time()
            while current_index < end_index:   
                coro = self._pipeline.prompt_llm(current_index, self._llm_system_prompt, self._llm_task_description) 
                if self._llm_schedular.is_full(): 
                    epoch_start_time = await self._process_point(process_id, epoch_start_time)
                else: 
                    print(f"Scheduled LLM prompt with index {current_index}")
                    scheduled = await self._llm_schedular.schedule(coro)
                    if not scheduled: 
                        raise SchedulerError(f"Task scheduler was not full, but task failed to schedule.")
                    current_index += 1

            while not self._llm_schedular.is_empty():
               epoch_start_time = await self._process_point(process_id, epoch_start_time)
               
            print("Out of loop!")
            self._epoch_logger.log_epoch(epoch_start_time)    
            self._pipeline.clean_up()
              

        except DataLengthMismatchError as e: 
            self._error_logger.exception(f"Iteration {current_index} of App.run(): DataLengthMismatchError:")
            await self.run(process_id, current_index+1, end_index)

        except CodeFenceNotFoundError as e: 
            self._error_logger.exception(f"Iteration {current_index} of App.run(): CodeFenceNotFoundError:")
            await self.run(process_id, current_index+1, end_index)

        except DirectoryNotFoundError as e: 
            self._error_logger.exception(f"Iteration {current_index} of App.run(): DirectoryNotFoundError: ")
            return 
        
        except FileNotFoundError as e: 
            self._error_logger.exception(f"Iteration {current_index} of App.run(): FileNotFoundError:")
            return 
        
        except WrongOSError as e: 
            self._error_logger.exception(f"Iteration {current_index} of App.run():WrongOSError:")
            return
        
        except NestedTypeMismatchError as e: 
            self._error_logger.exception(f"Iteration {current_index} of App.run(): NestedTypeMismatchError:")
            await self.run(process_id, current_index+1, end_index)

        except ColumnMismatchError as e: 
            self._error_logger.exception(f"Iteration {current_index} of App.run(): ColumnMismatchError:")
            return
        
        except SchedulerError as e: 
            self._error_logger.exception(f"Iteration {current_index} of App.run(): SchedulerError:")
            return
        

    async def _process_point(self,
                             process_id:str, 
                             epoch_start_time:float) -> float: 
        
        print(f"Waiting for schedular response at:{round(time(), 2)}")
        response_start_time = time()
        llm_prompt_datapoints = await self._llm_schedular.get()
        response_end_time = time()

        response_time = response_end_time - response_start_time
        if len(llm_prompt_datapoints) == 0: 
            raise SchedulerError(f"Process point was called while Task Scheduler was empty.")

        for llm_prompt_data in llm_prompt_datapoints: 
            eval_time, _, status = self._pipeline.run_evaluation(llm_prompt_data, process_id)

            time_data = { 
                "AI_API_RESPONSE_TIME":llm_prompt_data.ai_time, 
                "AI_API_WAITING_TIME":response_time,
                "EVALUATION_TIME":eval_time
            }

            for k,v in time_data.items(): 
                print(f"{k}: {round(v,4)}")

            epoch_start_time = self._epoch_logger.update_epoch(time_data, epoch_start_time, status)
        return epoch_start_time




   
    

    



   

        




