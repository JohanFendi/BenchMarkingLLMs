from typing import TypeVar

from src.Logging.GetLogger import get_logger
from constants import EPOCH_LOG_FILE_NAME, EPOCH_LOGGER_NAME, LOGGER_FORMAT


class EpochLogger(): 

    def __init__(self, epoch_size:int) -> None:
        self._logger = get_logger(LOGGER_FORMAT, EPOCH_LOGGER_NAME, EPOCH_LOG_FILE_NAME)
        self._epoch_size = epoch_size
        self._epoch_ai_time = self._epoch_eval_time = self._epoch_id = self._epoch_num_itr = 0
        self._epoch_status_occurences = {}
    
    
    def update_epoch(self, 
                    ai_time:int, 
                    eval_time:int, 
                    db_write_time:int,
                    status:str) -> bool: #If epoch was logged
        
        eval_time += db_write_time   
        self._epoch_ai_time += ai_time
        self._epoch_eval_time += eval_time
        self._epoch_num_itr += 1

        if status in self._epoch_status_occurences: 
            self._epoch_status_occurences[status] += 1
        else: 
            self._epoch_status_occurences[status] = 1

        if self._epoch_num_itr == self._epoch_size: 
            self.log_epoch()
            self._epoch_id += 1
            self._epoch_ai_time = self._epoch_eval_time = self._epoch_num_itr = 0
            self._epoch_status_occurences = {}
            return True #Epoch was logged
        
        return False #Epoch was not logged

    
    def log_epoch(self) -> None:
        if self._epoch_num_itr == 0: 
            self._logger.warning(f"Attempted to log epoch {self._epoch_id}, but no iterations were done in epoch.")
            return 
        
        avg_pipeline_time = self._epoch_eval_time / self._epoch_num_itr
        avg_ai_time = self._epoch_ai_time / self._epoch_num_itr
        self._logger.info(f"\nEpoch {self._epoch_id}:")
        self._logger.info(f"Average AI prompting time for epoch {self._epoch_id}: {round(avg_ai_time, 2)}")
        self._logger.info(f"Average evaluation time for epoch {self._epoch_id}: {round(avg_pipeline_time, 2)}")
        self._logger.info(f"Percentages for statuses: " + EpochLogger._get_percentages_str(self._epoch_status_occurences, self._epoch_num_itr))
    
  
    @staticmethod
    def _get_percentages_str(occurences_dict:dict[any, int], 
                             total_num_occur:int) -> str: 
        
        keys = occurences_dict.keys()
        percentages = []

        for key in keys: 
            occur = occurences_dict[key] 
            percentage = round((occur / total_num_occur) * 100, 2)
            percentages.append(f"| {key}: {percentage} ")

        return "".join(percentages)