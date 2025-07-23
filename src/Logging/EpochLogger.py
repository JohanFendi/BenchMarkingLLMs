from time import time


from src.Logging.GetLogger import get_logger
from constants import EPOCH_LOG_FILE_NAME, EPOCH_LOGGER_NAME, LOGGER_FORMAT


class EpochLogger(): 


    _AVERAGES_NAMES = ["AI_API_RESPONSE_TIME", 
                 "AI_API_WAITING_TIME", 
                 "EVALUATION_TIME"]


    def __init__(self, epoch_size:int) -> None:
        self._logger = get_logger(LOGGER_FORMAT, EPOCH_LOGGER_NAME, EPOCH_LOG_FILE_NAME)
        self._epoch_size = epoch_size
        self._epoch_id = self._epoch_num_itr = 0
        self._epoch_status_occurrences = {}
        self._averages = {avg:0 for avg in EpochLogger._AVERAGES_NAMES}
        
    
    def update_epoch(self, 
                    time_data:dict[str, float], 
                    epoch_start_time:float, 
                    status:str) -> float: #Start time

        self._update_averages(time_data)
        self._epoch_num_itr += 1

        if status in self._epoch_status_occurrences: 
            self._epoch_status_occurrences[status] += 1
        else: 
            self._epoch_status_occurrences[status] = 1

        if self._epoch_num_itr == self._epoch_size:
            self.log_epoch(epoch_start_time)
            self._reset_epoch()
            return time() #Return new epoch start time
        
        return epoch_start_time #Epoch was not logged

    
    def log_epoch(self, epoch_start_time:float) -> None:
        if self._epoch_num_itr == 0: 
            self._logger.warning(f"Attempted to log epoch {self._epoch_id}, but no iterations were done in epoch.")
            return 

        epoch_end_time = time()
        epoch_time = epoch_end_time - epoch_start_time
        
        #Log totals 
        self._logger.info(f"========== Epoch {self._epoch_id} ==========")
        self._logger.info(f"Total epoch time: {round(epoch_time, 2)}")
        self._logger.info(f"Num iterations done in epoch: {self._epoch_num_itr}")
        self._logger.info(f"Percentages for statuses: {EpochLogger._get_percentages_str(self._epoch_status_occurrences, self._epoch_num_itr)}")

        #log averages
        for (avg_name, time_value) in self._averages.items(): 
            avg_time = time_value / self._epoch_num_itr
            self._logger.info(f"Average {avg_name}: {round(avg_time, 2)}")
        
        
    def _update_averages(self, time_data:dict[str, float]) -> None:
        for avg_name in EpochLogger._AVERAGES_NAMES:
             if avg_name not in time_data: 
                 raise RuntimeError() #TODO Fix 
             self._averages[avg_name] += time_data[avg_name] 


    def _reset_epoch(self) -> None: 
        self._epoch_id += 1
        self._epoch_num_itr = 0
        self._averages = {avg:0 for avg in EpochLogger._AVERAGES_NAMES}
        self._epoch_status_occurrences = {}

  
    @staticmethod
    def _get_percentages_str(occurences_dict:dict[any, int], 
                             total_num_occur:int) -> str: 
        
        keys = occurences_dict.keys()
        percentages = []

        for key in keys: 
            occur = occurences_dict[key] 
            percentage = round((occur / total_num_occur) * 100, 2)
            percentages.append(f"{key}: {percentage}")

        return " | ".join(percentages)