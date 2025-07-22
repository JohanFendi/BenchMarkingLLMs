from abc import ABC, abstractmethod


class DBWriter(ABC): 

    def write(self, data:dict[str:any]) -> None: 
        """Takes datapoint, and writes it to the buffer, and flushes the buffer if needed"""
        self._buffer(data)

        if self._is_buffer_full(): 
            self.flush()


    @abstractmethod
    def _buffer(self, data:dict[str:any]) -> None: 
        """Stores a datapoint in temporary buffer. Should increment a variable representing
        the current amount of datapoints stored in the buffer, which will be used in _isBufferFull()."""
        pass

    
    @abstractmethod
    def _is_buffer_full(self) -> bool: 
        pass


    @abstractmethod
    def flush(self) -> None: 
        """Writes all points in the buffer to the database. Should reset 
            the value of the number of points in the buffer to zero and 
            clear the buffer"""
        pass


    @abstractmethod
    def get_free_space(self) -> int: 
        pass


    @abstractmethod
    def get_column_names(self) -> list[str]: 
        """Return copy of column names."""
        pass