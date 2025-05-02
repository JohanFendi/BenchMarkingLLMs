from abc import ABC, abstractmethod

class DBWriter(ABC): 

    def write(self, column_names:list[str], values:list[str]) -> None: 
        """Takes datapoint, and writes it to the buffer, and flushes the buffer if needed"""
        if self._isBufferFull(): 
            self._flush()

        self._buffer(column_names, values)

    @abstractmethod
    def _buffer(self, column_names:list[str], values:list[str]) -> None: 
        """Stores datapoints in temporary buffer. Should increment a variable representing
        the current amount of datapoints stored in the buffer, which will be used in _isBufferFull()."""
        pass

    
    @abstractmethod
    def _isBufferFull(self) -> bool: 
        pass


    @abstractmethod
    def _flush(self) -> None: 
        """Writes all points in the buffer to the database. Should reset 
            the value of the number of points in the buffer to zero."""
        pass

    @abstractmethod
    def get_free_space(self) -> None: 
        pass