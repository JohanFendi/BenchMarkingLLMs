import csv
import os
from typing import override

from .DBWriter import DBWriter


class CSVWriter(DBWriter): 

    def __init__(self, file_name:str, column_names:list[str], max_buffer_size:int) -> None:
        if ".csv" in file_name and ".csv" == file_name[len(file_name)-4:]: 
            file_name = file_name[:len(file_name)-4]
        
        self._path = f"{file_name}.csv"
        self._column_names = column_names
        self._rows = [] # Tuple where _rows[i] is data of type _column_names[i]
        self._max_buffer_size = max_buffer_size
        self._current_buffer_size = 0
        self._setup_database()
        
        
    @override
    def _setup_database(self):
    
        #If file exists and is non-empty, then we make sure 
        #its column names match the column names of the csvWriter
        #Throws a ValueError.
 
        if os.path.exists(self._path) and os.path.getsize(self._path) > 0: 
            column_names = None

            with open(self._path,"r", newline="") as f:
                reader = csv.reader(f)
                column_names = next(reader)

            if column_names != self._column_names: 
                raise ValueError(
                f"CSV schema mismatch: expected columns {self._column_names} "
                f"but found {column_names}" )
            
        else: 
            with open(self._path, "w", newline="") as csv_file: 
                writer = csv.writer(csv_file)
                writer.writerow(self._column_names)
        

    @override
    def _buffer(self, column_names:list[str], values:list[str]) -> None:
        if len(column_names) != len(values): 
            raise ValueError("Number of column names not equal to number of values.")

        for column in column_names: 
            if column not in self._column_names: 
                raise ValueError(f"Column {column} not a column of the DBWriter.")

        self._rows.append(tuple(values))
        self._current_buffer_size += 1


    @override
    def _flush(self) -> None:
        with open(self._path, "a", newline="") as csv_file: 
            writer = csv.writer(csv_file)
            for row in self._rows: 
                writer.writerow(row)

        self._rows = []
        self._current_buffer_size = 0


    @override
    def _isBufferFull(self) -> bool: 
        return self._current_buffer_size >= self._max_buffer_size


    @override
    def get_free_space(self) -> int:
        return self._max_buffer_size - self._current_buffer_size
