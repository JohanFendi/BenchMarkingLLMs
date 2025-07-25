import csv
import os
from typing import override
from copy import deepcopy

from .DBWriter import DBWriter
from src.Exceptions import ColumnMismatchError


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

    
    def _setup_database(self):
    
        #If file exists and is non-empty, then we make sure 
        #its column names match the column names of the csvWriter
        #Throws a ValueError.
 
        if os.path.exists(self._path) and os.path.getsize(self._path) > 2: 
            column_names = None

            with open(self._path,"r", newline="") as f:
                reader = csv.reader(f)
                column_names = next(reader)

            if len(column_names) == 0: 
                with open(self._path, "w", newline="") as csv_file: 
                    writer = csv.writer(csv_file)
                    writer.writerow(self._column_names)
                

            elif column_names != self._column_names: 
                raise ColumnMismatchError(
                f"""CSV schema mismatch: expected columns {self._column_names} 
                in {self._path} but found {column_names}.""" )
            
        else: 
            with open(self._path, "w", newline="") as csv_file: 
                writer = csv.writer(csv_file)
                writer.writerow(self._column_names)
        

    @override
    def _buffer(self, data:dict[str, any]) -> None:
        row = [data[col] for col in self._column_names]
        
        if len(row) != len(self._column_names): 
            raise ColumnMismatchError(f"""Data mismatch. Columns of datapoint: {data.keys} 
                                      do not match columns of CSVWriter : {self._column_names}.""")

        self._rows.append(row)
        self._current_buffer_size += 1


    @override
    def flush(self) -> None:
        with open(self._path, "a", newline="") as csv_file: 
            writer = csv.writer(csv_file)
            for row in self._rows: 
                writer.writerow(row)

        self._rows = []
        self._current_buffer_size = 0


    @override
    def _is_buffer_full(self) -> bool: 
        return self._current_buffer_size >= self._max_buffer_size


    @override
    def get_free_space(self) -> int:
        return self._max_buffer_size - self._current_buffer_size


    @override
    def get_column_names(self):
        return deepcopy(self._column_names)