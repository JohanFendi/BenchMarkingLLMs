import csv
from typing import override

from DataManagment.DBwriting.DBWriter import DBWriter


class CSVWriter(DBWriter): 

    def __init__(self, file_name:str, column_names:list[str], max_buffer_size:int) -> None:
        self._file_name = file_name
        self._column_names = column_names
        self._columns = {col:[] for col in self._column_names}
        self._max_buffer_size = max_buffer_size
        self._current_buffer_size = 0


    @override
    def _buffer(self, column_names:list[str], values:list[str]) -> None:

        """Appends datapoint to buffer."""

        if len(column_names) != len(values): 
            raise ValueError("Number of column names not equal to number of values.")

        for (column, value) in zip(column_names, values): 
            if column not in column_names: 
                raise ValueError(f"Column {column} not a column of the DBWriter.")

            self._columns[column].append(value)

        self._current_buffer_size += 1


    @override
    def _flush(self) -> None:
        with open(f"{self._file_name}.csv", "a") as csv_file: 
            writer = csv.writer(csv_file)
            for i in range(self._current_buffer_size): 
                writer.writerow([self._columns[col][i] for col in self._column_names]  )

        self._columns = {col:[] for col in self._column_names}
        self._current_buffer_size = 0


    @override
    def _isBufferFull(self) -> bool: 
        if self._current_buffer_size > self._max_buffer_size: 
            raise ValueError("Current_buffer_size larger than max_buffer_size.")
        return self._current_buffer_size == self._max_buffer_size


    @override
    def get_free_space(self):
        return self._max_buffer_size - self._current_buffer_size
