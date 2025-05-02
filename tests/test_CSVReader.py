import pytest
from pathlib import Path

from src.DataManagment.DBwriting import CSVWriter


@pytest.fixture
def get_csv_writer(): 
    with open("test.csv", "w") as f: 
        pass

def buffer_cleared_at_max_size(): 
    cols = ["Solution", "result"]
    writer = CSVWriter("test.csv",cols , 3)
    writer.write(cols, ["print(hey)", "PASSED"])
    writer.write(cols, ["print(hey)", "PASSED"])
    writer.write(cols, ["print(hey)", "PASSED"])
    assert writer.getFreeSpace() == 0, "Free space not equal zero"
    assert writer._isBufferFull() == True
    writer.write(cols, ["print(hey)", "PASSED"])
    assert writer.get_free_space() == 2
    assert writer._isBufferFull() == False
