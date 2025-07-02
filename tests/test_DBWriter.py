import pytest

from src.DataManagment.DBwriting import CSVWriter


#Clears a file 
def clear_file(file_path:str, postfix:str) -> None: 
    with open(f"{file_path}.{postfix}", "w") as f: 
        pass


# Global test variables    
CLEARING_FUNCTION = clear_file
FILE_PATH = "tests/test"
POSTFIX =  "csv"
COLS =  ["Result", "Solution"]
DATA_POINT = ["PASSED", "print(x)"]
DBWriter = CSVWriter


# ==== AUTOUSE FIXTURES =====


#Clears the database between test
@pytest.fixture(autouse=True)
def betweenTests(): 
    CLEARING_FUNCTION(FILE_PATH, POSTFIX)


#Handles setup and teardown of test program
@pytest.fixture(autouse=True, scope="session")
def setupTearDown(): 
    #Setup 

    CLEARING_FUNCTION(FILE_PATH, POSTFIX)

    yield
    #Teardown

    CLEARING_FUNCTION(FILE_PATH, POSTFIX)


# ==== Tests ====


def test_buffer_cleared_at_max_size(): 
    writer = DBWriter(FILE_PATH, COLS , 4)

    for i in range(4): 
        writer.write(COLS, DATA_POINT)
       
    assert writer.get_free_space() == 4, "Free space not equal zero"
    writer.write(COLS, DATA_POINT)
    assert writer.get_free_space() == 3
    assert writer._isBufferFull() == False


def test_appending_more_than_max():
    writer = DBWriter(FILE_PATH, COLS , 4)

    for i in range(10):
        writer.write(COLS,DATA_POINT)
       
    assert writer.get_free_space() == 2, f"Free of {writer.get_free_space()} space not equal to 2"
    assert writer._isBufferFull() == False


def test_buffer_is_size_correct_when_not_above_limit(): 
    writer =  DBWriter(FILE_PATH, COLS , 15)

    for i in range(10):
        writer.write(COLS, DATA_POINT)
       
    assert writer.get_free_space() == 5, "Free space not equal to five"


def test_error_when_appending_incomplete_datapoint(): 
    writer =  DBWriter(FILE_PATH, COLS , 15)

    with pytest.raises(ValueError) as exeptionInfo: 
        writer.write(COLS, ["Print(Hello)"])

    assert str(exeptionInfo.value) == "Number of column names not equal to number of values.", "Improper exeption"


def test_error_when_appending_faulty_columns(): 
    writer = DBWriter(FILE_PATH, COLS , 15)

    with pytest.raises(ValueError) as exeptionInfo: 
        writer.write(["Solution", "Weather", "Rating"], ["Print(Hello)", "Failed", "655"])

    assert str(exeptionInfo.value) == f"Column Weather not a column of the DBWriter.", "Improper exeption"

