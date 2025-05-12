import pytest
from pathlib import Path

from src.DataManagment.DBwriting import CSVWriter


#Clears csv file 
def clear_file(file_path:str, postfix:str) -> None: 
    with open(f"{file_path}.{postfix}", "w") as f: 
        pass


# ==== AUTOUSE FIXTURES =====

@pytest.fixture(autouse=True)
def betweenTests(clearing_function): 
    clearing_function(file_path, postfix)


@pytest.fixture(autouse=True, scope="session")
def setupTearDown(file_path, postfix): 
    #Setup 

    clear_file(file_path, postfix)

    yield
    #Teardown

    clear_file(file_path, postfix)


# ==== VARIABLE GENERATING FIXTURES ====


@pytest.fixture(scope="session")
def cols() -> list[str]: 
    return ["Solution", "result", "Rating"]


@pytest.fixture(scope="session")
def file_path() -> str: 
    return "tests/test"


@pytest.fixture(scope="session")
def postfix() -> str: 
    return ".csv"


@pytest.fixture(scope="session")
def clearing_function(): 
    return clear_file


@pytest.fixture(scope="session")
def max_buffer_size(): 
    return 100


@pytest.fixture
def writer(file_path, cols, max_buffer_size): 
    return CSVWriter(file_path, cols, max_buffer_size)


# ==== Tests ====

def test_buffer_cleared_at_max_size(cols): 
    writer = CSVWriter(file_path,cols , 4)

    for i in range(4): 
        writer.write(cols, ["print(hey)", "PASSED", str(i)])
       
    assert writer.get_free_space() == 4, "Free space not equal zero"
    writer.write(cols, ["print(hey)", "PASSED", "2"])
    assert writer.get_free_space() == 3
    assert writer._isBufferFull() == False


def test_appending_more_than_max():
    writer = CSVWriter(file_path,cols , 4)

    for i in range(10):
        writer.write(cols, ["print(hey)", "PASSED", str(i)])
       
    assert writer.get_free_space() == 2, f"Free of {writer.get_free_space()} space not equal to 2"
    assert writer._isBufferFull() == False


def test_buffer_is_size_correct_when_not_above_limit(): 
    writer =  CSVWriter(file_path, ["Solution", "result", "Rating"] , 15)

    for i in range(10):
        writer.write(["Solution", "result", "Rating"], ["print(hey)", "PASSED", str(i)])
       
    assert writer.get_free_space() == 5, "Free space not equal to five"

def test_error_when_appending_incomplete_datapoint(): 
    writer =  CSVWriter(file_path, ["Solution", "result", "Rating"] , 15)


    with pytest.raises(ValueError) as exeptionInfo: 
        writer.write(["Solution", "result", "Rating"], ["Print(Hello)", "Failed"])

    assert str(exeptionInfo.value) == "Number of column names not equal to number of values.", "Improper exeption"

def test_error_when_appending_faulty_columns(): 
    writer =  CSVWriter(file_path, ["Solution", "result", "Rating"] , 15)


    with pytest.raises(ValueError) as exeptionInfo: 
        writer.write(["Solution", "Weather", "Rating"], ["Print(Hello)", "Failed", "655"])

    assert str(exeptionInfo.value) == f"Column Weather not a column of the DBWriter.", "Improper exeption"

