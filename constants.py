SYSTEM_PROMPT = "You are a Haskell expert."

#Status strings
COMPILATION_ERROR_STRING = "COMPILATION_ERROR"
ERROR_STRING = "ERROR"
FAILED_STRING = "FAILED"
PASSED_STRING = "PASSED"


#initial values
NO_FAILED_TESTCASES_INDEX = -1
NO_FAILED_TESTCASES_OUTPUT = ""


#Columns available for collection
AVAILABLE_COLUMNS = [
    "status",
    "problem_solution",
    "failed_testcase_index",
    "failed_output",
    "stderr",
    "return_code"
]


#logger names
EPOCH_LOGGER_NAME = "epoch_logger"
ERROR_LOGGER_NAME = "error_logger"


#log file names
EPOCH_LOG_FILE_NAME = "Epoch.log"
ERROR_LOG_FILE_NAME = "Error.log"


#logger format
LOGGER_FORMAT = "%(asctime)s - %(name)s - %(message)s"


