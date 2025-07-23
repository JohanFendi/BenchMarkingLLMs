from datasets import load_dataset
from os import getpid
from asyncio import run 
from sys import exit


from src.App import App
from constants import *
from LLMTaskDescriptions import HASKELL_TASK_DESCRIPTION
from src.LLMPrompting.GPTPrompter import GPTPrompter
from src.Compilation.ProcessFolderCreator import ProcessFolderCreator
from src.Compilation.WindowsGhcCompiler import WindowsGhcCompiler
from src.DataManagment.DBreading.DatasetReader import DatasetReader
from src.DataManagment.DBwriting.csvWriter import CSVWriter
from src.DataManagment.Preprocessing.CodeContestsPreprocessor import CodeContestsPreprocessor
from src.SolutionWriting.RegularSolutionWriter import RegularSolutionWriter
from src.SolutionTesting.Tester import Tester
from src.Logging.EpochLogger import EpochLogger
from src.Logging.GetLogger import get_logger
from src.Pipeline import Pipeline
from src.Scheduling.QueueScheduler import QueueScheduler
from src.Scheduling.UnorderedScheduler import UnorderedScheduler



file_postfix = ".hs"
csv_file_name = "test.csv"
haskell_file_name = "main.hs"
programming_language = "haskell"
column_names = AVAILABLE_COLUMNS
llm_task_description = HASKELL_TASK_DESCRIPTION
llm_system_prompt = SYSTEM_PROMPT
max_buffer_size = 25
max_parallell_ai_prompts = 8
start_index = 0
end_index = 1 #Exclusive
epoch_size = max_buffer_size


#Pipeline components
dataset = load_dataset("deepmind/code_contests")["train"]
preprocessor = CodeContestsPreprocessor(dataset)
llm_prompter = GPTPrompter("o4-mini-2025-04-16")
folder_creator = ProcessFolderCreator()
compiler = WindowsGhcCompiler()
solution_writer = RegularSolutionWriter()
db_writer = CSVWriter(csv_file_name, column_names, max_buffer_size)
solution_tester = Tester()
epoch_logger = EpochLogger(epoch_size)
error_logger = get_logger(LOGGER_FORMAT, ERROR_LOGGER_NAME, ERROR_LOG_FILE_NAME)
llm_scheduler = UnorderedScheduler(max_parallell_ai_prompts)



if __name__ == "__main__":
    try: 
        pids = [str(getpid())]
        pipeline = Pipeline(preprocessor, preprocessor, llm_prompter, folder_creator, 
                            compiler, solution_writer, db_writer, solution_tester, 
                            programming_language, pids, haskell_file_name, file_postfix)
        app = App(pipeline, epoch_logger, error_logger, llm_scheduler,
                    llm_system_prompt, llm_task_description, programming_language, 
                    haskell_file_name, file_postfix)

        
        run(app.run(pids[0], start_index, end_index))
    except Exception as e: 
        db_writer.flush()
        epoch_logger.log_epoch()
        error_logger.exception("Uncaught exception:")
        exit()











