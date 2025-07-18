from logging import getLogger, FileHandler, Logger, Formatter, INFO


def get_logger(fmt:str, logger_name:str, log_file_name:str) -> Logger: 
    logger = getLogger(logger_name)
    logger.setLevel(INFO)
    handler = FileHandler(log_file_name)
    handler.setLevel(INFO)
    formatter = Formatter(fmt)
    handler.setFormatter(formatter)
    logger.addHandler(handler)
    return logger


