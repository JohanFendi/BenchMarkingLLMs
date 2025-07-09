from logging import getLogger, FileHandler, Logger, Formatter


def get_logger(fmt:str, logger_name:str, log_file_name:str) -> Logger: 
    logger = getLogger(logger_name)
    handler = FileHandler(log_file_name)
    formatter = Formatter(fmt)
    handler.setFormatter(formatter)
    logger.addHandler(handler)
    return logger


