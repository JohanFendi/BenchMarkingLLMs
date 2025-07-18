from dotenv import load_dotenv
from os import getenv


from src.Exceptions import APIKeyNotFoundError


def load_api_key(api_key_name:str) -> str: 
    load_dotenv()
    key = getenv(api_key_name)
    if not key: 
        raise APIKeyNotFoundError(f"{api_key_name} is missing from environment.")
    return key