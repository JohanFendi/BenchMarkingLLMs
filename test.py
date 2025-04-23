from os import getpid
from pathlib import Path
from FolderCreation.HSFolderCreator import HSFolderCreator
from subprocess import run

process_id = getpid()

print(process_id)

print(Path("Haskell/main.hs").exists())

folderCreator = HSFolderCreator()

x = ""
for i in range(1): 
    x = folderCreator.create(f"main :: IO()\nmain = putStrLn $ show {i}", 6648, "Haskell", "main.hs     ")

result = run([x], capture_output=True, text=True)
print(result.stdout)