import subprocess
import os


dir_path = "Haskell/"
file_name = "main.hs"
executable = "main.exe"
command = f"{dir_path}{executable}"
file_path = f"{dir_path}{file_name}"

compilation = subprocess.run(["ghc",file_path])
running = subprocess.run([command], input="4\n5", text=True, capture_output=True, timeout=1)

print(running.stdout)
   