#!/usr/bin/env python3
import random
import sys

# Fixed seed for determinism
random.seed(42)

# Number of lines to generate roughly 30 seconds of processing time
N = 7500

colors = ["\033[31m", "\033[32m", "\033[33m", "\033[34m", "\033[35m", "\033[36m", "\033[1;31m", "\033[1;32m"]
reset = "\033[0m"

for i in range(N):
    length = random.randint(100, 1000)
    num_escapes = random.randint(1, 10)
    
    line = ""
    for j in range(num_escapes):
        # Add some text
        chunk_len = length // num_escapes
        line += "".join(random.choice("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 ") for _ in range(chunk_len))
        # Add an escape
        if random.random() < 0.1:
            line += "\033[K" # Clear to end of line
        elif random.random() < 0.1:
            line += f"\033[{random.randint(1, 10)}D" # Cursor back
        else:
            line += random.choice(colors)
    
    line += reset
    print(f"{i:05d} {line}")
