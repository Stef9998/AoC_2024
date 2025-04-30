import os
import sys
import runpy

if __name__ == '__main__':
    day = 2
    directory = f'day{day}'
    sys.path.insert(0, os.path.abspath(directory))
    os.chdir(directory)
    runpy.run_module(f'{directory}.main', run_name='__main__')