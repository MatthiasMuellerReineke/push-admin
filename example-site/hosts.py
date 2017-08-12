import os 
from aslib import predefined
from aslib.predefined import All

class Debianish(predefined.Debianish):
    packages = 'zsh-doc'


class All(predefined.All):
    packages = 'zsh'


hosts = [
    All('apollo11.local'),
    All('apollo13.local'),
    All(os.environ['REAL_SYSTEM']),
]
