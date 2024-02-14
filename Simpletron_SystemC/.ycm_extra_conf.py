import os
import ycm_core

SYSTEMC_PATH = '/mnt/c/DevelTools/systemc/'

flags = [
    '-Wall',
    '-Wextra',
    '-I.',
    '-I'+SYSTEMC_PATH+'include',
    '-I'+SYSTEMC_PATH+'lib-linux64',
    '-lsystemc',
    '-lm'
]

SOURCE_EXTENSIONS = [ '.cpp', '.cxx', '.cc', '.c', ]

def Settings(**kwargs ):
  return {
  'flags': flags,
  'do_cache': True
  }
