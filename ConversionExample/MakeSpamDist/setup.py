

from distutils.core import setup, Extension

spam_module = Extension('spam', sources=['spammodule.cpp'], language='C++', )

setup(name='spam',
      version='1.0',
      ext_modules=[spam_module],
      )
