from distutils.core import setup, Extension

UM_module = Extension('UserMethods', sources=['../src/UserMethodsModule.cpp'], language='C++', )

setup(name='UserMethods',
      version='1.0',
      ext_modules=[UM_module],
      )
