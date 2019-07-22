

unset PYTHONHOME
unset PYTHONPATH

rm -r build/
rm ~/.local/lib/python3.6/site-packages/UserMethods-1.0-py3.6.egg-info 
rm ~/.local/lib/python3.6/site-packages/UserMethods.cpython-36m-x86_64-linux-gnu.so

root-config --cflags --glibs

python3 setup.py install --user  

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH



