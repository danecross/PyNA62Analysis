

python3 setup.py sdist
cd dist/
cp spam-1.0.tar.gz ~/
cd
tar -xvf spam-1.0.tar.gz
cd spam-1.0/
python3 setup.py install --user
cd 

rm spam-1.0.tar.gz
rm -r spam-1.0/
