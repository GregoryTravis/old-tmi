#python hoob.py ; exit

set -e

#python zoom.py ; exit

#python -m pdb parse.py
make
python parse.py 2>&1
#./pt
exit

acompile input.tmi 2>&1
exit

python tmi.py
#./compile-and-run
#REQUEST_METHOD=GET QUERY_STRING=%5B%22logout%22%2C%20%5B%5D%5D ./run-old.cgi 
