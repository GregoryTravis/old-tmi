set -e

#python -m pdb parse.py
parse.py
exit

ncompile input.tmi 2>&1
exit

python tmi.py
#./compile-and-run
#REQUEST_METHOD=GET QUERY_STRING=%5B%22logout%22%2C%20%5B%5D%5D ./run-old.cgi 
