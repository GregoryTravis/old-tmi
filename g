python monious.py ; exit

#python hoob.py | tee j; exit

set -e

#python zoom.py ; exit

#python -m pdb parse.py
#make
rm -f input.tmi.pre
python parse.py 2>&1
#cat input.tmi.pre
#./pt
exit

acompile input.tmi 2>&1
exit

python tmi.py
#./compile-and-run
#REQUEST_METHOD=GET QUERY_STRING=%5B%22logout%22%2C%20%5B%5D%5D ./run-old.cgi 
