from web import call

def foo(a, b):
  return 'YEAH ' + a + ' ' + b

def main():
  return 'OLDMAIN ' + call('fuu', foo, 'xx', 'yy')
