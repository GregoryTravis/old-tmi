
def maketag(name):
  globals()[name] = lambda *args, **kwargs: tag(name, *args, **kwargs)

def tag(name, *args, **kwargs):
  open = '<' + ' '.join([name] + ['%s="%s"' % (k, v) for k, v in kwargs.iteritems()]) + '>'
  close = '</%s>' % name
  return [open] + list(args) + [close]

_TAGS = ['a', 'b', 'font', 'i', 'pre']
map(maketag, _TAGS)
assert ['<a href="blah">', 'asdf', '</a>'] == a('asdf', href='blah')

