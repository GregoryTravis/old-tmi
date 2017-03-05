_TAGS = ['a', 'b', 'br', 'button', 'font', 'form', 'i', 'input', 'pre']

def maketag(name):
  globals()[name] = lambda *args, **kwargs: tag(name, *args, **kwargs)

def tag(tag_name, *args, **kwargs):
  open = ('<' + ' '.join([tag_name] + ['%s="%s"' % (k, v) for k, v in kwargs.iteritems()]) +
    ('/>' if len(args) == 0 else '>'))
  close = '</%s>' % tag_name
  return [open] + list(args) + ([close] if len(args) > 0 else [])

map(maketag, _TAGS)
assert ['<a href="blah">', 'asdf', '</a>'] == a('asdf', href='blah')

def hidden(k, v):
  return input(name=k, value=v, type='hidden')
