import re
import sys

def indentationOf(s):
  m = re.match('^(\s*)', s)
  return len(m.group(1))

def removeNewline(s):
  assert s[-1] == '\n'
  return s[:-1]

assert 'abc' == removeNewline('abc\n')

def removeEmptyLines(ss):
  return [s for s in ss if not re.match('^\s*$', s)]

# hash-style comment; does not handle hashes in strings properly
def removeComment(s):
  index = s.find('#')
  if index != -1:
    s = s[0:index]
  return s

assert 'hey ' == removeComment('hey # comment')

assert ['a\n', 'b\n'] == removeEmptyLines(['a\n', '\n', '   \n', 'b\n'])

def sp(n):
  return ' ' * (n)

assert '   ' == sp(3)

file = sys.argv[1]
with open(file) as f:
  lines = removeEmptyLines([removeComment(removeNewline(x)) for x in f])

indentations = [0]
for (line, next) in zip(lines, lines[1:] + ['']):
  lineInd = indentationOf(line)
  nextInd = indentationOf(next) if next != None else 0
  if nextInd > lineInd:
    line = line + ' {'
    indentations.append(nextInd)
  elif nextInd == lineInd:
    line = line + ';'
  else:
    sys.stdout.flush()
    while nextInd < indentations[-1]:
      line = line + ' };'
      indentations.pop()
  print line

sys.exit(0)

indentations = [0]
for line in lines:
  assert line[-1] == '\n'
  line = line[:-1]
  if line == '':
    continue
  m = re.match('^(\s*)', line)
  indentation = len(m.group(1))
  if indentation == indentations[-1]:
    print (' ' * indentation) + '  ;'
  elif indentation > indentations[-1]:
    print (' ' * indentations[-1]) + '{'
    indentations.append(indentation)
  else:
    while indentation < indentations[-1]:
      indentations.pop()
      print (' ' * indentations[-1]) + '};'
    assert indentation == indentations[-1]
  print line
while 0 < indentations[-1]:
  indentations.pop()
  print (' ' * indentations[-1]) + '};'

while len(indentations) > 1:
  print (' ' * indentations[-1]) + '}'
  indentations = indentations[:-1]
