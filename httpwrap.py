import re
import StringIO
import sys
import traceback

REQUIRED_HEADERS = set(['Location', 'Content-type'])

class HttpWrap(object):
  def __enter__(self):
    self.orig_stdout = sys.stdout
    self.orig_stderr = sys.stderr
    sys.stdout = StringIO.StringIO()
    sys.stderr = StringIO.StringIO()
    return None

  def __exit__(self, exc_type, exc_value, tb):
    stdout = sys.stdout
    stderr = sys.stderr
    sys.stdout = self.orig_stdout
    sys.stderr = self.orig_stderr
    out = stdout.getvalue()
    err = stderr.getvalue()
    if not self.properly_formatted(out) or len(err) > 0:
      out = 'Content-type: text/plain\n\n' + out
    if len(err) > 0:
      out += '<hr><hr>' + err
    if exc_type != None:
      out += '<hr><hr>qq' + ''.join(traceback.format_exception(exc_type, exc_value, tb))
    print out
    return True

  # The output must have a proper header section and this section must contain
  # either Content-type or Location.  If a Location header is present, then
  # there must be nothing after the headers.
  def properly_formatted(self, s):
    lines = s.split('\n')
    # Look through headers
    blank_line = False
    headers = set([])
    for line in lines:
      m = re.match('^([a-zA-Z-]+): .*$', line)
      if not m:
        blank_line = line == ''
        break
      headers.add(m.group(1))
    saw_required_header = len(headers.intersection(REQUIRED_HEADERS)) > 0
    extraneous_content = 'Location' in headers and len(headers) != len(lines)-2
    return saw_required_header and blank_line and not extraneous_content
