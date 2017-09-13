# zoom = (x, start, end) where is x an array or another zoom.

def isidon(f, x):
  return x == f(x)

def pairmap(f, os):
  return [] if len(os) < 2 else [f(os[0], os[1])] + pairmap(f, os[1:])

assert [3, 5, 7, 9] == pairmap(lambda a, b: a + b, [1, 2, 3, 4, 5])
assert [True, False, False, False, True, False, False, False, True, False, True] == pairmap(lambda a, b: a == b, [0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1])

def first(os, p):
  if len(os) == 0:
    return None
  elif p(os[0]):
    return os[0]
  else:
    return first(os[1:], p)

assert None == first([1, 3, 7, 9], lambda x: (x%2)==0)
assert None == first([], lambda x: (x%2)==0)
assert 1 == first([1, 2, 3], lambda x: True)
assert 8 == first([1, 3, 7, 8, 9, 10], lambda x: (x%2)==0)
assert 10 == first([1, 3, 7, 8, 9, 10], lambda x: x == 10)

def allsame(os):
  return all(pairmap(lambda a, b: a == b, os))

assert allsame([])
assert allsame([1])
assert allsame([1, 1])
assert allsame([1, 1, 1])
assert not allsame([1, 2])
assert not allsame([1, 2, 1])
assert not allsame([1, 1, 2])

assert isidon(lambda x: x, 12)
assert not isidon(lambda x: x + 1, 12)

def zmake(arr):
  return (arr, 0, len(arr))

assert ([1, 2, 3], 0, 3) == zmake([1, 2, 3])
assert ([1], 0, 1) == zmake([1])
assert ([], 0, 0) == zmake([])

def zis(z):
  return type(z) == tuple and len(z) == 3 and type(z[1]) == int and type(z[2]) == int and (zis(z[0]) or type(z[0]) == list)

assert zis(([1, 2], 0, 1))
assert zis((([1, 2], 0, 1), 0, 1))
assert zis(zmake([1, 2]))
assert not zis([1, 2])
assert not zis(1)
assert not zis((1, 2))

def zlen(z):
  if zis(z):
    (x, s, e) = z
    return e - s
  elif type(z) == list:
    return len(z)
  else:
    assert False, z

assert 3 == zlen(zmake([1, 2, 3]))
assert 1 == zlen(([1, 2, 3], 0, 1))
assert 2 == zlen(([1, 2, 3], 0, 2))

def zok(z):
  (x, s, e) = z
  return s >= 0 and e <= (zlen(x) if zis(x) else len(x)) and s <= e and (type(x) == list or zok(x))

assert zok(zmake([1]))
assert not zok(([1, 2, 3], -1, 3))
assert not zok(([1, 2, 3], 0, 4))
assert zok(([1, 2, 3], 1, 2))
assert not zok(([1, 2, 3], 2, 1))

def zcheck(z):
  assert zok(z), z
  return z

def zoom(z, s, e):
  return zcheck((z, s, e))

def zstart(z):
  (x, s, e) = z
  return s

assert 2 == zstart(zoom(zmake([1, 2, 3, 4]), 2, 3))

def zend(z):
  (x, s, e) = z
  return e

assert 3 == zend(zoom(zmake([1, 2, 3, 4]), 2, 3))

def zval(z):
  (x, s, e) = z
  x = zval(x) if zis(x) else x
  return x[s:e]

assert [3] == zval(zoom(zmake([1, 2, 3]), 2, 3))
assert [1] == zval(zoom(zmake([1, 2, 3]), 0, 1))
assert [2, 3] == zval(zoom(zmake([1, 2, 3, 4]), 1, 3))
assert [2] == zval(zoom(zoom(zmake([1, 2, 3, 4]), 1, 3), 0, 1))

assert all([isidon(lambda arr: zval(zmake(arr)), x) for x in [[], [1], [1, 2], [1, 2, 3, 4]]])

def zval1(z):
  v = zval(z)
  assert len(v) == 1
  return v[0]

assert 3 == zval1(zoom(zmake([1, 2, 3]), 2, 3))
assert 1 == zval1(zoom(zmake([1, 2, 3]), 0, 1))

def zflatten(z):
  (x, s, e) = z
  if not zis(x):
    return z
  else:
    (x2, s2, e2) = x
    return zflatten((x2, s2 + s, s2 + e))

assert ([0, 1, 2, 3, 4, 5, 6, 7], 4, 6) == zflatten(zoom(zoom(zoom(zmake([0, 1, 2, 3, 4, 5, 6, 7]), 0, 7), 2, 6), 2, 4))

def zpop(z):
  (x, s, e) = z
  return x

assert [1, 2, 3] == zval(zpop(zoom(zmake([1, 2, 3]), 1, 2)))

def zbottom(z):
  (x, s, e) = z
  return zbottom(x) if zis(x) else x

assert [0, 1, 2, 3] == zbottom(zoom(zoom(zmake([0, 1, 2, 3]), 0, 2), 0, 1))

def zleft(z):
  (x, s, e) = z
  return (x, 0, s)

assert [] == zval(zleft(zmake([1, 2, 3])))
assert [0, 1] == zval(zleft(zoom(zmake([0, 1, 2, 3, 4, 5, 6]), 2, 4)))

def zright(z):
  (x, s, e) = z
  return (x, e, zlen(x))

assert [] == zval(zright(zmake([1, 2, 3])))
assert [4, 5, 6] == zval(zright(zoom(zmake([0, 1, 2, 3, 4, 5, 6]), 2, 4)))

def zleft1(z):
  (x, s, e) = z
  return zcheck((x, s-1, s))

assert [1] == zval(zleft1(zoom(zmake([1, 2, 3]), 1, 2)))

def zright1(z):
  (x, s, e) = z
  return zcheck((x, e, e+1))

assert [3] == zval(zright1(zoom(zmake([1, 2, 3]), 1, 2)))

assert all([zval(zpop(z)) == zval(zleft(z)) + zval(z) + zval(zright(z))
  for z in [ zoom(zmake([0, 1, 2, 3]), 1, 2), zoom(zmake([0, 1, 2, 3]), 2, 4) ]])

def zlvr(z):
  return (zval(zleft(z)), zval(z), zval(zright(z)))

assert ([0], [1], [2, 3]) == zlvr(zoom(zmake([0, 1, 2, 3]), 1, 2))

def zmakelvr(arrl, arrv, arrr):
  return zoom(zmake(arrl + arrv + arrr), len(arrl), len(arrl) + len(arrv))

assert zoom(zmake([0, 1, 2, 3]), 1, 3) == zmakelvr([0], [1, 2], [3])
assert zoom(zmake([0, 1, 2, 3]), 0, 1) == zmakelvr([], [0], [1, 2, 3])
assert zoom(zmake([0, 1, 2, 3]), 4, 4) == zmakelvr([0, 1, 2, 3], [], [])

def zwrite(z, newval):
  (left, val, right) = zlvr(z)
  return zmakelvr(left, newval, right)

assert [0, 1, 20, 3] == zbottom(zwrite(zoom(zmake([0, 1, 2, 3]), 2, 3), [20]))
assert [100, 1, 2, 3] == zbottom(zwrite(zoom(zmake([0, 1, 2, 3]), 0, 1), [100]))
assert [0, 10, 15, 20, 3] == zbottom(zwrite(zoom(zmake([0, 1, 2, 3]), 1, 3), [10, 15, 20]))

def zisstart(z):
  (x, s, e) = z
  return s == 0 and e == 0

assert zisstart(zleft(zmake([0, 1, 2])))

def zisend(z):
  (x, s, e) = z
  return s == zlen(x) and e == s

assert zisend(zright(zmake([0, 1, 2])))

def zwiden(z, n):
  (x, s, e) = z
  return zcheck(zoom(x, s-n, e+n))

assert zoom(zmake([0, 1, 2, 3, 4]), 1, 4) == zwiden(zoom(zmake([0, 1, 2, 3, 4]), 2, 3), 1)

def zspots(z):
  return [zoom(z, x, x) for x in range(zlen(z) + 1)]

assert [zoom(zmake([1, 2]), 0, 0), zoom(zmake([1, 2]), 1, 1), zoom(zmake([1, 2]), 2, 2)] == zspots(zmake([1, 2]))

def zfindspot(z, p):
  return first(zspots(z), p)

assert (zoom(zmake([-1, 1, 3, 4, 4, 7, 8]), 4, 4) ==
  zfindspot(zmake([-1, 1, 3, 4, 4, 7, 8]), lambda spot: not zisstart(spot) and not zisend(spot) and zval(zleft1(spot)) == zval(zright1(spot))))

# where stuff
# find spot
# is end
# get pair around
# is dedent
# find spot multi

#def zwrites(zs, newvals):
#  assert allsame(map(zbottom, zs))
