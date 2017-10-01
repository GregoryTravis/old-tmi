from parsimonious.grammar import Grammar
with open('monious.ebnf', 'r') as f:
  grammar = Grammar(f.read())

print grammar.parse('((bold stuff))')
