from pyparsing import Literal,CaselessLiteral,Word,Combine,Group,Optional,ZeroOrMore,Forward,nums,\
   alphas,And,MatchFirst,Suppress,oneOf
import math
import operator
import random

# some random element generators
def poisson_gen(elem,lmbda):
   def poisson():
      u=random.uniform(0,1)
      x=0
      p=math.exp(-lmbda)
      s=p
      while u>s:
         x+=1
         p=p*lmbda/x
         s+=p
      return [elem]*x

   return poisson

def choose_gen(elems,probs):
   assert len(elems)==len(probs)
   def choose():
      u=random.uniform(0,1)
      cum=0.0
      for i in range(len(elems)):
         cum+=probs[i]
         if u<cum:
            return elems[i]

   return choose

def choose_gen_uni(elems):
   def choose():
      return random.choice(elems)
   return choose

def bernoulli_gen(elems,p):
   assert len(elems)==2
   def flip():
      u=random.uniform(0,1)
      if u<p:
         return elems[0]
      return elems[1]

   return flip

def bernoulli(p):
   return bernoulli_gen([True,False],p)()

def flip_gen(elems):
   return bernoulli_gen(elems,0.5)

def flip():
   return bernoulli(0.5)

def num_gen(chars):
   def choose_number():
      char_list=list(chars)
      char="+"
      while char=="+" or char=="-":
         char=random.choice(char_list)
      if flip():
         return char
      else:
         return "-"+char

   return choose_number



# every optionnal, matchfirst, or ZeroOrMore needs to have a probability defined in this dict
generators={}


# operators
plus=Literal("+")
minus=Literal("-")
mult=Literal("*")
div=Literal("/")
addsub=plus|minus
generators[addsub]=flip_gen([plus,minus])
multdiv=mult|div
generators[multdiv]=flip_gen([mult,div])

expon=Literal("^")

# constants
pi=CaselessLiteral("PI")
e=CaselessLiteral("E")

# numbers and identifiers
point=Literal(".")
# number=Combine(Word("+-"+nums,nums)+Optional(point+Optional(Word(nums)))+\
#    Optional(e+Word("+-"+nums,nums)))
integer=Combine(Word("+-"+nums,nums))
generators[integer.expr]=num_gen(nums)

#ident=Word(alphas,alphas+nums)
fnsin=Literal("sin")
fncos=Literal("cos")
fntan=Literal("tan")
fnabs=Literal("abs")
fnident=fnsin|fncos|fntan|fnabs
generators[fnident]=choose_gen([fnsin,fncos,fntan,fnabs],[0.4,0.4,0.1,0.1])
variable=Literal("x")

# grouping
lparen=Literal("(").suppress()
rparen=Literal(")").suppress()

def pushFirst( strg, loc, toks ):
   exprStack.append( toks[0] )

def pushUMinus( strg, loc, toks ):
   if toks and toks[0]=='-':
      exprStack.append( 'unary -' )

# expressions
expr=Forward()
optneg=Optional("-")
functioncall=fnident+lparen+expr+rparen
atom_base=pi|e|integer|functioncall|variable
atom_base_choices=[pi,e,integer,functioncall,variable]
generators[atom_base]=(choose_gen(atom_base_choices,[0.1,0.1,0.3,0.2,0.3]),\
   choose_gen(atom_base_choices,[0.1,0.1,0.4,0.0,0.4]))
parenthetical=lparen+expr.suppress()+rparen
atom_part1=optneg+atom_base
atom=atom_part1.setParseAction(pushFirst)|parenthetical.setParseAction(pushUMinus)
atom_choices=[atom_part1,parenthetical]
generators[atom]=(choose_gen(atom_choices,[0.5,0.5]),choose_gen(atom_choices,[1.0,0.0]))
# atom = (Optional("-")+(pi|e|number|fnident+lparen+expr+rparen|variable).setParseAction(pushFirst)|\
#    (lparen+expr.suppress()+rparen)).setParseAction(pushUMinus)
# print type(atom1),id(atom1),atom1,len(atom1.exprs)

# for e in atom.exprs:
#    print type(e),id(e),e

# by defining exponentiation as "atom [ ^ factor ]..." instead of "atom [ ^ atom ]...", we get right-to-left exponents, instead of left-to-righ
# that is, 2^3^2 = 2^(3^2), not (2^3)^2.
factor=Forward()
exponfactor=(expon+lparen+factor+rparen).setParseAction(pushFirst)
zom_exponfactor=ZeroOrMore(exponfactor)
generators[zom_exponfactor]=poisson_gen(exponfactor,0.5)
factor << atom + zom_exponfactor

multdivfactor=(multdiv+factor).setParseAction(pushFirst)
zom_multdivfactor=ZeroOrMore(multdivfactor)
generators[zom_multdivfactor]=poisson_gen(multdivfactor,0.5)
term=factor+zom_multdivfactor

addsubterm=(addsub+term).setParseAction(pushFirst)
zom_addsubterm=ZeroOrMore(addsubterm)
generators[zom_addsubterm]=poisson_gen(addsubterm,0.5)
expr << term+zom_addsubterm

id_expr=id(expr)

epsilon = 1e-12
opn = { "+" : operator.add,
        "-" : operator.sub,
        "*" : operator.mul,
        "/" : operator.truediv,
        "^" : operator.pow }
fn  = { "sin" : math.sin,
        "cos" : math.cos,
        "tan" : math.tan,
        "abs" : abs,
        "trunc" : lambda a: int(a),
        "round" : round,
        "sgn" : lambda a: abs(a)>epsilon and cmp(a,0) or 0}
vars=['x']

def evaluateStack( s, assignments ):
   op = s.pop()
   if op == 'unary -':
      return -evaluateStack( s,assignments )
   if op in "+-*/^":
      op2 = evaluateStack( s,assignments )
      op1 = evaluateStack( s,assignments )
      return opn[op]( op1, op2 )
   elif op == "PI":
      return math.pi # 3.1415926535
   elif op == "E":
      return math.e  # 2.718281828
   elif op in fn:
      return fn[op]( evaluateStack( s,assignments ) )
   elif op[0].isalpha():
      if len(op)==1 and op in assignments.keys():
         return assignments[op]
      return 0
   else:
      return float( op )

class GeneratorError(Exception):
  '''Base class for generator errors'''

  @property
  def message(self):
    '''Returns the first argument used to construct this error.'''
    return self.args[0]

def generate(root,recurselimit=1):
   def generate_node(node,recurse,out):
      print '-'*10,type(node),id(node),'-'*10
      if type(node) is str:
         print node
         out.append("".join("".join(node.split('"')).split("'")).lower())
      elif type(node) is And:
         for e in node.exprs:
            print type(e),e
         for e in node.exprs:
            out=generate_node(e,recurse,out)
      elif type(node) is MatchFirst:
         for e in node.exprs:
            print type(e),e
         if node not in generators:
            raise GeneratorError("No MatchFirst found for: {0}".format(e))
         if type(generators[node]) is tuple:
            if recurse<recurselimit:
               e=generators[node][0]()
            else:
               e=generators[node][1]()
         else:
            e=generators[node]()
         out=generate_node(e,recurse,out)
      elif type(node) is Optional:
         print type(node.expr),node.expr
         if flip():
            out=generate_node(node.expr,recurse,out)
      elif type(node) is ZeroOrMore:
         #TODO:generate from poisson distribution
         print type(node.expr),node.expr
         if node not in generators:
            raise GeneratorError("No ZeroOrMore found for: {0}".format(e))
         exprs=generators[node]()
         for e in exprs:
            out=generate_node(e,recurse,out)
      elif type(node) is Forward:
         print type(node.expr),id(node),node.expr
         if id(node)==id_expr:
            recurse+=1
         out=generate_node(node.expr,recurse,out)
      elif type(node) is Literal or type(node) is CaselessLiteral:
         print node.name
         out=generate_node(node.name,recurse,out)
      elif type(node) is Suppress:
         print node.expr
         out=generate_node(node.expr,recurse,out)
      elif type(node) is Combine:
         print node.expr
         out=generate_node(node.expr,recurse,out)
      elif type(node) is Word:
         print node.initChars
         if node not in generators:
            raise GeneratorError("No Word generator found for: {0}".format(node))
         out=generate_node(generators[node](),recurse,out)
      else:
         print "Unknown Type: {0}".format(type(node))

      return out

   return generate_node(root,0,[])


if __name__ == "__main__":

   def test( s, assignments, expVal ):
      global exprStack
      exprStack = []
      results = expr.parseString( s )
      val = evaluateStack( exprStack[:], assignments )
      if val == expVal:
         print s, "=", val, results, "=>", exprStack
      else:
         print s+"!!!", val, "!=", expVal, results, "=>", exprStack

   test("(x-2)+a",{'x':3,'a':5},6)
   test("x-(2+a)",{'x':3,'a':5},-4)

   output = generate(expr.expr)
   output_str="".join(output)
   output_str_pruned="-".join(output_str.split("--"))
   print output_str
   print output_str_pruned
