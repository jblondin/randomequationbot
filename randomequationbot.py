from pyparsing import Literal,CaselessLiteral,Word,Combine,Group,Optional,ZeroOrMore,Forward,nums,\
   alphas,And,MatchFirst,Suppress,oneOf
import math
import operator
import random

# some random element generators
def poission_gen(elem,lmbda):
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
      for i in range(len(lst)):
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
      return elem[1]

   return flip

def flip_gen(elems):
   return bernoulli_gen(elems,0.5)

# every optionnal, matchfirst, or ZeroOrMore needs to have a probability defined in this dict
gen_probs={}


# operators
plus=Literal("+")
minus=Literal("-")
mult=Literal("*")
div=Literal("/")
addsub=plus|minus
gen_probs[addsub]=flip_gen([plus,minus])
multdiv=mult|div
gen_probs[multdiv]=filp_gen([mult,div])

expon=Literal("^")

# constants
pi=CaselessLiteral("PI")
e=CaselessLiteral("E")

# numbers and identifiers
point=Literal(".")
# number=Combine(Word("+-"+nums,nums)+Optional(point+Optional(Word(nums)))+\
#    Optional(e+Word("+-"+nums,nums)))
integer=Combine(Word("+-"+nums,nums))
#ident=Word(alphas,alphas+nums)
fnsin=Literal("sin")
fncos=Literal("cos")
fntan=Literal("tan")
fnabs=Literal("abs")
fnident=fnsin|fncos|fntan|fnabs
gen_probs[fnident]=choosen_gen([fnsin,fncos,fntan,fnabs],[0.4,0.4,0.1,0.1])
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
gen_probs[atom_base]=([0.1,0.1,0.3,0.2,0.3],[0.1,0.1,0.4,0.0,0.4])
parenthetical=lparen+expr.suppress()+rparen
atom_part1=optneg+atom_base
atom=atom_part1.setParseAction(pushFirst)|parenthetical.setParseAction(pushUMinus)
gen_probs[atom]=([0.5,0.5],[1.0,0.0])
# atom = (Optional("-")+(pi|e|number|fnident+lparen+expr+rparen|variable).setParseAction(pushFirst)|\
#    (lparen+expr.suppress()+rparen)).setParseAction(pushUMinus)
print type(atom),id(atom),atom,len(atom.exprs)
# print type(atom1),id(atom1),atom1,len(atom1.exprs)
e1=atom.exprs[0]
print type(e1),id(e1),e1

# for e in atom.exprs:
#    print type(e),id(e),e

# by defining exponentiation as "atom [ ^ factor ]..." instead of "atom [ ^ atom ]...", we get right-to-left exponents, instead of left-to-righ
# that is, 2^3^2 = 2^(3^2), not (2^3)^2.
factor = Forward()
factor << atom + ZeroOrMore( ( expon + factor ).setParseAction( pushFirst ) )

term = factor + ZeroOrMore( ( multdiv + factor ).setParseAction( pushFirst ) )
expr << term + ZeroOrMore( ( addsub + term ).setParseAction( pushFirst ) )

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


def generate(root):
   def generate_node(node,recurse,out):
      print '-'*10,type(node),id(node),'-'*10
      if type(node) is str:
         print node
         out.append(node)
      elif type(node) is And:
         for e in node.exprs:
            print type(e),e
         for e in node.exprs:
            out=generate_node(e,recurse,out)
      elif type(node) is MatchFirst:
         for e in node.exprs:
            print type(e),e
         e=random.choice(node.exprs)
         out=generate_node(e,recurse,out)
      elif type(node) is Optional:
         print type(node.expr),node.expr
         if random.randint(0,1):
            out=generate_node(node.expr,recurse,out)
      elif type(node) is ZeroOrMore:
         #TODO:generate from poisson distribution
         num_times=poission()
         print type(node.expr),node.expr
         for _ in range(num_times):
            out=generate_node(node.expr,recurse,out)
      elif type(node) is Forward:
         print type(node.expr),id(node),node.expr
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
         def choose_number(st):
            char="+"
            while char=="+" or char=="-":
               char=random.choice(node.initChars)
            if random.randint(0,1):
               return char
            else:
               return "-"+char
         out=generate_node(choose_number(node.initChars),recurse,out)
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

   output = generate(expr.expr,0,[])
#   print output
