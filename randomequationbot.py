from pyparsing import Literal,CaselessLiteral,Word,Combine,Group,Optional,ZeroOrMore,Forward,nums,\
   alphas,And,MatchFirst,Suppress,oneOf
import math
import operator
import random

# some random number generators
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
gen_probs[addsub]=flip_gen(plus,minus)
multdiv=mult|div
gen_probs[multdiv]=filp_gen(mult,div)

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
fnident=oneOf("sin cos tan abs",useRegex=False)
gen_probs[fnident]=
variable=Word("x",exact=1)

# grouping
lparen=Literal("(").suppress()
rparen=Literal(")").suppress()

# assignment
assignment=Literal("=")

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

atom_base_probs=[0.1,0.1,0.3,0.2,0.3]
atom_base_probs_recurselimit=[0.1,0.1,0.4,0.0,0.4]
parenthetical=lparen+expr.suppress()+rparen
atom_part1=optneg+atom_base
atom=atom_part1.setParseAction(pushFirst)|parenthetical.setParseAction(pushUMinus)
atom_probs=[0.5,0.5]
arom_probs_recurselimit=[1.0,0.0]
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

   def generate(root,ntr,out):
      print '-'*10,type(root),id(root),'-'*10
      if type(root) is str:
         print root
         out.append(root)
      elif type(root) is And:
         for expr in root.exprs:
            print type(expr),expr
         for expr in root.exprs:
            out=generate(expr,ntr,out)
      elif type(root) is MatchFirst:
         for expr in root.exprs:
            print type(expr),expr
         expr=random.choice(root.exprs)
         out=generate(expr,ntr,out)
      elif type(root) is Optional:
         print root.expr,root.expr
         if random.randint(0,1):
            out=generate(root.expr,ntr,out)
      elif type(root) is ZeroOrMore:
         #TODO:generate from poisson distribution
         num_times=poission()
         print type(root.expr),root.expr
         for _ in range(num_times):
            out=generate(root.expr,ntr,out)
      elif type(root) is Forward:
         if id(root) not in ntr:
            ntr[id(root)]=0
         if id(root) == id_expr:
            ntr[id(root)]+=1
         print type(root.expr),id(root),root.expr
         if ntr[id(root)] < poission(3):
            out=generate(root.expr,ntr,out)
         else:
            print "ENDING recursion!"
      elif type(root) is Literal or type(root) is CaselessLiteral:
         print root.name
         out=generate(root.name,ntr,out)
      elif type(root) is Suppress:
         print root.expr
         out=generate(root.expr,ntr,out)
      elif type(root) is Combine:
         print root.expr
         out=generate(root.expr,ntr,out)
      elif type(root) is Word:
         print root.initChars
         def choose_number(st):
            char="+"
            while char=="+" or char=="-":
               char=random.choice(root.initChars)
            if random.randint(0,1):
               return char
            else:
               return "-"+char
         out=generate(choose_number(root.initChars),ntr,out)
      else:
         print "Unknown Type: {0}".format(type(root))

      return out


   test("(x-2)+a",{'x':3,'a':5},6)
   test("x-(2+a)",{'x':3,'a':5},-4)

   num_times_recursed={}
#   output = generate(expr.expr,num_times_recursed,[])
#   print output
