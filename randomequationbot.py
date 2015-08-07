import math
import operator
import random

from pyparsing import Literal,CaselessLiteral,Word,Combine,Group,Optional,ZeroOrMore,Forward,nums,\
   alphas,And,MatchFirst,Suppress,oneOf

from imagebot import ImageBot
import duration

import requests

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
      neg=""
      if flip():
         neg="~"
      return "{0}{1}".format(neg,char)

   return choose_number

def passthrough_gen(elem):
   def passthrough():
      return elem
   return passthrough

class EquationGrammar(object):
   def __init__(self):
      self._expr_stack=[]

      # every optional, matchfirst, or ZeroOrMore needs to have a probability defined in this dict
      self._generators={}

      # operators
      plus=Literal("+")
      minus=Literal("-")
      mult=Literal("*")
      div=Literal("/")
      addsub=plus|minus
      self._generators[addsub]=flip_gen([plus,minus])
      multdiv=mult|div
      self._generators[multdiv]=flip_gen([mult,div])

      # constants
      pi=CaselessLiteral("PI")
      e=CaselessLiteral("E")

      # numbers and identifiers
      point=Literal(".")
      integer=Combine(Word("+-"+nums,nums))
      self._generators[integer.expr]=num_gen(nums)

      #ident=Word(alphas,alphas+nums)
      fnsin=Literal("sin")
      fncos=Literal("cos")
      fntan=Literal("tan")
      fnabs=Literal("abs")
      fnident=fnsin|fncos|fntan|fnabs
      self._generators[fnident]=choose_gen([fnsin,fncos,fntan,fnabs],[0.4,0.4,0.1,0.1])
      variable=Literal("x")

      # grouping
      lparen=Literal("(").suppress()
      rparen=Literal(")").suppress()

      # expressions
      self._expr=Forward()
      optneg=Optional("-")
      self._generators[optneg]=flip_gen(["-",None])
      functioncall=fnident+lparen+self._expr+rparen
      atom_base=pi|e|integer|functioncall|variable
      atom_base_choices=[pi,e,integer,functioncall,variable]
      self._generators[atom_base]=(choose_gen(atom_base_choices,[0.1,0.1,0.2,0.3,0.3]),\
         choose_gen(atom_base_choices,[0.1,0.1,0.4,0.0,0.4]))
      parenthetical=lparen+self._expr.suppress()+rparen
      atom=(optneg+atom_base.setParseAction(self.push_first)|parenthetical).setParseAction(\
         self.push_uminus)
      atom_choices=[atom_base,parenthetical]
      self._generators[atom]=(choose_gen(atom_choices,[0.5,0.5]),choose_gen(atom_choices,[1.0,0.0]))

      # by defining exponentiation as "atom [ ^ factor ]..." instead of "atom [ ^ atom ]...", we get
      # right-to-left exponents, instead of left-to-right
      # that is, 2^3^2 = 2^(3^2), not (2^3)^2.
      factor=Forward()
      # parse either curly braces or parenthesis, but only generate curly braces
      expon=Literal("^")
      exponlparen=Literal("{").suppress()
      exponrparen=Literal("}").suppress()
      mf_lparen=exponlparen|lparen
      lparen_choices=[exponlparen,lparen]
      self._generators[mf_lparen]=choose_gen(lparen_choices,[1.0,0.0])
      mf_rparen=exponrparen|rparen
      rparen_choices=[exponrparen,rparen]
      self._generators[mf_rparen]=choose_gen(rparen_choices,[1.0,0.0])

      exponfactor=(expon+mf_lparen.suppress()+factor+mf_rparen.suppress()).setParseAction(\
         self.push_first)
      zom_exponfactor=ZeroOrMore(exponfactor)
      self._generators[zom_exponfactor]=poisson_gen(exponfactor,0.2)
      factor << atom + zom_exponfactor

      multdivfactor=(multdiv+factor).setParseAction(self.push_first)
      zom_multdivfactor=ZeroOrMore(multdivfactor)
      self._generators[zom_multdivfactor]=poisson_gen(multdivfactor,0.5)
      term=factor+zom_multdivfactor

      addsubterm=(addsub+term).setParseAction(self.push_first)
      zom_addsubterm=ZeroOrMore(addsubterm)
      self._generators[zom_addsubterm]=poisson_gen(addsubterm,0.5)
      self._expr << term+zom_addsubterm

      self._id_expr=id(self._expr)

   def set_expr_stack(self,expr_stack):
      self._expr_stack=expr_stack

   def push_first(self,strg,loc,toks):
      self._expr_stack.append(toks[0])

   def push_uminus(self,strg,loc,toks):
      if toks and toks[0]=='-':
         self._expr_stack.append('unary -')

   def parse_string(self,s):
      return self._expr.parseString(s)

   @property
   def generators(self):
       return self._generators

   @property
   def id_expr(self):
       return self._id_expr

   @property
   def expr_stack(self):
       return self._expr_stack

   @property
   def root(self):
       return self._expr

class EquationEvaluator(object):
   def __init__(self,grammar=EquationGrammar()):
      self._expr_stack=[]
      self._grammar=grammar
      self._epsilon = 1e-12
      self._opn = { "+" : operator.add,
                    "-" : operator.sub,
                    "*" : operator.mul,
                    "/" : operator.truediv,
                    "^" : operator.pow }
      self._fn  = { "sin" : math.sin,
                    "cos" : math.cos,
                    "tan" : math.tan,
                    "abs" : abs,
                    "trunc" : lambda a: int(a),
                    "round" : round,
                    "sgn" : lambda a: abs(a)>self._epsilon and cmp(a,0) or 0}

   def evaluate_stack(self,s,assignments):
      op=s.pop()
      if op == 'unary -':
         return -self.evaluate_stack(s,assignments)
      if op in "+-*/^":
         op2=self.evaluate_stack(s,assignments)
         op1=self.evaluate_stack(s,assignments)
         return self._opn[op](op1,op2)
      elif op == "PI":
         return math.pi # 3.1415926535
      elif op == "E":
         return math.e  # 2.718281828
      elif op in self._fn:
         return self._fn[op](self.evaluate_stack(s,assignments))
      elif op[0].isalpha():
         if len(op)==1 and op in assignments.keys():
            return assignments[op]
         return 0
      else:
         return float(op)

   def evaluate(self,string,assignments):
      self._expr_stack=[]
      self._grammar.set_expr_stack(self._expr_stack)
      results=self._grammar.parse_string(string)
      val=self.evaluate_stack(self._expr_stack[:],assignments)
      return results,val

   def evaluate_print(self,s,assignments):
      results,val=self.evaluate(s,assignments)
      print s,
      for k in assignments:
         print "{0}={1}".format(k,assignments[k]),
      print val
      print results,self._expr_stack

   def test(self,s,assignments,expected):
      results,val=self.evaluate(s,assignments)
      if val == float(expected):
         print s, "=", val, results, "=>", self._expr_stack
      else:
         print s+"!!!", val, "!=", expected, results, "=>", self._expr_stack

   @property
   def grammar(self):
       return self._grammar

class GeneratorError(Exception):
  '''Base class for generator errors'''

  @property
  def message(self):
    '''Returns the first argument used to construct this error.'''
    return self.args[0]

class EquationGenerator(object):
   def __init__(self,evaluator=EquationEvaluator()):
      self._evaluator=evaluator
      self._generators=self._evaluator.grammar.generators
      self._grammar=self._evaluator.grammar

   def generate(self,root,recurselimit=2,debug=False):
      def generate_node(node,recurse,out):
         if debug:
            print '-'*10,type(node),id(node),'-'*10
         if type(node) is str:
            if debug:
               print node
            out.append("".join("".join(node.split('"')).split("'")).lower())
         elif type(node) is And:
            if debug:
               for e in node.exprs:
                  print type(e),e
            for e in node.exprs:
               out=generate_node(e,recurse,out)
         elif type(node) is MatchFirst:
            if debug:
               for e in node.exprs:
                  print type(e),e
            if node not in self._generators:
               raise GeneratorError("No MatchFirst found for: {0}".format(e))
            if type(self._generators[node]) is tuple:
               if recurse<recurselimit:
                  e=self._generators[node][0]()
               else:
                  e=self._generators[node][1]()
            else:
               e=self._generators[node]()
            out=generate_node(e,recurse,out)
         elif type(node) is Optional:
            if debug:
               print type(node.expr),node.expr
            e=self._generators[node]()
            if e:
               # translate unary minus to ~
               if e=="-":
                  out=generate_node("~",recurse,out)
               else:
                  out=generate_node(e,recurse,out)
         elif type(node) is ZeroOrMore:
            if debug:
               print type(node.expr),node.expr
            if node not in self._generators:
               raise GeneratorError("No ZeroOrMore found for: {0}".format(e))
            exprs=self._generators[node]()
            for e in exprs:
               out=generate_node(e,recurse,out)
         elif type(node) is Forward:
            if debug:
               print type(node.expr),id(node),node.expr
            if id(node)==self._grammar.id_expr:
               recurse+=1
            out=generate_node(node.expr,recurse,out)
         elif type(node) is Literal or type(node) is CaselessLiteral:
            if debug:
               print node.name
            out=generate_node(node.name,recurse,out)
         elif type(node) is Suppress:
            if debug:
               print node.expr
            out=generate_node(node.expr,recurse,out)
         elif type(node) is Combine:
            if debug:
               print node.expr
            out=generate_node(node.expr,recurse,out)
         elif type(node) is Word:
            if debug:
               print node.initChars
            if node not in self._generators:
               raise GeneratorError("No Word generator found for: {0}".format(node))
            out=generate_node(self._generators[node](),recurse,out)
         else:
            raise GeneratorError("Unknown Type: {0}".format(type(node)))

         return out

      return generate_node(self._grammar.root,0,[])

   @classmethod
   def translate(cls,s,replacements):
      for find,replace in replacements:
         s=replace.join(s.split(find))
      return s

   @classmethod
   def tokens_to_base_string(cls,tokens):
      # remove double negative
      return cls.translate("".join(tokens),[("-~","+")])

   @classmethod
   def base_to_latex(cls,base):
      # changes asterisk to dots
      return cls.translate(base,[("~","-"),("*","{\cdot}")])

   @classmethod
   def tokens_to_latex_string(cls,tokens):
      return cls.base_to_latex(cls.tokens_to_base_string(tokens))

   @classmethod
   def base_to_parseable(cls,base):
      return cls.translate(base,[("~","-"),("{","("),("}",")")])

   @classmethod
   def tokens_to_parseable_string(cls,tokens):
      return cls.base_to_parseable(cls.tokens_to_base_string(tokens))

   def generate_valid(self,min_token_length=1,max_token_length=100,\
         min_string_length=1,max_string_length=135):
      valid=False
      while not valid:
         tokens=self.generate(self._grammar.root)
         valid=True
         s=self.tokens_to_parseable_string(tokens)
         if len(s)<min_string_length or len(s)>max_string_length or \
               len(tokens)<min_token_length or len(tokens)>max_token_length:
            valid=False
            continue
         try:
            for x in [float(x)/100. for x in range(-1000,1000)]:
               self._evaluator.evaluate(s,{'x':x})
         except Exception,ex:
            valid=False
      return tokens

class RandomEquationBot(ImageBot):
   def __init__(self,*args,**kwargs):
      super(RandomEquationBot,self).__init__(*args,**kwargs)
      self._eval=EquationEvaluator()
      self._gen=EquationGenerator(self._eval)

   def generate(self):
      output=self._gen.generate_valid(min_token_length=4)
      print self._gen.tokens_to_latex_string(output)
      filename=self.generate_image(self._gen.tokens_to_latex_string(output))
      message="f(x)={0}".format(self._gen.tokens_to_parseable_string(output))
      return (filename,message)

   def generate_image(self,image_text):
      url='http://chart.apis.google.com/chart'
      params={'cht':'tx','chs':50,'chl':image_text}
      r = requests.get(url,params=params)

      filename=None
      if r.status_code not in [200]:
         print "Unable to generate image! Status code: {0}".format(r.status_code)
      else:
         filename="eq.png"
         with open(filename, 'wb') as fd:
          for chunk in r.iter_content(1024):
              fd.write(chunk)

      return filename

def test():
   #TODO: make a unit test with some of this(and more)
   evaluator=EquationEvaluator()
   generator=EquationGenerator(evaluator)

   evaluator.test("(x-2)+x",{'x':3},4)
   evaluator.test("x-(2+x)",{'x':3},-2)
   evaluator.test("x*x^(x^(2))",{'x':5},1490116119384765625)
   output=generator.generate_valid(min_token_length=4)
   output_str_base=generator.tokens_to_base_string(output)
   print output_str_base, "len={0}".format(len(output_str_base))

   output_str_parseable=generator.base_to_parseable(output_str_base)
   print output_str_parseable
   evaluator.evaluate_print(output_str_parseable,{'x':5})

   output_str_latex=generator.base_to_latex(output_str_base)
   print output_str_latex

if __name__ == "__main__":
   bot=RandomEquationBot(oauth_config_file="randomeqbot.oauth",\
      period_between_tweets=duration.Duration(hours=3))
   print bot.run()

   # test()
