# ===================================================================#
#   Imports and globals
# ===================================================================#

import re
import copy
import datetime

__all__ = ['GlobalLogicParser', 'LogFunction', 'LogPredicate',
           'LogSentence',]

GL_PCONDS = [':icond:', ':implies:', ':equiv:']
SYMBS = dict([
               ('|>',':icond:'),
               ('<=>',':equiv:'), 
               (' =>',':implies:'),
               ('||',':or:'),
               ('&&',':and:')
             ])
SYMB_ORD = ['|>', '<=>', ' =>', '||', '&&']

##### Regex
rgx_par = re.compile(r'\{(.*?)\}')
rgx_ob = re.compile(r'\b(.*?)\]')
rgx_br = re.compile(r'\}(.*?)\{')

# ===================================================================#
#   LOGIC SENTENCE PARSER
# ===================================================================#

def GlobalLogicParser(sent):
    """Takes a string, discovers type and returns the corresponding
    object.
    
    This is a global parsing function for logic functions.
    """    
    ori, comp, hier = parse_sent(sent)
    par_form = comp[ori]
    if not ':vars:' in par_form:
        if '[' in par_form and len(comp) == 1:
            # It's an atom predicate
            pred = par_form.replace(' ','')
            sets = rgx_ob.findall(pred)
            sets = sets[0].split('[')
            if ';' in sets[1]:
                sets[1] = sets[1].split(';')
            if '<' in pred[0]:
                # Is a function declaration -> implies a relation
                # between different objects or classes.           
                return make_function(pred, 'relation')
            else:
                # Is a membership declaration -> the object(s) belong(s) 
                # to a set of objects.
                if isinstance(sets[1], list):
                    facts = []
                    for e in sets[1]:
                        facts.append(make_fact([sets[0],e], 'grounded_term'))
                    return facts
                else:
                    return make_fact(sets, 'grounded_term')
        elif any(symb in par_form for symb in GL_PCONDS):
            # It's a complex sentence with various predicates/funcs
            sent = make_logic_sent(ori, comp, hier)
            if sent.validity is True or sent.validity is None:
                del sent.validity
                return sent
            else:
                msg = "Illegal connectives used in the consequent " \
                    + " of an indicative conditional sentence."
                raise AssertionError(msg)
        else:
            msg = "No indicative conditional, implication or " \
            "equality found."
            raise AssertionError(msg)
    else:
        # It's a complex sentence with variables
        sent = make_logic_sent(ori, comp, hier)
        return sent

def parse_sent(sent):
    """Parser for logic sentences."""

    def decomp_par(s, symb, f=0):
        initpar = []
        endpar = []
        idx = 0
        while idx < len(s):
            if s[idx] == symb[0]:
                initpar.append(idx)
            elif s[idx] == symb[1]:
                endpar.append(idx)
            idx += 1
        min_ = float('inf')
        for i in initpar:
            for e in endpar:
                diff = abs(e - i)
                if diff < min_ and i < e:
                    min_ = diff
                    par = (i, e)
        if len(initpar) == 0 and len(endpar) == 0:
            comp.append(s[:])
            s = s.replace(s[:], '{'+str(f)+'}')
            return
        elif (len(initpar) == 0 and len(endpar) != 0) or \
             (len(initpar) != 0 and len(endpar) == 0):
            raise AssertionError('Odd number of parentheses.')
        else:
            elem = s[par[0]+1:par[1]]
            comp.append(elem)
            s = s.replace(s[par[0]:par[1]+1], '{'+str(f)+'}')
            f += 1
            return decomp_par(s, symb, f)

    def decomp_symbs():
        symb = [x for x in SYMB_ORD if x in form][0]
        memb = form.split(symb)
        if len(memb) > 2:
            while len(memb) > 2:
                last = memb.pop()                        
                memb[-1] =  memb[-1] + symb + last
        x, y = len(comp), len(comp)+1            
        comp[idx] = '{'+str(x)+'}'+SYMBS[symb]+'{'+str(y)+'}'
        comp.append(memb[0])
        comp.append(memb[1])
        return True
    
    def iter_childs():
        for n in range(0, ls):
            exp = comp[n]
            childs = rgx_par.findall(exp)
            childs = [int(x) for x in childs]
            if childs != []:
                hier[n] = {'childs': childs, 'parent': -1}
            else:
                hier[n] = {'childs': -1, 'parent': -1}
        for n in range(0, ls):
            childs = hier[n]['childs']
            if childs != -1:
                for c in childs:
                    hier[c]['parent'] = n
    
    comp = []
    hier = {}
    decomp_par(sent.rstrip('\n'), symb=('(', ')'))
    ori = len(comp) - 1
    for idx, form in enumerate(comp):            
        if any(symb in form for symb in SYMBS.keys()):
            decomp_symbs()
    ls = len(comp)
    iter_childs()
    return ori, comp, hier


class LogSentence(object):
    """Object to store a first-order logic complex sentence.

    This sentence is the result of parsing a sentence and encode
    it in an usable form for the agent to classify and reason about
    objects and relations, cannot be instantiated directly.
    
    It's callable when instantiated, accepts as arguments:
    1) the working knowledge-base
    2) n strins which will subsitute the variables in the sentence
       or a list of string.
    """
    def __init__(self):
        self.depth = 0
        self.var_order = []
        self.particles = []
    
    def __call__(self, ag, *args):
        if type(args[0]) is tuple or type(args[0] is list):
            args = args[0]
        # Clean up previous results.
        self.assigned = {}
        self.cln_res()
        if len(self.var_order) == len(args):
            # Check the properties/classes an obj belongs to
            for n, const in enumerate(args):
                if const not in ag.individuals:
                    return
                var_name = self.var_order[n]
                # Assign an entity to a variable by order.
                self.assigned[var_name] = const
            ag.bmsWrapper.register(self)
            self.start.solve(self, ag, key=[0])
        elif len(self.var_order) == 0:
            ag.bmsWrapper.register(self)
            self.start.solve(self, ag, key=[0])
        else: return

    def get_ops(self, p, chk_op=[':or:', ':implies:', ':equiv:']):
        ops = []
        for p in self:
            if any(x in p.cond for x in chk_op):
                ops.append(p)
        for p in ops:
            x = p
            while x.cond != ':icond:' or x.parent == -1:
                if x.parent.cond == ':icond:' and x.parent.next[1] == x:
                    return False
                else:
                    x = x.parent
        return True

    def get_pred(self, branch='l', conds=GL_PCONDS):
        preds = []
        for p in self:
            if p.cond == ':predicate:':
                preds.append(p)
        res = []
        for p in preds:
            x = p
            while x.parent.cond not in conds:
                x = x.parent
            if branch == 'l' and x.parent.next[0] == x:
                res.append(p.pred)
            elif branch != 'l' and x.parent.next[1] == x:
                res.append(p.pred)
        return res
    
    def cln_res(self):
        for p in self.particles:
            p.results = []

    def __iter__(self):
        return iter(self.particles)

def make_logic_sent(ori, comp, hier):
    """Takes a parsed FOL sentence and creates an object with
    the embedded methods to resolve it.
    """
    
    class Particle(object):
        """This is the base class to create logic particles
        which pertain to a given sentence.
        
        Keys for solving proofs:
        100: Substitute a child's predicates.
        101: Check the truthiness of a child atom.
        103: Return to parent atom.
        """
        def __init__(self, cond, depth, id_, parent, syb, *args):
            self.pID = id_
            self.depth = depth
            self.cond = cond
            self.next = syb
            self.parent = parent
            self.results = []
            if cond == ':predicate:':
                self.pred = args[0]
    
        def __str__(self):
            if self.cond != ':predicate:':
                s = '<operator ' + ' (depth:' + str(self.depth) + ') "' \
                + str(self.cond) + '">'
            else:
                s = '<predicate ' + ' (depth:' + str(self.depth) + '): ' \
                + str(self.pred) + '>'
            return s
        
        def connect(self, part_list):
            for x, child in enumerate(self.next):
                for part in part_list:
                    if part.pID == child:
                        self.next[x] = part
                        self.next[x].parent = self
        
        def returning_value(self, proof, ag, key, truth):
            self.results.append(truth)
            self.solve(proof, ag, key)
    
    class LogicIndCond(Particle):
    
        def solve(self, proof, ag, key, *args):            
            #print(self, '// Key:'+str(key), '// Args:', args)
            if key[-1] == 103 and self.parent == -1: return
            current, next_ = len(self.results), None 
            if current == 0:
                key.append(101)
                next_ = True
            elif current == 1 and self.results[0] is True:
                key.append(100)
                next_ = True
            elif current == 1 and self.results[0] is False:
                # The left branch was false, so do not continue.
                if hasattr(proof, 'result') is False: 
                    proof.result = False
                result = False
                key.append(103)
            else:
                # Substitution failed.
                result = None
                key.append(103)
            if self.parent != -1 and next_ is None:
                self.parent.solve(proof, ag, key, result)
            elif next_ is True:
                self.next[current].solve(proof, ag, key)
    
    class LogicEquivalence(Particle):
    
        def solve(self, proof, ag, key, *args):            
            #print(self, '// Key:'+str(key), '// Args:', args)
            if key[-1] == 103 and self.parent == -1: return
            current, next_ = len(self.results), None 
            if current == 0:
                key.append(101)
                next_ = True
            elif current == 1 and self.results[0] is not None:
                # If it's not a predicate, follow standard FOL
                # rules for equiv
                if self.next[1].cond != ':predicate:':
                    key.append(101)
                else:
                    key.append(103)
                next_ = True
            elif current > 1:
                # The second term of the implication was complex
                # check the result of it's substitution
                if self.results[1] is None:
                    result = None
                elif self.results[0] == self.results[1]:
                    proof.result, result = True, True
                else:
                    if hasattr(proof, 'result') is False:
                        proof.result = False
                    result = False
                key.append(103)
            else:
                # Not known solution.      
                result = None
                key.append(103)
            if self.parent != -1 and next_ is None:
                self.parent.solve(proof, ag, key, result)
            elif next_ is True:
                self.next[current].solve(proof, ag, key)
    
    class LogicImplication(Particle):
    
        def solve(self, proof, ag, key, *args):            
            #print(self, '// Key:'+str(key), '// Args:', args)
            if key[-1] == 103 and self.parent == -1: return
            current, next_ = len(self.results), None
            if current == 0:
                key.append(101)
                next_ = True
            elif current == 1 and self.results[0] is not None:
                # If it's not a predicate, follow standard FOL
                # rules for implication
                if self.next[1].cond != ':predicate:': key.append(101)
                elif self.results[0] is True: key.append(100)
                next_ = True
            elif current > 1:
                # The second term of the implication was complex
                # check the result of it's substitution
                if (self.results[0] and self.results[1]) is None:
                    result = None
                elif self.results[0] is True and self.results[1] is False:
                    proof.result, result = False, False
                else:
                    if hasattr(proof, 'result') is False:
                        proof.result = True
                    result = True
                key.append(103)
            else:
                # Not known solution.
                key.append(103)
                result = None
            if self.parent != -1 and next_ is None:
                self.parent.solve(proof, ag, key, result)
            elif next_ is True:
                self.next[current].solve(proof, ag, key)
    
    class LogicConjunction(Particle):
    
        def solve(self, proof, ag, key, *args):
            #print(self, '// Key:'+str(key), '// Args:', args)
            if key[-1] == 103 and self.parent == -1: return
            current = len(self.results)
            if key[-1] == 103 and len(self.next) >= 2:
                self.parent.solve(proof, ag, key)
            elif key[-1] == 103:
                key.pop()
            elif key[-1] == 101:
                if current < len(self.next):
                    key.append(101)
                    self.next[current].solve(proof, ag, key)
                else: self.test(proof, ag, key)
            elif key[-1] == 100:
                if current < len(self.next) and \
                self.next[current].cond == ':predicate:':
                    key.append(103)
                    self.next[current].solve(proof, ag, key)
                elif current < len(self.next):
                    self.next[current].solve(proof, ag, key)
                else:
                    # All substitutions done
                    key.append(103)
                    self.parent.solve(proof, ag, key)
        
        def test(self, proof, ag, key):
            left_branch, right_branch = self.results[0], self.results[1]
            if key[-1] == 101:
                # Two branches finished, check if both are true.
                if (left_branch and right_branch) is None: result = None           
                elif left_branch == right_branch and left_branch is True:
                    result = True       
                else: result = False
                if self.parent != -1: 
                    self.parent.returning_value(proof, ag, key, result)
                else: proof.result = result
            elif key[-1] == 100:
                # Test if this conjunction fails
                if (left_branch and right_branch) is None: return None
                elif left_branch == right_branch and left_branch is True:
                    result = True
                else: return False
                proof.result = result
    
    class LogicDisjunction(Particle):
    
        def solve(self, proof, ag, key, *args):
            #print(self, '// Key:'+str(key), '// Args:', args)
            if key[-1] == 103 and self.parent == -1: return
            current = len(self.results)
            if key[-1] == 103 and len(self.next) >= 2:
                self.parent.solve(proof, ag, key)
            elif key[-1] == 103:
                key.pop()
            elif key[-1] == 101:
                if current < len(self.next):
                    key.append(101)
                    self.next[current].solve(proof, ag, key)
                else: self.test(proof, ag, key)
            elif key[-1] == 100:
                if current < len(self.next) and \
                self.next[current].cond == ':predicate:':
                    key.append(103)
                    self.next[current].solve(proof, ag, key)
                elif current < len(self.next):
                    self.next[current].solve(proof, ag, key)
                else:
                    # All substitutions done
                    key.append(103)
                    self.parent.solve(proof, ag, key)
        
        def test(self, proof, ag, key):
            left_branch, right_branch = self.results[0], self.results[1]
            if key[-1] == 101:
                # Two branches finished, check if both are true.            
                if (left_branch and right_branch) is None: result = None
                elif left_branch != right_branch or \
                (left_branch and right_branch) is True: 
                    result = True         
                else: result = False
                if self.parent != -1: 
                    self.parent.returning_value(proof, ag, key, result)
                else: proof.result = result
            elif key[-1] == 100:
                # Test if this disjunction fails
                if (left_branch and right_branch) is None: return None
                elif left_branch != right_branch or \
                (left_branch and right_branch) is True: 
                    result = True
                else: return False
                proof.result = result
    
    class LogicAtom(Particle):
    
        def solve(self, proof, ag, key, *args):
            #print(self, '// Key:'+str(key), '// Args:', args)
            if key[-1] == 103 and self.parent == -1: return   
            result = self.test(proof, ag, key)
            x = key.pop()
            if x != 100:
                self.parent.returning_value(proof, ag, key, result)
        
        def test(self, proof, ag, key):
            
            def isvar(s):
                try: s = proof.assigned[s]
                except KeyError: pass
                return s
            
            if key[-1] == 101:
                if issubclass(self.pred.__class__, LogFunction):
                    # Check funct between a set/entity and other set/entity.
                    result = None
                    args = self.pred.get_args()
                    for x, arg in enumerate(args):
                        if arg in proof.assigned:
                            args[x] = proof.assigned[arg]
                    test = self.pred.substitute(args)
                    if '$' in args[0][0]:
                        result = ag.individuals[args[0]].test_rel(test)
                    else:
                        result = ag.classes[args[0]].test_rel(test)
                    if result is True:
                        ag.bmsWrapper.prev_blf('PLACEHOLDER')
                else:
                    # Check membership to a set of an entity.
                    sbj = isvar(self.pred.term)
                    if '$' not in sbj[0]: categs = ag.classes[sbj].get_parents()
                    else: categs = ag.individuals[sbj].get_cat()
                    # If is True, then the object belongs to the set.
                    # Else, must be False, and the object doesn't belong.
                    result = None
                    if self.pred.parent in categs:
                        val = categs[self.pred.parent]
                        test = self.pred.substitute(sbj, val)
                        if self.pred == test: result = True                        
                        else: result = False
                    if result is True:
                        ag.bmsWrapper.prev_blf('PLACEHOLDER')
                return result
            else:
                # marked for declaration
                # subtitute var(s) for constants
                # and pass to agent for updating
                if issubclass(self.pred.__class__, LogFunction):                
                    args = self.pred.get_args()
                    for x, arg in enumerate(args):
                        if arg in proof.assigned:
                            args[x] = proof.assigned[arg]
                    pred = self.pred.substitute(args)
                    ag.bmsWrapper.check('PLACEHOLDER')
                    ag.up_rel(pred)
                else:
                    sbj = isvar(self.pred.term)
                    pred = self.pred.substitute(sbj, val=None)
                    ag.bmsWrapper.check('PLACEHOLDER')
                    ag.up_memb(pred)  
                if key[-1] == 100 and hasattr(proof, 'result'):
                    proof.result.append(pred)
                elif key[-1] == 100:
                    proof.result = [pred]
    
    def make_parts(ori, comp, hier, depth=0):
        form = comp[ori]
        childs = hier[ori]['childs']
        parent = hier[ori]['parent']
        new_atom(form, depth, parent, ori, childs)
        depth += 1
        for child in childs:
            syb = hier[child]['childs']
            if syb != -1:
                make_parts(child, comp, hier, depth)
            else:
                form = comp[child]
                parent = hier[child]['parent']
                new_atom(form, depth, parent, child, syb=[-1])
    
    def new_atom(form, depth, parent, part_id, syb):
        form = form.replace(' ','').strip()
        cond = rgx_br.findall(form)
        if depth > sent.depth:
            sent.depth = depth
        if len(cond) > 0:
            type_ = cond[0]
            if type_ == ':icond:':                
                l = LogicIndCond(type_, depth, part_id, parent, syb)
            elif type_ == ':equiv:':
                l = LogicEquivalence(type_, depth, part_id, parent, syb)
            elif type_ == ':implies:':
                l = LogicImplication(type_, depth, part_id, parent, syb)
            elif type_ == ':and:':
                l = LogicConjunction(type_, depth, part_id, parent, syb)
            elif type_ == ':or:':
                l = LogicDisjunction(type_, depth, part_id, parent, syb)            
            sent.particles.append(l)
        elif any(x in form for x in [':vars:', ':exists:']):
            form = form.split(':')
            cond = ':stub:'
            for i, a in enumerate(form):
                if a == 'vars':
                    vars_ = form[i+1].split(',')
                    for var in vars_:
                        if var not in sent.var_order:
                            sent.var_order.append(var)
                    p = Particle(cond, depth, part_id, parent, syb, form)
                    sent.particles.append(p)
        elif '[' in form:
            cond = ':predicate:'
            if '<' in form:
                form = make_function(form, 'relation')
            else:
                form = make_fact(form, 'free_term')
            p = LogicAtom(cond, depth, part_id, parent, syb, form)
            sent.particles.append(p)
        else:
            cond = ':stub:'
            p = Particle(cond, depth, part_id, parent, syb, form)
            sent.particles.append(p)
    
    def connect_parts():
        particles = []
        icond = False
        lvl = sent.depth
        while lvl > -1:
            p = [part for part in sent.particles if part.depth == lvl]
            for part in p:
                particles.append(part)
                if part.cond == ':icond:':
                    icond = part
            lvl -= 1
        sent.particles = particles
        for p in sent.particles:
            p.connect(sent.particles)
        # Check for illegal connectives for implicative cond sentences
        sent.validity = None
        if icond is not False:
            sent.validity = sent.get_ops(icond)
        for p in sent.particles:
            del p.pID
            p.sent = sent
            p.results = []
            if p.parent == -1:
                sent.start = p
        for p in iter(p for p in sent.particles if p.cond == ':stub:'):
            for e in p.next:
                e.depth = p.depth
                e.parent = p.parent
                if hasattr(sent, 'start') and sent.start is p:
                    sent.start = e
            del p
    
    sent = LogSentence()
    make_parts(ori, comp, hier,)
    connect_parts()
    return sent

# ===================================================================#
#   LOGIC CLASSES AND SUBCLASSES
# ===================================================================#

        
class LogPredicate(object):
    """Base class to represent a ground predicate."""
    types = ['grounded_term', 'free_term']    
    def __init__(self, pred):
        if type(pred) is str:
            pred = pred.replace(' ','')
            pred = rgx_ob.findall(pred)
            pred = pred[0].split('[')
        val = pred[1].split(',')
        op, val[1] = val[1][1], float(val[1][2:])
        if (val[1] > 1 or val[1] < 0):
            m = "Illegal value: {0}, must be > 0, or < 1.".format(val[1])
            raise AssertionError(m)
        dates = None
        if len(val) >= 3:
            for arg in val[2:]:                
                if '*t' in arg[0:] and 'NOW' in arg:
                    if dates is None: dates = []
                    dates.append(datetime.datetime.utcnow())
                elif '*t' in arg[0:]:
                    if dates is None: dates = []
                    date = _set_date(arg[3:].split('.'))
                    dates.append(date)
        return pred[0], val, op, dates
    
    def change_params(self, new=None, revert=False):
        if revert is not True:
            self.oldTerm, self.term = self.term, new
        else:
            self.term = self.oldTerm
            del self.oldTerm

def make_fact(pred, f_type=None, *args):
    """Parses a grounded predicate and returns a 'fact'."""
    
    class GroundedTerm(LogPredicate):        
        def __init__(self, pred):
            parent, val, op, dates = super(GroundedTerm, self).__init__(pred)
            assert (op == '='), \
            "It's a grounded predicate, must assign truth value."
            self.parent = parent
            self.term = val[0]
            self.value = val[1]
            if dates is not None:
                self.dates = dates
    
    class FreeTerm(LogPredicate):
        def __init__(self, pred):
            parent, val, op, dates = super(FreeTerm, self).__init__(pred)
            self.parent = parent
            self.term = val[0]
            self.value = val[1]
            self.op = op
            if dates is not None:
                self.dates = dates
        
        def __eq__(self, other):
            if not issubclass(other.__class__, LogPredicate):
                m = "{0} and {1} are not comparable.".format(other, LogPredicate)
                raise TypeError(m)
            if self.op == '=' and other.value == self.value:
                return True
            elif self.op == '>' and other.value > self.value:
                return True
            elif self.op == '<' and other.value < self.value:
                return True
            else: return False
        
        def substitute(self, sbj=None, val=None):
            subs = copy.deepcopy(self)
            if sbj is not None: subs.term = sbj
            if val is not None: subs.value = val
            return subs
    
    assert (f_type in LogPredicate.types or f_type is None), \
            'Function {0} does not exist.'.format(f_type)
    if f_type == 'grounded_term': return GroundedTerm(pred)
    elif f_type == 'free_term': return FreeTerm(pred)
    else: return LogPredicate(pred)


class LogFunction(object):
    """Base class to represent a logic function."""    
    types = ['relation']
    
    def __init__(self, sent):
        func = rgx_ob.findall(sent)[0].split('[')
        self.func, vrs = func[0], func[1]
        args, hls, mk_args = vrs.split(';'), list(), list()
        dates = None
        for x, arg in enumerate(args):
            if ',u' in arg:
                narg = arg.split(',u')
                narg = narg[0], float(narg[1][1:]), narg[1][0]
                if narg[1] > 1 or narg[1] < 0:
                    raise ValueError(narg[1])
                hls.append(narg[0])
                mk_args.append(narg)   
            elif '*t' in arg[0:] and 'NOW' in arg:
                if dates is None: dates = []
                dates.append(datetime.datetime.utcnow())
            elif '*t' in arg:
                if dates is None: dates = []
                date = _set_date(arg[3:].split('.'))
                dates.append(date)
            else:
                mk_args.append(arg)
                hls.append(arg)
        self.args_ID = hash(tuple(hls))
        self._id = self.args_ID
        self.args = mk_args
        self.arity = len(self.args)
        if dates is not None:
            self.dates = dates
        
    def get_args(self):
        ls = []
        for arg in self.args:
            if isinstance(arg, tuple):
                ls.append(arg[0])
            else:
                ls.append(arg)
        return ls
    
    def substitute(self, args):
        subs = copy.deepcopy(self)
        subs.args_ID = hash(tuple(args))
        for x, arg in enumerate(subs.args):
            if isinstance(arg, tuple):
                subs.args[x] = list(arg)
                subs.args[x][0] = args[x]
                subs.args[x] = tuple(subs.args[x])
            else:
                subs.args[x] = args[x]
        return subs
    
    def change_params(self, new=None, revert=False):
        if revert is False:
            self.oldTerm = self.args.copy()
            for x, arg in enumerate(self.args):
                if isinstance(arg, tuple):
                    self.args[x] = list(arg)
                    self.args[x][0] = new[x]
                    self.args[x] = tuple(self.args[x])
                else:
                    self.args[x] = new[x]
        else:
            self.term = self.oldTerm
            del self.oldTerm 
    
    def __str__(self):
        return "<LogFunction '{0}' -> args: {1}>".format(self.func,self.args)

def make_function(sent, f_type=None, *args):
    """Parses and makes a function of n-arity.
    
    Functions describe relations between objects (which can be instantiated
    or variables). This functions can have any number of arguments, the
    most common being the binary functions.
    
    This class is instantiated and provides a common interface for all the 
    function types, which are registered in this class. It acts as an 
    abstraction to hide the specific details from the clients.
    
    The types are subclasses and will implement the details and internal
    data structure for the function, but are not meant to be instantiated
    directly.
    """
        
    class NotCompFuncError(Exception):
        """Logic functions are not comparable exception."""
    
        def __init__(self, args):
            self.err, self.arg1, self.arg2 = args  
    
    class RelationFunc(LogFunction):
    
        def __eq__(self, other):
            comparable = self.chk_args_eq(other)
            if comparable is not True:
                raise NotCompFuncError(comparable)
            for x, arg in enumerate(self.args):
                if isinstance(arg, tuple):
                    oarg = other.args[x]
                    if arg[2] == '=' and arg[1] != oarg[1]:  
                        return False                      
                    elif arg[2] == '>'and arg[1] > oarg[1]:
                        return False     
                    elif arg[2] == '<'and arg[1] < oarg[1]:  
                        return False
            return True
        
        def __ne__(self, other):
            comparable = self.chk_args_eq(other)
            if comparable is not True:
                raise NotCompFuncError(comparable)
            for x, arg in enumerate(self.args):
                if isinstance(arg, tuple):
                    oarg = other.arg[x]
                    if arg[2] == '=' and arg[1] != oarg[1]:
                        return True                      
                    elif arg[2] == '>'and arg[1] < oarg[1]:
                        return True     
                    elif arg[2] == '<'and arg[1] > oarg[1]: 
                        return True
            return False
    
        def chk_args_eq(self, other):
            if other.arity != self.arity:
                return ('arity', other.arity, self.arity)
            if other.func != self.func:
                return ('function', other.func, self.func)
            for x, arg in enumerate(self.args):
                if isinstance(arg, tuple):
                    if other.args[x][0] != arg[0]:
                        return ('args', other.args[x][0], arg[0])
                else:
                    if other.args[x] != arg:
                        return ('args', other.args[x], arg)
            return True
    
    assert (f_type in LogFunction.types or f_type is None), \
            'Function {0} does not exist.'.format(f_type)
    if f_type == 'relation':
        return RelationFunc(sent)
    else:
        return LogFunction(sent)

#   HELPER FUNCTIONS

def _set_date(date):
    tb =['year','month','day','hour','minute','second','microsecond']
    dobj = {}
    for i,p in enumerate(date):
        if 'tzinfo' not in p:
           dobj[tb[i]] = int(p)
    if 'year' not in dobj: raise ValueError('year not specified')
    if 'month' not in dobj: raise ValueError('month not specified')
    if 'day' not in dobj: raise ValueError('day not specified')
    if 'hour' not in dobj: dobj['hour'] = 0
    if 'minute' not in dobj: dobj['minute'] = 0
    if 'second' not in dobj: dobj['second'] = 0
    if 'microsecond' not in dobj: dobj['microsecond'] = 0
    if 'tzinfo' not in dobj: dobj['tzinfo'] = None
    date = datetime.datetime(year=dobj['year'],
                              month=dobj['month'],
                              day=dobj['day'],
                              hour=dobj['hour'],
                              minute=dobj['minute'],
                              second=dobj['second'],
                              microsecond=dobj['microsecond'],
                              tzinfo=dobj['tzinfo'])
    return date
    
