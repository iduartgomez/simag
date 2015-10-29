import unittest
import os

from core.kblogic import *
from core.logic_parser import make_logic_sent, _parse_sent

#====================#
#    UNIT TESTING    #
#====================#

class AskReprGetAnswer(unittest.TestCase):
    
    def setUp(self):
        self.rep = Representation()
    
    def test_ask_pred(self):
        sents = load_sentences('ask_pred.txt')
        ask = [['professor[$Lucy,u=1] && person[$Lucy,u=1]'],
               ['professor[$Lucy,u=1]', 'person[$John,u=1]'],
               ['professor[$Lucy,u>0] && person[$Lucy,u<1]'],
               ['criminal[$West,u=1]'],
               ['fat[$Pancho,u=1,*t=NOW]'],]
        eval = [None, [True,True], False, True, True]
        iter_test(self, sents, ask, eval, single=True)
    
    def test_ask_func(self):
        
        sents = load_sentences('ask_func.txt')
        ask = [['<criticize[$John,u=1;$Lucy]>'],
               ['<friend[$Lucy,u=0;$John]>'],
               ['<sells[$M1,u=1;$West;$Nono]>'],
               ['<produce[milk,u=1;cow]>'],
               ['<eat[$M1,u=1;$Pancho]>'],]
        eval = [True, True, True, True, True]
        iter_test(self, sents, ask, eval, single=True)

class EvaluationOfFOLSentences(unittest.TestCase):
    
    def setUp(self):
        self.tests = load_sentences('eval_fol.txt')
    
    def test_eval_icond(self):
        num = [0,10]
        results = [True,None]
        tests = [test for x, test in enumerate(self.tests) if x in num]
        for x, test in enumerate(tests):
            self.rep = Representation()
            with self.subTest(sent='subtest {0}: {1}'.format(x,test[0])):
                for y, s in enumerate(test):
                    if y > 0: self.rep.tell(s)
                self.assertIs(self.rep.ask(test[0],single=True), results[x])
    
    def test_eval_impl(self):
        num = [1,2,3,4,5]
        results = [None,True,True,False,True]
        assert_this = ['$West','$West','$West','$West','$West']
        tests = [test for x, test in enumerate(self.tests) if x in num]
        self.check(tests, assert_this, results)
    
    def test_eval_equiv(self):
        num = [6,7,8,9]
        results = [None,False,True,True]
        assert_this = ['$West','$West','$West','$West']
        tests = [test for x, test in enumerate(self.tests) if x in num]
        self.check(tests, assert_this, results)
    
    def test_eval_or(self):
        pass
    
    def test_eval_and(self):
        pass
    
    def check(self, tests, assert_this, results):
        for x, test in enumerate(tests):
            self.rep = Representation()
            #print('\n===== SUBTEST =====')
            #print('subtest',x,'|',test,'\n')
            with self.subTest(sent='subtest {0}: {1}'.format(x,test[0])):
                for s in test[1:]:
                    self.rep.tell(s)
                ori, comp, hier = _parse_sent(test[0])
                proof = make_logic_sent(ori, comp, hier)     
                if results[x] is None:
                    self.assertIs(hasattr(proof,'result'),False)
                else:
                    proof(self.rep, assert_this[x])
                    self.assertIs(proof.result, results[x])

class LogicSentenceParsing(unittest.TestCase):
    
    def test_parse_predicate(self):
        """Test parsing of predicates."""
            
        def assert_res(cls=False):
            name, ctg, val = e[0], e[1], e[2]
            if cls is False:
                obj = rep.individuals[name]
                chk_ctg = obj.check_ctg([ctg])            
                self.assertEqual(val, obj.get_ctg(ctg=ctg))
            else:
                obj = rep.classes[name]
                chk_ctg = obj.check_ctg([ctg])
                self.assertEqual(val, obj.get_ctg(ctg=ctg))
            self.assertIn(ctg, chk_ctg, "Category not declared.")
        
        rep = Representation()
        sents = load_sentences('parse_predicate.txt')
        objs = [('$Lucy','professor', 1),
                ('$John','dean', 0),
                (('$Bill','student', 1),('$John','student',1)),
                ('cow','animal',1)]
        isactg = [3]
        failures = [4, 5, 6, 7]
        for x, sent in enumerate(sents):
            with self.subTest(sent='subtest {0}: {1}'.format(x,sent)):
                if x in isactg: cls = True
                else: cls = False
                if x not in failures:
                    rep.tell(sent)
                    if isinstance(objs[x][0], tuple):
                        for e in objs[x]:
                            assert_res(cls=cls)
                    else:
                        e = objs[x]
                        assert_res(cls=cls)
                else:
                    self.assertRaises(AssertionError, rep.tell, sent)
    
    def test_parse_function(self):
        """Test parsing of functions."""
        
        sents = load_sentences('parse_function.txt')
        eval = [([('$John', 1, '='), '$Lucy'], 'criticize'),
                ([('$analysis', 0, '>'), '$Bill'], 'takes'),
                ([('$Bill', 1, '<'), '$Lucy'], 'sister'),
                ([('cow', 1, '='), 'bull'], 'loves')]
        failures = [4, 5]
        for x, sent in enumerate(sents):
            with self.subTest(sent='subtest {0}: {1}'.format(x,sent)):
                if x not in failures:
                    func = make_function(sent)
                    self.assertEqual(func.func, eval[x][1])
                    self.assertListEqual(func.args, eval[x][0])
                else:
                    self.assertRaises(ValueError, make_function, sent)
    
    def test_parse_sentence_with_vars(self):
        """Test parsing logic sentences with variables."""
        
        sents = load_sentences('parse_sentence_with_vars.txt')
        eval = [('dean','professor'),
                ('professor','person'),
                ('professor','dean','friend','knows'),
                ('person','criticize','friend'),
                ('american','weapon','sells','hostile','criminal'),
                ('owns','missile','sells'),
                ('missile','weapon'),
                ('enemy','hostile')]
        for x, sent in enumerate(sents):
            with self.subTest(sent='subtest {0}: {1}'.format(x,sent)):
                ori, comp, hier = _parse_sent(sent)
                lg_sent = make_logic_sent(ori, comp, hier)
                preds = lg_sent.get_pred(conds=GL_PCONDS)
                preds.extend(lg_sent.get_pred(branch='r',conds=GL_PCONDS))
                chk1 = [p.func for p in preds if \
                       issubclass(p.__class__,LogFunction)]
                chk2 = [p.parent for p in preds if \
                       issubclass(p.__class__,LogPredicate)]
                chk1.extend(chk2)
                for obj in eval[x]:
                    self.assertIn(obj, chk1)

#====================#
#    HELPER FUNCTIONS  #
#====================#

def load_sentences(test):
    path = os.path.dirname(__file__)
    logic_test = os.path.join(path, 'kblogic', test)
    ls, sup_ls = [], []
    with open(logic_test, 'r') as f:
        for line in f:
            line = line.strip()
            if 'cb' in locals() and line[0] != '}':
                cb = cb + line
            else:
                if line[0] == '#': pass
                elif line == 'BLOCK':
                    sup_ls, ls = ls, list()
                elif line == '/BLOCK':
                    sup_ls.append(ls)
                    ls = sup_ls
                elif line[0] == '{':
                    cb = line
                elif line[0] == '}':
                    cb = cb + line
                    ls.append(cb)
                    del cb
                else: ls.append(line)
    return ls

def iter_test(self, sents, ask, eval, single=False):
    for i, test in enumerate(sents):
        with self.subTest(test='subtest {0}: {1}'.format(i,ask[i])):
            if isinstance(test, list):
                for s in test:
                    self.rep.tell(s)
                for j, q in enumerate(ask[i]):
                    answ = self.rep.ask(q,single=single)                    
                    if isinstance(eval[i], list):
                        if single is not True:
                            for k in eval[i][j].keys():
                                self.assertEqual(eval[i][j][k], answ[k])
                        else:
                            self.assertEqual(eval[i][j], answ)
                    else:
                        if single is not True:
                            for k in eval[i].keys():
                                self.assertEqual(eval[i][k], answ[k])
                        else:
                            self.assertEqual(eval[i], answ)
            else:
                self.rep.tell(s)
                for q in ask[i]:
                    answ = self.rep.ask(q)
                    for k in eval[i].keys():
                        self.assertEqual(eval[i][k], answ[k])


if __name__ == "__main__":
    unittest.main()

