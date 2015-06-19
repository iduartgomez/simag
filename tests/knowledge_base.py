"""
@author: Ignacio Duart Gomez
"""

import unittest
from core.kblogic import *

def load_sentences(test):
    path = os.path.dirname(__file__)
    logic_test = os.path.join(path, 'knowledge_base', test)
    ls, sup_ls = [], []
    with open(logic_test, 'r') as f:
        for line in f:
            if line.strip()[0] == '#': pass
            elif line.strip() == '{':
                sup_ls, ls = ls, list()
            elif line.strip() == '}':
                sup_ls.append(ls)
                ls = sup_ls
            else: ls.append(line.strip())
    return ls

def iter_test(self, sents, ask, eval):
    for i, test in enumerate(sents):
        with self.subTest(test='subtest {0}: {1}'.format(i,test)):
            if isinstance(test, list):
                for s in test:
                    self.rep.tell(s)
                for j, q in enumerate(ask[i]):
                    if isinstance(eval[i], list):
                        answ = self.rep.ask(q)
                        for k in eval[i][j].keys():
                            self.assertEqual(eval[i][j][k], answ[k])
                    else:
                        answ = self.rep.ask(q)
                        for k in eval[i].keys():
                            self.assertEqual(eval[i][k], answ[k])
            else:
                self.rep.tell(s)
                for q in ask[i]:
                    answ = self.rep.ask(q)
                    for k in eval[i].keys():
                        self.assertEqual(eval[i][k], answ[k])

#====================#
#    UNIT TESTING    #
#====================#

@unittest.skip('')
class AskReprGetAnswer(unittest.TestCase):
    
    def setUp(self):
        self.rep = Representation()
    
    def test_ask_pred(self):
        sents = load_sentences('ask_pred.txt')
        ask = [
               ['professor[$Lucy,u=1] && person[$Lucy,u=0]'],
               ['professor[$Lucy,u=1]', 'person[$John,u=1]'],
               ['professor[$Lucy,u=1] && person[$Lucy,u=0]'],
               ['criminal[$West,u=1]']
              ]
        eval = [
                {'$Lucy': {'professor': True, 'person': None}},
                [{'$Lucy': {'professor': True}},{'$John': {'person': True}}],
                {'$Lucy': {'professor': True, 'person': True}},
                {'$West': {'criminal': True}}
               ]
        iter_test(self, sents, ask, eval)
    
    @unittest.skip('Not ready')
    def test_ask_func(self):
        sents = load_sentences('ask_func.txt')
        ask = [
               ['<friend[$Lucy,u=0;$John]>'],
               ['<friend[$Lucy,u=0;$John]>'],
               ['<sells[$M1,u=1;$West;$Nono]>']
              ]
        eval = [
                {'$John': {'friend': ('$Lucy', None)}},
                {'$John': {'friend': ('$Lucy', True)}},
                {'$West': {'sells': ('$M1', True, '$Nono')}}
               ]
        iter_test(self, sents, ask, eval)


class EvaluationOfFOLSentences(unittest.TestCase):
    
    def setUp(self):
        self.rep = Representation()
        self.tests = load_sentences('eval_fol.txt')
        
    def test_eval_icond(self):
        ask = ['scum[$West,u=1] && good[$West,u=0]']
        for s in self.tests[0]:
            self.rep.tell(s)
        self.assertIs(self.rep.ask(ask[0],single=True), True)
        
    def test_eval_impl(self):
        num = [1, 2]
        results = [False, True]
        tests = [test for x, test in enumerate(self.tests) if x in num]
        for x, test in enumerate(tests):
            with self.subTest(sent='subtest {0}: {1}'.format(x,test[0])):
                for s in test[1:]:
                    self.rep.tell(s)
                ori, comp, hier = parse_sent(test[0])
                proof = LogSentence(ori, comp, hier)
                proof(self.rep, '$West')
                self.assertIs(proof.result, results[x])
        
    def test_eval_equiv(self):
        pass
    
    def test_eval_or(self):
        pass
    
    def test_eval_and(self):
        pass

@unittest.skip('')
class LogicSentenceParsing(unittest.TestCase):
    
    def test_parse_predicate(self):
        """Test parsing of predicates."""
            
        def test():
            name, ctg, val = e[0], e[1], e[2]
            obj = rep.individuals[name]
            chk_ctg = obj.check_cat([ctg])
            self.assertIn(ctg, chk_ctg, "Category not declared.")
            self.assertEqual(val, obj.get_cat(ctg=ctg))
                            
        rep = Representation()
        sents = load_sentences('parse_predicate.txt')
        objs = [
                ('$Lucy', 'professor', 1),
                ('$John', 'dean', 0),
                (('$Bill', 'student', 1), ('$John', 'student', 1))
               ]
        failures = [3, 4, 5, 6]
        for x, sent in enumerate(sents):
            with self.subTest(sent='subtest {0}: {1}'.format(x,sent)):
                if x not in failures:
                    rep.tell(sent)
                    if isinstance(objs[x][0], tuple):
                        for e in objs[x]:
                            test()
                    else:
                        e = objs[x]
                        test()
                else:
                    self.assertRaises(AssertionError, rep.tell, sent)
    
    def test_parse_function(self):
        """Test parsing of functions."""
        
        sents = load_sentences('parse_function.txt')
        eval = [
                ([('$John', 1, '='), '$Lucy'], 'criticize'),
                ([('$analysis', 0, '>'), '$Bill'], 'takes'),
                ([('$Bill', 1, '<'), '$Lucy'], 'sister'),
               ]
        failures = [3, 4]
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
        objs = [('dean','professor'),
                ('professor','person'),
                ('professor','dean','friend','knows'),
                ('person','criticize','friend'),
                ('american','weapon','sells','hostile','criminal'),
                ('owns','missile','sells'),
                ('missile','weapon'),
                ('enemy','hostile')]        
        for x, sent in enumerate(sents):
            with self.subTest(sent='subtest {0}: {1}'.format(x,sent)):
                ori, comp, hier = parse_sent(sent)
                lg_sent = LogSentence(ori, comp, hier)
                preds = lg_sent.get_pred(conds=gr_conds)
                preds.extend(lg_sent.get_pred(branch='r',conds=gr_conds))
                p_func = [p.func for p in preds \
                          if isinstance(p,LogFunction)]
                chk = [x[0] for x in preds if isinstance(x, tuple)]
                chk.extend(p_func)
                for obj in objs[x]:
                    self.assertIn(obj, chk)

if __name__ == "__main__":
    unittest.main()

