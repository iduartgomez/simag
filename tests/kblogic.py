import unittest
import os

from simag.core.kblogic import *
from simag.core.parser import (
    logic_parser
)

#========================#
#    HELPER FUNCTIONS    #
#========================#

def load_sentences(test):
    comment = False
    path = os.path.dirname(__file__)
    logic_test = os.path.join(path, 'kblogic', test)
    ls, sup_ls = [], []
    with open(logic_test, 'r') as f:
        for line in f:
            line = line.strip()
            if 'cb' in locals() and line[0] != '}':
                cb = cb + line
            else:
                if line[0:2] == '/*':
                    comment = True
                elif line[-2:] == '*/':                    
                    comment = False
                elif line[0] == '#' or comment: 
                    pass
                elif line == 'BLOCK':
                    sup_ls, ls = ls, list()
                elif line == '/BLOCK':
                    sup_ls.append(ls)
                    ls = sup_ls
                elif line[0] == '{':
                    cb = line[1:]
                elif line[0] == '}':
                    ls.append(cb)
                    del cb
                else:
                    ls.append(line)
    return ls

def repeat(times):
    def repeat_helper(f):
        def call_helper(*args):
            for i in range(0, times):
                f(*args)
        return call_helper
    return repeat_helper

#====================#
#    UNIT TESTING    #
#====================#


class BMSTesting(unittest.TestCase):
    
    def setUp(self):
        self.rep = Representation()
    
    def test_rollback(self):
        fol="""
            (ugly[$Pancho, u=0])
            (dog[$Pancho,u=1])
            (meat[$M1,u=1])
            (fn::eat[$M1,u=1;$Pancho])
            
            ((let x, y)
             ((dog[x,u=1] && meat[y,u=1] && fn::eat[y,u=1;x]) 
              |> fat[x,u=1]))
            
            ((let x)
             ((fat[x,u=1] && dog[x,u=1]) |> (sad[x,u=1] && ugly[x,u=1])))
        """
        self.rep.tell(fol)
        answ = self.rep.ask("(fat[$Pancho,u=1] && sad[$Pancho,u=1])", single=True)
        self.assertTrue(answ)
        
        self.rep.tell("(fn::run[$Pancho,u=1])")
        fol = """
        ((let x, y) 
         ((fn::run[x,u=1] && dog[x,u=1]) |> fat[x,u=0]))
        """
        self.rep.tell(fol)
        answ = self.rep.ask("(fat[$Pancho,u=1])", single=True)
        self.assertFalse(answ)
        
        sad = self.rep.individuals['$Pancho'].get_ctg('sad')
        ugly = self.rep.individuals['$Pancho'].get_ctg('ugly')
        self.assertEqual(ugly, 0)
        self.assertEqual(sad, None)
    
    def test_review_after_change(self):
        fol="""
            ( ( let x, y )
              ( ( dog[x,u=1] && meat[y,u=1] && fn::eat[y,u=1;x] ) 
                |> fat[x,u=1] ) )
            ( dog[$Pancho,u=1] )
            ( meat[$M1,u=1] )
            ( fn::eat[$M1,u=1;$Pancho] )
        """
        self.rep.tell(fol)
        fat = self.rep.individuals['$Pancho'].get_ctg('fat')
        self.assertEqual(fat, 1)
        
        fol = """
            ( fn::run[$Pancho,u=1] )
            (( let x,y ) (( dog[x,u=1] && fn::run[x,u=1] ) |> fat[x,u=0] ))
        """        
        self.rep.tell(fol)
        fat = self.rep.individuals['$Pancho'].get_ctg('fat')
        self.assertEqual(fat, 0)
        
        self.rep.tell("(fn::eat[$M1,u=1;$Pancho])")
        answ = self.rep.ask("(fat[$Pancho,u=1])", single=True)
        self.assertTrue(answ)
        
        #fat = self.rep.individuals['$Pancho'].get_ctg('fat')
        #self.assertEqual(fat, 1)

class AskReprGetAnswer(unittest.TestCase):
    
    def test_ask_pred(self):
        sents = load_sentences('ask_pred.txt')
        ask = [
            (['(professor[$Lucy,u=1] && person[$Lucy,u=1])'], None),
            (['(professor[$Lucy,u=1])', '(person[$John,u=1])'], (True, True)),
            (['(professor[$Lucy,u>0] && person[$Lucy,u<1])'], False), # <--- this one fails sometimes
            (['(criminal[$West,u=1])'], True),
            (["(fat(t='*now')[$Pancho,u=1])"], True),
            (["(fat(t='*now')[$Pancho,u=1])"], True),
            (['((let x) (professor[x,u=1]))'], 
             {'$Lucy': {'professor': True}, '$John': {'professor': True}}),
            (['((let x) (x[$Lucy,u>0.5]))'], 
             {'$Lucy': {'professor': True, 'person': True}}),
        ]
        self.iter_eval(sents, ask)
    
    def test_ask_func(self):
        sents = load_sentences('ask_func.txt')
        ask = [
            (['(fn::criticize[$John,u=1;$Lucy])'], True),
            (['(fn::friend[$Lucy,u=0;$John])'], True),
            (['(fn::sells[$M1,u=1;$West;$Nono])'], True),
            (['(fn::produce[milk,u=1;cow])'], True),
            (['(fn::eat[$M1,u=1;$Pancho])'], True),
            (['(fn::eat[$M1,u=1;$Pancho])'], True),
            (['((let x) (fn::produce[milk,u>0;x]))'],
             {'$Lucy': {'produce': True}, '$Vicky': {'produce': True}}),
            (['((let x) (fn::x[$Vicky,u>0;$Lucy]))'],
             {'$Lucy': {'loves': True}, '$Vicky': {'loves': True}}),
        ]
        self.iter_eval(sents, ask)
    
    def test_event_chain_with_times(self): # <--- fails some times
        self.rep = Representation()
        fol = """
            (dog[$Pancho,u=1])
            (meat[$M1,u=1])
            (fn::eat(time='2015.01.01')[$M1,u=1;$Pancho])
            ((let x, y)
              ((dog[x,u=1] && meat[y,u=1] && fn::eat[y,u=1;x]) 
                |> fat[x,u=1]))
        """  
        self.rep.tell(fol)
        fat = self.rep.individuals['$Pancho'].get_ctg('fat')
        self.assertEqual(fat,1)
        
        fol = """
            (fn::run(time='2015.01.02')[$Pancho,u=1])
            ((let x, y ) (( dog[x,u=1] && fn::run[x,u=1] ) |> fat[x,u=0]))
        """        
        self.rep.tell(fol)
        fat = self.rep.individuals['$Pancho'].get_ctg('fat')
        self.assertEqual(fat,0)
        
        fol = """
            (fn::eat(time='2015.01.02')[$M1,u=1;$Pancho])
            (fn::run(time='2015.01.01')[$Pancho,u=1])
            ((let x, y, t1:time, t2:time)
             (((fn::run(t2=time)[x,u=1] && fn::eat(t1=time)[y,u=1;x] 
                    && dog[x,u=1] && meat[y,u=1])
                && fn::time_calc(t1>t2))
              |> (fat[x,u=1] || fat[x,u=0]) ))
        """
        self.rep.tell(fol)
        fat = self.rep.individuals['$Pancho'].get_ctg('fat')
        self.assertEqual(fat,1)
        
        self.rep.tell("""
        (fn::eat(time='2015.02.01')[$M1,u=1;$Pancho])
        (fn::run(time='2015.02.02')[$Pancho,u=1])
        """)
        answ = self.rep.ask("(fat[$Pancho,u=0])", single=True)
        self.assertTrue(answ)
        
    @unittest.skip
    def test_single_stmt(self):
        # for testing single subtests in the other tests
        fol = """
            ( dog[$Pancho,u=1] )
            ( meat[$M1,u=1] )
            ( fat[$Pancho,u=1] )
            (( let x, y, t1: time="2015.01.01", t2: time="2015.02.01" )
             ( ( dog[x,u=1] && meat[y,u=1] && fat(time=t2)[x,u=1] && fn::time_calc(t1<t2) )
               |> fn::eat(time=t1)[y,u=1;x]
             )
            )
        """
        self.rep = Representation()
        self.rep.tell(fol)
        answ = self.rep.ask('(fn::eat[$M1,u=1;$Pancho])', single=True)
        self.assertEqual(answ, True)
    
    def iter_eval(self, sents, ask):
        for i, test in enumerate(sents):
            self.rep = Representation()
            with self.subTest(test='subtest {0}: {1}'.format(i,ask[i])):
                #print("subtest %s" % i)
                for s in test:
                    self.rep.tell(s)
                for j, q in enumerate(ask[i][0]):
                    if type(ask[i][1]) in (bool, tuple) \
                    or ask[i][1] is None:
                        single = True
                    else:
                        single = False
                    answ = self.rep.ask(
                        q, single=single, ignore_dates=True, ignore_current=False)                
                    if isinstance(ask[i][1], tuple):
                        self.assertEqual(ask[i][1][j], answ)
                    else:
                        self.assertEqual(ask[i][1], answ)
    
    def tearDown(self):
        if hasattr(self, 'rep'):
            del self.rep
    
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
                for s in test[1:]:
                    self.rep.tell(s)
                self.assertIs(self.rep.ask(test[0],single=True), results[x])
    
    def test_eval_impl(self):
        num = [1,2,3,4,5]
        results = [None,True,True,False,True]
        assert_this = ['$West','$West','$West','$West','$West']
        tests = [test for x, test in enumerate(self.tests) if x in num]
        self.iter_eval(tests, assert_this, results)
    
    def test_eval_equiv(self):
        num = [6,7,8,9]
        results = [None,False,True,True]
        assert_this = ['$West','$West','$West','$West']
        tests = [test for x, test in enumerate(self.tests) if x in num]
        self.iter_eval(tests, assert_this, results)
    
    def test_eval_or(self):
        pass
    
    def test_eval_and(self):
        pass
    
    @unittest.skip
    def test_single_stmt(self):
        # for testing single subtests in the other tests
        self.rep = Representation()
        fol = """
            ( drugDealer[$West,u=1] |> ( scum[$West,u=1] && good[$West,u=0] ) )
            ( drugDealer[$West,u=0] )
        """
        self.rep.tell(fol)
        answ = self.rep.ask("( scum[$West,u=1] && good[$West,u=0] )")
        self.assertIsNone(answ)
        
    def iter_eval(self, tests, assert_this, results):
        for x, test in enumerate(tests):
            self.rep = Representation()
            with self.subTest(sent='subtest {0}: {1}'.format(x,test[0])):
                for s in test[1:]:
                    self.rep.tell(s)
                proof = logic_parser(test[0]).assert_rules[0]
                res = proof(self.rep, assert_this[x])
                self.assertIs(res[0], results[x])
                
    def tearDown(self):
        if hasattr(self, 'rep'):
            del self.rep
    
class LogicSentenceParsing(unittest.TestCase):
    
    def test_parse_predicate(self):            
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
                ('cow','animal',1)]
        isactg = [2]
        failures = [4, 5, 6, 3]
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
                    self.assertRaises(Exception, rep.tell, sent)
    
    def test_parse_function(self):
        rep = Representation()
        sents = load_sentences('parse_function.txt')
        eval = [( [('$John', 1, '='), '$Lucy'], 'criticize') ,
                ( [('$analysis', 0, '>'), '$Bill'], 'takes'),
                ( [('$Bill', 1, '<'), '$Lucy'], 'sister'),
                ( [('cow', 1, '='), 'bull'], 'loves' )]
        failures = [4, 5]
        for x, sent in enumerate(sents):
            with self.subTest(sent='subtest {0}: {1}'.format(x,sent)):
                if x not in failures:
                    func = logic_parser(sent).assert_rel[0]
                    self.assertEqual(func.func, eval[x][1])
                    self.assertListEqual(func.args, eval[x][0])
                else:
                    self.assertRaises(ValueError, rep.tell, sent)
    
    def test_parse_sentence_with_vars(self):        
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
                lg_sent = logic_parser(sent).assert_cogs[0]
                preds = lg_sent.get_all_preds()
                chk = [p.func for p in preds \
                       if issubclass(p.__class__, LogFunction)]
                chk2 = [p.parent for p in preds \
                        if issubclass(p.__class__, LogPredicate)]
                chk.extend(chk2)
                for obj in eval[x]:
                    self.assertIn(obj, chk)
                self.assertGreater(len(lg_sent.var_order), 0)

if __name__ == "__main__":
    unittest.main()
