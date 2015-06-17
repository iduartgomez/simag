"""
@author: Ignacio Duart Gomez
"""
import unittest
from core.kblogic import *

#====================#
#    UNIT TESTING    #
#====================#

def load_sentences(test):
    path = os.path.dirname(__file__)
    logic_test = os.path.join(path, 'knowledge_base', test)
    ls, sup_ls = [], []
    with open(logic_test, 'r') as f:
        for line in f:
            if line[0] == '#': pass
            elif line.strip() == '{':
                sup_ls, ls = ls, list()
            elif line.strip() == '}':
                sup_ls.append(ls)
                ls = sup_ls
            else: ls.append(line.strip())
    return ls


class AskReprGetAnswer(unittest.TestCase):
    
    def setUp(self):
        self.rep = Representation()
    
    def test_ask_pred(self):
        sents = load_sentences('ask_pred.txt')
        ask = [
                ['professor[$Lucy,u=1] && person[$Lucy,u=0]'],
                ['professor[$Lucy,u=1]', 'person[$Lucy,u=1]'],
                ['criminal[$West,u=1]']
              ]
        eval = [
                 {'$Lucy': {'professor': None, 'person': None}},
                 [{'$Lucy': {'professor': True}},
                  {'$Lucy': {'person': True}}],
                 {'$West': {'criminal': True}}
               ]
        self.iter_test(sents, ask, eval)
               
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
        self.iter_test(sents, ask, eval)
        
    def iter_test(self, sents, ask, eval):
        for i, test in enumerate(sents):
            with self.subTest(test=i):
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


@unittest.skip("Test not writte.")
class EvaluationOfFOLSentences(unittest.TestCase):
    
    def setUp(self):
        self.rep = Representation()
    
    def test_eval_icond(self):
        pass
    
    def test_eval_impl(self):
        pass
    
    def test_eval_equiv(self):
        pass
    
    def test_eval_or(self):
        pass
    
    def test_eval_and(self):
        pass


class LogicSentenceParsing(unittest.TestCase):
    
    def setUp(self):
        self.rep = Representation()
    
    def test_tell_predicate(self):
        """Test parsing and declaration of simple predicates.
        """
        sents = load_sentences('tell_predicate.txt')
        objs = [('$Lucy', 'professor', 1),
                ('$John', 'dean', 1),
                ('$Bill', 'student', 1),
                ('$M1', 'missile', 1),
                ('$West', 'american', 1)]
        for x, sent in enumerate(sents):
            with self.subTest(sent=x):
                self.rep.tell(sent)
                name, ctg, val = objs[x][0], objs[x][1], objs[x][2]
                obj = self.rep.individuals[name]
                chk_ctg = obj.check_cat([ctg])
                self.assertIn(ctg, chk_ctg, "Category not declared.")
                self.assertEqual(val, obj.get_cat(ctg=ctg))
    
    def test_tell_function(self):
        """Test parsing and declaration of simple functions.
        """
        sents = load_sentences('tell_function.txt')
        objs = [('$Lucy', 'criticize', ('$John', 1, '=')),
                ('$Bill', 'takes', ('$analysis', 1, '=')),
                ('$Lucy', 'sister', ('$Bill', 1, '=')),
                ('$Nono', 'owns', ('$M1', 1, '=')),
                ('$America', 'enemy', ('$Nono', 1, '='))]
        for x, sent in enumerate(sents):
            with self.subTest(sent=x):
                self.rep.tell(sent)
                name, rel, val = objs[x][0], objs[x][1], objs[x][2]
                obj = self.rep.individuals[name]
                func = obj.relations[rel][0]
                self.assertIsInstance(func, LogFunction)
                self.assertTupleEqual(func.args[0], val)
    
    def test_parse_sentence_with_vars(self):
        """Test parsing logic sentences with variables.
        """
        sents = load_sentences('parse_sentence_with_vars.txt')
        objs = [('dean','professor'),
                ('professor','person'),
                ('professor','dean','friend','knows'),
                ('person','criticize','friend'),
                ('american','weapon','sells','hostile','criminal'),
                ('owns','missile','sells'),
                ('missile','weapon',),
                ('enemy','hostile')]
        for x, sent in enumerate(sents):
            with self.subTest(sent=x):
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

    @unittest.skip("Method not implemented.")
    def test_parse_complx_sent(self):
        sents = load_sentences('parse_complx_sent.txt')
        for i, sent in enumerate(sents):
            with self.subTest(sent=i):
                self.rep.tell(sent)

if __name__ == "__main__":
    unittest.main()
