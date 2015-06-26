********************
Agents inner working
********************

Representation of knowledge
===========================

When an agent is instantiated, a knowledge base is created for that agent.

KB are encapsulated and independent of each other, as they form part of
the internal representation of a single agent. At every moment, each agent
holds a representation of the environment, including other agents or
objects, and their own internal state.

The representation holds both axioms about the world where the agent
inhabits, and beliefs or facts that could be objectively false (in case
of beliefs), or only temporally true (in case of facts). This includes
attribution of beliefs or actions to other agents as well.

New information can be contradictory with previous beliefs or facts,
in this case the new information is processed and the inconsistencies
in the KB are fixed, repaired, or mitigated.

The new information is categorized in different ways, depending on the
structure and attributes of that information. There are two main categories
in which agents categorize information:

* Categories (also called classes or sets). Sets of objects which share 
  common properties.
* Individuals. Unique objects in the world, which could be said to be
  the sole members of their own class.

Categories
----------

Categories or classes have several properties:

1. Attributes. Attributes common to to every member of the class.
2. Rules. Those are 'cognitions' in the form of FOL/fuzzy logic that are
applied to infer qualities about objects and classes.

******************************
Synthax and semantics of SimAg
******************************

Agent representation synthax
============================

The synthax for knowledge representation and logic inference is based
on first-order logic with a few important differences. The differences
with classic FOL are:

* Unique name assumption: each constant (term) refers to a single object or set.
* Includes a symbol for indicative conditional (and other for material
  implication).

  The indicative conditional is an operation in which if the antecedent
  is true, the consequent is true. Contrary to that, the material conditional 
  is always true, unless the consequent is false and the antecedent is true.
  
  The indicative conditional is used to assert rules for classification
  of objects and sets. In other words, is a more combenient way to
  assert rules about declarations.
* There is no negation symbol. This is because every assertion must include
  a real number truth value which can go from 0 to 1. This follows from fuzzy
  logic and allow various degrees of membership to a set, instead of binary
  membership (one either is part or isn't of a set).

Here is a detailed description, with examples, of the synthax:

In general, there are no capitalization rules unless stated.

Variable: :vars:
  Declares a variable, for example: :vars:x: declares
  that the literal 'x' is a variable. Variables are assumed to be universally
  quantified.

Constant - individual: $Lucy
  It's a name, and refeers to a single unique object in the world. Constants
  have a $ as the first symbol. 

Constant - set: professor
  It's a class/set of items, refeers to a class of objects which share some
  attributes.

Predicate assertion atomic sentence: professor[$Lucy,u=1]
  "professor" is the predicate and the variables or terms are passed inside
  square brackets [], in this case is an individual with the name '$Lucy'
  and it has a full membership (u = 1) to the professor class.
  Predicates are unary.

Function assertion atomic sentence: <loves[$Lucy,u=1;$John]>
  Relationships between two objects, or functions, are asserted between
  '<' and '>' symbols, it takes two arguments split by the ';' symbol.
  The first argument is related to the second argument and must include
  an 'u' value. In this example it would mean that the object $John
  is completely (u=1) in love with the object $Lucy.

Complex sentences: AtomicSentence && (AtomicSentence)
  Complex sentences are compossed of various atomic sentences connected by
  connectives/operators and/or sentences which use quantified variables
  (when used, variables must always be quantified).

Operators (by precedence):
  * &&  :  and
  * ||  :  or
  * =>  :  material implication
  * <=>  :  biconditional
  * |>  :  indicative implication

In the consequent of an indicative implication only && operator can be nested.
Parentheses can be used to split complex sentences for comprenhension and 
clarity. An example of a complex sentence:
  \:vars:x,y: (<takes[$analysis,u=1;x]> && <takes[y,u=1;x]> && course[y,u=1] && 
  student[x,u=1] |> silly[x,u=1])
  
  In natural language: Every student that takes analysis and takes an other 
  course is silly.

Agent representation semantics
==============================

Every sensory input of the agent is translated to sentences, as well
as any preprogrammed knowledge or axiom is introduced through parsing
of this sentences. This idiom provides a highly abstract, yet efficient, 
way of translating any type of information to internal representation.

There are four ways facts enter the KB. the first is throught predicate assertions:
  | professor[$Lucy,u=1]
  | white[cows,u=0.7]
  
Predicate assertions declare an object or a class as a member of an other 
class. In the first example, the individual 'Lucy' is declared as a member 
of the 'professor' class. In the second, the class 'cows' is declared as subclass
of the 'white' class, but notice that membership is not complete (1.0) but only 
partial (0.7). You can still use declaration only in classical binary style
by using the values 0 (negation) or 1 (true membership).

The second way is through function assertions:
  | <loves[$Lucy,u=1;$John]>
  | <steals[peasant,u=1;king]>
  | <steals[$John,u=1;king]>

In the first example, it's stated that the object John has a relation of the
'loves' type to the object $Lucy (note this does not mean that the object 'Lucy'
has a 'loves relation with John, there is not implied reciprocity in a function).

In the second example, it's stated that any member of the 'king' class has
a 'steals' relationship with the class 'peasant'. Whereas in the third, this
relationship is holded from the king class to the object 'John' (regardless of
what classes does the object John belongs to).
