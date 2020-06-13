# Representation of knowledge

When an agent is instantiated, a knowledge base (KB) is created for that agent.

Knowledge bases are encapsulated and independent of each other, as they form part of the internal representation of a single agent. At every moment, each agent
holds a representation of the environment, including other agents or
objects, and their own internal state.

The representation holds both axioms about the world where the agent
inhabits, and beliefs or facts that could be objectively false (in case
of beliefs), or only temporally true (in case of facts). This includes
attribution of beliefs or actions to other agents as well.

New information can be contradictory with previous beliefs or facts,
in this case the new information is processed and the inconsistencies
in the KB are fixed, repaired, or mitigated following different strategies.

The new information is categorized in different ways, depending on the
structure and attributes of that information. There are two main categories
in which agents categorize information:

* Classes: Sets of objects which share common properties.
* Entities. Unique objects in the world, which could be said to be
  the sole members of their own singleton set.

### Classes

Classes have several properties:
1. Attributes: Attributes common to to every member of the class in varying
   degrees.
2. Beliefs: Those are 'cognitions' in the form of FOL/fuzzy logic that are
   applied to infer qualities about objects and classes (helpful for inference).

LINK TO LOGIC CHAPTER FOR MORE INSIGHTFUL EXPLANATION

## Knowledge representation semantics

Every consistent, over time, sensory input (scene) of the agent is translated eventually 
to a series of beliefs and rules expressed as sentences, this is how the agent retains
a high level view of the environment, by extracting structure from it. As well,
any programmed knowledge or axiom is introduced through parsing of this logic sentences.
This idiom provides an abstract, yet efficient, way of translating any type of information
to the internal representation.

There are four ways facts enter the KB. The first is through predicate assertions:
* `professor[$Lucy=1]`
* `white[cows=0.7]`
  
Predicate assertions declare an object or a class as a member of an other 
class. In the first example, the individual `Lucy` is declared as a member 
of the `professor` class. In the second, the class `cows` is declared as subclass
of the `white` class, but notice that membership is not complete (1.0) but only 
partial (0.7). You can still use declaration only in classical binary style
by using the values 0 (negation) or 1 (assertion).

The second way is through function assertions:
* `fn::love[$Lucy=1;$John]`
* `fn::steal[peasant=1;king]`
* `fn::steal[$John=1;king]`

In the first example, it's stated that the entity `John` has a relation of the
'love' type with the entity `Lucy` (note this does not mean that the object 'Lucy'
has a 'love' relation with John, there is not implied reciprocity in a function).

In the second example, it's stated that any member of the `king` class has
a `steal` relationship with the `peasant` class. Whereas in the third, this
relationship is held from the `king` class to the object `John` (regardless of
what classes does the object `John` belongs to).

For a more throughout view on asserting knowledge check the logic programming language.
