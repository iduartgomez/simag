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
* Categories (also called classes or sets).
  Categories are sets of objects which share common properties.
* Individuals.
  Individuals are unique objects in the world, which could be said to be
  the sole members of their own class.

Categories
----------

Categories or classes have several properties:
1. Attributes. This are unique items, which can be added

******************************
Synthax and semantics of SimAg
******************************

Agent representation synthax
============================

The synthax for knowledge representation and logic inference is based
on first-order logic.

Every sensory input of the agent is translated to sentences, as well
as any preprogrammed knowledge or axiom is introduced through parsing
of this sentences.

This idiom provides a high abstraction, yet efficient, way of translating
any type of information to internal representation.