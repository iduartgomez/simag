# Syntax for the Agent Representation Language (ARL)

In general, there are no capitalization rules unless stated.

* Alphanumeric literal starting with the $ symbol: an entity identifier, refers to a single unique entity in the world. E.g: `$Lucy`

* Alphanumeric literal: a class identifier, refers to a class of objects which share some attributes. E.g: `professor`

### Predicate assertion 
Example: `professor[$Lucy,u=1]`
> `professor` is the predicate and the variables or terms are passed inside
> square brackets `[ ]`, in this case is an entity with the name `Lucy`
> and it has a full membership (u = 1) to the `professor` class.

### Relational function assertion
Example: `fn::loves[$Lucy,u=1;$John]`
> Relationships between two (and an optional third) entities/classes are asserted
> using the `fn` keyword. A relational function it takes two arguments split by
> the ';' symbol.  The first argument is related to the second argument and must include
> an truth value. In this example it would mean that the object $John
> is completely (u=1) in love with the object $Lucy.

## Keywords

* `let` declares a variable, for example: `let x y` or `let x, y` declares
  that the literals 'x' and y are variables in the current scope. Variables are 
  assumed to be universally quantified.
* `fn` defines a function. E.g: `fn::name` defined a function with name `name` (relational or otherwise depends on the context).

## Logic operators
  * `&&`   :  and
  * `||`   :  or
  * `=>`   :  material implication
  * `<=>`  :  biconditional
  * `:=`   :  entailment

# Full program examples

Complex sentences are composed of various atomic sentences connected by
connectives/operators and/or sentences which use quantified variables
(when used, variables must always be quantified).

In the consequent of an entailment only && operator can be nested.
Parentheses can be used to split complex sentences for comprehension and 
clarity. An example of a complex sentence:

```
(
    let x, y in 
    ( fn::takes[$analysis,u=1;x] && fn::takes[y,u=1;x] && course[y,u=1] && student[x,u=1] 
      := silly[x,u=1] )
)
```

In natural language: Every student that takes analysis and takes an other 
course is silly.
