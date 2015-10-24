# Generic Zebra Solver
A Prolog solver using CLPFD to any logic puzzle like the Zebra Puzzle

[![Build Status](https://travis-ci.org/fpg1503/Generic-Zebra-Solver.svg)](https://travis-ci.org/fpg1503/Generic-Zebra-Solver)

# Specification

## Solver Predicate
The predicate `solver(+Filename, -Solution)` is satisfied when `Solution` is the solution of the logic puzzle specified by the file named `Filename`.

## Puzzle definition

A puzzle is defined by a `.prob` file which consists in a **list of domains** and a **list of constraints**. There must be a blank line between the list of domains and the list of constraints.

### List of Domains
The domain list consists of the domain name followed by a colon and the list of possible values separated by commas. Each domains must be in a single line. All domains **must** have the same nember of possible values.

The domain `color` with the colors `blue`, `red`, and `black` can be represented as follows:

```
color: blue, red, black
```

analogously `nacionality` can be defined as:

```
nacionality: german, spanish, italian
```


### List of Constraints

A list of contraints consists of several constraints, one per line followed by an empty line.

Each constraint is represented by a logical expression which, by definition, is true. This expression is one of
- `LHS = RHS`
- `LHS < RHS`
- `LHS > RHS`

where `LHS` and `RHS` are mathematical expressions.

A constraint can also be
- `LHS or RHS`
where `LHS` and `RHS` are one of the logical expressions defined above.

For simplicity sake let's define a `value` as either an `integer` or a  `variable`. With that in mind a mathematical expression is one of

- `value`
- `value + value`
- `value - value`
- `abs(mathematical_expression)`

Some sample constraints are:

```
spanish = red + 1
german = blue
italian = 2
german = 3 or black - 2 = abs(red - blue)
```

## Solution definition

`Solution` is a Prolog list of `variable assignment`s. The first house is assumed to be number one.

### Variable Assignment
A `variable assignment` consists of the functor `c` with arity 2 where the first argument is the variable name and the second is the its value, for example

```
[c(italian,2),c(spanish,3),c(german,1),c(black,3),c(red,2),c(blue,1)]
```

# Contributing

Please fell free to fork, and create issues for any bugs/improvements/doubts.
