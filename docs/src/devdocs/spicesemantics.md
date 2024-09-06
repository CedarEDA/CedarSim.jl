SPICE has ill-documented semantics, particularly with respect to expression evaluation.
We generally try to match ngspice semantics, because we can easily test against it
and the source is available under a permissive license, so we can double check what
the implemented semantics are. However, the `dialect` mechanism can be used to request
differing semantics. Depending on which SPICE dialtect you're targeting, the level of
maturity will differ though.

# NGSpice scope semantics:

This reflects our best understanding of what ngspice's evaluation semantics are supposed to be

- At every hierarchy level, parameter, subcircuit and model names are global. For parameters and models, later
  (lexically, including inside .lib and .include) definitions override earlier ones. For subcircuits,
  later definitions are ignored.

- For subcircuits, parameters that are not defined within the subcircuit are looked up in the *instantiating* scope (N.B.:,
  not the lexically enclosing scope)

## Parameters are evaluated in callee scope with the exception of itself
The following uses the outer parameter:
```
* Self-assignment test
.subckt test 1 2
.param x=x
R 1 2 {x}
.ends
.param x=1
V1 1 0 1
X1 1 0 test
```

While this is an error:
```
* Self-assignment test
.subckt test 1 2
.param x=y y=x
R 1 2 {x}
.ends
.param x=1
V1 1 0 1
X1 1 0 test
```

## Caller arguments are evaluated in callee scope
Consider the following:

```
* Callee scoping
.subckt 1 0 r=1
.param a=2
r1 1 0 r
.ends
.param a=3
x0 1 0 'a'
v0 1 0 1
```

Which value of `a` will r1 see? It turns out to be `2`, i.e. the evaluation will
scope to subcircuit's context. This is pretty bad both because it's not what
the user expects and also because it's a bit annoying to implement. Unfortunately,
that does appear to be the semantics that ngspice has.

To restore some semblence of sanity, we will pretend this isn't the case and if
necessary duplicate computation into the caller to handle this particular case.
