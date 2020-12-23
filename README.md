# Untyped

An untyped lambda calculus parser, evaluator, and REPL, written in Haskell. 

## Build & Run
- [`docker`](#docker)
- [`stack`](#stack)

### `docker`

#### Build
```
docker build -t lambda-calculus .
```

#### Launch REPL
```
docker run -it lambda-calculus
```

### `stack`

#### Build
```
stack build
```

#### Launch REPL
```
stack exec lambda-calculus-exe
```

## REPL Commands

- [`let`](#let-bindings)
- [`?`](#show-context)
- [`βnf`](#evaluate-to-β-normal-form)
- [`trace`](#trace-β-normal-form-evaluation)
- [`η-equivalance`](#η-equivalance)
- [`fv`](#free-variables)


### `let` bindings

#### Syntax

```haskell
let {var} = {expr}
```
#### Examples
```haskell
λ> let q = λx.x
q := λx.x
```

```haskell
λ> let y = (λxyz.xz(yz))(λx.z)(λx.a)
y := (λx.λy.λz.xz(yz))(λx.z)(λx.a)
```

### Show context

#### Syntax
```haskell
?
```

#### Examples
```haskell
λ> ?
y := (λx.λy.λz.xz(yz))(λx.z)(λx.a)
q := λx.x
```

### Evaluate to β-normal-form

#### Syntax
```haskell
! {expr}
```
#### Examples

```haskell
λ> ! (λxyz.xz(yz))(λx.z)(λx.a)
λw.za
```

```haskell
λ> let y = (λxyz.xz(yz))(λx.z)(λx.a)
λ> ! y
λw.za
```

### Trace β-normal-form evaluation

#### Syntax
```haskell
trace {expr}
```

#### Examples
```haskell
λ> trace (λxyz.xz(yz))(λx.z)(λx.a)
Γ, a, z, (λx.λy.λz.xz(yz))(λx.z)(λx.a)
--------------------------------------
(λx.λy.λz.xz(yz))(λx.z)(λx.a)
-----------------------------
(λy.λw.(λx.z)w(yw))(λx.a)
-------------------------
λw.(λx.z)w((λx.a)w)
-------------------
λw.z((λx.a)w)
-------------
λw.za
```

### η-equivalance 
> (evaluate to β-normal-form and check α-equivalence)

#### Syntax
```haskell
= {expr} {expr}
```

#### Examples

```haskell
λ> let z = λx.x
λ> let y = λq.q
λ> = z y
True
  via α?.β
    λx.x
    λq.q
```

```haskell
λ> let z = (λz.z)(λz.zz)(λz.zy)
λ> let w = (λx.λy.xyy)(λy.y)y
λ> = z w
True
  via α?.β
    yy
    yy
```

### Free Variables

#### Syntax
```haskell
fv {expr}
```
#### Examples

```haskell
λ> fv (λxyz.xz(yz))(λx.z)(λx.a)
{a,z}
```
