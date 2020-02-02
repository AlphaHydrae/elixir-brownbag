class: center, middle, inverse

# Playing With Elixir

### Have some delicious buzzwords

dynamic multi-paradigm programming language, declarative functional programming, immutable data structures,
massive scalability, soft real-time systems, high availability, maintainability, hot code swapping,
low latency, message passing concurrency, preemptive scheduling, distribution, fault tolerance,
remote debugging, parallelized automated tests, doctests

---

class: center, middle, inverse

# Agenda

.agenda[
1. Introduction to Elixir
2. The Erlang Concurrency Model
3. Demo: Highly Available & Fault Tolerant Elixir
4. Demo: Playing With Elixir
]

???

Disclaimer: WOW effect.

---

class: center

# Credits

.third-column.speaker[
  ![José Valim](../img/speaker02b.jpg)

  [José Valim](https://github.com/josevalim) and the [Elixir](https://elixir-lang.org) community
]

.third-column.speaker[
  ![Joe Armstrong](../img/speaker01b.jpg)

  [How we program multicores - Joe Armstrong - SICS Software Week 2016](https://www.youtube.com/watch?v=bo5WL5IQAd0)
]

.third-column.speaker[
  ![Saša Jurić](../img/speaker03b.jpg)

  [The Soul of Erlang and Elixir - Saša Jurić - GOTO 2019](https://gotocph.com/2019/sessions/1013/the-soul-of-erlang-and-elixir)
]

---

class: center, middle, inverse

# Introduction to Elixir

---

class: center, middle

.half-column[
![2012](../img/2012.jpg)
]

.half-column[
## Origins

[Plataformatec](https://plataformatec.com/en/), a software consultancy founded in São Paulo, Brazil

R&D project led by José Valim, former member of the [Ruby on Rails](https://rubyonrails.org) core team

Elixir core team now an [independent open source organization](http://blog.plataformatec.com.br/2020/01/important-information-about-our-elixir-and-ruby-open-source-projects/)

<img src="../img/elixir-orig.png" width=50% />
]

???

Plataformatec [active in the Ruby community](https://plataformatec.com/en/why-us/ruby/):

* [Devise](https://github.com/heartcombo/devise)
* [Simple Form](https://github.com/heartcombo/simple_form)

Also contribute to the Elixir community:

* [Elixir](https://elixir-lang.org)
* [Ecto](https://hexdocs.pm/ecto/Ecto.html)
* [Phoenix](http://www.phoenixframework.org)

---

class: center, middle

## Why make Elixir?

Productivity
.muted[vs.]
Performance

**Why not have both?**

???

Started studying Erlang after trying to make Ruby on Rails thread-safe.

Productivity vs. performance example: Ruby vs. Java.

It's a false dychotomy.

---

class: center-headers, middle

.half-column[
## Erlang

.center.muted.smaller[1986]
.center.muted.smaller[[Prolog](https://en.wikipedia.org/wiki/Prolog)-like syntax]

```erlang
-module(fibonacci).
-export([fib/1]).

fib(0) ->
  0;
fib(1) ->
  1;
fib(N) when N >= 2 ->
  fib(N - 1) + fib(N - 2).
```
]

.half-column[
## Elixir

.center.muted.smaller[2012, v1 in 2014]
.center.muted.smaller[[Ruby](https://www.ruby-lang.org)-like syntax]

```elixir
defmodule Fibonacci do
  def fib(0), do: 0
  def fib(1), do: 1
  def fib(n) when n >= 2 do
    fib(n - 1) + fib(n - 2)
  end
end
```
]

<div class="clear" />

.center[
  Both are **compiled** and run on the [**BEAM virtual machine**](https://en.wikipedia.org/wiki/BEAM_%28Erlang_virtual_machine%29).

  .muted.smaller[Like these: [Alpaca](https://github.com/alpaca-lang/alpaca), [Gleam](https://gleam.run), [LFE](http://lfe.io)]
]

---

## Elixir 101 - Types

**Basic Types**

```elixir
$> iex
iex> 1         # integer
iex> 1.0       # float
iex> true      # boolean
iex> :atom     # atom / symbol
iex> "elixir"  # string (more or less...)
iex> self()    # pid (not yet...)
```

**Basic Structures**

```elixir
iex> [1, 2, 3]          # linked list
iex> {"life", 42}       # tuple
iex> %{"foo" => "bar"}  # map
```

**Atoms In Disguise**

```elixir
iex> nil === :nil      # true
iex> true === :true    # true
iex> false === :false  # true
```

---

## Elixir 101 - Strings... but are they really?

There are actually no strings in Elixir, just binaries:

```elixir
iex> is_string("foo")
** (CompileError) iex:10: undefined function is_string/1
iex> is_binary("foo")
true
iex> <<102, 111, 111>> == "foo"
true
iex> <<256 :: utf8>> == "Ā"
true
iex> <<196, 128>> == "Ā"
true
```

And actually, binaries are just bitstrings with a number of bits divisible by 8:

```elixir
iex> is_bitstring("foo")
true
iex> <<102 :: size(8), 111 :: size(8), 111 :: size(8)>>
"foo"
iex> <<2::size(2), 3 :: size(3)>>  # 10011
<<19::size(5)>>
```

---

## Elixir 101 - Modules & functions

```elixir
defmodule Hello do
  def world do
    IO.puts "Hello World"
  end
end
```

---

## Elixir 101 - Guards

When

---

## Functional programming

* Pragmatic
* Expression (no returns)
* Immutability
* Pipe operator
* No objects
* Pattern matching
  * Case
  * Function definitions
  * Split strings
  * Authorization header example

---

class: center, middle

## Meta programming

.lang-makeup[
![PHP language make-up](../img/lang-makeup-php.png)
![Swift language make-up](../img/lang-makeup-swift.png)
![Node.js language make-up](../img/lang-makeup-node.png)
![Ruby language make-up](../img/lang-makeup-ruby.png)
![Python language make-up](../img/lang-makeup-python.png)
![Kotlin language make-up](../img/lang-makeup-kotlin.png)
![Elixir language make-up](../img/lang-makeup-elixir.png)
]

???

The third language is Node.js.

---

### Meta programming example

TODO: meta programming example

---

## Modern programming language

TODO: toolset image

* Build tool
* Automated test framework
  * Parallel tests
* Package manager
* Great documentation
  * Documentation generator
  * Fun help in shell
* Code formatter
* Production releases

TODO: what if I told you?

---

## Fully compatible with Erlang

Elixir compiler: Elixir -> Elixir AST -> Erlang AST -> Erlang compiler

Modules are atoms in both Erlang and Elixir:

```elixir
iex> Enum == :'Elixir.Enum'
true
iex> Enum.empty?([])
true
iex> :'Elixir.Enum'.empty?([])
true
iex> :rand.uniform()
0.8175669086010815
iex> Base.encode16(:crypto.hash(:sha256, "Elixir"))
"3315715A7A3AD57428298676C5AE465DADA38D951BDFAC9348A8A31E9C7401CB"
```

Call Erlang libraries with **no runtime cost**.

---

## The power of the BEAM

Elixir runs on the BEAM virtual machine.

Takes full advantage of the Erlang concurrency model and 30+ years of experience
running large scale systems.

---

class: center, middle, inverse

# The Erlang Concurrency Model

---

class: center, middle

*"Why doesn't my program run N times faster on an N-core computer?"*

.muted.smaller[Anonymous programmer]

---

class: center, middle

## Programming for multicores

<img src='../img/multi-core.png' width='100%' />

---

## Shared memory concurrency

* Threads
* Extremely difficult to get right
* Simultaneity violates the laws of physics

---

## Fault tolerance

* Impossible on one computer

---

class: center, middle

*"Make it right before you make it fast."*

*"Make it beautiful before you make it faster."*

*"Keep it right when you make it faster."*

.muted.smaller[Joe Armstrong]

---

## Fault tolerance

* How to program large real-time systems better?
* Main concerns: fault tolerance & high availability
* Impossible with one computer (cacti)
* Do not violate the laws of physics

---

## Concepts

* Background jobs
* Promises (Task)
* Observables (GenStage)

---

class: middle

## Make it right before you make it fast

.half-column[
**Q:** *"Did you have to change the original design to make it faster?"*
]

--

.half-column[
**A:** *"No it just went faster."*
]