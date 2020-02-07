class: center, middle, inverse

# Playing With Elixir

### Have some delicious buzzwords

dynamic multi-paradigm programming language, declarative functional programming,
immutable data structures, massive scalability, soft real-time systems, high
availability, maintainability, hot code swapping, low latency, share-nothing
message passing concurrency, preemptive scheduling, distribution, fault
tolerance, remote debugging, parallelized automated tests, doctests

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

class: center, hidden-header

# Credits

.third-column.speaker[
**José Valim**

![José Valim](../img/speaker02b.jpg)

.sources[
The [Elixir](https://elixir-lang.org) community
]
]

.third-column.speaker[
**Joe Armstrong**

![Joe Armstrong](../img/speaker01b.jpg)

.sources[
* [How we program multicores • SICS Software Week 2016](https://www.youtube.com/watch?v=bo5WL5IQAd0)
* [Faults, Scaling, and Erlang Concurrency • Standford Seminar 2014](https://youtu.be/YaUPdgtUYko)
]
]

.third-column.speaker[
**Saša Jurić**

![Saša Jurić](../img/speaker03b.jpg)

.sources[
[The Soul of Erlang and Elixir • GOTO 2019](https://gotocph.com/2019/sessions/1013/the-soul-of-erlang-and-elixir)
]
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

<img src="../img/elixir-orig.png" width=40% />
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

Why not have **both**?

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

## Elixir 101 - First-class Functions

**Modules**

.half-column[
```elixir
defmodule Math do
  def add(a, b) do
    a + b
  end
end
```
]

.half-column[
```elixir
iex> Math.add(1, 2)
3
```

```elixir
iex> add = &Math.add/2
iex> add.(1, 2)
3
```
]

<div class='clear'></div>

**Anonymous functions**

.half-column[
```elixir
hello = fn -> IO.puts "Hello World" end
```
]

.half-column[
```elixir
iex> hello.()  # "Hello World"
:ok
```
]

<div class='clear' />

???

* Module not a class but a collection of functions.
* No objects, no instantiation, no `this`.
* (Processes later.)

---

class: center, middle, hidden-header

## Functional programming

<img src='../img/fp.png' width='60%' />

???

* *"FP emphasizes the evaluation of expressions rather than the execution of commands"*
* Pragmatic functional language, not religious about it like Haskell.

---

### Immutability

```elixir
iex> person = %{first: "John", last: "Doe"}
iex> person_with_age = Map.put(person, :age, 40)
iex>
iex> person_with_age.first  # "John"
iex> person_with_age.age    # 40
iex> person.age             # nil
```

---

### Transformation

```elixir
fact = "Elixir Rocks"
String.upcase(List.first(String.split(fact, " ")))  # "ELIXIR"
```

--

.half-column[
```elixir
String.upcase(
  List.first(
    String.split(
      fact,
      " "
    )
  )
)  # "ELIXIR"
```
]

.half-column[
```elixir
fact_parts = String.split(fact, " ")
first_part = List.first(fact_parts)
String.upcase(first_part)  # "ELIXIR"
```
]

--

<br class='clear' />

```elixir
"Elixir Rocks"
|> String.split(" ")
|> List.first()
|> String.upcase()  # "ELIXIR"
```

???

* *"Functional programming is nothing more than transformation."*

---

### Pattern matching

.half-column[
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

.half-column[
```elixir
iex> import Fibonacci
Fibonacci

iex> fib 0
0
iex> fib 1
1
iex> fib 2
1
iex> fib 3
2
iex> fib 4
3

iex> fib -1
** (FunctionClauseError) no function clause
   matching in Fibonacci.fib/1

  The following arguments were given:

  # 1
  -1

  iex:2: Fibonacci.fib/1
```
]

---

### A practical example

```js
function authMiddleware(req, res, next) {

  const authorizationHeader = req.get('Authorization');
  if (!authorizationHeader) {
    throw new Error('Authorization header is missing');
  }

  const match = authorizationHeader.match(/^Bearer (.+)$/);
  if (!match) {
    throw new Error('Authorization header is malformed');
  }

  return match[1];
}
```

---

### Doing it with Phoenix

```elixir
defmodule MyApp.MyController do
  use MyApp, :controller

  def get_authorization(%Conn{} = conn) do
    conn
    |> get_req_header("authorization")
    |> get_authorization()
  end

  def get_authorization([_, _ | _]) do
    {:error, :auth_header_duplicated}
  end

  def get_authorization(["Bearer " <> token]) do
    {:ok, token}
  end

  def get_authorization([_]) do
    {:error, :auth_header_malformed}
  end

  def get_authorization([]) do
    {:error, :auth_header_missing}
  end
end
```

---

* Pattern matching
  * Case
  * https://github.com/tc39/proposal-pattern-matching
  * Guards

---

## Typespecs and behaviors

https://elixir-lang.org/getting-started/typespecs-and-behaviours.html

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

Show if and + implementation to demonstrate macros and syntactic sugar
Elixir implemented in Elixir
AST written in elixir terms, quote/unquote
Kernel specialforms

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
* Introspectable runtime
  * Observer

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

Takes full advantage of the Erlang concurrency model and 30+ years of libraries
and experience running large scale systems.

WhatsApp, CouchDB, Riak, Rabbit MQ

---

class: center, middle, inverse

# The Erlang Concurrency Model

---

class: center, middle

<img src='../img/ericsson.png' width='50%' />

???

Ericsson had large computer networks before the internet (20-30 years), Erlang designed for big worldwide infrastructure

---

* Multicore
  * Why doesn't my program run N times faster on an N core computer?
  * Have to write a parallel program
  * Massively parallel problems are extremely common: www, phone networks
  * We want to make it easy to write parallel programs: we can’t. Too complicated.
  * Relatively easy to write a concurrent program
* Goals
  * Concurrency
  * Fault tolerance
* Concurrency
  * Concurrency vs. parallelism
  * True parallelism impossible on single core (hyperthreading, superscalar processor)
* Problems with shared memory
  * Handled by the operating system, not the programming language
  * Programming parallel programs in languages that have the wrong primitives, and semantically different, makes it difficult
  * Extremely difficult to get right and to understand
  * Simultaneity violates the laws of physics
* Message passing
  * Erlang/Elixir is a simple dynamic functional language, purely sequential, easy to understand
  * Process
    * Very lightweight compared to OS process (130 mio process limit, Smallest OS of process: ~64kb, Erlang process 309 words (233 for the heap): http://erlang.org/doc/efficiency_guide/processes.html)
  * Isolated processes communicate through message passing
    * Messaging decouples, OOP Alan Kay
      * Big idea of OO was the messaging, not the organization of objects and methods, also big idea in Unix pipes, decoupling source and destination
  * 3 concurrency primitives: spawn, send, receive (single instruction when compiled)
    * Erlang & Elixir primitives side by side (https://elixir-lang.org/getting-started/processes.html)
    * Concurrent programming should be at the lowest level possible in the programming language
    * Erlang system is very operating-system-like
  * No shared memory, mutex, locks, etc. Pure message passing.
  * Immutability: everything is copied, no pointer, you either get the message or you don't
    * Not religious FP: ETS tables, don't change the entire universe every time you change something in it
  * Each process: separate heap, separate stack, separately garbage collected, no shared anything
  * Failure semantics the same as in the real world: you send a message, it either gets there or it doesn't, and you'll never know
* Easier to understand
  * Single site where things happen
  * Erlang processes obey the laws of physics:
    * No simultaneity: a single place consumes a single stream of events
    * Messages travel at or under the speed of light, if A and B separated in space and B depends on A, A must send message to B before B can do anything, simultaneity is impossible
    * We only know how things were, not how they are
  * Million users: million processes, the concurrency of the system is reflected in the architecture
  * Process granularity defined by observing the world (e.g. phone conversation)
  * Actor model, physics modeling (Hewitt)
  * Classic web server shared memory vs. Erlang isolated processes
    * Apache php has to spawn os processes, cannot do more than 10k
* Automate parallelization (experts)
  * Scheduling multi cores is NP hard
  * Process sand, barrels to fill
  * Schedulers

* Fault tolerance
  * Stuff fails, shit happens
  * Impossible with one computer (cacti)
  * Shared memory: write differently for single core or multiple machines
  * Need one programming model for local and distributed programming
  * Isolation
    * Copying is inefficient but required for fault tolerance
  * More computers increases reliability as long as they are independent
  * Like to write correct programs but impossible to write correct big programs, must handle errors, evolve and run forever, like biological systems
  * Von Neumann: treat computer systems as biological systems: failure is an inherent part of its operation, we have to detect it and recover from it
  * Defensive programming: check args all the time. In a sequential program you lose everything if it crashes. In Erlang you can have as many processes as you want
  * Different failure model: instead of one very fast delivery car, many little taxis, you don’t care if a few fail
  * Let it crash
  * Do not be lenient with input, programs should crash when going off spec
  * Monitor and restart
  * Software faults are soft - the Bohrbug/Heisenbug hypothesis
    * “If the program state is reinitialised and the failed operation retried, the operation will usually not fail the second time.”
  * Errors: complexity, reproducibility, frequency? (diagram)
* OTP
  * OTP is like Unix for C: large set of libraries for the language Erlang
  * Links, must detect failure, works whether local or distributed
  * Failure: let somebody else fix the problem, same model for separate processes or machines (heart attack, unknown state)
  * Supervisors

* Erlang fits multicore
  * Fault tolerance implies isolation implies possible parallelism implies scalability
  * Message passing between isolated processes scales nicely
  * It just went faster
    * Tandem computers also noticed FT scales quite well
  * Goal in Erlang is that a program with a reasonable balance of processes scales automatically on multi cores (0.75)

* Erlang: very lightweight processes, very fast message passing, total separation between processes, automatic marshalling/demarshalling, fast sequential code, strict functional code, dynamic typing, transparent distribution, compose sequential and concurrent code

---

Images:

* cacti-fault-tolerance
* concurrency-vs-parallelism
* concurrency
* parallelism
* erlang-the-movie
* let-it-crash
* links
* message-passing-mailbox
* message-passing
* otp-common
* otp
* supervision-tree
* supervisor-one-for-all
* supervisor-one-for-one
* supervisor-rest-for-one

---

class: center, middle

AXD301 Asynchronous Transfer Mode (ATM) Telephone Switch (1998)

<img src='../img/axd301.png' width='40%'>

Over 1,000,000 lines of Erlang. Reported to achieve nine "9"s availability.

---

class: center, middle

*"Why doesn't my program run N times faster on an N-core computer?"*

.muted.smaller[Anonymous programmer]

---

class: center, middle, hidden-header

## Programming for multicores

<img src='../img/multi-core.png' width='85%' />

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

???

Message passing: care about correctness not efficiency. Efficiency comes from parallelism

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

---

## Sources


.half-column[
**Books**

.sources[
* [Learn You Some Erlang For Great Good](https://learnyousomeerlang.com)
* [Stuff Goes Bad: Erlang in Anger](https://www.erlang-in-anger.com)
]
]

.half-column[
**Presentations**

.sources[
* [The Zen Of Erlang](https://ferd.ca/the-zen-of-erlang.html)
* [An Introduction to Erlang • Mirko Bonadei](https://www.slideshare.net/MirkoBonadei/an-introduction-to-erlang)
]
]

**Articles & Papers**

.sources[
* [Which Companies are Using Erlang and why? • Erlang Solutions](https://www.erlang-solutions.com/blog/which-companies-are-using-erlang-and-why-mytopdogstatus.html)
* [Convenience Over Correctness • Steve Vinoski • IEEE](http://steve.vinoski.net/pdf/IEEE-Convenience_Over_Correctness.pdf)
* [Using Rust to Scale Elixir for 11 Million Concurrent Users • Matt Nowack • Discord](https://blog.discordapp.com/using-rust-to-scale-elixir-for-11-million-concurrent-users-c6f19fc029d3)
* [Robust compute for RDF queries • Managing fault tolerance in Elixir with supervision trees • Tonny Hammond](https://medium.com/@tonyhammond/robust-compute-for-rdf-queries-eb2ad665ef12)
* [Understanding Elixir Macros • Saša Jurić](https://www.theerlangelist.com/article/macros_1)
]

---

## Sources • Videos

**Main Inspiration**

.sources[
* [The Soul of Erlang and Elixir • Saša Jurić • GOTO 2019](https://youtu.be/JvBT4XBdoUE)
* [How we program multicores • Joe Armstrong • SICS Software Week 2016](https://youtu.be/bo5WL5IQAd0)
* [Faults, Scaling, and Erlang Concurrency • Stanford Seminar 2014](https://youtu.be/YaUPdgtUYko)
]

**Erlang, Concurrent Programming, Fault Tolerance**

.sources[
* [Erlang: The Movie](https://www.youtube.com/watch?v=xrIjfIjssLE)
* [Computer Science — A Guide for the Perplexed • Joe Armstrong • GOTO 2018](https://youtu.be/rmueBVrLKcY)
* [The Do's and Don'ts of Error Handling • Joe Armstrong • GOTO 2018](https://youtu.be/TTM_b7EJg5E)
* [Breaking Open: Erlang • Joe Armstrong](https://youtu.be/m5RWdNBPsTY)
* [Let's #TalkConcurrency with Joe Armstrong • Cambridge University 2018](https://youtu.be/i9Kf12NMPWE)
* [Why We Chose Erlang over Java, Scala, Go, C • Colin Hemmings • QCon London 2017](https://youtu.be/OcExABAAsXs)
* [Hitchhiker's Tour of the BEAM • Robert Virding • Erlang User Conference 2014](https://youtu.be/_Pwlvy3zz9M)
* [Building A Highly Scalable Service that Survived A Super Bowl • Keith Elder • Code BEAM SF 19](https://youtu.be/lXiiiLhwBI4)
]

---

## Sources • Videos • Elixir

.sources[
.half-column[
* [Elixir — A Mini-Documentary](https://youtu.be/lxYFOM3UJzo)
* [The Climb: Experiencing the Rise of Elixir from the Inside • Erlang Factory SF 2016](https://youtu.be/fklep3sUSWo)
* [Phoenix and Elm — Making the Web Functional • Erlang Factory SF 2016 Keynote](https://youtu.be/XJ9ckqCMiKk)
* [Don't Write Macros But Do Learn How They Work • Jesse Anderson • ElixirConf 2017](https://youtu.be/Bo48sQDb-hk)
* [Elixir in Elixir • Jay Hayes • ElixirConf 2016](https://youtu.be/p8MGNw045AE)
* [Write Less, Do More (and Have Fun!) with Elixir Macros • Erlang Factory 2014](https://youtu.be/mkoYqXdXl5Y)
* [High Availability • Saša Jurić • ElixirConf EU 2015](https://youtu.be/wYttHG3S76Y)
* [The ABCs of OTP • Jesse J. Anderson • EEF17 Conference](https://youtu.be/4SCwubzqsVU)
* [What every Node.js developer needs to know about Elixir • Bryan Hunter • NDC Conferences 2016](https://youtu.be/q8wueg2hswA)
* [Keynote: Phoenix 1.3 • Chris McCord • Lonestar ElixirConf 2017](https://youtu.be/tMO28ar0lW8)
* [Idioms for building distributed fault-tolerant applications with Elixir • José Valim • Curry On Barcelona 2017](https://youtu.be/MMfYXEH9KsY)
* [Elixir Umbrella — Microservices or Majestic Monolith? • Georgina McFadyen • Elixir.LDN 2017](https://youtu.be/jhZwQ1LTdUI)
* [Perhap: Applying Domain Driven Design and Reactive Architectures to Functional Programming • Rob Martin • ElixirConf 2017](https://youtu.be/kq4qTk18N-c)
]
.half-column[
* [Solid Ground • Saša Jurić • EMPEX Conference 2017](https://youtu.be/pO4_Wlq8JeI)
* [Elixir: The only Sane Choice in an Insane World • Brian Cardarella • GOTO 2017](https://youtu.be/gom6nEvtl3U)
* [Phoenix a Web Framework for the New Web • José Valim • GOTO 2016](https://youtu.be/bk3icU8iIto)
* [Why Elixir Matters: A Genealogy of Functional Programming • Osayame Gaius-Obaseki • ElixirDaze 2018](https://youtu.be/cWAHpvkh8Vs)
* [Discovering Processes • Saša Jurić • ElixirConfEU 2016](https://youtu.be/y_b6RTes83c)
* [High availability with Elixir and Erlang • Saša Jurić • Full Stack Fest 2016](https://youtu.be/Ba3aCm3A0o8)
* [Intro to OTP in Elixir • Jesse J. Anderson • Full Stack Talks 2016](https://youtu.be/CJT8wPnmjTM)
* [Processes and Supervision in Elixir • Steve Grossi • Indy Elixir 2016](https://youtu.be/eUxang6_WQA)
* [Docker and OTP Friends or Foes • Daniel Azuma • ElixirConf 2018](https://youtu.be/nLApFANtkHs)
* [Phoenix a web framework for the new web • Jose Valim • Lambda Days 2016](https://youtu.be/MD3P7Qan3pw)
* [Consistent, Distributed Elixir • Chris Keathley • ElixirDaze 2018](https://youtu.be/CmMMpaUD3g8)
]
]

---

class: center, middle, inverse

## Backup

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