# Demo 1

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->


- [Initial setup](#initial-setup)
- [Setup](#setup)
- [Introduction](#introduction)
- [Part 1](#part-1)
- [Part 2](#part-2)
- [Part 3](#part-3)
- [Part 4](#part-4)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Initial setup

> Perform once in new checkout.

```bash
git submodule update --init --recursive

cd elixir-demo-system/example_system

mix deps.get && \
  pushd assets && \
  npm install && \
  popd && \
  mix compile

mix test
```

## Setup

> Prepare before demo.

* Build & start production release:

  ```bash
  cd elixir-demo-system/example_system
  ./rebuild.sh
  ./_build/prod/rel/system/bin/system start
  ```
* Open http://localhost:4000 with developer console showing network & WebSocket activity.
* Open http://localhost:4000/load.
* Open 3 terminals:
  * Tail log:

    ```bash
    tail -f _build/prod/rel/system/var/log/erlang.log.1
    ```
  * Prepare to run remote console:

    ```bash
    ./_build/prod/rel/system/bin/system remote_console
    ```
  * Prepare to run hot upgrade:

    ```bash
    mix system.upgrade
    ```

## Introduction

* Explain demo system.
  * Background daemon in production mode.
  * Single BEAM VM instance and single OS process.
  * 1 scheduler to simulate a single-core machine.
* Generate synthetic load with 10,000 jobs.
  * Show 10k successes per second.
* Show user-facing UI.
* Explain process architecture:
  * One process per WebSocket connection (client).
  * Calculation is performed in a separate one-off process that is short-lived.

## Part 1

* As any real production system, it suffers from bugs.
* Submit number 13:
  * Show terminated task process in log.
  * Show 10k successes per second still steady even with a bunch of errors.
* The connection was made aware of the crash of the calculation process through
  a message, and can notify the client with no service interruption. This is an
  example of the "let it crash" philosophy at work.

## Part 2

* The implementation is naive.
* Submit number 999,999,999:
  * Show scheduler maxing out at 100% CPU (of one core).
  * Show system still responsive.
  * Show 10k successes per second still steady despite the fact that it is a
    single-threaded system (one scheduler).
  * Show WebSocket connection still open.
  * The BEAM VM does very quick context switching with preemptive scheduling.
* **Trade-off**: the BEAM always favors a fair distribution of time for every
  process in the system. It promotes the progress of the system as a whole at
  the expense of the maximum efficiency of any single activity in the system.

  This is geared towards massively parallel software systems like web
  applications.
* Process isolation separates failure and latency.

## Part 3

* Submit number -1:
  * Show scheduler maxing out due to activity gone rogue.
  * Show system still responsive and 10k successes per second still steady
    thanks to BEAM and process architecture.
  * The system will of course grind to a halt with too many loops like this.
* Open remote console and show process information:

  ```bash
  # Run remote console in the context of the running system.
  ./_build/prod/rel/system/bin/system remote_console
  # List processes (the system is still responsive).
  > Process.list()
  # Show process info (reductions).
  > Process.info(hd(Process.list()))
  # Utility function to find the process with the most reductions in the last second.
  > Runtime.top()
  # Check again.
  > Runtime.top()
  # Get the culprit.
  > pid = hd(Runtime.top()).pid
  # Show its information (can still ask the BEAM about it despite it being unresponsive).
  > Process.info(pid)
  # Get its stacktrace for debugging.
  > Process.info(pid, :current_stacktrace)
  # Trace its activity for 1 second or 50 function invocations (whichever takes place first).
  > Runtime.trace(pid)
  # Kill it (like kill -9).
  > Process.kill(pid, :kill)
  ```
* Show aftermath:
  * Scheduler CPU usage has dropped to expected levels.
  * The error has been reported to the client
  * Show 10k successes per second still steady.
  * Show WebSocket connection still open.

## Part 4

* Show problem in `lib/example_system/math.ex`.
* Show validations in `lib/example_system_web/math/sum.ex`.
* Fix:
  * Uncomment validation in `lib/example_system_web/math/sum.ex`.
  * Remove number 13 error in `lib/example_system/math.ex`.
  * Improve calculation in `lib/example_system/math.ex`:

    ```elixir
    defp calc_sum(n), do: div(n * (n + 1), 2)
    ```
* Run `mix system.upgrade`:
  * Show 10k successes per second still steady.
  * Show WebSocket connection still open.
  * Show previous errors all solved.
  * Show logs indicating module upgrade.
* System remained available, debuggable and upgradable without disturbing
  anything.