# Playing With Elixir

This repository contains the source for the "Playing With Elixir" presentation
given at HEIG-VD St-Roch in Yverdon on February 10th 2020. It also links to all
relevant information.

<!-- START doctoc -->
<!-- END doctoc -->

## Contents

* [Landing page](https://alphahydrae.github.io/elixir-brownbag/)
  * [Source HTML](./index.html)
* [Slides](https://alphahydrae.github.io/elixir-brownbag/slides/)
  * [Source Markdown](./slides/SLIDES.md) (made with [Remark](https://remarkjs.com/))
* Demo 1 (by Saša Jurić)
  * [Script](./DEMO-1.md)
  * [Source code](https://github.com/sasa1977/demo_system)
* Demo 2
  * [Script](./DEMO-2.md)
  * [Source code](https://github.com/AlphaHydrae/boardr)

> The submodules in this repository point to the exact version of the demos used
> in the presentation.

## Requirements

* [Node.js](https://nodejs.org) v12.14.1

## Usage

```bash
git clone https://github.com/AlphaHydrae/elixir-brownbag.git
cd elixir-brownbag
git submodule update --init --recursive
npm install -g live-server
live-server --open=/slides .
```