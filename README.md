# Playing With Elixir

This repository contains the source for the "Playing With Elixir" presentation
given at HEIG-VD St-Roch in Yverdon on February 10th 2020. It also links to all
relevant information.

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->


- [Contents](#contents)
- [Requirements](#requirements)
- [Usage](#usage)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->



## Contents

* [Landing page](https://alphahydrae.github.io/elixir-brownbag/)
  * [Source HTML](./index.html)
* [Slides](https://alphahydrae.github.io/elixir-brownbag/slides/)
  * [Source Markdown](./slides/SLIDES.md) (made with [Remark](https://remarkjs.com/))
  * [Sources (Videos, Books, Articles, Presentations)](./slides/SLIDES.md#sources)
* Demo 1 (by Saša Jurić)
  * [Script](./DEMO-1.md)
  * [Source code](https://github.com/sasa1977/demo_system)
* Demo 2
  * [Script](./DEMO-2.md)
  * [Source code](https://github.com/AlphaHydrae/boardr)

> The submodules in this repository point to the exact version of the demos used
> in the presentation.



## Requirements

* [Node.js](https://nodejs.org) 12.x



## Usage

```bash
git clone https://github.com/AlphaHydrae/elixir-brownbag.git
cd elixir-brownbag
git submodule update --init --recursive
npm install -g live-server
live-server --open=/slides .
```