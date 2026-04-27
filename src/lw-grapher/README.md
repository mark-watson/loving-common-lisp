# LW-Grapher — Graph Visualizer for LispWorks CAPI

**Book Chapter:** [Knowledge Graph Navigator User Interface Using LispWorks CAPI](https://leanpub.com/read/lovinglisp/knowledge-graph-navigator-user-interface-using-lispworks-capi) — *Loving Common Lisp* (free to read online).

A simple graph layout and visualization library built on the LispWorks CAPI pinboard. It renders nodes as labeled ellipses connected by edges, using an automatic layout algorithm adapted from Gabriel Robbins' ISI-Grapher.

This library is used by the Knowledge Graph Navigator CAPI UI (`kgn-capi-ui`) to display entity relationship graphs returned from DBpedia SPARQL queries.

> **Note:** Requires LispWorks (commercial). Will not run on SBCL or other free implementations.

## Algorithm

The layout algorithm is based on the ISI-Grapher system by Gabriel Robbins. See the original manual:

[The ISI-Grapher Manual (PDF)](http://www.cs.virginia.edu/~robins/papers/The_ISI_Grapher_Manual.pdf)

## Usage

```lisp
(ql:quickload "lw-grapher")

;; See g-test.lisp for example graph definitions
```

## Files

| File | Description |
|------|-------------|
| `lw-grapher.lisp` | Core node drawing and CAPI pinboard objects |
| `grapher.lisp` | Graph layout algorithm |
| `text-node.lisp` | Text node rendering |
| `info-pane-grapher.lisp` | Info pane integration |
| `g-test.lisp` | Test/demo graph definitions |
| `package.lisp` | Package definition |
