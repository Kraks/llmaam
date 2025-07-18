## Hallucination-Resilient LLM-Driven Sound Abstract Interpretation

Prototype abstract interpreter using LLMs to improve precision while maintaining soundness.

### Get Started

Install `sbt` and JVM. Once started with a `sbt` session, you can compile code
using command `compile`, run a main function using `run`, execute all test
cases using `test`.
You can also run a specific test given the package path, e.g. `testOnly llmaam.aam.TestAAM`.

Currently, the code base supports Google Gemini and OpenAI API.
You need to put a Google Gemini API key in file `GEMINI_AI_KEY` or
OpenAI API in file `OPENAI_API_KEY`.

### Examples

See some visualized analysis results of using different allocation strategies in `examples` folder.

An example is the `stack2` program to demonstrate spurious binding and return flow:

```
let id = λz. z in
let idid = λw. id(w) in
let x = idid(1) in
let y = idid(2) in
x
```

Neither [0CFA](examples/stack2_0cfa.pdf), [1CFA](examples/stack2_1cfa.pdf), or
the [combination](examples/stack2_0cfa_p4f.pdf) of 0CFA and [P4F](https://dl.acm.org/doi/10.1145/2837614.2837631)
yields a precision abstract state graph.
Using the [combination](examples/stack2_1cfa_p4f.pdf) of 1CFA and P4F can
achieve the optimal precision (i.e., no spurious state transition, being
identical to concrete execution).

This project demonstrates that using [LLM as abstract address allocator](examples/stack2_gemini25_jun7.pdf)
can achieve the optimal precision too.
The allocation strategy is neither fixed k-CFA nor P4F, but entirely adaptive
to the program and "runtime" abstract state (while still remain sound).
See the system prompt and conversion with LLM [here](examples/stack2_gemini25_jun7_chat.txt).

### TODO

- [x] tweak prompt so that the response can be reified back to Scala code
- [x] OpenAI support
- [x] support conditionals and boolean
- [x] support loops
- [x] support mutable states
- [x] make use of ErrState (eg. adding a function with a number produce ErrState)
- [x] a Scheme parser (to handle some [benchmarks](https://github.com/TimWhiting/context-sensitive-demand-cfa/tree/main/evaluation/benchmarks))
- [ ] Refactor desugaring from Scheme to core syntax
- [ ] Deepseek, QWen API support
- [ ] Give better prompt (eg existing strategy etc) to guide LLM make better decisions
- [ ] support call/cc
