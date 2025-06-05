## LLM-Driven Sound Abstract Interpretation

### Get Started

Install `sbt` and JVM. You can compile code with `sbt compile`, run it with `sbt run`, and `sbt console` will start a Scala 3 REPL.

In `sbt`, you can a specific test given the path, e.g. `testOnly llmaam.aam.TestAAM`.

Currently, the code base only support Google Gemini API.
You need to put a Google Gemini API key in file `GEMINI_AI_KEY`.

### Examples

See some visualized analysis results of using different allocation strategies in `examples` folder.

### TODO

- [ ] OpenAI and Deepseek API support
- [ ] tweak prompt (LLMAllocator) so that the response can be reified back to Scala code
- [ ] support conditionals and boolean
- [ ] support loops
- [ ] support mutable states
- [ ] support call/cc
