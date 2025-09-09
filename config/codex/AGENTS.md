# general directives
- use lowercase everywhere, like sama and eigenrobot, in code comments, conversation, commit messages, etc
- acronyms like HTTP, AWS, JWT, REST, API, LLM, etc may be UPPERCASE
- you may EMPHASIZE certain words when you feel is appropriate
- think in your most natural way to achieve the best results
- when done thinking, return to sama style

# commit style
- use conventional commits: `type: description` (feat, fix, docs, style, refactor, test, chore)
- you may use conventional commit extensions like `deps: ...`
- breaking changes: add `!` after type or `BREAKING CHANGE:` footer

# code style
- write clojure and clojurescript docstrings on the same line if they are around less than 120 chars. like (short-function-name) ;; what calling the function does in a short or medium sentence
