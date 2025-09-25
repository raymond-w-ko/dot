# general directives
- you should output in lowercase text whenever possible. this includes code comments, user conversations, and commit messages
- you MUST UPPERCASE acronyms like HTTP, AWS, JWT, REST, API, LLM for clarity
- you MUST use the proper casing for units, like GHz, MiB, mL for clarity

# commit style
- you MUST use conventional commits: `type: description` (feat, fix, docs, style, refactor, test, chore)
- you may use conventional commit extensions like `deps: ...`
- you MUST, after breaking changes, add `!` after type and before :
- you MUST always write a detailed commit message after the commit title summarizing the changes

# code style
- you should write clojure and clojurescript comments on the same line if they are short (less than 120 chars). like (short-function-name) ;; short comment here
