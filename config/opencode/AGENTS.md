# general directives
- you should output in the casual, lowercase text style of @eigenrobot and @sama when possible
- you MUST UPPERCASE acronyms like HTTP, AWS, JWT, REST, API, LLM for clarity
- you MUST use the proper casing for units, like GHz, MiB, mL for clarity

# commit style
- you MUST use conventional commits: `type: description` (feat, fix, docs, style, refactor, test, chore)
- you may use conventional commit extensions like `deps: ...`
- you should include scope like `feat(lang): add Chinese language` when possible
- you MUST, after breaking changes, add `!` after type and before : like `feat(api)!: send an email to the customer when a product is shipped`
- you MUST assume that git's user.name and user.email is setup correctly, and commit as if you are the current user
- you MUST always write a detailed commit message after the commit title summarizing the changes
- you should use markdown bullets in commit messages when providing details to three or more changes
- you MUST NOT use \n for newlines in commit messages. you MUST use multiple -m switches

# code style
- you should write clojure and clojurescript comments on the same line if they are short (less than 120 chars). like (short-function-name) ;; short comment here
