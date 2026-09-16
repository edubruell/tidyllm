# Comments from last Submission
invalid file URI fixed as per Uwe Ligges comment from 03.09.2026 17:21

# Old CRAN-comments from initial submission
tidyllm 0.6.0 adds non-blocking chat. `send_chat()` runs a single request against
R's own event loop instead of blocking the session, and `parallel_chat()` runs a
list of prompts against one provider concurrently. Streaming and tool calls are
no longer mutually exclusive, streaming now runs through one shared pump across
providers (a truncated stream raises instead of hanging), and several provider
bugs are fixed (`chat_ellmer()` double-sending the last turn, `openai_chat(.stateful
= TRUE)` skipping its retry on tool-loop rounds, `claude_chat()` ignoring
`.max_tries`, and others; see NEWS.md).

No new required dependency: `later` and `promises` move from unused to
`Suggests`-and-checked-at-use-site for `send_chat()`; the Shiny example app needs
neither.

## Test environments

* local macOS (aarch64-apple-darwin20), R 4.5.3

## R CMD check results

0 errors | 0 warnings | 1 note

The note is "checking for future file timestamps: unable to verify current time", which is a network condition on the check machine rather than a package issue.

There are no reverse dependencies.
