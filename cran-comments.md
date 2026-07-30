This is a bugfix release. It fixes structured output schemas for providers that enforce OpenAI strict mode (nested object nodes were missing `additionalProperties: false`), restores tool use for the Gemini backend and web search for older Claude models after upstream API changes, surfaces provider errors that previously arrived empty, and adds two token-accounting columns to `get_metadata()`.

## Test environments

* local macOS (aarch64-apple-darwin20), R 4.5.3

## R CMD check results

0 errors | 0 warnings | 1 note

The note is "checking for future file timestamps: unable to verify current time", which is a network condition on the check machine rather than a package issue.

A previous submission raised a note for `https://platform.openai.com/account/api-keys` in the vignette. That URL is valid and current; the host returns 403 to automated clients, so the note is a false positive and the link is kept.

There are no reverse dependencies.
