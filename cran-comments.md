tidyllm 0.7.1 fixes a bug in `claude_cli()`, a provider added in 0.7.0: with its tools turned off, the default, the Claude command line tool could still read files in the working directory. The fix removes every built-in tool and the user's MCP servers unless the user asks for them.

It also adds two providers that work the same way, `codex_cli()` for OpenAI's Codex CLI and `vibe_cli()` for Mistral's Vibe CLI; a `.thinking_level` argument for `gemini()`, ahead of Google's change to how Gemini controls reasoning; support for Mistral Large 4; and clearer error messages for Mistral and for rejected streaming requests. An image is now an error for providers that cannot take images.

No new Imports or Suggests.

## Test environments

* local macOS (aarch64-apple-darwin20), R 4.5.3

## R CMD check results

0 errors | 0 warnings | 1 note

The note is environmental: "unable to verify current time".

## Reverse dependencies

Two CRAN packages suggest tidyllm: joinery and RKorAPClient. joinery passes R CMD check with tidyllm 0.7.1. RKorAPClient uses tidyllm only in tests that need LLM API keys and skip without them; the functions it calls are unchanged.
