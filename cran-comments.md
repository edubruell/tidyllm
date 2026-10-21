tidyllm 0.7.2 extends `embed()` so that it also embeds images, audio and video. Until now only `voyage()` could embed images. `embed()` now takes a list that mixes text with image, audio and video files for `llamacpp()`, `gemini()`, `openrouter()` and `ollama()`, and has new arguments `.task`, `.dimensions` and `.media_columns`.

Smaller changes: `gemini()` warns when sampling arguments reach a model that ignores them, `claude()` refuses these arguments for Claude Haiku 5 models before sending the request, and tool results reach the model as text on every provider.

No new Imports or Suggests.

## Test environments

* local macOS (aarch64-apple-darwin20), R 4.5.3

## R CMD check results

0 errors | 0 warnings | 3 notes

The notes come from the local setup and the network. R could not verify the current time, the installed HTML Tidy is too old to validate the manual, and the check got a 403 response from `https://openai.com` and `https://platform.openai.com/account/api-keys`, which load in a browser.

## Reverse dependencies

Two CRAN packages suggest tidyllm: joinery and RKorAPClient. I have not re-run their checks against 0.7.2. The changes are additive and no existing function lost an argument, so I expect no effect.
