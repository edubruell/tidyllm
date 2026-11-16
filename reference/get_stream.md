# A running chat's deltas, as a stream

Returns a `coro` async generator over the text deltas: the pull form of
`.on_chunk`. It replays whatever has already arrived and then continues
live, so it is safe to ask for at any point in a job's life and always
tells the same story as
[`get_partial()`](https://edubruell.github.io/tidyllm/reference/get_partial.md).

## Usage

``` r
get_stream(.job)
```

## Arguments

- .job:

  A streaming `tidyllm_chat_job` from
  [`send_chat()`](https://edubruell.github.io/tidyllm/reference/send_chat.md).

## Value

A `coro` async generator instance yielding character deltas.

## Details

This is what `shinychat::chat_append()` consumes directly, because
shinychat's whole extensibility contract is
`inherits(x, "coro_generator_instance")`. The generator is asynchronous
rather than synchronous on purpose: shinychat's consumer only yields the
event loop at an `await()`, so a synchronous generator would drain in
one tick and the reply would appear all at once instead of token by
token.

In a plain script this is rarely what you want. An async generator hands
a [`coro::loop()`](https://coro.r-lib.org/reference/collect.html)
promises rather than text, so consuming it takes
[`coro::async()`](https://coro.r-lib.org/reference/async.html) and
[`coro::await_each()`](https://coro.r-lib.org/reference/async_generator.html);
`.on_chunk` or a
[`get_partial()`](https://edubruell.github.io/tidyllm/reference/get_partial.md)
loop says the same thing with less ceremony.

## Examples

``` r
if (FALSE) { # \dontrun{
job <- llm_message("Tell me a story") |> send_chat(claude(), .stream = TRUE)
shinychat::chat_append("chat", get_stream(job))
} # }
```
