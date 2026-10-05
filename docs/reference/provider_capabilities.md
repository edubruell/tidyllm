# What each provider supports

Lists the verbs a provider implements and the arguments each verb
accepts, or the media types it accepts in a message. Use it to find out
which providers can do something before you write code that depends on
it.

## Usage

``` r
provider_capabilities(
  .provider = NULL,
  .verb = NULL,
  .argument = NULL,
  .what = c("arguments", "media"),
  .internal = FALSE
)
```

## Arguments

- .provider:

  A provider call such as
  [`claude()`](https://edubruell.github.io/tidyllm/reference/claude.md),
  a provider name such as `"claude"`, or `NULL` (default) for every
  provider.

- .verb:

  Character vector of verb names (such as `"chat"` or `"send_batch"`) to
  keep. `NULL` keeps all verbs.

- .argument:

  Character vector of argument names (such as `".thinking"`) to keep.
  `NULL` keeps all arguments.

- .what:

  `"arguments"` (default) for verbs and arguments, or `"media"` for one
  row per provider and media type.

- .internal:

  Logical; if `TRUE`, include the internal `build` verb that
  [`send_chat()`](https://edubruell.github.io/tidyllm/reference/send_chat.md)
  and
  [`parallel_chat()`](https://edubruell.github.io/tidyllm/reference/parallel_chat.md)
  use. Default `FALSE`.

## Value

A tibble. For `.what = "arguments"`: `provider`, `verb`, `argument`,
`default`, `fn`. For `.what = "media"`: `provider`, `media` (`image`,
`pdf`, `audio`, `video`, `files`) and `supported`.

## Details

With `.what = "arguments"` the result has one row per provider, verb and
argument. A verb that takes no arguments of its own gets one row with
`argument = NA`. The `default` column holds the argument's default as
text, so for `.model` it is the provider's default model. `fn` names the
function that implements the verb for that provider; its help page
documents every argument.

The table says that a provider's function accepts an argument. It does
not say that every model of that provider accepts it: for example,
`.thinking` can be accepted by
[`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md)
while a particular model rejects some effort levels.

## Examples

``` r
provider_capabilities(claude(), .verb = "chat")
#> # A tibble: 22 × 5
#>    provider verb  argument        default                 fn         
#>    <chr>    <chr> <chr>           <chr>                   <chr>      
#>  1 claude   chat  .llm             NA                     claude_chat
#>  2 claude   chat  .model          "\"claude-sonnet-5-5\"" claude_chat
#>  3 claude   chat  .max_tokens     "2048"                  claude_chat
#>  4 claude   chat  .temperature    "NULL"                  claude_chat
#>  5 claude   chat  .top_k          "NULL"                  claude_chat
#>  6 claude   chat  .top_p          "NULL"                  claude_chat
#>  7 claude   chat  .metadata       "NULL"                  claude_chat
#>  8 claude   chat  .stop_sequences "NULL"                  claude_chat
#>  9 claude   chat  .tools          "NULL"                  claude_chat
#> 10 claude   chat  .json_schema    "NULL"                  claude_chat
#> # ℹ 12 more rows

provider_capabilities(.argument = ".thinking")
#> # A tibble: 4 × 5
#>   provider verb       argument  default fn               
#>   <chr>    <chr>      <chr>     <chr>   <chr>            
#> 1 claude   chat       .thinking FALSE   claude_chat      
#> 2 claude   send_batch .thinking FALSE   send_claude_batch
#> 3 deepseek chat       .thinking NULL    deepseek_chat    
#> 4 llamacpp chat       .thinking NULL    llamacpp_chat    

provider_capabilities(.what = "media")
#> # A tibble: 75 × 3
#>    provider         media supported
#>    <chr>            <chr> <lgl>    
#>  1 azure_openai     image TRUE     
#>  2 azure_openai     pdf   FALSE    
#>  3 azure_openai     audio FALSE    
#>  4 azure_openai     video FALSE    
#>  5 azure_openai     files FALSE    
#>  6 chat_completions image TRUE     
#>  7 chat_completions pdf   FALSE    
#>  8 chat_completions audio TRUE     
#>  9 chat_completions video FALSE    
#> 10 chat_completions files FALSE    
#> # ℹ 65 more rows
```
