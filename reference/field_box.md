# Ask `decide()` to find something in an image (experimental)

`field_box()` is a
[`decide()`](https://edubruell.github.io/tidyllm/reference/decide.md)
question that asks where something is in a picture. The answer is one
box around it, given as four fractions of the image: `top`, `bottom`,
`left` and `right`, each measured from the top-left corner (0 is the
edge you measure from, 1 the far side). Multiply by the pixel height and
width to crop or draw;
[`box_to_pixels()`](https://edubruell.github.io/tidyllm/reference/box_to_pixels.md)
does it for you.

## Usage

``` r
field_box(.description)
```

## Arguments

- .description:

  What to find, as a short sentence, for example "The protest banner in
  `photo`".

## Value

An S7 `tidyllm_field` object for use in
[`tidyllm_schema()`](https://edubruell.github.io/tidyllm/reference/tidyllm_schema.md).

## Details

This feature is experimental. Only
[`jev()`](https://edubruell.github.io/tidyllm/reference/jev.md) answers
it, the vendor labels image input and boxes as preview, and the format
may change between Jev releases.
[`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md),
[`gemini()`](https://edubruell.github.io/tidyllm/reference/gemini.md)
and
[`ollama()`](https://edubruell.github.io/tidyllm/reference/ollama.md)
stop with a message before any request. A state without an image stops
too.

A box question always returns a box, even when the thing is not in the
picture. Pair it with a yes/no question that asks whether it is there,
and ignore the box when the answer is no. To point at one of several
images, name the image in the description: give the images names in
`.state` and write "the dog in `photo_b`". Ask for one thing per box
question; for several things, add several questions.

In the result, a box question is one row with type `"box"`, `answer` and
`probability` set to `NA`, and `confidence` as the model gives it (it
can be `NA`). The columns `top`, `bottom`, `left` and `right` appear
only when the schema holds a box question and are `NA` on all other
rows. Boxes are rough for small objects in large images and their
accuracy is unmeasured; check a sample by eye before you rely on them.

## See also

[`box_to_pixels()`](https://edubruell.github.io/tidyllm/reference/box_to_pixels.md),
[`decide()`](https://edubruell.github.io/tidyllm/reference/decide.md)

## Examples

``` r
if (FALSE) { # \dontrun{
questions <- tidyllm_schema(
  has_cat = field_lgl("Is there a cat in `photo`?"),
  cat = field_box("The cat in `photo`")
)
decide(list(list(photo = img("cat.jpg"))), questions, jev())
} # }
```
