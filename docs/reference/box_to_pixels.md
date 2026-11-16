# Turn the fractions of box answers into pixel positions

A box answer from
[`field_box()`](https://edubruell.github.io/tidyllm/reference/field_box.md)
gives `top`, `bottom`, `left` and `right` as fractions of the image.
`box_to_pixels()` multiplies them by the image size, after clamping them
to 0 and 1, and returns your
[`decide()`](https://edubruell.github.io/tidyllm/reference/decide.md)
result with four more columns: `x_min`, `x_max`, `y_min` and `y_max`, in
pixels counted from the top-left corner. Rows that are not boxes, or
whose `type` is missing, get `NA`. The pixel values are rounded to whole
pixels. If `.result` already has pixel columns from an earlier call,
they are replaced.

## Usage

``` r
box_to_pixels(.result, .width = NULL, .height = NULL, .image = NULL)
```

## Arguments

- .result:

  A
  [`decide()`](https://edubruell.github.io/tidyllm/reference/decide.md)
  result that holds a
  [`field_box()`](https://edubruell.github.io/tidyllm/reference/field_box.md)
  question.

- .width, .height:

  Image width and height in pixels, one number each.

- .image:

  Instead of `.width` and `.height`, the path of the PNG or JPEG file
  the boxes refer to; its size is read from the file. If you give it,
  `.width` and `.height` are ignored.

## Value

`.result` with the columns `x_min`, `x_max`, `y_min` and `y_max`.

## Details

Clamping keeps each edge inside the image but does not repair a box the
model returned upside down: if `left` is larger than `right`, `x_min`
stays larger than `x_max`. Widths and heights need not be whole numbers.

The result must come from one image size. If your states are images of
different sizes, filter the result by `state_id` and call the function
once per size.

## See also

[`field_box()`](https://edubruell.github.io/tidyllm/reference/field_box.md)

## Examples

``` r
res <- tibble::tibble(question = "cat", type = "box",
                      top = 0.2, bottom = 0.7, left = 0, right = 0.8)
box_to_pixels(res, .width = 1000, .height = 600)
#> # A tibble: 1 × 10
#>   question type    top bottom  left right x_min x_max y_min y_max
#>   <chr>    <chr> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 cat      box     0.2    0.7     0   0.8     0   800   120   420
```
