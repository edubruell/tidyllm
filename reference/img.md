# Create an Image Object

This function reads an image file from disk, encodes it in base64, and
returns a `tidyllm_image` object that can be used in multimodal
embedding requests or attached to messages via `.media`.

## Usage

``` r
img(.path)
```

## Arguments

- .path:

  The path to the image file on disk.

## Value

A `tidyllm_image` object.
