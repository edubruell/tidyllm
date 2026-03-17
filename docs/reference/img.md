# Create an Image Object

This function reads an image file from disk, encodes it in base64, and
returns a `tidyllm_image` object that can be used in multimodal
embedding requests.

## Usage

``` r
img(.path)
```

## Arguments

- .path:

  The path to the image file on disk.

## Value

An `tidyllm_image`, containing:

- `imagepath`: The original file path

- `imagename`: The basename of the image

- `imagebase64`: a "data:image/...;base64,..." string
