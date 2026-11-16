# Create a Video Object

Stores a reference to a local video file for use in multimodal messages.
The file is not encoded until the message is formatted for a provider.

## Usage

``` r
video_file(.path)
```

## Arguments

- .path:

  The path to the video file on disk.

## Value

A `tidyllm_video` object.
