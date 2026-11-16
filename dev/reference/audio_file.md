# Create an Audio Object

Stores a reference to a local audio file for use in multimodal messages.
The file is not encoded until the message is formatted for a provider.

## Usage

``` r
audio_file(.path)
```

## Arguments

- .path:

  The path to the audio file on disk.

## Value

A `tidyllm_audio` object.
