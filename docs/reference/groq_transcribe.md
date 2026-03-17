# Transcribe an Audio File Using Groq transcription API

This function reads an audio file and sends it to the Groq transcription
API for transcription.

## Usage

``` r
groq_transcribe(
  .audio_file,
  .model = "playai-tts",
  .language = NULL,
  .prompt = NULL,
  .temperature = 0,
  .api_url = "https://api.groq.com/openai/v1/audio/transcriptions",
  .dry_run = FALSE,
  .verbose = FALSE,
  .max_tries = 3
)
```

## Arguments

- .audio_file:

  The path to the audio file (required). Supported formats include flac,
  mp3, mp4, mpeg, mpga, m4a, ogg, wav, or webm.

- .model:

  The model to use for transcription (default: "whisper-large-v3").

- .language:

  The language of the input audio, in ISO-639-1 format (optional).

- .prompt:

  A prompt to guide the transcription style. It should match the audio
  language (optional).

- .temperature:

  Sampling temperature, between 0 and 1, with higher values producing
  more randomness (default: 0).

- .api_url:

  Base URL for the API (default:
  "https://api.groq.com/openai/v1/audio/transcriptions").

- .dry_run:

  Logical; if TRUE, performs a dry run and returns the request object
  without making the API call (default: FALSE).

- .verbose:

  Logical; if TRUE, rate limiting info is displayed after the API
  request (default: FALSE).

- .max_tries:

  Maximum retries to peform request

## Value

A character vector containing the transcription.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage
groq_transcribe(.audio_file = "example.mp3")
} # }
```
