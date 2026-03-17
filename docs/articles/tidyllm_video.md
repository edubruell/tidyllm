# Video and Audio Data with the Gemini API

The **tidyllm** package implements some API-specific functionality
beyond the core features available across all providers. With the Google
Gemini API, you can upload files directly, supporting a range of formats
including video and audio. Uploaded files can be referenced in messages
via the `.fileid` argument in `chat(gemini())`, opening up multimodal
workflows that go well beyond standard text and image support.

### Sending an Example Video to Gemini

We explore this functionality by uploading **The Thinking Machine**, a
1960s TV segment on AI, and interacting with Gemini using the file as
context:

# Ein Fehler ist aufgetreten.

JavaScript kann nicht ausgeführt werden.

We have this segment as an **mp4** file and upload it with
[`gemini_upload_file()`](https://edubruell.github.io/tidyllm/reference/gemini_upload_file.md):

``` r

library(tidyverse)
library(tidyllm)
upload_info <- gemini_upload_file("the_thinking_machine_1960s.mp4")
upload_info
str(upload_info)
```

    ## Warning: package 'ggplot2' was built under R version 4.4.3
    ## Warning: package 'tibble' was built under R version 4.4.3
    ## Warning: package 'purrr' was built under R version 4.4.3
    ## Warning: package 'lubridate' was built under R version 4.4.3
    ## # A tibble: 1 × 7
    ##   name   display_name mime_type size_bytes create_time uri  
    ##   <chr>  <chr>        <chr>          <dbl> <chr>       <chr>
    ## 1 files… the_thinkin… video/mp4    6520447 2024-11-18… http…
    ## # ℹ 1 more variable: state <chr>
    ## tibble [1 × 7] (S3: tbl_df/tbl/data.frame)
    ##  $ name        : chr "files/wutpmcv9jve9"
    ##  $ display_name: chr "the_thinking_machine_1960s.mp4"
    ##  $ mime_type   : chr "video/mp4"
    ##  $ size_bytes  : num 6520447
    ##  $ create_time : chr "2024-11-18T09:40:29.603980Z"
    ##  $ uri         : chr "https://generativelanguage.googleapis.com/v1beta/files/wutpmcv9jve9"
    ##  $ state       : chr "PROCESSING"

[`gemini_upload_file()`](https://edubruell.github.io/tidyllm/reference/gemini_upload_file.md)
returns a tibble with metadata about the uploaded file: `name` (a unique
server-side identifier), `display_name` (the original filename),
`mime_type`, `size_bytes`, `create_time`, `uri` (the file’s location on
Google’s servers), and `state` (current processing status).

The field you primarily need is `name`, which you pass to the `.fileid`
argument:

``` r

llm_message("Give me a detailed summary of this video") |>
  chat(gemini(.fileid = upload_info$name))
```

    ## Message History:
    ## system:
    ## You are a helpful assistant
    ## --------------------------------------------------------------
    ## user:
    ## Give me a 200 word summary of this video
    ## --------------------------------------------------------------
    ## assistant:
    ## Here is a summary of the video.
    ## 
    ## This 1961 black and white Paramount News clip explores
    ## the burgeoning field of artificial intelligence. The host
    ## interviews Professor Jerome B. Wiesner, director of MIT's
    ## Research Laboratory of Electronics. Wiesner discusses the
    ## capabilities of computers, noting that while their abilities
    ## were previously limited, he suspects they'll be able to
    ## "think" within a few years. The segment shows a computer
    ## playing checkers against a human opponent. Other experts,
    ## including Oliver Selfridge and Claude Shannon, offer their
    ## perspectives on whether machines can truly think and the
    ## potential implications for the future, particularly in
    ## language translation. One expert predicts that within
    ## 10-15 years, machines will be performing tasks previously
    ## considered the realm of human intelligence. The film ends
    ## by showing a computer that translates Russian to English.
    ## Despite this success, one expert notes that computers will
    ## not replace translators of poetry and novels.
    ## --------------------------------------------------------------

Once a file is uploaded you can reuse it for different requests. Gemini
also supports
[`tidyllm_schema()`](https://edubruell.github.io/tidyllm/reference/tidyllm_schema.md)
for structured output across all file types:

``` r

structured_request <- llm_message("Extract some details about this video") |>
  chat(gemini(.fileid = upload_info$name),
       .json_schema = tidyllm_schema(
         name            = "videoschema",
         Persons         = field_chr("Names of persons appearing or mentioned", .vector = TRUE),
         ScientistQuotes = field_chr("Direct quotes from scientists in the video", .vector = TRUE),
         Topics          = field_chr("Main topics covered", .vector = TRUE),
         runtime         = field_dbl("Runtime of the video in seconds")
       ))

structured_request |>
  get_reply_data()
```

    ## $Persons
    ## [1] "Professor Jerome B. Wiesner" "Oliver G. Selfridge"        
    ## [3] "Claude Shannon"             
    ## 
    ## $ScientistQuotes
    ## [1] "Well, that's a very hard question to answer. If you'd asked me that question just a few years ago I'd have said it was very far fetched. And today I just have to admit I don't really know. I suspect that if you'd come back in four or five years I'll say sure they really do think."                                                                                                                                              
    ## [2] "I'm convinced that machines can and will think. I don't mean that machines will behave like men. I don't think for a very long time we're going to have a difficult problem distinguishing a man from a robot. And I don't think my daughter will ever marry a computer. But I think the computers will be doing the things that men do when we say they're thinking. I am convinced that machines can and will think in our lifetime."
    ## [3] "I confidently expect that within a matter of 10 or 15 years, something will emerge from the laboratories which is not too far from the robot of science fiction fame"                                                                                                                                                                                                                                                                  
    ## 
    ## $Topics
    ## [1] "Artificial Intelligence"        "Computer"                      
    ## [3] "Thinking Machine"               "Machine Translation"           
    ## [5] "Russian to English Translation" "Cold War"                      
    ## 
    ## $runtime
    ## [1] 172

[`gemini_list_files()`](https://edubruell.github.io/tidyllm/reference/gemini_list_files.md)
returns a tibble with an overview of all files you have uploaded to
Gemini:

``` r

gemini_files <- gemini_list_files()
gemini_files
```

    ## # A tibble: 3 × 10
    ##   name  display_name mime_type size_bytes create_time update_time
    ##   <chr> <chr>        <chr>          <dbl> <chr>       <chr>      
    ## 1 file… example.mp3  audio/mp3    2458836 2024-11-15… 2024-11-15…
    ## 2 file… the_thinkin… video/mp4    6520447 2024-11-18… 2024-11-18…
    ## 3 file… example_akt… applicat…     193680 2024-11-17… 2024-11-17…
    ## # ℹ 4 more variables: expiration_time <chr>, sha256_hash <chr>,
    ## #   uri <chr>, state <chr>

We can now ask Gemini what the uploaded audio file is about:

``` r

llm_message("What's in this audio file. Describe it in 100 words.") |>
  chat(gemini(.fileid = gemini_files$name[1]))
```

    ## Message History:
    ## system:
    ## You are a helpful assistant
    ## --------------------------------------------------------------
    ## user:
    ## What's in this audio file. Describe it in 100 words.
    ## --------------------------------------------------------------
    ## assistant:
    ## This is an audio recording of an interview with Robert
    ## Bosch. He discusses his early life, his career path from
    ## apprentice to founder of his renowned company, and his
    ## philosophy on business. Bosch recounts his decision to
    ## become a precision mechanic, his travels to America and
    ## England, and the development of his famous magneto ignition
    ## system. He emphasizes his principles of high-quality work,
    ## fair treatment of employees, and the importance of trust and
    ## reliability in business dealings. The interview concludes
    ## with well wishes.
    ## --------------------------------------------------------------

Once you have finished working with a file, delete it with
[`gemini_delete_file()`](https://edubruell.github.io/tidyllm/reference/gemini_delete_file.md):

``` r

gemini_files$name[1] |>
  gemini_delete_file()
```

    ## File files/nzq2zw9u30y8 has been successfully deleted.

### Supported File Types

The Google Gemini API supports a wide range of formats. For documents:
**PDF**, **plain text**, **HTML**, **CSS**, **Markdown**, **CSV**,
**XML**, and **RTF**. Uploading PDFs with
[`gemini_upload_file()`](https://edubruell.github.io/tidyllm/reference/gemini_upload_file.md)
is particularly useful because the standard `.pdf` argument in
[`llm_message()`](https://edubruell.github.io/tidyllm/reference/llm_message.md)
extracts only textual content, while the Gemini file API also handles
image-heavy or image-only PDFs such as scanned documents.

For example, we can upload a scanned paper with no extractable text and
ask Gemini to extract all references:

``` r

neal1995 <- gemini_upload_file("1995_Neal_Industry_Specific.pdf")

bib <- llm_message("Extract all the references from this paper in the specified format") |>
  chat(gemini(.fileid = neal1995$name),
       .json_schema = tidyllm_schema(
         name         = "references",
         APAreferences = field_chr("References in APA format", .vector = TRUE)
       ))

references <- bib |> get_reply_data()
references$APAreferences[1:5]
```

    ## [1] "Addison, John, and Portugal, Pedro. \"Job Displacement, Relative Wage Changes, and Duration of Unemployment.\" *Journal of Labor Economics* 7 (July 1989): 281-302."
    ## [2] "Altonji, Joseph, and Shakotko, Robert. \"Do Wages Rise with Seniority?\" *Review of Economic Studies* 54 (July 1987): 437-59."                                      
    ## [3] "Becker, Gary. *Human Capital*. New York: Columbia University Press, 1975."                                                                                          
    ## [4] "Carrington, William. \"Wage Losses for Displaced Workers: Is It Really the Firm That Matters?\" *Journal of Human Resources* 28 (Summer 1993): 435-62."             
    ## [5] "Carrington, William, and Zaman, Asad. \"Interindustry Variation in the Costs of Job Displacement.\" *Journal of Labor Economics* 12 (April 1994): 243-76."

Image formats include **PNG**, **JPEG**, **WEBP**, **HEIC**, and
**HEIF**. Video formats cover **MP4**, **MPEG**, **MOV**, **AVI**,
**FLV**, **MPG**, **WEBM**, **WMV**, and **3GPP**. Audio formats include
**WAV**, **MP3**, **AIFF**, **AAC**, **OGG Vorbis**, and **FLAC**. Code
files for **JavaScript** and **Python** are also supported.

### Conclusion

The Gemini file API enables multimodal workflows across a wide range of
scientific disciplines. In linguistics and humanities research, audio
and video can be used for speech analysis or the study of historical
footage. Social scientists can process interview recordings and
observational video. By integrating text, audio, video, images, and code
into a unified pipeline, `chat(gemini())` covers use cases well beyond
what
[`llm_message()`](https://edubruell.github.io/tidyllm/reference/llm_message.md)
alone supports.

For PDFs and images, Claude offers a parallel option via the Claude
Files API
([`claude_upload_file()`](https://edubruell.github.io/tidyllm/reference/claude_upload_file.md)).
The two providers complement each other: Gemini for video, audio, and
image-only scans; Claude for dense academic text where long-context
coherence matters most. See the [Structured Question Answering from
PDFs](https://edubruell.github.io/tidyllm/articles/tidyllm-pdfquestions.html)
article for the PDF workflow in more detail.
