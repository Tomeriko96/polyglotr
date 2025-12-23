# Translate English Text to Morse Code with Audio

This function takes an English text string as input and translates it to
Morse code with an audio output using the FunTranslations API.

## Usage

``` r
translate_to_morse_audio(text, api_key = NULL)
```

## Arguments

- text:

  A character string containing the English text to be translated.

- api_key:

  (optional) Your FunTranslations API key, if you have a paid
  subscription.

## Value

A list containing the translated Morse code text, the Morse code audio
as a base64-encoded string, and other metadata.
