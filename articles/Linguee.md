# Introduction to Linguee API

## Introduction

The `polyglotr` package provides convenient functions to access the
Linguee API and retrieve translations, examples, and external sources.
This vignette demonstrates the usage of three key functions:
[`linguee_external_sources()`](https://tomeriko96.github.io/polyglotr/reference/linguee_external_sources.md),
[`linguee_translation_examples()`](https://tomeriko96.github.io/polyglotr/reference/linguee_translation_examples.md),
and
[`linguee_word_translation()`](https://tomeriko96.github.io/polyglotr/reference/linguee_word_translation.md).

``` r

library(polyglotr)
```

### External Sources

The
[`linguee_external_sources()`](https://tomeriko96.github.io/polyglotr/reference/linguee_external_sources.md)
function retrieves external sources using the Linguee Translation API.
Here’s an example usage:

``` r

external_sources <- linguee_external_sources("hello", src = "en", dst = "de")

print(external_sources)
```

### Translation Examples

The
[`linguee_translation_examples()`](https://tomeriko96.github.io/polyglotr/reference/linguee_translation_examples.md)
function provides translation examples using the Linguee Translation
API. Here’s an example usage:

``` r

translation_examples <- linguee_translation_examples("hello", src = "en", dst = "de")

print(translation_examples)
```

### Word Translation

The
[`linguee_word_translation()`](https://tomeriko96.github.io/polyglotr/reference/linguee_word_translation.md)
function translates a word using the Linguee Translation API. Here’s an
example usage:

``` r

word_translation <- linguee_word_translation("hello", source_language = "en", target_language = "de")

print(word_translation)
```
