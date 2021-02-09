Turkish Phonetic Translator
================

## What does this do?

This is a G2P (grapheme to phoneme) translator for Turkish that uses
orthography and phonotactic rules. Additionally it syllabifies the
translated words. There are two .csv files encoding underlying and
allophonic sounds. There are 37 phonemes for consonants and 15 phonemes
for vowels.

-   **Consonants IPA:** b, d͡ʒ, t͡ʃ, t͡ʃʰ, d, f, ɸ, g, g̟, ɣ, h, ç, x, ʒ, k,
    kʰ, k̟, k̟ʰ, l, ɬ, ʎ, m, n, p, pʰ, ɹ, r̥, r, s, ʃ, t, tʰ, v, β, ʋ, j, z
-   **Vowels IPA:** a, a:, a̟, e, ɜ, ɯ, i, i:, u, u:, u̟, y, o, o̟, ø

### Currently implemented phonetic rules

-   **Vowel fronting**<br> Short back vowels (a, u, o) are fronted when
    they are in the environment of a liquid (j) or preceded by a lateral
    (l, ɬ, ʎ).

-   **Open “e”**<br> The vowel “e” gets more open preceding a sonorant
    (l, m, n, r, j).

-   **Consonant fronting**<br> Velar consonants (g,h,k) are fronted when
    preceding a front vowel (e, i, i:, y, ø)

-   **Consonant aspiration**<br> Voiceless stops (k, p, t) and affricate
    (t͡ʃ) are aspirated word finally, preceding a vowel and in complex
    onsets.

-   **V changes**<br> The underlying voiced labiodental fricative (v)
    becomes an approximant (ʋ) in between vowels and if one of the
    vowels are round. It becomes a bilabial (β) if it is in the
    environment of a round vowel and a consonant.

-   **F changes**<br> The underlying voiceless labiodental fricative (f)
    becomes a bilabial (ɸ) if it is in the environment of a round vowel.

-   **R changes**<br> The underlying alveolar rhotic (r) becomes a tap
    (r̥) at the end of a word. It becomes a trill (ɹ) in the word
    beginning.

## Version info

Alpha. Further development points:

1.  Improve the rule profile
2.  Turn the script into a function with toggleable rules
3.  Include a dictionary for exceptional underlying forms
4.  Overhaul (version shift Beta)
5.  Provide a rule writing scheme
6.  Optimize for bulk translation
7.  Come up with a learning algorithm for selecting epenthesis in
    consonant clusters (version shift 1.0)
8.  Provide acoustic profile for underlying forms
