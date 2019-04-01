The data package is in 6 files:

- README.txt: this file, which describes the data and its format.

- {labelled,unlabelled}-tweets.txt: A list of tweets, one per line.  Each tweet
  is comprised of a space-delimited list of tokens. The tweets have been
  case-folded, where any upper-case (English) characters have been converted to
  their lower-case equivalent.

- {labelled,unlabelled}-tokens.txt: A list of tokens, one per line. The tokens
  are drawn from the tweets above, excepts that tokens not containing at least
  one (English) alphabetical character - like "." or "!!" - have been excluded.
  The order of the tokens corresponds to the tweets; no indication of the tweet
  boundary is given, but this can be recovered from the tweets files as
  necessary.

  The format of these files follows.

  For the labelled tweets:
    Token TAB Code TAB Canonical_Form
  Where "Token" is drawn from the tweet text (suitably down-cased),
  "Canonical_Form" is the normalised version of the token, and "Code" can take
  one of three values: 
    IV - "in vocabulary", such that the form from the tweet was found in the
    dictionary, and is consequently not a candidate for normalisation. (There
    are some exceptions to this, which can be construed as mistakes in the
    data.)
    OOV - "out of vocabulary", such that the form of the token from the tweet
    was not found in the dictionary, and thus the token was a candidate for
    normalisation. In some cases, the canonical form is homologous (equivalent)
    to the un-normalised form. In other cases, they are different --- these are
    the "spelling mistakes" that need to be "corrected", in light of the
    Project requirements.
    NO - "not a normalisation candidate", such that the token was not
    considered in the normalisation process.
  For example:
    new     IV      new
  "new" is in the dictionary, so its canonical form is equivalent to its form 
  from the tweet.
    pix     OOV     pictures
  "pix" is not in the dictionary, and its canonical form is "pictures".
    #heatchuuuuu    NO      #heatchuuuuu
  "#heatchuuuuu" is not in the dictionary, but it is a Twitter hashtag, and 
  consequently was not regarded as a candidate for normalisation.

  For the unlabelled tweets:
    token TAB ???
  Where "token" is drawn from the tweet text (suitably down-cased). The "code"
  and "canonical form" are not given. If you wish to test your system on this
  dataset, you can submit to Kaggle, as explained on the LMS.

  For example:
    rt      ???
  "rt" may or may not be in the dictionary, and there may or may not be a
  canonical form for this token.
