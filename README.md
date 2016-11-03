## yeast

[![Build Status](https://travis-ci.org/stevana/yeast.svg?branch=master)](https://travis-ci.org/stevana/yeast)

`yeast` is a Haskell library for working with different kinds of news
feeds (RSS1, RSS2, and Atom).

For documentation and examples, see `yeast`'s Hackage page.

### Shortcomings

Here's a list of a couple of things that would be nice to fix at some
point:

  * Upload the package to Hackage;

  * Add examples to the documentation and check them with the `doctests`
    library, see the following
    [commit](https://github.com/stevana/yeast/commit/8fec0051927b45a4b0f7204803e880657e91cac9);

  * Rewrite the `Serve` module so that we can test monadic properties,
    see the following
    [commit](https://github.com/stevana/yeast/commit/008ca33fb272de87a418cd940fa4c30212f880c9);

  * At least some compatibility with the old `feed` library would be
    nice, see the following
    [commit](https://github.com/stevana/yeast/commit/150463f31f068da229378ebcf154df50a5caa468);

  * Add more unit tests;

  * Integrate `stylish-haskell`, `hlint`, and perhaps other stylistic
    checks to the build;

  * Use more precise datatypes in the `Feed` module, in particular
    `Network.URI` and `UTCTime` should be used instead of `Maybe Text`
    for links and dates;

  * Do some profiling and maybe write some benchmarks.

### License

ISC, see the file `LICENSE`.
