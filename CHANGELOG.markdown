0.5.1
=====

  * Added configurable maximum info block width (https://github.com/supki/envparse/pull/21).

  * Added `instance (Field e a, ...) => Field e (Maybe a)` for generic environment parsing. (https://github.com/supki/envparse/pull/16)

0.5
===

  * Added `char`.

  * Fixed masking parse errors with default values. (https://github.com/supki/envparse/issues/8)

  * Replaced `keep` with `sensitive`. All variables are kept in the environment after a successul parse
    except those wrapped in `sensitive`. (https://github.com/supki/envparse/issues/9)

0.4.1
=====

  * Supported GHC 8.4.3.


0.4
===

  * Supported GHC 8.0.1.

  * On GHC 7.8 and newer, as a secutiry measure, all declared variables are unset by the end of
    a successful parse.  If you want to keep the variable in the environment, use the `keep` modifier.
    (https://github.com/supki/envparse/pull/7)

0.3.4
=====

  * Added `splitOn`.

0.3.3
=====

  * Removed the unnecessary `AsEmpty` and `AsUnset` constraints on the error type
    in the `flag` and `switch` parsers.

0.3.2
=====

  * Added `showDef`.

0.3.1
=====

  * Supported `Generic`ally derived `Parser`s on GHC 7.10.

0.3.0
=====

  * Supported user-defined `Reader` errors. (https://github.com/supki/envparse/pull/4)

  * Widened the range of GHC versions to test the library with from 7.6–7.8 to 7.6–HEAD.

0.2.2
=====

  * Added `helpDoc`, a `Parser` pretty-printer for use in error messages.

0.2.1
=====

  * Cosmetic changes in the help output.

0.2.0
=====

  * Added `prefixed`.

  * Added `parseOr`, a slightly generalized version of `parse`.

  * Renamed `parseTest` to `parsePure`.

0.1.0
=====

  * Initial release.
