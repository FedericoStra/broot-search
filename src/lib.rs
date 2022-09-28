/*! Parse search strings.

A search string is a combination or `search_atom`s, delimiters `(` and `)`,
binary operators `&` and `|`, unary operator `!`.

```txt
<search-string> ::= <unit-search-string>
                  | <search-string> "&" <search-string>
                  | <search-string> "|" <search-string>
<unit-search-string> ::= <search-atom>
                       | "(" <search-string> ")"
                       | "!" <unit-search-string>
```

A `<search-atom>` comes in three flavors: simple, with a prefix, or with a prefix and some flags.
```txt
<search-atom> ::= <search-term>
                | <prefix> "/" <search-term>
                | <prefix> "/" <search-term> "/" <flags>
```
When it's in the first form, then `<search-term>` must not be empty.
(I don't provide a rigorous grammar for this detail because for simplicity
I don't want to introduce the syntactic variable `<non-empty-search-term>`).

The prefix and flags control the kind of search that is performed:
```txt
<prefix> ::= "" | "c" | "cr" | "e" | "ep" | "f" | "nt" | "rp" | "t"
<flags> ::= "" | <flags> "i" | <flags> "m" | <flags> "s"
          | <flags> "U" | <flags> "u" | <flags> "x"
```

The search term is composed of characters different from `" "`, `"\"`, `"/"`, or escape sequences.
It is matched by this regex `([^ \\/]|\\.)*` and can be described by this grammar:
```txt
<search-term> ::= ""
                | <search-term> <unescaped>
                | <search-term> <escape-sequence>
<unescaped> ::= any Unicode codepoint except " ", "\", "/"
<escape-sequence> ::= "\" <unicode>
<unicode> ::= any Unicode codepoint
```

When parsed in a search term, the escape sequences `"\ "` and `"\/"` are unescaped,
so they become `" "` and `"/"` respectively. All other escape sequences are preserved
and passed on to the regex engine.

Notice in particular that a `<search-string>` can only contains spaces inside `<search-term>`s
and they must be escaped.
*/

use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{alpha0, char, none_of, one_of},
    combinator::{all_consuming, map, opt, recognize, rest, value, verify},
    multi::many0,
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
};

pub fn search_term(input: &str) -> nom::IResult<&str, String> {
    // morally matches the regex `([^ \\/]|\\.)*`, followed by unescaping ' ' and '/'
    many0(alt((
        is_not(r"\ /"),
        value(r" ", tag(r"\ ")),                     // unescape ' '
        value(r"/", tag(r"\/")),                     // unescape '/'
        recognize(pair(char('\\'), none_of(r" /"))), // all other escape sequences are preserved
    )))(input)
    .map(|(s, v)| (s, v.into_iter().collect()))
}

pub fn prefix(input: &str) -> nom::IResult<&str, &str> {
    alpha0(input)
}

pub fn flags(input: &str) -> nom::IResult<&str, &str> {
    alpha0(input)
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SearchAtom<S> {
    Simple(String),            // `<search_term>`, will be non-empty
    Prefix(S, String),         // `<prefix>/<search_term>`
    PrefixFlags(S, String, S), // `<prefix>/<search_term>/<flags>`
}

pub fn search_atom(input: &str) -> nom::IResult<&str, SearchAtom<&str>> {
    alt((
        // <prefix>/<search-term>/<flags>
        map(
            tuple((prefix, delimited(tag("/"), search_term, tag("/")), flags)),
            |(p, s, f)| SearchAtom::PrefixFlags(p, s, f),
        ),
        // <prefix>/<search-term>
        map(separated_pair(prefix, tag("/"), search_term), |(p, s)| {
            SearchAtom::Prefix(p, s)
        }),
        // <search-term>, cannot be empty
        map(
            verify(search_term, |s: &str| !s.is_empty()),
            SearchAtom::Simple,
        ),
    ))(input)
}

pub fn bet(input: &str) -> nom::IResult<&str, char> {
    one_of("()&|!")(input)
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token<S> {
    Atom(SearchAtom<S>),
    Bet(char),
}

pub fn search_string_token(input: &str) -> nom::IResult<&str, Token<&str>> {
    alt((map(bet, Token::Bet), map(search_atom, Token::Atom)))(input)
}

pub fn search_string_tokens(input: &str) -> nom::IResult<&str, Vec<Token<&str>>> {
    many0(search_string_token)(input)
}

pub fn command_line(input: &str) -> nom::IResult<&str, (Vec<Token<&str>>, Option<&str>)> {
    alt((
        // " cmd args..."
        map(preceded(tag(" "), rest), |cmd| (vec![], Some(cmd))),
        // "search cmd args..."
        map(
            separated_pair(search_string_tokens, tag(" "), rest),
            |(search, cmd)| (search, Some(cmd)),
        ),
        // "search" or "search "
        map(
            all_consuming(terminated(search_string_tokens, opt(tag(" ")))),
            |search| (search, None),
        ),
    ))(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_search_term() {
        assert_eq!(
            search_term(r"a\\b\/c\ d verb"),
            Ok((" verb", r"a\\b/c d".to_string()))
        );
        assert_eq!(
            search_term(r"a\\b\/c\ d/flags"),
            Ok(("/flags", r"a\\b/c d".to_string()))
        );
        assert_eq!(
            search_term(r#"a\'b\"c\td/flags"#),
            Ok(("/flags", r#"a\'b\"c\td"#.to_string()))
        );
    }

    macro_rules! test_search_atom {
        ($input:expr => $term:expr; $rest:expr) => {
            assert_eq!(
                search_atom($input),
                Ok(($rest, SearchAtom::Simple($term.to_string())))
            );
        };
        ($input:expr => $prefix:expr, $term:expr; $rest:expr) => {
            assert_eq!(
                search_atom($input),
                Ok(($rest, SearchAtom::Prefix($prefix, $term.to_string())))
            );
        };
        ($input:expr => $prefix:expr, $term:expr, $flags:expr; $rest:expr) => {
            assert_eq!(
                search_atom($input),
                Ok((
                    $rest,
                    SearchAtom::PrefixFlags($prefix, $term.to_string(), $flags)
                ))
            );
        };
        ($input:expr =>! $code:expr, $rest:expr) => {
            assert_eq!(
                search_atom($input),
                Err(nom::Err::Error(nom::error::Error {
                    input: $rest,
                    code: $code
                }))
            )
        };
    }

    #[test]
    fn test_search_atom_simple() {
        test_search_atom!(r" verb" =>! nom::error::ErrorKind::Verify, " verb");
        test_search_atom!(r"a\\b\/c\ d verb" => r"a\\b/c d"; " verb");
        test_search_atom!(r#"a\'b\"c\td verb"# => r#"a\'b\"c\td"#; " verb");
    }

    #[test]
    fn test_search_atom_prefix() {
        test_search_atom!(r"/ verb" => "", r""; " verb");
        test_search_atom!(r"/a\\b\/c\ d verb" => "", r"a\\b/c d"; " verb");
        test_search_atom!(r#"/a\'b\"c\td verb"# => "", r#"a\'b\"c\td"#; " verb");
        test_search_atom!(r"pref/a\\b\/c\ d verb" => "pref", r"a\\b/c d"; " verb");
        test_search_atom!(r#"pref/a\'b\"c\td verb"# => "pref", r#"a\'b\"c\td"#; " verb");
    }

    #[test]
    fn test_search_atom_prefix_flags() {
        test_search_atom!(r"// verb" => "", r"", ""; " verb");
        test_search_atom!(r"/a\\b\/c\ d/ verb" => "", r"a\\b/c d", ""; " verb");
        test_search_atom!(r#"/a\'b\"c\td/ verb"# => "", r#"a\'b\"c\td"#, ""; " verb");
        test_search_atom!(r"pref/a\\b\/c\ d/ verb" => "pref", r"a\\b/c d", ""; " verb");
        test_search_atom!(r#"pref/a\'b\"c\td/ verb"# => "pref", r#"a\'b\"c\td"#, ""; " verb");
        test_search_atom!(r"/a\\b\/c\ d/flags verb" => "", r"a\\b/c d", "flags"; " verb");
        test_search_atom!(r#"/a\'b\"c\td/flags verb"# => "", r#"a\'b\"c\td"#, "flags"; " verb");
        test_search_atom!(r"pref/a\\b\/c\ d/flags verb" => "pref", r"a\\b/c d", "flags"; " verb");
        test_search_atom!(r#"pref/a\'b\"c\td/flags verb"# => "pref", r#"a\'b\"c\td"#, "flags"; " verb");
    }

    #[test]
    fn test_search_string_tokens() {
        use SearchAtom::*;
        use Token::*;

        assert_eq!(
            search_string_tokens(r#"(!/reg/f&p/re/)|q/re&|/&p/re verb"#),
            Ok((
                " verb",
                vec![
                    Bet('('),
                    Bet('!'),
                    Atom(PrefixFlags("", "reg".to_string(), "f")),
                    Bet('&'),
                    Atom(PrefixFlags("p", "re".to_string(), "")),
                    Bet(')'),
                    Bet('|'),
                    Atom(PrefixFlags("q", "re&|".to_string(), "")),
                    Bet('&'),
                    Atom(Prefix("p", "re".to_string())),
                ]
            ))
        );

        assert_eq!(
            search_string_tokens(r#"(!/reg/f&p/re)|q/re&|/&p/re verb"#),
            Ok((
                " verb",
                vec![
                    Bet('('),
                    Bet('!'),
                    Atom(PrefixFlags("", "reg".to_string(), "f")),
                    Bet('&'),
                    Atom(PrefixFlags("p", "re)|q".to_string(), "re")),
                    Bet('&'),
                    Bet('|'),
                    Atom(PrefixFlags("", "&p".to_string(), "re")),
                ]
            ))
        );

        assert_eq!(
            search_string_tokens(r#"(!/ΔᎠ\ β/f&p/re\/🙂!/)|q/ⅠᏴ\"γ&|/&p/δⅡ verb"#),
            Ok((
                " verb",
                vec![
                    Bet('('),
                    Bet('!'),
                    Atom(PrefixFlags("", r"ΔᎠ β".to_string(), "f")),
                    Bet('&'),
                    Atom(PrefixFlags("p", "re/🙂!".to_string(), "")),
                    Bet(')'),
                    Bet('|'),
                    Atom(PrefixFlags("q", r#"ⅠᏴ\"γ&|"#.to_string(), "")),
                    Bet('&'),
                    Atom(Prefix("p", "δⅡ".to_string())),
                ]
            ))
        );
    }

    #[test]
    fn test_command_line() {
        use SearchAtom::*;
        use Token::*;

        assert_eq!(command_line(""), Ok(("", (Vec::new(), None))));
        assert_eq!(command_line(" "), Ok(("", (Vec::new(), Some("")))));
        assert_eq!(
            command_line(" verb arg"),
            Ok(("", (Vec::new(), Some("verb arg"))))
        );
        assert_eq!(
            command_line("fzy"),
            Ok(("", (vec![Atom(Simple("fzy".to_string()))], None)))
        );
        assert_eq!(
            command_line("fzy "),
            Ok(("", (vec![Atom(Simple("fzy".to_string()))], Some(""))))
        );
        assert_eq!(
            command_line("fzy cmd arg"),
            Ok(("", (vec![Atom(Simple("fzy".to_string()))], Some("cmd arg"))))
        );
        assert_eq!(
            command_line(r"(/toml$/|/\.rs$/)&f/xyz cp /dev/null"),
            Ok((
                "",
                (
                    vec![
                        Bet('('),
                        Atom(PrefixFlags("", "toml$".to_string(), "")),
                        Bet('|'),
                        Atom(PrefixFlags("", r"\.rs$".to_string(), "")),
                        Bet(')'),
                        Bet('&'),
                        Atom(Prefix("f", "xyz".to_string())),
                    ],
                    Some("cp /dev/null")
                )
            ))
        );
    }
}

/// NOTE: this iteration interface is completely optional; I don't recommend using it.
pub mod iter {
    use super::*;

    pub struct SearchStringTokens<'i> {
        input: &'i str,
        atom_allowed: bool,
    }

    impl<'i> SearchStringTokens<'i> {
        pub fn new(input: &str) -> SearchStringTokens {
            SearchStringTokens {
                input,
                atom_allowed: true,
            }
        }
    }

    impl<'i> Iterator for SearchStringTokens<'i> {
        type Item = Token<&'i str>;

        fn next(&mut self) -> Option<Token<&'i str>> {
            let (rest, tok) = if self.atom_allowed {
                map(bet, Token::Bet)(self.input)
                    .or_else(|_| map(search_atom, Token::Atom)(self.input))
            } else {
                map(bet, Token::Bet)(self.input)
            }
            .ok()?;
            self.input = rest;
            self.atom_allowed = matches!(tok, Token::Bet('!' | '&' | '|'));
            Some(tok)
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_search_string_tokens_iter() {
            use SearchAtom::*;
            use Token::*;

            assert_eq!(
                SearchStringTokens::new(r#"(!/reg/f&p/re/)|q/re&|/&p/re verb"#).collect::<Vec<_>>(),
                vec![
                    Bet('('),
                    Bet('!'),
                    Atom(PrefixFlags("", "reg".to_string(), "f")),
                    Bet('&'),
                    Atom(PrefixFlags("p", "re".to_string(), "")),
                    Bet(')'),
                    Bet('|'),
                    Atom(PrefixFlags("q", "re&|".to_string(), "")),
                    Bet('&'),
                    Atom(Prefix("p", "re".to_string())),
                ]
            )
        }
    }
}
