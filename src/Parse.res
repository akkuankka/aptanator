type error = string
type parse_span = {
    input: string,
    output: string,
}
type res = Belt.Result.t<parse_span, error>

exception Re_not_matched(string)



open Js.Re

let const: 'a => 'b => 'a = (a) => _ => a 

// asssumes `regex` matches at index 0
let partition_match = (regex, str) => {
    switch exec_(regex, str) {
        | Some(match) => {
            let match = captures(match)
            switch Js.Nullable.toOption(match[0]) {
                | None => raise(Re_not_matched(str))
                | Some(match) => (match, Js.String.substringToEnd(str, ~from=Js.String.length(match)))
            }
        }
        | None => raise(Re_not_matched(str))
    }
}
// may end up needing module generics for this
/*module type Parser = {
    type input
    let p: input => string => res
}
*/



let parse_regex = (regex, input) => (
    if Js.Re.test_(regex, input) {
        let (match, remaining) = partition_match(regex, input)
        Ok({input: remaining, output: match})
    } else {
        Error("Regex match failed")
    }
)

let map = (fn, parser, inp) => {
    switch parser(inp) {
        | Ok(re) => Ok({...re, output: fn(re.output)})
        | otherwise => otherwise
    }
}
let toOpt = (res: Belt.Result.t<'t, 'e>) => switch res {
    | Ok(re) => Some(re)
    | _ => None
}
let alts = (parsers, inp) => {
    switch Belt.List.keepMap(parsers, parser => parser(inp)->toOpt) {
        | list{re, ..._rest} => Ok(re)
        | list{} => Error("None of a list of alternates matched")
    }
}

let rec many1 = (~already="", parser, inp: string)  => {
    if Js.String.length(inp) == 0 {Ok({input: "", output: already})} 
    else {switch parser(inp) {
        | Ok({input, output}) => many1(parser, input, ~already=(already ++ output))
        | e => if already != "" { Ok({input: inp, output: already})}
                      else {e}
    }}
}
let rec and_then = (discards, keep, inp) => {
    switch discards {
        | list{} => keep(inp)
        | list{parser, ...rest} => switch parser(inp) {
            | Ok({input, output}) => and_then(rest, keep, input)
            | error => error
        }
    }
}

let must_complete = (parser, inp) => {
    switch parser(inp) {
        | Ok({input: "", output}) => Ok({input: "", output})
        | Ok({input, output}) => Error(`letters ${input} left over after parsing terminated`)
        | error => error
    }
}

open Js.String

let with_result = (a) => map(const(a))

let tag = (tag: string, input) => {
    let test = substring(input, ~from=0, ~to_=(tag->length))
    if tag == test {
        Ok({input: substringToEnd(input, ~from=tag->length), output: tag})
    } else {
        Error(`Tag ${tag} didn't match with ${test}`)
    }
}

let one_of = (options: string, input) => {
    let foundOne = ref(None);
    let finished = ref(false)
    for i in 0 to options->length - 1 {
        if finished.contents {
            () // basically have to spin through to the end of the loop
        }
        // else if input->startsWith(options->Js.String2.charAt(i)) {
        else if (%raw(`input.startsWith(options.charAt(i))`) ){
            foundOne := Some(options->Js.String2.charAt(i))
        } else {()} 
    }
    switch foundOne.contents {
        | None => Error(`none of the list ${options} matched`)
        | Some(c) => Ok({output: c, input: input->substringToEnd(~from=1)})
    }
}

let transformer = (mapping, key) => Belt.Map.String.getWithDefault(mapping, key, "ERROR")

let transformerArray = (array, key) => transformer(Belt.Map.String.fromArray(array), key)
/* [======[Tone Markers]======]*/
let transform_tone_marker = transformerArray([
    ("#1", "˩"),
    ("#2", "˨"),
    ("#3", "˧"),
    ("#4", "˦"),
    ("#5", "˥"),
    ("#6", "˩˧"),
    ("#7", "˩˥"),
    ("#8", "˧˥"),
    ("#9", "˧˩"),
    ("#A", "˥˩"),
    ("#B", "˥˧"),
    ("#C", "˩˥˩"),
    ("#D", "˥˩˥"),
    ("+1", "\u030F"),
    ("+2", "\u0300"),
    ("+3", "\u0304"),
    ("+4", "\u0301"),
    ("+5", "\u030B"),
    ("+6", "\u1DC7"),
    ("+7", "\u030C"),
    ("+8", "\u1DC4"),
    ("+9", "\u1DC6"),
    ("+A", "\u0302"),
    ("+B", "\u1DC5"),
    ("+C", "\u1DC8"),
    ("+D", "\u1DC9"),
])

let parse_tone_marker = map(transform_tone_marker, parse_regex(%re("/^#[1-9A-D]|^\+[1-9A-D]/")))

/* [======[Diacritics]======]*/

let transform_naked_diacritic = transformerArray([
    ("=", "\u0329"),
    ("{", "\u032A"),
    ("\\\"", "\u0315"),
    ("~", "\u0303"),
    (">", "\u0325"),
])

let parse_naked_diacritic = map(
    transform_naked_diacritic,
    one_of(`={"~>`)
)

let transform_superscript_letter = transformerArray([
    ("h", "\u02B0"), 
    ("j", "\u02B2"), 
    ("G", "\u02E0"), 
    ("w", "\u02B7"), 
    ("Q", "\u02E4")
])

let parse_superscript_letter = map(
    transform_superscript_letter,
    and_then(
        list{one_of("^")},
        one_of("hjGwQ")
    )
)

let transform_lower_diacritic = transformerArray([
    ("^", "\u032F"), 
    ("a", "\u033A"), 
    ("l", "\u033B"), 
    ("O", "\u0339"), 
    ("c", "\u031C"), 
    ("q", "\u0318"), 
    ("p", "\u0319"), 
    ("+", "\u031F"), 
    ("-", "\u0320"), 
    ("r", "\u031D"), 
    ("T", "\u031E"), 
    ("~", "\u0330"), 
    ("\\\"", "\u0324"),
    ("L", "\u0334"),     
    ("m", "\u033C")
]);

let parse_lower_diacritic = map(
    transform_lower_diacritic,
    and_then(
        list{one_of("-")},
        one_of("^alOcqp+-rT~Lm\""),
    )
)

let transform_upper_diacritic = transformerArray([
    ("?", "\u031A"),
    ("\\\"", "\u030A"),
    ("R", "\u02DE"),
    ("x", "\u033D")
]);

let parse_upper_diacritic = map(
    transform_upper_diacritic,
    and_then(
        list{one_of("+")},
        one_of("?\"Rx")
    )
)

let parse_diacritic = alts(list{
    parse_naked_diacritic, 
    parse_superscript_letter, 
    parse_lower_diacritic,
    parse_upper_diacritic,
});

/* [======[Suprasegmentals and Lines]======]*/

let transform_suprasegs = transformerArray([
    ("'", "\u02C8"), 
    ("';", "ꜛ"),
    (",", "\u02CC"), 
    (",;", "ꜜ"), 
    (":", "ː"),
    (":;", "ˑ"),
    (".", "."), 
    ("}", "\u0361"), 
    ("+u", "\u0306"),
    ("#R", "↗"),
    ("#F", "↘"),
    ("#=", "\u035C"),
    ("|", "|"),
    ("||", "‖")
]);

let parse_suprasegmental = map(
    transform_suprasegs,
    alts(list{
        parse_regex(%re("/^[':,];?/")),
        tag("."),
        tag("}"),
        tag("+u"),
        tag("||"),
        tag("|"),
        tag("#="),
        tag("#R"),
        tag("#F"),
    })
);

// consonants need to be parsed before vowels
let transform_vowel = transformerArray([
    ("i", "i"),
    ("y", "i"),
    ("1", "ɨ"),
    ("W", "ʉ"),
    ("M", "ɯ"),
    ("u", "u"),
    ("I", "ɪ"),
    ("Y", "ʏ"),
    ("I;", "ᵻ"),
    ("U;", "ᵿ"),
    ("U", "ʊ"),
    ("e", "e"), 
    ("0", "ø"),
    ("e;", "ɘ"),
    ("o;", "ɵ"),
    ("7", "ɤ"),
    ("o", "o"),
    ("@", "ə"),  
    ("@;", "ɚ"), 
    ("E", "ɛ"), 
    ("9", "œ"), 
    ("3", "ɜ"), 
    ("3;", "ɝ"), 
    ("O;", "ɞ"), 
    ("2", "ʌ"), 
    ("O", "ɔ"), 
    ("&", "æ"),  
    ("6", "ɐ"), 
    ("a", "a"), 
    ("9;", "ɶ"), 
    ("A", "ɑ"), 
    ("8", "ɒ"),
    ]);

let parse_vowel = map(
    transform_vowel,
    alts(list{
        parse_regex(%re("/^[IUeo@3O9];?/")), // everything that might have a semicolon
        one_of("iy1WMuY07E2&6aA8"),
    })
)

/* [======[Consonants]======]*/

let parse_click = alts(list{
    with_result("ʘ", tag("0;")),
    with_result("ǀ", tag("|;")),
    with_result("ǁ", tag("#;")),
    with_result("!", tag("!")),
    with_result("ǂ", tag("=;")),
});

let parse_ingressive = alts(list{
    with_result("ɓ", tag("b<")),
    with_result("ɗ", tag("d<")),
    with_result("ᶑ", tag("d;<")),
    with_result("ʄ", tag("F<")),
    with_result("ɠ", tag("g<")),
    with_result("ʛ", tag("G;<")),
});

let transform_pulmonic_cons = transformerArray([
    ("m", "m"), 
    ("m;", "ɱ"), ("n", "n"), ("n;", "ɳ"),("J", "ɲ"), ("N", "ŋ"), ("N;", "ɴ"),
    ("p", "p"), 
    ("b", "b"), ("t", "t"), ("d", "d"), ("t;", "ʈ"), ("d;", "ɖ"), ("c", "c"),
    ("F", "ɟ"), ("k", "k"), ("g", "g"),("q", "q"),("G;", "ɢ"),("?;", "ʡ"),
    ("?", "ʔ"),
    ("P","ɸ"),("B","β"),("f","f"),("v","v"),("T","θ"),("D","ð"),
    ("s","s"),("z","z"),("S","ʃ"),("Z","ʒ"),("s;","ʂ"),("z;","ʐ"),
    ("S;","ɕ"),("Z;","ʑ"),("C","ç"),("j;","ʝ"),("x;","ɧ"),("x","x"),
    ("G","ɣ"),("X","χ"),("K","ʁ"),("h;","ħ"),("%","ħ"),("Q","ʕ"),("h","h"),
    ("H","ɦ"),
    ("v;","ⱱ"),("4","ɾ"),("r;","ɽ"),
    ("B;","ʙ"),("r","r"),("K;","ʀ"),("H;","ʜ"),("Q;","ʢ"),
    ("V","ʋ"),("R","ɹ"),("R;","ɻ"),("j","j"),("y;","ɥ"),("M;","ɰ"),
    ("w;","ʍ"),("w","w"),
    ("$","ɬ"),("$;","ɮ"),
    ("l","l"),("l;","ɭ"),("5","ʎ"),("L","ɫ"),("L;","ʟ"),
    ("4;","ɺ"),
]);

let parse_pulmonic = map(
    transform_pulmonic_cons,
    alts(list{
        tag("y;"), // y without semi is a vowel
        parse_regex(%re("/^[mnNtdG?szSZjxhvrBKHQRMw$lL4];?/")), // anything that can be followed by a semicolon
        one_of("TDJFVP5CbkLcfgpqX%")
    })
);

let parse_consonant = alts(list{
    parse_click,
    parse_ingressive,
    parse_pulmonic
});

/* =====[Final Parsing]========*/

let parse_glyph = glyph => {
    alts(list{
        parse_tone_marker,
        parse_diacritic,
        parse_suprasegmental,
        parse_consonant,
        parse_vowel,
    })(glyph)
}

let parse_word = word => {
    must_complete(many1(parse_glyph))(word)
}