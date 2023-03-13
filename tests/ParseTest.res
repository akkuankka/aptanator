open Test
open Parse


// Maybe I eta-factorise these given the last args are the same?
let strEq = (~message=?, a, b) => assertion(
    ~message?, 
    ~operator="string equality", 
    (x, y) => x == y, 
    a, b)

let okAndNoInput = (~message=?, parserOutput, shouldBe) => assertion(
    ~message?,
    ~operator="successful parsing match",
    (output, match) => switch output {
        | Error(_) => false
        | Ok(result) => result.input == "" && result.output == match
    },
    parserOutput, shouldBe)

let okAndRemaining = (~message=?, parserOutput, shouldBe, remaining) => assertion(
    ~message?,
    ~operator="successful parsing match",
    (output, (match, remaining)) => switch output {
        | Error(_) => false
        | Ok(result) => result.input == remaining && result.output == match
    },
    parserOutput, (shouldBe, remaining))

test("parser tag", () => {
    okAndNoInput(tag("bees", "bees"), "bees")
    okAndRemaining(tag("blue", "bluebottle"), "blue", "bottle")
})

test("parser regex all one letter", () => {
    okAndNoInput(parse_regex(%re("/^[ab]+/"), "aaa"), "aaa")
    okAndRemaining(parse_regex(%re("/^[ab]+/"), "aaaaccc"), "aaaa", "ccc")
})

test("parser regex multiple letters", () => {
    okAndNoInput(parse_regex(%re("/^[ab]+/"), "abba"), "abba")
    okAndRemaining(parse_regex(%re("/^[ab]+/"), "aabccc"), "aab", "ccc")
})

test("map", () => {
    let testString = "AAAA"
    okAndNoInput(map(Js.String.toUpperCase, tag("aaaa"), "aaaa"), testString)
})

test("alts", () => {
    okAndNoInput(alts(list{
        tag("ee"), 
        tag("oooooo"),
        tag("zozo"),
        tag("zoinks")},
        "zozo"
        ), "zozo")

    okAndNoInput(alts(list{
        tag("ee"), 
        tag("oooooo"),
        tag("zozo"),
        tag("zoinks")},
        "zoinks"
        ), "zoinks")
})

test("many1 tag", () => {
    okAndNoInput(many1(tag("et"), "etetetet"), "etetetet")
    okAndRemaining(many1(tag("osc"), "oscoscos"), "oscosc", "os")
})

test ("many1 alts", () => {

    okAndRemaining( ~message="Tag only",
        many1(alts(
            list{
                tag("et"),
                tag("osc"),
            }
        ), "oscetetowww"),
        "oscetet", "owww")

    okAndRemaining( ~message="Tag and finite regex",
        many1(alts(
            list{
                tag("boots"),
                tag("cats"),
                parse_regex(%re("/^[xyz]/"))
            }
        ), "bootsxcatsybootszoinks"),
        "bootsxcatsybootsz", "oinks"
    )

    okAndNoInput ( ~message="Infinite regex",
        many1(alts(list{
            parse_regex(%re("/^x+/")),
            parse_regex(%re("/^y+/")),
        }), "xxyyxx"), "xxyyxx")

    okAndRemaining ( ~message="Infinite regex + tag",
        many1(alts(list{
            parse_regex(%re("/^x+/")),
            tag("org"),
            parse_regex(%re("/^y+/")),
        }), "xorgxyyorgxxzz"), "xorgxyyorgxx", "zz")   
    
    // TODO this causes a spinloop?!
    okAndRemaining(
        many1(alts(
            list{
                tag("et"),
                tag("osc"),
                parse_regex(%re("/^w+/"))
            }
        ), "oscetwwwetowww"),
        "oscetwwwet", "owww")
})

test("one_of", () => {
    okAndNoInput(one_of("aeiou", "a"), "a")
    okAndRemaining(one_of("aeiou", "ak"), "a", "k")
})

test("parse consonants", () => {
    okAndRemaining(~message="ka", parse_consonant("ka"), "k", "a")
    okAndRemaining(~message="la", parse_consonant("la"), "l", "a")
})


test("parse words", () => {
    okAndNoInput(~message="cons. vowel", parse_word("ka"), "ka")
})


test("parse individual glyphs", () => {
    okAndNoInput(~message="k", parse_glyph("k"), "k")
    okAndRemaining(~message="ka", parse_glyph("ka"), "k", "a")
    okAndNoInput(~message="a", parse_glyph("a"), "a")
    okAndNoInput(~message="q", parse_glyph("q"), "q")
    okAndNoInput(~message="r-schwa", parse_glyph("@;"), "ɚ")
    okAndNoInput(~message="r-schwa", parse_glyph("@;"), "ɚ")
    okAndNoInput(~message="un v", parse_glyph("2"), "ʌ")
})
