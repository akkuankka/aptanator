exception LogicError

open Belt.Array

let c: ('a => 'b, 'b => 'c) => ('a => 'c) = (f, g) => arg => {
    f(g(arg))
}

let unwrap = (opt) => switch opt {
    | Some(a) => a
    | None => raise(LogicError)
}
// let words = Js.String.splitByRe(%re("/[ \n]/"))->c(Js.Array2.map(unwrap)
let innerWords = Js.String.splitByRe(%re("/[ \n]/"))
let words = (w) => Js.Array.map(unwrap, innerWords(w)) 

let ident = a => a

let unwrap_error = (error) => switch error {
    | Error(e) => e
    | Ok(_) => raise(LogicError)
}



let process_text = input => {
    let worded = words(input)
    let processed = worded->map(Parse.parse_word)
    let error_list = processed->keep(Belt.Result.isError)
    let with_error_placeholders = processed->map(item => switch item {
        | Ok(word) => word.output
        | Error(_) => "[err]"
    })
    (with_error_placeholders->joinWith(" ", ident), error_list->map(unwrap_error))
};

module J = {
    %%raw(`
function readTextArea(id) {
    return document.getElementById(id).value;
}

function setDivText(id, value) {
    document.getElementById(id).innerHTML = value;
}

function replaceClasses(id, froms, to) {
     document.getElementById(id).classList.remove(...froms);
     document.getElementById(id).classList.add(to);
}

function apply_event_listener(id, event, fn) {
    document.getElementById(id).addEventListener(event, fn);
}

function remove_event_listener(id, event, fn) {
    document.getElementById(id).removeEventListener(event, fn);
}



    `) 
    @val
    external readTextArea: string => string = "readTextArea"

    let getInput = () => readTextArea("input")

    @val 
    external setDivText: (string, string) => () = "setDivText" 

    @val
    external replaceClasses: (string, Js.Array.t<string>, string) => () = "replaceClasses"

    let reportOutput = (s) => setDivText("output", s)

    let reportErrors = (list) => {
        let hadErrors = list != []
        let processed = list->map((error) => `<li class="error-item">${error}</li>`)
        setDivText("error-list", processed->joinWith("", ident))
       replaceClasses("output", ["is-neutral", "is-ok", "is-bad"], if hadErrors {"is-bad"} else {"is-ok"} )
    }

@val 
external should_auto_update: bool = "should_auto_update"

@val
external i_applyEventListener: (string, string, () => ()) => () = "apply_event_listener"
let applyEventListener = (~id, ~event, fn) => {
    i_applyEventListener(id, event, fn)
}

@val
external i_removeEventListener: (string, string, () => ()) => () = "remove_event_listener"
let removeEventListener = (~id, ~event, fn) => {
    i_removeEventListener(id, event, fn)
}

}


let translate = () => {
    let input = J.getInput()
    let (output, errorList) = process_text(input)
    J.reportOutput(output)
    J.reportErrors(errorList)
}


let rec startAutoUpdater = () => {
    J.applyEventListener(~id="input", ~event="input", auto_update_cycle)   
} 
and removeAutoUpdater = () => {
    J.removeEventListener(~id="input", ~event="input", auto_update_cycle)
} and auto_update_cycle = () => {
    if !J.should_auto_update {
        removeAutoUpdater()
    }
    translate()
}

let tostr = (thing) => {
    switch thing {
        | "true" => true
        | _ => false
    }
}


%%raw(`





var should_auto_update = false;

function main() {
    
    // keep auto_update updated
    let auto_update_toggle = document.getElementById("keepup");
    auto_update_toggle.addEventListener("change", () => {
        should_auto_update = auto_update_toggle.checked;
        if (should_auto_update) {
            startAutoUpdater()
        }
    })

    document.getElementById("doit").addEventListener("click", () => translate());
}


main()
`)