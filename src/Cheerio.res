type dom
type selector = string => dom
@send external text: dom => string = "text"
@module("cheerio") external load: string => selector = "load"

let trim: string => string = Js.String.trim
let toNumber: string => option<float> = str =>
  try (str |> trim |> Js.String.replaceByRe(%re("/(MP[0-9]|[^\\d.])/g"), ""))
  ->float_of_string
  ->Some catch {
  | _ => None
  }

@deriving({jsConverter: newType})
type entry = {
  course: string,
  grade: option<float>,
}

let parse: selector => array<abs_entry> = %raw(`
  $ => $("table.list > tbody > tr:not(.listheading)").map((_, e) => ({
    course: trim($(e).children("td:nth-child(1)").text()),
    grade: toNumber($(e).children("td:nth-child(3)").text()),
  })).toArray()
  `)
