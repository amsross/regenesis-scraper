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
  $ => $("div.itemContainer > div").map((_, e) => ({
    course: trim($(e).find("div:nth-child(2) > div > span").text()),
    grade: toNumber($(e).children("div:nth-child(1)").text()),
  })).toArray()
  `)
