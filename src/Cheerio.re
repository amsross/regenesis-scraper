type dom;
type selector = string => dom;
[@bs.send] external text: dom => string = "text";
[@bs.module "cheerio"] external load: string => selector = "load";

let trim: string => string = Js.String.trim;
let toNumber: string => option(float) =
  str =>
    try(
      (str |> trim |> Js.String.replaceByRe([%re "/[^\\d.]/g"], ""))
      ->float_of_string
      ->Some
    ) {
    | _ => None
    };

[@bs.deriving {jsConverter: newType}]
type entry = {
  course: string,
  grade: option(float),
};

let parse: selector => array(abs_entry) = [%raw
  {|
  $ => $("table.list > tbody > tr:not(.listheading)").map((_, e) => ({
    course: trim($(e).children("td:nth-child(1)").text()),
    grade: toNumber($(e).children("td:nth-child(3)").text()),
  })).toArray()
  |}
];
