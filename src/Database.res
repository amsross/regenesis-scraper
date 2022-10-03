module Option = Belt.Option
module Array = Belt.Array
module Decode = Json.Decode

module OptionApply = Fantasy.Apply({
  type t<'a> = option<'a>
  include Belt.Option
})

type db = AWS.DynamoDB.t

let make = AWS.DynamoDB.make

@val @scope(("process", "env")) external stage: Js.Nullable.t<string> = "STAGE"
@val @scope(("process", "env")) external service: Js.Nullable.t<string> = "SERVICE"
let tableName =
  OptionApply.liftA2(service->Js.Nullable.toOption, stage->Js.Nullable.toOption, (service, stage) =>
    service ++ "-" ++ stage
  )->Option.getWithDefault("unknown")

module Write = {
  type data = AWS.DynamoDB.put_data

  module Params = {
    type t<'t> = AWS.DynamoDB.put_params<'t>

    let make: 'a. 'a => t<'a> = item => AWS.DynamoDB.put_params(~tableName, ~item, ())
  }

  let make: 'a. ('a, db) => Future.t<data> = (item, db) => {
    let params = Params.make(item)

    (error, success) =>
      AWS.DynamoDB.put(db, params, (err, data) =>
        switch Js.Nullable.toOption(err) {
        | None => success(data)
        | Some(exn) => error(Future.JsError(exn))
        }
      )
  }
}

module Read = {
  type data = AWS.DynamoDB.query_data

  module Params = {
    type t<'t> = AWS.DynamoDB.query_params<'t>

    let make: 'a. ('a, string) => t<'a> = (expressionAttributeValues, filterExpression) =>
      AWS.DynamoDB.query_params(
        ~tableName,
        ~select="ALL_ATTRIBUTES",
        ~scanIndexForward=false,
        ~filterExpression,
        ~keyConditionExpression="partition_key = :partition_key",
        ~expressionAttributeValues,
        (),
      )
  }

  let make: 'a 'b. (Params.t<'a>, db) => Future.t<data> = (params, db, error, success) =>
    AWS.DynamoDB.query(db, params, (err, result) =>
      switch Js.Nullable.toOption(err) {
      | None => success(result)
      | Some(exn) => error(Future.JsError(exn))
      }
    )
}

module Grades = {
  type t = Genesis.t

  let decode: Decode.t<t> = Decode.object((field): t => {
    partition_key: field.required(. "partition_key", Decode.string),
    sort_key: field.required(. "sort_key", Decode.string),
    studentid: field.required(. "studentid", Decode.int),
    schoolyear: field.required(. "schoolyear", Decode.string),
    mp: field.required(. "mp", Decode.int),
    course: field.required(. "course", Decode.string),
    unixstamp: field.required(. "unixstamp", Decode.float),
    grade: field.required(. "grade", Decode.float),
  })

  let create = (db, ~grade: t) => Write.make(grade, db)->Future.map(_ => grade)

  let read = (db, ~studentid: Genesis.studentid, ~schoolyear=?, ~mp=?, ()): Future.t<array<t>> => {
    let filterExpression = switch (schoolyear, mp) {
    | (Some(_), Some(_)) => "studentid = :studentid and schoolyear = :schoolyear and mp = :mp"
    | (Some(_), None) => "studentid = :studentid and schoolyear = :schoolyear"
    | (None, Some(_)) => "studentid = :studentid and mp = :mp"
    | _ => "studentid = :studentid"
    }

    Read.Params.make(
      {
        ":partition_key": string_of_int(studentid),
        ":studentid": studentid,
        ":schoolyear": Js.Nullable.fromOption(schoolyear),
        ":mp": Js.Nullable.fromOption(mp),
      },
      filterExpression,
    )
    ->Read.make(db)
    ->Future.map(result => {
      let items = result->AWS.DynamoDB.itemsGet

      items->Array.reduce([], (items, json) => {
        switch json->Json.decode(decode) {
        | Ok(item) => items->Array.concat([item])
        | Error(parseError) => {
            Js.Console.error("unable to parse item: " ++ parseError)
            items
          }
        }
      })
    })
  }
}
