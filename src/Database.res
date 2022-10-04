module Array = Belt.Array
module Option = Belt.Option
module StringMap = Belt.Map.String
module Console = Js.Console
module Nullable = Js.Nullable
module Promise = Js.Promise
module Decode = Json.Decode

module OptionApply = Fantasy.Apply({
  type t<'a> = option<'a>
  include Belt.Option
})

type db = AWS.DynamoDB.t

let make = AWS.DynamoDB.make

@val @scope(("process", "env")) external stage: Nullable.t<string> = "STAGE"
@val @scope(("process", "env")) external service: Nullable.t<string> = "SERVICE"
let tableName =
  OptionApply.liftA2(service->Nullable.toOption, stage->Nullable.toOption, (service, stage) =>
    service ++ "-" ++ stage
  )->Option.getWithDefault("unknown")

module Create = {
  type data = AWS.DynamoDB.PutItemCommand.Output.t

  module Params = {
    type t = AWS.DynamoDB.PutItemCommand.t

    let make = item =>
      AWS.DynamoDB.PutItemCommand.Input.t(
        ~item,
        ~returnConsumedCapacity=#TOTAL,
        ~returnItemCollectionMetrics=#SIZE,
        ~returnValues=#NONE,
        ~tableName,
        (),
      )->AWS.DynamoDB.PutItemCommand.make
  }

  let make: (Params.t, db) => Future.t<data> = (params, db, error, success) => {
    AWS.DynamoDB.PutItemCommand.send(db, params)
    |> Promise.then_(result => success(result)->Promise.resolve)
    |> Promise.catch(err => error(Future.PromiseError(err))->Promise.resolve)
    |> ignore
  }
}

module Read = {
  type data = AWS.DynamoDB.QueryCommand.Output.t

  module Params = {
    type t = AWS.DynamoDB.QueryCommand.t

    let make = (expressionAttributeValues, filterExpression) =>
      AWS.DynamoDB.QueryCommand.Input.t(
        ~tableName,
        ~select=#ALL_ATTRIBUTES,
        ~returnConsumedCapacity=#TOTAL,
        ~scanIndexForward=false,
        ~filterExpression,
        ~keyConditionExpression="partition_key = :partition_key",
        ~expressionAttributeValues,
        (),
      )->AWS.DynamoDB.QueryCommand.make
  }

  let make: (Params.t, db) => Future.t<data> = (params, db, error, success) =>
    AWS.DynamoDB.QueryCommand.send(db, params)
    |> Promise.then_(result => success(result)->Promise.resolve)
    |> Promise.catch(err => error(Future.PromiseError(err))->Promise.resolve)
    |> ignore
}

module Grades = {
  type t = Genesis.t

  let decode: Decode.t<t> = Decode.object((field): t => {
    partition_key: field.required(. "partition_key", AWS.DynamoDB.Decode.string),
    sort_key: field.required(. "sort_key", AWS.DynamoDB.Decode.string),
    studentid: field.required(. "studentid", AWS.DynamoDB.Decode.int),
    schoolyear: field.required(. "schoolyear", AWS.DynamoDB.Decode.string),
    mp: field.required(. "mp", AWS.DynamoDB.Decode.int),
    course: field.required(. "course", AWS.DynamoDB.Decode.string),
    unixstamp: field.required(. "unixstamp", AWS.DynamoDB.Decode.float),
    grade: field.required(. "grade", AWS.DynamoDB.Decode.float),
  })

  let create = (db, ~grade: t) => {
    let item = Js.Dict.fromArray([
      ("partition_key", Js.Dict.fromArray([("S", grade.partition_key)])),
      ("sort_key", Js.Dict.fromArray([("S", grade.sort_key)])),
      ("studentid", Js.Dict.fromArray([("N", string_of_int(grade.studentid))])),
      ("schoolyear", Js.Dict.fromArray([("S", grade.schoolyear)])),
      ("mp", Js.Dict.fromArray([("N", string_of_int(grade.mp))])),
      ("course", Js.Dict.fromArray([("S", grade.course)])),
      ("unixstamp", Js.Dict.fromArray([("N", Js.Float.toString(grade.unixstamp))])),
      ("grade", Js.Dict.fromArray([("N", Js.Float.toString(grade.grade))])),
    ])

    Create.Params.make(item)->Create.make(db)->Future.map(_ => grade)
  }

  let read = (db, ~studentid: Genesis.studentid, ~schoolyear=?, ~mp=?, ()): Future.t<array<t>> => {
    let expressionAttributeValues = [
      (":partition_key", Js.Dict.fromArray([("S", string_of_int(studentid))])),
      (":studentid", Js.Dict.fromArray([("N", string_of_int(studentid))])),
    ]

    let (filterExpression, expressionAttributeValues) = switch (schoolyear, mp) {
    | (Some(schoolyear), Some(mp)) => (
        "studentid = :studentid and schoolyear = :schoolyear and mp = :mp",
        [
          (":schoolyear", Js.Dict.fromArray([("S", schoolyear)])),
          (":mp", Js.Dict.fromArray([("N", string_of_int(mp))])),
        ]->Array.concat(expressionAttributeValues),
      )
    | (Some(schoolyear), None) => (
        "studentid = :studentid and schoolyear = :schoolyear",
        [(":schoolyear", Js.Dict.fromArray([("N", schoolyear)]))]->Array.concat(
          expressionAttributeValues,
        ),
      )
    | (None, Some(mp)) => (
        "studentid = :studentid and mp = :mp",
        [(":mp", Js.Dict.fromArray([("N", string_of_int(mp))]))]->Array.concat(
          expressionAttributeValues,
        ),
      )
    | _ => ("studentid = :studentid", expressionAttributeValues)
    }

    Read.Params.make(Js.Dict.fromArray(expressionAttributeValues), filterExpression)
    ->Read.make(db)
    ->Future.map(result => {
      let json = result.items->Nullable.toOption

      json
      ->Option.map(json =>
        json->Array.reduce([], (items: array<t>, json) => {
          switch json->Json.decode(decode) {
          | Ok(item) => items->Array.concat([item])
          | Error(parseError) => {
              Console.error("unable to parse items: " ++ parseError)
              items
            }
          }
        })
      )
      ->Option.getWithDefault([])
    })
  }
}

module Filters = {
  type t = {
    schoolyear: Genesis.schoolyear,
    mps: array<Genesis.mp>,
  }

  type db_t = {
    studentid: Genesis.studentid,
    schoolyear: Genesis.schoolyear,
    mp: Genesis.mp,
  }

  let decode: Decode.t<db_t> = Decode.object((field): db_t => {
    studentid: field.required(. "studentid", AWS.DynamoDB.Decode.int),
    schoolyear: field.required(. "schoolyear", AWS.DynamoDB.Decode.string),
    mp: field.required(. "mp", AWS.DynamoDB.Decode.int),
  })

  let create = (db, ~filter: db_t) => {
    let item = Js.Dict.fromArray([
      (":studentid", Js.Dict.fromArray([("N", string_of_int(filter.studentid))])),
      (":schoolyear", Js.Dict.fromArray([("S", filter.schoolyear)])),
      (":mp", Js.Dict.fromArray([("N", string_of_int(filter.mp))])),
    ])

    Create.Params.make(item)->Create.make(db)->Future.map(_ => filter)
  }

  let filtersByYear = Array.reduce(_, StringMap.empty, (map, filter) => {
    let key = filter.schoolyear
    map->StringMap.getWithDefault(key, [])->Belt.Array.concat([filter]) |> map->StringMap.set(key)
  })

  let read = (db, ~studentid: Genesis.studentid, ~schoolyear=?, ()): Future.t<array<t>> => {
    let expressionAttributeValues = [
      (":partition_key", Js.Dict.fromArray([("S", "filters")])),
      (":studentid", Js.Dict.fromArray([("N", string_of_int(studentid))])),
    ]

    let (filterExpression, expressionAttributeValues) = switch schoolyear {
    | Some(schoolyear) => (
        "studentid = :studentid and schoolyear = :schoolyear",
        [(":schoolyear", Js.Dict.fromArray([("S", schoolyear)]))]->Array.concat(
          expressionAttributeValues,
        ),
      )
    | _ => ("studentid = :studentid", expressionAttributeValues)
    }


    Read.Params.make(Js.Dict.fromArray(expressionAttributeValues), filterExpression)
    ->Read.make(db)
    ->Future.map(result => {
      let items = result.items->Nullable.toOption

      items
      ->Option.map(json =>
        json
        ->Array.reduce([], (items, json) => {
          switch json->Json.decode(decode) {
          | Ok(item) => items->Array.concat([item])
          | Error(parseError) => {
              Console.error("unable to parse items: " ++ parseError)
              items
            }
          }
        })
        ->filtersByYear
        ->StringMap.reduce([], (filters, schoolyear, mps) => {
          let mps = mps->Array.map(({mp}) => mp)

          filters->Array.concat([{schoolyear: schoolyear, mps: mps}])
        })
      )
      ->Option.getWithDefault([])
    })
  }
}
