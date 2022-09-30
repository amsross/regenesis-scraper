module Option = Belt.Option

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
  type data<'t> = AWS.DynamoDB.query_data<'t>

  module Params = {
    type t<'t> = AWS.DynamoDB.query_params<'t>

    let make: 'a. (string, 'a, option<string>) => t<'a> = (
      keyConditionExpression,
      expressionAttributeValues,
      filterExpression,
    ) =>
      AWS.DynamoDB.query_params(
        ~tableName,
        ~select="ALL_ATTRIBUTES",
        ~scanIndexForward=false,
        ~filterExpression?,
        ~keyConditionExpression,
        ~expressionAttributeValues,
        (),
      )
  }

  let make: 'a 'b. (Params.t<'a>, db) => Future.t<data<'b>> = (params, db, error, success) =>
    AWS.DynamoDB.query(db, params, (err, result) =>
      switch Js.Nullable.toOption(err) {
      | None => success(result)
      | Some(exn) => error(Future.JsError(exn))
      }
    )
}

module Grades = {
  let create = (db, ~grade: Genesis.t) => Write.make(grade, db)->Future.map(_ => grade)

  let read = (db, ~studentid: Genesis.studentid, ~schoolyear=?, ~mp=?, ()): Future.t<
    array<Genesis.t>,
  > => {
    let filterExpression = switch (schoolyear, mp) {
    | (Some(_), Some(_)) => Some("schoolyear = :schoolyear and mp = :mp")
    | (Some(_), None) => Some("schoolyear = :schoolyear")
    | (None, Some(_)) => Some("mp = :mp")
    | _ => None
    }

    Read.Params.make(
      "partition_key = :partition_key",
      {
        ":partition_key": string_of_int(studentid),
        ":schoolyear": Js.Nullable.fromOption(schoolyear),
        ":mp": Js.Nullable.fromOption(mp),
      },
      filterExpression,
    )
    ->Read.make(db)
    ->Future.map(result => result->AWS.DynamoDB.itemsGet)
  }
}
