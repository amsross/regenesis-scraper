module Database = App.Database
module Genesis = App.Genesis
module Option = Belt.Option
module Promise = Js.Promise
module Affect = BsEffects.Affect
module T = BsAbstract.List.Traversable(Affect.Applicative)

@val @scope(("process", "env")) external stage: string = "STAGE"
@val @scope(("process", "env")) external service: string = "SERVICE"
@val @scope(("process", "env"))
external genesis_uname: Genesis.username = "GENESIS_UNAME"
@val @scope(("process", "env"))
external genesis_pword: Genesis.password = "GENESIS_PWORD"
@val @scope(("process", "env"))
external baseURL: string = "GENESIS_URL"
let tableName = service ++ ("-" ++ stage)

let date = Js.Date.make()
let year = date |> Js.Date.getUTCFullYear |> int_of_float
let schoolyear =
  Js.Date.getUTCMonth(date) +. 1.0 > 7.0
    ? string_of_int(year) ++ ("-" ++ string_of_int(year + 1))
    : string_of_int(year - 1) ++ ("-" ++ string_of_int(year))
let mps = list{1, 2, 3, 4}
let db = Database.make()

let instance = Got.create(
  baseURL,
  {
    "Content-Type": "application/x-www-form-urlencoded",
    "User-Agent": "curl/7.54.0",
  },
)

let headers = Js.Dict.fromList(list{
  ("Access-Control-Allow-Origin", Js.Json.string("*")),
  ("Access-Control-Allow-Credentials", Js.Json.boolean(true)),
})

let read: AWS.APIGatewayProxy.handler<AWS.APIGatewayProxy.Event.t> = (event, _) => {
  let params = event->AWS.APIGatewayProxy.Event.pathParametersGet->Js.Nullable.toOption
  let studentid: option<App.studentid> =
    params->Option.flatMap(params => params->Js.Dict.get("studentid"))->Option.map(int_of_string)
  let schoolyear: option<App.schoolyear> =
    params->Option.flatMap(params => params->Js.Dict.get("schoolyear"))
  let mp: option<App.mp> =
    params->Option.flatMap(params => params->Js.Dict.get("mp"))->Option.map(int_of_string)

  Database.fetch(db, schoolyear, studentid, mp)
  |> Affect.map(AWS.DynamoDB.itemsGet)
  |> Affect.to_promise
  |> Promise.then_(results => results->Js.Json.stringifyAny->Promise.resolve)
  |> Promise.then_(x =>
    switch x {
    | Some(body) => Promise.resolve(body)
    | None => Promise.reject(Js.Exn.raiseError("could not stringify payload"))
    }
  )
  |> Promise.then_(body =>
    Promise.resolve(AWS.APIGatewayProxy.Result.make(~body, ~headers, ~statusCode=200))
  )
}

let fetchUpdatedGrades = (authenticated, studentid, mp) => {
  let oldGrades = Database.fetchOldGrades(db, schoolyear, studentid, mp)
  let newGrades = Genesis.fetch(authenticated, instance, schoolyear, studentid, mp)

  Affect.apply(
    oldGrades |> Affect.map(oldGrades => List.filter(Genesis.gradeHasChanged(oldGrades))),
    newGrades,
  )
}

let fetchGrades = (authenticated, studentid) =>
  mps |> List.map(fetchUpdatedGrades(authenticated, studentid)) |> T.sequence

let write: AWS.APIGatewayProxy.handler<{
  "studentid": Js.Nullable.t<int>,
}> = (event, _) =>
  try {
    let studentid: App.studentid = event["studentid"] |> Js.Nullable.toOption |> Option.getExn

    Genesis.login(instance, genesis_uname, genesis_pword)
    |> Affect.flat_map(_, fetchGrades(_, studentid))
    |> Affect.flat_map(_, grades =>
      grades
      |> List.fold_left(List.append, list{})
      |> List.map(grade => Database.write(db, grade))
      |> T.sequence
    )
    |> Affect.map(Array.of_list)
    |> Affect.to_promise
    |> Promise.then_(results => results->Js.Json.stringifyAny->Promise.resolve)
    |> Promise.then_(x =>
      switch x {
      | Some(body) => Promise.resolve(body)
      | None => Promise.reject(Js.Exn.raiseError("could not stringify payload"))
      }
    )
    |> Promise.then_(body =>
      Promise.resolve(AWS.APIGatewayProxy.Result.make(~body, ~headers, ~statusCode=200))
    )
    |> Promise.catch(err =>
      Promise.resolve(
        AWS.APIGatewayProxy.Result.make(~body="unknown error", ~headers, ~statusCode=500),
      )
    )
  } catch {
  | exn =>
    Js.Console.log(exn)

    Promise.resolve(
      AWS.APIGatewayProxy.Result.make(
        ~body="error occured during wirte",
        ~headers,
        ~statusCode=500,
      ),
    )
  }
