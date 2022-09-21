module Database = App.Database
module Genesis = App.Genesis
module Option = Belt.Option
module Promise = Js.Promise

module ListFuture = {
  let sequence = xs => List.fold_right((acc, x) => {
      x |> Future.flat_map(_, x => acc |> Future.map(acc => List.append(list{acc}, x)))
    }, xs, Future.pure(list{}))
}

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
  try {
    let params = event->AWS.APIGatewayProxy.Event.pathParametersGet->Js.Nullable.toOption
    let studentid: option<App.studentid> =
      params->Option.flatMap(params => params->Js.Dict.get("studentid"))->Option.map(int_of_string)
    let schoolyear: option<App.schoolyear> =
      params->Option.flatMap(params => params->Js.Dict.get("schoolyear"))
    let mp: option<App.mp> =
      params->Option.flatMap(params => params->Js.Dict.get("mp"))->Option.map(int_of_string)

    Database.fetch(db, schoolyear, studentid, mp)
    |> Future.map(results => {
      let items = AWS.DynamoDB.itemsGet(results)

      items->Js.Json.stringifyAny
    })
    |> Future.to_promise
    |> Promise.then_(body =>
      switch body {
      | Some(body) =>
        Promise.resolve(AWS.APIGatewayProxy.Result.make(~body, ~headers, ~statusCode=200))
      | None =>
        Promise.resolve(
          AWS.APIGatewayProxy.Result.make(
            ~body="could not stringify payload",
            ~headers,
            ~statusCode=500,
          ),
        )
      }
    )
    |> Promise.catch(err => {
      Js.Console.error(err)

      Promise.resolve(
        AWS.APIGatewayProxy.Result.make(~body="unknown error", ~headers, ~statusCode=500),
      )
    })
  } catch {
  | exn =>
    Js.Console.error(exn)

    Promise.resolve(
      AWS.APIGatewayProxy.Result.make(
        ~body="error occured during write",
        ~headers,
        ~statusCode=500,
      ),
    )
  }
}

let fetchUpdatedGrades = (authenticated, studentid, mp) => {
  let oldGrades = Database.fetchOldGrades(db, schoolyear, studentid, mp)
  let newGrades = Genesis.fetch(authenticated, instance, schoolyear, studentid, mp)

  Future.apply(
    oldGrades |> Future.map(oldGrades => List.filter(Genesis.gradeHasChanged(oldGrades))),
    newGrades,
  )
}

let fetchGrades = (authenticated, studentid) =>
  mps |> List.map(fetchUpdatedGrades(authenticated, studentid)) |> ListFuture.sequence

let write: AWS.APIGatewayProxy.handler<{
  "studentid": Js.Nullable.t<int>,
}> = (event, _) =>
  try {
    let studentid: App.studentid = event["studentid"] |> Js.Nullable.toOption |> Option.getExn

    let writeGrades = grades =>
      grades |> List.fold_left(List.append, list{}) |> List.map(grade => Database.write(db, grade))

    Genesis.login(instance, genesis_uname, genesis_pword)
    |> Future.flat_map(_, fetchGrades(_, studentid))
    |> Future.flat_map(_, grades => grades->writeGrades->ListFuture.sequence)
    |> Future.map(results => results->Array.of_list->Js.Json.stringifyAny)
    |> Future.to_promise
    |> Promise.then_(x =>
      switch x {
      | Some(body) =>
        Promise.resolve(AWS.APIGatewayProxy.Result.make(~body, ~headers, ~statusCode=200))
      | None =>
        Promise.resolve(
          AWS.APIGatewayProxy.Result.make(
            ~body="could not stringify payload",
            ~headers,
            ~statusCode=500,
          ),
        )
      }
    )
    |> Promise.catch(err => {
      Js.Console.error(err)

      Promise.resolve(
        AWS.APIGatewayProxy.Result.make(~body="unknown error", ~headers, ~statusCode=500),
      )
    })
  } catch {
  | exn =>
    Js.Console.error(exn)

    Promise.resolve(
      AWS.APIGatewayProxy.Result.make(
        ~body="error occured during write",
        ~headers,
        ~statusCode=500,
      ),
    )
  }
