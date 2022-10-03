module Database = Database
module Genesis = Genesis
module Option = Belt.Option
module Promise = Js.Promise

exception Could_Not_Stringify
module ListFuture = {
  let sequence = xs => List.fold_right((acc, x) => {
      x |> Future.flat_map(_, x => acc->Future.map(acc => List.append(list{acc}, x)))
    }, xs, Future.pure(list{}))
}

@val @scope(("process", "env"))
external genesis_uname: Genesis.username = "GENESIS_UNAME"
@val @scope(("process", "env"))
external genesis_pword: Genesis.password = "GENESIS_PWORD"
@val @scope(("process", "env"))
external baseURL: string = "GENESIS_URL"

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

module App = App.Make({
  @val @scope(("process", "env"))
  external genesis_uname: Js.Nullable.t<Genesis.username> = "GENESIS_UNAME"
  @val @scope(("process", "env"))
  external genesis_pword: Js.Nullable.t<Genesis.password> = "GENESIS_PWORD"
  @val @scope(("process", "env"))
  external baseURL: Js.Nullable.t<string> = "GENESIS_URL"

  let date = Js.Date.make()
})

let headers = Js.Dict.fromList(list{
  ("Access-Control-Allow-Origin", Js.Json.string("*")),
  ("Access-Control-Allow-Credentials", Js.Json.boolean(true)),
})

let grades_read: AWS.APIGatewayProxy.handler<AWS.APIGatewayProxy.Event.t> = (event, _) => {
  let params = event->AWS.APIGatewayProxy.Event.pathParametersGet->Js.Nullable.toOption
  let studentid: option<Genesis.studentid> =
    params->Option.flatMap(params => params->Js.Dict.get("studentid"))->Option.map(int_of_string)
  let schoolyear: option<Genesis.schoolyear> =
    params->Option.flatMap(params => params->Js.Dict.get("schoolyear"))
  let mp: option<Genesis.mp> =
    params->Option.flatMap(params => params->Js.Dict.get("mp"))->Option.map(int_of_string)

  Js.Promise.make((~resolve, ~reject as _) => {
    App.readGrades(~studentid?, ~schoolyear?, ~mp?, ())
    ->Future.map(results =>
      switch results->Js.Json.stringifyAny {
      | Some(result) => result
      | None => raise(Could_Not_Stringify)
      }
    )
    ->Future.fork(
      err => {
        Js.Console.error(err)

        resolve(.
          AWS.APIGatewayProxy.Result.make(~body="something went wrong", ~headers, ~statusCode=500),
        )
      },
      body => resolve(. AWS.APIGatewayProxy.Result.make(~body, ~headers, ~statusCode=200)),
    )
  })
}

let grades_write: AWS.APIGatewayProxy.handler<{
  "studentid": Js.Nullable.t<int>,
}> = (event, _) => {
  let studentid: option<Genesis.studentid> = event["studentid"]->Js.Nullable.toOption

  Js.Promise.make((~resolve, ~reject as _) => {
    App.fetchFreshGrades(studentid)
    ->Future.flat_map(App.writeUpdatedGrades)
    ->Future.map(results =>
      switch results->Array.of_list->Js.Json.stringifyAny {
      | Some(result) => result
      | None => raise(Could_Not_Stringify)
      }
    )
    ->Future.fork(
      err => {
        Js.Console.error(err)

        resolve(.
          AWS.APIGatewayProxy.Result.make(~body="something went wrong", ~headers, ~statusCode=500),
        )
      },
      body => resolve(. AWS.APIGatewayProxy.Result.make(~body, ~headers, ~statusCode=200)),
    )
  })
}

let filters_read: AWS.APIGatewayProxy.handler<AWS.APIGatewayProxy.Event.t> = (event, _) => {
  let params = event->AWS.APIGatewayProxy.Event.pathParametersGet->Js.Nullable.toOption
  let studentid: option<Genesis.studentid> =
    params->Option.flatMap(params => params->Js.Dict.get("studentid"))->Option.map(int_of_string)
  let schoolyear: option<Genesis.schoolyear> =
    params->Option.flatMap(params => params->Js.Dict.get("schoolyear"))

  Js.Promise.make((~resolve, ~reject as _) => {
    App.readFilters(~studentid?, ~schoolyear?, ())
    ->Future.map(results =>
      switch results->Js.Json.stringifyAny {
      | Some(result) => result
      | None => raise(Could_Not_Stringify)
      }
    )
    ->Future.fork(
      err => {
        Js.Console.error(err)

        resolve(.
          AWS.APIGatewayProxy.Result.make(~body="something went wrong", ~headers, ~statusCode=500),
        )
      },
      body => resolve(. AWS.APIGatewayProxy.Result.make(~body, ~headers, ~statusCode=200)),
    )
  })
}
