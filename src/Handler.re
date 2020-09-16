module Affect = BsEffects.Affect;
module T = BsAbstract.List.Traversable(Affect.Applicative);
let ((<.), (>.)) = BsAbstract.Function.Infix.((<.), (>.));

[@bs.val] [@bs.scope ("process", "env")] external stage: string = "STAGE";
[@bs.val] [@bs.scope ("process", "env")] external service: string = "SERVICE";
[@bs.val] [@bs.scope ("process", "env")]
external genesis_uname: App.Genesis.username = "GENESIS_UNAME";
[@bs.val] [@bs.scope ("process", "env")]
external genesis_pword: App.Genesis.password = "GENESIS_PWORD";
[@bs.val] [@bs.scope ("process", "env")]
external baseURL: string = "GENESIS_URL";
let tableName = service ++ "-" ++ stage;

let date = Js.Date.make();
let year = date |> Js.Date.getUTCFullYear |> int_of_float;
let schoolyear =
  date |> Js.Date.getUTCMonth |> (+.)(1.0) |> (<)(7.0)
    ? string_of_int(year) ++ "-" ++ string_of_int(year + 1)
    : string_of_int(year - 1) ++ "-" ++ string_of_int(year);
let mps = [1, 2, 3, 4];
let db = App.Database.make();

let instance =
  Got.create(
    baseURL,
    {
      "Content-Type": "application/x-www-form-urlencoded",
      "User-Agent": "curl/7.54.0",
    },
  );

let headers =
  Js.Dict.fromList([
    ("Access-Control-Allow-Origin", Js.Json.string("*")),
    ("Access-Control-Allow-Credentials", Js.Json.boolean(true)),
  ]);

let read: AWS.APIGatewayProxy.handler(AWS.APIGatewayProxy.Event.t) =
  (event, _) => {
    let params =
      event->AWS.APIGatewayProxy.Event.pathParametersGet->Js.Nullable.toOption;
    let studentid: option(App.studentid) =
      BsAbstract.Option.Infix.(
        params >>= Js.Dict.get(_, "studentid") <#> int_of_string
      );
    let schoolyear: option(App.schoolyear) =
      BsAbstract.Option.Infix.(params >>= Js.Dict.get(_, "schoolyear"));
    let mp: option(App.mp) =
      BsAbstract.Option.Infix.(
        params >>= Js.Dict.get(_, "mp") <#> int_of_string
      );

    Affect.Infix.(
      App.Database.fetch(db, schoolyear, studentid, mp)
      <#> AWS.DynamoDB.itemsGet
    )
    |> Js.Promise.(
         Affect.to_promise
         >. then_(Js.Json.stringifyAny >. resolve)
         >. then_(
              fun
              | Some(body) => resolve(body)
              | None =>
                reject(Js.Exn.raiseError("could not stringify payload")),
            )
         >. then_(body =>
              resolve(
                AWS.APIGatewayProxy.Result.make(
                  ~body,
                  ~headers,
                  ~statusCode=200,
                ),
              )
            )
       );
  };

let fetchUpdatedGrades = (authenticated, studentid, mp) => {
  let oldGrades = App.Database.fetchOldGrades(db, schoolyear, studentid, mp);
  let newGrades =
    App.Genesis.fetch(authenticated, instance, schoolyear, studentid, mp);

  Affect.Infix.(
    oldGrades <#> (List.filter <. App.Genesis.gradeHasChanged) <*> newGrades
  );
};

let fetchGrades = (authenticated, studentid) =>
  BsAbstract.List.Infix.(
    [fetchUpdatedGrades(authenticated, studentid)] <*> mps |> T.sequence
  );

let write: AWS.APIGatewayProxy.handler({. "studentid": Js.Nullable.t(int)}) =
  (event, _) => {
    let studentid: App.studentid =
      event##studentid |> Js.Nullable.toOption |> Belt.Option.getExn;

    Affect.Infix.(
      App.Genesis.login(instance, genesis_uname, genesis_pword)
      >>= fetchGrades(_, studentid)
      <#> List.fold_left(List.append, [])
      <#> List.map(App.Database.write(db))
      >>= T.sequence
      <#> Array.of_list
    )
    |> Js.Promise.(
         Affect.to_promise
         >. then_(Js.Json.stringifyAny >. resolve)
         >. then_(
              fun
              | Some(body) => resolve(body)
              | None =>
                reject(Js.Exn.raiseError("could not stringify payload")),
            )
         >. then_(body =>
              resolve(
                AWS.APIGatewayProxy.Result.make(
                  ~body,
                  ~headers,
                  ~statusCode=200,
                ),
              )
            )
       );
  };
