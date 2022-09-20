module Option = Belt.Option

@val @scope(("process", "env")) external stage: string = "STAGE"
@val @scope(("process", "env")) external service: string = "SERVICE"
let tableName = service ++ ("-" ++ stage)

type schoolyear = string
type studentid = int
type mp = int

module type Genesis = {
  type username
  type password
  type authenticated
  type t = {
    partition_key: string,
    sort_key: string,
    studentid: int,
    schoolyear: string,
    mp: int,
    course: string,
    unixstamp: float,
    grade: float,
  }

  let gradeHasChanged: (Js.Dict.t<float>, t) => bool

  let login: (Got.instance, username, password) => Future.t<authenticated>
  let fetch: (authenticated, Got.instance, schoolyear, studentid, mp) => Future.t<list<t>>
}

module Genesis: Genesis = {
  type username = string
  type password = string
  type authenticated = unit
  type t = {
    partition_key: string,
    sort_key: string,
    studentid: int,
    schoolyear: string,
    mp: int,
    course: string,
    unixstamp: float,
    grade: float,
  }
  let unixstamp = Js.Date.now()

  let login = (instance, username, password) =>
    Got.get(instance, "parents", ())
    |> Future.flat_map(_, _ =>
      Got.post(instance, "j_security_check", {"j_username": username, "j_password": password})
    )
    |> Future.map(_ => ())

  let makeSortKey = (schoolyear, mp, course, unixstamp) =>
    Js.String.replaceByRe(
      %re("/\\W/g"),
      "",
      schoolyear ++ (string_of_int(mp) ++ (course ++ Js.Float.toString(unixstamp))),
    )

  let makeGrade = (schoolyear, mp, studentid, course, grade, unixstamp): t => {
    partition_key: string_of_int(studentid),
    sort_key: makeSortKey(schoolyear, mp, course, unixstamp),
    studentid: studentid,
    schoolyear: schoolyear,
    mp: mp,
    course: course,
    unixstamp: unixstamp,
    grade: grade,
  }

  let gradeHasChanged = (oldGrades, {course, grade}) =>
    Js.Dict.get(oldGrades, course)->Option.map(o => o != grade)->Option.getWithDefault(true)

  let cleanGrades = (schoolyear, studentid, mp) =>
    List.fold_left((grades, {Cheerio.course: course, grade}) =>
      switch grade {
      | Some(grade) =>
        List.append(list{makeGrade(schoolyear, mp, studentid, course, grade, unixstamp)}, grades)
      | None => grades
      }
    , list{})

  let fetch = (_, instance, schoolyear, studentid, mp) => {
    let params = {
      "tab1": "studentdata",
      "tab2": "gradebook",
      "tab3": "weeklysummary",
      "action": "form",
      "studentid": studentid,
      "mpToView": "MP" ++ string_of_int(mp),
    }

    Got.get(instance, "parents", ~params, ()) |> Future.map(data => {
      let entries =
        data["body"]->Cheerio.load->Cheerio.parse->Array.to_list |> List.map(Cheerio.entryFromJs)

      cleanGrades(schoolyear, studentid, mp, entries)
    })
  }
}

module type Database = {
  type db
  let make: unit => db

  let fetch: (
    db,
    option<schoolyear>,
    option<studentid>,
    option<mp>,
  ) => Future.t<AWS.DynamoDB.query_data<Genesis.t>>
  let write: (db, Genesis.t) => Future.t<Genesis.t>

  let fetchOldGrades: (db, schoolyear, studentid, mp) => Future.t<Js.Dict.t<float>>
}

module Database: Database = {
  type db = AWS.DynamoDB.t
  let make = AWS.DynamoDB.make

  let put: (
    db,
    AWS.DynamoDB.put_params<Genesis.t>,
  ) => Future.t<AWS.DynamoDB.put_params<Genesis.t>> = (db, item, error, success) =>
    AWS.DynamoDB.put(db, item, (err, _) =>
      switch Js.Nullable.toOption(err) {
      | None => success(item)
      | Some(exn) => error(Future.JsError(exn))
      }
    )

  let query: (
    db,
    AWS.DynamoDB.query_params<{
      ":partition_key": string,
      ":schoolyear": Js.Nullable.t<string>,
      ":mp": Js.Nullable.t<int>,
    }>,
  ) => Future.t<AWS.DynamoDB.query_data<Genesis.t>> = (db, params, error, success) =>
    AWS.DynamoDB.query(db, params, (err, result) =>
      switch Js.Nullable.toOption(err) {
      | None => success(result)
      | Some(exn) => error(Future.JsError(exn))
      }
    )

  let fetch = (db, schoolyear, studentid, mp) => {
    let studentid = Option.getExn(studentid)
    let filterExpression = switch (schoolyear, mp) {
    | (Some(_), Some(_)) => Some("schoolyear = :schoolyear and mp = :mp")
    | (Some(_), None) => Some("schoolyear = :schoolyear")
    | (None, Some(_)) => Some("mp = :mp")
    | _ => None
    }

    let params = AWS.DynamoDB.query_params(
      ~tableName,
      ~select="ALL_ATTRIBUTES",
      ~scanIndexForward=false,
      ~keyConditionExpression="partition_key = :partition_key",
      ~expressionAttributeValues={
        ":partition_key": string_of_int(studentid),
        ":schoolyear": Js.Nullable.fromOption(schoolyear),
        ":mp": Js.Nullable.fromOption(mp),
      },
    )

    params()
    |> Option.getWithDefault(filterExpression->Option.map(params(~filterExpression=_, ())))
    |> query(db)
  }

  let write = (db, item) => {
    let params = AWS.DynamoDB.put_params(~tableName, ~item, ())

    put(db, params) |> Future.map(_ => item)
  }

  let fetchOldGrades = (db, schoolyear, studentid, mp) =>
    fetch(db, Some(schoolyear), Some(studentid), Some(mp)) |> Future.map(result => {
      let items = result |> AWS.DynamoDB.itemsGet

      Array.fold_right(({Genesis.course: course, grade}, dict) => {
        Js.Dict.set(dict, course, grade)
        dict
      }, items, Js.Dict.empty())
    })
}
