module Affect = BsEffects.Affect;
let const = BsAbstract.Function.(const);
let (>.) = BsAbstract.Function.Infix.((>.));

[@bs.val] [@bs.scope ("process", "env")] external stage: string = "STAGE";
[@bs.val] [@bs.scope ("process", "env")] external service: string = "SERVICE";
let tableName = service ++ "-" ++ stage;

type schoolyear = string;
type studentid = int;
type mp = int;

module type Genesis = {
  type username;
  type password;
  type authenticated;
  [@bs.deriving {jsConverter: newType}]
  type t = {
    partition_key: string,
    sort_key: string,
    studentid: int,
    schoolyear: string,
    mp: int,
    course: string,
    unixstamp: float,
    grade: float,
  };

  let gradeHasChanged: (Js.Dict.t(float), t) => bool;

  let login:
    (Got.instance, username, password) => Affect.affect(authenticated);
  let fetch:
    (authenticated, Got.instance, schoolyear, studentid, mp) =>
    Affect.affect(list(t));
};

module Genesis: Genesis = {
  type username = string;
  type password = string;
  type authenticated = unit;
  [@bs.deriving {jsConverter: newType}]
  type t = {
    partition_key: string,
    sort_key: string,
    studentid: int,
    schoolyear: string,
    mp: int,
    course: string,
    unixstamp: float,
    grade: float,
  };
  let unixstamp = Js.Date.now();

  let login = (instance, username, password) =>
    Affect.Infix.(
      Got.get(instance, "parents")
      >>= const(
            Got.post(
              instance,
              "/j_security_check",
              {"j_username": username, "j_password": password},
            ),
          )
      >>= const(Affect.pure())
    );

  let makeSortKey = (schoolyear, mp, course, unixstamp) =>
    Js.String.replaceByRe(
      [%bs.re "/\\W/g"],
      "",
      schoolyear
      ++ string_of_int(mp)
      ++ course
      ++ Js.Float.toString(unixstamp),
    );

  let makeGrade = (schoolyear, mp, studentid, course, grade, unixstamp): t => {
    partition_key: string_of_int(studentid),
    sort_key: makeSortKey(schoolyear, mp, course, unixstamp),
    studentid,
    schoolyear,
    mp,
    course,
    unixstamp,
    grade,
  };

  let gradeHasChanged = (oldGrades, {course, grade}) =>
    Js.Dict.get(oldGrades, course)
    ->Belt.Option.map(o => o != grade)
    ->Belt.Option.getWithDefault(true);

  let cleanGrades = (schoolyear, studentid, mp) =>
    List.fold_left(
      (grades, {Cheerio.course, grade}) =>
        switch (grade) {
        | Some(grade) =>
          List.append(
            [makeGrade(schoolyear, mp, studentid, course, grade, unixstamp)],
            grades,
          )
        | None => grades
        },
      [],
    );

  let fetch = (_, instance, schoolyear, studentid, mp) => {
    let params = {
      "tab1": "studentdata",
      "tab2": "gradebook",
      "tab3": "weeklysummary",
      "action": "form",
      "studentid": studentid,
      "mpToView": "MP" ++ string_of_int(mp),
    };

    Affect.Infix.(
      Got.get(instance, "/parents", ~params)
      <#> (data => data##body)
      <#> Cheerio.(load >. parse >. Array.to_list >. List.map(entryFromJs))
      <#> cleanGrades(schoolyear, studentid, mp)
    );
  };
};

module type Database = {
  type db;
  let make: unit => db;

  let fetch:
    (db, option(schoolyear), option(studentid), option(mp)) =>
    Affect.affect(AWS.DynamoDB.query_data(Genesis.abs_t));
  let write: (db, Genesis.t) => Affect.affect(Genesis.t);

  let fetchOldGrades:
    (db, schoolyear, studentid, mp) => Affect.affect(Js.Dict.t(float));
};

module Database: Database = {
  type db = AWS.DynamoDB.t;
  let make = AWS.DynamoDB.make;

  let put:
    (db, AWS.DynamoDB.put_params(Genesis.abs_t)) =>
    Affect.affect(AWS.DynamoDB.put_params(Genesis.abs_t)) =
    (db, item, error, success) => {
      AWS.DynamoDB.put(db, item, (err, _) =>
        Js.Nullable.isNullable(err)
          ? success(item) : error(Js.Nullable.toOption(err))
      );
    };

  let query:
    (
      db,
      AWS.DynamoDB.query_params({
        .
        ":partition_key": string,
        ":schoolyear": Js.Nullable.t(string),
        ":mp": Js.Nullable.t(int),
      })
    ) =>
    Affect.affect(AWS.DynamoDB.query_data(Genesis.abs_t)) =
    (db, params, error, success) =>
      AWS.DynamoDB.query(db, params, (err, result) =>
        Js.Nullable.isNullable(err)
          ? success(result) : error(Js.Nullable.toOption(err))
      );

  let fetch = (db, schoolyear, studentid, mp) => {
    let studentid = Belt.Option.getExn(studentid);
    let filterExpression =
      switch (schoolyear, mp) {
      | (Some(_), Some(_)) => Some("schoolyear = :schoolyear and mp = :mp")
      | (Some(_), None) => Some("schoolyear = :schoolyear")
      | (None, Some(_)) => Some("mp = :mp")
      | _ => None
      };

    let params =
      AWS.DynamoDB.query_params(
        ~tableName,
        ~select="ALL_ATTRIBUTES",
        ~scanIndexForward=false,
        ~keyConditionExpression="partition_key = :partition_key",
        ~expressionAttributeValues={
          ":partition_key": string_of_int(studentid),
          ":schoolyear": Js.Nullable.fromOption(schoolyear),
          ":mp": Js.Nullable.fromOption(mp),
        },
      );

    BsAbstract.Option.Infix.(
      params() |? (filterExpression <#> params(~filterExpression=_, ()))
    )
    |> query(db);
  };

  let write = (db, grade) => {
    let params =
      AWS.DynamoDB.put_params(~tableName, ~item=Genesis.tToJs(grade), ());

    Affect.Infix.(put(db, params) <#> const(grade));
  };

  let fetchOldGrades = (db, schoolyear, studentid, mp) =>
    Affect.Infix.(
      fetch(db, Some(schoolyear), Some(studentid), Some(mp))
      <#> AWS.DynamoDB.itemsGet
      <#> Array.fold_right(
            (v, a) => {
              let {Genesis.course, grade} = Genesis.tFromJs(v);
              Js.Dict.set(a, course, grade);
              a;
            },
            _,
            Js.Dict.empty(),
          )
    );
};
