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

  let gradeToDict: t => Js.Dict.t(string);
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

  let gradeToDict = x =>
    Js.Dict.fromList([
      ("partition_key", x.partition_key),
      ("sort_key", x.sort_key),
      ("studentid", string_of_int(x.studentid)),
      ("schoolyear", x.schoolyear),
      ("mp", string_of_int(x.mp)),
      ("course", x.course),
      ("unixstamp", Js.Float.toString(x.unixstamp)),
      ("grade", Js.Float.toString(x.grade)),
    ]);

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
    Affect.affect(AWS.DynamoDB.query_data);
  let write: (db, Genesis.t) => Affect.affect(Genesis.t);

  let fetchOldGrades:
    (db, schoolyear, studentid, mp) => Affect.affect(Js.Dict.t(float));
};

module Database: Database = {
  type db = AWS.DynamoDB.t;
  let make = AWS.DynamoDB.make;

  let put:
    (db, AWS.DynamoDB.put_params) => Affect.affect(AWS.DynamoDB.put_params) =
    (db, item, error, success) => {
      AWS.DynamoDB.put(db, item, (err, _) =>
        Js.Nullable.isNullable(err)
          ? success(item) : error(Js.Nullable.toOption(err))
      );
    };

  let query:
    (db, AWS.DynamoDB.query_params) => Affect.affect(AWS.DynamoDB.query_data) =
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
        ~expressionAttributeValues=
          Js.Dict.fromList(
            List.concat([
              [(":partition_key", string_of_int(studentid))],
              Belt.Option.mapWithDefault(schoolyear, [], v =>
                [(":schoolyear", v)]
              ),
              Belt.Option.mapWithDefault(mp, [], v =>
                [(":mp", string_of_int(v))]
              ),
            ]),
          ),
      );

    BsAbstract.Option.Infix.(
      params() |? (filterExpression <#> params(~filterExpression=_, ()))
    )
    |> query(db);
  };

  let write = (db, grade) => {
    let params =
      AWS.DynamoDB.put_params(
        ~tableName,
        ~item=Genesis.gradeToDict(grade),
        (),
      );

    Affect.Infix.(put(db, params) <#> const(grade));
  };

  let fetchOldGrades = (db, schoolyear, studentid, mp) =>
    Affect.Infix.(
      fetch(db, Some(schoolyear), Some(studentid), Some(mp))
      <#> AWS.DynamoDB.itemsGet
      <#> Array.fold_right(
            (v, a) => {
              let course = Js.Dict.get(v, "course");
              let grade = Js.Dict.get(v, "grade");

              switch (course, grade) {
              | (Some(course), Some(grade)) =>
                Js.Dict.set(a, course, float_of_string(grade));
                a;
              | _ => a
              };
            },
            _,
            Js.Dict.empty(),
          )
    );
};
