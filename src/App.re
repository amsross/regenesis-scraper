module Option = Belt.Option;
module Affect = BsEffects.Affect;

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

  let login: (Got.instance, username, password) => Affect.affect(authenticated);
  let fetch: (authenticated, Got.instance, schoolyear, studentid, mp) => Affect.affect(list(t));
};

module Genesis: Genesis = {
  type username = string;
  type password = string;
  type authenticated = unit;
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
    Got.get(instance, "parents", ())
    |> Affect.flat_map(_, _ =>
         Got.post(instance, "j_security_check", {"j_username": username, "j_password": password})
       )
    |> Affect.map(_ => ());

  let makeSortKey = (schoolyear, mp, course, unixstamp) =>
    Js.String.replaceByRe(
      [%bs.re "/\\W/g"],
      "",
      schoolyear ++ string_of_int(mp) ++ course ++ Js.Float.toString(unixstamp),
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
    Js.Dict.get(oldGrades, course)->Option.map(o => o != grade)->Option.getWithDefault(true);

  let cleanGrades = (schoolyear, studentid, mp) =>
    List.fold_left(
      (grades, {Cheerio.course, grade}) =>
        switch (grade) {
        | Some(grade) =>
          List.append([makeGrade(schoolyear, mp, studentid, course, grade, unixstamp)], grades)
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

    Got.get(instance, "parents", ~params, ())
    |> Affect.map(data => {
         let entries =
           data##body->Cheerio.load->Cheerio.parse->Array.to_list |> List.map(Cheerio.entryFromJs);

         cleanGrades(schoolyear, studentid, mp, entries);
       });
  };
};

module type Database = {
  type db;
  let make: unit => db;

  let fetch:
    (db, option(schoolyear), option(studentid), option(mp)) =>
    Affect.affect(AWS.DynamoDB.query_data(Genesis.t));
  let write: (db, Genesis.t) => Affect.affect(Genesis.t);

  let fetchOldGrades: (db, schoolyear, studentid, mp) => Affect.affect(Js.Dict.t(float));
};

module Database: Database = {
  type db = AWS.DynamoDB.t;
  let make = AWS.DynamoDB.make;

  let put:
    (db, AWS.DynamoDB.put_params(Genesis.t)) =>
    Affect.affect(AWS.DynamoDB.put_params(Genesis.t)) =
    (db, item, error, success) => {
      AWS.DynamoDB.put(db, item, (err, _) =>
        Js.Nullable.isNullable(err) ? success(item) : error(Js.Nullable.toOption(err))
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
    Affect.affect(AWS.DynamoDB.query_data(Genesis.t)) =
    (db, params, error, success) =>
      AWS.DynamoDB.query(db, params, (err, result) =>
        Js.Nullable.isNullable(err) ? success(result) : error(Js.Nullable.toOption(err))
      );

  let fetch = (db, schoolyear, studentid, mp) => {
    let studentid = Option.getExn(studentid);
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

    params()
    |> Option.getWithDefault(filterExpression->Option.map(params(~filterExpression=_, ())))
    |> query(db);
  };

  let write = (db, item) => {
    let params = AWS.DynamoDB.put_params(~tableName, ~item, ());

    put(db, params) |> Affect.map(_ => item);
  };

  let fetchOldGrades = (db, schoolyear, studentid, mp) =>
    fetch(db, Some(schoolyear), Some(studentid), Some(mp))
    |> Affect.map(result => {
         let items = result |> AWS.DynamoDB.itemsGet;

         Array.fold_right(
           (Genesis.{course, grade}, dict) => {
             Js.Dict.set(dict, course, grade);
             dict;
           },
           items,
           Js.Dict.empty(),
         );
       });
};
