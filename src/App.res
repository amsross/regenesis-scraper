exception No_Base_URL
exception No_Username
exception No_Password
exception No_Student_ID

module Result = Belt.Result
module ResultFuture = {
  let sequence: 'data 'err. result<Future.t<'data>, 'err> => Future.t<
    result<'data, 'err>,
  > = result =>
    switch result {
    | Ok(future) => future->Future.map(data => Ok(data))
    | Error(err) => Future.pure(Error(err))
    }
}

module ListFuture = {
  let sequence = xs => List.fold_right((acc, x) => {
      x |> Future.flat_map(_, x => acc->Future.map(acc => List.append(list{acc}, x)))
    }, xs, Future.pure(list{}))
}

module ResultApply = Fantasy.Apply({
  type t<'data> = result<'data, exn>
  let map = Result.map
  let flatMap = Result.flatMap
})

let resultToFuture = result =>
  Future.pure(result)->Future.map(result =>
    switch result {
    | Ok(data) => data
    | Error(ex) => raise(ex)
    }
  )

let optionToResult = (opt, error) =>
  switch opt {
  | None => Error(error)
  | Some(opt) => Ok(opt)
  }

module Make = (
  Config: {
    let date: Js.Date.t
    let genesis_uname: Js.Nullable.t<Genesis.username>
    let genesis_pword: Js.Nullable.t<Genesis.password>
    let baseURL: Js.Nullable.t<string>
  },
) => {
  let year = Config.date |> Js.Date.getUTCFullYear |> int_of_float
  let schoolyear =
    Js.Date.getUTCMonth(Config.date) +. 1.0 > 7.0
      ? string_of_int(year) ++ ("-" ++ string_of_int(year + 1))
      : string_of_int(year - 1) ++ ("-" ++ string_of_int(year))
  let mps = list{1, 2, 3, 4}

  let db = try {
    Ok(Database.make())
  } catch {
  | err => Error(err)
  }

  let baseURL = optionToResult(Config.baseURL->Js.Nullable.toOption, No_Base_URL)
  let username = optionToResult(Config.genesis_uname->Js.Nullable.toOption, No_Username)
  let password = optionToResult(Config.genesis_pword->Js.Nullable.toOption, No_Password)

  let readFilters = (~studentid=?, ~schoolyear=?, ()) => {
    let studentid = optionToResult(studentid, No_Student_ID)

    Ok((db, studentid) => Database.Filters.read(db, ~studentid, ~schoolyear?, ()))
    ->ResultApply.ap(db)
    ->ResultApply.ap(studentid)
    ->Future.pure
    ->Future.flat_map(results =>
      switch results {
      | Ok(results) => results
      | Error(ex) => raise(ex)
      }
    )
  }

  let readGrades = (~studentid=?, ~schoolyear=?, ~mp=?, ()) => {
    let studentid = optionToResult(studentid, No_Student_ID)

    Ok((db, studentid) => Database.Grades.read(db, ~studentid, ~schoolyear?, ~mp?, ()))
    ->ResultApply.ap(db)
    ->ResultApply.ap(studentid)
    ->Future.pure
    ->Future.flat_map(results =>
      switch results {
      | Ok(results) => results
      | Error(ex) => raise(ex)
      }
    )
  }

  let got = Ok(
    baseURL =>
      Got.create(
        baseURL,
        {
          "Content-Type": "application/x-www-form-urlencoded",
          "User-Agent": "curl/7.54.0",
        },
      ),
  )->ResultApply.ap(baseURL)

  let authenticated =
    Ok((got, uname, pword) => Genesis.login(got, uname, pword))
    ->ResultApply.ap(got)
    ->ResultApply.ap(username)
    ->ResultApply.ap(password)

  let fetchFreshGrades = (studentid: option<Genesis.studentid>) => {
    authenticated
    ->ResultFuture.sequence
    ->Future.map(authenticated =>
      Ok((got, authenticated, studentid) => (got, authenticated, studentid))
      ->ResultApply.ap(got)
      ->ResultApply.ap(authenticated)
      ->ResultApply.ap(optionToResult(studentid, No_Student_ID))
      ->Result.map(((got, authenticated, studentid)) =>
        mps
        |> List.map(mp =>
          Genesis.fetch(authenticated, got, schoolyear, studentid, mp)->Future.map(newGrades => (
            mp,
            newGrades,
          ))
        )
        |> ListFuture.sequence
        |> Future.flat_map(_, results =>
          results
          |> List.filter(((_, grades)) => List.length(grades) > 0)
          |> List.map(((mp, newGrades)) =>
            Future.pure(items => Array.fold_right(({Genesis.course: course, grade}, dict) => {
                Js.Dict.set(dict, course, grade)
                dict
              }, items, Js.Dict.empty()))
            ->Future.apply(readGrades(~studentid, ~schoolyear, ~mp, ()))
            ->Future.map(oldGrades => List.filter(Genesis.gradeHasChanged(oldGrades)))
            ->Future.apply(Future.pure(newGrades))
          )
          |> ListFuture.sequence
        )
      )
    )
    ->Future.flat_map(results =>
      switch results {
      | Ok(results) => results
      | Error(ex) => raise(ex)
      }
    )
  }

  let writeUpdatedGrades: list<list<Genesis.t>> => Future.t<list<Genesis.t>> = grades => {
    let writeGrades = (grades, db) =>
      grades
      |> List.fold_left(List.append, list{})
      |> List.map(grade => Database.Grades.create(db, ~grade))

    Ok(db => grades->writeGrades(db)->ListFuture.sequence)
    ->ResultApply.ap(db)
    ->resultToFuture
    ->Future.flat_map(x => x)
  }
}
