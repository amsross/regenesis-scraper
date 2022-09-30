module Option = Belt.Option

type schoolyear = string
type studentid = int
type mp = int

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
  ->Future.flat_map(_ =>
    Got.post(instance, "j_security_check", {"j_username": username, "j_password": password})
  )
  ->Future.map(_ => ())

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

  Got.get(instance, "parents", ~params, ())->Future.map(data => {
    let entries =
      data["body"]->Cheerio.load->Cheerio.parse->Array.to_list |> List.map(Cheerio.entryFromJs)

    cleanGrades(schoolyear, studentid, mp, entries)
  })
}
