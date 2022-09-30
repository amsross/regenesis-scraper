type schoolyear = string
type studentid = int
type mp = int

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
