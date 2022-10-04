type db

module Grades: {
  type t = Genesis.t

  let create: (db, ~grade: t) => Future.t<t>

  let read: (
    db,
    ~studentid: Genesis.studentid,
    ~schoolyear: Genesis.schoolyear=?,
    ~mp: Genesis.mp=?,
    unit,
  ) => Future.t<array<t>>
}

module Filters: {
  type t
  type db_t

  let create: (db, ~filter: db_t) => Future.t<db_t>

  let read: (
    db,
    ~studentid: Genesis.studentid,
    ~schoolyear: Genesis.schoolyear=?,
    unit,
  ) => Future.t<array<t>>
}

let make: unit => db
