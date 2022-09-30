type db

module Grades: {
  let create: (db, ~grade: Genesis.t) => Future.t<Genesis.t>

  let read: (
    db,
    ~studentid: Genesis.studentid,
    ~schoolyear: Genesis.schoolyear=?,
    ~mp: Genesis.mp=?,
    unit,
  ) => Future.t<array<Genesis.t>>
}

let make: unit => db
